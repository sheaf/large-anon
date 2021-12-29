{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- TODO: Remove
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Record.Anonymous.Plugin.Constraints (
    -- * Wanted constraints recognized by the plugin
    CHasField(..)
  , Fields(..)
  , Field(..)
  , findField
    -- * Parsing
    -- ** Infrastructure
  , ParseResult(..)
  , parseAll
  , parseAll'
  , withOrig
    -- ** Specific parsers
  , parseHasField
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Void

import GHC.TcPlugin.API
import GHC.Utils.Outputable

-- TODO: We'll need to shim this
-- (And once we do, make sure we just import that module, like in typelet)
import Type (tyConAppTyCon_maybe, splitAppTy_maybe, isStrLitTy)
import BasicTypes (Boxity(Boxed))

import Data.Record.Anonymous.Plugin.NameResolution
import Data.Foldable (asum)

{-------------------------------------------------------------------------------
  Wanted constraints recognized by the plugin
-------------------------------------------------------------------------------}

data CHasField = CHasField {
      -- | Label we're looking for
      --
      -- This is always a monomorphic, statically known string; if we don't
      -- know what label we're looking for, we'll definitely not be able
      -- to resolve the constraint.
      hasFieldLabel :: FastString

      -- | Fields of the record
      --
      -- These may be fully or partially known, or completely unknown.
    , hasFieldRecord :: Fields

      -- | Type of the record field we're looking for
    , hasFieldType :: Type
    }

-- TODO: Will need extension for the polymorphic case
data Fields =
    FieldsCons Field Fields
  | FieldsNil

-- TODO: Will need extension for the polymorphic case
data Field =
    -- | Name and type of a known (non-polymorphic) field
    FieldKnown FastString Type

findField :: FastString -> Fields -> Maybe Type
findField nm = go
  where
    go :: Fields -> Maybe Type
    go (FieldsCons (FieldKnown nm' typ) fs)
      | nm == nm' = Just typ
      | otherwise = go fs
    go FieldsNil = Nothing

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CHasField where
  ppr CHasField{..} = parens $
          text "CHasField"
      <+> ppr hasFieldLabel
      <+> ppr hasFieldRecord
      <+> ppr hasFieldType

instance Outputable Fields where
  ppr (FieldsCons f fs) = parens $
          text "FieldsCons"
      <+> ppr f
      <+> ppr fs
  ppr FieldsNil = text "FieldsNil"

instance Outputable Field where
  ppr (FieldKnown nm typ) = parens $
          text "FieldKnown"
      <+> ppr nm
      <+> ppr typ

-- Orphan instance, for debugging
instance Outputable CtLoc where
  ppr _ = text "<CtLoc>"

{-------------------------------------------------------------------------------
  Parsing infrastructure

  TODO: This is copied straight from typelet. I wonder if it would make sense
  somewhere more general; part of ghc-tcplugin-api, perhaps?
-------------------------------------------------------------------------------}

data ParseResult e a =
    -- | Parse successful
    ParseOk a

    -- | Different constraint than we're looking for (does not imply an error)
  | ParseNoMatch

    -- | Constraint of the shape we're looking for, but something is wrong
  | ParseError e
  deriving (Functor)

instance Bifunctor ParseResult where
  bimap _ g (ParseOk a)    = ParseOk (g a)
  bimap _ _ ParseNoMatch   = ParseNoMatch
  bimap f _ (ParseError e) = ParseError (f e)

-- | Apply parser to each value in turn, bailing at the first error
parseAll :: forall e a b. (a -> ParseResult e b) -> [a] -> Either e [b]
parseAll f = go []
  where
    go :: [b] -> [a] -> Either e [b]
    go acc []     = Right (reverse acc)
    go acc (a:as) = case f a of
                      ParseOk b    -> go (b:acc) as
                      ParseNoMatch -> go    acc  as
                      ParseError e -> Left e

-- | Variation on 'parseAll' which rules out the error case
parseAll' :: (a -> ParseResult Void b) -> [a] -> [b]
parseAll' f = aux . parseAll f
  where
    aux :: Either Void [b] -> [b]
    aux (Left  v)  = absurd v
    aux (Right bs) = bs

-- | Bundle the parse result with the original value
withOrig :: (a -> ParseResult e b) -> (a -> ParseResult e (a, b))
withOrig f x = fmap (x, ) $ f x

{-------------------------------------------------------------------------------
  Parser for specific constraints
-------------------------------------------------------------------------------}

parseHasField ::
     ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CHasField)
parseHasField ResolvedNames{..} ct = fmap (L $ ctLoc ct) $
    case classifyPredType (ctPred ct) of
      ClassPred cls args | cls == clsHasField ->
        case parseHasFieldArgs args of
          Just parsed ->
            ParseOk parsed
          Nothing ->
            panic $ "Unexpected HasField constraint with arguments:\n"
                ++ unlines (map (showSDocUnsafe . ppr) args)
      _otherwise ->
        ParseNoMatch
  where
    parseHasFieldArgs :: [Type] -> Maybe CHasField
    parseHasFieldArgs [k, x, r, a] = do
        -- Check the kind
        tcSymbol <- tyConAppTyCon_maybe k
        guard $ tcSymbol == typeSymbolKindCon

        -- We insist the name we're looking for is statically known
        x' <- isStrLitTy x

        -- Check that it's of the form @Record r@
        (tyRecord, tyFields) <- splitAppTy_maybe r
        tcRecord <- tyConAppTyCon_maybe tyRecord
        guard $ tcRecord == tyConRecord

        -- Parse the individual fields
        fields <- parseFields tyFields
        return $ CHasField x' fields a
    parseHasFieldArgs _invalidNumArgs =
        Nothing

    parseFields :: Type -> Maybe Fields
    parseFields fields = asum [
          do (f, fs) <- parseCons fields
             f' <- parseField f
             (FieldsCons f') <$> parseFields fs
        , do parseNil fields
             return FieldsNil
        ]

    parseField :: Type -> Maybe Field
    parseField field = asum [
          do (nm, typ) <- parsePair field
             nm' <- isStrLitTy nm
             return $ FieldKnown nm' typ
        ]

    -- Parse @x ': xs == (':) x xs == ((':) x) xs@
    parseCons :: Type -> Maybe (Type, Type)
    parseCons t = do
        ( t'  , xs ) <- splitAppTy_maybe t
        ( t'' , x  ) <- splitAppTy_maybe t'
        tcCons <- tyConAppTyCon_maybe t''
        guard $ tcCons == promotedConsDataCon
        return (x, xs)

    -- Parse '[]
    parseNil :: Type -> Maybe ()
    parseNil t = do
        tcNil <- tyConAppTyCon_maybe t
        guard $ tcNil == promotedNilDataCon
        return ()

    -- Parse @'(x, y) == '(,) x y == ('(,) x) y@
    parsePair :: Type -> Maybe (Type, Type)
    parsePair t = do
        ( t'  , y ) <- splitAppTy_maybe t
        ( t'' , x ) <- splitAppTy_maybe t'
        tcPair <- tyConAppTyCon_maybe t''
        guard $ tcPair == promotedTupleDataCon Boxed 2
        return (x, y)

