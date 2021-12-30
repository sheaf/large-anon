{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- TODO: Remove
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Record.Anonymous.Plugin.Constraints (
    -- * Wanted constraints recognized by the plugin
    CHasField(..)
  , CConstraintsRecord(..)
  , Fields(..)
  , Field(..)
  , findField
  , allFieldsKnown
    -- * Parsing
    -- ** Infrastructure
  , ParseResult(..)
  , parseAll
  , parseAll'
  , withOrig
    -- ** Specific parsers
  , parseHasField
  , parseConstraintsRecord
    -- * Evidence
  , evidenceHasField
  , evidenceConstraintsRecord
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Foldable (asum)
import Data.Map (Map)
import Data.Void

import qualified Data.Map as Map

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution

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

      -- | Raw arguments to @HasField@ (for evidence construction)
    , hasFieldTypeRaw :: [Type]

      -- | The type of the record (the @r@ in @Record r@)
    , hasFieldTypeRecord :: Type

      -- | Type of the record field we're looking for
    , hasFieldTypeField :: Type
    }

data CConstraintsRecord = CConstraintsRecord {
      -- | Fields of the record
      constraintsRecordFields :: Fields

      -- | Raw arguments to @ConstraintsRecord@ (for evidence construction)
    , constraintsRecordTypeRaw :: [Type]

      -- | The type of the record (the @r@ in @Record r@)
    , constraintsRecordTypeRecord :: Type

      -- | The constraint that we need for every field
    , constraintsRecordTypeConstraint :: Type
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

-- | Return map from field name to type, /if/ all fields are statically known
--
-- TODO: For our current 'Fields' definition, this will /always/ be the case,
-- but if we extend the parser to deal with field name variables or list
-- variables, this will no longer be the case.
allFieldsKnown :: Fields -> Maybe (Map FastString Type)
allFieldsKnown = go Map.empty
  where
    go :: Map FastString Type -> Fields -> Maybe (Map FastString Type)
    go acc = \case
        FieldsNil ->
          Just acc
        FieldsCons f fs ->
          case f of
            FieldKnown nm typ ->
              go (Map.insert nm typ acc) fs

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CHasField where
  ppr (CHasField label record typeRaw typeRecord typeField) = parens $
          text "CHasField"
      <+> ppr label
      <+> ppr record
      <+> ppr typeRaw
      <+> ppr typeRecord
      <+> ppr typeField

instance Outputable CConstraintsRecord where
  ppr (CConstraintsRecord fields typeRaw typeRecord typeConstraint) = parens $
          text "CConstraintsRecord"
      <+> ppr fields
      <+> ppr typeRaw
      <+> ppr typeRecord
      <+> ppr typeConstraint

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
parseHasField rn@ResolvedNames{..} =
    parseConstraint clsHasField $ \case
      args@[k, x, r, a] -> do
        -- Check the kind
        tcSymbol <- tyConAppTyCon_maybe k
        guard $ tcSymbol == typeSymbolKindCon

        -- We insist the name we're looking for is statically known
        x' <- isStrLitTy x

        -- Check that it's of the form @Record r@
        tyFields <- parseRecord rn r
        fields   <- parseFields tyFields

        return $ CHasField {
            hasFieldLabel      = x'
          , hasFieldRecord     = fields
          , hasFieldTypeRaw    = args
          , hasFieldTypeRecord = tyFields
          , hasFieldTypeField  = a
          }
      _invalidNumArgs ->
        Nothing

parseConstraintsRecord ::
     ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CConstraintsRecord)
parseConstraintsRecord ResolvedNames{..} =
    parseConstraint clsConstraintsRecord $ \case
      args@[r, c] -> do
        fields <- parseFields r
        return CConstraintsRecord {
            constraintsRecordFields         = fields
          , constraintsRecordTypeRaw        = args
          , constraintsRecordTypeRecord     = r
          , constraintsRecordTypeConstraint = c
          }
      _invalidNumArgs ->
        Nothing

{-------------------------------------------------------------------------------
  Supporting parsers
-------------------------------------------------------------------------------}

-- | Parse @Record r@
--
-- Returns the  argument @r@
parseRecord :: ResolvedNames -> Type -> Maybe Type
parseRecord ResolvedNames{..} r = do
    (tyRecord, tyFields) <- splitAppTy_maybe r
    tcRecord <- tyConAppTyCon_maybe tyRecord
    guard $ tcRecord == tyConRecord
    return tyFields

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

{-------------------------------------------------------------------------------
  Generic (not large-anon specific) parsing utility
-------------------------------------------------------------------------------}

-- | Generic constraint parser
--
-- TODO: If we add some parsing infra to ghc-tcplugin-api, maybe a (form of)
-- this function could live there too.
parseConstraint ::
     Class                -- ^ Class we're matching against
  -> ([Type] -> Maybe a)  -- ^ Parser for the class arguments
  -> Ct                   -- ^ Constraint to parse
  -> ParseResult e (GenLocated CtLoc a)
parseConstraint cls f ct = fmap (L $ ctLoc ct) $
    case classifyPredType (ctPred ct) of
      ClassPred cls' args | cls == cls' ->
        case f args of
          Just parsed ->
            ParseOk parsed
          Nothing ->
            panic $ concat [
                "Unexpected "
              , showSDocUnsafe (ppr cls)
              , " constraint with arguments:\n"
              , unlines (map (showSDocUnsafe . ppr) args)
              ]
      _otherwise ->
        ParseNoMatch

-- | Parse @x ': xs == (':) x xs == ((':) x) xs@
parseCons :: Type -> Maybe (Type, Type)
parseCons t = do
    ( t'  , xs ) <- splitAppTy_maybe t
    ( t'' , x  ) <- splitAppTy_maybe t'
    tcCons <- tyConAppTyCon_maybe t''
    guard $ tcCons == promotedConsDataCon
    return (x, xs)

-- | Parse @'[]@
parseNil :: Type -> Maybe ()
parseNil t = do
    tcNil <- tyConAppTyCon_maybe t
    guard $ tcNil == promotedNilDataCon
    return ()

-- | Parse @'(x, y) == '(,) x y == ('(,) x) y@
parsePair :: Type -> Maybe (Type, Type)
parsePair t = do
    ( t'  , y ) <- splitAppTy_maybe t
    ( t'' , x ) <- splitAppTy_maybe t'
    tcPair <- tyConAppTyCon_maybe t''
    guard $ tcPair == promotedTupleDataCon Boxed 2
    return (x, y)

{-------------------------------------------------------------------------------
  Evidence

  During development may want to compile with -dcore-lint.
-------------------------------------------------------------------------------}

evidenceHasField ::
     ResolvedNames
  -> CHasField
  -> TcPluginM 'Solve EvTerm
evidenceHasField ResolvedNames{..} CHasField{..} = do
    str <- mkStringExprFS hasFieldLabel
    return $
      evDataConApp
        (classDataCon clsHasField)
        hasFieldTypeRaw
        [ mkCoreApps (Var idUnsafeRecordHasField) [
              Type hasFieldTypeRecord
            , Type hasFieldTypeField
            , str
            ]
        ]

-- | Construct evidence for 'ConstraintsClass'
--
-- The evidence for the fields must be specified in the right order
-- (see 'Generic' instance for 'Record').
evidenceConstraintsRecord ::
     ResolvedNames
  -> [(EvVar, Type)]  -- Evidence for and type of each field of the record
  -> CConstraintsRecord
  -> TcPluginM 'Solve EvTerm
evidenceConstraintsRecord ResolvedNames{..} cs CConstraintsRecord{..} = do
    return $
      evDataConApp
        (classDataCon clsConstraintsRecord)
        constraintsRecordTypeRaw
        [ mkCoreApps (Var idUnsafeDictRecord) [
              Type constraintsRecordTypeRecord
            , Type constraintsRecordTypeConstraint
            , mkListExpr dictType (map (uncurry mkDictAny) cs)
            ]
        ]
  where
    dictType :: Type
    dictType = mkTyConApp tyConDict [
          liftedTypeKind
        , constraintsRecordTypeConstraint
        , anyType
        ]

    mkDictAny :: EvVar -> Type -> EvExpr
    mkDictAny dict fieldType = mkCoreConApps dataConDict [
          Type liftedTypeKind
        , Type constraintsRecordTypeConstraint
        , Type anyType
        , mkCoreApps (Var idUnsafeCoerce) [
              Type $ mkAppTy constraintsRecordTypeConstraint fieldType
            , Type $ mkAppTy constraintsRecordTypeConstraint anyType
            , Var dict
            ]
        ]

    -- Any at kind Type
    anyType :: Type
    anyType = mkTyConApp anyTyCon [liftedTypeKind]

