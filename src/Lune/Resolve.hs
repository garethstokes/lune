module Lune.Resolve
  ( ResolveError (..)
  , resolveProgram
  , qualifyName
  ) where

import Control.Applicative ((<|>))
import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Builtins as Builtins
import qualified Lune.ModuleGraph as MG
import qualified Lune.Syntax as S

data ResolveError
  = DuplicateImportAlias Text Text Text
  | DuplicateUnqualifiedImport Text Text Text
  | ImportExposesMissingValue Text Text Text
  | ImportExposesMissingType Text Text Text
  | ImportExposesMissingAllMembers Text Text Text
  | ModuleExportsMissingName Text Text
  | QualifiedAccessUnknownModule Text Text
  | QualifiedAccessNotExported Text Text Text
  | OverlappingBuiltinInstance Text Text Text
  | OrphanInstance Text Text Text
  | UnboundName Text Text
  deriving (Eq, Show)

qualifyName :: Text -> Text -> Text
qualifyName modName name =
  modName <> "." <> name

data ModuleExports = ModuleExports
  { exportValues :: Set Text
  , exportTypes :: Map Text S.ExposeMembers
  , exportClasses :: Map Text S.ExposeMembers
  , exportCtorsByType :: Map Text [Text]
  , exportClassMethodsByClass :: Map Text [Text]
  }
  deriving (Eq, Show)

resolveProgram :: MG.Program -> Either ResolveError S.Module
resolveProgram prog = do
  checkProgramCoherence prog
  exportTable <- buildExportTable (MG.progModules prog)
  decls <- concat <$> mapM (resolveOne exportTable) (MG.progOrder prog)
  pure
    S.Module
      { S.modName = MG.progEntryName prog
      , S.modExports = []
      , S.modImports = []
      , S.modDecls = decls
      }
  where
    resolveOne exportTable modName =
      case Map.lookup modName (MG.progModules prog) of
        Nothing ->
          Left (UnboundName modName modName)
        Just m ->
          resolveModule exportTable m

checkProgramCoherence :: MG.Program -> Either ResolveError ()
checkProgramCoherence prog =
  mapM_ checkModule (Map.elems (MG.progModules prog))
  where
    checkModule lm =
      let m = MG.lmModule lm
          modName = S.modName m
          decls = S.modDecls m
          instanceDecls =
            [ (cls, headTy)
            | S.DeclInstance cls headTy _ <- decls
            ]
          importedModules =
            Set.fromList (map S.impName (S.modImports m))
              <> if shouldImplicitPreludeImport lm then Set.singleton "Lune.Prelude" else Set.empty
       in mapM_ (checkInstance modName importedModules) instanceDecls

    classDefs =
      Map.fromListWith (<>)
        [ (clsName, Set.singleton (MG.lmName lm))
        | lm <- Map.elems (MG.progModules prog)
        , S.DeclClass clsName _ _ _ <- S.modDecls (MG.lmModule lm)
        ]

    typeDefs =
      Map.fromListWith (<>)
        [ (typeName, Set.singleton (MG.lmName lm))
        | lm <- Map.elems (MG.progModules prog)
        , decl <- S.modDecls (MG.lmModule lm)
        , typeName <-
            case decl of
              S.DeclType n _ _ -> [n]
              S.DeclTypeAlias _ n _ _ -> [n]
              S.DeclNewtype n _ _ _ -> [n]
              _ -> []
        ]

    checkInstance modName importedModules (cls, headTy) =
      case instanceHeadCon headTy of
        Nothing ->
          Right ()
        Just headCon -> do
          if Map.member (cls, headCon) Builtins.builtinInstanceDicts
            then Left (OverlappingBuiltinInstance modName cls headCon)
            else Right ()

          let classOk = moduleMentions importedModules modName (Map.findWithDefault Set.empty cls classDefs)
              typeOk = moduleMentions importedModules modName (Map.findWithDefault Set.empty headCon typeDefs)
          if classOk && typeOk
            then Right ()
            else Left (OrphanInstance modName cls headCon)

    instanceHeadCon ty =
      case unapplyTypeApps ty of
        (S.TypeCon name, _) -> Just name
        _ -> Nothing

    unapplyTypeApps =
      go []
      where
        go args t =
          case t of
            S.TypeApp f x ->
              go (x : args) f
            _ ->
              (t, args)

    moduleMentions importedModules modName definingModules =
      let available = Set.insert modName importedModules
       in not (Set.null (available `Set.intersection` definingModules))

buildExportTable :: Map Text MG.LoadedModule -> Either ResolveError (Map Text ModuleExports)
buildExportTable modules =
  foldlM step Map.empty (Map.elems modules)
  where
    step acc lm =
      do
        ex <- exportsForModule (MG.lmModule lm)
        pure (Map.insert (MG.lmName lm) ex acc)

exportsForModule :: S.Module -> Either ResolveError ModuleExports
exportsForModule m = do
  let definedValues =
        Set.fromList $
          [ name
          | S.DeclValue name _ _ <- S.modDecls m
          ]
          <>
          [ name
          | S.DeclForeignImport _ _ name _ <- S.modDecls m
          ]

      definedTypes =
        Set.fromList
          [ name
          | decl <- S.modDecls m
          , name <-
              case decl of
                S.DeclType n _ _ -> [n]
                S.DeclTypeAlias _ n _ _ -> [n]
                S.DeclNewtype n _ _ _ -> [n]
                _ -> []
          ]

      definedCtors =
        Map.fromListWith (<>)
          [ (typeName, [ctorName])
          | decl <- S.modDecls m
          , (typeName, ctorName) <- ctorsFromDecl decl
          ]

      definedClasses =
        Map.fromList
          [ (name, map S.classMethodName methods)
          | S.DeclClass name _ _ methods <- S.modDecls m
          ]

      exportedTypes =
        Map.fromList
          [ (name, members)
          | S.ExposeType name members <- S.modExports m
          , name `Set.member` definedTypes
          ]

      exportedClasses =
        Map.fromList
          [ (name, members)
          | S.ExposeType name members <- S.modExports m
          , name `Set.member` Map.keysSet definedClasses
          ]

      exportedCtors =
        Map.fromList
          [ (typeName, ctors)
          | S.ExposeType typeName S.ExposeAll <- S.modExports m
          , Just ctors <- [Map.lookup typeName definedCtors]
          ]

      exportedClassMethods =
        Map.fromList
          [ (cls, methods)
          | S.ExposeType cls S.ExposeAll <- S.modExports m
          , Just methods <- [Map.lookup cls definedClasses]
          ]

      exportedValueNames =
        foldMap (exposeValues definedValues exportedCtors) (S.modExports m)

  mapM_ (checkExpose definedValues definedTypes (Map.keysSet definedClasses)) (S.modExports m)

  pure
    ModuleExports
      { exportValues = exportedValueNames
      , exportTypes = exportedTypes
      , exportClasses = exportedClasses
      , exportCtorsByType = exportedCtors
      , exportClassMethodsByClass = exportedClassMethods
      }
  where
    exposeValues definedValues exportedCtors expose =
      case expose of
        S.ExposeValue name ->
          Set.singleton name
        S.ExposeType _ S.ExposeOpaque ->
          Set.empty
        S.ExposeType typeName S.ExposeAll ->
          Set.fromList (fromMaybe [] (Map.lookup typeName exportedCtors))

    checkExpose values types classes expose =
      case expose of
        S.ExposeValue name ->
          if name `Set.member` values
            then Right ()
            else Left (ModuleExportsMissingName (S.modName m) name)
        S.ExposeType name _ ->
          if name `Set.member` types || name `Set.member` classes
            then Right ()
            else Left (ModuleExportsMissingName (S.modName m) name)

ctorsFromDecl :: S.Decl -> [(Text, Text)]
ctorsFromDecl decl =
  case decl of
    S.DeclType typeName _ ctors ->
      [ (typeName, ctorName)
      | S.TypeCtor ctorName _ <- ctors
      ]
    S.DeclNewtype typeName _ ctorName _ ->
      [(typeName, ctorName)]
    _ ->
      []

resolveModule :: Map Text ModuleExports -> MG.LoadedModule -> Either ResolveError [S.Decl]
resolveModule exportTable lm = do
  let m = MG.lmModule lm
      modName = S.modName m
      builtinNames = Map.keysSet Builtins.builtinSchemes

  importAliases <- buildImportAliases modName (S.modImports m)

  unqualifiedImports0 <- buildUnqualifiedImports modName exportTable importAliases (S.modImports m)
  unqualifiedImports <-
    if shouldImplicitPreludeImport lm
      then addImplicitPreludeImports modName exportTable unqualifiedImports0
      else Right unqualifiedImports0

  let localValues = localValueMap modName (S.modDecls m)

      initialScope =
        Scope
          { scopeModule = modName
          , scopeBound = Set.empty
          , scopeLocalValues = localValues
          , scopeUnqualifiedImports = unqualifiedImports
          , scopeImportAliases = importAliases
          , scopeExports = exportTable
          , scopeBuiltinNames = builtinNames
          }

  mapM (resolveDecl initialScope) (S.modDecls m)

shouldImplicitPreludeImport :: MG.LoadedModule -> Bool
shouldImplicitPreludeImport lm =
  MG.lmName lm /= "Lune.Prelude" && not ("prelude/" `T.isPrefixOf` T.pack (MG.lmPath lm))

addImplicitPreludeImports :: Text -> Map Text ModuleExports -> Map Text Text -> Either ResolveError (Map Text Text)
addImplicitPreludeImports current exportTable acc =
  case Map.lookup "Lune.Prelude" exportTable of
    Nothing ->
      Right acc
    Just preludeExports -> do
      acc1 <-
        foldlM
          (\m name -> insertUnqualified current name (qualifyName "Lune.Prelude" name) m)
          acc
          (Set.toList (exportValues preludeExports))
      foldlM
        (\m name -> insertUnqualified current name name m)
        acc1
        (concat (Map.elems (exportClassMethodsByClass preludeExports)))

data Scope = Scope
  { scopeModule :: Text
  , scopeBound :: Set Text
  , scopeLocalValues :: Map Text Text
  , scopeUnqualifiedImports :: Map Text Text
  , scopeImportAliases :: Map Text Text
  , scopeExports :: Map Text ModuleExports
  , scopeBuiltinNames :: Set Text
  }

buildImportAliases :: Text -> [S.Import] -> Either ResolveError (Map Text Text)
buildImportAliases current imports =
  foldlM add Map.empty imports
  where
    add acc imp = do
      let target = S.impName imp
          alias = fromMaybe (defaultAlias target) (S.impAs imp)
      case Map.lookup alias acc of
        Nothing ->
          Right (Map.insert alias target acc)
        Just other
          | other == target ->
              Right acc
          | otherwise ->
              Left (DuplicateImportAlias current other target)

defaultAlias :: Text -> Text
defaultAlias name =
  last (T.splitOn "." name)

buildUnqualifiedImports ::
  Text ->
  Map Text ModuleExports ->
  Map Text Text ->
  [S.Import] ->
  Either ResolveError (Map Text Text)
buildUnqualifiedImports current exportTable _aliasTable =
  foldlM step Map.empty
  where
    step acc imp =
      case S.impExposing imp of
        Nothing ->
          Right acc
        Just exposing -> do
          moduleExports <-
            case Map.lookup (S.impName imp) exportTable of
              Nothing ->
                Left (QualifiedAccessUnknownModule current (S.impName imp))
              Just ex ->
                Right ex

          imported <- foldlM (addExpose (S.impName imp) moduleExports) acc exposing
          Right imported

    addExpose importedModule moduleExports acc expose =
      case expose of
        S.ExposeValue name ->
          if name `Set.member` exportValues moduleExports
            then insertUnqualified current name (qualifyName importedModule name) acc
            else Left (ImportExposesMissingValue current importedModule name)
        S.ExposeType typeName members ->
          case lookupExportedTypeOrClass typeName moduleExports of
            Nothing ->
              Left (ImportExposesMissingType current importedModule typeName)
            Just exportedMembers ->
              case members of
                S.ExposeOpaque ->
                  Right acc
                S.ExposeAll ->
                  if exportedMembers /= S.ExposeAll
                    then Left (ImportExposesMissingAllMembers current importedModule typeName)
                    else do
                      let ctors = fromMaybe [] (Map.lookup typeName (exportCtorsByType moduleExports))
                          methods = fromMaybe [] (Map.lookup typeName (exportClassMethodsByClass moduleExports))
                      acc1 <- foldlM (\m name -> insertUnqualified current name (qualifyName importedModule name) m) acc ctors
                      foldlM (\m name -> insertUnqualified current name name m) acc1 methods

    lookupExportedTypeOrClass name moduleExports =
      Map.lookup name (exportTypes moduleExports) <|> Map.lookup name (exportClasses moduleExports)

insertUnqualified :: Text -> Text -> Text -> Map Text Text -> Either ResolveError (Map Text Text)
insertUnqualified current name resolved acc =
  case Map.lookup name acc of
    Nothing ->
      Right (Map.insert name resolved acc)
    Just existing
      | existing == resolved ->
          Right acc
      | otherwise ->
          Left (DuplicateUnqualifiedImport current existing resolved)

localValueMap :: Text -> [S.Decl] -> Map Text Text
localValueMap modName decls =
  valuesMap <> ctorsMap <> methodsMap
  where
    valuesMap =
      Map.fromList $
        [ (name, qualifyName modName name)
        | S.DeclValue name _ _ <- decls
        ]
        <>
        [ (name, qualifyName modName name)
        | S.DeclForeignImport _ _ name _ <- decls
        ]

    ctorsMap =
      Map.fromList
        [ (ctorName, qualifyName modName ctorName)
        | decl <- decls
        , (_, ctorName) <- ctorsFromDecl decl
        ]

    methodsMap =
      Map.fromList
        [ (methodName, methodName)
        | S.DeclClass _ _ _ methods <- decls
        , methodName <- map S.classMethodName methods
        ]

resolveDecl :: Scope -> S.Decl -> Either ResolveError S.Decl
resolveDecl scope decl =
  case decl of
    S.DeclTypeSig name qualTy -> do
      qualTy' <- resolveQualType scope qualTy
      Right (S.DeclTypeSig (qualifyName (scopeModule scope) name) qualTy')
    S.DeclValue name args expr -> do
      args' <- mapM (resolvePatternHead scope) args
      let bound' = scopeBound scope <> Set.fromList (patVars args)
      expr' <- resolveExpr scope {scopeBound = bound'} expr
      Right (S.DeclValue (qualifyName (scopeModule scope) name) args' expr')
    S.DeclType typeName vars ctors ->
      Right (S.DeclType typeName vars [qualCtor c | c <- ctors])
    S.DeclTypeAlias {} ->
      Right decl
    S.DeclNewtype typeName vars ctorName ctorType ->
      Right (S.DeclNewtype typeName vars (qualifyName (scopeModule scope) ctorName) ctorType)
    S.DeclClass {} ->
      Right decl
    S.DeclInstance cls headTy methods -> do
      methods' <-
        mapM
          ( \(S.InstanceMethodDef methodName methodExpr) -> do
              e <- resolveExpr scope methodExpr
              Right (S.InstanceMethodDef methodName e)
          )
          methods
      Right (S.DeclInstance cls headTy methods')
    S.DeclForeignImport convention symbol name qualTy -> do
      qualTy' <- resolveQualType scope qualTy
      Right (S.DeclForeignImport convention symbol (qualifyName (scopeModule scope) name) qualTy')
  where
    qualCtor (S.TypeCtor name tys) =
      S.TypeCtor (qualifyName (scopeModule scope) name) tys

-- | Resolve qualified type names in a QualType.
resolveQualType :: Scope -> S.QualType -> Either ResolveError S.QualType
resolveQualType scope (S.QualType constraints ty) = do
  ty' <- resolveTypeName scope ty
  constraints' <- mapM (resolveConstraint scope) constraints
  Right (S.QualType constraints' ty')

resolveConstraint :: Scope -> S.Constraint -> Either ResolveError S.Constraint
resolveConstraint scope (S.Constraint cls args) = do
  args' <- mapM (resolveTypeName scope) args
  Right (S.Constraint cls args')

-- | Walk a type and resolve qualified type constructor names (e.g. "Api.Api" -> "Api").
resolveTypeName :: Scope -> S.Type -> Either ResolveError S.Type
resolveTypeName scope ty =
  case ty of
    S.TypeCon name ->
      case T.breakOnEnd "." name of
        ("", _) ->
          -- No dot, leave as-is
          Right ty
        (prefix, typeName) ->
          let alias = T.dropEnd 1 prefix  -- remove trailing dot
           in case Map.lookup alias (scopeImportAliases scope) of
                Just targetModule ->
                  case Map.lookup targetModule (scopeExports scope) of
                    Just exports
                      | typeName `Map.member` exportTypes exports ->
                          Right (S.TypeCon typeName)
                    _ ->
                      -- Not an exported type, leave as-is (could be a qualified builtin)
                      Right (S.TypeCon typeName)
                Nothing ->
                  -- Not a known import alias, leave as-is
                  Right ty
    S.TypeVar _ ->
      Right ty
    S.TypeApp f x -> do
      f' <- resolveTypeName scope f
      x' <- resolveTypeName scope x
      Right (S.TypeApp f' x')
    S.TypeArrow a b -> do
      a' <- resolveTypeName scope a
      b' <- resolveTypeName scope b
      Right (S.TypeArrow a' b')
    S.TypeRecord fields -> do
      fields' <- mapM (\(n, t, anns) -> do t' <- resolveTypeName scope t; Right (n, t', anns)) fields
      Right (S.TypeRecord fields')

resolveExpr :: Scope -> S.Expr -> Either ResolveError S.Expr
resolveExpr scope expr =
  case expr of
    S.Var name ->
      resolveVar scope name
    S.StringLit _ ->
      Right expr
    S.TemplateLit flavor parts -> do
      parts' <- mapM (resolveTemplatePart scope) parts
      Right (S.TemplateLit flavor parts')
    S.IntLit _ ->
      Right expr
    S.FloatLit _ ->
      Right expr
    S.CharLit _ ->
      Right expr
    S.App f x ->
      S.App <$> resolveExpr scope f <*> resolveExpr scope x
    S.Lam pats body -> do
      pats' <- mapM (resolvePatternHead scope) pats
      let bound' = scopeBound scope <> Set.fromList (patVars pats)
      body' <- resolveExpr scope {scopeBound = bound'} body
      Right (S.Lam pats' body')
    S.LetIn name bound body -> do
      bound' <- resolveExpr scope bound
      let bodyScope = scope {scopeBound = Set.insert name (scopeBound scope)}
      body' <- resolveExpr bodyScope body
      Right (S.LetIn name bound' body')
    S.Case scrut alts -> do
      scrut' <- resolveExpr scope scrut
      alts' <- mapM (resolveAlt scope) alts
      Right (S.Case scrut' alts')
    S.DoBlock stmts -> do
      stmts' <- resolveDo scope stmts
      Right (S.DoBlock stmts')
    S.RecordLiteral fields ->
      S.RecordLiteral <$> mapM (\(n, e) -> do e' <- resolveExpr scope e; pure (n, e')) fields
    S.RecordUpdate base fields ->
      S.RecordUpdate <$> resolveExpr scope base <*> mapM (\(n, e) -> do e' <- resolveExpr scope e; pure (n, e')) fields
    S.FieldAccess base field ->
      resolveFieldAccess scope base field

resolveTemplatePart :: Scope -> S.TemplatePart -> Either ResolveError S.TemplatePart
resolveTemplatePart scope part =
  case part of
    S.TemplateText _ ->
      Right part
    S.TemplateHole e ->
      S.TemplateHole <$> resolveExpr scope e

resolveVar :: Scope -> Text -> Either ResolveError S.Expr
resolveVar scope name
  | name `Set.member` scopeBound scope =
      Right (S.Var name)
  | Just resolved <- Map.lookup name (scopeLocalValues scope) =
      Right (S.Var resolved)
  | Just resolved <- Map.lookup name (scopeUnqualifiedImports scope) =
      Right (S.Var resolved)
  | name `Set.member` scopeBuiltinNames scope =
      Right (S.Var name)
  | otherwise =
      Left (UnboundName (scopeModule scope) name)

resolveFieldAccess :: Scope -> S.Expr -> Text -> Either ResolveError S.Expr
resolveFieldAccess scope base field =
  case base of
    S.Var q
      | q `Set.notMember` scopeBound scope
      , Map.notMember q (scopeLocalValues scope) ->
          case Map.lookup q (scopeImportAliases scope) of
            Nothing -> do
              base' <- resolveExpr scope base
              Right (S.FieldAccess base' field)
            Just targetModule -> do
              ensureExported targetModule field
              Right (S.Var (qualifyName targetModule field))
    _ -> do
      base' <- resolveExpr scope base
      Right (S.FieldAccess base' field)
  where
    ensureExported targetModule name =
      case Map.lookup targetModule (scopeExports scope) of
        Nothing ->
          Left (QualifiedAccessUnknownModule (scopeModule scope) targetModule)
        Just exports ->
          if name `Set.member` exportValues exports
            then Right ()
            else Left (QualifiedAccessNotExported (scopeModule scope) targetModule name)

resolveAlt :: Scope -> S.Alt -> Either ResolveError S.Alt
resolveAlt scope (S.Alt pat body) = do
  pat' <- resolvePatternHead scope pat
  let bound' = scopeBound scope <> Set.fromList (patVars [pat])
  body' <- resolveExpr scope {scopeBound = bound'} body
  Right (S.Alt pat' body')

resolvePatternHead :: Scope -> S.Pattern -> Either ResolveError S.Pattern
resolvePatternHead scope pat =
  case pat of
    S.PVar _ ->
      Right pat
    S.PWildcard ->
      Right pat
    S.PCon name ps -> do
      name' <- resolveCtorName scope name
      ps' <- mapM (resolvePatternHead scope) ps
      Right (S.PCon name' ps')

resolveCtorName :: Scope -> Text -> Either ResolveError Text
resolveCtorName scope name
  | Just resolved <- Map.lookup name (scopeLocalValues scope) =
      Right resolved
  | Just resolved <- Map.lookup name (scopeUnqualifiedImports scope) =
      Right resolved
  | name `Set.member` scopeBuiltinNames scope =
      Right name
  | otherwise =
      Left (UnboundName (scopeModule scope) name)

patVars :: [S.Pattern] -> [Text]
patVars =
  concatMap go
  where
    go pat =
      case pat of
        S.PVar name -> [name]
        S.PWildcard -> []
        S.PCon _ ps -> concatMap go ps

resolveDo :: Scope -> [S.Stmt] -> Either ResolveError [S.Stmt]
resolveDo scope stmts =
  go scope [] stmts
  where
    go _ acc [] =
      Right (reverse acc)
    go scope0 acc (stmt : rest) =
      case stmt of
        S.BindStmt pat rhs -> do
          pat' <- resolvePatternHead scope0 pat
          rhs' <- resolveExpr scope0 rhs
          let bound' = scopeBound scope0 <> Set.fromList (patVars [pat])
          go scope0 {scopeBound = bound'} (S.BindStmt pat' rhs' : acc) rest
        S.DiscardBindStmt rhs -> do
          rhs' <- resolveExpr scope0 rhs
          go scope0 (S.DiscardBindStmt rhs' : acc) rest
        S.LetStmt name rhs -> do
          rhs' <- resolveExpr scope0 rhs
          let bound' = Set.insert name (scopeBound scope0)
          go scope0 {scopeBound = bound'} (S.LetStmt name rhs' : acc) rest
        S.ExprStmt rhs -> do
          rhs' <- resolveExpr scope0 rhs
          go scope0 (S.ExprStmt rhs' : acc) rest
