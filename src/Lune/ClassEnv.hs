module Lune.ClassEnv
  ( ClassEnv
  , ClassInfo (..)
  , ClassError (..)
  , buildClassEnv
  , classKindEnv
  , classMethodEnv
  ) where

import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Lune.Syntax as S
import qualified Lune.Kind as K
import Lune.Type
import Lune.TypeConvert (buildAliasEnv, convertConstraint, schemeFromQualType)

type ClassEnv = Map Text ClassInfo

data ClassInfo = ClassInfo
  { classInfoName :: Text
  , classInfoParams :: [Text]
  , classInfoParamKinds :: [K.Kind]
  , classInfoSupers :: [Constraint]
  , classInfoMethods :: Map Text Scheme
  }
  deriving (Eq, Show)

data ClassError
  = DuplicateClass Text
  | DuplicateClassParam Text
  | DuplicateMethod Text
  | ConflictingMethodScheme Text
  | UnknownClassInConstraint Text
  | InvalidKindAnnotation Text S.Type
  deriving (Show)

builtinClasses :: ClassEnv
builtinClasses =
  Map.fromList
    [ ("Functor", functorInfo)
    , ("Applicative", applicativeInfo)
    , ("Monad", monadInfo)
    ]
  where
    kf = K.KArr K.KType K.KType

    functorInfo =
      ClassInfo
        { classInfoName = "Functor"
        , classInfoParams = ["f"]
        , classInfoParamKinds = [kf]
        , classInfoSupers = []
        , classInfoMethods =
            Map.fromList
              [ ("fmap", functorFmapScheme)
              ]
        }

    functorFmapScheme =
      Forall
        ["f", "a", "b"]
        [Constraint "Functor" [TVar "f"]]
        ( TArrow
            (TArrow (TVar "a") (TVar "b"))
            (TArrow (TApp (TVar "f") (TVar "a")) (TApp (TVar "f") (TVar "b")))
        )

    applicativeInfo =
      ClassInfo
        { classInfoName = "Applicative"
        , classInfoParams = ["f"]
        , classInfoParamKinds = [kf]
        , classInfoSupers = [Constraint "Functor" [TVar "f"]]
        , classInfoMethods =
            Map.fromList
              [ ("pureA", applicativePureScheme)
              , ("ap", applicativeApScheme)
              ]
        }

    applicativePureScheme =
      Forall
        ["f", "a"]
        [Constraint "Applicative" [TVar "f"]]
        (TArrow (TVar "a") (TApp (TVar "f") (TVar "a")))

    applicativeApScheme =
      Forall
        ["f", "a", "b"]
        [Constraint "Applicative" [TVar "f"]]
        ( TArrow
            (TApp (TVar "f") (TArrow (TVar "a") (TVar "b")))
            (TArrow (TApp (TVar "f") (TVar "a")) (TApp (TVar "f") (TVar "b")))
        )

    monadInfo =
      ClassInfo
        { classInfoName = "Monad"
        , classInfoParams = ["m"]
        , classInfoParamKinds = [kf]
        , classInfoSupers = [Constraint "Applicative" [TVar "m"]]
        , classInfoMethods =
            Map.fromList
              [ ("pureM", monadPureScheme)
              , ("bindM", monadBindScheme)
              , ("thenM", monadThenScheme)
              ]
        }

    monadPureScheme =
      Forall
        ["m", "a"]
        [Constraint "Monad" [TVar "m"]]
        (TArrow (TVar "a") (TApp (TVar "m") (TVar "a")))

    monadBindScheme =
      Forall
        ["m", "a", "b"]
        [Constraint "Monad" [TVar "m"]]
        ( TArrow
            (TApp (TVar "m") (TVar "a"))
            (TArrow (TArrow (TVar "a") (TApp (TVar "m") (TVar "b"))) (TApp (TVar "m") (TVar "b")))
        )

    monadThenScheme =
      Forall
        ["m", "a", "b"]
        [Constraint "Monad" [TVar "m"]]
        (TArrow (TApp (TVar "m") (TVar "a")) (TArrow (TApp (TVar "m") (TVar "b")) (TApp (TVar "m") (TVar "b"))))

buildClassEnv :: S.Module -> Either ClassError ClassEnv
buildClassEnv m = do
  moduleClasses <- buildModuleClasses
  let env = moduleClasses <> builtinClasses
  checkSuperclassNames (classKindEnv env) moduleClasses
  pure env
  where
    decls = S.modDecls m
    aliasEnv = buildAliasEnv decls

    buildModuleClasses :: Either ClassError ClassEnv
    buildModuleClasses =
      foldlM addDecl Map.empty decls

    addDecl :: ClassEnv -> S.Decl -> Either ClassError ClassEnv
    addDecl env decl =
      case decl of
        S.DeclClass name params supers methods -> do
          if Map.member name env
            then Left (DuplicateClass name)
            else do
              let paramNames = map S.classParamName params
              checkDistinctParams paramNames
              paramKinds <- mapM (classParamKindToKind name) params
              let classConstraint = Constraint name (map TVar paramNames)
              let superConstraints = map (convertConstraint aliasEnv) supers
              methodMap <- buildMethodMap name classConstraint methods
              let info =
                    ClassInfo
                      { classInfoName = name
                      , classInfoParams = paramNames
                      , classInfoParamKinds = paramKinds
                      , classInfoSupers = superConstraints
                      , classInfoMethods = methodMap
                      }
              pure (Map.insert name info env)
        _ ->
          pure env

    classParamKindToKind :: Text -> S.ClassParam -> Either ClassError K.Kind
    classParamKindToKind _ (S.ClassParam _ Nothing) =
      pure K.KType
    classParamKindToKind className (S.ClassParam _ (Just k)) =
      kindFromType className k

    buildMethodMap :: Text -> Constraint -> [S.ClassMethodSig] -> Either ClassError (Map Text Scheme)
    buildMethodMap className classConstraint =
      foldlM (addMethod className classConstraint) Map.empty

    addMethod :: Text -> Constraint -> Map Text Scheme -> S.ClassMethodSig -> Either ClassError (Map Text Scheme)
    addMethod className classConstraint methods (S.ClassMethodSig name qualTy) =
      if Map.member name methods
        then Left (DuplicateMethod name)
        else do
          let (Forall _ cs ty) = schemeFromQualType aliasEnv qualTy
              cs' = classConstraint : cs
              vars' = Set.toList (ftvConstraints cs' <> ftvType ty)
              scheme = Forall vars' cs' ty
          pure (Map.insert name scheme methods)

    checkDistinctParams :: [Text] -> Either ClassError ()
    checkDistinctParams names =
      case duplicates names of
        [] -> Right ()
        (dup : _) -> Left (DuplicateClassParam dup)

    duplicates :: Ord a => [a] -> [a]
    duplicates =
      go Set.empty Set.empty
      where
        go _ dupes [] =
          Set.toList dupes
        go seen dupes (x : xs)
          | x `Set.member` seen =
              go seen (Set.insert x dupes) xs
          | otherwise =
              go (Set.insert x seen) dupes xs

checkSuperclassNames :: K.ClassEnv -> ClassEnv -> Either ClassError ()
checkSuperclassNames knownKinds moduleClasses =
  mapM_ checkClass (Map.elems moduleClasses)
  where
    checkClass info =
      mapM_ checkConstraint (classInfoSupers info)

    checkConstraint c =
      if Map.member (constraintClass c) knownKinds
        then Right ()
        else Left (UnknownClassInConstraint (constraintClass c))

kindFromType :: Text -> S.Type -> Either ClassError K.Kind
kindFromType className ty =
  case ty of
    S.TypeCon "Type" ->
      Right K.KType
    S.TypeArrow a b ->
      K.KArr <$> kindFromType className a <*> kindFromType className b
    _ ->
      Left (InvalidKindAnnotation className ty)

classKindEnv :: ClassEnv -> K.ClassEnv
classKindEnv =
  Map.map classInfoParamKinds

classMethodEnv :: ClassEnv -> Either ClassError TypeEnv
classMethodEnv env =
  foldlM add Map.empty (concatMap (Map.toList . classInfoMethods) (Map.elems env))
  where
    add acc (name, scheme) =
      case Map.lookup name acc of
        Nothing ->
          pure (Map.insert name scheme acc)
        Just existing ->
          if existing == scheme
            then pure acc
            else Left (ConflictingMethodScheme name)
