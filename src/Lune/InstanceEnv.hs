module Lune.InstanceEnv
  ( InstanceEnv
  , InstanceInfo (..)
  , InstanceError (..)
  , buildInstanceEnv
  ) where

import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.ClassEnv as C
import qualified Lune.Syntax as S

type InstanceKey = (Text, Text)

type InstanceEnv = Map InstanceKey InstanceInfo

data InstanceInfo = InstanceInfo
  { instanceInfoClass :: Text
  , instanceInfoHead :: S.Type
  , instanceInfoHeadCon :: Text
  , instanceInfoHeadVars :: [Text]
  , instanceInfoMethods :: Map Text (S.Located S.Expr)
  }
  deriving (Show)

data InstanceError
  = UnknownClass Text
  | InvalidInstanceHead Text S.Type
  | OverlappingInstance Text Text
  | OrphanInstance Text Text
  | MissingInstanceMethods Text [Text]
  | ExtraInstanceMethods Text [Text]
  deriving (Show)

buildInstanceEnv :: S.Module -> C.ClassEnv -> Either InstanceError InstanceEnv
buildInstanceEnv m classEnv =
  foldlM addInstance Map.empty instances
  where
    decls = S.modDecls m

    definedClasses :: Set Text
    definedClasses =
      Set.fromList
        [ name
        | S.DeclClass name _ _ _ <- decls
        ]

    definedTypeCons :: Set Text
    definedTypeCons =
      Set.fromList
        [ name
        | decl <- decls
        , name <-
            case decl of
              S.DeclType n _ _ -> [n]
              S.DeclTypeAnn _ n _ _ -> [n]
              S.DeclTypeAlias _ n _ _ -> [n]
              S.DeclNewtype n _ _ _ -> [n]
              _ -> []
        ]

    instances :: [(Text, S.Type, [S.InstanceMethodDef])]
    instances =
      [ (cls, headTy, methods)
      | S.DeclInstance cls headTy methods <- decls
      ]

    addInstance :: InstanceEnv -> (Text, S.Type, [S.InstanceMethodDef]) -> Either InstanceError InstanceEnv
    addInstance env (cls, headTy, methodDefs) = do
      classInfo <-
        case Map.lookup cls classEnv of
          Nothing -> Left (UnknownClass cls)
          Just info -> Right info

      (headCon, headVars) <- viewInstanceHead cls headTy

      let key = (cls, headCon)
      case Map.lookup key env of
        Just _ ->
          Left (OverlappingInstance cls headCon)
        Nothing -> do
          if cls `Set.member` definedClasses || headCon `Set.member` definedTypeCons
            then pure ()
            else Left (OrphanInstance cls headCon)

          let providedNames = map S.instanceMethodName methodDefs
              requiredNames = Map.keys (C.classInfoMethods classInfo)
              missing = Set.fromList requiredNames `Set.difference` Set.fromList providedNames
              extra = Set.fromList providedNames `Set.difference` Set.fromList requiredNames

          if not (Set.null missing)
            then Left (MissingInstanceMethods (renderInstanceHead cls headTy) (Set.toList missing))
            else pure ()

          if not (Set.null extra)
            then Left (ExtraInstanceMethods (renderInstanceHead cls headTy) (Set.toList extra))
            else pure ()

          let methodMap =
                Map.fromList
                  [ (S.instanceMethodName def, S.instanceMethodExpr def)
                  | def <- methodDefs
                  ]

          let info =
                InstanceInfo
                  { instanceInfoClass = cls
                  , instanceInfoHead = headTy
                  , instanceInfoHeadCon = headCon
                  , instanceInfoHeadVars = headVars
                  , instanceInfoMethods = methodMap
                  }

          pure (Map.insert key info env)

viewInstanceHead :: Text -> S.Type -> Either InstanceError (Text, [Text])
viewInstanceHead cls ty = do
  let (headTy, args) = unapplyTypeApps ty
  headCon <-
    case headTy of
      S.TypeCon name ->
        Right name
      _ ->
        Left (InvalidInstanceHead cls ty)

  vars <- mapM argVar args
  if Set.size (Set.fromList vars) == length vars
    then Right (headCon, vars)
    else Left (InvalidInstanceHead cls ty)
  where
    argVar arg =
      case arg of
        S.TypeVar name -> Right name
        _ -> Left (InvalidInstanceHead cls ty)

unapplyTypeApps :: S.Type -> (S.Type, [S.Type])
unapplyTypeApps =
  go []
  where
    go args t =
      case t of
        S.TypeApp f x ->
          go (x : args) f
        _ ->
          (t, args)

renderInstanceHead :: Text -> S.Type -> Text
renderInstanceHead cls ty =
  cls <> " " <> renderType ty

renderType :: S.Type -> Text
renderType ty =
  case ty of
    S.TypeVar v ->
      v
    S.TypeCon n ->
      n
    S.TypeApp f x ->
      renderType f <> " " <> renderTypeAtom x
    S.TypeArrow a b ->
      renderTypeAtom a <> " -> " <> renderType b
    S.TypeRecord fields ->
      "{ " <> T.intercalate ", " [name <> " : " <> renderType t | (name, t, _) <- fields] <> " }"
  where
    renderTypeAtom t =
      case t of
        S.TypeArrow {} -> "(" <> renderType t <> ")"
        S.TypeApp {} -> "(" <> renderType t <> ")"
        _ -> renderType t
