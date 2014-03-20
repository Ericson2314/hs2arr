{-# LANGUAGE FlexibleContexts #-}
module HS2Arr where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer

import Data.Map as M
import Data.List (intercalate)
import Data.Functor.Identity

import Language.Haskell.Syntax
import Language.Haskell.Parser

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap
for2 a b f = zipWith f a b

unIdent :: HsName -> String
unIdent (HsIdent s) = s

encloseSeperate :: String -> String -> String -> [String] -> String
encloseSeperate _ _ _ [] = []
encloseSeperate b e s l  = b ++ intercalate s l ++ e

indent :: Functor m => WriterT String m a -> WriterT String m a
indent = WriterT
         . (fmap fmap fmap $ \w -> concat $ (\a -> "  " ++ a ++ "\n") <$> lines w)
         . runWriterT


toPyret :: HsModule -> (String, String)
toPyret (HsModule _ (Module mname) _ imports prog) =
  (,) (mname ++ ".arr") $ snd $ runWriter $ do
    mapM_ impor imports
    tell "\n"
    foldM decl M.empty prog

impor :: MonadWriter [Char] m => HsImportDecl -> m ()
impor (HsImportDecl _ (Module mod) True (Just (Module as)) Nothing) =
  tell $ "import \"" ++ mod ++ ".arr\" as " ++ as ++ "\n"

specialCon :: HsSpecialCon -> String
specialCon a = case a of
  HsUnitCon -> "Nothing"
  HsListCon -> "List"
  HsCons    -> "link"

qname :: HsQName -> String
qname (Qual (Module mod) (HsIdent s)) = mod ++ "." ++ s
qname (UnQual (HsIdent s)) = s
qname (Special s) = specialCon s

qtyp :: HsQualType -> String
qtyp (HsQualType [] t) = typ t

typ :: HsType -> String
typ (HsTyVar (HsIdent tvar)) = tvar
typ (HsTyCon tqid)           = qname tqid
typ (HsTyFun t1 t2)          = typ t1 ++ " -> " ++ typ t2
typ app = uncur [] app
  where uncur :: [HsType] -> HsType -> String
        uncur args (HsTyApp a b) = uncur (b : args) a
        uncur args t             = typ t ++ (encloseSeperate "<" ">" ", " $ fmap (uncur []) args)

type TMap = Map String HsType

decl :: TMap -> HsDecl -> Writer String TMap
decl mp declerations = case declerations of
  (HsTypeDecl _ (HsIdent name) [] t) -> do
    tell $ "# s/" ++ name ++"/" ++ typ t ++ "/\n"
    return mp

  (HsDataDecl _ _ (HsIdent name) params cons _) -> do
    tell $ "data " ++ name
    tell $ encloseSeperate "<" ">" ", " $ fmap unIdent params
    tell ":\n"
    indent $ forM_ cons $ \(HsConDecl _ (HsIdent name) params) -> do
      tell $ "| " ++ name ++ " "
      tell $ encloseSeperate "(" ")" ", " $ for2 params [1..] $
        \(HsUnBangedTy t) i -> "_p-" ++ show i ++ " :: " ++ typ t
      tell "\n"
    tell "end\n\n"
    return mp

  (HsInfixDecl _ _ _ _) -> error "infix"
  (HsDefaultDecl _ types) -> error "default"
  (HsTypeSig _ [HsIdent name] (HsQualType [] t)) -> return $ insert name t mp

  (HsFunBind topcases) -> do
    forM_ topcases $ \(HsMatch _ (HsIdent name) pats expr wheres) -> do
      case pats of
        [] -> tell $ name ++ " = block:"
        vars -> do
          tell $ "fun " ++ name ++ " "
          tell $ encloseSeperate "(" "):" ", " $ for2 vars (splitArgs $ mp ! name) $
            \(HsPVar (HsIdent var)) t -> var ++ " :: " ++ typ t
      tell "\n"
      indent $ block mp expr wheres
      tell "end\n\n"
    return mp
    where splitArgs :: HsType -> [HsType]
          splitArgs (HsTyFun t1 t2) = t1 : splitArgs t2
          splitArgs t               = [t]

  (HsPatBind _ pat expr wheres) -> do
    tell "PATBIND = block:\n"
    indent $ block mp expr wheres
    tell "end"
    return mp

block :: TMap -> HsRhs -> [HsDecl] -> Writer String ()
block mp (HsUnGuardedRhs e) bindings = do
  foldM_ decl mp bindings
  expr e

expr :: HsExp -> Writer String ()
expr e = undefined

test :: ParseResult HsModule -> IO ()
test (ParseOk mod) = putStrLn $ snd $ toPyret mod

ast = parseModule <$> readFile "/home/jcericso/git/pyrec/haskell/Pyrec/AST.hs"
ast' = test =<< ast

report = parseModule <$> readFile "/home/jcericso/git/pyrec/haskell/Pyrec/Report.hs"
report' = test =<< report
