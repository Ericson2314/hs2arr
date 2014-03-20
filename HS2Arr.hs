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
toPyret (HsModule _ (Module mname) _ _ prog) =
  (mname, snd $ runWriter $ foldM decl M.empty prog)

specialCon :: HsSpecialCon -> String
specialCon a = case a of
  HsUnitCon -> "Nothing"
  HsListCon -> "List"
  HsCons    -> "link"

dq :: HsQName -> String
dq (Qual (Module mod) (HsIdent s)) = mod ++ "." ++ s
dq (UnQual (HsIdent s)) = s
dq (Special s) = specialCon s

qtyp :: HsQualType -> String
qtyp (HsQualType [] t) = typ t

typ :: HsType -> String
typ (HsTyVar (HsIdent tvar)) = tvar
typ (HsTyCon tqid)           = dq tqid
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
    forM_ cons $ \(HsConDecl _ (HsIdent name) params) -> do
      tell $ "  | " ++ name ++ " "
      tell $ encloseSeperate "(" ")\n" ", " $ for2 params [1..] $
        \(HsUnBangedTy t) i -> "_p-" ++ show i ++ " :: " ++ typ t
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
          tell $ encloseSeperate "(" "):\n" ", " $ for2 vars (splitArgs $ mp ! name) $
            \(HsPVar (HsIdent var)) t -> var ++ " :: " ++ typ t
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

block :: TMap -> t -> [HsDecl] -> Writer String ()
block mp expr bindings = do
  foldM_ decl mp bindings
  tell "EXPR\n"

test :: ParseResult HsModule -> IO ()
test (ParseOk mod) = putStrLn $ snd $ toPyret mod

ast = parseModule <$> readFile "/home/jcericso/git/pyrec/haskell/Pyrec/AST.hs"
ast' = test =<< ast

report = parseModule <$> readFile "/home/jcericso/git/pyrec/haskell/Pyrec/Report.hs"
report' = test =<< report
