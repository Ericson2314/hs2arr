{-# LANGUAGE FlexibleContexts #-}
module HS2Arr where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer

import Data.List
import Data.Functor.Identity

import Language.Haskell.Syntax
import Language.Haskell.Parser

for = flip map
for2 a b f = zipWith f a b

unIdent :: HsName -> String
unIdent (HsIdent s) = s

encloseSeperate :: String -> String -> String -> [String] -> String
encloseSeperate _ _ _ [] = []
encloseSeperate b e s l  = b ++ intercalate s l ++ e



toPyret :: HsModule -> (String, String)
toPyret (HsModule _ (Module mname) _ _ prog) = (mname, snd $ runWriter $ mapM decl prog)

specialCon :: HsSpecialCon -> String
specialCon a = case a of
  HsUnitCon -> "Nothing"
  HsListCon -> "List"
  HsCons    -> "link"

dq :: HsQName -> String
dq (Qual (Module mod) (HsIdent s)) = mod ++ "." ++ s
dq (UnQual (HsIdent s)) = s
dq (Special s) = specialCon s

typ :: HsType -> String
typ (HsTyVar (HsIdent tvar)) = tvar
typ (HsTyCon tqid)           = dq tqid
typ app = uncur [] app
  where uncur :: [HsType] -> HsType -> String
        uncur args (HsTyApp a b) = uncur (b : args) a
        uncur args t             = typ t ++ (encloseSeperate "<" ">" ", " $ map (uncur []) args)

decl :: HsDecl -> Writer String ()

decl (HsTypeDecl _ (HsIdent name) [] t) =
  tell $ "# s/" ++ name ++"/" ++ typ t

decl (HsDataDecl _ _ (HsIdent name) params cons _) = do
  tell $ "data " ++ name
  tell $ encloseSeperate "<" ">" ", " $ map unIdent params
  tell ":\n"
  forM_ cons $ \(HsConDecl _ (HsIdent name) params) -> do
    tell $ "  | " ++ name ++ " "
    tell $ encloseSeperate "(" ")" ", " $ for2 params ([1..] :: [Int]) $
      \(HsUnBangedTy t) i -> "_p-" ++ show i ++ " :: " ++ typ t
    tell "\n"
  tell "end\n\n"

--decl (HsInfixDecl _ HsAssoc Int HsOP) =

ast :: IO (ParseResult HsModule)
ast = parseModule <$> readFile "/home/jcericso/git/pyrec/haskell/Pyrec/AST.hs"

report :: IO (ParseResult HsModule)
report = parseModule <$> readFile "/home/jcericso/git/pyrec/haskell/Pyrec/HS2Arr.hs"

dst :: IO ()
dst = do (ParseOk mod) <- ast
         putStrLn $ snd $ toPyret mod
