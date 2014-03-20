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
import Language.Haskell.Pretty (prettyPrint)

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
typ app@(HsTyApp _ _) = uncur [] app
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
expr e = case e of
  (HsVar qn) -> tell $ qname qn
  (HsCon qn) -> tell $ qname qn

  (HsLit lit) -> tell $ prettyPrint lit -- printing literals as Haskell

  (HsInfixApp e1 op e2) -> do
    expr e1
    tell $ qname $ case op of
      (HsQVarOp qn) -> qn
      (HsQConOp qn) -> qn
    expr e2

  (HsNegApp (HsLit lit)) -> tell $ "-" ++ prettyPrint lit
  (HsNegApp e)           -> tell "(0 - " >> expr e >> tell ")"

  (HsLambda _ pats exp) -> error "lambda"

  (HsLet decls expr) -> do tell "(block:\n"
                           indent $ block M.empty (HsUnGuardedRhs e) decls
                           tell "end)"

  (HsIf a b c) -> do tell "if "     >> expr a
                     tell ": "      >> expr b
                     tell " else: " >> expr c

  (HsCase e alts) -> do
    tell $ "cases (TYPE) " >> expr e >> tell ":\n"
    indent $ forM_

  (HsList es) -> tell $ encloseSeperate "[" "]" ", " $ mapToString expr es
  (HsParen e) -> tell "(" >> expr e >> tell ")"

  (HsExpTypeSig _ e t) -> tell "(" >> expr e >> tell " :: " >> (tell $ qtyp t) >> tell ")"

  (HsApp _ _) -> uncur [] e

  where uncur :: [HsExp] -> HsExp -> Writer String ()
        uncur args (HsApp a b) = uncur (b : args) a
        uncur args e           = do expr e
                                    tell $ encloseSeperate "(" ")" ", " $ mapToString (uncur []) args

        mapToString :: (a -> Writer b ()) -> [a] -> [b]
        mapToString f = fmap $ snd . runWriter . f


test :: ParseResult HsModule -> IO ()
test (ParseOk mod) = putStrLn $ snd $ toPyret mod

ast = parseModule <$> readFile "/home/jcericso/git/pyrec/haskell/Pyrec/AST.hs"
ast' = test =<< ast

report = parseModule <$> readFile "/home/jcericso/git/pyrec/haskell/Pyrec/Report.hs"
report' = test =<< report
