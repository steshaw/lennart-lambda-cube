module CubeExpr(
    Sym, Expr(..), Type,
    subst, nf, alphaEq, typeCheck, skipLambda
    ) where
import Data.Char(isAlphaNum, isAlpha)
import Data.List(union, (\\))
import Control.Monad.Error
import Text.PrettyPrint.HughesPJ(Doc, renderStyle, style, text, (<>), (<+>), parens, ($$),
       				 vcat, punctuate, sep, fsep, nest)
import Text.ParserCombinators.ReadP(ReadP, (+++), char, munch1, many1, string, pfail, sepBy,
                                    optional, many, skipSpaces, readP_to_S, look)

type Sym = String

data Expr
        = Var Sym
        | App Expr Expr
        | Lam Sym Type Expr
        | Pi Sym Type Type
	| Let Sym Type Expr Expr
        | Kind Kind
        deriving (Eq)

type Type = Expr

data Kind = Star | Box deriving (Eq)

expandLet :: Sym -> Type -> Expr -> Expr -> Expr
expandLet i t e b = App (Lam i t b) e

freeVars :: Expr -> [Sym]
freeVars (Var s) = [s]
freeVars (App f a) = freeVars f `union` freeVars a
freeVars (Lam i t e) = freeVars t `union` (freeVars e \\ [i])
freeVars (Pi i k t) = freeVars k `union` (freeVars t \\ [i])
freeVars (Let i t e b) = freeVars (expandLet i t e b)
freeVars (Kind _) = []

subst :: Sym -> Expr -> Expr -> Expr
subst v x = sub
  where sub e@(Var i) = if i == v then x else e
        sub (App f a) = App (sub f) (sub a)
        sub (Lam i t e) = abstr Lam i t e
        sub (Pi i t e) = abstr Pi i t e
	sub (Let i t e b) = let App (Lam i' t' b') e' = sub (expandLet i t e b)
	    	       	    in  Let i' t' e' b'
        sub (Kind k) = Kind k
        fvx = freeVars x
        cloneSym e i = loop i
           where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
                 vars = fvx ++ freeVars e
        abstr con i t e =
            if v == i then
                con i (sub t) e
            else if i `elem` fvx then
                let i' = cloneSym e i
                    e' = substVar i i' e
                in  con i' (sub t) (sub e')
            else
                con i (sub t) (sub e)

whnf :: Expr -> Expr
whnf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s _ e) (a:as) = spine (subst s a e) as
	spine (Let i t e b) as = spine (expandLet i t e b) as
        spine f as = foldl App f as

nf :: Expr -> Expr
nf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s t e) [] = Lam s (nf t) (nf e)
        spine (Lam s _ e) (a:as) = spine (subst s a e) as
        spine (Pi s k t) as = app (Pi s (nf k) (nf t)) as
	spine (Let i t e b) as = spine (expandLet i t e b) as
        spine f as = app f as
        app f as = foldl App f (map nf as)

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' e = subst s (Var s') e

alphaEq :: Expr -> Expr -> Bool
alphaEq (App f a)   (App f' a')    = alphaEq f f' && alphaEq a a'
alphaEq (Lam s t e) (Lam s' t' e') = alphaEq t t' && alphaEq e (substVar s' s e')
alphaEq (Pi s k t)  (Pi s' k' t')  = alphaEq k k' && alphaEq t (substVar s' s t')
alphaEq (Let s t e b) (Let s' t' e' b') = alphaEq t t' && alphaEq e e' && alphaEq b (substVar s' s b')
alphaEq (Var s)     (Var s')       = s == s'
alphaEq (Kind k)    (Kind k')      = k == k'
alphaEq _ _ = False

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

-------------------------------

type ErrorMsg = String

data Env = Env [(Sym, Type)] deriving (Show)

initalEnv :: Env
initalEnv = Env []

extend :: Sym -> Type -> Env -> Env
extend s t (Env r) = Env ((s, t) : r)

findVar :: Env -> Sym -> TC Type
findVar (Env r) s =
    case lookup s r of
    Just t -> return t
    Nothing -> throwError ("Cannot find variable " ++ s)

type TC a = Either ErrorMsg a

tCheck :: Env -> Expr -> TC Type
tCheck r (Var s) =
    findVar r s
tCheck r (Let s t a e) = do
    tCheck r t
    ta <- tCheck r a
    when (not (betaEq ta t)) $ throwError $ "Bad let def\n" ++ show (ta, t)
    te <- tCheck r (subst s a e)
    tCheck r (Pi s t te)
    return te
tCheck r (App f a) = do
    tf <- tCheckRed r f
    case tf of
     Pi x at rt -> do
        ta <- tCheck r a
        when (not (betaEq ta at)) $ throwError $ "Bad function argument type:\n" ++
	     	  	     	    	       	 "Function: " ++ show (nf f) ++ "\n" ++
						 "    arg type: " ++ show at ++ "\n" ++
						 "Argument: " ++ show (nf a) ++ "\n" ++
						 "    type: " ++ show ta
        return $ subst x a rt
     _ -> throwError $ "Non-function in application: " ++ show tf
tCheck r (Lam s t e) = do
    tCheck r t
    let r' = extend s t r
    te <- tCheck r' e
    let lt = Pi s t te
    tCheck r lt
    return lt
tCheck r (Pi x a b) = do
    s <- tCheckRed r a
    let r' = extend x a r
    t <- tCheckRed r' b
    when ((s, t) `notElem` allowedKinds) $ throwError $ "Bad abstraction: " ++ show (Pi x a b)
    return t
tCheck _ (Kind Star) = return $ Kind Box
tCheck _ (Kind Box) = throwError "Found a Box"

allowedKinds :: [(Type, Type)]
allowedKinds = [(Kind Star, Kind Star), (Kind Star, Kind Box), (Kind Box, Kind Star), (Kind Box, Kind Box)]

tCheckRed :: Env -> Expr -> TC Type
tCheckRed r e = do
    t <- tCheck r e
    return $ whnf t

typeCheck :: Expr -> Either ErrorMsg Type
typeCheck e = fmap nf $ tCheck initalEnv e
--    case  of
--    Left msg -> error ("Type error:\n" ++ msg)
--    Right t -> nf t

---------------------------------------------------------------------

ppsExpr :: Expr -> String
ppsExpr e = renderStyle style $ ppExpr 0 e

ppExpr :: Int -> Expr -> Doc
ppExpr p (Pi s t e) | s `notElem` freeVars e = pparens (p > 0) $ ppExpr 1 t <> text "->" <> ppExpr 0 e
--ppExpr p (Pi s t e) = pparens (p > 0) $ (parens $ text s <> text "::" <> ppExpr 0 t) <> text "->" <> ppExpr 0 e
ppExpr p l@(Pi _ _ _) = pparens (p > 0) $ text "forall" <+> (fsep $ args ++ [text ".", ppExpr 0 b])
  where (args, b) = collectPi [] l
        collectPi vts (Pi v t e) | v `elem` freeVars e = collectPi (ppBound v t : vts) e
	collectPi vts e = (reverse vts, e)
ppExpr p l@(Lam _ _ _) = pparens (p > 0) $ text "\\" <+> (fsep $ args ++ [text "->", ppExpr 0 b])
  where (args, b) = collectLam [] l
        collectLam vts (Lam v t e) = collectLam (ppBound v t : vts) e
	collectLam vts e = (reverse vts, e)

ppExpr p ee@(Let _ _ _ _) = 
    let (stes, body) = collectBinds [] ee
	ppBind (s, t, Just e) = sep [text s <+> text "::" <+> ppExpr 0 t <> text " =", nest 4 $ ppExpr 0 e]
	ppBind (s, t, Nothing) = text s <+> text "::" <+> ppExpr 0 t
	ppBinds xs = vcat $ punctuate (text ";") (map ppBind xs)
	collectBinds bs (Let s t e b) = collectBinds (bs ++ [(s, t, Just e)]) b
--	collectBinds bs (Lam s t b) = collectBinds (bs ++ [(s, t, Nothing)]) b
	collectBinds bs b = (bs, b)
    in  pparens (p > 0) $
        (text "let " <> ppBinds stes) $$ (text "in  " <> ppExpr 0 body)

ppExpr p (App f a) = pparens (p > 9) $ ppExpr 9 f <> text " " <> ppExpr 10 a
ppExpr _ (Var s) = text s
ppExpr _ (Kind Star) = text "*"
ppExpr _ (Kind Box) = text "[]"

ppBound :: Sym -> Expr -> Doc
ppBound v t = parens $ text v <+> text "::" <+> ppExpr 0 t

pparens :: Bool -> Doc -> Doc
pparens True d = parens d
pparens False d = d

instance Show Expr where
    show e = ppsExpr e

-------------------------------------------------------

instance Read Expr where
    readsPrec _ = readP_to_S pTop . removeComments

removeComments :: String -> String
removeComments "" = ""
removeComments ('-':'-':cs) = skip cs
  where skip "" = ""
	skip s@('\n':_) = removeComments s
	skip (_:s) = skip s
removeComments (c:cs) = c : removeComments cs

pTop :: ReadP Expr
pTop = do
    e <- pExpr
    skipSpaces
    return e

pExpr :: ReadP Expr
pExpr = pAExpr +++ pPi +++ pLam +++ pLet

pAExpr :: ReadP Expr
pAExpr = pAtomExpr +++ pApply

pType :: ReadP Expr
pType = pExpr

pAtomExpr :: ReadP Expr
pAtomExpr = pVar +++ pKind +++ pParen pExpr

pParen :: ReadP a -> ReadP a
pParen p = do
    schar '('
    e <- p
    schar ')'
    return e

pApply :: ReadP Expr
pApply = do
    f <- pAtomExpr
    as <- many1 pAtomExpr
    return $ foldl App f as

pLet :: ReadP Expr
pLet = do
    skeyword "let"
    stes <- sepBy pBind (schar ';')
    optional (schar ';')
    skeyword "in"
    b <- pExpr
    return $ eLets' stes b

pBind :: ReadP (Sym, Type, Maybe Expr)
pBind = pBindH +++ pBindR

pBindH :: ReadP (Sym, Type, Maybe Expr)
pBindH = do
    sy <- pSym
    sstring "::"
    ty <- pType
    schar ';'
    sy' <- pSym
    as <- many pSym
    schar '='
    b <- pExpr
    e <- matchH ty as b
    if sy /= sy' then
        pfail
     else
        return (sy, ty, Just e)

matchH :: Expr -> [Sym] -> Expr -> ReadP Expr
matchH _ [] e = return e
matchH (Pi v t t') (a:as) e | v == a || v == "_" = do
    e' <- matchH t' as e
    return (Lam a t e')
matchH _ _ _ = pfail

pBindR :: ReadP (Sym, Type, Maybe Expr)
pBindR = do
    let addT (s, t) r = Pi s t r
	addE (s, t) e = Lam s t e
    sy <- pSym
    args <- many pArg
    sstring "::"
    rt <- pType
    (do
        schar '='
        be <- pExpr
        return (sy, foldr addT rt args, Just $ foldr addE be args)
     ) +++
       (return (sy, foldr addT rt args, Nothing))

eLet' :: (Sym, Type, Maybe Expr) -> Expr -> Expr
eLet' (s, t, Nothing) b = Lam s t b
eLet' (s, t, Just e) b = Let s t e b

eLets' :: [(Sym, Type, Maybe Expr)] -> Expr -> Expr
eLets' stes b = foldr eLet' b stes

pPi :: ReadP Expr
pPi = pPiQuant +++ pPiArrow

pPiQuant :: ReadP Expr
pPiQuant = do
    sstring "forall" -- +++ sstring "\\/"
    sts <- (fmap (:[]) pVarType) +++ many1 (pParen pVarType)
    schar '.'
    e <- pType
    return $ foldr (uncurry Pi) e sts
--Pi s t e

pPiArrow :: ReadP Expr
pPiArrow = do
    ts <- many1 (do e <- pPiArg; sstring "->"; return e)
    rt <- pAExpr
    return $ foldr (\ (s, t) r -> Pi s t r) rt ts
  where pPiArg = pPiNoDep +++ pArg
	pPiNoDep = do
	    t <- pAExpr
	    return ("_", t)

pArg :: ReadP (Sym, Type)
pArg = pParen pVarType

pVarType :: ReadP (Sym, Type)
pVarType = do
    s <- pSym
    sstring "::"
    t <- pType
    return (s, t)
    
pLam :: ReadP Expr
pLam = do
    schar '\\' --  +++ sstring "/\\"
    sts <- fmap (:[]) pVarType +++ many1 (pParen pVarType)
    sstring "->"
    e <- pExpr
    return $ foldr (uncurry Lam) e sts

pVar :: ReadP Expr
pVar = do
    s <- pSym
    return $ Var s

pKind :: ReadP Expr
pKind = do
    (do schar '*'; return $ Kind Star) +++ (do sstring "[]"; return $ Kind Box)

pSym :: ReadP Sym
pSym = do
    skipSpaces
    cs <- munch1 isSym
    if cs `elem` ["let", "in", "forall", "_"] then
	pfail
     else
        return cs

schar :: Char -> ReadP ()
schar c = do
    skipSpaces
    char c
    return ()

sstring :: String -> ReadP ()
sstring s = do
    skipSpaces
    string s
    return ()

skeyword :: String -> ReadP ()
skeyword s = do
    sstring s
    cs <- look
    case cs of
     c:_ | isAlpha c -> pfail
     _ -> return ()

isSym :: Char -> Bool
isSym c = isAlphaNum c || c `elem` "_'"

-------

skipLambda :: Expr -> Type -> (Expr, Type)
skipLambda (Lam _ _ e) (Pi _ _ t) = skipLambda e t
skipLambda e t = (e, t)
