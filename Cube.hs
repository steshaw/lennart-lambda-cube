-- Simple interface to lambda cube type checker.
import System.Environment(getArgs)
import Data.Char(isSpace)
import Data.List(sortBy)
import Data.Function(on)
import REPL
import CubeExpr

main :: IO ()
main = do
    args <- getArgs
    case args of
	["-?"] -> usage
	["-help"] -> usage
	["--help"] -> usage
        [] -> interactive
	_  -> batch args

usage :: IO ()
usage = putStr "\
\Usage: cube           -- start in interactive mode\n\
\       cube files     -- concatenate files, type check, evaluate\n\
\       cube - files   -- insert let&in, concatenate files, type check, evaluate\n\
\"

batch :: [FilePath] -> IO ()
batch ("-":names) = mapM readFile names >>= batch' . addIn . ("let " :)
  where addIn ss = init ss ++ [" in "] ++ [last ss]
batch names = mapM readFile names >>= batch'

batch' :: [String] -> IO ()
batch' files = do
    let expr = unRight "Parse error: " $ unreads $ concat files
        typ = unRight "Type error: " $ typeCheck expr
    putStrLn $ "Type:\n" ++ show typ
    putStrLn $ "Value:\n" ++ show (nf expr)

unRight :: String -> Either String a -> a
unRight msg (Left msg') = error $ msg ++ msg'
unRight _ (Right a) = a

unreads :: (Show a, Read a) => String -> Either String a
unreads s =
    let xss = reads s
    in  case filter (null . snd) xss of
        (x, _) : _ -> Right x  -- pick first when ambiguous.
        _ -> 
	    -- Find the shortest suffix and report error there.
	    let rest = head $ sortBy (compare `on` length) $ map snd xss ++ [s]
	        ls = length (filter (=='\n') s)
		lrest = length (filter (=='\n') rest)
	    in  Left $ "line " ++ show (ls - lrest) ++ ": " ++ rest

data State = State { defs :: String, skipLam :: Bool } deriving (Show)

interactive :: IO ()
interactive = repl $ REPL { repl_init = cinit, repl_eval = ceval, repl_exit = cexit }
  where cinit = do
          putStrLn "Welcome to the Cube."
	  putStrLn "Use :help to get help."
	  return ("Cube> ", State "" False)
	cexit _ = putStrLn "Bye."
	ceval s line = do
	    let rest = dropWhile isSpace $ dropWhile (not . isSpace) line
	        load = readFile rest >>= addFile s
		quit = return (True, s)
		help = do putStrLn helpMsg; return (False, s)
	    case words line of
	      [] -> return (False, s)
	      ":h" : _ -> help
	      ":help" : _ -> help
	      ":q" : _ -> quit
	      ":quit" : _ -> quit
	      ":let" : _ -> addFile s (rest ++ ";\n")
	      ":l" : _ -> load
	      ":load" : _ -> load
	      ":defs" : _ -> do putStrLn (defs s); return (False, s)
	      ":skip" : _ -> return (False, s { skipLam = not (skipLam s) })
	      _ -> do evalPrint s line 
	              return (False, s)

        evalPrint s se = do
	    mt <- readAndCheck s se
	    case mt of
	         Nothing -> return ()
		 Just (e, t) -> do
		     let v = nf e
		         (v', t') = if skipLam s then skipLambda v t else (v, t)
		     putStrLn $ show v' ++ "\n  ::\n" ++ show t'
	    return (False, s)

        addFile s n = do
	     let s' = s { defs = defs s ++ n }
	     mt <- readAndCheck s' "\\ (a::*) -> a"
	     case mt of
	         Nothing -> return (False, s)
		 Just _ ->  return (False, s')

	readAndCheck s e =
	     case unreads $ "let " ++ defs s ++ " in " ++ e of
	     Left msg -> do putStrLn $ "Syntax error: " ++ msg; return Nothing
	     Right expr ->
	         case typeCheck expr of
		 Left msg -> do putStrLn $ "Type error: " ++ msg; return Nothing
		 Right typ -> return $ Just (expr, typ)

helpMsg :: String
helpMsg = "\
\Commands:\n\
\  :defs        show loaded definitions\n\
\  :let def     add definition\n\
\  :load name   load file with definitions\n\
\  :help        show this message\n\
\  :quit        quit\n\
\  :skip        toggle skipping initial lambdas\n\
\  expr         evaluate expression\n\
\"
