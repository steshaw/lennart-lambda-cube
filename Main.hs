-- Simple interface to lambda cube type checker.
import System.Environment(getArgs)
import Data.List(sortBy)
import Data.Function(on)
import Cube

main :: IO ()
main = do
    args <- getArgs
    case args of
	["-?"] -> help
	["-help"] -> help
	["--help"] -> help
        [] -> interactive
	_  -> batch args

help :: IO ()
help = putStr "\
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

interactive :: IO ()
interactive = undefined
