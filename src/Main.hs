import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano
import           Language.Nano.Repl
import           Text.Printf
import           GHC.IO.Encoding

main :: IO ()                             
main = do
  setLocaleEncoding utf8
  putStrLn(Hello)
  loop 0 []

--------------------------------------------------------------------------------
-- | Some useful functions 
--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   :: 
-- putStrLn :: String -> IO ()
-- getLine  :: IO String 

loop :: Int -> Nano.Env -> IO()
loop i env = do
  task <- getLine
  case strCmd task of
      CQuit -> do
               doQuit
               loop(i+1) env
      Cload str -> do
                    envs <- doLoad str
                    helper(envs)
                    helper(envs)
                    loop(i+1) (envs ++ env)
                      where
                      helper :: Nano.Env -> IO()
                      helper ls = do
                        putStrLn("definition: " ++ init(printLs ls))
                      where
                        printLs :: Nano.Env -> String
                        printLs [] = []
                        printLs ((xId, xVal): xs) = xId ++ " " ++ printLs xs
      CEval str -> do
                   doEval env str
                   loop(i+1) env
      CRun str -> do
                  doRun str
                  loop(i+1) env
      CUnkown -> do
                 doUnknown
                 loop(i+1) env

