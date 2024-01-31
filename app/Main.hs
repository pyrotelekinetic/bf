import Parser
import Interpreter

import Data.Text (Text)

runCode :: Text -> IO Tape
runCode = exec (initTape 10) . runParse

runDebug :: Text -> IO Tape
runDebug = execDebug (initTape 10) . runParse

main :: IO ()
main = putStrLn "noerr"
