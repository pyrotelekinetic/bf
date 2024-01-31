import Parser
import Interpreter

import Data.Text (Text)

runCode :: Text -> IO Tape
runCode = exec empty . runParse

runDebug :: Text -> IO Tape
runDebug = execDebug empty . runParse

main :: IO ()
main = putStrLn "noerr"
