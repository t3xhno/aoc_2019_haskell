module Main where

import Data.Array
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Memory = Array Int Int
data Machine = Machine { getMemory :: Memory, getIp :: Int, isHalt :: Bool } deriving (Show)

makeMachine :: Memory -> Machine
makeMachine mem = Machine mem 0 False

step :: Machine -> Machine
step machine@(Machine _ _ True) = machine
step machine@(Machine memory ip _) =
  let opcode = memory ! getIp machine in
  case opcode of
    1 -> let a1 = memory ! (memory ! (ip + 1))
             a2 = memory ! (memory ! (ip + 2))
             dst =  memory ! (ip + 3)
          in Machine { getMemory = memory // [(dst, a1 + a2)]
                     , getIp = ip + 4
                     , isHalt = isHalt machine
                     }
    2 -> let a1 = memory ! (memory ! (ip + 1))
             a2 = memory ! (memory ! (ip + 2))
             dst =  memory ! (ip + 3)
          in Machine { getMemory = memory // [(dst, a1 * a2)]
                     , getIp = ip + 4
                     , isHalt = isHalt machine
                     }
    99 -> machine { isHalt = True }
    _ -> error $ printf "Unknown opcode %d at position %d" opcode ip

execute :: Machine -> Machine
execute = head . dropWhile (not . isHalt) . iterate step

provideInput :: Int -> Int -> Machine -> Machine
provideInput n v machine@(Machine memory _ _) = machine { getMemory = memory // [(1, n), (2, v)] }

output :: Int -> Int -> Machine -> Int
output n v = (! 0) . getMemory . execute . provideInput n v

solve_1 :: Machine -> Int
solve_1 = output 12 2

solve_2 :: Machine -> Int
solve_2 machine = head [100 * n + v | n <- [0..99], v <- [0..99], output n v machine == 19690720]

main :: IO ()
main = do
  [fName] <- getArgs
  input <- map ((read :: String -> Int) . T.unpack . T.strip) . T.splitOn (T.pack ",") <$> T.readFile fName
  let n        = length input
      contents = array (0, n - 1) (zip [0..] input)
      machine  = makeMachine contents
  print $ map ($ machine) [solve_1, solve_2]