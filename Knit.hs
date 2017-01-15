-- Authors:
--   Mike MacHenry    https://github.com/mmachenry
--   Benjamin Staffin https://github.com/benley

import System.Environment (getArgs)
import KnitParse

-- For the right side
cmd2Str cmd = case cmd of
  K -> "|"
  K2Tog -> "/"
  K3Tog -> "/"
  P -> "-"
  P2Tog -> "/"
  Slppsso -> "/"
  Slkpsso -> "\\"
  Ssk -> "\\"
  Yo -> "O"

runKnit :: [Row] -> [[Command]] -> [String]
runKnit [] result = map (concatMap cmd2Str) result
runKnit (row:rows) result = runKnit rows (knitRow row : result)

knitRow row = case row of
  WrongSide _ cmds -> map wrongSideTrans cmds
  RightSide _ cmds -> reverse cmds

wrongSideTrans cmd = case cmd of
  K -> P
  P -> K
  a -> a

test :: [Row]
test = [
  WrongSide 1 (replicate 121 K),
  RightSide 2 (replicate 121 P),
  WrongSide 3 (replicate 4 K ++ replicate 113 P ++ replicate 4 K),
  RightSide 4 (
    (replicate 4 P)
    ++ (replicate 12 K)
    ++ [Ssk, Yo, K,K,K, Ssk, Yo, K,K,K, Ssk, Yo, K,K,K,K,K, Ssk, Yo]
    ++ (replicate 8 K)
    ++ [Ssk, Yo, K,K,K, Ssk, Yo]
    ++ (replicate 7 K)
    ++ [Yo, K2Tog]
    ++ (replicate 6 K)
    ++ [Ssk, Yo]
    ++ (replicate 10 K)
    ++ [Ssk, Yo, K,K,K, Ssk, Yo, K,K, Yo, Slppsso, Yo, K,K,K, Ssk, Yo]
    ++ (replicate 9 K)
    ++ [Yo, K2Tog, K,K,K,K, Ssk, Yo, K,K,K, Ssk, Yo, K, P,P,P,P])
  ]

main = do
  [filename] <- getArgs
  f <- readFile filename
  let l = lines f
  let rows = map readRow l
  let output = runKnit rows []
  mapM_ putStrLn output

{-
file = [
  "Row 1 (ws): k121",
  "Row 2 (rs): p121",
  "Row 3 (ws): k4, p113, k4",
  "Row 4 (rs): p4, k12, ssk, yo, k3, ssk, yo, k3, ssk, yo, k5, ssk, yo, k8,ssk, yo, k3, ssk, yo, k7, yo, k2 tog, k6, ssk, yo, k10, ssk, yo, k3, ssk, yo, k2, yo, sl 1-k2 tog-psso, yo, k3, ssk, yo, k9, yo, k2 tog, k4, ssk, yo, k3, ssk, yo, k1, p4"
  ]
-}
