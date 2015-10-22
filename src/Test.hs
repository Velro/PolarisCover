module Test (main) where

import qualified System.Random as R

main = do
  r <- randomValue
  print r


--random number between 0 and 1
randomValue :: IO Float
randomValue = do
    gen <- R.newStdGen
    let (result, _) = R.randomR (0, 1) gen
    return result


randomArray :: Int -> [IO Float] -> [IO Float]
randomArray index array =
    if (index > 0)
    then do newVal <- randomValue; return (randomArray (index -1) (randomValue:array))
    else return 0:array
