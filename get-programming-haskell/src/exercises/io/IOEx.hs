module IOEx where

import System.Random

{-
Java! Yuk!
Let's say we have the following two functions:

public class Yuk {
  public static int mystery1(int val1, int val2) {
    int val3 = 3;
    return Math.pow(val1 + val2 + val3, 2);
  }

  public static int mystery2(int val1, int val2) {
    int val3 = 3;
    System.out.print("Enter a number");
         
    try {
      Scanner in = new Scanner(System.in);
      val3 = in.nextInt();
    } catch (IOException e) {
      e.printStackTrace();
    }
    
    return Math.pow(val1 + val2 + val3, 2);
  }
}

Function mystery1 takes 2 Ints and results in an Int.
It is pure, even though a client may not know about val3 i.e. the name mystery1 hides this fact.
However, after playing around with this (maybe 3rd party) function, a client can work out what it does and can write a unit test.

But mystery2 is impure. It involves IO, but the Java contract does not show that and so we are in the world of non-determinism.

Haskell solves this problem by forcing these two functions to be different types.
Whenever a function uses IO, the results of that function are forever marked as coming from IO.
-}

mystery1 :: Int -> Int -> Int
mystery1 val1 val2 =
  (val1 + val2 + val3) ^ 2
  where val3 = 3
 
mystery2 :: Int -> Int -> IO Int
mystery2 val1 val2 = do
  putStrLn "Enter a number"
  val3Input <- getLine
  let val3 = read val3Input
  return ((val1 + val2 + val3) ^ 2)

{-
IO makes it impossible to accidentally use values that have been tainted with I/O in other, pure functions.
For example, addition is a pure function, so you can add the results of two calls to mystery1:

safeValue = (mystery1 2 4) + (mystery1 5 6)

But if you try to do the same thing, you’ll get a compiler error:
 
unsafeValue = (mystery2 2 4) + (mystery2 2 4)
"No instance for (Num (IO Int)) arising from a use of '+'"
-}

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

minDie :: Int
minDie = 1
 
maxDie :: Int
maxDie = 6

main :: IO ()
main = do
  dieRoll <- randomRIO (minDie, maxDie)
  let youRolled = "you rolled " ++ show dieRoll
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name ++ " " ++ youRolled
  putStrLn statement