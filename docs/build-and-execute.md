# Build and Execute

## Executable

Let's say we have a Haskell source file named **Foo.hs** and we wish to build an executable named **foo**:

```haskell
module Main where

main :: IO ()
main = print "You've been Fooed"
```

```bash
ghc Foo.hs -o foo
```

and now we can execute:

```bash
./foo
```

Take note, that the exposed module is named **Main**.

If we wish to name the module differently, then we must inform GHC when building an executable, since GHC defaults to expecting an executable to have its module named as **Main** (of which you will no doubt have one in your overall project but may need other executables for maybe testing). So, another example:

```haskell
module EchoArgs where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  mapM_ putStrLn args
```

```bash
ghc -main-is EchoArgs EchoArgs.hs -o echo-args
```

and again, to execute:

```bash
./echo-args
```