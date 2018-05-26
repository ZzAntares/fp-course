{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FileIO where

import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.List
import           Course.Monad

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile :: FilePath -> Chars -> IO ()
-- printFile name contents = putStrLn name >>= \_ -> putStrLn contents
-- printFile name contents = putStrLn name >>= const (putStrLn contents)
printFile name contents = do
  putStrLn $ "============ " ++ name
  putStrLn contents

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles :: List (FilePath, Chars) -> IO ()
-- printFiles = void . sequence . (uncurry printFile <$>)
-- printFiles = void . traverse' (uncurry printFile)
printFiles = traverse_ (uncurry printFile)
--printFiles fps =
--  let
--    pf  = uncurry printFile  -- Make printFile to: (FilePath, Chars) -> IO ()
--    res = pf <$> fps         -- Get List (IO ())
--    ios = sequence res       -- List (IO ())   ->   IO (List ())
--  -- void :: IO a -> IO ()
--  -- in ios >>= \_ -> pure ()   -- Ignore list results, we care for the effects
--  in void ios   -- Ignore list results, we care for the effects
-- printFiles fs =
--   let
--     pf  = flip uncurry <$> fs     -- In the fs functor convert pairs to arrows
--     pc  = pf <*> (pure printFile) -- Apply printFile to all items in pf
--     ios = sequence pc             -- Turn List (IO ()) -> IO (List ())
--   in ios >>= (pure . const ())    -- Ignore list results, we care for effects

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile :: FilePath -> IO (FilePath, Chars)
-- getFile fp = readFile fp >>= (\chars -> return (fp, chars))
-- getFile fp = readFile fp >>= return . (,) fp
-- getFile fp = (\chars -> (fp, chars)) <$> readFile fp
getFile fp = (,) fp <$> readFile fp

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles :: List FilePath -> IO (List (FilePath, Chars))
-- getFiles fps = sequence $ getFile <$> fps
-- getFiles fps = sequence . (getFile <$>) $ fps
-- getFiles = sequence . (getFile <$>)
-- getFiles = sequence . (<$>) getFile
getFiles = traverse' getFile

-- Given a file name, read it and for each line in that file, read and print
-- contents of each.  Use @getFiles@ and @printFiles@.
run :: FilePath -> IO ()
-- run fp = readFile fp >>= (\conts -> getFiles (lines conts) >>= printFiles)
run fp = do
  contents <- readFile fp
  files    <- getFiles $ lines contents
  printFiles files
-- run fp = readFile fp >>= (printFiles =<<) . (getFiles . lines)
-- run fp = (printFiles =<<) . (getFiles . lines) =<< readFile fp
-- run = ((printFiles =<<) . (getFiles . lines) =<<) . readFile
-- Relevant bindings:
--   readFile   :: FilePath -> IO Chars
--   printFiles :: List (FilePath, Chars) -> IO ()
--   getFiles   :: List FilePath -> IO (List (FilePath, Chars))

-- /Tip:/ use @getArgs@ and @run@
main :: IO ()
-- main = getArgs >>= (\fs -> void $ sequence (run <$> fs))
-- main = getArgs >>= void . sequence . (run <$>)
-- main = getArgs >>= void . sequence . (<$>) run
main = getArgs >>= traverse_ run
-- Relevant bindings:
--   run     :: FilePath -> IO ()
--   getArgs :: IO (List Chars)

----

-- Was there was some repetition in our solution?
-- This is the traverse function?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.

traverse' :: Applicative f => (a -> f b) -> List a -> f (List b)
traverse' f xs = sequence $ f <$> xs
-- traverse' f = sequence . (<$>) f

traverse_ :: Applicative f => (a -> f b) -> List a -> f ()
-- traverse_ f xs = void $ traverse' f xs
traverse_ f = void . traverse' f
