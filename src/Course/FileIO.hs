{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

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
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile fp s = do
                 putStrLn $ "============ " ++ fp
                 putStrLn s
                 return ()

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars)
  -> IO ()
-- foldRight :: ((FilePath, Chars) -> IO () -> IO ()) -> IO () -> List (FilePath, Chars) -> IO ()
-- sequence ::  List (IO a)  -> IO (List a)
-- printFiles fs = foldRight (\(fp, chars) _ -> printFile fp chars) (return ()) fs
printFiles fs = void . sequence $ (\(fp, chars) -> printFile fp chars) <$> fs


-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath
  -> IO (FilePath, Chars)
-- readFile :: FilePath -> IO Chars
-- \x -> (f, x) :: IO Chars -> IO (FilePath, Chars)
getFile fp = (\x -> (fp, x)) <$> readFile fp

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
-- List FilePath -> List (IO (FilePath, Chars))  then sequence
getFiles fs = sequence $ getFile <$> fs

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
-- getFiles ::  List FilePath  -> IO (List (FilePath, Chars))
-- printFiles ::  List (FilePath, Chars)  -> IO ()
-- readFile :: FilePath -> IO Chars
-- lines :: Chars -> List Chars
-- (<=<) ::  Monad f =>  (b -> f c)  -> (a -> f b)  -> a  -> f c
-- (>>=) ::  Monad f =>  f a  -> (a -> f b)  -> f b
-- (<*>) ::    f (a -> b)    -> f a    -> f b
-- (<$>) ::    (a -> b)    -> f a    -> f b
run fp = lines <$> readFile fp >>= getFiles >>= printFiles

-- /Tip:/ use @getArgs@ and @run@
-- :main "share/files.txt"
main ::
  IO ()
main = do
  args <- getArgs
  run $ headOr "" args
  return ()

----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
