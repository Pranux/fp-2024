module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["remove-movie", "add-movie", "print-movies", "compound-query"]
