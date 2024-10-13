
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    or2,
    parseAddMovie,
    parseRemoveMovie,
    parseCompoundQuery,
    parseNum,
    wordsBy,
    addMovieTransition,
    removeMovieTransition,
    printMoviesTransition,
    removeMovie,
    addMovie,
    printMovie,
    printMovies,
    Movie(director, title, year, id)
    ) where

import Data.Char as C (isDigit)

-- main data type
data Movie = Movie {
    director :: String,
    title :: String,
    year :: Integer,
    id :: Integer
    } deriving (Show, Eq)

data Query =  AddMovie String String Integer Integer
            | RemoveMovie Integer
            | PrintMovies
            | CompoundQuery Query Query

-- The instances are needed basically for tests
instance Eq Query where
    (AddMovie dir1 title1 year1 id1) == (AddMovie dir2 title2 year2 id2) =
        dir1 == dir2 && title1 == title2 && year1 == year2 && id1 == id2

    (RemoveMovie idNum1) == (RemoveMovie idNum2) =
        idNum1 == idNum2

    PrintMovies == PrintMovies = True

    (CompoundQuery q1a q1b) == (CompoundQuery q2a q2b) =
        q1a == q2a && q1b == q2b

    _ == _ = False

instance Show Query where
    show (AddMovie d t y i) =
        "AddMovie: Director = " ++ d ++ ", Title = " ++ t ++ ", Year = " ++ show y ++ ", ID = " ++ show i
    show (RemoveMovie idNum) =
        "RemoveMovie: ID = " ++ show idNum
    show PrintMovies = printMovies movieList
    show (CompoundQuery q1 q2 ) = show q1 ++ " & " ++ show q2

-- Define the Parser type
type Parser a = String -> Either String (a, String)

-- An entity which represents your program's state.
data State = State [Movie] deriving Show

-- Creates an initial program's state.
emptyState :: State
emptyState = State []

-- Utility to apply to parsers and get either first or second
or2 :: Parser a -> Parser a -> Parser a
or2 a b input
  = case a input of
      Right r1 -> Right r1
      Left e1
        -> case b input of
             Right r2 -> Right r2
             Left e2 -> Left (e1 ++ ", " ++ e2)

-- Parser for a whole query
parseQuery :: Parser Query
parseQuery input =
    case words input of
      ("add-movie" : rest) -> parseAddMovie (unwords rest)
      ("remove-movie" : rest) -> parseRemoveMovie (unwords rest)
      ("print-movies" : _) -> Right (PrintMovies, "")
      ("compound-query" : rest) -> parseCompoundQuery (unwords rest)
      _ -> Left "Invalid command."

-- Parser for add-movie
parseAddMovie :: Parser Query
parseAddMovie input =
    let (_, afterSlash) = break (== '/') input
    in case wordsBy ('/' ==) afterSlash of
        [d, t, y, i] ->
            case (parseNum y, parseNum i) of
                (Right (y', _), Right (idNum, _)) ->
                    Right (AddMovie d t y' idNum, "")
                _ -> Left "Invalid year or ID in add-movie"
        _ -> Left "Invalid add-movie input format"

-- Parser for remove-movie
parseRemoveMovie :: Parser Query
parseRemoveMovie input =
    case parseNum input of
        Right (idNum, _) -> Right (RemoveMovie idNum, "")
        _ -> Left "Invalid remove-movie input format"

-- Parser for compound-query
parseCompoundQuery :: Parser Query
parseCompoundQuery input =
    let (q1Str, r) = break (== '&') input
    in case r of
        '&' : q2Str ->
            case (parseQuery q1Str, parseQuery q2Str) of
                (Right (q1, _), Right (q2, _)) -> Right (CompoundQuery q1 q2, "")
                _ -> Left "Invalid subqueries in compound-query"
        _ -> Left "Invalid compound-query format"

-- Helper function to parse a number
parseNum :: Parser Integer
parseNum input =
    let digits = takeWhile isDigit input
        rest = drop (length digits) input
    in if null digits then Left "Expected a number" else Right (read digits, rest)

-- Helper function to split by a given delimiter
wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s =
    case dropWhile p s of
        "" -> []
        s' -> w : wordsBy p s''
              where (w, s'') = break p s'


-- Updates the state according to a query.
stateTransition :: State -> (Query, String) -> Either String (Maybe String, State)
-- Handle AddMovie
stateTransition (State movies) (AddMovie d t y i, _) =
    let newMovie = Movie d t y i
    in if newMovie `elem` movies
       then Left ("Movie " ++ show newMovie ++ " already exists.")
       else Right (Just ("Added movie: " ++ show newMovie), State (newMovie : movies))

-- Handle RemoveMovie
stateTransition (State movies) (RemoveMovie idNum, _) =
    let movieExists = any (\(Movie _ _ _ id') -> id' == idNum) movies
    in if movieExists
       then Right (Just ("Removed movie with ID: " ++ show idNum), State (filter (\(Movie _ _ _ id') -> id' /= idNum) movies))
       else Left ("Movie with ID " ++ show idNum ++ " not found.")

-- Handle PrintMovies
stateTransition (State movies) (PrintMovies, _) =
    if null movies
    then Right (Just "No movies available.", State movies)
    else Right (Just ("Current movies: " ++ printMovies movies), State movies)

-- Handle CompoundQuery
stateTransition state (CompoundQuery q1 q2, a) =
    case stateTransition state (q1, a) of
        Right (msg1, newState) ->
            case stateTransition newState (q2, a) of
                Right (msg2, finalState) ->
                    let combinedMsg = unwords $ filter (not . null) [maybe "" Prelude.id msg1, maybe "" Prelude.id msg2]
                    in Right (Just combinedMsg, finalState)
                Left err -> Left err
        Left err -> Left err

-- Add movie to the state
addMovieTransition :: State -> String -> String -> Integer -> Integer -> Either String (Maybe String, State)
addMovieTransition (State newState) d t y i =
    let newMovie = addMovie newState (Movie d t y i)
    in Right (Just "Movie added", State newMovie)

-- Remove movie from the state
removeMovieTransition :: State -> Integer -> Either String (Maybe String, State)
removeMovieTransition (State newState) idNum =
    let oldMovie = removeMovie newState idNum
    in Right (Just "Movie removed", State oldMovie)

-- Print movies in the state
printMoviesTransition :: State -> Either String (Maybe String, State)
printMoviesTransition (State newState) =
    let printer = printMovies newState
    in Right (Just printer, State newState)

-- Helper to remove a movie from the list
removeMovie :: [Movie] -> Integer -> [Movie]
removeMovie [] _ = []
removeMovie (Movie dir t y idNum : movies) n
    | idNum == n = removeMovie movies n
    | otherwise = Movie dir t y idNum : removeMovie movies n

-- Helper to add a movie to the list
addMovie :: [Movie] -> Movie -> [Movie]
addMovie movies n = movies ++ [n]

-- Helper to print a movie
printMovie :: Movie -> String
printMovie (Movie d t y i) =
        "Director: " ++ d ++ ", Title: " ++ t ++
        ", Year: " ++ show y ++ ", Id: " ++ show i

-- Helper to print all movies
printMovies :: [Movie] -> String
printMovies [] = "No movies to display"
printMovies movies = unlines $ map printMovie movies

-- Sample movie list
movie1 :: Movie
movie1 = Movie "Pirmas" "Filmas#1" 1999 1

movie2 :: Movie
movie2 = Movie "Antras" "Filmas#2" 2000 2

movie3 :: Movie
movie3 = Movie "Trecias" "Filmas#3" 2004 3

movieList :: [Movie]
movieList = [movie1, movie2, movie3]