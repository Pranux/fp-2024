module Lib2
    ( Query(..),
      State(..),
      Movie(..),
      or2,
      emptyState,
      stateTransition,
      parseString,
      parseNum,
      parseWhitespaces,
      parseQuery,
      parseAddMovie,
      parseRemoveMovie,
      parsePrintMovies,
      parseCompoundQuery
    ) where

import Data.Char as C (isDigit, isAlpha)
import Data.List (isPrefixOf)

-- main data type
data Movie = Movie {
    director :: String,
    title :: String,
    year :: Integer,
    id :: Integer
    } deriving (Show, Eq)

-- query type
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

instance Eq State where
  (State movies1) == (State movies2) = movies1 == movies2

instance Show Query where
    show (AddMovie d t y i) =
        "AddMovie: Director = " ++ d ++ ", Title = " ++ t ++ ", Year = " ++ show y ++ ", ID = " ++ show i
    show (RemoveMovie idNum) =
        "RemoveMovie: ID = " ++ show idNum
    show PrintMovies = "Current movies: "
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
or2 a b input =
    case a input of
        Right c -> Right c
        Left "" -> 
            case b input of
                Right c -> Right c
                Left err -> Left err
        Left err -> Left err

-- Parser for a whole query
parseQuery :: Parser Query
parseQuery input = case (parseAddMovie `or2` parseRemoveMovie `or2` parsePrintMovies `or2` parseCompoundQuery) input of
    Left "" -> Left "Invalid command."
    Left msg -> Left msg
    query -> query    

-- Parser for AddMovie
parseAddMovie :: Parser Query
parseAddMovie input = 
    if "add-movie" `isPrefixOf` input then
        let r = drop (length "add-movie") input
        in case parseWhitespaces r of
            Left _ -> Left "Invalid command"
            Right (_, r0) ->
                case r0 of
                    ('/' : r1) -> 
                        case parseString r1 of
                            Left _ -> Left "Invalid add-movie director"
                            Right (d, r2) ->  
                                case r2 of
                                    ('/' : r3) -> 
                                        case parseString r3 of
                                            Left _ -> Left "Invalid add-movie title"
                                            Right (t, r4) ->  
                                                case r4 of
                                                    ('/' : r5) -> 
                                                        case parseNum r5 of
                                                            Left _ -> Left "Invalid add-movie year"
                                                            Right (y, r6) ->  
                                                                case r6 of
                                                                    ('/' : r7) -> 
                                                                        case parseNum r7 of
                                                                            Left _ -> Left "Invalid add-movie id"
                                                                            Right (i, rest) -> Right (AddMovie d t y i, rest)
                                                    _ -> Left "Input does not start with a '/' separator after title"
                                    _ -> Left "Input does not start with a '/' separator after director"
                    _ -> Left "Input does not start with a '/' separator"
    else Left ""
        
-- Parser for remove-movie
parseRemoveMovie :: Parser Query
parseRemoveMovie input =
    if "remove-movie" `isPrefixOf` input then
        let r = drop (length "remove-movie") input
        in case parseWhitespaces r of
            Left _ -> Left "Invalid command"
            Right (_, r1) ->
                case parseNum r1 of
                    Right (idNum, rest) -> Right (RemoveMovie idNum, rest)
                    _ -> Left "Invalid remove-movie input format"
    else Left ""

-- Parser for print-movies
parsePrintMovies :: Parser Query
parsePrintMovies input =
    if "print-movies" `isPrefixOf` input then
        let r = drop (length "print-movies") input
        in case parseWhitespaces r of
            Left _ -> Right (PrintMovies, "")
            Right (_, rest) -> Right (PrintMovies, rest)
    else Left ""

-- Parser for compound-query
parseCompoundQuery :: Parser Query
parseCompoundQuery input =
    if "compound-query" `isPrefixOf` input then
        let r0 = drop (length "compound-query") input
        in case parseWhitespaces r0 of
            Left _ -> Left "Expected whitespace after 'compound-query'"
            Right (_, r1) ->
                let (q1Str, r) = break (== '&') r1
                in case r of
                    '&' : rAfterAmpersand ->
                        case parseWhitespaces rAfterAmpersand of
                            Left _ -> Left "Expected valid subquery after '&'"
                            Right (_, q2Str) ->
                                case parseQuery q1Str of
                                    Right (q1, _) ->
                                        case parseQuery q2Str of
                                            Right (q2, rest) -> Right (CompoundQuery q1 q2, rest)
                                            Left err2 -> Left $ "Error in second subquery: " ++ err2
                                    Left err1 -> Left $ "Error in first subquery: " ++ err1
                    _ -> Left "Invalid compound-query format: '&' separator missing"
    else Left ""

-- Helper function to parse a alpahbetic value
parseString :: Parser String
parseString [] = Left "Empty input"
parseString input =
    let
        chars = takeWhile isAlpha input
        rest = drop (length chars) input
    in
        case chars of
            [] -> Left "Expected a char"
            _ -> Right (chars, rest)

-- Helper function to parse a number
parseNum :: Parser Integer
parseNum [] = Left "Empty input"
parseNum input =
    let
        digits = takeWhile isDigit input
        rest = drop (length digits) input
    in
        case digits of
            [] -> Left "Expected a number"
            _ -> Right (read digits, rest)

-- Helper function to parse a whitespace
parseWhitespaces :: Parser String
parseWhitespaces [] = Left "Empty input"
parseWhitespaces str =
  let
    whitespaces = takeWhile (== ' ') str
    rest = drop (length whitespaces) str
  in 
    Right (whitespaces, rest)

-- stateTransition definition.
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
    else
        let movieList = unlines (map show movies)
        in Right (Just ("Current movies: " ++ movieList), State movies)

-- Handle CompoundQuery
stateTransition state (CompoundQuery q1 q2, a) =
    case stateTransition state (q1, a) of
        Right (msg1, newState) ->
            case stateTransition newState (q2, a) of
                Right (msg2, finalState) ->
                    let combinedMsg = case (msg1, msg2) of
                                          (Just m1, Just m2) | not (null m1) && not (null m2) -> Just (m1 ++ " " ++ m2)
                                          (Just m1, _)       | not (null m1) -> Just m1
                                          (_, Just m2)       | not (null m2) -> Just m2
                                          _ -> Nothing
                    in Right (combinedMsg, finalState)
                Left err -> Left err
        Left err -> Left err