{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
--import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

-- unitTests :: TestTree
-- unitTests = testGroup "Lib1 tests"
--   [ 
--     testCase "List of completions is not empty" $
--       null Lib1.completions @?= False,
--     testCase "Parsing case 1 - give a better name" $
--       Lib2.parseQuery "" @?= Left "Invalid command.",
--     testCase "Parsing case 2 - give a better name" $
--       Lib2.parseQuery "o" @?= Left "Invalid command."
--   ]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ 
     -- Tests for parseQuery
    testCase "Parse valid add-movie command" $
      Lib2.parseQuery "add-movie /Tom/Showdown/2022/100" @?=
        Right (Lib2.AddMovie "Tom" "Showdown" 2022 100, ""),

    testCase "Parse valid add-movie command with extra spaces" $
      Lib2.parseQuery "add-movie       /Tom/Showdown/2022/100" @?=
        Right (Lib2.AddMovie "Tom" "Showdown" 2022 100, ""),

    testCase "Parse valid remove-movie command" $
      Lib2.parseQuery "remove-movie 100" @?=
        Right (Lib2.RemoveMovie 100, ""),

    testCase "Parse print-movies command" $
      Lib2.parseQuery "print-movies" @?=
        Right (Lib2.PrintMovies, ""),

    testCase "Parse valid compound-query command" $
      Lib2.parseQuery "compound-query add-movie /Tom/Showdown/2022/100 & remove-movie 100" @?=
        Right (Lib2.CompoundQuery
                (Lib2.AddMovie "Tom" "Showdown" 2022 100)
                (Lib2.RemoveMovie 100), ""),

    testCase "Parse compound-query with spaces" $
      Lib2.parseQuery "compound-query  add-movie /Tom/Showdown/2022/100  &  remove-movie 100" @?=
        Right (Lib2.CompoundQuery
                (Lib2.AddMovie "Tom" "Showdown" 2022 100)
                (Lib2.RemoveMovie 100), ""),

    testCase "Invalid command type" $
      Lib2.parseQuery "add-moviex /Tom/Showdown/2022/100" @?=
        Left "Invalid command.",

    testCase "Invalid add-movie format" $
      Lib2.parseQuery "add-movie /Tom/Showdown/2022" @?=
        Left "Invalid add-movie input format",

    testCase "Invalid remove-movie format" $
      Lib2.parseQuery "remove-movie notANumber" @?=
        Left "Invalid remove-movie input format",

    testCase "Invalid compound-query format" $
      Lib2.parseQuery "compound-query add-movie /Tom/Showdown/2022/100 &" @?=
        Left "Invalid compound-query format",

    testCase "Empty input" $
      Lib2.parseQuery "" @?=
        Left "Invalid command.",

    -- stateTransition
    testCase "State transition with AddMovie" $
      let initialState = Lib2.emptyState
          query = Lib2.AddMovie "Tom" "Showdown" 2022 100
          expectedState = Lib2.State [Lib2.Movie "Tom" "Showdown" 2022 100]
      in stateTransition initialState (query, "") @?=
        Right (Just "Added movie: Movie {director = \"Tom\", title = \"Showdown\", year = 2022, id = 100}", expectedState),

    testCase "State transition with RemoveMovie" $
      let initialState = Lib2.State [Lib2.Movie "Tom" "Showdown" 2022 100]
          query = Lib2.RemoveMovie 100
      in stateTransition initialState (query, "") @?= Right (Just "Removed movie with ID: 100", Lib2.State [])

    testCase "State transition with PrintMovies" $
      let initialState = Lib2.State [Lib2.Movie "Tom" "Showdown" 2022 100]
          query = Lib2.PrintMovies
      in stateTransition initialState (query, "") @?= Right (Just "Current movies: Director: Tom, Title: Showdown, Year: 2022, Id: 100\n", initialState),

    testCase "CompoundQuery with two valid queries" $
      let initialState = Lib2.emptyState
          query = Lib2.CompoundQuery (Lib2.AddMovie "Tom" "Showdown" 2022 100) Lib2.PrintMovies
          expectedState = Lib2.State [Lib2.Movie "Tom" "Showdown" 2022 100]
      in stateTransition initialState (query, "") @?= Right (Just "Added movie: Movie {director = \"Tom\", title = \"Showdown\", year = 2022, id = 100} Current movies: Director: Tom, Title: Showdown, Year: 2022, Id: 100\n"), expectedState,

    -- parseAddMovie
    testCase "Parse valid AddMovie input" $
      Lib2.parseAddMovie "/Tom/Showdown/2022/100" @?= 
        Right (Lib2.AddMovie "Tom" "Showdown" 2022 100, ""),

    testCase "Parse invalid AddMovie input" $
      Lib2.parseAddMovie "/Tom/Showdown/invalid_year/100" @?= 
        Left "Invalid year or ID in add-movie",

    -- parseRemoveMovie
    testCase "Parse valid RemoveMovie input" $
      Lib2.parseRemoveMovie "100" @?= 
        Right (Lib2.RemoveMovie 100, ""),

    testCase "Parse invalid RemoveMovie input" $
      Lib2.parseRemoveMovie "abc" @?= 
        Left "Invalid remove-movie input format",

    -- parseCompoundQuery
    testCase "Parse valid CompoundQuery" $
      Lib2.parseCompoundQuery "add-movie /Tom/Showdown/2022/100 & print-movies" @?= 
        Right (Lib2.CompoundQuery (Lib2.AddMovie "Tom" "Showdown" 2022 100) Lib2.PrintMovies, ""),

    testCase "Parse invalid CompoundQuery" $
      Lib2.parseCompoundQuery "invalid_query & print-movies" @?= 
        Left "Invalid subqueries in compound-query",

    -- parseNum
    testCase "Parse valid number" $
      Lib2.parseNum "100 more" @?= Right (100, " more"),

    testCase "Parse invalid number" $
      Lib2.parseNum "abc more" @?= Left "Expected a number",

    -- wordsBy
    testCase "WordsBy delimiter test" $
      Lib2.wordsBy ('/' ==) "Hello/World/Example" @?= ["Hello", "World", "Example"],

    testCase "WordsBy no delimiter test" $
      Lib2.wordsBy ('/' ==) "NoDelimitersHere" @?= ["NoDelimitersHere"],

    -- addMovie
    testCase "Add a movie to an empty list" $
      let movies = []
          newMovie = Lib2.Movie "Tom" "Showdown" 2022 100
          updatedMovies = Lib2.addMovie movies newMovie
      in updatedMovies @?= [newMovie],

    testCase "Add a movie to a non-empty list" $
      let movies = [Lib2.Movie "Tom" "Showdown" 2022 100]
          newMovie = Lib2.Movie "Jane" "Epic" 2023 101
          updatedMovies = Lib2.addMovie movies newMovie
      in updatedMovies @?= [Lib2.Movie "Tom" "Showdown" 2022 100, newMovie],

    -- removeMovie
    testCase "Remove a movie that exists" $
      let movies = [Lib2.Movie "Tom" "Showdown" 2022 100]
          updatedMovies = Lib2.removeMovie movies 100
      in updatedMovies @?= [],

    testCase "Remove a movie that does not exist" $
      let movies = [Lib2.Movie "Tom" "Showdown" 2022 100]
          updatedMovies = Lib2.removeMovie movies 999
      in updatedMovies @?= [Lib2.Movie "Tom" "Showdown" 2022 100], 

    -- printMovie
    testCase "Print a movie" $
      let movie = Lib2.Movie "Tom" "Showdown" 2022 100
      in Lib2.printMovie movie @?= "Director: Tom, Title: Showdown, Year: 2022, Id: 100",

    -- printMovies
    testCase "Print movies" $
      let movies = [Lib2.Movie "Tom" "Showdown" 2022 100, Lib2.Movie "Jane" "Epic" 2023 101]
      in Lib2.printMovies movies @?= "Director: Tom, Title: Showdown, Year: 2022, Id: 100\nDirector: Jane, Title: Epic, Year: 2023, Id: 101\n"
  ]
