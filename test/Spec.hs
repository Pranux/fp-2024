{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [
    -- parseState
    testCase "Parse valid parseState command for multiple movies" $
      Lib2.parseState "/directorA/titleA/2022/101 /directorB/titleB/2023/102" @?= 
        Right (Lib2.State [Lib2.Movie {Lib2.director = "directorA", Lib2.title = "titleA", Lib2.year = 2022, Lib2.id = 101},
                          Lib2.Movie {Lib2.director = "directorB", Lib2.title = "titleB", Lib2.year = 2023, Lib2.id = 102}], ""),

    testCase "Parse invalid parseState command (missing director)" $
      Lib2.parseState "/-/title/2022/101" @?=
        Left "Invalid add-movie director",

    testCase "Parse invalid parseState command (missing title)" $
      Lib2.parseState "/director/-/2022/101" @?=
        Left "Invalid add-movie title",

    testCase "Parse invalid parseState command (missing year)" $
      Lib2.parseState "/director/title/a/101" @?=
        Left "Invalid add-movie year",

    testCase "Parse invalid parseState command (missing id)" $
      Lib2.parseState "/director/title/2022/a/" @?=
        Left "Invalid add-movie id",

    -- parseMovies
    testCase "Parse valid parseMovie command" $
      Lib2.parseMovie "/director/title/2022/1" @?=
        Right (Lib2.Movie {Lib2.director = "director", Lib2.title = "title", Lib2.year = 2022, Lib2.id = 1}, ""),

    testCase "Parse invalid parseMovie command" $
      Lib2.parseMovie "/error/director/title/2022/1" @?=
        Left "Invalid add-movie input format",

     -- parseQuery
    testCase "Parse valid add-movie command" $
      Lib2.parseQuery "add-movie /Tom/Showdown/2022/100" @?=
        Right (Lib2.AddMovie "Tom" "Showdown" 2022 100, ""),

    testCase "Parse valid add-movie command with extra spaces" $
      Lib2.parseQuery "add-movie       /Tom/Showdown/2022/100" @?=
        Right (Lib2.AddMovie "Tom" "Showdown" 2022 100, ""),
    
    testCase "Parse invalid add-movie command" $
      Lib2.parseQuery "add-movie /Error/Tom/Showdown/2022/100" @?=
        Left "Invalid add-movie input format",

    testCase "Parse valid remove-movie command" $
      Lib2.parseQuery "remove-movie 100" @?=
        Right (Lib2.RemoveMovie 100, ""),

    testCase "Parse invalid remove-movie command" $
      Lib2.parseQuery "remove-movie a100" @?=
        Left "Invalid remove-movie input format",

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

    testCase "Invalid compound-query format" $
      Lib2.parseQuery "compound-query add-movie /Tom/Showdown/2022/100 &" @?=
        Left "Invalid subqueries in compound-query",

    testCase "Invalid command type" $
      Lib2.parseQuery "add-moviex /Tom/Showdown/2022/100" @?=
        Left "Invalid command.",

    testCase "Empty input" $
      Lib2.parseQuery "" @?=
        Left "Invalid command.",

     -- stateTransition
    testCase "State transition with AddMovie" $
      let initialState = Lib2.emptyState
          query = Lib2.AddMovie "Tom" "Showdown" 2022 100
          expectedState = Lib2.State [Lib2.Movie "Tom" "Showdown" 2022 100]
      in Lib2.stateTransition initialState (query, "") @?=
        Right (Just "Added movie: Movie {director = \"Tom\", title = \"Showdown\", year = 2022, id = 100}", expectedState),

    testCase "State transition with RemoveMovie" $
      let initialState = Lib2.State [Lib2.Movie "Tom" "Showdown" 2022 100]
          query = Lib2.RemoveMovie 100
      in Lib2.stateTransition initialState (query, "") @?=
        Right (Just "Removed movie with ID: 100", Lib2.State []),

    testCase "State transition with PrintMovies" $
      let initialState = Lib2.State [Lib2.Movie "Tom" "Showdown" 2022 100]
          query = Lib2.PrintMovies
      in Lib2.stateTransition initialState (query, "") @?= Right (Just "Current movies: Director: Tom, Title: Showdown, Year: 2022, Id: 100\n", initialState),

    testCase "CompoundQuery with two valid queries" $
      let initialState = Lib2.emptyState
          query = Lib2.CompoundQuery (Lib2.AddMovie "Tom" "Showdown" 2022 100) Lib2.PrintMovies
          expectedState = Lib2.State [Lib2.Movie "Tom" "Showdown" 2022 100]
      in Lib2.stateTransition initialState (query, "") @?=
        Right (Just "Added movie: Movie {director = \"Tom\", title = \"Showdown\", year = 2022, id = 100} Current movies: Director: Tom, Title: Showdown, Year: 2022, Id: 100\n", expectedState),

    -- parseAddMovie
    testCase "Parse valid AddMovie input" $
      Lib2.parseAddMovie "/Tom/Showdown/2022/100" @?=
        Right (Lib2.AddMovie "Tom" "Showdown" 2022 100, ""),

    testCase "Parse invalid AddMovie input" $
      Lib2.parseAddMovie "/Tom/Showdown/invalid_year/100" @?=
        Left "Invalid add-movie year",

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
      Lib2.parseNum "100 word" @?= Right (100, " word"),

    testCase "Parse invalid number" $
      Lib2.parseNum "abc abc" @?= Left "Expected a number",

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
