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
     -- parseQuery
    testCase "Parse valid add-movie command" $
      Lib2.parseQuery "add-movie /Tom/Showdown/2022/100" @?=
        Right (Lib2.AddMovie "Tom" "Showdown" 2022 100, ""),

    testCase "Parse valid add-movie command with extra spaces" $
      Lib2.parseQuery "add-movie       /Tom/Showdown/2022/100" @?=
        Right (Lib2.AddMovie "Tom" "Showdown" 2022 100, ""),
    
    testCase "Parse invalid add-movie command" $
      Lib2.parseQuery "add-movie /Error/Tom/Showdown/2022/100" @?=
        Left "Invalid add-movie year",

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

    testCase "Parse invalid compound-query format" $
      Lib2.parseQuery "compound-query add-movie /Tom/Showdown/2022/100 &" @?=
        Left "Expected valid subquery after '&'",

    testCase "Parse invalid command type" $
      Lib2.parseQuery "add-moviex /Tom/Showdown/2022/100" @?=
        Left "Input does not start with a '/' separator",

    testCase "Parse empty input" $
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
      in Lib2.stateTransition initialState (query, "") @?= Right (Just "Current movies: Movie {director = \"Tom\", title = \"Showdown\", year = 2022, id = 100}\n", initialState),

    testCase "State transition with compoundQuery with two valid queries" $
      let initialState = Lib2.emptyState
          query = Lib2.CompoundQuery (Lib2.AddMovie "Tom" "Showdown" 2022 100) Lib2.PrintMovies
          expectedState = Lib2.State [Lib2.Movie "Tom" "Showdown" 2022 100]
      in Lib2.stateTransition initialState (query, "") @?=
        Right (Just "Added movie: Movie {director = \"Tom\", title = \"Showdown\", year = 2022, id = 100} Current movies: Movie {director = \"Tom\", title = \"Showdown\", year = 2022, id = 100}\n", expectedState),

    -- parseAddMovie
    testCase "Parse valid AddMovie input" $
      Lib2.parseAddMovie "add-movie /Tom/Showdown/2022/100" @?=
        Right (Lib2.AddMovie "Tom" "Showdown" 2022 100, ""),

    testCase "Parse invalid AddMovie input" $
      Lib2.parseAddMovie "add-movie /Tom/Showdown/invalid_year/100" @?=
        Left "Invalid add-movie year",

    -- parseRemoveMovie
    testCase "Parse valid RemoveMovie input" $
      Lib2.parseRemoveMovie "remove-movie 100" @?=
        Right (Lib2.RemoveMovie 100, ""),

    testCase "Parse invalid RemoveMovie input" $
      Lib2.parseRemoveMovie "remove-movie abc" @?=
        Left "Invalid remove-movie input format",

    -- parseCompoundQuery
    testCase "Parse valid CompoundQuery" $
      Lib2.parseCompoundQuery "compound-query add-movie /Tom/Showdown/2022/100 & print-movies" @?=
        Right (Lib2.CompoundQuery (Lib2.AddMovie "Tom" "Showdown" 2022 100) Lib2.PrintMovies, ""),

    testCase "Parse invalid CompoundQuery" $
      Lib2.parseCompoundQuery "compound-query invalid_query & print-movies" @?=
        Left "Error in first subquery: Invalid command.",

    -- parseNum
    testCase "Parse valid number" $
      Lib2.parseNum "100 word" @?= Right (100, " word"),

    testCase "Parse invalid number" $
      Lib2.parseNum "abc abc" @?= Left "Expected a number",
  ]
