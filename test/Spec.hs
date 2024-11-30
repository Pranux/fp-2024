import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Lib2
import qualified Lib3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, unitTests', propertyTests]

propertyTests :: TestTree
propertyTests = testGroup "Lib3 property tests"
  [ QC.testProperty "Statements -> String -> State -> String -> Statement" $ withMaxSuccess 100 mainTest ]

mainTest :: Property
mainTest = forAll generateStatements $ \statements ->
  let initState = applyStatements statements Lib2.emptyState
      renderedString = Lib3.renderStatements statements
      parsedResult = Lib3.parseStatements renderedString
      newState = case parsedResult of
        Right (parsedStatements, _) -> applyStatements parsedStatements Lib2.emptyState
        Left err -> error $ "Parsing error: " ++ err
  in extractMovies initState === extractMovies newState

generateQuery :: Gen Lib2.Query
generateQuery = do
  director <- elements ["Tomas", "Jonas", "Petras", "Antanas", "Kristina", "Julija"]
  title <- elements ["Marsas", "Merkurijus", "Venera", "Uranas", "Jupiteris"]
  year <- choose (1924, 2024)
  id' <- choose (1, 1000)
  return $ Lib2.AddMovie director title year id'

generateStatements :: Gen Lib3.Statements
generateStatements = oneof [Lib3.Batch <$> listOf1 generateQuery, Lib3.Single <$> generateQuery]

extractMovies :: Lib2.State -> [Lib2.Movie]
extractMovies (Lib2.State movies) = movies

applyStatements :: Lib3.Statements -> Lib2.State -> Lib2.State

applyStatements (Lib3.Batch queries) state = applyQueries queries state
  where
    applyQueries [] currentState = currentState
    applyQueries (query:rest) currentState =
      case Lib2.stateTransition currentState (query, "") of
        Right (_, newState) -> applyQueries rest newState
        Left err -> error $ "Query failed: " ++ err

applyStatements (Lib3.Single query) state =
  case Lib2.stateTransition state (query, "") of
    Right (_, newState) -> newState
    Left err -> error $ "Query failed: " ++ err


unitTests' :: TestTree
unitTests' = testGroup "Lib3 tests"
  [
    -- parseCommand
    testCase "Parse LOAD command" $
      Lib3.parseCommand "LOAD" @?=
        Right (Lib3.LoadCommand, ""),

    testCase "Parse SAVE command" $
      Lib3.parseCommand "SAVE" @?=
        Right (Lib3.SaveCommand, ""),

    testCase "Parse batch query statements" $
      Lib3.parseCommand "BEGIN add-movie /Tom/Showdown/2022/100; remove-movie 100 END" @?=
        Right (Lib3.StatementCommand (Lib3.Batch [ Lib2.AddMovie "Tom" "Showdown" 2022 100 , Lib2.RemoveMovie 100 ]), ""),

    testCase "Parse batch query statements without ';'" $
      Lib3.parseCommand "BEGIN add-movie /Tom/Showdown/2022/100 remove-movie 100 END" @?=
        Right (Lib3.StatementCommand (Lib3.Batch [ Lib2.AddMovie "Tom" "Showdown" 2022 100 ]), ""),

    testCase "Parse single query command" $
      Lib3.parseCommand "add-movie /Tom/Showdown/2022/100" @?=
        Right (Lib3.StatementCommand (Lib3.Single (Lib2.AddMovie "Tom" "Showdown" 2022 100)), ""),

    -- parseStatements
    testCase "Parse empty BEGIN-END block" $
      Lib3.parseStatements "BEGIN END" @?=
        Right (Lib3.Batch [], ""),

    testCase "Parse multiple statements with whitespace" $
      Lib3.parseStatements "BEGIN   add-movie /Tom/Showdown/2022/100 ; remove-movie 100   END" @?=
        Right (Lib3.Batch [ Lib2.AddMovie "Tom" "Showdown" 2022 100, Lib2.RemoveMovie 100 ], ""),

    testCase "Reject invalid BEGIN-END block" $
      Lib3.parseStatements "BEGIN add-movie /Tom/Showdown/2022/100" @?=
        Left "Expected 'END'",

    -- marshallState
    testCase "Marshall empty state" $
      Lib3.marshallState (Lib2.State []) @?=
        Lib3.Single Lib2.PrintMovies,

    testCase "Marshall single movie state" $
      Lib3.marshallState (Lib2.State [Lib2.Movie "Tom" "Showdown" 2022 100 ]) @?=
        Lib3.Single (Lib2.AddMovie "Tom" "Showdown" 2022 100),

    testCase "Marshall multiple movies state" $
      Lib3.marshallState (Lib2.State [ Lib2.Movie "Tom" "Showdown" 2022 100, Lib2.Movie "Jane" "Blockbuster" 2023 101 ]) @?=
        Lib3.Batch [ Lib2.AddMovie "Tom" "Showdown" 2022 100, Lib2.AddMovie "Jane" "Blockbuster" 2023 101 ],

    -- renderStatements
    testCase "Render single query" $
      Lib3.renderStatements (Lib3.Single (Lib2.AddMovie "Tom" "Showdown" 2022 100)) @?=
        "BEGIN add-movie /Tom/Showdown/2022/100 END",

    testCase "Render batch queries" $
      Lib3.renderStatements (Lib3.Batch [ Lib2.AddMovie "Tom" "Showdown" 2022 100, Lib2.RemoveMovie 100 ]) @?=
        "BEGIN add-movie /Tom/Showdown/2022/100; remove-movie 100; END"
  ]


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
      Lib2.parseNum "abc abc" @?= Left "Expected a number"
  ]
