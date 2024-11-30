module Lib3
    ( stateTransition,
    Statements (..),
    Command (..),
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
    ) where

import Control.Concurrent.Chan (Chan, readChan, writeChan, newChan)
import Control.Concurrent.STM (TVar, atomically, writeTVar, readTVarIO)
import System.IO (withFile, IOMode(..), hPutStr, hGetContents, openFile)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Lib2
import Data.Either (partitionEithers)

data StorageOp = Save String (Chan ()) | Load (Chan String)

storageFilePath :: FilePath
storageFilePath = "storage.txt"

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
    op <- readChan chan
    case op of
        Save content responseChan -> do
            withFile storageFilePath WriteMode $ \handle -> do
                hPutStr handle content
            writeChan responseChan ()
            storageOpLoop chan

        Load responseChan -> do
            fileHandle <- openFile storageFilePath ReadMode
            content <- hGetContents fileHandle
            writeChan responseChan content
            storageOpLoop chan

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
-- parseCommand :: String -> Either String (Command, String)
-- parseCommand input = 
--     let cleanInput = processMultilineInput input
--     in case parseLoad cleanInput of
--         Right result -> Right result
--         Left "" ->
--             case parseSave cleanInput of
--                 Right result -> Right result
--                 Left "" ->
--                     case parseWrappedStatements cleanInput of
--                         Right result -> Right result
--                         Left err -> Left err
--                 Left err -> Left err
--         Left err -> Left err

parseCommand :: String -> Either String (Command, String)
parseCommand input
  | "LOAD" `isPrefixOf` input = Right (LoadCommand, drop (length "LOAD") input)
  | "SAVE" `isPrefixOf` input = Right (SaveCommand, drop (length "SAVE") input)
  | "BEGIN" `isPrefixOf` input = case parseStatements input of
        Left err -> Left err
        Right (s, r) -> Right (StatementCommand s, r)
  | otherwise = case Lib2.parseQuery input of
        Left err -> Left err
        Right (q, _) -> Right (StatementCommand (Single q), "")

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
-- | Parses Statement.
parseStatements :: String -> Either String (Statements, String)
parseStatements input = 
    let input' = trimWhitespace input in
    if "BEGIN" `isPrefixOf` input'
        then if "END" `isSuffixOf` input'
            then let statements = trimWhitespace $ drop (length "BEGIN") $ take (length input' - length "END") input' in
                if null statements
                    then Right (Batch [], "")
                else 
                    let queries = map (Lib2.parseQuery . trimWhitespace) (filter (not . null) $ splitOn ';' statements)
                        (err, pQueries) = partitionEithers queries
                    in if null err
                        then Right (Batch (map fst pQueries), "")
                        else Left $ "Error parsing queries: " ++ show err
                else Left "Expected 'END'"
    else Left "Expected 'BEGIN' and 'END'"

-- Helper function to split string by delimiter
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimiter (c:cs)
    | c == delimiter = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
    rest = splitOn delimiter cs

-- Helper function to trim whitespace
trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State []) = Single Lib2.PrintMovies
marshallState (Lib2.State movies) = 
    -- Convert each movie into an AddMovie query
    let queries = map (\m -> Lib2.AddMovie (Lib2.director m) (Lib2.title m) (Lib2.year m) (Lib2.id m)) movies
    in if length queries == 1
       then Single (head queries)
       else Batch queries

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements statements = case statements of
    Single query -> "BEGIN " ++ renderSingleQuery query ++ " END"
    Batch queries -> "BEGIN " ++ concatMap (\q -> renderSingleQuery q ++ "; ") queries ++ "END"
  where
    renderSingleQuery :: Lib2.Query -> String
    renderSingleQuery query = case query of
        Lib2.AddMovie dir title year id' -> "add-movie /" ++ dir ++ "/" ++ title ++ "/" ++ show year ++ "/" ++ show id'
        Lib2.RemoveMovie id' -> "remove-movie " ++ show id'
        Lib2.RemoveAllMovies -> "remove-all-movies"
        Lib2.PrintMovies -> "print-movies"
        Lib2.CompoundQuery q1 q2 -> "compound-query " ++ renderSingleQuery q1 ++ " & " ++ renderSingleQuery q2

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition stateVar command ioChan = do
    currentState <- readTVarIO stateVar
    
    let executeQueries :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
        executeQueries state [] = Right (Nothing, state)
        executeQueries state (query:rest) = do
            case Lib2.stateTransition state (query, "") of
                Right (msg1, newState) -> 
                    case executeQueries newState rest of
                        Right (msg2, finalState) -> 
                            let combinedMsg = case (msg1, msg2) of
                                    (Just m1, Just m2) -> Just (m1 ++ "\n" ++ m2)
                                    (Just m1, Nothing) -> Just m1
                                    (Nothing, Just m2) -> Just m2
                                    (Nothing, Nothing) -> Nothing
                            in Right (combinedMsg, finalState)
                        Left err -> Left err
                Left err -> Left err

    case command of

        LoadCommand -> do
            responseChan <- newChan
            writeChan ioChan (Load responseChan)
            content <- readChan responseChan
            case parseStatements content of
                Right (statements, _) -> 
                    case statements of
                        Single query ->
                            case Lib2.stateTransition Lib2.emptyState (query, "") of
                                Right (_, newState) -> do
                                    atomically $ writeTVar stateVar newState
                                    return $ Right $ Just "State loaded successfully"
                                Left err -> return $ Left $ "Failed to load state: " ++ err
                        Batch queries ->
                            case executeQueries Lib2.emptyState queries of
                                Right (_, newState) -> do
                                    atomically $ writeTVar stateVar newState
                                    return $ Right $ Just "State loaded successfully"
                                Left err -> return $ Left $ "Failed to load state: " ++ err
                Left err -> return $ Left $ "Failed to load state: " ++ err
        
        SaveCommand -> do
            let content = renderStatements $ marshallState currentState
            responseChan <- newChan
            writeChan ioChan (Save content responseChan)
            _ <- readChan responseChan
            return $ Right $ Just "State saved successfully"

        StatementCommand statements -> 
            case statements of
                Single query -> do
                    case Lib2.stateTransition currentState (query, "") of
                        Right (message, newState) -> do
                            atomically $ writeTVar stateVar newState
                            return $ Right message
                        Left err -> return $ Left err
                
                Batch queries -> do
                    case executeQueries currentState queries of
                        Right (message, newState) -> do
                            atomically $ writeTVar stateVar newState
                            return $ Right message
                        Left err -> return $ Left err
                        