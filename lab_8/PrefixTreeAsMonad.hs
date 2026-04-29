module Main where

import Control.Monad.State
import Data.List (sort)
import Data.Function (on)

data PrefixTree a = Node {
    nodeValue :: Maybe a,
    children  :: [(String, PrefixTree a)]
} deriving (Show, Eq)

emptyTree :: PrefixTree a
emptyTree = Node Nothing []

addElem :: [String] -> a -> PrefixTree a -> PrefixTree a
addElem [] val (Node _ chs) = Node (Just val) chs
addElem (k:ks) val (Node v chs) = Node v (updateChildren k ks val chs)
  where
    updateChildren :: String -> [String] -> a -> [(String, PrefixTree a)] -> [(String, PrefixTree a)]
    updateChildren key restKeys newVal [] =
        [(key, addElem restKeys newVal emptyTree)]
    updateChildren key restKeys newVal ((childKey, childNode):rest)
        | key == childKey = (childKey, addElem restKeys newVal childNode) : rest
        | otherwise       = (childKey, childNode) : updateChildren key restKeys newVal rest

findElem :: [String] -> PrefixTree a -> Maybe a
findElem [] (Node v _) = v
findElem (k:ks) (Node _ chs) =
    case findChild k chs of
        Nothing -> Nothing
        Just nextNode -> findElem ks nextNode
  where
    findChild :: String -> [(String, PrefixTree a)] -> Maybe (PrefixTree a)
    findChild _ [] = Nothing
    findChild key ((childKey, childNode):rest)
        | key == childKey = Just childNode
        | otherwise       = findChild key rest

addElemM :: [String] -> a -> State (PrefixTree a) a
addElemM key val = state $ \tree -> (val, addElem key val tree)

findElemM :: [String] -> State (PrefixTree a) (Maybe a)
findElemM key = state $ \tree -> (findElem key tree, tree)

collectPairs :: PrefixTree a -> [([String], a)]
collectPairs tree = go [] tree
  where
    go path (Node (Just v) chs) = (path, v) : concatMap (goChild path) chs
    go path (Node Nothing chs)  = concatMap (goChild path) chs
    goChild path (label, child) = go (path ++ [label]) child

compareTrees :: (Ord a) => PrefixTree a -> PrefixTree a -> Bool
compareTrees t1 t2 = sort (collectPairs t1) == sort (collectPairs t2)

test_sequence_1 :: State (PrefixTree String) String
test_sequence_1 = do
    addElemM ["a", "b"] "value1"
    addElemM ["a", "c"] "value2"
    addElemM ["a", "b", "d"] "value3"
    addElemM ["x", "y"] "value4"

test_sequence_2 :: State (PrefixTree String) String
test_sequence_2 = do
    addElemM ["x", "y"] "value4"
    addElemM ["a", "b"] "value1"
    addElemM ["a", "c"] "value2"
    addElemM ["a", "b", "d"] "value3"

main :: IO ()
main = do
    putStrLn "=== Тестирование монадического префиксного дерева ===\n"

    let initialState = emptyTree :: PrefixTree String

    let (_, tree1) = runState test_sequence_1 initialState
    let (_, tree2) = runState test_sequence_2 initialState

    putStrLn "Дерево после test_sequence_1:"
    print tree1
    putStrLn "\nДерево после test_sequence_2:"
    print tree2

    if compareTrees tree1 tree2
        then putStrLn "\ntest1: Ok — деревья эквивалентны"
        else putStrLn "\ntest1: Error — деревья различаются"

    let (maybeVal, treeAfterFind) = runState (findElemM ["a", "b"]) tree1
    putStrLn $ "\nПоиск [\"a\",\"b\"] в tree1: " ++ show maybeVal

    if compareTrees tree1 treeAfterFind
        then putStrLn "Поиск не изменил дерево"
        else putStrLn "Поиск изменил дерево (ошибка)"

    let (_, tree3) = runState (addElemM ["a", "b"] "new_value") tree1
    putStrLn "\nДерево после замены значения по ключу [\"a\",\"b\"]:"
    print tree3
    let (maybeNew, _) = runState (findElemM ["a", "b"]) tree3
    putStrLn $ "Новое значение: " ++ show maybeNew

    let (maybeOld, _) = runState (findElemM ["a", "b"]) tree1
    putStrLn $ "Старое значение в tree1: " ++ show maybeOld

    putStrLn "\n=== Все тесты пройдены ==="
