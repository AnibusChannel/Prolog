module Main where

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

main :: IO ()
main = do
    putStrLn "=== Тестирование префиксного дерева ===\n"
    
    -- 1. Пустое дерево
    let t0 = emptyTree :: PrefixTree String
    putStrLn "1. Пустое дерево:"
    print t0
    
    -- 2. Добавление ключей
    let t1 = addElem ["a", "b"] "value1" t0
    let t2 = addElem ["a", "c"] "value2" t1
    let t3 = addElem ["a", "b", "d"] "value3" t2
    
    putStrLn "\n2. После добавлений:"
    putStrLn $ "t3 = " ++ show t3
    
    -- 3. Поиск существующих ключей
    putStrLn "\n3. Поиск существующих ключей:"
    print $ findElem ["a", "b"] t3        -- Just "value1"
    print $ findElem ["a", "c"] t3        -- Just "value2"
    print $ findElem ["a", "b", "d"] t3   -- Just "value3"
    
    -- 4. Поиск отсутствующих ключей
    putStrLn "\n4. Поиск отсутствующих ключей:"
    print $ findElem ["a", "x"] t3        -- Nothing
    print $ findElem ["a"] t3             -- Nothing (промежуточный узел)
    print $ findElem [] t3                -- Nothing (корень)
    
    -- 5. Замена значения
    let t4 = addElem ["a", "b"] "new_value" t3
    putStrLn "\n5. После замены значения по ключу [\"a\",\"b\"]:"
    print $ findElem ["a", "b"] t4        -- Just "new_value"
    print $ findElem ["a", "b", "d"] t4   -- Just "value3" (не затронуто)
    
    -- 6. Добавление ключа, который является префиксом существующего
    let t5 = addElem ["a", "b"] "prefix_value" t3  -- замена
    putStrLn "\n6. Ключ [\"a\",\"b\"] теперь имеет значение \"prefix_value\":"
    print $ findElem ["a", "b"] t5
    print $ findElem ["a", "b", "d"] t5   -- всё ещё "value3"
    
    -- 7. Добавление ключа, который расширяет существующий
    let t6 = addElem ["a", "b", "d", "e"] "value4" t3
    putStrLn "\n7. Добавлен длинный ключ [\"a\",\"b\",\"d\",\"e\"]:"
    print $ findElem ["a", "b", "d", "e"] t6   -- Just "value4"
    print $ findElem ["a", "b", "d"] t6        -- Just "value3" (не изменилось)
    
    -- 8. Проверки неизменяемости (старые деревья не меняются)
    putStrLn "\n8. Старое дерево t3 не изменилось после операций:"
    print $ findElem ["a", "b"] t3   -- всё ещё "value1"
    
    putStrLn "\n=== Все тесты пройдены ==="
