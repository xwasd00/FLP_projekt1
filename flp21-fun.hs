-- G = (N, T, S, P)
-- TODO: sjednoceni komentaru (CZ nebo EN) + doplneni komentaru nekde
module Main(main) where
    import System.Environment (getArgs)
    
    type Symbol = String
    type T = String
    type N = String
    data Rule = Rule {leftSide :: N, rightSide :: [Symbol]} deriving (Eq, Show)
    data Set = Set {name :: N, elements :: [N]} deriving (Eq, Show)


    parseInput :: [String] -> ([N], [T], N, [Rule])
    parseInput (n:t:s:rules) = (parseN n, parseT t, s, getRules rules)
    -- get list of nonterminal symbols
    parseN :: String -> [N]
    parseN n = [[x] | x <- n, x /= ',']
    -- get list of terminal symbols
    parseT :: String -> [T]
    parseT t = [[x] | x <- t, x /= ',']
    -- get list of rules
    getRules :: [String] -> [Rule]
    getRules [] = []
    getRules (x:xs) = getRule x : getRules xs
    -- get rule from string 
    -- "A->ac" => Rule "A" ["a", "c"]
    getRule :: String -> Rule
    getRule (x:'-':'>':xs) = Rule [x] [[s] | s <- xs]
    

    -- vytvoreni mnozin pro jeden krok (A -> B) => N_A = {A, B}
    -- args: mnozina nezpracovanych neterminalu, mnozina vsech terminalu, mnozina pravidel
    sucSets :: [N] -> [N] -> [Rule] -> [Set]
    sucSets [] _ _ = []
    sucSets (n:ns) nn r = set : (sucSets ns nn r)
        where set = Set n (n:(sucSet n nn r))

    -- vytvoreni mnoziny N_x, kde x je neterminal NEobsazen v N_x po jednom kroku
    sucSet :: N -> [N] -> [Rule] -> [N]
    sucSet x nn r = [n | n <- nn, rule <- r, x /= n, Rule x [n] == rule]

    -- rozsireni mnozin pro vice kroku (A ->* C) => N_A = {A, B} -> N_A = {A, B, C}
    expandSets :: [Set] -> [Set] -> [Set]
    expandSets [] _ = []
    expandSets (x:xs) allSets = expandIf x allSets : expandSets xs allSets

    -- same set -> cannot be expanded 'return'
    -- exanded set -> call again expandIf
    expandIf :: Set -> [Set] -> Set
    expandIf old allSets = if new == old 
        then old
        else expandIf new allSets
        where new = expandSet old allSets

    -- Set "A" "AB" [Set "B" "BC"] -> Set "A" "ABC"
    expandSet :: Set -> [Set] -> Set
    expandSet m [] = m
    expandSet m (x:xs) = if (name x) `elem` (elements m)
        then expandSet (Set (name m) newE) xs
        else expandSet m xs
        where newE = mergeU (elements m) (elements x)

    -- merge two lists into one with unique elements
    mergeU :: [Symbol] -> [Symbol] -> [Symbol]
    mergeU l [] = l
    mergeU l (x:xs) = if x `elem` l
        then mergeU l xs
        else mergeU (l ++ [x]) xs


    recreateRules :: [Set] -> [Rule] -> [Rule]
    recreateRules [] _ = []
    recreateRules (s:ss) r = (getRulesBySet s r) ++ recreateRules ss r

    getRulesBySet :: Set -> [Rule] -> [Rule]
    getRulesBySet s [] = []
    getRulesBySet s (r:rs) = if leftSide r `elem` elements s && not easyRule
        then (Rule (name s) (rightSide r)) : (getRulesBySet s rs)
        else getRulesBySet s rs
        where easyRule = length (rightSide r) == 1 && (head (rightSide r)) `elem` elements s


    printInner :: ([N], [T], N, [Rule]) -> IO()
    printInner (n,t,s,r) = do
        putStr "Nonterminals: "
        print n
        putStr "Terminals: "
        print t
        putStr "Start: "
        print s
        putStr "Rules: "
        print r

    printAlg1 :: ([N], [T], N, [Rule]) -> IO()
    printAlg1 (n,t,s,r) = do
        let setsNext = sucSets n n r
        let sets = expandSets setsNext setsNext
        let newRules = recreateRules sets r
        --putStrLn "Sets:"
        --print sets
        --putStrLn "New Rules:"
        --print newRules
        printGrammar (n,t,s,newRules)

    printAlg2 :: ([N], [T], N, [Rule]) -> IO()
    printAlg2 (n,t,s,r) = do
        print "TODO!!!"
        printGrammar (n,t,s,r)

    -- print grammar to stdout
    printGrammar :: ([N], [T], N, [Rule]) -> IO()
    printGrammar (n,t,s,r) = do
        printList n
        putStrLn ""
        printList t
        putStrLn ""
        putStrLn s
        printRules r

    printList :: [String] -> IO()
    printList [] = putStr ""
    printList [x] = putStr x
    printList (x:xs) = do
        putStr x
        putStr ","
        printList xs

    printRules :: [Rule] -> IO()
    printRules [] = putStr ""
    printRules (r:rs) = do
        putStr (leftSide r)
        putStr "->"
        putStrLn (concatList (rightSide r))
        printRules rs

    concatList :: [String] -> String
    concatList = foldl (++) ""
        
    main = do
        (option:fileName) <- getArgs
        
        contents <- if null fileName
            then getContents
            else readFile (head fileName)

        let bkg = parseInput (lines contents)
        if option == "-i" 
            then printInner bkg
        else if option == "-1"
            then printAlg1 bkg
        else if option == "-2"
            then printAlg2 bkg
        else putStrLn "Wrong option, possible options are '-i', '-1' or '-2'"
