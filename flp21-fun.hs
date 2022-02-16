-- G = (N, T, S, P)

module Main(main) where
    import System.Environment (getArgs)
    
    type Symbol = String
    type T = String
    type N = String
    data Rule = Rule N [Symbol] deriving (Eq, Show)
    data Set = Set N [N] deriving (Eq, Show)


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
    expandSet x [] = x
    expandSet (Set m ms) ((Set o os):xs) = if o `elem` ms
        then expandSet (Set m (mergeU ms os)) xs
        else expandSet (Set m ms) xs

    -- merge two lists into one with unique elements
    mergeU :: [Symbol] -> [Symbol] -> [Symbol]
    mergeU l [] = l
    mergeU l (x:xs) = if x `elem` l
        then mergeU l xs
        else mergeU (l ++ [x]) xs


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

    printOne :: ([N], [T], N, [Rule]) -> IO()
    printOne (n,t,s,r) = do
        let setsNext = sucSets n n r
        print (expandSets setsNext setsNext)
        putStrLn "TODO"

    printTwo :: ([N], [T], N, [Rule]) -> IO()
    printTwo (n,t,s,r) = do
        print "TODO!"

    -- TODO!! printGrammar bkg -> print in correct format

    main = do
        (option:fileName) <- getArgs
        
        contents <- if null fileName
            then getContents
            else readFile (head fileName)

        let bkg = parseInput (lines contents)
        if option == "-i" 
            then printInner bkg
        else if option == "-1"
            then printOne bkg
        else if option == "-2"
            then printTwo bkg
        else putStrLn "Error - TODO"
