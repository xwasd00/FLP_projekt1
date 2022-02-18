-- G = (N, T, S, P)
-- TODO: sjednoceni komentaru (CZ nebo EN) + doplneni komentaru
-- TODO: testy
module Main(main) where
    import System.Environment (getArgs)
    
    type Symbol = String
    type T = String
    type N = String
    data Rule = Rule {leftSide :: N, rightSide :: [Symbol]} deriving (Eq, Show)
    data Set = Set {name :: N, elements :: [N]} deriving (Eq, Show)

    ------------------------------------------------------------------------------
    ------------------------------ PARSING ---------------------------------------
    ------------------------------------------------------------------------------
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
    
    ------------------------------------------------------------------------------
    ------------------------------- ALG1 -----------------------------------------
    ------------------------------------------------------------------------------
    -- vytvoreni mnozin pro jeden krok (A -> B) => N_A = {A, B}
    -- args: mnozina nezpracovanych neterminalu, mnozina vsech terminalu, mnozina pravidel
    alg1SucSets :: [N] -> [N] -> [Rule] -> [Set]
    alg1SucSets [] _ _ = []
    alg1SucSets (n:ns) nn r = set : (alg1SucSets ns nn r)
        where set = Set n (n:(alg1SucSet n nn r))

    -- vytvoreni mnoziny N_x, kde x je neterminal NEobsazen v N_x po jednom kroku
    alg1SucSet :: N -> [N] -> [Rule] -> [N]
    alg1SucSet x nn r = [n | n <- nn, rule <- r, x /= n, Rule x [n] == rule]

    -- rozsireni mnozin pro vice kroku (A ->* C) => N_A = {A, B} -> N_A = {A, B, C}
    alg1ExpandSets :: [Set] -> [Set] -> [Set]
    alg1ExpandSets [] _ = []
    alg1ExpandSets (x:xs) allSets = alg1ExpandIf x allSets : alg1ExpandSets xs allSets

    -- same set -> cannot be expanded 'return'
    -- exanded set -> call again alg1ExpandIf
    alg1ExpandIf :: Set -> [Set] -> Set
    alg1ExpandIf old allSets = if new == old 
        then old
        else alg1ExpandIf new allSets
        where new = alg1ExpandSet old allSets

    -- Set "A" "AB" [Set "B" "BC"] -> Set "A" "ABC"
    alg1ExpandSet :: Set -> [Set] -> Set
    alg1ExpandSet m [] = m
    alg1ExpandSet m (x:xs) = if (name x) `elem` (elements m)
        then alg1ExpandSet (Set (name m) newE) xs
        else alg1ExpandSet m xs
        where newE = mergeU (elements m) (elements x)


    alg1RecreateRules :: [Set] -> [Rule] -> [Rule]
    alg1RecreateRules [] _ = []
    alg1RecreateRules (s:ss) r = (alg1GetRulesBySet s r) ++ alg1RecreateRules ss r

    alg1GetRulesBySet :: Set -> [Rule] -> [Rule]
    alg1GetRulesBySet s [] = []
    alg1GetRulesBySet s (r:rs) = if leftSide r `elem` elements s && not easyRule
        then (Rule (name s) (rightSide r)) : (alg1GetRulesBySet s rs)
        else alg1GetRulesBySet s rs
        where easyRule = length (rightSide r) == 1 && (head (rightSide r)) `elem` elements s


    alg1 :: ([N], [T], N, [Rule]) -> ([N], [T], N, [Rule])
    alg1 (n,t,s,r) = (n,t,s,alg1RecreateRules sets r)
        where 
            sets = alg1ExpandSets setsNext setsNext
            setsNext = alg1SucSets n n r


    ------------------------------------------------------------------------------
    ------------------------------- ALG2 -----------------------------------------
    ------------------------------------------------------------------------------
    -- prerazeni pravidel
    alg2RegroupRules :: [Rule] -> [Rule]
    alg2RegroupRules r = oldAlteredRules ++ newNontermRules ++ newTermRules
        where
            oldAlteredRules = [x | x <- r, not ('\'' `elem` leftSide x) && '<' /= head (leftSide x)]
            newNontermRules = [y | y <- r, '<' == head (leftSide y)]
            newTermRules = [z | z <- r, '\'' `elem` leftSide z]

    alg2NewRules :: [T] -> [Rule] -> [Rule]
    alg2NewRules _ [] = []
    alg2NewRules t (r:rs) = newRules ++ (alg2NewRules t rs)
        where newRules = alg2CreateNewRules t r

    alg2CreateNewRules :: [T] -> Rule -> [Rule]
    alg2CreateNewRules t r = if ruleLen == 1
            then [r]
        else if ruleLen > 2
            then [alteredRule]++ (symToRule fS) ++ (alg2CreateNewRules t newRule)
        else alg2RuleWithTwoSymbols t r
        where 
            ruleLen = length (rightSide r)
            fS = head (rightSide r)
            restS = tail (rightSide r)
            alteredRule = Rule (leftSide r) [nonterminal fS, longN]
            newRule = Rule longN restS
            longN = "<" ++ concatList restS ++ ">"
            nonterminal x = if x `elem` t
                then x ++ "'"
                else x
            symToRule sym = if sym `elem` t
                then [Rule (sym ++ "'") [sym]]
                else []

    alg2RuleWithTwoSymbols :: [T] -> Rule -> [Rule]
    alg2RuleWithTwoSymbols t r = if lS `elem` t && fS `elem` t
            then [Rule (leftSide r) [fS ++ "'", lS ++ "'"], fSRule, lSRule]
        else if fS `elem` t
            then [Rule (leftSide r) [fS ++ "'", lS], fSRule]
        else if lS `elem` t
            then [Rule (leftSide r) [fS, lS ++ "'"], lSRule]
        else [r]
        where 
            fS = head (rightSide r)
            lS = last (rightSide r)
            fSRule = Rule (fS ++ "'") [fS]
            lSRule = Rule (lS ++ "'") [lS]

    alg2NewNonterminals :: [N] -> [Rule] -> [N]
    alg2NewNonterminals n r = mergeU n [leftSide x | x <- r]


    alg2 :: ([N], [T], N, [Rule]) -> ([N], [T], N, [Rule])
    alg2 (n,t,s,r) = (newNonterminals, t, s, unique newRules)
        where
            newRules = alg2RegroupRules (alg2NewRules t r)
            newNonterminals = alg2NewNonterminals n newRules


    ------------------------------------------------------------------------------
    ------------------------------- PRINT ----------------------------------------
    ------------------------------------------------------------------------------
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
        let bkg = alg1 (n,t,s,r)
        printGrammar bkg

    printAlg2 :: ([N], [T], N, [Rule]) -> IO()
    printAlg2 (n,t,s,r) = do
        let bkg = alg2 (alg1 (n,t,s,r))
        printGrammar bkg

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

    ------------------------------------------------------------------------------
    ------------------------------ HELPER FUNCTIONS ------------------------------
    ------------------------------------------------------------------------------
    -- concatenate list of strings into single string
    concatList :: [String] -> String
    concatList = foldl (++) ""

    -- merge two lists into one with unique elements
    mergeU :: Eq a => [a] -> [a] -> [a]
    mergeU l [] = l
    mergeU l (x:xs) = if x `elem` l
        then mergeU l xs
        else mergeU (l ++ [x]) xs

    unique :: Eq a => [a] -> [a]
    unique [] = []
    unique (x:xs) = x:(unique (filter (/=x) xs))
    
    ------------------------------------------------------------------------------
    -------------------------------- MAIN ----------------------------------------
    ------------------------------------------------------------------------------
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
------------------------------------END------------------------------------------