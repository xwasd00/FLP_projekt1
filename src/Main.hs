-- TODO: Readme, tests
module Main(main) where
    import System.Environment (getArgs)
    
    -- type and data definitions
    type T = String      -- terminal
    type N = String      -- nonterminal
    type Symbol = String -- nonterminal or terminal
    -- rule definition: leftSide -> rightSide
    data Rule = Rule {leftSide::N, rightSide::[Symbol]} deriving (Eq, Show)
    -- set definition: N_name = elements
    data Set = Set {name::N, elements::[N]} deriving (Eq, Show)


    ------------------------------------------------------------------------------
    ------------------------------ PARSING ---------------------------------------
    ------------------------------------------------------------------------------
    -- parse list of input lines into grammar
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
    
    -- get rule from string (line) 
    getRule :: String -> Rule
    getRule (x:'-':'>':xs) = Rule [x] [[s] | s <- xs]
    

    ------------------------------------------------------------------------------
    ------------------------------- ALG1 -----------------------------------------
    ------------------------------------------------------------------------------    
    -- agorithm 1: delete simple rules
    alg1 :: ([N], [T], N, [Rule]) -> ([N], [T], N, [Rule])
    alg1 (n,t,s,r) = (n,t,s,alg1RecreateRules sets r)
        where 
            sets = alg1ExpandSets setsNext setsNext
            setsNext = alg1SucSets n n r

    -- create sets for one step (A -> B) => N_A = {A, B}
    alg1SucSets :: [N] -> [N] -> [Rule] -> [Set]
    alg1SucSets [] _ _ = []
    alg1SucSets (n:ns) allN allR = set : (alg1SucSets ns allN allR)
        where set = Set n (n:(alg1SucSet n allN allR))

    -- create set N_x, where x is not included in N_x after one step of algorithm
    alg1SucSet :: N -> [N] -> [Rule] -> [N]
    alg1SucSet x allN allR = [n | n <- allN, rule <- allR, x /= n, Rule x [n] == rule]

    -- expand sets
    -- (A ->* C) => N_A = {A, B} -> N_A = {A, B, C}
    alg1ExpandSets :: [Set] -> [Set] -> [Set]
    alg1ExpandSets [] _ = []
    alg1ExpandSets (x:xs) allSets = alg1ExpandIf x allSets : alg1ExpandSets xs allSets

    -- expand set until N_x_old == N_x_new
    alg1ExpandIf :: Set -> [Set] -> Set
    alg1ExpandIf oldSet allSets 
        | newSet == oldSet = oldSet
        | otherwise        = alg1ExpandIf newSet allSets
        where newSet = alg1ExpandSet oldSet allSets

    -- expand set m by other sets
    alg1ExpandSet :: Set -> [Set] -> Set
    alg1ExpandSet m [] = m
    alg1ExpandSet m (x:xs) = if (name x) `elem` (elements m)
        then alg1ExpandSet (Set (name m) newElements) xs
        else alg1ExpandSet m xs
        where newElements = mergeU (elements m) (elements x)

    -- recreate rules from old rules and created sets
    alg1RecreateRules :: [Set] -> [Rule] -> [Rule]
    alg1RecreateRules [] _ = []
    alg1RecreateRules (s:ss) r = (alg1GetRulesBySet s r) ++ alg1RecreateRules ss r

    -- recreate rules by set
    alg1GetRulesBySet :: Set -> [Rule] -> [Rule]
    alg1GetRulesBySet s [] = []
    alg1GetRulesBySet s (r:rs) = if leftSide r `elem` elements s && not easyRule
        then (Rule (name s) (rightSide r)) : (alg1GetRulesBySet s rs)
        else alg1GetRulesBySet s rs
        where easyRule = length (rightSide r) == 1 && (head (rightSide r)) `elem` elements s


    ------------------------------------------------------------------------------
    ------------------------------- ALG2 -----------------------------------------
    ------------------------------------------------------------------------------
    -- algorithm 2: bkg-2-cnf, grammar already does not contain simple rules
    alg2 :: ([N], [T], N, [Rule]) -> ([N], [T], N, [Rule])
    alg2 (n,t,s,r) = (newNonterminals, t, s, unique newRules)
        where
            newRules = alg2RearrangeRules (alg2NewRules t r)
            newNonterminals = alg2NewNonterminals n newRules

    -- rearrange rules
    alg2RearrangeRules :: [Rule] -> [Rule]
    alg2RearrangeRules r = oldAlteredRules ++ newNontermRules ++ newTermRules
        where
            oldAlteredRules = [x | x <- r, not ('\'' `elem` leftSide x) && '<' /= head (leftSide x)]
            newNontermRules = [y | y <- r, '<' == head (leftSide y)]
            newTermRules = [z | z <- r, '\'' `elem` leftSide z]

    -- create new rules based on algorithm 2
    alg2NewRules :: [T] -> [Rule] -> [Rule]
    alg2NewRules _ [] = []
    alg2NewRules t (r:rs) = newRules ++ (alg2NewRules t rs)
        where newRules = alg2CreateNewRules t r

    -- modify rule to be in cnf, add new rules if necessary
    alg2CreateNewRules :: [T] -> Rule -> [Rule]
    alg2CreateNewRules t r
        | ruleLen == 1 = [r]
        | ruleLen == 2 = alg2RuleWithTwoSymbols t r
        | otherwise    = [alteredRule] ++ (symToRule fS) ++ (alg2CreateNewRules t newRule)
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

    -- modify rule that has only two symbols on the right side (4 possible combinations)
    alg2RuleWithTwoSymbols :: [T] -> Rule -> [Rule]
    alg2RuleWithTwoSymbols t r
        | lS `elem` t && fS `elem` t = [Rule (leftSide r) [fSNon, lSNon], fSRule, lSRule]
        | fS `elem` t = [Rule (leftSide r) [fSNon, lS], fSRule]
        | lS `elem` t = [Rule (leftSide r) [fS, lSNon], lSRule]
        | otherwise   = [r]
        where 
            fS = head (rightSide r)
            lS = last (rightSide r)
            fSNon = fS ++ "'"
            lSNon = lS ++ "'"
            fSRule = Rule fSNon [fS]
            lSRule = Rule lSNon [lS]

    -- get new nonterminals from old ones and rules
    alg2NewNonterminals :: [N] -> [Rule] -> [N]
    alg2NewNonterminals n r = mergeU n [leftSide x | x <- r]


    ------------------------------------------------------------------------------
    ------------------------------- PRINT ----------------------------------------
    ------------------------------------------------------------------------------
    -- print internal representation of grammar
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

    -- do and print algorithm 1 (option -1)
    printAlg1 :: ([N], [T], N, [Rule]) -> IO()
    printAlg1 (n,t,s,r) = do
        let bkg = alg1 (n,t,s,r)
        printGrammar bkg

    -- do and print algorithm 2 (option -2)
    printAlg2 :: ([N], [T], N, [Rule]) -> IO()
    printAlg2 (n,t,s,r) = do
        let bkg1 = alg1 (n,t,s,r) -- need to remove simple rules first
        let bkg2 = alg2 bkg1
        printGrammar bkg2

    -- print grammar to stdout
    printGrammar :: ([N], [T], N, [Rule]) -> IO()
    printGrammar (n,t,s,r) = do
        printList n
        putStrLn ""
        printList t
        putStrLn ""
        putStrLn s
        printRules r

    -- print elements of list divided by comma: ["a", "b"] => a,b
    printList :: [String] -> IO()
    printList [] = putStr ""
    printList [x] = putStr x
    printList (x:xs) = do
        putStr x
        putStr ","
        printList xs

    -- print rules: Rule "A" ["a", "B"] => A->aB
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
    mergeU x y = unique (x ++ y)

    -- return list with no duplicates
    unique :: Eq a => [a] -> [a]
    unique [] = []
    unique (x:xs) = x:(unique (filter (/=x) xs))
    

    ------------------------------------------------------------------------------
    -------------------------------- MAIN ----------------------------------------
    ------------------------------------------------------------------------------
    main :: IO()
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
        else fail "Wrong option, possible options are '-i', '-1' or '-2'"
------------------------------------END------------------------------------------
