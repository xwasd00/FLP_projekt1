-- G = (N, T, S, P)

module Main(main) where
    import System.Environment (getArgs)

    parseInput :: [String] -> ([Char], [Char], Char, [(Char, [Char])])
    parseInput (n:t:s:rules) = (parseN n, parseT t, parseS s, getRules rules)
    
    -- get list of nonterminal symbols
    parseN :: String -> [Char]
    parseN n = [x | x <- n, x /= ',']
    -- get list of terminal symbols
    parseT :: String -> [Char]
    parseT t = [x | x <- t, x /= ',']
    -- get starting symbol
    parseS :: String -> Char
    parseS = head
    -- adjust list of rules
    -- "A->Bc" => ('A', "Bc")
    getRules :: [String] -> [(Char, [Char])]
    getRules [] = []
    getRules (x:xs) = getRule x : getRules xs
    
    getRule :: [Char] -> (Char, [Char])
    getRule (x:'-':'>':xs) = (x, xs)

    -- TODO: testovani, kontrola prejmenovani funkci, refaktorizace
    -- vytvoreni mnozin pro vice kroku (A ->* B)
    fn3 :: [(Char, [Char])] -> [(Char, [Char])]
    -- [('S', "SA"), ('A', "AB"), ('B', "B")] -> [('S', "SAB"), ('A', "AB"), ('B', "B")]
    fn3 [] = []
    fn3 (x:xs) = fn4 x xs : fn3 xs

    fn4 :: (Char, [Char]) -> [(Char, [Char])] -> (Char, [Char])
    -- ('S', "SA") [('A', "AB"), ('B', "B")] -> ('S', "SAB")
    fn4 old other = if snd new == snd old 
        then old
        else fn4 new other
        where new = fn5 old other

    fn5 :: (Char, [Char]) -> [(Char, [Char])] -> (Char, [Char])
    -- ('S', "SA") [('A', "AB"), ('B', "B")] -> ('S', "SAB")
    fn5 m [] = m
    fn5 m (x:xs) = if (fst x) `elem` (snd m) 
        then fn5 new xs
        else fn5 m xs
        where new = (fst m, mergeU (snd m) (snd x))

    mergeU :: [Char] -> [Char] -> [Char]
    -- "SA" "AB" -> "SAB"
    mergeU [] [] = []
    mergeU [] l = l
    mergeU l [] = l
    mergeU l (x:xs) = if x `elem` l
        then mergeU l xs
        else mergeU (l ++ [x]) xs

    -- vytvoreni mnozin jen pro jeden krok!!! TODO: pro vice -> jen zkontrolovat ostatni vytvorene mnoziny
    -- "SAB" [('S', "AB"), ('S', "c"), ('A', "B")] -> [('S', "S"), ('A', "AB"), ('B', "B")]
    fn2 :: [Char] -> [(Char, [Char])] -> [(Char, [Char])]
    fn2 [] r = []
    fn2 (n:ns) r = set : (fn2 ns r)
        where set = (n, n:(fn1 n ns r))

    -- 'A' "SB" [('S', "AB"), ('S', "c"), ('A', "B")] -> "B"
    -- 'A' "SB" ('S', "AB") -> ""
    fn1 :: Char -> [Char] -> [(Char, [Char])] -> [Char]
    fn1 x n r = [y | y <- n, rule <- r, [y] == snd rule && x == fst rule ]

    main = do
        (option:fileName) <- getArgs
        
        contents <- if null fileName
            then getContents
            else readFile (head fileName)

        let bkg = parseInput (lines contents)
        print bkg
        putStrLn "Done!"
