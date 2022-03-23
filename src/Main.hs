{-
Project: FLP project - bkg-2-cnf
Author: Michal Sova <xsovam00@stud.fit.vutbr.cz>
Year: 2022
Module: Main
Description: Main file, handling arguments and IO
-}

{-# LANGUAGE RecordWildCards #-}

module Main(main) where
    import System.Environment (getArgs)

    import Types
    import Parser (parseInput)
    import Algorithm1 (alg1)
    import Algorithm2 (alg2)
    import Helper (concatList)

    ------------------------------------------------------------------------------
    ------------------------------- PRINT ----------------------------------------
    ------------------------------------------------------------------------------
    -- print grammar to stdout
    printGrammar :: Bkg -> IO()
    printGrammar (Bkg n t s r) = do
        printListLn n
        printListLn t
        putStrLn s
        printRules r

    -- print elements of list divided by comma: ["a", "b"] => a,b
    printListLn :: [String] -> IO()
    printListLn [] = putStr "\n"
    printListLn [x] = putStr (x ++ "\n")
    printListLn (x:xs) = do
        putStr x
        putStr ","
        printListLn xs

    -- print rules: Rule "A" ["a", "B"] => A->aB
    printRules :: [Rule] -> IO()
    printRules [] = putStr ""
    printRules (r:rs) = do
        putStr (leftSide r)
        putStr "->"
        putStrLn (concatList (rightSide r))
        printRules rs

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
            then print bkg
        else if option == "-1"
            then printGrammar $ alg1 bkg
        else if option == "-2"
            then printGrammar $ alg2 $ alg1 bkg
        else fail "Wrong option, possible options are '-i', '-1' or '-2'"
------------------------------------END------------------------------------------
