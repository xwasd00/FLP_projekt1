{-
Project: FLP project - bkg-2-cnf
Author: Michal Sova <xsovam00@stud.fit.vutbr.cz>
Year: 2022
Module: Algorithm2
Description: Implementation of algorithm bkg-2-cnf
-}

{-# LANGUAGE RecordWildCards #-}

module Algorithm2 (alg2) where

    import Types
    import Helper (concatList, unique, mergeU)

    -- algorithm 2: bkg-2-cnf, grammar already does not contain simple rules
    alg2 :: Bkg -> Bkg
    alg2 (Bkg n t s r) = Bkg newN t s newR
        where
            newR = unique $ rearrangeRules (newRules t r)
            newN = newNonterminals n newR

    -- rearrange rules
    rearrangeRules :: [Rule] -> [Rule]
    rearrangeRules r = oldAlteredRules ++ newNontermRules ++ newTermRules
        where
            oldAlteredRules = [x | x <- r, not ('\'' `elem` leftSide x) && '<' /= head (leftSide x)]
            newNontermRules = [y | y <- r, '<' == head (leftSide y)]
            newTermRules = [z | z <- r, '\'' `elem` leftSide z]

    -- create new rules based on algorithm 2
    newRules :: [T] -> [Rule] -> [Rule]
    newRules _ [] = []
    newRules t (r:rs) = newR ++ (newRules t rs)
        where newR = createNewRules t r

    -- modify rule to be in cnf, add new rules if necessary
    -- A -> t => [A -> t]
    -- A -> Bc => ruleWithTwoSymbols
    -- A -> ccABs => [A -> c'<cABs>; c' -> c; <cABs> -> ...; ...]
    createNewRules :: [T] -> Rule -> [Rule]
    createNewRules t r
        | ruleLen == 1 = [r]
        | ruleLen == 2 = ruleWithTwoSymbols t r
        | otherwise    = [alteredRule] ++ (symToRule fS) ++ (createNewRules t newRule)
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
    -- A -> bc => [A -> b'c'; b' -> b; c' -> c]
    -- A -> bC => [A -> b'C; b' -> b]
    -- A -> Bc => [A -> Bc'; c' -> c]
    -- A -> BC => [A -> BC]
    ruleWithTwoSymbols :: [T] -> Rule -> [Rule]
    ruleWithTwoSymbols t r
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

    -- get new nonterminals from old nonterminals and new rules
    newNonterminals :: [N] -> [Rule] -> [N]
    newNonterminals n r = mergeU n [leftSide x | x <- r]

