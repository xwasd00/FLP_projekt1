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
    alg2 (Bkg n t s r) = Bkg newNonterminals t s newRules
        where
            newRules = unique $ alg2RearrangeRules (alg2NewRules t r)
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

