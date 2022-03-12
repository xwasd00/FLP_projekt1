{-
Project: FLP project - bkg-2-cnf
Author: Michal Sova <xsovam00@stud.fit.vutbr.cz>
Year: 2022
Module: Algorithm1
Description: Replacing simple rules
-}

{-# LANGUAGE RecordWildCards #-}

module Algorithm1 (alg1) where

    import Types
    import Helper (mergeU)

    -- agorithm 1: delete simple rules
    alg1 :: Bkg -> Bkg
    alg1 (Bkg n t s r) = Bkg n t s (alg1RecreateRules sets r)
        where 
            sets = alg1ExpandSets setsNext setsNext
            setsNext = alg1SucSets n n r

    -- create sets for one step (A->B) => N_A = {A, B}
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
    alg1GetRulesBySet _ [] = []
    alg1GetRulesBySet s (r:rs) = if leftSide r `elem` elements s && not easyRule
        then (Rule (name s) (rightSide r)) : (alg1GetRulesBySet s rs)
        else alg1GetRulesBySet s rs
        where easyRule = length (rightSide r) == 1 && (head (rightSide r)) `elem` elements s
