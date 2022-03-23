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

    -- agorithm 1: replace simple rules
    alg1 :: Bkg -> Bkg
    alg1 (Bkg n t s r) = Bkg n t s (recreateRules sets r)
        where 
            sets = expandSets setsNext setsNext
            setsNext = sucSets n n r

    -- create sets for one step (A->B) => N_A = {A, B}
    sucSets :: [N] -> [N] -> [Rule] -> [Set]
    sucSets [] _ _ = []
    sucSets (n:ns) allN allR = set : (sucSets ns allN allR)
        where set = Set n (n:(sucSet n allN allR))

    -- create set N_x, where x is not included in N_x after one step of algorithm => N_A = {B}
    sucSet :: N -> [N] -> [Rule] -> [N]
    sucSet x allN allR = [n | n <- allN, rule <- allR, x /= n, Rule x [n] == rule]

    -- expand sets
    -- (A ->* C) => N_A = {A, B} -> N_A = {A, B, C}
    expandSets :: [Set] -> [Set] -> [Set]
    expandSets [] _ = []
    expandSets (x:xs) allSets = expandIf x allSets : expandSets xs allSets

    -- expand set until N_x_old == N_x_new 
    -- N_A = {A, B} => N_A = {A, B, C} => N_A = {A, B, C} -> cannot expand more
    expandIf :: Set -> [Set] -> Set
    expandIf oldSet allSets
        | newSet == oldSet = oldSet
        | otherwise        = expandIf newSet allSets
        where 
            newSet = expandSet oldSet allSets

    -- expand set m by other sets
    expandSet :: Set -> [Set] -> Set
    expandSet m [] = m
    expandSet m (x:xs) = if (name x) `elem` (elements m)
        then expandSet (Set (name m) newElements) xs
        else expandSet m xs
        where newElements = mergeU (elements m) (elements x)

    -- recreate rules from old rules and created sets
    recreateRules :: [Set] -> [Rule] -> [Rule]
    recreateRules [] _ = []
    recreateRules (s:ss) r = (getRulesBySet s r) ++ recreateRules ss r

    -- recreate rules by set
    getRulesBySet :: Set -> [Rule] -> [Rule]
    getRulesBySet _ [] = []
    getRulesBySet s (r:rs) = if leftSide r `elem` elements s && not easyRule
        then (Rule (name s) (rightSide r)) : (getRulesBySet s rs)
        else getRulesBySet s rs
        where easyRule = length (rightSide r) == 1 && (head (rightSide r)) `elem` elements s
