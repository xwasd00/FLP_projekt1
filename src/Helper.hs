{-
Project: FLP project - bkg-2-cnf
Author: Michal Sova <xsovam00@stud.fit.vutbr.cz>
Year: 2022
Module: Helper
Description: Helper functions
-}

{-# LANGUAGE RecordWildCards #-}

module Helper where
    -- concatenate list of strings into single string
    concatList :: [String] -> String
    concatList = foldl (++) ""

    -- merge two lists into one with unique elements
    mergeU :: Eq a => [a] -> [a] -> [a]
    mergeU x y = unique (x ++ y)

    -- return list with no duplicates
    unique :: Eq a => [a] -> [a]
    unique [] = []
    unique (x:xs) = x:(unique (filter (/=x) xs))
