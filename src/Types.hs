{-
Project: FLP project - bkg-2-cnf
Author: Michal Sova <xsovam00@stud.fit.vutbr.cz>
Year: 2022
Module: Types
Description: Declaration of data and types used in project
-}

{-# LANGUAGE RecordWildCards #-}

module Types where
    -- terminal
    type T = String
    -- nonterminal
    type N = String
    -- nonterminal or terminal
    type Symbol = String
    -- rule definition: leftSide -> rightSide
    data Rule = Rule {leftSide :: N, rightSide :: [Symbol]} deriving (Eq, Show)
    
    -- set definition
    data Set = Set {name :: N, elements :: [N]} deriving (Eq, Show)
    -- grammar definition
    data Bkg = Bkg {nonterms :: [N], terms :: [T], start :: N, rules :: [Rule]}

    instance Show Bkg where
        show (Bkg n t s r) = "nonterminals: " ++ show n ++ "\n" ++ "terminals: " ++ show t ++ "\n" ++ "start: " ++ show s ++ "\n" ++ "rules: " ++ show r