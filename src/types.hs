{-
Project: FLP project - bkg-2-cnf
Author: Michal Sova <xsovam00@stud.fit.vutbr.cz>
Year: 2022
Module: Types
Description: Declaration of datatypes used in project
-}

{-# LANGUAGE RecordWildCards #-}

module Types where
    -- type and data definitions
    type T = String      -- terminal
    type N = String      -- nonterminal
    type Symbol = String -- nonterminal or terminal
    -- rule definition: leftSide -> rightSide
    data Rule = Rule {leftSide::N, rightSide::[Symbol]} deriving (Eq, Show)
    -- set definition
    data Set = Set {name::N, elements::[N]} deriving (Eq, Show)
    -- bkg definition
    data Bkg = Bkg {nonterms::[N], terms::[T], start::N, rules::[Rule]} -- deriving (Show)

    instance Show Bkg where
        show (Bkg n t s r) = "nonterminals: " ++ show n ++ "\n" ++ "terminals: " ++ show t ++ "\n" ++ "start: " ++ show s ++ "\n" ++ "rules: " ++ show r