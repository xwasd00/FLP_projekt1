{-
Project: FLP project - bkg-2-cnf
Author: Michal Sova <xsovam00@stud.fit.vutbr.cz>
Year: 2022
Module: Parser
Description: Implementation of bkg parser
-}

{-# LANGUAGE RecordWildCards #-}

module Parser (parseInput) where
    import Types

    -- parse list of input lines into grammar
    parseInput :: [String] -> Bkg
    parseInput (n:t:s:rules) = Bkg (parseN n) (parseT t) start (getRules rules)
        where 
            start = if s `elem` (parseN n) then s
                else error "parseInput: could not match starting symbol with nonterminals"
    parseInput _ = error "parseInput: wrong representation of bkg"
    
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
    getRule s = error ("getRule: wrong representation of rule: " ++ s)

