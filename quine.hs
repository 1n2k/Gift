module Quine (Term(Var,And,Or,Not),MinTerm(Term))
    import Prelude
    import Data.Bits

-- ========= --
-- Some ADTs --
-- ========= --

    data Term   = Var String | And Term Term | Or Term Term | Not Term | True
        deriving (Eq, Show)
    data MinTerm 
                = Term Int
        deriving (Eq, Show)        

    minimize    :: [MinTerm] -> [[MinTerm]]
    rowDominance
                :: [MinTerm] -> [[MinTerm]] -> [MinTerm]
    lineDominance
                :: [MinTerm] -> [[MinTerm]] -> [[MinTerm]]
    dominance   :: [MinTerm] -> [[MinTerm]] -> [[MinTerm]]

    collapse    :: [MinTerm] -> Term
    collapseAll :: [[MinTerm]]
                             -> Term

    quine       :: [MinTerm] -> Term           

-- ======================== --
-- Function implementations --
-- ======================== --

--- ----------------------------- ---
--- Collapsing MinTenrms to Terms ---
--- ----------------------------- --- 

    collapse [] = (Not True)
--    collapse [x,y]
--                = 

--- ----------------------- ---    
--- Puttin' it all together ---
--- ----------------------- ---

    quine []    = (Not True)
    quine x     = collapseAll (dominance x (minimize x))
