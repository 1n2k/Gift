module Quine (Term(Var,And,Or,Not), MinTerm(Term), quine, print) where
    import Prelude hiding (print)
    import Data.Bits
-- ========= --
-- Some ADTs --
-- ========= --

    data Term   = Var String | And Term Term | Or Term Term | Not Term | Val Bool
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
    print       :: Term      -> IO ()

-- ======================== --
-- Function implementations --
-- ======================== --

--- ----------------------------- ---
--- Collapsing MinTenrms to Terms ---
--- ----------------------------- --- 
    (⇔)         :: Int -> Int -> Int
    (⇔) a b     = complement (xor a b)

--    termize     :: Int -> Int -> Term
--    termize 0   = True
--    termize 1   = (Var "x1")

--    collapse [x,y]
--                = 
    
    collapseAll [x]
                = collapse x
    collapseAll (x:xs)
                = (Or (collapse x) (collapseAll xs))

--- ----------------------- ---    
--- Puttin' it all together ---
--- ----------------------- ---

    quine []    = (Not True)
    quine x     = collapseAll (dominance x (minimize x))

--- -------------- ---
--- Printing terms ---
--- -------------- ---

    print (Val True)
                = "¹"
    print (Val False)
                = "⁰"
    print (Var a)
                = a
    print (Or a b)
                = (print a) ++ "∨" ++ (print b)
    print (And a b)
                = (print a) ++ "∧" ++ (print b)
    print (Not a)
                = "¬" ++ (print a)    
