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
    data CombinedTerm
                = Term Int Int Int--Term (Bitmask (Varible=1 | Negation=0)) (Bitmask Ignore=0) (№ of Variables)
        deriving (Eq, Show)

    combine     :: MinTerm   -> Int            -> CombinedTerm

    minimize    :: [CombinedTerm] -> [CombinedTerm]

--    rowDominance
--                :: [MinTerm] -> [[MinTerm]] -> [MinTerm]
--    lineDominance
--                :: [MinTerm] -> [[MinTerm]] -> [[MinTerm]]
    dominance   :: [MinTerm] -> [CombinedTerm] -> [CombinedTerm]

    collapse :: [CombinedTerm]
                             -> Term

    quine       :: [MinTerm] -> Term           

    print       :: Term      -> IO ()

-- ======================== --
-- Function implementations --
-- ======================== --
--- ----------- ---
--- Basic stuff ---
--- ----------- ---

    (⇔)         :: Int -> Int -> Int
    (⇔) a b     = complement (xor a b)

    combine _ 0 = (Term 0 0 0)
    combine (Term a) l
                = (Term a ((2^l)-1) l)
--- -------------- ---
--- Minimize terms ---
--- -------------- ---
    isPowerOf2  :: Int -> Bool
    isPowerOf2 0
                = False
    isPowerOf2 1 
                = True
    isPOwerOf2 a
                = ((mod a 2) != 1) && (isPowerOf2 (a/2))

    minimize [] = []
    minimize [x]
                = [[x]]
    minimize [(Term x),(Term y)]
                | (isPowerOf2 (complement (x⇔y))) = []
                | otherwise          = [[(Term x

--- ------------------ ---
--- Dominance checking ---
--- ------------------ ---

    dominance [] []
                = []

--- ----------------------------- ---
--- Collapsing MinTenrms to Terms ---
--- ----------------------------- --- 

    termize     :: Int -> Int -> Term
    termize 0   = True
--    termize 1   = (Var "x1")

    collapse [Term a]
                = termize a
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

-- ======== --
-- Examples --
-- ======== --


