module Quine (Term(Var,And,Or,Not), MinTerm(MTerm), CombinedTerm(Term), quine, print) where
    import Prelude hiding (print)
    import Data.Bits
    import Data.List
-- ========= --
-- Some ADTs --
-- ========= --

    data Term   = Var String | And Term Term | Or Term Term | Not Term | Val Bool
        deriving (Eq)
    data MinTerm 
                = MTerm Int
        deriving (Eq, Show)
    data CombinedTerm
                = Term String 
        deriving (Eq, Show)

    combine     :: Int            -> MinTerm   -> CombinedTerm

    minimize    :: [CombinedTerm] -> [CombinedTerm]

--    rowDominance
--                :: [MinTerm] -> [[MinTerm]] -> [MinTerm]
--    lineDominance
--                :: [MinTerm] -> [[MinTerm]] -> [[MinTerm]]
    dominance   :: [MinTerm] -> [CombinedTerm] -> [CombinedTerm]

    collapse    :: Int       -> CombinedTerm   -> Term
    collapseAll :: [CombinedTerm]
                             -> Term

    quine       :: [MinTerm] -> Term           

    print       :: Term      -> String

-- ======================== --
-- Function implementations --
-- ======================== --
--- ----------- ---
--- Basic stuff ---
--- ----------- ---

    (⇔)         :: Int -> Int -> Int
    (⇔) a b     = complement (xor a b)

    limit       :: (Eq a) => (a -> a) -> a -> a
    limit f α
                | α == α' = α
                | otherwise = limit f α'
        where
            α'  = (f α)


    construct   :: Int -> Int -> String
    construct 0 _
                = ""
    construct length value
                | (mod value 2) == 0 = (next ++ "0")
                | otherwise = (next ++ "1")
        where
            next
                = (construct (length-1) (div value 2))

    combine 0 _ = (Term "")
    combine l (MTerm a)
                = (Term (construct l a))
--- -------------- ---
--- Minimize terms ---
--- -------------- ---

    countChar   :: String -> Char -> Int
    countChar "" _
                = 0
    countChar (s:str) c
                | s == c = 1 + remainder
                | otherwise = remainder
        where
            remainder
                = countChar str c

    termCmp     :: CombinedTerm -> CombinedTerm -> Bool
    termCmp (Term a) (Term b) 
                = (countChar a '1') < (countChar b '1')

    termSort    :: [CombinedTerm] -> [CombinedTerm]
    termSort [] = []
    termSort (x:xs)
                = termSort [a | a <- xs, (termCmp a x)]
                ++ [x]
                ++ termSort [a | a <- xs, not (termCmp a x)]

    countDiffsO  :: CombinedTerm -> CombinedTerm -> Int
    countDiffsO (Term "") _
                = 0
    countDiffsO _ (Term "")
                = 0
    countDiffsO (Term (a:as)) (Term (b:bs))
                | (a /= b) && ((a == '-') || (b == '-')) = 2
                | (a /= b) = 1 + remainder
                | otherwise = remainder
        where
            remainder
                = countDiffsO (Term as) (Term bs)

    mergeO      :: CombinedTerm -> CombinedTerm -> CombinedTerm
    mergeO (Term "") _
                = (Term "")
    mergeO _ (Term "")
                = (Term "")
    mergeO (Term (a:as)) (Term (b:bs))
                | a == b = (Term (a:rem))
                | otherwise = (Term ('-':rem))
        where
            (Term rem)
                = mergeO (Term as) (Term bs)

    minimizeHelp
                :: [CombinedTerm] -> [CombinedTerm]
    minimizeHelp [] 
                = []
    minimizeHelp ((x@(Term ξ)):xs)
                | foldl (||) False (map ((==) 1) (map (countDiffsO x) xs)) = [(mergeO x a) | a <- xs, (countDiffsO x a) == 1 ] ++ remaining
                | otherwise = [x] ++ remaining
        where
            remaining
                = minimizeHelp xs

    minimizeHelp2
                :: [CombinedTerm] -> [CombinedTerm]
    minimizeHelp2 a
                = intersect (nub (minimizeHelp α)) (nub (minimizeHelp α'))
        where
            α   = termSort a
            α'  = reverse α

    minimize a  = limit minimizeHelp2 a          

--- ------------------ ---
--- Dominance checking ---
--- ------------------ ---

    dominance [] []
                = []

--- ----------------------------- ---
--- Collapsing MinTenrms to Terms ---
--- ----------------------------- --- 

    collapse _ (Term "")
                = (Val True)
    collapse i (Term [a])
                | a == '1' = xi
                | a == '0' = (Not xi)
                | otherwise = (Val True)
        where
            xi  = (Var ("x" ++ (show i)))
    collapse i (Term (a:as))
                | a == '1' = (And xi remainder)
                | a == '0' = (And (Not xi) remainder)
                | otherwise = remainder
        where
            xi  = (Var ("x" ++ (show i)))
            remainder
                = collapse (i+1) (Term as)
    
    simplify    :: Term -> Term
    simplify (And (Val True) (Val True))
                = (Val True)
    simplify (And a (Val True))
                = simplify a
    simplify (And (Val True) a)
                = simplify a
    simplify (And a b)
                | α == (Val True) && β == (Val True) = (Val True)
                | β == (Val True) = α
                | α == (Val True) = β
                | otherwise = (And α β)
        where
            α   = simplify a
            β   = simplify b
    simplify a  = a

    collapseAll [x]
                = simplify (collapse 1 x)
    collapseAll (x:xs)
                | ξ == (Val True) || χ == (Val True) = (Val True)
                | otherwise = (Or ξ χ)
        where 
            ξ   = simplify (collapse 1 x)
            χ   = collapseAll xs

--- ----------------------- ---    
--- Puttin' it all together ---
--- ----------------------- ---

    -- Calculate the number of used variables
    getMax      :: [MinTerm] -> Int
    getMax []   = 1
    getMax ((MTerm a):xs)
                = max (ceiling (log2 (fromIntegral a))) (getMax xs) 
        where
            log2 a
                = ((log a) / (log 2))

    quine []    = (Val False)
    quine x     = collapseAll (dominance x (minimize (map (combine (getMax x)) x)))

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
                = (print a) ++ ") ∨ (" ++ (print b)
    print (And a b)
                = (print a) ++ " ∧ " ++ (print b)
    print (Not a)
                = "¬" ++ (print a)   

    put         :: Term -> IO()
    put a       = putStrLn ("(" ++ (print a) ++ ")")

-- ======== --
-- Examples --
-- ======== --

    combineAll  :: [MinTerm] -> [CombinedTerm]
    combineAll x 
                = map (combine (getMax x)) x

    example1    :: [MinTerm]
    example1    = [MTerm 4, MTerm 8, MTerm 9, MTerm 10, MTerm 11, MTerm 12, MTerm 14, MTerm 15]

