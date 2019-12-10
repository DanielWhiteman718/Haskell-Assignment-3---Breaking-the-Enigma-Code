{- Name: Daniel Whiteman -}
module Assignment3 where
import AssignmentHelp
import Assignment2
--import bombeTesting16
import Data.List
import Data.Char

type SteckerPair = (Char,Char)
{- breakEnigma :: Crib -> Maybe (Offsets, Steckerboard) -}

{- breakEA :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe (Offsets, Steckerboard) -}

{- findStecker :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard -}

{-followMenu :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard-}



charPInSteckerBoard  :: Char -> Steckerboard -> Bool
charPInSteckerBoard l sb | (length sb == 0) = False
                         | (l == snd (sb !! 0)) || (l == fst (sb !! 0)) = True
                         | otherwise = charPInSteckerBoard l (tail sb)

steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
steckerAdd (x,y) sb | ((charPInSteckerBoard x sb) || (charPInSteckerBoard y sb)) = Nothing
                    | otherwise = Just (sb ++ [(x,y)])