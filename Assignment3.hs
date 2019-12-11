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
testCrib :: Crib
testCrib = ("COMPUTERSCIENCE","QWAVMZPNGFQVGWG")
testSB :: Steckerboard
testSB = [('L','P'),('C','Q'),('M','R'),('H','S'),('G','T'),('E','U'),('D','V'),('I','W'),('A','X')]

incrementOffsetsTimes :: Offsets -> Int -> Int -> Offsets
incrementOffsetsTimes ofs count incTimes | (count == incTimes) = ofs
                                         | otherwise = incrementOffsetsTimes (incrementOffsets ofs) (count + 1) incTimes

followMenu :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
followMenu c m sb ofs | length m == 0 = Just sb
                      | steckerAdd (r,ch) sb == sb = Nothing
                      | otherwise = followMenu c (tail m) (steckerAdd (r, ch) sb) ofs      
                      where r = enigmaEncode 
                                (steckerLetter sb ((fst c)!!(head m))) 
                                (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (incrementOffsetsTimes ofs 0 ((head m)+1)))
                            ch = (snd c) !! (head m)                              

--r = enigmaEncode (steckerLetter sb ((fst c) !! x)) (SimpleEnigma rotor1 rotor2 rotor3 reflectorB ofs)
--c = (snd c) !! x


charPInSteckerBoard  :: Char -> Steckerboard -> Bool
charPInSteckerBoard l sb | (length sb == 0) = False
                         | (l == snd (sb !! 0)) || (l == fst (sb !! 0)) = True
                         | otherwise = charPInSteckerBoard l (tail sb)

steckerAdd :: SteckerPair -> Steckerboard -> Steckerboard
steckerAdd (x,y) sb | ((charPInSteckerBoard x sb) || (charPInSteckerBoard y sb)) = sb
                    | otherwise = (sb ++ [(x,y)])