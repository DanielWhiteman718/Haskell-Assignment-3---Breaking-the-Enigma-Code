{- Name: Daniel Whiteman -}
module Assignment3 where
import AssignmentHelp
import Assignment2
import Data.List
import Data.Char
type SteckerPair = (Char,Char)
c :: Crib
c = ("AIDEGHMC","TTCMAANO")
p :: String
p = "COMPUTERSCIENCECALIBRATIONSTRINGTESTINGONETWOTHREE"
s :: Steckerboard
s = [('L','P'),('C','Q'),('M','R'),('H','S'),('G','T'),('E','U'),('D','V'),('I','W'),('A','X')]

breakEnigma :: Crib -> Maybe (Offsets, Steckerboard)
--breakEnigma crib = breakEA crib (longestMenu crib) [((fst crib) !! (head(longestMenu crib)),(fst crib) !! (head(longestMenu crib)))] (0,0,0)
breakEnigma crib = breakEA crib (longestMenu crib) [((fst crib) !! (head (longestMenu crib)),(fst crib) !! (head (longestMenu crib)))] (0,0,0)

breakEA :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe (Offsets, Steckerboard)
breakEA c m sb ofs |(ofs == (25,25,25)) = if ((findStecker c m sb ofs) == Nothing) then Nothing
                                          else Just (ofs,fromMaybe(findStecker c m sb ofs))
                   |(findStecker c m sb ofs == Nothing) = breakEA c m sb (incrementOffsets ofs)
                   |otherwise = Just (ofs,fromMaybe(findStecker c m sb ofs))


incrementAlphabet :: Steckerboard -> Steckerboard
incrementAlphabet sb = [(fst (head sb), alphabet!!((alphaPos (snd (head sb)) + 1) `mod` 26))]

findStecker :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
findStecker c m [(x,y)] ofs |((alphaPos x) - (alphaPos y) == 1) = 
                                        if (followMenu c m [(x,y)] ofs == Nothing) then Nothing
                                        else followMenu c m [(x,y)] ofs
                            |(followMenu c m [(x,y)] ofs == Nothing) = findStecker c m (incrementAlphabet [(x,y)]) ofs
                            |otherwise = (followMenu c m [(x,y)] ofs)

testCrib :: Crib
testCrib = ("COMPUTERSCIENCE","QWAVMZPNGFQVGWG")
testSB :: Steckerboard
testSB = [('L','P'),('C','Q'),('M','R'),('H','S'),('G','T'),('E','U'),('D','V'),('I','W'),('A','X')]


incrementOffsetsTimes :: Offsets -> Int -> Int -> Offsets
incrementOffsetsTimes ofs count incTimes | (count == incTimes) = ofs
                                         | otherwise = incrementOffsetsTimes (incrementOffsets ofs) (count + 1) incTimes

followMenu :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
followMenu c m sb ofs | (length m == 0) = Just sb
                      | (steckerAdd (r,ch) sb == Nothing) = Nothing
                      | otherwise = followMenu c (tail m) (fromMaybe (steckerAdd (r, ch) sb)) ofs     
                      where r = enigmaEncode 
                                (steckerLetter sb ((fst c)!!(head m))) 
                                (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (incrementOffsetsTimes ofs 0 ((head m)+1)))
                            ch = (snd c) !! (head m)                              


charPInSteckerBoard  :: Char -> Steckerboard -> Bool
charPInSteckerBoard l sb | (length sb == 0) = False
                         | (l == snd (sb !! 0)) || (l == fst (sb !! 0)) = True
                         | otherwise = charPInSteckerBoard l (tail sb)

steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
steckerAdd (x,y) sb | ((charPInSteckerBoard x sb) || (charPInSteckerBoard y sb)) = Nothing
                    | otherwise = Just (sb ++ [(x,y)])