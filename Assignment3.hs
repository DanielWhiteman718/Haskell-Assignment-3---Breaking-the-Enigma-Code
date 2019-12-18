{- Name: Daniel Whiteman -}
module Assignment3 where
import AssignmentHelp
import Assignment2
import Data.List
import Data.Char
type SteckerPair = (Char,Char)

breakEnigma :: Crib -> Maybe (Offsets, Steckerboard)
breakEnigma (p,c) = breakEA (p,c) (longestMenu (p,c)) [(p !! (head (longestMenu (p,c))),p !! (head (longestMenu (p,c))))] (0,0,0)

breakEA :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe (Offsets, Steckerboard)
breakEA c m sb ofs |(ofs == (25,25,25)) = if ((findStecker c m sb ofs) == Nothing) then Nothing
                                          else Just (ofs,fromMaybe(findStecker c m sb ofs))
                   |(findStecker c m sb ofs == Nothing) = breakEA c m sb (incrementOffsets ofs)
                   |otherwise = Just (ofs,fromMaybe(findStecker c m sb ofs))


incrementAlphabet :: Steckerboard -> Steckerboard
incrementAlphabet ((x,y):rest) = [(x, alphabet!!((alphaPos y + 1) `mod` 26))]

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
followMenu (p,c) [] sb ofs = Just sb

followMenu (p,c) [x] sb ofs | (steckerAdd (r,ch) sb == Nothing) = Nothing
                            | otherwise = steckerAdd (r, ch) sb    
                            where r = enigmaEncode (steckerLetter sb (p!!x)) (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (incrementOffsetsTimes ofs 0 (x+1)))
                                  ch = c !! x

followMenu (p,c) (x:xs) sb ofs | (steckerAdd (r,ch) sb == Nothing) = Nothing
                               | otherwise = followMenu (p,c) (xs) (fromMaybe (steckerAdd (r, ch) sb)) ofs     
                               where r = enigmaEncode (steckerLetter sb (p!!x)) (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (incrementOffsetsTimes ofs 0 (x+1)))
                                     ch = c !! x                              






charPInSteckerBoard :: Char -> Steckerboard -> Bool
charPInSteckerBoard l [] = False
charPInSteckerBoard l [(x,y)] | (l == y) || (l == x) = True
                              | otherwise = False
charPInSteckerBoard l ((x,y):rest) | (l == y) || (l == x) = True
                                   | otherwise = charPInSteckerBoard l rest

steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
steckerAdd (x,y) sb | (elem (x,y) sb) || (elem (y,x) sb) = Just sb
                    | ((charPInSteckerBoard x sb) || (charPInSteckerBoard y sb)) = Nothing
                    | otherwise = Just (sb ++ [(x,y)])