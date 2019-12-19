{- Name: Daniel Whiteman -}
module Assignment3 where
import AssignmentHelp
import Assignment2
import Data.List
import Data.Char
type SteckerPair = (Char,Char)


{- TASK 2 - TESTING
(1)
p = "COMPUTERSCIENCECALIBRATIONSTRINGTESTINGONETWOTHREE"
x = "QWAVMZPNGFQVGWGYCKCXXHMEXTCGWPFOCWCSYXAEFUNXQFIZJW"
Input: breakEnigma (p,x)
Output: Just ((0,0,0),[('R','M'),('N','N'),('G','T'),('H','S'),('O','O'),('U','E'),('X','A'),('W','I'),('Q','C')])
x decoded to: "COMYUTWRSCIZNCECAPIBRATIONSTRTNGTESTINGONETWOTHREE"

(2)
p4 = "AFJEQTMC"
x4 = "FJEQTMCF"
Input: breakEnigma (p4,x4)
Output: Just ((0,0,0),[('A','A'),('B','F'),('J','J'),('D','E'),('L','Q'),('U','T'),('P','M'),('I','C')])
x4 decoded to: "AFJEQTMC" 

(3)
p5 = "ZGXWAUTS"
x5 = "XKGZWAUT"
Input: breakEnigma (p5,x5)
Output: Just ((0,0,0),[('S','S'),('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X'),('G','G')])
x5 decoded to: "ZGXWAUTS" 

(4)
p6 = "TURINGBOMBEHASKELLSIMULATIONSTOP"
x6 = "FDLQIYHKFXSYEEXAYTWJBNNMFCHUACVMERSLXIXVWCCOBSVUESKCQGKSCXSQUMCWLWXCW
     NDEKHCGRKAUWLSCNUUROQVOTZCWUICNEXDCQPKQKHSTFTCQJXEFKDLKOTH"
Input: breakEnigma (p6,x6)
Output: Just ((1,2,14),[('S','X'),('B','E'),('N','A'),('J','M'),('C','H'),('V','Y')])
x6 decoded to: "TURINGBOMBEHASKELLSIMULATIONSTOPYOUSHOULDHAVEAMENUFORTHISTE
                STTHATISSUFFICIENTTODECODETHISMESSAGESTOPIFNOTITISALLEMMASFAULTAGAIN" 

(5)                
p7 = "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP"
x7 = "YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHLAPJMTMFNLURRADJFCBRBXBCUS
      SXVYWAPQIRCUVVNODKELDMNNQHYFEFOZPBUIPWKPXIYPKQHMVOAVXFVDCKMZOUL
      MTQNUFBVHFUSXYCYPWFKBYW"
Input: breakEnigma (p7,x7)
Output: Just ((4,3,7),[('T','T'),('K','C'),('N','E'),('O','M'),('V','V'),('P','P'),('S','S'),('J','U'),('X','F'),('G','R'),
                                                                                                        ('L','D'),('B','Z')])
x7 decoded to: "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOS
                ECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP"
-}




{- breakEnigma - Finds the longest menu of the crib then tries out different intial offsets
   starting at (0,0,0), until one succeeds or it reaches (25,25,25), which indicates failure.
   
   params: A crib (p,c) - tuple of two strings, plaintext and ciphertext.

   return: Maybe (Offsets, Steckerboard) - the steckerboard and the offsets at which it was found.-}

breakEnigma :: Crib -> Maybe (Offsets, Steckerboard)
breakEnigma (p,c) = breakEA (p,c) (longestMenu (p,c)) [(p !! (head (longestMenu (p,c))),p !! (head (longestMenu (p,c))))] (0,0,0)





{- breakEA - Auxiliary function to breakEnigma. Tries to find a steckerboard and returns it if one is
   found. If no steckerboard is found then the function recurses my the next set of offsets.
   The function reports failure if there is no steckerboard found after offsets (25,25,25).
   
   params: crib - tuple of two strings, plaintext and ciphertext.
           Menu - List of indexes (int).
           Steckerboard - Initial steckerboard that we start with.
           offsets - The set of offsets that we are currently using.

   return: Maybe (Offsets, Steckerboard) - the steckerboard and the offsets at which it was found.-}

breakEA :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe (Offsets, Steckerboard)
breakEA c m sb ofs |(ofs == (25,25,25)) = if ((findStecker c m sb ofs) == Nothing) then Nothing
                                          else Just (ofs,fromMaybe(findStecker c m sb ofs))
                   |(findStecker c m sb ofs == Nothing) = breakEA c m sb (incrementOffsets ofs)
                   |otherwise = Just (ofs,fromMaybe(findStecker c m sb ofs))





{- incrementAlphabet (Helper function for findStecker)- Advances the intial steckerboard pair. 
   For example if it was given [('A','A')], then it would return [('A','B')].
   
   params: Steckerboard - steckerboard we want to increment.
   
   return: Steckerboard - incremented steckerboard.-}

incrementAlphabet :: Steckerboard -> Steckerboard
incrementAlphabet ((x,y):rest) = [(x, alphabet!!((alphaPos y + 1) `mod` 26))]





{- findStecker - findStecker is given a 1-pair initial steckerboard [(x,y)]. It then tries
   to find a steckerboard. If it finds one then it is returned and if not the function
   recurses with a new 1-pair initial steckerboard [(x,(y+1 mod 26))]. If all 26 possible pairs
   have been tried we still don't have a steckerboard then the function reports failure.
   
   params: crib - tuple of two strings, plaintext and ciphertext.
           Menu - List of indexes (int).
           Steckerboard - Initial steckerboard that we start with.
           offsets - The set of offsets that we are currently using.
           
   return: Maybe steckerboard - Potential steckerboard that the function could find.-}

findStecker :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
findStecker c m [(x,y)] ofs |((alphaPos x) - (alphaPos y) == 1) = 
                                        if (followMenu c m [(x,y)] ofs == Nothing) then Nothing
                                        else followMenu c m [(x,y)] ofs
                            |(followMenu c m [(x,y)] ofs == Nothing) = findStecker c m (incrementAlphabet [(x,y)]) ofs
                            |otherwise = (followMenu c m [(x,y)] ofs)





{- incrementOffsetsTimes (Helper function for followMenu) - Increments the offsets n times. 
   For example if I give it (0,0,0) and want to increment it 5 times, the function would 
   return (0,0,5).
   
   params: offsets - Set of offsets that we want to increment.
           count - counter.
           incTimes - number of times we want to increment the offsets.

   return: Offsets - Incremented offsets.-}

incrementOffsetsTimes :: Offsets -> Int -> Int -> Offsets
incrementOffsetsTimes ofs count incTimes | (count == incTimes) = ofs
                                         | otherwise = incrementOffsetsTimes (incrementOffsets ofs) (count + 1) incTimes





{- followMenu - Attempts to follow a menu and tries to build a steckerboard. If the function
   reaches the end of the list then the current steckerboard is returned otherwise the function
   reports failure.
   
   params: crib - tuple of two strings, plaintext and ciphertext.
           Menu - List of indexes (int).
           Steckerboard - steckerboard being built.
           offsets - The set of offsets that we are currently using.
   
   return: Maybe steckerboard - Potential steckerboard that the function could find.-}

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





{- charPInSteckerBoard - Checks if the given character exists in a steckerboard and returns a
   boolean. Returns true if the character exists in the steckerboard and false if not.
    
   params: character - character being checked.
           Steckerboard - steckerboard being checked.
    
   returns: Bool - true if character is in the steckerboard, false if not.-}

charPInSteckerBoard :: Char -> Steckerboard -> Bool
charPInSteckerBoard l [] = False
charPInSteckerBoard l [(x,y)] | (l == y) || (l == x) = True
                              | otherwise = False
charPInSteckerBoard l ((x,y):rest) | (l == y) || (l == x) = True
                                   | otherwise = charPInSteckerBoard l rest





{- steckerAdd - Tries to add a steckerpair to a steckboard. Returns the new steckerboard if successful
   and returns nothing if unsuccessful.

   params: Steckerpair - pair of characters we want to add.
           Steckerboard - Steckerboard we want to add too.     

   return: Maybe Steckerboard - Potential updated steckerboard.-}

steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
steckerAdd (x,y) sb | (elem (x,y) sb) || (elem (y,x) sb) = Just sb
                    | ((charPInSteckerBoard x sb) || (charPInSteckerBoard y sb)) = Nothing
                    | otherwise = Just (sb ++ [(x,y)])