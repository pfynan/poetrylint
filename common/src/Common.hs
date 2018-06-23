{-# LANGUAGE DeriveFunctor #-}
module Common
    (syllabifyText, checkIambicPentameter, checkTextIambicPentameter, ScansionError(..), SourcePos(..)) where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
-- import Text.Hyphenation
import Data.Foldable
import Data.List
import Data.Function
import qualified Data.Char as Char
import Control.Applicative
import qualified Text.Pronounce as Pronounce
import qualified Text.Pronounce.ParseDict as Pronounce
import System.IO.Unsafe (unsafePerformIO)
import qualified Debug.Trace as Debug

data SourcePos = SourcePos { _sourceStart :: !Int, _sourceLength :: !Int }
            deriving (Show, Eq)


data Syllable = Syllable {
    _syllableText :: Text,
    _syllablePos :: SourcePos,
    _syllableStress :: Maybe Stress }
    deriving (Show, Eq)

data TextWord = TextWord {
    _wordText :: Text,
    _wordPos :: SourcePos }
    deriving (Show, Eq)


data ScansionError = IncorrectStress SourcePos Stress Stress
                   | UnknownWord SourcePos
                   | MissingWord
                   | ExtraWord SourcePos
                   deriving (Show, Eq)

data Stress = PrimaryStress
            | SecondaryStress
            | NoStress
            deriving (Show, Eq, Enum)

-- Method to syllabify using hyphenation
-- syllabifyText :: Text -> [Syllable]
-- syllabifyText = concat . map syllabifyWord . toWords

-- syllabifyWord :: (Int, Text) -> [Syllable]
-- syllabifyWord (startix, w) = getZipList $ Syllable <$> ZipList segments <*> ZipList ixs <*> ZipList stresses
--     where segments = hyphenateT w
--           ixs = unfoldr annseg (startix, segments)
--           annseg (i, []) = Nothing
--           annseg (i, s:ss) = Just (i, (i + Text.length s, ss))
--           stresses = iterate (`subtract` 1) 0

-- hyphenateT :: Text -> [Text]
-- hyphenateT = map Text.pack . hyphenate (english_US {hyphenatorLeftMin = 1, hyphenatorRightMin = 1}) . Text.unpack

-- FIXME: Whee!
cmuDict :: Pronounce.CMUdict
cmuDict = Debug.trace "Got dict" $! Pronounce.myDict

cmuDictStressToStress :: Char -> Stress
cmuDictStressToStress '0' = NoStress
cmuDictStressToStress '1' = PrimaryStress
cmuDictStressToStress '2' = SecondaryStress

-- What is a word -> a word has multiple stresses so model as function
-- variant -> syllable -> stress
-- returns list for different pronounciations of word
-- each element is a list of stresses for the word
lookupWordStress :: Text -> [[Stress]]
lookupWordStress = nub . map to_stress . flip Pronounce.runPronounce cmuDict . Pronounce.stressesForEntry . Text.toUpper
    where to_stress = adjust_single_syl . map cmuDictStressToStress . Text.unpack
          adjust_single_syl [] = []
          adjust_single_syl [s] = [SecondaryStress]
          adjust_single_syl ss = ss




-- returns list for different pronounciations of word
-- each element is a list of stresses for the word
wordGraphemesToStresses :: Text -> [[Maybe Stress]]
wordGraphemesToStresses = squelch_unknown . lookupWordStress
    where -- if word is unknown, return invalid
          squelch_unknown [] = [[Nothing]]
          squelch_unknown x = map (map Just) x

syllabifyText :: Text -> NonEmpty [Syllable]
syllabifyText = fromMaybe ([] NonEmpty.:| []) . NonEmpty.nonEmpty . fmap concat . allComb . fmap syllabifyWord . toWords

-- words (word is multiple syllables)
-- basically just want permutations
-- but I don't remember how that function works...
-- either rewrite that algorithm as an exercise
-- Or build a trie
-- the trie is probably better
-- can annotate the trie with the match weight
-- trie won't work. Need digraph...
-- search DAG with DFS to find all paths...
-- well... not really DFS, but yes
-- then yes
-- Do I need an actual graph representation?
-- Maybe no...
-- In list form, the graph's edges would be:
--  0,0 -> 1,0 -> 2,0 -> ...
-- The number will be...
-- 3 ** 10 or so. That's big...
-- But it's hard to be clever.
-- To do that, it would have to involve dynamic programming...
-- it would be to find the best match...
-- Need to derive bounds on the match essentially
-- So, to traverse the graph BFS:
-- for each a: map (a:) rest
-- [a,b] [c,d] 
-- [[a,c], [a,d]]
-- Or, think in steps
-- Need operator that prepends list of vertices on list of paths

-- Takes: list, where each element is a list of possible pronounciations
-- Returns: list of all combnations of all pronounciations
-- e.g. list of sequences of elements
allComb :: [[a]] -> [[a]]
allComb = foldr go []
    where go :: [a] -> [[a]] -> [[a]]
          go xs [] = map (:[]) xs
          go xs ps = do
              x <- xs
              p <- ps
              return $ x:p

-- Take a word, and syllabify it
-- 
syllabifyWord :: TextWord -> [[Syllable]]
syllabifyWord w = do
    stresses <- wordGraphemesToStresses . _wordText $ w
    return $ alignStressToGraph w $ stresses


alignStressToGraph :: TextWord -> [Maybe Stress] -> [Syllable]
alignStressToGraph w stresses =
    let nsyllables = length stresses
        wordlength = Text.length . _wordText $ w
        -- syllable text.
        -- divide word into equal parts
        -- so each syllable (i.e. stress) gets an equal chunk of the word
        segments = Text.chunksOf ((wordlength + nsyllables - 1) `div` nsyllables) (_wordText w)
        segmentlengths = map Text.length segments
        -- Index of each syllable start
        -- accumulate lengths of each syllable to get
        -- FIXME: init is partial
        start_ixs = map ((\(TextWord _ (SourcePos six _)) i -> i + six) w) (0 : (cumSum segmentlengths))
     in getZipList $ Syllable <$> ZipList segments <*> (SourcePos <$> ZipList start_ixs <*> ZipList segmentlengths) <*> ZipList stresses


cumSum :: Integral a => [a] -> [a]
cumSum ls = unfoldr go (0, ls)
    where go (i, []) = Nothing
          go (i, x:xs) = Just (i + x, (i + x, xs))


-- Split text into words. Like words function.
-- Returns list of TextWords, which preserve source positions
-- FIXME: handle apostrophes in words
toWords :: Text -> [TextWord]
toWords t = unfoldr go (t, 0)
    where go (t, i)
            | Text.null t || Text.null wordstart = Nothing
            | otherwise = Just (tw, (leftoverpart, inext))
                where (notwordpart, wordstart) = Text.span (not . Char.isAlphaNum) t
                      (wordpart, leftoverpart) = Text.span Char.isAlphaNum wordstart
                      iword = i + (Text.length notwordpart)
                      inext = iword + (Text.length wordpart)
                      tw = TextWord { _wordText = wordpart, _wordPos = SourcePos iword (Text.length wordpart) }

-- The above will syllabify the text
-- The next step is matching the text to a meter
-- Easiest approach is to just zip two together

iambicPentameter = concat $ replicate 5 [NoStress, PrimaryStress]

-- Compare syllable stresses
-- Secondary Stress matches either nostress or PrimaryStress
matchStress :: Stress -> Stress -> Bool
matchStress exp act = exp == act || act == SecondaryStress

checkStress :: Stress -> Syllable -> Maybe ScansionError
checkStress exp w
  | Nothing <- _syllableStress w = Just $ UnknownWord (_syllablePos w)
  | Just act <- _syllableStress w, not $ matchStress exp act = Just $ (IncorrectStress (_syllablePos w) exp act)
  | otherwise = Nothing

-- Returns list of errors
checkIambicPentameter :: [Syllable] -> [ScansionError]
checkIambicPentameter ss = matches
    where matches = unfoldr checkSyl (ss, iambicPentameter)
          checkSyl (syl:syls, stress:stresses)
            | Just err <- checkStress stress syl = Just (err, (syls, stresses))
            | otherwise = checkSyl (syls, stresses)
          checkSyl (syl:syls, []) = Just (ExtraWord (_syllablePos syl), (syls, []))
          checkSyl ([], stress:stresses) = Just (MissingWord, ([], stresses))
          checkSyl ([], []) = Nothing

shiftSourcePos n (SourcePos s l) = SourcePos (s + n) l
shiftScansionError n (IncorrectStress sp a b) = IncorrectStress (shiftSourcePos n sp) a b
shiftScansionError n (UnknownWord sp) = UnknownWord (shiftSourcePos n sp)
shiftScansionError n (MissingWord) = MissingWord
shiftScansionError n (ExtraWord sp) = ExtraWord (shiftSourcePos n sp)

checkTextIambicPentameterLine :: Text -> [ScansionError]
checkTextIambicPentameterLine = minimumBy (compare `on` length) . fmap checkIambicPentameter . syllabifyText

checkTextIambicPentameter :: Text -> [ScansionError]
checkTextIambicPentameter t = concat . zipWith (\l -> map (shiftScansionError l)) llength . map checkTextIambicPentameterLine $ lines
  where lines = Text.lines t
        llength = unfoldr uf (0, lines)
        uf (i, l:ls) = Just (i, (i + Text.length l + 1, ls))
        uf (i, []) = Nothing


