{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Applicative
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"


fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams s path = map ncString . filter (flip S.member $ perms) <$> words'
  where words' = map NoCaseString <$> lines <$> readFile path
        perms :: S.Set NoCaseString
        perms = S.fromList . hlist . map NoCaseString $ permutations s

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Ord NoCaseString where
  compare = compare `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
