-- The code in this file provides a simple parsing library,
-- similar to the one in the lectures.

-- This package exports parser combinators 'derived' (i.e. constructed)
-- exclusively from the other parser combinators, for your convenience.

-- History:
--  This file is based on code vendored (in Ocober 2025) from
--  https://git.science.uu.nl/b3tc/uu-tc, an un-packaged fork of the
--  uu-tc package on Hackage, which is abandoned since 2018.

-- The code at https://git.science.uu.nl/b3tc/uu-tc contained the LICENSE file:
--  Copyright (c) 2009 Andres Loeh
--  All rights reserved.
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--  1. Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in the
--     documentation and/or other materials provided with the distribution.
--  3. Neither the name of the author nor the names of his contributors
--     may be used to endorse or promote products derived from this software
--     without specific prior written permission.
--  THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND
--  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
--  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
--  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
--  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
--  SUCH DAMAGE.

module ParseLib.Derived
  (
    module ParseLib.Core,
    -- * Derived combinators
    (<$),
    (<*),
    (*>),
    epsilon,
    symbol,
    token,
    pack,
    choice,
    guard,
    satisfy,
    -- * EBNF parser combinators 
    option,
    many,
    some, many1,
    listOf,
    -- * Chain expression combinators
    chainr,
    chainl,
    -- * Greedy parsers
    greedy,
    greedy1,
    -- * End of input
    eof,
    -- * Applications of elementary parsers
    digit,
    newdigit,
    natural,
    integer,
    identifier,
    parenthesised,
    bracketed,
    braced,
    commaList,
    semiList
  )
  where

import ParseLib.Core
import Control.Applicative
import qualified Data.Char as Char
import Data.Function

-- | Parser for epsilon that does return '()'.
epsilon :: Parser s ()
epsilon = succeed ()

-- | Parses a specific given symbol.
symbol :: Eq s  => s -> Parser s s
symbol x = satisfy (==x)

-- | Parses a specific given sequence of symbols.
token :: Eq s => [s] -> Parser s [s]
token = traverse symbol

-- | Takes three parsers: a delimiter, the parser for the
-- content, and another delimiter. Constructs a sequence of
-- the three, but returns only the result of the enclosed
-- parser.
pack :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack p r q  =  p *> r <* q

-- | Takes a list of parsers and combines them using
-- choice.
choice :: [Parser s a] -> Parser s a
choice = foldr (<|>) empty

-- | Parses an optional element. Takes the default value
-- as its second argument.
option :: Parser s a -> a -> Parser s a
option p d = p <|> succeed d

-- | Same as 'some'. Provided for compatibility with
-- the lecture notes. 'many' and 'some' are already
-- defined (for any Alternative) in Control.Applicative
many1 :: Parser s a -> Parser s [a]
many1 = some

-- | Takes a parser @p@ and a separator parser @s@. Parses
-- a sequence of @p@s that is separated by @s@s.
listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p s = (:) <$> p <*> many (s *> p)

-- | Takes a parser @pe@ and an operator parser @po@. Parses
-- a sequence of @pe@s separated by @po@s. The results are
-- combined using the operator associated with @po@ in a
-- right-associative way.
chainr  ::  Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr pe pop = do
  fs <- many $ do
    e1 <- pe
    op <- pop
    pure $ \e2 -> e1 `op` e2
  e <- pe
  pure $ foldr ($) e fs

-- | Takes a parser @pe@ and an operator parser @po@. Parses
-- a sequence of @pe@s separated by @po@s. The results are
-- combined using the operator associated with @po@ in a
-- left-associative way.
chainl  ::  Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl pe pop  = do
  e <- pe
  fs <- many $ do
    op <- pop
    e2 <- pe
    pure $ \e1 -> e1 `op` e2
  pure $ foldl (&) e fs

-- | Greedy variant of 'many'.
greedy :: Parser s b -> Parser s [b]
greedy p = (:) <$> p <*> greedy p <<|> succeed []

-- | Greedy variant of 'many1'.
greedy1 :: Parser s b -> Parser s [b]
greedy1 p = (:) <$> p <*> greedy p

-- | Succeeds only on the end of the input.
eof :: Parser s ()
eof = look >>= \ xs -> if null xs then succeed () else failp

-- | Succeeds only if the predicate holds on the result.
guard :: (a -> Bool) -> Parser s a -> Parser s a
guard c p = p >>= \a -> if c a then pure a else failp

-- | Takes a predicate and returns a parser that parses a
-- single symbol satisfying that predicate.
satisfy :: (s -> Bool) -> Parser s s
satisfy p = guard p anySymbol

digit  :: Parser Char Char
digit  =  satisfy Char.isDigit

newdigit :: Parser Char Int
newdigit = read . (:[]) <$> digit

natural :: Parser Char Int
natural = foldl (\a b -> a * 10 + b) 0 <$> many1 newdigit

integer :: Parser Char Int
integer = option (negate <$ symbol '-') id  <*>  natural

identifier :: Parser Char String
identifier = (:) <$> satisfy Char.isAlpha <*> greedy (satisfy Char.isAlphaNum)

parenthesised :: Parser Char a -> Parser Char a
parenthesised p = pack (symbol '(') p (symbol ')')

bracketed :: Parser Char a -> Parser Char a
bracketed p = pack (symbol '[') p (symbol ']')

braced :: Parser Char a -> Parser Char a
braced p = pack (symbol '{') p (symbol '}')

commaList :: Parser Char a -> Parser Char [a]
commaList p = listOf p (symbol ',')

semiList :: Parser Char a -> Parser Char [a]
semiList p = listOf p (symbol ';')
