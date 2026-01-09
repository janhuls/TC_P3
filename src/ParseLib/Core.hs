-- The code in this file provides a simple parsing library,
-- similar to the one in the lectures.

-- This package exports a minimal 'core' set of parser combinators,
-- which can't be easily and efficiently implemented "in user-space".

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

{-# LANGUAGE LambdaCase #-}
module ParseLib.Core
  (
    -- * The type of parsers
    Parser(),
    -- * Elementary parsers
    anySymbol,
    empty, failp,
    succeed, pure,
    -- * Parser combinators
    (<<|>),
    -- * Running parsers
    parse,
    look
  )
  where

import Data.Char
import Data.Traversable
import Data.Maybe
import Control.Monad
import Control.Applicative

-- | An input string is mapped to a list of successful parses.
-- For each succesful parse, we return the result of type 'r',
-- and the remaining input string. The input must be a list of
-- symbols.
newtype Parser s r  =  Parser { runParser :: [s] -> [(r,[s])] }

instance Functor (Parser s) where
  fmap f p  =  Parser (\xs -> [(f y, ys)
                              | (y, ys) <- runParser p xs])

instance Applicative (Parser s) where
  pure      =  succeed 
  p <*> q   =  Parser (\xs -> [(f x,zs)
                              |(f  ,ys) <- runParser p xs
                              ,(  x,zs) <- runParser q ys])

instance Alternative (Parser s) where
  empty     = failp
  p <|> q   =  Parser (\xs -> runParser p xs <> runParser q xs)

infixr 3 <<|>

-- | Biased choice. If the left hand side parser succeeds,
-- the right hand side is not considered. Use with care!
(<<|>) :: Parser s a -> Parser s a -> Parser s a
p <<|> q  =  Parser (\xs -> let r = runParser p xs in if null r then runParser q xs else r)

instance Monad (Parser s) where
  return    =  pure
  p >>= f   =  Parser (\xs -> [(z  ,zs)
                              |(y  ,ys) <- runParser p xs
                              ,(z  ,zs) <- runParser (f y) ys
                              ])

instance MonadPlus (Parser s) where
  mzero     =  empty
  mplus     =  (<|>)

-- | Parses any single symbol.
anySymbol :: Parser s s
anySymbol = Parser (\case
  (x:xs) -> [(x,xs)]
  [] -> [])

-- | Parser that always succeeds, i.e., for epsilon.
succeed :: a -> Parser s a
succeed r = Parser (\xs -> [(r,xs)])

-- | Same as 'empty'; provided for compatibility with the lecture notes.
failp :: Parser s a
failp = Parser (const [])

-- | Runs a parser on a given string.
parse :: Parser s a -> [s] -> [(a,[s])]
parse = runParser

-- | Returns the rest of the input without consuming anything.
look :: Parser s [s]
look = Parser (\xs -> [(xs, xs)])
