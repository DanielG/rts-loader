-- |
-- Module      :  GHC.Way
-- Copyright   :  (c) The University of Glasgow 2005
--                Copyright (C) 2016  Daniel Gröber
-- License     :  BSD3
-- Stability   :  unknown, ask the GHC guys I guess ;)
-- Portability :  portable
--
-- Stolen from GHC's 'DynFlags' module.
--

module GHC.Way where

import Data.List

-- The Glasgow Haskell Compiler License

-- Copyright 2002, The University Court of the University of Glasgow.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- - Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- - Redistributions in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.
--
-- - Neither name of the University nor the names of its contributors may be
-- used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
-- GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
-- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
-- FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
-- DAMAGE.

-- | GHC library way, see <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/CompilerWays GHC Wiki/Commentary/Rts/CompilerWays>
data Way
  = WayCustom String -- for GHC API clients building custom variants
  | WayThreaded
  | WayDebug
  | WayProf
  | WayEventLog
  | WayDyn
  deriving (Eq, Ord, Show)

allowed_way_combination :: [Way] -> Bool
allowed_way_combination way = and [ x `allowedWith` y
                                  | x <- way, y <- way, x < y ]
  where
        -- Note ordering in these tests: the left argument is
        -- <= the right argument, according to the Ord instance
        -- on Way above.

        -- dyn is allowed with everything
        _ `allowedWith` WayDyn                  = True
        WayDyn `allowedWith` _                  = True

        -- debug is allowed with everything
        _ `allowedWith` WayDebug                = True
        WayDebug `allowedWith` _                = True

        (WayCustom {}) `allowedWith` _          = True
        WayThreaded `allowedWith` WayProf       = True
        WayThreaded `allowedWith` WayEventLog   = True
        _ `allowedWith` _                       = False

mkBuildTag :: [Way] -> String
mkBuildTag ways = concat (intersperse "_" (map wayTag ways))

wayTag :: Way -> String
wayTag (WayCustom xs) = xs
wayTag WayThreaded = "thr"
wayTag WayDebug    = "debug"
wayTag WayDyn      = "dyn"
wayTag WayProf     = "p"
wayTag WayEventLog = "l"

wayRTSOnly :: Way -> Bool
wayRTSOnly (WayCustom {}) = False
wayRTSOnly WayThreaded = True
wayRTSOnly WayDebug    = True
wayRTSOnly WayDyn      = False
wayRTSOnly WayProf     = False
wayRTSOnly WayEventLog = True
