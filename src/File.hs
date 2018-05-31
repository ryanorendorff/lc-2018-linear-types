{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module File (
    funFunc,
    firstLine
    )
where

import Prelude hiding (return, (>>=), (>>))
import Prelude.Linear hiding (IO, (>>=), (>>), return, ($))

import Data.Text (Text)

import System.IO as SI
import System.IO.Resource as SIR
    
firstLine :: FilePath -> IO Text
firstLine filepath = run $ do
    f <- SIR.openFile (filepath) SI.ReadMode
    (line, f1) <- SIR.hGetLine(f)
    SIR.hClose(f1)
    SIR.return line
    where
        -- The builder here is only for using @RebindableSyntax@ in this
        -- monad.
        SIR.Builder {..} = SIR.builder

funFunc :: Int ->. Int
funFunc x = x
