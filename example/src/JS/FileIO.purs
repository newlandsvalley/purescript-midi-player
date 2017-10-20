module JS.FileIO
  ( FILEIO
  , Filespec
  , loadTextFile
  , loadBinaryFileAsText
  , saveTextFile ) where

import Prelude (Unit, (<<<))
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)

-- | the result of a file load
type Filespec = {
    contents :: String
  , name :: String
  }

-- | File IO Effect
foreign import data FILEIO :: Effect

foreign import loadTextFileImpl
  :: ∀ eff. String -> EffFnAff (fileio :: FILEIO | eff) Filespec

-- | load a text file from a DOM element with the supplied id
loadTextFile :: ∀ eff. String -> Aff (fileio :: FILEIO | eff) Filespec
loadTextFile =
  fromEffFnAff <<< loadTextFileImpl

foreign import loadBinaryFileAsTextImpl
  :: ∀ eff. String -> EffFnAff (fileio :: FILEIO | eff) Filespec

-- | load a binary file from a DOM element with the supplied id
-- | and tunnel it as a binary string
loadBinaryFileAsText :: ∀ eff. String -> Aff (fileio :: FILEIO | eff) Filespec
loadBinaryFileAsText =
  fromEffFnAff <<< loadBinaryFileAsTextImpl

-- | save a text file
foreign import saveTextFile :: ∀ eff. Filespec -> Eff (fileio :: FILEIO | eff) Unit
