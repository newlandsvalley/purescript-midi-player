module App where

import CSS.TextAlign
import Data.Midi as Midi
import Data.Midi.Player as MidiPlayer
import Audio.SoundFont (AUDIO, loadRemoteSoundFont)
import BinaryFileIO.FileIO (FILEIO, Filespec, loadBinaryFile)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Midi.Parser (parse, normalise, translateRunningStatus)
import Prelude (bind, show, pure, (<>), (<<<))
import Data.Function (const, ($), (#))
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (onClick, onChange)
import Pux.DOM.HTML (HTML, child)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (button, div, h1, input)
import Text.Smolder.HTML.Attributes (type', id, accept)
import Text.Smolder.Markup (Attribute, text, (#!), (!))

-- import Debug.Trace (trace)

data Event
  = NoOp
  | RequestLoadFonts
  | FontLoaded Boolean
  | RequestFileUpload
  | FileLoaded Filespec
  | PlayerEvent MidiPlayer.Event

type State =
  { filespec :: Maybe Filespec
  , recording :: Either String Midi.Recording
  , fontLoaded :: Boolean                 -- is the soundfount loaded?
  , playerState :: MidiPlayer.State
  }

initialState :: State
initialState =
  { filespec : Nothing
  , recording : Left "not started"
  , fontLoaded : false
  , playerState : MidiPlayer.initialState 0
  }

-- foldp :: Event -> State -> EffModel State Action
foldp :: ∀ fx. Event -> State -> EffModel State Event (fileio :: FILEIO, au :: AUDIO | fx)
foldp NoOp state =  noEffects $ state
foldp RequestLoadFonts state =
 { state: state
   , effects:
     [ do
         loaded <- loadRemoteSoundFont "acoustic_grand_piano"
         pure $ Just (FontLoaded loaded)
     ]
  }
foldp (FontLoaded loaded) state =
  noEffects $ state { fontLoaded = loaded }
foldp RequestFileUpload state =
 { state: state
   , effects:
     [ do
         filespec <- loadBinaryFile
         pure $ Just (FileLoaded filespec)
     ]
 }
foldp (FileLoaded filespec) state =
  let
    newState = processFile filespec state
  in
    { state : newState
    , effects :
      [ do
          pure $ Just (PlayerEvent (MidiPlayer.SetRecording newState.recording))
      ]
    }
foldp (PlayerEvent e) state =
  MidiPlayer.foldp e state.playerState
    # mapEffects PlayerEvent
    # mapState \pst -> state { playerState = pst }


processFile :: Filespec -> State -> State
processFile filespec state =
  let
    recording = (translateRunningStatus <<< parse <<< normalise) filespec.contents
  in
    state { filespec =  Just filespec
          , recording = recording
          }

fullParse :: String -> String
fullParse s =
  case translateRunningStatus $ parse $ normalise $ s of
    Left err ->
      ("Parse error:" <> err)
    Right midi ->
      (show midi)

view :: State -> HTML Event
view state =
  if (state.fontLoaded) then
     div  do
       h1 ! centreStyle $ text "play a MIDI file"
       div do
         input ! type' "file" ! id "fileinput" ! accept ".midi"
           #! onChange (const RequestFileUpload)
         child PlayerEvent MidiPlayer.view $ state.playerState

  else
    button #! onClick (const RequestLoadFonts) $ text "load soundfonts"

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center
