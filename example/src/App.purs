module App where

import CSS.TextAlign
import Data.Midi as Midi
import Data.Midi.Player as MidiPlayer
import Audio.SoundFont (AUDIO, loadRemoteSoundFont)
import BinaryFileIO.FileIO (FILEIO, Filespec, loadBinaryFile)
import Data.Either (Either(..))
import Data.Function (const, ($), (#))
import Data.Maybe (Maybe(..))
import Data.Midi.Parser (parse, normalise, translateRunningStatus)
import Prelude (bind, show, pure, (<>), (<<<))
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (onClick, onChange)
import Pux.DOM.HTML (HTML, child)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (button, div, h1, input, p)
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
  , playerState :: Maybe MidiPlayer.State
  }

initialState :: State
initialState =
  { filespec : Nothing
  , recording : Left "not started"
  , fontLoaded : false
  , playerState : Nothing
  }

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
    case newState.recording of
      Right midiRecording ->
        { state : newState
        , effects :
            [ do
                pure $ Just (PlayerEvent (MidiPlayer.SetRecording midiRecording))
            ]
        }
      _ ->
        noEffects newState

foldp (PlayerEvent e) state =
  case state.playerState of
    Just pstate ->
      MidiPlayer.foldp e pstate
        # mapEffects PlayerEvent
        # mapState \pst -> state { playerState = Just pst }
    _ ->
      noEffects state

-- | this needs a bit of clearing up - we're setting player state too many times
processFile :: Filespec -> State -> State
processFile filespec state =
  let
    mrecording = (translateRunningStatus <<< parse <<< normalise) filespec.contents
  in
    case mrecording of
      Right recording ->
        state { filespec = Just filespec
              , recording = mrecording
              , playerState = Just (MidiPlayer.setState recording)
              }
      _ ->
        state { filespec =  Just filespec
              , recording = mrecording
              , playerState = Nothing
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
         viewPlayer state
  else
    button #! onClick (const RequestLoadFonts) $ text "load soundfonts"

-- | only display the player if we have a MIDI recording
viewPlayer :: State -> HTML Event
viewPlayer state =
  case state.playerState of
    Just pstate ->
      child PlayerEvent MidiPlayer.view $ pstate
    _ ->
      p $ text ""

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center
