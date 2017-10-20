module App where

import CSS.TextAlign
import Data.Midi as Midi
import Data.Midi.Instrument (InstrumentName(..))
import Audio.Midi.Player as MidiPlayer
import Audio.SoundFont (AUDIO, Instrument, loadRemoteSoundFonts)
import JS.FileIO (FILEIO, Filespec, loadBinaryFileAsText)
import Data.Either (Either(..))
import Data.Function (const, ($), (#))
import Data.Maybe (Maybe(..))
import Data.Array (singleton)
import Data.Midi.Parser (parse, normalise, translateRunningStatus)
import Prelude (bind, discard, pure, (<<<), (<>))
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (onClick, onChange)
import Pux.DOM.HTML (HTML, child)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (button, div, h1, input, p)
import Text.Smolder.HTML.Attributes (type', id, accept)
import Text.Smolder.Markup (Attribute, text, (#!), (!))
import Network.HTTP.Affjax (AJAX)

-- import Debug.Trace (trace)

data Event
  = NoOp
  | RequestLoadFonts
  | FontsLoaded (Array Instrument)
  | RequestFileUpload
  | FileLoaded Filespec
  | PlayerEvent MidiPlayer.Event

type State =
  { filespec :: Maybe Filespec
  , recording :: Either String Midi.Recording
  , playerState :: MidiPlayer.State
  }

initialState :: State
initialState =
  { filespec : Nothing
  , recording : Left "not started"
  , playerState : MidiPlayer.initialState []
  }

foldp :: ∀ fx. Event -> State -> EffModel State Event (ajax :: AJAX, fileio :: FILEIO, au :: AUDIO | fx)
foldp NoOp state =  noEffects $ state
foldp RequestLoadFonts state =
 { state: state
   , effects:
     [ do
         instruments <- loadRemoteSoundFonts (singleton AcousticGrandPiano)
         pure $ Just (FontsLoaded instruments)
     ]
  }
foldp (FontsLoaded instruments) state =
  let
    basePlayerState = MidiPlayer.setInstruments instruments state.playerState
  in
    noEffects $ state { playerState = basePlayerState }
foldp RequestFileUpload state =
 { state: state
   , effects:
     [ do
         filespec <- loadBinaryFileAsText "fileinput"
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
      MidiPlayer.foldp e state.playerState
        # mapEffects PlayerEvent
        # mapState \pst -> state { playerState = pst }

-- | get the MIDI recording and parse it
processFile :: Filespec -> State -> State
processFile filespec state =
  state { filespec = Just filespec
        , recording = (translateRunningStatus <<< parse <<< normalise) filespec.contents
        }

-- | not ideal.  At the moment we don't catch errors from fonts that don't load
isFontLoaded :: State -> Boolean
isFontLoaded state =
  state.playerState.fontsLoaded

view :: State -> HTML Event
view state =
  if (isFontLoaded state) then
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
  case state.recording of
    Right r ->
      child PlayerEvent MidiPlayer.view $ state.playerState
    Left e ->
      p $ text ("Error in recording: "  <> e)

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center
