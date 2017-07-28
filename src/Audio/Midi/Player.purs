module Audio.Midi.Player
  (MelodySource(..), State, Event (SetRecording, SetAbc), PlaybackState(..), initialState, foldp, view) where

import Prelude ((&&), (==))
import Data.Midi (Recording) as Midi
import Data.Abc (AbcTune)
import Data.Abc.Midi (toMidi)
import Audio.BasePlayer as BasePlayer
-- import Data.Either (Either(..))
import Data.Function (($), (#), (<<<))
import Data.Array (null)
import Audio.SoundFont (AUDIO)
import Audio.Midi.HybridPerformance (toPerformance)
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.HTML (HTML, mapEvent)

-- | experiment with re-export
type PlaybackState = BasePlayer.PlaybackState

data MelodySource =
    MIDI Midi.Recording
  | ABC AbcTune
  | ABSENT

data Event =
    SetRecording Midi.Recording         -- we'll set the melody from the MIDI Recording
  | SetAbc AbcTune                      -- we'll set it from an ABC tune
  | BasePlayerEvent BasePlayer.Event

type State =
  { melodySource :: MelodySource
  , basePlayer :: BasePlayer.State
  }

initialState :: State
initialState =
  { melodySource : ABSENT
  , basePlayer : BasePlayer.initialState
  }

foldp :: ∀ fx. Event -> State -> EffModel State Event (au :: AUDIO | fx)
foldp (SetRecording recording) state =
  noEffects $ setMidiRecording recording state
foldp (SetAbc abcTune) state =
  noEffects $ setAbcTune abcTune state
foldp (BasePlayerEvent e) state =
  let
    -- establish the melody only when the Play button is first pressed
    newState =
      case e of
        BasePlayer.PlayMelody playbackState ->
          if (playbackState == BasePlayer.PLAYING) && (null state.basePlayer.melody) then
            establishMelody state
          else
            state
        _ -> state
  in
    delegate e newState

-- | set a MIDI recording as the source of the melody
setMidiRecording :: Midi.Recording -> State -> State
setMidiRecording recording state =
  let
    bpState = BasePlayer.setMelody [] state.basePlayer
  in
    state { melodySource = MIDI recording, basePlayer = bpState }


-- | set an ABC tune as the source of the melody
setAbcTune :: AbcTune -> State -> State
setAbcTune abcTune state =
  let
    bpState = BasePlayer.setMelody [] state.basePlayer
  in
    state { melodySource = ABC abcTune, basePlayer = bpState }

-- | delegate to the Base Player
delegate :: ∀ fx. BasePlayer.Event -> State -> EffModel State Event (au :: AUDIO | fx)
delegate e state =
  BasePlayer.foldp e state.basePlayer
    # mapEffects BasePlayerEvent
    # mapState \pst -> state { basePlayer = pst }

-- | establish the Base Player melody from the Midi (if we have it)
establishMelody :: State -> State
establishMelody state =
  let
    melody =
      case state.melodySource of
        MIDI recording ->
          toPerformance recording
        ABC abcTune ->
          (toPerformance <<< toMidi) abcTune
        _ ->
         []
    bpState = BasePlayer.setMelody melody state.basePlayer
  in
    state { basePlayer = bpState }

view :: State -> HTML Event
view state =
  mapEvent BasePlayerEvent $ BasePlayer.view state.basePlayer
