module Audio.Midi.Player
  (State, Event (SetRecording), initialState, foldp, view) where

import Prelude ((&&), (==))
import Data.Midi (Recording) as Midi
import Audio.BasePlayer as BasePlayer
import Data.Either (Either(..))
import Data.Function (($), (#))
import Data.Array (null)
import Audio.SoundFont (AUDIO)
import Audio.Midi.HybridPerformance (toPerformance)
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.HTML (HTML, mapEvent)

data Event
  = SetRecording Midi.Recording         -- we'll set the melody from the MIDI Recording
  | BasePlayerEvent BasePlayer.Event

type State =
  { recording :: Either String Midi.Recording
  , basePlayerState :: BasePlayer.State
  }

initialState :: State
initialState =
  { recording : Left "not started"
  , basePlayerState : BasePlayer.initialState
  }

foldp :: ∀ fx. Event -> State -> EffModel State Event (au :: AUDIO | fx)
foldp (SetRecording recording) state =
  let
    bpState = BasePlayer.setMelody [] state.basePlayerState
  in
    noEffects $ state { recording = Right recording, basePlayerState = bpState }
foldp (BasePlayerEvent e) state =
  let
    -- establish the melody only when the Play button is first pressed
    newState =
      case e of
        BasePlayer.PlayMelody playbackState ->
          if (playbackState == BasePlayer.PLAYING) && (null state.basePlayerState.melody) then
            establishMelody state
          else
            state
        _ -> state
  in
    delegate e newState

-- | delegate to the Base Player
delegate :: ∀ fx. BasePlayer.Event -> State -> EffModel State Event (au :: AUDIO | fx)
delegate e state =
  BasePlayer.foldp e state.basePlayerState
    # mapEffects BasePlayerEvent
    # mapState \pst -> state { basePlayerState = pst }

-- | establish the Base Player melody from the Midi (if we have it)
establishMelody :: State -> State
establishMelody state =
  let
    melody =
      case state.recording of
        Right rec ->
         toPerformance rec
        _ ->
         []
    bpState = BasePlayer.setMelody melody state.basePlayerState
  in
    state { basePlayerState = bpState }

view :: State -> HTML Event
view state =
  mapEvent BasePlayerEvent $ BasePlayer.view state.basePlayerState
