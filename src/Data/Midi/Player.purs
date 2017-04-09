module Data.Midi.Player where

import CSS.TextAlign
import Data.Midi as Midi
import Audio.SoundFont (AUDIO, playNote)
import CSS (color, fromString)
import CSS.Background (background, backgroundImages)
import CSS.Border (border, borderRadius, solid)
import CSS.Box (boxShadow)
import CSS.Color (rgb, rgba)
import CSS.Display (display, float, floatLeft, inlineBlock, position, relative)
import CSS.Geometry (width, height, padding, margin)
import CSS.Overflow (hidden, overflow)
import CSS.Size (px)
import Control.Monad.Aff (later')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Int (toNumber)
import Data.List (List(..), head, index, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Prelude (bind, const, negate, not, show, pure, ($), (>), (+), (*), (/))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (div, input, progress)
import Text.Smolder.HTML.Attributes (type', max, src, value)
import Text.Smolder.Markup (Attribute, text, (#!), (!))

-- import Debug.Trace (trace)

data Event
  = NoOp
  | SetRecording Midi.Recording
  | StepMidi             -- not called directly but its presence allows a view update
  | PlayMidi Boolean     -- play | pause
  | StopMidi             -- stop and set index to zero

type State =
  { recording :: Midi.Recording
  , playing :: Boolean
  , eventMax :: Int
  , eventIndex :: Int
  , midiEvent :: Maybe Midi.Event
  , ticksPerBeat :: Int
  , tempo :: Int
  }

setState :: Midi.Recording -> State
setState recording =
  let
    max = maxMidiEvents recording
  in
    { recording : recording
    , playing : false
    , eventIndex : 0
    , eventMax : max
    , midiEvent : Nothing
    , ticksPerBeat : 480
    , tempo : 1000000   -- this will almost certainly be reset at the start of the MIDI file
    }

foldp :: ∀ fx. Event -> State -> EffModel State Event (au :: AUDIO | fx)
foldp NoOp state =  noEffects $ state
foldp (SetRecording recording) state =
  step $ setState recording
foldp StepMidi state =  step state
foldp (PlayMidi playing) state =
  step $ state { playing = playing }
foldp (StopMidi) state =
  noEffects $ state { eventIndex = 0
                    , playing = false }

-- | count the number of MIDI events in the recording
maxMidiEvents :: Midi.Recording -> Int
maxMidiEvents recording =
  let
    track0 = fromMaybe (Midi.Track Nil) (head (unwrap recording).tracks)
    trackLength (Midi.Track track) = length track
  in
    trackLength track0

-- | step through the MIDI events, one by one
step :: forall e. State -> EffModel State Event (au :: AUDIO | e)
step state =
  case locateNextMessage state of
    Just (Midi.Message ticks midiEvent) ->
      let
        -- listen for tempo changes
        tempo =
          case midiEvent of
            Midi.Tempo t ->
              t
            _ ->
              state.tempo
        -- set the new state
        newState =
          state { eventIndex = state.eventIndex + 1
                , midiEvent = Just midiEvent
                , tempo = tempo
                }
        -- work out the delay for this message
        delay =
          (ticks * tempo)  / (newState.ticksPerBeat * 1000)
      in
        { state: newState
        , effects:
          [ do
              done <-
                if (ticks > 0) then
                  later' delay $ liftEff (playEvent midiEvent)
                else
                  liftEff (playEvent midiEvent)
              pure (Just StepMidi)
          ]
        }
    _ ->
      noEffects state

-- | play a MIDI event
-- | only NoteOn events with a nominated pitch produce sound
playEvent :: forall eff. Midi.Event -> Eff (au :: AUDIO | eff) Number
playEvent event =
  case event of
    Midi.NoteOn channel pitch velocity ->
      if (pitch > 0) then
        let
          maxVolume = 127
          gain =
            toNumber velocity / toNumber maxVolume
          midiNote = { id: pitch, timeOffset: 0.0, duration : 1.0, gain : gain }
        in
          playNote midiNote
      else
        -- a pitch of 0 is interpreted simply as a rest
        pure 0.0
    _ ->
      pure 0.0

-- | locate the next message in the MIDI track
locateNextMessage :: State -> Maybe Midi.Message
locateNextMessage state =
  if (not state.playing) then
    Nothing
  else
    case state.recording of
      Midi.Recording {header: _, tracks: ts } ->
        case head ts of
          Just (Midi.Track events) ->
            index events (state.eventIndex)
          _ -> Nothing


-- | just display the next MIDI event
viewNextEvent :: State -> String
viewNextEvent state =
  case state.midiEvent of
    Nothing ->
      ""
    Just me ->
      show me

{-
view :: State -> HTML Event
view state =
  if true then
    div do
      player state
  else
    p $ text ""
-}
view :: State -> HTML Event
view state =
  player state

player :: State -> HTML Event
player state =
  let
    sliderPos = show state.eventIndex

    startImg = "assets/images/play.png"
    stopImg =  "assets/images/stop.png"
    pauseImg = "assets/images/pause.png"
    playAction =
      if state.playing then
         PlayMidi false
      else
         PlayMidi true
    playButtonImg =
      if state.playing then
        pauseImg
      else
        startImg
    capsuleMax =
      show state.eventMax
  in
        div ! playerBlockStyle $ do
          div ! playerBaseStyle ! playerStyle $ do
            progress ! capsuleStyle ! max capsuleMax ! value sliderPos $ do
              text ""
            div ! buttonStyle $ do
              input ! type' "image" ! src playButtonImg
                 #! onClick (const playAction)
              input ! type' "image" ! src stopImg
                 #! onClick (const StopMidi)

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center

-- | the capsule is the bit in the centre of the player widget that shows progress
-- | through the recording
capsuleStyle :: Attribute
capsuleStyle =
  style do
    border solid (px 1.0) (rgb 0 0 0)
    margin (px 8.0) (px 0.0) (px 8.0) (px 0.0) -- ??
    borderRadius (px 5.0) (px 5.0) (px 5.0) (px 5.0)
    -- backgroundColor (rgb 0 0 0)
    background (rgb 0 0 0)
    backgroundImages
      [ fromString "-webkit-gradient(linear, left top, left bottom, color-stop(1, rgba(0,0,0,0.5)), color-stop(0, #333))"
      , fromString "-webkit-linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-moz-linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-ms-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-o-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      ]
    boxShadow (px 0.0) (px 0.0) (px 0.0) (rgb 5 5 5)
    overflow hidden
    display inlineBlock
    width (px 220.0)
    height (px 20.0)


-- | the basic style of the outline of the player which surrounds
-- | both the buttons and the capsule
playerBlockStyle :: Attribute
playerBlockStyle =
  style do
    background (rgba 0 0 0 0.7)
    border solid (px 1.0) (rgb 0 0 0)
    borderRadius (px 10.0) (px 10.0) (px 10.0) (px 10.0)
    width (px 330.0)
    position relative  -- "relative; z-index: 2"

-- the style of the player
playerStyle :: Attribute
playerStyle =
  style do
    height (px 36.0)
    boxShadow (px (-1.0)) (px (-1.0)) (px (-1.0))  (rgb 0 0 0)
    borderRadius (px 10.0) (px 10.0) (px 10.0) (px 10.0)

-- more player style attributes
playerBaseStyle :: Attribute
playerBaseStyle =
  style do
    border solid (px 1.0) (rgb 0 0 0)
    backgroundImages
      [ fromString "-webkit-gradient(linear,left top,left bottom,from(rgba(66,66,66,1)),to(rgba(22,22,22,1)))"
      , fromString "-webkit-linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-moz-linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-ms-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-o-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      ]
    boxShadow (px 0.0) (px 0.0) (px 10.0) (rgb 15 15 15) -- #fff
    borderRadius (px 10.0) (px 10.0) (px 10.0) (px 10.0)
    padding (px 15.0) (px 20.0) (px 15.0) (px 20.0)
    color (rgba 255 255 255 0.8)
    -- "text-shadow", "1px 1px 2px #000"  ???

-- player button styling
buttonStyle :: Attribute
buttonStyle =
  style do
    width (px 80.0)
    margin (px 2.0) (px 3.0) (px 0.0) (px 3.0)
    float floatLeft
    -- opacity 0.7
