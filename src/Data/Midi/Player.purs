module Data.Midi.Player
  (MelodySource, PlaybackState, State, Event (SetRecording, SetAbc, SetMelody), initialState, foldp, view) where

import CSS.TextAlign (center, textAlign)
import Audio.SoundFont (AUDIO, playNotes)
import CSS (color, fromString)
import CSS.Background (background, backgroundImages)
import CSS.Border (border, borderRadius, solid)
import CSS.Box (boxShadow)
import CSS.Color (rgb, rgba)
import CSS.Display (display, float, floatLeft, inlineBlock, position, relative)
import CSS.Geometry (width, height, padding, margin)
import CSS.Overflow (hidden, overflow)
import CSS.Size (px)
import Control.Monad.Aff (delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Abc (AbcTune)
import Data.Abc.Midi (toMidi)
import Data.Array (null, index, length)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Midi (Recording)
import Data.Midi.Player.HybridPerformance (Melody, MidiPhrase, toPerformance)
import Prelude (class Show, class Eq, bind, const, discard, negate, not, show, pure, (<<<), (==), (>), ($), (+), (*), (&&), (||))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (div, input, progress)
import Text.Smolder.HTML.Attributes (type', disabled, max, src, value)
import Text.Smolder.Markup (Attribute, text, (#!), (!), (!?))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

-- | Player events,  Only SetMelody is exposed.
data Event
  = NoOp
  | SetRecording Recording   -- we can get the melody from a MIDI recoding
  | SetAbc AbcTune           -- or else from an ABC tune
  | SetMelody Melody         -- or directly from a melody itself
  | StepMelody Number        -- not called directly but its presence allows a view update
  | PlayMelody PlaybackState -- play | pause
  | EnablePlayButton         -- re-enable the pause/play button (after a pause)
  | StopMelody               -- stop and set index to zero

-- | the source of the melody
data MelodySource
  = ABC AbcTune
  | MIDI Recording
  | DIRECT  -- straight from a Melody itself
  | ABSENT

-- | the internal state of the player
type State =
  { melodySource :: MelodySource
  , melody :: Melody
  , playing :: PlaybackState
  , phraseMax :: Int
  , phraseIndex :: Int
  , lastPhraseLength :: Number
  }

-- | now we have tri-state logic for playback state because of the pending status
data PlaybackState =
    PLAYING           -- the melody is playing
  | PENDINGPAUSED     -- the pause button has been hit, but the phrase is stil finishing
  | PAUSED            -- the melody is not playing  (paused or stopped)

derive instance genericPlaybackState :: Generic PlaybackState _
instance showEvent :: Show PlaybackState where
  show = genericShow
instance eqEvent :: Eq PlaybackState where
  eq = genericEq

-- | the initial state of the player (with no melody to play yet)
initialState :: State
initialState =
  { melodySource : ABSENT
  , melody : []
  , playing : PAUSED
  , phraseMax : 0
  , phraseIndex : 0
  , lastPhraseLength : 0.0
  }

-- | set the source of the melody as a MIDI recording
setMidiRecording :: Recording -> State -> State
setMidiRecording recording state =
  state { melodySource = MIDI recording
        , melody = []
        }

-- | set the source of the melody as an ABC tune
setAbcTune :: AbcTune -> State -> State
setAbcTune abcTune state =
  state { melodySource = ABC abcTune
        , melody = []
        }

-- | set the source of the melody directly as a Melody itself (needing no transformation)
setMelody :: Melody -> State -> State
setMelody melody state =
  state { melodySource = DIRECT
        , melody = melody
        }

-- | truly establish the melody only once the play button is pressed
-- | for the first time
establishMelody :: PlaybackState -> State -> State
establishMelody playing state =
    case state.melodySource of
      MIDI recording ->
        let
          melody = toPerformance recording
          max = length melody
        in
          state { melody = melody, playing = playing, phraseMax = max, phraseIndex = 0 }
      ABC abcTune ->
        let
          melody = (toPerformance <<< toMidi) abcTune
          max = length melody
        in
          state { melody = melody, playing = playing, phraseMax = max, phraseIndex = 0 }
      DIRECT ->
        -- the melody is already written directly to state
        let
          max = length state.melody
        in
          state { playing = playing, phraseMax = max, phraseIndex = 0 }
      ABSENT ->
        state

-- | the autonomous state update
foldp :: ∀ fx. Event -> State -> EffModel State Event (au :: AUDIO | fx)
foldp NoOp state =  noEffects $ state
foldp (SetRecording recording) state =
  noEffects $ setMidiRecording recording state
foldp (SetAbc abcTune) state =
  noEffects $ setAbcTune abcTune state
foldp (SetMelody melody) state =
  noEffects $ setMelody melody state
foldp (StepMelody delay) state =
  step state delay
foldp (PlayMelody playing) state =
  if ((playing == PLAYING) && state.phraseIndex == 0) then
    -- start
    -- actually establish the melody on first reference
    step (establishMelody playing state) 0.0
  else if ((playing == PLAYING) && state.phraseIndex > 0) then
    -- restart
    step (state { playing = playing }) 0.0
  else
    -- pause
    temporarilyFreezePlayButton (state { playing = PENDINGPAUSED })
foldp (StopMelody) state =
  noEffects $ state { phraseIndex = 0
                    , playing = PAUSED }
foldp EnablePlayButton state =
  noEffects $ state { playing = PAUSED }

-- | step through the MIDI events, one by one
step :: forall e. State -> Number -> EffModel State Event (au :: AUDIO | e)
step state sDelay =
  case locateNextPhrase state of
    Just (midiPhrase) ->
      let
        msDelay = sDelay * 1000.0
        -- set the new state
        newState =
          state { phraseIndex = state.phraseIndex + 1
                , lastPhraseLength = sDelay
                }
      in
        { state: newState
        , effects:
          [ do
              _ <- delay (Milliseconds msDelay)
              nextDelay <- liftEff (playEvent midiPhrase)
              pure $ Just (StepMelody nextDelay)
          ]
        }
    _ ->
      noEffects state

-- | the pause button is unresponsive.  The playback only pauses when the current
-- | phrase finishes playing. So temporarily freeze the play button.
temporarilyFreezePlayButton :: forall e. State -> EffModel State Event (au :: AUDIO | e)
temporarilyFreezePlayButton state =
  let
    msDelay = state.lastPhraseLength * 1000.0
  in
    { state: state
    , effects:
      [ do
          _ <- delay (Milliseconds msDelay)
          pure $ Just EnablePlayButton
      ]
    }

-- | play a MIDI Phrase (a bunch of MIDI notes)
-- | only NoteOn events produce sound
playEvent :: forall eff. MidiPhrase -> Eff (au :: AUDIO | eff) Number
playEvent midiPhrase =
  playNotes midiPhrase

-- | locate the next MIDI phrase from the performance
locateNextPhrase :: State -> Maybe MidiPhrase
locateNextPhrase state =
  if (not (state.playing == PLAYING)) || (null state.melody) then
    Nothing
  else
    index state.melody (state.phraseIndex)

-- | the player widget (start, stop, pause, progress)
view :: State -> HTML Event
view state =
  player state

player :: State -> HTML Event
player state =
  let
    sliderPos = show state.phraseIndex
    -- the play button is temporarily disabled after a pause command
    isDisabled = (state.playing == PENDINGPAUSED)

    startImg = "assets/images/play.png"
    stopImg =  "assets/images/stop.png"
    pauseImg = "assets/images/pause.png"
    -- the action reverses the PLAYING - PAUSED status
    playAction =
      if (state.playing == PLAYING) then
         PlayMelody PAUSED
      else
         PlayMelody PLAYING
    playButtonImg =
      if (state.playing == PAUSED) then
        startImg
      else
        -- the pause image is displayed both if the tune is to be PlayMelody
        -- or else if it is disabled pending a pause
        pauseImg
    capsuleMax =
      show state.phraseMax
  in
        div ! playerBlockStyle $ do
          div ! playerBaseStyle ! playerStyle $ do
            progress ! capsuleStyle ! max capsuleMax ! value sliderPos $ do
              text ""
            div ! buttonStyle $ do
              -- input ! type' "image" ! src playButtonImg
              (input !? isDisabled) (disabled "disabled") ! type' "image" ! src playButtonImg
                 #! onClick (const playAction)
              input ! type' "image" ! src stopImg
                 #! onClick (const StopMelody)

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
    margin (px 10.0) (px 0.0) (px 10.0) (px 0.0)
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
