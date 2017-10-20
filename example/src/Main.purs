module Main where

import App (foldp, initialState, view)
import Audio.SoundFont (AUDIO)
import JS.FileIO (FILEIO)
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Network.HTTP.Affjax (AJAX)

-- | Start and render the app
main :: ∀ fx. Eff (CoreEffects (ajax :: AJAX, fileio :: FILEIO, au :: AUDIO | fx)) Unit
main = do
  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
