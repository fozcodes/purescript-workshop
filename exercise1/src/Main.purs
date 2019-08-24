module Main where

import Prelude hiding (div)

import CSS (CSS, backgroundColor, margin, marginBottom, padding, px, rgb)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (length, zip, (..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(Just, Nothing))
import Pux (EffModel, start)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Pux.Renderer.React (renderToDOM)
import Signal (constant)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML (div, h1, a, span, ol, li)
import Text.Smolder.Markup (text, (!), attribute)

import HackerNewsApi (Story, hackerNewsStories)

data Event
  = LoadFrontPage
  | SetStories (Array Story)

type State = { stories :: Array Story }

initialState :: State
initialState = { stories: [] }

foldp :: forall eff. Event -> State -> EffModel State Event (console :: CONSOLE | eff)
foldp event@(LoadFrontPage) state = 
  { state
  , effects: [loadHackerNewsStories] }
foldp event@(SetStories stories) state =
  { state: state { stories = stories }
  , effects: [] }

loadHackerNewsStories :: forall e. Aff (console :: CONSOLE | e) (Maybe Event)
loadHackerNewsStories = do
  pure $ Just (SetStories hackerNewsStories)

view :: State -> HTML Event
view {stories} =
  div do
    h1 ! style headerStyle $ do
      text "Hacker Reader"
    div ! style contentStyle $ do
      div $ for_ storiesWithRank {storyItem, rank}
  where
    headerStyle :: CSS
    headerStyle = do
      backgroundColor (rgb 255 102 0)
      margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)
      padding (px 10.0) (px 10.0) (px 10.0) (px 10.0)

    contentStyle :: CSS
    contentStyle = do
      padding (px 10.0) (px 10.0) (px 10.0) (px 10.0)

    storiesWithRank = zip (1 .. (length stories + 1)) stories

storyItem :: Story Int -> HTML Event
storyItem story _ =
  ol $ do
    li ! style (marginBottom (px 5.0)) $ do
      div $ text story.objectID
      div $ do
        a ! attribute "href" story.url $ text (story.title)
      div $ text ("By " <> story.author)
      div $ do
        span $ text ("Points: " <> show story.points)

main :: forall eff. Eff (channel :: CHANNEL, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
main = do
  app <- start
    { initialState
    , view
    , foldp
    , inputs: [constant LoadFrontPage]
    }
  renderToDOM "#app" app.markup app.input
