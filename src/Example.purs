module Example where

import Prelude
import Data.Array ((..))
import Effect (Effect)
import Effect.Class.Console (log)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, ReactComponent, component, element, memo, useEffect, useReducer, useState, (/\))
import React.Basic.Hooks as React

data HoverInfo
  = HoverRow Int
  | NoHover

data Action
  = SetHover HoverInfo

type State
  = { hover :: HoverInfo }

reducer :: State -> Action -> State
reducer state = case _ of
  SetHover h -> state { hover = h }

mkExample :: Effect (ReactComponent {})
mkExample = do
  let
    initialState = { hover: NoHover }
  table <- memo mkTable
  component "Example" \_ -> React.do
    state /\ dispatch <- useReducer initialState reducer
    let
      hovTxt = case state.hover of
        HoverRow i -> show i
        NoHover -> "Not hovering"
    pure
      $ R.div
          { className: "col"
          , children:
              [ R.div_ [ R.text hovTxt ]
              , element table { dispatch }
              ]
          }

mkRow :: (Action → Effect Unit) → Int → JSX
mkRow dispatch idx =
  R.tr
    { onMouseEnter:
        handler_ do
          dispatch $ SetHover $ HoverRow idx
          log $ show idx
    , children:
        map
          (\str -> R.td_ [ R.text str ])
          [ show idx, show $ idx * 10 ]
    }

mkTable :: Effect (ReactComponent { dispatch :: Action -> Effect Unit })
mkTable = do
  component "Table" \props -> React.do
    entries /\ setEntries <- useState $ 1 .. 1000
    pure
      $ R.table
          { className: "table table-sm table-hover"
          , children:
              [ R.thead_
                  [ R.tr_
                      $ map
                          ( \str ->
                              R.th
                                { className: "col-sm-1"
                                , children: [ R.text str ]
                                }
                          )
                          [ "id", "value" ]
                  ]
              , R.tbody
                  { onMouseLeave:
                      handler_ do
                        props.dispatch $ SetHover NoHover
                        log "exit"
                  , children: map (mkRow props.dispatch) entries
                  }
              ]
          }
