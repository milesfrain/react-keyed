module Example where

import Prelude
import Data.Array ((..))
import Effect (Effect)
import Effect.Class.Console (log)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, component, useState, (/\))
import React.Basic.Hooks as React

data HoverInfo
  = HoverRow Int
  | NoHover

mkExample :: Effect (ReactComponent {})
mkExample = do
  component "Example" \_ -> React.do
    hover /\ setHover <- useState NoHover
    entries /\ setEntries <- useState $ 1 .. 1000
    let
      hovTxt = case hover of
        HoverRow i -> show i
        NoHover -> "Not hovering"

      mkRow idx =
        R.tr
          { onMouseEnter:
              handler_ do
                log $ show idx
                setHover \_ -> HoverRow idx
          , children:
              map
                (\str -> R.td_ [ R.text str ])
                [ show idx, show $ idx * 10 ]
          }
    pure
      $ R.div
          { className: "col"
          , children:
              [ R.div_ [ R.text hovTxt ]
              , R.table
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
                                (setHover \_ -> NoHover)
                          --log $ "exit"
                          , children: map mkRow entries
                          }
                      ]
                  }
              ]
          }
