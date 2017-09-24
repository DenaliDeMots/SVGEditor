module Reactor exposing (..)

--This is an alternative version of Main that gives predefined values for the start flags
--This is the module that runs the program in Elm Reactor

import Html
import Main


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


flags =
    { width = 1536
    , height = 759
    }


init =
    Main.init flags


update =
    Main.update


view =
    Main.view


subscriptions =
    Main.subscriptions
