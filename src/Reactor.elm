module Reactor exposing (..)

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
