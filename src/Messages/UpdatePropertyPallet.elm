module Messages.UpdatePropertyPallet exposing (..)

import Color exposing (Color)


type UpdatePropertyPalletState
    = UpdateFillColor Color
    | UpdateStrokeColor Color
    | StrokeWidth (Result String Float)
