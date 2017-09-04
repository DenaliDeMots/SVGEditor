module Messages.UpdatePropertyPallet exposing (..)

import Color exposing (Color)


type UpdatePropertyPalletState
    = FillColor Color
    | StrokeColor Color
    | StrokeWidth (Result String Float)
