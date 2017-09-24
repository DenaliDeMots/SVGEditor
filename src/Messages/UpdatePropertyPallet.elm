module Messages.UpdatePropertyPallet exposing (..)

--Message suptype for Property Pallet update messages

import Color exposing (Color)


type UpdatePropertyPalletState
    = UpdateFillColor Color
    | UpdateStrokeColor Color
    | StrokeWidth (Result String Float)
