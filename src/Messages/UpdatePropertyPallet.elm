module Messages.UpdatePropertyPallet exposing (..)

import Color exposing (Color)
import ColorPicker


type UpdatePropertyPalletState
    = UpdateColor ColorPicker.Msg
    | StrokeWidth (Result String Float)
