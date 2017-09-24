module ColorPickerState exposing (..)

import ColorPicker


type ColorPickerState
    = SelectingStrokeColor ColorPicker.State
    | SelectingFillColor ColorPicker.State
    | Hidden
