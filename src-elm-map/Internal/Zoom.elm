module Internal.Zoom exposing
    ( Zoom
    , fromNumber
    , toNumber
    )


type Zoom
    = Zoom Int


toNumber : Zoom -> Int
toNumber (Zoom v) =
    v


fromNumber : Int -> Zoom
fromNumber v =
    Zoom v
