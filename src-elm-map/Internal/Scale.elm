module Internal.Scale exposing
    ( Scale
    , fromNumber
    , toNumber
    )


type Scale
    = Scale Float


toNumber : Scale -> Float
toNumber (Scale v) =
    v


fromNumber : Float -> Scale
fromNumber v =
    Scale v
