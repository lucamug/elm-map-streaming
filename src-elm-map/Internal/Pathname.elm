module Internal.Pathname exposing
    ( Pathname
    , calculateNewZoom
    , fromLatLngZoomScale
    , fromString
    , limitDecimal
    , toLatLngZoomScale
    , toString
    )

import Internal.LatLng
import Internal.Scale
import Internal.Zoom


type Pathname
    = Pathname String


toString : Pathname -> String
toString (Pathname v) =
    v


fromString : String -> Pathname
fromString v =
    Pathname v


toLatLngZoomScale :
    Pathname
    ->
        { latLng : Internal.LatLng.LatLng
        , zoom : Internal.Zoom.Zoom
        , scale : Internal.Scale.Scale
        }
toLatLngZoomScale pathname =
    pathname
        |> toString
        |> String.dropLeft 2
        |> String.dropRight 1
        |> String.split ","
        |> List.map String.toFloat
        |> (\list ->
                case list of
                    [ Just lat, Just lng, Just z ] ->
                        { latLng = { lat = lat, lng = lng }
                        , zoom = Internal.Zoom.fromNumber (floor z)
                        , scale = Internal.Scale.fromNumber (1 + (z - toFloat (floor z)))
                        }

                    _ ->
                        initLatLngZoom
           )


fromLatLngZoomScale :
    { latLng : Internal.LatLng.LatLng
    , zoom : Internal.Zoom.Zoom
    , scale : Internal.Scale.Scale
    }
    -> Pathname
fromLatLngZoomScale { latLng, zoom, scale } =
    let
        newZoomAndScale : { relativeZoom : Internal.Zoom.Zoom, scale : Internal.Scale.Scale }
        newZoomAndScale =
            calculateNewZoom scale

        decimals : Float
        decimals =
            Internal.Scale.toNumber newZoomAndScale.scale - 1

        decimalsQty : Int
        decimalsQty =
            round (toFloat (Internal.Zoom.toNumber zoom) / 5) + 3
    in
    fromString <|
        "/@"
            ++ String.join ","
                [ limitDecimal decimalsQty latLng.lat
                , limitDecimal decimalsQty latLng.lng
                , limitDecimal 3 (toFloat (Internal.Zoom.toNumber zoom + Internal.Zoom.toNumber newZoomAndScale.relativeZoom) + decimals) ++ "z"
                ]


limitDecimal : Int -> Float -> String
limitDecimal decimalsQty float =
    float
        |> String.fromFloat
        |> String.split "."
        |> (\list ->
                case list of
                    [ a, b ] ->
                        if decimalsQty <= 0 then
                            a

                        else
                            String.join "." [ a, String.left decimalsQty b ]

                    _ ->
                        String.join "." list
           )


calculateNewZoom : Internal.Scale.Scale -> { scale : Internal.Scale.Scale, relativeZoom : Internal.Zoom.Zoom }
calculateNewZoom scale =
    let
        ( newScale, relativeZoom ) =
            calculateNewZoomHelper ( scale, Internal.Zoom.fromNumber 0 )
    in
    { scale = newScale
    , relativeZoom = relativeZoom
    }


calculateNewZoomHelper : ( Internal.Scale.Scale, Internal.Zoom.Zoom ) -> ( Internal.Scale.Scale, Internal.Zoom.Zoom )
calculateNewZoomHelper ( scale, zoom ) =
    let
        scaleAsNumber : Float
        scaleAsNumber =
            Internal.Scale.toNumber scale
    in
    if scaleAsNumber >= 2 then
        calculateNewZoomHelper ( Internal.Scale.fromNumber (scaleAsNumber / 2), Internal.Zoom.fromNumber (Internal.Zoom.toNumber zoom + 1) )

    else if scaleAsNumber < 1 then
        calculateNewZoomHelper ( Internal.Scale.fromNumber (scaleAsNumber * 2), Internal.Zoom.fromNumber (Internal.Zoom.toNumber zoom - 1) )

    else
        ( scale, zoom )


initLatLngZoom :
    { latLng : Internal.LatLng.LatLng
    , zoom : Internal.Zoom.Zoom
    , scale : Internal.Scale.Scale
    }
initLatLngZoom =
    { latLng = { lat = 0, lng = 0 }
    , zoom = Internal.Zoom.fromNumber 2
    , scale = Internal.Scale.fromNumber 1
    }
