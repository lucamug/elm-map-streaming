module MapAddOn exposing
    ( markerLatLong
    , markerLatLongLive
    , markerWithIcon
    , markerWithText
    )

import Color
import FontAwesome.Icon
import FontAwesome.Svg
import Map
import TypedSvg as Svg
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg
import TypedSvg.Events as SA
import TypedSvg.Types as SA


showInnerIcon : Bool
showInnerIcon =
    True


scaleIconDuringZoom : Bool
scaleIconDuringZoom =
    True


markerLatLong : Map.AddOnArgs msg -> Svg.Svg msg
markerLatLong { viewport, situation, cache, zoomAndPanEvents } =
    markerLatLong_ viewport situation.zoom cache.latLngCenter


markerLatLongLive : Map.AddOnArgs msg -> Svg.Svg msg
markerLatLongLive { viewport, situation, cache, zoomAndPanEvents } =
    markerLatLong_ viewport situation.zoom (Map.getLatLngCenter "MapAddOn:markerLatLongLive" situation viewport)


calloutWidth : String -> Float
calloutWidth string =
    (toFloat (String.length string) ^ 1.07) * 6.5 - 20


markerLatLong_ : Map.Viewport -> Map.Zoom -> Map.LatLng -> Svg.Svg msg
markerLatLong_ viewport zoom latLngCenter =
    let
        decimalsQty : Int
        decimalsQty =
            round (toFloat (Map.zoomToNumber zoom) / 5) + 0

        string : String
        string =
            Map.limitDecimal decimalsQty latLngCenter.lat
                ++ ", "
                ++ Map.limitDecimal decimalsQty (Map.normalize -180 180 latLngCenter.lng)
    in
    Svg.g
        [ SA.style "pointer-events: none; user-select: none"
        , SA.transform [ SA.Translate (viewport.width / 2) (viewport.height / 2), SA.Scale 1 1 ]
        ]
        [ markerCallout string
        , markerText string
        ]


markerWithText : Maybe msg -> String -> Map.LatLng -> Map.AddOnArgs msg -> Svg.Svg msg
markerWithText maybeMsg string latLng args =
    Svg.g
        (transformIcon maybeMsg latLng args)
        [ markerCallout string
        , markerText string
        ]


markerCallout : String -> Svg.Svg msg
markerCallout string =
    Svg.path
        -- From https://www.aleksandrhovhannisyan.com/blog/svg-tutorial-how-to-code-svg-icons-by-hand/
        [ SA.fill <| SA.Paint <| Color.rgba 1 1 1 0.9
        , SA.strokeWidth <| SA.px 1
        , SA.stroke <| SA.Paint Map.primaryColor
        , SA.d <| callout <| calloutWidth string
        ]
        []


delta : Map.Viewport -> Map.Cache -> Map.LatLng -> { dx : Float, dy : Float }
delta viewport cache latLngPosition =
    { dx = viewport.width / 2 - (cache.bxbyWhenEnteredThisZoom.bx - Map.lngToX cache.mapSizeInPixels latLngPosition.lng)
    , dy = viewport.height / 2 - (cache.bxbyWhenEnteredThisZoom.by - Map.latToY cache.mapSizeInPixels latLngPosition.lat)
    }


transformIcon : Maybe msg -> Map.LatLng -> Map.AddOnArgs msg -> List (Svg.Attribute msg)
transformIcon maybeMsg latLng args =
    let
        d : { dx : Float, dy : Float }
        d =
            delta args.viewport args.cache latLng
    in
    [ SA.transform
        ([ SA.Translate d.dx d.dy ]
            ++ (if scaleIconDuringZoom then
                    [ SA.Scale args.situation.scaleMarker args.situation.scaleMarker ]

                else
                    []
               )
        )
    ]
        ++ (case maybeMsg of
                Just msg ->
                    SA.onClick msg :: args.zoomAndPanEvents

                Nothing ->
                    [ SA.style "pointer-events: none" ]
           )


markerText : String -> Svg.Svg msg
markerText string =
    Svg.text_
        [ SA.transform [ SA.Translate -12 -18, SA.Scale 1 1 ]
        , SA.fill <| SA.Paint Map.primaryColor
        , SA.style "font-size: 13px; font-family: monospace"
        ]
        [ Svg.text string ]


callout : Float -> String
callout width =
    """
        M 0 0 
        l 10 -10 
        l """ ++ String.fromFloat width ++ """ 0 
        a 8 8 0 0 0 8 -8 
        l 0 -9 
        a 8 8 0 0 0 -8 -8 
        l -""" ++ String.fromFloat (width + 20) ++ """ 0 
        a 8 8 0 0 0 -8 8 
        l 0 9 
        a 8 8 0 0 0 8 8 
        l 10 0 
        z
    """


markerWithIcon : Maybe msg -> FontAwesome.Icon.Icon -> Map.LatLng -> Map.AddOnArgs msg -> Svg.Svg msg
markerWithIcon maybeMsg icon latLng args =
    Svg.g
        (transformIcon maybeMsg latLng args)
        -- ([ Svg.use [ SA.xlinkHref "#marker54" ] [] ]
        ([ Svg.path
            -- From https://www.aleksandrhovhannisyan.com/blog/svg-tutorial-how-to-code-svg-icons-by-hand/
            [ SA.fill <| SA.Paint <| Color.rgb 1 0.3 0.1
            , SA.strokeWidth <| SA.px 1
            , SA.stroke <| SA.Paint <| Color.rgb 0.8 0.3 0
            , SA.d "M 0 0 l 5 -10 a 13 13 0 1 0 -10 0 z"
            , SA.id "marker54"
            ]
            []
         ]
            ++ (if showInnerIcon then
                    let
                        ( i, w, h ) =
                            FontAwesome.Svg.viewIconNEW icon

                        i_ : Svg.Svg msg
                        i_ =
                            i

                        maxSize : Int
                        maxSize =
                            Basics.max w h

                        scale : Float
                        scale =
                            15 / toFloat maxSize

                        toCenter : Float
                        toCenter =
                            toFloat (maxSize - w) / 2
                    in
                    [ Svg.g
                        -- I don't like to have an extra transformation here
                        [ SA.fill <| SA.Paint <| Color.rgb 1 1 1
                        , SA.transform
                            [ SA.Translate (-7.5 + toCenter / 40) -29
                            , SA.Scale scale scale
                            ]
                        ]
                        [ i ]
                    ]

                else
                    []
               )
        )
