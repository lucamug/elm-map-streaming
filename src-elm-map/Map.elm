module Map exposing
    ( AddOnArgs
    , Cache
    , LatLng
    , Model
    , Msg
    , TileDataOrder(..)
    , Viewport
    , Zoom
    , ZoomAndPanAsRecord
    , getLatLngCenter
    , getPathname
    , initModel
    , isClick
    , latToY
    , limitDecimal
    , listTouples
    , lngToX
    , normalize
    , pathnameFromString
    , primaryColor
    , subscriptions
    , update
    , updatePathname
    , view
    , xToLng
    , xyToLatLng
    , xyToLatLngNormalized
    , yToLat
    , zoomAndPanAsRecord
    , zoomToNumber
    )

import Browser.Events
import Color
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Internal.LatLng
import Internal.Pathname
import Internal.Scale
import Internal.Zoom
import Json.Decode
import Process
import Set
import Task
import TypedSvg as Svg
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg
import TypedSvg.Events as SA
import TypedSvg.Types as SA
import VirtualDom
import Zoom


type Model
    = Model
        { situation : Situation
        , viewport : Viewport
        , tileImageLoaded : Set.Set String
        , isUserInteracting : Bool
        , tilesQtyToCoverEntireViewport : { x : Int, y : Int }
        , cache : Cache
        , pathname : Pathname
        }


type Msg
    = OnZoomAndPan Zoom.OnZoom
    | OnLoad String
    | OnZoomAndPanDelayed Zoom.Zoom


type alias Flags =
    { pathnameAsString : String, viewport : Viewport }


type alias AddOnArgs msg =
    { viewport : Viewport
    , situation : Situation
    , cache : Cache
    , zoomAndPanEvents : List (Attribute msg)
    }


type alias Zoom =
    Internal.Zoom.Zoom


type alias Scale =
    Internal.Scale.Scale


type alias Pathname =
    Internal.Pathname.Pathname


type alias XY =
    { x : Float, y : Float }


type alias ZoomAndPanAsRecord =
    { translate : XY, scale : Float }


type alias LatLng =
    Internal.LatLng.LatLng


type alias Viewport =
    { width : Float, height : Float }


type alias Situation =
    { zoomAndPan : Zoom.Zoom
    , zoom : Zoom
    , latLngCenter : LatLng
    , scaleMarker : Float
    }


type alias Cache =
    { latLngCenter : LatLng
    , txtyCentertile : { tx : Int, ty : Int }
    , xyCornerOfCentertile : XY
    , bxbyWhenEnteredThisZoom : { bx : Float, by : Float }
    , mapSizeInPixels : Float
    }


zoomToNumber : Zoom -> Int
zoomToNumber =
    Internal.Zoom.toNumber


zoomFromNumber : Int -> Zoom
zoomFromNumber =
    Internal.Zoom.fromNumber


scaleToNumber : Scale -> Float
scaleToNumber =
    Internal.Scale.toNumber


scaleFromNumber : Float -> Scale
scaleFromNumber =
    Internal.Scale.fromNumber


pathnameToString : Pathname -> String
pathnameToString =
    Internal.Pathname.toString


pathnameFromString : String -> Pathname
pathnameFromString =
    Internal.Pathname.fromString


pathnameToLatLngZoomScale : Pathname -> { latLng : LatLng, scale : Scale, zoom : Zoom }
pathnameToLatLngZoomScale =
    Internal.Pathname.toLatLngZoomScale


limitDecimal : Int -> Float -> String
limitDecimal =
    Internal.Pathname.limitDecimal


zoomAndPanAsRecord : Zoom.Zoom -> { scale : Float, translate : XY }
zoomAndPanAsRecord =
    Zoom.asRecord


recenterMap : Viewport -> Scale -> XY
recenterMap viewport scale =
    --
    -- This reposition the map (translate) after a zoom change. The new scale
    -- is always between 1 and 2(excluded).
    -- If the new scale is 1, no need to translate.
    -- If the scale is larger than 1, we need to recenter the larger map.
    -- On the edge case, almost 2, we need to move the map of width/2 and
    -- height/2.
    --
    { x = (viewport.width / 2) - (viewport.width / 2) * scaleToNumber scale
    , y = (viewport.height / 2) - (viewport.height / 2) * scaleToNumber scale
    }


pathnameToSituation : Viewport -> Pathname -> Situation
pathnameToSituation viewport pathname =
    let
        dataFromPathname : { latLng : LatLng, zoom : Zoom, scale : Scale }
        dataFromPathname =
            pathnameToLatLngZoomScale pathname

        scale : Float
        scale =
            scaleToNumber dataFromPathname.scale
    in
    { zoomAndPan =
        initZoomAndPan viewport
            |> Zoom.setTransform
                Zoom.instantly
                { scale = scale
                , translate = recenterMap viewport dataFromPathname.scale
                }
    , latLngCenter = dataFromPathname.latLng
    , zoom = dataFromPathname.zoom
    , scaleMarker = 1 / scale
    }


initModel : Maybe Model -> Flags -> Model
initModel maybeModel flags =
    let
        tileImageLoaded =
            case maybeModel of
                Just (Model model) ->
                    -- In case the model already exsists, we preserve the
                    -- information about loaded tiles
                    model.tileImageLoaded

                Nothing ->
                    Set.empty

        _ =
            flags

        situation : Situation
        situation =
            pathnameToSituation flags.viewport (pathnameFromString flags.pathnameAsString)
    in
    Model
        { situation = situation
        , viewport = flags.viewport
        , tileImageLoaded = tileImageLoaded
        , isUserInteracting = False
        , tilesQtyToCoverEntireViewport = viewportToTilesQtyToCoverEntireViewport flags.viewport
        , cache = cachedValues situation flags.viewport
        , pathname = pathnameFromString flags.pathnameAsString
        }


initZoomAndPan : Viewport -> Zoom.Zoom
initZoomAndPan viewport =
    Zoom.init { width = viewport.width, height = viewport.height }


viewportToTilesQtyToCoverEntireViewport : Viewport -> { x : Int, y : Int }
viewportToTilesQtyToCoverEntireViewport viewport =
    { x = round (viewport.width / 256 / 2) + 1
    , y = round (viewport.height / 256 / 2) + 1
    }


updatePathname : Pathname -> Model -> Model
updatePathname pathname (Model model) =
    let
        situation : Situation
        situation =
            pathnameToSituation model.viewport pathname
    in
    Model
        { model
            | situation = situation
            , cache = cachedValues situation model.viewport
        }


getPathname : Model -> String
getPathname (Model model) =
    pathnameToString model.pathname


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        -- PopState pathname ->
        --     ( { model | situation = pathnameToSituation model.viewport pathname }, Cmd.none )
        --
        OnZoomAndPanDelayed delayedzoomAndPan ->
            let
                newZoomAndPan : Zoom.Zoom
                newZoomAndPan =
                    model.situation.zoomAndPan

                newZoomAndPanAsRecord : ZoomAndPanAsRecord
                newZoomAndPanAsRecord =
                    Zoom.asRecord newZoomAndPan

                delayedZoomAndPanAsRecord : ZoomAndPanAsRecord
                delayedZoomAndPanAsRecord =
                    Zoom.asRecord delayedzoomAndPan
            in
            if newZoomAndPanAsRecord /= delayedZoomAndPanAsRecord then
                -- User still interacting with the map because the map
                -- moved during the 200ms of delay
                ( Model model, Cmd.none )

            else if not model.isUserInteracting then
                -- User not interacting but it was already not interacting
                -- This happen for events that don't move the map, like
                -- releasing the button of the mouse
                ( Model model, Cmd.none )

            else if Zoom.isDragging model.situation.zoomAndPan then
                -- User is still dragging
                ( Model model, Cmd.none )

            else
                -- User not interacting anymore
                -- Transition True -> False of isUserInteracting
                let
                    newLatLngCenter : LatLng
                    newLatLngCenter =
                        getLatLngCenter "update:OnZoomAndPanDelayed" model.situation model.viewport

                    pathname : Pathname
                    pathname =
                        Internal.Pathname.fromLatLngZoomScale
                            { latLng = newLatLngCenter
                            , zoom = model.situation.zoom
                            , scale = scaleFromNumber newZoomAndPanAsRecord.scale
                            }

                    situation : Situation
                    situation =
                        pathnameToSituation model.viewport pathname

                    isDifferentZoomAndDifferentScale =
                        let
                            latLngZoomScaleBefore : { latLng : Internal.LatLng.LatLng, zoom : Internal.Zoom.Zoom, scale : Internal.Scale.Scale }
                            latLngZoomScaleBefore =
                                Internal.Pathname.toLatLngZoomScale model.pathname

                            latLngZoomScaleAfter : { latLng : Internal.LatLng.LatLng, zoom : Internal.Zoom.Zoom, scale : Internal.Scale.Scale }
                            latLngZoomScaleAfter =
                                Internal.Pathname.toLatLngZoomScale pathname
                        in
                        (latLngZoomScaleBefore.scale /= latLngZoomScaleAfter.scale) && (latLngZoomScaleBefore.zoom /= latLngZoomScaleAfter.zoom)
                in
                ( Model
                    { model
                        | isUserInteracting = False
                        , situation = situation
                        , cache = cachedValues situation model.viewport
                        , pathname = pathname

                        -- We reset the status of loaded image, to avoid flikkering, only if zooming
                        -- level changed
                        , tileImageLoaded =
                            if isDifferentZoomAndDifferentScale then
                                Set.empty

                            else
                                model.tileImageLoaded
                    }
                , Cmd.none
                )

        OnLoad id ->
            ( Model { model | tileImageLoaded = Set.insert id model.tileImageLoaded }
            , Cmd.none
            )

        OnZoomAndPan zoomMsg ->
            let
                newZoomAndPan : Zoom.Zoom
                newZoomAndPan =
                    Zoom.update zoomMsg model.situation.zoomAndPan

                newZoomAndPanAsRecord : ZoomAndPanAsRecord
                newZoomAndPanAsRecord =
                    Zoom.asRecord newZoomAndPan

                newZoomAndScale : { relativeZoom : Zoom, scale : Scale }
                newZoomAndScale =
                    Internal.Pathname.calculateNewZoom (scaleFromNumber newZoomAndPanAsRecord.scale)

                totalZoom : Int
                totalZoom =
                    zoomToNumber model.situation.zoom + zoomToNumber newZoomAndScale.relativeZoom
            in
            if totalZoom >= 20 || totalZoom < 2 then
                ( Model model, Cmd.none )

            else
                ( Model
                    { model
                        | situation =
                            { zoomAndPan = newZoomAndPan
                            , zoom = model.situation.zoom
                            , latLngCenter = model.situation.latLngCenter
                            , scaleMarker = 1 / newZoomAndPanAsRecord.scale
                            }
                        , isUserInteracting =
                            if newZoomAndPanAsRecord == Zoom.asRecord model.situation.zoomAndPan then
                                model.isUserInteracting

                            else
                                True
                    }
                , Task.perform (always (OnZoomAndPanDelayed newZoomAndPan)) (Process.sleep 200)
                )


isClick : Msg -> Model -> Maybe ( Float, Float )
isClick msg (Model model) =
    case msg of
        OnZoomAndPan zoonAndPanMsg ->
            Zoom.isClick zoonAndPanMsg model.situation.zoomAndPan

        _ ->
            Nothing



-- CACHE


cachedValues : Situation -> Viewport -> Cache
cachedValues situation viewport =
    --
    -- We may have two different zoom levels, if we are moving from two different
    -- zoom level, while zooming in and out of the map.
    --
    -- * Get the lat/lng in the center, based on the OLD zoom level
    -- * Calculate the tileID (tileX and tileY) of the center tile, using NEW zoom level and above lat/lng
    -- * Caclulate lat,lng of the top left corner if above tile
    -- * Calculate the "relative" position (x,y) of the center tile
    --
    let
        -- OLD ZOOM LEVEL
        --
        latLngCenter : LatLng
        latLngCenter =
            getLatLngCenter "cachedValues" situation viewport

        -- NEW ZOOM LEVEL
        --
        txtyCentertile : { tx : Int, ty : Int }
        txtyCentertile =
            { tx = lngToTileX latLngCenter.lng situation.zoom
            , ty = latToTileY latLngCenter.lat situation.zoom
            }

        latLngCornerOfCentertile : LatLng
        latLngCornerOfCentertile =
            { lat = tileYTolat txtyCentertile.ty situation.zoom
            , lng = tileXTolng txtyCentertile.tx situation.zoom
            }

        mapSizeInPixels : Float
        mapSizeInPixels =
            -- Height and Width of the map are the same
            zoomToMapSizeInPixels situation.zoom

        bxbyWhenEnteredThisZoom : { bx : Float, by : Float }
        bxbyWhenEnteredThisZoom =
            -- "b" = big
            --
            -- This is the distance in pixels from lat 0, lng 0 at certain
            -- zoom level
            --
            { bx = lngToX mapSizeInPixels situation.latLngCenter.lng
            , by = latToY mapSizeInPixels situation.latLngCenter.lat
            }

        bxbyCornerOfCentertile : { bx : Float, by : Float }
        bxbyCornerOfCentertile =
            -- "b" = big
            { bx = lngToX mapSizeInPixels latLngCornerOfCentertile.lng
            , by = latToY mapSizeInPixels latLngCornerOfCentertile.lat
            }

        xyCornerOfCentertile : XY
        xyCornerOfCentertile =
            { x =
                bxbyCornerOfCentertile.bx
                    -- We remove the distance in pixels of this area compared to
                    -- lat 0, lng 0 because when we enter a new zoom level, we
                    -- always set x and y to 0
                    - bxbyWhenEnteredThisZoom.bx
                    -- We add half viewport width, because lat,lng is in the
                    -- center, so the center tile need to stay in the center
                    + (viewport.width / 2)
            , y =
                bxbyCornerOfCentertile.by
                    - bxbyWhenEnteredThisZoom.by
                    + (viewport.height / 2)
            }
    in
    { xyCornerOfCentertile = xyCornerOfCentertile
    , txtyCentertile = txtyCentertile
    , latLngCenter = latLngCenter
    , bxbyWhenEnteredThisZoom = bxbyWhenEnteredThisZoom
    , mapSizeInPixels = mapSizeInPixels
    }



-- VIEW


view :
    { listAddOn : List (AddOnArgs msg -> Svg.Svg msg)
    , listAddOnTransformed : List (AddOnArgs msg -> Svg.Svg msg)
    , extraCss : String
    , tilesSource : String
    , tileDataOrder : TileDataOrder
    }
    -> (Msg -> msg)
    -> Model
    -> Html msg
view args msgMapper (Model model) =
    let
        zoomAndPanEvents : List (Attribute msg)
        zoomAndPanEvents =
            List.map (\c -> Html.Attributes.map msgMapper c) (Zoom.events model.situation.zoomAndPan OnZoomAndPan)

        --
        -- type alias AddOn msg =
        --     { viewport : Viewport
        --     , situation : Situation
        --     , cache : Cache
        --     , zoomAndPanEvents : List (Attribute msg)
        --     }
        --     -> Svg.Svg msg
        --
        addOnArgs :
            { cache : Cache
            , situation : Situation
            , viewport : Viewport
            , zoomAndPanEvents : List (Attribute msg)
            }
        addOnArgs =
            { viewport = model.viewport
            , situation = model.situation
            , cache = model.cache
            , zoomAndPanEvents = zoomAndPanEvents
            }
    in
    Svg.svg
        [ SA.width <| SA.px model.viewport.width
        , SA.height <| SA.px model.viewport.height

        -- "vetical-align: bottom" to fix this issue: https://stackoverflow.com/questions/5804256/image-inside-div-has-extra-space-below-the-image
        , style "vertical-align" "bottom"
        ]
        ([ Svg.defs []
            [ Svg.path
                -- From https://www.aleksandrhovhannisyan.com/blog/svg-tutorial-how-to-code-svg-icons-by-hand/
                [ SA.fill <| SA.Paint <| Color.rgb 1 0.3 0.1
                , SA.strokeWidth <| SA.px 1
                , SA.stroke <| SA.Paint <| Color.rgb 0.8 0.3 0
                , SA.d "M 0 0 l 5 -10 a 13 13 0 1 0 -10 0 z"
                , SA.id "marker54"
                ]
                []
            ]
         , Svg.rect
            ([ SA.width <| SA.px model.viewport.width
             , SA.height <| SA.px model.viewport.height
             , SA.fill <| SA.Paint <| Color.rgb255 200 210 210
             ]
                ++ zoomAndPanEvents
            )
            []
         , Svg.g
            [ Zoom.transform model.situation.zoomAndPan ]
            (List.map (\tile -> Svg.map msgMapper <| tile) (viewTiles args.tileDataOrder args.tilesSource args.extraCss (Model model))
                ++ List.map (\addOn -> addOn addOnArgs) args.listAddOnTransformed
            )
         ]
            ++ List.map (\addOn -> addOn addOnArgs) args.listAddOn
        )


viewTiles : TileDataOrder -> String -> String -> Model -> List (Svg.Svg Msg)
viewTiles tileDataOrder tilesSource extraCss (Model model) =
    List.map
        (\( x, y ) ->
            viewTile
                tileDataOrder
                tilesSource
                extraCss
                model.tileImageLoaded
                model.cache.xyCornerOfCentertile
                model.situation.zoom
                model.cache.txtyCentertile
                x
                y
        )
        (listTouples
            { xFrom = -model.tilesQtyToCoverEntireViewport.x
            , xTo = model.tilesQtyToCoverEntireViewport.x
            , yFrom = -model.tilesQtyToCoverEntireViewport.y
            , yTo = model.tilesQtyToCoverEntireViewport.y
            }
        )


type TileDataOrder
    = ZYX
    | ZXY


viewTile : TileDataOrder -> String -> String -> Set.Set String -> XY -> Zoom -> { tx : Int, ty : Int } -> Int -> Int -> Svg.Svg Msg
viewTile tileDataOrder tilesSource extraCss loaded xyCornerOfCentertile zoom txtyCentertile dx dy =
    let
        maxTiles : Int
        maxTiles =
            2 ^ zoomToNumber zoom

        tileHttp : Zoom -> Int -> Int -> String
        tileHttp zl tileX tileY =
            -- "https://tile.openstreetmap.org/"
            -- "http://c.tile.stamen.com/watercolor/"
            -- "https://a.tile.opentopomap.org/"
            -- "https://stamen-tiles.a.ssl.fastly.net/toner/"
            case tileDataOrder of
                ZYX ->
                    tilesSource
                        ++ String.fromInt (zoomToNumber zl)
                        ++ "/"
                        ++ String.fromInt tileY
                        ++ "/"
                        ++ String.fromInt tileX
                        ++ ".png"

                ZXY ->
                    tilesSource
                        ++ String.fromInt (zoomToNumber zl)
                        ++ "/"
                        ++ String.fromInt tileX
                        ++ "/"
                        ++ String.fromInt tileY
                        ++ ".png"

        hrefTile : String
        hrefTile =
            tileHttp
                zoom
                (floor (normalize 0 (toFloat maxTiles) (toFloat (txtyCentertile.tx + dx))))
                (floor (normalize 0 (toFloat maxTiles) (toFloat (txtyCentertile.ty + dy))))

        id : Zoom -> Int -> Int -> String
        id zl tileX tileY =
            -- String.fromInt dx ++ "," ++ String.fromInt dy
            String.fromInt (zoomToNumber zl)
                ++ "/"
                ++ String.fromInt tileX
                ++ "/"
                ++ String.fromInt tileY

        idTile : String
        idTile =
            id
                zoom
                (floor (normalize 0 (toFloat maxTiles) (toFloat (txtyCentertile.tx + dx))))
                (floor (normalize 0 (toFloat maxTiles) (toFloat (txtyCentertile.ty + dy))))
    in
    Svg.g [ SA.style "pointer-events: none" ]
        [ Svg.image
            [ SA.href hrefTile
            , SA.width <| SA.px 256
            , SA.height <| SA.px 256
            , SA.x <| SA.px (xyCornerOfCentertile.x + toFloat (256 * dx))
            , SA.y <| SA.px (xyCornerOfCentertile.y + toFloat (256 * dy))
            , Html.Events.on "load" (Json.Decode.succeed (OnLoad idTile))
            , if Set.member idTile loaded then
                SA.style (extraCss ++ "; transition: opacity 0.2s")

              else
                SA.style "opacity: 0; transition: 0"
            ]
            []
        ]



-- HELPERS


listTouples : { xFrom : Int, xTo : Int, yFrom : Int, yTo : Int } -> List ( Int, Int )
listTouples { xFrom, yFrom, xTo, yTo } =
    List.concat <|
        List.map
            (\x_ ->
                List.map
                    (\y_ ->
                        ( x_, y_ )
                    )
                    (List.range yFrom yTo)
            )
            (List.range xFrom xTo)



-- TILE MATH


lngToTileX : Float -> Zoom -> Int
lngToTileX lon z =
    floor ((lon + 180.0) / 360.0 * (2 ^ toFloat (zoomToNumber z)))


latToTileY : Float -> Zoom -> Int
latToTileY lat z =
    let
        n : Float
        n =
            lat * pi / 180
    in
    floor ((1.0 - ln (tan n + 1.0 / cos n) / pi) / 2 * (2 ^ toFloat (zoomToNumber z)))


tileXTolng : Int -> Zoom -> Float
tileXTolng x z =
    toFloat x / (2 ^ toFloat (zoomToNumber z)) * 360.0 - 180


tileYTolat : Int -> Zoom -> Float
tileYTolat y z =
    let
        n : Float
        n =
            pi - 2 * pi * toFloat y / (2 ^ toFloat (zoomToNumber z))
    in
    180.0 / pi * atan (0.5 * (exp n - exp -n))


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Zoom.subscriptions model.situation.zoomAndPan OnZoomAndPan


normalize : Float -> Float -> Float -> Float
normalize min max value =
    if value >= max then
        normalize min max (value - (max - min))

    else if value < min then
        normalize min max (value + (max - min))

    else
        value


primaryColor : Color.Color
primaryColor =
    Color.rgb255 30 128 180


zoomToMapSizeInPixels : Zoom -> Float
zoomToMapSizeInPixels zoom =
    toFloat <| 2 ^ zoomToNumber zoom * 256



--


xyToLatLng : String -> Model -> XY -> LatLng
xyToLatLng caller (Model model) xy =
    let
        viewportStaticCenter : XY
        viewportStaticCenter =
            -- The center of the viewport
            { x = model.viewport.width / 2
            , y = model.viewport.height / 2
            }
    in
    xyToLatLng_ caller model.situation model.viewport viewportStaticCenter xy


xyToLatLngNormalized : String -> Model -> XY -> LatLng
xyToLatLngNormalized caller (Model model) xy =
    let
        zoomAndPanAsRecord_ : ZoomAndPanAsRecord
        zoomAndPanAsRecord_ =
            zoomAndPanAsRecord model.situation.zoomAndPan

        viewportStaticCenter : XY
        viewportStaticCenter =
            -- The center of the viewport
            { x = model.viewport.width / 2
            , y = model.viewport.height / 2
            }

        partial : XY
        partial =
            normalizeXYPartial
                xy
                viewportStaticCenter
                zoomAndPanAsRecord_.scale

        normalizedXY : XY
        normalizedXY =
            { x = partial.x + viewportStaticCenter.x
            , y = partial.y + viewportStaticCenter.y
            }
    in
    xyToLatLng_
        caller
        model.situation
        model.viewport
        viewportStaticCenter
        normalizedXY



-- LAT/LNG MAP CENTER


getLatLngCenter : String -> Situation -> Viewport -> LatLng
getLatLngCenter caller situation viewport =
    -- TODO
    --
    -- Check if we can leverage something from cache here instead or re-calculating everything
    let
        zoomAndPanAsRecord_ : ZoomAndPanAsRecord
        zoomAndPanAsRecord_ =
            zoomAndPanAsRecord situation.zoomAndPan

        viewportStaticCenter : XY
        viewportStaticCenter =
            -- The center of the viewport
            { x = viewport.width / 2
            , y = viewport.height / 2
            }

        viewportDynamicCenter : XY
        viewportDynamicCenter =
            --
            -- The center of the viewport that take in consideration the user
            -- moving around
            --
            normalizeXYPartial
                viewportStaticCenter
                zoomAndPanAsRecord_.translate
                zoomAndPanAsRecord_.scale
    in
    xyToLatLng_ caller situation viewport viewportStaticCenter viewportDynamicCenter


xyToLatLng_ : String -> Situation -> Viewport -> XY -> XY -> LatLng
xyToLatLng_ caller { zoomAndPan, zoom, latLngCenter } viewport viewportStaticCenter xy =
    --
    -- xy = pixel position relative to the map
    --
    --     (0,0) top left corner
    --     (widht, height) bottom right conrner
    --
    -- TODO
    --
    -- Check if we can leverage something from cache here instead or re-calculating everything
    let
        mapSizeInPixels : Float
        mapSizeInPixels =
            zoomToMapSizeInPixels zoom

        bxbyWhenEnteredThisZoom : XY
        bxbyWhenEnteredThisZoom =
            { x = lngToX mapSizeInPixels latLngCenter.lng
            , y = latToY mapSizeInPixels latLngCenter.lat
            }

        bxbySomething : { bx : Float, by : Float }
        bxbySomething =
            { bx = bxbyWhenEnteredThisZoom.x - viewportStaticCenter.x
            , by = bxbyWhenEnteredThisZoom.y - viewportStaticCenter.y
            }
    in
    { lat = yToLat mapSizeInPixels (bxbySomething.by + xy.y)
    , lng = xToLng mapSizeInPixels (bxbySomething.bx + xy.x)
    }


normalizeXYPartial : XY -> XY -> Float -> XY
normalizeXYPartial xy1 xy2 scale =
    { x = (xy1.x - xy2.x) / scale
    , y = (xy1.y - xy2.y) / scale
    }



-- MATH


ln : Float -> Float
ln =
    logBase e


exp : Float -> Float
exp =
    (^) e



-- X,Y MATH


xToLng : Float -> Float -> Float
xToLng mapSizeInPixels xValue =
    ((360 * xValue) / mapSizeInPixels) - 180


yToLat : Float -> Float -> Float
yToLat mapSizeInPixels yValue =
    90 * (-1 + (4 * atan (exp (pi - (2 * pi * yValue) / mapSizeInPixels))) / pi)


lngToX : Float -> Float -> Float
lngToX mapSizeInPixels lng =
    (lng + 180) * (mapSizeInPixels / 360)


latToY : Float -> Float -> Float
latToY mapSizeInPixels lat =
    (mapSizeInPixels / 2) - (mapSizeInPixels * ln (tan ((pi / 4) + ((lat * pi / 180) / 2))) / (2 * pi))
