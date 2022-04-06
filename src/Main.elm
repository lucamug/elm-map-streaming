port module Main exposing (main)

import Browser
import Browser.Events
import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FontAwesome.Icon
import FontAwesome.Regular
import FontAwesome.Solid
import FontAwesome.Svg
import Html
import Html.Attributes
import Html.Events
import Icons
import List.Extra
import Map
import MapAddOn
import TypedSvg as Svg
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg
import TypedSvg.Events as SA
import TypedSvg.Types as SA


largeQuantityOfIcons : Bool
largeQuantityOfIcons =
    False


port pushState : String -> Cmd msg


port toggleFullscreen : () -> Cmd msg


port onToggleFullscreen : (Bool -> msg) -> Sub msg


port popState : (String -> msg) -> Sub msg


type alias Flags =
    { pathnameAsString : String, viewport : Map.Viewport }


type alias Marker =
    { latLng : Map.LatLng
    , icon : String
    , open : Bool
    }


type alias Model =
    { mapModel1 : Map.Model
    , mapModel2 : Map.Model
    , viewport : Map.Viewport
    , stateFullviewport : Bool
    , stateFullscreen : Bool
    , markers : List Map.LatLng
    , extraCss : String
    , tilesSource : TilesSource
    }


type alias TilesSource =
    { name : String
    , url : String
    , order : Map.TileDataOrder
    , copyrights : List { text : String, url : String }
    }


defaultTilesSource : TilesSource
defaultTilesSource =
    { url = "https://tile.openstreetmap.org/"
    , name = "Open Street Map"
    , order = Map.ZXY
    , copyrights =
        [ { text = "© OpenStreetMap contributors"
          , url = "https://www.openstreetmap.org/copyright"
          }
        ]
    }


tilesSources : List TilesSource
tilesSources =
    [ defaultTilesSource
    , { url = "http://c.tile.stamen.com/watercolor/"
      , name = "Watercolor"
      , order = Map.ZXY
      , copyrights =
            [ { text = "© OpenStreetMap contributors"
              , url = "https://www.openstreetmap.org/copyright"
              }
            ]
      }
    , { url = "https://a.tile.opentopomap.org/"
      , name = "Open Topomap"
      , order = Map.ZXY
      , copyrights =
            [ { text = "© OpenStreetMap contributors"
              , url = "https://www.openstreetmap.org/copyright"
              }
            ]
      }
    , { url = "https://stamen-tiles.a.ssl.fastly.net/toner/"
      , name = "Toner"
      , order = Map.ZXY
      , copyrights =
            [ { text = "© OpenStreetMap contributors"
              , url = "https://www.openstreetmap.org/copyright"
              }
            ]
      }
    , { url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/"
      , name = "ArcGIS Online"
      , order = Map.ZYX
      , copyrights =
            [ { text = "© ArcGIS Online map hosted by Esri"
              , url = "https://doc.arcgis.com/en/arcgis-online/reference/terms-of-use.htm"
              }
            ]
      }
    ]


defaultCssFilter : String
defaultCssFilter =
    ""


cssFilters : List { name : String, url : String }
cssFilters =
    [ { url = defaultCssFilter, name = "None" }
    , { url = "filter: invert(100%) grayscale(100%) brightness(200%)", name = "Multiple" }
    , { url = "filter: hue-rotate(90deg)", name = "Hue Rotate" }
    , { url = "filter: grayscale(70%)", name = "Grayscale" }
    , { url = "filter: invert(100%)", name = "Invert" }
    , { url = "filter: sepia(70%)", name = "Sepia" }
    ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        tilesSource : TilesSource
        tilesSource =
            tilesSources
                |> List.head
                |> Maybe.withDefault defaultTilesSource
    in
    ( { mapModel1 =
            Map.initModel
                Nothing
                { pathnameAsString = flags.pathnameAsString
                , viewport = { width = flags.viewport.width - 100, height = 300 }
                }
      , mapModel2 =
            Map.initModel
                Nothing
                { pathnameAsString = flags.pathnameAsString
                , viewport = { width = 300, height = 300 }
                }
      , viewport = flags.viewport
      , stateFullviewport = False
      , stateFullscreen = False
      , markers = []
      , extraCss = "filter: sepia(70%)"
      , tilesSource = tilesSource
      }
    , Cmd.none
    )


type Msg
    = MapMsg1 Map.Msg
    | MapMsg2 Map.Msg
    | PopState String
    | GotNewViewport Map.Viewport
    | ToggleFullviewport
    | ToggleFullscreen
    | OnToggleFullscreen Bool
    | ClickOnMarker String
    | ChangeExtraCss String
    | ChangeTilesSource TilesSource


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTilesSource tilesSource ->
            ( { model | tilesSource = tilesSource }, Cmd.none )

        ChangeExtraCss extraCss ->
            ( { model | extraCss = extraCss }, Cmd.none )

        ClickOnMarker string ->
            -- TODO - Add something here
            ( model, Cmd.none )

        ToggleFullviewport ->
            ( { model
                | stateFullviewport = not model.stateFullviewport
                , mapModel1 = updateMap1Viewport model model.viewport (not model.stateFullviewport)
              }
            , Cmd.none
            )

        ToggleFullscreen ->
            ( model, toggleFullscreen () )

        OnToggleFullscreen stateFullscreen ->
            ( { model | stateFullscreen = stateFullscreen }, Cmd.none )

        GotNewViewport viewport ->
            ( { model
                | viewport = viewport
                , mapModel1 = updateMap1Viewport model viewport model.stateFullviewport
              }
            , Cmd.none
            )

        MapMsg1 mapMsg ->
            let
                ( mapModel, mapCmd ) =
                    Map.update mapMsg model.mapModel1

                newPathname : String
                newPathname =
                    Map.getPathname mapModel

                oldPathname : String
                oldPathname =
                    Map.getPathname model.mapModel1

                newModel =
                    case Map.isClick mapMsg model.mapModel1 of
                        Nothing ->
                            model

                        Just ( offsetX, offsetY ) ->
                            let
                                latLng =
                                    Map.xyToLatLngNormalized "Main.update.MapMsg1"
                                        model.mapModel1
                                        { x = offsetX, y = offsetY }
                            in
                            { model | markers = latLng :: model.markers }
            in
            if oldPathname == newPathname then
                ( { newModel | mapModel1 = mapModel }
                , Cmd.map MapMsg1 mapCmd
                )

            else
                ( { newModel | mapModel1 = mapModel }
                , Cmd.batch [ Cmd.map MapMsg1 mapCmd, pushState newPathname ]
                )

        MapMsg2 mapMsg ->
            let
                ( mapModel, mapCmd ) =
                    Map.update mapMsg model.mapModel2
            in
            ( { model | mapModel2 = mapModel }
            , Cmd.map MapMsg2 mapCmd
            )

        PopState pathnameAsString ->
            ( { model | mapModel1 = Map.updatePathname (Map.pathnameFromString pathnameAsString) model.mapModel1 }
            , Cmd.none
            )


updateMap1Viewport : Model -> Map.Viewport -> Bool -> Map.Model
updateMap1Viewport model viewport stateFullviewport =
    if stateFullviewport then
        Map.initModel
            (Just model.mapModel1)
            { pathnameAsString = Map.getPathname model.mapModel1
            , viewport = { width = viewport.width, height = viewport.height }
            }

    else
        Map.initModel
            (Just model.mapModel1)
            { pathnameAsString = Map.getPathname model.mapModel1
            , viewport = { width = viewport.width - 100, height = 300 }
            }



-- VIEW


attrsButton : List (Attribute msg)
attrsButton =
    [ paddingEach { top = 10, right = 20, bottom = 10, left = 20 }
    , Border.rounded 20
    , Background.color <| rgb 0.9 0.8 0.5
    , width fill
    ]


view : Model -> Html.Html Msg
view model =
    layout [] <|
        column
            [ spacing 20, padding 50 ]
            [ html <| Html.node "style" [] [ Html.text css ]
            , row [] [ image [ width <| px 60 ] { src = "elm-map-logo.png", description = "" }, text " elm-map!" ]

            -- , p [] [ text "Map examples" ]
            , el [] <|
                html <|
                    viewMap
                        { supporFullviewport = True
                        , stateFullscreen = model.stateFullscreen
                        , stateFullviewport = model.stateFullviewport
                        , listLayers = [ mapLinks ]

                        -- , listAddOn = [ MapAddOn.markerLatLongLive ]
                        , listAddOn = []
                        , msgMapper = MapMsg1
                        , mapModel = model.mapModel1
                        , markers = model.markers
                        , extraCss = model.extraCss
                        , tilesSource = model.tilesSource.url
                        , tileDataOrder = model.tilesSource.order
                        }
            , html <| Html.br [ Html.Attributes.style "user-select" "none" ] []
            , column [ spacing 20, width fill, spacing 40, height <| px 200, scrollbarY ]
                [ column [ spacing 10, width fill, alignTop ] <|
                    -- Input.text []
                    --     { label = Input.labelAbove [] <| text "Tiles Source"
                    --     , onChange = ChangeTilesSource
                    --     , placeholder = Nothing
                    --     , text = model.tilesSource
                    --     } ::
                    (text "Tiles Sources"
                        :: List.map
                            (\source ->
                                Input.button attrsButton { label = text source.name, onPress = Just <| ChangeTilesSource source }
                            )
                            tilesSources
                    )
                , column [ spacing 10, width fill, alignTop ] <|
                    Input.text []
                        { label = Input.labelAbove [] <| text "CSS Filter"
                        , onChange = ChangeExtraCss
                        , placeholder = Nothing
                        , text = model.extraCss
                        }
                        :: List.map
                            (\source ->
                                Input.button attrsButton { label = text source.name, onPress = Just <| ChangeExtraCss source.url }
                            )
                            cssFilters
                ]

            -- , div [ style "margin" "20px", style "display" "inline-block" ]
            --     [ viewMap
            --         { supporFullviewport = False
            --         , stateFullscreen = model.stateFullscreen
            --         , stateFullviewport = False
            --         , listLayers = []
            --         , listAddOn = [ MapAddOn.markerLatLongLive ]
            --         , msgMapper = MapMsg2
            --         , mapModel = model.mapModel2
            --         , markers = model.markers
            --         }
            --     ]
            -- , div [ style "margin" "20px", style "display" "inline-block" ]
            --     [ viewMap
            --         { supporFullviewport = False
            --         , stateFullscreen = model.stateFullscreen
            --         , stateFullviewport = False
            --         , listLayers = []
            --         , listAddOn = [ MapAddOn.markerLatLong ]
            --         , msgMapper = MapMsg2
            --         , mapModel = model.mapModel2
            --         , markers = model.markers
            --         }
            --     ]
            -- , div [ style "margin" "20px", style "display" "inline-block" ]
            --     [ viewMap
            --         { supporFullviewport = False
            --         , stateFullscreen = model.stateFullscreen
            --         , stateFullviewport = False
            --         , listLayers = []
            --         , listAddOn = []
            --         , msgMapper = MapMsg2
            --         , mapModel = model.mapModel2
            --         , markers = model.markers
            --         }
            --     ]
            -- , div [ style "margin" "20px", style "display" "inline-block" ]
            --     [ viewMap
            --         { supporFullviewport = False
            --         , stateFullscreen = model.stateFullscreen
            --         , stateFullviewport = False
            --         , listLayers = []
            --         , listAddOn = []
            --         , msgMapper = MapMsg2
            --         , mapModel = model.mapModel2
            --         , markers = model.markers
            --         }
            --     ]
            ]


viewMap :
    { listAddOn : List (Map.AddOnArgs Msg -> Svg.Svg Msg)
    , listLayers : List (Html.Html Msg)
    , mapModel : Map.Model
    , msgMapper : Map.Msg -> Msg
    , stateFullscreen : Bool
    , stateFullviewport : Bool
    , supporFullviewport : Bool
    , markers : List Map.LatLng
    , extraCss : String
    , tilesSource : String
    , tileDataOrder : Map.TileDataOrder
    }
    -> Html.Html Msg
viewMap args =
    let
        _ =
            Debug.log "order" args.tileDataOrder
    in
    Html.div
        ([ Html.Attributes.style "display" "inline-block"
         , Html.Attributes.style "user-select" "none"

         -- , style "margin" "20px"
         , Html.Attributes.class "map"
         ]
            ++ (if args.stateFullviewport then
                    [ Html.Attributes.style "position" "fixed"
                    , Html.Attributes.style "top" "0"
                    , Html.Attributes.style "left" "0"
                    , Html.Attributes.style "z-index" "1"
                    ]

                else
                    [ Html.Attributes.style "position" "relative" ]
               )
        )
        ([ Map.view
            { listAddOn = args.listAddOn
            , listAddOnTransformed = listAddOnTransformed args.markers
            , extraCss = args.extraCss
            , tilesSource = args.tilesSource
            , tileDataOrder = args.tileDataOrder
            }
            args.msgMapper
            args.mapModel
         , Html.a
            [ Html.Attributes.target "_blank"
            , Html.Attributes.class "map-footer"
            , Html.Attributes.href "https://www.openstreetmap.org/copyright"
            ]
            [ Html.text "© OpenStreetMap contributors" ]
         ]
            ++ [ Html.div
                    [ Html.Events.onClick <| ToggleFullscreen
                    , Html.Attributes.attribute "title" "Full screen"
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "top" "40px"
                    , Html.Attributes.style "right" "20px"
                    , Html.Attributes.style "background" "white"
                    , Html.Attributes.style "padding" "4px"
                    , Html.Attributes.style "border-radius" "5px"
                    , Html.Attributes.style "cursor" "pointer"
                    , Html.Attributes.attribute "class" "map-button"
                    ]
                    [ (if args.stateFullscreen then
                        FeatherIcons.minimize

                       else
                        FeatherIcons.maximize
                      )
                        |> FeatherIcons.withSize 24
                        |> FeatherIcons.withStrokeWidth 1.5
                        |> FeatherIcons.toHtml [ Html.Attributes.style "vertical-align" "bottom" ]
                    ]
               ]
            ++ (if args.supporFullviewport then
                    [ Html.div
                        [ Html.Events.onClick <| ToggleFullviewport
                        , Html.Attributes.attribute "title" "Full screen"
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "top" "100px"
                        , Html.Attributes.style "right" "20px"
                        , Html.Attributes.style "background" "white"
                        , Html.Attributes.style "padding" "4px"
                        , Html.Attributes.style "border-radius" "5px"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.attribute "class" "map-button"
                        , Html.Attributes.style "width" "24px"
                        ]
                        [ (if args.stateFullviewport then
                            FeatherIcons.minimize2

                           else
                            FeatherIcons.maximize2
                          )
                            |> FeatherIcons.withSize 24
                            |> FeatherIcons.withStrokeWidth 1.5
                            |> FeatherIcons.toHtml [ Html.Attributes.style "vertical-align" "bottom" ]
                        ]
                    ]

                else
                    []
               )
            ++ args.listLayers
            ++ (if args.stateFullviewport then
                    -- [ div [ class "header" ] [ h1 [] [ viewElmLogo, text " elm-map" ] ] ]
                    [ Html.div [ Html.Attributes.class "header" ]
                        [ Html.h1 []
                            [ Html.img
                                [ Html.Attributes.width 40
                                , Html.Attributes.src "elm-map-logo.png"
                                , Html.Attributes.style "vertical-align" "middle"
                                ]
                                []
                            , Html.text " elm-map"
                            ]
                        ]
                    ]

                else
                    []
               )
        )


listAddOnTransformed : List Map.LatLng -> List (Map.AddOnArgs Msg -> Svg.Svg Msg)
listAddOnTransformed markers =
    [ markerHippo, markerLakeVictoria ]
        ++ (if largeQuantityOfIcons then
                let
                    f =
                        List.Extra.zip
                            (Map.listTouples { xFrom = 0, xTo = 24, yFrom = 0, yTo = 39 })
                            Icons.icons
                in
                List.map
                    (\( ( x, y ), icon ) ->
                        MapAddOn.markerWithIcon
                            Nothing
                            icon
                            { lat = toFloat x * 2, lng = toFloat y * 1.5 }
                    )
                    (List.reverse f)

            else
                let
                    f =
                        List.Extra.zip
                            (Map.listTouples { xFrom = 0, xTo = 3, yFrom = 0, yTo = 9 })
                            icons
                in
                List.map
                    (\( ( x, y ), icon ) ->
                        MapAddOn.markerWithIcon
                            Nothing
                            icon
                            { lat = toFloat x * 2, lng = toFloat y * 1.5 }
                    )
                    (List.reverse f)
           )
        ++ List.indexedMap (\index latLng -> MapAddOn.markerWithText Nothing ("Marker " ++ String.fromInt (index + 1)) latLng) (List.reverse markers)


mapLinks : Html.Html msg
mapLinks =
    Html.div [ Html.Attributes.class "map-footer", Html.Attributes.style "left" "0px" ]
        [ Html.a [ Html.Attributes.href "https://dev.to/lucamug", Html.Attributes.target "_blank" ] [ Html.text "dev.to" ]
        , Html.text " "
        , Html.a [ Html.Attributes.href "https://github.com/lucamug/elm-map", Html.Attributes.target "_blank" ] [ Html.text "Code" ]
        , Html.text " "
        , Html.a [ Html.Attributes.href "https://elm-map.guupa.com/debug.html" ] [ Html.text "Debug" ]
        , Html.text " "
        , Html.a [ Html.Attributes.href "https://elm-map.guupa.com/" ] [ Html.text "NoDebug" ]
        , Html.text " "
        , Html.a [ Html.Attributes.href "/" ] [ Html.text "Reset" ]
        ]


viewElmLogo : Html.Html Msg
viewElmLogo =
    Svg.svg [ SA.viewBox -280 -280 560 560, SA.class [ "elm-logo" ], SA.fill <| SA.Paint <| Color.rgb255 30 128 180 ]
        [ Svg.g [ SA.transform [ SA.Scale 1 -1 ] ]
            [ Svg.polygon [ SA.points [ ( -280, -90 ), ( 0, 190 ), ( 280, -90 ) ], SA.transform [ SA.Translate 0 -210, SA.Rotate 0 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -280, -90 ), ( 0, 190 ), ( 280, -90 ) ], SA.transform [ SA.Translate -210 0, SA.Rotate -90 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -198, -66 ), ( 0, 132 ), ( 198, -66 ) ], SA.transform [ SA.Translate 207 207, SA.Rotate -45 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -130, 0 ), ( 0, -130 ), ( 130, 0 ), ( 0, 130 ) ], SA.transform [ SA.Translate 150 0, SA.Rotate 0 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -191, 61 ), ( 69, 61 ), ( 191, -61 ), ( -69, -61 ) ], SA.transform [ SA.Translate -89 239, SA.Rotate 0 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -130, -44 ), ( 0, 86 ), ( 130, -44 ) ], SA.transform [ SA.Translate 0 106, SA.Rotate -180 0 0 ] ] []
            , Svg.polygon [ SA.points [ ( -130, -44 ), ( 0, 86 ), ( 130, -44 ) ], SA.transform [ SA.Translate 256 -150, SA.Rotate -270 0 0 ] ] []
            ]
        ]



-- CSS


css : String
css =
    """
body {
    font-family: sans-serif;
    margin: 0;
}

.map-button {
    transform: scale(1);
    opacity: 0.8;
    transition: all .1s cubic-bezier(0,0,0.2,1);
}
    
.map-button:hover {
    transform: scale(1.1);
    opacity: 1;
    transition: all .1s cubic-bezier(0,0,0.2,1);
}

a,
a:link,
a:visited,
a:hover,
a:active {
    color: black;
    text-decoration: none;
}

.header {
    position: absolute;
    top: 10px;
    left: 10px;
    pointer-events: none;
    padding: 12px 12px 10px 12px;
    background: rgba(255, 255, 255, 0.8);
    border-radius: 10px;
    color: rgb(30, 128, 180);
    user-select: none;
}

h1 {
    margin: 0px;
    padding: 20px;
    color: rgb(30, 128, 180);
}

.map h1 {
    margin: 0;
    font-size: 24px;
    font-weight: normal;
}

.elm-logo {
    height: 30px;
    width: 30px;
}

.map-footer {
    position: absolute;
    font-size: 12px;
    bottom: 0px;
    right: 0px;
    padding: 2px 4px;
    opacity: 0.5;
    width: fit-content;
    background: rgba(255, 255, 255, 1);
    user-select: none;
}
"""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map MapMsg1 (Map.subscriptions model.mapModel1)
        , Sub.map MapMsg2 (Map.subscriptions model.mapModel2)
        , Browser.Events.onResize (\w h -> GotNewViewport { width = toFloat w, height = toFloat h })
        , popState PopState
        , onToggleFullscreen OnToggleFullscreen
        ]



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--
--


markerHippo : Map.AddOnArgs Msg -> Svg.Svg Msg
markerHippo =
    MapAddOn.markerWithIcon
        (Just (ClickOnMarker "markerHippo"))
        FontAwesome.Solid.hippo
        { lat = 35.625, lng = 139.703 }


markerLakeVictoria : Map.AddOnArgs Msg -> Svg.Svg Msg
markerLakeVictoria =
    MapAddOn.markerWithText
        (Just (ClickOnMarker "markerLakeVictoria"))
        "Lake Victoria"
        { lat = -1, lng = 33 }



--
--


icons : List FontAwesome.Icon.Icon
icons =
    List.reverse
        [ FontAwesome.Solid.bicycle

        -- , FontAwesome.Solid.motorcycle
        , FontAwesome.Solid.carCrash
        , FontAwesome.Solid.carSide
        , FontAwesome.Solid.taxi
        , FontAwesome.Solid.truck
        , FontAwesome.Solid.bus
        , FontAwesome.Solid.shuttleVan
        , FontAwesome.Solid.caravan
        , FontAwesome.Solid.tractor
        , FontAwesome.Solid.train
        , FontAwesome.Solid.subway
        , FontAwesome.Solid.plane
        , FontAwesome.Solid.helicopter
        , FontAwesome.Solid.ship
        , FontAwesome.Solid.rocket

        --
        , FontAwesome.Solid.babyCarriage

        -- , FontAwesome.Solid.music
        , FontAwesome.Solid.plug
        , FontAwesome.Solid.iceCream

        -- , FontAwesome.Solid.mitten
        -- , FontAwesome.Solid.puzzlePiece
        , FontAwesome.Solid.robot

        -- , FontAwesome.Solid.school
        , FontAwesome.Solid.snowman
        , FontAwesome.Solid.anchor
        , FontAwesome.Solid.shoppingBag
        , FontAwesome.Solid.shoppingCart

        -- , FontAwesome.Solid.binoculars
        --
        , FontAwesome.Solid.building
        , FontAwesome.Solid.bullhorn
        , FontAwesome.Solid.camera
        , FontAwesome.Solid.candyCane
        , FontAwesome.Solid.carBattery
        , FontAwesome.Solid.cat
        , FontAwesome.Solid.wineGlassAlt
        , FontAwesome.Solid.chargingStation
        , FontAwesome.Solid.cloudRain
        , FontAwesome.Solid.thumbsUp
        , FontAwesome.Solid.smile
        , FontAwesome.Solid.fan
        , FontAwesome.Solid.fish

        -- , FontAwesome.Solid.guitar
        , FontAwesome.Solid.heart
        , FontAwesome.Solid.tree
        , FontAwesome.Solid.flag
        , FontAwesome.Solid.star
        ]
