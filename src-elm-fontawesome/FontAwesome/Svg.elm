module FontAwesome.Svg exposing
    ( viewIcon
    , viewIconNEW
    )

{-| Rendering icons for use in SVG.

@docs viewIcon

-}

import FontAwesome.Icon exposing (Icon)
import FontAwesome.Svg.Internal as Internal
import Svg exposing (Svg)


{-| View an icon as an SVG node.
-}
viewIconNEW : Icon -> ( Svg msg, Int, Int )
viewIconNEW =
    Internal.corePathsNEW []


{-| View an icon as an SVG node.
-}
viewIcon : Icon -> Svg msg
viewIcon =
    Internal.corePaths []
