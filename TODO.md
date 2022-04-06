# TODO

* Implement the ZXY type
* Fix the layout to be mobile
* Add multiple copyrights
* Restore the capability to manually edit a tiles source

----

* Consider to have two layers, so that after zoom we don't remove the previous layer to avoid all the map to became gray
* Add inertia to mobile
* Fix the issue that when inertia is stopped by long click, dragging doesn't workI’m 
* Finish with marker, add Lat/Lng boundaries and some pre-calculated value. Also change icon library
* Add "+" "-" zoom buttons as addOn



* Avoid placing markers if they are out of bound (latLng)
* Avoid calculating bxbyWhenEnteredThisZoom, as this is fixed (should be derived from cache)
* Pass "scaleMarker" as argument, to avoid re-calculating over and over (should be part of "situation")

viewport and delta only change when user interaction stop (cache change)
scaleMarker change when user zoom

Create a function that add markers to a map, that uses a cache system
so no values are calculated over and over.

* Transition are calculated only when user stop interaction
* Scale is either calculate when user stop interaction, or continuosly

-- I need something like this:
--
-- Map.AddOnArgs Msg -> Svg.Svg Msg
--
-- type alias AddOnArgs msg =
--     { viewport : Viewport
--     , situation : Situation
--     , cache : Cache
--     , zoomAndPanEvents : List (Attribute msg)
--     }
--
-- addMarkersToMap : List a -> Map.Cache -> Bool -> Map.AddOnArgs Msg -> Svg.Svg Msg
-- addMarkersToMap markers oldCache updateOnZoom args =
--     if args.cache == oldCache then
--         if updateOnZoom then
--             Debug.todo "Need to update on zoom"
--
--         else
--             Debug.todo "Done"
--
--     else
--         Debug.todo "Need to update the cacheMarkers"
--
        --
        -- TODO
        --
        --  * Avoid placing markers if they are out of bound (latLng)
        --
-- TODO Make the path to respect debug.html
--   * When switching
--   * During normal use
--
-- 3 levels of updates
--
-- 1. Always (for example, the tip that show lat lng in real time)
-- 2. During zoom only (for example, resizing markers) -> Only change zoom of markers (also during panning?)
-- 3. After user interactions (everything) -> Recalculate all markers





# Tiles

## Free

Standard
https://tile.openstreetmap.org/15/29100/12909.png

German Fork
https://a.tile.openstreetmap.de/15/29100/12909.png

Humanitarian map style
http://a.tile.openstreetmap.fr/hot/15/29100/12909.png

OSM France
http://a.tile.openstreetmap.fr/osmfr/15/29100/12909.png

Stamen Toner - Black and White
https://stamen-tiles.a.ssl.fastly.net/toner/15/29100/12909.png
https://stamen-tiles-c.a.ssl.fastly.net/toner/15/29100/12909@2x.png

Stamen Watercolor
http://c.tile.stamen.com/watercolor/15/29100/12909.png

Öpnvkarte
http://tile.memomaps.de/tilegen/15/29100/12909.png

Dark
https://a.tile.opentopomap.org/15/29100/12909.png


pk.eyJ1IjoibHVjYW11ZyIsImEiOiJjbDB5dG00ZnQwazhwM2tvMHZ4c25xaGhrIn0.l_p01v4S6Aj3dQbYszzlYw

https://api.mapbox.com/styles/v1/mapbox/streets-v11/tiles/12/2046/1361?access_token=pk.eyJ1IjoibHVjYW11ZyIsImEiOiJjbDB5dG00ZnQwazhwM2tvMHZ4c25xaGhrIn0.l_p01v4S6Aj3dQbYszzlYw
https://api.mapbox.com/styles/v1/mapbox/dark-v10/tiles/12/2046/1361?access_token=pk.eyJ1IjoibHVjYW11ZyIsImEiOiJjbDB5dG00ZnQwazhwM2tvMHZ4c25xaGhrIn0.l_p01v4S6Aj3dQbYszzlYw
https://api.mapbox.com/styles/v1/mapbox/navigation-night-v1/tiles/12/2046/1361?access_token=pk.eyJ1IjoibHVjYW11ZyIsImEiOiJjbDB5dG00ZnQwazhwM2tvMHZ4c25xaGhrIn0.l_p01v4S6Aj3dQbYszzlYw

https://docs.mapbox.com/api/maps/styles/#mapbox-styles



https://github.com/mapbox/awesome-vector-tiles

https://leaflet-extras.github.io/leaflet-providers/preview/

https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/10/908/402@2x.png


https://a.tile.thunderforest.com/transport/13/7272/3228@2x.png?apikey=6170aad10dfd42a38d4d8c709a536f38