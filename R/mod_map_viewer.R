# Map viewer module

# Module UI
mapViewerUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = 700)
}

# Module Server
mapViewerServer <- function(id, point) {
  moduleServer(id,
               function(input, output, session) {

    # attribution for Sentinel-2 cloudless 2016 base map
    s2mapsAttribution <- paste0(
      '<a xmlns:dct="http://purl.org/dc/terms/"',
      'href="https://s2maps.eu" property="dct:title">Sentinel-2 cloudless - ',
      'https://s2maps.eu</a> by <a xmlns:cc="http://creativecommons.org/ns#"',
      'href="https://eox.at" property="cc:attributionName" rel="cc:attributionURL">',
      "EOX IT Services GmbH</a> (Contains modified Copernicus Sentinel data 2016 &amp; 2017)"
    )

    # base map
    output$map <- renderLeaflet({
      leaflet() %>%
        setView(0, 10, 2.5) %>%
        addScaleBar(
          position = c("bottomleft"),
          options = scaleBarOptions(imperial = F)
        ) %>%
        addTiles(
          "https://tiles.maps.eox.at/wmts/1.0.0/s2cloudless_3857/default/g/{z}/{y}/{x}.jpg",
          s2mapsAttribution,
          group = "Sentinel-2 cloudless"
        ) %>%
        addTiles(group = "OpenStreetMap") %>%
        addWMSTiles(
          "https://geo.igb-berlin.de/geoserver/ows?",
          layers = "hydrography90m_v1_sub_catchment_cog",
          group = "Sub-catchments",
          options = WMSTileOptions(
            format = "image/png", transparent = TRUE,
            opacity = 0.35,
          )
        ) %>%
        addWMSTiles(
          "https://geo.igb-berlin.de/geoserver/ows?",
          layers = "hydrography90m_v1_stream_order_strahler_cog",
          group = "Stream segments",
          options = WMSTileOptions(
            format = "image/png", transparent = TRUE,
            opacity = 1.0
          )
        ) %>%
        hideGroup(c("Stream segments", "Input points", "Snapped points", "AMBER")) %>%
        addLayersControl(
          baseGroups = c("Sentinel-2 cloudless", "OpenStreetMap"),
          overlayGroups = c("Input points", "Snapped points", "Stream segments",
                            "Sub-catchments", "AMBER"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    # # Show user points on base map
    observeEvent(point(),
                 {
                   # label in the map for each point
                   labeltext <- paste("id: ", point()$id, "<br/>") %>%
                     lapply(htmltools::HTML)
                   # points
                   leafletProxy("map", data = point()) %>%
                     # start with a clear map
                     clearMarkers() %>%
                     clearControls() %>%
                     hideGroup("Snapped points") %>%
                     # add user points
                     addMarkers(
                       icon = icons(
                         iconUrl = "./img/marker_purple.png",
                         iconWidth = 25, iconHeight = 41,
                         iconAnchorX = 12, iconAnchorY = 41,
                         shadowUrl = "./img/marker-shadow.png",
                         shadowWidth = 41, shadowHeight = 41,
                         shadowAnchorX = 12, shadowAnchorY = 41
                       ),
                       lat = ~latitude,
                       lng = ~longitude,
                       label = labeltext,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "13px",
                         direction = "bottom",
                         opacity = 0.9
                       ),
                       options = markerOptions(
                         zIndexOffset = -1000
                       ),
                       group = "Input points"
                     ) %>%
                     addLegend(
                       position = "topright",
                       colors = c("#b0a2f6ff", "#ffd456ff"),
                       labels = c("Input points", "Snapped points"),
                       opacity = 1
                     ) %>%
                     # zoom map to bounding box of user points,
                     fitBounds(
                       ~ min(longitude),
                       ~ min(latitude),
                       ~ max(longitude),
                       ~ max(latitude)
                     ) %>%
                     showGroup("Input points")
                 },
                 ignoreInit = TRUE
    )
  })
}
