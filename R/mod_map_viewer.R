# Map viewer module

# Module UI
mapViewerUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = 700)
}

# Module Server
mapViewerServer <- function(id) {
  moduleServer(id, function(input, output, session) {

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
  })
}
