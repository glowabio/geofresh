# Map viewer module
library(later)
# Module UI
mapViewerUI <- function(id, height) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = height)
}

# Module Server
# Requires: library(leaflet.extras) somewhere in your app

mapViewerServer <- function(id, point, show_toolbar = FALSE) {
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
      m <- leaflet() %>%
        setView(0, 10, 2.5) %>%
        addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial = FALSE)) %>%
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
          options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 0.35)
        ) %>%
        addWMSTiles(
          "https://geo.igb-berlin.de/geoserver/ows?",
          layers = "hydrography90m_v1_stream_order_strahler_cog",
          group = "Stream segments",
          options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 1.0)
        ) %>%
        hideGroup(c("Stream segments", "Input points", "Snapped points")) %>%
        addLayersControl(
          baseGroups    = c("Sentinel-2 cloudless", "OpenStreetMap"),
          overlayGroups = c("Input points", "Snapped points", "Stream segments",
                            "Sub-catchments", "AMBER", "AMBER-snapped"),
          options       = layersControlOptions(collapsed = FALSE)
        )

      if (isTRUE(show_toolbar)) {
        m <- m %>%
          addDrawToolbar(
            targetGroup = "draw",
            polygonOptions      = drawPolygonOptions(),
            rectangleOptions    = FALSE,
            circleOptions       = FALSE,
            markerOptions       = drawMarkerOptions(repeatMode = TRUE),
            polylineOptions     = FALSE,
            circleMarkerOptions = FALSE,
            editOptions         = editToolbarOptions(edit = FALSE)
          ) %>%
          # nice-to-have tooltips on the draw buttons
          htmlwidgets::onRender("
            function(el, x) {
              setTimeout(function() {
                var mk = document.querySelector('.leaflet-draw-draw-marker');
                if (mk) mk.title = 'Insert point';
                var pg = document.querySelector('.leaflet-draw-draw-polygon');
                if (pg) pg.title = 'Draw a polygon';
              }, 500);
            }
          ")
      }

      m
    })

    # Show user points (and snapped points if present)
    observeEvent(point(), {
      req(point())
      df <- point()

      has_snapped <- all(c("latitude_snap", "longitude_snap") %in% names(df)) &&
        any(is.finite(df$latitude_snap) & is.finite(df$longitude_snap))

      lbl_input <- lapply(paste0("id: ", df$id), htmltools::HTML)
      if (has_snapped) {
        lbl_snap <- lapply(paste0("id: ", df$id, " (snapped)"), htmltools::HTML)
      }

      icon_input <- icons(
        iconUrl = "./www/img/marker-icon-violet.png",
        iconWidth = 25, iconHeight = 41,
        iconAnchorX = 12, iconAnchorY = 41,
        shadowUrl = "./www/img/marker-shadow.png",
        shadowWidth = 41, shadowHeight = 41,
        shadowAnchorX = 12, shadowAnchorY = 41
      )
      icon_snap <- icons(
        iconUrl = "./www/img/marker-icon-yellow.png",
        iconWidth = 25, iconHeight = 41,
        iconAnchorX = 12, iconAnchorY = 41,
        shadowUrl = "./www/img/marker-shadow.png",
        shadowWidth = 41, shadowHeight = 41,
        shadowAnchorX = 12, shadowAnchorY = 41
      )

      proxy <- leafletProxy("map", data = df) %>%
        clearGroup("Input points") %>%
        clearGroup("Snapped points")

      proxy <- proxy %>%
        addMarkers(
          lat = ~latitude, lng = ~longitude,
          label = lbl_input,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px", direction = "bottom", opacity = 0.9
          ),
          options = markerOptions(zIndexOffset = -1000),
          icon = icon_input,
          group = "Input points"
        ) %>%
        showGroup("Input points")

      if (has_snapped) {
        proxy <- proxy %>%
          addMarkers(
            lat = ~latitude_snap, lng = ~longitude_snap,
            label = lbl_snap,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px", direction = "bottom", opacity = 0.9
            ),
            icon = icon_snap,
            group = "Snapped points"
          ) %>%
          showGroup("Snapped points")
      } else {
        proxy <- proxy %>% hideGroup("Snapped points")
      }

      proxy <- proxy %>%
        removeControl("points-legend") %>%
        addLegend(
          position = "topright",
          colors = c("#9C2BCB", "#ffd456"),
          labels = c("Input points", "Snapped points"),
          opacity = 1,
          layerId = "points-legend"
        )

      # Fit bounds
      if (has_snapped) {
        lats <- df$latitude_snap[is.finite(df$latitude_snap)]
        lngs <- df$longitude_snap[is.finite(df$longitude_snap)]
      } else {
        lats <- df$latitude[is.finite(df$latitude)]
        lngs <- df$longitude[is.finite(df$longitude)]
      }
      if (length(lats) >= 1 && length(lngs) >= 1) {
        if (length(lats) == 1) {
          proxy %>% setView(lng = lngs[1], lat = lats[1], zoom = 10)
        } else {
          proxy %>% fitBounds(min(lngs), min(lats), max(lngs), max(lats))
        }
      }
    }, ignoreInit = TRUE)

  })
}
