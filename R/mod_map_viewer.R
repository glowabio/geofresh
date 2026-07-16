# Map viewer module
library(later)
# Module UI
mapViewerUI <- function(id, height) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = height)
}

# Module Server
# Requires: library(leaflet.extras) somewhere in your app

mapViewerServer <- function(id, points_db, last_path_to_outlet, last_upstream_catchment, barrier_points, show_toolbar = FALSE) {
  moduleServer(id, function(input, output, session) {

    #observeEvent(points_db(), {
    #  showNotification("DEBUG: map viewer: points_db changed")
    #})

    # attribution for Sentinel-2 cloudless 2024 base map
    s2mapsAttribution <- paste0(
      '<a xmlns:dct="http://purl.org/dc/terms/"',
      'href="https://s2maps.eu" property="dct:title">Sentinel-2 cloudless 2024 - ',
      'https://s2maps.eu</a> by <a xmlns:cc="http://creativecommons.org/ns#"',
      'href="https://eox.at" property="cc:attributionName" rel="cc:attributionURL">',
      "EOX IT Services GmbH</a> (Contains modified Copernicus Sentinel data 2024)"
    )

    # base map
    output$map <- renderLeaflet({
      m <- leaflet() %>%
        setView(0, 10, 2.5) %>%
        addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial = FALSE)) %>%
        addTiles(
          "https://tiles.maps.eox.at/wmts/1.0.0/s2cloudless-2024_3857/default/g/{z}/{y}/{x}.jpg",
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
                            "Sub-catchments"),#, "AMBER", "AMBER-snapped"),
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

    # Show user points (and snapped points if present).
    # The reactive "points_db()" was passed into mapViewerServer
    # from app.R, where it is called "points_db()" too
    observeEvent(points_db(), {
      req(points_db())
      #showNotification("DEBUG: map viewer: points_db changed.")
      df <- points_db()

      has_snapped <- all(c("latitude_snap", "longitude_snap") %in% names(df)) &&
        any(is.finite(df$latitude_snap) & is.finite(df$longitude_snap))

      lbl_input <- lapply(paste0("id: ", df$id), htmltools::HTML)
      if (has_snapped) {
        lbl_snap <- lapply(paste0("id: ", df$id, " (snapped)"), htmltools::HTML)
      }

      # Define icons for original and snapped points:
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

      # Remove all old icons for original and snapped points
      # Also remove downstream paths and upstream catchments
      proxy <- leafletProxy("map", data = df) %>%
        clearGroup("Input points") %>%
        clearGroup("Snapped points") %>%
        clearGroup("routes") %>%
        clearGroup("catchments")

      # Add icons for original points to map
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

      # Add icons for snapped points (plus lines) to map
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

        # Add connecting lines from original to snapped points
        for (i in seq_len(nrow(df))) {
          lat_orig <- df$latitude[i]
          lon_orig <- df$longitude[i]
          lat_snap <- df$latitude_snap[i]
          lon_snap <- df$longitude_snap[i]

          # Only draw line if both points are valid
          if (is.finite(lat_orig) && is.finite(lon_orig) &&
              is.finite(lat_snap) && is.finite(lon_snap)) {
            proxy <- proxy %>%
              addPolylines(
                lng = c(lon_orig, lon_snap),
                lat = c(lat_orig, lat_snap),
                color = "#666666",
                weight = 1.5,
                opacity = 0.6,
                dashArray = "5, 5",
                group = "Snapped points"
              )
          }
        }
      } else {
        # if no snapped points, then hide the group!
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
    # end of: observeEvent(points_db()...


    # The reactive last_path_to_outlet contains the sf objects, one by one.
    # Whenever a new path to outlet is returned from the pygeoapi server,
    # this code gets triggered and the path is drawn.
    # TODO: This way, we never hold all of them in a variable! Maybe rather
    # store them in a list?
    observeEvent(last_path_to_outlet(), {
      #showNotification("DEBUG: map viewer: event paths to outlet")
      req(last_path_to_outlet())
      proxy <- leafletProxy("map")

      # plot one by one:
      one_path_sf <- last_path_to_outlet()
      proxy %>%
        addPolylines(
          data = one_path_sf,
          color = "blue",
          weight = 3,
          group = "routes"
        )

      # If we stored them in a list, maybe this could be a way:
      #proxy %>% clearGroup("routes")
      #lapply(paths_to_outlet(), function(sf_obj) {
      #  proxy %>%
      #    addPolylines(
      #      data = sf_obj,
      #      color = "blue",
      #      weight = 3,
      #      group = "routes"
      #    )
      # })
     })

    # The reactive last_upstream_catchment contains the sf objects, one after the other.
    # Whenever a new upstream catchment is returned from the pygeoapi server,
    # this code gets triggered and the polygon is drawn.
    observeEvent(last_upstream_catchment(), {
      #showNotification("DEBUG: map viewer: event upstream catchment to be drawn")
      req(last_upstream_catchment())
      proxy <- leafletProxy("map")

      # upstream catchment can be polygons or linestrings
      # Note: It will always just find of the two, so without suppressWarnings, the log
      # is filled with irrelevant warnings.
      polygons    <- suppressWarnings(st_collection_extract(last_upstream_catchment(), "POLYGON"))
      linestrings <- suppressWarnings(st_collection_extract(last_upstream_catchment(), "LINESTRING"))

      # plot polygons if any were in the geometry collection:
      if (nrow(polygons) > 0) {
        proxy %>%
          addPolygons(
            data = polygons,
            color = "blue",
            weight = 3,
            fillColor = "blue",
            fillOpacity = 0.2,
            group = "catchments"
          )
      }

      # plot linestrings if any were in the geometry collection:
      if (nrow(linestrings) > 0) {
        proxy %>%
          addPolylines(
            data = linestrings,
            color = "blue",
            weight = 3,
            group = "catchments"
          )
      }
    })

    # ...
    observeEvent(barrier_points(), {
      #showNotification("DEBUG: barriers to be shown")
      barriers_sf <- barrier_points()
      req(barriers_sf)
      proxy <- leafletProxy("map")
      proxy <- proxy %>%
        clearGroup("barriers") %>%
	addCircleMarkers(
          data = barriers_sf,
          group = "barriers",
          radius = 5,
          color = "red",
          fillOpacity = 0.8
        ) %>%
        showGroup("barriers")
    })

  })
}
