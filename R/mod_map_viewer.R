# Map viewer module
library(later)
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
        hideGroup(c("Stream segments", "Input points", "Snapped points")) %>%
        addLayersControl(
          baseGroups = c("Sentinel-2 cloudless", "OpenStreetMap"),
          overlayGroups = c("Input points", "Snapped points", "Stream segments",
                            "Sub-catchments", "AMBER", "AMBER-snapped"),
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

    # Data frame with coordinates and other attributes associated with AMBER
    # barriers. Change amber_df with the actual data from the Postgres database.
    # subcatchmentID: Hydorgraphy90m subcatchments, longitude_snap: longitude
    # after snapping, longitude_snap: latitude after snapping
    amber_df <- data.frame(
      guid = c(
        "{674921F0-B0D2-49BB-AA8F-7DC271835737}",
        "{E60A994D-C61C-4B5B-A382-009BA82A648E}",
        "{05907C76-D150-4073-9401-5505665510CE}",
        "{82DD31B5-926C-4163-B51A-8F854DDA470B}",
        "{43ADEF00-3A56-47F1-BB61-E9082626847D}",
        "{DFED46EC-5CF2-4E4B-B0AB-3196EC89DBCF}",
        "{2BD8514A-F00D-4534-86B6-4304806132A8}",
        "{622EE7DD-C72D-4B76-A0A1-E17A19C024F2}",
        "{30B50A42-5C23-4E12-A287-F0081DB4E071}",
        "{C8A986EE-F555-4610-9B89-394053D6D909}",
        "{A2B6CF7A-A8A6-49FB-B752-0D78816A76F0}",
        "{88B53E11-AF68-49C1-A0FF-B60D9CD2344C}"
      ),
      longitude_amber = c(12.223801, 12.22398, 12.426756, 12.42677, 11.767796, 11.768494,
                          11.869142, 11.869096, 11.198487, 10.952253, 14.152148, 13.819286),
      latitude_amber = c(54.254036, 54.25379, 54.245726, 54.24546, 54.152089, 54.15154,
                         54.147318, 54.147297, 53.99483, 53.973262, 53.968233, 53.841247),
      longitude_snap = c(12.2239, 12.2241, 12.4268, 12.4269, 11.7679, 11.7686,
                         11.8693, 11.8692, 11.1986, 10.9524, 14.1523, 13.8194),
      latitude_snap = c(54.2541, 54.2539, 54.2458, 54.2456, 54.1522, 54.1516,
                        54.1474, 54.1473, 53.9950, 53.9734, 53.9684, 53.8414),
      subcatchmentID = paste0("SUB_", 1:12),
      project = rep("AMBER_GERMANY_MECKLENBURG_VORPOMMERN", 12),
      country = rep("GERMANY", 12),
      type = rep("ramp", 12),
      height = c(1.1, 0.5, 1.4, 1.4, NA, 1.5, 1.9, 1.9, NA, 1.35, 1.9, 2),
      stringsAsFactors = FALSE
    )


    # Show AMBER points in map
    observe({
      labeltext_amber <- paste0("GUID: ", amber_df$guid, "<br/>",
                                "Type: ", amber_df$type, "<br/>",
                                "Height: ", amber_df$height) %>%
        lapply(htmltools::HTML)

      leafletProxy("map", data = amber_df) %>%
        addCircleMarkers(
          lng = ~longitude_amber,
          lat = ~latitude_amber,
          radius = 6,
          layerId = ~guid,
          color = "#FF5722",
          stroke = TRUE,
          fillOpacity = 0.8,
          label = labeltext_amber,
          group = "AMBER"
        ) %>%
        showGroup("AMBER")
    })

  # Show snapped AMBER in map
    observe({
      labeltext_amber_snap <- paste0("GUID: ", amber_df$guid, "<br/>",
                                "SubcatchmentID: ", amber_df$subcatchmentID, "<br/>",
                                "Type: ", amber_df$type, "<br/>",
                                "Height: ", amber_df$height) %>%
        lapply(htmltools::HTML)

      leafletProxy("map", data = amber_df) %>%
        addCircleMarkers(
          lng = ~longitude_snap,
          lat = ~latitude_snap,
          radius = 6,
          layerId = ~paste0("snapped_", guid),
          color = "#2196F3",
          stroke = TRUE,
          fillOpacity = 0.8,
          label = labeltext_amber_snap,
          group = "AMBER-snapped"
        ) %>%
        showGroup("AMBER-snapped")
    })

  # Indicates AMBER original point associated with AMBER-snapped point when
  # clicking on an AMBER-snapped point
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      # Only respond to clicks on AMBER-snapped points
      if (!is.null(click$id) && startsWith(click$id, "snapped_")) {
        guid_clicked <- gsub("snapped_", "", click$id)
        matched <- amber_df[amber_df$guid == guid_clicked, ]
        if (nrow(matched) == 1) {
          leafletProxy("map") %>%
            # Center the map on the original AMBER point
            flyTo(
              lng = matched$longitude_amber,
              lat = matched$latitude_amber,
              zoom = 14
            ) %>%
            # Clear any previous popups and highlight
            clearPopups() %>%
            removeShape("snap_line") %>%
            removeMarker(layerId = "highlight_marker") %>%
            # Show popup on the original AMBER point
            addPopups(
              lng = matched$longitude_amber,
              lat = matched$latitude_amber,
              popup = paste0(
                "<b>GUID:</b> ", matched$guid, "<br/>",
                "<b>Type:</b> ", matched$type, "<br/>",
                "<b>Height:</b> ", matched$height
              ),
              layerId = paste0("popup_", matched$guid)
            ) %>%
            # Highlight the original AMBER point
            addCircleMarkers(
              lng = matched$longitude_amber,
              lat = matched$latitude_amber,
              radius = 10,
              color = "#FFFF00",
              weight = 3,
              stroke = TRUE,
              fillOpacity = 0.9,
              group = "highlight",
              layerId = "highlight_marker"
            ) %>%
            # Draw a line connecting snapped and original points
            addPolylines(
              lng = c(matched$longitude_snap, matched$longitude_amber),
              lat = c(matched$latitude_snap, matched$latitude_amber),
              color = "#FFC107",  # amber yellow
              weight = 3,
              opacity = 1,
              group = "highlight",
              layerId = "snap_line"
            )

          # Remove the highlight and line after 3 seconds
          # later::later(function() {
          #   leafletProxy("map") %>%
          #     removeMarker(layerId = "highlight_marker") %>%
          #     removeShape("snap_line")
          # }, delay = 3)
        }
      }
    })



  })
}
