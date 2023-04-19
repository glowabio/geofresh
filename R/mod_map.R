# This module creates a map with points

library(leaflet)
library(leaflet.extras)

# Module UI function
mapOutput <- function(id, label = "maprecords") {
  ns <- NS(id)
  tagList(
    # map
    leafletOutput(ns("map"), height = 1000)
    # layer control panel
    # absolutePanel(
    #   top = 20, right = 20, width = 300,
    #   draggable = FALSE,
    #   wellPanel(
    #     checkboxInput(ns("inputpoints"), "Input points", TRUE),
    #     checkboxInput(ns("snappedoints"), "Snapped points", TRUE)
    #
    #   ),
    #   style = "opacity: 0.7"
    # )

  )
}

# Module server function. The parameter "point" must be a list with two data frames,
# These list comes from the module that upload a CSV file
#
mapServer <- function(id, point) {
  moduleServer(
    id,
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
          addTiles("https://tiles.maps.eox.at/wmts/1.0.0/s2cloudless_3857/default/g/{z}/{y}/{x}.jpg", s2mapsAttribution) %>%
          setView(0, 10, 2.5) %>%
          addScaleBar(
            position = c("bottomleft"),
            options = scaleBarOptions(imperial = F)
          ) %>%
          addLegend(position = "topright",
                    colors = c("blue", "red"),
                    labels = c("Input points", "Snapped points"))
      })

      # Show user points on base map
      observeEvent(point$user_points(), {
        # label in the map for each point
        labeltext <- paste("id: ", point$user_points()$id, "<br/>") %>%
          lapply(htmltools::HTML)
        # points
        #user_points <-
          leafletProxy("map", data = point$user_points()) %>%
          addCircleMarkers(
            data = point$user_points(),
            lng = ~longitude,
            lat = ~latitude,
            fillColor = "blue",
            fillOpacity = 0.7, color = "black",
            radius = 5,
            stroke = T,
            weight = 0.3,
            label = labeltext,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "bottom",
              opacity = 0.9
            ),
            group = "inputpoints"
          ) %>%
            addWMSTiles(
              "https://geo.igb-berlin.de/geoserver/ows?",
              layers = "hydrography90m_v1_sub_catchment_cog",
              options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                       opacity = 0.35,
                                       group = "Subcatchments")
            )
        # observe({
        #   if (input$inputpoints == TRUE) {
        #   user_points %>% showGroup("inputpoints")
        # } else {
        #   user_points %>% hideGroup("inputpoints")
        # }
        # })

      })

      # Show snapping points on base map
      observeEvent(point$snap_points(), {
        # label in the map for each point
        labeltext <- paste("id: ", point$snap_points()$id, "<br/>") %>%
          lapply(htmltools::HTML)
        # points
        #snap_points <-
        leafletProxy("map", data = point$snap_points()) %>%
          addCircleMarkers(
            data = point$snap_points(),
            lng = ~new_longitude,
            lat = ~new_latitude,
            fillColor = "red",
            fillOpacity = 0.7, color = "black",
            radius = 5,
            stroke = T,
            weight = 0.3,
            label = labeltext,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "bottom",
              opacity = 0.9
            ),
            group = "snappedpoints"
          )

        # observe({
        #   if (input$snappedoints == TRUE) {
        #     snap_points %>% showGroup("snappedpoints")
        #   } else {
        #     snap_points %>% hideGroup("snappedpoints")
        #   }
        # })
      })
    }
  )
}
