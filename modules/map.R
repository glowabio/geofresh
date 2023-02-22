# This module creates a map with points

library(shiny)
library(leaflet)
library(leaflet.extras)

# Module UI function
mapOutput <- function(id, label = 'maprecords') {
  ns <- NS(id)
  tagList(
    # map
    leafletOutput(ns('map'), height=1000)
  )
}

# Module server function. the parameter record must be a data frame with three
# columns: ids,  decimalLongitude and decimalLatitude. This data frame comes from
# the module that upload a CSV file
#
mapServer <- function(id, records){
  moduleServer(
    id,
    function(input, output, session) {

      # base map
      output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldImagery)%>%
          setView(0,10,2.5)%>%
          addScaleBar(position = c("bottomleft"),
                      options = scaleBarOptions(imperial = F))
      })

      # # If no CSV is uploaded, don't do anything, otherwise create a reactive
      # expression with coordinates
      point <- reactive({
        validate(need(records, message = FALSE))
        records
      })
      # Show points on base map. Use coordinates from point()
      observe({
        # label in the map for each point
        labeltext <- paste("id: ", point()$ids, "<br/>") %>%
          lapply(htmltools::HTML)
        # points
        leafletProxy("map", data = point()) %>%
            addCircleMarkers(data = point(),
              lng = ~decimalLongitude,
              lat = ~decimalLatitude,
              fillColor =  "blue",
              fillOpacity = 0.7, color="black",
              radius=5,
              stroke=T,
              weight = 0.3,
              label = labeltext,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "bottom",
                opacity = 0.9)
            )
      })

    }
  )
}
