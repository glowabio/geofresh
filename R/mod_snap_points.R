# This module allows the user to snap points to a stream network

# Module UI function
snapPointUI <- function(id, label = "Snap points") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  tagList(
    uiOutput(ns("snap_2_ntw_meth")),
  )
}


# Module server function
snapPointServer <- function(id, input_points) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input_points, {
        output$snap_2_ntw_meth <- renderUI({
          tagList(
            hr(),
            textInput("distance",
                      "Enter a snapping distance in meters or use default value",
                      value = 500),
            actionButton(ns("snap_button"),
                         label = "Snap points")
          )
        }
        )
      })

  # If click snap button, snap points, otherwise do nothing
        coordinates_snap <- eventReactive(input$snap_button, {
          # Dummy dataframe with coordinates to use as if they were the result of
          # snapping. (substitute these lines with the true snapping
          # script)
          snapped_coord <- data.frame("new_longitude" = c(-77.99, 13.40876,
                                                          15.4187255, 15.3187255,
                                                          15.5187255),
                                      "new_latitude" = c(21.633333, 52.5536535,
                                                         52.5536535, 52.3536535,
                                                         52.5336535))

          # Append New columns with new coordinates that resulted from snapping
          new_data <- data.frame(input_points, snapped_coord)
        })
    }
  )
}





