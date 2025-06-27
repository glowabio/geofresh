# Point editor module
library(leaflet)
library(DT)
library(sf)
library(terra)
library(dplyr)
library(htmlwidgets)
library(leaflet.extras)

# UI
pointEditorUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionLink(ns("open_modal"), "Open Point Editor")
  )
}


# Server logic
pointEditorServer <- function(id, point_user) {
  # point_user come from upload data module
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive dataframe to store points
    points <- reactiveVal(NULL)

    # Reactive value to store drawn shapes
    drawn_shape <- reactiveVal(NULL)

    # Run once when point_user() is available, and again if it ever changes
    observe({
      req(point_user())
      points(point_user())
    })

    # Show modal when clicking the actionLink
    observeEvent(input$open_modal, {
      showModal(
        modalDialog(
          size = "l",  # Large modal
          easyClose = FALSE,
          title = "Point editor",
          footer = fluidRow(column(width = 3, actionButton(ns("keep"), "Keep")),
                            column(width = 3, actionButton(ns("delete"), "Delete")),
                            column(width = 3, actionButton(ns("save"), "Save"))),
          # Modal dialogue content and close button start here
          # Position close button to the right corner
          div(style = "position: relative; padding: 20px;",

              # Custom close button from module
              modalCloseButtonUI(ns("close_bttn")),
              # Modal content starts here
              page_fluid(
                navset_tab(# Map tab
                  nav_panel("MAP",
                          leafletOutput(ns("map"), height = 600),
                          page_fluid(
                            accordion(
                              accordion_panel(
                                title = "Enter a bounding box to select points",
                                icon = bsicons::bs_icon("bounding-box-circles"),
                                p("Type bounding box coordinates"),
                                fluidRow(column(3, numericInput(ns("xmin"), "xmin:", value = 0)),
                                         column(3, numericInput(ns("ymin"), "ymin:", value = 0)),
                                         column(3, numericInput(ns("xmax"), "xmax:", value = 0)),
                                         column(3, numericInput(ns("ymax"), "ymax:", value = 0))
                                ),
                                fluidRow(fileInput(ns("sf_file"), "Or upload a *.gpkg file", accept = c(".gpkg")))
                              ), open = FALSE)),
                          icon = bsicons::bs_icon("globe-americas")
                          ),
                  # Table tab
                  nav_panel("TABLE",
                          DTOutput(ns("coord_table")),
                          icon = bsicons::bs_icon("table")
                          )
                  )
                )
              )
        )
      )
      # Server function of the close button module. Close module and reset all
      modalCloseButtonServer("close_bttn", closeAction = function() {
        removeModal()
        #points(NULL)
        # drawn_shape(NULL)
        # leafletProxy(ns("map")) %>% clearShapes()
        showNotification("Point editor closed and data reset.", type = "message")
      })

    })

    # Data Table
    output$coord_table <- renderDT({
      req(points())
      points()
    }, rownames = FALSE)

    # Render leaflet map with draggable markers

    # attribution for Sentinel-2 cloudless 2016 base map
    s2mapsAttribution <- paste0(
      '<a xmlns:dct="http://purl.org/dc/terms/"',
      'href="https://s2maps.eu" property="dct:title">Sentinel-2 cloudless - ',
      'https://s2maps.eu</a> by <a xmlns:cc="http://creativecommons.org/ns#"',
      'href="https://eox.at" property="cc:attributionName" rel="cc:attributionURL">',
      "EOX IT Services GmbH</a> (Contains modified Copernicus Sentinel data 2016 &amp; 2017)"
    )

    output$map <- renderLeaflet({
      #req(points())
      map <- leaflet() %>%
        addDrawToolbar(
          targetGroup = "draw",
          polygonOptions = drawPolygonOptions(),
          rectangleOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = drawMarkerOptions(repeatMode = TRUE),
          polylineOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions(edit = F)
        ) %>%
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
        ) %>% # Customized title for marker bottom in tool bar
        onRender("
          function(el, x) {
            setTimeout(function() {
              var toolbar = document.querySelector('.leaflet-draw-draw-marker');
              if (toolbar) {
                toolbar.title = 'Insert point';
              }
            }, 500);
          }
        ") %>%
        onRender("
          function(el, x) {
            setTimeout(function() {
              var toolbar = document.querySelector('.leaflet-draw-draw-polygon');
              if (toolbar) {
                toolbar.title = 'Draw a polygon to select points';
              }
            }, 500);
          }
        ")
      if(!is.null(points()) & is.numeric(points()$longitude)) {
        map_points <- map %>%
          addMarkers(data = points(),
                   lat = ~latitude, lng = ~longitude,
                   popup = ~paste("Lat:", latitude, "<br>Lng:", longitude),
                   layerId = ~id,
                   options = leaflet::markerOptions(draggable = TRUE))
        map_points
      } else {
        map
      }
    })

    # Draw new points
    observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature
      if (feature$geometry$type == "Point") {
        lat <- feature$geometry$coordinates[[2]]
        lng <- feature$geometry$coordinates[[1]]
        if(is.null(points())) {
          current_point <- data.frame("id" = 1, "latitude" = lat, "longitude" = lng)
          points(current_point)
        } else {
          current_points <- points()
          current_points <- current_points %>%
          add_row(id = nrow(current_points) + 1, latitude = lat, longitude = lng)
          points(current_points)
        }
      }
    })

    # Draw new polygons
    observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature
      if (feature$geometry$type == "Polygon") {
        shape_coords <- feature$geometry$coordinates[[1]]
        shape <- st_polygon(list(matrix(unlist(shape_coords), ncol = 2, byrow = TRUE))) %>%
          st_sfc(crs = 4326) %>%
          st_sf()
        drawn_shape(shape)
      }
    })

    # Handle bbox input
    observe({
      req(input$xmin, input$ymin, input$xmax, input$ymax)
      bbox_mat <- matrix(
        c(input$xmin, input$ymin,
          input$xmax, input$ymin,
          input$xmax, input$ymax,
          input$xmin, input$ymax,
          input$xmin, input$ymin),
        ncol = 2,
        byrow = TRUE
      )
      bbox_poly <- st_polygon(list(bbox_mat)) %>%
        st_sfc(crs = 4326)
      drawn_shape(bbox_poly)
    })

    # Handle file uploads
    observeEvent(input$sf_file, {
      ext <- tools::file_ext(input$sf_file$name)
      if (ext == "gpkg") {
        uploaded_sf <- st_read(input$sf_file$datapath, quiet = TRUE)
        drawn_shape(uploaded_sf)
      } else {
        showNotification("Unsupported file format", type = "error")
      }
    })

    # Render polygons on map
    observe({
      req(drawn_shape())
      leafletProxy(ns("map")) %>%
        clearShapes() %>%
        addPolygons(data = drawn_shape(), color = "blue", fillOpacity = 0.4)
    })

    # Delete points inside shape
    observeEvent(input$delete, {
      points_data <- points()  # Evaluate once and reuse
      shape <- drawn_shape()   # Evaluate once and reuse

      if (!is.null(points_data) && !is.null(shape)) {
        if (nrow(points_data) > 0) {
          points_sf <- st_as_sf(points_data,
                                coords = c("longitude", "latitude"),
                                crs = 4326)

          # Additional shape checks here
          if (!st_is_empty(shape) && all(st_is_valid(shape))) {
            inside <- st_within(points_sf, shape, sparse = FALSE)[,1]
            if (length(inside) == nrow(points_sf) && is.logical(inside)) {
              filtered <- points_sf[!inside, ]
              filtered_df <- as.data.frame(filtered) %>%
                dplyr::select(-geometry)
              filtered_df$longitude <- st_coordinates(filtered)[,1]
              filtered_df$latitude <- st_coordinates(filtered)[,2]
              points(filtered_df)
              # Message when there are not points inside the polygon
              if (!all(inside)) {
                showNotification("Nothing to delete.", type = "error")
              }
            } else {
              showNotification("No valid shape found to filter points.",
                               type = "error")
            }
          } else {
            showNotification("No valid shape drawn to filter points.",
                             type = "error")
          }
        } else {
          showNotification("No points to filter.",
                           type = "error")
        }
      } else {
        showNotification("No points or shape to filter.",
                         type = "error")
      }
    })



    # Keep points inside shape
    observeEvent(input$keep, {
      if (!is.null(drawn_shape())) {
        req(points())

        points_sf <- st_as_sf(points(),
                              coords = c("longitude", "latitude"),
                              crs = 4326)

        # Check drawn shape is not empty
        if (nrow(points_sf) > 0 && !st_is_empty(drawn_shape())) {
          inside <- st_within(points_sf, drawn_shape(), sparse = FALSE)[,1]
          if (length(inside) == nrow(points_sf) && is.logical(inside)) {
            filtered <- points_sf[inside, ]
            filtered_df <- as.data.frame(filtered) %>%
              dplyr::select(-geometry)
            filtered_df$longitude <- st_coordinates(filtered)[,1]
            filtered_df$latitude <- st_coordinates(filtered)[,2]
            points(filtered_df)
          } else {
            showNotification("No valid shape found to filter points.",
                             type = "error")
          }
        } else {
          showNotification("No points or shape to filter.",
                           type = "error")
        }
      }
    })


    # Update marker positions
    observeEvent(input$map_marker_dragend, {
      req(points())
      drag_info <- input$map_marker_dragend
      current_points <- points()
      idx <- which(current_points$id == drag_info$id)
      current_points[idx, c("latitude", "longitude")] <- c(round(drag_info$lat, 5),
                                                           round(drag_info$lng, 5))
      points(current_points)
    })

    # Return edited points
    return(points)
  })
}

