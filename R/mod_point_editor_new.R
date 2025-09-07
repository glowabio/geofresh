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

    # Working copy used inside the modal
    working_points <- reactiveVal(NULL)
    drawn_shape    <- reactiveVal(NULL)

    # Initialize working copy when source data changes
    observe({
      req(point_user())
      # only copy if truly different to avoid noisy updates
      if (is.null(working_points()) ||
          !isTRUE(all.equal(working_points(), point_user()))) {
        working_points(point_user())
      }
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

    # TABLE
    output$coord_table <- DT::renderDT({
      req(working_points())
      working_points()
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
      if (!is.null(working_points()) && is.numeric(working_points()$longitude)) {
        map %>%
          addMarkers(
            icon = icons(
              iconUrl = "./www/img/marker-icon-violet.png",
              iconWidth = 25, iconHeight = 41,
              iconAnchorX = 12, iconAnchorY = 41,
              shadowUrl = "./www/img/marker-shadow.png",
              shadowWidth = 41, shadowHeight = 41,
              shadowAnchorX = 12, shadowAnchorY = 41
            ),
            data = working_points(),
            lat = ~latitude, lng = ~longitude,
            popup = ~paste("Lat:", latitude, "<br>Lng:", longitude),
            layerId = ~id,
            options = leaflet::markerOptions(draggable = TRUE)
          )
      } else {
        map
      }
    })

    # Draw new features (merge your two observers into one)
    observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature
      type <- feature$geometry$type

      if (type == "Point") {
        lat <- feature$geometry$coordinates[[2]]
        lng <- feature$geometry$coordinates[[1]]
        cur <- working_points()
        if (is.null(cur)) {
          cur <- data.frame(id = 1, latitude = lat, longitude = lng)
        } else {
          cur <- dplyr::add_row(cur, id = nrow(cur) + 1, latitude = lat, longitude = lng)
        }
        working_points(cur)
      }

      if (type == "Polygon") {
        shape_coords <- feature$geometry$coordinates[[1]]
        shape <- st_polygon(list(matrix(unlist(shape_coords), ncol = 2, byrow = TRUE))) |>
          st_sfc(crs = 4326) |>
          st_sf()
        drawn_shape(shape)
      }
    })

    # BBox inputs -> drawn_shape
    observe({
      req(input$xmin, input$ymin, input$xmax, input$ymax)
      bbox_mat <- matrix(c(
        input$xmin, input$ymin,
        input$xmax, input$ymin,
        input$xmax, input$ymax,
        input$xmin, input$ymax,
        input$xmin, input$ymin
      ), ncol = 2, byrow = TRUE)
      bbox_poly <- st_polygon(list(bbox_mat)) |> st_sfc(crs = 4326)
      drawn_shape(bbox_poly)
    })

    # Upload shape file
    observeEvent(input$sf_file, {
      ext <- tools::file_ext(input$sf_file$name)
      if (tolower(ext) == "gpkg") {
        drawn_shape(st_read(input$sf_file$datapath, quiet = TRUE))
      } else {
        showNotification("Unsupported file format", type = "error")
      }
    })

    # Render polygons
    observe({
      req(drawn_shape())
      leafletProxy(ns("map")) %>% clearShapes() %>% addPolygons(data = drawn_shape(), color = "blue", fillOpacity = 0.4)
    })

    # DELETE inside shape
    observeEvent(input$delete, {
      pts <- working_points()
      shp <- drawn_shape()
      if (is.null(pts) || is.null(shp) || nrow(pts) == 0 || st_is_empty(shp)) {
        showNotification("No points or valid shape to filter.", type = "error")
        return()
      }
      pts_sf <- st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326)
      inside <- st_within(pts_sf, shp, sparse = FALSE)[, 1]
      if (!any(inside)) {
        showNotification("Nothing to delete.", type = "message")
        return()
      }
      kept <- pts_sf[!inside, ]
      kept_df <- kept |> as.data.frame() |> dplyr::select(-geometry)
      kept_df$longitude <- st_coordinates(kept)[, 1]
      kept_df$latitude  <- st_coordinates(kept)[, 2]
      working_points(kept_df)
    })

    # KEEP inside shape
    observeEvent(input$keep, {
      req(drawn_shape(), working_points())
      pts <- working_points()
      pts_sf <- st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326)
      if (nrow(pts_sf) == 0 || st_is_empty(drawn_shape())) {
        showNotification("No points or valid shape to keep.", type = "error")
        return()
      }
      inside <- st_within(pts_sf, drawn_shape(), sparse = FALSE)[, 1]
      kept <- pts_sf[inside, ]
      kept_df <- kept |> as.data.frame() |> dplyr::select(-geometry)
      kept_df$longitude <- st_coordinates(kept)[, 1]
      kept_df$latitude  <- st_coordinates(kept)[, 2]
      working_points(kept_df)
    })

    # DRAG updates
    observeEvent(input$map_marker_dragend, {
      req(working_points())
      drag <- input$map_marker_dragend
      cur  <- working_points()
      idx  <- which(cur$id == drag$id)
      cur[idx, c("latitude", "longitude")] <- c(round(drag$lat, 5), round(drag$lng, 5))
      working_points(cur)
    })

    # --- RETURN ONLY ON SAVE ---
    saved_points <- eventReactive(input$save, {
      req(working_points())
      working_points()
    }, ignoreInit = TRUE)

    return(saved_points)
  })
}
