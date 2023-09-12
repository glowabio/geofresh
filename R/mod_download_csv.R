# This module download a data frame as a CSV file

# Module UI function

downloadDataUI <- function(id, label = "Download") {
  ns <- NS(id)
  tagList(
    downloadButton(
      ns("downloadData"),
      label,
      icon = icon("download"),
      class = "btn-primary"
    )
  )
}

# Module server function
# For downloading a zipped file set 'zipped' to TRUE
# and pass 'dataset' as reactiveValues containing named lists
# with the file name and dataframe for each CSV,
# e.g. list(-env-var-topography-local = query_result_topo())
downloadDataServer <- function(id, dataset, zipped = FALSE, file_name = "") {
  moduleServer(
    id,
    function(input, output, session) {
      if (zipped) {
        # check if dataset is reactiveValues
        stopifnot(is.reactivevalues(dataset))
        # return zipped file
        output$downloadData <- downloadHandler(
          filename = function() {
            paste0("geofresh-", Sys.Date(), file_name, ".zip") # tempdir(), "\\geofresh-", Sys.Date(), file_name, ".zip")
          },
          content = function(file) {
            # create a temporary CSV file for each of the dataframes in the named list
            csv_list <- sapply(
              reactiveValuesToList(dataset),
              function(df) {
                path_to_csv <- paste0(tempdir(), "/geofresh-", Sys.Date(), names(df), ".csv")
                # write data frame to CSV
                write.csv(unname(df), file = path_to_csv, row.names = FALSE)
                # return path to temporary CSV
                path_to_csv
              }
            )
            # create zip file from list of temporary CSVs
            zip::zip(file, files = csv_list) #  mode = "cherry-pick"
          }
        )
      } else {
        # return single CSV file
        output$downloadData <- downloadHandler(
          filename = function() {
            paste0("geofresh-", Sys.Date(), file_name, ".csv")
          },
          content = function(file) {
            write.csv(dataset, file, row.names = FALSE)
          }
        )
      }
    }
  )
}
