# This module creates a DT table
library(DT)

tableUI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("tabledata"), height = "700px")
  )
}

# Table Module server function
tableServer <- function(id, table_data, column_defs = NULL, searching = TRUE) {
  moduleServer(
    id,
    function(input, output, session) {
      # render table with first column fixed
      output$tabledata <- renderDT({
        req(table_data())  # Ensure table_data exists before trying to render
        datatable(
          table_data(),
          #colnames = column_names,
          rownames = FALSE,
          extensions = "FixedColumns",
          options = list(
            deferRender = TRUE,
            scrollX = TRUE,
            scrollY = "700px",
            fixedColumns = list(leftColumns = 1),
            columnDefs = column_defs,
            searching = searching
          )
        )
      })
    }
  )
}

