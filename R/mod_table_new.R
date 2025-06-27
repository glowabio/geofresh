# This module creates a DT table
library(DT)

tableUI <- function(id, label = "tabledata") {
  ns <- NS(id)
  tagList(
    # table output
    DTOutput(ns("table"))
  )
}

# Table Module server function
tableServer <- function(id, table_data, column_defs = NULL, searching = TRUE) {
  moduleServer(
    id,
    function(input, output, session) {
      # render table with first column fixed
      output$table <- renderDT({
        req(table_data())  # Ensure table_data exists before trying to render
        datatable(
          table_data(),
          #colnames = column_names,
          rownames = FALSE,
          extensions = "FixedColumns",
          options = list(
            deferRender = TRUE,
            scrollX = TRUE,
            scrollY = "150px",
            fixedColumns = list(leftColumns = 1),
            columnDefs = column_defs,
            searching = searching
          )
        )
      })
    }
  )
}
