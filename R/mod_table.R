# This module creates a DT table for csv upload data or query results
library(DT)

tableOutput <- function(id, label = "maprecords") {
  ns <- NS(id)
  tagList(
    # table output
    DTOutput(ns("table"))
  )
}

# Table Module server function
# The parameter "table_data" is a non-reactive data frame
# The parameter "col_names" is a vector with column names
tableServer <- function(id, table_data, column_names, column_defs = NULL, searching = TRUE) {
  moduleServer(
    id,
    function(input, output, session) {
      # render table with first column fixed
      output$table <- renderDT({
        datatable(
          table_data,
          colnames = column_names,
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
