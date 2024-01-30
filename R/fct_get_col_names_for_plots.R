# function for filtering and splitting column names
# of environmental variables query results
# in module plot_results

get_col_names_for_plots <- function(dataset) {
  # get column names as list
  # remove first two elements (id, subc_id)
  dataset_cols <- dataset[[1]] %>%
    names() %>%
    as.list() %>%
    .[-c(1, 2)]

  # create empty list for filtered column names
  dataset_cols_filtered <- list()

  # remove "_mean" if present, else take value for single value topography columns
  for (x in dataset_cols) {
    if (x %in% topo_without_stats) {
      dataset_cols_filtered <- append(dataset_cols_filtered, x)
      names(dataset_cols_filtered)[length(dataset_cols_filtered)] <- x
    } else {
      if (grepl("_mean", x)) {
        dataset_cols_filtered <- append(dataset_cols_filtered, x)
        names(dataset_cols_filtered)[length(dataset_cols_filtered)] <- strsplit(x, "_mean")[[1]][1]
      }
    }
  }
  return(dataset_cols_filtered)
}
