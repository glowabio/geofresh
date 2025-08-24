


queryVar <- function(points, vars) {
  # Validate inputs
  stopifnot(is.data.frame(points))
  stopifnot(is.character(vars))

  # Check that all vars exist in points
  missing_vars <- setdiff(vars, names(points))
  if (length(missing_vars) > 0) {
    stop("The following variables are not in the data frame: ",
         paste(missing_vars, collapse = ", "))
  }

  # Return a data frame with variables
  #return(points[vars])
}
