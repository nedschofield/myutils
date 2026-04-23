#' check data.table has no empty points
#'
#' @description
#' author: Ned Ryan-Schofield
#' date: 27/02/2026
#' Helper function for checking whether a list of data.table objects (that are gps data derived using spot_to_DT function) have any empty lat/long positions
#' example use: lapply(dt, dt_has_no_empty_points)

#'
#' @param dt a list of data.tables of GPS data
#' @param x_col name of the x coordinate vector, should be numeric. Default is "long"
#' @param y_col name of the y coordinate vector, should be numeric. Default is "lat"
#' @returns outputs a TRUE/FALSE value in the console, for every element of dt. if FALSE, gives the index for the first NA
#' @export
dt_has_no_empty_points <- function(dt, x_col = "long", y_col = "lat") {

  x <- dt[[x_col]]
  y <- dt[[y_col]]

  x.NA <- which(is.na(x))
  y.NA <- which(is.na(y))

  if (length(x.NA) == 0 && length(y.NA) == 0) {
    return(list(no_empty_points = TRUE,
                first_NA_index = NA))
  }

  if (length(x.NA) > 0 | length(y.NA) > 0) {
    i <- min(c(x.NA[1], y.NA[1]))
    return(list(no_empty_points = FALSE,
           first_NA_index = i)
           )
  }
}
