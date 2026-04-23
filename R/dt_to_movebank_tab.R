#' data.table to movebank textfile
#'
#' @description
#' author: Ned Ryan-Schofield
#' date: 23/04/2026
#' Helper function for coercing a list of data.table objects of gps data that have been checked for errors and coerced back to data.table to a single .txt file for upload to movebank.
#'
#' @param dt a list of data.tables that are gps data, with same column names
#' @param outpath character variable of file path name. i.e. "./outputs/table/tiwi_cats_movebank_up_03032026.txt"
#' @param timestamp POSIXct variable - data.table timestamp column
#' @param x numerical variable - data.table coordinate longitude column
#' @param y numerical variable - data.table coordinate latitude column
#' @param ID character variable - data.table animal ID column
#' @returns writes a textfile to disc.

DT_to_movebank_tab <- function(dt, out.path,
                               t = "timestamp",
                               y = "lat",
                               x = "long",
                               ID = "ID") {

  ### checks ###
  if (!inherits(dt, "list")) {
    stop("`dt` must be a list of data.table objects")
  }

  #bind lists together
  dt.long <- data.table::rbindlist(dt, use.names = TRUE, fill = TRUE)

  #check no missing columns
  req <- c(t, y, x, ID)
  miss <- setdiff(req, names(dt.long))
  if (length(miss)) {
    stop("Missing required columns: ", paste(miss, collapse = ", "))
  }

  out <- dt.long[, .SD, .SDcols = req]

  #rename to movebank naming conventions
  data.table::setnames(
    out,
    old = c(t, y, x, ID),
    new = c("timestamp", "location.lat", "location.long", "individual.local.identifier")
  )

  #write to disc
  utils::write.table(
    x = dt.long,
    file = out.path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  invisible(out)

}
