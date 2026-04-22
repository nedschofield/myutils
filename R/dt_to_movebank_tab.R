#' data.table to movebank textfile
#'
#' @description
#' author: Ned Ryan-Schofield
#' date: 03/03/2026
#' Helper function for coercing a list of data.table objects of gps data that have been checked for errors and coerced back to data.table to a single .txt file for upload to movebank.
#'
#' @param dt a list of data.tables that are gps data
#' @param outpath character variable of file path name. i.e. "./outputs/table/tiwi_cats_movebank_up_03032026.txt"
#' @param timestamp POSIXct variable - data.table timestamp column
#' @param x numerical variable - data.table coordinate longitude column
#' @param y numerical variable - data.table coordinate latitude column
#' @param ID character variable - data.table animal ID column
#' @returns writes a textfile to disc.

DT_to_movebank_tab <- function(dt, out.path, t = "timestamp", x = "long", y = "lat", ID = "ID") {

  ### checks ###
  if (!inherits(dt, "list")) {
    stop("`", dt, "` is not a list of data.table objects")
  }

  dt.long <- do.call(rbind, dt)
  names(dt.long) <- c("timestamp", "location.lat", "location.long", "individual.local.identifier") #rename to movebank naming conventions

  write.table(
    x = dt.long,
    file = out.path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

}
