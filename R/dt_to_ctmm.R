#' data.table to ctmm telemetry object
#'
#'@description
#' author: Ned Ryan-Schofield
#' date: 28/02/2026
#' Helper function for coercing a list of data.table objects (that are gps data derived using spot_to_DT function) to ctmm telemetry objects, for easy removal of outliers
#'
#' @param dt a list of data.tables of GPS data
#' @param crs projection to use for as.telemetry. e.g. "EPSG:7852" for UTM 52 - tiwis
#' @param x numerical variable - data.table coordinate longitude column
#' @param y numerical variable - data.table coordinate latitude column
#' @param ID character variable - data.table animal ID column
#' @returns a ctmm telemetry object

dt_to_ctmm <- function(dt, crs, x = "long", y = "lat", ID = "ID") {

  ### checks ###
  if (!inherits(dt, "list")) {
    stop("`", dt, "` is not a list of data.tables")
  }

  dt_long <- do.call(rbind, dt)
  names(dt_long) <- c("timestamp", "location.lat", "location.long", "individual.local.identifier") #rename to movebank naming conventions

  ctmm::as.telemetry(dt_long, projection = crs)

}
