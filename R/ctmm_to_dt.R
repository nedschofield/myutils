#' ctmm telemetry to data.table
#'
#' @description
#' author: Ned Ryan-Schofield
#' date: 03/03/2026
#' Helper function for coercing a ctmm telemetry object back to a list of data.tables, after outlier removal.
#'
#' @param telemetry a ctmm telemetry object
#' @param crs projection to use for as.telemetry. e.g. "EPSG:7852" for UTM 52 - tiwis
#' @param x numerical variable - data.table coordinate longitude column
#' @param y numerical variable - data.table coordinate latitude column
#' @param ID character variable - data.table animal ID column
#' @returns a list of data.table objects

ctmm_to_dt <- function(telemetry) {

  ### checks ###
  if (!inherits(telemetry, "list")) {
    stop("`", telemetry, "` is not a list of telemetry objects")
  }

  gps_list <- vector("list", length(telemetry))
  names(gps_list) <- names(telemetry)

  for (i in seq_along(telemetry)) {

  gps_list[[i]] <- data.table::data.table(
    timestamp = telemetry[[i]]$timestamp,
    lat = telemetry[[i]]$latitude,
    long = telemetry[[i]]$longitude,
    ID = telemetry[[i]]@info$identity
    )

  }

  return(gps_list)

}
