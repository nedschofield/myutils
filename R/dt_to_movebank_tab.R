#' data.table to movebank textfile
#'
#' @description
#' author: Ned Ryan-Schofield
#' date: 23/04/2026
#' Helper function for coercing a list of data.table objects of gps data that have been checked for errors and coerced back to data.table to a single .txt file for upload to movebank. Still a bit of a proccess to get this data into movebank though.
#' This function converts from the local timezone to UTC +0, which is easier to work with in movebank.
#'
#' @param dt a list of data.tables that are gps data, with same column names
#' @param outpath character variable of file path name. i.e. "./outputs/table/tiwi_cats_movebank_up_03032026.txt"
#' @param timestamp POSIXct variable - data.table timestamp column
#' @param x numerical variable - data.table coordinate longitude column
#' @param y numerical variable - data.table coordinate latitude column
#' @param ID character variable - data.table animal ID column
#' @param tz_in character vector of timezone that timestamp (which is a POSIXct) is in. Default is "Australia/Darwin"
#' @returns writes a textfile to disc. Timestamp is in UTC +0
#' @export
dt_to_movebank_tab <- function(dt, out.path,
                               t = "timestamp",
                               y = "lat",
                               x = "long",
                               ID = "ID",
                               tz_in = "Australia/Darwin",
                               round_time = TRUE) {

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

  # ensure timestamp is POSIXct
  if (!inherits(out$timestamp, "POSIXct")) {
    stop("`", t, "` must be a POSIXct column")
  }

  # if timezone is missing/empty, force it to local timezone
  current_tz <- attr(out$timestamp, "tzone")
  if (is.null(current_tz) || identical(current_tz, "") || all(is.na(current_tz))) {
    out[, timestamp := as.POSIXct(timestamp, tz = tz_in)]
  }

  # round to nearest second if requested
  if (isTRUE(round_time)) {
    out[, timestamp := as.POSIXct(
      round(as.numeric(timestamp)),
      origin = "1970-01-01",
      tz = "UTC"
    )]
  }

  # format as UTC text without fractional seconds
  out[, timestamp := format(timestamp, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")]

  #write to disc
  utils::write.table(
    x = out,
    file = out.path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  invisible(out)

}
