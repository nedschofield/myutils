#' check data.table is time ordered without duplicates
#'
#' @description
#' author: Ned Ryan-Schofield
#' date: 27/02/2026
#' Helper function for checking whether a list of data.table objects (that are gps data derived using spot_to_DT function) are time ordered, without duplicate timestamps.
#' example use: lapply(dt, dt_is_time_ordered)
#'
#' @param dt a list of data.tables of GPS data
#' @param time_col name of the timestamp vector, of class POSIXct
#' @param na_ok an NA check. If true, NA's accepted.
#' @returns a list of TRUE or FALSE values

dt_is_time_ordered_no_duplicates <- function(dt, time_col = "timestamp", na_ok = FALSE) {

  x <- dt[[time_col]]

  if (!inherits(x, "POSIXt")) {
    stop("`", time_col, "` is not POSIXct/POSIXlt in this table.")
  }

  if (!na_ok && anyNA(x)) {
    return(list(ordered = FALSE, reason = "NA timestamps present"))
  }

  if (na_ok) x <- x[!is.na(x)]

  d <- diff(x)
  not_ordered <- which(d < 0)
  duplicate <- which(d == 0)

  if (length(not_ordered) == 0 & length(duplicate) == 0) {
    return(list(ordered = TRUE, first_order_break_i = NA_integer_, prev_time = NA, next_time = NA,
                no_duplicate = TRUE, first_dup_index = NA))
  }

  if (length(not_ordered) > 0 & length(duplicate) == 0) {
  i <- not_ordered[1]
  list(
    ordered = FALSE,
    first_order_break_i = i + 1,      # index of the "backwards" row
    prev_time = x[i],
    next_time = x[i + 1],
    no_duplicate = TRUE,
    first_dup_index = NA
  )
  }

  if (length(not_ordered) > 0 & length(duplicate) > 0) {
    i <- not_ordered[1]
    list(
      ordered = FALSE,
      first_order_break_i = i + 1,      # index of the "backwards" row
      prev_time = x[i],
      next_time = x[i + 1],
      no_duplicate = FALSE,
      first_dup_index = duplicate[1]
    )
  }

  if (length(not_ordered) == 0 & length(duplicate) > 0) {
    list(
      ordered = TRUE,
      first_order_break_i = NA_integer_,      # index of the "backwards" row
      prev_time = NA,
      next_time = NA,
      no_duplicate = FALSE,
      first_dup_index = duplicate[1]
    )
  }
}
