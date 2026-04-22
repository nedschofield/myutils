#' convert data.table to amt track.xyt
#'
#' @description
#' author: Ned Ryan-Schofield
#' date: 19/02/2026
#' Helper function for converting a list of data.table objects to amt_xyt data structure for step selection make sure that you specify the crs as the same as the covariate raster stack
#'
#' example usage:
#'
#' x <- lapply(gps, dt_to_amt_track, crs_out = tiwi.crs)
#'
#' x.cov <- amt::extract_covariates(x, fire)
#'
#' @param x list of data.table objects or single data.table
#' @param crs_out crs code. accept 3577, "EPSG:3577", proj string, or WKT
#' @param lon character denoting numeric longitude column
#' @param lat character denoting numeric latitude column
#' @param time character denoting posixct time column
#' @param id character denoting ID column
#' @param crs_in crs code. accept 3577, "EPSG:3577", proj string, or WKT
#'
#' @returns a list of track.xyt objects
#'
#' @examples
#' \dontrun{
#' x <- lapply(gps, dt_to_amt_track, crs_out = tiwi.crs)
#' x.cov <- amt::extract_covariates(x, fire)
#' }

dt_to_amt_track <- function(dt,
                            crs_out,
                            lon = "long",
                            lat = "lat",
                            time = "timestamp",
                            id  = "ID",
                            crs_in = 4326) {

  ### checks ###
  stopifnot(data.table::is.data.table(dt))

  # Basic column checks
  req <- c(lon, lat, time, id)
  miss <- setdiff(req, names(dt))
  if (length(miss)) stop("Missing columns in dt: ", paste(miss, collapse = ", "))

  # CRS parsing (accept 3577, "EPSG:3577", proj string, or WKT)
  out_crs <- sf::st_crs(crs_out)
  if (is.na(out_crs)) stop("`crs_out` couldn't be parsed. Use e.g. 3577 or 'EPSG:3577' or a proj string.")

  in_crs <- sf::st_crs(crs_in)
  if (is.na(in_crs)) stop("`crs_in` couldn't be parsed (default is EPSG:4326).")

  # Make sf -> transform -> coords
  sf_pts <- sf::st_as_sf(
    dt,
    coords = c(lon, lat),
    crs = in_crs,
    remove = FALSE
  )

  sf_pts <- sf::st_transform(sf_pts, out_crs)

  xy <- sf::st_coordinates(sf_pts)

  # Build the amt track
  out <- data.table::copy(dt)
  out[, `:=`(x = xy[, 1], y = xy[, 2], t = get(time), id = get(id))]

  trk <- amt::make_track(
    out,
    .x = x,
    .y = y,
    .t = t,
    id = id,
    crs = out_crs$wkt
  )

  trk

}
