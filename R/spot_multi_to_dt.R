#' SPOT data to data.table
#'
#' @description
#' author: Ned Ryan-Schofield
#' date: 27/02/2026
#' Helper function for converting SPOT GPS files downloaded as .xlsx into data.table's. Output is a list of data.table objects.
#' Excel dates are stored as serial dates, from the origin 1899-12-30 in whatever timezone the sheet was downloaded in. I.e. for the SPOT sheets, this is +9:30. You just have to be careful in future to make sure you know what timezone the data is actually in when it is stored in excel, because that information is not actually stored. Super annoying for working across timezones.
#'
#' @param paths vector of file paths to raw downloaded .xlsx files. Create with something like: files <- list.files(path = "./data/table/tiwi_cat_gps_raw_spot", full.names = FALSE, recursive = FALSE, pattern = NULL); paths <- paste0("./data/table/tiwi_cat_gps_raw_spot/", files)
#' @param names optional character vector of ID names to replace names in SPOT files with. Otherwise the list elements get the name of the sheet. Has to be in order.
#' @param tz local timezone to display POSIXct timestamp in. default is ACST
#' @param utc.offset numeric variable - difference between UTC and local time in hours. Default is for darwin time.
#' @param skip number of lines from top of sheet to skip. Default is 5
#' @returns a list of data.table objects, where each element of the list is a set of GPS data.
#' @export
spot_multi_to_dt <- function(paths,
                             names = NULL,
                             tz = "Australia/Darwin",
                             utc.offset = -9.5,
                             skip = 5) {

  ### checks ###
  if (any(!file.exists(paths))) {
    bad <- paths[!file.exists(paths)]
    stop("These paths do not exist:\n", paste(bad, collapse = "\n"))
  }


  # check
  sheets <- lapply(paths, readxl::excel_sheets)
  sheet_index_list <- lapply(sheets, \(x) which(x != "Summary"))

  # vectors for readxl
  expanded_paths  <- rep(paths, lengths(sheet_index_list))
  expanded_sheets <- unlist(sheet_index_list)

  # intialise list
  animal.names <- sheets |> unlist() |> unique() |>
    (\(x) sub(" Positions & Events", "", x[x != "Summary"]))()
  gps.data <- vector("list", length(expanded_paths))
  names(gps.data) <- animal.names

  for(i in seq_along(gps.data)) {

    int <- readxl::read_xlsx(path = expanded_paths[[i]],
                             sheet = expanded_sheets[[i]],
                             skip = skip,
                             col_types = "text")

    data.table::setDT(int)

    ### check required columns exist ###
    required <- c("Lat/Lng", "Date")
    missing_cols <- setdiff(required, names(int))
    if (length(missing_cols) > 0) {
      stop(sprintf(
        "File '%s' is missing required column(s): %s",
        expanded_paths[[i]], paste(missing_cols, collapse = ", ")
      ))
    }

    ### split Lat/Lng ###
    int[, c("lat", "long") := data.table::tstrsplit(`Lat/Lng`, "\\s*,\\s*", fixed = FALSE)]     # split on comma, allowing arbitrary spaces

    ### numeric conversion ###
    int[, lat  := as.numeric(trimws(lat))]  #trimws trims whitespace if any
    int[, long := as.numeric(trimws(long))]

    ### Date to POSIXct ###
    int[, Date := as.numeric(Date)]
    int[, timestamp := as.POSIXct(Date * (60*60*24) + (utc.offset*60*60), origin = "1899-12-30", tz = tz)]

    out <- int[, c("timestamp", "lat", "long")]
    out[, ID := animal.names[[i]]]

    #optional rename of ID column
    if (!is.null(names)) {
      if (length(names) != length(expanded_paths)) {
        stop("`paths` and `animal.names` must be the same length.")
      }
      out[, ID := names[[i]]]
    }

    ### add to list ###
    gps.data[[i]] <- out

  }

  #optional rename of list ID
  if (!is.null(names)) {
    names(gps.data) <- names
  }

  #sort list alphabetically, to align with ctmm
  gps.data <- gps.data[order(names(gps.data))]

  return(gps.data)

}


