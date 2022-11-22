#' Read data
#'
#' This function reads data. The document to read has data from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System (a csv file), which is a nationwide census providing the American
#' public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes.
#' The input of the function is the filename to import and return a data frame .
#'
#' @param filename Is a string what represents the file's name, class character.
#'
#' @return This function return a data frame of the data that was imported. If you write
#' wrong the file's name (or the file does not exist) then function return the message "file __ does not exist".
#'
#' @examples
#' \dontrun{fars_read(filename="accident_2014.csv.bz2")}
#'
#' @importFrom readr read_csv
#'
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create filename
#'
#' This function has as an input a integer called year and create the filename input that you need in the
#' fards_read function.
#'
#' @param  year it can be one number (one year) or a vector of numbers (years), class numeric.
#'
#' @return it can be one string or list according to the inputs. The output of this function you use
#' as the input called filename in the function fards_read, class character.
#'
#' @examples
#' \dontrun{make_filename(year=c(2014,2015))}
#' \dontrun{make_filename(year=2013)}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv", year)
}

#' This function use the function fars_read one or more times according to the amount of inputs.
#' Moreover, the function create a column with mutate called year and select variables MONTH and year.
#'
#' @param years it can be one number (one year) or a vector of numbers (years), class numeric.
#'
#' @return The output of this function is a single data frame with columns MONTH and year or a list
#' with the same columns (in case that input it is a list), class data frame. However, if a year is invalid then function
#' return a warning message "invalid year: _".
#'
#' @examples
#' \dontrun{fars_read_years(c(2013,2014))}
#' \dontrun{fars_read_years(2015)}
#'
#' @importFrom dplyr mutate select
#'
#' @importFrom magrittr %>%
#'
#' @importFrom rlang .data
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' This function summarize years, binding separate dataset for each year in one data frame with bind_rows.
#' After, with the spread function change rows into column, where years are in columns and months are in rows.
#'
#' @param years As before it can be one number (one year) or a vector of numbers (years), class numeric.
#'
#' @return The ouput of this function is a data frame where columns are years and rows are months, it is
#' important to considerar that the information of the data frame represent the number of accidents for each month and year.
#'
#' @examples
#' \dontrun{fars_read_years(c(2013,2014))}
#' \dontrun{fars_read_years(2015)}
#'
#' @importFrom dplyr bind_rows group_by summarize n
#'
#' @importFrom rlang .data
#'
#' @importFrom tidyr spread
#'
#' @importFrom magrittr %>%
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' This function generate data visualization. For a given state, the function show points that represents
#' accidents in the state during the year given as a input.
#'
#' @param state.num it is the code of the state, class numeric.
#'
#' @param year As before it can be one number (one year), class numeric.
#'
#' @return The ouput of this function is plot a map with points that represents accidents. However, if you write
#' wrong the state code then the function stop and show the message "invalid STATE number: ___".
#'
#' @examples
#' \dontrun{fars_map_state(state.num=6, year=2013)}
#' \dontrun{fars_map_state(state.num=29, year=2014)}
#'
#' @importFrom maps map
#'
#' @importFrom dplyr filter
#'
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

