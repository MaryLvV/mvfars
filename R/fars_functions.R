#'Read the Fatality Analysis Reporting System data into R
#'
#'This function reads the csv file into the local environment and returns
#'a data frame.
#'
#'@param filename A character string that gives the name of the file
#'to be read.
#'
#'@note A warning is given if the file does not exist.
#'
#'@return This function returns a data frame.
#'
#'@examples fars_read(system.file("extdata", "accident_2013.csv.bz2", package = "mvfars"))
#'
#'@importFrom readr read_csv
#'@importFrom dplyr tbl_df
#'
#'@export

fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file ", filename, " does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

#'Construct a file name from a given year
#'
#'This function takes a 4-digit year as input and returns a filename
#'in the form 'accident_YEAR.csv.bz2'.
#'
#'@param year A 4-digit year (integer or character string)
#'
#'@return This function returns a character string with the file name.
#'
#'@examples make_filename(2013)
#'
#'@export

make_filename <- function(year) {
    year <- as.integer(year)
    #sprintf("accident_%d.csv.bz2", year)
    paste0(system.file("extdata", sprintf("accident_%d.csv.bz2", year), package = "mvfars"))
}

#'Read Data For a List of Years
#'
#'This function takes a vector of years, finds the corresponding data file for each year,
#'reads it, and returns a data frame for each year with 'MONTH' and 'year' columns.
#'
#'@details The function first calls make_filename() to create a filename
#'for a year in the list, and then passes that filename to fars_read()
#'to read data from the file into a data frame. The year is added
#'as a column in the data frame. Finally, the MONTH and year are selected.
#'
#'@note Will give a warning for any year in the list that does not have a matching
#'data file.
#'
#'@param years A vector of the years for which we want to get data.
#'
#'@examples fars_read_years(c('2013', '2014', '2015'))
#'
#'@importFrom dplyr mutate select
#'@import magrittr
#'
#'@export


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

#'Create Summary Data for Specified Years
#'
#'This function takes a list of years, passes them to the
#'fars_read_years() function, and combines the resulting data frames
#'into a single summary data frame with 12 rows (one for each month)
#'and column for each year. Totals for each month are shown under their
#'respective year columns.
#'
#'@param years A vector of the years for which we want to get data.
#'
#'@return This function returns a summary data frame.
#'
#'@examples fars_summarize_years(c('2013', '2014', '2015'))
#'
#'@importFrom dplyr bind_rows group_by summarize
#'@importFrom tidyr spread
#'@import magrittr
#'
#'@export

fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}
#' #'
#' #'Plot the Fatality Analysis Reporting System data for
#' #'a particular state and year on a map of that state
#' #'
#' #'This function creates a map of a given state and plots accident data
#' #'on the map by location
#' #'
#' #'@param state.num a numeric value assigned to a state
#' #'@param year a 4-digit year (integer or character string)
#' #'
#' #'@note throws an error if an invalid state number is given
#' #'@note retruns a message if accidents for thae state and year given is 0
#' #'
#' #'@examples fars_map_state(47, 2013)  #fatal accidents in TN for 2013
#' #'
#' #'@importFrom dplyr filter
#' #'@importFrom maps map
#' #'@importFrom graphics points
#' #'
#' #'@export
#'
#' fars_map_state <- function(state.num, year) {
#'     filename <- make_filename(year)
#'     data <- fars_read(filename)
#'     state.num <- as.integer(state.num)
#'
#'     if(!(state.num %in% unique(data$STATE)))
#'         stop("invalid STATE number: ", state.num)
#'     data.sub <- dplyr::filter(data, STATE == state.num)
#'     if(nrow(data.sub) == 0L) {
#'         message("no accidents to plot")
#'         return(invisible(NULL))
#'     }
#'     is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
#'     is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
#'     with(data.sub, {
#'         maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
#'                   xlim = range(LONGITUD, na.rm = TRUE))
#'         graphics::points(LONGITUD, LATITUDE, pch = 46)
#'     })
#' }
