#' This is a function that creates a data.frame from a csv file (can be zipped .bz2) specified using \code{filename} argument.
#'
#' @param filename A character string giving the location of the csv file
#'
#' @return A data.frame object if the csv file exists. Otherwise the function stops with a "file does not exist" error.
#'
#' @examples
#' \dontrun{
#' fars_read("data/accident_2013.csv")
#' fars_read(filename = "data/accident_2013.csv")
#' }
#'
#' @importFrom readr read_csv
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

#' This is a function that generates datafile names for each year, which can be specified with \code{year} argument. This function is used together with \code{\link{fars_read}} function.
#'
#' @param year A character string that can be converted to an integer or an integer.
#'
#' @return A character string that is the file name for the data file for a specific year. The function will halt if the argument is not an integer or a character string that can be converted sucessfully to an integer.
#'
#' @example
#' \dontrun{
#' make_filename(2013)
#' make_filename(year=2013)
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("%s/accident_%d.csv.bz2", system.file("extdata", package="fars"), year)
}

#' This is a function that reads a lists of data input files with an argument specifying the \code{years} to read. It uses \code{\link{make_filename(year)}} function internally.
#'
#' @param years A vector of years in integer format to read. It might stop if the argument is not in integer format.
#'
#' @return A list of data.frames (tibble), in which each element contains data for a year.
#'
#' @example
#' \dontrun{
#' years = seq(2013, 2015)
#' fars_read_years(years)
#' fars_read_years(years=years)
#' }
#'
#' @importFrom dplyr mutate select
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

#' This is a function that generates a monthly sum for each year for the data. It uses \code{\link{fars_read_years(years)}} function internally.
#'
#' @param years A vector of years in integer format to read. It might stop if the argument is not in integer format.
#'
#' @return A data.frame(tibble) with monthly total for each year.
#'
#' @example
#' \dontrun{
#' years = seq(2013, 2015)
#' fars_summarize_years(years)
#' fars_summarize_years(years=years)
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' This is a function that generates a state map showing the locations of all accidents for a specific year. It uses \code{\link{make_filename(year)}}, \code{\link{fars_read(filename)}}.
#'
#' @param state.num The state number to generate the map for. It has to be an integer or a character string that can be converted into an integer, otherwise an error will be thrown and the function will halt. It must be present in the data files too, otherwise the function will stop and print an error message.
#' @param year An integer or character string that can be converted into an integer.
#'
#' @return A state map showing the locations for all accidents for a specific state.
#'
#' @example
#' \dontrun{
#' year = 2015
#' fars_map_state(1, 2015)
#' fars_map_state(state.num = 1, year = 2015)
#' fars_map_state(state.num = "1", year = "2015")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
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
