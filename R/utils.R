
#'Water Year These functions are hi-jacked from smwrBase package.
#'
#'Create an ordered factor or numeric values from a vector of dates based on
#'the water year.
#' @noRd
#' @param x an object of class "Date" or "POSIXt." Missing values are permitted and
#'result in corresponding missing values in the output.
#' @param wy_month A numeric indicating the month the water year begins.
#' @param numeric a logical value that indicates whether the returned values
#'should be numeric \code{TRUE} or an ordered factor \code{FALSE}. The default
#'value is \code{FALSE}.
#' @return An ordered factor or numeric vector corresponding to the water year.
#' @note The water year is defined as the period from October 1 to September 30.
#'The water year is designated by the calendar year in which it ends. Thus, the
#'year ending September 30, 1999, is the "1999 water year."
#' @seealso
#Flip for production/manual
#'\code{\link[lubridate]{year}}
#\code{year} (in lubridate package)

waterYear <- function(x, wy_month = 10, numeric=FALSE) {
  ## Coding history:
  ##    2005Jul14 DLLorenz Initial dated verion
  ##    2010Feb17 DLLorenz Added option to return numerics
  ##    2011Jun07 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  ## adjust for water year
  yr <- yr + ifelse(mn < as.integer(wy_month), 0L, 1L)
  if(numeric)
    return(yr)
  ordered(yr)
}

#' Convert dt to tibble
#'
#' @param data a data.frame and data.table
#' @noRd
#' @return a tibble df
dt_to_tibble <- function(data) {

  class(data) <- 'data.frame'
  data <- dplyr::tibble(data)

}

#' all na
#' @description Remove columns with all NA's
#' @param x a data.frame
#' @noRd
all_na <- function(x){
  any(!is.na(x))
}


#' Add Counts
#' @description Adds counts of observation per water and month
#'
#' @param data A daily value df
#'
#' @return counts within df
#' @noRd
add_date_counts <- function(data) {

  dplyr::group_by(data, site_no, wy) %>%
    dplyr::add_count(name = 'obs_per_wy') %>%
    dplyr::ungroup() %>%
    dplyr::group_by(site_no, wy, month) %>%
    dplyr::add_count(name = 'obs_per_month') %>%
    dplyr::ungroup()
}

#' water year to months
#' @description Change wy_month to doy.
#' @param wy_month A numeric
#' @param leap Logical
#' @return A numeric value
#' @noRd
month_to_doy <- function(wy_month, leap = FALSE) {


  ifelse(isTRUE(leap),
  dplyr::case_when(wy_month == 1 ~ 1,
                   wy_month == 2 ~ 32,
                   wy_month == 3 ~ 61,
                   wy_month == 4 ~ 92,
                   wy_month == 5 ~ 122,
                   wy_month == 6 ~ 153,
                   wy_month == 7 ~ 183,
                   wy_month == 8 ~ 214,
                   wy_month == 9 ~ 245,
                   wy_month == 10 ~ 275,
                   wy_month == 11 ~ 306,
                   wy_month == 12 ~ 336,
                   TRUE ~ NA_real_)
  ,
  dplyr::case_when(wy_month == 1 ~ 1,
                   wy_month == 2 ~ 32,
                   wy_month == 3 ~ 60,
                   wy_month == 4 ~ 91,
                   wy_month == 5 ~ 122,
                   wy_month == 6 ~ 152,
                   wy_month == 7 ~ 182,
                   wy_month == 8 ~ 213,
                   wy_month == 9 ~ 244,
                   wy_month == 10 ~ 274,
                   wy_month == 11 ~ 305,
                   wy_month == 12 ~ 335,
                   TRUE ~ NA_real_)
  )

}
