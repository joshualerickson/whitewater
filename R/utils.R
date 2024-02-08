


#' Title
#'
#' @return
#' @export
#'
#'
#' @examples
#'
add_stuff <- function() {


}


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

#' Add Proportion
#' @description Adds proportion of observation per water year based on the maximum.
#'
#' @param data A daily value df
#'
#' @return a proportion column within df per water year.
#' @noRd
add_proportion <- function(data) {

    cols <- cols_to_update(data)

    dplyr::group_by(data, site_no, wy) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(cols),
                                list(max_prop = ~.x/max(.x, na.rm = TRUE)
                                ))) %>%
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
#' Get param colnames
#'
#' @param data data.frame
#'
#' @return A vector with parameter names in the data.frame
#' @noRd
#'
cols_to_update <- function(data){

  param_names <- c("Wtemp","Precip","Flow","GH","SpecCond",
                   "DO", "pH", "GWL","Turb","WLBLS", "Chloride")

  names(data[which(names(data) %in% param_names)])
}

#' Get paramCd names
#'
#' @param x vector of parameter_cd
#' @return A vector of length 1 or n, matching the length of the logical input or output vectors.
#' @noRd
#'
name_params_to_update <- function(x){


  param_names <- dplyr::case_when(
    x == "00010" ~ "Wtemp",
    x == "00045" ~ "Precip",
    x == "00060" ~ "Flow",
    x == "00065" ~ "GH",
    x == "00095" ~ "SpecCond",
    x == "00300" ~ "DO",
    x == "00400" ~ "pH",
    x == "62611" ~ "GWL",
    x == "63680" ~ "Turb",
    x == "72019" ~ "WLBLS",
    x == "70290" ~ "Chloride",
    x == "param_00010" ~ "Wtemp",
    x == "param_00045" ~ "Precip",
    x == "param_00060" ~ "Flow",
    x == "param_00065" ~ "GH",
    x == "param_00095" ~ "SpecCond",
    x == "param_00300" ~ "DO",
    x == "param_00400" ~ "pH",
    x == "param_62611" ~ "GWL",
    x == "param_63680" ~ "Turb",
    x == "param_72019" ~ "WLBLS",
    x == "param_70290" ~ "Chloride",
    x == "Wtemp_param_00010" ~ "Wtemp",
    x == "Precip_param_00045" ~ "Precip",
    x == "Flow_param_00060" ~ "Flow",
    x == "GH_param_00065" ~ "GH",
    x == "SpecCond_param_00095" ~ "SpecCond",
    x == "DO_param_00300" ~ "DO",
    x == "pH_param_00400" ~ "pH",
    x == "GWL_param_62611" ~ "GWL",
    x == "Turb_param_63680" ~ "Turb",
    x == "WLBLS_param_72019" ~ "WLBLS",
    x == "Chloride_param_70290" ~ "Chloride")

}

#' @title Filter Null List
#' @description Taken from leaflet filterNULL function
#' remove NULL elements from a list
#' @param x A list whose NULL elements will be filtered
#' @return A list that has NULL elements removed
#' @noRd
wwfilterNULL <- function(x) {
  if (length(x) == 0 || !is.list(x)) return(x)
  x[!unlist(lapply(x, is.null))]
}



#' @title Get error codes
#' @description Used for instantneous API call where sometimes an error occurrs. We just
#' want to capture the description.
#' @param x A character
#' @return A vector of length 1 or n, matching the length of the logical input or output vectors.
#' @noRd
iv_error_codes <- function(x){

  dplyr::case_when(x == "Bkw" ~	'Value is affected by backwater at the measurement site.',
                   x == 'Dis' ~	'Record has been discontinued at the measurement site.',
                   x == 'Dry' ~	'Dry condition exists at the measurement site.',
                   x == 'Eqp' ~	'Value affected by equipment malfunction.',
                   x == 'Fld' ~	'Value is affected by flooding conditions at the measurement site.',
                   x == 'Ice' ~	'Value is affected by ice at the measurement site.',
                   x == 'Mnt' ~	'Site under going maintenance.',
                   x == 'Pr' ~	'Parameter only partially monitored over specific range of values or time periods.',
                   x == 'Pmp' ~	'Value is affected by pumping at time of measurement.',
                   x == 'Rat' ~	'Rating being developed.',
                   x == 'Ssn' ~	'Parameter monitored seasonally.',
                   x == 'Tst' ~	'Value is affected by artificial test condition.',
                   x == 'Zfl' ~	'Zero flow condition present at the measurement site.',
                   x == '***' ~	'Value unavailable.',
                   TRUE ~ NA_character_)

}



#' Prepping for loggin
#'
#' @param data data.frame
#'
#' @return values are padded if zero for parameter of interest
#' @noRd
#'
pad_zero_for_logging <- function(data){

  param_names <- c("Wtemp","Precip","Flow","GH","SpecCond",
                   "DO", "pH", "GWL","Turb","WLBLS")

  cols_to_update <- names(data[which(names(data) %in% param_names)])

  data %>%
    mutate(across(dplyr::all_of(cols_to_update), ~ifelse(.x <= 0 , .x + 0.000001, .x)))
}


#' Delay
#'
#' @return a number for amount of time to delay
#'
delay_setup <- function(){

  ## max 120 requests per minute
  max_freq_per_min <- 120

  ## delay in seconds between requests
  delay <- 60/max_freq_per_min * future::nbrOfWorkers()

}
