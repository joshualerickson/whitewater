
#' Options
#'
#' @param date_range A \code{character}. Indicating how to call the API. 'pfn' = Period from now,
#' 'date_range' = a date range, "recent" = the most recent value.
#' @param period A \code{numeric}. Return all values from a period from now (only if 'pfn' is used).
#' @param dates A \code{vector}. Return all values within an absolute date range (start and end dates).
#' Only if 'date_range' is used.
#' @param site_status A \code{character} indicating site status. Example, 'all' = both active and inactive,
#' 'active' = only active sites, 'inactive' = only inactive sites.
#' @param floor_iv A \code{character} on how to floor the instantaneous values, '1 hour' (default).
#' @param ... other options used for options.
#'
#' @note A site is considered active if; it has collected time-series (automated) data within the last 183 days (6 months) or
#' it has collected discrete (manually collected) data within 397 days (13 months).
#'
#' @return A list with API options.
#' @export
#' @examples \dontrun{
#'
#' library(whitewater)
#' yaak_river_dv <- ww_dvUSGS('12304500',
#' parameter_cd = '00060',
#' wy_month = 10)
#'
#' yaak_river_iv <- ww_floorIVUSGS(yaak_river_dv)
#'
#' #change floor method
#'
#' yaak_river_iv <- ww_floorIVUSGS(yaak_river_dv,
#'                                 options = wwOptions(floor_iv = '6-hour'))
#'
#' #change number of days
#'
#' yaak_river_iv <- ww_floorIVUSGS(yaak_river_dv,
#'                                 options = wwOptions(floor_iv = '2-hour',
#'                                                     period = 365))
#'
#' # get by date range
#'
#'
#' yaak_river_wy <- ww_floorIVUSGS(yaak_river_dv,
#'                                 options = wwOptions(date_range = 'date_range',
#'                                                     dates = c('2022-03-01', '2022-05-11')))
#'
#'
#' # site status as 'active'
#'
#' yaak_river_wy <- ww_floorIVUSGS(yaak_river_dv,
#'                                 options = wwOptions(site_status = 'active',
#'                                                     date_range = 'date_range',
#'                                                     dates = c('2022-03-01', '2022-05-11')))
#' }

wwOptions <- function(date_range = 'pfn',
                      period = 11,
                      dates = NULL,
                      site_status = 'all',
                      floor_iv = '1 hour',
                      ...){

  wwfilterNULL(
    list(date_range = date_range,
         period = period,
         dates = dates,
         site_status = site_status,
         floor_iv = floor_iv,
         ...)
  )
}
