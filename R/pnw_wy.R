
#' A subset of USGS stations in HUC 17
#'
#' @format A data frame with 18934 rows and 30 variables:
#' \describe{
#'   \item{Station}{name of USGS station}
#'   \item{site_no}{station site id number}
#'   \item{wy}{water year}
#'   \item{peak_va}{peak flow value}
#'   \item{peak_dt}{peak flow date}
#'   \item{drainage_area}{drainage area in sq.miles}
#'   \item{lat}{latitude}
#'   \item{long}{longitude}
#'   \item{altitude}{altitude in meters}
#'   \item{obs_per_wy}{observations per water year per site}
#'   \item{wy_count}{water year count per site}
#'   \item{Flow_sum}{Sum of Flow}
#'   \item{Flow_max}{Maximum of Flow}
#'   \item{Flow_min}{Minimum of Flow}
#'   \item{Flow_mean}{Mean of Flow}
#'   \item{Flow_median}{Median of Flow}
#'   \item{Flow_stdev}{Standard Deviation of Flow}
#'   \item{Flow_coef_var}{Coeffiecient of Variation of Flow}
#'   \item{Flow_max_dnorm}{Maximum of Flow normalized by drainage area}
#'   \item{Flow_min_dnorm}{Minimum of Flow normalized by drainage area}
#'   \item{Flow_mean_dnorm}{Mean of Flow normalized by drainage area}
#'   \item{Flow_med_dnorm}{Median of Flow normalized by drainage area}
#'   \item{Flow_max_sdnorm}{Maximum of Flow normalized by drainage area}
#'   \item{Flow_min_sdnorm}{Minimum of Flow normalized by standard deviation}
#'   \item{Flow_mean_sdnorm}{Mean of Flow normalized by standard deviation}
#'   \item{Flow_med_sdnorm}{Median of Flow normalized by standard deviation}
#'   \item{Flow_sd_norm}{Standard Deviation of Flow normalized by standard deviation}
#'   \item{decade}{decade}
#'   \item{COMID}{comid of site}
#'   \item{DamIndex}{dam index}
#' }
#' @return a tibble
"pnw_wy"
