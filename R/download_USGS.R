

#' Process USGS daily values
#' @description This function is a wrapper around \link[dataRetrieval]{readNWISdv} but includes
#' added variables like water year, lat/lon, station name, altitude and tidied dates.
#' @param sites A vector of USGS NWIS sites
#' @param parameter_cd A USGS code for metric, default is "00060".
#' @param start_date A character of date format, e.g. \code{"1990-09-01"}
#' @param end_date A character of date format, e.g. \code{"1990-09-01"}
#' @param stat_cd character USGS statistic code. This is usually 5 digits. Daily mean (00003) is the default.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param wy_month \code{numeric} indicating the start month of the water year. e.g. 10 (default).
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#'
#' @note Use it the same way you would use \link[dataRetrieval]{readNWISdv}.
#' @return A \code{tibble} with daily metrics and added meta-data.
#' @export
#'
#' @examples \dontrun{
#'
#' library(whitewater)
#' yaak_river_dv <- ww_dvUSGS('12304500',
#' parameter_cd = '00060',
#' wy_month = 10)
#'
#' #parallel
#'
#' #get sites
#'
#' huc17_sites <- dataRetrieval::whatNWISdata(huc = 17,
#' siteStatus = 'active',
#' service = 'dv',
#' parameterCd = '00060')
#'
#' library(future)
#' #need to call future::plan()
#' plan(multisession(workers = availableCores()-1))
#'
#' pnw_dv <- ww_dvUSGS(huc17_sites$site_no,
#' parameter_cd = '00060',
#' wy_month = 10,
#' parallel = TRUE)
#'
#' }
#' @importFrom dataRetrieval readNWISdv renameNWISColumns readNWISsite
#' @importFrom dplyr select left_join "%>%"
#' @importFrom purrr safely

ww_dvUSGS <- function(sites,
                         parameter_cd = "00060",
                         start_date = "",
                         end_date = "",
                         stat_cd = "00003",
                         parallel = FALSE,
                         wy_month = 10,
                         verbose = TRUE,
                         ...) {


  if(!is.character(sites)) sites = as.character(sites)

  site_id_usgs <- data.frame(sites = sites)

  delay <- delay_setup()

  if(isTRUE(parallel)){

    usgs_raw_dv <- site_id_usgs %>%
                    split(.$sites) %>%
                    furrr::future_map(purrr::safely(~prepping_USGSdv(.$sites,
                                                                     parameter_cd,
                                                                     start_date,
                                                                     end_date,
                                                                     stat_cd,
                                                                     verbose,
                                                                     delay = delay
                                                                     )),
                        ...) %>%
                    purrr::keep(~length(.) != 0) %>%
                    purrr::map(~.x[['result']]) %>%
                    plyr::rbind.fill()

  } else {

    usgs_raw_dv <- site_id_usgs %>%
                    split(.$sites) %>%
                    purrr::map(purrr::safely(~prepping_USGSdv(.$sites,
                                                              parameter_cd,
                                                              start_date,
                                                              end_date,
                                                              stat_cd,
                                                              verbose,
                                                              delay = 0
                    ))) %>%
                    purrr::keep(~length(.) != 0) %>%
                    purrr::map(~.x[['result']]) %>%
                    plyr::rbind.fill()

  }


  if(is.null(usgs_raw_dv)){usethis::ui_stop("site query ran into an error: please check parameters or stat code...")}

  leap_years <- c(seq(1832,by = 4, length.out = 2000))

  usgs_raw_dv <- usgs_raw_dv %>%
                  dt_to_tibble() %>%
                  mutate(Date = lubridate::as_date(Date),
                         year = year(Date),
                         month = month(Date),
                         day = day(Date),
                         doy=lubridate::yday(Date),
                         wy_doy = ifelse(!(year %in% leap_years),ifelse(doy >= month_to_doy(wy_month, leap = F),
                                         doy-month_to_doy(wy_month, leap = F)+1,
                                         (365-month_to_doy(wy_month, leap = F)+1+doy)),
                                         ifelse(doy >= month_to_doy(wy_month, leap = T),
                                         doy-month_to_doy(wy_month, leap = T)+1,
                                         (366-month_to_doy(wy_month, leap = T)+1+doy))),
                         month_day = str_c(month, day, sep = "-"),
                         wy = waterYear(Date, wy_month, TRUE),
                         month_abb = factor(month.abb[month], levels = month.abb),
                         month_day = str_c(month, day, sep = "-"),
                         drainage_area = drainage_area*0.621371) %>%
                  add_date_counts()

  attr(usgs_raw_dv, 'wy_month') <- wy_month
  attr(usgs_raw_dv, 'parameter_cd') <- parameter_cd
  attr(usgs_raw_dv, 'stat_cd') <- stat_cd
  attr(usgs_raw_dv, 'parameter_cd_names') <- cols_to_update(usgs_raw_dv)


  usgs_raw_dv %>%
    dplyr::relocate(Station, site_no, drainage_area,
                    lat, long, altitude,dplyr::everything())

}

#' Prep USGS daily
#'
#' @param site_no A NWIS site number.
#' @param parameter_cd A USGS code for metric, default is "00060".
#' @param start_date A character of date format, e.g. \code{"1990-09-01"}
#' @param end_date A character of date format, e.g. \code{"1990-09-01"}
#' @param stat_cd character USGS statistic code. This is usually 5 digits. Daily mean (00003) is the default.
#' @param verbose logical for printing information.
#' @param delay time to delay in future_call
#' @importFrom crayon red
#' @noRd
#' @return A tidied data frame with gage meta-data.
prepping_USGSdv <- function(site_no, parameter_cd, start_date, end_date, stat_cd, verbose, delay) {

  gage_data <- readNWISdv(siteNumbers = site_no,
                          parameterCd = parameter_cd,
                          startDate = start_date,
                          endDate = end_date,
                          statCd = stat_cd) %>%
                          dataRetrieval::renameNWISColumns(p70290 = "Chloride")

  # could use attr(gage_data, 'siteInfo') but not a big deal IMO

  gage_info <- tibble(
    site_no = site_no,
    drainage_area = readNWISsite(site_no) %>% dplyr::select(drain_area_va) %>% as.numeric(),
    Station = readNWISsite(site_no) %>% dplyr::select(station_nm) %>% as.character(),
    lat = readNWISsite(site_no) %>% dplyr::select(dec_lat_va) %>% as.numeric(),
    long = readNWISsite(site_no) %>% dplyr::select(dec_long_va) %>% as.numeric(),
    altitude = readNWISsite(site_no) %>% dplyr::select(alt_va) %>% as.numeric()
  )


  final_data <- dplyr::left_join(gage_data, gage_info, by = 'site_no')

  if(isTRUE(verbose)){
  if(nrow(final_data) < 1){

    final_data <- NULL
    cli::cli_alert('{usethis::ui_field(final_data$Station)} {crayon::red("your request for ")} {usethis::ui_value("daily values")} with stat_cd ({stat_cd}), parameter_cd ({parameter_cd}) and site # ({site_no}) ran into a request error.')

  } else {

    cols_in_fd <- cols_to_update(final_data)
    names_in_params <- name_params_to_update(parameter_cd)

    if(length(unique(names_in_params)) != length(cols_in_fd)){

      cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, 1)$Station)} {usethis::ui_value("daily")} was successfully downloaded.
                              {crayon::red("(warning)")} parameter_cd {usethis::ui_value(names_in_params[which(!(names_in_params %in% cols_in_fd))])} returned no data for {usethis::ui_field(dplyr::slice(final_data, 1)$Station)}.')

     } else {
      cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, 1)$Station)} {usethis::ui_value("daily")} was successfully downloaded.')
     }
  }}

  Sys.sleep(delay)

  final_data



}


#' Water Year Stats (USGS)
#' @description This function uses the results of the \link[whitewater]{ww_dvUSGS} object to
#' generate mean, maximum, median, standard deviation and some normalization methods (drainage
#' area, scaled by log and standard deviation) per water year.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} vector with NWIS site numbers (optional).
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param ... arguments to pass on to \link[furrr]{future_map} and/or \link[whitewater]{ww_dvUSGS}.
#'
#' @note If a previously created \link[whitewater]{ww_dvUSGS} object is not used then the user needs to
#' provide a \code{sites} vector. This will run \link[whitewater]{ww_dvUSGS} in the background.
#' @return A \code{tibble} filtered by water year with added meta-data.
#' @export
#'
#' @examples \dontrun{
#'
#' library(whitewater)
#' yaak_river_dv <- ww_dvUSGS('12304500',
#' parameter_cd = '00060',
#' wy_month = 10)
#'
#' yaak_river_wy <- ww_wyUSGS(yaak_river_dv)
#'
#' #parallel
#'
#' #get sites
#'
#' huc17_sites <- dataRetrieval::whatNWISdata(huc = 17,
#' siteStatus = 'active',
#' service = 'dv',
#' parameterCd = '00060')
#'
#' library(future)
#' #need to call future::plan()
#' plan(multisession(workers = availableCores()-1))
#'
#' pnw_dv <- ww_dvUSGS(huc17_sites$site_no,
#' parameter_cd = '00060',
#' wy_month = 10,
#' parallel = TRUE)
#'
#' pnw_wy <- ww_wyUSGS(pnw_dv,
#'                     parallel = TRUE)
#' }
#'
#' @importFrom lubridate year month day
#' @importFrom dplyr mutate filter group_by summarise slice_head ungroup everything row_number n
#' @importFrom stringr str_c str_remove_all
#' @importFrom stats median sd
#'
ww_wyUSGS <- function(procDV,
                      sites = NULL,
                      parallel = FALSE,
                      verbose = TRUE,
                      ...) {

if(missing(procDV)) {

  if(isTRUE(parallel)){

    usgs_raw <- suppressMessages(ww_dvUSGS(sites = sites, parallel = parallel, ...) %>%
                pad_zero_for_logging())

         } else {

    usgs_raw <- suppressMessages(ww_dvUSGS(sites = sites, ...) %>%
                pad_zero_for_logging())

            }

         } else {

    usgs_raw <- procDV %>%
                pad_zero_for_logging()
            }



#summarize by water year with different stats for different params

  cols <- cols_to_update(usgs_raw)
  suppressWarnings(usgs_min_max_wy <- usgs_raw %>%
                      group_by(Station,wy,site_no) %>%
                      summarise(across(dplyr::any_of(cols),
                                         list(
                                         sum = ~sum(.x, na.rm = TRUE),
                                         max = ~max(.x, na.rm = TRUE),
                                         min = ~min(.x, na.rm = TRUE),
                                         mean = ~mean(.x, na.rm = TRUE),
                                         median = ~median(.x, na.rm = TRUE),
                                         stdev = ~sd(.x, na.rm = TRUE),
                                         coef_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE),
                                         max_dnorm = ~max(.x, na.rm = TRUE)/drainage_area,
                                         min_dnorm = ~min(.x, na.rm = TRUE)/drainage_area,
                                         mean_dnorm = ~mean(.x, na.rm = TRUE)/drainage_area,
                                         med_dnorm = ~median(.x, na.rm = TRUE)/drainage_area,
                                         max_sdnorm = ~log(max(.x, na.rm = TRUE))/sd(log(.x), na.rm = TRUE),
                                         min_sdnorm = ~log(min(.x, na.rm = TRUE))/sd(log(.x), na.rm = TRUE),
                                         mean_sdnorm = ~log(mean(.x, na.rm = TRUE))/sd(log(.x), na.rm = TRUE),
                                         med_sdnorm = ~log(median(.x, na.rm = TRUE))/sd(log(.x), na.rm = TRUE),
                                         sd_norm = ~sd(log(.x), na.rm = TRUE))))  %>%
                      slice_head(n=1) %>%
                      ungroup() %>%
                      dt_to_tibble() %>%
                      dplyr::left_join(usgs_raw %>%
                                 dplyr::select(
                                        wy,
                                        drainage_area,
                                        lat,
                                        long,
                                        altitude,
                                        site_no,
                                        obs_per_wy) %>%
                                 dplyr::group_by(site_no,wy) %>%
                                 dplyr::slice(1) %>%
                                 dplyr::ungroup(), by = c('site_no', 'wy')) %>%
                      dplyr::group_by(site_no) %>%
                      dplyr::add_count(name = 'wy_count') %>%
                      dplyr::ungroup() %>%
                      dplyr::relocate(Station, site_no, drainage_area,
                                      lat, long, altitude, obs_per_wy, wy_count, dplyr::everything())
)

  if(nrow(usgs_min_max_wy) < 1){

    usgs_min_max_wy <- NULL

  } else {
if(isTRUE(verbose)){
    cli::cli_alert_success('{usethis::ui_value("water year")} was successfully downloaded.')
}
  }

    if("Flow" %in% cols){

      delay <- delay_setup()

      wy_month <- attributes(usgs_raw)$wy_month

if(isTRUE(verbose)){
      cli::cli_alert('now starting to gather peak flows using dataRetrieval::readNWISpeak')
}
      usgs_min_max_wy <- ww_peakUSGSdv(usgs_min_max_wy, parallel = parallel, wy_month = wy_month, verbose = verbose, delay = delay)

    }

  usgs_min_max_wy
}

#' Get Peak Flows
#'
#' @param sites A vector of USGS NWIS sites
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param wy_month \code{numeric} indicating the start month of the water year. e.g. 10 (default).
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @return a \code{tibble} with peaks by water year
#' @export

ww_peakUSGS <- function(sites, parallel = FALSE,wy_month = 10,verbose = TRUE, ...) {

  peak_sites <- data.frame(peaks = sites)

  delay = delay_setup()

  if(isTRUE(parallel)){

    peaks <- peak_sites %>%
      split(.$peaks) %>%
      furrr::future_map(purrr::safely(~peaks_USGS(.$peaks,
                                                  wy_month = wy_month,
                                                  verbose = verbose,
                                                  dv = F,
                                                  ,
                                                  delay = delay)), ...) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill()

  } else {

    peaks <- peak_sites %>%
      split(.$peaks) %>%
      purrr::map(purrr::safely(~peaks_USGS(.$peaks,
                                           wy_month = wy_month,
                                           verbose = verbose,
                                           dv = F,
                                           delay = 0))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill()

  }

  usgs_min_max_wy <-  peaks %>%
    dt_to_tibble() %>%
    dplyr::select(Station, site_no, wy, peak_va, peak_dt, dplyr::everything())
}

#' Water Year & Monthly Stats (USGS)
#'
#' @description This function uses the results of the \link[whitewater]{ww_dvUSGS} object to
#' generate mean, maximum, median, standard deviation and coefficient of variation
#' per water year per month.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} vector with NWIS site numbers (optional).
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param ... arguments to pass on to \link[furrr]{future_map} and \link[whitewater]{ww_dvUSGS}.
#'
#' @note If a previously created \link[whitewater]{ww_dvUSGS} object is not used then the user needs to
#' provide a \code{sites} vector. This will run \link[whitewater]{ww_dvUSGS} in the background.
#' @return A \code{tibble} filtered by water year and month with added meta-data.
#' @export
#'
#' @examples \dontrun{
#'
#' library(whitewater)
#' yaak_river_dv <- ww_dvUSGS('12304500',
#' parameter_cd = '00060',
#' wy_month = 10)
#'
#' yaak_river_wym <- ww_wymUSGS(yaak_river_dv)
#'
#' }
#'
#' @importFrom dplyr group_by mutate across summarise rename right_join ungroup n
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate parse_date_time ymd
#' @importFrom stringr str_c
#'
ww_wymUSGS <- function(procDV, sites = NULL, parallel = FALSE, verbose = TRUE, ...) {

    if(missing(procDV)) {

    if(isTRUE(parallel)){

      usgs_raw <- ww_dvUSGS(sites = sites, parallel = TRUE, ...)

    } else {

      usgs_raw <- ww_dvUSGS(sites = sites, parallel = FALSE, ...)

    }


    } else {usgs_raw <- procDV }

      cols <- cols_to_update(usgs_raw)

      wy_month <- attributes(usgs_raw)$wy_month

      suppressWarnings(final_data <- usgs_raw %>%
                    group_by(Station, site_no, wy, month_abb, month) %>%
                    summarise(across(dplyr::any_of(cols),
                                     list(
                                       sum = ~sum(.x, na.rm = TRUE),
                                       max = ~max(.x, na.rm = TRUE),
                                       min = ~min(.x, na.rm = TRUE),
                                       mean = ~mean(.x, na.rm = TRUE),
                                       median = ~median(.x, na.rm = TRUE),
                                       stdev = ~sd(.x, na.rm = TRUE),
                                       coef_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE)))) %>%
                    slice_head(n=1) %>%
                    ungroup() %>%
                    mutate(wy_month_day =  str_c(wy, month,"1", sep = "-"),
                           wy_month_day =  parse_date_time(wy_month_day, orders = c("%y-%m-%d", "%y%m%d", "%y-%m-%d %H:%M")),
                           wy_month_day =  ymd(as.character(wy_month_day)),
                           doy = dplyr::case_when(month == 1 ~ 1,
                                                  month == 2 ~ 32,
                                                  month == 3 ~ 60,
                                                  month == 4 ~ 91,
                                                  month == 5 ~ 122,
                                                  month == 6 ~ 152,
                                                  month == 7 ~ 182,
                                                  month == 8 ~ 213,
                                                  month == 9 ~ 244,
                                                  month == 10 ~ 274,
                                                  month == 11 ~ 305,
                                                  month == 12 ~ 335,
                                                  TRUE ~ NA_real_),
                           ifelse(doy >= month_to_doy(wy_month, leap = F),
                                  doy-month_to_doy(wy_month, leap = F)+1,
                                  (365-month_to_doy(wy_month, leap = F)+1+doy))) %>%
                      dplyr::left_join(usgs_raw %>%
                                         dplyr::select(
                                                       wy,
                                                       month,
                                                       drainage_area,
                                                       lat,
                                                       long,
                                                       altitude,
                                                       site_no,
                                                       obs_per_wy,
                                                       obs_per_month) %>%
                                         dplyr::group_by(site_no, wy, month) %>%
                                         dplyr::slice(1) %>%
                                         dplyr::ungroup(), by = c('site_no', 'wy', 'month')) %>%
                      dplyr::group_by(site_no, wy) %>%
                      dplyr::add_count(name = 'wym_count') %>%
                      dplyr::ungroup() %>%
                      dplyr::relocate(Station, site_no, wy, wy_month_day, doy, drainage_area,
                                      lat, long, altitude, obs_per_wy,
                                      obs_per_month, wym_count, dplyr::everything())
      )



  if(nrow(final_data) < 1){

  final_data <- NULL

  } else {
if(isTRUE(verbose)){
  cli::cli_alert_success('{usethis::ui_value("water year and month")} was successfully downloaded.')

  }
}
  final_data %>%
    dplyr::relocate(Station, site_no, drainage_area,
                    lat, long, altitude,dplyr::everything())

}

#' Month-Only Stats (USGS)
#'
#' @description This function uses the results of the \link[whitewater]{ww_dvUSGS} object to
#' generate mean, maximum, median, standard deviation and coefficient of variation for month only.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} vector with NWIS site numbers (optional).
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param ... arguments to pass on to \link[furrr]{future_map} and \link[whitewater]{ww_dvUSGS}.
#' @note If a previously created \link[whitewater]{ww_dvUSGS} object is not used then the user needs to
#' provide a \code{sites} vector. This will run \link[whitewater]{ww_dvUSGS} in the background.
#' @return A \code{tibble} filtered by month and added meta-data.
#' @export
#' @examples \dontrun{
#'
#' library(whitewater)
#' yaak_river_dv <- ww_dvUSGS('12304500',
#' parameter_cd = '00060',
#' wy_month = 10)
#'
#' yaak_river_month <- ww_monthUSGS(yaak_river_dv)
#'
#' }
#' @importFrom dplyr group_by summarise mutate relocate
#'
#'

ww_monthUSGS <- function(procDV, sites = NULL, parallel = FALSE, verbose = TRUE, ...) {

  if(missing(procDV)) {

    if(isTRUE(parallel)){

      usgs_raw <- ww_dvUSGS(sites = sites, parallel = TRUE, ...)

    } else {

      usgs_raw <- ww_dvUSGS(sites = sites, parallel = FALSE, ...)

    }

  } else {

    usgs_raw <- procDV }

  cols <- cols_to_update(usgs_raw)

 suppressWarnings( final_data <- usgs_raw  %>%
                group_by(Station,site_no, month_abb) %>%
                summarise(across(dplyr::any_of(cols),
                                 list(
                                   sum = ~sum(.x, na.rm = TRUE),
                                   max = ~max(.x, na.rm = TRUE),
                                   min = ~min(.x, na.rm = TRUE),
                                   mean = ~mean(.x, na.rm = TRUE),
                                   median = ~median(.x, na.rm = TRUE),
                                   stdev = ~sd(.x, na.rm = TRUE),
                                   coef_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE)))) %>%
                          ungroup()%>%
               dplyr::left_join(usgs_raw %>%
                                  dplyr::select(
                                    drainage_area,
                                    lat,
                                    long,
                                    altitude,
                                    site_no,
                                    month_abb) %>%
                                  dplyr::group_by(site_no, month_abb) %>%
                                  dplyr::slice(1) %>%
                                  dplyr::ungroup(), by = c('site_no', 'month_abb')))


  if(nrow(final_data) < 1){

    final_data <- NULL

  } else {
if(isTRUE(verbose)){
    cli::cli_alert_success('{usethis::ui_value("monthly")} data was successfully downloaded.')
}
  }

  final_data %>%
    dplyr::relocate(Station, site_no, drainage_area,
                    lat, long, altitude,dplyr::everything())
}


#' Floor IV USGS
#' @description This function generates instantaneous NWIS data from \url{https://waterservices.usgs.gov/}
#' and then floors to a user defined interval with \link[whitewater]{wwOptions}
#'  ('1 hour' is default) by taking the mean.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{vector} of USGS NWIS sites (optional).
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param options A \link[whitewater]{wwOptions} call.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#'
#' @note For performance reasons, with multi-site retrievals you may
#' retrieve data since October 1, 2007 only. If a previously created \link[whitewater]{ww_dvUSGS} object is not used then the user needs to
#' provide a \code{sites} vector. This will run \link[whitewater]{ww_dvUSGS} in the background.
#' @return A \code{tibble} with a user defined interval time step.
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
#' #parallel
#'
#' #get sites
#'
#' huc17_sites <- dataRetrieval::whatNWISdata(huc = 17,
#' siteStatus = 'active',
#' service = 'dv',
#' parameterCd = '00060')
#'
#' library(future)
#' #need to call future::plan()
#' plan(multisession(workers = availableCores()-1))
#'
#' pnw_dv <- ww_dvUSGS(huc17_sites$site_no,
#' parameter_cd = '00060',
#' wy_month = 10,
#' parallel = TRUE)
#'
#' pnw_iv <- ww_floorIVUSGS(pnw_dv,
#'                     parallel = TRUE)
#' }
#' @export
#' @importFrom lubridate ymd_hm floor_date
#' @importFrom dplyr mutate rename rename_with group_by mutate relocate summarise ungroup contains
#' @importFrom dataRetrieval renameNWISColumns readNWISsite setAccess
#' @importFrom httr GET write_disk http_error
#'
ww_floorIVUSGS <- function(procDV,
                          sites = NULL,
                          parameter_cd = NULL,
                          options = wwOptions(),
                          parallel = FALSE,
                          verbose = TRUE,
                          ...) {

  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

  #create iteration value
  site_station_days <- prepping_reports(procDV, sites, parameter_cd, options)
  cols <- site_station_days$cols$cols
  #run over api, pulling necessary data.

  delay <- delay_setup()

  if(isTRUE(parallel)){

    usgs_download_hourly <- site_station_days %>%
                            split(.$sites) %>%
                            furrr::future_map(safely(~iv_USGS(., options = .$options[[1]],
                                                              type = 'hour', verbose = verbose,
                                                              delay = delay)), ...) %>%
                            purrr::keep(~length(.) != 0) %>%
                            purrr::map(~.x[['result']]) %>%
                            purrr::map(~rename_with(., ~gsub(".*_","", .x),
                                       dplyr::ends_with(paste0('_', cols)))) %>%
                            plyr::rbind.fill()

  } else {

    usgs_download_hourly <- site_station_days %>%
                            split(.$sites) %>%
                            purrr::map(safely(~iv_USGS(., options = .$options[[1]],
                                                       type = 'hour', verbose = verbose,
                                                       delay = 0)))%>%
                            purrr::keep(~length(.) != 0) %>%
                            purrr::map(~.x[['result']]) %>%
                            purrr::map(~rename_with(., ~gsub(".*_","", .x),
                                        dplyr::ends_with(paste0('_', cols)))) %>%
                            plyr::rbind.fill()

  }

  #final step: clean up data

  usgs_download_hourly <- usgs_download_hourly %>%
                          dt_to_tibble() %>%
                          mutate(date = ymd_hm(datetime))

  suppressWarnings(usgs_download_hourly <- usgs_download_hourly %>%
    mutate(date=floor_date(date, options[['floor_iv']])) %>%
    mutate(across(dplyr::any_of(cols), readr::parse_number)) %>%
    group_by(Station, site_no,param_type, date) %>%
    dplyr::summarise(across(dplyr::any_of(cols),
                            ~mean(.x, na.rm = TRUE))) %>%
    ungroup()) %>%
    dplyr::mutate_all(~ifelse(is.nan(.), NA_real_, .))

  # usgs_download_hourly <- usgs_download_hourly %>%
  #   tidyr::pivot_wider(names_from = param_type, values_from = dplyr::any_of(cols)) %>%
  #   dplyr::select_if(all_na) %>%
  #   dplyr::rename_with(~name_params_to_update(.x), dplyr::contains('param'))

  usgs_download_hourly <- usgs_download_hourly %>% dplyr::filter(!is.na(date))

}

#' Instantaneous USGS
#' @description This function generates Instantaneous NWIS data from
#' \url{https://waterservices.usgs.gov/}.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{vector} of USGS NWIS sites. \code{optional}
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param options A \link[whitewater]{wwOptions} call.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @note For performance reasons, with multi-site retrievals you may
#' retrieve data since October 1, 2007 only. If a previously created \link[whitewater]{ww_dvUSGS} object is not used then the user needs to
#' provide a \code{sites} vector. This will run \link[whitewater]{ww_dvUSGS} in the background.
#' @return A \code{tibble} with instantaneous values.
#' @export
#'
#' @examples \dontrun{
#'
#' library(whitewater)
#' yaak_river_dv <- ww_dvUSGS('12304500',
#' parameter_cd = '00060',
#' wy_month = 10)
#'
#' yaak_river_iv <- ww_instantaneousUSGS(yaak_river_dv)
#'
#' #change number of days
#'
#' yaak_river_iv <- ww_instantaneousUSGS(yaak_river_dv,
#'                                 options = wwOptions(period = 365))
#'
#' # get by date range
#'
#' yaak_river_wy <- ww_instantaneousUSGS(yaak_river_dv,
#'                                 options = wwOptions(date_range = 'date_range',
#'                                                     dates = c('2022-03-01', '2022-05-11')))
#'
#' # get most recent
#'
#' yaak_river_wy <- ww_instantaneousUSGS(yaak_river_dv,
#'                                 options = wwOptions(date_range = 'recent'))
#'
#' #parallel
#'
#' #get sites
#'
#' huc17_sites <- dataRetrieval::whatNWISdata(huc = 17,
#' siteStatus = 'active',
#' service = 'dv',
#' parameterCd = '00060')
#'
#' library(future)
#' #need to call future::plan()
#' plan(multisession(workers = availableCores()-1))
#'
#' pnw_dv <- ww_dvUSGS(huc17_sites$site_no,
#' parameter_cd = '00060',
#' wy_month = 10,
#' parallel = TRUE)
#'
#' pnw_iv <- ww_instantaneousUSGS(pnw_dv,
#'                     parallel = TRUE)
#' }
#'
#' @importFrom lubridate ymd_hm floor_date
#' @importFrom dplyr mutate rename rename_with group_by mutate relocate summarise ungroup contains
#' @importFrom dataRetrieval renameNWISColumns readNWISsite
#' @importFrom httr GET write_disk http_error
#'
ww_instantaneousUSGS <- function(procDV,
                                 sites = NULL,
                                 parameter_cd = NULL,
                                 options = wwOptions(),
                                 parallel = FALSE,
                                 verbose = TRUE,
                                 ...) {

  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

  #create iteration value
  site_station_days <- prepping_reports(procDV, sites, parameter_cd, options)
  cols <- site_station_days$cols$cols
  #run over api, pulling necessary data.

  delay <- delay_setup()

  if(isTRUE(parallel)){

    usgs_download_inst <- site_station_days %>%
      split(.$sites) %>%
      furrr::future_map(safely(~iv_USGS(., options = .$options[[1]],
                                        type = 'inst', verbose = verbose,
                                        delay = delay)),...) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']])%>%
      purrr::map(~rename_with(., ~gsub(".*_","", .x),
                              dplyr::ends_with(paste0('_', cols)))) %>%
      plyr::rbind.fill()

  } else {

    usgs_download_inst <- site_station_days %>%
      split(.$sites) %>%
      purrr::map(safely(~iv_USGS(., options = .$options[[1]],
                                 type = 'inst', verbose = verbose,
                                 delay = 0)))%>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      purrr::map(~rename_with(., ~gsub(".*_","", .x),
                              dplyr::ends_with(paste0('_', cols)))) %>%
      plyr::rbind.fill()

  }

  #final step: clean up data

  usgs_download_inst <- usgs_download_inst %>%
    dt_to_tibble()

  suppressWarnings(usgs_download_inst <- usgs_download_inst %>%
    mutate(across(dplyr::any_of(cols), iv_error_codes, .names = "{.col}_error")) %>%
    mutate(across(dplyr::any_of(cols), readr::parse_number)) %>%
    select(Station, site_no,param_type,date = 'datetime', dplyr::any_of(cols), dplyr::ends_with("_error")))

  # usgs_download_inst <- usgs_download_inst %>%
  #   tidyr::pivot_wider(names_from = param_type, values_from = dplyr::any_of(cols)) %>%
  #   dplyr::select_if(all_na) %>%
  #   dplyr::rename_with(~name_params_to_update(.x), dplyr::contains('param'))

  usgs_download_inst <- usgs_download_inst %>%
                        dplyr::filter(!is.na(date)) %>%
                        dplyr::mutate_all(~ifelse(is.nan(.), NA_real_, .)) %>%
    mutate(date = lubridate::ymd_hm(as.character(date, "%Y-%m-%d %H:%M")))
}
#' Prep USGS Instantaneous
#'
#' @param data the original data.frame
#' @param options a list of API arguments
#' @param type character of API call
#' @param verbose logical for printing information
#' @param delay time to delay in future_call
#'
#' @noRd
#' @return A data.frame with instantaneous values
#'
iv_USGS <- function(data, options, type, verbose, delay){

  df_final <- data.frame()

  for(i in data$params[[1]]){
  # download csv

  base_url <- switch(options$date_range,
  'pfn' = paste0(
    "https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=",
    data$sites,
    "&period=P",options$period,"D&parameterCd=", i, "&siteStatus=", options$site_status
  ),
  'date_range' = paste0(
    "https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=",
    data$sites,
    "&startDT=",options$dates[[1]],"T00:00-0000&endDT=",options$dates[[2]],"T00:00-0000",
    "&parameterCd=", i, "&siteStatus=", options$site_status
  ),
  "recent" = paste0(
    "https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=",
    data$sites,
    "&parameterCd=", i, "&siteStatus=", options$site_status
  )
  )
  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "usgs_tmp.csv"),
                                      overwrite = TRUE))
  # read RDB file to R
  df <- utils::read.table(file.path(tempdir(),"usgs_tmp.csv"),
                          header = TRUE,
                          sep = "\t",
                          stringsAsFactors = FALSE)
  #remove excess data
  df <- df[-1,]

  df <- df %>% dplyr::mutate(dplyr::across(contains(c('agency_cd', 'site_no', 'datetime')), ~dplyr::na_if(.,"")))

  df <- renameNWISColumns(df) %>%
        select(site_no, datetime, dplyr::contains(data$cols[[1]]))
  #add metadata

  df <- df %>%
        mutate(Station = paste0(data$station),
               param_type = paste0('param_',i)) %>%
        relocate(Station)


  df_final <- plyr::rbind.fill(df_final, df)


  }
if(isTRUE(verbose)){
  if(all(is.na(df_final[,c('site_no', 'datetime')]))){

    cli::cli_alert_warning('{usethis::ui_field({data$station})} ran into an error (no data for user input).')

  } else {

    final_data_logic <- df_final %>%
                        dplyr::group_by(param_type) %>%
                        dplyr::summarise(dplyr::across(c('site_no'), ~all(is.na(.))))

    param_names <- name_params_to_update(final_data_logic[which(final_data_logic['site_no']$site_no == TRUE),]$param_type)

    if(purrr::is_empty(param_names)){

        if(type == 'inst'){
        cli::cli_alert_success('{usethis::ui_field(dplyr::slice(df_final, 1)$Station)} {usethis::ui_value("instantaneous values")} were successfully downloaded.')

        } else if (type == 'hour'){
        cli::cli_alert_success('{usethis::ui_field(dplyr::slice(df_final, 1)$Station)} {usethis::ui_value("floored values")} were successfully downloaded.')

      }

         } else {

    if(type == 'inst'){
    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(df_final, 1)$Station)} {usethis::ui_value("instantaneous values")} were successfully downloaded.
                              {crayon::red("(warning)")} Parameter(s) {usethis::ui_value(param_names)} returned no data.')

      } else if (type == 'hour'){
    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(df_final, 1)$Station)} {usethis::ui_value("floored values")} were successfully downloaded.
                              {crayon::red("(warning)")} Parameter(s) {usethis::ui_value(param_names)} returned no data.')

        }
    }

  }
}

 Sys.sleep(delay)

 df_final

}

#' Prepping Peaks
#'
#' @param site_no A \code{character} USGS NWIS site.
#' @param wy_month a water year to filter by
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param dv internal logical
#' @param delay time to delay in future_call
#'
#' @return a data.frame with instantaneous peaks from USGS
#' @noRd
peaks_USGS <- function(site_no, wy_month, verbose, dv = TRUE, delay){

 final_data <- dataRetrieval::readNWISpeak(site_no)%>% select(peak_va, peak_dt, site_no) %>%
               mutate(wy = waterYear(peak_dt, wy_month, TRUE))

 if(!isTRUE(dv)){
   gage_info <- tibble(
     site_no = site_no,
     drainage_area = readNWISsite(site_no) %>% select(drain_area_va) %>% as.numeric(),
     Station = readNWISsite(site_no) %>% select(station_nm) %>% as.character(),
     lat = readNWISsite(site_no) %>% select(dec_lat_va) %>% as.numeric(),
     long = readNWISsite(site_no) %>% select(dec_long_va) %>% as.numeric(),
     altitude = readNWISsite(site_no) %>% select(alt_va) %>% as.numeric()
   )

   final_data <- dplyr::left_join(final_data, gage_info, by = c('site_no'))
 }
if(isTRUE(verbose)){
  if(nrow(final_data) < 1){

    cli::cli_alert_warning('{usethis::ui_field(dplyr::slice(final_data, 1)$site_no)} ran into an error.')

  } else {

    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, 1)$site_no)} {usethis::ui_value("peak flows")} were successfully downloaded.')

  }
}

  Sys.sleep(delay)

  final_data


}


#' Get Peak Flows
#'
#' @param data data.frame
#' @param parallel whether to use future_map or not
#' @param wy_month a water year to filter by
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param delay time to delay in future_call
#' @param ... other stuff to pass to future_map
#'
#' @return a \code{tibble} with peaks by water year
#' @noRd

ww_peakUSGSdv <- function(data, parallel,wy_month,verbose,delay, ...) {

peak_sites <- data.frame(peaks = unique(data$site_no))


if(isTRUE(parallel)){

  peaks <- peak_sites %>%
            split(.$peaks) %>%
            furrr::future_map(purrr::safely(~peaks_USGS(.$peaks,
                                                        wy_month = wy_month,
                                                        verbose = verbose,
                                                        delay = delay)), ...) %>%
            purrr::keep(~length(.) != 0) %>%
            purrr::map(~.x[['result']]) %>%
            plyr::rbind.fill()

} else {

  peaks <- peak_sites %>%
            split(.$peaks) %>%
            purrr::map(purrr::safely(~peaks_USGS(.$peaks,
                                                 wy_month = wy_month,
                                                 verbose = verbose,
                                                 delay = 0))) %>%
            purrr::keep(~length(.) != 0) %>%
            purrr::map(~.x[['result']]) %>%
            plyr::rbind.fill()

}


usgs_min_max_wy <-  data %>%
                    dt_to_tibble() %>%
                    dplyr::left_join(peaks, by = c("site_no", "wy")) %>%
                    dplyr::select(Station, site_no, wy, peak_va, peak_dt, dplyr::everything())
}


