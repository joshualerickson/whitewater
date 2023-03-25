
#' USGS stats
#' @description This function uses the \link[dataRetrieval]{readNWISstat} to gather daily, monthly or yearly
#' percentiles.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} USGS NWIS site.
#' @param temporalFilter A \code{character} for the stat summary window, e.g. 'daily' (default), 'monthly', 'yearly'.
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param days A \code{numeric} input of days to go back from today (only needed if using .temporalFilter = 'daily').
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @note Be aware, the parameter values ('Flow', 'Wtemp', etc) are calculated from the \link[whitewater]{ww_floorIVUSGS}
#' function by taking the daily mean of the hourly data. Thus, the instantaneous values will look different than the daily mean values, as it should.
#' The \code{.temporalFilter} argument is used to generate the window of percentiles.
#' @return a tibble with associated site statistics.
#' @importFrom dataRetrieval readNWISstat
#' @importFrom lubridate mday
#' @importFrom dplyr tibble
#' @export
#' @examples \dontrun{
#' # get by date range
#'
#' yaak_river_dv <- ww_dvUSGS('12304500')
#'
#' #daily
#' yaak_river_stats <- ww_statsUSGS(yaak_river_dv,
#'                                  temporalFilter = 'daily',
#'                                  days = 10)
#' #monthly
#' yaak_river_stats <- ww_statsUSGS(yaak_river_dv,
#'                                  temporalFilter = 'monthly',
#'                                  days = 10)
#' #yearly
#' yaak_river_stats <- ww_statsUSGS(yaak_river_dv,
#'                                  temporalFilter = 'yearly',
#'                                  days = 10)
#'}
ww_statsUSGS <- function(procDV,
                         sites = NULL,
                         temporalFilter = 'daily',
                         parameter_cd = NULL,
                         days = 10,
                         parallel = FALSE,
                         verbose = TRUE,
                         ...) {

  switch(temporalFilter,
         'daily' = ww_reportUSGSdv(procDV = procDV,
                                   sites = sites,
                                   parameter_cd = parameter_cd,
                                   days = days,
                                   parallel = parallel,
                                   verbose = verbose,
                                   ...),
         'monthly' = ww_reportUSGSmv(procDV = procDV,
                                     sites = sites,
                                     parameter_cd = parameter_cd,
                                     parallel = parallel,
                                     verbose = verbose,
                                     ...),
         'yearly' = ww_reportUSGSav(procDV = procDV,
                                    sites = sites,
                                    parameter_cd = parameter_cd,
                                    parallel = parallel,
                                    verbose = verbose,
                                    ...))
}

#' USGS Report Daily
#' @description This function uses the \link[dataRetrieval]{readNWISstat} to gather daily
#' statistics like quantiles/percentiles. Be aware, the parameter values ('Flow', 'Wtemp', etc) are calculated from the \link[whitewater]{ww_floorIVUSGS}
#' function by taking the daily mean of the hourly data. Thus, the instantaneous values will look different than the daily mean values, as it should.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} USGS NWIS site.
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param days A \code{numeric} input of days.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @return A \code{tibble} with daily stats.
#' @noRd
#' @importFrom dataRetrieval readNWISstat
#' @importFrom lubridate mday
#' @importFrom dplyr tibble
#'
ww_reportUSGSdv <- function(procDV,
                            sites = NULL,
                            parameter_cd = NULL,
                            days = 10,
                            parallel = FALSE,
                            verbose = TRUE,
                            ...) {


  if(!is.character(sites)) sites <- as.character(sites)

  site_station_days <- prepping_reports(procDV, sites, parameter_cd)

  delay <- delay_setup()

  #run over api, pulling necessary data.

  if(isTRUE(parallel)){

    usgs_statsdv <- site_station_days %>%
      split(.$sites) %>%
      furrr::future_map(safely(~usgs_stats_fun(., type = 'daily',
                                                  verbose = verbose,
                                                  delay = delay)),...) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill() %>%
      dt_to_tibble()

  } else {

    usgs_statsdv <- site_station_days %>%
      split(.$sites) %>%
      purrr::map(safely(~usgs_stats_fun(., type = 'daily',
                                           verbose = verbose,
                                           delay = 0))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill() %>%
      dt_to_tibble()

  }

  #join it with original


  final_data <- clean_hourly_dv_report(pp = site_station_days,
                                       data = usgs_statsdv,
                                       days = days,
                                       parallel = parallel)

  final_data %>%
    group_by(site_no,parameter_cd) %>%
    dplyr::mutate(dplyr::across(site_station_days$cols$cols,
                                ~dplyr::case_when(
                                  .x <= min_va  ~'All-time low for this day',
                                  .x <= p10_va & .x > min_va ~"Much below normal",
                                  .x > p10_va & .x <= p25_va ~ "Below normal",
                                  .x > p25_va & .x <= p75_va ~"Normal",
                                  .x > p75_va & .x <= p90_va~"Above normal",
                                  .x > p90_va & .x < max_va ~ "Much above normal",
                                  .x >= max_va ~ 'All-time high for this day',
                                  is.na(.x) ~ NA_character_,
                                  TRUE~"Not ranked"
                                ), .names = "{.col}_StatisticsStatusDescription")) %>%
    mutate(across(contains("_StatisticsStatusDescription"),
                  ~factor(.x,
                          levels = c("Not ranked",
                                     "All-time low for this day",
                                     "Much below normal",
                                     "Below normal",
                                     "Normal",
                                     "Above normal",
                                     "Much above normal",
                                     "All-time high for this day")))) %>%
    ungroup() %>%
    mutate(Date = lubridate::as_date(Date)) %>%
    dplyr::relocate(Station, site_no, drainage_area,
                    lat, long, altitude,dplyr::everything())

}


#' get daily stats
#'
#' @param data data.frame
#' @param type what type of temporal stat to perform
#' @param verbose logical to print information
#' @param delay time to delay in future_call
#'
#' @return a data.frame with daily or monthly stats from readNWISstat
#' @noRd
usgs_stats_fun <- function(data, type, verbose, delay) {


  final_data <- switch(type,
                       'daily' = suppressMessages(dataRetrieval::readNWISstat(data$sites,
                                                                              parameterCd = data$params[[1]],
                                                                              statType = 'all',
                                                                              statReportType = 'daily')) %>%
                         mutate(
                           drainage_area = readNWISsite(site_no) %>% select(drain_area_va) %>% as.numeric(),
                           Station = readNWISsite(site_no) %>% select(station_nm) %>% as.character(),
                           lat = readNWISsite(site_no) %>% select(dec_lat_va) %>% as.numeric(),
                           long = readNWISsite(site_no) %>% select(dec_long_va) %>% as.numeric(),
                           altitude = readNWISsite(site_no) %>% select(alt_va) %>% as.numeric()),
                       'monthly' = suppressMessages(dataRetrieval::readNWISstat(data$sites,
                                                                                parameterCd = data$params[[1]],
                                                                                statType = 'all',
                                                                                statReportType = 'monthly')) %>%
                         mutate(
                           drainage_area = readNWISsite(site_no) %>% select(drain_area_va) %>% as.numeric(),
                           Station = readNWISsite(site_no) %>% select(station_nm) %>% as.character(),
                           lat = readNWISsite(site_no) %>% select(dec_lat_va) %>% as.numeric(),
                           long = readNWISsite(site_no) %>% select(dec_long_va) %>% as.numeric(),
                           altitude = readNWISsite(site_no) %>% select(alt_va) %>% as.numeric()),
                       'yearly' = suppressMessages(dataRetrieval::readNWISstat(data$sites,
                                                                               parameterCd = data$params[[1]],
                                                                               statType = 'all',
                                                                               statReportType = 'annual')) %>%
                         mutate(
                           drainage_area = readNWISsite(site_no) %>% select(drain_area_va) %>% as.numeric(),
                           Station = readNWISsite(site_no) %>% select(station_nm) %>% as.character(),
                           lat = readNWISsite(site_no) %>% select(dec_lat_va) %>% as.numeric(),
                           long = readNWISsite(site_no) %>% select(dec_long_va) %>% as.numeric(),
                           altitude = readNWISsite(site_no) %>% select(alt_va) %>% as.numeric()))
  if(isTRUE(verbose)){
    if(nrow(final_data) < 1){

      cli::cli_alert_success('{usethis::ui_field(data$Station)} {usethis::ui_value("no values")} for Temporal Filter ({type}) and parameter_cd ({data$params[[1]]}).')

    } else {

      if(length(unique(final_data$parameter_cd)) != length(data$params[[1]])){

        cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, 1)$Station)} {usethis::ui_value("NWIS Stat")} for Temporal Filter ({type}) was successfully downloaded.
                              {crayon::red("(warning)")} parameter_cd {data$params[[1]][which(!(data$params[[1]] %in% unique(final_data$parameter_cd)))]} returned no data for {usethis::ui_field(dplyr::slice(final_data, 1)$Station)}.')

      } else {
        cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, 1)$Station)} {usethis::ui_value("NWIS Stat")} for Temporal Filter ({type}) was successfully downloaded.')
      }
    }
  }

  Sys.sleep(delay)

  final_data

}


#' clean up hourly report
#'
#' @param pp a parameter names
#' @param data data original
#' @param days number of days back
#' @param ... args to pass to ww_floorIVUSGS
#'
#' @return a data.frame
#' @noRd
#'
clean_hourly_dv_report <- function(pp, data, days, ...) {


  cols <- pp$cols[[1]]

  u_hour <- suppressMessages(ww_floorIVUSGS(sites = pp$sites,
                                            parameter_cd = pp$params[[1]],
                                            options = wwOptions(period = days),
                                            ...))


  u_hour <- u_hour %>%
    mutate(Date = lubridate::as_date(date),
           year = year(Date),
           month = month(Date),
           day = day(Date),
           month_day = str_c(month, day, sep = "-")) %>%
    group_by(Station, param_type, month_day, Date) %>%
    summarise(across(dplyr::any_of(cols), ~mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
    dplyr::rename(parameter_cd = 'param_type') %>%
    dplyr::mutate(parameter_cd = stringr::str_remove_all(parameter_cd, "param_"))

  u_hour <- u_hour %>% dplyr::mutate_all(~ifelse(is.nan(.), NA_real_, .))

  usgs_statsdv <- data %>%
    mutate(month_day = str_c(month_nu, day_nu, sep = "-"))

  t <- vector()

  for(i in 0:days){
    time <- lubridate::date(Sys.time()) - i
    time <- time %>% paste(month(.), mday(.), sep = "-") %>% stringr::str_remove("...........")
    t <- append(t, time)
  }

  usgs_statsdv <- usgs_statsdv %>% filter(month_day %in% t)

  u_hour <- u_hour %>% filter(month_day %in% t)

  usgs_statsdv <- usgs_statsdv %>% left_join(u_hour, by = c("Station", "parameter_cd", "month_day"))
}


#' USGS Report Monthly
#' @description Uses \link[dataRetrieval]{readNWISstat} to gather monthly mean flows. Furthermore,
#' monthly quantiles are generated similar to \link[whitewater]{ww_reportUSGSdv}.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} USGS NWIS site.
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#' @return A \code{tibble} with monthly stats.
#' @noRd
#' @importFrom dataRetrieval readNWISstat
#' @importFrom dplyr summarize arrange desc
#'
ww_reportUSGSmv <- function(procDV,
                            sites = NULL,
                            parameter_cd = NULL,
                            parallel = FALSE,
                            verbose = TRUE,
                            ...) {

  if(missing(procDV) & is.null(sites))stop("Need at least one argument")



  site_station_days <- prepping_reports(procDV, sites, parameter_cd)

  delay <- delay_setup()

  #run over api, pulling necessary data.

  if(isTRUE(parallel)){

    usgs_statsmv <- site_station_days %>%
      split(.$sites) %>%
      furrr::future_map(safely(~usgs_stats_fun(., type = 'monthly', verbose = verbose,delay = delay)),...) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill() %>%
      dt_to_tibble()

  } else {

    usgs_statsmv <- site_station_days %>%
      split(.$sites) %>%
      purrr::map(safely(~usgs_stats_fun(., type = 'monthly', verbose = verbose, delay = 0))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill() %>%
      dt_to_tibble()

  }


  summary_stats <- usgs_statsmv %>%
    group_by(Station, parameter_cd, month = month_nu) %>%
    summarize(p100_va = quantile(mean_va, probs = 1, na.rm = TRUE),
              p95_va = quantile(mean_va, probs = .95, na.rm = TRUE),
              p90_va = quantile(mean_va, probs = .90, na.rm = TRUE),
              p80_va = quantile(mean_va, probs = .80, na.rm = TRUE),
              p75_va = quantile(mean_va, probs = .75, na.rm = TRUE),
              p50_va = quantile(mean_va, probs = .50, na.rm = TRUE),
              p25_va = quantile(mean_va, probs = .25, na.rm = TRUE),
              p20_va = quantile(mean_va, probs = .20, na.rm = TRUE),
              p10_va = quantile(mean_va, probs = 0.1, na.rm = TRUE),
              p05_va = quantile(mean_va, probs = 0.05, na.rm = TRUE),
              p0_va = quantile(mean_va, probs = 0, na.rm = TRUE))

  usgs_statsmv  <-  usgs_statsmv %>%
    arrange(desc(year_nu)) %>%
    rename(month = "month_nu", mean_value = "mean_va") %>%
    left_join(summary_stats, by = c("month", "Station", 'parameter_cd')) %>%
    dplyr::mutate(date = lubridate::ym(paste(as.character(year_nu),'-', as.character(month)))) %>%
    group_by(site_no,parameter_cd) %>%
    dplyr::mutate(
      StatisticsStatusDescription = dplyr::case_when(
        mean_value <= p0_va  ~'All-time low for this month',
        mean_value <= p10_va & mean_value > p0_va ~"Much below normal",
        mean_value > p10_va & mean_value <= p25_va ~ "Below normal",
        mean_value > p25_va & mean_value <= p75_va ~"Normal",
        mean_value > p75_va & mean_value <= p90_va~"Above normal",
        mean_value > p90_va & mean_value < p100_va ~ "Much above normal",
        mean_value >= p100_va ~ 'All-time high for this month',
        TRUE~"Not ranked"
      ),
      StatisticsStatusDescription = factor(StatisticsStatusDescription,
                                           levels = c("Not ranked",
                                                      "All-time low for this month",
                                                      "Much below normal",
                                                      "Below normal",
                                                      "Normal",
                                                      "Above normal",
                                                      "Much above normal",
                                                      "All-time high for this month"))) %>%
    ungroup() %>%
    dplyr::relocate(Station, site_no, drainage_area,
                    lat, long, altitude,dplyr::everything())


}

#' USGS Report Annually
#' @description Uses \link[dataRetrieval]{readNWISstat} to gather monthly mean values. Furthermore,
#' monthly quantiles are generated similar to \link[whitewater]{ww_reportUSGSdv}.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} USGS NWIS site.
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param verbose \code{logical} for printing information. TRUE (default).
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @return A \code{tibble} with annual stats.
#' @noRd
#' @importFrom dataRetrieval readNWISstat
#' @importFrom dplyr summarize arrange desc
#'
ww_reportUSGSav <- function(procDV,
                            sites = NULL,
                            parameter_cd = NULL,
                            parallel = FALSE,
                            verbose = TRUE,
                            ...) {

  if(missing(procDV) & is.null(sites))stop("Need at least one argument")



  site_station_days <- prepping_reports(procDV, sites, parameter_cd)

  delay <- delay_setup()

  #run over api, pulling necessary data.

  if(isTRUE(parallel)){

    usgs_statsav <- site_station_days %>%
      split(.$sites) %>%
      furrr::future_map(safely(~usgs_stats_fun(., type = 'yearly', verbose = verbose, delay = delay)),...) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill() %>%
      dt_to_tibble()

  } else {

    usgs_statsav <- site_station_days %>%
      split(.$sites) %>%
      purrr::map(safely(~usgs_stats_fun(., type = 'yearly', verbose = verbose, delay = 0))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill() %>%
      dt_to_tibble()

  }


  summary_stats <- usgs_statsav %>%
    group_by(Station, parameter_cd) %>%
    summarize(p100_va = quantile(mean_va, probs = 1, na.rm = TRUE),
              p95_va = quantile(mean_va, probs = .95, na.rm = TRUE),
              p90_va = quantile(mean_va, probs = .90, na.rm = TRUE),
              p80_va = quantile(mean_va, probs = .80, na.rm = TRUE),
              p75_va = quantile(mean_va, probs = .75, na.rm = TRUE),
              p50_va = quantile(mean_va, probs = .50, na.rm = TRUE),
              p25_va = quantile(mean_va, probs = .25, na.rm = TRUE),
              p20_va = quantile(mean_va, probs = .20, na.rm = TRUE),
              p10_va = quantile(mean_va, probs = 0.1, na.rm = TRUE),
              p05_va = quantile(mean_va, probs = 0.05, na.rm = TRUE),
              p0_va = quantile(mean_va, probs = 0, na.rm = TRUE))

  usgs_statsav  <-  usgs_statsav %>%
    arrange(desc(year_nu)) %>%
    rename(year = "year_nu", mean_value = "mean_va") %>%
    left_join(summary_stats, by = c("Station", 'parameter_cd')) %>%
    arrange(Station) %>%
    group_by(site_no, parameter_cd) %>%
    dplyr::mutate(
      StatisticsStatusDescription = dplyr::case_when(
        mean_value <= p0_va  ~'All-time low for this year',
        mean_value <= p10_va & mean_value > p0_va ~"Much below normal",
        mean_value > p10_va & mean_value <= p25_va ~ "Below normal",
        mean_value > p25_va & mean_value <= p75_va ~"Normal",
        mean_value > p75_va & mean_value <= p90_va~"Above normal",
        mean_value > p90_va & mean_value < p100_va ~ "Much above normal",
        mean_value >= p100_va ~ 'All-time high for this year',
        TRUE~"Not ranked"
      ),
      StatisticsStatusDescription = factor(StatisticsStatusDescription,
                                           levels = c("Not ranked",
                                                      "All-time low for this year",
                                                      "Much below normal",
                                                      "Below normal",
                                                      "Normal",
                                                      "Above normal",
                                                      "Much above normal",
                                                      "All-time high for this year"))) %>%
    ungroup() %>%
    dplyr::relocate(Station, site_no, drainage_area,
                    lat, long, altitude,dplyr::everything())


}


#' Prep Reports
#'
#' @param sites A \code{character} USGS NWIS site.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param options A \link[whitewater]{wwOptions} call.
#'
#' @return A prepped tibble with  meta data for api call
#' @noRd
prepping_reports <- function(procDV, sites, parameter_cd, options = wwOptions()){

  if(is.null(sites)) {sites <- unique(procDV[['site_no']])}

  #create iteration value

  if(!missing(procDV)){

    sites <- unique(procDV$site_no)
    station <- unique(procDV$Station)
    params <- attr(procDV, 'parameter_cd')
    cols <- attr(procDV, 'parameter_cd_names')

  } else {

    if(is.null(parameter_cd)){usethis::ui_stop('need a parameter_cd')}

    if(!is.null(sites) & length(sites) == 1){
      sites <- unique(sites)

      station <- readNWISsite(sites) %>% select(station_nm) %>% as.character()

    }

    if(!is.null(sites) & length(sites) > 1) {

      sites <- unique(sites)

      station <- vector()
      for(i in 1:length(sites)){

        station_1 <- readNWISsite(sites[[i]]) %>% select(station_nm) %>% as.character()
        station <- append(station,station_1)
      }

    }

    params <- parameter_cd

    cols <- name_params_to_update(params)

  }
  #create blank dataframe to store the information in

  site_station_days <- tibble(sites = sites,
                              station = station,
                              cols = list(cols = cols),
                              params = list(params = params),
                              options = list(options))
}
