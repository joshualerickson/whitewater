

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
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @note Use it the same way you would use \link[dataRetrieval]{readNWISdv}.
#' @return A \code{tibble} with daily metrics and added meta-data.
#' @export
#'
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
                         ...) {

  site_id_usgs <- data.frame(sites = sites)

  if(isTRUE(parallel)){

    usgs_raw_dv <- site_id_usgs %>%
                    split(.$sites) %>%
                    furrr::future_map(purrr::safely(~prepping_USGSdv(.$sites,
                                                                     parameter_cd,
                                                                     start_date,
                                                                     end_date,
                                                                     stat_cd
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
                                                              stat_cd
                    ))) %>%
                    purrr::keep(~length(.) != 0) %>%
                    purrr::map(~.x[['result']]) %>%
                    plyr::rbind.fill()

  }


  if(is.null(usgs_raw_dv)){usethis::ui_stop("site query ran into an error: please check parameters or stat code...")}

  usgs_raw_dv <- usgs_raw_dv %>%
                  dt_to_tibble() %>%
                  mutate(Date = lubridate::as_date(Date),
                         year = year(Date),
                         month = month(Date),
                         day = day(Date),
                         month_day = str_c(month, day, sep = "-"),
                         wy = waterYear(Date, wy_month, TRUE),
                         month_abb = factor(month.abb[month], levels = month.abb),
                         month_day = str_c(month, day, sep = "-"))

  attr(usgs_raw_dv, 'wy_month') <- wy_month
  attr(usgs_raw_dv, 'parameter_cd') <- parameter_cd
  attr(usgs_raw_dv, 'stat_cd') <- stat_cd
  attr(usgs_raw_dv, 'parameter_cd_names') <- cols_to_update(usgs_raw_dv)


  usgs_raw_dv

}

#' Prep USGS daily
#'
#' @param site_no A NWIS site number.
#' @param parameter_cd A USGS code for metric, default is "00060".
#' @param start_date A character of date format, e.g. \code{"1990-09-01"}
#' @param end_date A character of date format, e.g. \code{"1990-09-01"}
#' @param stat_cd character USGS statistic code. This is usually 5 digits. Daily mean (00003) is the default.
#'
#' @noRd
#' @return A tidied data frame with gage meta-data.
prepping_USGSdv <- function(site_no, parameter_cd, start_date, end_date, stat_cd) {

  gage_data <- readNWISdv(siteNumbers = site_no,
                          parameterCd = parameter_cd,
                          startDate = start_date,
                          endDate = end_date,
                          statCd = stat_cd) %>%
                          dataRetrieval::renameNWISColumns()

  # could use attr(gage_data, 'siteInfo') but not a big deal IMO

  gage_info <- tibble(
    site_no = site_no,
    drainage_area = readNWISsite(site_no) %>% select(drain_area_va) %>% as.numeric(),
    Station = readNWISsite(site_no) %>% select(station_nm) %>% as.character(),
    lat = readNWISsite(site_no) %>% select(dec_lat_va) %>% as.numeric(),
    long = readNWISsite(site_no) %>% select(dec_long_va) %>% as.numeric(),
    altitude = readNWISsite(site_no) %>% select(alt_va) %>% as.numeric()
  )


  final_data <- dplyr::left_join(gage_data, gage_info, by = 'site_no')

  if(nrow(final_data) < 1){

    final_data <- NULL
    cli::cli_alert('{usethis::ui_field(final_data$Station)} {crayon::red("your request for ")} {usethis::ui_value("daily values")} with stat_cd ({stat_cd}), parameter_cd ({parameter_cd}) and site # ({site_no}) ran into a request error.')

  } else {

    cols_in_fd <- cols_to_update(final_data)
    names_in_params <- name_params_to_update(parameter_cd)

    if(length(unique(names_in_params)) != length(cols_in_fd)){

      cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, n = 1)$Station)} {usethis::ui_value("daily")} was successfully downloaded.
                              {crayon::red("(warning)")} parameter_cd {usethis::ui_value(names_in_params[which(!(names_in_params %in% cols_in_fd))])} returned no data for {usethis::ui_field(dplyr::slice(final_data, n = 1)$Station)}.')

     } else {
      cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, n = 1)$Station)} {usethis::ui_value("daily")} was successfully downloaded.')
     }
  }

  final_data
}


#' Water Year Stats (USGS)
#' @description This function uses the results of the \link[whitewater]{ww_dvUSGS} object to
#' generate mean, maximum, median, standard deviation and some normalization methods (drainage
#' area, scaled by log and standard deviation) per water year.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} vector with NWIS site numbers (optional).
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map} and/or \link[whitewater]{ww_dvUSGS}.
#'
#' @note If a previously created \link[whitewater]{ww_dvUSGS} object is not used then the user needs to
#' provide a \code{sites} vector. This will run \link[whitewater]{ww_dvUSGS} in the background.
#' @return A \code{tibble} filtered by water year with added meta-data.
#' @export
#'
#' @importFrom lubridate year month day
#' @importFrom dplyr mutate filter group_by summarise slice_head ungroup everything row_number n
#' @importFrom stringr str_c str_remove_all
#' @importFrom stats median sd
ww_wyUSGS <- function(procDV,
                      sites = NULL,
                      parallel = FALSE,
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
  usgs_min_max_wy <- usgs_raw %>%
                      group_by(Station,wy,site_no) %>%
                      summarise(across(dplyr::any_of(cols),
                                       list(
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
                      dt_to_tibble()

  if(nrow(usgs_min_max_wy) < 1){

    usgs_min_max_wy <- NULL

  } else {

    cli::cli_alert_success('{usethis::ui_value("water year")} was successfully downloaded.')

  }

    if("Flow" %in% cols){

      wy_month <- attributes(usgs_raw)$wy_month

      cli::cli_alert('now starting to gather peak flows using dataRetrieval::readNWISpeak')

      usgs_min_max_wy <- adding_peaks_to_df(usgs_min_max_wy, parallel = parallel, wy_month = wy_month)

      }

  usgs_min_max_wy
}

#' Water Year & Monthly Stats (USGS)
#'
#' @description This function uses the results of the \link[whitewater]{ww_dvUSGS} object to
#' generate mean, maximum, median, standard deviation and coefficient of variation
#' per water year per month.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} vector with NWIS site numbers (optional).
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map} and \link[whitewater]{ww_dvUSGS}.
#'
#' @note If a previously created \link[whitewater]{ww_dvUSGS} object is not used then the user needs to
#' provide a \code{sites} vector. This will run \link[whitewater]{ww_dvUSGS} in the background.
#' @return A \code{tibble} filtered by water year and month with added meta-data.
#' @export
#'
#' @importFrom dplyr group_by mutate across summarise rename right_join ungroup n
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate parse_date_time ymd
#' @importFrom stringr str_c
#' @importFrom ape where
#'
ww_wymUSGS <- function(procDV, sites = NULL, parallel = FALSE, ...) {

    if(missing(procDV)) {

    if(isTRUE(parallel)){

      usgs_raw <- ww_dvUSGS(sites = sites, parallel = TRUE, ...)

    } else {

      usgs_raw <- ww_dvUSGS(sites = sites, parallel = FALSE, ...)

    }


    } else {usgs_raw <- procDV }

      cols <- cols_to_update(usgs_raw)

      final_data <- usgs_raw %>%
                    group_by(Station, wy, month_abb, month) %>%
                    summarise(across(dplyr::any_of(cols),
                                     list(
                                       max = ~max(.x, na.rm = TRUE),
                                       min = ~min(.x, na.rm = TRUE),
                                       mean = ~mean(.x, na.rm = TRUE),
                                       median = ~median(.x, na.rm = TRUE),
                                       stdev = ~sd(.x, na.rm = TRUE),
                                       coef_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE)))) %>%
                    slice_head(n=1) %>%
                    ungroup() %>%
                    mutate(year_month =  str_c(wy, month,"1", sep = "-"),
                           year_month =  parse_date_time(year_month, orders = c("%y-%m-%d", "%y%m%d", "%y-%m-%d %H:%M")),
                           year_month =  ymd(as.character(year_month)))


  if(nrow(final_data) < 1){

  final_data <- NULL

  } else {

  cli::cli_alert_success('{usethis::ui_value("water year and month")} was successfully downloaded.')

  }

  final_data

}

#' Month-Only Stats (USGS)
#'
#' @description This function uses the results of the \link[whitewater]{ww_dvUSGS} object to
#' generate mean, maximum, median, standard deviation and coefficient of variation for month only.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} vector with NWIS site numbers (optional).
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map} and \link[whitewater]{ww_dvUSGS}.
#' @note If a previously created \link[whitewater]{ww_dvUSGS} object is not used then the user needs to
#' provide a \code{sites} vector. This will run \link[whitewater]{ww_dvUSGS} in the background.
#' @return A \code{tibble} filtered by month and added meta-data.
#' @export
#' @importFrom dplyr group_by summarise mutate relocate
#'
#'

ww_monthUSGS <- function(procDV, sites = NULL, parallel = FALSE, ...) {

  if(missing(procDV)) {

    if(isTRUE(parallel)){

      usgs_raw <- ww_dvUSGS(sites = sites, parallel = TRUE, ...)

    } else {

      usgs_raw <- ww_dvUSGS(sites = sites, parallel = FALSE, ...)

    }

  } else {

    usgs_raw <- procDV }

  cols <- cols_to_update(usgs_raw)

  final_data <- usgs_raw  %>%
                group_by(Station, month_abb) %>%
                summarise(across(dplyr::any_of(cols),
                                 list(
                                   max = ~max(.x, na.rm = TRUE),
                                   min = ~min(.x, na.rm = TRUE),
                                   mean = ~mean(.x, na.rm = TRUE),
                                   median = ~median(.x, na.rm = TRUE),
                                   stdev = ~sd(.x, na.rm = TRUE),
                                   coef_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE)))) %>%
                          ungroup()

  if(nrow(final_data) < 1){

    final_data <- NULL

  } else {

    cli::cli_alert_success('{usethis::ui_value("monthly")} data was successfully downloaded.')

  }

  final_data
}


#' Floor IV USGS
#' @description This function generates instantaneous NWIS data from \url{https://waterservices.usgs.gov/nwis/iv/}
#' and then floors to a user defined interval with \link[whitewater]{wwOptions}
#'  ('1 hour' is default) by taking the mean.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{vector} of USGS NWIS sites (optional).
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param options A \link[whitewater]{wwOptions} call.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#'
#' @note For performance reasons, with multi-site retrievals you may
#' retrieve data since October 1, 2007 only. If a previously created \link[whitewater]{ww_dvUSGS} object is not used then the user needs to
#' provide a \code{sites} vector. This will run \link[whitewater]{ww_dvUSGS} in the background.
#' @return A \code{tibble} with a user defined interval time step.
#'
#' @export
#' @importFrom lubridate ymd_hm floor_date
#' @importFrom dplyr mutate rename rename_with group_by mutate relocate summarise ungroup contains
#' @importFrom dataRetrieval renameNWISColumns readNWISsite
#' @importFrom httr GET write_disk http_error
#'
ww_floorIVUSGS <- function(procDV,
                          sites = NULL,
                          parameter_cd = NULL,
                          options = wwOptions(),
                          parallel = FALSE, ...) {

  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

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

  site_station_days <- tibble(sites = sites,
                              station = station,
                              cols = list(cols = cols),
                              params = list(params = params),
                              options = list(options))

  #run over api, pulling necessary data.

  if(isTRUE(parallel)){

    usgs_download_hourly <- site_station_days %>%
                            split(.$sites) %>%
                            furrr::future_map(safely(~iv_USGS(., options = .$options[[1]],
                                                              type = 'hour')), ...) %>%
                            purrr::keep(~length(.) != 0) %>%
                            purrr::map(~.x[['result']]) %>%
                            purrr::map(~rename_with(., ~gsub(".*_","", .x),
                                       dplyr::ends_with(paste0('_', cols)))) %>%
                            plyr::rbind.fill()

  } else {

    usgs_download_hourly <- site_station_days %>%
                            split(.$sites) %>%
                            purrr::map(safely(~iv_USGS(., options = .$options[[1]],
                                                       type = 'hour')))%>%
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

  usgs_download_hourly <- usgs_download_hourly %>%
    mutate(date=floor_date(date, options[['floor_iv']])) %>%
    mutate(across(dplyr::any_of(cols), readr::parse_number)) %>%
    group_by(Station, site_no,param_type, date) %>%
    dplyr::summarise(across(dplyr::any_of(cols),
                            ~mean(.x, na.rm = TRUE))) %>%
    ungroup()

  usgs_download_hourly <- usgs_download_hourly %>%
    tidyr::pivot_wider(names_from = param_type, values_from = dplyr::any_of(cols)) %>%
    dplyr::select_if(all_na) %>%
    dplyr::rename_with(~name_params_to_update(.x), dplyr::contains('param'))

}

#' Instantaneous USGS
#' @description This function generates Instantaneous NWIS data from
#' \url{https://waterservices.usgs.gov/nwis/iv/}.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{vector} of USGS NWIS sites. \code{optional}
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param options A \link[whitewater]{wwOptions} call.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @note For performance reasons, with multi-site retrievals you may
#' retrieve data since October 1, 2007 only. If a previously created \link[whitewater]{ww_dvUSGS} object is not used then the user needs to
#' provide a \code{sites} vector. This will run \link[whitewater]{ww_dvUSGS} in the background.
#' @return A \code{tibble} with instantaneous values.
#' @export
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
                                 parallel = FALSE, ...) {

  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

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

  site_station_days <- tibble(sites = sites,
                              station = station,
                              cols = list(cols = cols),
                              params = list(params = params),
                              options = list(options))

  #run over api, pulling necessary data.

  if(isTRUE(parallel)){

    usgs_download_inst <- site_station_days %>%
      split(.$sites) %>%
      furrr::future_map(safely(~iv_USGS(., options = .$options[[1]],
                                        type = 'inst')),...) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']])%>%
      purrr::map(~rename_with(., ~gsub(".*_","", .x),
                              dplyr::ends_with(paste0('_', cols)))) %>%
      plyr::rbind.fill()

  } else {

    usgs_download_inst <- site_station_days %>%
      split(.$sites) %>%
      purrr::map(safely(~iv_USGS(., options = .$options[[1]],
                                 type = 'inst')))%>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      purrr::map(~rename_with(., ~gsub(".*_","", .x),
                              dplyr::ends_with(paste0('_', cols)))) %>%
      plyr::rbind.fill()

  }

  #final step: clean up data

  usgs_download_inst <- usgs_download_inst %>%
    dt_to_tibble() %>%
    mutate(date = ymd_hm(datetime))

  usgs_download_inst <- usgs_download_inst %>%
    mutate(across(dplyr::any_of(cols), iv_error_codes, .names = "{.col}_error")) %>%
    mutate(across(dplyr::any_of(cols), readr::parse_number)) %>%
    select(Station, site_no,param_type, date, dplyr::any_of(cols), dplyr::ends_with("_error"))

  usgs_download_inst <- usgs_download_inst %>%
    tidyr::pivot_wider(names_from = param_type, values_from = dplyr::any_of(cols)) %>%
    dplyr::select_if(all_na) %>%
    dplyr::rename_with(~name_params_to_update(.x), dplyr::contains('param'))

  usgs_download_inst <- usgs_download_inst %>% dplyr::filter(!is.na(date))
}
#' Prep USGS Istantaneous
#'
#' @param data the original data.frame
#' @param options a list of API arguments
#' @param type character of API call
#'
#' @noRd
#' @return A data.frame with instantaneous values
#'
iv_USGS <- function(data, options, type){

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

  if(all(is.na(df_final[,c('site_no', 'datetime')]))){

    cli::cli_alert_warning('{usethis::ui_field({data$station})} ran into an error (no data for user input).')

  } else {

    final_data_logic <- df_final %>%
                        dplyr::group_by(param_type) %>%
                        dplyr::summarise(dplyr::across(c('site_no'), ~all(is.na(.))))
    param_names <- name_params_to_update(final_data_logic[which(final_data_logic['site_no']$site_no == TRUE),]$param_type)

    if(all(isTRUE(final_data_logic[,c('site_no')]$site_no))){

        if(type == 'inst'){
        cli::cli_alert_success('{usethis::ui_field(dplyr::slice(df_final, n = 1)$Station)} {usethis::ui_value("instantaneous values")} were successfully downloaded.')

        } else if (type == 'hour'){
        cli::cli_alert_success('{usethis::ui_field(dplyr::slice(df_final, n = 1)$Station)} {usethis::ui_value("floored values")} were successfully downloaded.')

      }

         } else {

    if(type == 'inst'){
    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(df_final, n = 1)$Station)} {usethis::ui_value("instantaneous values")} were successfully downloaded.
                              {crayon::red("(warning)")} Parameter(s) {usethis::ui_value(param_names)} returned no data.')

      } else if (type == 'hour'){
    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(df_final, n = 1)$Station)} {usethis::ui_value("floored values")} were successfully downloaded.
                              {crayon::red("(warning)")} Parameter(s) {usethis::ui_value(param_names)} returned no data.')

        }
    }

  }

 df_final

}

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

#' NWIS stats
#' @description This function uses the \link[dataRetrieval]{readNWISstat} to gather daily
#' statistics like quantiles/percentiles.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} USGS NWIS site.
#' @param temporalFilter A \code{character} for the stat summary window, e.g. 'daily' (default), 'monthly', 'yearly'.
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param days A \code{numeric} input of days to go back from today (only needed if using .temporalFilter = 'daily').
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @note Be aware, the parameter values ('Flow', 'Wtemp', etc) are calculated from the \link[whitewater]{ww_floorIVUSGS}
#' function by taking the daily mean of the hourly data. Thus, the instantaneous values will look different than the daily mean values, as it should.
#' The \code{.temporalFilter} argument is used to generate the window of percentiles.
#' @importFrom dataRetrieval readNWISstat
#' @importFrom lubridate mday
#' @importFrom dplyr tibble
#' @export
#'
ww_statsNWIS <- function(procDV,
                            sites = NULL,
                            temporalFilter = 'daily',
                            parameter_cd = NULL,
                            days = 10,
                            parallel = FALSE, ...) {

  switch(temporalFilter,
         'daily' = ww_reportUSGSdv(procDV = procDV,
                                   sites = sites,
                                   parameter_cd = parameter_cd,
                                   days = days,
                                   parallel = parallel,
                                   ...),
         'monthly' = ww_reportUSGSmv(procDV = procDV,
                                     sites = sites,
                                     parameter_cd = parameter_cd,
                                     parallel = parallel,
                                     ...),
         'yearly' = ww_reportUSGSav(procDV = procDV,
                                     sites = sites,
                                     parameter_cd = parameter_cd,
                                     parallel = parallel,
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
                            parallel = FALSE, ...) {


  site_station_days <- prepping_reports(procDV, sites, parameter_cd)

  #run over api, pulling necessary data.

  if(isTRUE(parallel)){

    usgs_statsdv <- site_station_days %>%
                    split(.$sites) %>%
                    furrr::future_map(safely(~usgs_stats_fun(., type = 'daily')),...) %>%
                    purrr::keep(~length(.) != 0) %>%
                    purrr::map(~.x[['result']]) %>%
                    plyr::rbind.fill() %>%
                    dt_to_tibble()

  } else {

    usgs_statsdv <- site_station_days %>%
                    split(.$sites) %>%
                    purrr::map(safely(~usgs_stats_fun(., type = 'daily'))) %>%
                    purrr::keep(~length(.) != 0) %>%
                    purrr::map(~.x[['result']]) %>%
                    plyr::rbind.fill() %>%
                    dt_to_tibble()

  }

#join it with original


  final_data <- clean_hourly_dv_report(pp = site_station_days,
                                       data = usgs_statsdv, days = days)

  final_data

}


#' get daily stats
#'
#' @param data data.frame
#'
#' @return a data.frame with daily or monthly stats from readNWISstat
#' @noRd
usgs_stats_fun <- function(data, type) {


  final_data <- switch(type,
             'daily' = suppressMessages(dataRetrieval::readNWISstat(data$sites,
                    parameterCd = data$params[[1]],
                    statType = 'all',
                    statReportType = 'daily')) %>%
                    mutate(Station = readNWISsite(data$sites) %>%
                    select(station_nm) %>%
                    as.character()),
          'monthly' = suppressMessages(dataRetrieval::readNWISstat(data$sites,
                    parameterCd = data$params[[1]],
                    statType = 'all',
                    statReportType = 'monthly')) %>%
                    mutate(Station = readNWISsite(data$sites) %>%
                    select(station_nm) %>%
                    as.character()),
          'yearly' = suppressMessages(dataRetrieval::readNWISstat(data$sites,
                    parameterCd = data$params[[1]],
                    statType = 'all',
                    statReportType = 'annual')) %>%
                    mutate(Station = readNWISsite(data$sites) %>%
                    select(station_nm) %>%
                    as.character()))

  if(nrow(final_data) < 1){

    cli::cli_alert_success('{usethis::ui_field(data$Station)} {usethis::ui_value("no values")} for Temporal Filter ({type}) and parameter_cd ({data$params[[1]]}).')

  } else {

    if(length(unique(final_data$parameter_cd)) != length(data$params[[1]])){

      cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, n = 1)$Station)} {usethis::ui_value("NWIS Stat")} for Temporal Filter ({type}) was successfully downloaded.
                              {crayon::red("(warning)")} parameter_cd {data$params[[1]][which(!(data$params[[1]] %in% unique(final_data$parameter_cd)))]} returned no data for {usethis::ui_field(dplyr::slice(final_data, n = 1)$Station)}.')

    } else {
    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, n = 1)$Station)} {usethis::ui_value("NWIS Stat")} for Temporal Filter ({type}) was successfully downloaded.')
    }
  }

  final_data

}


#' clean up hourly report
#'
#' @param pp a parameter names
#' @param data data original
#' @param days number of days back
#'
#' @return a data.frame
#' @noRd
#'
clean_hourly_dv_report <- function(pp, data, days) {


  cols <- pp$cols[[1]]

  u_hour <- suppressMessages(ww_floorIVUSGS(sites = pp$sites,
                       parameter_cd = pp$params[[1]],
                       options = wwOptions(period = days)))

  u_hour <- u_hour %>%
    mutate(Date = lubridate::as_date(date),
           year = year(Date),
           month = month(Date),
           day = day(Date),
           month_day = str_c(month, day, sep = "-")) %>%
    group_by(Station, month_day, Date) %>%
    summarise(across(dplyr::any_of(cols), ~mean(.x, na.rm = TRUE)))

  usgs_statsdv <- data %>%
    mutate(month_day = str_c(month_nu, day_nu, sep = "-"))

  t <- vector()

  for(i in 0:days){
    time <- lubridate::date(Sys.time()) - i
    time <- time %>% paste(month(.), mday(.), sep = "-") %>% str_remove("...........")
    t <- append(t, time)
  }

  usgs_statsdv <- usgs_statsdv %>% filter(month_day %in% t)

  u_hour <- u_hour %>% filter(month_day %in% t)

  usgs_statsdv <- usgs_statsdv %>% left_join(u_hour, by = c("Station", "month_day"))
}


#' USGS Report Monthly
#' @description Uses \link[dataRetrieval]{readNWISstat} to gather monthly mean flows. Furthermore,
#' monthly quantiles are generated similar to \link[whitewater]{ww_reportUSGSdv}.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} USGS NWIS site.
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param parallel \code{logical} indicating whether to use future_map().
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
                            ...) {

if(missing(procDV) & is.null(sites))stop("Need at least one argument")



  site_station_days <- prepping_reports(procDV, sites, parameter_cd)

  #run over api, pulling necessary data.

  if(isTRUE(parallel)){

    usgs_statsmv <- site_station_days %>%
      split(.$sites) %>%
      furrr::future_map(safely(~usgs_stats_fun(., type = 'monthly')),...) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill() %>%
      dt_to_tibble()

  } else {

    usgs_statsmv <- site_station_days %>%
      split(.$sites) %>%
      purrr::map(safely(~usgs_stats_fun(., type = 'monthly'))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill() %>%
      dt_to_tibble()

  }


    summary_stats <- usgs_statsmv %>%
                      group_by(Station, parameter_cd, month = month_nu) %>%
                      summarize(p95_va = quantile(mean_va, probs = .95, na.rm = TRUE),
                                p90_va = quantile(mean_va, probs = .90, na.rm = TRUE),
                                p80_va = quantile(mean_va, probs = .80, na.rm = TRUE),
                                p75_va = quantile(mean_va, probs = .75, na.rm = TRUE),
                                p50_va = quantile(mean_va, probs = .50, na.rm = TRUE),
                                p25_va = quantile(mean_va, probs = .25, na.rm = TRUE),
                                p20_va = quantile(mean_va, probs = .20, na.rm = TRUE),
                                p10_va = quantile(mean_va, probs = 0.1, na.rm = TRUE),
                                p05_va = quantile(mean_va, probs = 0.05, na.rm = TRUE))

    usgs_statsmv  <-  usgs_statsmv %>%
                      arrange(desc(year_nu)) %>%
                      rename(month = "month_nu", mean_value = "mean_va") %>%
                      left_join(summary_stats, by = c("month", "Station", 'parameter_cd')) %>%
                      dplyr::mutate(date = lubridate::ym(paste(as.character(year_nu),'-', as.character(month))))


}

#' USGS Report Annually
#' @description Uses \link[dataRetrieval]{readNWISstat} to gather monthly mean values. Furthermore,
#' monthly quantiles are generated similar to \link[whitewater]{ww_reportUSGSdv}.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param sites A \code{character} USGS NWIS site.
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#' @param parallel \code{logical} indicating whether to use future_map().
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
                            ...) {

  if(missing(procDV) & is.null(sites))stop("Need at least one argument")



  site_station_days <- prepping_reports(procDV, sites, parameter_cd)

  #run over api, pulling necessary data.

  if(isTRUE(parallel)){

    usgs_statsav <- site_station_days %>%
      split(.$sites) %>%
      furrr::future_map(safely(~usgs_stats_fun(., type = 'yearly')),...) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill() %>%
      dt_to_tibble()

  } else {

    usgs_statsav <- site_station_days %>%
      split(.$sites) %>%
      purrr::map(safely(~usgs_stats_fun(., type = 'yearly'))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill() %>%
      dt_to_tibble()

  }


  summary_stats <- usgs_statsav %>%
    group_by(Station, parameter_cd) %>%
    summarize(p95_va = quantile(mean_va, probs = .95, na.rm = TRUE),
              p90_va = quantile(mean_va, probs = .90, na.rm = TRUE),
              p80_va = quantile(mean_va, probs = .80, na.rm = TRUE),
              p75_va = quantile(mean_va, probs = .75, na.rm = TRUE),
              p50_va = quantile(mean_va, probs = .50, na.rm = TRUE),
              p25_va = quantile(mean_va, probs = .25, na.rm = TRUE),
              p20_va = quantile(mean_va, probs = .20, na.rm = TRUE),
              p10_va = quantile(mean_va, probs = 0.1, na.rm = TRUE),
              p05_va = quantile(mean_va, probs = 0.05, na.rm = TRUE))

  usgs_statsav  <-  usgs_statsav %>%
    arrange(desc(year_nu)) %>%
    rename(year = "year_nu", mean_value = "mean_va") %>%
    left_join(summary_stats, by = c("Station", 'parameter_cd')) %>%
    arrange(Station)


}
#' Prep Reports
#'
#' @param sites A \code{character} USGS NWIS site.
#' @param procDV A previously created \link[whitewater]{ww_dvUSGS} object.
#' @param parameter_cd A USGS code parameter code, only if using \code{sites} argument.
#'
#' @return A prepped tibblewith  meta data for api call
#' @noRd
prepping_reports <- function(procDV, sites, parameter_cd){

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
                            params = list(params = params))
}




#' Prepping Peaks
#'
#' @param site_no A \code{character} USGS NWIS site.
#'
#' @return a data.frame with instantaneous peaks from USGS
#' @noRd
peaks_USGS <- function(site_no, wy_month){

 final_data <- dataRetrieval::readNWISpeak(site_no)%>% select(peak_va, peak_dt, site_no) %>%
               mutate(wy = waterYear(peak_dt, wy_month, TRUE))

  if(nrow(final_data) < 1){

    cli::cli_alert_warning('{usethis::ui_field(dplyr::slice(final_data, n = 1)$site_no)} ran into an error.')

  } else {

    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, n = 1)$site_no)} {usethis::ui_value("peak flows")} were successfully downloaded.')

  }

  final_data


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
    mutate(across(cols_to_update, ~ifelse(.x <= 0 , .x + 0.000001, .x)))
}


#' Get Peak Flows
#'
#' @param data data.frame
#' @param parallel whether to use future_map or not
#' @param ... other stuff to pass to future_map
#'
#' @return a df with peaks by water year
#' @noRd

adding_peaks_to_df <- function(data, parallel,wy_month, ...) {

peak_sites <- data.frame(peaks = unique(data$site_no))


if(isTRUE(parallel)){

  peaks <- peak_sites %>%
            split(.$peaks) %>%
            furrr::future_map(purrr::safely(~peaks_USGS(.$peaks, wy_month = wy_month)), ...) %>%
            purrr::keep(~length(.) != 0) %>%
            purrr::map(~.x[['result']]) %>%
            plyr::rbind.fill()

} else {

  peaks <- peak_sites %>%
            split(.$peaks) %>%
            purrr::map(purrr::safely(~peaks_USGS(.$peaks, wy_month = wy_month))) %>%
            purrr::keep(~length(.) != 0) %>%
            purrr::map(~.x[['result']]) %>%
            plyr::rbind.fill()

}


usgs_min_max_wy <-  data %>%
                    dt_to_tibble() %>%
                    dplyr::left_join(peaks, by = c("site_no", "wy")) %>%
                    dplyr::select(Station, site_no, wy, peak_va, peak_dt, dplyr::everything())
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
                   "DO", "pH", "GWL","Turb","WLBLS")

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
                   x == "Wtemp_param_00010" ~ "Wtemp",
                   x == "Precip_param_00045" ~ "Precip",
                   x == "Flow_param_00060" ~ "Flow",
                   x == "GH_param_00065" ~ "GH",
                   x == "SpecCond_param_00095" ~ "SpecCond",
                   x == "DO_param_00300" ~ "DO",
                   x == "pH_param_00400" ~ "pH",
                   x == "GWL_param_62611" ~ "GWL",
                   x == "Turb_param_63680" ~ "Turb",
                   x == "WLBLS_param_72019" ~ "WLBLS")

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
