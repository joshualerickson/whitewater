

#' Process SNOTEL daily values
#'
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#' @description This function gets daily snotel data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website.
#'
#' @return A \code{tibble} with user defined SNOTEL metrics.
#'
#'
#' @importFrom httr GET write_disk http_error
#' @export
#'

ww_dvSNOTEL <- function(sites,
                        parameterCd = 'WTEQ',
                        start_date = "",
                        end_date = "",
                        statCd = 'MEAN',
                        value_type = 'value',
                        depth = NULL,
                        wy_month = 10,
                        parallel = FALSE, ...) {


  # download meta-data
  meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),] %>%
                 dplyr::select(-c(start, end))

  meta_snotel <- meta_snotel %>%
                        mutate(
                        parameterCd = 'WTEQ',
                        start_date = "",
                        end_date = "",
                        statCd = 'MEAN',
                        value_type = 'value',
                        depth = NULL,
                        wy_month = 10)

  # check if the provided site index is valid
  if (nrow(meta_snotel) == 0){
    stop("no site found with the requested ID")
  }

  # loop over selection, and download the data

                if(isTRUE(parallel)){

                  snotel_data <- meta_snotel %>%
                                  split(.$site_id) %>%
                                  furrr::future_map(safely(~snotel_daily(.))) %>%
                                  purrr::keep(~length(.) != 0) %>%
                                  purrr::map(~.x[['result']]) %>%
                                  plyr::rbind.fill()

                } else {

                  snotel_data <- meta_snotel %>%
                                  split(.$site_id) %>%
                                  purrr::map(safely(~snotel_daily(.)))%>%
                                  purrr::keep(~length(.) != 0) %>%
                                  purrr::map(~.x[['result']])  %>%
                                  plyr::rbind.fill()
                }
  snotel_data <- snotel_data %>%
                  dt_to_tibble() %>%
                  dplyr::mutate(Date = lubridate::parse_date_time(date, orders = c("%y-%m-%d", "%y%m%d", "%y-%m-%d %H:%M")),
                         year = lubridate::year(Date),
                         month = lubridate::month(Date),
                         month_abb = factor(month.abb[month], levels = month.abb),
                         site_name = str_to_title(site_name),
                         site_name = factor(site_name))

}

#' Prep snotel daily
#'
#' @param md meta data for snotel sites
#' @noRd
#' @return a data frame with daily snotel results
snotel_daily <- function(md) {
  # download url (metric by default!)
  base_url <- paste0(
    "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/",
    md$site_id, ":",
    md$state, ":",
    md$network,
    "%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value,SNWD::value"
  )

  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "snotel_tmp.csv"),
                                      overwrite = TRUE))

  # catch error and remove resulting zero byte files
  if (httr::http_error(error)) {
    warning(sprintf("Downloading site %s failed, removed empty file.",
                    md$site_id))
  }

  # read in the snotel data
  final_data <- utils::read.table(file.path(tempdir(),"snotel_tmp.csv"),
                                  header = TRUE,
                                  sep = ",",
                                  stringsAsFactors = FALSE)

  # subsitute column names
  final_data <- snotel_wild_custom(final_data) %>% mutate(site_id = md$site_id)

  final_data <- left_join(md, final_data, by = 'site_id')

  if(nrow(final_data) < 1){

    final_data <- NULL

  } else {

    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, n = 1)$site_name)} {usethis::ui_value("daily")} was successfully downloaded.')

  }

  final_data


}
#' Water Year Stats (SNOTEL)
#' @description This function gets snotel data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website. It generates
#' the maximum and mean of snow water equivalent and snow depth per water year.
#' @param procDV A previously created \link[whitewater]{batch_SNOTELdv} object. \code{recommended}
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}. \code{optional}
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @return A \code{data.frame} with \code{mean} and \code{maximum} snow water equivalent and snow depth.
#' @export

ww_wySNOTEL <- function(procDV, sites = NULL,  parallel = FALSE, ...) {

  #error catching
  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

  if(!missing(procDV)) {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% procDV$site_id),]

  } else {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),]


  }

  # check if the provided site index is valid
  if (nrow(meta_snotel) == 0){
    stop("no site found with the requested ID")
  }

         if(isTRUE(parallel)){

                snotel_download_wy <- meta_snotel %>%
                                      split(.$site_id) %>%
                                      furrr::future_map(safely(~snotel_yearly(.))) %>%
                                      purrr::keep(~length(.) != 0) %>%
                                      purrr::map(~.x[['result']]) %>%
                                      data.table::rbindlist()

              } else {

                snotel_download_wy <- meta_snotel %>%
                                      split(.$site_id) %>%
                                      purrr::map(safely(~snotel_yearly(.)))%>%
                                      purrr::keep(~length(.) != 0) %>%
                                      purrr::map(~.x[['result']]) %>%
                                      data.table::rbindlist()

          }

  final_data <- snotel_download_wy %>% dt_to_tibble()

  final_data

}

#' Water Year & Monthly Stats (SNOTEL)
#' @description This function gets snotel data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website. It generates
#' the maximum and mean of snow water equivalent and snow depth per water year per month.
#' @param procDV A previously created \link[whitewater]{batch_SNOTELdv} object. \code{recommended}
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}. \code{optional}
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @return A \code{data.frame} with \code{mean} and \code{maximum} snow water equivalent and snow depth.
#' @export
#' @importFrom stringr str_remove str_extract str_c
#' @importFrom readr parse_number read_csv cols
#' @importFrom httr GET http_error write_disk
#' @importFrom lubridate as_date
#'

ww_wymSNOTEL <- function(procDV, sites = NULL, parallel = FALSE, ...) {

  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}
  if(!missing(procDV)) {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% procDV$site_id),]

  } else {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),]


  }

      if(isTRUE(parallel)){

        snotel_download_wym <- meta_snotel %>%
                                split(.$site_id) %>%
                                furrr::future_map(safely(~snotel_wym(.))) %>%
                                purrr::keep(~length(.) != 0) %>%
                                purrr::map(~.x[['result']]) %>%
                                data.table::rbindlist()

      } else {

        snotel_download_wym <- meta_snotel %>%
                                split(.$site_id) %>%
                                purrr::map(safely(~snotel_wym(.)))%>%
                                purrr::keep(~length(.) != 0) %>%
                                purrr::map(~.x[['result']]) %>%
                                data.table::rbindlist()

      }

    final_data <- snotel_download_wym   %>%
                  dt_to_tibble() %>%
                  mutate(year = str_extract(date, "(\\d+)"),
                         year = str_remove(year, " "),
                         month = str_extract(date, "(\\D+)"),
                         month = str_remove(month, " "),
                         day = 1,
                         Date = str_c(year,month,day, sep = "-"),
                         Date = as_date(Date),
                         wy = waterYear(Date,numeric = TRUE))


}

#' Month-Only Stats (SNOTEL)
#' @description This function uses a \link[whitewater]{batch_SNOTELdv} object to generate
#' month only statistics for snow water equivalent and snow depth.
#' @param procDV A previously created \link[whitewater]{batch_SNOTELdv} object
#'
#' @return A data.frame
#' @export
#' @importFrom dplyr group_by summarise mutate relocate
#'
#'

ww_monthSNOTEL <- function(procDV) {

  final_data <- procDV  %>%
                group_by(site_name, month_abb) %>%
                summarise(Maximum_swe = round(max(snow_water_equivalent, na.rm = TRUE),2),
                          Minimum_swe = round(min(snow_water_equivalent, na.rm = TRUE),2),
                          Mean_swe = round(mean(snow_water_equivalent, na.rm = TRUE),2),
                          Median_swe = round(median(snow_water_equivalent, na.rm = TRUE),2),
                          Maximum_snow = round(max(snow_depth, na.rm = TRUE),2),
                          Minimum_snow = round(min(snow_depth, na.rm = TRUE),2),
                          Mean_snow = round(mean(snow_depth, na.rm = TRUE),2),
                          Median_snow = round(median(snow_depth, na.rm = TRUE),2)) %>%
                ungroup()

  if(nrow(final_data) < 1){

    final_data <- NULL

  } else {

    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, n = 1)$site_name)} {usethis::ui_value("monthly")} was successfully downloaded.')

  }

  final_data
}

#' Hourly SNOTEL
#' @description This function gets hourly SNOTEL data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website.

#' @param procDV A previously created \link[whitewater]{batch_SNOTELdv} object
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}
#' @param days A \code{numeric} input of days, e.g. 1 = 24 hrs.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#' @importFrom httr GET write_disk http_error
#' @importFrom stringr str_to_title
#' @importFrom lubridate as_date ymd_hm
#' @return A data.frame with hourly SNOTEL data
#' @export

hourlySNOTEL <- function(procDV, sites = NULL,  days = 7, parallel = FALSE, ...) {

  if(length(days) > 1){stop("only length 1 vector")}
  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

  if(!missing(procDV)) {

   meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% procDV$site_id),]

                  } else {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),]


    }

  # check if the provided site index is valid
  if (nrow(meta_snotel) == 0){
    stop("no site found with the requested ID")
  }

  choice_days <- days

    if(isTRUE(parallel)){

        snotel_hourly_download <- meta_snotel %>%
                                  split(.$site_id) %>%
                                  furrr::future_map(safely(~snotel_hourly(., choice_days = choice_days))) %>%
                                  purrr::keep(~length(.) != 0) %>%
                                  purrr::map(~.x[['result']]) %>%
                                  data.table::rbindlist()

        } else {

        snotel_hourly_download <- meta_snotel %>%
                                  split(.$site_id) %>%
                                  purrr::map(safely(~snotel_hourly(., choice_days = choice_days)))%>%
                                  purrr::keep(~length(.) != 0) %>%
                                  purrr::map(~.x[['result']]) %>%
                                  data.table::rbindlist()

        }

  final_data <- snotel_hourly_download %>%
                dt_to_tibble()
}


#' Snotel Report Daily
#' @description This function gets daily SNOTEL report data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website. This includes
#' percentage of median from 1981-2010. Also included is current years data and the previous years data as well.
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}
#' @param procDV A previously created \link[whitewater]{batch_SNOTELdv} object
#' @param days A \code{numeric} input for days
#'
#' @return A \code{data.frame}
#' @export
#' @importFrom readr read_csv


reportSNOTELdv <- function(procDV, sites = NULL, days = 8) {

  if(length(days) > 1){stop("only length 1 vector")}
  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

  if(missing(procDV)) {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),]


  } else {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% procDV$site_id),]

  }

  # check if the provided site index is valid
  if (nrow(meta_snotel) == 0){
    stop("no site found with the requested ID")
  }

  snotel_report <- get_daily_snotel_report(meta_snotel, choice_days = days, type = 'daily')
  snotel_report %>% dt_to_tibble()

}

#' Snotel Report Monthly
#' @description This function gets monthly SNOTEL report data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website. This includes
#' percentage of median from 1981-2010. Also included is current years data and the previous years data as well.
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}
#' @param choice_months A numeric with how many months to go back, 12 (default).
#' @param procDV A previously created \link[whitewater]{batch_SNOTELdv} object
#'
#' @return A \code{data.frame}
#' @export
#' @importFrom readr read_csv


reportSNOTELmv <- function(procDV, choice_months = 12, sites = NULL) {

  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

  if(missing(procDV)) {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),]


  } else {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% procDV$site_id),]

  }

  # check if the provided site index is valid
  if (nrow(meta_snotel) == 0){
    stop("no site found with the requested ID")
  }

  snotel_report_month <- get_daily_snotel_report(meta_snotel, choice_days = choice_months, type = 'monthly')

  snotel_report_month <- snotel_report_month  %>%
                         dt_to_tibble() %>%
                         mutate(year = str_extract(date, "(\\d+)"),
                                 year = str_remove(year, " "),
                                 month = str_extract(date, "(\\D+)"),
                                 month = str_remove(month, " "),
                                 day = 1,
                                 Date = str_c(year,month,day, sep = "-"),
                                 Date = as_date(Date),
                                 wy = waterYear(Date,numeric = TRUE))

  return(snotel_report_month)

}




#' Prep SNOTEL Yearly
#'
#' @param md data.frame
#'
#' @return a data.frame with yearly stats
snotel_yearly <- function(md){
  # download url (metric by default!)
  base_url <- paste0(
    "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/annual_water_year/start_of_period/",
    md$site_id, ":",
    md$state, ":",
    md$network,
    '%7Cid=""%7Cname/POR_BEGIN,POR_END/WTEQ::value:daily%20MEAN,WTEQ::value:daily%20MAX,SNWD::value:daily%20MEAN,SNWD::value:daily%20MAX'
  )

  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "snotel_tmp.csv"),
                                      overwrite = TRUE))

  # read in the snotel data
  df <- readr::read_csv(file.path(tempdir(),"snotel_tmp.csv"),comment = "#", col_types = readr::cols())

  # subsitute column names
  # define new column names
  snotel_columns <- c(
    "wy",
    "swe_mean",
    "swe_max",
    "snow_mean",
    "snow_max"
  )

  # rename columns
  colnames(df) <- snotel_columns

  df <- df %>% dplyr::mutate(site_id = md$site_id)


  final_data <- dplyr::left_join(md, df, by = 'site_id')

  if(nrow(final_data) < 1){

    final_data <- NULL

  } else {

    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, n = 1)$site_name)} {usethis::ui_value("yearly")} was successfully downloaded.')

  }

  final_data

}


#' Change colnames snotel
#'
#' @param df A data.frame
#' @description hijacked 'snotelr' package functions and manipulated
#' @return a function for changing colnames
#'
#'
snotel_wild_custom <- function(df) {

  # check if it's a dataframe
  df_check <- is.data.frame(df)

  if (!df_check) {
    stop("Not a valid (SNOTEL) data frame...")
  }

  # check the file, if less than 8 columns
  # are present this is not a standard file,
  # stop processing
  if (ncol(df) != 8) {
    stop("not a standard snotel file")
  }

  # define new column names
  snotel_columns <- c(
    "date",
    "snow_water_equivalent",
    "precipitation_cumulative",
    "temperature_max",
    "temperature_min",
    "temperature_mean",
    "precipitation",
    "snow_depth"
  )

  # rename columns
  colnames(df) <- snotel_columns


  # return data frame
  return(df)
}

#' Change colnames snotel report
#'
#' @param df A data frame
#' @description hijacked 'snotelr' package functions and manipulated
#' @return a function for changing colnames for report
#'
#'
snotel_report_custom <- function(df) {

  # check if it's a dataframe
  df_check <- is.data.frame(df)

  if (!df_check) {
    stop("Not a valid (SNOTEL) data frame...")
  }

  # check the file, if less than 6 columns
  # are present this is not a standard file,
  # stop processing
  if (ncol(df) != 13) {
    stop("not a standard snotel file")
  }

  # define new column names
  snotel_columns <- c(
    "date",
    "snow_depth_percent_median_1991",
    "snow_depth_percent_median_1981",
    "swe_percent_median_1991",
    "swe_percent_median_1981",
    "prec_percent_median_1991",
    "prec_percent_median_1981",
    "swe_current",
    "swe_prev_year",
    "snow_current",
    "snow_prev_year",
    "prec_current",
    "prec_prev_year"


  )

  # rename columns
  colnames(df) <- snotel_columns


  # return data frame
  return(df)
}

#' Prepping SNOTEL hourly
#'
#' @param md data.frame
#' @param choice_days numeric
#'
#' @return a data.frame with site hourly values
snotel_hourly <- function(md, choice_days){
  # download url (metric by default!)
  base_url <- paste0(
    "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/hourly/start_of_period/",
    md$site_id, ":",
    md$state, ":",
    md$network,
    "%7Cid=\"\"%7Cname/-",round(choice_days*24,0),",0/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value,SNWD::value"
  )

  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "snotel_tmp.csv"),
                                      overwrite = TRUE))

  # read in the snotel data
  df <- utils::read.table(file.path(tempdir(),"snotel_tmp.csv"),
                          header = TRUE,
                          sep = ",",
                          stringsAsFactors = FALSE)

  # subsitute column names
  df <- snotel_wild_custom(df) %>% dplyr::mutate(site_id = md$site_id)

  final_data <- dplyr::left_join(md,df, by = 'site_id')

  if(nrow(final_data) < 1){

    final_data <- NULL

  } else {

    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, n = 1)$site_name)} {usethis::ui_value("hourly")} was successfully downloaded.')

  }

  final_data
}

#' Get SNOTEL reports
#'
#' @param data previously created api call
#' @param choice_days how much time to go back
#' @param type whether 'daily' or 'monthly'
#'
#' @return a data.frame with values from SNOTEL report generator.
get_daily_snotel_report <- function(data, choice_days, type) {

  snotel_report <- data.frame()

  #run for loop over api, pulling necessary data.
  for (i in 1:nrow(data)){
    # loop over selection, and download the data

    tryCatch({

      # download url (metric by default!)

      base_url <- switch(type,
                         'daily' = paste0(
        "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/daily/start_of_period/",
        data$site_id[i], ":",
        data$state[i], ":",
        data$network[i],
        "%7Cid=%22%22%7Cname/-",choice_days,
        ",0/","SNWD::pctOfMedian_1991,",
        "SNWD::pctOfMedian_1981,",
        "WTEQ::pctOfMedian_1991,",
        "WTEQ::pctOfMedian_1981,",
        "PREC::pctOfMedian_1991,",
        "PREC::pctOfMedian_1981,",
        "WTEQ::value,",
        "WTEQ::prevValue,",
        "SNWD::value,",
        "SNWD::prevValue,",
        "PREC::value,",
        "PREC::prevValue"
      ),
      'monthly' = paste0(
        "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/monthly/start_of_period/",
        data$site_id[i], ":",
        data$state[i], ":",
        data$network[i],
        "%7Cid=%22%22%7Cname/-",choice_days,
        ",0/","SNWD::pctOfMedian_1991,",
        "SNWD::pctOfMedian_1981,",
        "WTEQ::pctOfMedian_1991,",
        "WTEQ::pctOfMedian_1981,",
        "PREC::pctOfMedian_1991,",
        "PREC::pctOfMedian_1981,",
        "WTEQ::value,",
        "WTEQ::prevValue,",
        "SNWD::value,",
        "SNWD::prevValue,",
        "PREC::value,",
        "PREC::prevValue"
      )
      )

      # try to download the data
      error <- httr::GET(url = base_url,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "snotel_tmp.csv"),
                                          overwrite = TRUE))

      # read in the snotel data
      df <- readr::read_csv(file.path(tempdir(),"snotel_tmp.csv"),skip = 69, col_types = cols())

      # substitute column names
      df <- snotel_report_custom(df)

      df <- cbind.data.frame(data[i,c(3,4,11)], df, row.names = NULL)

      #combine df with blank dataframe (usgs_download_hourly)

      snotel_report <- dplyr::bind_rows(snotel_report, df)

      if(nrow(snotel_report) < 1){

        cli::cli_alert_warning('{usethis::ui_field(dplyr::slice(df, n = 1)$site_name)} ran into an error.')

        snotel_report <- NULL

      } else {

        cli::cli_alert_success('{usethis::ui_field(dplyr::slice(df, n = 1)$site_name)} {usethis::ui_value(type)} report was successfully downloaded.')

      }

    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }

  snotel_report

}

#' Prep SNOTEL wym
#'
#' @param md data.frame
#'
#' @return returns a data.frame with water year and month stats from report generator online
snotel_wym <- function(md){
  # download url (metric by default!)
  base_url <- paste0(
    "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/monthly/start_of_period/",
    md$site_id, ":",
    md$state, ":",
    md$network,
    '%7Cid=""%7Cname/POR_BEGIN,POR_END/WTEQ::value:daily%20MEAN,WTEQ::value:daily%20MAX,SNWD::value:daily%20MEAN,SNWD::value:daily%20MAX'
  )

  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "snotel_tmp.csv"),
                                      overwrite = TRUE))

  # read in the snotel data
  df <- readr::read_csv(file.path(tempdir(),"snotel_tmp.csv"),comment = "#", col_types = readr::cols())

  # subsitute column names
  # define new column names
  snotel_columns <- c(
    "date",
    "swe_mean",
    "swe_max",
    "snow_mean",
    "snow_max"
  )

  # rename columns
  colnames(df) <- snotel_columns

  df <- df %>% dplyr::mutate(site_id = md$site_id)

  final_data <- left_join(md, df, by = 'site_id')

  if(nrow(final_data) < 1){

    final_data <- NULL

  } else {

    cli::cli_alert_success('{usethis::ui_field(dplyr::slice(final_data, n = 1)$site_name)} {usethis::ui_value("water year and month")} was successfully downloaded.')

  }

  final_data


}


#' Construct NWIS url for data retrieval
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{https://nwis.waterdata.usgs.gov/nwis/qwdata}
#' A list of parameter codes can be found here: \url{https://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{https://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param sites string or vector of strings USGS site number.  This is usually an 8 digit number
#' @param service string NRCS service to call. Possible values are "dv" (daily values), "hv" (hourly values),
#'  "mv" (monthly values), "wy" (annual water year values), "cy" (calendar year values), and
#'  "smv" (semi-monthly values).
#' @param parameterCd string or vector of NRCS parameter code.
#'   This is usually an 3-4 digit character, ex. 'WTEQ' = snow water equivalent.
#' @param start_date character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param end_date character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @param statCd string or vector USGS statistic code only used for daily value service. This is usually 5 digits.  Daily mean (00003) is the default.
#' @param format string, can be "tsv" or "xml", and is only applicable for daily and unit value requests.  "tsv" returns results faster, but there is a possiblitiy that an incomplete file is returned without warning. XML is slower,
#' but will offer a warning if the file was incomplete (for example, if there was a momentary problem with the internet connection). It is possible to safely use the "tsv" option,
#' but the user must carefully check the results to see if the data returns matches what is expected. The default is therefore "xml".
#' @param expanded logical defaults to \code{TRUE}. If \code{TRUE}, retrieves additional information, only applicable for qw data.
#' @param ratingType can be "base", "corr", or "exsa". Only applies to rating curve data.
#' @param statReportType character Only used for statistics service requests.  Time division for statistics: daily, monthly, or annual.  Default is daily.
#' Note that daily provides statistics for each calendar day over the specified range of water years, i.e. no more than 366
#' data points will be returned for each site/parameter.  Use readNWISdata or readNWISdv for daily averages.
#' Also note that 'annual' returns statistics for the calendar year.  Use readNWISdata for water years. Monthly and yearly
#' provide statistics for each month and year within the range individually.
#' @param statType character Only used for statistics service requests. Type(s) of statistics to output for daily values.  Default is mean, which is the only
#' option for monthly and yearly report types. See the statistics service documentation
#' at \url{https://waterservices.usgs.gov/rest/Statistics-Service.html} for a full list of codes.
#' @keywords data import USGS web service
#' @return url string
#' @export
#' @examples
#' site_id <- '01594440'
#' startDate <- '1985-01-01'
#' endDate <- ''
#' pCode <- c("00060","00010")
#' url_daily <- constructNWISURL(site_id,pCode,
#'            startDate,endDate,'dv',statCd=c("00003","00001"))
#' url_unit <- constructNWISURL(site_id,pCode,"2012-06-28","2012-06-30",'iv')
#'
#' url_qw_single <- constructNWISURL(site_id,"01075",startDate,endDate,'qw')
#' url_qw <- constructNWISURL(site_id,c('01075','00029','00453'),
#'            startDate,endDate,'qw')
#' url_daily_tsv <- constructNWISURL(site_id,pCode,startDate,endDate,'dv',
#'            statCd=c("00003","00001"),format="tsv")
#' url_rating <- constructNWISURL(site_id,service="rating",ratingType="base")
#' url_peak <- constructNWISURL(site_id, service="peak")
#' url_meas <- constructNWISURL(site_id, service="meas")
#' urlQW <- constructNWISURL("450456092225801","70300",startDate="",endDate="","qw",expanded=TRUE)
constructSNOTELURL <- function(sites,
                               service,
                               parameterCd = 'WTEQ',
                               start_date = "",
                               end_date = "",
                               statCd = 'MEAN',
                               value_type = 'value',
                               depth = NUL){

  service <- match.arg(service, c("dv","hv", 'mv', 'wy', 'cy', 'smv'))

  service[service == "dv"] <- "daily"
  service[service == "hv"] <- "hourly"
  service[service == "mv"] <- "monthly"
  service[service == "wy"] <- "annual_water_year"
  service[service == "cy"] <- "annual_calendar_year"
  service[service == "smv"] <- "semimonthly"

  if(any(!is.na(parameterCd) & parameterCd != "all")){
    pcodeCheck <- all(nchar(parameterCd) == 5) & all(!is.na(suppressWarnings(as.numeric(parameterCd))))

    if(!pcodeCheck){
      badIndex <- which(nchar(parameterCd) != 5 | is.na(suppressWarnings(as.numeric(parameterCd))))
      stop("The following pCodes appear mistyped:",paste(parameterCd[badIndex],collapse=","))
    }

    if(length(parameterCd) > 200){
      stop("Maximum parameter codes allowed is 200, please adjust data request.")
    }
  }

  multipleSites <- length(siteNumbers) > 1

  siteNumbers <- paste(siteNumbers, collapse=",")

  baseURL <- drURL(service, Access=pkg.env$access)

  switch(service,
         dv = {
           if(multipleSites){
             searchCriteria <- "multiple_site_no"
             url <- appendDrURL(baseURL,multiple_site_no=siteNumbers)
           } else {
             searchCriteria <- "search_site_no"
             url <- appendDrURL(baseURL,
                                search_site_no=siteNumbers,
                                search_site_no_match_type="exact")
           }

           multiplePcodes <- length(parameterCd)>1

           if(multiplePcodes){
             pCodes <- paste(parameterCd, collapse=",")
             url <- appendDrURL(url,multiple_parameter_cds=pCodes,param_cd_operator="OR")
           } else {
             url <- appendDrURL(url,multiple_parameter_cds=parameterCd,param_cd_operator="AND")
           }

           searchCriteria <- paste(searchCriteria, "multiple_parameter_cds", sep=",")
           url <- appendDrURL(url, list_of_search_criteria = searchCriteria)


           url <- paste(url, "group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd",
                        "column_name=site_no&column_name=station_nm&inventory_output=0&rdb_inventory_output=file",
                        "TZoutput=0&pm_cd_compare=Greater%20than&radio_parm_cds=previous_parm_cds&qw_attributes=0",
                        "format=rdb&rdb_qw_attributes=0&date_format=YYYY-MM-DD",
                        "rdb_compression=value", sep = "&")
           if(expanded){
             url <- appendDrURL(url,qw_sample_wide="0")
             url <- gsub("rdb_qw_attributes=0","rdb_qw_attributes=expanded",url)
           } else {
             url <- appendDrURL(url,qw_sample_wide="separated_wide")
           }

           if (nzchar(startDate)) {
             url <- appendDrURL(url,begin_date=startDate)
           }

           if (nzchar(endDate)) {
             url <- appendDrURL(url,end_date=endDate)
           }
         },
         rating = {
           ratingType <- match.arg(ratingType, c("base", "corr", "exsa"))
           url <- appendDrURL(baseURL, site_no=siteNumbers,file_type=ratingType)
         },
         peak = {
           url <- appendDrURL(baseURL, site_no=siteNumbers,
                              range_selection="date_range",
                              format="rdb")
           if (nzchar(startDate)) {
             url <- appendDrURL(url,begin_date=startDate)
           }
           if(nzchar(endDate)){
             url <- appendDrURL(url, end_date=endDate)
           }
         },
         measurements = {
           url <- appendDrURL(baseURL, site_no=siteNumbers,
                              range_selection="date_range")
           if (nzchar(startDate)) {
             url <- appendDrURL(url,begin_date=startDate)
           }
           if(nzchar(endDate)){
             url <- appendDrURL(url, end_date=endDate)
           }
           if(expanded){
             url <- appendDrURL(url,format="rdb_expanded")
           } else {
             url <- appendDrURL(url,format="rdb")
           }

         },
         stat = { #for statistics service

           message("Please be aware the NWIS data service feeding this function is in BETA.\n
          Data formatting could be changed at any time, and is not guaranteed")

           #make sure only statTypes allowed for the statReportType are being requested
           if(!grepl("(?i)daily",statReportType) && !all(grepl("(?i)mean",statType)) && !all(grepl("(?i)all",statType))){
             stop("Monthly and annual report types can only provide means")
           }

           #make sure dates aren't too specific for statReportType
           if(grepl("(?i)monthly",statReportType) && (length(unlist(gregexpr("-",startDate))) > 1
                                                      || length(unlist(gregexpr("-",endDate))) > 1)){
             stop("Start and end dates for monthly statReportType can only include months and years")
           }
           if(grepl("(?i)annual",statReportType) && (grepl("-",startDate) || grepl("-",endDate))){
             stop("Start and end dates for annual statReportType can only include years")
           }
           statType <- paste(statType,collapse=",")
           parameterCd <- paste(parameterCd,collapse=",")
           url <- appendDrURL(baseURL, sites=siteNumbers,
                              statType=statType,
                              statReportType=statReportType,
                              parameterCd=parameterCd)
           if (nzchar(startDate)) {
             url <- appendDrURL(url,startDT=startDate)
           }
           if (nzchar(endDate)) {
             url <- appendDrURL(url,endDT=endDate)
           }
           if (!grepl("(?i)daily",statReportType)){
             url <- appendDrURL(url,missingData="off")
           }

         },

         { # this will be either dv, uv, groundwater
           multiplePcodes <- length(parameterCd)>1
           # Check for 5 digit parameter code:
           if(multiplePcodes){
             parameterCd <- paste(parameterCd, collapse=",")
           }

           format <- match.arg(format, c("xml","tsv","wml1","wml2","rdb"))

           formatURL <- switch(format,
                               xml = {
                                 if ("gwlevels" == service) {
                                   "waterml"
                                 } else {
                                   "waterml,1.1"
                                 }
                               },
                               rdb = {
                                 if("gwlevels" == service){
                                   "rdb,3.0"
                                 } else {
                                   "rdb,1.0"
                                 }
                               },
                               tsv = {
                                 if("gwlevels" == service){
                                   "rdb"
                                 } else {
                                   "rdb,1.0"
                                 }
                               },
                               wml2 = "waterml,2.0",
                               wml1 = {
                                 if ("gwlevels" == service) {
                                   "waterml"
                                 } else {
                                   "waterml,1.1"
                                 }
                               }
           )

           url <- appendDrURL(baseURL, site=siteNumbers, format=formatURL)

           if(!is.na(parameterCd)){
             url <- appendDrURL(url, ParameterCd=parameterCd)
           }

           if("dv"==service) {
             if(length(statCd) > 1){
               statCd <- paste(statCd, collapse=",")
             }
             url <- appendDrURL(url, StatCd=statCd)
           }

           if (nzchar(startDate)) {
             url <- appendDrURL(url, startDT=startDate)
           } else {
             startorgin <- "1851-01-01"
             if ("iv" == service) startorgin <- "1900-01-01"
             url <- appendDrURL(url, startDT=startorgin)
           }

           if (nzchar(endDate)) {
             url <- appendDrURL(url, endDT=endDate)
           }
         }

  )

  return(url)
}




