

#' Get Current Conditions
#'
#' @return a \code{tibble} with current conditions and attributes from USGS dashboard.
#'
#' @note The time zone used in the URL call is the R session time zone. Also, the time is 1-hour behind.
#' Here are the attributes that are with the data.frame: AgencyCode,SiteNumber,SiteName,SiteTypeCode,Latitude,Longitude,
#' CurrentConditionID,ParameterCode,TimeLocal,TimeZoneCode,Value,
#' ValueFlagCode,RateOfChangeUnitPerHour,StatisticStatusCode,FloodStageStatusCode.
#' @export
#'
#' @examples \dontrun{
#'
#' current_conditions <- ww_current_conditions()
#'
#' }
#'
ww_current_conditions <- function(){

  user_date <- lubridate::as_date(Sys.time())
  user_time <- format(Sys.time()-3600, "%H:%M:%S")

  ids <- paste0("https://dashboard.waterdata.usgs.gov/service/cwis/1.0/odata/CurrentConditions?$top=15000&$filter=(UpdatedUtc%20gt%20",
                user_date,"T",user_time,".190Z)%20and%20(AccessLevelCode%20eq%20%27P%27)%20and%20(1%20eq%201%20and%20true)%20and%20(SiteTypeCode%20in%20(%27ST%27,%27ST-TS%27,%27ST-CA%27,%27ST-DCH%27))%20and%20(ParameterCode%20in%20(%2730208%27,%2730209%27,%2750042%27,%2750050%27,%2750051%27,%2772137%27,%2772138%27,%2772139%27,%2772177%27,%2772243%27,%2774072%27,%2781395%27,%2799060%27,%2799061%27,%2700056%27,%2700058%27,%2700059%27,%2700060%27,%2700061%27))&$select=AgencyCode,SiteNumber,SiteName,SiteTypeCode,Latitude,Longitude,CurrentConditionID,ParameterCode,TimeLocal,TimeZoneCode,Value,ValueFlagCode,RateOfChangeUnitPerHour,StatisticStatusCode,FloodStageStatusCode&$orderby=SiteNumber,AgencyCode,ParameterCode,TimeLocal%20desc&caller=National%20Water%20Dashboard%20default")

  error_ids <- httr::GET(url = ids,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "nld_tmp.json"),overwrite = TRUE))

  status_current <- jsonlite::fromJSON(file.path(tempdir(),"nld_tmp.json"))$value %>%
    tibble()

  status_current <- status_current %>%
    dplyr::mutate(
      StatisticsStatusDescription = dplyr::case_when(
        StatisticStatusCode == "P0" ~'All-time low for this day',
        StatisticStatusCode == "P0_10"~"Much below normal",
        StatisticStatusCode == "P10_25"~"Below normal",
        StatisticStatusCode == "P25_75"~"Normal",
        StatisticStatusCode == "P75_90"~"Above normal",
        StatisticStatusCode == "P90_100"~"Much above normal",
        StatisticStatusCode == "P100" ~ 'All-time high for this day',
        StatisticStatusCode == "NR_0FLOW"~"Not flowing",
        StatisticStatusCode == "NR_REVFLOW"~"Not ranked",
        StatisticStatusCode == "NR_NOMEAS"~"Measurement flag",
        !is.na(ValueFlagCode) & is.na(StatisticStatusCode) ~ "Measurement flag",
        TRUE~"Not ranked"
      ),
      StatisticsStatusDescription = factor(StatisticsStatusDescription,
                                           levels = c("Not ranked",
                                                      "Measurement flag",
                                                      "Not flowing",
                                                      "All-time low for this day",
                                                      "Much below normal",
                                                      "Below normal",
                                                      "Normal",
                                                      "Above normal",
                                                      "Much above normal",
                                                      "All-time high for this day")),
      StatisticsStatusColorFill = dplyr::case_when(
        StatisticsStatusDescription == "Not ranked" ~ '#999999',
        StatisticsStatusDescription == "Measurement flag" ~ '#989898',
        StatisticsStatusDescription == "Not flowing" ~ '#a9a9a9',
        StatisticsStatusDescription == "All-time low for this day" ~ "#FF0000",
        StatisticsStatusDescription == "Much below normal" ~ "#BB2222",
        StatisticsStatusDescription == "Below normal" ~ "#FFAA00",
        StatisticsStatusDescription == "Normal" ~ "#00ff00",
        StatisticsStatusDescription == "Above normal" ~ "#44dddd",
        StatisticsStatusDescription == "Much above normal" ~ "#0000FF",
        StatisticsStatusDescription == "All-time high for this day" ~ "#000055",
        TRUE ~ NA_character_

      ),
      StatisticsStatusColorStroke = dplyr::case_when(
        StatisticsStatusDescription == "Not ranked" ~ '#666666',
        StatisticsStatusDescription == "Measurement flag" ~ '#996633',
        StatisticsStatusDescription == "Not flowing" ~ '#997700',
        StatisticsStatusDescription == "All-time low for this day" ~ "#990000",
        StatisticsStatusDescription == "Much below normal" ~ "#661111",
        StatisticsStatusDescription == "Below normal" ~ "#996600",
        StatisticsStatusDescription == "Normal" ~ "#009900",
        StatisticsStatusDescription == "Above normal" ~ "#11aaaa",
        StatisticsStatusDescription == "Much above normal" ~ "#000099",
        StatisticsStatusDescription == "All-time high for this day" ~ "#000000",
        TRUE ~ NA_character_
      )
    )
}

