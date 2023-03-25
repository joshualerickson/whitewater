testthat::skip_on_cran()

test_that("usgs flow", {

yaak_dv <- ww_dvUSGS('12304500')

expect_error(expect_equal(yaak_dv[1,]$site_no, '1204500'))

expect_equal(yaak_dv[1,]$site_no, '12304500')

yaak_wy <- ww_wyUSGS(yaak_dv)

expect_equal(yaak_wy %>%
               dplyr::filter(wy == 1967) %>%
               dplyr::pull(peak_va), 10200)

yaak_wym <- ww_wymUSGS(yaak_dv)

expect_equal(yaak_wym %>%
               dplyr::filter(wy == 1982) %>%
               nrow(), 12
               )
yaak_month <- ww_monthUSGS(yaak_dv)

expect_equal(nrow(yaak_month), 12)


yaak_reportdv <- ww_reportUSGSdv(yaak_dv, days = 11)

expect_equal(nrow(yaak_reportdv), 12)

yaak_reportmv <- ww_reportUSGSmv(yaak_dv)

expect_equal(yaak_reportmv %>%
               dplyr::filter(year_nu == 2021, month == 6) %>%
               dplyr::pull(mean_value), 846.5)

})

test_that("usgs water temperature", {

  withlacoochee_dv <- ww_dvUSGS(sites="02319394",
                          parameter_cd = c("00010"))

  expect_error(expect_equal(withlacoochee_dv[1,]$site_no, '1204500'))

  expect_equal(withlacoochee_dv[1,]$site_no, '02319394')

  withlacoochee_wy <- ww_wyUSGS(withlacoochee_dv)

  expect_equal(withlacoochee_wy %>%
                 dplyr::filter(wy == 2017) %>%
                 dplyr::pull(Wtemp_max), 27.5)

  withlacoochee_wym <- ww_wymUSGS(withlacoochee_dv)

  expect_equal(withlacoochee_wym %>%
                 dplyr::filter(wy == 2019) %>%
                 nrow(), 12
  )
  withlacoochee_month <-ww_monthUSGS(withlacoochee_dv)

  expect_equal(nrow(withlacoochee_month), 12)


  withlacoochee_reportdv <- ww_reportUSGSdv(withlacoochee_dv, days = 11)

  expect_equal(nrow(withlacoochee_reportdv), 12)

  withlacoochee_reportmv <- ww_reportUSGSmv(withlacoochee_dv)

  expect_equal(withlacoochee_reportmv %>%
                      dplyr::filter(year_nu == 2021,
                                    month == 5) %>%
                      dplyr::pull(mean_value), 22.25)


})


test_that("two sites two parameters", {

  usgs_dv <- purrr::quietly(ww_dvUSGS)

  usgs_dv <- usgs_dv(sites=c("02319394", "12304500"),
                     parameter_cd = c("00010", "00060"))

  expect_error(expect_equal(usgs_dv$result[1,]$site_no, '1204500'))

  expect_equal(usgs_dv$result[1,]$site_no, '02319394')

  wy_usgs <- purrr::quietly(ww_wyUSGS)

  usgs_wy <- wy_usgs(usgs_dv$result)

  expect_equal(usgs_wy$result %>%
                 dplyr::filter(wy == 2017) %>%
                 dplyr::pull(Wtemp_max) %>%
                   length(), 2)

  wym_usgs <- purrr::quietly(ww_wymUSGS)

  usgs_wym <- wym_usgs(usgs_dv$result)

  expect_equal(usgs_wym$result %>%
                 dplyr::filter(wy == 2019) %>%
                 nrow(), 24
  )
  month_usgs <- purrr::quietly(ww_monthUSGS)

  usgs_month <- month_usgs(usgs_dv$result)

  expect_equal(nrow(usgs_month$result), 24)


})


test_that('two sites two params reports', {

  usgs_dv <- purrr::quietly(ww_dvUSGS)

  usgs_dv <- usgs_dv(sites=c("02319394", "12304500"),
                     parameter_cd = c("00010", "00060"))

  usgs_report <- purrr::quietly(ww_reportUSGSdv)
  usgs_reportdv <- usgs_report(usgs_dv$result, days = 11)

  expect_equal(nrow(usgs_reportdv$result), 36)

  usgs_report2 <- usgs_report(sites=c("12304500", "02319394"),
                                  parameter_cd = c("00010", "00060"),
                                  days = 11)

  expect_equal(nrow(usgs_report2$result), 36)

  usgs_report <- purrr::quietly(ww_reportUSGSmv)

  usgs_reportmv <- usgs_report(sites=c("12304500", "02319394"),
                                   parameter_cd = c("00010", "00060"))

  expect_equal(usgs_reportmv$result %>%
                 dplyr::filter(year_nu == 2021,
                               parameter_cd == '00010',
                               month == 5) %>%
                 dplyr::pull(mean_value), 22.25)

})


test_that('two sites two params reports using main function', {

  usgs_dv <- purrr::quietly(ww_dvUSGS)

  usgs_dv <- usgs_dv(sites=c("02319394", "12304500"),
                     parameter_cd = c("00010", "00060"))

  usgs_report <- purrr::quietly(ww_statsUSGS)

  #daily
  usgs_reportdv <- usgs_report(usgs_dv$result, days = 11)

  expect_equal(nrow(usgs_reportdv$result), 36)

  #monthly
  usgs_reportmv <- usgs_report(usgs_dv$result,
                               temporalFilter = 'monthly')

  expect_true(nrow(usgs_reportmv$result) > 0)

  #yearly
  usgs_reportyv <- usgs_report(usgs_dv$result,
                               temporalFilter = 'yearly')

  expect_true(nrow(usgs_reportyv$result) > 0)

})

test_that("water year", {

  yaak_dv <- ww_dvUSGS('12304500', wy_month = 6)

  expect_equal(yaak_dv %>%
    dplyr::filter(month %in% c(5,6),
                  year == 1956) %>%
    dplyr::group_by(month) %>%
    dplyr::slice(1) %>%
    dplyr::pull(wy),
    c(1956,1957)
  )


  yaak_dv <- ww_dvUSGS('12304500', wy_month = 4)

  expect_equal(yaak_dv %>%
                 dplyr::filter(month %in% c(3,4),
                               year == 1956) %>%
                 dplyr::group_by(month) %>%
                 dplyr::slice(1) %>%
                 dplyr::pull(wy),
               c(1956,1957)
  )

})


test_that('wwOptions, ww_floorIVUSGS, ww_instantaneousUSGS', {

  yaak_river_dv <- ww_dvUSGS('12304500', parameter_cd = c('00010','00060'))

  yaak_river_iv <- ww_floorIVUSGS(yaak_river_dv,
                                  options = wwOptions(date_range = 'date_range',
                                  dates = c('2021-08-01', '2021-08-11')), verbose = F)
  expect_equal(nrow(yaak_river_iv), 241)

  yaak_river_iv <- ww_floorIVUSGS(yaak_river_dv,
                                  options = wwOptions(date_range = 'date_range',
                                                      dates = c('2021-08-01', '2021-08-11')))
  expect_equal(nrow(yaak_river_iv), 241)

  #change the floor value

  yaak_river_dv <- ww_dvUSGS('12304500', parameter_cd = c('00060'))
  yaak_river_iv <- ww_floorIVUSGS(yaak_river_dv,
                                  options = wwOptions(date_range = 'date_range',
                                                      dates = c('2021-08-01', '2021-08-11'),
                                                      floor_iv = '2-hour'))
  expect_equal(nrow(yaak_river_iv), 121)

  #get recent

  yaak_river_iv <- ww_floorIVUSGS(yaak_river_dv,
                                  options = wwOptions(date_range = 'recent'))

  expect_equal(nrow(yaak_river_iv), 1)


  yaak_river_dv <- ww_dvUSGS('12304500', parameter_cd = c('00060', '00010'))


  yaak_river_iv <- ww_floorIVUSGS(yaak_river_dv,
                                  options = wwOptions(date_range = 'recent'))

  expect_equal(nrow(yaak_river_iv), 1)

  # should only return one since date_range is 'recent'

  yaak_river_iv <- ww_instantaneousUSGS(yaak_river_dv,
                                        options = wwOptions(date_range = 'recent',
                                                            dates = c('2022-03-01', '2022-05-11'),
                                                            floor_iv = '2-hour'), verbose = F)

  expect_equal(nrow(yaak_river_iv), 1)

  yaak_river_iv <- ww_instantaneousUSGS(yaak_river_dv,
                                        options = wwOptions(date_range = 'recent',
                                                            dates = c('2022-03-01', '2022-05-11'),
                                                            floor_iv = '2-hour'), verbose = T)

  expect_equal(nrow(yaak_river_iv), 1)


  ## checking date column works

  yaak_river_iv <- ww_instantaneousUSGS(yaak_river_dv)


  expect_equal(class(yaak_river_iv$date)[1], 'POSIXct')
})



test_that("usgs flow with verbose off", {

  yaak_dv <- ww_dvUSGS('12304500', verbose = F)

  expect_error(expect_equal(yaak_dv[1,]$site_no, '1204500'))

  expect_equal(yaak_dv[1,]$site_no, '12304500')

  yaak_wy <- ww_wyUSGS(yaak_dv, verbose = F)

  expect_equal(yaak_wy %>%
                 dplyr::filter(wy == 1967) %>%
                 dplyr::pull(peak_va), 10200)

  yaak_wym <- ww_wymUSGS(yaak_dv, verbose = F)

  expect_equal(yaak_wym %>%
                 dplyr::filter(wy == 1982) %>%
                 nrow(), 12
  )
  yaak_month <- ww_monthUSGS(yaak_dv, verbose = F)

  expect_equal(nrow(yaak_month), 12)


  yaak_reportdv <- ww_reportUSGSdv(yaak_dv, days = 11, verbose = F)

  expect_equal(nrow(yaak_reportdv), 12)

  yaak_reportmv <- ww_reportUSGSmv(yaak_dv, verbose = F)

  expect_equal(yaak_reportmv %>%
                 dplyr::filter(year_nu == 2021, month == 6) %>%
                 dplyr::pull(mean_value), 846.5)

})



test_that("usgs peaks", {

  yaak_peak <- ww_peakUSGS(sites = '12304500', parallel = F)

  expect_equal(yaak_peak[1,]$wy, 1948)
})


test_that('current conditions', {

  curcond <- ww_current_conditions()

  expect_gt(nrow(curcond), 0)
})


