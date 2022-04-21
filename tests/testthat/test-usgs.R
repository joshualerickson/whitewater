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
                          parameterCd = c("00010"))

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
                     parameterCd = c("00010", "00060"))

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
                     parameterCd = c("00010", "00060"))

  usgs_report <- purrr::quietly(ww_reportUSGSdv)
  usgs_reportdv <- usgs_report(usgs_dv$result, days = 11)

  expect_equal(nrow(usgs_reportdv$result), 36)

  usgs_report2 <- usgs_report(sites=c("12304500", "02319394"),
                                  parameterCd = c("00010", "00060"),
                                  days = 11)

  expect_equal(nrow(usgs_report2$result), 36)

  usgs_report <- purrr::quietly(ww_reportUSGSmv)

  usgs_reportmv <- usgs_report(sites=c("12304500", "02319394"),
                                   parameterCd = c("00010", "00060"))

  expect_equal(usgs_reportmv$result %>%
                 dplyr::filter(year_nu == 2021,
                               parameter_cd == '00010',
                               month == 5) %>%
                 dplyr::pull(mean_value), 22.25)

})


test_that('two sites two params reports using main function', {

  usgs_dv <- purrr::quietly(ww_dvUSGS)

  usgs_dv <- usgs_dv(sites=c("02319394", "12304500"),
                     parameterCd = c("00010", "00060"))

  usgs_report <- purrr::quietly(ww_statsNWIS)

  #daily
  usgs_reportdv <- usgs_report(usgs_dv$result, days = 11)

  expect_equal(nrow(usgs_reportdv$result), 36)

  #monthly
  usgs_reportmv <- usgs_report(usgs_dv$result,
                               temporalFilter = 'monthly')

  expect_equal(nrow(usgs_reportmv$result), 1061)

  #yearly
  usgs_reportyv <- usgs_report(usgs_dv$result,
                               temporalFilter = 'yearly')

  expect_equal(nrow(usgs_reportyv$result), 82)

})


