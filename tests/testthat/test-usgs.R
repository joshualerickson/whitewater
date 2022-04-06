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


yaak_report <- ww_reportUSGSdv(yaak_dv, days = 11)

expect_equal(nrow(yaak_report), 12)

yaak_report <- ww_reportUSGSmv(yaak_dv, days = 11)

expect_equal(nrow(yaak_report), 12)
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


  withlacoochee_report <- ww_reportUSGSdv(withlacoochee_dv, days = 11)

  expect_equal(nrow(withlacoochee_report), 12)


})


test_that("two sites two parameters", {


  usgs_dv <- ww_dvUSGS(sites=c("02319394", "12304500"),
                    parameterCd = c("00010", "00060"))

  expect_error(expect_equal(usgs_dv[1,]$site_no, '1204500'))

  expect_equal(usgs_dv[1,]$site_no, '02319394')

  usgs_wy <- ww_wyUSGS(usgs_dv)

  expect_equal(usgs_wy %>%
                 dplyr::filter(wy == 2017) %>%
                 dplyr::pull(Wtemp_max), c(27.5, -Inf))

  usgs_wym <- ww_wymUSGS(usgs_dv)

  expect_equal(usgs_wym %>%
                 dplyr::filter(wy == 2019) %>%
                 nrow(), 24
  )
  usgs_month <- ww_monthUSGS(usgs_dv)

  expect_equal(nrow(usgs_month), 24)


  usgs_report <- ww_reportUSGSdv(usgs_dv, days = 11)

  expect_equal(nrow(usgs_report), 36)


})


