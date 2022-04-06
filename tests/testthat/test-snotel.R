test_that("snotel dv", {

  stahl_dv <- batch_SNOTELdv('787')

  expect_error(expect_equal(stahl_dv[1,]$site_id, 7870))

  expect_equal(stahl_dv[1,]$site_id, 787)

  stahl_wy <- wySNOTEL(stahl_dv)

  expect_equal(stahl_wy %>%
               dplyr::filter(wy == 1981) %>%
               dplyr::pull(swe_max), 34.5)

  stahl_wym <- wymSNOTEL(stahl_dv)

  expect_equal(stahl_wym %>%
                 dplyr::filter(wy == 1982) %>%
                 nrow(), 12)

  stahl_month <- monthSNOTEL(stahl_dv)

  expect_equal(nrow(stahl_month), 12)

  stahl_report <- reportSNOTELdv(stahl_dv, days = 11)

  expect_equal(nrow(stahl_report), 12)

  stahl_report <- reportSNOTELmv(stahl_dv, choice_months = 5)

  expect_equal(nrow(stahl_report), 6)

  stahl_hourly <- hourlySNOTEL(stahl_dv, days = 7)

})
