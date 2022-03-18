test_that("usgs dv", {

yaak_dv <- batch_USGSdv('12304500')

expect_error(expect_equal(yaak_dv[1,]$site_no, '1204500'))

expect_equal(yaak_dv[1,]$site_no, '12304500')

yaak_wy <- wyUSGS(yaak_dv)

expect_equal(yaak_wy %>%
               dplyr::filter(wy == 1967) %>%
               dplyr::pull(Peak), 10200)

yaak_wym <- wymUSGS(yaak_dv)

expect_equal(yaak_wym %>%
               dplyr::filter(wy == 1982) %>%
               nrow(), 12
               )
yaak_month <- monthUSGS(yaak_dv)

expect_equal(nrow(yaak_month), 12)


yaak_report <- reportUSGSdv(yaak_dv, days = 11)

expect_equal(nrow(yaak_report), 12)


})
