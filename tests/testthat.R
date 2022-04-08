Sys.unsetenv("R_TESTS")
library(testthat)
library(whitewater)
test_check("whitewater")
