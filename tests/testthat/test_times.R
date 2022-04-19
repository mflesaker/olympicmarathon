testthat::test_that("times_over_time makes a ggplot", {
  testthat::expect_equal(class(times_over_time()), c("gg", "ggplot"))
})
