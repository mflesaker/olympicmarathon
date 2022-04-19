testthat::test_that("Test error for invalid nationality", {
  testthat::expect_error(two_country_viz("USA", "ITL"), "nationality ITL not found in data.")
})

testthat::test_that("Test error for invalid input type", {
  testthat::expect_error(two_country_viz(3, "ITL"), "nationality1 input value must be of class character. 3 has class numeric.")
})

testthat::test_that("Returns ggplot object", {
  testthat::expect_equal(class(two_country_viz("SUI", "NZL")), c("gg", "ggplot"))
})




