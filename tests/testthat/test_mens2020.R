testthat::test_that("the first rank is 1", {
  testthat::expect_equal(mens_2020_data[[1, 1]], 1)
})

testthat::test_that("the class is tbl/data frame", {
  testthat::expect_equal(class(mens_2020_data), c("tbl_df", "tbl", "data.frame"))
})
