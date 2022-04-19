testthat::test_that("the first rank is 1", {
  testthat::expect_equal(olympic_marathon[[1, 1]], 1)
})

testthat::test_that("the class is tbl/data frame", {
  testthat::expect_equal(class(olympic_marathon), c("tbl_df", "tbl", "data.frame"))
})

testthat::test_that("rank is numeric", {
  testthat::expect_equal(class(olympic_marathon$rank), "numeric")
})

