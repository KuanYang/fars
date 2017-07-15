context("all")

## TODO: Rename context
## TODO: Add more tests

test_that("make_filename works", {
  expect_equal(make_filename(2013), paste0(system.file("extdata", package="fars"), "/accident_", 2013, ".csv.bz2"))
})
