context("Show files")

df1 <- cq_show(cq_example(display = FALSE, example_name = "ex1.shw"))
df2 <- cq_itanal(cq_example(display = FALSE)) # using the itanal for comparison

test_that("Values imported correctly", {
  expect_equal(df1$est, df2$delta, tolerance = 0.01)
})

test_that("imports even if mnsq is not requested (fit=no)", {
  x <-   cq_show(system.file("extdata", "no_fit.shw", package = "conquestr"), fit = FALSE)
  expect_warning(cq_show(system.file("extdata", "no_fit.shw", package = "conquestr")))
  expect_equal(df1$est, x$est)
  expect_equal(df1$err, x$err)
})
