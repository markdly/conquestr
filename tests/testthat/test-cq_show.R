context("Show files")

df1 <- cq_show(cq_example(display = FALSE, example_name = "ex1.shw"))
df2 <- cq_itanal(cq_example(display = FALSE)) # using the itanal for comparison

test_that("Values imported correctly", {
  expect_equal(round(df1$est, 2), df2$delta)
})

test_that("imports even if mnsq is not requested (fit=no)", {
  x <-   cq_show(system.file("extdata", "no_fit.shw", package = "conquestr"), fit = FALSE)
  expect_warning(cq_show(system.file("extdata", "no_fit.shw", package = "conquestr")))
  expect_equal(df1$est, x$est)
  expect_equal(df1$err, x$err)
})
