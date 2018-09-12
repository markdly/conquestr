context("Show files")

df1 <- cq_show(cq_example(display = FALSE, example_name = "ex1.shw"))
df1 <- df1$data[[1]]
df2 <- cq_itanal(cq_example(display = FALSE)) # using the itanal for comparison

test_that("Values imported correctly", {
  expect_equal(df1$est, df2$delta, tolerance = 0.01)
})

test_that("imports even if mnsq is not requested (fit=no)", {
  x <-   cq_show(system.file("extdata", "no_fit.shw", package = "conquestr"), fit = FALSE)
  x <- x$data[[1]]
  expect_warning(cq_show(system.file("extdata", "no_fit.shw", package = "conquestr")))
  expect_equal(df1$est, x$est)
  expect_equal(df1$err, x$err)
})

test_that("long labels can be imported", {
  x <- cq_show(system.file("extdata", "long_labels.shw", package = "conquestr"))
  x <- x$data[[1]]
  expect_equal(df1$est, x$est, tolerance = 0.01)
})

test_that("no labels are OK", {
  x <- cq_show(system.file("extdata", "no_labels.shw", package = "conquestr"))
  x <- x$data[[1]]
  expect_equal(x$item_index, x$item)
  expect_equal(x$item, seq.int(9))
  expect_equal(df1$est, x$est, tolerance = 0.01)
})

test_that("interaction terms are imported", {
  x <- cq_show(system.file("extdata", "dif.shw", package = "conquestr"))
  expect_equal(x$term[3], "item*grp")
  expect_equal(head(names(x$data[[3]]), 4),
               c("item_index", "item", "grp_index", "grp"))
})

