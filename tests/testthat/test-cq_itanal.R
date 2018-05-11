context("Itanal")

df <- cq_itanal(cq_example(display = FALSE))

test_that("Values imported correctly", {
  expect_equal(unique(df$case), 1209)
  expect_equal(df$disc[4], 0.43)
})

test_that("import one item", {
  one_item <- cq_itanal(system.file("extdata", "edge_case", "one_item.itn", package = "conquestr"))
  expect_equal(nrow(one_item), 1)
  expect_equal(one_item$item_index, "8")
})

test_that("import zero item", {
  x <- suppressWarnings(cq_itanal(system.file("extdata", "edge_case", "zero_item.itn", package = "conquestr")))
  expect_warning(cq_itanal(system.file("extdata", "edge_case", "zero_item.itn", package = "conquestr")))
  expect_equal(nrow(x), 1)
  expect_equal(trimws(x$id), "item:106 (I_am_a_zero_item)")
})

test_that("item with one response code", {
  x <- suppressWarnings(cq_itanal(system.file("extdata", "edge_case", "one_resp_code.itn", package = "conquestr")))
  expect_warning(cq_itanal(system.file("extdata", "edge_case", "one_resp_code.itn", package = "conquestr")))
  expect_equal(nrow(x), 1)
  expect_equal(trimws(x$id), "item:25 (one_resp_code)")
})

test_that("very difficult item with extreme logit", {
  x <- suppressWarnings(cq_itanal(system.file("extdata", "edge_case", "difficult_item.itn", package = "conquestr")))
  expect_warning(cq_itanal(system.file("extdata", "edge_case", "difficult_item.itn", package = "conquestr")))
  expect_equal(nrow(x), 1)
  expect_equal(trimws(x$id), "item:111 (difficult_item)")
  expect_equal(x$delta, -37.01)
  expect_equal(x$thrsh, -32.00)
  expect_equal(x$mnsq, NA_real_)
})

test_that("item rest cor version imports correctly", {
  x <- cq_itanal(system.file("extdata", "edge_case", "item_rest_corr_version.itn", package = "conquestr"))
  expect_equal(nrow(x), 1)
  expect_equal(trimws(x$id), "item:120 (120)")
  expect_equal(x$delta, 2.74)
  expect_equal(x$thrsh, 2.74)
  expect_equal(x$mnsq, NA_real_)
  expect_equal(x$disc, 0.25)
})

test_that("thresholds still read correctly even if mnsq is not requested (fit=no)", {
  x <- cq_itanal(system.file("extdata", "no_fit.itn", package = "conquestr"))
  expect_equal(nrow(x), 9)
  expect_equal(x$thrsh[1], 0.39)
  expect_equal(x$mnsq[1], NA_real_)
})
