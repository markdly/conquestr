context("Itanal")

df <- cq_itanal(cq_example(display = FALSE))

test_that("Values imported correctly", {
  expect_equal(unique(df$case), 1475)
  expect_equal(df$disc[4], 0.780)
})

test_that("import one item", {
  one_item <- cq_itanal(system.file("extdata", "edge_case", "one_item.itn", package = "conquestr"))
  expect_equal(nrow(one_item), 1)
  expect_equal(one_item$item_index, "8")
})

test_that("import zero item", {
  zero_item <- cq_itanal(system.file("extdata", "edge_case", "zero_item.itn", package = "conquestr"))
  expect_equal(nrow(zero_item), 1)
  expect_equal(trimws(zero_item$id), "item:106 (I_am_a_zero_item)")
})
