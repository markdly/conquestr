context("Itanal")

df <- cq_itanal(cq_example(display = FALSE))

test_that("Values imported correctly", {
  expect_equal(unique(df$case), 1475)
  expect_equal(df$disc[4], 0.780)
})
