context("PVs")

pv05 <- cq_pv(cq_example(display = FALSE, example_name = "ex1_05.pv"))
pv10 <- cq_pv(cq_example(display = FALSE, example_name = "ex1_10.pv"), np = 10)

library(dplyr)
library(tidyr)
pv05_wide <- pv05 %>% select(recid, field, val) %>% spread(field, val)
pv10_wide <- pv10 %>% select(recid, field, val) %>% spread(field, val)

test_that("Values imported correctly", {
  pvt <- tail(pv05, 8)
  expect_equal(pvt$index, c("1209", "1", "2", "3", "4", "5", NA, NA))
  expect_equal(pvt$val, c("11209", "-0.12", "-0.86", "-2.85", "-1.53", "-0.02", "-0.77869", "0.73213"))
  expect_equal(unique(pvt$recid), 1209L)
  expect_equal(pvt$field, c("pid", "pv1", "pv2", "pv3", "pv4", "pv5", "eap", "eap_se"))
})

test_that("Num variables is correct", {
  # expected width is np + 3 (pid, eap, eap_se) + 1 (recid)
  expect_equal(ncol(pv05_wide), 5 + 4)
  expect_equal(ncol(pv10_wide), 10 + 4)
})

test_that("Num records is correct", {
  # each record takes up np + 3 rows in the pv file
  expect_equal(nrow(pv05_wide), nrow(pv05) / 8)
  expect_equal(nrow(pv10_wide), nrow(pv10) / 13)
})
