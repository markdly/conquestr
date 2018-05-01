context("Syntax")

test_that("Default usage", {
  expect_error(cqc_syntax())
  expect_error(cqc_syntax("myname"))
  expect_true(typeof(cqc_syntax("myname", "1-10")) == "list")
})

test_that("Optional args work", {
  # cqc_syntax is essentially customised string interpolation
  # override the defaults to show simple expected results
  expect_equal(paste(cqc_syntax("x", "1", cmds = list("foo"))), "foo")
  expect_equal(paste(cqc_syntax("x", "1", cmds = "foo{bar}",
                                lookup_vals = list(bar = "baz"))), "foobaz")
  # expect an error if there's a placeholder we haven't provide a value for
  expect_error(cqc_syntax("x", "1", cmds = "foo{bar}", lookup_vals = list(zzz = "baz")))
})

test_that("positional args work", {
  expect_equal(paste(cqc_syntax("x", "1", "fname",
                                list(a = "my {foo}"),
                                list(foo = "baz"))), "my baz")
})


test_that("condense resp cols", {
  expect_warning(cqc_resp_cols(c(3, 2, 1)))
  expect_equal(suppressWarnings(cqc_resp_cols(c(3, 2, 1))), "3, 2, 1")
  expect_error(cqc_resp_cols(c(1, 1)))
  expect_equal(cqc_resp_cols(c(1,3,4,6)), "1, 3-4, 6")
  expect_equal(cqc_resp_cols(c(1,2,3,4,5,6)), "1-6")
})
