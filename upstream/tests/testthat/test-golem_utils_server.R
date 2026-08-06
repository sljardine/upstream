test_that("drop_sentinel strips the sentinel and keeps real selections", {
  expect_equal(drop_sentinel(c("0", "12345")), "12345")
  expect_equal(drop_sentinel(c("0", "123", "456")), c("123", "456"))
  expect_equal(drop_sentinel("0"), character(0))
  expect_equal(drop_sentinel(character(0)), character(0))
})

test_that("drop_sentinel copes with what selectize actually returns", {
  # selectize hands back characters, but the choices were built from numbers
  expect_equal(drop_sentinel(c(0, 12345)), "12345")
  expect_equal(drop_sentinel(NULL), character(0))
  expect_equal(drop_sentinel(c("0", NA, "77")), "77")
})

test_that("drop_sentinel honours a non-default sentinel", {
  expect_equal(drop_sentinel(c("none", "77"), sentinel = "none"), "77")
  # and does not strip "0" when that is a legitimate value
  expect_equal(drop_sentinel(c("none", "0"), sentinel = "none"), "0")
})

test_that("a site_id that merely contains the sentinel character survives", {
  # combined crossings look like "997675/933384"; "10" and "40" must not be
  # confused with the "0" sentinel by partial matching
  expect_equal(drop_sentinel(c("0", "10", "40", "0/1")), c("10", "40", "0/1"))
})
