context("igroup")

tsti_a <- tibble::tribble(
  ~main, ~grp, ~val,
  "A", "x", 2,
  "A", "y", 3,
  "B", NA, 3
) %>%
  group_by2(grp = NA)

test_that("can make multiple groups", {
  res <- group_by2(mtcars, vs = NULL, gear = NULL)
  expect_equal(length(groups(res)), 2)
})

test_that("can expand an igrouped dataframe", {
})