context("igroup")

tsti_a <- tibble::tribble(
  ~main, ~grp, ~val,
  "A", "x", 2,
  "A", "y", 3,
  "B", NA, 3
) %>%
  group_by2(main, grp = NA)

tsti_b <-
  tibble::tribble(
    ~grp, ~type, ~subgrp, ~val,
    1, "sub", "sub1", 1.9,
    1, "sub", "sub2", 3.1,
    2, "sub", "sub1", 9.7,
    2, "sub", "sub2", 3.8,
    2, "main", "na", 4.0
  ) %>%
  group_by2(grp, subgrp = "na")

test_that("group_by2 throws if no data", {
  expect_error(group_by2())
})

test_that("group_by2 throws if groups are unavailable", {
  expect_error(group_by2(mtcars, nonexistent),
               class = "error_miss_col")
})

test_that("group_by2 throws if inapplicable value doesn't exist", {
  expect_error(group_by2(mtcars, vs = 3))
})

test_that("group_by2 can make multiple groups", {
  res <- group_by2(mtcars, vs, gear)
  expect_equal(length(dplyr::groups(res)), 2)
})

test_that("can ungroup", {
  res <- group_data(ungroup(tsti_a))
  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 1)
})

test_that("group_by2 ungroups if empty args", {
  len <- length(group_vars(group_by2(tsti_a)))
  expect_equal(len, 0)
})

test_that("can expand an igrouped dataframe", {
  res <- dplyr::select(ungroup(expand_igrps(tsti_b)), subgrp, val)
  cmp <- tibble(
    subgrp = c("sub1", "sub2", "sub1", "sub2", "sub1", "sub2"),
    val = c(1.9, 3.1, 9.7, 3.8, 4.0, 4.0)
  )
  expect_mapequal(res, cmp)
})

test_that("can expand a different igroup (?)", {
  expect_true({
    expand_igrps(group_by2(mtcars, vs = 1))
    TRUE
  })
})
