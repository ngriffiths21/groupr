context("igroup")

tsti_a <- tibble::tribble(
  ~main, ~grp, ~val,
  "A", "x", 2,
  "A", "y", 3,
  "B", NA, 3
) %>%
  group_by2(main = NULL, grp = NA)

tsti_b <-
  tibble::tribble(
    ~grp, ~type, ~subgrp, ~val,
    1, "sub", "sub1", 1.9,
    1, "sub", "sub2", 3.1,
    2, "sub", "sub1", 9.7,
    2, "sub", "sub2", 3.8,
    2, "main", "na", 4.0
  ) %>%
  group_by2(grp = NULL, subgrp = "na")

test_that("group_by2 can make multiple groups", {
  res <- group_by2(mtcars, vs = NULL, gear = NULL)
  expect_equal(length(groups(res)), 2)
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
  res <- select(ungroup(expand_igrps(tsti_b)), subgrp, val)
  cmp <- tibble::tibble(
    subgrp = c("sub1", "sub2", "sub1", "sub2", "sub1", "sub2"),
    val = c(1.9, 3.1, 9.7, 3.8, 4.0, 4.0)
  )
  expect_mapequal(res, cmp)
})
