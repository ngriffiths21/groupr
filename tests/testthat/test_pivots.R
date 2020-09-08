context("Pivoting")

testa <- tibble::tribble(
  ~ ok, ~ grp, ~ val,
  "ok", 1, 1.9,
  "ok", 2, 3.1,
  "notok", 2, 4.9
) %>%
  group_by2(ok, grp)

testb2 <-
  tibble(
    grp = c(1, 2),
    val = tibble(
      ok = c(1.9, 3.1),
      notok = c(NA, 4.9)
    )
  ) %>%
  group_by2(grp)

testb <-
  tibble::tribble(
    ~grp, ~val_ok, ~val_notok,
    1, 1.9, NA,
    2, 3.1, 4.9
  ) %>%
  group_by2(grp)

testc <- 
  tibble::tribble(
    ~grp, ~type, ~subgrp, ~val,
    1, "sub", "sub1", 1.9,
    1, "sub", "sub2", 3.1,
    2, "sub", "sub1", 9.7,
    2, "sub", "sub2", 3.8,
    2, "main", "na", 4.0
  ) %>%
  group_by2(grp, type, subgrp = "na")

testd <-
  tibble::tribble(
    ~grp1, ~grp2, ~val,
    1, 3, 0.3,
    1, 2, 0.4,
    1, 1, 0.5,
    0, 3, 0.8,
    0, 2, 0.9,
    0, 1, 1.3
  )

test_that("can pivot group to column", {
  expect_equal(pivot_gc(testa, "ok")$val, testb2$val)
})

test_that("can pivot column to group", {
  obj <- pivot_cg(testb, list(val = c("val_ok", "val_notok"))) %>%
    dplyr::rename(ok = name) %>%
    ungroup() %>%
    dplyr::mutate(ok = dplyr::recode(ok, "val_ok" = "ok", "val_notok" = "notok")) %>%
    tidyr::drop_na(val) %>%
    group_by2(ok = NULL, grp = NULL)
  expect_mapequal(obj, testa)
})

testres <- tibble(
  name = c("val_notok", "val_ok"),
  val_1 = c(NA, 1.9),
  val_2 = c(4.9, 3.1)
) %>%
  group_by2(name = NULL)

test_that("can pivot both ways", {
  res <- pivot_grps(testb, rows = list(val = c("val_ok", "val_notok")),
               cols = "grp")
  expect_s3_class(res, "tbl")
  expect_mapequal(res, testres)
})

test_that("can ignore I groups", {
  res <- pivot_grps(testc, cols = "type")
  expect_equal(res$val_main, c(NA, NA, 4, 4))
})

mtcars2 <- mtcars %>%
  dplyr::mutate(id = seq_along(mpg))

test_that("can pivot groups for a bigger dataset", {
  input <- group_by2(mtcars2, vs, id)
  expect_equal(nrow(pivot_grps(input, cols = "vs")), 32)
})

test_that("pivot_grps throws when col isn't in the grouping", {
  expect_error(pivot_grps(testd, cols = "grp"), class = "error_bad_arg")
})

test_that("pivot_grps throws when `row` refers to missing columns", {
  expect_error(pivot_grps(testd, rows = list(new = "nonexistent")),
               class = "error_miss_col")
})

test_that("pivot_grps throws when grouping does not uniquely identify all rows", {
  testd_grp <- group_by2(testd, grp2)
  expect_error(pivot_grps(testd_grp, cols = "grp2"), class = "error_bad_pivot")
})
