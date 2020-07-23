context("Pivoting")

testa <- tibble::tribble(
  ~ ok, ~ grp, ~ val,
  "ok", 1, 1.9,
  "ok", 2, 3.1,
  "notok", 2, 4.9
) %>%
  group_by2(ok = NULL, grp = NULL)

testb <-
  tibble::tribble(
    ~grp, ~val_ok, ~val_notok,
    1, 1.9, NA,
    2, 3.1, 4.9
  ) %>%
  group_by2(grp = NULL)

testc <- 
  tibble::tribble(
    ~grp, ~type, ~subgrp, ~val,
    1, "sub", "sub1", 1.9,
    1, "sub", "sub2", 3.1,
    2, "sub", "sub1", 9.7,
    2, "sub", "sub2", 3.8,
    2, "main", "na", 4.0
  ) %>%
  group_by2(grp = NULL, type = NULL, subgrp = "na")

test_that("can pivot group to column", {
  expect_equal(pivot_gc(testa, "ok"), testb)
})

test_that("can pivot column to group", {
  obj <- pivot_cg(testb, list(val = c("val_ok", "val_notok"))) %>%
    dplyr::rename(ok = name) %>%
    dplyr::mutate(ok = dplyr::recode(ok, "val_ok" = "ok", "val_notok" = "notok")) %>%
    tidyr::drop_na(val) %>%
    dplyr::group_by(ok, grp)
  expect_mapequal(obj, testa)
})

test_that("can pivot both ways", {
  res <- pivot_grps(testb, rows = list(val = c("val_ok", "val_notok")),
               cols = "grp")
  expect_s3_class(res, "tbl")
})

test_that("can ignore I groups", {
  res <- pivot_grps(testa, cols = "grp")
})
