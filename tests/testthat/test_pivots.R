context("Pivoting")

testa <- tibble::tribble(
  ~ ok, ~ grp, ~ val,
  "ok", 1, 1.9,
  "ok", 2, 3.1,
  "notok", 2, 4.9
) %>%
  dplyr::group_by(ok, grp)

testb <-
  tibble::tribble(
    ~grp, ~val_ok, ~val_notok,
    1, 1.9, NA,
    2, 3.1, 4.9
  ) %>%
  dplyr::group_by(grp)

testc <- 
  tibble::tribble(
    ~main, ~grp, ~val,
    1, "sub1", 1.9,
    1, "sub2", 3.1,
    2, "sub1", 9.7,
    2, "sub2", 3.8,
    3, NA, 4.0
  ) %>%
  dplyr::group_by(grp, main, .add = TRUE)

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

  res
