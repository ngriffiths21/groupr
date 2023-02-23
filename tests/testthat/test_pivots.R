context("Pivoting")

testa <- tibble::tribble(
  ~ is_ok, ~ grp, ~ val,
  "ok", 1, 1.9,
  "ok", 2, 3.1,
  "notok", 2, 4.9
) %>%
  group_by2(is_ok, grp)

testb2 <-
  tibble(
    grp = c(1, 2),
    val = tibble(
      ok = c(1.9, 3.1),
      notok = c(NA, 4.9)
    )
  ) %>%
  group_by2(grp) %>% 
  infer_colgrps("is_ok")

testb3 <-
  tibble(
    grp = c(1, 2),
    val = tibble(
      ok = c(1.9, 3.1),
      notok = c(NA, 4.9)
    ),
    val2 = tibble(
      ok = c(3, 4),
      notok = c(NA, 5)
    ),
  ) %>%
  group_by2(grp) %>% 
  infer_colgrps("is_ok")

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
  expect_equal(pivot_gc(testa, "is_ok"), testb2)
})

test_that("can pivot column to group", {
  obj <- pivot_cg(testb2, "is_ok")
  expect_equal(nrow(dplyr::inner_join(testa, obj)), 3)
})

testres <- tibble(
  is_ok = c("notok", "ok"),
  val = tibble(`1` = c(NA, 1.9),
               `2` = c(4.9, 3.1))
) %>%
  group_by2(is_ok)

test_that("can pivot both ways", {
  res <- pivot_grps(testb2, rows = "is_ok",
               cols = "grp")
  expect_s3_class(res, "tbl")
  expect_equal(names(res), names(testres))
})

test_that("can pivot two column groupings to rows", {
  expect_equal(names(pivot_cg(testb3, "is_ok")), c("grp", "is_ok", "val", "val2"))
})

test_that("can ignore I groups", {
  res <- pivot_grps(testc, cols = "type")
  expect_equal(res$val$main, c(NA, NA, 4, 4))
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
  expect_error(pivot_grps(testd, rows = "nonexistent"),
               class = "error_miss_col")
})

test_that("pivot_grps throws when grouping does not uniquely identify all rows", {
  testd_grp <- group_by2(testd, grp2)
  expect_error(pivot_grps(testd_grp, cols = "grp2"), class = "error_bad_pivot")
})

test_that("can recover inapplicable row groupings after pivoting to columns", {
  scientists %>% 
    dplyr::filter(type != "Age") %>% 
    group_by2(type, person, city, day = NA_real_) %>% 
    pivot_grps(cols = "type") -> widetest
  
  # make some attribute denoting that location rows should have inapplicable day
  attr(widetest, "metas") <- list(Location = "day", Value = character())
  
  pivot_grps(widetest, rows = "type")
  
  stop("incorrect pivoting")
})

test_that("can convert an igrouped_df to internal pivot structure", {
  hierarchical |>
    group_by2(grp, type, subgrp = "na") |>
    to_pivoting_data() -> int_test
  
  stop("test not written yet")
})

test_that("can convert internal pivot data to igrouped_df", {
  hierarchical |>
    group_by2(grp, type, subgrp = "na") |>
    to_pivot_data() -> int_test
  
  from_test <- from_pivot_data(int_test, cols = "grp")
  
  stop("test not written and no column pivots yet")
})
  
