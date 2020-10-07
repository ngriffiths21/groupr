context("cols")

tstc_a <- tibble(
  grp = c(1,1,2),
  val1 = c(10,11,12),
  val2 = c(20,21,22)
)

tstc_b <- tibble(
  val_grp1 = c(1, 2, 3),
  val_grp2 = c(4, 5, 6),
  type_grp1 = c("a", "b", "c"),
  type_grp2 = c("d", "e", "f")
)

testc_c <-
  tibble(
    grp = c(1, 2),
    ok = c(1.9, 3.1),
    notok = c(NA, 4.9)
  ) %>% 
  group_by2(grp)

test_that("can create groups with colgroup()", {
  expect_equal(names(colgrp(testc_c, "val")), c("grp", "val"))
})

test_that("can create colgroups with sep_colgrp()", {
  expect_equal(names(sep_colgrp(tstc_b, "_")), c("val", "type"))
})