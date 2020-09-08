context("cols")

tstc_a <- tibble(
  grp = c(1,1,2),
  val1 = c(10,11,12),
  val2 = c(20,21,22)
)

test_that("can create col groupings", {
  res <- def_grp_cols(tstc_a, "val", "which", val1 = val1, val2 = val2)
  expect_equal(names(res), c("grp", "val"))
})