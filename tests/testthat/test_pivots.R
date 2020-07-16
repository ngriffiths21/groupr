context("Pivoting")

testa <- tribble(
  ~ ok, ~ grp, ~ val,
  "ok", 1, 1.9,
  "ok", 2, 3.1,
  "notok", 2, 4.9
) %>%
  group_by(ok, grp)

testb <-
  tribble(
    ~grp, ~val_ok, ~val_notok,
    1, 1.9, NA,
    2, 3.1, 4.9
  ) %>%
  group_by(grp)

test_that("can pivot group to column", {
  expect_equal(pivot_gc(testa, "ok"), testb)
})

test_that("can pivot column to group", {
  obj <- pivot_cg(testb, list(val = c("val_ok", "val_notok"))) %>%
    rename(ok = name) %>%
    mutate(ok = recode(ok, "val_ok" = "ok", "val_notok" = "notok")) %>%
    drop_na(val) %>%
    group_by(ok, grp)
  expect_mapequal(obj, testa)
})


