#' Convert a flat dataframe into a col-group structure
def_grp_cols <- function (x, data_name, grp_name, ...) {
  dots <- map(enexprs(...), rlang::eval_tidy, data = x)
  x[[data_name]] <- tibble(!!!dots)
  oldnames <- map(enexprs(...), rlang::as_name)
  x <- add_grps(x, grp_name, names(dots))
  x[setdiff(names(x), oldnames)]
}

add_grps <- function (df, grp_name, grp_vals) {
  grp_dat <- tibble(
    !!grp_name := grp_vals,
    .cols = syms(grp_vals)
  )

  grp_ord <- grp_dat[order(grp_dat[1]),]
  attr(df, "colgroups") <- grp_ord
  df
}
