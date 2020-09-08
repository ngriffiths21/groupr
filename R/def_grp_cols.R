#' Convert a flat dataframe into a col-group structure
def_grp_cols <- function (x, data_name, grp_name, ...) {
  dots <- map(enexprs(...), rlang::eval_tidy, data = x)
  x <- as_tibble(x)
  x[[data_name]] <- add_grps(tibble(!!!dots),
                             grp_name, names(dots))
  oldnames <- map(enexprs(...), rlang::as_name)
  x[setdiff(names(x), oldnames)]
}

add_grps <- function (df, grp_name, grp_vals) {
  grp_dat <- tibble(
    !!grp_name := grp_vals,
    .cols = syms(grp_vals)
  )
  attr(df, "colgroups") <- grp_dat
  df
}


