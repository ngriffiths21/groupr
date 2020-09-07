#' Convert a flat dataframe into a col-group structure
def_grp_cols <- function (x, data_name, grp_name, ...) {
  dots <- map(enexprs(...), rlang::eval_tidy, data = x)
  x <- as_tibble(x)
  x[[data_name]] <- tibble(
    !!grp_name := tibble(!!!dots)
  )
  x
}
