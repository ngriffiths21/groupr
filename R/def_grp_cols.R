#' make a spec defining a column index
make_spec <- function (index_name, ...) {
  grps <- names(rlang::list2(...)[[1]])
  vals <- map(rlang::list2(...), ~ rlang::syms(.[grps]))
  out <- tibble(.index = grps, !!!vals)
  attr(out, "index_name") <- index_name
  out
}

#' make column index using a spec
spec_colgrp <- function (x, spec) {
  if (ncol(spec) != 2) { stop ("spec_clgrp: spec must identify one data column") }
  data_name <- setdiff(names(spec), ".index")

  dots <- map(spec[[data_name]], rlang::eval_tidy, data = x)
  
  oldnames <- map(spec[[data_name]], rlang::as_name)
  
  x[[data_name]] <- tibble(!!!dots)
  out <- x[setdiff(names(x), oldnames)]
  attr(out, "colgroups") <- spec
  out
}

#' Make a Single Column Grouping
#' 
#' 
#' @export
colgrp <- function (x, data_name, index_name = "group") {
  datacols <- setdiff(names(x), group_vars(x))
  vals <- datacols
  names(vals) <- datacols
  args <- rlang::list2(!!data_name := vals)
  
  spec_colgrp(x, make_spec(index_name, !!!args))
}

sep_colgrp <- function (x, sep) {
  
}



# older version of adding a colgroup spec
add_grps <- function (df, grp_name, grp_vals) {
  grp_dat <- tibble(
    !!grp_name := grp_vals,
    .cols = syms(grp_vals)
  )

  grp_ord <- grp_dat[order(grp_dat[1]),]
  attr(df, "colgroups") <- grp_ord
  df
}
