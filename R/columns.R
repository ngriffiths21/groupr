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
  datanms <- setdiff(names(spec), ".index")
  nested <- map_dfc(datanms, ~ make_one_col(x, spec[.]))
  out <- vec_cbind(x[group_vars(x)], nested)
  attr(out, "colgroups") <- spec
  out
}

make_one_col <- function (x, spec_col) {
  dots <- map(spec_col[[1]], rlang::eval_tidy, data = x)
  oldnames <- map(spec_col[[1]], rlang::as_name)
  
  x[[names(spec_col)]] <- tibble(!!!dots)
  x[names(spec_col)]
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

#' Separate columns by a character
#' 
#' @export
sep_colgrp <- function (x, sep, index_name = "group") {
  datacols <- setdiff(names(x), group_vars(x))
  splitnms <- strsplit(datacols, sep, fixed = TRUE)
  if(!all(map_lgl(splitnms, ~length(.) == 2))) {
    stop("The separator `", sep, "` must appear exactly once in each data column name.")
  }
  
  grp_by_dcol <- transpose(splitnms)
  
  spec_input <- reduce(1:length(datacols), function (y, x) {
    named_col <- setNames(datacols[x], grp_by_dcol[[2]][[x]])
    currgrp <- grp_by_dcol[[1]][[x]]
    y[[currgrp]] <- c(y[[currgrp]], named_col)
    y
  }, .init = list())

  spec <- make_spec(index_name, !!!spec_input)
  spec_colgrp(x, spec)
}

colgrp_vars <- function (x) {
  colgrps <- attr(x, "colgroups")
  setdiff(names(colgrps), ".index")
}

#' Set column grouping for an already structured dataframe
#' 
#' @export
infer_colgrps <- function (x, index_name = "group", sep = "_") {
  datacols <- x[map_lgl(x, is.data.frame)]
  datanms <- names(datacols)
  data_grps <- map(datanms, ~ setNames(paste0(., sep, names(x[[.]])), names(x[[.]])))
  names(data_grps) <- datanms
  
  attr(x, "colgroups") <- make_spec(index_name, !!!data_grps)
  x
}

col_index_name <- function (x) {
  attr(attr(x, "colgroup"), "index_name")
}
