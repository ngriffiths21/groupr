# make a spec defining a column index
make_spec <- function (index_name, ...) {
  grps <- names(rlang::list2(...)[[1]])
  vals <- map(rlang::list2(...), ~ rlang::syms(.[grps]))
  out <- tibble(.index = grps, !!!vals)
  attr(out, "index_name") <- index_name
  out
}

# make column index using a spec
spec_colgrp <- function (x, spec) {
  if (!"tbl" %in% class(x)) {
    x <- as_tibble(x)
  }

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
#' Takes a tibble and groups columns together into a single data column. All
#' columns that are not row indices will be grouped, and the resulting column
#' will be named \code{data_name}.
#' 
#' @param x A tibble
#' @param data_name A string, the name of the new column
#' @param index_name A string, the name of the new column index
#' @return A grouped tibble
#' 
#' @export
colgrp <- function (x, data_name, index_name = "group") {
  datacols <- setdiff(names(x), group_vars(x))
  vals <- datacols
  names(vals) <- datacols
  args <- rlang::list2(!!data_name := vals)
  
  spec_colgrp(x, make_spec(index_name, !!!args))
}

#' Separate Columns By a Character
#' 
#' Creates a column index by interpreting each column name as a data column name
#' and a group name separated by \code{sep}. Only columns that are not row indices
#' are used. \code{sep} must occur exactly once in each column name.
#' 
#' @param x A tibble
#' @param sep A character delimiting the two parts of the name
#' @param index_name A name for the new column index
#' @return A grouped tibble
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

#' Set Column Grouping for a Structured Data Frame
#' 
#' Takes a data frame where each non row index is a data frame column, and
#' sets the corresponding column grouping.
#' 
#' @param x A tibble with data frame data columns
#' @param index_name A name for the new column index
#' @param sep A character used to join the data column and group names
#' @return A grouped tibble
#' 
#' 
#' @export
infer_colgrps <- function (x, index_name = "group", sep = "_") {
  if (!is.null(attr(x, "colgroups"))) { return(x) } # already grouped

  datacols <- x[setdiff(names(x), group_vars(x))]
  if (!all(map_lgl(datacols, is.data.frame))) {
    abort("infer_colgrps: `x` must have data frame data columns")
  }

  datanms <- names(datacols)
  data_grps <- map(datanms, ~ setNames(paste0(., sep, names(x[[.]])), names(x[[.]])))
  names(data_grps) <- datanms
  
  attr(x, "colgroups") <- make_spec(index_name, !!!data_grps)
  x
}

col_index_name <- function (x) {
  attr(attr(x, "colgroup"), "index_name")
}

#' Remove Column Grouping
#' 
#' Separate the current column grouping, represented by a data frame column, into
#' individual columns. The name of each new column is a concatenation of the
#' data column name, the argument \code{sep}, and the group name.
#' 
#' @param data A tibble with a column grouping
#' @param sep The separator character used to construct new column names
#' @return A tibble without a column grouping
#' 
#' @export
drop_colgrp <- function (data, sep = "_") {
  tidyr::unpack(data, colgrp_vars(data), names_sep = sep) %>% 
    group_by2(!!!igroup_vars(data))
}


