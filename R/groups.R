igrouped_df <- function (x = data.frame(), groups = data.frame()) {
  x <- vctrs::vec_rbind(x, data.frame())
  groups <- vctrs::vec_rbind(groups, data.frame())
  vctrs::new_data_frame(x, class = c("igrouped_df", "tbl_df", "tbl"), groups = groups)
}

group_by2 <- function (data, ...) {
  dots <- rlang::list2(...)
  gvars <- syms(names(dots))

  grouped <- dplyr::group_by(data, !!!gvars)
  groups <- attr(grouped, "groups")

  groups$I <- purrr::reduce(purrr::imap(dots, ~ groups[[.y]] %in% .x), `|`)
  igrouped_df(grouped, groups)
}
 
pivot_grp <- function (df, data, rows, cols) {
  igroup_map(df, function (x) {
    grp_cols(x, cols, data)
  }) %>%
    reduce(vctrs::vec_rbind)
}

igroup_map <- function(data, fn) {
  grps <- attr(data, "groups")
  map2(grps$.rows, grps$I, ~ fn(data[.x,]))
}

grp_cols <- function (x, cols, data) {
  dta <- x[data]
  col_vals <- unique(x[cols])
  newname <- map_chr(col_vals, ~ str_c(names(dta), "_", .))

  x[newname] <- dta
  x[c(data,cols)] <- NULL
  x
}
