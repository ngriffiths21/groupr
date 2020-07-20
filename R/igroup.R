igrouped_df <- function (x = data.frame(), groups = data.frame()) {
  x <- vctrs::vec_rbind(x, data.frame())
  groups <- vctrs::vec_rbind(groups, data.frame())
  vctrs::new_data_frame(x, class = c("igrouped_df", "tbl_df", "tbl"), groups = groups)
}

group_by2 <- function (data, ...) {
  dots <- list2(...)
  gvars <- syms(names(dots))

  grouped <- dplyr::group_by(data, !!!gvars)
  groups <- attr(grouped, "groups")

  groups$I <- reduce(imap(dots, ~ groups[[.y]] %in% .x), `|`)
  igrouped_df(grouped, groups)
}

# maps a function over an igroup
igroup_map <- function(data, fn) {
  if (!"igrouped_df" %in% class(data)) { stop("argument data must be an igrouped_df (igroup_map())") }
  assert_that(is.function(fn), msg = "argument fn must be a function (igroup_map())")
  
  grps <- attr(data, "groups")
  map2(grps$.rows, grps$I, ~ fn(data[.x,]))
}
