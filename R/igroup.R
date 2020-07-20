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

expand_igrps <- function (x) {
  if(length(group_vars(x)) > 1) {
    stop("argument x has multiple groups, and cannot tell which is inapplicable (expand_igrps)")
  }

  I <- group_data(x)$I

  Idata <- x[group_data(x)[I,]$.rows[[1]],]
  nonIdata <- x[-group_data(x)[I,]$.rows[[1]],]

  
  
  nonI <- group_data(x)[!group_data(x)$I,]
  
  nonIvals <- nonI[!names(nonI) %in% c(".rows", "I")]

  expanded <- bind_cols(nonIvals, tibble(data = list(Idata[!names(Idata) %in% names(nonIvals)])))

  vctrs::vec_rbind(tidyr::unnest(expanded, cols = data),
                   nonIdata)
}

group_data.igrouped_df <- function (.data) {
  attr(.data, "groups")
}

group_vars.igrouped_df <- function (x) {
  setdiff(names(group_data(x)), c(".rows", "I"))
}

tbl_sum.igrouped_df <- function (x) {
  grps <- n_groups(x)
  group_sum <- paste0(paste0(group_vars(x), collapse = ", "), " [", formatC(grps, big.mark = ","), "]")
  c(NextMethod(), c(Groups = group_sum))
}
