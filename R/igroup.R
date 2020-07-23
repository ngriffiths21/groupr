igrouped_df <- function (x = data.frame(), groups = data.frame()) {
  x <- vec_rbind(x, data.frame())
  groups <- vec_rbind(groups, data.frame())
  new_data_frame(x, class = c("igrouped_df", "tbl_df", "tbl"), groups = groups)
}

igroup_vars <- function (x) {
  grps <- attr(x, "groups")
  map(grps[-length(grps)],
      ~ unique(field(.[inapplicable(.)], "x")))
}

group_by2 <- function (data, ...) {
  dots <- list2(...)
  if(length(dots) == 0) { return(data) }
  gvars <- syms(names(dots))

  grouped <- dplyr::group_by(data, !!!gvars)
  groups <- attr(grouped, "groups")

  groups_out <- imap_dfc(dots, ~ cast_grps(groups, .x, .y)) %>%
    bind_cols(groups[".rows"])

  igrouped_df(grouped, groups_out)
}

cast_grps <- function (groups, .x, .y) {
  polymiss(
    groups[[.y]],
    to_miss(groups[[.y]] %in% .x)
  )
}

to_miss <- function (x) {
  ifelse(x, "I", NA_character_)
}


# maps a function over an igroup
igroup_map <- function(data, fn) {
  if (!"igrouped_df" %in% class(data)) { stop("argument data must be an igrouped_df (igroup_map())") }
  assert_that(is.function(fn), msg = "argument fn must be a function (igroup_map())")
  
  grps <- attr(data, "groups")
  map2(grps$.rows, grps$I, ~ fn(data[.x,]))
}

expand_igrps <- function (x) {
  old_igrps <- igroup_vars(x)
  inap_grps <- inap_selector(group_data(x))
  if (sum(inap_grps) == 0) { return(x) }

  inaps <- group_data(x)[inap_grps,]
  apps <- group_data(x)[!inap_grps,]
  
  exp_inaps <- map_dfr(1:nrow(inaps), 
                       ~ expand_item(x, as.list(inaps[.,])))

  app_data <- map_dfr(apps$.rows, ~ x[.,])
  group_by2(vec_rbind(app_data, exp_inaps),
            !!!old_igrps)
}

expand_item <- function (data, grow) {
  grow <- grow[-length(grow)]
  selectors <- grow[!map_lgl(grow, inapplicable)]

  out <- data[same_group(data, selectors),]
  ivars <- igroup_vars(out)
  newgrps <- ivars[map_lgl(ivars, ~ length(.) > 0)]
  expand_igrp(group_by2(out, !!!newgrps))
}

same_group <- function(data, grps) {
  reduce(imap(grps, ~ data[[.y]] == field(.x, "x")), `&`)
}

eq_or_na <- function (x, y) {
  (is.na(x) & is.na(y)) |
    (!is.na(x) & !is.na(y) & x == y)
}

inap_selector <- function (x) {
  x[-length(x)] %>%
    map(~ inapplicable(.)) %>%
    transpose() %>%
    map_lgl(~ reduce(., `|`))
}


expand_igrp <- function (x) {
  if(length(group_vars(x)) > 1) {
    stop("argument x has multiple groups, and cannot tell which is inapplicable (expand_igrps)")
  }

  I <- inapplicable(group_data(x)[[1]])

  Idata <- x[group_data(x)[I,]$.rows[[1]],]
  nonIdata <- x[-group_data(x)[I,]$.rows[[1]],]

  if (nrow(nonIdata) == 0) { return(x) }

  nonI <- group_data(x)[!I,]
  
  nonIvals <- nonI[!names(nonI) %in% c(".rows")]

  expanded <- bind_cols(nonIvals, tibble(data = list(Idata[!names(Idata) %in% names(nonIvals)])))

  coerced <- dplyr::mutate(expanded,
                           across(where(~ "polymiss" %in% class(.)),
                                        ~ field(., "x")))

  tidyr::unnest(coerced, cols = data)
}

group_data.igrouped_df <- function (.data) {
  attr(.data, "groups")
}

group_vars.igrouped_df <- function (x) {
  setdiff(names(dplyr::group_data(x)), c(".rows", "I"))
}

tbl_sum.igrouped_df <- function (x) {
  grps <- dplyr::n_groups(x)
  group_sum <- paste0(paste0(dplyr::group_vars(x), collapse = ", "), " [", formatC(grps, big.mark = ","), "]")
  c(NextMethod(), c(Groups = group_sum))
}
