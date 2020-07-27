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

#' Group a tibble with inapplicable groups
#'
#' Similar to dplyr::group_by, this function groups a
#' tibble while also marking certain groups as inapplicable.
#'
#' A grouped tibble has one or more grouping variables, where each unique
#' combination of values identifies a group. This function allows some of
#' the values to be marked inapplicable, such that the corresponding rows
#' are not considered to be grouped on that variable at all.
#'
#' Grouping variables, and inapplicable values, are passed as arguments in
#' the form \code{group_var = c(value1, value2, ...)}. Any included values
#' will be marked inapplicable. If an argument has length 0 or is NULL, no
#' values will be marked inapplicable.
#'
#' @param data A tibble to group
#' @param ... Arguments of the form \code{name = c(inapplicableval1, ...)}
#' @return An igrouped tibble
#'
#' @export
group_by2 <- function (data, ...) {
  dots <- list2(...)
  if(missing(data)) { abort("group_by2: missing argument `data`.", class="error_bad_argument") }
  if(length(dots) == 0) { return(ungroup(data)) }
  gvars <- syms(names(dots))

  grouped <- dplyr::group_by(data, !!!gvars)
  groups <- attr(grouped, "groups")

  groups_out <- imap_dfc(dots, ~ cast_grps(groups, .x, .y)) %>%
    bind_cols(groups[".rows"])

  igrouped_df(grouped, groups_out)
}

#' Ungroup a tibble with inapplicable groups
#'
#' Ungroup method for tibbles that have inapplicable groups.
#'
#' @param x An igrouped tibble (as created by group_by2)
#' @return A tibble with no groups. The "groups" attribute will be set to
#' contain one column, .rows, with a single value that lists all rows.
#'
#' @export
ungroup.igrouped_df <- function (x) {
  attr(x, "groups") <- NULL
  as_tibble(x)
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

  sel_plm <- map_lgl(expanded, ~ "polymiss" %in% class(.))
  expanded[sel_plm] <- map_df(expanded[sel_plm], ~ field(., "x"))

  tidyr::unnest(expanded, cols = .data$data)
}

#' @export
group_data.igrouped_df <- function (.data) {
  attr(.data, "groups")
}

#' @export
group_vars.igrouped_df <- function (x) {
  setdiff(names(dplyr::group_data(x)), c(".rows", "I"))
}

#' @export
tbl_sum.igrouped_df <- function (x) {
  grps <- dplyr::n_groups(x)
  group_sum <- paste0(paste0(dplyr::group_vars(x), collapse = ", "), " [", formatC(grps, big.mark = ","), "]")
  c(NextMethod(), c(Groups = group_sum))
}
