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

#' Group a Tibble With Inapplicable Groups
#'
#' Similar to \code{dplyr::group_by()}, this function groups a
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
#' @param ... Arguments of the form \code{var = c(val1, val2)} or the name of a variable
#' @return An igrouped tibble
#'
#' @export
group_by2 <- function (data, ...) {
  UseMethod("group_by2")
}

#' @export
group_by2.data.frame <- function (data, ...) {
  dots <- parse_grp_dots(...)
  if(missing(data)) { abort("group_by2: missing argument `data`.", class="error_bad_argument") }
  not_found <- names(dots)[!names(dots) %in% names(data)]
  if(length(not_found) != 0) {
    abort(paste0("group_by2: must group by variables found in `data`.\n",
                 "Could not find columns: ",
                 paste0(not_found, collapse = ", ")),
          class = "error_miss_col")
  }
  
  iwalk(dots, function (.x, .y) {
    if(!all(.x %in% data[[.y]])) {
      abort(paste0("group_by2: could not mark value `", .x, "` of `", .y,
                   "` as inapplicable."))
    }
  })

  group_by2_ok(data, dots)
}

group_by2_ok <- function (data, dots) {
  if(length(dots) == 0) { return(ungroup(data)) }
  gvars <- syms(names(dots))
  
  grouped <- dplyr::group_by(data, !!!gvars)
  groups <- attr(grouped, "groups")
  
  groups_out <- imap_dfc(dots, ~ cast_grps(groups, .x, .y)) %>%
    vec_cbind(groups[".rows"])
  
  igrouped_df(grouped, groups_out)
}

parse_grp_dots <- function (...) {
  dots <- enexprs(...)
  if (length(dots) == 0) { return(list()) }
  flatten(map(1:length(dots), dot_to_arg, dots))
}

# Parse an argument to group_by2
# 
# The param (dots[i]) should be either a named vector, which creates an inapplicable group,
# or a symbol making a full group. All other values are errors.
# 
dot_to_arg <- function (i, dots) {
  curr <- dots[i]
  if(is.symbol(curr[[1]])) {
    return(setNames(list(NULL), rlang::as_name(curr[[1]])))
  } else if (length(names(curr)) != 0 && names(curr) != "") {
    curr[[1]] <- eval(curr[[1]])
    return(curr)
  } else {
    stop("could not parse argument to group_by2")
  }
}

#' Ungroup a Tibble With Inapplicable Groups
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

# Add rows to capture each grouping
#
# Expansion is turning a hierarchical grouping with I-values into a flat one
# without I-values.
expand_igrps <- function (x) {
  inap_grps <- inap_selector(group_data(x))
  if (sum(inap_grps) == 0) { return(x) }

  exp_inaps <- expand_inap_grps(x, group_data(x)[inap_grps,])
  app_data <- x[applicable_row_nos(group_data(x)),]
  
  group_by2(vec_rbind(app_data, exp_inaps),
            !!!syms(group_vars(x)))
}

applicable_row_nos <- function (agrps) {
  unlist(
    agrps[!inap_selector(agrps),]$.rows
  )
}

expand_inap_grps <- function (x, inaps) {
  map_dfr(1:nrow(inaps),
          ~ expand_inap_row(x, as.list(inaps[.,])))
}

# data is the igrouped df, grow is the current row of group_data(data)
expand_inap_row <- function (data, grow) {
  grow <- grow[-length(grow)]
  selectors <- grow[!map_lgl(grow, inapplicable)]

  out <- data[same_group(data, selectors),]
  ivars <- igroup_vars(out)
  newgrps <- ivars[map_lgl(ivars, ~ length(.) > 0)]
  expand_igrp(group_by2(out, !!!newgrps))
}

# Expand a Data Frame with One Inapplicable Grouping
# 
# @param x df, that can only have exactly one grouping variable
# @return A df with only the changed (formerly inapplicable) rows
expand_igrp <- function (x) {
  if(length(group_vars(x)) > 1) {
    stop("argument x has multiple groups, and cannot tell which is inapplicable (expand_igrps)")
  }
  
  app_nos <- applicable_row_nos(group_data(x))
  if (length(app_nos) == 0) { return(x) }

  Idata <- x[-app_nos,]
  gdata <- group_data(x)

  fill_irow(Idata, gdata)
}

# Replicate an Inapplicable Row Filling In its Applicable Values
# 
# @param Idata df with one row containing an inapplicable value
# @param gdata df containing group_data() output for the whole group
# @return a df replicating the inapplicable row with the other applicable values in the group
fill_irow <- function (Idata, gdata) {
  nonIvals <- drop_inap_firstcol(gdata)
  expanded <- vec_cbind(nonIvals, tibble(data = list(Idata[!names(Idata) %in% names(nonIvals)])))
  
  sel_plm <- map_lgl(expanded, ~ "polymiss" %in% class(.))
  expanded[sel_plm] <- map_df(expanded[sel_plm], ~ field(., "x"))
  
  tidyr::unnest(expanded, cols = "data")
}

# Get All Applicable Values in the First Column
drop_inap_firstcol <- function (x) {
  x[!inapplicable(x[[1]]),1]
}

same_group <- function(data, grps) {
  if(length(grps) == 0) {
    return(rep_along(data[[1]], TRUE))
  }
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

#' @export
group_data.igrouped_df <- function (.data) {
  attr(.data, "groups")
}

#' @export
group_vars.igrouped_df <- function (x) {
  setdiff(names(dplyr::group_data(x)), c(".rows", "I"))
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.igrouped_df <- function (x) {
  grps <- dplyr::n_groups(x)
  group_sum <- paste0(paste0(format_igrps(igroup_vars(x)), collapse = ", "), " [", formatC(grps, big.mark = ","), "]")
  out <- c(`Row indices` = group_sum)
  
  if ("colgroups" %in% names(attributes(x))) {
    out <- c(out, `Col index` = attr(attr(x, "colgroups"), "index_name"))
  }
  x <- as_tibble(x)
  c(NextMethod(), out)
}

format_igrps <- function (igrps) {
  formatted <- map_chr(format(igrps), ~ ifelse(. == "", "", paste0(" (I: ", ., ")")))
  paste0(names(igrps), formatted)
}

restore_grps <- function (x, out) {
  if (nrow(out) == 0) {
    return(as_tibble(out))
  }

  igrps <- igroup_vars(x)
  prune_names <- igrps[intersect(names(igrps), names(out))]
  prune_vals <- imap(prune_names,
                    ~ intersect(.x, out[[.y]]))
  group_by2(out, !!!prune_vals)
}

#' @export
`[.igrouped_df` <- function (x, i, j, drop = FALSE) {
  out <- NextMethod()
  if (!is.data.frame(out)) {
    return(out)
  } else {
    restore_grps(x, out)
  }
}

#' @export
`[<-.igrouped_df` <- function (x, i, j, ..., value) {
  out <- NextMethod()
  restore_grps(x, out)
}
