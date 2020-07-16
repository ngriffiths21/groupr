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

# maps a function over an igroup
igroup_map <- function(data, fn) {
  if (!"igrouped_df" %in% class(data)) { stop("argument data must be an igrouped_df (igroup_map())") }
  assert_that(is.function(fn), msg = "argument fn must be a function (igroup_map())")
  
  grps <- attr(data, "groups")
  map2(grps$.rows, grps$I, ~ fn(data[.x,]))
}

col_grps <- function (x, cols) {
  assert_that(is.list(cols), msg = "argument cols must be a list of strings (col_grps)")
  
  imap_dfr(cols, ~ make_col(x, .x, .y))
}

omake_col <- function (x, col, nm) {
  map(col, ~ tibble(
    name = rep_along(x[[.]], .),
    !!nm := x[[.]]
  )) %>%
    reduce(vctrs::vec_rbind)
}

pivot_cg <- function (x, cols) {
  group_modify(
    x, ~ col_grps(., cols)
  )
}
            
grp_cols <- function (x) {
  assert_that(is.data.frame(x), msg = "argument x must be dataframe (grp_cols)")

  grps <- attr(x, "groups")
  gnames <- names(grps[-length(grps)])
  dnames <- setdiff(names(x), gnames)
  grows <- grps[[".rows"]]

  out <- slice_cbind(ungroup(x[dnames]), grows)
  
  newnames <- cross2_str(
    dnames,
    names_fr_groups(grps[gnames])
  )

  names(out) <- newnames
  out
}

pivot_gc <- function (x, cols) {
  grps <- syms(setdiff(flatten(map(groups(x), as.character)), cols))

  x %>%
    group_by(!!!grps) %>%
    group_modify(~ grp_cols(group_by(., !!!syms(cols))))
}

slice_cbind <- function (x, rows) {
  map(rows, ~ slice(x, .)) %>%
    reduce(bind_cols)
}

names_fr_groups <- function (x) {
  map(transpose(x), lift(str_c, sep = "_"))
}

cross2_str <- function (x, y) {
  cross2(x, y) %>%
    map_chr(lift(str_c, sep = "_"))
}
