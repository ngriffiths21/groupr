pivot_grps <- function (x, rows = NULL, cols = NULL) {
  assert_that(!missing(x), msg = "missing argument x (pivot_grps)")
  if(is.null(rows) & is.null(cols)) { return(x) }
  if(is.null(rows)) {
    return(pivot_gc(x, cols))
  }
  if (is.null(cols)) {
    return(pivot_cg(x, rows))
  }
  pivot_gc(pivot_cg(x, rows), cols)
}
  

col_grps <- function (x, cols) {
  assert_that(is.list(cols), msg = "argument cols must be a list of strings (col_grps)")
  
  imap_dfr(cols, ~ make_col(x, .x, .y))
}

make_col <- function (x, col, nm) {
  map(col, ~ tibble::tibble(
    name = rep_along(x[[.]], .),
    !!nm := x[[.]]
  )) %>%
    reduce(vctrs::vec_rbind)
}

pivot_cg <- function (x, cols) {
  dplyr::group_modify(
    x, ~ col_grps(., cols)
  )
}
            
grp_cols <- function (x) {
  assert_that(is.data.frame(x), msg = "argument x must be dataframe (grp_cols)")

  grps <- attr(x, "groups")
  gnames <- names(grps[-length(grps)])
  dnames <- setdiff(names(x), gnames)
  grows <- grps[[".rows"]]

  out <- slice_cbind(dplyr::ungroup(x[dnames]), grows)
  
  newnames <- cross2_str(
   dnames,
    names_fr_groups(grps[gnames])
  )

  names(out) <- newnames
  out
}

pivot_gc <- function (x, cols) {
  grps <- syms(setdiff(flatten(map(dplyr::groups(x), as.character)), cols))

  x %>%
    dplyr::group_by(!!!grps) %>%
    dplyr::group_modify(~ grp_cols(dplyr::group_by(., !!!syms(cols))))
}

slice_cbind <- function (x, rows) {
  map(rows, ~ dplyr::slice(x, .)) %>%
    reduce(dplyr::bind_cols)
}

names_fr_groups <- function (x) {
  map(transpose(x), lift(stringr::str_c, sep = "_"))
}

cross2_str <- function (x, y) {
  cross2(x, y) %>%
    map_chr(lift(stringr::str_c, sep = "_"))
}
