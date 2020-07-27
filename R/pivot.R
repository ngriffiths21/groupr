pivot_grps <- function (x, rows = NULL, cols = NULL) {
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
  cbind(
    x[group_cols(data = x)],
    imap_dfr(cols, ~ make_col(x, .x, .y))
  )
}

make_col <- function (x, col, nm) {
  map(col, ~ tibble::tibble(
    name = rep_along(x[[.]], .),
    !!nm := x[[.]]
  )) %>%
    reduce(vctrs::vec_rbind)
}

pivot_cg <- function (x, cols) {
  old_igrps <- igroup_vars(x)
  out <- dplyr::group_modify(
    x, ~ col_grps(., cols)
  )
  group_by2(out, !!!old_igrps)
}
            
grp_cols <- function (x) {
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
  old_igrps <- igroup_vars(x)
  grps <- old_igrps[!names(old_igrps) %in% cols]
  exp <- expand_igrps(group_by2(x, !!!grps))

  exp %>%
    group_by(!!!syms(group_vars(exp))) %>%
    dplyr::group_modify(~ grp_cols(dplyr::group_by(., !!!syms(cols)))) %>%
    group_by2(!!!grps)
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
