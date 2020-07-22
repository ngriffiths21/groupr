new_polymiss <- function(x = double(), miss = character()) {
  vec_assert(miss, ptype = character(), size=length(x))
  if(!all(miss == "I" | is.na(miss))) stop("The `miss` field must be I or NA.")

  new_rcrd(list(x = x, miss = miss), class = "polymiss")
}

polymiss <- function(x = double(), miss = character()) {
  new_polymiss(x, miss)
}

format.polymiss <- function (x, ...) {
  x2 <- field(x, "x")
  miss <- field(x, "miss")

  ifelse(is.na(miss), as.character(x2), "<I>")
}

inapplicable <- function (x) {
  if(!("polymiss" %in% class(x))) { return(rep_along(x, NA)) }

  miss <- field(x, "miss")

  !is.na(miss)
}
