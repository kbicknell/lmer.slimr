insert <- function(x, pos, item) {
  stopifnot(length(x) >= (pos-1))
  first.bit <- x[1:pos-1]
  if (length(x) == (pos-1)) {
    return(c(first.bit, item))
  } else {
    last.bit <- x[pos:length(x)]
    return(c(first.bit, item, last.bit))
  }
}

#' Takes the formal arguments and the matched call to a function and provides a
#' list containing the subset of the matched call that corresponds to the
#' ellipsis. If nothings corresponds to the ellipsis, the list will be empty
get.ellipsis.args <- function(formals, matched.call) {
  ## get rid of function name (=matched.call[[1]])
  matched.args <- matched.call[seq(2, length(matched.call))]
  is.ellipsis.arg <- !(names(matched.args) %in% names(formals))
  if (any(is.ellipsis.arg)) {
    ellipsis.args <- as.list(matched.args[is.ellipsis.arg])
  } else {
    ellipsis.args <- list()
  }
  return(ellipsis.args)
}
