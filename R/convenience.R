insert <- function(x, pos, item) {
  if (length(x) < (pos-1)) {
    stop("'x' is not long enough to have 'item' inserted as position 'pos'")
  }
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

safe.deparse <- function(x) {
  paste(deparse(x), collapse=" ")
}

# Quietly try to require a package
# Queitly require a package, returning an error message if that package is not installed.
# I lifted this from Hadley Wickham's ggplot2.
#
# @param name of package
# @keyword internal
try_require <- function(package) {
  available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts=FALSE)))
  missing <- package[!available]

  if (length(missing) > 0)
    stop(paste(package, collapse=", "), " package required for this functionality.  Please install and try again.", call. = FALSE)
}
