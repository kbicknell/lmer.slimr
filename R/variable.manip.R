#' for continuous variables. should be called in the model formula, to ensure
#' that variable is centered on the relevant data subset
#'
#' @export
ct <- function(x) {
  if (!is.numeric(x)) {
    stop("Variable passed to ct() is not numeric.")
  }
  scale(x, scale=F)
}

#' deviation coding. like contrasts = c(-.5, .5) but continuous
#'
#' @export
dc <- function(x) {
  if (is.factor(x)) {
    if (length(levels(x)) < 2) {
      stop("Variable passed to dc() has fewer than 2 levels.")
    } else if (length(levels(x)) > 2) {
      stop("Variable passed to dc() has more than 2 levels.")
    }
    return(as.numeric(x) - 1.5)
  } else if (is.logical(x)) {
    return(as.numeric(x) - 0.5)
  } else {
    stop("Variable passed to dc() is neither a factor nor logical.")
  }
}

#' to recode categorical variables. just does successive differences coding
#' (like contr.sdif), but is coded as continuous and split into multiple
#' factors, which is useful to remove random correlations from lmer models. This
#' is equivalent to deviation coding (-.5, .5) for binary contrasts. Input
#' should be coded as a factor(). Returns a data.frame to be cbind'ed to the
#' dataframe that x is in. The '.' in the column names should be interpreted as
#' a "minus".
#'
#' @importFrom MASS contr.sdif
#' @export
sdif.split <- function(x, label="") {
  if (!is.factor(x)) {
    stop("Variable passed to sdif.split() is not a factor.")
  }
  levs <- levels(x)
  n <- length(levs)
  cmat <- contr.sdif(n)
  cols <- lapply(seq_len(n-1), function(i) {
    name <- paste(label, levs[i+1], ".", levs[i], sep="")
    d <- data.frame(cmat[as.numeric(x), i])
    names(d) <- name
    return(d)
  })
  do.call(cbind, cols)
}
