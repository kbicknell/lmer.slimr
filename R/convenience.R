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
