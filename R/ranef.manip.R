get.terms <- function(x) {
  ## returns the terms of a single random effect specification
  stopifnot(x[[1]] == "|")
  lhs <- x[[2]]
  group <- x[[3]]
  lhs.as.terms <- terms(formula(paste("~", deparse(lhs))))
  intercept <- attr(lhs.as.terms, "intercept")
  terms <- attr(lhs.as.terms, "term.labels")
  terms <- c(intercept, terms)
  list(terms=terms, group=group)
}

build.ranef.string <- function(terms, group) {
  terms <- paste(terms, collapse="+")
  paste("(", terms, "|", group, ")", sep="")
}

get.all.steps <- function(f, df) {
  if (class(f) != "formula") {
    f <- formula(f)
  }
  f.prime <- drop.corr.from.lmer.formula(f, df)
  if (f.prime == F) {
    return(list(f))
  } else {
    return(c(f, get.all.steps(f.prime, df)))
  }
}

can.reparam <- function(term, df) {
  ## we can reparameterize 1+x to 0+x iff x is a factor
  if (length(term$terms)==2 && term$terms[1]=="1") {
    ## this looks complicated because we want to handle cases with function
    ## calls like "factor(x)"
    if (with(df, is.factor(eval(parse(text=term$terms[2]))) ||
               is.logical(eval(parse(text=term$terms[2]))))) {
      return(T)
    }
  }
  return(F)
}

#' @importFrom lme4 nobars findbars
drop.corr.from.lmer.formula <- function(f, df) {
  ## drops a single row out of the correlation matrix of a mixed-effects
  ## regression model formula
  fixedefs <- lme4:::nobars(f)
  ranefs <- lme4:::findbars(f)
  terms.list <- lapply(ranefs, get.terms)
  num.terms <- sapply(terms.list, function(x) {length(x$terms)})
  highest <- max(num.terms)

  ## check for cases with just two terms where the first is 0
  highest.bool <- (num.terms == highest)
  if (highest==1) { # this only happen when there are only random intercepts
    return(F)
  } else if (highest==2) {
    ## if a term has two elements and the first one is zero, there's no
    ## correlation to remove!
    zero.bool <- sapply(terms.list, function(x) {x$terms[1] != 0})
    ## if a term has two elements, the first one is one, and the other is a
    ## factor, there's no correlation to remove! (but it can be reparameterized)
    reparam.bool <- sapply(terms.list, function(x) can.reparam(x, df))
    candidate.indices <- which(zero.bool & highest.bool & !reparam.bool)
    candidate.reparam.indices <- which(reparam.bool)
  } else {
    candidate.indices <- which(highest.bool)
    ## don't reparameterize if we can still remove correlations
    candidate.reparam.indices <- c()
  }
  if ((length(candidate.indices) == 0) &&
        (length(candidate.reparam.indices)==0)) {
    return(F)
  } else if (length(candidate.indices) > 0) {
    ## decrement the last one
    index.to.decrement <- candidate.indices[length(candidate.indices)]

    terms.to.decrement <- terms.list[[index.to.decrement]]$terms
    group.to.decrement <- terms.list[[index.to.decrement]]$group
    new.terms <- list(terms=c(0,terms.to.decrement[length(terms.to.decrement)]),
                      group=group.to.decrement)
    terms.list[[index.to.decrement]]$terms <-
      terms.to.decrement[1:length(terms.to.decrement)-1]
    terms.list <- insert(terms.list, index.to.decrement+1, list(new.terms))
  } else { # length(candidate.reparam.indices) > 0
    index.to.reparam <-
      candidate.reparam.indices[length(candidate.reparam.indices)]
    terms.list[[index.to.reparam]]$terms[1] <- "0"
  }
  terms <- sapply(terms.list,
                  function(x) {build.ranef.string(x$terms, x$group)})
  ranef.string <- paste(terms, collapse="+")
  formula(paste(deparse(fixedefs), ranef.string,sep="+"))
}
