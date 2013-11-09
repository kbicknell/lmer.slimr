library(parallel)
library(lme4)

#' Evaluate sequence of (g)lmer models with a decreasing number of random
#' correlations and return the first one to converge.
#'
#' @export
lmer.slimr <- function(formula, data, family, parallel, ...) {
  mf <- match.call() # acrobatics to recover ... and names of data, etc.

  num.proper.args <- 4 # formula, data, family, parallel
  ## we need to grab all the ... arguments to pass them along to (g)lmer. The
  ## function call is mf[[1]], and there are num.proper.args proper arguments,
  ## so the ... arguments start at 1+num.proper.args+1
  first.ellipsis.arg <- num.proper.args + 2
  if (first.ellipsis.arg <= length(mf)) {
    ellipsis.args <- mf[seq(first.ellipsis.arg, length(mf))]
  } else {
    ellipsis.args <- list()
  }

  env <- parent.frame()
  formula <- formula(formula)
  possible.steps <- get.all.steps(formula, data)
  current.step <- 1
  next.to.start <- 1
  if (parallel > length(possible.steps)) {
    parallel <- length(possible.steps)
  }

  ## initialize first batch of jobs
  jobs <- list()
  for (i in seq(parallel)) {
    ## this gets evaluated with repect to the parent.frame() environment, which
    ## can't directly see functions not exported from the package, so we need to
    ## use lmer.slimr:::
    call.base <- list(quote(lmer.slimr:::lmer.check.convergence),
                      formula=possible.steps[[next.to.start]],
                      data=mf$data)
    lmer.call <- as.call(c(call.base, ellipsis.args))

    jobs[[next.to.start]] <- mcparallel(eval(lmer.call, env))
    next.to.start <- next.to.start+1
  }

  ## iteratively look at results
  for (i in seq(length(possible.steps))) {
    result <- mccollect(jobs[[i]])[[1]]
    if (!(identical(result, F))) {
      output.convergence.info(i, possible.steps[[i]])
      if (next.to.start-1 > i) {
        for (j in seq(i+1, next.to.start-1)) {
          parallel:::mckill(jobs[[j]], 15L) # 15 is code for SIGTERM
        }
      }
      return(result)
    } else if (next.to.start <= length(possible.steps)) {
      lmer.call$formula <- possible.steps[[next.to.start]]
      jobs[[next.to.start]] <- mcparallel(eval(lmer.call, env))
      next.to.start <- next.to.start + 1
    }
  }

  message("No models converged.")
  return(F)
}

# #' @export
# slmer <- function(formula, ...) {
#   ### this function just calls lmer.slimr with parallel=1
#   ## acrobatics to recover ... and formula
#   mf <- match.call() # acrobatics to recover ... and formula
#   mf[[1]] <- quote(lmer.slimr)
#   oldlength <- length(mf)
#   newlength <- oldlength+1
#   mf[4:newlength] <- mf[3:oldlength]
#   names(mf)[4:newlength] <- names(mf)[3:oldlength]
#   mf[[3]] <- 1
#   names(mf)[3] <- "parallel"
#   env <- parent.frame()
#   eval(mf, env)
# }
