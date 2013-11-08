library(parallel)
library(lme4)

#' Evaluate sequence of (g)lmer models with a decreasing number of random
#' correlations and return the first one to converge.
#'
#' @export
lmer.slimr <- function(formula, parallel, ...) {
  ### Note: this function assumes that a data argument is given
  ## acrobatics to recover ... and formula
  mf <- match.call()

  ## this gets evaluated with repect to the parent.frame() environment, which
  ## can't directly see functions not exported from the package
  mf[[1]] <- quote(lmer.slimr:::lmer.check.convergence)

  mf$parallel <- NULL
  env <- parent.frame()
  formula <- formula(formula)
  names(mf)[3] <- "data"
  possible.steps <- get.all.steps(formula, eval(mf$data))
  current.step <- 1
  next.to.start <- 1
  if (parallel > length(possible.steps)) {
    parallel <- length(possible.steps)
  }

  ## initialize first batch of jobs
  jobs <- list()
  for (i in seq(parallel)) {
    mf$formula <- possible.steps[[next.to.start]]
    jobs[[next.to.start]] <- mcparallel(eval(mf, env))
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
      mf$formula <- possible.steps[[next.to.start]]
      jobs[[next.to.start]] <- mcparallel(eval(mf, env))
      next.to.start <- next.to.start + 1
    }
  }

  message("No models converged.")
  return(F)
}

#' @export
slmer <- function(formula, ...) {
  ### this function just calls lmer.slimr with parallel=1
  ## acrobatics to recover ... and formula
  mf <- match.call()
  mf[[1]] <- quote(lmer.slimr)
  oldlength <- length(mf)
  newlength <- oldlength+1
  mf[4:newlength] <- mf[3:oldlength]
  names(mf)[4:newlength] <- names(mf)[3:oldlength]
  mf[[3]] <- 1
  names(mf)[3] <- "parallel"
  env <- parent.frame()
  eval(mf, env)
}
