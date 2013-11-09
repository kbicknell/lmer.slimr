library(parallel)
library(lme4)

#' Evaluate sequence of lmer models with a decreasing number of random
#' correlations and return the first one to converge.
#'
#' @export
lmer.slimr <- function(formula, data,
                       parallel=getOption("mc.cores", 2L),
                       show.warnings=F, simplest.only=F, ...) {
  matched.call <- match.call()
  env <- parent.frame()
  if ("family" %in% names(matched.call)) {
    stop("lmer.slimr is only for linear models, but 'family' is specified. ",
         "Use glmer.slimr instead.")
  }
  matched.call[[1]] <- quote(lmer.slimr.core)
  eval(matched.call, env)
}

#' Evaluate sequence of glmer models with a decreasing number of random
#' correlations and return the first one to converge.
#'
#' @export
glmer.slimr <- function(formula, data, family,
                        parallel=getOption("mc.cores", 2L),
                        show.warnings=F, simplest.only=F, ...) {
  if (missing(family)) {
    stop("glmer.slimr requires that 'family' be specified. ",
         "Use lmer.slimr for linear models.")
  }
  matched.call <- match.call()
  env <- parent.frame()
  matched.call[[1]] <- quote(lmer.slimr.core)
  eval(matched.call, env)
}

lmer.slimr.core <- function(formula, data, family="gaussian",
                            parallel=getOption("mc.cores", 2L),
                            show.warnings=F, simplest.only=F, ...) {
  matched.call <- match.call() # acrobatics to recover ..., names of data, etc.
  env <- parent.frame()
  ellipsis.args <- get.ellipsis.args(formals(), matched.call)

  possible.steps <- get.all.steps(formula, data)
  if (simplest.only) {
    possible.steps <- possible.steps[length(possible.steps)]
  }
  num.steps <- length(possible.steps)
  next.to.start <- 1
  parallel <- min(parallel, num.steps)

  ## initialize first batch of jobs
  jobs <- list()
  for (i in seq(parallel)) {
    ## this gets evaluated with repect to the parent.frame() environment, which
    ## can't directly see functions not exported from the package, so we need to
    ## use lmer.slimr:::
    call.base <- list(quote(lmer.slimr:::lmer.check.convergence),
                      formula=possible.steps[[next.to.start]],
                      data=matched.call$data, family=family)
    lcc.call <- as.call(c(call.base, ellipsis.args))

    jobs[[next.to.start]] <- mcparallel(eval(lcc.call, env))
    next.to.start <- next.to.start + 1
  }

  ## iteratively look at results
  for (i in seq_len(num.steps)) {
    result <- mccollect(jobs[[i]])[[1]]
    if (is.list(result)) { # returned warning or error
      if ("error" %in% class(result)) { # returned error
        stop(result)
      }
      if (show.warnings) {
        warning(result)
      }
      if (next.to.start <= num.steps) {
        lcc.call$formula <- possible.steps[[next.to.start]]
        jobs[[next.to.start]] <- mcparallel(eval(lcc.call, env))
        next.to.start <- next.to.start + 1
      }
    } else { # it converged
      output.convergence.info(i, possible.steps[[i]])
      if (next.to.start-1 > i) {
        for (j in seq(i+1, next.to.start-1)) {
          parallel:::mckill(jobs[[j]], 15L) # 15 is code for SIGTERM
        }
      }
      return(result)
    }
  }

  message("No models converged.")
  return(F)
}

# #' @export
# slmer <- function(formula, ...) {
#   ### this function just calls lmer.slimr with parallel=1
#   ## acrobatics to recover ... and formula
#   matched.call <- match.call() # acrobatics to recover ... and formula
#   matched.call[[1]] <- quote(lmer.slimr)
#   oldlength <- length(matched.call)
#   newlength <- oldlength+1
#   matched.call[4:newlength] <- matched.call[3:oldlength]
#   names(matched.call)[4:newlength] <- names(matched.call)[3:oldlength]
#   matched.call[[3]] <- 1
#   names(matched.call)[3] <- "parallel"
#   env <- parent.frame()
#   eval(matched.call, env)
# }
