#' Returns a (g)lmer model if it converges, and a warning or error otherwise
#'
#' @importFrom lme4 lmer glmer
lmer.check.convergence <- function(formula, data, family="gaussian",
                                   use.old.lme4=F, ...) {
  ## We have to do acrobatics to see the evaluated ... in the model output
  matched.call <- match.call()
  env <- parent.frame()
  ellipsis.args <- get.ellipsis.args(formals(), matched.call)
  if (use.old.lme4) {
    try_require("lme4.0")
    if (family == "gaussian") {
      lme4.command <- quote(lme4.0::lmer)
    } else {
      lme4.command <- quote(lme4.0::glmer)
    }
  } else {
    if (family == "gaussian") {
      lme4.command <- quote(lme4::lmer)
    } else {
      lme4.command <- quote(lme4::glmer)
    }
  }

  if (family == "gaussian") {
    call.base <- list(lme4.command,
                      formula=matched.call$formula,
                      data=matched.call$data)
  } else {
    call.base <- list(lme4.command,
                      formula=matched.call$formula,
                      data=matched.call$data,
                      family=family)
  }
  lmer.call <- as.call(c(call.base, ellipsis.args))

  m <- tryCatch({
    withWarnings(eval(lmer.call, env))
  }, error = function(e) {
    return(e)
  })
  return(m)
}

output.convergence.info <- function(return.step, formula) {
  if (return.step == 1) {
    message("Full model converged!")
  } else {
    message(paste("Full model did not converge. Had to change formula by",
                  return.step-1, "steps."))
  }
}

withWarnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = myWarnings)
}
