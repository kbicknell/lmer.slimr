#' Returns a (g)lmer model if it converges, and a warning or error otherwise
#'
#' @importFrom lme4 lmer glmer
lmer.check.convergence <- function(formula, data, family="gaussian", ...) {
  ## We have to do acrobatics to see the evaluated ... in the model output
  matched.call <- match.call()
  env <- parent.frame()
  ellipsis.args <- get.ellipsis.args(formals(), matched.call)

  if (family == "gaussian") {
    call.base <- list(quote(lme4::lmer),
                      formula=matched.call$formula,
                      data=matched.call$data)
  } else {
    call.base <- list(quote(lme4::glmer),
                      formula=matched.call$formula,
                      data=matched.call$data,
                      family=family)
  }
  lmer.call <- as.call(c(call.base, ellipsis.args))

  m <- tryCatch({
    eval(lmer.call, env)
  }, warning = function(w) {
    return(list(w, m))
  }, error = function(e) {
    return(list(e, m))
  })
  return(list(T, m))
}

output.convergence.info <- function(return.step, formula) {
  if (return.step == 1) {
    message("Full model converged!")
  } else {
    message(paste("Full model did not converge. Had to change formula by",
                  return.step-1, "steps."))
  }
}
