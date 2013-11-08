lmer.check.convergence <- function(...) {
  ## We have to do acrobatics to see the evaluated ... in the model output
  mf <- match.call()
  mf[[1]] <- quote(lmer)
  env <- parent.frame()
  mf[[2]] <- eval(mf[[2]], env) # eval formula
  op <- options(warn=2)
  on.exit(options(op))
  m <- try(eval(mf, env), silent=T)
  if (inherits(m, "try-error")) {
    return(F)
  } else {
    return(m)
  }
}

output.convergence.info <- function(return.step, formula) {
  if (return.step == 1) {
    message("Full model converged!")
  } else {
    message(paste("Full model did not converge. Had to change formula by",
                  return.step-1, "steps."))
  }
}
