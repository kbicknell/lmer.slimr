\docType{package}
\name{lmer.slimr}
\alias{lmer.slimr}
\alias{lmer.slimr-package}
\title{Evaluate sequence of lmer models with a decreasing number of random
correlations and return the first one to converge.}
\usage{
  lmer.slimr(formula, data,
    parallel = getOption("mc.cores", 2L),
    show.warnings = F, simplest.only = F, ...)
}
\description{
  Evaluate sequence of lmer models with a decreasing number
  of random correlations and return the first one to
  converge.

  lmer.slimr: iteratively slim down (g)lmer models
}

