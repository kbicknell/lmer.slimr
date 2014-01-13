\name{glmer.slimr}
\alias{glmer.slimr}
\title{Evaluate sequence of glmer models with a decreasing number of random
correlations and return the first one to converge.}
\usage{
glmer.slimr(formula, data, family, parallel = getOption("mc.cores", 2L),
  suppress.warnings = F, simplest.only = F, ...)
}
\description{
Evaluate sequence of glmer models with a decreasing number
of random correlations and return the first one to
converge.
}

