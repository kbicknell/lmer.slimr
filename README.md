# lmer.slimr

Often `(g)lmer` models (from the `lme4` package) with a large number
of random correlations to estimate will fail to converge. The
`lmer.slimr` package provides functions that iteratively simplify the
random correlation matrix until convergence. In order to make this
process faster, it can evaluate this sequence of models in parallel.

In particular, `lmer.slimr`:

 * Begins evaluation with the full user-specified random effect
   specification.

 * In the case where this model doesn't converge, removes random
   correlations until it find a model that converges.

The easiest way to install is to run

    install_github('kbicknell/lmer.slimr')

after loading the `devtools` library.

If you want to take advantage of the functionality to use the old
version of `lme4` (`lme4.0`), you'll need to install that first like so:

    install.packages("lme4.0", repos = c("http://lme4.r-forge.r-project.org/repos", getOption("repos")))
