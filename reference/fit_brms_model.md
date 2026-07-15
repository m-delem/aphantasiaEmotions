# Fit a Bayesian model using the brms package with default settings

Fit a Bayesian model using the brms package with default settings

## Usage

``` r
fit_brms_model(
  ...,
  chains = 4,
  iterations = 2000,
  warmup = 1000,
  cores = chains,
  refresh = 500,
  backend = "rstan",
  file_refit = "on_change",
  file_compress = "xz",
  model_folder = "models/",
  sample_prior = FALSE,
  save_pars = NULL,
  adapt_delta = 0.95,
  max_treedepth = 10,
  seed = 667
)
```

## Arguments

- ...:

  Arguments passed to brms::brm(), such as formula, data, family,
  priors, etc.

- chains:

  Number of MCMC chains. Default is 4 (standard convention: enough for
  reliable Rhat/ESS convergence diagnostics on typical models, without
  the overhead of running far more chains than needed). Increase for
  models with tricky posteriors, not as a routine choice.

- iterations:

  Number of POST-WARMUP iterations PER CHAIN (not divided by anything,
  not a total across chains). Default is 2000. Total post-warmup draws
  across all chains = iterations \* chains.

- warmup:

  Number of warmup iterations per chain. Default is 1000.

- cores:

  Number of cores to use for parallel processing. Default is `chains`,
  i.e. one core per chain (fully parallel). Set lower only if the
  machine has fewer cores than chains requested.

- refresh:

  Frequency of progress updates. Default is 500.

- backend:

  Backend to use for fitting the model. Default is "rstan".

- file_refit:

  Condition for refitting the model. Default is "on_change".

- file_compress:

  Compression method for saving the model file. Default is "xz".

- model_folder:

  Folder to save the fitted models. Default is "models/".

- sample_prior:

  Logical. If TRUE, prior samples are drawn. If "only", only prior
  samples are drawn. Default is FALSE. FALSE

- save_pars:

  Parameters to save. Default is NULL.

- adapt_delta:

  Target acceptance rate for the NUTS sampler. Default is 0.95.

- max_treedepth:

  Maximum treedepth for the NUTS sampler. Default is 10 (brms/Stan
  default). Increase (e.g. 12-15) if you see "maximum treedepth
  exceeded" warnings - this doesn't fix an underlying geometry problem,
  it just lets the sampler take more steps per iteration before giving
  up, which is often sufficient for models with awkward but not
  pathological posteriors (e.g. nonlinear/hinge models with a weakly
  identified parameter).

- seed:

  Random seed for reproducibility. Default is 667.

## Value

A fitted brms model object.
