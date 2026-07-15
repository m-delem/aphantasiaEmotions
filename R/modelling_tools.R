#' Fit a Bayesian model using the brms package with default settings
#'
#' @param ... Arguments passed to brms::brm(), such as formula, data, family,
#' priors, etc.
#' @param chains Number of MCMC chains. Default is 4 (standard convention:
#' enough for reliable Rhat/ESS convergence diagnostics on typical models,
#' without the overhead of running far more chains than needed). Increase for
#' models with tricky posteriors, not as a routine choice.
#' @param iterations Number of POST-WARMUP iterations PER CHAIN (not divided
#' by anything, not a total across chains). Default is 2000. Total post-warmup
#' draws across all chains = iterations * chains.
#' @param warmup Number of warmup iterations per chain. Default is 1000.
#' @param cores Number of cores to use for parallel processing. Default is
#' `chains`, i.e. one core per chain (fully parallel). Set lower only if the
#' machine has fewer cores than chains requested.
#' @param refresh Frequency of progress updates. Default is 500.
#' @param backend Backend to use for fitting the model. Default is "rstan".
#' @param file_refit Condition for refitting the model. Default is "on_change".
#' @param file_compress Compression method for saving the model file. Default is
#' "xz".
#' @param model_folder Folder to save the fitted models. Default is "models/".
#' @param sample_prior Logical. If TRUE, prior samples are drawn. If "only",
#' only prior samples are drawn. Default is FALSE.
#' FALSE
#' @param save_pars Parameters to save. Default is NULL.
#' @param adapt_delta Target acceptance rate for the NUTS sampler. Default is
#' 0.95.
#' @param max_treedepth Maximum treedepth for the NUTS sampler. Default is 10
#' (brms/Stan default). Increase (e.g. 12-15) if you see "maximum treedepth
#' exceeded" warnings - this doesn't fix an underlying geometry problem, it
#' just lets the sampler take more steps per iteration before giving up,
#' which is often sufficient for models with awkward but not pathological
#' posteriors (e.g. nonlinear/hinge models with a weakly identified parameter).
#' @param seed Random seed for reproducibility. Default is 667.
#'
#' @returns A fitted brms model object.
#' @export
fit_brms_model <- function(
    ...,
    chains = 4,
    iterations = 2000, # post-warmup draws PER CHAIN
    warmup = 1000,
    cores = chains,
    refresh = 500,
    backend = "rstan", # or rstan, cmdstanr conflicts with pkgdown
    file_refit = "on_change",
    file_compress = "xz",
    model_folder = "models/",
    sample_prior = FALSE, # TRUE if BFs needed
    save_pars = NULL, # brms::save_pars(all = TRUE) if BFs needed
    adapt_delta = 0.95,
    max_treedepth = 10,
    seed = 667
) {
  # rlang::check_installed("fs", reason = "to create folders")
  
  # Set the folder to save the cmdstanr parameters
  # options(cmdstanr_write_stan_file_dir = paste0(model_folder, "stan/"))
  
  # Create a folder for the models if necessary
  # fs::dir_create(model_folder)
  
  n_iter <- iterations + warmup
  
  # Fit a brms model with the arguments in `...` and my default options
  brms::brm(
    ...,
    chains = chains,
    cores = cores,
    iter = n_iter,
    warmup = warmup,
    refresh = refresh,
    backend = backend,
    file_refit = file_refit,
    file_compress = file_compress,
    sample_prior = sample_prior,
    save_pars = save_pars,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    seed = seed
  )
}


#' Check that a segmented model's cached knot still matches a fresh MARS estimate
#'
#' The segmented `brms` model's breakpoint prior is seeded from a fast,
#' frequentist MARS knot search (`earth::earth()`), then the model is fit once
#' and cached to disk with `file_refit = "never"` (see
#' [fit_brms_model()] and the "Continuous alternatives" section of
#' `vignette("model-comparison", package = "aphantasiaEmotions")`). Because the
#' cached model is never automatically refitted, it can silently go stale if
#' the underlying data changes - the site would keep reporting a knot fit
#' around old data without any visible sign that anything is wrong.
#'
#' This function is a safeguard against exactly that. It re-runs the cheap
#' `earth::earth()` search on the data supplied *now*, and compares the knot
#' it finds against `seed_knot` - the knot value that was actually used to
#' seed the prior for the cached model at the time it was fit (**not** the
#' brms model's own posterior estimate of the knot, which is expected to
#' differ from the earth seed even on unchanged data, since brms re-estimates
#' it from a prior rather than reproducing earth's point estimate exactly).
#' If the two don't match exactly, the underlying data has almost certainly
#' changed since the model was cached, and the model should be refit.
#'
#' @param data The data the live model-comparison page is using right now
#' (e.g. `all_data`).
#' @param seed_knot The knot value that was used to seed the cached model's
#' prior when it was originally fit - a fixed constant recorded at fit time,
#' not read from the model object itself.
#' @param formula Formula passed to `earth::earth()`. Default is
#' `tas ~ vviq`, matching the segmented model's own formula.
#'
#' @returns Invisibly, the freshly-computed knot value, if it matches
#' `seed_knot`. Errors loudly otherwise.
#'
#' @export
check_knot_still_matches <- function(
    data,
    seed_knot,
    formula = tas ~ vviq
) {
  rlang::check_installed("earth", reason = "to re-run the MARS knot search")

  fresh_fit  <- earth::earth(formula, data = data)
  fresh_cuts <- fresh_fit$cuts

  predictor <- all.vars(formula[[3]])
  if (!predictor %in% colnames(fresh_cuts)) {
    stop(
      "check_knot_still_matches(): predictor '", predictor,
      "' not found among earth's cut columns. Was the formula changed?",
      call. = FALSE
    )
  }

  nonzero_cuts <- fresh_cuts[, predictor]
  nonzero_cuts <- nonzero_cuts[nonzero_cuts != 0]

  if (length(nonzero_cuts) == 0) {
    stop(
      "check_knot_still_matches(): earth found no knot for '", predictor,
      "' on this data (0 non-zero cuts) - cannot compare against seed_knot.",
      call. = FALSE
    )
  }

  # In the ordinary single-predictor case this is one value repeated across
  # the hinge pair; taking the first is safe, but flag if that assumption
  # ever stops holding (e.g. formula changed to include more terms).
  fresh_knot <- unname(nonzero_cuts[1])

  if (!isTRUE(all.equal(fresh_knot, seed_knot))) {
    stop(
      "check_knot_still_matches(): a fresh earth::earth() search finds a ",
      "knot at ", fresh_knot, ", but the cached segmented model was seeded ",
      "with a knot at ", seed_knot, ". The underlying data has likely ",
      "changed since the model was fit. Refit and re-cache the segmented ",
      "model (see the 'Continuous alternatives' section of the ",
      "model-comparison vignette) before trusting this page's results.",
      call. = FALSE
    )
  }

  invisible(fresh_knot)
}


#' Report the ROPE analysis for marginal effects
#'
#' @param marg_effects A marginaleffects object obtained with
#' [marginaleffects::avg_comparisons()] containing the contrasts to analyse.
#' @param ... Grouping variables for summarising the ROPE results.
#' @param digits Number of decimal places to round the results. Default is 3.
#'
#' @returns A data frame summarising the ROPE analysis with the estimates,
#' 95% CIs, and proportions of draws within, below, and above the ROPE.
#' @export
report_rope <- function(
    marg_effects,
    ...,
    digits = 3
) {
  rlang::check_installed("bayestestR", reason = "to compute ROPE ranges")
  rlang::check_installed("marginaleffects", reason = "to extract draws")
  
  model <- attr(marg_effects, "marginaleffects")@model
  range <- bayestestR::rope_range(model)
  sigma <- stats::sd(model$data[, 1])
  
  rope_report <-
    marg_effects |>
    marginaleffects::posterior_draws() |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      Estimate = unique(.data$estimate) |> round(digits),
      "95% CI" = paste0(
        "[",
        round(unique(.data$conf.low), digits),
        ", ",
        round(unique(.data$conf.high), digits),
        "]"
      ),
      d = abs(unique(.data$estimate) / sigma) |> round(2),
      PD = bayestestR::p_direction(.data$draw)$pd |> round(digits),
      "Below ROPE" = mean(.data$draw < range[1]) |> round(digits),
      "Inside ROPE" = mean(.data$draw > range[1] & .data$draw < range[2]) |>
        round(digits),
      "Above ROPE" = mean(.data$draw > range[2]) |> round(digits)
    ) |>
    dplyr::ungroup()
  
  return(rope_report)
}