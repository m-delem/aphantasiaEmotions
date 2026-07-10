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
#' exceeded" warnings — this doesn't fix an underlying geometry problem, it
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