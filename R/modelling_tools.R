#' Fit a Bayesian model using the brms package with default settings
#'
#' @param ... Arguments passed to brms::brm(), such as formula, data, family,
#' priors, etc.
#' @param iterations Total number of iterations. This number is divided by the
#' number of cores for parallel processing. Default is 20000 (40k recommended
#' if Bayes Factors are needed).
#' @param warmup Number of warmup iterations added for each chain. Default is
#' 2000.
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
#' @param seed Random seed for reproducibility. Default is 667.
#'
#' @returns A fitted brms model object.
#' @export
fit_brms_model <- function(
  ...,
  iterations = 24000, # 40k recommended if BFs needed
  warmup = 2000,
  refresh = 500,
  backend = "rstan", # or rstan, cmdstanr conflicts with pkgdown
  file_refit = "on_change",
  file_compress = "xz",
  model_folder = "models/",
  sample_prior = FALSE, # TRUE if BFs needed
  save_pars = NULL, # brms::save_pars(all = TRUE) if BFs needed
  adapt_delta = 0.95,
  seed = 667
) {
  # rlang::check_installed("fs", reason = "to create folders")

  # Set the folder to save the cmdstanr parameters
  # options(cmdstanr_write_stan_file_dir = paste0(model_folder, "stan/"))

  # Create a folder for the models if necessary
  # fs::dir_create(model_folder)

  # Parallel processing setup for 40k samples
  n_cores <- parallel::detectCores()
  n_iter <- ceiling(iterations / n_cores) + warmup

  # Fit a brms model with the arguments in `...` and my default options
  brms::brm(
    ...,
    chains = n_cores,
    cores = n_cores,
    iter = n_iter,
    warmup = warmup,
    refresh = refresh,
    backend = backend,
    file_refit = file_refit,
    file_compress = file_compress,
    sample_prior = sample_prior,
    save_pars = save_pars,
    control = list(adapt_delta = adapt_delta),
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

  range <- bayestestR::rope_range(attr(marg_effects, "marginaleffects")@model)

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
      PD = bayestestR::p_direction(.data$draw)$pd |> round(digits),
      "Below ROPE" = mean(.data$draw < range[1]) |> round(digits),
      "Inside ROPE" = mean(.data$draw > range[1] & .data$draw < range[2]) |>
        round(digits),
      "Above ROPE" = mean(.data$draw > range[2]) |> round(digits)
    ) |>
    dplyr::ungroup()

  return(rope_report)
}
