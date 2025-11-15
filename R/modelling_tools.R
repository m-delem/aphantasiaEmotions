fit_vviq_gam <- function(
    vd = "tas", 
    data = tas_data,
    family = stats::gaussian()
) {
  model_formula <- stats::formula(glue::glue("{vd} ~ s(vviq)"))
  model <- 
    mgcv::gam(
      formula = model_formula,
      data    = data,
      family  = family
    )
  return(model)
}

report_contrast <- function(model, formula, interaction = FALSE) {
  rlang::check_installed("emmeans", reason = "to use `report_contrast()`.")
  
  emmeans_results <- model |> 
    emmeans::emmeans(formula)
  
  contrast_results <- emmeans_results |> 
    emmeans::contrast(method = "pairwise", interaction = interaction)
  
  return(contrast_results)
}

fit_brms_model <- function(..., iterations = 4000) {
  # Set the folder to save the cmdstanr parameters
  options(cmdstanr_write_stan_file_dir = here::here("outputs/models/stan/"))
  
  # Create a folder for the models if necessary
  fs::dir_create(here::here("inst/models/"))
  
  # Parallel processing setup for 40k samples
  n_cores <- parallel::detectCores()
  n_iter  <- ceiling(iterations / n_cores) + 2000
  
  # Fit a brms model with the arguments in `...` and my default options
  brms::brm(
    ...,
    chains  = n_cores,
    cores   = n_cores,
    iter    = n_iter,
    warmup  = 2000,
    refresh = floor(n_iter / 20),
    backend = "cmdstanr",
    file_refit    = "never",
    file_compress = "xz",
    sample_prior  = TRUE,
    save_pars     = brms::save_pars(all = TRUE), 
    control       = list(adapt_delta = 0.95),
    seed = 667
  )
}