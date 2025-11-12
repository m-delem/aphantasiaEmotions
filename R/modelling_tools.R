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