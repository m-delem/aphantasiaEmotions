report_contrast <- function(model, formula, interaction = FALSE) {
  rlang::check_installed("emmeans", reason = "to use `report_contrast()`.")
  
  emmeans_results <- model |> 
    emmeans::emmeans(formula)
  
  contrast_results <- emmeans_results |> 
    emmeans::contrast(method = "pairwise", interaction = interaction)
  
  return(contrast_results)
}