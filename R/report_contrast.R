# pacman allows to check, install and load packages with a single call
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(emmeans)

report_contrast <- function(model, formula, interaction = FALSE) {
  emmeans_results <- model |> 
    emmeans(formula)
  
  contrast_results <- emmeans_results |> 
    contrast(method = "pairwise", interaction = interaction)
  
  return(contrast_results)
}