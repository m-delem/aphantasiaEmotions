#' VVIQ and TAS data from various studies
#'
#' This dataset contains the responses to the Vividness of Visual Imagery
#' Questionnaire (VVIQ) and the Toronto Alexithymia Scale (TAS) collected
#' across various studies. It
#' contains the following:
#' - `study`: the study from which the data originates (see source below)
#' - `lang`: language of the study (English or French)
#' - `id`: unique identifier for each participant
#' - `sex` 
#' - `gender` (if distinct from sex)
#' - `age` 
#' - `vviq`: total score on the VVIQ
#' - `tas`: total score on the TAS
#' - `tas_identify`: Difficulty Identifying Feelings (DIF) subscale
#' - `tas_describe`: Difficulty Describing Feelings (DDF) subscale
#' - `tas_external`: Externally Oriented Thinking (EOT) subscale
#' - `tas_group`: categorical variable indicating alexithymia status
#'   (alexithymic or non-alexithymic) 
#' - `vviq_group_4`: categorical variable indicating VVIQ group with 4 levels
#'   (aphantasia, hypophantasia, typical, hyperphantasia)
#' - `vviq_group_3`: categorical variable indicating VVIQ group with 3 levels
#'   (aphantasia, hypophantasia, typical/hyperphantasia)
#' - `vviq_group_2`: categorical variable indicating VVIQ group with 2 levels
#'   (aphantasia/hypophantasia, typical/hyperphantasia)   
#' - `items`: nested list-column with individual VVIQ and TAS item responses
#'   when available
#' - `other_data`: nested list-column with other questionnaire data collected
#'   in the studies that are not used in the current analyses 
#'
#' @source Data collected by various teams in online experiments:
#' - Ale & Burns (2024): https://doi.org/10.31234/osf.io/kj5d3 
#'   (study name "burns")
#' - Monzel et al. (2024): https://doi.org/10.1016/j.bionps.2024.100106
#'   (study name "monzel")
#' - Ruby (2025): Original data collected by Perrine Ruby at the Centre de
#'   Recherche en Neurosciences de Lyon (study name "ruby") 
#' - Mas (2025): Original data collected by Marine Mas et al. at Louvain
#'   (study name "mas")
#' - Kvamme et al. (2025): https://doi.org/10.31234/osf.io/6fhj4_v1   
#'   (study name "kvamme")
"all_data"
