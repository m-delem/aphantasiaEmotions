# Generates a codebook for `merged_data.xlsx` (VVIQ/TAS aphantasia-alexithymia
# dataset) in two formats from a single source of truth:
#   - dataset_description.json  (Psych-DS compliant data dictionary)
#   - codebook.md               (human-readable Markdown table)
#   
# ---------------------------------------------------------------------------- #
# 0. Dataset-level metadata ----
# ---------------------------------------------------------------------------- #

dataset_info <- list(
  name = "VVIQ and TAS-20 data across five studies (aphantasia / alexithymia)",
  description = paste(
    "Pooled participant-level data from five independent studies, combining",
    "responses to the Vividness of Visual Imagery Questionnaire (VVIQ; Marks,",
    "1973) and the 20-item Toronto Alexithymia Scale (TAS-20; Bagby, Parker,",
    "& Taylor, 1994). Used to examine the relationship between visual mental",
    "imagery vividness (including aphantasia and hyperphantasia) and",
    "alexithymia."
  ),
  sources = c(
    "Ale & Burns (2024) - https://doi.org/10.31234/osf.io/kj5d3 (study = \"burns\")",
    "Monzel et al. (2024) - https://doi.org/10.1016/j.bionps.2024.100106 (study = \"monzel\")",
    "Ruby (2025) - unpublished, Perrine Ruby, Centre de Recherche en Neurosciences de Lyon (study = \"ruby\")",
    "Mas & Luminet (2025) - unpublished, Marine Mas and Olivier Luminet, Louvain-la-Neuve (study = \"mas\")",
    "Kvamme et al. (2026) - https://doi.org/10.1016/j.neuropsychologia.2026.109368 (study = \"kvamme\")"
  ),
  instrument_sources = c(
    "VVIQ item text: Marks, D. F. (1973). Visual imagery differences in the recall of pictures. British Journal of Psychology, 64(1), 17-24. https://doi.org/10.1111/j.2044-8295.1973.tb01322.x",
    "TAS-20 item text: Bagby, R. M., Parker, J. D. A., & Taylor, G. J. (1994). The twenty-item Toronto Alexithymia Scale-I. Journal of Psychosomatic Research, 38(1), 23-32. https://doi.org/10.1016/0022-3999(94)90005-1"
  ),
  copyright_note = paste(
    "VVIQ and TAS-20 item wording is copyrighted by the original scale authors",
    "and is not reproduced here. Item columns below are identified by their",
    "official item number and (for the TAS-20) subscale; consult the source",
    "publications above for exact wording."
  )
)

# ---------------------------------------------------------------------------- #
# 1. Variable definitions ----
# ---------------------------------------------------------------------------- #
# Each variable is a named list. Common fields:
#   name         - exact column header
#   description  - plain-language explanation
#   dataType     - one of "string", "integer", "float" (Psych-DS convention)
#   unitText     - unit of measurement, if any (optional)
#   minValue/maxValue - numeric range, if applicable (optional)
#   levels       - named vector of level -> meaning, for categorical vars (optional)
#   levelsOrdered - TRUE/FALSE, whether levels above have a natural order (optional)
#   notes        - free-text caveats / details that don't fit elsewhere (optional)

variables <- list(

  list(
    name = "study",
    description = "Study of origin.",
    dataType = "string",
    levels = c(
      burns  = "Ale & Burns (2024)",
      monzel = "Monzel et al. (2024)",
      ruby   = "Ruby (2025), unpublished",
      mas    = "Mas & Luminet (2025), unpublished",
      kvamme = "Kvamme et al. (2026)"
    ),
    levelsOrdered = FALSE
  ),

  list(
    name = "lang",
    description = "Language in which the study was administered.",
    dataType = "string",
    levels = c(en = "English", fr = "French"),
    levelsOrdered = FALSE
  ),

  list(
    name = "id",
    description = paste(
      "Unique participant identifier, of the form \"subj_<study>_<number>\".",
      "Unique across the whole pooled dataset (not just within study)."
    ),
    dataType = "string"
  ),

  list(
    name = "sex",
    description = "Sex assigned at birth, as reported by the participant.",
    dataType = "string",
    levels = c(male = "Male", female = "Female", other = "Other"),
    levelsOrdered = FALSE,
    notes = paste(
      "Distinct from `gender` below. For most participants sex and gender",
      "coincide; the Burns study collected both constructs separately and a",
      "small number of participants (n = 4) reported a sex and gender that",
      "differ."
    )
  ),

  list(
    name = "gender",
    description = "Gender identity, as reported by the participant, where collected separately from sex.",
    dataType = "string",
    levels = c(male = "Male", female = "Female", other = "Other"),
    levelsOrdered = FALSE
  ),

  list(
    name = "age",
    description = "Participant age in years, self-reported.",
    dataType = "integer",
    unitText = "years",
    minValue = 10,
    maxValue = 86,
    notes = "Missing for 3 participants."
  ),

  list(
    name = "vviq",
    description = paste(
      "Total score on the Vividness of Visual Imagery Questionnaire (VVIQ;",
      "Marks, 1973), summed across all 16 items (see `vviq_q1`-`vviq_q16`",
      "below). Lower scores indicate weaker/absent visual imagery."
    ),
    dataType = "integer",
    minValue = 16,
    maxValue = 80
  ),

  list(
    name = "tas",
    description = paste(
      "Total score on the 20-item Toronto Alexithymia Scale (TAS-20; Bagby,",
      "Parker, & Taylor, 1994), summed across all 20 items (see `tas_q1`-",
      "`tas_q20` below, already reverse-scored where applicable). Higher",
      "scores indicate greater alexithymia."
    ),
    dataType = "integer",
    minValue = 20,
    maxValue = 100
  ),

  list(
    name = "tas_identify",
    description = paste(
      "Difficulty Identifying Feelings (DIF) subscale of the TAS-20.",
      "Sum of items 1, 3, 6, 7, 9, 13, 14 (7 items)."
    ),
    dataType = "integer",
    minValue = 7,
    maxValue = 35
  ),

  list(
    name = "tas_describe",
    description = paste(
      "Difficulty Describing Feelings (DDF) subscale of the TAS-20.",
      "Sum of items 2, 4, 11, 12, 17 (5 items)."
    ),
    dataType = "integer",
    minValue = 5,
    maxValue = 25
  ),

  list(
    name = "tas_external",
    description = paste(
      "Externally Oriented Thinking (EOT) subscale of the TAS-20.",
      "Sum of items 5, 8, 10, 15, 16, 18, 19, 20 (8 items)."
    ),
    dataType = "integer",
    minValue = 8,
    maxValue = 40
  ),

  list(
    name = "tas_group",
    description = "Categorical alexithymia status based on standard TAS-20 cutoff (total score >= 61 = alexithymia).",
    dataType = "string",
    levels = c(
      typical_tas = "Total TAS-20 score 20-60 (non-alexithymic)",
      alexithymia = "Total TAS-20 score 61-100 (alexithymic)"
    ),
    levelsOrdered = TRUE
  ),

  list(
    name = "vviq_group_4",
    description = "Categorical VVIQ group with 4 levels, based on standard VVIQ cutoffs.",
    dataType = "string",
    levels = c(
      aphantasia     = "VVIQ = 16 (complete absence of visual imagery)",
      hypophantasia  = "VVIQ 17-32 (reduced visual imagery)",
      typical        = "VVIQ 33-74 (typical visual imagery)",
      hyperphantasia = "VVIQ 75-80 (extremely vivid visual imagery)"
    ),
    levelsOrdered = TRUE
  ),

  list(
    name = "vviq_group_3",
    description = "Categorical VVIQ group with 3 levels (typical and hyperphantasia collapsed).",
    dataType = "string",
    levels = c(
      aphantasia    = "VVIQ = 16",
      hypophantasia = "VVIQ 17-32",
      typical       = "VVIQ 33-80 (typical and hyperphantasia collapsed)"
    ),
    levelsOrdered = TRUE
  ),

  list(
    name = "vviq_group_2",
    description = "Categorical VVIQ group with 2 levels (broadest split).",
    dataType = "string",
    levels = c(
      aphantasia = "VVIQ 16-32 (aphantasia and hypophantasia collapsed)",
      typical    = "VVIQ 33-80 (typical and hyperphantasia collapsed)"
    ),
    levelsOrdered = TRUE
  )
)

# ---------------------------------------------------------------------------- #
# 1a. VVIQ item columns (vviq_q1-vviq_q16) ----
# ---------------------------------------------------------------------------- #
# The VVIQ has no sub-scales; its 16 items are organised into 4 scenes of 4
# items each (familiar person, sunrise, shop, natural scene), each rated
# 1 (perfectly clear and vivid) to 5 (no image at all). Item text is not
# reproduced here for copyright reasons (see instrument_sources above).

vviq_scenes <- c(
  "a relative or friend you often see",
  "the rising sun",
  "the front of a familiar shop",
  "a country scene with trees, mountains, and a lake"
)

vviq_items <- lapply(1:16, function(i) {
  scene_index <- ceiling(i / 4)
  list(
    name = paste0("vviq_q", i),
    description = paste0(
      "VVIQ item ", i, " of 16, from the scene \"", vviq_scenes[scene_index],
      "\" (items ", (scene_index - 1) * 4 + 1, "-", scene_index * 4,
      " share this scene). Item wording not reproduced; see Marks (1973)."
    ),
    dataType = "integer",
    minValue = 1,
    maxValue = 5,
    notes = paste(
      "1 = perfectly clear and as vivid as normal vision;",
      "5 = no image at all. Missing for all 105 participants from the",
      "Monzel study, for whom only total/subscale scores were shared."
    )
  )
})

# ---------------------------------------------------------------------------- #
# 1b. TAS-20 item columns (tas_q1-tas_q20) ----
# ---------------------------------------------------------------------------- #
# Subscale assignment and reverse-scored items per Bagby, Parker, & Taylor
# (1994). Item wording not reproduced here for copyright reasons.

tas_subscale_of <- function(i) {
  if (i %in% c(1, 3, 6, 7, 9, 13, 14)) return("DIF (Difficulty Identifying Feelings)")
  if (i %in% c(2, 4, 11, 12, 17)) return("DDF (Difficulty Describing Feelings)")
  if (i %in% c(5, 8, 10, 15, 16, 18, 19, 20)) return("EOT (Externally Oriented Thinking)")
  stop("Unexpected TAS item number: ", i)
}

tas_reverse_scored <- c(4, 5, 10, 18, 19)

tas_items <- lapply(1:20, function(i) {
  reversed_note <- if (i %in% tas_reverse_scored) {
    "This item is reverse-keyed on the original scale; values in this column are already reverse-scored (i.e. summing tas_q1-tas_q20 directly reproduces `tas`, no further reversal needed)."
  } else {
    NULL
  }
  list(
    name = paste0("tas_q", i),
    description = paste0(
      "TAS-20 item ", i, " of 20, ", tas_subscale_of(i),
      " subscale. Item wording not reproduced; see Bagby, Parker, & Taylor (1994)."
    ),
    dataType = "integer",
    minValue = 1,
    maxValue = 5,
    notes = paste(
      c("1 = strongly disagree; 5 = strongly agree.",
        reversed_note,
        "Missing for all 105 participants from the Monzel study, for whom only total/subscale scores were shared."),
      collapse = " "
    )
  )
})

all_variables <- c(variables, vviq_items, tas_items)

# ---------------------------------------------------------------------------- #
# 2. Render: Psych-DS dataset_description.json ----
# ---------------------------------------------------------------------------- #

variable_to_property_value <- function(v) {
  pv <- list(
    `@type` = "PropertyValue", 
    name = v$name, 
    description = v$description
  )

  if (!is.null(v$dataType)) pv$dataType <- v$dataType
  if (!is.null(v$unitText)) pv$unitText <- v$unitText
  if (!is.null(v$minValue)) pv$minValue <- v$minValue
  if (!is.null(v$maxValue)) pv$maxValue <- v$maxValue

  if (!is.null(v$levels)) {
    pv$levels <- as.list(v$levels)
    pv$levelsOrdered <- isTRUE(v$levelsOrdered)
  }

  if (!is.null(v$notes)) pv$notes <- v$notes

  pv
}

write_json_codebook <- function(all_variables, dataset_info, path) {
  property_values <- lapply(all_variables, variable_to_property_value)

  doc <- list(
    `@context` = "https://schema.org/",
    `@type` = "Dataset",
    name = dataset_info$name,
    description = dataset_info$description,
    schemaVersion = "Psych-DS 0.4.0",
    citation = as.list(dataset_info$sources),
    creditText = as.list(dataset_info$instrument_sources),
    usageInfo = dataset_info$copyright_note,
    variableMeasured = property_values
  )

  json_text <- jsonlite::toJSON(doc, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(json_text, path)
  invisible(json_text)
}

# ---------------------------------------------------------------------------- #
# 3. Render: human-readable Markdown codebook ----
# ---------------------------------------------------------------------------- #

# Markdown table cells break on literal "|" and newlines, so escape/strip them.
md_escape <- function(x) {
  if (is.null(x)) return("")
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("\\r?\\n", " ", x)
  x
}

format_levels_md <- function(levels) {
  if (is.null(levels)) return("")
  paste(
    sprintf("`%s` = %s", names(levels), md_escape(unname(levels))),
    collapse = "; "
  )
}

format_range_md <- function(v) {
  if (!is.null(v$minValue) && !is.null(v$maxValue)) {
    unit <- if (!is.null(v$unitText)) paste0(" ", v$unitText) else ""
    return(sprintf("%s-%s%s", v$minValue, v$maxValue, unit))
  }
  ""
}

md_table_rows <- function(vars) {
  rows <- vapply(vars, function(v) {
    levels_txt <- format_levels_md(v$levels)
    range_txt <- format_range_md(v)
    notes_txt <- if (!is.null(v$notes)) md_escape(v$notes) else ""
    extra <- paste(c(levels_txt, notes_txt), collapse = " ")
    extra <- trimws(extra)

    sprintf(
      "| `%s` | %s | %s | %s | %s |",
      v$name, md_escape(v$description), v$dataType, range_txt, extra
    )
  }, character(1))
  paste(rows, collapse = "\n")
}

write_markdown_codebook <- function(variables, vviq_items, tas_items, dataset_info, path) {

  header <- c(
    paste0("# Codebook: ", dataset_info$name),
    "",
    dataset_info$description,
    "",
    "## Sources",
    "",
    paste0("- ", dataset_info$sources),
    "",
    "## Instrument references and copyright notice",
    "",
    dataset_info$copyright_note,
    "",
    paste0("- ", dataset_info$instrument_sources),
    "",
    "## Variables",
    "",
    "### Core variables",
    "",
    "| Variable | Description | Type | Range | Levels / notes |",
    "|---|---|---|---|---|",
    md_table_rows(variables),
    "",
    "### VVIQ item-level responses (`vviq_q1`-`vviq_q16`)",
    "",
    paste(
      "The VVIQ has no subscales; its 16 items are grouped into 4 scenes of",
      "4 items each (familiar person, sunrise, shop, natural scene), each",
      "rated 1 (perfectly clear and vivid) to 5 (no image at all). Item",
      "wording is not reproduced here for copyright reasons \u2014 see Marks",
      "(1973) for the original items. **Available for the burns, mas, ruby,",
      "and kvamme studies only; missing (NA) for all 105 monzel",
      "participants**, for whom only total/subscale scores were shared."
    ),
    "",
    "| Variable | Description | Type | Range | Notes |",
    "|---|---|---|---|---|",
    md_table_rows(vviq_items),
    "",
    "### TAS-20 item-level responses (`tas_q1`-`tas_q20`)",
    "",
    paste(
      "20 items rated 1 (strongly disagree) to 5 (strongly agree), across",
      "three subscales (DIF, DDF, EOT; see core variables above). Item",
      "wording is not reproduced here for copyright reasons \u2014 see Bagby,",
      "Parker, & Taylor (1994) for the original items.",
      "**Values in these columns are already reverse-scored for the 5",
      "reverse-keyed items (4, 5, 10, 18, 19)** \u2014 summing `tas_q1`-`tas_q20`",
      "directly reproduces the `tas` total column, with no further reversal",
      "needed. Available for the burns, mas, ruby, and kvamme studies only;",
      "missing (NA) for all 105 monzel participants."
    ),
    "",
    "| Variable | Description | Type | Range | Notes |",
    "|---|---|---|---|---|",
    md_table_rows(tas_items),
    ""
  )

  writeLines(header, path)
  invisible(header)
}

# ---------------------------------------------------------------------------- #
# 4. Run ----
# ---------------------------------------------------------------------------- #

write_json_codebook(all_variables, dataset_info, "data-raw/dataset_description.json")
write_markdown_codebook(variables, vviq_items, tas_items, dataset_info, "data-raw/codebook.md")

cat("Wrote dataset_description.json and codebook.md\n")
