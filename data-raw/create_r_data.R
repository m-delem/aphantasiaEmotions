pacman::p_load(dplyr, here, openxlsx, readr, readxl, tidyr)

# Burns -------------------------------------------------------------
df_burns_raw <- 
  bind_cols(
    read_xlsx(here("data-raw/data_burns.xlsx")),
    read_xlsx(here("data-raw/data_burns_items.xlsx")) |> 
      select("VVIQ1":"TAS20", "tas_excel_2_initial" = "TAS") |> 
      rename_with(.fn = stringr::str_to_lower)
  ) |> 
  mutate(
    study = "burns",
    id = paste0("subj_burns_", row_number()),
    sex = as.numeric(.data$SexAtBirth),
    gender = as.numeric(.data$CurrentGender),
    tas4_rev = 6 - tas4,
    tas5_rev = 6 - tas5,
    tas10_rev = 6 - tas10,
    tas18_rev = 6 - tas18,
    tas19_rev = 6 - tas19,
  ) |> 
  select(
    "study", "id", "sex", "gender",
    "age"  = "Age",
    "vviq" = "VVIQ",
    "tas_excel_1"  = "TAS",
    "tas1":"tas_excel_2_initial",
    contains("_rev"),
    everything()
  ) |>
  rowwise() |> 
  mutate(
    tas_excel_2_raw_sum = sum(c_across("tas1":"tas20")),
    tas_excel_2_with_rev = sum(
      c_across(
        c(
          "tas1", "tas2", "tas3", "tas4_rev", "tas5_rev",
          "tas6", "tas7", "tas8", "tas9", "tas10_rev",
          "tas11", "tas12", "tas13", "tas14", "tas15",
          "tas16", "tas17", "tas18_rev", "tas19_rev", "tas20"
        )
      )
    ),
  ) |> 
  relocate(contains("excel"), .after = "vviq") |> 
  ungroup()

openxlsx::write.xlsx(
  x          = df_burns_raw,
  file       = here("data-raw/data_burns_merged.xlsx"),
  asTable    = TRUE,
  colNames   = TRUE,
  colWidths  = "auto",
  borders    = "all",
  tableStyle = "TableStyleMedium16"
)

df_burns <-
  df_burns_raw |> 
  rowwise() |> 
  mutate(
    tas_identify = sum(
      c_across(c("tas1", "tas3", "tas6", "tas7", "tas9", "tas13", "tas14"))
    ),
    tas_describe = sum(
      c_across(c("tas2", "tas4_rev", "tas11", "tas12", "tas17"))
    ),
    tas_external = sum(
      c_across(
        c(
          "tas5_rev", "tas8", "tas10_rev", "tas15",
          "tas16", "tas18_rev", "tas19_rev", "tas20"
        )
      )
    )
  ) |> 
  select(
    "study", "id", "sex", "gender", "age", "vviq",
    "tas"  = "tas_excel_2_with_rev",
    "tas_identify":"tas_external"
  ) |> 
  ungroup()

# Monzel -------------------------------------------------------------
df_monzel <- 
  read_xlsx(
    here("data-raw/data_monzel.xlsx"), 
    sheet = "raw_data"
    # sheet = "data_matched"
  ) |> 
  mutate(
    study = "monzel",
    id = paste0("subj_monzel_", row_number()), 
    sex = .data$gender
  ) |> 
  rename_with(
    .fn = ~ stringr::str_replace_all(.x, stringr::fixed("."), "_"),
    .cols = contains(".")
  ) |> 
  select(
    "study", "id", "sex", "gender", "age",
    "vviq" = "vviq_score",
    "tas"  = "tas_score",
    "tas_identify":"tas_external"
  )

# Ruby -------------------------------------------------------------
df_ruby_raw <- 
  read_xlsx(here("data-raw/data_ruby.xlsx")) |> 
  select(
    "age" = 1,
    "sex" = 2,
    "vviq_q1" = 3,
    "vviq_q2" = 4,
    "vviq_q3" = 5,
    "vviq_q4" = 6,
    # "vviq_scale_1" = 7,
    "vviq_q5" = 8,
    "vviq_q6" = 9,
    "vviq_q7" = 10,
    "vviq_q8" = 11,
    # "vviq_scale_2" = 12,
    "vviq_q9" = 13,
    "vviq_q10" = 14,
    "vviq_q11" = 15,
    "vviq_q12" = 16,
    # "vviq_scale_3" = 17,
    "vviq_q13" = 18,
    "vviq_q14" = 19,
    "vviq_q15" = 20,
    "vviq_q16" = 21,
    "vviq_score" = 22,
    "tas_q1" = 23,
    "tas_q2" = 24,
    "tas_q3" = 25,
    "tas_q4" = 26,
    "tas_q5" = 27,
    "tas_q6" = 28,
    "tas_q7" = 29,
    "tas_q8" = 30,
    "tas_q9" = 31,
    "tas_q10" = 32,
    "tas_q11" = 33,
    "tas_q12" = 34,
    "tas_q13" = 35,
    "tas_q14" = 36,
    "tas_q15" = 37,
    "tas_q16" = 38,
    "tas_q17" = 39,
    "tas_q18" = 40,
    "tas_q19" = 41,
    "tas_q20" = 42,
    "tas_dif" = 43,
    "tas_ddf" = 44,
    "tas_eot" = 45,
    "tas_tot" = 46
  ) |> 
  mutate(
    # Converting TAS items from text to numeric
    across(
      starts_with("tas_q"),
      ~case_when(
        . == "Désaccord complet" ~ 1,
        . == "Désaccord relatif" ~ 2,
        . == "Ni accord ni désaccord" ~ 3,
        . == "Accord relatif" ~ 4,
        . == "Accord complet" ~ 5,
        TRUE ~ NA
      )
    ),
    # Reverse coding TAS items
    tas_q4  = 6 - tas_q4,
    tas_q5  = 6 - tas_q5,
    tas_q10 = 6 - tas_q10,
    tas_q18 = 6 - tas_q18,
    tas_q19 = 6 - tas_q19
  ) |>
  rowwise() |> 
  mutate(
    vviq = sum(c_across(starts_with("vviq_q"))),
    tas  = sum(c_across(contains("tas_q"))),
    tas_identify = sum(
      c_across(
        c(
          "tas_q1", "tas_q3", "tas_q6", 
          "tas_q7", "tas_q9", "tas_q13", "tas_q14"
        )
      )
    ),
    tas_describe = sum(
      c_across(c("tas_q2", "tas_q4", "tas_q11", "tas_q12", "tas_q17"))
    ),
    tas_external = sum(
      c_across(
        c(
          "tas_q5", "tas_q8", "tas_q10", "tas_q15",
          "tas_q16", "tas_q18", "tas_q19", "tas_q20"
        )
      )
    )
  ) |>
  ungroup() |> 
  suppressMessages()

openxlsx::write.xlsx(
  x = df_ruby_raw |> 
    select(
      age, sex, vviq, tas,
      tas_identify, tas_describe, tas_external,
      starts_with("vviq_q"),
      starts_with("tas_q")
    ),
  file       = here("data-raw/data_ruby_raw.xlsx"),
  asTable    = TRUE,
  colNames   = TRUE,
  colWidths  = "auto",
  borders    = "all",
  tableStyle = "TableStyleMedium16"
)

df_ruby <-
  df_ruby_raw |> 
  mutate(
    study = "ruby",
    id = paste0("subj_ruby_", row_number()),
    sex = case_when(
      .data$sex == "Féminin" ~ 1, 
      .data$sex == "Masculin" ~ 0,
      TRUE ~ 2
    ),
    gender = NA,
  ) |> 
  select(
    "study", "id", "sex", "gender", "age", "vviq", "tas",
    "tas_identify":"tas_external"
  )

# Kvamme -----------------------------------------------------------
df_kvamme <-
  read_csv(here("data-raw/data_kvamme.csv"), show_col_types = FALSE)  |> 
  mutate(
    study = "kvamme",
    id = paste0("subj_kvamme_", row_number()),
    sex = NA,
    # Kvamme mistakenly reversed TAS items 11, 15 & 16 and forgot to reverse 18
    tas_11 = 6 - tas_11,
    tas_15 = 6 - tas_15,
    tas_16 = 6 - tas_16,
    tas_18 = 6 - tas_18
  ) |>
  rowwise() |> 
  mutate(
    tas = sum(c_across(contains("tas_"))),
    tas_identify = sum(
      c_across(
        c("tas_1", "tas_3", "tas_6", "tas_7", "tas_9", "tas_13", "tas_14")
      )
    ),
    tas_describe = sum(
      c_across(c("tas_2", "tas_4", "tas_11", "tas_12", "tas_17"))
    ),
    tas_external = sum(
      c_across(
        c(
          "tas_5", "tas_8", "tas_10", "tas_15",
          "tas_16", "tas_18", "tas_19", "tas_20"
        )
      )
    )
  ) |> 
  select(
    "study", "id", "sex", "gender", "age", "vviq", "tas",
    "tas_identify":"tas_external"
  ) |> 
  ungroup()

# Merging and creating groups ---------------------------------------------
create_vviq_groups <- function(df) {
  df |> 
    filter(vviq >= 16 & vviq <= 80) |>
    mutate(
      vviq_group_4 = case_when(
        vviq == 16 ~ "aphantasia",
        vviq >= 17 & vviq <= 32 ~ "hypophantasia",
        vviq >= 33 & vviq <= 74 ~ "typical",
        vviq >= 75 ~ "hyperphantasia",
        TRUE ~ NA_character_
      ),
      vviq_group_3 = ifelse(
        vviq_group_4 == "hyperphantasia",
        "typical",
        vviq_group_4
      ),
      vviq_group_2 = ifelse(
        vviq_group_3 == "hypophantasia",
        "aphantasia",
        vviq_group_3
      ),
      vviq_group_4 = factor(
        vviq_group_4, 
        levels = c("aphantasia", "hypophantasia", "typical", "hyperphantasia")
      ),
      vviq_group_3 = factor(
        vviq_group_3, 
        levels = c("aphantasia", "hypophantasia", "typical")
      ),
      vviq_group_2 = factor(vviq_group_2, levels = c("aphantasia", "typical"))
    )
}

tas_data <- 
  bind_rows(df_burns, df_monzel, df_ruby, df_kvamme) |> 
  mutate(
    across("study":"gender", as.factor),
    study = factor(
      study, 
      levels = c("burns", "monzel", "ruby", "kvamme")
    ),
    tas_group = 
      ifelse(tas >= 61, "alexithymia", "typical_tas") |> 
      factor(levels = c("alexithymia", "typical_tas"))
  ) |> 
  create_vviq_groups()

usethis::use_data(tas_data, overwrite = TRUE)
