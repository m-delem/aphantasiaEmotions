pacman::p_load(dplyr, here, openxlsx, readr, readxl, stringr, tidyr)

# Quick helper
select_and_sum_scales <- function(df) {
  df <-
    df |> 
    select(
      "study", "id", "sex", "gender", "age",
      "vviq_q1":"vviq_q16",
      "tas_q1":"tas_q20",
      "other_data"
    ) |>
    rowwise() |> 
    mutate(
      vviq = sum(c_across(starts_with("vviq_q"))),
      tas  = sum(c_across(starts_with("tas_q"))),
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
    ungroup()
  
  return(df)
}

# Burns -------------------------------------------------------------
df_burns <-
  bind_cols(
    read_xlsx(here("data-raw/data_burns.xlsx")),
    read_xlsx(here("data-raw/data_burns_items.xlsx")) |> select("VVIQ1":"TAS20")
  ) |> 
  rename_with(
    .fn = ~ .x |> 
      str_to_lower() |> 
      str_replace_all("tas", "tas_q") |> 
      str_replace_all("vviq", "vviq_q")
  ) |>
  mutate(
    study = "burns",
    id = paste0("subj_burns_", row_number()),
    sex = ifelse(.data$sexatbirth == 1, "male", "female"),
    gender = case_when(
      .data$currentgender == 1 ~ "male",
      .data$currentgender == 2 ~ "female",
      .data$currentgender == 3 ~ "other"
    ),
    # Reverse coding TAS items
    tas_q4  = 6 - tas_q4,
    tas_q5  = 6 - tas_q5,
    tas_q10 = 6 - tas_q10,
    tas_q18 = 6 - tas_q18,
    tas_q19 = 6 - tas_q19
  ) |> 
  nest(
    other_data = c(
      contains("ptsd"),
      contains("traum"),
      contains("itq"),
      contains("pcl5"))
  ) |> 
  select_and_sum_scales() |> 
  nest(items = c(starts_with("vviq_q"), starts_with("tas_q"))) |> 
  relocate("vviq":"tas_external", "items", .after = "age")

# Monzel -------------------------------------------------------------
df_monzel <- 
  read_xlsx(here("data-raw/data_monzel.xlsx"), sheet = "raw_data") |> 
  mutate(
    study = "monzel",
    id = paste0("subj_monzel_", row_number()), 
    gender = ifelse(.data$gender == 1, "male", "female"),
    sex = .data$gender,
    items = list(NULL)
  ) |> 
  rename_with(
    .fn = ~ .x |> str_replace_all(stringr::fixed("."), "_"),
    .cols = contains(".")
  ) |> 
  nest(other_data = c("brt_priming":"ert_sad_rt")) |> 
  select(
    "study", "id", "sex", "gender", "age",
    "vviq" = "vviq_score",
    "tas"  = "tas_score",
    "tas_identify":"tas_external",
    "items", "other_data"
  )

# Ruby -------------------------------------------------------------
df_ruby <- 
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
    study = "ruby",
    id = paste0("subj_ruby_", row_number()),
    age = ifelse(age == 1 | age == 99, NA, age),
    sex = case_when(
      .data$sex == "Féminin" ~ "female", 
      .data$sex == "Masculin" ~ "male",
      TRUE ~ "other"
    ),
    gender = .data$sex,
    other_data = list(NULL),
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
  select_and_sum_scales() |> 
  nest(items = c(starts_with("vviq_q"), starts_with("tas_q"))) |> 
  relocate("vviq":"tas_external", "items", .after = "age") |>
  suppressMessages()

# Kvamme -----------------------------------------------------------
df_kvamme <-
  read_csv(here("data-raw/data_kvamme.csv"), show_col_types = FALSE) |> 
  rename_with(.fn = ~ .x |> str_replace_all("tas_", "tas_q")) |> 
  rename(
    "vviq_q1" = "vviq_q_2_1_1",
    "vviq_q2" = "vviq_q_2_1_2",
    "vviq_q3" = "vviq_q_2_1_3",
    "vviq_q4" = "vviq_q_2_1_4",
    "vviq_q5" = "vviq_q2_2_1",
    "vviq_q6" = "vviq_q2_2_2",
    "vviq_q7" = "vviq_q2_2_3",
    "vviq_q8" = "vviq_q2_2_4",
    "vviq_q9" = "vviq_q2_3_1",
    "vviq_q10" = "vviq_q2_3_2",
    "vviq_q11" = "vviq_q2_3_3",
    "vviq_q12" = "vviq_q2_3_4",
    "vviq_q13" = "vviq_q2_4_1",
    "vviq_q14" = "vviq_q2_4_2",
    "vviq_q15" = "vviq_q2_4_3",
    "vviq_q16" = "vviq_q2_4_4"
  ) |> 
  mutate(
    study = "kvamme",
    id = paste0("subj_kvamme_", row_number()),
    gender = case_when(
      .data$gender == 1 ~ "male",
      .data$gender == 2 ~ "female",
      .data$gender == 3 ~ "other"
    ),
    sex = .data$gender,
    # Kvamme mistakenly reversed TAS items 11, 15 & 16 and forgot to reverse 18
    # The rest of the reverse coding has already been done
    tas_q11 = 6 - .data$tas_q11,
    tas_q15 = 6 - .data$tas_q15,
    tas_q16 = 6 - .data$tas_q16,
    tas_q18 = 6 - .data$tas_q18
  ) |> 
  nest(
    other_data = c(
      "duration (in seconds)",
      "userlanguage",
      "ias_1":"stai_20"
    )
  ) |> 
  select_and_sum_scales() |> 
  nest(items = c(starts_with("vviq_q"), starts_with("tas_q"))) |> 
  relocate("vviq":"tas_external", "items", .after = "age")

# Merging and creating groups ---------------------------------------------
all_data <- 
  bind_rows(df_burns, df_monzel, df_ruby, df_kvamme) |> 
  filter(vviq >= 16 & vviq <= 80) |>
  mutate(
    across("study":"gender", as.factor),
    study = factor(
      .data$study, 
      levels = c("burns", "monzel", "ruby", "kvamme")
    ),
    tas_group = 
      ifelse(.data$tas >= 61, "alexithymia", "typical_tas") |> 
      factor(levels = c("alexithymia", "typical_tas")),
    vviq_group_4 = case_when(
      .data$vviq == 16 ~ "aphantasia",
      .data$vviq >= 17 & .data$vviq <= 32 ~ "hypophantasia",
      .data$vviq >= 33 & .data$vviq <= 74 ~ "typical",
      .data$vviq >= 75 ~ "hyperphantasia",
      TRUE ~ NA_character_
    ),
    vviq_group_3 = ifelse(
      .data$vviq_group_4 == "hyperphantasia",
      "typical",
      .data$vviq_group_4
    ),
    vviq_group_2 = ifelse(
      .data$vviq_group_3 == "hypophantasia",
      "aphantasia",
      .data$vviq_group_3
    ),
    vviq_group_4 = factor(
      .data$vviq_group_4, 
      levels = c("aphantasia", "hypophantasia", "typical", "hyperphantasia")
    ),
    vviq_group_3 = factor(
      .data$vviq_group_3, 
      levels = c("aphantasia", "hypophantasia", "typical")
    ),
    vviq_group_2 = factor(
      .data$vviq_group_2, levels = c("aphantasia", "typical"))
  ) |> 
  relocate("items", "other_data", .after = last_col())

usethis::use_data(all_data, overwrite = TRUE)
