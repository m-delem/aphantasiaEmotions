# pacman allows to check, install and load packages with a single call
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
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
df_ruby <- 
  read_xlsx(here("data-raw/data_ruby.xlsx")) |> 
  mutate(
    study = "ruby",
    id = paste0("subj_ruby_", row_number()),
    sex = NA,
    gender = NA,
    age = NA
  ) |> 
  select(
    "study":"age",
    "vviq" = "VVIQ",
    "tas" = "TAS20 - tot",
    "tas_identify" = "DIF",
    "tas_describe" = "DDF",
    "tas_external" = "EOT"
  )

# Kvamme -----------------------------------------------------------
df_kvamme <-
  read_csv(here("data-raw/data_kvamme.csv"))  |> 
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


# D'Argembeau ------------------------------------------------------
df_darg <-
  read_xlsx(here("data-raw/data_dargembeau.xlsx")) |> 
  mutate(
    study = "dargembeau",
    id = paste0("subj_dargembeau_", row_number()),
    gender = NA,
    sex = ifelse(SEXE == "M", 0, 1)
  ) |> 
  rename_with(
    .fn = ~ stringr::str_replace(., "-", "_") |> stringr::str_to_lower(),
    .cols = everything()
  ) |> 
  select(
    "study", "id", "sex", "gender", "age",
    "vviq" = "vviq_tot",
    "erq_r", "erq_s"
  )

# Ji -----------------------------------------------------------------
df_ji <-
  read_csv(here("data-raw/data_ji.csv")) |> 
  rename_with(
    .fn = 
      ~ stringr::str_replace_all(., stringr::fixed("."), "_") |> 
      stringr::str_to_lower(),
    .cols = everything()
  ) |> 
  mutate(
    study = "ji",
    id = paste0("subj_ji_", row_number()),
    sex = NA
  ) |> 
  select(
    "study", "id", "sex", "gender", "age",
    "vviq" = "vviq_score",
    "paq" = "paq_score"
  )

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

erq_data <- 
  df_darg |> 
  mutate(across("study":"gender", as.factor)) |> 
  create_vviq_groups()

paq_data <- 
  df_ji |> 
  mutate(across("study":"gender", as.factor)) |> 
  create_vviq_groups()

save(tas_data, file = here("data/tas_data.rda"))
save(erq_data, file = here("data/erq_data.rda"))
save(paq_data, file = here("data/paq_data.rda"))
