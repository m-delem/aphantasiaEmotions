# pacman allows to check, install and load packages with a single call
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(mgcv, modelbased, performance)

source(here::here("R/report_contrast.R"))

load(here::here("data/tas_data.rda"))

# Total TAS score linear (group) model ---------------------
m_total <- lm(tas ~ vviq_group_4, data = tas_data)
report_contrast(m_total, ~vviq_group_4)

# DIF
m_identify <- lm(tas_identify ~ vviq_group_4, data = tas_data)
report_contrast(m_identify, ~vviq_group_4)

# DDF
m_describe <- lm(tas_describe ~ vviq_group_4, data = tas_data)
report_contrast(m_describe, ~vviq_group_4)

# EOT
m_external <- lm(tas_external ~ vviq_group_4, data = tas_data)
report_contrast(m_external, ~vviq_group_4)

# Generalized additive modelling with mgcv (non-linear) ---------
family <- gaussian()
# family <- ziP()

m_gam_total <- gam(tas ~ s(vviq, bs = "tp"), data = tas_data, family = family)
m_gam_identify <- gam(tas_identify ~ s(vviq), data = tas_data, family = family)
m_gam_describe <- gam(tas_describe ~ s(vviq), data = tas_data, family = family)
m_gam_external <- gam(tas_external ~ s(vviq), data = tas_data, family = family)

# Numerical description
describe_nonlinear(
  estimate_means(m_gam_total, by = "vviq", length = 75),
  x = "vviq", y = "Mean"
)
