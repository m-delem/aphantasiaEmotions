devtools::load_all()

# Total TAS score linear (group) model ---------------------
m_total <- lm(tas ~ vviq_group_4, data = all_data)
report_contrast(m_total, ~vviq_group_4)

# DIF
m_identify <- lm(tas_identify ~ vviq_group_4, data = all_data)
report_contrast(m_identify, ~vviq_group_4)

# DDF
m_describe <- lm(tas_describe ~ vviq_group_4, data = all_data)
report_contrast(m_describe, ~vviq_group_4)

# EOT
m_external <- lm(tas_external ~ vviq_group_4, data = all_data)
report_contrast(m_external, ~vviq_group_4)

# Generalized additive modelling with mgcv (non-linear) ---------
m_gam_total    <- fit_vviq_gam(vd = "tas")
m_gam_identify <- fit_vviq_gam(vd = "tas_identify")
m_gam_describe <- fit_vviq_gam(vd = "tas_describe")
m_gam_external <- fit_vviq_gam(vd = "tas_external")

# Numerical description
modelbased::describe_nonlinear(
  modelbased::estimate_means(m_gam_total, by = "vviq", length = 75),
  x = "vviq", y = "Mean"
)
