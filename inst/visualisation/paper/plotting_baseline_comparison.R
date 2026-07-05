# ==============================================================================
# 2-group (naive baseline) + 4-group categorical figure, side by side
# ==============================================================================
#
# Uses the EXISTING plot_group_violins() function (R/plot_data_distribution.R)
# unchanged — no new plotting machinery needed. vviq_group_2 and
# vviq_group_4 both already exist in all_data (confirmed in R/data.R), so
# this is purely a composition + styling task.
#
# DESIGN: the 2-group panel is deliberately coloured uniform grey (both
# violins the same shade), rather than plot_group_violins()'s default
# discrete colour mapping — this visually signals "this comparison doesn't
# differentiate much" before the reader even reads the numbers, in
# contrast with the 4-group panel's full colour richness (using the
# package's existing scale_discrete_aphantasia() convention). The two
# panels together make the "naive baseline vs. richer model" argument
# largely through colour alone.

library(patchwork)

# ------------------------------------------------------------------------------
# 2-group panel — grey override
# ------------------------------------------------------------------------------
panel_2group <-
  plot_group_violins(
    tas ~ vviq_group_2,
    data = all_data,
    y_lab = "Total TAS score",
    x_lab = NULL
  ) +
  scale_x_aphantasia(add = c(0.4, 0.7)) +
  # Override plot_group_violins()'s default discrete colour/fill mapping
  # with a uniform grey for BOTH groups — deliberately undifferentiated.
  ggplot2::scale_color_manual(values = c("grey50", "grey50"), guide = "none") +
  ggplot2::scale_fill_manual(values = c("grey50", "grey50"), guide = "none") +
  ggplot2::labs(title = "The common approach")

# ------------------------------------------------------------------------------
# 4-group panel — existing package convention (scale_discrete_aphantasia())
# ------------------------------------------------------------------------------
panel_4group <-
  plot_group_violins(
    tas ~ vviq_group_4,
    data = all_data,
    y_lab = NULL,  # shared y-axis meaning with panel_2group — avoid
                    # repeating the label when composed side by side
    x_lab = NULL
  ) +
  scale_x_aphantasia(add = c(0.4, 0.7)) +
  scale_discrete_aphantasia() +
  ggplot2::labs(title = "A richer, still-common alternative")

# ------------------------------------------------------------------------------
# Compose side by side
# ------------------------------------------------------------------------------
# combined_baseline_panel <-
  panel_2group + panel_4group +
  patchwork::plot_layout(widths = c(1, 1.6))  # 4-group needs more width for
                                                # 4 violins vs. 2 — ratio is a
                                                # first guess, adjust to taste

# save_ggplot("baseline_comparison_panel.pdf", ncol = 2, height = 90)  # adjust
# dimensions as needed, following the same save_ggplot() convention used
# for the floor-group composite figure

# ------------------------------------------------------------------------------
# THINGS TO CHECK once run against real data:
# 1. scale_color_manual()/scale_fill_manual() with only 2 grey values
#    assumes vviq_group_2 has EXACTLY 2 levels in the order the function
#    expects — verify factor levels/order match (e.g. via
#    levels(all_data$vviq_group_2)) before trusting the override applies
#    to both violins correctly rather than erroring on a level mismatch.
# 2. plot_group_violins()'s internal geom_point/geom_crossbar colour
#    aesthetics are ALSO mapped to the grouping variable (see the
#    function's source) — scale_color_manual()/scale_fill_manual() should
#    override all three layers (points, crossbar, violin) at once since
#    they share the same discrete scale, but this hasn't been visually
#    confirmed only text-reasoned from the function's aes() mappings.
# 3. widths = c(1, 1.6) in plot_layout() is a first-guess ratio, not
#    derived from anything — likely needs your usual visual tuning pass.
# ------------------------------------------------------------------------------
