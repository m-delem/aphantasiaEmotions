# Package index

## Raw data

The pooled dataset from all five studies underlying this project’s
analyses.

- [`all_data`](https://m-delem.github.io/aphantasiaEmotions/reference/all_data.md)
  : VVIQ and TAS data from various studies

## Modelling

Wrappers around brms and bayestestR for fitting Bayesian models with
this project’s own conventions, and for summarising evidence from their
posterior distributions.

- [`fit_brms_model()`](https://m-delem.github.io/aphantasiaEmotions/reference/fit_brms_model.md)
  : Fit a Bayesian model using the brms package with default settings
- [`report_rope()`](https://m-delem.github.io/aphantasiaEmotions/reference/report_rope.md)
  : Report the ROPE analysis for marginal effects
- [`check_slope_evidence()`](https://m-delem.github.io/aphantasiaEmotions/reference/check_slope_evidence.md)
  : Check VVIQ slope data for Bayesian GAMs

## Visualisation

ggplot2 helpers built for this project’s specific figures — raincloud
plots, posterior contrasts, GAM curves, the floor-group model’s
signature figure, and shared scales and themes used throughout the
Extended Online Report.

- [`summarise_aph_and_alexi()`](https://m-delem.github.io/aphantasiaEmotions/reference/summarise_aph_and_alexi.md)
  : Compute counts and proportions of alexithymia groups within VVIQ
  groups
- [`plot_alexithymia_proportions()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_alexithymia_proportions.md)
  : Plot alexithymia proportions within VVIQ groups based on a summary
  table
- [`plot_vviq_group_proportions()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_vviq_group_proportions.md)
  : Plot VVIQ group proportions of a specified variable across studies
- [`plot_group_violins()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_group_violins.md)
  : Plot group violins with means and confidence intervals
- [`plot_posterior_contrasts()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_posterior_contrasts.md)
  : Plot the posterior distributions of marginal contrasts
- [`plot_gam_means()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_gam_means.md)
  : Plot GAM estimated means
- [`plot_gam_slopes()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_gam_slopes.md)
  : Plot GAM estimated slopes
- [`plot_coloured_subjects()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_coloured_subjects.md)
  : Plot individual participants coloured by VVIQ group
- [`plot_alexithymia_cutoff()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_alexithymia_cutoff.md)
  : Plot Alexithymia cut-off line
- [`plot_floor_group()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_floor_group.md)
  : Plot the floor-group additive model against the data
- [`plot_vviq_marginal_histogram()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_vviq_marginal_histogram.md)
  : Plot the marginal VVIQ distribution (floor spike + continuous
  remainder)
- [`scale_discrete_aphantasia()`](https://m-delem.github.io/aphantasiaEmotions/reference/scale_discrete_aphantasia.md)
  : Custom discrete scale for Aphantasia groups
- [`scale_x_aphantasia()`](https://m-delem.github.io/aphantasiaEmotions/reference/scale_x_aphantasia.md)
  : Custom x-axis scale for Aphantasia groups
- [`scale_x_vviq()`](https://m-delem.github.io/aphantasiaEmotions/reference/scale_x_vviq.md)
  : Custom x-axis scale for VVIQ scores
- [`theme_pdf()`](https://m-delem.github.io/aphantasiaEmotions/reference/theme_pdf.md)
  : Theme for elegant scientific vector figures
- [`save_ggplot()`](https://m-delem.github.io/aphantasiaEmotions/reference/save_ggplot.md)
  : Custom ggsave wrapper set with Nature's formatting guidelines
  (width-locked)
