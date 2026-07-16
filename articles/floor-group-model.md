# The floor-group model, in depth

``` r

library(aphantasiaEmotions)
library(ggplot2)
library(patchwork)

# Models and results are loaded directly from their saved artefacts in the 
# vignette, and explicitly never refitted (this is merely to protect the
# website). The "pkg" shorthand will be used throughout to point to the files of
# the aphantasiaEmotions package.
# See the Implementation Notes page for how these models were actually built.
pkg <- "aphantasiaEmotions"
refit <- "never"
```

The [previous
page](https://m-delem.github.io/aphantasiaEmotions/articles/model-comparison.html)
ended on a twist: among six candidate models, the one that best combines
quality of fit and parsimony is not the most flexible one (the segmented
model with an estimated knot), but the simplest one that captures the
right structure: a plain linear relationship among everyone above VVIQ =
16, plus a single coefficient letting complete aphantasics have their
own mean. This page is the full case for that model: what it says, how
confident we can be in what it says, and whether it holds up once
study-level heterogeneity and prior choice are both accounted for.

## The model

``` r

# Creating a binary column for whether a participant is in the floor-VVIQ group
# (complete aphantasia) or not
model_data <- all_data
model_data$complete_aphant <- factor(
  ifelse(model_data$vviq_group_4 == "aphantasia", "floor", "above_floor"),
  levels = c("above_floor", "floor")
)

# Fitting the model
floor_group_additive_multilevel <- fit_brms_model(
  formula = tas ~ vviq + complete_aphant + (vviq | study),
  data    = model_data,
  prior   = brms::prior(normal(0, 20), class = "b"),
  file    = system.file(
    "models", "floor_group_additive_multilevel_tot.rds", package = pkg),
  file_refit = refit
)
```

The formula is deliberately asymmetric, and that asymmetry is the whole
point. Complete aphantasics (VVIQ = 16) have no variance in VVIQ among
themselves (every one of them has the same score) so there is no data to
estimate a VVIQ-TAS *slope* specific to that group. What the data can
support is a single, well-identified quantity: how far that group’s mean
TAS score sits from where the continuous relationship, fit on everyone
else, would have predicted it. `complete_aphant` is that quantity.
`(vviq | study)` lets both the slope and the intercept vary by study,
which is what makes the model’s conclusions checkable against
between-study heterogeneity rather than just the pooled average (see
[below](#multilevel)).

## The floor group, visualised

Before the full figure, it’s worth seeing the piece of data that
motivates this whole model on its own: the VVIQ distribution itself is
not smoothly continuous. There is a sharp, isolated spike at the scale’s
floor, distinct from a more continuous, if irregular, remainder above
it.

``` r

plot_vviq_marginal_histogram(model_data, base_size = 16) +
  ggplot2::labs(
    title = "The VVIQ distribution isn't smoothly continuous",
    x = "VVIQ score"
  )
```

![A histogram of VVIQ scores across the full pooled sample, showing a
sharp, isolated spike at the floor value (VVIQ = 16, coloured red) and a
more continuous, irregular distribution of scores from about 20 to 80
(coloured with a viridis
gradient).](floor-group-model_files/figure-html/vviq-marginal-standalone-1.png)

That spike is the reason a group-specific intercept, rather than a
smooth curve, turns out to be the right tool: the data itself is telling
you one part of the range behaves like a distinct category, not like the
tail of a continuum.

Composed with the model’s own fitted relationship, the same histogram
becomes the header panel of this project’s signature figure:

``` r

hist_panel <-
  plot_vviq_marginal_histogram(model_data, base_size = 16) +
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  ggplot2::scale_x_continuous(
    limits = c(8, 81),
    expand = ggplot2::expansion(c(0.02, 0))
  )

main_panel <- 
  plot_floor_group(
    floor_group_additive_multilevel, model_data, 
    base_size = 16, stat_txt_size = 4.5)

hist_panel / main_panel + patchwork::plot_layout(heights = c(1, 4))
```

![A two-panel composite figure. The top panel repeats the VVIQ histogram
above. The bottom panel shows Total TAS score against VVIQ score, with a
fitted line for the above-floor relationship (coloured by a viridis
gradient matching the histogram), a violin plot showing the floor
group's own TAS distribution to the left, a cross marking where the
above-floor line would predict the floor group's mean to be, and an
arrow showing the gap between that prediction and the floor group's
actual
mean.](floor-group-model_files/figure-html/floor-group-figure-1.png)

## The evidence

``` r

rope_range_contrast <- bayestestR::rope_range(floor_group_additive_multilevel)

sd_tas  <- stats::sd(model_data$tas)
sd_vviq <- stats::sd(model_data$vviq)
rope_range_slope <- 0.2 * (sd_tas / sd_vviq)

floor_effect <- bayestestR::describe_posterior(
  floor_group_additive_multilevel,
  parameters = "complete_aphant",
  rope_range = rope_range_contrast
)

vviq_slope <- bayestestR::describe_posterior(
  floor_group_additive_multilevel,
  parameters = "vviq",
  rope_range = c(-rope_range_slope, rope_range_slope)
)
```

The floor group’s mean sits 8.41 points below where the above-floor
relationship would predict it (95% CI \[-11.15, -5.66\]), with a
probability of direction of 100.0% and 0.0% of the posterior inside the
region of practical equivalence to zero — as clear and as meaningful an
effect as this project’s evidentiary standards can show.

The above-floor slope itself is -0.266 (95% CI \[-0.350, -0.186\]), pd =
99.9%, 0.0% in ROPE. Note that this uses a different ROPE convention
than the floor-group contrast above, since a raw slope and a group
contrast aren’t comparable on the same scale (see [implementation
notes](https://m-delem.github.io/aphantasiaEmotions/articles/implementation-notes.html#rope-conventions-contrasts-vs--slopes)
for the full reasoning).

## Multilevel robustness

The result above already comes from the multilevel model:
`(vviq | study)` is part of the formula, not a separate add-on. It’s
worth showing directly what that buys: does the floor effect look like a
pooled-sample artefact, or does it hold up study by study?

``` r

study_coefs <- coef(floor_group_additive_multilevel)$study
study_coefs_df <- data.frame(
  study     = dimnames(study_coefs)[[1]],
  intercept = study_coefs[, "Estimate", "Intercept"],
  slope     = study_coefs[, "Estimate", "vviq"]
)

pooled_coefs <- brms::fixef(floor_group_additive_multilevel)

study_lines <- do.call(rbind, lapply(unique(model_data$study), function(s) {
  study_range <- range(model_data$vviq[model_data$study == s])
  coefs <- study_coefs_df[study_coefs_df$study == s, ]
  grid <- data.frame(
    vviq = seq(study_range[1], study_range[2], length.out = 100),
    study = s
  )
  grid$estimate <- coefs$intercept + coefs$slope * grid$vviq
  grid
}))

pooled_line <- data.frame(vviq = seq(16, 80, length.out = 100))
pooled_line$estimate <- pooled_coefs["Intercept", "Estimate"] +
  pooled_coefs["vviq", "Estimate"] * pooled_line$vviq

floor_raw <- model_data[model_data$vviq_group_4 == "aphantasia", ]
dens <- stats::density(floor_raw$tas, from = 20, to = 100, n = 200)
dens_scaled <- dens$y / max(dens$y) * 2
violin_df <- data.frame(x = 16 - dens_scaled, y = dens$x)

study_colors <- c(
  burns = "#E69F00", monzel = "#56B4E9", mas = "#009E73",
  ruby = "#D55E00", kvamme = "#CC79A7"
)
study_labels <-  c(
  burns = "Ale & B", monzel = "Monzel", mas = "Mas & L",
  ruby = "Ruby", kvamme = "Kvamme"
)

ggplot2::ggplot() +
  ggplot2::geom_point(
    data = model_data, ggplot2::aes(x = vviq, y = tas),
    alpha = 0.08, size = 0.8, color = "grey60"
  ) +
  ggplot2::geom_polygon(
    data = rbind(
      data.frame(x = violin_df$x, y = violin_df$y),
      data.frame(x = rep(16, nrow(violin_df)), y = rev(violin_df$y))
    ),
    ggplot2::aes(x = x, y = y),
    fill = "#C44E52", alpha = 0.35, color = "#8B3A3E", linewidth = 0.2
  ) +
  ggplot2::geom_line(
    data = pooled_line, ggplot2::aes(x = vviq, y = estimate),
    color = "black", linewidth = 1
  ) +
  ggplot2::geom_line(
    data = study_lines,
    ggplot2::aes(x = vviq, y = estimate, color = study),
    linewidth = 0.6, alpha = 0.85
  ) +
  ggplot2::scale_color_manual(
    values = study_colors, 
    labels = study_labels,
    name = "Study"
  ) +
  ggplot2::labs(
    x = "VVIQ score",
    y = "Total TAS score",
    caption = "Floor-group N by study: Ale & Burns: 60; Monzel et al: 20\nMas et Luminet: 0; Ruby: 13; Kvamme et al: 54"
  ) +
  theme_pdf(
    base_size = 16,
    base_theme = ggplot2::theme_minimal,
    panel.grid.minor = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
    legend_relative = 0.9
  )
```

![A line plot showing each of the five studies' own fitted VVIQ-TAS
relationship (in five different colours, each restricted to that study's
own observed VVIQ range), overlaid with the pooled population-level
relationship in bold black. A muted violin at VVIQ=16 shows the floor
group's pooled TAS distribution. Four of the five studies' lines cluster
closely around the pooled line; one study (Mas & Luminet) diverges
somewhat, with a shallower slope and a higher
intercept.](floor-group-model_files/figure-html/per-study-figure-1.png)

Four of the five studies’ own fitted lines cluster closely around the
pooled relationship. Mas & Luminet’s diverges somewhat — a shallower
slope, offset upward — consistent with that study’s own composition
described on the [sample
description](https://m-delem.github.io/aphantasiaEmotions/articles/sample-description.html)
page (young, homogeneous, no complete-aphantasia participants of its own
to anchor the floor group’s contribution). The other four studies,
spanning different languages, recruitment channels, and sample sizes,
tell a consistent story.

## Various checks

### Is the floor group responding coherently?

The floor effect could, in principle, reflect something other than
typical emotional functioning: if complete aphantasics found the
TAS-20’s items harder to understand or introspect on, their low scores
might reflect noisy or degraded responding rather than a genuine absence
of alexithymia. This is directly checkable. If responding were degraded,
it should show up as weaker internal coherence — the three TAS-20
sub-scales moving together less consistently, and the twenty individual
items agreeing with each other less — within complete aphantasics
specifically, compared to the rest of the sample.

``` r

items_flat <- 
  all_data |>
  dplyr::select(id, study, vviq, tas_identify, tas_describe, tas_external, items) |>
  tidyr::unnest(items) |>
  dplyr::select(
    id, study, vviq, tas_identify, tas_describe, tas_external,
    dplyr::starts_with("tas_q")) |>
  dplyr::mutate(
    group = dplyr::if_else(vviq == 16, "Complete aphantasia", "Rest of sample")
  )
```

``` r

subscale_corr <- 
  items_flat |>
  dplyr::group_by(group) |>
  dplyr::summarise(
    "DIF-DDF" = cor(tas_identify, tas_describe),
    "DIF-EOT" = cor(tas_identify, tas_external),
    "DDF-EOT" = cor(tas_describe, tas_external),
    n = dplyr::n(),
    .groups = "drop"
  )

subscale_corr |> knitr::kable(digits = 3)
```

| group               | DIF-DDF | DIF-EOT | DDF-EOT |    n |
|:--------------------|--------:|--------:|--------:|-----:|
| Complete aphantasia |   0.740 |   0.192 |   0.426 |  127 |
| Rest of sample      |   0.717 |   0.191 |   0.325 | 1246 |

``` r

alpha_by_group <- 
  items_flat |>
  dplyr::group_by(group) |>
  dplyr::group_map(
    ~ psych::alpha(
      dplyr::select(.x, dplyr::starts_with("tas_q")), 
      warnings = FALSE)
  )

names(alpha_by_group) <- 
  items_flat |> 
  dplyr::distinct(group) |> 
  dplyr::pull(group)

alpha_summary <- purrr::map_dfr(
  names(alpha_by_group),
  \(g) tibble::tibble(
    group = g,
    "Cronbach's alpha" = alpha_by_group[[g]]$total$raw_alpha,
    "Items" = 20
  )
)

alpha_summary |> knitr::kable(digits = 3)
```

| group               | Cronbach’s alpha | Items |
|:--------------------|-----------------:|------:|
| Complete aphantasia |            0.875 |    20 |
| Rest of sample      |            0.862 |    20 |

Both checks come back clean. The three sub-scales correlate with each
other in complete aphantasics in essentially the same pattern as in the
rest of the sample — DIF and DDF move together most strongly, DIF and
EOT most weakly, in both groups alike — and Cronbach’s alpha across all
twenty items is, if anything, marginally *higher* in complete
aphantasics (0.875) than in the rest of the sample (0.862). There is no
sign here of degraded or incoherent responding in the floor group: their
answers hang together at least as well as everyone else’s, which is the
pattern expected of genuine, typical self-report rather than one
distorted by an introspective deficit specific to this group.

### Prior sensitivity

The group-level slope SD term, i.e., how much the VVIQ-TAS slope is
allowed to vary by study, relies on brms’s own default
weakly-informative prior rather than a hand-picked one, deliberately:
with only five studies informing that specific variance component, a
tighter, hand-chosen prior would risk doing more inferential work than
could be defended. The prior on fixed effects we chose was also
deliberately weakly informative. Whether the model’s substantive
conclusions depend on these choices is checked directly, refitting with
priors twice as wide as the defaults chosen:

``` r

sensitivity_priors <- c(
  brms::prior(
    normal(0, 40), class = "b"), # twice as wide as our normal(0,20) default
  brms::prior(
    student_t(3, 0, 26.6), # twice as wide as brms' default (13.3)
    class = "sd", group = "study", coef = "vviq")
)

floor_group_additive_multilevel_wide_prior <- fit_brms_model(
  formula = tas ~ vviq + complete_aphant + (vviq | study),
  data    = model_data,
  prior   = sensitivity_priors,
  file    = system.file(
    "models", "floor_group_additive_multilevel_wide_prior_tot.rds", 
    package = pkg),
  file_refit = refit
)

default_fixef <- brms::fixef(floor_group_additive_multilevel)
wide_fixef    <- brms::fixef(floor_group_additive_multilevel_wide_prior)

sensitivity_table <- data.frame(
  parameter = c("vviq (slope)", "complete_aphantfloor"),
  default_prior = c(
    default_fixef["vviq", "Estimate"], 
    default_fixef["complete_aphantfloor", "Estimate"]),
  wide_prior = c(
    wide_fixef["vviq", "Estimate"], 
    wide_fixef["complete_aphantfloor", "Estimate"])
)

sensitivity_table |> knitr::kable(digits = 3)
```

| parameter            | default_prior | wide_prior |
|:---------------------|--------------:|-----------:|
| vviq (slope)         |        -0.267 |     -0.267 |
| complete_aphantfloor |        -8.412 |     -8.444 |

Both parameters are essentially unchanged between the default and the
deliberately wider priors: the headline result does not depend on which
weakly-informative priors were used to fit the model.

### Why gaussian()

Every model in this report, including this one, uses brms’s default
Gaussian family. That choice is checked, not just assumed: see the
[model
diagnostics](https://m-delem.github.io/aphantasiaEmotions/articles/model-diagnostics.html#gaussian-family)
page for the residual skewness, heteroscedasticity, and boundary checks
behind it.

## TAS-20 sub-scales

The total-TAS floor effect above is this project’s central finding. The
TAS-20 also has three established sub-scales — Difficulty Identifying
Feelings (DIF), Difficulty Describing Feelings (DDF), and
Externally-Oriented Thinking (EOT) — and the same model was fit
separately on each, to check whether the floor effect holds uniformly or
is concentrated in a specific facet of alexithymia.

``` r

subscale_results <- readRDS(
  system.file("results", "floor_group_subscale_results.rds", package = pkg)
)

subscale_results |> knitr::kable(digits = 3)
```

| subscale | parameter    | median | ci_low | ci_high |    pd | rope_low | rope_high | pct_in_rope |
|:---------|:-------------|-------:|-------:|--------:|------:|---------:|----------:|------------:|
| DIF      | floor_effect | -3.740 | -5.144 |  -2.340 | 1.000 |   -0.636 |     0.636 |       0.000 |
| DIF      | vviq_slope   | -0.108 | -0.158 |  -0.059 | 0.998 |   -0.072 |     0.072 |       0.037 |
| DDF      | floor_effect | -2.977 | -4.033 |  -1.925 | 1.000 |   -0.479 |     0.479 |       0.000 |
| DDF      | vviq_slope   | -0.094 | -0.131 |  -0.056 | 0.999 |   -0.054 |     0.054 |       0.000 |
| EOT      | floor_effect | -1.765 | -2.758 |  -0.802 | 1.000 |   -0.466 |     0.466 |       0.000 |
| EOT      | vviq_slope   | -0.061 | -0.098 |  -0.028 | 0.997 |   -0.052 |     0.052 |       0.252 |

``` r

m_dif <- readRDS(
  system.file("models", "floor_group_additive_multilevel_dif.rds", package = pkg))
m_ddf <- readRDS(
  system.file("models", "floor_group_additive_multilevel_ddf.rds", package = pkg))
m_eot <- readRDS(
  system.file("models", "floor_group_additive_multilevel_eot.rds", package = pkg))

p_dif <- 
  plot_floor_group(
    m_dif, model_data, y_lab = "TAS DIF score",
    tas_breaks = scales::pretty_breaks(5),
    base_size = 16, 
    stat_txt_size = 4, 
    floor_label_size = 0,
    legend.position = "none") +
  ggplot2::labs(title = "Difficulty Identifying Feelings")

p_ddf <- 
  plot_floor_group(m_ddf, model_data, y_lab = "TAS DDF score",
    tas_breaks = scales::pretty_breaks(5),
    base_size = 16, 
    stat_txt_size = 4, 
    floor_label_size = 0,
    legend.position = "none") +
  ggplot2::labs(title = "Difficulty Describing Feelings")

p_eot <- 
  plot_floor_group(m_eot, model_data, y_lab = "TAS EOT score",
    tas_breaks = scales::pretty_breaks(5),
    base_size = 16, 
    stat_txt_size = 4, 
    floor_label_size = 0,
    legend.position = "none") +
  ggplot2::labs(title = "Externally-Oriented Thinking")

p_dif / p_ddf / p_eot
```

![Three floor-group figures, one per TAS-20 subscale (Difficulty
Identifying Feelings, Difficulty Describing Feelings, and
Externally-Oriented Thinking), each following the same visual structure
as the total-TAS figure
above.](floor-group-model_files/figure-html/subscale-figures-1.png)

The floor effect is unambiguous across all three sub-scales: the floor
group’s mean sits clearly below the above-floor extrapolation on DIF,
DDF, and EOT alike, with 0%, 0%, and 0% of each posterior distribution
(respectively) inside its region of practical equivalence to zero. This
is not a pattern confined to one facet of alexithymia: complete
aphantasics score in typical-imager territory across every sub-scale
this instrument distinguishes, not just on the total score.

The above-floor slopes tell a more textured story. DIF’s and DDF’s
slopes are both clearly outside their negligible-effect ranges (3.7% and
0.0% of their respective posteriors inside ROPE). EOT’s slope is the one
partial exception: still directionally certain (pd = 99.7%) and still
mostly outside its ROPE, but with a meaningfully larger share of its
posterior (25.2%) falling inside the negligible range than either other
sub-scale. In other words, the continuous relationship between imagery
vividness and alexithymia above the floor is more consistently present
for the difficulty-identifying and difficulty-describing facets than for
externally-oriented thinking specifically — while the floor effect
itself, this project’s central finding, holds with equal force across
all three.

------------------------------------------------------------------------

**Continuing through the Extended Online Report:** this page follows the
[model
comparison](https://m-delem.github.io/aphantasiaEmotions/articles/model-comparison.html).
To keep reading in order, continue to [for those who come
after](https://m-delem.github.io/aphantasiaEmotions/articles/for-those-who-come-after.html)
next. Or see [model
diagnostics](https://m-delem.github.io/aphantasiaEmotions/articles/model-diagnostics.html#ppc-floor-group)
and [implementation
notes](https://m-delem.github.io/aphantasiaEmotions/articles/implementation-notes.html)
for the technical detail behind this model.
