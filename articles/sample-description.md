# Sample description

``` r

library(aphantasiaEmotions)
library(ggplot2)
library(ggpubr)
```

Every analysis in this report runs on `all_data`, containing 1478
participants pooled from five studies: two unpublished datasets shared
by the authors specifically for this project, three retrieved from
previously published or shared work. This page describes where each of
the five came from, who was in them, and a few places where they
genuinely differ from each other. Differences that may matter later,
when the question becomes whether a pattern found in the pooled sample
holds up once study membership is accounted for (see the [floor-group
model](https://m-delem.github.io/aphantasiaEmotions/articles/floor-group-model.htm#multilevel)
page).

## The five studies

``` r

all_data |>
  dplyr::group_by(study) |>
  dplyr::reframe(
    Language = unique(lang),
    N = paste0(
      dplyr::n(),
      " (",
      sum(gender == "female", na.rm = TRUE),
      " F, ",
      sum(gender == "other", na.rm = TRUE),
      " O)"
    ),
    M_age = mean(age, na.rm = TRUE),
    SD_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE)
  ) |>
  knitr::kable(digits = 2)
```

| study  | Language | N                | M_age | SD_age | min_age | max_age |
|:-------|:---------|:-----------------|------:|-------:|--------:|--------:|
| burns  | en       | 192 (122 F, 3 O) | 38.69 |  11.44 |      18 |      86 |
| monzel | en       | 105 (74 F, 0 O)  | 27.87 |   9.29 |      18 |      59 |
| mas    | fr       | 123 (110 F, 0 O) | 19.78 |   1.15 |      18 |      24 |
| ruby   | fr       | 225 (180 F, 3 O) | 35.96 |  16.07 |      10 |      82 |
| kvamme | en       | 833 (426 F, 5 O) | 40.45 |  13.44 |      18 |      83 |

**Ale & Burns** (2024) collected VVIQ and TAS-20 data as part of a study
on aphantasia, alexithymia, and PTSD symptomatology. 192
English-speaking participants (122 females, 3 other genders; mean age
38.7, SD 11.4; range 18-86) were recruited via social media. Their data
are archived on a private OSF ([osf.io/hqz3e](https://osf.io/hqz3e/)),
but made openly available in clean form in this [study’s own OSF
project](https://doi.org/10.17605/OSF.IO/B837S).

**Monzel et al.** (2024) collected VVIQ and TAS-20 data as part of a
study on aphantasia, alexithymia, and affective processing. 105
English-speaking participants (74 females; mean age 27.9, SD 9.29; range
18-59) were recruited via the *Aphantasia Research Project Bonn*’s
participant database, split into 75 controls (VVIQ \> 32) and 30
participants with weak or no imagery (VVIQ ≤ 23). Their own paper
reports a real, worth-flagging imbalance: the aphantasia sub-group was
on average 6.6 years older than controls (t = 3.09, p = .004, d = 0.76),
a difference large enough that the original authors treated age as a
covariate throughout. It’s a genuine limitation of that specific sample,
though not one that appears to have distorted the pattern this project
is built on: the VVIQ-TAS relationship in Monzel et al.’s data looks the
same as in the other four studies. Their data are openly archived on OSF
([osf.io/y9c8g](https://osf.io/y9c8g/?view_only=1e6bd8670a3f4eacb1cf0f600343205e)).

**Ruby** collected VVIQ and TAS-20 data as part of a study on the
sensory and emotional characteristics of autobiographical and dream
memories. 225 French participants (180 females, 42 males, 3 other; mean
age 36, SD 16.1; range 10-82), recruited by an announcement on social
media and on mailing lists dedicated to research volunteers in Lyon and
Paris, completed VVIQ and TAS-20 together with sensory and emotional
scales to describe the one autobiographical memory from the day before
and the one dream memory from the night before they had to report. The
inclusion criterion was to have a memory of a dream from the night
before when filling in the questionnaire.

**Mas & Luminet** (2025) collected VVIQ and TAS-20 data as part of a
preregistered lab experiment on alexithymia and mental representations.
123 French-speaking participants (110 females; mean age 19.78, SD 1.15;
range 18-24) were recruited from a research methods course, in exchange
for course credit. No inclusion or exclusion criteria were specified for
this study.

**Kvamme et al.** (2026) collected VVIQ and TAS-20 data as part of a
much larger study on mental imagery, mental health, subjective
interoception, and alexithymia. 833 English-speaking participants (426
females, 5 other genders; mean age 40.5, SD 13.4; range 18-83) were
recruited through Prolific, in two phases: an initial, targeted
recruitment of individuals with VVIQ ≤ 32 from a pre-existing database,
followed by open recruitment to cover the full range of imagery
vividness. That two-phase design is a large part of why this study
contributes such a substantial share of the pool’s complete-aphantasia
and hypophantasia participants despite not being an aphantasia-only
sample (see the [model
comparison](https://m-delem.github.io/aphantasiaEmotions/articles/model-comparison.html)
page for how much that group specifically matters to this project’s
central finding).

Kvamme et al.’s exclusion pipeline is also worth naming directly, since
it is more thoroughly documented than most: of an initial 855
respondents, 22 were excluded for excessively short or implausibly long
completion times, and for suspected careless responding, detected with
the Even-Odd Inconsistency Index (via the `careless` R package). The 833
remaining participants are what this project’s `kvamme` study component
contains.

## By VVIQ group

The same pooled sample, seen instead through the four-group VVIQ
classification used throughout this report:

``` r

all_data |>
  dplyr::group_by(vviq_group_4) |>
  dplyr::reframe(
    N = paste0(
      dplyr::n(),
      " (",
      sum(gender == "female", na.rm = TRUE),
      " F, ",
      sum(gender == "other", na.rm = TRUE),
      " O)"
    ),
    M_age = mean(age, na.rm = TRUE),
    SD_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    M_vviq = mean(vviq, na.rm = TRUE),
    SD_vviq = sd(vviq, na.rm = TRUE)
  ) |>
  dplyr::rename("VVIQ group" = 1) |>
  knitr::kable(digits = 2)
```

| VVIQ group     | N                 | M_age | SD_age | min_age | max_age | M_vviq | SD_vviq |
|:---------------|:------------------|------:|-------:|--------:|--------:|-------:|--------:|
| aphantasia     | 147 (102 F, 5 O)  | 40.27 |  12.90 |      19 |      86 |  16.00 |    0.00 |
| hypophantasia  | 141 (87 F, 1 O)   | 36.40 |  12.21 |      17 |      64 |  24.33 |    4.61 |
| typical        | 1115 (675 F, 4 O) | 36.34 |  14.47 |      10 |      82 |  55.33 |   10.05 |
| hyperphantasia | 75 (48 F, 1 O)    | 40.48 |  14.95 |      16 |      83 |  77.31 |    1.90 |

## Recruitment, and why the five studies aren’t interchangeable

These five studies were not designed together, and it shows in ways
worth being upfront about, not smoothing over. They differ in language
(three English-speaking samples, two French-speaking), in recruitment
channel (social media, a dedicated aphantasia research database, a
university subject pool, Prolific), and most consequentially, in how
“aphantasia” was operationalised before this project pooled everything
onto a common 16-80 VVIQ scale. Ale & Burns used a cutoff of VVIQ ≤ 32
to define their aphantasia group, explicitly because their own prior
work found that threshold more inclusive of people who self-identify as
having aphantasia than a stricter cutoff. Monzel et al. used VVIQ ≤ 23.
Kvamme et al. used ≤ 32 for their broader “aphantasia” group, then
further distinguished “core aphantasia” (16-23) from “hypophantasia”
(24-32) within it, a finer split closely related to the one this
project’s own categorical model uses (see [model
comparison](https://m-delem.github.io/aphantasiaEmotions/articles/model-comparison.html)).

None of this is a flaw in any individual study, as each threshold was a
reasonable choice given that study’s own goals. It is, however, a real
illustration of a point this project’s own results speak to directly:
VVIQ threshold conventions vary across the field, sometimes
substantially, and a pooled analysis that treats VVIQ as continuous
rather than pre-sorting everyone into study-specific categories
sidesteps that inconsistency rather than inheriting it.

## By dataset, VVIQ, and TAS-20 (alexithymia) group

``` r

p_counts <-
  all_data |>
  dplyr::bind_rows(all_data |> dplyr::mutate(study = "total")) |>
  dplyr::mutate(
    study = factor(
      study,
      levels = c("burns", "monzel", "mas", "ruby", "kvamme", "total")
    )
  ) |>
  plot_vviq_group_proportions(vviq_group_4, base_size = 13, prop_txt_size = 5)

p_props <-
  all_data |>
  summarise_aph_and_alexi(vviq_group_4) |>
  plot_alexithymia_proportions(
    vviq_group_4,
    ncol = 6,
    base_size = 13,
    prop_txt_size = 3
  )

ggpubr::ggarrange(
  p_counts,
  p_props,
  ncol = 1,
  heights = c(1.1, 1),
  labels = "AUTO",
  font.label = list(size = 24, face = "bold")
) |> suppressWarnings()
```

![Two combined panels. The top panel shows, for each of the five studies
plus an aggregate 'total' bar, how many participants fall into each of
the four VVIQ groups (aphantasia, hypophantasia, typical,
hyperphantasia). The bottom panel shows the proportion of participants
in each VVIQ group who score above the TAS-20's clinical alexithymia
cutoff.](sample-description_files/figure-html/plot-proportions-1.png)

The complete-aphantasia group (VVIQ = 16) is present in every study
except Mas & Luminet’s, whose young, French, course-credit sample
happened not to include anyone at the scale’s absolute floor, which is
worth keeping in mind when reading the [floor-group
model](https://m-delem.github.io/aphantasiaEmotions/articles/floor-group-model.html)
page’s per-study breakdown, where that study’s line is necessarily
estimated without any direct floor-group data of its own.

------------------------------------------------------------------------

**Continuing through the Extended Online Report:** this page follows
[how this study found its
shape](https://m-delem.github.io/aphantasiaEmotions/articles/how-this-study-found-its-shape.html).
To keep reading in order, continue to the [model
comparison](https://m-delem.github.io/aphantasiaEmotions/articles/model-comparison.html)
page next. Or jump to [the floor-group model, in
depth](https://m-delem.github.io/aphantasiaEmotions/articles/floor-group-model.html),
[model
diagnostics](https://m-delem.github.io/aphantasiaEmotions/articles/model-diagnostics.html),
[implementation
notes](https://m-delem.github.io/aphantasiaEmotions/articles/implementation-notes.html),
or [for those who come
after](https://m-delem.github.io/aphantasiaEmotions/articles/for-those-who-come-after.html).

------------------------------------------------------------------------

### References

Ale, E., & Burns, E. (2024, March 5). *Aphantasia and alexithymia
predict complex PTSD symptoms*. <https://doi.org/10.31234/osf.io/kj5d3>

Kvamme, T. L., Monzel, M., Nagai, Y., & Silvanto, J. (2026). When weak
imagery is worse than none: Core aphantasia and hypophantasia relate
differently to mental health, mediated by subjective interoception.
*Neuropsychologia*, *222*, 109368.
<https://doi.org/10.1016/j.neuropsychologia.2026.109368>

Mas, M. (2025). Alexithymia and mental representations: An investigation
with the reverse correlation paradigm. In *OSF*.
<https://doi.org/10.17605/OSF.IO/DZNKE>

Monzel, M., Karneboge, J., & Reuter, M. (2024). Affective processing in
aphantasia and potential overlaps with alexithymia: Mental imagery
facilitates the recognition of emotions in oneself and others.
*Biomarkers in Neuropsychiatry*, *11*, 100106.
<https://doi.org/10.1016/j.bionps.2024.100106>
