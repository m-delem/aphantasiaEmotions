# Get started

This page is a short, practical introduction to `aphantasiaEmotions` —
how to load it, what the data looks like, and how to run one basic
model. It is not the place to learn about the study’s findings; for
that, see the [Extended Online
Report](https://m-delem.github.io/aphantasiaEmotions), starting with
[how this study found its
shape](https://m-delem.github.io/aphantasiaEmotions/articles/how-this-study-found-its-shape.html).
This page is about *using the package*, for anyone who wants to
reproduce or build on the analyses directly in R.

``` r

library(aphantasiaEmotions)
```

## The data

The package ships with one built-in dataset, `all_data`: the pooled
sample from five studies, combined and cleaned for this project’s
analyses.

``` r

dplyr::glimpse(all_data)
#> Rows: 1,478
#> Columns: 16
#> $ study        <fct> burns, burns, burns, burns, burns, burns, burns, burns, b…
#> $ lang         <fct> en, en, en, en, en, en, en, en, en, en, en, en, en, en, e…
#> $ id           <fct> subj_burns_1, subj_burns_2, subj_burns_3, subj_burns_4, s…
#> $ sex          <fct> male, male, male, male, female, male, male, male, female,…
#> $ gender       <fct> female, female, female, female, male, female, female, fem…
#> $ age          <dbl> 62, 39, 45, 57, 40, 86, 59, 50, 25, 44, 49, 57, 45, 69, 4…
#> $ vviq         <dbl> 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 1…
#> $ tas          <dbl> 57, 33, 94, 30, 62, 48, 58, 67, 51, 37, 35, 39, 44, 74, 8…
#> $ tas_identify <dbl> 23, 7, 33, 7, 17, 22, 19, 30, 22, 11, 7, 11, 20, 31, 27, …
#> $ tas_describe <dbl> 16, 5, 25, 8, 19, 14, 16, 20, 13, 8, 8, 10, 8, 22, 21, 18…
#> $ tas_external <dbl> 18, 21, 36, 15, 26, 12, 23, 17, 16, 18, 20, 18, 16, 21, 3…
#> $ tas_group    <fct> typical_tas, typical_tas, alexithymia, typical_tas, alexi…
#> $ vviq_group_4 <fct> aphantasia, aphantasia, aphantasia, aphantasia, aphantasi…
#> $ vviq_group_3 <fct> aphantasia, aphantasia, aphantasia, aphantasia, aphantasi…
#> $ vviq_group_2 <fct> aphantasia, aphantasia, aphantasia, aphantasia, aphantasi…
#> $ items        <list> [<tbl_df[1 x 36]>], [<tbl_df[1 x 36]>], [<tbl_df[1 x 36]…
```

A few columns worth knowing about from the start:

- **`study`** identifies which of the five source datasets a row comes
  from (`"burns"`, `"monzel"`, `"ruby"`, `"mas"`, `"kvamme"`) — see the
  [sample
  description](https://m-delem.github.io/aphantasiaEmotions/articles/sample-description.html)
  page for what each one is.
- **`vviq`** is the raw Vividness of Visual Imagery Questionnaire total
  score (16-80, where 16 is a complete absence of voluntary visual
  imagery).
- **`tas`** is the raw Toronto Alexithymia Scale (TAS-20) total score.
- **`vviq_group_4`** and **`vviq_group_2`** are categorical groupings of
  `vviq` at different levels of granularity, used throughout this
  project’s more traditional, categorical analyses.

## Fitting a model

Most of this project’s statistical work uses
[`fit_brms_model()`](https://m-delem.github.io/aphantasiaEmotions/reference/fit_brms_model.md),
a thin wrapper around
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) with
this project’s own conventions for chains, iterations, and priors
already set as sensible defaults. Here is the simplest possible example
— a plain linear model of alexithymia as a function of imagery
vividness:

``` r

simple_model <- fit_brms_model(
  formula = tas ~ vviq,
  data = all_data,
  prior = brms::prior(normal(0, 20), class = "b")
)

summary(simple_model)
```

This example is not run on this page (fitting a real Bayesian model
takes real time), but it will work as written if you run it yourself.
See
[`?fit_brms_model`](https://m-delem.github.io/aphantasiaEmotions/reference/fit_brms_model.md)
for the full set of arguments, including how to control the number of
chains and iterations.

## Where to go next

- To reproduce a specific analysis from the paper, the [model
  comparison](https://m-delem.github.io/aphantasiaEmotions/articles/model-comparison.html)
  and [floor-group
  model](https://m-delem.github.io/aphantasiaEmotions/articles/floor-group-model.html)
  pages show the actual code behind each result, narrated in full.
- To see every function this package exports, browse the [reference
  index](https://m-delem.github.io/aphantasiaEmotions/reference/index.html).
- To understand *why* the study took the shape it did before diving into
  the numbers, start with [how this study found its
  shape](https://m-delem.github.io/aphantasiaEmotions/articles/how-this-study-found-its-shape.html).
