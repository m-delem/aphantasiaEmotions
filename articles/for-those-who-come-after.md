# For those who come after

Some of the most interesting directions this project surfaced didn’t
make it into the final analyses — not because they weren’t worth
pursuing, but because a study has to stop somewhere, and this project’s
own philosophy (see [how this study found its
shape](https://m-delem.github.io/aphantasiaEmotions/articles/how-this-study-found-its-shape.html))
is that the reasoning behind a stopping point deserves to be written
down, not quietly dropped. Each of the four ideas below has a real
motivation, a rough sense of what the next step would look like, and an
honest account of why it’s here rather than in the main report.

## Mixture regression on the VVIQ floor spike

The pooled VVIQ distribution isn’t smoothly continuous: there’s a sharp,
isolated spike at the scale’s floor (VVIQ = 16), visually distinct from
a more continuous, if irregular, remainder from about 20 to 80. The
floor-group additive model (see the [floor-group
page](https://m-delem.github.io/aphantasiaEmotions/articles/floor-group-model.html))
treats that floor group as a single, discrete category with its own
mean, which the evidence supports well, but which assumes the floor
group is internally homogeneous.

A genuinely different, and more ambitious, way to ask the same question
is finite mixture regression: rather than assuming a fixed number of
researcher-defined groups, a mixture model asks whether the *sample
itself* is a blend of several latent sub-populations, each with its own
regression relationship, estimated directly from the data rather than
assumed in advance. This is close to what one reviewer specifically
suggested, pointing to the **flexmix** R package (Leisch, 2004) as a
tool for exactly this: modelling the VVIQ-TAS relationship as
potentially modified by empirical clusters in the data, particularly
relevant if aphantasics themselves consist of distinguishable subgroups.

This is a meaningfully larger step than the floor-group model, not just
a variant of it: it’s a different *kind* of claim (multiple populations,
not one population with one distinct subgroup), and mixture models bring
real complications of their own (label-switching during estimation,
convergence sensitivity, the same challenge of choosing how many
components to fit that a purely category-driven analysis avoids). It’s
flagged here, not attempted, because taking it on properly would have
meant introducing a third major statistical paradigm into an already
substantial modelling arc. This is a deliberate scope decision, not a
technical dead end.

## Bayesian MARS (BASS)

The segmented model’s estimated knot (see the [model comparison
page](https://m-delem.github.io/aphantasiaEmotions/articles/model-comparison.html#continuous-alternatives))
was first located using
[`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html) — a fast,
frequentist implementation of Multivariate Adaptive Regression Splines
(MARS). That result was then re-estimated properly in a Bayesian
framework, but only for the single-knot structure `earth` had already
found.

A more thoroughly Bayesian version of the same idea exists: the **BASS**
package (Francom & Sansó, 2020) (Bayesian Adaptive Spline Surfaces) fits
genuinely Bayesian MARS models (Denison et al., 1998), using
reversible-jump MCMC to put a full posterior not just over a spline’s
parameters, but over its *structure*: how many knots there are and where
they sit, estimated jointly rather than fixed in advance. That’s a more
complete answer to “where does this relationship change shape” than even
the estimated-knot segmented model gives, since it doesn’t assume in
advance that there’s exactly one breakpoint.

The practical cost is real: BASS doesn’t give WAIC or LOO directly, so
comparing it formally to the brms-based models in this report would need
new work (likely a custom cross-validated predictive-density
calculation), and its prior structure (inverse-gamma and beta
hyperpriors on spline complexity) is unfamiliar territory relative to
the priors used throughout this project. None of that makes it a bad
idea — it’s a genuinely distinctive, rare thing to see worked through in
this literature, and fitting a first model is mechanically simple (a
single function call, not unlike
[`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html)). It’s
parked here specifically because *understanding and defending* the
result is the real cost, not the fitting itself, and that’s a task for
its own dedicated adventure.

## A per-study estimate of the segmented model’s knot

The segmented model’s knot is estimated once, pooling all five studies.
A natural extension is to let the knot location vary by study:
`k | study` rather than a single pooled `k`. This would directly answer
a specific, sharp question: does Kvamme et al.’s (2026) own data, on its
own, actually support their chosen threshold of VVIQ = 32 better than
the pooled estimate of about 19.5 does? A per-study knot posterior would
show, directly, which studies (if any) pull the pooled estimate toward
or away from any given value.

This is a harder model to fit than it might sound. The knot parameter
`k` sits *inside* a non-linear term (multiplying `step(vviq - k)`), so
letting it vary by study means the sampler has to jointly resolve,
separately for each of the five studies, both *where* the breakpoint is
and *how steep* each segment is using only that study’s own data. The
smallest study in this project’s pool (n = 105) is a fraction of what
the pooled estimate needed to be well-identified in the first place;
asking a study that size to support its own independent knot estimate is
a real statistical stretch, likely requiring careful priors, generous
`adapt_delta`/`max_treedepth` settings well beyond what the pooled model
needed (see [implementation
notes](https://m-delem.github.io/aphantasiaEmotions/articles/implementation-notes.html#the-segmented-models-estimated-knot)
for what the *pooled* estimated-knot model already required), and
probably more debugging than any other model in this report. It’s a
genuinely interesting question with a genuinely hard model behind it,
but a question worth flagging clearly *for those who come after*,
including its difficulty, rather than downplaying it.

------------------------------------------------------------------------

**This is the last page of the Extended Online Report’s narrative arc.**
If you’ve read through from the start, thank you for following the whole
story. Two technical pages remain, for anyone building on this project’s
code directly: [model
diagnostics](https://m-delem.github.io/aphantasiaEmotions/articles/model-diagnostics.html)
and [implementation
notes](https://m-delem.github.io/aphantasiaEmotions/articles/implementation-notes.html).
The [superseded
models](https://m-delem.github.io/aphantasiaEmotions/articles/superseded-models.html)
page contains the legacy of the 4-group and GAM models that were once
central to this study’s first iteration. Otherwise, you can jump back to
[how this study found its
shape](https://m-delem.github.io/aphantasiaEmotions/articles/how-this-study-found-its-shape.html),
or return to the [package
homepage](https://m-delem.github.io/aphantasiaEmotions/).

------------------------------------------------------------------------

## References

Denison, D. G. T., Mallick, B. K., & Smith, A. F. M. (1998). Bayesian
MARS. *Statistics and Computing*, *8*(4), 337–346.
<https://doi.org/10.1023/A:1008824606259>

Francom, D., & Sansó, B. (2020). BASS: An R Package for Fitting and
Performing Sensitivity Analysis of Bayesian Adaptive Spline Surfaces.
*Journal of Statistical Software*, *94*(1), 1–36.
<https://doi.org/10.18637/jss.v094.i08>

Leisch, F. (2004). FlexMix: A General Framework for Finite Mixture
Models and Latent Class Regression in R. *Journal of Statistical
Software*, *11*(8), 1–18. <https://doi.org/10.18637/jss.v011.i08>
