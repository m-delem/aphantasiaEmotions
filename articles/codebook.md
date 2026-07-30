# Codebook

This page documents every column in `all_data`, the pooled dataset
described on the [sample
description](https://m-delem.github.io/aphantasiaEmotions/articles/sample-description.html)
page. It is meant to serve the same purpose as the data dictionaries
shared alongside the flat `merged_data.xlsx` file archived on
[OSF](https://osf.io/), for anyone who wants to reuse the data without
also pulling in this package.

The tables below are computed directly from `all_data` rather than typed
out by hand, so they cannot drift out of sync with the data itself. A
machine-readable version of this same information, following the
[Psych-DS](https://psych-ds.github.io/) `dataset_description.json`
convention, is archived alongside the data on OSF.

## Core variables

| Variable | Description | Type | Range / levels | N missing |
|:---|:---|:---|:---|---:|
| study | Study of origin. | string | burns, monzel, mas, ruby, kvamme | 0 |
| lang | Language in which the study was administered. | string | en, fr | 0 |
| id | Unique participant identifier (“subj\_\_“), unique across the whole pooled dataset. | string | subj\_\[study\]\_\[number\] | 0 |
| sex | Sex assigned at birth, as reported by the participant. | string | female, male, other | 1 |
| gender | Gender identity, as reported by the participant, where collected separately from sex. | string | female, male, other | 1 |
| age | Participant age in years, self-reported. | integer | 10-86 | 3 |
| vviq | Total score on the VVIQ (Marks, 1973), summed across all 16 items. Lower scores indicate weaker/absent visual imagery. | integer | 16-80 | 0 |
| tas | Total score on the TAS-20 (Bagby et al., 1994), summed across all 20 items (already reverse-scored where applicable). Higher scores indicate greater alexithymia. | integer | 20-94 | 0 |
| tas_identify | Difficulty Identifying Feelings (DIF) subscale of the TAS-20. Sum of items 1, 3, 6, 7, 9, 13, 14. | integer | 7-35 | 0 |
| tas_describe | Difficulty Describing Feelings (DDF) subscale of the TAS-20. Sum of items 2, 4, 11, 12, 17. | integer | 5-25 | 0 |
| tas_external | Externally Oriented Thinking (EOT) subscale of the TAS-20. Sum of items 5, 8, 10, 15, 16, 18, 19, 20. | integer | 8-36 | 0 |
| tas_group | Alexithymia status from the standard TAS-20 cutoff (total \>= 61 = alexithymia). | string | alexithymia, typical_tas | 0 |
| vviq_group_4 | VVIQ group, 4 levels: aphantasia, hypophantasia, typical, hyperphantasia. | string | aphantasia, hypophantasia, typical, hyperphantasia | 0 |
| vviq_group_3 | VVIQ group, 3 levels: aphantasia, hypophantasia, typical (typical and hyperphantasia collapsed). | string | aphantasia, hypophantasia, typical | 0 |
| vviq_group_2 | VVIQ group, 2 levels: aphantasia, typical (broadest split). | string | aphantasia, typical | 0 |

## VVIQ item-level responses (`vviq_q1`-`vviq_q16`)

The VVIQ (Marks, 1973) has no subscales; its 16 items are grouped into 4
scenes of 4 items each (a relative or friend you often see; the rising
sun; the front of a familiar shop; a country scene with trees,
mountains, and a lake), each rated 1 (“perfectly clear and as vivid as
normal vision”) to 5 (“no image at all”). **Item wording is not
reproduced here**, as it is copyrighted by the scale’s author — see
Marks (1973) for the original items.

| Variable | Scene | Type | Range / levels | N missing |
|:---|:---|:---|:---|---:|
| vviq_q1 | a relative or friend you often see | integer | 1-5 | 105 |
| vviq_q2 | a relative or friend you often see | integer | 1-5 | 105 |
| vviq_q3 | a relative or friend you often see | integer | 1-5 | 105 |
| vviq_q4 | a relative or friend you often see | integer | 1-5 | 105 |
| vviq_q5 | the rising sun | integer | 1-5 | 105 |
| vviq_q6 | the rising sun | integer | 1-5 | 105 |
| vviq_q7 | the rising sun | integer | 1-5 | 105 |
| vviq_q8 | the rising sun | integer | 1-5 | 105 |
| vviq_q9 | the front of a familiar shop | integer | 1-5 | 105 |
| vviq_q10 | the front of a familiar shop | integer | 1-5 | 105 |
| vviq_q11 | the front of a familiar shop | integer | 1-5 | 105 |
| vviq_q12 | the front of a familiar shop | integer | 1-5 | 105 |
| vviq_q13 | a country scene with trees, mountains, and a lake | integer | 1-5 | 105 |
| vviq_q14 | a country scene with trees, mountains, and a lake | integer | 1-5 | 105 |
| vviq_q15 | a country scene with trees, mountains, and a lake | integer | 1-5 | 105 |
| vviq_q16 | a country scene with trees, mountains, and a lake | integer | 1-5 | 105 |

Item-level VVIQ data are available for the burns, mas, ruby, and kvamme
studies. They are missing (`NA`) for all 105 Monzel participants, for
whom only total scores were shared.

## TAS-20 item-level responses (`tas_q1`-`tas_q20`)

The TAS-20 (Bagby et al., 1994) has 20 items rated 1 (“strongly
disagree”) to 5 (“strongly agree”), assigned to the three sub-scales
described above. **Item wording is not reproduced here**, as it is
copyrighted by the scale’s authors — see Bagby et al. (1994) for the
original items.

Five items (4, 5, 10, 18, 19) are reverse-keyed on the original scale.
**Values in `tas_q4`, `tas_q5`, `tas_q10`, `tas_q18`, and `tas_q19` in
`all_data`/`merged_data.xlsx` are already reverse-scored** — summing
`tas_q1`-`tas_q20` directly reproduces the `tas` total column, with no
further reversal needed.

| Variable | Subscale | Reverse-keyed          | Type    | Range / levels | N missing |
|:---------|:---------|:-----------------------|:--------|:---------------|----------:|
| tas_q1   | DIF      |                        | integer | 1-5            |       105 |
| tas_q2   | DDF      |                        | integer | 1-5            |       105 |
| tas_q3   | DIF      |                        | integer | 1-5            |       105 |
| tas_q4   | DDF      | yes (already reversed) | integer | 1-5            |       105 |
| tas_q5   | EOT      | yes (already reversed) | integer | 1-5            |       105 |
| tas_q6   | DIF      |                        | integer | 1-5            |       105 |
| tas_q7   | DIF      |                        | integer | 1-5            |       105 |
| tas_q8   | EOT      |                        | integer | 1-5            |       105 |
| tas_q9   | DIF      |                        | integer | 1-5            |       105 |
| tas_q10  | EOT      | yes (already reversed) | integer | 1-5            |       105 |
| tas_q11  | DDF      |                        | integer | 1-5            |       105 |
| tas_q12  | DDF      |                        | integer | 1-5            |       105 |
| tas_q13  | DIF      |                        | integer | 1-5            |       105 |
| tas_q14  | DIF      |                        | integer | 1-5            |       105 |
| tas_q15  | EOT      |                        | integer | 1-5            |       105 |
| tas_q16  | EOT      |                        | integer | 1-5            |       105 |
| tas_q17  | DDF      |                        | integer | 1-5            |       105 |
| tas_q18  | EOT      | yes (already reversed) | integer | 1-5            |       105 |
| tas_q19  | EOT      | yes (already reversed) | integer | 1-5            |       105 |
| tas_q20  | EOT      |                        | integer | 1-5            |       105 |

Item-level TAS-20 data are available for the same four studies as the
VVIQ items above, and missing for the same 105 Monzel participants.

## A note on `sex` and `gender`

For most participants, `sex` and `gender` coincide. The Burns study
collected both constructs separately, and a small number of participants
(n = 4) reported a sex and gender that differ.

------------------------------------------------------------------------

**Continuing through the Extended Online Report:** this page is a
technical reference, linked to from the narrative pages rather than
meant to be read start to finish. See the [sample
description](https://m-delem.github.io/aphantasiaEmotions/articles/sample-description.html)
page for how the five studies were combined, or [implementation
notes](https://m-delem.github.io/aphantasiaEmotions/articles/implementation-notes.html)
for the modelling code’s conventions.

## References

Bagby, R. M., Parker, J. D. A., & Taylor, G. J. (1994). The twenty-item
Toronto Alexithymia Scale-I. Item selection and cross-validation of the
factor structure. *Journal of Psychosomatic Research*, *38*(1), 23–32.
<https://doi.org/10.1016/0022-3999(94)90005-1>

Marks, D. F. (1973). Visual imagery differences in the recall of
pictures. *British Journal of Psychology*, *64*(1), 17–24.
<https://doi.org/10.1111/j.2044-8295.1973.tb01322.x>
