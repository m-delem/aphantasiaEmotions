# Compute counts and proportions of alexithymia groups within VVIQ groups

This function computes the counts and proportions of alexithymia groups
within specified grouping variables (e.g., VVIQ groups) for each study
as well as for the combined data across all studies.

## Usage

``` r
summarise_aph_and_alexi(df, ...)
```

## Arguments

- df:

  Data frame containing the data with `tas_group` and grouping
  variables.

- ...:

  Grouping variables to summarise the data by (e.g., vviq_group_4).

## Value

A data frame with counts and proportions of alexithymia groups within
the specified grouping variables for each study and for the combined
data.

## Examples

``` r
summarise_aph_and_alexi(all_data, vviq_group_4) |> head()
#> # A tibble: 6 × 6
#> # Groups:   vviq_group_4, study [4]
#>   vviq_group_4 tas_group   study      n n_group  prop
#>   <fct>        <fct>       <fct>  <int>   <int> <dbl>
#> 1 aphantasia   alexithymia burns     18      60 0.3  
#> 2 aphantasia   alexithymia monzel     5      20 0.25 
#> 3 aphantasia   alexithymia kvamme    17      54 0.315
#> 4 aphantasia   typical_tas burns     42      60 0.7  
#> 5 aphantasia   typical_tas monzel    15      20 0.75 
#> 6 aphantasia   typical_tas ruby      12      12 1    
summarise_aph_and_alexi(all_data, vviq_group_2) |> head()
#> # A tibble: 6 × 6
#> # Groups:   vviq_group_2, study [5]
#>   vviq_group_2 tas_group   study      n n_group  prop
#>   <fct>        <fct>       <fct>  <int>   <int> <dbl>
#> 1 aphantasia   alexithymia burns     23      75 0.307
#> 2 aphantasia   alexithymia monzel     9      30 0.3  
#> 3 aphantasia   alexithymia mas        1       2 0.5  
#> 4 aphantasia   alexithymia ruby       5      25 0.2  
#> 5 aphantasia   alexithymia kvamme    61     153 0.399
#> 6 aphantasia   typical_tas burns     52      75 0.693
```
