
<!-- README.md is generated from README.Rmd. Please edit that file -->

# analysistools

<!-- badges: start -->

[![check-standard](https://github.com/impact-initiatives/analysistools/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/impact-initiatives/analysistools/actions/workflows/check-standard.yaml)
[![Codecov test
coverage](https://codecov.io/gh/impact-initiatives/analysistools/branch/main/graph/badge.svg)](https://app.codecov.io/gh/impact-initiatives/analysistools?branch=main)
<!-- badges: end -->

The goal of analysistools is to …

## Installation

You can install the development version of analysistools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("impact-initiatives/analysistools")
```

## Example

This is a basic example which shows you how to calculate the mean:

``` r
library(analysistools)
somedata <- data.frame(aa = 1:10,
                      bb = rep(c("a","b"),5),
                      weights = rep(c(.5,1.5),5),
                      stratas = rep(c("strata_a", "strata_b"),5))
dap_mean <- data.frame(group_var = c(NA, "bb"),
                      analysis_var = c("aa", "aa"),
                      level = c(.95, .95))
me_design <- srvyr::as_survey_design(somedata, strata = stratas, weights = weights)
create_analysis_mean(me_design, dap_mean[1,])
#> # A tibble: 1 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 mean          aa           <NA>               <NA>      <NA>             5.75
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_mean(me_design, dap_mean[2,])
#> # A tibble: 2 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 mean          aa           <NA>               bb        a                   5
#> 2 mean          aa           <NA>               bb        b                   6
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```

This is a basic example which shows you how to calculate the proportion
for select one:

``` r
somedata <- data.frame(groups = sample(c("group_a", "group_b"), size = 100,
                       replace = TRUE),
                       value = sample(c("a", "b", "c"), size = 100, replace = TRUE,
                       prob = c(.6,.4,.1)))
dap <- data.frame(group_var = c(NA, "groups"),
                  analysis_var = c("value", "value"),
                  level = c(0.95, 0.95))
create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups), dap[1,])
#> # A tibble: 3 × 13
#>   analysis_type  analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>          <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 prop_select_o… value        a                  <NA>      <NA>             0.54
#> 2 prop_select_o… value        b                  <NA>      <NA>             0.37
#> 3 prop_select_o… value        c                  <NA>      <NA>             0.09
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups), dap[2,])
#> # A tibble: 6 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value   stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>            <dbl>
#> 1 prop_select_… value        a                  groups    group_a         0.538 
#> 2 prop_select_… value        b                  groups    group_a         0.404 
#> 3 prop_select_… value        c                  groups    group_a         0.0577
#> 4 prop_select_… value        a                  groups    group_b         0.542 
#> 5 prop_select_… value        b                  groups    group_b         0.333 
#> 6 prop_select_… value        c                  groups    group_b         0.125 
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```
