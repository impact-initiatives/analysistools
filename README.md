
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
#>   analysis…¹ analy…² analy…³ group…⁴ group…⁵  stat stat_…⁶ stat_…⁷     n n_total
#>   <chr>      <chr>   <chr>   <chr>   <chr>   <dbl>   <dbl>   <dbl> <int>   <int>
#> 1 mean       aa      <NA>    <NA>    <NA>     5.75    3.17    8.33    10      10
#> # … with 3 more variables: n_w <dbl>, n_w_total <dbl>, analysis_key <chr>, and
#> #   abbreviated variable names ¹​analysis_type, ²​analysis_var,
#> #   ³​analysis_var_value, ⁴​group_var, ⁵​group_var_value, ⁶​stat_low, ⁷​stat_upp
create_analysis_mean(me_design, dap_mean[2,])
#> # A tibble: 2 × 13
#>   analysis…¹ analy…² analy…³ group…⁴ group…⁵  stat stat_…⁶ stat_…⁷     n n_total
#>   <chr>      <chr>   <chr>   <chr>   <chr>   <dbl>   <dbl>   <dbl> <int>   <int>
#> 1 mean       aa      <NA>    bb      a           5    1.74    8.26     5       5
#> 2 mean       aa      <NA>    bb      b           6    2.74    9.26     5       5
#> # … with 3 more variables: n_w <dbl>, n_w_total <dbl>, analysis_key <chr>, and
#> #   abbreviated variable names ¹​analysis_type, ²​analysis_var,
#> #   ³​analysis_var_value, ⁴​group_var, ⁵​group_var_value, ⁶​stat_low, ⁷​stat_upp
```
