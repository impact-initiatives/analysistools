
<!-- README.md is generated from README.Rmd. Please edit that file -->

# analysistools

<!-- badges: start -->

[![check-standard](https://github.com/impact-initiatives/analysistools/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/impact-initiatives/analysistools/actions/workflows/check-standard.yaml)
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
                      weights = rep(c(.5,1.5),5))
dap_mean <- data.frame(group = c(NA, "bb"),
                      question_name = c("aa", "aa"),
                      level = c(.95, .95))
me_design <- srvyr::as_survey(somedata)
calculate_mean(me_design, dap_mean[1,])
#> # A tibble: 1 × 8
#>    stat stat_low stat_upp group name  choice analysis_type groupped_by
#>   <dbl>    <dbl>    <dbl> <chr> <chr> <lgl>  <chr>         <lgl>      
#> 1   5.5     3.33     7.67 <NA>  aa    NA     mean          NA
calculate_mean(me_design, dap_mean[2,])
#> # A tibble: 2 × 8
#>   groupped_by  stat stat_low stat_upp group name  choice analysis_type
#>   <chr>       <dbl>    <dbl>    <dbl> <chr> <chr> <lgl>  <chr>        
#> 1 a               5     1.98     8.02 bb    aa    NA     mean         
#> 2 b               6     2.98     9.02 bb    aa    NA     mean
```
