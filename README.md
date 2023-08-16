
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

``` r
library(analysistools)
```

### How to add weights

``` r
shorter_df <- analysistools_MSNA_template_data[, c(
  "admin1",
  "admin2",
  "expenditure_debt",
  "income_v1_salaried_work",
  "wash_drinkingwatersource"
)]

example_sample <- data.frame(
  strata = c("admin1a", "admin1b", "admin1c"),
  population = c(30000, 50000, 80000)
)

weighted_shorter_df <- shorter_df %>%
  add_weights(example_sample,
    strata_column_dataset = "admin1",
    strata_column_sample = "strata",
    population_column = "population"
  )

weighted_shorter_df %>% head()
#>    admin1  admin2 expenditure_debt income_v1_salaried_work
#> 1 admin1b admin2a               22                      18
#> 2 admin1c admin2b               18                      24
#> 3 admin1c admin2b               18                      22
#> 4 admin1c admin2b               23                      20
#> 5 admin1c admin2a               20                      21
#> 6 admin1c admin2b               23                      22
#>   wash_drinkingwatersource  weights
#> 1            tanker_trucks 1.157407
#> 2            bottled_water 1.190476
#> 3              water_kiosk 1.190476
#> 4                dont_know 1.190476
#> 5                dont_know 1.190476
#> 6              water_kiosk 1.190476
```

### How to perform a descriptive analysis (mean, median, proportions)

The *create_analysis* function needs a survey design from *srvyr*.

``` r
example_design <- srvyr::as_survey(weighted_shorter_df, strata = admin1, weights = weights)
```

If only the design is provided, it will perform mean, median and
proportions.

``` r
ex1_results <- create_analysis(.design = example_design)
#> Joining with `by = join_by(type)`
```

It should return an object with 3 elements: - the results table (in a
long format and analysis key), - the dataset used, - the list of
analysis performed.

``` r
names(ex1_results)
#> [1] "results_table" "dataset"       "loa"
```

``` r
ex1_results[["results_table"]] %>% head()
#> # A tibble: 6 × 13
#>   analysis_type  analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>          <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 prop_select_o… admin1       admin1a            <NA>      <NA>            0.188
#> 2 prop_select_o… admin1       admin1b            <NA>      <NA>            0.313
#> 3 prop_select_o… admin1       admin1c            <NA>      <NA>            0.5  
#> 4 prop_select_o… admin2       admin2a            <NA>      <NA>            0.284
#> 5 prop_select_o… admin2       admin2b            <NA>      <NA>            0.385
#> 6 prop_select_o… admin2       admin2c            <NA>      <NA>            0.331
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```

``` r
ex1_results[["loa"]] %>% head()
#>     analysis_type            analysis_var group_var level
#> 1 prop_select_one                  admin1      <NA>  0.95
#> 2 prop_select_one                  admin2      <NA>  0.95
#> 3            mean        expenditure_debt      <NA>  0.95
#> 4          median        expenditure_debt      <NA>  0.95
#> 5            mean income_v1_salaried_work      <NA>  0.95
#> 6          median income_v1_salaried_work      <NA>  0.95
```

#### Grouping variables

The group_var can be used to defined the different grouping, independent
variables. For example: - one variable

``` r
ex2_results <- create_analysis(.design = srvyr::as_survey(shorter_df), group_var = "admin1")
#> Joining with `by = join_by(type)`
ex2_results[["loa"]]
#>      analysis_type             analysis_var group_var level
#> 1  prop_select_one                   admin1      <NA>  0.95
#> 2  prop_select_one                   admin2      <NA>  0.95
#> 3             mean         expenditure_debt      <NA>  0.95
#> 4           median         expenditure_debt      <NA>  0.95
#> 5             mean  income_v1_salaried_work      <NA>  0.95
#> 6           median  income_v1_salaried_work      <NA>  0.95
#> 7  prop_select_one wash_drinkingwatersource      <NA>  0.95
#> 8  prop_select_one                   admin2    admin1  0.95
#> 9             mean         expenditure_debt    admin1  0.95
#> 10          median         expenditure_debt    admin1  0.95
#> 11            mean  income_v1_salaried_work    admin1  0.95
#> 12          median  income_v1_salaried_work    admin1  0.95
#> 13 prop_select_one wash_drinkingwatersource    admin1  0.95
```

- two variables separately

``` r
ex3_results <- create_analysis(.design = srvyr::as_survey(shorter_df), group_var = c("admin1", "admin2"))
#> Joining with `by = join_by(type)`
#> ■■■■■■■■■■■■■■ 42% | ETA: 1s
#> ■■■■■■■■■■■■■■■ 47% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■■ 68% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■■■ 74% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■ 79% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 95% | ETA: 0s
ex3_results[["loa"]]
#>      analysis_type             analysis_var group_var level
#> 1  prop_select_one                   admin1      <NA>  0.95
#> 2  prop_select_one                   admin2      <NA>  0.95
#> 3             mean         expenditure_debt      <NA>  0.95
#> 4           median         expenditure_debt      <NA>  0.95
#> 5             mean  income_v1_salaried_work      <NA>  0.95
#> 6           median  income_v1_salaried_work      <NA>  0.95
#> 7  prop_select_one wash_drinkingwatersource      <NA>  0.95
#> 8  prop_select_one                   admin2    admin1  0.95
#> 9             mean         expenditure_debt    admin1  0.95
#> 10          median         expenditure_debt    admin1  0.95
#> 11            mean  income_v1_salaried_work    admin1  0.95
#> 12          median  income_v1_salaried_work    admin1  0.95
#> 13 prop_select_one wash_drinkingwatersource    admin1  0.95
#> 14 prop_select_one                   admin1    admin2  0.95
#> 15            mean         expenditure_debt    admin2  0.95
#> 16          median         expenditure_debt    admin2  0.95
#> 17            mean  income_v1_salaried_work    admin2  0.95
#> 18          median  income_v1_salaried_work    admin2  0.95
#> 19 prop_select_one wash_drinkingwatersource    admin2  0.95
```

- two variables combined

``` r
ex4_results <- create_analysis(.design = srvyr::as_survey(shorter_df), group_var = "admin1, admin2")
#> Joining with `by = join_by(type)`
ex4_results[["loa"]]
#>      analysis_type             analysis_var      group_var level
#> 1  prop_select_one                   admin1           <NA>  0.95
#> 2  prop_select_one                   admin2           <NA>  0.95
#> 3             mean         expenditure_debt           <NA>  0.95
#> 4           median         expenditure_debt           <NA>  0.95
#> 5             mean  income_v1_salaried_work           <NA>  0.95
#> 6           median  income_v1_salaried_work           <NA>  0.95
#> 7  prop_select_one wash_drinkingwatersource           <NA>  0.95
#> 8             mean         expenditure_debt admin1, admin2  0.95
#> 9           median         expenditure_debt admin1, admin2  0.95
#> 10            mean  income_v1_salaried_work admin1, admin2  0.95
#> 11          median  income_v1_salaried_work admin1, admin2  0.95
#> 12 prop_select_one wash_drinkingwatersource admin1, admin2  0.95
```

### How to perform a descriptive analysis with a *list of analysis*

``` r
ex5_results <- create_analysis(.design = srvyr::as_survey(shorter_df), loa = analysistools_loa)
ex5_results[["loa"]]
#>      analysis_type             analysis_var group_var level
#> 1  prop_select_one                   admin1      <NA>  0.95
#> 2             mean  income_v1_salaried_work      <NA>  0.95
#> 3           median  income_v1_salaried_work      <NA>  0.95
#> 4             mean         expenditure_debt      <NA>  0.95
#> 5           median         expenditure_debt      <NA>  0.95
#> 6  prop_select_one wash_drinkingwatersource      <NA>  0.95
#> 7             mean  income_v1_salaried_work    admin1  0.95
#> 8           median  income_v1_salaried_work    admin1  0.95
#> 9             mean         expenditure_debt    admin1  0.95
#> 10          median         expenditure_debt    admin1  0.95
#> 11 prop_select_one wash_drinkingwatersource    admin1  0.95
```

### How to perform specfic analysis

#### Mean

This is a basic example which shows you how to calculate the mean:

``` r
somedata <- data.frame(
  aa = 1:10,
  bb = rep(c("a", "b"), 5),
  weights = rep(c(.5, 1.5), 5),
  stratas = rep(c("strata_a", "strata_b"), 5)
)
me_design <- srvyr::as_survey(somedata)
create_analysis_mean(me_design, analysis_var = "aa")
#> # A tibble: 1 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 mean          aa           <NA>               <NA>      <NA>              5.5
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_mean(me_design, group_var = "bb", analysis_var = "aa")
#> # A tibble: 2 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 mean          aa           <NA>               bb        a                   5
#> 2 mean          aa           <NA>               bb        b                   6
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
me_design_w <- srvyr::as_survey(somedata, weights = weights)
create_analysis_mean(me_design_w, analysis_var = "aa")
#> # A tibble: 1 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 mean          aa           <NA>               <NA>      <NA>             5.75
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_mean(me_design_w, group_var = "bb", analysis_var = "aa")
#> # A tibble: 2 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 mean          aa           <NA>               bb        a                   5
#> 2 mean          aa           <NA>               bb        b                   6
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```

#### Median

This is a basic example which shows you how to calculate the median:

``` r
somedata <- data.frame(
  aa = 1:10,
  bb = rep(c("a", "b"), 5),
  weights = rep(c(.5, 1.5), 5),
  stratas = rep(c("strata_a", "strata_b"), 5)
)
me_design <- srvyr::as_survey(somedata)
create_analysis_median(me_design, analysis_var = "aa")
#> # A tibble: 1 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 median        aa           <NA>               <NA>      <NA>                5
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_median(me_design, group_var = "bb", analysis_var = "aa")
#> # A tibble: 2 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 median        aa           <NA>               bb        a                   5
#> 2 median        aa           <NA>               bb        b                   6
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
me_design_w <- srvyr::as_survey(somedata, weights = weights)
create_analysis_median(me_design_w, analysis_var = "aa")
#> # A tibble: 1 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 median        aa           <NA>               <NA>      <NA>                6
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_median(me_design_w, group_var = "bb", analysis_var = "aa")
#> # A tibble: 2 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 median        aa           <NA>               bb        a                   5
#> 2 median        aa           <NA>               bb        b                   6
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```

#### Proportion

##### Select one

This is a basic example which shows you how to calculate the proportion
for select one:

``` r
somedata <- data.frame(
  groups = sample(c("group_a", "group_b"),
    size = 100,
    replace = TRUE
  ),
  value = sample(c("a", "b", "c"),
    size = 100, replace = TRUE,
    prob = c(.6, .4, .1)
  )
)

create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups),
  group_var = NA,
  analysis_var = "value",
  level = .95
)
#> # A tibble: 3 × 13
#>   analysis_type  analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>          <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 prop_select_o… value        a                  <NA>      <NA>             0.6 
#> 2 prop_select_o… value        b                  <NA>      <NA>             0.31
#> 3 prop_select_o… value        c                  <NA>      <NA>             0.09
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups),
  group_var = "groups",
  analysis_var = "value",
  level = .95
)
#> # A tibble: 6 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value   stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>            <dbl>
#> 1 prop_select_… value        a                  groups    group_a         0.571 
#> 2 prop_select_… value        b                  groups    group_a         0.327 
#> 3 prop_select_… value        c                  groups    group_a         0.102 
#> 4 prop_select_… value        a                  groups    group_b         0.627 
#> 5 prop_select_… value        b                  groups    group_b         0.294 
#> 6 prop_select_… value        c                  groups    group_b         0.0784
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```

##### Select multiple

``` r
somedata <- data.frame(
  groups = sample(c("group_a", "group_b"), size = 100, replace = T),
  smvar = rep(NA_character_, 100),
  smvar.option1 = sample(c(TRUE,FALSE), size = 100, replace = T, prob = c(.7,.3)),
  smvar.option2 = sample(c(TRUE,FALSE), size = 100, replace = T, prob = c(.6,.4)),
  smvar.option3 = sample(c(TRUE,FALSE), size = 100, replace = T, prob = c(.1,.9)),
  smvar.option4 = sample(c(TRUE,FALSE), size = 100, replace = T, prob = c(.8,.2)),
  uuid = 1:100 %>% as.character()) %>%
  cleaningtools::recreate_parent_column(uuid = "uuid", sm_sep = ".")
#> groups
#> smvar
#> smvar.option1
#> smvar.option2
#> smvar.option3
#> smvar.option4
#> groups
#> smvar.option1
#> smvar.option2
#> smvar.option3
#> smvar.option4
#> groups
#> smvar
#> smvar.option1
#> smvar.option2
#> smvar.option3
#> smvar.option4

somedata <- somedata$data_with_fix_concat
create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
                                     group_var = NA,
                                     analysis_var = "smvar",
                                     level = 0.95)
#> # A tibble: 4 × 13
#>   analysis_type  analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>          <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 prop_select_m… smvar        option1            <NA>      <NA>             0.68
#> 2 prop_select_m… smvar        option2            <NA>      <NA>             0.65
#> 3 prop_select_m… smvar        option3            <NA>      <NA>             0.1 
#> 4 prop_select_m… smvar        option4            <NA>      <NA>             0.84
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <dbl>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>

create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
                                     group_var = "groups",
                                     analysis_var = "smvar",
                                     level = 0.95)
#> # A tibble: 8 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value   stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>            <dbl>
#> 1 prop_select_… smvar        option1            groups    group_a         0.661 
#> 2 prop_select_… smvar        option2            groups    group_a         0.695 
#> 3 prop_select_… smvar        option3            groups    group_a         0.119 
#> 4 prop_select_… smvar        option4            groups    group_a         0.881 
#> 5 prop_select_… smvar        option1            groups    group_b         0.707 
#> 6 prop_select_… smvar        option2            groups    group_b         0.585 
#> 7 prop_select_… smvar        option3            groups    group_b         0.0732
#> 8 prop_select_… smvar        option4            groups    group_b         0.780 
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <dbl>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```

#### Ratios

This is a basic example which shows you how to calculate the ratio
between 2 numeric variables:

``` r
school_ex <- data.frame(
  hh = c("hh1", "hh2", "hh3", "hh4"),
  num_children = c(3, 0, 2, NA),
  num_enrolled = c(3, NA, 0, NA),
  num_attending = c(1, NA, NA, NA),
  group = c("a", "a", "b", "b")
)
me_design <- srvyr::as_survey(school_ex)
```

Default value will give a ratio of 0.2 as there are 1 child out of 5
attending school. In the hh3, the NA is present because there is a skip
logic, there cannot be a child attending as none are enrolled. The
number of household counted, n, is equal to 2, as there are 2 households
only having child.

``` r
create_analysis_ratio(me_design,
  analysis_var_numerator = "num_attending",
  analysis_var_denominator = "num_children"
)
#> # A tibble: 1 × 13
#>   analysis_type analysis_var  analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>         <chr>              <chr>     <chr>           <dbl>
#> 1 ratio         num_attendin… NA ~/~ NA          <NA>      <NA>              0.2
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```

If numerator_NA_to_0 is set to FALSE, ratio will be 1/3, as hh3 with 2
children and NA for attending will be removed with the na.rm = T inside
the survey_ratio calculation. The number of household used in the
calculation is 1.

``` r
create_analysis_ratio(me_design,
  analysis_var_numerator = "num_attending",
  analysis_var_denominator = "num_children",
  numerator_NA_to_0 = FALSE
)
#> Warning: There were 2 warnings in `dplyr::summarise()`.
#> The first warning was:
#> ℹ In argument: `srvyr::survey_ratio(...)`.
#> Caused by warning in `qt()`:
#> ! NaNs produced
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
#> # A tibble: 1 × 13
#>   analysis_type analysis_var  analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>         <chr>              <chr>     <chr>           <dbl>
#> 1 ratio         num_attendin… NA ~/~ NA          <NA>      <NA>            0.333
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```

If filter_denominator_0 is set to FALSE, ratio will be 0.2 as there are
1 child out of 5 attending school. In the hh3, the NA is present because
there is a skip logic, there cannot be a child attending as none are
enrolled. The number of household counted, n, is equal to 3 instead 2.
The household with 0 child is counted in the n.

``` r
create_analysis_ratio(me_design,
  analysis_var_numerator = "num_attending",
  analysis_var_denominator = "num_children",
  numerator_NA_to_0 = FALSE
)
#> Warning: There were 2 warnings in `dplyr::summarise()`.
#> The first warning was:
#> ℹ In argument: `srvyr::survey_ratio(...)`.
#> Caused by warning in `qt()`:
#> ! NaNs produced
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
#> # A tibble: 1 × 13
#>   analysis_type analysis_var  analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>         <chr>              <chr>     <chr>           <dbl>
#> 1 ratio         num_attendin… NA ~/~ NA          <NA>      <NA>            0.333
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```

For weigths and group:

``` r
set.seed(8988)
somedata <- data.frame(
  groups = rep(c("a", "b"), 50),
  children_518 = sample(0:5, 100, replace = TRUE),
  children_enrolled = sample(0:5, 100, replace = TRUE)
) %>%
  dplyr::mutate(children_enrolled = ifelse(children_enrolled > children_518,
    children_518,
    children_enrolled
  ))
somedata[["weights"]] <- ifelse(somedata$groups == "a", 1.33, .67)
create_analysis_ratio(srvyr::as_survey(somedata, weights = weights, strata = groups),
  group_var = NA,
  analysis_var_numerator = "children_enrolled",
  analysis_var_denominator = "children_518",
  level = 0.95
)
#> # A tibble: 1 × 13
#>   analysis_type analysis_var  analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>         <chr>              <chr>     <chr>           <dbl>
#> 1 ratio         children_enr… NA ~/~ NA          <NA>      <NA>            0.639
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_ratio(srvyr::as_survey(somedata, weights = weights, strata = groups),
  group_var = "groups",
  analysis_var_numerator = "children_enrolled",
  analysis_var_denominator = "children_518",
  level = 0.95
)
#> # A tibble: 2 × 13
#>   analysis_type analysis_var  analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>         <chr>              <chr>     <chr>           <dbl>
#> 1 ratio         children_enr… NA ~/~ NA          groups    a               0.670
#> 2 ratio         children_enr… NA ~/~ NA          groups    b               0.578
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```
