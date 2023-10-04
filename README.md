
<!-- README.md is generated from README.Rmd. Please edit that file -->

# analysistools

<!-- badges: start -->

[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
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
  "wash_drinkingwatersource", 
  grep("edu_learning_conditions_reasons_v1", names(analysistools_MSNA_template_data), value = T)
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
#>   wash_drinkingwatersource
#> 1            tanker_trucks
#> 2            bottled_water
#> 3              water_kiosk
#> 4                dont_know
#> 5                dont_know
#> 6              water_kiosk
#>                                                                                                                                  edu_learning_conditions_reasons_v1
#> 1 curriculum_not_adapted poor_wash discrimination displacement language_barriers curriculum_not_adapted_remote unreliable_technology lack_equipment other dont_know
#> 2                 overcrowding curriculum_not_adapted lack_teachers lack_qualified_staff lack_materials discrimination curriculum_not_adapted_remote lack_equipment
#> 3                                                                           overcrowding lack_materials discrimination curriculum_not_adapted_remote lack_equipment
#> 4                                            overcrowding curriculum_not_adapted lack_qualified_staff displacement language_barriers lack_equipment other dont_know
#> 5                                                                          overcrowding lack_materials language_barriers unreliable_technology lack_equipment other
#> 6                                                                                                                                displacement unreliable_technology
#>   edu_learning_conditions_reasons_v1/overcrowding
#> 1                                           FALSE
#> 2                                            TRUE
#> 3                                            TRUE
#> 4                                            TRUE
#> 5                                            TRUE
#> 6                                           FALSE
#>   edu_learning_conditions_reasons_v1/curriculum_not_adapted
#> 1                                                      TRUE
#> 2                                                      TRUE
#> 3                                                     FALSE
#> 4                                                      TRUE
#> 5                                                     FALSE
#> 6                                                     FALSE
#>   edu_learning_conditions_reasons_v1/lack_teachers
#> 1                                            FALSE
#> 2                                             TRUE
#> 3                                            FALSE
#> 4                                            FALSE
#> 5                                            FALSE
#> 6                                            FALSE
#>   edu_learning_conditions_reasons_v1/lack_qualified_staff
#> 1                                                   FALSE
#> 2                                                    TRUE
#> 3                                                   FALSE
#> 4                                                    TRUE
#> 5                                                   FALSE
#> 6                                                   FALSE
#>   edu_learning_conditions_reasons_v1/lack_materials
#> 1                                             FALSE
#> 2                                              TRUE
#> 3                                              TRUE
#> 4                                             FALSE
#> 5                                              TRUE
#> 6                                             FALSE
#>   edu_learning_conditions_reasons_v1/poor_wash
#> 1                                         TRUE
#> 2                                        FALSE
#> 3                                        FALSE
#> 4                                        FALSE
#> 5                                        FALSE
#> 6                                        FALSE
#>   edu_learning_conditions_reasons_v1/discrimination
#> 1                                              TRUE
#> 2                                              TRUE
#> 3                                              TRUE
#> 4                                             FALSE
#> 5                                             FALSE
#> 6                                             FALSE
#>   edu_learning_conditions_reasons_v1/displacement
#> 1                                            TRUE
#> 2                                           FALSE
#> 3                                           FALSE
#> 4                                            TRUE
#> 5                                           FALSE
#> 6                                            TRUE
#>   edu_learning_conditions_reasons_v1/language_barriers
#> 1                                                 TRUE
#> 2                                                FALSE
#> 3                                                FALSE
#> 4                                                 TRUE
#> 5                                                 TRUE
#> 6                                                FALSE
#>   edu_learning_conditions_reasons_v1/curriculum_not_adapted_remote
#> 1                                                             TRUE
#> 2                                                             TRUE
#> 3                                                             TRUE
#> 4                                                            FALSE
#> 5                                                            FALSE
#> 6                                                            FALSE
#>   edu_learning_conditions_reasons_v1/unreliable_technology
#> 1                                                     TRUE
#> 2                                                    FALSE
#> 3                                                    FALSE
#> 4                                                    FALSE
#> 5                                                     TRUE
#> 6                                                     TRUE
#>   edu_learning_conditions_reasons_v1/lack_equipment
#> 1                                              TRUE
#> 2                                              TRUE
#> 3                                              TRUE
#> 4                                              TRUE
#> 5                                              TRUE
#> 6                                             FALSE
#>   edu_learning_conditions_reasons_v1/other
#> 1                                     TRUE
#> 2                                    FALSE
#> 3                                    FALSE
#> 4                                     TRUE
#> 5                                     TRUE
#> 6                                    FALSE
#>   edu_learning_conditions_reasons_v1/dont_know
#> 1                                         TRUE
#> 2                                        FALSE
#> 3                                        FALSE
#> 4                                         TRUE
#> 5                                        FALSE
#> 6                                        FALSE
#>   edu_learning_conditions_reasons_v1/prefer_not_to_answer  weights
#> 1                                                   FALSE 1.157407
#> 2                                                   FALSE 1.190476
#> 3                                                   FALSE 1.190476
#> 4                                                   FALSE 1.190476
#> 5                                                   FALSE 1.190476
#> 6                                                   FALSE 1.190476
```

### How to perform a descriptive analysis (mean, median, proportions)

The *create_analysis* function needs a survey design from *srvyr*.

``` r
example_design <- srvyr::as_survey(weighted_shorter_df, strata = admin1, weights = weights)
```

If only the design is provided, it will perform mean, median and
proportions.

``` r
ex1_results <- create_analysis(.design = example_design, sm_separator = "/")
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
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <dbl>, n_total <dbl>,
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
ex2_results <- create_analysis(.design = srvyr::as_survey(shorter_df), group_var = "admin1", sm_separator = "/")
#> Joining with `by = join_by(type)`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 93% | ETA: 0s
ex2_results[["loa"]]
#>           analysis_type                       analysis_var group_var level
#> 1       prop_select_one                             admin1      <NA>  0.95
#> 2       prop_select_one                             admin2      <NA>  0.95
#> 3                  mean                   expenditure_debt      <NA>  0.95
#> 4                median                   expenditure_debt      <NA>  0.95
#> 5                  mean            income_v1_salaried_work      <NA>  0.95
#> 6                median            income_v1_salaried_work      <NA>  0.95
#> 7       prop_select_one           wash_drinkingwatersource      <NA>  0.95
#> 8  prop_select_multiple edu_learning_conditions_reasons_v1      <NA>  0.95
#> 9       prop_select_one                             admin2    admin1  0.95
#> 10                 mean                   expenditure_debt    admin1  0.95
#> 11               median                   expenditure_debt    admin1  0.95
#> 12                 mean            income_v1_salaried_work    admin1  0.95
#> 13               median            income_v1_salaried_work    admin1  0.95
#> 14      prop_select_one           wash_drinkingwatersource    admin1  0.95
#> 15 prop_select_multiple edu_learning_conditions_reasons_v1    admin1  0.95
```

- two variables separately

``` r
ex3_results <- create_analysis(.design = srvyr::as_survey(shorter_df), group_var = c("admin1", "admin2"), sm_separator = "/")
#> Joining with `by = join_by(type)`
#> ■■■■■■■■■■■■ 36% | ETA: 2s
#> ■■■■■■■■■■■■■ 41% | ETA: 2s
#> ■■■■■■■■■■■■■■■■■■■■ 64% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■ 68% | ETA: 2s
#> ■■■■■■■■■■■■■■■■■■■■■■■ 73% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■■■■ 77% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■ 82% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 91% | ETA: 0s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 95% | ETA: 0s
ex3_results[["loa"]]
#>           analysis_type                       analysis_var group_var level
#> 1       prop_select_one                             admin1      <NA>  0.95
#> 2       prop_select_one                             admin2      <NA>  0.95
#> 3                  mean                   expenditure_debt      <NA>  0.95
#> 4                median                   expenditure_debt      <NA>  0.95
#> 5                  mean            income_v1_salaried_work      <NA>  0.95
#> 6                median            income_v1_salaried_work      <NA>  0.95
#> 7       prop_select_one           wash_drinkingwatersource      <NA>  0.95
#> 8  prop_select_multiple edu_learning_conditions_reasons_v1      <NA>  0.95
#> 9       prop_select_one                             admin2    admin1  0.95
#> 10                 mean                   expenditure_debt    admin1  0.95
#> 11               median                   expenditure_debt    admin1  0.95
#> 12                 mean            income_v1_salaried_work    admin1  0.95
#> 13               median            income_v1_salaried_work    admin1  0.95
#> 14      prop_select_one           wash_drinkingwatersource    admin1  0.95
#> 15 prop_select_multiple edu_learning_conditions_reasons_v1    admin1  0.95
#> 16      prop_select_one                             admin1    admin2  0.95
#> 17                 mean                   expenditure_debt    admin2  0.95
#> 18               median                   expenditure_debt    admin2  0.95
#> 19                 mean            income_v1_salaried_work    admin2  0.95
#> 20               median            income_v1_salaried_work    admin2  0.95
#> 21      prop_select_one           wash_drinkingwatersource    admin2  0.95
#> 22 prop_select_multiple edu_learning_conditions_reasons_v1    admin2  0.95
```

- two variables combined

``` r
ex4_results <- create_analysis(.design = srvyr::as_survey(shorter_df), group_var = "admin1, admin2", sm_separator = "/")
#> Joining with `by = join_by(type)`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 93% | ETA: 0s
ex4_results[["loa"]]
#>           analysis_type                       analysis_var      group_var level
#> 1       prop_select_one                             admin1           <NA>  0.95
#> 2       prop_select_one                             admin2           <NA>  0.95
#> 3                  mean                   expenditure_debt           <NA>  0.95
#> 4                median                   expenditure_debt           <NA>  0.95
#> 5                  mean            income_v1_salaried_work           <NA>  0.95
#> 6                median            income_v1_salaried_work           <NA>  0.95
#> 7       prop_select_one           wash_drinkingwatersource           <NA>  0.95
#> 8  prop_select_multiple edu_learning_conditions_reasons_v1           <NA>  0.95
#> 9                  mean                   expenditure_debt admin1, admin2  0.95
#> 10               median                   expenditure_debt admin1, admin2  0.95
#> 11                 mean            income_v1_salaried_work admin1, admin2  0.95
#> 12               median            income_v1_salaried_work admin1, admin2  0.95
#> 13      prop_select_one           wash_drinkingwatersource admin1, admin2  0.95
#> 14 prop_select_multiple edu_learning_conditions_reasons_v1 admin1, admin2  0.95
```

### How to perform a descriptive analysis with a *list of analysis*

``` r
ex5_results <- create_analysis(.design = srvyr::as_survey(shorter_df), loa = analysistools_MSNA_template_loa, sm_separator = "/")
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 92% | ETA: 0s
ex5_results[["loa"]]
#>           analysis_type                       analysis_var group_var level
#> 1       prop_select_one                             admin1      <NA>  0.95
#> 2                  mean            income_v1_salaried_work      <NA>  0.95
#> 3                median            income_v1_salaried_work      <NA>  0.95
#> 4                  mean                   expenditure_debt      <NA>  0.95
#> 5                median                   expenditure_debt      <NA>  0.95
#> 6       prop_select_one           wash_drinkingwatersource      <NA>  0.95
#> 7  prop_select_multiple edu_learning_conditions_reasons_v1      <NA>  0.95
#> 8                  mean            income_v1_salaried_work    admin1  0.95
#> 9                median            income_v1_salaried_work    admin1  0.95
#> 10                 mean                   expenditure_debt    admin1  0.95
#> 11               median                   expenditure_debt    admin1  0.95
#> 12      prop_select_one           wash_drinkingwatersource    admin1  0.95
#> 13 prop_select_multiple edu_learning_conditions_reasons_v1    admin1  0.95
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
#> 1 prop_select_o… value        a                  <NA>      <NA>             0.52
#> 2 prop_select_o… value        b                  <NA>      <NA>             0.36
#> 3 prop_select_o… value        c                  <NA>      <NA>             0.12
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups),
  group_var = "groups",
  analysis_var = "value",
  level = .95
)
#> # A tibble: 6 × 13
#>   analysis_type  analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>          <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 prop_select_o… value        a                  groups    group_a         0.511
#> 2 prop_select_o… value        b                  groups    group_a         0.356
#> 3 prop_select_o… value        c                  groups    group_a         0.133
#> 4 prop_select_o… value        a                  groups    group_b         0.527
#> 5 prop_select_o… value        b                  groups    group_b         0.364
#> 6 prop_select_o… value        c                  groups    group_b         0.109
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <int>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```

##### Select multiple

``` r
somedata <- data.frame(
  groups = sample(c("group_a", "group_b"), size = 100, replace = T),
  smvar = rep(NA_character_, 100),
  smvar.option1 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.7, .3)),
  smvar.option2 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.6, .4)),
  smvar.option3 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.1, .9)),
  smvar.option4 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.8, .2)),
  uuid = 1:100 %>% as.character()
) %>%
  cleaningtools::recreate_parent_column(uuid = "uuid", sm_separator = ".")
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
  level = 0.95
)
#> # A tibble: 4 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value   stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>            <dbl>
#> 1 prop_select_… smvar        option1            <NA>      <NA>            0.616 
#> 2 prop_select_… smvar        option2            <NA>      <NA>            0.667 
#> 3 prop_select_… smvar        option3            <NA>      <NA>            0.0707
#> 4 prop_select_… smvar        option4            <NA>      <NA>            0.768 
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <dbl>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>

create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
  group_var = "groups",
  analysis_var = "smvar",
  level = 0.95
)
#> # A tibble: 8 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value   stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>            <dbl>
#> 1 prop_select_… smvar        option1            groups    group_a         0.543 
#> 2 prop_select_… smvar        option2            groups    group_a         0.652 
#> 3 prop_select_… smvar        option3            groups    group_a         0.0652
#> 4 prop_select_… smvar        option4            groups    group_a         0.783 
#> 5 prop_select_… smvar        option1            groups    group_b         0.679 
#> 6 prop_select_… smvar        option2            groups    group_b         0.679 
#> 7 prop_select_… smvar        option3            groups    group_b         0.0755
#> 8 prop_select_… smvar        option4            groups    group_b         0.755 
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

## Code of Conduct

Please note that the analysistools project is released with a
[Contributor Code of
Conduct](https://impact-initiatives.github.io/analysistools/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
