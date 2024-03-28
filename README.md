
<!-- README.md is generated from README.Rmd. Please edit that file -->

# analysistools

<!-- badges: start -->

[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
[![check-standard](https://github.com/impact-initiatives/analysistools/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/impact-initiatives/analysistools/actions/workflows/check-standard.yaml)
[![Codecov test
coverage](https://codecov.io/gh/impact-initiatives/analysistools/branch/main/graph/badge.svg)](https://app.codecov.io/gh/impact-initiatives/analysistools?branch=main)

<!-- badges: end -->

The goal of analysistools is to provide tools to analyse dataset
collected with ODK. The main function is create_analysis.

All create_analysis\_\* fuctions will take a survey design object as
input and will return a long result table with the analysis key.

The analysis key is the unique identifier of the analysis. The format is
the following:

- analysis type @/@ analysis variable %/% analysis variable value @/@
  grouping variable %/% grouping variable value

- analysis type @/@ dependent variable %/% dependent variable value @/@
  independent variable %/% independent variable value

If there are two or more grouping variables it would look like that

- analysis type @/@ analysis variable %/% analysis variable value @/@
  grouping variable 1 %/% grouping variable value 1 -/- grouping
  variable 2 %/% grouping variable value 2

There are 3 types of separators:

- @/@ will separate the top level information: analysis type, the
  analysis (dependent) variable information and the grouping
  (independent) variable

- %/% will separate the analysis and grouping information: it will
  separate the variable name and the variable value

- -/- will separate 2 variables in case there are multiple variable in
  either the analysis or grouping sets.

The current analysis types available are :

- mean
- median
- prop_select_one: proportion for select one
- prop_select_multiple: proportion for select multiple
- ratio

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

weighted_shorter_df[, c("admin1", "weights")] %>% head()
#>    admin1  weights
#> 1 admin1b 1.157407
#> 2 admin1c 1.190476
#> 3 admin1c 1.190476
#> 4 admin1c 1.190476
#> 5 admin1c 1.190476
#> 6 admin1c 1.190476
```

### How to perform a descriptive analysis (mean, median, proportions)

The *create_analysis* function needs a survey design from *srvyr*.

``` r
example_design <- srvyr::as_survey(weighted_shorter_df, strata = admin1, weights = weights)
```

If only the design is provided, it will perform mean, median and
proportions.

``` r
ex1_results <- create_analysis(design = example_design, sm_separator = "/")
#> Joining with `by = join_by(type)`
#> Joining with `by = join_by(admin1)`
#> Joining with `by = join_by(admin2)`
#> Joining with `by = join_by(wash_drinkingwatersource)`
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
ex2_results <- create_analysis(design = srvyr::as_survey(shorter_df), group_var = "admin1", sm_separator = "/")
#> Joining with `by = join_by(type)`
#> Joining with `by = join_by(admin1)`
#> Joining with `by = join_by(admin2)`
#> Joining with `by = join_by(wash_drinkingwatersource)`
#> ■■■■■■■■■■■■■■■ 47% | ETA: 1s
#> Joining with `by = join_by(admin1, admin2)`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■ 80% | ETA: 1s
#> Joining with `by = join_by(admin1, wash_drinkingwatersource)`
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
ex3_results <- create_analysis(design = srvyr::as_survey(shorter_df), group_var = c("admin1", "admin2"), sm_separator = "/")
#> Joining with `by = join_by(type)`
#> Joining with `by = join_by(admin1)`
#> Joining with `by = join_by(admin2)`
#> Joining with `by = join_by(wash_drinkingwatersource)`
#> ■■■■■■■■■■■ 32% | ETA: 2s
#> ■■■■■■■■■■■■ 36% | ETA: 2s
#> Joining with `by = join_by(admin1, admin2)`
#> ■■■■■■■■■■■■■ 41% | ETA: 2s
#> ■■■■■■■■■■■■■■■ 45% | ETA: 2s
#> ■■■■■■■■■■■■■■■■■ 55% | ETA: 2s
#> Joining with `by = join_by(admin1, wash_drinkingwatersource)`
#> ■■■■■■■■■■■■■■■■■■■■ 64% | ETA: 2s
#> ■■■■■■■■■■■■■■■■■■■■■ 68% | ETA: 2s
#> Joining with `by = join_by(admin2, admin1)`
#> ■■■■■■■■■■■■■■■■■■■■■■■ 73% | ETA: 2s
#> ■■■■■■■■■■■■■■■■■■■■■■■■ 77% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■ 86% | ETA: 1s
#> Joining with `by = join_by(admin2, wash_drinkingwatersource)`
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
ex4_results <- create_analysis(design = srvyr::as_survey(shorter_df), group_var = "admin1, admin2", sm_separator = "/")
#> Joining with `by = join_by(type)`
#> Joining with `by = join_by(admin1)`
#> Joining with `by = join_by(admin2)`
#> Joining with `by = join_by(wash_drinkingwatersource)`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■ 86% | ETA: 0s
#> Joining with `by = join_by(admin1, admin2, wash_drinkingwatersource)`
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
ex5_results <- create_analysis(design = srvyr::as_survey(shorter_df), loa = analysistools_MSNA_template_loa, sm_separator = "/")
#> Joining with `by = join_by(admin1)`
#> Joining with `by = join_by(wash_drinkingwatersource)`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■ 85% | ETA: 0s
#> Joining with `by = join_by(admin1, wash_drinkingwatersource)`
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
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_mean(me_design, group_var = "bb", analysis_var = "aa")
#> # A tibble: 2 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 mean          aa           <NA>               bb        a                   5
#> 2 mean          aa           <NA>               bb        b                   6
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
me_design_w <- srvyr::as_survey(somedata, weights = weights)
create_analysis_mean(me_design_w, analysis_var = "aa")
#> # A tibble: 1 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 mean          aa           <NA>               <NA>      <NA>             5.75
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_mean(me_design_w, group_var = "bb", analysis_var = "aa")
#> # A tibble: 2 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 mean          aa           <NA>               bb        a                   5
#> 2 mean          aa           <NA>               bb        b                   6
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
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
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_median(me_design, group_var = "bb", analysis_var = "aa")
#> # A tibble: 2 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 median        aa           <NA>               bb        a                   5
#> 2 median        aa           <NA>               bb        b                   6
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
me_design_w <- srvyr::as_survey(somedata, weights = weights)
create_analysis_median(me_design_w, analysis_var = "aa")
#> # A tibble: 1 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 median        aa           <NA>               <NA>      <NA>                6
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_median(me_design_w, group_var = "bb", analysis_var = "aa")
#> # A tibble: 2 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 median        aa           <NA>               bb        a                   5
#> 2 median        aa           <NA>               bb        b                   6
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
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
#> Joining with `by = join_by(value)`
#> # A tibble: 3 × 13
#>   analysis_type  analysis_var analysis_var_value group_var group_var_value  stat
#>   <chr>          <chr>        <chr>              <chr>     <chr>           <dbl>
#> 1 prop_select_o… value        a                  <NA>      <NA>             0.52
#> 2 prop_select_o… value        b                  <NA>      <NA>             0.38
#> 3 prop_select_o… value        c                  <NA>      <NA>             0.1 
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups),
  group_var = "groups",
  analysis_var = "value",
  level = .95
)
#> Joining with `by = join_by(groups, value)`
#> # A tibble: 6 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value   stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>            <dbl>
#> 1 prop_select_… value        a                  groups    group_a         0.549 
#> 2 prop_select_… value        b                  groups    group_a         0.373 
#> 3 prop_select_… value        c                  groups    group_a         0.0784
#> 4 prop_select_… value        a                  groups    group_b         0.490 
#> 5 prop_select_… value        b                  groups    group_b         0.388 
#> 6 prop_select_… value        c                  groups    group_b         0.122 
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
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
#> # A tibble: 5 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value   stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>            <dbl>
#> 1 prop_select_… smvar        option1            <NA>      <NA>             0.694
#> 2 prop_select_… smvar        option2            <NA>      <NA>             0.622
#> 3 prop_select_… smvar        option3            <NA>      <NA>             0.143
#> 4 prop_select_… smvar        option4            <NA>      <NA>             0.806
#> 5 prop_select_… smvar        NA                 <NA>      <NA>            NA    
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <dbl>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>

create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
  group_var = "groups",
  analysis_var = "smvar",
  level = 0.95
)
#> # A tibble: 9 × 13
#>   analysis_type analysis_var analysis_var_value group_var group_var_value   stat
#>   <chr>         <chr>        <chr>              <chr>     <chr>            <dbl>
#> 1 prop_select_… smvar        option1            groups    group_a          0.76 
#> 2 prop_select_… smvar        option2            groups    group_a          0.58 
#> 3 prop_select_… smvar        option3            groups    group_a          0.04 
#> 4 prop_select_… smvar        option4            groups    group_a          0.74 
#> 5 prop_select_… smvar        option1            groups    group_b          0.625
#> 6 prop_select_… smvar        option2            groups    group_b          0.667
#> 7 prop_select_… smvar        option3            groups    group_b          0.25 
#> 8 prop_select_… smvar        option4            groups    group_b          0.875
#> 9 prop_select_… smvar        NA                 groups    group_b         NA    
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
#> 1 ratio         num_attendin… NA %/% NA          <NA>      <NA>              0.2
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
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
#> 1 ratio         num_attendin… NA %/% NA          <NA>      <NA>            0.333
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
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
#> 1 ratio         num_attendin… NA %/% NA          <NA>      <NA>            0.333
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
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
#> 1 ratio         children_enr… NA %/% NA          <NA>      <NA>            0.639
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
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
#> 1 ratio         children_enr… NA %/% NA          groups    a               0.670
#> 2 ratio         children_enr… NA %/% NA          groups    b               0.578
#> # ℹ 7 more variables: stat_low <dbl>, stat_upp <dbl>, n <int>, n_total <dbl>,
#> #   n_w <dbl>, n_w_total <dbl>, analysis_key <chr>
```

### How to review results

The logic behind reviewing analysis is to compare the results from 2
independent analysis of the same variables using the *review_analysis*.

In this example, the results table to be review and the dataset are
loaded.

``` r
results_to_review <- analysistools::analysistools_MSNA_template_with_ratio_results_table$results_table

dataset_to_analyse <- analysistools::analysistools_MSNA_template_data
```

The list of analysis from the results can be reproduced with
*create_loa_from_results* and the analysis key. This *loa* can be used
to create a new analysis to be compared with.

``` r
me_loa <- create_loa_from_results(results_to_review)

me_analysis <- create_analysis(srvyr::as_survey(dataset_to_analyse),
                               loa = me_loa,
                               sm_separator = "/")
#> Joining with `by = join_by(admin1)`
#> Joining with `by = join_by(wash_drinkingwatersource)`
#> ■■■■■■■■■■■■■■■ 47% | ETA: 2s
#> ■■■■■■■■■■■■■■■■■■■ 60% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■ 67% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■ 80% | ETA: 1s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■ 87% | ETA: 0s
#> Joining with `by = join_by(admin1, wash_drinkingwatersource)`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 93% | ETA: 0s
```

The new results and the results to be reviewed are bound together by the
*analysis_key*.

``` r
binded_results <- results_to_review %>%
  dplyr::full_join(me_analysis$results_table, by ="analysis_key")
```

``` r
review_results <- review_analysis(binded_results, 
                                   stat_columns_to_review = c("stat.x", "stat_low.x", "stat_upp.x"),
                                   stat_columns_to_compare_with = c("stat.y", "stat_low.y", "stat_upp.y"))

review_results$review_table %>%
  dplyr::group_by(stat) %>%
  dplyr::summarise(prop_correct = mean(review_check))
#> # A tibble: 3 × 2
#>   stat       prop_correct
#>   <chr>             <dbl>
#> 1 stat.x                1
#> 2 stat_low.x            1
#> 3 stat_upp.x            1

review_results$review_table %>%
  dplyr::group_by(stat, review_comment) %>%
  dplyr::tally(sort = T)
#> # A tibble: 3 × 3
#> # Groups:   stat [3]
#>   stat       review_comment     n
#>   <chr>      <glue>         <int>
#> 1 stat.x     Same results     147
#> 2 stat_low.x Same results     147
#> 3 stat_upp.x Same results     147

review_results$review_table %>%
  dplyr::filter(!review_check) %>%
  dplyr::select(analysis_type,analysis_var,group_var) %>% 
  dplyr::distinct()
#> [1] analysis_type analysis_var  group_var    
#> <0 rows> (or 0-length row.names)
```

``` r
analysis_key_column <-  c("mean @/@ income %/% NA @/@ NA %/% NA",
                          "prop_select_one @/@ water_source %/% tap_water @/@ district %/% district_a",
                          "prop_select_one @/@ water_source %/% tap_water @/@ district %/% district_a -/- population %/% displaced",
                          "prop_select_multiple @/@ source_information %/% relatives @/@ NA %/% NA")
test_analysis_results <- data.frame(
  test = c(
    "test equality",
    "test difference",
    "test Missing in y",
    "test Missing in x"
  ),
  stat_col.x = c(0, 1, 2, NA),
  stat_col.y = c(0, 2, NA, 3),
  analysis_key = analysis_key_column
)
review_results2 <- review_analysis(test_analysis_results,
                stat_columns_to_review = "stat_col.x",
                stat_columns_to_compare_with = "stat_col.y")
review_results2$review_table %>%
  dplyr::group_by(stat) %>%
  dplyr::summarise(prop_correct = mean(review_check))
#> # A tibble: 1 × 2
#>   stat       prop_correct
#>   <chr>             <dbl>
#> 1 stat_col.x         0.25

review_results2$review_table %>%
  dplyr::group_by(stat, review_comment) %>%
  dplyr::tally(sort = T)
#> # A tibble: 4 × 3
#> # Groups:   stat [1]
#>   stat       review_comment            n
#>   <chr>      <glue>                <int>
#> 1 stat_col.x Different results         1
#> 2 stat_col.x Missing in stat_col.x     1
#> 3 stat_col.x Missing in stat_col.y     1
#> 4 stat_col.x Same results              1
review_results2$review_table %>%
  dplyr::filter(!review_check) %>%
  dplyr::select(review_check, analysis_type,analysis_var,group_var) %>% 
  dplyr::distinct()
#>   review_check        analysis_type       analysis_var               group_var
#> 1        FALSE      prop_select_one       water_source                district
#> 2        FALSE      prop_select_one       water_source district %/% population
#> 3        FALSE prop_select_multiple source_information                      NA
```

### Converting the analysis index into a table

This is is how to turn the analysis index into a table

``` r
resultstable <- data.frame(analysis_index = c(
  "mean @/@ v1 %/% NA @/@ NA %/% NA",
  "mean @/@ v1 %/% NA @/@ gro %/% A",
  "mean @/@ v1 %/% NA @/@ gro %/% B"
))

key_table <- create_analysis_key_table(resultstable, "analysis_index")
key_table
#> # A tibble: 3 × 8
#>   analysis_index   analysis_type analysis_var_1 analysis_var_value_1 group_var_1
#>   <chr>            <chr>         <chr>          <chr>                <chr>      
#> 1 mean @/@ v1 %/%… mean          v1             NA                   NA         
#> 2 mean @/@ v1 %/%… mean          v1             NA                   gro        
#> 3 mean @/@ v1 %/%… mean          v1             NA                   gro        
#> # ℹ 3 more variables: group_var_value_1 <chr>, nb_analysis_var <dbl>,
#> #   nb_group_var <dbl>
```

You can then unite the analysis and grouping variables if needed.

``` r
unite_variables(key_table)
#> # A tibble: 3 × 8
#>   analysis_index         analysis_type analysis_var analysis_var_value group_var
#>   <chr>                  <chr>         <chr>        <chr>              <chr>    
#> 1 mean @/@ v1 %/% NA @/… mean          v1           NA                 NA       
#> 2 mean @/@ v1 %/% NA @/… mean          v1           NA                 gro      
#> 3 mean @/@ v1 %/% NA @/… mean          v1           NA                 gro      
#> # ℹ 3 more variables: group_var_value <chr>, nb_analysis_var <dbl>,
#> #   nb_group_var <dbl>
```

## Code of Conduct

Please note that the analysistools project is released with a
[Contributor Code of
Conduct](https://impact-initiatives.github.io/analysistools/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
