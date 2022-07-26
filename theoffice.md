The Office
================
Raya Shelashska

``` r
library(tidyverse)
library(tidymodels)
library(schrute)
library(lubridate)
```

Use `theoffice` data from the
[**schrute**](https://bradlindblad.github.io/schrute/) package to
predict IMDB scores for episodes of The Office.

``` r
glimpse(theoffice)
```

    ## Rows: 55,130
    ## Columns: 12
    ## $ index            <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16…
    ## $ season           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ episode          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ episode_name     <chr> "Pilot", "Pilot", "Pilot", "Pilot", "Pilot", "Pilot",…
    ## $ director         <chr> "Ken Kwapis", "Ken Kwapis", "Ken Kwapis", "Ken Kwapis…
    ## $ writer           <chr> "Ricky Gervais;Stephen Merchant;Greg Daniels", "Ricky…
    ## $ character        <chr> "Michael", "Jim", "Michael", "Jim", "Michael", "Micha…
    ## $ text             <chr> "All right Jim. Your quarterlies look very good. How …
    ## $ text_w_direction <chr> "All right Jim. Your quarterlies look very good. How …
    ## $ imdb_rating      <dbl> 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6…
    ## $ total_votes      <int> 3706, 3706, 3706, 3706, 3706, 3706, 3706, 3706, 3706,…
    ## $ air_date         <fct> 2005-03-24, 2005-03-24, 2005-03-24, 2005-03-24, 2005-…

Fix `air_date` for later use.

``` r
theoffice <- theoffice %>%
  mutate(air_date = ymd(as.character(air_date)))
```

``` r
glimpse(theoffice)
```

    ## Rows: 55,130
    ## Columns: 12
    ## $ index            <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16…
    ## $ season           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ episode          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ episode_name     <chr> "Pilot", "Pilot", "Pilot", "Pilot", "Pilot", "Pilot",…
    ## $ director         <chr> "Ken Kwapis", "Ken Kwapis", "Ken Kwapis", "Ken Kwapis…
    ## $ writer           <chr> "Ricky Gervais;Stephen Merchant;Greg Daniels", "Ricky…
    ## $ character        <chr> "Michael", "Jim", "Michael", "Jim", "Michael", "Micha…
    ## $ text             <chr> "All right Jim. Your quarterlies look very good. How …
    ## $ text_w_direction <chr> "All right Jim. Your quarterlies look very good. How …
    ## $ imdb_rating      <dbl> 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6…
    ## $ total_votes      <int> 3706, 3706, 3706, 3706, 3706, 3706, 3706, 3706, 3706,…
    ## $ air_date         <date> 2005-03-24, 2005-03-24, 2005-03-24, 2005-03-24, 2005…

We will

-   engineer features based on episode scripts
-   train a model
-   perform cross validation
-   make predictions

Note: The episodes listed in `theoffice` don’t match the ones listed in
the data we used in the [cross validation
lesson](https://ids-s1-20.github.io/slides/week-10/w10-d02-cross-validation/w10-d02-cross-validation.html).

``` r
theoffice %>%
  distinct(season, episode)
```

    ## # A tibble: 186 × 2
    ##    season episode
    ##     <int>   <int>
    ##  1      1       1
    ##  2      1       2
    ##  3      1       3
    ##  4      1       4
    ##  5      1       5
    ##  6      1       6
    ##  7      2       1
    ##  8      2       2
    ##  9      2       3
    ## 10      2       4
    ## # … with 176 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

### Exercise 1 - Calculate the percentage of lines spoken by Jim, Pam, Michael, and Dwight for each episode of The Office.

``` r
office_lines <- theoffice %>%
  group_by(season, episode) %>%
  mutate(n_lines = n()) %>%
  relocate(n_lines) %>%
  print()
```

    ## # A tibble: 55,130 × 13
    ## # Groups:   season, episode [186]
    ##    n_lines index season episode episode_n…¹ direc…² writer chara…³ text  text_…⁴
    ##      <int> <int>  <int>   <int> <chr>       <chr>   <chr>  <chr>   <chr> <chr>  
    ##  1     229     1      1       1 Pilot       Ken Kw… Ricky… Michael All … All ri…
    ##  2     229     2      1       1 Pilot       Ken Kw… Ricky… Jim     Oh, … Oh, I …
    ##  3     229     3      1       1 Pilot       Ken Kw… Ricky… Michael So y… So you…
    ##  4     229     4      1       1 Pilot       Ken Kw… Ricky… Jim     Actu… Actual…
    ##  5     229     5      1       1 Pilot       Ken Kw… Ricky… Michael All … All ri…
    ##  6     229     6      1       1 Pilot       Ken Kw… Ricky… Michael Yes,… [on th…
    ##  7     229     7      1       1 Pilot       Ken Kw… Ricky… Michael I've… I've, …
    ##  8     229     8      1       1 Pilot       Ken Kw… Ricky… Pam     Well… Well. …
    ##  9     229     9      1       1 Pilot       Ken Kw… Ricky… Michael If y… If you…
    ## 10     229    10      1       1 Pilot       Ken Kw… Ricky… Pam     What? What?  
    ## # … with 55,120 more rows, 3 more variables: imdb_rating <dbl>,
    ## #   total_votes <int>, air_date <date>, and abbreviated variable names
    ## #   ¹​episode_name, ²​director, ³​character, ⁴​text_w_direction
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

### Exercise 2 - Identify episodes that touch on Halloween, Valentine’s Day, and Christmas.

### Exercise 3 - Put together a modeling dataset that includes features you’ve engineered. Also add an indicator variable called `michael` which takes the value `1` if Michael Scott (Steve Carrell) was there, and `0` if not. Note: Michael Scott (Steve Carrell) left the show at the end of Season 7.

### Exercise 4 - Split the data into training (75%) and testing (25%).

``` r
set.seed(1122)
```

### Exercise 5 - Specify a linear regression model.

### Exercise 6 - Create a recipe that updates the role of `episode_name` to not be a predictor, removes `air_date` as a predictor, uses `season` as a factor, and removes all zero variance predictors.

### Exercise 7 - Build a workflow for fitting the model specified earlier and using the recipe you developed to preprocess the data.

### Exercise 8 - Fit the model to training data and interpret a couple of the slope coefficients.

### Exercise 9 - Perform 5-fold cross validation and view model performance metrics.

``` r
set.seed(345)
folds <- vfold_cv(___, v = ___)
folds

set.seed(456)
office_fit_rs <- ___ %>%
  ___(___)

___(office_fit_rs)
```

    ## Error: <text>:2:20: unexpected input
    ## 1: set.seed(345)
    ## 2: folds <- vfold_cv(__
    ##                       ^

### Exercise 10 - Use your model to make predictions for the testing data and calculate the RMSE. Also use the model developed in the [cross validation lesson](https://ids-s1-20.github.io/slides/week-10/w10-d02-cross-validation/w10-d02-cross-validation.html) to make predictions for the testing data and calculate the RMSE as well. Which model did a better job in predicting IMDB scores for the testing data?

#### New model

#### Old model

``` r
office_mod_old <- linear_reg() %>%
  set_engine("lm")

office_rec_old <- recipe(imdb_rating ~ season + episode + total_votes + air_date, data = office_train) %>%
  # extract month of air_date
  step_date(air_date, features = "month") %>%
  step_rm(air_date) %>%
  # make dummy variables of month 
  step_dummy(contains("month")) %>%
  # remove zero variance predictors
  step_zv(all_predictors())

office_wflow_old <- workflow() %>%
  add_model(office_mod_old) %>%
  add_recipe(office_rec_old)

office_fit_old <- office_wflow_old %>%
  fit(data = office_train)

tidy(office_fit_old)

___
```

    ## Error: <text>:22:2: unexpected input
    ## 21: 
    ## 22: __
    ##      ^
