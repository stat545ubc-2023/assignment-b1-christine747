Assignmentb1 - STAT545B
================
2023-10-28

Load required packages needed for the assignment.

``` r
library(tidyverse)
library(testthat) 
library(dplyr) 
library(palmerpenguins)
library(roxygen2)
library(rlang)
```

### Exercise 1: Make a function

``` r
sum_table = function (df, group, numerical_var) {
  if(!is.numeric(pull(df, {{numerical_var}}))) {
    stop('Sorry, this function only works for numeric inputs.') # check for numeric inputs only
  } 
  if (!is.factor(pull(df, {{group}}))) { 
    stop('Sorry, group column parameter invalid.') # check for factor inputs only
  }

  else {
   df %>%
     group_by({{group}}) %>%
     summarize(average = mean({{numerical_var}}, na.rm = TRUE), # return average
               std_dev = sd({{numerical_var}}, na.rm = TRUE), # return standard deviation
              count = n()) # return the number of counts/observations
  }     
}
```

### Exercise 2: Document function

The creation of the function is documented using roxygen2 tags.

``` r
#' @title
#' Summarize a numerical variable by group within a data frame
#' 
#' @description
#' This function calculates the average, standard deviation, and number of observations from the selected numerical variable #' within a data frame and is also grouped by a selected categorical variable from the same data frame.
#' 
#' @param df A data frame that contains the categorical variable and numerical variable of interest. It is a common way to 
#' name a data frame because it is the first letter of each word. 
#' 
#' @param group A categorical variable within the data frame. It is named group because we are grouping the numerical
#' variable by this parameter 
#' 
#' @param numerical_var A numeric variable within the data frame. It is named numerical_var for clarity and is simple to
#' understand. 
#' 
#' @else A summary table with statistical values.
#' \itemize{
#'   \item average: The average per group. 
#'   \item std_dev: The standard deviation per group.
#'   \item count: The number of observations per group.
#' } 
```

### Exercise 3: Include examples

##### Example 1:

Using the penguins data frame, calculate the average, standard
deviation, and observation counts for the numerical variable
“bill_depth_mm” grouped by the categorical variable “species”.

``` r
sum_table(penguins, species, bill_depth_mm)
```

    ## # A tibble: 3 × 4
    ##   species   average std_dev count
    ##   <fct>       <dbl>   <dbl> <int>
    ## 1 Adelie       18.3   1.22    152
    ## 2 Chinstrap    18.4   1.14     68
    ## 3 Gentoo       15.0   0.981   124

##### Example 2

Using the penguins data frame, calculate the average, standard
deviation, and observation counts for the numerical variable
“Sepal.Width” grouped by the categorical variable “Species”.

``` r
sum_table(iris, Species, Sepal.Width)
```

    ## # A tibble: 3 × 4
    ##   Species    average std_dev count
    ##   <fct>        <dbl>   <dbl> <int>
    ## 1 setosa        3.43   0.379    50
    ## 2 versicolor    2.77   0.314    50
    ## 3 virginica     2.97   0.322    50

##### Example 3

This example demonstrates an error with the message “Sorry, group column
parameter invalid.” This is because the categorical variable used in the
function for ‘group’ is not a factor. The group variable in this
function must be a factor.

``` r
sum_table(penguins, flipper_length_mm, bill_depth_mm)
```

    ## Error in sum_table(penguins, flipper_length_mm, bill_depth_mm): Sorry, group column parameter invalid.

``` r
class(penguins$flipper_length_mm) # class is integer not factor and the sum_table function only takes factors
```

    ## [1] "integer"

##### Example 4

This example demonstrates an error with the message “Sorry, this
function only works for numeric inputs.” This is because the numerical
variable used in the function for ‘numerical_var’ is not numerical. The
input for numerical_var in this function must be numerical.

``` r
sum_table(penguins, species, island)
```

    ## Error in sum_table(penguins, species, island): Sorry, this function only works for numeric inputs.

### Exercise 4: Test the function

###### Test 1

``` r
# create table to match the output for sum_table(penguins, species, bill_depth_mm)
example_tbl <- penguins %>%
  group_by(species) %>%  
  summarize(average = mean(bill_depth_mm, na.rm = TRUE),
               std_dev = sd(bill_depth_mm, na.rm = TRUE),
              count = n())
  
test_that("Test function for correct output calculations", {
  expect_equal(sum_table(penguins, species, bill_depth_mm), example_tbl) # Create example table to match answer
})
```

    ## Test passed 😸

###### Test 2

``` r
test_that("Test function for correct error message for numerical variable", {
  expect_error(sum_table(penguins, species, island), 'Sorry, this function only works for numeric input.')
})
```

    ## Test passed 😸

###### Test 3

``` r
test_that("Test function for correct error message for categorical variable", {
  expect_error(sum_table(penguins, body_mass_g, bill_depth_mm), 'Sorry, group column parameter invalid.')
})
```

    ## Test passed 🥇

###### Test 4

``` r
test_that("Test function for correct output columns", {
  expect_named(sum_table(penguins, species, body_mass_g), c("species", "average", "std_dev", "count"))
  })
```

    ## Test passed 😀
