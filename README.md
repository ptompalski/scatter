
<!-- README.md is generated from README.Rmd. Please edit that file -->

This package provides functions for quickly calculating measures of
model prediction accuracy and creating enhanced scatterplots. The
scatterplots can include text summarizing the agreement metrics (e.g.,
R², bias, RMSE) between two plotted variables, with support for grouped
data and faceting.

# Installation

``` r
devtools::install_github("ptompalski/scatter")
library(scatter)
```

# Features

- Calculate Model Prediction Accuracy: Use `agreement_metrics()` to
  compute metrics like R2, bias and RMSE.

- Generate Scatterplots: Use `scatter()` to generate scatterplots with
  optional agreement metrics text annotations, faceting for grouped
  data.

# Examples

Simple scatterplot

``` r
library(dplyr)
library(ggplot2)
library(scatter)

# some fake data
df <- 
tibble(
  observed = c(rnorm(150, 10, 2)),
  predicted = observed + rnorm(150,0, 1),
  group = rep(c("A", "B", "C"), each = 50)          
)

scatter(df, observed, predicted)
```

![](man/figures/unnamed-chunk-3-1.png)<!-- -->

Scatterplot with agreement metrics

``` r
scatter(df, observed, predicted, add_metrics = TRUE)
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

Grouped scatterplot with agreement metrics

``` r
df %>%
  group_by(group) %>%
  scatter(observed, predicted, add_metrics = TRUE)
```

![](man/figures/unnamed-chunk-5-1.png)<!-- -->
