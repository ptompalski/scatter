# Compute Agreement Metrics for Predictions

This function calculates various metrics to assess the agreement between
predicted and true values. It leverages the flexibility of the
\`yardstick\` package to compute a variety of metrics, which can be
specified through the \`metrics\` parameter.

## Usage

``` r
agreement_metrics(
  data,
  truth,
  estimate,
  metrics = list(rsq, md, rmd, rmse, rrmse),
  label = FALSE
)
```

## Arguments

- data:

  A data frame containing the observed (\`truth\`) and predicted
  (\`estimate\`) values.

- truth:

  The column in \`data\` representing the true values. Use tidy
  evaluation (e.g., \`\`).

- estimate:

  The column in \`data\` representing the predicted values. Use tidy
  evaluation (e.g., \`\`).

- metrics:

  A list of metrics to compute. Metrics can be almost any function from
  the \`yardstick\` package (e.g., \`rsq\`, \`rmse\`, \`mape\`,
  \`msd\`), provided they follow the \`yardstick\` format. Defaults to
  \`list(rsq, md, rmd, rmse, rrmse)\`. Can also be a named list of
  metrics to compute (e.g. \`list(bias=md, "bias Names are then used
  instead of the \`yardstick\` metric names.

- label:

  Logical. If \`TRUE\`, the function creates a concatenated string
  summarizing all computed metrics in a new column called \`label\`.
  Defaults to \`FALSE\`.

## Value

A data frame with the computed metrics. If \`label = TRUE\`, the output
includes a \`label\` column summarizing all metrics.

## Details

The function internally uses the \`metric_set\` function from
\`yardstick\` to handle the specified metrics. The results are returned
in a wide format, with each metric as a column. All numeric metrics are
rounded to two decimal places for readability.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(yardstick)
library(tidyr)

# Example dataset
data <- data.frame(
  truth = c(3, 5, 2.5, 7),
  estimate = c(2.8, 5.1, 2.6, 7.2)
)

# Compute default metrics
agreement_metrics(data, truth = truth, estimate = estimate)
#> # A tibble: 1 × 5
#>     rsq    md   rmd  rmse rrmse
#>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     1  0.05  1.14  0.16  3.61

```
