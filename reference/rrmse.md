# Relative root mean squared error

Calculate the relative root mean squared error. \`rrmse()\` expresses
the RMSE as a proportion of either the \*\*mean\*\* or \*\*range\*\* of
the true values.

## Usage

``` r
rrmse(data, ...)

# S3 method for class 'data.frame'
rrmse(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  normalization = "mean",
  ...
)

rrmse_vec(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  normalization = "mean",
  ...
)
```

## Arguments

- data:

  A \`data.frame\` containing the columns specified by the \`truth\` and
  \`estimate\` arguments.

- ...:

  Additional arguments passed to lower-level functions.

- truth:

  The column identifier for the true results (numeric).

- estimate:

  The column identifier for the predicted results (numeric).

- na_rm:

  A \`logical\` value indicating whether \`NA\` values should be
  removed.

- case_weights:

  Currently not implemented.

- normalization:

  Character string specifying the normalization method: \`"mean"\`
  (default) or \`"range"\`.
