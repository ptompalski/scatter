# Relative Mean Difference (RMD)

Computes the relative mean signed deviation between \`estimate\` and
\`truth\`, calculated as the mean of (\`estimate - truth\`) divided by
the mean of \`truth\`, expressed as a percentage. Positive values
indicate overestimation.

This is a thin wrapper around \`yardstick::msd()\`, with reversed sign
and scaled by the mean of the true values. The default
\`yardstick::msd()\` computes \`truth - estimate\`, while this function
computes \`estimate - truth\`.

## Usage

``` r
rmd(data, ...)

# S3 method for class 'data.frame'
rmd(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

rmd_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
```

## Arguments

- data:

  A data frame containing the \`truth\` and \`estimate\` columns.

- ...:

  Additional arguments passed to lower-level functions.

- truth:

  The column identifier for the true values (bare or quoted).

- estimate:

  The column identifier for the predicted values (bare or quoted).

- na_rm:

  A logical value indicating whether \`NA\` values should be removed
  before computation.

- case_weights:

  An optional column of case weights.

## Details

The relative mean difference is useful when comparing average deviations
across models or datasets, as it expresses the difference relative to
the size of the true value. It supports optional case weights and
handles missing values similarly to other \`yardstick\` metrics.
