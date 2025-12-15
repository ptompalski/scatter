# Mean Difference (Mean Signed Deviation)

Computes the mean signed deviation between \`estimate\` and \`truth\`,
calculated as the mean of (\`estimate - truth\`). This is a wrapper
around \`yardstick::msd()\` that reverses the default sign, so positive
values indicate overestimation.

## Usage

``` r
md(data, ...)

# S3 method for class 'data.frame'
md(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

md_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
```

## Arguments

- data:

  A data frame containing the \`truth\` and \`estimate\` columns.

- ...:

  Additional arguments (currently ignored).

- truth:

  The column identifier for the true (observed) values (bare or quoted).

- estimate:

  The column identifier for the predicted (estimated) values (bare or
  quoted).

- na_rm:

  A logical value indicating whether \`NA\` values should be removed
  before computation.

- case_weights:

  An optional column of case weights.

## Value

A single numeric value representing the mean signed deviation
(\`estimate - truth\`).

## Details

\`yardstick::msd()\` computes the mean signed deviation as \`truth -
estimate\`, which means positive values indicate underestimation. This
metric reverses the sign by returning \`-(truth - estimate)\`,
effectively calculating \`estimate - truth\`.

This metric is useful for assessing average directional bias in
predictions. Like other \`yardstick\` metrics, it supports missing value
handling and case weights.
