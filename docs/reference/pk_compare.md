# Compare runs

`pk_compare()` returns one row per run with OFV, the difference in OFV
(dOFV), the difference in the number of estimated parameters (df), the
p-value of the likelihood-ratio test, AIC, BIC, condition number,
covariance step status, number of flags, and the decision. The reference
is the parent of each run (`ref = "parent"`), the first run in the list
(`ref = "first"`), or a given run id.

## Usage

``` r
pk_compare(..., ref = c("parent", "first"), proj = NULL)

pk_compare_params(..., proj = NULL)
```

## Arguments

- ...:

  Runs (references or ids).

- ref:

  `"parent"`, `"first"`, or a run id.

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A tibble.

## Details

The likelihood-ratio test assumes nested models; the p-value is only
given for a decrease in OFV with a positive increase in the number of
parameters. The OFV is only comparable between runs estimated on the
same data with the same method. Runs with different data hashes or
estimation methods are flagged in the column `comparable`.

`pk_compare_params()` shows the parameter estimates of several runs side
by side, with the relative change against the first run.
