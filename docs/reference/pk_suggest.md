# Non-binding suggestions for the next modelling step

Translates the flags of a run into hints, for example "consider removing
IIV on KA" for a high ETA shrinkage. The analyst decides; the hints are
derived purely from rules.

## Usage

``` r
pk_suggest(run, thresholds = list(), proj = NULL)
```

## Arguments

- run:

  A run reference or a run id.

- thresholds:

  Named list overriding the thresholds.

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A character vector of hints (empty if there is nothing to suggest).
