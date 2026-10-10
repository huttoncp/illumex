# Make one remedy

Applies one remedy from an
[`ilm_remedies()`](https://huttoncp.github.io/illumex/reference/ilm_remedies.md)
table to the target the table was listed for. A generic: illumex applies
data remedies (the method for data frames returns the cleaned data with
a cleaning log), and illume refits models.

## Usage

``` r
ilm_apply_remedy(object, remedies, which, ...)

# S3 method for class 'data.frame'
ilm_apply_remedy(object, remedies, which, reason = NULL, recheck = TRUE, ...)
```

## Arguments

- object:

  What the remedy is applied to: the data frame, or the fitted model,
  the table was listed for.

- remedies:

  The whole table from
  [`ilm_remedies()`](https://huttoncp.github.io/illumex/reference/ilm_remedies.md)
  the remedy was chosen from. Required rather than recomputed, so the
  remedy made is always the one that was read.

- which:

  The remedy: its `id`, or its `key`. Spaces in a key are ignored, so
  `"drop/(1|g)"` finds `"drop/(1 | g)"`.

- ...:

  Arguments for methods.

- reason:

  Why the remedy is being made, in a sentence – "the lab reports values
  below 0.5 as '\<0.5'". Kept in the cleaning log and written into the
  cleaning script beside the step.

- recheck:

  Run the check that called for the remedy again on the result, and say
  what it finds now.

## Value

For a data frame, the data with the remedy made, carrying the cleaning
log; see
[`ilm_cleaning_log()`](https://huttoncp.github.io/illumex/reference/ilm_cleaning_log.md).

## See also

[`ilm_remedies()`](https://huttoncp.github.io/illumex/reference/ilm_remedies.md),
[`ilm_cleaning_script()`](https://huttoncp.github.io/illumex/reference/ilm_cleaning_script.md).
