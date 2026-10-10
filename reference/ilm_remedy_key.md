# Build a remedy key

The one way keys are written, so that the same change always gets the
same key: `<kind>/<target>`, with `:qualifier` only where the qualifier
is part of what the change is (`convert/temp:degF>degC`), never a tuning
number. Several columns are sorted and joined by commas; a model term is
written as its formula label.

## Usage

``` r
ilm_remedy_key(kind, target, qualifier = NULL)
```

## Arguments

- kind:

  The change's verb: `"drop_cols"`, `"set_missing"`, ...

- target:

  Character: the columns, or a term (a formula, or its label).

- qualifier:

  Optional: what distinguishes two changes of the same kind to the same
  target.

## Value

A single string.

## Examples

``` r
ilm_remedy_key("drop_cols", c("b", "a"))
#> [1] "drop_cols/a,b"
ilm_remedy_key("convert", "temp", "degF>degC")
#> [1] "convert/temp:degF>degC"
ilm_remedy_key("drop", ~ (1 | g))
#> [1] "drop/(1 | g)"
```
