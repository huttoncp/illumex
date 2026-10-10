# Find a remedy in a table by id or key

The lookup every
[`ilm_apply_remedy()`](https://huttoncp.github.io/illumex/reference/ilm_apply_remedy.md)
method uses, so that a remedy is found the same way whatever it is
applied to. Spaces are ignored on both sides of a key.

## Usage

``` r
ilm_remedy_find(remedies, which)
```

## Arguments

- remedies:

  An `"ilm_remedies"` table.

- which:

  An `id` or a `key`.

## Value

The table's row number.
