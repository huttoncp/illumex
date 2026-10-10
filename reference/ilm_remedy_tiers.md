# The tiers a target's remedies are ranked by

Every remedies table ranks its remedies by an ordered set of tiers,
least to most consequential, and a target – a data frame, or a fitted
model – has one set, so that every package listing remedies for it ranks
them the same way and their tables can be combined with
[`c()`](https://rdrr.io/r/base/c.html). A package building a table asks
this generic for the set rather than assuming one.

## Usage

``` r
ilm_remedy_tiers(object, ...)
```

## Arguments

- object:

  The target: a data frame or a fitted model.

- ...:

  Arguments for methods.

## Value

A list: `tiers`, the ordered tier names, and `note`, the paragraph a
printed table closes with to say what they mean, or `NULL` when the
package printing the table supplies its own.

## Details

For a data frame the tiers are `representation`, `values` and `rows`
(see
[`ilm_remedies()`](https://huttoncp.github.io/illumex/reference/ilm_remedies.md)).
For a fitted model they are illume's `numerical`, `structural` and
`estimand`, unless the model's class gives this generic a method: a
package whose fits admit another kind of change – a revised prior, for a
Bayesian fit – returns its own ordered set, which must keep the model
tiers in their order.

## See also

[`ilm_remedy_assemble()`](https://huttoncp.github.io/illumex/reference/ilm_remedy_assemble.md),
which takes the set, and
[`ilm_remedies()`](https://huttoncp.github.io/illumex/reference/ilm_remedies.md).

## Examples

``` r
ilm_remedy_tiers(data.frame(x = 1))$tiers
#> [1] "representation" "values"         "rows"          
ilm_remedy_tiers(lm(mpg ~ wt, data = mtcars))$tiers
#> [1] "numerical"  "structural" "estimand"  
```
