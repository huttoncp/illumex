# How far a variable is from Gaussian, and why

Reports an agreement index on 0-1 and, when it is low, the reason.

## Usage

``` r
ilm_gauss_check(x, min_n = 20L, cap = 0.06)
```

## Arguments

- x:

  A numeric vector.

- min_n:

  Below this many observations the index is not computed.

- cap:

  The excess distance at which agreement reaches 0. The default 0.06
  puts clear departures at the bottom of the scale: with 500 rows a
  lognormal, a two-humped mixture, a Poisson count and t with 3 degrees
  of freedom all score 0.07 or less, while normal data score 1.

## Value

A list with `gauss` (0-1), `ks_d` (the raw distance) and `gauss_note`
(empty when agreement is high and there is no floor or ceiling).

## Details

The index measures the size of the departure from normal, not its kind.
A variable scoring 0.3 may be skewed, heavy-tailed, lumpy or piled on a
few values; `gauss_note` names which, and a plot of it –
`ilm_plot(data, x)` – shows it.

A pile of values at a bound – a detection limit, a capped scale, a
count's excess zeros – is named first, whatever the index: when the
count at the lowest (or highest) value is at least 5 and 2% of the
values, at least twice the count at the next value, and significantly
above it (a one-sided exact test at 0.001). It is looked for on a
variable with at least 5 distinct values, or 4 on a short scale, so that
a 0-3 item is read; a 3-point scale or a 0/1 variable is not. The advice
follows the variable's kind:

- a continuous variable's floor or ceiling points to
  `illume::ilm_censor()`;

- a count's excess zeros point to a two-part or zero-inflated model;

- on a rating scale – whole numbers from 1 spanning at most 10 points,
  or symmetric about 0 and spanning at most 10 (-3 to 3, say) – a floor
  or ceiling means the scale cannot separate people at that end, and
  points to an ordinal model;

- whole numbers from 0 to at most 10 may be a count or a rating, so the
  note names both readings and both remedies.

An ordered factor is a rating scale:
[`ilm_describe()`](https://huttoncp.github.io/illumex/reference/ilm_describe.md)'s
note for it leads with a floor or ceiling, named by the level's label.
Its bounds are its declared first and last levels; when the first is
unused, a pile at the next is not called the scale's lowest point (the
note counts the unused levels). A numeric variable's bounds are the
values it takes.

An ordinal model uses only the order of a scale's levels, so a scale
centred on 0 needs no shifting to positive values before one is fitted.
Shifting the values changes the dispersion column, not the scale.

This deliberately does not report a normality test p-value. Any test of
exact normality rejects everything once `n` is large, so the p-value
answers a question nobody asked. What matters is how far from normal a
variable is, which is an effect size.

The measure is the Kolmogorov distance between the data and the
best-fitting normal: the largest amount, on the cumulative probability
scale, by which a normal model misstates the data. For normal data,
sampling noise alone produces a distance of about `0.6/sqrt(n)`, and 95%
of the time no more than about `0.9/sqrt(n)` (Lilliefors, 1967, gives
0.886 for large samples). The index subtracts that 95th percentile, so
it charges only the departure beyond what normal data of the same size
would plausibly give. It is an agreement index, not a probability.

## References

Lilliefors, H. W. (1967). On the Kolmogorov-Smirnov test for normality
with mean and variance unknown. Journal of the American Statistical
Association, 62(318), 399-402.

## Examples

``` r
set.seed(1)
ilm_gauss_check(rnorm(500))
#> $gauss
#> [1] 1
#> 
#> $ks_d
#> [1] 0.037
#> 
#> $gauss_note
#> [1] ""
#> 
ilm_gauss_check(rlnorm(500))
#> $gauss
#> [1] 0
#> 
#> $ks_d
#> [1] 0.2189
#> 
#> $gauss_note
#> [1] "bounded at zero; right-skewed"
#> 
ilm_gauss_check(c(rnorm(250), rnorm(250, 5)))
#> $gauss
#> [1] 0
#> 
#> $ks_d
#> [1] 0.1441
#> 
#> $gauss_note
#> [1] "multimodal (check for subgroups); light-tailed / flat"
#> 
```
