# How far a variable is from gaussian, and why

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
(empty when agreement is high).

## Details

The index measures the size of the departure from normal, not its kind.
A variable scoring 0.3 may be skewed, heavy-tailed, lumpy or piled on a
few values; `gauss_note` names which, and a plot of it –
`ilm_plot(data, x)` – shows it.

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
