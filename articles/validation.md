# Validated against FactoMineR and PCAmixdata

[`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md),
and so
[`ilm_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_cluster.md)
and
[`ilm_profile()`](https://huttoncp.github.io/illumex/reference/ilm_profile.md),
compute their dimensions by factor analysis of mixed data (Pagès 2004):
principal components when every column is a number, multiple
correspondence analysis when every column is a category, and the mixed
method in between. illumex computes it itself. It used to call
`PCAmixdata::PCAmix()`, and before relying on its own it was held to
that and to `FactoMineR::FAMD()`, the two established implementations.
The outputs of both are stored with the package’s tests, so every check
of the package repeats the comparison without either being installed.
This page runs it again as the site is built.

## Against PCAmixdata

Four kinds of data from
[`ilm_sim()`](https://huttoncp.github.io/illumex/reference/ilm_sim.md):
mixed (four numbers, two categories), numbers only, categories only, and
mixed with missing values. The largest difference from what `PCAmix()`
gave, on every eigenvalue, every coordinate of every row on every
retained dimension, and every squared loading:

| data             | rows | eigenvalues | coordinates | loadings |
|:-----------------|-----:|------------:|------------:|---------:|
| mixed            |  900 |     3.6e-15 |     4.3e-13 |  1.6e-14 |
| numeric_only     |  900 |     2.0e-15 |     9.1e-15 |  1.2e-15 |
| categorical_only |  900 |     4.4e-15 |     3.8e-12 |  5.3e-14 |
| mixed_with_gaps  |  900 |     2.7e-15 |     2.0e-13 |  7.3e-15 |

A coordinate’s sign is arbitrary in any implementation – the same
dimension read from the other end – so coordinates are compared after
turning each of illumex’s dimensions to face the same way as PCAmix’s.
illumex fixes its own signs by a rule, so the same data always give the
same coordinates: the column that loads most on a dimension loads
positively.

## Against FactoMineR

Five columns of
[`ilm_sim()`](https://huttoncp.github.io/illumex/reference/ilm_sim.md),
through
[`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md)
itself, against what `FactoMineR::FAMD()` gave: its first five
eigenvalues, and every row’s coordinates on the first three dimensions.

| quantity                       | largest_difference |
|:-------------------------------|-------------------:|
| eigenvalues 1 to 5             |            2.1e-15 |
| coordinates, dimensions 1 to 3 |            2.0e-13 |

[`ilm_profile()`](https://huttoncp.github.io/illumex/reference/ilm_profile.md)
describes each cluster by v-tests, as `FactoMineR::catdes()` does: for a
number, how far the cluster’s mean sits from the overall mean in
standard errors; for a category, the hypergeometric probability of the
cluster’s count on the normal scale. Against `catdes()` on a fixed
partition of the same data:

| v_test            | compared | largest_difference |
|:------------------|---------:|-------------------:|
| numeric variables |        9 |            6.6e-15 |
| categories        |       12 |            3.8e-15 |

## Speed

Measured once by `dev/studies/famd_own.R` in the package’s source, on
one core, as the median ratio of illumex’s time to PCAmix’s over
repeated runs (below 1 is faster); FactoMineR’s is beside it:

|    rows | columns | illumex / PCAmix | FactoMineR / PCAmix |
|--------:|--------:|-----------------:|--------------------:|
|     200 |      12 |             1.25 |                1.50 |
|   2,000 |      12 |             0.35 |                0.83 |
|   2,000 |     120 |             0.62 |                0.97 |
|  20,000 |      12 |             0.19 |                0.80 |
|  20,000 |     120 |             0.51 |                0.96 |
| 100,000 |      20 |             0.22 |                0.81 |

## Checking it yourself

The comparisons above run in the package’s tests, `test-famd.R` and
`test-factominer-agreement.R`, on every check. The references were
recorded by `dev/studies/make_pcamix_fixtures.R` (PCAmixdata 3.1) and
`dev/studies/make_factominer_fixtures.R` (FactoMineR 2.11); run them
again if either package changes its method.

## References

Pagès, J. (2004). Analyse factorielle de données mixtes. *Revue de
Statistique Appliquée* 52(4), 93-111.

Chavent, M., Kuentz-Simonet, V., Labenne, A. and Saracco, J. (2014).
Multivariate analysis of mixed data: the PCAmixdata R package.
arXiv:1411.4911.

Lê, S., Josse, J. and Husson, F. (2008). FactoMineR: an R package for
multivariate analysis. *Journal of Statistical Software* 25(1), 1-18.
