# Exploring data before you model it

## What this package is for

Most exploratory tools answer “what does this data look like”. `illumex`
asks a narrower question: **what about this data will break a model?**

That is a different emphasis, and it shows up everywhere in the output.
A factor summary reports how many levels are too thin to estimate,
because that is the usual reason a model fails to fit. A date summary
reports whether the spacing is regular, because an AR(1) term requires
it. A numeric summary reports how far the variable is from Gaussian *and
why*, because the reason determines what you do next.

The functions share one prefix with `illume`, the modelling package this
one was split from and which attaches it, so exploring and fitting feel
like one workflow rather than two packages bolted together.

``` r

d <- ilm_sim()
dim(d)
#> [1] 900  13
```

[`ilm_sim()`](https://huttoncp.github.io/illumex/reference/ilm_sim.md)
generates a synthetic dataset for examples and tests. Every column is
there because some check should have something to say about it — a
fixture where nothing is wrong tests nothing.

## Describing everything at once

``` r

r <- ilm_describe_all(d)
names(r)
#> [1] "numeric"     "time"        "categorical" "logical"     "constant"
```

One table per class, because columns of different classes do not share a
sensible set of statistics. Constant columns are split out separately: a
variable with one value tells you nothing a statistic can show, and it
will break a model matrix.

``` r

r$numeric
#>    variable obs   n  na         sum         mean           sd           se
#> 1        id 900 900   0    34200.00    38.000000    21.660748   0.72202492
#> 2     score 900 900   0    44709.09    49.676767     5.879836   0.19599453
#> 3    income 900 900   0 33361609.93 37068.455478 22902.676777 763.42255922
#> 4    visits 900 900   0     2779.00     3.087778     1.830427   0.06101423
#> 5    claims 900 900   0     3773.00     4.192222     4.656588   0.15521960
#> 6  downtime 900 900   0     1339.00     1.487778     2.385735   0.07952449
#> 7 lab_value 900 792 108     5840.86     7.374823     1.152760   0.04096156
#>        p0       p50      p100     p_zero dispersion     gauss
#> 1    1.00    38.000     75.00 0.00000000  12.347052 0.4379448
#> 2   29.99    49.835     65.34 0.00000000         NA 1.0000000
#> 3 6356.18 31137.270 182886.22 0.00000000         NA 0.0000000
#> 4    0.00     3.000     10.00 0.05333333   1.085072 0.0000000
#> 5    0.00     3.000     27.00 0.17666667   5.172391 0.0000000
#> 6    0.00     0.000     11.00 0.64000000   3.825659 0.0000000
#> 7    4.04     7.330     11.66 0.00000000         NA 1.0000000
#>                                                                                                                gauss_note
#> 1                                                                                    bounded at zero; light-tailed / flat
#> 2                                                                                                                        
#> 3                                                                                              right-skewed; heavy-tailed
#> 4                                                                          discrete (11 distinct values); bounded at zero
#> 5                                                                                           bounded at zero; right-skewed
#> 6 64% are 0, far above the next value: excess zeros, see a two-part or zero-inflated model; discrete (12 distinct values)
#> 7
```

Reading across: `p_zero` flags zero inflation, `dispersion` is the
variance-to-mean ratio for count-like variables, and `gauss` with
`gauss_note` say how far the variable is from normal and why.

``` r

r$constant
#>   variable       class obs   n na value
#> 1   cohort categorical 900 900  0  2024
```

## The Gaussian index, and why it is not a p-value

`gauss` runs from 0 to 1. A value of 1 means the departure from
normality is no larger than sampling noise produces 95% of the time at
this sample size.

``` r

ilm_gauss_check(rnorm(500))$gauss
#> [1] 1
ilm_gauss_check(rlnorm(500))$gauss
#> [1] 0
```

It deliberately is **not** a normality test p-value. Any test of exact
normality rejects everything once *n* is large, so the p-value ends up
answering a question nobody asked — you already know real data is not
exactly normal. What matters is *how far* from normal it is, which is an
effect size.

``` r

# a normality test would reject this; the index does not
ilm_gauss_check(rnorm(1e5))$gauss
#> [1] 1
```

The measure underneath is the Kolmogorov distance to the best-fitting
normal: the largest amount, on the cumulative probability scale, by
which a normal model misstates the data. Ask for it directly with
`gauss = "ks_d"` if you would rather apply your own threshold.

``` r

ilm_describe(d, "income", gauss = "both")[, c("gauss", "ks_d", "gauss_note")]
#>   gauss   ks_d                 gauss_note
#> 1     0 0.1134 right-skewed; heavy-tailed
```

### The reason matters more than the number

``` r

notes <- function(x) ilm_gauss_check(x)$gauss_note
notes(rlnorm(2000))
#> [1] "bounded at zero; right-skewed"
notes(rt(2000, 3))
#> [1] "heavy-tailed"
notes(rpois(2000, 3))
#> [1] "discrete (11 distinct values); bounded at zero"
notes(c(rnorm(1000), rnorm(1000, 5)))
#> [1] "multimodal (check for subgroups); light-tailed / flat"
```

That last one is the case worth dwelling on. A mixture of two normals is
**multimodal**, and the advice — check for subgroups — is what you
actually need. An earlier version of this feature tried to name the
single best-fitting distribution family instead, and on that data it
confidently answered “uniform”. It was not close to right, and it
offered nothing to act on.

## What the summaries are looking for

``` r

r$categorical[, c("variable", "n_empty", "n_unique", "n_rare", "n_unused",
                  "case_variants", "note")]
#>   variable n_empty n_unique n_rare n_unused case_variants
#> 1      grp       0        4      1        1             0
#> 2     site      46        6      0        0             1
#>                                               note
#> 1 1 unused levels; 1 rare levels (separation risk)
#> 2 46 empty strings (not NA); 1 case/space variants
```

Each of those columns exists because of a specific failure:

- `n_empty` — `""` is not `NA` to R, but it is almost always missing to
  you, and it silently becomes a factor level.
- `n_rare` — levels with only a handful of observations are the usual
  reason a factor model will not fit, and in a logistic model a direct
  route to separation.
- `n_unused` — a declared but unobserved level produces an all-zero
  column in the model matrix and then breaks
  [`predict()`](https://rdrr.io/r/stats/predict.html) on new data.
- `case_variants` — `"North"`, `"north"` and `"North "` counted as three
  things.

``` r

r$logical[, c("variable", "p_TRUE", "note")]
#>    variable    p_TRUE                            note
#> 1      flag 0.5100000                                
#> 2 consented 0.9977778 near-constant (separation risk)
```

An indicator that is almost always `TRUE` is a separation risk in any
binary model.

### Dates carry the AR(1) precondition

``` r

r$time[, c("variable", "n_unique", "spacing", "regular", "n_gaps", "note")]
#>   variable n_unique spacing regular n_gaps
#> 1     date       12 monthly    TRUE      0
#>                                                       note
#> 1 repeated dates: 12 unique across 900 rows (long format?)
```

`regular` and `n_gaps` matter because `illume::ilm_model()`’s AR(1) term
needs regularly spaced observations within group. Seeing “irregular, 3
gaps” here is much better than discovering it when the model fails.

Repeated dates are *described* rather than warned about: many units
sharing a date is simply what long-format panel data looks like.

## Problems that belong to the whole data frame

Some problems are not properties of any single variable.

``` r

dd <- d
dd$score_copy <- dd$score
ilm_frame_issues(dd)
#>               issue            columns                                 detail
#> 1          constant             cohort zero variance; breaks the model matrix
#> 2 duplicate_columns score = score_copy                       identical values
#>                                                           remedy
#> 1 Leave it out: a column with one value cannot explain anything.
#> 2                                           Keep one of the two.
```

Note that `score` is not reported as identifier-like even though every
value is distinct — a continuous variable is unique per row by
construction. Only discrete-valued columns earn that flag.

A model matrix also breaks on structure no pair of numbers shows: a
column that is the sum of others, a factor that relabels another, a code
fixed by a factor’s levels. Groupings inside groupings are reported too,
since they are expected for random effects and a trap for fixed ones.
Each row says what to do.

``` r

set.seed(1)
dd$units <- sample(c("north", "south"), nrow(dd), TRUE)
dd$branch <- paste0(dd$units, "-", sample(1:3, nrow(dd), TRUE))
dd$score_total <- dd$score + dd$income / 1000
ilm_frame_issues(dd)[, c("issue", "columns", "remedy")]
#>               issue                      columns
#> 1          constant                       cohort
#> 2 duplicate_columns           score = score_copy
#> 3            nested              branch in units
#> 4    rank_deficient score_total ~ score + income
#>                                                                                                                                                                          remedy
#> 1                                                                                                                Leave it out: a column with one value cannot explain anything.
#> 2                                                                                                                                                          Keep one of the two.
#> 3 Expected for grouping factors: as random effects, write (1 | units/branch). As fixed effects keep one of them, since branch absorbs every difference between levels of units.
#> 4                                                                Leave out score_total or one of the columns it is built from; with all of them a model has no unique solution.
```

## Missingness

``` r

head(ilm_describe_na_all(d), 4)
#>    variable obs   n  na p_na
#> 1 lab_value 900 792 108 0.12
#> 2    claims 900 900   0 0.00
#> 3    cohort 900 900   0 0.00
#> 4 consented 900 900   0 0.00
```

Sorted with the most missing first, since those are the variables worth
looking at.

## Plots that carry the verdict

[`ilm_plot()`](https://huttoncp.github.io/illumex/reference/ilm_plot.md)
chooses a plot from the classes of what you give it, and annotates it
with the same verdict
[`ilm_describe()`](https://huttoncp.github.io/illumex/reference/ilm_describe.md)
reports.

``` r

ilm_plot(d, "income")
#> ilm_plot: gauss 0.00 — right-skewed; heavy-tailed
```

![](exploring-data_files/figure-html/unnamed-chunk-15-1.png)

The subtitle is not decoration. A histogram of a skewed variable looks
like a histogram; the annotation says how far from Gaussian it is and in
which direction.

You can ask what would be drawn without drawing it:

``` r

ilm_pick_geom(d$score)$geom
#> [1] "histogram"
ilm_pick_geom(d$grp, d$score)$geom
#> [1] "box"
```

### Rendering is aware of how much data there is

``` r

big <- data.frame(a = rnorm(50000), b = rnorm(50000))
ilm_pick_geom(big$a, big$b)
#> $geom
#> [1] "bin2d"
#> 
#> $reason
#> [1] "50000 points exceeds n_max = 5000, so density is drawn instead of points"
#> 
#> $n
#> [1] 50000
```

A scatter of fifty thousand points is a black rectangle. Above `n_max`
the plot becomes a binned density, and says so rather than misleading
you quietly.

``` r

ilm_plot(big, "a", "b")
#> ilm_plot: gauss 1.00  |  50000 points exceeds n_max = 5000, so density is drawn instead of points
```

![](exploring-data_files/figure-html/unnamed-chunk-18-1.png)

### Choosing the geom yourself

``` r

ilm_geom_spec()
#>        geom needs_y                         x_class
#> 1 histogram   FALSE                         numeric
#> 2   density   FALSE                         numeric
#> 3       bar   FALSE           categorical / logical
#> 4       box    TRUE categorical / logical / numeric
#> 5    violin    TRUE categorical / logical / numeric
#> 6     point    TRUE                         numeric
#> 7     bin2d    TRUE                         numeric
#> 8      line    TRUE                  time / numeric
#> 9     spine    TRUE                     categorical
#>                           y_class     size_controls
#> 1                        not used line/border width
#> 2                        not used line/border width
#> 3                        not used line/border width
#> 4 numeric / categorical / logical line/border width
#> 5 numeric / categorical / logical line/border width
#> 6                         numeric        point size
#> 7                         numeric                 -
#> 8           numeric / categorical line/border width
#> 9                     categorical line/border width
```

That table is not just documentation — it is what the argument checks
read, so the documented requirements and the enforced ones cannot drift
apart. Ask for something impossible and the error says what would work:

``` r

ilm_plot(d, "grp", geom = "histogram")
#> Error:
#> ! geom 'histogram' needs `x` to be numeric, but 'grp' is categorical. Geoms that accept a categorical `x`: 'bar', 'box', 'violin', 'spine'.
```

### Appearance

Colours, fills, transparency, point size, palettes and themes are all
available, with ggplot2-style names:

``` r

ilm_plot(d, "grp", "score", geom = "violin", fill = "lightblue",
         alpha = 0.7, theme = "clean")
#> ilm_plot: gauss 1.00
```

![](exploring-data_files/figure-html/unnamed-chunk-21-1.png)

``` r

ilm_plot(d, "score", "income", by = "grp", palette = "Dark 2", alpha = 0.6)
#> ilm_plot: gauss 0.00 — right-skewed; heavy-tailed
```

![](exploring-data_files/figure-html/unnamed-chunk-22-1.png)

## Cleaning

``` r

messy <- data.frame(
  "Study ID"  = c("1", "2", "3", ""),
  someFlag    = c("TRUE", "FALSE", "TRUE", ""),
  measurement = c("1.5", "2.5", "n/a", ""),
  allEmpty    = c("", "", "", ""),
  check.names = FALSE, stringsAsFactors = FALSE)

ilm_wash_df(messy)
#>   study_id some_flag measurement
#> 1        1      TRUE         1.5
#> 2        2     FALSE         2.5
#> 3        3      TRUE         n/a
```

Names become snake_case, the empty row and column go, and text columns
that are really numeric or logical are converted. `measurement` stays
character on purpose: one value does not convert, and silently turning a
column into `NA`s is worse than leaving it alone.

Known-bad codes are handled separately, because a sentinel that means
missing in one column can be a real measurement in another:

``` r

ilm_recode_errors(data.frame(x = c(1, 999, 3), y = c(999, 2, 3)),
                  errors = 999, cols = "x")
#>    x   y
#> 1  1 999
#> 2 NA   2
#> 3  3   3
```

And recoding against a lookup table:

``` r

ilm_translate(c(1, 2, 3, 99), old = 1:3, new = c("low", "mid", "high"))
#> [1] "low"  "mid"  "high" NA
```

Unmatched values become `NA` rather than passing through, so codes your
dictionary does not cover cannot hide.

## Duplicates and counts

``` r

ilm_counts(d$grp)
#>   value   n
#> 1 alpha 404
#> 2  beta 329
#> 3 gamma 164
#> 4 delta   3
ilm_counts_tb(d$site, n = 2)
#>   top_value top_n bot_value bot_n
#> 1     North   277    North     46
#> 2     South   265              46
```

[`ilm_counts_tb()`](https://huttoncp.github.io/illumex/reference/ilm_counts_tb.md)
shows both ends at once because that is where the problems are: a level
that dominates, and levels too thin to model.

``` r

dup <- data.frame(a = c(1, 1, 2, 2, 2, 3), b = c("x", "x", "y", "y", "z", "w"))
ilm_copies(dup)
#> 4 of 6 rows share their values with another row; ilm_copies(dup, filter = "first") keeps one of each, leaving 4.
#>   a b copy_number n_copies
#> 1 1 x           1        2
#> 2 1 x           2        2
#> 3 2 y           1        2
#> 4 2 y           2        2
#> 5 2 z           1        1
#> 6 3 w           1        1
```

## A worked cleaning: a messy copy of mtcars

A messy table, made from `mtcars` so that the right answer is known:
numbers typed as words, three codes for a missing value, names that need
cleaning, an empty column, and twelve copied rows.

``` r

messy_cars <- mtcars[c("mpg", "cyl", "disp", "hp", "wt", "gear", "am")]
messy_cars$cyl <- ifelse(messy_cars$cyl == 6, "six", as.character(messy_cars$cyl))
messy_cars$gear <- ifelse(messy_cars$gear == 4 & messy_cars$mpg > 20, "four",
                          as.character(messy_cars$gear))
names(messy_cars)[1:3] <- c("Miles per gallon", "# of cylinders", "DISP")
messy_cars$notes <- NA
messy_cars$hp <- ifelse(messy_cars$hp == max(messy_cars$hp), 999, messy_cars$hp)
messy_cars$wt <- ifelse(messy_cars$wt < quantile(messy_cars$wt, 0.1), -1, messy_cars$wt)
messy_cars$DISP <- ifelse(messy_cars$DISP > 300, "N/A", as.character(messy_cars$DISP))
set.seed(1234)
messy_cars <- rbind(messy_cars, messy_cars[sample(nrow(messy_cars), 12, replace = TRUE), ])
rownames(messy_cars) <- NULL
dim(messy_cars)
#> [1] 44  8
```

Copies first, since every count after this depends on them:

``` r

head(ilm_dupes(messy_cars), 4)
#> 22 of 44 rows share their values with another row; ilm_copies(messy_cars, filter = "first") keeps one of each, leaving 32.
#>    Miles per gallon # of cylinders DISP  hp    wt gear am notes n_copies
#> 15             10.4              8  N/A 205 5.250    3  0    NA        2
#> 39             10.4              8  N/A 205 5.250    3  0    NA        2
#> 16             10.4              8  N/A 215 5.424    3  0    NA        3
#> 34             10.4              8  N/A 215 5.424    3  0    NA        3
```

The line above the table counts the rows involved, each copy and the row
it copies, and gives the call that keeps one of each.

Errors tend to sit at the two ends of a count, a typo among the rarest
values and a missing-value code among the most common:

``` r

ilm_counts_tb_all(messy_cars[c("# of cylinders", "gear", "DISP")], n = 2)
#>         variable top_value top_n bot_value bot_n
#> 1 # of cylinders         8    21       six     9
#> 2 # of cylinders         4    14         4    14
#> 3           gear         3    24         4     2
#> 4           gear      four    12         5     6
#> 5           DISP       N/A    17      78.7     1
#> 6           DISP     275.8     4      75.7     1
```

`"six"` sits at the rare end of the cylinder counts, `"four"` is among
the most common gears, and the most common `DISP` is `"N/A"`. The
numbers show the other kind of code:

``` r

ilm_describe_all(messy_cars)$numeric[c("variable", "p0", "p100", "gauss_note")]
#>           variable   p0    p100                                    gauss_note
#> 1 Miles per gallon 10.4  33.900                                              
#> 2               hp 52.0 999.000                    right-skewed; heavy-tailed
#> 3               wt -1.0   5.424 multimodal (check for subgroups); left-skewed
#> 4               am  0.0   1.000 discrete (2 distinct values); bounded at zero
```

A horsepower of 999 and a weight of -1 are outside what either column
can hold. `gauss_note` sees the pile of -1s as a floor and suggests
`ilm_censor()`; knowing that a weight cannot be negative says it is a
code for missing instead, which no statistic can tell you.

The cleaning, one step for each finding:

``` r

cars <- ilm_copies(messy_cars, filter = "first")
cars <- ilm_recode_errors(cars, "six", "6")
cars <- ilm_recode_errors(cars, "four", "4")
cars <- ilm_recode_errors(cars, c(-1, 999), cols = c("hp", "wt"))
cars <- ilm_recode_errors(cars, "N/A")
cars <- ilm_wash_df(cars)
str(cars)
#> 'data.frame':    32 obs. of  7 variables:
#>  $ miles_per_gallon   : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>  $ number_of_cylinders: int  6 6 4 6 8 6 8 4 4 6 ...
#>  $ disp               : num  160 160 108 258 NA ...
#>  $ hp                 : num  110 110 93 110 175 105 245 62 95 123 ...
#>  $ wt                 : num  2.62 2.88 2.32 3.21 3.44 ...
#>  $ gear               : int  4 4 4 3 3 3 3 4 4 4 ...
#>  $ am                 : num  1 1 1 0 0 0 0 0 0 0 ...
```

The names are clean (`"# of cylinders"` is `number_of_cylinders`), the
empty column is gone, and the text columns that now hold only numbers
are numbers, integer where every value is whole. Against the original:

``` r

all.equal(cars$number_of_cylinders, as.integer(mtcars$cyl))
#> [1] TRUE
all.equal(cars$gear, as.integer(mtcars$gear))
#> [1] TRUE
colSums(is.na(cars))
#>    miles_per_gallon number_of_cylinders                disp                  hp 
#>                   0                   0                  11                   1 
#>                  wt                gear                  am 
#>                   4                   0                   0
```

The `NA`s are where the codes were, and nowhere else.

## Uncertainty without a model

``` r

ilm_boot_ci(d, "score", R = 500, seed = 1)
#>   stat observed    lower    upper conf   R    ci_type   n
#> 1 mean 49.67677 49.28963 50.03244 0.95 500 percentile 900
```

### A median for each group

`income` is right-skewed in every group large enough to judge, so the
median describes it better than the mean:

``` r

ilm_describe(d, "income", by = "grp")[c("grp", "n", "mean", "p50", "gauss_note")]
#>         grp   n     mean      p50                 gauss_note
#> alpha alpha 404 37375.61 31161.54 right-skewed; heavy-tailed
#> beta   beta 329 36370.77 31056.61 right-skewed; heavy-tailed
#> gamma gamma 164 37007.52 30989.96               right-skewed
#> delta delta   3 75549.05 92636.93      n too small to assess
#>                            gauss
#> alpha right-skewed; heavy-tailed
#> beta  right-skewed; heavy-tailed
#> gamma               right-skewed
#> delta      n too small to assess
```

`delta` has three rows, too few to resample: a bootstrap of three values
can only reshuffle them. The other groups, with a BCa interval, which
corrects the percentile interval for bias and skew in the resampled
medians:

``` r

d4 <- droplevels(d[d$grp != "delta", ])
ilm_boot_ci(d4, "income", by = "grp", stat = "median", ci_type = "bca",
            R = 2000, seed = 1)
#>     grp   stat observed    lower    upper conf    R ci_type   n
#> 1 alpha median 31161.54 29091.08 33302.70 0.95 2000     bca 404
#> 2  beta median 31056.61 28145.81 33794.49 0.95 2000     bca 329
#> 3 gamma median 30989.96 26299.36 37287.25 0.95 2000     bca 164
```

The 95% describes the method, not one interval: in repeated samples,
about 95% of intervals built this way contain the true median.

For a difference between two groups, ask for the difference directly:

``` r

d2 <- d[d$grp %in% c("alpha", "beta"), ]
d2$grp <- factor(d2$grp)
ilm_boot_diff(d2, "score", "grp", R = 500, seed = 1)
#>   stat group  from   to   observed     lower      upper conf   R    ci_type
#> 1 mean   grp alpha beta -0.7764371 -1.609053 0.03153625 0.95 500 percentile
#>   adjust n_comparisons n_from n_to p_value p_adj p_superiority excludes_zero
#> 1   none             1    404  329   0.052 0.052         0.026         FALSE
```

`excludes_zero` answers the question people actually ask. Comparing two
separate intervals for overlap is a conservative and lossy substitute:
two intervals can overlap while the difference is clearly non-zero.

Past two groups every pair is compared, and a formula works in place of
the two column names:

``` r

ilm_boot_diff(score ~ grp, data = d, R = 500, seed = 1)
#>   stat group  from    to   observed      lower     upper conf   R ci_type
#> 1 mean   grp alpha  beta -0.7764371 -1.8089638 0.2560897 0.95 500   max_t
#> 2 mean   grp alpha gamma -0.3748473 -1.6862971 0.9366025 0.95 500   max_t
#> 3 mean   grp alpha delta  3.3021040  0.3007192 6.3034887 0.95 500   max_t
#> 4 mean   grp  beta gamma  0.4015898 -0.9985315 1.8017111 0.95 500   max_t
#> 5 mean   grp  beta delta  4.0785410  1.0440608 7.1130212 0.95 500   max_t
#> 6 mean   grp gamma delta  3.6769512  0.5549078 6.7989947 0.95 500   max_t
#>   adjust n_comparisons n_from n_to p_value p_adj p_superiority excludes_zero
#> 1  max_t             6    404  329   0.054 0.236         0.026         FALSE
#> 2  max_t             6    404  164   0.500 0.920         0.232         FALSE
#> 3  max_t             6    404    3   0.000 0.022         1.000          TRUE
#> 4  max_t             6    329  164   0.508 0.918         0.746         FALSE
#> 5  max_t             6    329    3   0.000 0.004         1.000          TRUE
#> 6  max_t             6    164    3   0.000 0.012         1.000          TRUE
```

Six comparisons at a nominal 95% each do not jointly cover at 95%, so
the intervals are **simultaneous** by default: the groups are resampled
together, each comparison is standardised by its own bootstrap standard
error, and the largest standardised value in each replicate supplies one
critical value for all of them. On normal, equal-variance data this
reproduces [`TukeyHSD()`](https://rdrr.io/r/stats/TukeyHSD.html) to
about 0.006, without assuming either normality or a common variance. Set
`adjust = "none"` for comparisons chosen in advance, or
`adjust = "bonferroni"` to keep the shape of a percentile interval.

`ref` compares every level against one:

``` r

ilm_boot_diff(score ~ grp, data = d, ref = "alpha", R = 500, seed = 1)
#>   stat group  from    to   observed      lower     upper conf   R ci_type
#> 1 mean   grp alpha  beta -0.7764371 -1.7560509 0.2031767 0.95 500   max_t
#> 2 mean   grp alpha gamma -0.3748473 -1.6190904 0.8693959 0.95 500   max_t
#> 3 mean   grp alpha delta  3.3021040  0.4545284 6.1496795 0.95 500   max_t
#>   adjust n_comparisons n_from n_to p_value p_adj p_superiority excludes_zero
#> 1  max_t             3    404  329   0.054 0.186         0.026         FALSE
#> 2  max_t             3    404  164   0.500 0.900         0.232         FALSE
#> 3  max_t             3    404    3   0.000 0.014         1.000          TRUE
```

On skewed data, `"percentile"` and `"bca"` hold their nominal coverage
better than `"normal"` and `"basic"`. When the skew is severe and the
groups are small, no interval shape rescues a bootstrapped *mean* –
reach for `stat = "median"` instead, which stays nominal where the mean
does not.

## Unusual values

``` r

head(ilm_outliers_all(mtcars), 5)
#>   row_id variable   value score is_outlier
#> 1     31       hp 335.000 1.845       TRUE
#> 2     16       wt   5.424 1.602       TRUE
#> 3     17       wt   5.345 1.530       TRUE
#> 4      9     qsec  22.900 1.985       TRUE
#> 5     31     carb   8.000 2.000       TRUE
```

Three rules, in decreasing order of robustness. `"iqr"` is the Tukey
fence and reproduces exactly what a boxplot’s whiskers draw — it uses
Tukey’s hinges, not
[`quantile()`](https://rdrr.io/r/stats/quantile.html)’s default, which
matters on small samples and is pinned to
[`grDevices::boxplot.stats()`](https://rdrr.io/r/grDevices/boxplot.stats.html)
by a test. `"mad"` measures from the median in median absolute
deviations. `"zscore"` measures from the mean in standard deviations,
and is the weakest of the three for a reason worth seeing:

``` r

z <- c(rnorm(30), 100)
c(mad = max(ilm_outliers(z, "mad")$score),
  zscore = max(ilm_outliers(z, "zscore")$score))
#>        mad     zscore 
#> 169.178151   5.382697
```

One large value inflates the standard deviation enough to hide itself.
The median and the MAD are not dragged around by the values being
detected.

Grouping changes the answer, and usually should:

``` r

nrow(ilm_outliers_all(mtcars))
#> [1] 5
nrow(ilm_outliers_all(mtcars, by = "cyl"))
#> [1] 15
```

A value can be perfectly ordinary for its own group and extreme against
the pooled distribution, so flagging without `by` on grouped data partly
just rediscovers the groups.

**A flag is not a verdict.** It says a value is far from the others by
some rule — which may mean a typo, a different population, or an
ordinary draw from a heavy tail, and the third is common. Dropping
flagged rows because they are flagged changes what you are estimating.
If the tail is real, the fix is a model that expects it: a different
family, or `illume::ilm_model(dispformula = )`.

## Naming the plot you want

[`ilm_plot()`](https://huttoncp.github.io/illumex/reference/ilm_plot.md)
chooses a geometry from the data. When you would rather say which one,
each has its own function —
[`ilm_plot_histogram()`](https://huttoncp.github.io/illumex/reference/ilm_plot_histogram.md),
[`ilm_plot_density()`](https://huttoncp.github.io/illumex/reference/ilm_plot_density.md),
[`ilm_plot_box()`](https://huttoncp.github.io/illumex/reference/ilm_plot_box.md),
[`ilm_plot_violin()`](https://huttoncp.github.io/illumex/reference/ilm_plot_violin.md),
[`ilm_plot_scatter()`](https://huttoncp.github.io/illumex/reference/ilm_plot_scatter.md),
[`ilm_plot_bar()`](https://huttoncp.github.io/illumex/reference/ilm_plot_bar.md),
[`ilm_plot_line()`](https://huttoncp.github.io/illumex/reference/ilm_plot_line.md),
[`ilm_plot_stat_error()`](https://huttoncp.github.io/illumex/reference/ilm_plot_stat_error.md)
— and they all take column names as strings, like everything else here.

``` r

ilm_plot_c(
  ilm_plot_histogram(mtcars, "mpg"),
  ilm_plot_box(mtcars, "mpg", x = "cyl"),
  nrow = 1
)
```

![](exploring-data_files/figure-html/namedplots-1.png)

[`ilm_plot_var_pairs()`](https://huttoncp.github.io/illumex/reference/ilm_plot_var_pairs.md)
handles every pair at once, mixed column types included, and
[`ilm_plot_var_all()`](https://huttoncp.github.io/illumex/reference/ilm_plot_var_all.md)
draws one panel per column.

The bootstrap difference from earlier can be seen rather than only
summarised:

``` r

b <- ilm_boot_diff(d, "score", "grp", R = 500, seed = 1)
ilm_plot_boot_diff(b, row = 1)
```

![](exploring-data_files/figure-html/bootplot-1.png)

The interval says where the difference is. This says what the resampling
produced — whether it is symmetric, skewed, or piled against a boundary,
which the two endpoints cannot show.

## Same numbers, different data

Base R’s `anscombe` holds four small datasets, stacked here into one:

``` r

a <- data.frame(set = rep(c("I", "II", "III", "IV"), each = 11),
                x = unlist(anscombe[1:4], use.names = FALSE),
                y = unlist(anscombe[5:8], use.names = FALSE))
ilm_describe(a, c("x", "y"), by = "set")[c("variable", "set", "mean", "sd",
                                           "p0", "p50", "p100")]
#>      variable set     mean       sd   p0  p50  p100
#> I           x   I 9.000000 3.316625 4.00 9.00 14.00
#> II          x  II 9.000000 3.316625 4.00 9.00 14.00
#> III         x III 9.000000 3.316625 4.00 9.00 14.00
#> IV          x  IV 9.000000 3.316625 8.00 8.00 19.00
#> I1          y   I 7.500909 2.031568 4.26 7.58 10.84
#> II1         y  II 7.500909 2.031657 3.10 8.14  9.26
#> III1        y III 7.500000 2.030424 5.39 7.11 12.74
#> IV1         y  IV 7.500909 2.030579 5.25 7.04 12.50
```

The four share a mean and SD of `x` (9 and 3.317) and of `y` (7.50 and
2.03), and a least-squares line, y = 3 + 0.5x. The quartiles already
disagree: in set IV, `x` is 8 in ten rows of eleven. With eleven rows
`gauss` declines to judge. Draw them:

``` r

ilm_plot_scatter(a, "y", "x", facet = "set", trend = "lm")
```

![](exploring-data_files/figure-html/anscombeplot-1.png)

Only set I looks like its summary. Set II is a curve, set III is a line
with one point off it, and in set IV a single point decides the slope.

## Change over time

Monthly means of `score` in each group:

``` r

m <- droplevels(aggregate(score ~ date + grp, data = d, FUN = mean))
table(m$grp)
#> 
#> alpha  beta gamma delta 
#>    12    12    12     3
ilm_plot_line(m, "score", "date", by = "grp")
```

![](exploring-data_files/figure-html/overtime-1.png)

A line joins the months that have data, so `delta`, present in three
months, is drawn as three points joined by straight segments that look
like a trend. Before reading a line, check how many rows lie behind each
point: `ilm_counts(d$grp)` gives `delta` three rows in all.

## Profiling: what shape is this data set?

[`ilm_describe_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_all.md)
tells you about columns one at a time.
[`ilm_profile()`](https://huttoncp.github.io/illumex/reference/ilm_profile.md)
asks a different question – are there *kinds* of row here? – by reducing
the columns to a few dimensions, clustering on those, and then saying
what each cluster is.

``` r

cars <- mtcars
cars$cyl <- factor(cars$cyl)
cars$am  <- factor(cars$am)

p <- ilm_profile(cars, k_max = 5, B = 25, seed = 1)
cat(p$summary, sep = "\n\n")
#> Cluster 1 holds 12 rows, 37.5% of the data (stable). What sets it apart: cyl is '8' for 100% of them, against 44% across all rows; disp is higher: the middle 50% of its values lie between 276 and 400, against 120 to 318 across all rows; gear is lower: the middle 50% of its values lie between 3 and 3, against 3 to 4 across all rows; and wt is higher: the middle 50% of its values lie between 3.52 and 4.07, against 2.47 to 3.57 across all rows. Less strongly, 5 more variables set it apart as well.
#> 
#> Cluster 2 holds 8 rows, 25.0% of the data (stable). What sets it apart: mpg is higher: the middle 50% of its values lie between 22.8 and 30.4, against 15.2 to 22.8 across all rows; cyl is '4' for 100% of them, against 34% across all rows; wt is lower: the middle 50% of its values lie between 1.62 and 2.20, against 2.47 to 3.57 across all rows; and am is '0' for none of them, against 59% across all rows. Less strongly, 6 more variables set it apart as well.
#> 
#> Cluster 3 holds 7 rows, 21.9% of the data (moderate). What sets it apart: qsec is higher: the middle 50% of its values lie between 18.9 and 20.2, against 16.9 to 18.9 across all rows; vs is higher: the middle 50% of its values lie between 1 and 1, against 0 to 1 across all rows; cyl is '8' for none of them, against 44% across all rows; and am is '0' for 100% of them, against 59% across all rows. 1 of its members sits close enough to another cluster to be uncertain.
#> 
#> Cluster 4 holds 5 rows, 15.6% of the data (stable). What sets it apart: carb is higher: the middle 50% of its values lie between 4 and 6, against 2 to 4 across all rows; qsec is lower: the middle 50% of its values lie between 14.6 and 16.5, against 16.9 to 18.9 across all rows; gear is higher: the middle 50% of its values lie between 4 and 5, against 3 to 4 across all rows; and am is '0' for none of them, against 59% across all rows. Less strongly, 1 more variable sets it apart as well.
```

The reduction picks its own method from the column types: PCA when they
are all numeric, multiple correspondence analysis when they are all
categorical, and a mixed method when both are present, as here.

Two things get reported that a bare cluster number does not give you.
**Stability** is a property of the cluster – resample the rows,
recluster, and see whether the same grouping comes back. **Ambiguity**
is a property of the row, from its silhouette width, and is independent
of cluster size: a point can sit on the boundary between two clusters
inside a large and perfectly stable one, which a size-based check would
never surface.

``` r

p$cluster$clusters
#>   cluster size    pct   jaccard stability mean_silhouette anomalous
#> 1       1   12 37.500 0.9950000    stable       0.6753886     FALSE
#> 2       2    8 25.000 0.8216667    stable       0.5484379     FALSE
#> 3       3    7 21.875 0.6975873  moderate       0.2524941     FALSE
#> 4       4    5 15.625 0.7656190    stable       0.2860767     FALSE
```

A cluster is characterised by v-test, the same device `FactoMineR`’s
`catdes()` uses. It is a threshold rather than a test, and is documented
as one: the clusters were found from the very coordinates being tested,
so the usual sampling argument does not apply. What speaks to whether a
partition is real is the stability and silhouette figures above, not the
characterisation.

## Missing values

[`ilm_describe_na_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_na_all.md)
counts them. The harder question is whether they matter, and that has a
more useful answer than it usually gets.

``` r

set.seed(4)
n  <- 500
z  <- rnorm(n)
x  <- 0.6 * z + rnorm(n)
y  <- 0.5 * x + 0.3 * z + rnorm(n)
d2 <- data.frame(x = x, z = z, y = y)
d2$x[plogis(1.2 * z - 0.4) > runif(n)] <- NA   # missingness follows z

ilm_check_missing(d2, y = "y", verbose = FALSE)
#> <ilm_missing> 309 of 500 rows complete (61.8%)
#> 
#>   missing by variable
#>     x                       191   38.2%
#> 
#>   pattern: monotone (2 distinct)
#>   verdict: RELATED_TO_COVARIATES 
#>     related to covariates, not to the outcome: complete cases stay unbiased 
#> 
#>   does missingness depend on `y` once the others are held fixed?
#>     x                partial r = 0.044, p = 0.4
#> 
#>   strongest associations with missingness
#>     x                <- z                0.490
#>     x                <- y                0.216  (outcome)
#> 
#>   MNAR cannot be ruled out by any test; that needs a sensitivity analysis.
```

Note the verdict. Missingness here depends on `z` — so this is
emphatically not MCAR — and yet **complete-case analysis is unbiased**.
Dropping incomplete rows needs missingness to be independent of the
*outcome* given the covariates, which is far weaker than MCAR and is
satisfied here. Imputing would buy back some precision and nothing else.

That distinction is why the check asks the question conditionally.
Missingness driven by `z` is *marginally* associated with `y`, because
`y` depends on `z` too; reading the verdict off that marginal
association would send you off to impute for nothing.

Change what drives the missingness and the advice changes with it:

``` r

d3 <- d2
d3$x <- x
d3$x[plogis(1.0 * y - 0.4) > runif(n)] <- NA   # now it follows the outcome
ilm_check_missing(d3, y = "y", verbose = FALSE)$verdict
#> [1] "RELATED_TO_OUTCOME"
```

Measured over 200 replicates at about 40% missing, the coefficient on
`x` came back with a bias of 0.0001 and 98% interval coverage in the
first case, against a bias of −0.099 — a fifth of the effect — and 61.5%
coverage in the second.

### Imputing, when it is actually needed

Imputing and pooling are `illume`’s, because pooling needs a model. With
`illume` loaded:

``` r

imp <- ilm_impute(d3, m = 10, seed = 1, verbose = FALSE)
ilm_mi_pool(imp, y ~ x + z, family = "gaussian")
```

Each missing value is **drawn** from the predictive distribution rather
than set to a fitted mean, and the `m` completed datasets are pooled by
Rubin’s rules, where the spread of the estimates *across* imputations
becomes part of the reported uncertainty. `fmi` is the fraction of
information lost to missingness.

`single = TRUE` gives one completed dataset and warns. Filling values in
once and analysing as though they had been observed gets the point
estimate about right and the standard errors badly wrong: in that same
simulation, single imputation covered 79% to 89% where the nominal rate
was 95%, because nothing in its standard errors knows part of the data
was invented.

### Which values go missing together

``` r

pn <- ilm_profile_na(airquality, k_max = 4, B = 25, seed = 1)
#> ilm_reduce_na(): dropping column(s) whose missingness never varies (always or never missing): Wind, Temp, Month, Day
#> Warning: k was chosen as 4, which is the largest value searched. The curve had
#> not turned, so this is where the search stopped rather than where the evidence
#> pointed. Raise `k_max`, or set `k` from what the design says. On mixed data a
#> selector can also lock onto the number of category combinations rather than the
#> number of clusters; plot(x) shows the gap curve.
cat(pn$summary[1:2], sep = "\n\n")
#> Cluster 1 holds 111 rows, 72.5% of the data (stable). What sets it apart: Ozone is missing for none of them, against 24% across all rows.
#> 
#> Cluster 2 holds 35 rows, 22.9% of the data (stable). What sets it apart: Ozone is missing for 100% of them, against 24% across all rows.
```

The same reduce-cluster-describe pipeline, pointed at present/missing
indicators instead of values. A block of columns that go missing as one
points at a shared cause — a section of a form a whole group skipped —
which is a different problem from values going one at a time, and calls
for a different fix.

And the boundary of all this is worth stating plainly: **MAR and MNAR
cannot be told apart from the observed data.** That is a theorem, not a
gap in the implementation — the data that would separate them are the
ones that are missing. Everything above tests MCAR and describes what
missingness is related to among the things you can see. If the chance of
a value going missing depends on the value itself, no test will reveal
it, and the remedy is a sensitivity analysis rather than a better
diagnostic.

## Into the model

The point of all this is what comes next. The exploration tells you
which family to consider, which variables will cause trouble, and
whether an AR(1) term is even possible. The model is `illume`’s:

``` r

library(illume)
f <- ilm_model(score ~ income + grp + (1 | id), data = d,
               family = "gaussian", verbose = FALSE)
ilm_plot_model(f, "coef")
```

and, once it is fitted, what it says in words:

``` r

ilm_interpret(f, ame = FALSE)
```

See
[`vignette("profiling")`](https://huttoncp.github.io/illumex/articles/profiling.md)
for structure across many columns and
[`vignette("anomaly-detection")`](https://huttoncp.github.io/illumex/articles/anomaly-detection.md)
for rows that are implausible as combinations. In `illume`:
`vignette("workflow", package = "illume")` for where this sits in an
analysis, `vignette("regression-models", package = "illume")` for the
modelling, and `vignette("causal-models", package = "illume")` for
turning an association into a causal claim.
