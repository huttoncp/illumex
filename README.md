# illumex <a href="https://huttoncp.github.io/illumex/"><img src="man/figures/logo.png" align="right" height="139" alt="illumex website" /></a>

**Describe, Clean, Plot and Profile Data Before a Model.**

`illumex` is the exploratory half of the [`illume`](https://github.com/huttoncp/illume)
workflow, usable on its own: the routine work before a model, each step one
call with a consistent interface. It asks one question more narrowly than most
exploratory tools -- not "what does this data look like" but **what about this
data will break a model** -- and the output is organised around the answer.

`illume` attaches `illumex`, so with `illume` loaded everything here is
available exactly as before. Every function is prefixed `ilm_`, as in `illume`.

> Not on CRAN yet. This is pre-release software under active development.

## Installation

```r
# install.packages("remotes")
remotes::install_github("huttoncp/illumex")
```

R >= 4.1. Nothing compiled: the grouped arithmetic runs on `collapse` and the
plots on `tinyplot`.

## A worked example

```r
library(illumex)

d <- ilm_sim()                       # a grouped dataset with a known structure

## what is in it, and what will break a model
ilm_describe_all(d)
ilm_frame_issues(d)                  # problems that belong to pairs of columns

## missing values: how much, and whether it matters for this question
ilm_check_missing(d, downtime ~ income + region)

## plots that carry the verdict
ilm_plot(d, "income")
ilm_plot_all(d)

## unusual values, and unusual rows
ilm_outliers_all(d)                  # extreme for its own column
ilm_anomaly(d)                       # implausible as a combination

## what shape is this data set?
ilm_profile(d, ndim = 5)
```

## What is in it

| | |
|---|---|
| **Describing** | `ilm_describe_all()`, `ilm_describe()`, `ilm_gauss_check()`, `ilm_counts()`, `ilm_dupes()`, `ilm_copies()`, `ilm_frame_issues()` |
| **Cleaning** | `ilm_wash_df()`, `ilm_recode_errors()`, `ilm_translate()` |
| **Uncertainty without a model** | `ilm_boot_ci()`, `ilm_boot_diff()` |
| **Unusual values** | `ilm_outliers()` for a value, `ilm_anomaly()` and `ilm_plot_anomaly()` for a row |
| **Structure** | `ilm_profile()`, `ilm_reduce()`, `ilm_cluster()`, `ilm_glrm()`, `ilm_var_contrib()` |
| **Missing values** | `ilm_check_missing()`, `ilm_describe_na_all()`, `ilm_plot_missing()`, `ilm_profile_na()` |
| **Plots** | `ilm_plot()`, `ilm_plot_all()` and around thirty `ilm_plot_*()` functions, named for what they show |
| **Example data** | `ilm_sim()` |

Imputing missing values and pooling across the imputations need a model, so
they are `illume`'s: `ilm_impute()` and `ilm_mi_pool()`.

## Documentation

| Vignette | Covers |
|---|---|
| `exploring-data` | descriptives, plots, bootstrap intervals, missing values |
| `profiling` | `ilm_reduce()`, `ilm_cluster()`, `ilm_profile()` |
| `anomaly-detection` | rows that are implausible as combinations |

```r
vignette("exploring-data", package = "illumex")
```

## How this package was built

`illumex` was developed in collaboration with
[Claude Opus 5.5](https://www.anthropic.com)
under a human-in-the-loop model, as part of `illume`, from which it was later
split. The division of labour was consistent throughout and is worth stating
plainly rather than leaving to be inferred.

**What the model did.** Prototyped and drafted the implementation, the
documentation and the simulation studies. Proposed designs, and argued for them
when it thought a choice was wrong.

**What the author did.** Specified what the package should be and what it
should refuse to do. Set the standing constraints -- no new hard dependencies,
every diagnostic names a remedy that exists in the package, no claim without a
measurement behind it. Made the decisions the design turned on: which methods
to include, which defaults to set, what to do when two defensible options
existed. Directed the ordering of the work, and rejected proposals.

**Status of independent review.** The author is reviewing the package and
testing it independently of the development process, and that work is *in
progress at the time of writing*. It will be complete before any release, and
this paragraph will say so when it is. Until then the package is pre-release
and should be treated as such.

### Why this is disclosed rather than mentioned quietly

Partly because it is true and the provenance of a statistical tool is a
reasonable thing for its users to know. Partly because it is increasingly
required: the Journal of Open Source Software has mandated an AI usage
disclosure since January 2026, and other venues are moving the same way.

But mostly because the disclosure is less load-bearing than it looks, and
saying so is the honest position. The question that matters about a statistical
package is not who typed it. It is whether its claims are checkable and whether
anyone checked them. Every validity claim here is a comparison against an
independent implementation that somebody else wrote, or a measurement against
planted structure whose answer is known.

That is the standard the package asks to be judged by, and it is the same
standard whoever wrote it.

## Standing on other people's shoulders

`collapse` does the grouped operations fast enough that exploration is usable
on real data, and `tinyplot` draws every plot. The profiling workflow is a
lighter-weight rebuild of `FactoMineR`'s `HCPC()` and `catdes()`, checked
against `FactoMineR::FAMD()`; `PCAmixdata`, `cluster` and `isotree` are used
where they are installed.

**And one predecessor.** This package is a direct descendant of
[`elucidate`](https://github.com/bcgov/elucidate), written by the same author
for the BC Public Service. The lineage is visible in the function names:
`describe()`, `describe_all()`, `counts()`, `counts_tb()`, `dupes()`,
`copies()`, `wash_df()`, `recode_errors()`, `translate()` and the whole
`plot_*()` family became `ilm_describe()`, `ilm_counts()`, `ilm_dupes()` and
the rest, with the same idea behind them -- that the routine work before a
model should be one call with a consistent interface, rather than six lines of
`sapply()` reassembled from memory every time. `illumex` reimplements that on a
different backend and extends it -- elucidate is built on `data.table`, `dplyr`
and `ggplot2`, this on `collapse` and `tinyplot`, and the two share an
interface rather than an implementation -- but the design is elucidate's and it
would be poor form to pretend otherwise.

## License

MIT. See `LICENSE`.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md). Two things get asked of anything new:
that it adds no hard dependency, and that any check names a remedy which
exists in this package or in `illume`.

Please note that this project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing, you agree
to abide by its terms.
