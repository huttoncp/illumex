# Contributing to illumex

Thanks for considering it. This file says what the project expects, so that a
contribution is not turned away for a reason nobody told you.

`illumex` is the exploratory half of [`illume`](https://github.com/huttoncp/illume),
and the two are maintained together. Anything that needs a fitted model belongs
in `illume`; anything that works on a data frame alone belongs here.

## Reporting a bug

Open an issue with a **reproducible example** — the smallest script that shows
the problem, with `set.seed()` if it involves simulation, plus the output of
`sessionInfo()`.

If the bug is that a function gives the wrong answer, say what you compared it
against. A report that says "illumex gives 0.42 and `FactoMineR` gives 0.51 on
this data" is worth more than a long description.

## Proposing a feature

Open an issue before writing code. Two questions get asked of anything new:

1. **Does it need a new hard dependency?** The answer has to be no. A package
   in `Suggests`, used behind `requireNamespace()`, is fine.
2. **If it is a check, what remedy does it name?** Every check names a fix that
   exists in this package or in `illume`. A check that reports a problem and
   leaves the user to find the answer elsewhere is a complaint, and will not be
   merged as one.

## Making a change

- Branch from `main`, one change per pull request.
- `devtools::document()` after touching roxygen. Committed `man/` must match.
  A new export also needs `dev/make_iml_aliases.R` and
  `pkgdown/make_reference_index.R` re-run.
- `devtools::test()` and `devtools::check()` must pass. CI runs the check on
  Linux, macOS and Windows against R release, devel and oldrel.
- A few small helpers exist in both packages (`R/illumex-utils.R` and the
  plotting characters in `R/ilm_plot.R`). `illume`'s tests fail if its copies
  differ from these, so change them in both.
- Match the surrounding code: `ilm_` prefix on exported functions, en-GB
  spelling, comments that say *why* rather than *what*.

## Using generative AI

Permitted, and disclose it. Say in the pull request which tools you used and
what for. You are responsible for reviewing and validating anything they
produced, and for the design decisions either way.

This is not a rule imposed on contributors from outside it: the package itself
was drafted in collaboration with a language model under human direction, and
says so in the README. The standard asked of a contribution is the standard the
package holds itself to -- that a claim about behaviour comes with a
measurement, and that the measurement is against something somebody else
wrote.

## Code of conduct

By participating you agree to abide by the [code of conduct](CODE_OF_CONDUCT.md).

## Questions

Open an issue. There is no mailing list.
