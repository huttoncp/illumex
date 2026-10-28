# Study B: parallel analysis variants for choosing ndim. Report (2026-10-11)

**The study:** dev/studies/ndim_pa_variants.R, as registered, with addendum 1 (which withdrew the deflated arm, R1 and the shadow designs before any pilot).

**The full run:** 13:46 to 16:28 on 2026-10-11, one core, from a frozen clone of the registered study, with a pinned study library (diptest 0.77-2; the log records every loaded package).
- 220 replicates (20 x 11 designs). 2 h 42 min, against 3.2 h registered, so no stop.
- Rows: results/ndim_pa_variants_full_arms.csv and _dims.csv.
- Rules applied by ndim_pa_variants_summary.R (output in results/ndim_pa_variants_full_summary.txt); the post hoc description by ndim_pa_variants_posthoc.R (results/..._posthoc.txt).

## Under the registered rules

**R3, the screen's statistic.** The dip test is NOT clearly better, so the screen would use Sarle's BC, with no new dependency.
- Detection (keeping a left-out dimension with at least 25% of the between-group SS, pooled over separated, overlapping and numeric_many): BC 0.000, dip 0.000.
- False alarms (pooled over continuum, unimodal_heavy and bimodal_null): BC 0.000, dip 0.033 (SE 0.023), all on bimodal_null (0.10 there).
- diptest does not go into Suggests.

**R2, does the screen join the display?** NO.
- On separated, screen_bc minus perm95 is -0.048 ARI (SE 0.048). It needed +0.10.
- It is behind perm95 by more than 0.02 on mixed_few_noise (-0.626), mixed_counts (-0.194), mixed_many (-0.138) and separated (-0.048).

**So the display shows plain permutation PA (perm95), and no screen.**

## Mean ARI by design (truth from the generator)

| design | fixed5 | perm95 | screen_bc | oracle |
|---|---|---|---|---|
| mixed_few | 0.960 | 0.999 | 0.999 | 0.999 |
| mixed_few_noise | 0.361 | **0.997** | 0.370 | 0.997 |
| mixed_counts | 0.444 | **0.830** | 0.636 | 0.493 |
| mixed_many | 0.700 | **0.844** | 0.706 | 0.970 |
| numeric_few | 1.000 | 1.000 | 1.000 | 1.000 |
| numeric_many | 0.977 | 0.978 | 0.978 | 0.978 |
| separated | **0.646** | 0.405 | 0.357 | 0.646 |
| overlapping | 0.023 | 0.000 | 0.000 | 0.023 |
| bimodal_null | 0.380 | 0.467 | 0.536 | 0.728 |

continuum and unimodal_heavy have one group, so ARI is undefined there. perm95 found k = 1 in every replicate of both. These means agree with ndim_choice on the shared designs: PA well ahead where a categorical column splits groups, and behind on separated.

## Why the screen fails (POST HOC, descriptive only)

1. **The separated design's cluster dimension is not bimodal enough to see.**
   - The left-out dimension carrying the most between-group SS holds a median of 11% of it (25% was registered as "carrying" structure, so R3's detection could not score there).
   - Its median BC is 0.53, below 5/9, and its median dip p is 0.56. BC would have kept it in 5% of replicates, the dip test in none.
   - A 25/75 mixture this far apart is barely bimodal in one dimension. No unimodality test rescues it, so the separated design's loss under variance-ordered truncation stands. That bears on the default ndim.
2. **On mixed data the screen keeps the wrong dimensions.**
   - Dimensions built from a categorical column's indicators are multimodal by construction. BC kept 61% of the left-out dimensions on mixed_few_noise, 27% on mixed_counts and 10% on mixed_many, with a median between-group share of 0.0003, 0.009 and 0.0002.
   - That is exactly the failure PA corrects, reintroduced.

## Secondary arms (outside the rules)

The _select arms are in the arms file and add nothing to the conclusion: the screens rarely keep a dimension that matters.

## A design note, stated plainly

R3's detection threshold, 25% of the between-group SS, was set without checking what the target dimension carries. On separated it carries about 11%. The verdict stands as registered (the dip test is not clearly better; neither detects). A lower threshold would not change it, since neither statistic keeps that dimension even when it is the one that matters.

## For the display and the default ndim

- The display shows perm95, B = 30, 95th percentile, as the engine computes it.
- The separated design's failure is variance ordering itself, not PA's count. It belongs with the choice of k after the reduction, and with the question of whether any truncation should be the default.
