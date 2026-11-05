# EXPLORATORY: why the oracle did worst on the separated design

This is not a registered study, and it changes nothing by itself. `ndim_separated_explore.R` re-ran `ndim_choice.R`'s separated design exactly: the same 20 replicates, seeds, `ilm_reduce()` and `ilm_cluster()` calls, at 3 dimensions (the oracle) and 5 (fixed5). It kept the gap curves and applied every rule `cluster::maxSE()` offers to the same curves.
- It reproduces the study: k = 1 in 13 of 20 at 3 dimensions, and k = 4 in 13 of 20 at 5.
- Outputs: `ndim_separated_rules.csv`, `ndim_separated_curves.csv` and `ndim_separated_gap_curves.png`.

**k = 4 out of 20, by rule:**

| ndim | firstSEmax (the default) | Tibs2001SEmax | globalSEmax | firstmax | globalmax |
|---|---|---|---|---|---|
| 3 | 2 | 2 | 0 | 1 | 0 |
| 5 | 13 | 13 | 0 | 13 | 0 |

**The mean gap curve** (SE about 0.008 at every k):

| k | 1 | 2 | 3 | 4 | 5 | 6 | 8 | 10 |
|---|---|---|---|---|---|---|---|---|
| 3 dimensions, the 13 replicates choosing k = 1 | 0.411 | **0.394** | 0.506 | 0.534 | 0.546 | 0.551 | 0.595 | 0.592 |
| 5 dimensions, the 13 replicates choosing k = 4 | 0.454 | 0.467 | 0.530 | **0.578** | 0.574 | 0.576 | 0.591 | 0.609 |

What it shows:
- **At 3 dimensions, the curve falls from k = 1 to k = 2.** With four clusters at the corners of a 3-dimensional simplex, the best split in two separates them less, relative to the reference, than not splitting at all. Every "first" rule stops at k = 1 there: firstSEmax, Tibs2001SEmax, and firstmax, which uses no standard error at all.
- **It then climbs past 4 with no peak,** to about 0.60 at 8 to 10, so the "global" rules choose 8 to 10.
- **At 5 dimensions,** the two noise columns lift k = 1 and 2 relative to the reference. The curve climbs to a knee at 4 and is flat after, which the first rules find.
- **So the stopping rule explains the k = 1 choices, but no rule `maxSE()` offers recovers 4 at 3 dimensions.** The shape of the curve does it: the dip at 2 and the lack of a peak at 4.
- The reference the gap statistic draws, `clusGap()`'s default, is uniform over the data's principal-component box. On 3 standardised axes holding a simplex, it may be what makes 2 clusters look worse than 1. That is a lead, not tested here.
