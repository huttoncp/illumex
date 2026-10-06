# How many dimensions to keep before clustering: the report

The study is `dev/studies/ndim_choice.R`:
- the pre-registration, d429d97;
- the addendum, 1c14996, which redrew two designs after a pilot in which they could not be clustered at all, with a gate that the oracle passed on fresh replicates 101 and 102 (`ndim_choice_repilot.csv`);
- the summary script, 0e05a37, written before the results were read.

The full run went from a clean tree at 1c14996, on one core:
- 180 replicates, 520 rows, in 92 minutes, against 2 hours estimated;
- rows in `ndim_choice.csv`, the summary in `ndim_choice_summary.txt`, the log in `ndim_choice_run.log`.

## The rule, as registered

| criterion | result |
|---|---|
| (i) pa's mean ARI ahead by at least 0.05, and by more than 2 paired SE, in at least 2 of the 4 mixed conditions | **met**: mixed_few_noise +0.640 (SE 0.005), mixed_counts +0.399 (SE 0.012) |
| (ii) pa's mean ARI behind by more than 0.02 in no condition | **not met**: separated, -0.254 (SE 0.123) |
| (iii) on the continuum, pa's share of k = 1 not lower than fixed5's by more than 0.05 | **met**: 1.000 against 0.950 |

**Under the rule, ndim = 5 stays the default.** The help and the profiling vignette say how to choose `ndim`.

## What the results show, besides the rule

- **Where fixed5 fails.** Where a categorical column gives the reduction dimensions of its own, five dimensions lead the gap statistic to split the groups. A noise factor (mixed_few_noise) chose k = 10 in 20 of 20 replicates, ARI 0.36. Count columns with a 3-level factor (mixed_counts) chose 7 to 10, ARI 0.43. Parallel analysis chose 2 dimensions there and recovered k = 3 in every replicate, as the oracle did (ARI 1.00 and 0.83).
- **Where nothing changes.** On mixed_few, numeric_few and numeric_many, all three arms recover the truth (ARI 0.98 to 1.00). On mixed_many, pa and fixed5 are alike (0.83 and 0.81) and both fall short of the oracle (0.98). pa chose 2, 3 or 4 dimensions there, and only 2 matched the oracle.
- **The separated design: unexplained.**
  - This is the cluster-options study's well-separated design: 4 clusters 6 apart in 5 numeric columns. That study's gap statistic, on the raw coordinates, chose k = 4.
  - Here, after `ilm_reduce()`, it recovered k = 4 in 13 of 20 replicates at five dimensions, 8 of 20 at pa's 2 or 3, and 2 of 20 at the oracle's 3. The oracle, with the most structure per dimension, did worst: k = 1 in 13 of 20, ARI 0.24.
  - The reduction standardises the columns, which narrows the gaps along the three cluster axes relative to the two noise columns, but that alone does not explain why fewer dimensions do worse.
  - This is not explained, and it bears on the default either way. It is a finding for a separate look.
- **The overlapping design** (centres 2.5 apart) gave k = 1 under every arm in every replicate. Its clusters are not recoverable here, as the options study expected.

The results are reported as they are; nothing in the package changes until they are reviewed.
