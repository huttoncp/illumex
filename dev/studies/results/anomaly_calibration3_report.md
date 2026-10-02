_Commit hashes in this report are from illumex's history before its public restart on 2026-10-01; they are not in this repository._

# Anomaly calibration, stage 3: report

Registration: `dev/studies/anomaly_calibration3.R` at 88286a9, with its two
addenda. Build: illumex installed from an unedited clone of 88286a9
into a pinned library (RemoteSha 88286a9), mclust 6.1;
each process checked the build before its first dataset, and every log says
"build check passed". Summary: the registered `summarise` mode, run from the
same clone and library, over the three processes' files
(`anomaly_calibration3_summary.txt`, beside this report).

## Completeness, checked before any analysis

A completeness script, not kept here, rebuilt every expected key from the run
section's own loops: 50,400 rows expected, 50,400 found; none missing, none
repeated, none outside the design; every replicate in the process that
should hold it (every third, from the k-th); no missing outcome. The default
arm reproduced `ilm_anomaly()`'s p-values exactly in 2,120 of 2,120 datasets
checked.

## The registered verdicts, literally

**R1 (item 143, the arms): no arm qualifies; nothing changes until the results are reviewed.**

| arm | clean cells held (of 36) | planted rows flagged, normal (28 cells) | all 60 power cells | floor 0.3548 |
|---|---|---|---|---|
| default | 6 | 0.4731 | 0.5506 | above, but fails the clean cells |
| heavy_kept | 2 | 0.5367 | 0.5940 | above, fails the clean cells |
| heavy_kept_q90 | 3 | 0.4647 | 0.4527 | above, fails the clean cells |
| mixture | 8 | 0.4189 | 0.4941 | above, fails the clean cells |

The default holds in the same 6 of 36 cells as stage 2, so this run
reproduces stage 2 where it repeats it.

**R2 (item 144, the warning's threshold): 0.07, which decides as 2/40 =
0.050.** Admissible thresholds: 0.01 to 0.07. At the p-values the check can
take, 0.07 warns exactly when p <= 0.050.

**R3 (item 88, the five-column excess): not explained by the rank, the
degrees-of-freedom factor or the matching alone.** rank2 holds in 1 of 3
p = 5 cells (the default in 0) and 2 of 3 p = 8 controls; nodf 0 of 3 and 1
of 3; nomatch 0 of 3 and 3 of 3. None meets the rule.

**R4 (item 88, the mixture score): does not fix both failures.** It holds in
6 of 33 heavy-tailed and clustered cells.

**Clustered data (reported, no rule):** the default arm's share of datasets
flagging anything is 0.105 (n 400, p 5), 0.070 (400, 8), 0.025 (400, 15),
0.165 (1000, 5), 0.130 (1000, 8), 0.010 (1000, 15), so it holds in 3 of 6
cells. The warning's rate at the R2 threshold (p <= 0.05) on the same cells
is 0.085, 0.080, 0.050, 0.135, 0.080, 0.050.

## Deviations from the plan

- Processes 1 and 3 started late, at 01:03 on 30 September; process 2 had
  finished at 22:45 on 29 September. The two were started later. This
  affects timing only. All three ran the same build (88286a9, checked in each log) and the
  same script from the same unedited clone.
- The run was split as the script's "k/3" option allows (every third
  replicate of every cell per process), not by part as the addendum's
  example has it. The addendum leaves scheduling open, and the
  completeness check shows the split covers the design exactly once.

No other deviation: the arms, parts, seeds, outcomes and rules are as
registered.

## Readings made after seeing the results (not registered)

1. **The mixture arm is the most powerful on scattered anomalies and blind
   to shared ones.** It flags 0.67 to 0.70 of planted rows in the random
   conditions (normal noise), the best of the four, but 0.009 at shared10
   and 0.30 at shared5. A shared shift is a small cluster of its own, so it
   gets its own mixture component. Its clean cells held include every
   p = 5 normal cell, where the default fails.
2. **Heavy tails defeat every arm, not only the default.** In the t3 and t5
   power cells the false-discovery proportion is 0.37 to 0.81 for every arm.
   No reference distribution tried here gives calibrated flags on
   heavy-tailed noise.
3. **The p = 5 excess may lie partly in the rank choice.** The default chose
   rank 1, not the true 2, in 149 of 600 datasets at p = 5, and fixing the
   rank at 2 (rank2) held in one p = 5 cell where the default holds in none.
   Not enough to explain it by R3's rule. A study of the rank choice at few
   columns would be a new registration.
4. **The tail warning will not catch clustered over-flagging.** At the R2
   threshold it fires on 5% to 13.5% of clustered datasets, about as often
   as on clean normal ones, since clustering does not fatten the residuals'
   tails. The help's sentence about clustered data carries that case, not
   the warning.
5. **On heavy-tailed clean data the warning fires almost always with 400
   rows or more and 8 or more columns.** At p <= 0.05 there, t3 and
   lognormal give 0.99 to 1.00 and t5 gives 0.79 to 1.00. It is weaker with
   150 rows or 5 columns: t3 at 150 rows and 8 columns 0.67, t5 at 150 rows
   and 5 columns 0.165.

## Recommendation

- Keep the default scoring, as R1 requires: no arm qualifies.
- Adopt the tail warning at the rule's threshold, p <= 0.05 (2 of 40
  reference datasets at least as extreme). Its false-warning rate on clean
  normal data stays within 0.10 plus Monte Carlo error in all nine cells.
- Keep the help's statement that the scan over-flags on heavy-tailed or
  clustered data and with few columns. Add that the warning covers heavy
  tails but not clustering (reading 4).
- Treat the mixture arm's power on scattered anomalies, and the rank choice
  at few columns, as candidates for a later registration, not for this
  release.
