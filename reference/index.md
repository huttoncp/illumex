# Package index

## Describing data

Descriptive statistics, counts, and the things that are wrong with a
data frame before any model sees it.

- [`ilm_describe_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_all.md)
  : Describe every column of a data frame
- [`ilm_describe()`](https://huttoncp.github.io/illumex/reference/ilm_describe.md)
  : Class-aware description of one variable
- [`ilm_frame_issues()`](https://huttoncp.github.io/illumex/reference/ilm_frame_issues.md)
  : Problems that belong to the data frame as a whole
- [`ilm_gauss_check()`](https://huttoncp.github.io/illumex/reference/ilm_gauss_check.md)
  : How far a variable is from Gaussian, and why
- [`ilm_copies()`](https://huttoncp.github.io/illumex/reference/ilm_copies.md)
  : Find copied or duplicated rows
- [`ilm_counts()`](https://huttoncp.github.io/illumex/reference/ilm_counts.md)
  : Frequency counts of a vector's unique values
- [`ilm_counts_all()`](https://huttoncp.github.io/illumex/reference/ilm_counts_all.md)
  : Frequency counts for every column
- [`ilm_counts_tb()`](https://huttoncp.github.io/illumex/reference/ilm_counts_tb.md)
  : The most and least frequent values, side by side
- [`ilm_counts_tb_all()`](https://huttoncp.github.io/illumex/reference/ilm_counts_tb_all.md)
  : Most and least frequent values for every column
- [`ilm_describe_clusters()`](https://huttoncp.github.io/illumex/reference/ilm_describe_clusters.md)
  : Describe data whose rows sit in sampling clusters
- [`ilm_dupes()`](https://huttoncp.github.io/illumex/reference/ilm_dupes.md)
  : Duplicated rows only

## The cleaning loop

Checks that find messy data and say what they saw, remedies written out
as code, and a log of every remedy applied and why, which writes out as
a script that replays the cleaning.

- [`ilm_check_data()`](https://huttoncp.github.io/illumex/reference/ilm_check_data.md)
  : Check a data frame for problems, all at once
- [`ilm_remedies()`](https://huttoncp.github.io/illumex/reference/ilm_remedies.md)
  [`print(`*`<ilm_remedies>`*`)`](https://huttoncp.github.io/illumex/reference/ilm_remedies.md)
  : Remedies for what a check found
- [`ilm_apply_remedy()`](https://huttoncp.github.io/illumex/reference/ilm_apply_remedy.md)
  : Make one remedy
- [`ilm_cleaning_script()`](https://huttoncp.github.io/illumex/reference/ilm_cleaning_script.md)
  : Write the cleaning as an R script
- [`ilm_cleaning_log()`](https://huttoncp.github.io/illumex/reference/ilm_cleaning_log.md)
  : The cleaning log
- [`ilm_check_frame()`](https://huttoncp.github.io/illumex/reference/ilm_check_frame.md)
  : Check the data frame as a whole
- [`ilm_data_id()`](https://huttoncp.github.io/illumex/reference/ilm_data_id.md)
  : A data frame's fingerprint
- [`ilm_drop_cols()`](https://huttoncp.github.io/illumex/reference/ilm_drop_cols.md)
  : Leave columns out
- [`ilm_remedy_assemble()`](https://huttoncp.github.io/illumex/reference/ilm_remedy_assemble.md)
  : Assemble a remedies table
- [`ilm_remedy_find()`](https://huttoncp.github.io/illumex/reference/ilm_remedy_find.md)
  : Find a remedy in a table by id or key
- [`ilm_remedy_key()`](https://huttoncp.github.io/illumex/reference/ilm_remedy_key.md)
  : Build a remedy key
- [`ilm_remedy_table()`](https://huttoncp.github.io/illumex/reference/ilm_remedy_table.md)
  [`c(`*`<ilm_remedies>`*`)`](https://huttoncp.github.io/illumex/reference/ilm_remedy_table.md)
  : A table of remedies of one's own
- [`ilm_secret()`](https://huttoncp.github.io/illumex/reference/ilm_secret.md)
  : Keep a secret out of the record

## Cleaning

Tidying a data frame and recoding values that were entered wrongly.

- [`ilm_recode_errors()`](https://huttoncp.github.io/illumex/reference/ilm_recode_errors.md)
  : Replace known-bad values with NA or another value
- [`ilm_recode_errors_vec()`](https://huttoncp.github.io/illumex/reference/ilm_recode_errors_vec.md)
  : Replace known-bad values in a vector
- [`ilm_translate()`](https://huttoncp.github.io/illumex/reference/ilm_translate.md)
  : Recode a variable against a dictionary held as two vectors
- [`ilm_wash_df()`](https://huttoncp.github.io/illumex/reference/ilm_wash_df.md)
  : Clean up a messy data frame

## Uncertainty without a model

Bootstrap intervals for a statistic, and for differences between groups.

- [`ilm_boot_ci()`](https://huttoncp.github.io/illumex/reference/ilm_boot_ci.md)
  : Bootstrap confidence interval for a statistic
- [`ilm_boot_diff()`](https://huttoncp.github.io/illumex/reference/ilm_boot_diff.md)
  : Bootstrap intervals for differences between groups
- [`ilm_plot_boot_diff()`](https://huttoncp.github.io/illumex/reference/ilm_plot_boot_diff.md)
  : The bootstrap distribution behind a group difference

## Outliers and anomalies

A value extreme for its own column, against a row implausible as a
combination.

- [`ilm_anomaly()`](https://huttoncp.github.io/illumex/reference/ilm_anomaly.md)
  : Rows that do not fit the pattern the other rows make
- [`ilm_anomalous()`](https://huttoncp.github.io/illumex/reference/ilm_anomalous.md)
  : The rows an anomaly scan flagged
- [`ilm_plot_anomaly()`](https://huttoncp.github.io/illumex/reference/ilm_plot_anomaly.md)
  : See an anomaly scan
- [`ilm_outliers()`](https://huttoncp.github.io/illumex/reference/ilm_outliers.md)
  : Flag unusual values in a numeric vector
- [`ilm_outliers_all()`](https://huttoncp.github.io/illumex/reference/ilm_outliers_all.md)
  : Flag unusual values across a data frame

## Structure: profile, cluster, reduce

ilm_profile() is the front door: it reduces, clusters and describes the
groups in one call, and handles mixed columns. The other two are the
same pipeline taken a step at a time, for when you want the coordinates
or the partition on their own.

- [`ilm_profile()`](https://huttoncp.github.io/illumex/reference/ilm_profile.md)
  : Profile a dataset: reduce, cluster, and describe the clusters
- [`ilm_var_contrib()`](https://huttoncp.github.io/illumex/reference/ilm_var_contrib.md)
  : What each variable contributes to a clustering
- [`ilm_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_cluster.md)
  : Cluster observations, choosing the number of clusters
- [`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md)
  : Reduce a data frame's variables to a few dimensions
- [`ilm_glrm()`](https://huttoncp.github.io/illumex/reference/ilm_glrm.md)
  : A low-rank model with a loss chosen per column
- [`ilm_profile_na()`](https://huttoncp.github.io/illumex/reference/ilm_profile_na.md)
  : Profile which values are missing, and for whom
- [`ilm_cluster_na()`](https://huttoncp.github.io/illumex/reference/ilm_cluster_na.md)
  : Cluster observations by which values they are missing
- [`ilm_reduce_na()`](https://huttoncp.github.io/illumex/reference/ilm_reduce_na.md)
  : Reduce a data frame's missingness pattern to a few dimensions
- [`ilm_plot_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_plot_cluster.md)
  [`ilm_plot_cluster_na()`](https://huttoncp.github.io/illumex/reference/ilm_plot_cluster.md)
  : Map the clusters
- [`ilm_plot_cluster_gap()`](https://huttoncp.github.io/illumex/reference/ilm_plot_cluster_gap.md)
  [`ilm_plot_cluster_gap_na()`](https://huttoncp.github.io/illumex/reference/ilm_plot_cluster_gap.md)
  : The gap statistic across every k considered
- [`ilm_plot_profile()`](https://huttoncp.github.io/illumex/reference/ilm_plot_profile.md)
  [`ilm_plot_profile_na()`](https://huttoncp.github.io/illumex/reference/ilm_plot_profile.md)
  : Map the clusters from a profile
- [`ilm_plot_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_plot_reduce.md)
  [`ilm_plot_reduce_na()`](https://huttoncp.github.io/illumex/reference/ilm_plot_reduce.md)
  : Map the observations from a reduction
- [`ilm_plot_reduce_contrib()`](https://huttoncp.github.io/illumex/reference/ilm_plot_reduce_contrib.md)
  [`ilm_plot_reduce_contrib_na()`](https://huttoncp.github.io/illumex/reference/ilm_plot_reduce_contrib.md)
  : Which variables a dimension is made of
- [`ilm_plot_reduce_scree()`](https://huttoncp.github.io/illumex/reference/ilm_plot_reduce_scree.md)
  [`ilm_plot_reduce_scree_na()`](https://huttoncp.github.io/illumex/reference/ilm_plot_reduce_scree.md)
  : Scree plot for a reduction

## Missing values

What is missing, whether it matters, and which values go missing
together. Filling them in and pooling across the imputations is
illume’s.

- [`ilm_check_missing()`](https://huttoncp.github.io/illumex/reference/ilm_check_missing.md)
  : What the missing values look like, and whether they matter
- [`ilm_describe_na_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_na_all.md)
  : Missingness in every variable
- [`ilm_describe_na()`](https://huttoncp.github.io/illumex/reference/ilm_describe_na.md)
  : Missingness in one variable
- [`ilm_plot_missing()`](https://huttoncp.github.io/illumex/reference/ilm_plot_missing.md)
  : Proportion missing, by variable
- [`ilm_plot_na()`](https://huttoncp.github.io/illumex/reference/ilm_plot_na.md)
  : Missing values in one column, across groups
- [`ilm_plot_na_all()`](https://huttoncp.github.io/illumex/reference/ilm_plot_na_all.md)
  : Missing values by column

## Plots

Built on tinyplot, named for what they show.

- [`ilm_plot()`](https://huttoncp.github.io/illumex/reference/ilm_plot.md)
  : Adaptive plot of one or two variables
- [`ilm_plot_all()`](https://huttoncp.github.io/illumex/reference/ilm_plot_all.md)
  : Small multiples of every variable
- [`ilm_pick_geom()`](https://huttoncp.github.io/illumex/reference/ilm_pick_geom.md)
  : Which geom would be drawn, and why
- [`ilm_geom_spec()`](https://huttoncp.github.io/illumex/reference/ilm_geom_spec.md)
  : What each geom requires
- [`ilm_plot_bar()`](https://huttoncp.github.io/illumex/reference/ilm_plot_bar.md)
  : Bar plot
- [`ilm_plot_box()`](https://huttoncp.github.io/illumex/reference/ilm_plot_box.md)
  : Boxplot
- [`ilm_plot_c()`](https://huttoncp.github.io/illumex/reference/ilm_plot_c.md)
  : Combine several plots into one figure
- [`ilm_plot_density()`](https://huttoncp.github.io/illumex/reference/ilm_plot_density.md)
  : Density plot
- [`ilm_plot_histogram()`](https://huttoncp.github.io/illumex/reference/ilm_plot_histogram.md)
  : Histogram
- [`ilm_plot_line()`](https://huttoncp.github.io/illumex/reference/ilm_plot_line.md)
  : Line plot
- [`ilm_plot_scatter()`](https://huttoncp.github.io/illumex/reference/ilm_plot_scatter.md)
  : Scatter plot
- [`ilm_plot_stat_error()`](https://huttoncp.github.io/illumex/reference/ilm_plot_stat_error.md)
  : Group means or medians with an error bar
- [`ilm_plot_var()`](https://huttoncp.github.io/illumex/reference/ilm_plot_var.md)
  : Plot one or two variables, choosing the geometry
- [`ilm_plot_var_all()`](https://huttoncp.github.io/illumex/reference/ilm_plot_var_all.md)
  : Plot every column of a data frame
- [`ilm_plot_var_pairs()`](https://huttoncp.github.io/illumex/reference/ilm_plot_var_pairs.md)
  : Pairwise plots
- [`ilm_plot_violin()`](https://huttoncp.github.io/illumex/reference/ilm_plot_violin.md)
  : Violin plot

## Choosing rows and columns

Which rows and columns a function uses: `subset` and `cols` with their
options, a random sample of rows, and the data a call would use.

- [`ilm_selection`](https://huttoncp.github.io/illumex/reference/ilm_selection.md)
  : How columns can be chosen

- [`ilm_subset()`](https://huttoncp.github.io/illumex/reference/ilm_subset.md)
  : Rows and columns of a data frame, chosen as illumex chooses them

- [`ilm_sample()`](https://huttoncp.github.io/illumex/reference/ilm_sample.md)
  :

  A random sample of rows, or of whole groups, for `subset`

## Example data

A grouped data set with a known structure, used throughout the examples.

- [`ilm_sim()`](https://huttoncp.github.io/illumex/reference/ilm_sim.md)
  : A simulated mixed-type dataset for testing and examples

## Shared parameters and print methods

Documentation shared across functions, and methods you call by printing
rather than by name.

- [`ilm_progress_arg`](https://huttoncp.github.io/illumex/reference/ilm_progress_arg.md)
  : Progress reporting in illumex and illume
