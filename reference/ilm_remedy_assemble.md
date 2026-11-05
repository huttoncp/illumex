# Assemble a remedies table

The table's one constructor, for packages that build remedies of their
own kind of target (illume's models). Rows answering the same key are
listed once, with their checks joined; where they put the change in
different tiers the more cautious is taken; the table is ordered by tier
and numbered.

## Usage

``` r
ilm_remedy_assemble(
  rows,
  target_id,
  tiers,
  tier_note,
  by_hand = "by hand: a decision about what the data mean"
)
```

## Arguments

- rows:

  A list of remedies, each a list with `check`, `status`, `tier`,
  `remedy`, `key`, `change` (the code as text, `""` by hand) and
  `payload` (whatever the package's apply method needs, kept by id).

- target_id:

  What the table belongs to.

- tiers:

  The ordered tier set.

- tier_note:

  The paragraph print() closes with, saying how to apply one and what
  the tiers mean.

- by_hand:

  What print() says of a remedy with no change.

## Value

An `"ilm_remedies"` table.
