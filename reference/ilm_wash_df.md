# Clean up a messy data frame

Removes empty rows and columns, standardises names to snake_case, and
converts text columns that are really numeric or logical.

## Usage

``` r
ilm_wash_df(
  data,
  clean_names = TRUE,
  retype = TRUE,
  drop_empty = TRUE,
  column_to_rownames = FALSE,
  names_col = NULL,
  encoding = NULL
)
```

## Arguments

- data:

  A data frame.

- clean_names:

  Standardise column names to snake_case.

- retype:

  Convert character columns that are really numeric or logical.

- drop_empty:

  Drop rows and columns that are entirely missing or blank.

- column_to_rownames:

  Use a column's values as row names.

- names_col:

  The column to use when `column_to_rownames = TRUE`.

- encoding:

  The encoding the file was saved in, such as `"windows-1252"` (or
  `"latin1"`), to convert the text that is not valid UTF-8 from. `NULL`,
  the default, converts nothing.
  [`iconvlist()`](https://rdrr.io/r/base/iconv.html) lists the encodings
  this system knows.

## Value

A cleaned data frame, of the input's class for a tibble or a data.table
(a plain data.frame when `column_to_rownames = TRUE`, since neither
keeps row names) and a plain data.frame for any other. When some text is
not valid UTF-8, `attr(x, "not_utf8")` is a data frame of the columns
holding it (by their cleaned names): `column`, `n`, and the first `rows`
of `data` where it is. With `encoding`, `attr(x, "converted")` gives the
values converted per column (`column`, `n`).

## Details

Conversion happens only when every non-missing value converts cleanly,
so a single stray `"n/a"` cannot silently turn a column into `NA`s. A
text column of whole numbers becomes integer; the values are unchanged.
Columns that are already numbers, and factors, are left as they are.

Text that is not valid UTF-8 – what a file saved in another encoding,
such as Windows-1252, gives when read as UTF-8 – is left as it is, its
bytes unchanged: no encoding is guessed. A warning names each column
holding such text, with its count and first rows, and says how to read
the file in its own encoding; the result keeps the same table as
`attr(x, "not_utf8")`. A column name that is not valid UTF-8 is cleaned
with each stray byte written out (`caf_e9`).

Given the file's encoding, `encoding = "windows-1252"` say, the text
that is not valid UTF-8 – values, factor levels and column names – is
converted from it, and a message says how many values were converted in
each column. Nothing that is already valid UTF-8 is touched, and no
encoding is ever guessed. A value that does not convert, or does not
convert back to the same bytes, is left as it is and reported as above.

Names follow the rules of janitor's `make_clean_names()`, without
needing janitor, and no letter is dropped: `"%"` becomes `percent` and
`"#"` `number`; camelCase is split; accented Latin letters become plain
ones (`"é"` becomes `e`, `"ß"` becomes `ss`, `"œ"` becomes `oe`);
letters of other scripts are kept as they are; a name starting with a
digit gains an `x`; and a repeated name gains `_2`, `_3` and so on, as
janitor numbers it.

It washes the whole data frame, and takes no `subset` or `cols`: a
column has one type, which retyping decides from all of its rows. To
wash part of a data frame, choose it with
[`ilm_subset()`](https://huttoncp.github.io/illumex/reference/ilm_subset.md)
first.

## Examples

``` r
m <- data.frame("Col One" = c("1", "2", ""), someFlag = c("TRUE", "FALSE", ""),
                check.names = FALSE)
ilm_wash_df(m)
#>   col_one some_flag
#> 1       1      TRUE
#> 2       2     FALSE
```
