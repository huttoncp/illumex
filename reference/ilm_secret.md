# Keep a secret out of the record

A remedy argument that must not be written down – the key a column is
pseudonymised with – is given as `ilm_secret("ENV_VAR")`. Its value is
read from that environment variable when the remedy is made, and only
the call `ilm_secret("ENV_VAR")` appears in the remedy's `change`, the
cleaning log and the cleaning script, whose comment names the variable
to set.

## Usage

``` r
ilm_secret(var)
```

## Arguments

- var:

  The environment variable's name.

## Value

The secret's value, a string.

## Details

If the variable is not set, an interactive session asks for the value; a
non-interactive one stops and names the variable.

## Examples

``` r
Sys.setenv(MY_PSEUDO_KEY = "not a real key")
nchar(ilm_secret("MY_PSEUDO_KEY"))
#> [1] 14
Sys.unsetenv("MY_PSEUDO_KEY")
```
