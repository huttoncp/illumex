# Craig's item 204: ilm_wash_df()'s names follow janitor's rules, in base R,
# with no letter dropped silently. `janitor` below is what
# janitor::make_clean_names() gave for `input` on 2026-09-29 (janitor 2.2.0,
# snakecase 0.11.1, stringi 1.8.3), frozen here so janitor need not be
# installed. illumex departs from it only where Craig ruled, listed in
# `departures`: letters of other scripts are kept rather than transliterated,
# and the ligatures oe and ae are made plain letters without janitor's split
# after them (oeuvre, not o_euvre).

names_input <- c(
  "Miles per gallon", "# of cylinders", "DISP", "% change", "P\u00e9riode",
  "\u00c2ge moyen", "\u00c9tat", "\u00e9tat", "Gr\u00f6\u00dfe", "\u0152uvre",
  "Ann\u00e9e", "Revenu m\u00e9dian ($)", "na\u00efve caf\u00e9", "camelCase",
  "someFlag", "HTMLParser", "x2Value", "Col One", "col.one", "col_one",
  "  padded  ", "a__b", "1st place", "2020", "", "NA", "a-b-c",
  "r\u00e9sum\u00e9 #2", "Cr\u00e8me br\u00fbl\u00e9e", "\u00c6r\u00f8",
  "\u0141\u00f3d\u017a", "\u00f1and\u00fa", "Stra\u00dfe", "c\u0153ur",
  "\u00c7a va?", "per cent%", "\u5e74\u9f62",
  "\u0412\u043e\u0437\u0440\u0430\u0441\u0442", "\u03b1\u03b2\u03b3",
  "age \u5e74\u9f62", "x", "x", "x", "v1", "v1", "Craig's age", "\"quoted\"",
  "a.b", "mpg/cyl", "x1_", "CO2 level", "pH", "ID", "userID", "\u00c9TAT", "if",
  "TRUE", "function", "_private", ".hidden", "\u00c7aVa", "\u00e9coleNormale",
  "\u00c0BC", "Hindi \u0939\u093f\u0928\u094d\u0926\u0940", "e\u0301te\u0301",
  "a&b", "a+b", "a@b", "x...1", "NULL", "\u00c9tatCivil", "Bra\u0219ov",
  "\u021a\u0103ri")

names_janitor <- c(
  "miles_per_gallon", "number_of_cylinders", "disp", "percent_change",
  "periode", "age_moyen", "etat", "etat_2", "grosse", "o_euvre", "annee",
  "revenu_median", "naive_cafe", "camel_case", "some_flag", "html_parser",
  "x2value", "col_one", "col_one_2", "col_one_3", "padded", "a_b",
  "x1st_place", "x2020", "x", "na", "a_b_c", "resume_number_2",
  "creme_brulee", "a_ero", "lodz", "nandu", "strasse", "coeur", "ca_va",
  "per_cent_percent", "nian_ling", "vozrast", "abg", "age_nian_ling", "x_2",
  "x_3", "x_4", "v1", "v1_2", "craigs_age", "quoted", "a_b_2", "mpg_cyl", "x1",
  "co2_level", "p_h", "id", "user_id", "etat_3", "if", "true", "function",
  "private", "hidden", "ca_va_2", "ecole_normale", "abc", "hindi_hindi", "ete",
  "a_b_3", "a_b_4", "a_b_5", "x_1", "null", "etat_civil", "brasov", "tari")

## position in names_input = what illumex gives there instead
names_departures <- c(
  "10" = "oeuvre",                                   # Oeuvre ligature
  "30" = "aero",                                     # AE ligature
  "37" = "\u5e74\u9f62",                             # kept, not nian_ling
  "38" = "\u0432\u043e\u0437\u0440\u0430\u0441\u0442", # kept, lower-cased
  "39" = "\u03b1\u03b2\u03b3",                       # kept, not abg
  "40" = "age_\u5e74\u9f62",
  "64" = "hindi_\u0939\u093f\u0928\u094d\u0926\u0940") # its marks kept too

test_that("names follow janitor's, departing only where Craig ruled", {
  expect_length(names_janitor, length(names_input))
  want <- names_janitor
  want[as.integer(names(names_departures))] <- unname(names_departures)
  got <- ilm_snake(names_input)
  expect_identical(got, want)
  ## every departure is one Craig ruled, and nothing else departs
  expect_identical(which(got != names_janitor),
                   sort(as.integer(names(names_departures))))
})

test_that("repeats are numbered as janitor numbers them", {
  ## janitor 2.2.0's results, measured the same day
  expect_identical(ilm_snake(c("a", "a", "a_2")), c("a", "a_2", "a_2_2"))
  expect_identical(ilm_snake(c("a_2", "a", "a")), c("a_2", "a", "a_2_2"))
  expect_identical(ilm_snake(c("x", "", "")), c("x", "x_2", "x_3"))
  expect_identical(ilm_snake(c("b", "B", "b_2", "b")), c("b", "b_2", "b_2_2", "b_3"))
  expect_identical(ilm_snake(c("v", "v_1", "v")), c("v", "v_1", "v_2"))
})

test_that("no letter is dropped: every letter in a name survives in some form", {
  ## what the old rule lost: accented letters, and "%" and "#" with them
  got <- ilm_snake(c("P\u00e9riode", "\u00c9tat", "\u00e9tat", "% change", "change"))
  expect_identical(got, c("periode", "etat", "etat_2", "percent_change", "change"))
  expect_false(anyDuplicated(got) > 0)
  ## a Latin letter outside the table is kept as it is, not dropped
  expect_identical(ilm_snake("\u01ceb"), "\u01ceb")
})

test_that("ilm_wash_df() gives these names", {
  d <- data.frame(1, 2, 3, 4, 5, 6)
  names(d) <- c("# of cylinders", "% change", "P\u00e9riode", "\u00c2ge moyen",
                "\u00c9tat", "\u00e9tat")
  expect_identical(names(ilm_wash_df(d)),
                   c("number_of_cylinders", "percent_change", "periode",
                     "age_moyen", "etat", "etat_2"))
})
