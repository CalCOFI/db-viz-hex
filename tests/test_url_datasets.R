#!/usr/bin/env Rscript
# tests/test_url_datasets.R — the ?datasets= rule (app/functions.R's
# parse_datasets_param).
#
#     Rscript tests/test_url_datasets.R
#
# The app needs a current release DuckDB to start, so the rule that decides what
# a link selects is a pure function and is checked here rather than by clicking.
# Two things it must get right, both of which a link outliving a release will
# find: an unknown key is dropped so the app opens instead of erroring, and an
# all-unknown list is NULL rather than character(0), because in this app an empty
# dataset selection already means ALL of them — a stale link must not read as a
# deliberate empty filter.

# source the one function under test, not the whole app (functions.R expects a
# live DB connection at load)
src   <- readLines(file.path("app", "functions.R"))
start <- grep("^parse_datasets_param <- function", src)
stopifnot(length(start) == 1)
eval(parse(text = paste(src[start:length(src)], collapse = "\n")))

KNOWN <- c("calcofi_bottle", "swfsc_ichthyo", "cce-lter_zoodb", "farallon_bird-mammal")
fails <- 0L
ok <- function(label, got, want) {
  good <- identical(got, want)
  if (!good) {
    fails <<- fails + 1L
    cat("FAIL ", label, "\n  got:  ", paste(deparse(got), collapse = ""),
        "\n  want: ", paste(deparse(want), collapse = ""), "\n", sep = "")
  } else cat("ok   ", label, "\n", sep = "")
}

ok("one key",              parse_datasets_param("swfsc_ichthyo", KNOWN), "swfsc_ichthyo")
ok("several, URL order",   parse_datasets_param("swfsc_ichthyo,calcofi_bottle", KNOWN),
                           c("swfsc_ichthyo", "calcofi_bottle"))
ok("whitespace is trimmed", parse_datasets_param(" swfsc_ichthyo , calcofi_bottle ", KNOWN),
                           c("swfsc_ichthyo", "calcofi_bottle"))
ok("a hyphenated key survives", parse_datasets_param("cce-lter_zoodb", KNOWN), "cce-lter_zoodb")
ok("duplicates collapse",  parse_datasets_param("swfsc_ichthyo,swfsc_ichthyo", KNOWN), "swfsc_ichthyo")

# a link that outlives a release
ok("an unknown key is dropped, the known ones kept",
   parse_datasets_param("swfsc_ichthyo,retired_dataset", KNOWN), "swfsc_ichthyo")
ok("ALL unknown is NULL, never character(0)",
   parse_datasets_param("retired_dataset,gone_too", KNOWN), NULL)

# nothing asked for
ok("no parameter",         parse_datasets_param(NULL, KNOWN), NULL)
ok("empty parameter",      parse_datasets_param("", KNOWN), NULL)
ok("commas only",          parse_datasets_param(",,", KNOWN), NULL)

# the exact link CalCOFI.github.io builds from products.yml
ok("the dataset page's own link",
   parse_datasets_param("calcofi_bottle", KNOWN), "calcofi_bottle")

# ?env= : the environmental dataset's leading variable (parse_env_param)
start <- grep("^parse_env_param <- function", src)
stopifnot(length(start) == 1)
eval(parse(text = paste(src[start:length(src)], collapse = "\n")))

ENV_VARS <- data.frame(
  dataset_key      = c("calcofi_bottle", "calcofi_bottle", "calcofi_bottle", "calcofi_dic", "x_uncurated", "x_uncurated"),
  measurement_type = c("salinity", "temperature", "r_temp", "dic", "zeta", "alpha"),
  stringsAsFactors = FALSE)
HEADLINE <- c("temperature", "salinity", "dic")

ok("bottle leads with temperature",   parse_env_param("calcofi_bottle", ENV_VARS, HEADLINE), "temperature")
ok("dic leads with dic",              parse_env_param("calcofi_dic", ENV_VARS, HEADLINE), "dic")
ok("uncurated leads alphabetically",  parse_env_param("x_uncurated", ENV_VARS, HEADLINE), "alpha")
ok("a taxa dataset is not env",       parse_env_param("swfsc_cufes", ENV_VARS, HEADLINE), NULL)
ok("env: no parameter",               parse_env_param(NULL, ENV_VARS, HEADLINE), NULL)
ok("env: empty parameter",            parse_env_param("", ENV_VARS, HEADLINE), NULL)
ok("env: first of a list",            parse_env_param("calcofi_dic,calcofi_bottle", ENV_VARS, HEADLINE), "dic")

cat("\n", if (fails) sprintf("%d FAILURE(S)\n", fails) else "all pass\n", sep = "")
quit(status = if (fails) 1L else 0L)
