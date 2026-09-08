#!/usr/bin/env Rscript
# tests/test_attribution_fields.R — the pure rules behind the attribution
# fields, and the two counts they feed.
#
#     Rscript tests/test_attribution_fields.R
#
# Every rule here is one a bad `attributions.csv` row would trip, and the CSV
# is edited by hand ("editing this CSV is the entire workflow ... no code
# change needed", global.R). The app needs a live release DuckDB to start, so
# these are checked as pure functions rather than by clicking:
#
#   · a blank cell is NA_character_, and nzchar(NA) is TRUE — a bare
#     nzchar()/paste() filter rendered the literal text "NA" as a provider
#   · a license annotation comes in two written forms and only one used to be
#     stripped, so a curator's note reached users
#   · a dataset_key the curated DATASET_LABELS map has never heard of must
#     degrade to its raw key, never error and never vanish
#   · sp_unit_summary() must return one row per cpue_unit: two callers read
#     nrow() as "how many units are present"

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
})

# source only the functions under test; functions.R as a whole expects a live
# DB connection at load (same approach as tests/test_url_datasets.R)
src <- readLines(file.path("app", "functions.R"))
take <- function(pattern) {
  start <- grep(pattern, src)
  stopifnot(length(start) == 1)
  # to the closing brace in column 1
  ends  <- grep("^}", src)
  end   <- ends[ends > start][1]
  eval(parse(text = paste(src[start:end], collapse = "\n")), envir = globalenv())
}
`%||%` <- function(a, b) if (is.null(a)) b else a
has_val_v <- function(x) !is.na(x) & nzchar(x)
take("^summarize_institutions <- function")
take("^summarize_programs <- function")
take("^strip_license_annotation <- function")
take("^sp_unit_summary <- function")
take("^dataset_full_label <- function")
take("^dataset_short_label <- function")
take("^fmt_cpue_unit <- function")
take("^sp_value_label <- function")
take("^cpue_unit_selector_ui <- function")

fails <- 0L
ok <- function(label, got, want) {
  good <- identical(got, want)
  if (!good) {
    fails <<- fails + 1L
    cat("FAIL ", label, "\n  got:  ", paste(deparse(got), collapse = ""),
        "\n  want: ", paste(deparse(want), collapse = ""), "\n", sep = "")
  } else cat("ok   ", label, "\n", sep = "")
}

# ---- blank cells are absent, never the string "NA" --------------------------
ok("institution: a blank cell contributes nothing",
   summarize_institutions(NA_character_), "")
ok("institution: a blank cell beside a real one is dropped",
   summarize_institutions(c("Farallon Institute (FI)", NA_character_)), "FI")
ok("institution: 'Full Name (ABBR)' collapses to ABBR, de-duplicated",
   summarize_institutions(c("California Department of Fish and Wildlife (CDFW)",
                            "California Department of Fish and Wildlife (CDFW) / UCSD SIO")),
   "CDFW / UCSD SIO")
ok("program: a blank cell contributes nothing",
   summarize_programs(NA_character_), "")
ok("program: a blank cell beside a real one is dropped",
   summarize_programs(c("CalCOFI", NA_character_)), "CalCOFI")
ok("program: duplicates collapse",
   summarize_programs(c("CalCOFI", "CalCOFI")), "CalCOFI")

# ---- license annotations: both written forms, and what must survive ---------
ok("license: parenthesised annotation stripped",
   strip_license_annotation("CC-BY-4.0 (confirmed via calcofi.org/data/data-usage-policy/, 2026-09-07)"),
   "CC-BY-4.0")
ok("license: dash-introduced annotation stripped (farallon_bird-mammal)",
   strip_license_annotation("CC BY 4.0 -- confirmed via the Farallon Institute Data Sharing Agreement, 23 July 2025."),
   "CC BY 4.0")
ok("license: em-dash form stripped",
   strip_license_annotation("CC0-1.0 — confirmed from the EDI landing page, 2026-09-07"),
   "CC0-1.0")
# the four long licenses in the CSV whose own text contains a dash or a
# trailing parenthetical — none of them is an annotation
survives <- c(
  "custom (EDI, non-commercial; requires the acknowledgement text + depositing reprints)",
  "custom (EDI, non-commercial -- scholarly use only, permission required for commercial use)",
  "(none stated beyond a general \"Ocean Informatics - SIO, UCSD\" copyright notice)",
  paste("Scholarly/non-commercial use only, per the database's own Data Use Policy;",
        "written permission from Dr. Mark D. Ohman required for any commercial use."))
ok("license: a real license is never truncated", strip_license_annotation(survives), survives)

# ---- an unregistered dataset_key degrades, never errors ---------------------
# the four-fallback dataset_label() lives in global.R; stub it the way global.R
# builds it for a release whose `dataset` table has no row for this key
DATASET_LABELS <- c(swfsc_ichthyo = "SWFSC: Ichthyoplankton")
dataset_label  <- function(key) {
  lbl <- unname(DATASET_LABELS[key])
  ifelse(is.na(lbl) | lbl == "", key, lbl)
}
# the curated map stays FIRST for the prefixed spots. The release's own
# dataset_name_short does NOT carry the "SWFSC: "/"CalCOFI: " prefix, so
# reading these through dataset_label() instead silently renamed all 15 rows
# of the Data Sources page and the cite modal ("CalCOFI: Bottle" ->
# "Hydrographic Bottle") while fixing the error. Same string as before is the
# requirement; only the fallback changed.
ok("label: a known key keeps its curated prefixed form",
   dataset_full_label("swfsc_ichthyo"), "SWFSC: Ichthyoplankton")
ok("label: a known key loses its institution prefix in the short form",
   dataset_short_label("swfsc_ichthyo"), "Ichthyoplankton")
ok("label: a newly ingested key falls back through dataset_label(), not an error",
   dataset_full_label("newprovider_newdataset"), "newprovider_newdataset")
ok("label: ... and the short form leaves a prefix-less fallback alone",
   dataset_short_label("newprovider_newdataset"), "newprovider_newdataset")
# the release short form is what dataset_label() supplies when the curated map
# has no row -- a new dataset names itself without anyone editing DATASET_LABELS
d_release_named <- "otherprovider_otherdataset"
dataset_label <- function(key) ifelse(key == d_release_named, "Some New Survey", key)
ok("label: an unlisted key takes the release's own short name",
   dataset_full_label(d_release_named), "Some New Survey")

# ---- sp_unit_summary(): one row per cpue_unit ------------------------------
# both flags on ONE unit — prep_db.R's `ELSE COALESCE(mt.units, ...)` fallback
# can produce this, and count(cpue_unit, standardized) used to return two rows
mixed_flag <- tibble(
  std_tally         = c(1, 2, 3, 4),
  cpue_unit         = c("count/10m2", "count/10m2", "count/10m2", "count/100m3"),
  cpue_standardized = c(TRUE, TRUE, FALSE, TRUE))
u <- sp_unit_summary(mixed_flag)
ok("units: one row per cpue_unit, not per (unit, flag) pair",
   nrow(u), 2L)
ok("units: counts are summed across the flag split",
   u$n[u$cpue_unit == "count/10m2"], 3L)
ok("units: 'standardized' claims only when every row in the unit is",
   u$standardized[u$cpue_unit == "count/10m2"], FALSE)

one_unit <- tibble(
  std_tally         = c(1, 2, 3),
  cpue_unit         = rep("count/10m2", 3),
  cpue_standardized = c(TRUE, TRUE, FALSE))
u1 <- sp_unit_summary(one_unit)
ok("units: a single unit with a split flag is still one row", nrow(u1), 1L)
ok("units: ... so the legend does not claim '(mixed units)'",
   sp_value_label(u1), "Avg. count/10m²")
ok("units: ... and the picker offers nothing to choose between",
   cpue_unit_selector_ui(u1, "count/10m2"), NULL)

ok("units: nothing summarizable is zero rows, not an error",
   nrow(sp_unit_summary(tibble(std_tally = NA_real_, cpue_unit = NA_character_,
                               cpue_standardized = NA))), 0L)

cat("\n", if (fails == 0L) "all pass" else paste(fails, "FAILED"), "\n", sep = "")
quit(status = if (fails == 0L) 0L else 1L)
