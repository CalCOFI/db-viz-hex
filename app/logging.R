# Query logging → Google Sheet -------------------------------------------------
#
# WHY A SHEET (not Google Analytics): GA4 is aggregate behavioral analytics — it
# anonymizes IP, caps custom-event params, and is not meant to hold arbitrary
# query text or a per-query log. A Google Sheet is directly viewable/filterable/
# chartable by non-technical users and holds a full row per query (timestamp, IP,
# app event, filter params, response time, row count, error). We keep GA
# (app/google-analytics.html) for aggregate page/feature usage and add this Sheet
# log for the detailed per-query record.
#
# HOW IT WORKS: a Google Apps Script "web app" bound to the Sheet exposes a POST
# endpoint that appends a row. The Shiny server POSTs a JSON row to it — no OAuth
# or service account in the app, and logging is fire-and-forget so it can never
# slow or break a user query.
#
# SETUP (one-time, by a maintainer):
#   1. Create a Google Sheet with a header row:
#        timestamp | ip | session | event | params | n_rows | ms | status | error | app_version
#   2. Extensions → Apps Script, paste doPost() below, Deploy → New deployment →
#      type "Web app", execute as "Me", access "Anyone". Copy the /exec URL.
#   3. Set env var CALCOFI_LOG_URL to that URL for the app (Renviron / shinyapps
#      env / systemd). If unset, logging is a silent no-op.
#
#   // ---- Apps Script (Code.gs) ----
#   // function doPost(e) {
#   //   var sh = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
#   //   var d  = JSON.parse(e.postData.contents);
#   //   sh.appendRow([new Date(), d.ip, d.session, d.event, d.params,
#   //                 d.n_rows, d.ms, d.status, d.error, d.app_version]);
#   //   return ContentService.createTextOutput(JSON.stringify({ok:true}))
#   //            .setMimeType(ContentService.MimeType.JSON);
#   // }

librarian::shelf(httr2, jsonlite, quiet = TRUE)

CALCOFI_LOG_URL <- Sys.getenv("CALCOFI_LOG_URL", "")
APP_VERSION     <- tryCatch(readLines("VERSION", warn = FALSE)[1], error = function(e) NA_character_)

# best-effort client IP: X-Forwarded-For (behind the shinyapps/nginx proxy) then
# the direct REMOTE_ADDR. Never errors.
client_ip <- function(session) {
  tryCatch({
    req <- session$request
    xff <- req[["HTTP_X_FORWARDED_FOR"]]
    if (!is.null(xff) && nzchar(xff)) trimws(strsplit(xff, ",")[[1]][1])
    else req[["REMOTE_ADDR"]] %||% NA_character_
  }, error = function(e) NA_character_)
}

# append one row to the log Sheet. Fire-and-forget: a logging failure (bad URL,
# no network) is swallowed so it never affects the user's query.
log_query <- function(session, event, params = list(), n_rows = NA,
                      ms = NA, status = "ok", error = NULL) {
  if (!nzchar(CALCOFI_LOG_URL)) return(invisible(FALSE))
  row <- list(
    ip          = client_ip(session),
    session     = tryCatch(session$token, error = function(e) NA_character_),
    event       = event,
    params      = jsonlite::toJSON(params, auto_unbox = TRUE, null = "null"),
    n_rows      = if (is.na(n_rows)) NULL else as.integer(n_rows),
    ms          = if (is.na(ms)) NULL else round(as.numeric(ms), 1),
    status      = status,
    error       = error %||% "",
    app_version = APP_VERSION)
  tryCatch(
    httr2::request(CALCOFI_LOG_URL) |>
      httr2::req_body_json(row) |>
      httr2::req_timeout(4) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL)
  invisible(TRUE)
}

# wrap a query expression: time it, capture row count + errors, log, re-raise.
# Usage: with_query_log(session, "map:get_sp", list(taxa = sp, qtr = qtr), {
#          get_sp(sp, qtr, date_range) })
with_query_log <- function(session, event, params, expr) {
  t0  <- Sys.time()
  res <- tryCatch(force(expr), error = function(e) e)
  ms  <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
  if (inherits(res, "error")) {
    log_query(session, event, params, ms = ms, status = "error",
              error = conditionMessage(res))
    stop(res)
  }
  n <- tryCatch(
    if (is.data.frame(res)) nrow(res)
    else if (inherits(res, "tbl_lazy")) NA_integer_
    else NA_integer_, error = function(e) NA_integer_)
  log_query(session, event, params, n_rows = n, ms = ms, status = "ok")
  res
}
