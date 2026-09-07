# In-app feedback

The **Feedback** dialog ([`functions.R::modal_feedback`](../app/functions.R),
[`app/www/cc-feedback.js`](../app/www/cc-feedback.js)) collects a note, an
optional email and an optional full-view screenshot (grabbed through the
browser's screen-share prompt, with a pen / box / arrow / text annotator). The
browser POSTs it — **no Shiny round-trip** — as one `text/plain` JSON object to a
Google Apps Script `/exec` endpoint:

```
{ app:"db-viz-hex", url, release, viewport, theme, text, email,
  image (data:image/png;base64, optional), website (honeypot) }
```

The script then, in order: saves the PNG to Drive → files a **public GitHub
issue** in `CalCOFI/db-viz-hex` (screenshot committed to `feedback/<id>.png`,
email withheld) → appends a row to the `feedback` tab → emails everyone on the
`recipients` tab with the screenshot inline → sends the submitter a copy. It
answers `{ ok, id, image_url, issue_url, status }`, which the dialog turns into
the "thank you + link to the issue" line.

Until the endpoint is set the dialog still works — it just offers only the
**"Open a GitHub issue instead"** fallback (a prefilled `issues/new` URL).

## `Code.gs` is generated — don't hand-edit

```r
writeLines(
  calcofi4r::cc_feedback_script(repos = c("db-viz-hex" = "CalCOFI/db-viz-hex")),
  "feedback/Code.gs"
)
calcofi4r::cc_feedback_header()   # the `feedback` tab's row 1, in order
```

## One-time setup

1. **Sheet** — one Google Sheet (any name), two tabs:
   - `feedback` — row 1 = `cc_feedback_header()`:
     `ts app url release viewport theme text email image_url issue_url id user_agent status`
   - `recipients` — `A1` = `email`, then one address per row. Editable any time,
     no redeploy.

2. **Apps Script** — in that Sheet, **Extensions → Apps Script**. Replace
   `Code.gs` with [`Code.gs`](Code.gs). **Project Settings → Script properties:**
   - `GITHUB_TOKEN` — a fine-grained PAT on `CalCOFI/db-viz-hex` with
     **Contents: Read and write** + **Issues: Read and write**. Issues are filed
     as this token's account. Omit it and everything else still runs (no issue).
   - `DRIVE_FOLDER_ID` — optional; default makes a "CalCOFI app feedback" folder
     next to the Sheet.

3. **Deploy** — **Deploy → New deployment → Web app**, execute as **Me**, access
   **Anyone**. Authorize the Drive / Gmail / external-request scopes. Copy the
   **`/exec` URL**. Verify: open it — `doGet` returns
   `{"ok":true,"endpoint":"calcofi-feedback","rows":N,"recipients":N,"github":true}`.

4. **Point the app at it** — `.Renviron` does not take on the CalCOFI server, so
   pin the URL in [`app/global.R`](../app/global.R) the same way `CALCOFI_LOG_URL`
   is pinned:

   ```r
   if (!debug && !nzchar(Sys.getenv("CALCOFI_FEEDBACK_URL")))
     Sys.setenv(CALCOFI_FEEDBACK_URL = "https://script.google.com/macros/s/AKfyc…/exec")
   ```

   Then push and reload the app (UI-only change):

   ```
   git -C /share/github/CalCOFI/db-viz-hex pull --ff-only
   touch /share/github/CalCOFI/db-viz-hex/app/restart.txt
   ```

5. **Test** — Feedback → note → Capture screenshot → annotate → Send. Expect the
   thank-you with an issue link, a labelled issue in `CalCOFI/db-viz-hex`, a
   `feedback` row, and mail to the recipients + submitter.

## Deploying a `Code.gs` change

Extensions → Apps Script, paste the new `Code.gs`, then **Deploy → Manage
deployments → (edit) → New version → Deploy**. Editing the existing deployment
keeps the `/exec` URL, so `global.R` needs no change. A *new* deployment mints a
new URL.

## Notes

- `text/plain` body is deliberate — it keeps the POST a CORS "simple request"
  (an `application/json` body triggers a preflight `OPTIONS`, which an Apps
  Script `/exec` does not answer, and every submit would be dropped). Same
  reason as the analytics beacon — see [`analytics/README.md`](../analytics/README.md).
- The client sends **PNG** (`cc_feedback_script` rejects anything else) and
  downscales to ≤ 1440 px wide to stay under the script's 6 MB decoded cap.
- `max_per_hour` (default 20, all apps combined) is a script-side spam cap;
  `website` is a hidden honeypot field.
