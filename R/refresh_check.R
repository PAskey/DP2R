#Small, fast DataPond change checks shared by DP2R() (reminder) and DP2Rdata_refresh().
#Each query returns one row, so they add almost nothing to a DP2R() call.

.dp2r_state <- new.env(parent = emptyenv())

#SQL for the quick DataPond signatures. One row each.
.refresh_sql <- list(
  effort = paste(
    "SELECT COUNT(*) AS n, MAX(fishing_effort_id) AS max_id,",
    "CONVERT(varchar(30), MAX(assessed_dt), 126) AS max_dt FROM dbo.vwEffort"),
  #BINARY_CHECKSUM(*) skips geography, ntext and other non-comparable columns, so the
  #ntext aliases column gets its own checksum
  waterbody = paste(
    "SELECT COUNT(*) AS n, CHECKSUM_AGG(BINARY_CHECKSUM(*)) AS chk,",
    "CHECKSUM_AGG(CHECKSUM(CAST(aliases AS nvarchar(max)))) AS alias_chk FROM dbo.vwWaterbody")
)

#Current DataPond signatures as a named character vector, e.g.
#c(effort.n = "...", effort.max_id = "...", effort.max_dt = "...", waterbody.n = "...", waterbody.chk = "...")
#Sources that fail are left out (with an attribute naming the failures).
refresh_db_signature <- function(conn, sources = names(.refresh_sql),
                                 .query = function(sql) DBI::dbGetQuery(conn, sql)) {
  out <- character(0)
  failed <- character(0)
  for (s in sources) {
    res <- tryCatch(.query(.refresh_sql[[s]]), error = function(e) NULL)
    if (is.null(res) || !nrow(res)) { failed <- c(failed, s); next }
    v <- vapply(res[1, , drop = FALSE], function(x) as.character(x), character(1))
    names(v) <- paste0(s, ".", names(res))
    out <- c(out, v)
  }
  attr(out, "failed") <- failed
  out
}

#Stored stamp (package data refresh_stamp) as a named character vector
stored_refresh_stamp <- function() {
  #getExportedValue() is what DP2R::refresh_stamp uses; it finds lazy-loaded package data
  st <- tryCatch(getExportedValue("DP2R", "refresh_stamp"), error = function(e) NULL)
  if (is.null(st) || !nrow(st)) return(NULL)
  v <- st$value; names(v) <- st$check
  attr(v, "stamped") <- max(st$stamped, na.rm = TRUE)
  v
}

#Called from DP2R(): once per session, never errors, prints one message if DataPond
#has changed since the stored data were last refreshed.
refresh_reminder <- function(conn, .query = NULL) {
  if (!isTRUE(getOption("DP2R.refresh_check", TRUE))) return(invisible(NULL))
  if (isTRUE(.dp2r_state$checked)) return(invisible(NULL))
  .dp2r_state$checked <- TRUE

  tryCatch({
    stamp <- stored_refresh_stamp()
    if (is.null(stamp)) return(invisible(NULL))
    cur <- if (is.null(.query)) refresh_db_signature(conn) else refresh_db_signature(conn, .query = .query)
    keys <- intersect(names(stamp), names(cur))
    changed <- unique(sub("\\..*$", "", keys[stamp[keys] != cur[keys]]))
    if (!length(changed)) return(invisible(NULL))

    what <- c(effort = "new or changed effort counts (vwEffort)",
              waterbody = "changed lake/access records (vwWaterbody)")[changed]
    maintainer <- file.exists(file.path("data-raw", "Create_Lakes.R"))
    message("DP2R: DataPond has ", paste(what, collapse = " and "),
            " since DP2R's stored data were refreshed (", format(as.Date(attr(stamp, "stamped"))), "). ",
            if (maintainer) "Consider running DP2Rdata_refresh() to see what needs updating."
            else "Stored tables (e.g. Lakes, Effort_all) may be out of date; reinstall the latest DP2R or ask the package maintainer.",
            "\n(Turn this check off with options(DP2R.refresh_check = FALSE).)")
  }, error = function(e) NULL)
  invisible(NULL)
}
