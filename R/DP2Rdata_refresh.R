#' Check for and rebuild out-of-date DP2R stored data
#'
#' Maintainer tool for the DP2R source repository. Works out which stored datasets
#' (data/*.rda) are out of date and, with \code{run = TRUE}, rebuilds only those,
#' in dependency order, by running their data-raw scripts. The package is reloaded
#' (\code{pkgload::load_all()}) after each rebuild so the next script sees the new data.
#'
#' A dataset needs a refresh when any of these changed since it was last built:
#' \itemize{
#'   \item its data-raw script;
#'   \item its input files (e.g. \code{data-raw/effort_sources/}, \code{bathymetry_links.csv});
#'   \item the DataPond records it uses: vwEffort (count, highest fishing_effort_id, latest
#'     assessed_dt) or vwWaterbody (count + server-side checksum of all rows);
#'   \item new lakes in vwWaterbody that \code{Elev_ice_estimates} does not have yet;
#'   \item a dataset it depends on is being rebuilt in the same run.
#' }
#' Slow or rarely-changing datasets (Species, Mgt_Region_Unit, FISS_Spp, the gillnet
#' selectivity tables) are never rebuilt automatically. They are listed as "due" once
#' older than a year and are rebuilt only when named in \code{include}.
#'
#' What was built and the inputs it was built from are kept in
#' \code{data-raw/refresh_log.csv}, with a running record in
#' \code{data-raw/refresh_history.csv}. The DataPond signatures are also saved as the
#' package dataset \code{refresh_stamp}, which \code{DP2R()} compares against to remind
#' you (once per session) when DataPond has changed.
#'
#' Needs a DataPond connection assigned to \code{conn} and must be run from the
#' DP2R source folder (the RStudio project).
#'
#' @title DP2Rdata_refresh
#' @name DP2Rdata_refresh
#' @keywords DP2R; maintenance; package data
#' @export
#' @param run Logical. FALSE (default) only reports what is out of date; TRUE rebuilds it.
#' @param include Names of manual datasets to rebuild as well (e.g. "FISS_Spp",
#'   "gillnet_selectivity", "Mgt_Region_Unit", "Species").
#' @param force Names of datasets to rebuild even if nothing changed ("all" for everything).
#'   Datasets that depend on them are rebuilt too.
#' @param baseline Logical. Record the current stored data as up to date without
#'   rebuilding anything. Use once to start the log, or after rebuilding by hand.
#' @param repo Path to the DP2R source folder (default: working directory).
#' @param .query For testing only: a function(sql) returning a data.frame, used instead of
#'   querying \code{conn}.
#' @return Invisibly, a data frame with one row per dataset: step, status, reason.
#' @examples
#' #Must be connected to VPN, with a DataPond connection assigned to conn
#' DP2Rdata_refresh()                      #what is out of date?
#' DP2Rdata_refresh(run = TRUE)            #rebuild it
#' DP2Rdata_refresh(run = TRUE, include = "FISS_Spp")
#' DP2Rdata_refresh(baseline = TRUE)       #first use: mark current data as up to date

DP2Rdata_refresh <- function(run = FALSE, include = NULL, force = NULL, baseline = FALSE,
                             repo = ".", .query = NULL) {

  repo <- normalizePath(repo, winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(repo, "data-raw", "Create_Lakes.R")) ||
      !file.exists(file.path(repo, "DESCRIPTION"))) {
    stop("DP2Rdata_refresh() must be run from the DP2R source folder (open the DP2R RStudio project), ",
         "or point `repo` at it.", call. = FALSE)
  }
  if (is.null(.query)) {
    if (!exists("conn", envir = .GlobalEnv) || !DBI::dbIsValid(get("conn", envir = .GlobalEnv))) {
      stop("First connect to DataPond and assign the connection to `conn` (see ?DP2R).", call. = FALSE)
    }
    conn <- get("conn", envir = .GlobalEnv)
    .query <- function(sql) DBI::dbGetQuery(conn, sql)
  }
  if (run && !requireNamespace("pkgload", quietly = TRUE)) {
    stop("run = TRUE needs the pkgload package: install.packages('pkgload')", call. = FALSE)
  }

  steps <- .refresh_steps()
  step_names <- vapply(steps, `[[`, "", "name")
  names(steps) <- step_names
  bad <- setdiff(c(include, setdiff(force, "all")), step_names)
  if (length(bad)) stop("Unknown dataset(s): ", paste(bad, collapse = ", "),
                        ". Choose from: ", paste(step_names, collapse = ", "), call. = FALSE)
  if (identical(force, "all")) force <- step_names

  log_file  <- file.path(repo, "data-raw", "refresh_log.csv")
  hist_file <- file.path(repo, "data-raw", "refresh_history.csv")
  log <- if (file.exists(log_file)) utils::read.csv(log_file, colClasses = "character") else
    data.frame(step = character(0), built = character(0), signature = character(0))

  #___________________________________________________________________________
  #Current signatures
  message("Checking DataPond and source files...")
  db <- refresh_db_signature(sources = names(.refresh_sql), .query = .query)
  if (length(attr(db, "failed")))
    warning("Could not query DataPond for: ", paste(attr(db, "failed"), collapse = ", "),
            ". Those checks are skipped.", call. = FALSE)
  new_lakes <- .refresh_new_lakes(repo, .query)

  sig_of <- function(st, db, new_lakes) {
    parts <- c(script = .md5_files(file.path(repo, "data-raw", st$script)))
    if (length(st$files)) parts <- c(parts, files = .md5_files(file.path(repo, st$files)))
    for (s in st$db) {
      v <- db[startsWith(names(db), paste0(s, "."))]
      if (length(v)) parts <- c(parts, v)
    }
    if (isTRUE(st$new_lakes) && !is.na(new_lakes)) parts <- c(parts, new_lakes = as.character(new_lakes))
    parts
  }
  parse_sig <- function(x) {
    if (is.na(x) || !nzchar(x)) return(character(0))
    kv <- strsplit(strsplit(x, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
    stats::setNames(vapply(kv, function(p) paste(p[-1], collapse = "="), ""), vapply(kv, `[`, "", 1))
  }
  fmt_sig <- function(p) paste(paste0(names(p), "=", p), collapse = ";")
  labels <- c(script = "build script changed", files = "input files changed",
              effort = "DataPond effort counts changed", waterbody = "DataPond lake/access records changed")

  #___________________________________________________________________________
  #Baseline: record current state as up to date
  if (baseline) {
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log <- do.call(rbind, lapply(steps, function(st) {
      out1 <- file.path(repo, "data", paste0(st$outputs[1], ".rda"))
      built <- if (file.exists(out1)) format(file.mtime(out1), "%Y-%m-%d %H:%M:%S") else NA_character_
      data.frame(step = st$name, built = built, signature = fmt_sig(sig_of(st, db, new_lakes)))
    }))
    utils::write.csv(log, log_file, row.names = FALSE)
    .write_history(hist_file, data.frame(time = now, step = "all", action = "baseline",
                                         reason = "current stored data marked up to date", minutes = 0))
    .write_stamp(repo, db, c("effort", "waterbody"))
    message("Baseline recorded for ", nrow(log), " datasets in data-raw/refresh_log.csv, and refresh_stamp saved.\n",
            "Rebuild the package (Install & Restart) so DP2R() uses the new refresh_stamp.")
    return(invisible(log))
  }
  if (!nrow(log)) {
    message("No refresh log yet (data-raw/refresh_log.csv).\n",
            "Run DP2Rdata_refresh(baseline = TRUE) to mark the current stored data as up to date,\n",
            "or DP2Rdata_refresh(run = TRUE, force = \"all\") to rebuild everything.")
    return(invisible(NULL))
  }

  #___________________________________________________________________________
  #Plan
  plan <- data.frame(step = step_names, auto = vapply(steps, function(s) isTRUE(s$auto), TRUE),
                     status = "up to date", reason = "", stringsAsFactors = FALSE)
  rebuild <- stats::setNames(rep(FALSE, length(steps)), step_names)
  covered <- character(0)

  for (i in seq_along(steps)) {
    st <- steps[[i]]
    reasons <- character(0)
    prev <- log[log$step == st$name, , drop = FALSE]
    cur <- sig_of(st, db, new_lakes)
    old <- parse_sig(if (nrow(prev)) prev$signature[nrow(prev)] else NA)

    if (!nrow(prev)) reasons <- c(reasons, "not in refresh log")
    if (!all(file.exists(file.path(repo, "data", paste0(st$outputs, ".rda")))))
      reasons <- c(reasons, "output file missing")
    for (k in names(cur)) {
      if (k == "new_lakes") {
        if (as.numeric(cur[k]) > 0) reasons <- c(reasons, paste(cur[k], "new lakes in vwWaterbody"))
      } else if (!is.na(old[k]) && old[k] != cur[k]) {
        reasons <- c(reasons, labels[[sub("\\..*$", "", k)]])
      }#a check with no previous value (newly added) is recorded at the next build, not treated as a change
    }
    reasons <- unique(reasons)
    up <- intersect(st$upstream, step_names[rebuild])
    if (length(up)) reasons <- c(reasons, paste0("uses ", paste(up, collapse = ", "), " (being rebuilt)"))
    if (st$name %in% force) reasons <- c("forced", reasons)
    if (st$name %in% include) reasons <- c("included", reasons)

    if (st$name %in% covered) {
      plan$status[i] <- if (run) "rebuilt with shinydata" else "rebuilt with shinydata (above)"
      next
    }
    if (isTRUE(st$auto) || st$name %in% c(include, force)) {
      if (length(reasons)) {
        rebuild[i] <- TRUE
        plan$status[i] <- if (run) "rebuild" else "needs refresh"
        if (length(st$covers)) covered <- c(covered, st$covers)
      }
    } else {
      built <- if (nrow(prev)) as.POSIXct(prev$built[nrow(prev)]) else
        file.mtime(file.path(repo, "data", paste0(st$outputs[1], ".rda")))
      age <- as.numeric(difftime(Sys.time(), built, units = "days"))
      if (!is.na(age) && !is.null(st$max_age) && age > st$max_age) {
        plan$status[i] <- "manual: due"
        reasons <- c(paste0("last built ", format(as.Date(built))), reasons)
      } else {
        plan$status[i] <- "manual"
        if (!is.na(age)) reasons <- c(paste0("last built ", format(as.Date(built))), reasons)
      }
    }
    plan$reason[i] <- paste(reasons, collapse = "; ")
  }

  .print_plan(plan)
  if (!run) {
    if (any(rebuild)) message("\nRun DP2Rdata_refresh(run = TRUE) to rebuild these",
                              if ("shinydata" %in% step_names[rebuild]) " (shinydata refits the effort model, ~20 min)." else ".")
    else message("\nNothing needs refreshing.")
    return(invisible(plan))
  }
  if (!any(rebuild)) {
    message("\nNothing needs refreshing.")
    .write_stamp(repo, db, c("effort", "waterbody"))
    return(invisible(plan))
  }

  #___________________________________________________________________________
  #Run
  old_opt <- options(DP2R.refresh_check = FALSE)
  old_wd <- setwd(repo)
  on.exit({ setwd(old_wd); options(old_opt) }, add = TRUE)

  failed <- character(0)
  for (i in which(rebuild)) {
    st <- steps[[i]]
    up_failed <- intersect(st$upstream, failed)
    if (length(up_failed)) {
      plan$status[i] <- "skipped"; plan$reason[i] <- paste("upstream failed:", paste(up_failed, collapse = ", "))
      failed <- c(failed, st$name, st$covers)
      next
    }
    message("\n=== Rebuilding ", st$name, " (data-raw/", st$script, ") ===")
    before <- .rows_of(repo, st$outputs)
    old_lakes <- if (st$name == "Lakes") .load_rda(repo, "Lakes") else NULL
    t0 <- Sys.time()
    env <- new.env(parent = globalenv())
    if (length(st$vars)) list2env(st$vars, envir = env)
    err <- tryCatch({ source(file.path("data-raw", st$script), local = env); NULL },
                    error = function(e) conditionMessage(e))
    mins <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    if (!is.null(err)) {
      plan$status[i] <- "FAILED"; plan$reason[i] <- err
      failed <- c(failed, st$name, st$covers)
      plan$status[plan$step %in% st$covers] <- "skipped"
      plan$reason[plan$step %in% st$covers] <- paste("upstream failed:", st$name)
      message("FAILED: ", err)
      .write_history(hist_file, data.frame(time = now, step = st$name, action = "failed", reason = err, minutes = mins))
      next
    }

    pkgload::load_all(repo, quiet = TRUE, export_all = FALSE, helpers = FALSE)
    if (isTRUE(st$new_lakes)) new_lakes <- .refresh_new_lakes(repo, .query)
    for (nm in c(st$name, st$covers)) {
      sig <- fmt_sig(sig_of(steps[[nm]], db, new_lakes))
      log <- rbind(log[log$step != nm, , drop = FALSE], data.frame(step = nm, built = now, signature = sig))
    }
    utils::write.csv(log, log_file, row.names = FALSE)

    after <- .rows_of(repo, st$outputs)
    note <- paste0(names(after), ": ", before[names(after)], " -> ", after, " rows", collapse = ", ")
    if (!is.null(old_lakes)) note <- paste0(note, "; ", .lakes_changed(old_lakes, .load_rda(repo, "Lakes")))
    plan$status[i] <- "rebuilt"; plan$reason[i] <- note
    message("Done in ", mins, " min. ", note)
    .write_history(hist_file, data.frame(time = now, step = st$name, action = "built",
                                         reason = paste(plan$reason[i]), minutes = mins))
  }

  #Stamp DataPond signatures for sources whose datasets are now current
  ok <- function(x) !any(x %in% failed) && !any(plan$status[plan$step %in% x] %in% c("needs refresh", "skipped"))
  stamp_sources <- c(if (ok(c("shinydata", "Effort_all"))) "effort",
                     if (ok(c("Lakes", "lake_names"))) "waterbody")
  .write_stamp(repo, db, stamp_sources)
  pkgload::load_all(repo, quiet = TRUE, export_all = FALSE, helpers = FALSE)

  message("\n")
  .print_plan(plan[plan$status %in% c("rebuilt", "FAILED", "skipped", "rebuilt with shinydata"), ])
  built <- plan$step[plan$status %in% c("rebuilt", "rebuilt with shinydata")]
  message("\nNext: Install & Restart (Ctrl+Shift+B) so the installed DP2R has the new data, then commit and push.",
          if (any(c("Effort_all", "shinydata") %in% built)) "\n  - Angler_Effort: re-run data/Create_datafiles.R" else "",
          if (any(c("Effort_all", "shinydata", "Lakes") %in% built)) "\n  - Shiny.lake.report: re-run data/Create_datafiles.R" else "")
  invisible(plan)
}


#Dataset registry, in build order. auto = rebuilt automatically when out of date.
#upstream = datasets it reads (rebuilding those triggers this one).
#covers = datasets the same script also rebuilds.
.refresh_steps <- function() list(
  list(name = "Species", script = "Species_tbl.R", outputs = "Species",
       auto = FALSE, max_age = 365),
  list(name = "Mgt_Region_Unit", script = "Mgt_Region_Unit.R", outputs = "Mgt_Region_Unit",
       auto = FALSE),
  list(name = "Elev_ice_estimates", script = "Create_Elev_ice_estimates.R", outputs = "Elev_ice_estimates",
       auto = TRUE, new_lakes = TRUE),
  list(name = "Lakes", script = "Create_Lakes.R", outputs = "Lakes",
       auto = TRUE, db = "waterbody", files = "data-raw/bathymetry_links.csv",
       upstream = "Elev_ice_estimates"),
  list(name = "lake_names", script = "lake_names.R", outputs = "lake_names",
       auto = TRUE, db = "waterbody"),
  list(name = "Effort_external", script = "Create_Effort_external.R", outputs = "Effort_external",
       auto = TRUE, files = "data-raw/effort_sources", upstream = c("Lakes", "Mgt_Region_Unit")),
  list(name = "shinydata", script = "Create_Shiny_Effort.R", outputs = c("shinydata", "remove_E_ids"),
       auto = TRUE, db = "effort", vars = list(rerun_estimates = TRUE), covers = "Effort_all"),
  list(name = "Effort_all", script = "Create_Shiny_Effort.R", outputs = c("Effort_all", "lakesum"),
       auto = TRUE, upstream = c("shinydata", "Effort_external", "Lakes", "Mgt_Region_Unit"),
       vars = list(rerun_estimates = FALSE)),
  list(name = "FISS_Spp", script = "Create_FISS_Spp.R", outputs = "FISS_Spp",
       auto = FALSE, upstream = c("Lakes", "Species"), max_age = 365),
  list(name = "gillnet_selectivity", script = "update_gillnet_selectivity.R",
       outputs = c("best_models", "approx_select_spp", "sel_lookup", "sel_classes", "sel_lookup_event"),
       auto = FALSE, upstream = "Species", max_age = 365)
)

.refresh_newlake_sql <- paste(
  "SELECT DISTINCT waterbody_identifier AS WBID FROM dbo.vwWaterbody",
  "WHERE waterbody_identifier IS NOT NULL AND (waterbody_type IS NULL OR waterbody_type <> 'RV')")

#Number of current lake WBIDs missing from the stored Elev_ice_estimates (NA if unknown)
.refresh_new_lakes <- function(repo, .query) {
  wb <- tryCatch(.query(.refresh_newlake_sql), error = function(e) NULL)
  ei <- .load_rda(repo, "Elev_ice_estimates")
  if (is.null(wb) || is.null(ei)) return(NA_integer_)
  length(setdiff(as.character(wb$WBID), as.character(ei$WBID)))
}

#One md5 for a set of files/folders (folders: all files inside, by relative name)
.md5_files <- function(paths) {
  f <- unlist(lapply(paths, function(p) {
    if (dir.exists(p)) list.files(p, recursive = TRUE, full.names = TRUE) else p
  }))
  f <- sort(f[file.exists(f)])
  if (!length(f)) return("none")
  h <- unname(tools::md5sum(f))
  if (length(h) == 1) return(h)
  tmp <- tempfile(); on.exit(unlink(tmp))
  writeLines(paste(basename(f), h), tmp)
  unname(tools::md5sum(tmp))
}

.load_rda <- function(repo, name) {
  f <- file.path(repo, "data", paste0(name, ".rda"))
  if (!file.exists(f)) return(NULL)
  e <- new.env()
  tryCatch({ load(f, envir = e); get(name, envir = e) }, error = function(e) NULL)
}

.rows_of <- function(repo, outputs) {
  vapply(stats::setNames(nm = outputs), function(o) {
    x <- .load_rda(repo, o)
    if (is.null(x)) NA_integer_ else as.integer(NROW(x))
  }, integer(1))
}

.lakes_changed <- function(old, new) {
  key <- function(d) {
    d <- d[!duplicated(d$WBID), , drop = FALSE]
    stats::setNames(do.call(paste, c(lapply(d, as.character), sep = "|")), d$WBID)
  }
  ko <- key(old); kn <- key(new)
  both <- intersect(names(ko), names(kn))
  paste0(length(setdiff(names(kn), names(ko))), " lakes added, ",
         length(setdiff(names(ko), names(kn))), " removed, ",
         sum(ko[both] != kn[both]), " changed")
}

.write_history <- function(file, row) {
  utils::write.table(row, file, sep = ",", row.names = FALSE,
                     col.names = !file.exists(file), append = file.exists(file), qmethod = "double")
}

#Save the DataPond signatures for the given sources into data/refresh_stamp.rda
.write_stamp <- function(repo, db, sources) {
  f <- file.path(repo, "data", "refresh_stamp.rda")
  old <- .load_rda(repo, "refresh_stamp")
  keep <- names(db)[sub("\\..*$", "", names(db)) %in% sources]
  if (!length(keep)) return(invisible(NULL))
  new <- data.frame(check = keep, value = unname(db[keep]), stamped = Sys.time(), stringsAsFactors = FALSE)
  if (!is.null(old)) new <- rbind(old[!old$check %in% keep, , drop = FALSE], new)
  refresh_stamp <- new[order(new$check), , drop = FALSE]
  rownames(refresh_stamp) <- NULL
  save(refresh_stamp, file = f, compress = "xz")
  invisible(refresh_stamp)
}

.print_plan <- function(plan) {
  if (!nrow(plan)) return(invisible(NULL))
  w <- max(nchar(plan$step)); ws <- max(nchar(plan$status))
  message(paste0(formatC(plan$step, width = -w), "  ", formatC(plan$status, width = -ws), "  ", plan$reason,
                 collapse = "\n"))
}
