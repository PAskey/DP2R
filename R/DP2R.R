#' A function to bring data into R from DataPond for analysis.
#' It is important that you establish a personal password protected connection (and assign as 'conn' object before running this function. Use example code below and/or you may have it set up under Connections tab in RStudio after running it once.See: https://github.com/r-dbi/odbc/blob/main/README.md
#' Function only usable by FFSBC staff who have a direct or vpn connection to SLD.Replaces old 'SLD2R()' function for the old database.
#'
#'
#' @title DP2R
#' @name DP2R
#' @keywords SPDT; DataPond
#' @export
#' @param DB A text string of "DataPond" (default production data) or "DataPond_STAGE" which is staged data to be reviewed before acceptance into production.
#' @param Tables A text vector of DataPond tables names to be pulled into the R environment. If unsure of the names of all tables, establish a DataPond connection (conn) and then run: 'DBI::dbListTables(conn)' or review connections tab in RStudio. The default tables are the 'views' (preceded by vw), which would typically be used in analyses of gillnet assessment data.
#' @param exclude_types A text string or vector describing data types to omit from the data set. The default is "geography" because R does not handle that datatype. That means any columns with point data in the geography datatype are removed from the dataset (there is still lat-long data that can be used).
#' @param envir Environment to assign results into (default: .GlobalEnv).
#' @param as.raw Logical; if TRUE, keep data in raw DB format (only excluded-type filtering is applied; no renaming/coalescing/aggregation). Default: FALSE.
#' @param where Optional named character vector (or list) of extra SQL conditions, one per table, applied in the
#'   database before download so fewer rows cross the network. Names are table names; values are SQL using the
#'   table's RAW column names (e.g. \code{waterbody_identifier}, not \code{WBID}). Example:
#'   \code{where = c(vwRelease = "waterbody_identifier NOT LIKE '00000\%'")}. Default NULL (no extra filters).
#' @examples
#'   conn <- DBI::dbConnect(drv = odbc::odbc(),
#'   Driver = 'ODBC Driver 18 for SQL Server',
#'   server = 'tcp:gofishbc.database.windows.net,1433',
#'   database = 'DataPond',
#'   uid = uid,
#'   pwd = pwd)
#'
#' DP2R()
#'
#' @importFrom magrittr "%>%"


DP2R <- function(Tables = c("vwRelease","vwIndividualFish", "vwCollectCount","vwFishCollection","vwWaterbody","Species", "SampleDesign_MeshSizeCode"),
                 exclude_types = c("geography", "varbinary", "ntext"),
                 envir = .GlobalEnv,
                 as.raw = FALSE,
                 where = NULL) {


  if(!exists("conn")|!DBI::dbIsValid(conn)){stop("First you must establish a connection to DataPond or DataPond_STAGE in R and assign to 'conn' object. Use example code with your personal uid and pwd. Note this often fails on first attempt (some sort of timeout lag with Azure, but then works when you re-run the code
  conn <- DBI::dbConnect(drv = odbc::odbc(),
                         Driver = 'ODBC Driver 18 for SQL Server'',
                         server = 'tcp:gofishbc.database.windows.net,1433',
                         database = 'DataPond',
                         uid = uid,
                         pwd = pwd")}

  #Once per session: tiny DataPond queries to remind if stored package data are out of date
  #(see DP2Rdata_refresh()). Silent on any problem; turn off with options(DP2R.refresh_check = FALSE)
  refresh_reminder(conn)


  #vwPaAssessEvent used to be added automatically whenever vwFishCollection, vwCollectCount or
  #vwIndividualFish was requested, but nothing uses it: assess_event_name is a column of those
  #views. Request it in Tables if you need it (it is still in the default Tables list).

  if (!is.null(where) && (is.null(names(where)) || any(!nzchar(names(where))))) {
    stop("`where` must be named by table, e.g. where = c(vwRelease = \"...\").")
  }

  # Identify invalid tables or spelling mistakes
  invalid_tables <- Tables[!Tables %in% DBI::dbListTables(conn)]

  # Stop if any tables are invalid
  if (length(invalid_tables) > 0) {
    stop(paste("The following tables do not exist in the database:",
               paste(invalid_tables, collapse = ", ")))
  }


  # Optional SQL filters applied automatically when specific columns exist
  build_where_clause <- function(col_names, table_name) {

    conditions <- character(0)

    # Caller-supplied filter for this table (see `where` argument)
    if (!is.null(where) && table_name %in% names(where)) {
      conditions <- c(conditions, paste0("(", where[[table_name]], ")"))
    }

    # Remove TL records from any table containing this field
    if ("water_analysis_type" %in% col_names) {
      conditions <- c(
        conditions,
        "water_analysis_type <> 'TL'"
      )
    }

    # Remove records identified by EffortClean

    if ("fishing_effort_id" %in% col_names) {

      data("remove_E_ids", package = "DP2R")

      remove_E_ids <- unique(na.omit(remove_E_ids))

      if (length(remove_E_ids) > 0) {

        id_list <- paste(remove_E_ids, collapse = ", ")

        conditions <- c(
          conditions,
          sprintf("fishing_effort_id NOT IN (%s)", id_list)
        )
      }
    }


    if (length(conditions) == 0) {
      return(NULL)
    }

    paste(conditions, collapse = " AND ")
  }



  # FIXED helper: schema-qualified, case-insensitive, excludes nvarchar(max) + geography, etc.
  #SUes where clause built above to efficiently filter out cases not needed across network
  fetch_data_excluding_types <- function(conn, table_name,
                                         exclude_types = c("geography","varbinary","ntext")) {

    col_info <- DBI::dbGetQuery(conn, sprintf("
    SELECT COLUMN_NAME AS name,
           DATA_TYPE   AS type_name,
           CHARACTER_MAXIMUM_LENGTH AS column_size
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = '%s'
    ORDER BY ORDINAL_POSITION", table_name))

    tp  <- tolower(col_info$type_name)
    len <- col_info$column_size

    # Keep columns not in excluded types
    not_excluded_type <- !tp %in% tolower(exclude_types)

    # Drop varchar(max), nvarchar(max), varbinary(max)
    is_max_strbin <- tp %in% c("varchar","nvarchar","varbinary") &
      !is.na(len) &
      len == -1

    keep <- not_excluded_type & !is_max_strbin

    cols_to_include <- col_info$name[keep]

    if (!length(cols_to_include)) {
      stop("After exclusions, no columns remain to select.")
    }

    select_list <- paste(
      DBI::dbQuoteIdentifier(conn, cols_to_include),
      collapse = ", "
    )

    from_id <- DBI::dbQuoteIdentifier(
      conn,
      DBI::Id(schema = "dbo", table = table_name)
    )

    # Build any automatic filters based on available columns
    where_clause <- build_where_clause(col_info$name, table_name)

    sql <- sprintf(
      "SELECT %s FROM %s",
      select_list,
      from_id
    )

    if (is.null(where_clause)) {
      return(DBI::dbGetQuery(conn, sql))
    }

    tryCatch(
      DBI::dbGetQuery(conn, paste(sql, "WHERE", where_clause)),
      error = function(e) {
        # A caller-supplied `where` filter must never break a pull: warn and retry without it
        # (the automatic filters above are kept).
        if (is.null(where) || !table_name %in% names(where)) stop(e)
        warning("DP2R(): `where` filter for ", table_name, " failed (", conditionMessage(e),
                "); pulling ", table_name, " without it.", call. = FALSE)
        where_auto <- build_where_clause(col_info$name, "")
        DBI::dbGetQuery(conn, if (is.null(where_auto)) sql else paste(sql, "WHERE", where_auto))
      })
  }


  #Rename some columns to shorter, standard descriptors if present in table
  rename_columns_if_present <- function(data) {
    # Recode fish_collection_type before combining
    if ("fish_collection_type" %in% names(data)) {
      data <- data %>%
        dplyr::mutate(
          fish_collection_type = dplyr::case_when(
            fish_collection_type %in% c("FK", "TN") ~ "LT",
            TRUE ~ fish_collection_type
          )
        )
    }

    # If any sample_type cols exist, coalesce them into `method`
    candidates <- intersect(
      c("survey_type", "fish_collection_type"),
      names(data)
    )

    if (length(candidates) > 0 && !"method" %in% names(data)) {
      data <- data %>%
        dplyr::mutate(method = dplyr::coalesce(!!!rlang::syms(candidates)))%>%
        dplyr::select(-tidyselect::any_of(candidates))
    }

   #Need to separate out "strain" which is in request view so as to not make a duplicate in block below
     if("strain" %in% names(data)) {
      data <- data %>%
        dplyr::rename(strain_name = strain)
    }


    data<-data %>%
      dplyr::rename_with(~ dplyr::case_when(
      #. == "region_code" ~ "region_code", #keep as region_code code, because some views use "region_code" for the region_name
      . == "waterbody_identifier" ~ "WBID",
      . == "fishing_effort_type" ~ "method",
      . == "assess_year" ~ "year",#In Effort table
      . == "date_assessed_string" ~ "year",#In IndividualFish table
      . == "sex_code" ~ "sex",
      . == "maturity_code" ~ "maturity",
      . == "strain_species_code" ~ "strain",
      . == "strain_code" ~ "strain",
      . == "ploidy_code" ~ "ploidy",
      . == "surface_area_ha" ~ "area_ha",
      . == "accepted_age" ~ "age",
      . == "accepted_brood_year" ~ "brood_year",
      . == "ave_size_g" ~ "size_g",#vwRelease (was size_g in vwLegacyRelease)
      TRUE ~ .
    ))

    # Convert "NA" strings in character columns and NaN in numeric columns to NA
    data <- data %>%
      dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(.x, "NA"))) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ replace(.x, is.nan(.x), NA)))

    data
  }


  # Remove rows with NA in WBID. There are currently 39 records.
  remove_na_wbid <- function(data) {
    if (!is.null(data) && "WBID" %in% names(data)) {
      data <- data %>% dplyr::filter(!is.na(WBID))
    }
    data
  }

   #Function to aggregate vwWaterbody when multi records per lake and rename if alias is used.
   #Surface area is never averaged: it is taken from the active record (most recent row if
   #several are active), so release densities use one current, active area per lake.
  aggregate_vwWaterbody <- function(data) {
    area <- NULL
    if ("area_ha" %in% names(data)) {
      area <- data %>%
        dplyr::filter(!is.na(.data$area_ha)) %>%
        dplyr::mutate(.row = dplyr::row_number(),
                      .act = if ("active" %in% names(data)) .data$active %in% c(TRUE, "TRUE", 1) else TRUE) %>%
        dplyr::group_by(.data$WBID) %>%
        dplyr::arrange(dplyr::desc(.data$.act), dplyr::desc(.data$.row), .by_group = TRUE) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select("WBID", area_ha_active = "area_ha")
    }
    out <- data %>%
      dplyr::group_by(WBID) %>%
      dplyr::summarise(across(everything(), mean_or_concat), .groups = 'drop')
    if (!is.null(area)) {
      out <- out %>%
        dplyr::left_join(area, by = "WBID") %>%
        dplyr::mutate(area_ha = .data$area_ha_active) %>%
        dplyr::select(-"area_ha_active")
    }
    out
 }

  # Function to determine mean for numeric and concatenate for character
  mean_or_concat <- function(x) {
    if (is.numeric(x)) {
      if (all(is.na(x))) return(NA_real_)
      return(mean(x, na.rm = TRUE))
    } else {
      vals <- unique(x[!is.na(x)])

      if (length(vals) == 0) {
        return(NA_character_)
      } else {
        return(toString(vals))
      }
    }
  }


  # Load each table excluding specified data types
  out <- lapply(stats::setNames(nm = Tables), function(tb) {
    ret <- tryCatch(
      fetch_data_excluding_types(conn, tb, exclude_types),
      error = function(e) conditionMessage(e))

    # Apply renaming if the data is valid (not NULL or error message)
    if (!is.null(ret) && is.data.frame(ret) && !as.raw) {
      ret <- rename_columns_if_present(ret)
      ret <- remove_na_wbid(ret)  # Remove rows with NA in WBID
      # Fill a blank accepted brood year from the lab (aged) or submitted brood year,
      # so ages loaded but never confirmed as accepted_brood_year are not lost.
      if (tb == "vwIndividualFish" && "brood_year" %in% names(ret)) {
        aged <- if ("aged_brood_year" %in% names(ret)) ret$aged_brood_year else NA
        subm <- if ("submitted_brood_year" %in% names(ret)) ret$submitted_brood_year else NA
        ret <- ret %>%
          dplyr::mutate(
            brood_year = dplyr::coalesce(as.numeric(brood_year), as.numeric(aged), as.numeric(subm)))
      }
      # If the current table is vwWaterbody, apply aggregation
      if (tb == "vwWaterbody") {
        ret <- aggregate_vwWaterbody(ret)

      }
      # ---------------------------------------------------------
      # Add Season and Sample_event for specific assessment tables
      # ---------------------------------------------------------
      if (tb %in% c("vwFishCollection", "vwCollectCount", "vwIndividualFish")) {

        if (tb %in% c("vwFishCollection", "vwCollectCount") && "end_dt" %in% names(ret)) {
          ret <- ret %>% dplyr::filter(!is.na(end_dt))
          date_vec <- as.POSIXct(ret$end_dt, tz = "UTC")

        } else if (tb == "vwIndividualFish" && "date_assessed" %in% names(ret)) {
          ret <- ret %>% dplyr::filter(!is.na(date_assessed))
          date_vec <- as.POSIXct(ret$date_assessed, tz = "UTC")

        } else {
          date_vec <- NULL
        }

        if (!is.null(date_vec)) {

          #ret$Season <- metR::season(date_vec)

          ret$Season <- Season_cat(date_vec)#A season category that extends winter into March
          ret$year <- lubridate::year(date_vec)

          ret$Sample_event <- stringr::str_c(
            stringr::str_trim(ret$method),
            stringr::str_trim(ret$WBID),
            ret$year,
            ret$Season,
            sep = "_"
          )

          # Remove bad sample events
          #Group of 3g fry lakes where clips were bad or misread
          Bad_data <- c(
            "GN_00607THOM_2018_FAL",
            "GN_00719THOM_2018_FAL",
            "GN_00072STHM_2018_FAL",
            "GN_00072STHM_2019_FAL"
          )

          ret <- ret %>%
            dplyr::filter(!(Sample_event %in% Bad_data))

          # Fill a blank accepted age from the (filled) brood year, so fish with
          # only an aged/submitted brood year also carry an age before linkClips()
          # runs its stocked-age test. Done here because `year` is set above.
          if (tb == "vwIndividualFish" && all(c("age", "brood_year") %in% names(ret))) {
            ret <- ret %>%
              dplyr::mutate(
                .age_by = suppressWarnings(as.numeric(
                  DP2R::sby2age(species_code, as.numeric(brood_year), year))),
                age = dplyr::coalesce(as.numeric(age), .age_by)) %>%
              dplyr::select(-.age_by)
          }


        }
}}

    # Assign the result to the specified environment if provided
    if (!is.null(envir)) assign(tb, ret, envir = envir)
    ret
  })

  invisible(out)
}

