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
#' @examples
#'   conn <- DBI::dbConnect(drv = odbc::odbc(),
#'   Driver = 'SQL Server',
#'   server = 'tcp:gofishbc.database.windows.net,1433',
#'   database = 'DataPond',
#'   uid = uid,
#'   pwd = pwd)
#'
#' DP2R()
#'
#' @importFrom magrittr "%>%"


DP2R <- function(Tables = c("vwIndividualFish", "vwCollectCount","vwFishCollection","vwWaterbodyLake","Species"),
                 exclude_types = c("geography", "varbinary", "ntext"),
                 envir = .GlobalEnv,
                 as.raw = FALSE) {


  if(!exists("conn")|!DBI::dbIsValid(conn)){stop("First you must establish a connection to DataPond or DataPond_STAGE in R and assign to 'conn' object. Use example code with your personal uid and pwd. Note this often fails on first attempt (some sort of timeout lag with Azure, but then works when you re-run the code
  conn <- DBI::dbConnect(drv = odbc::odbc(),
                         Driver = 'SQL Server'',
                         server = 'tcp:gofishbc.database.windows.net,1433',
                         database = 'DataPond',
                         uid = uid,
                         pwd = pwd")}


  #stopifnot(is.null(envir) || inherits(envir, "environment"))

  # Manually fetch the 'Releases' table. This works
  #releases_data <- DBI::dbGetQuery(conn, "SELECT * FROM paris.Releases")

  # Use the retrieved data instead of relying on DP2R
  #print(head(releases_data))


  # Identify invalid tables or spelling mistakes
  invalid_tables <- Tables[!Tables %in% DBI::dbListTables(conn)]

  # Stop if any tables are invalid
  if (length(invalid_tables) > 0) {
    stop(paste("The following tables do not exist in the database:",
               paste(invalid_tables, collapse = ", ")))
  }

  # FIXED helper: schema-qualified, case-insensitive, excludes nvarchar(max) + geography, etc.
  fetch_data_excluding_types <- function(conn, table_name,
                                         exclude_types = c("geography","varbinary","ntext")) {
    # 1) include CHARACTER_MAXIMUM_LENGTH in the metadata
    col_info <- DBI::dbGetQuery(conn, sprintf("
    SELECT COLUMN_NAME AS name,
           DATA_TYPE   AS type_name,
           CHARACTER_MAXIMUM_LENGTH AS column_size
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_SCHEMA = 'dbo' AND TABLE_NAME = '%s'
    ORDER BY ORDINAL_POSITION", table_name))

    tp  <- tolower(col_info$type_name)
    len <- col_info$column_size

    # 2) keep columns NOT in excluded types (case-insensitive)
    not_excluded_type <- !tp %in% tolower(exclude_types)

    # 3) drop only (MAX) for varchar/nvarchar/varbinary (length == -1)
    is_max_strbin <- tp %in% c("varchar","nvarchar","varbinary") & !is.na(len) & len == -1

    keep <- not_excluded_type & !is_max_strbin

    cols_to_include <- col_info$name[keep]
    if (!length(cols_to_include)) stop("After exclusions, no columns remain to select.")

    select_list <- paste(DBI::dbQuoteIdentifier(conn, cols_to_include), collapse = ", ")
    from_id     <- DBI::dbQuoteIdentifier(conn, DBI::Id(schema = "dbo", table = table_name))
    sql         <- sprintf("SELECT %s FROM %s", select_list, from_id)

    DBI::dbGetQuery(conn, sql)
  }


  #Rename some columns to shorter, standard descriptors if present in table
  rename_columns_if_present <- function(data) {
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


    data<-data %>%
      dplyr::rename_with(~ dplyr::case_when(
      . == "region_code" ~ "region",
      . == "waterbody_identifier" ~ "WBID",
      . == "fishing_effort_type" ~ "method",
      . == "assess_year" ~ "year",#In Effort table
      . == "date_assessed_string" ~ "year",#In IndividualFish table
      . == "sex_code" ~ "sex",
      . == "maturity_code" ~ "maturity",
      . == "strain_species_code" ~ "strain",
      . == "ploidy_code" ~ "ploidy",
      . == "surface_area_ha" ~ "area_ha",
      . == "accepted_age" ~ "age",
      TRUE ~ .
    ))
    data
  }


  # Remove rows with NA in WBID. There are currently 39 records.
  remove_na_wbid <- function(data) {
    if (!is.null(data) && "WBID" %in% names(data)) {
      data <- data %>% dplyr::filter(!is.na(WBID))
    }
    data
  }

   #Function to aggregate vwWaterbodyLake when multi records per lake and rename if alias is used
  aggregate_vwWaterbodyLake <- function(data) {
    data %>%
      dplyr::group_by(WBID) %>%
      dplyr::summarise(across(everything(), mean_or_concat), .groups = 'drop')
 }

  # Function to determine mean for numeric and concatenate for character
  mean_or_concat <- function(x) {
    if (is.numeric(x)) {
      return(mean(x, na.rm = TRUE))  # Calculate mean to one decimale place for numeric columns
    } else {
      return(toString(unique(x)))  # Concatenate unique values for character columns
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
      # If the current table is vwWaterbodyLake, apply aggregation
      if (tb == "vwWaterbodyLake") {
        ret <- aggregate_vwWaterbodyLake(ret)

      }
    }

    # Assign the result to the specified environment if provided
    if (!is.null(envir)) assign(tb, ret, envir = envir)
    ret
  })

  invisible(out)
}

