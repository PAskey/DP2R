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


DP2R <- function(Tables = c("vwIndividualFish", "vwFishCollection", "vwCollectCount","vwWaterbodyLake"),
                 exclude_types = c("geography", "varbinary"),
                 envir = parent.frame()) {


  if(!exists("conn")){stop("First you must establish a connection to DataPond or DataPond_STAGE in R and assign to 'conn' object. Use example code with your personal uid and pwd. Note this often fails on first attempt (some sort of timeout lag with Azure, but then works when you re-run the code
  conn <- DBI::dbConnect(drv = odbc::odbc(),
                         Driver = 'SQL Server'',
                         server = 'tcp:gofishbc.database.windows.net,1433',
                         database = 'DataPond',
                         uid = uid,
                         pwd = pwd")}


  stopifnot(is.null(envir) || inherits(envir, "environment"))

  fetch_data_excluding_types <- function(conn, table_name, exclude_types) {
    # Get column information
    col_info <- DBI::dbGetQuery(conn, paste0("SELECT COLUMN_NAME, DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '", table_name, "'"))

    # Filter out columns of specified data types
    cols_to_include <- col_info$COLUMN_NAME[!col_info$DATA_TYPE %in% exclude_types]

    # Construct the SQL query
    cols_string <- paste(cols_to_include, collapse = ", ")
    query <- paste0("SELECT ", cols_string, " FROM ", table_name)

    # Execute the query and return the results
    DBI::dbGetQuery(conn, query)
  }

  # Load each table excluding specified data types
  out <- lapply(stats::setNames(nm = Tables), function(tb) {
    ret <- tryCatch(
      fetch_data_excluding_types(conn, tb, exclude_types),
      error = function(e) conditionMessage(e))

    if (!is.null(envir)) assign(tb, ret, envir = envir)
    ret
  })

  invisible(out)
}

