#' Query Data SQL
#'
#' Query data from SQL sources
#'
#' Helper function to efficiently query data from SQL sources.
#'
#' @param connection a connection handle as returned by 'odbcConnect'.
#' @param table an existing table or view in the database specified with \sQuote{connection}.
#' @param namecou a character vector of 3-digit ISO country codes read from column \sQuote{cou}.
#' @param namevar a character vector of variables read from column \sQuote{var}.
#' @param nameeuc a character vector of end-use categories read from column \sQuote{CATEG}.
#' @param dim.ind name of column containing the industry dimension, if different from \sQuote{ind}.
#' @param isic an integer specifying the ISIC classification of data sources.
#' @param nameind a character vector of industries read from column \sQuote{ind} or from \sQuote{dim.ind} if specified.
#' @param nameyear and integer vector specifying the selected period.
#' @param add.where additional statements passed to the \sQuote{WHERE} condition of the query.
#' @param topn extract only top N number of records.
#'
#' @author OECD STAN
#' @keywords SQL
#' @seealso \code{\link{packageData}}, RODBC
#' @export
#' @examples
#' library(RODBC)
#' SQL.STAN <- odbcDriverConnect(connection="SERVER=VS-GEN-SQL-3;DRIVER=SQL Server;DATABASE=STAN",
#'                               readOnlyOptimize = TRUE)
#' DATA.STAN <- queryData(connection=SQL.STAN, table="STANPUB", isic=3)

queryData <- function(connection=stop("'connection' must be specified"),
                      table=stop("'table' must be specified"),
                      namecou=character(),
                      namevar=character(),
                      nameeuc=character(),
                      dim.ind='ind',
                      isic=4,
                      nameind=character(),
                      nameyear=character(),
                      add.where="",
                      topn=character())
{
    require(RODBC)
    if (isic==3 & length(nameind)==0)
    {
        nameind=STANi3.INDALL
    } else if (isic==4 & length(nameind)==0)
    {
        nameind=STANi4.INDALL
    }
    ind.str <- paste0(' WHERE ', dim.ind, ' IN (', toString(paste0("'", nameind, "'")), ')')
    if (length(topn)==0)
    {
        topn.str <- ""
    } else {
        topn.str <- paste0('TOP ', topn)
    }
    if (length(namecou)==0)
    {
        cou.str <- ""
    } else {
        cou.str <- paste0(' AND cou IN (', toString(paste0("'", namecou, "'")), ')')
    }
    if (length(namevar)==0)
    {
        var.str <- ""
    } else {
        var.str <- paste0(' AND var IN (', toString(paste0("'", namevar, "'")), ')')
    }
    if (length(nameeuc)==0)
    {
        euc.str <- ""
    } else {
        euc.str <- paste0(' AND CATEG IN (', toString(paste0("'", nameeuc, "'")), ')')
    }
    if (length(nameyear)==0)
    {
        year.str <- ""
    } else {
        year.str <- paste0(' AND year BETWEEN ', min(nameyear), ' AND ', max(nameyear))
    }
    if (connection=="sqldf")
      {
          data <- sqldf(paste0('SELECT ', topn.str, ' * FROM ',table,
                               ind.str,
                               cou.str,
                               var.str,
                               euc.str,
                               year.str,
                               add.where)
                        )
      } else {
          data <- sqlQuery(connection,
                           paste0('SELECT ', topn.str, ' * FROM ',table,
                                  ind.str,
                                  cou.str,
                                  var.str,
                                  euc.str,
                                  year.str,
                                  add.where)
                           )
      }
    return(data)
}
