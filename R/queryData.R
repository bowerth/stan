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
#' @param dim.ind name of column containing the industry dimension, if different from \sQuote{ind}.
#' @param isic an integer specifying the ISIC classification of data sources.
#' @param nameind a character vector of industries read from column \sQuote{ind} or from \sQuote{dim.ind} if specified.
#' @param nameyear and integer vector specifying the selected period.
#' @param add.where additional statements passed to the \sQuote{WHERE} condition of the query.
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
                       namecou=STAN.COU,
                       namevar=character(),
                       dim.ind='ind',
                       isic=4,
                       nameind=character(),
                       nameyear=character(),
                       add.where="")
{
    require(RODBC)
    if (isic==3 & length(nameind)==0)
    {
        nameind=STANi3.INDALL
    } else if (isic==4 & length(nameind)==0)
    {
        nameind=STAN.INDALL
    }
    ind.str <- paste0(' AND ', dim.ind, ' IN (', toString(paste0("'", nameind, "'")), ')')
    if (length(namevar)==0)
    {
        var.str <- ""
    } else {
        var.str <- paste0(' AND var IN (', toString(paste0("'", namevar, "'")), ')')
    }
    if (length(nameyear)==0)
    {
        year.str <- ""
    } else {
        year.str <- paste0(' AND year BETWEEN ', min(nameyear), ' AND ', max(nameyear))
    }
    data <- sqlQuery(connection,
                     paste0('SELECT * FROM ',table,
                            ' WHERE cou IN (', toString(paste0("'", namecou, "'")), ')',
                            ind.str,
                            var.str,
                            year.str,
                            add.where)
                     )
    return(data)
}
