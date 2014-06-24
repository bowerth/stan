#' Bcp read
#'
#' Use bcp to read from SQL
#'
#' Use the bcp utility to read data from a table on Microsoft SQL Server
#'
#' @param table a character string specifying the SQL table in the form [DBname].dbo.[TableName].
#' @param server a character string specifying the SQL server containing the database specified with \sQuote{table}.
#' @param bcpOUT a character string passing additional argumens to the bcp utility for writing data from Microsoft SQL Server.
#'
#' @author OECD STAN
#' @keywords SQL
#' @seealso \code{\link{bcpWrite}}
#' @export
#' @examples
#' bcpRead(table="STAN.dbo.STANPUB", server="VS-GEN-SQL-3")

bcpRead <- function(table=NULL,
                    server=NULL,
                    bcpOUT="-w -T")
{
    file.data <- tempfile(fileext=".txt")
    file.format <- tempfile(fileext=".txt")
    ## create data file
    system(paste("bcp", table, "out", file.data, "-S", server, bcpOUT))
    data <- read.table(file.data, fileEncoding='UCS-2LE', sep='\t', quote="")
    ## create format file to retrieve column names
    system(paste("bcp", table, "format nul -S", server, "-f", file.format, "-w -T"))
    names <- as.vector(t(as.matrix((read.table(file.format, sep='', skip=2)[7]))))
    names(data) <- names
    return(data)
}
