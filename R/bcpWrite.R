#' Bcp write
#'
#' Use bcp to write to SQL
#'
#' Use the bcp utility to write data to a table on Microsoft SQL Server
#'
#' @param x a dataframe object with the data to be written to SQL.
#' @param table a character string specifying the SQL table in the form [DBname].dbo.[TableName].
#' @param server a character string specifying the SQL server containing the database specified with \sQuote{table}.
#' @param bcpIN a character string passing additional argumens to the bcp utility for reading data into Microsoft SQL Server.
#'
#' @author OECD STAN
#' @keywords SQL
#' @seealso \code{\link{bcpRead}}
#' @export
#' @examples
#' x <- data.frame("values"=c(1:10))
#' bcpWrite(x=x, table="STAN.dbo.STANPUB", server="VS-GEN-SQL-3")

bcpWrite <- function(x=NULL,
                     table=NULL,
                     server=NULL,
                     bcpIN="-w -T")
{
    delim <- "\\t"
    shell <- "C:\\WINDOWS\\system32\\cmd.exe"
    file.in <- tempfile(fileext = ".txt")
    file.out <- tempfile(fileext = ".txt")
    ##
    write.table(x, file = file.in, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t",  fileEncoding = "utf-8", na = "")
    shell(shQuote(paste(PATH.ICONV, "-f utf-8 -t ucs-2le", file.in, ">", file.out)), shell = shell)
    system(paste("bcp", table, "in", file.out, "-S", server, "-t", delim, bcpIN))
}
