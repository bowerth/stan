#' FAME load
#'
#' Execute program in FAME
#'
#' Start FAME and execute created code file the output is written to cou.dat file in the respective countrys STD-SNA folder.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param shell a character string pointing to the system command shell.
#' @param path.fame character string pointing to the location of FAME.exe.
#' @param isic an integer to specify the version of STAN.
#' @param subfolder character string pointing to the location of the folder conatining GESMES data and FAME program file.
#'
#' @author OECD STAN
#' @keywords FAME
#' @seealso \code{\link{FAMEall}}, CMD
#' @export
#' @examples
#' FAMEload(cou="ITA")

FAMEload <- function(cou=stop("'cou' must be specified"),
                     shell="C:\\WINDOWS\\system32\\cmd.exe",
                     isic=4,
                     subfolder="\\Rawdata\\STD-SNA",
                     path.fame="\\\\asap1\\fame\\fame")
{
    if (isic==3) path.cou="I:\\STAN07\\COU\\"
    if (isic==4) path.cou="I:\\STANi4\\COU\\"
    input.file <- paste0(path.cou, cou, subfolder, '\\', cou, "_R.inp")
    shell(shQuote(paste0(path.fame, "\\fame i ", input.file)), shell = shell)
}
