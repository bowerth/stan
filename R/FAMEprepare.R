#' FAME prepare
#'
#' Prepare for FAME
#'
#' Identify latest version of submitted table, copy files and generate program for FAME
#'
#' @param cou a 3-digit country ISO code character string.
#' @param year an integer specifying the year of interest.
#' @param isic an integer to specify the version of STAN.
#' @param classification a character sring matching the industry classification used in the dataset name.
#' @param tables an integer vector specifying the SNA tables to be looked for.
#' @param version optional: the version number of the file to be used.
#'
#' @author OECD STAN
#' @keywords FAME
#' @seealso \code{\link{FAMEall}}
#' @export
#' @examples
#' FAMEprepare(cou="ITA")

FAMEprepare <- function(cou=stop("'cou' must be specified"),
                        year=2012,
                        isic=4,
                        classification="NACE2",
                        tables=c(301,302,303,2000),
                        version='latest')
{
    if (isic==4) path.cou <- PATH.COUi4
    if (isic==3) path.cou <- PATH.COUi3
    path <- paste0(path.cou, cou, '\\', 'Rawdata\\STD-SNA\\')
    if (year==2012)
    {
        path.data <- paste0(PATH.STDANA, paste(cou, year, classification), '\\')
    } else { # other years are in a subfolder
        path.data <- paste0(PATH.STDANA, year, '\\', paste(cou, year, classification), '\\')
    }
    ##
    filenames <- list.files(path)
    for (file in filenames) {
        if(substr(file, 1,5)=='ESAP2')
            file.remove(paste0(path, file))
    }
    ##
    ges.dat <- paste0(path, tolower(cou), '.dat')
    ##
    command <- NULL # create GESMES command, one row for each table
    filenames <- list.files(path.data)
    ## only select GESMES files
    X <- strsplit(x = filenames, split = '[.]')
    all.ges <- NULL
    ## i <- 1
    for (i in c(1:length(X)))
    {
        if (tolower(X[[i]][length(X[[i]])])=="ges")
        {
            all.ges <- c(all.ges, filenames[i])
        }
    }
    filenames <- all.ges
    for(table in tables) {
        ## return all versions of the selected table
        X <- strsplit(x = filenames, split = '_')
        filenames.table <- NULL
        for (i in seq(along=filenames)) {
            if (as.numeric(sapply(X, '[[', 2))[i]%in%table==TRUE) {
                filenames.table <- c(filenames.table, filenames[i])
            }
        }
        if(!is.null(filenames.table)) {
            if(length(filenames.table) > 1) {
                ## version
                X <- strsplit(x = filenames.table, split = '_')
                if (!version=='latest') {
                    file.zip <- filenames.table[match(version, as.numeric(substr(sapply(X, '[[', 7), 2, 5)))]
                } else {
                    Y <- strsplit(x = filenames.table, split = '_')
                    file.zip <- filenames.table[as.numeric(substr(sapply(Y, '[[', 7), 2, 5))==max(as.numeric(substr(sapply(Y, '[[', 7), 2, 5)))]
                }
            } else {
                file.zip <- filenames.table
            }
            if (substr(file.zip, nchar(file.zip)-3, nchar(file.zip))=='.zip') {
                unzip(zipfile = paste0(path.data, file.zip), exdir = substr(path, 1, nchar(path)-1))
            } else {
                file.copy(paste0(path.data, file.zip), paste0(path, file.zip))
            }
            ##
            file.ges <- sub('.zip', '', file.zip)
            command <- paste0(command, '$GESMES "',paste0(path, file.ges),'", "',paste0(tolower(cou), '_gdb.db'),'" \n')
        }
    }
    text.inp <- paste('--  commandes pour le chargement et contrôle du fichier de format Gesmes \n',
                      '--      en provenance de pays de l\'Union européenne ou d\'Eurostat.\n',
                      '--      la procédure $gesmes crée une base de données en Fame\n',
                      '\n',
                      'CD "K:\\Users\\Werth_B"\n',
                      '\n',
                      'Try\n',
                      'Unsave ', tolower(cou), '_gdb.db\n',
                      'End Try\n',
                      'Try\n',
                      'Unsave ', tolower(cou), '_gdb_output.txt\n',
                      'End Try\n',
                      '\n',
                      '\n',
                      '\n',
                      'LOAD "K:\\UTILITIES\\GESMES"\n',
                      '\n',
                      '\n',
                      '-- gesmes procedure : indicate where gesmes input file is located" \n',
                      '\n',
                      command,
                      '\n',
                      '\n',
                      '-- to obtain time series on file output_gesmes\n',
                      '\n',
                      'OUTPUT <ACCESS OVERWRITE> output_gesmes \n',
                      'WIDTH 160\n',
                      '\n',
                      'OPEN   ', tolower(cou), '_gdb.db\n',
                      '\n',
                      'BLOCK\n',
                      'IGNORE ON, ADDITION ON\n',
                      '\n',
                      '\n',
                      '\n',
                      'END BLOCK\n',
                      '\n',
                      '-- REPORT a.lu?\n',
                      'CLOSE  ', tolower(cou), '_gdb.db\n',
                      'OUTPUT TERMINAL   -- Redirection des sorties vers le Terminal\n',
                      '\n',
                      '\n',
                      '\n',
                      'Try\n',
                      '!DEL  "',ges.dat,'" \n',
                      '\n',
                      'CLOSE ', tolower(cou), '_gdb\n',
                      'End try\n',
                      '\n',
                      '-- to obtain a DAT file from the fame database\n',
                      '\n',
                      'CD "K:\\Users\\Werth_B"\n',
                      '\n',
                      '!K:\\FAME\\ffi  dataout.des   ', tolower(cou), '_gdb.db   "',ges.dat,'" -TODATA\n',
                      '\n',
                      '\n',
                      'TYPE "ok, fin"\n',
                      '\n',
                      'EXIT\n', sep='')
    ##
    fileConn <- file(paste0(path, cou, '_R.inp'))
    writeLines(text.inp, fileConn)
    close(fileConn)
}
