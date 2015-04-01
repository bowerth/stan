#' createDotStatXML
#'
#' Send file to createDotStatXML procedure
#'
#' Use the createDotStatXML command line tool to upload data in OECD.Stat
#'
#' @param exdir character string to specify output location
#' @param notifications character, empty string or one of the following: \code{N} no notification is needed, \code{W} on warning, \code{F} on failure or \code{S} on success
#' @param domain character string doamin, usually \code{OECDMAIN}
#' @param contacts character string author Windows account
#' @param direction character string [direction]/[division]
#' @param email character string author email address
#' @param datasetnameEN character string for dataset label in English
#' @param datasetnameFR character string for dataset label in French
#' @param ds_code dataset code identifier, e.g. STI_STANi4_2015_v0 [direction]_[label]_[year]_[version]
#' @param theme character string to location of dataset in dotstat
#' @param action character string one of \code{CREATE}, \code{UPDATE}, \code{REPLACEALL} or \code{DELETE}
#' @param updatequeries logical to update dotstat queries
#' @param dimensions.list a list with dimension members and dimension metadata
#' @param langEN integer value to specify language ID for English (default: 1033)
#' @param langFR integer value to specify language ID for French (default: 1036)
#' @param useExe logical to use command line tool at \code{\\\\dotstat\\Data\\UserUtilities\\DataProviderInterface\\CreateDotStatXml.exe}
#'
#' @author OECD STAN
#' @keywords SQL
#' @seealso \code{\link{createDimension}}, \code{vignette("createDotStatXML")}
#' @importFrom XML xmlParse
#' @export
#' @examples
#'
#' vignette(topic = "createDotStatXML", package = "stan")
#'
#' source(file.path(dbpath, "GitHub", "stanData", "data-raw", "DOTSTAT", "exportSTANi4SA10.R"))
#'
#' contacts="Werth_B"
#' notifications="f"
#' owners="Werth_B"
#' security="Werth_B"
#' ds_names=c("STANi4 Test5", "STANi4 Test5")
#' ds_code="STANi4SA10_10"
#' languages=c("en", "fr")
#' action="Create"
#' updatequeries=TRUE # exe uses "1" and "0"
#' theme="Industry and Services|Structural Analysis (STAN) Databases"
#' dir="STI"
#' nbdims=4
#' dim_names_e=c("Country", "Variable", "Industry", "Time")
#' dim_names_f=c("Pays", "Variable", "Industrie", "Temps")
#' dim_codes=c("LOCATION", "VAR", "IND", "TIME");
#' dim_common=c("LOCATION", "*", "*", "TIME");
#' dim_members_columns=c("\\*", shQuote("CLO"), shQuote("CPLO"), "\\*")
#' dim_members_csv=c("*", "dim_Var.txt", "dim_Ind.txt", "*")
#' data_flags="false"
#' domain="OECDMAIN"
#' entrygate="STI - Structural Analysis (STAN) Databases"
#' xml_filename="CREATE_STANi4SA10.xml"
#' print=FALSE
#' ## useExe=TRUE
#'
#' test <- createDotStatXML(contacts=contacts, notifications=notifications, owners=owners, security=security, ds_names=ds_names, ds_code=ds_code, languages=languages, action=action, updatequeries=updatequeries, theme=theme, dir=dir, nbdims=nbdims, dim_names_e=dim_names_e, dim_names_f=dim_names_f, dim_codes=dim_codes, dim_common=dim_common, dim_members_columns=dim_members_columns, dim_members_csv=dim_members_csv, data_flags=data_flags, domain=domain, entrygate=entrygate, csv_folder=csv_folder, xml_filename=xml_filename, print=TRUE)
#' cat(test)

createDotStatXML <- function(
    ## contacts=error('"contacts" is not specified'),
    ## notifications=error('"notifications" is not specified'),
    ## owners=error('"owners" is not specified'),
    ## security=error('"security" is not specified'),
    ## ds_names=error('"ds_names" is not specified'),
    ## ds_code=error('"ds_code" is not specified'),
    ## languages=error('"languages" is not specified'),
    ## action=error('"action" is not specified'),
    ## updatequeries=error('"updatequeries" is not specified'),
    ## theme=error('"theme" is not specified'),
    ## dir=error('"dir" is not specified'),
    ## nbdims=error('"nbdims" is not specified'),
    ## dim_names_e=error('"dim_names_e" is not specified'),
    ## dim_names_f=error('"dim_names_f" is not specified'),
    ## dim_codes=error('"dim_codes" is not specified'),
    ## dim_common=error('"dim_common" is not specified'),
    ## dim_members_columns=error('"dim_members_columns" is not specified'),
    ## dim_members_csv=error('"dim_members_csv" is not specified'),
    ## data_flags=error('"data_flags" is not specified'),
    ## domain=error('"domain" is not specified'),
    ## csv_folder=error('"csv_folder" is not specified'),
    ## entrygate=error('"entrygate" is not specified'),
    ## xml_filename=error('"xml_filename" is not specified'),
    contacts=NULL,
    notifications="f",
    owners=NULL,
    security=NULL,
    ds_names=NULL,
    ds_code=NULL,
    languages=NULL,
    action=NULL,
    updatequeries=NULL,
    theme=NULL,
    dir=NULL,
    nbdims=NULL,
    dim_names_e=NULL,
    dim_names_f=NULL,
    dim_codes=NULL,
    dim_common=NULL,
    dim_members_columns=NULL,
    dim_members_csv=NULL,
    data_csv=NULL,
    data_flags=NULL,
    flags_csv=NULL,
    domain=NULL,
    entrygate=NULL,
    csv_folder=NULL,
    xml_filename="R_createDotStat.xml",
    useExe=TRUE,
    print=FALSE
) {

    if (useExe==TRUE) {

        prog <- file.path("//Dotstat", "Data", "UserUtilities", "DataProviderInterface", "CreateDotStatXml.exe")

        contacts <- shQuote(contacts)
        notifications <- shQuote(notifications)
        owners <- shQuote(owners)
        security <- shQuote(security)
        ds_names <- paste0(ds_names, collapse = '#')
        ds_names <- shQuote(ds_names)
        ds_code <- shQuote(ds_code)
        languages <- paste0(languages, collapse = '#')
        languages <- shQuote(languages)
        updatequeries <- ifelse(TRUE, 1, 0)
        theme <- shQuote(theme)
        dim_names_e <- gsub(" ", "", toString(shQuote(dim_names_e)))
        dim_names_f <- gsub(" ", "", toString(shQuote(dim_names_f)))
        dim_names <- paste0(c(dim_names_e, '#', dim_names_f), collapse = "")
        dim_codes <- gsub(" ", "", toString(dim_codes))
        dim_codes <- shQuote(dim_codes)
        dim_common <- gsub(" ", "", toString(dim_common))
        dim_common <- shQuote(dim_common)
        if (!is.null(dim_members_columns)) dim_members_columns <- gsub(" ", "", toString(dim_members_columns))
        if (!is.null(dim_members_csv)) dim_members_csv <- gsub(" ", "", toString(shQuote(dim_members_csv)))
        ##
        data_csv <- shQuote(data_csv)
        if (!is.null(data_flags)) data_flags <- shQuote(ifelse(data_flags, "true", "false"))
        if (!is.null(flags_csv)) flags_csv <- shQuote(flags_csv)
        domain <- shQuote(domain)
        csv_folder <- file.path(entrygate, "CSVFILES", fsep = "\\")
        entrygate <- shQuote(entrygate)
        csv_folder <- shQuote(csv_folder)
        xml_filename <- shQuote(xml_filename)

        ## create dataset
        if (tolower(action)%in%c("create")) {
            nameparam <- c("contacts",
                           "notifications",
                           "owners",
                           "security",
                           "ds_names",
                           "ds_code",
                           "languages",
                           "action",
                           "updatequeries",
                           "theme",
                           "dir",
                           "nbdims",
                           "dim_names",
                           "dim_codes",
                           "dim_common",
                           "dim_members_columns",
                           "dim_members_csv",
                           "data_flags",
                           "flags_csv",
                           "domain",
                           "entrygate",
                           "csv_folder",
                           "xml_filename")

            ## modify dimensions
        } else if (tolower(action)%in%c("update")) {

            nameparam <- c("contacts",
                           "notifications",
                           "owners",
                           "ds_code",
                           "languages",
                           "action",
                           "updatequeries",
                           "nbdims",
                           "dim_codes",
                           ## "dim_common",
                           "dim_members_columns",
                           "dim_members_csv",
                           "data_flags",
                           "flags_csv",
                           ## "domain",
                           "entrygate",
                           "csv_folder",
                           "xml_filename")

            ## replace data
        } else if (tolower(action)%in%c("replaceall")) {
            nameparam <- c("contacts",
                           "notifications",
                           "owners",
                           "ds_code",
                           "languages",
                           "action",
                           "updatequeries",
                           "nbdims",
                           "dim_codes",
                           "data_csv",
                           ##
                           "data_flags",
                           "flags_csv",
                           ##
                           "entrygate",
                           "csv_folder",
                           "xml_filename")

            ## remove dataset
        } else if (tolower(action)%in%c("removedataset")) {
            nameparam <- c("contacts",
                           "owners",
                           "ds_code",
                           "languages",
                           "action",
                           "updatequeries",
                           "nbdims",
                           "dim_codes",
                           "entrygate",
                           "xml_filename")
        }

        ## cat(paste0(nameparam, '=', nameparam, ',\n'))
        ## cat(paste0(nameparam, '=error(\'"', nameparam, '" is not specified\'),\n'))
        ## cat(paste0(nameparam, '=NULL,\n'))

        ## get("dir")

        ## remove "NULL" parameters
        nameparam <- nameparam[sapply(nameparam, function (x) length(get(x))) > 0]

        param <- lapply(nameparam, get, envir = environment())
        names(param) <- toupper(nameparam)

        ## param <- list(
        ##     CONTACTS=contacts,
        ##     NOTIFICATIONS=notifications,
        ##     OWNERS=owners,
        ##     SECURITY=security,
        ##     DS_NAMES=ds_names,
        ##     DS_CODE=ds_code,
        ##     LANGUAGES=languages,
        ##     ACTION=action,
        ##     UPDATEQUERIES=updatequeries,
        ##     THEME=theme,
        ##     DIR=dir,
        ##     NBDIMS=nbdims,
        ##     DIM_NAMES=dim_names,
        ##     DIM_CODES=dim_codes,
        ##     DIM_COMMON=dim_common,
        ##     DIM_MEMBERS_COLUMNS=dim_members_columns,
        ##     DIM_MEMBERS_CSV=dim_members_csv,
        ##     ##
        ##     DATA_FLAGS=data_flags,
        ##     DOMAIN=domain,
        ##     ENTRYGATE=entrygate,
        ##     CSV_FOLDER=csv_folder,
        ##     XML_FILENAME=xml_filename
        ## )

        ## end action: create

        ## ## action: update
        ## action <- "UPDATE"
        ## xml_filename <- "UPDATE_STANi4SA10.xml"
        ## param <- list(DS_CODE=shQuote(ds_code),
        ##               ACTION=action,
        ##               ENTRYGATE=shQuote(entrygate),
        ##               CONTACTS=shQuote(contacts),
        ##               OWNERS=shQuote(owners),
        ##               SECURITY=shQuote(security),
        ##               DOMAIN=domain,
        ##               CSV_FOLDER=shQuote(csv_folder),
        ##               NBDIMS=nbdims,
        ##               DIM_CODES=shQuote(dim_codes),
        ##               DIM_COMMON=dim_common,
        ##               DIM_MEMBERS_CSV=dim_members_csv,
        ##               DIM_MEMBERS_COLUMNS=dim_members_columns,
        ##               DIM_NAMES=dim_names,
        ##               UPDATEQUERIES=updatequeries,
        ##               XML_FILENAME=shQuote(xml_filename)
        ##               )
        ## ## end actin: update

        ## ## action: remove
        ## action <- "REMOVEDATASET"
        ## xml_filename <- "DELETE_STANi4SA10.xml"
        ## param <- list(DS_CODE=shQuote(ds_code),
        ##               ACTION=action,
        ##               ENTRYGATE=shQuote(entrygate),
        ##               XML_FILENAME=shQuote(xml_filename)
        ##               )
        ## ## end action: remove

        ## command <- paste(names(param), unlist(param), sep = "=", collapse = " ")
        command <- paste(names(param), unlist(param), sep = "=")

        ## ## write to file
        ## filecon <- file(file.path(dlpath, "commands4.txt"))
        ## writeLines(text = command, con = filecon)
        ## close(filecon)

        ## ## read from file
        ## filecon <- file(file.path(dlpath, "commands.txt"))
        ## command <- readLines(con = filecon)
        ## close(filecon)

        command <- paste(command, collapse = " ")
        command <- paste(prog, command, '\n')

        if (print==TRUE)
            return(command)
        ## cat(command)
        ## writeClipboard(str = command)
        else
            shell(command, shell = PATH.SHELL)


    } else {

        ## ## examles
        ## ## create XML directly
        ##
        ## require(XML)
        ## domain <- "OECDMAIN"
        ## contacts <- "Werth_B"
        ## email <- "Bo.Werth@OECD.
        ## direction <- "STI/EAS"
        ## datasetnameEN <- "STAN Database for Structural Analysis (ISIC Rev. 4)"
        ## ## 233: utf8ToInt(iconv("'e" to  <-  "utf-8")),
        ## datasetnameFR <- paste0('Base de donn', intToUtf8(233), 'es STAN pour l\'Analyse Structurelle (CITI Rev. 4)')
        ## ds_code <- "STANi4SA10"
        ## file <- file.path(exdir, paste0('CREATE_', ds_code, '.xml'))
        ## theme <- "Industry and Services|Structural Analysis (STAN) Databases"
        ## action <- "CREATE"
        ## updatequeries <- FALSE
        ## dimensions.list <- dimensions.list # see example createDimension()
        ## langEN="1033"
        ## langFR="1036"
        ##
        ## file.remove(file)
        ##
        ## createDotStatXML(file=file,
        ##                  domain=domain,
        ##                  contacts=contacts,
        ##                  direction=direction,
        ##                  datasetnameEN=datasetnameEN,
        ##                  ## 233: utf8ToInt(iconv("'e", to = "utf-8")),
        ##                  datasetnameFR=datasetnameFR,
        ##                  ds_code=ds_code,
        ##                  theme=theme,
        ##                  action=action,
        ##                  updatequeries=updatequeries,
        ##                  dimensions.list=dimensions.list)

        ## ## function parameters
        ## exdir=exdir,
        ## file=file, # file.path(exdir, paste0('CREATE_', ds_code, '.xml'))
        ##  domain="OECDMAIN",
        ##  contacts="Werth_B",
        ##  direction="STI",
        ##  email="Bo.WERTH@oecd.org"
        ## ,
        ##  datasetnameEN="STAN Database for Structural Analysis (ISIC Rev. 4)",
        ##  ## 233: utf8ToInt(iconv("'e", to = "utf-8")),
        ##  datasetnameFR=paste0('Base de donn', intToUtf8(233), 'es STAN pour l\'Analyse Structurelle (CITI Rev. 4)'),
        ##  ds_code="STANi4SA10",
        ##  theme="Industry and Services|Structural Analysis (STAN) Databases",
        ##  action="Create",
        ##  updatequeries=TRUE,
        ##  dimensions.list=dimensions.list,
        ##  ## langEN="1033",
        ##  ## langFR="1036",
        ##  useExe=TRUE
        ##  ## ,
        ##  ##  cr="&lt;br \\&gt;"


        admin.list <- list(Admin=
                               list(Separator="|",
                                    Contacts=
                                        list(Contact=
                                                 list(Name=paste0(domain, '\\', contacts),
                                                      Direction=direction,
                                                      "E-mail"=email,
                                                      Language=langEN)
                                             ),
                                    Owners=
                                        list(Owner=
                                                 list(Name=paste0(domain, '\\', contacts),
                                                      Direction=direction,
                                                      "E-mail"=email,
                                                      Language=langEN)
                                             ),
                                    Security=
                                        list(Membership=
                                                 list(UserGroup=contacts,
                                                      Domain=domain)
                                             )
                                    ## ,
                                    )
                           )
        ## str(admin.list)

        dataset.head.list <- list(DatasetName=
                                      list(Language=langEN,
                                           ## Value="STANi4SA10"),
                                           Value=stan:::cdata(datasetnameEN)),
                                  DatasetName=
                                      list(Language=langFR,
                                           ## Value="STANi4SA10"),
                                           Value=stan:::cdata(datasetnameFR)),
                                  Code=ds_code,
                                  ## Languages=
                                  ##     list(Language="en#fr")
                                  ## )
                                  Action=action,
                                  ## UpdateQueries="false",
                                  UpdateQueries=ifelse(updatequeries, "true", "false"),
                                  ## Theme="Industry and Services|Structural Analysis (STAN) Databases")
                                  Theme=theme)
        ##
        ## str(dataset.head.list)

        ## dimensions.list <- list(Dimension=dimCou.list,
        ##                         Dimension=dimVar.list,
        ##                         Dimension=dimInd.list,
        ##                         Dimension=dimYear.list
        ##                         )
        ## names(dimensions.list) <- rep("Dimension", length(dimensions.list))

        ## combine to dataset.list
        dataset.body.list <- list(DimensionCount=length(dimensions.list),
                                  Dimensions=dimensions.list)

        ## language.list=
        ##     list(Languages=
        ##              list(Language="en#fr")
        ##          )

        xml.list2 <- c(
            admin.list,
            ## language.list,
            list(Dataset=c(
                     dataset.head.list,
                     dataset.body.list)
                 )
        )

        xml.str <- listToXml(xml.list2, "OECD.STAT")

        ## xmlfile.out.ansi <- file.path(exdir, "CREATE_STANi4SA10_ansi.xml")
        xmlfile.out.ansi <- tempfile(fileext = ".xml")
        ## xmlfile.out.utf8 <- file.path(exdir, paste0('CREATE_', ds_code, '.xml'))
        xmlfile.out.utf8 <- file
        ## xmlfile.out.utf8 <- file.path(exdir, "CREATE_STANi4SA10.xml")

        ## ansi | save xml
        saveXML(doc = xml.str, file = xmlfile.out.ansi, prefix = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")  # help says: encoding ignored at the moment

        ## ansi | read lines from file
        ## replace: &apos;
        text <- readLines(xmlfile.out.ansi)
        text <- sub("<OECD.STAT>", "<OECD.STAT xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" Version=\"1.0\">" , text)
        text <- gsub("&apos;" , "\'", text)
        text <- gsub("&lt;" , "<", text)
        text <- gsub("&gt;" , ">", text)
        ## convert ansi to utf-8
        text <- iconv(text, to = "UTF-8")
        filecon <- file(xmlfile.out.utf8, encoding = "utf-8")
        writeLines(text = text, con = filecon)
        close(filecon)

        ## add 'Languages' node to dataset
        input.xml <- xmlInternalTreeParse(xmlfile.out.utf8) ## , "//dataset")
        ## input.xml <- xmlTreeParse(xmlfile.out.utf8, useInternalNodes = TRUE) ## , "//dataset")
        ## input.xml <- xmlTreeParse(xmlfile.out.utf8, useInternalNodes = FALSE) ## , "//dataset")
        dataset.xml <- getNodeSet(input.xml, "//Dataset")
        node.languages = newXMLNode("Languages")
        newXMLNode("Language", attrs = c(LanguageCode = "en", Order = 1), parent = node.languages)
        newXMLNode("Language", attrs = c(LanguageCode = "fr", Order = 2), parent = node.languages)
        dataset.xml[[1]] <- addChildren(dataset.xml[[1]], node.languages)

        ## add line delimiters to dimensions data
        ## ...

        ## class(xml.str)
        ## [1] "XMLNode"          "RXMLAbstractNode" "XMLAbstractNode"
        ## class(input.xml)
        ## [1] "XMLInternalDocument" "XMLAbstractDocument"

        ## text
        ## text <- gsub("<br \\>" , "\n", text)

        ## input.xml.test <- xmlTreeParse(xmlfile.out.utf8.internal)

        xmlfile.out.utf8.internal <- file.path(exdir, "CREATE_STANi4SA10_int.xml")

        ## source(file.path(dbpath, "GitHub", "XML", "R", "saveXML.R"))


        saveXML(doc = input.xml,
                file = xmlfile.out.utf8.internal
                ## ,
                ##  prefix = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
                )  # help says: encoding ignored at the moment






        ## ## xmlfile.out.utf8.internal <- file.path(dlpath, "CREATE_STANi4SA10_int.xml")
        ## xmlfile.out.utf8.linebreak <- file.path(dlpath, "CREATE_STANi4SA10_crlf.xml")
        ## text <- readLines(xmlfile.out.utf8.internal, encoding = "utf-8")
        ## text <- gsub("CRLF", "\n", text)
        ## filecon <- file(xmlfile.out.utf8.linebreak, encoding = "utf-8")
        ## ## writeLines(text = text, con = filecon)
        ## writeLines(text = saveXML(doc = input.xml), con = filecon)
        ## close(filecon)

        ## ?saveXML

        ## b = newXMLNode("bob")
        ## saveXML(b)

        ## f = tempfile()
        ## saveXML(b, f)
        ## doc = xmlInternalTreeParse(f)
        ## saveXML(doc)

        ## class(xml.str)
        ## class(input.xml)

        ## shell <- file.path("C:", "Windows", "system32", "cmd.exe")
        ## shell(shQuote(paste(PATH.ICONV, "-f cp1252 -t utf-8", xmlfile.out.ansi.replace, ">", xmlfile.out.utf8)), shell = shell)
        ## xml.data <- xmlParse(file = xmlfile.out.utf8)
        ## xml.data <- xmlParse(file = xmlfile.out.utf8)
        ## test <- xmlParse(file = file.path(exdir, "Export_Examples", "CREATE_ADB.xml"))
        ## iconv doesn't translate to utf...
        ## file.remove(xmlfile.out.ansi)
        ## file.remove(xmlfile.out.ansi.replace)

    }
}

#' createDimension
#'
#' \code{createDimension}: function to create list for each dimension in data
#'
#' createDimension(code, nameen, namefr, common, data, langEN, langFR)
#'
#' @rdname createDotStatXML
#' @export
#' @examples
#' require(stanData)
#' data(STANNAi4)
#' data=DATA.STANi4[sample(c(1:nrow(DATA.STANi4)), 100),]
#'
#' code.year <- c(min(data[["year"]]):max(data[["year"]]))
#' dimYear.df <- data.frame(Key=code.year,
#'                          KeyParent=code.year,
#'                          NameEN=code.year,
#'                          NameFR=code.year,
#'                          Order=seq(along=code.year))
#'
#' create.dimensions.list <- list(year=
#'                                    list(code="TIME",
#'                                         nameen="Time",
#'                                         namefr="Temps",
#'                                         common=TRUE,
#'                                         data=dimYear.df)
#'                                )
#'
#' dimensions.list <- lapply(create.dimensions.list,
#'                           function (x) createDimension(code=x$code,
#'                                                              nameen=x$nameen,
#'                                                              namefr=x$namefr,
#'                                                              common=x$common,
#'                                                              data=x$data
#'                                                              ))
#'
#' names(dimensions.list) <- rep("Dimension", length(dimensions.list))
createDimension <- function(code=stop('"code" must be specified'),
                            nameen=stop('"nameen" must be specified'),
                            namefr=stop('"namefr" must be specified'),
                            common=FALSE,
                            data=data,
                            langEN="1033",
                            langFR="1036",
                            cr="\n"
                            ## , cr="&#x13&#x10;"
                            ) {
    ##
    rows <- sapply(as.data.frame(t(data)), paste, collapse="|")
    ## string <- paste(rows, collapse='\n')
    ## string <- paste0(string, '\n')  # additional linebreak at end
    string <- paste(rows, collapse = cr)
    string <- paste0(string, sep = cr)  # additional linebreak at end

    ##
    Dimension <-
        list(DimensionName=
                 list(Language=langEN,
                      Value=stan:::cdata(nameen)),
             DimensionName=
                 list(Language=langFR,
                      Value=stan:::cdata(namefr)),
             Code=code)
    ##
    ## if (common==TRUE) Dimension <- c(Dimension, list(Common="LOCATION"))
    if (common==TRUE) Dimension <- c(Dimension, list(Common=code))
    ##
    DataStructure <- as.list(as.character(seq(along=data)))
    names(DataStructure) <- names(data)
    ##
    Dimension <- c(Dimension, list(DataStructure=DataStructure,
                                   DimensionData=string,
                                   ## DimensionData=stan:::cdata(string),
                                   DimensionDataCount=as.character(nrow(data))))
    ##
    return(Dimension)
}

#' listToXml
#'
#' \code{listToXml}: function to create xml from list. copied from https://github.com/PecanProject/pecan/blob/master/utils/R/utils.R
#'
#' listToXml(item, tag)
#'
#' @rdname createDotStatXML
#' @export
listToXml <- function(item, tag) {
                                        # just a textnode, or empty node with attributes
    if(typeof(item) != 'list') {
        if (length(item) > 1) {
            xml <- xmlNode(tag)
            for (name in names(item)) {
                xmlAttrs(xml)[[name]] <- item[[name]]
            }
            return(xml)
        } else {
            return(xmlNode(tag, item))
        }
    }
    ##
                                        # create the node
    if (identical(names(item), c("text", ".attrs"))) {
                                        # special case a node with text and attributes
        xml <- xmlNode(tag, item[['text']])
    } else {
                                        # node with child nodes
        xml <- xmlNode(tag)
        for(i in 1:length(item)) {
            if (names(item)[i] != ".attrs") {
                xml <- append.xmlNode(xml, listToXml(item[[i]], names(item)[i]))
            }
        }
    }
    ##
                                        # add attributes to node
    attrs <- item[['.attrs']]
    for (name in names(attrs)) {
        xmlAttrs(xml)[[name]] <- attrs[[name]]
    }
    return(xml)
}

cdata <- function(str) {
    paste0('<![CDATA[', str, ']]>')
}
## cdata("text")

## ######################### ##
## use exe command line tool ##
## ######################### ##




## ##################################### ##
## create data for exe command line tool ##
## ##################################### ##

## bcpRead(table="UNMADT.dbo.sna93_group", server="VS-GEN-SQL-5", bcpOUT = "-c -t| -T", file.data = file.path(dlpath, "test.txt"))
## data
## h(data)
##
## bcp qna.dbo.ab_QNA_API_DATA out "\\dotStat\Data\DWEntryGate\STD - Quarterly National Accounts\CSVFILES\QNA_DATA.txt" -c -t| -T -S STD-SQL12-1
##
## SQL bcp Utility: https://msdn.microsoft.com/en-us/library/ms162802.aspx
##
## -t: Specifies the field terminator. The default is \t (tab character)
## -T: Specifies that the bcp utility connects to SQL Server with a trusted connection using integrated security.
## -c: Performs the operation using a character data type. It uses char as the storage type and \r\n (newline character) as the row terminator. -c is not compatible with -w.
## -w: Performs the bulk copy operation using Unicode characters. It uses nchar as the storage type and \n (newline character) as the row terminator. -w is not compatible with -c.
##
## Step 2: creates the xml to transfers the data file to Dotstat
## \\dotstat\Data\UserUtilities\DataProviderInterface\CreateDotStatXml.exe DS_CODE="QNA" ACION=REPLACEALL CONTACTS="deBayser_A" NOTIFICATIONS=f ENTRYGATE="STD - Quarterly National Accounts" CSV_FOLDER="STD - Quarterly National Accounts\CSVFILES" NBDIMS=5 XML_FILENAME="QNA_REPLACEALL_DATA.xml" DATA_CSV="QNA_DATA.txt" DATA_FLAGS="True" DIM_CODES="LOCATION","SUBJECT","MEASURE","FREQUENCY","TIME"
##
## The same applies for dimension members updates & metadata transfers.
## Also all our sql jobs are scheduled so they run with a generic proxy account.
