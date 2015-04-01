#' Read Metadata
#'
#' Read metadata from xls saved as tab delimited txt file
#'
#' Create list with non-empty metadata from file
#'
#' @param file the exported \code{rdata} file.
#' @param level type of metadata: \code{higher} and \code{lower}
#' @param print print code for list creation to console
#'
#' @author OECD STAN
#' @keywords metadata
#' @seealso \code{\link{packageData}}
#' @export
#' @examples
#' ## see /data-raw/create_vignette/stanIndicator.R
#' file <- system.file("extdata", "stanIndic_meta_lower.txt", package = "stan")

readMetadata <- function(file=stop("'file' must be provided"),
                         level="lower",
                         print=FALSE) {

    if (level=="lower") {
        header <- read.table(file, sep = "\t", quote = '"', skip = 5, nrows = 1)
        data <- read.table(file, sep = "\t", quote = '"', skip = 10, header = FALSE)

        names(data) <- header
        names(data)[1] <- "node"
        data <- data[, colnames(data)!="NA"]

        ## ## transform header for list labels
        ## header <- read.table(file, sep = "\t", quote = '"', skip = 5, nrows = 2)
        ## header <- header[, as.character(header[1,])!=""]
        ## header.df <- as.data.frame(t(header))
        ## header.df <- header.df[-1, ] # Coordinates
        ## cat(paste0('c(', header.df[, 1], ', "', header.df[, 2], '"),\n'))
        colnames <- rbind.data.frame(
            c(1, "Contact person/organisation"),
            c(2, "Data source(s) used"),
            c(3, "Name of collection/source"),
            c(4, "Direct source"),
            c(5, "Source Periodicity"),
            c(6, "Source metadata"),
            c(7, "Date last input received"),
            c(8, "Unit of measure used"),
            c(9, "Power code"),
            c(10, "Variables collected"),
            c(11, "Sampling"),
            c(12, "Periodicity"),
            c(13, "Reference period"),
            c(14, "Base period"),
            c(15, "Date last updated"),
            c(16, "Link to Release calendar"),
            c(17, "Contact person"),
            c(18, "Other data characteristics"),
            c(19, "Statistical population"),
            c(20, "Geographic coverage"),
            c(21, "Sector coverage"),
            c(22, "Institutional coverage"),
            c(23, "Item coverage"),
            c(24, "Population coverage"),
            c(25, "Product coverage"),
            c(26, "Other coverage"),
            c(27, "Key statistical concept"),
            c(28, "Classification(s) used"),
            c(29, "Aggregation & consolidation"),
            c(30, "Estimation"),
            c(31, "Imputation"),
            c(32, "Transformations"),
            c(33, "Validation"),
            c(34, "Index type"),
            c(35, "Weights"),
            c(36, "Seasonal adjustment"),
            c(37, "Other manipulations"),
            c(38, "Dissemination format(s)"),
            c(39, "Recommended uses and limitations"),
            c(40, "Quality comments"),
            c(41, "Other comments")
        )
        names(colnames) <- c("item", "label")

        ## i <- 33
        meta.list <- list()
        for (i in seq(along = data$node)) {
            row <- data[i, 2:length(data)]
            row.vector <- t(row)[,1]
            ## remove href in anchor tags
            row.vector <- gsub("<[a|A][^>]*>", "", row.vector)
            row.vector <- gsub("</a>", "", row.vector)
            row.vector <- gsub("</a>", "", row.vector)
            row.vector <- gsub("&lt;br&gt;", "\n\n    ", row.vector)

            ##
            names(row.vector) <- colnames$label
            row.vector <- row.vector[!is.na(row.vector) & row.vector!="" & row.vector!=0]
            row.list <- as.list(row.vector)
            ## meta.list[[data$node[i]]] <- row.list
            meta.list[[i]] <- row.list
        }
        names(meta.list) <- gsub("[|]", ".", data$node)

        ## return(meta.list)

        if (print==FALSE) {
            return(meta.list)
        } else {
            string.all <- NULL
            for (i in seq(along = meta.list)) {
                content <- meta.list[[i]]
                content <- gsub('"', '\"', content)
                string.row <- paste0('"', names(meta.list[[i]]), '" = "', content, '"', collapse = ',\n')
                string.row <- paste0('list(\n', string.row, '\n)')
                string.all <- paste0(string.all, ',\n', names(meta.list)[i], ' = ', string.row)
            }
            ## ## remove href in anchor tags
            ## string.all <- gsub("<[a|A][^>]*>", "", string.all)
            ## string.all <- gsub("</a>", "", string.all)
            string.all <- paste0(string.all, '\n')

            return(cat(string.all))
            ## return(string.all)
        }

    } else {
        return()
    }
}
