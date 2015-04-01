
namaSdmx <- function(nace=1
                     ,
                     table=c("c","e","k","p")
                     ) {

    require(RJSDMX)
    provider <- "EUROSTAT"
    query.freq <- "A"
    query.unit <- "MIO_NAC"
    query.nace_r <- "" # get all industries

    if (nace==1) {
        indlist <- c("06", "31", "60")
    } else if (nace==2) {
        indlist <- c("10", "21", "64")
    }

    nameflow <- names(getFlows(provider, pattern = "nama_nace*"))
    nameflow <- nameflow[substr(nameflow, 10, 11)%in%indlist]
    ## t <- table[1]
    for (t in table) {
        nameflow.table <- nameflow
        nameflow.table <- nameflow.table[substr(nameflow.table, 13, 13)==t]
        ## flow <- nameflow.table[1]

        for (flow in nameflow.table) {
            try(getDimensions(provider, flow), silent = TRUE)

            ## getDimensions(provider, "nama_nace31_c")
            getDimensions(provider, "nama_nace06_c")
            getDimensions(provider, "nama_nace64_c")
            getDimensions(provider, "nama_nace60_c")
            getDimensions(provider, "nama_nace10_c")

        }

        TScodes.var <- names(getCodes(provider, flow, "INDIC_NA"))

        namevareu <- subset(STAN.VAREUROSTAT, var%in%namevar & eurostat%in%TScodes.var, select = "eurostat")
        namevareu <- as.character(unlist(namevareu))
        query.var <- gsub(", ", "+", toString(namevareu))
    }
}

