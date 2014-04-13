#' COU Create
#'
#' Create Country
#'
#' Sets up the SAS folder structure for a new country in STAN.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param isic an integer to specify the version of STAN.
#' @param period a character string with update period.
#' @param remove logical remove country from STAN SAS tables (country, units).
#'
#' @author OECD STAN
#' @keywords SAS
#' @seealso \code{\link{couList}}, \code{\link{SASbatch}}
#' @export
#' @examples
#' couCreate(cou="BGR", isic=3)

couCreate <- function(cou=stop("'cou' must be specified"),
                      isic=3,
                      period="2013-14",
                      remove=FALSE)
{
    if (isic==3)
    {
        path.cou <- PATH.COUi3
        path.sas <- PATH.SASi3
    }
    if (isic==4)
    {
        path.cou <- PATH.COUi4
        path.sas <- PATH.SASi4
    }
    ## file.exists: Directory names must not include a trailing backslash or slash on Windows
    path.template <- paste0(path.cou, 'XXX')
    path.progs <- paste0(path.sas, 'progs\\PAYS\\')
    if (!file.exists(paste0(path.cou, cou)) & remove==FALSE)
    {
        dir.create(paste0(path.cou, cou))
        cat(paste0(cou, " data folder doesn't exist, created at\n\t", paste0(path.cou, cou), "\n"))
        coufolders <- c("Graphs",
                        "Rawdata",
                        "Rawdata\\NSO",
                        "Rawdata\\STD-SNA",
                        "Rawdata\\SUT",
                        "Lists",
                        "SASdata")
        for (folder in coufolders)
        {
            dir.create(paste0(paste0(path.cou, cou), "\\", folder))
        }
        files <- c("mii.sas7bdat", "swn.sas7bdat")
        for (file in files)
        {
            file.copy(from = paste0(path.template, "\\SASdata\\", file),
                      to  =  paste0(paste0(path.cou, cou),      "\\SASdata\\", file))
        }
        files <- c("MDL_IND_ANA.txt")
        for (file in files)
        {
            file.copy(from = paste0(path.template, "\\Lists\\", file),
                      to  =  paste0(paste0(path.cou, cou),      "\\Lists\\", file))
        }
    }
    ## create SAS program folder for country
    if (!file.exists(paste0(path.progs, cou)) & remove==FALSE)
    {
        dir.create(paste0(path.progs, cou))
        cat(paste0(cou, " prorgrams folder doesn't exist, created at\n\t", path.progs, cou, "\n"))
    }
    ## modify SAS libraries
    ## modify country in STAN.cou
    file.cou.stan <- paste0(path.sas, "Lists\\STAN_cou_list.csv")
    cou.stan <- couList(namecou = cou, isic = 3)
    if (remove==FALSE)
    {
        cat(paste0(cou, " added to country list at\n\t", file.cou.stan, "\n"))
    } else {
        cou.stan <- cou.stan[!cou.stan$Cou==cou,]
        cat(paste0(cou, " removed from country list at\n\t", file.cou.stan, "\n"))
    }
    write.csv(cou.stan, file.cou.stan, row.names = FALSE, na = "0")
    ## modify country in STAN.units
    file.units.stan <- paste0(path.sas, "Lists\\STAN_units_list.csv")
    units.stan <- read.csv(file.units.stan)
    units.stan <- units.stan[!units.stan$Cou==cou,]
    ##
    file.units.cou <- paste0(paste0(path.cou, cou), "\\Lists\\", cou, "_units_list.csv")
    if (remove==FALSE)
    {
        if (!file.exists(file.units.cou))
        {
            cat(paste0(cou, " units table doesn't exist, created based on USA information at\n\t", file.units.cou, "\n"))
            usa.units <- units.stan[units.stan$Cou=="USA",]
            units.cou <- cbind.data.frame(cou, usa.units[,2], usa.units[,3], sub("USD", STAN.CUR$cur[STAN.CUR$cou==cou], usa.units[,4]))
            names(units.cou) <- c("Cou", "Var", "Pwc", "Units")
            write.csv(units.cou, file = file.units.cou, row.names = FALSE)
        }
        cat(paste0(cou, " units from\n\t", file.units.cou,
                   "\n\twere added to STAN units table at\n\t", file.units.stan, "\n"))
        units.cou <- read.csv(file.units.cou)
        units.stan <- rbind(units.stan, units.cou)
        units.stan$Cou <- factor(units.stan$Cou, levels = sort(as.character(unique(units.stan$Cou))))
        units.stan <- units.stan[order(units.stan$Cou, units.stan$Var),]
    } else {
        cat(paste0(cou, " units removed from STAN units table at\n\t", file.units.stan, "\n"))
    }
    write.csv(units.stan, file = file.units.stan, row.names = FALSE)
}
