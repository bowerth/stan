## cat(paste0('c("', indic.label$indic, '", "', indic.label$label, '"),\n'))
indic.label <- rbind.data.frame(
    c("VSHT", "Value added share relative to total economy"),
    c("VSHM", "Value added shares relative to manufacturing"),
    c("ESHT", "Employment shares in total economy"),
    c("ESHM", "Employment shares in total manufacturing"),
    c("LBNT", "Labour compensation per workforce in total economy"),
    c("LBNM", "Labour compensation per workforce in manufacturing"),
    c("LBET", "Labour compensation per employee in total economy"),
    c("LBEM", "Labour compensation per employee in manufacturing"),
    c("LBVA", "Labour share of value added in total economy"),
    c("IPYE", "Labour productivity index"),
    c("IPYH", "Labour productivity index"),
    c("IULC", "Unit labor cost index"),
    c("LULC", "Unit labor cost level"),
    c("AVHW", "Average hours worked"),
    c("VAPR", "Value added share of production"),
    c("INPR", "Intermediate consumption share of production"),
    c("INVV", "Investment intensity based on value added"),
    c("INVT", "Investment shares relative to total economy"),
    c("INVM", "Investment shares relative to total manufacturing"),
    c("RDST", "Distribution of R&D expenditures across all activities"),
    c("RDSM", "Distribution of R&D expenditures across manufacturing activities"),
    c("RDIV", "R&D intensity using value added"),
    c("RDIP", "R&D intensity using production"),
    c("IITR", "Intra-industry trade"),
    c("CMTB", "Contribution to manufacturing trade balance"),
    c("EXIM", "Export import ratio"),
    c("TBAL", "Trade balance - in US dollars"),
    c("XSHT", "Composition of total exports of goods"),
    c("XSHM", "Composition of manufacturing exports of goods"),
    c("MSHT", "Composition of total imports of goods"),
    c("MSHM", "Composition of manufacturing imports of goods"),
    c("XSHP", "Export share of production"),
    c("MPEN", "Import penetration")
)
names(indic.label) <- c("indic", "label")
list.indic.label <- as.list(as.character(indic.label$label))
names(list.indic.label) <- indic.label$indic

indic.formula <- rbind.data.frame(
    c("IITR", "\\left( {1 - \\frac{ \\left| {EXPO_i - IMPO_i} \\right| } {EXPO_i + IMPO_i}} \\right) \\times 100"),
    c("CMTB", "\\frac{ \\left( EXPO_i - IMPO_i \\right) - \\left( EXPO_{manuf} - IMPO_{manuf} \\right) \\frac{EXPO_i + IMPO_i}{EXPO_{manuf} + IMPO_{manuf}} }{ EXPO_{manuf} + IMPO_{manuf} } \\times 100"),
    c("EXIM", "\\frac{EXPO_i}{IMPO_i} \\times 100"),
    c("TBAL", "EXPO_i - IMPO_i"),
    c("XSHT", "\\frac{EXPO_i}{EXPO_{total}} \\times 100"),
    c("XSHM", "\\frac{EXPO_i}{EXPO_{manuf}} \\times 100"),
    c("MSHT", "\\frac{IMPO_i}{IMPO_{total}} \\times 100"),
    c("MSHM", "\\frac{IMPO_i}{IMPO_{manuf}} \\times 100"),
    c("XSHP", "\\frac{EXPO_i}{PROD_i} \\times 100"),
    c("MPEN", "\\frac{IMPO_i}{PROD_i - EXPO_i - IMPO_i} \\times 100"),
    c("VSHT", "\\frac{VALU_i}{VALUe_{total}} \\times 100"),
    c("VSHM", "\\frac{VALU_i}{VALUe_{manuf}} \\times 100"),
    c("VAPR", "\\frac{VALU_i}{PROD_i} \\times 100"),
    c("INPR", "\\frac{inti_i}{PROD_i} \\times 100"),
    c("RDST", "\\frac{ANBERD_i}{ANBERD{total}} \\times 100"),
    c("RDSM", "\\frac{ANBERD_i}{ANBERD{manuf}} \\times 100"),
    c("RDIV", "\\frac{ANBERD_i}{VALU_i} \\times 100"),
    c("RDIP", "\\frac{ANBERD_i}{PROD_i} \\times 100"),
    c("ESHT", "\\frac{EMPN_i}{EMPN_{total}} \\times 100"),
    c("ESHM", "\\frac{EMPN_i}{EMPN_{manuf}} \\times 100"),
    c("LBNT", "\\frac { \\frac{LABR_i}{EMPN_i} }{ \\frac{LABR_{total}}{EMPN_{total}} } \\times 100"),
    c("LBNM", "\\frac { \\frac{LABR_i}{EMPN_i} }{ \\frac{LABR_{manuf}}{EMPN_{manuf}} } \\times 100"),
    c("LBET", "\\frac { \\frac{LABR_i}{EMPE_i} }{ \\frac{LABR_{total}}{EMPE_{total}} } \\times 100"),
    c("LBEM", "\\frac { \\frac{LABR_i}{EMPE_i} }{ \\frac{LABR_{manuf}}{EMPE_{manuf}} } \\times 100"),
    c("LBVA", "\\frac{LABR_i}{VALU_i} \\times 100"),
    c("IPYE", "\\frac { \\frac{VALK_i}{EMPN_i} }{ \\frac{VALK_{i,2005}}{EMPN_{i,2005}} } \\times 100"),
    c("IPYH", "\\frac { \\frac{VALK_i}{hrsn_i} }{ \\frac{VALK_{i,2005}}{hrsn_{i,2005}} } \\times 100 "),
    c("IULC", "\\frac { \\frac{EMPN_i}{EMPE_i} \\times \\frac{LABR_i}{VALK_i} }{ \\frac{LABR_{i,2005}}{VALK_{i,2005}} } \\times 100"),
    c("LULC", "\\frac{EMPN_i}{EMPE_i} \\times \\frac{LABR_i}{VALK_i}"),
    c("AVHW", "\\frac{hrsn_i}{EMPN_i}"),
    c("INVV", "\\frac{GFCF_i}{VALU_i} \\times 100"),
    c("INVT", "\\frac{GFCF_i}{GFCF_{total}} \\times 100"),
    c("INVM", "\\frac{GFCF_i}{GFCF_{manuf}} \\times 100")
)
names(indic.formula) <- c("indic", "formula")
list.indic.formula <- as.list(as.character(indic.formula$formula))
names(list.indic.formula) <- indic.formula$indic

## indic.all <- merge(indic.label, indic.formula)
## indic.list <- list()
## for (i in seq(along = indic.all$indic)) {
##     temp <- list(label = as.character(indic.all$label[i]),
##                  formula = as.character(indic.all$formula[i]))
##     indic.list[[i]] <- temp
## }
## names(indic.list) <- indic.all$indic

## create metadata for jekyll page
## file <- file.path("//ASAP5", "STI", "Progs", "STAN", "STANi4", "PUB", "DATA", "Indicators", "METADATA", "STANINDICATORSi4_Metadata_LOWER_LEVEL.txt")
file <- system.file("extdata", "stanIndic_meta_lower.txt", package = "stan")
##
## file.prepend <- file.path(dbpath, "GitHub", "jekyll", "industry", "indicator_stanIndic_old.rmd")
## text.prepend <- readLines(file.prepend)
## text.prepend <- paste0(paste0(text.prepend, collapse = '\n'), '\n\n')
text.prepend <- "---
title: STAN Indicators
layout: default
output: bookdown::html_chapter
---

# STAN Indicators

<img src=\"diagrams/stanIndic_menu.png\" alt=\"STAN Indicators Menu\"/>

The application source code is available on
[GitHub](https://github.com/bowerth/desk/blob/master/inst/industry/tools/indic/stanIndic.R)

Indicators derived from the STAN database
:   Calculations based on variables in STAN are popular for macroeconomic analysis, may it be simple indicators such as the share of an industry in the total ecomony or complex indicators such as the contribution of an industry to the economy's manufacturing trade balance.

Timeliness of results
:   To observe the ceteris paribus effects of data modifications on indicator results, it is helpful to use a self-standing calculation system. Underlying data of relatively small size are the only inputs into the system.

Maintenance and flexibility
:   The user interface and output elements of the application are designed in a functional form. Thereby, modifications can be implemented quickly, e.g. new indicators only require the calculation formula and users can be enabled modify the calculation formulas.

Data sources
:   Data underlying the calculations are STAN variables from National Accounts as well as ANBERD and BTDIxE. Please see documentation of STAN database for additional information.

"

##
file.out <- file.path(dbpath, "GitHub", "jekyll", "industry", "indicator_stanIndic.rmd")
## list2rmd(list=meta2, label=list.indic.label, text.prepend=text.prepend, print = TRUE)

meta2 <- readMetadata(file = file, level = "lower")
## list2rmd(list=meta2, label=list.indic.label, text.prepend=text.prepend, file=file.out)
list2rmd(list=meta2, label=list.indic.label, formula=list.indic.formula, text.prepend=text.prepend, file=file.out)

