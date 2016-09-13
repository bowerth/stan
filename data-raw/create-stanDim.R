
## setwd to current dir "data-raw"

rm(list = ls())


file_stanDim <- file.path("..", "data", "stanDim.rda")

library(stan)
## add UN hierarchy of countries from inst/extdata/loadDim_regionUNagg.csv
loadDim(dim = c("hierarchyCOUUN"), file = file_stanDim, datalist = TRUE)

load(file_stanDim)
ls()

STANCOUUN.HIERARCHY
