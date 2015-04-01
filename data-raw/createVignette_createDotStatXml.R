
txtfile <- file.path(dbpath, "GitHub", "stan", "data-raw", "createVignette_createDotStatXml.txt")

filecon <- file(txtfile)
text <- readLines(con = filecon)
close(filecon)

## text <- read.table(file = txtfile, sep = "\t", header = TRUE)

X <- strsplit(text, split = "\t")

parameter <- sapply(X, "[[", 1)
description <- sapply(X, "[[", 2)
example <- sapply(X, "[[", 3)

param.list <- list()

as.list(description)

ex.desc.df <- data.frame(parameter = parameter,
                         description = description,
                         example = example)

mdtable <- knitr::kable(x = ex.desc.df)

?knitr::kable

mdtablefile <- file.path(dbpath, "GitHub", "stan", "data-raw", "createVignette_createDotStatXml_mdtable.txt")

filecon <- file(mdtablefile)
writeLines(text = mdtable, con = filecon)

## text <- readLines(con = filecon)
close(filecon)



as.list(ex.desc.df)

param.list$desc

X
