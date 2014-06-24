library(knitr)

library(RSQLite)

path <- '/home/z930/Dropbox/GitHub/stan/inst/doc/'
file <- "demo"

DbFile <- paste0(path, 'faq.db')

diskdb <- dbConnect(SQLite(), dbname = DbFile)
## ok <- dbWriteTable(diskdb, "estates", estates.new, row.names = FALSE, overwrite = TRUE)

entries.db <- dbReadTable(diskdb, "entries")
sections.db <- dbReadTable(diskdb, "sections")

header <- paste0('% OECD Structural Analysis Database', '\n',
                 '% Frequently Asked Questions', '\n'
                 ## ,'% April 2014', '\n'
                 )

md.text <- NULL
for (sec in unique(entries.db$sec))
  {
    label.sec <- sections.db$label[sections.db$sec==sec]
    md.text.section <- c(paste0('# ', label.sec, '\n'),
                         paste0('## ', entries.db$title[entries.db$sec==sec], '\n\n', entries.db$text[entries.db$sec==sec], '\n')
                         )
    md.text <- c(md.text, md.text.section)
  }
## cat(md.text)

fileConn <- file(paste0(path, 'demo.md'))
writeLines(c(header, md.text), fileConn)
close(fileConn)

command.pandoc <- paste0('pandoc ', path, file, '.md -s -o ', path, file, '.texi')
command.texinfo <- paste0('makeinfo ', path, file, '.texi --html -o ', path, file)

system(command.pandoc)
system(command.texinfo)


