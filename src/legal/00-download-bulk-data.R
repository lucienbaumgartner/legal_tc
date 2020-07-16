library(rvest)

setwd('/Volumes/INTENSO/legal_tc/input/bulk-data/')
getwd()
html <- read_html('https://www.courtlistener.com/api/bulk-info/')
datafiles <- html_node(html, '.table.settings-table') %>% html_table() %>% setNames(., tolower(names(.)))

for(i in datafiles$abbreviation[-c(1,2)]){
  source.url <- paste0('https://www.courtlistener.com/api/bulk-data/opinions/', i,'.tar.gz')
  out <- paste0(i, '.tar.gz')
  utils::download.file(url = source.url, destfile = out)
  write(out, file = '~/legal_tc/src/legal/log.txt', append = T)
}

datafiles <- list.files()

for(i in datafiles){
  system(paste0('mkdir /Volumes/INTENSO/legal_tc/input/bulk-data/', gsub('\\..*', '', i)))
  system(paste0('tar -zxf ', i, ' -C ', gsub('\\..*', '', i)))
  write(i, file = '~/legal_tc/src/legal/unzip-log.txt', append = T)
}
