library(quanteda)
library(ggplot2)
library(dplyr)
library(pbmcapply)
library(lubridate)
library(viridis)
#library(ggwordcloud)
library(car)
#library(nortest)
#library(fastDummies)
library(pbapply)
library(tm)

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")
kw <- read.table('../input/dict_operation_pony.txt', stringsAsFactors = F, sep=',', header = T) %>% rename(TARGET = word)

### load reddit data
load('../../output/02-finalized-corpora/baseline/reddit/reddit.RDS')
# available as <reddit>

### load legal data
fileslist <- list.files('../../output/02-finalized-corpora/legal/', full.names = T)
df <- pbmclapply(fileslist, function(x){
  load(x)
  df <- mutate(df, context=gsub('\\..*', '', x))
  return(df)
}, mc.cores=4)


