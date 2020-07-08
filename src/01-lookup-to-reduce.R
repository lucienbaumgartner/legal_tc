library(stringr)
library(spacyr)
library(gtools)
library(tokenizers)

rm(list=ls())
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

datasets <- list.files('../output/00-bulk-data', full.names = T)
search.terms <- read.table('../input/dict.txt', header = T, stringsAsFactors = F, sep=',')

for(i in datasets){
  
  load(i)
  .lookup <- paste0(paste0('\\b', search.terms$word, '\\b'), '(\\,)?\\s\\band\\b', collapse = '|')
  corpus <- pbmclapply(df$txt, function(x){
    #print(x)
    tmp <- tokenizers::tokenize_sentences(x)
    tmp <- unlist(tmp)
    tmp <- tolower(tmp)
    tmp <- tmp[grepl(.lookup, tolower(tmp), perl = T)]
    tmp <- unname(tmp)
    return(tmp)
  }, mc.cores = 4)
  
  df <- cbind(df[rep(1:nrow(df), lengths(corpus)),], corpus=unlist(corpus)) %>% as_tibble
  rm(corpus)
  df <- mutate(df, corpus = as.character(corpus))
  reg_matches <- pbmclapply(df$corpus, function(x) str_extract_all(x, .lookup), mc.cores=4)
  reg_matches <- unlist(reg_matches, recursive=F)
  lengths(reg_matches2)
  df <- cbind(df[rep(1:nrow(df), lengths(reg_matches)),], match=unlist(reg_matches)) %>% as_tibble
  df <- mutate(df, match = as.character(match))
  df <- as_tibble(df)
  out <- 
  
}
