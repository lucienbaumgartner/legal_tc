library(stringr)
library(spacyr)
library(gtools)
library(tokenizers)
library(pbmcapply)
library(dplyr)

rm(list=ls())
setwd('~/legal_tc/src/legal/')

bytes <- 1500*1024^2
options(future.globals.maxSize = bytes)
getOption('future.globals.maxSize')
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#datasets <- list.files('../../output/00-bulk-data/legal', full.names = T)
datasets <- list.files('/Volumes/INTENSO/legal_tc/output/00-bulk-data/legal', full.names = T)
datasets <- datasets[!grepl('scotus', datasets)]
#search.terms <- read.table('../../input/dict-add.txt', header = T, stringsAsFactors = F, sep=',')
search.terms <- read.table('../../input/descriptive_terms.txt', header = T, stringsAsFactors = F, sep=',')
search.term <- mutate(search.terms, cat = 'descriptive')
  
for(i in datasets){
  
  #i = datasets[1]
  print(i)
  load(i)
  .lookup <- paste0(paste0('(\\w+(\\,)?\\s\\band\\b\\s\\b', search.terms$word, '\\b', ')|(\\b', search.terms$word, '\\b'), '(\\,)?\\s\\band\\b\\s\\w+)', collapse = '|')
  df <- mutate(df, year = as.numeric(year))
  df <- filter(df, year > 1979)
  
  set.seed(12345)
  df <- df %>% ungroup %>% sample_frac(.6)
  
  if(object.size(df) > 1000*1024^2){
    print('splitting corpus')
    vec <- list()
    vec[[1]] <- df$txt[1:(round(length(df$txt)/2, 0))]
    vec[[2]] <- df$txt[(round(length(df$txt)/2, 0)+1):length(df$txt)]
    for(m in 1:2){
      vec[[m]] <- pbmclapply(vec[[m]], function(x){
        #print(x)
        tmp <- tokenizers::tokenize_sentences(x)
        tmp <- unlist(tmp)
        tmp <- tolower(tmp)
        tmp <- tmp[grepl(.lookup, tolower(tmp), perl = T)]
        tmp <- unname(tmp)
        return(tmp)
      }, mc.cores = 4)
    }
    corpus <- append(vec[[1]], vec[[2]])
    length(corpus) == nrow(df)
  }else{
    corpus <- pbmclapply(df$txt, function(x){
      #print(x)
      tmp <- tokenizers::tokenize_sentences(x)
      tmp <- unlist(tmp)
      tmp <- tolower(tmp)
      tmp <- tmp[grepl(.lookup, tolower(tmp), perl = T)]
      tmp <- unname(tmp)
      return(tmp)
    }, mc.cores = 4)
  }
  
  df <- cbind(df[rep(1:nrow(df), lengths(corpus)),], corpus=unlist(corpus)) %>% as_tibble
  rm(corpus)
  df <- mutate(df, corpus = as.character(corpus))
  reg_matches <- pbmclapply(df$corpus, function(x) str_extract_all(x, .lookup), mc.cores=4)
  reg_matches <- unlist(reg_matches, recursive=F)
  df <- cbind(df[rep(1:nrow(df), lengths(reg_matches)),], match=unlist(reg_matches)) %>% as_tibble
  df <- mutate(df, match = as.character(match))
  df <- as_tibble(df)
  #df
  # save data
  out <- paste0('../../output/01-reduced-corpora/legal/descriptive-', gsub('.*\\/', '', i))
  save(df, file = out)
  rm(df)
  rm(reg_matches)
}
