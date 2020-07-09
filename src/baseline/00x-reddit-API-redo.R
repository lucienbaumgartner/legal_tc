library(dplyr)
library(pbmcapply)
library(httr)
library(lubridate)
library(anytime)
library(stringi)
library(rlist)
library(Hmisc)
rm(list=ls())

setwd('~/legal_tc/src/baseline/')
getwd()

search.terms <- read.table('../../input/dict-add.txt', header = T, stringsAsFactors = F, sep=',')
search.terms <- mutate(search.terms, word = c(paste0('%22', capitalize(search.terms$word), '%20and%22')))

subreddit <- '!legaladvice,!Advice'
range_end <- 100

Lurls <- lapply(search.terms$word, function(x){
  paste0(
    'https://api.pushshift.io/reddit/search/comment/?q=',
    x,
    '&',
    'subreddit=',
    subreddit,
    '&',
    'after=',
    (range_end+1):2,
    'd&before=',
    range_end:1,
    'd&sort=dsc&size=1000'
  )})

names(Lurls) <- search.terms$word

for(m in search.terms$word){
  urls <- Lurls[[m]]
  print(m)
  for(i in urls){
    print(i)
    #check <- today()-1-as.numeric(stri_extract_first_regex(i, '[0-9]+'))
    #check <- paste0(m, '_', check, '.RDS') %in% list.files('../output/raw/and_but/')
    #if(check) next
    response <- try(GET(i))
    if(!'try-error' %in% class(response)){
      response <- try(content(response))
      if(!'try-error' %in% class(response)&!identical(response, list())){
        df <- try(do.call(rbind, response$data) %>% as_tibble)
        if('try-error' %in% class(df)){
          selcols <- list.common(lapply(response$data, names))
          df <- lapply(response$data, function(x) x[selcols])
          df <- do.call(rbind, df) %>% as_tibble
        }
        utc <- unlist(df$created_utc)
        utc <- as.numeric(utc)
        utc <- na.omit(utc)
        utc <- anydate(utc)
        utc <- na.omit(unique(utc)[1])
        utc <- ifelse(is.null(utc), 'not_spec', utc)
        if(nrow(df)>0){
          out <- paste0('../../output/00-bulk-data/baseline/reddit/raw/', m, '_', utc, '.RDS')
          save(df, file = out)
        }
      }
    }
  }
}


file_paths <- list.files('../../output/00-bulk-data/baseline/reddit/raw/', full.names = T)
df <- pbmclapply(file_paths, function(x){
  load(x)
  return(df$body)
}, mc.cores = 4)
names(df) <- gsub('\\_.*','', list.files('../../output/00-bulk-data/baseline/reddit/raw/'))
for(i in unique(names(df))){
  print(i)
  dta <- unlist(df[grepl(i, names(df))])
  if(!is.null(dta)){
    names(dta) <- i
    save(dta, file = paste0('../../output/00-bulk-data/baseline/reddit/raw_aggr/', i, '.RDS'))
  }
}

