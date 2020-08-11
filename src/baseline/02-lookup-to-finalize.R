library(dplyr)
library(stringr)
library(spacyr)
library(gtools)
library(tokenizers)
library(pbmcapply)
library(stringr)

rm(list=ls())
setwd('~/legal_tc/src/baseline/')

datasets <- list.files('../../output/01-reduced-corpora/baseline/reddit', full.names = T, pattern = 'new')
search.terms <- read.table('../../input/dict-add.txt', header = T, stringsAsFactors = F, sep=',')

syntax.regex <- '(ADV\\s)?ADJ\\s(PUNCT\\s)?CCONJ\\s(ADV\\s)?ADJ'
make_regex <- function(INDEX){
  TARGET = paste0('\\\\b',df$TARGET[INDEX], '\\\\b')
  LEMMA = paste0(txtparsed[[INDEX]], collapse = ' ')
  SYNTAX = paste0(names(txtparsed[[INDEX]]), collapse = ' ')
  SYNTAX <- unlist(str_extract(SYNTAX, syntax.regex))
  SYNTAX <- unique(SYNTAX)
  tmp <- str_replace_all(SYNTAX, c(
    'ADV' = '\\\\w+',
    'CCONJ' = 'and',
    'PUNCT' = '\\\\,'
  ))
  regex1 <- sub('ADJ', TARGET, tmp)
  regex1 <- str_replace_all(regex1, c(
    ' ' = '\\\\s',
    'ADJ' = '\\\\w+'
  ))
  regex2 <- stri_replace_last(tmp, replacement = TARGET, regex = 'ADJ')
  regex2 <- str_replace_all(regex2, c(
    ' ' = '\\\\s',
    'ADJ' = '\\\\w+'
  ))
  match1 <- unlist(str_extract_all(LEMMA, regex1))
  match2 <- unlist(str_extract_all(LEMMA, regex2))
  MATCH <- unlist(c(match1, match2))
  tryCatch(tmp <- lapply(MATCH, function(y){
    POS <- spacy_parse(y, pos = T)
    if(paste0(POS$pos, collapse = ' ') %in% SYNTAX){
      ADJ <- filter(POS, pos == 'ADJ' & !lemma == df$TARGET[INDEX])$lemma
      TARGET_mod <- filter(POS, (pos == 'ADJ' & lemma == df$TARGET[INDEX]) | pos == 'ADV')
      TARGET_mod <- TARGET_mod$lemma[TARGET_mod$pos == 'ADV' & TARGET_mod$token_id == TARGET_mod$token_id[TARGET_mod$pos == 'ADJ'] - 1]
      TARGET_mod <- ifelse(identical(TARGET_mod, character(0)), NA, TARGET_mod)
      
      ADV <- filter(POS, (pos == 'ADJ' & !lemma == df$TARGET[INDEX]) | pos == 'ADV')
      ADV <- ADV$lemma[ADV$pos == 'ADV' & ADV$token_id == ADV$token_id[ADV$pos == 'ADJ'] - 1]
      ADV <- ifelse(identical(ADV, character(0)), NA, ADV)
      
      first <- filter(POS, pos == 'ADJ')
      first <- ifelse(first$token_id[first$lemma == df$TARGET[INDEX]] < first$token_id[!first$lemma == df$TARGET[INDEX]], 1, 0)
      dta <- tibble(match = y, ADJ, TARGET_mod, ADV, first)
      return(dta)
    }
  }), warning = function(e) print(INDEX))
  if(is.list(tmp)) tmp <- do.call(rbind, tmp)
  if(is.list(tmp)|is.null(tmp)) return(tmp)
}


for(i in datasets){
  
  #i=datasets[1]
  load(i)
  print(i)
  df <- mutate(df, TARGET = strsplit(gsub('\\,', '', match), '\\s'))
  df <- mutate(df, CCONJ = sapply(TARGET, function(x) return(x[x %in% c('and', 'or')][1])))
  df <- mutate(df, TARGET = sapply(TARGET, function(x) return(x[x %in% search.terms$word][1])))
  df <- mutate(df, comma = grepl('\\,', match))
  
  txtparsed <- spacy_parse(tolower(df$corpus), pos = TRUE)
  txtparsed <- split(txtparsed, txtparsed$doc_id, lex.order = F)
  txtparsed <- txtparsed[mixedsort(names(txtparsed))]
  txtparsed <- pbmclapply(txtparsed, function(x) x$lemma %>% setNames(., x$pos), mc.cores = 4)
  
  system.time(txtparsed_adj <- pbmclapply(1:length(txtparsed), make_regex, mc.cores=4))
  #system.time(txtparsed_adj <- lapply(1:length(txtparsed), make_regex))
  
  #df <- rename(df, comma_check = comma, TARGET_check = TARGET, CCONJ_check = CCONJ)
  reps <- unlist(lapply(sapply(txtparsed_adj, nrow), function(x) ifelse(is.null(x), 0, x)))
  df <- df[rep(1:nrow(df), reps),]
  txtparsed_adj <- do.call(rbind, txtparsed_adj)
  #txtparsed_adj <- mutate(txtparsed_adj, id=df$id)
  #txtparsed_adj <- select(txtparsed_adj, -id)
  df <- rename(df, match_first = match)
  df <- cbind(df, txtparsed_adj)
  df <- as_tibble(df)
  #table(df$TARGET)
  df <- filter(df, TARGET%in%search.terms$word)
  #table(df$TARGET)
  out <- paste0('../../output/02-finalized-corpora/baseline/reddit/new-', gsub('.*\\/', '', i))
  save(df, file = out)
  
}

### generate full corpus
fileslist <- list.files('../../output/02-finalized-corpora/baseline/reddit', full.names = T, pattern = 'new\\-')
reddit <- pbmclapply(fileslist, function(x){
  load(x)
  return(df)
})

reddit <- do.call(rbind, reddit)
reddit <- as_tibble(reddit)
reddit <- mutate(reddit, context = 'reddit')

save(reddit, file = '../../output/02-finalized-corpora/baseline/reddit/new-reddit.RDS', compress = T)
