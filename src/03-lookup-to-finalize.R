library(stringr)
library(spacyr)
library(gtools)
library(tokenizers)

rm(list=ls())
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

datasets <- list.files('../output/01-reduced-corpora', full.names = T)
search.terms <- read.table('../input/dict.txt', header = T, stringsAsFactors = F, sep=',')

for(i in datasets){
  
  #i=datasets[1]
  load(i)
  df <- mutate(df, TARGET = strsplit(gsub('\\,', '', match), '\\s'))
  df <- mutate(df, CCONJ = sapply(TARGET, '[[', 2))
  df <- mutate(df, TARGET = sapply(TARGET, '[[', 1))
  df <- mutate(df, comma = grepl('\\,', match))
  
  txtparsed <- spacy_parse(tolower(df$corpus), pos = TRUE)
  txtparsed <- split(txtparsed, txtparsed$doc_id, lex.order = F)
  txtparsed <- txtparsed[mixedsort(names(txtparsed))]
  txtparsed <- pbmclapply(txtparsed, function(x) x$lemma %>% setNames(., x$pos), mc.cores = 4)
  
  #.lookup <- strsplit(gsub('\\\\b|\\(\\,\\)\\?', '', .lookup), '\\s') %>% unlist
  txtparsed_adj <- pbmclapply(1:length(txtparsed), function(INDEX){
    z <- txtparsed[[INDEX]]
    .lookup <- c(df$TARGET[INDEX], df$CCONJ[INDEX])
    windex <- grep(.lookup[1], z) # lookup the ADJ
    # if the word afterwards is the corresponding AND/BUT OR
    # comma followed by the corresponding AND/BUT
    windex <- windex[!is.na(z[windex + 2])]
    if(!any(is.na(windex)|is.null(windex))){
      if(any( z[windex + 1] == .lookup[2] | (z[windex + 1] == ',' & z[windex + 2] == .lookup[2]) )){
        tmp <- lapply(windex, function(y, pop=z){
          p <- ifelse(pop[y + 1] == ',' & pop[y + 2] == .lookup[2], 1, 0)
          if(y+ 2 +p <length(pop)){
            
            if(pop[y + 1 + p] == .lookup[2] & ( names(pop[y + 2 + p]) == 'ADJ' | ( names(pop[y + 2 + p]) == 'ADV' & ifelse('try-error'%in%class(try(names(pop[y + 3 + p]) == 'ADJ')), F, names(pop[y + 3 + p]) == 'ADJ') ) ) ){
              
              if(y-1>0){
                pop <- pop[(y-1):length(pop)]
              }else{
                pop <- c(NA, pop[y:length(pop)])
              }
              pop <- tryCatch({pop[1:grep('ADJ', names(pop))[2]]}, error = function(e) NULL)
              if(!is.null(pop)){
                pop <- tibble(TARGET = pop[2], CCONJ = pop[3 + p], ADV = ifelse(names(pop[4 + p]) == 'ADV', pop[4 + p], NA),
                              ADJ = ifelse(is.na(ADV), pop[4 + p], pop[5 + p]), modifier = ifelse(names(pop[1]) == 'ADV', pop[1], NA),
                              comma = p)
              }
              return(pop)
            }
            
          }
        })
        tmp <- do.call(rbind, tmp)
        return(tmp)
      }
    }
  }
  , mc.cores=4)
  
  df <- rename(df, comma_check = comma, TARGET_check = TARGET, CCONJ_check = CCONJ)
  reps <- unlist(lapply(sapply(txtparsed_adj, nrow), function(x) ifelse(is.null(x), 0, x)))
  df <- df[rep(1:nrow(df), reps),]
  txtparsed_adj <- do.call(rbind, txtparsed_adj)
  #txtparsed_adj <- mutate(txtparsed_adj, id=df$id)
  #txtparsed_adj <- select(txtparsed_adj, -id)
  df <- cbind(df, txtparsed_adj)
  df <- as_tibble(df)
  df <- filter(df, TARGET%in%search.terms$word)
  #table(df$TARGET)
  out <- paste0('../output/02-finalized-corpora/', gsub('.*\\/', '', i))
  save(df, file='../output/02-finalized-corpora/scotus.rda')
  
}
