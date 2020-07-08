library(jsonlite)
library(dplyr)
library(pbmcapply)
library(rvest)
library(tm)
library(stringr)
library(spacyr)
library(pbapply)
library(gtools)
rm(list=ls())

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

files <- list.files('/Users/lucienbaumgartner/Desktop/scotus/', full.names = T)

df <- pbmclapply(files, function(x){
  df <- try(fromJSON(x))
  if('try-error'%in%class(df)){
    write(x, file="out.txt", append=T)
    return(NULL)
  }else{
    id <- df$id
    if(ifelse(identical(!df$plain_text=="", logical(0)), F, !df$plain_text=="")){
      df <- df$plain_text
    }else if(ifelse(identical(!df$plain_html=="", logical(0)), F, !df$plain_html=="")){
      df <- df$html
    }else if(ifelse(identical(!df$html_with_citations=="", logical(0)), F, !df$html_with_citations=="")){
      df <- df$html_with_citations
    }else if(ifelse(identical(!df$html_lawbox=="", logical(0)), F, !df$html_lawbox=="")){
      df <- df$html_lawbox
    }else if(ifelse(identical(!df$html_columbia=="", logical(0)), F, !df$html_columbia=="")){
      df <- df$html_columbia
    }else  if(ifelse(identical(!df$html_harvard=="", logical(0)), F, !df$html_harvard=="")){
      df <- df$html_harvard
    }
    
    df_try <- try(read_html(df))
    if(!'try-error'%in%class(df_try)){
      df <- try(html_text(df_try))
      if('try-error'%in%class(df)){
        write(x, file="out.txt", append=T)
        return(NULL)
      }
    }
    return(list(id, df))
  }
}, mc.cores = 4)

df <- do.call(rbind, df)
df <- as_tibble(df)
names(df) <- c('id', 'txt')
if(length(unlist(df$id)) == nrow(df)) df <- mutate(df, id=unlist(id))
df <- filter(df, !lengths(txt)>1)
if(length(unlist(df$txt)) == nrow(df)) df <- mutate(df, txt=unlist(txt))

df <- mutate(df, txt = stripWhitespace(txt))
df <- mutate(df, year = gsub('.*(\\([0-9]{4}\\)).*', '\\1', txt))
#load('legal_corpus.RDS')
df <- mutate(df, year = gsub('\\(|\\)', '', year))
df <- mutate(df, year_num = as.numeric(year))
#reg <- paste0(paste0('.*', month.name, '\\s[0-9]+\\,\\s([0-9]{4}).*'), collapse = '|')
reg <- '.*\\s[0-9]+\\,\\s([0-9]{4}).*'
df <- mutate(df, year3 = ifelse(is.na(year_num), gsub(reg, '\\1', txt), year_num))
df <- mutate(df, year4 = as.numeric(year3))
df2 <- filter(df, is.na(year4))
#df2$txt[2]
#gsub('.*\\s[0-9]+\\,\\s([0-9]{4}).*', '\\1', df2$txt[1])
#gsub(reg, '\\1', df2$txt[1])
df <- select(df, -year, -year_num, -year3) %>% rename(year=year4)
save(df, file='legal_corpus.RDS')
load('legal_corpus.RDS')

format(object.size(df), units='Mb')

search.terms <- read.table('../input/dict_operation_pony.txt', header = T, stringsAsFactors = F, sep=',')
.lookup <- paste0(paste0('\\b', search.terms$word, '\\b'), '(\\,)?\\s(\\bbut\\b|\\band\\b)', collapse = '|')
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
save(df, file='legal_corpus2.RDS')
load('legal_corpus2.RDS')


df <- mutate(df, TARGET = strsplit(gsub('\\,', '', match), '\\s'))
df <- mutate(df, CCONJ = sapply(TARGET, '[[', 2))
df <- mutate(df, TARGET = sapply(TARGET, '[[', 1))
df <- mutate(df, comma = grepl('\\,', match))

#txtparsed2 <- spacy_parse(tolower(df2$corpus[190:200]), pos = TRUE)
#txtparsed2 <- split(txtparsed2, txtparsed2$doc_id, lex.order = F)
#txtparsed2 <- txtparsed2[mixedsort(names(txtparsed2))]
#txtparsed2 <- pbmclapply(txtparsed2, function(x) x$lemma %>% setNames(., x$pos), mc.cores = 4)

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
table(df$TARGET)
save(df, file='legal_corpus3.RDS')
load('legal_corpus3.RDS')
table(df$CCONJ)
head(df$corpus[df$CCONJ=='but'])
df2 <- filter(df, CCONJ=='but')
head(df2$corpus)

#txtparsed_adj2 <- do.call(rbind, txtparsed_adj) %>% as.data.frame() %>% mutate(id=rownames(.)) %>% as_tibble
#corpus2 <- tibble(corpus, id = paste0('text', 1:length(corpus)))
#corpus2 <- left_join(txtparsed_adj2, corpus2)
#write.table(corpus2, file = paste0('../output/curated/operation_pony/', subreddit,'/', gsub('\\..*', '', i), '.txt'), row.names = F, quote = T, sep = ',')
#save(corpus2, file = paste0('../output/curated/operation_pony/', subreddit,'/', gsub('\\..*', '', i), '.RDS'))

