library(udpipe)
library(dplyr)
library(mcapply)
library(stringr)
library(stringi)
library(reshape2)
library(spacyr)
#library(rbenchmark)
library(quanteda)
library(data.table)
library(ggplot2)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load udpipe model
udmodel <-'/Users/lucienbaumgartner/legal_tc/input/english-ewt-ud-2.4-190531.udpipe'
udmodel <- udpipe_load_model(udmodel)

#iterators
fileslist <- list.files('/Volumes/INTENSO/legal_tc/output/00-bulk-data/legal', full.names = T)

for(i in fileslist){
  #i = fileslist[1]
  load(i)
  
  set.seed(123456)
  df <- df[sample(1:nrow(df), 2000),]
  
  dfx <- pbmclapply(1:nrow(df),
                    function(x){
                      tmp <- df$txt[x]
                      tmp <- spacyr::spacy_parse(tmp)
                      #tmp <- udpipe::udpipe_annotate(udmodel, tmp)
                      #tmp <- as.data.frame(tmp)
                      tmp$phrase_tag <- as_phrasemachine(tmp$pos, type = "upos")
                      #tmp$phrase_tag <- as_phrasemachine(tmp$xpos, type = "penn-treebank")
                      nounphrases <- 
                        keywords_phrases(tmp$phrase_tag, term = tmp$token, 
                                         pattern = "(M)*A(,|O)*C(,|O)*(M)*A",
                                         is_regex = TRUE, detailed = T, 
                                         ngram_max = 6)
                      nounphrases <- nounphrases %>% arrange(start, desc(ngram)) %>% filter(!(duplicated(start)|duplicated(end)))
                      nounphrases <- mutate(nounphrases, doc_id = x)
                      rm(tmp)
                      return(nounphrases)
                    }, mc.cores = 6
  )
  
  dfx <- do.call(rbind, dfx)
  dfx <- filter(dfx, grepl('\\band|but\\b', keyword))
  dfx <- filter(dfx, !grepl('[0-9]+', keyword))
  df <- mutate(dfx, 
                 keyword = tolower(keyword),
                 CCONJ = str_extract(keyword, '\\band\\b|\\bbut\\b'),
                 comma = str_extract(keyword, '\\,'))
  save(df, file = paste0('/Volumes/INTENSO/legal_tc/output/01-inductive-approach/', gsub('.*\\/', '', i)))
}


#iterators
fileslist <- list.files('/Volumes/INTENSO/legal_tc/output/01-inductive-approach', full.names = T)
# load the sentiment dictionary
load('../../res/sentiWords-db.RDS')

for(i in fileslist){
  #i = fileslist[1]
  print(i)
  load(i)
  
  annot <- strsplit(df$keyword, '\\s')
  nannot <- strsplit(df$pattern, '')
  for(k in 1:length(annot)) names(annot[[k]]) <- nannot[[k]]
  for(k in 1:length(annot)) annot[[k]] <- suppressMessages(data.frame(t(annot[[k]])))
  
  annot <- rbindlist(annot, fill=TRUE)
  annot <- select(annot, - grep('O', names(annot), value=T))

  df <- as_tibble(cbind(df, annot))
  df <- rename(df, ADJ1 = 'A', ADJ2 = 'A.1')
  names(df) <- gsub('\\.', '', names(df))
  df <- filter(df, !(is.na(ADJ1)|is.na(ADJ2)))
  df <- filter(df, !(M %in% c('too', 'not', 'less') | M1 %in% c('too', 'not', 'less') | M2 %in% c('too', 'not', 'less')))
  
  combos <- apply(df[, c('ADJ1', 'ADJ2')], 1, function(x) paste0(sort(x), collapse = "_"))
  combos <- table(combos)
  
  df <- tibble(ADJ1 = sapply(strsplit(names(combos), '_'), '[[', 1),
              ADJ2 = sapply(strsplit(names(combos), '_'), '[[', 2),
              n = combos,
              court = gsub('.*\\/|\\.RDS', '', i)
  )
  
  # annotate data
  annot <- tokens(df$ADJ2)
  annot <- tokens_lookup(annot, dictionary = sentiWords$num)
  #rm(sentiWords)
  annot <- sapply(annot, function(x) mean(as.numeric(unlist(x)),  na.rm = T))
  df$sentiWords <- annot
  
  save(df, file = paste0('/Volumes/INTENSO/legal_tc/output/01-inductive-approach-only-ADJ/', gsub('.*\\/', '', i)))
}

fileslist <- list.files('/Volumes/INTENSO/legal_tc/output/01-inductive-approach-only-ADJ', full.names = T)
df <- lapply(fileslist, function(x){
  load(x)
  return(df)
  })

df <- do.call(rbind, df)

aggr <- df %>% group_by(ADJ1, ADJ2) %>% 
  summarise(n = sum(n), sentiWords = unique(sentiWords)) %>% 
  mutate(perc = n/sum(n))

vec <- df %>% group_by(ADJ1) %>% summarise(n = sum(n)) %>% top_n(n = 10, wt = n) %>% pull(ADJ1)

aggr <- aggr %>% ungroup %>% filter(ADJ1 %in% vec)

ggplot(aggr, aes(y = perc, x = sentiWords, colour = ADJ1)) +
  geom_point() +
  facet_wrap(~ADJ1)

######## benchmarking
benchmark(
  'spacyR' = {
    pblapply(1:5,
             function(x){
               tmp <- df$txt[x]
               tmp <- spacyr::spacy_parse(tmp)
               #tmp <- udpipe::udpipe_annotate(udmodel, tmp)
               #tmp <- as.data.frame(tmp)
               tmp$phrase_tag <- as_phrasemachine(tmp$pos, type = "upos")
               #tmp$phrase_tag <- as_phrasemachine(tmp$xpos, type = "penn-treebank")
               nounphrases <- 
                 keywords_phrases(tmp$phrase_tag, term = tmp$token, 
                                  pattern = "(M)*A(,)*C(M)*A",
                                  is_regex = TRUE, detailed = T, 
                                  ngram_max = 6)
               nounphrases <- nounphrases %>% arrange(start, desc(ngram)) %>% filter(!(duplicated(start)|duplicated(end)))
               nounphrases <- mutate(nounphrases, doc_id = x)
               rm(tmp)
               return(nounphrases)
             })
  },
  'udpipe' = {
    pblapply(1:5,
             function(x){
               tmp <- df$txt[x]
               #tmp <- spacyr::spacy_parse(tmp)
               tmp <- udpipe::udpipe_annotate(udmodel, tmp)
               tmp <- as.data.frame(tmp)
               #tmp$phrase_tag <- as_phrasemachine(tmp$pos, type = "upos")
               tmp$phrase_tag <- as_phrasemachine(tmp$xpos, type = "penn-treebank")
               nounphrases <- 
                 keywords_phrases(tmp$phrase_tag, term = tmp$token, 
                                  pattern = "(M)*A(,)*C(M)*A",
                                  is_regex = TRUE, detailed = T, 
                                  ngram_max = 6)
               nounphrases <- nounphrases %>% arrange(start, desc(ngram)) %>% filter(!(duplicated(start)|duplicated(end)))
               nounphrases <- mutate(nounphrases, doc_id = x)
               rm(tmp)
               return(nounphrases)
             })
  },
  replications = 100,
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self")
)

