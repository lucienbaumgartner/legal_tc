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

abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")

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
  mutate(perc = n/sum(n),
         total = sum(n))

nope <- c(
  'one', 'two', 'three', 'five', 'four',
  'fourth', 'second', 'fifth', 'first',
  'black', 'other', 'eight', 'non', 'eighth',
  'full'
)
vec <- df %>% 
  filter(! ADJ1 %in% nope) %>% 
  group_by(ADJ1) %>% summarise(n = sum(n)) %>% 
  top_n(n = 100, wt = n) %>% pull(ADJ1)

aggr <- aggr %>% ungroup %>% filter(ADJ1 %in% vec)
.lvl <- unique(aggr[, c('ADJ1', 'total')]) %>% arrange(desc(total))
aggr <- mutate(aggr, ADJ1 = factor(ADJ1, levels = .lvl$ADJ1))
yep <- aggr %>% filter(perc > .25) %>% pull(ADJ1) %>% unique
aggr <- mutate(aggr, var = ifelse(ADJ1 %in% yep, 0, 1))
yep <- unique(aggr[, c('ADJ1', 'var')])
p <- ggplot(aggr) +
  geom_rect(data = yep, aes(fill = factor(var)), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = .4) +
  geom_point(aes(y = perc, x = sentiWords)) +
  #geom_rug(aes(y = perc, x = sentiWords), sides="b") +
  geom_hline(aes(yintercept = .25), colour = 'red') +
  facet_wrap(~ADJ1) +
  scale_fill_manual(values = c(NA, '#00BFC4')) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = 'Share',
    fill = 'High Variance',
    title = abbrv('Overall Sentiment Variance for Most Frequent Adjectives Occuring in AND-Conjuctions'),
    subtitle = abbrv(paste0('Based on conjunctions occuring in random 2000 document-per-court samples. In total the random sampe consists of ', 
                            format(nrow(df), big.mark = "'"), 
                            ' adjective conjunctions.'))
  ) +
  theme(
    plot.title = element_text(face = 'bold')
  )

ggsave(p, filename = '../../output/03-results/plots/most-freq-adj.png', width = 15 , height = 10)

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

