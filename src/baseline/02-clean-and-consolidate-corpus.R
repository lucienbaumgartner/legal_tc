library(quanteda)
library(ggplot2)
library(dplyr)
library(pbapply)
library(tm)
library(rvest)

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# read in target adjective lists
kw1 <- read.table('../../input/dict-3.txt', stringsAsFactors = F, sep=',', header = T) %>% rename(TARGET = word)
kw2 <- read.table('../../input/descriptive_terms.txt', stringsAsFactors = F, sep=',', header = T) %>% rename(TARGET = word) %>% mutate(cat = 'descriptive')
kw <- rbind(kw1, kw2)

### load reddit corpus
load('../../output/02-finalized-corpora/baseline/reddit/new-reddit.RDS')
df <- reddit

### sentiment annotation
# load the sentiment dictionary
load('../../res/sentiWords-db.RDS')
# annotate conjoined adjectives
annot <- tokens(df$ADJ)
annot <- tokens_lookup(annot, dictionary = sentiWords$num)
annot <- sapply(annot, function(x) mean(as.numeric(unlist(x)),  na.rm = T))
df$sentiWords <- annot
rm(annot)
# annotate target adjectives
df <- left_join(df, kw)
annot <- tokens_lookup(tokens(unique(df$TARGET)), dictionary = sentiWords$dichot)
annot <- tibble(TARGET = unique(df$TARGET), TARGET_pol = sapply(annot, function(x) x[1]))
# manually fix NAs
annot <- annot %>% mutate(TARGET_pol = case_when(
  TARGET %in% c('arbitrary', 'illegal') ~ 'negative',
  TARGET %in% c('appropriate') ~ 'positive',
  TARGET %in% kw2$TARGET ~ 'neutral',
  TRUE ~ TARGET_pol))
df <- left_join(df, annot)

### filter out problematic metadata
dfx <- df
## get rid of entries containing a negative modifier-adverb
prblm <- c('too', 'not', 'less')
dfx <- dfx %>% filter(!(TARGET_mod%in%prblm | ADV%in%prblm))
# data loss
nrow(filter(df, TARGET_mod%in%prblm | ADV%in%prblm))/nrow(df)
## get rid of enries containing messy conjuncts
prblm <- c('most', 'many', 'more', 'non', 'other', 'last', 'overall', 'much', 'idk', 'holy', 'such')
dfx <- dfx %>% filter(!ADJ%in%prblm)
# data loss
nrow(filter(df, ADJ%in%prblm))/nrow(df)

# make sure that only AND-CCONJ are retained
table(dfx$CCONJ, useNA = 'always')

# get rid of entries without sentiment values for conjoined adjective
dfx <- filter(dfx, !is.na(sentiWords))

# overwrite old corpus
df <- dfx
rm(dfx)

### write out average conjoined sentiment for target adjectives
means <- df %>% group_by(TARGET, cat) %>% summarise(avg_conj_sentiment = mean(sentiWords, na.rm = T))
means <- left_join(means, annot)
means <- means %>% arrange(cat, TARGET_pol, desc(avg_conj_sentiment))
means
write.csv(means, file = '../../output/03-results/tables/BC_avg_conj_sentiment.csv', quote = F, row.names = F)

### compute diversity measures to decide which descriptive adjectives to keep
div_mes <- df %>% 
  group_by(TARGET) %>% 
  summarise(ADJ_full = paste0(ADJ, collapse = ' '),
            n = n())
res_div <- quanteda::tokens(div_mes$ADJ_full)
res_div <- textstat_lexdiv(res_div, measure = c("TTR", "CTTR", "K"))
res_div <- cbind(res_div, div_mes[, c('TARGET', 'n')])
res_div <- res_div %>% left_join(., kw) %>% left_join(., annot)
plot(res_div$TTR, res_div$n)
# write out results
res_div <- res_div %>% arrange(cat, TARGET_pol, desc(n), K)
write.csv(res_div, file = '../../output/03-results/tables/BC_diversity-analysis.csv', quote = F, row.names = F)

## get final list of target adjectives
f_res_div <- read.csv('../../output/03-results/tables/LC_final_list.csv', stringsAsFactors = F)
f_res_div <- res_div %>% filter(TARGET %in% f_res_div$TARGET)
# write out results
f_res_div <- f_res_div %>% arrange(cat, TARGET_pol, K)
f_res_div
write.csv(f_res_div, file = '../../output/03-results/tables/BC_final_list.csv', quote = F, row.names = F)

### filter the adjectives that are not needed
df <- filter(df, TARGET%in%f_res_div$TARGET)

### clean corpus text for vector space analysis
vec <- df$corpus
df$corpus_clean <- tolower(vec) %>% 
  gsub("<.*?>", ' ', .) %>% 
  removePunctuation() %>% 
  removeNumbers() %>% 
  removeWords(., stopwords('en')) %>% 
  stripWhitespace()

## save corpus
save(df, file = '../../output/02-finalized-corpora/baseline/reddit/BC_consolidated.RDS', compress = 'gzip')

