library(quanteda)
library(ggplot2)
library(dplyr)
library(purrr)
library(xtable)
library(envalysis)
library(gridExtra)
library(pbmcapply)
library(tm)
library(textstem)
library(emmeans)
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# function for linebreaks in plots
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")
# function to compute quantiles per group in dplyr
p <- c(0.25, 0.5, 0.75)
p_names <- map_chr(p, ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
# function to compute diversity measures
k <- c('TTR', 'CTTR', 'K')
k_funs <- map(k, ~partial(textstat_lexdiv, measure = .x)) %>% 
  set_names(nm = k)

# load data
load('../output/02-finalized-corpora/baseline/reddit/BC_consolidated.RDS')
bc <- df
load('../output/02-finalized-corpora/legal/LC_consolidated.RDS')
lc <- df
rm(df)

### write out adjective list with aggregates
df <- rbind(mutate(bc, TARGET_pol_new = TARGET_pol), select(lc, -id, -year, -court)) %>% 
  mutate(cat = dplyr::recode(cat, epistemic = 'Epistemic', legal = 'Legal', tc = 'TC', descriptive = 'Descriptive'),
         context = dplyr::recode(context, court = 'LC', reddit = 'BC'))


### write out adjective list with aggregates
# quantiles
df_quants <- df %>% 
  group_by(cat, TARGET, TARGET_pol) %>% 
  summarize_at(vars(sentiWords), p_funs)
# average
df_avg <- df %>% 
  group_by(cat, TARGET, TARGET_pol) %>% 
  summarise(mean = mean(sentiWords, na.rm = T))
# diversity
df_div <- df %>% 
  group_by(cat, TARGET, TARGET_pol) %>% 
  summarize(conjuncts = paste0(ADJ, collapse = ' '),
            n = n())
df_div <- cbind(df_div, textstat_lexdiv(tokens(df_div$conjuncts), measure = c('TTR', 'CTTR', 'K')))
df_div <- select(df_div, -conjuncts, -document)
# combine
df_summary <- left_join(df_quants, df_avg) %>% left_join(., df_div) %>% arrange(cat, TARGET_pol, TARGET)
# write out results
write.csv(df_summary, file = '../output/03-results/tables/COMBINED_summary_stats.csv', quote = F, row.names = F)
df_summary <- xtable(df_summary)
print(df_summary, include.rownames =F , file = '../output/03-results/tables/COMBINED_summary_stats.tex')

## LC
# quantiles
lc_quants <- lc %>% 
  group_by(cat, TARGET_pol) %>% 
  summarize_at(vars(sentiWords), p_funs)
# average
lc_avg <- lc %>% 
  group_by(cat, TARGET_pol) %>% 
  summarise(mean = mean(sentiWords, na.rm = T))
# diversity
lc_div <- lc %>% 
  group_by(cat, TARGET_pol) %>% 
  summarize(conjuncts = paste0(ADJ, collapse = ' '), 
            n = n())
lc_div <- cbind(lc_div, textstat_lexdiv(tokens(lc_div$conjuncts), measure = c('TTR', 'CTTR', 'K')))
lc_div <- select(lc_div, -conjuncts, -document)
# combine
lc_summary <- left_join(lc_quants, lc_avg) %>% left_join(., lc_div)
# write out results
write.csv(lc_summary, file = '../output/03-results/tables/LC_summary_stats.csv', quote = F, row.names = F)
lc_summary <- xtable(lc_summary)
print(lc_summary, include.rownames =F , file = '../output/03-results/tables/LC_summary_stats.tex')

## BC
# quantiles
bc_quants <- bc %>% 
  group_by(cat, TARGET_pol) %>% 
  summarize_at(vars(sentiWords), p_funs)
# average
bc_avg <- bc %>% 
  group_by(cat, TARGET_pol) %>% 
  summarise(mean = mean(sentiWords, na.rm = T))
# diversity
bc_div <- bc %>% 
  group_by(cat, TARGET_pol) %>% 
  summarize(conjuncts = paste0(ADJ, collapse = ' '), 
            n = n())
bc_div <- cbind(bc_div, textstat_lexdiv(tokens(bc_div$conjuncts), measure = c('TTR', 'CTTR', 'K')))
bc_div <- select(bc_div, -conjuncts, -document)
# combine
bc_summary <- left_join(bc_quants, bc_avg) %>% left_join(., bc_div)
# write out results
write.csv(bc_summary, file = '../output/03-results/tables/BC_summary_stats.csv', quote = F, row.names = F)
bc_summary <- xtable(bc_summary)
print(bc_summary, include.rownames =F , file = '../output/03-results/tables/BC_summary_stats.tex')

### 
#### adj distribution
means <- df %>% group_by(context, TARGET_pol, cat, TARGET) %>% summarise(avg = mean(sentiWords, na.rm = T))
means_overall <- df %>% group_by(context, TARGET_pol, cat) %>% summarise(avg = mean(sentiWords, na.rm = T))
p1 <- 
  df %>% 
  filter(TARGET_pol == 'negative')  %>% 
  ggplot(., aes(x = TARGET, y = sentiWords, fill = context)) +
  geom_boxplot(outlier.color = NA) +
  geom_point(data = means %>% filter(TARGET_pol == 'negative'), aes(y = avg, color = context)) +
  geom_point(data = means %>% filter(TARGET_pol == 'negative'), aes(y = avg), shape = 1) +
  geom_hline(data = means_overall  %>% filter(TARGET_pol == 'negative'), aes(yintercept = avg, colour = context)) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  scale_fill_grey(start = 0.5, end = 0.8) +
  scale_colour_grey(start = 0.5, end = 0.8) +
  scale_y_continuous(limits = c(-1,1), expand = c(0.01,0.01)) +
  facet_grid(~ cat, scales = 'free_x') +
  labs(
    x = 'Target Adjective',
    y = 'Conjoined\nSentiment Values',
    fill = 'Corpus',
    colour = 'Corpus',
    title = abbrv(paste0('Negative Concepts'), width = 40)
  )
p2 <- 
  df %>% filter(TARGET_pol == 'positive') %>% 
  ggplot(., aes(x = TARGET, y = sentiWords, fill = context)) +
  geom_boxplot(outlier.color = NA) +
  geom_point(data = means %>% filter(TARGET_pol == 'positive'), aes(y = avg, color = context)) +
  geom_point(data = means %>% filter(TARGET_pol == 'positive'), aes(y = avg), shape = 1) +
  geom_hline(data = means_overall  %>% filter(TARGET_pol == 'positive'), aes(yintercept = avg, colour = context)) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  scale_fill_grey(start = 0.5, end = 0.8) +
  scale_colour_grey(start = 0.5, end = 0.8) +
  scale_y_continuous(limits = c(-1,1), expand = c(0.01,0.01)) +
  facet_grid(~ cat, scales = 'free_x') +
  labs(
    x = 'Target Adjective',
    y = 'Conjoined\nSentiment Values',
    fill = 'Corpus',
    colour = 'Corpus',
    title = abbrv(paste0('Positive Concepts'), width = 40)
  )
p3 <- 
  df %>% filter(cat == 'descriptive') %>% 
  ggplot(., aes(x = TARGET, y = sentiWords, fill = context)) +
  geom_boxplot(outlier.color = NA) +
  geom_point(data = means %>% filter(TARGET_pol == 'neutral'), aes(y = avg, color = context)) +
  geom_point(data = means %>% filter(TARGET_pol == 'neutral'), aes(y = avg), shape = 1) +
  geom_hline(data = means_overall  %>% filter(TARGET_pol == 'neutral'), aes(yintercept = avg, colour = context)) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  scale_fill_grey(start = 0.5, end = 0.8) +
  scale_colour_grey(start = 0.5, end = 0.8) +
  scale_y_continuous(limits = c(-1,1), expand = c(0.01,0.01)) +
  facet_grid(~ cat, scales = 'free_x') +
  labs(
    x = 'Target Adjective',
    y = 'Conjoined\nSentiment Values',
    fill = 'Corpus',
    colour = 'Corpus',
    title = abbrv(paste0('Descriptive Concepts'), width = 40)
  )
# set theme
theme_set(theme_light(base_size = 15))
theme_update(
  plot.title = element_text(face= 'bold', size = 15),
  axis.text.x = element_text(angle = 45, hjust = 1),
  strip.text = element_text(colour = 'black')
)
p <- grid.arrange(p2, p1, nrow =2)
ggsave(p, filename = '../output/03-results/plots/bc_lc_summary_stats_adj-distr.pdf', width = 8, height = 9)
ggsave(p3, filename = '../output/03-results/plots/lc_descriptive_adj-distr.pdf', width = 4, height = 3)

### write out overall sentiment analysis
load('../res/sentiWords-db-full-PoS.RDS')
head(sentiWords$num)
collection <- list()
for(i in c(lc, bc)){
  #i <-  bc
  set.seed(562647)
  p_source <- sample_n(i, 2000)
  p <- p_source$txt %>% setNames(., paste0('doc', 1:2000))
  p <- quanteda::tokens(tolower(p), remove_punct = T, remove_symbols = T, remove_numbers = T, remove_url = T, remove_separators = T)
  stopwords_regex = paste(tm::stopwords('en'), collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  p <- pbmclapply(p, function(x){
    #x <- p[[1]]
    tmp <- unlist(x) 
    tmp <- stringr::str_replace_all(tmp, stopwords_regex, '')
    tmp <- tmp[nchar(tmp) > 2]
    tmp <- tmp[!grepl('[0-9]+|[[:punct:]]', tmp, perl = T)]
    tmp <- textstem::lemmatize_words(tmp)
    tmp <- tmp[!tmp == '']
    return(tmp)
  }, mc.cores = 4)
  p <- pbmclapply(p, function(x){
    #x <- p[[1]]
    tmp <- quanteda::tokens_lookup(quanteda::tokens(x), dictionary = sentiWords$num)
    tmp <- unlist(tmp)
    tmp <- as.numeric(tmp[!tmp == 0])
    return(tmp)
  }, mc.cores = 4)
  save(p, file = '~/Downloads/senti2-tmp.RDS')
  length(p)
  p_source$doc_sentiment <- p
  head(p_source)
  #save(p_source, file='../output/02-finalized-corpora/baseline/reddit/BC_consolidated_full_sentiment.RDS', compress = 'gzip')
  save(p_source, file='../output/02-finalized-corpora/legal/LC_consolidated_full_sentiment.RDS', compress = 'gzip')
  collection <- append(collection, list(p_source))
}


load('../output/02-finalized-corpora/baseline/reddit/BC_consolidated_full_sentiment.RDS')
collection[[1]] <- p_source
load('../output/02-finalized-corpora/legal/LC_consolidated_full_sentiment.RDS')
collection[[2]] <- p_source

str(collection)
### MEANS by POLARITY
## means of means
means_lc_per_doc <- lapply(collection[[1]]$doc_sentiment, mean)
means_bc_per_doc <- lapply(collection[[2]]$doc_sentiment, mean)
mean(unlist(means_lc_per_doc))
mean(unlist(means_bc_per_doc))
## significance test
data_m1 <- rbind(tibble(sentiment = unlist(means_lc_per_doc), corpus = 'lc'),
            tibble(sentiment = unlist(means_bc_per_doc), corpus = 'bc'))
m1 <- lm(sentiment ~ corpus, data = data_m1)
emmeans(m1, specs = pairwise ~ corpus)

## means overall
mList_lc <- unlist(collection[[1]]$doc_sentiment)
mean(mList_lc)
mList_bc <- unlist(collection[[2]]$doc_sentiment)
mean(mList_bc)
## significance test
data_m2 <- rbind(tibble(sentiment = mList_lc, corpus = 'lc'),
                 tibble(sentiment = mList_bc, corpus = 'bc'))
m2 <- lm(sentiment ~ corpus, data = data_m2)
emmeans(m2, specs = pairwise ~ corpus)

### ABSOLUTE MEANS (INTENSITY)
## means of means
abs_means_lc_per_doc <- lapply(collection[[1]]$doc_sentiment, function(x) mean(abs(x)))
abs_means_bc_per_doc <- lapply(collection[[2]]$doc_sentiment, function(x) mean(abs(x)))
mean(unlist(abs_means_lc_per_doc))
mean(unlist(abs_means_bc_per_doc))
## significance test
data_m3 <- rbind(tibble(sentiment = unlist(abs_means_lc_per_doc), corpus = 'lc'),
                 tibble(sentiment = unlist(abs_means_bc_per_doc), corpus = 'bc'))
m3 <- lm(sentiment ~ corpus, data = data_m3)
emmeans(m3, specs = pairwise ~ corpus)

## means overall
mList_lc <- unlist(collection[[1]]$doc_sentiment)
mean(abs(mList_lc))
mList_bc <- unlist(collection[[2]]$doc_sentiment)
mean(abs(mList_bc))
## significance test
data_m4 <- rbind(tibble(sentiment = abs(mList_lc), corpus = 'lc'),
                 tibble(sentiment = abs(mList_bc), corpus = 'bc'))
m4 <- lm(sentiment ~ corpus, data = data_m4)
emmeans(m4, specs = pairwise ~ corpus)
