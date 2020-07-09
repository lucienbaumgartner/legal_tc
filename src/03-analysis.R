library(quanteda)
library(ggplot2)
library(dplyr)
library(pbmcapply)
library(lubridate)
library(viridis)
#library(ggwordcloud)
library(car)
#library(nortest)
#library(fastDummies)
library(pbapply)
library(tm)
library(scales)

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# function for linebreaks in plots
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")
kw <- read.table('../input/dict.txt', stringsAsFactors = F, sep=',', header = T) %>% rename(TARGET = word)

### load reddit data
load('../output/02-finalized-corpora/baseline/reddit/reddit.RDS')
reddit <- mutate(reddit, year = 2020)
# available as <reddit>

### load legal data
fileslist <- list.files('../output/02-finalized-corpora/legal', full.names = T)
df <- pbmclapply(fileslist, function(x){
  load(x)
  df <- mutate(df, context = gsub('\\.RDS|.*\\/', '', x))
  return(df)
}, mc.cores=4)
df <- do.call(rbind, df)
df <- select(df, -id)

# combine data
df <- rbind(df, reddit)
rm(reddit)
df <- as_tibble(df)
df <- mutate(df, year = as.numeric(year))
df <- filter(df, year > 1788 & year < 2021)
df <- filter(df, TARGET %in% kw$TARGET)

# load the sentiment dictionary
load('../res/sentiWords-db.RDS')
# annotate data
annot <- tokens(df$ADJ)
annot <- tokens_lookup(annot, dictionary = sentiWords$num)
#rm(sentiWords)
annot <- sapply(annot, function(x) mean(as.numeric(unlist(x)),  na.rm = T))
df$sentiWords <- annot
rm(annot)

# data treatment
prblm <- c('carefree', 'dishonest', 'dishonesty', 'impermanent', 'louder', 'rudeness', 'rudest', 'stupidity', 'stupidly', 'uncruel', 'unfair', 'unfriendly', 'unfunny', 'honesty', 'crude', 'selfishness', 'unselfish', 'asocial', 'antisocial', 'unsafe', 'unreasonable', 'irresponsible')
nrow(filter(df, TARGET%in%prblm))/nrow(df)
dfx <- df %>% filter(!TARGET%in%prblm)

prblm <- c('too', 'not', 'less')
nrow(filter(df, modifier%in%prblm))/nrow(df)
nrow(filter(df, modifier%in%c('so', 'very', 'really')))/nrow(df)
dfx <- dfx %>% filter(!modifier%in%prblm)
nrow(filter(df, ADV%in%prblm))/nrow(df)
dfx <- dfx %>% filter(!ADV%in%prblm)
prblm <- c('most', 'many', 'more', 'non', 'other', 'last', 'overall', 'much', 'idk', 'holy', 'such')
nrow(filter(df, ADJ%in%prblm))/nrow(df)
dfx <- dfx %>% filter(!ADJ%in%prblm)

df %>% filter(!is.na(CCONJ)) %>% filter(CCONJ%in%c('and', 'but')) %>% nrow/nrow(df)
dfx <- dfx %>% filter(!is.na(CCONJ)|CCONJ%in%c('and', 'but'))

# get aggregates
dfx <- left_join(dfx, kw)
#rm(sentiWords)
means <- dfx %>% group_by(TARGET, cat, CCONJ, context) %>% 
  summarise(sentiWords = mean(sentiWords, na.rm = T))
dfx <- dfx %>% filter(!(is.na(sentiWords)|is.na(cat)|is.na(CCONJ)))
annot <- tokens_lookup(tokens(unique(dfx$TARGET)), dictionary = sentiWords$dichot)
annot <- tibble(TARGET = unique(dfx$TARGET), TARGET_pol = sapply(annot, function(x) x[1]))
dfx <- left_join(dfx, annot)
means <- left_join(means, annot)
dfx <- dfx %>% 
  ungroup %>% 
  mutate(TARGET = factor(TARGET, levels = kw$TARGET))
means <- means %>% 
  ungroup %>% 
  mutate(TARGET = factor(TARGET, levels = kw$TARGET))

p <- ggplot(dfx, aes(y=sentiWords, x=context, fill=cat)) + 
  geom_hline(aes(yintercept=0), lty='dashed') +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(data = means, aes(y=sentiWords, colour=cat)) +
  geom_point(data = means, aes(y=sentiWords), shape=1) +
  facet_grid(~TARGET, scales = 'free_x', drop = T) +
  #scale_color_manual(values = rev(cols)) +
  #scale_fill_manual(values = rev(cols)) +
  guides(color = FALSE) +
  labs(
    title = 'Sentiment Distribution of Adjective Conjunctions',
    subtitle = abbrv(paste0('The data consists of ', 
                            format(as.character(length(dfx$TARGET[!is.na(dfx$sentiWords)])), big.mark = "'"),
                            ' TARGET ADJ + CCONJ + ADJ constructions from reddit comments and the SCOUTUS corpus (legal). The sample has been drawn randomly via the pushshift API. The boxes represent the quartiles, the whiskers +/-1.5*IQR, the horizontal line the median, and the dots the means.'
    ), width = 130),
    fill = 'category',
    y = 'sentiWords Score of conjoined adjectives\nfor lemma#pos:#a (adjectives)',
    caption = 
      abbrv(
        paste0('NUMBER OF NON-UNIQUE ADJ:   ',
               paste0(
                 names(table(dfx$TARGET[!is.na(dfx$sentiWords)])),
                 ': ',
                 format(as.character(table(dfx$TARGET[!is.na(dfx$sentiWords)]), big.mark = "'")),
                 collapse = '; '
               )
        )
        , width = 170)
  ) +
  theme(
    plot.title = element_text(face= 'bold'),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
p
ggsave(p, filename = '../output/03-results/plots/overview.png', width = 11, height = 6)


# get sample texts
set.seed(1234)
for(i in unique(dfx$TARGET)){
  tmp <- dfx %>% filter(TARGET == i, !context=='reddit')
  tmp <- sample_n(tmp, 20, replace=T)
  tmp <- tmp$corpus
  write.table(tmp, file = paste0('../output/03-results/lists/texts/', i, '.txt'))
}

# get top 30 adjective lists
for(i in unique(dfx$TARGET)){
  tmp <- dfx %>% filter(TARGET == i, !context=='reddit')
  tmp <- table(tmp$ADJ)
  tmp <- tmp[order(tmp, decreasing=T)]
  tmp <- tmp[1:30]
  write.table(tmp, paste0('../output/03-results/lists/adjectives/', i, '.txt'))
}

# timeline of sentiment
table(dfx$year)
summary(dfx$year)
aggr <- dfx %>% 
  filter(!is.na(sentiWords)) %>% 
  group_by(TARGET, year) %>% 
  summarise(mean = summary(sentiWords)[4], 
            lower = summary(sentiWords)[2],
            upper = summary(sentiWords)[5])
dfx2 <- dfx %>% filter(!is.na(sentiWords) & !year==2020)
means <- dfx %>% filter(year == 2020 & !is.na(sentiWords)) %>% group_by(TARGET) %>% summarise(avg = mean(sentiWords))
p <- ggplot(dfx2, 
       #aes(x = year, y = mean, ymin = lower, ymax = upper, colour = TARGET, fill = TARGET),
       aes(x = year, y = sentiWords, fill = TARGET)
       ) +
  geom_hline(yintercept = 0, lty = 'dashed') + 
  geom_hline(data = means, aes(yintercept = avg, colour = TARGET)) +
  geom_point(aes(colour = TARGET), alpha = .2) +
  geom_smooth(aes(colour = TARGET)) +
  facet_wrap(~TARGET) +
  guides(fill = FALSE, colour = FALSE) +
  theme(plot.title = element_text(face = 'bold')) +
  labs(
    title = 'Sentiment over time',
    subtitle = abbrv('Dots and smoothed line are based on legal data. The horizontal lines are the reddit averages from 2020')
  )
ggsave(p, filename = '../output/03-results/plots/sentiment_over_time.png', width = 11, height = 6)


