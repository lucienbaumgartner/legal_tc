library(quanteda)
library(ggplot2)
library(dplyr)
library(pbmcapply)
library(pbapply)
library(lubridate)
library(viridis)
#library(ggwordcloud)
library(car)
#library(nortest)
#library(fastDummies)
library(pbapply)
library(tm)
library(scales)
library(ggrepel)

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# function for linebreaks in plots
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")
kw <- read.table('../input/dict-2.txt', stringsAsFactors = F, sep=',', header = T) %>% rename(TARGET = word)

### load legal data
fileslist <- list.files('../output/02-finalized-corpora/legal', full.names = T)
fileslist <- fileslist[!grepl('scotus', fileslist)]
df <- pblapply(fileslist[-length(fileslist)], function(x){
  load(x)
  df <- mutate(df, context = gsub('\\.RDS|.*\\/', '', x))
  return(df)
})
df <- do.call(rbind, df)

# combine data
df <- as_tibble(df)
# filter the right years, the right target adjectives, and change context variable to a dummy
df <- mutate(df, year = as.numeric(year))
df <- filter(df, year > 1979 & year < 2021)
df <- filter(df, TARGET %in% kw$TARGET)
df <- mutate(df, context = ifelse(context == 'reddit', context, 'court'))

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
#prblm <- c('carefree', 'dishonest', 'dishonesty', 'impermanent', 'louder', 'rudeness', 'rudest', 'stupidity', 'stupidly', 'uncruel', 'unfair', 'unfriendly', 'unfunny', 'honesty', 'crude', 'selfishness', 'unselfish', 'asocial', 'antisocial', 'unsafe', 'unreasonable', 'irresponsible')
#nrow(filter(df, TARGET%in%prblm))/nrow(df)
#dfx <- df %>% filter(!TARGET%in%prblm)

dfx <- df

prblm <- c('too', 'not', 'less')
nrow(filter(df, TARGET_mod%in%prblm))/nrow(df)
nrow(filter(df, TARGET_mod%in%c('so', 'very', 'really')))/nrow(df)
dfx <- dfx %>% filter(!TARGET_mod%in%prblm)
nrow(filter(df, ADV%in%prblm))/nrow(df)
dfx <- dfx %>% filter(!ADV%in%prblm)
prblm <- c('most', 'many', 'more', 'non', 'other', 'last', 'overall', 'much', 'idk', 'holy', 'such')
nrow(filter(df, ADJ%in%prblm))/nrow(df)
dfx <- dfx %>% filter(!ADJ%in%prblm)

df %>% filter(!is.na(CCONJ)) %>% filter(CCONJ%in%c('and', 'but')) %>% nrow/nrow(df)
dfx <- dfx %>% filter(!is.na(CCONJ)|CCONJ%in%c('and', 'but'))

dfx <- filter(dfx, !is.na(sentiWords))

# get aggregates
dfx <- left_join(dfx, kw)
#rm(sentiWords)
#means <- dfx %>% group_by(TARGET, cat, CCONJ, context) %>% summarise(sentiWords = mean(sentiWords, na.rm = T))
#dfx <- dfx %>% filter(!(is.na(sentiWords)|is.na(cat)|is.na(CCONJ)))
dfx <- dfx %>% filter(!(is.na(sentiWords)|is.na(CCONJ)))
annot <- tokens_lookup(tokens(unique(dfx$TARGET)), dictionary = sentiWords$dichot)
annot <- tibble(TARGET = unique(dfx$TARGET), TARGET_pol = sapply(annot, function(x) x[1]))
annot <- annot %>% mutate(TARGET_pol = ifelse(TARGET %in% c('arbitrary', 'illegal'), 'negative', TARGET_pol))
dfx <- left_join(dfx, annot)
means <- dfx %>% group_by(TARGET, CCONJ) %>% summarise(sentiWords = mean(sentiWords, na.rm = T))
means <- left_join(means, annot)

dfx %>% group_by(TARGET, context) %>% summarise(n = n())

means <- means %>% 
  arrange(TARGET_pol, desc(sentiWords)) %>% 
  mutate(TARGET = factor(TARGET, levels = TARGET))
dfx <- dfx %>% mutate(TARGET = factor(TARGET, levels=means$TARGET))

#p <- ggplot(dfx, aes(y=sentiWords, x=context, fill=cat)) + 
p <- ggplot(dfx, aes(y=sentiWords, x=TARGET)) + 
  geom_hline(aes(yintercept=0), lty='dashed') +
  geom_boxplot(aes( fill=cat), outlier.shape = NA) + 
  #geom_point(data = means, aes(y=sentiWords, colour=cat)) +
  geom_point(data = means, aes(y=sentiWords)) +
  geom_point(data = means, aes(y=sentiWords), shape=1) +
  facet_grid(~TARGET_pol, scales = 'free_x', drop = T) +
  #scale_color_manual(values = rev(cols)) +
  #scale_fill_manual(values = rev(cols)) +
  guides(color = FALSE) +
  labs(
    title = 'Sentiment Distribution of Adjective Conjunctions',
    subtitle = abbrv(paste0('The data consists of ',
                            format(nrow(dfx), big.mark = "'"),
                            ' court opinions from the US Supreme Court (SCOTUS) and the US Court of Appeals (1st to 11th Circuit). The boxes represent the quartiles, the whiskers +/-1.5*IQR, the horizontal line the median, and the dots the means.'
    ), width = 130),
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
ggsave(p, filename = '../output/03-results/plots/overview-without-reddit.png', width = 11, height = 6)

# aggregated on groups
p <- ggplot(filter(dfx, !TARGET %in% c('adequate', 'cruel', 'arbitrary', 'fair')), aes(x = cat, y = sentiWords, fill = cat)) +
  geom_boxplot() +
  facet_wrap(~TARGET_pol)
p

### have a look at distrubution of adjectives
aggr <- dfx %>% 
  filter(context == 'court') %>% 
  mutate(mod_dummy = ifelse(is.na(TARGET_mod)&is.na(ADV), 0, 1)) %>% 
  group_by(TARGET, ADJ) %>% 
  summarise(n=n(), sentiWords = unique(sentiWords)) %>% 
  mutate(perc=n/sum(n))
ggplot(aggr, aes(x = sentiWords, y = perc)) +
  geom_point(alpha=.2) +
  geom_density_2d() +
  facet_wrap(~TARGET, scales = 'free_y')

p <- ggplot(dfx %>% 
         filter(context == 'court'), aes(x = year, y = sentiWords)) +
  geom_hline(aes(yintercept=0.25), lty = 'dashed') +
  geom_smooth(colour = 'black', fill='black', size = .5) +
  geom_density_2d(alpha = .2) +
  facet_wrap(~TARGET) +
  labs(
    title = 'Sentiment mapping over time',
    subtitle = 'Variance of the sentiment of the conjoined ADJ over time.'
  ) +
  theme(
    plot.title = element_text(face= 'bold')
  )
ggsave(p, filename = '../output/03-results/plots/sentiment_mapping_over_time.png', width = 11, height = 6)

aggr <- dfx %>% 
  filter(context == 'court') %>% 
  select(TARGET, ADJ) %>% 
  unique() %>% 
  group_by(TARGET) %>% 
  summarise(n_adj = n()) %>% 
  left_join(., dfx %>% 
              filter(context == 'court') %>% 
              group_by(TARGET) %>% 
              summarise(n_TARGET = n()))
ggplot(aggr, aes(y = n_adj, x = n_TARGET, label = TARGET)) +
  geom_point() +
  geom_text_repel()

aggr <- dfx %>% 
  filter(context == 'court') %>% 
  group_by(TARGET, ADJ) %>% 
  summarise(n=n()) %>% 
  mutate(perc=n/sum(n)) %>% 
  top_n(n=10, wt=perc) %>% 
  arrange(desc(perc))

p <- ggplot(aggr, aes(x = TARGET, y = perc)) +
  geom_point(size=.8) +
  geom_text_repel(aes(label = ADJ), size = 2, segment.size = .1) +
  geom_hline(aes(yintercept=0.25), lty = 'dashed') +
  facet_wrap(~TARGET, scales = 'free_x') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = NULL) +
  labs(
    x = '',
    y = '',
    title = 'Share of the top 10 conjoined ADJ on total instances of TARGET structures'
  ) +
  theme(
    plot.title = element_text(face= 'bold')
  )
ggsave(p, filename = '../output/03-results/plots/top10_conjoined_ADJ.png', width = 11, height = 6)

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
  #geom_point(aes(colour = TARGET), alpha = .2, shape = '.') +
  geom_smooth(aes(colour = TARGET)) +
  facet_wrap(~TARGET) +
  guides(fill = FALSE, colour = FALSE) +
  theme(plot.title = element_text(face = 'bold')) +
  labs(
    title = 'Sentiment over time',
    subtitle = abbrv('The smoothed lines are based on legal data. The horizontal lines are the reddit averages from 2020')
  )
p
ggsave(p, filename = '../output/03-results/plots/sentiment_over_time.png', width = 11, height = 6)

################ Quantitative Analysis
dfx <- mutate(dfx, TARGET_pol = ifelse(TARGET == 'illegal' & is.na(TARGET_pol), 'negative', TARGET_pol))
# test for homogenity of variance
bartlett.test(sentiWords ~ interaction(context,TARGET_pol), data = dfx)
car::leveneTest(sentiWords ~ context*TARGET_pol, data = dfx)
# test for normality in variables
set.seed(1234)
dfxC <- filter(dfx, context == 'court')
dfxR <- filter(dfx, context == 'reddit')
dfxi <- rbind(dfxC[sample(1:nrow(dfxC), 2500), c('sentiWords', 'context', 'TARGET_pol')], 
      dfxR[sample(1:nrow(dfxR), 2500), c('sentiWords', 'context', 'TARGET_pol')])
shapiro.test(dfxi$sentiWords)
plot(density(dfxi$sentiWords))
# both assumptions are violated --> Pairwise Wilcoxon
# here we sample per the minimum number of [TARGET, TARGET_pol, context] per this same group 
min_n <- dfx %>% group_by(TARGET, TARGET_pol, context) %>% summarise(n=n()) %>% .$n %>% min(.)
dfxi <- mutate(dfx, context_pol = paste0(context, '_', TARGET_pol))
dfxi <- dfxi %>% group_by(TARGET, TARGET_pol, context) %>% sample_n(min_n)
dfxi %>% group_by(context_pol) %>% summarise(n=n())
dfxi %>% group_by(TARGET, TARGET_pol, context) %>% summarise(n=n())
pairwise.wilcox.test(dfxi$sentiWords, dfxi$context_pol, p.adjust.method = "BH")


# find other adjectives worth investigatings
dfx %>% group_by(ADJ) %>% summarise(n=n()) %>% top_n(n=50, wt=n) %>% arrange(desc(n)) %>% print(n=200)

