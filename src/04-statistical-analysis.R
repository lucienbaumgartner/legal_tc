library(quanteda)
library(ggplot2)
library(dplyr)
library(pblapply)

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# function for linebreaks in plots
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")
kw <- read.table('../input/dict-2.txt', stringsAsFactors = F, sep=',', header = T) %>% rename(TARGET = word)

### load reddit data
load('../output/02-finalized-corpora/baseline/reddit/reddit.RDS')
reddit <- mutate(reddit, year = 2020, court = 'reddit')

### load legal data
fileslist <- list.files('../output/02-finalized-corpora/legal', full.names = T)
fileslist <- fileslist[!grepl('scotus', fileslist)]
df <- pblapply(fileslist[-length(fileslist)], function(x){
  load(x)
  df <- mutate(df, context = gsub('\\.RDS|.*\\/', '', x))
  return(df)
})
df <- do.call(rbind, df)

df <- cbind(df, reddit)
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
dfx <- dfx %>% filter(!(is.na(sentiWords)|is.na(CCONJ)))
annot <- tokens_lookup(tokens(unique(dfx$TARGET)), dictionary = sentiWords$dichot)
annot <- tibble(TARGET = unique(dfx$TARGET), TARGET_pol = sapply(annot, function(x) x[1]))
annot <- annot %>% mutate(TARGET_pol = ifelse(TARGET %in% c('arbitrary', 'illegal'), 'negative', TARGET_pol))
dfx <- left_join(dfx, annot)

dfx <- dfx %>% rowwise %>% mutate(group = paste0(cat, '_', TARGET_polarity))


## test ANOVA assumptions
# homogeneity of variances
levene.test(df$sentiWords, df$group, location = 'mean', trim.alpha=0.25, correction.method = 'correction.factor')
# normality
# per group
lapply(unique(df$group), function(x){shapiro.test(df$value[df$group==x])}) %>% setNames(., unique(df$group))
# global
shapiro.test(df$sentiWords)
# conclusion: non-parametric tests advised

### H1
m1 <- lm(abs(sentiWords) ~ context, data = dfx)
anova(m1)
emm1 <-  emmeans(m1, specs = pairwise ~ context, at = list(.group = c("reddit", "legal")))
emm1
emm1 <- pwpp(emm1$emmeans, method = "trt.vs.ctrl1", type = "response", side = ">")
emm1

### H1a & H1b
m2 <- lm(sentiWords ~ context * TARGET_pol, data = dfx)
anova(m2)
emm2 <- emmeans(m2, specs = pairwise ~ context, at = list(.group = c("reddit", "legal")), by = 'TARGET_pol')
emm2
emm2 <- pwpp(emm2$emmeans, method = "trt.vs.ctrl1", type = "response", side = ">")
emm2

### H2a & H2b
m3 <- lm(abs(sentiWords) ~ context * cat, data = dfx)
emm3 <- emmeans(m3, specs = pairwise ~ cat, at = list(.group = c("reddit", "legal")), by = 'context')
emm3
