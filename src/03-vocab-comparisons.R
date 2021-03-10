# !diagnostics off
library(quanteda)
library(ggplot2)
library(dplyr)
library(emmeans)
library(xtable)
library(stargazer)

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# function for linebreaks in plots
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")
# load data
load('../output/02-finalized-corpora/baseline/reddit/BC_consolidated.RDS')
bc <- df %>% mutate(TARGET_pol_new = TARGET_pol)
load('../output/02-finalized-corpora/legal/LC_consolidated.RDS')
lc <- df
rm(df)
# combine to full corpus
df <- rbind(bc, select(lc, -id, -year, -court)) %>% 
  mutate(cat = dplyr::recode(cat, epistemic = 'Epistemic', legal = 'Legal', tc = 'TC'),
         context = dplyr::recode(context, court = 'LC', reddit = 'BC'))
# filter out descriptive target adjectives for study 1
df <- filter(df, !TARGET_pol == 'neutral')

sentiCalc <- unique(df[, c('sentiWords', 'ADJ')])
sentiCalc <- sentiCalc$sentiWords %>% setNames(., sentiCalc$ADJ)

# function for jaccard similarity
jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}
# setdiff
diff_coef <- function(a, b) {
  difference = length(setdiff(a, b))
  union = length(a) + length(b) - length(intersect(a, b))
  return (difference/union)
}
# average sentiment per function
jaccard_sent <- function(a, b) {
  intersection = intersect(a, b)
  return (mean(sentiCalc[intersection]))
}
# setdiff
diff_coef_sent <- function(a, b) {
  difference = setdiff(a, b)
  return (mean(sentiCalc[difference]))
}
# setdiff frequency
diff_freq <- function(a, b) {
  difference = setdiff(a, b)
  return (mean(sentiCalc[difference]))
}
data_jaccard <- df %>% group_by(context, TARGET_pol, cat, TARGET) %>% summarise(vec = list(unique(ADJ)))
data_freq <- df %>% group_by(context, TARGET_pol, cat, TARGET) %>% summarise(freq_vec = list(table(ADJ)))

res <- c()
for(i in unique(data_jaccard$TARGET)){
  res[i] <- jaccard(unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'BC']), 
          unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'LC']))
}
res
  
diff_bc_not_in_lc <- c()
for(i in unique(data_jaccard$TARGET)){
  diff_bc_not_in_lc[i] <- diff_coef(unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'BC']), 
                    unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'LC']))
}
diff_bc_not_in_lc

diff_lc_not_in_bc <- c()
for(i in unique(data_jaccard$TARGET)){
  diff_lc_not_in_bc[i] <- diff_coef(unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'LC']), 
                                    unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'BC']))
}
diff_lc_not_in_bc


sent_jaccard <- c()
for(i in unique(data_jaccard$TARGET)){
  sent_jaccard[i] <- jaccard_sent(unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'BC']), 
                    unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'LC']))
}
sent_jaccard


sent_diff_bc_not_in_lc <- c()
for(i in unique(data_jaccard$TARGET)){
  sent_diff_bc_not_in_lc[i] <- diff_coef_sent(unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'BC']), 
                                  unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'LC']))
}
sent_diff_bc_not_in_lc


sent_diff_lc_not_in_bc <- c()
for(i in unique(data_jaccard$TARGET)){
  sent_diff_lc_not_in_bc[i] <- diff_coef_sent(unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'LC']), 
                                    unlist(data_jaccard$vec[data_jaccard$TARGET == i & data_jaccard$context == 'BC']))
}
sent_diff_lc_not_in_bc


resframe <- unique(data_jaccard[, c('TARGET', 'cat', 'TARGET_pol')])
res <- cbind(resframe, 
             jaccard = res, 
             sentiJaccard = sent_jaccard,
             diff_bc_not_in_lc = diff_bc_not_in_lc,
             sentiDiff_bc_not_in_lc = sent_diff_bc_not_in_lc,
             diff_lc_not_in_bc = diff_lc_not_in_bc,
             sentiDiff_lc_not_in_bc = sent_diff_lc_not_in_bc)
res %>% mutate(check = jaccard + diff_bc_not_in_lc + diff_lc_not_in_bc)
m <- res %>% 
  group_by(cat, TARGET_pol) %>% 
  summarise(avg_jaccard = mean(jaccard), 
            avg_sentiJaccard = mean(sentiJaccard),
            'avg_BCnotinLC' = mean(diff_bc_not_in_lc),
            avg_sentiBCnotinLC = mean(sentiDiff_bc_not_in_lc),
            'avg_LCnotinBC' = mean(diff_lc_not_in_bc),
            avg_sentiLCnotinBC = mean(sentiDiff_lc_not_in_bc)
            )
mean(m$avg_jaccard)

res %>% 
  group_by(cat) %>% 
  summarise(avg_jaccard = mean(jaccard), 
            avg_sentiJaccard = mean(abs(sentiJaccard)),
            'avg_BCnotinLC' = mean(diff_bc_not_in_lc),
            avg_sentiBCnotinLC = mean(abs(sentiDiff_bc_not_in_lc)),
            'avg_LCnotinBC' = mean(diff_lc_not_in_bc),
            avg_sentiLCnotinBC = mean(abs(sentiDiff_lc_not_in_bc))
  )
