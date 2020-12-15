library(quanteda)
library(ggplot2)
library(dplyr)
library(purrr)
library(xtable)
library(envalysis)
library(gridExtra)

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

### compute aggregates for each corpus
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
write.csv(bc_summary, file = '../output/03-results/tables/bc_summary_stats.csv', quote = F, row.names = F)
bc_summary <- xtable(bc_summary)
print(bc_summary, include.rownames =F , file = '../output/03-results/tables/bc_summary_stats.tex')

### 
#### adj distribution
df <- rbind(bc, select(lc, -id, -year, -court)) %>% 
  mutate(cat = dplyr::recode(cat, epistemic = 'Epistemic', legal = 'Legal', tc = 'TC'),
         context = dplyr::recode(context, court = 'LC', reddit = 'BC'))
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
  df %>% filter(TARGET_pol == 'neutral') %>% 
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
