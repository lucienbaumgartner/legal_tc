# !diagnostics off
library(quanteda)
library(ggplot2)
library(dplyr)
library(emmeans)
library(xtable)
library(factoextra)
library(Ckmeans.1d.dp)


rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# function for linebreaks in plots
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")
# load data
load('../output/02-finalized-corpora/legal/LC_consolidated.RDS')

# combine to full corpus
df <- df %>% 
  mutate(cat = dplyr::recode(cat, epistemic = 'Epistemic', legal = 'Legal', tc = 'TC', descriptive = 'Desc.'))

# differences between concept classes
m1 <- lm(abs(sentiWords) ~ cat, data = df)
emm1 <- emmeans(m1, specs = pairwise ~ cat)
res1 <- xtable(emm1$contrasts)
print(res1, include.rownames = F)

# K-Means Cluster Analysis
aggr <- df %>% 
  #mutate(group = paste0(TARGET, '_', TARGET_pol)) %>% 
  #group_by(group) %>% 
  group_by(cat, TARGET) %>% 
  summarise(means = mean(abs(sentiWords), na.rm =T),
            lex = paste0(ADJ, collapse = ' '))

res.mean <- Ckmeans.1d.dp(aggr$means, 8)
aggr <- cbind(aggr, res.mean = res.mean$cluster)

p <- ggplot(aggr, aes(x=means, y=TARGET)) +
  geom_text(aes(label = res.mean)) +
  scale_x_continuous(limits = c(-.5,.5)) +
  facet_grid(cat~., scales = 'free_y', space = "free_y", drop = T) +
  labs(
    y = 'Target Ajective',
    x = 'Average SentiWords Score of\nConjoined Adjectives',
    caption = abbrv('NTC: Neg. Thick Concepts; NVAC: Neg. Value-Assciated Concepts; NThin: Neg. Thin Concepts; DC: Descriptive Concepts; PThin: Pos. Thin Concepts; PVAC: Pos. Value-Associated Concepts; PTC: Pos. Thick Concepts.', width = 75)
  ) 
p

aggr$lex_div <- textstat_lexdiv(tokens(aggr$lex), measure = 'TTR')$TTR
aggr <- select(aggr, -lex) %>% as.data.frame()
row.names(aggr) <- aggr$group
res.kmeans <- eclust(aggr[, c('means', 'lex_div')], "kmeans", k=8, nstart = 25)
p <- fviz_cluster(res.kmeans,
                  main = 'Multivariate k-means Clustering (k=7)',
                  repel = T,
                  legend.title = 'Cluster') +
  theme_minimal() +
  guides(colour = F) +
  theme(plot.title = element_text(face='bold'))
p
