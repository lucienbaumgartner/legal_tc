# !diagnostics off
library(quanteda)
library(ggplot2)
library(dplyr)
library(emmeans)
library(xtable)

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
