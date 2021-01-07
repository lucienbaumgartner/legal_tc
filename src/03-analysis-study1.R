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

# create group variable
df <- mutate(df, group = paste0(cat, TARGET_pol, collapse = '_'))

## test ANOVA assumptions
# homogeneity of variances
leveneTest(df$sentiWords, df$group, location = 'mean', trim.alpha=0.25, correction.method = 'correction.factor')
# normality
# per group
lapply(unique(df$group), function(x){shapiro.test(df$value[df$group==x])}) %>% setNames(., unique(df$group))
# global
shapiro.test(df$sentiWords)
# conclusion: non-parametric tests advised

### H1
m1 <- lm(abs(sentiWords) ~ context, data = df)
anova(m1)
summary(m1)
emm1 <-  emmeans(m1, specs = pairwise ~ context, at = list(.group = c("BC", "LC")))
emm1
# write out results
res1 <- xtable(emm1$emmeans)
print(res1, include.rownames = F)
emm1 <- pwpp(emm1$emmeans, method = "trt.vs.ctrl1", type = "response", side = ">")
emm1

### H1a & H1b
m2 <- lm(sentiWords ~ context * TARGET_pol, data = df)
summary(m2)
emm2 <- emmeans(m2, specs = pairwise ~ context, at = list(.group = c("BC", "LC")), by = 'TARGET_pol')
emm2
m2a <- aov(sentiWords ~ context * TARGET_pol, data = df)
emmeans(m2a, specs = pairwise ~ context, at = list(.group = c("BC", "LC")), by = 'TARGET_pol')

# write out results
res2 <- xtable(emm2$emmeans)
print(res2, include.rownames = F)
emm2 <- pwpp(emm2$emmeans, method = "trt.vs.ctrl1", type = "response", side = ">")
emm2

### H2a & H2b
m3 <- lm(abs(sentiWords) ~ context * cat, data = df)
summary(m3)
stargazer(m3)
emm3 <- emmeans(m3, specs = pairwise ~ context, at = list(.group = c("Epistemic", "TC", "Legal")), by = 'cat')
emm3
res3 <- xtable(emm3$contrasts)
print(res3, include.rownames = F)
