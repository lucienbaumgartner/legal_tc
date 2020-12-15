library(quanteda)
library(ggplot2)
library(dplyr)

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# function for linebreaks in plots
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")
load('../../output/02-finalized-corpora/baseline/reddit/BC_consolidated.RDS')
load('../../output/02-finalized-corpora/legal/LC_consolidated.RDS')

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
