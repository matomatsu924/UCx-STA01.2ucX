################################################################################
#4 Mixed Effects Models
#4.1 Repeated measures and paired t-test
################################################################################

d <- read.csv("BPdata.csv")
#View(d)

m <- lm(BP~Group, data = d)
summary(m)

t.test(BP~Group, data=d, var.equal=T)


#Simple Mixed Effects Model
# Now, let’s add the participant ID into the mix as follows:

library(lme4)
library(lmerTest)
library(sjPlot)
library(tidyverse)

m <- lmer(BP~ Group + (1|PID), data=d)
summary(m)

plot_model(m,terms=c("Group"), type="eff") + theme_bw()

plot_model(m,terms=c("Group"), type="re") + theme_bw()
