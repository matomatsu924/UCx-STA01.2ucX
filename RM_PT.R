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


################################################################################

d <- read.csv("eucalyptus.csv")
d <- d %>% select(plot,stocking,spp,hgt)
#View(d)

library(lme4)
library(lmerTest)
library(performance)

d$stocking <- factor(d$stocking)

m0 <- lm(hgt~ spp*stocking, data=d)
step(m0)
m1 <- lmer(hgt~ spp*stocking + (1|plot), data=d)
step(m1)
m2 <- lmer(hgt~ spp+stocking + (1|plot), data=d)

anova(m0,m1,m2)
compare_performance(m0,m1,m2)

summary(m1)

ranef(m1) #Extraer el itercepto de los efectos aleatorios


plot_model(m1,terms=c("spp","stocking"), type="int") + theme_bw()
