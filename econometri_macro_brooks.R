library(tidyverse)
library(readxl)
library(dygraphs)
library(car)

#mettre un repertoire de donnees et projets

macro <- read_excel("./raw_data/macro.xls")

macro<-macro %>% 
  mutate(dspread=c(NA, 100*diff(log(BMINUSA))),
         dcredit=c(NA, 100*diff(log(CCREDIT))),
         dprod=c(NA,100*diff(log(INDPRO))),
         dmoney=c(NA,100*diff(log(INDPRO))),
         dmoney=c(NA,100*diff(log(M1SUPPLY))),
         inflation=c(NA,diff(log(CPI))),
         rterm=c(NA,diff(USTB10Y-USTB3M)),
         dinflation=c(NA,100*diff(inflation)),
         rsandp=c(NA,100*diff(log(SANDP))),
         ermsoft=c(NA,100*diff(log(MICROSOFT)))-USTB3M/12,
         ersandp=rsandp-USTB3M/12
         )

lm_msoft <- lm(ermsoft~ersandp+dprod+dcredit+dinflation+dmoney+dspread+rterm, data=macro)
summary(lm_msoft)
linearHypothesis(lm_msoft, c("dprod=0","dcredit=0", "dspread=0", "dmoney=0"))

lm_start<-lm(ermsoft~1,data=macro[-2,])

summary(lm_start)
step(lm_start, direction="forward",scope=formula(lm_msoft))
