library(tidyverse)
library(readxl)
library(dygraphs)
library(car)



macro <- read_excel("./Téléchargements/macro.xls")

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
