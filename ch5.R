library(tidyverse)
library(car)
library(readxl)
library(dygraphs)
library(xts)
library(quantreg)

capm<-read_excel('./raw_data/capm.xls')

capm<-capm%>%
	mutate(rsandp=c(NA,100*diff(log(SANDP))),
			rford=c(NA,100*diff(log(FORD))),
			rge=c(NA, 100*diff(log(GE))),
			rmsoft=c(NA,100*diff(log(MICROSOFT))),
			roracle=c(NA,100*diff(log(ORACLE))),
			USTB3M=USTB3M/12,
			ersandp=rsandp-USTB3M,
			erford=rford-USTB3M)
#str(capm)
date<-as.Date(capm$Date, "%y-%m-%d")
#class(date)
capm_xts<-capm%>%
	xts(date)
#<-xts(capm, order.by='Date')
	
capm_xts%>%
	#select(rsandp,rford, Date)%>%
	#mutate(Date=as.Date(Date))%>%
	#xts(order.by='Date')%>%
	dygraph()
#dygraph(capm)
capm%>%
head()

lm_rford<-lm(erford~ersandp,data=capm)

#summary(lm_rford)

qreg<-rq(erford~ersandp, data=capm, tau=c(0.1,0.9,0.1))

summary(qreg)


plot(summary(qreg),level=0.95)