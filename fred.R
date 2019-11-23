library(tidyverse)
library(readxl)

fred<-read_xls('./raw_data/fred.xls')
summary(fred)

pca<- fred%>%
	  select('GS3M','GS6M','GS1','GS3', 'GS5','GS10')%>%
		
		prcomp(scale=T)

summary(pca)		
pca$rotation