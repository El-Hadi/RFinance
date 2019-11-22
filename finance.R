#suppressMessages(library(quantmod, quietly=T))
suppressMessages(library(tidyverse, quietly=T))
suppressMessages(library(PerformanceAnalytics, quietly=T))
suppressMessages(library(tidyquant))
suppressMessages(library(quantmod))
suppressMessages(library(tibbletime))
suppressMessages(library(timetk))
suppressMessages(library(highcharter))
suppressMessages(library(scales))
library(htmlwidgets)
options(warn=-1, message=-1)
args=commandArgs(trailingOnly=T)

if(length(args)==0){

	stop("un arguments doit être fournie", call.=F)
}else{
#print(args)
options("getSymbols.yahoo.warning"=F)
options("getSymbols.warning4.0"=F)

#Obtension des cours du portfeuille.
y<-length(args)/2
x=length(args)

sym<-args[1:y]
weight<-as.vector(as.numeric(args[-(1:y)]))
#print(str(weight))
#ajouter ici un test pour les cas où le nombre de symbol et nombre de poids sont differents
#ajouter ici test sur somme des poids
#ajouter ici un test pour faire la sauvegarde

prices<-getSymbols(sym,
			auto.assign=T,
			from="2009-01-02",
			warnings=F) %>%
			map(~Ad(get(.))) %>%
			reduce(merge)%>%
			`colnames<-`(sym)

prices%>%
as_tibble()%>%
write_tsv('port.tsv') 
#calcul des prix mensuelles

prices_monthly<-to.monthly(prices,
							indexAt="lastof",
							OHLC=F)			


Return_xts<-Return.calculate(prices_monthly,
							method='log') %>%
							na.omit()%>%
							round(.,4)
							


#print(class(Return_xts))
Return_port_xts<-Return.portfolio(Return_xts,
								weights=weight,
								rebalance_on="months")%>%
								`colnames<-`("Returns")
								
print(tail(Return_port_xts))

print(100*round(StdDev(Return_xts, weights=weight),4))






Return_tidy<-prices_monthly%>%
			data.frame(date=index(.))%>%
			remove_rownames()%>%
			gather(asset,prices,-date)%>%
			group_by(asset)%>%
			mutate(returns=log(prices)-log(lag(prices)))%>%
			select(-prices)%>%
			spread(asset,returns)%>%
			select(date,sym)%>%
			na.omit()
Return_tidy_long<-Return_tidy%>%
				 gather(asset,returns,-date)

Return_port_tidy<-Return_tidy_long%>%
				 group_by(asset)%>%
				 mutate(weight=case_when(asset==sym[1]~weight[1],asset==sym[2]~weight[2]),
				weighted_return=returns*weight)%>%			  						  
				group_by(date)%>%
				summarise(returns=sum(weighted_return))			  						  
				  						  
				  						 
SD_port_tidy<-Return_port_tidy%>%
			  summarise(sd=sd(returns))

print(tail(Return_port_tidy))
print(100*round(SD_port_tidy,4))
			
Return_tq<-prices%>%
			tk_tbl(preserve_index=T,rename_index="date")%>%
			
gather(asset,prices,-date)%>%
group_by(asset)%>%
tq_transmute(mutate_fun=periodReturn,
				period="monthly",
				type="log",
				col_rename='returns')%>%
spread(asset, returns)%>%
select(date,sym)%>%
slice(-1)


Return_port_tq<-Return_tidy_long%>%
				tq_portfolio(
					assets_col=asset,
					returns_col=returns,
					weights=weight,
					col_rename="returns",
					rebalance_on="months"
				)

print(tail(Return_port_tq))


Return_tbl<-prices%>%
			tk_tbl(preserve_index=T,
					rename_index="date")%>%
			as_tbl_time(index=date)%>%
			as_period(period="month",
					  side="end")%>%
			gather(asset, prices,-date)%>%
			group_by(asset)%>%
			# tq_transmute(mutate_fun=periodReturn,
							# 
							# type="log",
							# col_rename='returns')%>%
			# 
			
			tq_transmute(mutate_fun=periodReturn,
							 type="log",
							 col_rename="returns")%>%
			spread(asset,returns)%>%
			select(date,sym)%>%
			slice(-1)

			
#print(head(Return_tbl))









# print(last(prices))
# print(first(prices))
# (last(prices)/first(prices)-1)*100
	#select(Close)%>%
	

#prices<-getSymbols(src='yahoo',


#					symbols, 
#					from='2017-12-31',
#					to='2019-08-31')%>%
#					map(~Ad(get(.)))%>%
#					reduce(merge)%>%
#					`colnames<-`(symbols)
#prices%>%
#head()
#prices_monthly<-to.monthly(
#	prices,
#	indexAt='lastof',
#	OHLC=F
#)
#prices_monthly%>%
#head()
#asset_return_xts<-Return.calculate(prices_monthly,
#						method='log')%>%
#						na.omit()
						
#asset_return_xts%>%
#as_tibble()%>%w
#write_tsv(x, path, na = "NA", append = FALSE, col_names = !append,
 # quote_escape = "double")
#write_tsv(.,'pea.csv',na="NA",append=F,  col_names=!append, quote_escape="double")
cov_matrix<-cov(Return_xts)


100*sqrt(t(weight)%*%cov_matrix%*%weight)
100*StdDev(Return_xts,weights= weight)
1
Return_port_tidy%>%
  summarize(std=100*sd(returns))

Return_port_tq%>%
	tq_performance(Ra=returns,
					Rb=NULL,
					performance_fun=table.Stats)%>%
					select(Stdev)%>%
					mutate(Stdev=100*Stdev)
					

ht<-highchart(type='stock')%>%
	hc_add_series(Return_port_xts)

saveWidget(ht,file='chart.html', selfcontained=F)
Return_port_tidy%>%
	ggplot(aes(x=date, y=returns))+
	geom_point(color="cornflowerblue")+
	scale_x_date(breaks=pretty_breaks(n=6))+
	ggtitle('returns')+
	theme(plot.title=element_text(hjust=0.5))	

}