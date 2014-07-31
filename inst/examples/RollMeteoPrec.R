# TODO: Add comment
# 
# Author: ecor
###############################################################################

## 3 hour-rolled Precipitation 
rm(list=ls())
library(ncdf4df)
library(ggplot2)

ncname <- system.file("trentino/data/trentino_hourlyweatherdata_v2.nc",package="ncdf4df")
nc <- nc_open(ncname)
meteoPrec <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE)

nc_close(nc)

width <- 3 #c(1,3,6,12,24) ## rolling windows of 1,3,12,24 time inidices, e.g. hour

rollPrec <- NULL 
print("mark1")
for (it in width) { 
	print(it)
	temp <-  rolldfapply(data=meteoPrec,width=it,FUN=sum,align="right") ## l
	temp$variable <- sprintf("RollPrec%02dh",it)
	
	rollPrec <- rbind(rollPrec,temp)
}
print("mark2")

## Uses tdfapply 

q90 <- function(x,...) { na.rm <- (length(!which(is.na(x)))>=0.85*length(x)) ; quantile(x[x>0],probs=0.90,names=FALSE,na.rm=na.rm,...)}
q95 <- function(x,...) { na.rm <- (length(!which(is.na(x)))>=0.85*length(x)) ; quantile(x[x>0],probs=0.95,names=FALSE,...)}
q99 <- function(x,...) { na.rm <- (length(!which(is.na(x)))>=0.85*length(x)) ; quantile(x[x>0],probs=0.99,names=FALSE,...)}

meanp  <- function(x,...) { na.rm <- (length(!which(is.na(x)))>=0.85*length(x)) ; mean(x[x>0],...)}
medianp  <- function(x,...) { na.rm <- (length(!which(is.na(x)))>=0.85*length(x)) ; median(x[x>0],...)}




fun <- "q99" ## A lunedi!!!

MonthlyAggrRollPrec <- tdfapply(data=rollPrec,FactorFields=c("Station","month","year","variable"),FUN=fun,na.rm=TRUE)

##############################################################################
create_barplot <- function(data=MonthlyAggrRollPrec,variable=NA,station=c("T0129","T0099"),fun,dir,funname=NULL,wpath,ind="qXX") {
	
	if (is.na(variable)) variable <- data$variable[1]
	data <- data[data$variable==variable,]
	years <- range(data$year)
	if (is.null(funname)) funname <- as.character(body(fun))[2]
	if (is.character(fun)) funname <- fun 
	for (it in station) {
		
		title <- paste(variable,it,funname,years[1],years[2],sep=" ")
		ylab <- "Precipitation Depth [mm]"
		xlab <- "Month"
		df <- data[data$Station==it,]
		df$month <- factor(df$month)
		out <-  ggplot(df,aes(x=month,y=value))+geom_boxplot()+xlab(xlab)+ylab(ylab)+ggtitle(title)
		
		file_out <- paste(wpath,paste(variable,it,ind,sep="_"),sep="/")
		file_out <- paste(file_out,".pdf",sep="")
		
		pdf(file_out)
		print(out)
		dev.off()
		
		
	}
	###p <-  ggplot(df,aes(x=month,y=value))+geom_boxplot()
	out <- 0
	return(out)
	
	
}


##############################################################################
wpath <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/examples/output"
create_barplot(station=c("T0129","T0099"),fun=fun,ind="qXX",wpath=wpath)
