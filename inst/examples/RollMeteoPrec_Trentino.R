
rm(list=ls())
library(ncdf4df)
library(ggplot2)
library(climograph)

data(TrentinoClimate)
station <- unique(TrentinoClimate$station)
### Corretion from FEMst to FEM 
station <- str_replace(station,"FEM","FEMst")
nc <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data/trentino_hourlyweatherdata_complete_%s.nc"

parts <- c("NW","NE","SE","SW")

nc <- sprintf(nc,parts)
names(nc) <- parts
station <- list(NW=list(nc=nc["NW"],station=station),
		        NE=list(nc=nc["NE"],station=station),
		        SE=list(nc=nc["SE"],station=station),
				SW=list(nc=nc["SW"],station=station))
 
station <- lapply(X=station,FUN=function(x) {
			idname <- "StationIdName"

			
			out <- x 
			nc <- nc_open(x$nc)
			
			stationFound <- ncvar_get(nc,idname,verbose=TRUE)
			
			if (is.null(out$station)) out$station <- stationFound
			#### id 
			nc_close(nc)
		
			out$station <- out$station[out$station %in% stationFound]
			return(out)
		})

data <- lapply(X=station,FUN=function(x) {
	var <- "Prec"
	idname <- "StationIdName"
	nc <- nc_open(x$nc)
	out <- ncvar_get_df_values(nc=nc,x=var,verbose=TRUE)
	nc_close(nc)
	
	out <- out[out$Station %in% x$station,]
	
	
	return(out)
	
	})

data_n <- NULL

### it reates a single data frame !!!! 
for (it in data) {
	
	data_n <- rbind(data_n,it)
	
}
data <- data_n 
data_n <- NULL 
##data <- data[!is.na(data$value),]
rm(data_n)
width <- 24 #c(1,3,6,12,24) ## rolling windows of 1,3,12,24 time inidices, e.g. hour

rollPrec <- NULL 

for (it in width) { 
	print(it)
	temp <-  rolldfapply(data=data,width=it,FUN=sum,align="right") ## l
	temp$variable <- sprintf("RollPrec%02dh",it)
	
	rollPrec <- rbind(rollPrec,temp)
}
print("Creating plots")

## Uses tdfapply 

quantilemod <- function(x,na.rm=TRUE,...) { if (na.rm) { quantile(x,na.rm=na.rm,...) } else { NA   }}

q90 <- function(x,...) { na.rm <- !(length(which(is.na(x)))>=0.85*length(x)) ; quantilemod(x[x>0],probs=0.90,names=FALSE,na.rm=na.rm,...)}
q95 <- function(x,...) { na.rm <- !(length(which(is.na(x)))>=0.85*length(x)) ; quantilemod(x[x>0],probs=0.95,names=FALSE,na.rm=na.rm,...)}
q99 <- function(x,...) { na.rm <- !(length(which(is.na(x)))>=0.85*length(x)) ; quantilemod(x[x>0],probs=0.99,names=FALSE,na.rm=na.rm,...)}

meanp  <- function(x,...) { na.rm <- !(length(which(is.na(x)))>=0.85*length(x)) ; mean(x[x>0],na.rm=na.rm,...)}
medianp  <- function(x,...) { na.rm <- !(length(!which(is.na(x)))>=0.85*length(x)) ; median(x[x>0],na.rm=na.rm,...)}




fun <- "q99"## A lunedi!!!

MonthlyAggrRollPrec <- tdfapply(data=rollPrec,FactorFields=c("Station","month","year","variable"),FUN=fun)

##############################################################################
create_barplot <- function(data=MonthlyAggrRollPrec,variable=NA,station=c("T0129","T0099"),fun,dir,funname=NULL,wpath) {
	
	if (is.na(variable)) variable <- data$variable[1]
	data <- data[data$variable==variable,]
	###years <- range(data$year)
	if (is.null(funname)) funname <- as.character(body(fun))[2]
	if (is.character(fun)) funname <- fun 
	for (it in station) {
		df <- data[data$Station==it,]
		df <- df[!is.na(df$value),]
		years <- range(df$year)
		title <- paste(variable,it,funname,years[1],years[2],sep=" ")
		ylab <- "Precipitation Depth [mm]"
		xlab <- "Month"
		
		df$month <- factor(df$month)
		out <-  ggplot(df,aes(x=month,y=value))+geom_boxplot()+xlab(xlab)+ylab(ylab)+ggtitle(title)
		
		file_out <- paste(wpath,paste(variable,it,fun,sep="_"),sep="/")
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
station <- unique(MonthlyAggrRollPrec$Station)
create_barplot(data=MonthlyAggrRollPrec,station=station,fun=fun,wpath=wpath)
