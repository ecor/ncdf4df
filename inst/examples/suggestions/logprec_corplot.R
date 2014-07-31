# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL 
#'
#' 
#' @param prec_gen list of precipitation data frames 
#' @param Tx_gen,Tn_gen list of daily maximim and minimum temperature data frames
#' 
#' 
#' 
#' 


logcor.plot <- function(prec=prec_gen,Tx=Tx_gen,Tn=Tn_gen,station=station,origin="1961-01-01",valmin=0.5,xlab="xlab",ylab="ylab",title="tilte",...) {
	sample <- "seasonally"
	extract_cor_value <- function(...) {
		
		out <- cor.test(...)
		conf.int <- out$conf.int 
		estimate <- out$estimate 
		
		out <- c(conf.int[1],estimate,conf.int[2])
		names(out) <- c("min","estimate","max")
		df <- data.frame(value=out,label=names(out))
		out <- df 
		return(out)
		
	}
	
	log_prec <- lapply(X=prec,function(x,valmin){
				x[x<valmin] <- valmin 
				log(x)
			},valmin=0.5)
	
	cors <- applyDailyWeatherFunList(prec=log_prec,Tx=Tx,Tn=Tn,station=station,sample=sample,origin=origin,fun=applyCor,valmin=log(valmin),applyCorfun=extract_cor_value)
	
	
	cors <- melt(cors,id="label")
	names(cors) <- c("label","variable","value","season","station","configuration")
	

	index_gen <- (cors$configuration!="obs"& cors$label=="estimate") 
	
	cors_estimate <- cors[cors$label=="estimate",]
	cors_max <-      cors[cors$label=="max",]
	cors_min <-      cors[cors$label=="min",]
	names(cors_estimate)[names(cors_estimate)=="value"] <- "estimate"
	names(cors_max)[names(cors_max)=="value"] <- "max"
	names(cors_min)[names(cors_min)=="value"] <- "min"

	cors_estimate <- cors_estimate[,names(cors_estimate)!="label"]
	cors_min <- cors_min[,names(cors_min)!="label"]
	cors_max <- cors_max[,names(cors_max)!="label"]
	
	
	str(cors_estimate)
	cors_estimate$max <- cors_estimate$estimate+0.1
	cors_estimate$min <- cors_estimate$estimate-0.1
	
	##
	##
	out <-  qplot(configuration,estimate, data = cors_estimate, geom = "point",shape=configuration,asp=1)
	
	out <- out+facet_grid(season ~ station, scale = "fixed")+xlab(xlab)+ylab(ylab)+ggtitle(title)
	out <- out+geom_linerange(mapping=aes(x=configuration,ymax=max,ymin=min),data=cors_estimate)
	out <- out+scale_x_discrete(labels="")
	return(out)	
	
	
}


NULL 

temperature.wetdry.barplot <- function(prec=prec_gen,Tx=Tx_gen,Tn=Tn_gen,station,origin="1961-01-01",valmin=0.5,xlab="xlab",ylab="ylab",title="title",variable,...) {

	sample <- "seasonally"
    if (length(station)>1) station <- station[1]
	
	get.tx <- function(prec,Tx,Tn,valmin_prec=valmin) {
		
		index <- which(prec>valmin_prec)
		print(prec)
		print(Tx)
		out <- list(wet=Tx[index],dry=Tx[-index],all=Tx)
		
		return(out)
		
	}
	get.tn <- function(prec,Tx,Tn,valmin_prec=valmin) {
		
		index <- which(prec>valmin_prec)
		
		out <- list(wet=Tn[index],dry=Tn[-index],all=Tn)
		
		return(out)
		
	}
	get.dt <- function(prec,Tx,Tn,valmin_prec=valmin) {
		
		index <- which(prec>valmin_prec)
		
		out <- list(wet=Tx[index]-Tn[index],dry=Tx[-index]-Tn[-index],all=Tx-Tn)
		
		return(out)
		
	}
	out_tx <- applyDailyWeatherFunList(prec=prec,Tx=Tx_gen,Tn=Tn_gen,station=station,sample=sample,origin=origin,fun=get.tx,valmin=NA) 
	out_tn <- applyDailyWeatherFunList(prec=prec,Tx=Tx_gen,Tn=Tn_gen,station=station,sample=sample,origin=origin,fun=get.tn,valmin=NA) 
	out_dt <- applyDailyWeatherFunList(prec=prec,Tx=Tx_gen,Tn=Tn_gen,station=station,sample=sample,origin=origin,fun=get.dt,valmin=NA) 
	
	out_tx <- melt(out_tx)
	out_tn <- melt(out_tn)
	out_dt <- melt(out_dt)
	
	
	names(out_tx) <- c("value","state","season","configuration") 
	names(out_tn) <- c("value","state","season","configuration") 
	names(out_dt) <- c("value","state","season","configuration") 
	
	
	out_tx$variable <- "temperature_max"
	out_tn$variable <- "temperature_min"
	out_dt$variable <- "thermal_range"
	df <- rbind(out_tx,out_tn,out_dt)
	
	conf <- unique(as.character(df$configuration))
	##df <- df[df$station %in% station,]
	df <- df[df$variable==variable,]
	##
	out <- ggplot(df, aes(x=configuration,y=value))+geom_boxplot()+xlab(xlab)+ylab(ylab)+ggtitle(title)+scale_x_discrete(labels=(1:length(conf)))
	out <- out+facet_grid(season ~ state)
	return(out)
}