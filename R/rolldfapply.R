# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL
#' Wrapper for \code{\link{rollapply}}
#' 
#' Function \code{\link{rollapply}} applied to a Data Frame structured as follows:  \code{Time,Station,Variable,Value}
#' 
#' @param data data frame 
#' @param TimeField character string containg time field of \code{data}
#' @param ValueField character string containg time field of \code{data}
#' @param align argument of \code{\link{rollapply}}. Default value is here set to \code{right}.
#' @param ... arguments for \code{\link{rollapply}}
#' 
#' 
#' @examples 
#' library(ncdf4df)
#'## ncname <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data/trentino_hourlyweatherdata.nc"
#' ncname <- system.file("trentino/data/trentino_hourlyweatherdata.nc",package="ncdf4df")
#' nc <- nc_open(ncname)
#' meteoPrec <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE)
#' 
#' nc_close(nc)
#' 
#' width <- 3 ## rolling windows of 3 time inidices, e.g. hour 
#' meteoPrec3h <-  rolldfapply(data=meteoPrec,width=width,FUN=sum,align="right") ## l
#' 
#' 
#' @seealso \code{\link{rollapply}}
#' 
#' @return A data frame with the same format of \code{data} with "rolled" calculated  values. 
#' @export 
#' 
#' 
#' 
rolldfapply <- function(data,TimeField="Time",ValueField="value",align="right",...) {
	
	
	out <- data 
	out[,ValueField] <- NA
	
    otherfields <- names(data)[!(names(data) %in% c(TimeField,ValueField))]
	
	level <- NULL 
	for (it in otherfields) {
		
		level <- paste(level,data[,it],sep=";")
		
	}
	
	####out$level <- level
	
	levels <- unique(level) 
	
	for (j in levels) {
		
		rows <- which(level==j)
		
		x <- data[rows,]
		
		
		xzoo <- as.zoo(x[,ValueField])
		index(xzoo) <- x[,TimeField]
		outzoo <- xzoo*NA
		
		outv <- rollapply(xzoo,align=align,...)
		
		
		cond <- which(index(outzoo) %in% index(outv))
		
		outzoo[cond] <- outv
		
		out[rows,ValueField] <- as.vector(outzoo)
		
		## TO TEST 
		
		
		
	}
	
	####str(out)
	
	
	return(out)
}


