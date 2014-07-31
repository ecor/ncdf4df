# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL 
#' Time Zone Manager 
#' 
#' This function calculated the difference between a time zone \code{tz} and Greenwhich Mean Time or viceversa.
#' 
#' @param tz time zone (string character or number)
#' @param returns.string lofical. If it is  \code{TRUE} (default), the return value is always coerced to a character string
#' @param when string character indicating a reference date (YYYY-MM-DD). See default. 
#' @param offset.sign offset signum. It can be \code{c(NA,-1,1)}. Default is \code{NA} and mans that is set to 1 except for the case where \code{tz} appears like \code{Etc/GMT+n} (where \code{n} is interpreted in an opposite way for most operative systems, see latest lines on \code{\link{timezones)}}) and is set -1.  


#' @seealso \code{\link{timezones)}},\url{http://en.wikipedia.org/wiki/ISO_8601#Dates}
#' @export 
#' @examples
#' 
#' tz <- Sys.timezone()
#' 
#' difftz <- tzmanager(tz=tz)
#' difftznum <- tzmanager(tz=tz,returns.string=FALSE)
#' 
#' tz_v2 <- tzmanager(tz=difftznum)
#' 
#' 
#' 


tzmanager <- function(tz=Sys.timezone(location = TRUE),returns.string=TRUE,when=as.character(Sys.Date()),offset.sign=NA) {
		  out <- NA
		  
		  
		  
		if (is.numeric(tz)) {
			
			tzs <- as.character(tz)
			if (tz>=0) tzs <- paste("+",tzs,sep="")
			
			tzs <- paste("Etc/GMT",tzs,sep="")
			out <- tzs
			
		} else if (is.character(tz)) {
			
			when <- as.character(when)
			
			cond <- str_detect(tz,"Etc/GMT") & is.na(offset.sign)
			
			if (cond) {
				
				offset.sign <- -1
			} else {
				
				offset.sign <- 1
			}
			tz0 <- "GMT"
			out <- -offset.sign*as.numeric(as.POSIXct(when,tz=tz)-as.POSIXct(when,tz=tz0),units="hours")
			
			if (returns.string) {
				
				if (out>=0) { 
					out <- paste("+",out,sep="")
				} else {
				
				out <- as.character(out)
				}
			}
			
		}

		return(out)

}

NULL 

#' Time Unit Manager
#' 
#' Convert time to POSIXct to a numeric value expressed in seconds
#' 
#' @param x a vector or 
#' @param unit mesarement unit with origin data and time zone. Default is \code{"seconds since 1970-01-01 +1"}
#' 
#' @export 
#' 
#' @examples 
#' 
#' today_sec <- timeconverter() 
#' 
#' today <- timeconverter(today_sec)
#' 
#' 
#' 

timeconverter <- function(x=as.POSIXct(Sys.time()),unit="seconds since 1970-01-01 +1") {
	
	out <- NULL
	
	unitl <- str_split(unit," ")[[1]]
	l <- length(unitl)
	tz <- tzmanager(as.numeric(unitl[l]))
	origin <- as.POSIXct(unitl[l-1],tz=tz)
	
	
	if (!is.numeric(x)) {
		
		out <- as.numeric(x-origin,units="secs") 
	} else {
		
#		x <- as.POSIXct(x,tz=tz,origin=origin)
	#	print(x)
	#	print(origin)
		out <- origin+x
		
	}
	
	
	
	return(out)
}
