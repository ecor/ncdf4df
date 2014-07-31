# TODO: Add comment
# 
# Author: ecor
###############################################################################

NULL 
#' Show 'year','month','season' 
#' 
#' 
#' Add a column indicating year,month or season 
#' 
#' 
#' 
#' 
#' @param data data frame 
#' @param TimeField character string containg time field of \code{data}
# @param ValueField character string containg time field of \code{data}
#' @param add_fields character string. Default is \code{c("month","year","season")}
#' 
#' 
#' @export
#' 

show.year.month.season <- function(data,TimeField="Time",add_fields=c("month","year","season")) {
	
	
	out <- data
	time <- data[,TimeField]
	
	
	if (length(time)<1) {
		
		warning("Nothing shown")
		return(out)				
		
		
	}
	
	show_month <- "month" %in% add_fields
	show_year <- "year" %in% add_fields
	show_season <- "season" %in% add_fields
	
	
	if (show_month) {
			
		out$month <- as.numeric(format(out[,TimeField],"%m"))
			
	}
	if (show_year) {
		
		out$year <- as.numeric(format(out[,TimeField],"%Y"))
		
	}
	if (show_season) { 
		
		out$month <- as.numeric(format(out[,TimeField],"%m"))
		out$year <- as.numeric(format(out[,TimeField],"%Y"))
		out$year_of_season <- out$year
		december <- 12
		out$year_of_season[out$month %in% december] <- out$year[out$month %in% december]+1
		out$season <- "NA"
		
		# winter
		out$season[out$month %in% c(12,1,2)] <- "DJF"
		# spring 
		out$season[out$month %in% c(3,4,5)] <- "MAM"
		# summer
		out$season[out$month %in% c(6,7,8)] <- "JJA"
		# autumn
		out$season[out$month %in% c(9,10,11)] <- "SON"
		
	}
		
	
	return(out)
	
} 

