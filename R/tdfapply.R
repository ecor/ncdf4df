# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL
#' Apply a Function of a value vector in a data frame 
#' 
#' Apply a function to each value of the value field of the data frame conserving the other fields of the data frame using \code{\link{tapply}}
#' 
#' @param data data frame 
#' @param TimeField character string containg time field of \code{data}
#' @param ValueField character string containg time field of \code{data}
#' @param add_fields character string. Default is \code{c("month","year","season")}
#' @param FactorFields character string indicating the name of data frema fiels using as factors
#' @param FUN atomic function 
#' @param return.data.frame logical vaue. If it is \code{TRUE} the value is melted into a data rame. Default is \code{TRUE} 
#' @param ... further arguments for \code{\link{tapply}}
#' 
#' @export
#' @seealso \code{\link{tapply}},\code{\link{melt}},\code{\link{show.year.month.season}}
#' 
#' @examples 
#' 
#' library(ncdf4df)
#'## ncname <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data/trentino_hourlyweatherdata.nc"
#' ncname <- system.file("trentino/data/trentino_hourlyweatherdata.nc",package="ncdf4df")
#' nc <- nc_open(ncname)
#' meteoPrec <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE)
#' meteoPrec2 <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE,show_year=TRUE,show_month=TRUE)
#' nc_close(nc) 
#' 
#' MonthlyPrec <- tdfapply(data=meteoPrec,FactorFields=c("Station","month","year"),FUN=sum,na.rm=TRUE)
#' 
#' str(MonthlyPrec)
#' 




tdfapply <- function(data,TimeField="Time",ValueField="value",add_fields=c("month","year","season"),FactorFields=c("Station","month","variable"),FUN=sum,return.data.frame=TRUE,...){
	
	data <- show.year.month.season(data,TimeField=TimeField,add_fields=add_fields)
	
	FactorFields <-  FactorFields[FactorFields %in% names(data)]
	
	cond_season <- ("season" %in% FactorFields) & ("year" %in% FactorFields) & !("month" %in% FactorFields)
	
	if (cond_season) FactorFields[FactorFields=="year"] <- "year_of_season"
	
	if  (length(FactorFields)<=0) {
		
		data$factor <- array(1,nrow(data))
		FactorFields <- "factor"
	}
	
	if (is.character(FUN)) FUN <- get(FUN)
	out <- tapply(X=data[,ValueField],INDEX=data[,FactorFields],FUN=FUN,...)
	
	if (return.data.frame) out <- melt(out)
	
	
	
	
	
	
	
	return(out)
	
}
