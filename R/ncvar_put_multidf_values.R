# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL
#' More varibles in a data frame (put in netCDF)
#' 
#' It is a wrapper for 'ncvar_put_df_values' in case of more thsn one variables
#' 
#' @param x data frame
#' @param nameVar_df  vector of string containing variable names
#' @param varid list of \code{ncvar4} objects 
# @param add_stations logical value. If stations ids are expressed as character strings in \code{x} and then transformed to a numeric value for the corresponding netCDF dimension, and it is \code{TRUE} a new variable containing Station Name is created in the NetCDF archive file.    
# @param units,missval,prec,longname vector of respective arguments of \code{\link{ncvar_put_df_values}}
#' @param ... arguments for \code{\link{ncvar_put_df_values}}
#' 
#' @export
#' 
#' 
#' @seealso \code{\link{ncvar_put_df_values}}
#' 

# add_stations=TRUE




ncvar_put_multidf_values <- function(x,nameVar_df=NULL,varid,...){
	
	
	
	if (is.null(nameVar_df)) nameVar_df <- names(varid)
	if (is.null(nameVar_df)) nameVar_df <- lapply(X=varid,FUN=function(x){x$name})
	
	

	len <- length(nameVar_df)
	
	if (length(varid)!=len) {
		variables <- paste(nameVar_df,collapse=";")
		message <- paste("Variable names",variables, "do not match with varid length")
		stop(message)
		
	}
	

	out <- list()
	for (i in 1:len)  {
		
		out[[i]] <- ncvar_put_df_values(x=x,nameVar_df=nameVar_df[i],
				varid=varid[[i]],...)
	}
		
	
	return(out)
	
	
}



