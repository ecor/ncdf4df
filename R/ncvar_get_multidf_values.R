# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL
#' More varibles in a data frame (get from netCDF)
#' 
#' It is a wrapper for 'ncvar_get_df_values' in case of more thsn one variables
#' 
#' @param x  vector of string containing variable names
#' @param ... arguments for \code{\link{ncvar_get_df_values}}
#' 
#' @export
#' 
#' 
#' @seealso \code{\link{ncvar_get_df_values}}
#' 






ncvar_get_multidf_values <- function(x,...){
	
	len <- length(x)
	
	out <- ncvar_get_df_values(x[1],...)
	
	if (len>1) {
		for (i in 2:len) {
			
			temp <- ncvar_get_df_values(x[1],...)
			out <- rbind(out,temp)
		}
			
		
	}	
	
	return(out)
	
	
}



