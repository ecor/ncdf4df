# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL
#' From netCDF variable to Data Frame
#' 
#' It puts valus of a data frame in a netCDF archive
#' 
#' @param x names of the variables to get
#' @param nc An object of class \code{ncdf4} (as returned by either function \code{\link{nc_open}} or function \code{\link{nc_create}}), indicating what netCDF file to read from.
#' @param start index vactor where to start the reading data.
#' @param count vector containing the data layers to be read. 
#' @param dimTime name of Time dimension. If it is declared (Default is \code{Time}), time is returned as \code{POSIXct}. To run this option, time is required to be expressed in seconds in the netCDF file.
#' @param show_month,show_year,show_season logical values. If \code{TRUE} month or year and season are shown respectively. 
#' @param dimStation TO DO 
#' @param ... further arguments for \code{\link{ncvar_get}}. 
#' 
## ncvar_get(nc, varid=NA, start=NA, count=NA, verbose=FALSE,
#  signedbyte=TRUE, collapse_degen=TRUE)
#' 
#' @export 
#' 
#' @seealso \code{\link{ncvar_put}},\code{\link{ncvar_get_multidf_values}}
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
ncvar_get_df_values <- function(x=c("Prec","Temp"),start=NA,count=NA,dimTime="Time",dimStation=list(c("Station","StationIdName")),show_month=FALSE,show_year=FALSE,show_season=FALSE,
						nc, ...){
					
	out <- NULL
	
	if (length(x)>1) {
		
		warning("x is a vector: only first element is considered. Use ncvar_multidf_values alternatively!!")
		x <- x[1]
	}	
	## work with dimensions
	dim <- nc$var[[x]]$dim
	names(dim) <- unlist(lapply(X=dim,FUN=function(x){x$name}))
	dim <- dim[length(dim):1]
	##str(dim)
	dimvars <- lapply(X=dim,FUN=function(x){ ncvar_get(nc=nc,varid=x,...)}) 
	## check startc countc 
	
	if (is.na(start)) start <- array(1,length(dim))
	if (is.na(count)) count <- unlist(lapply(X=dim,FUN=function(x) {x$len}))
	startc <- start[length(start):1]
	countc <-  count[length(count):1]
	##
#	startc <- start
#	countc <- count
#	if (!is.na(startc)) startc <- start[length(startc):1]
#	if (!is.na(countc)) countc <- count[length(countc):1]
	##
	
	for (i in 1:length(dimvars)) {
		
		ind <- start[i]-1+1:count[i]
		vect <- dimvars[[i]][ind]
		dimvars[[i]] <- vect
		
	}
	##
	
	
	
	
	
	v <- as.vector(ncvar_get(nc=nc,varid=x,start=startc,count=countc,...))
	
	
	names(start) <- names(dim)
	names(count) <- names(dim)
	out <- array(NA,c(length(v),length(dim)+2))
	out <- as.data.frame(out)
	names(out) <- c(names(dim),"value","variable")
	mass0 <- 1
	mass <- array(mass0,length(dim))
	names(mass) <- names(dim)
	
	for (c in names(dim)[length(dim):1]) { 
		
		mass[c] <- mass0
		mass0 <- mass0*count[c]
			
	}
	mass_total <- mass0
	
	for (c in names(dim)[length(dim):1]) {
	
		index <- rep(1:count[c],each=mass[c],len=nrow(out))+start[c]-1
		###print(index)
		out[,c] <- dimvars[[c]][index]
		
		
	}
	
	out$value <- v
	out$variable <- x
	if (is.null(dimTime)) dimTime <- NA 
	
	if (dimTime %in% names(dim)) {
		
			units <- ncatt_get(nc=nc,varid=dimTime,attname="units",...)$value
			tzstring <- "Etc/GMTtzn"
			
			units <- str_split(units," ")[[1]]	
			l <- length(units)
			
			tz <- str_replace(tzstring,"tzn",units[l])
			#print(units)
			#print(units[l-1])
		    ###tz <- tzmanager(tz=as.numeric(units[l]))
			origin <-  as.POSIXct(units[l-1],tz=tz)
			
			out[,dimTime] <- origin+out[,dimTime]
			attr(out,"tz") <-  tz
			
			
			if (show_month) {
				
				out$month <- as.numeric(format(out[,dimTime],"%m"))
			
			}
			if (show_year) {
				
				out$year <- as.numeric(format(out[,dimTime],"%Y"))
				
			}
			if (show_season) { 
			
				out$month <- as.numeric(format(out[,dimTime],"%m"))
				out$year <- as.numeric(format(out[,dimTime],"%Y"))
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
			
			
	} 
	
	if (!is.null(dimStation)) {
		
		if (!is.list(dimStation)) dimStation <- list(dimStation) 
		
		for (it in dimStation) {
			
			if(length(it)>=2) { 
				
				cond <- it[1] %in% names(out)
				if (cond==TRUE) {
					
					namefordimid <- ncvar_get(nc,varid=it[2])
					dimid <- paste("id",dimvars[[it[1]]],sep="_")
					dimid_df <- paste("id",out[,it[1]],sep="_")
					names(namefordimid) <- dimid
					#print(dimid)
					#print(dimvars[[it[1]]])
					dimidname <- namefordimid[dimid_df]
					
					out[,it[1]] <-  dimidname
					
				}
				
				
				
			}
			
			
			
		}
		
		
	}
	return(out)
}