NULL
#' From Data Frame to netCDF variable
#' 
#' It puts valus of a data frame in a netCDF archive
#' 
#' @param x data frame
#' @param nameVar_df name of the variable in the Data Frame
#' @param varid netCDF variable. See  \code{\link{ncvar_put}}
#' @param nc netCDF archive. See \code{\link{ncvar_put}}.
#' @param nameDim_df names of the variable dimensions in the data frame.
#' @param invertDim logical value. If \code{inverseDim} is \code{TRUE} (Default) order dimensions is inverted respect to the dimension order set in \code{\link{ncvar_def}}. 
#' @param variableField field name of \code{x} containing variable names
#' @param valueField field name of \code{x} containing variable values.
#' @param TimeField name of Time dimension. 
# @param StationField name of Station data frame field and dimension.
# @param ncchar netCDF character dimension used to store string-type variables. 
# @param idStationNumber vector of Station ID Numbers. The names of this vector  
# @param add_stations logical value. If stations ids are expressed as character strings in \code{x} and then transformed to a numeric value for the corresponding netCDF dimension, and it is \code{TRUE} a new variable containing Station Name is created in the NetCDF archive file.    
#' @param units,missval,prec,longname see \code{\link{ncvar_def}}.
#' @param verbose logial value. Default is \code{FALSE}.
#' @param ... further arguments for \code{\link{ncvar_put}}. 
#' 
#' 
#' 
#' @export 
#' 
#' @seealso \code{\link{ncvar_put}},\code{\link{ncvar_get_df_values}},\code{\link{ncvar_put_multidf_values}}
#'
#' 
#' @examples 
#' 
#' library(ncdf4df)
#'## ncname <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data/trentino_hourlyweatherdata.nc"
#' datadir <- system.file("trentino/data",package="ncdf4df") 
#' ncname <- paste(datadir,"trentino_hourlyweatherdata.nc",sep="/") 
#' ncname_rolled <- paste(datadir,"trentino_hourlyweatherdata_rolled.nc",sep="/") 
#' ##ncname <- system.file("trentino/data/trentino_hourlyweatherdata.nc",package="ncdf4df")
#' ##ncname_rolled <- system.file("trentino/data/trentino_hourlyweatherdata_rolled.nc",package="ncdf4df")
#' 
#' nc <- nc_open(ncname)
#' meteoPrec <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE)
#' 
#' nc_close(nc)
#' 
#' width <- 3 ## rolling windows of 3 time inidices, e.g. hour 
#' meteoPrec3 <-  rolldfapply(data=meteoPrec,width=width,FUN=sum,align="right") ## l
#' 
#'  
#' ## Write the variable into a new netCDF archive file 
#' 
#' tz <- attr(meteoPrec,"tz")
#' tzvalue <- tzmanager(tz)
#' origin <- as.POSIXct("1970-01-01",tz=tz)
#' time_units <- paste("seconds since",origin,tzvalue,sep=" ")
#' time_sec <- sort(unique(meteoPrec$Time))
#' time_sec <- as.numeric(time_sec-origin,units="secs") 
#' 
#' id <- sort(unique(meteoPrec$Station))
#' 
#' nctime <- ncdim_def(name="Time",units=time_units,vals=time_sec,unlim=TRUE)
#' ncid <- ncdim_def(name="Station", units="id",vals=index(id),unlim=TRUE)
#' 
#' ## Define variables to put in the netCDF ARICHIVE 
#' 
#' prec_units <- "millimeters"
#' prec_name <- "Hourly Precipitation"
#' prec_longname <- "Precipitation observed in the previous hour"
#' ncprec <- ncvar_def(name=prec_name,units=prec_units,dim=list(ncid,nctime),missval=NA,prec='double',longname=prec_longname)
#' 
#' prec3_name <- paste(width,"hour Precipitation",sep="-")
#' prec3_longname <- paste("Precipitation observed in the previous",width,"hours",sep=" ")
#' ncprecrolled <- ncvar_def(name=prec3_name,units=prec_units,dim=list(ncid,nctime),missval=NA,prec='double',longname=prec3_longname) 
#' ncnew <- nc_create(ncname_rolled,list(ncprec,ncprecrolled))
#' 
#'
#' 
#' ncvar_put_df_values(x=meteoPrec,nameVar_df=prec_name,
#' 		varid=ncprec,nc=ncnew,
#' 		nameDim_df=c("Time","Station"),
#' 		variableField="variable",
#' 		valueField="value",
#' 		verbose=TRUE) 			
#' 
#' ncvar_put_df_values(x=meteoPrec3,nameVar_df=prec3_name,
#' 		varid=ncprecrolled,nc=ncnew,
#' 		nameDim_df=c("Time","Station"),
#' 		variableField="variable",
#' 		valueField="value",
#' 		verbose=TRUE)
#' 
#'  
#' nc_close(ncnew)
#' 


ncvar_put_df_values <- function(x,nameVar_df="AirTemperature",
								varid=NULL,nc, ###nclist$ncairtemp,nc,
								nameDim_df=c("Time_Sec","idStationNumber"),
							    invertDim=TRUE,
								variableField="variable",
								valueField="value",
								TimeField="Time",
						#		StationField="Station",
						#		ncchar=NULL,
						#		idStationNumber=NULL,
						#		add_stations=TRUE,
								units,missval=NA,prec=prec,longname=nameVar_df,verbose=FALSE,
								...) {
	
	       ### 
	##	   names_df <- names(x[,names(x) %in% c(variableField,valueField,TimeField)])
		   
	#	   isNumeric <- array(TRUE,ncol(x))
	#	   names(isNumeeric) <- names(x)
		   for (it in names(x)[!(names(x) %in% c(variableField,valueField,TimeField))]){
			   
			   isNumeric <- is.numeric(x[,it]) 
			   if (!isNumeric) {
				   val <- as.numeric(factor(x[,it]))
				   x[,it] <- val
				   warning <- paste("Column",it,"is not numeric, it is thus coerced to integer factors!!")
				   warning(warning)
			   }
			   
	  	   }
		   
		   
		##   if (is.na(varid[[1]])) varid <- NULL
#		   	if  (StationField %in% names(x))) {
#				
#					stations <- unique(x[,StationField]) 
#					if  (is.character(stations)) {
#						if (is.null(idStationNumber)) {
#						idStationNumber <- 1:length(stations)
#						names(idStationNumber) <- stations
#					} ## INSERIRE CONTROLLI QUI .... 
#						x[,StationField] <- idStationNumber[x[,StationField]]
#						
#						
#						
#					} else {
#						
#						add_stations <- FALSE
#					}
#					
#					if (add_stations) {
#						
#						isDimStation <- which(unlist(lapply(X=nc$dim,FUN=function(x,StationField){x$name==StationField},StationField=StationField)))
#						if (length(isDimStation)>0) { 
#							StationDim <- nc$dim[[isDimStation[1]]]
#							#str(StationDim) ## 
#							str(ncchar)
#							ncStationNames <- ncvar_def(name=paste(StationField,"Name",sep=""),units="",dim=list(ncchar,StationDim),prec='char',verbose=verbose)
#							print("ccc")
#							str(nc$dim)
#							str(ncStationNames)
#							
#							nc <- ncvar_add(nc,ncStationNames,indefine=FALSE,verbose=verbose)
#							print("cccaaa")
#							str(nc$dim)
#							ncvar_put(nc=nc,varid=ncStationNames,vals=stations)
#						}
#						
#						
#						
#					}
#					
#					
#					
#				
#			}
		
		   
		   if (is.null(varid)) {
			   
			
		     dim <- nc$dim[nameDim_df]
			 dim <- dim[length(dim):1]
			 
			 varid <- ncvar_def(name=nameVar_df,units=units,dim=dim,missval=missval,prec=prec,longname=longname)
		   
		   }
		   
	       if (nameVar_df %in% names(x)) {
			   
			   value <- x[,nameVar_df] 
			   
		   } else if (nameVar_df %in% x[,variableField]) {
			   cnt <- which(x[,variableField]==nameVar_df) 
			   x <- x[cnt,]
			   value <- x[,valueField]
			   
		   } else if(length(unique(x[,variableField]))==1) { 
			   value <- x[,valueField]
		   } else {
			   
			   stop("Error in argument nameVar_df not found in x data frame") 
		   }
	
		   dim <- varid$dim
		   if (invertDim) dim <- dim[length(dim):1] ## inversion of the order of dimesion
		   cond <- length(dim)==length(nameDim_df) 
		   if (!cond) {
			   
			   stop("Error: bad dimension correspondance betwwen nameVar_df and varid dimensions!")
		   }
		   
		   names(dim) <- unlist(lapply(X=dim,FUN=function(x){x$name}))
		   
		   if (!identical(names(dim),nameDim_df)) {
			   
			   n1 <- paste(names(dim),collapse=";")
			   n2 <- paste(nameDim_df,collapse=";")
			   message <- paste("Warning: mismatch between netCDF and df dimension names respectively: ",n1,n2,sep=" ")
			   warning(message)
		   }
		   
		   ## dimension data frame 
		   dim_df <- as.data.frame(x[,nameDim_df])
		
		   names(dim_df) <- nameDim_df
		   ### creates a list of dimension values
		   dim_vals <- lapply(X=dim,FUN=function(x){
					   out <- x$vals
					   names(out) <- index(out)
					   return(out)
				   })
		   
		   names(dim_vals) <- names(dim_df)
		   ## length of each dimension
		   dim_vals_len <- unlist(lapply(X=dim_vals,FUN=length))
		   names(dim_vals_len) <- names(dim_vals)
		   numDimensions <- length(dim_vals_len)
		   ## length of all dimensions together
		   len <- dim_vals_len[1]
		   if (numDimensions>1) {
			   for (i in 2:length(dim_vals_len)) { 
			   		
				   	len <- len*dim_vals_len[i]
				   
			   }
		   }	   
		   ## Initializa vals
		   
		   vals <- array(missval,len)
		   ## 
		   
		   if (TimeField %in% names(dim_df)) {
			   
			  
			 
			   unitt <- dim[[TimeField]]$units
			
			   dim_df[,TimeField] <- timeconverter(x=dim_df[,TimeField],unit=unitt)
			   
		   }
		   ## Calculate which indices are to be filled with data frame variable values 
		
		   dim_df_index <- x[,nameDim_df]
		  
		   dim_df_index[,] <- NA
		   
		   for (it in names(dim_df_index)) {
			   print(it)
			   v <- as.vector(dim_vals[[it]])
			   
			 
			   dim_df_index[,it] <- unlist(lapply(X=dim_df[,it],FUN=function(x,v) which(v==x),v=v))
	
		   }
		   
		   cind <- array(1,nrow(dim_df_index))
		   ##
		   mass <- 1 
		   
		  
		   for (it in nameDim_df[numDimensions:1]) {
			   index <- dim_df_index[,it]
			   cind <- cind+(index-1)*mass
			   mass <- mass*dim_vals_len[it]
			   
			   
		   }
		   
		   ##
		   
		   
		   #DA METTEREA POSTO 
	# 	   cind <- as.vector(as.matrix(dim_df_index) %*% as.matrix(dim_vals_len))
		  
	   	   str(value)
		   str(cind)
		   vals[cind] <- value
			
		   out <- ncvar_put(nc=nc,varid=varid,vals=vals,verbose=verbose,...)
		   
		  ### dim_vals_index <- unlist(lapply(X=dim_vals,FUN=function(x)))
		   
		   
		   
		   return(out)
		   
		   
	
}



