# TODO: Add comment
# 
# Author: ecor
###############################################################################


library(ncdf4df)
## ncname <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data/trentino_hourlyweatherdata.nc"
datadir <- system.file("trentino/data",package="ncdf4df")
ncname <- paste(datadir,"trentino_hourlyweatherdata.nc",sep="/")
ncname_rolled <- paste(datadir,"trentino_hourlyweatherdata_rolled_2.nc",sep="/")
##ncname <- system.file("trentino/data/trentino_hourlyweatherdata.nc",package="ncdf4df")
##ncname_rolled <- system.file("trentino/data/trentino_hourlyweatherdata_rolled.nc",package="ncdf4df")

nc <- nc_open(ncname)
meteoPrec <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE)

nc_close(nc)
### 
###
###

### It is assumed that precipitation are stored with hourly frequency 
### No control about that was made in this script




tz <- attr(meteoPrec,"tz")
tzvalue <- tzmanager(tz)
origin <- as.POSIXct("1970-01-01",tz=tz)
time_units <- paste("seconds since",origin,tzvalue,sep=" ")


time_sec <- sort(unique(meteoPrec$Time))
time_sec <- as.numeric(time_sec-origin,units="secs")

##  Put Precipitation Value 
id <- sort(unique(meteoPrec$Station))
nctime <- ncdim_def(name="Time",units=time_units,vals=time_sec,unlim=TRUE)

if (!is.character(id)) {
	
	ncid <- ncdim_def(name="Station", units="id",vals=id,unlim=TRUE)
	ncchar <- NULL
} else {
	
	## define dimensions 
	MAXNAMELEN <- 200
	

	idn <- index(id) 
	names(idn) <- id 
	###meteoPrec$Station <- idn[meteoPrec$Station]
	
	ncid <- ncdim_def(name="Station", units="id",vals=idn,unlim=TRUE)
	ncchar <- ncdim_def(name="CHARS",units="",vals=1:MAXNAMELEN,create_dimvar=TRUE)
	ncStationNames <- ncvar_def(name="StationIdName",units="",dim=list(ncchar,ncid),prec='char')
	
#	nclist <- list()
	
#	ncid <- ncdim_def(name="Station", units="id",vals=index(id),unlim=TRUE)

	
}


width <- c(1,3,6,12,24) ## rolling windows of 3 time inidices, e.g. hour

precwdth_name <- paste(width,"hour_Precipitation",sep="-")
names(width) <- precwdth_name
precwdth_longname <- paste("Precipitation observed in the previous",width,"hours",sep=" ")

precroll <- rolldfapply(data=meteoPrec,width=width[1],FUN=sum,align="right") 
precroll$variable <- precwdth_name[1]

ncvar <- list()

for (it in width[-1]) {
	
	name <- precwdth_name[width==it]


	precroll_t <- rolldfapply(data=meteoPrec,width=it,FUN=sum,align="right") 
	precroll_t$variable <- name
	precroll <- rbind(precroll,precroll_t)
	
	
}

ncvarlist_rolled <- lapply(X=precwdth_name,FUN=function(name,dim){
			
			width <- str_split(name,"-")[[1]][1]
			longname <- paste("Precipitation_observed_in_the_previous",width,"hours",sep="_")
			units <- "millimiters"
			
			out <- ncvar_def(name=name,units=units,missval=NA,prec='double',dim=dim,longname=longname)
			return(out)
		},dim=list(ncid,nctime))


###ncvarlist_rolled[[length(ncvarlist_rolled)+1]]  <- ncvar_def(name="STRING",units="",prec='char',dim=ncchar)

ncnew <- nc_create(ncname_rolled,ncvarlist_rolled)

ncvar_put_multidf_values(x=precroll,nameVar_df=NULL,
		varid=ncvarlist_rolled,nc=ncnew,
		nameDim_df=c("Time","Station"),
		variableField="variable",
		valueField="value",
		verbose=TRUE)

nc_close(ncnew)
#meteoPrecwdith <-  lapply(X=width,FUN=function(x,meteoPrec,retuns.value=NULL,dim,nc) {
#			
#			
#			name <- paste(x,"hour Precipitation",sep="-")
#			longname <- paste("Precipitation observed in the previous",width,"hours",sep=" ")
#			units <- "millimiters"
#			names_dim <- unlist(lapply(X=dim,FUN=function(d){d$name}))
#			names_dim <- names_dim[length(names_dim):1]
#			
#			out <- rolldfapply(data=meteoPrec,width=x,FUN=sum,align="right") 
#			out$variable <- name
#			ncout <- ncvar_def(name=name,units=units,missval=NA,prec='double',dim=dim,longname=longname)
#			output <- ncvar_put_df_values(x=meteoPrec,nameVar_df=name,
#					varid=ncout,nc=nc,
#					nameDim_df=names_dim,
#					variableField="variable",
#					valueField="value",
#					verbose=TRUE)		
#					
#			if (is.null(return.value)) out <- output
#			return(out)
#		},meteoPrec=meteoPrec,dim=list(ncid,nctime),nc=ncnew)
#
#nc_close(ncnew)
#
### l
### Write the variable into a new netCDF archive file
#



#
#####time_unit <- paste("seconds since",origin,tzvalue,sep=" ")
#time_sec <- sort(unique(meteoPrec$Time))
#time_sec <- timecoverter(time_sec,time_unit)
#
#id <- sort(unique(meteoPrec$Station))
#nctime <- ncdim_def(name="Time",units=time_unit,vals=time_sec,unlim=TRUE)
#ncid <- ncdim_def(name="Station", units="id",vals=id,unlim=TRUE)
#
### Define variables to put in the netCDF ARICHIVE
#
#
#ncprec <- ncvar_def(name=prec_name,units=prec_units,dim=list(ncid,nctime),missval=NA,prec='double',longname=prec_longname)
#
#prec3_name <- paste(width,"hour Precipitation",sep="-")
#prec3_longname <- paste("Precipitation observed in the previous",width,"hours",sep=" ")
#ncprecrolled <- ncvar_def(name=prec3_name,units=prec_unit,dim=list(ncid,nctime),missval=NA,prec='double',longname=prec3_longname)
#ncnew <- nc_create(ncname_rolled,list(ncprec,ncprecrolled))
#ncvar_put_df_values(x=meteoPrec,nameVar_df=prec_name,
#		varid=ncprec,nc=ncnew,
#		nameDim_df=c("Time","Station"),
#		variableField="variable",
#		valueField="value",
#		verbose=TRUE)
#
#ncvar_put_df_values(x=meteoPrec3,nameVar_df=prec3_name,
#		varid=ncprecrolled,nc=ncnew,
#		nameDim_df=c("Time","Station"),
#		variableField="variable",
#		valueField="value",
#		verbose=TRUE)
#nc_close(ncnew)
