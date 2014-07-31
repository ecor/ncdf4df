# TODO: Add comment
# 
# Author: ecor
###############################################################################

rm(list=ls())


library(geotopbricks)
library(reshape2)
library(ncdf4)
library(ncdf4df)
## source 
##source('/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/R/ncvar_put_values.R', chdir = TRUE)
### look at here http://www.unidata.ucar.edu/software/netcdf/docs/user_guide.html
wpath <- "/home/ecor/Dropbox/hydropica/sourcesR/geotopbricks_private/simulation_idroclima_template"

##### set meteo data

start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="A")

end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="A")

lat <- get.geotop.inpts.keyword.value("MeteoStationLatitude",numeric=TRUE,wpath=wpath)
lon <- get.geotop.inpts.keyword.value("MeteoStationLongitude",numeric=TRUE,wpath=wpath)


## set names ##
names_metadata_f <- "/home/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/metadata/station.csv"
names_metada <- read.table(names_metadata_f,header=TRUE,sep=",")
names <- names_metada$id


## set names ## 
names(lat) <- names #sprintf("X%04d",1:length(lat))
names(lon) <- names #sprintf("X%04d",1:length(lon))

###
###



nmeteo <- get.geotop.inpts.keyword.value("NumberOfMeteoStations",numeric=TRUE,wpath=wpath)
level <- 1:nmeteo
names(level) <- names 
#######
median_lat <- median(lat)
median_lon <- median(lon)

west <- which(lon<=median_lon)
east <- which(lon>median_lon)
south <- which(lat<=median_lat)
north <- which(lat>median_lat)

northwest <- intersect(north,west)
northeast <- intersect(north,east)

southwest <- intersect(south,west)
southeast <- intersect(south,east)

NW_indicator <- 0
NE_indicator <- 100
SE_indicator <-200
SW_indicator <- 300

### NORTHEAST
indicator <- NE_indicator
level <- level[northeast]
names <- names(level)
ncname <- "trentino_hourlyweatherdata_complete_NE"
########

precipitation_field <- "Iprec"
Date_field <- "Date"

#
lat <- lat[names]
lon <- lon[names]

tz <- "Etc/GMT+1"

start_date <- NULL # as.POSIXct("2005-04-01 00:00:00",tz=tz)
end_date <- NULL # as.POSIXct("2005-04-03 00:00:00",tz=tz)
meteo <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
		level=level,start_date=start_date,end_date=end_date)
names(meteo) <- names 
names(lat) <- names 
names(lon) <- names 
variables <- c("Iprec","AirT")


meteo <- lapply(X=meteo,FUN=function(x,id) {     
		str(x)
		time <- index(x)
		x <- as.data.frame(x)
		
		id <- id[id %in% names(x)]
		
	 	if (length(id)==1){
			
			x <- x[,id]
			x <- as.data.frame(x)
			names(x) <- id 
		} else {
			
			x <- x[,id]
		}
		
		
		
		x$Time <- as.character(time)
		
		
	    return(x)
	},id=variables)



meteo <- melt(meteo,id="Time")

names(meteo) [names(meteo)=="L1"] <- "idStation"


tz <-  "Etc/GMT+1"
meteo$Time <- as.POSIXct(meteo$Time,tz=tz)

####


time <- unique(meteo$Time)

time <- sort(time) 
tzn <- "+1"
tz <-  "Etc/GMTtz"
tz <- str_replace(tz,"tz",tzn)
## correct timezone

time <- as.character(time)
time <- as.POSIXct(time,tz=tz)

##
origin <- as.POSIXct("1970-01-01 00:00:00",tz=tz)
time_num <- as.numeric(difftime(time,origin,units="secs"))
meteo$Time_Sec <- as.numeric(difftime(meteo$Time,origin,units="secs"))
units_time <- "senconds since ORIGIN tz"
units_time <- str_replace(units_time,"ORIGIN",as.character(origin))
units_time_n <- str_replace(units_time,"tz",tzn) 

##

idst <- unique(meteo$idStation) 
dim_idst <- 1:length(idst)+indicator
names(dim_idst) <- idst
meteo$idStationNumber <- dim_idst[meteo$idStation]

## define dimensions 
MAXNAMELEN <- 200
nctime <- ncdim_def(name="Time",units=units_time_n,vals=time_num,unlim=TRUE)
ncid <- ncdim_def(name="Station", units="id",vals=dim_idst,unlim=TRUE)
ncchars <- ncdim_def(name="characters",units="",vals=1:MAXNAMELEN,create_dimvar=FALSE)
nclist <- list()

nclist$nclat <- ncvar_def(name="StationLat",units="degrees North",dim=ncid,missval=NA,prec='double')
nclist$nclon <- ncvar_def(name="StationLon",units="degrees East",dim=ncid,missval=NA,prec='double')
nclist$ncstid <- ncvar_def(name="StationIdName",units="",dim=list(ncchars,ncid),prec='char')
nclist$ncprec <- ncvar_def(name="Prec",units="millimeters",dim=list(ncid,nctime),missval=NA,prec='double',longname="Hourly Precipitation")
nclist$ncairtemp <- ncvar_def(name="Temp",units="degrees C",dim=list(ncid,nctime),missval=NA,prec='double',longname="Averaged Air Temperature")
## 

ncpath <- "/home/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data" 
 ###""/Users/ecor/Dropbox/iasma/RMAWGENdev/RSUBDAILY""/Users/ecor/Documents/workspace_new/activity_misc/RSUBDAILYCLIM/inst/trentino/data"
ncfilename <- paste(ncpath,ncname,sep="/")
ncfilename <- paste(ncfilename,"nc",sep=".")

### 
### Build Netcdf4 following the instruction on http://www.unidata.ucar.edu/software/netcdf/docs/build_hdf4.html
###
###
ncnew <- nc_create(ncfilename,nclist,force_v4=FALSE,verbose=TRUE)

## put variable values
ncvar_put(ncnew,nclist$ncstid,names(dim_idst))
ncvar_put(ncnew,nclist$nclat,lat)
ncvar_put(ncnew,nclist$nclon,lon)
ncvar_put_df_values(x=meteo,nameVar_df="AirT",
		varid=nclist$ncairtemp,nc=ncnew,
		nameDim_df=c("Time_Sec","idStationNumber"),
		variableField="variable",
		valueField="value",
		verbose=TRUE) 
ncvar_put_df_values(x=meteo,nameVar_df="Iprec",
		varid=nclist$ncprec,nc=ncnew,
		nameDim_df=c("Time_Sec","idStationNumber"),
		variableField="variable",
		valueField="value",
		verbose=TRUE) 			

nc_close(ncnew)
###
###
###








##





## Create netcdf archive 

#for ( i in level[2]) {
	
#	meteo <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
#         level=i,start_date=start,end_date=end)

			
	
	
	
#}



##look at here
#
###
## Define some straightforward dimensions
#x <- ncdim_def( "Lon", "degreesE", 0.5:359.5)
#y <- ncdim_def( "Lat", "degreesN", as.double(-89:89))
#t <- ncdim_def( "Time", "days since 1900-01-01", 1:10, unlim=TRUE)
#
## Make a variable with those dimensions.  Note order: time is LAST
#salinity <- ncvar_def("Salinity",    "ppt",  list(x,y,t), 1.e30 )
#
## Create a netCDF file with this variable
#ncnew <- nc_create( "salinity.nc", salinity )
#
#nc_close(ncnew)
#
## Now, illustrate some manipulations of the ncdim object.
#filename <- "salinity.nc"
#nc <- nc_open( filename )
#print(paste("File",filename,"contains",nc$ndims,"dimensions"))
#for( i in 1:nc$ndims ) {
#	print(paste("Here is information about dimension number",i,":"))
#	d <- nc$dim[[i]]
#	print(paste("    Name  :",d$name))
#	print(paste("    Units :",d$units))
#	print(paste("    Length:",d$len))
#	print("    Values:")
#	print(d$vals)
#	print(paste("    Unlimited:",d$unlim))
#}
#
#
