\name{ncvar_put_df_values}
\alias{ncvar_put_df_values}
\title{From Data Frame to netCDF variable}
\usage{
ncvar_put_df_values(x, nameVar_df = "AirTemperature", varid = NULL, nc,
  nameDim_df = c("Time_Sec", "idStationNumber"), invertDim = TRUE,
  variableField = "variable", valueField = "value", TimeField = "Time",
  units, missval = NA, prec = prec, longname = nameVar_df,
  verbose = FALSE, ...)
}
\arguments{
  \item{x}{data frame}

  \item{nameVar_df}{name of the variable in the Data Frame}

  \item{varid}{netCDF variable. See
  \code{\link{ncvar_put}}}

  \item{nc}{netCDF archive. See \code{\link{ncvar_put}}.}

  \item{nameDim_df}{names of the variable dimensions in the
  data frame.}

  \item{invertDim}{logical value. If \code{inverseDim} is
  \code{TRUE} (Default) order dimensions is inverted
  respect to the dimension order set in
  \code{\link{ncvar_def}}.}

  \item{variableField}{field name of \code{x} containing
  variable names}

  \item{valueField}{field name of \code{x} containing
  variable values.}

  \item{TimeField}{name of Time dimension.}

  \item{units,missval,prec,longname}{see
  \code{\link{ncvar_def}}.}

  \item{verbose}{logial value. Default is \code{FALSE}.}

  \item{...}{further arguments for
  \code{\link{ncvar_put}}.}
}
\description{
It puts valus of a data frame in a netCDF archive
}
\examples{
library(ncdf4df)
## ncname <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data/trentino_hourlyweatherdata.nc"
datadir <- system.file("trentino/data",package="ncdf4df")
ncname <- paste(datadir,"trentino_hourlyweatherdata.nc",sep="/")
ncname_rolled <- paste(datadir,"trentino_hourlyweatherdata_rolled.nc",sep="/")
##ncname <- system.file("trentino/data/trentino_hourlyweatherdata.nc",package="ncdf4df")
##ncname_rolled <- system.file("trentino/data/trentino_hourlyweatherdata_rolled.nc",package="ncdf4df")

nc <- nc_open(ncname)
meteoPrec <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE)

nc_close(nc)

width <- 3 ## rolling windows of 3 time inidices, e.g. hour
meteoPrec3 <-  rolldfapply(data=meteoPrec,width=width,FUN=sum,align="right") ## l


## Write the variable into a new netCDF archive file

tz <- attr(meteoPrec,"tz")
tzvalue <- tzmanager(tz)
origin <- as.POSIXct("1970-01-01",tz=tz)
time_units <- paste("seconds since",origin,tzvalue,sep=" ")
time_sec <- sort(unique(meteoPrec$Time))
time_sec <- as.numeric(time_sec-origin,units="secs")

id <- sort(unique(meteoPrec$Station))

nctime <- ncdim_def(name="Time",units=time_units,vals=time_sec,unlim=TRUE)
ncid <- ncdim_def(name="Station", units="id",vals=index(id),unlim=TRUE)

## Define variables to put in the netCDF ARICHIVE

prec_units <- "millimeters"
prec_name <- "Hourly Precipitation"
prec_longname <- "Precipitation observed in the previous hour"
ncprec <- ncvar_def(name=prec_name,units=prec_units,dim=list(ncid,nctime),missval=NA,prec='double',longname=prec_longname)

prec3_name <- paste(width,"hour Precipitation",sep="-")
prec3_longname <- paste("Precipitation observed in the previous",width,"hours",sep=" ")
ncprecrolled <- ncvar_def(name=prec3_name,units=prec_units,dim=list(ncid,nctime),missval=NA,prec='double',longname=prec3_longname)
ncnew <- nc_create(ncname_rolled,list(ncprec,ncprecrolled))



ncvar_put_df_values(x=meteoPrec,nameVar_df=prec_name,
		varid=ncprec,nc=ncnew,
		nameDim_df=c("Time","Station"),
		variableField="variable",
		valueField="value",
		verbose=TRUE)

ncvar_put_df_values(x=meteoPrec3,nameVar_df=prec3_name,
		varid=ncprecrolled,nc=ncnew,
		nameDim_df=c("Time","Station"),
		variableField="variable",
		valueField="value",
		verbose=TRUE)


nc_close(ncnew)
}
\seealso{
\code{\link{ncvar_put}},\code{\link{ncvar_get_df_values}},\code{\link{ncvar_put_multidf_values}}
}

