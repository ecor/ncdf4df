\name{ncvar_get_df_values}
\alias{ncvar_get_df_values}
\title{From netCDF variable to Data Frame}
\usage{
ncvar_get_df_values(x = c("Prec", "Temp"), start = NA, count = NA,
  dimTime = "Time", dimStation = list(c("Station", "StationIdName")),
  show_month = FALSE, show_year = FALSE, show_season = FALSE, nc, ...)
}
\arguments{
  \item{x}{names of the variables to get}

  \item{nc}{An object of class \code{ncdf4} (as returned by
  either function \code{\link{nc_open}} or function
  \code{\link{nc_create}}), indicating what netCDF file to
  read from.}

  \item{start}{index vactor where to start the reading
  data.}

  \item{count}{vector containing the data layers to be
  read.}

  \item{dimTime}{name of Time dimension. If it is declared
  (Default is \code{Time}), time is returned as
  \code{POSIXct}. To run this option, time is required to
  be expressed in seconds in the netCDF file.}

  \item{show_month,show_year,show_season}{logical values.
  If \code{TRUE} month or year and season are shown
  respectively.}

  \item{dimStation}{TO DO}

  \item{...}{further arguments for
  \code{\link{ncvar_get}}.}
}
\description{
It puts valus of a data frame in a netCDF archive
}
\examples{
library(ncdf4df)
## ncname <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data/trentino_hourlyweatherdata.nc"
ncname <- system.file("trentino/data/trentino_hourlyweatherdata.nc",package="ncdf4df")
nc <- nc_open(ncname)
meteoPrec <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE)
meteoPrec2 <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE,show_year=TRUE,show_month=TRUE)
nc_close(nc)
}
\seealso{
\code{\link{ncvar_put}},\code{\link{ncvar_get_multidf_values}}
}

