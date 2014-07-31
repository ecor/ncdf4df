\name{rolldfapply}
\alias{rolldfapply}
\title{Wrapper for \code{\link{rollapply}}}
\usage{
rolldfapply(data, TimeField = "Time", ValueField = "value",
  align = "right", ...)
}
\arguments{
  \item{data}{data frame}

  \item{TimeField}{character string containg time field of
  \code{data}}

  \item{ValueField}{character string containg time field of
  \code{data}}

  \item{align}{argument of \code{\link{rollapply}}. Default
  value is here set to \code{right}.}

  \item{...}{arguments for \code{\link{rollapply}}}
}
\value{
A data frame with the same format of \code{data} with
"rolled" calculated values.
}
\description{
Function \code{\link{rollapply}} applied to a Data Frame
structured as follows: \code{Time,Station,Variable,Value}
}
\examples{
library(ncdf4df)
## ncname <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data/trentino_hourlyweatherdata.nc"
ncname <- system.file("trentino/data/trentino_hourlyweatherdata.nc",package="ncdf4df")
nc <- nc_open(ncname)
meteoPrec <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE)

nc_close(nc)

width <- 3 ## rolling windows of 3 time inidices, e.g. hour
meteoPrec3h <-  rolldfapply(data=meteoPrec,width=width,FUN=sum,align="right") ## l
}
\seealso{
\code{\link{rollapply}}
}

