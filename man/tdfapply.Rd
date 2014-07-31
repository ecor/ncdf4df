\name{tdfapply}
\alias{tdfapply}
\title{Apply a Function of a value vector in a data frame}
\usage{
tdfapply(data, TimeField = "Time", ValueField = "value",
  add_fields = c("month", "year", "season"), FactorFields = c("Station",
  "month", "variable"), FUN = sum, return.data.frame = TRUE, ...)
}
\arguments{
  \item{data}{data frame}

  \item{TimeField}{character string containg time field of
  \code{data}}

  \item{ValueField}{character string containg time field of
  \code{data}}

  \item{add_fields}{character string. Default is
  \code{c("month","year","season")}}

  \item{FactorFields}{character string indicating the name
  of data frema fiels using as factors}

  \item{FUN}{atomic function}

  \item{return.data.frame}{logical vaue. If it is
  \code{TRUE} the value is melted into a data rame. Default
  is \code{TRUE}}

  \item{...}{further arguments for \code{\link{tapply}}}
}
\description{
Apply a function to each value of the value field of the
data frame conserving the other fields of the data frame
using \code{\link{tapply}}
}
\examples{
library(ncdf4df)
## ncname <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data/trentino_hourlyweatherdata.nc"
ncname <- system.file("trentino/data/trentino_hourlyweatherdata.nc",package="ncdf4df")
nc <- nc_open(ncname)
meteoPrec <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE)
meteoPrec2 <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE,show_year=TRUE,show_month=TRUE)
nc_close(nc)

MonthlyPrec <- tdfapply(data=meteoPrec,FactorFields=c("Station","month","year"),FUN=sum,na.rm=TRUE)

str(MonthlyPrec)
}
\seealso{
\code{\link{tapply}},\code{\link{melt}},\code{\link{show.year.month.season}}
}

