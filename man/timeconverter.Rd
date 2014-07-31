\name{timeconverter}
\alias{timeconverter}
\title{Time Unit Manager}
\usage{
timeconverter(x = as.POSIXct(Sys.time()),
  unit = "seconds since 1970-01-01 +1")
}
\arguments{
  \item{x}{a vector or}

  \item{unit}{mesarement unit with origin data and time
  zone. Default is \code{"seconds since 1970-01-01 +1"}}
}
\description{
Convert time to POSIXct to a numeric value expressed in
seconds
}
\examples{
today_sec <- timeconverter()

today <- timeconverter(today_sec)
}

