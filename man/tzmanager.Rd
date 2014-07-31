\name{tzmanager}
\alias{tzmanager}
\title{Time Zone Manager}
\usage{
tzmanager(tz = Sys.timezone(location = TRUE), returns.string = TRUE,
  when = as.character(Sys.Date()), offset.sign = NA)
}
\arguments{
  \item{tz}{time zone (string character or number)}

  \item{returns.string}{lofical. If it is \code{TRUE}
  (default), the return value is always coerced to a
  character string}

  \item{when}{string character indicating a reference date
  (YYYY-MM-DD). See default.}

  \item{offset.sign}{offset signum. It can be
  \code{c(NA,-1,1)}. Default is \code{NA} and mans that is
  set to 1 except for the case where \code{tz} appears like
  \code{Etc/GMT+n} (where \code{n} is interpreted in an
  opposite way for most operative systems, see latest lines
  on \code{\link{timezones)}}) and is set -1.}
}
\description{
This function calculated the difference between a time zone
\code{tz} and Greenwhich Mean Time or viceversa.
}
\examples{
tz <- Sys.timezone()

difftz <- tzmanager(tz=tz)
difftznum <- tzmanager(tz=tz,returns.string=FALSE)

tz_v2 <- tzmanager(tz=difftznum)
}
\seealso{
\code{\link{timezones)}},\url{http://en.wikipedia.org/wiki/ISO_8601#Dates}
}

