\name{ncvar_put_multidf_values}
\alias{ncvar_put_multidf_values}
\title{More varibles in a data frame (put in netCDF)}
\usage{
ncvar_put_multidf_values(x, nameVar_df = NULL, varid, ...)
}
\arguments{
  \item{x}{data frame}

  \item{nameVar_df}{vector of string containing variable
  names}

  \item{varid}{list of \code{ncvar4} objects}

  \item{...}{arguments for
  \code{\link{ncvar_put_df_values}}}
}
\description{
It is a wrapper for 'ncvar_put_df_values' in case of more
thsn one variables
}
\seealso{
\code{\link{ncvar_put_df_values}}
}

