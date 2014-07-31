\name{ncvar_get_multidf_values}
\alias{ncvar_get_multidf_values}
\title{More varibles in a data frame (get from netCDF)}
\usage{
ncvar_get_multidf_values(x, ...)
}
\arguments{
  \item{x}{vector of string containing variable names}

  \item{...}{arguments for
  \code{\link{ncvar_get_df_values}}}
}
\description{
It is a wrapper for 'ncvar_get_df_values' in case of more
thsn one variables
}
\seealso{
\code{\link{ncvar_get_df_values}}
}

