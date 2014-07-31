# TODO: Add comment
# 
# Author: ecor
###############################################################################

## Calculate Monthly Precipitation 
rm(list=ls())
library(ncdf4df)
## ncname <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data/trentino_hourlyweatherdata.nc"
ncname <- system.file("trentino/data/trentino_hourlyweatherdata.nc",package="ncdf4df")
nc <- nc_open(ncname)
meteoPrec <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE)
meteoPrec2 <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE,show_year=TRUE,show_month=TRUE)
nc_close(nc)

MonthlyPrec <- tdfapply(data=meteoPrec,FactorFields=c("Station","month","year"),FUN=sum,na.rm=TRUE)

str(MonthlyPrec)




