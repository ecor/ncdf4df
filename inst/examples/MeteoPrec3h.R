# TODO: Add comment
# 
# Author: ecor
###############################################################################

## 3 hour-rolled Precipitation 
rm(list=ls())
library(ncdf4df)
## ncname <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/ncdf4df/inst/trentino/data/trentino_hourlyweatherdata.nc"
ncname <- system.file("trentino/data/trentino_hourlyweatherdata.nc",package="ncdf4df")
nc <- nc_open(ncname)
meteoPrec <- ncvar_get_df_values(nc=nc,x="Prec",verbose=TRUE)

nc_close(nc)

width <- 3 ## rolling windows of 3 time inidices, e.g. hour
meteoPrec3h <-  rolldfapply(data=meteoPrec,width=width,FUN=sum,align="right") ## l
