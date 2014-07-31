##PKG_CPPFLAGS=-I/path/to/netcdf/header
##PKG_LIBS=-L/path/to/netcdf/lib -lnetcdf

local <- "/Users/ecor/local"
PKG_CPPFLAGS <- paste("-I",local,"/include",sep="")
PKG_LIBS <- paste(paste("-L",local,"/lib",sep=""),"-lnetcdf -lhdf5_hl -lhdf5 -lz",sep=" ")

Sys.setenv(PKG_CPPFLAGS=PKG_CPPFLAGS,PKG_LIBS=PKG_LIBS)
###http://mazamascience.com/WorkingWithData/?p=1429
configure.args <- paste(PKG_CPPFLAGS,PKG_LIBS,sep=" ")
install.packages("ncdf4",configure.args=configure.args)


