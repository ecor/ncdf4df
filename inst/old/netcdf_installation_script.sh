

export HDF4_SOURCE_DIRECTORY=/Users/ecor/local/hdf5-1.8.12 
export NETCDF_SOURCE_DIRECTORY=/Users/ecor/local/netcdf-4.3.2
export H5DIR=/Users/ecor/local
export H4DIR=$H5DIR

###http://www.unidata.ucar.edu/software/netcdf/docs/build_hdf4.html
# Build and install HDF4
cd ${HDF4_SOURCE_DIRECTORY}
./configure --enable-shared --disable-netcdf --disable-fortran --prefix=${H4DIR}
make
make install
# Build and install netCDF with HDF4 access enabled
cd ${NETCDF_SOURCE_DIRECTORY}
CPPFLAGS="-I${H5DIR}/include -I${H4DIR}/include" \
LDFLAGS="-L${H5DIR}/lib -L${H4DIR}/lib" \
./configure --enable-hdf4 --enable-hdf4-file-tests
make check
make install