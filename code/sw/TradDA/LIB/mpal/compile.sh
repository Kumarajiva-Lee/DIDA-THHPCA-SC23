export CC=mpicc CXX=mpicxx FC=mpif90
rm -rf build
mkdir build
cd build
cmake ..
make
cp mpal*.mod ../include/
cp libmpal.a ../lib/libmpal.a
