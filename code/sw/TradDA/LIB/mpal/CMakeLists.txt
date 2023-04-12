cmake_minimum_required(VERSION 3.0)

project(mpal)

enable_language(Fortran)
set(CMAKE_BUILD_TYPE Release)

set(CMAKE_Fortran_COMPILER mpif90)
set(CMAKE_Fortran_FLAGS "-cpp")

include_directories("${PROJECT_SOURCE_DIR}/include")
link_directories("${PROJECT_SOURCE_DIR}/lib")

message(${PROJECT_SOURCE_DIR})

set(mpal_files
  src/mpal.F90
  src/mpal_basic.F90
  src/blas/mpal_blas.F90
  src/eig/mpal_eig.F90
)

add_library(mpal ${mpal_files})
target_link_libraries(mpal)
