#include "../configure.F90"
module mpal_blas

    use mpal_basic

    implicit none

contains

#include "gemm.F90"

#include "gemv.F90"

#include "symv.F90"

#include "dot.F90"

#include "syr2k.F90"

#include "ger.F90"

#include "trmv.F90"

#include "trmm.F90"

end module