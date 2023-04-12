#include "../configure.F90"
module mpal_eig

    use mpal_basic
    use mpal_blas

    implicit none

contains

#include "lamch.F90"

#include "auxiliary.F90"

#include "lasr.F90"

#include "larst.F90"

#include "sterf.F90"

#include "steqr.F90"

#include "sytrd.F90"

#include "dlaed0.F90"

#include "stedc.F90"

#include "ormqr.F90"

#include "ormtr.F90"

#include "syevd.F90"

end module