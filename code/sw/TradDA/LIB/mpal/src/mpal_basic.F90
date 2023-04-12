#include "configure.F90"
module mpal_basic

#ifdef RPE_ON
    use rp_emulator
#endif
#ifdef CADNA_ON
    use cadna
#endif

    implicit none

    type :: mpal_st

        real(8) :: dvalue
        real(4) :: svalue
        integer :: padding
        
#ifdef CADNA_ON
        type(double_st) :: dcad_st
        !type(single_st) :: scad_st
#endif

#ifdef RPE_ON
        type(rpe_var) :: rpe_st
#endif

    end type mpal_st

    interface mpal_init
        module procedure mpal_init_bits
    end interface

    interface st_init
        module procedure mpal_st_d
        module procedure mpal_st_s
    end interface st_init

    interface operator(+)
        module procedure add
        module procedure add_const
        module procedure const_add
        module procedure add_const_s
        module procedure add_const_i
    end interface
    interface operator(-)
        module procedure sub
        module procedure sub_const
        module procedure const_sub
        module procedure sub_const_s
        module procedure sub_const_i
        module procedure neg
    end interface
    interface operator(*)
        module procedure multi
        module procedure multi_const
        module procedure const_multi
        module procedure multi_const_s
        module procedure multi_const_i
    end interface
    interface operator(/)
        module procedure div
        module procedure div_const
        module procedure const_div
        module procedure div_const_s
        module procedure div_const_i
    end interface
    interface operator(**)
        module procedure power_mm
        module procedure power_const_i
    end interface
    interface operator(.lt.)
        module procedure mpal_lt
        module procedure mpal_lt_const
    end interface
    interface operator(.gt.)
        module procedure mpal_gt
        module procedure mpal_gt_const
    end interface
    interface operator(.le.)
        module procedure mpal_le
        module procedure mpal_le_const
    end interface
    interface operator(.ge.)
        module procedure mpal_ge
        module procedure mpal_ge_const
    end interface
    interface operator(.ne.)
        module procedure mpal_ne
        module procedure mpal_ne_const
    end interface
    interface operator(.eq.)
        module procedure mpal_eq
        module procedure mpal_eq_const
    end interface
    interface assignment(=)
        module procedure assign
        module procedure assign_s
        module procedure assign_i
        module procedure assign_mm
        module procedure r8_assign
        module procedure r4_assign
    end interface

    interface abs
        module procedure mpal_abs
    end interface

    interface sqrt
        module procedure mpal_sqrt
    end interface

    interface log
        module procedure mpal_log
    end interface

    interface max
        module procedure mpal_max
        module procedure mpal_max3
    end interface

    interface min
        module procedure mpal_min
        module procedure mpal_min3
    end interface

    interface sign
        module procedure mpal_sign
    end interface

    interface int
        module procedure mpal_int
    end interface

#ifdef RPE_ON
    interface mpal_set_sbits
        module procedure mpal_set_sbits_1e
        module procedure mpal_set_sbits_1d
        module procedure mpal_set_sbits_2d
    end interface
#endif

    interface mpal_report
        module procedure mpal_report_1e
        module procedure mpal_report_1d
        module procedure mpal_report_2d
        module procedure mpal_report_3d
    end interface

    interface epsilon
        module procedure mpal_epsilon
    end interface

    interface tiny
        module procedure mpal_tiny
    end interface

    interface huge
        module procedure mpal_huge
    end interface

    interface minexponent
        module procedure mpal_minexponent
    end interface

    interface maxexponent
        module procedure mpal_maxexponent
    end interface 

contains

#include "mpal_init.F90"

#include "mpal_type.F90"

#include "mpal_operation.F90"

#include "mpal_intrinsic.F90"

#include "mpal_report.F90"

#ifdef RPE_ON
#include "mpal_sbits.F90"

#include "mpal_EPS.F90"
#endif

end module mpal_basic
