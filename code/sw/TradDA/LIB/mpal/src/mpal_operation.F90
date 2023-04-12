! mpal_st op mpal_st

subroutine assign_mm(lhs, rhs)

    type(mpal_st), intent(inout) :: lhs
    type(mpal_st), intent(in) :: rhs

#ifdef MONITOR_ON
    real(8) :: fraction
    real(8) :: denominator
#endif

    lhs%dvalue = rhs%dvalue
    lhs%svalue = rhs%svalue

#ifdef CADNA_ON
    lhs%dcad_st = rhs%dcad_st
#endif

#ifdef RPE_ON
    lhs%rpe_st = rhs%rpe_st
#endif

#ifdef MONITOR_ON
    fraction = abs(lhs%dvalue - MPAL_VAL(lhs))
    denominator = abs(lhs%dvalue)
    if (denominator == 0d0) denominator = 1d0
    if (fraction / denominator > ERROR_MAX) then
        write(*, "(A57)") "[ MPAL Error ] >> Assign relative error beyond threshold!"
        call mpi_abort()
    end if
#endif

end subroutine

type(mpal_st) function add(a, b)
    
    type(mpal_st), intent(in) :: a, b

#ifdef MONITOR_ON
    real(8) :: fraction
    real(8) :: denominator
#endif

    add%dvalue = a%dvalue + b%dvalue
    add%svalue = a%svalue + b%svalue

#ifdef CADNA_ON
    add%dcad_st = a%dcad_st + b%dcad_st
#endif

#ifdef RPE_ON
    add%rpe_st%sbits = MAX(mpal_sbits(a), mpal_sbits(b))
    add%rpe_st = a%rpe_st + b%rpe_st
#endif

#ifdef MONITOR_ON
    fraction = abs(add%dvalue - MPAL_VAL(add))
    denominator = abs(add%dvalue)
    if (denominator == 0d0) denominator = 1d0
    if (fraction / denominator > ERROR_MAX) then
        write(*, "(A54)") "[ MPAL Error ] >> ADD relative error beyond threshold!"
        write(*, "(A18, A4, F18.6, A10, F18.6, A12)") " ", "a = ", a%dvalue, " (double) ", MPAL_VAL(a), " (MPAL_VAL) "
        write(*, "(A18, A4, F18.6, A10, F18.6, A12)") " ", "b = ", b%dvalue, " (double) ", MPAL_VAL(b), " (MPAL_VAL) "
        write(*, "(A18, A4, F18.6, A10, F18.6, A12)") " ", "c = ", add%dvalue, " (double) ", MPAL_VAL(add), " (MPAL_VAL) "
        call mpi_abort()
    end if
#endif

end function add

type(mpal_st) function sub(a, b)

    type(mpal_st), intent(in) :: a, b

#ifdef MONITOR_ON
    real(8) :: fraction
    real(8) :: denominator
#endif

    sub%dvalue = a%dvalue - b%dvalue
    sub%svalue = a%svalue - b%svalue

#ifdef CADNA_ON
    sub%dcad_st = a%dcad_st - b%dcad_st
#endif

#ifdef RPE_ON
    sub%rpe_st%sbits = MAX(mpal_sbits(a), mpal_sbits(b))
    sub%rpe_st = a%rpe_st - b%rpe_st
#endif

#ifdef MONITOR_ON
    fraction = abs(sub%dvalue - MPAL_VAL(sub))
    denominator = abs(sub%dvalue)
    if (denominator == 0d0) denominator = 1d0
    if (fraction / denominator > ERROR_MAX) then
        write(*, "(A54)") "[ MPAL Error ] >> SUB relative error beyond threshold!"
        write(*, "(A18, A4, F18.6, A10, F18.6, A12)") " ", "a = ", a%dvalue, " (double) ", MPAL_VAL(a), " (MPAL_VAL) "
        write(*, "(A18, A4, F18.6, A10, F18.6, A12)") " ", "b = ", b%dvalue, " (double) ", MPAL_VAL(b), " (MPAL_VAL) "
        write(*, "(A18, A4, F18.6, A10, F18.6, A12)") " ", "c = ", sub%dvalue, " (double) ", MPAL_VAL(sub), " (MPAL_VAL) "
        call mpi_abort()
    end if
#endif

end function sub

type(mpal_st) function multi(a, b)
    type(mpal_st), intent(in) :: a, b

    multi%dvalue = a%dvalue * b%dvalue
    multi%svalue = a%svalue * b%svalue

#ifdef CADNA_ON
    multi%dcad_st = a%dcad_st * b%dcad_st
#endif

#ifdef RPE_ON
    multi%rpe_st%sbits = MAX(mpal_sbits(a), mpal_sbits(b))
    multi%rpe_st = a%rpe_st * b%rpe_st
#endif
end function multi

type(mpal_st) function div(a, b)

    type(mpal_st), intent(in) :: a, b

    div%dvalue = a%dvalue / b%dvalue
    div%svalue = a%svalue / b%svalue

#ifdef CADNA_ON
    div%dcad_st = a%dcad_st / b%dcad_st
#endif

#ifdef RPE_ON
    div%rpe_st%sbits = MAX(mpal_sbits(a), mpal_sbits(b))
    div%rpe_st = a%rpe_st / b%rpe_st
#endif

end function div

type(mpal_st) function power_mm(a, b)
    type(mpal_st), intent(in) :: a, b

    power_mm%dvalue = a%dvalue ** b%dvalue
    power_mm%svalue = a%svalue ** b%svalue

#ifdef CADNA_ON
    power_mm%dcad_st = a%dcad_st ** b%dcad_st
#endif

#ifdef RPE_ON
    power_mm%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    power_mm%rpe_st = a%rpe_st ** b%rpe_st
#endif
end function power_mm

pure logical function mpal_lt(a, b)
    
    type(mpal_st), intent(in) :: a, b

    mpal_lt = MPAL_VAL(a) < MPAL_VAL(b)

end function

pure logical function mpal_gt(a, b)
    
    type(mpal_st), intent(in) :: a, b

    mpal_gt = MPAL_VAL(a) > MPAL_VAL(b)

end function

pure logical function mpal_le(a, b)
    
    type(mpal_st), intent(in) :: a, b

    mpal_le = MPAL_VAL(a) .le. MPAL_VAL(b)

end function

pure logical function mpal_ge(a, b)
    
    type(mpal_st), intent(in) :: a, b

    mpal_ge = MPAL_VAL(a) .ge. MPAL_VAL(b)

end function

pure logical function mpal_ne(a, b)
    
    type(mpal_st), intent(in) :: a, b

    mpal_ne = MPAL_VAL(a) .ne. MPAL_VAL(b)

end function

pure logical function mpal_eq(a, b)
    
    type(mpal_st), intent(in) :: a, b

    mpal_eq = MPAL_VAL(a) .eq. MPAL_VAL(b)

end function

! mpal_st op constant

subroutine assign(lhs, rhs)

    type(mpal_st), intent(inout) :: lhs
    real(8), intent(in) :: rhs

    lhs%dvalue = rhs
    lhs%svalue = real(rhs, 4)

#ifdef CADNA_ON
    lhs%dcad_st = rhs
#endif

#ifdef RPE_ON
    lhs%rpe_st = rhs
#endif

end subroutine

subroutine assign_s(lhs, rhs)

    type(mpal_st), intent(inout) :: lhs
    real(4), intent(in) :: rhs

    lhs%dvalue = real(rhs, 8)
    lhs%svalue = rhs

#ifdef CADNA_ON
    lhs%dcad_st = rhs
#endif

#ifdef RPE_ON
    lhs%rpe_st = rhs
#endif

end subroutine

subroutine assign_i(lhs, rhs)

    type(mpal_st), intent(inout) :: lhs
    integer, intent(in) :: rhs

    lhs%dvalue = real(rhs, 8)
    lhs%svalue = real(rhs, 4)
    
#ifdef CADNA_ON
    lhs%dcad_st = rhs
#endif

#ifdef RPE_ON
    lhs%rpe_st = rhs
#endif

end subroutine

subroutine r8_assign(lhs, rhs)

    real(8), intent(inout) :: lhs
    type(mpal_st), intent(in) :: rhs

    lhs = MPAL_VAL(rhs)

end subroutine

subroutine r4_assign(lhs, rhs)

    real(4), intent(inout) :: lhs
    type(mpal_st), intent(in) :: rhs

    lhs = MPAL_VAL(rhs)

end subroutine

! mpal_st op double const
type(mpal_st) function add_const(a, b)
    type(mpal_st), intent(in) :: a
    real(8), intent(in) :: b

    add_const%dvalue = a%dvalue + b
    add_const%svalue = a%svalue + b

#ifdef CADNA_ON
    add_const%dcad_st = a%dcad_st + b
#endif

#ifdef RPE_ON
    add_const%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    add_const%rpe_st = a%rpe_st + b
#endif
end function add_const

type(mpal_st) function sub_const(a, b)
    type(mpal_st), intent(in) :: a
    real(8), intent(in):: b

    sub_const%dvalue = a%dvalue - b
    sub_const%svalue = a%svalue - b

#ifdef CADNA_ON
    sub_const%dcad_st = a%dcad_st - b
#endif

#ifdef RPE_ON
    sub_const%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    sub_const%rpe_st = a%rpe_st - b
#endif
end function sub_const

type(mpal_st) function multi_const(a, b)
    type(mpal_st), intent(in) :: a
    real(8), intent(in) :: b

    multi_const%dvalue = a%dvalue * b
    multi_const%svalue = a%svalue * b

#ifdef CADNA_ON
    multi_const%dcad_st = a%dcad_st * b
#endif

#ifdef RPE_ON
    multi_const%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    multi_const%rpe_st = a%rpe_st * b
#endif
end function multi_const

type(mpal_st) function div_const(a, b)
    type(mpal_st), intent(in) :: a
    real(8), intent(in) :: b

    div_const%dvalue = a%dvalue / b
    div_const%svalue = a%svalue / b

#ifdef CADNA_ON
    div_const%dcad_st = a%dcad_st / b
#endif

#ifdef RPE_ON
    div_const%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    div_const%rpe_st = a%rpe_st / b
#endif
end function div_const

type(mpal_st) function power_const(a, b)
    type(mpal_st), intent(in) :: a
    real(8), intent(in) :: b
    type(rpe_var) :: x

    power_const%dvalue = a%dvalue ** b
    power_const%svalue = a%svalue ** b

#ifdef CADNA_ON
    power_const%dcad_st = a%dcad_st ** b
#endif

#ifdef RPE_ON
    power_const%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    power_const%rpe_st = a%rpe_st ** b
#endif
end function power_const

! double const op mpal_st

type(mpal_st) function const_add(a, b)
    real(8), intent(in) :: a
    type(mpal_st), intent(in) :: b

    const_add%dvalue = a + b%dvalue
    const_add%svalue = a + b%svalue

#ifdef CADNA_ON
    const_add%dcad_st = a + b%dcad_st
#endif

#ifdef RPE_ON
    const_add%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    const_add%rpe_st = a + b%rpe_st
#endif
end function const_add

type(mpal_st) function const_sub(a, b)
    real(8), intent(in):: a
    type(mpal_st), intent(in) :: b

    const_sub%dvalue = a - b%dvalue
    const_sub%svalue = a - b%svalue

#ifdef CADNA_ON
    const_sub%dcad_st = a - b%dcad_st
#endif

#ifdef RPE_ON
    const_sub%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    const_sub%rpe_st = a - b%rpe_st
#endif
end function const_sub

type(mpal_st) function const_multi(a, b)
    real(8), intent(in) :: a    
    type(mpal_st), intent(in) :: b

    const_multi%dvalue = a * b%dvalue
    const_multi%svalue = a * b%svalue

#ifdef CADNA_ON
    const_multi%dcad_st = a * b%dcad_st
#endif

#ifdef RPE_ON
    const_multi%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    const_multi%rpe_st = a * b%rpe_st
#endif
end function const_multi

type(mpal_st) function const_div(a, b)
    real(8), intent(in) :: a    
    type(mpal_st), intent(in) :: b
    
    const_div%dvalue = a / b%dvalue
    const_div%svalue = a / b%svalue

#ifdef CADNA_ON
    const_div%dcad_st = a / b%dcad_st
#endif

#ifdef RPE_ON
    const_div%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    const_div%rpe_st = a / b%rpe_st
#endif
end function const_div

pure logical function mpal_lt_const(a, b)
    
    type(mpal_st), intent(in) :: a
    real(8), intent(in) :: b

    mpal_lt_const = MPAL_VAL(a) < b

end function

pure logical function mpal_gt_const(a, b)
    
    type(mpal_st), intent(in) :: a
    real(8), intent(in) :: b

    mpal_gt_const = MPAL_VAL(a) > b

end function

pure logical function mpal_le_const(a, b)
    
    type(mpal_st), intent(in) :: a
    real(8), intent(in) :: b

    mpal_le_const = MPAL_VAL(a) .le. b

end function

pure logical function mpal_ge_const(a, b)
    
    type(mpal_st), intent(in) :: a
    real(8), intent(in) :: b

    mpal_ge_const = MPAL_VAL(a) .ge. b

end function

pure logical function mpal_ne_const(a, b)
    
    type(mpal_st), intent(in) :: a
    real(8), intent(in) :: b

    mpal_ne_const = MPAL_VAL(a) .ne. b

end function

pure logical function mpal_eq_const(a, b)
    
    type(mpal_st), intent(in) :: a
    real(8), intent(in) :: b

    mpal_eq_const = MPAL_VAL(a) .eq. b

end function

! mpal_st op single const

type(mpal_st) function add_const_s(a, b)
    type(mpal_st), intent(in) :: a
    real(4), intent(in) :: b

    add_const_s%dvalue = a%dvalue + real(b, 8)
    add_const_s%svalue = a%svalue + b

#ifdef CADNA_ON
    add_const_s%dcad_st = a%dcad_st + b
#endif

#ifdef RPE_ON
    add_const_s%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    add_const_s%rpe_st = a%rpe_st + b
#endif
end function add_const_s

type(mpal_st) function sub_const_s(a, b)
    type(mpal_st), intent(in) :: a
    real(4), intent(in):: b

    sub_const_s%dvalue = a%dvalue - real(b, 8)
    sub_const_s%svalue = a%svalue - b

#ifdef CADNA_ON
    sub_const_s%dcad_st = a%dcad_st - b
#endif

#ifdef RPE_ON
    sub_const_s%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    sub_const_s%rpe_st = a%rpe_st - b
#endif
end function sub_const_s

type(mpal_st) function multi_const_s(a, b)
    type(mpal_st), intent(in) :: a
    real(4), intent(in) :: b

    multi_const_s%dvalue = a%dvalue * real(b, 8)
    multi_const_s%svalue = a%svalue * b

#ifdef CADNA_ON
    multi_const_s%dcad_st = a%dcad_st * b
#endif

#ifdef RPE_ON
    multi_const_s%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    multi_const_s%rpe_st = a%rpe_st * b
#endif
end function multi_const_s

type(mpal_st) function div_const_s(a, b)
    type(mpal_st), intent(in) :: a
    real(4), intent(in) :: b

    div_const_s%dvalue = a%dvalue / real(b, 8)
    div_const_s%svalue = a%svalue / b

#ifdef CADNA_ON
    div_const_s%dcad_st = a%dcad_st / b
#endif

#ifdef RPE_ON
    div_const_s%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    div_const_s%rpe_st = a%rpe_st / b
#endif
end function div_const_s

! mpal_st op const integer

type(mpal_st) function add_const_i(a, b)
    type(mpal_st), intent(in) :: a
    integer, intent(in) :: b

    add_const_i%dvalue = a%dvalue + real(b, 8)
    add_const_i%svalue = a%svalue + real(b, 4)

#ifdef CADNA_ON
    add_const_i%dcad_st = a%dcad_st + real(b, 8)
#endif

#ifdef RPE_ON
    add_const_i%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    add_const_i%rpe_st = a%rpe_st + real(b, 8)
#endif
end function add_const_i

type(mpal_st) function sub_const_i(a, b)
    type(mpal_st), intent(in) :: a
    integer, intent(in):: b

    sub_const_i%dvalue = a%dvalue - real(b, 8)
    sub_const_i%svalue = a%svalue - real(b, 4)

#ifdef CADNA_ON
    sub_const_i%dcad_st = a%dcad_st - real(b, 8)
#endif

#ifdef RPE_ON
    sub_const_i%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    sub_const_i%rpe_st = a%rpe_st - real(b, 8)
#endif
end function sub_const_i

type(mpal_st) function multi_const_i(a, b)
    type(mpal_st), intent(in) :: a
    integer, intent(in) :: b

    multi_const_i%dvalue = a%dvalue * real(b, 8)
    multi_const_i%svalue = a%svalue * real(b, 4)

#ifdef CADNA_ON
    multi_const_i%dcad_st = a%dcad_st * real(b, 8)
#endif

#ifdef RPE_ON
    multi_const_i%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    multi_const_i%rpe_st = a%rpe_st * real(b, 8)
#endif
end function multi_const_i

type(mpal_st) function div_const_i(a, b)
    type(mpal_st), intent(in) :: a
    integer, intent(in) :: b

    div_const_i%dvalue = a%dvalue / real(b, 8)
    div_const_i%svalue = a%svalue / real(b, 4)

#ifdef CADNA_ON
    div_const_i%dcad_st = a%dcad_st / real(b, 8)
#endif

#ifdef RPE_ON
    div_const_i%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    div_const_i%rpe_st = a%rpe_st / real(b, 8)
#endif
end function div_const_i

type(mpal_st) function power_const_i(a, b)
    type(mpal_st), intent(in) :: a
    integer, intent(in) :: b
    type(rpe_var) :: x

    power_const_i%dvalue = a%dvalue ** b
    power_const_i%svalue = a%svalue ** b

#ifdef CADNA_ON
    power_const_i%dcad_st = a%dcad_st ** b
#endif

#ifdef RPE_ON
    power_const_i%rpe_st%sbits = max(mpal_sbits(a), mpal_sbits(b))
    power_const_i%rpe_st = a%rpe_st ** b
#endif
end function power_const_i

! negative mpal_st

type(mpal_st) function neg(a)

    type(mpal_st), intent(in) :: a

    neg%dvalue = -(a%dvalue)
    neg%svalue = -(a%svalue)

#ifdef CADNA_ON
    neg%dcad_st = -(a%dcad_st)
#endif

#ifdef RPE_ON
    neg%rpe_st%sbits = a%rpe_st%sbits
    neg%rpe_st = -(a%rpe_st)
#endif
end function neg

#ifdef RPE_ON
integer function mpal_sbits(a)

    class(*), intent(in) :: a

    select type (a)
    type is (mpal_st)
        mpal_sbits = significand_bits(a%rpe_st)
    class default
        mpal_sbits = significand_bits(a)
    end select

end function mpal_sbits

#endif