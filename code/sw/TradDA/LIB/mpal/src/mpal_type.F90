
type(mpal_st) function mpal_st_d(fp_data, sbits)

    real(8), intent(in) :: fp_data
    integer , intent(in), optional :: sbits

    mpal_st_d%dvalue = fp_data
    mpal_st_d%svalue = real(fp_data, 4)
#ifdef CADNA_ON
    mpal_st_d%dcad_st = fp_data
    call data_st(mpal_st_d%dcad_st)
#endif

#ifdef RPE_ON
    if (present(sbits)) then
        mpal_st_d%rpe_st = rpe_literal(fp_data, sbits)
        mpal_st_d%rpe_st%sbits = sbits
    else
        mpal_st_d%rpe_st = fp_data
    end if
#endif

end function mpal_st_d

type(mpal_st) function mpal_st_s(fp_data, sbits)

    real(4), intent(in) :: fp_data
    integer , intent(in), optional :: sbits

    mpal_st_s%dvalue = real(fp_data, 8)
    mpal_st_s%svalue = fp_data
#ifdef CADNA_ON
    mpal_st_s%dcad_st = fp_data
    call data_st(mpal_st_s%dcad_st)
#endif

#ifdef RPE_ON
    if (present(sbits)) then
        mpal_st_s%rpe_st = rpe_literal(fp_data, sbits)
        mpal_st_s%rpe_st%sbits = sbits
    else
        mpal_st_s%rpe_st = fp_data
    end if
#endif

end function mpal_st_s

real(8) pure function mpal_val(a)
    
    type(mpal_st), intent(in) :: a
    mpal_val = GET_MPAL_VAL(a)

end function

subroutine mpal_debug_show(a)

    type(mpal_st), intent(in) :: a
    real :: tmp

#ifdef CADNA_ON
    tmp = a%dcad_st    
    write(*, *) "value: ", tmp
    write(*, *) "sbits: ", nb_significant_digit(a%dcad_st)
#endif

end subroutine mpal_debug_show