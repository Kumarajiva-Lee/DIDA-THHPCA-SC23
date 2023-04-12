type(mpal_st) function mpal_epsilon(a)

    type(mpal_st), intent(in) :: a

    integer(8) :: bits
    integer(8) :: t2
    real(8) :: t
    integer :: lmtb

    bits = transfer(1.0D0, bits)

    lmtb = mpal_sbits(a)

    if (lmtb > 52) lmtb = 52

    t2 = int(2, 8) ** 52
    bits = bits - lmtb * t2

    t = transfer(bits, t)
    
    mpal_epsilon%rpe_st%sbits = mpal_sbits(a)
    mpal_epsilon = t

end function

type(mpal_st) function mpal_tiny(a)

    type(mpal_st), intent(in) :: a

    integer :: lmtb
    integer(8) :: sbits
    integer(8) :: t2
    real(8) :: t
    integer(8) :: t_15

    lmtb = 52 - mpal_sbits(a)

    if (lmtb < 0) lmtb = 0

    mpal_tiny%rpe_st%sbits = mpal_sbits(a)
    if ((mpal_sbits(a) == 10) .and. (RPE_IEEE_HALF)) then
        sbits = transfer(1.0D0, sbits)
        t2 = int(2, 8) ** 52
        t_15 = int(14, 8)
        sbits = sbits - t2 * t_15
        t = transfer(sbits, t)
        mpal_tiny = t
    else
        mpal_tiny = tiny(mpal_val(a))
    end if

end function

type(mpal_st) function mpal_huge(a)

    type(mpal_st), intent(in) :: a

    integer :: lmtb
    integer(8) :: sbits
    real(8) :: t
    integer(8), parameter :: zero_bits = 0

    mpal_huge%rpe_st%sbits = mpal_sbits(a)
    if ((mpal_sbits(a) == 10) .and. (RPE_IEEE_HALF)) then
        mpal_huge = 65504
    else
        lmtb = 52 - mpal_sbits(a) - 1
        t = huge(mpal_val(a))
        sbits = transfer(t, sbits)
        call mvbits(zero_bits, 0, lmtb + 1, sbits, 0)
        mpal_huge = transfer(sbits, t)
    end if

end function

integer function mpal_minexponent(a)

    type(mpal_st), intent(in) :: a

    if ((mpal_sbits(a) == 10) .and. (RPE_IEEE_HALF)) then
        mpal_minexponent = -13
    else
        mpal_minexponent = minexponent(mpal_val(a))
    end if

end function

integer function mpal_maxexponent(a)

    type(mpal_st), intent(in) :: a

    if ((mpal_sbits(a) == 10) .and. (RPE_IEEE_HALF)) then
        mpal_maxexponent = 16
    else
        mpal_maxexponent = maxexponent(mpal_val(a))
    end if

end function