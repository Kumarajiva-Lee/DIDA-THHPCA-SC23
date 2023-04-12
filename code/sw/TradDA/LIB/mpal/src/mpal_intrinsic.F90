
type(mpal_st) function mpal_abs(x)

    type(mpal_st), intent(in) :: x

    mpal_abs = abs(MPAL_VAL(x))

end function

type(mpal_st) function mpal_sqrt(x)

    type(mpal_st), intent(in) :: x

    mpal_sqrt = sqrt(MPAL_VAL(x))

end function

type(mpal_st) function mpal_log(x)

    type(mpal_st), intent(in) :: x

    mpal_log = log(MPAL_VAL(x))

end function

type(mpal_st) function mpal_max(x, y)

    type(mpal_st), intent(in) :: x, y

    mpal_max = max(MPAL_VAL(x), MPAL_VAL(y))

end function

type(mpal_st) function mpal_max3(x, y, z)

    type(mpal_st), intent(in) :: x, y, z

    mpal_max3 = max(MPAL_VAL(x), MPAL_VAL(y), MPAL_VAL(z))

end function

type(mpal_st) function mpal_min(x, y)

    type(mpal_st), intent(in) :: x, y

    mpal_min = min(MPAL_VAL(x), MPAL_VAL(y))

end function

type(mpal_st) function mpal_min3(x, y, z)

    type(mpal_st), intent(in) :: x, y, z

    mpal_min3 = min(MPAL_VAL(x), MPAL_VAL(y), MPAL_VAL(z))

end function

type(mpal_st) function mpal_sign(x, y)

    type(mpal_st), intent(in) :: x, y

    mpal_sign = sign(MPAL_VAL(x), MPAL_VAL(y))

end function

type(mpal_st) function mpal_int(x)

    type(mpal_st), intent(in) :: x

    mpal_int = int(MPAL_VAL(x))

end function