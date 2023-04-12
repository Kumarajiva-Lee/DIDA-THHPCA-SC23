
pure real(8) function mpal_abs(x)

    type(mpal_st), intent(in) :: x

    mpal_abs = abs(MPAL_VAL(x))

end function

pure real(8) function mpal_sqrt(x)

    type(mpal_st), intent(in) :: x

    mpal_sqrt = sqrt(MPAL_VAL(x))

end function
