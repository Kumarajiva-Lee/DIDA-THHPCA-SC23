program main

    use mpal

    implicit none

    type(mpal_st) :: a, b
    type(mpal_st) :: c
    

    call mpal_init()

    a = 1.123d0
    b = 1.123d0
    c = mpal_st(1.0d0)

    a = a * c
    b = b * c

    call mpal_debug_show(a)
    call mpal_debug_show(b)

    c = a - b

    call mpal_debug_show(c)

    call mpal_finalize()

end program