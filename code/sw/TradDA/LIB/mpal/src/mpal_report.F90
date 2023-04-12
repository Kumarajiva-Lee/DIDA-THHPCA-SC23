
subroutine mpal_report_1e(a, name)

    type(mpal_st), intent(in) :: a
    character*(*), intent(in) :: name

    real(8) :: norm
    real(8) :: denominator

    denominator = abs(a%dvalue)
    if (denominator == 0) denominator = 1d0
    norm = abs(a%dvalue - MPAL_VAL(a)) / denominator

    write(*, "(A54)") "v--------------- MPAL report element ----------------v"
    write(*, "(A14, A18)") "Element name:     ", trim(adjustl(name))
    write(*, "(A14, F18.6)") "Double value:     ", a%dvalue
    write(*, "(A14, F18.6)") "MPAL_VAL value:   ", MPAL_VAL(a)
    write(*, "(A14, F18.6)") "Relative error:   ", norm
    write(*, "(A54)") "^----------------------------------------------------^"

end subroutine

subroutine mpal_report_1d(a, n, name)

    type(mpal_st), dimension(n), intent(in) :: a
    integer, intent(in) :: n

    character*(*), intent(in) :: name

    integer :: i

    integer :: maxi
    real(8) :: normmax, norm2
    real(8) :: tmp
    real(8) :: denominator

    normmax = -1d0
    maxi = 0
    do i = 1, n
        denominator = abs(a(i)%dvalue)
        if (denominator == 0) denominator = 1d0
        tmp = abs(a(i)%dvalue - MPAL_VAL(a(i))) / denominator
        if (tmp > normmax) then
            normmax = tmp
            maxi = i
        end if
    end do
    
    norm2 = 0
    do i = 1, n
        norm2 = norm2 + (a(i)%dvalue - MPAL_VAL(a(i))) ** 2
    end do
    norm2 = sqrt(norm2)

    write(*, "(A54)") "v--------------- MPAL report 1d array ---------------v"
    write(*, "(A14, A18)") "Array name:       ", trim(adjustl(name))
    write(*, "(A14, F18.6)") "Norm max:         ", normmax
    write(*, "(A14, I18)") "Error max at: i = ", maxi
    write(*, "(A14, F18.6)") "Double value:     ", a(maxi)%dvalue
    write(*, "(A14, F18.6)") "MPAL_VAL value:   ", MPAL_VAL(a(maxi))
    write(*, "(A14, F18.6)") "Norm Frobenius:   ", norm2
    write(*, "(A54)") "^----------------------------------------------------^"

end subroutine

subroutine mpal_report_2d(a, m, n, name)

    type(mpal_st), dimension(m, n), intent(in) :: a
    integer, intent(in) :: m, n

    character*(*), intent(in) :: name

    real(8) :: error_m(m, n)

    real(8) :: normmax, normF
    real(8) :: work(1)

    real(8) :: denominator

    external dlange
    double precision dlange

    integer :: i, j
    integer :: maxi, maxj

    normmax = -1d0
    maxi = 0
    maxj = 0

    do i = 1, n
        do j = 1, m
            denominator = abs(a(j, i)%dvalue)
            if (denominator == 0) denominator = 1d0
            error_m(j, i) = abs(a(j, i)%dvalue - MPAL_VAL(a(j, i))) / denominator
            if (error_m(j, i) > normmax) then
                normmax = error_m(j, i)
                maxi = j
                maxj = i
            end if
        end do
    end do

    normmax = dlange('M', m, n, error_m, m, work)
    normF = dlange('F', m, n, error_m, m, work)

    write(*, "(A54)") "v--------------- MPAL report 2d array ---------------v"
    write(*, "(A14, A18)") "Array name:       ", trim(adjustl(name))
    write(*, "(A14, F18.6)") "Norm max:         ", normmax
    write(*, "(A14, I18)") "Error max at: i = ", maxi
    write(*, "(A14, I18)") "Error max at: j = ", maxj
    write(*, "(A14, F18.6)") "Double value:     ", a(maxi, maxj)%dvalue
    write(*, "(A14, F18.6)") "MPAL_VAL value:   ", MPAL_VAL(a(maxi, maxj))
    write(*, "(A14, F18.6)") "Norm Frobenius:   ", normF
    write(*, "(A54)") "^----------------------------------------------------^"

end subroutine

subroutine mpal_report_3d(a, m, n, l)

    type(mpal_st), dimension(m, n, l), intent(in) :: a
    integer, intent(in) :: m, n, l

end subroutine
