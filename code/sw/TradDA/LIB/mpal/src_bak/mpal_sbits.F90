subroutine mpal_set_sbits_1e(a, sbits)

    type(mpal_st), intent(inout) :: a
    integer, intent(in) :: sbits

    a%rpe_st%sbits = sbits
    call apply_truncation(a%rpe_st)

end subroutine

subroutine mpal_set_sbits_1d(a, n, sbits)

    type(mpal_st), dimension(n), intent(inout) :: a
    integer, intent(in) :: n
    integer, intent(in) :: sbits

    integer :: i

    do i = 1, n
        a(i)%rpe_st%sbits = sbits
        call apply_truncation(a(i)%rpe_st)
    end do
    
end subroutine

subroutine mpal_set_sbits_2d(a, m, n, sbits)

    type(mpal_st), dimension(m, n), intent(inout) :: a
    integer, intent(in) :: m, n
    integer, intent(in) :: sbits

    integer :: i, j

    do i = 1, n
        do j = 1, m
            a(j, i)%rpe_st%sbits = sbits
            call apply_truncation(a(j, i)%rpe_st)
        end do
    end do
    
end subroutine