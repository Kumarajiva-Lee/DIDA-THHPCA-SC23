program main

    use mpal

    implicit none

    type(mpal_st), dimension(:, :), allocatable :: a, b
    type(mpal_st), dimension(:, :), allocatable :: c
    real(4), dimension(:, :), allocatable :: a2, b2
    real(4), dimension(:, :), allocatable :: c2
    real(8), dimension(:, :), allocatable :: a3, b3
    real(8), dimension(:, :), allocatable :: c3
    integer :: m, n, k
    integer :: i, j
    real(4) :: delta
    real(8) :: maxerror
    real(8) :: toterror
    real(8) :: maxerror2
    real(8) :: toterror2
    real(8) :: maxerror3
    real(8) :: toterror3
    integer :: repeat
    integer :: times = 1

    type(mpal_st) :: q1
    type(mpal_st) :: q2
    type(mpal_st) :: q3

    call mpal_init(10)

    q1%rpe_st%sbits = 10
    q2%rpe_st%sbits = 10
    q1 = 1d-5
    q2 = 1d0
    q3 = 1d0
    q3 = q3 + q1 * q2
    write(*, *) q3%rpe_st%val

    m = 10; n = 10; k = 100
    allocate(a(m, k)); allocate(b(k, n)); allocate(c(m, n))
    allocate(a2(m, k)); allocate(b2(k, n)); allocate(c2(m, n))
    allocate(a3(m, k)); allocate(b3(k, n)); allocate(c3(m, n))

    !call random_seed()
    toterror = 0
    toterror2 = 0
    toterror3 = 0
    do repeat = 1, times
        do j = 1, k
            do i = 1, m
                call random_number(delta)
                a(i, j) = delta * 1
                a2(i, j) = delta * 1
                a3(i, j) = delta * 1
            end do
        end do
        do j = 1, n
            do i = 1, k
                call random_number(delta)
                b(i, j) = delta * 1
                b2(i, j) = delta * 1
                b3(i, j) = delta * 1
            end do
        end do
        do j = 1, n
            do i = 1, m
                c(j, i)%rpe_st%sbits = 51
            end do
        end do

        ! call sgemm('n','n',m,n,k,1.e0,a2,m, &
        ! b2,k,0.e0,c2,m)
        ! call dgemm('n','n',m,n,k,1.d0,a3,m, &
        ! b3,k,0.d0,c3,m)
        call mpal_gemm('n', 'n', m,n,k,a,m, &
        b,n,c)
        call single_gemm('n', 'n', m,n,k,a2,m, &
        b2,n,c2)
        call double_gemm('n', 'n', m,n,k,a3,m, &
        b3,n,c3)
        do i = 1, n
            do j = 1, m
                print 100, c(j, i)%rpe_st%val
            end do
            print *, " "
        end do
        print *, c(1, 1)%rpe_st%sbits
        do i = 1, n
            do j = 1, m
                print 100, c2(j, i)!b(j, i)%rpe_st%val
            end do
            print *, " "
        end do
        do i = 1, n
            do j = 1, m
                print 100, c3(j, i)!c(j, i)%rpe_st%val
            end do
            print *, " "
        end do
    100 format(F23.15,$)
        
        maxerror = 0d0
        maxerror2 = 0d0
        maxerror3 = 0d0
        do i = 1, n
            do j = 1, m
                if (abs(c(j, i)%rpe_st%val - c2(j, i)) > maxerror) then
                    maxerror = c(j, i)%rpe_st%val - c2(j, i)
                end if
                if (abs(c2(j, i) - c3(j, i)) > maxerror2) then
                    maxerror2 = c2(j, i) - c3(j, i)
                end if
                if (abs(c(j, i)%rpe_st%val - c3(j, i)) > maxerror3) then
                    maxerror3 = c(j, i)%rpe_st%val - c3(j, i)
                end if
            end do
        end do
        toterror = toterror + abs(maxerror)
        toterror2 = toterror2 + abs(maxerror2)
        toterror3 = toterror3 + abs(maxerror3)
        
    end do
    write (*, *) "rpe and single error", toterror / times
    write (*, *) "double and single error", toterror2 / times
    write (*, *) "rpe and double error", toterror3 / times

    call mpal_finalize()

end program

subroutine double_gemm(TRANSA, TRANSB, M, N, K, A, LDA, B, LDB, C)
    character, intent(in) :: TRANSA
    character, intent(in) :: TRANSB
    integer, intent(in) :: M
    integer, intent(in) :: N
    integer, intent(in) :: K
    real(8), dimension(lda,*), intent(in) :: A
    integer, intent(in) :: LDA
    real(8), dimension(ldb,*), intent(in) :: B
    integer, intent(in) :: LDB
    real(8), dimension(M, N), intent(inout) :: C
    !integer, intent(in) :: LDC

    real(8) :: tmp

    integer :: x, y, z
    if ((TRANSA == 'n' .or. TRANSA == 'N') &
    .and. (TRANSB == 'n'.or. TRANSB == 'N')) then
        do y = 1, n
            do x = 1, m
                c(x, y) = 0d0
            end do
        end do
        do y = 1, n
            do z = 1, k
                do x = 1, m
                    c(x, y) = c(x, y) + a(x, z) * b(z, y)
                end do
            end do
        end do
    else if ((TRANSA == 'n' .or. TRANSA == 'N') &
    .and. (TRANSB == 't'.or. TRANSB == 'T')) then
        do y = 1, n
            do x = 1, m
                c(x, y) = 0d0
            end do
        end do
        do z = 1, k
            do y = 1, n
                do x = 1, m
                    c(x, y) = c(x, y) + a(x, z) * b(y, z)
                end do
            end do
        end do
    else
        write(*, *) "warning: not implemented!"
    end if

end subroutine

subroutine single_gemm(TRANSA, TRANSB, M, N, K, A, LDA, B, LDB, C)
    character, intent(in) :: TRANSA
    character, intent(in) :: TRANSB
    integer, intent(in) :: M
    integer, intent(in) :: N
    integer, intent(in) :: K
    real(4), dimension(lda,*), intent(in) :: A
    integer, intent(in) :: LDA
    real(4), dimension(ldb,*), intent(in) :: B
    integer, intent(in) :: LDB
    real(4), dimension(M, N), intent(inout) :: C
    !integer, intent(in) :: LDC

    real(4) :: tmp

    integer :: x, y, z
    if ((TRANSA == 'n' .or. TRANSA == 'N') &
    .and. (TRANSB == 'n'.or. TRANSB == 'N')) then
        do y = 1, n
            do x = 1, m
                c(x, y) = 0d0
            end do
        end do
        do y = 1, n
            do z = 1, k
                do x = 1, m
                    c(x, y) = c(x, y) + a(x, z) * b(z, y)
                end do
            end do
        end do
    else if ((TRANSA == 'n' .or. TRANSA == 'N') &
    .and. (TRANSB == 't'.or. TRANSB == 'T')) then
        do y = 1, n
            do x = 1, m
                c(x, y) = 0d0
            end do
        end do
        do z = 1, k
            do y = 1, n
                do x = 1, m
                    c(x, y) = c(x, y) + a(x, z) * b(y, z)
                end do
            end do
        end do
    else
        write(*, *) "warning: not implemented!"
    end if

end subroutine