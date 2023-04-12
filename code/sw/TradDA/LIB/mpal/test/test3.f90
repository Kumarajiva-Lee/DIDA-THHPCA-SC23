program main

    use mpal

    implicit none

    type(mpal_st), dimension(:, :), allocatable :: a, w
    type(mpal_st), dimension(:), allocatable :: e, tau, d, work

    real(8), dimension(:, :), allocatable :: a2, w2
    real(8), dimension(:), allocatable :: e2, tau2, d2, work2

    integer, dimension(:), allocatable :: iwork

    real(8) delta
    integer m, n
    integer nb
    integer i, j
    integer info
    integer ierr

    m = 10; n = m
    nb = 5
    allocate(a(m, n)); allocate(w(m, nb)); allocate(e(n)); allocate(tau(n-1))
    allocate(a2(m, n)); allocate(w2(m, nb)); allocate(e2(n)); allocate(tau2(n-1))
    allocate(d(m)); allocate(d2(m))
    allocate(work(2*m*m+6*m+1))
    allocate(work2(2*m*m+6*m+1))
    allocate(iwork(5*m+3))
    
    call mpal_init(10)

    !call random_seed()
    !call mpal_set_sbits(a, m, n, 53)
    do j = 1, n
        do i = 1, m
            call random_number(delta)
            a(i, j) = delta * 1
            if (i == j) a(i, j) = a(i, j) + real(m, 8)
            a2(i, j) = delta * 1
            if (i == j) a2(i, j) = a2(i, j) + real(m, 8)
        
            !write(*, *) 'A', a(i, j)%rpe_st%val

        end do
    end do

    ! call mpal_dlatrd('L', m, nb, a, m, e, tau, w, m)
    ! call dlatrd('L', m, nb, a2, m, e2, tau2, w2, m)

    ! call mpal_sytrd('L', m, a, m, d, e, tau, work, m*m, info)
    ! call dsytrd('L', m, a2, m, d2, e2, tau2, work2, m*m, info)

    call mpal_report(a, m, n, "a")

    call mpal_dsyevd('V','L',m,a,m,e,work,size(work),iwork,size(iwork),ierr)
    call dsyevd('V','L',m,a2,m,e2,work2,size(work2),iwork,size(iwork),ierr)

    ! do i = 1, m
    !     do j = 1, n
    !         print 100, mpal_val(a(i, j)) - a2(i, j)
    !     end do
    !     print *, " "
    ! end do
    do i = 1, m
        print 100, mpal_val(e(i))
    end do
    print *, " "
    do i = 1, m
        print 100, e2(i)
    end do
    print *, " "
    
    ! do i = 1, m
    !     do j = 1, n
    !         print 100, mpal_val(a(i, j))
    !     end do
    !     print *, " "
    ! end do

    call mpal_finalize()

100 format(F15.8,$)

end program