! generate yb & ayb matrix

subroutine gen_yb_dis_batch(batch, sj_array, si_array, ej_array, ei_array, nobsl, yb, dis, rdiaginv, rloc)

    integer, intent(in)  :: batch
    integer, dimension(batch), intent(in) :: sj_array, si_array, ej_array, ei_array
    
    integer, intent(out) :: nobsl(batch)
    yb_type, intent(out) :: yb(ens, MAX_OBS, batch)
    ayb_type,intent(out) :: dis(MAX_OBS, batch)
    real(4), intent(out) :: rdiaginv(MAX_OBS, batch)
    yb_type, intent(out) :: rloc(MAX_OBS, batch)

    real(8) :: center_lat, center_lon
    real(8) :: obs_dist
    real(8) :: gc

    integer :: i, j, k, l, m, n, batch_i,padding_nobsl

!     i = 0; j = 0

!     do k = 1, obs_type_num

! #define current current

!         do l = 1, current%obs_num

!             if (abs(current%lat(l)) <= obs_polar) then

!                 obs_dist = latlon_dist(current%lat(l), current%lon(l),&
!                 center_lat, center_lon)

!                 if (gc_filter == .true.) then
!                     gc = schr2(obs_dist * 1d0 / local_dist)
!                 else
!                     gc = schr(obs_dist * 1d0 / local_dist)
!                 end if

!                 if (gc > 0) then
!                     do m = 1, current%obs_nz
!                         do n = 1, current%obs_nvar
!                             nobsl = nobsl + 1

!                             yb(:, nobsl) = current%pro_yb(:, n, m, l)
!                             dis(nobsl) = current%dis_yo(n, m, l)
!                             rdiaginv(nobsl) = current%obs_rinv(n, m, l)
!                             rloc(nobsl) = gc

!                             if (nobsl >= MAX_OBS) then
! #if (DEBUG == 1)
!                                 !call log_notice("number of observations out of limit!")
! #endif
!                                 goto 10
!                             end if

!                         end do
!                     end do
!                 end if

!             end if
!         end do

! #undef current

!     end do
    
    do batch_i = 1, batch
        nobsl(batch_i) = 0
    end do

    do i = -north_n, south_n

        do j = -obs_lc(i)%west_n, obs_lc(i)%east_n

            !if (i .ne. 0 .and. j .ne. 0) then

                do k = 1, obs_type_num

#define current obs_lc(i)%obs_data(k, j)

                    do l = 1, current%obs_num

                        if (abs(current%lat(l)) <= obs_polar) then
                                
                            do batch_i = 1, batch
    
                                center_lat = grid_lat((sj_array(batch_i) + ej_array(batch_i)) / 2)
                                center_lon = grid_lon((si_array(batch_i) + ei_array(batch_i)) / 2)
                                
                                obs_dist = latlon_dist(current%lat(l), current%lon(l),&
                                center_lat, center_lon)

                                if (gc_filter .eqv. .true.) then
                                    gc = schr2(obs_dist * 1d0 / local_dist)
                                else
                                    gc = schr(obs_dist * 1d0 / local_dist)
                                end if

                                if (gc > 0) then
                                    ! if (pid == 0 .and. nobsl < 100) then
                                    !     print *, obs_dist, i, j, k, l, obs_dist, gc
                                    ! end if
                                    do m = 1, current%obs_nz
                                        do n = 1, current%obs_nvar

                                            if (nobsl(batch_i) < MAX_OBS) then
                                                nobsl(batch_i) = nobsl(batch_i) + 1
                                                yb(:, nobsl(batch_i), batch_i) = current%pro_yb(:, n, m, l)
                                                dis(nobsl(batch_i), batch_i) = current%dis_yo(n, m, l)
                                                rdiaginv(nobsl(batch_i), batch_i) = current%obs_rinv(n, m, l)
                                                rloc(nobsl(batch_i), batch_i) = gc
                                            else
#if (DEBUG == 1)
                                            !call log_notice("number of observations out of limit!")
#endif
                                                goto 10
                                            end if

                                        end do
                                    end do
                                end if
                            10 end do
                        end if
                    end do

#undef current

                end do
            
            !end if

        end do

    end do
! 10 end do

    do batch_i = 1, batch
        padding_nobsl = 1
        do while (nobsl(batch_i) > padding_nobsl)
            padding_nobsl = padding_nobsl * 2
        end do
        do while (nobsl(batch_i) < padding_nobsl)
            nobsl(batch_i) = nobsl(batch_i) + 1

            yb(:, nobsl(batch_i), batch_i) = 0
            dis(nobsl(batch_i), batch_i) = 0
            rdiaginv(nobsl(batch_i), batch_i) = 0
            rloc(nobsl(batch_i), batch_i) = 0
        end do
    end do
    return!generate yb end

end subroutine

subroutine gen_yb_dis(sj, si, ej, ei, nobsl, yb, dis, rdiaginv, rloc)

    integer, intent(in) :: sj, si, ej, ei
    integer, intent(out) :: nobsl

    yb_type, intent(out) :: yb(ens, MAX_OBS)
    ayb_type,intent(out) :: dis(MAX_OBS)
    real(4), intent(out) :: rdiaginv(MAX_OBS)
    yb_type, intent(out) :: rloc(MAX_OBS)

    real(8) :: center_lat, center_lon
    real(8) :: obs_dist
    real(8) :: gc

    integer :: i, j, k, l, m, n
    integer :: padding_nobsl

    center_lat = grid_lat((sj + ej) / 2)
    center_lon = grid_lon((si + ei) / 2)

    nobsl = 0

    do i = -north_n, south_n

        do j = -obs_lc(i)%west_n, obs_lc(i)%east_n

            !if (i .ne. 0 .and. j .ne. 0) then

                do k = 1, obs_type_num

#define current obs_lc(i)%obs_data(k, j)

                    do l = 1, current%obs_num

                        if (abs(current%lat(l)) <= obs_polar) then

                            obs_dist = latlon_dist(current%lat(l), current%lon(l),&
                            center_lat, center_lon)

                            if (gc_filter .eqv. .true.) then
                                gc = schr2(obs_dist * 1d0 / local_dist)
                            else
                                gc = schr(obs_dist * 1d0 / local_dist)
                            end if

                            if (gc > 0) then
                                ! if (pid == 0 .and. nobsl < 100) then
                                !     print *, obs_dist, i, j, k, l, obs_dist, gc
                                ! end if
                                do m = 1, current%obs_nz
                                    do n = 1, current%obs_nvar
                                        nobsl = nobsl + 1

                                        yb(:, nobsl) = current%pro_yb(:, n, m, l)
                                        dis(nobsl) = current%dis_yo(n, m, l)
                                        rdiaginv(nobsl) = current%obs_rinv(n, m, l)
                                        rloc(nobsl) = gc

                                        if (nobsl >= MAX_OBS) then
#if (DEBUG == 1)
                                            !call log_notice("number of observations out of limit!")
#endif
                                            goto 10
                                        end if

                                    end do
                                end do
                            end if

                        end if
                    end do

#undef current

                end do
            
            !end if

        end do

    end do

10  padding_nobsl = 1
    do while (nobsl > padding_nobsl)
        padding_nobsl = padding_nobsl * 2
    end do
    do while (nobsl < padding_nobsl)
        nobsl = nobsl + 1

        yb(:, nobsl) = 0
        dis(nobsl) = 0
        rdiaginv(nobsl) = 0
        rloc(nobsl) = 0
    end do
    !if (mod(nobsl, 2) == 1) then
    !    nobsl = nobsl + 1

    !    yb(:, nobsl) = 0
    !    dis(nobsl) = 0
    !    rdiaginv(nobsl) = 0
    !    rloc(nobsl) = 0
    !end if

    return!generate yb end

end subroutine

! generate yb & ayb matrix

subroutine gen_yb_dis2(sj, si, ej, ei, nobsl, yb, dis, rdiaginv, rloc)

    integer, intent(in) :: sj, si, ej, ei
    integer, intent(out) :: nobsl

    yb_type, intent(out) :: yb(ens, MAX_OBS)
    ayb_type,intent(out) :: dis(MAX_OBS)
    real(4), intent(out) :: rdiaginv(MAX_OBS)
    yb_type, intent(out) :: rloc(MAX_OBS)

    real(8) :: center_lat, center_lon
    real(8) :: obs_dist
    real(8) :: gc

    integer :: i, j, k, l, m, n
    integer :: padding_nobsl

    center_lat = grid_lat((sj + ej) / 2)
    center_lon = grid_lon((si + ei) / 2)

    nobsl = 0

    do i = -north_n, south_n

        do j = -obs_lc(i)%west_n, obs_lc(i)%east_n

            !if (i .ne. 0 .and. j .ne. 0) then

                do k = 1, obs_type_num

#define current obs_lc(i)%obs_data(k, j)

                    call letkf_yb_fast(ens, current%obs_num, current%obs_nz, &
                    current%obs_nvar, current%lat, current%lon, &
                    center_lat, center_lon, local_dist, nobsl, &
                    MAX_OBS, obs_polar, gc_filter, &
                    current%pro_yb, current%dis_yo, current%obs_rinv, &
                    yb, dis, rdiaginv, rloc)

                    if (nobsl >= MAX_OBS) goto 10

#undef current

                end do
            
            !end if

        end do

    end do

10  padding_nobsl = 1
    do while (nobsl > padding_nobsl)
        padding_nobsl = padding_nobsl * 2
    end do
    do while (nobsl < padding_nobsl)
        nobsl = nobsl + 1

        yb(:, nobsl) = 0
        dis(nobsl) = 0
        rdiaginv(nobsl) = 0
        rloc(nobsl) = 0
    end do
    !if (mod(nobsl, 2) == 1) then
    !    nobsl = nobsl + 1

    !    yb(:, nobsl) = 0
    !    dis(nobsl) = 0
    !    rdiaginv(nobsl) = 0
    !    rloc(nobsl) = 0
    !end if

    return!generate yb end

end subroutine

! generate yb & ayb matrix

subroutine gen_yb_dis_vertical(sj, si, ej, ei, nobsl, yb, dis, rdiaginv, rloc, lnp)

    integer, intent(in) :: sj, si, ej, ei
    integer, intent(out) :: nobsl

    yb_type, intent(out) :: yb(ens, MAX_OBS)
    ayb_type,intent(out) :: dis(MAX_OBS)
    real(4), intent(out) :: rdiaginv(MAX_OBS)
    yb_type, intent(out) :: rloc(MAX_OBS)
    yb_type, intent(out) :: lnp(MAX_OBS)

    real(8) :: center_lat, center_lon
    real(8) :: obs_dist
    real(8) :: gc

    integer :: i, j, k, l, m, n

    center_lat = grid_lat((sj + ej) / 2)
    center_lon = grid_lon((si + ei) / 2)

    nobsl = 0

    do i = -north_n, south_n

        do j = -obs_lc(i)%west_n, obs_lc(i)%east_n

    ! do i = 0, 0
    !     do j = 0, 0

            do k = 1, obs_type_num

#define current obs_lc(i)%obs_data(k, j)

                do l = 1, current%obs_num

                    if (abs(current%lat(l)) <= obs_polar) then

                        obs_dist = latlon_dist(current%lat(l), current%lon(l),&
                        center_lat, center_lon)

                        if (gc_filter .eqv. .true.) then
                            gc = schr2(obs_dist * 1d0 / local_dist)
                        else
                            gc = schr(obs_dist * 1d0 / local_dist)
                        end if

                        if (gc > 0) then
                            ! if (pid == 0 .and. nobsl < 100) then
                            !     print *, obs_dist, i, j, k, l, obs_dist, gc
                            ! end if
                            do m = 1, current%obs_nz
                                do n = 1, current%obs_nvar
                                    nobsl = nobsl + 1

                                    yb(:, nobsl) = current%pro_yb(:, n, m, l)
                                    dis(nobsl) = current%dis_yo(n, m, l)
                                    rdiaginv(nobsl) = current%obs_rinv(n, m, l)
                                    lnp(nobsl) = log(current%ph(m, l))
                                    rloc(nobsl) = gc

                                    if (nobsl >= MAX_OBS) then
#if (DEBUG == 1)
                                        !call log_notice("number of observations out of limit!")
#endif
                                        goto 10
                                    end if

                                end do
                            end do
                        end if

                    end if
                end do

#undef current

            end do

        end do

    end do

10  return!generate yb end

end subroutine

subroutine vertical_localization(r_loc_in, r_loc, nobsl, lnp, obs_lnp)

    yb_type, intent(in) :: r_loc_in(nobsl)
    yb_type, intent(out) :: r_loc(nobsl)
    integer, intent(in) :: nobsl

    ayb_type, intent(in) :: lnp
    ayb_type, intent(in) :: obs_lnp(1: nobsl)

    integer :: i

    do i = 1, nobsl

        r_loc(i) = r_loc_in(i) * schr2(abs(lnp - obs_lnp(i)) / vert_dist)

    end do

end subroutine

real(8) function schr2(ri)

    implicit none

    real(8) :: rci,ri,rj,rk

    if (ri.gt.2.0) then
        rci=0.0
    else if (ri.gt.1.0) then
        rj=ri/12.0-0.5
        rj=rj*ri+0.625
        rj=rj*ri+1.666667
        rci=(rj*ri-5.0)*ri+4.0-0.6666667/ri
    else if (ri.gt.0.0) then
        rk=ri*ri
        rj=(-0.25*ri+0.5)*ri+0.625
        rci=(rj*ri-1.666667)*rk+1.0
    else
        rci=1.0
    endif

    schr2=rci

    return
end
real(8) function schr(ri)

    implicit none

    real(8) :: rci,ri,rj,rk

    if (ri.gt.2.0) then
        rci=0.0
    else
        rci=1.0
    endif

    schr=rci

    return
end
