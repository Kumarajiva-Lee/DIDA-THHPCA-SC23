module pgf_lin97_mod

  use flogger
  use const_mod
  use namelist_mod
  use parallel_mod
  use parallel_types_mod
  use block_mod
  use pa_mod

  implicit none

contains

  subroutine pgf_lin97_prepare(block, dstate)

    type(block_type), intent(in) :: block
    type(dstate_type), intent(inout) :: dstate

  end subroutine pgf_lin97_prepare

  subroutine pgf_lin97_run(block, dstate, dtend)

    type(block_type), intent(inout), target :: block
    type(dstate_type), intent(inout) :: dstate
    type(dtend_type), intent(inout) :: dtend

    real(r8) L(member_num), dph1(member_num), dph2(member_num), dgz1(member_num), dgz2(member_num), dp1(member_num), dp2(member_num), dpp1(member_num), dpp2(member_num), dpdph(member_num)
    integer i, j, k, move, rf

    !                    o
    !                   /|
    !                  / |
    !                 /  |
    !                /   |
    !   o-----------/------------o
    !   |          /|            |
    !   |         / |            |
    !   |        /  |            |
    !   |       /   |            |
    !   |      o    |            |
    !   o------|    -------------o
    !          |   /
    !          |  /
    !          | /
    !          |/
    !          o

#ifdef Detail_Time
    call Get_Start_Time(tran_time_start)
    call Add_Function("pgf_lin97_run")
    call Indent_In()
#endif

    call dstate%async(async_gz_lev)%wait()
    call dstate%async(async_ph_exn_lev)%wait()

#ifdef Detail_Time
    call Get_End_Time(tran_time_end)
    tran_time = tran_time + tran_time_end - tran_time_start
    call Get_Start_Time(cal_time_start)
#endif

    associate (mesh       => block%mesh       , & ! in
      qm         => dstate%qm        , & ! in
      ph_exn_lev => dstate%ph_exn_lev, & ! in
      ph_lev     => dstate%ph_lev    , & ! in
      gz_lev     => dstate%gz_lev    , & ! in
      p_lev      => dstate%p_lev     , & ! in
      pgf_lon    => dtend%pgf_lon    , & ! out
      pgf_lat    => dtend%pgf_lat)       ! out

      

      if (hydrostatic) then
        do k = mesh%full_kds, mesh%full_kde
        !
        !   4             3
        ! i,j,k        i+1,j,k
        !   o-------------o
        !   |             |
        !   |             |
        !   |    i,j,k    |
        !   |             |
        !   |             |
        !   o-------------o
        ! i,j,k+1      i+1,j,k+1  --> east
        !   1             2
        !
          do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
            do i = mesh%half_ids, mesh%half_ide
              L = 1 + 0.5_r8 * (qm(:,i,j,k) + qm(:,i+1,j,k))
              dph1 = ph_exn_lev(:,i+1,j,k+1) - ph_exn_lev(:,i  ,j,k  ) ! 2 - 4
              dph2 = ph_exn_lev(:,i  ,j,k+1) - ph_exn_lev(:,i+1,j,k  ) ! 1 - 3
              dgz1 = gz_lev    (:,i  ,j,k+1) - gz_lev    (:,i+1,j,k  ) ! 1 - 3
              dgz2 = gz_lev    (:,i  ,j,k  ) - gz_lev    (:,i+1,j,k+1) ! 4 - 2
              pgf_lon(:,i,j,k) = -(dph1 * dgz1 + dph2 * dgz2) / mesh%de_lon(j) / (dph1 + dph2) / L
            end do
          end do
          !
          !   4             3
          ! i,j,k        i,j+1,k
          !   o-------------o
          !   |             |
          !   |             |
          !   |    i,j,k    |
          !   |             |
          !   |             |
          !   o-------------o
          ! i,j,k+1      i,j+1,k+1  --> north
          !   1             2
          !
          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              L = 1 + 0.5_r8 * (qm(:,i,j,k) + qm(:,i,j+1,k))
              dph1 = ph_exn_lev(:,i,j+1,k+1) - ph_exn_lev(:,i,j  ,k  ) ! 2 - 4
              dph2 = ph_exn_lev(:,i,j  ,k+1) - ph_exn_lev(:,i,j+1,k  ) ! 1 - 3
              dgz1 = gz_lev    (:,i,j  ,k+1) - gz_lev    (:,i,j+1,k  ) ! 1 - 3
              dgz2 = gz_lev    (:,i,j  ,k  ) - gz_lev    (:,i,j+1,k+1) ! 4 - 2
              pgf_lat(:,i,j,k) = -(dph1 * dgz1 + dph2 * dgz2) / mesh%de_lat(j) / (dph1 + dph2) / L
            end do
          end do
        end do
      else if (nonhydrostatic) then
        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
            do i = mesh%half_ids, mesh%half_ide
              dpp1 = p_lev     (:,i+1,j,k+1) - p_lev     (:,i  ,j,k  ) - ( &
                      ph_lev    (:,i+1,j,k+1) - ph_lev    (:,i  ,j,k  )) ! 2 - 4
              dpp2 = p_lev     (:,i  ,j,k+1) - p_lev     (:,i+1,j,k  ) - ( &
                      ph_lev    (:,i  ,j,k+1) - ph_lev    (:,i+1,j,k  )) ! 1 - 3
              dph1 = ph_exn_lev(:,i+1,j,k+1) - ph_exn_lev(:,i  ,j,k  )  ! 2 - 4
              dph2 = ph_exn_lev(:,i  ,j,k+1) - ph_exn_lev(:,i+1,j,k  )  ! 1 - 3
              dp1  = ph_lev    (:,i+1,j,k+1) - ph_lev    (:,i  ,j,k  )  ! 2 - 4
              dp2  = ph_lev    (:,i  ,j,k+1) - ph_lev    (:,i+1,j,k  )  ! 1 - 3
              dgz1 = gz_lev    (:,i  ,j,k+1) - gz_lev    (:,i+1,j,k  )  ! 1 - 3
              dgz2 = gz_lev    (:,i  ,j,k  ) - gz_lev    (:,i+1,j,k+1)  ! 4 - 2
              pgf_lon(:,i,j,k) = -(                             &
                (dph1 * dgz1 + dph2 * dgz2) / (dph1 + dph2) + &
                (dpp1 * dgz1 + dpp2 * dgz2) / (dp1  + dp2 )   & ! Nonhydrostatic part
              ) / mesh%de_lon(j)
            end do
          end do
          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              dpp1 = p_lev     (:,i,j+1,k+1) - p_lev     (:,i,j  ,k  ) - ( &
                      ph_lev    (:,i,j+1,k+1) - ph_lev    (:,i,j  ,k  )) ! 2 - 4
              dpp2 = p_lev     (:,i,j  ,k+1) - p_lev     (:,i,j+1,k  ) - ( &
                      ph_lev    (:,i,j  ,k+1) - ph_lev    (:,i,j  ,k  )) ! 1 - 3
              dph1 = ph_exn_lev(:,i,j+1,k+1) - ph_exn_lev(:,i,j  ,k  )  ! 2 - 4
              dph2 = ph_exn_lev(:,i,j  ,k+1) - ph_exn_lev(:,i,j+1,k  )  ! 1 - 3
              dp1  = ph_lev    (:,i,j+1,k+1) - ph_lev    (:,i,j  ,k  )  ! 2 - 4
              dp2  = ph_lev    (:,i,j  ,k+1) - ph_lev    (:,i,j+1,k  )  ! 1 - 3
              dgz1 = gz_lev    (:,i,j  ,k+1) - gz_lev    (:,i,j+1,k  )  ! 1 - 3
              dgz2 = gz_lev    (:,i,j  ,k  ) - gz_lev    (:,i,j+1,k+1)  ! 4 - 2
              pgf_lat(:,i,j,k) = -(                             &
                (dph1 * dgz1 + dph2 * dgz2) / (dph1 + dph2) + &
                (dpp1 * dgz1 + dpp2 * dgz2) / (dp1  + dp2 )   & ! Nonhydrostatic part
              ) / mesh%de_lat(j)
            end do
          end do
        end do
      end if
      end associate

#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Indent_Out()
#endif

  end subroutine pgf_lin97_run

end module pgf_lin97_mod