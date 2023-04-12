module ffsl_mod

    use const_mod
    use namelist_mod
    use block_mod
    use parallel_mod
    use adv_batch_mod
    use ppm_mod
    use limiter_mod
    use parallel_mod
    use parallel_types_mod
    use parallel_zonal_mod
    use pa_mod
  
    implicit none
  
    private
  
    public ffsl_init
    public ffsl_calc_mass_hflx
    public ffsl_calc_mass_vflx
    public ffsl_calc_tracer_hflx
    public ffsl_calc_tracer_vflx
    public ffsl_calc_tracer_vflx_lev
  
    interface
      subroutine hflx_interface(block, batch, u, v, mx, my, mfx, mfy)
        import block_type, adv_batch_type, r8
        type(block_type    ), intent(inout) :: block
        type(adv_batch_type), intent(inout) :: batch
        real(r8), intent(in ) :: u  (member_num,                              &
                                     block%mesh%half_ims:block%mesh%half_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
        real(r8), intent(in ) :: v  (member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%half_jms:block%mesh%half_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
        real(r8), intent(in ) :: mx (member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
        real(r8), intent(in ) :: my (member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
        real(r8), intent(out) :: mfx(member_num,                              &
                                     block%mesh%half_ims:block%mesh%half_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
        real(r8), intent(out) :: mfy(member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%half_jms:block%mesh%half_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
      end subroutine hflx_interface
      subroutine vflx_interface(block, batch, w, m, mfz)
        import block_type, adv_batch_type, r8
        type(block_type    ), intent(inout) :: block
        type(adv_batch_type), intent(inout) :: batch
        real(r8), intent(in ) :: w  (member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%half_kms:block%mesh%half_kme)
        real(r8), intent(in ) :: m  (member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
        real(r8), intent(out) :: mfz(member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%half_kms:block%mesh%half_kme)
      end subroutine vflx_interface
      subroutine vflx_lev_interface(block, batch, w, m, mfz)
        import block_type, adv_batch_type, r8
        type(block_type    ), intent(inout) :: block
        type(adv_batch_type), intent(inout) :: batch
        real(r8), intent(in ) :: w  (member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
        real(r8), intent(in ) :: m  (member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%half_kms:block%mesh%half_kme)
        real(r8), intent(out) :: mfz(member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
      end subroutine vflx_lev_interface
    end interface
  
    procedure(hflx_interface    ), pointer :: hflx     => null()
    procedure(vflx_interface    ), pointer :: vflx     => null()
    procedure(vflx_lev_interface), pointer :: vflx_lev => null()
  
  contains
  
    subroutine ffsl_init()
  
      select case (ffsl_flux_type)
      case ('van_leer')
        hflx     => hflx_van_leer
        vflx     => vflx_van_leer
      case ('ppm')
        hflx     => hflx_ppm
        vflx     => vflx_ppm
        vflx_lev => vflx_ppm_lev
      case default
        call log_error('Invalid ffsl_flux_type ' // trim(ffsl_flux_type) // '!')
      end select
  
      call limiter_init()
  
    end subroutine ffsl_init
  
    subroutine ffsl_calc_mass_hflx(block, batch, m, mfx, mfy, dt)
  
      type(block_type    ), intent(inout) :: block
      type(adv_batch_type), intent(inout) :: batch
      real(r8), intent(in ) :: m  (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: mfx(member_num,                              &
                                   block%mesh%half_ims:block%mesh%half_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: mfy(member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%half_jms:block%mesh%half_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(in), optional :: dt
  
      integer i, j, k
      real(r8) work(member_num, block%mesh%full_ids:block%mesh%full_ide,block%mesh%full_nlev)
      real(r8) pole(member_num, block%mesh%full_nlev)
      real(r8) dt_

#ifdef Detail_Time
    call Add_Function("ffsl_calc_mass_hflx")
    call Indent_In()
#endif
  
      dt_ = merge(dt, batch%dt, present(dt))
  
      associate (mesh => block%mesh, &
                 u    => batch%u   , & ! in
                 v    => batch%v   , & ! in
                 divx => batch%divx, & ! in
                 divy => batch%divy, & ! in
                 mx   => batch%qx  , & ! work array
                 my   => batch%qy)     ! work array
      ! Run inner advective operators.
      call hflx(block, batch, u, v, m, m, mfx, mfy)

#ifdef Detail_Time
    call Get_Start_Time(tran_time_start)
#endif
      
      call fill_halo_member(block, mfx, full_lon=.false., full_lat=.true., full_lev=.true., &
                            south_halo=.false., north_halo=.false., &
                            async=block%dstate(1)%async(async_mfx))
      call block%dstate(1)%async(async_mfx)%wait()
      call fill_halo_member(block, mfy, full_lon=.true., full_lat=.false., full_lev=.true., &
                            west_halo=.false.,  east_halo=.false., &
                            async=block%dstate(1)%async(async_mfy))
      call block%dstate(1)%async(async_mfy)%wait()

#ifdef Detail_Time
      call Get_End_Time(tran_time_end)
      tran_time = tran_time + tran_time_end - tran_time_start
      call Get_Start_Time(cal_time_start)
#endif
      
      ! Calculate intermediate tracer density due to advective operators.
      do k = mesh%full_kds, mesh%full_kde
        do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
          do i = mesh%full_ids, mesh%full_ide
            ! Subtract divergence terms from flux to form advective operators.
            mx(:,i,j,k) = m(:,i,j,k) - 0.5_r8 * (          &
              (                                        &
                mfx(:,i,j,k) - mfx(:,i-1,j,k)              &
              ) * mesh%le_lon(j) / mesh%area_cell(j) - &
              divx(:,i,j,k) * m(:,i,j,k)                   &
            ) * dt_
            my(:,i,j,k) = m(:,i,j,k) - 0.5_r8 * (     &
              (                                   &
                mfy(:,i,j  ,k) * mesh%le_lat(j  ) - &
                mfy(:,i,j-1,k) * mesh%le_lat(j-1)   &
              ) / mesh%area_cell(j) -             &
              divy(:,i,j,k) * m(:,i,j,k)              &
            ) * dt_
          end do
        end do
      end do
      ! Handle the Pole boundary conditions.
      if (mesh%has_south_pole()) then
        j = mesh%full_jds
        do k = mesh%full_kds, mesh%full_kde
          do i = mesh%full_ids, mesh%full_ide
            work(:,i,k) = mfy(:,i,j,k)
          end do
        end do
        call zonal_sum_member(proc%zonal_circle, work, pole)
        pole = pole * mesh%le_lat(j) / global_mesh%full_nlon / mesh%area_cell(j)
        do k = mesh%full_kds, mesh%full_kde
          do i = mesh%full_ids, mesh%full_ide
            mx(:,i,j,k) = m(:,i,j,k)
            my(:,i,j,k) = m(:,i,j,k) - 0.5_r8 * (pole(:,k) - divy(:,i,j,k) * m(:,i,j,k)) * dt_
          end do
        end do
      end if
      if (mesh%has_north_pole()) then
        j = mesh%full_jde
        do k = mesh%full_kds, mesh%full_kde
          do i = mesh%full_ids, mesh%full_ide
            work(:,i,k) = mfy(:,i,j-1,k)
          end do
        end do
        call zonal_sum_member(proc%zonal_circle, work, pole)
        pole = pole * mesh%le_lat(j-1) / global_mesh%full_nlon / mesh%area_cell(j)
        do k = mesh%full_kds, mesh%full_kde
          do i = mesh%full_ids, mesh%full_ide
            mx(:,i,j,k) = m(:,i,j,k)
            my(:,i,j,k) = m(:,i,j,k) + 0.5_r8 * (pole(:,k) - divy(:,i,j,k) * m(:,i,j,k)) * dt_
          end do
        end do
      end if

#ifdef Detail_Time
      call Get_End_Time(cal_time_end)
      cal_time = cal_time + cal_time_end - cal_time_start
      call Get_Start_Time(tran_time_start)
#endif

      !call fill_halo(block%halo, mx, full_lon=.true., full_lat=.true., full_lev=.true.,  west_halo=.false.,  east_halo=.false.)
      !call fill_halo(block%halo, my, full_lon=.true., full_lat=.true., full_lev=.true., south_halo=.false., north_halo=.false.)
      
      call fill_halo_member(block, mx, full_lon=.true., full_lat=.true., full_lev=.true., &
                            west_halo=.false.,  east_halo=.false., &
                            async=block%dstate(1)%async(async_mx))
      call block%dstate(1)%async(async_mx)%wait()
      call fill_halo_member(block, my, full_lon=.true., full_lat=.true., full_lev=.true., &
                            south_halo=.false., north_halo=.false., &
                            async=block%dstate(1)%async(async_my))
      call block%dstate(1)%async(async_my)%wait()

#ifdef Detail_Time
      call Get_End_Time(tran_time_end)
      tran_time = tran_time + tran_time_end - tran_time_start
#endif

      ! Run outer flux form operators.
      call hflx(block, batch, u, v, my, mx, mfx, mfy)
      end associate

#ifdef Detail_Time
    call Indent_Out()
#endif
  
    end subroutine ffsl_calc_mass_hflx
  
    subroutine ffsl_calc_mass_vflx(block, batch, m, mfz, dt)
  
      type(block_type    ), intent(inout) :: block
      type(adv_batch_type), intent(inout) :: batch
      real(r8), intent(in ) :: m  (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: mfz(member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%half_kms:block%mesh%half_kme)
      real(r8), intent(in), optional :: dt
  
#ifdef Detail_Time
    call Add_Function("ffsl_calc_mass_vflx")
    call Indent_In()
#endif

      call vflx(block, batch, batch%we, m, mfz)

#ifdef Detail_Time
    call Indent_Out()
#endif
  
    end subroutine ffsl_calc_mass_vflx
  
    subroutine ffsl_calc_tracer_hflx(block, batch, q, qmfx, qmfy, dt)
  
      type(block_type    ), intent(inout) :: block
      type(adv_batch_type), intent(inout) :: batch
      real(r8), intent(in ) :: q    (member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: qmfx (member_num,                              &
                                     block%mesh%half_ims:block%mesh%half_ime, &
                                     block%mesh%full_jms:block%mesh%full_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: qmfy (member_num,                              &
                                     block%mesh%full_ims:block%mesh%full_ime, &
                                     block%mesh%half_jms:block%mesh%half_jme, &
                                     block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(in), optional :: dt
  
      integer i, j, k
      real(r8) work(member_num, block%mesh%full_ids:block%mesh%full_ide,block%mesh%full_nlev)
      real(r8) pole(member_num, block%mesh%full_nlev)
      real(r8) dt_

#ifdef Detail_Time
    call Add_Function("ffsl_calc_tracer_hflx")
    call Indent_In()
#endif
  
      dt_ = merge(dt, batch%dt, present(dt))
  
      associate (mesh => block%mesh, &
                 u    => batch%u   , & ! in
                 v    => batch%v   , & ! in
                 mfx  => batch%mfx , & ! in
                 mfy  => batch%mfy , & ! in
                 divx => batch%divx, & ! in
                 divy => batch%divy, & ! in
                 qx   => batch%qx  , & ! work array
                 qy   => batch%qy)     ! work array
      ! Run inner advective operators.
      call hflx(block, batch, u, v, q, q, qmfx, qmfy)

#ifdef Detail_Time
    call Get_Start_Time(tran_time_start)
#endif
      
      call fill_halo_member(block, qmfx, full_lon=.false., full_lat=.true., full_lev=.true., &
                            south_halo=.false., north_halo=.false., &
                            async=block%dstate(1)%async(async_qmfx))
      call block%dstate(1)%async(async_qmfx)%wait()
      call fill_halo_member(block, qmfy, full_lon=.true., full_lat=.false., full_lev=.true., &
                            west_halo=.false.,  east_halo=.false., &
                            async=block%dstate(1)%async(async_qmfy))
      call block%dstate(1)%async(async_qmfy)%wait()
      
#ifdef Detail_Time
      call Get_End_Time(tran_time_end)
      tran_time = tran_time + tran_time_end - tran_time_start
      call Get_Start_Time(cal_time_start)
#endif
      
      ! Calculate intermediate tracer density due to advective operators.
      do k = mesh%full_kds, mesh%full_kde
        do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
          do i = mesh%full_ids, mesh%full_ide
            ! Subtract divergence terms from flux to form advective operators.
            qx(:,i,j,k) = q(:,i,j,k) - 0.5_r8 * (          &
              (                                        &
                qmfx(:,i,j,k) - qmfx(:,i-1,j,k)            &
              ) * mesh%le_lon(j) / mesh%area_cell(j) - &
              divx(:,i,j,k) * q(:,i,j,k)                   &
            ) * dt_
            qy(:,i,j,k) = q(:,i,j,k) - 0.5_r8 * (      &
              (                                    &
                qmfy(:,i,j  ,k) * mesh%le_lat(j  ) - &
                qmfy(:,i,j-1,k) * mesh%le_lat(j-1)   &
              ) / mesh%area_cell(j) -              &
              divy(:,i,j,k) * q(:,i,j,k)               &
            ) * dt_
          end do
        end do
      end do
      ! Handle the Pole boundary conditions.
      if (mesh%has_south_pole()) then
        j = mesh%full_jds
        do k = mesh%full_kds, mesh%full_kde
          do i = mesh%full_ids, mesh%full_ide
            work(:,i,k) = qmfy(:,i,j,k)
          end do
        end do
        call zonal_sum_member(proc%zonal_circle, work, pole)
        pole = pole * mesh%le_lat(j) / global_mesh%full_nlon / mesh%area_cell(j)
        do k = mesh%full_kds, mesh%full_kde
          do i = mesh%full_ids, mesh%full_ide
            qx(:,i,j,k) = q(:,i,j,k)
            qy(:,i,j,k) = q(:,i,j,k) - 0.5_r8 * (pole(:,k) - divy(:,i,j,k) * q(:,i,j,k)) * dt_
          end do
        end do
      end if
      if (mesh%has_north_pole()) then
        j = mesh%full_jde
        do k = mesh%full_kds, mesh%full_kde
          do i = mesh%full_ids, mesh%full_ide
            work(:,i,k) = qmfy(:,i,j-1,k)
          end do
        end do
        call zonal_sum_member(proc%zonal_circle, work, pole)
        pole = pole * mesh%le_lat(j-1) / global_mesh%full_nlon / mesh%area_cell(j)
        do k = mesh%full_kds, mesh%full_kde
          do i = mesh%full_ids, mesh%full_ide
            qx(:,i,j,k) = q(:,i,j,k)
            qy(:,i,j,k) = q(:,i,j,k) + 0.5_r8 * (pole(:,k) - divy(:,i,j,k) * q(:,i,j,k)) * dt_
          end do
        end do
      end if

#ifdef Detail_Time
      call Get_End_Time(cal_time_end)
      cal_time = cal_time + cal_time_end - cal_time_start
      call Get_Start_Time(tran_time_start)
#endif
           
      call fill_halo_member(block, qx, full_lon=.true., full_lat=.true., full_lev=.true., &
                            west_halo=.false.,  east_halo=.false., &
                            async=block%dstate(1)%async(async_qx))
      call block%dstate(1)%async(async_qx)%wait()
      call fill_halo_member(block, qy, full_lon=.true., full_lat=.true., full_lev=.true., &
                            south_halo=.false., north_halo=.false., &
                            async=block%dstate(1)%async(async_qy))
      call block%dstate(1)%async(async_qy)%wait()

#ifdef Detail_Time
      call Get_End_Time(tran_time_end)
      tran_time = tran_time + tran_time_end - tran_time_start
#endif
      
      ! Run outer flux form operators.
      call hflx(block, batch, mfx, mfy, qy, qx, qmfx, qmfy)

      end associate

#ifdef Detail_Time
    call Indent_Out()
#endif
  
    end subroutine ffsl_calc_tracer_hflx
  
    subroutine ffsl_calc_tracer_vflx(block, batch, q, qmfz, dt)
  
      type(block_type    ), intent(inout) :: block
      type(adv_batch_type), intent(inout) :: batch
      real(r8), intent(in ) :: q   (member_num,                              &
                                    block%mesh%full_ims:block%mesh%full_ime, &
                                    block%mesh%full_jms:block%mesh%full_jme, &
                                    block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: qmfz(member_num,                              &
                                    block%mesh%full_ims:block%mesh%full_ime, &
                                    block%mesh%full_jms:block%mesh%full_jme, &
                                    block%mesh%half_kms:block%mesh%half_kme)
      real(r8), intent(in), optional :: dt

#ifdef Detail_Time
    call Add_Function("ffsl_calc_tracer_vflx")
    call Indent_In()
#endif
  
      call vflx(block, batch, batch%we, q, qmfz)
  
#ifdef Detail_Time
    call Indent_Out()
#endif

    end subroutine ffsl_calc_tracer_vflx
  
    subroutine ffsl_calc_tracer_vflx_lev(block, batch, q, qmfz, dt)
  
      type(block_type    ), intent(inout) :: block
      type(adv_batch_type), intent(inout) :: batch
      real(r8), intent(in ) :: q   (member_num,                              &
                                    block%mesh%full_ims:block%mesh%full_ime, &
                                    block%mesh%full_jms:block%mesh%full_jme, &
                                    block%mesh%half_kms:block%mesh%half_kme)
      real(r8), intent(out) :: qmfz(member_num,                              &
                                    block%mesh%full_ims:block%mesh%full_ime, &
                                    block%mesh%full_jms:block%mesh%full_jme, &
                                    block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(in), optional :: dt

#ifdef Detail_Time
    call Add_Function("ffsl_calc_tracer_vflx_lev")
    call Indent_In()
#endif
  
      call vflx_lev(block, batch, batch%we, q, qmfz)

#ifdef Detail_Time
    call Indent_Out()
#endif
  
    end subroutine ffsl_calc_tracer_vflx_lev
  
    subroutine hflx_van_leer(block, batch, u, v, mx, my, mfx, mfy)
  
      type(block_type    ), intent(inout) :: block
      type(adv_batch_type), intent(inout) :: batch
      real(r8), intent(in ) :: u  (member_num,                              &
                                   block%mesh%half_ims:block%mesh%half_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(in ) :: v  (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%half_jms:block%mesh%half_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(in ) :: mx (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(in ) :: my (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: mfx(member_num,                              &
                                   block%mesh%half_ims:block%mesh%half_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: mfy(member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%half_jms:block%mesh%half_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
  
      integer i, j, k, iu, ju, ci, im
      real(r8) cf, dm

#ifdef Detail_Time
    call Get_Start_Time(cal_time_start)
    call Add_Function("hflx_van_leer")
    call Indent_In()
#endif
  
      associate (mesh => block%mesh, &
                 cflx => batch%cflx, & ! in
                 cfly => batch%cfly)   ! in
      do k = mesh%full_kds, mesh%full_kde
        ! Along x-axis
        do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
          do i = mesh%half_ids, mesh%half_ide
            do im = 1 , member_num
              ci = int(cflx(im,i,j,k))
              cf = cflx(im,i,j,k) - ci
              if (cflx(im,i,j,k) > 0) then
                iu = i - ci
                dm = slope(mx(im,iu-1,j,k), mx(im,iu,j,k), mx(im,iu+1,j,k))
                mfx(im,i,j,k) = u(im,i,j,k) * (cf * (mx(im,iu,j,k) + dm * 0.5_r8 * (1 - cf)) + sum(mx(im,i+1-ci:i,j,k))) / cflx(im,i,j,k)
              else if (cflx(im,i,j,k) < 0) then
                iu = i - ci + 1
                dm = slope(mx(im,iu-1,j,k), mx(im,iu,j,k), mx(im,iu+1,j,k))
                mfx(im,i,j,k) = u(im,i,j,k) * (cf * (mx(im,iu,j,k) - dm * 0.5_r8 * (1 + cf)) - sum(mx(im,i+1:i-ci,j,k))) / cflx(im,i,j,k)
              else
                mfx(im,i,j,k) = 0
              end if
            end do
          end do
        end do
        ! Along y-axis
        do j = mesh%half_jds, mesh%half_jde
          do i = mesh%full_ids, mesh%full_ide
            do im = 1 , member_num
              cf = cfly(im,i,j,k)
              ju = merge(j, j + 1, cf > 0)
              dm = slope(my(im,i,ju-1,k), my(im,i,ju,k), my(im,i,ju+1,k))
              mfy(im,i,j,k) = v(im,i,j,k) * (my(im,i,ju,k) + dm * 0.5_r8 * (sign(1.0_r8, cf) - cf))
            end do
          end do
        end do
      end do
      end associate

#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Indent_Out()
#endif
  
    end subroutine hflx_van_leer
  
    subroutine vflx_van_leer(block, batch, w, m, mfz)
  
      type(block_type    ), intent(inout) :: block
      type(adv_batch_type), intent(inout) :: batch
      real(r8), intent(in ) :: w  (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%half_kms:block%mesh%half_kme)
      real(r8), intent(in ) :: m  (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: mfz(member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%half_kms:block%mesh%half_kme)
  
      integer i, j, k, ku, ci, im
      real(r8) cf, dm

#ifdef Detail_Time
    call Get_Start_Time(cal_time_start)
    call Add_Function("vflx_van_leer")
    call Indent_In()
#endif
  
      associate (mesh => block%mesh, &
                 cflz => batch%cflz)   ! in
      do k = mesh%half_kds + 1, mesh%half_kde - 1
        do j = mesh%full_jds, mesh%full_jde
          do i = mesh%full_ids, mesh%full_ide
            do im = 1 , member_num
                ci = int(cflz(im,i,j,k))
                cf = cflz(im,i,j,k) - ci
              if (cflz(im,i,j,k) > 0) then
                ku = k - ci - 1
                dm = slope(m(im,i,j,ku-1), m(im,i,j,ku), m(im,i,j,ku+1))
                mfz(im,i,j,k) = w(im,i,j,k) * (cf * (m(im,i,j,ku) + dm * 0.5_r8 * (1 - cf)) + sum(m(im,i,j,k-ci:k-1))) / cflz(im,i,j,k)
              else if (cflz(im,i,j,k) < 0) then
                ku = k - ci
                dm = slope(m(im,i,j,ku-1), m(im,i,j,ku), m(im,i,j,ku+1))
                mfz(im,i,j,k) = w(im,i,j,k) * (cf * (m(im,i,j,ku) - dm * 0.5_r8 * (1 + cf)) - sum(m(im,i,j,k:k-ci-1))) / cflz(im,i,j,k)
              else
                mfz(im,i,j,k) = 0
              end if
            end do
          end do
        end do
      end do
      end associate
  
#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Indent_Out()
#endif

    end subroutine vflx_van_leer

    subroutine hflx_ppm(block, batch, u, v, mx, my, mfx, mfy)
  
      type(block_type    ), intent(inout) :: block
      type(adv_batch_type), intent(inout) :: batch
      real(r8), intent(in ) :: u  (member_num,                              &
                                   block%mesh%half_ims:block%mesh%half_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(in ) :: v  (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%half_jms:block%mesh%half_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(in ) :: mx (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(in ) :: my (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: mfx(member_num,                              &
                                   block%mesh%half_ims:block%mesh%half_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: mfy(member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%half_jms:block%mesh%half_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
                                    
  
      integer i, j, k, iu, ju, ci, im
      real(r8) cf, s1, s2, ds1, ds2, ds3

#ifdef Detail_Time
    call Get_Start_Time(cal_time_start)
    call Add_Function("hflx_ppm")
    call Indent_In()
#endif
  
      associate (mesh => block%mesh, &
                 cflx => batch%cflx, & ! in
                 cfly => batch%cfly, & ! in
                 mlx  => batch%qlx , & ! work array
                 mly  => batch%qly , & ! work array
                 dmx  => batch%dqx , & ! work array
                 dmy  => batch%dqy , & ! work array
                 m6x  => batch%q6x , & ! work array
                 m6y  => batch%q6y )   ! work array
      do k = mesh%full_kds, mesh%full_kde
        do j = mesh%full_jds, mesh%full_jde
          do i = mesh%full_ids, mesh%full_ide
            do im = 1 , member_num
              call ppm(mx(im,i-2,j,k), mx(im,i-1,j,k), mx(im,i,j,k), mx(im,i+1,j,k), mx(im,i+2,j,k), mlx(im,i,j,k), dmx(im,i,j,k), m6x(im,i,j,k))
              call ppm(my(im,i,j-2,k), my(im,i,j-1,k), my(im,i,j,k), my(im,i,j+1,k), my(im,i,j+2,k), mly(im,i,j,k), dmy(im,i,j,k), m6y(im,i,j,k))
            end do
          end do
        end do
      end do
      ! call fill_halo(block%halo, mlx, full_lon=.true., full_lat=.true., full_lev=.true., south_halo=.false., north_halo=.false.)
      ! call fill_halo(block%halo, dmx, full_lon=.true., full_lat=.true., full_lev=.true., south_halo=.false., north_halo=.false.)
      ! call fill_halo(block%halo, m6x, full_lon=.true., full_lat=.true., full_lev=.true., south_halo=.false., north_halo=.false.)
      ! call fill_halo(block%halo, mly, full_lon=.true., full_lat=.true., full_lev=.true.,  west_halo=.false.,  east_halo=.false.)
      ! call fill_halo(block%halo, dmy, full_lon=.true., full_lat=.true., full_lev=.true.,  west_halo=.false.,  east_halo=.false.)
      ! call fill_halo(block%halo, m6y, full_lon=.true., full_lat=.true., full_lev=.true.,  west_halo=.false.,  east_halo=.false.)
      
#ifdef Detail_Time
      call Get_End_Time(cal_time_end)
      cal_time = cal_time + cal_time_end - cal_time_start
      call Get_Start_Time(tran_time_start)
#endif

      call fill_halo_member(block, mlx, full_lon=.true., full_lat=.true., full_lev=.true., &
                            south_halo=.false., north_halo=.false., &
                            async=block%dstate(1)%async(async_mlx))
      call block%dstate(1)%async(async_mlx)%wait()
      call fill_halo_member(block, dmx, full_lon=.true., full_lat=.true., full_lev=.true., &
                            south_halo=.false., north_halo=.false., &
                            async=block%dstate(1)%async(async_dmx))
      call block%dstate(1)%async(async_dmx)%wait()
      call fill_halo_member(block, m6x, full_lon=.true., full_lat=.true., full_lev=.true., &
                            south_halo=.false., north_halo=.false., &
                            async=block%dstate(1)%async(async_m6x))
      call block%dstate(1)%async(async_m6x)%wait()

      call fill_halo_member(block, mly, full_lon=.true., full_lat=.true., full_lev=.true., &
                            west_halo=.false.,  east_halo=.false., &
                            async=block%dstate(1)%async(async_mly))
      call block%dstate(1)%async(async_mly)%wait()
      call fill_halo_member(block, dmy, full_lon=.true., full_lat=.true., full_lev=.true., &
                            west_halo=.false.,  east_halo=.false., &
                            async=block%dstate(1)%async(async_dmy))
      call block%dstate(1)%async(async_dmy)%wait()
      call fill_halo_member(block, m6y, full_lon=.true., full_lat=.true., full_lev=.true., &
                            west_halo=.false.,  east_halo=.false., &
                            async=block%dstate(1)%async(async_m6y))
      call block%dstate(1)%async(async_m6y)%wait()

#ifdef Detail_Time
    call Get_End_Time(tran_time_end)
    tran_time = tran_time + tran_time_end - tran_time_start
    call Get_Start_Time(cal_time_start)
#endif
      
      do k = mesh%full_kds, mesh%full_kde
        ! Along x-axis
        do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
          do i = mesh%half_ids, mesh%half_ide
            do im = 1 , member_num
              ci = int(cflx(im,i,j,k))
              cf = cflx(im,i,j,k) - ci
              if (cflx(im,i,j,k) > 0) then
                iu = i - ci
                s1 = 1 - cf
                s2 = 1
                ds1 = s2    - s1
                ds2 = s2**2 - s1**2
                ds3 = s2**3 - s1**3                
                mfx(im,i,j,k) =  u(im,i,j,k) * (sum(mx(im,i+1-ci:i,j,k)) + mlx(im,iu,j,k) * ds1 + 0.5_r8 * dmx(im,iu,j,k) * ds2 + m6x(im,iu,j,k) * (ds2 / 2.0_r8 - ds3 / 3.0_r8)) / cflx(im,i,j,k)
              else if (cflx(im,i,j,k) < 0) then
                iu = i - ci + 1
                s1 = 0
                s2 = -cf
                ds1 = s2    - s1
                ds2 = s2**2 - s1**2
                ds3 = s2**3 - s1**3
                mfx(im,i,j,k) = -u(im,i,j,k) * (sum(mx(im,i+1:i-ci,j,k)) + mlx(im,iu,j,k) * ds1 + 0.5_r8 * dmx(im,iu,j,k) * ds2 + m6x(im,iu,j,k) * (ds2 / 2.0_r8 - ds3 / 3.0_r8)) / cflx(im,i,j,k)
              else
                mfx(im,i,j,k) = 0
              end if
            end do
          end do
        end do
        ! Along y-axis
        do j = mesh%half_jds, mesh%half_jde
          do i = mesh%full_ids, mesh%full_ide
            do im = 1 , member_num
              if (cfly(im,i,j,k) > 0) then
                ju = j
                s1 = 1 - cfly(im,i,j,k)
                s2 = 1
                ds1 = s2    - s1
                ds2 = s2**2 - s1**2
                ds3 = s2**3 - s1**3
                mfy(im,i,j,k) =  v(im,i,j,k) * (mly(im,i,ju,k) * ds1 + 0.5_r8 * dmy(im,i,ju,k) * ds2 + m6y(im,i,ju,k) * (ds2 / 2.0_r8 - ds3 / 3.0_r8)) / cfly(im,i,j,k)
              else if (cfly(im,i,j,k) < 0) then
                ju = j + 1
                s1 = 0
                s2 = -cfly(im,i,j,k)
                ds1 = s2    - s1
                ds2 = s2**2 - s1**2
                ds3 = s2**3 - s1**3
                mfy(im,i,j,k) = -v(im,i,j,k) * (mly(im,i,ju,k) * ds1 + 0.5_r8 * dmy(im,i,ju,k) * ds2 + m6y(im,i,ju,k) * (ds2 / 2.0_r8 - ds3 / 3.0_r8)) / cfly(im,i,j,k)
              else
                mfy(im,i,j,k) = 0
              end if
            end do    
          end do
        end do
      end do
      end associate

#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Indent_Out()
#endif
  
    end subroutine hflx_ppm
  
    subroutine vflx_ppm(block, batch, w, m, mfz)
  
      type(block_type    ), intent(inout) :: block
      type(adv_batch_type), intent(inout) :: batch
      real(r8), intent(in ) :: w  (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%half_kms:block%mesh%half_kme)
      real(r8), intent(in ) :: m  (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(out) :: mfz(member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%half_kms:block%mesh%half_kme)
  
      integer i, j, k, ku, ci, im
      real(r8) cf, s1, s2, ds1, ds2, ds3
  
#ifdef Detail_Time
    call Get_Start_Time(cal_time_start)
    call Add_Function("vflx_ppm")
    call Indent_In()
#endif

      associate (mesh => block%mesh, &
                 cflz => batch%cflz, & ! in
                 mlz  => batch%qlx , & ! work array
                 dmz  => batch%dqx , & ! work array
                 m6z  => batch%q6x )   ! work array
      do k = mesh%full_kds, mesh%full_kde
        do j = mesh%full_jds, mesh%full_jde
          do i = mesh%full_ids, mesh%full_ide
            do im = 1 , member_num
                call ppm(m(im,i,j,k-2), m(im,i,j,k-1), m(im,i,j,k), m(im,i,j,k+1), m(im,i,j,k+2), mlz(im,i,j,k), dmz(im,i,j,k), m6z(im,i,j,k))
            end do
          end do
        end do
      end do
      do k = mesh%half_kds + 1, mesh%half_kde - 1
        do j = mesh%full_jds, mesh%full_jde
          do i = mesh%full_ids, mesh%full_ide
            do im = 1 , member_num
              ci = int(cflz(im,i,j,k))
              cf = cflz(im,i,j,k) - ci
              if (cflz(im,i,j,k) > 0) then
                ku = k - ci - 1
                s1 = 1 - cf
                s2 = 1
                ds1 = s2    - s1
                ds2 = s2**2 - s1**2
                ds3 = s2**3 - s1**3
                mfz(im,i,j,k) =  w(im,i,j,k) * (sum(m(im,i,j,k-ci:k-1)) + mlz(im,i,j,ku) * ds1 + 0.5_r8 * dmz(im,i,j,ku) * ds2 + m6z(im,i,j,ku) * (ds2 / 2.0_r8 - ds3 / 3.0_r8)) / cflz(im,i,j,k)
              else if (cflz(im,i,j,k) < 0) then
                ku = k - ci
                s1 = 0
                s2 = -cf
                ds1 = s2    - s1
                ds2 = s2**2 - s1**2
                ds3 = s2**3 - s1**3
                mfz(im,i,j,k) = -w(im,i,j,k) * (sum(m(im,i,j,k:k-ci-1)) + mlz(im,i,j,ku) * ds1 + 0.5_r8 * dmz(im,i,j,ku) * ds2 + m6z(im,i,j,ku) * (ds2 / 2.0_r8 - ds3 / 3.0_r8)) / cflz(im,i,j,k)
              else
                mfz(im,i,j,k) = 0
              end if
            end do
          end do
        end do
      end do
      end associate

#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Indent_Out()
#endif
  
    end subroutine vflx_ppm
  
    subroutine vflx_ppm_lev(block, batch, w, m, mfz)
  
      type(block_type    ), intent(inout) :: block
      type(adv_batch_type), intent(inout) :: batch
      real(r8), intent(in ) :: w  (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
      real(r8), intent(in ) :: m  (member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%half_kms:block%mesh%half_kme)
      real(r8), intent(out) :: mfz(member_num,                              &
                                   block%mesh%full_ims:block%mesh%full_ime, &
                                   block%mesh%full_jms:block%mesh%full_jme, &
                                   block%mesh%full_kms:block%mesh%full_kme)
  
      integer i, j, k, ku, ci, im
      real(r8) cf, s1, s2, ds1, ds2, ds3
  
#ifdef Detail_Time
    call Get_Start_Time(cal_time_start)
    call Add_Function("vflx_ppm_lev")
    call Indent_In()
#endif

      associate (mesh => block%mesh, &
                 cflz => batch%cflz, & ! in
                 mlz  => batch%qlx , & ! work array
                 dmz  => batch%dqx , & ! work array
                 m6z  => batch%q6x )   ! work array
      do k = mesh%half_kds, mesh%half_kde
        do j = mesh%full_jds, mesh%full_jde
          do i = mesh%full_ids, mesh%full_ide
            do im = 1 , member_num
              call ppm(m(im,i,j,k-2), m(im,i,j,k-1), m(im,i,j,k), m(im,i,j,k+1), m(im,i,j,k+2), mlz(im,i,j,k), dmz(im,i,j,k), m6z(im,i,j,k))
            end do
          end do
        end do
      end do
      do k = mesh%full_kds, mesh%full_kde
        do j = mesh%full_jds, mesh%full_jde
          do i = mesh%full_ids, mesh%full_ide
            do im = 1 , member_num
              ci = int(cflz(im,i,j,k))
              cf = cflz(im,i,j,k) - ci
              if (cflz(im,i,j,k) > 0) then
                ku = k - ci
                s1 = 1 - cf
                s2 = 1
                ds1 = s2    - s1
                ds2 = s2**2 - s1**2
                ds3 = s2**3 - s1**3
                mfz(im,i,j,k) =  w(im,i,j,k) * (sum(m(im,i,j,k-ci+1:k)) + mlz(im,i,j,ku) * ds1 + 0.5_r8 * dmz(im,i,j,ku) * ds2 + m6z(im,i,j,ku) * (ds2 / 2.0_r8 - ds3 / 3.0_r8)) / cflz(im,i,j,k)
              else if (cflz(im,i,j,k) < 0) then
                ku = k - ci + 1
                s1 = 0
                s2 = -cf
                ds1 = s2    - s1
                ds2 = s2**2 - s1**2
                ds3 = s2**3 - s1**3
                mfz(im,i,j,k) = -w(im,i,j,k) * (sum(m(im,i,j,k+1:k-ci)) + mlz(im,i,j,ku) * ds1 + 0.5_r8 * dmz(im,i,j,ku) * ds2 + m6z(im,i,j,ku) * (ds2 / 2.0_r8 - ds3 / 3.0_r8)) / cflz(im,i,j,k)
              else
                mfz(im,i,j,k) = (m(im,i,j,k) + m(im,i,j,k+1)) * 0.5_r8
              end if
            end do
          end do
        end do
      end do
      end associate
  
#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Indent_Out()
#endif

    end subroutine vflx_ppm_lev
  
  end module ffsl_mod
  