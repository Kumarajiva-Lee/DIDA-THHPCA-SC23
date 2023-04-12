module gmcore_mod
 
  use flogger
  use string
  use const_mod
  use namelist_mod
  use parallel_mod
  use parallel_types_mod
  use process_mod
  use datetime
  use time_mod, old => old_time_idx, new => new_time_idx
  use history_mod
  use restart_mod
  use initial_mod
  use prepare_mod
  use block_mod
  use dynamics_types_mod
  use diag_state_mod
  use vert_coord_mod
  use time_schemes_mod
  use operators_mod
  use interp_mod
  use debug_mod
  use adv_mod
  use moist_mod
  use pgf_mod
  use damp_mod
  use diag_state_mod
  use pa_mod
  use member_mod
  

  ! use mountain_zonal_flow_test_mod
  ! use rossby_haurwitz_wave_test_mod
  ! use jet_zonal_flow_test_mod
  ! use steady_geostrophic_flow_test_mod
  ! use cross_pole_flow_test_mod
  ! use shallow_water_waves_test_mod
  ! use vortex_erosion_test_mod
  ! use steady_state_test_mod
  ! use rossby_haurwitz_wave_3d_test_mod
  ! use mountain_wave_test_mod
  ! use baroclinic_wave_test_mod
  ! use held_suarez_test_mod
  ! use steady_state_pgf_test_mod
  ! use ksp15_test_mod
  ! use dcmip31_test_mod
  ! use mars_cold_run_mod


  use gmcore_coupler, only: init_gmcore_coupler,gmcore2da_send_coupling,gmcore2da_recv_coupling,clean_gmcore_coupler

  implicit none

  private

  public atm_init
  public atm_run
  public atm_final

  interface
    subroutine splitter_interface(dt, blocks)
      import block_type
      real(8), intent(in) :: dt
      type(block_type), intent(inout) :: blocks(:)
    end subroutine splitter_interface

    subroutine set_ic_interface(block)
      import block_type
      type(block_type), intent(inout), target :: block
    end subroutine set_ic_interface
  end interface

  procedure(splitter_interface), pointer :: splitter
  procedure(set_ic_interface), pointer :: set_ic
  procedure(space_operators_interface), pointer :: operators

contains

  subroutine atm_init(namelist_path , comm , group_id , proc_lon_ibeg , proc_lon_iend , proc_lat_ibeg , proc_lat_iend)

    character(*), intent(in) :: namelist_path
    integer , intent(in) :: comm
    integer , intent(in) , optional :: group_id
    integer , intent(inout) , optional :: proc_lon_ibeg
    integer , intent(inout) , optional :: proc_lon_iend
    integer , intent(inout) , optional :: proc_lat_ibeg
    integer , intent(inout) , optional :: proc_lat_iend

    character(10) time_value, time_units
    real(r8) seconds
    real(8) :: new_second
    integer :: iblk
    integer :: im, iv, now_ensemble
    integer origin_time_step 
    integer :: ierr
    type(datetime_type) origin_start_time
    type(datetime_type) origin_end_time

    ivector = 1
    call parse_namelist(namelist_path)
    call const_init(planet)
    call log_init()
    call global_mesh%init_global(nlon, nlat, nlev, &
                                 lon_hw = 2, lat_hw = 2)
    call process_init(comm)  
    call process_init_zonal()
    if (is_root_proc()) call print_namelist()
    call vert_coord_init(nlev, namelist_path)
    call process_create_blocks_stage2
    if (present(group_id)) then
      call pa_init(proc%comm,group_id);
    else
      call pa_init(proc%comm)
    endif
    call time_init(dt_dyn)
    !call diag_state_init(blocks)
    if (present(group_id)) then
      call history_init(group_id)
    else
      call history_init()
    endif
    call restart_init()
    call time_scheme_init()
    if (use_adv) then
      call adv_init()
    end if
    call pgf_init()
    call interp_init()
    call operators_init()
    call damp_init(blocks) 
    if (use_phy) then
      if (baroclinic) call moist_init()
    end if
    if (use_adv) then
      call adv_allocate_tracers(blocks)
    end if

#ifndef Only_Gmcore
    call init_gmcore_coupler(proc%comm, blocks(1)%dstate(1))
#endif

    if (present(proc_lon_ibeg)) proc_lon_ibeg = proc%lon_ibeg
    if (present(proc_lon_iend)) proc_lon_iend = proc%lon_iend
    if (present(proc_lat_ibeg)) proc_lat_ibeg = proc%lat_ibeg
    if (present(proc_lat_iend)) proc_lat_iend = proc%lat_iend

    operators => space_operators

    ! if (initial_file == 'N/A' .and. .not. restart) then
    !   select case (test_case)
    !   case ('pgf_test')
    !     call steady_state_pgf_test_set_params()
    !   case ('ksp15_01', 'ksp15_02')
    !     call ksp15_test_set_params()
    !   case ('dcmip31')
    !     call dcmip31_test_set_params()
    !   end select
    ! end if

    call MPI_Barrier(comm, ierr)


    if (initial_file /= 'N/A') then
      if (present(group_id)) then
        call initial_read(initial_file, group_id)
      else
        call initial_read(initial_file)
      endif
    else if (topo_file /= 'N/A' .and. bkg_file /= 'N/A') then

      call topo_read(topo_file)
      do iblk = 1, size(blocks)
        call topo_regrid(blocks(iblk))
      end do
    
      if (use_topo_smooth) then
        do iblk = 1, size(blocks)
          call topo_smooth(blocks(iblk))
        end do
      end if
      
      if (present(group_id)) then
        call prepare_run(group_id_in = group_id)
      else
        call prepare_run()
      end if
    else if (restart) then
      if (present(group_id)) then
        call restart_read(group_id)
      else
        call restart_read()
      end if
    else
      select case (test_case)
      ! case ('steady_state')
      !   set_ic => steady_state_test_set_ic
      ! case ('rossby_haurwitz_wave')
      !   set_ic => rossby_haurwitz_wave_3d_test_set_ic
      ! case ('mountain_wave')
      !   set_ic => mountain_wave_test_set_ic
      ! case ('baroclinic_wave')
      !   set_ic => baroclinic_wave_test_set_ic
      ! case ('held_suarez')
      !   set_ic => held_suarez_test_set_ic
      ! case ('pgf_test')
      !   set_ic => steady_state_pgf_test_set_ic
      ! case ('ksp15_01')
      !   set_ic => ksp15_01_test_set_ic
      ! case ('ksp15_02')
      !   set_ic => ksp15_02_test_set_ic
      ! case ('dcmip31')
      !   set_ic => dcmip31_test_set_ic
      ! case ('mars_cold_run')
      !   !set_ic => mars_cold_run_set_ic
      ! case ('mountain_zonal_flow')
      !   set_ic => mountain_zonal_flow_test_set_ic
      ! case ('rossby_haurwitz_wave_swm')
      !   set_ic => rossby_haurwitz_wave_test_set_ic
      ! case ('jet_zonal_flow')
      !   set_ic => jet_zonal_flow_test_set_ic
      ! case ('steady_geostrophic_flow')
      !   set_ic => steady_geostrophic_flow_test_set_ic
      ! case ('cross_pole_flow')
      !   set_ic => cross_pole_flow_test_set_ic
      ! case ('shallow_water_waves')
      !   set_ic => shallow_water_waves_test_set_ic
      ! case ('vortex_erosion')
        !set_ic => vortex_erosion_test_set_ic
      case default
        call log_error('Unknown test case ' // trim(test_case) // '!')
      end select

      do iblk = 1, size(blocks)
        call set_ic(blocks(iblk))
      end do

    end if


    time_value = split_string(print_interval, ' ', 1)
    time_units = split_string(print_interval, ' ', 2)
    read(time_value, *) seconds
    select case (time_units)
    case ('days')
      seconds = seconds * 86400
    case ('hours')
      seconds = seconds * 3600
    case ('minutes')
      seconds = seconds * 60
    case ('seconds')
      seconds = seconds
    case default
      call log_error('Invalid print interval ' // trim(print_interval) // '!')
    end select

    call time_add_alert('print', seconds=seconds, first_alert=.true.)

#ifndef Only_Gmcore
    new_second = da_in_seconds
    call time_add_alert('with_letkf', seconds=new_second, first_alert=.false.)
#endif

    if (is_root_proc()) call log_notice("Finish atm_init")

   
  end subroutine atm_init

  subroutine atm_run(comm , group_id)

    integer, intent(in) :: comm
    integer , intent(in) , optional :: group_id
    
    integer iter_member, iv
    integer flag , now_ensemble
    integer tmp 
    integer iblk, itime
    logical new_file 
    integer da_cycle, ierr
    real*8 cycle_start, cycle_end, cycle_mid

    da_cycle = 0

#ifdef Detail_Time
    call Add_Function("Gmcore_Run")
    call Indent_In()
#endif


    if (is_root_proc()) call log_notice("Begin atm_run")
    new_file = .true.

    ! if (is_root_proc()) then
    !   write(*,*) blocks(1)%mesh%full_cos_lon(-1),blocks(1)%mesh%full_cos_lon(0),blocks(1)%mesh%full_cos_lon(1),blocks(1)%mesh%full_cos_lon(360),blocks(1)%mesh%full_cos_lon(361),blocks(1)%mesh%full_cos_lon(362)
    !   write(*,*) blocks(1)%mesh%full_sin_lon(-1),blocks(1)%mesh%full_sin_lon(0),blocks(1)%mesh%full_sin_lon(1),blocks(1)%mesh%full_sin_lon(360),blocks(1)%mesh%full_sin_lon(361),blocks(1)%mesh%full_sin_lon(362)
    !   write(*,*) blocks(1)%mesh%half_cos_lon(-1),blocks(1)%mesh%half_cos_lon(0),blocks(1)%mesh%half_cos_lon(1),blocks(1)%mesh%half_cos_lon(360),blocks(1)%mesh%half_cos_lon(361),blocks(1)%mesh%half_cos_lon(362)
    !   write(*,*) blocks(1)%mesh%half_sin_lon(-1),blocks(1)%mesh%half_sin_lon(0),blocks(1)%mesh%half_sin_lon(1),blocks(1)%mesh%half_sin_lon(360),blocks(1)%mesh%half_sin_lon(361),blocks(1)%mesh%half_sin_lon(362)
    ! end if

    call MPI_Barrier(comm, ierr)
    call Get_Time_Pa(total_time_start)

    do iblk = 1, size(blocks)
      associate (block => blocks(iblk)     , &
                 mesh  => blocks(iblk)%mesh, &
                 state => blocks(iblk)%dstate(old))
        if (baroclinic) then 
          call prepare_static(block)
            ! Ensure bottom gz_lev is the same as gzs.
            do itime = lbound(block%dstate, 1), ubound(block%dstate, 1)
              block%dstate(itime)%gz_lev(:,:,:,global_mesh%half_kde) = block%static%gzs
            end do
        end if
        call block%dstate(old)%c2a()
        if (use_phy) then
          if (baroclinic) call moist_link_state(block)
        end if

      end associate

    end do


    call operators_prepare(blocks, old, dt_dyn)
    if (use_adv) then
      call adv_prepare(old)
    end if

    ! if (nonhydrostatic) call nh_prepare(blocks)
    call diagnose(blocks, old, 1)
    if (present(group_id)) then
      call output(old,group_id,new_file_in = .true.)
    else
      call output(old,new_file_in = .true.)
    endif




    if (is_root_proc() .and. time_is_alerted('print')) call log_print_diag(curr_time%isoformat())
    
    !if (is_root_proc()) call log_notice("Here!!!!!!!")

    do while (.not. time_is_finished())

      do iblk = 1, size(blocks)
        call time_integrator(operators, blocks(iblk), old, new, dt_dyn)
        call damp_run(blocks(iblk), blocks(iblk)%dstate(new), blocks(iblk)%dtend(new), dt_dyn)
        call blocks(iblk)%dstate(new)%c2a()
      end do

     
      

#ifndef Only_Gmcore
        !fix ivector
        if (time_is_alerted('with_letkf')) then
          da_cycle = da_cycle + 1
          if (da_cycle == 1) then
            call MPI_Barrier(comm, ierr)
            call Get_Time_Pa(cycle_start)
          end if
          if (da_cycle == 2) then
           call MPI_Barrier(comm, ierr)
           call Get_Time_Pa(cycle_end)
          end if

          
          call gmcore2da_send_coupling(comm, blocks(1), blocks(1)%dstate(new))
          call gmcore2da_recv_coupling(comm, blocks(1), blocks(1)%dstate(new))
          if (da_cycle == 1) then
            call MPI_Barrier(comm, ierr)
            call Get_Time_Pa(cycle_mid)
          end if

        
        end if
#endif

        call time_advance(dt_dyn)
        
        if (use_adv) then
          do iblk = 1, size(blocks)
            call adv_run(blocks(iblk), old)
          end do
        end if

        if (use_phy) then
          if (baroclinic) then
            do iblk = 1, size(blocks)
              !call test_forcing_run(blocks(iblk), dt_dyn, blocks(iblk)%static, blocks(iblk)%dstate(old))
              call moist_link_state(blocks(iblk))
              !call physics_run_after_dynamics(blocks(iblk), old, dt_phys)
            end do
          end if
        end if

        call diagnose(blocks, old , 1)
        if (is_root_proc() .and. time_is_alerted('print')) call log_print_diag(curr_time%isoformat())
        if (present(group_id)) then
          call output(old,group_id)
        else
          call output(old)
        endif
    end do

    !call operators_prepare(blocks, old, dt_in_seconds)
    !call diagnose(blocks, old , 1)

    call Get_Time_Pa(total_time_end)

    if (is_root_proc()) call log_notice("Finish atm_run")
    if (is_root_proc()) call log_notice('Atm_run time cost ' // to_str(total_time_end - total_time_start, 5) // ' seconds.')
    if (is_root_proc()) call log_notice('DA Cycle time cost ' // to_str(cycle_end - cycle_start, 6) // ' seconds.')
    if (is_root_proc()) call log_notice('DA Cycle ATM cost ' // to_str(cycle_end - cycle_mid, 6) // ' seconds.')
    if (is_root_proc()) call log_notice('DA Cycle DA cost ' // to_str(cycle_mid - cycle_start, 6) // ' seconds.')

#ifdef Detail_Time
  call Indent_Out()
#endif
    

  end subroutine atm_run

  subroutine atm_final()

#ifndef Only_Gmcore
    call clean_gmcore_coupler()
#endif
    call interp_final()
    if (use_adv) then
      call adv_final()
    end if
    call damp_final()
    !call diag_state_final()
    call history_final()
#ifdef Detail_Time
    call pa_final()
#endif
    call process_final()

    if (is_root_proc()) call log_notice("Finish atm_final")

  end subroutine atm_final

  subroutine output(itime , group_id , new_file_in)

    integer, intent(in) :: itime
    integer , intent(in) , optional :: group_id
    logical , intent(in) , optional :: new_file_in

    real(8), save :: time1 = 0, time2
    integer i, j, k, iblk
    real*8 io_time_start , io_time_end
    logical new_file

    if (time_is_alerted('history_write')) then
      if (time_step == 0) time1 = mpi_wtime()
      time2 = mpi_wtime()
      if (time_step /= 0) then
        if (is_root_proc()) call log_notice('Time cost ' // to_str(time2 - time1, 5) // ' seconds.')
        time1 = time2
      end if

      if (present(new_file_in)) then
        new_file = new_file_in
      else
        new_file = .false.
      end if

#ifdef Detail_io_Time
    io_time_start = mpi_wtime()
#endif

      if (output_h0) then
        if (present(group_id)) then
          call history_write_state(blocks, itime,group_id,new_file = new_file)
          if (output_h1) call history_write_debug(blocks, itime ,group_id,new_file = new_file)
        else
          call history_write_state(blocks, itime,new_file = new_file)
          if (output_h1) call history_write_debug(blocks, itime,new_file = new_file)
        endif
      end if

#ifdef Detail_io_Time
    io_time_end = mpi_wtime()
    write((23333 + myid),*) , io_time_end - io_time_start
#endif

    end if
    if (time_is_alerted('restart_write')) then
      if (present(group_id)) then
        call restart_write(itime , group_id)
      else
        call restart_write(itime)
      end if
    end if

  end subroutine output

  subroutine diagnose(blocks, itime , member_output)

    type(block_type), intent(inout), target :: blocks(:)
    integer, intent(in) :: itime
    integer, intent(in) :: member_output

    type(mesh_type), pointer :: mesh
    type(dstate_type), pointer :: dstate
    type(static_type), pointer :: static
    integer i, j, k, iblk
    real(r8) tm(member_num), te(member_num), tav(member_num), tpe(member_num), te_ke(member_num), te_ie(member_num), te_pe(member_num)

    tm = 0.0_r8
    te = 0.0_r8
    tav = 0.0_r8
    tpe = 0.0_r8
    te_ke = 0.0_r8
    te_ie = 0.0_r8
    te_pe = 0.0_r8
    do iblk = 1, size(blocks)
      mesh => blocks(iblk)%mesh
      dstate => blocks(iblk)%dstate(itime)
      static => blocks(iblk)%static

      do k = mesh%full_kds, mesh%full_kde
        do j = mesh%full_jds, mesh%full_jde
          do i = mesh%full_ids, mesh%full_ide
            tm(:) = tm(:) + dstate%m(:,i,j,k) * mesh%area_cell(j)
          end do
        end do
      end do

      call dstate%async(async_mfx_lon)%wait()
      call dstate%async(async_u_lon)%wait()
      call dstate%async(async_mfy_lat)%wait()
      call dstate%async(async_v_lat)%wait()

      do k = mesh%full_kds, mesh%full_kde
        do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
          do i = mesh%half_ids, mesh%half_ide
            te_ke(:) = te_ke(:) + dstate%mfx_lon(:,i,j,k) * 0.5_r8 * dstate%u_lon(:,i,j,k) * mesh%area_lon(j) * 2
            !if (i == 1 .and. j == 2 .and. k == 1 .and. time_step == 0) write(*,*) , "te_1",te_ke(1),dstate%mfx_lon(1,i,j,k),dstate%u_lon(1,i,j,k),mesh%area_lon(j)
          end do
        end do
        do j = mesh%half_jds, mesh%half_jde
          do i = mesh%full_ids, mesh%full_ide
            te_ke(:) = te_ke(:) + dstate%mfy_lat(:,i,j,k) * 0.5_r8 * dstate%v_lat(:,i,j,k) * mesh%area_lat(j) * 2
            !if (i == 1 .and. j == 2 .and. k == 1 .and. time_step == 0) write(*,*) , "te_2", te_ke(1),dstate%mfy_lat(1,i,j,k),dstate%v_lat(1,i,j,k),mesh%area_lat(j)
          end do
        end do
      end do

      if (hydrostatic) then

        call dstate%async(async_t)%wait()
        call dstate%async(async_phs)%wait()

        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%full_ids, mesh%full_ide
              te_ie(:) = te_ie(:) + dstate%m(:,i,j,k) * cpd * dstate%t(:,i,j,k) * mesh%area_cell(j)
              !if (i == 1 .and. j == 2 .and. k == 1 .and. time_step == 0) write(*,*) , "te_3",te_ie(1), dstate%m(1,i,j,k),dstate%t(1,i,j,k),mesh%area_cell(j)
            end do
          end do
        end do
        do j = mesh%full_jds, mesh%full_jde
          do i = mesh%full_ids, mesh%full_ide
            te_pe(:) = te_pe(:) + static%gzs(:,i,j) * dstate%phs(:,i,j) * mesh%area_cell(j)
            !if (i == 1 .and. j == 2 .and. time_step == 0) write(*,*) , "te_4" ,te_pe(1),static%gzs(1,i,j),dstate%phs(1,i,j),mesh%area_cell(j)
          end do
        end do
      else if (nonhydrostatic) then
      else
        do j = mesh%full_jds, mesh%full_jde
          do i = mesh%full_ids, mesh%full_ide
            te_pe(:) = te_pe(:) + (dstate%m(:,i,j,1)**2 * g * 0.5_r8 + dstate%m(:,i,j,1) * static%gzs(:,i,j)) * mesh%area_cell(j)
            
          end do
        end do
      end if

      call dstate%async(async_pv)%wait()

      do k = mesh%full_kds, mesh%full_kde
        do j = mesh%half_jds, mesh%half_jde
          do i = mesh%half_ids, mesh%half_ide
            tav(:) = tav(:) + dstate%pv(:,i,j,k) * mesh%area_vtx(j)
          end do
        end do
      end do

      call dstate%async(async_m_lon)%wait()
      call dstate%async(async_m_lat)%wait()
      call dstate%async(async_pv_lon)%wait()
      call dstate%async(async_pv_lat)%wait()

      do k = mesh%full_kds, mesh%full_kde
        do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
          do i = mesh%half_ids, mesh%half_ide
            tpe(:) = tpe(:) + dstate%m_lon(:,i,j,k) * dstate%pv_lon(:,i,j,k)**2 * 0.5_r8 * mesh%area_lon(j)
          end do
        end do
        do j = mesh%half_jds, mesh%half_jde
          do i = mesh%full_ids, mesh%full_ide
            tpe(:) = tpe(:) + dstate%m_lat(:,i,j,k) * dstate%pv_lat(:,i,j,k)**2 * 0.5_r8 * mesh%area_lat(j)
          end do
        end do
      end do
    end do

    te(:) = te_ke(:) + te_ie(:) + te_pe(:)
    call global_sum(proc%comm, tm)
    call global_sum(proc%comm, te)
    call global_sum(proc%comm, tav)
    call global_sum(proc%comm, tpe)

    do iblk = 1, size(blocks)
      blocks(iblk)%dstate(itime)%tm  = tm
      blocks(iblk)%dstate(itime)%te  = te
      blocks(iblk)%dstate(itime)%tav = tav
      blocks(iblk)%dstate(itime)%tpe = tpe
      !call diag_state(iblk)%run(blocks(iblk)%state(itime))
    end do

    call log_add_diag('tm' , tm(member_output) )
    call log_add_diag('te' , te(member_output) )
    call log_add_diag('tpe', tpe(member_output) )

  end subroutine diagnose

  subroutine space_operators(block, old_state, star_state, new_state, tend1, tend2, dt, pass)

    type(block_type), intent(inout) :: block
    type(dstate_type), intent(inout)    :: old_state
    type(dstate_type), intent(inout) :: star_state
    type(dstate_type), intent(inout) :: new_state
    type(dtend_type ), intent(inout) :: tend1
    type(dtend_type ), intent(in   ) :: tend2
    real(8), intent(in) :: dt
    integer, intent(in) :: pass

    type(mesh_type), pointer :: mesh
    integer i, j, k

#ifdef Detail_Time
    call Add_Function("space_operators")
    call Indent_In()
#endif

    call tend1%reset_flags()

    associate (mesh => block%mesh)
    select case (pass)
    case (all_pass)
      call operators_prepare(block, star_state, dt, pass)
      if (hydrostatic) then
        call calc_grad_mf          (block, star_state, tend1, dt)
        call calc_dphsdt           (block, star_state, tend1, dt)
        call calc_we_lev           (block, star_state, tend1, dt)
        call calc_wedudlev_wedvdlev(block, star_state, tend1, dt)
        call calc_grad_ptf         (block, star_state, tend1, dt)
        call calc_coriolis         (block, star_state, tend1, dt)
        call calc_grad_ke          (block, star_state, tend1, dt)
        call pgf_run               (block, star_state, tend1)

        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
            do i = mesh%half_ids, mesh%half_ide
              tend1%du(:,i,j,k) =   tend1%qhv(:,i,j,k) - tend1%pgf_lon(:,i,j,k) - tend1%dkedlon(:,i,j,k) - tend1%wedudlev(:,i,j,k)
            end do
          end do

          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dv(:,i,j,k) = - tend1%qhu(:,i,j,k) - tend1%pgf_lat(:,i,j,k) - tend1%dkedlat(:,i,j,k) - tend1%wedvdlev(:,i,j,k)
            end do
          end do

          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dpt(:,i,j,k) = - tend1%dptfdlon(:,i,j,k) - tend1%dptfdlat(:,i,j,k) - tend1%dptfdlev(:,i,j,k)
            end do
          end do
        end do

        tend1%update_u   = .true.
        tend1%update_v   = .true.
        tend1%update_phs = .true.
        tend1%update_pt  = .true.
      else if (nonhydrostatic) then
        call calc_grad_mf          (block, star_state, tend1, dt)
        call calc_dphsdt           (block, star_state, tend1, dt)
        call calc_we_lev           (block, star_state, tend1, dt)
        call calc_grad_ptf         (block, star_state, tend1, dt)

        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dpt(:,i,j,k) = - tend1%dptfdlon(:,i,j,k) - tend1%dptfdlat(:,i,j,k) - tend1%dptfdlev(:,i,j,k)
            end do
          end do
        end do

        tend1%update_phs = .true.
        tend1%update_pt  = .true.
        call update_state(block, tend1, old_state, new_state, dt)

        !call nh_solve(block, tend1, old_state, star_state, new_state, dt)

        call calc_coriolis         (block, star_state, tend1, dt)
        call calc_grad_ke          (block, star_state, tend1, dt)
        call calc_wedudlev_wedvdlev(block, star_state, tend1, dt)
        call pgf_run               (block,  new_state, tend1)

        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
            do i = mesh%half_ids, mesh%half_ide
              tend1%du(:,i,j,k) =   tend1%qhv(:,i,j,k) - tend1%pgf_lon(:,i,j,k) - tend1%dkedlon(:,i,j,k) - tend1%wedudlev(:,i,j,k)
            end do
          end do

          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dv(:,i,j,k) = - tend1%qhu(:,i,j,k) - tend1%pgf_lat(:,i,j,k) - tend1%dkedlat(:,i,j,k) - tend1%wedvdlev(:,i,j,k)
            end do
          end do
        end do

        tend1%update_u   = .true.
        tend1%update_v   = .true.
      else
        call calc_grad_mf        (block, star_state, tend1, dt)
        call calc_coriolis       (block, star_state, tend1, dt)
        call calc_grad_ke        (block, star_state, tend1, dt)
        call pgf_run             (block, star_state, tend1)

        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
            do i = mesh%half_ids, mesh%half_ide
              tend1%du(:,i,j,k) =   tend1%qhv(:,i,j,k) - tend1%pgf_lon(:,i,j,k) - tend1%dkedlon(:,i,j,k)
            end do
          end do

          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dv(:,i,j,k) = - tend1%qhu(:,i,j,k) - tend1%pgf_lat(:,i,j,k) - tend1%dkedlat(:,i,j,k)
            end do
          end do

          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dgz(:,i,j,k) = - (tend1%dmfdlon(:,i,j,k) + tend1%dmfdlat(:,i,j,k)) * g
            end do
          end do
        end do

        tend1%update_u  = .true.
        tend1%update_v  = .true.
        tend1%update_gz = .true.
      end if
    case (forward_pass)
      call operators_prepare(block, star_state, dt, pass)
      if (hydrostatic) then
        call calc_grad_mf          (block, star_state, tend1, dt)
        call calc_dphsdt           (block, star_state, tend1, dt)
        call calc_we_lev           (block, star_state, tend1, dt)
        call calc_wedudlev_wedvdlev(block, star_state, tend1, dt)
        call calc_grad_ptf         (block, star_state, tend1, dt)
        call calc_coriolis         (block, star_state, tend1, dt)
        call calc_grad_ke          (block, star_state, tend1, dt)

        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
            do i = mesh%half_ids, mesh%half_ide
              tend1%du(:,i,j,k) =   tend1%qhv(:,i,j,k) - tend1%dkedlon(:,i,j,k) - tend1%wedudlev(:,i,j,k)
            end do
          end do

          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dv(:,i,j,k) = - tend1%qhu(:,i,j,k) - tend1%dkedlat(:,i,j,k) - tend1%wedvdlev(:,i,j,k)
            end do
          end do

          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dpt(:,i,j,k) = - tend1%dptfdlon(:,i,j,k) - tend1%dptfdlat(:,i,j,k) - tend1%dptfdlev(:,i,j,k)
            end do
          end do
        end do

        tend1%update_phs = .true.
        tend1%update_pt  = .true.
      else if (nonhydrostatic) then

      else
        call calc_grad_mf         (block, star_state, tend1, dt)
        call calc_coriolis        (block, star_state, tend1, dt)
        call calc_grad_ke         (block, star_state, tend1, dt)

        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
            do i = mesh%half_ids, mesh%half_ide
              tend1%du(:,i,j,k) =   tend1%qhv(:,i,j,k) - tend1%dkedlon(:,i,j,k)
            end do
          end do

          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dv(:,i,j,k) = - tend1%qhu(:,i,j,k) - tend1%dkedlat(:,i,j,k)
            end do
          end do

          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dgz(:,i,j,k) = - (tend1%dmfdlon(:,i,j,k) + tend1%dmfdlat(:,i,j,k)) * g
            end do
          end do
        end do

        tend1%update_gz = .true.
      end if
    case (backward_pass)
      call operators_prepare(block, new_state, dt, pass)
      if (hydrostatic) then
        call pgf_run(block, new_state, tend1)

        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
            do i = mesh%half_ids, mesh%half_ide
              tend1%du(:,i,j,k) = tend2%du(:,i,j,k) - tend1%pgf_lon(:,i,j,k)
            end do
          end do

          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dv(:,i,j,k) = tend2%dv(:,i,j,k) - tend1%pgf_lat(:,i,j,k)
            end do
          end do
        end do

        tend1%update_u   = .true.
        tend1%update_v   = .true.
      else if (nonhydrostatic) then

      else
        call pgf_run(block, new_state, tend1)

        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
            do i = mesh%half_ids, mesh%half_ide
              tend1%du(:,i,j,k) = tend2%du(:,i,j,k) - tend1%pgf_lon(:,i,j,k)
            end do
          end do

          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              tend1%dv(:,i,j,k) = tend2%dv(:,i,j,k) - tend1%pgf_lat(:,i,j,k)
            end do
          end do
        end do

        tend1%update_u  = .true.
        tend1%update_v  = .true.
      end if
    end select
    end associate

#ifdef Detail_Time
  call Indent_Out()
#endif

  end subroutine space_operators

end module gmcore_mod
