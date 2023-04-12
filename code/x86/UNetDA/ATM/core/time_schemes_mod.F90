module time_schemes_mod

  use flogger
  use const_mod
  use namelist_mod
  use block_mod
  use operators_mod
  use parallel_mod
  use parallel_types_mod
  use filter_mod
  use pa_mod
  use time_mod
  use nh_mod
  use member_mod

  implicit none

  private

  public time_scheme_init
  public time_integrator
  public update_state
  public space_operators_interface

  interface
    subroutine space_operators_interface(block, old_state, star_state, new_state, tend1, tend2, dt, pass)
      import block_type, dstate_type, dtend_type, r8
      type(block_type),  intent(inout) :: block
      type(dstate_type), intent(inout) :: old_state
      type(dstate_type), intent(inout) :: star_state
      type(dstate_type), intent(inout) :: new_state
      type(dtend_type ), intent(inout) :: tend1
      type(dtend_type ), intent(in   ) :: tend2
      real(r8), intent(in) :: dt
      integer, intent(in) :: pass
    end subroutine space_operators_interface

    subroutine step_interface(space_operators, block, old_state, star_state, new_state, tend1, tend2, dt)
        import space_operators_interface, block_type, dstate_type, dtend_type, r8
        procedure(space_operators_interface) space_operators
        type(block_type ), intent(inout) :: block
        type(dstate_type), intent(inout) :: old_state
        type(dstate_type), intent(inout) :: star_state
        type(dstate_type), intent(inout) :: new_state
        type(dtend_type ), intent(inout) :: tend1
        type(dtend_type ), intent(inout) :: tend2
        real(r8), intent(in) :: dt
    end subroutine step_interface

    subroutine time_integrator_interface(space_operators, block, old, new, dt)
      import block_type, dtend_type, dstate_type, space_operators_interface, r8
      procedure(space_operators_interface) space_operators
      type(block_type), intent(inout) :: block
      integer, intent(in) :: old
      integer, intent(in) :: new
      real(r8), intent(in) :: dt
    end subroutine time_integrator_interface
  end interface

  procedure(step_interface), pointer :: step
  procedure(time_integrator_interface), pointer :: time_integrator

contains

  subroutine time_scheme_init()

    call time_scheme_final()

    select case (time_scheme)
    case ('euler')
      time_integrator => euler
    case ('pc2')
      time_integrator => pc2
    case ('wrfrk3')
      time_integrator => wrfrk3
    case default
      time_integrator => pc2
    end select

    step => step_forward_backward

  end subroutine time_scheme_init

  subroutine time_scheme_final()
    !Pass
  end subroutine time_scheme_final

  subroutine step_all(space_operators, block, old_state, star_state, new_state, tend1, tend2, dt)

      procedure(space_operators_interface) space_operators
      type(block_type ), intent(inout) :: block
      type(dstate_type), intent(inout) :: old_state
      type(dstate_type), intent(inout) :: star_state
      type(dstate_type), intent(inout) :: new_state
      type(dtend_type ), intent(inout) :: tend1
      type(dtend_type ), intent(inout) :: tend2
      real(r8), intent(in) :: dt

#ifdef Detail_Time
    call Add_Function("step_all")
    call Indent_In()
#endif


      call space_operators(block, old_state, star_state, new_state, tend1, tend2, dt, all_pass)
      call update_state(block, tend1, old_state, new_state, dt)

#ifdef Detail_Time
    call Indent_Out()
#endif

  end subroutine step_all

  subroutine step_forward_backward(space_operators, block, old_state, star_state, new_state, tend1, tend2, dt)

      procedure(space_operators_interface) space_operators
      type(block_type ), intent(inout) :: block
      type(dstate_type), intent(inout) :: old_state
      type(dstate_type), intent(inout) :: star_state
      type(dstate_type), intent(inout) :: new_state
      type(dtend_type ), intent(inout) :: tend1
      type(dtend_type ), intent(inout) :: tend2
      real(r8), intent(in) :: dt

#ifdef Detail_Time
    call Add_Function("step_forward_backward")
    call Indent_In()
#endif

      call space_operators(block, old_state, star_state, new_state, tend1, tend2, dt, forward_pass)
      call update_state(block, tend1, old_state, new_state, dt)
      call space_operators(block, old_state, star_state, new_state, tend2, tend1, dt, backward_pass)
      call update_state(block, tend2, old_state, new_state, dt)

#ifdef Detail_Time
    call Indent_Out()
#endif
   
  end subroutine step_forward_backward

  subroutine update_state(block, dtend, old_state, new_state, dt)

    type(block_type),  intent(inout) :: block
    type(dtend_type),  intent(inout) :: dtend
    type(dstate_type), intent(inout) :: old_state
    type(dstate_type), intent(inout) :: new_state
    real(8), intent(in) :: dt

    integer i, j, k
    real(r8) wgt
    real(r8) tmp1(member_num, global_mesh%full_nlev)

    associate (mesh => block%mesh)

#ifdef Detail_Time
    call Add_Function("update_state")
    call Indent_In()
#endif

    if (baroclinic) then
      if (dtend%update_phs) then 
        !-----------------------------------------------------------------------------
#ifdef Detail_Time
    call Get_Start_Time(tran_time_start)
#endif
        call fill_halo_member(block, dtend%dphs, full_lon=.true., full_lat=.true., south_halo=.false., north_halo=.false., async=old_state%async(async_dphs))
        call old_state%async(async_dphs)%wait()

#ifdef Detail_Time
    call Get_End_Time(tran_time_end)
    tran_time = tran_time + tran_time_end - tran_time_start
#endif
      
        call filter_on_cell(block%big_filter, dtend%dphs)
        !-----------------------------------------------------------------------------

#ifdef Detail_Time
    call Get_Start_Time(cal_time_start)
#endif

        do j = mesh%full_jds, mesh%full_jde
          do i = mesh%full_ids, mesh%full_ide
            new_state%phs(:,i,j) = old_state%phs(:,i,j) + dt * dtend%dphs(:,i,j)
          end do
        end do

#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Get_Start_Time(tran_time_start)
#endif

        call fill_halo_member(block, new_state%phs, full_lon=.true., full_lat=.true., async=new_state%async(async_phs))
        call new_state%async(async_phs)%wait()

#ifdef Detail_Time
    call Get_End_Time(tran_time_end)
    tran_time = tran_time + tran_time_end - tran_time_start
#endif

        call calc_ph(block, new_state)
        call calc_m (block, new_state)
      else if (dtend%copy_phs) then
        new_state%phs    = old_state%phs
        new_state%ph_lev = old_state%ph_lev
        new_state%ph     = old_state%ph
        new_state%m      = old_state%m
      end if

      if (dtend%update_pt) then
        if (.not. dtend%update_phs .and. .not. dtend%copy_phs .and. is_root_proc()) call log_error('Mass is not updated or copied!')
        !-----------------------------------------------------------------------
#ifdef Detail_Time
    call Get_Start_Time(tran_time_start)
#endif

        call fill_halo_member(block, dtend%dpt, full_lon=.true., full_lat=.true., full_lev=.true., south_halo=.false., north_halo=.false., async=old_state%async(async_dpt))
        call old_state%async(async_dpt)%wait()

#ifdef Detail_Time
    call Get_End_Time(tran_time_end)
    tran_time = tran_time + tran_time_end - tran_time_start
#endif

        call filter_on_cell(block%big_filter, dtend%dpt)
        !--------------------------------------------------------------------------

#ifdef Detail_Time
    call Get_Start_Time(cal_time_start)
#endif

        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%full_ids, mesh%full_ide
              new_state%pt(:,i,j,k) = (old_state%pt(:,i,j,k) * old_state%m(:,i,j,k) + dt * dtend%dpt(:,i,j,k)) / new_state%m(:,i,j,k)
            end do
          end do
        end do

#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Get_Start_Time(tran_time_start)
#endif

        call fill_halo_member(block, new_state%pt, full_lon=.true., full_lat=.true., full_lev=.true., async=new_state%async(async_pt))
        call new_state%async(async_pt)%wait()

#ifdef Detail_Time
    call Get_End_Time(tran_time_end)
    tran_time = tran_time + tran_time_end - tran_time_start
#endif

      else if (dtend%copy_pt) then
        new_state%pt = old_state%pt
      end if
    else
      if (dtend%update_gz) then
        !------------------------------------------------------
#ifdef Detail_Time
    call Get_Start_Time(tran_time_start)
#endif

        call fill_halo_member(block, dtend%dgz, full_lon=.true., full_lat=.true., south_halo=.false., north_halo=.false., async=old_state%async(async_dgz))
        call old_state%async(async_dgz)%wait()

#ifdef Detail_Time
    call Get_End_Time(tran_time_end)
    tran_time = tran_time + tran_time_end - tran_time_start
#endif

        call filter_on_cell(block%big_filter, dtend%dgz)
        !-------------------------------------------------------

#ifdef Detail_Time
    call Get_Start_Time(cal_time_start)
#endif

        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%full_ids, mesh%full_ide
              new_state%gz(:,i,j,k) = old_state%gz(:,i,j,k) + dt * dtend%dgz(:,i,j,k)
            end do
          end do
        end do

#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Get_Start_Time(tran_time_start)
#endif

        call fill_halo_member(block, new_state%gz, full_lon=.true., full_lat=.true., async=new_state%async(async_gz))
        call new_state%async(async_gz)%wait()

#ifdef Detail_Time
    call Get_End_Time(tran_time_end)
    tran_time = tran_time + tran_time_end - tran_time_start
#endif

        call calc_m(block, new_state)
      else if (dtend%copy_gz) then
        new_state%gz = old_state%gz
      end if
    end if

    if (dtend%update_u .and. dtend%update_v) then
      !------------------------------------------------------

#ifdef Detail_Time
    call Get_Start_Time(tran_time_start)
#endif

      call fill_halo_member(block, dtend%du, full_lon=.false., full_lat=.true., full_lev=.true., south_halo=.false., north_halo=.false., async=old_state%async(async_du))
      call fill_halo_member(block, dtend%dv, full_lon=.true., full_lat=.false., full_lev=.true., south_halo=.false., north_halo=.false., async=old_state%async(async_dv))
      call old_state%async(async_du)%wait()
      call old_state%async(async_dv)%wait()

#ifdef Detail_Time
    call Get_End_Time(tran_time_end)
    tran_time = tran_time + tran_time_end - tran_time_start
#endif
      
      call filter_on_lon_edge(block%big_filter, dtend%du)
      call filter_on_lat_edge(block%big_filter, dtend%dv)
      !------------------------------------------------------

#ifdef Detail_Time
    call Get_Start_Time(cal_time_start)
#endif

      do k = mesh%full_kds, mesh%full_kde
        do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
          do i = mesh%half_ids, mesh%half_ide
            new_state%u_lon(:,i,j,k) = old_state%u_lon(:,i,j,k) + dt * dtend%du(:,i,j,k)
          end do
        end do
      end do
      do k = mesh%full_kds, mesh%full_kde
        do j = mesh%half_jds, mesh%half_jde
          do i = mesh%full_ids, mesh%full_ide
            new_state%v_lat(:,i,j,k) = old_state%v_lat(:,i,j,k) + dt * dtend%dv(:,i,j,k)
          end do
        end do
      end do

#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Get_Start_Time(tran_time_start)
#endif

      call fill_halo_member(block, new_state%u_lon, full_lon=.false., full_lat=.true., full_lev=.true., async=new_state%async(async_u_lon))
      call fill_halo_member(block, new_state%v_lat, full_lon=.true., full_lat=.false., full_lev=.true., async=new_state%async(async_v_lat))
      call new_state%async(async_u_lon)%wait()
      call new_state%async(async_v_lat)%wait()
    end if
   
#ifdef Detail_Time
    call Get_End_Time(tran_time_end)
    tran_time = tran_time + tran_time_end - tran_time_start
    call Indent_Out()
#endif

    end associate

  end subroutine update_state

  subroutine pc2(space_operators, block, old, new, dt)

    procedure(space_operators_interface) space_operators
    type(block_type), intent(inout) :: block
    integer, intent(in) :: old
    integer, intent(in) :: new
    real(r8), intent(in) :: dt
    
#ifdef Detail_Time
    call Add_Function("pc2")
    call Indent_In()
#endif

    associate (dstate => block%dstate, dtend => block%dtend)
      call step(space_operators, block, dstate(old), dstate(old), dstate(new), dtend(old), dtend(new), dt / 2.0_r8)
      call step(space_operators, block, dstate(old), dstate(new), dstate(3  ), dtend(old), dtend(new), dt / 2.0_r8)
      call step(space_operators, block, dstate(old), dstate(3  ), dstate(new), dtend(old), dtend(new), dt         )
    end associate

#ifdef Detail_Time
    call Indent_Out()
#endif
    
  end subroutine pc2

  subroutine wrfrk3(space_operators, block, old, new, dt)

    procedure(space_operators_interface) space_operators
    type(block_type), intent(inout) :: block
    integer, intent(in) :: old
    integer, intent(in) :: new
    real(r8), intent(in) :: dt
    
#ifdef Detail_Time
    call Add_Function("wrfrk3")
    call Indent_In()
#endif
  
    associate (dstate => block%dstate, dtend => block%dtend)
      call step(space_operators, block, dstate(old), dstate(old), dstate(new), dtend(old), dtend(new), dt / 3.0_r8)
      call step(space_operators, block, dstate(old), dstate(new), dstate(3  ), dtend(old), dtend(new), dt / 2.0_r8)
      call step(space_operators, block, dstate(old), dstate(3  ), dstate(new), dtend(old), dtend(new), dt         )
    end associate

#ifdef Detail_Time
    call Indent_Out()
#endif
    
  end subroutine wrfrk3

  
  subroutine euler(space_operators, block, old, new, dt)

    procedure(space_operators_interface) space_operators
    type(block_type), intent(inout) :: block
    integer, intent(in) :: old
    integer, intent(in) :: new
    real(r8), intent(in) :: dt
    
#ifdef Detail_Time
    call Add_Function("euler")
    call Indent_In()
#endif

    associate (dstate => block%dstate, dtend => block%dtend)
      call step(space_operators, block, dstate(old), dstate(old), dstate(new), dtend(old), dtend(new), dt)
    end associate

#ifdef Detail_Time
    call Indent_Out()
#endif
    
  end subroutine euler

end module time_schemes_mod
