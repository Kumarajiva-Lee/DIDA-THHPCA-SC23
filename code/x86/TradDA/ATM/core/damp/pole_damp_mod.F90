module pole_damp_mod

    use namelist_mod
    use block_mod
    use filter_mod
    use operators_mod
    use parallel_mod
    use parallel_types_mod
    use pa_mod
  
    implicit none
  
    private
  
    public pole_damp_run
  
  contains
  
    subroutine pole_damp_run(block, dstate)
  
      type(block_type), intent(in) :: block
      type(dstate_type), intent(inout) :: dstate

#ifdef Detail_Time
    call Add_Function("pole_damp_run")
    call Indent_In()
#endif
  
      if (baroclinic) then

#ifdef Detail_Time
    call Get_Start_Time(tran_time_start)
#endif

        call dstate%async(async_pt)%wait()

#ifdef Detail_Time
      call Get_End_Time(tran_time_end)
      tran_time = tran_time + tran_time_end - tran_time_start
      call Get_Start_Time(cal_time_start)
#endif

        dstate%pt = dstate%pt * dstate%m

#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Get_Start_Time(tran_time_start)
#endif

        call dstate%async(async_phs)%wait()

#ifdef Detail_Time
      call Get_End_Time(tran_time_end)
      tran_time = tran_time + tran_time_end - tran_time_start
#endif

        call filter_on_cell(block%small_filter_phs, dstate%phs)

#ifdef Detail_Time
    call Get_Start_Time(tran_time_start)
#endif

        call fill_halo_member(block, dstate%phs, full_lon=.true., full_lat=.true.,async=dstate%async(async_phs))
        call dstate%async(async_phs)%wait()

#ifdef Detail_Time
      call Get_End_Time(tran_time_end)
      tran_time = tran_time + tran_time_end - tran_time_start
#endif

        call calc_ph(block, dstate)
        call calc_m (block, dstate)
        
        call filter_on_cell(block%small_filter_pt, dstate%pt)

#ifdef Detail_Time
    call Get_Start_Time(cal_time_start)
#endif

        dstate%pt = dstate%pt / dstate%m

#ifdef Detail_Time
    call Get_End_Time(cal_time_end)
    cal_time = cal_time + cal_time_end - cal_time_start
    call Get_Start_Time(tran_time_start)
#endif

        call fill_halo_member(block,dstate%pt,full_lon=.true., full_lat=.true.,full_lev=.true.,async=dstate%async(async_pt))
        call dstate%async(async_pt)%wait()
        call dstate%async(async_u_lon)%wait()

#ifdef Detail_Time
      call Get_End_Time(tran_time_end)
      tran_time = tran_time + tran_time_end - tran_time_start
#endif

        call filter_on_lon_edge(block%small_filter_uv, dstate%u_lon)

#ifdef Detail_Time
    call Get_Start_Time(tran_time_start)
#endif

        call fill_halo_member(block, dstate%u_lon, full_lon=.false., full_lat=.true.,full_lev=.true., async=dstate%async(async_u_lon))
        call dstate%async(async_u_lon)%wait()
        call dstate%async(async_v_lat)%wait()

#ifdef Detail_Time
      call Get_End_Time(tran_time_end)
      tran_time = tran_time + tran_time_end - tran_time_start
#endif

        call filter_on_lat_edge(block%small_filter_uv, dstate%v_lat)

#ifdef Detail_Time
    call Get_Start_Time(tran_time_start)
#endif

        call fill_halo_member(block, dstate%v_lat, full_lon=.true., full_lat=.false.,full_lev=.true., async=dstate%async(async_v_lat))
        call dstate%async(async_v_lat)%wait()

#ifdef Detail_Time
      call Get_End_Time(tran_time_end)
      tran_time = tran_time + tran_time_end - tran_time_start
#endif

      else
        ! call filter_on_cell(block%small_filter_phs, dstate%gz)
        ! call fill_halo(block%halo, dstate%gz, full_lon=.true., full_lat=.true.)
        ! call calc_m (block, dstate)
      end if

#ifdef Detail_Time
    call Indent_Out()
#endif
  
    end subroutine pole_damp_run

  end module pole_damp_mod
  