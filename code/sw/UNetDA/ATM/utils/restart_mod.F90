module restart_mod

  use fiona
  use string
  use flogger
  use datetime
  use const_mod
  use namelist_mod
  use time_mod
  use block_mod
  use parallel_mod
  use parallel_types_mod
  use dynamics_types_mod
  use member_mod

  implicit none

  private

  public restart_init
  public restart_write
  public restart_read

contains

  subroutine restart_init()

    character(10) time_value, time_units
    real(8) seconds

    if (restart_interval == 'N/A') then
      if (is_root_proc()) call log_warning('Parameter restart_interval is not set, so no restart file outputted.')
      return
    end if
    if (case_name == 'N/A') call log_error('Parameter case_name is not set!')

    time_value = split_string(restart_interval, ' ', 1)
    time_units = split_string(restart_interval, ' ', 2)
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
      call log_error('Invalid restart interval ' // trim(restart_interval) // '!')
    end select

    call time_add_alert('restart_write', seconds=seconds , first_alert=.true.)

  end subroutine restart_init

  subroutine restart_write(itime , group_id_in)

    integer, intent(in)           :: itime
    integer, intent(in), optional :: group_id_in

    integer iblk, is, ie, js, je, ks, ke
    integer start(3), count(3)
    character(4) lon_dims_3d(4), lat_dims_3d(4), cell_dims_3d(4)
    character(4) lon_dims_2d(3), lat_dims_2d(3), cell_dims_2d(3)

    integer im
    character*20 fir0  

    integer group_id

    if (present(group_id_in)) then
      group_id = group_id_in
    else
      group_id = 0
    endif

     lon_dims_3d(1) = 'ilon';  lon_dims_3d(2) =  'lat';  lon_dims_3d(3) =  'lev';  lon_dims_3d(4) = 'time'
     lat_dims_3d(1) =  'lon';  lat_dims_3d(2) = 'ilat';  lat_dims_3d(3) =  'lev';  lat_dims_3d(4) = 'time'
    cell_dims_3d(1) =  'lon'; cell_dims_3d(2) =  'lat'; cell_dims_3d(3) =  'lev'; cell_dims_3d(4) = 'time'
     lon_dims_2d(1) = 'ilon';  lon_dims_2d(2) =  'lat';  lon_dims_2d(3) = 'time'
     lat_dims_2d(1) =  'lon';  lat_dims_2d(2) = 'ilat';  lat_dims_2d(3) = 'time'
    cell_dims_2d(1) =  'lon'; cell_dims_2d(2) =  'lat'; cell_dims_2d(3) = 'time'

    do im = 1 , member_num

      if ( (ivector - 1) * member_num + im > member_total ) exit

      write(fir0, '("r0_", i6.6)') (ivector - 1) * member_num + im + group_id * member_total

        call fiona_create_dataset(fir0, desc=case_desc, file_prefix=trim(case_name) // '.' // trim(curr_time_str), mpi_comm=proc%comm)
        call fiona_add_att(fir0, 'time_step_size', dt_in_seconds)
        call fiona_add_att(fir0, 'restart_interval', restart_interval)
        call fiona_add_dim(fir0, 'time', add_var=.true.)
        call fiona_add_dim(fir0, 'lon' , size=global_mesh%full_nlon, add_var=.true., decomp=.true.)
        call fiona_add_dim(fir0, 'lat' , size=global_mesh%full_nlat, add_var=.true., decomp=.true.)
        call fiona_add_dim(fir0, 'ilon', size=global_mesh%half_nlon, add_var=.true., decomp=.true.)
        call fiona_add_dim(fir0, 'ilat', size=global_mesh%half_nlat, add_var=.true., decomp=.true.)
      if (baroclinic) then
        call fiona_add_dim(fir0, 'lev' , size=global_mesh%full_nlev, add_var=.true.)
        call fiona_add_dim(fir0, 'ilev', size=global_mesh%half_nlev, add_var=.true.)
        call fiona_add_var(fir0, 'u'   , long_name='u wind component'            , units='m s-1' , dim_names=lon_dims_3d , data_type='r8')
        call fiona_add_var(fir0, 'v'   , long_name='v wind component'            , units='m s-1' , dim_names=lat_dims_3d , data_type='r8')
        call fiona_add_var(fir0, 'phs' , long_name='hydrostatic surface pressure', units='Pa'    , dim_names=cell_dims_2d, data_type='r8')
        call fiona_add_var(fir0, 'pt'  , long_name='potential temperature'       , units='K'     , dim_names=cell_dims_3d, data_type='r8')
      else
        call fiona_add_var(fir0, 'u'   , long_name='u wind component'            , units='m s-1' , dim_names=lon_dims_2d , data_type='r8')
        call fiona_add_var(fir0, 'v'   , long_name='v wind component'            , units='m s-1' , dim_names=lat_dims_2d , data_type='r8')
        call fiona_add_var(fir0, 'gz'  , long_name='geopotential height'         , units='m2 s-2', dim_names=cell_dims_2d, data_type='r8')
      end if
        call fiona_add_var(fir0, 'gzs' , long_name='surface geopotential height' , units='m2 s-2', dim_names=cell_dims_2d, data_type='r8')

        call fiona_start_output(fir0, elapsed_seconds, new_file=.true.)
        call fiona_output(fir0, 'lon' , global_mesh%full_lon_deg(1:global_mesh%full_nlon))
        call fiona_output(fir0, 'lat' , global_mesh%full_lat_deg(1:global_mesh%full_nlat))
        call fiona_output(fir0, 'ilon', global_mesh%half_lon_deg(1:global_mesh%half_nlon))
        call fiona_output(fir0, 'ilat', global_mesh%half_lat_deg(1:global_mesh%half_nlat))
        do iblk = 1, size(blocks)
          associate (mesh   => blocks(iblk)%mesh        , &
                    state  => blocks(iblk)%dstate(itime), &
                    static => blocks(iblk)%static)

            is = mesh%full_ids; ie = mesh%full_ide
            js = mesh%full_jds; je = mesh%full_jde
            ks = mesh%full_kds; ke = mesh%full_kde
            start = [is,js,ks]
            count = [mesh%full_nlon,mesh%full_nlat,mesh%full_nlev]

            call fiona_output(fir0, 'gzs', static%gzs(im,is:ie,js:je), start=start, count=count)
            if (baroclinic) then
              call fiona_output(fir0, 'phs', state%phs(im,is:ie,js:je      ), start=start, count=count)
              call fiona_output(fir0, 'pt' , state%pt (im,is:ie,js:je,ks:ke), start=start, count=count)
            else
              call fiona_output(fir0, 'gz' , state%gz (im,is:ie,js:je,ks:ke), start=start, count=count)
            end if

            is = mesh%half_ids; ie = mesh%half_ide
            js = mesh%full_jds; je = mesh%full_jde
            ks = mesh%full_kds; ke = mesh%full_kde
            start = [is,js,ks]
            count = [mesh%half_nlon,mesh%full_nlat,mesh%full_nlev]

            call fiona_output(fir0, 'u'  , state %u_lon(im,is:ie,js:je,ks:ke), start=start, count=count)

            is = mesh%full_ids; ie = mesh%full_ide
            js = mesh%half_jds; je = mesh%half_jde
            ks = mesh%full_kds; ke = mesh%full_kde
            start = [is,js,ks]
            count = [mesh%full_nlon,mesh%half_nlat,mesh%full_nlev]

            call fiona_output(fir0, 'v'  , state %v_lat(im,is:ie,js:je,ks:ke), start=start, count=count)
          end associate
        end do
        call fiona_end_output(fir0)
    end do

  end subroutine restart_write

  subroutine restart_read(group_id_in)

    integer, intent(in), optional :: group_id_in

    type(block_type), pointer :: block
    type(mesh_type), pointer :: mesh
    type(dstate_type), pointer :: state
    type(static_type), pointer :: static
    type(datetime_type) time
    integer iblk, time_step, is, ie, js, je, ks, ke
    integer start(3), count(3)
    real(8) time_value
    character(30) time_units

    integer im
    character*20 fir0 

    integer group_id

    if (present(group_id_in)) then
      group_id = group_id_in
    else
      group_id = 0
    endif

    if (restart_file == 'N/A') then
      call log_error('Parameter restart_file is needed to restart!')
    end if

    do im = 1 , member_num

      if ( (ivector - 1) * member_num + im > member_total ) exit

      write(fir0, '("r0_", i6.6)') (ivector - 1) * member_num + im + group_id * member_total

      call fiona_open_dataset(fir0, file_path=restart_file, mpi_comm=proc%comm)
      call fiona_start_input(fir0)

      time_step = 1

      call fiona_input(fir0, 'time', time_value, time_step=time_step)
      call fiona_get_att(fir0, 'time', 'units', time_units)
      do iblk = 1, size(blocks)
        associate (block  => blocks(iblk)                    , &
                  mesh   => blocks(iblk)%mesh               , &
                  state  => blocks(iblk)%dstate(old_time_idx), &
                  static => blocks(iblk)%static)
          is = mesh%full_ids; ie = mesh%full_ide
          js = mesh%full_jds; je = mesh%full_jde
          ks = mesh%full_kds; ke = mesh%full_kde
          start = [is,js,ks]
          count = [mesh%full_nlon,mesh%full_nlat,mesh%full_nlev]

          call fiona_input(fir0, 'gzs', static%gzs(im,is:ie,js:je), start=start, count=count, time_step=time_step)
          call fill_halo_member(block, static%gzs, full_lon=.true., full_lat=.true., async=state%async(async_gzs))
          if (baroclinic) then
            call fiona_input(fir0, 'phs', state%phs(im,is:ie,js:je      ), start=start, count=count, time_step=time_step)
            call fill_halo_member(block, state%phs, full_lon=.true., full_lat=.true., async=state%async(async_phs))
            call fiona_input(fir0, 'pt' , state%pt (im,is:ie,js:je,ks:ke), start=start, count=count, time_step=time_step)
            call fill_halo_member(block, state%pt, full_lon=.true., full_lat=.true., full_lev=.true., async=state%async(async_pt))
          else
            call fiona_input(fir0, 'gz' , state%gz (im,is:ie,js:je,ks:ke), start=start, count=count, time_step=time_step)
            call fill_halo_member(block, state%gz, full_lon=.true., full_lat=.true., full_lev=.true., async=state%async(async_gz))
          end if

          is = mesh%half_ids; ie = mesh%half_ide
          js = mesh%full_jds; je = mesh%full_jde
          ks = mesh%full_kds; ke = mesh%full_kde
          start = [is,js,ks]
          count = [mesh%half_nlon,mesh%full_nlat,mesh%full_nlev]

          call fiona_input(fir0, 'u'  , state%u_lon(im,is:ie,js:je,ks:ke), start=start, count=count, time_step=time_step)
          call fill_halo_member(block, state%u_lon, full_lon=.false., full_lat=.true., full_lev=.true., async=state%async(async_u_lon))

          is = mesh%full_ids; ie = mesh%full_ide
          js = mesh%half_jds; je = mesh%half_jde
          ks = mesh%full_kds; ke = mesh%full_kde
          start = [is,js,ks]
          count = [mesh%full_nlon,mesh%half_nlat,mesh%full_nlev]

          call fiona_input(fir0, 'v'  , state%v_lat(im,is:ie,js:je,ks:ke), start=start, count=count, time_step=time_step)
          call fill_halo_member(block, state%v_lat, full_lon=.true., full_lat=.false., full_lev=.true., async=state%async(async_v_lat))

          call state%async(async_gzs)%wait()
          call state%async(async_phs)%wait()
          call state%async(async_pt)%wait()
          call state%async(async_gz)%wait()
          call state%async(async_u_lon)%wait()
          call state%async(async_v_lat)%wait()
        
        end associate
      end do
      call fiona_end_input(fir0)
    end do

    call time_fast_forward(time_value, time_units)
    if (is_root_proc()) call log_notice('Restart to ' // trim(curr_time_str))

  end subroutine restart_read

end module restart_mod
