module bkg_mod

  use fiona
  use flogger
  use const_mod
  use block_mod
  use vert_coord_mod
  use formula_mod
  use process_mod
  use parallel_mod
  use parallel_types_mod
  use era5_reader_mod
  use mpas_reader_mod
  use waccm_reader_mod
  use openmars_reader_mod
  use latlon_interp_mod
  use vert_interp_mod
  use interp_mod
  use member_mod

  implicit none

  private

  public bkg_read
  public bkg_final
  public bkg_regrid_phs
  public bkg_calc_ph
  public bkg_regrid_pt
  public bkg_regrid_u
  public bkg_regrid_v

  character(30) :: bkg_type = 'N/A'

contains

  subroutine bkg_read(bkg_type_, bkg_file)

    character(*), intent(in) :: bkg_type_
    character(*), intent(in) :: bkg_file
    
    bkg_type = bkg_type_

    select case (bkg_type)
    case ('era5')
      call era5_reader_run(bkg_file)
    case ('mpas')
      call mpas_reader_run(bkg_file)
    case ('waccm')
      call waccm_reader_run(bkg_file)
    case ('openmars')
      call openmars_reader_run(bkg_file)
    case default
      if (is_root_proc()) call log_error('Unknown bkg_type ' // trim(bkg_type) // '!')
    end select

  end subroutine bkg_read

  subroutine bkg_final()

    call era5_reader_final()
    call mpas_reader_final()
    call waccm_reader_final()
    call openmars_reader_final()

  end subroutine bkg_final

  subroutine bkg_regrid_phs(member_id)

    integer , intent(in) :: member_id

    real(r8), allocatable, dimension(:,:) :: p0, t0, z0, t0_p
    real(r8) lapse_kappa
    integer iblk, i, j
    logical do_hydrostatic_correct, do_drymass_correct

    if (is_root_proc()) call log_notice('Regrid mean sea level pressure and calculate surface pressure based on pressure-height formula.')

    lapse_kappa = lapse_rate * Rd_o_g
    do iblk = 1, size(blocks)
      associate (block => blocks(iblk) ,&
                 state => blocks(iblk)%dstate(1),&
                 mesh => blocks(iblk)%mesh, &
                 gzs => blocks(iblk)%static%gzs, &
                 phs => blocks(iblk)%dstate(1)%phs)
        allocate(p0  (mesh%full_ims:mesh%full_ime,mesh%full_jms:mesh%full_jme))
        allocate(t0  (mesh%full_ims:mesh%full_ime,mesh%full_jms:mesh%full_jme))
        allocate(z0  (mesh%full_ims:mesh%full_ime,mesh%full_jms:mesh%full_jme))
        allocate(t0_p(mesh%full_ims:mesh%full_ime,mesh%full_jms:mesh%full_jme))

        select case (bkg_type)
        case ('era5')
          call latlon_interp_bilinear_cell(era5_lon, era5_lat, era5_mslp, mesh, p0)
          call latlon_interp_bilinear_cell(era5_lon, era5_lat, era5_t(:,:,num_era5_lev), mesh, t0)
          t0_p = era5_lev(num_era5_lev)
          z0 = 0.0_r8
          do_hydrostatic_correct = .true.
          do_drymass_correct = .false.
        case ('mpas')
          ! call latlon_interp_bilinear_cell(mpas_lon, mpas_lat, mpas_ps, mesh, p0)
          ! call latlon_interp_bilinear_cell(mpas_lon, mpas_lat, mpas_zs, mesh, z0)
          ! call latlon_interp_bilinear_cell(mpas_lon, mpas_lat, mpas_p(:,:,num_mpas_lev), mesh, t0_p)
          ! call latlon_interp_bilinear_cell(mpas_lon, mpas_lat, mpas_t(:,:,num_mpas_lev), mesh, t0  )
          call latlon_interp_bilinear_cell(mpas_lon, mpas_lat, mpas_ps, mesh, phs(member_id,:,:))
          do_hydrostatic_correct = .false.
        case ('waccm')
          call latlon_interp_bilinear_cell(waccm_lon, waccm_lat, waccm_ps, mesh, p0)
          call latlon_interp_bilinear_cell(waccm_lon, waccm_lat, waccm_zs, mesh, z0)
          call latlon_interp_bilinear_cell(waccm_lon, waccm_lat, waccm_p(:,:,waccm_nlev), mesh, t0_p)
          call latlon_interp_bilinear_cell(waccm_lon, waccm_lat, waccm_t(:,:,waccm_nlev), mesh, t0  )
          do_hydrostatic_correct = .true.
        case ('openmars')
          call latlon_interp_bilinear_cell(openmars_lon, openmars_lat, openmars_ps, mesh, phs(member_id,:,:))
          do_hydrostatic_correct = .false.
        end select
        ! According to pressure-height formula based on hydrostatic assumption.
        if (do_hydrostatic_correct) then
          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%full_ids, mesh%full_ide
              t0(i,j) = t0(i,j) * (p0(i,j) / t0_p(i,j))**lapse_kappa
              phs(member_id,i,j) = p0(i,j) * (1.0_r8 - lapse_rate * (gzs(member_id,i,j) / g - z0(i,j)) / t0(i,j))**(1.0_r8 / lapse_kappa)
            end do
          end do
        end if
        ! Evaluate dry mass surface pressure
        if (do_drymass_correct) then
          call calc_dry_air_ps(era5_lon, era5_lat, era5_lev, era5_ps, era5_q, era5_psd)
          call latlon_interp_bilinear_cell(era5_lon, era5_lat, era5_psd, mesh, phs(member_id,:,:))
        end if

        call fill_halo_member(blocks(iblk),phs,full_lon=.true., full_lat=.true., async=state%async(async_phs))
        call state%async(async_phs)%wait()

        deallocate(p0, t0, z0, t0_p)
      end associate
    end do

  end subroutine bkg_regrid_phs

  subroutine calc_dry_air_ps(lon, lat, lev, ps, q, psd)

    real(r8), intent(in) :: lon(num_era5_lon), &
                            lat(num_era5_lat), &
                            lev(num_era5_lev)
    real(r8), intent(in) :: ps(num_era5_lon,num_era5_lat), &
                             q(num_era5_lon,num_era5_lat,num_era5_lev)
    real(r8), intent(out) :: psd(num_era5_lon,num_era5_lat)

    integer i, j, k, nlev_eff
    real(r8) pfull(num_era5_lev  ), hpfull(num_era5_lev  ), dpfull(num_era5_lev)
    real(r8) pface(num_era5_lev+1), hpface(num_era5_lev+1)

    do j = 1, num_era5_lat
      do i = 1, num_era5_lon
        pfull = lev(:)
        ! Check effective full level number.
        nlev_eff = 99999
        do k = 1, num_era5_lev
          if (pfull(k) >= ps(i,j)) then
            nlev_eff = k
            exit
          end if 
        end do
        if (nlev_eff == 99999) nlev_eff = num_era5_lev 

        pface(2:nlev_eff) = (pfull(1:nlev_eff-1) + pfull(2:nlev_eff)) * 0.5_r8
        pface(1) = 0.0_r8
        pface(nlev_eff+1) = ps(i,j)
 
        dpfull(1:nlev_eff) = pface(2:nlev_eff+1) - pface(1:nlev_eff)
        hpface(1) = pface(1)

        do k = 2, nlev_eff+1
          hpface(k) = hpface(k-1) + dpfull(k-1) * (1.0_r8 - q(i,j,k-1))
        end do 
        hpfull(1:nlev_eff) = (hpface(1:nlev_eff) + hpface(2:nlev_eff+1)) * 0.5_r8
        psd(i,j) = hpface(nlev_eff+1)
      end do
    end do

  end subroutine calc_dry_air_ps

  subroutine bkg_calc_ph(member_id)

    integer , intent(in) :: member_id

    integer iblk, i, j, k

    if (is_root_proc()) call log_notice('Calculate pressure on each grid.')

    do iblk = 1, size(blocks)
      associate (mesh => blocks(iblk)%mesh        , &
                 phs  => blocks(iblk)%dstate(1)%phs, &
                 ph   => blocks(iblk)%dstate(1)%ph , &
                 state => blocks(iblk)%dstate(1) )
        ! Calculate pressure on GMCORE grids.
        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%full_ids, mesh%full_ide           
              ph(member_id,i,j,k) = vert_coord_calc_ph(k, phs(member_id,i,j))
            end do
          end do
        end do

        call fill_halo_member(blocks(iblk),ph,full_lon=.true., full_lat=.true., full_lev=.true., async=state%async(async_ph))
        call state%async(async_ph)%wait()

      end associate
    end do

  end subroutine bkg_calc_ph

  subroutine bkg_regrid_pt(member_id)

    integer , intent(in) :: member_id

    real(r8), allocatable, dimension(:,:,:) :: t1, pt1, p1
    integer iblk, i, j, k

    if (is_root_proc()) call log_notice('Regrid temperature and calculate potential temperature.')

    do iblk = 1, size(blocks)
      associate (block => blocks(iblk)             , &
                 state => blocks(iblk)%dstate(1) ,&
                 mesh  => blocks(iblk)%mesh        , &
                 phs   => blocks(iblk)%dstate(1)%phs, &
                 ph    => blocks(iblk)%dstate(1)%ph , &
                !  old   => blocks(iblk)%adv_batches(1)%old, & ! in
                !  q     => blocks(iblk)%adv_batches(1)%q  , & ! in
                 t     => blocks(iblk)%dstate(1)%t  , &
                 pt    => blocks(iblk)%dstate(1)%pt)
        
        select case (bkg_type)
        case ('era5')
          ! allocate(t1(mesh%full_ims:mesh%full_ime,mesh%full_jms:mesh%full_jme,num_era5_lev))
          ! do k = 1, num_era5_lev
          !   call latlon_interp_bilinear_cell(era5_lon, era5_lat, era5_t(:,:,k), mesh, t1(:,:,k))
          ! end do
          ! do j = mesh%full_jds, mesh%full_jde
          !   do i = mesh%full_ids, mesh%full_ide
          !     call vert_interp_log_linear(era5_lev, t1(i,j,:), ph(member_id,i,j,:), t(member_id,i,j,:), allow_extrap=.true.)
          !     pt(member_id,i,j,:) = potential_temperature(t(member_id,i,j,:), ph(member_id,i,j,:), q(member_id,i,j,:,1,old))
          !   end do
          ! end do
          ! deallocate(t1)
        case ('mpas')
          allocate(pt1(mesh%full_ims:mesh%full_ime,mesh%full_jms:mesh%full_jme,num_mpas_lev))
          allocate(p1 (mesh%full_ims:mesh%full_ime,mesh%full_jms:mesh%full_jme,num_mpas_lev))
          do k = 1, num_mpas_lev
            call latlon_interp_bilinear_cell(mpas_lon, mpas_lat, mpas_pt(:,:,k), mesh, pt1(:,:,k))
            call latlon_interp_bilinear_cell(mpas_lon, mpas_lat, mpas_p (:,:,k), mesh, p1 (:,:,k))
          end do
          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%full_ids, mesh%full_ide
              call vert_interp_log_linear(p1(i,j,:), pt1(i,j,:), ph(member_id,i,j,1:mesh%full_nlev), pt(member_id,i,j,1:mesh%full_nlev), allow_extrap=.false.)
            end do
          end do
          deallocate(pt1, p1)
        case ('waccm')
          ! allocate(t1(mesh%full_ims:mesh%full_ime,mesh%full_jms:mesh%full_jme,waccm_nlev))
          ! allocate(p1(mesh%full_ims:mesh%full_ime,mesh%full_jms:mesh%full_jme,waccm_nlev))
          ! do k = 1, waccm_nlev
          !   call latlon_interp_bilinear_cell(waccm_lon, waccm_lat, waccm_t(:,:,k), mesh, t1(:,:,k))
          !   call latlon_interp_bilinear_cell(waccm_lon, waccm_lat, waccm_p(:,:,k), mesh, p1(:,:,k))
          ! end do
          ! do j = mesh%full_jds, mesh%full_jde
          !   do i = mesh%full_ids, mesh%full_ide
          !     call vert_interp_log_linear(p1(i,j,:), t1(i,j,:), ph(member_id,i,j,:), t(member_id,i,j,:), allow_extrap=.true.)
          !     pt(member_id,i,j,:) = potential_temperature(t(member_id,i,j,:), ph(member_id,i,j,:), q(member_id,i,j,:,1,old))
          !   end do
          ! end do
          ! deallocate(t1, p1)
        case ('openmars')
          ! allocate(t1(mesh%full_ims:mesh%full_ime,mesh%full_jms:mesh%full_jme,nlev_openmars))
          ! allocate(p1(mesh%full_ims:mesh%full_ime,mesh%full_jms:mesh%full_jme,nlev_openmars))
          ! do k = 1, nlev_openmars
          !   call latlon_interp_bilinear_cell(openmars_lon, openmars_lat, openmars_t(:,:,k), mesh, t1(:,:,k))
          !   call latlon_interp_bilinear_cell(openmars_lon, openmars_lat, openmars_p(:,:,k), mesh, p1(:,:,k))
          ! end do
          ! do j = mesh%full_jds, mesh%full_jde
          !   do i = mesh%full_ids, mesh%full_ide
          !     call vert_interp_log_linear(p1(i,j,:), t1(i,j,:), ph(member_id,i,j,:), t(member_id,i,j,:), allow_extrap=.true.)
          !     pt(member_id,i,j,:) = potential_temperature(t(member_id,i,j,:), ph(member_id,i,j,:), q(member_id,i,j,:,1,old))
          !   end do
          ! end do
          ! deallocate(t1, p1)
        end select
        call fill_halo_member(block, pt, full_lon=.true., full_lat=.true., full_lev=.true., async=state%async(async_pt))
        call state%async(async_pt)%wait()
      end associate
    end do

  end subroutine bkg_regrid_pt

  subroutine bkg_regrid_u(member_id)

    integer , intent(in) :: member_id

    real(r8), allocatable, dimension(:,:,:) :: u1, p1
    integer iblk, i, j, k

    if (is_root_proc()) call log_notice('Regrid u wind component.')

    do iblk = 1, size(blocks)
      associate (block => blocks(iblk)             , &
                 mesh  => blocks(iblk)%mesh        , &
                 state => blocks(iblk)%dstate(1) ,&
                 ph    => blocks(iblk)%dstate(1)%ph , &
                 u     => blocks(iblk)%dstate(1)%u_lon )
        select case (bkg_type)
        case ('era5')
          allocate(u1(mesh%half_ims:mesh%half_ime,mesh%full_jms:mesh%full_jme,num_era5_lev))
          do k = 1, num_era5_lev
            call latlon_interp_bilinear_lon_edge(era5_lon, era5_lat, era5_u(:,:,k), mesh, u1(:,:,k), zero_pole=.true.)
          end do
          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%half_ids, mesh%half_ide
              call vert_interp_linear(era5_lev, u1(i,j,:), ph(member_id,i,j,:), u(member_id,i,j,:), allow_extrap=.true.)
            end do
          end do
          deallocate(u1)
        case ('mpas')
          allocate(u1(mesh%half_ims:mesh%half_ime,mesh%full_jms:mesh%full_jme,num_mpas_lev))
          allocate(p1(mesh%half_ims:mesh%half_ime,mesh%full_jms:mesh%full_jme,num_mpas_lev))
          do k = 1, num_mpas_lev
            call latlon_interp_bilinear_lon_edge(mpas_lon, mpas_lat, mpas_u(:,:,k), mesh, u1(:,:,k), zero_pole=.true.)
            call latlon_interp_bilinear_lon_edge(mpas_lon, mpas_lat, mpas_p(:,:,k), mesh, p1(:,:,k))
          end do
          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%half_ids, mesh%half_ide
              call vert_interp_linear(p1(i,j,:), u1(i,j,:), ph(member_id,i,j,1:mesh%full_nlev), u(member_id,i,j,1:mesh%full_nlev), allow_extrap=.false.)
            end do
          end do
          deallocate(u1, p1)
        case ('waccm')
          allocate(u1(mesh%half_ims:mesh%half_ime,mesh%full_jms:mesh%full_jme,waccm_nlev))
          allocate(p1(mesh%half_ims:mesh%half_ime,mesh%full_jms:mesh%full_jme,waccm_nlev))
          do k = 1, waccm_nlev
            call latlon_interp_bilinear_cell(waccm_lon, waccm_lat, waccm_u(:,:,k), mesh, u1(:,:,k), zero_pole=.true.)
            call latlon_interp_bilinear_cell(waccm_lon, waccm_lat, waccm_p(:,:,k), mesh, p1(:,:,k))
          end do
          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%half_ids, mesh%half_ide
                call vert_interp_linear(p1(i,j,:), u1(i,j,:), ph(member_id,i,j,:), u(member_id,i,j,:), allow_extrap=.true.)
            end do
          end do
          deallocate(u1, p1)
        case ('openmars')
          allocate(u1(mesh%half_ims:mesh%half_ime,mesh%full_jms:mesh%full_jme,nlev_openmars))
          allocate(p1(mesh%half_ims:mesh%half_ime,mesh%full_jms:mesh%full_jme,nlev_openmars))
          do k = 1, nlev_openmars
            call latlon_interp_bilinear_lon_edge(openmars_lon, openmars_lat, openmars_u(:,:,k), mesh, u1(:,:,k), zero_pole=.true.)
            call latlon_interp_bilinear_lon_edge(openmars_lon, openmars_lat, openmars_p(:,:,k), mesh, p1(:,:,k))
          end do
          do j = mesh%full_jds, mesh%full_jde
            do i = mesh%half_ids, mesh%half_ide
              call vert_interp_linear(p1(i,j,:), u1(i,j,:), ph(member_id,i,j,:), u(member_id,i,j,:), allow_extrap=.true.)
            end do
          end do
          deallocate(u1, p1)
        end select
        call fill_halo_member(block, u, full_lon=.false., full_lat=.true., full_lev=.true., async=state%async(async_u_lon))
        call state%async(async_u_lon)%wait()
      end associate
    end do

  end subroutine bkg_regrid_u

  subroutine bkg_regrid_v(member_id)

    integer , intent(in) :: member_id

    real(r8), allocatable, dimension(:,:,:) :: v1, p1
    integer iblk, i, j, k

    if (is_root_proc()) call log_notice('Regrid v wind component.')

    do iblk = 1, size(blocks)
      associate (block => blocks(iblk)             , &
                 mesh  => blocks(iblk)%mesh        , &
                 state => blocks(iblk)%dstate(1) ,&
                 ph    => blocks(iblk)%dstate(1)%ph , &
                 v     => blocks(iblk)%dstate(1)%v_lat )
        select case (bkg_type)
        case ('era5')
          allocate(v1(mesh%full_ims:mesh%full_ime,mesh%half_jms:mesh%half_jme,num_era5_lev))
          do k = 1, num_era5_lev
            call latlon_interp_bilinear_lat_edge(era5_lon, era5_lat, era5_v(:,:,k), mesh, v1(:,:,k))
          end do
          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              call vert_interp_linear(era5_lev, v1(i,j,:), ph(member_id,i,j,:), v(member_id,i,j,:), allow_extrap=.true.)
            end do
          end do
          deallocate(v1)
        case ('mpas')
          allocate(v1(mesh%full_ims:mesh%full_ime,mesh%half_jms:mesh%half_jme,num_mpas_lev))
          allocate(p1(mesh%full_ims:mesh%full_ime,mesh%half_jms:mesh%half_jme,num_mpas_lev))
          do k = 1, num_mpas_lev
            call latlon_interp_bilinear_lat_edge(mpas_lon, mpas_lat, mpas_v(:,:,k), mesh, v1(:,:,k))
            call latlon_interp_bilinear_lat_edge(mpas_lon, mpas_lat, mpas_p(:,:,k), mesh, p1(:,:,k))
          end do
          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              call vert_interp_linear(p1(i,j,:), v1(i,j,:), ph(member_id,i,j,1:mesh%full_nlev), v(member_id,i,j,1:mesh%full_nlev), allow_extrap=.false.)
            end do
          end do
          deallocate(v1, p1)
        case ('waccm')
          allocate(v1(mesh%full_ims:mesh%full_ime,mesh%half_jms:mesh%half_jme,waccm_nlev))
          allocate(p1(mesh%full_ims:mesh%full_ime,mesh%half_jms:mesh%half_jme,waccm_nlev))
          do k = 1, waccm_nlev
            call latlon_interp_bilinear_lat_edge(waccm_lon, waccm_lat, waccm_v(:,:,k), mesh, v1(:,:,k))
            call latlon_interp_bilinear_lat_edge(waccm_lon, waccm_lat, waccm_p(:,:,k), mesh, p1(:,:,k))
          end do
          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              call vert_interp_linear(p1(i,j,:), v1(i,j,:), ph(member_id,i,j,:), v(member_id,i,j,:), allow_extrap=.true.)
            end do
          end do
          deallocate(v1, p1)
        case ('openmars')
          allocate(v1(mesh%full_ims:mesh%full_ime,mesh%half_jms:mesh%half_jme,nlev_openmars))
          allocate(p1(mesh%full_ims:mesh%full_ime,mesh%half_jms:mesh%half_jme,nlev_openmars))
          do k = 1, nlev_openmars
            call latlon_interp_bilinear_lat_edge(openmars_lon, openmars_lat, openmars_v(:,:,k), mesh, v1(:,:,k))
            call latlon_interp_bilinear_lat_edge(openmars_lon, openmars_lat, openmars_p(:,:,k), mesh, p1(:,:,k))
          end do
          do j = mesh%half_jds, mesh%half_jde
            do i = mesh%full_ids, mesh%full_ide
              call vert_interp_linear(p1(i,j,:), v1(i,j,:), ph(member_id,i,j,:), v(member_id,i,j,:), allow_extrap=.true.)
            end do
          end do
          deallocate(v1, p1)
        end select
        call fill_halo_member(block, v, full_lon=.true., full_lat=.false., full_lev=.true., async=state%async(async_v_lat))
        call state%async(async_v_lat)%wait()
      end associate
    end do

  end subroutine bkg_regrid_v

end module bkg_mod
