module baroclinic_wave_test_mod

  use const_mod
  use namelist_mod
  use formula_mod
  use parallel_mod
  use parallel_types_mod
  use block_mod
  use vert_coord_mod
  use member_mod

  implicit none

  private

  public baroclinic_wave_test_set_ic

  real(r8), parameter :: alpha = 0.0_r8
  real(r8), parameter :: u0    = 35.0_r8   ! m s-1
  real(r8), parameter :: t0    = 288.0_r8  ! K
  real(r8), parameter :: gamma = 0.005_r8  ! K m-1
  real(r8), parameter :: dt    = 4.8e5_r8  ! K
  real(r8), parameter :: eta0  = 0.252
  real(r8), parameter :: etat  = 0.2       ! Tropopause level
  real(r8), parameter :: lonc  = pi / 9.0
  real(r8), parameter :: latc  = pi2 / 9.0
  real(r8), parameter :: up    = 1.0       ! m s-1  

contains

  subroutine baroclinic_wave_test_set_ic(block)

    type(block_type), intent(inout), target :: block

    real(r8) etav, eta, tbar, gzbar, sin_lat, cos_lat, half_lon, r
    integer i, j, k, im
    type(mesh_type), pointer :: mesh
    type(state_type), pointer :: state
    type(static_type), pointer :: static

    mesh => block%mesh
    state => block%state(1,ivector)
    static => block%static(ivector)

    state%phs = 1.0e5_r8
    state%v   = 0

    do k = mesh%half_lev_ibeg, mesh%half_lev_iend
      do j = mesh%full_lat_ibeg, mesh%full_lat_iend
        do i = mesh%full_lon_ibeg, mesh%full_lon_iend
          do im = 1 , member_num
            state%ph_lev(im,i,j,k) = vert_coord_calc_ph_lev(k, state%phs(im,i,j))
          end do
        end do
      end do
    end do
    call fill_halo_member(block, state%ph_lev, full_lon=.true., full_lat=.true., full_lev=.false.,async=state%async(async_ph_lev))
    call state%async(async_ph_lev)%wait()

    do k = mesh%full_lev_ibeg, mesh%full_lev_iend
      do j = mesh%full_lat_ibeg, mesh%full_lat_iend
        do i = mesh%full_lon_ibeg, mesh%full_lon_iend
          state%ph(:,i,j,k) = 0.5d0 * (state%ph_lev(:,i,j,k) + state%ph_lev(:,i,j,k+1))
        end do
      end do
    end do
    call fill_halo_member(block, state%ph, full_lon=.true., full_lat=.true., full_lev=.true.,async=state%async(async_ph))
    call state%async(async_ph)%wait()

    do k = mesh%full_lev_ibeg, mesh%full_lev_iend
      eta = mesh%full_lev(k)
      etav = (eta - eta0) * pi / 2d0
      do j = mesh%full_lat_ibeg, mesh%full_lat_iend
        sin_lat = mesh%full_sin_lat(j)
        cos_lat = mesh%full_cos_lat(j)
        do i = mesh%half_lon_ibeg, mesh%half_lon_iend
          half_lon = mesh%half_lon(i)
          r = 10.0 * acos(sin(latc) * sin_lat + cos(latc) * cos_lat * cos(half_lon - lonc))
          state%u(:,i,j,k) = u0 * cos(etav)**(1.5d0) * sin(2 * mesh%full_lat(j))**2 + &
                             up * merge(exp(-r**2), 0.0_r8, r**2 <= 50)
        end do
      end do
    end do
    call fill_halo_member(block, state%u, full_lon=.false., full_lat=.true., full_lev=.true.,async=state%async(async_u))
    call state%async(async_u)%wait()

    do k = mesh%full_lev_ibeg, mesh%full_lev_iend
      eta = mesh%full_lev(k)
      etav = (eta - eta0) * pi / 2d0
      if (etat <= eta .and. eta <= 1) then
        tbar = t0 * eta**(Rd * gamma / g)
      else
        tbar = t0 * eta**(Rd * gamma / g) + dt * (etat - eta)**5
      end if
      do j = mesh%full_lat_ibeg, mesh%full_lat_iend
        sin_lat = mesh%full_sin_lat(j)
        cos_lat = mesh%full_cos_lat(j)
        do i = mesh%full_lon_ibeg, mesh%full_lon_iend
          do im = 1 , member_num
            state%t(im,i,j,k) = tbar + 3d0 / 4d0 * eta * pi * u0 / Rd * sin(etav) * sqrt(cos(etav)) * (     &
                (-2 * sin_lat**6 * (cos_lat**2 + 1d0 / 3d0) + 10d0 / 63d0) * 2 * u0 * cos(etav)**1.5d0 + &
                (8d0 / 5d0 * cos_lat**3 * (sin_lat**2 + 2d0 / 3d0) - pi / 4d0) * radius * omega          &
              )
            state%pt(im,i,j,k) = potential_temperature(state%t(im,i,j,k), state%ph(im,i,j,k))
          end do
        end do
      end do
    end do
    call fill_halo_member(block, state%t , full_lon=.true., full_lat=.true., full_lev=.true.,async=state%async(async_t))
    call fill_halo_member(block, state%pt, full_lon=.true., full_lat=.true., full_lev=.true.,async=state%async(async_pt))
    call state%async(async_t)%wait()
    call state%async(async_pt)%wait()

    do k = mesh%half_lev_ibeg, mesh%half_lev_iend
      eta = mesh%half_lev(k)
      etav = (eta - eta0) * pi / 2d0
      if (etat <= eta .and. eta <= 1) then
        gzbar = t0 * g / gamma * (1 - eta**(Rd * gamma / g))
      else
        gzbar = t0 * g / gamma * (1 - eta**(Rd * gamma / g)) - Rd * dt * (   &
            (log(eta / etat) + 137d0 / 60d0) * etat**5 - 5 * etat**4 * eta + &
            5 * etat**3 * eta**2 - 10d0 / 3d0 * etat**2 * eta**3 +           &
            5d0 / 4d0 * etat * eta**4 - 1d0 / 5d0 * eta**5                   &
          )
      end if
      do j = mesh%full_lat_ibeg, mesh%full_lat_iend
        sin_lat = mesh%full_sin_lat(j)
        cos_lat = mesh%full_cos_lat(j)
        do i = mesh%full_lon_ibeg, mesh%full_lon_iend
          state%gz_lev(:,i,j,k) = gzbar + u0 * cos(etav)**1.5d0 * (                                &
            (-2 * sin_lat**6 * (cos_lat**2 + 1d0 / 3d0) + 10d0 / 63d0) * u0 * cos(etav)**1.5d0 + &
            (8d0 / 5d0 * cos_lat**3 * (sin_lat**2 + 2d0 / 3d0) - pi / 4d0) * radius * omega      &
          )
        end do
      end do
    end do
    call fill_halo_member(block, state%gz_lev, full_lon=.true., full_lat=.true., full_lev=.false.,async=state%async(async_ph_lev))
    call state%async(async_gz_lev)%wait()

    do k = mesh%full_lev_ibeg, mesh%full_lev_iend
      eta = mesh%full_lev(k)
      etav = (eta - eta0) * pi / 2d0
      if (etat <= eta .and. eta <= 1) then
        gzbar = t0 * g / gamma * (1 - eta**(Rd * gamma / g))
      else
        gzbar = t0 * g / gamma * (1 - eta**(Rd * gamma / g)) - Rd * dt * (   &
            (log(eta / etat) + 137d0 / 60d0) * etat**5 - 5 * etat**4 * eta + &
            5 * etat**3 * eta**2 - 10d0 / 3d0 * etat**2 * eta**3 +           &
            5d0 / 4d0 * etat * eta**4 - 1d0 / 5d0 * eta**5                   &
          )
      end if
      do j = mesh%full_lat_ibeg, mesh%full_lat_iend
        sin_lat = mesh%full_sin_lat(j)
        cos_lat = mesh%full_cos_lat(j)
        do i = mesh%full_lon_ibeg, mesh%full_lon_iend
          state%gz(:,i,j,k) = gzbar + u0 * cos(etav)**1.5d0 * (                                    &
            (-2 * sin_lat**6 * (cos_lat**2 + 1d0 / 3d0) + 10d0 / 63d0) * u0 * cos(etav)**1.5d0 + &
            (8d0 / 5d0 * cos_lat**3 * (sin_lat**2 + 2d0 / 3d0) - pi / 4d0) * radius * omega      &
          )
        end do
      end do
    end do
    call fill_halo_member(block, state%gz, full_lon=.true., full_lat=.true., full_lev=.true.,async=state%async(async_gz))
    call state%async(async_gz)%wait()

    etav = (1 - eta0) * pi / 2
    do j = mesh%full_lat_ibeg, mesh%full_lat_iend
      sin_lat = mesh%full_sin_lat(j)
      cos_lat = mesh%full_cos_lat(j)
      do i = mesh%full_lon_ibeg, mesh%full_lon_iend
        static%gzs(:,i,j) = u0 * cos(etav)**1.5d0 * (                                            &
          (-2 * sin_lat**6 * (cos_lat**2 + 1d0 / 3d0) + 10d0 / 63d0) * u0 * cos(etav)**1.5d0 + &
          (8d0 / 5d0 * cos_lat**3 * (sin_lat**2 + 2d0 / 3d0) - pi / 4d0) * radius * omega      &
        )
      end do
    end do
    call fill_halo_member(block, static%gzs, full_lon=.true., full_lat=.true.,async=state%async(async_gzs))
    call state%async(async_gzs)%wait()

  end subroutine baroclinic_wave_test_set_ic

end module baroclinic_wave_test_mod
