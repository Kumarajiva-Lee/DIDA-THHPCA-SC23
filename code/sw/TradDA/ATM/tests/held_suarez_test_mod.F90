module held_suarez_test_mod

  use flogger
  use namelist_mod
  use const_mod
  use formula_mod
  use vert_coord_mod
  use block_mod
  use parallel_mod
  use parallel_types_mod
  use rossby_haurwitz_wave_3d_test_mod
  use member_mod

  implicit none

  private

  public held_suarez_test_set_ic
  public held_suarez_test_apply_forcing

  real(r8), parameter :: sig_b    = 0.7_r8
  real(r8), parameter :: kf       = 1.0_r8 / 86400.0_r8   ! s-1
  real(r8), parameter :: ka       = 0.025_r8 / 86400.0_r8 ! s-1
  real(r8), parameter :: ks       = 0.25_r8 / 86400.0_r8  ! s-1
  real(r8), parameter :: dt_lat   = 60.0_r8               ! K
  real(r8), parameter :: dpt_lev  = 10.0_r8               ! K
  real(r8), parameter :: p0       = 1.0e5_r8              ! Pa

contains

  subroutine held_suarez_test_set_ic(block)

    type(block_type), intent(inout), target :: block

    type(mesh_type), pointer :: mesh
    type(state_type), pointer :: state
    integer i, j, k, im
    real(r8) random

    call rossby_haurwitz_wave_3d_test_set_ic(block)

    mesh => block%mesh
    state => block%state(1,ivector)

    call random_seed()

    do j = mesh%full_lat_ibeg, mesh%full_lat_iend
      do i = mesh%full_lon_ibeg, mesh%full_lon_iend
        call random_number(random)
        state%phs(:,i,j) = state%phs(:,i,j) - (0.5_r8 + random) * mesh%full_cos_lat(j)**2
      end do
    end do
    call fill_halo_member(block, state%phs, full_lon=.true., full_lat=.true.,async=state%async(async_phs))
    call state%async(async_phs)%wait()

    do k = mesh%full_lev_ibeg, mesh%full_lev_iend
      do j = mesh%full_lat_ibeg, mesh%full_lat_iend
        do i = mesh%full_lon_ibeg, mesh%full_lon_iend
          call random_number(random)
          do im = 1 , member_num
            state%pt(im , i,j,k) = potential_temperature(state%t(im ,i,j,k), state%ph(im ,i,j,k)) - (0.5_r8 + random) * mesh%full_cos_lat(j)**2
          end do
        end do
      end do
    end do
    call fill_halo_member(block, state%pt, full_lon=.true., full_lat=.true., full_lev=.true.,async=state%async(async_pt))
    call state%async(async_pt)%wait()

  end subroutine held_suarez_test_set_ic

  subroutine held_suarez_test_apply_forcing(block, dt, state)

    type(block_type), intent(in) :: block
    real(8), intent(in) :: dt
    type(state_type), intent(inout) :: state

    type(mesh_type), pointer :: mesh
    real(r8) kv, kt, teq, p_p0
    integer i, j, k, im

    mesh => state%mesh
    do k = mesh%full_lev_ibeg, mesh%full_lev_iend
      kv = kf * max(0.0_r8, (mesh%full_lev(k) - sig_b) / (1.0_r8 - sig_b))
      do j = mesh%full_lat_ibeg_no_pole, mesh%full_lat_iend_no_pole
        do i = mesh%half_lon_ibeg, mesh%half_lon_iend
          state%u(:,i,j,k) = state%u(:,i,j,k) - dt * kv * state%u(:,i,j,k)
        end do
      end do
    end do
    call fill_halo_member(block, state%u, full_lon=.false., full_lat=.true., full_lev=.true.,async=state%async(async_u))
    call state%async(async_u)%wait()

    do k = mesh%full_lev_ibeg, mesh%full_lev_iend
      kv = kf * max(0.0_r8, (mesh%full_lev(k) - sig_b) / (1.0_r8 - sig_b))
      do j = mesh%half_lat_ibeg_no_pole, mesh%half_lat_iend_no_pole
        do i = mesh%full_lon_ibeg, mesh%full_lon_iend
          state%v(:,i,j,k) = state%v(:,i,j,k) - dt * kv * state%v(:,i,j,k)
        end do
      end do
    end do
    call fill_halo_member(block, state%v, full_lon=.true., full_lat=.false., full_lev=.true.,async=state%async(async_v))
    call state%async(async_v)%wait()

    do k = mesh%full_lev_ibeg, mesh%full_lev_iend
      do j = mesh%full_lat_ibeg, mesh%full_lat_iend
        kt = ka + (ks - ka) * max(0.0_r8, (mesh%full_lev(k) - sig_b) / (1.0_r8 - sig_b)) * mesh%full_cos_lat(j)**4
        do i = mesh%full_lon_ibeg, mesh%full_lon_iend
          do im = 1 , member_num
            p_p0 = state%ph(im,i,j,k) / p0
            teq = max(200.0_r8, (315.0_r8 - dt_lat * mesh%full_sin_lat(j)**2 - dpt_lev * log(p_p0) * mesh%full_cos_lat(j)**2) * p_p0**Rd_o_cp)
            state%pt(im,i,j,k) = state%pt(im,i,j,k) - dt * kt * state%pt(im,i,j,k) * (1.0_r8 - teq / state%t(im,i,j,k))
          end do
        end do
      end do
    end do
    call fill_halo_member(block, state%pt, full_lon=.true., full_lat=.true., full_lev=.true.,async=state%async(async_pt))
    call state%async(async_pt)%wait()

  end subroutine held_suarez_test_apply_forcing

end module held_suarez_test_mod
