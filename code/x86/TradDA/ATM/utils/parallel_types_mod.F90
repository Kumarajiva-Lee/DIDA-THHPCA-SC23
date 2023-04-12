module parallel_types_mod

  use mpi
  use const_mod
  use mesh_mod
  use namelist_mod

  implicit none

  private

  public async_type
  public zonal_circle_type
  public process_type
  public round_robin
  public is_root_proc_pa

  public proc

  ! operators_mod中用到的
  integer, public, parameter :: async_u                       = 1
  integer, public, parameter :: async_u_lon                   = 2
  integer, public, parameter :: async_u_lat                   = 3
  integer, public, parameter :: async_v                       = 4
  integer, public, parameter :: async_v_lon                   = 5
  integer, public, parameter :: async_v_lat                   = 6

  integer, public, parameter :: async_we_lev                  = 7
  integer, public, parameter :: async_we_lev_lon              = 8
  integer, public, parameter :: async_we_lev_lat              = 9

  integer, public, parameter :: async_gz                      = 10
  integer, public, parameter :: async_gz_lev                  = 11

  integer, public, parameter :: async_m                       = 12
  integer, public, parameter :: async_m_vtx                   = 13
  integer, public, parameter :: async_m_lon                   = 14
  integer, public, parameter :: async_m_lat                   = 15
  integer, public, parameter :: async_m_lev                   = 16

  integer, public, parameter :: async_mfx_lon                 = 17
  integer, public, parameter :: async_mfy_lat                 = 18
  integer, public, parameter :: async_mfx_lat                 = 19
  integer, public, parameter :: async_mfy_lon                 = 20

  integer, public, parameter :: async_pv                      = 21
  integer, public, parameter :: async_pv_lon                  = 22
  integer, public, parameter :: async_pv_lat                  = 23

  integer, public, parameter :: async_ke                      = 24

  integer, public, parameter :: async_pt                      = 25
  integer, public, parameter :: async_ptf_lon                  = 26  
  integer, public, parameter :: async_ptf_lat                  = 27
  integer, public, parameter :: async_ptf_lev                  = 28

  integer, public, parameter :: async_t                       = 29
  integer, public, parameter :: async_ph                      = 30
  integer, public, parameter :: async_ph_lev                  = 31 
  integer, public, parameter :: async_ph_exn_lev              = 32
  
  integer, public, parameter :: async_phs                     = 33
  integer, public, parameter :: async_div                     = 34  
  integer, public, parameter :: async_div2                    = 35  
  integer, public, parameter :: async_vor                     = 36

  ! integer, public, parameter :: async_wedphdlev               = 36
  ! integer, public, parameter :: async_w                       = 37
  ! integer, public, parameter :: async_w_lev                   = 38
  ! integer, public, parameter :: async_w_lev_lon               = 39
  ! integer, public, parameter :: async_w_lev_lat               = 40
  ! integer, public, parameter :: async_gz_lev_lon              = 41
  ! integer, public, parameter :: async_gz_lev_lat              = 42
  ! integer, public, parameter :: async_rhod                    = 43
  ! integer, public, parameter :: async_rhod_lon                = 44 
  ! integer, public, parameter :: async_rhod_lat                = 45
  ! integer, public, parameter :: async_p                       = 46
  ! integer, public, parameter :: async_p_lev                   = 47  
  ! integer, public, parameter :: async_p_lev_lon               = 48
  ! integer, public, parameter :: async_p_lev_lat               = 49
  ! integer, public, parameter :: async_u_lev_lon               = 50
  ! integer, public, parameter :: async_v_lev_lat               = 51
  ! integer, public, parameter :: async_mf_lev_lon_n            = 52  
  ! integer, public, parameter :: async_mf_lev_lat_n            = 53
  ! integer, public, parameter :: async_tension_h               = 54
  ! integer, public, parameter :: async_shear_h                 = 55
  ! integer, public, parameter :: async_kmh                     = 56
  ! integer, public, parameter :: async_kmh_vtx                 = 57
  ! integer, public, parameter :: async_kmh_lon                 = 58
  ! integer, public, parameter :: async_kmh_lat                 = 59

  integer, public, parameter :: async_qv                      = 37
  integer, public, parameter :: async_qm                      = 38

  integer, public, parameter :: async_smag_t                  = 39
  integer, public, parameter :: async_smag_s                  = 40
  integer, public, parameter :: async_kmh                     = 41
  integer, public, parameter :: async_kmh_lon                 = 42
  integer, public, parameter :: async_kmh_lat                 = 43

  integer, public, parameter :: async_du                      = 44
  integer, public, parameter :: async_dv                      = 45  
  integer, public, parameter :: async_dgz                     = 46
  integer, public, parameter :: async_dpt                     = 47 
  integer, public, parameter :: async_dphs                    = 48

  integer, public, parameter :: async_qhv                     = 49
  integer, public, parameter :: async_qhu                     = 50  
  integer, public, parameter :: async_dkedlon                 = 51
  integer, public, parameter :: async_dkedlat                 = 52
  integer, public, parameter :: async_dmfdlon                 = 53
  integer, public, parameter :: async_dmfdlat                 = 54
  integer, public, parameter :: async_dptfdlon                = 55
  integer, public, parameter :: async_dptfdlat                = 56  
  integer, public, parameter :: async_dptfdlev                = 57
  integer, public, parameter :: async_pgf_lon                 = 58  
  integer, public, parameter :: async_pgf_lat                 = 59
  integer, public, parameter :: async_wedudlev                = 60
  integer, public, parameter :: async_wedvdlev                = 61
  integer, public, parameter :: async_smag_dptdt              = 62
  integer, public, parameter :: async_smag_dudt               = 63
  integer, public, parameter :: async_smag_dvdt               = 64

  integer, public, parameter :: async_landmask                = 65
  integer, public, parameter :: async_gzs                     = 66    
  integer, public, parameter :: async_zs_std                  = 67
  integer, public, parameter :: async_dzsdlon                 = 68
  integer, public, parameter :: async_dzsdlat                 = 69  
  integer, public, parameter :: async_ref_ps                  = 70

  integer, public, parameter :: async_mfx                     = 71
  integer, public, parameter :: async_mfy                     = 72
  integer, public, parameter :: async_mx                      = 73
  integer, public, parameter :: async_my                      = 74
  integer, public, parameter :: async_qmfx                    = 75
  integer, public, parameter :: async_qmfy                    = 76
  integer, public, parameter :: async_qx                      = 77
  integer, public, parameter :: async_qy                      = 78
  integer, public, parameter :: async_mlx                     = 79
  integer, public, parameter :: async_dmx                     = 80
  integer, public, parameter :: async_m6x                     = 81
  integer, public, parameter :: async_mly                     = 82
  integer, public, parameter :: async_dmy                     = 83
  integer, public, parameter :: async_m6y                     = 84




  integer, public, parameter :: async_tmp                     = 90

  !adv

  integer, public, parameter :: async_total_num               = 91
  integer, public, parameter :: async_adv_num                 = 10

  type async_type
    integer, allocatable :: send_req(:)
    integer, allocatable :: recv_req(:)
  contains
    procedure :: init => async_init
    procedure :: wait => async_wait
    final :: async_final
  end type async_type

  type process_neighbor_type
    integer :: id       = MPI_PROC_NULL
    integer :: cart_id  = MPI_PROC_NULL
    integer :: orient   = 0                         !east west south north
    integer :: ngb_type = 0                         !send recv
    integer :: tag      = 0                         !message tag 
    integer :: lon_ibeg = inf_i4
    integer :: lon_iend = inf_i4
    integer :: lat_ibeg = inf_i4
    integer :: lat_iend = inf_i4
  contains
    procedure :: init => process_neighbor_init
  end type process_neighbor_type

  type zonal_circle_type
    integer :: group = MPI_GROUP_NULL
    integer :: comm  = MPI_COMM_NULL
    integer :: np    = 0
    integer :: id    = MPI_PROC_NULL
    integer :: west_ngb_id    = MPI_PROC_NULL
    integer :: east_ngb_id    = MPI_PROC_NULL
    integer, allocatable :: recv_type_r4(:,:) ! 0: one level, 1: full_lev, 2: half_lev
    integer, allocatable :: recv_type_r8(:,:) ! 0: one level, 1: full_lev, 2: half_lev
  contains
    procedure :: init => zonal_circle_init
    final :: zonal_circle_final
  end type zonal_circle_type

  type irr_comm_type
    integer :: group = MPI_GROUP_NULL
    integer :: comm  = MPI_COMM_NULL
    integer :: cart_group = MPI_GROUP_NULL
    integer :: cart_comm = MPI_COMM_NULL
    integer :: np    = 0
    integer :: id    = MPI_PROC_NULL
    integer :: cart_id = MPI_PROC_NULL
    integer :: part_id
    integer :: pid_ibeg = -1
    integer :: cart_dims(2)   = 0
    integer :: cart_coords(2) = 0
    integer, allocatable :: recv_type_r8(:,:) ! 0: one level, 1: full_lev, 2: half_lev

  end type irr_comm_type

  type process_type
    integer :: comm           = MPI_COMM_NULL
    integer :: cart_comm      = MPI_COMM_NULL
    integer :: group          = MPI_GROUP_NULL
    integer :: cart_group     = MPI_GROUP_NULL
    integer :: cart_dims(2)   = 0
    integer :: cart_coords(2) = 0
    integer :: id             = MPI_PROC_NULL          ! MPI process ID
    integer :: cart_id        = MPI_PROC_NULL          ! MPI process ID in cart_comm
    integer idom                                       ! Nest domain index (root domain is 1)
    integer west_pids(2), east_pids(2)                 ! 给reduced_chunk用
    integer np
    integer num_lon
    integer num_lat
    integer lon_ibeg
    integer lon_iend
    integer lat_ibeg
    integer lat_iend
    integer comp_lat_ibeg
    integer comp_lat_iend
    integer lon_halo_width , lon_halo_width_north , lon_halo_width_south
    integer lat_halo_width , lat_halo_width_north , lat_halo_width_south

    logical :: at_south_pole = .false.
    logical :: at_north_pole = .false.

    integer member_value
    
    integer ngb_num
    integer ngb_send_num , ngb_recv_num
    integer ngb_north_send_beg , ngb_south_send_beg
    integer ngb_north_recv_beg , ngb_south_recv_beg

    type(irr_comm_type) irr_comm
    type(zonal_circle_type) zonal_circle
    type(process_neighbor_type), allocatable :: ngb(:) ! Neighbor processes send
    type(process_neighbor_type), allocatable :: ngb_send(:) ! Neighbor processes send
    type(process_neighbor_type), allocatable :: ngb_recv(:) ! Neighbor processes recv
    ! type(block_type), allocatable :: blocks(:)

    integer decomp_type
    integer decomp_loc
  end type process_type

  type(process_type) proc

contains

  subroutine async_init(this, num_send_ngb , num_recv_ngb)

    class(async_type), intent(inout) :: this
    integer, intent(in) :: num_send_ngb
    integer, intent(in) :: num_recv_ngb

    if (allocated(this%send_req)) deallocate(this%send_req)
    if (allocated(this%recv_req)) deallocate(this%recv_req)

    allocate(this%send_req(num_send_ngb)); this%send_req = MPI_REQUEST_NULL
    allocate(this%recv_req(num_recv_ngb)); this%recv_req = MPI_REQUEST_NULL

  end subroutine async_init

  subroutine async_wait(this)

    class(async_type), intent(inout) :: this

    integer i, ierr

    ! do i = 1, size(this%send_req)
    !   if (this%send_req(i) /= MPI_REQUEST_NULL) then
    !     call MPI_WAIT(this%send_req(i), MPI_STATUS_IGNORE, ierr)
    !   end if
    !   if (this%recv_req(i) /= MPI_REQUEST_NULL) then
    !     call MPI_WAIT(this%recv_req(i), MPI_STATUS_IGNORE, ierr)
    !   end if
    ! end do

    do i = 1, size(this%send_req)
      call MPI_WAIT(this%send_req(i), MPI_STATUS_IGNORE, ierr)
    end do
    do i = 1, size(this%recv_req)
      call MPI_WAIT(this%recv_req(i), MPI_STATUS_IGNORE, ierr)
    end do

  end subroutine async_wait

  subroutine async_final(this)

    type(async_type), intent(inout) :: this

    if (allocated(this%send_req)) deallocate(this%send_req)
    if (allocated(this%recv_req)) deallocate(this%recv_req)

  end subroutine async_final

  subroutine zonal_circle_init(this)

    class(zonal_circle_type), intent(inout) :: this

    integer ierr, i, num_lon, ibeg, iend
    integer west_cart_id, east_cart_id, tmp_id(1)
    integer, allocatable :: zonal_proc_id(:)
    
    if (partition_type == 'regular') then
      allocate(zonal_proc_id(proc%cart_dims(1)))
      do i = 1, proc%cart_dims(1)
        call MPI_CART_RANK(proc%cart_comm, [i-1,proc%cart_coords(2)], zonal_proc_id(i), ierr)
      end do
      call MPI_GROUP_INCL(proc%cart_group, size(zonal_proc_id), zonal_proc_id, this%group, ierr)
      call MPI_COMM_CREATE_GROUP(proc%cart_comm, this%group, sum(zonal_proc_id), this%comm, ierr)
      call MPI_COMM_SIZE(this%comm, this%np, ierr)
      call MPI_COMM_RANK(this%comm, this%id, ierr)
      deallocate(zonal_proc_id)

      ! Get IDs of the west and east neighbors in zonal circle comm.
      call MPI_CART_SHIFT(proc%cart_comm, 0, 1, west_cart_id, east_cart_id, ierr)
      call MPI_GROUP_TRANSLATE_RANKS(proc%cart_group, 1, [west_cart_id], this%group, tmp_id, ierr); this%west_ngb_id = tmp_id(1)
      call MPI_GROUP_TRANSLATE_RANKS(proc%cart_group, 1, [east_cart_id], this%group, tmp_id, ierr); this%east_ngb_id = tmp_id(1)
    else if (partition_type == 'irregular') then
      allocate(zonal_proc_id(proc%irr_comm%cart_dims(1)))
      do i = 1, proc%irr_comm%cart_dims(1)
        call MPI_CART_RANK(proc%irr_comm%cart_comm, [i-1,proc%irr_comm%cart_coords(2)], zonal_proc_id(i), ierr)
      end do
      call MPI_GROUP_INCL(proc%irr_comm%cart_group, size(zonal_proc_id), zonal_proc_id, this%group, ierr)
      call MPI_COMM_CREATE_GROUP(proc%irr_comm%cart_comm, this%group, sum(zonal_proc_id), this%comm, ierr)
      call MPI_COMM_SIZE(this%comm, this%np, ierr)
      call MPI_COMM_RANK(this%comm, this%id, ierr)
      deallocate(zonal_proc_id)

      ! Get IDs of the west and east neighbors in zonal circle comm.
      call MPI_CART_SHIFT(proc%irr_comm%cart_comm, 0, 1, west_cart_id, east_cart_id, ierr)
      call MPI_GROUP_TRANSLATE_RANKS(proc%irr_comm%cart_group, 1, [west_cart_id], this%group, tmp_id, ierr); this%west_ngb_id = tmp_id(1)
      call MPI_GROUP_TRANSLATE_RANKS(proc%irr_comm%cart_group, 1, [east_cart_id], this%group, tmp_id, ierr); this%east_ngb_id = tmp_id(1)
    end if

    

    if (this%id == 0) then
      ! Single precision
      allocate(this%recv_type_r4(this%np,0:2))
      do i = 1, this%np
        num_lon = global_mesh%full_nlon
        call round_robin(this%np, i - 1, num_lon, ibeg, iend)
        call MPI_TYPE_CREATE_SUBARRAY(2, [member_num , global_mesh%full_nlon], &
                                         [member_num ,             num_lon], &
                                         [0,ibeg-1], MPI_ORDER_FORTRAN, MPI_REAL, &
                                         this%recv_type_r4(i,0), ierr)
        call MPI_TYPE_COMMIT(this%recv_type_r4(i,0), ierr)
        call MPI_TYPE_CREATE_SUBARRAY(3, [member_num , global_mesh%full_nlon,global_mesh%full_nlev], &
                                         [member_num ,                  num_lon,global_mesh%full_nlev], &
                                         [0,ibeg-1,0], MPI_ORDER_FORTRAN, MPI_REAL, &
                                         this%recv_type_r4(i,1), ierr)
        call MPI_TYPE_COMMIT(this%recv_type_r4(i,1), ierr)
        call MPI_TYPE_CREATE_SUBARRAY(3, [member_num , global_mesh%full_nlon,global_mesh%half_nlev], &
                                         [member_num ,                  num_lon,global_mesh%half_nlev], &
                                         [0,ibeg-1,0], MPI_ORDER_FORTRAN, MPI_REAL, &
                                         this%recv_type_r4(i,2), ierr)
        call MPI_TYPE_COMMIT(this%recv_type_r4(i,2), ierr)
      end do
      ! Double precision
      allocate(this%recv_type_r8(this%np,0:2))
      do i = 1, this%np
        num_lon = global_mesh%full_nlon
        call round_robin(this%np, i - 1, num_lon, ibeg, iend)
        call MPI_TYPE_CREATE_SUBARRAY(2, [member_num , global_mesh%full_nlon], &
                                         [member_num ,             num_lon], &
                                         [0,ibeg-1], MPI_ORDER_FORTRAN, MPI_DOUBLE, &
                                         this%recv_type_r8(i,0), ierr)
        call MPI_TYPE_COMMIT(this%recv_type_r8(i,0), ierr)
        call MPI_TYPE_CREATE_SUBARRAY(3, [member_num , global_mesh%full_nlon,global_mesh%full_nlev], &
                                         [member_num ,                  num_lon,global_mesh%full_nlev], &
                                         [0,ibeg-1,0], MPI_ORDER_FORTRAN, MPI_DOUBLE, &
                                         this%recv_type_r8(i,1), ierr)
        call MPI_TYPE_COMMIT(this%recv_type_r8(i,1), ierr)
        call MPI_TYPE_CREATE_SUBARRAY(3, [member_num , global_mesh%full_nlon,global_mesh%half_nlev], &
                                         [member_num ,                  num_lon,global_mesh%half_nlev], &
                                         [0,ibeg-1,0], MPI_ORDER_FORTRAN, MPI_DOUBLE, &
                                         this%recv_type_r8(i,2), ierr)
        call MPI_TYPE_COMMIT(this%recv_type_r8(i,2), ierr)
      end do
      
    end if

  end subroutine zonal_circle_init

  subroutine zonal_circle_final(this)

    type(zonal_circle_type), intent(inout) :: this

    integer i, k, ierr

    if (allocated(this%recv_type_r4)) then
      do k = 0, 2
        do i = 1, this%np
          call MPI_TYPE_FREE(this%recv_type_r4(i,k), ierr)
        end do
        deallocate(this%recv_type_r4)
      end do
    end if

    if (allocated(this%recv_type_r8)) then
      do k = 0, 2
        do i = 1, this%np
          call MPI_TYPE_FREE(this%recv_type_r8(i,k), ierr)
        end do
        deallocate(this%recv_type_r8)
      end do
    end if

    if (this%group /= MPI_GROUP_NULL) call MPI_GROUP_FREE(this%group, ierr)

  end subroutine zonal_circle_final

  subroutine process_neighbor_init(this, orient, ngb_type, tag, lon_ibeg, lon_iend, lat_ibeg, lat_iend)

    class(process_neighbor_type), intent(inout) :: this
    integer, intent(in) :: orient
    integer, intent(in), optional :: ngb_type
    integer, intent(in), optional :: tag
    integer, intent(in), optional :: lon_ibeg
    integer, intent(in), optional :: lon_iend
    integer, intent(in), optional :: lat_ibeg
    integer, intent(in), optional :: lat_iend

    this%orient      = orient
    this%ngb_type    = ngb_type
    this%tag         = tag

    this%lon_ibeg = lon_ibeg
    this%lon_iend = lon_iend
    this%lat_ibeg = lat_ibeg
    this%lat_iend = lat_iend

  end subroutine process_neighbor_init

  subroutine round_robin(dim, coord, num, ibeg, iend)

    integer, intent(in) :: dim
    integer, intent(in) :: coord
    integer, intent(inout) :: num
    integer, intent(out) :: ibeg ! Start from 1.
    integer, intent(out) :: iend ! Start from 1.

    integer res_num, tmp_num, i

    res_num = mod(num, dim)
    ibeg = 1
    do i = 0, coord - 1
      if (res_num /= 0 .and. i < res_num) then
        tmp_num = num / dim + 1
      else
        tmp_num = num / dim
      end if
      ibeg = ibeg + tmp_num
    end do
    if (res_num /= 0 .and. coord < res_num) then
      num = num / dim + 1
    else
      num = num / dim
    end if
    iend = ibeg + num - 1

  end subroutine round_robin

  pure logical function is_root_proc_pa()

    is_root_proc_pa = proc%id == 0

  end function is_root_proc_pa

end module parallel_types_mod
