module halo_mod

  use mpi
  use flogger
  use const_mod
  use mesh_mod

  implicit none

  private

  public halo_type

  integer, parameter :: cross_proc_halo = 1
  integer, parameter :: cross_comm_halo = 2
  integer, parameter :: inner_halo = 3
  integer, parameter :: nest_halo = 4

  type halo_type
    integer :: comm = MPI_COMM_NULL
    integer :: host_id = MPI_PROC_NULL
    integer :: proc_id = MPI_PROC_NULL
    integer :: iblk = 0
    integer :: orient = 0
    integer :: dtype = 0
    integer :: type = 0
    integer :: tag  = 0
    integer :: lon_hw
    integer :: lat_hw
    integer :: mpi_type_2d(2,2)    = MPI_DATATYPE_NULL
    integer :: mpi_type_3d(2,2,2)  = MPI_DATATYPE_NULL

    !For origin subroutine
    integer :: send_type_2d(2,2) = MPI_DATATYPE_NULL
    integer :: recv_type_2d(2,2) = MPI_DATATYPE_NULL
    integer :: send_type_3d(2,2,2) = MPI_DATATYPE_NULL
    integer :: recv_type_3d(2,2,2) = MPI_DATATYPE_NULL
  contains
    procedure :: init => halo_init
    procedure :: init_nest => halo_init_nest
    procedure :: clear => halo_clear
    final :: halo_final
  end type halo_type

contains

  subroutine halo_init(this, mesh, orient, dtype, host_id, ngb_proc_id, tag, iblk, lon_ibeg, lat_ibeg, &
                       halo_lon_ibeg, halo_lon_iend, halo_lat_ibeg, halo_lat_iend, debug)

    class(halo_type), intent(out) :: this
    type(mesh_type), intent(in) :: mesh
    integer, intent(in) :: orient 
    integer, intent(in) :: dtype
    integer, intent(in) :: host_id
    integer, intent(in), optional :: ngb_proc_id
    integer, intent(in), optional :: tag
    integer, intent(in), optional :: iblk
    !integer, intent(in), optional :: member_num
    integer, intent(in), optional :: lon_ibeg
    integer, intent(in), optional :: lat_ibeg
    integer, intent(in), optional :: halo_lon_ibeg
    integer, intent(in), optional :: halo_lon_iend
    integer, intent(in), optional :: halo_lat_ibeg
    integer, intent(in), optional :: halo_lat_iend
    logical, intent(in), optional :: debug

    integer full_lon_ibeg, full_lon_iend
    integer full_lat_ibeg, full_lat_iend
    integer half_lon_ibeg, half_lon_iend
    integer half_lat_ibeg, half_lat_iend
    integer array_size(4,2,2)
    integer mpi_subarray_size(4,2,2)
    !integer recv_subarray_size(4,2,2)
    integer mpi_subarray_start(4,2,2)
    !integer recv_subarray_start(4,2,2)
    integer num_lev(2)
    integer i, j, k, ierr
    logical :: unequal = .false.

    if (present(ngb_proc_id)) then
      this%proc_id = ngb_proc_id
    else if (present(iblk)) then
      call log_error('Handle internal halo!', __FILE__, __LINE__)
    end if

    this%host_id = host_id
    this%dtype   = dtype
    this%tag     = tag

    this%lon_hw  = mesh%lon_hw
    this%lat_hw  = mesh%lat_hw



    full_lon_ibeg = halo_lon_ibeg - lon_ibeg
    full_lon_iend = halo_lon_iend - lon_ibeg
    half_lon_ibeg = full_lon_ibeg
    half_lon_iend = full_lon_iend

    full_lat_ibeg = halo_lat_ibeg - lat_ibeg
    full_lat_iend = halo_lat_iend - lat_ibeg
    half_lat_ibeg = merge(full_lat_ibeg - 1, full_lat_ibeg, mesh%has_north_pole() .and. orient == north)
    half_lat_iend = merge(full_lat_iend - 1, full_lat_iend, mesh%has_north_pole() .and. orient == north)

    
    ! NOTE: MPI array index starts from zero.

    !                          wx                          nx                          wx      
    !                          |                           |                           |
    !                  |---------------|---------------------------------------|---------------|
    !                  |_______________|_______________________________________|_______________|__ 
    !                  |\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|  |
    !         wy + ny -|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|  |- wy
    !                  |\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|  |
    !                  |_______|_______|_______________________________________|_______________|__|
    !                  |///////|///////|       |       |       |       |       |///////|///////|  |
    !     wy + ny - 1 -|///////|///////|       |       |       |       |       |///////|///////|  |
    !                  |///////|///////|       |       |       |       |       |///////|///////|  |
    !                  |_______|_______|_______|_______|_______|_______|_______|_______|_______|  |
    !                  |///////|///////|       |       |       |       |       |///////|///////|  |
    !                  |///////|///////|       |       |       |       |       |///////|///////|  |- ny
    !                  |///////|///////|       |       |       |       |       |///////|///////|  |
    !                  |_______|_______|_______|_______|_______|_______|_______|_______|_______|  |
    !                  |///////|///////|       |       |       |       |       |///////|///////|  |
    !              wy -|///////|///////|       |       |       |       |       |///////|///////|  |
    !                  |///////|///////|       |       |       |       |       |///////|///////|  |
    !                  |_______|_______|_______|_______|_______|_______|_______|_______|_______|__|
    !                  |\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|  |
    !               0 -|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|  |- wy
    !                  |\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|\\\\\\\|  |
    !                  |_______|_______|_______|_______|_______|_______|_______|_______|_______|__|
    !                      |               |                       |       |       |
    !                      0               wx                      | wx + nx - 1   |
    !                                                              |            wx + nx
    !                                                              nx


    this%orient = orient
    this%type = cross_proc_halo
    num_lev = [mesh%full_kme-mesh%full_kms+1,mesh%half_kme-mesh%half_kms+1]

    ! if (host_id == 18) then
    !   write(*,*) "size = " ,  mesh%full_nlon+2*mesh%lon_hw,mesh%full_nlat+2*mesh%lat_hw , "subsize = " , full_lon_iend-full_lon_ibeg+1,full_lat_iend-full_lat_ibeg+1
    ! end if

    
    do k = 1, 2
      array_size(:,1,1) = [member_num , mesh%full_nlon+2*mesh%lon_hw,mesh%full_nlat+2*mesh%lat_hw,num_lev(k)]
      array_size(:,2,1) = [member_num , mesh%half_nlon+2*mesh%lon_hw,mesh%full_nlat+2*mesh%lat_hw,num_lev(k)]
      array_size(:,1,2) = [member_num , mesh%full_nlon+2*mesh%lon_hw,mesh%half_nlat+2*mesh%lat_hw,num_lev(k)]
      array_size(:,2,2) = [member_num , mesh%half_nlon+2*mesh%lon_hw,mesh%half_nlat+2*mesh%lat_hw,num_lev(k)]

      mpi_subarray_size(:,1,1) = [member_num , full_lon_iend-full_lon_ibeg+1,full_lat_iend-full_lat_ibeg+1,num_lev(k)]
      mpi_subarray_size(:,2,1) = [member_num , half_lon_iend-half_lon_ibeg+1,full_lat_iend-full_lat_ibeg+1,num_lev(k)]
      mpi_subarray_size(:,1,2) = [member_num , full_lon_iend-full_lon_ibeg+1,half_lat_iend-half_lat_ibeg+1,num_lev(k)]
      mpi_subarray_size(:,2,2) = [member_num , half_lon_iend-half_lon_ibeg+1,half_lat_iend-half_lat_ibeg+1,num_lev(k)]

      ! full_lon + full_lat
      mpi_subarray_start(:,1,1) = [0,full_lon_ibeg,full_lat_ibeg,0]
      ! half_lon + full_lat
      mpi_subarray_start(:,2,1) = [0,half_lon_ibeg,full_lat_ibeg,0]
      ! full_lon + half_lat
      mpi_subarray_start(:,1,2) = [0,full_lon_ibeg,half_lat_ibeg,0]
      ! half_lon + half_lat
      mpi_subarray_start(:,2,2) = [0,half_lon_ibeg,half_lat_ibeg,0]

      do j = 1, 2
        do i = 1, 2
          ! if (host_id == 18) then
          !   write(*,*) "3d " , i , j , k , "size =  " ,  array_size(:,i,j) , "subsize = " , mpi_subarray_size(:,i,j) , "start = " , mpi_subarray_start(:,i,j)
          ! end if
          call MPI_TYPE_CREATE_SUBARRAY(4, array_size(:,i,j), mpi_subarray_size(:,i,j), &
                                        mpi_subarray_start(:,i,j), MPI_ORDER_FORTRAN, dtype, &
                                        this%mpi_type_3d(i,j,k), ierr)
          call MPI_TYPE_COMMIT(this%mpi_type_3d(i,j,k), ierr)
        end do
      end do
    end do

    do j = 1, 2
      do i = 1, 2
        call MPI_TYPE_CREATE_SUBARRAY(3, array_size(1:3,i,j), mpi_subarray_size(1:3,i,j), &
                                      mpi_subarray_start(1:3,i,j), MPI_ORDER_FORTRAN, dtype, &
                                      this%mpi_type_2d(i,j), ierr)
        call MPI_TYPE_COMMIT(this%mpi_type_2d(i,j), ierr)
      end do
    end do

  end subroutine halo_init

  subroutine halo_init_nest(this, parent_mesh, parent_proc_id)

    class(halo_type), intent(inout) :: this
    type(mesh_type), intent(in) :: parent_mesh
    integer, intent(in) :: parent_proc_id

  end subroutine halo_init_nest

  subroutine halo_clear(this)

    class (halo_type), intent(inout) :: this

    integer i, j, k
    integer ierr

    do k = 1, 2
      do j = 1, 2
        do i = 1, 2
          !if (this%mpi_type_3d(i,j,k) /= MPI_DATATYPE_NULL) call MPI_TYPE_FREE(this%mpi_type_3d(i,j,k), ierr)
        end do
      end do
    end do

    do j = 1, 2
      do i = 1, 2
        !if (this%mpi_type_2d(i,j) /= MPI_DATATYPE_NULL) call MPI_TYPE_FREE(this%mpi_type_2d(i,j), ierr)
      end do
    end do

  end subroutine halo_clear

  subroutine halo_final(this)

    type(halo_type), intent(inout) :: this

    call this%clear()
    
  end subroutine halo_final

end module halo_mod
