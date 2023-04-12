module parallel_zonal_mod
    use mpi
    use mesh_mod
    use parallel_types_mod

    implicit none
    
    public zonal_sum
    public zonal_sum_member
    public zonal_sum_ensure_order
    public gather_zonal_array
    public scatter_zonal_array
    

    interface zonal_sum_old
        module procedure zonal_sum_old_0d_r8
        module procedure zonal_sum_old_1d_r8
        module procedure zonal_sum_old_2d_r8
    end interface zonal_sum_old

    interface zonal_sum
        module procedure zonal_sum_0d_r8
        module procedure zonal_sum_1d_r8
    end interface zonal_sum

    interface zonal_sum_member
        module procedure zonal_sum_0d_r4_member
        module procedure zonal_sum_0d_r8_member
        module procedure zonal_sum_1d_r4_member
        module procedure zonal_sum_1d_r8_member
    end interface zonal_sum_member

    interface zonal_sum_ensure_order
        module procedure zonal_sum_ensure_order_1d_r8
        module procedure zonal_sum_ensure_order_2d_r8
    end interface zonal_sum_ensure_order

    interface gather_zonal_array  ! need add member
        module procedure gather_zonal_array_1d_r8
        module procedure gather_zonal_array_2d_r8
    end interface gather_zonal_array

    interface scatter_zonal_array ! need add member 
        module procedure scatter_zonal_array_1d_r8
        module procedure scatter_zonal_array_2d_r8
    end interface scatter_zonal_array

    interface gather_zonal_array_member
        module procedure gather_zonal_array_1d_r4_member
        module procedure gather_zonal_array_1d_r8_member
        module procedure gather_zonal_array_2d_r4_member
        module procedure gather_zonal_array_2d_r8_member
    end interface gather_zonal_array_member

    interface scatter_zonal_array_member 
        module procedure scatter_zonal_array_1d_r4_member
        module procedure scatter_zonal_array_1d_r8_member
        module procedure scatter_zonal_array_2d_r4_member
        module procedure scatter_zonal_array_2d_r8_member
    end interface scatter_zonal_array_member

contains

  subroutine zonal_sum_0d_r8(zonal_circle, work, value)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: work(:)
    real(8), intent(out) :: value

#ifdef ENSURE_ORDER
    real(8) allvalue(global_mesh%full_nlon)
#endif
    integer ierr

#ifdef ENSURE_ORDER
    if (zonal_circle%np == 1) then
        value = sum(work)
    else
        call gather_zonal_array(zonal_circle, work, allvalue)
        if (zonal_circle%id == 0) value = sum(allvalue)
        call MPI_BCAST(value, 1, MPI_DOUBLE, 0, zonal_circle%comm, ierr)
    end if
#else
    call MPI_ALLREDUCE(sum(work), value, 1, MPI_DOUBLE, MPI_SUM, zonal_circle%comm, ierr)
#endif

  end subroutine zonal_sum_0d_r8
    
  subroutine zonal_sum_1d_r8(zonal_circle, work, value)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: work(:,:)
    real(8), intent(out) :: value(:)

#ifdef ENSURE_ORDER
    real(8) allvalue(global_mesh%full_nlon,size(value))
#endif
    integer ierr

#ifdef ENSURE_ORDER
    if (zonal_circle%np == 1) then
        value = sum(work, dim=1)
    else
        call gather_zonal_array(zonal_circle, work, allvalue)
        if (zonal_circle%id == 0) value = sum(allvalue, dim=1)
        call MPI_BCAST(value, size(value), MPI_DOUBLE, 0, zonal_circle%comm, ierr)
    end if
#else
    call MPI_ALLREDUCE(sum(work, dim=1), value, size(value), MPI_DOUBLE, MPI_SUM, zonal_circle%comm, ierr)
#endif

  end subroutine zonal_sum_1d_r8

  subroutine zonal_sum_0d_r4_member(zonal_circle, work, value)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(4), intent(in) :: work(:,:)
    real(4), intent(out) :: value(:)

#ifdef ENSURE_ORDER
    real(4) allvalue(member_num , global_mesh%full_nlon)
#endif
    integer ierr

#ifdef ENSURE_ORDER
    if (zonal_circle%np == 1) then
        value(:) = sum(work,2)
    else
        call gather_zonal_array_member(zonal_circle, work, allvalue)
        if (zonal_circle%id == 0) value(:) = sum(allvalue,2)
        call MPI_BCAST(value, size(value), MPI_REAL, 0, zonal_circle%comm, ierr)
    end if
#else
    call MPI_ALLREDUCE(sum(work,2), value, size(value), MPI_REAL, MPI_SUM, zonal_circle%comm, ierr)
#endif

  end subroutine zonal_sum_0d_r4_member

  subroutine zonal_sum_0d_r8_member(zonal_circle, work, value)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: work(:,:)
    real(8), intent(out) :: value(:)

#ifdef ENSURE_ORDER
    real(8) allvalue(member_num , global_mesh%full_nlon)
#endif
    integer ierr

#ifdef ENSURE_ORDER
    if (zonal_circle%np == 1) then
        value(:) = sum(work,2)
    else
        call gather_zonal_array_member(zonal_circle, work, allvalue)
        if (zonal_circle%id == 0) value(:) = sum(allvalue,2)
        call MPI_BCAST(value, size(value), MPI_DOUBLE, 0, zonal_circle%comm, ierr)
    end if
#else
    call MPI_ALLREDUCE(sum(work,2), value, size(value), MPI_DOUBLE, MPI_SUM, zonal_circle%comm, ierr)
#endif

  end subroutine zonal_sum_0d_r8_member

  subroutine zonal_sum_1d_r4_member(zonal_circle, work, value)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(4), intent(in) :: work(:,:,:)
    real(4), intent(out) :: value(:,:)

#ifdef ENSURE_ORDER
    real(4) allvalue(size(value,1),global_mesh%full_nlon,size(value,2))
#endif
    integer ierr

#ifdef ENSURE_ORDER
    if (zonal_circle%np == 1) then
        value = sum(work, dim=2)
    else
        call gather_zonal_array_member(zonal_circle, work, allvalue)
        if (zonal_circle%id == 0) value = sum(allvalue, dim=2)
        call MPI_BCAST(value, size(value), MPI_REAL, 0, zonal_circle%comm, ierr)
    end if
#else
    call MPI_ALLREDUCE(sum(work, dim=2), value, size(value), MPI_REAL, MPI_SUM, zonal_circle%comm, ierr)
#endif

  end subroutine zonal_sum_1d_r4_member

  subroutine zonal_sum_1d_r8_member(zonal_circle, work, value)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: work(:,:,:)
    real(8), intent(out) :: value(:,:)

#ifdef ENSURE_ORDER
    real(8) allvalue(size(value,1),global_mesh%full_nlon,size(value,2))
#endif
    integer ierr

#ifdef ENSURE_ORDER
    if (zonal_circle%np == 1) then
        value = sum(work, dim=2)
    else
        call gather_zonal_array_member(zonal_circle, work, allvalue)
        if (zonal_circle%id == 0) value = sum(allvalue, dim=2)
        call MPI_BCAST(value, size(value), MPI_DOUBLE, 0, zonal_circle%comm, ierr)
    end if
#else
    call MPI_ALLREDUCE(sum(work, dim=2), value, size(value), MPI_DOUBLE, MPI_SUM, zonal_circle%comm, ierr)
#endif

  end subroutine zonal_sum_1d_r8_member

  subroutine zonal_sum_old_0d_r8(comm, value)

    integer, intent(in) :: comm
    real(8), intent(inout) :: value

    integer ierr
    real(8) res

    call MPI_ALLREDUCE(value, res, 1, MPI_DOUBLE, MPI_SUM, comm, ierr)
    value = res

  end subroutine zonal_sum_old_0d_r8

  subroutine zonal_sum_old_1d_r8(comm, value)

    integer, intent(in) :: comm
    real(8), intent(inout) :: value(:)

    integer ierr
    real(8) res(size(value))

    call MPI_ALLREDUCE(value, res, size(value), MPI_DOUBLE, MPI_SUM, comm, ierr)
    value = res

  end subroutine zonal_sum_old_1d_r8

  subroutine zonal_sum_old_2d_r8(comm, value)

    integer, intent(in) :: comm
    real(8), intent(inout) :: value(:,:)

    integer ierr
    real(8) res(size(value,1) , size(value,2))

    call MPI_ALLREDUCE(value, res, size(value), MPI_DOUBLE, MPI_SUM, comm, ierr)
    value = res

    end subroutine zonal_sum_old_2d_r8


  subroutine zonal_sum_ensure_order_1d_r8(comm, value , sum)

    integer, intent(in) :: comm
    real(8), intent(in) :: value(:)
    real(8), intent(out):: sum 

    integer status(MPI_STATUS_SIZE), ierr
    integer numproc , myid
    integer i 
    real(8), allocatable :: allvalue(:)  


    call MPI_COMM_SIZE(comm,numproc,ierr)
    call MPI_COMM_RANK(comm , myid , ierr)
    
    sum = 0.0

    if (numproc == 1) then
        do i = 1 , size(value)
        sum = sum + value(i)
        end do
    else
    
        if (proc%cart_coords(1) == 0) then  !root proc
        allocate(allvalue(global_mesh%full_nlon))
        allvalue(1:size(value)) = value(:)
        do i = 1 , numproc - 1
            call MPI_RECV(allvalue( i * size(value) + 1 : (i+1)  * size(value) ) , size(value) , MPI_DOUBLE , i  , 0 , comm , status,ierr)
        end do
        do i = 1 , global_mesh%full_nlon
            sum = sum + allvalue(i)
        end do
        do i = 1 , numproc - 1
            call MPI_SEND(sum , 1 , MPI_DOUBLE , i , 0 , comm , status,ierr)
        end do
        deallocate(allvalue)
        else !other proc
        call MPI_SEND(value(:) , size(value) , MPI_DOUBLE , 0 , 0 , comm , ierr)
        call MPI_RECV(sum , 1 , MPI_DOUBLE , 0 , 0 , comm , status,ierr)
        end if  

    end if

  end subroutine zonal_sum_ensure_order_1d_r8

    subroutine zonal_sum_ensure_order_2d_r8(comm, value , sum , pos_name)

    integer, intent(in) :: comm
    real(8), intent(in) :: value(:,:)
    real(8), intent(out):: sum(:) 
    character(*), optional , intent(in) :: pos_name

    integer status(MPI_STATUS_SIZE), ierr
    integer numproc , myid
    integer i , nm , nx
    real(8), allocatable :: allvalue(:,:)  


    call MPI_COMM_SIZE(comm,numproc,ierr)
    call MPI_COMM_RANK(comm,myid,ierr)
    sum = 0.0
    nm  = size(value,1)
    nx  = size(value,2)


    if (numproc == 1) then
        do i = 1 , nx
        sum = sum + value(:,i)
        end do
    else

        
        if (proc%cart_coords(1) == 0) then  !root proc
        allocate(allvalue(member_num , global_mesh%full_nlon))
        allvalue(:,1:nx) = value
        do i = 1 , numproc - 1
            call MPI_RECV(allvalue( : , i * nx + 1 : (i+1)  * nx ) , nx * nm , MPI_DOUBLE , i  , 0 , comm , status,ierr)
        end do
        do i = 1 , global_mesh%full_nlon
            sum = sum + allvalue(:,i)
        end do
        do i = 1 , numproc - 1
            call MPI_SEND(sum , nm , MPI_DOUBLE , i , 0 , comm , status,ierr)
        end do
        deallocate(allvalue)
        
        else !other proc
        call MPI_SEND(value(:,:) , nx * nm , MPI_DOUBLE , 0 , 0 , comm , ierr)
        call MPI_RECV(sum , nm , MPI_DOUBLE , 0 , 0 , comm , status,ierr)
        end if  

    end if

  end subroutine zonal_sum_ensure_order_2d_r8

  subroutine gather_zonal_array_1d_r8(zonal_circle, local_array, array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: local_array(:)
    real(8), intent(out) :: array(:)

    integer ierr, i, status

    if (zonal_circle%id == 0) then
      array(1:size(local_array)) = local_array
      do i = 2, zonal_circle%np
        call MPI_RECV(array, 1, zonal_circle%recv_type_r8(i,0), i - 1, 30, zonal_circle%comm, status, ierr)
      end do
    else
      call MPI_SEND(local_array, size(local_array), MPI_DOUBLE, 0, 30, zonal_circle%comm, ierr)
    end if

  end subroutine gather_zonal_array_1d_r8

  subroutine gather_zonal_array_2d_r8(zonal_circle, local_array, array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: local_array(:,:)
    real(8), intent(inout) :: array(:,:)

    integer ierr, i, k, status

    if (zonal_circle%id == 0) then
      k = merge(1, 2, size(local_array, 2) == global_mesh%full_nlev)
      array(1:size(local_array, 1),:) = local_array
      do i = 2, zonal_circle%np
        call MPI_RECV(array, 1, zonal_circle%recv_type_r8(i,k), i - 1, 31, zonal_circle%comm, status, ierr)
      end do
    else
      call MPI_SEND(local_array, size(local_array), MPI_DOUBLE, 0, 31, zonal_circle%comm, ierr)
    end if

  end subroutine gather_zonal_array_2d_r8

  subroutine scatter_zonal_array_1d_r8(zonal_circle, array, local_array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: array(:)
    real(8), intent(out) :: local_array(:)

    integer ierr, i, status

    if (zonal_circle%id == 0) then
      local_array = array(1:size(local_array))
      do i = 2, zonal_circle%np
        call MPI_SEND(array, 1, zonal_circle%recv_type_r8(i,0), i - 1, 32, zonal_circle%comm, ierr)
      end do
    else
      call MPI_RECV(local_array, size(local_array), MPI_DOUBLE, 0, 32, zonal_circle%comm, status, ierr)
    end if

  end subroutine scatter_zonal_array_1d_r8

  subroutine scatter_zonal_array_2d_r8(zonal_circle, array, local_array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: array(:,:)
    real(8), intent(out) :: local_array(:,:)

    integer ierr, i, k, status

    if (zonal_circle%id == 0) then
      k = merge(1, 2, size(local_array, 2) == global_mesh%full_nlev)
      local_array = array(1:size(local_array, 1),:)
      do i = 2, zonal_circle%np
        call MPI_SEND(array, 1, zonal_circle%recv_type_r8(i,k), i - 1, 33, zonal_circle%comm, ierr)
      end do
    else
      call MPI_RECV(local_array, size(local_array), MPI_DOUBLE, 0, 33, zonal_circle%comm, status, ierr)
    end if

  end subroutine scatter_zonal_array_2d_r8

  subroutine gather_zonal_array_1d_r4_member(zonal_circle, local_array, array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(4), intent(in) :: local_array(:,:)
    real(4), intent(out) :: array(:,:)

    integer ierr, i, status(MPI_STATUS_SIZE)

    if (zonal_circle%id == 0) then
      array(:,1:size(local_array,2)) = local_array
      do i = 2, zonal_circle%np
        call MPI_RECV(array, 1, zonal_circle%recv_type_r4(i,0), i - 1, 30, zonal_circle%comm, status, ierr)
      end do
    else
      call MPI_SEND(local_array, size(local_array) , MPI_REAL, 0, 30, zonal_circle%comm, ierr)
    end if

  end subroutine gather_zonal_array_1d_r4_member

  subroutine gather_zonal_array_1d_r8_member(zonal_circle, local_array, array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: local_array(:,:)
    real(8), intent(out) :: array(:,:)

    integer ierr, i, status(MPI_STATUS_SIZE)

    if (zonal_circle%id == 0) then
      array(:,1:size(local_array,2)) = local_array
      do i = 2, zonal_circle%np
        call MPI_RECV(array, 1, zonal_circle%recv_type_r8(i,0), i - 1, 30, zonal_circle%comm, status, ierr)
      end do
    else
      call MPI_SEND(local_array, size(local_array) , MPI_DOUBLE, 0, 30, zonal_circle%comm, ierr)
    end if

  end subroutine gather_zonal_array_1d_r8_member

  subroutine gather_zonal_array_2d_r4_member(zonal_circle, local_array, array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(4), intent(in) :: local_array(:,:,:)
    real(4), intent(inout) :: array(:,:,:)

    integer ierr, i, k, status(MPI_STATUS_SIZE)

    if (zonal_circle%id == 0) then
      k = merge(1, 2, size(local_array, 2) == global_mesh%full_nlev)
      array(:,1:size(local_array, 2),:) = local_array
      do i = 2, zonal_circle%np
        call MPI_RECV(array, 1, zonal_circle%recv_type_r4(i,k), i - 1, 31, zonal_circle%comm, status, ierr)
      end do
    else
      call MPI_SEND(local_array, size(local_array), MPI_REAL, 0, 31, zonal_circle%comm, ierr)
    end if

  end subroutine gather_zonal_array_2d_r4_member

  subroutine gather_zonal_array_2d_r8_member(zonal_circle, local_array, array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: local_array(:,:,:)
    real(8), intent(inout) :: array(:,:,:)

    integer ierr, i, k, status(MPI_STATUS_SIZE)

    if (zonal_circle%id == 0) then
      k = merge(1, 2, size(local_array, 2) == global_mesh%full_nlev)
      array(:,1:size(local_array, 2),:) = local_array
      do i = 2, zonal_circle%np
        call MPI_RECV(array, 1, zonal_circle%recv_type_r8(i,k), i - 1, 31, zonal_circle%comm, status, ierr)
      end do
    else
      call MPI_SEND(local_array, size(local_array), MPI_DOUBLE, 0, 31, zonal_circle%comm, ierr)
    end if

  end subroutine gather_zonal_array_2d_r8_member

  subroutine scatter_zonal_array_1d_r4_member(zonal_circle, array, local_array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(4), intent(in) :: array(:,:)
    real(4), intent(out) :: local_array(:,:)

    integer ierr, i, status(MPI_STATUS_SIZE)

    if (zonal_circle%id == 0) then
      local_array = array(:,1:size(local_array))
      do i = 2, zonal_circle%np
        call MPI_SEND(array, 1, zonal_circle%recv_type_r4(i,0), i - 1, 32, zonal_circle%comm, ierr)
      end do
    else
      call MPI_RECV(local_array, size(local_array), MPI_REAL, 0, 32, zonal_circle%comm, status, ierr)
    end if

  end subroutine scatter_zonal_array_1d_r4_member

  subroutine scatter_zonal_array_1d_r8_member(zonal_circle, array, local_array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: array(:,:)
    real(8), intent(out) :: local_array(:,:)

    integer ierr, i, status(MPI_STATUS_SIZE)

    if (zonal_circle%id == 0) then
      local_array = array(:,1:size(local_array))
      do i = 2, zonal_circle%np
        call MPI_SEND(array, 1, zonal_circle%recv_type_r8(i,0), i - 1, 32, zonal_circle%comm, ierr)
      end do
    else
      call MPI_RECV(local_array, size(local_array), MPI_DOUBLE, 0, 32, zonal_circle%comm, status, ierr)
    end if

  end subroutine scatter_zonal_array_1d_r8_member

  subroutine scatter_zonal_array_2d_r4_member(zonal_circle, array, local_array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(4), intent(in) :: array(:,:,:)
    real(4), intent(out) :: local_array(:,:,:)

    integer ierr, i, k, status(MPI_STATUS_SIZE)

    if (zonal_circle%id == 0) then
      k = merge(1, 2, size(local_array, 2) == global_mesh%full_nlev)
      local_array = array(:,1:size(local_array, 1),:)
      do i = 2, zonal_circle%np
        call MPI_SEND(array, 1, zonal_circle%recv_type_r4(i,k), i - 1, 33, zonal_circle%comm, ierr)
      end do
    else
      call MPI_RECV(local_array, size(local_array), MPI_REAL, 0, 33, zonal_circle%comm, status, ierr)
    end if

  end subroutine scatter_zonal_array_2d_r4_member

  subroutine scatter_zonal_array_2d_r8_member(zonal_circle, array, local_array)

    type(zonal_circle_type), intent(in) :: zonal_circle
    real(8), intent(in) :: array(:,:,:)
    real(8), intent(out) :: local_array(:,:,:)

    integer ierr, i, k, status(MPI_STATUS_SIZE)

    if (zonal_circle%id == 0) then
      k = merge(1, 2, size(local_array, 2) == global_mesh%full_nlev)
      local_array = array(:,1:size(local_array, 1),:)
      do i = 2, zonal_circle%np
        call MPI_SEND(array, 1, zonal_circle%recv_type_r8(i,k), i - 1, 33, zonal_circle%comm, ierr)
      end do
    else
      call MPI_RECV(local_array, size(local_array), MPI_DOUBLE, 0, 33, zonal_circle%comm, status, ierr)
    end if

  end subroutine scatter_zonal_array_2d_r8_member

end module parallel_zonal_mod