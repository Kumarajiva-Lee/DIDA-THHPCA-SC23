#include "../../../mp_config/mct_config.inc"

module cnn_coupler
    use iso_c_binding

    use m_MCTWorld,only: MCTWorld_init => init
    use m_MCTWorld,only: MCTWorld_clean => clean
    use m_GlobalSegMap,only: GlobalSegMap
    use m_GlobalSegMap,only: GlobalSegMap_init => init
    use m_GlobalSegMap,only: GlobalSegMap_lsize => lsize
    use m_GlobalSegMap,only: GlobalSegMap_clean => clean
    use m_GlobalSegMap,only: GlobalSegMap_Ordpnts => OrderedPoints
    use m_Rearranger,only: Rearranger
    use m_Rearranger,only: Rearranger_init => init
    use m_Rearranger,only: Rearranger_clean => clean
    use m_Rearranger,only: Rearranger_rearrange => rearrange
    use m_AttrVect,only    : AttrVect
    use m_AttrVect,only    : AttrVect_init => init
    use m_AttrVect,only    : AttrVect_clean => clean
    use m_AttrVect,only    : AttrVect_indxR => indexRA
    use m_AttrVect,only    : AttrVect_importRAttr => importRAttr
    use m_AttrVect,only    : AttrVect_exportRAttr => exportRAttr
    use m_Router,only: Router
    use m_Router,only: Router_init => init
    use m_Router,only: Router_clean => clean
    use m_Transfer,only : MCT_Send => send
    use m_Transfer,only : MCT_Recv => recv
  
    use mpi
    use da_parallel
    use da_coupler
    use coupler_config
    use pro_info
    use redis_mod
    use string
    use splitchar
    use var_info_mod
    use flogger

    implicit none

    integer :: rk, sz
    integer :: lon_st, lon_ed, lat_st, lat_ed, mem_st, mem_ed, lev_max
    integer :: lon_sec, lon_res, lat_sec, lat_res, mem_sec, mem_res
    type(GlobalSegMap)   :: CNN_Map
    type(AttrVect) :: CNN_AV
    type(Router) :: R_CNN2DA
    integer :: cnn_lsize, n_mem
    integer :: nlon, nlat

  interface
    subroutine cnn_c_init(dir) bind(c, name='ModuleCnnInit')
      use iso_c_binding
      character(c_char) :: dir(*)
    end subroutine

    subroutine cnn_c_predict(my_ens, my_lev, my_lat, my_lon, input_val, output_val) &
      bind (c, name='ModuleCnnPredict')
      use iso_c_binding
      integer(c_int), intent(in) :: my_ens, my_lev, my_lat, my_lon
      type(c_ptr), value :: input_val, output_val
    end subroutine

    subroutine cnn_c_destroy() bind(c, name='ModuleCnnDestroy')
      use iso_c_binding
    end subroutine
    
  end interface

contains

subroutine cnn_init(MPI_CNN_GROUP)
  integer, intent(in) :: MPI_CNN_GROUP
  integer :: ierrs
  integer :: comm_cart
  integer :: dims(3), coords(3)
  logical :: periods(3)
  integer :: lrank
  integer, allocatable,dimension(:) :: start,length
  integer, allocatable,dimension(:) :: start_t,length_t  
  character(len = 20), dimension(10) :: var_name
  integer :: n_var, n_max, obs_num_2d, obs_num_3d
  character(len = 20), allocatable, dimension(:) :: var_name_ordered
  integer :: i, j, k, ij
  integer :: cnn_gsize

  call MPI_Comm_rank(MPI_CNN_GROUP, rk, ierrs)
  call MPI_Comm_size(MPI_CNN_GROUP, sz, ierrs)

  dims(1) = 0
  dims(2) = 0
  dims(3) = 0

  call MPI_Dims_create(sz, 3, dims,ierrs)
  call MPI_Cart_create(MPI_CNN_GROUP, 3, dims, periods, 0, comm_cart, ierrs)
  call MPI_Comm_rank(comm_cart, lrank, ierrs)
  call MPI_Cart_coords(comm_cart, lrank, 3, coords, ierrs)

  lon_sec = num_lon / dims(1)
  lon_res = mod(num_lon, dims(1))
  lat_sec = num_lat / dims(2)
  lat_res = mod(num_lat, dims(2))
  mem_sec = (cnn_ensemble/atm_ensemble_total) / dims(3)
  mem_res = mod((cnn_ensemble/atm_ensemble_total), dims(3))

  if(coords(1) < lon_res) then
      lon_st = coords(1) * (lon_sec + 1) + 1
      lon_ed = lon_st + lon_sec
  else
      lon_st = coords(1) * lon_sec + lon_res + 1
      lon_ed = lon_st + lon_sec - 1
  end if

  if(coords(2) < lat_res) then
      lat_st = coords(2) * (lat_sec + 1) + 1
      lat_ed = lat_st + lat_sec 
  else
      lat_st = coords(2) * lat_sec + lat_res + 1
      lat_ed = lat_st + lat_sec - 1
  end if

  if(coords(3) < mem_res) then
      mem_st = coords(3) * (mem_sec + 1) + 1
      mem_ed = mem_st + mem_sec
  else
      mem_st = coords(3) * mem_sec + mem_res + 1
      mem_ed = mem_st + mem_sec - 1
  end if
  mem_st = (mem_st - 1) * atm_ensemble_total + 1
  mem_ed = mem_ed * atm_ensemble_total

  nlon = lon_ed - lon_st + 1
  nlat = lat_ed - lat_st + 1
  n_mem = mem_ed - mem_st + 1

!!cnn_map [ens,lev,lat,lon] with halo
  call stringsplit(da_var_name,',',var_name,n_var)
  allocate(var_name_ordered(n_var))
  call var_info(var_name, n_var, var_name_ordered, obs_num_2d, obs_num_3d)
  n_max = 0
  do i = 1, obs_num_2d
    n_max = n_max + 1
  end do
  do i = obs_num_2d + 1, n_var
    n_max = n_max + num_lev
  end do
  lev_max = n_max
  
  call log_notice("cnn init begin "//to_str(mem_st)//" "//to_str(mem_ed))
  call log_notice("cnn range "//to_str(lon_st)//" "//to_str(lon_ed)//" "//to_str(lat_st)//" "//to_str(lat_ed))
  call MCTWorld_init(ncomps, MPI_COMM_WORLD, MPI_CNN_GROUP, CNN_Id)

  allocate(start(1:((nlon+2*cnn_halo)*(nlat+2*cnn_halo)*n_max)))
  allocate(length(1:((nlon+2*cnn_halo)*(nlat+2*cnn_halo)*n_max)))
  ij = 0
  do i = lon_st-cnn_halo, lon_ed+cnn_halo
    do j = lat_st-cnn_halo, lat_ed+cnn_halo
      if (j >= 1 .and. j <= num_lat) then
        do k = 1, n_max
          ij = ij + 1
          if (i < 1) then
            start(ij) = (i+num_lon-1)*num_lat*n_max*cnn_ensemble + (j-1)*n_max*cnn_ensemble + (k-1)*cnn_ensemble + mem_st
          else if (i > num_lon) then
            start(ij) = (i-num_lon-1)*num_lat*n_max*cnn_ensemble + (j-1)*n_max*cnn_ensemble + (k-1)*cnn_ensemble + mem_st
          else
            start(ij) = (i-1)*num_lat*n_max*cnn_ensemble + (j-1)*n_max*cnn_ensemble + (k-1)*cnn_ensemble + mem_st
          end if
          length(ij) = n_mem
        end do
      end if
    end do
  end do

  cnn_gsize = num_lon*num_lat*n_max*cnn_ensemble

  allocate(start_t(1:ij))
  allocate(length_t(1:ij))
  start_t(1:ij) = start(1:ij)
  length_t(1:ij) = length(1:ij)
  call GlobalSegMap_init(CNN_Map, start_t, length_t, 0, MPI_CNN_GROUP, CNN_Id, gsize=cnn_gsize)
  cnn_lsize = GlobalSegMap_lsize(CNN_Map, MPI_CNN_GROUP)
  call AttrVect_init(CNN_AV,rList="val",lsize=cnn_lsize)

  call Router_init(DA_Id,CNN_Map,MPI_CNN_GROUP,R_CNN2DA)

  call log_notice("cnn_mct_init_end")
  !调若干次， [ens_st, ens_ed] * n_max 次
  !cnn_init_C_kernel
  !do i = 1, n_max
    call cnn_c_init(trim(cnn_model_path))
  !end do

  call log_notice("cnn_init_end")

  deallocate(start)
  deallocate(length)
  deallocate(start_t)
  deallocate(length_t)
    
end subroutine cnn_init
  
subroutine cnn_run(MPI_CNN_GROUP)
  use datetime
  use coupler_config, only:da_start_time_array=>da_start_time, end_time_array=>end_time, &
                           da_in_seconds

  integer, intent(in) :: MPI_CNN_GROUP
  integer :: da_time
  type(datetime_type) da_start_time
  type(datetime_type) end_time
  type(timedelta_type) delta_time
  integer :: ts, ix, iy, iz, mem_i, niz, n_max
  axb_type, dimension(:), pointer :: recvdata
  integer :: lxst, lxed, lyst, lyed
  integer :: m_st, m_ed
  integer :: debug_int
  real(4), allocatable, dimension(:, :, :, :) :: data_in
  real(4), allocatable, dimension(:, :, :, :) :: data_out
  real(4), allocatable, dimension(:, :, :) :: data_tmp


  da_start_time = create_datetime(year=da_start_time_array(1),  &
                                  month=da_start_time_array(2), &
                                  day=da_start_time_array(3),   &
                                  hour=da_start_time_array(4),  &
                                  minute=da_start_time_array(5))

  end_time = create_datetime(year=end_time_array(1),  &
                             month=end_time_array(2), &
                             day=end_time_array(3),   &
                             hour=end_time_array(4),  &
                             minute=end_time_array(5))

  delta_time = end_time - da_start_time
  !call MCT_Recv(DA_AV, R_DA2ATM)
  !print *, "Before receiving"

  da_time = ceiling((delta_time%days*86400+delta_time%hours*3600+delta_time%minutes*60+delta_time%seconds)/da_in_seconds)

  allocate(recvdata(cnn_lsize))

  lxst = lon_st - cnn_halo
  lxed = lon_ed + cnn_halo
  lyst = lat_st - cnn_halo
  lyed = lat_ed + cnn_halo
  if (lat_st == 1) then
    lyst = lat_st
  end if
  if (lat_ed == num_lat) then
    lyed = lat_ed
  end if

  n_max = lev_max

  allocate(data_in(lon_st-cnn_halo:lon_ed+cnn_halo,lat_st-cnn_halo:lat_ed+cnn_halo,n_max,atm_ensemble_total))
  allocate(data_out(lon_st-cnn_halo:lon_ed+cnn_halo,lat_st-cnn_halo:lat_ed+cnn_halo,n_max,n_mem))
  allocate(data_tmp(lon_st-cnn_halo:lon_ed+cnn_halo,lat_st-cnn_halo:lat_ed+cnn_halo,atm_ensemble_total))

  call log_notice("cnn run begin "//to_str(lxst)//" "//to_str(lxed)//" "//to_str(lyst)//" "//to_str(lyed)//" "//to_str(n_max))


  do ts = 1, da_time
    
    call MCT_Recv(CNN_AV,R_CNN2DA)
    call AttrVect_exportRAttr(CNN_AV, "val", recvdata)
    
    !open(51, FILE='test_cnn_'//to_str(rk)//'.txt')

    ! [ps,u,v,pt] to [ps,pt,u,v]
    debug_int = 0
    do ix = lxst, lxed
      do iy = lyst, lyed
        do iz = 1, n_max
          if (iz == 1) then
            niz = iz
          else if (iz >= 2 .and. iz <= 33) then
            niz = iz + 32
          else if (iz >= 34 .and. iz <= 65) then
            niz = iz + 32
          else
            niz = iz - 64
          end if
          data_in(ix,iy,niz,1:atm_ensemble_total) = recvdata(debug_int+1:debug_int+atm_ensemble_total)
          debug_int = debug_int + n_mem
          do mem_i = 1, atm_ensemble_total
          ! write (51, *), ix, iy, iz, mem_i, data_in(ix,iy,niz,mem_i)
          end do
        end do
      end do
    end do
    !close(51)

    !!正则化

    if (lat_st == 1) then
      do ix = 1, cnn_halo
        data_in(:,lat_st-ix,:,:) = data_in(:,lat_st,:,:)
      end do
    end if

    if (lat_ed == num_lat) then
      do ix = 1, cnn_halo
        data_in(:,lat_ed+ix,:,:) = data_in(:,lat_ed,:,:)
      end do
    end if

    call log_notice("before cnn_predict")
    do ix = 1, (n_mem/atm_ensemble_total)
      do iy = 1, n_max
        call cnn_predict(data_in, data_tmp)
        m_st = (ix - 1) * atm_ensemble_total + 1
        m_ed = m_st + atm_ensemble_total - 1
        call cnn_predict(data_in, data_tmp)
        !do iz = 1, atm_ensemble_total
          data_out(:,:,iy,m_st:m_ed) = data_tmp(:,:,1:atm_ensemble_total)
        !end do
      end do 
    end do

    do mem_i = 1, n_mem
      data_out(:,:,:,mem_i) = data_in(:,:,:,mod(mem_i-1, atm_ensemble_total)+1)
    end do
    call log_notice("after cnn_predict")
    !call cnn_C_kernel
    ! 97 * (64/2)
    !call cnn_c_predict(atm_ensemble_total, n_max, nlat+2*cnn_halo, nlon+2*cnn_halo, c_loc(data_in), c_loc(data_out))
    ! end do 
    ! [lon,lat,n_max,atm] -> [lon,lat,atm] -> [lon,lat,n_max,n_mem]
    ! [mem_st,mem_ed]

    ! 调若干次，数据拼接

    !data_out

    !!反正则化

    ![ps,pt,u,v] to [ps,u,v,pt]
    debug_int = 0
    do ix = lxst, lxed
      do iy = lyst, lyed
        do iz = 1, n_max
          if (iz == 1) then
            niz = iz
          else if (iz >= 2 .and. iz <= 33) then
            niz = iz + 32
          else if (iz >= 34 .and. iz <= 65) then
            niz = iz + 32
          else
            niz = iz - 64
          end if
          recvdata(debug_int+1:debug_int+n_mem) = data_out(ix,iy,niz,1:n_mem)
          debug_int = debug_int + n_mem
        end do
      end do
    end do

    call AttrVect_importRAttr(CNN_AV, "val", recvdata)
    call MCT_Send(CNN_AV,R_CNN2DA)
  end do
  deallocate(recvdata)

end subroutine cnn_run

subroutine cnn_predict(input_buf, output_buf)
  
  type(*), target :: input_buf(*), output_buf(*)
  call cnn_c_predict(atm_ensemble_total, n_max, nlat+2*cnn_halo, nlon+2*cnn_halo, c_loc(input_buf), c_loc(output_buf))

end subroutine


subroutine cnn_final()
  call Router_clean(R_CNN2DA)
  call AttrVect_clean(CNN_AV)
  call GlobalSegMap_clean(CNN_Map)
  call MCTWorld_clean()
  ! CNN_destroy_C_kernel
  call cnn_c_destroy()
end subroutine cnn_final

end module cnn_coupler
