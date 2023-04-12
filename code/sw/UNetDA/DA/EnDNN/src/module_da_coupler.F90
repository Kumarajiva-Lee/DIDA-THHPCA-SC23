#include "../../../mp_config/mct_config.inc"

#define INDEX(i, j, k, N, L) ((i-1)*(N)*(L)+(j-1)*(L)+(k-1))

module da_coupler
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
  use letkf_mod
  use coupler_config
  use pro_info
  use redis_mod
  use da_output_mod
  use da_namelist_mod, only:obs_stat_interval_lon, obs_stat_interval_lat, &
  obs_stat_interval_vertical, da_halo
  use string
  use splitchar
  use var_info_mod
  use flogger
  use fiona
  !use process_mod

  implicit none

  type(GlobalSegMap)   :: DA_GSMap, DA_N_GSMap, DA_CNNMap, DA_CNN_OV          ! MCT defined type
  type(AttrVect) :: DA_AV, DA_AV_N, DA_Cal, DA_Cal_N, DA_AV_CNN, DA_AV_CNN_OV                      ! MCT defined type
  type(Rearranger) :: DA_Re_To_Nov, DA_Re_To_Ov
  type(Router) :: R_DA2CNN
  type(Router),allocatable,dimension(:) :: R_DA2ATM             ! MCT defined type
  !type(Router)          :: R_DA2ATM                      ! MCT defined type

  integer, allocatable,dimension(:) :: start,length
  integer, allocatable,dimension(:) :: start_t,length_t
  integer, allocatable,dimension(:) :: start_n,length_n
  integer, allocatable,dimension(:) :: start_c,length_c
  integer, allocatable,dimension(:) :: start_nc,length_nc
  axb_type, dimension(:), pointer :: avdata, area_count, area_count_no_overlap
  integer :: avsize, av_nsize, size_nooverlap, cnn_size, letkf_size, da2cnnsize
  !type(c_ptr) :: da_rc
  integer :: myid

  !cnn 
  integer :: cnn_lonst, cnn_loned, cnn_latst, cnn_lated, cnn_memst, cnn_memed
  integer :: cnn_nlon, cnn_nlat, cnn_nmem, cnn_gsize, cnn_lsize, cnn_nmax
  integer :: cnn_gid, cnn_group_num, cnn_levst, cnn_leved, cnn_nlev
  integer, allocatable,dimension(:) :: cnn_start, cnn_length
  type(GlobalSegMap) :: CNN_Map, DA2CNN_MAP
  type(Rearranger) :: CNN_Re_from_DA, CNN_Re_to_DA
  type(AttrVect) :: CNN_AV

  interface
    subroutine cnn_c_init(dir, index_st, index_len, my_ens, my_lev, my_lat, my_lon) bind(c, name='ModuleCnnInit')
      use iso_c_binding
      character(c_char) :: dir(*)
      integer(c_int), intent(in) :: index_st, index_len, my_ens, my_lev, my_lat, my_lon
    end subroutine

    subroutine cnn_c_predict(my_ens, my_lev, my_lat, my_lon, my_index, input_val, output_val, rk) &
      bind (c, name='ModuleCnnPredict')
      use iso_c_binding
      integer(c_int), intent(in) :: my_ens, my_lev, my_lat, my_lon, my_index
      type(c_ptr), value :: input_val, output_val
      integer(c_int), intent(in) :: rk
    end subroutine

    subroutine cnn_c_destroy() bind(c, name='ModuleCnnDestroy')
      use iso_c_binding
    end subroutine
    
  end interface


  contains

subroutine init_da_coupler(MPI_DA_GROUP)

  implicit none
  integer, intent(in) :: MPI_DA_GROUP
  integer :: i, j, k, mem_num, mem_num_local, ij
  integer :: ierr
  integer :: halo2
  integer :: gnum, cnn_gnum
  integer :: ylength_max
  integer :: groupid
  character(len = 20), dimension(10) :: var_name
  integer :: n_var, n_max, obs_num_2d, obs_num_3d
  character(len = 20), allocatable, dimension(:) :: var_name_ordered
  integer :: nlon, nlat

  integer :: cnn_comm_cart, sz, cnn_rk
  integer :: cnn_dims(3), cnn_coords(3)
  logical :: periods(3)
  integer :: lon_sec, lon_res, lat_sec, lat_res, mem_sec, mem_res, lev_sec, lev_res

  integer :: model_ix_st, model_num

  call log_notice_root("da_couper init")

  call MPI_Comm_rank(MPI_DA_GROUP, myid, ierr)

  halo2 = da_halo * 2

  !call log_notice("da MCT_init "//to_str(ncomps))
  call MCTWorld_init(ncomps, MPI_COMM_WORLD, MPI_DA_GROUP, DA_Id)
  !call log_notice("out of MCT_init "//to_str(ncomps))

  ylength_max = yed-yst+1+halo2
  allocate(start_t(num_lev*ylength_max*3))
  allocate(length_t(num_lev*ylength_max*3))

#if defined ATM_COUPLING
  mem_num = atm_ensemble_total
  mem_num_local = atm_ensemble_total / atm_ensemble_group
#endif

  mem_num = cnn_ensemble
  mem_num_local = cnn_ensemble

  gnum = num_lev*num_lat*num_lon*mem_num_local

  nlon = xed - xst + 1
  nlat = yed - yst + 1

  ij = 0
  if (xst - da_halo >= 1 .and. xed + da_halo <= num_lon) then
    do j = 1, num_lev
      do i = yst-da_halo, yed+da_halo
        if (i >= 1 .and. i <= num_lat) then
          ij = ij + 1
          length_t(ij) = (xed - xst + 1 + halo2) * mem_num_local
          start_t(ij) = INDEX(j, i, xst-da_halo, num_lat, num_lon) * mem_num_local + 1
        end if
      end do
    end do
  end if
  if (xst - da_halo < 1 .and. xed + da_halo <= num_lon) then
    do j = 1, num_lev
      do i = yst-da_halo, yed+da_halo
        if (i >= 1 .and. i <= num_lat) then
          ij = ij + 1
          length_t(ij) = da_halo * mem_num_local
          start_t(ij) = INDEX(j, i, num_lon-da_halo+1, num_lat, num_lon) * mem_num_local + 1
          ij = ij + 1
          length_t(ij) = (xed - xst + 1 + da_halo) * mem_num_local
          start_t(ij) = INDEX(j, i, xst, num_lat, num_lon) * mem_num_local + 1
        end if
      end do
    end do
  end if
  if (xed + da_halo > num_lon .and. xst - da_halo >= 1) then
    do j = 1, num_lev
      do i = yst-da_halo, yed+da_halo
        if (i >= 1 .and. i <= num_lat) then
          ij = ij + 1
          length_t(ij) = (xed - xst + 1 + da_halo) * mem_num_local
          start_t(ij) = INDEX(j, i, xst-da_halo, num_lat, num_lon) * mem_num_local + 1
          ij = ij + 1
          length_t(ij) = da_halo * mem_num_local
          start_t(ij) = INDEX(j, i, 1, num_lat, num_lon) * mem_num_local + 1
        end if
      end do
    end do
  end if
  if (xst - da_halo < 1 .and. xed + da_halo > num_lon) then
    do j = 1, num_lev
      do i = yst-da_halo, yed+da_halo
        if (i >= 1 .and. i <= num_lat) then
          ij = ij + 1
          length_t(ij) = da_halo * mem_num_local
          start_t(ij) = INDEX(j, i, num_lon-da_halo+1, num_lat, num_lon) * mem_num_local + 1
          ij = ij + 1
          length_t(ij) = (xed - xst + 1) * mem_num_local
          start_t(ij) = INDEX(j, i, xst, num_lat, num_lon) * mem_num_local + 1
          ij = ij + 1
          length_t(ij) = da_halo * mem_num_local
          start_t(ij) = INDEX(j, i, 1, num_lat, num_lon) * mem_num_local + 1
        end if
      end do
    end do
  end if

  allocate(start(ij))
  allocate(length(ij))
  start(1:ij) = start_t(1:ij)
  length(1:ij) = length_t(1:ij)

  !GSMap receive from gmcore
  call GlobalSegMap_init(DA_GSMap, start, length, 0, MPI_DA_GROUP, DA_Id, gsize=gnum)

  deallocate(start)
  deallocate(length)
  call log_notice_root("segmap1 init")

  ij = 0
  do j = 1, num_lev
    do i = yst, yed
      ij = ij + 1
      length_t(ij) = (xed - xst + 1) * mem_num_local
      start_t(ij) = INDEX(j, i, xst, num_lat, num_lon) * mem_num_local + 1
    end do
  end do
  
  allocate(start_n(ij))
  allocate(length_n(ij))
  start_n(1:ij) = start_t(1:ij)
  length_n(1:ij) = length_t(1:ij)
  call GlobalSegMap_init(DA_N_GSMap, start_n, length_n, 0, MPI_DA_GROUP, DA_Id, gsize=gnum)
  call log_notice_root("segmap2 init")

  call Rearranger_init(DA_GSMap, DA_N_GSMap, MPI_DA_GROUP, DA_Re_To_Nov)
  call Rearranger_init(DA_N_GSMap, DA_GSMap, MPI_DA_GROUP, DA_Re_To_Ov)
  call log_notice_root("rearranger init")

  deallocate(start_t)
  deallocate(length_t)
  deallocate(start_n)
  deallocate(length_n)

  avsize = GlobalSegMap_lsize(DA_GSMap,MPI_DA_GROUP)
  av_nsize = GlobalSegMap_lsize(DA_N_GSMap,MPI_DA_GROUP)

  !print *, da_var_name

  call AttrVect_init(DA_AV,rList="ps:u:v:pt",lsize=avsize)
  !call AttrVect_init(DA_Cal, rList="num", lsize=avsize)
  call AttrVect_init(DA_AV_N,rList="ps:u:v:pt",lsize=av_nsize)
  !call AttrVect_init(DA_Cal_N, rList="num", lsize=av_nsize)

  !print *, da_var_name

#if defined ATM_COUPLING
  allocate(R_DA2ATM(1:atm_ensemble_group))
  do groupid = 1, atm_ensemble_group
    call Router_init(atm_subid(groupid),DA_GSMap,MPI_DA_GROUP,R_DA2ATM(groupid))
  end do
  call log_notice_root("router init")
#endif

  !!cnn_map [lev,ens,lat,lon] with halo
  !!cnn_final_map [ens,lev,lat,lon] with halo
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

  !call log_notice("da n_max:"//to_str(n_max))
  !call Router_init(CNN_Id,DA_CNNMap,MPI_DA_GROUP,R_DA2CNN)
  call log_notice_root("da coupler init end")


  !allocate(area_count(avsize))
  !do i = 1, avsize
  !  area_count(i) = 1.0
  !end do
  !call AttrVect_importRAttr(DA_Cal, "num", area_count)
  !call Rearranger_rearrange(DA_Cal, DA_Cal_N, DA_Re, sum=.true.) 
  !call AttrVect_exportRAttr(DA_Cal_N, "num", area_count_no_overlap, size_nooverlap)

  !deallocate(area_count)
  !call AttrVect_clean(DA_Cal)
  !call AttrVect_clean(DA_Cal_N)
  !deallocate(start)
  !deallocate(length)
  !deallocate(start_c)
  !deallocate(length_c)
  !deallocate(start_nc)
  !deallocate(length_nc)



  call log_notice_root("cnn_init")
  !cnn_init
  
  cnn_nmax = n_max
  cnn_gnum = 6
  if (myid < cnn_group) then
    cnn_lonst = 1
    cnn_loned = num_lon
    cnn_latst = 1
    cnn_lated = num_lat
  
    cnn_gid = myid / cnn_gnum
    cnn_group_num = cnn_group / cnn_gnum
    mem_sec = cnn_ensemble / cnn_group_num
    mem_res = mod(cnn_ensemble, cnn_group_num)
    if (cnn_gid < mem_res) then
      cnn_memst = mem_sec * cnn_gid + cnn_gid + 1
      cnn_memed = cnn_memst + mem_sec
    else
      cnn_memst = mem_sec * cnn_gid + mem_res + 1
      cnn_memed = cnn_memst + mem_sec - 1
    end if
    cnn_nlon = num_lon
    cnn_nlat = num_lat
    cnn_nmem = cnn_memed - cnn_memst + 1

    lev_sec = cnn_nmax / cnn_gnum
    lev_res = mod(cnn_nmax, cnn_gnum)
    cnn_gid = mod(myid, cnn_gnum)
    if (cnn_gnum - cnn_gid <= lev_res) then
      cnn_levst = lev_sec * cnn_gid + (lev_res - (cnn_gnum - cnn_gid)) + 1
      cnn_leved = cnn_levst + lev_sec
    else
      cnn_levst = lev_sec * cnn_gid + 1
      cnn_leved = cnn_levst + lev_sec - 1
    end if
    cnn_nlev = cnn_leved - cnn_levst + 1

  else
    cnn_memst = 1
    cnn_memed = 0
    cnn_latst = 1
    cnn_lated = 0
    cnn_lonst = 1
    cnn_loned = 0
    cnn_levst = 1
    cnn_leved = 0
    cnn_nmem = 0
    cnn_nlat = 0
    cnn_nlon = 0
    cnn_nlev = 0
  end if
  call log_notice("cnn range:"//to_str(myid)//" "//to_str(cnn_levst)//" "//to_str(cnn_leved)//" "//to_str(cnn_memst)//" "//to_str(cnn_memed))
  
  allocate(start(1:num_lev*mem_num_local*(yed-yst+1)))
  allocate(length(1:num_lev*mem_num_local*(yed-yst+1)))

  ! lon,lat,mem,lev
  ij = 0
  do j = 1, num_lev
    do k = 1, mem_num_local
      do i = yst, yed
        ij = ij + 1
        length(ij) = (xed - xst + 1)
        start(ij) = (j-1)*mem_num_local*num_lat*num_lon + INDEX(k, i, xst, num_lat, num_lon) + 1 
      end do
    end do
  end do

  call GlobalSegMap_init(DA2CNN_MAP, start, length, 0, MPI_DA_GROUP, DA_Id, gsize=gnum)
  da2cnnsize = GlobalSegMap_lsize(DA2CNN_MAP, MPI_DA_GROUP)

  !deallocate(start)
  !deallocate(length)

  allocate(cnn_start(num_lev))
  allocate(cnn_length(num_lev))

  do i = 1, num_lev
    cnn_start(i) = (i-1)*mem_num_local*num_lat*num_lon + (cnn_memst-1)*num_lat*num_lon + 1
    cnn_length(i) = num_lat*num_lon*cnn_nmem
  end do

  call GlobalSegMap_init(CNN_Map, cnn_start, cnn_length, 0, MPI_DA_GROUP, DA_Id, gsize=gnum)
  cnn_lsize = GlobalSegMap_lsize(CNN_Map, MPI_DA_GROUP)
  
  call log_notice("cnn map:"//to_str(myid)//" "//to_str(cnn_lsize)//" "//to_str(av_nsize)//" "//to_str(da2cnnsize))

  call Rearranger_init(DA2CNN_MAP, CNN_Map, MPI_DA_GROUP, CNN_Re_from_DA)
  call Rearranger_init(CNN_Map, DA2CNN_MAP, MPI_DA_GROUP, CNN_Re_to_DA)

  call AttrVect_init(CNN_AV,rList="ps:u:v:pt",lsize=cnn_lsize)
  call log_notice("cnn_mct_init_end")
  
  if (cnn_nmem > 0) then
    call GPTLstart("cnn init time")
    call cnn_c_init(trim(cnn_model_path)//c_null_char, cnn_levst-1, lev_sec+1, 1, cnn_nmax, cnn_nlat+2*cnn_lathalo, cnn_nlon+2*cnn_lonhalo) ! Fixed sample num
    call GPTLstop("cnn init time")
  end if

  call log_notice("cnn_init_end")

  deallocate(start)
  deallocate(length)
  deallocate(cnn_start)
  deallocate(cnn_length)

end subroutine 


subroutine dafatm_coupling(MPI_DA_GROUP)

  use datetime
  use coupler_config, only:da_start_time_array=>da_start_time, end_time_array=>end_time, &
                           da_in_seconds, num_lon, num_lat

  implicit none
  integer, intent(in) :: MPI_DA_GROUP
  integer :: myrank
  integer :: ierr
  integer :: i, ls, j, ij, ts, da_time
  integer :: halo2
  integer :: mem_num, mem_num_local

  ! x : longitude, y : latitude
  integer :: nx, ny
  integer :: lxst, lxed, lyst, lyed
  integer :: ix, iy, iz, debug_int, mem_i, mod_i, niz, nix
  integer :: debug_i, debug_j, debug_k

  axb_type, dimension(:), pointer :: recvdata, recv_nooverlap, cnn_data, letkf_data
  ! N = num of vars
  integer :: n_var
  ! n = num of n dimension (2d + 3d * lev)
  integer :: n_max
  integer :: n_start, ens_start, obs_num_2d, obs_num_3d
  integer, allocatable, dimension(:) :: var_lev
  character(len = 20), dimension(10) :: var_name
  character(len = 20), allocatable, dimension(:) :: var_name_ordered
  character(len = 20) :: tname
  integer :: groupid
  character(12) :: curr_time_str
  
  axb_type, allocatable, dimension(:, :, :, :) :: da_data_t
  axb_type, allocatable, dimension(:, :, :, :) :: da_data
  axb_type, allocatable, dimension(:, :, :, :) :: result_data
  axb_type, allocatable, dimension(:, :, :) :: da_data_mean
  axb_type, allocatable, dimension(:, :, :) :: result_data_mean

  real(8), allocatable, dimension(:, :, :, :) :: data_middle

  real(8), allocatable, dimension(:, :, :) :: spread_b
  real(8), allocatable, dimension(:, :, :) :: spread_a

  type(datetime_type) da_start_time
  type(datetime_type) end_time
  type(datetime_type) curr_time
  type(timedelta_type) delta_time

  integer :: test_x, test_y

  real(8), dimension(:), pointer :: cnn_recvdata
  integer :: cnn_lxst, cnn_lxed, cnn_lyst, cnn_lyed 
  integer :: m_st, m_ed, model_ix_st, model_ix
  real(4), allocatable, dimension(:, :, :, :) :: cnn_data_in
  real(4), allocatable, dimension(:, :, :, :) :: cnn_data_out
  real(4), allocatable, dimension(:, :, :) :: cnn_data_in_tmp
  real(4), allocatable, dimension(:, :) :: cnn_data_tmp

  real(8), allocatable, dimension(:) :: cnn_std
  real(8), allocatable, dimension(:) :: cnn_mean

  character(200) :: init_root_path  = '/home/export/online1/mdt00/shisuan/swthhpca/liyiyuan/dida-v3.0/SCRIPTS/gmcore100km_512/gmcore_100km.h0_000000.nc'
  character(200) :: init_data_paths(513)
  character(10) :: s_id
  integer :: ens_step, ens_id, lth
  integer :: is, ie, js, je, ks, ke
  integer :: nc_start(3), nc_count(3)

  nx = xed - xst + 1
  ny = yed - yst + 1

  lxst = 1 - da_halo; lxed = nx + da_halo
  lyst = 1 - da_halo; lyed = ny + da_halo
  if (yst == 1) lyst = 1
  if (yed == num_lat) lyed = ny

  mem_num = cnn_ensemble
  mem_num_local = cnn_ensemble

#if defined ATM_COUPLING
  mem_num = atm_ensemble_total
  mem_num_local = atm_ensemble_total / atm_ensemble_group
#endif

  ! !!!!!!!!!!!!!
  ! vars parsing
  call stringsplit(da_var_name,',',var_name,n_var)
  allocate(var_name_ordered(n_var))
  allocate(var_lev(n_var))
  call var_info(var_name, n_var, var_name_ordered, obs_num_2d, obs_num_3d)
  n_max = 0
  do i = 1, obs_num_2d
    var_lev(i) = 1
    n_max = n_max + var_lev(i)
  end do
  do i = obs_num_2d + 1, n_var
    var_lev(i) = num_lev
    n_max = n_max + var_lev(i)
  end do
  ! !!!!!!!!!!!!!
  ! !!!!!!!!!!!!!

  ! var_name(1) = "u"; var_name(2) = "v"; var_name(3) = "gz"
  ! var_lev(1) = num_lev; var_lev(2) = num_lev; var_lev(3) = num_lev
  ! n_max = 3 * num_lev

  allocate(data_middle(mem_num,n_max,1-da_halo:ny+da_halo,1-da_halo:nx+da_halo))

  allocate(da_data_t(mem_num_local, 1-da_halo:nx+da_halo, &
  1-da_halo:ny+da_halo, n_max))
  allocate(da_data(cnn_ensemble, n_max, &
  1-da_halo:ny+da_halo, 1-da_halo:nx+da_halo))
  allocate(result_data(cnn_ensemble, n_max, &
  1-da_halo:ny+da_halo, 1-da_halo:nx+da_halo))
  allocate(da_data_mean(n_max, 1:ny, 1:nx))
  allocate(result_data_mean(n_max, 1:ny, 1:nx))
  allocate(spread_b(n_max, 1:ny, 1:nx))
  allocate(spread_a(n_max, 1:ny, 1:nx))

  halo2 = 2 * da_halo

  da_start_time = create_datetime(year=da_start_time_array(1),  &
                                 month=da_start_time_array(2), &
                                 day=da_start_time_array(3),   &
                                 hour=da_start_time_array(4),  &
                                 minute=da_start_time_array(5))

  end_time      = create_datetime(year=end_time_array(1),  &
                                 month=end_time_array(2), &
                                 day=end_time_array(3),   &
                                 hour=end_time_array(4),  &
                                 minute=end_time_array(5))

  delta_time = end_time - da_start_time
  !call MCT_Recv(DA_AV, R_DA2ATM)
  !print *, "Before receiving"
  
  da_time = ceiling((delta_time%days*86400+delta_time%hours*3600+delta_time%minutes*60+delta_time%seconds)/da_in_seconds)
  !da_time = da_time + 1
  !da_time = 3 !Warning !
  !print *, "before recving"
  call MPI_COMM_RANK(MPI_DA_GROUP, myrank, ierr)
  if (myrank == 0) then
    call log_notice_root("da_time " // to_str(da_time))
  endif

  allocate(recvdata(avsize))
  allocate(recv_nooverlap(av_nsize))
  !allocate(cnn_data(cnn_size))
  !allocate(letkf_data(letkf_size))
  allocate(cnn_recvdata(cnn_lsize))

  !allocate(cnn_data_in_tmp(cnn_lonst-cnn_halo:cnn_loned+cnn_halo,cnn_latst-cnn_halo:cnn_lated+cnn_halo,n_max,atm_ensemble_total))
  !allocate(cnn_data_out_tmp(cnn_lonst-cnn_halo:cnn_loned+cnn_halo,cnn_latst-cnn_halo:cnn_lated+cnn_halo,n_max,cnn_nmem))

  if (cnn_nmem > 0) then
    allocate(cnn_data_in(cnn_lonst-cnn_lonhalo:cnn_loned+cnn_lonhalo,cnn_latst-cnn_lathalo:cnn_lated+cnn_lathalo,n_max,cnn_nmem))
    allocate(cnn_data_out(cnn_lonst-cnn_lonhalo:cnn_loned+cnn_lonhalo,cnn_latst-cnn_lathalo:cnn_lated+cnn_lathalo,n_max,cnn_nmem))
    allocate(cnn_data_tmp(cnn_lonst-cnn_lonhalo:cnn_loned+cnn_lonhalo,cnn_latst-cnn_lathalo:cnn_lated+cnn_lathalo))
    allocate(cnn_data_in_tmp(cnn_lonst-cnn_lonhalo:cnn_loned+cnn_lonhalo,cnn_latst-cnn_lathalo:cnn_lated+cnn_lathalo,n_max))
  end if

  cnn_lxst = cnn_lonst - cnn_halo
  cnn_lxed = cnn_loned + cnn_halo
  cnn_lyst = cnn_latst - cnn_halo
  cnn_lyed = cnn_lated + cnn_halo
  if (cnn_latst == 1) then
    cnn_lyst = cnn_latst
  end if
  if (cnn_lated == num_lat) then
    cnn_lyed = cnn_lated
  end if

  ens_step = 512 / cnn_ensemble
  lth = len_trim(init_root_path)

  call GPTLstart("GMCORE data init")

  call log_notice("before data init")
  ! read da_data from nc file, initial
  call fiona_init()

  if (cnn_nmem > 0) then

    ens_id = 1 + (cnn_memst - 1) * ens_step
    do i = 1, cnn_nmem
      init_data_paths(i) = init_root_path
      write(s_id,"(i3.3)") ens_id
      init_data_paths(i)(lth-5:lth-3) = s_id
      ens_id = ens_id + ens_step
    end do

    ens_id = 1 + (cnn_memst - 1) * ens_step
    write(s_id,"(i3.3)") ens_id
    call fiona_open_dataset('i0', file_paths=init_data_paths)
    do i = 1, cnn_nmem
      !init_data_path = init_root_path
      !write(s_id,"(i3.3)") ens_id
      !init_data_path(lth - 5 : lth - 3) = s_id
      !ens_id = ens_id + ens_step

      !call fiona_open_dataset(trim(s_id), file_path=init_data_path, mpi_comm=MPI_DA_GROUP)
      !call log_notice("create dataset")
      call fiona_start_input('i0', file_idx=i)
      !call log_notice("start_input")

      is = cnn_lonst; ie = cnn_loned
      js = cnn_latst; je = cnn_lated
      ks = 1; ke = num_lev
      nc_start = [is,js,ks]
      nc_count = [cnn_nlon,cnn_nlat,num_lev]

      call fiona_input('i0', 'phs', cnn_data_out(is:ie,js:je,1,i), start=nc_start, count=nc_count)
      !call log_notice("input phs")
      call fiona_input('i0', 'pt' , cnn_data_out(is:ie,js:je,65+ks:65+ke,i), start=nc_start, count=nc_count)
      !call log_notice("input pt")
      
      !call log_notice("nc_info:"//to_str(is)//" "//to_str(js)//" "//to_str(ks)//" "//to_str(nx)//" "//to_str(ny)//" "//to_str(num_lev)//" "//to_str(da_data_t(i,1,1,1), 3)//" "//to_str(da_data_t(i,1,1,66)))

      call fiona_input('i0', 'u_Agrid', cnn_data_out(is:ie,js:je,1+ks:1+ke,i), start=nc_start, count=nc_count)
      !call log_notice("input_u")
      call fiona_input('i0', 'v_Agrid', cnn_data_out(is:ie,js:je,33+ks:33+ke,i), start=nc_start, count=nc_count)
      !call log_notice("input_v")
      call fiona_end_input('i0')
      
      call log_notice("nc_input member "//to_str(cnn_memst+i-1))
    
    end do
    call fiona_close_dataset('i0')

    n_start = 0
    do i = 1, n_var

      tname = trim(var_name_ordered(i))

      debug_int = 0
      do iz = 1, var_lev(i)
        do mem_i = 1, cnn_nmem
          do iy = cnn_latst, cnn_lated
            do ix = cnn_lonst, cnn_loned
              niz = n_start + iz
              cnn_recvdata(debug_int+1) = cnn_data_out(ix,iy,niz,mem_i)
              debug_int = debug_int + 1
            end do
          end do
        end do
      end do

      call AttrVect_importRAttr(CNN_AV, &
      static_string(tname, len_trim(tname)), &
      cnn_recvdata)
      n_start = n_start + var_lev(i)
    end do

  end if

  call log_notice("init data end")

  !call log_notice("nc_input:"//to_str(da_data_t(1,1,1,1),5)//" "//to_str(da_data_t(1,1,1,2),5)//" "//to_str(da_data_t(1,1,1,34),5)//" "//to_str(da_data_t(1,1,1,66),5))
  call Rearranger_rearrange(CNN_AV, DA_AV_N, CNN_Re_To_DA)
  call log_notice("CNN_Rearrange_to_DA")
  
  !call GPTLstart("comm: CNN to LETKF")
  ! (lon:lat:mem:lev) to (mem:lon:lat:lev) 

  n_start = 0
  do i = 1, n_var

    tname = trim(var_name_ordered(i))
    call AttrVect_exportRAttr(DA_AV_N, &
    static_string(tname, len_trim(tname)), &
    recv_nooverlap, av_nsize)

    debug_int = 0
    
    do iz = 1, var_lev(i)
      do mem_i = 1, mem_num
        do iy = 1, ny
          do ix = 1, nx
            da_data_t(mem_i,ix,iy,n_start+iz) = recv_nooverlap(debug_int+1)
            debug_int = debug_int + 1
          end do
        end do
      end do
    end do

    n_start = n_start + var_lev(i)
  end do

  n_start = 0
  do i = 1, n_var

    debug_int = 0
    do iz = 1, var_lev(i)
      do iy = 1, ny
        do ix = 1, nx
          recv_nooverlap(debug_int+1:debug_int+mem_num) = da_data_t(1:mem_num,ix,iy,n_start+iz)
          debug_int = debug_int + mem_num
        end do
      end do
    end do
    n_start = n_start + var_lev(i)

    tname = trim(var_name_ordered(i))
    call AttrVect_importRAttr(DA_AV_N, &
    static_string(tname, len_trim(tname)), &
    recv_nooverlap)

  end do

  call Rearranger_rearrange(DA_AV_N, DA_AV, DA_Re_To_Ov) 
  n_start = 0
  do i = 1, n_var
    tname = trim(var_name_ordered(i))
    call AttrVect_exportRAttr(DA_AV, &
    static_string(tname, len_trim(tname)), &
    recvdata, avsize)
    debug_int = 0
    do iz = 1, var_lev(i)
      do iy = lyst, lyed
        do ix = lxst, lxed
          da_data(1:mem_num,n_start+iz,iy,ix) = recvdata(debug_int+1:debug_int+mem_num)
          debug_int = debug_int + mem_num
        end do
      end do
    end do
    n_start = n_start + var_lev(i)
  end do
  call log_notice("init rearrange end")

  allocate(cnn_mean(n_max))
  allocate(cnn_std(n_max))
  if (myrank == 0) then
    open(50,FILE=trim(cnn_mean_path),FORM='unformatted',ACCESS='stream')
    read(50) cnn_mean
    close(50)

    open(50,FILE=trim(cnn_std_path),FORM='unformatted',ACCESS='stream')
    read(50) cnn_std
    close(50)
  end if
  call MPI_Bcast(cnn_mean, n_max, MPI_REAL8, 0, MPI_DA_GROUP, ierr)
  call MPI_Bcast(cnn_std, n_max, MPI_REAL8, 0, MPI_DA_GROUP, ierr)
  call log_notice("std&mean init:"//to_str(cnn_mean(1),5)//" "//to_str(cnn_std(1),5))

  call GPTLstop("GMCORE data init")

  !call log_notice("da_input:"//to_str(da_data(1,1,1,1),5)//" "//to_str(da_data(1,2,1,1),5)//" "//to_str(da_data(1,34,1,1),5)//" "//to_str(da_data(1,66,1,1),5))

  do ts = 1, da_time

    ens_start = 1

    !call alloc_obs_struct(ts-1)

#if defined ATM_COUPLING
    do groupid = 1, atm_ensemble_group
      !print *, n_var, groupid
      call MCT_Recv(DA_AV, R_DA2ATM(groupid))
      !print *, mem_num_local

      n_start = 1
      do i = 1, n_var

        !print *, trim_len(var_name_ordered(i))
        tname = trim(var_name_ordered(i))
        !print *, myrank, "len", len_trim(tname)
        call AttrVect_exportRAttr(DA_AV, &
        static_string(tname, len_trim(tname)), &
        recvdata, ls)
        !call AttrVect_exportRAttr(DA_AV, "u", recvdata, ls)

        !da_data_t(lxst:lxed, lyst:lyed, :) = recvdata(1:ls)
        !print *, ls, mem_num_local*(lxed-lxst+1)*(lyed-lyst+1)*var_lev(i)

        debug_int = 0
        do iz = 1, var_lev(i)
          do iy = lyst, lyed
            do ix = lxst, lxed
              da_data_t(1:mem_num_local, ix, iy, iz) = recvdata(debug_int + 1:debug_int + mem_num_local)
              if (isnan(da_data_t(1, ix, iy, iz))) then
                print *, rank, ix, iy, iz, "recv data nan"
                call log_error("recv nan")
              end if
              debug_int = debug_int + mem_num_local
            end do
          end do
        end do

        ! da_data_t(1:mem_num, lxst:lxed, lyst:lyed, 1:var_lev(i)) = &
        ! reshape(recvdata(1:ls), (/mem_num,lxed-lxst+1,lyed-lyst+1,var_lev(i)/))

        ! (ens_num, lon_num, lat_num, n_2d + lev_num * n_3d) 
        ! transform to 
        !(ens_num, n_2d + lev_num * n_3d, lat_num, lon_num)
        call struct_atm2endnn(mem_num_local, var_lev(i), ny+halo2, nx+halo2, mem_num, n_max, ens_start, n_start, &
        da_data_t(:, :, :, :), data_middle(:, :, :, :))
        !result_data(:, :, :, :) = da_data(:, :, :, :)

        n_start = n_start + var_lev(i)

      end do

      ens_start = ens_start + mem_num_local
      
    end do
#endif    
    
    call GPTLstart("DA online cycle")

    call log_notice("da_input:"//to_str(da_data(1,1,1,1),5)//" "//to_str(da_data(1,2,1,1),5)//" "//to_str(da_data(1,34,1,1),5)//" "//to_str(da_data(1,66,1,1),5))

    call GPTLstart("LETKF running time")

    !result_data(:,:,:,:)=da_data(:,:,:,:)
    call run_da_parallel(n_max, ny, nx, ts-1, da_data, result_data, spread_b, spread_a)

    call GPTLstop("LETKF running time")
    !print *, da_data(1, 1, 1, 1), result_data(1, 1, 1, 1)
    ! (ens_num, n_2d + lev_num * n_3d, lat_num, lon_num)

    do ix = 1, nx
      do iy = 1, ny
        do iz = 1, n_max
          da_data_mean(iz, iy, ix) = sum(da_data(1:mem_num, iz, iy, ix)) / mem_num
        end do
      end do
    end do
    do ix = 1, nx
      do iy = 1, ny
        do iz = 1, n_max
          result_data_mean(iz, iy, ix) = sum(result_data(1:mem_num, iz, iy, ix)) / mem_num
        end do
      end do
    end do

    data_middle(1:mem_num,:,:,:) = result_data(1:mem_num,:,:,:)

    !Extract DA Output Data
    n_start = 0
    do i = 1, n_var

      debug_int = 0
      do iz = 1, var_lev(i)
        do iy = lyst, lyed
          do ix = lxst, lxed
            recvdata(debug_int+1:debug_int+mem_num) = data_middle(1:mem_num,n_start+iz,iy,ix)
            debug_int = debug_int + mem_num
          end do
        end do
      end do
      tname = trim(var_name_ordered(i))
      call AttrVect_importRAttr(DA_AV, &
      static_string(tname, len_trim(tname)), &
      recvdata)
      n_start = n_start + var_lev(i)

    end do
    ! call log_notice("before_rearrange_1")
    
    call GPTLstart("comm: LETKF to CNN")

    call Rearranger_rearrange(DA_AV, DA_AV_N, DA_Re_To_Nov, sum=.true.) 
    call log_notice("rearrange_to_NonOverlap_ccomplete")
    
    !call GPTLstop("comm: LETKF to CNN")

    !(mem:lon:lat:lev) to (lon:lat:mem:lev)
    n_start = 0
    do i = 1, n_var
      tname = trim(var_name_ordered(i))
      call AttrVect_exportRAttr(DA_AV_N, &
      static_string(tname, len_trim(tname)), &
      recv_nooverlap, av_nsize)

      debug_int = 0
      do iz = 1, var_lev(i)
        do iy = 1, ny
          do ix = 1, nx
            recv_nooverlap(debug_int+1:debug_int+mem_num) = da_data_t(1:mem_num,ix,iy,n_start+iz)
            debug_int = debug_int + mem_num
          end do
        end do
      end do
      n_start = n_start + var_lev(i)
    end do
  
    n_start = 0
    do i = 1, n_var

      debug_int = 0
      do iz = 1, var_lev(i)
        do mem_i = 1, mem_num
          do iy = 1, ny
            do ix = 1, nx
              recv_nooverlap(debug_int+1) = da_data_t(mem_i,ix,iy,n_start+iz)
              debug_int = debug_int + 1
            end do
          end do
        end do
      end do

      tname = trim(var_name_ordered(i))
      call AttrVect_importRAttr(DA_AV_N, &
      static_string(tname, len_trim(tname)), &
      recv_nooverlap)

      n_start = n_start + var_lev(i)
    end do

    call Rearranger_rearrange(DA_AV_N, CNN_AV, CNN_Re_from_DA)
    call log_notice("DA_rearrange_to_CNN")

    call GPTLstop("comm: LETKF to CNN")

    if (cnn_nmem > 0) then
  
    n_start = 0
    do i = 1, n_var

    !   !recvdata(1:ls) = &
    !   !reshape(da_data_t(1:mem_num, lxst:lxed, lyst:lyed, 1:var_lev(i)), (/ls/))
      tname = trim(var_name_ordered(i))
      call AttrVect_exportRAttr(CNN_AV, &
      static_string(tname, len_trim(tname)), &
      cnn_recvdata, cnn_lsize)

      debug_int = 0
      do iz = 1, var_lev(i)
        do mem_i = 1, cnn_nmem
          do iy = cnn_latst, cnn_lated
            do ix = cnn_lonst, cnn_loned
              niz = n_start + iz
              if (i == 4) then
                niz = niz - 64
              else if (i == 2) then
                niz = niz + 32
              else if (i == 3) then
                niz = niz + 32
              end if
              cnn_data_in(ix,iy,niz,mem_i) = cnn_recvdata(debug_int+1)
              debug_int = debug_int + 1
            end do
          end do
        end do
      end do

      n_start = n_start + var_lev(i)

    end do


    ! deal with cnn lon && lat halo data
    if (cnn_latst == 1) then !
      do ix = 1, cnn_lathalo
        cnn_data_in(:,cnn_latst-ix,:,:) = cnn_data_in(:,cnn_latst,:,:)
      end do
    end if

    if (cnn_lated == num_lat) then
      do ix = 1, cnn_lathalo
        cnn_data_in(:,cnn_lated+ix,:,:) = cnn_data_in(:,cnn_lated,:,:)
      end do
    end if

    do ix = 1, cnn_lonhalo
      cnn_data_in(1-ix,:,:,:) = cnn_data_in(cnn_loned+1-ix,:,:,:)
      cnn_data_in(cnn_loned+ix,:,:,:) = cnn_data_in(ix,:,:,:)
    end do

    call log_notice("before cnn_predict "//to_str(myrank)//" "//to_str(cnn_data_in(10,10,1,1),5)//" "//to_str(cnn_data_in(10,10,2,1),5)//" "//to_str(cnn_data_in(10,10,34,1),5)//" "//to_str(cnn_data_in(10,10,66,1),5))

    
    call GPTLstart("cnn run time")

    cnn_data_out(:,:,:,:) = 0
    do ix = 1, cnn_nmem
      cnn_data_in_tmp(:,:,:) = cnn_data_in(:,:,:,ix)

      do iy = 1, n_max
        cnn_data_in_tmp(:,:,iy) = (cnn_data_in_tmp(:,:,iy) - cnn_mean(iy)) / cnn_std(iy)
      end do

      if (ix == 1) then

      call log_notice("after norm "//to_str(myrank)//" "//to_str(cnn_data_in_tmp(10,10,1),5)//" "//to_str(cnn_data_in_tmp(10,10,2),5)//" "//to_str(cnn_data_in_tmp(10,10,34),5)//" "//to_str(cnn_data_in_tmp(10,10,66),5))

      end if

      do iy = cnn_levst, cnn_leved
        call GPTLstart("cnn predict time")
        call cnn_predict(cnn_data_in_tmp, cnn_data_tmp, iy-cnn_levst, myrank)
        call GPTLstop("cnn predict time")

        cnn_data_tmp(:,:) = cnn_data_tmp(:,:) - cnn_data_in_tmp(:,:,1) + cnn_data_in_tmp(:,:,iy)

        if (iy == cnn_levst .and. ix == 1) then
            call log_notice("out before norm "//to_str(myrank)//" "//to_str(cnn_levst)//" "//to_str(cnn_data_tmp(10,10),5))
        end if

        cnn_data_tmp(:,:) = cnn_data_tmp(:,:) * cnn_std(iy) + cnn_mean(iy)

        cnn_data_out(:,:,iy,ix) = cnn_data_tmp(:,:)

      end do 
    end do

    call GPTLstop("cnn run time")

    call log_notice("after cnn_predict "//to_str(myrank)//" "//to_str(cnn_levst)//" "//to_str(cnn_data_out(10,10,cnn_levst,1),5))
  
    n_start = 0
    do i = 1, n_var

      tname = trim(var_name_ordered(i))

      debug_int = 0
      do iz = 1, var_lev(i)
        do mem_i = 1, cnn_nmem
          do iy = cnn_latst, cnn_lated
            do ix = cnn_lonst, cnn_loned
              niz = n_start + iz
              if (i == 4) then
                niz = niz - 64
              else if (i == 2) then
                niz = niz + 32
              else if (i == 3) then
                niz = niz + 32
              end if
              cnn_recvdata(debug_int+1) = cnn_data_out(ix,iy,niz,mem_i)
              debug_int = debug_int + 1
            end do
          end do
        end do
      end do

      call AttrVect_importRAttr(CNN_AV, &
      static_string(tname, len_trim(tname)), &
      cnn_recvdata)
      n_start = n_start + var_lev(i)

    end do

    end if

    call GPTLstart("comm: CNN to LETKF")

    call Rearranger_rearrange(CNN_AV, DA_AV_N, CNN_Re_To_DA, sum=.true.)
    call log_notice("CNN_Rearrange_to_DA")
    
    !call GPTLstart("comm: CNN to LETKF")
    ! (lon:lat:mem:lev) to (mem:lon:lat:lev) 

    n_start = 0
    do i = 1, n_var

      tname = trim(var_name_ordered(i))
      call AttrVect_exportRAttr(DA_AV_N, &
      static_string(tname, len_trim(tname)), &
      recv_nooverlap, av_nsize)

      debug_int = 0
      
      do iz = 1, var_lev(i)
        do mem_i = 1, mem_num
          do iy = 1, ny
            do ix = 1, nx
              da_data_t(mem_i,ix,iy,n_start+iz) = recv_nooverlap(debug_int+1)
              debug_int = debug_int + 1
            end do
          end do
        end do
      end do

      n_start = n_start + var_lev(i)
    end do

    n_start = 0
    do i = 1, n_var

      debug_int = 0
      do iz = 1, var_lev(i)
        do iy = 1, ny
          do ix = 1, nx
            recv_nooverlap(debug_int+1:debug_int+mem_num) = da_data_t(1:mem_num,ix,iy,n_start+iz)
            debug_int = debug_int + mem_num
          end do
        end do
      end do
      n_start = n_start + var_lev(i)

      tname = trim(var_name_ordered(i))
      call AttrVect_importRAttr(DA_AV_N, &
      static_string(tname, len_trim(tname)), &
      recv_nooverlap)

    end do
  
    call Rearranger_rearrange(DA_AV_N, DA_AV, DA_Re_To_Ov) 
    call log_notice("rearrange_to_Overlap_complete")

    call GPTLstop("comm: CNN to LETKF")

    n_start = 0
    do i = 1, n_var

      tname = trim(var_name_ordered(i))
      call AttrVect_exportRAttr(DA_AV, &
      static_string(tname, len_trim(tname)), &
      recvdata, avsize)

      debug_int = 0
      do iz = 1, var_lev(i)
        do iy = lyst, lyed
          do ix = lxst, lxed
            da_data(1:mem_num,n_start+iz,iy,ix) = recvdata(debug_int+1:debug_int+mem_num)
            debug_int = debug_int + mem_num
          end do
        end do
      end do
      n_start = n_start + var_lev(i)

    end do

    call GPTLstop("DA online cycle")

    call log_notice("DA loop end")

#if defined ATM_COUPLING
    data_middle(1:mem_num,:,:,:) = result_data(1:mem_num,:,:,:)
    ens_start = 1

    do groupid = 1, atm_ensemble_group
      n_start = 1

      do i = 1, n_var

        da_data_t(:, :, :, :) = 0

        call struct_endnn2atm(mem_num_local, var_lev(i), ny+halo2, nx+halo2, mem_num, n_max, ens_start, n_start, &
        data_middle(:, :, :, :), da_data_t(:, :, :, :))

        !da_data_t(:, lxst:lxed, lyst:lyed, :) = 0

        debug_int = 0
        do iz = 1, var_lev(i)
          do iy = lyst, lyed
            do ix = lxst, lxed
              recvdata(debug_int + 1:debug_int + mem_num_local) = da_data_t(1:mem_num_local, ix, iy, iz)
              debug_int = debug_int + mem_num_local
            end do
          end do
        end do

        !recvdata(1:ls) = &
        !reshape(da_data_t(1:mem_num, lxst:lxed, lyst:lyed, 1:var_lev(i)), (/ls/))
        tname = trim(var_name_ordered(i))
        call AttrVect_importRAttr(DA_AV, &
        static_string(tname, len_trim(tname)), &
        recvdata)

        n_start = n_start + var_lev(i)

      end do

      ! call log_notice("before_rearrange_1")
      ! call Rearranger_rearrange(DA_AV, DA_AV_N, DA_Re, sum=.true.) 
      ! call log_notice("rearrange_1_ccomplete")
      ! do i = 1, n_var

      !   !recvdata(1:ls) = &
      !   !reshape(da_data_t(1:mem_num, lxst:lxed, lyst:lyed, 1:var_lev(i)), (/ls/))
      !   tname = trim(var_name_ordered(i))
      !   call AttrVect_exportRAttr(DA_AV_N, &
      !   static_string(tname, len_trim(tname)), &
      !   recv_nooverlap, ls)

      !   ! do j = 1, ls
      !   !   recv_nooverlap(j) = recv_nooverlap(j) / area_count_no_overlap(j)
      !   ! end do

      !   call AttrVect_importRAttr(DA_AV_N, &
      !   static_string(tname, len_trim(tname)), &
      !   recv_nooverlap)

      ! end do
      ! call log_notice("before_rearrange_2")
      ! call Rearranger_rearrange(DA_AV_N, DA_AV, DA_Re_back) 
      ! call log_notice("rearrange_2_ccomplete")


      call MCT_Send(DA_AV, R_DA2ATM(groupid))

      ens_start = ens_start + mem_num_local

    end do
#endif


   ! print *, "haha", result_data_mean(26,61,85)
   !print *, "haha", result_data_mean(2,61,85)
   ! print *, "haha", result_data_mean(72,61,85)
    !call log_notice("before da_output")

    curr_time = da_start_time+create_timedelta(seconds=(ts-1)*da_in_seconds)
    curr_time_str = curr_time%format('%Y%m%d%H%M')
    
    !print *, 'curr time:', curr_time_str
    !print *,'var_name_ordered',var_name_ordered
    call GPTLstart("da output time")
#ifdef DAOUT_NC
    call da_output_nc(myrank, MPI_DA_GROUP,da_rc, yst, yed, xst, xed, obs_num_2d, obs_num_3d, var_name_ordered, curr_time_str, da_data_mean, result_data_mean, spread_b, spread_a)
#else
    call da_output_redis(myrank, MPI_DA_GROUP,da_rc, yst, yed, xst, xed, obs_num_2d, obs_num_3d, var_name_ordered, curr_time_str, da_data_mean, result_data_mean, spread_b, spread_a)
#endif
    call log_notice("da_output done!")
    call GPTLstop("da output time")
     !print *, myid, "redis done!"

    
    ! open(51, FILE='test_da_'//to_str(myrank)//'.txt')
    ! write (51, *) myrank, ls, xst, xed, yst, yed
    ! close(51)

  end do

  call log_notice("Out da run loop")

  deallocate(da_data)
  deallocate(da_data_t)
  deallocate(data_middle)
  deallocate(result_data)
  deallocate(da_data_mean)
  deallocate(result_data_mean)
  deallocate(var_name_ordered)
  deallocate(var_lev)
!  deallocate(recvdata)
 ! deallocate(spread_b)
 ! deallocate(spread_a)
  !deallocate(cnn_data)
  !deallocate(letkf_data)
!  deallocate(cnn_recvdata)
  !deallocate(cnn_data_in_tmp)
  !deallocate(cnn_data_out_tmp)
  if (cnn_nmem > 0) then
    deallocate(cnn_data_in)
    deallocate(cnn_data_out)
    deallocate(cnn_data_tmp)
    deallocate(cnn_data_in_tmp)
  end if
  !deallocate(cnn_mean)
  !deallocate(cnn_std)
  call log_notice("Out dafatm coupling")

end subroutine 

subroutine cnn_predict(input_buf, output_buf, indx, rk)
  
  type(*), target :: input_buf(*), output_buf(*)
  integer, intent(in) :: indx, rk
  call cnn_c_predict(1, cnn_nmax, cnn_nlat+2*cnn_lathalo, cnn_nlon+2*cnn_lonhalo, indx, c_loc(input_buf), c_loc(output_buf), rk)

end subroutine

subroutine clean_da_coupler()

  implicit none
  integer :: groupid

  deallocate(area_count_no_overlap)
  do groupid = 1, atm_ensemble_group
     call Router_clean(R_DA2ATM(groupid))
  end do
  deallocate(R_DA2ATM)
  !call Router_clean(R_DA2CNN)
  !call Router_clean(R_DA2ATM)
  call Rearranger_clean(DA_Re_To_Ov)
  call Rearranger_clean(DA_Re_To_Nov)
  call Rearranger_clean(CNN_Re_from_DA)
  call Rearranger_clean(CNN_Re_to_DA)
  !call Rearranger_clean(DA_Re_back)
  call AttrVect_clean(DA_AV)
  !call AttrVect_clean(DA_AV_N)
  call AttrVect_clean(DA_AV_CNN)
  call AttrVect_clean(DA_AV_CNN_OV)
  call AttrVect_clean(CNN_AV)
  call GlobalSegMap_clean(DA_GSMap)
  call GlobalSegMap_clean(DA_CNNMap)
  call GlobalSegMap_clean(DA_CNN_OV)
  call GlobalSegMap_clean(CNN_Map)
  call MCTWorld_clean()
  if (cnn_nlev > 0) then
    call cnn_c_destroy()
  end if

end subroutine

#include "data_trans.F90"

! Fortran string
function static_string(f_str, f_len)
  character(20), intent(in) :: f_str
  integer, intent(in) :: f_len
  character(len=f_len) static_string
  integer :: i
  character :: ts

  static_string = ""
  do i = 1, f_len
    ts = f_str(i:i)
    static_string(i:i) = ts
  end do
end function static_string

end module da_coupler
