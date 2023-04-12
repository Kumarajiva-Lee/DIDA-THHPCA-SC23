#include "../../mp_config/mct_config.inc"

#define INDEX(i, j, k, N, L) ((i-1)*(N)*(L)+(j-1)*(L)+(k-1))

module gmcore_coupler

    use m_MCTWorld,only: MCTWorld_init => init
    use m_MCTWorld,only: MCTWorld_clean => clean
    use m_GlobalSegMap,only: GlobalSegMap
    use m_GlobalSegMap,only: GlobalSegMap_init => init
    use m_GlobalSegMap,only: GlobalSegMap_lsize => lsize
    use m_GlobalSegMap,only: GlobalSegMap_clean => clean
    use m_GlobalSegMap,only: GlobalSegMap_Ordpnts => OrderedPoints
    use m_AttrVect,only    : AttrVect
    use m_AttrVect,only    : AttrVect_init => init
    use m_AttrVect,only    : AttrVect_clean => clean
    use m_AttrVect,only    : AttrVect_zero => zero
    use m_AttrVect,only    : AttrVect_indxR => indexRA
    use m_AttrVect,only    : AttrVect_importRAttr => importRAttr
    use m_AttrVect,only    : AttrVect_exportRAttr => exportRAttr
    use m_Router,only: Router
    use m_Router,only: Router_init => init
    use m_Router,only: Router_clean => clean
    use m_Transfer,only : MCT_Send => send
    use m_Transfer,only : MCT_Recv => recv
  
    use mpi
    use coupler_config
    use process_mod
    use parallel_mod
    use parallel_types_mod
    use dynamics_types_mod
    use mesh_mod
    use block_mod
    use string
    use namelist_mod , only : member_total
    use flogger
  
    implicit none
  
    type(GlobalSegMap)   :: ATM_GSMap             ! MCT defined type
    type(AttrVect) :: ATM_AV                      ! MCT defined type
    type(Router) :: R_ATM2DA                      ! MCT defined type
  
    integer, allocatable,dimension(:) :: start,length
    axb_type, dimension(:), pointer :: avdata
    integer :: avsize
  
    contains

    subroutine StringSplit_ATM(InStr,delimiter,StrArray,nsize)
      !----------------------------------------------
      !---将字符串InStr进行分割,结果放入StrArray中
      !---delimiter::分隔符号,例如';,,' 使用;和,分割字符串
      !---nsize:分割数目
      !----------------------------------------------
      character(len = *) , Intent( IN ) :: InStr
      character(len = *)  , Intent( IN ) :: delimiter
      character(len = LEN(InStr)),dimension(LEN(InStr)),Intent( OUT ) :: StrArray
      integer, Intent( OUT ) :: nsize ! Effective Size of StrArray
      integer:: i,j ! loop variable
      integer:: istart ! split index for Start Position
      !write(*,*)LEN(InStr)
      nsize=0
      istart=1
      do i=1,LEN(InStr)
        do j=1,LEN(delimiter)
          if (InStr(i:i) == delimiter(j:j)) then
            if (istart == i) then
              istart=i+1 ! ---可防止分隔符相连的情况
            end if
            if (istart<i) then
              nsize=nsize+1
              StrArray(nsize)=InStr(istart:i-1)
              istart=i+1
            end if
          end if
        end do
      end do
      ! ---匹配最后一个子字符串
      if (nsize>0) then
        if (istart<LEN(InStr)) then
          nsize=nsize+1
          StrArray(nsize)=InStr(istart:LEN(InStr))
        end if
      end if
      ! ---如果无可分割的子字符串,则包含整个字符串为数组的第一元素
      if ( (nsize<1) .AND. (LEN(TRIM(InStr)) > 0 )) then
        nsize=1
        StrArray(1)=InStr
      end if
    end subroutine StringSplit_ATM
  
  subroutine init_gmcore_coupler(MPI_ATM_GROUP, state)
  
    implicit none
    integer, intent(in) :: MPI_ATM_GROUP
    type(dstate_type), intent(in) :: state
    type(mesh_type), pointer :: mesh
    integer :: ierr
    integer :: i, j, ij
    integer :: lon_ibeg, lon_iend, lat_ibeg, lat_iend, lev_ibeg, lev_iend
    integer :: lev_num, lon_num, lat_num
    !integer :: gnum

    call MCTWorld_init(ncomps, dida_comm, MPI_ATM_GROUP, atm_subid(atm_groupmember%atm_subgid))

    !print *, "mct__init:", xst, xed, yst, yed, ATM_Id
    mesh => state%mesh

    lon_ibeg = mesh%full_ids
    lon_iend = mesh%full_ide
    lat_ibeg = mesh%full_jds
    lat_iend = mesh%full_jde
    lev_ibeg = mesh%full_kds
    lev_iend = mesh%full_kde
    lev_num = lev_iend - lev_ibeg + 1
    lon_num = lon_iend - lon_ibeg + 1
    lat_num = lat_iend - lat_ibeg + 1

    !gnum = lev_num * lon_num * lat_num * proc%member_value

    !print *, 'gmcore_init', lon_ibeg, lon_iend, lat_ibeg, lat_iend, global_mesh%full_nlat

    allocate(start(1:lat_num*lev_num))
    allocate(length(1:lat_num*lev_num))
  
    ij = 0
    do j = lev_ibeg, lev_iend
      do i = lat_ibeg, lat_iend
        ij = ij + 1
        length(ij) = (lon_iend - lon_ibeg + 1) * member_total
        start(ij) = ((j - 1) * global_mesh%full_nlat * global_mesh%full_nlon +  &
        (i - 1) * global_mesh%full_nlon + lon_ibeg - 1) * member_total + 1
      end do
    end do 

    call GlobalSegMap_init(ATM_GSMap, start, length, 0, MPI_ATM_GROUP, atm_subid(atm_groupmember%atm_subgid))

    deallocate(start)
    deallocate(length)
  
    avsize = GlobalSegMap_lsize(ATM_GSMap,MPI_ATM_GROUP)
  
    allocate(avdata(1:avsize))

    call AttrVect_init(ATM_AV,rList="ps:u:v:pt",lsize=avsize)
  
    call Router_init(DA_Id,ATM_GSMap,MPI_ATM_GROUP,R_ATM2DA)

  end subroutine 
  
  subroutine gmcore2da_send_coupling(MPI_ATM_GROUP, block, state)
  
    implicit none
    integer, intent(in) :: MPI_ATM_GROUP
    type(block_type), intent(inout) :: block
    type(dstate_type),intent(inout) :: state
    type(mesh_type), pointer :: mesh
    integer :: ierr
    integer i, j, ij, myrank, k , iter, ivec
    integer :: lon_ibeg, lon_iend, lat_ibeg, lat_iend, lev_ibeg, lev_iend, mem_num
    real, dimension(:), pointer :: revdata
    real, allocatable, dimension(:,:,:,:) :: nu,nv,ngd

    integer ,  dimension(10) :: var_lev      
    character(5) , dimension(10) :: var_name
    integer  :: n_var 
    integer  :: vb,ve

    call MPI_Comm_rank(MPI_ATM_GROUP, myrank, ierr)
  
    mesh => state%mesh

    lon_ibeg = mesh%full_ids
    lon_iend = mesh%full_ide
    lat_ibeg = mesh%full_jds
    lat_iend = mesh%full_jde
    lev_ibeg = mesh%full_kds
    lev_iend = mesh%full_kde

    mem_num = member_total
 
    ! call stringsplit_ATM(da_var_name,',',var_name,n_var)
    ! do i = 1 , n_var
    !   if (var_name(i) == 'ps') then 
    !     var_lev(i) = 1
    !   else
    !     var_lev(i) = num_lev
    !   end if
    ! end do

    n_var = 4
    var_name(2) = "u"; var_name(3) = "v"; var_name(4) = "pt" ; var_name(1) = 'ps'
    var_lev(2) = num_lev; var_lev(3) = num_lev; var_lev(4) = num_lev ; var_lev(1) = 1

    if (is_root_proc()) write(*,*) n_var , var_name(1:4) , var_lev(1:4)
    
    do iter = 1 , n_var
      select case(var_name(iter))
      case('u')

        allocate(nu (1:mem_num,lon_ibeg:lon_iend,lat_ibeg:lat_iend,lev_ibeg:lev_iend))
        do ivec = 1 , 1
          vb = (ivec-1)*member_num + 1
          ve = ivec*member_num
          do k = lev_ibeg, lev_iend
            do i = lon_ibeg, lon_iend
              do j = lat_ibeg, lat_iend
                nu(vb:ve,i,j,k) = (state%u_lon(:,i,j,k) + state%u_lon(:,i-1,j,k)) / 2
              end do
            end do
          end do
        end do   

        ij=1
        do k = lev_ibeg, lev_iend
          do j = lat_ibeg, lat_iend
            do i = lon_ibeg, lon_iend
              avdata(ij:ij+mem_num-1) = nu(:, i, j, k)
              ij = ij + mem_num
            end do
          end do
        end do   
        call AttrVect_importRAttr(ATM_AV,"u",avdata)

      case('v')

        allocate(nv (1:mem_num,lon_ibeg:lon_iend,lat_ibeg:lat_iend,lev_ibeg:lev_iend))
        do ivec = 1 , 1
          vb = (ivec-1)*member_num + 1
          ve = ivec*member_num
          do k = lev_ibeg, lev_iend
            do i = lon_ibeg, lon_iend
              do j = lat_ibeg, lat_iend
                nv(vb:ve,i,j,k) = 2 * (state%v_lat(:,i,j-1,k) * mesh%area_subcell(1,j) + state%v_lat(:,i,j,k) * mesh%area_subcell(2,j)) / mesh%area_cell(j)     
                !nv(vb:ve,i,j,k) =  state%v(:,i,j,k)
              end do
            end do
          end do
        end do
        ij=1
        do k = lev_ibeg, lev_iend
          do j = lat_ibeg, lat_iend
            do i = lon_ibeg, lon_iend
              avdata(ij:ij+mem_num-1) = nv(:, i, j, k)
              ij = ij + mem_num
            end do
          end do
        end do
        call AttrVect_importRAttr(ATM_AV,"v",avdata)

      case('pt')

        ij=1
        do k = lev_ibeg, lev_iend
          do j = lat_ibeg, lat_iend
            do i = lon_ibeg, lon_iend
              do ivec = 1 , 1
                avdata(ij:ij+member_num-1) = state%pt(:, i, j, k)
                ij = ij + member_num
              end do
            end do
          end do
        end do
        call AttrVect_importRAttr(ATM_AV,"pt",avdata)
      case('ps')
        ij = 1;
        do k = 1 , 1
          do j = lat_ibeg, lat_iend
            do i = lon_ibeg, lon_iend
              do ivec = 1 , 1
                avdata(ij:ij+member_num-1) = state%phs(:, i, j)
                ij = ij + member_num
              end do
            end do
          end do
        end do
        call AttrVect_importRAttr(ATM_AV,"ps",avdata) 
      end select
    end do
    
    call MCT_Send(ATM_AV, R_ATM2DA)
  end subroutine 

  subroutine gmcore2da_recv_coupling(MPI_ATM_GROUP, block, state)
  
    implicit none
    integer, intent(in) :: MPI_ATM_GROUP
    type(block_type), intent(inout) :: block
    type(dstate_type),intent(inout) :: state
    type(mesh_type), pointer :: mesh
    integer :: ierr
    integer i, j, ij, myrank, k , iter, ivec
    integer :: lon_ibeg, lon_iend, lat_ibeg, lat_iend, lev_ibeg, lev_iend, mem_num
    real, dimension(:), pointer :: revdata
    real, allocatable, dimension(:,:,:,:) :: nu,nv,ngd

    integer ,  dimension(10) :: var_lev      
    character(5) , dimension(10) :: var_name
    integer  :: n_var
    integer  :: vb,ve

    call MPI_Comm_rank(MPI_ATM_GROUP, myrank, ierr)
  
    mesh => state%mesh

    lon_ibeg = mesh%full_ids
    lon_iend = mesh%full_ide
    lat_ibeg = mesh%full_jds
    lat_iend = mesh%full_jde
    lev_ibeg = mesh%full_kds
    lev_iend = mesh%full_kde

    mem_num =member_total

    ! call stringsplit_ATM(da_var_name,',',var_name,n_var)
    ! do i = 1 , n_var
    !   if (var_name(i) == 'ps') then 
    !     var_lev(i) = 1
    !   else
    !     var_lev(i) = num_lev
    !   end if
    ! end do

    n_var = 4

    var_name(2) = "u"; var_name(3) = "v"; var_name(4) = "pt" ; var_name(1) = 'ps'
    var_lev(2) = num_lev; var_lev(3) = num_lev; var_lev(4) = num_lev ; var_lev(1) = 1
    
    call AttrVect_zero(ATM_AV, .true., .true.)
    call MCT_Recv(ATM_AV, R_ATM2DA, Sum=.false.)

    allocate(revdata(1:avsize))

    do iter = 1 , n_var
      select case(var_name(iter))
      case('u')
        call AttrVect_exportRAttr(ATM_AV,"u",revdata)
        ij=1
        do k = lev_ibeg, lev_iend
          do j = lat_ibeg, lat_iend
            do i = lon_ibeg, lon_iend
              do ivec = 1 , 1
                state%tmp(:, i, j, k) = revdata(ij:ij+member_num-1)
                ij = ij + member_num
              end do
            end do
          end do
        end do
        do ivec = 1 , 1
          call fill_halo_member(block, state%tmp, full_lon=.true., full_lat=.true., full_lev=.true., async=state%async(async_tmp))
          call state%async(async_tmp)%wait()
          
          do k = lev_ibeg, lev_iend
            do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
              do i = mesh%half_ids, mesh%half_ide
                state%u_lon(:,i,j,k) = (state%tmp(:,i,j,k) + state%tmp(:,i+1,j,k))/2
                !state%u(:,i,j,k) = state%tmp(:,i,j,k)
              end do
            end do
          end do
          call fill_halo_member(block, state%u_lon, full_lon=.false., full_lat=.true., full_lev=.true., async=state%async(async_u_lon))
          call state%async(async_u_lon)%wait()
        end do

      case('v')
        call AttrVect_exportRAttr(ATM_AV,"v",revdata)
        ij=1
        do k = lev_ibeg, lev_iend
          do j = lat_ibeg, lat_iend
            do i = lon_ibeg, lon_iend
              do ivec = 1 , 1
                state%tmp(:, i, j, k) = revdata(ij:ij+member_num-1)
                ij = ij + member_num
              end do
            end do
          end do
        end do

        do ivec = 1 , 1
          call fill_halo_member(block, state%tmp, full_lon=.true., full_lat=.true., full_lev=.true., async=state%async(async_tmp))
          call state%async(async_tmp)%wait()
      
          do k = lev_ibeg, lev_iend
            do j = mesh%half_jds, mesh%half_jde
              do i = mesh%full_ids, mesh%full_ide
                state%v_lat(:,i,j,k) = 2 * (state%tmp(:,i,j,k)*mesh%area_subcell(2,j) + state%tmp(:,i,j+1,k)*mesh%area_subcell(1,j+1))/mesh%area_vtx(j)
                !state%v(:,i,j,k) = state%tmp(:,i,j,k)
              end do
            end do
          end do
          call fill_halo_member(block, state%v_lat, full_lon=.true., full_lat=.false., full_lev=.true., async=state%async(async_v_lat))
          call state%async(async_v_lat)%wait()
        end do
    
      case('pt')
        call AttrVect_exportRAttr(ATM_AV,"pt",revdata)
        ij=1
        do k = lev_ibeg, lev_iend
          do j = lat_ibeg, lat_iend
            do i = lon_ibeg, lon_iend
              do ivec = 1 , 1
                state%pt(:, i, j, k) = revdata(ij:ij+member_num-1)
                ij = ij + member_num
              end do
            end do
          end do
        end do

        do ivec = 1 , 1
          call fill_halo_member(block, state%pt, full_lon=.true., full_lat=.true., full_lev=.true., async=state%async(async_pt))
          call state%async(async_pt)%wait()
        end do

      case('ps')
        call AttrVect_exportRAttr(ATM_AV,"ps",revdata)
        ij=1
        do k = 1, 1
          do j = lat_ibeg, lat_iend
            do i = lon_ibeg, lon_iend
              do ivec = 1 , 1
                state%phs(:, i, j) = revdata(ij:ij+member_num-1)
                ij = ij + member_num
              end do
            end do
          end do
        end do

        do ivec = 1 , 1
          call fill_halo_member(block, state%phs, full_lon=.true., full_lat=.true., async=state%async(async_phs))  
          call state%async(async_phs)%wait() 
        end do 
      end select
    end do

    deallocate(revdata)

  end subroutine
  
  subroutine clean_gmcore_coupler()
  
    implicit none
    deallocate(avdata)

    call Router_clean(R_ATM2DA)
    call AttrVect_clean(ATM_AV)
    call GlobalSegMap_clean(ATM_GSMap)
    call MCTWorld_clean()
  
  end subroutine 
  
end module gmcore_coupler
  
