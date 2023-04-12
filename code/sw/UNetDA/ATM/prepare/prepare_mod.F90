module prepare_mod

  use const_mod
  use namelist_mod
  use parallel_mod
  use process_mod
  use block_mod
  use topo_mod
  use bkg_mod
  use initial_mod
  use operators_mod
  use ref_mod
  use parallel_types_mod
  use member_mod


contains


  subroutine prepare_static(block)

    class(block_type), intent(inout) :: block

    integer i, j

    associate (mesh    => block%mesh                , &
               dstate  => block%dstate(1)            , & 
               gzs     => block%static%gzs      , & ! in
               dzsdlon => block%static%dzsdlon  , & ! out
               dzsdlat => block%static%dzsdlat)     ! out
      do j = mesh%full_jds_no_pole, mesh%full_jde_no_pole
        do i = mesh%half_ids, mesh%half_ide
          dzsdlon(:,i,j) = (gzs(:,i+1,j) - gzs(:,i,j)) / g / mesh%de_lon(j)
        end do
      end do
      do j = mesh%half_jds, mesh%half_jde
        do i = mesh%full_ids, mesh%full_ide
          dzsdlat(:,i,j) = (gzs(:,i,j+1) - gzs(:,i,j)) / g / mesh%de_lat(j)
        end do
      end do
      call fill_halo_member(block, dzsdlon, full_lon=.false., full_lat=.true. , async=dstate%async(async_dzsdlon))
      call fill_halo_member(block, dzsdlat, full_lon=.true. , full_lat=.false., async=dstate%async(async_dzsdlat))

      call dstate%async(async_dzsdlon)%wait()
      call dstate%async(async_dzsdlat)%wait()

    end associate

  end subroutine prepare_static

  subroutine prepare_run(group_id_in)

    integer , intent(in), optional :: group_id_in

    integer iblk, i , im ,iter_file
    integer group_id
    integer ini_minute , ini_hour , ini_day , ini_month , ini_year
    integer mpas_len , gmcore_len
    character(256):: bkg_file_local
    character(30) :: s_id

    group_id = merge(group_id_in , 0 , present(group_id_in))


    ! call topo_read(topo_file)
    ! do iblk = 1, size(blocks)
    !   call topo_regrid(blocks(iblk))
    ! end do
  
    ! if (use_topo_smooth) then
    !   do iblk = 1, size(blocks)
    !     call topo_smooth(blocks(iblk))
    !   end do
    ! end if

    bkg_file_local = bkg_file
    mpas_len = len_trim(bkg_file_local)
    ! write(*,*) bkg_file_local , mpas_len
    read(bkg_file_local(mpas_len - 21 : mpas_len - 18) , *) ini_year
    read(bkg_file_local(mpas_len - 16 : mpas_len - 15) , *) ini_month
    read(bkg_file_local(mpas_len - 13 : mpas_len - 12) , *) ini_day
    read(bkg_file_local(mpas_len - 10 : mpas_len -  9) , *) ini_hour
    read(bkg_file_local(mpas_len -  7 : mpas_len -  6) , *) ini_minute

    call date_advance(ini_year , ini_month , ini_day , ini_hour , ini_minute , initial_interval * group_id * member_total) 
    call date_advance(ini_year , ini_month , ini_day , ini_hour , ini_minute , initial_interval * (ivector - 1) * member_num) 

    write(s_id,"(i4.4)") ini_year
    bkg_file_local(mpas_len - 21 : mpas_len - 18) = s_id
    s_id = 'N/A'

    write(s_id,"(i2.2)") ini_month
    bkg_file_local(mpas_len - 16 : mpas_len - 15) = s_id
    s_id = 'N/A'
    
    write(s_id,"(i2.2)") ini_day
    bkg_file_local(mpas_len - 13 : mpas_len - 12) = s_id
    s_id = 'N/A'

    write(s_id,"(i2.2)") ini_hour
    bkg_file_local(mpas_len - 10 : mpas_len - 9)  = s_id
    s_id = 'N/A'

    write(s_id,"(i2.2)") ini_minute
    bkg_file_local(mpas_len - 7  : mpas_len - 6)  = s_id
    s_id = 'N/A'

    do im = 1 , member_num

      if ( (ivector - 1) * member_num + im > member_total ) exit
  
      call bkg_read(bkg_type, bkg_file_local)
    
      call bkg_regrid_phs(im)
      call bkg_calc_ph(im)
      call bkg_regrid_pt(im)
      call bkg_regrid_u(im)
      call bkg_regrid_v(im)

      call date_advance(ini_year , ini_month , ini_day , ini_hour , ini_minute, initial_interval)

      write(s_id,"(i4.4)") ini_year
      bkg_file_local(mpas_len - 21 : mpas_len - 18) = s_id
      s_id = 'N/A'

      write(s_id,"(i2.2)") ini_month
      bkg_file_local(mpas_len - 16 : mpas_len - 15) = s_id
      s_id = 'N/A'
      
      write(s_id,"(i2.2)") ini_day
      bkg_file_local(mpas_len - 13 : mpas_len - 12) = s_id
      s_id = 'N/A'

      write(s_id,"(i2.2)") ini_hour
      bkg_file_local(mpas_len - 10 : mpas_len - 9)  = s_id
      s_id = 'N/A'

      write(s_id,"(i2.2)") ini_minute
      bkg_file_local(mpas_len - 7  : mpas_len - 6)  = s_id
      s_id = 'N/A'

    end do

    ! if (nonhydrostatic) then
    !   do iblk = 1, size(blocks)
    !     call diag_ph    (blocks(iblk), blocks(iblk)%dstate(1))
    !     call diag_t     (blocks(iblk), blocks(iblk)%dstate(1))
    !     call diag_gz_lev(blocks(iblk), blocks(iblk)%dstate(1))
    !   end do
    ! end if

  end subroutine prepare_run

  subroutine prepare_final()

    call topo_final()
    call bkg_final()

  end subroutine prepare_final

end module prepare_mod
