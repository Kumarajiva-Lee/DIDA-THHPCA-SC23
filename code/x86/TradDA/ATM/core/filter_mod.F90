module filter_mod

    use const_mod
    use block_mod
    use parallel_mod
    use filter_types_mod
    use namelist_mod
    use pa_mod
  
    implicit none
  
    private
  
    public filter_type
    public filter_on_cell
    public filter_on_lon_edge
    public filter_on_lat_edge
    public filter_on_lev_edge

  
    interface filter_on_cell
      module procedure filter_on_cell_2d
      module procedure filter_on_cell_3d
    end interface filter_on_cell
  
  contains
  
    subroutine filter_on_cell_2d(filter, x, y)
  
      type(filter_type), intent(in) :: filter
      real(r8), intent(inout) :: x(member_num ,&
                                   filter%mesh%full_ims:filter%mesh%full_ime, &
                                   filter%mesh%full_jms:filter%mesh%full_jme)
      real(r8), intent(out), optional :: y(member_num ,&
                                           filter%mesh%full_ims:filter%mesh%full_ime, &
                                           filter%mesh%full_jms:filter%mesh%full_jme)
  
      real(r8) tmp(member_num ,filter%mesh%full_ims:filter%mesh%full_ime)
      integer i, j, n, hn, im
  
#ifdef Detail_Time
      call Get_Start_Time(cal_time_start)
      call Add_Function("filter_on_cell_2d")
      call Indent_In()
#endif

      associate (mesh => filter%mesh)
      do im = 1 , member_num
        do j = mesh%full_jds, mesh%full_jde
            if (filter%ngrid_lon(j) > 1) then
            n  = filter%ngrid_lon(j)
            hn = (n - 1) / 2
            do i = mesh%full_ids, mesh%full_ide
                tmp(im,i) = sum(filter%wgt_lon(:n,j) * x(im,i-hn:i+hn,j))
            end do
            if (present(y)) then
                y(im,:,j) = tmp(im,:)
            else
                x(im,:,j) = tmp(im,:)
            end if
            else if (present(y)) then
            y(im,:,j) = x(im,:,j)
            end if
        end do
      end do
      end associate
#ifdef Detail_Time
      call Get_End_Time(cal_time_end)
      cal_time = cal_time + cal_time_end - cal_time_start
      fliter_time = fliter_time + cal_time_end - cal_time_start
      CALL Indent_Out()
#endif
  
    end subroutine filter_on_cell_2d
  
    subroutine filter_on_cell_3d(filter, x, y)
  
      type(filter_type), intent(in) :: filter
      real(r8), intent(inout) :: x(member_num , &
                                   filter%mesh%full_ims:filter%mesh%full_ime, &
                                   filter%mesh%full_jms:filter%mesh%full_jme, &
                                   filter%mesh%full_kms:filter%mesh%full_kme)
      real(r8), intent(out), optional :: y(member_num , &
                                           filter%mesh%full_ims:filter%mesh%full_ime, &
                                           filter%mesh%full_jms:filter%mesh%full_jme, &
                                           filter%mesh%full_kms:filter%mesh%full_kme)
      
  
      real(r8) tmp(member_num,filter%mesh%full_ims:filter%mesh%full_ime)
      integer i, j, k, n, hn, im
  
#ifdef Detail_Time
      call Get_Start_Time(cal_time_start)
      call Add_Function("filter_on_cell_3d")
      call Indent_In()
#endif
      associate (mesh => filter%mesh)

      
      
      do im = 1 , member_num
        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds, mesh%full_jde
            
            if (filter%ngrid_lon(j) > 1) then
              n  = filter%ngrid_lon(j)
              hn = (n - 1) / 2
              do i = mesh%full_ids, mesh%full_ide
                tmp(im,i) = sum(filter%wgt_lon(:n,j) * x(im,i-hn:i+hn,j,k))
              end do
              if (present(y)) then
                y(im,:,j,k) = tmp(im,:)
              else
                x(im,:,j,k) = tmp(im,:)
              end if
            else if (present(y)) then
              y(im,:,j,k) = x(im,:,j,k)
            end if
          end do
        end do
      end do
      end associate
#ifdef Detail_Time
      call Get_End_Time(cal_time_end)
      cal_time = cal_time + cal_time_end - cal_time_start
      fliter_time = fliter_time + cal_time_end - cal_time_start
      call Indent_Out()
#endif
  
    end subroutine filter_on_cell_3d
       
    subroutine filter_on_lon_edge(filter, x, y)
  
      type(filter_type), intent(in) :: filter
      real(r8), intent(inout) :: x(member_num ,&
                                   filter%mesh%half_ims:filter%mesh%half_ime, &
                                   filter%mesh%full_jms:filter%mesh%full_jme, &
                                   filter%mesh%full_kms:filter%mesh%full_kme)
      real(r8), intent(out), optional :: y(member_num ,&
                                           filter%mesh%half_ims:filter%mesh%half_ime, &
                                           filter%mesh%full_jms:filter%mesh%full_jme, &
                                           filter%mesh%full_kms:filter%mesh%full_kme)
  
      real(r8) tmp(member_num ,filter%mesh%half_ims:filter%mesh%half_ime)
      integer i, j, k, n, hn, im
  
      associate (mesh => filter%mesh)
#ifdef Detail_Time
      call Get_Start_Time(cal_time_start)
      call Add_Function("filter_on_lon_edge")
      call Indent_In()
#endif
      do im = 1 , member_num
        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%full_jds, mesh%full_jde
            if (filter%ngrid_lon(j) > 1) then
              n  = filter%ngrid_lon(j)
              hn = (n - 1) / 2
              do i = mesh%half_ids, mesh%half_ide
                tmp(im,i) = sum(filter%wgt_lon(:n,j) * x(im,i-hn:i+hn,j,k))
              end do
              if (present(y)) then
                y(im,:,j,k) = tmp(im,:)
              else
                x(im,:,j,k) = tmp(im,:)
              end if
            else if (present(y)) then
                y(im,:,j,k) = x(im,:,j,k)
            end if
          end do
        end do
      end do
#ifdef Detail_Time
      call Get_End_Time(cal_time_end)
      cal_time = cal_time + cal_time_end - cal_time_start
      fliter_time = fliter_time + cal_time_end - cal_time_start
      call Indent_Out()
#endif
      end associate
  
    end subroutine filter_on_lon_edge
  
    subroutine filter_on_lat_edge(filter, x, y)
  
      type(filter_type), intent(in) :: filter
      real(r8), intent(inout) :: x(member_num ,&
                                   filter%mesh%full_ims:filter%mesh%full_ime, &
                                   filter%mesh%half_jms:filter%mesh%half_jme, &
                                   filter%mesh%full_kms:filter%mesh%full_kme)
      real(r8), intent(out), optional :: y(member_num ,&
                                           filter%mesh%full_ims:filter%mesh%full_ime, &
                                           filter%mesh%half_jms:filter%mesh%half_jme, &
                                           filter%mesh%full_kms:filter%mesh%full_kme)
  
      real(r8) tmp(member_num,filter%mesh%full_ims:filter%mesh%full_ime)
      integer i, j, k, n, hn, im
  
      associate (mesh => filter%mesh)
#ifdef Detail_Time
      call Get_Start_Time(cal_time_start)
      call Add_Function("filter_on_lat_edge")
      call Indent_In()
#endif
      do im = 1 , member_num
        do k = mesh%full_kds, mesh%full_kde
          do j = mesh%half_jds, mesh%half_jde
            if (filter%ngrid_lat(j) > 1) then
                n  = filter%ngrid_lat(j)
                hn = (n - 1) / 2
                do i = mesh%full_ids, mesh%full_ide
                tmp(im,i) = sum(filter%wgt_lat(:n,j) * x(im,i-hn:i+hn,j,k))
                end do
                if (present(y)) then
                y(im,:,j,k) = tmp(im,:)
                else
                x(im,:,j,k) = tmp(im,:)
                end if
            else if (present(y)) then
                y(im,:,j,k) = x(im,:,j,k)
            end if
          end do
        end do
      end do
#ifdef Detail_Time
      call Get_End_Time(cal_time_end)
      cal_time = cal_time + cal_time_end - cal_time_start
      fliter_time = fliter_time + cal_time_end - cal_time_start
      call Indent_Out()
#endif
      end associate
  
    end subroutine filter_on_lat_edge
  
    subroutine filter_on_lev_edge(filter, x, y)
  
      type(filter_type), intent(in) :: filter
      real(r8), intent(inout) :: x(member_num ,&
                                   filter%mesh%full_ims:filter%mesh%full_ime, &
                                   filter%mesh%full_jms:filter%mesh%full_jme, &
                                   filter%mesh%half_kms:filter%mesh%half_kme)
      real(r8), intent(out), optional :: y(member_num ,&
                                           filter%mesh%full_ims:filter%mesh%full_ime, &
                                           filter%mesh%full_jms:filter%mesh%full_jme, &
                                           filter%mesh%half_kms:filter%mesh%half_kme)
  
      real(r8) tmp(member_num ,filter%mesh%full_ims:filter%mesh%full_ime)
      integer i, j, k, n, hn, im
  
#ifdef Detail_Time
      call Get_Start_Time(cal_time_start)
      call Add_Function("filter_on_lev_edge")
      call Indent_In()
#endif
      associate (mesh => filter%mesh)
      do im = 1 , member_num
        do k = mesh%half_kds, mesh%half_kde
          do j = mesh%full_jds, mesh%full_jde
            if (filter%ngrid_lon(j) > 1) then
                n  = filter%ngrid_lon(j)
                hn = (n - 1) / 2
                do i = mesh%full_ids, mesh%full_ide
                tmp(im,i) = sum(filter%wgt_lon(:n,j) * x(im,i-hn:i+hn,j,k))
                end do
                if (present(y)) then
                y(im,:,j,k) = tmp(im,:)
                else
                x(im,:,j,k) = tmp(im,:)
                end if
            else if (present(y)) then
                y(im,:,j,k) = x(im,:,j,k)
            end if
          end do
        end do
      end do
      end associate
#ifdef Detail_Time
      call Get_End_Time(cal_time_end)
      cal_time = cal_time + cal_time_end - cal_time_start
      fliter_time = fliter_time + cal_time_end - cal_time_start
      call Indent_Out()
#endif
  
    end subroutine filter_on_lev_edge  
  
  end module filter_mod
  