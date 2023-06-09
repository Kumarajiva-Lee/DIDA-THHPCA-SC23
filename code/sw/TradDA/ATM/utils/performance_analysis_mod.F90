module pa_mod
  use mpi
  use parallel_types_mod
  use time_mod

  implicit none



  integer myid
  character*50 file_name              !output file name
  character*50 detail_file_name       !optional output file name
  character*50 stencil_time_file      !stencil_time output file
  character*50 flowgraph_file_name   !para graph output file
  character*50 why
  logical pa_operator_prepare_flag    !whether do time counting
  logical pa_space_operator_flag
  logical pa_reduce_flag
  logical pa_flag

  real*8    sub_time_start , sub_time_end , cal_time_start , cal_time_end 
  real*8    tran_time_start , tran_time_end ,step_time_start , step_time_end
  real*8    send_time_start, send_time_end, ffsl_time_start, ffsl_time_end
  real*8    sub_time , cal_time , tran_time
  real*8    time_start , time_end 
  real*8    detail_time_start , detail_time_end
  real*8    wait_time_start , wait_time_end , wait_time
  real*8    total_time_start , total_time_end
  real*8    lin_time, fliter_time, ffsl_time, send_time
  real*8    stencil_time(20)
  real*8    stencil_time_start,stencil_time_end,stencil_total

  integer   indent



contains

subroutine pa_init(comm , group_id_in)
  
  integer, intent(in), optional :: comm
  integer, intent(in), optional :: group_id_in

  integer ierr
  integer group_id
  integer proc_np , proc_id
  character*10 date
  character*10 time
  character*10 zone
  integer date_time(8)
  character*10 s_id 
  character*50 command_cd , command_mk
  logical istatus1,istatus2
  integer i


  if (present(group_id_in)) then
    group_id = group_id_in
  else
    group_id = 0
  endif
  
  call MPI_COMM_SIZE(comm, proc_np, ierr)
  call MPI_COMM_RANK(comm, proc_id, ierr)
  myid = proc_np * group_id + proc_id

#ifdef Detail_Time
  write(s_id,"(i6.6)") myid
  file_name(1:7)  = 'detail_'
  file_name(8:13) = s_id
  file_name(14:17) = '.txt'
  open(unit=(10000+myid),POSITION='APPEND',file=file_name)

  if (is_root_proc_pa()) then
    flowgraph_file_name(1:13) = 'flowgraph.txt'
    open(unit=(1234),POSITION='APPEND',file=flowgraph_file_name)
    indent = 0
  end if
  ! stencil_time_file(1:13) = 'stencil_time_'
  ! stencil_time_file(14:19) = s_id
  ! stencil_time_file(20:23) = '.txt'
  ! open(unit=(1000000+myid),POSITION='APPEND',file=stencil_time_file)

#endif

! #ifdef Detail_Calc_Time
!   detail_file_name(1:17)  = 'detail_calc_time_'
!   detail_file_name(18:23) = s_id
!   detail_file_name(24:27) = '.txt'
!   open(unit=(100000+myid),POSITION='APPEND',file=detail_file_name)
! #endif 

!   wait_time = 0
!   do i = 1 , 20 
!     stencil_time(i) = 0
!   end do

  

  ! file_name(1:21)  = 'mpi_stateupdate_time_'
  ! file_name(22:25) = s_id
  ! file_name(26:29) = '.txt'
  ! open(unit=(20000+myid),POSITION='APPEND',file=file_name)

  call Get_Time_Init()


end subroutine pa_init

subroutine Indent_In()
  indent = indent + 2
end subroutine Indent_In

subroutine Indent_Out()
  indent = indent - 2
end subroutine Indent_Out

subroutine Add_Function(funcname)
  character(*), intent(in) :: funcname

  character*100 :: line 

  integer i

  if (.not. is_root_proc_pa()) return
  if (time_step > 1) return

  line(:) = ""

  do i = 1 , indent
    line(i:i) = " " 
  end do

  do i = 1 , len(funcname)
    line(i + indent:i + indent) = funcname(i:i)
  end do

  if (time_step == 0) then
    line(len(funcname) + indent + 1 : len(funcname) + indent + 2) = " 0"
  else
    line(len(funcname) + indent + 1 : len(funcname) + indent + 2) = " 1"
  end if

  write(1234,*) , line


end subroutine Add_Function

subroutine Get_Time_Pa(get_time)
  real*8 , intent(inout) :: get_time

  get_time = mpi_wtime();
end subroutine Get_Time_Pa

subroutine Get_Start_Time(get_time)
  real*8 , intent(inout) :: get_time

  get_time = mpi_wtime();
end subroutine Get_Start_Time

subroutine Get_End_Time(get_time)
  real*8 , intent(inout) :: get_time

  get_time = mpi_wtime();
end subroutine Get_End_Time

subroutine Get_Time_Init()
  cal_time = 0
  tran_time = 0
  fliter_time = 0
  send_time = 0
  ffsl_time = 0
  wait_time = 0
end subroutine Get_Time_Init

subroutine pa_final()

  integer i

  call Get_Time_Pa(time_end)

#ifdef Detail_Time
  write((10000+myid),*) , total_time_end - total_time_start , cal_time , tran_time, fliter_time, send_time, ffsl_time, tran_time - send_time
  close((10000+myid))
  if (is_root_proc_pa()) then
    close(1234)
  end if
#endif

  ! do i = 1 , 12 
  !   stencil_total = stencil_total + stencil_time(i)
  ! end do
  ! write((1000000+myid),*) , stencil_total
  ! do i = 1 , 12 
  !   write((1000000+myid),*) , i, stencil_time(i) , stencil_time(i) / stencil_total
  ! end do
  !  close((1000000+myid))


#ifdef Detail_Calc_Time
  close((100000+myid))
#endif

end subroutine pa_final

end module pa_mod
