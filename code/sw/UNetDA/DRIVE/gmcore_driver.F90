program gmcore_driver
  use mpi
  use log_mod
  use namelist_mod
  use block_mod
  use parallel_mod
  use restart_mod
  use gmcore_mod
  !use steady_state_test_mod
  !use rossby_haurwitz_wave_3d_test_mod
  !use mountain_wave_test_mod
  !use baroclinic_wave_test_mod
  !use held_suarez_test_mod

  implicit none

  character(256) namelist_path
  integer iblk
  integer :: ierr , comm

  ! interface
  !   subroutine set_initial_condition_interface(block)
  !     import block_type
  !     type(block_type), intent(inout), target :: block
  !   end subroutine set_initial_condition_interface
  ! end interface
  ! procedure(set_initial_condition_interface), pointer :: set_initial_condition

  call get_command_argument(1, namelist_path)
  if (namelist_path == '') then
    call log_error('You should give a namelist file path!')
  end if
  
  call MPI_INIT(ierr)
  comm = MPI_COMM_WORLD
  call atm_init(namelist_path , comm)

  ! select case (test_case)
  ! case ('steady_state')
  !   set_initial_condition => steady_state_test_set_initial_condition
  ! case ('rossby_haurwitz_wave')
  !   set_initial_condition => rossby_haurwitz_wave_3d_test_set_initial_condition
  ! case ('mountain_wave')
  !   set_initial_condition => mountain_wave_test_set_initial_condition
  ! case ('baroclinic_wave')
  !   set_initial_condition => baroclinic_wave_test_set_initial_condition
  ! case ('held_suarez')
  !   set_initial_condition => held_suarez_test_set_initial_condition
  ! case default
  !   call log_error('Unknown test case ' // trim(test_case) // '!')
  ! end select

  ! if (restart) then
  !   call restart_read(proc%blocks, old_time_idx , proc%member_num)
  ! else
  !   do iblk = 1, size(proc%blocks)
  !     call set_initial_condition(proc%blocks(iblk))
  !   end do
  ! end if

  ! write(*,*) "I'm over set_initial_condition"

  call atm_run(comm)

  call atm_final()

end program gmcore_driver
