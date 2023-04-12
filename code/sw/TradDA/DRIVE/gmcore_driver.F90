program gmcore_driver
  use mpi
  use log_mod
  use namelist_mod
  use block_mod
  use parallel_mod
  use restart_mod
  use gmcore_mod

  implicit none

  character(256) namelist_path
  integer iblk
  integer :: ierr , comm

  call get_command_argument(1, namelist_path)
  if (namelist_path == '') then
    call log_error('You should give a namelist file path!')
  end if
  
  call MPI_INIT(ierr)
  comm = MPI_COMM_WORLD
  call atm_init(namelist_path , comm)

  call atm_run(comm)

  call atm_final()

end program gmcore_driver
