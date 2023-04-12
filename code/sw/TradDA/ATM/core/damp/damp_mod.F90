module damp_mod

  use flogger
  use string
  use const_mod
  use namelist_mod
  use parallel_mod
  use parallel_types_mod
  use block_mod
  use operators_mod
  use div_damp_mod
  use smag_damp_mod
  use laplace_damp_mod
  use pole_damp_mod
  use filter_mod
  use pa_mod


  implicit none

  private

  public damp_init
  public damp_run
  public damp_final


contains

  subroutine damp_init(blocks)

    type(block_type), intent(in) :: blocks(:)

    call div_damp_init(blocks)
    call smag_damp_init()
    call laplace_damp_init()

  end subroutine damp_init

  subroutine damp_final()

    call div_damp_final()
    call smag_damp_final()
    call laplace_damp_final()

  end subroutine damp_final

  subroutine damp_run(block, dstate, dtend, dt)

    type(block_type), intent(inout) :: block
    type(dstate_type), intent(inout) :: dstate
    type(dtend_type), intent(inout) :: dtend
    real(r8), intent(in) :: dt

#ifdef Detail_Time
    call Add_Function("damp_run")
    call Indent_In()
#endif

    if (use_div_damp) then
      call div_damp_run(block, dstate)
    end if
    if (use_smag_damp) then
      call smag_damp_run(block, dt, dtend, dstate)
    end if
    call pole_damp_run(block, dstate)

#ifdef Detail_Time
    call Indent_Out()
#endif

  end subroutine damp_run

end module damp_mod
