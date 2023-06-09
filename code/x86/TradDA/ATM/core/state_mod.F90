module state_mod

  use const_mod
  use namelist_mod
  use mesh_mod
  use allocator_mod
  use parallel_types_mod

  implicit none

  private

  public state_type

  ! NOTE:
  !   Variables with '_lon', '_lat' and '_lev' are on the half grids on the corresponding direction,
  !   and '_p' indicates that the variable is perturbed.
  type state_type
    type(mesh_type), pointer :: mesh => null()
    ! For nesting
    integer :: id = 0
    type(state_type), pointer :: parent => null()
    real(r8), allocatable, dimension(:,:,:,:) :: u             ! Zonal wind speed (m s-1)
    real(r8), allocatable, dimension(:,:,:,:) :: u_f           ! Zonal wind speed (m s-1)
    real(r8), allocatable, dimension(:,:,:,:) :: u_Agrid       ! u on A grid
    real(r8), allocatable, dimension(:,:,:,:) :: v             ! Meridional wind speed (m s-1)
    real(r8), allocatable, dimension(:,:,:,:) :: v_f           ! Meridional wind speed (m s-1)
    real(r8), allocatable, dimension(:,:,:,:) :: v_Agrid       ! v on A grid
    
    real(r8), allocatable, dimension(:,:,:,:) :: wedphdlev_lev     ! Vertical coordinate speed multiplied by 𝛛π/𝛛η
    real(r8), allocatable, dimension(:,:,:,:) :: wedphdlev_lev_lon ! Vertical coordinate speed multiplied by 𝛛π/𝛛η on zonal edge
    real(r8), allocatable, dimension(:,:,:,:) :: wedphdlev_lev_lat ! Vertical coordinate speed multiplied by 𝛛π/𝛛η on merdional edge
    
    real(r8), allocatable, dimension(:,:,:,:) :: gz                ! Geopotential (m2 s-2)
    real(r8), allocatable, dimension(:,:,:,:) :: gz_f              ! Geopotential (m2 s-2)
    real(r8), allocatable, dimension(:,:,:,:) :: gz_lev            ! Geopotential height on half levels (m2 s-2)
    
    real(r8), allocatable, dimension(:,:,:,:) :: m                 ! Mass
    real(r8), allocatable, dimension(:,:,:,:) :: m_vtx             ! Mass on vertex
    real(r8), allocatable, dimension(:,:,:,:) :: m_lon             ! Mass on zonal edge
    real(r8), allocatable, dimension(:,:,:,:) :: m_lat             ! Mass on merdional edge
    real(r8), allocatable, dimension(:,:,:,:) :: mf_lon_n          ! Normal mass flux on zonal edge
    real(r8), allocatable, dimension(:,:,:,:) :: mf_lat_n          ! Normal mass flux on merdional edge
    real(r8), allocatable, dimension(:,:,:,:) :: mf_lat_t          ! Tangient mass flux on zonal edge
    real(r8), allocatable, dimension(:,:,:,:) :: mf_lon_t          ! Tangient mass flux on merdional edge
    
    real(r8), allocatable, dimension(:,:,:,:) :: pv                ! Potential vorticity
    real(r8), allocatable, dimension(:,:,:,:) :: pv_lon            ! Potential vorticity on zonal edge
    real(r8), allocatable, dimension(:,:,:,:) :: pv_lat            ! Potential vorticity on merdional edge

    real(r8), allocatable, dimension(:,:,:,:) :: ke                ! Kinetic energy

    real(r8), allocatable, dimension(:,:,:,:) :: pt                ! Potential temperature
    real(r8), allocatable, dimension(:,:,:,:) :: pt_f              ! Potential temperature
    real(r8), allocatable, dimension(:,:,:,:) :: pt_lon            ! Potential temperature on the zonal edge
    real(r8), allocatable, dimension(:,:,:,:) :: pt_lat            ! Potential temperature on the merdional edge
    real(r8), allocatable, dimension(:,:,:,:) :: pt_lev            ! Potential temperature on the vertical edge
    
    real(r8), allocatable, dimension(:,:,:,:) :: t                 ! Temperature        ! ak T
    
    real(r8), allocatable, dimension(:,:,:,:) :: ph                ! Hydrostatic pressure on full levels
    real(r8), allocatable, dimension(:,:,:,:) :: ph_lev            ! Hydrostatic pressure on half levels
    real(r8), allocatable, dimension(:,:,:  ) :: phs               ! Surface hydrostatic pressure
    real(r8), allocatable, dimension(:,:,:  ) :: phs_f             ! Surface hydrostatic pressure
    
    real(r8), allocatable, dimension(:,:,:,:) :: div               ! Divergence (s-1)
    real(r8), allocatable, dimension(:,:,:,:) :: div2              ! Laplacian of divergence (s-1)
    real(r8), allocatable, dimension(:,:,:,:) :: vor               ! Vorticity (s-1)

    ! Nonhydrostatic variables
    real(r8), allocatable, dimension(:,:,:,:) :: m_lev
    real(r8), allocatable, dimension(:,:,:,:) :: wedphdlev
    real(r8), allocatable, dimension(:,:,:,:) :: w                 ! Vertical wind speed
    real(r8), allocatable, dimension(:,:,:,:) :: w_lev             ! Vertical wind speed
    real(r8), allocatable, dimension(:,:,:,:) :: w_lev_lon         ! Vertical wind speed
    real(r8), allocatable, dimension(:,:,:,:) :: w_lev_lat         ! Vertical wind speed
    real(r8), allocatable, dimension(:,:,:,:) :: gz_lev_lon        ! Geopotential
    real(r8), allocatable, dimension(:,:,:,:) :: gz_lev_lat        ! Geopotential
    real(r8), allocatable, dimension(:,:,:,:) :: rhod              ! Dry air density
    real(r8), allocatable, dimension(:,:,:,:) :: rhod_lon          ! Dry air density
    real(r8), allocatable, dimension(:,:,:,:) :: rhod_lat          ! Dry air density
    real(r8), allocatable, dimension(:,:,:,:) :: p                 ! Pressure on full levels
    real(r8), pointer    , dimension(:,:,:,:) :: p_lev             ! Pressure on half levels
    real(r8), allocatable, dimension(:,:,:,:) :: p_lev_lon         ! Pressure on half levels
    real(r8), allocatable, dimension(:,:,:,:) :: p_lev_lat         ! Pressure on half levels
    real(r8), allocatable, dimension(:,:,:,:) :: u_lev_lon
    real(r8), allocatable, dimension(:,:,:,:) :: v_lev_lat
    real(r8), allocatable, dimension(:,:,:,:) :: mf_lev_lon_n      ! Mass flux on zonal edge and half level
    real(r8), allocatable, dimension(:,:,:,:) :: mf_lev_lat_n      ! Mass flux on merdional edge and half level
    ! Smagorinsky damping variables
    real(r8), allocatable, dimension(:,:,:,:) :: tension_h         ! tension strain
    real(r8), allocatable, dimension(:,:,:,:) :: shear_h           ! shear strain on vertex
    real(r8), allocatable, dimension(:,:,:,:) :: kmh               ! nonlinear diffusion coef
    real(r8), allocatable, dimension(:,:,:,:) :: kmh_vtx           ! nonlinear diffusion coef on vertex
    real(r8), allocatable, dimension(:,:,:,:) :: kmh_lon           ! nonlinear diffusion coef on zonal edge
    real(r8), allocatable, dimension(:,:,:,:) :: kmh_lat           ! nonlinear diffusion coef on meridional edge

    real(r8), allocatable, dimension(:) :: tm
    real(r8), allocatable, dimension(:) :: te
    real(r8), allocatable, dimension(:) :: tpe
    real(r8), allocatable, dimension(:) :: tav
    type(async_type), allocatable :: async(:)

    real(r8), allocatable, dimension(:,:,:,:) :: test          !test halo
    real(r8), allocatable, dimension(:,:,:,:) :: tmp           !for MCT A->C
  contains
    procedure :: init => state_init
    procedure :: clear => state_clear
    final :: state_final
  end type state_type

contains

  subroutine state_init(this, mesh)

    class(state_type), intent(inout), target :: this
    type(mesh_type  ), intent(in   ), target :: mesh
    !integer , intent(in)  :: member_num

    call this%clear()

    this%mesh => mesh

    call allocate_array(mesh, this%u                , member_num , half_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%u_f              , member_num , half_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%u_Agrid          , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%v                , member_num , full_lon=.true., half_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%v_f              , member_num , full_lon=.true., half_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%v_Agrid          , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    
    call allocate_array(mesh, this%wedphdlev_lev    , member_num , full_lon=.true., full_lat=.true., half_lev=.true.)
    call allocate_array(mesh, this%wedphdlev_lev_lon, member_num , half_lon=.true., full_lat=.true., half_lev=.true.)
    call allocate_array(mesh, this%wedphdlev_lev_lat, member_num , full_lon=.true., half_lat=.true., half_lev=.true.)
    
    call allocate_array(mesh, this%gz               , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%gz_f             , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%gz_lev           , member_num , full_lon=.true., full_lat=.true., half_lev=.true.)
    
    call allocate_array(mesh, this%m                , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%m_vtx            , member_num , half_lon=.true., half_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%m_lon            , member_num , half_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%m_lat            , member_num , full_lon=.true., half_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%mf_lon_n         , member_num , half_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%mf_lon_t         , member_num , half_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%mf_lat_n         , member_num , full_lon=.true., half_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%mf_lat_t         , member_num , full_lon=.true., half_lat=.true., full_lev=.true.)
    
    call allocate_array(mesh, this%pv               , member_num , half_lon=.true., half_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%pv_lon           , member_num , half_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%pv_lat           , member_num , full_lon=.true., half_lat=.true., full_lev=.true.)

    call allocate_array(mesh, this%ke               , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    
    call allocate_array(mesh, this%pt               , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%pt_f             , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%pt_lon           , member_num , half_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%pt_lat           , member_num , full_lon=.true., half_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%pt_lev           , member_num , full_lon=.true., full_lat=.true., half_lev=.true.)
    
    call allocate_array(mesh, this%t                , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%ph               , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%ph_lev           , member_num , full_lon=.true., full_lat=.true., half_lev=.true.)
    call allocate_array(mesh, this%phs              , member_num , full_lon=.true., full_lat=.true.                 )
    call allocate_array(mesh, this%phs_f            , member_num , full_lon=.true., full_lat=.true.                 )
    call allocate_array(mesh, this%div              , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    call allocate_array(mesh, this%vor              , member_num , half_lon=.true., half_lat=.true., full_lev=.true.)
    
    if (nonhydrostatic) then
      call allocate_array(mesh, this%m_lev          , member_num , full_lon=.true., full_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%wedphdlev      , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
      call allocate_array(mesh, this%w              , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
      call allocate_array(mesh, this%w_lev          , member_num , full_lon=.true., full_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%w_lev_lon      , member_num , half_lon=.true., full_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%w_lev_lat      , member_num , full_lon=.true., half_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%gz_lev_lon     , member_num , half_lon=.true., full_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%gz_lev_lat     , member_num , full_lon=.true., half_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%rhod           , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
      call allocate_array(mesh, this%rhod_lon       , member_num , half_lon=.true., full_lat=.true., full_lev=.true.)
      call allocate_array(mesh, this%rhod_lat       , member_num , full_lon=.true., half_lat=.true., full_lev=.true.)
      call allocate_array(mesh, this%p              , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
      call allocate_array(mesh, this%p_lev          , member_num , full_lon=.true., full_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%p_lev_lon      , member_num , half_lon=.true., full_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%p_lev_lat      , member_num , full_lon=.true., half_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%u_lev_lon      , member_num , half_lon=.true., full_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%v_lev_lat      , member_num , full_lon=.true., half_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%mf_lev_lon_n   , member_num , half_lon=.true., full_lat=.true., half_lev=.true.)
      call allocate_array(mesh, this%mf_lev_lat_n   , member_num , full_lon=.true., half_lat=.true., half_lev=.true.)
    else
      this%p_lev => this%ph_lev
    end if

    if (div_damp_order == 4) then
      call allocate_array(mesh, this%div2         , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    end if

    if (use_smag_damp) then
      call allocate_array(mesh, this%tension_h    , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
      call allocate_array(mesh, this%shear_h      , member_num , half_lon=.true., half_lat=.true., full_lev=.true.)
      call allocate_array(mesh, this%kmh          , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
      call allocate_array(mesh, this%kmh_vtx      , member_num , half_lon=.true., half_lat=.true., full_lev=.true.)
      call allocate_array(mesh, this%kmh_lon      , member_num , half_lon=.true., full_lat=.true., full_lev=.true.)
      call allocate_array(mesh, this%kmh_lat      , member_num , full_lon=.true., half_lat=.true., full_lev=.true.)
    end if

    call allocate_array(mesh, this%tmp            , member_num , full_lon=.true., full_lat=.true., full_lev=.true.)
    
    allocate(this%tm(member_num))
    allocate(this%te(member_num))
    allocate(this%tpe(member_num))
    allocate(this%tav(member_num))


    !need fix
    allocate(this%async(async_total_num))

  end subroutine state_init

  subroutine state_clear(this)

    class(state_type), intent(inout) :: this

    integer i

    if (allocated(this%async            )) then
      do i = 1 , async_total_num 
        call this%async(i)%wait()
      end do
      deallocate(this%async            )
    end if

    if (allocated(this%u                )) deallocate(this%u                )
    if (allocated(this%u_f              )) deallocate(this%u_f              )
    if (allocated(this%u_Agrid          )) deallocate(this%u_Agrid          )
    if (allocated(this%v                )) deallocate(this%v                )
    if (allocated(this%v_f              )) deallocate(this%v_f              )
    if (allocated(this%v_Agrid          )) deallocate(this%v_Agrid          )
    
    if (allocated(this%wedphdlev_lev    )) deallocate(this%wedphdlev_lev    )
    if (allocated(this%wedphdlev_lev_lon)) deallocate(this%wedphdlev_lev_lon)
    if (allocated(this%wedphdlev_lev_lat)) deallocate(this%wedphdlev_lev_lat)
    
    if (allocated(this%gz               )) deallocate(this%gz               )
    if (allocated(this%gz_f             )) deallocate(this%gz_f             )
    if (allocated(this%gz_lev           )) deallocate(this%gz_lev           )
    
    if (allocated(this%m                )) deallocate(this%m                )
    if (allocated(this%m_vtx            )) deallocate(this%m_vtx            )
    if (allocated(this%m_lon            )) deallocate(this%m_lon            )
    if (allocated(this%m_lat            )) deallocate(this%m_lat            )
    if (allocated(this%mf_lon_n         )) deallocate(this%mf_lon_n         )
    if (allocated(this%mf_lat_n         )) deallocate(this%mf_lat_n         )
    if (allocated(this%mf_lat_t         )) deallocate(this%mf_lat_t         )
    if (allocated(this%mf_lon_t         )) deallocate(this%mf_lon_t         )
    
    if (allocated(this%pv               )) deallocate(this%pv               )
    if (allocated(this%pv_lon           )) deallocate(this%pv_lon           )
    if (allocated(this%pv_lat           )) deallocate(this%pv_lat           )
    
    if (allocated(this%ke               )) deallocate(this%ke               )
    
    if (allocated(this%pt               )) deallocate(this%pt               )
    if (allocated(this%pt_f             )) deallocate(this%pt_f             )
    if (allocated(this%pt_lon           )) deallocate(this%pt_lon           )
    if (allocated(this%pt_lat           )) deallocate(this%pt_lat           )
    if (allocated(this%pt_lev           )) deallocate(this%pt_lev           )
    
    if (allocated(this%t                )) deallocate(this%t                )
    
    if (allocated(this%ph               )) deallocate(this%ph               )
    if (allocated(this%ph_lev           )) deallocate(this%ph_lev           )
    if (allocated(this%phs              )) deallocate(this%phs              )
    if (allocated(this%phs_f            )) deallocate(this%phs_f            )
    
    if (allocated(this%div              )) deallocate(this%div              )
    if (allocated(this%div2             )) deallocate(this%div2             )
    if (allocated(this%vor              )) deallocate(this%vor              )

    if (allocated(this%m_lev            )) deallocate(this%m_lev            )
    if (allocated(this%wedphdlev        )) deallocate(this%wedphdlev        )
    if (allocated(this%w                )) deallocate(this%w                )
    if (allocated(this%w_lev            )) deallocate(this%w_lev            )
    if (allocated(this%w_lev_lon        )) deallocate(this%w_lev_lon        )
    if (allocated(this%w_lev_lat        )) deallocate(this%w_lev_lat        )
    if (allocated(this%gz_lev_lon       )) deallocate(this%gz_lev_lon       )
    if (allocated(this%gz_lev_lat       )) deallocate(this%gz_lev_lat       )
    if (allocated(this%rhod             )) deallocate(this%rhod             )
    if (allocated(this%rhod_lon         )) deallocate(this%rhod_lon         )
    if (allocated(this%rhod_lat         )) deallocate(this%rhod_lat         )
    if (allocated(this%p                )) deallocate(this%p                )
    if (allocated(this%p_lev_lon        )) deallocate(this%p_lev_lon        )
    if (allocated(this%p_lev_lat        )) deallocate(this%p_lev_lat        )
    if (allocated(this%u_lev_lon        )) deallocate(this%u_lev_lon        )
    if (allocated(this%v_lev_lat        )) deallocate(this%v_lev_lat        )
    if (allocated(this%mf_lev_lon_n     )) deallocate(this%mf_lev_lon_n     )
    if (allocated(this%mf_lev_lat_n     )) deallocate(this%mf_lev_lat_n     )

    if (nonhydrostatic) then
      if (associated(this%p_lev         )) deallocate(this%p_lev            )
    end if

    if (allocated(this%tmp              )) deallocate(this%tmp              )
    if (allocated(this%test             )) deallocate(this%test             )

    if (allocated(this%tm      )) deallocate(this%tm     )
    if (allocated(this%te      )) deallocate(this%te     )
    if (allocated(this%tpe     )) deallocate(this%tpe     )
    if (allocated(this%tav     )) deallocate(this%tav     )

  end subroutine state_clear

  subroutine state_final(this)

    type(state_type), intent(inout) :: this

    call this%clear()

  end subroutine state_final

end module state_mod
