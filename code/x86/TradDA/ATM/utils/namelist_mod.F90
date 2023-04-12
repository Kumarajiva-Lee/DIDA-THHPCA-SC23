!#define Only_Gmcore

module namelist_mod

  use string
  use const_mod
  use flogger
  use time_mod , start_time => start_time_array, end_time => end_time_array
#ifdef Only_Gmcore
#else
  use coupler_config   , only: num_lon , num_lat , num_lev , case_name, da_in_seconds, da_var_name , vert_coord_scheme,vert_coord_template
#endif

  implicit none 

  character(30)   :: planet               = 'earth'

  real(r8)        :: dt_dyn               = 0
  real(r8)        :: dt_adv               = 0
  real(r8)        :: dt_phys              = 0

  character(256)  :: case_desc            = 'N/A'
  character(30)   :: test_case            = 'N/A'
  character(30)   :: history_interval(1)  = 'N/A'
  character(30)   :: restart_interval     = 'N/A'
  character(30)   :: print_interval       = '1 hours'
  character(256)  :: initial_file         = 'N/A'
  character(256)  :: restart_file         = 'N/A'
  character(256)  :: topo_file            = 'N/A'
  character(30 )  :: topo_type            = 'etopo1' ! etopo1, mola32
  character(256)  :: bkg_file             = 'N/A'
  character(30 )  :: bkg_type             = 'era5'


#ifdef Only_Gmcore
  character(256)  :: case_name            = 'N/A'
#endif

  integer         :: nlon
  integer         :: nlat
  integer         :: nlev                 = 1


  logical         :: baroclinic           = .false.
  logical         :: hydrostatic          = .true.
  logical         :: nonhydrostatic       = .false.
  logical         :: advection            = .false.
  logical         :: restart              = .false.

  character(30)   :: physics_suite        = 'none'
  character(30)   :: pbl_scheme           = 'ysu'

  integer         :: lon_hw               = 2


  integer         :: num_proc_total       = 0 
  integer         :: num_proc_lon(20)     = 0
  integer         :: num_proc_lat(20)     = 0
  character(30)   :: partition_type       = 'irregular'
  logical         :: blocked_comm         = .false. ! 是否启用非阻塞通信

  integer         :: irr_part             = 0
  integer         :: irr_num_lat(30)      = 0
  integer         :: irr_num_proc_lon(30) = 0
  integer         :: irr_num_proc_lat(30) = 0


  character(30)   :: tangent_wgt_scheme   = 'classic'

  real(r8)        :: implicit_w_wgt       = 0.5_r8

#ifdef Only_Gmcore
  character(30)   :: vert_coord_scheme    = 'hybrid'
  character(30)   :: vert_coord_template  = 'N/A'
#endif

  character(30)   :: refer_state_scheme   = 'wrf'
  real(r8)        :: ptop                 = 2.194e2_r8
  real(r8)        :: hybrid_coord_p0      = 1.0e5_r8


  integer         :: ke_scheme            = 2
  real(r8)        :: ke_cell_wgt          = 0.5_r8

  character(30)   :: pv_scheme            = 'upwind' ! midpoint, upwind, ffsl
  logical         :: pv_pole_stokes       = .true.
  integer         :: upwind_order_pv      = 3
  real(r8)        :: upwind_wgt_pv        = 1

  character(8)    :: pgf_scheme           = 'lin97'
  integer         :: coriolis_scheme      = 1

  character(8)    :: adv_scheme           = 'ffsl'
  character(8)    :: depart_point_scheme  = 'eul'
  character(8)    :: limiter_type         = 'mono'
  character(8)    :: ffsl_flux_type       = 'ppm'
  character(8)    :: tvd_limiter_type     = 'van_leer'

  logical         :: use_adv              = .true.
  logical         :: use_phy              = .true.

  character(8)    :: zonal_tridiag_solver = 'mkl' ! mkl, spk

  integer         :: weno_order           = -1 ! -1, 3
  integer         :: upwind_order         = 3 ! -1, 1, 3
  real(r8)        :: upwind_wgt           = 1.0_r8
  real(r8)        :: upwind_wgt_pt        = 0.25_r8

  integer         :: vert_weno_order      = -1 ! -1, 3
  integer         :: vert_upwind_order    = 3 ! -1, 1, 3
  real(r8)        :: vert_upwind_wgt      = 1.0_r8

  character(30)   :: time_scheme          = 'wrfrk3'

  real(r8)        :: coarse_pole_mul      = 0
  real(r8)        :: coarse_pole_decay    = 100.0

  ! Filter settings
  real(r8)        :: max_wave_speed       = 300
  real(r8)        :: max_cfl              = 0.5
  real(r8)        :: filter_coef_a        = 1.0
  real(r8)        :: filter_coef_b        = 0.4
  real(r8)        :: filter_coef_c        = 0.2
  
  real(8)         :: filter_coef1_phs     = 0.375_r8
  real(8)         :: filter_coef2_phs     = 0
  real(8)         :: filter_lat0_phs      = 60.0_r8
  real(8)         :: filter_coef1_pt      = 0.375_r8
  real(8)         :: filter_coef2_pt      = 0
  real(8)         :: filter_lat0_pt       = 60.0_r8
  real(8)         :: filter_coef1_uv      = 0.375_r8
  real(8)         :: filter_coef2_uv      = 0
  real(8)         :: filter_lat0_uv       = 60.0_r8

  ! Damping settings
  logical         :: use_topo_smooth      = .false.
  integer         :: topo_smooth_cycles   = 1
  logical         :: use_div_damp         = .false.
  integer         :: div_damp_order       = 2
  integer         :: div_damp_k0          = 3
  real(r8)        :: div_damp_imp_lat0    = 90
  real(r8)        :: div_damp_top         = 3.0_r8
  real(r8)        :: div_damp_pole        = 0.0_r8
  real(r8)        :: div_damp_lat0        = 90
  real(r8)        :: div_damp_coef2       = 1.0_r8 / 128.0_r8
  real(r8)        :: div_damp_coef4       = 0.001_r8
  real(r8)        :: rayleigh_damp_w_coef = 0.2
  real(r8)        :: rayleigh_damp_top    = 10.0d3 ! m
  logical         :: use_smag_damp        = .false.
  real(r8)        :: smag_damp_coef       = 0.1

  ! Output settings
  character(8)    :: output_i0_dtype      = 'r8'
  logical         :: output_h0            = .true.
  character(8)    :: output_h0_dtype      = 'r4'
  logical         :: output_h1            = .false.
  logical         :: split_h0             = .false.
  character(30)   :: output_h0_new_file   = ''
  character(8)    :: output_h0_vars(100)  = ''
  integer         :: output_group_size    = 0
  logical         :: output_Agrid         = .true.

  !member_num
  integer         :: member_total         = 1
  integer         :: vector_num           = 1
  logical         :: use_create_ensemble  = .false.
  integer         :: create_ensemble_interval
  character(30)   :: initial_file_type    = 'N/A'
  integer         :: initial_interval     = -1

  !with LETKF
  integer         :: da_cycle_total       = -1

  

  namelist /namelist_atm/     &
    planet                    , &
    test_case                 , &
    case_desc                 , &
    nonhydrostatic            , &
    advection                 , &
    lon_hw                    , &
    num_proc_total            , &
    num_proc_lon              , &
    num_proc_lat              , &
    partition_type            , &
    blocked_comm              , &
    irr_part                  , &
    irr_num_lat               , &
    irr_num_proc_lon          , &
    irr_num_proc_lat          , &


#ifdef Only_Gmcore
    case_name                 , &
    nlon                      , &
    nlat                      , &
    nlev                      , &
    start_time                , &
    end_time                  , &
#endif

#ifndef Only_Gmcore
    start_time          , &
    end_time            , &
#endif
    dt_dyn                    , &
    dt_adv                    , &
    dt_phys                   , &
    run_hours                 , &
    run_days                  , &
    history_interval          , &
    restart_interval          , &
    print_interval            , &
    initial_file              , &
    restart_file              , &
    topo_file                 , &
    bkg_file                  , &
    bkg_type                  , &
    restart                   , &
    tangent_wgt_scheme        , &
    implicit_w_wgt            , &
#ifdef Only_Gmcore
    vert_coord_scheme         , &
    vert_coord_template       , &
#endif
    refer_state_scheme        , &
    ptop                      , &
    ke_scheme                 , &
    ke_cell_wgt               , &
    pv_scheme                 , &
    pv_pole_stokes            , &
    upwind_order_pv           , &
    upwind_wgt_pv             , &
    pgf_scheme                , &
    coriolis_scheme           , &
    use_adv                   , &
    use_phy                   , &
    adv_scheme                , &
    depart_point_scheme       , &
    limiter_type              , &
    ffsl_flux_type            , &
    tvd_limiter_type          , &
    zonal_tridiag_solver      , &
    weno_order                , &
    upwind_order              , &
    upwind_wgt                , &
    upwind_wgt_pt             , &
    vert_weno_order           , &
    vert_upwind_order         , &
    vert_upwind_wgt           , &
    time_scheme               , &
    max_wave_speed            , &
    max_cfl                   , &
    filter_coef_a             , &
    filter_coef_b             , &
    filter_coef_c             , &
    filter_coef1_phs          , &
    filter_coef2_phs          , &
    filter_lat0_phs           , &
    filter_coef1_pt           , &
    filter_coef2_pt           , &
    filter_lat0_pt            , &
    filter_coef1_uv           , &
    filter_coef2_uv           , &
    filter_lat0_uv            , &
    coarse_pole_mul           , &
    coarse_pole_decay         , &
    physics_suite             , &
    pbl_scheme                , &
    use_topo_smooth           , &
    topo_smooth_cycles        , &
    use_div_damp              , &
    div_damp_order            , &
    div_damp_imp_lat0         , &
    div_damp_k0               , &
    div_damp_top              , &
    div_damp_pole             , &
    div_damp_lat0             , &
    div_damp_coef2            , &
    div_damp_coef4            , &
    rayleigh_damp_w_coef      , &
    rayleigh_damp_top         , &
    use_smag_damp             , &
    smag_damp_coef            , &
    output_h0                 , &
    output_h0_dtype           , &
    output_h1                 , &
    split_h0                  , &
    output_Agrid              , &
    member_total              , &
    use_create_ensemble       , &
    create_ensemble_interval  , &
    initial_file_type         , &
    initial_interval          , &
    da_cycle_total


contains

  subroutine parse_namelist(file_path)

    character(*), intent(in) :: file_path

    open(10, file=file_path, status='old')
#ifndef Only_Gmcore
    rewind(10)
    nlon = num_lon
    nlat = num_lat
    nlev = num_lev
#endif
    read(10, nml=namelist_atm)  
    close(10)
  
    baroclinic = nlev > 1
    if (.not. baroclinic) then
      hydrostatic    = .false.
      nonhydrostatic = .false.
      ke_scheme      = 1
    else
      hydrostatic = .not. nonhydrostatic
    end if

    if (advection) then
      hydrostatic    = .false.
      baroclinic     = .false.
      nonhydrostatic = .false.
    end if


    ! Validate parameters.
    if (member_total < member_num) call log_error('Wrong! Member_num is smaller than Member_total !')

    vector_num = (member_total + member_num - 1) / member_num

  end subroutine parse_namelist

  subroutine print_namelist()

    write(*, *) '=================== GMCORE Parameters ==================='
    write(*, *) 'case_name           = ', trim(case_name)
    write(*, *) 'nlon                = ', to_str(nlon)
    write(*, *) 'nlat                = ', to_str(nlat)
    write(*, *) 'nlev                = ', to_str(nlev)
  if (num_proc_total /= 0) then
    write(*,*) ,'num_proc_total      = ', to_str(num_proc_total)
  end if
    write(*, *) 'num_proc_lon        = ', to_str(irr_num_proc_lon)
    write(*, *) 'num_proc_lat        = ', to_str(irr_num_proc_lat)
    write(*, *) 'blocked_comm        = ', to_str(blocked_comm)
    write(*, *) 'partition_type      = ', trim(partition_type)
    if (coarse_pole_mul /= 0) then
      write(*, *) 'coarse_pole_mul     = ', to_str(coarse_pole_mul, 3)
      write(*, *) 'coarse_pole_decay   = ', to_str(coarse_pole_decay, 3)
    end if
    write(*, *) 'physics_suite       = ', trim(physics_suite)
    write(*, *) 'pbl_scheme          = ', trim(pbl_scheme)
    write(*, *) 'hydrostatic         = ', to_str(hydrostatic)
    write(*, *) 'nonhydrostatic      = ', to_str(nonhydrostatic)
    write(*, *) 'vert_coord_scheme   = ', trim(vert_coord_scheme)
    write(*, *) 'vert_coord_template = ', trim(vert_coord_template)
    write(*, *) 'ptop                = ', to_str(ptop, 4)
    write(*, *) 'hybrid_coord_p0     = ', hybrid_coord_p0
    write(*, *) 'dt_dyn              = ', to_str(dt_dyn , 2)
    write(*, *) 'dt_adv              = ', to_str(dt_adv , 2)
    write(*, *) 'dt_phys             = ', to_str(dt_phys, 2)
    write(*, *) 'max_wave_speed      = ', max_wave_speed
    write(*, *) 'max_cfl             = ', max_cfl
    write(*, *) 'filter_coef_a       = ', filter_coef_a
    write(*, *) 'filter_coef_b       = ', filter_coef_b
    write(*, *) 'filter_coef_c       = ', filter_coef_c
    write(*, *) 'filter_coef1_phs    = ', filter_coef1_phs
    write(*, *) 'filter_coef2_phs    = ', filter_coef2_phs
    write(*, *) 'filter_lat0_phs     = ', filter_lat0_phs
    write(*, *) 'filter_coef1_pt     = ', filter_coef1_pt
    write(*, *) 'filter_coef2_pt     = ', filter_coef2_pt
    write(*, *) 'filter_lat0_pt      = ', filter_lat0_pt
    write(*, *) 'filter_coef1_uv     = ', filter_coef1_uv
    write(*, *) 'filter_coef2_uvs    = ', filter_coef2_uv
    write(*, *) 'filter_lat0_uv      = ', filter_lat0_uv
    write(*, *) 'pgf_scheme          = ', trim(pgf_scheme)
    write(*, *) 'adv_scheme          = ', trim(adv_scheme)
    write(*, *) 'depart_point_scheme = ', trim(depart_point_scheme)
    write(*, *) 'limiter_type        = ', trim(limiter_type)
  if (adv_scheme == 'ffsl') then
    write(*, *) 'ffsl_flux_type      = ', trim(ffsl_flux_type)
  end if
    write(*, *) 'ke_scheme           = ', to_str(ke_scheme)
  if (ke_scheme == 2) then
    write(*, *) 'ke_cell_wgt         = ', to_str(ke_cell_wgt, 2)
  end if
    write(*, *) 'pv_scheme           = ', trim(pv_scheme)
    write(*, *) 'pv_pole_stokes      = ', to_str(pv_pole_stokes)
  if (pv_scheme == 'upwind') then
    write(*, *) 'upwind_order_pv     = ', to_str(upwind_order_pv)
    write(*, *) 'upwind_wgt_pv       = ', to_str(upwind_wgt_pv, 2)
  end if
    write(*, *) 'time_scheme         = ', trim(time_scheme)
    write(*, *) 'upwind_order        = ', to_str(upwind_order)
    write(*, *) 'use_topo_smooth     = ', to_str(use_topo_smooth)
  if (use_topo_smooth) then
    write(*, *) 'topo_smooth_cycles  = ', to_str(topo_smooth_cycles)
  end if
    write(*, *) 'use_div_damp        = ', to_str(use_div_damp)
  if (use_div_damp) then
    write(*, *) 'div_damp_coef2      = ', div_damp_coef2
    write(*, *) 'div_damp_top        = ', to_str(div_damp_top, 3)
    write(*, *) 'div_damp_pole       = ', div_damp_pole
    write(*, *) 'div_damp_lat0       = ', div_damp_lat0
  end if
  if (nonhydrostatic) then
    write(*, *) 'implicit_w_wgt      = ', to_str(implicit_w_wgt, 3)
    write(*, *) 'rayleigh_damp_w_coef= ', to_str(rayleigh_damp_w_coef, 2)
    write(*, *) 'rayleigh_damp_top   = ', to_str(rayleigh_damp_top   , 2)
  end if
    write(*, *) 'use_smag_damp       = ', to_str(use_smag_damp)
  if (use_smag_damp) then
    write(*, *) 'smag_damp_coef      = ', to_str(smag_damp_coef, 1)
  end if
    write(*, *) 'use_adv             = ', to_str(use_adv)
    write(*, *) 'use_phy             = ', to_str(use_phy)
    
    write(*, *) 'output_h0           = ', to_str(output_h0)
    write(*, *) 'output_h1           = ', to_str(output_h1)
    write(*, *) 'output_Agrid        = ', to_str(output_Agrid)   
    write(*, *) 'member_inside       = ', to_str(member_num)
    write(*, *) 'member_total        = ', to_str(member_total)
    write(*, *) 'vector_outside      = ', to_str(vector_num)
    write(*, *) 'use_create_ensemble = ', to_str(use_create_ensemble)
    if (use_create_ensemble) then
      write(*, *) 'create_ensemble_interval = ', to_str(create_ensemble_interval)
    end if
    write(*, *) 'initial_file_type   = ', initial_file_type
    if (initial_file_type == 'time') then
      write(*,*) 'initial_interval   = ', to_str(initial_interval)
    end if
    write(*, *) '========================================================='

  end subroutine print_namelist

end module namelist_mod
