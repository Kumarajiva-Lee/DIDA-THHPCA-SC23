module redis_mod
  use iso_c_binding

  implicit none

  character(len=1), parameter :: nullchar=char(0)

  interface RedisHget
      module procedure RedisHgeti
      module procedure RedisHgetf
      module procedure RedisHgetd
      module procedure RedisHgets
  end interface RedisHget 

  interface RedisHset
      module procedure RedisHseti
      module procedure RedisHsetf
      module procedure RedisHsetd
      module procedure RedisHsets
  end interface RedisHset 

  !interface RedisHmsetf
  !    module procedure RedisHmsetf3d
  !    module procedure RedisHmsetf2d
  !end interface RedisHmsetf
  !interface RedisCmd
  !    module procedure RedisCmdreturn
  !    module procedure RedisCmdreturnf
  !end interface RedisCmd

  interface

     function usleep(n) bind(c)
       use iso_c_binding
       integer(c_int), value :: n
       integer(c_int) usleep
     end function usleep

     !--------------------------------------------------------
     !  init part
     !--------------------------------------------------------
     subroutine redis_test_set(c) bind(c, name='redisTestSet')
       use iso_c_binding
       type(c_ptr), value :: c
     end subroutine redis_test_set

     type(c_ptr) function RedisConnect(redis_address) &
             bind(c, name='setupConnection')
       use iso_c_binding
       character(c_char)     :: redis_address(*)
     end function RedisConnect

     subroutine RedisDisconnect(c) bind(c, name='redisClusterFree')
       use iso_c_binding
       type(c_ptr), value :: c
     end subroutine RedisDisconnect

     subroutine redis_cmd_f(c, cmd) bind(c, name='redis_cmd')
       use iso_c_binding
       type(c_ptr), value    :: c
       character(c_char)     :: cmd(*)
     end subroutine redis_cmd_f

     subroutine redis_cmdreturnf_f(c, cmd, buf) bind(c, name='redis_cmdreturnf')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: cmd(*)
     end subroutine redis_cmdreturnf_f

     subroutine redis_cmdreturnd_f(c, cmd, buf) bind(c, name='redis_cmdreturnd')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: cmd(*)
     end subroutine redis_cmdreturnd_f

     subroutine redis_cmdreturni_f(c, cmd, buf) bind(c, name='redis_cmdreturni')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: cmd(*)
     end subroutine redis_cmdreturni_f

     !--------------------------------------------------------------------------
     !  string part
     !-------------------------------------------------------------------------
     subroutine redis_set_f(c, key, val) bind(c, name='redis_set')
       use iso_c_binding
       type(c_ptr), value    :: c
       character(c_char)     :: val(*), key(*)
     end subroutine redis_set_f

     subroutine redis_setb_f(c, key, buf, len) bind(c, name='redis_setb')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: key(*)
       integer(c_int), intent(in) ::len
     end subroutine redis_setb_f

     !----------------------------------------------------------------------------
     !  hash part
     !----------------------------------------------------------------------------
     subroutine RedisHgeti_f(c, hash, key, buf) bind(c, name='RedisHgeti')
       use iso_c_binding
       type(c_ptr), value    :: c
       character(c_char)     :: hash(*), key(*)
       integer(c_int)        :: buf
     end subroutine RedisHgeti_f

     subroutine RedisHgetf_f(c, hash, key, buf) bind(c, name='RedisHgetf')
       use iso_c_binding
       type(c_ptr), value    :: c
       character(c_char)     :: hash(*), key(*)
       real(c_float)                :: buf
     end subroutine RedisHgetf_f

     subroutine RedisHgetd_f(c, hash, key, buf) bind(c, name='RedisHgetd')
       use iso_c_binding
       type(c_ptr), value    :: c
       character(c_char)     :: hash(*), key(*)
       real(c_double)                :: buf
     end subroutine RedisHgetd_f

     subroutine RedisHgets_f(c, hash, key, buf, len1, len2, len3) bind(c, name='RedisHgets')
       use iso_c_binding
       type(c_ptr), value    :: c
       integer(c_int)        :: len1, len2, len3
       character(c_char)     :: hash(*), key(*), buf(*)
     end subroutine RedisHgets_f

     subroutine RedisHseti_f(c, hash, key, buf) bind(c, name='RedisHseti')
       use iso_c_binding
       type(c_ptr), value    :: c
       character(c_char)     :: hash(*), key(*)
       integer(c_int)        :: buf
     end subroutine RedisHseti_f

     subroutine RedisHsetf_f(c, hash, key, buf) bind(c, name='RedisHsetf')
       use iso_c_binding
       type(c_ptr), value    :: c
       character(c_char)     :: hash(*), key(*)
       real(c_float)                :: buf
     end subroutine RedisHsetf_f

     subroutine RedisHsetd_f(c, hash, key, buf) bind(c, name='RedisHsetd')
       use iso_c_binding
       type(c_ptr), value    :: c
       character(c_char)     :: hash(*), key(*)
       real(c_double)                :: buf
     end subroutine RedisHsetd_f

     subroutine RedisHsets_f(c, hash, key, buf) bind(c, name='RedisHsets')
       use iso_c_binding
       type(c_ptr), value    :: c
       character(c_char)     :: hash(*), key(*), buf(*)
     end subroutine RedisHsets_f

     !------------------------------------------------------------------
     !  zset part
     !------------------------------------------------------------------

     !------------------------------------------------------------------
     !  custum part
     !-----------------------------------------------------------------
     subroutine redis_hmsetf_f(c, hkey, varlist, its, ite, istride, jts, jte, &
             jstride, kms, kme, kstride, n, buf) bind(c, name='redis_hmsetf')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, jts, jte, kms, kme, n
       integer(c_int), intent(in) :: istride, jstride, kstride
     end subroutine redis_hmsetf_f

     subroutine redis_hmsetf2d_f(c, hkey, varlist, its, ite, istride, jts, jte, &
             jstride, n, buf) bind(c, name='redis_hmsetf2d')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, jts, jte, n
       integer(c_int), intent(in) :: istride, jstride
     end subroutine redis_hmsetf2d_f

     subroutine redis_hmsetf1d_f(c, hkey, varlist, its, ite, istride, n, buf) &
             bind(c, name='redis_hmsetf1d')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, n
       integer(c_int), intent(in) :: istride
     end subroutine redis_hmsetf1d_f

     subroutine redis_hmgetf1d_f(c, hkey, varlist,  its, ite, istride, n, buf) &
             bind(c, name='redis_hmgetf1d')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, n
       integer(c_int), intent(in) :: istride
     end subroutine redis_hmgetf1d_f

     subroutine redis_hmgetf2d_f(c, hkey, varlist,  its, ite, istride, jts, jte, &
             jstride, n, buf) bind(c, name='redis_hmgetf2d')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, jts, jte, n
       integer(c_int), intent(in) :: istride, jstride
     end subroutine redis_hmgetf2d_f

     subroutine redis_hmgetf_f(c, hkey, varlist,  its, ite, istride, jts, jte, &
             jstride, kms, kme, kstride, n, buf) bind(c, name='redis_hmgetf')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, jts, jte, kms, kme, n
       integer(c_int), intent(in) :: istride, jstride, kstride
     end subroutine redis_hmgetf_f

     subroutine redis_hmsetd1d_f(c, hkey, varlist,  its, ite, istride, n, buf) &
             bind(c, name='redis_hmsetd1d')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, n
       integer(c_int), intent(in) :: istride
     end subroutine redis_hmsetd1d_f

     subroutine redis_hmsetd2d_f(c, hkey, varlist,  its, ite, istride, jts, jte, &
             jstride, n, buf) bind(c, name='redis_hmsetd2d')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, jts, jte, n
       integer(c_int), intent(in) :: istride, jstride
     end subroutine redis_hmsetd2d_f

     subroutine redis_hmsetd_f(c, hkey, varlist,  its, ite, istride, jts, jte,&
             jstride, kms, kme, kstride, n, buf) bind(c, name='redis_hmsetd')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, jts, jte, kms, kme, n
       integer(c_int), intent(in) :: istride, jstride, kstride
     end subroutine redis_hmsetd_f

     subroutine redis_hmgetd1d_f(c, hkey, varlist,  its, ite, istride, n, buf) &
             bind(c, name='redis_hmgetd1d')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, n
       integer(c_int), intent(in) :: istride
     end subroutine redis_hmgetd1d_f

     subroutine redis_hmgetd2d_f(c, hkey, varlist,  its, ite, istride, jts, jte, &
             jstride, n, buf) bind(c, name='redis_hmgetd2d')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, jts, jte, n
       integer(c_int), intent(in) :: istride, jstride
     end subroutine redis_hmgetd2d_f

     subroutine redis_hmgetd_f(c, hkey, varlist,  its, ite, istride, jts, jte, &
             jstride, kms, kme, kstride, n, buf) bind(c, name='redis_hmgetd')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: its, ite, jts, jte, kms, kme, n
       integer(c_int), intent(in) :: istride, jstride, kstride
     end subroutine redis_hmgetd_f

     !------------------------------------------------------------------------------
     ! da_letkf_customize
     !-----------------------------------------------------------------------------

     subroutine redis_da_outputd_f(c, hkey, varlist,  lonids, lonide, lonstep, latids, latide, latstep, &
             mpas_num_lev, num_lev, zstep, num_2d, num_3d, buf) bind(c, name='redis_da_outputd')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: lonids, lonide, lonstep, latids, latide, &
           latstep, mpas_num_lev, num_lev, zstep, num_2d, num_3d
     end subroutine

     subroutine redis_da_outputf_f(c, hkey, varlist,  lonids, lonide, lonstep, latids, latide, latstep, &
             mpas_num_lev, num_lev, zstep, num_2d, num_3d, buf) bind(c, name='redis_da_outputf')
       use iso_c_binding
       type(c_ptr), value    :: c, buf
       character(c_char)     :: hkey(*) , varlist(*)
       integer(c_int), intent(in) :: lonids, lonide, lonstep, latids, latide, &
           latstep, mpas_num_lev, num_lev, zstep, num_2d, num_3d
     end subroutine
  end interface

contains

  !------------------------------------------------------------------------------------
  !  init part
  !------------------------------------------------------------------------------------
  subroutine RedisCmd(c, cmd)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: cmd

    call redis_cmd_f(c, c_str(cmd))
  end subroutine

  subroutine RedisCmdreturn(c, cmd, buf)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: cmd
    type(*), target :: buf(*)

    call redis_cmdreturnf_f(c, c_str(cmd), c_loc(buf))
  end subroutine

  subroutine RedisCmdreturnd(c, cmd, buf)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: cmd
    type(*), target :: buf(*)

    call redis_cmdreturnd_f(c, c_str(cmd), c_loc(buf))
  end subroutine

  subroutine RedisCmdreturni(c, cmd, buf)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: cmd
    type(*), target :: buf(*)

    call redis_cmdreturni_f(c, c_str(cmd), c_loc(buf))
  end subroutine

  !------------------------------------------------------------------------------------
  !  string part
  !------------------------------------------------------------------------------------
  subroutine redis_set(c, key, val)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: key, val

    call redis_set_f(c, c_str(key), c_str(val))
  end subroutine redis_set

  subroutine redis_setb(c, key, buf, len)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: key
    type(*), target :: buf(*)
    integer, intent(in) :: len

    call redis_setb_f(c, c_str(key), c_loc(buf), len)
  end subroutine redis_setb

  !------------------------------------------------------------------------------------
  !  hash part
  !------------------------------------------------------------------------------------
  subroutine RedisHgets(c, hash, key, buf)
    type(c_ptr), intent(in)                  :: c
    type(c_ptr)                              :: c_string
    character(len=*), intent(in)             :: hash, key
    character(len=*), intent(out)            :: buf
    integer                                  :: len1, len2, len3

    call RedisHgets_f(c, c_str(hash), c_str(key), buf, len1, len2, len3)

  end subroutine RedisHgets

  subroutine RedisHgeti(c, hash, key, buf)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: hash, key
    integer(c_int)  :: buf

    call RedisHgeti_f(c, c_str(hash), c_str(key), buf)
  end subroutine RedisHgeti

  subroutine RedisHgetf(c, hash, key, buf)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: hash, key
    real(c_float)  :: buf

    call RedisHgetf_f(c, c_str(hash), c_str(key), buf)
  end subroutine RedisHgetf

  subroutine RedisHgetd(c, hash, key, buf)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: hash, key
    real(c_double)  :: buf

    call RedisHgetd_f(c, c_str(hash), c_str(key), buf)
  end subroutine RedisHgetd

  subroutine RedisHsets(c, hash, key, buf)
    type(c_ptr), intent(in)                  :: c
    type(c_ptr)                              :: c_string
    character(len=*), intent(in)             :: hash, key
    character(len=*), intent(in)            :: buf

    call RedisHsets_f(c, c_str(hash), c_str(key), c_str(buf))

  end subroutine RedisHsets

  subroutine RedisHseti(c, hash, key, buf)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: hash, key
    integer(c_int)  :: buf

    call RedisHseti_f(c, c_str(hash), c_str(key), buf)
  end subroutine RedisHseti

  subroutine RedisHsetf(c, hash, key, buf)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: hash, key
    real(c_float)  :: buf

    call RedisHsetf_f(c, c_str(hash), c_str(key), buf)
  end subroutine RedisHsetf

  subroutine RedisHsetd(c, hash, key, buf)
    type(c_ptr), intent(in)                  :: c
    character(len=*), intent(in)             :: hash, key
    real(c_double)  :: buf

    call RedisHsetd_f(c, c_str(hash), c_str(key), buf)
  end subroutine RedisHsetd

  !------------------------------------------------------------------------------------
  !  zset part
  !------------------------------------------------------------------------------------

  !------------------------------------------------------------------------------------
  !  custom part
  !------------------------------------------------------------------------------------

  subroutine RedisHmsetf3d(c, hkey, varlist, its, ite, istride, jts, jte, &
          jstride, kms, kme, kstride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, jts, jte, kms, kme, n
    integer(c_int), intent(in) :: istride, jstride, kstride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmsetf_f(c, c_str(hkey), c_str(varlist), its, ite, istride, jts, jte,&
        jstride, kms, kme, kstride, n, c_loc(buf))
  end subroutine

  subroutine RedisHmsetf2d(c, hkey, varlist, its, ite, istride, jts, jte, jstride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, jts, jte, n
    integer(c_int), intent(in) :: istride, jstride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmsetf2d_f(c, c_str(hkey), c_str(varlist), its, ite, istride, &
        jts, jte, jstride, n, c_loc(buf))
  end subroutine

  subroutine RedisHmsetf1d(c, hkey, varlist, its, ite, istride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, n
    integer(c_int), intent(in) :: istride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmsetf1d_f(c, c_str(hkey), c_str(varlist), its, ite, istride, n, c_loc(buf))
  end subroutine

  subroutine RedisHmgetf3d(c, hkey, varlist, its, ite, istride, jts, jte, jstride, &
          kms, kme, kstride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, jts, jte, kms, kme, n
    integer(c_int), intent(in) :: istride, jstride, kstride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmgetf_f(c, c_str(hkey), c_str(varlist), its, ite, istride, jts, jte, &
        jstride, kms, kme, kstride, n, c_loc(buf))
  end subroutine

  subroutine RedisHmgetf2d(c, hkey, varlist, its, ite, istride, jts, jte, jstride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, jts, jte, n
    integer(c_int), intent(in) :: istride, jstride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmgetf2d_f(c, c_str(hkey), c_str(varlist), its, ite, istride, jts, jte, &
        jstride, n, c_loc(buf))
  end subroutine

  subroutine RedisHmgetf1d(c, hkey, varlist, its, ite, istride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, n
    integer(c_int), intent(in) :: istride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmgetf1d_f(c, c_str(hkey), c_str(varlist), its, ite, istride, n, c_loc(buf))
  end subroutine

  subroutine RedisHmsetd1d(c, hkey, varlist, its, ite, istride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, n
    integer(c_int), intent(in) :: istride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmsetd1d_f(c, c_str(hkey), c_str(varlist), its, ite, istride, n, c_loc(buf))
  end subroutine

  subroutine RedisHmsetd2d(c, hkey, varlist, its, ite, istride, jts, jte, jstride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, jts, jte, n
    integer(c_int), intent(in) :: istride, jstride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmsetd2d_f(c, c_str(hkey), c_str(varlist), its, ite, istride, jts, jte,&
        jstride, n, c_loc(buf))
  end subroutine

  subroutine RedisHmsetd3d(c, hkey, varlist, its, ite, istride, jts, jte, jstride,&
          kms, kme, kstride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, jts, jte, kms, kme, n
    integer(c_int), intent(in) :: istride, jstride, kstride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmsetd_f(c, c_str(hkey), c_str(varlist), its, ite, istride, jts, jte, &
        jstride, kms, kme, kstride, n, c_loc(buf))
  end subroutine

  subroutine RedisHmgetd1d(c, hkey, varlist, its, ite, istride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, n
    integer(c_int), intent(in) :: istride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmgetd1d_f(c, c_str(hkey), c_str(varlist), its, ite, istride, n, c_loc(buf))
  end subroutine

  subroutine RedisHmgetd2d(c, hkey, varlist, its, ite, istride, jts, jte, jstride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, jts, jte, n
    integer(c_int), intent(in) :: istride, jstride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmgetd2d_f(c, c_str(hkey), c_str(varlist), its, ite, istride, jts, jte,&
        jstride, n, c_loc(buf))
  end subroutine

  subroutine RedisHmgetd3d(c, hkey, varlist, its, ite, istride, jts, jte, jstride, &
          kms, kme, kstride, n, buf)
    type(c_ptr), intent(in)                  :: c
    integer(c_int), intent(in) :: its, ite, jts, jte, kms, kme, n
    integer(c_int), intent(in) :: istride, jstride, kstride
    character(len=*), intent(in)             :: hkey, varlist
    type(*), target :: buf(*)

    call redis_hmgetd_f(c, c_str(hkey), c_str(varlist), its, ite, istride, &
        jts, jte, jstride, kms, kme, kstride, n, c_loc(buf))
  end subroutine

  !------------------------------------------------------------------------------
  ! da_letkf_customize
  !-----------------------------------------------------------------------------

  subroutine redis_da_outputd(c, hkey, varlist,  lonids, lonide, lonstep, &
          latids, latide, latstep, mpas_num_lev, num_lev, zstep, num_2d, num_3d, buf)
    type(c_ptr), intent(in)                  :: c
    type(*), target :: buf(*)
    character(len=*), intent(in)             :: hkey, varlist
    integer(c_int), intent(in) :: lonids, lonide, lonstep, latids, latide, &
        latstep, mpas_num_lev, num_lev, zstep, num_2d, num_3d
    call redis_da_outputd_f(c, c_str(hkey), c_str(varlist), lonids, lonide, lonstep, &
          latids, latide, latstep, mpas_num_lev, num_lev, zstep, num_2d, num_3d, c_loc(buf))
  end subroutine

  subroutine redis_da_outputf(c, hkey, varlist,  lonids, lonide, lonstep, &
          latids, latide, latstep, mpas_num_lev, num_lev, zstep, num_2d, num_3d, buf)
    type(c_ptr), intent(in)                  :: c
    type(*), target :: buf(*)
    character(len=*), intent(in)             :: hkey, varlist
    integer(c_int), intent(in) :: lonids, lonide, lonstep, latids, latide, &
        latstep, mpas_num_lev, num_lev, zstep, num_2d, num_3d
    call redis_da_outputf_f(c, c_str(hkey), c_str(varlist), lonids, lonide, lonstep, &
          latids, latide, latstep, mpas_num_lev, num_lev, zstep, num_2d, num_3d, c_loc(buf))
  end subroutine

  function RedisConnect_Balanced(group_size, group_rank, redis_address)
    use string
    implicit none
    type(c_ptr) :: RedisConnect_Balanced
    integer, allocatable, dimension(:) :: addnum
    type(string_type), allocatable :: redis_add(:)
    integer,intent(in) :: group_rank, group_size
    character(len=*), intent(in)  :: redis_address

    redis_add = split_string(redis_address, ',')
    allocate(addnum(0:group_size - 1))
    addnum(group_rank) = mod(group_rank, size(redis_add)) + 1

    RedisConnect_Balanced = RedisConnect(c_str(trim(redis_add(addnum(group_rank))%value)))

  end function RedisConnect_Balanced
  !------------------------------------------------------------------------------------
  !------------------------------------------------------------------------------------
  !  function part
  !------------------------------------------------------------------------------------
  function c_str(f_str)

    character(*), intent(in) :: f_str
    character(len=len_trim(f_str)+1,kind=c_char) c_str

    c_str = trim(f_str) // c_null_char

  end function c_str

  function static_s(f_str, f_len)
    character, allocatable, intent(in) :: f_str(:)
    integer, intent(in) :: f_len
    character(len=f_len) static_s
    integer :: i
    character :: ts

    static_s = ""
    do i = 1, f_len
      ts = f_str(i)
      static_s(i:i) = ts
    end do
  end function static_s

end module redis_mod
