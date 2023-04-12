TYPE(MPAL_ST) FUNCTION MPAL_DLAMCH( CMACH )

      CHARACTER          CMACH

      TYPE(MPAL_ST)      ONE, ZERO

      TYPE(MPAL_ST)      EPS, RMACH, RND, SFMIN, SMALL

      LOGICAL            LSAME
      EXTERNAL           LSAME

      INTRINSIC          DIGITS

      ONE = 1.0D+0
      ZERO = 0.0D+0

      RND = ONE

      IF( ONE.EQ.RND ) THEN
         EPS = EPSILON(ZERO) * 0.5
      ELSE
         EPS = EPSILON(ZERO)
      END IF

      IF( LSAME( CMACH, 'E' ) ) THEN
         RMACH = EPS
      ELSE IF( LSAME( CMACH, 'S' ) ) THEN
         SFMIN = TINY(ZERO)
         SMALL = ONE / HUGE(ZERO)
         IF( SMALL.GE.SFMIN ) THEN
! *
! *           Use SMALL plus a bit, to avoid the possibility of rounding
! *           causing overflow when computing  1/sfmin.
! *
            SFMIN = SMALL*( ONE+EPS )
         END IF
         RMACH = SFMIN
      ELSE IF( LSAME( CMACH, 'B' ) ) THEN
         RMACH = RADIX(MPAL_VAL(ZERO))
      ELSE IF( LSAME( CMACH, 'P' ) ) THEN
         RMACH = EPS * RADIX(MPAL_VAL(ZERO))
      ELSE IF( LSAME( CMACH, 'N' ) ) THEN
         RMACH = mpal_sbits(ZERO) + 1
      ELSE IF( LSAME( CMACH, 'R' ) ) THEN
         RMACH = RND
      ELSE IF( LSAME( CMACH, 'M' ) ) THEN
         RMACH = minexponent(ZERO)
      ELSE IF( LSAME( CMACH, 'U' ) ) THEN
         RMACH = tiny(zero)
      ELSE IF( LSAME( CMACH, 'L' ) ) THEN
         RMACH = maxexponent(ZERO)
      ELSE IF( LSAME( CMACH, 'O' ) ) THEN
         RMACH = huge(ZERO)
      ELSE
         RMACH = ZERO
      END IF

      MPAL_DLAMCH = RMACH
      RETURN

      END