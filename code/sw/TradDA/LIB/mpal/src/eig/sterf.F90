SUBROUTINE MPAL_DLAE2( A, B, C, RT1, RT2 )

          TYPE(MPAL_ST)      A, B, C
          TYPE(MPAL_ST)      RT1, RT2

          TYPE(MPAL_ST)      ONE

          TYPE(MPAL_ST)      TWO
          TYPE(MPAL_ST)      ZERO
          TYPE(MPAL_ST)      HALF

          TYPE(MPAL_ST)      AB, ACMN, ACMX, ADF, DF, RT, SM, TB

          INTRINSIC          ABS, SQRT

          ONE = 1.0D0
          TWO = 2.0D0
          ZERO = 0.0D0
          HALF = 0.5D0

          SM = A + C
          DF = A - C
          ADF = ABS( DF )
          TB = B + B
          AB = ABS( TB )
          IF( ABS( A ).GT.ABS( C ) ) THEN
             ACMX = A
             ACMN = C
          ELSE
             ACMX = C
             ACMN = A
          END IF
          IF( ADF.GT.AB ) THEN
             RT = ADF*SQRT( ONE+( AB / ADF )**2 )
          ELSE IF( ADF.LT.AB ) THEN
             RT = AB*SQRT( ONE+( ADF / AB )**2 )
          ELSE
    ! *
    ! *        Includes case AB=ADF=0
    ! *
             RT = AB*SQRT( TWO )
          END IF
          IF( SM.LT.ZERO ) THEN
             RT1 = HALF*( SM-RT )
    ! *
    ! *        Order of execution important.
    ! *        To get fully accurate smaller eigenvalue,
    ! *        next line needs to be executed in higher precision.
    ! *
             RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
          ELSE IF( SM.GT.ZERO ) THEN
             RT1 = HALF*( SM+RT )
    ! *
    ! *        Order of execution important.
    ! *        To get fully accurate smaller eigenvalue,
    ! *        next line needs to be executed in higher precision.
    ! *
             RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
          ELSE
    ! *
    ! *        Includes case RT1 = RT2 = 0
    ! *
             RT1 = HALF*RT
             RT2 = -HALF*RT
          END IF
          RETURN
    ! *
    ! *     End of DLAE2
    ! *
          END

SUBROUTINE MPAL_DSTERF( N, D, E, INFO )

    INTEGER            INFO, N

    TYPE(MPAL_ST)      D( * ), E( * )

    TYPE(MPAL_ST)      ZERO, ONE, TWO, THREE
    INTEGER            MAXIT
    PARAMETER          ( MAXIT = 30 )

    INTEGER            I, ISCALE, JTOT, L, L1, LEND, LENDSV, LSV, M, &
                       NMAXIT
    TYPE(MPAL_ST)      ALPHA, ANORM, BB, C, EPS, EPS2, GAMMA, OLDC, &
                       OLDGAM, P, R, RTE, S, SAFMAX, SAFMIN, &
                       SIGMA, SSFMAX, SSFMIN, RMAX

    TYPE(MPAL_ST)      RT1, RT2

    EXTERNAL           DLASRT, XERBLA

    INTRINSIC          ABS, SIGN, SQRT

    ZERO = 0.0D0
    ONE = 1.0D0
    TWO = 2.0D0
    THREE = 3.0D0

    INFO = 0

    IF( N.LT.0 ) THEN
        INFO = -1
            CALL XERBLA( 'DSTERF', -INFO )
            RETURN
        END IF
        IF( N.LE.1 ) RETURN
    ! *
    ! *     Determine the unit roundoff for this environment.
    ! *
        EPS = MPAL_DLAMCH( 'E' )
        EPS2 = EPS**2
        SAFMIN = MPAL_DLAMCH( 'S' )
        SAFMAX = ONE / SAFMIN
        SSFMAX = SQRT( SAFMAX ) / THREE
        SSFMIN = SQRT( SAFMIN ) / EPS2
        RMAX = MPAL_DLAMCH( 'O' )
    ! *
    ! *     Compute the eigenvalues of the tridiagonal matrix.
    ! *
        NMAXIT = N*MAXIT
        SIGMA = ZERO
        JTOT = 0
    ! *
    ! *     Determine where the matrix splits and choose QL or QR iteration
    ! *     for each block, according to whether top or bottom diagonal
    ! *     element is smaller.
    ! *
          L1 = 1

    10  CONTINUE
        IF( L1.GT.N ) &
            GO TO 170
            IF( L1.GT.1 ) &
                E( L1-1 ) = ZERO
        DO 20 M = L1, N - 1
            IF( ABS( E( M ) ).LE.( SQRT( ABS( D( M ) ) )*SQRT( ABS( D( M+ &
              1 ) ) ) )*EPS ) THEN
                E( M ) = ZERO
                GO TO 30
            END IF
       20 CONTINUE
        M = N

       30 CONTINUE
        L = L1
        LSV = L
        LEND = M
        LENDSV = LEND
        L1 = M + 1
        IF( LEND.EQ.L ) &
            GO TO 10
    ! *
    ! *     Scale submatrix in rows and columns L to LEND
    ! *
        ANORM = MPAL_DLANST( 'M', LEND-L+1, D( L ), E( L ) )
        ISCALE = 0
        IF( ANORM.EQ.ZERO ) &
            GO TO 10
        IF( (ANORM.GT.SSFMAX) ) THEN
            ISCALE = 1
            CALL MPAL_DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L+1, 1, D( L ), N, &
                        INFO )
            CALL MPAL_DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L, 1, E( L ), N, &
                        INFO )
        ELSE IF( ANORM.LT.SSFMIN ) THEN
            ISCALE = 2
            CALL MPAL_DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L+1, 1, D( L ), N, &
                        INFO )
            CALL MPAL_DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L, 1, E( L ), N, &
                        INFO )
          END IF

        DO 40 I = L, LEND - 1
            E( I ) = E( I ) * E( I )
       40 CONTINUE
    ! *
    ! *     Choose between QL and QR iteration
    ! *
        IF( ABS( D( LEND ) ).LT.ABS( D( L ) ) ) THEN
            LEND = LSV
            L = LENDSV
        END IF

        IF( LEND.GE.L ) THEN
    ! *
    ! *        QL Iteration
    ! *
    ! *        Look for small subdiagonal element.
    ! *
       50    CONTINUE
            IF( L.NE.LEND ) THEN
                DO 60 M = L, LEND - 1
                    IF( ABS( E( M ) ).LE. &
                    EPS2*ABS( D( M )*D( M+1 ) ) ) &
                        GO TO 70
       60       CONTINUE
            END IF
            M = LEND

       70    CONTINUE
            IF( M.LT.LEND ) &
                E( M ) = ZERO
            P = D( L )
            IF( M.EQ.L ) &
                GO TO 90
    ! *
    ! *        If remaining matrix is 2 by 2, use DLAE2 to compute its
    ! *        eigenvalues.
    ! *
            IF( M.EQ.L+1 ) THEN
                RTE = SQRT( E( L ) )
                CALL MPAL_DLAE2( D( L ), RTE, D( L+1 ), RT1, RT2 )
                D( L ) = RT1
                D( L+1 ) = RT2
                E( L ) = ZERO
                L = L + 2
                IF( L.LE.LEND ) &
                    GO TO 50
                GO TO 150
            END IF

            IF( JTOT.EQ.NMAXIT ) &
                GO TO 150
            JTOT = JTOT + 1

            RTE = SQRT( E( L ) )
            SIGMA = ( D( L+1 )-P ) / ( TWO*RTE )
            R = MPAL_DLAPY2( SIGMA, ONE )
            SIGMA = P - ( RTE / ( SIGMA+SIGN( R, SIGMA ) ) )

            C = ONE
            S = ZERO
            GAMMA = D( M ) - SIGMA
            P = GAMMA*GAMMA
    ! *
    ! *        Inner loop
    ! *
             DO 80 I = M - 1, L, -1
                BB = E( I )
                R = P + BB
                IF( I.NE.M-1 ) E( I+1 ) = S*R
                OLDC = C
                C = P / R
                S = BB / R
                OLDGAM = GAMMA
                ALPHA = D( I )
                GAMMA = C*( ALPHA-SIGMA ) - S*OLDGAM
                D( I+1 ) = OLDGAM + ( ALPHA-GAMMA )
                IF( C.NE.ZERO ) THEN
                   P = ( GAMMA*GAMMA ) / C
                ELSE
                   P = OLDC*BB
                END IF
       80    CONTINUE

             E( L ) = S*P
             D( L ) = SIGMA + GAMMA
             GO TO 50
    ! *
    ! *        Eigenvalue found.
    ! *
       90    CONTINUE
             D( L ) = P

             L = L + 1
             IF( L.LE.LEND ) GO TO 50
             GO TO 150

          ELSE
    ! *
    ! *        QR Iteration
    ! *
    ! *        Look for small superdiagonal element.
    ! *
      100    CONTINUE
             DO 110 M = L, LEND + 1, -1
                IF( ABS( E( M-1 ) ).LE.EPS2*ABS( D( M )*D( M-1 ) ) ) &
                    GO TO 120
      110    CONTINUE
             M = LEND

      120    CONTINUE
             IF( M.GT.LEND ) &
                E( M-1 ) = ZERO
             P = D( L )
             IF( M.EQ.L ) &
                GO TO 140
    ! *
    ! *        If remaining matrix is 2 by 2, use DLAE2 to compute its
    ! *        eigenvalues.
    ! *
             IF( M.EQ.L-1 ) THEN
                RTE = SQRT( E( L-1 ) )
                CALL MPAL_DLAE2( D( L ), RTE, D( L-1 ), RT1, RT2 )
                D( L ) = RT1
                D( L-1 ) = RT2
                E( L-1 ) = ZERO
                L = L - 2
                IF( L.GE.LEND ) &
                    GO TO 100
                GO TO 150
             END IF

             IF( JTOT.EQ.NMAXIT ) &
                GO TO 150
             JTOT = JTOT + 1
    ! *
    ! *        Form shift.
    ! *
             RTE = SQRT( E( L-1 ) )
             SIGMA = ( D( L-1 )-P ) / ( TWO*RTE )
             R = MPAL_DLAPY2( SIGMA, ONE )
             SIGMA = P - ( RTE / ( SIGMA+SIGN( R, SIGMA ) ) )

             C = ONE
             S = ZERO
             GAMMA = D( M ) - SIGMA
             P = GAMMA*GAMMA
    ! *
    ! *        Inner loop
    ! *
             DO 130 I = M, L - 1
                BB = E( I )
                R = P + BB
                IF( I.NE.M ) &
                    E( I-1 ) = S*R
                OLDC = C
                C = P / R
                S = BB / R
                OLDGAM = GAMMA
                ALPHA = D( I+1 )
                GAMMA = C*( ALPHA-SIGMA ) - S*OLDGAM
                D( I ) = OLDGAM + ( ALPHA-GAMMA )
                IF( C.NE.ZERO ) THEN
                   P = ( GAMMA*GAMMA ) / C
                ELSE
                   P = OLDC*BB
                END IF
      130    CONTINUE

             E( L-1 ) = S*P
             D( L ) = SIGMA + GAMMA
             GO TO 100
    ! *
    ! *        Eigenvalue found.
    ! *
      140    CONTINUE
             D( L ) = P

             L = L - 1
             IF( L.GE.LEND ) &
                GO TO 100
             GO TO 150
    
          END IF
    ! *
    ! *     Undo scaling if necessary
    ! *
      150 CONTINUE
          IF( ISCALE.EQ.1 ) &
                CALL MPAL_DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV+1, 1, &
                        D( LSV ), N, INFO )
          IF( ISCALE.EQ.2 ) &
                CALL MPAL_DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV+1, 1, &
                        D( LSV ), N, INFO )
    ! *
    ! *     Check for no convergence to an eigenvalue after a total
    ! *     of N*MAXIT iterations.
    ! *
          IF( JTOT.LT.NMAXIT ) &
            GO TO 10
          DO 160 I = 1, N - 1
             IF( E( I ).NE.ZERO ) &
               INFO = INFO + 1
      160 CONTINUE
          GO TO 180
    ! *
    ! *     Sort eigenvalues in increasing order.
    ! *
      170 CONTINUE
          CALL MPAL_LASRT( 'I', N, D, INFO )
    
      180 CONTINUE
          RETURN
    ! *
    ! *     End of DSTERF
    ! *
          END
    