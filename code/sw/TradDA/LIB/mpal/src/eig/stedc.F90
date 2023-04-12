SUBROUTINE MPAL_DSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK, &
                            LIWORK, INFO )

    CHARACTER          COMPZ
    INTEGER            INFO, LDZ, LIWORK, LWORK, N

    INTEGER            IWORK( * )
    TYPE(MPAL_ST)      D( * ), E( * ), WORK( * ), Z( LDZ, * )

    TYPE(MPAL_ST)      ZERO, ONE, TWO

    LOGICAL            LQUERY
    INTEGER            FINISH, I, ICOMPZ, II, J, K, LGN, LIWMIN, &
                       LWMIN, M, SMLSIZ, START, STOREZ, STRTRW
    TYPE(MPAL_ST)      EPS, ORGNRM, P, TINY

    LOGICAL            LSAME
    INTEGER            ILAENV
    EXTERNAL           LSAME, ILAENV

    EXTERNAL           XERBLA

    INTRINSIC          ABS, DBLE, INT, LOG, MAX, MOD, SQRT

    ZERO = 0.0D0
    ONE = 1.0D0
    TWO = 2.0D0

    INFO = 0
    LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )

    IF( LSAME( COMPZ, 'N' ) ) THEN
        ICOMPZ = 0
    ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
        ICOMPZ = 1
    ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
        ICOMPZ = 2
    ELSE
        ICOMPZ = -1
    END IF
    IF( ICOMPZ.LT.0 ) THEN
        INFO = -1
    ELSE IF( N.LT.0 ) THEN
        INFO = -2
    ELSE IF( ( LDZ.LT.1 ) .OR. &
             ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1, N ) ) ) THEN
        INFO = -6
    END IF

    IF( INFO.EQ.0 ) THEN
! *
! *        Compute the workspace requirements
! *
        SMLSIZ = ILAENV( 9, 'DSTEDC', ' ', 0, 0, 0, 0 )
        IF( N.LE.1 .OR. ICOMPZ.EQ.0 ) THEN
            LIWMIN = 1
            LWMIN = 1
        ELSE IF( N.LE.SMLSIZ ) THEN
            LIWMIN = 1
            LWMIN = 2*( N - 1 )
        ELSE
            LGN = MPAL_VAL(INT( LOG( DBLE( N ) )/LOG( TWO ) ))
            IF( 2**LGN.LT.N ) &
                LGN = LGN + 1
            IF( 2**LGN.LT.N ) &
                LGN = LGN + 1
            IF( ICOMPZ.EQ.1 ) THEN
                LWMIN = 1 + 3*N + 2*N*LGN + 4*N**2
                LIWMIN = 6 + 6*N + 5*N*LGN
            ELSE IF( ICOMPZ.EQ.2 ) THEN
                LWMIN = 1 + 4*N + N**2
                LIWMIN = 3 + 5*N
            END IF
        END IF
        WORK( 1 ) = LWMIN
        IWORK( 1 ) = LIWMIN

        IF( LWORK.LT.LWMIN .AND. .NOT. LQUERY ) THEN
            INFO = -8
        ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT. LQUERY ) THEN
            INFO = -10
        END IF
    END IF

    IF( INFO.NE.0 ) THEN
        CALL XERBLA( 'DSTEDC', -INFO )
        RETURN
    ELSE IF (LQUERY) THEN
        RETURN
    END IF
! *
! *     Quick return if possible
! *
    IF( N.EQ.0 ) &
        RETURN
    IF( N.EQ.1 ) THEN
        IF( ICOMPZ.NE.0 ) &
            Z( 1, 1 ) = ONE
        RETURN
     END IF
! *
! *     If the following conditional clause is removed, then the routine
! *     will use the Divide and Conquer routine to compute only the
! *     eigenvalues, which requires (3N + 3N**2) real workspace and
! *     (2 + 5N + 2N lg(N)) integer workspace.
! *     Since on many architectures DSTERF is much faster than any other
! *     algorithm for finding eigenvalues only, it is used here
! *     as the default. If the conditional clause is removed, then
! *     information on the size of workspace needs to be changed.
! *
! *     If COMPZ = 'N', use DSTERF to compute the eigenvalues.
! *
    IF( ICOMPZ.EQ.0 ) THEN
        CALL MPAL_DSTERF( N, D, E, INFO )
        GO TO 50
    END IF
! *
! *     If N is smaller than the minimum divide size (SMLSIZ+1), then
! *     solve the problem with another solver.
! *
    !WRITE(*, *) SMLSIZ
    IF( N.LE.SMLSIZ ) THEN

        CALL MPAL_DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )

     ELSE
! *
! *        If COMPZ = 'V', the Z matrix must be stored elsewhere for later
! *        use.
! *
        IF( ICOMPZ.EQ.1 ) THEN
           STOREZ = 1 + N*N
        ELSE
           STOREZ = 1
        END IF

        IF( ICOMPZ.EQ.2 ) THEN
           CALL MPAL_DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
        END IF
! *
! *        Scale.
! *
        ORGNRM = MPAL_DLANST( 'M', N, D, E )
        IF( ORGNRM.EQ.ZERO ) GO TO 50

        EPS = MPAL_DLAMCH( 'Epsilon' )

        START = 1

  10    CONTINUE
        IF( START.LE.N ) THEN
! *
! *           Let FINISH be the position of the next subdiagonal entry
! *           such that E( FINISH ) <= TINY or FINISH = N if no such
! *           subdiagonal exists.  The matrix identified by the elements
! *           between START and FINISH constitutes an independent
! *           sub-problem.
! *
           FINISH = START
  20       CONTINUE
           IF( FINISH.LT.N ) THEN
              TINY = EPS*SQRT( ABS( D( FINISH ) ) )* &
                        SQRT( ABS( D( FINISH+1 ) ) )
              IF( ABS( E( FINISH ) ).GT.TINY ) THEN
                 FINISH = FINISH + 1
                 GO TO 20
              END IF
           END IF
! *
! *           (Sub) Problem determined.  Compute its size and solve it.
! *
           M = FINISH - START + 1
           IF( M.EQ.1 ) THEN
              START = FINISH + 1
              GO TO 10
           END IF
           IF( M.GT.SMLSIZ ) THEN
! *
! *              Scale.
! *
              ORGNRM = MPAL_DLANST( 'M', M, D( START ), E( START ) )
              CALL MPAL_DLASCL( 'G', 0, 0, ORGNRM, ONE, M, 1, D( START ), M, &
                           INFO )
              CALL MPAL_DLASCL( 'G', 0, 0, ORGNRM, ONE, M-1, 1, E( START ), &
                           M-1, INFO )

              IF( ICOMPZ.EQ.1 ) THEN
                 STRTRW = 1
              ELSE
                 STRTRW = START
              END IF
              CALL MPAL_DLAED0( ICOMPZ, N, M, D( START ), E( START ), &
                           Z( STRTRW, START ), LDZ, WORK( 1 ), N, &
                           WORK( STOREZ ), IWORK, INFO )
              IF( INFO.NE.0 ) THEN
                 INFO = ( INFO / ( M+1 )+START-1 )*( N+1 ) + &
                        MOD( INFO, ( M+1 ) ) + START - 1
                 GO TO 50
              END IF
! *
! *              Scale back.
! *
              CALL MPAL_DLASCL( 'G', 0, 0, ONE, ORGNRM, M, 1, D( START ), M, &
                           INFO )
           ELSE
              IF( ICOMPZ.EQ.1 ) THEN
! *
! *                 Since QR won't update a Z matrix which is larger than
! *                 the length of D, we must solve the sub-problem in a
! *                 workspace and then multiply back into Z.
! *
                 CALL MPAL_DSTEQR( 'I', M, D( START ), E( START ), WORK, M, &
                              WORK( M*M+1 ), INFO )
                 CALL MPAL_DLACPY( 'A', N, M, Z( 1, START ), LDZ, &
                              WORK( STOREZ ), N )
                 CALL MPAL_GEMM( 'N', 'N', N, M, M, ONE, &
                             WORK( STOREZ ), N, WORK, M, ZERO, &
                             Z( 1, START ), LDZ )
              ELSE IF( ICOMPZ.EQ.2 ) THEN
                 CALL MPAL_DSTEQR( 'I', M, D( START ), E( START ), &
                              Z( START, START ), LDZ, WORK, INFO )
              ELSE
                 CALL MPAL_DSTERF( M, D( START ), E( START ), INFO )
              END IF
              IF( INFO.NE.0 ) THEN
                 INFO = START*( N+1 ) + FINISH
                 GO TO 50
              END IF
           END IF

           START = FINISH + 1
           GO TO 10
        END IF

        IF( ICOMPZ.EQ.0 ) THEN
! *
! *          Use Quick Sort
! *
          CALL MPAL_DLASRT( 'I', N, D, INFO )

        ELSE
! *
! *          Use Selection Sort to minimize swaps of eigenvectors
! *
          DO 40 II = 2, N
             I = II - 1
             K = I
             P = D( I )
             DO 30 J = II, N
                IF( D( J ).LT.P ) THEN
                   K = J
                   P = D( J )
                END IF
  30         CONTINUE
             IF( K.NE.I ) THEN
                D( K ) = D( I )
                D( I ) = P
                CALL MPAL_SWAP( N, Z( 1, I ), 1, Z( 1, K ), 1 )
             END IF
  40      CONTINUE
        END IF
     END IF

  50 CONTINUE
     WORK( 1 ) = LWMIN
     IWORK( 1 ) = LIWMIN

     RETURN
! *
! *     End of DSTEDC
! *
     END
