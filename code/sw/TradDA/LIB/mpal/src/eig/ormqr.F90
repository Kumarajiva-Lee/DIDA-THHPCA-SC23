SUBROUTINE MPAL_DLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )

    CHARACTER          SIDE
    INTEGER            INCV, LDC, M, N
    TYPE(MPAL_ST)      TAU

    TYPE(MPAL_ST)      C( LDC, * ), V( * ), WORK( * )

    TYPE(MPAL_ST)      ONE, ZERO

    LOGICAL            APPLYLEFT
    INTEGER            I, LASTV, LASTC

    LOGICAL            LSAME
    INTEGER            ILADLR, ILADLC
    EXTERNAL           LSAME, ILADLR, ILADLC

    ONE = 1.0D+0
    ZERO = 0.0D+0

    APPLYLEFT = LSAME( SIDE, 'L' )
    LASTV = 0
    LASTC = 0
    IF( TAU.NE.ZERO ) THEN
!     Set up variables for scanning V.  LASTV begins pointing to the end
!     of V.
       IF( APPLYLEFT ) THEN
          LASTV = M
       ELSE
          LASTV = N
       END IF
       IF( INCV.GT.0 ) THEN
          I = 1 + (LASTV-1) * INCV
       ELSE
          I = 1
       END IF
!     Look for the last non-zero row in V.
       DO WHILE( LASTV.GT.0 .AND. V( I ).EQ.ZERO )
          LASTV = LASTV - 1
          I = I - INCV
       END DO
       IF( APPLYLEFT ) THEN
!     Scan for the last non-zero column in C(1:lastv,:).
          LASTC = ILADLC(LASTV, N, C, LDC)
       ELSE
!     Scan for the last non-zero row in C(:,1:lastv).
          LASTC = ILADLR(M, LASTV, C, LDC)
       END IF
    END IF
!     Note that lastc.eq.0 renders the BLAS operations null; no special
!     case is needed at this level.
    IF( APPLYLEFT ) THEN
       IF( LASTV.GT.0 ) THEN
          CALL MPAL_GEMV( 'Transpose', LASTV, LASTC, ONE, C, LDC, V, INCV, &
               ZERO, WORK, 1 )

          CALL MPAL_GER( LASTV, LASTC, -TAU, V, INCV, WORK, 1, C, LDC )
       END IF
    ELSE
       IF( LASTV.GT.0 ) THEN
          CALL MPAL_GEMV( 'No transpose', LASTC, LASTV, ONE, C, LDC, &
               V, INCV, ZERO, WORK, 1 )

          CALL MPAL_GER( LASTC, LASTV, -TAU, WORK, 1, V, INCV, C, LDC )
       END IF
    END IF
    RETURN

    END
    
     SUBROUTINE MPAL_DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
                            WORK, INFO )

         CHARACTER          SIDE, TRANS
         INTEGER            INFO, K, LDA, LDC, M, N

         TYPE(MPAL_ST)      A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )

         DOUBLE PRECISION   ONE
         PARAMETER          ( ONE = 1.0D+0 )

         LOGICAL            LEFT, NOTRAN
         INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
         DOUBLE PRECISION   AII

         LOGICAL            LSAME
         EXTERNAL           LSAME

         EXTERNAL           XERBLA

         INTRINSIC          MAX

         INFO = 0
         LEFT = LSAME( SIDE, 'L' )
         NOTRAN = LSAME( TRANS, 'N' )
         IF( LEFT ) THEN
            NQ = M
         ELSE
            NQ = N
         END IF
         IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
            INFO = -1
         ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
            INFO = -2
         ELSE IF( M.LT.0 ) THEN
            INFO = -3
         ELSE IF( N.LT.0 ) THEN
            INFO = -4
         ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
            INFO = -5
         ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
            INFO = -7
         ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
            INFO = -10
         END IF
         IF( INFO.NE.0 ) THEN
            CALL XERBLA( 'DORM2R', -INFO )
            RETURN
         END IF
         IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) RETURN

         IF( ( LEFT .AND. .NOT.NOTRAN ) .OR. ( .NOT.LEFT .AND. NOTRAN ) ) &
              THEN
            I1 = 1
            I2 = K
            I3 = 1
         ELSE
            I1 = K
            I2 = 1
            I3 = -1
         END IF
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         END IF
         DO 10 I = I1, I2, I3
            IF( LEFT ) THEN
               MI = M - I + 1
               IC = I
            ELSE
               NI = N - I + 1
               JC = I
            END IF
            AII = A( I, I )
            A( I, I ) = ONE
            CALL MPAL_DLARF( SIDE, MI, NI, A( I, I ), 1, TAU( I ), C( IC, JC ), &
                        LDC, WORK )
            A( I, I ) = AII
      10 CONTINUE
         RETURN

         END

SUBROUTINE MPAL_DLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )

        CHARACTER          DIRECT, STOREV
        INTEGER            K, LDT, LDV, N

        TYPE(MPAL_ST)      T( LDT, * ), TAU( * ), V( LDV, * )

        TYPE(MPAL_ST)      ONE, ZERO

        INTEGER            I, J, PREVLASTV, LASTV

        LOGICAL            LSAME
        EXTERNAL           LSAME

        ONE = 1.0D+0
        ZERO = 0.0D+0

        IF( N.EQ.0 ) &
            RETURN

        IF( LSAME( DIRECT, 'F' ) ) THEN
            PREVLASTV = N
            DO I = 1, K
            PREVLASTV = MAX( I, PREVLASTV )
            IF( TAU( I ).EQ.ZERO ) THEN
                DO J = 1, I
                    T( J, I ) = ZERO
                END DO
            ELSE
                IF( LSAME( STOREV, 'C' ) ) THEN
                    DO LASTV = N, I+1, -1
                        IF( V( LASTV, I ).NE.ZERO ) EXIT
                    END DO
                    DO J = 1, I-1
                        T( J, I ) = -TAU( I ) * V( I , J )
                    END DO
                    J = MIN( LASTV, PREVLASTV )
! *
! *                 T(1:i-1,i) := - tau(i) * V(i:j,1:i-1)**T * V(i:j,i)
! *
                    CALL MPAL_GEMV( 'Transpose', J-I, I-1, -TAU( I ), &
                                V( I+1, 1 ), LDV, V( I+1, I ), 1, ONE, &
                                T( 1, I ), 1 )
                ELSE
                    DO LASTV = N, I+1, -1
                        IF( V( I, LASTV ).NE.ZERO ) EXIT
                    END DO
                    DO J = 1, I-1
                        T( J, I ) = -TAU( I ) * V( J , I )
                    END DO
                    J = MIN( LASTV, PREVLASTV )
! *
! *                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:j) * V(i,i:j)**T
! *
                    CALL MPAL_GEMV( 'No transpose', I-1, J-I, -TAU( I ), &
                                 V( 1, I+1 ), LDV, V( I, I+1 ), LDV, ONE, &
                                 T( 1, I ), 1 )
                END IF
! *
! *              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i)
! *
                CALL MPAL_TRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T, &
                              LDT, T( 1, I ), 1 )
                T( I, I ) = TAU( I )
                IF( I.GT.1 ) THEN
                    PREVLASTV = MAX( PREVLASTV, LASTV )
                ELSE
                    PREVLASTV = LASTV
                END IF
            END IF
            END DO
        ELSE
            PREVLASTV = 1
            DO I = K, 1, -1
            IF( TAU( I ).EQ.ZERO ) THEN
! *
! *              H(i)  =  I
! *
                DO J = I, K
                    T( J, I ) = ZERO
                END DO
            ELSE
! *
! *              general case
! *
                IF( I.LT.K ) THEN
                    IF( LSAME( STOREV, 'C' ) ) THEN
! *                    Skip any leading zeros.
                        DO LASTV = 1, I-1
                        IF( V( LASTV, I ).NE.ZERO ) EXIT
                        END DO
                        DO J = I+1, K
                        T( J, I ) = -TAU( I ) * V( N-K+I , J )
                        END DO
                        J = MAX( LASTV, PREVLASTV )
! *
! *                    T(i+1:k,i) = -tau(i) * V(j:n-k+i,i+1:k)**T * V(j:n-k+i,i)
! *
                        CALL MPAL_GEMV( 'Transpose', N-K+I-J, K-I, -TAU( I ), &
                                    V( J, I+1 ), LDV, V( J, I ), 1, ONE, &
                                    T( I+1, I ), 1 )
                    ELSE
                        DO LASTV = 1, I-1
                        IF( V( I, LASTV ).NE.ZERO ) EXIT
                        END DO
                        DO J = I+1, K
                        T( J, I ) = -TAU( I ) * V( J, N-K+I )
                        END DO
                        J = MAX( LASTV, PREVLASTV )
! *
! *                    T(i+1:k,i) = -tau(i) * V(i+1:k,j:n-k+i) * V(i,j:n-k+i)**T
! *
                        CALL MPAL_GEMV( 'No transpose', K-I, N-K+I-J, &
                             -TAU( I ), V( I+1, J ), LDV, V( I, J ), LDV, &
                             ONE, T( I+1, I ), 1 )
                    END IF
! *
! *                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i)
! *
                    CALL MPAL_TRMV( 'Lower', 'No transpose', 'Non-unit', K-I, &
                                 T( I+1, I+1 ), LDT, T( I+1, I ), 1 )
                    IF( I.GT.1 ) THEN
                        PREVLASTV = MIN( PREVLASTV, LASTV )
                    ELSE
                        PREVLASTV = LASTV
                    END IF
                END IF
                T( I, I ) = TAU( I )
            END IF
            END DO
        END IF
        RETURN
! *
! *     End of DLARFT
! *
        END
            
        SUBROUTINE MPAL_DLARFB( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV, &
                                T, LDT, C, LDC, WORK, LDWORK )

             CHARACTER          DIRECT, SIDE, STOREV, TRANS
             INTEGER            K, LDC, LDT, LDV, LDWORK, M, N

             TYPE(MPAL_ST)      C( LDC, * ), T( LDT, * ), V( LDV, * ), &
                                WORK( LDWORK, * )

             TYPE(MPAL_ST)      ONE

             CHARACTER          TRANST
             INTEGER            I, J

             LOGICAL            LSAME
             EXTERNAL           LSAME

             ONE = 1.0D+0

             IF( M.LE.0 .OR. N.LE.0 ) &
                RETURN

             IF( LSAME( TRANS, 'N' ) ) THEN
                TRANST = 'T'
             ELSE
                TRANST = 'N'
             END IF

             IF( LSAME( STOREV, 'C' ) ) THEN

                IF( LSAME( DIRECT, 'F' ) ) THEN

                   IF( LSAME( SIDE, 'L' ) ) THEN
                      DO 10 J = 1, K
                         CALL MPAL_DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
          10          CONTINUE

                      CALL MPAL_TRMM( 'Right', 'Lower', 'No transpose', 'Unit', N, &
                                  K, ONE, V, LDV, WORK, LDWORK )
                      IF( M.GT.K ) THEN
                         CALL MPAL_GEMM( 'Transpose', 'No transpose', N, K, M-K, &
                                     ONE, C( K+1, 1 ), LDC, V( K+1, 1 ), LDV, &
                                     ONE, WORK, LDWORK )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K, &
                                  ONE, T, LDT, WORK, LDWORK )
                      IF( M.GT.K ) THEN
                         CALL MPAL_GEMM( 'No transpose', 'Transpose', M-K, N, K, &
                                     -ONE, V( K+1, 1 ), LDV, WORK, LDWORK, ONE, &
                                     C( K+1, 1 ), LDC )
                      END IF

                      CALL MPAL_TRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K, &
                                  ONE, V, LDV, WORK, LDWORK )
                      DO 30 J = 1, K
                         DO 20 I = 1, N
                            C( J, I ) = C( J, I ) - WORK( I, J )
          20             CONTINUE
          30          CONTINUE

                   ELSE IF( LSAME( SIDE, 'R' ) ) THEN
                      DO 40 J = 1, K
                         CALL MPAL_DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
          40          CONTINUE
                      CALL MPAL_TRMM( 'Right', 'Lower', 'No transpose', 'Unit', M, &
                                  K, ONE, V, LDV, WORK, LDWORK )
                      IF( N.GT.K ) THEN
                         CALL MPAL_GEMM( 'No transpose', 'No transpose', M, K, N-K, &
                                     ONE, C( 1, K+1 ), LDC, V( K+1, 1 ), LDV, &
                                     ONE, WORK, LDWORK )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K, &
                                  ONE, T, LDT, WORK, LDWORK )
                      IF( N.GT.K ) THEN
                         CALL MPAL_GEMM( 'No transpose', 'Transpose', M, N-K, K, &
                                     -ONE, WORK, LDWORK, V( K+1, 1 ), LDV, ONE, &
                                     C( 1, K+1 ), LDC )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K, &
                                  ONE, V, LDV, WORK, LDWORK )
                      DO 60 J = 1, K
                         DO 50 I = 1, M
                            C( I, J ) = C( I, J ) - WORK( I, J )
          50             CONTINUE
          60          CONTINUE
                   END IF
                ELSE
                   IF( LSAME( SIDE, 'L' ) ) THEN
                      DO 70 J = 1, K
                         CALL MPAL_DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
          70          CONTINUE
                      CALL MPAL_TRMM( 'Right', 'Upper', 'No transpose', 'Unit', N, &
                                  K, ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
                      IF( M.GT.K ) THEN
                         CALL MPAL_GEMM( 'Transpose', 'No transpose', N, K, M-K, &
                                     ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K, &
                                  ONE, T, LDT, WORK, LDWORK )
                      IF( M.GT.K ) THEN
                         CALL MPAL_GEMM( 'No transpose', 'Transpose', M-K, N, K, &
                                     -ONE, V, LDV, WORK, LDWORK, ONE, C, LDC )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K, &
                                  ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
                      DO 90 J = 1, K
                         DO 80 I = 1, N
                            C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
          80             CONTINUE
          90          CONTINUE
                   ELSE IF( LSAME( SIDE, 'R' ) ) THEN
                      DO 100 J = 1, K
                         CALL MPAL_DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
         100          CONTINUE
                      CALL MPAL_TRMM( 'Right', 'Upper', 'No transpose', 'Unit', M, &
                                  K, ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
                      IF( N.GT.K ) THEN
                         CALL MPAL_GEMM( 'No transpose', 'No transpose', M, K, N-K, &
                                     ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K, &
                                  ONE, T, LDT, WORK, LDWORK )
                      IF( N.GT.K ) THEN
                         CALL MPAL_GEMM( 'No transpose', 'Transpose', M, N-K, K, &
                                     -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
                      END IF

                      CALL MPAL_TRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K, &
                                  ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
                      DO 120 J = 1, K
                         DO 110 I = 1, M
                            C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
         110             CONTINUE
         120          CONTINUE
                   END IF
                END IF
             ELSE IF( LSAME( STOREV, 'R' ) ) THEN
                IF( LSAME( DIRECT, 'F' ) ) THEN
                   IF( LSAME( SIDE, 'L' ) ) THEN
       
                      DO 130 J = 1, K
                         CALL MPAL_DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
         130          CONTINUE
                      CALL MPAL_TRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K, &
                                  ONE, V, LDV, WORK, LDWORK )
                      IF( M.GT.K ) THEN
                         CALL MPAL_GEMM( 'Transpose', 'Transpose', N, K, M-K, ONE, &
                                     C( K+1, 1 ), LDC, V( 1, K+1 ), LDV, ONE, &
                                     WORK, LDWORK )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K, &
                                  ONE, T, LDT, WORK, LDWORK )
                      IF( M.GT.K ) THEN
                         CALL MPAL_GEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE, &
                                     V( 1, K+1 ), LDV, WORK, LDWORK, ONE, &
                                     C( K+1, 1 ), LDC )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Upper', 'No transpose', 'Unit', N, &
                                  K, ONE, V, LDV, WORK, LDWORK )
                      DO 150 J = 1, K
                         DO 140 I = 1, N
                            C( J, I ) = C( J, I ) - WORK( I, J )
         140             CONTINUE
         150          CONTINUE
                   ELSE IF( LSAME( SIDE, 'R' ) ) THEN
                      DO 160 J = 1, K
                         CALL MPAL_DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
         160          CONTINUE
                      CALL MPAL_TRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K, &
                                  ONE, V, LDV, WORK, LDWORK )
                      IF( N.GT.K ) THEN
                         CALL MPAL_GEMM( 'No transpose', 'Transpose', M, K, N-K, &
                                     ONE, C( 1, K+1 ), LDC, V( 1, K+1 ), LDV, &
                                     ONE, WORK, LDWORK )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K, &
                                  ONE, T, LDT, WORK, LDWORK )
                      IF( N.GT.K ) THEN
                         CALL MPAL_GEMM( 'No transpose', 'No transpose', M, N-K, K, &
                                     -ONE, WORK, LDWORK, V( 1, K+1 ), LDV, ONE, &
                                     C( 1, K+1 ), LDC )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Upper', 'No transpose', 'Unit', M, &
                                  K, ONE, V, LDV, WORK, LDWORK )
                      DO 180 J = 1, K
                         DO 170 I = 1, M
                            C( I, J ) = C( I, J ) - WORK( I, J )
         170             CONTINUE
         180          CONTINUE
                   END IF
                ELSE
                   IF( LSAME( SIDE, 'L' ) ) THEN
                      DO 190 J = 1, K
                         CALL MPAL_DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
         190          CONTINUE
                      CALL MPAL_TRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K, &
                                  ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
                      IF( M.GT.K ) THEN
                         CALL MPAL_GEMM( 'Transpose', 'Transpose', N, K, M-K, ONE, &
                                     C, LDC, V, LDV, ONE, WORK, LDWORK )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K, &
                                  ONE, T, LDT, WORK, LDWORK )
                      IF( M.GT.K ) THEN
                         CALL MPAL_GEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE, &
                                     V, LDV, WORK, LDWORK, ONE, C, LDC )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Lower', 'No transpose', 'Unit', N, &
                                  K, ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
                      DO 210 J = 1, K
                         DO 200 I = 1, N
                            C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
         200             CONTINUE
         210          CONTINUE
                   ELSE IF( LSAME( SIDE, 'R' ) ) THEN
                      DO 220 J = 1, K
                         CALL MPAL_DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
         220          CONTINUE
                      CALL MPAL_TRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K, &
                                  ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
                      IF( N.GT.K ) THEN
                         CALL MPAL_GEMM( 'No transpose', 'Transpose', M, K, N-K, &
                                     ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K, &
                                  ONE, T, LDT, WORK, LDWORK )
                      IF( N.GT.K ) THEN
                         CALL MPAL_GEMM( 'No transpose', 'No transpose', M, N-K, K, &
                                     -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
                      END IF
                      CALL MPAL_TRMM( 'Right', 'Lower', 'No transpose', 'Unit', M, &
                                  K, ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
                      DO 240 J = 1, K
                         DO 230 I = 1, M
                            C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
         230             CONTINUE
         240          CONTINUE
                   END IF
                END IF
             END IF
             RETURN
             END
       

SUBROUTINE MPAL_DORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
                        WORK, LWORK, INFO )

     CHARACTER          SIDE, TRANS
     INTEGER            INFO, K, LDA, LDC, LWORK, M, N

     TYPE(MPAL_ST)      A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )

     INTEGER            NBMAX, LDT, TSIZE
     PARAMETER          ( NBMAX = 64, LDT = NBMAX+1, &
                          TSIZE = LDT*NBMAX )

     LOGICAL            LEFT, LQUERY, NOTRAN
     INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWT, JC, LDWORK, &
                        LWKOPT, MI, NB, NBMIN, NI, NQ, NW

     LOGICAL            LSAME
     INTEGER            ILAENV
     EXTERNAL           LSAME, ILAENV

     EXTERNAL           XERBLA

     INTRINSIC          MAX, MIN

     INFO = 0
     LEFT = LSAME( SIDE, 'L' )
     NOTRAN = LSAME( TRANS, 'N' )
     LQUERY = ( LWORK.EQ.-1 )
! *
! *     NQ is the order of Q and NW is the minimum dimension of WORK
! *
     IF( LEFT ) THEN
        NQ = M
        NW = N
     ELSE
        NQ = N
        NW = M
     END IF
     IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
        INFO = -1
     ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
        INFO = -2
     ELSE IF( M.LT.0 ) THEN
        INFO = -3
     ELSE IF( N.LT.0 ) THEN
        INFO = -4
     ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
        INFO = -5
     ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
        INFO = -7
     ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
        INFO = -10
     ELSE IF( LWORK.LT.MAX( 1, NW ) .AND. .NOT.LQUERY ) THEN
        INFO = -12
     END IF

     IF( INFO.EQ.0 ) THEN
! *
! *        Compute the workspace requirements
! *
        NB = MIN( NBMAX, ILAENV( 1, 'DORMQR', SIDE // TRANS, M, N, K, &
             -1 ) )
        LWKOPT = MAX( 1, NW )*NB + TSIZE
        WORK( 1 ) = LWKOPT
     END IF

     IF( INFO.NE.0 ) THEN
        CALL XERBLA( 'DORMQR', -INFO )
        RETURN
     ELSE IF( LQUERY ) THEN
        RETURN
     END IF
! *
! *     Quick return if possible
! *
     IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
        WORK( 1 ) = 1
        RETURN
     END IF

     NBMIN = 2
     LDWORK = NW
     IF( NB.GT.1 .AND. NB.LT.K ) THEN
        IF( LWORK.LT.NW*NB+TSIZE ) THEN
           NB = (LWORK-TSIZE) / LDWORK
           NBMIN = MAX( 2, ILAENV( 2, 'DORMQR', SIDE // TRANS, M, N, K, &
                   -1 ) )
        END IF
     END IF

     IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
! *
! *        Use unblocked code
! *
        CALL MPAL_DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK, &
                     IINFO )
     ELSE
! *
! *        Use blocked code
! *
        IWT = 1 + NW*NB
        IF( ( LEFT .AND. .NOT.NOTRAN ) .OR. &
            ( .NOT.LEFT .AND. NOTRAN ) ) THEN
           I1 = 1
           I2 = K
           I3 = NB
        ELSE
           I1 = ( ( K-1 ) / NB )*NB + 1
           I2 = 1
           I3 = -NB
        END IF

        IF( LEFT ) THEN
           NI = N
           JC = 1
        ELSE
           MI = M
           IC = 1
        END IF

        DO 10 I = I1, I2, I3
           IB = MIN( NB, K-I+1 )
! *
! *           Form the triangular factor of the block reflector
! *           H = H(i) H(i+1) . . . H(i+ib-1)
! *
           CALL MPAL_DLARFT( 'Forward', 'Columnwise', NQ-I+1, IB, A( I, I ), &
                        LDA, TAU( I ), WORK( IWT ), LDT )
           IF( LEFT ) THEN
! *
! *              H or H**T is applied to C(i:m,1:n)
! *
              MI = M - I + 1
              IC = I
           ELSE
! *
! *              H or H**T is applied to C(1:m,i:n)
! *
              NI = N - I + 1
              JC = I
           END IF
! *
! *           Apply H or H**T
! *
           CALL MPAL_DLARFB( SIDE, TRANS, 'Forward', 'Columnwise', MI, NI, &
                        IB, A( I, I ), LDA, WORK( IWT ), LDT, &
                        C( IC, JC ), LDC, WORK, LDWORK )
  10    CONTINUE
     END IF
     WORK( 1 ) = LWKOPT
     RETURN
! *
! *     End of DORMQR
! *
     END
