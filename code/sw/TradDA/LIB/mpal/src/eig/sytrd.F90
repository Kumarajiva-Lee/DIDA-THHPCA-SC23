
SUBROUTINE MPAL_DLARFG( N, ALPHA, X, INCX, TAU )
    
    INTEGER            INCX, N
    TYPE(MPAL_ST)      ALPHA, TAU
    TYPE(MPAL_ST)      X( * )
    
    TYPE(MPAL_ST)      ONE, ZERO
    
    INTEGER            J, KNT
    TYPE(MPAL_ST)      BETA
    TYPE(MPAL_ST)      RSAFMN, SAFMIN, XNORM
    
    INTRINSIC          ABS, SIGN

    ONE = 1.0D+0
    ZERO = 0.0D+0
    
    IF( N.LE.1 ) THEN
        TAU = ZERO
        RETURN
    END IF

    XNORM = MPAL_DNRM2( N-1, X, INCX)
    
    IF( XNORM.EQ.ZERO ) THEN
        TAU = ZERO
    ELSE

        BETA = -SIGN( MPAL_DLAPY2( ALPHA, XNORM ), ALPHA )
        SAFMIN = MPAL_DLAMCH( 'S' ) / MPAL_DLAMCH( 'E' )
        KNT = 0

        IF( ABS( BETA ).LT.SAFMIN ) THEN
            RSAFMN = ONE / SAFMIN
            10  CONTINUE
            KNT = KNT + 1
            CALL MPAL_DSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHA = ALPHA*RSAFMN
            IF( (ABS( BETA ).LT.SAFMIN) .AND. (KNT .LT. 20) ) GO TO 10

            XNORM = MPAL_DNRM2( N-1, X, INCX )
            BETA = -SIGN( MPAL_DLAPY2( ALPHA, XNORM ), ALPHA )
        END IF
        TAU = ( BETA-ALPHA ) / BETA
        CALL MPAL_DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )

        DO 20 J = 1, KNT
            BETA = BETA*SAFMIN
        20  CONTINUE
        ALPHA = BETA
    END IF

    RETURN
END SUBROUTINE

SUBROUTINE MPAL_DLATRD( UPLO, N, NB, A, LDA, E, TAU, W, LDW )

    CHARACTER          UPLO
    INTEGER            LDA, LDW, N, NB
    TYPE(MPAL_ST)      A( LDA, * ), E( * ), TAU( * ), W( LDW, * )
    TYPE(MPAL_ST)      ZERO, ONE, HALF
    INTEGER            I, IW
    TYPE(MPAL_ST)      ALPHA
    LOGICAL            LSAME
    EXTERNAL           LSAME
    INTRINSIC          MIN
    
    ZERO = 0.0D+0
    ONE = 1.0D+0
    HALF = 0.5D+0

    IF( N.LE.0 ) RETURN

    IF( LSAME( UPLO, 'U' ) ) THEN
        DO 10 I = N, N - NB + 1, -1
            IW = I - N + NB
            IF( I.LT.N ) THEN
                CALL MPAL_GEMV( 'No transpose', I, N-I, -ONE, A( 1, I+1 ), &
                               LDA, W( I, IW+1 ), LDW, ONE, A( 1, I ), 1 )
                CALL MPAL_GEMV( 'No transpose', I, N-I, -ONE, W( 1, IW+1 ), &
                               LDW, A( I, I+1 ), LDA, ONE, A( 1, I ), 1 )
                END IF
                IF( I.GT.1 ) THEN
    
                CALL MPAL_DLARFG( I-1, A( I-1, I ), A( 1, I ), 1, TAU( I-1 ) )
                E( I-1 ) = A( I-1, I )
                A( I-1, I ) = ONE
    
                CALL MPAL_SYMV( 'Upper', I-1, ONE, A, LDA, A( 1, I ), 1, &
                            ZERO, W( 1, IW ), 1 )
                IF( I.LT.N ) THEN
                    CALL MPAL_GEMV( 'Transpose', I-1, N-I, ONE, W( 1, IW+1 ), &
                        LDW, A( 1, I ), 1, ZERO, W( I+1, IW ), 1 )
                    CALL MPAL_GEMV( 'No transpose', I-1, N-I, -ONE, &
                        A( 1, I+1 ), LDA, W( I+1, IW ), 1, ONE, &
                        W( 1, IW ), 1 )
                    CALL MPAL_GEMV( 'Transpose', I-1, N-I, ONE, A( 1, I+1 ), &
                        LDA, A( 1, I ), 1, ZERO, W( I+1, IW ), 1 )
                    CALL MPAL_GEMV( 'No transpose', I-1, N-I, -ONE, &
                        W( 1, IW+1 ), LDW, W( I+1, IW ), 1, ONE, &
                        W( 1, IW ), 1 )
                END IF
                CALL MPAL_DSCAL( I-1, TAU( I-1 ), W( 1, IW ), 1 )
                ALPHA = -HALF*TAU( I-1 )*MPAL_DOT( I-1, W( 1, IW ), 1, &
                    A( 1, I ), 1 )
                CALL MPAL_DAXPY( I-1, ALPHA, A( 1, I ), 1, W( 1, IW ), 1 )
            END IF
       10    CONTINUE
        ELSE
            DO 20 I = 1, NB

                CALL MPAL_GEMV( 'No transpose', N-I+1, I-1, -ONE, A( I, 1 ), &
                    LDA, W( I, 1 ), LDW, ONE, A( I, I ), 1 )

                CALL MPAL_GEMV( 'No transpose', N-I+1, I-1, -ONE, W( I, 1 ), &
                    LDW, A( I, 1 ), LDA, ONE, A( I, I ), 1 )

                IF( I.LT.N ) THEN
                    CALL MPAL_DLARFG( N-I, A( I+1, I ), A( MIN( I+2, N ), I ), 1, &
                        TAU( I ) )

                   E( I ) = A( I+1, I )
                   A( I+1, I ) = ONE
                   CALL MPAL_SYMV( 'Lower', N-I, ONE, A( I+1, I+1 ), LDA, &
                        A( I+1, I ), 1, ZERO, W( I+1, I ), 1 )
                   CALL MPAL_GEMV( 'Transpose', N-I, I-1, ONE, W( I+1, 1 ), LDW, &
                        A( I+1, I ), 1, ZERO, W( 1, I ), 1 )
                   CALL MPAL_GEMV( 'No transpose', N-I, I-1, -ONE, A( I+1, 1 ), &
                        LDA, W( 1, I ), 1, ONE, W( I+1, I ), 1 )
                   CALL MPAL_GEMV( 'Transpose', N-I, I-1, ONE, A( I+1, 1 ), LDA, &
                        A( I+1, I ), 1, ZERO, W( 1, I ), 1 )

                   CALL MPAL_GEMV( 'No transpose', N-I, I-1, -ONE, W( I+1, 1 ), &
                        LDW, W( 1, I ), 1, ONE, W( I+1, I ), 1 )

                    !CALL MPAL_REPORT(W(1: N, I), N, "W1")

                   CALL MPAL_DSCAL( N-I, TAU( I ), W( I+1, I ), 1 )
                
                   ! mixed
                   ! dot computation need to be calculated in double
                   ALPHA = -HALF*TAU( I )*MPAL_DOT( N-I, W( I+1, I ), 1, &
                        A( I+1, I ), 1 )

                    !CALL MPAL_REPORT(W(I+1, 1:N), N, "W2")

                   CALL MPAL_DAXPY( N-I, ALPHA, A( I+1, I ), 1, W( I+1, I ), 1 )

                   !CALL MPAL_REPORT(ALPHA, "ALPHA")
                   !CALL MPAL_REPORT(W(I+1, 1:N), N, "W2")
                END IF

                !CALL MPAL_REPORT(A(1: N, 1: N), N, N, "DLARFG B")

       20    CONTINUE
            END IF
        RETURN
        
END SUBROUTINE

SUBROUTINE MPAL_DSYTD2( UPLO, N, A, LDA, D, E, TAU, INFO )

        CHARACTER          UPLO
        INTEGER            INFO, LDA, N
 
        TYPE(MPAL_ST)      A( LDA, * ), D( * ), E( * ), TAU( * )
    
        TYPE(MPAL_ST)      ONE, ZERO, HALF

        LOGICAL            UPPER
        INTEGER            I
        TYPE(MPAL_ST)      ALPHA
        TYPE(MPAL_ST)      TAUI
    
        EXTERNAL           XERBLA
 
        LOGICAL            LSAME
        EXTERNAL           LSAME
    
        INTRINSIC          MAX, MIN

        ONE = 1.0D0
        ZERO = 0.0D0
        HALF = 1.0D0 / 2.0D0
            
        INFO = 0
        UPPER = LSAME( UPLO, 'U' )
        IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
            INFO = -1
        ELSE IF( N.LT.0 ) THEN
            INFO = -2
        ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
            INFO = -4
        END IF
        IF( INFO.NE.0 ) THEN
            CALL XERBLA( 'DSYTD2', -INFO )
            RETURN
        END IF
    
        IF( N.LE.0 ) RETURN
            IF( UPPER ) THEN
    
            DO 10 I = N - 1, 1, -1
    
                CALL MPAL_DLARFG( I, A( I, I+1 ), A( 1, I+1 ), 1, TAUI )
                E( I ) = A( I, I+1 )
                IF( TAUI.NE.ZERO ) THEN
                    
                   A( I, I+1 ) = ONE
                   CALL MPAL_SYMV( UPLO, I, TAUI, A, LDA, A( 1, I+1 ), 1, ZERO, &
                           TAU, 1 )

                   ALPHA = -HALF*TAUI*MPAL_DOT( I, TAU, 1, A( 1, I+1 ), 1 )
                   CALL MPAL_DAXPY( I, ALPHA, A( 1, I+1 ), 1, TAU, 1 )

                   CALL MPAL_DSYR2( UPLO, I, -ONE, A( 1, I+1 ), 1, TAU, 1, A, &
                               LDA )

                   A( I, I+1 ) = E( I )
                END IF
                D( I+1 ) = A( I+1, I+1 )
                TAU( I ) = TAUI
         10    CONTINUE
            D( 1 ) = A( 1, 1 )
        ELSE

            DO 20 I = 1, N - 1
! *
! *           Generate elementary reflector H(i) = I - tau * v * v**T
! *           to annihilate A(i+2:n,i)
! *

                CALL MPAL_DLARFG( N-I, A( I+1, I ), A( MIN( I+2, N ), I ), 1, &
                            TAUI )
                
                !CALL MPAL_REPORT(A, N, N, "A2")

                E( I ) = A( I+1, I )

                IF( TAUI.NE.ZERO ) THEN
    
                   A( I+1, I ) = ONE
! *
! *              Compute  x := tau * A * v  storing y in TAU(i:n-1)
! *
                    CALL MPAL_SYMV( UPLO, N-I, TAUI, A( I+1, I+1 ), LDA, &
                                A( I+1, I ), 1, ZERO, TAU( I ), 1 )

                    !CALL MPAL_REPORT(TAU, N - I, "TAU SYMV")
! *
! *              Compute  w := x - 1/2 * tau * (x**T * v) * v
! *
                    ALPHA = -HALF*TAUI*MPAL_DOT( N-I, TAU( I ), 1, A( I+1, I ), &
                                1 )
                    
                    !CALL MPAL_REPORT(ALPHA*A(10, I), "A")
                    !CALL MPAL_REPORT(TAU(9), "TAU7")
                    
                    CALL MPAL_DAXPY( N-I, ALPHA, A( I+1, I ), 1, TAU( I ), 1 )

                    !CALL MPAL_REPORT(TAU, N - I, "TAU DAXPY")

                    CALL MPAL_DSYR2( UPLO, N-I, -ONE, A( I+1, I ), 1, TAU( I ), 1, &
                                A( I+1, I+1 ), LDA )
                   
                    A( I+1, I ) = E( I )
                END IF
                D( I ) = A( I, I )
                TAU( I ) = TAUI

       20    CONTINUE
            D( N ) = A( N, N )

        END IF
        
        RETURN
    
        END
    

SUBROUTINE MPAL_DSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )
        
        CHARACTER          UPLO
        INTEGER            INFO, LDA, LWORK, N

        TYPE(MPAL_ST)      A( LDA, * ), D( * ), E( * ), TAU( * ), &
                           WORK( * )
    
        TYPE(MPAL_ST)      ONE
    
        LOGICAL            LQUERY, UPPER
        INTEGER            I, IINFO, IWS, J, KK, LDWORK, LWKOPT, NB, &
                           NBMIN, NX
        
        EXTERNAL           XERBLA
        INTRINSIC          MAX

        LOGICAL            LSAME
        INTEGER            ILAENV
        EXTERNAL           LSAME, ILAENV

        ONE = 1.0D+0
    
        INFO = 0
        UPPER = LSAME( UPLO, 'U' )
        LQUERY = ( LWORK.EQ.-1 )
        IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
            INFO = -1
        ELSE IF( N.LT.0 ) THEN
            INFO = -2
        ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
            INFO = -4
        ELSE IF( LWORK.LT.1 .AND. .NOT.LQUERY ) THEN
            INFO = -9
        END IF
    
        IF( INFO.EQ.0 ) THEN
            NB = ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 )
            LWKOPT = N*NB
            WORK( 1 ) = LWKOPT
        END IF

        IF( INFO.NE.0 ) THEN
            CALL XERBLA( 'DSYTRD', -INFO )
            RETURN
        ELSE IF( LQUERY ) THEN
            RETURN
        END IF
    
        IF( N.EQ.0 ) THEN
            WORK( 1 ) = 1
            RETURN
        END IF

        NX = N
        IWS = 1
        IF( NB.GT.1 .AND. NB.LT.N ) THEN
    ! *
    ! *        Determine when to cross over from blocked to unblocked code
    ! *        (last block is always handled by unblocked code).
    ! *
            NX = MAX( NB, ILAENV( 3, 'DSYTRD', UPLO, N, -1, -1, -1 ) )
            IF( NX.LT.N ) THEN
    ! *
    ! *           Determine if workspace is large enough for blocked code.
    ! *
                LDWORK = N
                IWS = LDWORK*NB
                IF( LWORK.LT.IWS ) THEN
    ! *
    ! *              Not enough workspace to use optimal NB:  determine the
    ! *              minimum value of NB, and reduce NB or force use of
    ! *              unblocked code by setting NX = N.
    ! *
                   NB = MAX( LWORK / LDWORK, 1 )
                   NBMIN = ILAENV( 2, 'DSYTRD', UPLO, N, -1, -1, -1 )
                   IF( NB.LT.NBMIN ) NX = N
                END IF
            ELSE
                NX = N
            END IF
        ELSE
            NB = 1
        END IF
    
        IF( UPPER ) THEN
    ! *
    ! *        Reduce the upper triangle of A.
    ! *        Columns 1:kk are handled by the unblocked method.
    ! *
            KK = N - ( ( N-NX+NB-1 ) / NB )*NB
                DO 20 I = N - NB + 1, KK + 1, -NB
    ! *
    ! *           Reduce columns i:i+nb-1 to tridiagonal form and form the
    ! *           matrix W which is needed to update the unreduced part of
    ! *           the matrix
    ! *
                    CALL MPAL_DLATRD( UPLO, I+NB-1, NB, A, LDA, E, TAU, WORK, &
                            LDWORK )
    ! *
    ! *           Update the unreduced submatrix A(1:i-1,1:i-1), using an
    ! *           update of the form:  A := A - V*W**T - W*V**T
    ! *
                    CALL MPAL_DSYR2K( UPLO, 'No transpose', I-1, NB, -ONE, A( 1, I ), &
                            LDA, WORK, LDWORK, ONE, A, LDA )
    ! *
    ! *           Copy superdiagonal elements back into A, and diagonal
    ! *           elements into D
    ! *
                DO 10 J = I, I + NB - 1
                   A( J-1, J ) = E( J-1 )
                   D( J ) = A( J, J )
       10       CONTINUE
       20    CONTINUE
    ! *
    ! *        Use unblocked code to reduce the last or only block
    ! *
            CALL MPAL_DSYTD2( UPLO, KK, A, LDA, D, E, TAU, IINFO )
        ELSE
    ! *
    ! *        Reduce the lower triangle of A
    ! *
            DO 40 I = 1, N - NX, NB
    ! *
    ! *           Reduce columns i:i+nb-1 to tridiagonal form and form the
    ! *           matrix W which is needed to update the unreduced part of
    ! *           the matrix
    ! *
                CALL MPAL_DLATRD( UPLO, N-I+1, NB, A( I, I ), LDA, E( I ), &
                                TAU( I ), WORK, LDWORK )

    ! *
    ! *           Update the unreduced submatrix A(i+ib:n,i+ib:n), using
    ! *           an update of the form:  A := A - V*W**T - W*V**T
    ! *
                CALL MPAL_DSYR2K( UPLO, 'No transpose', N-I-NB+1, NB, -ONE, &
                                A( I+NB, I ), LDA, WORK( NB+1 ), LDWORK, ONE, &
                                A( I+NB, I+NB ), LDA )
    ! *
    ! *           Copy subdiagonal elements back into A, and diagonal
    ! *           elements into D
    ! *
                DO 30 J = I, I + NB - 1
                    A( J+1, J ) = E( J )
                    D( J ) = A( J, J )
    30          CONTINUE

    40      CONTINUE

    ! *
    ! *        Use unblocked code to reduce the last or only block
    ! *

            CALL MPAL_DSYTD2( UPLO, N-I+1, A( I, I ), LDA, D( I ), E( I ), &
                    TAU( I ), IINFO )

            !
            
        END IF

        WORK( 1 ) = LWKOPT
        RETURN
    ! *
    ! *     End of DSYTRD
    ! *
        END
    