SUBROUTINE MPAL_DSCAL(N,DA,DX,INCX)

    DOUBLE PRECISION DA
    INTEGER INCX,N
    TYPE(MPAL_ST) DX(*)
    
    INTEGER I,M,MP1,NINCX
    INTRINSIC MOD
    
    IF (N.LE.0 .OR. INCX.LE.0) RETURN
    IF (INCX.EQ.1) THEN
        M = MOD(N,5)
        IF (M.NE.0) THEN
            DO I = 1,M
                DX(I) = DX(I)*DA
            END DO
            IF (N.LT.5) RETURN
        END IF
        MP1 = M + 1
        DO I = MP1,N,5
            DX(I) = DX(I)*DA
            DX(I+1) = DX(I+1)*DA
            DX(I+2) = DX(I+2)*DA
            DX(I+3) = DX(I+3)*DA
            DX(I+4) = DX(I+4)*DA
        END DO
    ELSE
        NINCX = N*INCX
        DO I = 1,NINCX,INCX
            DX(I) = DX(I)*DA
        END DO
    END IF
    RETURN
END

SUBROUTINE MPAL_DLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )

    CHARACTER          TYPE
    INTEGER            INFO, KL, KU, LDA, M, N
    DOUBLE PRECISION   CFROM, CTO
    
    TYPE(MPAL_ST)      A( LDA, * )

    DOUBLE PRECISION   ZERO, ONE
    PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
    
    LOGICAL            DONE
    INTEGER            I, ITYPE, J, K1, K2, K3, K4
    DOUBLE PRECISION   BIGNUM, CFROM1, CFROMC, CTO1, CTOC, MUL, SMLNUM
    
    LOGICAL            LSAME, DISNAN
    DOUBLE PRECISION   DLAMCH
    EXTERNAL           LSAME, DLAMCH, DISNAN
    
    INTRINSIC          ABS, MAX, MIN

    INFO = 0
    IF( LSAME( TYPE, 'G' ) ) THEN
        ITYPE = 0
    ELSE IF( LSAME( TYPE, 'L' ) ) THEN
        ITYPE = 1
    ELSE IF( LSAME( TYPE, 'U' ) ) THEN
        ITYPE = 2
    ELSE IF( LSAME( TYPE, 'H' ) ) THEN
        ITYPE = 3
    ELSE IF( LSAME( TYPE, 'B' ) ) THEN
        ITYPE = 4
    ELSE IF( LSAME( TYPE, 'Q' ) ) THEN
        ITYPE = 5
    ELSE IF( LSAME( TYPE, 'Z' ) ) THEN
        ITYPE = 6
    ELSE
        ITYPE = -1
    END IF

    IF( ITYPE.EQ.-1 ) THEN
        INFO = -1
    ELSE IF( CFROM.EQ.ZERO .OR. DISNAN(CFROM) ) THEN
        INFO = -4
    ELSE IF( DISNAN(CTO) ) THEN
        INFO = -5
    ELSE IF( M.LT.0 ) THEN
        INFO = -6
    ELSE IF( N.LT.0 .OR. ( ITYPE.EQ.4 .AND. N.NE.M ) .OR. &
        ( ITYPE.EQ.5 .AND. N.NE.M ) ) THEN 
            INFO = -7
    ELSE IF( ITYPE.LE.3 .AND. LDA.LT.MAX( 1, M ) ) THEN 
            INFO = -9
    ELSE IF( ITYPE.GE.4 ) THEN
        IF( KL.LT.0 .OR. KL.GT.MAX( M-1, 0 ) ) THEN
            INFO = -2
        ELSE IF( KU.LT.0 .OR. KU.GT.MAX( N-1, 0 ) .OR. &
                ( ( ITYPE.EQ.4 .OR. ITYPE.EQ.5 ) .AND. KL.NE.KU ) ) &
                THEN
            INFO = -3
        ELSE IF( ( ITYPE.EQ.4 .AND. LDA.LT.KL+1 ) .OR. &
                ( ITYPE.EQ.5 .AND. LDA.LT.KU+1 ) .OR. &
                ( ITYPE.EQ.6 .AND. LDA.LT.2*KL+KU+1 ) ) THEN
            INFO = -9
        END IF
    END IF

    IF( INFO.NE.0 ) THEN
        CALL XERBLA( 'DLASCL', -INFO )
        RETURN
    END IF

    IF( N.EQ.0 .OR. M.EQ.0 ) &
        RETURN

    SMLNUM = DLAMCH( 'S' )
    BIGNUM = ONE / SMLNUM

    CFROMC = CFROM
    CTOC = CTO
       
    10  CONTINUE
        CFROM1 = CFROMC*SMLNUM
        IF( CFROM1.EQ.CFROMC ) THEN
    !        CFROMC is an inf.  Multiply by a correctly signed zero for
    !        finite CTOC, or a NaN if CTOC is infinite.
            MUL = CTOC / CFROMC
            DONE = .TRUE.
            CTO1 = CTOC
        ELSE
            CTO1 = CTOC / BIGNUM
            IF( CTO1.EQ.CTOC ) THEN
    !           CTOC is either 0 or an inf.  In both cases, CTOC itself
    !           serves as the correct multiplication factor.
                MUL = CTOC
                DONE = .TRUE.
                CFROMC = ONE
            ELSE IF( ABS( CFROM1 ).GT.ABS( CTOC ) .AND. CTOC.NE.ZERO ) THEN
                MUL = SMLNUM
                DONE = .FALSE.
                CFROMC = CFROM1
            ELSE IF( ABS( CTO1 ).GT.ABS( CFROMC ) ) THEN
                MUL = BIGNUM
                DONE = .FALSE.
                CTOC = CTO1
            ELSE
                MUL = CTOC / CFROMC
                DONE = .TRUE.
            END IF
        END IF

        IF( ITYPE.EQ.0 ) THEN
    ! *
    ! *        Full matrix
    ! *
            DO 30 J = 1, N
                DO 20 I = 1, M
                    A( I, J ) = A( I, J )*MUL
    20          CONTINUE
    30      CONTINUE

        ELSE IF( ITYPE.EQ.1 ) THEN
    ! *
    ! *        Lower triangular matrix
    ! *
            DO 50 J = 1, N
                DO 40 I = J, M
                    A( I, J ) = A( I, J )*MUL
    40          CONTINUE
    50      CONTINUE

        ELSE IF( ITYPE.EQ.2 ) THEN
    ! *
    ! *        Upper triangular matrix
    ! *
            DO 70 J = 1, N
                DO 60 I = 1, MIN( J, M )
                    A( I, J ) = A( I, J )*MUL
    60          CONTINUE
    70      CONTINUE

        ELSE IF( ITYPE.EQ.3 ) THEN
    ! *
    ! *        Upper Hessenberg matrix
    ! *
            DO 90 J = 1, N
                DO 80 I = 1, MIN( J+1, M )
                    A( I, J ) = A( I, J )*MUL
    80          CONTINUE
    90      CONTINUE

        ELSE IF( ITYPE.EQ.4 ) THEN
    ! *
    ! *        Lower half of a symmetric band matrix
    ! *
            K3 = KL + 1
            K4 = N + 1
            DO 110 J = 1, N
                DO 100 I = 1, MIN( K3, K4-J )
                    A( I, J ) = A( I, J )*MUL
    100         CONTINUE
    110     CONTINUE

        ELSE IF( ITYPE.EQ.5 ) THEN
    ! *
    ! *        Upper half of a symmetric band matrix
    ! *
            K1 = KU + 2
            K3 = KU + 1
            DO 130 J = 1, N
                DO 120 I = MAX( K1-J, 1 ), K3
                    A( I, J ) = A( I, J )*MUL
    120         CONTINUE
    130     CONTINUE

        ELSE IF( ITYPE.EQ.6 ) THEN
    ! *
    ! *        Band matrix
    ! *
            K1 = KL + KU + 2
            K2 = KL + 1
            K3 = 2*KL + KU + 1
            K4 = KL + KU + 1 + M
            DO 150 J = 1, N
                DO 140 I = MAX( K1-J, K2 ), MIN( K3, K4-J )
                    A( I, J ) = A( I, J )*MUL
    140         CONTINUE
    150     CONTINUE

        END IF

        IF( .NOT.DONE ) &
            GO TO 10
        
        RETURN
        
    END

DOUBLE PRECISION FUNCTION MPAL_DNRM2(N,X,INCX)
    INTEGER INCX,N

    TYPE(MPAL_ST) X(*)
    DOUBLE PRECISION ONE,ZERO
    PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
    DOUBLE PRECISION ABSXI,NORM,SCALE,SSQ
    INTEGER IX
    IF (N.LT.1 .OR. INCX.LT.1) THEN
        NORM = ZERO
    ELSE IF (N.EQ.1) THEN
        NORM = ABS(MPAL_VAL(X(1)))
    ELSE
        SCALE = ZERO
        SSQ = ONE

        DO 10 IX = 1,1 + (N-1)*INCX,INCX
            IF (MPAL_VAL(X(IX)).NE.ZERO) THEN
                ABSXI = ABS(MPAL_VAL(X(IX)))
                IF (SCALE.LT.ABSXI) THEN
                    SSQ = ONE + SSQ* (SCALE/ABSXI)**2
                    SCALE = ABSXI
                ELSE
                    SSQ = SSQ + (ABSXI/SCALE)**2
                END IF
            END IF
        10  CONTINUE
        NORM = SCALE*SQRT(SSQ)
    END IF

    MPAL_DNRM2 = NORM
    RETURN
END

SUBROUTINE MPAL_DAXPY(N,DA,DX,INCX,DY,INCY)

    DOUBLE PRECISION DA
    INTEGER INCX,INCY,N
    
    TYPE(MPAL_ST) DX(*),DY(*)
    
    INTEGER I,IX,IY,M,MP1
    INTRINSIC MOD

        IF (N.LE.0) RETURN
        IF (DA.EQ.0.0d0) RETURN
        IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
            M = MOD(N,4)
            IF (M.NE.0) THEN
                DO I = 1,M
                   DY(I) = DY(I) + DA*DX(I)
                END DO
            END IF
            IF (N.LT.4) RETURN
            MP1 = M + 1
            DO I = MP1,N,4
                DY(I) = DY(I) + DA*DX(I)
                DY(I+1) = DY(I+1) + DA*DX(I+1)
                DY(I+2) = DY(I+2) + DA*DX(I+2)
                !write(*, *) MPAL_VAL(DY(I+3)), MPAL_VAL(DA*DX(I+3))
                DY(I+3) = DY(I+3) + DA*DX(I+3)
            END DO
        ELSE
            IX = 1
            IY = 1
            IF (INCX.LT.0) IX = (-N+1)*INCX + 1
            IF (INCY.LT.0) IY = (-N+1)*INCY + 1
            DO I = 1,N
                DY(IY) = DY(IY) + DA*DX(IX)
                IX = IX + INCX
                IY = IY + INCY
            END DO
        END IF
        RETURN
    END

DOUBLE PRECISION FUNCTION MPAL_DLANSY( NORM, UPLO, N, A, LDA, WORK )

        IMPLICIT NONE
        
        CHARACTER          NORM, UPLO
        INTEGER            LDA, N

        TYPE(MPAL_ST)      A( LDA, * ), WORK( * )

        DOUBLE PRECISION   ONE, ZERO
        PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )

        INTEGER            I, J
        DOUBLE PRECISION   SUM, VALUE

        LOGICAL            LSAME, DISNAN
        EXTERNAL           LSAME, DISNAN

        INTRINSIC          ABS

        IF( N.EQ.0 ) THEN
            VALUE = ZERO
        ELSE IF( LSAME( NORM, 'M' ) ) THEN
            VALUE = ZERO
            IF( LSAME( UPLO, 'U' ) ) THEN
                DO 20 J = 1, N
                    DO 10 I = 1, J
                        SUM = ABS( MPAL_VAL(A( I, J )) )
                        IF( VALUE .LT. SUM .OR. DISNAN( SUM ) ) VALUE = SUM
    10              CONTINUE
    20          CONTINUE
            ELSE
                DO 40 J = 1, N
                    DO 30 I = J, N
                        SUM = ABS( MPAL_VAL(A( I, J )) )
                        IF( VALUE .LT. SUM .OR. DISNAN( SUM ) ) VALUE = SUM
    30              CONTINUE
    40          CONTINUE
            END IF
        ELSE
            WRITE(*, *) "Not implemented!"
        END IF
    
    MPAL_DLANSY = VALUE
    RETURN

    END

    SUBROUTINE MPAL_DLASSQ( N, X, INCX, SCALE, SUMSQ )

              INTEGER            INCX, N
              DOUBLE PRECISION   SCALE, SUMSQ

              TYPE(MPAL_ST)      X( * )

              DOUBLE PRECISION   ZERO
              PARAMETER          ( ZERO = 0.0D+0 )

              INTEGER            IX
              DOUBLE PRECISION   ABSXI

              LOGICAL            DISNAN
              EXTERNAL           DISNAN

              INTRINSIC          ABS

              IF( N.GT.0 ) THEN
                 DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX
                    ABSXI = ABS( MPAL_VAL(X( IX )) )
                    IF( ABSXI.GT.ZERO.OR.DISNAN( ABSXI ) ) THEN
                       IF( SCALE.LT.ABSXI ) THEN
                          SUMSQ = 1 + SUMSQ*( SCALE / ABSXI )**2
                          SCALE = ABSXI
                       ELSE
                          SUMSQ = SUMSQ + ( ABSXI / SCALE )**2
                       END IF
                    END IF
           10    CONTINUE
              END IF
              RETURN

              END        

DOUBLE PRECISION FUNCTION MPAL_DLANST( NORM, N, D, E )

          CHARACTER          NORM
          INTEGER            N

          TYPE(MPAL_ST)   D( * ), E( * )

          DOUBLE PRECISION   ONE, ZERO
          PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )

          INTEGER            I
          DOUBLE PRECISION   ANORM, SCALE, SUM

          LOGICAL            LSAME, DISNAN
          EXTERNAL           LSAME, DISNAN

          EXTERNAL           DLASSQ

          INTRINSIC          ABS, SQRT

          IF( N.LE.0 ) THEN
             ANORM = ZERO
          ELSE IF( LSAME( NORM, 'M' ) ) THEN
             ANORM = ABS( MPAL_VAL(D( N )) )
             DO 10 I = 1, N - 1
                SUM = ABS( MPAL_VAL(D( I )) )
                IF( ANORM .LT. SUM .OR. DISNAN( SUM ) ) ANORM = SUM
                SUM = ABS( MPAL_VAL(E( I )) )
                IF( ANORM .LT. SUM .OR. DISNAN( SUM ) ) ANORM = SUM
       10    CONTINUE
          ELSE IF( LSAME( NORM, 'O' ) .OR. NORM.EQ.'1' .OR. &
                  LSAME( NORM, 'I' ) ) THEN
    ! *
    ! *        Find norm1(A).
    ! *
             IF( N.EQ.1 ) THEN
                ANORM = ABS( MPAL_VAL(D( 1 )) )
             ELSE
                ANORM = ABS( MPAL_VAL(D( 1 )) )+ABS( MPAL_VAL(E( 1 )) )
                SUM = ABS( MPAL_VAL(E( N-1 )) )+ABS( MPAL_VAL(D( N )) )
                IF( ANORM .LT. SUM .OR. DISNAN( SUM ) ) ANORM = SUM
                DO 20 I = 2, N - 1
                   SUM = ABS( MPAL_VAL(D( I )) )+ABS( MPAL_VAL(E( I )) ) + ABS( MPAL_VAL(E( I-1 )) )
                   IF( ANORM .LT. SUM .OR. DISNAN( SUM ) ) ANORM = SUM
       20       CONTINUE
             END IF
          ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
    ! *
    ! *        Find normF(A).
    ! *
             SCALE = ZERO
             SUM = ONE
             IF( N.GT.1 ) THEN
                CALL MPAL_DLASSQ( N-1, E, 1, SCALE, SUM )
                SUM = 2*SUM
             END IF
             CALL MPAL_DLASSQ( N, D, 1, SCALE, SUM )
             ANORM = SCALE*SQRT( SUM )
          END IF

          MPAL_DLANST = ANORM
          RETURN
    
          END

SUBROUTINE MPAL_LASRT( ID, N, D, INFO )

        CHARACTER          ID
        INTEGER            INFO, N

        TYPE(MPAL_ST)      D( * )

        INTEGER            SELECT
        PARAMETER          ( SELECT = 20 )

        INTEGER            DIR, ENDD, I, J, START, STKPNT
        TYPE(MPAL_ST)      D1, D2, D3, DMNMX, TMP

        INTEGER            STACK( 2, 32 )

        LOGICAL            LSAME
        EXTERNAL           LSAME

        EXTERNAL           XERBLA

        INFO = 0
        DIR = -1
        IF( LSAME( ID, 'D' ) ) THEN
            DIR = 0
        ELSE IF( LSAME( ID, 'I' ) ) THEN
            DIR = 1
        END IF
        IF( DIR.EQ.-1 ) THEN
            INFO = -1
        ELSE IF( N.LT.0 ) THEN
            INFO = -2
        END IF
        IF( INFO.NE.0 ) THEN
            CALL XERBLA( 'DLASRT', -INFO )
            RETURN
        END IF

        IF( N.LE.1 ) &
        RETURN
        STKPNT = 1
        STACK( 1, 1 ) = 1
        STACK( 2, 1 ) = N
    10 CONTINUE
        START = STACK( 1, STKPNT )
        ENDD = STACK( 2, STKPNT )
        STKPNT = STKPNT - 1
        IF( ENDD-START.LE.SELECT .AND. ENDD-START.GT.0 ) THEN
            IF( DIR.EQ.0 ) THEN
            DO 30 I = START + 1, ENDD
                DO 20 J = I, START + 1, -1
                    IF( MPAL_VAL(D( J )).GT.MPAL_VAL(D( J-1 )) ) THEN
                        DMNMX = D( J )
                        D( J ) = D( J-1 )
                        D( J-1 ) = DMNMX
                    ELSE
                        GO TO 30
                    END IF
    20          CONTINUE
    30       CONTINUE
            ELSE
            DO 50 I = START + 1, ENDD
                DO 40 J = I, START + 1, -1
                    IF( MPAL_VAL(D( J )).LT.MPAL_VAL(D( J-1 )) ) THEN
                        DMNMX = D( J )
                        D( J ) = D( J-1 )
                        D( J-1 ) = DMNMX
                    ELSE
                        GO TO 50
                    END IF
    40          CONTINUE
    50       CONTINUE

            END IF

        ELSE IF( ENDD-START.GT.SELECT ) THEN

            D1 = D( START )
            D2 = D( ENDD )
            I = ( START+ENDD ) / 2
            D3 = D( I )
            IF( D1.LT.D2 ) THEN
            IF( D3.LT.D1 ) THEN
                DMNMX = D1
            ELSE IF( D3.LT.D2 ) THEN
                DMNMX = D3
            ELSE
                DMNMX = D2
            END IF
            ELSE
            IF( D3.LT.D2 ) THEN
                DMNMX = D2
            ELSE IF( D3.LT.D1 ) THEN
                DMNMX = D3
            ELSE
                DMNMX = D1
            END IF
            END IF

            IF( DIR.EQ.0 ) THEN

            I = START - 1
            J = ENDD + 1
    60       CONTINUE
    70       CONTINUE
            J = J - 1
            IF( D( J ).LT.DMNMX ) &
                GO TO 70
    80       CONTINUE
            I = I + 1
            IF( D( I ).GT.DMNMX ) &
                GO TO 80
            IF( I.LT.J ) THEN
                TMP = D( I )
                D( I ) = D( J )
                D( J ) = TMP
                GO TO 60
            END IF
            IF( J-START.GT.ENDD-J-1 ) THEN
                STKPNT = STKPNT + 1
                STACK( 1, STKPNT ) = START
                STACK( 2, STKPNT ) = J
                STKPNT = STKPNT + 1
                STACK( 1, STKPNT ) = J + 1
                STACK( 2, STKPNT ) = ENDD
            ELSE
                STKPNT = STKPNT + 1
                STACK( 1, STKPNT ) = J + 1
                STACK( 2, STKPNT ) = ENDD
                STKPNT = STKPNT + 1
                STACK( 1, STKPNT ) = START
                STACK( 2, STKPNT ) = J
            END IF
            ELSE
            I = START - 1
            J = ENDD + 1
    90       CONTINUE
    100       CONTINUE
            J = J - 1
            IF( D( J ).GT.DMNMX ) &
                 GO TO 100
    110       CONTINUE
            I = I + 1
            IF( D( I ).LT.DMNMX ) &
                 GO TO 110
            IF( I.LT.J ) THEN
                TMP = D( I )
                D( I ) = D( J )
                D( J ) = TMP
                GO TO 90
            END IF
            IF( J-START.GT.ENDD-J-1 ) THEN
                STKPNT = STKPNT + 1
                STACK( 1, STKPNT ) = START
                STACK( 2, STKPNT ) = J
                STKPNT = STKPNT + 1
                STACK( 1, STKPNT ) = J + 1
                STACK( 2, STKPNT ) = ENDD
            ELSE
                STKPNT = STKPNT + 1
                STACK( 1, STKPNT ) = J + 1
                STACK( 2, STKPNT ) = ENDD
                STKPNT = STKPNT + 1
                STACK( 1, STKPNT ) = START
                STACK( 2, STKPNT ) = J
            END IF
            END IF
        END IF
        IF( STKPNT.GT.0 ) &
            GO TO 10
        RETURN

        END

SUBROUTINE MPAL_DLASET( UPLO, M, N, ALPHA, BETA, A, LDA )

            CHARACTER          UPLO
            INTEGER            LDA, M, N
            DOUBLE PRECISION   ALPHA, BETA

            TYPE(MPAL_ST)      A( LDA, * )
            INTEGER            I, J
    
            LOGICAL            LSAME
            EXTERNAL           LSAME
            INTRINSIC          MIN

            IF( LSAME( UPLO, 'U' ) ) THEN

                DO 20 J = 2, N
                DO 10 I = 1, MIN( J-1, M )
                    A( I, J ) = ALPHA
        10       CONTINUE
        20    CONTINUE

            ELSE IF( LSAME( UPLO, 'L' ) ) THEN

                DO 40 J = 1, MIN( M, N )
                DO 30 I = J + 1, M
                    A( I, J ) = ALPHA
        30       CONTINUE
        40    CONTINUE

            ELSE

                DO 60 J = 1, N
                DO 50 I = 1, M
                    A( I, J ) = ALPHA
        50       CONTINUE
        60    CONTINUE
            END IF

            DO 70 I = 1, MIN( M, N )
                A( I, I ) = BETA
        70 CONTINUE

            RETURN

            END

SUBROUTINE MPAL_SWAP(N,DX,INCX,DY,INCY)

            INTEGER INCX,INCY,N

            TYPE(MPAL_ST) DX(*),DY(*)
            TYPE(MPAL_ST) DTEMP
            INTEGER I,IX,IY,M,MP1

            INTRINSIC MOD

            IF (N.LE.0) RETURN
            IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
                M = MOD(N,3)
                IF (M.NE.0) THEN
                DO I = 1,M
                    DTEMP = DX(I)
                    DX(I) = DY(I)
                    DY(I) = DTEMP
                END DO
                IF (N.LT.3) RETURN
                END IF
                MP1 = M + 1
                DO I = MP1,N,3
                DTEMP = DX(I)
                DX(I) = DY(I)
                DY(I) = DTEMP
                DTEMP = DX(I+1)
                DX(I+1) = DY(I+1)
                DY(I+1) = DTEMP
                DTEMP = DX(I+2)
                DX(I+2) = DY(I+2)
                DY(I+2) = DTEMP
                END DO
            ELSE
                IX = 1
                IY = 1
                IF (INCX.LT.0) IX = (-N+1)*INCX + 1
                IF (INCY.LT.0) IY = (-N+1)*INCY + 1
                DO I = 1,N
                DTEMP = DX(IX)
                DX(IX) = DY(IY)
                DY(IY) = DTEMP
                IX = IX + INCX
                IY = IY + INCY
                END DO
            END IF
            RETURN
            END

SUBROUTINE MPAL_DCOPY(N,DX,INCX,DY,INCY)

    INTEGER INCX,INCY,N

    TYPE(MPAL_ST) DX(*),DY(*)

    INTEGER I,IX,IY,M,MP1

    INTRINSIC MOD

    IF (N.LE.0) RETURN
    IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN

        M = MOD(N,7)
        IF (M.NE.0) THEN
            DO I = 1,M
                DY(I) = DX(I)
            END DO
            IF (N.LT.7) RETURN
        END IF
        MP1 = M + 1
        DO I = MP1,N,7
            DY(I) = DX(I)
            DY(I+1) = DX(I+1)
            DY(I+2) = DX(I+2)
            DY(I+3) = DX(I+3)
            DY(I+4) = DX(I+4)
            DY(I+5) = DX(I+5)
            DY(I+6) = DX(I+6)
        END DO
    ELSE
        IX = 1
        IY = 1
        IF (INCX.LT.0) IX = (-N+1)*INCX + 1
        IF (INCY.LT.0) IY = (-N+1)*INCY + 1
        DO I = 1,N
            DY(IY) = DX(IX)
            IX = IX + INCX
            IY = IY + INCY
        END DO
    END IF
    RETURN
    END

SUBROUTINE MPAL_DLAMRG( N1, N2, A, DTRD1, DTRD2, INDEX )

            INTEGER            DTRD1, DTRD2, N1, N2

            INTEGER            INDEX( * )
            TYPE(MPAL_ST)      A( * )

            INTEGER            I, IND1, IND2, N1SV, N2SV

            N1SV = N1
            N2SV = N2
            IF( DTRD1.GT.0 ) THEN
                IND1 = 1
            ELSE
                IND1 = N1
            END IF
            IF( DTRD2.GT.0 ) THEN
                IND2 = 1 + N1
            ELSE
                IND2 = N1 + N2
            END IF
            I = 1
        10 CONTINUE
            IF( N1SV.GT.0 .AND. N2SV.GT.0 ) THEN
                IF( A( IND1 ).LE.A( IND2 ) ) THEN
                INDEX( I ) = IND1
                I = I + 1
                IND1 = IND1 + DTRD1
                N1SV = N1SV - 1
                ELSE
                INDEX( I ) = IND2
                I = I + 1
                IND2 = IND2 + DTRD2
                N2SV = N2SV - 1
                END IF
                GO TO 10
            END IF
            IF( N1SV.EQ.0 ) THEN
                DO 20 N1SV = 1, N2SV
                INDEX( I ) = IND2
                I = I + 1
                IND2 = IND2 + DTRD2
        20    CONTINUE
            ELSE
                DO 30 N2SV = 1, N1SV
                INDEX( I ) = IND1
                I = I + 1
                IND1 = IND1 + DTRD1
        30    CONTINUE
            END IF
            RETURN
    END

SUBROUTINE MPAL_DLACPY( UPLO, M, N, A, LDA, B, LDB )
            CHARACTER          UPLO
            INTEGER            LDA, LDB, M, N

            TYPE(MPAL_ST)      A( LDA, * ), B( LDB, * )

            INTEGER            I, J

            LOGICAL            LSAME
            EXTERNAL           LSAME

            INTRINSIC          MIN

            IF( LSAME( UPLO, 'U' ) ) THEN
                DO 20 J = 1, N
                DO 10 I = 1, MIN( J, M )
                    B( I, J ) = A( I, J )
        10       CONTINUE
        20    CONTINUE
            ELSE IF( LSAME( UPLO, 'L' ) ) THEN
                DO 40 J = 1, N
                DO 30 I = J, M
                    B( I, J ) = A( I, J )
        30       CONTINUE
        40    CONTINUE
            ELSE
                DO 60 J = 1, N
                DO 50 I = 1, M
                    B( I, J ) = A( I, J )
        50       CONTINUE
        60    CONTINUE
            END IF
            RETURN

            END

SUBROUTINE MPAL_DROT(N,DX,INCX,DY,INCY,C,S)

    DOUBLE PRECISION C,S
    INTEGER INCX,INCY,N

    TYPE(MPAL_ST) DX(*),DY(*)

    TYPE(MPAL_ST) DTEMP
    INTEGER I,IX,IY

    IF (N.LE.0) RETURN
    IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
        DO I = 1,N
            DTEMP = C*DX(I) + S*DY(I)
            DY(I) = C*DY(I) - S*DX(I)
            DX(I) = DTEMP
        END DO
    ELSE
        IX = 1
        IY = 1
        IF (INCX.LT.0) IX = (-N+1)*INCX + 1
        IF (INCY.LT.0) IY = (-N+1)*INCY + 1
        DO I = 1,N
            DTEMP = C*DX(IX) + S*DY(IY)
            DY(IY) = C*DY(IY) - S*DX(IX)
            DX(IX) = DTEMP
            IX = IX + INCX
            IY = IY + INCY
        END DO
    END IF
    RETURN
    END
       