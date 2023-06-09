SUBROUTINE MPAL_GEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
    TYPE(MPAL_ST) ALPHA,BETA
    INTEGER INCX,INCY,LDA,M,N
    CHARACTER TRANS

    TYPE(MPAL_ST) A(LDA,*),X(*),Y(*)
    TYPE(MPAL_ST) ONE,ZERO
    TYPE(MPAL_ST) TEMP
    INTEGER I,INFO,IX,IY,J,JX,JY,KX,KY,LENX,LENY
    LOGICAL LSAME
    EXTERNAL LSAME
    EXTERNAL XERBLA
    INTRINSIC MAX

    ONE=1.0D+0
    ZERO=0.0D+0
    
    INFO = 0
    IF (.NOT.LSAME(TRANS,'N') .AND. .NOT.LSAME(TRANS,'T') .AND. &
        .NOT.LSAME(TRANS,'C')) THEN
            INFO = 1
        ELSE IF (M.LT.0) THEN
            INFO = 2
        ELSE IF (N.LT.0) THEN
            INFO = 3
        ELSE IF (LDA.LT.MAX(1,M)) THEN
            INFO = 6
        ELSE IF (INCX.EQ.0) THEN
            INFO = 8
        ELSE IF (INCY.EQ.0) THEN
            INFO = 11
    END IF
        IF (INFO.NE.0) THEN
            CALL XERBLA('DGEMV ',INFO)
            RETURN
        END IF
    IF ((M.EQ.0) .OR. (N.EQ.0) .OR. &
        ((ALPHA.EQ.ZERO).AND. (BETA.EQ.ONE))) RETURN
        IF (LSAME(TRANS,'N')) THEN
            LENX = N
            LENY = M
        ELSE
            LENX = M
            LENY = N
        END IF
        IF (INCX.GT.0) THEN
            KX = 1
        ELSE
            KX = 1 - (LENX-1)*INCX
        END IF
        IF (INCY.GT.0) THEN
            KY = 1
        ELSE
            KY = 1 - (LENY-1)*INCY
        END IF
          
        IF (BETA.NE.ONE) THEN
            IF (INCY.EQ.1) THEN
                IF (BETA.EQ.ZERO) THEN
                    DO 10 I = 1,LENY
                        Y(I) = ZERO
    10             CONTINUE
                ELSE
                    DO 20 I = 1,LENY
                        Y(I) = BETA*Y(I)
    20             CONTINUE
                END IF
            ELSE
                IY = KY
                IF (BETA.EQ.ZERO) THEN
                    DO 30 I = 1,LENY
                        Y(IY) = ZERO
                        IY = IY + INCY
    30             CONTINUE
                ELSE
                    DO 40 I = 1,LENY
                        Y(IY) = BETA*Y(IY)
                        IY = IY + INCY
    40             CONTINUE
                END IF
            END IF
        END IF
            IF (ALPHA.EQ.ZERO) RETURN
            IF (LSAME(TRANS,'N')) THEN
                JX = KX
                IF (INCY.EQ.1) THEN
                    DO 60 J = 1,N
                        TEMP = ALPHA*X(JX)
                        DO 50 I = 1,M
                            Y(I) = Y(I) + TEMP*A(I,J)
    50                  CONTINUE
                    JX = JX + INCX
    60          CONTINUE
            ELSE
                DO 80 J = 1,N
                    TEMP = ALPHA*X(JX)
                    IY = KY
                    DO 70 I = 1,M
                        Y(IY) = Y(IY) + TEMP*A(I,J)
                        IY = IY + INCY
    70             CONTINUE
                    JX = JX + INCX
    80         CONTINUE
            END IF
        ELSE
            JY = KY
            IF (INCX.EQ.1) THEN
                DO 100 J = 1,N
                    TEMP = ZERO
                    DO 90 I = 1,M
                        TEMP = TEMP + A(I,J)*X(I)
    90             CONTINUE
                    Y(JY) = Y(JY) + ALPHA*TEMP
                    JY = JY + INCY
    100         CONTINUE
            ELSE
                DO 120 J = 1,N
                    TEMP = ZERO
                    IX = KX
                    DO 110 I = 1,M
                        TEMP = TEMP + A(I,J)*X(IX)
                        IX = IX + INCX
    110             CONTINUE
                    Y(JY) = Y(JY) + ALPHA*TEMP
                    JY = JY + INCY
    120         CONTINUE
            END IF
        END IF

        RETURN

    END
    