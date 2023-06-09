SUBROUTINE MPAL_SYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)

        TYPE(MPAL_ST) ALPHA,BETA
        INTEGER INCX,INCY,LDA,N
        CHARACTER UPLO

        TYPE(MPAL_ST) A(LDA,*),X(*),Y(*)
        TYPE(MPAL_ST) ONE,ZERO

        TYPE(MPAL_ST) TEMP1,TEMP2
        INTEGER I,INFO,IX,IY,J,JX,JY,KX,KY

        LOGICAL LSAME
        EXTERNAL LSAME

        EXTERNAL XERBLA
        INTRINSIC MAX

        ONE=1.0D+0
        ZERO=0.0D+0

        INFO = 0
        IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
            INFO = 1
        ELSE IF (N.LT.0) THEN
            INFO = 2
        ELSE IF (LDA.LT.MAX(1,N)) THEN
            INFO = 5
        ELSE IF (INCX.EQ.0) THEN
            INFO = 7
        ELSE IF (INCY.EQ.0) THEN
            INFO = 10
        END IF
        IF (INFO.NE.0) THEN
            CALL XERBLA('DSYMV ',INFO)
            RETURN
        END IF
        IF ((N.EQ.0) .OR. ((ALPHA.EQ.ZERO).AND. (BETA.EQ.ONE))) RETURN
        
        IF (INCX.GT.0) THEN
            KX = 1
        ELSE
            KX = 1 - (N-1)*INCX
        END IF
        IF (INCY.GT.0) THEN
            KY = 1
        ELSE
            KY = 1 - (N-1)*INCY
        END IF

        IF (BETA.NE.ONE) THEN
            IF (INCY.EQ.1) THEN
                IF (BETA.EQ.ZERO) THEN
                    DO 10 I = 1,N
                        Y(I) = ZERO
    10             CONTINUE
                ELSE
                    DO 20 I = 1,N
                        Y(I) = BETA*Y(I)
    20             CONTINUE
                END IF
            ELSE
                IY = KY
                IF (BETA.EQ.ZERO) THEN
                    DO 30 I = 1,N
                        Y(IY) = ZERO
                        IY = IY + INCY
    30             CONTINUE
                ELSE
                    DO 40 I = 1,N
                        Y(IY) = BETA*Y(IY)
                        IY = IY + INCY
    40             CONTINUE
                END IF
            END IF
        END IF
        IF (ALPHA.EQ.ZERO) RETURN
        IF (LSAME(UPLO,'U')) THEN

            IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
                DO 60 J = 1,N
                    TEMP1 = ALPHA*X(J)
                    TEMP2 = ZERO
                    DO 50 I = 1,J - 1
                        Y(I) = Y(I) + TEMP1*A(I,J)
                        TEMP2 = TEMP2 + A(I,J)*X(I)
    50             CONTINUE
                    Y(J) = Y(J) + TEMP1*A(J,J) + ALPHA*TEMP2
    60         CONTINUE
            ELSE
                JX = KX
                JY = KY
                DO 80 J = 1,N
                    TEMP1 = ALPHA*X(JX)
                    TEMP2 = ZERO
                    IX = KX
                    IY = KY
                    DO 70 I = 1,J - 1
                        Y(IY) = Y(IY) + TEMP1*A(I,J)
                        TEMP2 = TEMP2 + A(I,J)*X(IX)
                        IX = IX + INCX
                        IY = IY + INCY
    70             CONTINUE
                    Y(JY) = Y(JY) + TEMP1*A(J,J) + ALPHA*TEMP2
                    JX = JX + INCX
                    JY = JY + INCY
    80         CONTINUE
            END IF
        ELSE
            IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
                DO 100 J = 1,N
                    TEMP1 = ALPHA*X(J)
                    TEMP2 = ZERO
                    Y(J) = Y(J) + TEMP1*A(J,J)
                    DO 90 I = J + 1,N
                        Y(I) = Y(I) + TEMP1*A(I,J)
                        TEMP2 = TEMP2 + A(I,J)*X(I)
    90             CONTINUE
                    Y(J) = Y(J) + ALPHA*TEMP2
    100         CONTINUE
            ELSE
                JX = KX
                JY = KY
                DO 120 J = 1,N
                    TEMP1 = ALPHA*X(JX)
                    TEMP2 = ZERO
                    Y(JY) = Y(JY) + TEMP1*A(J,J)
                    IX = JX
                    IY = JY
                    DO 110 I = J + 1,N
                        IX = IX + INCX
                        IY = IY + INCY
                        Y(IY) = Y(IY) + TEMP1*A(I,J)
                        TEMP2 = TEMP2 + A(I,J)*X(IX)
    110             CONTINUE
                    Y(JY) = Y(JY) + ALPHA*TEMP2
                    JX = JX + INCX
                    JY = JY + INCY
    120         CONTINUE
            END IF
        END IF

        RETURN
        
        END
    