SUBROUTINE MPAL_DSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)

        TYPE(MPAL_ST) ALPHA,BETA
        INTEGER K,LDA,LDB,LDC,N
        CHARACTER TRANS,UPLO
    
        TYPE(MPAL_ST) A(LDA,*),B(LDB,*),C(LDC,*)
        
        LOGICAL LSAME
        EXTERNAL LSAME

        EXTERNAL XERBLA

        INTRINSIC MAX
    
        TYPE(MPAL_ST) TEMP1,TEMP2
        INTEGER I,INFO,J,L,NROWA
        LOGICAL UPPER
    
        TYPE(MPAL_ST) ONE,ZERO
        
        ONE=1.0D+0
        ZERO=0.0D+0
        
        IF (LSAME(TRANS,'N')) THEN
            NROWA = N
        ELSE
            NROWA = K
        END IF
        UPPER = LSAME(UPLO,'U')

        INFO = 0
        IF ((.NOT.UPPER) .AND. (.NOT.LSAME(UPLO,'L'))) THEN
            INFO = 1
        ELSE IF ((.NOT.LSAME(TRANS,'N')) .AND. &
            (.NOT.LSAME(TRANS,'T')) .AND. &
            (.NOT.LSAME(TRANS,'C'))) THEN
            INFO = 2
        ELSE IF (N.LT.0) THEN
            INFO = 3
        ELSE IF (K.LT.0) THEN
            INFO = 4
        ELSE IF (LDA.LT.MAX(1,NROWA)) THEN
            INFO = 7
        ELSE IF (LDB.LT.MAX(1,NROWA)) THEN
            INFO = 9
        ELSE IF (LDC.LT.MAX(1,N)) THEN
            INFO = 12
        END IF
        IF (INFO.NE.0) THEN
            CALL XERBLA('DSYR2K',INFO)
            RETURN
        END IF

        IF ((N.EQ.0) .OR. (((ALPHA.EQ.ZERO).OR. &
            (K.EQ.0)).AND. (BETA.EQ.ONE))) RETURN
        
        IF (ALPHA.EQ.ZERO) THEN
            IF (UPPER) THEN
                IF (BETA.EQ.ZERO) THEN
                    DO 20 J = 1,N
                        DO 10 I = 1,J
                            C(I,J) = ZERO
    10                 CONTINUE
    20             CONTINUE
                ELSE
                    DO 40 J = 1,N
                        DO 30 I = 1,J
                            C(I,J) = BETA*C(I,J)
    30                 CONTINUE
    40             CONTINUE
                END IF
            ELSE
                IF (BETA.EQ.ZERO) THEN
                    DO 60 J = 1,N
                        DO 50 I = J,N
                            C(I,J) = ZERO
    50                 CONTINUE
    60             CONTINUE
                ELSE
                    DO 80 J = 1,N
                        DO 70 I = J,N
                            C(I,J) = BETA*C(I,J)
    70                 CONTINUE
    80             CONTINUE
                END IF
            END IF
            RETURN
        END IF
        
        IF (LSAME(TRANS,'N')) THEN
            IF (UPPER) THEN
                DO 130 J = 1,N
                    IF (BETA.EQ.ZERO) THEN
                        DO 90 I = 1,J
                            C(I,J) = ZERO
    90                 CONTINUE
                    ELSE IF (BETA.NE.ONE) THEN
                        DO 100 I = 1,J
                            C(I,J) = BETA*C(I,J)
    100                 CONTINUE
                    END IF
                    DO 120 L = 1,K
                        IF ((A(J,L).NE.ZERO) .OR. (B(J,L).NE.ZERO)) THEN
                            TEMP1 = ALPHA*B(J,L)
                            TEMP2 = ALPHA*A(J,L)
                            DO 110 I = 1,J
                                C(I,J) = C(I,J) + A(I,L)*TEMP1 + &
                                        B(I,L)*TEMP2
    110                     CONTINUE
                        END IF
    120             CONTINUE
    130         CONTINUE
            ELSE
                DO 180 J = 1,N
                    IF (BETA.EQ.ZERO) THEN
                        DO 140 I = J,N
                            C(I,J) = ZERO
    140                 CONTINUE
                    ELSE IF (BETA.NE.ONE) THEN
                        DO 150 I = J,N
                            C(I,J) = BETA*C(I,J)
    150                 CONTINUE
                    END IF
                    DO 170 L = 1,K
                        IF ((A(J,L).NE.ZERO) .OR. (B(J,L).NE.ZERO)) THEN
                            TEMP1 = ALPHA*B(J,L)
                            TEMP2 = ALPHA*A(J,L)
                            DO 160 I = J,N
                                C(I,J) = C(I,J) + A(I,L)*TEMP1 + &
                                        B(I,L)*TEMP2
    160                     CONTINUE
                        END IF
    170             CONTINUE
    180         CONTINUE
            END IF
        ELSE
            IF (UPPER) THEN
                DO 210 J = 1,N
                    DO 200 I = 1,J
                        TEMP1 = ZERO
                        TEMP2 = ZERO
                        DO 190 L = 1,K
                            TEMP1 = TEMP1 + A(L,I)*B(L,J)
                            TEMP2 = TEMP2 + B(L,I)*A(L,J)
    190                 CONTINUE
                        IF (BETA.EQ.ZERO) THEN
                            C(I,J) = ALPHA*TEMP1 + ALPHA*TEMP2
                        ELSE
                            C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 + &
                                        ALPHA*TEMP2
                        END IF
    200             CONTINUE
    210         CONTINUE
            ELSE
                DO 240 J = 1,N
                    DO 230 I = J,N
                        TEMP1 = ZERO
                        TEMP2 = ZERO
                        DO 220 L = 1,K
                            TEMP1 = TEMP1 + A(L,I)*B(L,J)
                            TEMP2 = TEMP2 + B(L,I)*A(L,J)
    220                 CONTINUE
                        IF (BETA.EQ.ZERO) THEN
                            C(I,J) = ALPHA*TEMP1 + ALPHA*TEMP2
                        ELSE
                            C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 + &
                                    ALPHA*TEMP2
                        END IF
    230             CONTINUE
    240         CONTINUE
            END IF
        END IF
        RETURN
    
        END

SUBROUTINE MPAL_DSYR2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)

        TYPE(MPAL_ST) ALPHA
        INTEGER INCX,INCY,LDA,N
        CHARACTER UPLO
            
        TYPE(MPAL_ST) A(LDA,*),X(*),Y(*)
    
        TYPE(MPAL_ST) ZERO
    
        TYPE(MPAL_ST) TEMP1,TEMP2
        INTEGER I,INFO,IX,IY,J,JX,JY,KX,KY
    
        LOGICAL LSAME
        EXTERNAL LSAME
    
        EXTERNAL XERBLA
    
        INTRINSIC MAX

        ZERO=0.0D+0
    
        INFO = 0
        IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
            INFO = 1
        ELSE IF (N.LT.0) THEN
            INFO = 2
        ELSE IF (INCX.EQ.0) THEN
            INFO = 5
        ELSE IF (INCY.EQ.0) THEN
            INFO = 7
        ELSE IF (LDA.LT.MAX(1,N)) THEN
            INFO = 9
        END IF
        IF (INFO.NE.0) THEN
            CALL XERBLA('DSYR2 ',INFO)
            RETURN
        END IF

        IF ((N.EQ.0) .OR. (ALPHA.EQ.ZERO)) RETURN
            IF ((INCX.NE.1) .OR. (INCY.NE.1)) THEN
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
                JX = KX
                JY = KY
            END IF

            IF (LSAME(UPLO,'U')) THEN
                IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
                    DO 20 J = 1,N
                        IF ((X(J).NE.ZERO) .OR. (Y(J).NE.ZERO)) THEN
                            TEMP1 = ALPHA*Y(J)
                            TEMP2 = ALPHA*X(J)
                            DO 10 I = 1,J
                                A(I,J) = A(I,J) + X(I)*TEMP1 + Y(I)*TEMP2
        10                 CONTINUE
                        END IF
        20         CONTINUE
                ELSE
                    DO 40 J = 1,N
                        IF ((X(JX).NE.ZERO) .OR. (Y(JY).NE.ZERO)) THEN
                            TEMP1 = ALPHA*Y(JY)
                            TEMP2 = ALPHA*X(JX)
                            IX = KX
                            IY = KY
                            DO 30 I = 1,J
                                A(I,J) = A(I,J) + X(IX)*TEMP1 + Y(IY)*TEMP2
                                IX = IX + INCX
                                IY = IY + INCY
        30                 CONTINUE
                        END IF
                        JX = JX + INCX
                        JY = JY + INCY
        40         CONTINUE
                END IF
            ELSE
                IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
                    DO 60 J = 1,N
                        IF ((X(J).NE.ZERO) .OR. (Y(J).NE.ZERO)) THEN
                            TEMP1 = ALPHA*Y(J)
                            TEMP2 = ALPHA*X(J)
                            DO 50 I = J,N
                                A(I,J) = A(I,J) + X(I)*TEMP1 + Y(I)*TEMP2
        50                 CONTINUE
                        END IF
        60         CONTINUE
                ELSE
                    DO 80 J = 1,N
                        IF ((X(JX).NE.ZERO) .OR. (Y(JY).NE.ZERO)) THEN
                            TEMP1 = ALPHA*Y(JY)
                            TEMP2 = ALPHA*X(JX)
                            IX = JX
                            IY = JY
                            DO 70 I = J,N
                                A(I,J) = A(I,J) + X(IX)*TEMP1 + Y(IY)*TEMP2
                                IX = IX + INCX
                                IY = IY + INCY
        70                 CONTINUE
                        END IF
                        JX = JX + INCX
                        JY = JY + INCY
        80         CONTINUE
                END IF
            END IF

            RETURN

            END
    