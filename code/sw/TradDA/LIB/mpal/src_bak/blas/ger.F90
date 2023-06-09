SUBROUTINE MPAL_GER(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)

          DOUBLE PRECISION ALPHA
          INTEGER INCX,INCY,LDA,M,N

          TYPE(MPAL_ST) A(LDA,*),X(*),Y(*)

          DOUBLE PRECISION ZERO
          PARAMETER (ZERO=0.0D+0)

          TYPE(MPAL_ST) TEMP
          INTEGER I,INFO,IX,J,JY,KX

          EXTERNAL XERBLA

          INTRINSIC MAX

          INFO = 0
          IF (M.LT.0) THEN
              INFO = 1
          ELSE IF (N.LT.0) THEN
              INFO = 2
          ELSE IF (INCX.EQ.0) THEN
              INFO = 5
          ELSE IF (INCY.EQ.0) THEN
              INFO = 7
          ELSE IF (LDA.LT.MAX(1,M)) THEN
              INFO = 9
          END IF
          IF (INFO.NE.0) THEN
              CALL XERBLA('DGER  ',INFO)
              RETURN
          END IF
          IF ((M.EQ.0) .OR. (N.EQ.0) .OR. (ALPHA.EQ.ZERO)) RETURN
          IF (INCY.GT.0) THEN
              JY = 1
          ELSE
              JY = 1 - (N-1)*INCY
          END IF
          IF (INCX.EQ.1) THEN
              DO 20 J = 1,N
                  IF (Y(JY).NE.ZERO) THEN
                      TEMP = ALPHA*Y(JY)
                      DO 10 I = 1,M
                          A(I,J) = A(I,J) + X(I)*TEMP
       10             CONTINUE
                  END IF
                  JY = JY + INCY
       20     CONTINUE
          ELSE
              IF (INCX.GT.0) THEN
                  KX = 1
              ELSE
                  KX = 1 - (M-1)*INCX
              END IF
              DO 40 J = 1,N
                  IF (Y(JY).NE.ZERO) THEN
                      TEMP = ALPHA*Y(JY)
                      IX = KX
                      DO 30 I = 1,M
                          A(I,J) = A(I,J) + X(IX)*TEMP
                          IX = IX + INCX
       30             CONTINUE
                  END IF
                  JY = JY + INCY
       40     CONTINUE
          END IF

          RETURN

          END