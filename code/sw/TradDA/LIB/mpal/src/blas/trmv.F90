SUBROUTINE MPAL_TRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)

          INTEGER INCX,LDA,N
          CHARACTER DIAG,TRANS,UPLO

          TYPE(MPAL_ST) A(LDA,*),X(*)

          TYPE(MPAL_ST) ZERO

          TYPE(MPAL_ST) TEMP
          INTEGER I,INFO,IX,J,JX,KX
          LOGICAL NOUNIT

          LOGICAL LSAME
          EXTERNAL LSAME

          EXTERNAL XERBLA

          INTRINSIC MAX

          ZERO=0.0D+0

          INFO = 0
          IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
              INFO = 1
          ELSE IF (.NOT.LSAME(TRANS,'N') .AND. .NOT.LSAME(TRANS,'T') .AND. &
                   .NOT.LSAME(TRANS,'C')) THEN
              INFO = 2
          ELSE IF (.NOT.LSAME(DIAG,'U') .AND. .NOT.LSAME(DIAG,'N')) THEN
              INFO = 3
          ELSE IF (N.LT.0) THEN
              INFO = 4
          ELSE IF (LDA.LT.MAX(1,N)) THEN
              INFO = 6
          ELSE IF (INCX.EQ.0) THEN
              INFO = 8
          END IF
          IF (INFO.NE.0) THEN
              CALL XERBLA('DTRMV ',INFO)
              RETURN
          END IF
          IF (N.EQ.0) RETURN

          NOUNIT = LSAME(DIAG,'N')

          IF (INCX.LE.0) THEN
              KX = 1 - (N-1)*INCX
          ELSE IF (INCX.NE.1) THEN
              KX = 1
          END IF
          IF (LSAME(TRANS,'N')) THEN
              IF (LSAME(UPLO,'U')) THEN
                  IF (INCX.EQ.1) THEN
                      DO 20 J = 1,N
                          IF (X(J).NE.ZERO) THEN
                              TEMP = X(J)
                              DO 10 I = 1,J - 1
                                  X(I) = X(I) + TEMP*A(I,J)
       10                     CONTINUE
                              IF (NOUNIT) X(J) = X(J)*A(J,J)
                          END IF
       20             CONTINUE
                  ELSE
                      JX = KX
                      DO 40 J = 1,N
                          IF (X(JX).NE.ZERO) THEN
                              TEMP = X(JX)
                              IX = KX
                              DO 30 I = 1,J - 1
                                  X(IX) = X(IX) + TEMP*A(I,J)
                                  IX = IX + INCX
       30                     CONTINUE
                              IF (NOUNIT) X(JX) = X(JX)*A(J,J)
                          END IF
                          JX = JX + INCX
       40             CONTINUE
                  END IF
              ELSE
                  IF (INCX.EQ.1) THEN
                      DO 60 J = N,1,-1
                          IF (X(J).NE.ZERO) THEN
                              TEMP = X(J)
                              DO 50 I = N,J + 1,-1
                                  X(I) = X(I) + TEMP*A(I,J)
       50                     CONTINUE
                              IF (NOUNIT) X(J) = X(J)*A(J,J)
                          END IF
       60             CONTINUE
                  ELSE
                      KX = KX + (N-1)*INCX
                      JX = KX
                      DO 80 J = N,1,-1
                          IF (X(JX).NE.ZERO) THEN
                              TEMP = X(JX)
                              IX = KX
                              DO 70 I = N,J + 1,-1
                                  X(IX) = X(IX) + TEMP*A(I,J)
                                  IX = IX - INCX
       70                     CONTINUE
                              IF (NOUNIT) X(JX) = X(JX)*A(J,J)
                          END IF
                          JX = JX - INCX
       80             CONTINUE
                  END IF
              END IF
          ELSE
              IF (LSAME(UPLO,'U')) THEN
                  IF (INCX.EQ.1) THEN
                      DO 100 J = N,1,-1
                          TEMP = X(J)
                          IF (NOUNIT) TEMP = TEMP*A(J,J)
                          DO 90 I = J - 1,1,-1
                              TEMP = TEMP + A(I,J)*X(I)
       90                 CONTINUE
                          X(J) = TEMP
      100             CONTINUE
                  ELSE
                      JX = KX + (N-1)*INCX
                      DO 120 J = N,1,-1
                          TEMP = X(JX)
                          IX = JX
                          IF (NOUNIT) TEMP = TEMP*A(J,J)
                          DO 110 I = J - 1,1,-1
                              IX = IX - INCX
                              TEMP = TEMP + A(I,J)*X(IX)
      110                 CONTINUE
                          X(JX) = TEMP
                          JX = JX - INCX
      120             CONTINUE
                  END IF
              ELSE
                  IF (INCX.EQ.1) THEN
                      DO 140 J = 1,N
                          TEMP = X(J)
                          IF (NOUNIT) TEMP = TEMP*A(J,J)
                          DO 130 I = J + 1,N
                              TEMP = TEMP + A(I,J)*X(I)
      130                 CONTINUE
                          X(J) = TEMP
      140             CONTINUE
                  ELSE
                      JX = KX
                      DO 160 J = 1,N
                          TEMP = X(JX)
                          IX = JX
                          IF (NOUNIT) TEMP = TEMP*A(J,J)
                          DO 150 I = J + 1,N
                              IX = IX + INCX
                              TEMP = TEMP + A(I,J)*X(IX)
      150                 CONTINUE
                          X(JX) = TEMP
                          JX = JX + INCX
      160             CONTINUE
                  END IF
              END IF
          END IF

          RETURN

          END