DOUBLE PRECISION FUNCTION MPAL_DOT(N,DX,INCX,DY,INCY)

    INTEGER INCX,INCY,N

    TYPE(MPAL_ST) DX(*),DY(*)

    TYPE(MPAL_ST) DTEMP
    INTEGER I,IX,IY,M,MP1

    INTRINSIC MOD
    
    MPAL_DOT = 0.0d0
    DTEMP = 0.0d0
    IF (N.LE.0) RETURN
    IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
        M = MOD(N,5)
        IF (M.NE.0) THEN
            DO I = 1,M
               DTEMP = DTEMP + DX(I)*DY(I)
            END DO
            IF (N.LT.5) THEN
                MPAL_DOT=MPAL_VAL(DTEMP)
            RETURN
            END IF
        END IF
        MP1 = M + 1
        DO I = MP1,N,5
            DTEMP = DTEMP + DX(I)*DY(I) + DX(I+1)*DY(I+1) + &
                DX(I+2)*DY(I+2) + DX(I+3)*DY(I+3) + DX(I+4)*DY(I+4)
        END DO
    ELSE
        IX = 1
        IY = 1
        IF (INCX.LT.0) IX = (-N+1)*INCX + 1
        IF (INCY.LT.0) IY = (-N+1)*INCY + 1
        DO I = 1,N
            DTEMP = DTEMP + DX(IX)*DY(IY)
            IX = IX + INCX
            IY = IY + INCY
        END DO
    END IF
    MPAL_DOT = MPAL_VAL(DTEMP)
    RETURN
    END
