
SUBROUTINE MPAL_DORMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC, &
                   WORK, LWORK, INFO )
     CHARACTER          SIDE, TRANS, UPLO
     INTEGER            INFO, LDA, LDC, LWORK, M, N

     TYPE(MPAL_ST)      A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )

     LOGICAL            LEFT, LQUERY, UPPER
     INTEGER            I1, I2, IINFO, LWKOPT, MI, NB, NI, NQ, NW

     LOGICAL            LSAME
     INTEGER            ILAENV
     EXTERNAL           LSAME, ILAENV

     EXTERNAL           XERBLA

     INTRINSIC          MAX
! *     ..
! *     .. Executable Statements ..
! *
! *     Test the input arguments
! *
     INFO = 0
     LEFT = LSAME( SIDE, 'L' )
     UPPER = LSAME( UPLO, 'U' )
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
     ELSE IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
        INFO = -2
     ELSE IF( .NOT.LSAME( TRANS, 'N' ) .AND. .NOT.LSAME( TRANS, 'T' ) ) &
               THEN
        INFO = -3
     ELSE IF( M.LT.0 ) THEN
        INFO = -4
     ELSE IF( N.LT.0 ) THEN
        INFO = -5
     ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
        INFO = -7
     ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
        INFO = -10
     ELSE IF( LWORK.LT.MAX( 1, NW ) .AND. .NOT.LQUERY ) THEN
        INFO = -12
     END IF

     IF( INFO.EQ.0 ) THEN
        IF( UPPER ) THEN
           IF( LEFT ) THEN
              NB = ILAENV( 1, 'DORMQL', SIDE // TRANS, M-1, N, M-1, &
                   -1 )
           ELSE
              NB = ILAENV( 1, 'DORMQL', SIDE // TRANS, M, N-1, N-1, &
                   -1 )
           END IF
        ELSE
           IF( LEFT ) THEN
              NB = ILAENV( 1, 'DORMQR', SIDE // TRANS, M-1, N, M-1, &
                   -1 )
           ELSE
              NB = ILAENV( 1, 'DORMQR', SIDE // TRANS, M, N-1, N-1, &
                   -1 )
           END IF
        END IF
        LWKOPT = MAX( 1, NW )*NB
        WORK( 1 ) = LWKOPT
     END IF

     IF( INFO.NE.0 ) THEN
        CALL XERBLA( 'DORMTR', -INFO )
        RETURN
     ELSE IF( LQUERY ) THEN
        RETURN
     END IF
! *
! *     Quick return if possible
! *
     IF( M.EQ.0 .OR. N.EQ.0 .OR. NQ.EQ.1 ) THEN
        WORK( 1 ) = 1
        RETURN
     END IF

     IF( LEFT ) THEN
        MI = M - 1
        NI = N
     ELSE
        MI = M
        NI = N - 1
     END IF

     IF( UPPER ) THEN
! *
! *        Q was determined by a call to DSYTRD with UPLO = 'U'
! *
        ! CALL MPAL_DORMQL( SIDE, TRANS, MI, NI, NQ-1, A( 1, 2 ), LDA, TAU, C, &
        !              LDC, WORK, LWORK, IINFO )
        
        WRITE(*, *) "Not implemented!"

     ELSE

        IF( LEFT ) THEN
           I1 = 2
           I2 = 1
        ELSE
           I1 = 1
           I2 = 2
        END IF
        CALL MPAL_DORMQR( SIDE, TRANS, MI, NI, NQ-1, A( 2, 1 ), LDA, TAU, &
                     C( I1, I2 ), LDC, WORK, LWORK, IINFO )
     END IF
     WORK( 1 ) = LWKOPT
     RETURN
    !  *
    !  *     End of DORMTR
    !  *
     END