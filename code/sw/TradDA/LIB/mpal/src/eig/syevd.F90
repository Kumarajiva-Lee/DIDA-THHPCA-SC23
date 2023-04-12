SUBROUTINE MPAL_DSYEVD( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, IWORK, &
                       LIWORK, INFO )

    CHARACTER          JOBZ, UPLO
    INTEGER            INFO, LDA, LIWORK, LWORK, N

    INTEGER            IWORK( * )
    TYPE(MPAL_ST)      A( LDA, * ), W( * ), WORK( * ) 

    TYPE(MPAL_ST)      SAFMIN, EPS

    LOGICAL            LOWER, LQUERY, WANTZ
    INTEGER            IINFO, INDE, INDTAU, INDWK2, INDWRK, ISCALE, &
                       LIOPT, LIWMIN, LLWORK, LLWRK2, LOPT, LWMIN

    TYPE(MPAL_ST)      BIGNUM, RMAX, RMIN, SMLNUM, &
                       ZERO, ONE, SIGMA

    TYPE(MPAL_ST)      ANRM

    LOGICAL            LSAME
    INTEGER            ILAENV
    EXTERNAL           LSAME, ILAENV

    EXTERNAL           XERBLA

    INTRINSIC          MAX, SQRT

    ZERO = 0.0D+0
    ONE = 1.0D+0

    WANTZ = LSAME( JOBZ, 'V' )
    LOWER = LSAME( UPLO, 'L' )
    LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )

    INFO = 0
    IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
        INFO = -1
    ELSE IF( .NOT.( LOWER .OR. LSAME( UPLO, 'U' ) ) ) THEN
        INFO = -2
    ELSE IF( N.LT.0 ) THEN
        INFO = -3
    ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
        INFO = -5
    END IF

    IF( INFO.EQ.0 ) THEN
        IF( N.LE.1 ) THEN
            LIWMIN = 1
            LWMIN = 1
            LOPT = LWMIN
            LIOPT = LIWMIN
        ELSE
            IF( WANTZ ) THEN
                LIWMIN = 3 + 5*N
                LWMIN = 1 + 6*N + 2*N**2
            ELSE
                LIWMIN = 1
                LWMIN = 2*N + 1
            END IF
            LOPT = MAX( LWMIN, 2*N + &
                    ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 ) )
           LIOPT = LIWMIN
        END IF
        WORK( 1 ) = LOPT
        IWORK( 1 ) = LIOPT

        IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
           INFO = -8
        ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
           INFO = -10
        END IF
    END IF

    IF( INFO.NE.0 ) THEN
        CALL XERBLA( 'DSYEVD', -INFO )
        RETURN
    ELSE IF( LQUERY ) THEN
        RETURN
    END IF

    IF( N.EQ.0 ) RETURN

    IF( N.EQ.1 ) THEN
        W( 1 ) = A( 1, 1 )
        IF( WANTZ ) &
            A( 1, 1 ) = ONE
        RETURN
    END IF

    SAFMIN = MPAL_DLAMCH( 'Safe minimum' )
    EPS = MPAL_DLAMCH( 'Precision' )
    SMLNUM = SAFMIN / EPS
    BIGNUM = ONE / SMLNUM
    RMIN = SQRT( SMLNUM )
    RMAX = SQRT( BIGNUM )
! *
! *     Scale matrix to allowable range, if necessary.
! *
    ANRM = MPAL_DLANSY( 'M', UPLO, N, A, LDA, WORK )
    ISCALE = 0
    IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
        ISCALE = 1
        SIGMA = RMIN / ANRM
    ELSE IF( ANRM.GT.RMAX ) THEN
        ISCALE = 1
        SIGMA = RMAX / ANRM
    END IF

    IF( ISCALE.EQ.1 ) &
        CALL MPAL_DLASCL( UPLO, 0, 0, ONE, SIGMA, N, N, A, LDA, INFO )
! *
! *     Call DSYTRD to reduce symmetric matrix to tridiagonal form.
! *
    INDE = 1
    INDTAU = INDE + N
    INDWRK = INDTAU + N
    LLWORK = LWORK - INDWRK + 1
    INDWK2 = INDWRK + N*N
    LLWRK2 = LWORK - INDWK2 + 1

    CALL MPAL_DSYTRD( UPLO, N, A, LDA, W, WORK( INDE ), WORK( INDTAU ), &
                WORK( INDWRK ), LLWORK, IINFO )
    
! *
! *     For eigenvalues only, call DSTERF.  For eigenvectors, first call
! *     DSTEDC to generate the eigenvector matrix, WORK(INDWRK), of the
! *     tridiagonal matrix, then call DORMTR to multiply it by the
! *     Householder transformations stored in A.
! *
    IF( .NOT.WANTZ ) THEN
        WRITE (*, *) "Not implemented!"
        !CALL DSTERF( N, W, WORK( INDE ), INFO )
    ELSE
        CALL MPAL_DSTEDC( 'I', N, W, WORK( INDE ), WORK( INDWRK ), N, &
                    WORK( INDWK2 ), LLWRK2, IWORK, LIWORK, INFO )
        CALL MPAL_DORMTR( 'L', UPLO, 'N', N, N, A, LDA, WORK( INDTAU ), &
                    WORK( INDWRK ), N, WORK( INDWK2 ), LLWRK2, IINFO )
        CALL MPAL_DLACPY( 'A', N, N, WORK( INDWRK ), N, A, LDA )
    END IF
! *
! *     If matrix was scaled, then rescale eigenvalues appropriately.
! *
    IF( ISCALE.EQ.1 ) &
        CALL MPAL_DSCAL( N, ONE / SIGMA, W, 1 )

    WORK( 1 ) = LOPT
    IWORK( 1 ) = LIOPT

    RETURN
! *
! *     End of DSYEVD
! *
    END
