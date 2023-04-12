SUBROUTINE MPAL_GEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
    ! *
    ! *  -- Reference BLAS level3 routine --
    ! *  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
    ! *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    ! *
    ! *     .. Scalar Arguments ..
          TYPE(MPAL_ST) ALPHA,BETA
          INTEGER K,LDA,LDB,LDC,M,N
          CHARACTER TRANSA,TRANSB
    ! *     ..
    ! *     .. Array Arguments ..
          TYPE(MPAL_ST) A(LDA,*),B(LDB,*),C(LDC,*)
    ! *     ..
    ! *
    ! *  =====================================================================
    ! *
    ! *     .. External Functions ..
          LOGICAL LSAME
          EXTERNAL LSAME
    ! *     ..
    ! *     .. External Subroutines ..
          EXTERNAL XERBLA
    ! *     ..
    ! *     .. Intrinsic Functions ..
          INTRINSIC MAX
    ! *     ..
    ! *     .. Local Scalars ..
          TYPE(MPAL_ST) TEMP
          INTEGER I,INFO,J,L,NROWA,NROWB
          LOGICAL NOTA,NOTB
    ! *     ..
    ! *     .. Parameters ..
          TYPE(MPAL_ST) ONE,ZERO
          
          ONE=1.0D+0
          ZERO=0.0D+0
    ! *     ..
    ! *
    ! *     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
    ! *     transposed and set  NROWA and NROWB  as the number of rows of  A
    ! *     and  B  respectively.
    ! *

          NOTA = LSAME(TRANSA,'N')
          NOTB = LSAME(TRANSB,'N')
          IF (NOTA) THEN
              NROWA = M
          ELSE
              NROWA = K
          END IF
          IF (NOTB) THEN
              NROWB = K
          ELSE
              NROWB = N
          END IF
    ! *
    ! *     Test the input parameters.
    ! *
          INFO = 0
          IF ((.NOT.NOTA) .AND. (.NOT.LSAME(TRANSA,'C')) .AND. &
             (.NOT.LSAME(TRANSA,'T'))) THEN
              INFO = 1
          ELSE IF ((.NOT.NOTB) .AND. (.NOT.LSAME(TRANSB,'C')) .AND. &
                  (.NOT.LSAME(TRANSB,'T'))) THEN
              INFO = 2
          ELSE IF (M.LT.0) THEN
              INFO = 3
          ELSE IF (N.LT.0) THEN
              INFO = 4
          ELSE IF (K.LT.0) THEN
              INFO = 5
          ELSE IF (LDA.LT.MAX(1,NROWA)) THEN
              INFO = 8
          ELSE IF (LDB.LT.MAX(1,NROWB)) THEN
              INFO = 10
          ELSE IF (LDC.LT.MAX(1,M)) THEN
              INFO = 13
          END IF
          IF (INFO.NE.0) THEN
              CALL XERBLA('DGEMM ',INFO)
              RETURN
          END IF
    ! *
    ! *     Quick return if possible.
    ! *
          IF ((M.EQ.0) .OR. (N.EQ.0) .OR. &
             (((ALPHA.EQ.ZERO).OR. (K.EQ.0)).AND. (BETA.EQ.ONE))) RETURN
    ! *
    ! *     And if  alpha.eq.zero.
    ! *
          IF (ALPHA.EQ.ZERO) THEN
              IF (BETA.EQ.ZERO) THEN
                  DO 20 J = 1,N
                      DO 10 I = 1,M
                          C(I,J) = ZERO
       10             CONTINUE
       20         CONTINUE
              ELSE
                  DO 40 J = 1,N
                      DO 30 I = 1,M
                          C(I,J) = BETA*C(I,J)
       30             CONTINUE
       40         CONTINUE
              END IF
              RETURN
          END IF
    ! *
    ! *     Start the operations.
    ! *
          IF (NOTB) THEN
              IF (NOTA) THEN
    ! *
    ! *           Form  C := alpha*A*B + beta*C.
    ! *
                  DO 90 J = 1,N
                      IF (BETA.EQ.ZERO) THEN
                          DO 50 I = 1,M
                              C(I,J) = ZERO
       50                 CONTINUE
                      ELSE IF (BETA.NE.ONE) THEN
                          DO 60 I = 1,M
                              C(I,J) = BETA*C(I,J)
       60                 CONTINUE
                      END IF
                      DO 80 L = 1,K
                          TEMP = ALPHA*B(L,J)
                          DO 70 I = 1,M
                              C(I,J) = C(I,J) + TEMP*A(I,L)
       70                 CONTINUE
       80             CONTINUE
       90         CONTINUE
              ELSE
    ! *
    ! *           Form  C := alpha*A**T*B + beta*C
    ! *
                  DO 120 J = 1,N
                      DO 110 I = 1,M
                          TEMP = ZERO
                          DO 100 L = 1,K
                              TEMP = TEMP + A(L,I)*B(L,J)
      100                 CONTINUE
                          IF (BETA.EQ.ZERO) THEN
                              C(I,J) = ALPHA*TEMP
                          ELSE
                              C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                          END IF
      110             CONTINUE
      120         CONTINUE
              END IF
          ELSE
              IF (NOTA) THEN
    ! *
    ! *           Form  C := alpha*A*B**T + beta*C
    ! *
                  DO 170 J = 1,N
                      IF (BETA.EQ.ZERO) THEN
                          DO 130 I = 1,M
                              C(I,J) = ZERO
      130                 CONTINUE
                      ELSE IF (BETA.NE.ONE) THEN
                          DO 140 I = 1,M
                              C(I,J) = BETA*C(I,J)
      140                 CONTINUE
                      END IF
                      DO 160 L = 1,K
                          TEMP = ALPHA*B(J,L)
                          DO 150 I = 1,M
                              C(I,J) = C(I,J) + TEMP*A(I,L)
      150                 CONTINUE
      160             CONTINUE
      170         CONTINUE
              ELSE
    ! *
    ! *           Form  C := alpha*A**T*B**T + beta*C
    ! *
                  DO 200 J = 1,N
                      DO 190 I = 1,M
                          TEMP = ZERO
                          DO 180 L = 1,K
                              TEMP = TEMP + A(L,I)*B(J,L)
      180                 CONTINUE
                          IF (BETA.EQ.ZERO) THEN
                              C(I,J) = ALPHA*TEMP
                          ELSE
                              C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                          END IF
      190             CONTINUE
      200         CONTINUE
              END IF
          END IF

          RETURN
    ! *
    ! *     End of DGEMM .
    ! *
          END  

! subroutine mpal_gemm(TRANSA, TRANSB, M, N, K, A, LDA, B, LDB, C)
!     character, intent(in) :: TRANSA
!     character, intent(in) :: TRANSB
!     integer, intent(in) :: M
!     integer, intent(in) :: N
!     integer, intent(in) :: K
!     type(mpal_st), dimension(lda,*), intent(in) :: A
!     integer, intent(in) :: LDA
!     type(mpal_st), dimension(ldb,*), intent(in) :: B
!     integer, intent(in) :: LDB
!     type(mpal_st), dimension(M, N), intent(inout) :: C
!     !integer, intent(in) :: LDC

!     !type(mpal_st) :: tmp

!     integer :: x, y, z
!     if ((TRANSA == 'n' .or. TRANSA == 'N') &
!     .and. (TRANSB == 'n'.or. TRANSB == 'N')) then
!         do y = 1, n
!             do x = 1, m
!                 c(x, y) = 0d0
!             end do
!         end do
!         do y = 1, n
!             do z = 1, k
!                 do x = 1, m
!                     c(x, y) = c(x, y) + a(x, z) * b(z, y)
!                 end do
!             end do
!         end do
!     else if ((TRANSA == 'n' .or. TRANSA == 'N') &
!     .and. (TRANSB == 't'.or. TRANSB == 'T')) then
!         do y = 1, n
!             do x = 1, m
!                 c(x, y) = 0d0
!             end do
!         end do
!         do z = 1, k
!             do y = 1, n
!                 do x = 1, m
!                     c(x, y) = c(x, y) + a(x, z) * b(y, z)
!                 end do
!             end do
!         end do
!     else
!         write(*, *) "warning: not implemented!"
!     end if

! end subroutine