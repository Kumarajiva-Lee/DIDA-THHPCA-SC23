SUBROUTINE MPAL_DLAED6( KNITER, ORGATI, RHO, D, Z, FINIT, TAU, INFO )

         LOGICAL            ORGATI
         INTEGER            INFO, KNITER
         TYPE(MPAL_ST)      FINIT, RHO, TAU

         TYPE(MPAL_ST)      D( 3 ), Z( 3 )

         INTEGER            MAXIT
         PARAMETER          ( MAXIT = 40 )
         TYPE(MPAL_ST)      ZERO, ONE, TWO, THREE, FOUR, EIGHT

         TYPE(MPAL_ST)      DSCALE( 3 ), ZSCALE( 3 )

         LOGICAL            SCALE
         INTEGER            I, ITER, NITER
         TYPE(MPAL_ST)      A, B, BASE, C, DDF, DF, EPS, ERRETM, ETA, F, &
                            FC, SCLFAC, SCLINV, SMALL1, SMALL2, SMINV1, &
                            SMINV2, TEMP, TEMP1, TEMP2, TEMP3, TEMP4, &
                            LBD, UBD

         INTRINSIC          ABS, INT, LOG, MAX, MIN, SQRT

         ZERO = 0.0D0
         ONE = 1.0D0
         TWO = 2.0D0
         THREE = 3.0D0
         FOUR = 4.0D0
         EIGHT = 8.0D0

         INFO = 0

         IF( ORGATI ) THEN
            LBD = D(2)
            UBD = D(3)
         ELSE
            LBD = D(1)
            UBD = D(2)
         END IF
         IF( FINIT .LT. ZERO )THEN
            LBD = ZERO
         ELSE
            UBD = ZERO
         END IF

         NITER = 1
         TAU = ZERO
         IF( KNITER.EQ.2 ) THEN
            IF( ORGATI ) THEN
               TEMP = ( D( 3 )-D( 2 ) ) / TWO
               C = RHO + Z( 1 ) / ( ( D( 1 )-D( 2 ) )-TEMP )
               A = C*( D( 2 )+D( 3 ) ) + Z( 2 ) + Z( 3 )
               B = C*D( 2 )*D( 3 ) + Z( 2 )*D( 3 ) + Z( 3 )*D( 2 )
            ELSE
               TEMP = ( D( 1 )-D( 2 ) ) / TWO
               C = RHO + Z( 3 ) / ( ( D( 3 )-D( 2 ) )-TEMP )
               A = C*( D( 1 )+D( 2 ) ) + Z( 1 ) + Z( 2 )
               B = C*D( 1 )*D( 2 ) + Z( 1 )*D( 2 ) + Z( 2 )*D( 1 )
            END IF
            TEMP = MAX( ABS( A ), ABS( B ), ABS( C ) )
            A = A / TEMP
            B = B / TEMP
            C = C / TEMP
            IF( C.EQ.ZERO ) THEN
               TAU = B / A
            ELSE IF( A.LE.ZERO ) THEN
               TAU = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            ELSE
               TAU = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
            END IF
            IF( TAU .LT. LBD .OR. TAU .GT. UBD ) &
               TAU = ( LBD+UBD )/TWO
            IF( D(1).EQ.TAU .OR. D(2).EQ.TAU .OR. D(3).EQ.TAU ) THEN
               TAU = ZERO
            ELSE
               TEMP = FINIT + TAU*Z(1)/( D(1)*( D( 1 )-TAU ) ) + &
                              TAU*Z(2)/( D(2)*( D( 2 )-TAU ) ) + &
                              TAU*Z(3)/( D(3)*( D( 3 )-TAU ) )
               IF( TEMP .LE. ZERO )THEN
                  LBD = TAU
               ELSE
                  UBD = TAU
               END IF
               IF( ABS( FINIT ).LE.ABS( TEMP ) ) &
                  TAU = ZERO
            END IF
         END IF
         EPS = MPAL_DLAMCH( 'Epsilon' )
         BASE = MPAL_DLAMCH( 'Base' )
         SMALL1 = BASE**( INT( LOG( MPAL_DLAMCH( 'SafMin' ) ) / LOG( BASE ) / &
                  THREE ) )
         SMINV1 = ONE / SMALL1
         SMALL2 = SMALL1*SMALL1
         SMINV2 = SMINV1*SMINV1
   
         
         IF( ORGATI ) THEN
            TEMP = MIN( ABS( D( 2 )-TAU ), ABS( D( 3 )-TAU ) )
         ELSE
            TEMP = MIN( ABS( D( 1 )-TAU ), ABS( D( 2 )-TAU ) )
         END IF
         SCALE = .FALSE.
         IF( TEMP.LE.SMALL1 ) THEN
            SCALE = .TRUE.
            IF( TEMP.LE.SMALL2 ) THEN
               SCLFAC = SMINV2
               SCLINV = SMALL2
            ELSE
               SCLFAC = SMINV1
               SCLINV = SMALL1
            END IF
            DO 10 I = 1, 3
               DSCALE( I ) = D( I )*SCLFAC
               ZSCALE( I ) = Z( I )*SCLFAC
      10    CONTINUE
            TAU = TAU*SCLFAC
            LBD = LBD*SCLFAC
            UBD = UBD*SCLFAC
         ELSE
            DO 20 I = 1, 3
               DSCALE( I ) = D( I )
               ZSCALE( I ) = Z( I )
      20    CONTINUE
         END IF
         FC = ZERO
         DF = ZERO
         DDF = ZERO
         DO 30 I = 1, 3
            TEMP = ONE / ( DSCALE( I )-TAU )
            TEMP1 = ZSCALE( I )*TEMP
            TEMP2 = TEMP1*TEMP
            TEMP3 = TEMP2*TEMP
            FC = FC + TEMP1 / DSCALE( I )
            DF = DF + TEMP2
            DDF = DDF + TEMP3
      30 CONTINUE
         F = FINIT + TAU*FC
         IF( ABS( F ).LE.ZERO ) GO TO 60
         IF( F .LE. ZERO )THEN
            LBD = TAU
         ELSE
            UBD = TAU
         END IF
         ITER = NITER + 1
   
         DO 50 NITER = ITER, MAXIT
            IF( ORGATI ) THEN
               TEMP1 = DSCALE( 2 ) - TAU
               TEMP2 = DSCALE( 3 ) - TAU
            ELSE
               TEMP1 = DSCALE( 1 ) - TAU
               TEMP2 = DSCALE( 2 ) - TAU
            END IF
            A = ( TEMP1+TEMP2 )*F - TEMP1*TEMP2*DF
            B = TEMP1*TEMP2*F
            C = F - ( TEMP1+TEMP2 )*DF + TEMP1*TEMP2*DDF
            TEMP = MAX( ABS( A ), ABS( B ), ABS( C ) )
            A = A / TEMP
            B = B / TEMP
            C = C / TEMP
            IF( C.EQ.ZERO ) THEN
               ETA = B / A
            ELSE IF( A.LE.ZERO ) THEN
               ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            ELSE
               ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
            END IF
            IF( F*ETA.GE.ZERO ) THEN
               ETA = -F / DF
            END IF

            TAU = TAU + ETA
            IF( TAU .LT. LBD .OR. TAU .GT. UBD ) &
               TAU = ( LBD + UBD )/TWO
            FC = ZERO
            ERRETM = ZERO
            DF = ZERO
            DDF = ZERO
            DO 40 I = 1, 3
               IF ( ( DSCALE( I )-TAU ).NE.ZERO ) THEN
                  TEMP = ONE / ( DSCALE( I )-TAU )
                  TEMP1 = ZSCALE( I )*TEMP
                  TEMP2 = TEMP1*TEMP
                  TEMP3 = TEMP2*TEMP
                  TEMP4 = TEMP1 / DSCALE( I )
                  FC = FC + TEMP4
                  ERRETM = ERRETM + ABS( TEMP4 )
                  DF = DF + TEMP2
                  DDF = DDF + TEMP3
               ELSE
                  GO TO 60
               END IF
      40    CONTINUE
            F = FINIT + TAU*FC
            ERRETM = EIGHT*( ABS( FINIT )+ABS( TAU )*ERRETM ) + &
                     ABS( TAU )*DF
            IF( ( ABS( F ).LE.FOUR*EPS*ERRETM ) .OR. &
               ( (UBD-LBD).LE.FOUR*EPS*ABS(TAU) )  ) &
               GO TO 60
            IF( F .LE. ZERO )THEN
               LBD = TAU
            ELSE
               UBD = TAU
            END IF
      50 CONTINUE
         INFO = 1
      60 CONTINUE

         IF( SCALE ) TAU = TAU*SCLINV
         RETURN
         END

SUBROUTINE MPAL_DLAED5( I, D, Z, DELTA, RHO, DLAM )
         
         INTEGER            I
         TYPE(MPAL_ST)      DLAM, RHO

         TYPE(MPAL_ST)      D( 2 ), DELTA( 2 ), Z( 2 )

         TYPE(MPAL_ST)      ZERO, ONE, TWO, FOUR

         TYPE(MPAL_ST)      B, C, DEL, TAU, TEMP, W

         INTRINSIC          ABS, SQRT

         ZERO = 0.0D0
         ONE = 1.0D0
         TWO = 2.0D0
         FOUR = 4.0D0

         DEL = D( 2 ) - D( 1 )
         IF( I.EQ.1 ) THEN
            W = ONE + TWO*RHO*( Z( 2 )*Z( 2 )-Z( 1 )*Z( 1 ) ) / DEL
            IF( W.GT.ZERO ) THEN
               B = DEL + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
               C = RHO*Z( 1 )*Z( 1 )*DEL
               TAU = TWO*C / ( B+SQRT( ABS( B*B-FOUR*C ) ) )
               DLAM = D( 1 ) + TAU
               DELTA( 1 ) = -Z( 1 ) / TAU
               DELTA( 2 ) = Z( 2 ) / ( DEL-TAU )
            ELSE
               B = -DEL + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
               C = RHO*Z( 2 )*Z( 2 )*DEL
               IF( B.GT.ZERO ) THEN
                  TAU = -TWO*C / ( B+SQRT( B*B+FOUR*C ) )
               ELSE
                  TAU = ( B-SQRT( B*B+FOUR*C ) ) / TWO
               END IF
               DLAM = D( 2 ) + TAU
               DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
               DELTA( 2 ) = -Z( 2 ) / TAU
            END IF
            TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
            DELTA( 1 ) = DELTA( 1 ) / TEMP
            DELTA( 2 ) = DELTA( 2 ) / TEMP
         ELSE
            B = -DEL + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
            C = RHO*Z( 2 )*Z( 2 )*DEL
            IF( B.GT.ZERO ) THEN
               TAU = ( B+SQRT( B*B+FOUR*C ) ) / TWO
            ELSE
               TAU = TWO*C / ( -B+SQRT( B*B+FOUR*C ) )
            END IF
            DLAM = D( 2 ) + TAU
            DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
            DELTA( 2 ) = -Z( 2 ) / TAU
            TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
            DELTA( 1 ) = DELTA( 1 ) / TEMP
            DELTA( 2 ) = DELTA( 2 ) / TEMP
         END IF
         RETURN
         END
   

SUBROUTINE MPAL_DLAED4( N, I, D, Z, DELTA, RHO, DLAM, INFO )
         INTEGER            I, INFO, N
         TYPE(MPAL_ST)      DLAM, RHO

         TYPE(MPAL_ST)      D( * ), DELTA( * ), Z( * )

         INTEGER            MAXIT
         PARAMETER          ( MAXIT = 30 )
         TYPE(MPAL_ST)      ZERO, ONE, TWO, THREE, FOUR, EIGHT, TEN

         LOGICAL            ORGATI, SWTCH, SWTCH3
         INTEGER            II, IIM1, IIP1, IP1, ITER, J, NITER
         TYPE(MPAL_ST)      A, B, C, DEL, DLTLB, DLTUB, DPHI, DPSI, DW, &
                            EPS, ERRETM, ETA, MIDPT, PHI, PREW, PSI, &
                            RHOINV, TAU, TEMP, TEMP1, W

         TYPE(MPAL_ST)      ZZ( 3 )

         INTRINSIC          ABS, MAX, MIN, SQRT

         ZERO = 0.0D0
         ONE = 1.0D0
         TWO = 2.0D0
         THREE = 3.0D0
         FOUR = 4.0D0
         EIGHT = 8.0D0
         TEN = 10.0D0

         INFO = 0
         IF( N.EQ.1 ) THEN
            DLAM = D( 1 ) + RHO*Z( 1 )*Z( 1 )
            DELTA( 1 ) = ONE
            RETURN
         END IF
         IF( N.EQ.2 ) THEN
            CALL MPAL_DLAED5( I, D, Z, DELTA, RHO, DLAM )
            RETURN
         END IF
         EPS = MPAL_DLAMCH( 'Epsilon' )
         RHOINV = ONE / RHO

         IF( I.EQ.N ) THEN
            II = N - 1
            NITER = 1
            MIDPT = RHO / TWO
            DO 10 J = 1, N
               DELTA( J ) = ( D( J )-D( I ) ) - MIDPT
      10    CONTINUE
            PSI = ZERO
            DO 20 J = 1, N - 2
               PSI = PSI + Z( J )*Z( J ) / DELTA( J )
      20    CONTINUE
            C = RHOINV + PSI
            W = C + Z( II )*Z( II ) / DELTA( II ) + &
                Z( N )*Z( N ) / DELTA( N )
   
            IF( W.LE.ZERO ) THEN
               TEMP = Z( N-1 )*Z( N-1 ) / ( D( N )-D( N-1 )+RHO ) + &
                      Z( N )*Z( N ) / RHO
               IF( C.LE.TEMP ) THEN
                  TAU = RHO
               ELSE
                  DEL = D( N ) - D( N-1 )
                  A = -C*DEL + Z( N-1 )*Z( N-1 ) + Z( N )*Z( N )
                  B = Z( N )*Z( N )*DEL
                  IF( A.LT.ZERO ) THEN
                     TAU = TWO*B / ( SQRT( A*A+FOUR*B*C )-A )
                  ELSE
                     TAU = ( A+SQRT( A*A+FOUR*B*C ) ) / ( TWO*C )
                  END IF
               END IF
               DLTLB = MIDPT
               DLTUB = RHO
            ELSE
               DEL = D( N ) - D( N-1 )
               A = -C*DEL + Z( N-1 )*Z( N-1 ) + Z( N )*Z( N )
               B = Z( N )*Z( N )*DEL
               IF( A.LT.ZERO ) THEN
                  TAU = TWO*B / ( SQRT( A*A+FOUR*B*C )-A )
               ELSE
                  TAU = ( A+SQRT( A*A+FOUR*B*C ) ) / ( TWO*C )
               END IF
               DLTLB = ZERO
               DLTUB = MIDPT
            END IF
            DO 30 J = 1, N
               DELTA( J ) = ( D( J )-D( I ) ) - TAU
      30    CONTINUE
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            DO 40 J = 1, II
               TEMP = Z( J ) / DELTA( J )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
      40    CONTINUE
            ERRETM = ABS( ERRETM )
            TEMP = Z( N ) / DELTA( N )
            PHI = Z( N )*TEMP
            DPHI = TEMP*TEMP
            ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV + &
                     ABS( TAU )*( DPSI+DPHI )
 
            W = RHOINV + PHI + PSI
 
            IF( ABS( W ).LE.EPS*ERRETM ) THEN
               DLAM = D( I ) + TAU
               GO TO 250
            END IF

            IF( W.LE.ZERO ) THEN
               DLTLB = MAX( DLTLB, TAU )
            ELSE
               DLTUB = MIN( DLTUB, TAU )
            END IF

            NITER = NITER + 1
            C = W - DELTA( N-1 )*DPSI - DELTA( N )*DPHI
            A = ( DELTA( N-1 )+DELTA( N ) )*W - &
                DELTA( N-1 )*DELTA( N )*( DPSI+DPHI )
            B = DELTA( N-1 )*DELTA( N )*W
            IF( C.LT.ZERO ) C = ABS( C )
            IF( C.EQ.ZERO ) THEN
               ETA = DLTUB - TAU
            ELSE IF( A.GE.ZERO ) THEN
               ETA = ( A+SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            ELSE
               ETA = TWO*B / ( A-SQRT( ABS( A*A-FOUR*B*C ) ) )
            END IF
            IF( W*ETA.GT.ZERO ) &
               ETA = -W / ( DPSI+DPHI )
           TEMP = TAU + ETA
            IF( TEMP.GT.DLTUB .OR. TEMP.LT.DLTLB ) THEN
               IF( W.LT.ZERO ) THEN
                  ETA = ( DLTUB-TAU ) / TWO
               ELSE
                  ETA = ( DLTLB-TAU ) / TWO
               END IF
            END IF
            DO 50 J = 1, N
               DELTA( J ) = DELTA( J ) - ETA
      50    CONTINUE
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            DO 60 J = 1, II
               TEMP = Z( J ) / DELTA( J )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
      60    CONTINUE
            ERRETM = ABS( ERRETM )
            TEMP = Z( N ) / DELTA( N )
            PHI = Z( N )*TEMP
            DPHI = TEMP*TEMP
            ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV + &
                     ABS( TAU )*( DPSI+DPHI )
            W = RHOINV + PHI + PSI
            ITER = NITER + 1
            DO 90 NITER = ITER, MAXIT
               IF( ABS( W ).LE.EPS*ERRETM ) THEN
                  DLAM = D( I ) + TAU
                  GO TO 250
               END IF
               IF( W.LE.ZERO ) THEN
                  DLTLB = MAX( DLTLB, TAU )
               ELSE
                  DLTUB = MIN( DLTUB, TAU )
               END IF
               C = W - DELTA( N-1 )*DPSI - DELTA( N )*DPHI
               A = ( DELTA( N-1 )+DELTA( N ) )*W - &
                   DELTA( N-1 )*DELTA( N )*( DPSI+DPHI )
               B = DELTA( N-1 )*DELTA( N )*W
               IF( A.GE.ZERO ) THEN
                  ETA = ( A+SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
               ELSE
                  ETA = TWO*B / ( A-SQRT( ABS( A*A-FOUR*B*C ) ) )
               END IF
               IF( W*ETA.GT.ZERO ) &
                  ETA = -W / ( DPSI+DPHI )
               TEMP = TAU + ETA
               IF( TEMP.GT.DLTUB .OR. TEMP.LT.DLTLB ) THEN
                  IF( W.LT.ZERO ) THEN
                     ETA = ( DLTUB-TAU ) / TWO
                  ELSE
                     ETA = ( DLTLB-TAU ) / TWO
                  END IF
               END IF
               DO 70 J = 1, N
                  DELTA( J ) = DELTA( J ) - ETA
      70       CONTINUE

               TAU = TAU + ETA
   
               DPSI = ZERO
               PSI = ZERO
               ERRETM = ZERO
               DO 80 J = 1, II
                  TEMP = Z( J ) / DELTA( J )
                  PSI = PSI + Z( J )*TEMP
                  DPSI = DPSI + TEMP*TEMP
                  ERRETM = ERRETM + PSI
      80       CONTINUE
               ERRETM = ABS( ERRETM )
               TEMP = Z( N ) / DELTA( N )
               PHI = Z( N )*TEMP
               DPHI = TEMP*TEMP
               ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV + &
                        ABS( TAU )*( DPSI+DPHI )
   
               W = RHOINV + PHI + PSI
      90    CONTINUE
            INFO = 1
            DLAM = D( I ) + TAU
            GO TO 250
         ELSE
            NITER = 1
            IP1 = I + 1
            DEL = D( IP1 ) - D( I )
            MIDPT = DEL / TWO
            DO 100 J = 1, N
               DELTA( J ) = ( D( J )-D( I ) ) - MIDPT
     100    CONTINUE
            PSI = ZERO
            DO 110 J = 1, I - 1
               PSI = PSI + Z( J )*Z( J ) / DELTA( J )
     110    CONTINUE
            PHI = ZERO
            DO 120 J = N, I + 2, -1
               PHI = PHI + Z( J )*Z( J ) / DELTA( J )
     120    CONTINUE
            C = RHOINV + PSI + PHI
            W = C + Z( I )*Z( I ) / DELTA( I ) + &
                Z( IP1 )*Z( IP1 ) / DELTA( IP1 )
   
            IF( W.GT.ZERO ) THEN
               ORGATI = .TRUE.
               A = C*DEL + Z( I )*Z( I ) + Z( IP1 )*Z( IP1 )
               B = Z( I )*Z( I )*DEL
               IF( A.GT.ZERO ) THEN
                  TAU = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
               ELSE
                  TAU = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
               END IF
               DLTLB = ZERO
               DLTUB = MIDPT
            ELSE
               ORGATI = .FALSE.
               A = C*DEL - Z( I )*Z( I ) - Z( IP1 )*Z( IP1 )
               B = Z( IP1 )*Z( IP1 )*DEL
               IF( A.LT.ZERO ) THEN
                  TAU = TWO*B / ( A-SQRT( ABS( A*A+FOUR*B*C ) ) )
               ELSE
                  TAU = -( A+SQRT( ABS( A*A+FOUR*B*C ) ) ) / ( TWO*C )
               END IF
               DLTLB = -MIDPT
               DLTUB = ZERO
            END IF
            IF( ORGATI ) THEN
               DO 130 J = 1, N
                  DELTA( J ) = ( D( J )-D( I ) ) - TAU
     130       CONTINUE
            ELSE
               DO 140 J = 1, N
                  DELTA( J ) = ( D( J )-D( IP1 ) ) - TAU
     140       CONTINUE
            END IF
            IF( ORGATI ) THEN
               II = I
            ELSE
               II = I + 1
            END IF
            IIM1 = II - 1
            IIP1 = II + 1
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            DO 150 J = 1, IIM1
               TEMP = Z( J ) / DELTA( J )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
     150    CONTINUE
            ERRETM = ABS( ERRETM )
            DPHI = ZERO
            PHI = ZERO
            DO 160 J = N, IIP1, -1
               TEMP = Z( J ) / DELTA( J )
               PHI = PHI + Z( J )*TEMP
               DPHI = DPHI + TEMP*TEMP
               ERRETM = ERRETM + PHI
     160    CONTINUE
            W = RHOINV + PHI + PSI
            SWTCH3 = .FALSE.
            IF( ORGATI ) THEN
               IF( W.LT.ZERO ) SWTCH3 = .TRUE.
            ELSE
               IF( W.GT.ZERO ) SWTCH3 = .TRUE.
            END IF
            IF( II.EQ.1 .OR. II.EQ.N ) SWTCH3 = .FALSE.
            TEMP = Z( II ) / DELTA( II )
            DW = DPSI + DPHI + TEMP*TEMP
            TEMP = Z( II )*TEMP
            W = W + TEMP
            ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV + &
                     THREE*ABS( TEMP ) + ABS( TAU )*DW
            IF( ABS( W ).LE.EPS*ERRETM ) THEN
               IF( ORGATI ) THEN
                  DLAM = D( I ) + TAU
               ELSE
                  DLAM = D( IP1 ) + TAU
               END IF
               GO TO 250
            END IF
            IF( W.LE.ZERO ) THEN
               DLTLB = MAX( DLTLB, TAU )
            ELSE
               DLTUB = MIN( DLTUB, TAU )
            END IF
            NITER = NITER + 1
            IF( .NOT.SWTCH3 ) THEN
               IF( ORGATI ) THEN
                  C = W - DELTA( IP1 )*DW - ( D( I )-D( IP1 ) )* &
                      ( Z( I ) / DELTA( I ) )**2
               ELSE
                  C = W - DELTA( I )*DW - ( D( IP1 )-D( I ) )* &
                      ( Z( IP1 ) / DELTA( IP1 ) )**2
               END IF
               A = ( DELTA( I )+DELTA( IP1 ) )*W - &
                   DELTA( I )*DELTA( IP1 )*DW
               B = DELTA( I )*DELTA( IP1 )*W
               IF( C.EQ.ZERO ) THEN
                  IF( A.EQ.ZERO ) THEN
                     IF( ORGATI ) THEN
                        A = Z( I )*Z( I ) + DELTA( IP1 )*DELTA( IP1 )* &
                            ( DPSI+DPHI )
                     ELSE
                        A = Z( IP1 )*Z( IP1 ) + DELTA( I )*DELTA( I )* &
                            ( DPSI+DPHI )
                     END IF
                  END IF
                  ETA = B / A
               ELSE IF( A.LE.ZERO ) THEN
                  ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
               ELSE
                  ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
               END IF
            ELSE
               TEMP = RHOINV + PSI + PHI
               IF( ORGATI ) THEN
                  TEMP1 = Z( IIM1 ) / DELTA( IIM1 )
                  TEMP1 = TEMP1*TEMP1
                  C = TEMP - DELTA( IIP1 )*( DPSI+DPHI ) - &
                      ( D( IIM1 )-D( IIP1 ) )*TEMP1
                  ZZ( 1 ) = Z( IIM1 )*Z( IIM1 )
                  ZZ( 3 ) = DELTA( IIP1 )*DELTA( IIP1 )* &
                            ( ( DPSI-TEMP1 )+DPHI )
               ELSE
                  TEMP1 = Z( IIP1 ) / DELTA( IIP1 )
                  TEMP1 = TEMP1*TEMP1
                  C = TEMP - DELTA( IIM1 )*( DPSI+DPHI ) - &
                      ( D( IIP1 )-D( IIM1 ) )*TEMP1
                  ZZ( 1 ) = DELTA( IIM1 )*DELTA( IIM1 )* &
                            ( DPSI+( DPHI-TEMP1 ) )
                  ZZ( 3 ) = Z( IIP1 )*Z( IIP1 )
               END IF
               ZZ( 2 ) = Z( II )*Z( II )
               CALL MPAL_DLAED6( NITER, ORGATI, C, DELTA( IIM1 ), ZZ, W, ETA, &
                            INFO )
               IF( INFO.NE.0 ) &
                  GO TO 250
            END IF
            IF( W*ETA.GE.ZERO ) &
               ETA = -W / DW
            TEMP = TAU + ETA
            IF( TEMP.GT.DLTUB .OR. TEMP.LT.DLTLB ) THEN
               IF( W.LT.ZERO ) THEN
                  ETA = ( DLTUB-TAU ) / TWO
               ELSE
                  ETA = ( DLTLB-TAU ) / TWO
               END IF
            END IF
   
            PREW = W
   
            DO 180 J = 1, N
               DELTA( J ) = DELTA( J ) - ETA
     180    CONTINUE
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            DO 190 J = 1, IIM1
               TEMP = Z( J ) / DELTA( J )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
     190    CONTINUE
            ERRETM = ABS( ERRETM )
            DPHI = ZERO
            PHI = ZERO
            DO 200 J = N, IIP1, -1
               TEMP = Z( J ) / DELTA( J )
               PHI = PHI + Z( J )*TEMP
               DPHI = DPHI + TEMP*TEMP
               ERRETM = ERRETM + PHI
     200    CONTINUE
            TEMP = Z( II ) / DELTA( II )
            DW = DPSI + DPHI + TEMP*TEMP
            TEMP = Z( II )*TEMP
            W = RHOINV + PHI + PSI + TEMP
            ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV + &
                     THREE*ABS( TEMP ) + ABS( TAU+ETA )*DW
   
            SWTCH = .FALSE.
            IF( ORGATI ) THEN
               IF( -W.GT.ABS( PREW ) / TEN ) &
                  SWTCH = .TRUE.
            ELSE
               IF( W.GT.ABS( PREW ) / TEN ) &
                  SWTCH = .TRUE.
            END IF
   
            TAU = TAU + ETA
   
            ITER = NITER + 1
   
            DO 240 NITER = ITER, MAXIT
               IF( ABS( W ).LE.EPS*ERRETM ) THEN
                  IF( ORGATI ) THEN
                     DLAM = D( I ) + TAU
                  ELSE
                     DLAM = D( IP1 ) + TAU
                  END IF
                  GO TO 250
               END IF
   
               IF( W.LE.ZERO ) THEN
                  DLTLB = MAX( DLTLB, TAU )
               ELSE
                  DLTUB = MIN( DLTUB, TAU )
               END IF
               IF( .NOT.SWTCH3 ) THEN
                  IF( .NOT.SWTCH ) THEN
                     IF( ORGATI ) THEN
                        C = W - DELTA( IP1 )*DW - &
                            ( D( I )-D( IP1 ) )*( Z( I ) / DELTA( I ) )**2
                     ELSE
                        C = W - DELTA( I )*DW - ( D( IP1 )-D( I ) )* &
                            ( Z( IP1 ) / DELTA( IP1 ) )**2
                     END IF
                  ELSE
                     TEMP = Z( II ) / DELTA( II )
                     IF( ORGATI ) THEN
                        DPSI = DPSI + TEMP*TEMP
                     ELSE
                        DPHI = DPHI + TEMP*TEMP
                     END IF
                     C = W - DELTA( I )*DPSI - DELTA( IP1 )*DPHI
                  END IF
                  A = ( DELTA( I )+DELTA( IP1 ) )*W - &
                      DELTA( I )*DELTA( IP1 )*DW
                  B = DELTA( I )*DELTA( IP1 )*W
                  IF( C.EQ.ZERO ) THEN
                     IF( A.EQ.ZERO ) THEN
                        IF( .NOT.SWTCH ) THEN
                           IF( ORGATI ) THEN
                              A = Z( I )*Z( I ) + DELTA( IP1 )* &
                                  DELTA( IP1 )*( DPSI+DPHI )
                           ELSE
                              A = Z( IP1 )*Z( IP1 ) + &
                                  DELTA( I )*DELTA( I )*( DPSI+DPHI )
                           END IF
                        ELSE
                           A = DELTA( I )*DELTA( I )*DPSI + &
                               DELTA( IP1 )*DELTA( IP1 )*DPHI
                        END IF
                     END IF
                     ETA = B / A
                  ELSE IF( A.LE.ZERO ) THEN
                     ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
                  ELSE
                     ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
                  END IF
               ELSE
                  TEMP = RHOINV + PSI + PHI
                  IF( SWTCH ) THEN
                     C = TEMP - DELTA( IIM1 )*DPSI - DELTA( IIP1 )*DPHI
                     ZZ( 1 ) = DELTA( IIM1 )*DELTA( IIM1 )*DPSI
                     ZZ( 3 ) = DELTA( IIP1 )*DELTA( IIP1 )*DPHI
                  ELSE
                     IF( ORGATI ) THEN
                        TEMP1 = Z( IIM1 ) / DELTA( IIM1 )
                        TEMP1 = TEMP1*TEMP1
                        C = TEMP - DELTA( IIP1 )*( DPSI+DPHI ) - &
                            ( D( IIM1 )-D( IIP1 ) )*TEMP1
                        ZZ( 1 ) = Z( IIM1 )*Z( IIM1 )
                        ZZ( 3 ) = DELTA( IIP1 )*DELTA( IIP1 )* &
                                  ( ( DPSI-TEMP1 )+DPHI )
                     ELSE
                        TEMP1 = Z( IIP1 ) / DELTA( IIP1 )
                        TEMP1 = TEMP1*TEMP1
                        C = TEMP - DELTA( IIM1 )*( DPSI+DPHI ) - &
                            ( D( IIP1 )-D( IIM1 ) )*TEMP1
                        ZZ( 1 ) = DELTA( IIM1 )*DELTA( IIM1 )* &
                                  ( DPSI+( DPHI-TEMP1 ) )
                        ZZ( 3 ) = Z( IIP1 )*Z( IIP1 )
                     END IF
                  END IF
                  CALL MPAL_DLAED6( NITER, ORGATI, C, DELTA( IIM1 ), ZZ, W, ETA, &
                               INFO )
                  IF( INFO.NE.0 ) &
                     GO TO 250
               END IF
                  IF( W*ETA.GE.ZERO ) &
                  ETA = -W / DW
               TEMP = TAU + ETA
               IF( TEMP.GT.DLTUB .OR. TEMP.LT.DLTLB ) THEN
                  IF( W.LT.ZERO ) THEN
                     ETA = ( DLTUB-TAU ) / TWO
                  ELSE
                     ETA = ( DLTLB-TAU ) / TWO
                  END IF
               END IF
   
               DO 210 J = 1, N
                  DELTA( J ) = DELTA( J ) - ETA
     210       CONTINUE
   
               TAU = TAU + ETA
               PREW = W
   
               DPSI = ZERO
               PSI = ZERO
               ERRETM = ZERO
               DO 220 J = 1, IIM1
                  TEMP = Z( J ) / DELTA( J )
                  PSI = PSI + Z( J )*TEMP
                  DPSI = DPSI + TEMP*TEMP
                  ERRETM = ERRETM + PSI
     220       CONTINUE
               ERRETM = ABS( ERRETM )
   
               DPHI = ZERO
               PHI = ZERO
               DO 230 J = N, IIP1, -1
                  TEMP = Z( J ) / DELTA( J )
                  PHI = PHI + Z( J )*TEMP
                  DPHI = DPHI + TEMP*TEMP
                  ERRETM = ERRETM + PHI
     230       CONTINUE
               TEMP = Z( II ) / DELTA( II )
               DW = DPSI + DPHI + TEMP*TEMP
               TEMP = Z( II )*TEMP
               W = RHOINV + PHI + PSI + TEMP
               ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV + &
                        THREE*ABS( TEMP ) + ABS( TAU )*DW
               IF( W*PREW.GT.ZERO .AND. ABS( W ).GT.ABS( PREW ) / TEN ) &
                  SWTCH = .NOT.SWTCH
     240    CONTINUE
   
            INFO = 1
            IF( ORGATI ) THEN
               DLAM = D( I ) + TAU
            ELSE
               DLAM = D( IP1 ) + TAU
            END IF
         END IF
     250 CONTINUE
         RETURN
         END   

SUBROUTINE MPAL_DLAED3( K, N, N1, D, Q, LDQ, RHO, DLAMDA, Q2, INDX, &
                       CTOT, W, S, INFO )
      INTEGER            INFO, K, LDQ, N, N1
      TYPE(MPAL_ST)      RHO

      INTEGER            CTOT( * ), INDX( * )
      TYPE(MPAL_ST)      D( * ), DLAMDA( * ), Q( LDQ, * ), Q2( * ), &
                         S( * ), W( * )

      TYPE(MPAL_ST)      ONE, ZERO

      INTEGER            I, II, IQ2, J, N12, N2, N23
      TYPE(MPAL_ST)      TEMP

      EXTERNAL           XERBLA

      INTRINSIC          MAX, SIGN, SQRT

      ONE = 1.0D0
      ZERO = 0.0D0

      INFO = 0

      IF( K.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.K ) THEN
         INFO = -2
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED3', -INFO )
         RETURN
      END IF

      IF( K.EQ.0 ) RETURN
      DO 10 I = 1, K
         DLAMDA( I ) = (DLAMDA( I ) + DLAMDA( I )) - DLAMDA( I )
   10 CONTINUE
      DO 20 J = 1, K
         CALL MPAL_DLAED4( K, J, DLAMDA, W, Q( 1, J ), RHO, D( J ), INFO )
         IF( INFO.NE.0 ) GO TO 120
   20 CONTINUE

      IF( K.EQ.1 ) GO TO 110
      IF( K.EQ.2 ) THEN
         DO 30 J = 1, K
            W( 1 ) = Q( 1, J )
            W( 2 ) = Q( 2, J )
            II = INDX( 1 )
            Q( 1, J ) = W( II )
            II = INDX( 2 )
            Q( 2, J ) = W( II )
   30    CONTINUE
         GO TO 110
      END IF

      CALL MPAL_DCOPY( K, W, 1, S, 1 )
      CALL MPAL_DCOPY( K, Q, LDQ+1, W, 1 )
      DO 60 J = 1, K
         DO 40 I = 1, J - 1
            W( I ) = W( I )*( Q( I, J ) / ( DLAMDA( I )-DLAMDA( J ) ) )
   40    CONTINUE
         DO 50 I = J + 1, K
            W( I ) = W( I )*( Q( I, J ) / ( DLAMDA( I )-DLAMDA( J ) ) )
   50    CONTINUE
   60 CONTINUE
      DO 70 I = 1, K
         W( I ) = SIGN( SQRT( -W( I ) ), S( I ) )
   70 CONTINUE
      DO 100 J = 1, K
         DO 80 I = 1, K
            S( I ) = W( I ) / Q( I, J )
   80    CONTINUE
         TEMP = MPAL_DNRM2( K, S, 1 )
         DO 90 I = 1, K
            II = INDX( I )
            Q( I, J ) = S( II ) / TEMP
   90    CONTINUE
100 CONTINUE
110 CONTINUE
      N2 = N - N1
      N12 = CTOT( 1 ) + CTOT( 2 )
      N23 = CTOT( 2 ) + CTOT( 3 )
      CALL MPAL_DLACPY( 'A', N23, K, Q( CTOT( 1 )+1, 1 ), LDQ, S, N23 )
      IQ2 = N1*N12 + 1
      IF( N23.NE.0 ) THEN
         CALL MPAL_GEMM( 'N', 'N', N2, K, N23, ONE, Q2( IQ2 ), N2, S, N23, &
                   ZERO, Q( N1+1, 1 ), LDQ )
      ELSE
         CALL MPAL_DLASET( 'A', N2, K, ZERO, ZERO, Q( N1+1, 1 ), LDQ )
      END IF
      CALL MPAL_DLACPY( 'A', N12, K, Q, LDQ, S, N12 )
      IF( N12.NE.0 ) THEN
         CALL MPAL_GEMM( 'N', 'N', N1, K, N12, ONE, Q2, N1, S, N12, ZERO, Q, &
                   LDQ )
      ELSE
         CALL MPAL_DLASET( 'A', N1, K, ZERO, ZERO, Q( 1, 1 ), LDQ )
      END IF
120 CONTINUE
      RETURN
      END

SUBROUTINE MPAL_DLAED2( K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMDA, W, &
                       Q2, INDX, INDXC, INDXP, COLTYP, INFO )

    INTEGER            INFO, K, LDQ, N, N1
    TYPE(MPAL_ST)      RHO

    INTEGER            COLTYP( * ), INDX( * ), INDXC( * ), INDXP( * ), &
                       INDXQ( * )
    TYPE(MPAL_ST)      D( * ), DLAMDA( * ), Q( LDQ, * ), Q2( * ), &
                       W( * ), Z( * )

    TYPE(MPAL_ST)      MONE, ZERO, ONE, TWO, EIGHT

    INTEGER            CTOT( 4 ), PSM( 4 )

    INTEGER            CT, I, IMAX, IQ1, IQ2, J, JMAX, JS, K2, N1P1, &
                       N2, NJ, PJ
    TYPE(MPAL_ST)      C, EPS, S, T, TAU, TOL

    INTEGER            IDAMAX
    EXTERNAL           IDAMAX

    EXTERNAL           XERBLA

    INTRINSIC          ABS, MAX, MIN, SQRT

    MONE = -1.0D0
    ZERO = 0.0D0
    ONE = 1.0D0
    TWO = 2.0D0
    EIGHT = 8.0D0

    INFO = 0

    IF( N.LT.0 ) THEN
       INFO = -2
    ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
       INFO = -6
    ELSE IF( MIN( 1, ( N / 2 ) ).GT.N1 .OR. ( N / 2 ).LT.N1 ) THEN
       INFO = -3
    END IF
    IF( INFO.NE.0 ) THEN
       CALL XERBLA( 'DLAED2', -INFO )
       RETURN
    END IF

    IF( N.EQ.0 ) RETURN

    N2 = N - N1
    N1P1 = N1 + 1

    IF( RHO.LT.ZERO ) THEN
       CALL MPAL_DSCAL( N2, MONE, Z( N1P1 ), 1 )
    END IF

    T = ONE / SQRT( TWO )
    CALL MPAL_DSCAL( N, T, Z, 1 )

    RHO = ABS( TWO*RHO )

    DO 10 I = N1P1, N
       INDXQ( I ) = INDXQ( I ) + N1
 10 CONTINUE

 DO 20 I = 1, N
       DLAMDA( I ) = D( INDXQ( I ) )
 20 CONTINUE
    CALL MPAL_DLAMRG( N1, N2, DLAMDA, 1, 1, INDXC )
    DO 30 I = 1, N
       INDX( I ) = INDXQ( INDXC( I ) )
 30 CONTINUE

    IMAX = IDAMAX( N, Z, 1 )
    JMAX = IDAMAX( N, D, 1 )
    EPS = MPAL_DLAMCH( 'Epsilon' )
    TOL = EIGHT*EPS*MAX( ABS( D( JMAX ) ), ABS( Z( IMAX ) ) )

    IF( RHO*ABS( Z( IMAX ) ).LE.TOL ) THEN
       K = 0
       IQ2 = 1
       DO 40 J = 1, N
          I = INDX( J )
          CALL MPAL_DCOPY( N, Q( 1, I ), 1, Q2( IQ2 ), 1 )
          DLAMDA( J ) = D( I )
          IQ2 = IQ2 + N
 40    CONTINUE
       CALL MPAL_DLACPY( 'A', N, N, Q2, N, Q, LDQ )
       CALL MPAL_DCOPY( N, DLAMDA, 1, D, 1 )
       GO TO 190
    END IF
    DO 50 I = 1, N1
       COLTYP( I ) = 1
 50 CONTINUE
    DO 60 I = N1P1, N
       COLTYP( I ) = 3
 60 CONTINUE

    K = 0
    K2 = N + 1
    DO 70 J = 1, N
       NJ = INDX( J )
       IF( RHO*ABS( Z( NJ ) ).LE.TOL ) THEN
          K2 = K2 - 1
          COLTYP( NJ ) = 4
          INDXP( K2 ) = NJ
          IF( J.EQ.N ) GO TO 100
       ELSE
          PJ = NJ
          GO TO 80
       END IF
 70 CONTINUE
 80 CONTINUE
    J = J + 1
    NJ = INDX( J )
    IF( J.GT.N ) GO TO 100
    IF( RHO*ABS( Z( NJ ) ).LE.TOL ) THEN
       K2 = K2 - 1
       COLTYP( NJ ) = 4
       INDXP( K2 ) = NJ
    ELSE
       S = Z( PJ )
       C = Z( NJ )
       TAU = MPAL_DLAPY2( C, S )
       T = D( NJ ) - D( PJ )
       C = C / TAU
       S = -S / TAU
       IF( ABS( T*C*S ).LE.TOL ) THEN
          Z( NJ ) = TAU
          Z( PJ ) = ZERO
          IF( COLTYP( NJ ).NE.COLTYP( PJ ) ) COLTYP( NJ ) = 2
          COLTYP( PJ ) = 4
          CALL MPAL_DROT( N, Q( 1, PJ ), 1, Q( 1, NJ ), 1, C, S )
          T = D( PJ )*C**2 + D( NJ )*S**2
          D( NJ ) = D( PJ )*S**2 + D( NJ )*C**2
          D( PJ ) = T
          K2 = K2 - 1
          I = 1
 90       CONTINUE
          IF( K2+I.LE.N ) THEN
             IF( D( PJ ).LT.D( INDXP( K2+I ) ) ) THEN
                INDXP( K2+I-1 ) = INDXP( K2+I )
                INDXP( K2+I ) = PJ
                I = I + 1
                GO TO 90
             ELSE
                INDXP( K2+I-1 ) = PJ
             END IF
          ELSE
             INDXP( K2+I-1 ) = PJ
          END IF
          PJ = NJ
       ELSE
          K = K + 1
          DLAMDA( K ) = D( PJ )
          W( K ) = Z( PJ )
          INDXP( K ) = PJ
          PJ = NJ
       END IF
    END IF
    GO TO 80
100 CONTINUE
    K = K + 1
    DLAMDA( K ) = D( PJ )
    W( K ) = Z( PJ )
    INDXP( K ) = PJ
    DO 110 J = 1, 4
       CTOT( J ) = 0
110 CONTINUE
    DO 120 J = 1, N
       CT = COLTYP( J )
       CTOT( CT ) = CTOT( CT ) + 1
120 CONTINUE
    PSM( 1 ) = 1
    PSM( 2 ) = 1 + CTOT( 1 )
    PSM( 3 ) = PSM( 2 ) + CTOT( 2 )
    PSM( 4 ) = PSM( 3 ) + CTOT( 3 )
    K = N - CTOT( 4 )
    DO 130 J = 1, N
       JS = INDXP( J )
       CT = COLTYP( JS )
       INDX( PSM( CT ) ) = JS
       INDXC( PSM( CT ) ) = J
       PSM( CT ) = PSM( CT ) + 1
130 CONTINUE
    I = 1
    IQ1 = 1
    IQ2 = 1 + ( CTOT( 1 )+CTOT( 2 ) )*N1
    DO 140 J = 1, CTOT( 1 )
       JS = INDX( I )
       CALL MPAL_DCOPY( N1, Q( 1, JS ), 1, Q2( IQ1 ), 1 )
       Z( I ) = D( JS )
       I = I + 1
       IQ1 = IQ1 + N1
140 CONTINUE
    DO 150 J = 1, CTOT( 2 )
       JS = INDX( I )
       CALL MPAL_DCOPY( N1, Q( 1, JS ), 1, Q2( IQ1 ), 1 )
       CALL MPAL_DCOPY( N2, Q( N1+1, JS ), 1, Q2( IQ2 ), 1 )
       Z( I ) = D( JS )
       I = I + 1
       IQ1 = IQ1 + N1
       IQ2 = IQ2 + N2
150 CONTINUE
    DO 160 J = 1, CTOT( 3 )
       JS = INDX( I )
       CALL MPAL_DCOPY( N2, Q( N1+1, JS ), 1, Q2( IQ2 ), 1 )
       Z( I ) = D( JS )
       I = I + 1
       IQ2 = IQ2 + N2
160 CONTINUE
    IQ1 = IQ2
    DO 170 J = 1, CTOT( 4 )
       JS = INDX( I )
       CALL MPAL_DCOPY( N, Q( 1, JS ), 1, Q2( IQ2 ), 1 )
       IQ2 = IQ2 + N
       Z( I ) = D( JS )
       I = I + 1
170 CONTINUE
    IF( K.LT.N ) THEN
       CALL MPAL_DLACPY( 'A', N, CTOT( 4 ), Q2( IQ1 ), N, &
                    Q( 1, K+1 ), LDQ )
       CALL MPAL_DCOPY( N-K, Z( K+1 ), 1, D( K+1 ), 1 )
    END IF
    DO 180 J = 1, 4
       COLTYP( J ) = CTOT( J )
180 CONTINUE

190 CONTINUE
    RETURN
    END


SUBROUTINE MPAL_DLAED1( N, D, Q, LDQ, INDXQ, RHO, CUTPNT, WORK, IWORK, &
                   INFO )

      INTEGER            CUTPNT, INFO, LDQ, N
      TYPE(MPAL_ST)      RHO

      INTEGER            INDXQ( * ), IWORK( * )
      TYPE(MPAL_ST)      D( * ), Q( LDQ, * ), WORK( * )

      INTEGER            COLTYP, I, IDLMDA, INDX, INDXC, INDXP, IQ2, IS, &
                         IW, IZ, K, N1, N2, ZPP1

      EXTERNAL           DLAMRG, XERBLA

      INTRINSIC          MAX, MIN
      INFO = 0

      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( MIN( 1, N / 2 ).GT.CUTPNT .OR. ( N / 2 ).LT.CUTPNT ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED1', -INFO )
         RETURN
      END IF

      IF( N.EQ.0 ) RETURN

      IZ = 1
      IDLMDA = IZ + N
      IW = IDLMDA + N
      IQ2 = IW + N

      INDX = 1
      INDXC = INDX + N
      COLTYP = INDXC + N
      INDXP = COLTYP + N

      CALL MPAL_DCOPY( CUTPNT, Q( CUTPNT, 1 ), LDQ, WORK( IZ ), 1 )
      ZPP1 = CUTPNT + 1
      CALL MPAL_DCOPY( N-CUTPNT, Q( ZPP1, ZPP1 ), LDQ, WORK( IZ+CUTPNT ), 1 )

      CALL MPAL_DLAED2( K, N, CUTPNT, D, Q, LDQ, INDXQ, RHO, WORK( IZ ), &
                   WORK( IDLMDA ), WORK( IW ), WORK( IQ2 ), &
                   IWORK( INDX ), IWORK( INDXC ), IWORK( INDXP ), &
                   IWORK( COLTYP ), INFO )

      IF( INFO.NE.0 ) &
         GO TO 20

      IF( K.NE.0 ) THEN
         IS = ( IWORK( COLTYP )+IWORK( COLTYP+1 ) )*CUTPNT + &
              ( IWORK( COLTYP+1 )+IWORK( COLTYP+2 ) )*( N-CUTPNT ) + IQ2
         CALL MPAL_DLAED3( K, N, CUTPNT, D, Q, LDQ, RHO, WORK( IDLMDA ), &
                      WORK( IQ2 ), IWORK( INDXC ), IWORK( COLTYP ), &
                      WORK( IW ), WORK( IS ), INFO )
         IF( INFO.NE.0 ) GO TO 20
         N1 = K
         N2 = N - K
         CALL MPAL_DLAMRG( N1, N2, D, 1, -1, INDXQ )
      ELSE
         DO 10 I = 1, N
            INDXQ( I ) = I
   10    CONTINUE
      END IF

   20 CONTINUE
      RETURN
      END

SUBROUTINE MPAL_DLAED0( ICOMPQ, QSIZ, N, D, E, Q, LDQ, QSTORE, LDQS, &
     WORK, IWORK, INFO )

     INTEGER            ICOMPQ, INFO, LDQ, LDQS, N, QSIZ

     INTEGER            IWORK( * )
     TYPE(MPAL_ST)      D( * ), E( * ), Q( LDQ, * ), QSTORE( LDQS, * ), &
                        WORK( * )

     TYPE(MPAL_ST)      ZERO, ONE, TWO

     INTEGER            CURLVL, CURPRB, CURR, I, IGIVCL, IGIVNM, &
                        IGIVPT, INDXQ, IPERM, IPRMPT, IQ, IQPTR, IWREM, &
                        J, K, LGN, MATSIZ, MSD2, SMLSIZ, SMM1, SPM1, &
                        SPM2, SUBMAT, SUBPBS, TLVLS
     TYPE(MPAL_ST)      TEMP

     INTEGER            ILAENV
     EXTERNAL           ILAENV
     INTRINSIC          ABS, DBLE, INT, LOG, MAX

     ZERO = 0.D0
     ONE = 1.D0
     TWO = 2.D0

     INFO = 0
     IF( ICOMPQ.LT.0 .OR. ICOMPQ.GT.2 ) THEN
        INFO = -1
     ELSE IF( ( ICOMPQ.EQ.1 ) .AND. ( QSIZ.LT.MAX( 0, N ) ) ) THEN
        INFO = -2
     ELSE IF( N.LT.0 ) THEN
        INFO = -3
     ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
        INFO = -7
     ELSE IF( LDQS.LT.MAX( 1, N ) ) THEN
        INFO = -9
     END IF
     IF( INFO.NE.0 ) THEN
        CALL XERBLA( 'DLAED0', -INFO )
        RETURN
     END IF

     IF( N.EQ.0 ) RETURN

     SMLSIZ = ILAENV( 9, 'DLAED0', ' ', 0, 0, 0, 0 )

     IWORK( 1 ) = N
     SUBPBS = 1
     TLVLS = 0
  10 CONTINUE
     IF( IWORK( SUBPBS ).GT.SMLSIZ ) THEN
        DO 20 J = SUBPBS, 1, -1
           IWORK( 2*J ) = ( IWORK( J )+1 ) / 2
           IWORK( 2*J-1 ) = IWORK( J ) / 2
  20    CONTINUE
        TLVLS = TLVLS + 1
        SUBPBS = 2*SUBPBS
        GO TO 10
     END IF
     DO 30 J = 2, SUBPBS
        IWORK( J ) = IWORK( J ) + IWORK( J-1 )
  30 CONTINUE
! *
! *     Divide the matrix into SUBPBS submatrices of size at most SMLSIZ+1
! *     using rank-1 modifications (cuts).
! *
     SPM1 = SUBPBS - 1
     DO 40 I = 1, SPM1
        SUBMAT = IWORK( I ) + 1
        SMM1 = SUBMAT - 1
        D( SMM1 ) = D( SMM1 ) - ABS( E( SMM1 ) )
        D( SUBMAT ) = D( SUBMAT ) - ABS( E( SMM1 ) )
  40 CONTINUE

     INDXQ = 4*N + 3
     IF( ICOMPQ.NE.2 ) THEN
! *
! *        Set up workspaces for eigenvalues only/accumulate new vectors
! *        routine
! *
        TEMP = LOG( DBLE( N ) ) / LOG( TWO )
        LGN = MPAL_VAL(INT( TEMP ))
        IF( 2**LGN.LT.N ) LGN = LGN + 1
        IF( 2**LGN.LT.N ) LGN = LGN + 1
        IPRMPT = INDXQ + N + 1
        IPERM = IPRMPT + N*LGN
        IQPTR = IPERM + N*LGN
        IGIVPT = IQPTR + N + 2
        IGIVCL = IGIVPT + N*LGN

        IGIVNM = 1
        IQ = IGIVNM + 2*N*LGN
        IWREM = IQ + N**2 + 1
! *
! *        Initialize pointers
! *
        DO 50 I = 0, SUBPBS
           IWORK( IPRMPT+I ) = 1
           IWORK( IGIVPT+I ) = 1
  50    CONTINUE
        IWORK( IQPTR ) = 1
     END IF
! *
! *     Solve each submatrix eigenproblem at the bottom of the divide and
! *     conquer tree.
! *
     CURR = 0
     DO 70 I = 0, SPM1
        IF( I.EQ.0 ) THEN
           SUBMAT = 1
           MATSIZ = IWORK( 1 )
        ELSE
           SUBMAT = IWORK( I ) + 1
           MATSIZ = IWORK( I+1 ) - IWORK( I )
        END IF
        IF( ICOMPQ.EQ.2 ) THEN
           CALL MPAL_DSTEQR( 'I', MATSIZ, D( SUBMAT ), E( SUBMAT ), &
                        Q( SUBMAT, SUBMAT ), LDQ, WORK, INFO )
           IF( INFO.NE.0 ) GO TO 130
        ELSE
           CALL MPAL_DSTEQR( 'I', MATSIZ, D( SUBMAT ), E( SUBMAT ), &
                       WORK( IQ-1+IWORK( IQPTR+CURR ) ), MATSIZ, WORK, &
                       INFO )
           IF( INFO.NE.0 ) GO TO 130
           IF( ICOMPQ.EQ.1 ) THEN
            CALL MPAL_GEMM( 'N', 'N', QSIZ, MATSIZ, MATSIZ, ONE, &
                         Q( 1, SUBMAT ), LDQ, WORK( IQ-1+IWORK( IQPTR+ &
                         CURR ) ), MATSIZ, ZERO, QSTORE( 1, SUBMAT ), &
                         LDQS )
           END IF
           IWORK( IQPTR+CURR+1 ) = IWORK( IQPTR+CURR ) + MATSIZ**2
           CURR = CURR + 1
        END IF
        K = 1
        DO 60 J = SUBMAT, IWORK( I+1 )
           IWORK( INDXQ+J ) = K
           K = K + 1
  60    CONTINUE
  70 CONTINUE
! *
! *     Successively merge eigensystems of adjacent submatrices
! *     into eigensystem for the corresponding larger matrix.
! *
! *     while ( SUBPBS > 1 )
! *
     CURLVL = 1
  80 CONTINUE
     IF( SUBPBS.GT.1 ) THEN
        SPM2 = SUBPBS - 2
        DO 90 I = 0, SPM2, 2
           IF( I.EQ.0 ) THEN
              SUBMAT = 1
              MATSIZ = IWORK( 2 )
              MSD2 = IWORK( 1 )
              CURPRB = 0
           ELSE
              SUBMAT = IWORK( I ) + 1
              MATSIZ = IWORK( I+2 ) - IWORK( I )
              MSD2 = MATSIZ / 2
              CURPRB = CURPRB + 1
           END IF
! *
! *     Merge lower order eigensystems (of size MSD2 and MATSIZ - MSD2)
! *     into an eigensystem of size MATSIZ.
! *     DLAED1 is used only for the full eigensystem of a tridiagonal
! *     matrix.
! *     DLAED7 handles the cases in which eigenvalues only or eigenvalues
! *     and eigenvectors of a full symmetric matrix (which was reduced to
! *     tridiagonal form) are desired.
! *
           IF( ICOMPQ.EQ.2 ) THEN
              CALL MPAL_DLAED1( MATSIZ, D( SUBMAT ), Q( SUBMAT, SUBMAT ), &
                          LDQ, IWORK( INDXQ+SUBMAT ), &
                          E( SUBMAT+MSD2-1 ), MSD2, WORK, &
                          IWORK( SUBPBS+1 ), INFO )
           ELSE
              WRITE(*, *) "Not implemented!"
           END IF
           IF( INFO.NE.0 ) &
              GO TO 130
           IWORK( I / 2+1 ) = IWORK( I+2 )
  90    CONTINUE
        SUBPBS = SUBPBS / 2
        CURLVL = CURLVL + 1
        GO TO 80
     END IF
! *
! *     end while
! *
! *     Re-merge the eigenvalues/vectors which were deflated at the final
! *     merge step.
! *
     IF( ICOMPQ.EQ.1 ) THEN
        DO 100 I = 1, N
           J = IWORK( INDXQ+I )
           WORK( I ) = D( J )
           CALL MPAL_DCOPY( QSIZ, QSTORE( 1, J ), 1, Q( 1, I ), 1 )
 100    CONTINUE
        CALL MPAL_DCOPY( N, WORK, 1, D, 1 )
     ELSE IF( ICOMPQ.EQ.2 ) THEN
        DO 110 I = 1, N
           J = IWORK( INDXQ+I )
           WORK( I ) = D( J )
           CALL MPAL_DCOPY( N, Q( 1, J ), 1, WORK( N*I+1 ), 1 )
 110    CONTINUE
        CALL MPAL_DCOPY( N, WORK, 1, D, 1 )
        CALL MPAL_DLACPY( 'A', N, N, WORK( N+1 ), N, Q, LDQ )
     ELSE
        DO 120 I = 1, N
           J = IWORK( INDXQ+I )
           WORK( I ) = D( J )
 120    CONTINUE
        CALL MPAL_DCOPY( N, WORK, 1, D, 1 )
     END IF
     GO TO 140

 130 CONTINUE
     INFO = SUBMAT*( N+1 ) + SUBMAT + MATSIZ - 1

 140 CONTINUE
     RETURN

     END
