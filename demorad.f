C
C   PROGRAM DEMORAD (demo for subroutine package RADIAL)
C
C     This program solves the radial wave equations for modified Coulomb
C  potentials of the following forms:
C   1: Coulomb + spherical step,     V(R)=Z/R+(V0 if R<A)
C   2: Coulomb + exponential,        V(R)=Z/R+V0*EXP(-A*R)
C   3: Coulomb + Gaussian,           V(R)=Z/R+V0*EXP(-A*R**2)
C   4: Partially-screened Coulomb,   V(R)=Z/R+V0*EXP(-A*R)/R
C
      INCLUDE 'radial.f'  ! File included to simplify compilation.
C
C  NB: RADIAL must be compiled first to make global parameters available
C      to other subprograms.
C
C  *********************************************************************
C                       MAIN PROGRAM
C  *********************************************************************
C
C     Solves the radial wave equations for the analytical potentials
C  defined by function VPOTR.
C
      USE CONSTANTS
      
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
      character(len=50) :: output_filename, temp
      character(len=50) :: temp1
      CHARACTER FILEN*16, POTN*40
      PARAMETER (PI=3.1415926535897932D0)
      DIMENSION DR0(NDIM) ! Output from SGRID.
C  ****  Potential.
      COMMON/CVPOT/Z,V0,A,IPOT
      DIMENSION R0(NDIM),RV0(NDIM)
C  ****  Output radial functions.
      COMMON/RADWF/RAD(NDIM),P(NDIM),Q(NDIM),NGP,ILAST,IER
C  ****  Coulomb wave function parameters.
      COMMON/OCOUL/WAVNUM,ETA,DELTA
C
      EXTERNAL VPOTR
C
C  ****  Read potential parameters.
C
 10   CONTINUE
      output_filename = 'output.txt'
      temp='///'
      call appendd(temp)
      temp='///temp///'
      call appendd(temp)
      write(6,*) ' version: personal ' 
      Z=0.0D0
      V0=0.0D0
      A=0.0D0

      WRITE(6,*) ' Analytical potentials.'
      WRITE(6,*)
     1  '   1: Coulomb + spherical step,    V(R)=Z/R+C4/R**4'
      WRITE(6,*)
     1  '   2: Coulomb + exponential,       V(R)=Z/R+V0*EXP(-A*R)'
      WRITE(6,*)
     1  '   3: Coulomb + Gaussian,          V(R)=Z/R+V0*EXP(-A*R**2)'
      WRITE(6,*)
     1  '   4: Partially-screened Coulomb,  V(R)=Z/R+V0*EXP(-A*R)/R'
      WRITE(6,*) ' Select one option ...'
      READ(5,*) IPOT
C
 11   CONTINUE
      WRITE(6,*) ' Enter Z, V0 and A ...'
      READ(5,*) Z,V0,A
      write(6,*) 'Z received: ', Z
      write(6,*) 'V0 received: ', V0
      write(6,*) 'A received: ', A
      IF(A.LT.1.0D-6) THEN
        WRITE(6,*) ' A must be larger than 1.0D-6.'
        GO TO 11
      ENDIF
      write(temp1, '(F22.15)') Z
      temp1 = 'Z=' // temp1
      call appendd(temp1)
      write(temp1, '(F22.15)') V0
      temp1 = 'V0=' // temp1
      call appendd(temp1)
      write(temp1, '(F22.15)') A
      temp1 = 'A=' // temp1
      call appendd(temp1)
C
      IF(IPOT.EQ.1) THEN
        POTN='Potential: V(R)=Z/R+(V0/R**6 if R<A)*exp'
        HEDPS=-V0*A/SL
      ELSE IF(IPOT.EQ.2) THEN
        POTN='Potential: V(R)=Z/R+(V0/R**6 if R<A)*exp'
        HEDPS=-V0/(A*SL)
      ELSE IF(IPOT.EQ.3) THEN
        POTN='Potential: V(R)=Z/R+(V0/R**6 if R<A)'
        HEDPS=-V0*0.5D0*SQRT(PI/A)/SL
      ELSE IF(IPOT.EQ.4) THEN
        POTN='Potential: V(R)=Z/R+V0*EXP(-A*R)/R'
      ELSE IF(IPOT.EQ.6) THEN
        POTN='Potential: V(R)=Z/R+V0*(C6/R**6)'
      ELSE
        IF(IPOT.LT.0) STOP ! :-(
        GO TO 10
      ENDIF
      
      write(temp1, '(I3)') IPOT
      temp1 = 'IPOT=' // temp1
      call appendd(temp1)
C
C  ****  Set the potential table.
      NV=3200
      ROUT=1.0D8
      TOL=1.0D-15
      DO I = 1, 3000
         R0(I) = 1.0D-8 * (1.0D16)**((I-1.0D0) / 29.99D2)
      END DO
      ! recomend to compare with halogen structure
      ! to test the program
      NFIX=200
      !  NFIX=0
      NU=100
      IF(IPOT.EQ.1) THEN  ! Include the discontinuity.
        NFIX=NFIX+1
        R0(NFIX)=A
        NFIX=NFIX+1
        R0(NFIX)=A
      ENDIF
      CALL SPLSET(VPOTR,0.0D0,ROUT,R0,RV0,TOL,ERR,NDIM,NFIX,NU,NV)
      WRITE(6,'(/A,I5)') ' # Potential grid. Number of radii =',NV
      ! WRITE(temp1,'(/A,I5)') ' # Potential grid. Number of radii =',NV

      ! call appendd(temp1)
      WRITE(6,'(A,1P,E9.1)') ' # Spline interpolation error =',ERR
C
C  ****  Spline interpolation of the potential.
      CALL VINT(R0,RV0,NV)
      RV0(1)=-Z*1.0D0
      RANGE=VRANGE()
      WRITE(6,'(A,1P,E9.1)') ' # Range of the potential =',RANGE

      PRINT *, "NV =", NV
C
C
C -\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-\/-
C  ****  Print tables of the potential function and its interpolating
C        spline.
C
      write(6, '(A)') 'begin record to potential.dat'
      OPEN(8,FILE='potential.dat')
      WRITE(8,'(A)') '# Potential function.'
      WRITE(8,'(''#'',2X,''r'',13X,''r*V(r)'')')
      DO I=1,NV
        WRITE(8,'(1X,1P,2E14.6,E10.1)') R0(I),RV0(I)
      ENDDO
      CLOSE(8)
C
      OPEN(8,FILE='pot-spline.dat')
      WRITE(8,'(A)') '# Interpolated potential function.'
      WRITE(8,'(''#'',2X,''r'',13X,''r*V(r)'')')
      NINT=4
      DO I=1,NV
        IF(I.LT.NV) THEN
          DR=(R0(I+1)-R0(I))/DBLE(NINT)
        ELSE
          DR=(R0(NV)-R0(NV-1))/DBLE(NINT)
        ENDIF
        IF(DR.GT.1.0D-12*ABS(R0(I))) THEN
          DO J=1,NINT
            RINT=R0(I)+(J-1)*DR
            IF(J.EQ.1.AND.I.GT.1) THEN
              IF(R0(I)-R0(I-1).LT.1.0D-12*ABS(R0(I)))
     1          RINT=R0(I)*(1.0D0+1.0D-12)
            ENDIF
            RVS=RVSPL(RINT)
            WRITE(8,'(1X,1P,2E14.6)') RINT,RVS
          ENDDO
        ELSE
          RINT=R0(I)*(1.0D0-1.0D-12)
          RVS=RVSPL(RINT)
          WRITE(8,'(1X,1P,2E14.6,''  #'')') RINT,RVS
        ENDIF
      ENDDO
      CLOSE(8)
C -/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-/\-
C
C
      OPEN(8,FILE='res.dat')
      WRITE(8,'(/A,I5)') ' # Potential grid. Number of radii =',NV
      WRITE(8,'(A,1P,E9.1)') ' # Spline interpolation error =',ERR
      WRITE(8,'(A,1P,E9.1)') ' # Range of the potential =',RANGE
C
C  ****  High-energy limit of the Dirac inner phase shift.
C
      IF(IPOT.NE.4) THEN
        WRITE(6,'(/'' # Deltainfty = '',1P,E22.15,''  (analytical)'')')
     1     HEDPS
        CALL DELINF(HEDPS)
        WRITE(6,'('' # Delta_infty = '',1P,E22.15)') HEDPS
      ENDIF
C
C  ****  Pre-defined user grid.
C
      WRITE(6,1001)
 1001 FORMAT(/2X,'The user grid can be read from a file (single',
     1  ' column, increasing radii)'/2X,'or determined automati',
     2  'cally. If you wish to use a prepared grid, enter'/2X,
     1  'the file name, otherwise type ''n'' ...')
      READ(5,'(A12)') FILEN
      IF(FILEN.NE.'N'.AND.FILEN.NE.'n') THEN
        IGRID=1
        OPEN(7,FILE=FILEN)
        DO I=1,NDIM
          READ(7,*,END=20) RAD(I)
          NGP=I
        ENDDO
        CLOSE(7)
      ELSE
        IGRID=0
      ENDIF
C
C  ************  Calculation of radial functions.
C
 20   CONTINUE
      WRITE(6,*) '  '
      WRITE(6,*) ' Select one option ...'
      WRITE(6,*) '   1: Schrodinger bound state,   2: Sch',
     1  'rodinger free state,'
      WRITE(6,*) '   3: Dirac bound state,         4: Dir',
     1  'ac free state.'
      IF(Z.LT.-0.5D0)
     1  WRITE(6,*) '   5: Quantum defect.'
      READ(5,*,ERR=20) IOPT
      IF(IOPT.LT.0.OR.IOPT.GT.5) STOP
      IF(IOPT.EQ.5.AND.Z.GT.-0.5D0) THEN
        WRITE(6,'(/2X,'' Quantum defects are not defined for attrac'',
     1    ''tive potentials,'',/2X,''Press any key to continue...'')')
        READ(5,*)
        GO TO 20
      ENDIF
      WRITE(6,*) '  '
      WRITE(8,*) '  '
C
C  ****  Schrodinger equation. Bound state.
C
      IF(IOPT.EQ.1) THEN

        WRITE(6,*) ' Enter N, L and EPS ...'
        READ(5,*,ERR=20) N,L,EPS
        WRITE(6,*) ' Value of N L and EPS received. '
        EPS=MAX(EPS,1.0D-15)
        IF(N.LT.1) THEN
          WRITE(6,'(A,I5)') '  N =',N
          WRITE(6,'(A)') '  N must be >0.'
          WRITE(6,'(/2X,''Press any key to continue...'')')
          READ(5,*)
          GO TO 20
        ENDIF
        IF(L.LT.0.OR.L.GE.N) THEN
          WRITE(6,'(A,2I5)') '  L,N =',L,N
          WRITE(6,'(A)') '  L must be >0 and <N.'
          WRITE(6,'(/2X,''Press any key to continue...'')')
          READ(5,*)
          GO TO 20
        ENDIF
C
        IF(IGRID.EQ.0) THEN
          NGP=4000
          RN=3500.0D0
          write(6,*) ' Calculation of SGRID begin ... '
          CALL SGRID(RAD(NV-1),DR0,RN,1.0D-6,1.0D0,NGP,NDIM,IERS)
          write(6,*) ' Calculation of SGRID end. '
          IF(IERS.NE.0) STOP 'Error in the grid definition (SB).'
        ENDIF
        E=-Z**2/(2.0D0*N*N)
        write(6,*) 'E = ', E
        write(6,*) ' Calculation of SBOUND begin ... '
        IF(RAD(1).EQ.0) THEN
          RAD(1)=1.0D-6
        ENDIF
        CALL SBOUND(E,EPS,N,L)
        write(6,*) ' Calculation of SBOUND end. '
        IF(IER.NE.0) GO TO 20
C
        WRITE(6,1101) POTN,Z,V0,A,N,L,EPS,E
        WRITE(8,1101) POTN,Z,V0,A,N,L,EPS,E
 1101   FORMAT(1X,1P,'# **** Schrodinger Eq. ',A,
     1    /' #',6X,'Z=',E13.6,', V0=',E13.6,', A=',E13.6,
     2    /' #',6X,'Bound state: N=',I4,', L=',I4,'  (EPS=',E8.1,')',
     3    /' #',6X,'Binding energy=',E22.15)
        FILEN='schrodinger.dat'
        write(temp, '(F22.15)') E
	call appendd(temp)
C
C  ****  Schrodinger equation. Free state.
C
      ELSE IF(IOPT.EQ.2) THEN
        WRITE(6,*) ' Enter E, L and EPS ...'
        READ(5,*,ERR=20) E,L,EPS
        EPS=MAX(EPS,1.0D-15)
        IF(E.LT.0.0D0) THEN
          WRITE(6,'(A,1P,E14.6)') '  E =',E
          WRITE(6,'(A)') '  The energy must be positive.'
          WRITE(6,'(/2X,''Press any key to continue...'')')
          READ(5,*)
          GO TO 20
        ENDIF
        IF(L.LT.0) THEN
          WRITE(6,'(A,I5)') '  L =',L
          WRITE(6,'(A)') '  L must be a non-negative integer.'
          WRITE(6,'(/2X,''Press any key to continue...'')')
          READ(5,*)
          GO TO 20
        ENDIF
C
        IF(IGRID.EQ.0) THEN
          NGP=2000
          WAVEL=2.0D0*PI/SQRT(E+E)
          DRN=WAVEL/40.0D0
          RN=DRN*DBLE(NGP-300)
          CALL SGRID(RAD,DR0,RN,1.0D-6,DRN,NGP,NDIM,IERS)
          IF(IERS.NE.0) STOP 'Error in the grid definition (SF).'
        ENDIF
        CALL SFREE(E,EPS,PHASE,L,1)
        IF(IER.NE.0) THEN
          WRITE(6,'(A,I3)') 'Error in SFREE. IER =',IER
          GO TO 20
        ENDIF
C
        WRITE(6,1201) POTN,Z,V0,A,E,L,EPS,PHASE,DELTA,ETA
        WRITE(8,1201) POTN,Z,V0,A,E,L,EPS,PHASE,DELTA,ETA
 1201   FORMAT(1X,1P,'# **** Schrodinger Eq. ',A,
     1    /' #',6X,'Z=',E13.6,', V0=',E13.6,', A=',E13.6,
     2    /' #',6X,'Free state: E=',E13.6,', L=',I4,'  (EPS=',E8.1,')',
     3    /' #',6X,'  Inner phase shift=',E22.15,
     4    /' #',6X,'Coulomb phase shift=',E22.15,'  (ETA=',E13.6,')')
        WRITE(6,1202) ILAST,RAD(ILAST)
 1202   FORMAT(' #',6X,'Matching radius: RAD(',I5,')=',1P,E22.15)
        FILEN='schrodinger.dat'
C
C  ****  Dirac equation. Bound state.
C
      ELSE IF(IOPT.EQ.3) THEN
        WRITE(6,*) ' Enter N, K and EPS ...'
        READ(5,*,ERR=20) N,K,EPS
        EPS=MAX(EPS,1.0D-15)
        IF(N.LT.1) THEN
          WRITE(6,'(A,I5)') '  N =',N
          WRITE(6,'(A)') '  N must be >0.'
          WRITE(6,'(/2X,''Press any key to continue...'')')
          READ(5,*)
          GO TO 20
        ENDIF
        IF(K.EQ.0.OR.K.GE.N.OR.K.LT.-N) THEN
          WRITE(6,'(A,2I5)') '  K,N =',K,N
          WRITE(6,'(A)') '  K must be between -N and N-1 and .NE.0'
          WRITE(6,'(/2X,''Press any key to continue...'')')
          READ(5,*)
          GO TO 20
        ENDIF
C
        IF(IGRID.EQ.0) THEN
          NGP=4000
          RN=3500.0D0
          CALL SGRID(RAD,DR0,RN,1.0D-6,1.0D0,NGP,NDIM,IERS)
          IF(IERS.NE.0) STOP 'Error in the grid definition (DB).'
        ENDIF
        E=-Z**2/(2.0D0*N*N)
        CALL DBOUND(E,EPS,N,K)
        IF(IER.NE.0) GO TO 20
C
        WRITE(6,1301) POTN,Z,V0,A,N,K,EPS,E
        WRITE(8,1301) POTN,Z,V0,A,N,K,EPS,E
 1301   FORMAT(1X,1P,'# **** Dirac equation. ',A,
     1    /' #',6X,'Z=',E13.6,', V0=',E13.6,', A=',E13.6,
     2    /' #',6X,'Bound state: N=',I4,', K=',I4,'  (EPS=',E8.1,')',
     3    /' #',6X,'Binding energy=',E22.15)
        FILEN='dirac.dat'
C
C  ****  Dirac equation. Free state.
C
      ELSE IF(IOPT.EQ.4) THEN
        WRITE(6,*) ' Enter E, K and EPS ...'
        READ(5,*,ERR=20) E,K,EPS
        EPS=MAX(EPS,1.0D-15)
        IF(E.LT.0.0D0) THEN
          WRITE(6,'(A,1P,E14.6)') '  E =',E
          WRITE(6,'(A)') '  The energy must be positive.'
          WRITE(6,'(/2X,''Press any key to continue...'')')
          READ(5,*)
          GO TO 20
        ENDIF
        IF(K.EQ.0) THEN
          WRITE(6,'(A,I5)') '  K =',K
          WRITE(6,'(A)') '  K must be an integer different from 0.'
          WRITE(6,'(/2X,''Press any key to continue...'')')
          READ(5,*)
          GO TO 20
        ENDIF
C
        IF(K.LT.0) THEN
          L=-K-1
        ELSE
          L=K
        ENDIF
        IF(IGRID.EQ.0) THEN
          NGP=2000
          WAVEL=2.0D0*PI/SQRT(E*(2.0D0+E/SL**2))
          DRN=WAVEL/40.0D0
          RN=DRN*DBLE(NGP-300)
          CALL SGRID(RAD,DR0,RN,1.0D-6,DRN,NGP,NDIM,IERS)
          IF(IERS.NE.0) STOP 'Error in the grid definition (DF).'
        ENDIF
        CALL DFREE(E,EPS,PHASE,K,1)
        IF(IER.NE.0) THEN
          WRITE(6,'(A,I3)') 'Error in DFREE. IER =',IER
          GO TO 20
        ENDIF
C
        WRITE(6,1401) POTN,Z,V0,A,E,K,EPS,PHASE,DELTA,ETA
        WRITE(8,1401) POTN,Z,V0,A,E,K,EPS,PHASE,DELTA,ETA
 1401   FORMAT(1X,1P,'# **** Dirac equation. ',A,
     1    /' #',6X,'Z=',E13.6,', V0=',E13.6,', A=',E13.6,
     2    /' #',6X,'Free state: E=',E13.6,', K=',I4,'  (EPS=',E8.1,')',
     3    /' #',6X,'  Inner phase shift=',E22.15,
     4    /' #',6X,'Coulomb phase shift=',E22.15,'  (ETA=',E13.6,')')
        WRITE(6,1202) ILAST,RAD(ILAST)
        FILEN='dirac.dat'
C
C  ************  Quantum-defect function (Dirac).
C
      ELSE IF(IOPT.EQ.5) THEN
        IF(Z.GT.-0.5D0) THEN
          WRITE(6,*) ' Quantum defects are defined only for attractive'
          WRITE(6,*) ' Coulomb fields (Z < -0.5).'
          GO TO 20
        ENDIF
        WRITE(6,*) ' Enter K, EPS ...'
        READ(5,*,ERR=20) K,EPS
        EPS=MAX(EPS,1.0D-15)
        IF(K.EQ.0) THEN
          WRITE(6,'(A,I5)') ' K =',K
          WRITE(6,'(A)') ' K must be an integer different from 0.'
          GO TO 20
        ENDIF
        CALL QNTDEF(K,QD0,QDA,QDB,EPS,ERRM)
        WRITE(6,'('' #'',4X,''QD0 = '',1P,E13.6)') QD0
        WRITE(6,'('' #'',4X,''  A = '',1P,E13.6)') QDA
        WRITE(6,'('' #'',4X,''  B = '',1P,E13.6)') QDB
        WRITE(6,'(/'' #    Largest error (%) ='',1P,E13.6)') ERRM
        WRITE(6,*) ' '
        GO TO 20
      ELSE
        GO TO 10
      ENDIF
C
C  ****  Radial wave functions printed in output file.
C
*     READ(5,'(A)') FILEN  ! Uncomment to change the output filename.
      OPEN(10,FILE=FILEN)
 1501 FORMAT(1X,'# Radial wave functions calculated by RADIAL.')
      WRITE(10,1501)
      IF(IOPT.EQ.1) THEN
        WRITE(10,1101) POTN,Z,V0,A,N,L,EPS,E
      ELSE IF(IOPT.EQ.2) THEN
        WRITE(10,1201) POTN,Z,V0,A,E,L,EPS,PHASE,DELTA,ETA
        WRITE(10,1202) ILAST,RAD(ILAST)
      ELSE IF(IOPT.EQ.3) THEN
        WRITE(10,1301) POTN,Z,V0,A,N,K,EPS,E
      ELSE IF(IOPT.EQ.4) THEN
        WRITE(10,1401) POTN,Z,V0,A,E,K,EPS,PHASE,DELTA,ETA
        WRITE(10,1202) ILAST,RAD(ILAST)
      ENDIF
C
      NTAB=NGP
      DO I=NGP,1,-1
        IF(ABS(P(I)).GT.1.0D-35) THEN
          NTAB=I
          GO TO 30
        ENDIF
      ENDDO
 30   CONTINUE
C
      WRITE(10,1502)
 1502 FORMAT(1X,'#',7X,'R',14X,'P(R)',12X,'Q(R)')
      DO I=1,NTAB
C  ----  Do not print values less than 1.0D-99  ------------------------
        IF(ABS(P(I)).LT.1.0D-98) P(I)=0.0D0
        IF(ABS(Q(I)).LT.1.0D-98) Q(I)=0.0D0
C  ---------------------------------------------------------------------
        WRITE(10,'(1X,1P,3E16.8)') RAD(I),P(I),Q(I)
      ENDDO
      CLOSE(10)
C
      GO TO 20
      END


C  *********************************************************************
C                       FUNCTION VPOTR
C  *********************************************************************
      FUNCTION VPOTR(R)
C
C     Examples of analytical potentials. The output value VPOTR is the
C  potential energy at R, V(R), multiplied by R.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
      COMMON/CVPOT/Z,V0,A,IPOT

      parameter (l=0D0)
      parameter (s=0.5D0)
      parameter (j=0.5D0)

      ! Cesium from Marinescu, l=0
      parameter (Csa01=3.49546309D0)
      parameter (Csa02=1.47533800D0)
      parameter (Csa03=-9.72143084D0)
      parameter (Csa04=0.02629242D0)
      parameter (Csrc0=1.92046930D0)
      ! for the test of validity of the program using Cs
      ! should find 2 electron model for Sr
      ! optimize parameters using chi-2 method

      ! Cesium from Marenescu, l=1
      parameter (Csa11=4.69366096D0)
      parameter (Csa12=1.71398344D0)
      parameter (Csa13=-24.65624280D0)
      parameter (Csa14=-0.09543125D0)
      parameter (Csrc1=2.13383095D0)

      ! Cesium from Marenescu, l=2
      parameter (Csa21=4.32466196D0)
      parameter (Csa22=1.61365288D0)
      parameter (Csa23=-6.70128850D0)
      parameter (Csa24=-0.74095193D0)
      parameter (Csrc2=0.93007296D0)

      ! Cesium from Marenescu, l=3
      parameter (Csa31=3.01048361D0)
      parameter (Csa32=1.40000001D0)
      parameter (Csa33=-3.20036138D0)
      parameter (Csa34=0.00034538D0)
      parameter (Csrc3=1.99969677D0)

      ! hydrogen model, standard coulomb potential
      IF(IPOT.EQ.1) THEN
        ! original function
        VPOTR=Z
        IF(R.LT.A) VPOTR=VPOTR+V0*R
        ! change of r_c required according to 'dimer'

      ! compared with dimers
      ELSE IF(IPOT.EQ.2) THEN
         VPOTR = 1 + (Z-1)*EXP(-Csa01*R) - R*(Csa03 + Csa04*R)*
     1           EXP(-Csa02*R)
         IF(R.LT.A) THEN
            VPOTR = -VPOTR - V0/Csrc0**6*R**3
            VPOTR = VPOTR + (V0**2/(2*R**2))*(j*(j+1.0D0)-l*(l+1.0D0)-s
     1         *(s+1.0D0))*(1-EXP(-(R/Csrc0)**6))
         ENDIF
         IF(R.GT.A) THEN
            VPOTR = -VPOTR - (V0/R**3)*(1-EXP(-(R/Csrc0)**6))
            VPOTR = VPOTR + (V0**2/(2*R**2))*(j*(j+1.0D0)-l*(l+1.0D0)-s
     1      *(s+1.0D0))*(1-EXP(-(R/Csrc0)**6))
         ENDIF



      ! try of different meaning of A
      ELSE IF(IPOT.EQ.3) THEN
         VPOTR = 1 + (Z-1)*EXP(-Csa01*R) - R*(Csa03 + Csa04*R)*
     1           EXP(-Csa02*R)
         IF(R.LT.A) THEN
            VPORT=-VPORT- V0/Csrc0**6*R**3
         ENDIF
         IF(R.GT.A) THEN
            VPOTR = -VPOTR - (V0/R**3)*(1-EXP(-(R/Csrc0)**6))
            ! VPOTR = VPOTR + (V0**2/(2*R**2))*(j*(j+1.0D0)-l*(l+1.0D0)-s
      !1      *(s+1.0D0))*(1-EXP(-(R/Csrc0)**6))
         ENDIF


      ! compare with multiscale ...
      ELSE IF(IPOT.EQ.4) THEN
        VPOTR=Z
        IF(R.LT.A) THEN
          VPOTR=-VPOTR-V0/Csrc**6*R**3+V0/Csrc**12*R**9
        ENDIF
        IF(R.GT.A) THEN
          VPOTR=-VPOTR-(V0/R**3)*(1-exp(-(R/Csrc)**6))
        ENDIF


      ELSE IF(IPOT.EQ.6) THEN
        VPOTR=Z
      ELSE
        VPOTR=0.0D0  ! To avoid compiler warnings.
      ENDIF
C     IF(ABS(VPOTR).LT.1.0D-99) VPOTR=0.0D0
C     RETURN
      END


      subroutine appendd(data_to_write)
        character(len=*) :: data_to_write
        character(len=20) :: output_filename          
        integer :: unit_number
        logical :: file_exists
        unit_number = 10  
        output_filename='output_TEST.txt'

        open(unit=unit_number, file=output_filename,
     &     access='APPEND', RECL=73, action='WRITE')

        write(unit_number, *) data_to_write
        close(unit_number)
        print *, "Data appended to:", output_filename
      end subroutine appendd


