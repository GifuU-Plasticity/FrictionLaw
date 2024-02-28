C********************************************************************
      SUBROUTINE USRUPD(NPTRTN)          
C********************************************************************
C
C     NPTRTN:
c      1 Dry condition
c      2 Solid lubricant only
c      3 Oil lubricant only
c      4 Oil lubricant with dry condition
C
C********************************************************************
C
      IMPLICIT INTEGER*4 (I-N), REAL*8 (A-H,O-Z)
      CHARACTER*80 IUSRVL
      COMMON /IUSR/ IUSRVL(10)
C
      COMMON /USRCTL/ KOBJ,ISTATUS,KSTEP,KSSTEP     !ksstepはnegative step indicator
      COMMON /CLOK/ CURTIM
      COMMON /SSTU/ DTMAXC
C
      COMMON /ELMCOM/ RZE(2,4),URZE(2,4),STSE(6),EPSE(6),EFEPSE,EFSTSE,
     +                TEPSE,RDTYE,TEMPE(4),DTMPE(4),DAMAGE,
     +                USRE1(1500),USRE2(1500),
     +                USRNE(1500,4),NODEE(4),KELE,KELEL,KGROUP
      COMMON /ELMCOM2/ STRNE(4),NBCDE(2,4),INTNALE(4)
      COMMON /ELMCOM3/ TEPS_NE(4),EFEPS_NE(4),DAMG_NE(4),STS_NE(4,4)
      COMMON /ELMCOM4/ YLDTRNE(6)
C
      COMMON /NODCOM/ RZN(2),URZN(2),DRZN(2),TEMPN,DTMPN,USRN1(1500),
     +                USRN2(1500),KNODE
      COMMON /NODCOM2/ AREAN, TMPNEB, SLDVEL, PRESR(2), INTNAL
      COMMON /NODCOM3/ EFEPS_NN,TEPS_NN,DAMG_NN,STS_NN(4),IELMNOD(3)
C    
      COMMON /DEFGRA/ DFDX(3,3,2) 
      COMMON /VELGRDIPC/ DVDXIPC(2,2,5)
c
c ---------------------------------
      if(NPTRTN==1) then
c       Dry condition
        CALL USRSV1
      elseif(NPTRTN==2) then
c       Solid lubricant only
        CALL USRSV2
      elseif(NPTRTN==3) then
c       Oil lubricant only
        CALL USRSV3
      elseif(NPTRTN==4) then
c       Oil lubricant with dry condition
        CALL USRSV4
      endif
C
      RETURN
      END
c
C======================================================================
C======================================================================
C======================================================================
C
C     Dry condition
C
C======================================================================
c
      SUBROUTINE USRSV1
      IMPLICIT INTEGER*4 (I-N), REAL*8 (A-H,O-Z)
C
      real*8 NORPRE   !Only normal pressure is Float
c
      COMMON /USRCTL/ KOBJ,ISTATUS,KSTEP,KSSTEP
C
      COMMON /ELMCOM/ RZE(2,4),URZE(2,4),STSE(6),EPSE(6),EFEPSE,EFSTSE,
     +                TEPSE,RDTYE,TEMPE(4),DTMPE(4),DAMAGE,
     +                USRE1(1500),USRE2(1500),
     +                USRNE(1500,4),NODEE(4),KELE,KELEL,KGROUP
      COMMON /ELMCOM3/ TEPS_NE(4),EFEPS_NE(4)
      COMMON /ELMCOM4/ YLDTRNE(6)
C
      COMMON /NODCOM/ RZN(2),URZN(2),DRZN(2),TEMPN,DTMPN,USRN1(1500),
     +                USRN2(1500),KNODE
      COMMON /NODCOM2/ AREAN,TMPNEB,SLDVEL,PRESR(2),INTNAL
      COMMON /NODCOM3/ EFEPS_NN,TEPS_NN,IELMNOD2
C
      IF (ISTATUS.EQ.1.AND.KNODE.GT.0) THEN
C
       EFST = TEPS_NN + 0.08d0		!Calculate nodal efective strain + 0.08
       NORPRE = PRESR(2)              !Read normal pressure
C
c      Calculate effective stress for efective strain + 0.08
       CALL FLOW_STRESS(PEK,EST,E0,EFST,S1)
       PLCO = PEK                        ! Plastic coefficient
       WHE = EST                         ! n-value
       FIST = E0                         ! Initial strain
       FLO = S1                          ! Flow stress
C
c      Calculate Critical pressure
       CALL CRITICAL_PRESSURE_DRY(FLO,COF,SFF,CRI,SYS)
       CRIPRE = CRI                      ! Critical pressure
       FRECOE = COF                      ! Coulomb's friction coefficient
       FREFAC = SFF                      ! Shear friction factor
       FREYEI = SYS                      ! Shear stress
C
       IF(CRIPRE.LE.NORPRE) THEN
c        Pressure >= Critical pressure  =>Shear friction law
        FLAG = 1.d0
       ELSE
c        Pressure < Critical pressure  =>Coulomb's law
        FLAG = 0.d0
       ENDIF
C
c     Output user defined variable
       USRN2(1) = EFST       !nodal efective strain + 0.08
       USRN2(2) = NORPRE     !Normal pressure
       USRN2(3) = PLCO       !Plasticity coefficient
       USRN2(4) = WHE        !n-value
       USRN2(5) = FIST       !Initial strain
       USRN2(6) = FLO        !Flow stress
       USRN2(7) = CRIPRE     !Critical pressure
       USRN2(8) = FRECOE     !Coulomb's friction coefficient
       USRN2(9) = FREFAC     !Shear friction factor
       USRN2(10) = FREYEI    !Shear stress
       USRN2(11) = FLAG      !Flag
       USRN2(12) = PRESR(1)      !Tangential pressure
       USRN2(13) = (PRESR(1)**2.d0+PRESR(2)**2.d0)**0.5d0      !Norm of pressure
C     
       DO I=14, 1500
        USRN2(I)=USRN1(I)
       ENDDO
C        
      RETURN
      ENDIF
C
      RETURN
      END
c
C======================================================================
C======================================================================
C======================================================================
C
C     Solid lubricant only
C
C======================================================================
c
      SUBROUTINE USRSV2
      IMPLICIT INTEGER*4 (I,J,K,L,M,N), REAL*8 (A-H,O-Z)
      real*8 NORPRE   !Only normal pressure is Float
C
      COMMON /USRCTL/ KOBJ,ISTATUS,KSTEP,KSSTEP
C
      COMMON /ELMCOM/ RZE(2,4),URZE(2,4),STSE(6),EPSE(6),EFEPSE,EFSTSE,
     +                TEPSE,RDTYE,TEMPE(4),DTMPE(4),DAMAGE,
     +                USRE1(1500),USRE2(1500),
     +                USRNE(1500,4),NODEE(4),KELE,KELEL,KGROUP
      COMMON /ELMCOM3/ TEPS_NE(4),EFEPS_NE(4)
      COMMON /ELMCOM4/ YLDTRNE(6)
C
      COMMON /NODCOM/ RZN(2),URZN(2),DRZN(2),TEMPN,DTMPN,USRN1(1500),
     +                USRN2(1500),KNODE
      COMMON /NODCOM2/ AREAN,TMPNEB,SLDVEL,PRESR(2),INTNAL
      COMMON /NODCOM3/ EFEPS_NN,TEPS_NN,IELMNOD2
C
      IF (ISTATUS.EQ.1.AND.KNODE.GT.0) THEN
C
       NORPRE = PRESR(2)                 ! Normal pressure
       TEMPER = TEMPN 				   ! Nodal temperature
C
c       Read data from input
       CALL READ_DATA_SOL(SELECT,CFC,T0)
       SELTEMP = SELECT                      ! Number of tempearature to use
c
c     Output user defined variable
       USRN2(1) = NORPRE       !Normal pressure
       USRN2(2) = TEMPER       !Temperature of node
       USRN2(3) = SELTEMP      !kind of temperature to use
       USRN2(4) = CFC          !Reference coulomb's coefficient
       USRN2(5) = T0           !Reference temperature
C     
       DO I=6, 1500
        USRN2(I)=USRN1(I)
       ENDDO
C        
       RETURN
      ENDIF
C
      RETURN
      END     
c
C======================================================================
C======================================================================
C======================================================================
C
C     Oil lubricant only
C
C======================================================================
c
      SUBROUTINE USRSV3
      IMPLICIT INTEGER*4 (I,J,K,L,M,N), REAL*8 (A-H,O-Z)
      real*8 NORPRE   !Only normal pressure is Float
C
      COMMON /USRCTL/ KOBJ,ISTATUS,KSTEP,KSSTEP
C
      COMMON /ELMCOM/ RZE(2,4),URZE(2,4),STSE(6),EPSE(6),EFEPSE,EFSTSE,
     +                TEPSE,RDTYE,TEMPE(4),DTMPE(4),DAMAGE,
     +                USRE1(1500),USRE2(1500),
     +                USRNE(1500,4),NODEE(4),KELE,KELEL,KGROUP
      COMMON /ELMCOM3/ TEPS_NE(4),EFEPS_NE(4)
      COMMON /ELMCOM4/ YLDTRNE(6)
C
      COMMON /NODCOM/ RZN(2),URZN(2),DRZN(2),TEMPN,DTMPN,USRN1(1500),
     +                USRN2(1500),KNODE
      COMMON /NODCOM2/ AREAN,TMPNEB,SLDVEL,PRESR(2),INTNAL
      COMMON /NODCOM3/ EFEPS_NN,TEPS_NN,IELMNOD2
C
      IF (ISTATUS.EQ.1.AND.KNODE.GT.0) THEN
C
       NORPRE = PRESR(2)                 ! Normal pressure
c
       EFST = TEPS_NN + 0.08d0		!Calculate nodal efective strain + 0.08
c      Calculate effective stress for efective strain + 0.08
       CALL FLOW_STRESS(PEK,EST,E0,EFST,S1)
       PLCO = PEK                        ! Plastic coefficient
       WHE = EST                         ! n-value
       FIST = E0                         ! Initial strain
       FLO = S1                          ! Flow stress
C
c      Read friction coefficient and critical pressure
       CALL CRITICAL_PRESSURE_OIL(COF1,COF2,COFP1,COFP2)
       FRECOE1 = COF1                      ! Coulomb's friction coefficient 1
       FRECOE2 = COF2                      ! Coulomb's friction coefficient 2
       FRECRI1 = COFP1                     ! Critical pressure 1 divided by yield stress
       FRECRI2 = COFP2                     ! Critical pressure 2 divided by yield stress
C
c      set flag
       IF(NORPRE<=FRECRI1*FLO) THEN
c        Pressure <= Critical pressure 1  =>Friction coefficeint 1
        FLAG = 0.d0
       ELSEIF(NORPRE>=FRECRI1*FLO .and. 
     +        NORPRE<=FRECRI2*FLO) THEN
c        Critical pressure 1 < Pressure <= Critical pressure 2   =>Linear Interpolation
	  FLAG = 1.d0
       ELSE
c        Critical pressure 2 < Pressure   =>Friction coefficeint 2
        FLAG = 2.d0
       ENDIF
C
       USRN2(1) = EFST       !nodal efective strain + 0.08
       USRN2(2) = NORPRE     !Normal pressure
       USRN2(3) = PLCO       !Plasticity coefficient
       USRN2(4) = WHE        !n-value
       USRN2(5) = FIST       !Initial strain
       USRN2(6) = FLO        !Flow stress
       USRN2(7) = FRECOE1    !Friction coefficient 1
       USRN2(8) = FRECOE2    !Friction coefficient 2
       USRN2(9) = FRECRI1    !Critical pressure 1 divided by yield stress
       USRN2(10) = FRECRI2   !Critical pressure 2 divided by yield stress
       USRN2(11) = FRECRI1*FLO    !Critical pressure 1
       USRN2(12) = FRECRI2*FLO    !Critical pressure 2
       USRN2(13) = FLAG      !Flag
C     
       DO I=14, 1500
        USRN2(I)=USRN1(I)
       ENDDO
C        
       RETURN
      ENDIF
C
      RETURN
      END     
c
C======================================================================
C======================================================================
C======================================================================
C
C     Oil lubricant with dry condition
C
C======================================================================
c
      SUBROUTINE USRSV4
      IMPLICIT INTEGER*4 (I-N), REAL*8 (A-H,O-Z)
      real*8 NORPRE   !Only normal pressure is Float
c
      COMMON /SSTU/ DTMAXC
      COMMON /CLOK/ CURTIM 
      COMMON /USRCTL/ KOBJ,ISTATUS,KSTEP,KSSTEP
C
      COMMON /ELMCOM/ RZE(2,4),URZE(2,4),STSE(6),EPSE(6),EFEPSE,EFSTSE,
     +                TEPSE,RDTYE,TEMPE(4),DTMPE(4),DAMAGE,
     +                USRE1(1500),USRE2(1500),
     +                USRNE(1500,4),NODEE(4),KELE,KELEL,KGROUP
      COMMON /ELMCOM3/ TEPS_NE(4),EFEPS_NE(4)
      COMMON /ELMCOM4/ YLDTRNE(6)
C
      COMMON /NODCOM/ RZN(2),URZN(2),DRZN(2),TEMPN,DTMPN,USRN1(1500),
     +                USRN2(1500),KNODE
      COMMON /NODCOM2/ AREAN,TMPNEB,SLDVEL,PRESR(2),INTNAL
      COMMON /NODCOM3/ EFEPS_NN,TEPS_NN,IELMNOD2
C
      IF (ISTATUS.EQ.1.AND.KNODE.GT.0) THEN
C
       EFST = TEPS_NN + 0.08d0		!Calculate nodal efective strain + 0.08
       NORPRE = PRESR(2)                 ! Normal pressure
C
c      Calculate effective stress for efective strain + 0.08
       CALL FLOW_STRESS(PEK,EST,E0,EFST,S1)
       PLCO = PEK                        ! Plastic coefficient
       WHE = EST                         ! n-value
       FIST = E0                         ! Initial strain
       FLO = S1                          ! Flow stress
C
c      Calculate Critical pressure
       CALL CRITICAL_PRESSURE_DRY(FLO,COF,SFF,CRI,SYS)
       CRIPRE = CRI                      ! Critical pressure
       FRECOE = COF                      ! Coulomb's friction coefficient
       FREFAC = SFF                      ! Shear friction factor
       FREYEI = SYS                      ! Shear stress
C
       IF(CRIPRE.LE.NORPRE) THEN
c        Pressure >= Critical pressure  =>Shear friction law
        FLAG = 1.d0
       ELSE
c        Pressure < Critical pressure  =>Coulomb's law
        FLAG = 0.d0
       ENDIF
C
       USRN2(1) = EFST       !nodal efective strain + 0.08
       USRN2(2) = NORPRE     !Normal pressure
       USRN2(3) = PLCO       !Plasticity coefficient
       USRN2(4) = WHE        !n-value
       USRN2(5) = FIST       !Initial strain
       USRN2(6) = FLO        !Flow stress
       USRN2(7) = CRIPRE     !Critical pressure
       USRN2(8) = FRECOE     !Coulomb's friction coefficient
       USRN2(9) = FREFAC     !Shear friction factor
       USRN2(10) = FREYEI    !Shear stress
       USRN2(11) = FLAG      !Flag
       USRN2(12) = PRESR(1)      !Tangential pressure
       USRN2(13) = (PRESR(1)**2.d0+PRESR(2)**2.d0)**0.5d0      !Norm of pressure
C     
C
c      Read friction coefficient and critical pressure
       CALL FRIC_COEF_OILv2(COF1,COF2,COFP1,COFP2)
       FRECOE1 = COF1                      ! Coulomb's friction coefficient 1
       FRECOE2 = COF2                      ! Coulomb's friction coefficient 2
       FRECRI1 = COFP1                     ! Critical pressure 1 divided by yield stress
       FRECRI2 = COFP2                     ! Critical pressure 2 divided by yield stress
C
c      set flag
       IF(NORPRE<=FRECRI1*FLO) THEN
c        Pressure <= Critical pressure 1  =>Friction coefficeint 1
        FLAG = 0.d0
       ELSEIF(NORPRE>=FRECRI1*FLO .and. 
     +        NORPRE<=FRECRI2*FLO) THEN
c        Critical pressure 1 < Pressure <= Critical pressure 2   =>Linear Interpolation
	  FLAG = 1.d0
       ELSE
c        Critical pressure 2 < Pressure   =>Friction coefficeint 2
        FLAG = 2.d0
       ENDIF
C
       USRN2(14) = FRECOE1    !Friction coefficient 1
       USRN2(15) = FRECOE2    !Friction coefficient 2
       USRN2(16) = FRECRI1    !Critical pressure 1 divided by yield stress
       USRN2(17) = FRECRI2    !Critical pressure 2 divided by yield stress
       USRN2(18) = FRECRI1*FLO    !Critical pressure 1
       USRN2(19) = FRECRI2*FLO    !Critical pressure 2
       USRN2(20) = FLAG      !Flag
C     
       DO I=21, 1500
        USRN2(I)=USRN1(I)
       ENDDO
C        
      ENDIF
C
      RETURN
      END
C
C********************************************************************
c     Calculate flow stress
      SUBROUTINE FLOW_STRESS(PEK,EST,E0,EFST,S1)
C********************************************************************
C      
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N) 
      CHARACTER*80 IUSRVL
      COMMON /IUSR/ IUSRVL(10)    
C
c      Read data from iusrvl
      READ(IUSRVL(1),*)FSW      !Plastic coefficient
      READ(IUSRVL(2),*)BSW      !n-value
      READ(IUSRVL(3),*)CNI      !Initial strain
C
      PEK = FSW
      EST = BSW
      E0  = CNI
C
c     Calculate flow stress
      S1 = PEK * (EFST + E0) ** EST 
C
      RETURN
      END
c*********************************************************************
c
c
C*********************************************************************
c      Calculate Critical pressure
      SUBROUTINE CRITICAL_PRESSURE_DRY(FLO,COF,SFF,CRI,SYS)
C*********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER*80 IUSRVL
      COMMON /IUSR/ IUSRVL(10)
C         
c     Read Coulomb's friction coefficient
      READ(IUSRVL(4),*)COU
      COF = COU
c
      AOW = 0.d0     !Theta deg
C
      AOW =AOW * 3.1416d0 / 180.d0     ! deg -> rad
      a = -0.48d0 * AOW ** (2.d0) + 2.48d0 * AOW - 3.82d0      !a=-0.48*Theta^2 + 2.48*Theta - 3.82
      b = -1.19d0 * AOW + 3.d0         ! b=-1.19*Theta + 3.0
      SFF = SQRT(3.d0) * COF * (a * COF + b)     !m=sqrt(3)*mu*(a*mu + b)
      PHI = ACOS(SFF) * 0.5d0     !phi=cos^-1(m) / 2
      CRI = (1.d0 + SIN(2.d0 * PHI)) * 0.5d0    !(1+sin(2phi))/2
      CRI = CRI + 3.1416d0 / 4.d0 + PHI - AOW  !(1+sin(2phi))/2 + pi/4 + phi - Theta
      CRI = CRI * 2.d0 * FLO / SQRT(3.d0)     !Pcr = 2/sqrt(3) * (...) * Y
      SYS = FLO / SQRT(3.d0)     !Shear stress Y/sqrt(3)
C
      RETURN
      END
C********************************************************************            
c
c
C*********************************************************************
c       Read data from input for solid lubricant
      SUBROUTINE READ_DATA_SOL(SEL,CFC,T0)
C*********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER*80 IUSRVL
      COMMON /IUSR/ IUSRVL(10)  !Input variable
C         
      READ(IUSRVL(1),*) SELC
      READ(IUSRVL(2),*) CFCC
      READ(IUSRVL(3),*) T0C
C
      SEL = SELC
      CFC = CFCC
      T0 = T0C
C
      RETURN
      END
c
C********************************************************************            
c
c
C*********************************************************************
c       Read data from input for OIL lubricant for USRSV3
      SUBROUTINE CRITICAL_PRESSURE_OIL(COF1,COF2,COFP1,COFP2)
C*********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER*80 IUSRVL
C
      COMMON /IUSR/ IUSRVL(10)
C         
      READ(IUSRVL(4),*)COU1      !Friction coefficient 1
      READ(IUSRVL(5),*)COU2      !Friction coefficient 2
	READ(IUSRVL(6),*)AL1       !Critical pressure 1 divided by yield stress
      READ(IUSRVL(7),*)AL2       !Critical pressure 2 divided by yield stress
C
      COF1 = COU1     !Friction coefficient 1
      COF2 = COU2     !Friction coefficient 2
	COFP1 = AL1     !Critical pressure 1 divided by yield stress
	COFP2 = AL2     !Critical pressure 2 divided by yield stress
C
      RETURN
      END
C********************************************************************            
c
C*********************************************************************
c       Read data from input for OIL lubricant for USRSV4
      SUBROUTINE FRIC_COEF_OILv2(COF1,COF2,COFP1,COFP2)
C*********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER*80 IUSRVL
C
      COMMON /IUSR/ IUSRVL(10)
C         
      READ(IUSRVL(5),*)COU1      !Friction coefficient 1
      READ(IUSRVL(6),*)COU2      !Friction coefficient 2
      READ(IUSRVL(7),*)AL1       !Critical pressure 1 divided by yield stress
      READ(IUSRVL(8),*)AL2       !Critical pressure 2 divided by yield stress
C
      COF1 = COU1     !Friction coefficient 1
      COF2 = COU2     !Friction coefficient 2
      COFP1 = AL1     !Critical pressure 1 divided by yield stress
      COFP2 = AL2     !Critical pressure 2 divided by yield stress
C
      RETURN
      END
C********************************************************************          