C======================================================================
      SUBROUTINE USRBCC(VARIABLE,NODE1,NODE2,IELEM,NBCD,NDIE,RZ,
     &     DRZ,USRNOD,NUSRN,ENVTEM,TEMO,TEMC,CURTIM,MODEL,IMODE,ITYPE,
     &     NURZ)
C======================================================================
C
c     MODEL:
c      1 Dry condition
c      2 Solid lubricant only
c      3 Oil lubricant only
c      4 Oil lubricant with dry condition
c
C======================================================================
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)
c
      COMMON /MSNODE/ MSNODE1,MSNODE2		!Node number of master
c
      DIMENSION NBCD(NURZ,2),RZ(2,2),DRZ(NURZ,2),USRNOD(NUSRN,1),
     &     TEMO(2),TEMC(2)
c
      if(MODEL==1) then
c       Dry condition
        CALL UBCC1(VARIABLE,NODE1,NODE2,IELEM,NBCD,NDIE,RZ,
     &       DRZ,USRNOD,NUSRN,ENVTEM,TEMO,TEMC,CURTIM,IMODE,ITYPE,NURZ)
      elseif(MODEL==2) then
c       Solid lubricant only
        CALL UBCC2(VARIABLE,NODE1,NODE2,IELEM,NBCD,NDIE,RZ,
     &       DRZ,USRNOD,NUSRN,ENVTEM,TEMO,TEMC,CURTIM,IMODE,ITYPE,NURZ)
      elseif(MODEL==3) then
c       Oil lubricant only
        CALL UBCC3(VARIABLE,NODE1,NODE2,IELEM,NBCD,NDIE,RZ,
     &       DRZ,USRNOD,NUSRN,ENVTEM,TEMO,TEMC,CURTIM,IMODE,ITYPE,NURZ)
      elseif(MODEL==4) then
c       Oil lubricant with dry condition
        CALL UBCC4(VARIABLE,NODE1,NODE2,IELEM,NBCD,NDIE,RZ,
     &       DRZ,USRNOD,NUSRN,ENVTEM,TEMO,TEMC,CURTIM,IMODE,ITYPE,NURZ)
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
      SUBROUTINE UBCC1(VARIABLE,NODE1,NODE2,IELEM,NBCD,NDIE,RZ,
     &     DRZ,USRNOD,NUSRN,ENVTEM,TEMO,TEMC,CURTIM,IMODE,ITYPE,NURZ)
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)
C    
      DIMENSION NBCD(NURZ,2),RZ(2,2),DRZ(NURZ,2),USRNOD(NUSRN,1),
     &     TEMO(2),TEMC(2)
C
      COMMON /UFHCOM/ TMPENV,TMPSLF,SLDVEL,PRESS
      COMMON /UFHCOM2/ STRCH,EFSTS,EFEPS
C
C****Read node variable from USRUPD*****************************************
      CFC = USRNOD(8,NODE1)             ! Friction coefficient (coulomb)
      CSF = USRNOD(9,NODE1)             ! Friction shear factor
      CPE = USRNOD(7,NODE1)             ! Critical pressure
      SY  = USRNOD(10,NODE1)            ! Shear stress k
      PRE = USRNOD(2,NODE1)             ! Normal Pressure
C**********************************************************************     
      VARIABLE = 0.0                    ! Initialize the output value
C
      IF(IMODE.EQ.1) THEN
       IF (ITYPE.EQ.2) THEN
        IF (PRE.LE.CPE) THEN
c        Pressure <= Critical pressure  =>Coulomb's law
         FRCCOE = CFC
        ELSE
c        Pressure > Critical pressure  =>Shear friction law
         FRCCOE = CSF * SY / PRE           ! convert to Coulomb's friction coefficient
        ENDIF
        VARIABLE = FRCCOE                 ! Output as Coulomb's friction coefficient
C
       ENDIF
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
      SUBROUTINE UBCC2(VARIABLE,NODE1,NODE2,IELEM,NBCD,NDIE,RZ,
     &     DRZ,USRNOD,NUSRN,ENVTEM,TEMO,TEMC,CURTIM,IMODE,ITYPE,NURZ)
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)
C
      COMMON /MSNODE/ MSNODE1,MSNODE2
C
      COMMON /UFHCOM/ TMPENV,TMPSLF,SLDVEL,PRESS
      COMMON /UFHCOM2/ STRCH,EFSTS,EFEPS
C     
      DIMENSION NBCD(NURZ,2),RZ(2,2),DRZ(NURZ,2),USRNOD(NUSRN,1),     
     &     TEMO(2),TEMC(2)
c
C****Read node variable from USRUPD******************************************
c     Temperature of work
      TEMPERW = 
     &  0.5d0 * ( USRNOD(2,NODE1)+USRNOD(2,NODE2) )
c     Temperature of tool
	TEMPERT =
     &  0.5d0 * ( USRNOD(2,MSNODE1)+USRNOD(2,MSNODE2) )
c     Average temperature of work and tool
	TEMPERL = 0.5d0 * (TEMPERW + TEMPERT)
c     Temperature condition (what kind of temarature do you use)
      SELTEMP = USRNOD(3,NODE1)
c     Reference friction coefficient
      CFC = USRNOD(4,NODE1)
c     Reference temerature
      T0 = USRNOD(5,NODE1)
C**********************************************************************
c     Selected temperature
      if(SELTEMP==1) then
        TEMP = TEMPERW
      elseif(SELTEMP==2) then
        TEMP = TEMPERT
      elseif(SELTEMP==3) then
        TEMP = TEMPERL
      endif
c
      VARIABLE = 0.0                    ! Initialize the output value
C
      IF(IMODE.EQ.1) THEN
       IF (ITYPE.EQ.2) THEN
        VARIABLE = CFC*(TEMP/T0)**(-0.403)
C
       ENDIF
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
      SUBROUTINE UBCC3(VARIABLE,NODE1,NODE2,IELEM,NBCD,NDIE,RZ,
     &     DRZ,USRNOD,NUSRN,ENVTEM,TEMO,TEMC,CURTIM,IMODE,ITYPE,NURZ)
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)
C
      DIMENSION NBCD(NURZ,2),RZ(2,2),DRZ(NURZ,2),USRNOD(NUSRN,1),
     &     TEMO(2),TEMC(2)
C
      COMMON /UFHCOM/ TMPENV,TMPSLF,SLDVEL,PRESS
      COMMON /UFHCOM2/ STRCH,EFSTS,EFEPS
c
C****Read node variable from USRUPD******************************************
      CFC1 = USRNOD(7,NODE1)             ! Friction coefficient 1
      CFC2 = USRNOD(8,NODE1)             ! Friction coefficient 2
      CPE1 = USRNOD(11,NODE1)             ! Critical pressure 1
      CPE2 = USRNOD(12,NODE1)             ! Critical pressure 2
      PRE = USRNOD(2,NODE1)             ! Normal Pressure
C**********************************************************************
      VARIABLE = 0.0                    ! Initialize the output value
C
      IF(IMODE.EQ.1) THEN
       IF (ITYPE.EQ.2) THEN
C
        IF (PRE<=CPE1) THEN 
c        Pressure <= Critical pressure 1  =>Friction coefficeint 1
         FRCCOE = CFC1
        ELSEIF (PRE>=CRE1 .AND. PRE<=CRE2) THEN
c        Critical pressure 1 < Pressure < Critical pressure 2   =>Linear Interpolation
c        a=(CFC2-CFC1)/(CPE2-CPE1)
         FRCCOE = (CFC2-CFC1)/(CPE2-CPE1)*(PRE-CPE1)+CFC1
        ELSE
c        Critical pressure 2 <= Pressure   =>Friction coefficeint 2
         FRCCOE = CFC2
        ENDIF
c
	  VARIABLE = FRCCOE
C
       ENDIF
      ENDIF
C
      RETURN
C
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
      SUBROUTINE UBCC4(VARIABLE,NODE1,NODE2,IELEM,NBCD,NDIE,RZ,
     &     DRZ,USRNOD,NUSRN,ENVTEM,TEMO,TEMC,CURTIM,IMODE,ITYPE,NURZ)
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)
C
      DIMENSION NBCD(NURZ,2),RZ(2,2),DRZ(NURZ,2),USRNOD(NUSRN,1),
     &     TEMO(2),TEMC(2)
C
      COMMON /UFHCOM/ TMPENV,TMPSLF,SLDVEL,PRESS
      COMMON /UFHCOM2/ STRCH,EFSTS,EFEPS
c
C****Read node variable from USRUPD******************************************
      CFC1 = USRNOD(14,NODE1)             ! Friction coefficient 1
      CFC2 = USRNOD(15,NODE1)             ! Friction coefficient 2
      CPE1 = USRNOD(18,NODE1)             ! Critical pressure  1
      CPE2 = USRNOD(19,NODE1)             ! Critical pressure  2
      PRE = USRNOD(2,NODE1)             ! Normal Pressure
C**********************************************************************
      VARIABLE = 0.0                    ! Initialize the output value
C
      IF(IMODE.EQ.1) THEN 
       IF (ITYPE.EQ.2) THEN 
C
        IF (PRE<=CPE1) THEN
c        Pressure <= Critical pressure 1  =>Friction coefficeint 1
         FRCCOE = CFC1
        ELSEIF (PRE>=CRE1 .AND. PRE<=CRE2) THEN
c        Critical pressure 1 < Pressure < Critical pressure 2   =>Linear Interpolation
c        a=(CFC2-CFC1)/(CPE2-CPE1)
         FRCCOE = (CFC2-CFC1)/(CPE2-CPE1)*(PRE-CPE1)+CFC1
        ELSE
c        Critical pressure 2 <= Pressure   =>Friction coefficeint 2
         FRCCOE = CFC2
        ENDIF
c
	  VARIABLE = FRCCOE
C
       ENDIF
      ENDIF
C
      RETURN
      END 
C======================================================================
c
c
C======================================================================
      SUBROUTINE USRBCC2(VARIABLE,NODE1,NODE2,IELEM,NBCD,NDIE,RZ,
     &     DRZ,USRNOD,NUSRN,ENVTEM,TEMO,TEMC,CURTIM,MODEL,IMODE,ITYPE,
     &     NURZ,URZ,USRELE,NUSRE,STRAN,NSTRN,PRESS,CONCDS,HICAPAS,
     *     CONCDM,HICAPAM,DTMAXC)
C======================================================================
C
C     User defined boundary value
C
C     Input :
C
C     NODE1, NODE2 : edge node number
C     IELEM : element number it belong
C     NBCD(NURZ,2) : boundary condition code
C     NDIE : contact condition
C          0 : non-contact
C          n : contact to n object
C
C     RZ(2,2) : coordinate of nodes
C     DRZ(NURZ,2) : displacement of nodes      
C*     URZ(NURZ,2) : displacement of nodes      
C     USRNOD(NUSRN,I) : user defined node variables 
C        I : node number, use node1 and node2 to get the
C            values
C     NUSRN : number of user definde node variables
C*     USRELE(NUSRE,IELEM) : user defined element variables 
C        I : element number, use IELEM to get the values
C*     NUSRE : number of user definde element variables
C*    STRAN(LSTSR,IELEM) : strain components 
C*    LSTSR : number of strain components
C*    PRESS : nodal pressure
C*    CONCDS : conductivity for slave (workpiec)
C*    HICAPAS : heat capacity for slave (workpiece)
C*    CONCDM : conductivity for master ( die )
C*    HICAPAM : heat capacity master (die ) 
C     ENVTEM : environmental temperature or film temperature
C     TEMO(2) : previous step temperature
C     TEMC(2) : current step temperature     
C     CURTIM : currunt time 
C     MODEL : subroutine number (function number)
C     IMODE = 2 : simulation type heat transfer
C     ITYPE = 2  : output type lubricant coefficianet
C     NURZ  : number of degree of freedom in velocity and displacement 
C              2 : for axisymmetric and plane strain  
C              3 : for axisymmetric with angular velocity
C
C     /USRCTL/  KOBJ,ISTATUS,KSTEP,KSSTEP
C
C     KSTEP : number of step
C     KSSTEP : negative step indicator -1 for negative step or 1
C
C     Output :
C     
C     Variable : user defined boundary coefficient or value
C
C======================================================================
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)      
C      
      COMMON /USRCTL/ KOBJ,ISTATUS,KSTEP,KSSTEP

      DIMENSION NBCD(NURZ,2),RZ(2,2),DRZ(NURZ,2),USRNOD(NUSRN,*),     
     &     TEMO(2),TEMC(2),USRELE(NUSRE,*),STRAN(NSTRN,*),PRESS(2,*)
     
      VARIABLE = 1.0
      XMID =  ( RZ(1,1) + RZ(1,2) ) / 2.0D0
      VARIABLE = XMID
C
      WRITE(*,*) KSTEP, KSSTEP, KSTEP*KSSTEP
C
      RETURN
      END
C
C======================================================================
