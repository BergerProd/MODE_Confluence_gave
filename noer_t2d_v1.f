!                    *****************
                     SUBROUTINE NOEROD
!                    *****************
!
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
!
!***********************************************************************
! SISYPHE   V7P1                                  21/07/2011
!***********************************************************************
!
!brief    FIXES THE NON-ERODABLE BED ELEVATION ZR.
!
!note     METHODS OF TREATMENT OF NON-ERODABLE BEDS CAN LEAD TO ZF.
!note  CHOOSE TO SMOOTH THE SOLUTION WITH NLISS > 0.
!
!history  C. LENORMANT
!+
!+        V5P1
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        21/06/2013
!+        V6P3
!+   Now ZR=ZF-100.D0 by default
!+   previous versions was erronneously ZR=-100.D0
!history Costas (telemac forum) - Jose Díaz (Integral)
!+        04/2016
!+        V7P1
!+   Using FONSTR as a base adapted to V7P1
!+   this version uses NOER as the DEPTH of the erodible bed
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHOIX          |-->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
!| H              |-->| WATER DEPTH
!| NLISS          |<->| NUMBER OF SMOOTHINGS
!| NPOIN          |-->| NUMBER OF 2D POINTS
!| X,Y            |-->| 2D COORDINATES
!| Z              |-->| FREE SURFACE
!| ZF             |-->| BED LEVEL
!| ZR             |<--| RIGID BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
!      USE DECLARATIONS_SISYPHE , ONLY : SIS_FILES, SISGEO, IELMT, MESH
      USE DECLARATIONS_TELEMAC2D , ONLY : T2D_FILES, T2DGEO, IELMT, MESH
      USE INTERFACE_HERMES
!
      IMPLICIT NONE
!      INTEGER LNG,LU
!      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
!
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     USER VARIABLES
      INTEGER ERR,IERR,RECORD,I
      DOUBLE PRECISION BID
      TYPE(BIEF_OBJ) :: NOER
      CHARACTER(LEN=8) :: FFORMAT
!      CHARACTER, INTENT(OUT) :: T2D_FILES(T2DGEO)%LU
!      CHARACTER(LEN=16) :: NAME
!      REAL, ALLOCATABLE :: W(:)
!      LOGICAL OK
!      INTEGER I,ERR
!
!-----------------------------------------------------------------------
!
!    SET VARIABLES
!
      RECORD = 0
      FFORMAT = 'SERAFIN '

!
!-----------------------------------------------------------------------
!---------------------
! RIGID BEDS POSITION
!---------------------
!
!     DEFAULT VALUE: ZR=ZF-100.D0
!
!     Read rigid bed level from geometry file (variable 'RIGID BED' which is
!     erodible bed)
!     Inspired by subroutine FONSTR
!     
!     LOOKS FOR THE RIGID BOTTOM ELEVATION IN THE GEOMETRY FILE
!
      CALL BIEF_ALLVEC(1,NOER,'NOER    ',IELMT,1,2,MESH)
!      ALLOCATE(W(NPOIN),STAT=ERR)


      IF(LNG.EQ.1) CALL FIND_VARIABLE(FFORMAT, T2D_FILES(T2DGEO)%LU,
     &                            'NOER            ',NOER%R, NPOIN,
     &                            IERR,RECORD=RECORD,TIME_RECORD=BID)
      IF(LNG.EQ.2) CALL FIND_VARIABLE(FFORMAT, T2D_FILES(T2DGEO)%LU,
     &                            'NOER            ',NOER%R, NPOIN,
     &                            IERR,RECORD=RECORD,TIME_RECORD=BID)
      IF((IERR.EQ.HERMES_VAR_UNKNOWN_ERR).AND.LNG.EQ.1) THEN
        CALL FIND_VARIABLE(FFORMAT, T2D_FILES(T2DGEO)%LU,
     &                 'NOER            ',NOER%R,NPOIN,
     &                 IERR,RECORD=RECORD,TIME_RECORD=BID)
      ENDIF
      IF((IERR.EQ.HERMES_VAR_UNKNOWN_ERR).AND.LNG.EQ.2) THEN
        CALL FIND_VARIABLE(FFORMAT, T2D_FILES(T2DGEO)%LU,
     &                 'NOER            ',NOER%R,NPOIN,
     &                 IERR,RECORD=RECORD,TIME_RECORD=BID)
      ENDIF
!
!     INITIALISES THE RIGID BOTTOM ELEVATION
!
!      CALL OV('X=Y     ',ZR,NOER%R,ZF,-100.D0,NPOIN)
!      CALL OV('X=Y-C   ',ZR,ZF,ZF,0.D0,NPOIN)
!
      CALL OV('X=Y-Z   ',ZR,ZF,NOER%R,0.D0,NPOIN)

!      DO I=1,NPOIN
!        IF(ZR(I).GT.ZF(I)) THEN
!          WRITE(LU,*) I, ZF(I)-ZR(I)
!        ENDIF
!      ENDDO
!     CALL OV('X=Y+C   ',ZR,ZF,ZF,-100.D0,NPOIN)
!
!------------------
! SMOOTHING OPTION
!------------------
!
!     NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
!             DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
!
      NLISS = 0
!
!--------------------------------------------------
! CONTROL (CAN BE ACTIVATED IF ZR USER DEFINED...)
!--------------------------------------------------
!
!      DO I=1,NPOIN
!        IF(ZR(I).GT.ZF(I)) THEN
!          WRITE(LU,*) 'POINT ',I,' NON ERODABLE BED HIGHER THAN BED'
!          CALL PLANTE(1)
!          STOP
!        ENDIF
!      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
