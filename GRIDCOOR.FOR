C                                                                       00060000
C   GRIDCOOR PROGRAM                                                    00070000
C                                                                       00080000
C   CONVERTS GEOGRAPHIC COORDINATES TO POLAR GRID COORDINATES           00090000
C   USER SPECIFIES SITE ID, LATITUDE, LONGITUDE, AND HEIGHT FOR         00100000
C   INPUT SITES. PROGRAM THEN CALCULATES CARTESIAN GRID COORDINATES     00110000
C   WITH THE ORIGIN AT THE SOUTH POLE. THE SITE LONGITUDE IS THE        00120000
C   POLAR ANGLE FOR THAT SITE, AND THE RADIUS IS THE ARC LENGTH         00130000
C   FROM THE POLE TO THE SITE TAKING THE ELLIPSOID TO BE A SPHERE       00140000
C   OF RADIUS REFF. REFF IS COMPUTED BY AVERAGING THE RADIUS OF         00150000
C   CURVATURE OF THE ELLIPSOID BETWEEN TWO USER-SPECIFIED LATITUDES     00160000
C   LAT1 (THET1 IN RADIANS) AND LAT2 (THET2 IN RADIANS).                00170000
C   IN ADDITION, USER CAN CONTROL LOCATION OF X-AXIS BY SPECIFYING      00180000
C   THE REFERENCE LONGITUDE LONG0.                                      00190000
C                                                                       00200000
C   INPUT:                                                              00210000
C   FIRST CARD: TITLE (10A8)                                            00220000
C   SECOND CARD: INUM=NUMBER OF DATA POINTS (I3)                        00230000
C   THIRD CARD: LAT1,LAT2,LONG0 (2I3,F7.3,2I3,F7.3,I4,I3,F7.3)          00240000
C   1(FIRST COLUMN)                                                     00250000
C    XX XX XX.XXX XX XX XX.XXX XXX XX XX.XXX                            00260000
C   SITE DATA CARDS: LAT, LONG, HEIGHT, SITE ID                         00270000
C    XX XX XX.XXX XXX XX XX.XXX XXXX.XX AAAAAAAA                        00280000
C                                                                       00290000
      IMPLICIT REAL*8(A-H,L-Z)                                          00300000
      CHARACTER*12 INPUT,OUTPUT
      DIMENSION X(300),Y(300),Z(300)                                    00310000
      DIMENSION ILATD(300),ILATM(300),LATSEC(300)                       00320000
      DIMENSION ILONGD(300),ILONGM(300),LONGSC(300)                     00330000
      DIMENSION SITE(300),TITLE(10)                                     00340000
C                                                                       00350000
      PI=3.1415926535898D0                                              00360000
      SECRAD=PI/6.48D05                                                 00370000
C                                                                       00380000
C   WGS-72 ELLIPSOID:                                                   00390000
      A=6378135.0                                                       00400000
      E2=0.006694317778                                                 00410000
C
C   ENTER THE INPUT AND OUTPUT FILE NAMES
      WRITE(0,9001)
9001  FORMAT(' ','ENTER THE INPUT FILE NAME (INCLUDE THE EXTENSION)',//)
      READ(0,901) INPUT
901   FORMAT(A12)
      WRITE(0,902)
902   FORMAT(' ',//,1X,'ENTER THE OUTPUT FILE NAME (INCLUDE THE EXTENSIO
     1N)',//)
      READ(0,903) OUTPUT
903   FORMAT(A12)
C
C                                                                       00420000
      OPEN(5,FILE=INPUT,STATUS='OLD')
      OPEN(6,FILE=OUTPUT,STATUS='NEW')
      READ(5,111) (TITLE(K),K=1,10)                                     00430000
 111  FORMAT(10A8)                                                      00440000
      READ(5,112) INUM                                                  00450000
 112  FORMAT(I3)                                                        00460000
C                                                                       00470000
      READ(5,222) ILAT1D,ILAT1M,LAT1S,ILAT2D,ILAT2M,LAT2S,ILON0D,       00480000
     <ILON0M,LON0S                                                      00490000
 222  FORMAT(2I3,F7.3,2I3,F7.3,I4,I3,F7.3)                              00500000
C                                                                       00510000
C   CONVERT LAT1, LAT2, AND LONG0 TO RADIANS                            00520000
C                                                                       00530000
      THET1=SECRAD*(3600.*DBLE(ILAT1D)+60.*DBLE(ILATIM)+LAT1S)          00540000
      THET2=SECRAD*(3600.*DBLE(ILAT2D)+60.*DBLE(ILAT2M)+LAT2S)          00550000
      LONG0=SECRAD*(3600.*DBLE(ILON0D)+60.*DBLE(ILON0M)+LON0S)          00560000
C                                                                       00570000
C     COMPUTE EFFECTIVE RADIUS FOR APPROXIMATING ELLIPSOID AS SPHERE    00580000
C                                                                       00590000
      F1=DSIN(THET1)                                                    00600000
      F2=DSIN(THET2)                                                    00610000
      FF1=(1.D0-E2*F1*F1)**1.5                                          00620000
      FF2=(1.D0-E2*F2*F2)**1.5                                          00630000
      REFF=0.5D0*A*(1.D0-E2)*(1.D0/FF1+1.D0/FF2)                        00640000
C                                                                       00650000
C   READ INPUT COORDINATES AND CONVERT TO POLAR COORDINATES             00660000
C                                                                       00670000
      DO 10 I=1,INUM                                                    00680000
      READ(5,333) ILATD(I),ILATM(I),LATSEC(I),ILONGD(I),ILONGM(I),      00690000
     <LONGSC(I),Z(I),SITE(I)                                            00700000
 333  FORMAT(2I3,F7.3,I4,I3,F7.3,F8.2,A8)                               00710000
      LAT=SECRAD*(3600.*DBLE(ILATD(I))+60.*DBLE(ILATM(I))+              00720000
     <LATSEC(I))                                                        00730000
      LONG=SECRAD*(3600.*DBLE(ILONGD(I))+60.*DBLE(ILONGM(I))+           00740000
     <LONGSC(I))                                                        00750000
C                                                                       00760000
      R=REFF*(0.5D0*PI-LAT)                                             00770000
      X(I)=R*DCOS(LONG-LONG0)                                           00780000
      Y(I)=R*DSIN(LONG-LONG0)                                           00790000
C                                                                       00800000
 10   CONTINUE                                                          00810000
C                                                                       00820000
C ******* OUTPUT *******                                                00830000
C                                                                       00840000
      WRITE(6,900) (TITLE(K),K=1,10)                                    00850000
 900  FORMAT('1',25X,10A8/)                                             00860000
C                                                                       00870000
      WRITE(6,905)                                                      00880000
 905  FORMAT(' SITE',10X,'POLAR GRID COORDINATES',13X,                  00890000
     <'GEOGRAPHIC COORDINATES')                                         00900000
      WRITE(6,910)                                                      00910000
 910  FORMAT('  ID ',8X,'X',12X,'Y',11X,'Z',11X,'LATITUDE',6X,          00920000
     <'LONGITUDE'/)                                                     00930000
C                                                                       00940000
      DO 20 I=1,INUM                                                    00950000
      WRITE(6,915) SITE(I),X(I),Y(I),Z(I),ILATD(I),ILATM(I),            00960000
     <LATSEC(I),ILONGD(I),ILONGM(I),LONGSC(I)                           00970000
 915  FORMAT(1X,A8,F12.2,2X,F11.2,2X,F8.2,5X,2I3,F7.3,2X,2I3,F7.3/)    00980000
 20   CONTINUE                                                          00990000
C                                                                       01000000
C                                                                       01010000
      STOP                                                              01020000
      END                                                               01030000
