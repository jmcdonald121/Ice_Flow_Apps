C                                                                       00050000
C   GRIDVEL PROGRAM                                                     00060000
C                                                                       00070000
C   CALCULATES VELOCITIES (METERS/YEAR) FROM GEOGRAPHIC COORDINATES.    00080000
C   USER SPECIFIES SITE ID, INITIAL AND FINAL LATITUDE,LONGITUDE,       00090000
C   AND HEIGHT (OR ELEVATION), AND THE ELAPSED TIME (DAYS) BETWEEN      00100000
C   THE INITIAL AND FINAL POSITION MEASUREMENT.                         00110000
C   PROGRAM FIRST CONVERTS GEOGRAPHIC TO POLAR GRID COORDINATES,        00120000
C   WITH THE ORIGIN AT THE SOUTH POLE. THE SITE LONGITUDE IS THE        00130000
C   POLAR ANGLE FOR THAT SITE, AND THE RADIUS IS THE ARC LENGTH         00140000
C   FROM THE POLE TO THE SITE TAKING THE ELLIPSOID TO BE A SPHERE       00150000
C   OF RADIUS REFF. REFF IS COMPUTED BY AVERAGING THE RADIUS OF         00160000
C   CURVATURE OF THE ELLIPSOID BETWEEN TWO USER-SPECIFIED LATITUDES     00170000
C   LAT1 (THET1 IN RADIANS) AND LAT2 (THET2 IN RADIANS).                00180000
C   IN ADDITION, USER CAN CONTROL LOCATION OF X-AXIS BY SPECIFYING      00190000
C   THE REFERENCE LONGITUDE LONG0.                                      00200000
C   USING THE CARTESIAN COORDINATES X1,Y1,Z1 AND X2,Y2,Z2 COMPUTED      00210000
C   FOR EACH SITE, PROGRAM CALCULATES VX,VY,VZ, AND V, IN METERS        00220000
C   PER YEAR.                                                           00230000
C                                                                       00240000
C   INPUT:                                                              00250000
C   FIRST CARD: TITLE (10A8)                                            00260000
C   SECOND CARD: INUM=NUMBER OF DATA POINTS (I3)                        00270000
C   THIRD CARD: LAT1,LAT2,LONG0 (2I3,F7.3,2I3,F7.3,I4,I3,F7.3)          00280000
C   1(FIRST COLUMN)                                                     00290000
C    DD MM SS.SSS DD MM SS.SSS DDD MM SS.SSS                            00300000
C   SITE DATA CARDS: LATI, LONGI, HEIGHTI, LATF, LONGF, HEIGHTF, DELT, S00310000
C   DDMMSS.SSS DDDMMSS.SSS HHH.HH DDMMSS.SSS DDDMMSS.SS HHH.HH TTT.T AAA00320000
C                                                                       00330000
      IMPLICIT REAL*8(A-H,L-Z)                                          00340000
      CHARACTER*12 INPUT,OUTPUT
C                                                                       00350000
      DIMENSION X1(78),Y1(78),Z1(78),X2(78),Y2(78),Z2(78),DELT(78)      00360000
      DIMENSION ILT1D(78),ILT1M(78),LT1S(78)                            00370000
      DIMENSION ILNG1D(78),ILNG1M(78),LNG1S(78)                         00380000
      DIMENSION ILT2D(78),ILT2M(78),LT2S(78)                            00390000
      DIMENSION ILNG2D(78),ILNG2M(78),LNG2S(78)                         00400000
      DIMENSION VX(78),VY(78),VZ(78),V(78)                              00410000
      DIMENSION DELX(78),DELY(78),DELZ(78)                              00420000
      DIMENSION SITE(78),TITLE(10)                                      00430000
C                                                                       00440000
      PI=3.1415926535898D0                                              00450000
      SECRAD=PI/6.48D05                                                 00460000
      AN=365.D0                                                         00470000
C                                                                       00480000
C   WGS-72 ELLIPSOID:                                                   00490000
      A=6378135.D0                                                      00500000
      E2=6.694317778D-03                                                00510000
C                                                                       00520000
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
C
	OPEN(5,FILE=INPUT,STATUS='OLD')
      READ(5,111) (TITLE(K),K=1,10)                                     00530000
 111  FORMAT(10A8)                                                      00540000
      READ(5,112) INUM                                                  00550000
 112  FORMAT(I3)                                                        00560000
C                                                                       00570000
      READ(5,222) ILAT1D,ILAT1M,LAT1S,ILAT2D,ILAT2M,LAT2S,ILON0D,       00580000
     <ILON0M,LON0S                                                      00590000
 222  FORMAT(2I3,F7.3,2I3,F7.3,I4,I3,F7.3)                              00600000
C                                                                       00610000
C   CONVERT LAT1, LAT2, AND LONG0 TO RADIANS                            00620000
C                                                                       00630000
      THET1=SECRAD*(3.6D3*DBLE(ILAT1D)+6.0D1*DBLE(ILATIM)+LAT1S)        00640000
      THET2=SECRAD*(3.6D3*DBLE(ILAT2D)+6.0D1*DBLE(ILAT2M)+LAT2S)        00650000
      LONG0=SECRAD*(3.6D3*DBLE(ILON0D)+6.0D1*DBLE(ILON0M)+LON0S)        00660000
C                                                                       00670000
C     COMPUTE EFFECTIVE RADIUS FOR APPROXIMATING ELLIPSOID AS SPHERE    00680000
C                                                                       00690000
      F1=DSIN(THET1)                                                    00700000
      F2=DSIN(THET2)                                                    00710000
      FF1=(1.D0-E2*F1*F1)**1.5                                          00720000
      FF2=(1.D0-E2*F2*F2)**1.5                                          00730000
      REFF=0.5D0*A*(1.D0-E2)*(1.D0/FF1+1.D0/FF2)                        00740000
C                                                                       00750000
C   READ INPUT COORDINATES, CONVERT TO POLAR COORDINATES, COMPUTE       00760000
C   VELOCITIES                                                          00770000
C                                                                       00780000
      DO 10 I=1,INUM                                                    00790000
      READ(5,333) ILT1D(I),ILT1M(I),LT1S(I),ILNG1D(I),ILNG1M(I),        00800000
     <LNG1S(I),Z1(I),ILT2D(I),ILT2M(I),LT2S(I),ILNG2D(I),ILNG2M(I),     00810000
     <LNG2S(I),Z2(I),DELT(I),SITE(I)                                    00820000
 333  FORMAT(2I2,F7.3,I3,I2,F7.3,F7.2,2I2,F7.3,I3,I2,F7.3,F7.2,F6.3,A8) 00830000
      LT1=SECRAD*(3.6D3*DBLE(ILT1D(I))+6.0D1*DBLE(ILT1M(I))+            00840000
     <LT1S(I))                                                          00850000
      LT2=SECRAD*(3.6D3*DBLE(ILT2D(I))+6.0D1*DBLE(ILT2M(I))+            00860000
     <LT2S(I))                                                          00870000
      LNG1=SECRAD*(3.6D3*DBLE(ILNG1D(I))+6.0D1*DBLE(ILNG1M(I))+         00880000
     <LNG1S(I))                                                         00890000
      LNG2=SECRAD*(3.6D3*DBLE(ILNG2D(I))+6.0D1*DBLE(ILNG2M(I))+         00900000
     <LNG2S(I))                                                         00910000
C                                                                       00920000
      R1=REFF*(0.5D0*PI-LT1)                                            00930000
      X1(I)=R1*DCOS(LNG1-LONG0)                                         00940000
      Y1(I)=R1*DSIN(LNG1-LONG0)                                         00950000
C                                                                       00960000
      R2=REFF*(0.5D0*PI-LT2)                                            00970000
      X2(I)=R2*DCOS(LNG2-LONG0)                                         00980000
      Y2(I)=R2*DSIN(LNG2-LONG0)                                         00990000
C                                                                       01000000
      DELX(I)=X2(I)-X1(I)                                               01010000
      DELY(I)=Y2(I)-Y1(I)                                               01020000
      DELZ(I)=Z2(I)-Z1(I)                                               01030000
C                                                                       01040000
      TIME=DELT(I)/AN                                                   01050000
      VX(I)=DELX(I)/TIME                                                01060000
      VY(I)=DELY(I)/TIME                                                01070000
      VZ(I)=DELZ(I)/TIME                                                01080000
      V(I)=DSQRT(VX(I)*VX(I)+VY(I)*VY(I))                               01090000
C                                                                       01100000
 10   CONTINUE                                                          01110000
C                                                                       01120000
C ******* OUTPUT *******                                                01130000
C                                          
      OPEN(6,FILE=OUTPUT,STATUS='NEW')
      WRITE(6,900) (TITLE(K),K=1,10)                                    01150000
 900  FORMAT('1',25X,10A8/)                                             01160000
C                                                                       01170000
      WRITE(6,905) ILON0D,ILON0M,LON0S                                  01180000
 905  FORMAT(10X,'REFERENCE LONGITUDE IS AT',I4,I3,F7.3,' W')           01190000
C                                                                       01200000
      WRITE(6,910) ILAT1D,ILAT1M,LAT1S,ILAT2D,ILAT2M,LAT2S              01210000
 910  FORMAT(10X,'EFFECTIVE RADIUS COMPUTED BETWEEN',2I3,F7.3,          01220000
     <'S AND',2I3,F7.3,'S'/)                                            01230000
C                                                                       01240000
      WRITE(6,915)                                                      01250000
 915  FORMAT(3X,'SITE',6X,'XFINAL',5X,'YFINAL',5X,'ZFINAL',3X,          01260000
     <'DELTAX',2X,'DELTAY',3X,'VX M/A',3X,'VY M/A',3X,'VZ M/A',         01270000
     <3X,'VELOCITY',6X,'LATITUDE',6X,'LONGITUDE'/)                      01280000
C                                                                       01290000
      DO 20 I=1,INUM                                                    01300000
      WRITE(6,920) SITE(I),X2(I),Y2(I),Z2(I),DELX(I),DELY(I),VX(I),     01310000
     <VY(I),VZ(I),V(I),ILT2D(I),ILT2M(I),LT2S(I),ILNG2D(I),             01320000
     <ILNG2M(I),LNG2S(I)                                                01330000
 920  FORMAT(1X,A8,F12.2,F11.2,F9.2,F9.2,F8.2,2F9.2,F8.2,F11.2,I7,I3,   01340000
     <F7.3,I5,I3,F7.3/)                                                 01350000
 20   CONTINUE                                                          01360000
C                                                                       01370000
C                                                                       01380000
      STOP                                                              01390000
      END                                                               01400000
