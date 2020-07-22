CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     ***AZIMUTH.FOR***      (6/19/87)                               C   
C                                                                    C
C     THIS PROGRAM COMPUTES THE AZIMUTH OF A BASELINE BETWEEN TWO    C
C     POINTS USING THE GAUSS MID-LATITUDE FORMULA.  THE REFERENCE    C
C     ELLIPSOID IS WGS-72                                            C
C                                                                    C
C     REFERENCES                                                     C
C       1. RAPP, R.  1976?  GEOMETRIC GEODESY CLASS NOTES.           C
C       2. BOMFORD, GEODESY ?                                        C
C       3. EWING, INTRODUCTION TO GEODESY ?                          C
C                                                                    C
C     JAMES MCDONALD                                                 C
C     BYRD POLAR RESEARCH CENTER                                     C
C     OHIO STATE UNIVERSITY                                          C
C     COLUMBUS, OHIO  43210                                          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C********************************************************************C
C     VARIABLE DEFINITIONS                                           C
C                                                                    C
C     A                                                              C
C     A1           SEMI-MAJOR AXIS OF THE REFERENCE ELLIPSOID        C
C                  (WGS-72)                                          C
C     AZ           DECIMAL AZIMUTH                                   C
C     B                                                              C
C     C                                                              C
C     D                                                              C
C     DELLAM       DIFFERENCE BETWEEN SITE 1 AND SITE 2 LONGITUDE    C
C     DELPHI       DIFFERENCE BETWEEN SITE 1 AND SITE 2 LATITUDE     C
C     E            ECCENTRICITY OF THE REFERENCE ELLIPSOID (WGS-72)  C
C     IDEG         AZIMUTH, DEGREES                                  C
C     MIN          AZIMUTH, MINUTES                                  C
C     PHIM         MEAN LATITUDE                                     C
C     PHI1         SITE 1 DECIMAL LATITUDE                           C
C     PHI2         SITE 2 DECIMAL LATITUDE                           C
C     RADD         CONVERSION FROM DEGREES TO RADIANS                C
C     RMM                                                            C
C     RNM                                                            C
C     SEC          AZIMUTH, SECONDS                                  C
C     S1LAM        SITE 1 DECIMAL LONGITUDE                          C
C     S1LATD       SITE 1 LATITUDE, DEGREES                          C
C     S1LATM       SITE 1 LATITUDE, MINUTES                          C
C     S1LATS       SITE 1 LATITUDE, SECONDS                          C
C     S1LNGD       SITE 1 LONGITUDE, DEGREES                         C
C     S1LNGM       SITE 1 LONGITUDE, MINUTES                         C
C     S1LNGS       SITE 1 LONGITUDE, SECONDS                         C
C     S2LAM        SITE 2 DECIMAL LONGITUDE                          C
C     S2LATD       SITE 2 LATITUDE, DEGREES                          C
C     S2LATM       SITE 2 LATITUDE, MINUTES                          C
C     S2LATS       SITE 2 LATITUDE, SECONDS                          C
C     S2LNGD       SITE 2 LONGITUDE, DEGREES                         C
C     S2LNGM       SITE 2 LONGITUDE, MINUTES                         C
C     S2LNGS       SITE 2 LONGITUDE, SECONDS                         C
C********************************************************************C
      IMPLICIT REAL*8(A-H,O-Z)
      DATA A1/6378135.00D0/,RADD/1.7453292D-02/
      DATA E/6.694324D-3/
C-------INPUT THE LATITUDE AND LONGITUDE OF THE FIRST SITE-----------C
      WRITE(0,10)
10    FORMAT('1','INPUT THE LAT. AND LONG. OF THE FIRST SITE')
      READ(0,11) S1LATD,S1LATM,S1LATS,S1LNGD,S1LNGM,S1LNGS
11    FORMAT(D2.0,1X,D2.0,1X,D6.3,1X,D3.0,1X,D2.0,1X,D6.3)
C-------INPUT THE LATITUDE AND LONGITUDE OF THE SECOND SITE----------C
      WRITE(0,12)
12    FORMAT(' ','INPUT THE LAT. AND LONG. OF THE SECOND SITE')
      READ(0,11) S2LATD,S2LATM,S2LATS,S2LNGD,S2LNGM,S2LNGS
C-------COMPUTE DECIMAL LATITUDE AND LONGITUDE OF SITES 1 AND 2------C
      PHI1=S1LATD+S1LATM/60.D0+S1LATS/3.6D03
      S1LAM=S1LNGD+S1LNGM/60.D0+S1LNGS/3.6D03
      PHI2=S2LATD+S2LATM/60.D0+S2LATS/3.6D03
      S2LAM=S2LNGD+S2LNGM/60.D0+S2LNGS/3.6D03
C-------COMPUTE AVERAGE LATITUDE-------------------------------------C
      PHIM=(PHI1+PHI2)/2.D0
C-------GAUSS MID-LATITUDE FORMULAS----------------------------------C
      RNM=A1/(DSQRT(1.D0-E*DSIN(PHIM*RADD)**2.D0))
      RMM=A1*(1.D0-E)/((1.D0-E*DSIN(PHIM*RADD)**2.D0)**(3.D0/2.D0))
      DELLAM=S2LAM-S1LAM
      DELPHI=PHI2-PHI1
      A=(DSIN(PHIM*RADD)*DTAN(DELLAM*RADD/2.D0))/DCOS(DELPHI*RADD/2.D0)
      B=DATAN(A)
      C=DSIN(RMM*DELPHI*RADD/(2.D0*RNM))
      D=DCOS(PHIM*RADD)*DTAN(DELLAM*RADD/2.D0)/C
C-------COMPUTE DECIMAL AZIMUTH--------------------------------------C
      AZ=(DATAN(D)-B)/RADD
      AZ=180.D0+(AZ)
C-------CONVERT DECIMAL AZIMUTH TO DEGREES, MINUTES, SECONDS---------C
      IDEG=DINT(AZ)
      AZMIN=(AZ-IDEG)*60.D0
      MIN=DINT((AZ-IDEG)*60.D0)
      SEC=(AZMIN-MIN)*60.D0
C-------WRITE THE OUTPUT---------------------------------------------C
      WRITE(0,14) IDEG,MIN,SEC
14    FORMAT(' ','AZIMUTH= ',I5.0,1X,I5.0,1X,F9.3)
      STOP
      END

      
