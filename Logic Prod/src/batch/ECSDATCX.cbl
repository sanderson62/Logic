00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 ECSDATCX.                            00000030
00004 *                            VMOD=2.001                           00000031
00005                                                                   00000050
00006  AUTHOR.     LOGIC, INC.                                          00000060
00007              DALLAS, TEXAS.                                       00000070
00008                                                                   00000080
00009  DATE-COMPILED.                                                   00000090
00010                                                                   00000100
00011  SECURITY.   *****************************************************00000110
00012              *                                                   *00000120
00013              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *00000130
00014              *                                                   *00000140
00015              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *00000150
00016              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *00000160
00017              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *00000170
00018              *                                                   *00000180
00019              *****************************************************00000190
00020                                                                   00000200
00021  REMARKS.    *****************************************************00000210
00022              *                                                   *00000220
00023              *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *00000230
00024              *    OPTION SPECIFIED, CONVERT A GIVEN DATE INTO    *00000240
00025              *    THE FOLLOWING:  I. EDITED GREGORIAN (MM/DD/YY) *00000250
00026              *                   II. Y-M-D GREGORIAN  (YYMMDD)   *00000260
00027              *                  III. M-D-Y GREGORIAN  (MMDDYY)   *00000270
00028              *                   IV. ALPHA GREG  (MMM DD, YYYY)  *00000280
00029              *                    V. JULIAN  (YYDDD)             *00000290
00030              *                   VI. TOTAL DAYS SINCE 1900.      *00000300
00031              *    ADDITIONALLY, IF TWO DATES (EXPRESSED AS DAYS) *00000310
00032              *    ARE GIVEN, THE ELAPSED MONTHS AND DAYS CAN BE  *00000320
00033              *    CALCULATED.                                    *00000330
00034              *                                                   *00000340
00035              *****************************************************00000350
00036  ENVIRONMENT DIVISION.                                            00000360
00037                                                                   00000370
00038  DATA DIVISION.                                                   00000380
00039      EJECT                                                        00000390
00040  WORKING-STORAGE SECTION.                                         00000400
00041  77  FILLER   PIC X(32) VALUE '********************************'. 00000410
00042  77  FILLER   PIC X(32) VALUE '** ECSDATCX WORKING STORAGE   **'. 00000420
00043  77  FILLER  PIC X(32)   VALUE '*********** VMOD=2.001 *********'.00000430
00044                                                                   00000440
00045                              COPY ECSDATW1.                       00000441
00046                                                                   00000460
00047      EJECT                                                        00000470
00048  LINKAGE SECTION.                                                 00000480
00049                                                                   00000490
00050                              COPY ECSDATL1.                       00000491
00051                                                                   00000510
00052      EJECT                                                        00000520
00053  PROCEDURE DIVISION USING DATE-CONVERSION-DATA.                   00000530
00054                                                                   00000540
00055  0100-PROGRAM-PROCESSING SECTION. COPY ECSDATP1.                  00000550
00056                                                                   00000560
00057  0100-EXIT.                                                       00000570
00058                                                                   00000580
00059      GOBACK.                                                      00000590
00060                                                                   00000600
00061      EJECT                                                        00000610
00062  0200-DATE-CONVERSION-ROUTINES SECTION. COPY ECSDATP2.            00000620
00063                                                                   00000630
