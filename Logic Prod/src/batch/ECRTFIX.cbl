00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 ECRTFIX.                             00000030
00004 *                            VMOD=2.001.                          00000031
00005                                                                   00000050
00006  AUTHOR.        LOGIC, INC.                                       00000060
00007                 DALLAS, TEXAS.                                    00000070
00008                                                                   00000080
00009  DATE-COMPILED.                                                   00000090
00010  SKIP1                                                            00000100
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
00021  REMARKS.                                                         00000210
00022          ALL PURPOSE CERT FIX PROGRAM                             00000220
00023                                                                   00000230
00024  ENVIRONMENT DIVISION.                                            00000240
00025  INPUT-OUTPUT SECTION.                                            00000250
00026  FILE-CONTROL.                                                    00000260
00027                                                                   00000270
00028      SELECT  SORT-FILE     ASSIGN TO 6 SYS001-UT-2314-S-SORTWK1.  00000280
00029      SELECT  CERT-IN       ASSIGN TO SYS010-UT-2400-S-SYS010.     00000290
00030      SELECT  CERT-OT       ASSIGN TO SYS011-UT-2400-S-SYS011.     00000300
00031  EJECT                                                            00000310
00032  DATA DIVISION.                                                   00000320
00033  FILE SECTION.                                                    00000330
00034                                                                   00000340
00035  SD  SORT-FILE                                                    00000350
00036      RECORDING MODE IS F                                          00000360
00037      RECORD CONTAINS 700 CHARACTERS.                              00000361
00038                                                                   00000362
00039  01  SORT-REC.                                                    00000363
00040      12  FILLER              PIC  X(3).                           00000364
00041      12  SORT-KEY            PIC  X(36).                          00000365
00042      12  FILLER              PIC  X(661).                         00000366
00043  EJECT                                                            00000367
00044  FD  CERT-IN                                                      00000368
00045      RECORDING MODE IS F                                          00000369
00046      LABEL RECORDS ARE STANDARD                                   00000370
00047      BLOCK CONTAINS 0 RECORDS                                     00000371
00048      RECORD CONTAINS 700 CHARACTERS.                              00000372
00049                                                                   00000373
00050  01  CERT-IN-REC             PIC  X(700).                         00000374
00051  EJECT                                                            00000375
00052  FD  CERT-OT                                                      00000376
00053      RECORDING MODE IS F                                          00000377
00054      LABEL RECORDS ARE STANDARD                                   00000378
00055      BLOCK CONTAINS 0 RECORDS                                     00000379
00056      RECORD CONTAINS 700 CHARACTERS.                              00000380
00057                                                                   00000381
00058  01  CERT-OT-REC            PIC  X(700).                          00000382
00059  EJECT                                                            00000383
00060  WORKING-STORAGE SECTION.                                         00000384
00061  77  FILLER  PIC  X(32) VALUE '********************************'. 00000385
00062  77  FILLER  PIC  X(32) VALUE '   ECRTFIX   WORKING-STORAGE    '. 00000386
00063  77  FILLER  PIC  X(32) VALUE '*****VMOD=2.001*****************'. 00000387
00064                                                                   00000388
00065  77  CERT-IN-CTR             PIC  9(7)           VALUE ZERO.      00000389
00066  77  CERT-FX-CTR             PIC  9(7)           VALUE ZERO.      00000390
00067  77  CERT-OT-CTR             PIC  9(7)           VALUE ZERO.      00000391
00068                                                                   00000392
00069  01  WS.                                                          00000393
00070      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      00000394
00071      12  WS-ZERO                 PIC S9          VALUE ZERO.      00000395
00072      12  WS-ABEND-MESSAGE        PIC  X(80)      VALUE SPACES.    00000396
00073      12  WS-ABEND-FILE-STATUS    PIC  XX         VALUE ZERO.      00000397
00074  EJECT                                                            00000398
00075                              COPY ECSCRT01.                       00000399
00076  EJECT                                                            00000400
00077  PROCEDURE DIVISION.                                              00000401
00078                                                                   00000402
00079  0000-INITIALIZE SECTION.                                         00000403
00080      OPEN INPUT  CERT-IN.                                         00000404
00081                                                                   00000810
00082  0100-SORT-ROUTINE.                                               00000811
00083      SORT SORT-FILE  ON ASCENDING KEY  SORT-KEY                   00000812
00084          INPUT PROCEDURE 0200-INPUT-ROUTINE  THRU  3999-EXIT      00000813
00085          OUTPUT PROCEDURE 4000-RETURN-CERT  THRU  5999-EXIT.      00000814
00086                                                                   00000815
00087      IF SORT-RETURN  IS NOT EQUAL TO  ZEROS                       00000816
00088          MOVE '0101'             TO  WS-RETURN-CODE               00000817
00089          MOVE 'BAD SORT RETURN CODE'                              00000818
00090                                  TO  WS-ABEND-MESSAGE             00000819
00091          GO TO 9999-ABORT.                                        00000820
00092                                                                   00000821
00093      GO TO 9000-END-OF-JOB.                                       00000822
00094  EJECT                                                            00000823
00095  0200-INPUT-ROUTINE SECTION.                                      00000824
00096                                                                   00000825
00097  0205-READ-CERT.                                                  00000826
00098      READ CERT-IN  INTO  CERTIFICATE-RECORD  AT END               00000827
00099          CLOSE  CERT-IN                                           00000828
00100          GO TO 3999-EXIT.                                         00000829
00101                                                                   00000830
00102      ADD 1                       TO  CERT-IN-CTR.                 00000831
00103                                                                   00000832
00104 *** ALL CODE NEEDED FOR FIXING CERT MASTER GOES BETWEEN HERE      00000833
00105 *** AND 3000-RELEASE-CERT PARAGRAPH                               00000834
00106                                                                   00001060
00107      IF CR-FULL-CONTROL = '1000001PA00000014508511150000545182 '  00001061
00108          ADD 1                   TO  CERT-FX-CTR                  00001062
00109          MOVE '1'                TO  CR-LF-STATUS-AT-CANCEL       00001063
00110          MOVE '8'                TO  CR-LF-CURRENT-STATUS         00001064
00111          MOVE CR-LFPRM           TO  CR-LFRFND                    00001065
00112          ADD CR-LFPRM-ALT        TO  CR-LFRFND                    00001066
00113          MOVE CR-LFPRM-CALC      TO  CR-LFRFND-CALC               00001067
00114          ADD CR-LFPRM-CALC-ALT   TO  CR-LFRFND-CALC.              00001068
00115                                                                   00001069
00116      PERFORM 3000-RELEASE-CERT  THRU  3099-EXIT.                  00001070
00117                                                                   00001071
00118      GO TO 0205-READ-CERT.                                        00001072
00119                                                                   00001073
00120  3000-RELEASE-CERT.                                               00001074
00121      RELEASE SORT-REC  FROM  CERTIFICATE-RECORD.                  00001075
00122                                                                   00001220
00123  3099-EXIT.                                                       00001221
00124      EXIT.                                                        00001222
00125                                                                   00001250
00126  3999-EXIT.                                                       00001251
00127      EXIT.                                                        00001270
00128  EJECT                                                            00001271
00129  4000-RETURN-CERT SECTION.                                        00001272
00130      OPEN OUTPUT  CERT-OT.                                        00001273
00131                                                                   00001274
00132  4005-RETURN-REC.                                                 00001275
00133      RETURN SORT-FILE  INTO  CERTIFICATE-RECORD  AT END           00001276
00134          GO TO 5999-EXIT.                                         00001277
00135                                                                   00001278
00136  5000-WRITE-CERT.                                                 00001279
00137      WRITE CERT-OT-REC  FROM  CERTIFICATE-RECORD.                 00001280
00138                                                                   00001380
00139      ADD 1                       TO  CERT-OT-CTR.                 00001381
00140                                                                   00001382
00141      GO TO 4005-RETURN-REC.                                       00001383
00142                                                                   00001384
00143  5999-EXIT.                                                       00001385
00144      EXIT.                                                        00001386
00145  EJECT                                                            00001387
00146  6000-OUT-SORT SECTION.                                           00001388
00147                                                                   00001389
00148  9000-END-OF-JOB.                                                 00001390
00149      DISPLAY CERT-IN-CTR ' = CERT MASTER RECORDS READ'.           00001391
00150      DISPLAY CERT-FX-CTR ' = CERT MASTER RECORDS FIXED'.          00001392
00151      DISPLAY CERT-OT-CTR ' = CERT MASTER RECORDS WRITTEN'.        00001393
00152                                                                   00001394
00153      CLOSE CERT-OT.                                               00001395
00154                                                                   00001396
00155      GOBACK.                                                      00001397
00156                                                                   00001398
00157  9999-ABORT.                                                      00001399
00158                                                                   00001400
00159  ABEND-PGM  SECTION.                                              00001401
00160                              COPY ELCABEND.                       00001402
