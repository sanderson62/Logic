00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 EACTFIX.                             00000030
00004 *                            VMOD=2.002.                          00000031
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
00022          THIS PROGRAM IS TO FIX THE ECS ACCOUNT MASTER.           00000220
00023                                                                   00000230
00024  ENVIRONMENT DIVISION.                                            00000240
00025  INPUT-OUTPUT SECTION.                                            00000250
00026  FILE-CONTROL.                                                    00000260
00027                                                                   00000270
00028      SELECT SORT-FILE        ASSIGN TO SYS001-UT-2314-S-SORTWK1.  00000271
00029      SELECT ACCT-IN          ASSIGN TO SYS011-UT-2400-S-SYS011.   00000290
00030      SELECT ACCT-OT          ASSIGN TO SYS013-UT-2400-S-SYS013.   00000300
00031  EJECT                                                            00000310
00032  DATA DIVISION.                                                   00000320
00033  FILE SECTION.                                                    00000330
00034                                                                   00000340
00035  SD  SORT-FILE                                                    00000350
00036      RECORDING MODE IS F.                                         00000351
00037                                                                   00000352
00038  01  SORT-REC.                                                    00000353
00039      12  FILLER              PIC  XX.                             00000354
00040      12  SORT-KEY            PIC  X(26).                          00000355
00041      12  FILLER              PIC  X(88).                          00000356
00042      12  SORT-NAME           PIC  X(30).                          00000357
00043      12  FILLER              PIC  X(1854).                        00000358
00044                                                                   00000359
00045  FD  ACCT-IN                                                      00000450
00046      RECORDING MODE IS F                                          00000460
00047      LABEL RECORDS ARE STANDARD                                   00000470
00048      BLOCK CONTAINS 0 RECORDS                                     00000471
00049      RECORD CONTAINS 2000 CHARACTERS.                             00000472
00050                                                                   00000473
00051  01  ACCT-IN-REC             PIC  X(2000).                        00000474
00052                                                                   00000475
00053  FD  ACCT-OT                                                      00000476
00054      RECORDING MODE IS F                                          00000477
00055      LABEL RECORDS ARE STANDARD                                   00000478
00056      BLOCK CONTAINS 0 RECORDS                                     00000479
00057      RECORD CONTAINS 2000 CHARACTERS.                             00000480
00058                                                                   00000481
00059  01  ACCT-OT-REC             PIC  X(2000).                        00000482
00060  EJECT                                                            00000483
00061  WORKING-STORAGE SECTION.                                         00000484
00062  77  FILLER  PIC  X(32) VALUE '********************************'. 00000485
00063  77  FILLER  PIC  X(32) VALUE '*           EACTFIX            *'. 00000486
00064  77  FILLER  PIC  X(32) VALUE '************V/M=2.002***********'. 00000487
00065                                                                   00000488
00066  77  SUB                     PIC S9(3)   COMP    VALUE +0.        00000489
00067  77  ACCT-IN-CTR             PIC  9(7)           VALUE ZERO.      00000490
00068  77  ACCT-DR-CTR             PIC  9(7)           VALUE ZERO.      00000491
00069  77  ACCT-FX-CTR             PIC  9(7)           VALUE ZERO.      00000492
00070  77  ACCT-OT-CTR             PIC  9(7)           VALUE ZERO.      00000493
00071                                                                   00000494
00072  01  WS.                                                          00000495
00073      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      00000496
00074      12  WS-ZERO                 PIC S9          VALUE ZERO.      00000497
00075      12  WS-ABEND-MESSAGE        PIC  X(80)      VALUE SPACES.    00000498
00076      12  WS-ABEND-FILE-STATUS    PIC  XX         VALUE ZERO.      00000499
00077                                                                   00000500
00078      12  PREV-NAME               PIC X(30)       VALUE SPACES.    00000501
00079      12  PREV-ACCT-KEY           PIC X(19)       VALUE SPACES.    00000502
00080                                                                   00000503
00081  EJECT                                                            00000504
00082                              COPY ERCACCT.                        00000505
00083  EJECT                                                            00000506
00084  PROCEDURE DIVISION.                                              00000507
00085                                                                   00000850
00086  0000-INITIALIZE  SECTION.                                        00000851
00087      OPEN INPUT  ACCT-IN.                                         00000852
00088                                                                   00000853
00089  0100-SORT-ROUTINE1.                                              00000854
00090      SORT SORT-FILE  ON ASCENDING KEY  SORT-NAME                  00000855
00091                                        SORT-KEY                   00000856
00092          INPUT PROCEDURE 0200-INPUT-ROUTINE  THRU  3999-EXIT      00000857
00093          OUTPUT PROCEDURE 4000-RETURN-ACCT  THRU  5999-EXIT.      00000858
00094                                                                   00000859
00095      IF SORT-RETURN  IS NOT EQUAL TO  ZEROS                       00000860
00096          MOVE '0101'             TO  WS-RETURN-CODE               00000861
00097          MOVE 'BAD SORT RETURN CODE '                             00000862
00098                                  TO  WS-ABEND-MESSAGE             00000863
00099          GO TO 9999-ABORT.                                        00000864
00100                                                                   00001000
00101      GO TO 9000-END-OF-JOB.                                       00001001
00102  EJECT                                                            00001002
00103  0200-INPUT-ROUTINE  SECTION.                                     00001003
00104                                                                   00001004
00105  0205-READ-ACCT.                                                  00001005
00106      READ ACCT-IN  INTO  ACCOUNT-MASTER  AT END                   00001006
00107          CLOSE  ACCT-IN                                           00001007
00108          GO TO 3999-EXIT.                                         00001008
00109      ADD 1                       TO  ACCT-IN-CTR.                 00001009
00110                                                                   00001010
00111 *** ALL CODE NEEDED FOR FIXING ACCT MASTER GOES BETWEEN HERE      00001011
00112 *** AND 3000-RELEASE-ACCT PARAGRAPH                               00001012
00113                                                                   00001130
00114 *    IF AM-STATE = '05'   MOVE 'CA' TO AM-STATE.                  00001131
00115 *    IF AM-STATE = '17'   MOVE 'KY' TO AM-STATE.                  00001132
00116 *    IF AM-STATE = '20'   MOVE 'MD' TO AM-STATE.                  00001133
00117 *    IF AM-STATE = '22'   MOVE 'MI' TO AM-STATE.                  00001134
00118 *    IF AM-STATE = '28'   MOVE 'NV' TO AM-STATE.                  00001135
00119 *    IF AM-STATE = '35'   MOVE 'OH' TO AM-STATE.                  00001136
00120 *    IF AM-STATE = '38'   MOVE 'PA' TO AM-STATE.                  00001137
00121 *    IF AM-STATE = '46'   MOVE 'VA' TO AM-STATE.                  00001138
00122 *                                                                 00001139
00123 *    MOVE AM-PREV-EXP-DT         TO AM-EXPIRE-DT.                 00001140
00124                                                                   00001141
00125 *    IF AM-CARRIER EQUAL 'N' OR 'S' OR 'F'                        00001142
00126 *       MOVE 'NCB'               TO AM-REI-TABLE                  00001143
00127 *       MOVE 'Y'                 TO AM-RECALC-REIN                00001144
00128 *       ADD 1                    TO  ACCT-FX-CTR.                 00001145
00129                                                                   00001290
00130      PERFORM 3000-RELEASE-ACCT  THRU  3099-EXIT.                  00001291
00131                                                                   00001292
00132      GO TO 0205-READ-ACCT.                                        00001293
00133                                                                   00001330
00134  3000-RELEASE-ACCT.                                               00001331
00135      RELEASE SORT-REC  FROM  ACCOUNT-MASTER.                      00001332
00136                                                                   00001360
00137  3099-EXIT.                                                       00001361
00138      EXIT.                                                        00001362
00139                                                                   00001363
00140  3999-EXIT.                                                       00001364
00141      EXIT.                                                        00001365
00142  EJECT                                                            00001366
00143  4000-RETURN-ACCT  SECTION.                                       00001367
00144      OPEN OUTPUT  ACCT-OT.                                        00001368
00145                                                                   00001450
00146  4005-RETURN-REC.                                                 00001451
00147      RETURN SORT-FILE  INTO  ACCOUNT-MASTER  AT END               00001452
00148          GO TO 5999-EXIT.                                         00001453
00149                                                                   00001454
00150      IF AM-NAME = PREV-NAME  AND                                  00001455
00151         AM-CONTROL-A = PREV-ACCT-KEY                              00001456
00152          GO TO 4005-RETURN-REC.                                   00001457
00153                                                                   00001530
00154      MOVE AM-NAME                TO PREV-NAME.                    00001531
00155      MOVE AM-CONTROL-A           TO PREV-ACCT-KEY.                00001532
00156                                                                   00001533
00157  5000-WRITE-ACCT.                                                 00001534
00158      WRITE ACCT-OT-REC  FROM  ACCOUNT-MASTER.                     00001535
00159                                                                   00001536
00160      ADD 1                       TO  ACCT-OT-CTR.                 00001537
00161                                                                   00001538
00162      GO TO 4005-RETURN-REC.                                       00001539
00163                                                                   00001540
00164  5999-EXIT.                                                       00001541
00165      EXIT.                                                        00001542
00166  EJECT                                                            00001543
00167  6000-OUT-SORT  SECTION.                                          00001544
00168                                                                   00001545
00169  9000-END-OF-JOB.                                                 00001546
00170      DISPLAY ACCT-IN-CTR ' = ACCOUNT MASTERS READ'.               00001547
00171      DISPLAY ACCT-OT-CTR ' = ACCOUNT MASTERS WRITTEN'.            00001548
00172      DISPLAY ACCT-FX-CTR ' = ACCOUNT MASTERS FIXED'.              00001549
00173      DISPLAY ACCT-DR-CTR ' = ACCOUNT MASTERS DROPPED'.            00001550
00174                                                                   00001551
00175      CLOSE ACCT-OT.                                               00001552
00176                                                                   00001553
00177      GOBACK.                                                      00001554
00178                                                                   00001555
00179  9999-ABORT.                                                      00001556
00180                                                                   00001557
00181  ABEND-PGM  SECTION.                                              00001558
00182                                  COPY ELCABEND.                   00001559
