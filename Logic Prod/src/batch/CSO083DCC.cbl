00001  IDENTIFICATION DIVISION.                                         03/09/98
00002                                                                   ECS083
00003  PROGRAM-ID.                CSO083.                                  LV002
00004 *              PROGRAM CONVERTED BY                               ECS083
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS083
00006 *              CONVERSION DATE 02/08/96 18:42:05.                 ECS083
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS083
00008 *                           VMOD=2.008.                           ECS083
00009 *AUTHOR.     LOGIC, INC.                                          ECS083
00010 *            DALLAS, TEXAS.                                       ECS083
00011                                                                   ECS083
00012 *DATE-COMPILED.                                                   ECS083
00013                                                                   ECS083
000009                                                                  00000009
000010 SECURITY.   *****************************************************00000010
000011             *                                                   *00000011
000012             *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *00000012
000013             *                                                   *00000013
000014             *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *00000014
000015             *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *00000015
000016             *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *00000016
000017             *                                                   *00000017
000018             *****************************************************00000018
000019                                                                  00000019
000020*REMARKS.                                                         00000020
000021******************************************************************00000021
000022* ****************************************************************00000022
000023*                                                                 00000023
000024* THIS PROGRAM IS A COPY OF LOGIC PROGRAM "ECS083". IT HAS BEEN   00000024
000025*  MODIFIED TO PROVIDE FINANCIAL AND ACTUARIAL DEPARTMENTS WITH   00000025
000026*  ADDITIONAL REPORTS FOR BALANCING MONTHLY PRODUCTION REPORTS.   00000026
000027*                                                                 00000027
000028* REGULAR "ECS083" PROCESSING IS CONTINUED WITH NO CHANGES.       00000028
000029*                                                                 00000029
000030* IT RUNS ONLY IN THE MONTH END PROCESSING. CURRENTLY IT RUNS IN  00000030
000031*  CILGM17 (AS OF 12-04-96), AND REPLACES "ECS083" IN THIS JOB.   00000031
000032*                                                                 00000032
000033* ****************************************************************00000033
000034******************************************************************00000034
000035*                                                                 00000035
000036*                                                                 00000036
000037*        EXTRACT UNEARNED PREMIUM AND COMMISSION REPORT RECORDS.  00000037
000038*        EXTRACT BY PROGRAM SWITCHES                              00000038
000039*            1 - STATE                                            00000039
000040*                COMPANY                                          00000040
000041*                STATE WITHIN CARRIER                             00000041
000042*                CARRIER                                          00000042
000043*                STATE OVERALL                                    00000043
000044*                FINAL TOTALS                                     00000044
000045*            2 - COMPANY                                          00000045
000046*                STATE WITHIN CARRIER                             00000046
000047*                CARRIER                                          00000047
000048*                STATE OVERALL                                    00000048
000049*                FINAL TOTALS                                     00000049
000050*            3 - STATE WITHIN CARRIER                             00000050
000051*                CARRIER                                          00000051
000052*                STATE OVERALL                                    00000052
000053*                FINAL TOTALS                                     00000053
000054*            4 - CARRIER                                          00000054
000055*                STATE OVERALL                                    00000055
000056*                FINAL TOTALS                                     00000056
000057*            5 - STATE OVERALL                                    00000057
000058*                FINAL TOTALS                                     00000058
000059*            6 - FINAL TOTALS                                     00000059
031102******************************************************************
031102*                   C H A N G E   L O G
031102*
031102* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031102*-----------------------------------------------------------------
031102*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031102* EFFECTIVE    NUMBER
031102*-----------------------------------------------------------------
061402* 061402    2002040100003  PEMA  ADD CODE FOR VA MEAN
040114* 040114  CR2011122200002  AJRA  MODIFY DOMICILE PREM, COMM, AND TAX
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
013119* 013119  CR2019020100002  PEMA  Fix bug when > 99 mort tables
031102******************************************************************
000060                                                                  00000060
000061 ENVIRONMENT DIVISION.                                            00000061
000062 INPUT-OUTPUT SECTION.                                            00000062
000063 FILE-CONTROL.                                                    00000063
000064                                                                  00000064
000065     SELECT REPTFL         ASSIGN TO SYS004-UT-FBA1-S-SYS004.     00000065
000066     SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     00000066
000067     SELECT GAAP-EXTR      ASSIGN TO SYS011-UT-2400-S-SYS011.     00000067
000068     SELECT ACT-RPT        ASSIGN TO SYS012-UR-1403-S-SYS012.     00000068
000070     SELECT DISK-DATE      ASSIGN TO SYS019-UT-FBA1-S-SYS019.     00000070
000071     SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     00000071
000072 EJECT                                                            00000072
000073 DATA DIVISION.                                                   00000073
000074 FILE SECTION.                                                    00000074
000075                                                                  00000075
000076 FD  ACT-RPT                                                      00000076
000077     RECORDING MODE F                                             00000077
000080     BLOCK CONTAINS 0 RECORDS.                                    00000080
000082 01  ACT-REC.                                                     00000082
CIDMOD     12  ACT-CC              PIC X(01).                           00000083
000084     12  ACT-INFO            PIC X(132).                          00000084
000085                                                                  00000085
00062  FD  REPTFL                                                       ECS083
00063      BLOCK CONTAINS 0 RECORDS
00064      RECORDING MODE F.                                            ECS083
00065                                                                   ECS083
00066  01  RPT-REC.                                                     ECS083
013119     12  R-PARM              PIC X(27).                           ECS083
040114     12  FILLER              PIC X(130).                          ECS083
00069  EJECT                                                            ECS083
00070  FD  PRNTR                                                        ECS083
00071                              COPY ELCPRTFD.                       ECS083
00072  EJECT                                                            ECS083
00073  FD  GAAP-EXTR                                                    ECS083
00074                              COPY ECSGAPFD.                       ECS083
00075                                                                   ECS083
00076                              COPY ECSGAP01.                       ECS083
00077  EJECT                                                            ECS083
00078  FD  DISK-DATE                                                    ECS083
00079                              COPY ELCDTEFD.                       ECS083
00080  EJECT                                                            ECS083
00081  FD  FICH                                                         ECS083
00082                              COPY ECSFICH.                        ECS083
000097                                                                  00000097
000122 EJECT                                                            00000122
000123 WORKING-STORAGE SECTION.                                         00000123
000124 77  FILLER  PIC X(32) VALUE '********************************'.  00000124
000125 77  FILLER  PIC X(32) VALUE '     ECS083 WORKING STORAGE     '.  00000125
000126 77  FILLER  PIC X(32) VALUE '***** CLONED FROM VMOD=2.008 ***'.  00000126
000127                                                                  00000127
000128 77  AH-SUB                  PIC  999            VALUE ZEROS.     00000128
000129 77  CO-SAVE-SUB             PIC  999            VALUE ZEROS.     00000129
CIDMOD 77  GA-SAVE-SUB             PIC  999            VALUE ZEROS.     00000129
000130 77  CA-SAVE-SUB             PIC  999            VALUE ZEROS.     00000130
CIDMOD 77  OH-SAVE-SUB             PIC  999            VALUE ZEROS.     00000130
000131 77  OR-SAVE-SUB             PIC  999            VALUE ZEROS.     00000131
CIDMOD 77  TX-SAVE-SUB             PIC  999            VALUE ZEROS.     00000131
000132 77  WA-SAVE-SUB             PIC  999            VALUE ZEROS.     00000132
CIDMOD 77  CA-OH-OR-TX-WA-PRAT     PIC  9(11)V99       VALUE ZEROS.     00000133
CIDMOD 77  CA-OH-OR-TX-WA-R78      PIC  9(11)V99       VALUE ZEROS.     00000134
000135 77  TOT-CO-PRAT             PIC  9(11)V99       VALUE ZEROS.     00000135
CIDMOD 77  TOT-GA-PRAT             PIC  9(11)V99       VALUE ZEROS.     00000135
000136 77  TOT-CO-R78              PIC  9(11)V99       VALUE ZEROS.     00000136
CIDMOD 77  TOT-GA-R78              PIC  9(11)V99       VALUE ZEROS.     00000136
000137 77  CO-OVRALL-PRAT          PIC  9(11)V99       VALUE ZEROS.     00000137
061402 77  VA-OVRALL-PRAT          PIC  9(11)V99       VALUE ZEROS.     00000137
CIDMOD 77  GA-OVRALL-PRAT          PIC  9(11)V99       VALUE ZEROS.     00000137
000138 77  CO-OVRALL-R78           PIC  9(11)V99       VALUE ZEROS.     00000138
061402 77  VA-OVRALL-R78           PIC  9(11)V99       VALUE ZEROS.     00000138
CIDMOD 77  GA-OVRALL-R78           PIC  9(11)V99       VALUE ZEROS.     00000138
000139 77  COLO-TOT                PIC  9(11)V99       VALUE ZEROS.     00000139
000140 77  COLO-MEAN               PIC  9(11)V99       VALUE ZEROS.     00000140
000141 77  COLO-UNEARNED           PIC  9(11)V99       VALUE ZEROS.     00000141
061402 77  VA-TOT                  PIC  9(11)V99       VALUE ZEROS.     00000139
061402 77  VA-MEAN                 PIC  9(11)V99       VALUE ZEROS.     00000140
061402 77  VA-UNEARNED             PIC  9(11)V99       VALUE ZEROS.     00000141
CIDMOD 77  GA-TOT                  PIC  9(11)V99       VALUE ZEROS.     00000139
CIDMOD 77  GEORGIA-MEAN            PIC  9(11)V99       VALUE ZEROS.     00000140
CIDMOD 77  GA-UNEARNED             PIC  9(11)V99       VALUE ZEROS.     00000141
CIDMOD 77  OH-OVRALL-R78           PIC  9(11)V99       VALUE ZEROS.     00000138
CIDMOD 77  OH-OVRALL-PRAT          PIC  9(11)V99       VALUE ZEROS.     00000137
CIDMOD 77  OH-TOT                  PIC  9(11)V99       VALUE ZEROS.     00000139
CIDMOD 77  OHIO-MEAN               PIC  9(11)V99       VALUE ZEROS.     00000140
CIDMOD 77  TX-OVRALL-R78           PIC  9(11)V99       VALUE ZEROS.     00000138
CIDMOD 77  TX-OVRALL-PRAT          PIC  9(11)V99       VALUE ZEROS.     00000137
CIDMOD 77  TX-TOT                  PIC  9(11)V99       VALUE ZEROS.     00000139
CIDMOD 77  TEXAS-MEAN              PIC  9(11)V99       VALUE ZEROS.     00000140
000142 77  A-WORK-GRAND-TOT        PIC  9(11)V99       VALUE ZEROS.     00000142
000143 77  A-WORK-FINAL-TOT        PIC  9(11)V99       VALUE ZEROS.     00000143
000144 77  TOT-PRAT                PIC  9(11)V99       VALUE ZEROS.     00000144
000145 77  TOT-R78                 PIC  9(11)V99       VALUE ZEROS.     00000145
000146 77  TOT-CNT                 PIC  9(11)          VALUE ZEROS.     00000146
000147 77  CO-COUNT                PIC  9(11)          VALUE ZEROS.     00000147
CIDMOD 77  GA-COUNT                PIC  9(11)          VALUE ZEROS.     00000147
000148 77  CA-COUNT                PIC  9(11)          VALUE ZEROS.     00000148
CIDMOD 77  OH-COUNT                PIC  9(11)          VALUE ZEROS.     00000148
000149 77  OR-COUNT                PIC  9(11)          VALUE ZEROS.     00000149
CIDMOD 77  TX-COUNT                PIC  9(11)          VALUE ZEROS.     00000149
000150 77  WA-COUNT                PIC  9(11)          VALUE ZEROS.     00000150
000151 77  PGM-SUB                 PIC S999    COMP    VALUE +083.      00000151
000152 77  X1                      PIC S9(4)   COMP.                    00000152
000153 77  X2                      PIC S9(4)   COMP.                    00000153
000154 77  X3                      PIC S9(4)   COMP.                    00000154
000155 77  MAX-BEN                 PIC S9(4)   COMP    VALUE +100.      00000155
000156 77  X                       PIC X               VALUE SPACE.     00000156
CIDMOD*77  X-YR                    PIC 99              VALUE ZEROS.     00000157
CIDMOD 77  WORK-YR                 PIC S9999           VALUE ZEROS.     00000157
000158 77  DIS-EDIT                PIC $ZZ,ZZZ,ZZZ,ZZZ.99.              00000158
000159 77  REC-CNT-EDIT            PIC  ZZ,ZZZ,ZZZ,ZZ9.                 00000159
CIDMOD 77  CON-19961231            PIC  9(11)          VALUE 19961231.  00000149
061402 77  CON-20020630            PIC  9(11)          VALUE 20020630.  00000149
040114 77  WRK-DOMI                PIC S9(9)V99 COMP-3 VALUE +0.
000160                                                                  00000160
000161 SKIP2                                                            00000161
000162                                                                  00000162
000163                                                                  00000163
000164 01 STATE-ACCUMS.
           12  SA-INDEX                   PIC S9(3) COMP-3 VALUE +0.
           12  STATE-TABLE OCCURS 75.
               16  SA-STATE-CODE          PIC XX.
               16  SA-LT13-LF             PIC S9(11)V99 COMP-3.
               16  SA-LT13-LFC            PIC S9(11)V99 COMP-3.
               16  SA-LT13-AH             PIC S9(11)V99 COMP-3.
               16  SA-LT13-AHC            PIC S9(11)V99 COMP-3.
               16  SA-LT13-TOT            PIC S9(11)V99 COMP-3.
               16  SA-GT12-LF             PIC S9(11)V99 COMP-3.
               16  SA-GT12-LFC            PIC S9(11)V99 COMP-3.
               16  SA-GT12-AH             PIC S9(11)V99 COMP-3.
               16  SA-GT12-AHC            PIC S9(11)V99 COMP-3.
               16  SA-GT12-TOT            PIC S9(11)V99 COMP-3.
               16  SA-ALL-TOT             PIC S9(11)V99 COMP-3.
000337                                                                  00000336
000338 01  NEW-RPT.                                                     00000337
000339     05  NR-CC           PIC X(01)     VALUE SPACES.              00000338
000340     05  NR-INFO         PIC X(50)     VALUE SPACES.              00000339
000341     05  FILLER          PIC X(01)     VALUE SPACES.              00000340
000342     05  PAREN           PIC X(01)     VALUE SPACES.              00000341
000343     05  NR-AMT          PIC X(50)     VALUE SPACES.              00000342
000344     05  FILLER          PIC X(29)     VALUE SPACES.              00000343
000345                                                                  00000344
000346                                                                  00000345
000347 01  NEW-RPT-LINES.                                               00000346
000348                                                                  00000347
000349     05  DCC-HEAD1.                                               00000348
000350         10  FILLER     PIC X(5)      VALUE SPACES.               00000349
000351         10  FILLER     PIC X(43)     VALUE                       00000350
000352           'STATE          < 13                        '.
000351         10  FILLER     PIC X(36)     VALUE                       00000350
000352           '               > 12                 '.
               10  FILLER     PIC X(20)     VALUE
                 '       ALL       '.

000349     05  DCC-HEAD2.                                               00000348
000350         10  FILLER     PIC X(5)      VALUE SPACES.               00000349
000351         10  FILLER     PIC X(43)     VALUE                       00000350
000352           '        LIFE         AH         TOTAL      '.
000351         10  FILLER     PIC X(36)     VALUE                       00000350
000352           '   LIFE        AH          TOTAL    '.
               10  FILLER     PIC X(20)     VALUE
                 '       TERMS     '.

           05  DCC-DETAIL.
               10  FILLER              PIC X(7) VALUE SPACES.
               10  DCC-STATE           PIC XX.
               10  FILLER              PIC XX.
               10  DCC-LT13-LF         PIC ZZZ,ZZ9.99 VALUE ZEROS.
               10  FILLER              PIC XX VALUE SPACES.
               10  DCC-LT13-AH         PIC ZZZ,ZZ9.99 VALUE ZEROS.
               10  FILLER              PIC XX VALUE SPACES.
               10  DCC-LT13-TOT        PIC ZZZ,ZZ9.99 VALUE ZEROS.
               10  FILLER              PIC XXX VALUE SPACES.
               10  DCC-GT12-LF         PIC ZZZ,ZZ9.99 VALUE ZEROS.
               10  FILLER              PIC XX VALUE SPACES.
               10  DCC-GT12-AH         PIC ZZZ,ZZ9.99 VALUE ZEROS.
               10  FIILER              PIC XX VALUE SPACES.
               10  DCC-GT12-TOT        PIC ZZZ,ZZ9.99 VALUE ZEROS.
               10  FILLER              PIC X(4) VALUES SPACES.
               10  DCC-ALL-TOT         PIC ZZZ,ZZ9.99 VALUE ZEROS.
               
                   
000399                                                                  00000398
000400                                                                  00000399
000411                                                                  00000410
000412 01  GET-MONTH.                                                   00000411
000413     05  PULL-MONTH.                                              00000412
000414         10  FIRST4           PIC X(08)     VALUE SPACES.         00000413
000415         10  LAST-14          PIC X(10)     VALUE SPACES.         00000414
000419                                                                  00000415
000420                                                                  00000416
000421 01  PRT-LINE.                                                    00000417
000422     05  PRT-CC           PIC X(01)     VALUE SPACES.             00000418
000423     05  PRT-INFO         PIC X(32)     VALUE SPACES.             00000419
000424     05  PRT-AMT          PIC X(30)     VALUE SPACES.             00000420
000425     05  FILLER           PIC X(69)     VALUE SPACES.             00000421
000428                                                                  00000422
000429                                                                  00000423
000430 01  DIS-RCD.                                                     00000424
000431     05  DIS-INFO         PIC X(35)     VALUE SPACES.             00000425
000432     05  DIS-AMT          PIC X(35)     VALUE SPACES.             00000426
000433                                                                  00000427
000434                                                                  00000428
000435 01  PRNT-TOTALS.                                                 00000429
000436     05  FILLER           PIC XX        VALUE SPACES.             00000430
000437     05  FILLER           PIC X(14)     VALUE '- OVERALL -   '.   00000431
000438     05  FILLER           PIC X(08)     VALUE 'COUNT = '.         00000432
000439     05  TOT-COUNT-P      PIC ZZ,ZZZ,ZZ9.                         00000433
000440     05  FILLER           PIC X(05)     VALUE SPACES.             00000434
000441     05  FILLER           PIC X(07)     VALUE 'R-78 = '.          00000435
000442     05  TOT-R78-P        PIC $ZZ,ZZZ,ZZZ,ZZZ.99.                 00000436
000443     05  FILLER           PIC X(05)     VALUE SPACES.             00000437
000444     05  FILLER           PIC X(07)     VALUE 'PRAT = '.          00000438
000445     05  TOT-PRAT-P       PIC $ZZ,ZZZ,ZZZ,ZZZ.99.                 00000439
000446                                                                  00000440
000447                                                                  00000441
000448 01  PRNT-CODES.                                                  00000442
000449     05  FILLER           PIC XX        VALUE SPACES.             00000443
000450     05  FILLER           PIC X(07)     VALUE 'CODE = '.          00000444
000451     05  BEN-CODE-P       PIC XX.                                 00000445
000452     05  FILLER           PIC X(05)     VALUE SPACES.             00000446
000453     05  FILLER           PIC X(08)     VALUE 'COUNT = '.         00000447
000454     05  BEN-COUNT-P      PIC ZZ,ZZZ,ZZ9.                         00000448
000455     05  FILLER           PIC X(05)     VALUE SPACES.             00000449
000456     05  FILLER           PIC X(07)     VALUE 'R-78 = '.          00000450
000457     05  BEN-R78-P        PIC $ZZ,ZZZ,ZZZ,ZZZ.99.                 00000451
000458     05  FILLER           PIC X(05)     VALUE SPACES.             00000452
000459     05  FILLER           PIC X(07)     VALUE 'PRAT = '.          00000453
000460     05  BEN-PRAT-P       PIC $ZZ,ZZZ,ZZZ,ZZZ.99.                 00000454
000461                                                                  00000455
000462 01  ALL-BEN-COUNT.                                               00000456
000463     05  FILLER               PIC X(08)  VALUE SPACES.            00000457
000464     05  ALL-BEN-COUNT-P      PIC X(10)  VALUE SPACES.            00000458
000503                                                                  00000495
000504 01  WORK-REC.                                                    00000496
000505     12  W-REIN              PIC X(6).                            00000497
000506     12  W-SEQ.                                                   00000498
000507         16  W-SEQ1.                                              00000499
000508             20  W-CARR      PIC X.                               00000500
000509             20  W-CO        PIC X(6).                            00000501
000510             20  W-ST        PIC XX.                              00000502
000511         16  W-SEQ2.                                              00000503
000512             20  W-YR        PIC 9999.                            00000504
000513             20  W-CODE      PIC 9.                               00000505
000514         16  W-SEQ3.                                              00000506
000515             20  W-LAH       PIC X.                               00000507
000516             20  W-BEN       PIC XX.                              00000508
013119             20  W-MORT      PIC 999.                             00000509
000518     12  W-FL                PIC X.                               00000510
000519     12  W-AMTS.                                                  00000511
000520         16  W-DUP           PIC S9(7)     COMP-3.                00000512
000521         16  W-COUNT         PIC S9(7)     COMP-3.                00000513
000522         16  W-WRITTEN       PIC S9(9)V99  COMP-3.                00000514
000523         16  W-P78           PIC S9(9)V99  COMP-3.                00000515
000524         16  W-PRATA         PIC S9(9)V99  COMP-3.                00000516
000525         16  W-DOMICILE      PIC S9(9)V99  COMP-3.                00000517
000526         16  W-STATE         PIC S9(9)V99  COMP-3.                00000518
000527         16  W-RESERV        PIC S9(9)V99  COMP-3.                00000519
000528         16  W-ALTRSV        PIC S9(9)V99  COMP-3.                00000520
000529         16  W-REMAIN        PIC S9(13)V99 COMP-3.                00000521
000530         16  W-PAID          PIC S9(9)V99  COMP-3.                00000522
000531         16  W-C78           PIC S9(9)V99  COMP-3.                00000523
000532         16  W-CRATA         PIC S9(9)V99  COMP-3.                00000524
040114         16  W-CDOMI         PIC S9(9)V99  COMP-3.
000533         16  W-TAX           PIC S9(9)V99  COMP-3.                00000525
000534         16  W-T78           PIC S9(9)V99  COMP-3.                00000526
000535         16  W-TRATA         PIC S9(9)V99  COMP-3.                00000527
040114         16  W-TDOMI         PIC S9(9)V99  COMP-3.
000536     12  W-M-AMTS.                                                00000528
000537         16  W-IUNDR         PIC S9(9)V99  COMP-3.                00000529
000538         16  W-IOVER         PIC S9(9)V99  COMP-3.                00000530
000539         16  W-GUNDR         PIC S9(9)V99  COMP-3.                00000531
000540         16  W-GOVER         PIC S9(9)V99  COMP-3.                00000532
000541 EJECT                                                            00000533
000542 01  MISC-WS.                                                     00000534
000543     12  WS-RETURN-CODE      PIC S9(4)   COMP.                    00000535
000544     12  WS-ABEND-MESSAGE    PIC X(80).                           00000536
000545     12  WS-ABEND-FILE-STATUS PIC XX     VALUE ZEROS.             00000537
000546     12  WS-ZERO             PIC S9       VALUE +0 COMP-3.        00000538
000547     12  CUR-SEQ.                                                 00000539
000548         16  CUR-CARR        PIC X.                               00000540
000549         16  CUR-CO          PIC X(6).                            00000541
000550         16  CUR-ST          PIC XX.                              00000542
000551     12  PRE-SEQ.                                                 00000543
000552         16  PRE-CARR        PIC X          VALUE LOW-VALUE.      00000544
000553         16  PRE-CO          PIC X(6)       VALUE LOW-VALUE.      00000545
000554         16  PRE-ST          PIC XX         VALUE LOW-VALUE.      00000546
000555     12  PRE-REIN            PIC X(6)       VALUE LOW-VALUE.      00000547
CIDMOD     12  X-YR                PIC 9(4)       VALUE ZEROS.          00000547
000556                                                                  00000548
000557 01  PASS-ONE-WORK.                                               00000549
000558     12  XT-TABLE.                                                00000550
000559         16  XT-YEARS        OCCURS 15 TIMES.                     00000551
000560             20  XT-SEQ      OCCURS 100 TIMES.                    00000552
000561                 24  XT-SEQ1     PIC XXX.                         00000553
013119                 24  XT-SEQ2     PIC XXX.
000563                 24  XT-SEQ3     PIC X(6).                        00000555
000564     12  YT-TABLE.                                                00000556
000565         16  YT-YEARS        OCCURS 15 TIMES.                     00000557
000566             20  YT-SEQ      OCCURS 100 TIMES.                    00000558
000567                 24  YT-SEQ1     PIC XXX.                         00000559
013119                 24  YT-SEQ2     PIC XXX.
000569                 24  YT-SEQ3     PIC X(6).                        00000561
000570     12  X-SEQ.                                                   00000562
000571         16  X-SEQ1.                                              00000563
000572             20  X-LAH           PIC X.                           00000564
000573             20  X-BEN           PIC XX.                          00000565
000574         16  X-SEQ2.                                              00000566
013119             20  X-MORT          PIC 999.
000576         16  X-SEQ3.                                              00000568
000577             20  X-REIN          PIC X(6).                        00000569
000578                                                                  00000570
000579 01  COMMON-TOTALS.                                               00000571
000580     12  X-DETL.                                                  00000572
000581         16  X-DUP               PIC S9(7)     COMP-3.            00000573
000582         16  X-COUNT             PIC S9(7)     COMP-3.            00000574
000583         16  X-WRITTEN           PIC S9(9)V99  COMP-3.            00000575
000584         16  X-P78               PIC S9(9)V99  COMP-3.            00000576
000585         16  X-PRATA             PIC S9(9)V99  COMP-3.            00000577
000586         16  X-DOMICILE          PIC S9(9)V99  COMP-3.            00000578
000587         16  X-STATE             PIC S9(9)V99  COMP-3.            00000579
000588         16  X-RESERV            PIC S9(9)V99  COMP-3.            00000580
000589         16  X-ALTRSV            PIC S9(9)V99  COMP-3.            00000581
000590         16  X-REMAIN            PIC S9(13)V99 COMP-3.            00000582
000591         16  X-PAID              PIC S9(9)V99  COMP-3.            00000583
000592         16  X-C78               PIC S9(9)V99  COMP-3.            00000584
000593         16  X-CRATA             PIC S9(9)V99  COMP-3.            00000585
040114         16  X-CDOMI             PIC S9(9)V99  COMP-3.
000594         16  X-TAX               PIC S9(9)V99  COMP-3.            00000586
000595         16  X-T78               PIC S9(9)V99  COMP-3.            00000587
000596         16  X-TRATA             PIC S9(9)V99  COMP-3.            00000588
040114         16  X-TDOMI             PIC S9(9)V99  COMP-3.
000597     12  X-M-DETL.                                                00000589
000598         16  X-IUNDR             PIC S9(9)V99  COMP-3.            00000590
000599         16  X-IOVER             PIC S9(9)V99  COMP-3.            00000591
000600         16  X-GUNDR             PIC S9(9)V99  COMP-3.            00000592
000601         16  X-GOVER             PIC S9(9)V99  COMP-3.            00000593
000602                                                                  00000594
000603 01  COMMON-TOTALS-1.                                             00000595
000604     12  X-TOTALS.                                                00000596
000605         16  X-YEARS         OCCURS 15 TIMES.                     00000597
000606             20  X-TYPES     OCCURS 100 TIMES.                    00000598
040114                 24  X-AMTS      PIC X(106).                      00000599
000608                                                                  00000600
000609 01  COMMON-TOTALS-2.                                             00000601
000610     12  Y-TOTALS.                                                00000602
000611         16  Y-YEARS         OCCURS 15 TIMES.                     00000603
000612             20  Y-TYPES     OCCURS 100 TIMES.                    00000604
040114                 24  Y-AMTS      PIC X(106).                      00000605
000614                                                                  00000606
000615 01  COMMON-TOTALS-3.                                             00000607
000616     12  X-FILLER1.                                               00000608
000617         16  X-FILLER3       OCCURS 15  TIMES.                    00000609
000618             20  X-FILLER4   OCCURS 100 TIMES.                    00000610
000619                 24  X-M-AMTS    PIC X(24).                       00000611
000620     12  Y-FILLER1.                                               00000612
000621         16  Y-FILLER3       OCCURS 15  TIMES.                    00000613
000622             20  Y-FILLER4   OCCURS 100 TIMES.                    00000614
000623                 24  Y-M-AMTS    PIC X(24).                       00000615
000624                                                                  00000616
000625                                                                  00000617
000626 01  HEAD-1.                                                      00000618
000627     12  FILLER              PIC X(42)           VALUE SPACES.    00000619
000628     12  FILLER              PIC X(39)           VALUE            00000620
000629             '      UNEARNED PREMIUM EXTRACT         '.           00000621
CIDMOD     12  FILLER              PIC X(38)           VALUE SPACES.    00000622
000631     12  FILLER              PIC X(7)            VALUE 'ECS-083'. 00000623
000632                                                                  00000624
000633 01  HEAD-2.                                                      00000625
000634     12  FILLER              PIC X(47)           VALUE SPACES.    00000626
000635     12  HD-CLIENT           PIC X(30)           VALUE SPACES.    00000627
CIDMOD     12  FILLER              PIC X(42)           VALUE SPACES.    00000628
000637     12  HD-DATE             PIC X(8)            VALUE SPACES.    00000629
000638                                                                  00000630
000639 01  HEAD-3.                                                      00000631
000640     12  FILLER              PIC X(53)           VALUE SPACES.    00000632
000641     12  HD-ALF-DTE          PIC X(18)           VALUE SPACES.    00000633
CIDMOD     12  FILLER              PIC X(48)           VALUE SPACES.    00000634
000643     12  FILLER              PIC X(5)            VALUE 'PAGE'.    00000635
000644     12  HD-PAGE             PIC ZZ,ZZZ.                          00000636
000645                                                                  00000637
000646                             COPY ELCDTECX.                       00000638
CIDMOD                                                                  00000637
CIDMOD                             COPY ELCDTEVR.                       00000638
CIDMOD                                                                  00000637
CIDMOD                             COPY ELCGAPVR.                       00000638
CIDMOD                                                                  00000637
000648 EJECT                                                            00000640
000649 PROCEDURE DIVISION.                                              00000641
000650                                                                  00000642
000651 0100-SET-START.                                                  00000643
000652                             COPY ELCDTERX.                       00000644
000653                                                                  00000645
000654 0110-OPEN-RTN.                                                   00000646
000655     OPEN INPUT  GAAP-EXTR                                        00000647
000656          OUTPUT REPTFL                                           00000648
000657                 ACT-RPT                                          00000649
000659                 PRNTR.                                           00000651
000660                                                                  00000652
000661     MOVE ZEROS TO AH-SUB.                                        00000653
000666                                                                  00000658
000700 0120-ZERO-LOOP-END.                                              00000690
000702                                                                  00000691
           MOVE +1                     TO CLAS-INDEXS
           PERFORM VARYING SA-INDEX FROM +1 BY +1 UNTIL
              (SA-INDEX > +75)
              IF SA-INDEX NOT > CLAS-MAXS
                 MOVE STATE-ABBR (SA-INDEX)
                                       TO SA-STATE-CODE (SA-INDEX)
              ELSE
                 MOVE SPACES           TO SA-STATE-CODE (SA-INDEX)
              END-IF
              MOVE +0                  TO SA-LT13-LF  (SA-INDEX)
                                          SA-LT13-AH  (SA-INDEX)
                                          SA-LT13-LFC (SA-INDEX)
                                          SA-LT13-AHC (SA-INDEX)
                                          SA-LT13-TOT (SA-INDEX)
                                          SA-GT12-LF  (SA-INDEX)
                                          SA-GT12-AH  (SA-INDEX)
                                          SA-GT12-LFC (SA-INDEX)
                                          SA-GT12-AHC (SA-INDEX)
                                          SA-GT12-TOT (SA-INDEX)
                                          SA-ALL-TOT  (SA-INDEX)
               ADD +1                  TO CLAS-INDEXS
           END-PERFORM
           MOVE +1                     TO SA-INDEX

000703     PERFORM 0500-PRINT-HEADINGS THRU 0510-HEADING-EXIT.          00000692
000704                                                                  00000693
000705 0120-INTL-RTN.                                                   00000694
000706                                                                  00000695
000707     IF CLAS-MAXM = ZEROS                                         00000696
000708         MOVE +1 TO CLAS-STARTM.                                  00000697
000709                                                                  00000698
000710     ADD +1 TO CLAS-MAXM.                                         00000699
000711                                                                  00000700
CIDMOD     IF CLAS-MAXM GREATER THAN +60                                00000701
CIDMOD         MOVE +60 TO CLAS-MAXM.                                   00000702
000714                                                                  00000703
000715     MOVE CLAS-MAXM TO X1.                                        00000704
000716     MOVE SPACES    TO CLAS-MORT-CODE (X1).                       00000705
000717     MOVE 'OTHERS'  TO CLAS-MORT-DESC (X1).                       00000706
000718                                                                  00000707
000719     MOVE HIGH-VALUES      TO XT-TABLE  YT-TABLE.                 00000708
000720     PERFORM 0330-PRESS-RTN.                                      00000709
000721                                                                  00000710
000722     MOVE +0 TO X2.                                               00000711
000723 0125-INTL-X2.                                                    00000712
000724     ADD +1 TO X2.                                                00000713
000725                                                                  00000714
000726     IF X2 GREATER THAN +15                                       00000715
000727         GO TO 0130-READ-RTN.                                     00000716
000728                                                                  00000717
000729     MOVE +0 TO X3.                                               00000718
000730                                                                  00000719
000731 0127-INTL-X3.                                                    00000720
000732     ADD +1 TO X3.                                                00000721
000733                                                                  00000722
000734     IF X3 GREATER THAN MAX-BEN                                   00000723
000735         GO TO 0125-INTL-X2.                                      00000724
000736                                                                  00000725
000737     MOVE X-DETL   TO X-AMTS (X2 X3)                              00000726
000738                      Y-AMTS (X2 X3).                             00000727
000739     MOVE X-M-DETL TO X-M-AMTS (X2 X3)                            00000728
000740                      Y-M-AMTS (X2 X3).                           00000729
000741                                                                  00000730
000742     GO TO 0127-INTL-X3.                                          00000731
000743 EJECT                                                            00000732
000744 0130-READ-RTN.                                                   00000733
000745     READ GAAP-EXTR AT END                                        00000734
000746         GO TO 0520-END-INPUT.                                    00000735
000747                                                                  00000736
000748     IF GR-REIN = 'P' OR 'R'                                      00000737
000749         NEXT SENTENCE                                            00000738
000750     ELSE                                                         00000739
000751        MOVE 0301 TO WS-RETURN-CODE                               00000740
000752        MOVE ' INVALID GAAP RECORD ' TO WS-ABEND-MESSAGE          00000741
000753        GO TO ABEND-PGM.                                          00000742
000754                                                                  00000743
CIDMOD     COPY  ELCGAPM1.                                              00000743
CIDMOD                                                                  00000743
000755     MOVE GR-CARRIER   TO CUR-CARR.                               00000744
000756     MOVE GR-GROUPING  TO CUR-CO.                                 00000745
000757     MOVE GR-STATE     TO CUR-ST.                                 00000746
000758                                                                  00000747
000759     IF CUR-SEQ NOT = PRE-SEQ                                     00000748
000760         PERFORM 0330-PRESS-RTN THRU 0380-PRESS-XIT.              00000749
000761                                                                  00000750
000762 0140-SET-X1.                                                     00000751
000763     IF GR-REIN = 'P'                                             00000752
000764         MOVE +1 TO X1                                            00000753
000765     ELSE                                                         00000754
000766         MOVE +2 TO X1.                                           00000755
000767                                                                  00000756
000768 0150-SET-X2.                                                     00000757
CIDMOD     MOVE GR-CCYY TO X-YR.
CIDMOD*    MOVE GR-CC TO X-YR(1:2).                                     ECS083
CIDMOD*    MOVE GR-YR TO X-YR(3:2).                                     ECS083
000770                                                                  00000759
CIDMOD*    COMPUTE X2 = (RUN-YR - X-YR) + +1.                           00000760
CIDMOD     COMPUTE X2 = (RUN-CCYY - X-YR) + +1.                         00000760
000772                                                                  00000761
000773     IF X2 GREATER THAN +15                                       00000762
000774         MOVE +15 TO X2.                                          00000763
000775                                                                  00000764
000776     IF X2 LESS THAN +1                                           00000765
000777         MOVE +1 TO X2.                                           00000766
000778                                                                  00000767
000779 0160-FIND-LIFE.                                                  00000768
000780     IF GR-LFTYP = ZEROS                                          00000769
000781         GO TO 0210-FIND-AH.                                      00000770
000782                                                                  00000771
000783     MOVE '1'         TO X-LAH.                                   00000772
000784     MOVE GR-LFTYP    TO X-BEN.                                   00000773
000785     MOVE CLAS-STARTM TO CLAS-INDEXM.                             00000774
000786                                                                  00000775
000787 0170-LOOP-CLAS-INDEXM.                                           00000776
000788     IF CLAS-MORT-CODE (CLAS-INDEXM) = GR-MORT-CODE OR            00000777
000789        CLAS-MORT-CODE (CLAS-INDEXM) = SPACES                     00000778
000790         MOVE CLAS-INDEXM TO X-MORT                               00000779
000791     ELSE                                                         00000780
000792         ADD +1           TO CLAS-INDEXM                          00000781
000793         GO TO 0170-LOOP-CLAS-INDEXM.                             00000782
000794                                                                  00000783
000795     IF GR-REIN = 'R'                                             00000784
000796         MOVE GR-REIN-COMP TO X-REIN                              00000785
000797     ELSE                                                         00000786
000798         MOVE LOW-VALUE    TO X-REIN.                             00000787
000799                                                                  00000788
000800     PERFORM 0300-LOCATE-X3 THRU 0320-LOCATE-X3-XIT.              00000789
000801                                                                  00000790
000802     IF X1 = +1                                                   00000791
000803         MOVE X-AMTS (X2 X3)   TO X-DETL                          00000792
000804         MOVE X-M-AMTS (X2 X3) TO X-M-DETL                        00000793
000805     ELSE                                                         00000794
000806         MOVE Y-AMTS (X2 X3)   TO X-DETL                          00000795
000807         MOVE Y-M-AMTS (X2 X3) TO X-M-DETL.                       00000796
000808                                                                  00000797
000809     IF GR-SUMMARY-REC                                            00000798
000810        ADD GR-CNT-LF TO X-COUNT                                  00000799
000811      ELSE                                                        00000800
000812        ADD +1        TO X-COUNT.                                 00000801
000813                                                                  00000802
000814     ADD GR-LFPRM     TO X-WRITTEN.                               00000803
000815     ADD GRR-LFPRM    TO X-P78.                                   00000804
000816     ADD GRP-LFPRM    TO X-PRATA.                                 00000805
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
040114        ADD GRD-LFPRM    TO X-DOMICILE
040114     ELSE
040114        ADD GRS-LFPRM    TO X-DOMICILE
040114     END-IF.
000818     ADD GRS-LFPRM    TO X-STATE.                                 00000807
000819     ADD GR-LFCOM     TO X-PAID.                                  00000808
000820     ADD GRR-LFCOM    TO X-C78.                                   00000809
000821     ADD GRP-LFCOM    TO X-CRATA.                                 00000810
040114     ADD GRS-LFCOM    TO X-CDOMI.
000822     ADD GR-LFTAX     TO X-TAX.                                   00000811
000823     ADD GRR-LFTAX    TO X-T78.                                   00000812
000824     ADD GRP-LFTAX    TO X-TRATA.                                 00000813
040114     COMPUTE WRK-DOMI ROUNDED = 
040114              (GRS-LFPRM / GR-LFPRM) * GR-LFTAX.
040114     ADD WRK-DOMI     TO X-TDOMI.
000825                                                                  00000814
000826     IF DTE-CLIENT = 'HER'                                        00000815
000827         ADD GR-LFBEN     TO X-REMAIN                             00000816
000828     ELSE                                                         00000817
000829         ADD GR-REM-AMT   TO X-REMAIN.                            00000818
000830                                                                  00000819
000831     ADD GR-RESV      TO X-RESERV.                                00000820
000832     ADD GR-ALT-RESV  TO X-ALTRSV.                                00000821
000834                                                                  00000822
000835     IF GR-SUMMARY-REC                                            00000823
000836        GO TO  NO-SELECT-LF-ACCUM.                                00000824
000838                                                                  00000825
000839 SELECT-LF-ACCUM-DONE.                                            00000826
000840                                                                  00000827
000857                                                                  00000844
           IF GR-REIN = 'P'
              IF GR-STATE = SA-STATE-CODE (SA-INDEX)
                 CONTINUE
              ELSE
                 PERFORM VARYING SA-INDEX FROM +1 BY +1 UNTIL
                    (GR-STATE = SA-STATE-CODE (SA-INDEX))
                    OR (SA-INDEX > +75)
                 END-PERFORM
                 IF SA-INDEX > +75
                    DISPLAY ' SPECIAL STATE TABLE FOR DCC BLOWN '
                    PERFORM ABEND-PGM
                 END-IF
              END-IF
              IF GR-LF-TERM < +13
                 ADD GRS-LFPRM         TO SA-LT13-LF  (SA-INDEX)
                 ADD GRS-LFCOM         TO SA-LT13-LFC (SA-INDEX)
              ELSE
                 ADD GRS-LFPRM         TO SA-GT12-LF  (SA-INDEX)
                 ADD GRS-LFCOM         TO SA-GT12-LFC (SA-INDEX)
              END-IF
           END-IF
                        
           .
000858                                                                  00000845
000859 NO-SELECT-LF-ACCUM.                                              00000846
000861                                                                  00000847
000862     IF GR-IG = '1'                                               00000848
000863         GO TO 0190-LIFE-IND.                                     00000849
000864                                                                  00000850
000865 0180-LIFE-GRP.                                                   00000851
000866     IF GR-LF-TERM LESS THAN +121                                 00000852
000867        ADD GR-RESV TO X-GUNDR                                    00000853
000868     ELSE                                                         00000854
000869        ADD GR-RESV TO X-GOVER.                                   00000855
000870                                                                  00000856
000871     GO TO 0200-END-LIFE.                                         00000857
000872                                                                  00000858
000873 0190-LIFE-IND.                                                   00000859
000874     IF GR-LF-TERM LESS THAN +121                                 00000860
000875        ADD GR-RESV TO X-IUNDR                                    00000861
000876     ELSE                                                         00000862
000877        ADD GR-RESV TO X-IOVER.                                   00000863
000878                                                                  00000864
000879 0200-END-LIFE.                                                   00000865
000880     IF GR-AHTYP NOT = ZEROS                                      00000866
000881         IF GR-SUMMARY-REC                                        00000867
000882             NEXT SENTENCE                                        00000868
000883          ELSE                                                    00000869
000884             ADD +1 TO X-DUP.                                     00000870
000885                                                                  00000871
000886     IF X1 = +1                                                   00000872
000887         MOVE X-DETL   TO X-AMTS (X2 X3)                          00000873
000888         MOVE X-M-DETL TO X-M-AMTS (X2 X3)                        00000874
000889     ELSE                                                         00000875
000890         MOVE X-DETL   TO Y-AMTS (X2 X3)                          00000876
000891         MOVE X-M-DETL TO Y-M-AMTS (X2 X3).                       00000877
000892                                                                  00000878
000893 0210-FIND-AH.                                                    00000879
000894     IF GR-AHTYP = ZEROS                                          00000880
000895         GO TO 0130-READ-RTN.                                     00000881
000896                                                                  00000882
000897     MOVE '2'      TO X-LAH.                                      00000883
000898     MOVE GR-AHTYP TO X-BEN.                                      00000884
000899     MOVE ZEROS    TO X-SEQ2.                                     00000885
000900                                                                  00000886
000901     IF GR-REIN = 'R'                                             00000887
000902         MOVE GR-REIN-COMP TO X-REIN                              00000888
000903     ELSE                                                         00000889
000904         MOVE LOW-VALUE    TO X-REIN.                             00000890
000905                                                                  00000891
000906     PERFORM 0300-LOCATE-X3 THRU 0320-LOCATE-X3-XIT.              00000892
000907                                                                  00000893
000908     IF X1 = +1                                                   00000894
000909         MOVE X-AMTS (X2 X3)   TO X-DETL                          00000895
000910         MOVE X-M-AMTS (X2 X3) TO X-M-DETL                        00000896
000911     ELSE                                                         00000897
000912         MOVE Y-AMTS (X2 X3)   TO X-DETL                          00000898
000913         MOVE Y-M-AMTS (X2 X3) TO X-M-DETL.                       00000899
000914                                                                  00000900
000915     IF GR-SUMMARY-REC                                            00000901
000916        ADD GR-CNT-AH TO X-COUNT                                  00000902
000917      ELSE                                                        00000903
000918        ADD +1        TO X-COUNT.                                 00000904
000919                                                                  00000905
000920     ADD GR-AHPRM     TO X-WRITTEN.                               00000906
000921     ADD GRR-AHPRM    TO X-P78.                                   00000907
000922     ADD GRP-AHPRM    TO X-PRATA.                                 00000908
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
040114        ADD GRD-AHPRM    TO X-DOMICILE
040114     ELSE
040114        COMPUTE WRK-DOMI ROUNDED = (GRR-AHPRM + GRP-AHPRM) / 2
040114        ADD WRK-DOMI     TO X-DOMICILE
040114     END-IF.
000924     ADD GRS-AHPRM    TO X-STATE.                                 00000910
000925     ADD GR-AHCOM     TO X-PAID.                                  00000911
000926     ADD GRR-AHCOM    TO X-C78.                                   00000912
000927     ADD GRP-AHCOM    TO X-CRATA.                                 00000913
040114     COMPUTE WRK-DOMI ROUNDED = (GRR-AHCOM + GRP-AHCOM) / 2.
040114     ADD WRK-DOMI     TO X-CDOMI.
000928     ADD GR-AHTAX     TO X-TAX.                                   00000914
000929     ADD GRR-AHTAX    TO X-T78.                                   00000915
000930     ADD GRP-AHTAX    TO X-TRATA.                                 00000916
040114     COMPUTE WRK-DOMI ROUNDED = (GRR-AHTAX + GRP-AHTAX) / 2.
040114     ADD WRK-DOMI     TO X-TDOMI.
000931                                                                  00000917
000932     IF DTE-CLIENT = 'HER'                                        00000918
000933         COMPUTE X-REMAIN ROUNDED EQUAL                           00000919
000934                 (GR-AHBEN * GR-AH-TERM) + X-REMAIN               00000920
000935     ELSE                                                         00000921
000936         COMPUTE X-REMAIN ROUNDED EQUAL                           00000922
000937                 (GR-AHBEN * GR-AH-REMTERM) + X-REMAIN.           00000923
000939                                                                  00000924
           IF GR-REIN = 'P'
              IF GR-STATE = SA-STATE-CODE (SA-INDEX)
                 CONTINUE
              ELSE
                 PERFORM VARYING SA-INDEX FROM +1 BY +1 UNTIL
                    (GR-STATE = SA-STATE-CODE (SA-INDEX))
                    OR (SA-INDEX > +75)
                 END-PERFORM
                 IF SA-INDEX > +75
                    DISPLAY ' SPECIAL STATE TABLE FOR DCC BLOWN '
                    PERFORM ABEND-PGM
                 END-IF
              END-IF
              IF GR-AH-TERM < +13
                 ADD GRS-AHPRM         TO SA-LT13-AH  (SA-INDEX)
                 ADD GRS-AHCOM         TO SA-LT13-AHC (SA-INDEX)
              ELSE
                 ADD GRS-AHPRM         TO SA-GT12-AH  (SA-INDEX)
                 ADD GRS-AHCOM         TO SA-GT12-AHC (SA-INDEX)
              END-IF
           END-IF

001169                                                                  00001146
001170     IF X1 = +1                                                   00001147
001171         MOVE X-DETL   TO X-AMTS (X2 X3)                          00001148
001172         MOVE X-M-DETL TO X-M-AMTS (X2 X3)                        00001149
001173     ELSE                                                         00001150
001174         MOVE X-DETL   TO Y-AMTS (X2 X3)                          00001151
001175         MOVE X-M-DETL TO Y-M-AMTS (X2 X3).                       00001152
001176                                                                  00001153
001177     GO TO 0130-READ-RTN.                                         00001154
001178                                                                  00001155
001179 EJECT                                                            00001156
001180 0300-LOCATE-X3.                                                  00001157
001181     MOVE +0 TO X3.                                               00001158
001182                                                                  00001159
001183 0310-LOOP-LX3.                                                   00001160
001184     ADD +1 TO X3.                                                00001161
001185                                                                  00001162
001186     IF X3 GREATER THAN MAX-BEN                                   00001163
001187         PERFORM 0390-PRESS-ONE-RTN THRU 0420-PRESS-ONE-XIT.      00001164
001188                                                                  00001165
001189     IF X1 = +1                                                   00001166
001190       IF XT-SEQ (X2 X3) = HIGH-VALUE  OR                         00001167
001191          XT-SEQ (X2 X3) = X-SEQ                                  00001168
001192           MOVE X-SEQ TO XT-SEQ (X2 X3)                           00001169
001193           GO TO 0320-LOCATE-X3-XIT.                              00001170
001194                                                                  00001171
001195     IF X1 = +2                                                   00001172
001196       IF YT-SEQ (X2 X3) = HIGH-VALUE  OR                         00001173
001197          YT-SEQ (X2 X3) = X-SEQ                                  00001174
001198           MOVE X-SEQ TO YT-SEQ (X2 X3)                           00001175
001199           GO TO 0320-LOCATE-X3-XIT.                              00001176
001200                                                                  00001177
001201     GO TO 0310-LOOP-LX3.                                         00001178
001202                                                                  00001179
001203 0320-LOCATE-X3-XIT.                                              00001180
001204     EXIT.                                                        00001181
001205                                                                  00001182
001206 0330-PRESS-RTN.                                                  00001183
001207     MOVE +0 TO X1                                                00001184
001208                X-DUP                                             00001185
001209                X-COUNT                                           00001186
001210                X-WRITTEN                                         00001187
001211                X-P78                                             00001188
001212                X-PRATA                                           00001189
001213                X-DOMICILE                                        00001190
001214                X-STATE                                           00001191
001215                X-RESERV                                          00001192
001216                X-ALTRSV                                          00001193
001217                X-REMAIN                                          00001194
001218                X-PAID                                            00001195
001219                X-C78                                             00001196
001220                X-CRATA                                           00001197
040114                X-CDOMI
001221                X-TAX                                             00001198
001222                X-T78                                             00001199
001223                X-TRATA                                           00001200
040114                X-TDOMI
001224                X-IUNDR                                           00001201
001225                X-IOVER                                           00001202
001226                X-GUNDR                                           00001203
001227                X-GOVER.                                          00001204
001228                                                                  00001205
001229 0340-PRESS-LOOP-X1.                                              00001206
001230     ADD +1 TO X1.                                                00001207
001231                                                                  00001208
001232     IF X1 GREATER THAN +2                                        00001209
001233         GO TO 0370-INTL-PRESS.                                   00001210
001234                                                                  00001211
001235     MOVE +0 TO X2.                                               00001212
001236                                                                  00001213
001237 0350-PRESS-LOOP-X2.                                              00001214
001238     ADD +1 TO X2.                                                00001215
001239                                                                  00001216
001240     IF X2 GREATER THAN +15                                       00001217
001241         GO TO 0340-PRESS-LOOP-X1.                                00001218
001242                                                                  00001219
001243     MOVE +0 TO X3.                                               00001220
001244                                                                  00001221
001245 0360-PRESS-LOOP-X3.                                              00001222
001246     ADD +1 TO X3.                                                00001223
001247                                                                  00001224
001248     IF X3 GREATER THAN MAX-BEN                                   00001225
001249         GO TO 0350-PRESS-LOOP-X2.                                00001226
001250                                                                  00001227
001251     IF X1 = +1                                                   00001228
001252       IF XT-SEQ (X2 X3) = HIGH-VALUE                             00001229
001253           GO TO 0350-PRESS-LOOP-X2                               00001230
001254       ELSE                                                       00001231
001255           MOVE X-AMTS (X2 X3)       TO W-AMTS                    00001232
001256           IF W-COUNT NOT = +0                                    00001233
001257               MOVE XT-SEQ (X2 X3)   TO W-SEQ3                    00001234
001258               MOVE X-M-AMTS (X2 X3) TO W-M-AMTS                  00001235
001259               MOVE XT-SEQ3 (X2 X3)  TO PRE-REIN                  00001236
001260           ELSE                                                   00001237
001261               GO TO 0365-ZERO-PRESS.                             00001238
001262                                                                  00001239
001263     IF X1 = +2                                                   00001240
001264       IF YT-SEQ (X2 X3) = HIGH-VALUE                             00001241
001265           GO TO 0350-PRESS-LOOP-X2                               00001242
001266       ELSE                                                       00001243
001267           MOVE Y-AMTS (X2 X3)       TO W-AMTS                    00001244
001268           IF W-COUNT NOT = +0                                    00001245
001269               MOVE YT-SEQ (X2 X3)   TO W-SEQ3                    00001246
001270               MOVE Y-M-AMTS (X2 X3) TO W-M-AMTS                  00001247
001271               MOVE YT-SEQ3 (X2 X3)  TO PRE-REIN                  00001248
001272           ELSE                                                   00001249
001273               GO TO 0365-ZERO-PRESS.                             00001250
001274                                                                  00001251
001275     MOVE LOW-VALUE TO W-REIN.                                    00001252
001276     MOVE PRE-SEQ   TO W-SEQ1.                                    00001253
001277     MOVE X1        TO W-CODE.                                    00001254
001278     MOVE X2        TO W-YR.                                      00001255
001279                                                                  00001256
001280     PERFORM 0430-GEN-RTN THRU 0450-GEN-XIT.                      00001257
001281                                                                  00001258
001282 0365-ZERO-PRESS.                                                 00001259
001283                                                                  00001260
001284     IF X1 = +1                                                   00001261
001285         MOVE HIGH-VALUE TO XT-SEQ (X2 X3)                        00001262
001286         MOVE X-DETL     TO X-AMTS (X2 X3)                        00001263
001287         MOVE X-M-DETL   TO X-M-AMTS (X2 X3)                      00001264
001288     ELSE                                                         00001265
001289         MOVE HIGH-VALUE TO YT-SEQ (X2 X3)                        00001266
001290         MOVE X-DETL     TO Y-AMTS (X2 X3)                        00001267
001291         MOVE X-M-DETL   TO Y-M-AMTS (X2 X3).                     00001268
001292                                                                  00001269
001293     GO TO 0360-PRESS-LOOP-X3.                                    00001270
001294                                                                  00001271
001295 0370-INTL-PRESS.                                                 00001272
001296     MOVE CUR-SEQ TO PRE-SEQ.                                     00001273
001297                                                                  00001274
001298 0380-PRESS-XIT.                                                  00001275
001299     EXIT.                                                        00001276
001300                                                                  00001277
001301                                                                  00001278
001302 0390-PRESS-ONE-RTN.                                              00001279
001303     MOVE +0 TO X3                                                00001280
001304                X-DUP                                             00001281
001305                X-COUNT                                           00001282
001306                X-WRITTEN                                         00001283
001307                X-P78                                             00001284
001308                X-PRATA                                           00001285
001309                X-DOMICILE                                        00001286
001310                X-STATE                                           00001287
001311                X-RESERV                                          00001288
001312                X-ALTRSV                                          00001289
001313                X-REMAIN                                          00001290
001314                X-PAID                                            00001291
001315                X-C78                                             00001292
001316                X-CRATA                                           00001293
040114                X-CDOMI
001317                X-TAX                                             00001294
001318                X-T78                                             00001295
001319                X-TRATA                                           00001296
040114                X-TDOMI
001320                X-IUNDR                                           00001297
001321                X-IOVER                                           00001298
001322                X-GUNDR                                           00001299
001323                X-GOVER.                                          00001300
001324                                                                  00001301
001325 0400-PRESS-ONE-LOOP.                                             00001302
001326     ADD +1 TO X3.                                                00001303
001327                                                                  00001304
001328     IF X3 GREATER THAN MAX-BEN                                   00001305
001329         GO TO 0410-PRESS-ONE-END.                                00001306
001330                                                                  00001307
001331     MOVE LOW-VALUE TO W-REIN.                                    00001308
001332     MOVE PRE-SEQ   TO W-SEQ1.                                    00001309
001333     MOVE X1        TO W-CODE.                                    00001310
001334     MOVE X2        TO W-YR.                                      00001311
001335                                                                  00001312
001336     IF X1 = +1                                                   00001313
001337         MOVE XT-SEQ (X2 X3)   TO W-SEQ3                          00001314
001338         MOVE X-AMTS (X2 X3)   TO W-AMTS                          00001315
001339         MOVE X-M-AMTS (X2 X3) TO W-M-AMTS                        00001316
001340         MOVE XT-SEQ3 (X2 X3)  TO PRE-REIN                        00001317
001341     ELSE                                                         00001318
001342         MOVE YT-SEQ (X2 X3)   TO W-SEQ3                          00001319
001343         MOVE Y-AMTS (X2 X3)   TO W-AMTS                          00001320
001344         MOVE Y-M-AMTS (X2 X3) TO W-M-AMTS                        00001321
001345         MOVE YT-SEQ3 (X2 X3)  TO PRE-REIN.                       00001322
001346                                                                  00001323
001347     PERFORM 0430-GEN-RTN THRU 0450-GEN-XIT.                      00001324
001348                                                                  00001325
001349     IF X1 = +1                                                   00001326
001350         MOVE HIGH-VALUE TO XT-SEQ (X2 X3)                        00001327
001351         MOVE X-DETL     TO X-AMTS (X2 X3)                        00001328
001352         MOVE X-M-DETL   TO X-M-AMTS (X2 X3)                      00001329
001353     ELSE                                                         00001330
001354         MOVE HIGH-VALUE TO YT-SEQ (X2 X3)                        00001331
001355         MOVE X-DETL     TO Y-AMTS (X2 X3)                        00001332
001356         MOVE X-M-DETL   TO Y-M-AMTS (X2 X3).                     00001333
001357                                                                  00001334
001358     GO TO 0400-PRESS-ONE-LOOP.                                   00001335
001359                                                                  00001336
001360 0410-PRESS-ONE-END.                                              00001337
001361     MOVE +1 TO X3.                                               00001338
001362                                                                  00001339
001363 0420-PRESS-ONE-XIT.                                              00001340
001364     EXIT.                                                        00001341
001365                                                                  00001342
001367 0430-GEN-RTN.                                                    00001344
001368**   COMPUTE W-YR = (RUN-YR - W-YR) + 1.                          00001345
CIDMOD     COMPUTE WORK-YR = (RUN-CCYY - W-YR) + 1
001369                                                                  00001346
CIDMOD     IF WORK-YR < 0                                               ECS083
CIDMOD        ADD 100 TO WORK-YR.                                          CL**2
CIDMOD     MOVE WORK-YR TO W-YR.                                        ECS083
CIDMOD                                                                  ECS083
001370 0440-GEN-LOOP.                                                   00001347
001371     IF DTE-PGM-OPT NOT GREATER THAN 1                            00001348
001372         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001349
001373                                                                  00001350
001374     MOVE HIGH-VALUE TO W-ST.                                     00001351
001375                                                                  00001352
001376     IF DTE-PGM-OPT NOT GREATER THAN 2                            00001353
001377         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001354
001378                                                                  00001355
001379     MOVE HIGH-VALUE TO W-CO.                                     00001356
001380                                                                  00001357
001381     IF DTE-PGM-OPT NOT GREATER THAN 4                            00001358
001382         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001359
001383                                                                  00001360
001384     MOVE HIGH-VALUE TO W-CARR.                                   00001361
001385                                                                  00001362
001386     IF DTE-PGM-OPT NOT GREATER THAN 6                            00001363
001387         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001364
001388                                                                  00001365
001389     MOVE PRE-SEQ    TO W-SEQ1.                                   00001366
001390     MOVE HIGH-VALUE TO W-CO.                                     00001367
001391                                                                  00001368
001392     IF DTE-PGM-OPT NOT GREATER THAN 3                            00001369
001393         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001370
001394                                                                  00001371
001395     MOVE HIGH-VALUE TO W-CARR.                                   00001372
001396                                                                  00001373
001397     IF DTE-PGM-OPT NOT GREATER THAN 5                            00001374
001398         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001375
001399                                                                  00001376
001400     IF W-REIN NOT = PRE-REIN                                     00001377
001401         MOVE PRE-SEQ  TO W-SEQ1                                  00001378
001402         MOVE PRE-REIN TO W-REIN                                  00001379
001403         GO TO 0440-GEN-LOOP.                                     00001380
001404                                                                  00001381
001405 0450-GEN-XIT.                                                    00001382
001406     EXIT.                                                        00001383
001407                                                                  00001384
001408 0460-RLS-RTN.                                                    00001385
001409     MOVE WORK-REC TO RPT-REC.                                    00001386
001410                                                                  00001387
001411     WRITE RPT-REC.                                               00001388
001412                                                                  00001389
001413 0470-RLS-XIT.                                                    00001390
001414     EXIT.                                                        00001391
001415 EJECT                                                            00001392
001416 0480-PRT-RTN.                                                    00001393
CIDMOD*                            COPY ELCPRTN.                        00001394
CIDMOD                             COPY ELCPRT2.                        00001394
001418                                                                  00001395
001419 0490-E-PRT-RTN.                                                  00001396
001420     EXIT.                                                        00001397
001421                                                                  00001398
001422 0500-PRINT-HEADINGS.                                             00001399
001423     MOVE '1'    TO X.                                            00001400
001424     MOVE HEAD-1 TO P-DATA.                                       00001401
001425     PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    00001402
001426                                                                  00001403
001427     MOVE ' '             TO X.                                   00001404
001428     MOVE WS-CURRENT-DATE TO HD-DATE.                             00001405
001429     MOVE COMPANY-NAME    TO HD-CLIENT.                           00001406
001430     MOVE HEAD-2          TO P-DATA.                              00001407
001431     PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    00001408
001432                                                                  00001409
001433     MOVE 1         TO HD-PAGE.                                   00001410
001434     MOVE ALPH-DATE TO HD-ALF-DTE.                                00001411
001435*    MOVE ALPH-DATE TO PULL-MONTH.                                00001412
001436*    MOVE FIRST4    TO UNEARNED-MO.                               00001413
001437     MOVE HEAD-3    TO P-DATA.                                    00001414
001438     PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    00001415
001439                                                                  00001416
001440 0510-HEADING-EXIT.                                               00001417
001441     EXIT.                                                        00001418
001442                                                                  00001419
001443 0520-END-INPUT.                                                  00001420
001444     PERFORM 0330-PRESS-RTN THRU 0380-PRESS-XIT.                  00001421
001445                                                                  00001422
001446     CLOSE GAAP-EXTR REPTFL.                                      00001423
001447                                                                  00001424
001448*    MOVE '0' TO X.                                               00001425
001449*    MOVE 'UNEARNED PREMIUM AND COMMMISON EXTRACT COMPLETED'      00001426
001450*             TO P-DATA.                                          00001427
001451*    PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    00001428
001452                                                                  00001429
001453 0530-CLOSE-FICH.                                                 00001430
001454                             COPY ELCPRTC.                        00001431
CIDMOD                                                                  00000028
CIDMOD                                                                  00000028
001455                                                                  00001432
001456 9000-PRINT-REPORTS.                                              00001433
001458                                                                  00001434
001423     MOVE '1'                    TO P-CTL
001424     MOVE HEAD-1                 TO P-DATA
001462     PERFORM ACT-PRNT            THRU ACT-EXIT

001427     MOVE ' '                    TO P-CTL
001428     MOVE WS-CURRENT-DATE        TO HD-DATE
001429     MOVE COMPANY-NAME           TO HD-CLIENT
001430     MOVE HEAD-2                 TO P-DATA
001462     PERFORM  ACT-PRNT           THRU ACT-EXIT

001427     MOVE ' '                    TO P-CTL
001433     MOVE 1                      TO HD-PAGE
001434     MOVE ALPH-DATE              TO HD-ALF-DTE
001437     MOVE HEAD-3                 TO P-DATA
001462     PERFORM  ACT-PRNT           THRU ACT-EXIT

001773     MOVE SPACES                 TO P-DATA
001774     MOVE ' '                    TO P-CTL
001777     PERFORM ACT-PRNT            THRU ACT-EXIT
001779     PERFORM ACT-PRNT            THRU ACT-EXIT

001781     MOVE DCC-HEAD1              TO  P-DATA
001782     MOVE ' '                    TO  P-CTL
001784     PERFORM ACT-PRNT            THRU ACT-EXIT

001781     MOVE DCC-HEAD2              TO P-DATA
001782     MOVE ' '                    TO P-CTL
001784     PERFORM ACT-PRNT            THRU ACT-EXIT

001773     MOVE SPACES                 TO P-DATA
001774     MOVE ' '                    TO P-CTL
001777     PERFORM ACT-PRNT            THRU ACT-EXIT

           PERFORM VARYING SA-INDEX FROM +1 BY +1 UNTIL
              (SA-INDEX > +75)
              OR (SA-STATE-CODE (SA-INDEX) = SPACES)
              MOVE SA-STATE-CODE (SA-INDEX)
                                       TO DCC-STATE
              COMPUTE SA-LT13-TOT (SA-INDEX) = 
                 (SA-LT13-LF (SA-INDEX) - SA-LT13-LFC (SA-INDEX))
                 + (SA-LT13-AH (SA-INDEX) - SA-LT13-AHC (SA-INDEX))
              COMPUTE SA-GT12-TOT (SA-INDEX) = 
                 (SA-GT12-LF (SA-INDEX) - SA-GT12-LFC (SA-INDEX))
                 + (SA-GT12-AH (SA-INDEX) - SA-GT12-AHC (SA-INDEX))
              COMPUTE SA-ALL-TOT (SA-INDEX) =
                 SA-LT13-TOT (SA-INDEX) + SA-GT12-TOT (SA-INDEX)
              SUBTRACT SA-LT13-LFC (SA-INDEX)
                 FROM SA-LT13-LF (SA-INDEX) GIVING DCC-LT13-LF
              SUBTRACT SA-LT13-AHC (SA-INDEX)
                 FROM SA-LT13-AH (SA-INDEX) GIVING DCC-LT13-AH
              MOVE SA-LT13-TOT (SA-INDEX)
                                       TO DCC-LT13-TOT
              SUBTRACT SA-GT12-LFC (SA-INDEX)
                 FROM SA-GT12-LF (SA-INDEX) GIVING DCC-GT12-LF
              SUBTRACT SA-GT12-AHC (SA-INDEX)
                 FROM SA-GT12-AH (SA-INDEX) GIVING DCC-GT12-AH
              MOVE SA-GT12-TOT (SA-INDEX)
                                       TO DCC-GT12-TOT
              MOVE SA-ALL-TOT (SA-INDEX)
                                       TO DCC-ALL-TOT
              COMPUTE SA-LT13-LF (75) = SA-LT13-LF (75) +
                 SA-LT13-LF (SA-INDEX)
              COMPUTE SA-LT13-LFC (75) = SA-LT13-LFC (75) +
                 SA-LT13-LFC (SA-INDEX)
              COMPUTE SA-LT13-AH (75) = SA-LT13-AH (75) +
                 SA-LT13-AH (SA-INDEX)
              COMPUTE SA-LT13-AHC (75) = SA-LT13-AHC (75) +
                 SA-LT13-AHC (SA-INDEX)
              COMPUTE SA-GT12-LF (75) = SA-GT12-LF (75) +
                 SA-GT12-LF (SA-INDEX)
              COMPUTE SA-GT12-LFC (75) = SA-GT12-LFC (75) +
                 SA-GT12-LFC (SA-INDEX)
              COMPUTE SA-GT12-AH (75) = SA-GT12-AH (75) +
                 SA-GT12-AH (SA-INDEX)
              COMPUTE SA-GT12-AHC (75) = SA-GT12-AHC (75) +
                 SA-GT12-AHC (SA-INDEX)
              IF SA-ALL-TOT (SA-INDEX) NOT = ZEROS
                 MOVE DCC-DETAIL       TO P-DATA
                 MOVE ' '              TO P-CTL
                 PERFORM ACT-PRNT      THRU ACT-EXIT
              END-IF
           END-PERFORM              

           MOVE SPACES                 TO P-DATA
           MOVE ' '                    TO P-CTL
           PERFORM ACT-PRNT            THRU ACT-EXIT
           MOVE +75                    TO SA-INDEX
           
           MOVE SPACES                 TO DCC-STATE
           COMPUTE SA-LT13-TOT (SA-INDEX) = 
              (SA-LT13-LF (SA-INDEX) - SA-LT13-LFC (SA-INDEX))
              + (SA-LT13-AH (SA-INDEX) - SA-LT13-AHC (SA-INDEX))
           COMPUTE SA-GT12-TOT (SA-INDEX) = 
              (SA-GT12-LF (SA-INDEX) - SA-GT12-LFC (SA-INDEX))
              + (SA-GT12-AH (SA-INDEX) - SA-GT12-AHC (SA-INDEX))
           COMPUTE SA-ALL-TOT (SA-INDEX) =
              SA-LT13-TOT (SA-INDEX) + SA-GT12-TOT (SA-INDEX)
           SUBTRACT SA-LT13-LFC (SA-INDEX)
              FROM SA-LT13-LF (SA-INDEX) GIVING DCC-LT13-LF
           SUBTRACT SA-LT13-AHC (SA-INDEX)
              FROM SA-LT13-AH (SA-INDEX) GIVING DCC-LT13-AH
           MOVE SA-LT13-TOT (SA-INDEX)
                                       TO DCC-LT13-TOT
           SUBTRACT SA-GT12-LFC (SA-INDEX)
              FROM SA-GT12-LF (SA-INDEX) GIVING DCC-GT12-LF
           SUBTRACT SA-GT12-AHC (SA-INDEX)
              FROM SA-GT12-AH (SA-INDEX) GIVING DCC-GT12-AH
           MOVE SA-GT12-TOT (SA-INDEX)
                                       TO DCC-GT12-TOT
           MOVE SA-ALL-TOT (SA-INDEX)
                                       TO DCC-ALL-TOT

           MOVE DCC-DETAIL             TO P-DATA
           MOVE ' '                    TO P-CTL
           PERFORM ACT-PRNT            THRU ACT-EXIT
003707                                                                  00003552
003708     CLOSE PRNTR                                                  00003553
003709           ACT-RPT                                                00003554
003712                                                                  00003556
003713     GOBACK.                                                      00003557

CIDMOD ACT-PRNT.                                                           CL**4
CIDMOD                                                                     CL**4
CIDMOD       IF P-CTL = ' '                                                CL**4
CIDMOD         WRITE ACT-REC FROM PRT AFTER ADVANCING 1 LINE               CL**4
CIDMOD       ELSE                                                          CL**4
CIDMOD         IF P-CTL = '0'                                              CL**4
CIDMOD           WRITE ACT-REC FROM PRT AFTER ADVANCING 2 LINE             CL**4
CIDMOD         ELSE                                                        CL**4
CIDMOD           IF P-CTL = '-'                                            CL**4
CIDMOD             WRITE ACT-REC FROM PRT AFTER ADVANCING 3 LINE           CL**4
CIDMOD           ELSE                                                      CL**4
CIDMOD             WRITE ACT-REC FROM PRT AFTER ADVANCING PAGE.            CL**4
CIDMOD                                                                     CL**4
CIDMOD ACT-EXIT.                                                           CL**4
CIDMOD     EXIT.                                                           CL**4
CIDMOD                                                                     CL**4
003714                                                                  00003558
003715 ABEND-PGM.                                                       00003559
003716                     COPY ELCABEND SUPPRESS.                      00003560
003717     EJECT                                                        00003561
