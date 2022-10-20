000100 IDENTIFICATION DIVISION.                                         00000010
000200 PROGRAM-ID. CIDMNET.
000300 AUTHOR.  C. S. O.                                                00000012
000400          OMAHA, NE.                                              00000013
000500                                                                  00000014
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
110303* 110303                   PEMA  ADD DATE FILE PROCESSING TO 
110303*                                SEPARATE DCC AND CID
011304* 011304                   SMVA  FIX RPT HEADERS FOR DCC 
071904* 071904                   PEMA  ADD ACT # TO END OF RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
030206* 030206                   PEMA  REMOVE CERT NUMBER NUMERIC EDIT
050206* 050206    2006050200002  PEMA  DISCONTINUE SENDING DCC CASH CRT
112607* 112607    2007010300001  PEMA  REMOVE * FROM NAME
051112* 051112  CR2012042700004  PEMA  PASS ALL 7 BYTES OF CLAIM NO.
092016* 092016  CR2016062700001  PEMA  ADD EXP DT AND SPLIT FILES
122002******************************************************************
000600 ENVIRONMENT DIVISION.                                            00000015
000700 CONFIGURATION SECTION.                                           00000016
000800 SPECIAL-NAMES.                                                   00000017
000900     C01 IS NEW-PAGE.                                             00000018
001000                                                                  00000019
001100 INPUT-OUTPUT SECTION.                                            00000025
001200 FILE-CONTROL.                                                    00000026
001300                                                                  00000047
001400                                                                  00000048
001500     SELECT CLAIM-FILE       ASSIGN TO SYS011-UT-FBA1-S-CLMFILE.  00000049

092016     SELECT ELCERT           ASSIGN TO SYS023-FBA1-ELCERT
092016                             ORGANIZATION INDEXED
092016                             ACCESS       DYNAMIC
092016                             RECORD KEY   CM-CONTROL-PRIMARY
092016                             FILE STATUS  ELCERT-FILE-STATUS.

001700     SELECT PNDB-FILE        ASSIGN TO SYS012-UT-FBA1-S-PNDBFILE. 00000051
001800                                                                  00000052
001900     SELECT CRTC-FILE        ASSIGN TO SYS013-UT-FBA1-S-CRTCFILE. 00000053
002000                                                                  00000054
092016     SELECT CLM-HDR-FILE         ASSIGN TO CLMSFILE
092016             ORGANIZATION IS LINE SEQUENTIAL.
092016                                                                  00000056
092016     SELECT CRT-HDR-FILE         ASSIGN TO CERTFILE
092016             ORGANIZATION IS LINE SEQUENTIAL.

002300     SELECT DATE-KARD        ASSIGN TO SYS006-UR-2540R-S-SYS006.  00000057
002400                                                                  00000058
002500     SELECT  DISPLAY-PRT     ASSIGN TO SYS022-UR-1403-S-SYS022.   00000059
002600                                                                  00000060
002700     SELECT  ERROR-PRT       ASSIGN TO SYS023-UR-1403-S-SYS023.   00000061
110303
110303     SELECT DISK-DATE        ASSIGN TO SYS019.
002800                                                                  00000062
002900******************************************************************00000063
003000   EJECT                                                          00000064
003100 DATA DIVISION.                                                   00000065
003200 FILE SECTION.                                                    00000066
003300******************************************************************00000067
003400                                                                  00000068
003500 FD  DISPLAY-PRT                                                  00000069
003600     LABEL RECORDS ARE STANDARD                                   00000070
092016     RECORD CONTAINS 91 CHARACTERS                                00000071
003800     BLOCK CONTAINS 0 RECORDS                                     00000072
003900     DATA RECORD IS DISPLAY-REC.                                  00000073
004000                                                                  00000074
004100 01  DISPLAY-REC.                                                 00000075
004200     12  DISPLAY-CC              PIC X.                           00000076
004300     12  DISPLAY-INFO.                                            00000077
004400         15  DISPLAY-INFO-11     PIC X(11).                       00000078
004500         15  FILLER              PIC X.                           00000079
092016         15  DISPLAY-INFO-OTHERS PIC X(78).                       00000080
004700                                                                  00000081
004800******************************************************************00000082
004900                                                                  00000083
005000 FD  ERROR-PRT                                                    00000084
005100     LABEL RECORDS ARE STANDARD                                   00000085
092016     RECORD CONTAINS 91 CHARACTERS                                00000086
005300     BLOCK CONTAINS 0 RECORDS                                     00000087
005400     DATA RECORD IS ERR-REC.                                      00000088
005500                                                                  00000089
005600 01  ERR-REC.                                                     00000090
005700     12  ERR-CC              PIC X.                               00000091
092016     12  ERR-INFO            PIC X(90).                           00000092
005900                                                                  00000093
006000******************************************************************00000094
006100                                                                  00000095
006200 FD  DATE-KARD                                                    00000096
006300     LABEL RECORDS ARE STANDARD                                   00000097
006400     RECORD CONTAINS 80 CHARACTERS                                00000098
006500     DATA RECORD IS DATE-CARD-REC.                                00000099
006600                                                                  00000100
006700 01  DATE-CARD-REC            PIC X(80).                          00000101
006800******************************************************************00000102
006900                                                                  00000103
007000 FD  CLAIM-FILE                                                   00000104
007100     LABEL RECORDS ARE STANDARD                                   00000105
007200     RECORD CONTAINS 350 CHARACTERS                               00000106
007300     DATA RECORD IS CLAIM-REC.                                    00000107
007400                                                                  00000108
007500 01  CLAIM-REC                       PIC X(350).                  00000109

092016 FD  ELCERT.
092016                                 COPY ELCCERT.

007600******************************************************************00000110
007700                                                                  00000111
007800 FD  PNDB-FILE                                                    00000112
007900     LABEL RECORDS ARE STANDARD                                   00000113
008000     RECORD CONTAINS 585 CHARACTERS                               00000114
008100     DATA RECORD IS PNDB-REC.                                     00000115
008200                                                                  00000116
008300 01  PNDB-REC                       PIC X(585).                   00000117
008400******************************************************************00000118
008500                                                                  00000119
008600 FD  CRTC-FILE                                                    00000120
008700     LABEL RECORDS ARE STANDARD                                   00000121
008800     RECORD CONTAINS 300 CHARACTERS                               00000122
008900     DATA RECORD IS CRTC-REC.                                     00000123
009000                                                                  00000124
009100 01  CRTC-REC                            PIC X(300).              00000125
009200******************************************************************00000126
009300                                                                  00000127
092016 FD  CLM-HDR-FILE
092016     LABEL RECORDS ARE STANDARD             
092016     BLOCK CONTAINS 0 RECORDS.              
092016                                            
092016*01  HDR-REC                 PIC X(70).     
092016*01  HDR-REC                 PIC X(80).
092016 01  clm-rec                 pic x(90).
092016                                            
092016 FD  CRT-HDR-FILE
092016     LABEL RECORDS ARE STANDARD             
092016     BLOCK CONTAINS 0 RECORDS.              
092016                                            
092016*01  HDR-REC                 PIC X(70).     
092016*01  HDR-REC                 PIC X(80).
092016 01  crt-rec                 pic x(90).
009900                                                                  00000133
010000******************************************************************00000134
110303 FD  DISK-DATE                                                    
110303                             COPY ELCDTEFD.                       
110303 EJECT                                                            
010100   EJECT                                                          00000135
010200                                                                  00000136
010300 WORKING-STORAGE SECTION.                                         00000137
010400                                                                  00000138
010500 01  FILLER        PIC X(24) VALUE 'CIDMNET WORKING STORAGE '.    00000139
010600******************************************************************00000140
010700                                                                  00000141
010800 77  ERROR-CODE              PIC X(90)  VALUE SPACES.             00000142
010900 77  ERR-FLAG                PIC X      VALUE 'N'.                00000143
011000 77  SUB-IN                  PIC 99     VALUE ZEROS.              00000144
011100 77  SUB-OUT                 PIC 99     VALUE ZEROS.              00000145
011200 77  SPACE-SUB               PIC 99     VALUE ZEROS.              00000146
011300 77  E-SUB                   PIC 99     VALUE ZEROS.              00000147
011400 77  SAVE-BIN-DATE           PIC XX     VALUE LOW-VALUES.         00000148
011500 77  ELMSTR-FILE-STATUS      PIC XX     VALUE ZERO.               00000149
092016 77  ELCERT-FILE-STATUS      PIC XX     VALUE ZERO.
011600 77  CLAIM-CNTR              PIC 9(09)  VALUE ZERO.               00000150
011700 77  CLAIM-HDR               PIC 9(09)  VALUE ZERO.               00000151
011800 77  PNDB-CNTR               PIC 9(09)  VALUE ZERO.               00000152
011900 77  PNDB-HDR                PIC 9(09)  VALUE ZERO.               00000153
012000 77  CRTC-CNTR               PIC 9(09)  VALUE ZERO.               00000154
012100 77  CRTC-HDR                PIC 9(09)  VALUE ZERO.               00000155
012200 77  TOT-HDR                 PIC 9(09)  VALUE ZERO.               00000156
012300 77  ERR-CNTR                PIC 9(09)  VALUE ZERO.               00000157
012400 77  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.             00000158
012500 77  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZEROS.              00000159
012600 77  ERR-HEAD-SW             PIC X       VALUE 'Y'.               00000160
012700 77  DIS-HEAD-SW             PIC X       VALUE 'Y'.               00000161
012800 77  ERROR-COUNT             PIC 9(7)    VALUE ZEROS.             00000162
012900 77  DIS-LINE-CNT            PIC 99      VALUE ZEROS.             00000163
013000 77  ERR-LINE-CNT            PIC 99      VALUE ZEROS.             00000164
110303 77  PGM-SUB                 PIC S999 COMP  VALUE +521.
110303 77  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
013100                                                                  00000165
011304 01  FILLER.
011304     05  WS-DCC-ERR-RPT-TITLE PIC X(26)
011304                              VALUE 'DCC TO IMNET ERROR REPORT '.
011304     05  WS-DCC-RPT-TITLE     PIC X(24)
011304                              VALUE 'DCC TO IMNET LOAD REPORT'.

013200******************************************************************00000166
013300                                                                  00000167
013400 01  ERR-HD-1.                                                    00000168
013500     12  FILLER              PIC X     VALUE SPACES.   
013600     12  FILLER              PIC X(21) VALUE SPACES.  
011304     12  ERR-HD-1-TITLE      PIC X(26) 
                                   VALUE 'CID TO IMNET ERROR REPORT '. 
013800     12  FILLER              PIC X(20) VALUE SPACES.           
013900     12  FILLER              PIC X(07) VALUE 'CIIMNET'.       
014000     12  FILLER              PIC X(03) VALUE SPACES.         
014100                                                                  00001351
014200******************************************************************00001352
014300                                                                  00001353
014400 01  DISPLAY-HD-1.                                                00001354
014500     12  FILLER              PIC X     VALUE SPACES.   
014600     12  FILLER              PIC X(21) VALUE SPACES.  
011304     12  DISPLAY-HD-1-TITLE  PIC X(24) 
                                   VALUE 'CID TO IMNET LOAD REPORT'.  
014800     12  FILLER              PIC X(20) VALUE SPACES.           
014900     12  FILLER              PIC X(07) VALUE 'CIIMNET'.       
015000     12  FILLER              PIC X(05) VALUE SPACES.         
015100                                                                  00001361
015200******************************************************************00001362
015300                                                                  00001363
015400 01  DISPLAY-HD-2.                                                00001364
015500     12  FILLER      PIC X     VALUE SPACES.                      00001365
015600     12  FILLER      PIC X(24) VALUE SPACES.                      00001366
015700     12  FILLER      PIC X(11) VALUE 'RUN DATE = '.               00001367
015800     12  DIS-DATE    PIC X(8)  VALUE SPACES.                      00001368
015900     12  FILLER      PIC X(22) VALUE SPACES.                      00001369
016000                                                                  00001370
016100******************************************************************00001371
016200                                                                  00001372
016300 01  ERR-LINE.                                                    00001373
016400     05  ERR-LINE-05.                                             00001374
016500         10  ERR-LINE-CC         PIC X.                           00001375
092016         10  ERR-LINE-REC        PIC X(90).                       00001376
016700                                                                  00001377
016800     05  ERR-LINE-ALT  REDEFINES  ERR-LINE-05.                    00001378
016900         10  ERR-CC-ALT          PIC X.                           00001379
017000         10  ERR-FLD-1           PIC X(35).                       00001380
092016         10  ERR-FLD-2           PIC X(55).                       00001381
017200                                                                  00001442
017300                                                                  00001443
017400******************************************************************00001444
017500                                                                  00001445
017600 01  DISPLAY-LINE.                                                00001446
017700     05  DISPLAY-LINE-05.                                         00001447
017800         10  DIS-CC              PIC X.                           00001448
092016         10  DIS-LINE-REC        PIC X(90).                       00001449
018000                                                                  00001450
018100     05  DISPLAY-LINE-ALT  REDEFINES  DISPLAY-LINE-05.            00001451
018200         10  DIS-CC-ALT          PIC X.                           00001452
018300         10  DIS-FLD-1           PIC X(35).                       00001453
092016         10  DIS-FLD-2           PIC X(55).                       00001454
018500                                                                  00001455
018600******************************************************************00001456
018700                                                                  00001457
018800 01  CC-STATUS-CODE.                                              00001458
018900     05  CC-STATUS-1         PIC X      VALUE ZERO.               00001459
019000     05  CC-STATUS-2         PIC X      VALUE ZERO.               00001460
019100******************************************************************00001461
019200                                                                  00001462
019300 01  PB-STATUS-CODE.                                              00001463
019400     05  PB-STATUS-1         PIC X      VALUE ZERO.               00001464
019500     05  PB-STATUS-2         PIC X      VALUE ZERO.               00001465
019600******************************************************************00001466
019700                                                                  00001467
019800 01  SAVE-CMP-CODE           PIC S9(4) COMP   VALUE +04.          00001468
019900 01  FILLER        REDEFINES      SAVE-CMP-CODE.                  00001469
020000     05 FILLER               PIC X.                               00001470
020100     05 SAVE-CMP-CD          PIC X.                               00001471
020200******************************************************************00001472
020300                                                                  00001473
020400 01  CRT-NUM.                                                     00001474
020500     05 CRT-2                PIC X(02)   VALUE ZEROS.             00001475
020600     05 CRT-7                PIC X(07)   VALUE ZEROS.             00001476
020700     05 CRT-1                PIC X(01)   VALUE ZEROS.             00001477
020800******************************************************************00001478
020900                                                                  00001479
021000 01  EDIT-CRT-NUM.                                                00001480
021100     05 E-CRT-3              PIC X(03).                           00001481
021200     05 E-CRT-7              PIC X(07).                           00001482
021300                                                                  00001483
021400 01  EDIT-CRT-NUM-N REDEFINES  EDIT-CRT-NUM.                      00001484
021500     05 E-CRT-NUMERIC        PIC 9(10).                           00001485
021600******************************************************************00001486
021700                                                                  00001487
021800 01  CL-NUMBER.                                                   00001488
021900     05 CL-FIRST-3           PIC X(03)   VALUE ZEROS.             00001489
022000     05 CL-LAST-7.                                                00001490
022100         10 CL-LAST-2        PIC X(02)   VALUE ZEROS.             00001491
022200         10 CL-LAST-5        PIC X(05)   VALUE ZEROS.             00001492
022300******************************************************************00001493
022400                                                                  00001494
022500 01  NAME-FLD-IN-X                     PIC X(35).                 00001495
022600                                                                  00001496
022700 01  NAME-FLD-IN  REDEFINES NAME-FLD-IN-X.                        00001497
022800     05 FLD-POS-IN   OCCURS 35 TIMES   PIC X.                     00001498
022900******************************************************************00001499
023000                                                                  00001500
023100 01  NAME-FLD-OUT-X                    PIC X(35).                 00001501
023200                                                                  00001502
023300 01  NAME-FLD-OUT  REDEFINES NAME-FLD-OUT-X.                      00001503
023400     05 FLD-POS-OUT  OCCURS 35 TIMES   PIC X.                     00001504
023500******************************************************************00001505
023600                                                                  00001506
023700 01  ML-NAME.                                                     00001507
023800     05 ML-LAST-NAME         PIC X(15)   VALUE SPACES.            00001508
023900     05 ML-FIRST             PIC X(10)   VALUE SPACES.            00001509
024000     05 ML-INIT              PIC X(01)   VALUE SPACES.            00001510
024100     05 FILLER               PIC X(09)   VALUE SPACES.            00001511
024200******************************************************************00001512
024300                                                                  00001513
024400 01  CL-NAME.                                                     00001514
024500     05 CL-LAST-NAME         PIC X(15)      VALUE SPACES.         00001515
024600     05 CL-FIRST             PIC X(13)      VALUE SPACES.         00001516
024700     05 CL-INIT              PIC X(01)      VALUE SPACES.         00001517
024800     05 FILLER               PIC X(06)      VALUE SPACES.         00001518
024900******************************************************************00001519
025000                                                                  00001520
025100 01  CM-NAME.                                                     00001521
025200     05 CM-LAST-NAME         PIC X(15)      VALUE SPACES.         00001522
025300     05 CM-INITS.                                                 00001523
025400        10 CM-FIRST-INIT     PIC X(01)      VALUE SPACES.         00001524
025500        10 CM-SECOND-INIT    PIC X(01)      VALUE SPACES.         00001525
025600     05 FILLER               PIC X(18)      VALUE SPACES.         00001526
025700******************************************************************00001527
025800                                                                  00001528
025900 01  WS-GROUP.                                                    00001529
026000     05 WS-HDR-COUNT         PIC 9(7)       VALUE 0.              00001530
026100     05 WS-EXTR-COUNT        PIC 9(7)       VALUE 0.              00001531
026200******************************************************************00001532
026300                                                                  00001533
026400 01  WS-ISSUE-DATE.                                               00001534
026500     05  WS-ISS-MO          PIC X(02)    VALUE SPACES.            00001535
026600     05  WS-ISS-DA          PIC X(02)    VALUE SPACES.            00001536
026700     05  WS-ISS-YR          PIC X(02)    VALUE SPACES.            00001537
026800******************************************************************00001538
026900                                                                  00001539
027000 01  OVERRIDE-DATE-IN.                                            00001540
027100     05  OVERRIDE-DATE.                                           00001541
027200         10  OVERRIDE-MO         PIC X(02)    VALUE SPACES.       00001542
027300         10  FILLER              PIC X(01)    VALUE '/'.          00001543
027400         10  OVERRIDE-DA         PIC X(02)    VALUE SPACES.       00001544
027500         10  FILLER              PIC X(01)    VALUE '/'.          00001545
027600         10  OVERRIDE-YR         PIC X(02)    VALUE SPACES.       00001546
027700     05  FILLER                  PIC X(17)    VALUE               00001547
027800                                 ' PREV CYC DATE = '.             00001548
027900     05  P-C-DATE                PIC X(08)    VALUE SPACES.       00001549
028000     05  FILLER                  PIC X(47)    VALUE SPACES.       00001550
028100******************************************************************00001551
028200                                                                  00001552
028300 01  WS-CURR-DATE.                                                00001553
028400     05  WS-CURR-MO          PIC X(02)    VALUE SPACES.           00001554
028500     05  FILLER              PIC X(01)    VALUE '/'.              00001555
028600     05  WS-CURR-DA          PIC X(02)    VALUE SPACES.           00001556
028700     05  FILLER              PIC X(01)    VALUE '/'.              00001557
028800     05  WS-CURR-YR          PIC X(02)    VALUE SPACES.           00001558
028900******************************************************************00001559
029000                                                                  00001560
029100 01  WS-CYC-DATE.                                                 00001561
029200     05  WS-CYC-MO           PIC X(02)    VALUE SPACES.           00001562
029300     05  FILLER              PIC X(01)    VALUE '/'.              00001563
029400     05  WS-CYC-DA           PIC X(02)    VALUE SPACES.           00001564
029500     05  FILLER              PIC X(01)    VALUE '/'.              00001565
029600     05  WS-CYC-YR           PIC X(02)    VALUE SPACES.           00001566
029700******************************************************************00001567
CIDVAO 01  DATE-IN.                                                     00001568
CIDVAO     05  WORK-DATE-IN.
CIDVAO         10   DATE-YY-IN     PIC 99       VALUE ZEROES.
CIDVAO         10   DATE-MM-IN     PIC 99       VALUE ZEROES.
CIDVAO         10   DATE-DD-IN     PIC 99       VALUE ZEROES.
CIDVAO***********************************************************
029900 01  WORK-DATE.                                                   00001569
030000     05  WORK-CENT           PIC X(02)    VALUE SPACES.           00001570
030100     05  WORK-YR             PIC X(02)    VALUE SPACES.           00001571
030200******************************************************************00001572
030300                                                                  00001573
030400 01  WS-HDR-REC.                                            
030500     05  H-11.                                              
030600         10  H-TRANS-CODE        PIC X        VALUE SPACES. 
030700         10  H-POLICY-NUM        PIC X(10)    VALUE SPACES. 
030800     05  H-OTHERS.                                          
030900         10  H-INS-NAME          PIC X(35)    VALUE SPACES. 
031000         10  H-ISS-STATE         PIC XX       VALUE SPACES. 
092016         10  H-eff-dt. *> inc dt(clm),eff dt(crt)
031200             15  H-YR.                                      
031300                 20  H-CENTURY   PIC XX       VALUE SPACES. 
031400                 20  H-YEAR      PIC XX       VALUE SPACES. 
031500             15  H-MONTH         PIC XX       VALUE SPACES. 
031600             15  H-DAY           PIC XX       VALUE SPACES. 
092016         10  H-IMNET-DT. *> last maint date
031800             20  IMNET-YR        PIC XXXX     VALUE SPACES. 
031900             20  IMNET-MO        PIC XX       VALUE SPACES. 
032000             20  IMNET-DA        PIC XX       VALUE SPACES. 
092016         10  h-expire-dt         pic x(8)     value spaces.
032100         10  H-STATUS            PIC X        VALUE SPACES. 
071904         10  H-ACCOUNT           PIC X(10)    VALUE SPACES.
092016         10  h-claim-no          pic x(7)     value spaces.

032400******************************************************************00001594
032500                                                                  00001595
032600*01  EDIT-LINE               PIC X(70).                           00001596
032700*01  FILLER   REDEFINES   EDIT-LINE.                              00001597
032800*    05  EDIT-POS  OCCURS 70 TIMES PIC X.                         00001598

071904*01  EDIT-LINE               PIC X(80).
071904*01  FILLER   REDEFINES   EDIT-LINE.
071904*    05  EDIT-POS  OCCURS 80 TIMES PIC X.

092016 01  EDIT-LINE               PIC X(90).
092016 01  FILLER   REDEFINES   EDIT-LINE.
092016     05  EDIT-POS  OCCURS 90 TIMES PIC X.

110105                                 COPY ELCMSTR.
110105                                 COPY ERCPNDB.
110105                                 COPY ERCCRTC.

CIDVAO                                 COPY ELCDATE.
110303                                 COPY ELCDTECX.
110303                                 COPY ELCDTEVR.
108600                                                                  00002358
108700 PROCEDURE DIVISION.                                              00002359
108800 0000-MAINLINE SECTION.                                           00002360
108900                                                                  00002361
109300     DISPLAY 'START PROGRAM CIDMNET'.                             00002365
110303*0003-LOAD-DATE-CARD.                                             
110303                             COPY ELCDTERX.                       
109700                                                                  00002369
092016     OPEN  INPUT  DATE-KARD ELCERT
109900          OUTPUT  DISPLAY-PRT                                     00002371
110000                  ERROR-PRT.                                      00002372

092016     IF ELCERT-FILE-STATUS NOT = '00'
092016        DISPLAY ' ERROR-ELCERT-OPEN ' ELCERT-FILE-STATUS
092016        PERFORM ABEND-PGM
092016     END-IF

CIDVAO**9/16/98: COBOL II DATE CHANGE.
CIDVAO**
CIDVAO     ACCEPT  DATE-IN          FROM  DATE.
CIDVAO     MOVE    DATE-MM-IN       TO    WS-CURR-MO
CIDVAO                                    WS-CYC-MO.
CIDVAO     MOVE    DATE-DD-IN       TO    WS-CURR-DA
CIDVAO                                    WS-CYC-DA.
CIDVAO     MOVE    DATE-YY-IN       TO    WS-CURR-YR
CIDVAO                                    WS-CYC-YR.
CIDVAO****
TSTMOD     DISPLAY 'WS-CURR-DATE = '  WS-CURR-DATE.                     00002374
TSTMOD     DISPLAY 'WS-CYC-DATE  = '  WS-CYC-DATE.                      00002375
110200**** MOVE CURRENT-DATE  TO  WS-CURR-DATE.                         00002374
110300**** MOVE CURRENT-DATE  TO  WS-CYC-DATE.                          00002375
110400     MOVE WS-CURR-DATE  TO  DIS-DATE.                             00002376
110500                                                                  00002377
011304     IF DTE-CLIENT = 'DCC' 
011304         MOVE WS-DCC-ERR-RPT-TITLE TO ERR-HD-1-TITLE
011304         MOVE WS-DCC-RPT-TITLE     TO DISPLAY-HD-1-TITLE
011304     END-IF.

110600     MOVE  ZEROS  TO  DIS-LINE-CNT.                               00002378
110700     PERFORM 8600-DISPLAY-PRT THRU                                00002379
110800          8600-DISPLAY-EXIT.                                      00002380
110900                                                                  00002381
111000     MOVE  ZEROS  TO  ERR-LINE-CNT.                               00002382
111100     PERFORM 8600-ERR-PRT THRU                                    00002383
111200          8600-ERR-EXIT.                                          00002384
111300                                                                  00002385
111400                                                                  00002386
111500     READ  DATE-KARD  INTO  OVERRIDE-DATE-IN                      00002387
111600        AT END                                                    00002388
111700           CLOSE DATE-KARD.                                       00002389
111800                                                                  00002390
111900     DISPLAY 'OVERRIDE-DATE-IN = ' OVERRIDE-DATE-IN.              00002391
112000     DISPLAY ' '.                                                 00002392
112100     PERFORM 0500-START  THRU 0590-EXIT.                          00002393
112200                                                                  00002394
112300     MOVE    '**************************************************' 00002395
112400        TO  DIS-LINE-REC.                                         00002396
112500     PERFORM 8600-DISPLAY-PRT THRU                                00002397
112600          8600-DISPLAY-EXIT.                                      00002398
112700                                                                  00002399
112800     MOVE    '    '                                               00002400
112900        TO  DIS-LINE-REC.                                         00002401
113000     PERFORM 8600-DISPLAY-PRT THRU                                00002402
113100          8600-DISPLAY-EXIT.                                      00002403
113200                                                                  00002404
113300     MOVE    '      REQUESTED DATE IS = '                         00002405
113400        TO  DIS-FLD-1.                                            00002406
113500     MOVE    WS-CURR-DATE                                         00002407
113600        TO  DIS-FLD-2.                                            00002408
113700     PERFORM 8600-DISPLAY-PRT THRU                                00002409
113800          8600-DISPLAY-EXIT.                                      00002410
113900                                                                  00002411
114000     MOVE   SPACES                                                00002412
114100        TO  DIS-LINE-REC.                                         00002413
114200     PERFORM 8600-DISPLAY-PRT THRU                                00002414
114300          8600-DISPLAY-EXIT.                                      00002415
114400                                                                  00002416
114500     MOVE    '**************************************************' 00002417
114600        TO  DIS-LINE-REC.                                         00002418
114700     PERFORM 8600-DISPLAY-PRT THRU                                00002419
114800          8600-DISPLAY-EXIT.                                      00002420
114900                                                                  00002421
115000     MOVE    SPACES                                               00002422
115100        TO  DIS-LINE-REC.                                         00002423
115200     PERFORM 8600-DISPLAY-PRT THRU                                00002424
115300          8600-DISPLAY-EXIT.                                      00002425
115400                                                                  00002426
115500                                                                  00002427
115600     OPEN INPUT   CLAIM-FILE                                      00002428
092016          OUTPUT  CLM-HDR-FILE CRT-HDR-FILE.
115800                                                                  00002430
115900     MOVE    '**************************************************' 00002431
116000        TO  DIS-LINE-REC.                                         00002432
116100     PERFORM 8600-DISPLAY-PRT THRU                                00002433
116200          8600-DISPLAY-EXIT.                                      00002434
116300                                                                  00002435
116400     MOVE    '    '                                               00002436
116500        TO  DIS-LINE-REC.                                         00002437
116600     PERFORM 8600-DISPLAY-PRT THRU                                00002438
116700          8600-DISPLAY-EXIT.                                      00002439
116800                                                                  00002440
116900     MOVE    '  HDR FILE OPENED '                                 00002441
117000        TO  DIS-LINE-REC.                                         00002442
117100     PERFORM 8600-DISPLAY-PRT THRU                                00002443
117200          8600-DISPLAY-EXIT.                                      00002444
117300                                                                  00002445
117400     MOVE    '    '                                               00002446
117500        TO  DIS-LINE-REC.                                         00002447
117600     PERFORM 8600-DISPLAY-PRT THRU                                00002448
117700          8600-DISPLAY-EXIT.                                      00002449
117800                                                                  00002450
117900     MOVE    'CLAIM FILE OPENED '                                 00002451
118000        TO  DIS-LINE-REC.                                         00002452
118100     PERFORM 8600-DISPLAY-PRT THRU                                00002453
118200          8600-DISPLAY-EXIT.                                      00002454
118300                                                                  00002455
118400     MOVE    '    '                                               00002456
118500        TO  DIS-LINE-REC.                                         00002457
118600     PERFORM 8600-DISPLAY-PRT THRU                                00002458
118700          8600-DISPLAY-EXIT.                                      00002459
118800                                                                  00002460
118900     MOVE    '**************************************************' 00002461
119000        TO  DIS-LINE-REC.                                         00002462
119100     PERFORM 8600-DISPLAY-PRT THRU                                00002463
119200          8600-DISPLAY-EXIT.                                      00002464
119300                                                                  00002465
119400     MOVE    '    '                                               00002466
119500        TO  DIS-LINE-REC.                                         00002467
119600     PERFORM 8600-DISPLAY-PRT THRU                                00002468
119700          8600-DISPLAY-EXIT.                                      00002469
119800                                                                  00002470
119900                                                                  00002471
120000     PERFORM 1000-PROCESS-CLAIM THRU 1900-EXIT.                   00002472
120100                                                                  00002473
092016     CLOSE        CLAIM-FILE ELCERT
120300                                                                  00002475

120500     MOVE    ' CRTC FILE OPENED '                                 00002477
120600        TO  DIS-LINE-REC.                                         00002478
120700     PERFORM 8600-DISPLAY-PRT THRU                                00002479
120800          8600-DISPLAY-EXIT.                                      00002480
120900                                                                  00002481
121000     MOVE    '    '                                               00002482
121100        TO  DIS-LINE-REC.                                         00002483
121200     PERFORM 8600-DISPLAY-PRT THRU                                00002484
121300          8600-DISPLAY-EXIT.                                      00002485
121400                                                                  00002486
121500     MOVE    '**************************************************' 00002487
121600        TO  DIS-LINE-REC.                                         00002488
121700     PERFORM 8600-DISPLAY-PRT THRU                                00002489
121800          8600-DISPLAY-EXIT.                                      00002490
121900                                                                  00002491
122000     MOVE    '    '                                               00002492
122100        TO  DIS-LINE-REC.                                         00002493
122200     PERFORM 8600-DISPLAY-PRT THRU                                00002494
122300          8600-DISPLAY-EXIT.                                      00002495
122400                                                                  00002496
122500                                                                  00002497
122600     OPEN INPUT   CRTC-FILE.                                      00002498
122700                                                                  00002499
122800     PERFORM 3000-PROCESS-CRTC  THRU 3900-EXIT.                   00002500
122900                                                                  00002501
123000     CLOSE        CRTC-FILE.                                      00002502
123100                                                                  00002503
123200     MOVE    '**************************************************' 00002504
123300        TO  DIS-LINE-REC.                                         00002505
123400     PERFORM 8600-DISPLAY-PRT THRU                                00002506
123500          8600-DISPLAY-EXIT.                                      00002507
123600                                                                  00002508
123700     MOVE    '    '                                               00002509
123800        TO  DIS-LINE-REC.                                         00002510
123900     PERFORM 8600-DISPLAY-PRT THRU                                00002511
124000          8600-DISPLAY-EXIT.                                      00002512
124100                                                                  00002513
124200     MOVE    ' CRTC FILE CLOSED '                                 00002514
124300        TO  DIS-LINE-REC.                                         00002515
124400     PERFORM 8600-DISPLAY-PRT THRU                                00002516
124500          8600-DISPLAY-EXIT.                                      00002517
124600                                                                  00002518
124700     MOVE    '    '                                               00002519
124800        TO  DIS-LINE-REC.                                         00002520
124900     PERFORM 8600-DISPLAY-PRT THRU                                00002521
125000          8600-DISPLAY-EXIT.                                      00002522
125100                                                                  00002523
125200     MOVE    ' PNDB FILE OPENED '                                 00002524
125300        TO  DIS-LINE-REC.                                         00002525
125400     PERFORM 8600-DISPLAY-PRT THRU                                00002526
125500          8600-DISPLAY-EXIT.                                      00002527
125600                                                                  00002528
125700     MOVE    '    '                                               00002529
125800        TO  DIS-LINE-REC.                                         00002530
125900     PERFORM 8600-DISPLAY-PRT THRU                                00002531
126000          8600-DISPLAY-EXIT.                                      00002532
126100                                                                  00002533
126200     MOVE    '**************************************************' 00002534
126300        TO  DIS-LINE-REC.                                         00002535
126400     PERFORM 8600-DISPLAY-PRT THRU                                00002536
126500          8600-DISPLAY-EXIT.                                      00002537
126600                                                                  00002538
126700     MOVE    '    '                                               00002539
126800        TO  DIS-LINE-REC.                                         00002540
126900     PERFORM 8600-DISPLAY-PRT THRU                                00002541
127000          8600-DISPLAY-EXIT.                                      00002542
127100                                                                  00002543
127200                                                                  00002544
127300     OPEN INPUT   PNDB-FILE.                                      00002545
127400                                                                  00002546
127500     PERFORM 4000-PROCESS-PNDB  THRU 4900-EXIT.                   00002547
127600                                                                  00002548
127700     CLOSE        PNDB-FILE.                                      00002549
092016     CLOSE        CLM-HDR-FILE CRT-HDR-FILE
127900                                                                  00002551
128000     MOVE    '**************************************************' 00002552
128100        TO  DIS-LINE-REC.                                         00002553
128200     PERFORM 8600-DISPLAY-PRT THRU                                00002554
128300          8600-DISPLAY-EXIT.                                      00002555
128400                                                                  00002556
128500     MOVE    '    '                                               00002557
128600        TO  DIS-LINE-REC.                                         00002558
128700     PERFORM 8600-DISPLAY-PRT THRU                                00002559
128800          8600-DISPLAY-EXIT.                                      00002560
128900                                                                  00002561
129000     MOVE    '   PNDB FILE CLOSED '                               00002562
129100        TO  DIS-LINE-REC.                                         00002563
129200     PERFORM 8600-DISPLAY-PRT THRU                                00002564
129300          8600-DISPLAY-EXIT.                                      00002565
129400                                                                  00002566
129500     MOVE    '    '                                               00002567
129600        TO  DIS-LINE-REC.                                         00002568
129700     PERFORM 8600-DISPLAY-PRT THRU                                00002569
129800          8600-DISPLAY-EXIT.                                      00002570
129900                                                                  00002571
130000     MOVE    '    HDR FILE CLOSED '                               00002572
130100        TO  DIS-LINE-REC.                                         00002573
130200     PERFORM 8600-DISPLAY-PRT THRU                                00002574
130300          8600-DISPLAY-EXIT.                                      00002575
130400                                                                  00002576
130500     MOVE    '    '                                               00002577
130600        TO  DIS-LINE-REC.                                         00002578
130700     PERFORM 8600-DISPLAY-PRT THRU                                00002579
130800          8600-DISPLAY-EXIT.                                      00002580
130900                                                                  00002581
131000     MOVE    'DISPLAY FILE CLOSED '                               00002582
131100        TO  DIS-LINE-REC.                                         00002583
131200     PERFORM 8600-DISPLAY-PRT THRU                                00002584
131300          8600-DISPLAY-EXIT.                                      00002585
131400                                                                  00002586
131500     MOVE    '    '                                               00002587
131600        TO  DIS-LINE-REC.                                         00002588
131700     PERFORM 8600-DISPLAY-PRT THRU                                00002589
131800          8600-DISPLAY-EXIT.                                      00002590
131900                                                                  00002591
132000     MOVE    '**************************************************' 00002592
132100        TO  DIS-LINE-REC.                                         00002593
132200     PERFORM 8600-DISPLAY-PRT THRU                                00002594
132300          8600-DISPLAY-EXIT.                                      00002595
132400                                                                  00002596
132500     MOVE    '    '                                               00002597
132600        TO  DIS-LINE-REC.                                         00002598
132700     PERFORM 8600-DISPLAY-PRT THRU                                00002599
132800          8600-DISPLAY-EXIT.                                      00002600
132900                                                                  00002601
133000                                                                  00002602
133100                                                                  00002603
133200     PERFORM 6000-FINISH-PROC THRU 6900-EXIT.                     00002604
133300                                                                  00002605
133400     CLOSE        ERROR-PRT                                       00002606
133500                  DISPLAY-PRT.                                    00002607
133600                                                                  00002608
133700     DISPLAY 'END OF PROGRAM:  CIDMNET'.                          00002609
133800                                                                  00002610
133900     GOBACK.                                                      00002611
134000                                                                  00002612
134100******************************************************************00002613
134200                                                                  00002614
134300 0500-START.                                                      00002615
134400                                                                  00002616
CIDVAO**9/16/98: COBOLE II DATE CHANGE.
CIDVAO**
CIDVAO     ACCEPT  DATE-IN          FROM  DATE.
CIDVAO     MOVE    DATE-MM-IN       TO    WS-CURR-MO.
CIDVAO     MOVE    DATE-DD-IN       TO    WS-CURR-DA.
CIDVAO     MOVE    DATE-YY-IN       TO    WS-CURR-YR.
CIDVAO**
134500**   MOVE CURRENT-DATE  TO  WS-CURR-DATE.                         00002617
134600     MOVE WS-CURR-DATE  TO  DIS-DATE.                             00002618
134700     MOVE P-C-DATE      TO  WS-CYC-DATE.                          00002619
134800                                                                  00002620
134900     IF   OVERRIDE-MO  >  '00'                                    00002621
135000        MOVE OVERRIDE-DATE TO  WS-CURR-DATE                       00002622
135100                               DC-GREG-DATE-1-EDIT                00002623
135200                               WS-CYC-DATE                        00002624
135300        GO  TO  0500-DATE-SET.                                    00002625
135400                                                                  00002626
135500     MOVE WS-CYC-DATE   TO                                        00002627
135600                            DC-GREG-DATE-1-EDIT.                  00002628
135700                                                                  00002629
135800 0500-DATE-SET.                                                   00002630
135900                                                                  00002631
136000     DISPLAY '0500-CURR-DATE   = ' WS-CURR-DATE.                  00002632
136100     DISPLAY ' '.                                                 00002633
136200                                                                  00002634
136300     DISPLAY '0500-CYCLE-DATE  = ' WS-CYC-DATE.                   00002635
136400     DISPLAY ' '.                                                 00002636
136500                                                                  00002637
136600     MOVE '2'           TO  DC-OPTION-CODE.                       00002638
136700     PERFORM 8500-DATE-CONVERSION.                                00002639
136800                                                                  00002640
136900     MOVE  DC-BIN-DATE-1  TO  SAVE-BIN-DATE.                      00002641
137000                                                                  00002642
137100* N O T E:                    M                                   00002643
137200*   SAVE-BIN-DATE IS THE DATE USED TO SELECT DAILY CHANGES.       00002644
137300                                                                  00002645
137400     MOVE  'N'  TO  ERR-FLAG.                                     00002646
137500                                                                  00002647
137600 0590-EXIT.                                                       00002648
137700     EXIT.                                                        00002649
137800                                                                  00002650
137900                                                                  00002651
138000******************************************************************00002652
138100                                                                  00002653
138200 1000-PROCESS-CLAIM.                                              00002654
138300*1010-READ-CLAIM-FILE-NEXT.                                       00002655
138400                                                                  00002656
138500*    DISPLAY '1010-READ-CLAIM-FILE-NEXT ENTERED'.                 00002657
138600                                                                  00002658
138700     READ CLAIM-FILE  INTO  CLAIM-MASTER
138800       AT  END                                                    00002660
138900          GO  TO  1900-EXIT.                                      00002661
139000                                                                  00002662
110303     IF CL-COMPANY-CD = DTE-CLASIC-COMPANY-CD
110303        CONTINUE
110303     ELSE
110303        GO TO 1000-PROCESS-CLAIM
110303     END-IF
139100     ADD  1   TO  CLAIM-CNTR.                                     00002663
139200                                                                  00002664
139300     IF CL-LAST-MAINT-DT   <  SAVE-BIN-DATE                       00002665
139600        GO TO 1000-PROCESS-CLAIM.                                 00002666
139700                                                                  00002667
139800 1020-PROCESS-CLAIM-FILE.                                         00002668
139900                                                                  00002669
140000     MOVE CL-LAST-MAINT-DT TO                                     00002670
140100                            DC-BIN-DATE-1.                        00002671
140200                                                                  00002672
140300     MOVE ' '           TO  DC-OPTION-CODE.                       00002673
140400     PERFORM 8500-DATE-CONVERSION.                                00002674
140500                                                                  00002675
092016*    MOVE DC-GREG-DATE-1-MDY  TO  WS-ISSUE-DATE.                  00002676
092016*                                                                 00002678
092016*    IF  WS-ISS-YR LESS THAN '40'                                 00002679
092016*        MOVE '20' TO WORK-CENT                                   00002680
092016*      ELSE                                                       00002681
092016*        MOVE '19' TO WORK-CENT.                                  00002682
092016*                                                                 00002683
092016*    MOVE WS-ISS-YR TO WORK-YR.                                   00002684
092016*                                                                 00002685
092016*    MOVE  WORK-DATE  TO IMNET-YR.                                00002686
092016*                                                                 00002687
092016*    MOVE WS-ISS-MO  TO IMNET-MO.                                 00002688
092016*                                                                 00002689
092016*    MOVE WS-ISS-DA  TO IMNET-DA.                                 00002690

092016     move DC-GREG-DATE-CYMD-R    to H-IMNET-DT
142200     MOVE CL-CERT-STATE TO H-ISS-STATE.                           00002692
071904     MOVE CL-CERT-ACCOUNT      TO H-ACCOUNT
142300                                                                  00002693

092016     MOVE   CL-CLAIM-NO  TO  h-claim-no                           00002694
092016*    MOVE   ZEROS        TO  CL-FIRST-3.                          00002695
051112*    MOVE   ZEROS        TO  CL-LAST-2.                           00002696
092016     MOVE CL-cert-prime          TO H-POLICY-NUM
142800                                                                  00002698
142900     MOVE   CL-CLAIM-STATUS  TO  H-STATUS.                        00002699
143000                                                                  00002700
143100     MOVE CL-INCURRED-DT TO  DC-BIN-DATE-1.                       00002701
143200     MOVE ' '            TO  DC-OPTION-CODE.                      00002702
143300     PERFORM 8500-DATE-CONVERSION.                                00002703
143400                                                                  00002704
092016*    MOVE DC-GREG-DATE-1-MDY  TO  WS-ISSUE-DATE.                  00002705
092016*                                                                 00002706
092016*    IF  WS-ISS-YR LESS THAN '40'                                 00002707
092016*        MOVE '20' TO WORK-CENT                                   00002708
092016*      ELSE                                                       00002709
092016*        MOVE '19' TO WORK-CENT.                                  00002710
092016*                                                                 00002711
092016*    MOVE WS-ISS-YR TO  WORK-YR.                                  00002712
092016*                                                                 00002713
092016*    MOVE WORK-DATE TO  H-YR.                                     00002714
092016*    MOVE WS-ISS-MO TO  H-MONTH.                                  00002715
092016*    MOVE WS-ISS-DA TO  H-DAY.                                    00002716
144700                                                                  00002717
092016     move DC-GREG-DATE-CYMD-R    to H-eff-DT
144800     MOVE CL-INSURED-LAST-NAME TO CL-LAST-NAME.                   00002718
144900     MOVE CL-INSURED-1ST-NAME  TO CL-FIRST.                       00002719
145000     MOVE CL-INSURED-MID-INIT  TO CL-INIT.                        00002720
145100                                                                  00002721
145200     MOVE CL-NAME              TO H-INS-NAME.                     00002722
145300                                                                  00002723
145400     PERFORM  8000-FORMAT-NAME THRU 8400-EXIT.                    00002724
145500                                                                  00002725
145600     IF  ERR-FLAG = 'Y'                                           00002726
145700        MOVE  'N'  TO   ERR-FLAG                                  00002727
145800          GO TO 1000-PROCESS-CLAIM.                               00002728
145900                                                                  00002729
146000     MOVE 'A'       TO  H-TRANS-CODE.                             00002730
146100                                                                  00002731
146200     PERFORM  7000-EDITS  THRU   7900-EXIT.                       00002732
146300                                                                  00002733
146400     IF  ERR-FLAG = 'Y'                                           00002734
146500        MOVE  'N'  TO   ERR-FLAG                                  00002735
146600          GO TO 1000-PROCESS-CLAIM.                               00002736

092016     move cl-company-cd          to cm-company-cd
092016     move cl-cert-carrier        to cm-carrier
092016     move cl-cert-grouping       to cm-grouping
092016     move cl-cert-state          to cm-state
092016     move cl-cert-account        to cm-account
092016     move cl-cert-eff-dt         to cm-cert-eff-dt
092016     move cl-cert-no             to cm-cert-no
092016     
092016     read elcert
092016
092016     if elcert-file-status not = '00'
092016        display ' error-elcert-read ' elcert-file-status
092016        ' ' cm-carrier ' ' cm-account ' ' cm-cert-no
092016        perform abend-pgm
092016     end-if
092016
092016     move cm-lf-loan-expire-dt   to dc-bin-date-1
092016     if cm-ah-loan-expire-dt not = spaces and low-values
092016        if cm-ah-loan-expire-dt > dc-bin-date-1
092016           move cm-ah-loan-expire-dt
092016                                 to dc-bin-date-1
092016        end-if
092016     end-if
092016     move ' '                    to dc-option-code
092016     perform 8500-date-conversion
092016                                 thru 8500-exit
092016     move dc-greg-date-cymd-r    to h-expire-dt
092016
092016     WRITE  CLM-REC  FROM  WS-HDR-REC.                            00002997
092016                                                                  00002998
092016     ADD  1   TO  TOT-HDR.                                        00002999

092016*    PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                   00002738
146900     ADD  1   TO  CLAIM-HDR.                                      00002739
147000                                                                  00002740
092016*    MOVE 'U'       TO  H-TRANS-CODE.                             00002741
147200                                                                  00002742
092016*    PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                   00002743
092016*    ADD  1   TO  CLAIM-HDR.                                      00002744
147500                                                                  00002745
147600     MOVE SPACES TO WS-HDR-REC.                                   00002746
147700                                                                  00002747
147800     GO TO 1000-PROCESS-CLAIM.                                    00002748
147900                                                                  00002749
148000 1900-EXIT.                                                       00002750
148100     EXIT.                                                        00002751
148200                                                                  00002752
148300                                                                  00002753
148400******************************************************************00002754
148500                                                                  00002755
148600 3000-PROCESS-CRTC.                                               00002756
148700                                                                  00002757
148800*    DISPLAY '3000-PROCESS-CRTC  ENTERED'.                        00002758
148900                                                                  00002759
149000 3010-READ-CRTC-FILE-NEXT.                                        00002760
149100                                                                  00002761
149200     READ CRTC-FILE  INTO  PENDING-MAINT-TO-CERT-FILE
149300       AT  END                                                    00002763
149400          GO TO 3900-EXIT.                                        00002764
149500                                                                  00002765
110303     IF CC-COMPANY-CD = DTE-CLASIC-COMPANY-CD
110303        CONTINUE
110303     ELSE
110303        GO TO 3000-PROCESS-CRTC
110303     END-IF
149600     ADD  1   TO  CRTC-CNTR.                                      00002766
149700                                                                  00002767
149800     IF CC-LAST-MAINT-DT  <  SAVE-BIN-DATE                        00002768
150100        GO TO 3010-READ-CRTC-FILE-NEXT.                           00002769
150200                                                                  00002772
150300 3020-PROCESS-CRTC-FILE.                                          00002773
150400                                                                  00002774
092016     move spaces                 to ws-hdr-rec
150500     MOVE CC-LAST-MAINT-DT TO                                     00002775
150600                            DC-BIN-DATE-1.                        00002776
150700                                                                  00002777
150800     MOVE ' '           TO  DC-OPTION-CODE.                       00002778
150900     PERFORM 8500-DATE-CONVERSION.                                00002779
151000                                                                  00002780
092016*    MOVE DC-GREG-DATE-1-MDY  TO  WS-ISSUE-DATE.                  00002781
092016*                                                                 00002782
092016*    IF  WS-ISS-YR LESS THAN '40'                                 00002784
092016*        MOVE '20' TO WORK-CENT                                   00002785
092016*      ELSE                                                       00002786
092016*        MOVE '19' TO WORK-CENT.                                  00002787
092016*                                                                 00002788
092016*    MOVE WS-ISS-YR             TO WORK-YR.                       00002789
092016*                                                                 00002790
092016*    MOVE  WORK-DATE             TO IMNET-YR.                     00002791
092016*                                                                 00002792
092016*    MOVE WS-ISS-MO             TO IMNET-MO.                      00002793
092016*                                                                 00002794
092016*    MOVE WS-ISS-DA             TO IMNET-DA.                      00002795
152600                                                                  00002796
092016     move DC-GREG-DATE-CYMD-R    to H-IMNET-DT
152700     MOVE CC-STATE               TO H-ISS-STATE.                  00002797
071904     MOVE CC-ACCOUNT             TO H-ACCOUNT
152800                                                                  00002798
152900     MOVE   CC-CERT-PRIME        TO  H-POLICY-NUM.                00002799
153000                                                                  00002800
153100     MOVE  'A' TO  H-STATUS.                                      00002801
153200                                                                  00002802
153300     MOVE CC-CERT-EFF-DT         TO  DC-BIN-DATE-1.               00002803
153400     MOVE ' '                    TO  DC-OPTION-CODE.              00002804
153500     PERFORM 8500-DATE-CONVERSION.                                00002805
153600                                                                  00002806
092016*    MOVE DC-GREG-DATE-1-MDY     TO  WS-ISSUE-DATE.               00002807
092016*                                                                 00002808
092016*    IF  WS-ISS-YR LESS THAN '40'                                 00002809
092016*        MOVE '20' TO WORK-CENT                                   00002810
092016*      ELSE                                                       00002811
092016*        MOVE '19' TO WORK-CENT.                                  00002812
092016*                                                                 00002813
092016*    MOVE WS-ISS-YR TO  WORK-YR.                                  00002814
092016*                                                                 00002815
092016*    MOVE WORK-DATE TO  H-YR.                                     00002816
092016*    MOVE WS-ISS-MO TO  H-MONTH.                                  00002817
092016*    MOVE WS-ISS-DA TO  H-DAY.                                    00002818

092016     move DC-GREG-DATE-CYMD-R    to H-eff-DT

155000     MOVE SPACES               TO H-INS-NAME.                     00002820
155100     MOVE CC-INSURED-LAST-NAME TO CM-LAST-NAME.                   00002821
155200     MOVE CC-INSURED-1ST-INIT  TO CM-FIRST-INIT.                  00002822
155300     MOVE CC-INSURED-INITIAL2  TO CM-SECOND-INIT.                 00002823
155400                                                                  00002824
155500     MOVE CM-NAME              TO H-INS-NAME.                     00002825
155600                                                                  00002826
155700     PERFORM  8000-FORMAT-NAME THRU 8400-EXIT.                    00002827

092016     move cc-lf-expiry-dt        to dc-bin-date-1
092016     if cc-ah-expiry-dt not = spaces and low-values
092016        if cc-ah-expiry-dt > dc-bin-date-1
092016           move cc-ah-expiry-dt  to dc-bin-date-1
092016        end-if
092016     end-if
092016     move ' '                    to dc-option-code
092016     perform 8500-date-conversion thru 8500-exit
092016     move dc-greg-date-cymd-r    to h-expire-dt
155800                                                                  00002828
155900     IF  ERR-FLAG = 'Y'                                           00002829
156000        MOVE  'N'  TO   ERR-FLAG                                  00002830
156100          GO TO 3010-READ-CRTC-FILE-NEXT.                         00002831
156200                                                                  00002832
156300                                                                  00002833
156400      MOVE 'A'       TO  H-TRANS-CODE.                            00002834
156500                                                                  00002835
156600     PERFORM  7000-EDITS  THRU   7900-EXIT.                       00002836
156700                                                                  00002837
156800     IF  ERR-FLAG = 'Y'                                           00002838
156900        MOVE  'N'  TO   ERR-FLAG                                  00002839
157000          GO TO 3010-READ-CRTC-FILE-NEXT.                         00002840
157100                                                                  00002841
157200      PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                  00002842
157300      ADD  1   TO  CRTC-HDR.                                      00002843
157400                                                                  00002844
092016*     MOVE 'U'       TO  H-TRANS-CODE.                            00002845
092016*                                                                 00002846
092016*     PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                  00002847
092016*     ADD  1   TO  CRTC-HDR.                                      00002848
157900                                                                  00002849
158000      MOVE SPACES TO WS-HDR-REC.                                  00002850
158100                                                                  00002851
158200      GO TO 3010-READ-CRTC-FILE-NEXT.                             00002852
158300                                                                  00002853
158400 3900-EXIT.                                                       00002854
158500     EXIT.                                                        00002855
158600                                                                  00002856
158700******************************************************************00002857
158800                                                                  00002858
158900 4000-PROCESS-PNDB.                                               00002859
159000                                                                  00002860
159100*    DISPLAY '4000-PROCESS-PNDB  ENTERED'.                        00002861
159200                                                                  00002862
159300 4010-READ-PNDB-FILE-NEXT.                                        00002863
159400                                                                  00002864
159500     READ PNDB-FILE  INTO  PENDING-BUSINESS
159600       AT END                                                     00002866
159700         GO TO 4900-EXIT.                                         00002867
159800                                                                  00002868
110303     IF PB-COMPANY-CD = DTE-CLASIC-COMPANY-CD
110303        CONTINUE
110303     ELSE
110303        GO TO 4000-PROCESS-PNDB
110303     END-IF
159900     ADD  1   TO  PNDB-CNTR.                                      00002869
160000                                                                  00002870
160100     IF PB-RECORD-TYPE  =  '9'                                    00002871
160200        GO TO 4010-READ-PNDB-FILE-NEXT.                           00002872
LGC128                                                                  00002873
LGC128     IF IMNET-BYPASS-SW =  'Y'                                    00002874
LGC128        GO TO 4010-READ-PNDB-FILE-NEXT.                           00002875
160300                                                                  00002876
160400     IF PB-LAST-MAINT-DT  <  SAVE-BIN-DATE                        00002877
160700         GO TO 4010-READ-PNDB-FILE-NEXT.                          00002878
160800                                                                  00002879
050206*  THIS SHOULD BYPASS THE DCC CASH CERTS (BIG GUYS)
050206     IF DTE-CLIENT = 'DCC'
050206        IF PB-CERT-PRIME NOT NUMERIC
050206           IF (PB-I-LF-TERM = 1)
050206              OR (PB-I-AH-TERM = 1)
050206              GO TO 4010-READ-PNDB-FILE-NEXT
050206           END-IF
050206        END-IF
050206     END-IF

           .
160900 4020-PROCESS-PNDB-FILE.                                          00002880
161000                                                                  00002881
161100     MOVE PB-LAST-MAINT-DT TO                                     00002882
161200                            DC-BIN-DATE-1.                        00002883
161300                                                                  00002884
161400     MOVE ' '           TO  DC-OPTION-CODE.                       00002885
161500     PERFORM 8500-DATE-CONVERSION.                                00002886
161600                                                                  00002887
092016*    MOVE DC-GREG-DATE-1-MDY  TO  WS-ISSUE-DATE.                  00002888
092016*                                                                 00002890
092016*    IF  WS-ISS-YR LESS THAN '40'                                 00002891
092016*        MOVE '20' TO WORK-CENT                                   00002892
092016*      ELSE                                                       00002893
092016*        MOVE '19' TO WORK-CENT.                                  00002894
092016*                                                                 00002895
092016*    MOVE WS-ISS-YR             TO WORK-YR.                       00002896
092016*                                                                 00002897
092016*    MOVE  WORK-DATE             TO IMNET-YR.                     00002898
092016*                                                                 00002899
092016*    MOVE WS-ISS-MO             TO IMNET-MO.                      00002900
092016*                                                                 00002901
092016*    MOVE WS-ISS-DA             TO IMNET-DA.                      00002902
163200                                                                  00002903
092016     move DC-GREG-DATE-CYMD-R    to H-IMNET-DT
163300     MOVE PB-STATE               TO H-ISS-STATE.                  00002904
163400                                                                  00002905
163500     MOVE  PB-CERT-PRIME  TO  H-POLICY-NUM.                       00002906
164200                                                                  00002914
164300 4025-CONTINUE.                                                   00002915
164400                                                                  00002916
164500     MOVE  'A' TO  H-STATUS.                                      00002917
164600                                                                  00002918
164700     MOVE PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.               00002919
164800     MOVE ' '                    TO  DC-OPTION-CODE.              00002920
164900     PERFORM 8500-DATE-CONVERSION.                                00002921
165000                                                                  00002922
092016*    MOVE DC-GREG-DATE-1-MDY     TO  WS-ISSUE-DATE.               00002923
092016*                                                                 00002924
092016*    IF  WS-ISS-YR LESS THAN '40'                                 00002925
092016*        MOVE '20' TO WORK-CENT                                   00002926
092016*      ELSE                                                       00002927
092016*        MOVE '19' TO WORK-CENT.                                  00002928
092016*                                                                 00002929
092016*    MOVE WS-ISS-YR TO  WORK-YR.                                  00002930
092016*                                                                 00002931
092016*    MOVE WORK-DATE TO  H-YR.                                     00002932
092016*    MOVE WS-ISS-MO TO  H-MONTH.                                  00002933
092016*    MOVE WS-ISS-DA TO  H-DAY.                                    00002934

092016     move DC-GREG-DATE-CYMD-R    to H-eff-dt
071904     MOVE PB-ACCOUNT             TO H-ACCOUNT
166300                                                                  00002935
166400     IF PB-RECORD-TYPE = '1'                                      00002936
166500        MOVE SPACES                    TO H-INS-NAME              00002937
166600        MOVE PB-I-INSURED-LAST-NAME    TO CM-LAST-NAME            00002938
166700        MOVE PB-I-INSURED-1ST-INIT     TO CM-FIRST-INIT           00002939
166800        MOVE PB-I-INSURED-MIDDLE-INIT  TO CM-SECOND-INIT          00002940
112607        INSPECT CM-NAME REPLACING ALL '*' BY ' '
166900        MOVE CM-NAME                   TO H-INS-NAME              00002941
167000        PERFORM  8000-FORMAT-NAME THRU 8400-EXIT                  00002942
092016        move pb-i-lf-expire-dt   to dc-bin-date-1
092016        if pb-i-ah-expire-dt not = spaces and low-values
092016           if pb-i-ah-expire-dt > dc-bin-date-1
092016              move pb-i-ah-expire-dt
092016                                 to dc-bin-date-1
092016           end-if
092016        end-if
092016        move ' '                 to dc-option-code
092016        perform 8500-date-conversion
092016                                 thru 8500-exit
092016        move dc-greg-date-cymd-r to h-expire-dt
167100        GO TO 4030-CONTINUE.                                      00002943
167200                                                                  00002944
167300     IF PB-RECORD-TYPE = '2'                                      00002945
167400        MOVE SPACES                    TO H-INS-NAME              00002946
167500        MOVE PB-CI-LAST-NAME           TO CM-LAST-NAME            00002947
167600        MOVE PB-CI-INITIALS            TO CM-INITS                00002948
167700        MOVE CM-NAME                   TO H-INS-NAME              00002949
167800        PERFORM  8000-FORMAT-NAME THRU 8400-EXIT                  00002950
092016        move pb-ci-lf-expire-dt  to dc-bin-date-1
092016        if pb-ci-ah-expire-dt not = spaces and low-values
092016           if pb-ci-ah-expire-dt > dc-bin-date-1
092016              move pb-ci-ah-expire-dt
092016                                 to dc-bin-date-1
092016           end-if
092016        end-if
092016        move ' '                 to dc-option-code
092016        perform 8500-date-conversion
092016                                 thru 8500-exit
092016        move dc-greg-date-cymd-r to h-expire-dt
167900        GO TO 4030-CONTINUE.                                      00002951
168000                                                                  00002952
168100     IF PB-RECORD-TYPE = '0'                                      00002953
168200        MOVE SPACES                    TO H-INS-NAME              00002954
168300        MOVE PB-M-INSURED-LAST-NAME    TO ML-LAST-NAME            00002955
168400        MOVE PB-M-INSURED-FIRST-NAME   TO ML-FIRST                00002956
168500        MOVE PB-M-INSURED-MID-INIT     TO ML-INIT                 00002957
168600        MOVE ML-NAME                   TO H-INS-NAME              00002958
168700        PERFORM  8000-FORMAT-NAME THRU 8400-EXIT                  00002959
168800        GO TO 4030-CONTINUE.                                      00002960
168900                                                                  00002961
169000 4030-CONTINUE.                                                   00002962
169100                                                                  00002963
169200     IF  ERR-FLAG = 'Y'                                           00002964
169300        MOVE  'N'  TO   ERR-FLAG                                  00002965
169400          GO TO 4010-READ-PNDB-FILE-NEXT.                         00002966
169500                                                                  00002967
169700      MOVE 'A'       TO  H-TRANS-CODE.                            00002969
169800                                                                  00002970
169900     PERFORM  7000-EDITS  THRU   7900-EXIT.                       00002971
170000                                                                  00002972
170100     IF  ERR-FLAG = 'Y'                                           00002973
170200        MOVE  'N'  TO   ERR-FLAG                                  00002974
170300          GO TO 4010-READ-PNDB-FILE-NEXT.                         00002975
170400                                                                  00002976
170500      PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                  00002977
170600      ADD  1   TO  PNDB-HDR.                                      00002978
170700                                                                  00002979
092016*     MOVE 'U' TO  H-TRANS-CODE.                                  00002980
092016*                                                                 00002981
092016*     PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                  00002982
092016*     ADD  1   TO  PNDB-HDR.                                      00002983
171200                                                                  00002984
171300      MOVE SPACES TO WS-HDR-REC.                                  00002985
171400                                                                  00002986
171500      GO TO 4010-READ-PNDB-FILE-NEXT.                             00002987
171600                                                                  00002988
171700 4900-EXIT.                                                       00002989
171800     EXIT.                                                        00002990
171900                                                                  00002991
172000                                                                  00002992
172100******************************************************************00002993
172200                                                                  00002994
172300 5000-WRITE-HDR.                                                  00002995
172400                                                                  00002996
092016     WRITE  CRT-REC  FROM  WS-HDR-REC.                            00002997
172600                                                                  00002998
172700     ADD  1   TO  TOT-HDR.                                        00002999
172800                                                                  00003000
172900                                                                  00003001
173000                                                                  00003002
173100 5900-EXIT.                                                       00003003
173200     EXIT.                                                        00003004
173300                                                                  00003005
173400                                                                  00003006
173500******************************************************************00003007
173600                                                                  00003008
173700 6000-FINISH-PROC.                                                00003009
173800                                                                  00003010
173900     MOVE    SPACES                                               00003011
174000        TO  DIS-LINE-REC.                                         00003012
174100     PERFORM 8600-DISPLAY-PRT THRU                                00003013
174200          8600-DISPLAY-EXIT.                                      00003014
174300                                                                  00003015
174400     MOVE    '     CLAIM FILE RECORDS READ = '                    00003016
174500        TO  DIS-FLD-1.                                            00003017
174600     MOVE         CLAIM-CNTR                                      00003018
174700        TO  DIS-FLD-2.                                            00003019
174800     PERFORM 8600-DISPLAY-PRT THRU                                00003020
174900          8600-DISPLAY-EXIT.                                      00003021
175000                                                                  00003022
175100     MOVE    SPACES                                               00003023
175200        TO  DIS-LINE-REC.                                         00003024
175300     PERFORM 8600-DISPLAY-PRT THRU                                00003025
175400          8600-DISPLAY-EXIT.                                      00003026
175500                                                                  00003027
175600     MOVE    'CLAIM HEADER RECORDS WRITTEN = '                    00003028
175700        TO  DIS-FLD-1.                                            00003029
175800     MOVE    CLAIM-HDR                                            00003030
175900        TO  DIS-FLD-2.                                            00003031
176000     PERFORM 8600-DISPLAY-PRT THRU                                00003032
176100          8600-DISPLAY-EXIT.                                      00003033
176200                                                                  00003034
176300     MOVE    SPACES                                               00003035
176400        TO  DIS-LINE-REC.                                         00003036
176500     PERFORM 8600-DISPLAY-PRT THRU                                00003037
176600          8600-DISPLAY-EXIT.                                      00003038
176700                                                                  00003039
176800     MOVE    '      CRTC FILE RECORDS READ = '                    00003040
176900        TO  DIS-FLD-1.                                            00003041
177000     MOVE    CRTC-CNTR                                            00003042
177100        TO  DIS-FLD-2.                                            00003043
177200     PERFORM 8600-DISPLAY-PRT THRU                                00003044
177300          8600-DISPLAY-EXIT.                                      00003045
177400                                                                  00003046
177500     MOVE    SPACES                                               00003047
177600        TO  DIS-LINE-REC.                                         00003048
177700     PERFORM 8600-DISPLAY-PRT THRU                                00003049
177800          8600-DISPLAY-EXIT.                                      00003050
177900                                                                  00003051
178000     MOVE    ' CRTC HEADER RECORDS WRITTEN = '                    00003052
178100        TO  DIS-FLD-1.                                            00003053
178200     MOVE    CRTC-HDR                                             00003054
178300        TO  DIS-FLD-2.                                            00003055
178400     PERFORM 8600-DISPLAY-PRT THRU                                00003056
178500          8600-DISPLAY-EXIT.                                      00003057
178600                                                                  00003058
178700     MOVE    SPACES                                               00003059
178800        TO  DIS-LINE-REC.                                         00003060
178900     PERFORM 8600-DISPLAY-PRT THRU                                00003061
179000          8600-DISPLAY-EXIT.                                      00003062
179100                                                                  00003063
179200     MOVE    '      PNDB FILE RECORDS READ = '                    00003064
179300        TO  DIS-FLD-1.                                            00003065
179400     MOVE    PNDB-CNTR                                            00003066
179500        TO  DIS-FLD-2.                                            00003067
179600     PERFORM 8600-DISPLAY-PRT THRU                                00003068
179700          8600-DISPLAY-EXIT.                                      00003069
179800                                                                  00003070
179900     MOVE    SPACES                                               00003071
180000        TO  DIS-LINE-REC.                                         00003072
180100     PERFORM 8600-DISPLAY-PRT THRU                                00003073
180200          8600-DISPLAY-EXIT.                                      00003074
180300                                                                  00003075
180400     MOVE    ' PNDB HEADER RECORDS WRITTEN = '                    00003076
180500        TO  DIS-FLD-1.                                            00003077
180600     MOVE    PNDB-HDR                                             00003078
180700        TO  DIS-FLD-2.                                            00003079
180800     PERFORM 8600-DISPLAY-PRT THRU                                00003080
180900          8600-DISPLAY-EXIT.                                      00003081
181000                                                                  00003082
181100     MOVE    SPACES                                               00003083
181200        TO  DIS-LINE-REC.                                         00003084
181300     PERFORM 8600-DISPLAY-PRT THRU                                00003085
181400          8600-DISPLAY-EXIT.                                      00003086
181500                                                                  00003087
181600     MOVE    'TOTAL HEADER RECORDS WRITTEN = '                    00003088
181700        TO  DIS-FLD-1.                                            00003089
181800     MOVE    TOT-HDR                                              00003090
181900        TO  DIS-FLD-2.                                            00003091
182000     PERFORM 8600-DISPLAY-PRT THRU                                00003092
182100          8600-DISPLAY-EXIT.                                      00003093
182200                                                                  00003094
182300     MOVE    SPACES                                               00003095
182400        TO  DIS-LINE-REC.                                         00003096
182500     PERFORM 8600-DISPLAY-PRT THRU                                00003097
182600          8600-DISPLAY-EXIT.                                      00003098
182700                                                                  00003099
182800     MOVE    '      ERROR RECORDS BYPASSED = '                    00003100
182900        TO  DIS-FLD-1.                                            00003101
183000     MOVE    ERR-CNTR                                             00003102
183100        TO  DIS-FLD-2.                                            00003103
183200     PERFORM 8600-DISPLAY-PRT THRU                                00003104
183300          8600-DISPLAY-EXIT.                                      00003105
183400                                                                  00003106
183500     MOVE    SPACES                                               00003107
183600        TO  DIS-LINE-REC.                                         00003108
183700     PERFORM 8600-DISPLAY-PRT THRU                                00003109
183800          8600-DISPLAY-EXIT.                                      00003110
183900                                                                  00003111
184000                                                                  00003112
184100 6900-EXIT.                                                       00003113
184200     EXIT.                                                        00003114
184300                                                                  00003115
184400                                                                  00003116
184500******************************************************************00003117
184600                                                                  00003118
184700 7000-EDITS.                                                      00003119
184800                                                                  00003120
185000                                                                  00003122
185100     MOVE WS-HDR-REC TO EDIT-LINE.                                00003123
185200                                                                  00003124
185300     MOVE ZEROS TO E-SUB.                                         00003125
185400     MOVE SPACES TO ERROR-CODE.                                   00003126
185500                                                                  00003127
185600     ADD  1     TO E-SUB.                                         00003128
185700                                                                  00003129
185800*    IF   E-SUB GREATER THAN 70                                   00003130
071904*    IF   E-SUB GREATER THAN 80                                   00003130
092016     IF   E-SUB GREATER THAN 90                                   00003130
185900          GO TO 7800-EXIT.                                        00003131
186000                                                                  00003132
186100     IF   EDIT-POS (E-SUB)  =  ' '                                00003133
186200       MOVE 'POS 1 (TRANS CODE) IS BLANK '   TO ERROR-CODE        00003134
186300          GO TO 7100-ERROR.                                       00003135
186400                                                                  00003136
186500 7010-LOOP.                                                       00003137
186600                                                                  00003138
186700     MOVE  H-POLICY-NUM  TO  EDIT-CRT-NUM.                        00003139
186800                                                                  00003140
030206*    IF  E-CRT-NUMERIC IS NUMERIC                                 00003141
030206     ADD  11 TO E-SUB
030206     GO TO 7030-PROCESS.
187200                                                                  00003144
187300*    MOVE 'CERT NUMBER NOT ALL NUMERIC '   TO ERROR-CODE.         00003145
187400*    GO TO 7100-ERROR.                                            00003146
187500                                                                  00003147
187600 7020-LOOP.                                                       00003148
187700                                                                  00003149
187800     ADD  1     TO E-SUB.                                         00003150
187900*    IF   E-SUB GREATER THAN 70                                   00003151
071904*    IF   E-SUB GREATER THAN 80                                   00003130
092016     IF   E-SUB GREATER THAN 90                                   00003130
188000          GO TO 7800-EXIT.                                        00003152
188100                                                                  00003153
188200 7030-PROCESS.                                                    00003154
188300                                                                  00003155
188400     IF   EDIT-POS (E-SUB)  =  ' '                                00003156
188500          GO TO 7020-LOOP.                                        00003157
188600                                                                  00003158
188700     IF   EDIT-POS (E-SUB)  =  ZERO OR '9'                        00003159
188800          GO TO 7020-LOOP.                                        00003160
CIDVAO****
CIDVAO     IF   EDIT-POS (E-SUB)  >  '0' AND < '9'
CIDVAO          GO TO 7020-LOOP.
CIDVAO****
189000**** IF   EDIT-POS (E-SUB)  ( > ZERO AND < '9')                   00003162
189100****      GO TO 7020-LOOP.                                        00003163
189200                                                                  00003164
189300     IF   EDIT-POS (E-SUB)  =  'A'  OR 'Z'                        00003165
189400          GO TO 7020-LOOP.                                        00003166
CIDVAO****
CIDVAO     IF   EDIT-POS (E-SUB)  >   'A'  AND  < 'Z'
CIDVAO          GO TO 7020-LOOP.
CIDVAO****
189600**** IF   EDIT-POS (E-SUB)  ( > 'A'  AND  < 'Z')                  00003168
189700****      GO TO 7020-LOOP.                                        00003169
189800                                                                  00003170
189900     IF   EDIT-POS (E-SUB)  =  ','  OR '-' OR '.' OR ','          00003171
190000                               OR ''''                            00003172
190100          GO TO 7020-LOOP.                                        00003173
190200                                                                  00003174
190300     MOVE 'INVALID CHARACTER IN NAME FIELD '   TO ERROR-CODE.     00003175
190400                                                                  00003176
190500 7100-ERROR.                                                      00003177
190600                                                                  00003178
190700     MOVE    WS-HDR-REC                                           00003179
190800        TO  ERR-LINE-REC.                                         00003180
190900     PERFORM 8600-ERR-PRT THRU                                    00003181
191000          8600-ERR-EXIT.                                          00003182
191100                                                                  00003183
191200     MOVE    'ERR REC TYPE = '                                    00003184
191300        TO  ERR-FLD-1.                                            00003185
191400     MOVE    PB-RECORD-TYPE                                       00003186
191500        TO  ERR-FLD-2.                                            00003187
191600     PERFORM 8600-ERR-PRT THRU                                    00003188
191700          8600-ERR-EXIT.                                          00003189
191800                                                                  00003190
191900     MOVE    'ERROR CAUSE  = '                                    00003191
192000        TO  ERR-FLD-1.                                            00003192
192100     MOVE    ERROR-CODE                                           00003193
192200        TO  ERR-FLD-2.                                            00003194
192300     PERFORM 8600-ERR-PRT THRU                                    00003195
192400          8600-ERR-EXIT.                                          00003196
192500                                                                  00003197
192600     MOVE    'ERROR - - '                                         00003198
192700        TO  ERR-FLD-1.                                            00003199
192800     MOVE    WS-HDR-REC                                           00003200
192900        TO  ERR-FLD-2.                                            00003201
193000     PERFORM 8600-ERR-PRT THRU                                    00003202
193100          8600-ERR-EXIT.                                          00003203
193200                                                                  00003204
193300     ADD  1  TO ERR-CNTR                                          00003205
193400     MOVE 'Y' TO ERR-FLAG                                         00003206
193500     MOVE   SPACES                                                00003207
193600        TO  ERR-LINE-REC.                                         00003208
193700     PERFORM 8600-ERR-PRT THRU                                    00003209
193800          8600-ERR-EXIT.                                          00003210
193900                                                                  00003211
194000     GO TO 7900-EXIT.                                             00003212
194100                                                                  00003213
194200 7800-EXIT.                                                       00003214
194300                                                                  00003215
194400*    MOVE    WS-HDR-REC                                           00003216
194500*       TO  DIS-LINE-REC.                                         00003217
194600     MOVE   H-11         TO DISPLAY-INFO-11.                      00003218
194700     MOVE   H-OTHERS     TO DISPLAY-INFO-OTHERS.                  00003219
194800     MOVE   DISPLAY-INFO                                          00003220
194900        TO  DIS-LINE-REC.                                         00003221
195000     PERFORM 8600-DISPLAY-PRT THRU                                00003222
195100          8600-DISPLAY-EXIT.                                      00003223
195200                                                                  00003224
195300     MOVE    SPACES                                               00003225
195400        TO  DIS-LINE-REC.                                         00003226
195500     PERFORM 8600-DISPLAY-PRT THRU                                00003227
195600          8600-DISPLAY-EXIT.                                      00003228
195700                                                                  00003229
195800 7900-EXIT.                                                       00003230
195900     EXIT.                                                        00003231
196000                                                                  00003232
196100******************************************************************00003233
196200                                                                  00003234
196300 8000-FORMAT-NAME.                                                00003235
196400                                                                  00003236
196500     MOVE  SPACES      TO  NAME-FLD-IN.                           00003237
196600     MOVE  H-INS-NAME  TO  NAME-FLD-IN.                           00003238
196700     MOVE  ZEROS TO SPACE-SUB                                     00003239
196800                    SUB-IN                                        00003240
196900                    SUB-OUT.                                      00003241
197000                                                                  00003242
197100 8000-L-NAME.                                                     00003243
197200                                                                  00003244
197300     ADD  1  TO  SUB-IN.                                          00003245
197400     IF   SUB-IN GREATER THAN  15                                 00003246
197500          MOVE  15     TO  SUB-IN                                 00003247
197600          ADD  1  TO  SUB-OUT                                     00003248
197700          MOVE ',' TO  FLD-POS-OUT (SUB-OUT)                      00003249
197800          GO  TO  8000-F-NAME.                                    00003250
197900                                                                  00003251
198000     IF   FLD-POS-IN (SUB-IN) = SPACES                            00003252
198100          ADD  1  TO  SPACE-SUB                                   00003253
980729        ELSE                                                      00003253
980729          MOVE ZERO  TO  SPACE-SUB.                               00003253
198200                                                                  00003254
198300     IF   SPACE-SUB GREATER THAN  1                               00003255
198400          MOVE  ZEROS  TO  SPACE-SUB                              00003256
198500          MOVE  15     TO  SUB-IN                                 00003257
198600          MOVE ',' TO  FLD-POS-OUT (SUB-OUT)                      00003258
198700          GO  TO  8000-F-NAME.                                    00003259
198800                                                                  00003260
198900     ADD  1  TO  SUB-OUT.                                         00003261
199000                                                                  00003262
199100     MOVE  FLD-POS-IN (SUB-IN)                                    00003263
199200       TO  FLD-POS-OUT (SUB-OUT).                                 00003264
199300                                                                  00003265
199400     GO  TO  8000-L-NAME.                                         00003266
199500                                                                  00003267
199600 8000-F-NAME.                                                     00003268
199700                                                                  00003269
199800     ADD  1  TO  SUB-IN.                                          00003270
199900     IF   SUB-IN GREATER THAN  35                                 00003271
200000          GO  TO  8000-RESET-HDR-NAME.                            00003272
200100                                                                  00003273
200200     ADD  1  TO  SUB-OUT.                                         00003274
200300                                                                  00003275
200400     MOVE  FLD-POS-IN (SUB-IN)                                    00003276
200500       TO  FLD-POS-OUT (SUB-OUT).                                 00003277
200600                                                                  00003278
200700     GO  TO  8000-F-NAME.                                         00003279
200800                                                                  00003280
200900 8000-RESET-HDR-NAME.                                             00003281
201000                                                                  00003282
201100     IF   SUB-OUT GREATER THAN  35                                00003283
201200          GO  TO  8000-MOVE-HDR-NAME.                             00003284
201300                                                                  00003285
201400     ADD  1  TO  SUB-OUT.                                         00003286
201500                                                                  00003287
201600     MOVE ' '  TO  FLD-POS-OUT (SUB-OUT).                         00003288
201700                                                                  00003289
201800     GO TO  8000-RESET-HDR-NAME.                                  00003290
201900                                                                  00003291
202000 8000-MOVE-HDR-NAME.                                              00003292
202100                                                                  00003293
202200     MOVE  NAME-FLD-OUT                                           00003294
202300          TO  H-INS-NAME.                                         00003295
202400                                                                  00003296
202500     IF  H-INS-NAME =  SPACES OR ','                              00003297
202600         NEXT  SENTENCE                                           00003298
202700       ELSE                                                       00003299
202800         GO  TO  8400-EXIT.                                       00003300
202900                                                                  00003301
203000     MOVE    '* * * * * * * * * * * * * * * * * * * * * *'        00003302
203100        TO  ERR-LINE-REC.                                         00003303
203200     PERFORM 8600-ERR-PRT THRU                                    00003304
203300          8600-ERR-EXIT.                                          00003305
203400                                                                  00003306
203500     MOVE    WS-HDR-REC                                           00003307
203600        TO  ERR-LINE-REC.                                         00003308
203700     PERFORM 8600-ERR-PRT THRU                                    00003309
203800          8600-ERR-EXIT.                                          00003310
203900                                                                  00003311
204000     MOVE    'NO INSURED NAME '                                   00003312
204100        TO  ERR-LINE-REC.                                         00003313
204200     PERFORM 8600-ERR-PRT THRU                                    00003314
204300          8600-ERR-EXIT.                                          00003315
204400                                                                  00003316
204500     MOVE    'ERROR - - '                                         00003317
204600        TO  ERR-FLD-1.                                            00003318
204700     MOVE    WS-HDR-REC                                           00003319
204800        TO  ERR-FLD-2.                                            00003320
204900     PERFORM 8600-ERR-PRT THRU                                    00003321
205000          8600-ERR-EXIT.                                          00003322
205100                                                                  00003323
205200     ADD  1  TO ERR-CNTR.                                         00003324
205300     MOVE 'Y' TO ERR-FLAG.                                        00003325
205400     MOVE   SPACES                                                00003326
205500        TO  ERR-LINE-REC.                                         00003327
205600     PERFORM 8600-ERR-PRT THRU                                    00003328
205700          8600-ERR-EXIT.                                          00003329
205800                                                                  00003330
205900 8400-EXIT.                                                       00003331
206000     EXIT.                                                        00003332
206100                                                                  00003333
206200******************************************************************00003334
206300                                                                  00003335
206400 8500-DATE-CONVERSION.                                            00003336
206500                                                                  00003337
206600     CALL  'ELDATCX'  USING  DATE-CONVERSION-DATA.                00003338
206700                                                                  00003339
206800 8500-EXIT.                                                       00003340
206900     EXIT.                                                        00003341
207000                                                                  00003342
207100******************************************************************00003343
207200                                                                  00003344
207300 8600-DISPLAY-PRT.                                                00003345
207400                                                                  00003346
207500     IF  DIS-HEAD-SW =  'Y'                                       00003347
207600       MOVE 'N' TO  DIS-HEAD-SW                                   00003348
207700         PERFORM 8600-DISPLAY-HD THRU                             00003349
207800             8600-HD-EXIT                                         00003350
207900           GO TO 8600-DISPLAY-EXIT.                               00003351
208000                                                                  00003352
208100     IF  DIS-LINE-CNT GREATER THAN 77                             00003353
208200         PERFORM 8600-DISPLAY-HD THRU                             00003354
208300             8600-HD-EXIT.                                        00003355
208400                                                                  00003356
208500     ADD +1 TO DIS-LINE-CNT.                                      00003357
208600     MOVE   DISPLAY-LINE TO DISPLAY-INFO.                         00003358
208700     WRITE  DISPLAY-REC AFTER ADVANCING 1 LINE.                   00003359
208800     MOVE   SPACES TO DISPLAY-REC.                                00003360
208900     MOVE   SPACES TO DISPLAY-LINE.                               00003361
209000                                                                  00003362
209100                                                                  00003363
209200 8600-DISPLAY-EXIT.                                               00003364
209300     EXIT.                                                        00003365
209400                                                                  00003366
209500 8600-DISPLAY-HD.                                                 00003367
209600                                                                  00003368
209700     MOVE ZEROS TO DIS-LINE-CNT.                                  00003369
209800     MOVE  DISPLAY-HD-1 TO DISPLAY-INFO.                          00003370
209900     WRITE DISPLAY-REC AFTER ADVANCING NEW-PAGE.                  00003371
210000     ADD  +1  TO DIS-LINE-CNT.                                    00003372
210100     MOVE   SPACES TO DISPLAY-REC.                                00003373
210200     ADD +1 TO DIS-LINE-CNT.                                      00003374
210300     MOVE   DISPLAY-HD-2 TO DISPLAY-INFO.                         00003375
210400     WRITE  DISPLAY-REC AFTER ADVANCING 1 LINE.                   00003376
210500     ADD +1 TO DIS-LINE-CNT.                                      00003377
210600     MOVE   SPACES TO DISPLAY-REC.                                00003378
210700     WRITE  DISPLAY-REC AFTER ADVANCING 1 LINE.                   00003379
210800                                                                  00003380
210900 8600-HD-EXIT.                                                    00003381
211000     EXIT.                                                        00003382
211100 EJECT                                                            00003383
211200                                                                  00003384
211300******************************************************************00003385
211400                                                                  00003386
211500 8600-ERR-PRT.                                                    00003387
211600                                                                  00003388
211700     IF  ERR-HEAD-SW =  'Y'                                       00003389
211800       MOVE 'N' TO  ERR-HEAD-SW                                   00003390
211900         PERFORM 8600-ERR-HD THRU                                 00003391
212000             8600-ERR-HD-EXIT                                     00003392
212100           GO TO 8600-ERR-EXIT.                                   00003393
212200                                                                  00003394
212300     IF  ERR-LINE-CNT GREATER THAN 77                             00003395
212400         PERFORM 8600-ERR-HD THRU                                 00003396
212500             8600-ERR-HD-EXIT.                                    00003397
212600                                                                  00003398
212700     ADD +1 TO ERR-LINE-CNT.                                      00003399
212800     MOVE   ERR-LINE  TO ERR-INFO.                                00003400
212900     WRITE  ERR-REC AFTER ADVANCING 1 LINE.                       00003401
213000     MOVE   SPACES TO ERR-REC.                                    00003402
213100     MOVE   SPACES TO ERR-LINE.                                   00003403
213200                                                                  00003404
213300                                                                  00003405
213400 8600-ERR-EXIT.                                                   00003406
213500     EXIT.                                                        00003407
213600                                                                  00003408
213700 8600-ERR-HD.                                                     00003409
213800                                                                  00003410
213900     MOVE ZEROS TO ERR-LINE-CNT.                                  00003411
214000     MOVE  ERR-HD-1 TO ERR-INFO.                                  00003412
214100     WRITE ERR-REC AFTER ADVANCING NEW-PAGE.                      00003413
214200     ADD  +1  TO ERR-LINE-CNT.                                    00003414
214300     MOVE   SPACES TO ERR-REC.                                    00003415
214400     ADD +1 TO ERR-LINE-CNT.                                      00003416
214500     MOVE   DISPLAY-HD-2 TO ERR-INFO.                             00003417
214600     WRITE  ERR-REC AFTER ADVANCING 1 LINE.                       00003418
214700     ADD +1 TO ERR-LINE-CNT.                                      00003419
214800     MOVE   SPACES TO ERR-REC.                                    00003420
214900     WRITE  ERR-REC AFTER ADVANCING 1 LINE.                       00003421
215000                                                                  00003422
215100 8600-ERR-HD-EXIT.                                                00003423
215200      EXIT.                                                       00003424
215300                                                                  00003425
215400 EJECT                                                            00003426
215500                                                                  00003427
215600******************************************************************00003428
215700                                                                  00003429
215800 ABEND-PGM.                                                       00003430
215900                                                                  00003431
216000     DISPLAY '   ABEND MSG IS = ' WS-ABEND-MESSAGE.               00003432
216100     DISPLAY 'ABEND STATUS IS = ' WS-ABEND-FILE-STATUS.           00003433
216200                                                                  00003434
216300     DISPLAY 'PROGRAM PROBLEMS - JOB WILL ABEND VIA "GOBACK"'.    00003435
216400                                                                  00003436
216500     GOBACK.                                                      00003437
216600                                                                  00003438
216700******************************************************************00003439
216800                                                                  00003440
