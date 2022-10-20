000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.                 CIUM511.                             00000200
000300                                                                  00000300
000400*AUTHOR.     CENTRAL STATES OF OMAHA.                             00000400
000500*            OMAHA, NEBR.                                         00000500
000600                                                                  00000600
000700*DATE-COMPILED.                                                   00000700
000900                                                                  00000800
001000*REMARKS.                                                         00000900
001100*        THIS PROGRAM GENERATES ISSUE/CANCEL TRANSACTIONS FROM    00001000
001200*          UNITED MISSOURI BANK FOR INPUT INTO THE LOGIC SYSTEM   00001100
001300*          THRU LOGIC PROGRAM 'EL512'.                            00001200
001400*          (THIS IS ONE CSO VERSION OF 'EL511').                  00001300
001400                                                                  00001400
001500 ENVIRONMENT DIVISION.                                            00001500
001600 INPUT-OUTPUT SECTION.                                            00001600
001700 FILE-CONTROL.                                                    00001700
001800     SELECT  UMB-LIFE-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.   00001800
001900     SELECT  UMB-LIFE-OUT    ASSIGN TO SYS012-UT-2400-S-SYS012.   00001900
002000     SELECT  RPT-FILE        ASSIGN TO SYS013-UR-1403-S-SYS013.   00002000
002200                                                                  00002200
002300 SKIP3                                                            00002300
002400 DATA DIVISION.                                                   00002400
002500                                                                  00002500
002600 FILE SECTION.                                                    00002600
002700                                                                  00002700
002800******************************************************************00002800
002900**        INPUT TAPE FILE FROM UNITED MISSOURI BANK (UMB)       **00002900
003000******************************************************************00003000
003100                                                                  00003100
003200 FD  UMB-LIFE-IN                                                  00003200
003400     RECORDING MODE IS F                                          00003300
003500     LABEL RECORDS ARE STANDARD                                   00003400
003600     RECORD CONTAINS 150 CHARACTERS                               00003500
003700     BLOCK CONTAINS 0 RECORDS                                     00003600
003800     DATA RECORD IS UMB-LIFE-RECORD.                              00003700
003900                                                                  00003800
004000 01  UMB-LIFE-RECORD            PIC X(150).                       00003900
004100                                                                  00004000
004200******************************************************************00004100
004300**       OUTPUT CARD FILE FO  INPUT TO PROGRAM 'EL512'          **00004200
004400******************************************************************00004300
004500                                                                  00004400
004600 FD  UMB-LIFE-OUT                                                 00004500
004700     RECORDING MODE IS F                                          00004600
004800     LABEL RECORDS ARE STANDARD                                   00004700
004900     RECORD CONTAINS 90 CHARACTERS                                00004800
003700     BLOCK CONTAINS 0 RECORDS                                     00004900
005000     DATA RECORD IS CARD-RECORD.                                  00005000
005100                                                                  00005100
005200 01  CARD-RECORD             PIC X(90).                           00005200
005300                                                                  00005300
005400******************************************************************00005400
005500**                 OUTPUT ERROR REPORT                          **00005500
005600******************************************************************00005600
005700                                                                  00005700
005800 FD  RPT-FILE                                                     00005800
005900     RECORDING MODE IS F                                          00005900
006000     LABEL RECORDS ARE STANDARD                                   00006000
006100     RECORD CONTAINS 133 CHARACTERS                               00006100
006200     BLOCK CONTAINS 0 RECORDS                                     00006200
006300     DATA RECORD IS RPT-REC-OUT.                                  00006300
006400                                                                  00006400
006500 01  RPT-REC-OUT.                                                 00006500
006700     05  RPT-REC                  PIC X(132).                     00006700
006800                                                                  00006800
006900******************************************************************00006900
007100 SKIP3                                                            00007100
007000                                                                  00007110
007200 WORKING-STORAGE SECTION.                                         00007200
007300 77  FILLER  PIC X(32) VALUE '********************************'.  00007300
007400 77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  00007400
007500 77  FILLER  PIC X(32) VALUE '********************************'.  00007500
007600                                                                  00007600
062100 77  LAST-REC-SW            PIC X         VALUE 'N'.              00007700
007700 77  CANC-SW                PIC X         VALUE 'N'.              00007800
007700 77  ERROR-CODE             PIC X(25)     VALUE SPACES.           00008000
007700 77  SAVE-SEQ               PIC X(30)     VALUE SPACES.           00008100
008000 77  RPT-SUB                PIC 99        VALUE ZEROS.            00008300
008200 77  PRINT-SUB              PIC 99        VALUE ZEROS.            00008500
008300 77  RPT-PAGE-CNT           PIC 9999      VALUE ZEROS.            00008600
008400 77  RPT-CANUMB-CNT         PIC 99999     VALUE ZEROS.            00008700
008500 77  RPT-ISS-CNT            PIC 99999     VALUE ZEROS.            00008800
008600 77  RPT-LINE-CNT           PIC 99        VALUE ZEROS.            00008900
008800 77  RPT-CNT                PIC 9999999   VALUE ZEROS.            00009000
008900 77  DETAIL-CNT             PIC 9999      VALUE ZEROS.            00009100
008900 77  DETAIL-CNT-TOT         PIC 9999999   VALUE ZEROS.            00009110
009000 77  IN-CNT                 PIC 9999999   VALUE ZEROS.            00009200
008000 77  NAME-SUB               PIC 99        VALUE ZEROS.            00009300
008000 77  SAVE-LNAME             PIC X(15)     VALUE SPACES.           00009310
008000 77  SAVE-1ST-INIT          PIC X         VALUE SPACES.           00009320
008000 77  SAVE-MIDDLE-INIT       PIC X         VALUE SPACES.           00009330
009100 77  TOT-LIFE-WRITTEN       PIC S9(8)V99  VALUE ZEROS.            00009600
009200 77  TOT-LIFE-CANC          PIC S9(8)V99  VALUE ZEROS.            00009700
009300 77  TOT-AH-WRITTEN         PIC S9(8)V99  VALUE ZEROS.            00009800
009400 77  TOT-AH-CANC            PIC S9(8)V99  VALUE ZEROS.            00009900
009500 77  TOT-CERT-ISS           PIC S9(4)     VALUE ZEROS.            00010000
009600 77  TOT-CERT-CANC          PIC S9(4)     VALUE ZEROS.            00010100
009800 77  SUB1                   PIC 99        VALUE ZEROS.            00010200
010200 77  CANC-CNT               PIC 9999      VALUE ZEROS.            00010300
010200 77  CERT-CNT               PIC 9999      VALUE ZEROS.            00010400
010300 77  FIRST-SW               PIC X         VALUE 'Y'.              00010500
043300                                                                  00010510
043200 01  FILLER   PIC X(44) VALUE                                     00010511
043200      '****  UMB RECORD IN  ****'.                                00010512
045600                                                                  00010513
043400 01  UMB-IN-REC.                                                  00010520
043500     05  UMB-TRANS                 PIC X(02)     VALUE SPACES.    00010530
043500     05  UMB-ACCT                  PIC X(10)     VALUE SPACES.    00010540
043500     05  UMB-CERT-NO.                                             00010562
043500         10  UMB-LOAN-TYPE         PIC X(02)     VALUE SPACES.    00010570
043500         10  UMB-LOAN-NO           PIC X(08)     VALUE SPACES.    00010580
043500     05  UMB-NEW-ACCOUNT.                                         00010590
043500         10  UMB-EFF-DATE.                                        00010591
043500             15  UMB-EFF-MO        PIC X(02)     VALUE SPACES.    00010592
043500             15  UMB-EFF-DA        PIC X(02)     VALUE SPACES.    00010593
043500             15  UMB-EFF-YR        PIC X(02)     VALUE SPACES.    00010594
043500         10  UMB-ADVANCE-DATE.                                    00010595
043500             15  UMB-ADV-MO        PIC X(02)     VALUE SPACES.    00010596
043500             15  UMB-ADV-DA        PIC X(02)     VALUE SPACES.    00010597
043500             15  UMB-ADV-YR        PIC X(02)     VALUE SPACES.    00010598
043500         10  UMB-PAYMENT-DATE.                                    00010599
043500             15  UMB-PMT-MO        PIC X(02)     VALUE SPACES.    00010600
043500             15  UMB-PMT-DA        PIC X(02)     VALUE SPACES.    00010601
043500             15  UMB-PMT-YR        PIC X(02)     VALUE SPACES.    00010602
043500         10  UMB-MATURITY-DATE.                                   00010603
043500             15  UMB-MATUR-MO      PIC X(02)     VALUE SPACES.    00010604
043500             15  UMB-MATUR-DA      PIC X(02)     VALUE SPACES.    00010605
043500             15  UMB-MATUR-YR      PIC X(02)     VALUE SPACES.    00010606
043600         10  UMB-LOAN-TERM         PIC X(03)     VALUE SPACES.    00010607
043600         10  UMB-LIFE-COVG         PIC X(01)     VALUE SPACES.    00010608
043600         10  UMB-LIFE-AMT          PIC 9(06)V99  VALUE ZEROS.     00010609
043600         10  UMB-LIFE-PREM         PIC 9(05)V99  VALUE ZEROS.     00010610
043600         10  UMB-ACC-COVG          PIC X(01)     VALUE SPACES.    00010611
043600         10  UMB-ACC-BENEFIT       PIC 9(05)V99  VALUE ZEROS.     00010612
043600         10  UMB-ACC-PREM          PIC 9(05)V99  VALUE ZEROS.     00010613
043600         10  UMB-OFFICER           PIC X(02)     VALUE SPACES.    00010614
043600         10  UMB-NAME              PIC X(29)     VALUE SPACES.    00010615
043600         10  UMB-AGE               PIC X(02)     VALUE SPACES.    00010616
043600         10  UMB-NAME2             PIC X(29)     VALUE SPACES.    00010617
043600         10  UMB-AGE2              PIC X(02)     VALUE SPACES.    00010618
043600         10  FILLER                PIC X(06)     VALUE SPACES.    00010619
043600     05  UMB-ADJ-CANCEL  REDEFINES  UMB-NEW-ACCOUNT.              00010620
043500         10  UMB-ADJ-CAN-DATE.                                    00010621
043500             15  UMB-ADJ-CAN-MO    PIC X(02).                     00010622
043500             15  UMB-ADJ-CAN-DA    PIC X(02).                     00010623
043500             15  UMB-ADJ-CAN-YR    PIC X(02).                     00010624
043600         10  UMB-ADJ-LIFE-AMT      PIC 9(05)V99.                  00010625
043600         10  UMB-ADJ-ACC-AMT       PIC 9(05)V99.                  00010626
043500         10  UMB-ADJ-EFF-DATE.                                    00010627
043500             15  UMB-ADJ-EFF-MO    PIC X(02).                     00010628
043500             15  UMB-ADJ-EFF-DA    PIC X(02).                     00010629
043500             15  UMB-ADJ-EFF-YR    PIC X(02).                     00010630
043600         10  FILLER                PIC X(102).                    00010631
043300                                                                  00010640
010500 01  OUT-RECORD.                                                  00010700
010600     05  OUT-CERT-NO        PIC X(10)     VALUE SPACES.           00010800
010600     05  OUT-CERT-SFX       PIC X(01)     VALUE SPACES.           00010900
010700     05  OUT-EFF-DATE       PIC X(06)     VALUE SPACES.           00011000
010800     05  OUT-INFO           PIC X(61)     VALUE SPACES.           00011100
010800     05  OUT-TRAN-TYPE      PIC X         VALUE SPACES.           00011200
010800     05  OUT-SEQUENCE       PIC X         VALUE SPACES.           00011300
010600     05  SR-BATCH-NUMB.                                           00011400
010600         10  SR-BATCH-ID    PIC XX        VALUE ZEROS.            00011400
010600         10  SR-BATCH-DA    PIC XX        VALUE ZEROS.            00011400
010600         10  SR-BATCH-NO    PIC 9(2)      VALUE ZEROS.            00011400
010600     05  SR-REC-CNT         PIC 9(4)      VALUE ZEROS.            00011500
013600                                                                  00011510
019500 01  UMB-BATCH-HDR.                                               00011520
020500     05  UMB-CARR-CO.                                             00011530
020600         10  UMB-CARR        PIC X         VALUE '9'.             00011540
020700         10  UMB-COMP        PIC X(6)      VALUE '000000'.        00011550
020800     05  UMB-STATE           PIC XX        VALUE 'KS'.            00011560
020900     05  UMB-ACCT-NO         PIC X(10)     VALUE SPACES.          00011570
010600     05  UMB-BATCH-NUMB.                                          00011400
010600         10  UMB-BATCH-ID    PIC XX        VALUE 'UM'.            00011400
010600         10  UMB-BATCH-DA    PIC XX        VALUE SPACES.          00011400
010600         10  UMB-BATCH-NO    PIC 9(2)      VALUE ZEROS.           00011400
021000     05  UMB-HD-EFF-DT.                                           00011590
021100         10  UMB-HD-EFF-MO   PIC XX        VALUE SPACES.          00011591
021200         10  UMB-HD-EFF-DA   PIC XX        VALUE SPACES.          00011592
021300         10  UMB-HD-EFF-YR   PIC XX        VALUE SPACES.          00011593
020000     05  UMB-CERT-ISS        PIC S9(4)     VALUE ZEROS.           00011594
019600     05  UMB-LIFE-WRITTEN    PIC S9(7)V99  VALUE ZEROS.           00011595
019800     05  UMB-AH-WRITTEN      PIC S9(7)V99  VALUE ZEROS.           00011596
020100     05  UMB-CERT-CANC       PIC S9(4)     VALUE ZEROS.           00011597
019700     05  UMB-LIFE-CANC       PIC S9(7)V99  VALUE ZEROS.           00011598
019900     05  UMB-AH-CANC         PIC S9(7)V99  VALUE ZEROS.           00011599
020300     05  UMB-CLIENT-ID       PIC X(3)      VALUE 'CSO'.           00011600
021400     05  UMB-TRANS-TYPE      PIC X         VALUE '1'.             00011601
020400     05  UMB-SEQUENCE-NO     PIC X         VALUE '0'.             00011602
025000 01  REPORT-HD-LINE.                                              00014200
025100     05  FILLER              PIC X(14)  VALUE ' LIFE WRITTEN '.   00014300
025200     05  FILLER              PIC XX     VALUE SPACES.             00014400
025300     05  FILLER              PIC X(14)  VALUE '     LIFE CANC'.   00014500
025400     05  FILLER              PIC XX     VALUE SPACES.             00014600
025500     05  FILLER              PIC X(14)  VALUE '  A&H WRITTEN '.   00014700
025600     05  FILLER              PIC XX     VALUE SPACES.             00014800
025700     05  FILLER              PIC X(14)  VALUE '      A&H CANC'.   00014900
025800     05  FILLER              PIC XX     VALUE SPACES.             00015000
025900     05  FILLER              PIC X(8)   VALUE ' ISS CNT'.         00015100
026000     05  FILLER              PIC XX     VALUE SPACES.             00015200
026100     05  FILLER              PIC X(8)   VALUE 'CANC CNT'.         00015300
026200     05  FILLER              PIC X(34)  VALUE SPACES.             00015400
027000                                                                  00016200
027100 01  TOTAL-LINE.                                                  00016300
027200     05  T-LIFE-WRITTEN      PIC ZZ,ZZZ,ZZ9.99-.                  00016400
027300     05  FILLER              PIC XX              VALUE SPACES.    00016500
027400     05  T-LIFE-CANC         PIC ZZ,ZZZ,ZZ9.99-.                  00016600
027500     05  FILLER              PIC XX              VALUE SPACES.    00016700
027600     05  T-AH-WRITTEN        PIC ZZ,ZZZ,ZZ9.99-.                  00016800
027700     05  FILLER              PIC XX              VALUE SPACES.    00016900
027800     05  T-AH-CANC           PIC ZZ,ZZZ,ZZ9.99-.                  00017000
027900     05  FILLER              PIC XXXX            VALUE SPACES.    00017100
028000     05  T-CERT-ISS          PIC ZZZ,ZZ9.                         00017200
028100     05  FILLER              PIC XXXX            VALUE SPACES.    00017300
028200     05  T-CERT-CANC         PIC Z,ZZ9.                           00017400
028600     05  FILLER              PIC X(39)           VALUE SPACES.    00017500
028800                                                                  00017600
028900 01  RPT-TOT-LINE.                                                00017700
029000     05  FILLER              PIC X(10)           VALUE SPACES.    00017800
029100     05  FILLER              PIC X(35)  VALUE                     00017900
029200         ' NUMBER OF ISSUES WRITTEN IS - '.                       00018000
029300     05  P-ISS-CNT           PIC ZZ,ZZ9.                          00018100
029400     05  FILLER              PIC X(10)           VALUE SPACES.    00018200
029500     05  FILLER              PIC X(35)  VALUE                     00018300
029600         'NUMBER OF CANCELS WRITTEN IS - '.                       00018400
029700     05  P-CANUMB-CNT        PIC ZZ,ZZ9.                          00018500
029800     05  FILLER              PIC X(10)            VALUE SPACES.   00018600
029900     05  FILLER              PIC X(10)            VALUE SPACES.   00018700
030000                                                                  00018800
030100 01  RPT-HD-LINE1.                                                00018900
030200     05  FILLER              PIC X(26)           VALUE SPACES.    00019000
030300     05  UMB-RPT-ID          PIC X(34)  VALUE                     00019100
030400         ' UNITED MISSOURI BANK REPORT FOR '.                     00019200
030500     05  RPT-REPORT-DATE     PIC X(14)           VALUE SPACES.    00019300
030600     05  FILLER              PIC X(16)           VALUE SPACES.    00019400
030600     05  FILLER              PIC X(15)           VALUE            00019500
030400         'PGM = CIUMB511 '.                                       00019600
030600     05  FILLER              PIC X(14)           VALUE SPACES.    00019700
030700     05  FILLER              PIC X(5)   VALUE         'PAGE '.    00019800
030800     05  RPT-PAGE            PIC Z,ZZ9.                           00019900
030900     05  FILLER              PIC X(5)            VALUE SPACES.    00020000
031000                                                                  00020100
031100 01  RPT-HD-LINE2.                                                00020200
031300     05  FILLER              PIC X(10)  VALUE 'CERT #  '.         00020300
031500     05  FILLER              PIC X(12)  VALUE ' DETAIL INFO'.     00020400
031600     05  FILLER              PIC X(09)  VALUE ' '.                00020500
031700     05  FILLER              PIC X(5)   VALUE SPACES.             00020600
031800     05  FILLER              PIC X(20)  VALUE SPACES.             00020700
032000     05  FILLER              PIC X(20)  VALUE SPACES.             00020800
032100     05  FILLER              PIC X(40)  VALUE                     00020900
032200                 '              RECORD TYPE    '.                 00021000
032300     05  FILLER              PIC X(16)  VALUE SPACES.             00021100
032400                                                                  00021200
032500 01  RPT-PT-LINE.                                                 00021300
032600     05  FILLER              PIC X(80)  VALUE SPACES.             00021400
032600     05  FILLER              PIC X(10)  VALUE SPACES.             00021500
033600     05  PT-ERROR            PIC X(32)  VALUE SPACES.             00021600
033700     05  FILLER              PIC X(08)  VALUE SPACES.             00021700
010900                                                                  00021710
124600 01  SAVE-EFF-DATE.                                               00021760
124700     05 SAVE-EFF-MO         PIC X(02)     VALUE SPACES.           00021770
124600     05 SAVE-EFF-DA         PIC X(02)     VALUE SPACES.           00021780
124600     05 SAVE-EFF-YR         PIC X(02)     VALUE SPACES.           00021781
010900                                                                  00021790
124600 01  SAVE-FULL-ACCT.                                              00021791
124700     05 SAVE-ACCT-FILLER    PIC X(05)     VALUE SPACES.           00021792
124600     05 SAVE-ACCT           PIC X(05)     VALUE SPACES.           00021793
033800                                                                  00021800
007800 01  SAVE-CERT-FULL.                                              00021810
124700     05 SAVE-CERT-FILLER    PIC X(02)     VALUE SPACES.           00021811
124600     05 SAVE-CERT           PIC X(08)     VALUE SPACES.           00021812
033800                                                                  00021820
034800 01  WK-FULL-NAME.                                                00021900
034800     05  FULL-NAME           PIC X      OCCURS 29 TIMES.          00021901
033800                                                                  00021902
034800 01  WK-L-NAME.                                                   00021903
034800     05  L-NAME              PIC X      OCCURS 15 TIMES.          00021904
033800                                                                  00021905
034800 01  WK-DATE.                                                     00021920
034900     05  WK-MO               PIC XX     VALUE SPACES.             00022000
035000     05  FILLER              PIC X      VALUE SPACES.             00022100
035100     05  WK-DA               PIC XX     VALUE SPACES.             00022200
035200     05  FILLER              PIC X      VALUE SPACES.             00022300
035300     05  WK-YR               PIC XX     VALUE SPACES.             00022400
037600                                                                  00022500
038600 01  WORK-DATE.                                                   00022600
038700     12  WORK-DATE-X.                                             00022700
038800         15  WORK-MO-X       PIC XX.                              00022800
038900         15  WORK-DA-X       PIC XX.                              00022900
039000         15  WORK-YR-X       PIC XX.                              00023000
039100     12  WORK-DATE-N    REDEFINES   WORK-DATE-X.                  00023100
039200         15  WORK-MO-N       PIC 99.                              00023200
039300         15  WORK-DA-N       PIC 99.                              00023300
039400         15  WORK-YR-N       PIC 99.                              00023400
039500                                                                  00023500
039100     12  WORK-DATE-IN.                                            00023100
039400         15  WORK-YR-IN      PIC 99.                              00023400
039200         15  WORK-MO-IN      PIC 99.                              00023200
039300         15  WORK-DA-IN      PIC 99.                              00023300
039500                                                                  00023500
040900 01  PRNT-RPT-DATES.                                              00023600
041000     05  RPT-DT-ID.                                               00023700
041200         10  FILLER          PIC X(14)   VALUE 'JANUARY       '.  00023900
041300         10  FILLER          PIC X(14)   VALUE 'FEBRUARY      '.  00024000
041400         10  FILLER          PIC X(14)   VALUE 'MARCH         '.  00024100
041500         10  FILLER          PIC X(14)   VALUE 'APRIL         '.  00024200
041600         10  FILLER          PIC X(14)   VALUE 'MAY           '.  00024300
041700         10  FILLER          PIC X(14)   VALUE 'JUNE          '.  00024400
041800         10  FILLER          PIC X(14)   VALUE 'JULY          '.  00024500
041900         10  FILLER          PIC X(14)   VALUE 'AUGUST        '.  00024600
042000         10  FILLER          PIC X(14)   VALUE 'SEPTEMBER     '.  00024700
042100         10  FILLER          PIC X(14)   VALUE 'OCTOBER       '.  00024800
042200         10  FILLER          PIC X(14)   VALUE 'NOVEMBER      '.  00024900
041100         10  FILLER          PIC X(14)   VALUE 'DECEMBER      '.  00024910
042300                                                                  00025000
042400     05  RPT-DT-05     REDEFINES   RPT-DT-ID.                     00025100
042500         10  RPT-DT        OCCURS 12    PIC X(14).                00025200
042600                                                                  00025300
042700 01  FILLER   PIC X(24) VALUE '** CARD COPY BOOK  **'.            00025400
042800                                                                  00025500
042900*01  OUTPUT-COPYBOOK          COPY ERCPNDBI.                      00025600
042900     COPY ERCPNDBI.                                               00025600
043000 SKIP3                                                            00025700
043100                                                                  00025800
054900******************************************************************00039000
055000 SKIP3                                                            00039100
055100 PROCEDURE DIVISION.                                              00039200
055200 SKIP3                                                            00039300
055400                                                                  00039500
055500* *  START INPUT-ROUTINE PROCESSING.                              00039600
055600                                                                  00039700
055700     ACCEPT  WORK-DATE-IN  FROM DATE.                             00039800
055800                                                                  00039900
055700     MOVE  WORK-YR-IN    TO  WORK-YR-N.                           00039800
055700     MOVE  WORK-MO-IN    TO  WORK-MO-N.                           00039800
055700     MOVE  WORK-DA-IN    TO  WORK-DA-N.                           00039800
055800                                                                  00039900
055900     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040000
056000     DISPLAY '*   CURRENT DATE IS -- ' WK-DATE.                   00040100
056100     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040200
056200                                                                  00040300
056500     OPEN INPUT    UMB-LIFE-IN.                                   00040400
056600                                                                  00040500
056700     OPEN  OUTPUT  UMB-LIFE-OUT                                   00040600
056800                      RPT-FILE.                                   00040700
056900                                                                  00040800
057200     MOVE  WK-MO     TO   UMB-HD-EFF-MO                           00041500
057400     MOVE  WK-DA     TO   UMB-HD-EFF-DA.                          00041600
057300     MOVE  WK-YR     TO   UMB-HD-EFF-YR                           00041700
057900     MOVE  WK-MO     TO   WORK-MO-X.                              00041900
058000     MOVE  WK-YR     TO   WORK-YR-X.                              00042000
058100     MOVE  WK-DA     TO   WORK-DA-X.                              00042100
058200                                                                  00042200
058300     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00042300
058400     DISPLAY '*   WORK-DATE-X  IS -- ' WORK-DATE-X.               00042400
058500     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00042500
158100                                                                  00042600
158100     MOVE  'UM'               TO   UMB-BATCH-ID.                  00042600
158100     MOVE  WORK-DA-IN         TO   UMB-BATCH-DA.                  00042600
167600     ADD    1                 TO   UMB-BATCH-NO.                  00042710
058600                                                                  00042800
058700     MOVE  RPT-DT (WORK-MO-N) TO RPT-REPORT-DATE.                 00042900
058800                                                                  00043000
058900 005-ERR-RPT-HD.                                                  00043100
059000                                                                  00043200
059100     ADD  1                   TO RPT-PAGE-CNT.                    00043300
059200     MOVE   RPT-PAGE-CNT      TO RPT-PAGE.                        00043400
059300     MOVE    6                TO RPT-LINE-CNT.                    00043500
059500     MOVE  RPT-HD-LINE1       TO RPT-REC.                         00043700
059600     WRITE RPT-REC-OUT  AFTER ADVANCING PAGE.                     00043800
059800     MOVE  SPACES             TO RPT-REC.                         00044000
059900     WRITE RPT-REC-OUT  AFTER ADVANCING 1 LINE.                   00044100
060100     MOVE  RPT-HD-LINE2       TO RPT-REC.                         00044300
060200     WRITE RPT-REC-OUT  AFTER ADVANCING 1 LINE.                   00044400
060500     WRITE RPT-REC-OUT  AFTER ADVANCING 1 LINE.                   00044700
060600                                                                  00044800
060700 ERR-RPT-HD-EXIT.                                                 00044900
060800     EXIT.                                                        00045000
060900                                                                  00045100
061400 010-READ-INPUT-FILE.                                             00047100
122000                                                                  00047200
061600      IF  RPT-CNT  GREATER THAN  10                               00047600
061700          GO TO 0200-ERRORS.                                      00047700
061800                                                                  00047800
061900      READ UMB-LIFE-IN                                            00047900
062000         INTO  UMB-IN-REC                                         00048000
062100             AT END                                               00048100
062100                MOVE 'Y'  TO  LAST-REC-SW                         00048200
062100                GO TO LAST-RECORD.                                00048300
064700                                                                  00048310
063900      ADD  1  TO IN-CNT.                                          00048400
062100      MOVE 'N'  TO  LAST-REC-SW.                                  00048500
064400      GO  TO  GET-RECORDS.                                        00049200
064300                                                                  00049300
064400 LAST-RECORD.                                                     00049400
167600                                                                  00049500
158000*     DISPLAY 'LAST ACCT-BREAK R-REC  BUILD '.                    00049600
158100                                                                  00049700
167600      MOVE   '0'               TO   UMB-SEQUENCE-NO.              00049800
167600      MOVE   SAVE-FULL-ACCT    TO   PBI-ACCOUNT.                  00050000
057100      MOVE   SAVE-FULL-ACCT    TO   UMB-ACCT-NO.                  00050010
167600      MOVE   'KS'              TO   PBI-STATE.                    00050200
167600      MOVE   '9'               TO   PBI-CARRIER.                  00050300
167600      MOVE   '000000'          TO   PBI-GROUPING.                 00050400
157900      MOVE   UMB-HD-EFF-DT     TO   PBI-B-BATCH-DT-X.             00050500
157900      MOVE   CERT-CNT          TO   UMB-CERT-ISS                  00050600
157900                                    PBI-B-CERT-ISS-COUNT.         00050700
167600      MOVE   UMB-LIFE-WRITTEN  TO   PBI-B-LF-PRM-WRITTEN.         00050800
167600      MOVE   UMB-AH-WRITTEN    TO   PBI-B-AH-PRM-WRITTEN.         00050900
157900      MOVE   CANC-CNT          TO   UMB-CERT-CANC                 00051000
167600                                    PBI-B-CERT-CAN-COUNT.         00051100
167600      MOVE   UMB-LIFE-CANC     TO   PBI-B-LF-PRM-CANCELLED.       00051200
167600      MOVE   UMB-AH-CANC       TO   PBI-B-AH-PRM-CANCELLED.       00051300
157900      MOVE   'CSO'             TO   PBI-CLIENT-ID.                00051400
157900      MOVE   '1'               TO   PBI-B-TRANS-TYPE.             00051500
157900      MOVE   '0'               TO   PBI-B-SEQUENCE.               00051600
157900                                                                  00051700
158000*     DISPLAY 'UMB-BATCH-HDR  = ' UMB-BATCH-HDR.                  00051800
157900                                                                  00051900
057100      MOVE   UMB-BATCH-HDR     TO  OUT-RECORD.                    00052100
167600      MOVE   UMB-BATCH-NUMB    TO   SR-BATCH-NUMB.                00052110
157900      MOVE   ZEROS             TO   SR-REC-CNT.                   00052200
167600                                                                  00052300
168300      WRITE CARD-RECORD        FROM OUT-RECORD.                   00052400
158100                                                                  00052500
168300*     MOVE  SPACES             TO  CARD-RECORD.                   00052501
168300*     MOVE  SPACES             TO  OUT-RECORD.                    00052502
158100                                                                  00052510
062200      MOVE  RPT-CANUMB-CNT     TO P-CANUMB-CNT                    00052600
062300      MOVE  RPT-ISS-CNT        TO P-ISS-CNT                       00052700
062400      ADD   1                  TO RPT-PAGE-CNT                    00052800
062500      MOVE  RPT-PAGE-CNT       TO RPT-PAGE                        00052900
062600      MOVE  4                  TO RPT-LINE-CNT                    00053000
063500      CLOSE UMB-LIFE-IN                                           00053200
063500            UMB-LIFE-OUT                                          00053210
063600            RPT-FILE.                                             00053300
063800                                                                  00053400
063700      GO  TO  END-OF-JOB.                                         00053500
064300                                                                  00053700
064400 GET-RECORDS.                                                     00053800
064500                                                                  00053900
064600*    DISPLAY 'GET-RECORDS  ENTERED              '.                00054000
064300                                                                  00054600
064400*  CK-TYPE-30.                                                    00054700
064500                                                                  00054800
082600     MOVE  ' '  TO  CANC-SW.                                      00054900
064700                                                                  00055000
064800     IF  UMB-TRANS      = 30                                      00055100
064800*      DISPLAY 'REC TYPE 30 FOUND'                                00055200
082600         MOVE  'Y'  TO  CANC-SW                                   00055300
064400         GO TO CK-ACCT-BREAK.                                     00055500
064300                                                                  00056000
064400*  CK-TYPE-20.                                                    00056100
064700                                                                  00056200
064800     IF  UMB-TRANS      = 20                                      00056300
064800*        DISPLAY 'REC TYPE 20 FOUND'                              00056310
064300         GO TO CK-ACCT-BREAK                                      00056400
064300       ELSE                                                       00056500
064700         GO TO REC-TYPE-ERROR.                                    00056510
064700                                                                  00056700
064400 CK-ACCT-BREAK.                                                   00056810
122000                                                                  00056820
062100     IF  FIRST-SW  =  'Y'                                         00056830
062100       MOVE  'N' TO  FIRST-SW                                     00056840
062100       MOVE  UMB-EFF-DATE TO SAVE-EFF-DATE                        00056850
062100       MOVE  UMB-ACCT TO SAVE-FULL-ACCT.                          00056851
064500                                                                  00056900
064500     IF   UMB-ACCT    = SAVE-FULL-ACCT                            00057000
064500          NEXT  SENTENCE                                          00057100
064500       ELSE                                                       00057200
064500          PERFORM ACCT-BREAK                                      00057300
064500            THRU  ACCT-BREAK-EXIT.                                00057400
064500                                                                  00057530
080500     GO TO PROCESS-DETAILS.                                       00062900
065300                                                                  00063000
064700 REC-TYPE-ERROR.                                                  00063100
080400                                                                  00063200
066300     DISPLAY '******************************************'         00063300
066400     DISPLAY 'REC TYPE ERROR  -  CODE IS ' ERROR-CODE.            00063400
064800     DISPLAY 'UMB-TRANS-TYPE = ' UMB-TRANS-TYPE.                  00063500
066600     DISPLAY ' '                                                  00063600
066600     DISPLAY 'RECORD IS BYPASSED '                                00063700
066600     DISPLAY ' '                                                  00063800
066500     DISPLAY 'UMB-LIFE-RECORD     = ' UMB-LIFE-RECORD             00063900
066600     DISPLAY ' '                                                  00064000
066700     DISPLAY '******************************************'         00064100
080400                                                                  00064200
066800     MOVE SPACES TO ERROR-CODE.                                   00064300
080400                                                                  00064400
066800     GO TO 010-READ-INPUT-FILE.                                   00064500
080400                                                                  00064800
080700 PROCESS-DETAILS.                                                 00064900
080600                                                                  00065000
080700*    DISPLAY 'PROCESS-DETAILS      ENTERED              '.        00065100
080900                                                                  00065200
082100     MOVE  ZEROS  TO  RPT-SUB.                                    00065300
082200                                                                  00065400
082600     IF   CANC-SW =   'Y'                                         00065500
082600        MOVE  ' ' TO CANC-SW                                      00065600
082700          GO  TO  CANCEL-BLD.                                     00065700
082800                                                                  00065800
152200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00065810
152300* * * * * * * *      START ISSUE PROCESSING     * * * * * * * *   00065820
152400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00065830
152900                                                                  00065840
082800*                ICARD                                            00065900
083200                                                                  00066000
083100*    DISPLAY 'ISSUE-BLD           ENTERED  '.                     00066200
083200                                                                  00066500
083300* * * * * * *    BUILD THE CSO INPUT RECORDS   * * * * * * * *.   00066600
083000                                                                  00066900
083300* *  BUILD THE SEQUENCE 1 RECORD   * * * * * * * *.               00067000
083200                                                                  00067100
083300     MOVE   'ISSUE -  SEQUENCE 1 ' TO  SAVE-SEQ.                  00067200
057100                                                                  00067300
057100     ADD    1                  TO TOT-CERT-ISS.                   00067310
057100     ADD    1                  TO  CERT-CNT.                      00067320
057100                                                                  00067330
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00067340
088300                                                                  00067500
088500     MOVE   UMB-EFF-YR         TO  WORK-YR-X.                     00067600
088600     MOVE   UMB-EFF-MO         TO  WORK-MO-X.                     00067700
088700     MOVE   UMB-EFF-DA         TO  WORK-DA-X.                     00067800
088500     MOVE   WORK-DATE-X        TO  PBI-CERT-EFF-DT-X.             00068000
089000                                                                  00068100
088500*    DISPLAY '   '.                                               00068110
088500*    DISPLAY 'EFFECTIVE-DATE = '   PBI-CERT-EFF-DT-X.             00068111
089000                                                                  00068120
090600     MOVE   UMB-CERT-NO        TO  SAVE-CERT-FULL.                00068200
090600     MOVE   UMB-CERT-NO        TO  PBI-CERT-PRIME.                00068300
090600     MOVE   SPACES             TO  PBI-CERT-SFX.                  00068400
089000                                                                  00068500
090600     MOVE   SPACES             TO  PBI-I-INSURED-NAME.            00068501
084700     MOVE   ZEROS              TO  NAME-SUB.                      00068502
084700     MOVE   UMB-NAME           TO  WK-FULL-NAME.                  00068503
089000     PERFORM  GET-NAME         THRU  GET-NAME-EXIT.               00068510
084700     MOVE   SAVE-LNAME         TO  PBI-I-INS-LAST-NAME.           00068520
084700     MOVE   SAVE-1ST-INIT      TO  PBI-I-INS-1ST-INIT.            00068530
084700     MOVE   SAVE-MIDDLE-INIT   TO  PBI-I-INS-MIDDLE-INIT.         00068540
084800                                                                  00068970
083900     IF  UMB-AGE NOT NUMERIC                                      00069000
083900         MOVE 40 TO  UMB-AGE.                                     00069100
084800                                                                  00069200
083900     IF  UMB-AGE GREATER THAN ZEROS                               00069210
083900         NEXT  SENTENCE                                           00069220
083900      ELSE                                                        00069221
083900         MOVE 40 TO  UMB-AGE.                                     00069222
084800                                                                  00069230
083700     MOVE UMB-AGE              TO  PBI-I-INSURED-AGE-X.           00069300
083700     MOVE 'M'                  TO  PBI-I-INSURED-SEX.             00069400
083700     MOVE SPACES               TO  PBI-I-SOC-SEC-NO.              00069500
083700     MOVE SPACES               TO  PBI-I-MEMBER-NO.               00069600
083700     MOVE SPACES               TO  PBI-I-BIRTHDAY-X.              00069700
083700     MOVE SPACES               TO  PBI-I-ENTRY-CD.                00069800
083700     MOVE ' '                  TO  PBI-I-FORCE-CD.                00069900
083700     MOVE '2'                  TO  PBI-TRANS-TYPE.                00070000
083700     MOVE '1'                  TO  PBI-SEQUENCE.                  00070100
083200                                                                  00070200
010800     MOVE  PBI-I-ISSUE-REC-SEQ-1 TO  OUT-INFO.                    00070210
010600     MOVE  SAVE-CERT-FULL        TO  OUT-CERT-NO.                 00070300
010600     MOVE  SPACES                TO  OUT-CERT-SFX.                00070400
088500     MOVE  WORK-DATE-X           TO  OUT-EFF-DATE.                00070500
010800     MOVE  '2'                   TO  OUT-TRAN-TYPE                00070700
010800     MOVE  '1'                   TO  OUT-SEQUENCE.                00070800
088300                                                                  00070900
088300                                                                  00071000
121900     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00071100
122000                                                                  00071200
121900     PERFORM  CLEAR-NAME-WORK  THRU   CLEAR-NAME-EXIT.            00071202
089000                                                                  00071221
084700     MOVE   SPACES             TO  PBI-I-INS-LAST-NAME.           00071222
084700     MOVE   SPACES             TO  PBI-I-INS-1ST-INIT.            00071223
084700     MOVE   SPACES             TO  PBI-I-INS-MIDDLE-INIT.         00071230
084700     MOVE   SPACES             TO  WK-L-NAME.                     00071232
084700     MOVE   SPACES             TO  WK-FULL-NAME.                  00071233
084700     MOVE   SPACES             TO  SAVE-LNAME.                    00071234
084700     MOVE   SPACES             TO  SAVE-1ST-INIT.                 00071235
084700     MOVE   SPACES             TO  SAVE-MIDDLE-INIT.              00071236
089000                                                                  00071240
083300* *  END OF  SEQUENCE 1 RECORD BUILD  * * * * * * * *.            00071300
083200                                                                  00071400
083300* *  BUILD THE SEQUENCE 2 RECORD   * * * * * * * *.               00072000
083200                                                                  00072200
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00072500
083200                                                                  00072600
083300     MOVE  'ISSUE -  SEQUENCE 2 ' TO  SAVE-SEQ.                   00072700
088300                                                                  00072800
088500     MOVE   UMB-EFF-YR         TO  WORK-YR-X.                     00072900
088600     MOVE   UMB-EFF-MO         TO  WORK-MO-X.                     00073000
088700     MOVE   UMB-EFF-DA         TO  WORK-DA-X.                     00073100
088500     MOVE   WORK-DATE-N        TO  PBI-CERT-EFF-DT-X.             00073300
089000                                                                  00073400
090600     MOVE   SAVE-CERT-FULL     TO  PBI-CERT-PRIME.                00073500
090600     MOVE   SPACES             TO  PBI-CERT-SFX.                  00073600
084000                                                                  00073700
090600     MOVE   SPACES             TO  PBI-I-JOINT-NAME.              00073701
089000     MOVE   ZEROS              TO  NAME-SUB.                      00073710
084700     MOVE   UMB-NAME2          TO  WK-FULL-NAME.                  00073720
089000     PERFORM  GET-NAME         THRU  GET-NAME-EXIT.               00073722
084700     MOVE   SAVE-LNAME         TO  PBI-I-JNT-LAST-NAME.           00073723
084700     MOVE   SAVE-1ST-INIT      TO  PBI-I-JNT-1ST-INIT.            00073724
084700     MOVE   SAVE-MIDDLE-INIT   TO  PBI-I-JNT-MIDDLE-INIT.         00073725
089000                                                                  00073726
084700     MOVE   SPACES             TO  SAVE-LNAME.                    00073727
084700     MOVE   SPACES             TO  SAVE-1ST-INIT.                 00073728
084700     MOVE   SPACES             TO  SAVE-MIDDLE-INIT.              00073729
089000                                                                  00073740
084800     MOVE   UMB-AGE2           TO  PBI-I-JOINT-AGE-X.             00074100
084800     MOVE   SPACES             TO  PBI-I-POLICY-FORM-NO.          00074200
084000                                                                  00074300
083700     MOVE   '2'                TO  PBI-TRANS-TYPE.                00074400
083700     MOVE   '2'                TO  PBI-SEQUENCE.                  00074500
083200                                                                  00074600
010800     MOVE   PBI-I-ISSUE-REC-SEQ-2 TO  OUT-INFO.                   00074610
010600     MOVE   SAVE-CERT-FULL     TO  OUT-CERT-NO.                   00074700
010600     MOVE   SPACES             TO  OUT-CERT-SFX.                  00074800
088500     MOVE   WORK-DATE-X        TO  OUT-EFF-DATE.                  00074900
010800     MOVE   '2'                   TO  OUT-TRAN-TYPE               00075100
010800     MOVE   '2'                   TO  OUT-SEQUENCE.               00075200
088300                                                                  00075300
121900     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00075400
088300                                                                  00075401
121900     PERFORM  CLEAR-NAME-WORK  THRU   CLEAR-NAME-EXIT.            00075402
089000                                                                  00075410
084700     MOVE   SPACES             TO  PBI-I-JNT-LAST-NAME.           00075420
084700     MOVE   SPACES             TO  PBI-I-JNT-1ST-INIT.            00075430
084700     MOVE   SPACES             TO  PBI-I-JNT-MIDDLE-INIT.         00075440
084700     MOVE   SPACES             TO  WK-L-NAME.                     00075450
084700     MOVE   SPACES             TO  WK-FULL-NAME.                  00075460
084700     MOVE   SPACES             TO  SAVE-LNAME.                    00075470
084700     MOVE   SPACES             TO  SAVE-1ST-INIT.                 00075480
084700     MOVE   SPACES             TO  SAVE-MIDDLE-INIT.              00075490
089000                                                                  00075491
122000                                                                  00075500
083300* *  END OF  SEQUENCE 2 RECORD BUILD  * * * * * * * *.            00075600
088300                                                                  00075700
083300* *  BUILD THE SEQUENCE 3 RECORD   * * * * * * * *.               00076400
088300                                                                  00076800
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00076900
057100                                                                  00077000
092400*    DISPLAY '* * SEQ3 - FILL THE LIFE FIELDS     * * '.          00077100
083200                                                                  00077200
083300     MOVE  'ISSUE -  SEQUENCE 3 ' TO  SAVE-SEQ.                   00077300
092500                                                                  00077400
088500     MOVE   UMB-EFF-YR         TO  WORK-YR-X.                     00077500
088600     MOVE   UMB-EFF-MO         TO  WORK-MO-X.                     00077600
088700     MOVE   UMB-EFF-DA         TO  WORK-DA-X.                     00077700
088500     MOVE   WORK-DATE-X        TO  PBI-CERT-EFF-DT-X.             00077900
089000                                                                  00078000
090600     MOVE   SAVE-CERT-FULL     TO  PBI-CERT-PRIME.                00078100
090600     MOVE   SPACES             TO  PBI-CERT-SFX.                  00078200
089000                                                                  00078300
092500     IF   UMB-LIFE-AMT  IS NUMERIC                                00078700
092800     MOVE   UMB-LIFE-AMT    TO  PBI-I-LF-BENEFIT-AMT.             00079200
093900                                                                  00079210
092900*    DISPLAY 'LIFE-COVG      = ' UMB-LIFE-COVG.                   00079300
093900                                                                  00079400
094000     IF   UMB-LIFE-COVG       = 'A'                               00079706
094000        MOVE  '01'  TO PBI-I-LF-BENEFIT-TYPE                      00079707
094400          GO  TO   I-LIFE-DONE.                                   00079708
093900                                                                  00079709
094000     IF   UMB-LIFE-COVG       = 'B'                               00079710
094000        MOVE  '02'  TO PBI-I-LF-BENEFIT-TYPE                      00079711
094400          GO  TO   I-LIFE-DONE.                                   00079712
093900                                                                  00079713
094000     IF   UMB-LIFE-COVG       = 'C'                               00079714
094000        MOVE  '04'  TO PBI-I-LF-BENEFIT-TYPE                      00079715
094400          GO  TO   I-LIFE-DONE.                                   00079716
093900                                                                  00079717
094000     IF   UMB-LIFE-COVG       = 'E'                               00079718
094000        MOVE  '03'  TO PBI-I-LF-BENEFIT-TYPE                      00079719
094400          GO  TO   I-LIFE-DONE.                                   00079720
093900                                                                  00079730
094400     GO TO I-LIFE-COVG-CODE-ERROR.                                00079741
093900                                                                  00079742
094400 I-LIFE-COVG-CODE-ERROR.                                          00079743
094000                                                                  00079750
092900     DISPLAY '    '.                                              00079751
092900     DISPLAY 'LIFE COVG ERROR = ' UMB-LIFE-COVG.                  00079752
092900     DISPLAY '    '.                                              00079753
094000                                                                  00079754
094000     MOVE  '00'  TO PBI-I-LF-BENEFIT-TYPE.                        00079760
093900                                                                  00079800
094400 I-LIFE-DONE.                                                     00079900
093000                                                                  00082200
092900*    DISPLAY '    '.                                              00082201
092900*    DISPLAY                                                      00082210
092900*     'GOOD PBI-I-LF-BENEFIT-TYPE = '  PBI-I-LF-BENEFIT-TYPE.     00082211
093000                                                                  00082220
092000     MOVE  UMB-LOAN-TERM            TO  PBI-I-LF-TERM-X.          00082300
093000                                                                  00082400
093500     IF    UMB-LIFE-PREM    NOT  NUMERIC                          00082500
093500           MOVE ZEROS TO UMB-LIFE-PREM.                           00082600
093000                                                                  00082700
093500     MOVE    UMB-LIFE-PREM          TO  PBI-I-LF-PREM-AMT.        00082800
119700                                                                  00083000
093500     MOVE  ZEROS                    TO  PBI-I-LF-CRIT-PERIOD.     00083100
119800                                                                  00083200
093500     MOVE  UMB-MATUR-YR             TO  PBI-I-LF-EXPIRE-YR.       00083300
093500     MOVE  UMB-MATUR-MO             TO  PBI-I-LF-EXPIRE-MO.       00083400
093500     MOVE  UMB-MATUR-DA             TO  PBI-I-LF-EXPIRE-DA.       00083500
120100                                                                  00083600
093500     MOVE  ZEROS                    TO  PBI-I-LF-ALT-BENEFIT-AMT. 00083700
093500     MOVE  ZEROS                    TO  PBI-I-LF-ALT-PREM-AMT.    00083800
093500     MOVE  ZEROS                    TO  PBI-I-TERM-AS-DAYS.       00083900
084000                                                                  00084000
083700     MOVE '2'                       TO  PBI-TRANS-TYPE.           00084100
083700     MOVE '3'                       TO  PBI-SEQUENCE.             00084200
083200                                                                  00084300
157400     ADD  PBI-I-LF-PREM-AMT         TO UMB-LIFE-WRITTEN.          00084400
176600     ADD  PBI-I-LF-PREM-AMT         TO TOT-LIFE-WRITTEN.          00084500
088300                                                                  00084600
010800     MOVE  PBI-I-ISSUE-REC-SEQ-3    TO  OUT-INFO.                 00084610
010600     MOVE  SAVE-CERT-FULL           TO  OUT-CERT-NO.              00084700
010600     MOVE  SPACES                   TO  OUT-CERT-SFX.             00084800
088500     MOVE  WORK-DATE-X              TO  OUT-EFF-DATE.             00084900
010800     MOVE  '2'                      TO  OUT-TRAN-TYPE             00085100
010800     MOVE  '3'                      TO  OUT-SEQUENCE.             00085200
088300                                                                  00085300
121900     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00085400
122000                                                                  00085500
083300* *  END OF  SEQUENCE 3 RECORD BUILD  * * * * * * * *.            00085600
088300                                                                  00085700
083300* *  BUILD THE SEQUENCE 4 RECORD   * * * * * * * *.               00086300
088300                                                                  00086600
106900*   PROCESS-A-H.                                                  00086700
088300                                                                  00086800
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00086900
107000                                                                  00087000
107100*    DISPLAY '* *     FILL THE I-CARD AH FIELDS     * * '.        00087100
083200                                                                  00087300
083300     MOVE  'ISSUE -  SEQUENCE 4 ' TO  SAVE-SEQ.                   00087400
092500                                                                  00087500
088500     MOVE   UMB-EFF-YR         TO  WORK-YR-X.                     00087700
088600     MOVE   UMB-EFF-MO         TO  WORK-MO-X.                     00087800
088700     MOVE   UMB-EFF-DA         TO  WORK-DA-X.                     00087900
088500     MOVE   WORK-DATE-X        TO  PBI-CERT-EFF-DT-X.             00088100
089000                                                                  00088200
090600     MOVE   SAVE-CERT-FULL     TO  PBI-CERT-PRIME.                00088300
090600     MOVE   SPACES             TO  PBI-CERT-SFX.                  00088400
107200                                                                  00088500
119500     IF    UMB-ACC-PREM    NOT NUMERIC                            00088600
119500           MOVE  ZEROS  TO  UMB-ACC-PREM.                         00088700
093900                                                                  00088800
119500     IF    UMB-ACC-PREM    GREATER THAN ZEROS                     00088900
119500           NEXT  SENTENCE                                         00089000
119500       ELSE                                                       00089100
119500           MOVE  ZEROS                  TO  PBI-I-AH-PREM-AMT     00089200
119500           MOVE  ZEROS                  TO  PBI-I-AH-BENEFIT-AMT  00089300
094000           MOVE  ' '                    TO  PBI-I-AH-BENEFIT-POS1 00089400
094000           MOVE  SPACES                 TO  PBI-I-AH-BENEFIT-NO   00089500
119500           MOVE  ZEROS                  TO  PBI-I-AH-TERM         00089600
119500           MOVE  ZEROS                  TO  PBI-I-AH-CRIT-PERIOD  00089700
119500           MOVE  ZEROS                  TO  PBI-I-AH-TERM         00089800
119500           MOVE  ZEROS                  TO  PBI-I-AH-CRIT-PERIOD  00089900
093500           MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-YR    00090000
093500           MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-MO    00090100
093500           MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-DA    00090200
119500           MOVE  ZEROS                  TO  PBI-I-AH-PAYMENT      00090300
010600           MOVE  SAVE-CERT-FULL         TO  OUT-CERT-NO           00090400
010600           MOVE  SPACES                 TO  OUT-CERT-SFX          00090500
088500           MOVE  WORK-DATE-N            TO  OUT-EFF-DATE          00090600
010800           MOVE  PBI-I-ISSUE-REC-SEQ-4  TO  OUT-INFO              00090700
010800           MOVE  '2'                    TO  OUT-TRAN-TYPE         00090800
010800           MOVE  '4'                    TO  OUT-SEQUENCE          00090900
119500           GO  TO  AH-EXIT.                                       00091000
093900                                                                  00091100
119500     MOVE  ZEROS                        TO  PBI-I-AH-PREM-AMT.    00091200
119500     MOVE  UMB-ACC-PREM                 TO  PBI-I-AH-PREM-AMT.    00091300
093900                                                                  00091400
119500     MOVE  ZEROS                        TO  PBI-I-AH-BENEFIT-AMT. 00091500
119500     MOVE  UMB-ACC-BENEFIT              TO  PBI-I-AH-BENEFIT-AMT. 00091600
093900                                                                  00091610
092900*    DISPLAY '    '.                                              00091620
092900*    DISPLAY 'UMB-ACC-COVG         = ' UMB-ACC-COVG.              00091630
093900                                                                  00091700
094000     MOVE SPACES TO PBI-I-AH-BENEFIT-POS1.                        00091710
093900                                                                  00091720
094000     IF   UMB-ACC-COVG              = ' '  OR  '0'                00091800
094000        MOVE  '  '  TO PBI-I-AH-BENEFIT-NO                        00092000
120100          GO  TO  AH-COVG-DONE.                                   00092100
093900                                                                  00092101
094000     IF   UMB-ACC-COVG              = '1'                         00092130
094000        MOVE  '01'  TO PBI-I-AH-BENEFIT-NO                        00092150
120100          GO  TO  AH-COVG-DONE.                                   00092160
093900                                                                  00092170
094000     IF   UMB-ACC-COVG              = '2'                         00092180
094000        MOVE  '54'  TO PBI-I-AH-BENEFIT-NO                        00092200
120100          GO  TO  AH-COVG-DONE.                                   00092300
093900                                                                  00092400
094400*  I-AH-COVG-CODE-ERROR.                                          00092500
094000                                                                  00092600
092900     DISPLAY '    '.                                              00092610
092900     DISPLAY 'A&H COVG ERROR = ' UMB-ACC-COVG.                    00092611
092900     DISPLAY '    '.                                              00092612
094000                                                                  00092613
094000     MOVE SPACES TO PBI-I-AH-BENEFIT-POS1.                        00092614
094000     MOVE  '  '  TO PBI-I-LF-BENEFIT-TYPE.                        00092615
093900                                                                  00092616
120100 AH-COVG-DONE.                                                    00092617
093900                                                                  00092618
092900*    DISPLAY '  '.                                                00092720
092900*    DISPLAY 'GOOD AH-COVG-CODE = ' PBI-I-AH-BENEFIT-TYPE.        00092730
093900                                                                  00092820
119500     MOVE  UMB-LOAN-TERM            TO  PBI-I-AH-TERM.            00092900
118500                                                                  00093000
119500     MOVE  ZEROS                    TO  PBI-I-AH-CRIT-PERIOD.     00093100
119800                                                                  00093200
093500     MOVE  UMB-MATUR-YR             TO  PBI-I-AH-EXPIRE-YR.       00093300
093500     MOVE  UMB-MATUR-MO             TO  PBI-I-AH-EXPIRE-MO.       00093400
093500     MOVE  UMB-MATUR-DA             TO  PBI-I-AH-EXPIRE-DA.       00093500
093900                                                                  00093600
119500     MOVE  UMB-LIFE-AMT             TO  PBI-I-AH-PAYMENT.         00093700
093900                                                                  00093800
083700     MOVE '2'                       TO  PBI-TRANS-TYPE.           00093900
083700     MOVE '4'                       TO  PBI-SEQUENCE.             00094000
083200                                                                  00094100
157500     ADD  PBI-I-AH-PREM-AMT         TO UMB-AH-WRITTEN.            00094200
176800     ADD  PBI-I-AH-PREM-AMT         TO TOT-AH-WRITTEN.            00094300
083200                                                                  00094400
010800     MOVE  PBI-I-ISSUE-REC-SEQ-4    TO  OUT-INFO.                 00094410
010600     MOVE  SAVE-CERT-FULL           TO  OUT-CERT-NO.              00094500
010600     MOVE  SPACES                   TO  OUT-CERT-SFX.             00094600
088500     MOVE  WORK-DATE-X              TO  OUT-EFF-DATE.             00094700
010800     MOVE  '2'                      TO  OUT-TRAN-TYPE             00094900
010800     MOVE  '4'                      TO  OUT-SEQUENCE.             00095000
088300                                                                  00095100
120100 AH-EXIT.                                                         00095200
088300                                                                  00095600
121900     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00095700
122000                                                                  00095800
083300* *  END OF  SEQUENCE 4 RECORD BUILD  * * * * * * * *.            00096010
088300                                                                  00096011
083300* *  BUILD THE SEQUENCE 5 RECORD   * * * * * * * *.               00096030
088300                                                                  00096033
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00096034
083000                                                                  00096035
083300     MOVE  'ISSUE -  SEQUENCE 5 ' TO  SAVE-SEQ.                   00096037
083200                                                                  00096038
122200     MOVE  UMB-OFFICER  TO  PBI-I-LOAN-OFFICER.                   00096300
088300                                                                  00096310
010800     MOVE  PBI-I-ISSUE-REC-SEQ-5    TO  OUT-INFO.                 00096320
010600     MOVE  SAVE-CERT-FULL           TO  OUT-CERT-NO.              00096330
010600     MOVE  SPACES                   TO  OUT-CERT-SFX.             00096340
088500     MOVE  WORK-DATE-X              TO  OUT-EFF-DATE.             00096350
010800     MOVE  '2'                      TO  OUT-TRAN-TYPE             00096360
010800     MOVE  '5'                      TO  OUT-SEQUENCE.             00096370
122000                                                                  00096400
121900     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00096410
122000                                                                  00096420
083300* *  END OF  SEQUENCE 5 RECORD BUILD  * * * * * * * *.            00096500
083000                                                                  00096501
083300*    DISPLAY 'END OF  I-CARD-BUILD  '.                            00096510
122000                                                                  00096520
122000     MOVE SPACES TO UMB-IN-REC.                                   00096530
122000                                                                  00096540
122100     GO  TO  010-READ-INPUT-FILE.                                 00096900
092500                                                                  00097000
084800                                                                  00097100
084800 CLEAR-NAME-WORK.                                                 00097200
084800                                                                  00097300
084800     MOVE ZEROS TO NAME-SUB.                                      00097400
084800                                                                  00097500
084800 CLEAR-NAME-LOOP.                                                 00097600
084800                                                                  00097700
084800     ADD  1  TO NAME-SUB.                                         00097800
084800     IF  NAME-SUB GREATER THAN 15                                 00097900
084800       MOVE ZEROS TO NAME-SUB                                     00098000
084800         GO TO CLEAR-NAME-EXIT.                                   00098100
084800                                                                  00098200
084800     MOVE SPACE  TO L-NAME (NAME-SUB).                            00098300
084800                                                                  00098400
084800     GO TO CLEAR-NAME-LOOP.                                       00098500
084800                                                                  00098600
084800 CLEAR-NAME-EXIT.                                                 00098700
084800      EXIT.                                                       00098800
122500                                                                  00100100
122600* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00100200
122700* * * * * * * * *   END  ISSUE   PROCESSING   * * * * * * * * *   00100300
122800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00100400
122900                                                                  00100500
151300                                                                  00100510
152200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00100520
152300* * * * * * * *   START  CANCELLATION PROCESSING  * * * * * * *   00100530
152400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00100540
152100                                                                  00100550
122900*                   ACARD                                         00100600
123000 CANCEL-BLD.                                                      00100700
088300                                                                  00100800
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00100900
123100                                                                  00101000
123200*    DISPLAY 'CANCEL-BLD            ENTERED '.                    00101100
092500                                                                  00101200
123200*    DISPLAY 'UMB-IN REC  = '  UMB-IN-REC.                        00101201
092500                                                                  00101202
092500     ADD    1                  TO  CANC-CNT.                      00101210
092500                                                                  00101220
123200     DISPLAY 'UMB-ADJ-EFF-DATE = ' UMB-ADJ-EFF-DATE.              00101230
123200     DISPLAY 'UMB-ADJ-CAN-DATE = ' UMB-ADJ-CAN-DATE.              00101231
092500                                                                  00101240
088500     MOVE   UMB-ADJ-EFF-YR     TO  WORK-YR-X.                     00101300
088600     MOVE   UMB-ADJ-EFF-MO     TO  WORK-MO-X.                     00101400
088700     MOVE   UMB-ADJ-EFF-DA     TO  WORK-DA-X.                     00101500
092500                                                                  00101510
123200     DISPLAY 'EFF-WORK-DATE-X = '    WORK-DATE-X.                 00101520
092500                                                                  00101530
088700     MOVE   WORK-DATE-X        TO  PBI-CERT-EFF-DT-X.             00101600
092500                                                                  00101700
088500     MOVE   UMB-ADJ-CAN-YR     TO  WORK-YR-X.                     00101701
088600     MOVE   UMB-ADJ-CAN-MO     TO  WORK-MO-X.                     00101702
088700     MOVE   UMB-ADJ-CAN-DA     TO  WORK-DA-X.                     00101703
092500                                                                  00101704
123200     DISPLAY 'CAN-WORK-DATE-X = '    WORK-DATE-X.                 00101705
092500                                                                  00101706
088700     MOVE   WORK-DATE-X        TO  PBI-C-LF-CANCEL-DATE.          00101710
088700     MOVE   WORK-DATE-X        TO  PBI-C-AH-CANCEL-DATE.          00101711
089000                                                                  00101720
090600     MOVE   UMB-CERT-NO        TO  SAVE-CERT-FULL.                00101740
090600     MOVE   UMB-CERT-NO        TO  PBI-CERT-PRIME.                00101741
090600     MOVE   SPACES             TO  PBI-CERT-SFX.                  00101750
089000                                                                  00101760
090600     MOVE   SPACES             TO  PBI-C-LIVES-X.                 00102100
090600     MOVE   SPACES             TO  PBI-C-FORCE-CD.                00102200
084000                                                                  00102300
125400     MOVE SPACES               TO   PBI-C-INSURED-NAME.           00102400
125500                                                                  00102500
134300* *     FILL THE CANCEL LIFE FIELDS     * * * * * * * *           00102700
134900                                                                  00102800
134500     IF    UMB-ADJ-LIFE-AMT          NOT NUMERIC                  00103400
134500           MOVE ZEROS TO UMB-ADJ-LIFE-AMT.                        00103500
134400                                                                  00103600
134500     MOVE  ZEROS                     TO PBI-C-LF-PREM-REFUND.     00103700
134500     ADD   UMB-ADJ-LIFE-AMT          TO PBI-C-LF-PREM-REFUND.     00103800
128600                                                                  00104100
134300* *     FILL THE CANCEL A-H  FIELDS     * * * * * * * *           00104200
088300                                                                  00104300
134500     IF    UMB-ADJ-ACC-AMT          NOT NUMERIC                   00105000
134500           MOVE ZEROS TO UMB-ADJ-ACC-AMT.                         00105100
134400                                                                  00105200
134500     IF   PBI-C-AH-PREM-REFUND      NOT NUMERIC                   00105210
134500          MOVE ZEROS                TO PBI-C-AH-PREM-REFUND.      00105300
134500     ADD  UMB-ADJ-ACC-AMT           TO PBI-C-AH-PREM-REFUND.      00105400
093900                                                                  00105600
083700     MOVE '3'                       TO  PBI-C-TRANS-TYPE.         00105700
083700     MOVE '1'                       TO  PBI-C-SEQUENCE.           00105800
089000                                                                  00107000
090600     MOVE   SAVE-CERT-FULL         TO  PBI-CERT-PRIME.            00107100
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00107200
084000                                                                  00107300
010800     MOVE  PBI-C-CANCEL-REC-SEQ-1  TO  OUT-INFO.                  00107310
137000                                                                  00107320
148000*    DISPLAY '(CANCEL) OUT-RECORD  =  ' OUT-RECORD.               00107330
137000                                                                  00107340
010600     MOVE  SAVE-CERT-FULL          TO  OUT-CERT-NO.               00107400
010600     MOVE  SPACES                  TO  OUT-CERT-SFX.              00107500
092500                                                                  00107510
088500     MOVE   UMB-ADJ-EFF-YR         TO  WORK-YR-X.                 00107520
088600     MOVE   UMB-ADJ-EFF-MO         TO  WORK-MO-X.                 00107530
088700     MOVE   UMB-ADJ-EFF-DA         TO  WORK-DA-X.                 00107540
010700     MOVE  WORK-DATE-X             TO  OUT-EFF-DATE.              00107600
092500                                                                  00107700
010800     MOVE  '3'                     TO  OUT-TRAN-TYPE.             00107800
010800     MOVE  '1'                     TO  OUT-SEQUENCE.              00107900
088300                                                                  00108000
144900     ADD  1                        TO  DETAIL-CNT                 00108100
144900                                       DETAIL-CNT-TOT.            00108110
144900     MOVE  DETAIL-CNT              TO  SR-REC-CNT.                00108200
144900     MOVE  UMB-BATCH-NUMB          TO  SR-BATCH-NUMB.             00108300
088300                                                                  00108400
148000     WRITE CARD-RECORD             FROM OUT-RECORD.               00108500
088300                                                                  00108501
148000*    MOVE  SPACES                  TO  CARD-RECORD.               00108510
148000*    MOVE  SPACES                  TO  OUT-RECORD.                00108520
140300                                                                  00108600
143600     ADD  PBI-C-LF-PREM-REFUND     TO UMB-LIFE-CANC.              00108800
143700     ADD  PBI-C-AH-PREM-REFUND     TO UMB-AH-CANC.                00108900
176500                                                                  00109000
143500     ADD  1                        TO TOT-CERT-CANC.              00109100
143600     ADD  PBI-C-LF-PREM-REFUND     TO TOT-LIFE-CANC.              00109200
143700     ADD  PBI-C-AH-PREM-REFUND     TO TOT-AH-CANC.                00109300
146200                                                                  00109500
146300     ADD   1                       TO RPT-CANUMB-CNT.             00109600
146400     ADD   1                       TO RPT-LINE-CNT.               00109700
146500     IF   RPT-LINE-CNT GREATER THAN  60                           00109800
146600          PERFORM   005-ERR-RPT-HD THRU ERR-RPT-HD-EXIT.          00109900
146800     MOVE  OUT-RECORD              TO  RPT-PT-LINE.               00110100
147100     MOVE  'CANCEL '               TO  PT-ERROR.                  00110200
147200                                                                  00110300
147900     MOVE  RPT-PT-LINE             TO  RPT-REC.                   00110400
148000     WRITE RPT-REC-OUT AFTER ADVANCING 1  LINE.                   00110500
148100     ADD  1                        TO  RPT-LINE-CNT.              00110600
148300     MOVE  SPACES                  TO RPT-REC.                    00110800
148400     WRITE RPT-REC-OUT  AFTER ADVANCING 1  LINE.                  00110900
148500     MOVE  SPACES                  TO RPT-PT-LINE.                00111000
148600                                                                  00111100
122000*    MOVE SPACES TO UMB-IN-REC.                                   00111120
122000*    MOVE SPACES TO OUT-RECORD.                                   00111121
122000                                                                  00111130
137200     GO  TO  010-READ-INPUT-FILE.                                 00111200
137200                                                                  00111300
152200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00111700
152300* * * * * * * *   END  CANCELLATION PROCESSING  * * * * * * * *   00111800
152400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00111900
152100                                                                  00112000
153000******* * * * * * * * * * * * * * * * * * * * * * * * * * * *     00112100
152100                                                                  00112200
153200 0150-WRITE-DATA-CARD.                                            00112800
153300                                                                  00112900
153400*    DISPLAY '0150-WRITE-DATA-CARD ENTERED              '.        00113000
152600                                                                  00113100
152700     MOVE ZEROS                TO  SUB1.                          00113200
157200                                                                  00113300
158000*    DISPLAY 'ISSUE WRITE          ENTERED              '.        00113800
088300                                                                  00113900
168300*    DISPLAY  OUT-RECORD.                                         00114700
158100                                                                  00114800
144900     ADD  1                    TO  DETAIL-CNT                     00114900
144900                                   DETAIL-CNT-TOT.                00114910
144900     MOVE  DETAIL-CNT          TO  SR-REC-CNT.                    00115000
144900     MOVE  UMB-BATCH-NUMB      TO  SR-BATCH-NUMB.                 00115100
168300     WRITE CARD-RECORD         FROM OUT-RECORD.                   00115200
158100                                                                  00115300
148000*    MOVE  SPACES                  TO  CARD-RECORD.               00115310
148000*    MOVE  SPACES                  TO  OUT-RECORD.                00115320
140300                                                                  00115330
159200     MOVE  ZERO                TO PRINT-SUB.                      00115400
159300                                                                  00115500
159800     ADD   1                   TO RPT-ISS-CNT.                    00116000
159900     ADD   1                   TO RPT-LINE-CNT.                   00116100
160000     IF   RPT-LINE-CNT GREATER THAN  60                           00116200
160100          PERFORM   005-ERR-RPT-HD THRU ERR-RPT-HD-EXIT.          00116300
160300     MOVE  OUT-RECORD          TO  RPT-PT-LINE.                   00116500
160300     MOVE  SAVE-SEQ            TO  PT-ERROR.                      00116600
161400     MOVE  RPT-PT-LINE         TO  RPT-REC.                       00116700
161500     WRITE RPT-REC-OUT AFTER ADVANCING 1  LINE.                   00116800
161600     ADD  1                    TO  RPT-LINE-CNT.                  00116900
161800     MOVE  SPACES              TO RPT-REC.                        00117100
161900     WRITE RPT-REC-OUT  AFTER ADVANCING 1  LINE.                  00117200
162000*    MOVE  SPACES              TO RPT-PT-LINE.                    00117300
162000*    MOVE  SPACES              TO OUT-RECORD.                     00117310
162100                                                                  00117400
165600 0150-EXIT.                                                       00117500
165700     EXIT.                                                        00117600
165800                                                                  00117800
165900 0200-ERRORS.                                                     00117900
166000                                                                  00118000
166100*    DISPLAY '0200-ERRORS                 ENTERED              '. 00118100
166200                                                                  00118200
166300     DISPLAY '******************************************'         00118300
166400     DISPLAY 'INPUT FILE HAS MORE THAN 10 ERRORS '                00118400
166500     DISPLAY ' '                                                  00118500
166600     DISPLAY '******************************************'         00118600
166800                                                                  00118610
166700     CLOSE   UMB-LIFE-IN                                          00118700
167000             UMB-LIFE-OUT.                                        00118900
167100                                                                  00119000
167200     GO TO END-OF-JOB.                                            00119100
167300                                                                  00119200
089000 GET-NAME.                                                        00119420
089000                                                                  00119421
089000     ADD  1  TO NAME-SUB.                                         00119422
089000                                                                  00119423
089000     IF   FULL-NAME (NAME-SUB)  =  ','                            00119424
084700       MOVE   WK-L-NAME TO  SAVE-LNAME                            00119425
089000           GO TO GET-1ST-INIT.                                    00119426
089000                                                                  00119427
089000     IF  NAME-SUB  GREATER THAN 15                                00119428
084700       MOVE   WK-L-NAME TO  SAVE-LNAME                            00119429
089000           GO TO FIND-LNAME-END.                                  00119430
167600                                                                  00119453
167600     MOVE FULL-NAME (NAME-SUB)                                    00119454
167600          TO L-NAME (NAME-SUB)                                    00119455
089000             GO TO GET-NAME.                                      00119456
167600                                                                  00119457
089000 FIND-LNAME-END.                                                  00119459
089000                                                                  00119460
089000     IF   FULL-NAME (NAME-SUB)  =  ','                            00119461
089000          GO TO GET-1ST-INIT.                                     00119464
167600                                                                  00119465
089000     ADD  1  TO NAME-SUB.                                         00119466
167600                                                                  00119467
089000     IF  NAME-SUB  GREATER THAN 29                                00119468
084700       MOVE   ' '              TO  SAVE-1ST-INIT.                 00119469
084700       MOVE   ' '              TO  SAVE-MIDDLE-INIT.              00119470
089000          GO TO GET-NAME-EXIT.                                    00119471
084800                                                                  00119472
089000     GO TO FIND-LNAME-END.                                        00119473
167600                                                                  00119474
089000                                                                  00119480
089000 GET-1ST-INIT.                                                    00119481
167600                                                                  00119482
089000     ADD  1  TO NAME-SUB.                                         00119483
167600                                                                  00119484
089000     IF  NAME-SUB  GREATER THAN 29                                00119485
084700       MOVE   ' '              TO  SAVE-1ST-INIT                  00119486
084700       MOVE   ' '              TO  SAVE-MIDDLE-INIT               00119487
089000          GO TO GET-NAME-EXIT.                                    00119488
089000                                                                  00119489
089000     IF   FULL-NAME (NAME-SUB)  =  ','                            00119490
089000          GO TO GET-1ST-INIT.                                     00119491
089000                                                                  00119492
089000     IF   FULL-NAME (NAME-SUB)  =  ' '                            00119493
089000          GO TO GET-1ST-INIT.                                     00119494
084800                                                                  00119495
084700*    DISPLAY 'FIRST INITIAL = ' FULL-NAME (NAME-SUB).             00119496
084700     MOVE   FULL-NAME (NAME-SUB) TO   SAVE-1ST-INIT.              00119497
089000                                                                  00119498
089000 GET-2ND-INIT.                                                    00119499
167600                                                                  00119500
089000     ADD  1  TO NAME-SUB.                                         00119501
167600                                                                  00119502
089000     IF  NAME-SUB  GREATER THAN 29                                00119503
084700       MOVE   ' '              TO  SAVE-MIDDLE-INIT               00119505
089000          GO TO GET-NAME-EXIT.                                    00119506
089000                                                                  00119507
089000     IF   FULL-NAME (NAME-SUB)  =  ' '                            00119508
089000          NEXT SENTENCE                                           00119509
089000       ELSE                                                       00119510
089000          GO TO GET-2ND-INIT.                                     00119511
167600                                                                  00119512
089000     ADD  1  TO NAME-SUB.                                         00119513
089000                                                                  00119514
084700*    DISPLAY 'MIDDLE-INITIAL = ' FULL-NAME (NAME-SUB).            00119515
084700     MOVE   FULL-NAME (NAME-SUB) TO   SAVE-MIDDLE-INIT.           00119516
167600                                                                  00119517
089000 GET-NAME-EXIT.                                                   00119522
089000     EXIT.                                                        00119523
084800                                                                  00119530
064500 ACCT-BREAK.                                                      00119600
167600                                                                  00119700
158000*     DISPLAY 'ACCT-BREAK           ENTERED              '.       00119800
158100                                                                  00119900
167600      MOVE   '0'               TO   UMB-SEQUENCE-NO.              00120000
064500      PERFORM ACCT-BUILD                                          00120200
064500          THRU   ACCT-BUILD-EXIT.                                 00120300
064500      PERFORM STATE-BUILD                                         00120400
064500          THRU   STATE-BUILD-EXIT.                                00120500
167600      MOVE   '9'               TO   PBI-CARRIER.                  00120600
167600      MOVE   '000000'          TO   PBI-GROUPING.                 00120700
157900      MOVE   UMB-HD-EFF-DT     TO   PBI-B-BATCH-DT-X.             00120800
157900      MOVE   CERT-CNT          TO   PBI-B-CERT-ISS-COUNT          00120900
157900                                    UMB-CERT-ISS.                 00121000
167600      MOVE   UMB-LIFE-WRITTEN  TO   PBI-B-LF-PRM-WRITTEN.         00121100
167600      MOVE   UMB-AH-WRITTEN    TO   PBI-B-AH-PRM-WRITTEN.         00121200
167600      MOVE   CANC-CNT          TO   PBI-B-CERT-CAN-COUNT          00121300
157900                                    UMB-CERT-CANC.                00121400
167600      MOVE   UMB-LIFE-CANC     TO   PBI-B-LF-PRM-CANCELLED.       00121500
167600      MOVE   UMB-AH-CANC       TO   PBI-B-AH-PRM-CANCELLED.       00121600
157900      MOVE   'CSO'             TO   PBI-CLIENT-ID.                00121700
157900      MOVE   '1'               TO   PBI-B-TRANS-TYPE.             00121800
157900      MOVE   '0'               TO   PBI-B-SEQUENCE.               00121900
157900                                                                  00122000
158000*     DISPLAY 'UMB-BATCH-HDR  ' UMB-BATCH-HDR.                    00122100
158000                                                                  00122200
158000      MOVE    UMB-BATCH-HDR    TO  OUT-RECORD.                    00122300
167600      MOVE    UMB-BATCH-NUMB   TO   SR-BATCH-NUMB.                00122400
157900      MOVE    ZEROS            TO   SR-REC-CNT.                   00122500
167600                                                                  00122600
168300      WRITE CARD-RECORD        FROM OUT-RECORD.                   00122700
158100                                                                  00122710
148000*     MOVE  SPACES             TO  CARD-RECORD.                   00122720
148000*     MOVE  SPACES             TO  OUT-RECORD.                    00122730
158100                                                                  00122800
158100      MOVE  'UM'               TO   UMB-BATCH-ID.                 00042600
158100      MOVE  WORK-DA-IN         TO   UMB-BATCH-DA.                 00042600
167600      ADD    1                 TO   UMB-BATCH-NO.                 00122900
167600      MOVE  ZEROS              TO   UMB-LIFE-WRITTEN              00123000
167600                                    PBI-B-CERT-CAN-COUNT          00123010
167600                                    UMB-LIFE-CANC                 00123100
167600                                    UMB-AH-WRITTEN                00123200
167600                                    UMB-AH-CANC                   00123300
167600                                    UMB-CERT-ISS                  00123400
167600                                    UMB-CERT-CANC                 00123500
167600                                    CERT-CNT                      00123600
167600                                    CANC-CNT                      00123700
167600                                    DETAIL-CNT.                   00123800
167600                                                                  00123900
064500 ACCT-BREAK-EXIT.                                                 00124000
064500      EXIT.                                                       00124100
167600                                                                  00124200
064500 ACCT-BUILD.                                                      00124300
064500                                                                  00124301
167600      MOVE   SAVE-FULL-ACCT    TO   PBI-ACCOUNT.                  00125021
057100      MOVE   SAVE-FULL-ACCT    TO   UMB-ACCT-NO.                  00125022
167600                                                                  00125031
064500 ACCT-BUILD-CONT.                                                 00125032
167600                                                                  00125040
167600      MOVE   UMB-ACCT          TO   SAVE-FULL-ACCT.               00125100
064500                                                                  00125120
167600      MOVE   UMB-EFF-DATE      TO   SAVE-EFF-DATE.                00125130
167600                                                                  00125200
064500 ACCT-BUILD-EXIT.                                                 00125300
064500      EXIT.                                                       00125400
064500                                                                  00125500
064500 STATE-BUILD.                                                     00125600
064500                                                                  00125700
167600*                                                                 00126210
167600      MOVE   'KS'              TO   PBI-STATE.                    00126300
064500                                                                  00126400
064500 STATE-BUILD-EXIT.                                                00126500
064500      EXIT.                                                       00126600
064500                                                                  00126700
170400 SKIP3                                                            00127500
170500 END-OF-JOB SECTION.                                              00127600
167800                                                                  00127700
167900*    DISPLAY 'END-OF-JOB       ENTERED '.                         00127800
170800                                                                  00127900
171000     DISPLAY '************************************************'   00128000
171100     DISPLAY 'INPUT RECORDS      -- ' IN-CNT                      00128100
171200     DISPLAY ' '                                                  00128200
171300     DISPLAY 'RECS WRITTEN COUNT -- ' DETAIL-CNT-TOT.             00128300
171400     DISPLAY ' '                                                  00128400
171500     DISPLAY 'ERROR RECORD COUNT -- ' RPT-CNT.                    00128500
171600     DISPLAY ' '                                                  00128600
171700     DISPLAY '************************************************'.  00128700
171800                                                                  00128701
171300     MOVE ZEROS TO  DETAIL-CNT-TOT.                               00128710
171800                                                                  00128800
171900     OPEN   OUTPUT  RPT-FILE.                                     00128900
172000                                                                  00129000
172200     MOVE  RPT-PAGE-CNT     TO RPT-PAGE.                          00129100
172300     MOVE  4                TO RPT-LINE-CNT.                      00129200
172500     MOVE  '     U. M. B.  INPUT TOTALS FOR '  TO UMB-RPT-ID.     00129400
172600     MOVE  RPT-HD-LINE1     TO RPT-REC.                           00129500
172700     WRITE RPT-REC-OUT  AFTER ADVANCING PAGE.                     00129600
172900     MOVE  SPACES           TO RPT-REC.                           00129800
173000     WRITE RPT-REC-OUT  AFTER ADVANCING 1  LINE.                  00129900
173200     MOVE  REPORT-HD-LINE   TO RPT-REC.                           00130100
173300     WRITE RPT-REC-OUT AFTER ADVANCING 1  LINE.                   00130200
173400     ADD  1                 TO  RPT-LINE-CNT.                     00130300
173600     MOVE  SPACES           TO RPT-REC.                           00130500
173700     WRITE RPT-REC-OUT  AFTER ADVANCING 1  LINE.                  00130600
177200                                                                  00130700
177300     MOVE  TOT-LIFE-WRITTEN TO T-LIFE-WRITTEN.                    00130800
177400     MOVE  TOT-LIFE-CANC    TO T-LIFE-CANC.                       00130900
177500     MOVE  TOT-AH-WRITTEN   TO T-AH-WRITTEN.                      00131000
177600     MOVE  TOT-AH-CANC      TO T-AH-CANC.                         00131100
177700     MOVE  TOT-CERT-ISS     TO T-CERT-ISS.                        00131200
177800     MOVE  TOT-CERT-CANC    TO T-CERT-CANC.                       00131300
177900                                                                  00131400
178100     MOVE  TOTAL-LINE       TO RPT-REC.                           00131600
178200     WRITE RPT-REC-OUT AFTER ADVANCING 1 LINE.                    00131700
178300     ADD  1                 TO  RPT-LINE-CNT.                     00131800
178500     MOVE  SPACES           TO RPT-REC.                           00132000
178600     WRITE RPT-REC-OUT  AFTER ADVANCING 1  LINE.                  00132100
178700                                                                  00132200
178800     CLOSE RPT-FILE.                                              00132300
178900                                                                  00132400
179000     GOBACK.                                                      00132500
179100                                                                  00132600
