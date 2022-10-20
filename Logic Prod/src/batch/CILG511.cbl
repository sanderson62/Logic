000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.                CILG511.                              00000200
000300                                                                  00000300
000400*AUTHOR.     CENTRAL STATES OF OMAHA.                             00000400
000500*            OMAHA, NEBR.                                         00000500
000600                                                                  00000600
000700*DATE-COMPILED.                                                   00000700
000900                                                                  00000800
001000*REMARKS.                                                         00000900
001100*     THIS PROGRAM GENERATES ISSUE/CANCEL TRANSACTIONS FROM       00001000
001200*       LAURITZEN GROUP AFFILIATES RECORDS FOR INPUT INTO THE     00001100
001300*         LOGIC SYSTEM.  THE ENTRY IS THRU LOGIC PROGRAM 'EL512'. 00001200
001400*           ('CILG511' IS ONE CSO VERSION OF 'EL511').            00001300
001400*                                                                 00001400
001400*     IT READS THE 800 LRECL FILE FROM LAURITZEN GROUP AND BUILDS 00001410
001400*       2 OUTPUT FILES. A REPORT IS ALSO GENERATED.               00001411
001400*     IT BUILDS LOGIC FORMAT RECORDS FOR INPUT TO LOGIC, AND      00001412
001400*       IT BUILDS 115 LRECL "PP&A" PC RECORDS FOR USE BY CID.     00001413
001400*                                                                 00001420
001400                                                                  00001470
001500 ENVIRONMENT DIVISION.                                            00001500
pemtst*SPECIAL-NAMES.
pemtst*MFEBCDIC is EBCDIC.
001600 INPUT-OUTPUT SECTION.                                            00001600
001700 FILE-CONTROL.                                                    00001700
001800     SELECT  LAURITZEN-IN    ASSIGN TO SYS010-UT-2400-S-SYS010.   00001800
001900     SELECT  LAURITZEN-OUT   ASSIGN TO SYS012-UT-2400-S-SYS012.   00001900
001900     SELECT  PPA-OUT         ASSIGN TO SYS014-UT-2400-S-SYS014.   00002120
002000     SELECT  RPT-FILE        ASSIGN TO SYS013-UR-1403-S-SYS013.   00002000
002100*    SELECT  REPORT-FILE     ASSIGN TO SYS008-UR-1403-S-SYS008.   00002100
002200                                                                  00002200
002300 SKIP3                                                            00002300
002400 DATA DIVISION.                                                   00002400
002500                                                                  00002500
002600 FILE SECTION.                                                    00002600
002700                                                                  00002700
002800******************************************************************00004020
002900**        INPUT TAPE FILE FROM LAURITZEN GROUP AFFILIATES       **00004030
003000******************************************************************00004040
003100                                                                  00004050
003200 FD  LAURITZEN-IN                                                 00004060
003400     RECORDING MODE IS F                                          00004070
003500     LABEL RECORDS ARE STANDARD                                   00004080
003600     RECORD CONTAINS 800 CHARACTERS                               00004090
003700     BLOCK CONTAINS 0 RECORDS                                     00004091
003800     DATA RECORD IS LAURITZEN-RECORD.                             00004092
pemtst*    code-set MFEBCDIC.                                           00004093
004000 01  LAURITZEN-RECORD            PIC X(800).                      00004094
004100                                                                  00004095
004200******************************************************************00004100
004300**       OUTPUT CARD FILE FOR INPUT TO PROGRAM 'EL512'          **00004200
004400******************************************************************00004300
004500                                                                  00004400
004600 FD  LAURITZEN-OUT                                                00004500
004700     RECORDING MODE IS F                                          00004600
004800     LABEL RECORDS ARE STANDARD                                   00004700
004900     RECORD CONTAINS 90 CHARACTERS                                00004800
003700     BLOCK CONTAINS 0 RECORDS                                     00004900
005000     DATA RECORD IS CARD-RECORD.                                  00005000
005100                                                                  00005100
005200 01  CARD-RECORD             PIC X(90).                           00005200
005300                                                                  00005300
004100                                                                  00005310
004200******************************************************************00005320
004300**       OUTPUT DISK FILE FOR INPUT TO CID PC PP&A PROGRAM      **00005330
004400******************************************************************00005340
004500                                                                  00005350
004600 FD  PPA-OUT                                                      00005360
004700     RECORDING MODE IS F                                          00005370
004800     LABEL RECORDS ARE STANDARD                                   00005380
004900     RECORD CONTAINS 115 CHARACTERS                               00005390
003700     BLOCK CONTAINS 0 RECORDS                                     00005391
005000     DATA RECORD IS PPA-RECORD.                                   00005392
005100                                                                  00005393
005200 01  PPA-RECORD              PIC X(115).                          00005394
005300                                                                  00005395
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
007000                                                                  00007000
007100 SKIP3                                                            00007100
007200 WORKING-STORAGE SECTION.                                         00007200
007300 77  FILLER  PIC X(32) VALUE '********************************'.  00007300
007400 77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  00007400
007500 77  FILLER  PIC X(32) VALUE '********************************'.  00007500
007600                                                                  00007600
062100 77  LAST-REC-SW            PIC X         VALUE 'N'.              00007700
007700 77  CANCL-SW               PIC X         VALUE 'N'.              00007800
007700 77  SW-20                  PIC X         VALUE ' '.              00007900
007700 77  ERROR-CODE             PIC X(25)     VALUE SPACES.           00008000
007700 77  SAVE-SEQ               PIC X(30)     VALUE SPACES.           00008100
007700 77  SAVE-PRNT-ERR          PIC X(28)     VALUE SPACES.           00008100
007800 77  SAVE-CERT              PIC X(10)     VALUE SPACES.           00008200
007800 77  SAVE-STATE             PIC X(02)     VALUE SPACES.           00008210
008300 77  RPT-PAGE-CNT           PIC 9999      VALUE ZEROS.            00008600
008400 77  RPT-CANCL-CNT          PIC 99999     VALUE ZEROS.            00008700
008500 77  RPT-ISS-CNT            PIC 99999     VALUE ZEROS.            00008800
008600 77  RPT-LINE-CNT           PIC 99        VALUE ZEROS.            00008900
008800 77  RPT-CNT                PIC 9999999   VALUE ZEROS.            00009000
008900 77  DETAIL-CNT             PIC 9999      VALUE ZEROS.            00009100
008900 77  NINES-IN               PIC S9(12) VALUE 999999999999.        00009101
008900 77  DETAIL-CNT-TOT         PIC 9999999   VALUE ZEROS.            00009110
008900 77  PPA-CNT                PIC 9999999   VALUE ZEROS.
009000 77  IN-CNT                 PIC 9999999   VALUE ZEROS.            00009200
009100 77  TOT-LIFE-WRITTEN       PIC S9(8)V99  VALUE ZEROS.            00009600
009200 77  TOT-LIFE-CANC          PIC S9(8)V99  VALUE ZEROS.            00009700
009300 77  TOT-AH-WRITTEN         PIC S9(8)V99  VALUE ZEROS.            00009800
009400 77  TOT-AH-CANC            PIC S9(8)V99  VALUE ZEROS.            00009900
009500 77  TOT-CERT-ISS           PIC S9(4)     VALUE ZEROS.            00010000
009600 77  TOT-CERT-CANC          PIC S9(4)     VALUE ZEROS.            00010100
009400 77  TOT-BALLOON-AMT        PIC S9(8)V99  VALUE ZEROS.            00009900
010200 77  CANC-CNT               PIC 9999      VALUE ZEROS.            00010300
010200 77  CERT-CNT               PIC 9999      VALUE ZEROS.            00010400
010200 77  W-PROC-MO              PIC 99        VALUE ZEROS.            00010410
010300 77  FIRST-SW               PIC X         VALUE 'Y'.              00010500
010300 77  WRITTEN-PREM-SW        PIC X         VALUE 'N'.              00010510
010300 77  SAVE-WORK-MO           PIC 99        VALUE ZEROS.            00010520
010300 77  DISPLAY-TOT            PIC 9(10)     VALUE ZEROS.            00010520
010300 77  DISPLAY-CNT            PIC 9(4)      VALUE ZEROS.            00010520
010400                                                                  00010600
010400 01  WK-BALLOON.                                                  00010600
010400     05  WK-BALLOON-AMT     PIC ZZ,ZZ9.99-.                       00010600
010400     05  FILLER             PIC X(13)    VALUE ' BALLOON AMT.'.   00010600
010400     05  FILLER             PIC X(5)     VALUE SPACES.            00010600
010400                                                                  00010600
010500 01  BUILD-APR.                                                   00010830
010600     05  W-APR-N                PIC 9(02)V9999.                   00010831
010500     05  W-APR-X  REDEFINES  W-APR-N.                             00010832
010600         10  W-APR-ALL.                                           00010833
010600             15  W-APR-WHOLE    PIC X(02).                        00010834
010600             15  W-APR-DEC.                                       00010835
010600                 20  W-APR-DEC-FIRST2 PIC XX.                     00010836
010600                 20  W-APR-DEC-LAST2  PIC XX.                     00010837
010400                                                                  00010838
010500 01  W-LREFUND.                                                   00010839
010600     05  W-LREFUND-N            PIC 9(07)V99.                     00010840
010500     05  W-LREFUND-X   REDEFINES  W-LREFUND-N.                    00010841
010600         10  W-LREFUND-DOL.                                       00010842
010600             15  W-LREFUND-DOL2 PIC X(02).                        00010843
010600             15  W-LREFUND-DOL5 PIC X(05).                        00010844
010600         10  W-LREFUND-CEN      PIC X(02).                        00010845
010400                                                                  00010846
010500 01  W-AREFUND.                                                   00010847
010600     05  W-AREFUND-N            PIC 9(07)V99.                     00010848
010500     05  W-AREFUND-X   REDEFINES  W-AREFUND-N.                    00010849
010600         10  W-AREFUND-DOL.                                       00010850
010600             15  W-AREFUND-DOL2 PIC X(02).                        00010851
010600             15  W-AREFUND-DOL5 PIC X(05).                        00010852
010600         10  W-AREFUND-CEN      PIC X(02).                        00010853
010400                                                                  00010854
010400                                                                  00010855
010500 01  W-PR-NOTE-AMT.                                               00010856
010600     05  W-PN-AMT-N            PIC 9(06)V99.                      00010857
010500     05  W-PN-AMT-X   REDEFINES  W-PN-AMT-N.                      00010858
010600         10  W-PN-AMT-DOL      PIC X(06).                         00010859
010600         10  W-PN-AMT-CEN      PIC X(02).                         00010860
010400                                                                  00010861
010400                                                                  00010862
010500 01  W-PROC-YR-CEN.                                               00010863
010600     05  W-PROC-CEN         PIC X(02)     VALUE SPACES.           00010864
010600     05  W-PROC-YR          PIC 9(02)     VALUE ZEROS.            00010865
010400                                                                  00010866
010500 01  WK-LOAN-OFFICER.                                             00010863
010600     05  WK-LO-1ST-2        PIC X(02)     VALUE SPACES.           00010864
010600     05  WK-LO-LAST-3       PIC X(03)     VALUE SPACES.           00010865
010400                                                                  00010866
010500 01  OUT-RECORD.                                                  00010867
010600     05  OUT-CERT-NO        PIC X(10)     VALUE SPACES.           00010870
010600     05  OUT-CERT-SFX       PIC X(01)     VALUE SPACES.           00010900
010700     05  OUT-EFF-DATE       PIC X(06)     VALUE SPACES.           00011000
010800     05  OUT-INFO           PIC X(61)     VALUE SPACES.           00011100
010800     05  OUT-TRAN-TYPE      PIC X         VALUE SPACES.           00011200
010800     05  OUT-SEQUENCE       PIC X         VALUE SPACES.           00011300
010600     05  SR-BATCH-NUMB.                                           00011400
010600         10  SR-BATCH-ID    PIC XX        VALUE ZEROS.            00011400
010600         10  SR-BATCH-DA    PIC XX        VALUE ZEROS.            00011400
010600         10  SR-BATCH-NO    PIC 99        VALUE ZEROS.            00011400
010600     05  SR-REC-CNT         PIC 9(4)      VALUE ZEROS.            00011500
010900                                                                  00011600
124600 01  SAVE-FULL-ACCT.                                              00011700
124700     05 SAVE-ACCT-7         PIC X(07)     VALUE SPACES.           00011900
124600     05 SAVE-ACCT           PIC XXX       VALUE SPACES.           00011910
013600                                                                  00012000
019500 01  LAURITZEN-BATCH-HDR.                                         00012100
020500     05  LG-CARR-CO.                                              00012200
020600         10  LG-CARR         PIC X         VALUE '9'.             00012300
020700         10  LG-COMP         PIC X(6)      VALUE '000000'.        00012400
020800     05  LG-STATE            PIC XX        VALUE '  '.            00012500
020900     05  LG-ACCT-NO          PIC X(10)     VALUE SPACES.          00012600
020200     05  LG-BATCH-NUMB.                                           00012700
020200         10  LG-BATCH-ID     PIC XX        VALUE ZEROS.           00012700
020200         10  LG-BATCH-DA     PIC XX        VALUE ZEROS.           00012700
020200         10  LG-BATCH-NO     PIC 99        VALUE ZEROS.           00012700
021000     05  LG-HD-EFF-DT.                                            00012800
021100         10  LG-HD-EFF-MO    PIC XX        VALUE SPACES.          00012900
021200         10  LG-HD-EFF-DA    PIC XX        VALUE SPACES.          00013000
021300         10  LG-HD-EFF-YR    PIC XX        VALUE SPACES.          00013100
020000     05  LG-CERT-ISS         PIC S9(4)     VALUE ZEROS.           00013200
019600     05  LG-LF-WRITTEN       PIC S9(7)V99  VALUE ZEROS.           00013300
019800     05  LG-AH-WRITTEN       PIC S9(7)V99  VALUE ZEROS.           00013400
020100     05  LG-CERT-CANC        PIC S9(4)     VALUE ZEROS.           00013500
019700     05  LG-LF-CANC          PIC S9(7)V99  VALUE ZEROS.           00013600
019900     05  LG-AH-CANC          PIC S9(7)V99  VALUE ZEROS.           00013700
020300     05  LG-CLIENT-ID        PIC X(3)      VALUE 'CSO'.           00013800
021400     05  LG-TRANS-TYPE       PIC X         VALUE '1'.             00013900
020400     05  LG-SEQUENCE-NO      PIC X         VALUE '0'.             00014000
024900                                                                  00014100
025000 01  REPORT-HD-LINE.                                              00014200
025100     05  HD-LIFE-WRITTEN     PIC X(14)  VALUE ' LIFE WRITTEN '.   00014300
025200     05  FILLER              PIC XX     VALUE SPACES.             00014400
025300     05  HD-LIFE-CANC        PIC X(14)  VALUE '    LIFE CANC '.   00014500
025400     05  FILLER              PIC XX     VALUE SPACES.             00014600
025500     05  HD-AH-WRITTEN       PIC X(14)  VALUE '  A&H WRITTEN '.   00014700
025600     05  FILLER              PIC XX     VALUE SPACES.             00014800
025700     05  HD-AH-CANC          PIC X(14)  VALUE '     A&H CANC '.   00014900
025800     05  FILLER              PIC XXX    VALUE SPACES.             00015000
025900     05  HD-CERT-ISS         PIC X(8)   VALUE ' ISS CNT'.         00015100
026000     05  FILLER              PIC XXXX   VALUE SPACES.             00015200
026100     05  HD-CERT-CANC        PIC X(8)   VALUE 'CANC CNT'.         00015300
026200     05  FILLER              PIC X(10)  VALUE SPACES.             00015400
026300     05  FILLER              PIC X(11)  VALUE 'BALLOON AMT'.      00015500
026400     05  FILLER              PIC X(17)  VALUE SPACES.             00015600
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
028100     05  FILLER              PIC X(07)           VALUE SPACES.    00017300
028200     05  T-CERT-CANC         PIC Z,ZZ9.                           00017400
028600     05  FILLER              PIC X(8)            VALUE SPACES.    00017500
027800     05  T-BALLOON-AMT       PIC ZZ,ZZZ,ZZ9.99-.                  00017000
028600     05  FILLER              PIC X(16)            VALUE SPACES.   00017500
028800                                                                  00017600
028900 01  RPT-TOT-LINE.                                                00017700
029000     05  FILLER              PIC X(10)           VALUE SPACES.    00017800
029100     05  FILLER              PIC X(35)           VALUE            00017900
029200         ' NUMBER OF ISSUES WRITTEN IS - '.                       00018000
029300     05  P-ISS-CNT           PIC ZZ,ZZ9.                          00018100
029400     05  FILLER              PIC X(10)           VALUE SPACES.    00018200
029500     05  FILLER              PIC X(35)           VALUE            00018300
029600         'NUMBER OF CANCELS WRITTEN IS - '.                       00018400
029700     05  P-CANCL-CNT         PIC ZZ,ZZ9.                          00018500
029800     05  FILLER              PIC X(10)            VALUE SPACES.   00018600
029900     05  FILLER              PIC X(10)            VALUE SPACES.   00018700
030000                                                                  00018800
030100 01  RPT-HD-LINE1.                                                00018900
030200     05  FILLER              PIC X(26)           VALUE SPACES.    00019000
030300     05  LG-RPT-ID           PIC X(34)           VALUE            00019100
030400         '       LAURITZEN GROUP REPORT FOR '.                    00019200
030500     05  RPT-REPORT-DATE     PIC X(14)           VALUE SPACES.    00019300
030600     05  FILLER              PIC X(16)           VALUE SPACES.    00019400
030600     05  FILLER              PIC X(16)           VALUE            00019500
030400         'PGM = CILG511 '.                                        00019600
030600     05  FILLER              PIC X(10)           VALUE SPACES.    00019700
030700     05  FILLER              PIC X(5)            VALUE  'PAGE '.  00019800
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
032600     05  PRT-REC             PIC X(80)  VALUE SPACES.             00021400
032600     05  FILLER              PIC X(02)  VALUE SPACES.             00021500
033600     05  FULL-ERROR.                                              00021600
033600         10  PT-ERROR        PIC X(22)  VALUE SPACES.             00021610
033600         10  PRNT-ERROR      PIC X(28)  VALUE SPACES.             00021620
032400                                                                  00021810
032500 01  ERR-PT-LINE.                                                 00021820
032600     05  ERR-ID              PIC X(11)  VALUE ' * ERROR * '.      00021830
032600     05  ERR-REC             PIC X(80)  VALUE SPACES.             00021831
032600     05  FILLER              PIC X(02)  VALUE SPACES.             00021840
033600     05  ERR-LINE.                                                00021880
033600         10  ERR-REASON      PIC X(30)  VALUE SPACES.             00021890
033600         10  ERR-CODE        PIC X(09)  VALUE SPACES.             00021891
033800                                                                  00021892
034800 01  WK-DATE.                                                     00021900
034900     05  WK-MO               PIC XX     VALUE SPACES.             00022000
035000     05  FILLER              PIC X      VALUE SPACES.             00022100
035100     05  WK-DA               PIC XX     VALUE SPACES.             00022200
035200     05  FILLER              PIC X      VALUE SPACES.             00022300
035300     05  WK-YR               PIC XX     VALUE SPACES.             00022400
037600                                                                  00022500
038600 01  WORK-DATE.                                                   00022600
039100     12  WORK-DATE-N.                                             00023100
039200         15  WORK-MO-N       PIC 99.                              00023200
039300         15  WORK-DA-N       PIC 99.                              00023300
039400         15  WORK-YR-N       PIC 99.                              00023400
038700     12  WORK-DATE-X    REDEFINES   WORK-DATE-N.                  00022700
038800         15  WORK-MO-X       PIC XX.                              00022800
038900         15  WORK-DA-X       PIC XX.                              00022900
039000         15  WORK-YR-X       PIC XX.                              00023000
039500                                                                  00023500
039100     12  WORK-DATE-IN.                                            00023100
039400         15  WORK-YR-IN      PIC 99.                              00023400
039200         15  WORK-MO-IN      PIC 99.                              00023200
039300         15  WORK-DA-IN      PIC 99.                              00023300
039500                                                                  00023500
040900 01  PRNT-RPT-DATES.                                              00023600
041000     05  RPT-DT-ID.                                               00023700
041100         10  FILLER          PIC X(14)   VALUE 'DECEMBER      '.  00023800
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
042300                                                                  00025000
042400     05  RPT-DT-05     REDEFINES   RPT-DT-ID.                     00025100
042500         10  RPT-DT        OCCURS 12    PIC X(14).                00025200
006400                                                                  00025311
006800                                                                  00025350
042700 01  FILLER   PIC X(24) VALUE '** CARD COPY BOOK  **'.            00025400
042800                                                                  00025500
042900*01  INPUT-COPYBOOK          COPY ERCPNDBI.                       00025600
042800                                                                  00025500
042900     COPY ERCPNDBI.                                               00025600
043000 SKIP3                                                            00025700
043100                                                                  00025800
043200 01  FILLER   PIC X(24) VALUE '**LAURITZEN-RECORD-IN**'.          00025900
043300                                                                  00026000
043400*01  VENDOR-REC    COPY VENDREC8.                                 00026100
043400     COPY VENDREC8.                                               00026100
010500 01  LG-IN800  REDEFINES VENDOR-REC.                              00026200
010600     05  FILLER                        PIC X(400).                00026300
010500     05  LG-LAST400.                                              00026400
010600         10  LG-PR-ADDRESS             PIC X(25).                 00026500
010600         10  LG-PR-CITY                PIC X(20).                 00026600
010600         10  LG-PR-STATE               PIC X(02).                 00026700
010600         10  LG-PR-ZIP                 PIC X(05).                 00026800
010600         10  LG-CO-ADDRESS             PIC X(25).                 00026900
010600         10  LG-CO-CITY                PIC X(20).                 00027000
010600         10  LG-CO-STATE               PIC X(02).                 00027100
010600         10  LG-CO-ZIP                 PIC X(05).                 00027200
010600         10  LG-INIT-AMT-BORROWED      PIC 9(09).                 00027300
010600         10  LG-CLASS-CODE.                                       00027400
010600             15  LG-CLASS-CD-1         PIC X(01).                 00027600
010600             15  LG-CLASS-CD-2-5       PIC X(04).                 00027600
010600         10  LG-LOAN-MATUR-DT.                                    00027500
010600             15  LG-LOAN-MATUR-MO      PIC X(02).                 00027600
010600             15  LG-LOAN-MATUR-DA      PIC X(02).                 00027700
010600             15  LG-LOAN-MATUR-YR      PIC X(02).                 00027800
010600         10  LG-FIRST-NAME             PIC X(10).                 00027900
010600         10  LG-MID-INIT               PIC X(01).                 00028000
010600         10  LG-LAST-NAME              PIC X(15).                 00028100
010600         10  LG-ACCT-LIC-ID-NO         PIC X(10).                 00028200
010600         10  FILLER                    PIC X(240).                00028300
010400                                                                  00028400
020100 01  VENDOR-TOT-REC REDEFINES VENDOR-REC.
020200     05  V-TOT-REC-ID                  PIC X(7).
020300     05  V-TOT-LF-PREM                 PIC S9(9).
020400     05  V-TOT-LF-REFUND               PIC S9(10).
020400     05  V-TOT-AH-PREM                 PIC S9(10).
020400     05  V-TOT-AH-REFUND               PIC S9(10).
020400     05  V-TOT-ISS                     PIC 9(4).
020400     05  V-TOT-CANC                    PIC 9(4).
020400     05  V-TOT-RECORDS                 PIC 9(4).
045600                                                                  00038900
043400*01  PPA-WORK-REC   COPY PPAREC.                                  00038910
045600                                                                  00038900
043400     COPY PPAREC.                                                 00038910
045600                                                                  00038920
054900******************************************************************00039000
055000 SKIP3                                                            00039100
055100 PROCEDURE DIVISION.                                              00039200
055200 SKIP3                                                            00039300
055400                                                                  00039500
055500 INPUT-ROUTINE SECTION.                                           00039600
055600                                                                  00039700
055700     ACCEPT  WORK-DATE-IN  FROM DATE.                             00039800
055800                                                                  00039900
055700     MOVE    WORK-YR-IN    TO  WORK-YR-N.                         00039800
055700     MOVE    WORK-MO-IN    TO  WORK-MO-N.                         00039800
055700     MOVE    WORK-DA-IN    TO  WORK-DA-N.                         00039800
055800                                                                  00039900
055900     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040000
056000     DISPLAY '*   CURRENT DATE IS -- ' WORK-DATE-N.               00040100
056100     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040200
056200                                                                  00040300
056500     OPEN INPUT    LAURITZEN-IN.                                  00040400
056600                                                                  00040500
056800     OPEN OUTPUT   RPT-FILE                                       00040700
056800                    PPA-OUT                                       00040710
056800                      LAURITZEN-OUT.                              00040720
056900                                                                  00040800
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00040900
057100                                                                  00041000
057500     MOVE  ZEROS                  TO   LG-LF-CANC                 00041100
057600                                       LG-AH-CANC                 00041200
057700                                       LG-CERT-CANC.              00041300
119500     MOVE  ZEROS                  TO  PBI-I-AH-PREM-AMT           00041310
119500     MOVE  ZEROS                  TO  PBI-I-AH-BENEFIT-AMT        00041320
094000     MOVE  ZEROS                  TO  PBI-I-AH-BENEFIT-POS1       00041330
094000     MOVE  ZEROS                  TO  PBI-I-AH-BENEFIT-NO         00041340
119500     MOVE  ZEROS                  TO  PBI-I-AH-TERM               00041350
119500     MOVE  ZEROS                  TO  PBI-I-AH-CRIT-PERIOD        00041360
119500     MOVE  ZEROS                  TO  PBI-I-AH-TERM               00041370
119500     MOVE  ZEROS                  TO  PBI-I-AH-CRIT-PERIOD        00041380
093500     MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-YR          00041390
093500     MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-MO          00041391
093500     MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-DA          00041392
119500     MOVE  ZEROS                  TO  PBI-I-AH-PAYMENT            00041393
010600     MOVE  SPACES                 TO  OUT-CERT-NO                 00041394
010600     MOVE  SPACES                 TO  OUT-CERT-SFX                00041395
088500     MOVE  ZEROS                  TO  PBI-C-LF-CANCEL-DATE.       00041396
088500     MOVE  ZEROS                  TO  PBI-C-AH-CANCEL-DATE.       00041397
088500     MOVE  ZEROS                  TO  PBI-C-LF-PREM-REFUND.       00041398
088500     MOVE  ZEROS                  TO  PBI-C-AH-PREM-REFUND.       00041399
057100                                                                  00041400
057200     MOVE  WORK-MO-N              TO   LG-HD-EFF-MO.              00041500
057400     MOVE  WORK-DA-N              TO   LG-HD-EFF-DA.              00041600
057300     MOVE  WORK-YR-N              TO   LG-HD-EFF-YR               00041700
058200                                                                  00042200
058700     MOVE  WORK-MO-IN             TO  SAVE-WORK-MO.               00042700
058700                                                                  00042600
058700     MOVE  WORK-MO-N              TO  W-PROC-MO.                  00042700
058700     SUBTRACT 1 FROM W-PROC-MO.                                   00042700
058700     IF WORK-YR-N  >  50                                          00042700
058700        MOVE '19'                 TO  W-PROC-CEN
058700     ELSE
058700        MOVE '20'                 TO  W-PROC-CEN
058700     END-IF.
058700     MOVE  WORK-YR-N              TO  W-PROC-YR.                  00042700
058800                                                                  00042800
058900 005-ERR-RPT-HD.                                                  00043100
059000                                                                  00043200
059100     ADD  1                       TO RPT-PAGE-CNT.                00043300
059200     MOVE   RPT-PAGE-CNT          TO RPT-PAGE.                    00043400
059300     MOVE    6                    TO RPT-LINE-CNT.                00043500
058700     MOVE  RPT-DT (SAVE-WORK-MO)  TO RPT-REPORT-DATE.             00043610
059500     MOVE  RPT-HD-LINE1           TO RPT-REC.                     00043700
059600     WRITE RPT-REC-OUT            AFTER ADVANCING PAGE.           00043800
059800     MOVE  SPACES                 TO RPT-REC.                     00044000
059900     WRITE RPT-REC-OUT            AFTER ADVANCING 1  LINE.        00044100
060100     MOVE  RPT-HD-LINE2           TO RPT-REC.                     00044300
060200     WRITE RPT-REC-OUT            AFTER ADVANCING 1  LINE.        00044400
060400     MOVE  SPACES                 TO RPT-REC.                     00044600
060500     WRITE RPT-REC-OUT            AFTER ADVANCING 1  LINE.        00044700
060600                                                                  00044800
060700 ERR-RPT-HD-EXIT.                                                 00044900
060800     EXIT.                                                        00045000
060900                                                                  00045100
061000 010-READ-INPUT-FILE.                                             00047100
061100                                                                  00047200
061200     IF  RPT-CNT  GREATER THAN  10                                00047600
061300         GO TO 0200-ERRORS.                                       00047700
061400                                                                  00047800
061500     READ LAURITZEN-IN                                            00047900
061600       INTO  VENDOR-REC                                           00048000
061700         AT END                                                   00048100
061800           MOVE 'Y'  TO  LAST-REC-SW                              00048101
061900             PERFORM  ACCT-BREAK   THRU   ACCT-BREAK-EXIT         00048110
062000               GO TO END-OF-JOB.                                  00048300
062400                                                                  00048351
062400     ADD  1    TO IN-CNT.                                         00048400
062400                                                                  00048351
062400     IF V-TOT-REC-ID = 'TOTAL  '
062400        DISPLAY '** VENDOR TOTAL RECORD **'
062400        MOVE V-TOT-LF-PREM TO DISPLAY-TOT
062400        DISPLAY 'TOTAL LIFE PREM - ' DISPLAY-TOT
062400        MOVE V-TOT-LF-REFUND TO DISPLAY-TOT
062400        DISPLAY 'TOTAL LIFE RFND - ' DISPLAY-TOT
062400        MOVE V-TOT-AH-PREM TO DISPLAY-TOT
062400        DISPLAY 'TOTAL AH PREM   - ' DISPLAY-TOT
062400        MOVE V-TOT-AH-REFUND TO DISPLAY-TOT
062400        DISPLAY 'TOTAL AH RFND   - ' DISPLAY-TOT
062400        MOVE V-TOT-ISS TO DISPLAY-CNT
062400        DISPLAY 'TOTAL ISS REC   - ' DISPLAY-CNT
062400        MOVE V-TOT-CANC TO DISPLAY-CNT
062400        DISPLAY 'TOTAL CANC REC  - ' DISPLAY-CNT
062400        MOVE V-TOT-RECORDS TO DISPLAY-CNT
062400        DISPLAY 'TOTAL RECORDS   - ' DISPLAY-CNT
062400        GO TO 010-READ-INPUT-FILE.
062400                                                                  00048351
062100     MOVE 'N'  TO  LAST-REC-SW.                                   00048500
064000                                                                  00048512
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00048520
057100                                                                  00048530
064300                                                                  00053700
064400 PROCESS-INPUT.                                                   00053800
064500                                                                  00053900
064600*    DISPLAY 'PROCESS-INPUT   ENTERED  '.                         00054000
167600                                                                  00054108
062100     IF  FIRST-SW  =  'N'                                         00054109
062100         GO TO BUILD-PPA.                                         00054110
062100                                                                  00054111
062100     IF  V-WRITTEN-PREM-INDIC  =  'Y'                             00054112
062100         NEXT SENTENCE                                            00054113
062100       ELSE                                                       00054114
062100         MOVE 'N'  TO  FIRST-SW.                                  00054115
062100                                                                  00054116
062100     IF V-STATE-CODE  =  SPACES                                   00054117
062100        MOVE ZEROS TO V-STATE-CODE                                00054118
062100        GO TO MOVE-ACCT-NO.                                       00054119
062100                                                                  00054120
062100     MOVE   V-STATE-CODE        TO  SAVE-STATE.                   00054121
062100     MOVE   V-STATE-CODE        TO  LG-STATE.                     00054130
062100                                                                  00054131
062100 MOVE-ACCT-NO.                                                    00054132
062100                                                                  00054133
062100     MOVE 'Y'                    TO  WRITTEN-PREM-SW.             00054134
062100     MOVE 'LG'                   TO   LG-BATCH-ID.                00054135
062100     MOVE WORK-MO-IN             TO   LG-BATCH-DA.                00054135
062100     ADD    1                    TO   LG-BATCH-NO.                00054136
062100     MOVE  RPT-DT (SAVE-WORK-MO) TO RPT-REPORT-DATE               00054137
062100     MOVE  V-ACCOUNT-ID          TO  SAVE-FULL-ACCT               00054138
062100     MOVE 'N'                    TO  FIRST-SW.                    00054139
062100                                                                  00054140
062100                                                                  00054141
062100 BUILD-PPA.                                                       00054142
062100                                                                  00054143
062100     MOVE  V-COMPANY-ID          TO    P-COMPANY-ID.              00054150
062100     MOVE  V-BANK-ID             TO    P-BANK-ID.                 00054151
062100*    MOVE  V-DISTRICT-ID         TO    P-DISTRICT-ID.             00054152
062100     MOVE  '00000'               TO    P-DISTRICT-ID.             00054152
062100     MOVE  V-FIN-INST-ID         TO    P-FIN-INST-ID.             00054153
062100                                                                  00054143
062300     MOVE  W-PROC-MO             TO    P-PROC-MO.                 00054143
062300     MOVE  '/'                   TO    P-PROC-SLASH.              00054143
062300     MOVE  W-PROC-YR-CEN         TO    P-PROC-FULL-YEAR.          00054143
062300                                                                  00054185
062100     MOVE  V-NOTE-MO             TO    P-NOTE-MO.                 00054186
062100     MOVE  '/'                   TO    P-NOTE-SLASH1.             00054187
062100     MOVE  V-NOTE-DA             TO    P-NOTE-DA.                 00054188
062100     MOVE  '/'                   TO    P-NOTE-SLASH2.             00054189
062100     MOVE  V-NOTE-YR             TO    P-NOTE-YR.                 00054190
062100     MOVE  V-LOAN-NUMBER         TO    P-LAST-NAME.               00054191
062100     MOVE  V-ELIG-FOR-LIFE       TO    P-ELIG-FOR-LIFE.           00054192
062100     MOVE  V-ELIG-FOR-AH         TO    P-ELIG-FOR-AH.             00054193
062100     PERFORM BUILD-DOLLARS       THRU  BUILD-DOLLARS-EXIT.        00054194
122000     MOVE  V-LOAN-APR            TO    BUILD-APR.                 00054195
122000     MOVE  W-APR-WHOLE           TO    P-LOAN-APR-DOL.            00054196
122000     MOVE  W-APR-DEC-FIRST2      TO    P-LOAN-APR-CEN.            00054197
122000     MOVE  '.'                   TO    P-LOAN-APR-DEC.            00054198
122000     MOVE  V-LOAN-OFFICER-ID     TO    P-LOAN-OFFICER-ID.         00054199
122000     MOVE  V-LOAN-TERM           TO    P-LOAN-TERM.               00054200
122000                                                                  00054203
122000     WRITE  PPA-RECORD           FROM  PPA-REC.                   00054204
122000                                                                  00054210
062100 CK-FOR-LOGIC-BUILD.                                              00054228
122000                                                                  00054229
062100     IF  V-WRITTEN-PREM-INDIC  =  'Y'                             00054240
062100         NEXT SENTENCE                                            00054300
062100       ELSE                                                       00054310
062100         GO TO 010-READ-INPUT-FILE.                               00054400
064300                                                                  00054600
064400     IF  V-ACCOUNT-ID  =  SAVE-FULL-ACCT                          00054620
064400         GO TO CK-ISS-CANC.                                       00054621
122000                                                                  00054631
122000     PERFORM  ACCT-BREAK   THRU   ACCT-BREAK-EXIT.                00054632
122000                                                                  00054640
064400 CK-ISS-CANC.                                                     00054700
122000                                                                  00054701
062100     MOVE 'Y'  TO  WRITTEN-PREM-SW.                               00054702
062100     MOVE SPACES TO SAVE-PRNT-ERR.                                00054702
064700                                                                  00054710
064800     IF  V-TRANS-TYPE =  '2'                                      00054720
062100         NEXT SENTENCE                                            00054721
062100       ELSE                                                       00054722
064800         GO  TO  CANCEL-BLD.                                      00054730
064500                                                                  00054800
080500 PROCESS-DETAILS.                                                 00064900
080600                                                                  00065000
080700*    DISPLAY 'PROCESS-DETAILS      ENTERED '.                     00065100
080900                                                                  00065200
152200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00065810
152300* * * * * * * *      START ISSUE PROCESSING     * * * * * * * *   00065820
152400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00065830
152900                                                                  00065840
082800*                ICARD                                            00065900
082900 ISSUE-BLD.                                                       00066000
083000                                                                  00066100
083100*    DISPLAY 'ISSUE-BLD      ENTERED  '.                          00066200
083200                                                                  00066500
083300* * * * * * *    BUILD THE CSO INPUT RECORDS   * * * * * * * *    00066600
083200                                                                  00066700
082900 SEQ1-BLD.                                                        00066800
083000                                                                  00066900
083300* *  BUILD THE SEQUENCE 1 RECORD   * * * * * * * *                00067000
083200                                                                  00067100
083300     MOVE   'ISSUE -  SEQUENCE 1 ' TO  SAVE-SEQ.                  00067200
057100                                                                  00067300
057100     ADD  1  TO  CERT-CNT.                                        00067310
057100     ADD  1  TO  LG-CERT-ISS.                                     00067311
057100     ADD  1  TO  TOT-CERT-ISS.                                    00067312
057100                                                                  00067313
082100     MOVE   SPACES                 TO  PBI-RECORD-BODY.           00067400
088300                                                                  00067500
088500     MOVE  V-ISS-YR                TO  WORK-YR-X.                 00067600
088600     MOVE  V-ISS-MO                TO  WORK-MO-X.                 00067700
088700     MOVE  V-ISS-DA                TO  WORK-DA-X.                 00067800
088500     MOVE  WORK-DATE-X             TO  PBI-CERT-EFF-DT-X.         00068000
089000                                                                  00068100
090600     MOVE  V-ISS-CERT-NUMBER       TO  SAVE-CERT.                 00068200
090600     MOVE  SAVE-CERT               TO  PBI-CERT-PRIME.            00068300
090600     MOVE  SPACES                  TO  PBI-CERT-SFX.              00068400
089000                                                                  00068500
084700     MOVE  V-LAST-NAME             TO  PBI-I-INS-LAST-NAME.       00068600
084700     MOVE  V-FIRST-NAME            TO  PBI-I-INS-1ST-NAME.        00068700
084700     MOVE  V-MID-INIT              TO  PBI-I-INS-MIDDLE-INIT.     00068800
084800                                                                  00068900
083900     IF  V-AGE NOT NUMERIC MOVE 40 TO  V-AGE.                     00069000
083700     MOVE V-AGE                    TO  PBI-I-INSURED-AGE-X.       00069300
083700     MOVE 'M'                      TO  PBI-I-INSURED-SEX.         00069400
083700     MOVE SPACES                   TO  PBI-I-SOC-SEC-NO.          00069500
083700     MOVE SPACES                   TO  PBI-I-MEMBER-NO.           00069600
083700     MOVE SPACES                   TO  PBI-I-BIRTHDAY-X.          00069700
083700     MOVE SPACES                   TO  PBI-I-ENTRY-CD.            00069800
083700     MOVE ' '                      TO  PBI-I-FORCE-CD.            00069900
083700     MOVE '2'                      TO  PBI-TRANS-TYPE.            00070000
083700     MOVE '1'                      TO  PBI-SEQUENCE.              00070100
083200                                                                  00070200
010800     MOVE  PBI-I-ISSUE-REC-SEQ-1   TO  OUT-INFO.                  00070210
010600     MOVE  SAVE-CERT               TO  OUT-CERT-NO.               00070300
010600     MOVE  SPACES                  TO  OUT-CERT-SFX.              00070400
010600     MOVE  WORK-DATE-X             TO  OUT-EFF-DATE.              00070500
010800     MOVE  '2'                     TO  OUT-TRAN-TYPE              00070700
010800     MOVE  '1'                     TO  OUT-SEQUENCE.              00070800
010800                                                                  00071000
121900     PERFORM  0150-WRITE-DATA-CARD THRU 0150-EXIT.                00071100
122000                                                                  00071200
083300* *  END OF  SEQUENCE 1 RECORD BUILD  * * * * * * * *.            00071300
083200                                                                  00071400
082900 SEQ1-EXIT.                                                       00071500
088000     EXIT.                                                        00071600
083200                                                                  00071900
083300* *  BUILD THE SEQUENCE 2 RECORD   * * * * * * * *.               00072000
083200                                                                  00072200
082900 SEQ2-BLD.                                                        00072300
057100                                                                  00072400
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00072500
083200                                                                  00072600
082900     IF  V-JT-AGE       EQUAL '37'                                00072620
082900         GO TO SEQ2-HAS-JT-VALUES.                                00072621
083200                                                                  00072622
084700     IF  V-JT-LAST-NAME  > SPACES OR ZEROS                        00072623
082900         GO TO SEQ2-HAS-JT-VALUES.                                00072624
057100                                                                  00072626
082900*    GO TO SEQ2-HAS-NO-JT-VALUES.                                 00072627
057100                                                                  00072628
082900 SEQ2-HAS-NO-JT-VALUES.                                           00072629
084700                                                                  00072630
084700     MOVE   SPACES               TO  PBI-I-JNT-LAST-NAME          00072631
084700     MOVE   SPACES               TO  PBI-I-JNT-1ST-NAME           00072632
084700     MOVE   SPACES               TO  PBI-I-JNT-MIDDLE-INIT        00072633
084800     MOVE   SPACES               TO  PBI-I-JOINT-AGE-X            00072634
084800     MOVE   SPACES               TO  PBI-I-POLICY-FORM-NO         00072635
088500     MOVE   SPACES               TO  PBI-CERT-EFF-DT-X            00072636
090600     MOVE   SPACES               TO  PBI-CERT-PRIME               00072637
090600     MOVE   SPACES               TO  PBI-CERT-SFX                 00072638
082900     GO TO SEQ3-BLD.                                              00072639
083200                                                                  00072642
082900 SEQ2-HAS-JT-VALUES.                                              00072655
088300                                                                  00072660
083300     MOVE  'ISSUE -  SEQUENCE 2 '  TO  SAVE-SEQ.                  00072700
088300                                                                  00072800
088500     MOVE   V-ISS-YR               TO  WORK-YR-X.                 00072900
088600     MOVE   V-ISS-MO               TO  WORK-MO-X.                 00073000
088700     MOVE   V-ISS-DA               TO  WORK-DA-X.                 00073100
088500     MOVE   WORK-DATE-X            TO  PBI-CERT-EFF-DT-X.         00073300
089000                                                                  00073400
090600     MOVE   SAVE-CERT              TO  PBI-CERT-PRIME.            00073500
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00073600
084000                                                                  00073700
084700     MOVE   V-JT-LAST-NAME         TO  PBI-I-JNT-LAST-NAME.       00073800
084700     MOVE   V-JT-FIRST-NAME        TO  PBI-I-JNT-1ST-NAME.        00073900
084700     MOVE   V-JT-MID-INIT          TO  PBI-I-JNT-MIDDLE-INIT.     00074000
084000                                                                  00074010
084800     IF     V-JT-AGE               = '00'                         00074100
084800            MOVE '  '              TO  V-JT-AGE.                  00074101
084800     MOVE   V-JT-AGE               TO  PBI-I-JOINT-AGE-X.         00074102
084000                                                                  00074110
084800     MOVE   V-CERT-FORM-NUMBER     TO  PBI-I-POLICY-FORM-NO.      00074200
084000                                                                  00074300
083700     MOVE   '2'                    TO  PBI-TRANS-TYPE.            00074400
083700     MOVE   '2'                    TO  PBI-SEQUENCE.              00074500
083200                                                                  00074600
010800     MOVE   PBI-I-ISSUE-REC-SEQ-2  TO  OUT-INFO.                  00074610
010600     MOVE   SAVE-CERT              TO  OUT-CERT-NO.               00074700
010600     MOVE   SPACES                 TO  OUT-CERT-SFX.              00074800
088500     MOVE   WORK-DATE-X            TO  OUT-EFF-DATE.              00074900
010800     MOVE   '2'                    TO  OUT-TRAN-TYPE.             00075100
010800     MOVE   '2'                    TO  OUT-SEQUENCE.              00075200
088300                                                                  00075300
121900     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00075400
122000                                                                  00075500
083300* *  END OF  SEQUENCE 2 RECORD BUILD  * * * * * * * *.            00075600
088300                                                                  00075700
083300* *  BUILD THE SEQUENCE 3 RECORD   * * * * * * * *.               00076400
088300                                                                  00076600
082900 SEQ3-BLD.                                                        00076700
088300                                                                  00076800
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00076900
093000                                                                  00077020
092400*    DISPLAY '* * SEQ3 - FILL THE LIFE FIELDS     * * '.          00077100
083200                                                                  00077200
083300     MOVE  'ISSUE -  SEQUENCE 3 '  TO  SAVE-SEQ.                  00077300
092500                                                                  00077400
088500     MOVE   V-ISS-YR               TO  WORK-YR-X.                 00077500
088600     MOVE   V-ISS-MO               TO  WORK-MO-X.                 00077600
088700     MOVE   V-ISS-DA               TO  WORK-DA-X.                 00077700
088500     MOVE   WORK-DATE-X            TO  PBI-CERT-EFF-DT-X.         00077900
089000                                                                  00078000
090600     MOVE   SAVE-CERT              TO  PBI-CERT-PRIME.            00078100
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00078200
093900                                                                  00079210
094000     IF     V-LF-BEN-CODE  EQUAL '00'                             00079600
094000            MOVE '  '              TO  V-LF-BEN-CODE.             00079610
094000                                                                  00079814
094000     MOVE   V-LF-BEN-CODE          TO PBI-I-LF-BENEFIT-TYPE.      00079820
094000                                                                  00079900
092000     MOVE  V-LF-TERM               TO  PBI-I-LF-TERM-X.           00082300
pemtst*    move +55.55 to v-lf-ben-amt                                  00082310
pemtst*    move -77.77 to v-ah-ben-amt
092800     IF     V-LF-BEN-AMT NOT NUMERIC                              00082325
092800                       MOVE ZEROS  TO V-LF-BEN-AMT.               00082330
092800     IF     PBI-I-LF-BENEFIT-AMT   NOT NUMERIC                    00082340
092800                       MOVE ZEROS  TO PBI-I-LF-BENEFIT-AMT.       00082350
092800     ADD    V-LF-BEN-AMT           TO  PBI-I-LF-BENEFIT-AMT.      00082360
057100                                                                  00082700
093500     IF    V-LIFE-PREM-WRITTEN NOT NUMERIC                        00082800
093500     DISPLAY ' V-LIFE-PREM-WRITTEN NOT NUMERIC  *  = '            00082801
093500                                        V-LIFE-PREM-WRITTEN       00082802
093500     DISPLAY ' V-ISS-CERT-NUMBER      = '  V-ISS-CERT-NUMBER      00082803
093500                      MOVE ZEROS   TO V-LIFE-PREM-WRITTEN.        00082810
092800     IF     PBI-I-LF-PREM-AMT      NOT NUMERIC                    00082820
092800                       MOVE ZEROS  TO PBI-I-LF-PREM-AMT.          00082830
092800                                                                  00082841
093500     ADD   V-LIFE-PREM-WRITTEN     TO  PBI-I-LF-PREM-AMT          00082866
093500                                       TOT-LIFE-WRITTEN           00082867
093500                                       LG-LF-WRITTEN.             00082868
093500                                                                  00082869
093600     MOVE  ZEROS                   TO  PBI-I-LF-CRIT-PERIOD.      00083100
093600                                                                  00083200
093700     MOVE  ZEROS                   TO  PBI-I-LF-EXPIRE-YR.        00083300
093700     MOVE  ZEROS                   TO  PBI-I-LF-EXPIRE-MO.        00083400
093700     MOVE  ZEROS                   TO  PBI-I-LF-EXPIRE-DA.        00083500
093700                                                                  00083600
093800     MOVE V-LF-ALT-BEN-AMT         TO  PBI-I-LF-ALT-BENEFIT-AMT.  00083722
093800     MOVE V-LF-ALT-PREM-AMT        TO  PBI-I-LF-ALT-PREM-AMT.     00083760
093800     IF V-LF-ALT-PREM-AMT > ZERO                                  00083810
093800         MOVE V-LF-ALT-PREM-AMT TO WK-BALLOON-AMT                 00083810
093800         MOVE WK-BALLOON TO SAVE-PRNT-ERR                         00083810
093800         ADD V-LF-ALT-PREM-AMT TO TOT-BALLOON-AMT                 00083810
093800     END-IF.                                                      00083810
093800                                                                  00083810
093900     MOVE SPACES                   TO  PBI-I-TERM-AS-DAYS-X.      00083900
093900                                                                  00084000
094000     MOVE '2'                      TO  PBI-TRANS-TYPE.            00084100
094000     MOVE '3'                      TO  PBI-SEQUENCE.              00084200
094000                                                                  00084300
094000                                                                  00084600
010800     MOVE  PBI-I-ISSUE-REC-SEQ-3   TO  OUT-INFO.                  00084610
010600     MOVE  SAVE-CERT               TO  OUT-CERT-NO.               00084700
010600     MOVE  SPACES                  TO  OUT-CERT-SFX.              00084800
088500     MOVE  WORK-DATE-X             TO  OUT-EFF-DATE.              00084900
010800     MOVE  '2'                     TO  OUT-TRAN-TYPE              00085100
010800     MOVE  '3'                     TO  OUT-SEQUENCE.              00085200
088300                                                                  00085300
121900     PERFORM  0150-WRITE-DATA-CARD THRU 0150-EXIT.                00085400
122000                                                                  00085500
093500     MOVE ZEROS                    TO  PBI-I-LF-PREM-AMT          00085510
092800                                       PBI-I-LF-BENEFIT-AMT.      00085512
093500     MOVE SPACES                   TO  SAVE-PRNT-ERR.             00085510
122000                                                                  00085520
083300* *  END OF  SEQUENCE 3 RECORD BUILD  * * * * * * * *             00085600
088300                                                                  00085700
083300* *  BUILD THE SEQUENCE 4 RECORD    * * * * * * * * *             00086300
083200                                                                  00086400
082900 SEQ4-BLD.                                                        00086500
088300                                                                  00086600
106900*   PROCESS-A-H.                                                  00086700
088300                                                                  00086800
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00086900
089000                                                                  00087020
107100*    DISPLAY '* *     FILL THE I-CARD AH FIELDS     * * '.        00087100
083200                                                                  00087300
083300     MOVE  'ISSUE -  SEQUENCE 4 '  TO  SAVE-SEQ.                  00087400
092500                                                                  00087500
088500     MOVE   V-ISS-YR               TO  WORK-YR-X.                 00087700
088600     MOVE   V-ISS-MO               TO  WORK-MO-X.                 00087800
088700     MOVE   V-ISS-DA               TO  WORK-DA-X.                 00087900
088500     MOVE   WORK-DATE-X            TO  PBI-CERT-EFF-DT-X.         00088100
089000                                                                  00088200
090600     MOVE   SAVE-CERT              TO  PBI-CERT-PRIME.            00088300
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00088400
093900                                                                  00088800
119500     IF   V-AH-PREM-WRITTEN        GREATER THAN ZEROS             00088900
119500           NEXT  SENTENCE                                         00089000
119500       ELSE                                                       00089100
119500           MOVE  ZEROS             TO  PBI-I-AH-PREM-AMT          00089200
119500           MOVE  ZEROS             TO  PBI-I-AH-BENEFIT-AMT       00089300
094000           MOVE  ZEROS             TO  PBI-I-AH-BENEFIT-POS1      00089400
094000           MOVE  ZEROS             TO  PBI-I-AH-BENEFIT-NO        00089500
119500           MOVE  ZEROS             TO  PBI-I-AH-TERM              00089600
119500           MOVE  ZEROS             TO  PBI-I-AH-CRIT-PERIOD       00089700
119500           MOVE  ZEROS             TO  PBI-I-AH-TERM              00089800
119500           MOVE  ZEROS             TO  PBI-I-AH-CRIT-PERIOD       00089900
093500           MOVE  ZEROS             TO  PBI-I-AH-EXPIRE-YR         00090000
093500           MOVE  ZEROS             TO  PBI-I-AH-EXPIRE-MO         00090100
093500           MOVE  ZEROS             TO  PBI-I-AH-EXPIRE-DA         00090200
119500           MOVE  ZEROS             TO  PBI-I-AH-PAYMENT           00090300
010600           MOVE  SAVE-CERT         TO  OUT-CERT-NO                00090400
010600           MOVE  SPACES            TO  OUT-CERT-SFX               00090500
088500           MOVE  WORK-DATE-N       TO  OUT-EFF-DATE               00090600
010800           MOVE  PBI-I-ISSUE-REC-SEQ-4  TO  OUT-INFO              00090700
010800           MOVE  '2'               TO  OUT-TRAN-TYPE              00090800
010800           MOVE  '4'               TO  OUT-SEQUENCE               00090900
082900           GO  TO  SEQ5-BLD.                                      00091010
093900                                                                  00091100
119500     IF   V-AH-PREM-WRITTEN        NOT NUMERIC                    00091300
119500                        MOVE ZEROS TO V-AH-PREM-WRITTEN.          00091301
119500     IF PBI-I-AH-PREM-AMT NOT NUMERIC                             00091302
119500          MOVE  ZEROS TO  PBI-I-AH-PREM-AMT.                      00091303
119500     ADD  V-AH-PREM-WRITTEN   TO  PBI-I-AH-PREM-AMT               00091306
157500                                  LG-AH-WRITTEN                   00091320
176800                                  TOT-AH-WRITTEN.                 00091330
119800                                                                  00091340
119500     IF   V-AH-BEN-AMT NOT NUMERIC                                00091600
119500          MOVE ZEROS TO  V-AH-BEN-AMT.                            00091601
119500     IF   PBI-I-AH-BENEFIT-AMT NOT NUMERIC                        00091602
119500          MOVE  ZEROS  TO  PBI-I-AH-BENEFIT-AMT.                  00091603
119500     ADD  V-AH-BEN-AMT TO  PBI-I-AH-BENEFIT-AMT.                  00091604
093900                                                                  00091610
094000     IF   V-AH-BEN-CODE EQUAL '000' OR ' 00'                      00092140
094000       MOVE  '   '   TO  V-AH-BEN-CODE.                           00092141
093900                                                                  00092144
094000     MOVE V-AH-BEN-CODE            TO  PBI-I-AH-BENEFIT-TYPE.     00092169
094000     MOVE  ' '                     TO  PBI-I-AH-BENEFIT-POS1.     00092170
093900                                                                  00092820
119500     MOVE V-AH-TERM                TO  PBI-I-AH-TERM-X.           00092900
118500                                                                  00093000
119500     MOVE V-AH-CRIT-PERIOD         TO  PBI-I-AH-CRIT-PERIOD-X.    00093100
119800                                                                  00093200
093500     MOVE ZEROS                    TO  PBI-I-AH-EXPIRE-YR.        00093300
093500     MOVE ZEROS                    TO  PBI-I-AH-EXPIRE-MO.        00093400
093500     MOVE ZEROS                    TO  PBI-I-AH-EXPIRE-DA.        00093500
093900                                                                  00093600
119500     MOVE ZEROS                    TO  PBI-I-AH-PAYMENT.          00093700
093900                                                                  00093800
083700     MOVE '2'                      TO  PBI-TRANS-TYPE.            00093900
083700     MOVE '4'                      TO  PBI-SEQUENCE.              00094000
083200                                                                  00094400
010800     MOVE  PBI-I-ISSUE-REC-SEQ-4   TO  OUT-INFO.                  00094410
010600     MOVE  SAVE-CERT               TO  OUT-CERT-NO.               00094500
010600     MOVE  SPACES                  TO  OUT-CERT-SFX.              00094600
088500     MOVE  WORK-DATE-X             TO  OUT-EFF-DATE.              00094700
010800     MOVE  '2'                     TO  OUT-TRAN-TYPE              00094900
010800     MOVE  '4'                     TO  OUT-SEQUENCE.              00095000
088300                                                                  00095100
088300                                                                  00095200
121900     PERFORM  0150-WRITE-DATA-CARD THRU 0150-EXIT.                00095300
122000                                                                  00095400
122000                                                                  00095500
083300* *  END OF  SEQUENCE 4 RECORD BUILD  * * * * * * * *             00095900
083200                                                                  00095910
083300* *  BUILD THE SEQUENCE 5 RECORD   * * * * * * * *.               00095920
083200                                                                  00095930
082900 SEQ5-BLD.                                                        00095940
057100                                                                  00095950
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00095960
083200                                                                  00095970
083300     MOVE  'ISSUE -  SEQUENCE 5 '  TO  SAVE-SEQ.                  00095980
088300                                                                  00095990
088500     MOVE   V-ISS-YR               TO  WORK-YR-X.                 00095991
088600     MOVE   V-ISS-MO               TO  WORK-MO-X.                 00095992
088700     MOVE   V-ISS-DA               TO  WORK-DA-X.                 00095993
088500     MOVE   WORK-DATE-X            TO  PBI-CERT-EFF-DT-X.         00095994
089000                                                                  00095995
090600     MOVE   SAVE-CERT              TO  PBI-CERT-PRIME.            00095996
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00095997
084000                                                                  00095998
084700     MOVE   V-LOAN-OFFICER-ID      TO  WK-LOAN-OFFICER.           00095999
084700     MOVE   WK-LO-LAST-3           TO  PBI-I-LOAN-OFFICER.        00095999
084700     MOVE   V-LOAN-TERM            TO  PBI-I-LOAN-TERM-X.         00096000
084700     MOVE   V-PAYMENT-DATE         TO  PBI-I-1ST-PMT-DT.          00096010
084800     MOVE   SPACES                 TO  PBI-I-EXT-DAYS-X           00096020
084800                                       PBI-I-INDV-GRP-CD          00096030
084800                                       PBI-I-REIN-CODE            00096031
084800                                       PBI-I-SIG-SW               00096032
084800                                       PBI-I-LIVES-X              00096033
084800                                       PBI-I-PAY-FREQUENCY-X      00096034
084800                                       PBI-I-RATE-CLASS           00096035
084800                                       PBI-I-SKIP-CODE            00096036
084800                                       PBI-I-BENEFICIARY          00096037
084800                                       PBI-I-TERM-TYPE.           00096038
084000                                                                  00096039
084700     IF     V-LOAN-APR   NOT NUMERIC                              00096040
084700            MOVE  ZEROS            TO  V-LOAN-APR.                00096041
084000                                                                  00096042
084700     IF     PBI-I-LOAN-APR   NOT NUMERIC                          00096043
084700            MOVE  ZEROS            TO  PBI-I-LOAN-APR.            00096044
084000                                                                  00096045
084700     MOVE   V-LOAN-APR             TO  PBI-I-LOAN-APR.            00096046
084000                                                                  00096047
083700     MOVE   '2'                    TO  PBI-TRANS-TYPE.            00096050
083700     MOVE   '5'                    TO  PBI-SEQUENCE.              00096060
083200                                                                  00096070
010800     MOVE   PBI-I-ISSUE-REC-SEQ-5  TO  OUT-INFO.                  00096080
010600     MOVE   SAVE-CERT              TO  OUT-CERT-NO.               00096090
010600     MOVE   SPACES                 TO  OUT-CERT-SFX.              00096091
088500     MOVE   WORK-DATE-X            TO  OUT-EFF-DATE.              00096092
010800     MOVE   '2'                    TO  OUT-TRAN-TYPE.             00096093
010800     MOVE   '5'                    TO  OUT-SEQUENCE.              00096094
088300                                                                  00096095
121900     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00096110
122000                                                                  00096120
122000                                                                  00096200
122300 I-CARD-END.                                                      00096400
122400                                                                  00096800
122100     GO  TO  010-READ-INPUT-FILE.                                 00096900
092500                                                                  00097000
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
123200*    DISPLAY 'CANCEL-BLD  ENTERED '.                              00101100
092500                                                                  00101200
088500     ADD  1  TO  CANC-CNT.                                        00101300
092500                                                                  00101310
088500     MOVE   V-ISS-YR                 TO  WORK-YR-X.               00101320
088600     MOVE   V-ISS-MO                 TO  WORK-MO-X.               00101400
088700     MOVE   V-ISS-DA                 TO  WORK-DA-X.               00101500
088700     MOVE   WORK-DATE-X              TO  PBI-CERT-EFF-DT-X.       00101600
090600     MOVE   V-ISS-CERT-NUMBER        TO  SAVE-CERT.               00101610
089000                                                                  00101700
090600     MOVE   SAVE-CERT                TO  PBI-CERT-PRIME.          00101800
090600     MOVE   SPACES                   TO  PBI-CERT-SFX.            00101900
088300                                                                  00102000
090600     MOVE   SPACES                   TO  PBI-C-LIVES-X.           00102100
090600     MOVE   SPACES                   TO  PBI-C-FORCE-CD.          00102200
084000                                                                  00102300
125400     MOVE   V-LAST-NAME              TO   PBI-C-INSURED-NAME.     00102400
125500                                                                  00102500
134300* *     FILL THE CANCEL LIFE FIELDS     * * * * * * * *           00102700
088300                                                                  00102800
088500     MOVE   V-LF-CANC-YR             TO  WORK-YR-X.               00102900
088600     MOVE   V-LF-CANC-MO             TO  WORK-MO-X.               00103000
088700     MOVE   V-LF-CANC-DA             TO  WORK-DA-X.               00103100
134500     IF     WORK-DATE-N              NOT NUMERIC                  00103110
134500            MOVE  ZEROS              TO  WORK-DATE-N.             00103120
088700     MOVE   WORK-DATE-N              TO  PBI-C-LF-CANCEL-DATE-X.  00103200
134900                                                                  00103300
134500     IF    V-LF-CANC-REFUND          NOT  NUMERIC                 00103800
134500           MOVE  ZEROS               TO  V-LF-CANC-REFUND.        00103801
134500     IF    PBI-C-LF-PREM-REFUND      NOT  NUMERIC                 00103802
134500           MOVE  ZEROS               TO  PBI-C-LF-PREM-REFUND.    00103803
134500     ADD   V-LF-CANC-REFUND          TO PBI-C-LF-PREM-REFUND.     00103900
128600                                                                  00104100
134300* *     FILL THE CANCEL A-H  FIELDS     * * * * * * * *           00104200
088300                                                                  00104300
088500     MOVE   V-AH-CANC-YR             TO  WORK-YR-X.               00104400
088600     MOVE   V-AH-CANC-MO             TO  WORK-MO-X.               00104500
088700     MOVE   V-AH-CANC-DA             TO  WORK-DA-X.               00104600
088700     MOVE   WORK-DATE-N              TO  PBI-C-AH-CANCEL-DATE-X.  00104700
134900                                                                  00104900
134500     IF    V-AH-CANC-REFUND          NOT NUMERIC                  00105010
134500           MOVE  ZEROS               TO  V-AH-CANC-REFUND.        00105100
134500     IF    PBI-C-AH-PREM-REFUND      NOT  NUMERIC                 00105200
134500           MOVE  ZEROS               TO  PBI-C-AH-PREM-REFUND.    00105300
134500     ADD  V-AH-CANC-REFUND           TO PBI-C-AH-PREM-REFUND.     00105400
093900                                                                  00105600
083700     MOVE '3'                        TO  PBI-C-TRANS-TYPE.        00105700
083700     MOVE '1'                        TO  PBI-C-SEQUENCE.          00105800
137000                                                                  00105900
140200 WRITE-CANCL-REC.                                                 00106000
137000                                                                  00106100
140200*    DISPLAY 'WRITE-CANCL-REC   ENTERED    '.                     00106200
148000*    DISPLAY  OUT-RECORD.                                         00106300
088300                                                                  00106400
088500     MOVE   V-ISS-YR                 TO  WORK-YR-X.               00106500
088600     MOVE   V-ISS-MO                 TO  WORK-MO-X.               00106600
088700     MOVE   V-ISS-DA                 TO  WORK-DA-X.               00106700
088700     MOVE   WORK-DATE-X              TO  PBI-CERT-EFF-DT-X.       00106800
089000                                                                  00107000
090600     MOVE   SAVE-CERT                TO  PBI-CERT-PRIME.          00107100
090600     MOVE   SPACES                   TO  PBI-CERT-SFX.            00107200
084000                                                                  00107300
010800     MOVE  PBI-C-CANCEL-REC-SEQ-1    TO  OUT-INFO.                00107310
010600     MOVE  SAVE-CERT                 TO  OUT-CERT-NO.             00107400
010600     MOVE  SPACES                    TO  OUT-CERT-SFX.            00107500
010700     MOVE  WORK-DATE-X               TO  OUT-EFF-DATE.            00107600
010800     MOVE  '3'                       TO  OUT-TRAN-TYPE.           00107800
010800     MOVE  '1'                       TO  OUT-SEQUENCE.            00107900
088300                                                                  00108000
144900     ADD   1                         TO  DETAIL-CNT               00108100
144900                                         DETAIL-CNT-TOT.          00108110
144900     MOVE  DETAIL-CNT                TO  SR-REC-CNT.              00108200
144900     MOVE  LG-BATCH-NUMB             TO  SR-BATCH-NUMB.           00108300
088300                                                                  00108400
148000     WRITE CARD-RECORD               FROM OUT-RECORD.             00108500
140300                                                                  00108600
143500     ADD  1                          TO  LG-CERT-CANC.            00108700
143600     ADD  PBI-C-LF-PREM-REFUND       TO  LG-LF-CANC.              00108800
143700     ADD  PBI-C-AH-PREM-REFUND       TO  LG-AH-CANC.              00108900
176500                                                                  00109000
143500     ADD  1                          TO TOT-CERT-CANC.            00109100
143600     ADD  PBI-C-LF-PREM-REFUND       TO TOT-LIFE-CANC.            00109200
143700     ADD  PBI-C-AH-PREM-REFUND       TO TOT-AH-CANC.              00109300
146200                                                                  00109500
146300     ADD   1                         TO RPT-CANCL-CNT.            00109600
146400     ADD   1                         TO RPT-LINE-CNT.             00109700
146500     IF   RPT-LINE-CNT               GREATER THAN  60             00109800
146600          PERFORM   005-ERR-RPT-HD   THRU ERR-RPT-HD-EXIT.        00109900
146800     MOVE  OUT-RECORD                TO  RPT-PT-LINE.             00110100
147100     MOVE  'CANCEL '                 TO  PT-ERROR.                00110200
147200                                                                  00110300
147900     MOVE  RPT-PT-LINE               TO  RPT-REC.                 00110400
148000     WRITE RPT-REC-OUT               AFTER ADVANCING 1  LINE.     00110500
148100     ADD   1                         TO  RPT-LINE-CNT.            00110600
148300     MOVE  SPACES                    TO RPT-REC.                  00110800
148400     WRITE RPT-REC-OUT               AFTER ADVANCING 1  LINE.     00110900
148500     MOVE  SPACES                    TO RPT-PT-LINE.              00111000
148600                                                                  00111100
143600     MOVE ZEROS                      TO PBI-C-LF-PREM-REFUND      00111110
143700                                        PBI-C-AH-PREM-REFUND      00111120
143600                                        PBI-C-LF-PREM-REFUND      00111150
143700                                        PBI-C-AH-PREM-REFUND.     00111160
137200                                                                  00111170
137200     GO  TO  010-READ-INPUT-FILE.                                 00111200
137200                                                                  00111300
137300 A-CARD-END.                                                      00111400
137400     EXIT.                                                        00111500
151300                                                                  00111600
152200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00111700
152300* * * * * * * *   END  CANCELLATION PROCESSING  * * * * * * * *   00111800
152400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00111900
152100                                                                  00112000
153000******* * * * * * * * * * * * * * * * * * * * * * * * * * * *     00112100
152100                                                                  00112200
153200 0150-WRITE-DATA-CARD.                                            00112800
153300                                                                  00112900
153400*    DISPLAY '0150-WRITE-DATA-CARD ENTERED  '.                    00113000
152600                                                                  00113100
157800 0150-WRITE.                                                      00113600
157900                                                                  00113700
158000*    DISPLAY 'ISSUE WRITE   ENTERED '.                            00113800
088300                                                                  00113900
168300*    DISPLAY  OUT-RECORD.                                         00114700
158100                                                                  00114800
144900     ADD  1                    TO  DETAIL-CNT                     00114900
144900                                   DETAIL-CNT-TOT.                00114910
144900     MOVE  DETAIL-CNT          TO  SR-REC-CNT.                    00115000
144900     MOVE  LG-BATCH-NUMB       TO  SR-BATCH-NUMB.                 00115100
168300     WRITE CARD-RECORD         FROM OUT-RECORD.                   00115200
158100                                                                  00115300
159400 0150-RPT-PRINT.                                                  00115600
159500                                                                  00115700
159600*    DISPLAY '0150-RPT-PRINT  ENTERED  '.                         00115800
159700                                                                  00115900
159800     ADD   1                   TO RPT-ISS-CNT.                    00116000
159900     ADD   1                   TO RPT-LINE-CNT.                   00116100
160000     IF   RPT-LINE-CNT GREATER THAN  60                           00116200
160100          PERFORM   005-ERR-RPT-HD THRU ERR-RPT-HD-EXIT.          00116300
160300     MOVE  OUT-RECORD          TO  RPT-PT-LINE.                   00116500
160300     MOVE  SAVE-SEQ            TO  PT-ERROR.                      00116600
160300     MOVE  SAVE-PRNT-ERR       TO  PRNT-ERROR.                    00116600
161400     MOVE  RPT-PT-LINE         TO  RPT-REC.                       00116700
161500     WRITE RPT-REC-OUT AFTER ADVANCING 1  LINE.                   00116800
161600     ADD  1                    TO  RPT-LINE-CNT.                  00116900
161800     MOVE  SPACES              TO RPT-REC.                        00117100
161900     WRITE RPT-REC-OUT  AFTER ADVANCING 1  LINE.                  00117200
162000     MOVE  SPACES              TO RPT-PT-LINE.                    00117300
162100                                                                  00117400
165600 0150-EXIT.                                                       00117500
165700     EXIT.                                                        00117600
165800                                                                  00117800
165900 0200-ERRORS.                                                     00117900
166000                                                                  00118000
166100*    DISPLAY '0200-ERRORS  ENTERED '.                             00118100
166200                                                                  00118200
166300     DISPLAY '******************************************'.        00118300
166400     DISPLAY 'INPUT FILE HAS MORE THAN 10 ERRORS '.               00118400
166500     DISPLAY ' '.                                                 00118500
166600     DISPLAY '******************************************'.        00118600
166800                                                                  00118610
166700     CLOSE   LAURITZEN-IN                                         00118700
167000             LAURITZEN-OUT.                                       00118910
167100                                                                  00119000
167200     GO TO END-OF-JOB.                                            00119100
167300                                                                  00119200
167400 0200-EXIT.                                                       00119300
167500      EXIT.                                                       00119400
122000                                                                  00119410
062100 BUILD-DOLLARS.                                                   00119514
122000                                                                  00119515
122000     MOVE  V-LF-CANC-REFUND    TO  W-LREFUND-N.                   00119516
122000     MOVE  W-LREFUND-DOL5      TO  P-LF-CANC-REFUND-DOL.          00119517
122000     MOVE  W-LREFUND-CEN       TO  P-LF-CANC-REFUND-CEN.          00119518
122000     MOVE  '.'                 TO  P-LF-CANC-REFUND-DEC.          00119519
122000                                                                  00119520
122000     MOVE  V-AH-CANC-REFUND    TO  W-AREFUND-N.                   00119521
122000     MOVE  W-AREFUND-DOL5      TO  P-AH-CANC-REFUND-DOL.          00119522
122000     MOVE  W-AREFUND-CEN       TO  P-AH-CANC-REFUND-CEN.          00119523
122000     MOVE  '.'                 TO  P-AH-CANC-REFUND-DEC.          00119524
122000                                                                  00119525
122000     MOVE  V-PRINC-AMT         TO  W-PN-AMT-N.                    00119526
122000     MOVE  W-PN-AMT-DOL        TO  P-PRINC-AMT-DOL.               00119527
122000     MOVE  W-PN-AMT-CEN        TO  P-PRINC-AMT-CEN.               00119528
122000     MOVE  '.'                 TO  P-PRINC-AMT-DEC.               00119529
122000                                                                  00119530
122000     MOVE  V-TOT-NOTE-AMT      TO  W-PN-AMT-N.                    00119531
122000     MOVE  W-PN-AMT-DOL        TO  P-TOT-NOTE-AMT-DOL.            00119532
122000     MOVE  W-PN-AMT-CEN        TO  P-TOT-NOTE-AMT-CEN.            00119533
122000     MOVE  '.'                 TO  P-TOT-NOTE-AMT-DEC.            00119534
122000                                                                  00119535
122000     MOVE  V-LIFE-PREM-WRITTEN TO  W-LREFUND-N.                   00119536
122000     MOVE  W-LREFUND-DOL5      TO  P-LIFE-PREM-WRITTEN-DOL.       00119537
122000     MOVE  W-LREFUND-CEN       TO  P-LIFE-PREM-WRITTEN-CEN.       00119538
122000     MOVE  '.'                 TO  P-LIFE-PREM-WRITTEN-DEC.       00119539
122000                                                                  00119540
122000     MOVE  V-AH-PREM-WRITTEN   TO  W-AREFUND-N.                   00119541
122000     MOVE  W-AREFUND-DOL5      TO  P-AH-PREM-WRITTEN-DOL.         00119542
122000     MOVE  W-AREFUND-CEN       TO  P-AH-PREM-WRITTEN-CEN.         00119543
122000     MOVE  '.'                 TO  P-AH-PREM-WRITTEN-DEC.         00119544
122000                                                                  00119545
062100 BUILD-DOLLARS-EXIT.                                              00119546
062100     EXIT.                                                        00119547
122000                                                                  00119548
167600                                                                  00119550
064500 ACCT-BREAK.                                                      00119600
167600                                                                  00119700
158000*     DISPLAY 'ACCT-BREAK  ENTERED '.                             00119800
158100                                                                  00119900
167600      MOVE   '0'               TO    LG-SEQUENCE-NO.              00120000
167600      MOVE   SAVE-FULL-ACCT    TO    LG-ACCT-NO                   00120101
064500      MOVE   SAVE-STATE        TO    LG-STATE                     00120240
064500      MOVE   '1'               TO    LG-TRANS-TYPE.               00120250
064500      MOVE   '0'               TO    LG-SEQUENCE-NO.              00120260
064500      MOVE   'CSO'             TO    LG-CLIENT-ID.                00120270
064500      MOVE   '9000000'         TO    LG-CARR-CO.                  00120280
158000                                                                  00122200
167600      IF  SAVE-FULL-ACCT  =  ZEROS OR SPACES                      00122210
167600        MOVE  ZEROS TO    LG-BATCH-NO                             00122211
158000        DISPLAY ' '                                               00122212
158000        DISPLAY 'LAURITZEN-BATCH-HDR IS ZEROS  '                  00122213
158000                 LAURITZEN-BATCH-HDR                              00122213
158000         GO TO SKIP-WRITE.                                        00122220
158000                                                                  00122230
158000      MOVE LAURITZEN-BATCH-HDR TO  OUT-RECORD.                    00122300
167600      MOVE LG-BATCH-NUMB       TO  SR-BATCH-NUMB.                 00122400
157900      MOVE ZEROS               TO  SR-REC-CNT.                    00122500
167600                                                                  00122600
168300      WRITE CARD-RECORD        FROM OUT-RECORD.                   00122700
167600                                                                  00122710
064500 SKIP-WRITE.                                                      00122711
167600                                                                  00122712
158000      DISPLAY ' '.                                                00122720
158000      DISPLAY 'LAURITZEN-BATCH-HDR  ' LAURITZEN-BATCH-HDR.        00122730
158100                                                                  00122800
167600      ADD    1                TO    LG-BATCH-NO.                  00122900
167600      MOVE  ZEROS             TO    LG-LF-WRITTEN                 00123000
167600                                    LG-LF-CANC                    00123100
167600                                    LG-AH-WRITTEN                 00123200
167600                                    LG-AH-CANC                    00123300
167600                                    LG-CERT-ISS                   00123400
167600                                    LG-CERT-CANC                  00123500
167600                                    CERT-CNT                      00123600
167600                                    CANC-CNT                      00123700
167600                                    DETAIL-CNT.                   00123800
167600                                                                  00123801
062100      IF  LAST-REC-SW = 'Y'                                       00123802
064500          GO TO ACCT-BREAK-EXIT.                                  00123803
167600                                                                  00123804
058700      MOVE  RPT-DT (SAVE-WORK-MO) TO RPT-REPORT-DATE.             00123810
167600                                                                  00123811
064500      MOVE  V-STATE-CODE          TO  SAVE-STATE.                 00123832
064500      MOVE  V-STATE-CODE          TO  LG-STATE.                   00123833
167600                                                                  00123834
122000      MOVE  V-ACCOUNT-ID          TO  SAVE-FULL-ACCT.             00123840
167600                                                                  00123900
064500 ACCT-BREAK-EXIT.                                                 00124000
064500      EXIT.                                                       00124100
064500                                                                  00126700
170500 END-OF-JOB SECTION.                                              00127600
158100                                                                  00127601
167900*    DISPLAY 'END-OF-JOB   ENTERED '.                             00127602
158100                                                                  00127610
063500      CLOSE LAURITZEN-IN                                          00127620
063500            PPA-OUT                                               00127640
063500            LAURITZEN-OUT.                                        00127650
062100                                                                  00127680
171000     DISPLAY '************************************************'.  00128000
171100     DISPLAY 'INPUT RECORDS      -- ' IN-CNT.                     00128100
171200     DISPLAY ' '.                                                 00128200
171300     DISPLAY 'RECS WRITTEN COUNT -- ' DETAIL-CNT-TOT.             00128300
171400     DISPLAY ' '.                                                 00128400
171500     DISPLAY 'ERROR RECORD COUNT -- ' RPT-CNT.                    00128500
171600     DISPLAY ' '.                                                 00128600
171700     DISPLAY '************************************************'.  00128700
171800                                                                  00128701
171300     MOVE ZEROS TO  DETAIL-CNT-TOT.                               00128710
172000                                                                  00129000
172200     ADD  1  TO  RPT-PAGE-CNT.                                    00129100
172200     MOVE  RPT-PAGE-CNT     TO RPT-PAGE.                          00129100
172300     MOVE  4                TO RPT-LINE-CNT.                      00129200
172500     MOVE  ' LAURITZEN GROUP INPUT TOTALS FOR ' TO  LG-RPT-ID.    00129400
172600     MOVE  RPT-HD-LINE1     TO RPT-REC.                           00129500
172700     WRITE RPT-REC-OUT      AFTER ADVANCING PAGE.                 00129600
172900     MOVE  SPACES           TO RPT-REC.                           00129800
173000     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00129900
173200     MOVE  REPORT-HD-LINE   TO RPT-REC.                           00130100
173300     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00130200
173400     ADD  1                 TO  RPT-LINE-CNT.                     00130300
173600     MOVE  SPACES           TO RPT-REC.                           00130500
173700     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00130600
177200                                                                  00130700
177300     MOVE  TOT-LIFE-WRITTEN TO T-LIFE-WRITTEN.                    00130800
177400     MOVE  TOT-LIFE-CANC    TO T-LIFE-CANC.                       00130900
177500     MOVE  TOT-AH-WRITTEN   TO T-AH-WRITTEN.                      00131000
177600     MOVE  TOT-AH-CANC      TO T-AH-CANC.                         00131100
177700     MOVE  TOT-CERT-ISS     TO T-CERT-ISS.                        00131200
177800     MOVE  TOT-CERT-CANC    TO T-CERT-CANC.                       00131300
177800     MOVE  TOT-BALLOON-AMT  TO T-BALLOON-AMT.                     00131300
177900                                                                  00131400
178100     MOVE  TOTAL-LINE       TO RPT-REC.                           00131600
178200     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00131700
178300     ADD  1                 TO  RPT-LINE-CNT.                     00131800
178500     MOVE  SPACES           TO RPT-REC.                           00132000
178600     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00132100
178700                                                                  00132200
178800     CLOSE RPT-FILE.                                              00132300
178900                                                                  00132400
179000     GOBACK.                                                      00132500
179100                                                                  00132600
