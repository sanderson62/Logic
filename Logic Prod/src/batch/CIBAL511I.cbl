000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.                cibal511i.                              00000200
001500 ENVIRONMENT DIVISION.                                            00001500
001600 INPUT-OUTPUT SECTION.                                            00001600
001700 FILE-CONTROL.                                                    00001700
001800     SELECT  cert-in         ASSIGN TO SYS010.                    00001800
001900     SELECT  PACESETR-OUT    ASSIGN TO SYS012.                    00001900
002000     SELECT  RPT-FILE        ASSIGN TO SYS013.
00059      SELECT ELCERT           ASSIGN TO ELCERT
00060                              ORGANIZATION IS INDEXED
00061                              ACCESS IS DYNAMIC
00062                              RECORD KEY IS CM-CONTROL-PRIMARY
00063                              FILE STATUS IS ELCERT-FILE-STATUS.
002200                                                                  00002200
002300 SKIP3                                                            00002300
002400 DATA DIVISION.                                                   00002400
002500                                                                  00002500
002600 FILE SECTION.                                                    00002600
003100                                                                  00004050
003200 FD  ELCERT.                                                      00004060
003900                                                                  00004093
004000                                COPY ELCCERT.                     00004094
004100                                                                  00004095
003200 FD  cert-in                                                      00004060
003400     RECORDING MODE IS F                                          00004070
003500     LABEL RECORDS ARE STANDARD                                   00004080
003700     BLOCK CONTAINS 0 RECORDS.                                    00004091
003900                                                                  00004093
004000                                copy ecscrt01.                    00004094
004100                                                                  00004095
004600 FD  PACESETR-OUT                                                 00004500
004700     RECORDING MODE IS F                                          00004600
004800     LABEL RECORDS ARE STANDARD                                   00004700
004900     RECORD CONTAINS 90 CHARACTERS                                00004800
003700     BLOCK CONTAINS 0 RECORDS                                     00004900
005000     DATA RECORD IS CARD-RECORD.                                  00005000
005100                                                                  00005100
005200 01  CARD-RECORD             PIC X(90).                           00005200
005300                                                                  00005300
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
007800 77  SAVE-CERT              PIC X(11)     VALUE SPACES.           00008200
007800 77  SAVE-STATE             PIC X(02)     VALUE SPACES.           00008210
008300 77  RPT-PAGE-CNT           PIC 9999      VALUE ZEROS.            00008600
008400 77  RPT-CANCL-CNT          PIC 99999     VALUE ZEROS.            00008700
008500 77  RPT-ISS-CNT            PIC 99999     VALUE ZEROS.            00008800
008600 77  RPT-LINE-CNT           PIC 99        VALUE ZEROS.            00008900
008800 77  RPT-CNT                PIC 9999999   VALUE ZEROS.            00009000
008900 77  DETAIL-CNT             PIC 9999      VALUE ZEROS.            00009100
008900 77  NINES-IN               PIC S9(12) VALUE 999999999999.        00009101
008900 77  DETAIL-CNT-TOT         PIC 9999999   VALUE ZEROS.            00009110
009000 77  IN-CNT                 PIC 9999999   VALUE ZEROS.            00009200
009100 77  TOT-LIFE-WRITTEN       PIC S9(8)V99  VALUE ZEROS.            00009600
009200 77  TOT-LIFE-CANC          PIC S9(8)V99  VALUE ZEROS.            00009700
009300 77  TOT-AH-WRITTEN         PIC S9(8)V99  VALUE ZEROS.            00009800
009400 77  TOT-AH-CANC            PIC S9(8)V99  VALUE ZEROS.            00009900
009500 77  TOT-CERT-ISS           PIC S9(4)     VALUE ZEROS.            00010000
009600 77  TOT-CERT-CANC          PIC S9(4)     VALUE ZEROS.            00010100
010200 77  CANC-CNT               PIC 9999      VALUE ZEROS.            00010300
010200 77  CERT-CNT               PIC 9999      VALUE ZEROS.            00010400
010200 77  W-PROC-MO              PIC 99        VALUE ZEROS.            00010410
010300 77  FIRST-SW               PIC X         VALUE 'Y'.              00010500
010300 77  WRITTEN-PREM-SW        PIC X         VALUE 'N'.              00010510
010300 77  SAVE-WORK-MO           PIC 99        VALUE ZEROS.            00010520
010400                            COPY ELCDATE.                         00010600
       01  misc.
00221      05  WS-ZERO                PIC S9     VALUE +0 COMP-3.
00222      05  WS-RETURN-CODE         PIC S9(4)  VALUE +0 COMP-3.
00223      05  WS-ABEND-MESSAGE       PIC X(80)  VALUE SPACES.
00224      05  WS-ABEND-FILE-STATUS   PIC XX     VALUE ZERO.
           05  ELCERT-FILE-STATUS     PIC XX VALUE '00'.
           05  ws-work-dt             pic 9(11).
           05  ws-work-dt-a redefines ws-work-dt.
               10  filler             pic x(5).
               10  v-iss-yr           pic xx.
               10  v-iss-mo           pic xx.
               10  v-iss-da           pic xx.
010500 01  BUILD-APR.                                                   00010830
010600     05  W-APR-N                PIC 9(06).                        00010831
010500     05  W-APR-X  REDEFINES  W-APR-N.                             00010832
010600         10  W-APR-ALL.                                           00010833
010600             15  W-APR-WHOLE    PIC X(02).                        00010834
010600             15  W-APR-DEC.                                       00010835
010600                 20  W-APR-DEC-FIRST2 PIC XX.                     00010836
010600                 20  W-APR-DEC-LAST2  PIC XX.                     00010837
010400                                                                  00010838
010500 01  W-LREFUND.                                                   00010839
010600     05  W-LREFUND-N            PIC 9(09).                        00010840
010500     05  W-LREFUND-X   REDEFINES  W-LREFUND-N.                    00010841
010600         10  W-LREFUND-DOL.                                       00010842
010600             15  W-LREFUND-DOL2 PIC X(02).                        00010843
010600             15  W-LREFUND-DOL5 PIC X(05).                        00010844
010600         10  W-LREFUND-CEN      PIC X(02).                        00010845
010400                                                                  00010846
010500 01  W-AREFUND.                                                   00010847
010600     05  W-AREFUND-N            PIC 9(09).                        00010848
010500     05  W-AREFUND-X   REDEFINES  W-AREFUND-N.                    00010849
010600         10  W-AREFUND-DOL.                                       00010850
010600             15  W-AREFUND-DOL2 PIC X(02).                        00010851
010600             15  W-AREFUND-DOL5 PIC X(05).                        00010852
010600         10  W-AREFUND-CEN      PIC X(02).                        00010853
010400                                                                  00010854
CIDMOD                                                                  00010855
CIDMOD 01  AH-TERM-CHECK.                                               00010856
CIDMOD     05  AH-X                  PIC XXX.                           00010857
CIDMOD     05  AH-N  REDEFINES AH-X  PIC 999.                           00010858
010400                                                                  00010855
010500 01  W-PR-NOTE-AMT.                                               00010856
010600     05  W-PN-AMT-N            PIC 9(08).                         00010857
010500     05  W-PN-AMT-X   REDEFINES  W-PN-AMT-N.                      00010858
010600         10  W-PN-AMT-DOL      PIC X(06).                         00010859
010600         10  W-PN-AMT-CEN      PIC X(02).                         00010860
010400                                                                  00010861
010400                                                                  00010862
010500 01  W-PROC-YR-CEN.                                               00010863
010600     05  W-PROC-CEN         PIC X(02)     VALUE SPACES.           00010864
010600     05  W-PROC-YR          PIC 9(02)     VALUE ZEROS.            00010865
010400                                                                  00010866
010500 01  OUT-RECORD.                                                  00010867
010600     05  OUT-CERT-NO        PIC X(11)     VALUE SPACES.           00010870
010600*    05  OUT-CERT-SFX       PIC X(01)     VALUE SPACES.           00010900
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
019500 01  PACESETR-BATCH-HDR.                                          00012100
020500     05  PS-CARR-CO.                                              00012200
020600         10  PS-CARR         PIC X         VALUE '9'.             00012300
020700         10  PS-COMP         PIC X(6)      VALUE '000000'.        00012400
020800     05  PS-STATE            PIC XX        VALUE '  '.            00012500
020900     05  PS-ACCT-NO          PIC X(10)     VALUE SPACES.          00012600
020200     05  PS-BATCH-NUMB.                                           00012700
020200         10  PS-BATCH-ID     PIC XX        VALUE ZEROS.           00012700
020200         10  PS-BATCH-DA     PIC XX        VALUE ZEROS.           00012700
020200         10  PS-BATCH-NO     PIC 99        VALUE ZEROS.           00012700
021000     05  PS-HD-EFF-DT.                                            00012800
021100         10  PS-HD-EFF-MO    PIC XX        VALUE SPACES.          00012900
021200         10  PS-HD-EFF-DA    PIC XX        VALUE SPACES.          00013000
021300         10  PS-HD-EFF-YR    PIC XX        VALUE SPACES.          00013100
020000     05  PS-CERT-ISS         PIC S9(4)     VALUE ZEROS.           00013200
019600     05  PS-LF-WRITTEN       PIC S9(7)V99  VALUE ZEROS.           00013300
019800     05  PS-AH-WRITTEN       PIC S9(7)V99  VALUE ZEROS.           00013400
020100     05  PS-CERT-CANC        PIC S9(4)     VALUE ZEROS.           00013500
019700     05  PS-LF-CANC          PIC S9(7)V99  VALUE ZEROS.           00013600
019900     05  PS-AH-CANC          PIC S9(7)V99  VALUE ZEROS.           00013700
020300     05  PS-CLIENT-ID        PIC X(3)      VALUE 'CSO'.           00013800
021400     05  PS-TRANS-TYPE       PIC X         VALUE '1'.             00013900
020400     05  PS-SEQUENCE-NO      PIC X         VALUE '0'.             00014000
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
026200     05  FILLER              PIC XX     VALUE SPACES.             00015400
026300     05  HD-BATCH-NO         PIC X(7)   VALUE SPACES.             00015500
026400     05  FILLER              PIC XX     VALUE SPACES.             00015600
026500     05  HD-STATE            PIC XXXXX  VALUE SPACES.             00015700
026600     05  FILLER              PIC XX     VALUE SPACES.             00015800
026700     05  HD-ACCT-NO          PIC X(10)  VALUE SPACES.             00015900
026800     05  FILLER              PIC XX     VALUE SPACES.             00016000
026900     05  HD-EFF-DT           PIC X(8)   VALUE SPACES.             00016100
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
028600     05  FILLER              PIC X(36)           VALUE SPACES.    00017500
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
030300     05  PS-RPT-ID           PIC X(34)           VALUE            00019100
030400         '       PACESETTER CORP REPORT FOR '.                    00019200
030500     05  RPT-REPORT-DATE     PIC X(14)           VALUE SPACES.    00019300
030600     05  FILLER              PIC X(16)           VALUE SPACES.    00019400
030600     05  FILLER              PIC X(16)           VALUE            00019500
030400         'PGM = PACESETR '.                                       00019600
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
055900     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040000
056000     DISPLAY '*   WORK-DATE-IN IS -- ' WORK-DATE-IN.              00040100
056100     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040200
056200                                                                  00040300
055700     MOVE    WORK-YR-IN    TO  WORK-YR-N.                         00039800
055700     MOVE    WORK-MO-IN    TO  WORK-MO-N.                         00039800
055700     MOVE    WORK-DA-IN    TO  WORK-DA-N.                         00039800
055800                                                                  00039900
055900     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040000
056000     DISPLAY '*   CURRENT DATE IS -- ' WK-DATE.                   00040100
056100     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040200
056200                                                                  00040300
056500     OPEN INPUT    cert-in ELCERT.                                00040400
           IF ELCERT-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' BAD OPEN ELCERT ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

056600                                                                  00040500
056800     OPEN OUTPUT   RPT-FILE                                       00040700
056800                      PACESETR-OUT.                               00040720
056900                                                                  00040800
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00040900
057100                                                                  00041000
057500     MOVE  ZEROS                  TO   PS-LF-CANC                 00041100
057600                                       PS-AH-CANC                 00041200
057700                                       PS-CERT-CANC.              00041300
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
010600*    MOVE  SPACES                 TO  OUT-CERT-SFX                00041395
088500     MOVE  ZEROS                  TO  PBI-C-LF-CANCEL-DATE.       00041396
088500     MOVE  ZEROS                  TO  PBI-C-AH-CANCEL-DATE.       00041397
088500     MOVE  ZEROS                  TO  PBI-C-LF-PREM-REFUND.       00041398
088500     MOVE  ZEROS                  TO  PBI-C-AH-PREM-REFUND.       00041399
057100                                                                  00041400
057200     MOVE  WK-MO                  TO   PS-HD-EFF-MO.              00041500
057400     MOVE  WK-DA                  TO   PS-HD-EFF-DA.              00041600
057300     MOVE  WK-YR                  TO   PS-HD-EFF-YR               00041700
057900     MOVE  WK-MO                  TO  WORK-MO-X.                  00041900
058000     MOVE  WK-YR                  TO  WORK-YR-X.                  00042000
058100     MOVE  WK-DA                  TO  WORK-DA-X.                  00042100
058200                                                                  00042200
058300     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00042300
058400     DISPLAY '*   WORK-DATE-X  IS -- ' WORK-DATE-X.               00042400
058500     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00042500
158100                                                                  00042600
058100     MOVE  WORK-MO-IN             TO  SAVE-WORK-MO.               00042700
058200                                                                  00042800
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
061400 010-READ-INPUT-FILE.                                             00047100
122000                                                                  00047200
061600     IF  RPT-CNT  GREATER THAN  10                                00047600
061700         GO TO 0200-ERRORS.                                       00047700
061800                                                                  00047800
061900     READ cert-in                                                 00047900
062100         AT END                                                   00048100
062100           MOVE 'Y'  TO  LAST-REC-SW                              00048101
122000             PERFORM  ACCT-BREAK   THRU   ACCT-BREAK-EXIT         00048110
061700               GO TO END-OF-JOB.                                  00048300
064000                                                                  00048330
063900     ADD  1    TO IN-CNT.                                         00048400
062100     MOVE 'N'  TO  LAST-REC-SW.                                   00048500
064000                                                                  00048512
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00048520
057100                                                                  00048530
           MOVE X'04'           TO CM-COMPANY-CD
           MOVE CR-CARRIER      TO CM-CARRIER
           MOVE CR-GROUPING     TO CM-GROUPING
           MOVE CR-STATE        TO CM-STATE
           MOVE CR-ACCOUNT      TO CM-ACCOUNT
           MOVE CR-CERT-NO      TO CM-CERT-NO
00888      MOVE CR-DT           TO DC-GREG-DATE-CYMD.
00889      MOVE 'L'             TO DC-OPTION-CODE.
00011      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   ELCDCS
00891      MOVE DC-BIN-DATE-1   TO CM-CERT-EFF-DT

           READ ELCERT
           IF ELCERT-FILE-STATUS = '00'
               DISPLAY ' CERT ALREADY ON FILE ' CR-FULL-CONTROL
              GO TO 010-READ-INPUT-FILE
           END-IF
00892
064300     .                                                            00053700
064400 PROCESS-INPUT.                                                   00053800
064500                                                                  00053900
062100     IF  FIRST-SW  =  'N'                                         00054109
062100         GO TO BUILD-PPA.                                         00054110
062100                                                                  00054111
064500     IF cr-state  =  SPACES                                       00054117
064500        MOVE ZEROS TO cr-state                                    00054118
064500        GO TO MOVE-ACCT-NO.                                       00054119
064300                                                                  00054120
CIDMOD     MOVE   cr-state            TO  SAVE-STATE.                   00054121
CIDMOD     MOVE   cr-state            TO  PS-STATE.                     00054130
122000                                                                  00054131
064500 MOVE-ACCT-NO.                                                    00054132
064500                                                                  00054133
062100     MOVE 'Y'                    TO  WRITTEN-PREM-SW.             00054134
167600     MOVE 'PI'                   TO   PS-BATCH-ID.                00054135
167600     MOVE WORK-MO-IN             TO   PS-BATCH-DA.                00054135
167600     ADD    1                    TO   PS-BATCH-NO.                00054136
058700     MOVE  RPT-DT (SAVE-WORK-MO) TO RPT-REPORT-DATE               00054137
122000     MOVE  cr-account            TO  SAVE-FULL-ACCT               00054138
062100     MOVE 'N'  TO  FIRST-SW.                                      00054139
167600                                                                  00054140
122000                                                                  00054141
062100 BUILD-PPA.                                                       00054142
122000                                                                  00054143
062100     GO  TO  CK-FOR-LOGIC-BUILD.                                  00054144
122000                                                                  00054145
122000*                                                                 00054146
122000                                                                  00054227
062100 CK-FOR-LOGIC-BUILD.                                              00054228
122000                                                                  00054229
122000     IF  cr-account    =  SAVE-FULL-ACCT                          00054620
064400         GO TO CK-ISS-CANC.                                       00054621
122000                                                                  00054630
122000                                                                  00054631
122000     PERFORM  ACCT-BREAK   THRU   ACCT-BREAK-EXIT.                00054632
122000                                                                  00054633
122000                                                                  00054640
064400 CK-ISS-CANC.                                                     00054700
122000                                                                  00054701
062100     MOVE 'Y'  TO  WRITTEN-PREM-SW.                               00054702
064700                                                                  00054710
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
057100     ADD  1  TO   PS-CERT-ISS.                                    00067311
057100     ADD  1  TO  TOT-CERT-ISS.                                    00067312
057100                                                                  00067313
082100     MOVE   SPACES                 TO  PBI-RECORD-BODY.           00067400
088300                                                                  00067500
           move cr-dt                    to ws-work-dt

088500     MOVE  V-ISS-YR                TO  WORK-YR-X.                 00067600
088600     MOVE  V-ISS-MO                TO  WORK-MO-X.                 00067700
088700     MOVE  V-ISS-DA                TO  WORK-DA-X.                 00067800
088500     MOVE  WORK-DATE-X             TO  PBI-CERT-EFF-DT-X.         00068000
089000                                                                  00068100
090600     MOVE  cr-cert-no              to pbi-cert-no                 00068200
089000                                      SAVE-CERT                   00068500
084700     MOVE  cr-lname                TO  PBI-I-INS-LAST-NAME.       00068600
084700     MOVE  cr-fname                TO  PBI-I-INS-1ST-NAME.        00068700
084700     MOVE  cr-init                 TO  PBI-I-INS-MIDDLE-INIT.     00068800
084800                                                                  00068900
083900     IF  cr-AGE NOT NUMERIC MOVE 40 TO  cr-AGE.                   00069000
083700     MOVE cr-AGE                   TO  PBI-I-INSURED-AGE-X.       00069300
083700     MOVE cr-sex                   TO  PBI-I-INSURED-SEX.         00069400
083700     MOVE SPACES                   TO  PBI-I-SOC-SEC-NO.          00069500
083700     MOVE SPACES                   TO  PBI-I-MEMBER-NO.           00069600
083700     MOVE SPACES                   TO  PBI-I-BIRTHDAY-X.          00069700
083700     MOVE 'E'                      TO  PBI-I-ENTRY-CD.            00069800
083700     MOVE 'A'                      TO  PBI-I-FORCE-CD.            00069900
083700     MOVE '2'                      TO  PBI-TRANS-TYPE.            00070000
083700     MOVE '1'                      TO  PBI-SEQUENCE.              00070100
083200                                                                  00070200
010800     MOVE  PBI-I-ISSUE-REC-SEQ-1   TO  OUT-INFO.                  00070210
010600     MOVE  SAVE-CERT               TO  OUT-CERT-NO.               00070300
010600*    MOVE  SPACES                  TO  OUT-CERT-SFX.              00070400
088500     MOVE  WORK-DATE-X             TO  OUT-EFF-DATE.              00070500
010800     MOVE  '2'                     TO  OUT-TRAN-TYPE              00070700
010800     MOVE  '1'                     TO  OUT-SEQUENCE.              00070800
088300                                                                  00070900
088300                                                                  00071000
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
082900     IF  cr-joint-name not = spaces                               00072620
082900         GO TO SEQ2-HAS-JT-VALUES.                                00072621
083200                                                                  00072622
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
088500*    MOVE   V-ISS-YR               TO  WORK-YR-X.                 00072900
088600*    MOVE   V-ISS-MO               TO  WORK-MO-X.                 00073000
088700*    MOVE   V-ISS-DA               TO  WORK-DA-X.                 00073100
088500     MOVE   WORK-DATE-X            TO  PBI-CERT-EFF-DT-X.         00073300
089000                                                                  00073400
090600     MOVE   SAVE-CERT              TO  PBI-CERT-PRIME.            00073500
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00073600
084000                                                                  00073700
084700     MOVE   cr-jt-lname            TO  PBI-I-JNT-LAST-NAME.       00073800
084700     MOVE   cr-jt-fname            TO  PBI-I-JNT-1ST-NAME.        00073900
084700     MOVE   cr-jt-init             TO  PBI-I-JNT-MIDDLE-INIT.     00074000
084000                                                                  00074010
084800     IF     cr-joint-age not numeric                              00074100
084800            MOVE 00                TO  cr-joint-age               00074101
           end-if
084800     MOVE   cr-joint-age           TO  PBI-I-JOINT-AGE-X.         00074102
084000                                                                  00074110
084800     MOVE   cr-policy-form-no      TO  PBI-I-POLICY-FORM-NO.      00074200
084000                                                                  00074300
083700     MOVE   '2'                    TO  PBI-TRANS-TYPE.            00074400
083700     MOVE   '2'                    TO  PBI-SEQUENCE.              00074500
083200                                                                  00074600
010800     MOVE   PBI-I-ISSUE-REC-SEQ-2  TO  OUT-INFO.                  00074610
010600     MOVE   SAVE-CERT              TO  OUT-CERT-NO.               00074700
010600*    MOVE   SPACES                 TO  OUT-CERT-SFX.              00074800
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
088500*    MOVE   V-ISS-YR               TO  WORK-YR-X.                 00077500
088600*    MOVE   V-ISS-MO               TO  WORK-MO-X.                 00077600
088700*    MOVE   V-ISS-DA               TO  WORK-DA-X.                 00077700
088500     MOVE   WORK-DATE-X            TO  PBI-CERT-EFF-DT-X.         00077900
089000                                                                  00078000
090600     MOVE   SAVE-CERT              TO  PBI-CERT-PRIME.            00078100
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00078200
093900                                                                  00079210
094000     IF     cr-lftyp       EQUAL '00' or '  '                     00079600
094000            MOVE '  '              TO  cr-lftyp                   00079610
094000            GO TO LIFE-CODE-DONE.                                 00079620
094000                                                                  00079710
094000     move cr-lftyp           to pbi-i-lf-benefit-type

094000     .                                                            00079900
094000 LIFE-CODE-DONE.                                                  00080000
093900                                                                  00082000
092000     MOVE  cr-LF-TERM               TO  PBI-I-LF-TERM             00082300
089000                                                                  00082310
092800     IF     cr-lfamt     NOT NUMERIC                              00082325
092800                       MOVE ZEROS  TO cr-lfamt.                   00082330
092800     IF     PBI-I-LF-BENEFIT-AMT   NOT NUMERIC                    00082340
092800                       MOVE ZEROS  TO PBI-I-LF-BENEFIT-AMT.       00082350
092800     ADD    cr-lfamt               TO  PBI-I-LF-BENEFIT-AMT.      00082360
057100                                                                  00082700
093500     IF    cr-lfprm            NOT NUMERIC                        00082800
093500                      MOVE ZEROS   TO cr-lfprm.                   00082810
092800     IF     PBI-I-LF-PREM-AMT      NOT NUMERIC                    00082820
092800                       MOVE ZEROS  TO PBI-I-LF-PREM-AMT.          00082830
119800                                                                  00082841
093500     ADD   cr-lfprm                TO  PBI-I-LF-PREM-AMT          00082866
176600                                       TOT-LIFE-WRITTEN           00082867
157400                                        PS-LF-WRITTEN.            00082868
119800                                                                  00082869
093500     MOVE  ZEROS                   TO  PBI-I-LF-CRIT-PERIOD.      00083100
119800                                                                  00083200
093500     MOVE  ZEROS                   TO  PBI-I-LF-EXPIRE-YR.        00083300
093500     MOVE  ZEROS                   TO  PBI-I-LF-EXPIRE-MO.        00083400
093500     MOVE  ZEROS                   TO  PBI-I-LF-EXPIRE-DA.        00083500
120100                                                                  00083600
093500     MOVE cr-lfamt-alt             TO  PBI-I-LF-ALT-BENEFIT-AMT.  00083722
093500     MOVE cr-lfprm-alt             TO  PBI-I-LF-ALT-PREM-AMT.     00083760
120100                                                                  00083810
093500     MOVE SPACES                   TO  PBI-I-TERM-AS-DAYS-X.      00083900
 84000                                                                  00084000
083700     MOVE '2'                      TO  PBI-TRANS-TYPE.            00084100
083700     MOVE '3'                      TO  PBI-SEQUENCE.              00084200
083200                                                                  00084300
088300                                                                  00084600
010800     MOVE  PBI-I-ISSUE-REC-SEQ-3   TO  OUT-INFO.                  00084610
010600     MOVE  SAVE-CERT               TO  OUT-CERT-NO.               00084700
010600*    MOVE  SPACES                  TO  OUT-CERT-SFX.              00084800
088500     MOVE  WORK-DATE-X             TO  OUT-EFF-DATE.              00084900
010800     MOVE  '2'                     TO  OUT-TRAN-TYPE              00085100
010800     MOVE  '3'                     TO  OUT-SEQUENCE.              00085200
088300                                                                  00085300
121900     PERFORM  0150-WRITE-DATA-CARD THRU 0150-EXIT.                00085400
122000                                                                  00085500
093500     MOVE ZEROS                    TO  PBI-I-LF-PREM-AMT          00085510
092800                                       PBI-I-LF-BENEFIT-AMT.      00085512
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
088500*    MOVE   V-ISS-YR               TO  WORK-YR-X.                 00087700
088600*    MOVE   V-ISS-MO               TO  WORK-MO-X.                 00087800
088700*    MOVE   V-ISS-DA               TO  WORK-DA-X.                 00087900
088500     MOVE   WORK-DATE-X            TO  PBI-CERT-EFF-DT-X.         00088100
089000                                                                  00088200
090600     MOVE   SAVE-CERT              TO  PBI-CERT-PRIME.            00088300
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00088400
107200                                                                  00088500
093900                                                                  00088800
119500     IF   cr-ahprm                 GREATER THAN ZEROS             00088900
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
010600*          MOVE  SPACES            TO  OUT-CERT-SFX               00090500
088500           MOVE  WORK-DATE-N       TO  OUT-EFF-DATE               00090600
010800           MOVE  PBI-I-ISSUE-REC-SEQ-4  TO  OUT-INFO              00090700
010800           MOVE  '2'               TO  OUT-TRAN-TYPE              00090800
010800           MOVE  '4'               TO  OUT-SEQUENCE               00090900
082900           GO  TO  SEQ5-BLD.                                      00091010
093900                                                                  00091100
119500     IF   cr-ahprm                 NOT NUMERIC                    00091300
119500                        MOVE ZEROS TO cr-ahprm.                   00091301
119500     IF   PBI-I-AH-PREM-AMT        NOT NUMERIC                    00091302
119500                             MOVE  ZEROS TO  PBI-I-AH-PREM-AMT.   00091303
119500     ADD  cr-ahprm                 TO  PBI-I-AH-PREM-AMT          00091306
157500                                         PS-AH-WRITTEN            00091320
176800                                          TOT-AH-WRITTEN.         00091330
119800                                                                  00091340
119500     IF   cr-ahamt     NOT NUMERIC                                00091600
119500                        MOVE ZEROS TO  cr-ahamt.                  00091601
119500     IF   PBI-I-AH-BENEFIT-AMT     NOT NUMERIC                    00091602
119500                    MOVE  ZEROS    TO  PBI-I-AH-BENEFIT-AMT.      00091603
119500     ADD  cr-ahamt                 TO  PBI-I-AH-BENEFIT-AMT.      00091604
093900                                                                  00092166
094000 MOVE-AH-BEN-CODE.                                                00092167
093900                                                                  00092168
094000     MOVE cr-ahtyp                 TO  PBI-I-AH-BENEFIT-NO.       00092169
094000     MOVE  ' '                     TO  PBI-I-AH-BENEFIT-POS1.     00092170
093900                                                                  00092820
119500     MOVE cr-ah-term               TO  PBI-I-AH-TERM-X.           00092900
118500                                                                  00093000
119500     MOVE cr-AH-CRIT-PERIOD         TO  PBI-I-AH-CRIT-PERIOD.     00093100
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
010600*    MOVE  SPACES                  TO  OUT-CERT-SFX.              00094600
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
088500*    MOVE   V-ISS-YR               TO  WORK-YR-X.                 00095991
088600*    MOVE   V-ISS-MO               TO  WORK-MO-X.                 00095992
088700*    MOVE   V-ISS-DA               TO  WORK-DA-X.                 00095993
088500     MOVE   WORK-DATE-X            TO  PBI-CERT-EFF-DT-X.         00095994
089000                                                                  00095995
090600     MOVE   SAVE-CERT              TO  PBI-CERT-PRIME.            00095996
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00095997
084000                                                                  00095998
084700     MOVE   SPACES                 TO  PBI-I-LOAN-OFFICER.        00095999
084700     MOVE   cr-LOAN-TERM           TO  PBI-I-LOAN-TERM.           00096000
           move cr-1st-pmt-yr            to  pbi-i-1st-pmt-yr
           move cr-1st-pmt-mo            to pbi-i-1st-pmt-mo
           move cr-1st-pmt-da            to pbi-i-1st-pmt-da
           MOVE CR-IND-GRP               TO PBI-I-INDV-GRP-CD
           MOVE CR-PMT-FREQ              TO PBI-I-PAY-FREQUENCY
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
084000     move cr-rating-class    to pbi-i-rate-class.                 00096039
CIDMOD                                                                  00079770
084700     IF     cr-apr       NOT NUMERIC                              00096040
084700            MOVE  ZEROS            TO  cr-apr.                    00096041
084000                                                                  00096042
084700     IF     PBI-I-LOAN-APR   NOT NUMERIC                          00096043
084700            MOVE  ZEROS            TO  PBI-I-LOAN-APR.            00096044
084000                                                                  00096045
084700     MOVE   cr-apr                 TO  PBI-I-LOAN-APR.            00096046
084000                                                                  00096047
083700     MOVE   '2'                    TO  PBI-TRANS-TYPE.            00096050
083700     MOVE   '5'                    TO  PBI-SEQUENCE.              00096060
083200                                                                  00096070
010800     MOVE   PBI-I-ISSUE-REC-SEQ-5  TO  OUT-INFO.                  00096080
010600     MOVE   SAVE-CERT              TO  OUT-CERT-NO.               00096090
010600*    MOVE   SPACES                 TO  OUT-CERT-SFX.              00096091
088500     MOVE   WORK-DATE-X            TO  OUT-EFF-DATE.              00096092
010800     MOVE   '2'                    TO  OUT-TRAN-TYPE.             00096093
010800     MOVE   '5'                    TO  OUT-SEQUENCE.              00096094
088300                                                                  00096095
121900     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00096110
122000                                                                  00096120
122000                                                                  00096150
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
152100                                                                  00100550
122900*                   ACARD                                         00100600
137200                                                                  00111300
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
144900     MOVE  PS-BATCH-NUMB       TO  SR-BATCH-NUMB.                 00115100
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
166700     CLOSE   cert-in                                              00118700
167000             PACESETR-OUT.                                        00118910
167100                                                                  00119000
167200     GO TO END-OF-JOB.                                            00119100
167300                                                                  00119200
167400 0200-EXIT.                                                       00119300
167500      EXIT.                                                       00119400
122000                                                                  00119410
122000                                                                  00119548
167600                                                                  00119550
064500 ACCT-BREAK.                                                      00119600
167600                                                                  00119700
158000*     DISPLAY 'ACCT-BREAK  ENTERED '.                             00119800
158100                                                                  00119900
167600      MOVE   '0'               TO    PS-SEQUENCE-NO.              00120000
167600      MOVE   SAVE-FULL-ACCT    TO    PS-ACCT-NO                   00120101
064500      MOVE   SAVE-STATE        TO    PS-STATE                     00120240
064500      MOVE   '1'               TO    PS-TRANS-TYPE.               00120250
064500      MOVE   '0'               TO    PS-SEQUENCE-NO.              00120260
064500      MOVE   'CSO'             TO    PS-CLIENT-ID.                00120270
064500      MOVE   '9000000'         TO    PS-CARR-CO.                  00120280
158000                                                                  00122200
167600      IF  SAVE-FULL-ACCT  =  ZEROS OR SPACES                      00122210
167600        MOVE  ZEROS TO    PS-BATCH-NO                             00122211
158000        DISPLAY ' '                                               00122212
158000        DISPLAY 'PACESETR-BATCH-HDR IS ZEROS  ' PACESETR-BATCH-HDR00122213
158000         GO TO SKIP-WRITE.                                        00122220
158000                                                                  00122230
158000      MOVE    PACESETR-BATCH-HDR TO OUT-RECORD.                   00122300
167600      MOVE     PS-BATCH-NUMB   TO   SR-BATCH-NUMB.                00122400
157900      MOVE    ZEROS            TO   SR-REC-CNT.                   00122500
167600                                                                  00122600
168300      WRITE CARD-RECORD        FROM OUT-RECORD.                   00122700
167600                                                                  00122710
064500 SKIP-WRITE.                                                      00122711
167600                                                                  00122712
158000      DISPLAY ' '.                                                00122720
158000      DISPLAY 'PACESETR-BATCH-HDR  ' PACESETR-BATCH-HDR.          00122730
158100                                                                  00122800
167600      ADD    1                 TO    PS-BATCH-NO.                 00122900
167600      MOVE  ZEROS              TO    PS-LF-WRITTEN                00123000
167600                                     PS-LF-CANC                   00123100
167600                                     PS-AH-WRITTEN                00123200
167600                                     PS-AH-CANC                   00123300
167600                                     PS-CERT-ISS                  00123400
167600                                     PS-CERT-CANC                 00123500
167600                                    CERT-CNT                      00123600
167600                                    CANC-CNT                      00123700
167600                                    DETAIL-CNT.                   00123800
167600                                                                  00123801
062100      IF  LAST-REC-SW = 'Y'                                       00123802
064500          GO TO ACCT-BREAK-EXIT.                                  00123803
167600                                                                  00123804
058700      MOVE  RPT-DT (SAVE-WORK-MO) TO RPT-REPORT-DATE.             00123810
167600                                                                  00123811
064500      MOVE  cr-state              TO  SAVE-STATE.                 00123832
064500      MOVE  cr-state              TO   PS-STATE.                  00123833
167600                                                                  00123834
122000      MOVE  cr-account            TO  SAVE-FULL-ACCT.             00123840
167600                                                                  00123900
064500 ACCT-BREAK-EXIT.                                                 00124000
064500      EXIT.                                                       00124100
167600                                                                  00124200
064500                                                                  00126700
170500 END-OF-JOB SECTION.                                              00127600
158100                                                                  00127601
167900*    DISPLAY 'END-OF-JOB   ENTERED '.                             00127602
158100                                                                  00127610
063500      CLOSE cert-in                                               00127620
063500*           PPA-OUT                                               00127640
063500            PACESETR-OUT.                                         00127650
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
172200     MOVE  RPT-PAGE-CNT     TO RPT-PAGE.                          00129100
172300     MOVE  4                TO RPT-LINE-CNT.                      00129200
172500     MOVE  '   PACESETR CORP INPUT TOTALS FOR ' TO  PS-RPT-ID.    00129400
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
02256  ABEND-PGM.   COPY ELCABEND.

179100                                                                  00132600
