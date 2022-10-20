000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.                 CIUMFMT.                             00000200
000300                                                                  00000300
000400*AUTHOR.     CENTRAL STATES OF OMAHA.                             00000400
000500*            OMAHA, NEBR.                                         00000500
000600                                                                  00000600
000700*DATE-COMPILED.                                                   00000700
000800                                                                  00000800
000900*REMARKS.                                                         00000900
001000*    THIS PROGRAM CONVERTS UMB INPUT INFO BASED ON A CONVERSION   00001000
001100*      TABLE. FIELDS INVOLVED ARE : EFFECTIVE DATE                00001100
001200*                                   ACCOUNT NUMBER                00001200
001300*                                   CERTIFICATE NUMBER.           00001300
001400                                                                  00001400
001500*    ONLY CANCELLATION RECORDS ARE INVOLVED IN THE CHANGE.        00001500
001600*         ============ =======                                    00001600
001600                                                                  00001610
001700 ENVIRONMENT DIVISION.                                            00001700
001800 INPUT-OUTPUT SECTION.                                            00001800
001900 FILE-CONTROL.                                                    00001900
002000     SELECT  UMB-LIFE-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.   00002000
002100     SELECT  UMB-LIFE-OUT    ASSIGN TO SYS012-UT-2400-S-SYS012.   00002100
002200     SELECT  CID-CONV-FILE   ASSIGN TO SYS011-UT-2400-S-SYS011.   00002200
002300                                                                  00002300
002400 SKIP3                                                            00002400
002500 DATA DIVISION.                                                   00002500
002600                                                                  00002600
002700 FILE SECTION.                                                    00002700
002800                                                                  00002800
002900******************************************************************00002900
003000**        INPUT TAPE FILE FROM UNITED MISSOURI BANK (UMB)       **00003000
003100******************************************************************00003100
003200                                                                  00003200
003300 FD  UMB-LIFE-IN                                                  00003300
003400     RECORDING MODE IS F                                          00003400
003500     LABEL RECORDS ARE STANDARD                                   00003500
003600     RECORD CONTAINS 150 CHARACTERS                               00003600
003700     BLOCK CONTAINS 0 RECORDS                                     00003700
003800     DATA RECORD IS UMB-LIFE-RECORD.                              00003800
003900                                                                  00003900
004000 01  UMB-LIFE-RECORD            PIC X(150).                       00004000
004100                                                                  00004100
004200******************************************************************00004200
004300**     INPUT CONVERSION FILE FOR UMB-TO-CSO CERT/ACCT NUMBS.    **00004300
004400******************************************************************00004400
004500                                                                  00004500
004600 FD  CID-CONV-FILE                                                00004600
004700     RECORDING MODE IS F                                          00004700
004800     LABEL RECORDS ARE STANDARD                                   00004800
003600     RECORD CONTAINS 80 CHARACTERS                                00004900
005000     BLOCK CONTAINS 0 RECORDS                                     00005000
005100     DATA RECORD IS CONV-RECORD.                                  00005100
005200                                                                  00005200
005300 01  CONV-RECORD             PIC X(80).                           00005300
005400                                                                  00005400
005500                                                                  00005500
005600******************************************************************00005600
005700**       OUTPUT CARD FILE FOR INPUT TO PROGRAM 'CIUMB511'.      **00005700
005800******************************************************************00005800
005900                                                                  00005900
006000 FD  UMB-LIFE-OUT                                                 00006000
006100     RECORDING MODE IS F                                          00006100
006200     LABEL RECORDS ARE STANDARD                                   00006200
006300     RECORD CONTAINS 150 CHARACTERS                               00006300
006400     BLOCK CONTAINS 0 RECORDS                                     00006400
006500     DATA RECORD IS CONVERTED-RECORD.                             00006500
006600                                                                  00006600
006700 01  CONVERTED-RECORD        PIC X(150).                          00006700
006800                                                                  00006800
006900******************************************************************00006900
007000******************************************************************00007000
007100 SKIP3                                                            00007100
007200                                                                  00007200
007300 WORKING-STORAGE SECTION.                                         00007300
007400 77  FILLER  PIC X(32) VALUE '********************************'.  00007400
007500 77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  00007500
007600 77  FILLER  PIC X(32) VALUE '********************************'.  00007600
007700                                                                  00007700
007800 77  C-SUB                  PIC 999       VALUE ZEROS.            00007800
007900 77  IN-CNT                 PIC 99999     VALUE ZEROS.            00007900
008000 77  RPT-CNT                PIC 9999999   VALUE ZEROS.            00008000
008100 77  TOT-LIFE-WRITTEN       PIC S9(8)V99  VALUE ZEROS.            00008100
008200 77  TOT-LIFE-CANC          PIC S9(8)V99  VALUE ZEROS.            00008200
008300 77  TOT-AH-WRITTEN         PIC S9(8)V99  VALUE ZEROS.            00008300
008400 77  TOT-AH-CANC            PIC S9(8)V99  VALUE ZEROS.            00008400
008500 77  TOT-CERT-ISS           PIC  9(5)     VALUE ZEROS.            00008500
008600 77  TOT-CERTS-CHANGED      PIC  9(5)     VALUE ZEROS.            00008600
008700 77  TOT-CERTS-NOCHANGE     PIC  9(5)     VALUE ZEROS.            00008700
008800 77  TOT-CERT-CANC          PIC  9(5)     VALUE ZEROS.            00008800
008900 77  WRT-CNT                PIC 99999     VALUE ZEROS.            00008900
009000 77  WRITE-ISS-CNT          PIC 9999      VALUE ZEROS.            00009000
009100 77  WRITE-CNC-CNT          PIC 9999      VALUE ZEROS.            00009100
009200                                                                  00009200
009300 01  FILLER   PIC X(44) VALUE                                     00009300
009400      '****  UMB CONV FILE  ****'.                                00009400
009500                                                                  00009500
009600 01  UMB-CONV-REC.                                                00009600
009700     05  UMB-C-REC                 PIC X(80).                     00009700
009800                                                                  00009800
009900 01  CONV-REC REDEFINES UMB-CONV-REC.                             00009900
010000     05  C-REC  OCCURS  400.                                      00010000
010100         10  CONV-LAST-NAME        PIC X(17).                     00010100
010200         10  CONV-CSO-CERT         PIC X(10).                     00010200
010300         10  CONV-UMB-LOAN-NO      PIC X(10).                     00010300
010400         10  CONV-CSO-ACCT-NO      PIC X(10).                     00010400
010500         10  CONV-UMB-BANK-NO      PIC X(05).                     00010500
010600         10  CONV-CSO-DATE.                                       00010600
010700             15  CONV-MO           PIC X(02).                     00010700
010800             15  CONV-DA           PIC X(02).                     00010800
010900             15  CONV-YR           PIC X(02).                     00010900
010900         10  FILLER                PIC X(22).                     00010910
011000                                                                  00011000
011100                                                                  00011100
011200 01  FILLER   PIC X(44) VALUE                                     00011200
011300      '****  UMB RECORD IN  ****'.                                00011300
011400                                                                  00011400
011500 01  UMB-IN-REC.                                                  00011500
011600     05  UMB-TRANS                 PIC X(02)     VALUE SPACES.    00011600
011700     05  UMB-ACCT.                                                00011700
011800         10  UMB-BANK-NO           PIC X(02)     VALUE SPACES.    00011800
011900         10  UMB-BRANCH-NO         PIC X(03)     VALUE SPACES.    00011900
012000     05  UMB-CERT-NO.                                             00012000
012100         10  UMB-LOAN-TYPE         PIC X(02)     VALUE SPACES.    00012100
012200         10  UMB-LOAN-NO           PIC X(08)     VALUE SPACES.    00012200
012300     05  UMB-NEW-ACCOUNT.                                         00012300
012400         10  UMB-EFF-DATE.                                        00012400
012500             15  UMB-EFF-MO        PIC X(02)     VALUE SPACES.    00012500
012600             15  UMB-EFF-DA        PIC X(02)     VALUE SPACES.    00012600
012700             15  UMB-EFF-YR        PIC X(02)     VALUE SPACES.    00012700
012800         10  UMB-ADVANCE-DATE.                                    00012800
012900             15  UMB-ADV-MO        PIC X(02)     VALUE SPACES.    00012900
013000             15  UMB-ADV-DA        PIC X(02)     VALUE SPACES.    00013000
013100             15  UMB-ADV-YR        PIC X(02)     VALUE SPACES.    00013100
013200         10  UMB-PAYMENT-DATE.                                    00013200
013300             15  UMB-PMT-MO        PIC X(02)     VALUE SPACES.    00013300
013400             15  UMB-PMT-DA        PIC X(02)     VALUE SPACES.    00013400
013500             15  UMB-PMT-YR        PIC X(02)     VALUE SPACES.    00013500
013600         10  UMB-MATURITY-DATE.                                   00013600
013700             15  UMB-MATUR-MO      PIC X(02)     VALUE SPACES.    00013700
013800             15  UMB-MATUR-DA      PIC X(02)     VALUE SPACES.    00013800
013900             15  UMB-MATUR-YR      PIC X(02)     VALUE SPACES.    00013900
014000         10  UMB-LOAN-TERM         PIC X(03)     VALUE SPACES.    00014000
014100         10  UMB-LIFE-COVG         PIC X(01)     VALUE SPACES.    00014100
014200         10  UMB-LIFE-AMT          PIC 9(06)V99  VALUE ZEROS.     00014200
014300         10  UMB-LIFE-PREM         PIC 9(05)V99  VALUE ZEROS.     00014300
014400         10  UMB-ACC-COVG          PIC X(01)     VALUE SPACES.    00014400
014500         10  UMB-ACC-BENEFIT       PIC 9(05)V99  VALUE ZEROS.     00014500
014600         10  UMB-ACC-PREM          PIC 9(05)V99  VALUE ZEROS.     00014600
014700         10  UMB-OFFICER           PIC X(02)     VALUE SPACES.    00014700
014800         10  UMB-NAME              PIC X(29)     VALUE SPACES.    00014800
014900         10  UMB-AGE               PIC X(02)     VALUE SPACES.    00014900
015000         10  UMB-NAME2             PIC X(29)     VALUE SPACES.    00015000
015100         10  UMB-AGE2              PIC X(02)     VALUE SPACES.    00015100
015200         10  FILLER                PIC X(11)     VALUE SPACES.    00015200
015300     05  UMB-ADJ-CANCEL  REDEFINES  UMB-NEW-ACCOUNT.              00015300
015400         10  UMB-ADJ-CAN-DATE.                                    00015400
015500             15  UMB-ADJ-CAN-MO    PIC X(02).                     00015500
015600             15  UMB-ADJ-CAN-DA    PIC X(02).                     00015600
015700             15  UMB-ADJ-CAN-YR    PIC X(02).                     00015700
015800         10  UMB-ADJ-LIFE-AMT      PIC 9(05)V99.                  00015800
015900         10  UMB-ADJ-ACC-AMT       PIC 9(05)V99.                  00015900
016000         10  UMB-ADJ-EFF-DATE.                                    00016000
016100             15  UMB-ADJ-EFF-MO    PIC X(02).                     00016100
016200             15  UMB-ADJ-EFF-DA    PIC X(02).                     00016200
016300             15  UMB-ADJ-EFF-YR    PIC X(02).                     00016300
016400         10  FILLER                PIC X(107).                    00016400
016500                                                                  00016500
016600 01  FILLER   PIC X(44) VALUE                                     00016600
016700      '****  UMB RECORD OUT ****'.                                00016700
016800                                                                  00016800
016900 01  OUT-RECORD.                                                  00016900
017000     05  O-TRANS                   PIC X(02)     VALUE SPACES.    00017000
017100     05  O-ACCT                    PIC X(10)     VALUE SPACES.    00017100
017200     05  O-CERT-NO                 PIC X(10)     VALUE SPACES.    00017200
017300     05  O-MISC                    PIC X(128)    VALUE SPACES.    00017300
017400     05  O-ADJ-CANCEL  REDEFINES  O-MISC.                         00017400
017500         10  O-ADJ-CAN-DATE.                                      00017500
017600             15  O-ADJ-CAN-MO    PIC X(02).                       00017600
017700             15  O-ADJ-CAN-DA    PIC X(02).                       00017700
017800             15  O-ADJ-CAN-YR    PIC X(02).                       00017800
017900         10  O-ADJ-LIFE-AMT      PIC 9(05)V99.                    00017900
018000         10  O-ADJ-ACC-AMT       PIC 9(05)V99.                    00018000
018100         10  O-ADJ-EFF-DATE.                                      00018100
018200             15  O-ADJ-EFF-MO    PIC X(02).                       00018200
018300             15  O-ADJ-EFF-DA    PIC X(02).                       00018300
018400             15  O-ADJ-EFF-YR    PIC X(02).                       00018400
018500         10  FILLER                PIC X(102).                    00018500
018600                                                                  00018600
018700                                                                  00018700
018800 01  WORK-ACCT.                                                   00018800
018900     05 WORK-ACCT-1ST-5     PIC X(05)     VALUE SPACES.           00018900
019000     05 WORK-ACCT-L5        PIC X(05)     VALUE SPACES.           00019000
019100                                                                  00019100
019200 01  WK-DATE.                                                     00019200
019300     05  WK-MO               PIC XX     VALUE SPACES.             00019300
019400     05  FILLER              PIC X      VALUE SPACES.             00019400
019500     05  WK-DA               PIC XX     VALUE SPACES.             00019500
019600     05  FILLER              PIC X      VALUE SPACES.             00019600
019700     05  WK-YR               PIC XX     VALUE SPACES.             00019700
019800                                                                  00019800
019900 01  WORK-DATE.                                                   00019900
020000     12  WORK-DATE-X.                                             00020000
020100         15  WORK-MO-X       PIC XX.                              00020100
020200         15  WORK-DA-X       PIC XX.                              00020200
020300         15  WORK-YR-X       PIC XX.                              00020300
020400     12  WORK-DATE-N    REDEFINES   WORK-DATE-X.                  00020400
020500         15  WORK-MO-N       PIC 99.                              00020500
020600         15  WORK-DA-N       PIC 99.                              00020600
020700         15  WORK-YR-N       PIC 99.                              00020700
020800                                                                  00020800
020400     12  WORK-DATE-IN.                                            00020400
020700         15  WORK-YR-IN      PIC 99.                              00020700
020500         15  WORK-MO-IN      PIC 99.                              00020500
020600         15  WORK-DA-IN      PIC 99.                              00020600
020800                                                                  00020800
020900 SKIP3                                                            00020900
021000                                                                  00021000
021100******************************************************************00021100
021200 SKIP3                                                            00021200
021300 PROCEDURE DIVISION.                                              00021300
021400 SKIP3                                                            00021400
021500                                                                  00021500
021600* *  START INPUT-ROUTINE PROCESSING.                              00021600
021700                                                                  00021700
021800     ACCEPT WORK-DATE-IN  FROM  DATE.                             00021800
021900                                                                  00021900
021800     MOVE   WORK-YR-IN  TO  WORK-YR-N.                            00021800
021800     MOVE   WORK-MO-IN  TO  WORK-MO-N.                            00021800
021800     MOVE   WORK-DA-IN  TO  WORK-DA-N.                            00021800
021900                                                                  00021900
022000     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00022000
022100     DISPLAY '*   CURRENT DATE IS -- ' WK-DATE.                   00022100
022200     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00022200
022300                                                                  00022300
022400 PROCESS-CONV-FILE.                                               00022400
022500                                                                  00022500
022600     MOVE ZEROS TO C-SUB.                                         00022600
022700                                                                  00022700
022800     OPEN INPUT    CID-CONV-FILE.                                 00022800
022900                                                                  00022900
023000 READ-CONV-FILE.                                                  00023000
023100                                                                  00023100
023200      READ         CID-CONV-FILE                                  00023200
023300         AT END                                                   00023300
023400             DISPLAY '   '                                        00023400
023500             DISPLAY 'CONV FILE AT END - RCD COUNT = ' C-SUB      00023500
023600             DISPLAY '   '                                        00023600
023700             CLOSE CID-CONV-FILE                                  00023700
023800             GO TO CONT-NORMAL-PROCESSING.                        00023800
023900                                                                  00023900
024000      ADD  1  TO C-SUB.                                           00024000
024100                                                                  00024100
024200      IF   C-SUB  >  400                                          00024200
024300           DISPLAY '   '                                          00024300
024400           DISPLAY 'CONV FILE HAS MORE THAN 400 RCDS '            00024400
024400           DISPLAY 'CONV FILE TABLE MUST BE INCREASED'            00024410
024500           DISPLAY 'CONV FILE RECORD COUNT IS - ' C-SUB           00024500
024600           DISPLAY '   '                                          00024600
024700           GO  TO  READ-CONV-FILE.                                00024700
024800                                                                  00024800
024900      MOVE CONV-RECORD  TO  C-REC (C-SUB).                        00024900
025000                                                                  00025000
025100      GO  TO  READ-CONV-FILE.                                     00025100
025200                                                                  00025200
025300 CONT-NORMAL-PROCESSING.                                          00025300
025400                                                                  00025400
025500     OPEN  INPUT     UMB-LIFE-IN.                                 00025500
025600                                                                  00025600
025700     OPEN  OUTPUT    UMB-LIFE-OUT.                                00025700
025800                                                                  00025800
025900     MOVE  WK-MO     TO   WORK-MO-X.                              00025900
026000     MOVE  WK-YR     TO   WORK-YR-X.                              00026000
026100     MOVE  WK-DA     TO   WORK-DA-X.                              00026100
026200                                                                  00026200
026300     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00026300
026400     DISPLAY '*   WORK-DATE-X  IS -- ' WORK-DATE-X.               00026400
026500     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00026500
026600                                                                  00026600
026700 010-READ-INPUT-FILE.                                             00026700
026800                                                                  00026800
026900*    DISPLAY ' '.                                                 00026900
027000*    DISPLAY '  010-READ-INPUT ENTERED '.                         00027000
027100                                                                  00027100
027200      READ UMB-LIFE-IN                                            00027200
027300         INTO  UMB-IN-REC                                         00027300
027400             AT END                                               00027400
027500              DISPLAY ' "READ" AT END INPUT COUNT  = ' IN-CNT     00027500
027600              DISPLAY '"WRITE" AT END OUTPUT COUNT = ' WRT-CNT    00027600
027700                GO TO END-OF-JOB.                                 00027700
027800                                                                  00027800
027900*    DISPLAY ' '.                                                 00027900
028000*    DISPLAY 'UMB-IN-REC (ORIG) ' UMB-IN-REC.                     00028000
028100                                                                  00028100
TSTMOD      IF  UMB-BANK-NO  = '06' OR '08' OR '31' OR '53' OR '56'     00028200
TSTMOD          MOVE '30' TO UMB-BANK-NO.                               00028300
028400**           DISPLAY ' '                                          00028400
028500**              DISPLAY 'UMB-IN-REC (CHGD) ' UMB-IN-REC.          00028500
028600                                                                  00028600
028700                                                                  00028700
028800      ADD  1  TO IN-CNT.                                          00028800
028900                                                                  00028900
029000 GET-RECORDS.                                                     00029000
029100                                                                  00029100
029200*    DISPLAY 'GET-RECORDS  ENTERED '.                             00029200
029300                                                                  00029300
029400*  CK-TYPE-30.                                                    00029400
029500                                                                  00029500
029600     IF  UMB-TRANS      = 30                                      00029600
029700*      DISPLAY 'REC TYPE 30 FOUND'                                00029700
029800         GO  TO  CANCEL-BLD.                                      00029800
029900                                                                  00029900
030000*  CK-TYPE-20.                                                    00030000
030100                                                                  00030100
030200     IF  UMB-TRANS      = 20                                      00030200
030300*        DISPLAY 'REC TYPE 20 FOUND'                              00030300
030400         GO TO PROCESS-ISSUES.                                    00030400
030500                                                                  00030500
030600 REC-TYPE-ERROR.                                                  00030600
030700                                                                  00030700
030800     DISPLAY ' '.                                                 00030800
030900     DISPLAY 'REC-TYPE-ERROR  ENTERED'.                           00030900
031000                                                                  00031000
031100     ADD     1   TO  RPT-CNT.                                     00031100
031200                                                                  00031200
031300     DISPLAY '******************************************'         00031300
031400     DISPLAY ' '                                                  00031400
031500     DISPLAY 'ERROR RECORD - RECORD IS BYPASSED '                 00031500
031600     DISPLAY ' '                                                  00031600
031700     DISPLAY 'INPUT RECORD NUMBER IS '  IN-CNT.                   00031700
031800     DISPLAY ' '                                                  00031800
031900     DISPLAY 'UMB-TRANS = ' UMB-TRANS.                            00031900
032000     DISPLAY ' '                                                  00032000
032100     DISPLAY 'UMB-LIFE-RECORD     = ' UMB-LIFE-RECORD             00032100
032200     DISPLAY ' '                                                  00032200
032300     DISPLAY '******************************************'         00032300
032400                                                                  00032400
032500     GO TO 010-READ-INPUT-FILE.                                   00032500
032600                                                                  00032600
032700 PROCESS-ISSUES.                                                  00032700
032800                                                                  00032800
032900*    DISPLAY 'PROCESS-ISSUES       ENTERED '.                     00032900
033000                                                                  00033000
033100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00033100
033200* * * * * * * *      START ISSUE PROCESSING     * * * * * * * *   00033200
033300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00033300
033400                                                                  00033400
033500*                ICARD                                            00033500
033600                                                                  00033600
033700*    DISPLAY 'ISSUE-BLD           ENTERED  '.                     00033700
033800                                                                  00033800
033900*    DISPLAY ' '.                                                 00033900
034000*    DISPLAY 'UMB-IN-REC (ISS ) ' UMB-IN-REC.                     00034000
034100                                                                  00034100
034200* * * * * * *    BUILD THE CSO INPUT RECORDS   * * * * * * * *.   00034200
034300                                                                  00034300
034400      IF    UMB-LIFE-PREM  NOT  NUMERIC                           00034400
034500            MOVE  ZEROS  TO  UMB-LIFE-PREM.                       00034500
034600                                                                  00034600
034700      IF    UMB-ACC-PREM  NOT  NUMERIC                            00034700
034800            MOVE  ZEROS  TO  UMB-ACC-PREM.                        00034800
034900                                                                  00034900
035000     ADD    1                  TO  TOT-CERT-ISS.                  00035000
035100                                                                  00035100
035200     MOVE   UMB-TRANS          TO  O-TRANS.                       00035200
035300     MOVE   UMB-ACCT           TO  WORK-ACCT-L5.                  00035300
035400     MOVE   ZEROS              TO  WORK-ACCT-1ST-5.               00035400
035500     MOVE   WORK-ACCT          TO  O-ACCT.                        00035500
035600     MOVE   UMB-CERT-NO        TO  O-CERT-NO.                     00035600
035700     MOVE   UMB-NEW-ACCOUNT    TO  O-MISC.                        00035700
035800     ADD    UMB-LIFE-PREM      TO  TOT-LIFE-WRITTEN.              00035800
035900     ADD    UMB-ACC-PREM       TO  TOT-AH-WRITTEN.                00035900
036000                                                                  00036000
036100                                                                  00036100
036200*    DISPLAY 'WRITE ISSUE RECORD '.                               00036200
036300                                                                  00036300
036400*    DISPLAY  OUT-RECORD.                                         00036400
036500                                                                  00036500
036600     ADD  1  TO WRITE-ISS-CNT.                                    00036600
036700     ADD  1  TO WRT-CNT.                                          00036700
036800                                                                  00036800
036900     WRITE CONVERTED-RECORD    FROM OUT-RECORD.                   00036900
037000                                                                  00037000
037400     GO  TO  010-READ-INPUT-FILE.                                 00037400
037500                                                                  00037500
037600                                                                  00037600
037700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00037700
037800* * * * * * * * *   END  ISSUE   PROCESSING   * * * * * * * * *   00037800
037900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00037900
038000                                                                  00038000
038100                                                                  00038100
038200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00038200
038300* * * * * * * *   START  CANCELLATION PROCESSING  * * * * * * *   00038300
038400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00038400
038500                                                                  00038500
038600*                   ACARD                                         00038600
038700 CANCEL-BLD.                                                      00038700
038800                                                                  00038800
038900                                                                  00038900
039000*    DISPLAY 'CANCEL-BLD   ENTERED '.                             00039000
039100                                                                  00039100
039200*    DISPLAY ' '.                                                 00039200
039300*    DISPLAY 'UMB-IN-REC (CANC) ' UMB-IN-REC.                     00039300
039400                                                                  00039400
039500     ADD    1                  TO  TOT-CERT-CANC.                 00039500
039600                                                                  00039600
TSTMOD      IF    UMB-ADJ-LIFE-AMT  NOT  NUMERIC                        00039700
TSTMOD            MOVE  ZEROS  TO  UMB-ADJ-LIFE-AMT.                    00039800
TSTMOD                                                                  00039900
TSTMOD      IF    UMB-ADJ-ACC-AMT   NOT  NUMERIC                        00040000
TSTMOD            MOVE  ZEROS  TO  UMB-ADJ-ACC-AMT.                     00040100
040200                                                                  00040200
040300     MOVE   UMB-IN-REC         TO  OUT-RECORD.                    00040300
040400                                                                  00040400
040500 CK-FOR-CONVERSION.                                               00040500
040600                                                                  00040600
040700*    DISPLAY  'CK-FOR-CONVERSION ENTERED ' UMB-IN-REC.            00040700
040800                                                                  00040800
040900     MOVE ZEROS  TO C-SUB.                                        00040900
041000                                                                  00041000
041100 CONVERSION-LOOP.                                                 00041100
041200                                                                  00041200
041300     ADD  1      TO C-SUB.                                        00041300
041400                                                                  00041400
041500     IF   C-SUB   >   400                                         00041500
041600          GO TO CK-FOR-CONVERSION-EXIT.                           00041600
041700                                                                  00041700
041800     IF   CONV-UMB-LOAN-NO (C-SUB)  =  UMB-CERT-NO                00041800
041900          NEXT  SENTENCE                                          00041900
042000       ELSE                                                       00042000
042100          GO TO CONVERSION-LOOP.                                  00042100
042200                                                                  00042200
042300     ADD  1      TO TOT-CERTS-CHANGED.                            00042300
042400                                                                  00042400
042500     DISPLAY  '   '.                                              00042500
042600     DISPLAY  'RECORDS MATCH  -   UMB-IN =  ' UMB-IN-REC.         00042600
042700     DISPLAY  '               - CONV-REC =  ' C-REC (C-SUB).      00042700
042800                                                                  00042800
043700     MOVE  UMB-ADJ-CAN-DATE           TO  O-ADJ-CANCEL.           00042810
043800                                                                  00042820
042900*    MOVE   CONV-YR (C-SUB)           TO  O-ADJ-EFF-YR.           00042900
043000*    MOVE   CONV-MO (C-SUB)           TO  O-ADJ-EFF-MO            00043000
043100*    MOVE   CONV-DA (C-SUB)           TO  O-ADJ-EFF-DA.           00043100
043200                                                                  00043200
043100     MOVE   CONV-CSO-DATE (C-SUB)    TO  O-ADJ-EFF-DATE.          00043210
043200                                                                  00043220
043300     MOVE   CONV-CSO-ACCT-NO (C-SUB)  TO  O-ACCT.                 00043300
043400                                                                  00043400
043500     MOVE   CONV-CSO-CERT (C-SUB)     TO  O-CERT-NO.              00043500
040200                                                                  00043510
046500     MOVE  UMB-ADJ-LIFE-AMT           TO  O-ADJ-LIFE-AMT.         00043520
046600     MOVE  UMB-ADJ-ACC-AMT            TO  O-ADJ-ACC-AMT.          00043530
043600                                                                  00043600
043900     DISPLAY  '   '.                                              00043900
044000     DISPLAY  'CHANGED UMB OUT RECORD NOW =  ' OUT-RECORD.        00044000
044100                                                                  00044100
044200     GO TO CONT-CANCEL-REC.                                       00044200
044300                                                                  00044300
044400 CK-FOR-CONVERSION-EXIT.                                          00044400
044500                                                                  00044500
044600*    DISPLAY  '   '.                                              00044600
044700*    DISPLAY  'NO CHANGE MADE -   UMB-IN =  ' UMB-IN-REC.         00044700
044800                                                                  00044800
044900     ADD  1      TO TOT-CERTS-NOCHANGE.                           00044900
045000                                                                  00045000
045100     MOVE   UMB-ADJ-CAN-MO            TO  O-ADJ-CAN-MO.           00045100
045200     MOVE   UMB-ADJ-CAN-DA            TO  O-ADJ-CAN-DA            00045200
045300     MOVE   UMB-ADJ-CAN-YR            TO  O-ADJ-CAN-YR.           00045300
045400                                                                  00045400
045500     MOVE   UMB-TRANS                 TO  O-TRANS.                00045500
045600     MOVE   UMB-ACCT                  TO  WORK-ACCT-L5.           00045600
045700     MOVE   ZEROS                     TO  WORK-ACCT-1ST-5.        00045700
045800     MOVE   WORK-ACCT                 TO  O-ACCT.                 00045800
045900     MOVE   UMB-CERT-NO               TO  O-CERT-NO.              00045900
046000                                                                  00046000
046100     MOVE  UMB-ADJ-CANCEL             TO  O-ADJ-CANCEL.           00046100
046200                                                                  00046200
046300 CONT-CANCEL-REC.                                                 00046300
046400                                                                  00046400
046500     ADD   UMB-ADJ-LIFE-AMT           TO  TOT-LIFE-CANC.          00046500
046600     ADD   UMB-ADJ-ACC-AMT            TO  TOT-AH-CANC.            00046600
046700                                                                  00046700
046800     ADD  1  TO WRITE-CNC-CNT.                                    00046800
046900     ADD  1  TO WRT-CNT.                                          00046900
047000                                                                  00047000
047100*    DISPLAY  '   '.                                              00047100
047200*    DISPLAY  'WRITE CANCELLED RECORD '.                          00047200
047300                                                                  00047300
047400*    DISPLAY   OUT-RECORD.                                        00047400
047500                                                                  00047500
047600     WRITE CONVERTED-RECORD           FROM  OUT-RECORD.           00047600
047700                                                                  00047700
048100     GO  TO  010-READ-INPUT-FILE.                                 00048100
048200                                                                  00048200
048300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00048300
048400* * * * * * * *   END  CANCELLATION PROCESSING  * * * * * * * *   00048400
048500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00048500
048600                                                                  00048600
048700 SKIP3                                                            00048700
048800 END-OF-JOB SECTION.                                              00048800
048900                                                                  00048900
049000     DISPLAY 'END-OF-JOB       ENTERED '.                         00049000
049100                                                                  00049100
049200     DISPLAY '************************************************'   00049200
049300     DISPLAY ' '.                                                 00049300
049400     DISPLAY 'CONVERSION  RUN  TOTALS  AND  COUNTS '.             00049400
049500     DISPLAY '==================================== '.             00049500
049600     DISPLAY ' '.                                                 00049600
049700     DISPLAY 'INPUT RECORDS          -- ' IN-CNT.                 00049700
049800     DISPLAY ' '.                                                 00049800
049900     DISPLAY 'WRITE RECORDS          -- ' WRT-CNT.                00049900
050000     DISPLAY ' '.                                                 00050000
050100     DISPLAY 'ISSUE WRITE COUNT      -- ' WRITE-ISS-CNT.          00050100
050200     DISPLAY ' '.                                                 00050200
050300     DISPLAY 'CANCEL WRITE COUNT     -- ' WRITE-CNC-CNT.          00050300
050400     DISPLAY ' '.                                                 00050400
050500     DISPLAY 'ERROR RECORD COUNT     -- ' RPT-CNT.                00050500
050600     DISPLAY ' '.                                                 00050600
050700     DISPLAY 'TOTAL ISSUES           -- ' TOT-CERT-ISS.           00050700
050800     DISPLAY ' '.                                                 00050800
050900     DISPLAY 'TOTAL CANCELS          -- ' TOT-CERT-CANC.          00050900
051000     DISPLAY ' '.                                                 00051000
051100     DISPLAY 'TOTAL CERTS NOCHANGE   -- ' TOT-CERTS-NOCHANGE.     00051100
051200     DISPLAY ' '.                                                 00051200
051300     DISPLAY 'TOTAL CERTS CHANGED    -- ' TOT-CERTS-CHANGED.      00051300
051400     DISPLAY ' '.                                                 00051400
051500     DISPLAY 'TOTAL LIFE PREM        -- ' TOT-LIFE-WRITTEN.       00051500
051600     DISPLAY ' '.                                                 00051600
051700     DISPLAY 'TOTAL A&H PREM         -- ' TOT-AH-WRITTEN.         00051700
051800     DISPLAY ' '.                                                 00051800
051900     DISPLAY 'TOTAL LIFE CANCELS     -- ' TOT-LIFE-CANC.          00051900
052000     DISPLAY ' '.                                                 00052000
052100     DISPLAY 'TOTAL AH CANCELS       -- ' TOT-AH-CANC.            00052100
052200     DISPLAY ' '.                                                 00052200
052300     DISPLAY '************************************************'.  00052300
052400                                                                  00052400
052500     CLOSE   UMB-LIFE-IN                                          00052500
052600             UMB-LIFE-OUT.                                        00052600
052700                                                                  00052700
052800     GOBACK.                                                      00052800
052900                                                                  00052900
