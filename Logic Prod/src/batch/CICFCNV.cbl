000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.                 CICFCNV.                             00000200
000300                                                                  00000300
000400*AUTHOR.     CENTRAL STATES OF OMAHA.                             00000400
000500*            OMAHA, NEBR.                                         00000500
000600                                                                  00000600
000700*DATE-COMPILED.                                                   00000700
000900                                                                  00000800
001000*REMARKS.                                                         00000900
001100*        THIS PROGRAM CONVERTS THE COMMERCIAL FEDERAL ACCOUNT     00001000
001200*        NUMBERS ON THEIR TAPE FILE TO CSO ACCOUNT NUMBERS        00001100
001300*        FOR INPUT TO THE LOGIC PROCESS.                          00001200
001400*                                                                 00001300
010405* 010405        PEMA  FIX DEFAULT STATE CODES.
001400                                                                  00001400
001500 ENVIRONMENT DIVISION.                                            00001500
001600 INPUT-OUTPUT SECTION.                                            00001600
001700 FILE-CONTROL.                                                    00001700
pemuni     SELECT  COM-FED-IN      ASSIGN TO SYS010
                 organization is line sequential.
001900     SELECT  VENDREC-OUT     ASSIGN TO SYS012-UT-2400-S-SYS012.   00001900
002000*    SELECT  RPT-FILE        ASSIGN TO SYS013-UR-1403-S-SYS013.   00002000
002200                                                                  00002200
002300 SKIP3                                                            00002300
002400 DATA DIVISION.                                                   00002400
002500                                                                  00002500
002600 FILE SECTION.                                                    00002600
002700                                                                  00002700
002800******************************************************************00002800
002900**        INPUT TAPE FILE FROM COMMERCIAL FEDERAL               **00002900
003000******************************************************************00003000
003100                                                                  00003100
003200 FD  COM-FED-IN                                                   00003200
003400     RECORDING MODE IS F                                          00003300
003500     LABEL RECORDS ARE STANDARD                                   00003400
003600     RECORD CONTAINS 800 CHARACTERS                               00003500
003700     BLOCK CONTAINS 0 RECORDS                                     00003600
003800     DATA RECORD IS COM-FED-REC.                                  00003700
003900                                                                  00003800
004000 01  COM-FED-REC             PIC X(800).                          00003900
004100                                                                  00004000
004200******************************************************************00004100
004300**       OUTPUT FILE FOR INPUT TO PROGRAM 'CICF511'             **00004200
004400******************************************************************00004300
004500                                                                  00004400
004600 FD  VENDREC-OUT                                                  00004500
004700     RECORDING MODE IS F                                          00004600
004800     LABEL RECORDS ARE STANDARD                                   00004700
004900     RECORD CONTAINS 800 CHARACTERS                               00004800
003700     BLOCK CONTAINS 0 RECORDS                                     00004900
005000     DATA RECORD IS VENDREC-REC.                                  00005000
005100                                                                  00005100
005200 01  VENDREC-REC             PIC X(800).                          00005200
005300                                                                  00005300
005400******************************************************************00005400
005500**                 OUTPUT ERROR REPORT                          **00005500
005600******************************************************************00005600
005700*                                                                 00005700
005800*FD  RPT-FILE                                                     00005800
005900*    RECORDING MODE IS F                                          00005900
006000*    LABEL RECORDS ARE STANDARD                                   00006000
006100*    RECORD CONTAINS 133 CHARACTERS                               00006100
006200*    BLOCK CONTAINS 0 RECORDS                                     00006200
006300*    DATA RECORD IS RPT-REC-OUT.                                  00006300
006400*                                                                 00006400
006500*01  RPT-REC-OUT.                                                 00006500
006700*    05  RPT-REC                  PIC X(132).                     00006700
006800*                                                                 00006800
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
008000 77  RPT-SUB                PIC 99        VALUE ZEROS.            00008300
008200 77  PRINT-SUB              PIC 99        VALUE ZEROS.            00008500
008300 77  RPT-PAGE-CNT           PIC 9999      VALUE ZEROS.            00008600
008400 77  RPT-CAN-CNT            PIC 99999     VALUE ZEROS.            00008700
008500 77  RPT-ISS-CNT            PIC 99999     VALUE ZEROS.            00008800
008600 77  RPT-LINE-CNT           PIC 99        VALUE ZEROS.            00008900
008900 77  ERROR-CNT              PIC 999999    VALUE ZEROS.            00009110
009000 77  IN-CNT                 PIC 999999    VALUE ZEROS.            00009200
009000 77  VENDREC-CNT            PIC 999999    VALUE ZEROS.            00009200
009000 77  NAME-SUB               PIC 99        VALUE ZEROS.            00009300
009000 77  SAVE-LNAME             PIC X(15)     VALUE SPACES.           00009310
009000 77  SAVE-1ST-INIT          PIC X         VALUE SPACES.           00009320
009000 77  SAVE-MIDDLE-INIT       PIC X         VALUE SPACES.           00009330
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
020000                                                                  00010510
020100 01  FILLER   PIC X(44) VALUE                                     00010511
020200      '****  COM FED RECORD  ****'.                               00010512
047300                                                                  00047300
047400 01  COM-FED-RECORD.                                              00047400
047500     05  CF-WRITTEN-PREM-IND     PIC X.                           00047500
047600     05  CF-STATE-CODE           PIC X(02).                       00047601
047700     05  CF-ACCOUNT-ID           PIC X(10).                       00047700
047800     05  CF-TRANS-TYPE           PIC X(01).                       00047800
047800     05  CF-CERT-EFF-DATE.                                        00047800
047800         10  CF-CERT-EFF-MO      PIC X(02).                       00047800
047800         10  CF-CERT-EFF-DA      PIC X(02).                       00047800
047800         10  CF-CERT-EFF-YR      PIC X(02).                       00047800
047800     05  FILLER                  PIC X(166).                      00047800
047800     05  CF-ISS-CERT-NUMBER      PIC X(10).                       00047800
047800     05  FILLER                  PIC X(30).                       00047800
047900     05  CF-FIN-INST-ID          PIC X(05).                       00047900
048000     05  FILLER                  PIC X(169).                      00050201
049500     05  FILLER                  PIC X(400).                      00050201
028800                                                                  00017600
028900 01  RPT-TOT-LINE.                                                00017700
029000     05  FILLER              PIC X(10)           VALUE SPACES.    00017800
029100     05  FILLER              PIC X(35)  VALUE                     00017900
029200         ' NUMBER OF ISSUES WRITTEN IS - '.                       00018000
029300     05  P-ISS-CNT           PIC ZZ,ZZ9.                          00018100
029400     05  FILLER              PIC X(10)           VALUE SPACES.    00018200
029500     05  FILLER              PIC X(35)  VALUE                     00018300
029600         'NUMBER OF CANCELS WRITTEN IS - '.                       00018400
029700     05  P-CAN-CNT           PIC ZZ,ZZ9.                          00018500
029800     05  FILLER              PIC X(10)           VALUE SPACES.    00018600
029900     05  FILLER              PIC X(10)           VALUE SPACES.    00018700
030000                                                                  00018800
030100 01  RPT-HD-LINE1.                                                00018900
030200     05  FILLER              PIC X(26)           VALUE SPACES.    00019000
030300     05  CF-RPT-ID           PIC X(34)  VALUE                     00019100
030400         '   COMMERCIAL FEDERAL REPORT FOR '.                     00019200
030500     05  RPT-REPORT-DATE     PIC X(14)           VALUE SPACES.    00019300
030600     05  FILLER              PIC X(16)           VALUE SPACES.    00019400
030600     05  FILLER              PIC X(15)           VALUE            00019500
030400         'PGM = CICFCNV  '.                                       00019600
030600     05  FILLER              PIC X(14)           VALUE SPACES.    00019700
030700     05  FILLER              PIC X(5)   VALUE  'PAGE '.           00019800
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
033800                                                                  00021800
033800 01  WK-CERT-FULL.                                                00021810
034700     05 WK-CERT-FILLER      PIC X(03)     VALUE '000'.            00021811
034700     05 WK-CERT             PIC X(07)     VALUE SPACES.           00021812
034700                                                                  00021820
034700 01  WK-ACCT-FULL.                                                00021810
034700     05 WK-ACCT-FILLER      PIC X(04)     VALUE '0000'.           00021811
034700     05 WK-ACCT             PIC X(06)     VALUE SPACES.           00021812
034700                                                                  00021820
034700 01  WK-AH-BEN-FULL.                                              00021810
034700     05 WK-AH-BEN-FILLER    PIC X(01)     VALUE SPACE.            00021811
034700     05 WK-AH-BEN           PIC X(02)     VALUE SPACES.           00021812
034700                                                                  00021820
034800 01  WK-FULL-NAME.                                                00021900
034800     05  FULL-NAME           PIC X      OCCURS 29 TIMES.          00021901
034800                                                                  00021902
034800 01  WK-L-NAME.                                                   00021903
034800     05  L-NAME              PIC X      OCCURS 15 TIMES.          00021904
034800                                                                  00021905
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
039500 01  WK-CERT-EFF-DATE.                                            00023500
039500     05  WK-CERT-EFF-YR      PIC XX.                              00023500
039500     05  WK-CERT-EFF-MO      PIC XX.                              00023500
039500     05  WK-CERT-EFF-DA      PIC XX.                              00023500
039500                                                                  00023500
039500 01  WK-CERT-EFF-DATE-R      REDEFINES                            00023500
039500     WK-CERT-EFF-DATE        PIC 9(06).                           00023500
039500                                                                  00023500
039500 01  WK-CERT-EFF-DATE-N      REDEFINES                            00023500
039500     WK-CERT-EFF-DATE.                                            00023500
039500     05  WK-CERT-EFF-YR-N    PIC 99.                              00023500
039500     05  WK-CERT-EFF-MO-N    PIC 99.                              00023500
039500     05  WK-CERT-EFF-DA-N    PIC 99.                              00023500
039500                                                                  00023500
039500 01  WK-BRANCH               PIC X(05).                           00023500
039500     88  LIB401        VALUE '00723' '00745' '00748' '00753'.     00023500
039500     88  LIB402        VALUE '00746' '00747' '00760'.             00023500
039500     88  LIB403        VALUE '00716' '00734'.                     00023500
039500     88  LIB404        VALUE '00724' '00755'.                     00023500
039500     88  LIB405        VALUE '00714' '00717' '00718' '00720'      00023500
039500                             '00726' '00728' '00729' '00744'      00023500
039500                             '00749' '00756' '00757' '00758'      00023500
039500                             '00759' '00761'.                     00023500
039500     88  LIB406        VALUE '00733'.                             00023500
039500     88  LIB407        VALUE '00719' '00722' '00727' '00731'      00023500
039500                             '00736' '00738' '00739' '00743'      00023500
039500                             '00754'.                             00023500
039500     88  LIB408        VALUE '00715' '00732'.                     00023500
039500     88  FIRST-FED     VALUE '00823' '00825' '00827' '00828'      00023500
039500                             '00832' '00836' '00846' '00847'      00023500
039500                             '00848' '00850' '00851' '00854'      00023500
039500                             '00858' '00859' '00861' '00862'      00023500
039500                             '00863' '00865' '00866' '00867'      00023500
039500                             '00869' '00872' '00873' '00874'.     00023500
PEMMOD     88  INTERNET-ACT  VALUE '00303' '00282' '00277'.
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
042700 01  FILLER   PIC X(24) VALUE '*** VENDREC AREA ***'.             00025400
042800                                                                  00025500
042900*01  OUTPUT-COPYBOOK          COPY VENDREC.                       00025600
042900     COPY VENDREC8.                                               00025600
043000 SKIP3                                                            00025700
043100     COPY ELCDATE.                                                00025800
043200 SKIP3                                                            00025700
054900******************************************************************00039000
055000 SKIP3                                                            00039100
055100 PROCEDURE DIVISION.                                              00039200
055200 SKIP3                                                            00039300
055300                                                                  00039500
055400* *  START INPUT-ROUTINE PROCESSING.                              00039600
055500                                                                  00039700
055600     ACCEPT  WORK-DATE-IN  FROM DATE.                             00039800
055700                                                                  00039900
055700     MOVE  WORK-YR-IN    TO  WORK-YR-X.                           00039800
055700     MOVE  WORK-MO-IN    TO  WORK-MO-X.                           00039800
055700     MOVE  WORK-DA-IN    TO  WORK-DA-X.                           00039800
055800                                                                  00039900
055900     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040000
056000     DISPLAY '*   CURRENT DATE IS -- ' WORK-DATE-X.               00040100
056100     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040200
056200                                                                  00040300
056500     OPEN INPUT    COM-FED-IN.                                    00040400
056600                                                                  00040500
056700     OPEN  OUTPUT  VENDREC-OUT.                                   00040600
056800*                     RPT-FILE.                                   00040700
056900                                                                  00040800
061400 010-READ-INPUT-FILE.                                             00047100
061800                                                                  00047800
061900      READ COM-FED-IN                                             00047900
062000         INTO  COM-FED-RECORD                                     00048000
062100             AT END                                               00048100
062100                MOVE 'Y'  TO  LAST-REC-SW                         00048200
062100                GO TO END-OF-JOB.                                 00048300
062700                                                                  00048310
062900      ADD  1  TO IN-CNT.                                          00048400
063100      MOVE 'N'  TO  LAST-REC-SW.                                  00048500
080400                                                                  00064800
080400 BUILD-VENDREC.                                                   00053800
080600                                                                  00065000
080700*    DISPLAY 'BUILD-VENDREC   -   ENTERED           '.            00065100
080900                                                                  00065200
100402     IF CF-STATE-CODE = '  ' OR '00'
100402        MOVE 'AZ'              TO CF-STATE-CODE
100402     END-IF

010405     IF CF-FIN-INST-ID = '00863' OR '00810' OR '00823'
010405        MOVE 'CO'              TO CF-STATE-CODE
010405     END-IF
010405     IF CF-FIN-INST-ID = '00011' OR '00025' OR '00009'
010405        MOVE 'NE'              TO CF-STATE-CODE
010405     END-IF
010405     IF CF-FIN-INST-ID = '00704' OR '00353'
010405        MOVE 'IA'              TO CF-STATE-CODE
010405     END-IF
064500                                                                  00133761
083400     MOVE  COM-FED-RECORD      TO  VENDOR-REC.                    00067500
064500     MOVE CF-CERT-EFF-YR       TO WK-CERT-EFF-YR.                 00133761
064500     MOVE CF-CERT-EFF-MO       TO WK-CERT-EFF-MO.                 00133761
064500     MOVE CF-CERT-EFF-DA       TO WK-CERT-EFF-DA.                 00133761
064500                                                                  00133761
064500     MOVE CF-FIN-INST-ID       TO WK-BRANCH.                      00133761
064500                                                                  00133761
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'NE'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00052'          00133762
064500         MOVE  '0000606537' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'NE'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00058'          00133762
064500         MOVE  '0000606554' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'NE'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00055'          00133762
064500         MOVE  '0000606586' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'NE'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00057'          00133762
064500         MOVE  '0000606591' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'NE'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00059'          00133762
064500         MOVE  '0000606592' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'NE'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00056'          00133762
064500         MOVE  '0000606593' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
PEMMOD     IF CF-STATE-CODE = 'NE'
PEMMOD        AND INTERNET-ACT
PEMMOD        MOVE '000015860I'   TO V-ACCOUNT-ID
PEMMOD        GO TO ACCT-NO-CHANGED
PEMMOD     END-IF
PEMMOD
064500     IF CF-STATE-CODE = 'NE'                                      00133762
064500         MOVE  '0000015860' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'CO'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 990301
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND FIRST-FED                         00133762
064500         MOVE  '0000794500' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
PEMMOD     IF CF-STATE-CODE = 'CO'
PEMMOD        AND INTERNET-ACT
PEMMOD        MOVE '000017500I'   TO V-ACCOUNT-ID
PEMMOD        GO TO ACCT-NO-CHANGED
PEMMOD     END-IF
PEMMOD
064500     IF CF-STATE-CODE = 'CO'                                      00133766
064500         MOVE  '0000017500' TO V-ACCOUNT-ID                       00133767
064500         GO TO ACCT-NO-CHANGED.                                   00133768
064500                                                                  00133769
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'KS'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00552'          00133762
064500         MOVE  '0000606556' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'KS'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00549'          00133762
064500         MOVE  '0000606566' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'KS'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00551'          00133762
064500         MOVE  '0000606581' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'KS'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00550'          00133762
064500         MOVE  '0000606583' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
PEMMOD     IF CF-STATE-CODE = 'KS'
PEMMOD        AND INTERNET-ACT
PEMMOD        MOVE '000019580I'   TO V-ACCOUNT-ID
PEMMOD        GO TO ACCT-NO-CHANGED
PEMMOD     END-IF
PEMMOD
064500     IF CF-STATE-CODE = 'KS'                                      00133770
064500         MOVE  '0000019580' TO V-ACCOUNT-ID                       00133771
064500         GO TO ACCT-NO-CHANGED.                                   00133772
064500                                                                  00133773
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 990301
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND LIB401                            00133762
064500         MOVE  '0000610401' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 990301
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND LIB402                            00133762
064500         MOVE  '0000610402' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 990301
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND LIB403                            00133762
064500         MOVE  '0000610403' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 990301
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND LIB404                            00133762
064500         MOVE  '0000610404' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 990301
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND LIB405                            00133762
064500         MOVE  '0000610405' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 990301
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND LIB406                            00133762
064500         MOVE  '0000610406' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 990301
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND LIB407                            00133762
064500         MOVE  '0000610407' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 990301
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND LIB408                            00133762
064500         MOVE  '0000610408' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00347'          00133762
064500         MOVE  '0000606501' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00349'          00133762
064500         MOVE  '0000606503' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00334'          00133762
064500         MOVE  '0000606504' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00359'          00133762
064500         MOVE  '0000606505' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00343'          00133762
064500         MOVE  '0000606506' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00338'          00133762
064500         MOVE  '0000606507' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00345'          00133762
064500         MOVE  '0000606510' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00356'          00133762
064500         MOVE  '0000606512' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00355'          00133762
064500         MOVE  '000606513A' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00357'          00133762
064500         MOVE  '0000606514' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00353'          00133762
064500         MOVE  '0000606515' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00336'          00133762
064500         MOVE  '0000606527' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00352'          00133762
064500         MOVE  '0000606528' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00337'          00133762
064500         MOVE  '0000606529' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00335'          00133762
064500         MOVE  '0000606530' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00344'          00133762
064500         MOVE  '0000606531' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00361'          00133762
064500         MOVE  '0000606535' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00354'          00133762
064500         MOVE  '0000606536' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00358'          00133762
064500         MOVE  '0000606539' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00362'          00133762
064500         MOVE  '0000606549' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00346'          00133762
064500         MOVE  '0000606557' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00339'          00133762
064500         MOVE  '0000606559' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00360'          00133762
064500         MOVE  '0000606560' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00350'          00133762
064500         MOVE  '0000606561' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'IA'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00342'          00133762
064500         MOVE  '0000606573' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
PEMMOD     IF CF-STATE-CODE = 'IA'
PEMMOD        AND INTERNET-ACT
PEMMOD        MOVE '000610400I'   TO V-ACCOUNT-ID
PEMMOD        GO TO ACCT-NO-CHANGED
PEMMOD     END-IF
PEMMOD
064500     IF CF-STATE-CODE = 'IA'                                      00133774
064500         MOVE  '0000610400' TO V-ACCOUNT-ID                       00133775
064500         GO TO ACCT-NO-CHANGED.                                   00133776
064500                                                                  00133777
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'MO'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00388'          00133762
064500         MOVE  '0000606562' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'MO'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00380'          00133762
064500         MOVE  '0000606567' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'MO'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00386'          00133762
064500         MOVE  '0000606568' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'MO'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00387'          00133762
064500         MOVE  '0000606569' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'MO'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00385'          00133762
064500         MOVE  '0000606572' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'MO'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00384'          00133762
064500         MOVE  '0000606574' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'MO'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00381'          00133762
064500         MOVE  '0000606582' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
PEMMOD     IF CF-STATE-CODE = 'MO'
PEMMOD        AND INTERNET-ACT
PEMMOD        MOVE '000610500I'   TO V-ACCOUNT-ID
PEMMOD        GO TO ACCT-NO-CHANGED
PEMMOD     END-IF
PEMMOD
064500     IF CF-STATE-CODE = 'MO'                                      00133778
064500         MOVE  '0000610500' TO V-ACCOUNT-ID                       00133779
064500         GO TO ACCT-NO-CHANGED.                                   00133780
064500                                                                  00133781
PEMMOD     IF CF-STATE-CODE = 'OK'
PEMMOD        AND INTERNET-ACT
PEMMOD        MOVE '000490700I'   TO V-ACCOUNT-ID
PEMMOD        GO TO ACCT-NO-CHANGED
PEMMOD     END-IF
PEMMOD
064500     IF CF-STATE-CODE = 'OK'                                      00133782
064500         MOVE  '0000490700' TO V-ACCOUNT-ID                       00133783
064500         GO TO ACCT-NO-CHANGED.                                   00133784
064500                                                                  00133785
PEMMOD     IF CF-STATE-CODE = 'AZ'
PEMMOD        AND INTERNET-ACT
PEMMOD        MOVE '000596100I'   TO V-ACCOUNT-ID
PEMMOD        GO TO ACCT-NO-CHANGED
PEMMOD     END-IF
PEMMOD
064500     IF CF-STATE-CODE = 'AZ'                                      00133786
064500         MOVE  '0000596100' TO V-ACCOUNT-ID                       00133787
064500         GO TO ACCT-NO-CHANGED.                                   00133788
064500                                                                  00133789
064500     IF CF-STATE-CODE = 'IL'                                      00133790
064500         MOVE  '0000596200' TO V-ACCOUNT-ID                       00133791
064500         GO TO ACCT-NO-CHANGED.                                   00133792
064500                                                                  00133793
064500     IF CF-STATE-CODE = 'IN'                                      00133794
064500         MOVE  '0000596300' TO V-ACCOUNT-ID                       00133795
064500         GO TO ACCT-NO-CHANGED.                                   00133796
064500                                                                  00133797
PEMMOD     IF CF-STATE-CODE = 'MN'
PEMMOD        AND INTERNET-ACT
PEMMOD        MOVE '000596400I'   TO V-ACCOUNT-ID
PEMMOD        GO TO ACCT-NO-CHANGED
PEMMOD     END-IF
PEMMOD
064500     IF CF-STATE-CODE = 'MN'                                      00133798
064500         MOVE  '0000596400' TO V-ACCOUNT-ID                       00133799
064500         GO TO ACCT-NO-CHANGED.                                   00133800
064500                                                                  00133801
064500     IF CF-TRANS-TYPE = '3' AND CF-STATE-CODE = 'SD'              00133762
064500                            AND WK-CERT-EFF-DATE-R < 980801
064500                            AND WK-CERT-EFF-YR-N  > 50
064500                            AND CF-FIN-INST-ID = '00390'          00133762
064500         MOVE  '0000606579' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF CF-STATE-CODE = 'SD'                                      00133802
064500         MOVE  '0000596500' TO V-ACCOUNT-ID                       00133803
064500         GO TO ACCT-NO-CHANGED.                                   00133804
064500                                                                  00133805
064500     IF CF-STATE-CODE = 'WY'                                      00133806
064500         MOVE  '0000596600' TO V-ACCOUNT-ID                       00133807
064500         GO TO ACCT-NO-CHANGED.                                   00133808
064500                                                                  00133809
064500 INVALID-STATE-CODE.                                              00133810
064000                                                                  00133811
064000     DISPLAY ' '.                                                 00133811
064000     DISPLAY 'INVALID STATE CODE - CODE  = ' CF-STATE-CODE.       00133811
064000     DISPLAY 'NO CSO ACCT NUMBER CAN BE ASSIGNED'.                00133811
064000     DISPLAY 'COMFED ACCOUNT NUMBER IS   = ' CF-ACCOUNT-ID.       00133811
064000     DISPLAY 'INSURED NAME IS            = ' V-FULL-NAME.         00133811
064000     DISPLAY 'TRANS TYPE CODE IS         = ' CF-TRANS-TYPE.       00133811
064000     DISPLAY 'WRITTEN-PREM-INDIC         = ' CF-WRITTEN-PREM-IND. 00133827
064000     DISPLAY ' '.                                                 00133811
064000                                                                  00133827
064000     ADD 1 TO ERROR-CNT.                                          00054142
064000*    GO TO 010-READ-INPUT-FILE.                                   00133748
064000                                                                  00133811
064500 ACCT-NO-CHANGED.                                                 00133810
100000                                                                  00095100
100000     WRITE VENDREC-REC FROM VENDOR-REC.                           00095700
100000     ADD 1 TO VENDREC-CNT.                                        00095800
122000                                                                  00096540
122100     GO  TO  010-READ-INPUT-FILE.                                 00096900
122900                                                                  00100500
122900 DATE-CONVERT-ROUTINE.                                            00100500
122900                                                                  00100500
122900     CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   00100500
122900                                                                  00100500
122900 DATE-CONVERT-X.                                                  00100500
122900     EXIT.                                                        00100500
167300                                                                  00119200
170400 SKIP3                                                            00127500
170500 END-OF-JOB SECTION.                                              00127600
170600                                                                  00127700
170700*    DISPLAY 'END-OF-JOB    -   ENTERED '.                        00127800
170800                                                                  00127900
171000     DISPLAY '************************************************'   00128000
171100     DISPLAY 'INPUT RECORDS      -- ' IN-CNT.                     00128100
171200     DISPLAY ' '                                                  00128200
171300     DISPLAY 'VENDRECS WRITTEN   -- ' VENDREC-CNT.                00128300
171400     DISPLAY ' '                                                  00128400
171500     DISPLAY 'ERROR COUNT        -- ' ERROR-CNT.                  00128500
171600     DISPLAY ' '                                                  00128600
171700     DISPLAY '************************************************'.  00128700
178700                                                                  00132200
178700     CLOSE COM-FED-IN                                             00053200
178700           VENDREC-OUT.                                           00053210
178700*          RPT-FILE.                                              00053300
178700                                                                  00053400
178900                                                                  00132400
179000     GOBACK.                                                      00132500
179100                                                                  00132600
