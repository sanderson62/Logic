000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    CIRE010SP.                                          00000200
000300 DATE-WRITTEN.  MAY, 1991.                                        00000300
000400****************************************************************  00000400
000500*  N O T E:                                                    *  00000500
000700*  ========                                                    *  00000501
000500*  ANY CHANGE MADE IN THIS PROGRAM MUST ALSO BE MADE IN THE    *  00000510
000500*  YEAR-END VERSION CALLED 'CIRE010Y'.                         *  00000520
000700*                                                              *  00000530
000600****************************************************************  00000600
000500*  SELECT PRINT LINES FROM THE ECS-045 REPORTS.                *  00000720
122210*
122210*  THIS VERSION USES THE ECS-045.SPEC REPORT WITH CM/YTD/ITD
103002******************************************************************
  
103002******************************************************************
103002*                   C H A N G E   L O G
103002*
103002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103002*-----------------------------------------------------------------
103002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103002* EFFECTIVE    NUMBER
103002*-----------------------------------------------------------------
103002* 103002    2002102800001  SMVA  ADD REIN PRIME COMPANY '990' FOR
103002*                                ECS045A PRINT & REMOVE 2ND SET
103002*                                LOGIC FOR ECS045A
022404* 022404    2004020300008  SMVA  PRINT TOTAL PAGES ONLY FOR 180 AND 181
062507* 062507    2007040300001  PEMA  ADD REIN COMPANIES TO 45C PRINT
092607* 092607    2007040300001  PEMA  ADD COMPANIES 460 AND 465
020608* 020608    2007040300001  PEMA  ADD COMPANIES R14-R15,21-24,19,20
030408* 030408    2007040300001  PEMA  REPLACE ABOVE WITH 514 515 540 550
      *                                                  560 AND 516
040408* 040408    2008010200008  PEMA  ADD COMPS 525-533 507 508
020209* 020209    2009010500005  PEMA  ADD 013, REMOVE 026,410,875,885,J06,
020209* 020209                          J22,J26
071609* 071609                   PEMA  ADD ACE
073109* 073109                   PEMA  ADD S02,S04,S07,S08,S12,S15&S18
122210* 122210    2010030100001  AJRA  MOVE SERV LIFE COS TO 45C.SPEC RPT
050411* 050411    2011010400001  AJRA  REMOVE S04 FROM REPORT
052312* 052312    2012050800001	 AJRA  ADD S11, S19, S21
092412* 092412    2012080100001  AJRA  REMOVE S05, S16
103002******************************************************************
000700                                                                  00000740
000800 ENVIRONMENT DIVISION.                                            00000800
000900 INPUT-OUTPUT SECTION.                                            00000900
001000 FILE-CONTROL.                                                    00001000
001100                                                                  00001100
001200     SELECT ECS045-FICHE                                          00001200
001300         ASSIGN TO SYS010.                                        00001300
001400                                                                  00001610
001500     SELECT REPORT-FILE                                           00001620
001600         ASSIGN TO SYS015.                                        00001630
LGC105                                                                  00001640

060109     SELECT DISK-DATE        ASSIGN TO SYS019.

LGC105     SELECT REPORT-FILE2                                          00001650
LGC105         ASSIGN TO SYS016.                                        00001660
001700                                                                  00001700
001800     SELECT REINSURANCE-FILE                                      00001800
001900         ASSIGN TO ERREIN                                         00001900
002000         ACCESS IS SEQUENTIAL                                     00002000
002100         ORGANIZATION IS INDEXED                                  00002100
002200         FILE STATUS IS SYS014-STATUS                             00002200
002300         RECORD KEY IS RE-CONTROL-PRIMARY.                        00002300
002400                                                                  00002400
002500                                                                  00002500
002600 DATA DIVISION.                                                   00002600
002700 FILE SECTION.                                                    00002700

060109 FD  DISK-DATE
060109                                 COPY ELCDTEFD.

LGC105 FD  REPORT-FILE2                                                 00002900
LGC105     LABEL RECORDS ARE STANDARD                                   00003000
LGC105     RECORDING MODE IS F                                          00003100
LGC105     RECORD CONTAINS 179 CHARACTERS                               00003200
LGC105     BLOCK CONTAINS 0 RECORDS.                                    00003300
122210 01  REPORT-RECORD2          PIC X(196).                          00003400
002800                                                                  00003410
002900 FD  REPORT-FILE                                                  00003420
003000     LABEL RECORDS ARE STANDARD                                   00003430
003100     RECORDING MODE IS F                                          00003440
003200     RECORD CONTAINS 179 CHARACTERS                               00003450
003300     BLOCK CONTAINS 0 RECORDS.                                    00003460
122210 01  REPORT-RECORD           PIC X(196).                          00003470
003500                                                                  00003500
003600 FD  ECS045-FICHE                                                 00003600
003700     LABEL RECORDS ARE STANDARD                                   00003700
003800     RECORDING MODE IS F                                          00003800
122210     RECORD CONTAINS 150 CHARACTERS                               00003900
004000     BLOCK CONTAINS 0 RECORDS.                                    00004000
004100 01  FICHE-RECORD.                                                00004100
004200     05  CC                  PIC X(001).                          00004200
122210     05  FILLER              PIC X(149).                          00004300
004400                                                                  00004400
004500 FD  REINSURANCE-FILE                                             00004500
004600     LABEL RECORDS ARE STANDARD.                                  00004600
004700     COPY ERCREIN.                                                00004700
004800     EJECT                                                        00004800
004900                                                                  00004900
005000 WORKING-STORAGE SECTION.                                         00005000
005100                                                                  00005100
005200 01  WORK.                                                        00005200
060109     05  PGM-SUB                 PIC S999    COMP    VALUE +015.
005300     05  EOF-SW              PIC X       VALUE 'N'.               00005300
005400         88  EOF                         VALUE 'Y'.               00005400
005500     05  SEL-SW              PIC X       VALUE SPACE.             00005500
LGC105     05  RPT2-SW             PIC X       VALUE 'N'.               00005510
005600     05  SYS014-STATUS       PIC X(2)    VALUE SPACE.             00005600
005700     05  S0C7                PIC S9      COMP-3.                  00005700
005800     05  SAVE-COMPANY-NAME   PIC X(30)   VALUE SPACE.             00005800

060109 01  WS-ABEND-FIELDS.
060109     12  WS-RETURN-CODE          PIC S9(4)    VALUE ZERO.
060109     12  WS-ZERO                 PIC S9       VALUE ZERO.
060109     12  WS-ABEND-MESSAGE        PIC X(80)    VALUE SPACES.
060109     12  WS-ABEND-FILE-STATUS    PIC XX       VALUE ZERO.

006000***************************************************************** 00006000
006100*   COMPANIES TO BE SELECTED FOR REPORT ECS045C                *  00006100
006200***************************************************************** 00006200
062607 01  REINSURANCE-TABLE-VALUES.
122210     05  FILLER PIC X(33) VALUE 'S01'.
122210     05  FILLER PIC X(33) VALUE 'S02'.
122210     05  FILLER PIC X(33) VALUE 'S03'.
050411*     05  FILLER PIC X(33) VALUE 'S04'.
092412*     05  FILLER PIC X(33) VALUE 'S05'.
122210     05  FILLER PIC X(33) VALUE 'S07'.
122210     05  FILLER PIC X(33) VALUE 'S08'.
122210     05  FILLER PIC X(33) VALUE 'S09'.
122210     05  FILLER PIC X(33) VALUE 'S10'.
052312     05  FILLER PIC X(33) VALUE 'S11'.
122210     05  FILLER PIC X(33) VALUE 'S12'.
122210     05  FILLER PIC X(33) VALUE 'S13'.
122210     05  FILLER PIC X(33) VALUE 'S14'.
122210     05  FILLER PIC X(33) VALUE 'S15'.
092412*     05  FILLER PIC X(33) VALUE 'S16'.
122210     05  FILLER PIC X(33) VALUE 'S17'.
122210     05  FILLER PIC X(33) VALUE 'S18'.
052312     05  FILLER PIC X(33) VALUE 'S19'.
052312     05  FILLER PIC X(33) VALUE 'S21'.
062607     05  EOT    PIC X(33) VALUE HIGH-VALUES.
062607 01  FILLER REDEFINES REINSURANCE-TABLE-VALUES.
092412     05  REINSURANCE-TABLE OCCURS 17 TIMES INDEXED BY I.
062607         10  RET-COMP           PIC X(03).
062607         10  RET-NAME           PIC X(30).

022404 01  WS-TOTAL-HEADINGS.
022404     05  WS-TOTAL-HEADING-1.
022404         10  FILLER             PIC X(32)     VALUE
022404             'E GROUP-A CARRIER  GROUP  STATE*'.
022404         10  FILLER             PIC X(25)     VALUE SPACES.
022404
022404     05  WS-TOTAL-HEADING-2.
022404         10  FILLER             PIC X(25)     VALUE
022404             'E GROUP-A CARRIER  GROUP*'.
022404         10  FILLER             PIC X(32)     VALUE SPACES.
022404
022404     05  WS-TOTAL-HEADING-3.
022404         10  FILLER             PIC X(18)     VALUE
022404             'E GROUP-A CARRIER*'.
022404         10  FILLER             PIC X(39)     VALUE SPACES.
022404
022404     
010200 01  WORK-RECORD.                                                 00010200
010300     05  WORK-KEY.                                                00010300
010400         10  WORK-COMPANY       PIC X(030)    VALUE SPACE.        00010400
010500         10  WORK-COPY          PIC 9(001)    VALUE ZERO.         00010500
010600         10  WORK-REPORT        PIC X(007)    VALUE SPACE.        00010600
010700         10  WORK-LINE-NO       PIC 9(007)    VALUE ZERO.         00010700
122210     05  WORK-LINE              PIC X(150)    VALUE SPACE.        00010800
010900                                                                  00010900
011000 01  HDG1.                                                        00011000
LGC183*    05  FILLER                 PIC X(125).                       00011100
LGC183     05  FILLER                 PIC X(120).                       00011100
011200     05  HDG1-REPORT            PIC X(007).                       00011200
122210     05  FILLER                 PIC X(023).                       00011100
010900                                                                  00010900
122210 01  HDG2                       PIC X(150).                       00011300
010900                                                                  00010900
011400 01  HDG3.                                                        00011400
011500     10  FILLER                 PIC X(001).                       00011500
011600     10  HDG3-MSG               PIC X(015).                       00011600
011700     10  FILLER                 PIC X(008).                       00011700
011800     10  HDG3-COMPANY           PIC X(030).                       00011800
122210     10  FILLER                 PIC X(096).                       00011900
012000 01  HDG4.                                                        00012000
012100     10  FILLER                 PIC X(021).                       00012100
012200     10  HDG4-MSG               PIC X(016).                       00012200
LGC142     10  FILLER                 PIC X(026).                       00012300
LGC142     10  HDG4-MSG2              PIC X(001).                       00012301
122210     10  FILLER                 PIC X(086).                       00012310
122210 01  HDG5                       PIC X(150).                       00012400
012500 01  HDG6.                                                        00012500
012600     10  FILLER                 PIC X(002).                       00012600
012700     10  HDG6-PRIME             PIC X(003).                       00012700
022404     10  HDG6-TOTAL-HEADING     PIC X(057).                       00012701
LGC142     10  HDG6-MSG2              PIC X(001).                       00012710
122210     10  FILLER                 PIC X(087).                       00012800
012900 01  HDG7.                                                        00012900
013000     10  FILLER                 PIC X(002).                       00013000
013100     10  HDG7-PRIME             PIC X(003).                       00013100
013200     10  FILLER                 PIC X(032).                       00013200
013300     10  HDG7-ACCT              PIC X(010).                       00013300
122210     10  FILLER                 PIC X(103).                       00013400

060109                                 COPY ELCDTECX.
060109                                 COPY ELCDTEVR.

013800 PROCEDURE DIVISION.                                              00013800

060109                                 COPY ELCDTERX.

013900     PERFORM INITIALIZATION.                                      00013900
014000     PERFORM PROCESS-FILE UNTIL EOF.                              00014000
014100     PERFORM END-OF-JOB.                                          00014100
014200     STOP RUN.                                                    00014200
014300                                                                  00014300
014400                                                                  00014400
014500 PROCESS-FILE SECTION.                                            00014500
014600     READ ECS045-FICHE                                            00014600
014700         AT END MOVE 'Y' TO EOF-SW.                               00014700
014800     IF EOF                                                       00014800
014900         GO TO PROCESS-FILE-X.                                    00014900
015000     IF CC = '1'                                                  00015000
015100         PERFORM CHECK-REPORT.                                    00015100
015200     IF SEL-SW = 'Y'                                              00015200
015300         MOVE FICHE-RECORD TO WORK-LINE                           00015300
015400         PERFORM WRITE-REPORT.                                    00015400
015500 PROCESS-FILE-X.                                                  00015500
015600     EXIT.                                                        00015600
015700                                                                  00015700
015800                                                                  00015800
015900 CHECK-REPORT SECTION.                                            00015900
016000****************************************************************  00016000
016100*  THIS ROUTINE CHECKS THE HEADING LINES ON EACH PAGE. IT      *  00016100
016200*  SETS SEL-SW TO "Y" IF THE PAGE IS TO BE PRINTED.  IT SETS   *  00016200
016300*  SEL-SW TO "N" IF THE PAGE IN NOT TO BE PRINTED.             *  00016300
016400*                                                              *  00016310
016400*  N O T E                                                     *  00016320
016400*  -------                                                     *  00016330
016400*  IF LOGIC CHANGES THE LAYOUT OF THE REPORT HEADINGS, THIS    *  00016340
016400*    PROGRAM WILL NO LONGER WORK.                              *  00016350
016400*  THE LAYOUT MUST REMAIN AS IT IS NOW!!!                      *  00016360
016400*             ==== ====== == == == ===                         *  00016370
016400*  IF CHANGES ARE MADE, PROGRAM CHANGES WILL BE NEEDED ALSO.   *  00016380
016400*                                                              *  00016390
016400****************************************************************  00016400
016400*                                                                 00016410
016500* NOTE - DEFAULT TO "NO"                                          00016500
016400*                                                                 00016410
016500     MOVE 'N' TO SEL-SW.                                          00016500
LGC105     MOVE 'N' TO RPT2-SW.                                         00016510
016600     MOVE FICHE-RECORD  TO  HDG1.                                 00016600
016700     IF HDG1-REPORT NOT = ('ECS045A' AND 'ECS045C')               00016700
016800         GO TO CHECK-REPORT-X.                                    00016800
016900     READ ECS045-FICHE INTO HDG2.                                 00016900
017000     READ ECS045-FICHE INTO HDG3.                                 00017000
017100     READ ECS045-FICHE INTO HDG4.                                 00017100
017200     READ ECS045-FICHE INTO HDG5.                                 00017200
017300     READ ECS045-FICHE INTO HDG6.                                 00017300
017400     READ ECS045-FICHE INTO HDG7.                                 00017400
017500     IF HDG3-COMPANY NOT = SPACES                                 00017500
017600         MOVE HDG3-COMPANY TO SAVE-COMPANY-NAME.                  00017600
017700     IF HDG1-REPORT = 'ECS045A'                                   00017700
017800         PERFORM CHECK-045A.                                      00017800
017900     IF HDG1-REPORT = 'ECS045C'                                   00017900
018000         PERFORM CHECK-045C.                                      00018000
018100     IF SEL-SW = 'Y'                                              00018100
018200         PERFORM WRITE-HEADINGS.                                  00018200
018300 CHECK-REPORT-X.                                                  00018300
018400     EXIT.                                                        00018400
018500                                                                  00018500
018600                                                                  00018600
018700 CHECK-045A SECTION.                                              00018700
018800                                                                  00018800
TSTMOD                                                                  00018800
TSTMOD*    DISPLAY ' '.                                                 00018800
TSTMOD*    DISPLAY 'CHECK-045A ENTERED'.                                00018800
TSTMOD*    IF HDG7-PRIME = '885'                                        00019500
TSTMOD*    DISPLAY 'HDG7-PRIME = 885'.                                  00018800
TSTMOD                                                                  00018800
TSTMOD                                                                  00018800
TSTMOD                                                                  00018800
018900     IF (HDG6-PRIME = '122' OR '124'                              00018900
018900                   OR '155')                                      00019000
019100         AND  (HDG3-MSG = 'REIN COMP TOTAL')                      00019100
019200             MOVE 'Y' TO SEL-SW                                   00019200
019300             GO TO CHECK-045A-X.                                  00019300
019400                                                                  00019400
019500     IF HDG7-PRIME = '031' OR '035'                               00019500
TSTMOD                  OR '051' OR '052'                               00019510
TSTMOD                  OR '075' OR '077'                               00019600
TSTMOD                  OR '089'                                        00019601
TSTMOD                  OR '160' OR '161' OR '162'                      00019604
TSTMOD                  OR '310' OR '350'                               00019620
019700                  OR '850' OR '865' OR '875'                      00019710
LGC183                  OR '885'                                        00019601
103002                  OR '990'
019800         MOVE 'Y' TO SEL-SW.                                      00019800
019900                                                                  00019900
022404     IF HDG7-PRIME = '180' OR '181'
022404         IF HDG6-MSG2 = '*'
022404            OR HDG6-TOTAL-HEADING = WS-TOTAL-HEADING-1
022404            OR HDG6-TOTAL-HEADING = WS-TOTAL-HEADING-2
022404            OR HDG6-TOTAL-HEADING = WS-TOTAL-HEADING-3
022404             MOVE 'Y' TO SEL-SW
022404         END-IF
022404     END-IF.

LGC105     MOVE 'N' TO RPT2-SW. 
TSTMOD                                                                  00018800
LGC142                                                                  00019990
LGC142     IF  HDG7-PRIME = '150'                                       00020000
LGC142       IF  HDG7-ACCT = '0000012100'                               00020102
LGC142         IF  HDG6-MSG2 = '*'                                      00020103
LGC142           MOVE 'Y' TO SEL-SW.                                    00020200
019900                                                                  00020210
020000     IF HDG7-PRIME = '150'                                        00020220
020100         AND HDG7-ACCT = '0000074090'                             00020230
020200             MOVE 'Y' TO SEL-SW.                                  00020240
020300                                                                  00020300
020400     IF HDG7-PRIME = '150'                                        00020400
020500         AND HDG7-ACCT = '0000016710'                             00020500
020600             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
LGC187     IF HDG7-PRIME = '150'                                        00020400
LGC187         AND HDG7-ACCT = '0000015440'                             00020500
LGC187             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
LGC187     IF HDG7-PRIME = '150'                                        00020400
LGC187         AND HDG7-ACCT = '0000015441'                             00020500
LGC187             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
LGC187     IF HDG7-PRIME = '150'                                        00020400
LGC187         AND HDG7-ACCT = '0000015443'                             00020500
LGC187             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
LGC187     IF HDG7-PRIME = '150'                                        00020400
LGC187         AND HDG7-ACCT = '0000015444'                             00020500
LGC187             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
LGC187     IF HDG7-PRIME = '150'                                        00020400
LGC187         AND HDG7-ACCT = '0000015445'                             00020500
LGC187             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
LGC187     IF HDG7-PRIME = '150'                                        00020400
LGC187         AND HDG7-ACCT = '0000015446'                             00020500
LGC187             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
LGC187     IF HDG7-PRIME = '150'                                        00020400
LGC187         AND HDG7-ACCT = '0000015447'                             00020500
LGC187             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
LGC187     IF HDG7-PRIME = '150'                                        00020400
LGC187         AND HDG7-ACCT = '0000015448'                             00020500
LGC187             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
LGC187     IF HDG7-PRIME = '150'                                        00020400
LGC187         AND HDG7-ACCT = '0000017280'                             00020500
LGC187             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
LGC187     IF HDG7-PRIME = '150'                                        00020400
LGC187         AND HDG7-ACCT = '0000017290'                             00020500
LGC187             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
LGC187     IF HDG7-PRIME = '150'                                        00020400
LGC187         AND HDG7-ACCT = '0000418900'                             00020500
LGC187             MOVE 'Y' TO SEL-SW.                                  00020600
LGC187                                                                  00020300
020300                                                                  00020650
020700 CHECK-045A-X.                                                    00020700
020800     EXIT.                                                        00020800
020900                                                                  00020900
021000                                                                  00021000
021100 CHECK-045C SECTION.                                              00021100
062607     MOVE 'N'                    TO RPT2-SW

021200     SET I TO +1.                                                 00021200
021300     SEARCH REINSURANCE-TABLE                                     00021300
021400         WHEN RET-NAME (I) = HDG3-COMPANY NEXT SENTENCE           00021400
021500         WHEN RET-NAME (I) = HIGH-VALUE
                  DISPLAY ' COMP NOT FOUND ' HDG3-COMPANY
                  GO TO CHECK-045C-X.

062607     MOVE 'Y'                    TO RPT2-SW
LGC105*    IF RET-COMP (I) = '035'
LGC105*                   OR '051'                                      00021711
LGC105*                   OR '075'
LGC105*                   OR '310'                                      00021715
LGC153*                   OR '701' OR '702' OR '703' OR '704' OR '705'  00021717
LGC122*                   OR '850'
LGC153*                   OR '875' OR '880'
LGC183*                   OR '885'                                      00021721
LGC122*                   OR '890'                                      00021721
062507*                   OR '026' OR '010' OR '011' OR '106' OR '150'
062507*                   OR '122' OR '124' OR '180' OR '181'
062507*                   OR '800' OR '801' OR '020'
LGC105*            MOVE 'Y' TO RPT2-SW                                  00021722
LGC105*         ELSE                                                    00021723
LGC105*            MOVE 'N' TO RPT2-SW.                                 00021724
021600*    IF RET-COMP (I) = '106'                                      00021730
021700*        MOVE 'Y' TO SEL-SW.                                      00021740
062607     IF HDG4-MSG = '* NET OVER-ALL *'
062607        MOVE 'Y'                 TO SEL-SW
062607     END-IF

           .
022000 CHECK-045C-X.                                                    00022000
022100     EXIT.                                                        00022100
022200                                                                  00022200
022300                                                                  00022300
022400 WRITE-REPORT SECTION.                                            00022400
022300                                                                  00022410
022500     ADD 1 TO WORK-LINE-NO.                                       00022500
022300                                                                  00022510
TSTMOD     IF WORK-REPORT = 'ECS045A' OR 'ECS045A '                     00022600
022700      MOVE 1 TO WORK-COPY                                         00022601
103002*       PERFORM WRITE-SECOND-SET THRU                             00022610
103002*       WRITE-SECOND-SET-EXIT                                     00022620
022800         WRITE REPORT-RECORD FROM WORK-RECORD.                    00022800
TSTMOD     IF WORK-REPORT = 'ECS045C' OR 'ECS045C '                     00022900
023000      MOVE 1 TO WORK-COPY                                         00022901
LGC105        PERFORM WRITE-SECOND-SET THRU                             00022910
LGC105        WRITE-SECOND-SET-EXIT                                     00022920
023100         WRITE REPORT-RECORD FROM WORK-RECORD                     00023100
023200         MOVE 2 TO WORK-COPY                                      00023200
023300         WRITE REPORT-RECORD FROM WORK-RECORD.                    00023300
023400 WRITE-REPORT-X.                                                  00023400
023500     EXIT.                                                        00023500
023600                                                                  00023600
LGC105                                                                  00023601
LGC105 WRITE-SECOND-SET SECTION.                                        00023602
LGC105                                                                  00023603
LGC105     IF  RPT2-SW  =  'Y'                                          00023610
LGC105         WRITE REPORT-RECORD2 FROM WORK-RECORD.                   00023620
LGC105                                                                  00023621
LGC105 WRITE-SECOND-SET-EXIT.                                           00023630
LGC105     EXIT.                                                        00023640
023700                                                                  00023700
023800 WRITE-HEADINGS SECTION.                                          00023800
023900     MOVE SAVE-COMPANY-NAME TO WORK-COMPANY.                      00023900
024000     MOVE HDG1-REPORT       TO WORK-REPORT.                       00024000
024100     MOVE HDG1        TO WORK-LINE.                               00024100
024200     PERFORM WRITE-REPORT.                                        00024200
024300     MOVE HDG2        TO WORK-LINE.                               00024300
024400     PERFORM WRITE-REPORT.                                        00024400
024500     MOVE HDG3        TO WORK-LINE.                               00024500
024600     PERFORM WRITE-REPORT.                                        00024600
024700     MOVE HDG4        TO WORK-LINE.                               00024700
024800     PERFORM WRITE-REPORT.                                        00024800
024900     MOVE HDG5        TO WORK-LINE.                               00024900
025000     PERFORM WRITE-REPORT.                                        00025000
025100     MOVE HDG6        TO WORK-LINE.                               00025100
025200     PERFORM WRITE-REPORT.                                        00025200
025300*  NOTE HDG7 IS STILL IN THE INPUT BUFFER                         00025300
025400 WRITE-HEADINGS-X.                                                00025400
025500     EXIT.                                                        00025500
025600                                                                  00025600
025700                                                                  00025700
025800 INITIALIZATION SECTION.                                          00025800
025900     PERFORM LOAD-REIN-TABLE.                                     00025900
026000     OPEN INPUT  ECS045-FICHE.                                    00026000
026100     OPEN OUTPUT REPORT-FILE                                      00026100
LGC105                 REPORT-FILE2.                                    00026110
026200 INITIALIZATION-X.                                                00026200
026300     EXIT.                                                        00026300
026400                                                                  00026400
026500                                                                  00026500
026600 LOAD-REIN-TABLE SECTION.                                         00026600
026700     OPEN INPUT REINSURANCE-FILE.                                 00026700
026800     IF SYS014-STATUS NOT = ('00' AND '97')                       00026800
026900         DISPLAY 'OPEN ERROR ON SYS014. RC=' SYS014-STATUS        00026900
060109         PERFORM ABEND-PGM.                                       00027000
027100 LOAD-REIN-TABLE-010.                                             00027100
027200     READ REINSURANCE-FILE NEXT.                                  00027200
027300     IF (SYS014-STATUS = '10')
              OR (RE-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
027400         GO TO LOAD-REIN-TABLE-090.                               00027400
027500     IF SYS014-STATUS NOT = ZERO                                  00027500
027600         DISPLAY 'READ ERROR ON SYS014. RC=' SYS014-STATUS        00027600
060109         PERFORM ABEND-PGM.
027800     IF NOT RE-COMPANY-RECORD                                     00027800
027900         GO TO LOAD-REIN-TABLE-010.                               00027900
028000     SET I TO +1.                                                 00028000
028100     SEARCH REINSURANCE-TABLE
028200         WHEN RET-COMP (I) = RE-COMP-PRIME
028300             MOVE RE-NAME        TO RET-NAME (I)
TSTMOD*            MOVE RE-COMP-PRIME  TO RET-COMP (I)
TSTMOD             DISPLAY ' RE-COMP-PRIME = '  RE-COMP-PRIME
TSTMOD             DISPLAY '       RE-NAME = '  RE-NAME
062607     END-SEARCH
028400     GO TO LOAD-REIN-TABLE-010.                                   00028400
028500 LOAD-REIN-TABLE-090.                                             00028500
028600     CLOSE REINSURANCE-FILE.                                      00028600
028700 LOAD-REIN-TABLE-X.                                               00028700
028800     EXIT.                                                        00028800
028900                                                                  00028900
029000                                                                  00029000
029100 END-OF-JOB SECTION.                                              00029100
029200     CLOSE ECS045-FICHE                                           00029200
029300           REPORT-FILE                                            00029300
LGC105           REPORT-FILE2.                                          00029310
029400 END-OF-JOB-X.                                                    00029400
029500     EXIT.                                                        00029500
029700                                                                  00029700
060109 ABEND-PGM SECTION.              COPY ELCABEND.
