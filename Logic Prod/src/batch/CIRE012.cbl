000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    CIRE012.                                          00000200
000300 DATE-WRITTEN.  APR, 2008.                                        00000300
000400****************************************************************  00000400
000500*  SELECT PRINT LINES FROM THE ECS-045E REPORT.                *  00000720
103002**************************************************************** 
103002******************************************************************
103002*                   C H A N G E   L O G
103002*
103002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103002*-----------------------------------------------------------------
103002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103002* EFFECTIVE    NUMBER
103002*-----------------------------------------------------------------
041508* 041508    2008022000001  AJRA  REIN COMPANIES FOR 45E PRINT
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
002800                                                                  00003410
002900 FD  REPORT-FILE                                                  00003420
003000     LABEL RECORDS ARE STANDARD                                   00003430
003100     RECORDING MODE IS F                                          00003440
003200     RECORD CONTAINS 179 CHARACTERS                               00003450
003300     BLOCK CONTAINS 0 RECORDS.                                    00003460
003400 01  REPORT-RECORD           PIC X(179).                          00003470
003500                                                                  00003500
003600 FD  ECS045-FICHE                                                 00003600
003700     LABEL RECORDS ARE STANDARD                                   00003700
003800     RECORDING MODE IS F                                          00003800
003900     RECORD CONTAINS 133 CHARACTERS                               00003900
004000     BLOCK CONTAINS 0 RECORDS.                                    00004000
004100 01  FICHE-RECORD.                                                00004100
004200     05  CC                  PIC X(001).                          00004200
004300     05  FILLER              PIC X(132).                          00004300
004400                                                                  00004400
004500 FD  REINSURANCE-FILE                                             00004500
004600     LABEL RECORDS ARE STANDARD.                                  00004600
004700     COPY ERCREIN.                                                00004700
004800     EJECT                                                        00004800
004900                                                                  00004900
005000 WORKING-STORAGE SECTION.                                         00005000
005100                                                                  00005100
005200 01  WORK.                                                        00005200
005300     05  EOF-SW              PIC X       VALUE 'N'.               00005300
005400         88  EOF                         VALUE 'Y'.               00005400
005500     05  SEL-SW              PIC X       VALUE SPACE.             00005500
005600     05  SYS014-STATUS       PIC X(2)    VALUE SPACE.             00005600
005700     05  S0C7                PIC S9      COMP-3.                  00005700
005800     05  SAVE-COMPANY-NAME   PIC X(30)   VALUE SPACE.             00005800
005900                                                                  00005900
006000***************************************************************** 00006000
006100*   COMPANIES TO BE SELECTED FOR REPORT ECS045E                *  00006100
006200***************************************************************** 00006200
041508 01  REINSURANCE-TABLE-VALUES.
041508     05  FILLER PIC X(33) VALUE '106'.
041508     05  FILLER PIC X(33) VALUE '800'.
041508     05  FILLER PIC X(33) VALUE '801'.
041508     05  FILLER PIC X(33) VALUE '910'.
041508     05  FILLER PIC X(33) VALUE '911'.
041508     05  FILLER PIC X(33) VALUE '912'.
041508     05  FILLER PIC X(33) VALUE '913'.
041508     05  FILLER PIC X(33) VALUE '914'.
041508     05  FILLER PIC X(33) VALUE '915'.
041508     05  FILLER PIC X(33) VALUE '916'.
041508     05  FILLER PIC X(33) VALUE '917'.
041508     05  FILLER PIC X(33) VALUE '918'.
041508     05  FILLER PIC X(33) VALUE '919'.
041508     05  EOT    PIC X(33) VALUE HIGH-VALUES.
041508
041508 01  FILLER REDEFINES REINSURANCE-TABLE-VALUES.
041508     05  REINSURANCE-TABLE OCCURS 14 TIMES INDEXED BY I.
041508         10  RET-COMP           PIC X(03).
041508         10  RET-NAME           PIC X(30).
041508     
010200 01  WORK-RECORD.                                                 00010200
010300     05  WORK-KEY.                                                00010300
010400         10  WORK-COMPANY       PIC X(030)    VALUE SPACE.        00010400
010500         10  WORK-COPY          PIC 9(001)    VALUE ZERO.         00010500
010600         10  WORK-REPORT        PIC X(007)    VALUE SPACE.        00010600
010700         10  WORK-LINE-NO       PIC 9(007)    VALUE ZERO.         00010700
010800     05  WORK-LINE              PIC X(133)    VALUE SPACE.        00010800
010900                                                                  00010900
011000 01  HDG1.                                                        00011000
011100     05  FILLER                 PIC X(120).                       00011100
011200     05  HDG1-REPORT            PIC X(007).                       00011200
011200     05  FILLER                 PIC X(006).                       00011100
010900                                                                  00010900
011300 01  HDG2                       PIC X(133).                       00011300
010900                                                                  00010900
011400 01  HDG3.                                                        00011400
011500     10  FILLER                 PIC X(001).                       00011500
011600     10  HDG3-MSG               PIC X(015).                       00011600
011700     10  FILLER                 PIC X(008).                       00011700
011800     10  HDG3-COMPANY           PIC X(030).                       00011800
011900     10  FILLER                 PIC X(079).                       00011900
012000 01  HDG4.                                                        00012000
012100     10  FILLER                 PIC X(021).                       00012100
012200     10  HDG4-MSG               PIC X(016).                       00012200
LGC142     10  FILLER                 PIC X(026).                       00012300
LGC142     10  HDG4-MSG2              PIC X(001).                       00012301
LGC142     10  FILLER                 PIC X(069).                       00012310
012400 01  HDG5                       PIC X(133).                       00012400
012500 01  HDG6.                                                        00012500
012600     10  FILLER                 PIC X(002).                       00012600
012700     10  HDG6-PRIME             PIC X(003).                       00012700
022404     10  HDG6-TOTAL-HEADING     PIC X(057).                       00012701
LGC142     10  HDG6-MSG2              PIC X(001).                       00012710
LGC142     10  FILLER                 PIC X(065).                       00012800
012900 01  HDG7.                                                        00012900
013000     10  FILLER                 PIC X(002).                       00013000
013100     10  HDG7-PRIME             PIC X(003).                       00013100
013200     10  FILLER                 PIC X(032).                       00013200
013300     10  HDG7-ACCT              PIC X(010).                       00013300
013400     10  FILLER                 PIC X(086).                       00013400
013500                                                                  00013500
013600                                                                  00013600
013700                                                                  00013700
013800 PROCEDURE DIVISION.                                              00013800
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
016600     MOVE FICHE-RECORD  TO  HDG1.                                 00016600
016700     IF HDG1-REPORT NOT = 'ECS045E'
016800         GO TO CHECK-REPORT-X.                                    00016800
016900     READ ECS045-FICHE INTO HDG2.                                 00016900
017000     READ ECS045-FICHE INTO HDG3.                                 00017000
017100     READ ECS045-FICHE INTO HDG4.                                 00017100
017200     READ ECS045-FICHE INTO HDG5.                                 00017200
017300     READ ECS045-FICHE INTO HDG6.                                 00017300
017400     READ ECS045-FICHE INTO HDG7.                                 00017400
017500     IF HDG3-COMPANY NOT = SPACES                                 00017500
017600         MOVE HDG3-COMPANY TO SAVE-COMPANY-NAME.                  00017600
017900     IF HDG1-REPORT = 'ECS045E'                                   00017900
018000         PERFORM CHECK-045E.                                      00018000
018100     IF SEL-SW = 'Y'                                              00018100
018200         PERFORM WRITE-HEADINGS.                                  00018200
018300 CHECK-REPORT-X.                                                  00018300
018400     EXIT.                                                        00018400
018500                                                                  00018500
020900                                                                  00020900
021000                                                                  00021000
021100 CHECK-045E SECTION.                                              00021100
062607     MOVE 'N'                    TO SEL-SW

021200     SET I TO +1.                                                 00021200
021300     SEARCH REINSURANCE-TABLE                                     00021300
021400         WHEN RET-COMP (I) = HDG7-PRIME NEXT SENTENCE
021500         WHEN RET-COMP (I) = HIGH-VALUE
                  DISPLAY ' COMP NOT FOUND '  HDG7-PRIME ' '
                   HDG3-COMPANY
                  GO TO CHECK-045E-X.

062607     MOVE 'Y'                    TO SEL-SW

           .
022000 CHECK-045E-X.                                                    00022000
022100     EXIT.                                                        00022100
022200                                                                  00022200
022300                                                                  00022300
022400 WRITE-REPORT SECTION.                                            00022400
022300                                                                  00022410
022500     ADD 1 TO WORK-LINE-NO.                                       00022500
022300                                                                  00022510
TSTMOD     IF WORK-REPORT = 'ECS045E'
022700      MOVE 1 TO WORK-COPY                                         00022601
022800         WRITE REPORT-RECORD FROM WORK-RECORD.                    00022800
023400 WRITE-REPORT-X.                                                  00023400
023500     EXIT.                                                        00023500
023600                                                                  00023600
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
026100     OPEN OUTPUT REPORT-FILE.                                     00026100
026200 INITIALIZATION-X.                                                00026200
026300     EXIT.                                                        00026300
026400                                                                  00026400
026500                                                                  00026500
026600 LOAD-REIN-TABLE SECTION.                                         00026600
026700     OPEN INPUT REINSURANCE-FILE.                                 00026700
026800     IF SYS014-STATUS NOT = ('00' AND '97')                       00026800
026900         DISPLAY 'OPEN ERROR ON SYS014. RC=' SYS014-STATUS        00026900
027000         GO TO ABEND.                                             00027000
027100 LOAD-REIN-TABLE-010.                                             00027100
027200     READ REINSURANCE-FILE NEXT.                                  00027200
027300     IF SYS014-STATUS = '10'                                      00027300
027400         GO TO LOAD-REIN-TABLE-090.                               00027400
027500     IF SYS014-STATUS NOT = ZERO                                  00027500
027600         DISPLAY 'READ ERROR ON SYS014. RC=' SYS014-STATUS        00027600
027700         GO TO ABEND.                                             00027700
027800     IF NOT RE-COMPANY-RECORD                                     00027800
027900         GO TO LOAD-REIN-TABLE-010.                               00027900
028000     SET I TO +1.                                                 00028000
028100     SEARCH REINSURANCE-TABLE
028200         WHEN RET-COMP (I) = RE-COMP-PRIME
028300             MOVE RE-NAME        TO RET-NAME (I)
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
029300           REPORT-FILE.                                           00029300
029400 END-OF-JOB-X.                                                    00029400
029500     EXIT.                                                        00029500
029600                                                                  00029600
029700                                                                  00029700
029800 ABEND SECTION.                                                   00029800
029900     DISPLAY                                                      00029900
029900     'PROGRAM CIRE012 INTENTIONALLY ABENDED WITH A S0C7.'.        00029910
030000     DISPLAY 'SEE ERROR MESSAGE ABOVE.'.                          00030000
030100     MOVE +16 TO RETURN-CODE.                                     00030100
030200     ADD +1 TO S0C7.                                              00030200
030300 ABEND-X.                                                         00030300
030400     EXIT.                                                        00030400
