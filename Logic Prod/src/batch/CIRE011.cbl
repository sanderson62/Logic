000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    CIRE011.                                          00000200
000400****************************************************************  00000400
000500*  SELECT PRINT LINES FROM THE ECS045 REPORTS.                 *  00000720
103002****************************************************************
103002*                   C H A N G E   L O G
103002*
103002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103002*-----------------------------------------------------------------
103002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103002* EFFECTIVE    NUMBER
103002*-----------------------------------------------------------------
022404* 031104    2000040300006  SMVA  NEW PGM TO SORT ECS045A BY REIN CO.    
103002******************************************************************
000700                                                                  00000740
000800 ENVIRONMENT DIVISION.                                            00000800
000900 INPUT-OUTPUT SECTION.                                            00000900
001000 FILE-CONTROL.                                                    00001000
001100                          
001200     SELECT ECS045-FICH-IN         ASSIGN TO SYS010.
001500     SELECT REPORT-OUT             ASSIGN TO SYS015.  
001500     SELECT SORT-FILE              ASSIGN TO SORTWK1.  
002500                                                                  00002500
002600 DATA DIVISION.                                                   00002600
002700 FILE SECTION.                                                    00002700

003600 FD  ECS045-FICH-IN                                               00003600
003800     RECORDING MODE IS F                                          00003800
004000     BLOCK CONTAINS 0 RECORDS.                                    00004000
004100 01  ECS045-FICH-IN-RECORD.                                       00004100
004200     05  CC                     PIC X(001). 
004300     05  FILLER                 PIC X(132).
004400                                                                  00004400

002900 FD  REPORT-OUT
003100     RECORDING MODE IS F                                          00003440
003300     BLOCK CONTAINS 0 RECORDS.                                    00003460
003400 01  REPORT-OUT-RECORD          PIC X(133).   
003500                                                                  00003500
002900 SD  SORT-FILE.
003400 01  SORT-RECORD.
003400     05  SORT-KEY               PIC X(054).
003400     05  SORT-RPT-LINE          PIC X(133).  
003500                                                                  00003500

005000 WORKING-STORAGE SECTION.                                         00005000
005100                                                                  00005100
005200 01  WORK.                                                        00005200
005300     05  EOF-SW                 PIC X         VALUE 'N'.
005400         88  EOF                              VALUE 'Y'.
005500     05  SEL-SW                 PIC X         VALUE SPACE. 
005700     05  S0C7            COMP-3 PIC S9(01)    VALUE ZEROES.

010200 01  WORK-RECORD.                                                 00010200
010300     05  WORK-KEY.                                                00010300
010400         10  WORK-COMPANY       PIC X(030)    VALUE SPACE.        00010400
010500         10  WORK-ACCOUNT       PIC X(010)    VALUE ZERO.         00010500
010600         10  WORK-REPORT        PIC X(007)    VALUE SPACE.        00010600
010700         10  WORK-LINE-NO       PIC 9(007)    VALUE ZERO.         00010700
010800     05  WORK-LINE              PIC X(133)    VALUE SPACE.        00010800
010900                                                                  00010900
011000 01  HDG1.                                                        00011000
LGC183     05  FILLER                 PIC X(120).                       00011100
011200     05  HDG1-REPORT            PIC X(007).                       00011200
LGC183     05  FILLER                 PIC X(006).                       00011100
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
013700                                                                  00013700
013800 PROCEDURE DIVISION.                                              00013800

       0000-MAIN.

013900     PERFORM INITIALIZATION
test       display 'right before sort'
           SORT SORT-FILE ON ASCENDING KEY SORT-KEY 
014000         INPUT  PROCEDURE PROCESS-INPUT-FILE THRU 0100-EXIT
               OUTPUT PROCEDURE RETURN-SORT THRU 0200-EXIT
test       display 'end of job'
014100     PERFORM END-OF-JOB

014200     GOBACK
014300                                                                  00014300
014400     .  

       PROCESS-INPUT-FILE.
test       display 'into process input file'
           PERFORM READ-INPUT-RPT-LINE UNTIL EOF
 
           .
       0100-EXIT.
           EXIT.

       READ-INPUT-RPT-LINE.

014600     READ ECS045-FICH-IN                                          00014600
014700         AT END
                   MOVE 'Y' TO EOF-SW
014900             GO TO READ-INPUT-RPT-LINE-X
                   DISPLAY 'end of input'
           END-READ

015000     IF CC = '1'                                                  00015000
015100         PERFORM CHECK-REPORT
           END-IF

015200     IF SEL-SW = 'Y'                                              00015200
015300         MOVE ECS045-FICH-IN-RECORD TO WORK-LINE
015400         PERFORM RELEASE-SORT
           END-IF

           .
       READ-INPUT-RPT-LINE-X.
           EXIT.

015800                                                                  00015800
015900 CHECK-REPORT. 
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
016400*                                                              *  00016390
016400****************************************************************  00016400

016600     MOVE ECS045-FICH-IN-RECORD  TO  HDG1
016700     IF HDG1-REPORT = 'ECS045A'
               MOVE 'Y' TO SEL-SW
           ELSE
               MOVE 'N' TO SEL-SW
016800         GO TO CHECK-REPORT-X
           END-IF

016900     READ ECS045-FICH-IN INTO HDG2
017000     READ ECS045-FICH-IN INTO HDG3
017100     READ ECS045-FICH-IN INTO HDG4
017200     READ ECS045-FICH-IN INTO HDG5
017300     READ ECS045-FICH-IN INTO HDG6
017400     READ ECS045-FICH-IN INTO HDG7

018200     PERFORM WRITE-HEADINGS

           . 
018300 CHECK-REPORT-X.                                                  00018300
018400     EXIT.                                                        00018400
018500                                                                  00018500
018600                                                                  00018600
022400 RELEASE-SORT.
022300                                                                  00022410
022500     ADD +1 TO WORK-LINE-NO
022300                                                                  00022510
TSTMOD     IF WORK-REPORT = 'ECS045A' OR 'ECS045A '                     00022600
               RELEASE SORT-RECORD FROM WORK-RECORD
           END-IF

           .
023400 RELEASE-SORT-X.                                                  00023400
023500     EXIT.                                                        00023500
023600                                                                  00023600

023800 WRITE-HEADINGS. 

023900     MOVE HDG3-COMPANY      TO WORK-COMPANY
           MOVE HDG7-ACCT         TO WORK-ACCOUNT
024000     MOVE HDG1-REPORT       TO WORK-REPORT
024100     MOVE HDG1              TO WORK-LINE
024200     PERFORM RELEASE-SORT  

024300     MOVE HDG2              TO WORK-LINE
024400     PERFORM RELEASE-SORT

024500     MOVE HDG3              TO WORK-LINE
024600     PERFORM RELEASE-SORT

024700     MOVE HDG4              TO WORK-LINE
024800     PERFORM RELEASE-SORT

024900     MOVE HDG5              TO WORK-LINE
025000     PERFORM RELEASE-SORT

025100     MOVE HDG6              TO WORK-LINE
025200     PERFORM RELEASE-SORT

           .
025400 WRITE-HEADINGS-X.                                                00025400
025500     EXIT.                                                        00025500
025600                                                                  00025600
       RETURN-SORT.

test       display 'into return sort '
           MOVE 'N'               TO EOF-SW
           PERFORM PRINT-RPT-LINE THRU PRINT-RPT-LINE-X
               UNTIL EOF
test       display 'about to exit 0200 output procedure'

           .
       0200-EXIT.
           EXIT.

       PRINT-RPT-LINE.
test       display 'into print rpt line'
           RETURN SORT-FILE
               AT END SET EOF TO TRUE
test                  display 'at end of return sort'
                      GO TO PRINT-RPT-LINE-X
           END-RETURN
test       display 'after return'
test       MOVE SORT-RPT-LINE         TO REPORT-OUT-RECORD    
           WRITE REPORT-OUT-RECORD 

           .
       PRINT-RPT-LINE-X.
           EXIT.
025700                                                                  00025700
025800 INITIALIZATION. 

026000     OPEN INPUT  ECS045-FICH-IN
026100          OUTPUT REPORT-OUT                                       00026100

           .
026200 INITIALIZATION-X.                                                00026200
026300     EXIT.                                                        00026300
026400                                                                  00026400
029100 END-OF-JOB. 

029200     CLOSE ECS045-FICH-IN                                         00029200
029300           REPORT-OUT 

           . 
029400 END-OF-JOB-X.                                                    00029400
029500     EXIT.                                                        00029500
029600                                                                  00029600
029700                                                                  00029700
029800 ABEND SECTION.                                                   00029800

029900     DISPLAY 'PROGRAM CIRE011 ABENDED ' 
030100     MOVE +16 TO RETURN-CODE
030200     ADD +1 TO S0C7

           .
030300 ABEND-X.                                                         00030300
030400     EXIT.                                                        00030400
