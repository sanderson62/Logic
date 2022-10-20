       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 EL595.


      *AUTHOR.     AJRA.
      ******************************************************************
      *REMARKS.
      *        THIS PROGRAM CREATES A REPORT OF CLAIM LETTERS WITH FORM
      *        CICM THAT HAVE NOT BEEN RESOLVED.
      *
      *     INPUT:  ELTRLR
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 121108    2008111900001  AJRA  INITIAL PROGRAM
      * 030409    2008111900001  AJRA  ADD CDCM LETTER
      * 032510    2010010400006  AJRA  ADD CLAIM TYPE AND TOTALS
122010* 122010    2009122800001  AJRA  ADD NAPERSOFT RESEND LETTERS      
040312* 040312  IR2012040300004  PEMA  GO TO EOJ IF NO RECORDS ON FILE
      ******************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT DISK-DATE       ASSIGN TO SYS019-FBA1-S-SYS019.

           SELECT ELTRLR-INFILE   ASSIGN TO ELTRLR
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS AT-CONTROL-PRIMARY
                                  FILE STATUS IS
                                       WS-ELTRLR-FILE-STATUS.

           SELECT ELMSTR          ASSIGN TO ELMSTR
                                  ACCESS IS DYNAMIC
                                  ORGANIZATION IS INDEXED
                                  FILE STATUS IS WS-ELMSTR-FILE-STATUS
                                  RECORD KEY IS CL-CONTROL-PRIMARY.
      

           SELECT PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.

           SELECT SORT-FILE       ASSIGN TO SYS001-UT-3380-S-SORTWK1.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE
                                COPY ELCDTEFD.

       FD  ELTRLR-INFILE.
                                COPY ELCTRLR.

       FD  ELMSTR.
                                COPY ELCMSTR.
                                
                                
       FD  PRNTR
                                COPY ELCPRTFD.

       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SORT-DAYS                   PIC 9(03).
           05  SORT-CLAIM-NO               PIC X(07).
           05  SORT-CERT-NO                PIC X(11).
           05  SORT-NAME                   PIC X(30).
           05  SORT-RECORDED-BY            PIC X(04).
           05  SORT-RECORDED-DT            PIC X(02).
           05  SORT-REASON                 PIC X(70).
032510     05  SORT-CLAIM-TYPE             PIC X(05).
122010     05  SORT-LETTER-ID              PIC X(04).
122010     05  FILLER                      PIC X(14).


       WORKING-STORAGE SECTION.

       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*     EL353  WORKING STORAGE   *'.
       77  FILLER  PIC X(32)   VALUE '********** V/M 2.008 ***********'.

       01  FILLER                          COMP-3.
           05  WS-LINE-COUNT               PIC S9(03)    VALUE +0.
           05  WS-LINE-COUNT-MAX           PIC S9(03)    VALUE +55.
           05  WS-PAGE                     PIC S9(05)    VALUE +0.
           05  WS-SORT-RTN-COUNT           PIC S9(05)    VALUE +0.

           05  WS-RETURN-CODE              PIC S9(03)    VALUE +0.
           05  WS-ZERO                     PIC S9(01)    VALUE +0.

032510     05  WS-LIFE-COUNT               PIC S9(07)    VALUE +0.
032510     05  WS-DISAB-COUNT              PIC S9(07)    VALUE +0.

       01  FILLER                          COMP SYNC.
           05  PGM-SUB                     PIC S9(04)    VALUE +353.

       01  FILLER.
           05  ABEND-CODE                  PIC X(04).
           05  ABEND-OPTION                PIC X(01).
           05  OLC-REPORT-NAME             PIC X(05)     VALUE 'EL353'.

           05  WS-CYCLE-DT-BINARY          PIC X(02)     VALUE SPACES.

           05  WS-LAST-MAINT-CUTOFF-DT-BIN PIC X(02)     VALUE SPACES.

           05  WS-CYCLE-DT.
               10  WS-CYCLE-DT-CC          PIC 9(02)     VALUE ZEROS.
               10  WS-CYCLE-DT-YY          PIC 9(02)     VALUE ZEROS.
               10  WS-CYCLE-DT-MM          PIC 9(02)     VALUE ZEROS.
               10  WS-CYCLE-DT-DD          PIC 9(02)     VALUE ZEROS.

           05  WS-EDITED-CYCLE-DT.
               10  WS-EDITED-CYCLE-DT-MM   PIC X(02)     VALUE SPACES.
               10  FILLER                  PIC X(01)     VALUE '/'.
               10  WS-EDITED-CYCLE-DT-DD   PIC X(02)     VALUE SPACES.
               10  FILLER                  PIC X(01)     VALUE '/'.
               10  WS-EDITED-CYCLE-DT-CC   PIC X(02)     VALUE SPACES.
               10  WS-EDITED-CYCLE-DT-YY   PIC X(02)     VALUE SPACES.

           05  WS-EOF1-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-ELTRLR                         VALUE 'Y'.

           05  WS-EOF2-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-SORTFILE                       VALUE 'Y'.

122010     05  WS-LETTER-ID                PIC X(04)     VALUE SPACES.
122010         88  VALID-LETTER                          VALUE
122010             'CICM','CIC2','CIC3','CDCM','CDC2','CDC3'.               

           05  WS-ELTRLR-FILE-STATUS       PIC X(02)     VALUE ZERO.
           05  WS-ELMSTR-FILE-STATUS       PIC X(02)     VALUE ZERO.

           05  WS-ELTRLR-KEY-SW            PIC X(01)     VALUE SPACE.
               88  NEW-ELTRLR-KEY                        VALUE 'N'.
               88  ELTRLR-KEY-CHANGE                     VALUE 'C'.

           05  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.

           05  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE ZERO.

           05  WS-DETAIL-REPORT-TITLE      PIC X(42)     VALUE
030409         'CICM AND CDCM LETTERS WAITING FOR RESPONSE'.

           05  WS-MTD-LITERAL              PIC X(17)     VALUE
               'MONTH-TO-DATE    '.
                   
           05  WS-NO-LETTER-MESSAGE.
               10  FILLER                  PIC X(23)    VALUE SPACES.
               10  FILLER                  PIC X(47)    VALUE
                   'THERE ARE NO CICM LETTERS WAITING FOR RESPONSE.'.
               10  FILLER                  PIC X(63)    VALUE SPACES.

           05  PREV-DAYS                   PIC 9(3)     VALUE ZERO.

       01  WS-HEADING1.
           05  FILLER                      PIC X(01)     VALUE '1'.
           05  FILLER                      PIC X(29)     VALUE SPACES.
           05  WS-H1-TITLE                 PIC X(42)     VALUE SPACES.
           05  FILLER                      PIC X(21)     VALUE SPACES.
           05  WS-H1-REPORT-ID             PIC X(05)     VALUE 'EL595'.
           05  WS-H1-REPORT-ID2            PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(34)     VALUE SPACES.


       01  WS-HEADING2.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(35)     VALUE SPACES.
           05  WS-H2-COMPANY-NAME          PIC X(30)     VALUE SPACES.
           05  FILLER                      PIC X(25)     VALUE SPACES.
           05  WS-H2-DATE                  PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(34)     VALUE SPACES.


       01  WS-HEADING3.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(40)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               'AS OF  '.
           05  WS-H3-THROUGH-DT            PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(30)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE 'PAGE'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.
           05  FILLER                      PIC X(34)     VALUE SPACE.


       01  WS-HEADING4.
           05  FILLER                      PIC X(01)     VALUE '-'.
122010     05  FILLER                      PIC X(07)     VALUE
122010         'LETTER'.           
122010     05  FILLER                      PIC X(02)     VALUE SPACES.
032510     05  FILLER                      PIC X(05)     VALUE
032510         'CLAIM'.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'CLAIM'.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'CERT'.
           05  FILLER                      PIC X(39)     VALUE SPACES.
           05  FILLER                      PIC X(08)     VALUE
               'RECORDED'.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               '# OF'.
122010     05  FILLER                      PIC X(42)     VALUE SPACES.



       01  WS-HEADING4B.
           05  FILLER                      PIC X(01)     VALUE ' '.
122010     05  FILLER                      PIC X(07)     VALUE
122010         '  ID   '.           
122010     05  FILLER                      PIC X(02)     VALUE SPACES.
032510     05  FILLER                      PIC X(05)     VALUE
032510         'TYPE '.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'NUMBER'.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'NUMBER'.
           05  FILLER                      PIC X(11)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'NAME'.
           05  FILLER                      PIC X(25)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'DATE'.
           05  FILLER                      PIC X(06)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'DAYS'.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'STATUS'.    
122010     05  FILLER                      PIC X(31)     VALUE SPACES.


       01  WS-DETAIL.
           05  FILLER                      PIC X(01)     VALUE SPACE.
122010     05  FILLER                      PIC X(01)     VALUE SPACE.
122010     05  WS-DET-LETTER-ID            PIC X(04).           
122010     05  FILLER                      PIC X(04)     VALUE SPACES.
032510     05  WS-DET-CLAIM-TYPE           PIC X(05).
032510     05  FILLER                      PIC X(04)     VALUE SPACES.
           05  WS-DET-CLAIM-NO             PIC X(07).
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-DET-CERT-NO              PIC X(11).
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-DET-NAME                 PIC X(30).
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-DET-RECORDED-DT          PIC X(10).
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-DET-NUM-DAYS             PIC ZZ9.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-DET-STATUS               PIC X(20).
122010     05  FILLER                      PIC X(20)     VALUE SPACES.

032510 01  WS-LIFE-TOT-REC.
032510     05  FILLER                  PIC X(01)     VALUE '-'.
032510     05  FILLER                  PIC X(22)    VALUE SPACES.
032510     05  FILLER                  PIC X(27)    VALUE
032510            'TOTAL LIFE LETTERS         '.
032510     05  WS-LIFE-TOT             PIC Z,ZZZ,ZZ9.       
032510     05  FILLER                  PIC X(74)    VALUE SPACES.
032510
032510 01  WS-DISAB-TOT-REC.
032510     05  FILLER                  PIC X(01)     VALUE ' '.
032510     05  FILLER                  PIC X(22)    VALUE SPACES.
032510     05  FILLER                  PIC X(27)    VALUE
032510            'TOTAL DISABILITY LETTERS   '.
032510     05  WS-DISAB-TOT             PIC Z,ZZZ,ZZ9.       
032510     05  FILLER                  PIC X(74)    VALUE SPACES.
032510
032510 01  WS-TOT-REC.
032510     05  FILLER                  PIC X(01)     VALUE '0'.
032510     05  FILLER                  PIC X(22)    VALUE SPACES.
032510     05  FILLER                  PIC X(25)    VALUE
032510            'TOTAL LETTERS            '.
032510     05  WS-TOTAL                PIC ZZZ,ZZZ,ZZ9.       
032510     05  FILLER                  PIC X(74)    VALUE SPACES.
032510
      *              *************
      *              ELCDTECX: LAYOUT FOR DISK-DATE FILE
                     COPY ELCDTECX.


                     COPY ELCDTEVR.

      *              *************
      *              ELCDATE: LAYOUT OF DATA PASSED TO DATE CONV RTN
                     COPY ELCDATE.


       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-CYCLE-DT               PIC X(08)     VALUE SPACES.

      ******************************************************************
      ********************************
       PROCEDURE DIVISION USING PARM.

      ****************READ DISK-DATE FILE
       0000-DATE-CARD-READ. COPY ELCDTERX.

       1000-MAIN-LOGIC.

           PERFORM 1500-EDIT-CYCLE-DATE       THRU 1500-EXIT.

           PERFORM OPEN-FILES                 THRU OPEN-FILES-EXIT.

           MOVE WS-DETAIL-REPORT-TITLE        TO WS-H1-TITLE.
           MOVE COMPANY-NAME                  TO WS-H2-COMPANY-NAME.
           MOVE WS-CURRENT-DATE               TO WS-H2-DATE.
           MOVE WS-EDITED-CYCLE-DT            TO WS-H3-THROUGH-DT.
           
           PERFORM 3500-PRINT-HEADINGS THRU 3500-EXIT.

           SORT SORT-FILE  DESCENDING KEY SORT-DAYS ASCENDING 
                          SORT-NAME SORT-CLAIM-NO SORT-CERT-NO
           INPUT  PROCEDURE 2000-INPUT-PROCEDURE  THRU 2000-EXIT
           OUTPUT PROCEDURE 3000-OUTPUT-PROCEDURE THRU 3000-EXIT.

           IF SORT-RETURN  NOT = ZEROS
               MOVE 'INTERNAL SORT ABORTED'
                                       TO WS-ABEND-MESSAGE
               MOVE '0101'             TO WS-RETURN-CODE
               PERFORM ABEND-PGM       THRU APS-EXIT
           END-IF.

           PERFORM CLOSE-FILES        THRU CLOSE-FILES-EXIT.


           GOBACK.


       1500-EDIT-CYCLE-DATE.

           IF PARM-LENGTH = +0
               DISPLAY 'CYCLE DATE INPUT PARMS MISSING'
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF.

           MOVE PARM-CYCLE-DT               TO DC-GREG-DATE-CYMD.
           MOVE 'L'                         TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT.

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1           TO WS-CYCLE-DT-BINARY
               MOVE PARM-CYCLE-DT           TO WS-CYCLE-DT
               MOVE WS-CYCLE-DT-MM          TO WS-EDITED-CYCLE-DT-MM
               MOVE WS-CYCLE-DT-DD          TO WS-EDITED-CYCLE-DT-DD
               MOVE WS-CYCLE-DT-CC          TO WS-EDITED-CYCLE-DT-CC
               MOVE WS-CYCLE-DT-YY          TO WS-EDITED-CYCLE-DT-YY
           ELSE
               DISPLAY 'INVALID CYCLE DATE ' PARM-CYCLE-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF.


       1500-EXIT.
           EXIT.


       2000-INPUT-PROCEDURE.


           
           MOVE LOW-VALUES             TO AT-CONTROL-PRIMARY.
040312     MOVE dte-clasic-company-cd  TO AT-COMPANY-CD
           
           START ELTRLR-INFILE KEY NOT < AT-CONTROL-PRIMARY
               INVALID KEY
                   DISPLAY 'INVALID KEY ON START ELTRLR '
                                            AT-CONTROL-PRIMARY
                   PERFORM ABEND-PGM        THRU APS-EXIT
           END-START.

           PERFORM 2100-READ-ELTRLR         THRU 2100-EXIT
               UNTIL END-OF-ELTRLR.

           
       2000-EXIT.
           EXIT.


       2100-READ-ELTRLR.

           READ ELTRLR-INFILE NEXT RECORD
               AT END
                   SET END-OF-ELTRLR        TO TRUE
                   GO TO 2100-EXIT
           END-READ.

040312     if at-company-cd not = dte-clasic-company-cd
040312        set end-of-eltrlr        to true
040312        go to 2100-exit
040312     end-if

122010     IF CORRESPONDENCE-TR
122010         MOVE AT-STD-LETTER-FORM TO WS-LETTER-ID
122010     END-IF.

           IF CORRESPONDENCE-TR AND
122010        VALID-LETTER  AND
122010        AT-LETTER-ANSWERED-DT NOT GREATER THAN SPACES AND
122010        AT-RESEND-PRINT-DATE NOT GREATER THAN SPACES AND
122010        AT-STOP-LETTER-DT NOT GREATER THAN SPACES             
               PERFORM 2300-RELEASE-DETAIL THRU 2300-EXIT
           END-IF.
           
           
       2100-EXIT.
           EXIT.


       2300-RELEASE-DETAIL.
           MOVE AT-COMPANY-CD          TO CL-COMPANY-CD.
           MOVE AT-CARRIER             TO CL-CARRIER.
           MOVE AT-CLAIM-NO            TO CL-CLAIM-NO.
           MOVE AT-CERT-NO             TO CL-CERT-NO.
           READ ELMSTR.
122010     IF CLAIM-IS-CLOSED
122010         GO TO 2300-EXIT
122010     END-IF.           
       		 MOVE SPACES TO SORT-NAME.
           IF WS-ELMSTR-FILE-STATUS EQUAL '00'
               STRING CL-INSURED-LAST-NAME DELIMITED BY '  ', 
                      ', ' DELIMITED BY SIZE,
                      CL-INSURED-1ST-NAME DELIMITED BY '  ',
                      ' ' DELIMITED BY SIZE,
                      CL-INSURED-MID-INIT
                  INTO SORT-NAME
           END-IF.

032510     IF CL-CLAIM-TYPE = 'L'
032510        MOVE 'LIFE ' TO SORT-CLAIM-TYPE
032510     ELSE
032510        MOVE 'DISAB' TO SORT-CLAIM-TYPE
032510     END-IF.           
           MOVE AT-CLAIM-NO    TO SORT-CLAIM-NO.
           MOVE AT-CERT-NO     TO SORT-CERT-NO.
           MOVE AT-RECORDED-BY TO SORT-RECORDED-BY.
           MOVE AT-RECORDED-DT TO SORT-RECORDED-DT.
 
           MOVE AT-RECORDED-DT TO DC-BIN-DATE-1
           MOVE WS-CYCLE-DT-BINARY TO DC-BIN-DATE-2
           MOVE '1'                     TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-ELAPSED-DAYS     TO SORT-DAYS
           ELSE
               DISPLAY 'CONVERSION ERROR IN ELAPSED TIME CALC '
                                            DC-ERROR-CODE
               MOVE 999 TO SORT-DAYS
      *         PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF.
           
           IF AT-REASON-TEXT > SPACES
               MOVE AT-REASON-TEXT          TO SORT-REASON
           ELSE
               MOVE SPACES                  TO SORT-REASON
           END-IF.
122010
122010     MOVE AT-STD-LETTER-FORM          TO SORT-LETTER-ID.           
               
           RELEASE SORT-RECORD.

       2300-EXIT.
           EXIT.


       3000-OUTPUT-PROCEDURE.

           PERFORM 3020-RETURN-SORT THRU 3020-EXIT
               UNTIL END-OF-SORTFILE.

           IF WS-SORT-RTN-COUNT = 0
               MOVE WS-NO-LETTER-MESSAGE TO PRT
               PERFORM 3900-WRITE  THRU 3900-EXIT
032510     ELSE
032510         MOVE WS-LIFE-COUNT TO WS-LIFE-TOT
032510         MOVE WS-LIFE-TOT-REC TO PRT
032510         PERFORM 3900-WRITE  THRU 3900-EXIT
032510
032510         MOVE WS-DISAB-COUNT TO WS-DISAB-TOT
032510         MOVE WS-DISAB-TOT-REC TO PRT
032510         PERFORM 3900-WRITE  THRU 3900-EXIT
032510
032510         MOVE WS-SORT-RTN-COUNT TO WS-TOTAL
032510         MOVE WS-TOT-REC TO PRT
032510         PERFORM 3900-WRITE  THRU 3900-EXIT
           END-IF.
           
           
       3000-EXIT.
           EXIT.

       3020-RETURN-SORT.

           RETURN SORT-FILE
               AT END
                   SET END-OF-SORTFILE TO TRUE
                   GO TO 3020-EXIT
           END-RETURN.
 
           ADD +1 TO WS-SORT-RTN-COUNT
032510     IF SORT-CLAIM-TYPE EQUAL 'LIFE '
032510        ADD +1 TO WS-LIFE-COUNT 
032510     ELSE
032510        ADD +1 TO WS-DISAB-COUNT
032510     END-IF.          
           IF WS-SORT-RTN-COUNT = 1
               MOVE SORT-DAYS TO PREV-DAYS
           END-IF.           
                                            
           IF WS-LINE-COUNT >= WS-LINE-COUNT-MAX
               PERFORM 3500-PRINT-HEADINGS THRU 3500-EXIT
           END-IF.

032510     MOVE SORT-CLAIM-TYPE    TO WS-DET-CLAIM-TYPE.
           MOVE SORT-CLAIM-NO      TO WS-DET-CLAIM-NO.
           MOVE SORT-CERT-NO       TO WS-DET-CERT-NO.
           MOVE SORT-NAME          TO WS-DET-NAME.
           MOVE SORT-DAYS          TO WS-DET-NUM-DAYS.          
           MOVE SORT-REASON (1:20) TO WS-DET-STATUS.
           MOVE SORT-RECORDED-DT   TO DC-BIN-DATE-1.
           MOVE ' '                TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT.

           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-B-EDIT TO WS-DET-RECORDED-DT
           ELSE
               MOVE SPACES         TO WS-DET-RECORDED-DT
           END-IF.

122010     MOVE SORT-LETTER-ID     TO WS-DET-LETTER-ID.                

           MOVE WS-DETAIL                   TO PRT.
           IF SORT-DAYS NOT EQUAL PREV-DAYS
               MOVE SORT-DAYS TO PREV-DAYS
               MOVE '0' TO P-CTL
           ELSE
               MOVE ' ' TO P-CTL
           END-IF.
           PERFORM 3900-WRITE               THRU 3900-EXIT.
           
           
       3020-EXIT.
           EXIT.


       3500-PRINT-HEADINGS.

           MOVE 'A'                         TO WS-H1-REPORT-ID2.
           MOVE WS-HEADING1                 TO PRT.
           PERFORM 3900-WRITE               THRU 3900-EXIT.

           MOVE WS-HEADING2                 TO PRT.
           PERFORM 3900-WRITE               THRU 3900-EXIT.

           ADD +1                           TO WS-PAGE.
           MOVE WS-PAGE                     TO WS-H3-PAGE.
           MOVE WS-HEADING3                 TO PRT.
           PERFORM 3900-WRITE               THRU 3900-EXIT.

           MOVE WS-HEADING4                 TO PRT.
           PERFORM 3900-WRITE               THRU 3900-EXIT.

           MOVE WS-HEADING4B                TO PRT.
           PERFORM 3900-WRITE               THRU 3900-EXIT.

           MOVE SPACES                      TO PRT.
           MOVE ' '                         TO P-CTL.
           PERFORM 3900-WRITE               THRU 3900-EXIT.

          
       3500-EXIT.
           EXIT.


       3900-WRITE.

           EVALUATE TRUE
           WHEN P-CTL = '1'
               MOVE +1                      TO WS-LINE-COUNT

           WHEN P-CTL = SPACE
               ADD +1                       TO WS-LINE-COUNT

           WHEN P-CTL = '0'
               ADD +2                       TO WS-LINE-COUNT

           WHEN OTHER
               ADD +3                       TO WS-LINE-COUNT
           END-EVALUATE


           WRITE PRT

           .
       3900-EXIT.
           EXIT.



       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.


       OPEN-FILES.

           OPEN INPUT  ELTRLR-INFILE
                       ELMSTR
                OUTPUT PRNTR.

           IF WS-ELTRLR-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELTRLR'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-ELTRLR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF.

           IF WS-ELMSTR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELMSTR OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE WS-ELMSTR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.
           
           
       OPEN-FILES-EXIT.
           EXIT.


       CLOSE-FILES.



           CLOSE ELTRLR-INFILE
                 ELMSTR
                 PRNTR.

            
       CLOSE-FILES-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
ABEND.

ABEND.
ABEND.
BEND.
BEND.
ABEND.
