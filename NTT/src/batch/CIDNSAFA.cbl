       IDENTIFICATION DIVISION.
       PROGRAM-ID.                CIDNSAFA.
      *AUTHOR.     AJRA.
      *REMARKS.
      * THIS PROGRAM PERFORMS THE FINAL ACTION ON LETTERS
      * WITH FOLLOW-UP DATES FOR ACCT SERVICES
100812******************************************************************
100812*                   C H A N G E   L O G
100812*
100812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100812*-----------------------------------------------------------------
100812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100812* EFFECTIVE    NUMBER
100812*-----------------------------------------------------------------
100812* 100812    2011022800001  AJRA  ADD MSG FOR PREV CANCELLED TO RPT
101512* 101512    2011022800001  AJRA  ADD LF ALT PREM TO LF REF AMT
112812* 112812    2012101700001  AJRA  FIX REPORT
021714* 021714    2013090300001  AJRA  FORCE PENDING ISSUE AND ADD NOTE
022414* 022414    2013090300001  AJRA  ADD MSG FOR FATAL ERROR
011315* 011315  CR2014041500001  PEMA  Add cancels to Logic DB
051215* 051215  IR2015051200003  PEMA  Chg location of DB connection
100812******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERARCH           ASSIGN TO ERARCH
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS LA-CONTROL-PRIMARY
                                   FILE STATUS IS ERARCH-FILE-STATUS.

           SELECT ELCNTL           ASSIGN TO ELCNTL
                                   ACCESS IS DYNAMIC
                                   ORGANIZATION IS INDEXED
                                   FILE STATUS IS ELCNTL-FILE-STATUS
                                   RECORD KEY IS CF-CONTROL-PRIMARY.

           SELECT ELCERT           ASSIGN TO ELCERT
                                   ACCESS IS DYNAMIC
                                   ORGANIZATION IS INDEXED
                                   FILE STATUS IS ELCERT-FILE-STATUS
                                   RECORD KEY IS CM-CONTROL-PRIMARY.

           SELECT ERPNDB           ASSIGN TO ERPNDB2
                                   ACCESS IS DYNAMIC
                                   ORGANIZATION IS INDEXED
                                   FILE STATUS IS ERPNDB-FILE-STATUS
                                   RECORD KEY IS PB-CONTROL-BY-ACCOUNT.
021714
021714     SELECT ERCNOT           ASSIGN TO ERCNOT
021714                             ACCESS IS DYNAMIC
021714                             ORGANIZATION IS INDEXED
021714                             FILE STATUS IS ERCNOT-FILE-STATUS
021714                             RECORD KEY IS CZ-CONTROL-PRIMARY.

           SELECT DISK-DATE        ASSIGN TO SYS019.
           
           SELECT EXTRACT-FILE     ASSIGN TO SYS011
                                   ORGANIZATION IS RECORD SEQUENTIAL. 
           
           SELECT REPORT-FILE      ASSIGN TO SYS012
                                   ORGANIZATION IS LINE SEQUENTIAL. 

       DATA DIVISION.

       FILE SECTION.

       FD  ERARCH.
                                       COPY ERCARCH.

       FD  ELCNTL.
                                       COPY ELCCNTL.

       FD  ELCERT.
                                       COPY ELCCERT.

       FD  ERPNDB.
                                       COPY ERCPNDB.
021714
021714 FD  ERCNOT.
021714                                 COPY ERCCNOT.

       FD  DISK-DATE
           COPY ELCDTEFD.

       FD  EXTRACT-FILE
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  EXTRACT-RECORD                 PIC X(80).

       FD  REPORT-FILE
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                  PIC X(133).


         WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDNSARE  WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '*********** VMOD=2.001. ********'.

NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
011315
011315 EXEC SQL
011315    INCLUDE SQLCA
011315 END-EXEC

011315 77  ws-connect-sw               pic x  value spaces.
011315     88  connected-to-db            value 'Y'.
       77  WS-ABEND-FILE-STATUS        PIC XX            VALUE ZERO.    
       77  WS-ABEND-MESSAGE            PIC X(80)         VALUE SPACES.  
       77  WS-ABEND-PROGRAM            PIC X(8)          VALUE SPACES.  
       77  WS-RETURN-CODE              PIC S9(4)         VALUE +0.      
       77  WS-ZERO                     PIC S9     COMP-3 VALUE +0.      
       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ARCH             VALUE 'Y'.
       77  WS-DONE-SW                  PIC X VALUE SPACES.
           88  DONE-WITH-ARCH          VALUE 'Y'.
       77  WS-NEW-ARCH-SW              PIC X VALUE 'N'.
           88 HAVE-NEW-ARCH-NO               VALUE 'Y'.
           88 NEED-NEW-ARCH-NO               VALUE 'N'.
       77  WS-CERT-FOUND-SW            PIC X VALUE ' '.
           88  WS-CERT-FOUND                 VALUE 'Y'.
       77  WS-CERT-STATUS-SW           PIC X VALUE ' '.
           88  CERT-CANCELLED                VALUE 'C'.
       77  ARCH-RECS-IN                PIC 9(9) VALUE ZEROS.
       77  EXTRACT-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  ERARCH-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELCNTL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELCERT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERPNDB-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
021714 77  ERCNOT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  PGM-SUB                     COMP-3 PIC S9(04) VALUE +585.
       77  WS-CURRENT-TIME             PIC S9(7)   VALUE ZERO.
       77  WS-CURRENT-BIN-DT           PIC X(02)  VALUE LOW-VALUES.
       77  WS-BATCH-NO                 PIC S9(8)   COMP VALUE +0.
       77  WS-ZEROS                    PIC S9(03) VALUE +0 COMP-3.
       77  WS-LF-COV                   PIC X VALUE 'N'.
       77  WS-LF-PREM                  PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-AH-COV                   PIC X VALUE 'N'.
       77  WS-AH-PREM                  PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-INS-LAST-NAME            PIC X(15).
112812 77  WS-LINE-CNT                 PIC S9(03) VALUE +0 COMP-3.
112812 77  WS-PAGE-CNT                 PIC S9(03) VALUE +0 COMP-3.
021714 77  WS-CERT-UPDATE-SW           PIC X  VALUE ' '.
021714     88  NO-CERT-RW                 VALUE 'N'.
021714     88  CERT-RW                    VALUE 'Y'.
021714 77  WS-PEND-FORCED              PIC X  VALUE 'N'.
022414 77  WS-PEND-FATAL               PIC X  VALUE 'N'.
021714 77  WS-NOTE-SEQ                 PIC S9(5) VALUE +0 COMP.
011315 77  ws-sql-code                 pic s9(7) value zeros.
011315 77  ws-dis-sql-code             pic -9999999 value zeros.
011315 77  rec-cnt                     pic 9(9) value zeros.
011315 77  ws-recs-in                  pic 9(7) value zeros.

011315 01  P pointer.
011315 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
011315 01  var-ptr pointer.
011315 01  env-var-len                 pic 9(4)  binary.
011315 01  rc                          pic 9(9)  binary.
011315
011315 01  WS-KIXSYS.
011315     05  WS-KIX-FIL1             PIC X(10).
011315     05  WS-KIX-APPS             PIC X(10).
011315     05  WS-KIX-ENV              PIC X(10).
011315     05  WS-KIX-MYENV            PIC X(10).
011315     05  WS-KIX-SYS              PIC X(10).
011315
011315 EXEC SQL
011315    BEGIN DECLARE SECTION
011315 END-EXEC
011315
011315 01  sqlcmd                      pic x(1024).
011315 01  WS-MOE-DATE                 pic x(10).
011315 01  svr                         pic x(32).
011315 01  usr                         pic x(32).
011315 01  pass                        pic x(32).
011315 01  usr-pass                    pic x(64).
011315 01  ws-disp-code                pic s9(11).
011315
011315***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
011315***                                                            ***
011315***  These indicators are used to tell sql server that i am    ***
011315***  passing it a null value.  The indicator will be -1        ***
011315***  if the value is nulls and +0 if the value is other than   ***
011315***  nulls.  Here is a sample on how to use it.                ***
011315***                                                            ***
011315***      if db-date1 = spaces move -1 to nu-date1 end-if       *** 
011315***     EXEC SQL                                               ***
011315***        insert into ERCHEK (                                ***
011315***           date1,                                           ***
011315***           date2)                                           ***
011315***        values (                                            ***
011315***           :db-date1      :nu-date1,                        ***
011315***           :db-date2      :nu-date2)                        ***
011315***     END-EXEC                                               ***
011315***                                                            ***
011315***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
011315
011315 01  indicator-vaiables-for-nulls.
011315     05  NU-lf-cancel-date       PIC s9(4) comp value +0.
011315     05  NU-ah-cancel-date       PIC s9(4) comp value +0.
011315
011315 01  NS-EL513-TABLE-RECORD.
011315     05  TB-MOE-DATE             PIC X(10).
011315     05  TB-CARRIER              PIC X.               
011315     05  TB-GROUPING             PIC X(6).            
011315     05  TB-STATE                PIC XX.              
011315     05  TB-ACCOUNT              PIC X(10).           
011315     05  TB-CERT-EFF-DT          PIC X(10).
011315     05  TB-CERT-NO              PIC X(11).
011315     05  TB-BATCH-NO             PIC X(6).
011315     05  tb-insured-name         pic x(15).
011315     05  tb-lf-cancel-date       pic x(10).
011315     05  tb-lf-cancel-amt        pic 9(7).99.
011315     05  tb-lf-cancel-amt-a redefines
011315         tb-lf-cancel-amt        pic x(10).
011315     05  tb-ah-cancel-date       pic x(10).
011315     05  tb-ah-cancel-amt        pic 9(7).99.
011315     05  tb-ah-cancel-amt-a redefines
011315         tb-ah-cancel-amt        pic x(10).
011315
011315 EXEC SQL
011315    END DECLARE SECTION
011315 END-EXEC

       01  MISC.
           05  WS-CUR-MDY.
               10  WS-CUR-MDY-MM       PIC 9(2).
               10  WS-CUR-MDY-DD       PIC 9(2).
               10  WS-CUR-MDY-YY       PIC 9(2).
           
       01  EXT-INS-REC.
           05  EXT-CERT-NO             PIC X(11).
           05  EXT-EFF-DATE            PIC X(6).
           05  EXT-LF-CANCEL-DATE      PIC X(6).
           05  EXT-LF-CANCEL-PREM      PIC 9(7)V99.
           05  EXT-AH-CANCEL-DATE      PIC X(6).
           05  EXT-AH-CANCEL-PREM      PIC 9(7)V99.
           05  EXT-INSURED-NAME        PIC X(15).
           05  EXT-FILLER              PIC X(15).
           05  EXT-FORCE-CODE          PIC X(1).
           05  EXT-TRANS-TYPE          PIC X(1).
           05  EXT-TRANS-SEQ           PIC X(1).
           
       01  EXT-BATCH-REC.
           05  EXT-CARRIER             PIC X(1).
           05  EXT-GROUP               PIC X(6).
           05  EXT-STATE               PIC X(2).
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-BATCH-NO            PIC 9(6).
           05  EXT-BATCH-DATE          PIC X(6).
           05  EXT-ISS-COUNT           PIC 9(4).
           05  EXT-ISS-LF-PREM         PIC 9(7)V99.
           05  EXT-ISS-AH-PREM         PIC 9(7)V99.
           05  EXT-CAN-COUNT           PIC 9(4).
           05  EXT-CAN-LF-PREM         PIC 9(7)V99.
           05  EXT-CAN-AH-PREM         PIC 9(7)V99.
           05  EXT-CLIENT-ID           PIC X(3).
           05  EXT-B-TRANS-TYPE        PIC X(1).
           05  EXT-B-TRANS-SEQ         PIC X(1).

       01  BLANK-LINE.
           05  FILLER                  PIC X(133) VALUE SPACES.

       01  REPORT-HEAD1.
112812     05  FILLER                  PIC X(8)  VALUE 'CIDNSAFA'.       
112812     05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(23)
               VALUE 'CENTRAL STATES OF OMAHA'.
           05  FILLER                  PIC X(23) VALUE SPACES.
           05  RPT-DATE                PIC X(08) VALUE SPACES.
           05  FILLER                  PIC X(41) VALUE SPACES.

       01  REPORT-HEAD2.
           05  FILLER                  PIC X(32) VALUE SPACES.
           05  FILLER                  PIC X(35)
               VALUE 'LETTER ARCHIVE FINAL ACTION UPDATES'.
112812     05  FILLER                  PIC X(20) VALUE SPACES.
112812     05  FILLER                  PIC X(05) VALUE 'PAGE '.
112812     05  RPT-PAGE                PIC ZZ9   VALUE ZEROS.
112812     05  FILLER                  PIC X(38) VALUE SPACES.
           
       01  REPORT-HEAD3.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(11)
               VALUE 'CERT NUMBER'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(8)
               VALUE 'ARCH NUM'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(9)
               VALUE 'LETTER ID'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(12)
               VALUE 'FINAL ACTION'.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(7)
               VALUE 'MESSAGE'.
           05  FILLER                  PIC X(73) VALUE SPACES.

       01  REPORT-REC.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  RPT-CERTNO              PIC X(11).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  RPT-ARCHIVENO           PIC 9(8).
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  RPT-LETTERID            PIC X(4).
           05  FILLER                  PIC X(10) VALUE SPACES.
           05  RPT-FINAL-ACT           PIC X(1).
           05  FILLER                  PIC X(7) VALUE SPACES.
           05  RPT-MESSAGE             PIC X(50).
           05  FILLER                  PIC X(34) VALUE SPACES.

021714
021714 01  WS-MSGS.
021714     05  WS-CAN-W-FORCE          PIC X(50) VALUE
021714         'CANCEL TRANSACTION CREATED - PENDING ISSUE FORCED '.
021714     05  WS-CAN-NO-FORCE         PIC X(50) VALUE
021714         'CANCEL TRANSACTION CREATED                        '.
021714     05  WS-STAT-RPT             PIC X(50) VALUE
021714         'STAT REPORT                                       '.
021714     05  WS-CAN-W-FATAL          PIC X(50) VALUE
021714         'CANCEL TRANSACTION CREATED - X MESSAGE ON ISSUE   '.
021714
021714 01  CERT-NOTE-ENTRIES.
021714     12  CERT-NT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.
021714         16  CERT-NT-TEXT                PIC X(63).
021714         16  CERT-NT-LAST-MAINT-BY       PIC XXXX.
021714         16  CERT-NT-LAST-MAINT-DT       PIC XX.
021714         16  CERT-NT-LAST-MAINT-HHMMSS   PIC S9(7) COMP-3.
021714
                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       LINKAGE SECTION.

011315 01  var  pic x(30).
       
       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-CYCLE-DATE             PIC X(08)     VALUE SPACES.
           05  PARM-MONTHEND-DATE          PIC X(08)     VALUE SPACES.

       PROCEDURE DIVISION USING PARM.

           IF PARM-CYCLE-DATE = SPACES
               DISPLAY 'MISSING CYCLE DATE PARM'
               PERFORM ABEND-PGM
           END-IF.
           
           DISPLAY ' '.
           DISPLAY 'CYCLE DATE IS    ' PARM-CYCLE-DATE.
           DISPLAY 'MONTHEND DATE IS ' PARM-MONTHEND-DATE.
           DISPLAY ' '.
           
           IF PARM-CYCLE-DATE = PARM-MONTHEND-DATE
               DISPLAY '*****  LAST CYCLE OF THE MONTH. '
                       'FINAL ACTION JOB BYPASSED.  *****'
               DISPLAY ' '
               GOBACK
           END-IF.

011315     display ' Begin Program '
011315
011315     set P to address of KIXSYS
011315     CALL "getenv" using by value P returning var-ptr
011315     if var-ptr = null then
011315        display ' kixsys not set '
011315     else
011315        set address of var to var-ptr
011315        move 0 to env-var-len
011315        inspect var tallying env-var-len
011315          for characters before X'00' 
011315        unstring var (1:env-var-len) delimited by '/'
011315           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
011315              WS-KIX-SYS
011315        end-unstring
011315     end-if
011315
011315     display ' KIXSYS  ' ws-kix-myenv

           .
       0000-DATE-CARD-READ. COPY ELCDTERX.

051215     perform 0900-connect        thru 0900-exit
051215*    perform 0950-drop-table     thru 0950-exit
051215*    perform 1000-create-table   thru 1000-exit
           PERFORM 0040-OPEN-FILES     THRU 0040-EXIT.
           PERFORM 0050-INIT           THRU 0050-EXIT.

           PERFORM 0020-PROCESS        THRU 0020-EXIT UNTIL
              (END-OF-ARCH).
           PERFORM 0060-CLOSE-FILES    THRU 0060-EXIT.

011315     perform 1100-finish-up      thru 1100-exit
           DISPLAY ' ARCH RECS READ     ' ARCH-RECS-IN.
           DISPLAY ' EXTRACT RECS WRITE ' EXTRACT-RECS-OUT.

           GOBACK.

       0020-PROCESS.
           
           PERFORM 0025-READ-ERARCH THRU 0025-EXIT.

           IF END-OF-ARCH
               GO TO 0020-EXIT
           END-IF.
           
           IF LA-TEMP
               GO TO 0020-EXIT
           END-IF.

           IF NOT LA-STATUS-ACTIVE
               GO TO 0020-EXIT
           END-IF.

           IF (LA-FOLLOW-UP-DATE EQUAL LOW-VALUES OR SPACES) OR
              (LA-FINAL-ACT-IND EQUAL LOW-VALUES OR SPACES)
                 GO TO 0020-EXIT
           END-IF.

           IF  LA-FOLLOW-UP-DATE GREATER THAN WS-CURRENT-BIN-DT
               GO TO 0020-EXIT
           END-IF.

           IF (LA-REPLY-DATE NOT EQUAL LOW-VALUES AND SPACES) OR
              (LA-VOIDED-DATE NOT EQUAL LOW-VALUES AND SPACES) OR
              (LA-PURGED-DATE NOT EQUAL LOW-VALUES AND SPACES) 
                 GO TO 0020-EXIT
           END-IF.
                      
           MOVE SPACES TO WS-CERT-FOUND-SW
                          WS-CERT-STATUS-SW.
           PERFORM 0200-CHECK-CERT-STATUS THRU 0200-EXIT.
           IF (CERT-CANCELLED)
             OR (NOT WS-CERT-FOUND)
              DISPLAY 'CERT CANCELLED OR NOT FOUND '
                 LA-CERT-NO-A2 ' ' LA-ARCHIVE-NO ' ' LA-FORM-A3
100812
100812        MOVE LA-CERT-NO-A2       TO RPT-CERTNO
100812        MOVE LA-ARCHIVE-NO       TO RPT-ARCHIVENO
100812        MOVE LA-FORM-A3          TO RPT-LETTERID
100812        MOVE LA-FINAL-ACT-IND    TO RPT-FINAL-ACT
100812        MOVE 'CERT PREVIOUSLY CANCELLED, NO CANCEL TRAN CREATED'
100812                                 TO RPT-MESSAGE
112812        IF WS-LINE-CNT > +55
112812           ADD +1 TO WS-PAGE-CNT
112812           MOVE WS-PAGE-CNT TO RPT-PAGE
112812           WRITE REPORT-RECORD FROM REPORT-HEAD1
112812           WRITE REPORT-RECORD FROM REPORT-HEAD2
112812           WRITE REPORT-RECORD FROM BLANK-LINE
112812           WRITE REPORT-RECORD FROM REPORT-HEAD3
112812           MOVE +4 TO WS-LINE-CNT
112812        END-IF
100812        WRITE REPORT-RECORD FROM REPORT-REC
112812        ADD +1 TO WS-LINE-CNT

              MOVE 'C'                 TO LA-STATUS
              MOVE 'SYST'              TO LA-LAST-UPDATED-BY
              MOVE WS-CURRENT-BIN-DT   TO LA-LAST-MAINT-DATE
              MOVE WS-CURRENT-TIME     TO LA-LAST-MAINT-TIME
              REWRITE LETTER-ARCHIVE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ON ERARCH - REWRITE - CANCEL '
                    ERARCH-FILE-STATUS  '  CERT '  LA-CERT-NO-A2
              END-IF
              GO TO 0020-EXIT
           END-IF.
           
100812     IF LA-FINAL-ACT-IND EQUAL 'C' OR 'B'
               PERFORM 0035-WRITE-EXT THRU 0035-EXIT
           END-IF.

           PERFORM 0030-PROCESS-ARCHIVE  THRU 0030-EXIT.
                      
       0020-EXIT.
           EXIT.

       0025-READ-ERARCH.

           READ ERARCH NEXT RECORD.

           IF (ERARCH-FILE-STATUS = '23' OR '10') OR
             (LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)           
               SET END-OF-ARCH     TO TRUE
           ELSE 
              IF ERARCH-FILE-STATUS NOT = '00'
                 SET END-OF-ARCH     TO TRUE
                 DISPLAY 'BAD READ ERARCH ' ERARCH-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF.

           IF NOT END-OF-ARCH 
              ADD 1                TO ARCH-RECS-IN
           END-IF.
                      
       0025-EXIT.
           EXIT.


       0030-PROCESS-ARCHIVE.
       
           MOVE LA-CERT-NO-A2       TO RPT-CERTNO.
           MOVE LA-ARCHIVE-NO       TO RPT-ARCHIVENO.
           MOVE LA-FORM-A3          TO RPT-LETTERID.
           MOVE LA-FINAL-ACT-IND    TO RPT-FINAL-ACT.
100812     IF LA-FINAL-ACT-IND EQUAL 'C' OR 'B'
022414       IF WS-PEND-FATAL = 'Y'
022414         MOVE WS-CAN-W-FATAL  TO RPT-MESSAGE
022414       ELSE 
021714         IF WS-PEND-FORCED = 'Y'
021714            MOVE WS-CAN-W-FORCE  TO RPT-MESSAGE
021714         ELSE
021714            MOVE WS-CAN-NO-FORCE TO RPT-MESSAGE
021714         END-IF
022414       END-IF
           ELSE
021714         MOVE WS-STAT-RPT        TO RPT-MESSAGE
           END-IF.
112812     IF WS-LINE-CNT > +55
112812        ADD +1 TO WS-PAGE-CNT
112812        MOVE WS-PAGE-CNT TO RPT-PAGE
112812        WRITE REPORT-RECORD FROM REPORT-HEAD1
112812        WRITE REPORT-RECORD FROM REPORT-HEAD2
112812        WRITE REPORT-RECORD FROM BLANK-LINE
112812        WRITE REPORT-RECORD FROM REPORT-HEAD3
112812        MOVE +4 TO WS-LINE-CNT
112812     END-IF
           WRITE REPORT-RECORD FROM REPORT-REC.
112812     ADD +1 TO WS-LINE-CNT.


           MOVE 'C'                 TO LA-STATUS.
           MOVE 'SYST'              TO LA-LAST-UPDATED-BY.
           MOVE WS-CURRENT-BIN-DT   TO LA-LAST-MAINT-DATE.
           MOVE WS-CURRENT-TIME     TO LA-LAST-MAINT-TIME.
                                      
           REWRITE LETTER-ARCHIVE
                INVALID KEY
                    DISPLAY ' CIDNSAFA REWRITE ERROR - ERARCH'
                    SET END-OF-ARCH TO TRUE
                    PERFORM ABEND-PGM.

            .  
       0030-EXIT.
            EXIT.
              
       0035-WRITE-EXT.
           
           MOVE SPACES              TO EXT-INS-REC
                                       EXT-BATCH-REC
011315                                 NS-EL513-TABLE-RECORD
011315     MOVE ZEROS                  TO TB-LF-CANCEL-AMT
011315                                    TB-AH-CANCEL-AMT

011315     move -1                     to NU-lf-cancel-date
011315                                    NU-ah-cancel-date

           PERFORM 0500-GET-BATCH-NO THRU 0500-EXIT.
           
           MOVE LA-CERT-NO-A2       TO EXT-CERT-NO
011315                                 tb-cert-no
           MOVE LA-EFFECT-DATE-A2   TO DC-BIN-DATE-1.
           MOVE ' '                 TO DC-OPTION-CODE.
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT.
           IF DATE-CONVERSION-ERROR
              DISPLAY ' EFF DATE ERROR '
              PERFORM ABEND-PGM
           END-IF.
           MOVE DC-GREG-DATE-1-MDY  TO EXT-EFF-DATE

011315     string
011315        dc-edita-month       '/'
011315        dc-edita-day         '/'
011315        dc-edita-ccyy
011315        delimited by size into tb-cert-eff-dt
011315     end-string
           
011315     STRING
011315        PARM-MONTHEND-DATE (5:2) '/'
011315        PARM-MONTHEND-DATE (7:2) '/'
011315        PARM-MONTHEND-DATE (1:4)
011315        DELIMITED BY SIZE INTO TB-MOE-DATE
011315     END-STRING

           IF WS-LF-COV = 'Y'
              MOVE DC-GREG-DATE-1-MDY  TO EXT-LF-CANCEL-DATE
011315        move tb-cert-eff-dt      to tb-lf-cancel-date
              MOVE WS-LF-PREM          TO EXT-LF-CANCEL-PREM
011315                                    tb-lf-cancel-amt
011315        move +0                  to nu-lf-cancel-date
           END-IF.
           IF WS-AH-COV = 'Y'
              MOVE DC-GREG-DATE-1-MDY  TO EXT-AH-CANCEL-DATE
011315        move tb-cert-eff-dt      to tb-ah-cancel-date
              MOVE WS-AH-PREM          TO EXT-AH-CANCEL-PREM
011315                                    tb-ah-cancel-amt
011315        move +0                  to nu-ah-cancel-date
           END-IF.
           MOVE WS-INS-LAST-NAME    TO EXT-INSURED-NAME
011315                                 tb-insured-name
           MOVE SPACES              TO EXT-FILLER.
           MOVE '8'                 TO EXT-FORCE-CODE.
           MOVE '3'                 TO EXT-TRANS-TYPE.
           MOVE '1'                 TO EXT-TRANS-SEQ.
           
           MOVE LA-CARRIER-A2       TO EXT-CARRIER
011315                                 tb-carrier
           MOVE LA-GROUPING-A2      TO EXT-GROUP
011315                                 tb-grouping
           MOVE LA-STATE-A2         TO EXT-STATE
011315                                 tb-state
           MOVE LA-ACCOUNT-A2       TO EXT-ACCOUNT
011315                                 tb-account
           MOVE WS-BATCH-NO         TO EXT-BATCH-NO
011315     move ext-batch-no        to tb-batch-no
           MOVE WS-CUR-MDY          TO EXT-BATCH-DATE.
           MOVE ZEROS               TO EXT-ISS-COUNT.
           MOVE ZEROS               TO EXT-ISS-LF-PREM.
           MOVE ZEROS               TO EXT-ISS-AH-PREM.
           MOVE 1                   TO EXT-CAN-COUNT.
           MOVE WS-LF-PREM          TO EXT-CAN-LF-PREM
           MOVE WS-AH-PREM          TO EXT-CAN-AH-PREM
           MOVE 'CSO'               TO EXT-CLIENT-ID.
           MOVE '1'                 TO EXT-B-TRANS-TYPE.
           MOVE '0'                 TO EXT-B-TRANS-SEQ.
           
           WRITE EXTRACT-RECORD FROM EXT-BATCH-REC.
           WRITE EXTRACT-RECORD FROM EXT-INS-REC.
           ADD +1               TO EXTRACT-RECS-OUT.

021714     PERFORM 0600-UPDATE-PEND-ISSUE THRU 0600-EXIT
011315     perform 0650-insert-row     thru 0650-exit

           .
       0035-EXIT.
            EXIT.

       0040-OPEN-FILES.

           OPEN I-O ERARCH.
           OPEN I-O ELCNTL.
021714     OPEN I-O ERPNDB.
021714     OPEN I-O ELCERT.
021714     OPEN I-O ERCNOT.
           OPEN OUTPUT EXTRACT-FILE REPORT-FILE.

           IF ERARCH-FILE-STATUS NOT = '00' AND '97'
              DISPLAY 'ERARCH OPEN ERR  ' ERARCH-FILE-STATUS
              MOVE ' ERARCH OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERARCH-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ELCNTL-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCNTL OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ELCERT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCERT OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCERT-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ERPNDB-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERPNDB OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERPNDB-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

021714     IF ERCNOT-FILE-STATUS NOT = '00' AND '97'
021714        MOVE ' ERCNOT OPEN ERROR ' TO WS-ABEND-MESSAGE
021714        MOVE ERCNOT-FILE-STATUS    TO WS-ABEND-FILE-STATUS
021714        PERFORM ABEND-PGM
021714     END-IF.
021714
       0040-EXIT.
           EXIT.

       0050-INIT.
           
           ACCEPT WS-TIME-OF-DAY       FROM  TIME
           MOVE WS-TIME                TO  WS-CURRENT-TIME
           MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.
           MOVE '2'                    TO  DC-OPTION-CODE.
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.
           DISPLAY 'CURRENT DATE USED FOR RUN IS - - ' WS-CURRENT-DATE.
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.

           PERFORM 8510-DATE-CONVERSION.
           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1      TO  WS-CURRENT-BIN-DT
           END-IF.
           
           MOVE WS-CD-MM               TO WS-CUR-MDY-MM.
           MOVE WS-CD-DD               TO WS-CUR-MDY-DD.
           MOVE WS-CD-YY               TO WS-CUR-MDY-YY.
           
           MOVE LOW-VALUES             TO LA-CONTROL-PRIMARY.
           MOVE DTE-CLASIC-COMPANY-CD  TO LA-COMPANY-CD.
           MOVE ZEROS                  TO LA-ARCHIVE-NO.

           START ERARCH KEY IS NOT < LA-CONTROL-PRIMARY.

           IF ERARCH-FILE-STATUS = '10' OR '23'
              SET END-OF-ARCH        TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY 'ERARCH START     ' ERARCH-FILE-STATUS
                 SET END-OF-ARCH     TO TRUE
              END-IF
           END-IF.

           MOVE WS-CURRENT-DATE TO RPT-DATE.
112812     MOVE +1 TO WS-PAGE-CNT.
112812     MOVE WS-PAGE-CNT TO RPT-PAGE.
           WRITE REPORT-RECORD FROM REPORT-HEAD1.
           WRITE REPORT-RECORD FROM REPORT-HEAD2.
           WRITE REPORT-RECORD FROM BLANK-LINE.
           WRITE REPORT-RECORD FROM REPORT-HEAD3.
112812     MOVE +4 TO WS-LINE-CNT.

       0050-EXIT.
           EXIT.


       0060-CLOSE-FILES.
       
           CLOSE EXTRACT-FILE REPORT-FILE.
021714     CLOSE ERARCH ELCNTL ELCERT ERPNDB ERCNOT.

           IF ERARCH-FILE-STATUS NOT = '00'
              DISPLAY ' ERARCH CLOSE ' ERARCH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ELCNTL-FILE-STATUS NOT = '00'
              DISPLAY ' ELCNTL CLOSE ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ELCERT-FILE-STATUS NOT = '00' 
              MOVE ' ELCERT CLOSE ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCERT-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ERPNDB-FILE-STATUS NOT = '00' 
              MOVE ' ERPNDB CLOSE ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERPNDB-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           
       0060-EXIT.
           EXIT.



       0200-CHECK-CERT-STATUS.
      
           MOVE 'N'                    TO WS-LF-COV 
                                          WS-AH-COV.
           MOVE +0                     TO WS-LF-PREM
                                          WS-AH-PREM.                               
           PERFORM 0250-READ-CERT THRU 0250-EXIT.
           IF WS-CERT-FOUND
              MOVE CM-INSURED-LAST-NAME TO WS-INS-LAST-NAME
              IF CM-LF-BENEFIT-CD NOT = '00' AND SPACES
                 MOVE 'Y'               TO WS-LF-COV
101512           COMPUTE WS-LF-PREM = CM-LF-PREMIUM-AMT + 
101512                                CM-LF-ALT-PREMIUM-AMT
              END-IF
              IF CM-AH-BENEFIT-CD NOT = '00' AND SPACES
                 MOVE 'Y'               TO WS-AH-COV
                 MOVE CM-AH-PREMIUM-AMT TO WS-AH-PREM
              END-IF
              
              IF (CERT-PEND-ISSUE-ERROR)
                           AND
                 ((CERT-CANCELLED-ONLINE)
                 OR (CERT-PEND-CANCEL-ERROR))
                 
                 MOVE CM-CONTROL-PRIMARY
                                       TO PB-CONTROL-BY-ACCOUNT
                 MOVE +0               TO PB-ALT-CHG-SEQ-NO
                 MOVE '2'              TO PB-RECORD-TYPE
                 READ ERPNDB
                 IF ERPNDB-FILE-STATUS = '00'
                    IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
                       MOVE 'N'        TO WS-LF-COV
                       MOVE +0         TO WS-LF-PREM
                    ELSE
                       IF CM-LF-DEATH-DT NOT = LOW-VALUES AND SPACES
                          MOVE 'N'     TO WS-LF-COV
                          MOVE +0      TO WS-LF-PREM
                       ELSE
                          IF (CM-LF-LOAN-EXPIRE-DT NOT =
                             LOW-VALUES AND SPACES)
                             AND (CM-LF-LOAN-EXPIRE-DT < BIN-RUN-DATE)
                             MOVE 'N'  TO WS-LF-COV
                             MOVE +0   TO WS-LF-PREM
                          END-IF
                       END-IF
                    END-IF
                    IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
                       MOVE 'N'        TO WS-AH-COV
                       MOVE +0         TO WS-AH-PREM
                    ELSE
                       IF (CM-AH-SETTLEMENT-DT NOT =
                          LOW-VALUES AND SPACES)
                          MOVE 'N'     TO WS-AH-COV
                          MOVE +0      TO WS-AH-PREM
                       ELSE
                          IF (CM-AH-LOAN-EXPIRE-DT NOT =
                             LOW-VALUES AND SPACES)
                             AND (CM-AH-LOAN-EXPIRE-DT < BIN-RUN-DATE)
                             MOVE 'N'  TO WS-AH-COV
                             MOVE +0   TO WS-AH-PREM
                          END-IF
                       END-IF
                    END-IF
                 ELSE
                    DISPLAY ' BAD READ ON ERPNDB ' CM-STATE ' '
                       CM-ACCOUNT ' ' CM-CERT-NO
                 END-IF
              ELSE
                 IF CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
                    MOVE 'N'             TO WS-LF-COV
                    MOVE +0              TO WS-LF-PREM
                 ELSE
                    IF CM-LF-DEATH-DT NOT = LOW-VALUES AND SPACES
                       MOVE 'N'          TO WS-LF-COV
                       MOVE +0           TO WS-LF-PREM
                    ELSE
                       IF (CM-LF-LOAN-EXPIRE-DT NOT =
                          LOW-VALUES AND SPACES)
                          AND (CM-LF-LOAN-EXPIRE-DT < BIN-RUN-DATE)
                          MOVE 'N'       TO WS-LF-COV
                          MOVE +0        TO WS-LF-PREM
                       END-IF
                    END-IF
                 END-IF
                 IF CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
                    MOVE 'N'             TO WS-AH-COV
                    MOVE +0              TO WS-AH-PREM
                 ELSE
                    IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES AND SPACES
                       MOVE 'N'          TO WS-AH-COV
                       MOVE +0           TO WS-AH-PREM
                    ELSE
                       IF (CM-AH-LOAN-EXPIRE-DT NOT =
                          LOW-VALUES AND SPACES)
                          AND (CM-AH-LOAN-EXPIRE-DT < BIN-RUN-DATE)
                          MOVE 'N'       TO WS-AH-COV
                          MOVE +0        TO WS-AH-PREM
                       END-IF
                    END-IF
                 END-IF
              END-IF
              IF WS-LF-COV = 'N' AND WS-AH-COV = 'N'
                 SET CERT-CANCELLED    TO TRUE
              END-IF
           ELSE
              DISPLAY ' ERROR READING ELCERT ' LA-CERT-NO-A2
           END-IF.

       0200-EXIT.
           EXIT.

       0250-READ-CERT.

           MOVE LA-COMPANY-CD          TO CM-COMPANY-CD
           MOVE LA-CARRIER-A2          TO CM-CARRIER
           MOVE LA-GROUPING-A2         TO CM-GROUPING
           MOVE LA-STATE-A2            TO CM-STATE
           MOVE LA-ACCOUNT-A2          TO CM-ACCOUNT
           MOVE LA-EFFECT-DATE-A2      TO CM-CERT-EFF-DT
           MOVE LA-CERT-NO-A2          TO CM-CERT-NO

           READ ELCERT
           IF ELCERT-FILE-STATUS = '00'
              MOVE 'Y'                 TO WS-CERT-FOUND-SW
           ELSE
              DISPLAY ' ERROR ON ELCERT - READ - 0250 '
                 ELCERT-FILE-STATUS
           END-IF.

       0250-EXIT.
           EXIT.

       0500-GET-BATCH-NO.

           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '1'                    TO CF-RECORD-TYPE
           MOVE ZEROS                  TO CF-SEQUENCE-NO
           MOVE SPACES                 TO CF-ACCESS-CD-GENL

           READ ELCNTL

           IF ELCNTL-FILE-STATUS NOT = '00'
               DISPLAY 'BAD READ ELCNTL ' ELCNTL-FILE-STATUS
               MOVE ' ELCNTL READ  ERROR ' TO WS-ABEND-MESSAGE
               MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS
               PERFORM ABEND-PGM
           END-IF
           
           ADD +1                        TO CF-LAST-BATCH-NO
           MOVE CF-LAST-BATCH-NO         TO WS-BATCH-NO

           REWRITE CONTROL-FILE
           IF ELCNTL-FILE-STATUS NOT = '00'
              MOVE ' ELCNTL REWRITE ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

          .
       0500-EXIT.
           EXIT.

021714
021714 0600-UPDATE-PEND-ISSUE.
021714
021714    MOVE 'N'              TO WS-PEND-FORCED
022414    MOVE 'N'              TO WS-PEND-FATAL
021714    MOVE CM-CONTROL-PRIMARY TO PB-CONTROL-BY-ACCOUNT
021714    MOVE +0               TO PB-ALT-CHG-SEQ-NO
021714    MOVE '1'              TO PB-RECORD-TYPE
021714
021714    READ ERPNDB
021714
021714    IF ERPNDB-FILE-STATUS = '00'
022414       IF PB-FATAL-ERRORS
022414           MOVE 'Y'  TO WS-PEND-FATAL
022414       ELSE
021714         IF PB-UNFORCED-ERRORS
021714           MOVE 'Y'  TO WS-PEND-FORCED
021714           MOVE 'A'  TO PB-FORCE-CODE
021714           REWRITE PENDING-BUSINESS
021714           IF ERPNDB-FILE-STATUS = '00'
021714              PERFORM 0610-ADD-CERT-NOTE THRU 0610-EXIT
021714           ELSE
021714              DISPLAY ' ERROR ON ERPNDB - REWRITE - FORCE CD '
021714                 ERPNDB-FILE-STATUS  '  CERT '  PB-CERT-NO
021714           END-IF
021714         END-IF
022414       END-IF
021714    END-IF
021714
021714    .
021714 0600-EXIT.
021714     EXIT.
021714
021714
021714 0610-ADD-CERT-NOTE.
021714
021714     MOVE LOW-VALUES TO CERT-NOTE-ENTRIES
021714     SET TB-INDX TO 1
021714
021714     MOVE 'FORCED FOR AUTO CANCEL' TO   CERT-NT-TEXT (TB-INDX)
021714     MOVE 'SYST'            TO CERT-NT-LAST-MAINT-BY (TB-INDX)
021714     MOVE WS-CURRENT-BIN-DT TO CERT-NT-LAST-MAINT-DT (TB-INDX)
021714     MOVE WS-CURRENT-TIME   TO 
021714                           CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
021714     SET TB-INDX UP BY +1
021714
021714     MOVE CM-CONTROL-PRIMARY TO CZ-CONTROL-PRIMARY
021714     MOVE '1'                TO CZ-RECORD-TYPE
021714     MOVE +0                 TO CZ-NOTE-SEQUENCE
021714
021714     START ERCNOT KEY >= CZ-CONTROL-PRIMARY
021714     IF ERCNOT-FILE-STATUS NOT = '00'
021714        DISPLAY ' ERROR - ERCNOT - START ' ERCNOT-FILE-STATUS
021714           ' ' CM-CONTROL-PRIMARY (2:19)
021714        GO TO 0610-TABLE-LOADED
021714     END-IF
021714
021714     IF CM-CONTROL-PRIMARY NOT = CZ-CONTROL-PRIMARY (1:33)
021714        GO TO 0610-TABLE-LOADED
021714     END-IF.
021714
021714 0610-READ-LOOP.
021714
021714     READ ERCNOT NEXT RECORD
021714     IF ERCNOT-FILE-STATUS = '00' AND
021714        CM-CONTROL-PRIMARY = CZ-CONTROL-PRIMARY (1:33) AND
021714        CZ-RECORD-TYPE = '1'
021714            MOVE CZ-NOTE TO CERT-NT-TEXT (TB-INDX)
021714            MOVE CZ-LAST-MAINT-USER TO 
021714                    CERT-NT-LAST-MAINT-BY (TB-INDX)
021714            MOVE CZ-LAST-MAINT-DT TO 
021714                    CERT-NT-LAST-MAINT-DT (TB-INDX)
021714            MOVE CZ-LAST-MAINT-HHMMSS TO
021714                   CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
021714            SET TB-INDX UP BY 1
021714
021714            DELETE ERCNOT
021714            IF ERCNOT-FILE-STATUS NOT = '00'
021714                DISPLAY ' ERROR - ERCNOT - DELETE ' 
021714                  ERCNOT-FILE-STATUS ' - ' CZ-CERT-NO
021714                GO TO 0610-EXIT
021714            END-IF
021714            GO TO 0610-READ-LOOP
021714     END-IF.
021714
021714 0610-TABLE-LOADED.
021714
021714     PERFORM 0700-WRITE-CERT-NOTE THRU 0700-EXIT
021714
021714    .
021714 0610-EXIT.
021714     EXIT.

011315 0650-insert-row.
011315
011315     if dte-client not = 'CID'
011315        go to 0650-exit
011315     end-if
011315
011315     EXEC SQL
011315        insert into NAPERSOFT_EL513A (
011315           MOEDate          ,
011315           Carrier          ,
011315           Grouping         ,
011315           State            ,
011315           Account          ,
011315           Cert_Eff_Dt      ,
011315           Cert_No          ,
011315           Batch_No         ,
011315           Insured_Name     ,
011315           Lf_Cancel_Date   ,
011315           Lf_Cancel_Amt    ,
011315           Ah_Cancel_Date   ,
011315           Ah_Cancel_Amt)
011315        values (
011315           :TB-MOE-DATE         ,
011315           :TB-CARRIER          ,
011315           :TB-GROUPING         ,
011315           :TB-STATE            ,
011315           :TB-ACCOUNT          ,
011315           :TB-CERT-EFF-DT      ,
011315           :TB-CERT-NO          ,
011315           :TB-BATCH-NO         ,
011315           :TB-INSURED-NAME     ,
011315           :TB-LF-CANCEL-DATE   :nu-lf-cancel-date,
011315           :TB-LF-CANCEL-AMT-a  ,
011315           :TB-AH-CANCEL-DATE   :nu-ah-cancel-date,
011315           :TB-AH-CANCEL-AMT-a)
011315        end-exec
011315
011315     if sqlcode not = 0
011315        display "Error: cannot insert row "
011315        move sqlcode             to ws-sql-code
011315        move ws-sql-code         to ws-dis-sql-code
011315        display ' sqlcode ' ws-dis-sql-code
011315        display ' sql err mess    ' sqlerrmc
011315        display ' in out cnt ' ws-recs-in ' ' rec-cnt
011315        display ' offending rec ' ns-el513-table-record
011315     else
011315        add 1 to ws-recs-in
011315     end-if
011315
011315     .
011315 0650-exit.
011315     exit.

021714 0700-WRITE-CERT-NOTE.
021714
021714     SET TB-INDX         DOWN BY 1.
021714     SET TB-INDX1        TO TB-INDX.
021714
021714     SET TB-INDX TO 1.
021714     MOVE +0             TO WS-NOTE-SEQ
021714
021714     PERFORM VARYING TB-INDX FROM 1 BY 1
021714             UNTIL TB-INDX > TB-INDX1
021714        MOVE SPACES                 TO  CERT-NOTE-FILE
021714        MOVE  'CZ'                  TO  CZ-RECORD-ID
021714        MOVE CM-CONTROL-PRIMARY     TO  CZ-CONTROL-PRIMARY
021714        MOVE '1'                    TO  CZ-RECORD-TYPE
021714        ADD 1                       TO  WS-NOTE-SEQ
021714        MOVE WS-NOTE-SEQ            TO  CZ-NOTE-SEQUENCE
021714        MOVE CERT-NT-TEXT (TB-INDX) TO  CZ-NOTE
021714        MOVE CERT-NT-LAST-MAINT-BY (TB-INDX)
021714                                    TO  CZ-LAST-MAINT-USER
021714        MOVE CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
021714                                    TO  CZ-LAST-MAINT-HHMMSS
021714        MOVE CERT-NT-LAST-MAINT-DT (TB-INDX)
021714                                    TO  CZ-LAST-MAINT-DT
021714
021714        WRITE CERT-NOTE-FILE 
021714        IF ERCNOT-FILE-STATUS NOT = '00'
021714             DISPLAY ' ERROR - ERCNOT - WRITE ' 
021714                  ERCNOT-FILE-STATUS ' - ' CZ-CERT-NO
021714        END-IF
021714     END-PERFORM
021714
021714***UPDATE CERT NOTE FLAG ON CERT
021714     MOVE SPACES TO WS-CERT-UPDATE-SW
021714     EVALUATE CM-NOTE-SW
021714        WHEN '1'
021714        WHEN '3'
021714        WHEN '5'
021714        WHEN '7'
021714           SET NO-CERT-RW     TO TRUE
021714        WHEN ' '
021714           MOVE '1'           TO CM-NOTE-SW
021714        WHEN '2'
021714           MOVE '3'           TO CM-NOTE-SW
021714        WHEN '4'
021714           MOVE '5'           TO CM-NOTE-SW
021714        WHEN '6'
021714           MOVE '7'           TO CM-NOTE-SW
021714     END-EVALUATE
021714
021714     IF NOT NO-CERT-RW
021714         REWRITE CERTIFICATE-MASTER
021714     END-IF
021714
021714    .
021714 0700-EXIT.
021714     EXIT.


011315 0900-connect.
011315
011315***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
011315***                                                            ***
011315***  If this program is run in any other region than cid1p     ***
011315***  then it will populate the tabl on NTSQLTST2, otherwise    ***
011315***  it will populate the table on ntcso2.                     ***
011315***                                                            ***
011315***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
011315
011315     if dte-client not = 'CID'
011315        go to 0900-exit
011315     end-if

      ****  The below code is for when the db has been
      ****  converted to sql server 2016
           evaluate ws-kix-myenv
              when 'cid1p'
                 move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
              when 'mdoff'
                 move '//hov-tstdb01.cso.local:55330;'
                                       to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate

           move 'Logic'                to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc

011315
011315     if sqlcode not = 0
011315        display "Error: cannot connect "
011315        display sqlcode
011315        display sqlerrmc
011315        goback
011315     end-if
011315
011315     set connected-to-db to true
011315     display " connect to DB successful "
011315
011315     .
011315 0900-exit.
011315     exit.
      
011315 0950-drop-table.
011315
011315     if dte-client not = 'CID'
011315        go to 0950-exit
011315     end-if
011315
011315     display 'Begin Drop table'
011315     EXEC SQL
011315         drop table NAPERSOFT_EL513A
011315     END-EXEC
011315     if sqlcode not = 0
011315        display "Error(anticipated) : cannot drop table "
011315        move sqlcode             to ws-disp-code
011315        display ' sql return code ' ws-disp-code
011315        display ' sql err mess    ' sqlerrmc
011315     end-if
011315
011315     .
011315 0950-exit.
011315     exit.

011315 1000-create-table.
011315
011315     if dte-client not = 'CID'
011315        go to 1000-exit
011315     end-if
011315
011315     EXEC SQL
011315        create table NAPERSOFT_EL513A (
011315          MOEDate             datetime not null,
011315          Carrier             char(1)  NOT NULL,
011315          Grouping            char(6)  not null,
011315          State               char(2)  not null,
011315          Account             char(10) not null,
011315          Cert_Eff_Dt         datetime not null,
011315          Cert_No             char(11) not null,
011315          Batch_No            char(6)  not null,
011315          Insured_Name        char(20) null,
011315          Lf_Cancel_Date      datetime null,
011315          Lf_Cancel_Amt       decimal(9,2),
011315          Ah_Cancel_Date      datetime null,
011315          Ah_Cancel_Amt       decimal(9,2)
011315           CONSTRAINT PK_NAPERSOFT_EL513A PRIMARY KEY CLUSTERED
011315             (MOEDate, Carrier, Grouping, State, Account,
011315              Cert_Eff_Dt, Cert_No, Batch_No)
011315       	   )
011315     END-EXEC
011315
011315     if sqlcode not = 0
011315        display "Error: cannot create table "
011315        move sqlcode             to ws-disp-code
011315        display ' sql return code ' ws-disp-code
011315        display ' sql err mess    ' sqlerrmc
011315        goback
011315     end-if
011315
011315     .
011315 1000-exit.
011315     exit.

011315 1100-finish-up.

011315     if dte-client not = 'CID'
011315        go to 1100-exit
011315     end-if
011315
011315     EXEC SQL
011315         commit work
011315     END-EXEC
011315     if sqlcode not = 0
011315        display "Error: commit "
011315        display ' sql return code ' sqlcode
011315        display ' sql err mess    ' sqlerrmc
011315        goback
011315     end-if
011315
011315     if sqlcode not = 0
011315        display "Error: commit release "
011315        display ' sql return code ' sqlcode
011315        display ' sql err mess    ' sqlerrmc
011315     end-if
011315
011315     EXEC SQL
011315        DISCONNECT
011315     END-EXEC
011315
011315     if sqlcode not = 0
011315        display "Error: disconnect "
011315        display ' sql return code ' sqlcode
011315        display ' sql err mess    ' sqlerrmc
011315     end-if
011315
011315     .
011315 1100-exit.
011315     exit.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.   COPY ELCABEND.

