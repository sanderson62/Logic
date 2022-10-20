       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 EL593.


      *AUTHOR.     AJRA.
      ******************************************************************
      *REMARKS.
      *        THIS PROGRAM CREATES A REPORT OF CLAIMS THAT ARE OPEN
      *        THAT HAVE THEIR COVERAGES CANCELLED.
      *
      *     INPUT:   ELMSTR
      *              ELCRTT
      *              ELCERT
      *              ERACCT
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 012810    2009061500002  AJRA  INITIAL PROGRAM
      * 061410    2009061500002  AJRA  CHECK ENDORSEMENT FOR MANUAL FLAG
011211* 011211    2010030900001  AJRA  ADD OPTION 2 AND 4 TO REPORT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
022614* 022614    2013050100003  AJRA  ADD CLAIM NOTE, STOP AUTO PAY
022614*                                REMOVE ERENDR-NO LONGER USED
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
      ******************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT DISK-DATE          ASSIGN TO SYS019-FBA1-S-SYS019.

           SELECT ELMSTR-INFILE      ASSIGN TO ELMSTR
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS CL-CONTROL-PRIMARY
                                     FILE STATUS IS
                                       WS-ELMSTR-FILE-STATUS.

           SELECT ELCERT-INFILE      ASSIGN TO ELCERT
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS CM-CONTROL-PRIMARY
                                     FILE STATUS IS
                                       WS-ELCERT-FILE-STATUS.
      
      
           SELECT ERACCT-INFILE      ASSIGN TO ERACCT
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS AM-CONTROL-PRIMARY
                                     FILE STATUS IS
                                       WS-ERACCT-FILE-STATUS.
      
           SELECT ELCRTT-INFILE      ASSIGN TO ELCRTT
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS CS-CONTROL-PRIMARY
                                     FILE STATUS IS
                                       WS-ELCRTT-FILE-STATUS.
061410
022614*     SELECT ERENDR-INFILE      ASSIGN TO ERENDR
022614*                               ORGANIZATION IS INDEXED
022614*                               ACCESS IS DYNAMIC
022614*                               RECORD KEY IS EN-CONTROL-PRIMARY
022614*                               FILE STATUS IS
022614*                                 WS-ERENDR-FILE-STATUS.
                                             
022614     SELECT ELTRLR-INFILE      ASSIGN TO ELTRLR
022614                               ORGANIZATION IS INDEXED
022614                               ACCESS IS DYNAMIC
022614                               RECORD KEY IS AT-CONTROL-PRIMARY
022614                               FILE STATUS IS
022614                                 WS-ELTRLR-FILE-STATUS.
022614
           SELECT PRNTR              ASSIGN TO SYS008-UR-1403-S-SYS008.
            
           SELECT SORT-FILE          ASSIGN TO SYS001-UT-3380-S-SORTWK1.
      
       DATA DIVISION.
      
       FILE SECTION.
      
       FD  DISK-DATE
                                COPY ELCDTEFD.
      
       FD  ELMSTR-INFILE.
                                COPY ELCMSTR.
      
       FD  ELCERT-INFILE.
                                COPY ELCCERT.
      
       FD  ERACCT-INFILE.
                                COPY ERCACCT.
      
       FD  ELCRTT-INFILE.
                                COPY ELCCRTT.
      
022614* FD  ERENDR-INFILE.
022614*                          COPY ERCENDR.
061410
022614 FD  ELTRLR-INFILE.
022614                          COPY ELCTRLR.
022614
       FD  PRNTR
                                COPY ELCPRTFD.
            
      
       SD  SORT-FILE.
       01  SORT-RECORD.
           05  FILLER                      PIC X(02).
           05  SORT-AUDITOR                PIC X(04).
           05  FILLER                      PIC X(01).
           05  SORT-ACCT-NO                PIC X(10).
           05  FILLER                      PIC X(01).
           05  SORT-CERT                   PIC X(11).
           05  FILLER                      PIC X(01).
           05  SORT-STATE                  PIC X(02).
           05  FILLER                      PIC X(101).
      
      
       WORKING-STORAGE SECTION.
      
       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*     EL593  WORKING STORAGE   *'.
       77  FILLER  PIC X(32)   VALUE '********** V/M 2.008 ***********'.
      
       01  FILLER                          COMP-3.
           05  WS-LINE-COUNT               PIC S9(03)    VALUE +0.
           05  WS-LINE-COUNT-MAX           PIC S9(03)    VALUE +55.
           05  WS-PAGE                     PIC S9(05)    VALUE +0.
           05  WS-SORT-RTN-COUNT           PIC S9(05)    VALUE +0.
      
           05  WS-RETURN-CODE              PIC S9(03)    VALUE +0.
           05  WS-ZERO                     PIC S9(01)    VALUE +0.
           05  WS-SUB                      PIC S9(03) COMP-3 VALUE +0.  
NTTDel*    05  WS-ACCT-CSR                 PIC X(04).         
NTTIns 01  WS-ACCT-CSR                     PIC X(04).         
      
      
       01  FILLER                          COMP SYNC.
           05  PGM-SUB                     PIC S9(04)    VALUE +593.
      
       01  FILLER.
           05  ABEND-CODE                  PIC X(04).
           05  ABEND-OPTION                PIC X(01).
           05  OLC-REPORT-NAME             PIC X(05)     VALUE 'EL593'.
      
           05  WS-CYCLE-DT-BINARY          PIC X(02)     VALUE SPACES.
      
           05  WS-START-DT-BIN             PIC X(02)     VALUE SPACES.
      
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
      
           05  WS-START-DT.
               10  WS-START-DT-YEAR        PIC 9(04)     VALUE ZEROS.
               10  WS-START-DT-MO          PIC 9(02)     VALUE ZEROS.
               10  WS-START-DT-DAY01       PIC 9(02)     VALUE ZEROS.
      
022614     05  WS-CANCEL-DT-BIN            PIC X(02)     VALUE SPACES.
022614     05  WS-CANCEL-DT-GREG           PIC X(08)     VALUE SPACES.
022614
022614     05  WS-CANCEL-NOTE.
022614         10  FILLER                  PIC X(15)
022614             VALUE 'CERT CANCELLED '.
022614         10  WS-CANCEL-NOTE-DATE     PIC X(08).
022614         10  FILLER                  PIC X(37)     VALUE SPACES.
022614
           05  WS-EOF1-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-ELMSTR                         VALUE 'Y'.
061410
061410     05  WS-EOF2-SW                  PIC X(01)     VALUE SPACE.
061410         88  END-OF-ERENDR                         VALUE 'Y'.
      
           05  WS-EOF3-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-SORTFILE                       VALUE 'Y'.
      
           05  WS-EOF4-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-ERACCT                         VALUE 'Y'.
               
           05  WS-CERT-CONTROL-KEY.
               10  WS-COMPANY-CD       PIC X.
               10  WS-CARRIER          PIC X.
               10  WS-GROUPING         PIC X(6).
               10  WS-STATE            PIC XX.
               10  WS-ACCOUNT          PIC X(10).
               10  WS-CERT-EFF-DT      PIC XX.
               10  WS-CERT-PRIME       PIC X(10).
               10  WS-CERT-SFX         PIC X.
      
           05  WS-ACCT-CONTROL.
               10  WS-ACCT-CONTROL-KEY.
                   15  WS-ACCT-COMPANY-CD   PIC X.
                   15  WS-ACCT-CONTROL-A.
                       20  WS-ACCT-CARRIER  PIC X.
                       20  WS-ACCT-GROUPING PIC X(6).
                       20  WS-ACCT-STATE    PIC XX.
                       20  WS-ACCT-ACCOUNT  PIC X(10).
                   15  WS-ACCT-EXP-DT       PIC XX.
               10  FILLER                   PIC X(11).
      
           05  WS-CRTT-CONTROL.
               10  WS-CRTT-CONTROL-KEY     PIC X(33).
               10  WS-CRTT-TRAILER-TYPE    PIC X(1).
      
           05  WS-ELMSTR-FILE-STATUS       PIC X(02)     VALUE ZERO.
           05  WS-ELCERT-FILE-STATUS       PIC X(02)     VALUE ZERO.
           05  WS-ERACCT-FILE-STATUS       PIC X(02)     VALUE ZERO.
           05  WS-ELCRTT-FILE-STATUS       PIC X(02)     VALUE ZERO.
061410     05  WS-ERENDR-FILE-STATUS       PIC X(02)     VALUE ZERO.
022614     05  WS-ELTRLR-FILE-STATUS       PIC X(02)     VALUE ZERO.
      
           05  WS-DETAIL-SW                PIC X(01)     VALUE 'N'.
               88  DETAIL-SETUP-NOT-DONE                 VALUE 'N'.
               88  DETAIL-SETUP-DONE                     VALUE 'D'.
022614
022614     05  WS-MESSAGE-SW               PIC X(01)     VALUE ' '.
022614         88  AUTOPAY-MSG                           VALUE 'A'.
022614         88  PAID-DT-MSG                           VALUE 'P'.
022614         88  AUTOPAY-MSG2                          VALUE '2'.
022614
022614     05  WS-PAID-DT-MSG             PIC X(32)
022614         VALUE 'PAID TO DATE BEYOND CANCEL DATE '.
022614
022614     05  WS-AUTOPAY-MSG             PIC X(32)
022614         VALUE 'ON AUTO PAY - SCHEDULE STOPPED  '.
022614
022614     05  WS-AUTOPAY-MSG2            PIC X(32)
022614         VALUE 'ON AUTO PAY - STOP SCHED FAILED!'.
022614
022614     05  WS-T-NOTE-SW                PIC X(01)     VALUE 'N'.
022614         88 T-NOTE-ALREADY-EXISTS                  VALUE 'Y'.
      
           05  WS-REPORT-DELETE-SW         PIC X(01)     VALUE 'B'.
               88  BEGIN-REPORT-DELETE                   VALUE 'B'.
               88  REPORT-DELETE-DONE                    VALUE 'D'.
               
           05  WS-REPORT-RECORD-TYPE       PIC X(01)     VALUE 'A'.
               88 ACTIVE-AH-ON-LIFE-CLAIM                VALUE 'A'.   
               88 CERT-FLAG-INDICATOR                    VALUE 'F'.
               88 ACTIVE-LF-ON-LIFE-CLAIM                VALUE 'L'.
      
           05  WS-ERACCT-KEY-SW            PIC X(01)     VALUE SPACE.
               88  NEW-ERACCT-KEY                        VALUE 'N'.
               88  ERACCT-KEY-CHANGE                     VALUE 'C'.
      
           05  WS-PAID-LIFE-MSG-IND        PIC X(01)     VALUE SPACES.
               88  PAID-MSG-PRINTED                      VALUES 'A' 'L'.
               88  PAID-AH-MSG-PRINTED                   VALUE 'A'.
               88  PAID-LIFE-MSG-PRINTED                 VALUE 'L'.
      
           05  WS-WHICH-FLAG-IND           PIC X(01)     VALUE SPACES.
               88  MANUAL-FLAG                           VALUE 'M'.
               88  AUTO-FLAG                             VALUE 'A'.
061410               
061410     05  WS-ERENDR-KEY-SW            PIC X(01)     VALUE SPACE.
061410         88  NEW-ERENDR-KEY                        VALUE 'N'.
061410         88  ERENDR-KEY-CHANGE                     VALUE 'C'.               
      
           05  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.
      
           05  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE ZERO.
      
           05  WS-DETAIL-REPORT-TITLE      PIC X(45)     VALUE
               ' CERTS WITH REFUNDED COVERAGE AND OPEN CLAIM '.
      
           05  WS-MTD-LITERAL              PIC X(17)     VALUE
               'MONTH-TO-DATE    '.
      
           05  WS-FULL-DIS-LABEL.
               10  WS-DIS-LABEL-VAR        PIC X(10)     VALUE SPACES.
               10  WS-DIS-LABEL            PIC X(05)     VALUE
                   'DISAB'.
      
           05  WS-FULL-IU-LABEL.
               10  WS-IU-LABEL-VAR         PIC X(10)     VALUE SPACES.
               10  WS-IU-LABEL             PIC X(05)     VALUE
                   'UNEMP'.
      
           05  WS-FULL-GAP-LABEL.
               10  WS-GAP-LABEL-VAR        PIC X(10)     VALUE SPACES.
               10  WS-GAP-LABEL            PIC X(05)     VALUE
                   'GAP  '.
052614
052614     05  WS-FULL-FAM-LABEL.
052614         10  WS-FAM-LABEL-VAR        PIC X(10)     VALUE SPACES.
052614         10  WS-FAM-LABEL            PIC X(05)     VALUE
052614             'FAM  '.
100518
022122     05  WS-FULL-BRV-LABEL.
022122         10  WS-BRV-LABEL-VAR        PIC X(10)     VALUE SPACES.
022122         10  WS-BRV-LABEL            PIC X(05)     VALUE
022122             'BRV  '.
022122
022122     05  WS-FULL-HOS-LABEL.
022122         10  WS-HOS-LABEL-VAR        PIC X(10)     VALUE SPACES.
022122         10  WS-HOS-LABEL            PIC X(05)     VALUE
022122             'HOS  '.
100518
100518     05  WS-FULL-OTH-LABEL.
100518         10  WS-OTH-LABEL-VAR        PIC X(10)     VALUE SPACES.
100518         10  WS-OTH-LABEL            PIC X(05)     VALUE
100518             'OTH  '.
      
           05  WS-FULL-LIFE-LABEL.
               10  WS-LIFE-LABEL-VAR       PIC X(10)     VALUE SPACES.
               10  WS-LIFE-LABEL           PIC X(05)     VALUE
                   'LIFE '.
                   
           05  WS-PREV-AUDITOR             PIC X(04).
           
           05  WS-NO-ACTIVITY-MESSAGE.
               10  FILLER                  PIC X(48)    VALUE SPACES.
               10  FILLER                  PIC X(32)    VALUE
                   'THERE WAS NO ACTIVITY FOR TODAY.'.
               10  FILLER                  PIC X(53)    VALUE SPACES.

      
       01  WS-HEADING1.
           05  FILLER                      PIC X(01)     VALUE '1'.
           05  FILLER                      PIC X(41)     VALUE SPACES.
           05  WS-H1-TITLE                 PIC X(45)     VALUE SPACES.
           05  FILLER                      PIC X(34)     VALUE SPACES.
           05  WS-H1-REPORT-ID             PIC X(05)     VALUE 'EL593'.
           05  WS-H1-REPORT-ID2            PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE SPACES.
      
      
       01  WS-HEADING2.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(50)     VALUE SPACES.
           05  WS-H2-COMPANY-NAME          PIC X(30)     VALUE SPACES.
           05  FILLER                      PIC X(40)     VALUE SPACES.
           05  WS-H2-DATE                  PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE SPACES.
      
      
       01  WS-HEADING3.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(51)     VALUE SPACES.
           05  FILLER                      PIC X(17)     VALUE
               'ACTIVITY THROUGH '.
           05  WS-H3-THROUGH-DT            PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(42)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE 'PAGE'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACE.
      
      
       01  WS-HEADING4.
           05  FILLER                      PIC X(01)     VALUE '-'.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  FILLER                      PIC X(09)     VALUE SPACES.
           05  FILLER                      PIC X(15)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'EFF '.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'CLAIM'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(08)     VALUE
               'INCURRED'.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'PAID'.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'CANCEL'.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'CANCEL'.
           05  FILLER                      PIC X(15)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'REPORT'.
           05  FILLER                      PIC X(20)     VALUE SPACES.
      
      
       01  WS-HEADING4B.
           05  FILLER                      PIC X(01)     VALUE ' '.
           05  FILLER                      PIC X(05)     VALUE
               '  CSR'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'CLAIM'.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               ' CERT #'.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'NAME'.
           05  FILLER                      PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'STATE'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'DATE'.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'TYPE '.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               '  DATE'.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'THRU'.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'DATE '.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'REASON'.
           05  FILLER                      PIC X(15)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'REASON'.
           05  FILLER                      PIC X(14)     VALUE SPACES.
      
      
       01  WS-DETAIL1.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-AUDITOR               PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-CLAIM-NO              PIC X(07)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-CERT-NO               PIC X(11)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-CERT-LNAME            PIC X(15)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-ISS-STATE             PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-EFF-DT                PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-CLAIM-TYP-DESC        PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-INCURRED-DT           PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-PD-THRU-DT            PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-CLM-CANCEL-DT         PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-CANCEL-REASON         PIC X(12)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-REPORT-REASON         PIC X(32)     VALUE SPACES.

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

           PERFORM 1500-EDIT-CYCLE-DATE   THRU 1500-EXIT

           PERFORM OPEN-FILES             THRU OPEN-FILES-EXIT

           MOVE WS-DETAIL-REPORT-TITLE    TO WS-H1-TITLE
           MOVE COMPANY-NAME              TO WS-H2-COMPANY-NAME
           MOVE WS-CURRENT-DATE           TO WS-H2-DATE
           MOVE WS-EDITED-CYCLE-DT        TO WS-H3-THROUGH-DT

           PERFORM 3500-PRINT-HEADINGS  THRU 3500-EXIT

           MOVE DTE-CLASIC-COMPANY-CD     TO CL-COMPANY-CD
           START ELMSTR-INFILE KEY NOT < CL-COMPANY-CD
           END-START

           EVALUATE TRUE
           WHEN WS-ELMSTR-FILE-STATUS = '00'
               CONTINUE

           WHEN WS-ELMSTR-FILE-STATUS = '23'
               SET END-OF-ELMSTR TO TRUE
               MOVE '0NO ACTIVITY FOR EL593 REPORT'
                                          TO PRT
               PERFORM 3900-WRITE         THRU 3900-EXIT

           WHEN OTHER
              DISPLAY ' ELMSTR START ' WS-ELMSTR-FILE-STATUS
              PERFORM ABEND-PGM           THRU APS-EXIT
           END-EVALUATE

           IF NOT END-OF-ELMSTR 
               SORT SORT-FILE  ASCENDING KEY 
                              SORT-AUDITOR SORT-STATE SORT-ACCT-NO
                              SORT-CERT
               INPUT  PROCEDURE 2000-INPUT-PROCEDURE  THRU 2000-EXIT
               OUTPUT PROCEDURE 3000-OUTPUT-PROCEDURE THRU 3000-EXIT

               IF SORT-RETURN  NOT = ZEROS
                   MOVE 'INTERNAL SORT ABORTED'
                                          TO WS-ABEND-MESSAGE
                   MOVE '0101'            TO WS-RETURN-CODE
                   PERFORM ABEND-PGM      THRU APS-EXIT
               END-IF
           END-IF

           IF WS-SORT-RTN-COUNT = 0
               MOVE WS-NO-ACTIVITY-MESSAGE TO PRT
               PERFORM 3900-WRITE  THRU 3900-EXIT
           END-IF

           PERFORM CLOSE-FILES        THRU CLOSE-FILES-EXIT


           GOBACK.


       1500-EDIT-CYCLE-DATE.

           IF PARM-LENGTH = +0
               DISPLAY 'CYCLE DATE INPUT PARMS MISSING'
               PERFORM ABEND-PGM          THRU APS-EXIT
           END-IF

           MOVE PARM-CYCLE-DT             TO DC-GREG-DATE-CYMD
           MOVE 'L'                       TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION   THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1         TO WS-CYCLE-DT-BINARY
               MOVE PARM-CYCLE-DT         TO WS-CYCLE-DT
               MOVE WS-CYCLE-DT-MM        TO WS-EDITED-CYCLE-DT-MM
               MOVE WS-CYCLE-DT-DD        TO WS-EDITED-CYCLE-DT-DD
               MOVE WS-CYCLE-DT-CC        TO WS-EDITED-CYCLE-DT-CC
               MOVE WS-CYCLE-DT-YY        TO WS-EDITED-CYCLE-DT-YY
           ELSE
               DISPLAY 'INVALID CYCLE DATE ' PARM-CYCLE-DT
               PERFORM ABEND-PGM          THRU APS-EXIT
           END-IF


      ************* CREATE BEGINNING OF MONTH DATE FOR MTD PROCESSING
           MOVE PARM-CYCLE-DT             TO WS-START-DT
           SUBTRACT 1                     FROM WS-START-DT-YEAR
           MOVE '01'                      TO WS-START-DT-DAY01
           MOVE WS-START-DT               TO DC-GREG-DATE-CYMD
           MOVE 'L'                       TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION   THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1         TO WS-START-DT-BIN
           ELSE
               DISPLAY 'CONVERSION ERROR ON START-DT'
                                         WS-START-DT
               PERFORM ABEND-PGM          THRU APS-EXIT
           END-IF
      *************

           .
       1500-EXIT.
           EXIT.


       2000-INPUT-PROCEDURE.

           PERFORM 2010-BUILD-REPORT      THRU 2010-EXIT
               UNTIL END-OF-ELMSTR

           MOVE SPACES TO SORT-RECORD

           .
       2000-EXIT.
           EXIT.


       2010-BUILD-REPORT.

           READ ELMSTR-INFILE NEXT RECORD
               AT END
                   SET END-OF-ELMSTR      TO TRUE
                   GO TO 2010-EXIT
           END-READ

      *********************** COMPANY CODE OF HEX '04' = CID
      *********************** COMPANY CODE OF HEX '05' = DCC 
           IF (CL-COMPANY-CD = DTE-CLASIC-COMPANY-CD AND
               CL-LAST-MAINT-DT >= WS-START-DT-BIN AND
               CLAIM-IS-OPEN)
               CONTINUE
           ELSE
               GO TO 2010-EXIT
           END-IF

           MOVE LOW-VALUES                TO WS-CERT-CONTROL-KEY
           MOVE CL-COMPANY-CD             TO WS-COMPANY-CD
           MOVE CL-CERT-CARRIER           TO WS-CARRIER
           MOVE CL-CERT-GROUPING          TO WS-GROUPING
           MOVE CL-CERT-STATE             TO WS-STATE
           MOVE CL-CERT-ACCOUNT           TO WS-ACCOUNT
           MOVE CL-CERT-EFF-DT            TO WS-CERT-EFF-DT
           MOVE CL-CERT-PRIME             TO WS-CERT-PRIME
           MOVE CL-CERT-SFX               TO WS-CERT-SFX
           MOVE WS-CERT-CONTROL-KEY       TO CM-CONTROL-PRIMARY

           READ ELCERT-INFILE
           
           IF WS-ELCERT-FILE-STATUS NOT EQUAL '00'
               DISPLAY 'CERT NUMBER NOT FOUND ON ELCERT '
                                        CM-CERT-NO
               GO TO 2010-EXIT
           END-IF

022614*     MOVE SPACES                      TO WS-WHICH-FLAG-IND
022614*     MOVE LOW-VALUES                  TO EN-CONTROL-PRIMARY
022614*     MOVE WS-CERT-CONTROL-KEY         TO EN-CONTROL-PRIMARY
022614*     MOVE ZERO                        TO EN-SEQ-NO
022614*     START ERENDR-INFILE KEY NOT < EN-CONTROL-PRIMARY
022614*     if WS-ERENDR-FILE-STATUS = '10' or '23'
022614*        set end-of-erendr to true
022614*     else
022614*        if WS-ERENDR-FILE-STATUS = '00'
022614*           continue
022614*        else
022614*           DISPLAY 'INVALID KEY ON START ERENDR '
022614*                                      EN-CONTROL-PRIMARY
022614*             PERFORM ABEND-PGM        THRU APS-EXIT
022614*        end-if
022614*     end-if

022614*     SET NEW-ERENDR-KEY               TO TRUE
022614*     PERFORM 2100-READ-ERENDR         THRU 2100-EXIT
022614*         UNTIL ERENDR-KEY-CHANGE  OR
022614*               END-OF-ERENDR
022614*
022614*     IF MANUAL-FLAG
022614*         GO TO 2010-EXIT
022614*     END-IF

           
022614     MOVE SPACES                    TO WS-WHICH-FLAG-IND
           MOVE LOW-VALUES                TO CS-CONTROL-PRIMARY
           MOVE WS-CERT-CONTROL-KEY       TO WS-CRTT-CONTROL-KEY
           MOVE 'C'                       TO WS-CRTT-TRAILER-TYPE
           MOVE WS-CRTT-CONTROL           TO CS-CONTROL-PRIMARY
      
           READ ELCRTT-INFILE
           
           IF WS-ELCRTT-FILE-STATUS NOT EQUAL ZERO
               GO TO 2010-EXIT
           END-IF
       
011211     IF CS-REFUND-CLAIM-FLAG = '2' OR '3' OR '4'
               SET AUTO-FLAG TO TRUE
           ELSE
               GO TO 2010-EXIT
           END-IF

100518     IF CL-CLAIM-TYPE = 'L' OR 'O'
022614         MOVE CM-LF-CANCEL-DT       TO DC-BIN-DATE-1
022614                                       WS-CANCEL-DT-BIN
022614     ELSE
022614         MOVE CM-AH-CANCEL-DT       TO DC-BIN-DATE-1
022614                                       WS-CANCEL-DT-BIN
022614     END-IF
022614     MOVE SPACE                     TO DC-OPTION-CODE
022614     PERFORM 8500-DATE-CONVERSION   THRU 8500-EXIT
022614     
022614     IF NO-CONVERSION-ERROR
022614         MOVE DC-GREG-DATE-1-EDIT   TO WS-CANCEL-DT-GREG
022614     ELSE
022614         DISPLAY 'CONVERSION ERROR ON CANCEL DATE  CERT # '
022614                                 CL-CERT-NO 
022614         MOVE SPACES                TO WS-CANCEL-DT-GREG
022614     END-IF
022614     PERFORM 2500-ADD-CLAIM-NOTE THRU 2500-EXIT
022614
022614     MOVE SPACES TO WS-MESSAGE-SW
022614     IF (CL-NEXT-AUTO-PAY-DT NOT EQUAL LOW-VALUES)
022614         AND (CL-AUTO-PAY-SEQ GREATER THAN +0)
022614         PERFORM 2700-STOP-AUTOPAY THRU 2700-EXIT
022614         IF AUTOPAY-MSG OR AUTOPAY-MSG2
022614             PERFORM 2300-DETAIL-SETUP THRU 2300-EXIT
022614         END-IF
022614     END-IF
022614
022614     MOVE SPACES TO WS-MESSAGE-SW
022614     IF CL-PAID-THRU-DT > WS-CANCEL-DT-BIN
022614         SET PAID-DT-MSG TO TRUE
022614         PERFORM 2300-DETAIL-SETUP THRU 2300-EXIT
022614     END-IF
022614
           

           .
       2010-EXIT.
           EXIT.


022614* 2100-READ-ERENDR.
022614*
022614*     READ ERENDR-INFILE NEXT RECORD
022614*         AT END
022614*             SET END-OF-ERENDR        TO TRUE
022614*             GO TO 2100-EXIT
022614*     END-READ
022614*
022614*     IF EN-CONTROL-PRIMARY (1:33) = WS-CERT-CONTROL-KEY
022614*         CONTINUE
022614*     ELSE
022614*         SET ERENDR-KEY-CHANGE        TO TRUE
022614*         GO TO 2100-EXIT
022614*     END-IF
022614*
022614*******************CHECK FOR FLAG
022614*     IF EN-FLAG-CERT = '2' OR '3' OR '4'
022614*         SET CERT-FLAG-INDICATOR TO TRUE
022614*         SET MANUAL-FLAG TO TRUE
022614*     ELSE
022614*         GO TO 2100-EXIT
022614*     END-IF
022614* 
022614*     PERFORM 2300-DETAIL-SETUP THRU 2300-EXIT
022614*
022614*     .
022614* 2100-EXIT.
022614*     EXIT.
061410
061410
       2300-DETAIL-SETUP.

           MOVE LOW-VALUES                TO AM-CONTROL-PRIMARY
           MOVE WS-CERT-CONTROL-KEY       TO WS-ACCT-CONTROL
           MOVE WS-ACCT-CONTROL-KEY       TO AM-CONTROL-PRIMARY
022614     SET NEW-ERACCT-KEY             TO TRUE
022614     MOVE SPACES                    TO WS-ACCT-CSR
           START ERACCT-INFILE KEY NOT < AM-CONTROL-PRIMARY
               INVALID KEY
                   DISPLAY 'INVALID KEY ON START ERACCT '
                                          AM-CONTROL-PRIMARY
022614             SET ERACCT-KEY-CHANGE TO TRUE
           END-START
      
           PERFORM  UNTIL ERACCT-KEY-CHANGE  OR
                     END-OF-ERACCT
               READ ERACCT-INFILE NEXT RECORD
                   AT END
                       SET END-OF-ERACCT  TO TRUE
               END-READ
      
               IF AM-CONTROL-A = WS-ACCT-CONTROL-A
                   MOVE AM-CSR-CODE       TO WS-ACCT-CSR
               ELSE
                   SET ERACCT-KEY-CHANGE  TO TRUE
               END-IF
           END-PERFORM          
           
           MOVE SPACES                    TO WS-DETAIL1
           MOVE WS-ACCT-CSR               TO WS-D1-AUDITOR
           MOVE CL-CLAIM-NO               TO WS-D1-CLAIM-NO
           MOVE CL-CERT-NO                TO WS-D1-CERT-NO
           STRING CL-INSURED-LAST-NAME DELIMITED BY '  '
                  ',' DELIMITED BY SIZE
                  CL-INSURED-1ST-NAME DELIMITED BY '  '
                  INTO WS-D1-CERT-LNAME 
           END-STRING
           MOVE CL-CERT-STATE             TO WS-D1-ISS-STATE
      
           MOVE CM-CERT-EFF-DT            TO DC-BIN-DATE-1
           MOVE SPACE                     TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION   THRU 8500-EXIT
      
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-1-EDIT   TO WS-D1-EFF-DT
           ELSE
               DISPLAY 'CONVERSION ERROR ON EFF-DT CERT # '
                                      CL-CERT-NO
               MOVE SPACES                TO WS-D1-EFF-DT
           END-IF
      
           EVALUATE TRUE
           WHEN CL-CLAIM-TYPE = 'A'
               MOVE WS-DIS-LABEL          TO WS-D1-CLAIM-TYP-DESC
      
           WHEN CL-CLAIM-TYPE = 'G'
               MOVE WS-GAP-LABEL          TO WS-D1-CLAIM-TYP-DESC
      
           WHEN CL-CLAIM-TYPE = 'I'
               MOVE WS-IU-LABEL           TO WS-D1-CLAIM-TYP-DESC
052614
052614     WHEN CL-CLAIM-TYPE = 'F'
052614         MOVE WS-FAM-LABEL          TO WS-D1-CLAIM-TYP-DESC
100518
022122     WHEN CL-CLAIM-TYPE = 'B'
022122         MOVE WS-BRV-LABEL          TO WS-D1-CLAIM-TYP-DESC
022122
022122     WHEN CL-CLAIM-TYPE = 'H'
022122         MOVE WS-HOS-LABEL          TO WS-D1-CLAIM-TYP-DESC
100518
100518     WHEN CL-CLAIM-TYPE = 'O'
100518         MOVE WS-OTH-LABEL          TO WS-D1-CLAIM-TYP-DESC
      
           WHEN CL-CLAIM-TYPE = 'L'
               MOVE WS-LIFE-LABEL         TO WS-D1-CLAIM-TYP-DESC
      
           END-EVALUATE
      
           MOVE CL-INCURRED-DT            TO DC-BIN-DATE-1
           MOVE SPACE                     TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION   THRU 8500-EXIT
      
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-1-EDIT   TO WS-D1-INCURRED-DT
           ELSE
               DISPLAY 'CONVERSION ERROR ON INCURRED-DT CERT # '
                                      CL-CERT-NO
               MOVE SPACES                TO WS-D1-INCURRED-DT
           END-IF

           MOVE CL-PAID-THRU-DT           TO DC-BIN-DATE-1
           MOVE SPACE                     TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION   THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-1-EDIT   TO WS-D1-PD-THRU-DT
           ELSE
               DISPLAY 'CONVERSION ERROR ON PAID-THRU-DT CERT # '
                                      CL-CERT-NO
               MOVE SPACES                TO WS-D1-PD-THRU-DT
           END-IF

022614     MOVE WS-CANCEL-DT-GREG         TO WS-D1-CLM-CANCEL-DT
           
100518     IF CL-CLAIM-TYPE = 'L' OR 'O'
               IF CM-LF-CANCEL-DT > CM-LF-LOAN-EXPIRE-DT
                   MOVE 'EXPIRED  '       TO WS-D1-CANCEL-REASON
               ELSE
                   MOVE 'CANCELLED'       TO WS-D1-CANCEL-REASON
               END-IF
           ELSE
               IF CM-AH-CANCEL-DT > CM-AH-LOAN-EXPIRE-DT
                   MOVE 'EXPIRED  '       TO WS-D1-CANCEL-REASON
               ELSE
                   MOVE 'CANCELLED'       TO WS-D1-CANCEL-REASON
               END-IF
           END-IF
           
           MOVE 'DISAB REFUNDED,DISAB CLAIM STOPS' TO 
                              WS-D1-REPORT-REASON
022614
022614     IF AUTOPAY-MSG
022614          MOVE WS-AUTOPAY-MSG       TO WS-D1-REPORT-REASON
022614     END-IF.
022614
022614     IF AUTOPAY-MSG2
022614          MOVE WS-AUTOPAY-MSG2      TO WS-D1-REPORT-REASON
022614     END-IF.
022614
022614     IF PAID-DT-MSG
022614          MOVE WS-PAID-DT-MSG       TO WS-D1-REPORT-REASON
022614     END-IF.

           PERFORM 2900-RELEASE-SORT      THRU 2900-EXIT

           .
       2300-EXIT.
           EXIT.

022614
022614 2500-ADD-CLAIM-NOTE.
022614
022614     MOVE 'N' TO WS-T-NOTE-SW.
022614     PERFORM 2600-CHECK-FOR-EXISTING-MSG THRU 2600-EXIT.
022614
022614     IF T-NOTE-ALREADY-EXISTS
022614         GO TO 2500-EXIT
022614     END-IF.
022614
022614     MOVE LOW-VALUES TO ACTIVITY-TRAILERS.
022614     MOVE CL-CONTROL-PRIMARY TO AT-CONTROL-PRIMARY.
022614
022614     SUBTRACT 1 FROM  CL-TRAILER-SEQ-CNT.
022614     MOVE CL-TRAILER-SEQ-CNT TO AT-SEQUENCE-NO.
022614
022614     MOVE 'AT'               TO AT-RECORD-ID.
022614
022614     MOVE '6'                TO AT-TRAILER-TYPE.
022614     MOVE WS-CYCLE-DT-BINARY TO AT-RECORDED-DT
022614                                AT-GEN-INFO-LAST-MAINT-DT.
022614     MOVE 'E593'             TO AT-RECORDED-BY
022614                                AT-GEN-INFO-LAST-UPDATED-BY.
022614     MOVE 180000             TO AT-LAST-MAINT-HHMMSS.
022614     MOVE WS-CANCEL-DT-GREG  TO WS-CANCEL-NOTE-DATE.
022614     MOVE WS-CANCEL-NOTE     TO AT-INFO-LINE-1.
022614     MOVE 'T'                TO AT-INFO-TRAILER-TYPE.
022614
022614     WRITE ACTIVITY-TRAILERS
022614     IF WS-ELTRLR-FILE-STATUS = '22'
022614        DISPLAY ' ELTRLR DUP RECORD  '
022614     ELSE
022614        IF WS-ELTRLR-FILE-STATUS NOT = '00'
022614           DISPLAY ' ELTRLR  WRITE  ERROR - STATUS = '
022614                     WS-ELTRLR-FILE-STATUS
022614           GO TO 2500-EXIT
022614        END-IF
022614     END-IF
022614
022614     REWRITE CLAIM-MASTER
022614     IF WS-ELMSTR-FILE-STATUS NOT = '00'
022614         DISPLAY ' ELMSTR REWRITE ERROR - STATUS = '
022614             WS-ELMSTR-FILE-STATUS 
022614         GO TO 2500-EXIT
022614     END-IF.
022614
022614     .
022614 2500-EXIT.
022614     EXIT.
022614
022614
022614 2600-CHECK-FOR-EXISTING-MSG.
022614
022614     MOVE LOW-VALUES TO ACTIVITY-TRAILERS.
022614
022614     MOVE CL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY.
022614     MOVE +100                   TO  AT-SEQUENCE-NO.
022614
022614     START ELTRLR-INFILE KEY IS NOT < AT-CONTROL-PRIMARY.
022614     IF WS-ELTRLR-FILE-STATUS NOT = ZERO
022614         DISPLAY 'BAD START ON TRAILER REC FOR CLAIM ' 
022614                               CL-CLAIM-NO
022614         GO TO 2600-EXIT
022614     END-IF.
022614
022614 2600-READ-LOOP.
022614
022614     READ ELTRLR-INFILE NEXT RECORD.
022614
022614     IF WS-ELTRLR-FILE-STATUS NOT = ZERO
022614         DISPLAY 'BAD READ ON TRAILER REC FOR CLAIM '
022614                               CL-CLAIM-NO
022614         GO TO 2600-EXIT
022614     END-IF.
022614       
022614     IF CL-CONTROL-PRIMARY NOT EQUAL AT-CONTROL-PRIMARY (1:20)
022614          GO TO 2600-EXIT
022614     END-IF.
022614
022614     IF NOT GENERAL-INFO-TR
022614         GO TO 2600-READ-LOOP
022614     END-IF.
022614
022614     IF AT-INFO-TRAILER-TYPE = 'T'
022614        SET T-NOTE-ALREADY-EXISTS TO TRUE
022614        GO TO 2600-EXIT
022614     END-IF.
022614
022614     GO TO 2600-READ-LOOP.
022614
022614     .
022614 2600-EXIT.
022614     EXIT.
022614
022614
022614 2700-STOP-AUTOPAY.
022614
022614     MOVE LOW-VALUES TO ACTIVITY-TRAILERS.
022614
022614     MOVE CL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY.
022614     MOVE CL-AUTO-PAY-SEQ        TO  AT-SEQUENCE-NO.
022614
022614     READ ELTRLR-INFILE
022614
022614     IF WS-ELTRLR-FILE-STATUS NOT = ZERO
022614         DISPLAY 'AUTO PAY TRAILER NOT FOUND'
022614         GO TO 2700-EXIT
022614     END-IF.
022614
022614     SET AUTOPAY-MSG TO TRUE.
022614
022614     MOVE WS-CYCLE-DT-BINARY TO AT-TERMINATED-DT
022614                                AT-AUTO-PAY-LAST-MAINT-DT
022614     MOVE 'E593'             TO AT-AUTO-PAY-LAST-UPDATED-BY
022614     MOVE 180000             TO AT-LAST-MAINT-HHMMSS
022614
022614     REWRITE ACTIVITY-TRAILERS
022614     IF WS-ELTRLR-FILE-STATUS NOT = '00'
022614        DISPLAY ' ELTRLR  WRITE  ERROR - STATUS = '
022614                   WS-ELTRLR-FILE-STATUS
022614        SET AUTOPAY-MSG2 TO TRUE
022614        GO TO 2700-EXIT
022614     END-IF
022614
022614     MOVE ZEROS             TO CL-AUTO-PAY-SEQ.
022614     MOVE LOW-VALUES        TO CL-NEXT-AUTO-PAY-DT.
022614
022614     REWRITE CLAIM-MASTER
022614     IF WS-ELMSTR-FILE-STATUS NOT = '00'
022614         DISPLAY ' ELMSTR REWRITE ERROR - STATUS = '
022614                 WS-ELMSTR-FILE-STATUS
022614         GO TO 2700-EXIT
022614     END-IF.
022614     .
022614 2700-EXIT.
022614     EXIT.
022614
022614
       2900-RELEASE-SORT.

           RELEASE SORT-RECORD  FROM  WS-DETAIL1

           .
       2900-EXIT.
           EXIT.


       3000-OUTPUT-PROCEDURE.

           PERFORM 3020-RETURN-SORT THRU 3020-EXIT
               UNTIL END-OF-SORTFILE

           .
       3000-EXIT.
           EXIT.

       3020-RETURN-SORT.

           RETURN SORT-FILE
               AT END
                   SET END-OF-SORTFILE TO TRUE
                   GO TO 3020-EXIT
           END-RETURN
 
           ADD +1 TO WS-SORT-RTN-COUNT
                      
           IF WS-SORT-RTN-COUNT = 1
               MOVE SORT-AUDITOR TO WS-PREV-AUDITOR
           END-IF
           
           IF WS-LINE-COUNT >= WS-LINE-COUNT-MAX
               PERFORM 3500-PRINT-HEADINGS THRU 3500-EXIT
           END-IF

           MOVE SORT-RECORD               TO PRT
           IF SORT-AUDITOR <> WS-PREV-AUDITOR
               MOVE SORT-AUDITOR TO WS-PREV-AUDITOR
               MOVE '0'  TO  P-CTL
           END-IF
           PERFORM 3900-WRITE             THRU 3900-EXIT

           .
       3020-EXIT.
           EXIT.


       3500-PRINT-HEADINGS.

           MOVE 'A'                       TO WS-H1-REPORT-ID2
           MOVE WS-HEADING1               TO PRT
           PERFORM 3900-WRITE             THRU 3900-EXIT

           MOVE WS-HEADING2               TO PRT
           PERFORM 3900-WRITE             THRU 3900-EXIT

           ADD +1                         TO WS-PAGE
           MOVE WS-PAGE                   TO WS-H3-PAGE
           MOVE WS-HEADING3               TO PRT
           PERFORM 3900-WRITE             THRU 3900-EXIT

           MOVE WS-HEADING4               TO PRT
           PERFORM 3900-WRITE             THRU 3900-EXIT

           MOVE WS-HEADING4B              TO PRT
           PERFORM 3900-WRITE             THRU 3900-EXIT

           MOVE SPACES                    TO PRT
           MOVE ' '                       TO P-CTL
           PERFORM 3900-WRITE             THRU 3900-EXIT

           .
       3500-EXIT.
           EXIT.


       3900-WRITE.

           EVALUATE TRUE
           WHEN P-CTL = '1'
               MOVE +1                    TO WS-LINE-COUNT

           WHEN P-CTL = SPACE
               ADD +1                     TO WS-LINE-COUNT

           WHEN P-CTL = '0'
               ADD +2                     TO WS-LINE-COUNT

           WHEN OTHER
               ADD +3                     TO WS-LINE-COUNT
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

022614     OPEN I-O    ELMSTR-INFILE
022614                 ELTRLR-INFILE.
022614     OPEN INPUT  ELCERT-INFILE
                       ERACCT-INFILE
                       ELCRTT-INFILE
022614*                 ERENDR-INFILE

                OUTPUT PRNTR

           IF WS-ELMSTR-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELMSTR'
                                          TO WS-ABEND-MESSAGE
              MOVE WS-ELMSTR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM           THRU APS-EXIT
           END-IF

           IF WS-ELCERT-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELCERT'
                                          TO WS-ABEND-MESSAGE
              MOVE WS-ELCERT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM           THRU APS-EXIT
           END-IF
      
           IF WS-ERACCT-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ERACCT'
                                          TO WS-ABEND-MESSAGE
              MOVE WS-ERACCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM           THRU APS-EXIT
           END-IF
      
           IF WS-ELCRTT-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELCRTT'
                                          TO WS-ABEND-MESSAGE
              MOVE WS-ELCRTT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM           THRU APS-EXIT
           END-IF
022614
022614     IF WS-ELTRLR-FILE-STATUS  = '00' OR '97'
022614        CONTINUE
022614     ELSE
022614        MOVE 'OPEN ERROR OCCURRED ON ELTRLR'
022614                                        TO WS-ABEND-MESSAGE
022614        MOVE WS-ERENDR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
022614        PERFORM ABEND-PGM               THRU APS-EXIT
022614     END-IF
061410
022614*     IF WS-ERENDR-FILE-STATUS  = '00' OR '97'
022614*        CONTINUE
022614*     ELSE
022614*        MOVE 'OPEN ERROR OCCURRED ON ERENDR'
022614*                                        TO WS-ABEND-MESSAGE
022614*        MOVE WS-ERENDR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
022614*        PERFORM ABEND-PGM               THRU APS-EXIT
022614*     END-IF
           
           .
       OPEN-FILES-EXIT.
           EXIT.
      
      
       CLOSE-FILES.
      
      
      
           CLOSE ELMSTR-INFILE
                 ELCERT-INFILE
                 ERACCT-INFILE
                 ELCRTT-INFILE
022614*           ERENDR-INFILE
                 PRNTR

            .
       CLOSE-FILES-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
ABEND.

ABEND.
ABEND.
BEND.
BEND.
ABEND.
