       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 EL592.


      *AUTHOR.     AJRA.
      ******************************************************************
      *REMARKS.
      *        THIS PROGRAM CREATES A REPORT OF CLAIMS THAT HAVE CLOSED
      *        THAT ARE WAITING TO HAVE THEIR COVERAGES CANCELLED.
      *
      *     INPUT:   ELMSTR
      *              ELENDR
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 052407    2006062600001  AJRA  INITIAL PROGRAM
060409* 060409    2009010500002  AJRA  COUNT AH DEATH STATUS LIKE ACTIVE
101209* 101209    2009061500002  AJRA  FIX REPORT PROBLEMS
012510* 012510    2009061500002  AJRA  CHECK AUTO FLAG
030410* 031410    2009061500002  AJRA  CALL EL592OC TO CHECK FOR OPEN CLMS
051710* 051710    2009061500002  AJRA  EXCLUDE PAID THRU EXPIRE DT
022211* 022211    2011010400001  AJRA  REMOVE SPECIAL CODE FOR ALPHA ACCTS
051111* 051111    2011010400001  AJRA  REMOVE OB,CO ACCTS FOR WY,SC,NC
072412* 072412  IR2012071600001  PEMA  ADD FILE STATUS CHECK
072412* 072412  CR2012071700002  PEMA  BYPASS CERTAIN RECS FOR AHL ONLY
051713* 051713  CR2013021100004  PEMA  ADD NAME AND ADDRESS TO REPORT
082113* 082113  CR2012060700001  AJRA  ADD NAME AND ADDRESS TO REPORT B
010714* 010714  CR2013112100002  PEMA  BYPASS GOOD PENDING CANCELS
032114* 032114  CR2014013100001  PEMA  PRT HDINGS & MSG WHEN NO ACTIVITY
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
112917* 112917  IR2017111600001  TANA  REMOVE NC SPECIFIC CHECK
121317* 121317  CR2017120500001  PEMA  bypass mnthly addendums
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080720* 080720  IR2020071700002  TANA  DONT SHOW OB PREM TYPE
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

           SELECT ERENDR-INFILE      ASSIGN TO ERENDR
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS EN-CONTROL-PRIMARY
                                     FILE STATUS IS
                                       WS-ERENDR-FILE-STATUS.

           SELECT ELCERT-INFILE      ASSIGN TO ELCERT
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS CM-CONTROL-PRIMARY
                                     FILE STATUS IS
                                       WS-ELCERT-FILE-STATUS.


101209     SELECT ERACCT-INFILE      ASSIGN TO ERACCT
101209                               ORGANIZATION IS INDEXED
101209                               ACCESS IS DYNAMIC
101209                               RECORD KEY IS AM-CONTROL-PRIMARY
101209                               FILE STATUS IS
101209                                 WS-ERACCT-FILE-STATUS.
101209
012510     SELECT ELCRTT-INFILE      ASSIGN TO ELCRTT
012510                               ORGANIZATION IS INDEXED
012510                               ACCESS IS DYNAMIC
012510                               RECORD KEY IS CS-CONTROL-PRIMARY
012510                               FILE STATUS IS
012510                                 WS-ELCRTT-FILE-STATUS.
012510
051713     SELECT ELTRLR-INFILE      ASSIGN TO ELTRLR
051713                               ORGANIZATION IS INDEXED
051713                               ACCESS IS DYNAMIC
051713                               RECORD KEY IS AT-CONTROL-PRIMARY
051713                               FILE STATUS IS
051713                                 WS-ELTRLR-FILE-STATUS.
      
           SELECT PRNTR              ASSIGN TO SYS008-UR-1403-S-SYS008.

           SELECT PRNTR2             ASSIGN TO SYS010-UR-1403-S-SYS010.

           SELECT SORT-FILE          ASSIGN TO SYS001-UT-3380-S-SORTWK1.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE
                                COPY ELCDTEFD.

       FD  ELMSTR-INFILE.
                                COPY ELCMSTR.

       FD  ERENDR-INFILE.
                                COPY ERCENDR.

       FD  ELCERT-INFILE.
                                COPY ELCCERT.

101209 FD  ERACCT-INFILE.
101209                          COPY ERCACCT.
101209
012510 FD  ELCRTT-INFILE.
012510                          COPY ELCCRTT.
012510
051713 FD  ELTRLR-INFILE.
051713                          COPY ELCTRLR.
051713
       FD  PRNTR
                                COPY ELCPRTFD.

       FD  PRNTR2
           RECORDING MODE F                       
           LABEL RECORDS OMITTED                  
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.        
       01  PRT2.                                  
           12  P2-CTL               PIC  X.       
           12  P2-DATA              PIC  X(132).  


       SD  SORT-FILE.
       01  SORT-RECORD.
051713   03  sort-1st-part.
051713     05  sort-cc                     pic x.
           05  FILLER                      PIC X.
           05  SORT-AUDITOR                PIC X(04).
           05  FILLER                      PIC X(01).
           05  SORT-ACCT-NO                PIC X(10).
           05  FILLER                      PIC X(01).
           05  SORT-CERT                   PIC X(11).
           05  FILLER                      PIC X(01).
           05  SORT-STATE                  PIC X(02).
           05  FILLER                      PIC X(101).
051713   03  sort-2nd-part.
051713     05  SORT-ADDRESS                PIC X(123).


       WORKING-STORAGE SECTION.

       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*     EL592  WORKING STORAGE   *'.
       77  FILLER  PIC X(32)   VALUE '********** V/M 2.008 ***********'.

       01  FILLER                          COMP-3.
           05  WS-LINE-COUNT               PIC S9(03)    VALUE +0.
           05  WS-LINE-COUNT2              PIC S9(03)    VALUE +0.
           05  WS-LINE-COUNT-MAX           PIC S9(03)    VALUE +55.
           05  WS-PAGE                     PIC S9(05)    VALUE +0.
           05  WS-PAGE2                    PIC S9(05)    VALUE +0.
           05  WS-SORT-RTN-COUNT           PIC S9(05)    VALUE +0.

           05  WS-RETURN-CODE              PIC S9(03)    VALUE +0.
           05  WS-ZERO                     PIC S9(01)    VALUE +0.
101209     05  WS-SUB                      PIC S9(03) COMP-3 VALUE +0.  
NTTDel*    05  WS-ACCT-CSR                 PIC X(04).         
NTTIns 01  WS-ACCT-CSR                     PIC X(04).         


       01  FILLER                          COMP SYNC.
           05  PGM-SUB                     PIC S9(04)    VALUE +592.

       01  FILLER.
           05  ABEND-CODE                  PIC X(04).
           05  ABEND-OPTION                PIC X(01).
           05  OLC-REPORT-NAME             PIC X(05)     VALUE 'EL592'.

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

           05  WS-EOF1-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-ELMSTR                         VALUE 'Y'.

           05  WS-EOF2-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-ERENDR                         VALUE 'Y'.

           05  WS-EOF3-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-SORTFILE                       VALUE 'Y'.
101209
101209     05  WS-EOF4-SW                  PIC X(01)     VALUE SPACE.
101209         88  END-OF-ERACCT                         VALUE 'Y'.
               
           05  WS-CERT-CONTROL-KEY.
               10  WS-COMPANY-CD       PIC X.
               10  WS-CARRIER          PIC X.
               10  WS-GROUPING         PIC X(6).
               10  WS-STATE            PIC XX.
               10  WS-ACCOUNT          PIC X(10).
               10  WS-CERT-EFF-DT      PIC XX.
               10  WS-CERT-PRIME       PIC X(10).
               10  WS-CERT-SFX         PIC X.
101209
101209     05  WS-ACCT-CONTROL.
101209         10  WS-ACCT-CONTROL-KEY.
101209             15  WS-ACCT-COMPANY-CD   PIC X.
101209             15  WS-ACCT-CONTROL-A.
101209                 20  WS-ACCT-CARRIER  PIC X.
101209                 20  WS-ACCT-GROUPING PIC X(6).
101209                 20  WS-ACCT-STATE    PIC XX.
101209                 20  WS-ACCT-ACCOUNT  PIC X(10).
101209             15  WS-ACCT-EXP-DT       PIC XX.
101209         10  FILLER                   PIC X(11).
012510
012510     05  WS-CRTT-CONTROL.
012510         10  WS-CRTT-CONTROL-KEY     PIC X(33).
012510         10  WS-CRTT-TRAILER-TYPE    PIC X(1).

           05  WS-ERENDR-FILE-STATUS       PIC X(02)     VALUE ZERO.
           05  WS-ELMSTR-FILE-STATUS       PIC X(02)     VALUE ZERO.
           05  WS-ELCERT-FILE-STATUS       PIC X(02)     VALUE ZERO.
101209     05  WS-ERACCT-FILE-STATUS       PIC X(02)     VALUE ZERO.
012510     05  WS-ELCRTT-FILE-STATUS       PIC X(02)     VALUE ZERO.
051713     05  ws-eltrlr-file-status       pic x(02)     value zeros.

           05  WS-ERENDR-KEY-SW            PIC X(01)     VALUE SPACE.
               88  NEW-ERENDR-KEY                        VALUE 'N'.
               88  ERENDR-KEY-CHANGE                     VALUE 'C'.

           05  WS-DETAIL-SW                PIC X(01)     VALUE 'N'.
               88  DETAIL-SETUP-NOT-DONE                 VALUE 'N'.
               88  DETAIL-SETUP-DONE                     VALUE 'D'.

           05  WS-REPORT-DELETE-SW         PIC X(01)     VALUE 'B'.
               88  BEGIN-REPORT-DELETE                   VALUE 'B'.
               88  REPORT-DELETE-DONE                    VALUE 'D'.
               
           05  WS-REPORT-RECORD-TYPE       PIC X(01)     VALUE 'A'.
               88 ACTIVE-AH-ON-LIFE-CLAIM                VALUE 'A'.   
               88 CERT-FLAG-INDICATOR                    VALUE 'F'.
101209         88 ACTIVE-LF-ON-LIFE-CLAIM                VALUE 'L'.
101209
101209     05  WS-ERACCT-KEY-SW            PIC X(01)     VALUE SPACE.
101209         88  NEW-ERACCT-KEY                        VALUE 'N'.
101209         88  ERACCT-KEY-CHANGE                     VALUE 'C'.
101209
101209     05  WS-PAID-LIFE-MSG-IND        PIC X(01)     VALUE SPACES.
101209         88  PAID-MSG-PRINTED                      VALUES 'A' 'L'.
101209         88  PAID-AH-MSG-PRINTED                   VALUE 'A'.
101209         88  PAID-LIFE-MSG-PRINTED                 VALUE 'L'.
012510
012510     05  WS-WHICH-FLAG-IND           PIC X(01)     VALUE SPACES.
012510         88  MANUAL-FLAG                           VALUE 'M'.
012510         88  AUTO-FLAG                             VALUE 'A'.

           05  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.

           05  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE ZERO.

           05  WS-DETAIL-REPORT-TITLE      PIC X(42)     VALUE
               ' CERTS WITH COVERAGES THAT NEED CANCELLED '.

           05  WS-MTD-LITERAL              PIC X(17)     VALUE
               'MONTH-TO-DATE    '.

           05  WS-FULL-DIS-LABEL.
               10  WS-DIS-LABEL-VAR        PIC X(10)     VALUE SPACES.
101209         10  WS-DIS-LABEL            PIC X(05)     VALUE
101209             'DISAB'.

           05  WS-FULL-IU-LABEL.
               10  WS-IU-LABEL-VAR         PIC X(10)     VALUE SPACES.
101209         10  WS-IU-LABEL             PIC X(05)     VALUE
101209             'UNEMP'.

           05  WS-FULL-GAP-LABEL.
               10  WS-GAP-LABEL-VAR        PIC X(10)     VALUE SPACES.
101209         10  WS-GAP-LABEL            PIC X(05)     VALUE
101209             'GAP  '.
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
101209         10  WS-LIFE-LABEL           PIC X(05)     VALUE
101209             'LIFE '.
                   
           05  WS-PREV-AUDITOR             PIC X(04).

       01  WS-HEADING1.
           05  FILLER                      PIC X(01)     VALUE '1'.
           05  FILLER                      PIC X(43)     VALUE SPACES.
           05  WS-H1-TITLE                 PIC X(42)     VALUE SPACES.
           05  FILLER                      PIC X(35)     VALUE SPACES.
           05  WS-H1-REPORT-ID             PIC X(05)     VALUE 'EL592'.
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
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE SPACES.
101209     05  FILLER                      PIC X(09)     VALUE SPACES.
101209     05  FILLER                      PIC X(15)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'EFF '.
101209     05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'CLAIM'.
101209     05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(08)     VALUE
               'INCURRED'.
101209     05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'PAID'.
101209     05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'CLOSE'.
101209     05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'CLOSE'.
           05  FILLER                      PIC X(14)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'REPORT'.
101209     05  FILLER                      PIC X(18)     VALUE SPACES.


       01  WS-HEADING4B.
           05  FILLER                      PIC X(01)     VALUE ' '.
           05  FILLER                      PIC X(07)     VALUE
               '  CSR  '.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'ACCT'.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               ' CERT #'.
101209     05  FILLER                      PIC X(05)     VALUE SPACES.
101209     05  FILLER                      PIC X(04)     VALUE
101209         'NAME'.
101209     05  FILLER                      PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'STATE'.
101209     05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'DATE'.
101209     05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'TYPE '.
101209     05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               '  DATE'.
101209     05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'THRU'.
101209     05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'DATE '.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'REASON'.
           05  FILLER                      PIC X(14)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'REASON'.
101209     05  FILLER                      PIC X(12)     VALUE SPACES.


       01  WS-DETAIL1.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-AUDITOR               PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-ACCOUNT-NO            PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-CERT-NO               PIC X(11)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
101209     05  WS-D1-CERT-LNAME            PIC X(15)     VALUE SPACES.
101209     05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-ISS-STATE             PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-EFF-DT                PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
101209     05  WS-D1-CLAIM-TYP-DESC        PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
101209     05  WS-D1-INCURRED-DT           PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
101209     05  WS-D1-PD-THRU-DT            PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
101209     05  WS-D1-CLM-CLOSE-DT          PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-CLOSE-REASON          PIC X(12)     VALUE SPACES.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-REPORT-REASON         PIC X(29)     VALUE SPACES.

051713 01  ws-detail2.
051713     05  filler                      pic x(10)  value spaces.
051713     05  ws-d2-name-and-address      pic x(123) value spaces.

      *              *************
      *              ELCDTECX: LAYOUT FOR DISK-DATE FILE
                     COPY ELCDTECX.


                     COPY ELCDTEVR.

      *              *************
      *              ELCDATE: LAYOUT OF DATA PASSED TO DATE CONV RTN
                     COPY ELCDATE.

030410 01  CHECK-CLAIM-PASS.
030410     05  CHECK-CLAIM-KEY.
030410         10  CHECK-CLAIM-COMPANY       PIC X.
030410         10  CHECK-CLAIM-CERT-NO       PIC X(11).
030410     05  CHECK-CLAIM-ACCT.
030410         10  CHECK-CLAIM-CARRIER       PIC X.
030410         10  CHECK-CLAIM-GROUP         PIC X(6).
030410         10  CHECK-CLAIM-STATE         PIC X(2).
030410         10  CHECK-CLAIM-ACCOUNT       PIC X(10).
030410         10  CHECK-CLAIM-EFF-DT        PIC X(2).
030410     05  CHECK-CLAIM-OPEN              PIC X.         

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

           PERFORM 1500-EDIT-CYCLE-DATE       THRU 1500-EXIT

           PERFORM OPEN-FILES                 THRU OPEN-FILES-EXIT

           MOVE WS-DETAIL-REPORT-TITLE        TO WS-H1-TITLE
           MOVE COMPANY-NAME                  TO WS-H2-COMPANY-NAME
           MOVE WS-CURRENT-DATE               TO WS-H2-DATE
           MOVE WS-EDITED-CYCLE-DT            TO WS-H3-THROUGH-DT

           MOVE DTE-CLASIC-COMPANY-CD         TO CL-COMPANY-CD
           START ELMSTR-INFILE KEY NOT < CL-COMPANY-CD
           END-START

           EVALUATE TRUE
           WHEN WS-ELMSTR-FILE-STATUS = '00'
               CONTINUE

           WHEN WS-ELMSTR-FILE-STATUS = '23'
               SET END-OF-ELMSTR TO TRUE
               MOVE '0NO ACTIVITY FOR EL591 REPORT'
                                           TO PRT
               PERFORM 3900-WRITE          THRU 3900-EXIT

           WHEN OTHER
              DISPLAY ' ELMSTR START ' WS-ELMSTR-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-EVALUATE

032114     PERFORM 3500-PRINT-HEADINGS        THRU 3500-EXIT
032114     PERFORM 3550-PRINT-HEADINGS2       THRU 3550-EXIT

           IF NOT END-OF-ELMSTR 
               SORT SORT-FILE  ASCENDING KEY 
                              SORT-AUDITOR SORT-STATE SORT-ACCT-NO
                              SORT-CERT
               INPUT  PROCEDURE 2000-INPUT-PROCEDURE  THRU 2000-EXIT
               OUTPUT PROCEDURE 3000-OUTPUT-PROCEDURE THRU 3000-EXIT

               IF SORT-RETURN  NOT = ZEROS
                   MOVE 'INTERNAL SORT ABORTED'
                                           TO WS-ABEND-MESSAGE
                   MOVE '0101'             TO WS-RETURN-CODE
                   PERFORM ABEND-PGM       THRU APS-EXIT
               END-IF
           END-IF

           PERFORM CLOSE-FILES        THRU CLOSE-FILES-EXIT


           GOBACK.


       1500-EDIT-CYCLE-DATE.

           IF PARM-LENGTH = +0
               DISPLAY 'CYCLE DATE INPUT PARMS MISSING'
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF

           MOVE PARM-CYCLE-DT               TO DC-GREG-DATE-CYMD
           MOVE 'L'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

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
           END-IF

      ************* CREATE BEGINNING OF MONTH DATE FOR MTD PROCESSING
           MOVE PARM-CYCLE-DT               TO WS-START-DT
           SUBTRACT 1                       FROM WS-START-DT-YEAR
           MOVE '01'                        TO WS-START-DT-DAY01
           MOVE WS-START-DT                 TO DC-GREG-DATE-CYMD
           MOVE 'L'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1           TO WS-START-DT-BIN
           ELSE
               DISPLAY 'CONVERSION ERROR ON START-DT'
                                         WS-START-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF
      *************

           .
       1500-EXIT.
           EXIT.


       2000-INPUT-PROCEDURE.

           PERFORM 2010-BUILD-REPORT          THRU 2010-EXIT
               UNTIL END-OF-ELMSTR

           MOVE SPACES TO SORT-RECORD

           .
       2000-EXIT.
           EXIT.


       2010-BUILD-REPORT.

           READ ELMSTR-INFILE NEXT RECORD
               AT END
                   SET END-OF-ELMSTR        TO TRUE
                   GO TO 2010-EXIT
           END-READ

      *********************** COMPANY CODE OF HEX '04' = CID
      *********************** COMPANY CODE OF HEX '05' = DCC 
072412*********************** COMPANY CODE OF HEX '06' = AHL

072412     if cl-company-cd > dte-clasic-company-cd
072412        set end-of-elmstr to true
072412        go to 2010-exit
072412     end-if

           IF (CL-COMPANY-CD = DTE-CLASIC-COMPANY-CD)
               and (CL-LAST-MAINT-DT >= WS-START-DT-BIN)
               CONTINUE
           ELSE
               GO TO 2010-EXIT
           END-IF

           MOVE LOW-VALUES                  TO WS-CERT-CONTROL-KEY
           MOVE CL-COMPANY-CD               TO WS-COMPANY-CD
           MOVE CL-CERT-CARRIER             TO WS-CARRIER
           MOVE CL-CERT-GROUPING            TO WS-GROUPING
           MOVE CL-CERT-STATE               TO WS-STATE
           MOVE CL-CERT-ACCOUNT             TO WS-ACCOUNT
           MOVE CL-CERT-EFF-DT              TO WS-CERT-EFF-DT
           MOVE CL-CERT-PRIME               TO WS-CERT-PRIME
           MOVE CL-CERT-SFX                 TO WS-CERT-SFX
           MOVE WS-CERT-CONTROL-KEY         TO CM-CONTROL-PRIMARY
           
           READ ELCERT-INFILE
           
           IF WS-ELCERT-FILE-STATUS NOT EQUAL '00'
               DISPLAY 'CERT NUMBER NOT FOUND ON ELCERT '
                                        CM-CERT-NO
               GO TO 2010-EXIT
           END-IF
           
012510     IF CL-TYPE-RESCISSION OR CL-TYPE-REFORMATION OR
012510        CL-TYPE-REF-TO-RES
012510         GO TO 2010-EXIT
012510     END-IF           

072412     if dte-client = 'AHL'
072412        and (cl-paid-thru-dt = low-values or spaces)
072412        and (cl-last-close-dt = low-values or spaces)
072412        go to 2010-exit
072412     end-if

101209     MOVE SPACES                      TO WS-PAID-LIFE-MSG-IND

           IF CL-CLAIM-TYPE = 'L' AND CL-TOTAL-PAID-AMT > 0
060409       AND CM-AH-BENEFIT-CD > ZERO
060409        IF (CM-AH-POLICY-IS-ACTIVE
080720           OR CERT-PEND-CANCEL-ERROR
101209           OR (CM-AH-DEATH-CLAIM-APPLIED AND
080720               (NOT CM-O-B-COVERAGE) AND
101209               CM-AH-LOAN-EXPIRE-DT > CL-INCURRED-DT))
010714           and (not cert-cancelled-online)
121317           and (cm-entry-status <> 'M')
                  SET ACTIVE-AH-ON-LIFE-CLAIM TO TRUE
                  PERFORM 2300-DETAIL-SETUP THRU 2300-EXIT
101209            SET PAID-AH-MSG-PRINTED TO TRUE
              END-IF
           END-IF
101209
022211*     IF CL-CLAIM-TYPE = 'L' AND CL-TOTAL-PAID-AMT > 0
022211*        IF CL-CERT-ACCOUNT (10:1) >= 'A' AND 
022211*           CL-CERT-ACCOUNT (10:1) <= 'Z' AND
022211*          (CM-LF-POLICY-IS-ACTIVE OR 
022211*         (CM-LF-DEATH-CLAIM-APPLIED AND 
022211*          CM-LF-LOAN-EXPIRE-DT > CL-INCURRED-DT AND
022211*         (CM-LF-CANCEL-DT EQUAL SPACES OR LOW-VALUES)))
022211*           SET ACTIVE-LF-ON-LIFE-CLAIM TO TRUE
022211*           PERFORM 2300-DETAIL-SETUP THRU 2300-EXIT
022211*           SET PAID-LIFE-MSG-PRINTED TO TRUE
022211*       END-IF
022211*    END-IF
101209
101209     IF PAID-LIFE-MSG-PRINTED
101209         GO TO 2010-EXIT
101209     END-IF
101209
100518     IF (CL-CLAIM-TYPE = 'L' OR 'O') AND CL-TOTAL-PAID-AMT > 0
101209        IF CM-LF-BENEFIT-CD > ZERO AND 
051111           CL-CERT-ACCOUNT (9:2) <> 'OB' AND
051111           CL-CERT-ACCOUNT (9:2) <> 'CO' AND
101209          (CM-STATE = 'SC' OR 'WY') AND
121317          (cm-entry-status <> 'M') and
101209          (CM-LF-POLICY-IS-ACTIVE OR 
101209          (CM-LF-DEATH-CLAIM-APPLIED AND 
101209           CM-LF-LOAN-EXPIRE-DT > CL-INCURRED-DT AND
101209          (CM-LF-CANCEL-DT EQUAL SPACES OR LOW-VALUES)))
101209            SET ACTIVE-LF-ON-LIFE-CLAIM TO TRUE
101209            PERFORM 2300-DETAIL-SETUP THRU 2300-EXIT
101209            SET PAID-LIFE-MSG-PRINTED TO TRUE
101209        END-IF
101209     END-IF
101209
112917*    IF CL-CLAIM-TYPE = 'L' AND CL-TOTAL-PAID-AMT > 0
112917*       IF CM-LF-BENEFIT-CD > ZERO AND
112917*          CL-CERT-ACCOUNT (9:2) <> 'OB' AND
112917*          CL-CERT-ACCOUNT (9:2) <> 'CO' AND
112917*         (CM-STATE = 'NC') AND
112917*         (CM-LF-POLICY-IS-ACTIVE OR
112917*         (CM-LF-DEATH-CLAIM-APPLIED AND
112917*          CM-LF-LOAN-EXPIRE-DT > CL-INCURRED-DT AND
112917*         (CM-LF-CANCEL-DT EQUAL SPACES OR LOW-VALUES))) AND
112917*          CL-INCURRED-DT < X'A481'
112917*           SET ACTIVE-LF-ON-LIFE-CLAIM TO TRUE
112917*           PERFORM 2300-DETAIL-SETUP THRU 2300-EXIT
112917*           SET PAID-LIFE-MSG-PRINTED TO TRUE
112917*       END-IF
112917*    END-IF
101209
101209     IF PAID-MSG-PRINTED
101209         GO TO 2010-EXIT
101209     END-IF

           IF CL-LAST-CLOSE-REASON > SPACES  AND
              CL-LAST-CLOSE-DT >= WS-START-DT-BIN
012510        AND CLAIM-IS-CLOSED
               CONTINUE
           ELSE
               GO TO 2010-EXIT
           END-IF

030410     MOVE 'N'                  TO CHECK-CLAIM-OPEN
030410     IF CM-CLAIM-ATTACHED-COUNT > 1
030410         MOVE CL-COMPANY-CD    TO CHECK-CLAIM-COMPANY
030410         MOVE CL-CERT-NO       TO CHECK-CLAIM-CERT-NO
030410         MOVE CM-CARRIER       TO CHECK-CLAIM-CARRIER
030410         MOVE CM-GROUPING      TO CHECK-CLAIM-GROUP
030410         MOVE CM-STATE         TO CHECK-CLAIM-STATE
030410         MOVE CM-ACCOUNT       TO CHECK-CLAIM-ACCOUNT
030410         MOVE CM-CERT-EFF-DT   TO CHECK-CLAIM-EFF-DT
030410         CALL 'EL592OC'   USING CHECK-CLAIM-PASS
030410     END-IF
030410     
030410     IF CHECK-CLAIM-OPEN EQUAL 'Y'
030410         GO TO 2010-EXIT
030410     END-IF               
030410
           IF CM-AH-POLICY-IS-ACTIVE OR CM-LF-POLICY-IS-ACTIVE
101209         OR (CM-AH-DEATH-CLAIM-APPLIED AND 
101209           CM-AH-LOAN-EXPIRE-DT > CL-INCURRED-DT)
               CONTINUE
           ELSE
               GO TO 2010-EXIT
           END-IF
           
051710     IF (CL-CLAIM-TYPE = 'L' AND
051710         CL-PAID-THRU-DT < CM-LF-LOAN-EXPIRE-DT) OR
051710        (CL-CLAIM-TYPE = 'A' AND
051710         CL-PAID-THRU-DT < CM-AH-LOAN-EXPIRE-DT)
051710           CONTINUE
051710     ELSE
051710           GO TO 2010-EXIT
051710     END-IF

012510     MOVE SPACES                      TO WS-WHICH-FLAG-IND
012510           
           MOVE LOW-VALUES                  TO EN-CONTROL-PRIMARY
           MOVE WS-CERT-CONTROL-KEY         TO EN-CONTROL-PRIMARY
           MOVE ZERO                        TO EN-SEQ-NO
           START ERENDR-INFILE KEY NOT < EN-CONTROL-PRIMARY
           END-START
072412     if WS-ERENDR-FILE-STATUS = '10' or '23'
072412        set end-of-erendr to true
072412     else
              if WS-ERENDR-FILE-STATUS not = '00'
                   DISPLAY 'INVALID KEY ON START ERENDR '
                      EN-CONTROL-PRIMARY ' ' ws-erendr-file-status
                   PERFORM ABEND-PGM        THRU APS-EXIT
              end-if
           end-if

           SET NEW-ERENDR-KEY               TO TRUE
           PERFORM 2100-READ-ERENDR         THRU 2100-EXIT
               UNTIL ERENDR-KEY-CHANGE  OR
                     END-OF-ERENDR

012510     IF MANUAL-FLAG
012510         GO TO 2010-EXIT
012510     END-IF
012510  
012510     MOVE LOW-VALUES                 TO CS-CONTROL-PRIMARY
012510     MOVE WS-CERT-CONTROL-KEY        TO WS-CRTT-CONTROL-KEY
012510     MOVE 'C'                        TO WS-CRTT-TRAILER-TYPE
012510     MOVE WS-CRTT-CONTROL            TO CS-CONTROL-PRIMARY
012510
012510     READ ELCRTT-INFILE
012510     
012510     IF WS-ELCRTT-FILE-STATUS NOT EQUAL ZERO
012510         GO TO 2010-EXIT
012510     END-IF
012510 
012510     IF CS-REFUND-CLAIM-FLAG = '1'
012510         SET CERT-FLAG-INDICATOR TO TRUE
012510         SET AUTO-FLAG TO TRUE
012510     ELSE
012510         GO TO 2010-EXIT
012510     END-IF
012510
012510     PERFORM 2300-DETAIL-SETUP THRU 2300-EXIT

           .
       2010-EXIT.
           EXIT.


       2100-READ-ERENDR.

           READ ERENDR-INFILE NEXT RECORD
               AT END
                   SET END-OF-ERENDR        TO TRUE
                   GO TO 2100-EXIT
           END-READ

           IF EN-CONTROL-PRIMARY (1:33) = WS-CERT-CONTROL-KEY
               CONTINUE
           ELSE
               SET ERENDR-KEY-CHANGE        TO TRUE
               GO TO 2100-EXIT
           END-IF

      ******************CHECK FOR FLAG
           IF EN-FLAG-CERT = 'Y'
               SET CERT-FLAG-INDICATOR TO TRUE
012510         SET MANUAL-FLAG TO TRUE
           ELSE
               GO TO 2100-EXIT
           END-IF
 
           PERFORM 2300-DETAIL-SETUP THRU 2300-EXIT

           .
       2100-EXIT.
           EXIT.


       2300-DETAIL-SETUP.

101209     MOVE LOW-VALUES                  TO AM-CONTROL-PRIMARY
101209     MOVE WS-CERT-CONTROL-KEY         TO WS-ACCT-CONTROL
101209     MOVE WS-ACCT-CONTROL-KEY         TO AM-CONTROL-PRIMARY
101209     START ERACCT-INFILE KEY NOT < AM-CONTROL-PRIMARY
101209         INVALID KEY
101209             DISPLAY 'INVALID KEY ON START ERACCT '
101209                                      AM-CONTROL-PRIMARY
101209             PERFORM ABEND-PGM        THRU APS-EXIT
101209     END-START
101209
101209     SET NEW-ERACCT-KEY               TO TRUE
101209     MOVE SPACES                      TO WS-ACCT-CSR
101209     PERFORM  UNTIL ERACCT-KEY-CHANGE  OR
101209               END-OF-ERACCT
101209         READ ERACCT-INFILE NEXT RECORD
101209             AT END
101209                 SET END-OF-ERACCT    TO TRUE
101209         END-READ
101209
101209         IF AM-CONTROL-A = WS-ACCT-CONTROL-A
101209             MOVE AM-CSR-CODE         TO WS-ACCT-CSR
101209         ELSE
101209             SET ERACCT-KEY-CHANGE    TO TRUE
101209         END-IF
101209     END-PERFORM          
101209     
           MOVE SPACES                      TO WS-DETAIL1
101209     MOVE WS-ACCT-CSR                 TO WS-D1-AUDITOR
           MOVE CM-ACCOUNT                  TO WS-D1-ACCOUNT-NO
           MOVE CL-CERT-NO                  TO WS-D1-CERT-NO
           STRING CL-INSURED-LAST-NAME DELIMITED BY '  '
                  ',' DELIMITED BY SIZE
                  CL-INSURED-1ST-NAME DELIMITED BY '  '
                  INTO WS-D1-CERT-LNAME 
           END-STRING
           MOVE CL-CERT-STATE               TO WS-D1-ISS-STATE

           MOVE CM-CERT-EFF-DT              TO DC-BIN-DATE-1
           MOVE SPACE                       TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
101209         MOVE DC-GREG-DATE-1-EDIT   TO WS-D1-EFF-DT
           ELSE
               DISPLAY 'CONVERSION ERROR ON EFF-DT CERT # '
                                      CL-CERT-NO
               MOVE SPACES                TO WS-D1-EFF-DT
           END-IF

121902     EVALUATE TRUE
121902     WHEN CL-CLAIM-TYPE = 'A'
               MOVE WS-DIS-LABEL            TO WS-D1-CLAIM-TYP-DESC

120103     WHEN CL-CLAIM-TYPE = 'G'
120103         MOVE WS-GAP-LABEL            TO WS-D1-CLAIM-TYP-DESC
120103
121902     WHEN CL-CLAIM-TYPE = 'I'
121902         MOVE WS-IU-LABEL             TO WS-D1-CLAIM-TYP-DESC
052614
052614     WHEN CL-CLAIM-TYPE = 'F'
052614         MOVE WS-FAM-LABEL            TO WS-D1-CLAIM-TYP-DESC
100518
022122     WHEN CL-CLAIM-TYPE = 'B'
022122         MOVE WS-BRV-LABEL            TO WS-D1-CLAIM-TYP-DESC
022122
022122     WHEN CL-CLAIM-TYPE = 'H'
022122         MOVE WS-HOS-LABEL            TO WS-D1-CLAIM-TYP-DESC
100518
100518     WHEN CL-CLAIM-TYPE = 'O'
100518         MOVE WS-OTH-LABEL            TO WS-D1-CLAIM-TYP-DESC

121902     WHEN CL-CLAIM-TYPE = 'L'
               MOVE WS-LIFE-LABEL           TO WS-D1-CLAIM-TYP-DESC

121902     END-EVALUATE

           MOVE CL-INCURRED-DT              TO DC-BIN-DATE-1
           MOVE SPACE                       TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
101209         MOVE DC-GREG-DATE-1-EDIT   TO WS-D1-INCURRED-DT
           ELSE
               DISPLAY 'CONVERSION ERROR ON INCURRED-DT CERT # '
                                      CL-CERT-NO
               MOVE SPACES                TO WS-D1-INCURRED-DT
           END-IF

           MOVE CL-PAID-THRU-DT             TO DC-BIN-DATE-1
           MOVE SPACE                       TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
101209         MOVE DC-GREG-DATE-1-EDIT   TO WS-D1-PD-THRU-DT
           ELSE
               DISPLAY 'CONVERSION ERROR ON PAID-THRU-DT CERT # '
                                      CL-CERT-NO
               MOVE SPACES                TO WS-D1-PD-THRU-DT
           END-IF

           MOVE CL-LAST-CLOSE-DT            TO DC-BIN-DATE-1
           MOVE SPACE                       TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
101209         MOVE DC-GREG-DATE-1-EDIT   TO WS-D1-CLM-CLOSE-DT
           ELSE
               DISPLAY 'CONVERSION ERROR ON LAST-CLOSE-DT CERT # '
                                      CL-CERT-NO
               MOVE SPACES                TO WS-D1-CLM-CLOSE-DT
           END-IF
           
           EVALUATE TRUE
           WHEN CL-LAST-CLOSE-REASON = '1'
101209         MOVE 'FINAL PMNT'    TO WS-D1-CLOSE-REASON
           WHEN CL-LAST-CLOSE-REASON = '2'
101209         MOVE 'CLAIM DENIED'  TO WS-D1-CLOSE-REASON
           WHEN CL-LAST-CLOSE-REASON = '3'
101209         MOVE 'AUTO CLOSE'    TO WS-D1-CLOSE-REASON
           WHEN CL-LAST-CLOSE-REASON = '4'
101209         MOVE 'MANUAL CLOSE'  TO WS-D1-CLOSE-REASON
           WHEN CL-LAST-CLOSE-REASON = 'C'
101209         MOVE 'BENEFIT CHG '  TO WS-D1-CLOSE-REASON
           WHEN CL-LAST-CLOSE-REASON = 'E'
101209         MOVE 'SETUP ERRORS'  TO WS-D1-CLOSE-REASON
           END-EVALUATE
           
           IF ACTIVE-AH-ON-LIFE-CLAIM
               MOVE 'ACTIVE A&H ON PAID LIFE CLAIM' TO 
                              WS-D1-REPORT-REASON
           ELSE
101209      IF ACTIVE-LF-ON-LIFE-CLAIM
101209         MOVE 'ACTIVE LIFE ON PAID LIFE CLM ' TO
101209                        WS-D1-REPORT-REASON
101209      ELSE           
012510        IF MANUAL-FLAG
               MOVE 'CERT FLAGGED,CLAIM NOW CLOSED' TO 
                              WS-D1-REPORT-REASON
012510        ELSE
012510          MOVE 'AUTO FLAGGED,CLAIM NOW CLOSED' TO
012510                        WS-D1-REPORT-REASON
012510        END-IF
101209      END-IF
           END-IF

051713     move spaces                 to ws-detail2
051713     move cl-control-primary     to at-control-primary
051713     move +1                     to at-sequence-no
051713     read eltrlr-infile
051713     if ws-eltrlr-file-status = '00'
051713        string at-mail-to-name ','
051713           at-address-line-1 ','
051713           at-address-line-2 ','
051713           at-city ','
051713           at-state ','
051713           at-zip
051713           delimited by '  ' into ws-d2-name-and-address
051713        end-string
051713        inspect ws-d2-name-and-address
051713           replacing all ',,' by ', '
051713     else
051713        display ' error-eltrlr-read ' ws-eltrlr-file-status ' '
051713           cl-claim-no
051713     end-if
051713
051713     move ws-detail1             to sort-1st-part
051713     move ws-d2-name-and-address to sort-2nd-part
           PERFORM 2900-RELEASE-SORT        THRU 2900-EXIT

           .
       2300-EXIT.
           EXIT.

       2900-RELEASE-SORT.

051713     release sort-record
051713*    RELEASE SORT-RECORD  FROM  WS-DETAIL1

           .
       2900-EXIT.
           EXIT.


       3000-OUTPUT-PROCEDURE.

           PERFORM 3020-RETURN-SORT THRU 3020-EXIT
               UNTIL END-OF-SORTFILE

032114     if ws-sort-rtn-count = zeros
032114        move '0'                 to prt
032114        move ' No Activity for this Reporting Period '
032114                                 to prt (10:39)
032114        perform 3900-write       thru 3900-exit
032114        move '0'                 to prt2
032114        move ' No Activity for this Reporting Period '
032114                                 to prt2 (10:39)
032114        perform 3950-write       thru 3950-exit
032114     end-if

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
032114*        PERFORM 3500-PRINT-HEADINGS        THRU 3500-EXIT
032114*        PERFORM 3550-PRINT-HEADINGS2       THRU 3550-EXIT
           END-IF
           
           IF SORT-AUDITOR <> WS-PREV-AUDITOR
               MOVE +0                TO WS-PAGE
               MOVE WS-LINE-COUNT-MAX TO WS-LINE-COUNT
           END-IF
           
           IF WS-LINE-COUNT >= WS-LINE-COUNT-MAX
               PERFORM 3500-PRINT-HEADINGS THRU 3500-EXIT
           END-IF

051713     move '0'                         to sort-cc
051713     MOVE SORT-1st-part               TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT
051713     move sort-2nd-part               to ws-d2-name-and-address
051713     move ws-detail2                  to prt
051713     perform 3900-write               thru 3900-exit

           IF WS-LINE-COUNT2 >= WS-LINE-COUNT-MAX
               PERFORM 3550-PRINT-HEADINGS2 THRU 3550-EXIT
           END-IF

051713     move ' '                         to sort-cc
051713     MOVE SORT-1st-part               TO PRT2
           IF SORT-AUDITOR <> WS-PREV-AUDITOR
               MOVE SORT-AUDITOR TO WS-PREV-AUDITOR
               MOVE '0'  TO  P2-CTL
           END-IF
           PERFORM 3950-WRITE               THRU 3950-EXIT

082113     move sort-2nd-part               to ws-d2-name-and-address
082113     move ws-detail2                  to prt2
082113     PERFORM 3950-WRITE               THRU 3950-EXIT

           .
       3020-EXIT.
           EXIT.


       3500-PRINT-HEADINGS.

           MOVE 'A'                         TO WS-H1-REPORT-ID2
           MOVE WS-HEADING1                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           MOVE WS-HEADING2                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           ADD +1                           TO WS-PAGE
           MOVE WS-PAGE                     TO WS-H3-PAGE
           MOVE WS-HEADING3                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           MOVE WS-HEADING4                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           MOVE WS-HEADING4B                TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

051713*    MOVE SPACES                      TO PRT
051713*    MOVE ' '                         TO P-CTL
051713*    PERFORM 3900-WRITE               THRU 3900-EXIT

           .
       3500-EXIT.
           EXIT.

       3550-PRINT-HEADINGS2.

           MOVE 'B'                         TO WS-H1-REPORT-ID2
           MOVE WS-HEADING1                 TO PRT2
           PERFORM 3950-WRITE               THRU 3950-EXIT

           MOVE WS-HEADING2                 TO PRT2
           PERFORM 3950-WRITE               THRU 3950-EXIT

           ADD +1                           TO WS-PAGE2
           MOVE WS-PAGE2                    TO WS-H3-PAGE
           MOVE WS-HEADING3                 TO PRT2
           PERFORM 3950-WRITE               THRU 3950-EXIT

           MOVE WS-HEADING4                 TO PRT2
           PERFORM 3950-WRITE               THRU 3950-EXIT

           MOVE WS-HEADING4B                TO PRT2
           PERFORM 3950-WRITE               THRU 3950-EXIT

           MOVE SPACES                      TO PRT2
           MOVE ' '                         TO P2-CTL
           PERFORM 3950-WRITE               THRU 3950-EXIT

           .
       3550-EXIT.
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


       3950-WRITE.

           EVALUATE TRUE
           WHEN P2-CTL = '1'
               MOVE +1                      TO WS-LINE-COUNT2

           WHEN P2-CTL = SPACE
               ADD +1                       TO WS-LINE-COUNT2

           WHEN P2-CTL = '0'
               ADD +2                       TO WS-LINE-COUNT2

           WHEN OTHER
               ADD +3                       TO WS-LINE-COUNT2
           END-EVALUATE


           WRITE PRT2

           .
       3950-EXIT.
           EXIT.


       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.


       OPEN-FILES.

           OPEN INPUT  ELMSTR-INFILE
                       ERENDR-INFILE
                       ELCERT-INFILE
101209                 ERACCT-INFILE
012510                 ELCRTT-INFILE
051713                 ELTRLR-INFILE
                OUTPUT PRNTR
                       PRNTR2

           IF WS-ELMSTR-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELMSTR'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-ELMSTR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF


           IF WS-ERENDR-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ERENDR'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-ERENDR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF

           IF WS-ELCERT-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELCERT'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-ELCERT-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF
101209
101209     IF WS-ERACCT-FILE-STATUS  = '00' OR '97'
101209        CONTINUE
101209     ELSE
101209        MOVE 'OPEN ERROR OCCURRED ON ERACCT'
101209                                        TO WS-ABEND-MESSAGE
101209        MOVE WS-ERACCT-FILE-STATUS      TO WS-ABEND-FILE-STATUS
101209        PERFORM ABEND-PGM               THRU APS-EXIT
101209     END-IF
012510
012510     IF WS-ELCRTT-FILE-STATUS  = '00' OR '97'
012510        CONTINUE
012510     ELSE
012510        MOVE 'OPEN ERROR OCCURRED ON ELCRTT'
012510                                        TO WS-ABEND-MESSAGE
012510        MOVE WS-ELCRTT-FILE-STATUS      TO WS-ABEND-FILE-STATUS
012510        PERFORM ABEND-PGM               THRU APS-EXIT
012510     END-IF
           
051713     IF WS-ELTRLR-FILE-STATUS  = '00' OR '97'
051713        CONTINUE
051713     ELSE
051713        MOVE 'OPEN ERROR OCCURRED ON ELTRLR'
051713                                        TO WS-ABEND-MESSAGE
051713        MOVE WS-ELTRLR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
051713        PERFORM ABEND-PGM               THRU APS-EXIT
051713     END-IF

           .
       OPEN-FILES-EXIT.
           EXIT.


       CLOSE-FILES.



           CLOSE ELMSTR-INFILE
                 ERENDR-INFILE
                 ELCERT-INFILE
101209           ERACCT-INFILE
012510           ELCRTT-INFILE
051713           eltrlr-infile
                 PRNTR
                 PRNTR2

            .
       CLOSE-FILES-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
