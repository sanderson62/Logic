       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 EL585F.


      *AUTHOR.     SUZAN VUKOV.

      * This program modified to correct select dates on ELTRLR 
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
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

           SELECT ELTRLR-INOUT       ASSIGN TO ELTRLR
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS AT-CONTROL-PRIMARY
                                     FILE STATUS IS
                                       WS-ELTRLR-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE
                                COPY ELCDTEFD.

       FD  ELMSTR-INFILE.
                                COPY ELCMSTR.

       FD  ELTRLR-INOUT.
                                COPY ELCTRLR.


       WORKING-STORAGE SECTION.
       01  LCP-ABND-CODE                   PIC S999 COMP   VALUE +519.

       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*     EL585  WORKING STORAGE   *'.
       77  FILLER  PIC X(32)   VALUE '********** V/M 2.008 ***********'.

       01  FILLER                          COMP-3.
           05  WS-LINE-COUNT               PIC S9(03)    VALUE +0.
           05  WS-LINE-COUNT-MAX           PIC S9(03)    VALUE +55.
           05  WS-PAGE                     PIC S9(05)    VALUE +0.

      ******* COUNTS
           05  WS-DLY-DIS-CLMS-PD-CNT      PIC S9(06)    VALUE +0.
121902     05  WS-DLY-IU-CLMS-PD-CNT       PIC S9(06)    VALUE +0.
           05  WS-DLY-LIFE-CLMS-PD-CNT     PIC S9(06)    VALUE +0.
           05  WS-DLY-DIS-DRFTS-CNT        PIC S9(06)    VALUE +0.
121902     05  WS-DLY-IU-DRFTS-CNT         PIC S9(06)    VALUE +0.
           05  WS-DLY-LIFE-DRFTS-CNT       PIC S9(06)    VALUE +0.
           05  WS-DLY-DIS-VOID-CNT         PIC S9(06)    VALUE +0.
121902     05  WS-DLY-IU-VOID-CNT          PIC S9(06)    VALUE +0.
           05  WS-DLY-LIFE-VOID-CNT        PIC S9(06)    VALUE +0.
           05  WS-DLY-DIS-OFFLN-CNT        PIC S9(06)    VALUE +0.
121902     05  WS-DLY-IU-OFFLN-CNT         PIC S9(06)    VALUE +0.
           05  WS-DLY-LIFE-OFFLN-CNT       PIC S9(06)    VALUE +0.

           05  WS-TOT-DIS-CLMS-PD-CNT      PIC S9(06)    VALUE +0.
121902     05  WS-TOT-IU-CLMS-PD-CNT       PIC S9(06)    VALUE +0.
           05  WS-TOT-LIFE-CLMS-PD-CNT     PIC S9(06)    VALUE +0.
           05  WS-TOT-DIS-DRFTS-CNT        PIC S9(06)    VALUE +0.
121902     05  WS-TOT-IU-DRFTS-CNT         PIC S9(06)    VALUE +0.
           05  WS-TOT-LIFE-DRFTS-CNT       PIC S9(06)    VALUE +0.
           05  WS-TOT-DIS-VOID-CNT         PIC S9(06)    VALUE +0.
121902     05  WS-TOT-IU-VOID-CNT          PIC S9(06)    VALUE +0.
           05  WS-TOT-LIFE-VOID-CNT        PIC S9(06)    VALUE +0.
           05  WS-TOT-DIS-OFFLN-CNT        PIC S9(06)    VALUE +0.
121902     05  WS-TOT-IU-OFFLN-CNT         PIC S9(06)    VALUE +0.
           05  WS-TOT-LIFE-OFFLN-CNT       PIC S9(06)    VALUE +0.


      ******* DOLLAR AMOUNTS
           05  WS-DLY-DIS-CLMS-PD-AMT      PIC S9(08)V99 VALUE +0.
121902     05  WS-DLY-IU-CLMS-PD-AMT       PIC S9(08)V99 VALUE +0.
           05  WS-DLY-LIFE-CLMS-PD-AMT     PIC S9(08)V99 VALUE +0.
           05  WS-DLY-DIS-VOID-AMT         PIC S9(08)V99 VALUE +0.
121902     05  WS-DLY-IU-VOID-AMT          PIC S9(08)V99 VALUE +0.
           05  WS-DLY-LIFE-VOID-AMT        PIC S9(08)V99 VALUE +0.
           05  WS-DLY-DIS-OFFLN-AMT        PIC S9(08)V99 VALUE +0.
121902     05  WS-DLY-IU-OFFLN-AMT         PIC S9(08)V99 VALUE +0.
           05  WS-DLY-LIFE-OFFLN-AMT       PIC S9(08)V99 VALUE +0.
           05  WS-DLY-DIS-NETPD-AMT        PIC S9(08)V99 VALUE +0.
121902     05  WS-DLY-IU-NETPD-AMT         PIC S9(08)V99 VALUE +0.
           05  WS-DLY-LIFE-NETPD-AMT       PIC S9(08)V99 VALUE +0.


           05  WS-TOT-DIS-CLMS-PD-AMT      PIC S9(08)V99 VALUE +0.
121902     05  WS-TOT-IU-CLMS-PD-AMT       PIC S9(08)V99 VALUE +0.
           05  WS-TOT-LIFE-CLMS-PD-AMT     PIC S9(08)V99 VALUE +0.
           05  WS-TOT-DIS-VOID-AMT         PIC S9(08)V99 VALUE +0.
121902     05  WS-TOT-IU-VOID-AMT          PIC S9(08)V99 VALUE +0.
           05  WS-TOT-LIFE-VOID-AMT        PIC S9(08)V99 VALUE +0.
           05  WS-TOT-DIS-OFFLN-AMT        PIC S9(08)V99 VALUE +0.
121902     05  WS-TOT-IU-OFFLN-AMT         PIC S9(08)V99 VALUE +0.
           05  WS-TOT-LIFE-OFFLN-AMT       PIC S9(08)V99 VALUE +0.
           05  WS-TOT-DIS-NETPD-AMT        PIC S9(08)V99 VALUE +0.
121902     05  WS-TOT-IU-NETPD-AMT         PIC S9(08)V99 VALUE +0.
           05  WS-TOT-LIFE-NETPD-AMT       PIC S9(08)V99 VALUE +0.

           05  WS-GRAND-TOT-CLMS-PD-CNT    PIC S9(06)    VALUE +0.
           05  WS-GRAND-TOT-DRFTS-CNT      PIC S9(06)    VALUE +0.
           05  WS-GRAND-TOT-VOID-CNT       PIC S9(06)    VALUE +0.
           05  WS-GRAND-TOT-OFFLN-CNT      PIC S9(06)    VALUE +0.

           05  WS-GRAND-TOT-CLMS-PD-AMT    PIC S9(08)V99 VALUE +0.
           05  WS-GRAND-TOT-VOID-AMT       PIC S9(08)V99 VALUE +0.
           05  WS-GRAND-TOT-OFFLN-AMT      PIC S9(08)V99 VALUE +0.
           05  WS-GRAND-TOT-NETPD-AMT      PIC S9(08)V99 VALUE +0.

           05  WS-AMT-PAID-CRITERIA        PIC S9(07)V99 VALUE
                                                           +10000.
           05  WS-AMT-PD-THIS-CLM          PIC S9(07)V99 VALUE +0.
           05  WS-OVER-SETAMT-CNT          PIC S9(06)    VALUE +0.

           05  WS-DRFTLST-STOP-PAY-AMT     PIC S9(07)V99 VALUE +0.

           05  WS-RETURN-CODE              PIC S9(03)    VALUE +0.
           05  WS-ZERO                     PIC S9(01)    VALUE +0.


       01  FILLER                          COMP SYNC.
           05  PGM-SUB                     PIC S9(04)    VALUE +585.

       01  FILLER.
           05  ABEND-CODE                  PIC X(04).
           05  ABEND-OPTION                PIC X(01).
           05  OLC-REPORT-NAME             PIC X(05)     VALUE 'EL585'.

           05  WS-CYCLE-DT-BINARY          PIC X(02)     VALUE SPACES.
           05  WS-FIX-SEL-DT-BINARY        PIC X(02)     VALUE SPACES.

           05  WS-PREV-CYCLE-DT-BINARY     PIC X(02)     VALUE SPACES.

           05  WS-CURR-MNTH-STRT-DT-BIN    PIC X(02)     VALUE SPACES.

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
               10  WS-EDITED-CYCLE-DT-YY   PIC X(02)     VALUE SPACES.

           05  WS-CURR-MNTH-STRT-DT.
               10  FILLER                  PIC X(06)     VALUE SPACES.
               10  WS-CURR-MNTH-DAY01      PIC X(02)     VALUE SPACES.

           05  WS-EOF1-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-ELMSTR                         VALUE 'Y'.

           05  WS-EOF2-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-ELTRLR                         VALUE 'Y'.

           05  WS-EOF3-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-SORTFILE                       VALUE 'Y'.

           05  WS-ELTRLR-FILE-STATUS       PIC X(02)     VALUE ZERO.
           05  WS-ELMSTR-FILE-STATUS       PIC X(02)     VALUE ZERO.

           05  WS-ELTRLR-KEY-SW            PIC X(01)     VALUE SPACE.
               88  NEW-ELTRLR-KEY                        VALUE 'N'.
               88  ELTRLR-KEY-CHANGE                     VALUE 'C'.

           05  WS-STOP-PAY-SW              PIC X(01)     VALUE SPACE.
               88  THIS-IS-A-STOP-PAY                    VALUE 'Y'.
               88  STOP-PAY-SW-OFF                       VALUE 'N'.

           05  WS-CLAIM-COUNTED-SW         PIC X(01)     VALUE SPACE.
               88  CLAIM-NOT-COUNTED                     VALUE 'N'.
               88  CLAIM-COUNTED                         VALUE 'C'.

           05  WS-MTD-OR-DAILY-SW          PIC X(01)     VALUE SPACE.
               88  DAILY                                 VALUE 'D'.
               88  MONTH-TO-DATE                         VALUE 'M'.

           05  WS-DETAIL-SW                PIC X(01)     VALUE 'N'.
               88  DETAIL-SETUP-NOT-DONE                 VALUE 'N'.
               88  DETAIL-SETUP-DONE                     VALUE 'D'.

           05  WS-REPORT-DELETE-SW         PIC X(01)     VALUE 'B'.
               88  BEGIN-REPORT-DELETE                   VALUE 'B'.
               88  REPORT-DELETE-DONE                    VALUE 'D'.

           05  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.

           05  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE ZERO.

           05  WS-DETAIL-REPORT-TITLE      PIC X(42)     VALUE
               '  MONTH-TO-DATE CLAIMS PAID OVER $10,000  '.

           05  WS-SUMMARY-REPORT-TITLE     PIC X(42)     VALUE
               '  MONTH-TO-DATE CLAIMS ACTIVITY SUMMARY   '.

           05  WS-MTD-LITERAL              PIC X(17)     VALUE
               'MONTH-TO-DATE    '.

           05  WS-FULL-DIS-LABEL.
               10  WS-DIS-LABEL-VAR        PIC X(10)     VALUE SPACES.
               10  WS-DIS-LABEL            PIC X(17)     VALUE
                   'CREDIT DISABILITY'.

121902     05  WS-FULL-IU-LABEL.
121902         10  WS-IU-LABEL-VAR         PIC X(10)     VALUE SPACES.
121902         10  WS-IU-LABEL             PIC X(17)     VALUE
121902             'DCC UNEMPLOYMENT '.

           05  WS-FULL-LIFE-LABEL.
               10  WS-LIFE-LABEL-VAR       PIC X(10)     VALUE SPACES.
               10  WS-LIFE-LABEL           PIC X(17)     VALUE
                   'CREDIT LIFE      '.

121902     05  WS-GRAND-TOTALS-LABEL.
121902         10  FILLER                  PIC X(08)     VALUE
121902             '******* '.
121902         10  WS-GT-CLIENT-ID         PIC X(03)     VALUE
121902             'CID'.
121902         10  FILLER                  PIC X(16)     VALUE
121902         ' GRAND TOTALS   '.

       01  WS-HEADING1.
           05  FILLER                      PIC X(01)     VALUE '1'.
           05  FILLER                      PIC X(43)     VALUE SPACES.
           05  WS-H1-TITLE                 PIC X(42)     VALUE SPACES.
           05  FILLER                      PIC X(35)     VALUE SPACES.
           05  WS-H1-REPORT-ID             PIC X(05)     VALUE 'EL585'.
           05  FILLER                      PIC X(07)     VALUE SPACES.


       01  WS-HEADING2.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(46)     VALUE SPACES.
           05  WS-H2-COMPANY-NAME          PIC X(30)     VALUE SPACES.
           05  FILLER                      PIC X(44)     VALUE SPACES.
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
           05  FILLER                      PIC X(15)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               'CLAIM #'.
           05  FILLER                      PIC X(09)     VALUE SPACES.
           05  FILLER                      PIC X(30)     VALUE
               'CLAIMANT NAME                 '.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(10)     VALUE
               ' POLICY # '.
           05  FILLER                      PIC X(09)     VALUE SPACES.
           05  FILLER                      PIC X(17)     VALUE
               ' CLAIM TYPE      '.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  FILLER                      PIC X(11)     VALUE
               'AMOUNT PAID'.
           05  FILLER                      PIC X(15)     VALUE SPACES.


       01  WS-DETAIL1.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(15)     VALUE SPACES.
           05  WS-D1-CLAIM-NO              PIC X(07)     VALUE SPACES.
           05  FILLER                      PIC X(09)     VALUE SPACES.
           05  WS-D1-CLAIMANT-NM           PIC X(30)     VALUE SPACES.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  WS-D1-CERT-NO               PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(09)     VALUE SPACES.
           05  WS-D1-CLAIM-TYP-DESC        PIC X(17)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  WS-D1-AMT-PD-MTD            PIC ZZZ,ZZ9.99.
           05  FILLER                      PIC X(16)     VALUE SPACES.


      *************************
       01  WS-DRFTLST-HEADING1.
           05  FILLER                      PIC X(01)     VALUE '1'.
           05  FILLER                      PIC X(42)     VALUE SPACES.
           05  FILLER                      PIC X(47)     VALUE
               'DAILY DRAFT, STOP-PAY, AND OFFLINE TRANSACTIONS'.
           05  FILLER                      PIC X(31)     VALUE SPACES.
           05  WS-DRFTLST-H1-RPT-ID        PIC X(05)     VALUE 'EL585'.
           05  FILLER                      PIC X(07)     VALUE SPACES.

       01  WS-DRFTLST-HEADING3.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(55)     VALUE SPACES.
           05  FILLER                      PIC X(11)     VALUE
               'CYCLE DATE '.
           05  WS-DRFTLST-H3-DT            PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(44)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE 'PAGE'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-DRFTLST-H3-PAGE          PIC ZZ9.
           05  FILLER                      PIC X(04)     VALUE SPACE.


       01  WS-DRFTLST-HEADING4.
           05  FILLER                      PIC X(01)     VALUE '-'.
050604     05  FILLER                      PIC X(07)     VALUE
050604         'DRAFT #'.
050604     05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(12)     VALUE
               'DRAFT AMOUNT'.
050604     05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(10)     VALUE
               'ENTRY TYPE'.
050604     05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               'CLAIM #'.
050604     05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(10)     VALUE
               ' POLICY # '.
050604     05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(30)     VALUE
               'CLAIMANT NAME                 '.
050604     05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(30)     VALUE
               'PAYEE                         '.
050604     05  FILLER                      PIC X(01)     VALUE SPACES.
050604     05  FILLER                      PIC X(10)     VALUE
050604         'ACCOUNT NO'.


       01  WS-DRFTLST-DETAIL.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  WS-DRFTLST-CHECK-NO         PIC X(07)     VALUE SPACES.
050604     05  FILLER                      PIC X(02)     VALUE SPACES.
           05  WS-DRFTLST-DRFTAMT          PIC ZZZ,ZZ9.99-.
050604     05  FILLER                      PIC X(04)     VALUE SPACES.
           05  WS-DRFTLST-ENTRY-TYPE       PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  WS-DRFTLST-CLAIM-NO         PIC X(07)     VALUE SPACES.
050604     05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-DRFTLST-CERT-NO          PIC X(10)     VALUE SPACES.
050604     05  FILLER                      PIC X(02)     VALUE SPACES.
           05  WS-DRFTLST-CLAIMANT-NM      PIC X(30)     VALUE SPACES.
050604     05  FILLER                      PIC X(02)     VALUE SPACES.
           05  WS-DRFTLST-PAYEE            PIC X(30)     VALUE SPACES.
050604     05  FILLER                      PIC X(01)     VALUE SPACES.
050604     05  WS-DRFTLST-ACCT-NO          PIC X(10)     VALUE SPACES.

      *************************


       01  WS-SUMMARY-HEADING4.
           05  FILLER                      PIC X(01)     VALUE '-'.
           05  FILLER                      PIC X(29)     VALUE SPACES.
           05  FILLER                      PIC X(14)     VALUE
               ' GROSS TOTAL  '.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(08)     VALUE
               '# CLAIMS'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(08)     VALUE
               '# DRAFTS'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(15)     VALUE
               '        TOTAL  '.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               ' # STOP'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(14)     VALUE
               '       TOTAL  '.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(09)     VALUE
               '# OFFLINE'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(14)     VALUE
               '       NET    '.


       01  WS-SUMMARY-HEADING5.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(29)     VALUE SPACES.
           05  FILLER                      PIC X(14)     VALUE
               ' PAID CLAIMS  '.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(08)     VALUE
               '  PAID  '.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(08)     VALUE
               ' ISSUED '.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(15)     VALUE
               '      STOP-PAYS'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               '  PAYS '.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(14)     VALUE
               '      OFFLINE '.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(09)     VALUE
               '  TRANS  '.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(14)     VALUE
               '   PAID CLAIMS'.



       01  WS-SUMMARY-LINEX.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  WS-SUMMARY-LABEL            PIC X(27)     VALUE SPACES.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  WS-SUMMARY-TOT-PD-AMT       PIC $$Z,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02)     VALUE SPACE.
           05  WS-SUMMARY-CLAIMS-PD-CNT    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-SUMMARY-DRAFT-CNT        PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-SUMMARY-TOT-VOID-AMT     PIC $$Z,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-SUMMARY-VOID-CNT         PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  WS-SUMMARY-OFFLINE-AMT      PIC $$Z,ZZZ,ZZ9.99-.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  WS-SUMMARY-OFFLINE-CNT      PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-SUMMARY-NETPD-AMT          PIC $$Z,ZZZ,ZZ9.99.

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
           05  PARM-PREV-CYCLE-DT          PIC X(08)     VALUE SPACES.
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

010704     EVALUATE TRUE
010704     WHEN WS-ELMSTR-FILE-STATUS = '00'
010704         CONTINUE

010704     WHEN WS-ELMSTR-FILE-STATUS = '23'
010704         SET END-OF-ELMSTR TO TRUE

010704     WHEN OTHER
010704        DISPLAY ' ELMSTR START ' WS-ELMSTR-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
010704     END-EVALUATE

010704     IF NOT END-OF-ELMSTR 
               PERFORM 2000-INPUT-PROCEDURE  THRU 2000-EXIT
010704     END-IF

           DISPLAY 'REG DRAFTS CNT ' WS-DLY-DIS-DRFTS-CNT      
           DISPLAY 'VOID CNT ' WS-DLY-DIS-VOID-CNT    
           DISPLAY 'OFFLN CNT ' WS-DLY-DIS-OFFLN-CNT     
121902     DISPLAY '5/03 draft amt ' WS-DLY-DIS-CLMS-PD-AMT 
           DISPLAY '5/03 void amt '  WS-DLY-DIS-VOID-AMT   
           DISPLAY '5/03 offln amt ' WS-DLY-DIS-OFFLN-AMT 
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
               MOVE WS-CYCLE-DT-YY          TO WS-EDITED-CYCLE-DT-YY
           ELSE
               DISPLAY 'INVALID CYCLE DATE ' PARM-CYCLE-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF


           MOVE '20040531'                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1           TO WS-FIX-SEL-DT-BINARY
           ELSE
               DISPLAY 'INVALID SELECT DATE ' 
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF


      ************* CREATE BEGINNING OF MONTH DATE FOR MTD PROCESSING
           MOVE PARM-CYCLE-DT               TO WS-CURR-MNTH-STRT-DT
           MOVE '01'                        TO WS-CURR-MNTH-DAY01
           MOVE WS-CURR-MNTH-STRT-DT        TO DC-GREG-DATE-CYMD
           MOVE 'L'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1           TO WS-CURR-MNTH-STRT-DT-BIN
           ELSE
               DISPLAY 'CONVERSION ERROR ON MNTH-STRT-DT'
                                         WS-CURR-MNTH-STRT-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF
      *************

      ************* CREATE LAST MAINTENANCE CUTOFF DATE

           MOVE WS-CURR-MNTH-STRT-DT-BIN    TO DC-BIN-DATE-1
           MOVE -60                         TO DC-ELAPSED-DAYS
           MOVE ZERO                        TO DC-ELAPSED-MONTHS
           MOVE '6'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-2        TO WS-LAST-MAINT-CUTOFF-DT-BIN
           ELSE
               DISPLAY 'CONVERSION ERROR ON LAST-MAINT-CUTOFF-DT'
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF
      *************

           MOVE PARM-PREV-CYCLE-DT          TO DC-GREG-DATE-CYMD
           MOVE 'L'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1           TO WS-PREV-CYCLE-DT-BINARY
           ELSE
               DISPLAY 'INVALID PREVIOUS CYCLE DATE ' PARM-PREV-CYCLE-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF


           .
       1500-EXIT.
           EXIT.


       2000-INPUT-PROCEDURE.

           PERFORM 2010-BUILD-REPORT          THRU 2010-EXIT
               UNTIL END-OF-ELMSTR

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
121902*********************** COMPANY CODE OF HEX '05' = DCC 
           IF (CL-COMPANY-CD = DTE-CLASIC-COMPANY-CD AND
               CL-LAST-MAINT-DT >= WS-LAST-MAINT-CUTOFF-DT-BIN)
               CONTINUE
           ELSE
               GO TO 2010-EXIT
           END-IF

           MOVE LOW-VALUES                  TO AT-CONTROL-PRIMARY
           MOVE CL-CONTROL-PRIMARY          TO AT-CONTROL-PRIMARY (1:20)
           START ELTRLR-INOUT KEY NOT < AT-CONTROL-PRIMARY
               INVALID KEY
                   DISPLAY 'INVALID KEY ON START ELTRLR '
                                            CL-CONTROL-PRIMARY
                   PERFORM ABEND-PGM        THRU APS-EXIT
           END-START

           SET NEW-ELTRLR-KEY               TO TRUE
           SET CLAIM-NOT-COUNTED            TO TRUE
           MOVE ZEROS                       TO WS-AMT-PD-THIS-CLM
           PERFORM 2100-READ-ELTRLR         THRU 2100-EXIT
               UNTIL ELTRLR-KEY-CHANGE  OR
                     END-OF-ELTRLR

           .
       2010-EXIT.
           EXIT.


       2100-READ-ELTRLR.

           SET STOP-PAY-SW-OFF              TO TRUE
           READ ELTRLR-INOUT NEXT RECORD
               AT END
                   SET END-OF-ELTRLR        TO TRUE
                   GO TO 2100-EXIT
           END-READ


           IF AT-CONTROL-PRIMARY (1:20) = CL-CONTROL-PRIMARY
               CONTINUE
           ELSE
               SET ELTRLR-KEY-CHANGE        TO TRUE
               GO TO 2100-EXIT
           END-IF

      ****************** TYPE 2 = PAYMENT TRAILER
           IF AT-TRAILER-TYPE = '2'
               CONTINUE
           ELSE
               GO TO 2100-EXIT
           END-IF


           IF (AT-VOID-DT = LOW-VALUES OR
               AT-VOID-DT = SPACES)
               CONTINUE
           ELSE
               IF (AT-CHECK-WRITTEN-DT = LOW-VALUES OR
                   AT-CHECK-WRITTEN-DT = SPACES)
      ************ THIS IS A VOID  - THE CHECK IS NEVER PRINTED
                   GO TO 2100-EXIT
               END-IF

      ********** OTHERWISE IT'S A STOP-PAY
               PERFORM 2150-PROCESS-STOPPAY THRU 2150-EXIT
           END-IF

               IF (AT-CHECK-WRITTEN-DT = WS-CYCLE-DT-BINARY)
FIX                IF AT-PMT-SELECT-DT = WS-FIX-SEL-DT-BINARY    
FIX                   MOVE WS-FIX-SEL-DT-BINARY TO AT-PMT-SELECT-DT
FIX                   DISPLAY 'FIX MAY 3rd REC w/APRIL select dt ONLN'
FIX                   DISPLAY '         ' CL-CONTROL-PRIMARY  
FIX                   ADD +1 TO  WS-DLY-DIS-DRFTS-CNT
FIX                   ADD AT-AMOUNT-PAID TO WS-DLY-DIS-CLMS-PD-AMT
FIX   *               REWRITE ACTIVITY-TRAILERS
FIX                END-IF
               ELSE
                   GO TO 2100-EXIT
               END-IF
            
           IF AT-PAYMENT-ORIGIN = '3'
               PERFORM 2175-PROCESS-OFFLINE-DATA   THRU 2175-EXIT
           END-IF

           .
       2100-EXIT.
           EXIT.

       2150-PROCESS-STOPPAY.

               IF (AT-VOID-DT = WS-CYCLE-DT-BINARY)
FIX                IF AT-VOID-SELECT-DT = WS-FIX-SEL-DT-BINARY
FIX                   MOVE WS-FIX-SEL-DT-BINARY TO AT-VOID-SELECT-DT
FIX                   DISPLAY 'FIX MAY 3rd REC w/APRIL select dt VOID'
FIX                   DISPLAY '         ' CL-CONTROL-PRIMARY  
FIX                   ADD +1 TO WS-DLY-DIS-VOID-CNT
FIX                   ADD AT-AMOUNT-PAID TO WS-DLY-DIS-VOID-AMT  
FIX   *               REWRITE ACTIVITY-TRAILERS
FIX                END-IF
               ELSE
                   GO TO 2150-EXIT
               END-IF


           .
       2150-EXIT.
           EXIT.

       2175-PROCESS-OFFLINE-DATA.

               IF (AT-CHECK-WRITTEN-DT = WS-CYCLE-DT-BINARY)
FIX                IF AT-PMT-SELECT-DT = WS-FIX-SEL-DT-BINARY    
FIX                   MOVE WS-FIX-SEL-DT-BINARY TO AT-PMT-SELECT-DT
FIX                   DISPLAY 'FIX MAY 3rd REC w/APRIL select dt OFFLN'
FIX                   DISPLAY '         ' CL-CONTROL-PRIMARY  
FIX                   ADD +1 TO WS-DLY-DIS-OFFLN-CNT  
FIX                   ADD AT-AMOUNT-PAID TO WS-DLY-DIS-OFFLN-AMT      
FIX   *               REWRITE ACTIVITY-TRAILERS
FIX                END-IF
               ELSE
                   GO TO 2175-EXIT
               END-IF

           .
       2175-EXIT.
           EXIT.

       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.


       OPEN-FILES.

           OPEN INPUT  ELMSTR-INFILE
                       ELTRLR-INOUT 

           IF WS-ELMSTR-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELMSTR'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-ELMSTR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF


           IF WS-ELTRLR-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELTRLR'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-ELTRLR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF

           .
       OPEN-FILES-EXIT.
           EXIT.


       CLOSE-FILES.

           CLOSE ELMSTR-INFILE
                 ELTRLR-INOUT 

            .
       CLOSE-FILES-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
