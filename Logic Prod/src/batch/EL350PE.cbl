       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL350.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.

      *REMARKS
      *   This program reads the ELACTQ file sequentially and
      *    scans for records with pending letters to be printed.
      *    Once a record has been selected the ELLETR file is read
      *    to find the letter to be used. Next, all the possible
      *    variables are resolved. Next, the program reads through
      *    the letter and substitutes the variables in the letter
      *    with the ones that were resolved. Finally, the finalized
      *    letter is written to the ELARCH file and the ELACTQ record
      *    is deleted providing there isn't any pending activity.
      *   This program was modeled from EL1523 AND EL1524.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERACCT assign to ERACCT
      *         ASSIGN TO "e:/cid/seqfiles/eracct.dat"
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERACCT-FILE-STATUS
                               RECORD KEY IS AM-CONTROL-PRIMARY.
           SELECT ELCNTL assign to ELCNTL
      *         ASSIGN TO "e:/cid/seqfiles/elcntl.dat"
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELCNTL-FILE-STATUS
                               RECORD KEY IS CF-CONTROL-PRIMARY.
      *    SELECT ELACTQ       ASSIGN TO ELACTQ
      *                        ACCESS IS DYNAMIC
      *                        ORGANIZATION IS INDEXED
      *                        FILE STATUS IS ELACTQ-FILE-STATUS
      *                        RECORD KEY IS AQ-CONTROL-PRIMARY.
           SELECT ELTRLR assign to ELTRLR
      *         ASSIGN TO "e:/cid/seqfiles/eltrlr.dat"
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELTRLR-FILE-STATUS
                               RECORD KEY IS AT-CONTROL-PRIMARY.
           SELECT ELARCH ASSIGN TO ELARCH
      *         ASSIGN TO "e:/cid/seqfiles/elarch.dat"
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELARCH-FILE-STATUS
                               RECORD KEY IS LA-CONTROL-PRIMARY.
           SELECT ELBENE ASSIGN TO ELBENE
      *         ASSIGN TO "e:/cid/seqfiles/elbene.dat"
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELBENE-FILE-STATUS
                               RECORD KEY IS BE-CONTROL-PRIMARY.
           SELECT ELCERT ASSIGN TO ELCERT
      *         ASSIGN TO "e:/cid/seqfiles/elcert.dat"
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELCERT-FILE-STATUS
                               RECORD KEY IS CM-CONTROL-PRIMARY.
           SELECT ELMSTR ASSIGN TO ELMSTR
      *         ASSIGN TO "e:/cid/seqfiles/elmstr.dat"
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELMSTR-FILE-STATUS
                               RECORD KEY IS CL-CONTROL-PRIMARY.
           SELECT ERCOMP ASSIGN TO ERCOMP
      *         ASSIGN TO "e:/cid/seqfiles/ercomp.dat"
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERCOMP-FILE-STATUS
                               RECORD KEY IS CO-CONTROL-PRIMARY.
           SELECT ELLETR ASSIGN TO ELLETR
      *         ASSIGN TO "e:/cid/seqfiles/elletr.dat"
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELLETR-FILE-STATUS
                               RECORD KEY IS TX-CONTROL-PRIMARY.
           SELECT EXTRACT ASSIGN TO SYS011.
      *         ASSIGN TO "e:/cid/seqfiles/el350_ext.txt"
      *         organization line sequential.
DAN   *    SELECT UPLOAD-FILE  ASSIGN TO SYS018.
           SELECT DISK-DATE ASSIGN TO SYS019.
      *         ASSIGN TO "c:/cid/seqfiles/ci.dd.er.datecard".
           SELECT PRINTX ASSIGN TO SYS008.
      *         ASSIGN TO "e:/cid/seqfiles/el350.prt".

       DATA DIVISION.
       FILE SECTION.

       FD  ERACCT.
                                       COPY ERCACCT.
           EJECT
       FD  ELCNTL.
                                       COPY ELCCNTL.
           EJECT
      *FD  ELACTQ.
      *                                COPY ELCACTQ.
      *    EJECT
       FD  ELTRLR.
                                       COPY ELCTRLR.
           EJECT
       FD  ELARCH.
                                       COPY ELCARCH.
           EJECT
       FD  ELBENE.
                                       COPY ELCBENE.
           EJECT
       FD  ELCERT.
                                       COPY ELCCERT.
           EJECT
       FD  ELMSTR.
                                       COPY ELCMSTR.
           EJECT
       FD  ERCOMP.
                                       COPY ERCCOMP.
           EJECT
       FD  ELLETR.
                                       COPY ELCTEXT.
           EJECT


       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  EXTRACT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  EXTRACT-RECORD              PIC X(216).

      *FD  UPLOAD-FILE
      *    RECORDING MODE F
      *    LABEL RECORDS STANDARD
      *    BLOCK CONTAINS 0 RECORDS.
      *01  UPLOAD-RECORD    pic x(190).

       FD  PRINTX
                                       COPY ELCPRTFD.
           EJECT
       WORKING-STORAGE SECTION.
pemuni*77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
pemuni*77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
pemuni*                                   USAGE POINTER.
pemuni*77  LCP-INITIAL-COMP              PIC S9(8) COMP.
pemuni*77  LCP-INITIAL-PNTR              REDEFINES LCP-INITIAL-COMP
pemuni*                                     USAGE POINTER.
pemuni 77  LCP-WS-ADDR-COMP              PIC x(4) comp-5.
pemuni 77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
pemuni                                    USAGE POINTER.
pemuni 77  LCP-INITIAL-COMP              PIC x(4) comp-5.
pemuni 77  LCP-INITIAL-PNTR              REDEFINES LCP-INITIAL-COMP
pemuni                                      USAGE POINTER.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     EL350 WORKING STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  THERE-ARE-NO-MORE-RECORDS  VALUE 'Y'.
           88  THERE-ARE-MORE-RECORDS     VALUE 'N'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  tally-ctr                   PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  ERACCT-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELCNTL-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELACTQ-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELTRLR-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELARCH-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELBENE-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELCERT-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELMSTR-FILE-STATUS          PIC XX     VALUE '00'.
       77  ERCOMP-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELLETR-FILE-STATUS          PIC XX     VALUE '00'.
      *77  UPLOAD-FILE-STATUS          PIC XX     VALUE '00'.
       77  WS-RESEND-DATE              PIC XX     VALUE LOW-VALUES.
       77  WS-FOLLOW-UP-DATE           PIC XX     VALUE LOW-VALUES.
       77  W-NAME-SW                   PIC S9     VALUE +0 COMP-3.
       77  W-SUB                       PIC S999   VALUE +0 COMP-3.
       77  W-POSITION2                 PIC S9(4)  VALUE +0 COMP.
       77  W-POSITION21                PIC S9(4)  VALUE +0 COMP.
PEMTST 77  ELCNTL-READ                 PIC X      VALUE ' '.
PEMTST 77  ELMSTR-CNT                  PIC S9(7)  VALUE +0 COMP-3.
PEMTST 77  ELMSTR-CNT-MT               PIC S9(7)  VALUE +0 COMP-3.
PEMTST 77  ELMSTR-CNT-CA               PIC S9(7)  VALUE +0 COMP-3.
       77  DISP-CNT                    PIC 9(7)   VALUE ZEROS.
       77  WS-DUP-COUNT                PIC 9(11)  VALUE ZEROS.
       77  WS-EXT-COUNT                PIC 9(11)  VALUE ZEROS.
       77  WS-INIT-EXT-REC             PIC X(216) VALUE SPACES.

           EJECT
       01  WS-EXT-RECORD.
           12  EXT-CARRIER           PIC X.
           12  EXT-TAB1              PIC X.
           12  EXT-CLAIM-NO          PIC X(7).
           12  EXT-TAB2              PIC X.
           12  EXT-CERT-NO           PIC X(11).
           12  EXT-TAB3              PIC X.
           12  EXT-INC-DATE          PIC X(8).
           12  EXT-TAB3A             PIC X.
           12  EXT-INSURED-NAME      PIC X(30).
           12  EXT-TAB4              PIC X.
           12  EXT-INSURED-ADDR1     PIC X(30).
           12  EXT-TAB5              PIC X.
           12  EXT-INSURED-ADDR2     PIC X(30).
           12  EXT-TAB6              PIC X.
           12  EXT-INSURED-ADDR3     PIC X(30).
           12  EXT-TAB7              PIC X.
           12  EXT-INSURED-ADDR4     PIC X(30).
           12  EXT-TAB8              PIC X.
           12  EXT-INSURED-TRLR-NAME PIC X(30).

       01  W-LABEL-HOLD-AREA.
           12  W-SEQ-COUNTER           PIC S9(04) COMP VALUE +0.
           12  W-LABELS-SW             PIC  X(01) VALUE SPACE.
           12  W-LABEL-LINES OCCURS 6 TIMES
                              INDEXED BY W-NDX   W-NDX2.
               16  W-LABEL-ZIP.
                   20  W-LABEL-1ST-ZIP
                                       PIC  X(05).
                   20  FILLER          PIC  X(01).
                   20  W-LABEL-2ND-ZIP
                                       PIC  X(04).
               16  FILLER              PIC  X(09).
               16  W-LAST-DIGIT        PIC  X(01).
               16  W-LAST-ZIP.
                   20  W-LAST-1ST-ZIP  PIC  X(05).
                   20  FILLER          PIC  X(01).
                   20  W-LAST-2ND-ZIP PIC   X(04).

dan    01  ws-upload-record.
           05  ul-state        pic xx.
           05  ul-bus-type     pic xx.
           05  ul-acct         pic x(10).
           05  ul-acct-sfx     pic xx.
           05  ul-exp-dt       pic x(8).
           05  ul-conv-dt      pic x(8).
           05  ul-eff-dt       pic x(8).
           05  ul-exp-eff      pic x(8).
           05  ul-rptcd-1      pic x(30).
           05  ul-rptcd-2      pic x(30).
           05  ul-acct-name    pic x(40).
           05  ul-cont-name    pic x(40).
           05  ul-address      pic x(30).
           05  ul-city-st      pic x(30).
           05  ul-zip          pic x(9).
           05  ul-ig           pic x.
           05  ul-stat         pic x.
           05  ul-mail-cat     pic x(9).
           05  ul-ecs020-diff  pic x(3).

       01  W-RECORD-TABLE              PIC  X(21900) VALUE SPACES.
       01  W-REC-TABLE REDEFINES W-RECORD-TABLE.
           12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-NDX
                                       PIC  X(3650).

       01  W-REC-ENTRIES REDEFINES W-RECORD-TABLE.
           12  W-REC-ENT OCCURS 300 TIMES INDEXED BY W-TB-NDX W-TB-NDX1.
               16  W-REC-TEXT          PIC  X(70).
               16  W-REC-PC            PIC  9(02).
               16  FILLER              PIC  X(01).

                                       EJECT
       01  W-PROGRAM-VARIABLE-AREA.
           12  FILLER                  PIC  X(13)
                                            VALUE 'VARIABLE AREA'.
      ****************************************************
      *       WHEN ADDING OR DELETING ENTRIES TO         *
      *       THE SYSTEM-SUPPORTED-VARIABLES THE         *
      *       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *
      *       TO MATCH THE NUMBER OF ENTRIES IN THE      *
      *       SYSTEM-SUPPORTED-VARIABLE TABLE.           *
      *       ALSO YOU NEED TO INCREASE THE LENGTH OF    *
      *       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *
      ****************************************************

      *  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF
      *  THE SS-WORK-AREA-LENGTH FIELD FOR THE W-VARIABLE-WORK-AREA

           12  SS-NUM-ENTRIES          PIC  9(03) VALUE 115      COMP-3.
           12  SS-COUNTER              PIC  9(03)                COMP-3.
           12  SS-WORK-AREA-LENGTH     PIC S9(04) VALUE +3272    COMP.

       01  W-VARIABLE-WORK-AREA.
           12  W-VAR-CODE              PIC  X(04).
           12  W-VAR-LEN               PIC  9(02).
           12  W-VAR-DATA          PIC  X(100).
           12  W-VAR-DATA-R REDEFINES W-VAR-DATA.
             16  W-VAR-W-ONE-CHAR OCCURS 100 TIMES
                              INDEXED BY W-NDXV
                                       PIC  X(01).
       01  SYSTEM-SUPPORTED-VARIABLES.
      *****COMPANY NAME
           12  SS01                    PIC  X(04) VALUE    '01.0'.
           12  SS01L                   PIC  9(02) VALUE 36.
           12  SS01D                   PIC  X(30) VALUE ALL '*'.
      *****FULL COMPANY ADDRESS
           12  SS02-1                  PIC  X(04) VALUE    '02.1'.
           12  SS02-1L                 PIC  9(02) VALUE 36.
           12  SS02-1D                 PIC  X(30) VALUE ALL '*'.
           12  SS02-2                  PIC  X(04) VALUE    '02.2'.
           12  SS02-2L                 PIC  9(02) VALUE 36.
           12  SS02-2D                 PIC  X(30) VALUE ALL '*'.
           12  SS02-3                  PIC  X(04) VALUE    '02.3'.
           12  SS02-3L                 PIC  9(02) VALUE 36.
           12  SS02-3D                 PIC  X(30) VALUE ALL '*'.
           12  SS02-4                  PIC  X(04) VALUE    '02.4'.
           12  SS02-4L                 PIC  9(02) VALUE 36.
           12  SS02-4D                 PIC  X(30) VALUE ALL '*'.
           12  SS02-5                  PIC  X(04) VALUE    '02.5'.
           12  SS02-5L                 PIC  9(02) VALUE 36.
           12  SS02-5D                 PIC  X(30) VALUE ALL '*'.
      *****CARRIER NAME
           12  SS03                    PIC  X(04) VALUE    '03.0'.
           12  SS03L                   PIC  9(02) VALUE 36.
           12  SS03D                   PIC  X(30) VALUE ALL '*'.
      *****FULL CARRIER ADDRESS
           12  SS04-1                  PIC  X(04) VALUE    '04.1'.
           12  SS04-1L                 PIC  9(02) VALUE 36.
           12  SS04-1D                 PIC  X(30) VALUE ALL '*'.
           12  SS04-2                  PIC  X(04) VALUE    '04.2'.
           12  SS04-2L                 PIC  9(02) VALUE 36.
           12  SS04-2D                 PIC  X(30) VALUE ALL '*'.
           12  SS04-3                  PIC  X(04) VALUE    '04.3'.
           12  SS04-3L                 PIC  9(02) VALUE 36.
           12  SS04-3D                 PIC  X(30) VALUE ALL '*'.
           12  SS04-4                  PIC  X(04) VALUE    '04.4'.
           12  SS04-4L                 PIC  9(02) VALUE 36.
           12  SS04-4D                 PIC  X(30) VALUE ALL '*'.
           12  SS04-5                  PIC  X(04) VALUE    '04.5'.
           12  SS04-5L                 PIC  9(02) VALUE 36.
           12  SS04-5D                 PIC  X(30) VALUE ALL '*'.
      *****CARRIER PHONE NUMBER
           12  SS04-6                  PIC  X(04) VALUE    '04.6'.
           12  SS04-6L                 PIC  9(02) VALUE 18.
           12  SS04-6D                 PIC  X(12) VALUE ALL '*'.
      *****FULL ADDRESEE LABEL
           12  SS05-1                  PIC  X(04) VALUE    '05.1'.
           12  SS05-1L                 PIC  9(02) VALUE 36.
           12  SS05-1D                 PIC  X(30) VALUE ALL '*'.
           12  SS05-2                  PIC  X(04) VALUE    '05.2'.
           12  SS05-2L                 PIC  9(02) VALUE 36.
           12  SS05-2D                 PIC  X(30) VALUE ALL '*'.
           12  SS05-3                  PIC  X(04) VALUE    '05.3'.
           12  SS05-3L                 PIC  9(02) VALUE 36.
           12  SS05-3D                 PIC  X(30) VALUE ALL '*'.
           12  SS05-4                  PIC  X(04) VALUE    '05.4'.
           12  SS05-4L                 PIC  9(02) VALUE 36.
           12  SS05-4D                 PIC  X(30) VALUE ALL '*'.
           12  SS05-5                  PIC  X(04) VALUE    '05.5'.
           12  SS05-5L                 PIC  9(02) VALUE 36.
           12  SS05-5D                 PIC  X(30) VALUE ALL '*'.
           12  SS05-6                  PIC  X(04) VALUE    '05.6'.
           12  SS05-6L                 PIC  9(02) VALUE 36.
           12  SS05-6D                 PIC  X(30) VALUE ALL '*'.
      *****ACCOUNT NAME
           12  SS06                    PIC  X(04) VALUE    '06.0'.
           12  SS06L                   PIC  9(02) VALUE 36.
           12  SS06D                   PIC  X(30) VALUE ALL '*'.
      *****FULL ACCOUNT ADDRESS
           12  SS07-1                  PIC  X(04) VALUE    '07.1'.
           12  SS07-1L                 PIC  9(02) VALUE 36.
           12  SS07-1D                 PIC  X(30) VALUE ALL '*'.
           12  SS07-2                  PIC  X(04) VALUE    '07.2'.
           12  SS07-2L                 PIC  9(02) VALUE 36.
           12  SS07-2D                 PIC  X(30) VALUE ALL '*'.
           12  SS07-3                  PIC  X(04) VALUE    '07.3'.
           12  SS07-3L                 PIC  9(02) VALUE 36.
           12  SS07-3D                 PIC  X(30) VALUE ALL '*'.
           12  SS07-4                  PIC  X(04) VALUE    '07.4'.
           12  SS07-4L                 PIC  9(02) VALUE 36.
           12  SS07-4D                 PIC  X(30) VALUE ALL '*'.
           12  SS07-5                  PIC  X(04) VALUE    '07.5'.
           12  SS07-5L                 PIC  9(02) VALUE 36.
           12  SS07-5D                 PIC  X(30) VALUE ALL '*'.
      *****ACCOUNT PHONE NUMBER
           12  SS07-6                  PIC  X(04) VALUE    '07.6'.
           12  SS07-6L                 PIC  9(02) VALUE 18.
           12  SS07-6D                 PIC  X(12) VALUE ALL '*'.
      *****EXECUTING PROCESSOR NAME
           12  SS08                    PIC  X(04) VALUE    '08.0'.
           12  SS08L                   PIC  9(02) VALUE 36.
           12  SS08D                   PIC  X(30) VALUE ALL '*'.
      *****PROCESSOR TITLE
           12  SS09                    PIC  X(04) VALUE    '09.0'.
           12  SS09L                   PIC  9(02) VALUE 32.
           12  SS09D                   PIC  X(26) VALUE ALL '*'.
      *****INSUREDS NAME
           12  SS10                    PIC  X(04) VALUE    '10.0'.
           12  SS10L                   PIC  9(02) VALUE 36.
           12  SS10D                   PIC  X(30) VALUE ALL '*'.
      *****INSUREDS ADDRESS
           12  SS11-1                  PIC  X(04) VALUE    '11.1'.
           12  SS11-1L                 PIC  9(02) VALUE 36.
           12  SS11-1D                 PIC  X(30) VALUE ALL '*'.
           12  SS11-2                  PIC  X(04) VALUE    '11.2'.
           12  SS11-2L                 PIC  9(02) VALUE 36.
           12  SS11-2D                 PIC  X(30) VALUE ALL '*'.
           12  SS11-3                  PIC  X(04) VALUE    '11.3'.
           12  SS11-3L                 PIC  9(02) VALUE 36.
           12  SS11-3D                 PIC  X(30) VALUE ALL '*'.
           12  SS11-4                  PIC  X(04) VALUE    '11.4'.
           12  SS11-4L                 PIC  9(02) VALUE 36.
           12  SS11-4D                 PIC  X(30) VALUE ALL '*'.
      *****INSUREDS NAME FROM ADDR TRAILER
           12  SS11-5                  PIC  X(04) VALUE    '11.5'.
           12  SS11-5L                 PIC  9(02) VALUE 36.
           12  SS11-5D                 PIC  X(30) VALUE ALL '*'.
      *****INSUREDS PHONE NUMBER FROM ADDR TRAILER
           12  SS11-6                  PIC  X(04) VALUE    '11.6'.
           12  SS11-6L                 PIC  9(02) VALUE 18.
           12  SS11-6D                 PIC  X(12) VALUE ALL '*'.
      *****CLAIM TYPE NAME
           12  SS12                    PIC  X(04) VALUE    '12.0'.
           12  SS12L                   PIC  9(02) VALUE 12.
           12  SS12D                   PIC  X(6) VALUE ALL '*'.
      *****CLAIM INCURRED DATE
           12  SS13                    PIC  X(04) VALUE    '13.0'.
           12  SS13L                   PIC  9(02) VALUE 14.
           12  SS13D                   PIC  X(08) VALUE ALL '*'.
      *****CLAIM REPORTED DATE
           12  SS14                    PIC  X(04) VALUE    '14.0'.
           12  SS14L                   PIC  9(02) VALUE 14.
           12  SS14D                   PIC  X(08) VALUE ALL '*'.
      *****LAST PAYMENT DATE
           12  SS15                    PIC  X(04) VALUE    '15.0'.
           12  SS15L                   PIC  9(02) VALUE 14.
           12  SS15D                   PIC  X(08) VALUE ALL '*'.
      *****LAST PAYMENT AMOUNT
           12  SS16                    PIC  X(04) VALUE    '16.0'.
           12  SS16L                   PIC  9(02) VALUE 17.
           12  SS16D                   PIC $$$$,$$$.99 VALUE ZEROS.
      *****CLAIM PAID THRU/TO DATE
           12  SS17                    PIC  X(04) VALUE    '17.0'.
           12  SS17L                   PIC  9(02) VALUE 14.
           12  SS17D                   PIC  X(08) VALUE ALL '*'.
      *****TOTAL PAID TO DATE
           12  SS18                    PIC  X(04) VALUE    '18.0'.
           12  SS18L                   PIC  9(02) VALUE 17.
           12  SS18D                   PIC $$$$,$$$.99 VALUE ZEROS.
      *****DIAGNOSIS OR CAUSE
           12  SS19                    PIC  X(04) VALUE    '19.0'.
           12  SS19L                   PIC  9(02) VALUE 32.
           12  SS19D                   PIC  X(26) VALUE ALL '*'.
      *****CAUSE CODE
           12  SS19-1                  PIC  X(04) VALUE    '19.1'.
           12  SS19-1L                 PIC  9(02) VALUE 12.
           12  SS19-1D                 PIC  X(6) VALUE ALL '*'.
      *****CURRENT DATE
           12  SS20                    PIC  X(04) VALUE    '20.0'.
           12  SS20L                   PIC  9(02) VALUE 14.
           12  SS20D                   PIC  X(08) VALUE ALL '*'.
      *****FULL CURRENT DATE
           12  SS21                    PIC  X(04) VALUE    '21.0'.
           12  SS21L                   PIC  9(02) VALUE 24.
           12  SS21D                   PIC  X(18) VALUE ALL '*'.
      *****BENEFIT DESCRIPTION
           12  SS22                    PIC  X(04) VALUE    '22.0'.
           12  SS22L                   PIC  9(02) VALUE 16.
           12  SS22D                   PIC  X(10) VALUE ALL '*'.
      *****CARRIER CODE IN CERT
           12  SS23                    PIC  X(04) VALUE    '23.0'.
           12  SS23L                   PIC  9(02) VALUE 9.
           12  SS23D                   PIC  X(03) VALUE ALL '*'.
      *****GROUPING CODE IN CERT
           12  SS24                    PIC  X(04) VALUE    '24.0'.
           12  SS24L                   PIC  9(02) VALUE 12.
           12  SS24D                   PIC  X(6) VALUE ALL '*'.
      *****ACCOUNT NUMBER IN CERT
           12  SS25                    PIC  X(04) VALUE    '25.0'.
           12  SS25L                   PIC  9(02) VALUE 16.
           12  SS25D                   PIC  X(10) VALUE ALL '*'.
      *****CERTIFICATE NUMBER
           12  SS26                    PIC  X(04) VALUE    '26.0'.
           12  SS26L                   PIC  9(02) VALUE 17.
           12  SS26D                   PIC  X(11) VALUE ALL '*'.
      *****CERT EFFECTIVE DATE
           12  SS27                    PIC  X(04) VALUE    '27.0'.
           12  SS27L                   PIC  9(02) VALUE 14.
           12  SS27D                   PIC  X(08) VALUE ALL '*'.
      *****CERT EXPIRATION DATE
           12  SS28                    PIC  X(04) VALUE    '28.0'.
           12  SS28L                   PIC  9(02) VALUE 14.
           12  SS28D                   PIC  X(08) VALUE ALL '*'.
      *****APPLICABLE COVERAGE TERM
           12  SS29                    PIC  X(04) VALUE    '29.0'.
           12  SS29L                   PIC  9(02) VALUE 9.
           12  SS29D                   PIC  X(03) VALUE ALL '*'.
      *****APPLICABLE COVERAGE AMOUNT
           12  SS30                    PIC  X(04) VALUE    '30.0'.
           12  SS30L                   PIC  9(02) VALUE 18.
           12  SS30D                   PIC $$$$$,$$$.99 VALUE ZEROS.
      *****APPLICABLE COVERAGE CANCEL DATE
           12  SS31                    PIC  X(04) VALUE    '31.0'.
           12  SS31L                   PIC  9(02) VALUE 14.
           12  SS31D                   PIC  X(08) VALUE ALL '*'.
      *****APPLICABLE COVERAGE FORM NUMBER
           12  SS32                    PIC  X(04) VALUE    '32.0'.
           12  SS32L                   PIC  9(02) VALUE 18.
           12  SS32D                   PIC  X(12) VALUE ALL '*'.
      *****INSURES AGE AT POLICY ISSUE
           12  SS33                    PIC  X(04) VALUE    '33.0'.
           12  SS33L                   PIC  9(02) VALUE 9.
           12  SS33D                   PIC  X(03) VALUE ALL '*'.
      *****CLAIM NUMBER
           12  SS34                    PIC  X(04) VALUE    '34.0'.
           12  SS34L                   PIC  9(02) VALUE 13.
           12  SS34D                   PIC  X(07) VALUE ALL '*'.
      *****LAST DENIAL TEXT
           12  SS35-1                  PIC  X(04) VALUE    '35.1'.
           12  SS35-1L                 PIC  9(02) VALUE 66.
           12  SS35-1D                 PIC  X(60) VALUE ALL '*'.
           12  SS35-2                  PIC  X(04) VALUE    '35.2'.
           12  SS35-2L                 PIC  9(02) VALUE 66.
           12  SS35-2D                 PIC  X(60) VALUE ALL '*'.
      *****LOAN NUMBER
           12  SS36                    PIC  X(04) VALUE    '36.0'.
           12  SS36L                   PIC  9(02) VALUE 14.
           12  SS36D                   PIC  X(08) VALUE ALL '*'.
      *****CREDIT CARD LOAN NUMBER
           12  SS36-1                  PIC  X(04) VALUE    '36.1'.
           12  SS36-1L                 PIC  9(02) VALUE 26.
           12  SS36-1D                 PIC  X(20) VALUE ALL '*'.
      *****LOAN BALANCE
           12  SS37                    PIC  X(04) VALUE    '37.0'.
           12  SS37L                   PIC  9(02) VALUE 18.
           12  SS37D                   PIC $$$$$,$$$.99 VALUE ZEROS.
      *****MEMBER NUMBER
           12  SS38                    PIC  X(04) VALUE    '38.0'.
           12  SS38L                   PIC  9(02) VALUE 18.
           12  SS38D                   PIC  X(12) VALUE ALL '*'.
      *****INSURED NAME (FIRST M LAST)
           12  SS39                    PIC  X(04) VALUE    '39.0'.
           12  SS39L                   PIC  9(02) VALUE 36.
           12  SS39D                   PIC  X(30) VALUE ALL '*'.
      *****INSURED LAST NAME ONLY
           12  SS40                    PIC  X(04) VALUE    '40.0'.
           12  SS40L                   PIC  9(02) VALUE 21.
           12  SS40D                   PIC  X(15) VALUE ALL '*'.
      *****TITLE (MR/MS)
           12  SS41                    PIC  X(04) VALUE    '41.0'.
           12  SS41L                   PIC  9(02) VALUE 9.
           12  SS41D                   PIC  X(03) VALUE ALL '*'.
      *****ELIMINATION PERIOD
           12  SS42                    PIC  X(04) VALUE    '42.0'.
           12  SS42L                   PIC  9(02) VALUE 9.
           12  SS42D                   PIC  X(03) VALUE ALL '*'.
      *****BENEFICIARY NAME
           12  SS43                    PIC  X(04) VALUE    '43.0'.
           12  SS43L                   PIC  9(02) VALUE 36.
           12  SS43D                   PIC  X(30) VALUE ALL '*'.
      *****BENEFICIARY ADDRESS
           12  SS44-1                  PIC  X(04) VALUE    '44.1'.
           12  SS44-1L                 PIC  9(02) VALUE 36.
           12  SS44-1D                 PIC  X(30) VALUE ALL '*'.
           12  SS44-2                  PIC  X(04) VALUE    '44.2'.
           12  SS44-2L                 PIC  9(02) VALUE 36.
           12  SS44-2D                 PIC  X(30) VALUE ALL '*'.
           12  SS44-3                  PIC  X(04) VALUE    '44.3'.
           12  SS44-3L                 PIC  9(02) VALUE 36.
           12  SS44-3D                 PIC  X(30) VALUE ALL '*'.
           12  SS44-4                  PIC  X(04) VALUE    '44.4'.
           12  SS44-4L                 PIC  9(02) VALUE 36.
           12  SS44-4D                 PIC  X(30) VALUE ALL '*'.
           12  SS44-5                  PIC  X(04) VALUE    '44.5'.
           12  SS44-5L                 PIC  9(02) VALUE 18.
           12  SS44-5D                 PIC  X(12) VALUE ALL '*'.
      *****INSUREDS DATE OF BIRTH
           12  SS45                    PIC  X(04) VALUE    '45.0'.
           12  SS45L                   PIC  9(02) VALUE 14.
           12  SS45D                   PIC  X(08) VALUE ALL '*'.
      *****INSUREDS SOC SEC NUMBER
           12  SS46                    PIC  X(04) VALUE    '46.0'.
           12  SS46L                   PIC  9(02) VALUE 17.
           12  SS46D                   PIC  X(11) VALUE ALL '*'.
      *****PHYSICIANS  NAME
           12  SS47                    PIC  X(04) VALUE    '47.0'.
           12  SS47L                   PIC  9(02) VALUE 36.
           12  SS47D                   PIC  X(30) VALUE ALL '*'.
      *****PHYSICIANS  ADDRESS
           12  SS47-1                  PIC  X(04) VALUE    '47.1'.
           12  SS47-1L                 PIC  9(02) VALUE 36.
           12  SS47-1D                 PIC  X(30) VALUE ALL '*'.
           12  SS47-2                  PIC  X(04) VALUE    '47.2'.
           12  SS47-2L                 PIC  9(02) VALUE 36.
           12  SS47-2D                 PIC  X(30) VALUE ALL '*'.
           12  SS47-3                  PIC  X(04) VALUE    '47.3'.
           12  SS47-3L                 PIC  9(02) VALUE 36.
           12  SS47-3D                 PIC  X(30) VALUE ALL '*'.
           12  SS47-4                  PIC  X(04) VALUE    '47.4'.
           12  SS47-4L                 PIC  9(02) VALUE 36.
           12  SS47-4D                 PIC  X(30) VALUE ALL '*'.
           12  SS47-5                  PIC  X(04) VALUE    '47.5'.
           12  SS47-5L                 PIC  9(02) VALUE 18.
           12  SS47-5D                 PIC  X(12) VALUE ALL '*'.
      *****EMPLOYERS   NAME
           12  SS48                    PIC  X(04) VALUE    '48.0'.
           12  SS48L                   PIC  9(02) VALUE 36.
           12  SS48D                   PIC  X(30) VALUE ALL '*'.
      *****EMPLOYERS   ADDRESS
           12  SS48-1                  PIC  X(04) VALUE    '48.1'.
           12  SS48-1L                 PIC  9(02) VALUE 36.
           12  SS48-1D                 PIC  X(30) VALUE ALL '*'.
           12  SS48-2                  PIC  X(04) VALUE    '48.2'.
           12  SS48-2L                 PIC  9(02) VALUE 36.
           12  SS48-2D                 PIC  X(30) VALUE ALL '*'.
           12  SS48-3                  PIC  X(04) VALUE    '48.3'.
           12  SS48-3L                 PIC  9(02) VALUE 36.
           12  SS48-3D                 PIC  X(30) VALUE ALL '*'.
           12  SS48-4                  PIC  X(04) VALUE    '48.4'.
           12  SS48-4L                 PIC  9(02) VALUE 36.
           12  SS48-4D                 PIC  X(30) VALUE ALL '*'.
           12  SS48-5                  PIC  X(04) VALUE    '48.5'.
           12  SS48-5L                 PIC  9(02) VALUE 18.
           12  SS48-5D                 PIC  X(12) VALUE ALL '*'.
      *****OTHER1      NAME
           12  SS49                    PIC  X(04) VALUE    '49.0'.
           12  SS49L                   PIC  9(02) VALUE 36.
           12  SS49D                   PIC  X(30) VALUE ALL '*'.
      *****OTHER1      ADDRESS
           12  SS49-1                  PIC  X(04) VALUE    '49.1'.
           12  SS49-1L                 PIC  9(02) VALUE 36.
           12  SS49-1D                 PIC  X(30) VALUE ALL '*'.
           12  SS49-2                  PIC  X(04) VALUE    '49.2'.
           12  SS49-2L                 PIC  9(02) VALUE 36.
           12  SS49-2D                 PIC  X(30) VALUE ALL '*'.
           12  SS49-3                  PIC  X(04) VALUE    '49.3'.
           12  SS49-3L                 PIC  9(02) VALUE 36.
           12  SS49-3D                 PIC  X(30) VALUE ALL '*'.
           12  SS49-4                  PIC  X(04) VALUE    '49.4'.
           12  SS49-4L                 PIC  9(02) VALUE 36.
           12  SS49-4D                 PIC  X(30) VALUE ALL '*'.
           12  SS49-5                  PIC  X(04) VALUE    '49.5'.
           12  SS49-5L                 PIC  9(02) VALUE 18.
           12  SS49-5D                 PIC  X(12) VALUE ALL '*'.
      *****OTHER2      NAME
           12  SS50                    PIC  X(04) VALUE    '50.0'.
           12  SS50L                   PIC  9(02) VALUE 36.
           12  SS50D                   PIC  X(30) VALUE ALL '*'.
      *****OTHER2      ADDRESS
           12  SS50-1                  PIC  X(04) VALUE    '50.1'.
           12  SS50-1L                 PIC  9(02) VALUE 36.
           12  SS50-1D                 PIC  X(30) VALUE ALL '*'.
           12  SS50-2                  PIC  X(04) VALUE    '50.2'.
           12  SS50-2L                 PIC  9(02) VALUE 36.
           12  SS50-2D                 PIC  X(30) VALUE ALL '*'.
           12  SS50-3                  PIC  X(04) VALUE    '50.3'.
           12  SS50-3L                 PIC  9(02) VALUE 36.
           12  SS50-3D                 PIC  X(30) VALUE ALL '*'.
           12  SS50-4                  PIC  X(04) VALUE    '50.4'.
           12  SS50-4L                 PIC  9(02) VALUE 36.
           12  SS50-4D                 PIC  X(30) VALUE ALL '*'.
           12  SS50-5                  PIC  X(04) VALUE    '50.5'.
           12  SS50-5L                 PIC  9(02) VALUE 18.
           12  SS50-5D                 PIC  X(12) VALUE ALL '*'.
      *****A&H TERM TIMES MON. BEN.
           12  SS51                    PIC  X(04) VALUE    '51.0'.
           12  SS51L                   PIC  9(02) VALUE 17.
           12  SS51D                   PIC $$$$,$$$.99  VALUE ZEROS.
      *****THIRD PARTY NAME
           12  SS52                    PIC  X(04) VALUE    '52.0'.
           12  SS52L                   PIC  9(02) VALUE 36.
           12  SS52D                   PIC  X(30) VALUE ALL '*'.
      *****THIRD PARTY ADDRESS
           12  SS53-1                  PIC  X(04) VALUE    '53.1'.
           12  SS53-1L                 PIC  9(02) VALUE 36.
           12  SS53-1D                 PIC  X(30) VALUE ALL '*'.
           12  SS53-2                  PIC  X(04) VALUE    '53.2'.
           12  SS53-2L                 PIC  9(02) VALUE 36.
           12  SS53-2D                 PIC  X(30) VALUE ALL '*'.
           12  SS53-3                  PIC  X(04) VALUE    '53.3'.
           12  SS53-3L                 PIC  9(02) VALUE 36.
           12  SS53-3D                 PIC  X(30) VALUE ALL '*'.
           12  SS53-4                  PIC  X(04) VALUE    '53.4'.
           12  SS53-4L                 PIC  9(02) VALUE 36.
           12  SS53-4D                 PIC  X(30) VALUE ALL '*'.
           12  SS53-5                  PIC  X(04) VALUE    '53.5'.
           12  SS53-5L                 PIC  9(02) VALUE 36.
           12  SS53-5D                 PIC  X(30) VALUE ALL '*'.
      *****THIRD PARTY PHONE NUMBER
           12  SS53-6                  PIC  X(04) VALUE    '53.6'.
           12  SS53-6L                 PIC  9(02) VALUE 18.
           12  SS53-6D                 PIC  X(12) VALUE ALL '*'.
      *****CERTIFICATE SEQUENCE
           12  SS54                    PIC  X(04) VALUE    '54.0'.
           12  SS54L                   PIC  9(02) VALUE 09.
           12  SS54D                   PIC  X(03) VALUE ALL '*'.
      *****CERTIFICATE TOTAL  E
           12  SS55                    PIC  X(04) VALUE    '55.0'.
           12  SS55L                   PIC  9(02) VALUE 09.
           12  SS55D                   PIC  X(03) VALUE ALL '*'.
      *****CREDITOR ID
           12  SS56                    PIC  X(04) VALUE    '56.0'.
           12  SS56L                   PIC  9(02) VALUE 36.
           12  SS56D                   PIC  X(30) VALUE ALL '*'.
      *****INSUREDS NAME (CERTIFICATE)
           12  SS57                    PIC X(4)  VALUE     '57.0'.
           12  SS57L                   PIC 99    VALUE 36.
           12  SS57D                   PIC X(30) VALUE ALL '*'.
      *****JOINTS NAME (CERTIFICATE)
           12  SS58                    PIC X(4)  VALUE     '58.0'.
           12  SS58L                   PIC 99    VALUE 36.
           12  SS58D                   PIC X(30) VALUE ALL '*'.
      *****POLICY REFERENCE NUMBER
           12  SS59                    PIC X(4)  VALUE     '59.0'.
           12  SS59L                   PIC 99    VALUE 26.
           12  SS59D                   PIC X(20) VALUE ALL '*'.

      ****************************************************
      *       WHEN ADDING OR DELETING ENTRIES TO         *
      *       THE SYSTEM-SUPPORTED-VARIABLES THE         *
      *       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *
      *       TO MATCH THE NUMBER OF ENTRIES IN THE      *
      *       SYSTEM-SUPPORTED-VARIABLE TABLE.           *
      *       ALSO YOU NEED           TO INCREASE THE LENGTH OF *
      *       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *
      ****************************************************
                                       EJECT
       01  W-PROGRAM-CONSTANTS.
           12  W-WORK-AMOUNT           PIC S9(09)V99 VALUE +0.
           12  W-ZIP-NUMERIC           PIC  9(09).
           12  W-ZIP-NONNUM   REDEFINES  W-ZIP-NUMERIC
                                       PIC  X(09).
           12  FILLER                  PIC  X(17)
                                       VALUE 'PROGRAM CONSTANTS'.
           12  W-LOWER-CASE
                          PIC  X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
           12  W-MAX-LINES             PIC  9(03) VALUE 300.
           12  W-TEXT-ID               PIC  X(08) VALUE 'ELLETR'.
           12  W-TOP-FORM              PIC  X(70)
               VALUE '*****TOP OF FORM *****'.
           12  W-UPPER-CASE
                          PIC  X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

       01  WS-MISC.
           12  W-GETMAIN-SW            PIC X    VALUE 'N'.
               88  W-NO-GETMAIN-DONE-YET        VALUE 'N'.
           12  W-TOTAL-LINES           PIC S9(03) COMP-3 VALUE +0.
           12  W-NDX-WORK              PIC 9(02)   VALUE ZEROS.
           12  WS-ARCHIVE-NO           PIC S9(8)   COMP VALUE +0.
           12  WS-CURRENT-TIME         PIC S9(7)   VALUE ZERO.
           12  W-DATA-FOUND-SW         PIC  X(01).
               88  NO-CHARACTERS-FOUND            VALUE 'N'.
           12  WS-CURRENT-BIN-DATE     PIC XX  VALUE LOW-VALUES.
           12  W-DATE-WORK             PIC  9(07).
           12  W-DT-REDEF REDEFINES W-DATE-WORK.
               16  FILLER              PIC  X(02).
               16  W-DT-WORK           PIC  9(05).

           12  W-CORR-TRLR-SEQ         PIC S9(04) COMP.
           12  W-BEN-HOLD              PIC XX  VALUE ZEROS.
           12  W-BENEFIT-WORK          PIC  X(03).
           12  W-BEN-R REDEFINES W-BENEFIT-WORK.
               16  W-ELIM-DAYS         PIC  X(02).
               16  FILLER              PIC  X(01).

           12  W-Z-CONTROL-DATA.
               16  W-NUMBER-OF-COPIES  PIC  9(01).
               16  FILLER              PIC  X(01).
               16  W-DAYS-TO-FOLLOW-UP PIC  9(03).
               16  FILLER              PIC  X(01).
               16  W-DAYS-TO-RESEND-1  PIC  9(03).
               16  FILLER              PIC  X(01).
               16  W-DAYS-TO-RESEND-2  PIC  9(03).
               16  FILLER              PIC  X(01).
               16  W-DAYS-TO-RESEND-3  PIC  9(03).

           12  W-NAME.
               16  W-FIRST-NAME        PIC  X(12).
               16  W-MIDDLE-NAME       PIC  X(12).
               16  W-LAST-NAME         PIC  X(15).

           12  W-CREDIT-CARD-LOAN-NO.
               16  W-LOAN-NUMBER       PIC  X(08).
               16  W-CURRENT-LOAN-NO   PIC  X(12).

           12  W-DEEDIT-FIELD          PIC  X(15).
           12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD
                                       PIC S9(15).

           12  W-CURRENT-SAVE          PIC  X(02).
           12  W-EDIT-DATE-1.
               16  W-ED1-MM            PIC X(02).
               16  FILLER              PIC X(01)    VALUE '/'.
               16  W-ED1-DD            PIC X(02).
               16  FILLER              PIC X(01)    VALUE '/'.
               16  W-ED1-YY            PIC X(02).
           12  W-EDIT-DATE-2.
               16  W-ED2-DD            PIC X(02).
               16  FILLER              PIC X(01)    VALUE '/'.
               16  W-ED2-MM            PIC X(02).
               16  FILLER              PIC X(01)    VALUE '/'.
               16  W-ED2-YY            PIC X(02).

           12  W-INSURED-LAST-NAME     PIC  X(15).
           12  W-INSURED-MID-INIT      PIC  X(01).
           12  W-INSURED-1ST-NAME      PIC  X(12).

           12  W-NAME-WORK.
               16  W-NW                PIC  X(01)
                   OCCURS 30 TIMES INDEXED BY W-NWA-NDX.

           12  W-NAME-WORK2.
               16  W-NW2               PIC  X(01)
                   OCCURS 20 TIMES INDEXED BY W-NWA-NDX2 W-NWA-NDX3.


           12  W-PGM-NAME              PIC  X(08).
           12  W-PHONE-IN              PIC  9(11) VALUE ZEROS.
           12  W-PHONE-IN-R   REDEFINES W-PHONE-IN.
               16  FILLER              PIC  9(01).
               16  W-PI-AREA           PIC  9(03).
               16  W-PI-PFX            PIC  9(03).
               16  W-PI-SFX            PIC  9(04).
           12  W-PHONE-OUT.
               16  W-PO-AREA           PIC  X(03).
               16  FILLER              PIC  X(01) VALUE '-'.
               16  W-PO-PFX            PIC  X(03).
               16  FILLER              PIC  X(01) VALUE '-'.
               16  W-PO-SFX            PIC  X(04).

           12  W-SAVE-PROD-RECORD      PIC  X(2000) VALUE SPACES.
           12  W-SAVE-ACCT-RECORD      PIC  X(2000) VALUE SPACES.
           12  W-SAVE-BIN-DATE         PIC  X(02) VALUE SPACES.
           12  W-SAVE-DATE             PIC  X(08) VALUE SPACES.

           12  W-SINGLE-LINE           PIC  X(70).
           12  W-SINGLE-LINE-BY-1 REDEFINES W-SINGLE-LINE.
               16  W-ONE-CHAR OCCURS 70 TIMES INDEXED BY NDX1 NDX2
                                       PIC  X(01).
           12  W-STATE-LINE            PIC  X(01) VALUE 'N'.
           12  W-TEMP-AREA1.
               16  W-TEMP-1 OCCURS 29 TIMES INDEXED BY W-TA1
                                       PIC  X(01).
           12  W-TEMP-AREA2.
               16  W-TEMP-2 OCCURS 30 TIMES INDEXED BY W-TA2 W-TA21
                                                       W-MOVE-NDX
                                       PIC  X(01).

           12  W-TEMP-CURR-LINE        PIC S9(03)   COMP-3.
           12  W-TIME-IN               PIC S9(07).
           12  W-TIME-OUT-R REDEFINES W-TIME-IN.
               16  FILLER              PIC  X(01).
               16  W-TIME-OUT          PIC  9(02)V9(02).
               16  FILLER              PIC  X(02).
           12  W-VAR-HOLD.
               16  W-V1                PIC  X(01).
               16  W-V2                PIC  X(01).
               16  W-V3                PIC  X(01).
               16  W-V4                PIC  X(01).
           12  W-V-HOLD REDEFINES W-VAR-HOLD.
               16  W-V-NUM             PIC  9(02).
               16  W-V-PERIOD          PIC  X(01).
               16  W-V-DECIMAL         PIC  9(01).

           12  W-WORD-LENGTH           PIC S9(04)  COMP-3.

           12  W-ZIP-CODE.
               16  W-AM-ZIP-CODE       PIC  X(05).
               16  W-AM-ZIP-DASH       PIC  X(01).
               16  W-AM-ZIP-PLUS4      PIC  X(04).
           12  W-ZIP-CODE-CANADIAN   REDEFINES  W-ZIP-CODE.
               16  W-CAN-POSTAL-1      PIC  X(03).
               16  FILLER              PIC  X(01).
               16  W-CAN-POSTAL-2      PIC  X(03).
               16  FILLER              PIC  X(03).

      *01  SYSTEM-VARIABLES            PIC  X(3272).
      *01  SYS-VAR-ENTRY REDEFINES SYSTEM-VARIABLES.
      *    12  SYS-VAR-CODE            PIC  X(04).
      *    12  SYS-VAR-LEN             PIC  9(02).
      *    12  SYS-VAR-DATA            PIC  X(100).

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +350.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
       LINKAGE SECTION.

       01  SYSTEM-VARIABLES            PIC  X(3272).
       01  SYS-VAR-ENTRY REDEFINES SYSTEM-VARIABLES.
           12  SYS-VAR-CODE            PIC  X(04).
           12  SYS-VAR-LEN             PIC  9(02).
           12  SYS-VAR-DATA            PIC  X(100).


       PROCEDURE DIVISION USING SYSTEM-VARIABLES.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           display " Entering EL350PE.cbl "
           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0120-START-ELMSTR   THRU 0120-EXIT
           PERFORM 0110-READ-ELMSTR    THRU 0110-EXIT
DAN   *    PERFORM 0150-READ-UPLOAD    THRU 0150-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
               THERE-ARE-NO-MORE-RECORDS

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           MOVE ELMSTR-CNT             TO DISP-CNT
           DISPLAY ' OPEN CLAIM CNT '  DISP-CNT
           DISPLAY ' EXTRACT RECS OUT  ' WS-EXT-COUNT
           DISPLAY ' DUPLICATE CLM NOS ' WS-DUP-COUNT

           GOBACK

             .
       0010-INITIALIZE.

           ACCEPT WS-TIME-OF-DAY       FROM  TIME

           MOVE WS-TIME                TO WS-CURRENT-TIME
           MOVE ';'                    TO EXT-TAB1
                                          EXT-TAB2
                                          EXT-TAB3
                                          EXT-TAB3A
                                          EXT-TAB4
                                          EXT-TAB5
                                          EXT-TAB6
                                          EXT-TAB7
                                          EXT-TAB8
           MOVE WS-EXT-RECORD          TO WS-INIT-EXT-REC

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ERACCT
                      ELBENE
                      ELCERT
                      ERCOMP
                      ELLETR
                      ELCNTL
      *               ELMSTR
      *               UPLOAD-FILE
               I-O
                      ELARCH
      *               ELACTQ
      *               ELCNTL
                      ELMSTR
                      ELTRLR
               OUTPUT EXTRACT

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERACCT OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERACCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCNTL-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCNTL OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELACTQ-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELACTQ OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELACTQ-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELTRLR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELTRLR OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELTRLR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELARCH-FILE-STATUS NOT = '00' AND '97' AND '05'
              MOVE ' ELARCH OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELARCH-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELBENE-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELBENE OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELBENE-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCERT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCERT OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELCERT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELMSTR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELMSTR OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELMSTR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERCOMP OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERCOMP-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELLETR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELLETR OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELLETR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE      ERACCT EXTRACT
                      ELCNTL
                      ELTRLR
                      ELARCH
                      ELBENE
                      ELCERT
                      ELMSTR
                      ERCOMP
                      ELLETR
      *               ELACTQ
      *               UPLOAD-FILE


           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERACCT CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERACCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCNTL-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCNTL CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELACTQ-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELACTQ CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELACTQ-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELTRLR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELMSTR CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELMSTR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELARCH-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELARCH CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELARCH-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELBENE-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELBENE CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELBENE-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCERT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCERT CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELCERT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELMSTR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELMSTR CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELMSTR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERCOMP CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERCOMP-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELLETR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELLETR CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELLETR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       0030-EXIT.
           EXIT.

       0050-PROCESS-FILE.
      * if claim file was set up on or after 4/1/08 (a261) then skip
      * if claim was set up after 3/31/07 (a0df) then it had to be
      * open during the period
      * or if claim is open now then it had to be open during period
      * or if it was closed during period it had to be open during period

           IF (CL-FILE-ESTABLISH-DT < X'A261')
              AND (CL-CLAIM-TYPE NOT = 'L')
              IF (CL-FILE-ESTABLISH-DT > X'A0DF')
                 OR (CL-CLAIM-STATUS = 'O')
                 OR (CL-LAST-CLOSE-DT > X'A0DF')
                 PERFORM 0060-CONTINUE THRU 0060-EXIT
                 ADD +1                TO ELMSTR-CNT
              END-IF
           END-IF
           
           PERFORM 0110-READ-ELMSTR    THRU 0110-EXIT
DAN   *    PERFORM 0150-READ-UPLOAD    THRU 0150-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-CONTINUE.

           PERFORM 2000-CREATE-LETTER  THRU 2999-EXIT

           PERFORM 6000-ARCHIVE-LETTER THRU 6000-EXIT

PEMTST*    IF NOT PENDING-PAYMENTS
PEMTST*       PERFORM 0130-DELETE-ELACTQ
PEMTST*                                THRU 0130-EXIT
PEMTST*    else
PEMTST*       move spaces              to aq-pending-letter-flag
PEMTST*       PERFORM 0140-rewritE-ELACTQ
PEMTST*                                THRU 0140-EXIT
PEMTST*    END-IF

           .

       0060-EXIT.
           EXIT.

       2000-CREATE-LETTER.
      ***************************************************************
      *    THIS ROUTINE WILL CREATE A NEW LETTER BY READING THE     *
      *    TEXT FILE WITH THE FORM CODE FROM THE ELACTQ RECORD.     *
      *    ALL VARIABLE SYMBOLS WILL BE RESOLVED AND THE LETTER     *
      *    WILL BE WRITTEN TO THE ELARCH FILE.                      *
      *                                                             *
      ***************************************************************

           MOVE SPACES                 TO W-RECORD-TABLE

           PERFORM 3000-READ-ADDR      THRU 3999-EXIT

      *    IF PI-FATAL-ERROR
      *       GO TO 2999-EXIT
      *    END-IF

           SET W-TB-NDX                TO 7
           MOVE W-TOP-FORM             TO W-REC-TEXT (W-TB-NDX)
           SET W-TB-NDX UP BY 1

           MOVE LOW-VALUES             TO TX-CONTROL-PRIMARY
PEMTST     MOVE CL-COMPANY-CD          TO TX-COMPANY-CD
PEMTST*    MOVE AQ-AUTO-LETTER         TO TX-LETTER-NO
PEMTST     MOVE 'PRCV'                 TO TX-LETTER-NO
dan   *    MOVE x'04'                  TO TX-COMPANY-CD
dan   *    MOVE 'PRCN'                 TO TX-LETTER-NO


           START ELLETR KEY IS NOT LESS THAN tx-CONTROL-PRIMARY

           IF ELLETR-FILE-STATUS = '10' OR '23'
              GO TO 2120-ENDBR
           ELSE
              IF ELLETR-FILE-STATUS NOT = '00'
                 MOVE ' ELLETR START ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELLETR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           .

       2110-READ-NEXT.

           IF W-TB-NDX GREATER THAN W-MAX-LINES
              DISPLAY 'W-TB-NDX > W-MAX-LINES '
              GO TO 2120-ENDBR
           END-IF

           READ ELLETR NEXT RECORD
           IF ELLETR-FILE-STATUS = '10' OR '23'
              GO TO 2120-ENDBR
           ELSE
              IF ELLETR-FILE-STATUS NOT = '00'
                 MOVE ' ELLETR READ  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELLETR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

PEMTST*    IF (AQ-COMPANY-CD = TX-COMPANY-CD) AND
PEMTST*       (AQ-AUTO-LETTER = TX-LETTER-NO)
PEMTST     IF (CL-COMPANY-CD = TX-COMPANY-CD) AND
PEMTST        (TX-LETTER-NO = 'PRCV')
DAN   *    IF (X'04'         = TX-COMPANY-CD) AND
DAN   *       (TX-LETTER-NO = 'PRCN')
              CONTINUE
           ELSE
              GO TO 2120-ENDBR
           END-IF

           IF TX-LINE-SQUEEZE-CONTROL EQUAL 'Z'
              PERFORM 2800-PROCESS-Z-CONTROLS
                                       THRU 2800-EXIT
      *       IF PI-FATAL-ERROR
      *          GO TO 2999-EXIT
      *       ELSE
              GO TO 2110-READ-NEXT
      *       END-IF
           END-IF

           MOVE TX-TEXT-LINE           TO W-REC-TEXT (W-TB-NDX)
           MOVE TX-PROCESS-CONTROL     TO W-REC-PC (W-TB-NDX)
                                          W-NDX-WORK
           SET W-TB-NDX UP BY 1

           IF W-NDX-WORK = 99
              MOVE W-TOP-FORM          TO W-REC-TEXT (W-TB-NDX)
              SET W-TB-NDX UP BY 1
              GO TO 2110-READ-NEXT
           ELSE
              SET W-TB-NDX UP BY W-NDX-WORK
              GO TO 2110-READ-NEXT
           END-IF

           .
       2120-ENDBR.


           IF W-TB-NDX = 8
DAN****       DISPLAY ' W-TB-NDX = 8 '
              GO TO 2999-EXIT
           END-IF

           SET W-TB-NDX DOWN BY 1
           SET W-TOTAL-LINES           TO W-TB-NDX
      *    MOVE 1                      TO W-CURRENT-LINE


           PERFORM 7000-RESOLVE-VARIABLES
                                       THRU 7399-EXIT

           PERFORM 7800-VARIABLE-SEARCH
                                       THRU 7899-EXIT
                   VARYING
               W-TB-NDX FROM 7 BY 1
                   UNTIL
               W-TB-NDX > W-TOTAL-LINES
           GO TO 2999-EXIT

           .
       2800-PROCESS-Z-CONTROLS.

           MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA

PEMTST*    MOVE AQ-RESEND-DATE         TO WS-RESEND-DATE
PEMTST     MOVE LOW-VALUES             TO WS-RESEND-DATE
           IF WS-RESEND-DATE = LOW-VALUES
              IF (W-DAYS-TO-RESEND-1 NUMERIC) AND
                 (W-DAYS-TO-RESEND-1 > ZEROS)
                 MOVE '6'              TO DC-OPTION-CODE
                 MOVE W-SAVE-BIN-DATE  TO DC-BIN-DATE-1
                 MOVE ZEROS            TO DC-ELAPSED-MONTHS
                 MOVE W-DAYS-TO-RESEND-1
                                       TO DC-ELAPSED-DAYS
                 PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-exit
                 IF NO-CONVERSION-ERROR
                    MOVE DC-BIN-DATE-2 TO WS-RESEND-DATE
                 ELSE
                    DISPLAY ' RESEND DATE ERROR '
                 END-IF
              END-IF
           END-IF

PEMTST*    MOVE AQ-FOLLOWUP-DATE       TO WS-FOLLOW-UP-DATE
PEMTST     MOVE LOW-VALUES             TO WS-FOLLOW-UP-DATE
           IF WS-FOLLOW-UP-DATE = LOW-VALUES
              IF (W-DAYS-TO-FOLLOW-UP NUMERIC) AND
                 (W-DAYS-TO-FOLLOW-UP > ZEROS)
                 MOVE '6'              TO DC-OPTION-CODE
                 MOVE W-SAVE-BIN-DATE  TO DC-BIN-DATE-1
                 MOVE ZEROS            TO DC-ELAPSED-MONTHS
                 MOVE W-DAYS-TO-FOLLOW-UP
                                       TO DC-ELAPSED-DAYS
                 PERFORM 8510-date-conversion
                                       THRU 8590-exit
                 IF NO-CONVERSION-ERROR
                    MOVE DC-BIN-DATE-2 TO WS-FOLLOW-UP-DATE
                 ELSE
                    DISPLAY 'FOLLOW UP DATE ERROR'
                 END-IF
              END-IF
           END-IF


           .
       2800-EXIT.
           EXIT.


       2999-EXIT.
           EXIT.
                                       EJECT
       3000-READ-ADDR.


      *    MOVE AQ-COMPANY-CD          TO CL-COMPANY-CD
      *    MOVE AQ-CARRIER             TO CL-CARRIER
      *    MOVE AQ-CLAIM-NO            TO CL-CLAIM-NO
      *    MOVE AQ-CERT-NO             TO CL-CERT-NO
      *    READ ELMSTR
      *    IF ELMSTR-FILE-STATUS = '10' OR '23'
      *       GO TO 3999-exit
      *    ELSE
      *       IF ELMSTR-FILE-STATUS NOT = '00'
      *          MOVE ' ELMSTR READ  ERROR '
      *                                TO WS-ABEND-MESSAGE
      *          MOVE ELMSTR-FILE-STATUS
      *                                TO WS-ABEND-FILE-STATUS
      *          PERFORM ABEND-PGM
      *       END-IF
      *    END-IF.

       3999-EXIT.
            EXIT.


       6000-ARCHIVE-LETTER.
      ***************************************************************
      *    THIS ROUTINE WILL BE USED WHEN THE LETTER HAS BEEN       *
      *    COMPLETED AND IS TO BE PUT AS PERMANENT RECORDS ONTO     *
      *    THE ARCHIVE FILE.                                        *
      *    THE FOLLOWING FUNCTIONS WILL BE PERFORMED                *
      *        1. MAKE SURE THERE ARE NO UNRESOLVED SYMBOLS         *
      *        2. GET THE ARCHIVE NUMBER FROM THE CONTROL FILE.     *
      *        3. WRITE THE NEW ARCHIVE RECORDS FROM TS-TABLE.      *
      *        4. BUILD A CORRESPONDENCE TRAILER.                   *
      *        5. BUILD OR UPDATE THE ACTIVITY QUE FILE WITH THE    *
      *           ARCHIVE NUMBER IF IT IS TO BE PRINTED LATER.      *
      *        6. RESET ALL CONTROL FIELDS AND RETURN THE           *
      *           ARCHIVE NUMBER USED TO FILE THE RECORDS.          *
      ***************************************************************

           MOVE +0                     TO TALLY-ctr
           INSPECT W-RECORD-TABLE TALLYING TALLY-ctr
                                       FOR CHARACTERS BEFORE '@'.

           IF TALLY-ctr < +21900
              DISPLAY 'TALLY < 21900, UNRESOLVED SYSMBOLS'
           END-IF

           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '1'                    TO CF-RECORD-TYPE
           MOVE ZEROS                  TO CF-SEQUENCE-NO
           MOVE SPACES                 TO CF-ACCESS-CD-GENL


           IF ELCNTL-READ = ' '
           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              GO TO 6100-NOT-FOUND
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 MOVE ' ELCNTL READ  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELCNTL-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
PEMTST        ELSE
PEMTST           MOVE 'Y' TO ELCNTL-READ
                 ADD 1                 TO CF-CO-ARCHIVE-COUNTER
                 MOVE CF-CO-ARCHIVE-COUNTER
                                       TO LA-ARCHIVE-NO
                                          WS-ARCHIVE-NO
                 MOVE CF-PRINT-ADDRESS-LABELS
                                       TO W-LABELS-SW
              END-IF
           END-IF
           END-IF

           ADD 1                       TO WS-ARCHIVE-NO
PEMTST*    REWRITE CONTROL-FILE
           IF ELCNTL-FILE-STATUS NOT = '00'
              MOVE ' ELCNTL REWRITE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF


           PERFORM 6500-BUILD-CORRESPOND THRU 6599-EXIT.

           PERFORM 6700-BUILD-EXTRACT    THRU 6700-EXIT

           MOVE SPACES                 TO LETTER-ARCHIVE
           MOVE 'LA'                   TO LA-RECORD-ID
      *    MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
           MOVE WS-ARCHIVE-NO          TO LA-ARCHIVE-NO-A1
                                          LA-ARCHIVE-NO
           MOVE '1'                    TO LA-RECORD-TYPE
                                          LA-RECORD-TYPE-A1
           MOVE ZEROS                  TO LA-LINE-SEQ-NO
                                          LA-LINE-SEQ-NO-A1
           MOVE CL-COMPANY-CD          TO LA-COMPANY-CD
                                          LA-COMPANY-CD-A1
           MOVE CL-CARRIER             TO LA-CARRIER
           MOVE CL-CLAIM-NO            TO LA-CLAIM-NO
           MOVE CL-CERT-NO             TO LA-CERT-NO

           IF (W-NUMBER-OF-COPIES NUMERIC) AND
              (W-NUMBER-OF-COPIES > ZEROS)
               MOVE W-NUMBER-OF-COPIES
                                       TO LA-NO-OF-COPIES
           ELSE
              MOVE  1                  TO LA-NO-OF-COPIES
           END-IF

           MOVE WS-RESEND-DATE         TO LA-RESEND-DATE

           MOVE 'E350'                 TO LA-PROCESSOR-CD
pemmod*    MOVE W-CURRENT-SAVE         TO LA-CREATION-DT
           MOVE bin-run-date           TO LA-CREATION-DT
           MOVE LOW-VALUES             TO LA-INITIAL-PRINT-DATE
                                          LA-RESEND-PRINT-DATE
           MOVE W-CORR-TRLR-SEQ        TO LA-CORR-TRLR-SEQ
           PERFORM 6400-WRITE-ARCHIVE  THRU 6400-EXIT

           IF W-LABELS-SW EQUAL        TO 'N'
              CONTINUE
           ELSE
              SET W-TB-NDX             TO 1
              MOVE ZEROS               TO W-SEQ-COUNTER
              PERFORM 6300-FORMAT-ADDRESS
                                       THRU 6300-EXIT VARYING
                   W-TB-NDX FROM 1 BY 1  UNTIL
                   W-TB-NDX > 6
           END-IF

           MOVE ZEROS                  TO W-SEQ-COUNTER
           PERFORM 6200-FORMAT-TEXT    THRU 6200-EXIT
                   VARYING
               W-TB-NDX FROM 8 BY 1
                   UNTIL
               W-TB-NDX GREATER THAN W-TOTAL-LINES
           .
       6000-EXIT.
           EXIT.

       6100-NOT-FOUND.

           DISPLAY ' COMPANY RECORD, ELCNTL, NOT FOUND'
           GO TO 6000-EXIT.

       6200-FORMAT-TEXT.

           MOVE SPACES                 TO LETTER-ARCHIVE.
           MOVE '3'                    TO LA-RECORD-TYPE
                                          LA-RECORD-TYPE-A1.
           MOVE 'LA'                   TO LA-RECORD-ID.
           MOVE WS-ARCHIVE-NO          TO LA-ARCHIVE-NO
                                          LA-ARCHIVE-NO-A1.
           MOVE W-SEQ-COUNTER          TO LA-LINE-SEQ-NO
                                          LA-LINE-SEQ-NO-A1.
           MOVE CL-COMPANY-CD          TO LA-COMPANY-CD
                                          LA-COMPANY-CD-A1.
           MOVE W-REC-TEXT (W-TB-NDX)  TO LA-TEXT-LINE.
           SET W-TB-NDX1               TO W-TB-NDX.
           SET W-TB-NDX1 UP BY 1.
           MOVE ZEROS                  TO W-NDX-WORK.

       6200-LOOP.

           IF  W-TB-NDX1 LESS THAN W-TOTAL-LINES
                   AND
               W-REC-TEXT (W-TB-NDX1) = SPACES
               SET W-TB-NDX1 UP BY 1
               ADD 1                   TO W-NDX-WORK
               GO TO 6200-LOOP.

           IF  W-REC-TEXT (W-TB-NDX1) = W-TOP-FORM
               MOVE '99'               TO LA-SKIP-CONTROL
               SET W-TB-NDX1 UP BY 1

           ELSE
               MOVE W-NDX-WORK         TO LA-SKIP-CONTROL.

           SET W-TB-NDX                TO W-TB-NDX1.
           SET W-TB-NDX DOWN BY 1.
           PERFORM 6400-WRITE-ARCHIVE THRU 6400-EXIT.
           ADD 1 TO W-SEQ-COUNTER.

       6200-EXIT.
            EXIT.
                                       EJECT
       6300-FORMAT-ADDRESS.

           MOVE SPACES                 TO LETTER-ARCHIVE.
           MOVE '2'                    TO LA-RECORD-TYPE
                                          LA-RECORD-TYPE-A1.
           MOVE 'LA'                   TO LA-RECORD-ID.
           MOVE WS-ARCHIVE-NO          TO LA-ARCHIVE-NO
                                          LA-ARCHIVE-NO-A1.
           MOVE W-SEQ-COUNTER          TO LA-LINE-SEQ-NO
                                          LA-LINE-SEQ-NO-A1.
           MOVE CL-COMPANY-CD          TO LA-COMPANY-CD
                                          LA-COMPANY-CD-A1.
           MOVE W-REC-TEXT (W-TB-NDX)  TO LA-ADDRESS-LINE.
           PERFORM 6400-WRITE-ARCHIVE THRU 6400-EXIT.
           ADD 1 TO W-SEQ-COUNTER.

       6300-EXIT.
            EXIT.
                                       EJECT
       6400-WRITE-ARCHIVE.


           WRITE LETTER-ARCHIVE
           IF ELARCH-FILE-STATUS = '22'
              GO TO 6420-ARCH-DUPREC
           ELSE
              IF ELARCH-FILE-STATUS NOT = '00'
                 MOVE ' ELARCH  WRITE  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELARCH-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
           .
       6400-EXIT.
           EXIT.

       6420-ARCH-DUPREC.

           DISPLAY ' ELARCH DUP RECORD  '
           GO TO 6400-EXIT
           .
       6500-BUILD-CORRESPOND.
      ***************************************************************
      *    THIS ROUTINE WILL GET THE TRAILER SEQUENCE NUMBER FROM   *
      *    THE CLAIM MASTER AND BUILD A CORRESPONDENCE TRAILER      *
      *    USING THE NEW SEQUENCE NUMBER.                           *
      *    INPUT DATA FROM THE SCREEN IS USED TO CREATE THE NEW     *
      *    TRAILER RECORD.                                          *
      ***************************************************************

      *    MOVE AQ-COMPANY-CD          TO CL-COMPANY-CD
      *    MOVE AQ-CARRIER             TO CL-CARRIER
      *    MOVE AQ-CLAIM-NO            TO CL-CLAIM-NO
      *    MOVE AQ-CERT-NO             TO CL-CERT-NO
      *    READ ELMSTR
      *    IF ELMSTR-FILE-STATUS = '10' OR '23'
      *       MOVE ' ELMSTR  READ   ERROR '
      *                                TO WS-ABEND-MESSAGE
      *       MOVE ELMSTR-FILE-STATUS
      *                                TO WS-ABEND-FILE-STATUS
      *       PERFORM ABEND-PGM
      *    ELSE
      *       IF ELMSTR-FILE-STATUS NOT = '00'
      *          MOVE ' ELMSTR  read   ERROR '
      *                                TO WS-ABEND-MESSAGE
      *          MOVE ELMSTR-FILE-STATUS
      *                                TO WS-ABEND-FILE-STATUS
      *          PERFORM ABEND-PGM
      *       END-IF
      *    END-IF


           SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.

           IF WS-FOLLOW-UP-DATE > CL-NEXT-FOLLOWUP-DT
              MOVE WS-FOLLOW-UP-DATE   TO CL-NEXT-FOLLOWUP-DT
           END-IF

           IF WS-RESEND-DATE > CL-NEXT-FOLLOWUP-DT
              MOVE WS-RESEND-DATE      TO CL-NEXT-FOLLOWUP-DT
           END-IF

           MOVE '2'                    TO CL-LAST-MAINT-TYPE


           MOVE 'AT'                   TO AT-RECORD-ID
           MOVE  4                     TO AT-TRAILER-TYPE
pemmod*    MOVE W-CURRENT-SAVE         TO AT-RECORDED-DT
pemmod     MOVE bin-run-date           TO AT-RECORDED-DT
                                          CL-LAST-MAINT-DT
                                          AT-CORR-LAST-MAINT-DT
           MOVE 'E350'                 TO AT-RECORDED-BY
                                          CL-LAST-MAINT-USER
                                          AT-CORR-LAST-UPDATED-BY

           MOVE WS-CURRENT-TIME        TO AT-LAST-MAINT-HHMMSS
                                          CL-LAST-MAINT-HHMMSS
           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE CL-CLAIM-NO            TO AT-CLAIM-NO
           MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO
                                          W-CORR-TRLR-SEQ
pemmod*    MOVE W-CURRENT-SAVE         TO AT-LETTER-SENT-DT
pemmod     MOVE bin-run-date           TO AT-LETTER-SENT-DT
           MOVE WS-FOLLOW-UP-DATE      TO AT-RECEIPT-FOLLOW-UP
           MOVE WS-RESEND-DATE         TO AT-AUTO-RE-SEND-DT
           MOVE LOW-VALUES             TO AT-LETTER-ANSWERED-DT
                                          AT-LETTER-PURGED-DT
           MOVE WS-ARCHIVE-NO          TO AT-LETTER-ARCHIVE-NO
           MOVE '1'                    TO AT-LETTER-ORIGIN

      *    MOVE PI-FORM-NUMBER         TO AT-STD-LETTER-FORM
PEMTST*    MOVE AQ-AUTO-LETTER         TO AT-STD-LETTER-FORM
PEMTST     MOVE 'PRCV'                 TO AT-STD-LETTER-FORM

      *    IF PI-REASON GREATER THAN LOW-VALUES
      *       MOVE PI-REASON           TO AT-REASON-TEXT
      *    ELSE
           MOVE SPACES                 TO AT-REASON-TEXT
      *    END-IF

      *    MOVE W-PI-ADDR-SEQ          TO AT-ADDRESS-REC-SEQ-NO.
           MOVE ZEROS                  TO AT-ADDRESS-REC-SEQ-NO

      *    IF PI-ADDR-TYPE = LOW-VALUES
           MOVE SPACES                 TO AT-ADDRESEE-TYPE
      *    ELSE
      *       MOVE PI-ADDR-TYPE        TO AT-ADDRESEE-TYPE
      *    END-IF

           MOVE W-REC-TEXT (1)         TO AT-ADDRESSEE-NAME

           MOVE LOW-VALUES             TO AT-INITIAL-PRINT-DATE
                                          AT-RESEND-PRINT-DATE


           WRITE ACTIVITY-TRAILERS
           IF ELTRLR-FILE-STATUS = '22'
              GO TO 6596-ACTV-DUPREC
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  WRITE  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           REWRITE CLAIM-MASTER
           IF ELMSTR-FILE-STATUS = '22'
              GO TO 6598-REWRITE-CLAIM
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 MOVE ' ELMSTR REWRITE ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELMSTR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           GO TO 6599-EXIT
           .
       6596-ACTV-DUPREC.

           GO TO 6599-EXIT
           .

       6598-REWRITE-CLAIM.

       6599-EXIT.
            EXIT.
                                       EJECT
       6700-BUILD-EXTRACT.
      ***************************************************************
      *    THIS ROUTINE WILL GET THE INFO NEEDED TO CREATE AN       *
      *    EXTRACT RECROD AND WRITE AN EXTRACT RECORD.              *
      ***************************************************************

      *    IF (EXT-INSURED-NAME = SS10D)
      *       AND (EXT-INSURED-ADDR1 = SS11-1D)
      *       AND (EXT-INSURED-ADDR2 = SS11-2D)


           IF EXT-CLAIM-NO = CL-CLAIM-NO
              ADD 1                    TO WS-DUP-COUNT
              DISPLAY '*** FOUND DUPLICATE *** '
              DISPLAY '       ' CL-CLAIM-NO ' ' SS10D SS11-1D SS11-2D
              DISPLAY '       ' EXT-CLAIM-NO ' ' EXT-INSURED-NAME
                 EXT-INSURED-ADDR1 EXT-INSURED-ADDR2
           END-IF

           MOVE WS-INIT-EXT-REC        TO WS-EXT-RECORD
           MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO EXT-INC-DATE
           ELSE
              MOVE '99999999'          TO EXT-INC-DATE
           END-IF
           MOVE CL-CARRIER             TO EXT-CARRIER
           MOVE CL-CLAIM-NO            TO EXT-CLAIM-NO
           MOVE CL-CERT-NO             TO EXT-CERT-NO
           MOVE SS10D                  TO EXT-INSURED-NAME
           MOVE SS11-1D                TO EXT-INSURED-ADDR1
           MOVE SS11-2D                TO EXT-INSURED-ADDR2
           MOVE SS11-3D                TO EXT-INSURED-ADDR3
           MOVE SS11-4D                TO EXT-INSURED-ADDR4
           MOVE SS11-5D                TO EXT-INSURED-TRLR-NAME

           WRITE EXTRACT-RECORD        FROM WS-EXT-RECORD
           ADD 1                       TO WS-EXT-COUNT

           .
       6700-EXIT.
            EXIT.
                                       EJECT
       7000-RESOLVE-VARIABLES.
      ***************************************************************
      *    THIS ROUTINE WILL FORMAT THE SYSTEM DEFINED SYMBOLS      *
      *    WITH DATA PERTAINING        TO THE DESIGNATED CLAIM.     *
      *    THIS ROUTINE IS PERFORM THRU 7399-EXIT IN ORDER TO       *
      *    RESOLVE ALL OF THE SYMBOLS.                              *
      ***************************************************************
DAN   *    MOVE WS-CURRENT-DATE TO SS20D
DAN   *    MOVE UL-CONT-NAME    TO SS43D
DAN   *    MOVE UL-ACCT-NAME    TO SS44-1D
DAN   *    MOVE UL-ADDRESS      TO SS44-2D
DAN   *    MOVE UL-CITY-ST      TO SS44-3D
DAN   *    MOVE UL-ZIP          TO SS44-4D
DAN   *    GO TO 7399-EXIT.

PEMTST*    MOVE AQ-CLAIM-NO            TO SS34D
PEMTST     MOVE CL-CLAIM-NO            TO SS34D

           IF CL-CLAIM-TYPE = LIFE-OVERRIDE-L1
              MOVE LIFE-OVERRIDE-L6    TO SS12D
           ELSE
              MOVE AH-OVERRIDE-L6      TO SS12D
           END-IF

           MOVE CL-LAST-PMT-AMT        TO SS16D
           MOVE CL-TOTAL-PAID-AMT      TO SS18D
           MOVE CL-CAUSE-CD            TO SS19-1D
           MOVE CL-ASSOC-CERT-SEQU     TO SS54D
           MOVE CL-ASSOC-CERT-TOTAL    TO SS55D

           MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8510-date-conversion THRU 8590-exit

           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO SS13D
           else
              move spaces              to ss13d
           END-IF

           MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8510-date-conversion THRU 8590-exit

           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO SS14D
           else
              move spaces              TO SS14D
           END-IF

           MOVE CL-LAST-PMT-DT         TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8510-date-conversion THRU 8590-exit

           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO SS15D
           ELSE
              MOVE SPACES              TO SS15D
           END-IF

           IF DTE-CLAIM-PAID-THRU-TO NOT = '1'
              MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1
              MOVE SPACES              TO DC-OPTION-CODE
              PERFORM 8510-date-conversion THRU 8590-exit
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO SS17D
              ELSE
                 MOVE SPACES           TO SS17D
              END-IF
           ELSE
              MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1
              MOVE '6'                 TO DC-OPTION-CODE
              MOVE +1                  TO DC-ELAPSED-DAYS
              MOVE +0                  TO DC-ELAPSED-MONTHS
              PERFORM 8510-date-conversion THRU 8590-exit
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO SS17D
              ELSE
                 MOVE SPACES           TO SS17D
              END-IF
           END-IF

           MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8510-date-conversion THRU 8590-exit

           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO SS45D
           ELSE
              MOVE SPACES              TO SS45D
           END-IF

           MOVE SPACES                 TO SS46D
           IF CL-SSN-STATE = CL-CERT-STATE
              IF CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME
                 CONTINUE
              ELSE
                 MOVE CL-SOC-SEC-NO    TO SS46D
              END-IF
           ELSE
              MOVE CL-SOC-SEC-NO       TO SS46D
           END-IF


           PERFORM 7400-MOVE-NAME      THRU 7400-EXIT


           MOVE W-NAME-WORK            TO W-TEMP-AREA2
           PERFORM 7750-SEARCH-AND-TRANSLATE
                                       THRU 7750-EXIT
           MOVE W-TEMP-AREA2           TO SS10D

           MOVE CL-INSURED-1ST-NAME    TO W-FIRST-NAME
           MOVE CL-INSURED-LAST-NAME   TO W-LAST-NAME
           MOVE CL-INSURED-MID-INIT    TO W-MIDDLE-NAME
           PERFORM 7500-MOVE-NAME      THRU 7500-EXIT

           MOVE W-NAME-WORK            TO W-TEMP-AREA2
           PERFORM 7750-SEARCH-AND-TRANSLATE
                                       THRU 7750-EXIT
           MOVE W-TEMP-AREA2           TO SS39D

           MOVE CL-INSURED-LAST-NAME   TO SS40D

           IF INSURED-IS-FEMALE
              MOVE 'MS.'               TO SS41D
           ELSE
              MOVE 'MR.'               TO SS41D
           END-IF

           MOVE CL-COMPANY-CD          TO CM-COMPANY-CD
           MOVE CL-CARRIER             TO CM-CARRIER
           MOVE CL-CERT-GROUPING       TO CM-GROUPING
           MOVE CL-CERT-STATE          TO CM-STATE
           MOVE CL-CERT-ACCOUNT        TO CM-ACCOUNT
           MOVE CL-CERT-EFF-DT         TO CM-CERT-EFF-DT
           MOVE CL-CERT-NO             TO CM-CERT-NO

           READ ELCERT
           IF ELCERT-FILE-STATUS = '10' OR '23'
              GO TO 7024-CERT-NOT-FOUND
           ELSE
              IF ELCERT-FILE-STATUS NOT = '00'
                 MOVE ' ELCERT  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELCERT-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           MOVE CM-CERT-NO             TO SS26D
           MOVE CM-INSURED-ISSUE-AGE   TO SS33D
           MOVE CM-LOAN-BALANCE        TO SS37D
           MOVE CM-MEMBER-NO           TO SS38D

           MOVE CM-CARRIER             TO SS23D
           MOVE CM-GROUPING            TO SS24D
           MOVE CM-ACCOUNT             TO SS25D
           MOVE CM-LOAN-NUMBER         TO SS36D

           MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8510-date-conversion THRU 8590-exit

           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO SS27D
           ELSE
              MOVE SPACES              TO SS27D
           END-IF

           MOVE CM-INSURED-FIRST-NAME  TO W-FIRST-NAME
           MOVE CM-INSURED-LAST-NAME   TO W-LAST-NAME
           MOVE CM-INSURED-INITIAL2    TO W-MIDDLE-NAME
           PERFORM 7500-MOVE-NAME      THRU 7500-EXIT

           MOVE W-NAME-WORK            TO W-TEMP-AREA2
           PERFORM 7750-SEARCH-AND-TRANSLATE
                                       THRU 7750-EXIT
           MOVE W-TEMP-AREA2           TO SS57D

           MOVE CM-JT-FIRST-NAME       TO W-FIRST-NAME
           MOVE CM-JT-LAST-NAME        TO W-LAST-NAME
           MOVE CM-JT-INITIAL          TO W-MIDDLE-NAME
           PERFORM 7500-MOVE-NAME      THRU 7500-EXIT

           MOVE W-NAME-WORK            TO W-TEMP-AREA2
           PERFORM 7750-SEARCH-AND-TRANSLATE
                                       THRU 7750-EXIT
           MOVE W-TEMP-AREA2           TO SS58D

           IF CL-CLAIM-TYPE = LIFE-OVERRIDE-L1
              MOVE CM-LF-BENEFIT-CD    TO W-BEN-HOLD
              MOVE CM-LF-ORIG-TERM     TO SS29D
              MOVE CM-LF-BENEFIT-AMT   TO SS30D
              MOVE CM-POLICY-FORM-NO   TO SS32D
              MOVE CM-LF-CANCEL-DT     TO DC-BIN-DATE-1
           ELSE
              MOVE CM-AH-BENEFIT-CD    TO W-BEN-HOLD
              MOVE CM-AH-ORIG-TERM     TO SS29D
              MOVE CM-AH-BENEFIT-AMT   TO SS30D
              MOVE CM-POLICY-FORM-NO   TO SS32D
              MOVE CM-AH-CANCEL-DT     TO DC-BIN-DATE-1
           END-IF

           MOVE ZEROS                  TO W-WORK-AMOUNT
           COMPUTE W-WORK-AMOUNT =
              (CM-AH-ORIG-TERM * CM-AH-BENEFIT-AMT)
           MOVE W-WORK-AMOUNT          TO SS51D

           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8510-date-conversion THRU 8590-exit
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO SS31D
           END-IF

           MOVE ' '                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS

           MOVE SPACES                 TO SS28D
           IF CL-CLAIM-TYPE EQUAL      TO LIFE-OVERRIDE-L1
              MOVE CM-LF-LOAN-EXPIRE-DT
                                       TO DC-BIN-DATE-1
              PERFORM 8510-date-conversion THRU 8590-exit
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO SS28D
              END-IF
           ELSE
              MOVE CM-AH-LOAN-EXPIRE-DT
                                       TO DC-BIN-DATE-1
              PERFORM 8510-date-conversion THRU 8590-exit
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO SS28D
              END-IF
           END-IF

           .
       7015-READ-DIAGNOSIS-TRAILER.


           MOVE SPACES                 TO SS19D
           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +90                    TO AT-SEQUENCE-NO

           READ ELTRLR
           IF ELTRLR-FILE-STATUS = '10' OR '23'
              GO TO 7040-READ-BENEFICIARY
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF AT-TRAILER-TYPE EQUAL '6'
              MOVE AT-INFO-LINE-1      TO SS19D
           END-IF

           GO TO 7040-READ-BENEFICIARY
           .

       7024-CERT-NOT-FOUND.

           GO TO 7399-EXIT.


       7040-READ-BENEFICIARY.

           IF (CL-BENIF-ADDR-CNT = +0) AND
              (CL-BENEFICIARY = SPACES)
              GO TO 7100-READ-PHYSICIAN-ADDR
           END-IF

           MOVE CL-BENIF-ADDR-CNT      TO AT-SEQUENCE-NO
           ADD +10                     TO AT-SEQUENCE-NO


           MOVE SPACES                 TO W-LABEL-HOLD-AREA

           IF AT-SEQUENCE-NO NOT = +10
              GO TO 7060-GET-FROM-ACTIVITY
           END-IF

           MOVE CL-COMPANY-CD          TO BE-COMPANY-CD
           MOVE 'B'                    TO BE-RECORD-TYPE
           MOVE CL-BENEFICIARY         TO BE-BENEFICIARY

           READ ELBENE

           IF ELBENE-FILE-STATUS = '10' OR '23'
              GO TO 7100-READ-PHYSICIAN-ADDR
           ELSE
              IF ELBENE-FILE-STATUS NOT = '00'
                 MOVE ' ELBENE  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELBENE-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           MOVE BE-MAIL-TO-NAME        TO W-LABEL-LINES (01)
           MOVE BE-ADDRESS-LINE-1      TO W-LABEL-LINES (02)
           MOVE BE-ADDRESS-LINE-2      TO W-LABEL-LINES (03)
           MOVE BE-CITY-STATE          TO W-LABEL-LINES (04)

           MOVE SPACES                 TO W-ZIP-CODE

           IF BE-CANADIAN-POST-CODE
              MOVE BE-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE BE-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE BE-ZIP-PRIME        TO W-AM-ZIP-CODE
              IF BE-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE BE-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (05)

           MOVE BE-PHONE-NO            TO W-PHONE-IN

           GO TO 7080-SET-PHONE
           .
       7060-GET-FROM-ACTIVITY.


           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY (1:20)


           READ ELTRLR
           IF ELTRLR-FILE-STATUS = '10' OR '23'
              GO TO 7100-READ-PHYSICIAN-ADDR
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01)
           MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02)
           MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03)
           MOVE AT-CITY-STATE          TO W-LABEL-LINES (04)

           MOVE SPACES                 TO W-ZIP-CODE

           IF AT-CANADIAN-POST-CODE
              MOVE AT-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE AT-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE AT-ZIP-CODE         TO W-AM-ZIP-CODE
              IF AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE AT-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (05)

           MOVE AT-PHONE-NO            TO W-PHONE-IN
           .
       7080-SET-PHONE.

           MOVE W-PI-AREA              TO W-PO-AREA
           MOVE W-PI-PFX               TO W-PO-PFX
           MOVE W-PI-SFX               TO W-PO-SFX
           MOVE W-PHONE-OUT            TO SS44-5D

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS43D
           MOVE W-LABEL-LINES (02)     TO SS44-1D
           MOVE W-LABEL-LINES (03)     TO SS44-2D
           MOVE W-LABEL-LINES (04)     TO SS44-3D
           MOVE W-LABEL-LINES (05)     TO SS44-4D
           .
       7100-READ-PHYSICIAN-ADDR.

           MOVE CL-DOCTOR-ADDR-CNT     TO AT-SEQUENCE-NO
           ADD +30                     TO AT-SEQUENCE-NO

           IF AT-SEQUENCE-NO = +30
              GO TO 7120-READ-EMPLOYER-ADDR
           END-IF

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY (1:20)


           READ ELTRLR

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              GO TO 7120-READ-EMPLOYER-ADDR
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           MOVE SPACES                 TO W-LABEL-HOLD-AREA
           MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01)
           MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02)
           MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03)
           MOVE AT-CITY-STATE          TO W-LABEL-LINES (04)

           MOVE SPACES                 TO W-ZIP-CODE

           IF AT-CANADIAN-POST-CODE
              MOVE AT-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE AT-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE AT-ZIP-CODE         TO W-AM-ZIP-CODE
              IF AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE AT-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (05)

           MOVE ZEROS                  TO W-PHONE-IN
           MOVE AT-PHONE-NO            TO W-PHONE-IN
           MOVE W-PI-AREA              TO W-PO-AREA
           MOVE W-PI-PFX               TO W-PO-PFX
           MOVE W-PI-SFX               TO W-PO-SFX
           MOVE W-PHONE-OUT            TO SS47-5D

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS47D
           MOVE W-LABEL-LINES (02)     TO SS47-1D
           MOVE W-LABEL-LINES (03)     TO SS47-2D
           MOVE W-LABEL-LINES (04)     TO SS47-3D
           MOVE W-LABEL-LINES (05)     TO SS47-4D
           .

       7120-READ-EMPLOYER-ADDR.

           MOVE CL-EMPLOYER-ADDR-CNT   TO AT-SEQUENCE-NO
           ADD +40                     TO AT-SEQUENCE-NO

           IF AT-SEQUENCE-NO = +40
              GO TO 7140-READ-INSURED-ADDR
           END-IF

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY (1:20)


           READ ELTRLR

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              GO TO 7140-READ-INSURED-ADDR
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           MOVE SPACES                 TO W-LABEL-HOLD-AREA
           MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01)
           MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02)
           MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03)
           MOVE AT-CITY-STATE          TO W-LABEL-LINES (04)

           MOVE SPACES                 TO W-ZIP-CODE

           IF AT-CANADIAN-POST-CODE
              MOVE AT-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE AT-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE AT-ZIP-CODE         TO W-AM-ZIP-CODE
              IF AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE AT-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (05)

           MOVE ZEROS                  TO W-PHONE-IN
           MOVE AT-PHONE-NO            TO W-PHONE-IN
           MOVE W-PI-AREA              TO W-PO-AREA
           MOVE W-PI-PFX               TO W-PO-PFX
           MOVE W-PI-SFX               TO W-PO-SFX
           MOVE W-PHONE-OUT            TO SS48-5D

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS48D
           MOVE W-LABEL-LINES (02)     TO SS48-1D
           MOVE W-LABEL-LINES (03)     TO SS48-2D
           MOVE W-LABEL-LINES (04)     TO SS48-3D
           MOVE W-LABEL-LINES (05)     TO SS48-4D

           .
       7140-READ-INSURED-ADDR.

           MOVE CL-INSURED-ADDR-CNT    TO AT-SEQUENCE-NO

           IF AT-SEQUENCE-NO = +0
              GO TO 7160-READ-OTHER1-ADDR
           END-IF

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY (1:20)


           READ ELTRLR

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              GO TO 7160-READ-OTHER1-ADDR
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           MOVE SPACES                 TO W-LABEL-HOLD-AREA
           MOVE SS10D                  TO W-LABEL-LINES (01)
           MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02)
           MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03)
           MOVE AT-CITY-STATE          TO W-LABEL-LINES (04)

           MOVE SPACES                 TO W-ZIP-CODE

           IF AT-CANADIAN-POST-CODE
              MOVE AT-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE AT-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE AT-ZIP-CODE         TO W-AM-ZIP-CODE
              IF AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE AT-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (05)

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS10D
           MOVE W-LABEL-LINES (02)     TO SS11-1D
           MOVE W-LABEL-LINES (03)     TO SS11-2D
           MOVE W-LABEL-LINES (04)     TO SS11-3D
           MOVE W-LABEL-LINES (05)     TO SS11-4D
           MOVE AT-MAIL-TO-NAME        TO SS11-5D

           MOVE ZEROS                  TO W-PHONE-IN
           MOVE AT-PHONE-NO            TO W-PHONE-IN
           MOVE W-PI-AREA              TO W-PO-AREA
           MOVE W-PI-PFX               TO W-PO-PFX
           MOVE W-PI-SFX               TO W-PO-SFX
           MOVE W-PHONE-OUT            TO SS11-6D

           .
       7160-READ-OTHER1-ADDR.

           MOVE CL-OTHER-1-ADDR-CNT    TO AT-SEQUENCE-NO
           ADD +50                     TO AT-SEQUENCE-NO

           IF AT-SEQUENCE-NO = +50
              GO TO 7180-READ-OTHER2-ADDR
           END-IF

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY (1:20)

           READ ELTRLR

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              GO TO 7180-READ-OTHER2-ADDR
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE SPACES                 TO W-LABEL-HOLD-AREA
           MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01)
           MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02)
           MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03)
           MOVE AT-CITY-STATE          TO W-LABEL-LINES (04)

           MOVE SPACES                 TO W-ZIP-CODE

           IF AT-CANADIAN-POST-CODE
              MOVE AT-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE AT-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE AT-ZIP-CODE         TO W-AM-ZIP-CODE
              IF AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE AT-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (05)

           MOVE ZEROS                  TO W-PHONE-IN
           MOVE AT-PHONE-NO            TO W-PHONE-IN
           MOVE W-PI-AREA              TO W-PO-AREA
           MOVE W-PI-PFX               TO W-PO-PFX
           MOVE W-PI-SFX               TO W-PO-SFX
           MOVE W-PHONE-OUT            TO SS49-5D

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS49D
           MOVE W-LABEL-LINES (02)     TO SS49-1D
           MOVE W-LABEL-LINES (03)     TO SS49-2D
           MOVE W-LABEL-LINES (04)     TO SS49-3D
           MOVE W-LABEL-LINES (05)     TO SS49-4D
           .
       7180-READ-OTHER2-ADDR.

           MOVE CL-OTHER-2-ADDR-CNT    TO AT-SEQUENCE-NO
           ADD +60                     TO AT-SEQUENCE-NO

           IF AT-SEQUENCE-NO = +60
              GO TO 7200-NOT-FOUND
           END-IF

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY (1:20)


           READ ELTRLR

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              GO TO 7200-NOT-FOUND
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           MOVE SPACES                 TO W-LABEL-HOLD-AREA
           MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01)
           MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02)
           MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03)
           MOVE AT-CITY-STATE          TO W-LABEL-LINES (04)

           MOVE SPACES                 TO W-ZIP-CODE

           IF AT-CANADIAN-POST-CODE
              MOVE AT-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE AT-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE AT-ZIP-CODE         TO W-AM-ZIP-CODE
              IF AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE AT-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (05)

           MOVE ZEROS                  TO W-PHONE-IN
           MOVE AT-PHONE-NO            TO W-PHONE-IN
           MOVE W-PI-AREA              TO W-PO-AREA
           MOVE W-PI-PFX               TO W-PO-PFX
           MOVE W-PI-SFX               TO W-PO-SFX
           MOVE W-PHONE-OUT            TO SS50-5D

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS50D
           MOVE W-LABEL-LINES (02)     TO SS50-1D
           MOVE W-LABEL-LINES (03)     TO SS50-2D
           MOVE W-LABEL-LINES (04)     TO SS50-3D
           MOVE W-LABEL-LINES (05)     TO SS50-4D
           .
       7200-NOT-FOUND.

           IF ACCOUNT-IS-ONLINE
              GO TO 7220-READ-ACCOUNT
           END-IF

           MOVE CL-ACCOUNT-ADDR-CNT    TO AT-SEQUENCE-NO
           ADD +20                     TO AT-SEQUENCE-NO

           IF AT-SEQUENCE-NO = +20
              GO TO 7220-READ-ACCOUNT
           END-IF

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY (1:20)


           READ ELTRLR

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              GO TO 7220-READ-ACCOUNT
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           MOVE SPACES                 TO W-LABEL-HOLD-AREA
           MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01)
           MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02)
           MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03)
           MOVE AT-CITY-STATE          TO W-LABEL-LINES (04)

           MOVE SPACES                 TO W-ZIP-CODE

           IF AT-CANADIAN-POST-CODE
              MOVE AT-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE AT-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE AT-ZIP-CODE         TO W-AM-ZIP-CODE
              IF AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE AT-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (05)

           MOVE ZEROS                  TO W-PHONE-IN
           MOVE AT-PHONE-NO            TO W-PHONE-IN
           MOVE W-PI-AREA              TO W-PO-AREA
           MOVE W-PI-PFX               TO W-PO-PFX
           MOVE W-PI-SFX               TO W-PO-SFX
           MOVE W-PHONE-OUT            TO SS07-6D

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS06D
           MOVE W-LABEL-LINES (02)     TO SS07-1D
           MOVE W-LABEL-LINES (03)     TO SS07-2D
           MOVE W-LABEL-LINES (04)     TO SS07-3D
           MOVE W-LABEL-LINES (05)     TO SS07-4D
           MOVE W-LABEL-LINES (06)     TO SS07-5D
           .
       7220-READ-ACCOUNT.

      *    IF W-ACCT-READ-SW EQUAL 'Y'
      *       GO TO 7228-BUILD-ACCT-ADDR
      *    END-IF

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE CM-CONTROL-PRIMARY     TO AM-CONTROL-PRIMARY (1:20)
           MOVE CM-CERT-EFF-DT         TO AM-EXPIRATION-DT

           .
       7222-STARTBR-ACCOUNT.

           MOVE SPACES                 TO W-SAVE-ACCT-RECORD
           START ERACCT KEY IS > AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS = '10' OR '23'
              GO TO 7240-READ-3RD-PARTY
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 MOVE ' ERACCT  START  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERACCT-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       7226-READNEXT-ACCOUNT.

           READ ERACCT NEXT RECORD

           IF ERACCT-FILE-STATUS = '10' OR '23'
              GO TO 7240-READ-3RD-PARTY
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 MOVE ' ERACCT  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERACCT-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF AM-CONTROL-PRIMARY (1:20) NOT = CM-CONTROL-PRIMARY (1:20)
              IF W-SAVE-ACCT-RECORD = SPACES
                 GO TO 7240-READ-3RD-PARTY
              ELSE
                 MOVE W-SAVE-ACCT-RECORD
                                       TO ACCOUNT-MASTER
                 GO TO 7228-BUILD-ACCT-ADDR
              END-IF
           END-IF

           IF AM-EXPIRATION-DT = HIGH-VALUES
      *       MOVE AM-CONTROL-PRIMARY  TO W-ACCT-KEY
              CONTINUE
           ELSE
              MOVE ACCOUNT-MASTER      TO W-SAVE-ACCT-RECORD
              GO TO 7226-READNEXT-ACCOUNT
           END-IF
           .
       7228-BUILD-ACCT-ADDR.

           MOVE SPACES                 TO W-SAVE-ACCT-RECORD

           IF NOT ACCOUNT-IS-ONLINE
              GO TO 7240-READ-3RD-PARTY
           END-IF

           MOVE SPACES                 TO W-LABEL-HOLD-AREA
           MOVE AM-NAME                TO W-LABEL-LINES (01)
           MOVE AM-PERSON              TO W-LABEL-LINES (02)
           MOVE AM-ADDRS               TO W-LABEL-LINES (03)
           MOVE AM-CITY                TO W-LABEL-LINES (04)

           MOVE SPACES                 TO W-ZIP-CODE

           IF AM-CANADIAN-POST-CODE
              MOVE AM-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE AM-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE AM-ZIP-PRIME        TO W-AM-ZIP-CODE
              IF AM-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE AM-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (05)

           MOVE ZEROS                  TO W-PHONE-IN
           MOVE AM-AREA-CODE           TO W-PO-AREA
           MOVE AM-TEL-PRE             TO W-PO-PFX
           MOVE AM-TEL-NBR             TO W-PO-SFX
           MOVE W-PHONE-OUT            TO SS07-6D

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS06D
           MOVE W-LABEL-LINES (02)     TO SS07-1D
           MOVE W-LABEL-LINES (03)     TO SS07-2D
           MOVE W-LABEL-LINES (04)     TO SS07-3D
           MOVE W-LABEL-LINES (05)     TO SS07-4D
           MOVE W-LABEL-LINES (06)     TO SS07-5D

           .
       7240-READ-3RD-PARTY.

           MOVE +29                    TO AT-SEQUENCE-NO


           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY (1:20)

           READ ELTRLR

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              GO TO 7260-READ-COMP
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           MOVE SPACES                 TO W-LABEL-HOLD-AREA
           MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01)
           MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02)
           MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03)
           MOVE AT-CITY-STATE          TO W-LABEL-LINES (04)

           MOVE SPACES                 TO W-ZIP-CODE

           IF AT-CANADIAN-POST-CODE
              MOVE AT-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE AT-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE AT-ZIP-CODE         TO W-AM-ZIP-CODE
              IF AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE AT-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (05)

           MOVE ZEROS                  TO W-PHONE-IN
           MOVE AT-PHONE-NO            TO W-PHONE-IN
           MOVE W-PI-AREA              TO W-PO-AREA
           MOVE W-PI-PFX               TO W-PO-PFX
           MOVE W-PI-SFX               TO W-PO-SFX
           MOVE W-PHONE-OUT            TO SS53-6D

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS52D
           MOVE W-LABEL-LINES (02)     TO SS53-1D
           MOVE W-LABEL-LINES (03)     TO SS53-2D
           MOVE W-LABEL-LINES (04)     TO SS53-3D
           MOVE W-LABEL-LINES (05)     TO SS53-4D
           MOVE W-LABEL-LINES (06)     TO SS53-5D

           GO TO 7300-READ-DENIAL
           .
       7260-READ-COMP.

      *    IF W-COMP-READ-SW EQUAL 'Y'
      *       GO TO 7280-BUILD-COMP-ADDR
      *    END-IF

           IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
              MOVE ZEROS               TO AM-3RD-PARTY-NOTIF-LEVEL
              GO TO 7300-READ-DENIAL
           END-IF

           IF AM-3RD-PARTY-NOTIF-LEVEL GREATER THAN 00 AND
                   LESS THAN 11
              CONTINUE
           ELSE
              GO TO 7300-READ-DENIAL
           END-IF

           IF AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) EQUAL SPACES OR ZEROS
              GO TO 7300-READ-DENIAL
           END-IF

           MOVE AM-COMPANY-CD          TO CO-COMPANY-CD
           MOVE AM-CARRIER             TO CO-CARRIER
           MOVE AM-GROUPING            TO CO-GROUPING
           MOVE 'A'                    TO CO-TYPE
           MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
                                       TO CO-RESP-NO

           IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO
              IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR
                                            'G' OR 'B'
                  MOVE 'G'            TO CO-TYPE
                  MOVE LOW-VALUES     TO CO-ACCOUNT
               ELSE
                  MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
                                       TO CO-ACCOUNT
           ELSE
              MOVE 'G'                 TO CO-TYPE
              MOVE LOW-VALUES          TO CO-ACCOUNT
           END-IF


           IF DTE-COMPENSATION-ACCESS = '1' OR '3'
              MOVE ZEROS               TO CO-CARRIER
           END-IF

           IF DTE-COMPENSATION-ACCESS = '2' OR '3'
              MOVE ZEROS               TO CO-GROUPING
           END-IF


           READ ERCOMP

           IF ERCOMP-FILE-STATUS = '10' OR '23'
              GO TO 7300-READ-DENIAL
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 MOVE ' ERCOMP  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERCOMP-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
           .
       7280-BUILD-COMP-ADDR.

           MOVE SPACES                 TO W-LABEL-HOLD-AREA
           MOVE CO-ACCT-NAME           TO W-LABEL-LINES (01)

           IF CO-ACCT-NAME = SPACES
              MOVE CO-MAIL-NAME        TO W-LABEL-LINES (01)
           END-IF

           MOVE CO-ADDR-1              TO W-LABEL-LINES (02)
           MOVE CO-ADDR-2              TO W-LABEL-LINES (03)
           MOVE CO-ADDR-3              TO W-LABEL-LINES (04)

           MOVE SPACES                 TO W-ZIP-CODE

           IF CO-CANADIAN-POST-CODE
              MOVE CO-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE CO-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE CO-ZIP-PRIME        TO W-AM-ZIP-CODE
              IF CO-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE CO-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (05)

           MOVE ZEROS                  TO W-PHONE-IN
           MOVE CO-AREA-CODE           TO W-PO-AREA
           MOVE CO-PREFIX              TO W-PO-PFX
           MOVE CO-PHONE               TO W-PO-SFX
           MOVE W-PHONE-OUT            TO SS53-6D

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS52D
           MOVE W-LABEL-LINES (02)     TO SS53-1D
           MOVE W-LABEL-LINES (03)     TO SS53-2D
           MOVE W-LABEL-LINES (04)     TO SS53-3D
           MOVE W-LABEL-LINES (05)     TO SS53-4D
           MOVE W-LABEL-LINES (06)     TO SS53-5D
           .
       7300-READ-DENIAL.

           MOVE +90                    TO AT-SEQUENCE-NO

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY (1:20)


           START ELTRLR KEY IS NOT < AT-CONTROL-PRIMARY

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              GO TO 7340-READ-CNTL1
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR START   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           .
       7304-READ-NEXT.

           READ ELTRLR NEXT RECORD

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              GO TO 7340-READ-CNTL1
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           IF AT-CONTROL-PRIMARY (1:20) NOT = CL-CONTROL-PRIMARY
              GO TO 7340-READ-CNTL1
           END-IF

           IF AT-TRAILER-TYPE NOT = '8'
              GO TO 7304-READ-NEXT
           END-IF

           MOVE AT-DENIAL-INFO-1       TO SS35-1D
           MOVE AT-DENIAL-INFO-2       TO SS35-2D
           .
       7340-READ-CNTL1.


           IF SS35-1D = ALL '*'
              MOVE SPACES              TO SS35-1D
           END-IF

           IF SS35-2D = ALL '*'
              MOVE SPACES              TO SS35-2D
           END-IF

           MOVE SPACES                 TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '1'                    TO CF-RECORD-TYPE
           MOVE +0                     TO CF-SEQUENCE-NO


           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              GO TO 7340-READ-CNTL2
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 MOVE ' ELCNTL  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELCNTL-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           MOVE SPACES                 TO W-LABEL-HOLD-AREA
           MOVE CF-CL-MAIL-TO-NAME     TO W-LABEL-LINES (01)
           MOVE CF-CL-IN-CARE-OF       TO W-LABEL-LINES (02)
           MOVE CF-CL-ADDR-LINE-1      TO W-LABEL-LINES (03)
           MOVE CF-CL-ADDR-LINE-2      TO W-LABEL-LINES (04)
           MOVE CF-CL-CITY-STATE       TO W-LABEL-LINES (05)

           IF CF-CL-ZIP-CODE-NUM NOT NUMERIC
              MOVE ZEROS               TO CF-CL-ZIP-CODE-NUM
           END-IF

           IF CF-CL-ZIP-CODE-NUM NOT = ZEROS
              MOVE CF-CL-ZIP-CODE-NUM  TO W-ZIP-NUMERIC
              MOVE W-ZIP-NONNUM        TO CF-CL-ZIP-CODE
           END-IF

           MOVE SPACES                 TO W-ZIP-CODE

           IF CF-CL-CAN-POST-CODE
              MOVE CF-CL-CAN-POSTAL-1  TO W-CAN-POSTAL-1
              MOVE CF-CL-CAN-POSTAL-2  TO W-CAN-POSTAL-2

           ELSE
              MOVE CF-CL-ZIP-PRIME     TO W-AM-ZIP-CODE

              IF CF-CL-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE CF-CL-ZIP-PLUS4  TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (06)

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS01D
           MOVE W-LABEL-LINES (02)     TO SS02-1D
           MOVE W-LABEL-LINES (03)     TO SS02-2D
           MOVE W-LABEL-LINES (04)     TO SS02-3D
           MOVE W-LABEL-LINES (05)     TO SS02-4D
           MOVE W-LABEL-LINES (06)     TO SS02-5D
           .
       7340-READ-CNTL2.

           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '2'                    TO CF-RECORD-TYPE
           MOVE CL-PROCESSOR-ID        TO CF-ACCESS-OF-PROCESSOR
           MOVE +0                     TO CF-SEQUENCE-NO


           MOVE SPACES                 TO SS08D
                                          SS09D

           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              GO TO 7350-READ-CNTL4
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 MOVE ' ELCNTL  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELCNTL-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE CF-PROCESSOR-NAME      TO SS08D
           MOVE CF-PROCESSOR-TITLE     TO SS09D
           .
       7350-READ-CNTL4.

           IF W-BEN-HOLD = ZEROS
              GO TO 7350-READ-CNTL6
           END-IF

           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE SPACES                 TO CF-ACCESS-OF-BENEFIT
           MOVE W-BEN-HOLD             TO CF-HI-BEN-IN-REC

           IF CL-CLAIM-TYPE = LIFE-OVERRIDE-L1
              MOVE '4'                 TO CF-RECORD-TYPE
           ELSE
              MOVE '5'                 TO CF-RECORD-TYPE
           END-IF

           MOVE ZEROS                  TO CF-SEQUENCE-NO

           START ELCNTL KEY IS NOT LESS THAN CF-CONTROL-PRIMARY
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              GO TO 7350-READ-CNTL6
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 MOVE ' ELCNTL  START  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELCNTL-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           READ ELCNTL NEXT RECORD
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              GO TO 7350-READ-CNTL6
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 MOVE ' ELCNTL  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELCNTL-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE +1                     TO W-SUB
           .
       7350-LOOP.

           IF W-SUB = +9
              GO TO 7350-READ-CNTL6
           END-IF

           IF CF-BENEFIT-CODE (W-SUB) < W-BEN-HOLD
              ADD +1                   TO W-SUB
              GO TO 7350-LOOP
           END-IF

           IF W-BEN-HOLD = CF-BENEFIT-CODE (W-SUB)
              MOVE CF-BENEFIT-DESCRIP (W-SUB)
                                       TO SS22D
              IF CL-CLAIM-TYPE = AH-OVERRIDE-L1
                 MOVE CF-BENEFIT-ALPHA (W-SUB)
                                       TO W-BENEFIT-WORK
                 MOVE W-ELIM-DAYS      TO SS42D
              END-IF
           END-IF

           GO TO 7350-READ-CNTL6
           .
       7350-READ-CNTL6.

           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '6'                    TO CF-RECORD-TYPE
           MOVE SPACES                 TO CF-ACCESS-CD-GENL

           MOVE CL-CARRIER             TO CF-CARRIER-CNTL

      *    MOVE ZEROS                  TO W-CNTL-SEQ
           MOVE ZEROS                  TO CF-SEQUENCE-NO


           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              GO TO 7360-SET-DATE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 MOVE ' ELCNTL  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELCNTL-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE SPACES                 TO W-LABEL-HOLD-AREA
           MOVE CF-MAIL-TO-NAME        TO W-LABEL-LINES (01)
           MOVE CF-IN-CARE-OF          TO W-LABEL-LINES (02)
           MOVE CF-ADDRESS-LINE-1      TO W-LABEL-LINES (03)
           MOVE CF-ADDRESS-LINE-2      TO W-LABEL-LINES (04)
           MOVE CF-CITY-STATE          TO W-LABEL-LINES (05)

           IF CF-ZIP-CODE-NUM NOT NUMERIC
              MOVE ZEROS               TO CF-ZIP-CODE-NUM
           END-IF

           IF CF-ZIP-CODE-NUM NOT = ZEROS
              MOVE CF-ZIP-CODE-NUM     TO W-ZIP-NUMERIC
              MOVE W-ZIP-NONNUM        TO CF-ZIP-CODE
           END-IF

           MOVE SPACES                 TO W-ZIP-CODE

           IF CF-CANADIAN-POST-CODE
              MOVE CF-CAN-POSTAL-1     TO W-CAN-POSTAL-1
              MOVE CF-CAN-POSTAL-2     TO W-CAN-POSTAL-2
           ELSE
              MOVE CF-ZIP-PRIME        TO W-AM-ZIP-CODE
              IF CF-ZIP-PLUS4 NOT = SPACES AND  ZEROS
                 MOVE '-'              TO W-AM-ZIP-DASH
                 MOVE CF-ZIP-PLUS4     TO W-AM-ZIP-PLUS4
              END-IF
           END-IF

           MOVE W-ZIP-CODE             TO W-LABEL-LINES (06)

           MOVE ZEROS                  TO W-PHONE-IN
           MOVE CF-PHONE-NO            TO W-PHONE-IN
           MOVE W-PI-AREA              TO W-PO-AREA
           MOVE W-PI-PFX               TO W-PO-PFX
           MOVE W-PI-SFX               TO W-PO-SFX
           MOVE W-PHONE-OUT            TO SS04-6D

           PERFORM 7600-LABEL-MOVE     THRU 7600-EXIT

           MOVE W-LABEL-LINES (01)     TO SS03D
           MOVE W-LABEL-LINES (02)     TO SS04-1D
           MOVE W-LABEL-LINES (03)     TO SS04-2D
           MOVE W-LABEL-LINES (04)     TO SS04-3D
           MOVE W-LABEL-LINES (05)     TO SS04-4D
           MOVE W-LABEL-LINES (06)     TO SS04-5D

           .
       7360-SET-DATE.

           MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT
           MOVE '2'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-ALPHA TO ss21d
              move dc-bin-date-1       to ws-current-bin-date
           ELSE
              MOVE SPACES              TO SS21D
           END-IF
           MOVE WS-CURRENT-DATE        TO SS20D

           INSPECT SS19D     CONVERTING W-UPPER-CASE TO W-LOWER-CASE
           INSPECT SS35-1D   CONVERTING W-UPPER-CASE TO W-LOWER-CASE
           INSPECT SS35-2D   CONVERTING W-UPPER-CASE TO W-LOWER-CASE
           INSPECT SS22D     CONVERTING W-UPPER-CASE TO W-LOWER-CASE

           MOVE SS08D                  TO W-TEMP-AREA2
           PERFORM 7750-SEARCH-AND-TRANSLATE
                                       THRU 7750-EXIT
           MOVE W-TEMP-AREA2           TO SS08D

           MOVE SS09D                  TO W-TEMP-AREA2
           PERFORM 7750-SEARCH-AND-TRANSLATE
                                       THRU 7750-EXIT
           MOVE W-TEMP-AREA2           TO SS09D

           MOVE SS11-5D                TO W-TEMP-AREA2
           PERFORM 7750-SEARCH-AND-TRANSLATE
                                       THRU 7750-EXIT
           MOVE W-TEMP-AREA2           TO SS11-5D

           MOVE SS21D                  TO W-TEMP-AREA2
           PERFORM 7750-SEARCH-AND-TRANSLATE
                                       THRU 7750-EXIT
           MOVE W-TEMP-AREA2           TO SS21D

           MOVE SS40D                  TO W-TEMP-AREA2
           PERFORM 7750-SEARCH-AND-TRANSLATE
                                       THRU 7750-EXIT
           MOVE W-TEMP-AREA2           TO SS40D

           MOVE SS41D                  TO W-TEMP-AREA2
           PERFORM 7750-SEARCH-AND-TRANSLATE
                                       THRU 7750-EXIT
           MOVE W-TEMP-AREA2           TO SS41D

           .
       7399-EXIT.
           EXIT.
           EJECT
       7400-MOVE-NAME.

           MOVE SPACES                 TO W-NAME-WORK
                                          W-NAME-WORK2
           MOVE ZERO                   TO W-NAME-SW
           SET W-NWA-NDX               TO +1

           IF (CL-INSURED-1ST-NAME = SPACES)
                   AND
              (CL-INSURED-MID-INIT = SPACES)
              MOVE +1                  TO W-NAME-SW
           END-IF

           MOVE CL-INSURED-LAST-NAME   TO W-NAME-WORK2
           PERFORM 7420-MOVE-NAME      THRU 7429-EXIT

           MOVE CL-INSURED-1ST-NAME    TO W-NAME-WORK2
           PERFORM 7420-MOVE-NAME      THRU 7429-EXIT

           SET W-NWA-NDX UP BY +1.
           MOVE CL-INSURED-MID-INIT    TO W-NAME-WORK2
           PERFORM 7420-MOVE-NAME      THRU 7429-EXIT

           .
       7400-EXIT.
           EXIT.
           EJECT
       7420-MOVE-NAME SECTION.

           IF  W-NAME-SW GREATER THAN +1
               GO TO 7429-EXIT.

           IF  W-NAME-WORK2 = SPACES
               GO TO 7429-EXIT.

           SET W-NWA-NDX2              TO +1.
           SET W-NWA-NDX3              TO +2.

       7422-MOVE-NAME.

           MOVE W-NW2 (W-NWA-NDX2)   TO W-NW (W-NWA-NDX).

           IF  W-NWA-NDX LESS THAN +30
               SET W-NWA-NDX UP BY +1

           ELSE
               ADD +2                  TO W-NAME-SW
               GO TO 7429-EXIT.

           IF  W-NWA-NDX2 LESS THAN +20
               SET W-NWA-NDX3 UP BY +1
               SET W-NWA-NDX2 UP BY +1.

           IF  W-NW2 (W-NWA-NDX2) = SPACES
                   AND
               W-NW2 (W-NWA-NDX3) = SPACES

               IF  W-NAME-SW = ZERO
                   MOVE ','            TO W-NW (W-NWA-NDX)
                   SET W-NWA-NDX UP BY +2
                   MOVE +1             TO W-NAME-SW
                   GO TO 7429-EXIT

               ELSE
                   GO TO 7429-EXIT.

           GO TO 7422-MOVE-NAME.

       7429-EXIT.
           EXIT.
                                       EJECT
       7500-MOVE-NAME.

           MOVE SPACES                 TO W-NAME-WORK
                                          W-NAME-WORK2.
           MOVE ZERO                   TO W-NAME-SW.
           SET W-NWA-NDX               TO +1.

           IF  W-FIRST-NAME = SPACES
                   AND
               W-MIDDLE-NAME = SPACES
               MOVE W-LAST-NAME
                                       TO W-NAME-WORK
               GO TO 7500-EXIT.

           MOVE W-FIRST-NAME           TO W-NAME-WORK2.
           PERFORM 7520-MOVE-NAME THRU 7529-EXIT.

           SET W-NWA-NDX UP BY +1.

           IF  W-MIDDLE-NAME NOT = SPACES
               MOVE W-MIDDLE-NAME      TO W-NW (W-NWA-NDX)
               SET W-NWA-NDX UP BY +1
               MOVE '.'                TO W-NW (W-NWA-NDX)
               SET W-NWA-NDX UP BY +2.

           MOVE W-LAST-NAME            TO W-NAME-WORK2.
           PERFORM 7520-MOVE-NAME THRU 7529-EXIT.

       7500-EXIT.
           EXIT.
                                       EJECT
       7520-MOVE-NAME SECTION.

           IF  W-NAME-SW GREATER THAN +1
               GO TO 7529-EXIT.

           IF  W-NAME-WORK2 = SPACES
               GO TO 7529-EXIT.

           SET W-NWA-NDX2              TO +1.
           SET W-NWA-NDX3              TO +2.

       7522-MOVE-NAME.

           MOVE W-NW2 (W-NWA-NDX2)     TO W-NW (W-NWA-NDX).

           IF  W-NWA-NDX LESS THAN +30
               SET W-NWA-NDX UP BY +1

           ELSE
               ADD +2                 TO W-NAME-SW
               GO TO 7529-EXIT.

           IF  W-NWA-NDX2 LESS THAN +20
               SET W-NWA-NDX2 UP BY +1
               SET W-NWA-NDX3 UP BY +1.

           IF  W-NW2 (W-NWA-NDX2) = SPACES
                   AND
               W-NW2 (W-NWA-NDX3) = SPACES
               GO TO 7529-EXIT.

           GO TO 7522-MOVE-NAME.

       7529-EXIT.
           EXIT.
                                       EJECT
       7600-LABEL-MOVE.

           IF  W-LABEL-HOLD-AREA = SPACES
               GO TO 7600-EXIT.

           PERFORM 7700-TRANSLATE-LOWER THRU 7700-EXIT.

           IF  W-LABEL-LINES (1) = SPACES
               MOVE W-LABEL-LINES (2)  TO W-LABEL-LINES (1)
               MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
               MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
               MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
               MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
               MOVE SPACES             TO W-LABEL-LINES (6)
               GO TO 7600-LABEL-MOVE.

           IF  W-LABEL-LINES (2) = SPACES AND
               W-LABEL-LINES (3) = SPACES AND
               W-LABEL-LINES (4) = SPACES AND
               W-LABEL-LINES (5) = SPACES AND
               W-LABEL-LINES (6) = SPACES
               SET W-NDX               TO 1
               GO TO 7600-MOVE-ZIP.

           IF  W-LABEL-LINES (2) = SPACES
               MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
               MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
               MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
               MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
               MOVE SPACES             TO W-LABEL-LINES (6)
               GO TO 7600-LABEL-MOVE.

           IF  W-LABEL-LINES (3) = SPACES AND
               W-LABEL-LINES (4) = SPACES AND
               W-LABEL-LINES (5) = SPACES AND
               W-LABEL-LINES (6) = SPACES
               SET W-NDX               TO 2
               GO TO 7600-MOVE-ZIP.

           IF  W-LABEL-LINES (3) = SPACES
               MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
               MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
               MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
               MOVE SPACES             TO W-LABEL-LINES (6)
               GO TO 7600-LABEL-MOVE.

           IF  W-LABEL-LINES (4) = SPACES AND
               W-LABEL-LINES (5) = SPACES AND
               W-LABEL-LINES (6) = SPACES
               SET W-NDX               TO 3
               GO TO 7600-MOVE-ZIP.

           IF  W-LABEL-LINES (4) = SPACES
               MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
               MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
               MOVE SPACES             TO W-LABEL-LINES (6)
               GO TO 7600-LABEL-MOVE.

           IF  W-LABEL-LINES (5) = SPACES AND
               W-LABEL-LINES (6) = SPACES
               SET W-NDX               TO 4
               GO TO 7600-MOVE-ZIP.

           IF  W-LABEL-LINES (5) = SPACES
               MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
               MOVE SPACES             TO W-LABEL-LINES (6)
               SET W-NDX               TO 5
               GO TO 7600-MOVE-ZIP

           ELSE
               IF  W-LABEL-LINES (6) = SPACES
                   SET W-NDX           TO 5
                   GO TO 7600-MOVE-ZIP

               ELSE
                   SET W-NDX           TO 6.

       7600-MOVE-ZIP.

           SET W-NDX2                  TO W-NDX
           SET W-NDX2 DOWN BY +1

           IF (W-LAST-ZIP (W-NDX2) = SPACES) AND
              (W-LAST-DIGIT (W-NDX2) EQUAL SPACES)
      *****CANADIAN ZIP CODES (NON NUMERIC) STAY ON THE LAST LINE
              IF W-LABEL-1ST-ZIP (W-NDX) NUMERIC
                 MOVE W-LABEL-ZIP (W-NDX)
                                       TO W-LAST-ZIP (W-NDX2)
                 MOVE SPACES           TO W-LABEL-LINES (W-NDX)
              END-IF
           END-IF

           .
       7600-EXIT.
           EXIT.
                                       EJECT
       7700-TRANSLATE-LOWER.

           IF  W-LABEL-LINES (1) NOT = SPACES
               MOVE W-LABEL-LINES (1)  TO W-TEMP-AREA2
               PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
               MOVE W-TEMP-AREA2       TO W-LABEL-LINES (1).

           IF  W-LABEL-LINES (2) NOT = SPACES
               MOVE W-LABEL-LINES (2)  TO W-TEMP-AREA2
               PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
               MOVE W-TEMP-AREA2       TO W-LABEL-LINES (2).

           IF  W-LABEL-LINES (3) NOT = SPACES
               MOVE W-LABEL-LINES (3)  TO W-TEMP-AREA2
               PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
               MOVE W-TEMP-AREA2       TO W-LABEL-LINES (3).

      *****THE CITY STATE WILL BE ON LINE FOUR OR FIVE DEPENDING
      *****ON THE FORMAT USED.  THE SIXTH LINE BEING BLANK WILL TELL.

           MOVE 'N'                    TO W-STATE-LINE.

           IF  W-LABEL-LINES (4) NOT = SPACES
              IF  W-LABEL-LINES (6) = SPACES
                 MOVE 'Y'              TO W-STATE-LINE
                 MOVE W-LABEL-LINES (4)
                                       TO W-TEMP-AREA2
                 PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
                 MOVE W-TEMP-AREA2     TO W-LABEL-LINES (4).

           IF  W-LABEL-LINES (5) NOT = SPACES
              IF  W-LABEL-LINES (6) NOT = SPACES
                 MOVE 'Y'              TO W-STATE-LINE
                 MOVE W-LABEL-LINES (5)
                                       TO W-TEMP-AREA2
                 PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
                 MOVE W-TEMP-AREA2     TO W-LABEL-LINES (5).

       7700-EXIT.
           EXIT.
                                       EJECT

       7750-SEARCH-AND-TRANSLATE.

           SET W-TA1                   TO +1.

       7750-FIND-FIRST-NON-BLANK.
pemprv     if w-ta1 < +31
              IF W-TEMP-2 (W-TA1) = SPACES
                 SET W-TA1 UP BY 1
                 GO TO 7750-FIND-FIRST-NON-BLANK
              end-if
           end-if

      *****SET INDEX TO THE NEXT CHAR TO START THE TRANSLATE.
           SET W-TA2                   TO W-TA1
           SET W-TA2 UP BY 1
           SET W-TA21                  TO W-TA2
           SET W-TA21 UP BY 1.

           MOVE 'N'                    TO W-DATA-FOUND-SW.
           PERFORM 7282-FIND-NEXT-BLANK THRU 7282-EXIT.

       7750-EXIT.
           EXIT.
           EJECT

       7282-FIND-NEXT-BLANK.

           IF  W-TA21 GREATER THAN 31
               GO TO 7282-EXIT.

           IF  W-TA21 EQUAL 31

               IF  NO-CHARACTERS-FOUND
                   GO TO 7282-EXIT

               ELSE
                   SET W-TA1           TO +1
                   MOVE SPACES         TO W-TEMP-AREA1

                   PERFORM 7283-MOVE
                           VARYING
                       W-MOVE-NDX FROM W-TA2 BY 1
                           UNTIL
                       W-MOVE-NDX EQUAL W-TA21

                   PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT
                   SET W-TA1           TO +1

                   PERFORM 7284-MOVE-BACK
                           VARYING
                       W-MOVE-NDX FROM W-TA2 BY 1
                           UNTIL
                       W-MOVE-NDX EQUAL W-TA21

                   GO TO 7282-EXIT.

           IF  W-TEMP-2 (W-TA2) = SPACE OR ',' OR '/'
               GO TO 7282-NEXT-GROUP-SEARCH.

           IF  W-TEMP-2 (W-TA21) = SPACES OR ',' OR '/'
               NEXT SENTENCE

           ELSE
               MOVE 'Y'                TO W-DATA-FOUND-SW
               SET W-TA21 UP BY +1
               GO TO 7282-FIND-NEXT-BLANK.

           SET W-TA1                   TO +1.
           MOVE SPACES                 TO W-TEMP-AREA1.

           PERFORM 7283-MOVE
                   VARYING
               W-MOVE-NDX FROM W-TA2 BY 1
                   UNTIL
               W-MOVE-NDX EQUAL W-TA21.

           PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT.
           SET W-TA1                   TO +1.

           PERFORM 7284-MOVE-BACK
                   VARYING
               W-MOVE-NDX FROM W-TA2 BY 1
                   UNTIL
               W-MOVE-NDX EQUAL W-TA21.

           SET W-TA2                  TO W-TA21.
           SET W-TA21 UP BY 1.
           MOVE 'N'                   TO W-DATA-FOUND-SW.

       7282-NEXT-GROUP-SEARCH.
           if w-ta2 < +31
           IF  W-TEMP-2 (W-TA2) EQUAL SPACES OR ',' OR '/'
               SET W-TA2 UP BY 1

               IF  W-TA2 = 30
                   GO TO 7282-EXIT

               ELSE
                   GO TO 7282-NEXT-GROUP-SEARCH.

           SET W-TA2  UP BY 1.
           SET W-TA21                  TO W-TA2.
           SET W-TA21 UP BY 1.
           GO TO 7282-FIND-NEXT-BLANK.

       7282-EXIT.
           EXIT.

       7283-MOVE.

           MOVE W-TEMP-2 (W-MOVE-NDX)  TO W-TEMP-1 (W-TA1).
           SET W-TA1 UP BY +1.

       7284-MOVE-BACK.

           MOVE W-TEMP-1 (W-TA1)       TO W-TEMP-2 (W-MOVE-NDX).
           SET W-TA1 UP BY +1.

       7285-TEST-AND-TRANSLATE.
      ***BYPASS IF  THE AREA MAY BE A PO BOX, OR RR NUMBER

           IF  W-TEMP-AREA1 = '.O.' OR 'RR'
               GO TO 7285-EXIT.

      ***BYPASS IF IT IS A CITY/STATE LINE AND BEYOND CHARACTER 07
      ***AND IT APPEARS THAT IT MAY BE A ABREVIATION.

           SET W-POSITION2             TO W-TA2.
           SET W-POSITION21            TO W-TA21.
           COMPUTE W-WORD-LENGTH = W-POSITION21 - W-POSITION2.

           IF  W-WORD-LENGTH LESS THAN 3
                   AND
               W-STATE-LINE = 'Y'
                   AND
               W-TA2 GREATER THAN 07
               GO TO 7285-EXIT.

           INSPECT W-TEMP-AREA1 CONVERTING W-UPPER-CASE TO W-LOWER-CASE.

       7285-EXIT.
           EXIT.

       7800-VARIABLE-SEARCH.
      ***************************************************************
      *    THIS ROUTINE SEARCHES THE NEWLY CREATED LETTER FOR ANY   *
      *    VARIABLE SYMBOL AND WILL REPLACE THE VARIABLE SYMBOL     *
      *    WITH THE CORRESPONDING DATA FROM THE SYSTEM DEFINED      *
      *    DATA THAT WAS GENERATED IN PARAGRAPHS 7000-7299.         *
      *                                                             *
      *    THE ADDRESSING OF THE VARIABLE TABLE IS ACCOMPLISHED     *
      *    BY DOING A GETMAIN, MOVING THE TABLE TO THE NEW STORAGE  *
      *    AND BY ADJUSTING THE BLL POINTER TO POINT AT THE DATA.   *
      ***************************************************************

           MOVE W-REC-TEXT (W-TB-NDX)  TO W-SINGLE-LINE.
           SET NDX1                    TO 1.

       7801-LOOP.

           IF  NDX1 GREATER 70
               MOVE W-SINGLE-LINE      TO W-REC-TEXT (W-TB-NDX)
               GO TO 7899-EXIT.

           IF  W-ONE-CHAR (NDX1) NOT = '@'
               SET NDX1 UP BY 1
               GO TO 7801-LOOP.

           SET NDX2                    TO NDX1.
           SET NDX2 UP BY 1.

           IF  W-ONE-CHAR (NDX2) = '@'
               SET NDX1 UP BY 2
               GO TO 7801-LOOP.

           MOVE W-ONE-CHAR (NDX2)      TO W-V1.
           SET NDX2 UP BY 1.
           MOVE W-ONE-CHAR (NDX2)      TO W-V2.
           SET NDX2 UP BY 1.
           MOVE W-ONE-CHAR (NDX2)      TO W-V3.
           SET NDX2 UP BY 1.
           MOVE W-ONE-CHAR (NDX2)      TO W-V4.

           IF  W-V-NUM NOT NUMERIC
               GO TO 7830-VAR-ERROR.

           IF  W-V-PERIOD NOT = '.'
               MOVE '.'                TO W-V-PERIOD
               MOVE ZERO               TO W-V-DECIMAL
               GO TO 7840-TABLE-SEARCH.

           IF  W-V-DECIMAL NUMERIC
               GO TO 7840-TABLE-SEARCH.

       7830-VAR-ERROR.

           SET NDX1 UP BY 1.
           GO TO 7801-LOOP.

       7840-TABLE-SEARCH.

           IF W-NO-GETMAIN-DONE-YET
              MOVE 'Y'                 TO W-GETMAIN-SW
              SET LCP-INITIAL-PNTR TO ADDRESS OF SYSTEM-VARIABLES
              MOVE SYSTEM-SUPPORTED-VARIABLES
                                       TO SYSTEM-VARIABLES
              MOVE 1                   TO SS-COUNTER
              SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES
           ELSE
              SET ADDRESS OF SYSTEM-VARIABLES TO LCP-INITIAL-PNTR
              MOVE LCP-INITIAL-COMP    TO LCP-WS-ADDR-COMP
              MOVE 1                   TO SS-COUNTER
              MOVE SYSTEM-SUPPORTED-VARIABLES
                                       TO SYSTEM-VARIABLES
           END-IF

           .
       7850-TABLE-LOOP.

           IF  SS-COUNTER GREATER THAN  SS-NUM-ENTRIES
               GO TO 7830-VAR-ERROR.

           IF  SYS-VAR-CODE NOT = W-VAR-HOLD
               SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES
               ADD SYS-VAR-LEN         TO LCP-WS-ADDR-COMP
               SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
               ADD 1                   TO SS-COUNTER
               GO TO 7850-TABLE-LOOP.

           MOVE SYS-VAR-ENTRY          TO W-VARIABLE-WORK-AREA.
           SET W-NDXV                  TO 1.
           SUBTRACT 6                  FROM W-VAR-LEN.
           PERFORM 7900-MOVE-VAR-DATA THRU 7900-EXIT
               W-VAR-LEN TIMES.
           GO TO 7801-LOOP.

       7899-EXIT.
           EXIT.

       7900-MOVE-VAR-DATA.

           IF  NDX1 GREATER 70
               GO TO 7801-LOOP.

           MOVE W-VAR-W-ONE-CHAR (W-NDXV) TO W-ONE-CHAR (NDX1).
           SET W-NDXV UP BY 1.
           SET NDX1 UP BY 1.

       7900-EXIT.
            EXIT.
                                       EJECT

       0110-READ-ELMSTR.

           READ ELMSTR NEXT RECORD

           IF (ELMSTR-FILE-STATUS = '10' OR '23')
              OR (CL-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 MOVE ' ELMSTR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELMSTR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ELMSTR.

           MOVE LOW-VALUES             TO CL-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO CL-COMPANY-CD

           START ELMSTR KEY NOT < CL-CONTROL-PRIMARY

           IF ELMSTR-FILE-STATUS = '10' OR '23'
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 MOVE ' ELMSTR START ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELMSTR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0120-EXIT.
           EXIT.

       0130-DELETE-ELACTQ.

      *    DELETE ELACTQ

           IF ELACTQ-FILE-STATUS = '10' OR '23'
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           ELSE
              IF ELACTQ-FILE-STATUS NOT = '00'
                 MOVE ' ELACTQ  DELETE ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELACTQ-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0130-EXIT.
           EXIT.
       0140-REWRITE-ELACTQ.

PEMTST*    REWRITE ACTIVITY-QUE

           IF ELACTQ-FILE-STATUS = '10' OR '23'
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           ELSE
              IF ELACTQ-FILE-STATUS NOT = '00'
                 MOVE ' ELACTQ rewrite ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELACTQ-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0140-EXIT.
           EXIT.

DAN   *0150-READ-UPLOAD.
      *
      *    READ UPLOAD-FILE
      *      AT END
      *         SET THERE-ARE-NO-MORE-RECORDS TO TRUE
      *         GO TO 0150-EXIT.
      *
      *    UNSTRING UPLOAD-RECORD DELIMITED BY X'05'
      *      INTO UL-STATE    UL-BUS-TYPE UL-ACCT      UL-ACCT-SFX
      *           UL-EXP-DT   UL-CONV-DT  UL-EFF-DT    UL-EXP-EFF
      *           UL-RPTCD-1  UL-RPTCD-2  UL-ACCT-NAME UL-CONT-NAME
      *           UL-ADDRESS  UL-CITY-ST  UL-ZIP       UL-IG
      *           UL-STAT     UL-MAIL-CAT UL-ECS020-DIFF
      *    END-UNSTRING
      *
      *    .
      *0150-EXIT.
      *    EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
