       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EL337.
       AUTHOR.      CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.

      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *   HEALTH AND LIFE                                 *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CENTRAL STATES. *
      *            *                                                   *
      *            *****************************************************

      *REMARKS.
      *         THIS PROGRAM UPDATES THE ONLINE PURGED CERT FILE
      *       FROM THE OFF LINE PURGED CERT FILE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.
           SELECT DISK-DATE        ASSIGN TO SYS019.
           SELECT FICH             ASSIGN TO SYS020.
           SELECT CERTPURG         ASSIGN TO SYS010.
           SELECT ELPURG           ASSIGN TO ELPURG
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS PG-CONTROL-PRIMARY
                                   FILE STATUS IS PG-FILE-STATUS.

           EJECT

       DATA DIVISION.

       FILE SECTION.

       FD  PRNTR                       COPY ELCPRTFD.


       FD  FICH                        COPY ELCFCHFD.


       FD  CERTPURG                    COPY ECSCRIFD.

                                       COPY ECSCRT01.


           EJECT
       FD  ELPURG.
                                       COPY ELCPURG.

           EJECT
       FD  DISK-DATE                   COPY ELCDTEFD.

           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '*    EL337  WORKING-STORAGE    *'.
       77  FILLER  PIC X(32) VALUE '********** VMOD=2.001 **********'.

       77  WS-PRT-FULL-LINE-SW         PIC X              VALUE SPACE.
           88  LINE-FULL                   VALUE 'X'.

       77  WS-ECS-END-SW               PIC X              VALUE SPACE.
           88  NO-MORE-ECS-RECORDS         VALUE 'X'.
           88  MORE-ECS-RECORDS            VALUE ' '.

       77  ERROR-SW                    PIC X              VALUE SPACE.
           88  ERROR-OCCURRED              VALUE 'E'.
           88  NO-ERRORS                   VALUE ' '.

       77  WS-O-MATCH-SW               PIC X              VALUE 'M'.
           88  MATCHED-REC                 VALUE 'M'.
           88  NO-MATCH-FOUND              VALUE ' '.

       77  WS-IO-ERROR-SW              PIC X              VALUE SPACE.
           88  IO-ERROR                    VALUE 'E'.
           88  IO-ERROR-OCCURRED           VALUE 'E'.

       77  WS-LINE-CNT                 PIC S999  COMP-3   VALUE ZEROS.
       77  WS-PAGE-CNT                 PIC S999  COMP-3   VALUE ZEROS.
       77  WS-MAX-LINES                PIC S999  COMP-3   VALUE +60.
       77  X                           PIC X              VALUE SPACE.
       77  WS-ZERO                     PIC S9    COMP-3   VALUE ZERO.
       77  WS-RETURN-CODE              PIC S9(4) COMP     VALUE ZERO.
       77  WS-LEVEL-INDEX              PIC S9(4) COMP     VALUE ZERO.

       77  WS-ABEND-MESSAGE            PIC X(80)          VALUE SPACES.
       77  WS-ABEND-FILE-STATUS        PIC XX             VALUE ZERO.

       01  WK-MEMB-ACCT.
           12  WK-MEMB-ACCT-CHAR       PIC X   OCCURS 6.
       01  WK-LAST-NAME.
           12  WK-LAST-NAME-CHAR       PIC X   OCCURS 12.

       01  WORK-AREA.
           12  LST-NM-SUB              PIC S9(4) COMP  VALUE +0.
           12  ACCT-SUB                PIC S9(4) COMP  VALUE +0.
           12  BIN-CR-DT               PIC XX    VALUE SPACES.
           12  ERROR-FLAG              PIC X           VALUE ' '.
              88 NO-ERRORS                    VALUE ' '.
              88 NON-ALPHA                    VALUE 'A'.
              88 BLANK-NAME                   VALUE 'B'.

       01  DTE-INTERFACE-AREAS.
           12  PGM-SUB                 PIC S9(4)    COMP VALUE +331.
           12  ABEND-CODE              PIC XXXX          VALUE SPACE.
           12  ABEND-OPTION            PIC X.
           12  OLC-REPORT-NAME         PIC X(5)          VALUE 'EL331'.

       01  WS-SAVE-AREAS.
           12  FILLER   PIC X(27)  VALUE '---------------------------'.
           12  FILLER   PIC X(27)  VALUE '---- EL337 SAVED AREAS ----'.
           12  FILLER   PIC X(27)  VALUE '---------------------------'.

           12  WS-ECS-READ       COMP-3     PIC S9(9)   VALUE ZEROS.
PEMUNI     12  WS-PRG-READ       COMP-3     PIC S9(9)   VALUE ZEROS.
PEMUNI     12  WS-PRG-write      COMP-3     PIC S9(9)   VALUE ZEROS.
           12  WS-MATCHED-ONL    COMP-3     PIC S9(9)   VALUE ZEROS.
           12  WS-ONL-READ       COMP-3     PIC S9(9)   VALUE ZEROS.
           12  WS-LOADED-ECS     COMP-3     PIC S9(9)   VALUE ZEROS.
           12  WS-ONL-DELETED    COMP-3     PIC S9(9)   VALUE ZEROS.
           12  WS-ONL-EXIST      COMP-3     PIC S9(9)   VALUE ZEROS.
           12  WS-INACTIVE-CERTS COMP-3     PIC S9(9)   VALUE ZEROS.
           12  WS-EXPIRE-MOS     COMP-3     PIC S9(9)   VALUE ZEROS.
           12  WS-WORK-TERM      COMP-3     PIC S9(3)   VALUE ZEROS.
pemuni     12  ws-work                  pic s9(9) comp-3 value +0.
pemuni     12  ws-remainder             pic s9(9) comp-3 value +0.
           12  WS-CURRENT-MOS           PIC S9(9) COMP-3  VALUE ZEROS.
           12  WS-UPDATE-SW             PIC S9  COMP-3    VALUE ZERO.
           12  WS-EOM-DT                PIC X(02)      VALUE LOW-VALUES.

           12  WS-NAME-WORK.
               16  WS-NAME-14           PIC X(14)          VALUE SPACE.
               16  WS-NAME-15           PIC X              VALUE '|'.

           12  WS-EFF-ACT-NAME.
               16  WS-NAME-EFF-DT      PIC X(6)  VALUE SPACES.
               16  WS-NAME-FILL        PIC XX    VALUE SPACES.
               16  WS-NAME-ACCT        PIC X(7)  VALUE SPACES.

           12  PG-FILE-STATUS          PIC XX  VALUE SPACES.

           12  WS-CURRENT-DATE-MDY.
               16  WS-CURR-MO          PIC XX   VALUE SPACES.
               16  WS-CURR-DA          PIC XX   VALUE SPACES.
               16  WS-CURR-YR          PIC XX   VALUE SPACES.

                                       COPY ELCDATE.

           EJECT
       01  PRINTER-WORK-AREAS.
           12  HEADING-1.
               16  FILLER      PIC X(50) VALUE SPACE.
               16  FILLER      PIC X(21) VALUE '  PURGED CERT AUDIT  '.
               16  FILLER      PIC X(60) VALUE SPACES.

           12  HEADING-2.
               16  FILLER      PIC X(47) VALUE SPACE.
               16  HD-COMP-NM  PIC X(30) VALUE SPACE.
               16  FILLER      PIC X(42) VALUE SPACES.
               16  FILLER      PIC X(08) VALUE 'EL337  '.

           12  HEADING-3.
               16  FILLER      PIC X(50) VALUE SPACE.
               16  FILLER      PIC X(3)  VALUE '** '.
               16  HD-DATE-FULL PIC X(18).
               16  FILLER      PIC X(3)  VALUE ' **'.
               16  FILLER      PIC X(45)  VALUE SPACES.
               16  HD-RUN-DATE PIC X(8).

           12  HEADING-4.
               16  FILLER      PIC X(119) VALUE SPACE.
               16  FILLER      PIC X(4)   VALUE 'PAGE'.
               16  HD-PAGE-NO  PIC ZZZ9.

           12  HEADING-5.
               16  FILLER      PIC X(44)  VALUE
               ' *---------- CERTIFICATE CONTROLS ---------*'.
               16  FILLER      PIC X(44)  VALUE
               '                                            '.
               16  FILLER      PIC X(45)  VALUE
               '---* *------- EXCEPTION CONDITION ----------*'.

           12  HEADING-6.
               16  FILLER      PIC X(46)  VALUE
                   '   NUMBER   EFFECTIVE C GROUP  ST  ACCOUNT    '.
               16  FILLER      PIC X(46)  VALUE
                   '                                              '.
               16  FILLER      PIC X(41)  VALUE  SPACES.

           12  DTL-1.
               16  FILLER              PIC X              VALUE SPACE.
               16  DTL-CERT-NO         PIC X(11).
               16  FILLER              PIC X              VALUE SPACE.
               16  DTL-EFFECTIVE-DT.
                   20  DTL-EFF-MO      PIC XX.
                   20  FILLER          PIC X              VALUE '/'.
                   20  DTL-EFF-DA      PIC XX.
                   20  FILLER          PIC X              VALUE '/'.
                   20  DTL-EFF-YR      PIC XX.
               16  FILLER              PIC X              VALUE SPACE.
               16  DTL-CARR            PIC X.
               16  FILLER              PIC X             VALUE SPACE.
               16  DTL-GRPG            PIC X(6).
               16  FILLER              PIC X             VALUE SPACE.
               16  DTL-STAT            PIC XX.
               16  FILLER              PIC X             VALUE SPACE.
               16  DTL-ACCT            PIC X(10).
               16  FILLER              PIC X             VALUE SPACE.
               16  DTL-CL-NO           PIC X(7).
               16  FILLER              PIC X             VALUE SPACE.
               16  DTL-CL-TYPE         PIC XXXX.
               16  FILLER              PIC X             VALUE SPACE.
               16  DTL-CL-STATUS       PIC X(6).
               16  FILLER              PIC X              VALUE SPACE.
               16  DTL-CL-LN-KIND      PIC X(7).
               16  FILLER              PIC X              VALUE SPACE.
               16  DTL-CL-INCURRED     PIC X(8).
               16  FILLER              PIC XXX            VALUE SPACE.
               16  DTL-CL-PROCESSOR    PIC XXXX.
               16  FILLER              PIC X(4)           VALUE SPACE.
               16  DTL-EXC-MSG         PIC X(40).
               16  FILLER              PIC XX             VALUE SPACE.

           12  WS-EXC-MSG-CANCEL.
               16  WS-EXC-MSG-DESC    PIC X(26)
                     VALUE ' XXXX COV CANCELLED AS OF '.
               16  WS-EXC-MSG-CANCEL-DATE.
                   25  WS-EXC-C-MO    PIC XX.
                   25  FILLER         PIC X                VALUE '/'.
                   25  WS-EXC-C-DA    PIC XX.
                   25  FILLER         PIC X                VALUE '/'.
                   25  WS-EXC-C-YR    PIC XX.

           12  WS-EXC-MSG-NO-NOTES.
               16  WS-EXC-MSG-NO-DE    PIC X(21)
                     VALUE ' CERTIFICATE PENDING-'.
               16  WS-EXC-MSG-NO-COPY  PIC X(15)
                     VALUE 'NOTES ATTACHED.'.

           12  WS-EXC-MSG-CO-DELETE   PIC X(40)
                     VALUE ' COVERAGE DELETED - SEE CREDIT DEPT.    '.

           12  WS-EXC-MSG-CE-DELETE.
               16  WS-EXC-MSG-CE-DE    PIC X(21)
                     VALUE ' CERTIFICATE DELETED '.
               16  WS-EXC-MSG-CE-COPY  PIC X(15)
                     VALUE SPACES.

           12  WS-EXC-MSG-EE-EXPIRED.
               16  WS-EXC-MSG-EE-DE    PIC X(21)
                     VALUE ' CERTIFICATE EXPIRED '.
               16  WS-EXC-MSG-EE-COPY  PIC X(15)
                     VALUE SPACES.

           12  WS-EXC-MSG-ADDED.
               16  FILLER             PIC X(22) VALUE
                           ' COVERAGE ADDED - AMT '.
               16  WS-EXC-MSG-ADDED-AMT     PIC ZZZZZZ9.99.
               16  FILLER             PIC X(6) VALUE ' TYPE-'.
               16  WS-EXC-MSG-ADDED-TYPE    PIC XX.

           12  WS-EXC-MSG-CHANGED-NEW.
               16  FILLER            PIC X(22) VALUE
                           ' COVERAGE CHANGED-NEW '.
               16  WS-EXC-MSG-CHGD-AMT-NEW  PIC ZZZZZZ9.99.
               16  FILLER            PIC X(6) VALUE ' TYPE-'.
               16  WS-EXC-MSG-CHGD-TYPE-NEW PIC XX.

           12  WS-EXC-MSG-CHANGED-OLD.
               16  FILLER              PIC X(18) VALUE   SPACES.
               16  FILLER              PIC X(04) VALUE
                                             'OLD '.
               16  WS-EXC-MSG-CHGD-AMT-OLD  PIC ZZZZZZ9.99.
               16  FILLER                   PIC X(6) VALUE ' TYPE-'.
               16  WS-EXC-MSG-CHGD-TYPE-OLD PIC XX.

           12  WS-EXC-MSG-COPY.
               16  FILLER              PIC X(20) VALUE
                                             'CERT COPIED FROM ECS'.

           12  FTG-1.
               16  P-MATCHED-ONL       PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' MATCHED / UPDATED (+)   '.

           12  FTG-2.
               16  P-LOADED-ECS        PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' LOADED FROM OFFLINE (+) '.

           12  FTG-3.
               16  P-ONL-DELETED       PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' ONLINE DELETED    (-)   '.
               16  FILLER              PIC X(30)
                         VALUE  ' CURRENT ELCERT RECORD COUNT--'.
               16  P-ONL-EXIST         PIC ZZZ,ZZZ,ZZ9-.

           12  FTG-4.
               16  P-CLAIM-MATCH       PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' HAVE ATTACHED CLAIMS    '.

           12  FTG-5.
               16  P-INACTIVE-CERTS    PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' INACTIVE AND BYPASSED   '.

           12  FTG-6.
               16  P-ONL-READ          PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' ONLINE CERTS READ       '.

           12  FTG-7.
               16  P-ECS-READ          PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' OFFLINE CERTS READ      '.

           12  FTG-8.
               16  P-EXCP-ADD          PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' COMP EXCEPTIONS ADDED'.

           12  FTG-9.
               16  P-EXCP-DEL          PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' COMP EXCEPTIONS DELETED'.

           12  FTG-10.
               16  P-EXCP-TBL          PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' ACCTS WITH COMM TABLES '.

           12  FTG-11.
               16  P-MAIL-DEL          PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' INSURED ADDRESS MAIL RECS DELETED '.

           12  FTG-12.
               16  P-ACCT-READS        PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' ACCOUNT MASTER MATCH ATTEMPTS '.

           12  FTG-13.
               16  P-ACT-READ          PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' ERACCT RECORDS READ '.

           12  FTG-14.
               16  P-ACCT-MATCH        PIC ZZZ,ZZZ,ZZ9-.
               16  FILLER              PIC X(35)
                         VALUE  ' CERTS MATCHED TO ACCOUNT MASTER '.


                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCCRTVR.

           EJECT
       PROCEDURE DIVISION.

       00000-LOAD-DATE-CARD.           COPY ELCDTERX.

           MOVE BIN-RUN-DATE           TO WS-EOM-DT

           .
       0000-MAINLINE.

           PERFORM 7000-INITIALIZE     THRU 7000-EXIT

           PERFORM 2000-PROCESS-CERT   THRU 2000-EXIT UNTIL
              (NO-MORE-ECS-RECORDS) OR
              (IO-ERROR-OCCURRED)

           PERFORM 8000-FINALIZE THRU  8000-EXIT

           GOBACK

           .
       2000-PROCESS-CERT.

           if ws-ecs-read > zeros
              divide ws-ecs-read by +10000 giving
                 ws-work remainder     ws-remainder
              if ws-remainder = +0
                 display ' ecs records read    ' ws-ecs-read
              end-if
           end-if
           if ws-prg-read > zeros
              divide ws-prg-read by +10000 giving
                 ws-work remainder     ws-remainder
              if ws-remainder = +0
                 display ' prg records read    ' ws-ecs-read
              end-if
           end-if
           if ws-prg-write > zeros
              divide ws-prg-write by +10000 giving
                 ws-work remainder     ws-remainder
              if ws-remainder = +0
                 display ' prg records written ' ws-ecs-read
              end-if
           end-if
           PERFORM 2200-READ-ELPURG    THRU 2200-EXIT

           IF PG-FILE-STATUS = '10' OR '23'
              PERFORM 2050-BUILD-ELPURG
                                       THRU 2050-EXIT
           ELSE
              ADD 1                    TO WS-MATCHED-ONL
           END-IF

           PERFORM 2100-READ-ECS-RECORDS
                                       THRU 2100-EXIT

           .
       2000-EXIT.
           EXIT.

       2050-BUILD-ELPURG.

           MOVE SPACES                 TO PURGE-CERT-MASTER
           MOVE 'PG'                   TO PG-RECORD-ID
           MOVE DTE-CLASIC-COMPANY-CD  TO PG-COMPANY-CD
                                          PG-COMPANY-CD-A1
                                          PG-COMPANY-CD-A2
                                          PG-COMPANY-CD-A4
                                          PG-COMPANY-CD-A5
           MOVE CR-CARRIER             TO PG-CARRIER
           MOVE CR-GROUPING            TO PG-GROUPING
           MOVE CR-STATE               TO PG-STATE
           MOVE CR-ACCOUNT             TO PG-ACCOUNT

           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 7100-DATE-RTN       THRU 7100-EXIT
           MOVE DC-BIN-DATE-1          TO PG-CERT-EFF-DT

           MOVE CR-CERT-NO             TO PG-CERT-NO
                                          PG-CERT-NO-A4
           IF CR-LNAME EQUAL SPACES
              MOVE CR-CERT-NO          TO CR-LNAME
           END-IF

           MOVE CR-LNAME               TO PG-INSURED-LAST-NAME
           MOVE CR-1ST-INITIAL         TO PG-INSURED-INITIALS (1:1)
           MOVE CR-INIT                TO PG-INSURED-INITIALS (2:1)
           IF CR-SOC-SEC NOT = SPACES
              MOVE CR-SOC-SEC          TO PG-SOC-SEC-NO
           END-IF

           INSPECT CR-SOC-SEC
               REPLACING ALL ' ' BY '0'

           IF CR-SOC-SEC = ZEROS
               MOVE CR-STATE           TO PG-SSN-STATE
               MOVE CR-ACCT-PRIME      TO PG-SSN-ACCOUNT
               MOVE CR-LNAME           TO PG-PART-LAST-NAME-A2
               MOVE PG-INSURED-INITIALS
                                       TO PG-INSURED-INITIALS-A2
           END-IF

           IF CR-MEMBER-NO NOT = SPACES
              MOVE CR-MEMBER-NO        TO PG-MEMBER-NO
           END-IF

           INSPECT CR-MEMBER-NO
               REPLACING ALL ' ' BY '0'

           IF CR-MEMBER-NO = ZEROS
              MOVE CR-STATE            TO PG-MEMB-STATE
              MOVE CR-ACCT-PRIME       TO PG-MEMB-ACCOUNT
              MOVE CR-LNAME            TO PG-PART-LAST-NAME-A5
              MOVE PG-INSURED-INITIALS TO PG-INSURED-INITIALS-A5
           END-IF

           MOVE CR-FNAME               TO PG-INSURED-FIRST-NAME
           MOVE CR-AGE                 TO PG-INSURED-ISSUE-AGE
           MOVE CR-SEX                 TO PG-INSURED-SEX
           MOVE CR-JOINT-AGE           TO PG-INSURED-JOINT-AGE
           MOVE CR-JOINT-NAME          TO PG-JOINT-INSURED-NAME
           MOVE CR-LFTYP               TO PG-LF-BENEFIT-CD
           MOVE CR-LF-TERM             TO PG-LF-ORIG-TERM
           MOVE CR-LFAMT               TO PG-LF-BENEFIT-AMT
           MOVE CR-LFPRM               TO PG-LF-PREMIUM-AMT
           MOVE CR-LFAMT-ALT           TO PG-LF-ALT-BENEFIT-AMT
           MOVE CR-LFPRM-ALT           TO PG-LF-ALT-PREMIUM-AMT
           MOVE CR-AHTYP               TO PG-AH-BENEFIT-CD
           MOVE CR-AH-TERM             TO PG-AH-ORIG-TERM
           MOVE CR-AHAMT               TO PG-AH-BENEFIT-AMT
           MOVE CR-AHPRM               TO PG-AH-PREMIUM-AMT
           MOVE CR-APR                 TO PG-LOAN-APR
           MOVE CR-LOAN-TERM           TO PG-LOAN-TERM
           MOVE CR-PMT-EXTENSION-DAYS  TO PG-PMT-EXTENSION-DAYS
           MOVE CR-IND-GRP             TO PG-IND-GRP-TYPE
           MOVE CR-REIN-TABLE          TO PG-REIN-TABLE

           IF CR-LF-EXPIRE-DATE NOT = ZEROS
              MOVE CR-LF-EXPIRE-DATE   TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 7100-DATE-RTN    THRU 7100-EXIT
              MOVE DC-BIN-DATE-1       TO PG-LF-LOAN-EXPIRE-DT
           END-IF

           IF CR-AH-EXPIRE-DATE NOT = ZEROS
              MOVE CR-AH-EXPIRE-DATE   TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 7100-DATE-RTN    THRU 7100-EXIT
              MOVE DC-BIN-DATE-1       TO PG-AH-LOAN-EXPIRE-DT
           END-IF

           IF CR-LOAN-1ST-PMT-DT NOT = ZEROS
              MOVE CR-LOAN-1ST-PMT-DT  TO DC-GREG-DATE-1-YMD
              MOVE '3'                 TO DC-OPTION-CODE
              PERFORM 7100-DATE-RTN    THRU 7100-EXIT
              MOVE DC-BIN-DATE-1       TO PG-LOAN-1ST-PMT-DT
           END-IF

           MOVE CR-ENTRY-STATUS        TO PG-ENTRY-STATUS
           IF CR-ENTRY-DATE NOT = ZERO
              MOVE CR-ENTRY-DATE       TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 7100-DATE-RTN    THRU 7100-EXIT
              MOVE DC-BIN-DATE-1       TO PG-ENTRY-DT
           END-IF

           MOVE CR-LF-STATUS-AT-CANCEL TO PG-LF-STATUS-AT-CANCEL
           IF CR-LF-CANC-DT NOT = ZEROS
              MOVE CR-LF-CANC-DT       TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 7100-DATE-RTN    THRU 7100-EXIT
              MOVE DC-BIN-DATE-1       TO PG-LF-CANCEL-DT
           END-IF

           IF CR-LF-CANCEL-EXIT-DATE NOT = ZEROS
              MOVE CR-LF-CANCEL-EXIT-DATE
                                       TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 7100-DATE-RTN    THRU 7100-EXIT
              MOVE DC-BIN-DATE-1       TO PG-LF-CANCEL-EXIT-DT
           END-IF

           MOVE CR-LF-STATUS-AT-DEATH  TO PG-LF-STATUS-AT-DEATH
           IF CR-LF-STATUS-AT-DEATH NOT = ZEROS
              MOVE CR-DTH-DT           TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 7100-DATE-RTN    THRU 7100-EXIT
              MOVE DC-BIN-DATE-1       TO PG-LF-DEATH-DT
           END-IF

           IF CR-LF-CLAIM-EXIT-DATE NOT = ZEROS
              MOVE CR-LF-CLAIM-EXIT-DATE
                                       TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 7100-DATE-RTN    THRU 7100-EXIT
              MOVE DC-BIN-DATE-1       TO PG-LF-DEATH-EXIT-DT
           END-IF

           MOVE CR-LF-CURRENT-STATUS   TO PG-LF-CURRENT-STATUS
           MOVE CR-AH-STATUS-AT-CANCEL TO PG-AH-STATUS-AT-CANCEL
           IF CR-AH-CANC-DT NOT = ZEROS
              MOVE CR-AH-CANC-DT       TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 7100-DATE-RTN    THRU 7100-EXIT
              MOVE DC-BIN-DATE-1       TO PG-AH-CANCEL-DT
           END-IF

           IF CR-AH-CANCEL-EXIT-DATE NOT = ZEROS
              MOVE CR-AH-CANCEL-EXIT-DATE
                                       TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 7100-DATE-RTN    THRU 7100-EXIT
              MOVE DC-BIN-DATE-1       TO PG-AH-CANCEL-EXIT-DT
           END-IF

           MOVE CR-AH-STATUS-AT-SETTLEMENT
                                       TO PG-AH-STATUS-AT-SETTLEMENT
           IF (CR-AH-LUMP-SUM-DISAB)  OR
              (CR-AH-STATUS-AT-CANCEL = '6')
              IF CR-DIS-DT NOT EQUAL ZERO
                 MOVE CR-DIS-DT        TO DC-GREG-DATE-CYMD
                 MOVE 'L'              TO DC-OPTION-CODE
                 PERFORM 7100-DATE-RTN THRU 7100-EXIT
                 MOVE DC-BIN-DATE-1    TO PG-AH-SETTLEMENT-DT
              END-IF
           END-IF

           IF CR-AH-SETTLEMENT-EXIT-DATE NOT = ZEROS
              MOVE CR-AH-SETTLEMENT-EXIT-DATE
                                       TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 7100-DATE-RTN    THRU 7100-EXIT
              MOVE DC-BIN-DATE-1       TO PG-AH-SETTLEMENT-EXIT-DT
           END-IF

           MOVE CR-AH-CURRENT-STATUS   TO PG-AH-CURRENT-STATUS
           MOVE CR-ENTRY-BATCH         TO PG-ENTRY-BATCH
           MOVE CR-USER-CODE           TO PG-USER-FIELD
           MOVE CR-USER-FUTURE         TO PG-USER-RESERVED

           WRITE PURGE-CERT-MASTER
           IF PG-FILE-STATUS (1:1) = '0'
              add +1 to ws-prg-write
           ELSE
              MOVE 'ERROR OCCURED WRITE - ELPURG'
                                       TO WS-ABEND-MESSAGE
              MOVE PG-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       2050-EXIT.
           EXIT.

       2100-READ-ECS-RECORDS.

           READ CERTPURG  AT END
               MOVE 'X'                TO WS-ECS-END-SW
           END-READ

           IF NOT NO-MORE-ECS-RECORDS
              ADD  1                   TO WS-ECS-READ
              IF CR-LF-CANCEL-EXIT-DATE  NOT NUMERIC
                 MOVE ZEROS            TO CR-LF-CANCEL-EXIT-DATE
              END-IF

              IF CR-AH-CANCEL-EXIT-DATE  NOT NUMERIC
                 MOVE ZEROS            TO CR-AH-CANCEL-EXIT-DATE
              END-IF

              IF CR-LF-CLAIM-EXIT-DATE  NOT NUMERIC
                 MOVE ZEROS            TO CR-LF-CLAIM-EXIT-DATE
              END-IF

              IF CR-AH-SETTLEMENT-EXIT-DATE  NOT NUMERIC
                 MOVE ZEROS            TO CR-AH-SETTLEMENT-EXIT-DATE
              END-IF
           END-IF

           .
       2100-EXIT.
            EXIT.

           EJECT

       2200-READ-ELPURG.

           MOVE DTE-CLASIC-COMPANY-CD  TO PG-COMPANY-CD
                                          PG-COMPANY-CD-A1
                                          PG-COMPANY-CD-A2
                                          PG-COMPANY-CD-A4
                                          PG-COMPANY-CD-A5
           MOVE CR-CARRIER             TO PG-CARRIER
           MOVE CR-GROUPING            TO PG-GROUPING
           MOVE CR-STATE               TO PG-STATE
           MOVE CR-ACCOUNT             TO PG-ACCOUNT

           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 7100-DATE-RTN       THRU 7100-EXIT
           MOVE DC-BIN-DATE-1          TO PG-CERT-EFF-DT

           MOVE CR-CERT-NO             TO PG-CERT-NO

           READ ELPURG
           if pg-file-status = '00'
              add +1 to ws-prg-read
           end-if
           IF PG-FILE-STATUS = '00' OR '10' OR '23'
              CONTINUE
           ELSE
              MOVE 'ERROR OCCURED READ - ELPURG'
                                       TO WS-ABEND-MESSAGE
              MOVE PG-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       2200-EXIT.
            EXIT.

           EJECT
       2510-BUILD-PRINT.

           IF NOT LINE-FULL
              PERFORM 7300-BLD-ECS-PRT-LINE
                                       THRU 7300-EXIT
           END-IF

           PERFORM 7370-PRINT-DETAIL THRU 7370-EXIT

           .
       2510-EXIT.
            EXIT.

       2530-PRINT-OLD.

           MOVE SPACES                 TO DTL-1.
           MOVE WS-EXC-MSG-CHANGED-OLD TO DTL-EXC-MSG.
           MOVE DTL-1                  TO P-DATA.
           MOVE ' '                    TO X.
           ADD  1                      TO WS-LINE-CNT.
           PERFORM 7600-PRT-RTN   THRU 7600-EXIT.

       2530-EXIT.
            EXIT.

           EJECT


       7000-INITIALIZE.

           OPEN I-O    ELPURG

           IF PG-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'ERROR OCCURED OPEN - ELPURG'
                                       TO WS-ABEND-MESSAGE
              MOVE PG-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           OPEN INPUT CERTPURG

           OPEN OUTPUT PRNTR

           MOVE 1                      TO WS-PAGE-CNT

           MOVE WS-CURRENT-DATE        TO HD-RUN-DATE

           MOVE WS-CD-MM               TO WS-CURR-MO
           MOVE WS-CD-DD               TO WS-CURR-DA
           MOVE WS-CD-YY               TO WS-CURR-YR

           COMPUTE WS-CURRENT-MOS EQUAL (RUN-CCYY * 12) + RUN-MO

           MOVE ALPH-DATE              TO HD-DATE-FULL
           MOVE COMPANY-NAME           TO HD-COMP-NM

           PERFORM 7500-HEADING-RTN    THRU 7500-EXIT
           PERFORM 2100-READ-ECS-RECORDS
                                       THRU 2100-EXIT


           .
       7000-EXIT.
            EXIT.

       7100-DATE-RTN.
           CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.

           IF DC-ERROR-CODE NOT EQUAL SPACE
               MOVE  ZEROS             TO DC-CONVERSION-DATES.

       7100-EXIT.
            EXIT.
           EJECT
       7300-BLD-ECS-PRT-LINE.

           MOVE CR-CERT-NO             TO DTL-CERT-NO.
           MOVE CR-YR                  TO DTL-EFF-YR.
           MOVE CR-MO                  TO DTL-EFF-MO.
           MOVE CR-DA                  TO DTL-EFF-DA.
           MOVE CR-CARRIER             TO DTL-CARR.
           MOVE CR-GROUPING            TO DTL-GRPG.
           MOVE CR-STATE               TO DTL-STAT.
           MOVE CR-ACCOUNT             TO DTL-ACCT.
           MOVE 'X'                    TO WS-PRT-FULL-LINE-SW.

       7300-EXIT.
            EXIT.

       7320-BLD-ONL-PRT-LINE.

           MOVE PG-CERT-NO             TO DTL-CERT-NO

           MOVE ' '                    TO DC-OPTION-CODE
           MOVE PG-CERT-EFF-DT         TO DC-BIN-DATE-1
           PERFORM 7100-DATE-RTN       THRU 7100-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO DTL-EFFECTIVE-DT
           MOVE PG-CARRIER             TO DTL-CARR
           MOVE PG-GROUPING            TO DTL-GRPG
           MOVE PG-STATE               TO DTL-STAT
           MOVE PG-ACCOUNT             TO DTL-ACCT

           .
       7320-EXIT.
            EXIT.


       7370-PRINT-DETAIL.
           MOVE DTL-1                  TO P-DATA.
           MOVE '0'                    TO X.
           PERFORM 7600-PRT-RTN  THRU 7600-EXIT.
           ADD  2                      TO WS-LINE-CNT.

           IF WS-LINE-CNT GREATER WS-MAX-LINES
                PERFORM 7500-HEADING-RTN  THRU 7500-EXIT.

           MOVE SPACES                 TO DTL-1.

       7370-EXIT.
            EXIT.

       7500-HEADING-RTN.
           MOVE  WS-PAGE-CNT           TO HD-PAGE-NO.
           MOVE '1'                    TO X.
           MOVE HEADING-1              TO P-DATA.
           PERFORM 7600-PRT-RTN   THRU 7600-EXIT.

           MOVE SPACES                 TO X.
           MOVE HEADING-2              TO P-DATA.
           PERFORM 7600-PRT-RTN  THRU 7600-EXIT.

           MOVE SPACES                 TO X.
           MOVE HEADING-3              TO P-DATA.
           PERFORM 7600-PRT-RTN  THRU 7600-EXIT.

           MOVE SPACES                 TO X.
           MOVE HEADING-4              TO P-DATA.
           PERFORM 7600-PRT-RTN  THRU 7600-EXIT.

           MOVE SPACES                 TO  X.
           MOVE HEADING-5              TO P-DATA.
           PERFORM 7600-PRT-RTN  THRU 7600-EXIT.

           MOVE SPACES                 TO  X.
           MOVE HEADING-6              TO P-DATA.
           PERFORM 7600-PRT-RTN  THRU 7600-EXIT.

           MOVE 16                     TO WS-LINE-CNT.
           ADD 1 TO WS-PAGE-CNT.

       7500-EXIT.
            EXIT.

       7600-PRT-RTN.                   COPY ELCPRT2.
       7600-PRT-CLEAR.
           MOVE SPACES                 TO P-DATA.
           MOVE SPACES                 TO DTL-1.

       7600-EXIT.
            EXIT.

       8000-FINALIZE.

           MOVE ALL '*'                TO  P-DATA.
           PERFORM  7600-PRT-RTN   THRU 7600-EXIT.

           DISPLAY ' TOTAL PURGE READS ' WS-ECS-READ
           DISPLAY ' TOTAL MATCHES     ' WS-MATCHED-ONL

           CLOSE PRNTR
                 CERTPURG
                 ELPURG

           IF PG-FILE-STATUS  NOT = '00'
              MOVE 'ERROR OCCURED CLOSE - ELPURG'
                                       TO WS-ABEND-MESSAGE
              MOVE PG-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       8000-CLOSE-OTHER.        COPY ELCPRTC.

       8000-EXIT.
            EXIT.

       9999-PROGRAM-END.

           GOBACK.

           EJECT
       ABEND-PGM SECTION. COPY ELCABEND.
