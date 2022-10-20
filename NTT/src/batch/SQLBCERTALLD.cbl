       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SQLBCERTALLD.
       AUTHOR.        Cowtown.
       DATE-COMPILED.

      *REMARKS. This program builds the CERTALL table
      * every month.

103016******************************************************************
103016*                   C H A N G E   L O G
103016*
103016* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103016*-----------------------------------------------------------------
103016*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103016* EFFECTIVE    NUMBER
103016*-----------------------------------------------------------------
103016* 103016  CR2015120300004  PEMA  ADD CLP COLUMNS
070722* 070722  CR2020061200002  PEMA  Add cancel reason
103016******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERTS        ASSIGN TO SYS010.

           SELECT ERMAIL       ASSIGN TO ERMAIL
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED              
                               FILE STATUS IS ERMAIL-FILE-STATUS   
                               RECORD KEY IS MA-CONTROL-PRIMARY.    

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  CERTS
                                       COPY ECSCRIFD.
                                       COPY ECSCRT01.

       FD  ERMAIL.                                                      
                                       COPY ERCMAIL.

       FD  DISK-DATE
                                    COPY ELCDTEFD.

       WORKING-STORAGE SECTION.

NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-CERT                VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  CERT-IN-CNT                 PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.
       77  WS-CANC-DATE                PIC 9(8)  VALUE ZEROS.
       77  S1                          PIC S999  VALUE +0 COMP-3.
       77  a1                          pic s999  value +0 comp-3.
       77  rec-cnt                     pic 9(9) value zeros.
       77  ws-recs-in                  pic 9(7) value zeros.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  ERMAIL-FILE-STATUS          PIC XX    VALUE LOW-VALUES.
       77  ws-factor                   pic s9v9(5) comp-3 value zeros.
       77  ws-lf-refund                pic s9(5)v99 comp-3 value zeros.
       77  ws-ah-refund                pic s9(5)v99 comp-3 value zeros.
       77  ws-records-inserted         pic 9(9) value zeros.
       77  ws-sql-date-time            pic x(24) value spaces.
       77  i1                          pic s9(5) comp-3 value +0.
       77  o1                          pic s9(5) comp-3 value +0.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
      
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  sqlcmd                      pic x(3072).
       01  ws-sqlcmd-in                pic x(3072) value spaces.
       01  ws-sqlcmd-out               pic x(3072) value spaces.
       01  WS-MOE-DATE                 pic x(10).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to tell sql server that i am    ***
      ***  passing it a null value.  The indicator will be -1        ***
      ***  if the value is nulls and +0 if the value is other than   ***
      ***  nulls.  Here is a sample on how to use it.                ***
      ***                                                            ***
      ***      if db-date1 = spaces move -1 to nu-date1 end-if       *** 
      ***     EXEC SQL                                               ***
      ***        insert into TABLE_NAME (                            ***
      ***           date1,                                           ***
      ***           date2)                                           ***
      ***        values (                                            ***
      ***           :db-date1      :nu-date1,                        ***
      ***           :db-date2      :nu-date2)                        ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-LF-EXP-DT            PIC s9(4) comp value +0.
           05  nu-LF-CAN-DT            PIC s9(4) comp value +0.
           05  nu-LF-CAN-EXT-DT        PIC s9(4) comp value +0.
           05  nu-LF-DTH-DT            PIC s9(4) comp value +0.
           05  nu-LF-DTH-EXT-DT        PIC s9(4) comp value +0.
           05  nu-AH-EXP-DT            PIC s9(4) comp value +0.
           05  nu-AH-CAN-DT            PIC s9(4) comp value +0.
           05  nu-AH-CAN-EXT-DT        PIC s9(4) comp value +0.
           05  nu-AH-DIS-DT            PIC s9(4) comp value +0.

       01  EXTRACT-RECORD.
           05  EXT-MOE-DATE            PIC X(10).
           05  EXT-CARRIER             PIC X.
           05  EXT-GROUP               PIC X(6).
           05  ext-clp-state           pic xx.
           05  EXT-STATE               PIC XX.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-EFF-DT              PIC X(10).
           05  EXT-CERT-NO             PIC X(11).
           05  EXT-ENT-DT              PIC X(10).
           05  EXT-1ST-PAY-DT          PIC X(10).
           05  EXT-ENTRY-STATUS        PIC X.
           05  EXT-LAST-NAME           PIC X(15).
           05  EXT-FIRST-NAME          PIC X(10).
           05  EXT-MID-INIT            PIC X.
           05  EXT-JNT-LAST-NAME       PIC X(15).
           05  EXT-JNT-FIRST-NAME      PIC X(10).
           05  EXT-JNT-MID-INIT        PIC X.
           05  EXT-SSN                 PIC X(11).
           05  EXT-MEMBER-NO           PIC X(12).
           05  EXT-SEX                 PIC X.
           05  EXT-INS-AGE             PIC 99.
           05  EXT-JNT-AGE             PIC 99.
           05  EXT-APR                 PIC 999.9999.
           05  ext-apr-a redefines
               ext-apr                 pic x(8).
           05  EXT-LOAN-TERM           PIC 999.
           05  EXT-EXT-DAYS            PIC 999.
           05  EXT-IND-GRP             PIC X.
           05  EXT-FREQ                PIC 99.
           05  EXT-LF-BEN              PIC XX.
           05  EXT-LF-CUR-STATUS       PIC X.
           05  EXT-LF-TERM             PIC 999.
           05  EXT-LF-AMT              PIC 9(7).99.
           05  ext-lf-amt-a redefines
               ext-lf-amt              pic x(10).
           05  EXT-LF-PREM             PIC 9(7).99.
           05  ext-lf-prem-a redefines
               ext-lf-prem             pic x(10).
           05  EXT-LF-AMT-ALT          PIC 9(7).99.
           05  ext-lf-amt-alt-a redefines
               ext-lf-amt-alt          pic x(10).
           05  EXT-LF-PREM-ALT         PIC 9(7).99.
           05  ext-lf-prem-alt-a redefines
               ext-lf-prem-alt         pic x(10).
           05  EXT-LF-NSP-PREM         PIC 9(7).99.
           05  ext-lf-nsp-prem-a redefines
               ext-lf-nsp-prem         pic x(10).
           05  EXT-LF-DTH-AMT          PIC 9(7).99.
           05  ext-lf-dth-amt-a redefines
               ext-lf-dth-amt          pic x(10).
           05  EXT-LF-REM-AMT          PIC 9(7).99.
           05  ext-lf-rem-amt-a redefines
               ext-lf-rem-amt          pic x(10).
           05  EXT-LF-PREM-RATE        PIC 99.9(5).
           05  ext-lf-prem-rate-a redefines
               ext-lf-prem-rate        pic x(8).
           05  EXT-LF-PREM-RATE-ALT    PIC 99.9(5).
           05  ext-lf-prem-rate-alt-a redefines
               ext-lf-prem-rate-alt    pic x(8).
           05  EXT-LF-REFUND           PIC 9(7).99.
           05  ext-lf-refund-a redefines
               ext-lf-refund           pic x(10).
           05  EXT-LF-EXP-DT           PIC X(10).
           05  EXT-LF-CAN-DT           PIC X(10).
           05  EXT-LF-CAN-EXT-DT       PIC X(10).
           05  EXT-LF-DTH-DT           PIC X(10).
           05  EXT-LF-DTH-EXT-DT       PIC X(10).
           05  EXT-AH-BEN              PIC XX.
           05  EXT-AH-CUR-STATUS       PIC X.
           05  EXT-AH-TERM             PIC 999.
           05  EXT-AH-AMT              PIC 9(7).99.
           05  ext-ah-amt-a redefines
               ext-ah-amt              pic x(10).
           05  EXT-AH-PREM             PIC 9(7).99.
           05  ext-ah-prem-a redefines
               ext-ah-prem             pic x(10).
           05  EXT-TOT-FEES            PIC 9(8).99.
           05  ext-tot-fees-a redefines
               ext-tot-fees            pic x(11).
           05  EXT-AH-NSP-PREM         PIC 9(7).99.
           05  ext-ah-nsp-prem-a redefines
               ext-ah-nsp-prem         pic x(10).
           05  EXT-AH-CLM-AMT          PIC 9(7).99.
           05  ext-ah-clm-amt-a redefines
               ext-ah-clm-amt          pic x(10).
           05  EXT-AH-PREM-RATE        PIC 99.9(5).
           05  ext-ah-prem-rate-a redefines
               ext-ah-prem-rate        pic x(8).
           05  EXT-AH-CRIT-PER         PIC 999.
           05  EXT-AH-REFUND           PIC 9(7).99.
           05  ext-ah-refund-a redefines
               ext-ah-refund           pic x(10).
           05  EXT-AH-EXP-DT           PIC X(10).
           05  EXT-AH-CAN-DT           PIC X(10).
           05  EXT-AH-CAN-EXT-DT       PIC X(10).
           05  EXT-AH-DIS-DT           PIC X(10).
           05  EXT-AGT1                PIC X(10).
           05  EXT-TYP1                PIC X.
           05  EXT-LF-COM1             PIC .9(5).
           05  ext-lf-com1-a redefines
               ext-lf-com1             pic x(6).
           05  EXT-AH-COM1             PIC .99999.
           05  ext-ah-com1-a redefines
               ext-ah-com1             pic x(6).
           05  EXT-AGT2                PIC X(10).
           05  EXT-TYP2                PIC X.
           05  EXT-LF-COM2             PIC .99999.
           05  ext-lf-com2-a redefines
               ext-lf-com2             pic x(6).
           05  EXT-AH-COM2             PIC .99999.
           05  ext-ah-com2-a redefines
               ext-ah-com2             pic x(6).
           05  EXT-AGT3                PIC X(10).
           05  EXT-TYP3                PIC X.
           05  EXT-LF-COM3             PIC .99999.
           05  ext-lf-com3-a redefines
               ext-lf-com3             pic x(6).
           05  EXT-AH-COM3             PIC .99999.
           05  ext-ah-com3-a redefines
               ext-ah-com3             pic x(6).
           05  EXT-AGT4                PIC X(10).
           05  EXT-TYP4                PIC X.
           05  EXT-LF-COM4             PIC .99999.
           05  ext-lf-com4-a redefines
               ext-lf-com4             pic x(6).
           05  EXT-AH-COM4             PIC .99999.
           05  ext-ah-com4-a redefines
               ext-ah-com4             pic x(6).
           05  EXT-AGT5                PIC X(10).
           05  EXT-TYP5                PIC X.
           05  EXT-LF-COM5             PIC .99999.
           05  ext-lf-com5-a redefines
               ext-lf-com5             pic x(6).
           05  EXT-AH-COM5             PIC .99999.
           05  ext-ah-com5-a redefines
               ext-ah-com5             pic x(6).
           05  EXT-ISSUE-CCYY          PIC 9999.
           05  EXT-BANK-NO             PIC X(10).
           05  EXT-LOAN-OFF            PIC X(5).
           05  EXT-INS-ADDR1           PIC X(30).
           05  EXT-INS-ADDR2           PIC X(30).
           05  EXT-INS-CITY-ST         PIC X(30).
           05  ext-ins-city            pic x(28).
           05  ext-ins-state           pic xx.
           05  EXT-INS-ZIP             PIC X(9). 
           05  EXT-LF-ISS-TAX          PIC 9.9999.
           05  ext-lf-iss-tax-a redefines
               ext-lf-iss-tax          pic x(6).
           05  EXT-LF-REF-TAX          PIC 9.9999.
           05  ext-lf-ref-tax-a redefines
               ext-lf-ref-tax          pic x(6).
           05  EXT-AH-ISS-TAX          PIC 9.9999.
           05  ext-ah-iss-tax-a redefines
               ext-ah-iss-tax          pic x(6).
           05  EXT-AH-REF-TAX          PIC 9.9999.
           05  ext-ah-ref-tax-a redefines
               ext-ah-ref-tax          pic x(6).
           05  EXT-REIN-TABLE          PIC XXX.
           05  EXT-AH-CLP              PIC 9(5).99.
           05  EXT-AH-CLP-A REDEFINES
               EXT-AH-CLP              PIC X(8).
           05  EXT-AH-RFND-CLP         PIC 9(5).99.
           05  EXT-AH-RFND-CLP-A REDEFINES
               EXT-AH-RFND-CLP         PIC X(8).
           05  EXT-ADDL-FEES           PIC 9(5).99.
           05  EXT-ADDL-FEES-A REDEFINES
               EXT-ADDL-FEES           PIC X(8).
           05  EXT-ACT-AH-CLP          PIC 9(5).99.
           05  EXT-ACT-AH-CLP-A REDEFINES
               EXT-ACT-AH-CLP          PIC X(8).
           05  EXT-ACT-AH-RFND-CLP     PIC 9(5).99.
           05  EXT-ACT-AH-RFND-CLP-A REDEFINES
               EXT-ACT-AH-RFND-CLP     PIC X(8).
           05  EXT-LF-CLP              PIC 9(5).99.
           05  EXT-LF-CLP-A REDEFINES
               EXT-LF-CLP              PIC X(8).
           05  EXT-LF-RFND-CLP         PIC 9(5).99.
           05  EXT-LF-RFND-CLP-A REDEFINES
               EXT-LF-RFND-CLP         PIC X(8).
           05  EXT-ACT-LF-CLP          PIC 9(5).99.
           05  EXT-ACT-LF-CLP-A REDEFINES
               EXT-ACT-LF-CLP          PIC X(8).
           05  EXT-ACT-LF-RFND-CLP     PIC 9(5).99.
           05  EXT-ACT-LF-RFND-CLP-A REDEFINES
               EXT-ACT-LF-RFND-CLP     PIC X(8).
070722     05  ext-cancel-reason       pic x.               

       01  filler.
           05  ws-table-name.
               10  ws-table-comp-id    pic xxx  value spaces.
               10  filler              pic x(8) value '_CERTALL'.


       01  ws-stuff-for-files-upload-data.
           05  ws-up-table-name        pic x(15).
           05  ws-up-file-name         pic x(15).
           05  ws-up-create-date       pic x(25).
           05  ws-up-modify-date       pic x(25).
           05  ws-up-full-path         pic x(25).
           05  ws-up-rec-count         pic 9(9).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  filler.
           05  ws-retention            pic s9(9)v9(5) comp-3 value +0.
           05  ws-wholesale-fee        pic s9(9)v9(5) comp-3 value +0.
           05  ws-acct-fee             pic s9(9)v9(2) comp-3 value +0.
           05  ws-cso-admin-fee        pic s9(9)v9(2) comp-3 value +0.
           05  ws-ccc-admin-fee        pic s9(9)v9(5) comp-3 value +0.
           05  ws-total-fees           pic s9(9)v9(5) comp-3 value +0.
           05  ws-ga-fee               pic s9(9)v9(2) comp-3 value +0.
           05  ws-clp-prem             pic s9(9)v9(5) comp-3 value +0.

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

       01  DATE-AREAS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC X(4).
               10  WS-WORK-MM          PIC XX.
               10  WS-WORK-DD          PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       copy ELCFUNDT.
                                       COPY ELCDTECX.
                                       copy ELCDTEVR.
                                       COPY ELCDATE.
       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       LINKAGE SECTION.                                                 
       01  var                         pic x(30).

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

           display ' Begin Program SQLBCERTALLD '

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if

           display ' KIXSYS = ' ws-kix-myenv
           
           move FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           display ' function date = ' function-date

           string
              ws-fn-ccyr '-'
              ws-fn-mo   '-'
              ws-fn-da   '  '
              ws-fn-hours ':'
              ws-fn-minutes ':'
              ws-fn-seconds '.000'
                 delimited by size into ws-sql-date-time
           end-string

           display ' sql date time ' ws-sql-date-time

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT
           PERFORM 0080-PROCESS-CERT   THRU 0080-EXIT UNTIL
PEMTST           (END-OF-CERT)
PEMTST*          OR (CERT-IN-CNT > 100000)

           PERFORM 1050-FINISH-UP      THRU 1050-EXIT
           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           GOBACK
           .
       0002-EXIT.
           EXIT.

       0010-INITIALIZE.

           MOVE DTE-CLIENT             TO ws-table-comp-id

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 1000-CONNECT        THRU 1000-EXIT
      *    PERFORM 1010-drop-table     THRU 1010-EXIT
           perform 1020-truncate-table thru 1020-exit
      *    PERFORM 1030-create-table   THRU 1030-EXIT
           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT CERTS ERMAIL

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL ERROR - OPEN ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' CERT IN RECORDS  ' CERT-IN-CNT
           DISPLAY ' EXTR OUT RECORDS ' EXTR-OUT-CNT
           CLOSE CERTS ERMAIL

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL ERROR - CLOSE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0060-READ-CERT.

           READ CERTS AT END
               SET END-OF-CERT TO TRUE
           END-READ

           IF NOT END-OF-CERT
              ADD 1 TO CERT-IN-CNT
           END-IF

           .
       0060-EXIT.
           EXIT.

       0080-PROCESS-CERT.

           PERFORM 0090-BUILD-EXTRACT  THRU 0090-EXIT
           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.

           move zeros                  to ws-retention    
                                          ws-wholesale-fee
                                          ws-acct-fee     
                                          ws-cso-admin-fee
                                          ws-ccc-admin-fee
                                          ws-total-fees   
                                          ws-ga-fee       
                                          ws-clp-prem     

           MOVE SPACES                 TO EXTRACT-RECORD
           MOVE run-date               TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-moe-date
           END-STRING
           MOVE CR-CARRIER             TO EXT-CARRIER
           MOVE CR-GROUPING            TO EXT-GROUP
           if cr-clp-state = spaces
              move cr-state            to cr-clp-state
           end-if
           move cr-clp-state           to ext-clp-state
           MOVE CR-STATE               TO EXT-STATE
           MOVE CR-ACCOUNT             TO EXT-ACCOUNT
           MOVE CR-CERT-NO             TO EXT-CERT-NO

           IF CR-ENTRY-STATUS = '5'
              MOVE +0                  TO CR-LFPRM
                                          CR-LFPRM-ALT
                                          CR-AHPRM
                                          cr-ah-clp
                                          CR-MOB-NET-TOT-FEES
           END-IF

           MOVE CR-DT                  TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-EFF-DT
           END-STRING

           MOVE WS-CCYY                TO EXT-ISSUE-CCYY

           STRING CR-1ST-PMT-MO '/' CR-1ST-PMT-DA '/'
              CR-1ST-PMT-YR DELIMITED BY SIZE
               INTO EXT-1ST-PAY-DT
           END-STRING

           MOVE CR-ENTRY-DATE          TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-ENT-DT
           END-STRING
           MOVE CR-ENTRY-STATUS        TO EXT-ENTRY-STATUS

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +11
              IF (CR-SOC-SEC (S1:1) < 'A'
                 OR > 'Z')
                 AND (CR-SOC-SEC (S1:1) NOT NUMERIC)
                 AND (CR-SOC-SEC (S1:1) NOT = ' ')
      *          DISPLAY ' FIXING SSN ' CR-CERT-NO
      *             ' ' CR-SOC-SEC (S1:1)
                 MOVE ' '              TO CR-SOC-SEC (S1:1)
              END-IF
           END-PERFORM
           MOVE CR-SOC-SEC             TO EXT-SSN

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +12
              IF (CR-MEMBER-NO (S1:1) < 'A'
                 OR > 'Z')
                 AND (CR-MEMBER-NO (S1:1) NOT NUMERIC)
                 AND (CR-MEMBER-NO (S1:1) NOT = ' ')
                 DISPLAY ' FIXING MEM NO ' CR-CERT-NO
                    ' ' CR-MEMBER-NO (S1:1)
                 MOVE ' '              TO CR-MEMBER-NO (S1:1)
              END-IF
           END-PERFORM
           MOVE CR-MEMBER-NO           TO EXT-MEMBER-NO

           IF CR-APR > 100.00
              MOVE ZEROS               TO CR-APR
           END-IF
           MOVE CR-APR                 TO EXT-APR
           MOVE CR-LOAN-TERM           TO EXT-LOAN-TERM
           MOVE CR-AGE                 TO EXT-INS-AGE
           MOVE CR-SEX                 TO EXT-SEX
           MOVE CR-IND-GRP             TO EXT-IND-GRP
           MOVE CR-PMT-FREQ            TO EXT-FREQ

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +26
              IF (CR-NAME (S1:1) < 'A'
                 OR > 'Z')
                 AND (CR-NAME (S1:1) NOT = ' ' AND '-' AND ',' AND '.')
                 DISPLAY ' FIXING CR NAME ' CR-CERT-NO
                    ' ' CR-NAME (S1:1)
                 MOVE SPACES             TO CR-NAME (S1:1)
              END-IF
           END-PERFORM
           MOVE CR-LNAME               TO EXT-LAST-NAME
           MOVE CR-FNAME               TO EXT-FIRST-NAME
           MOVE CR-INIT                TO EXT-MID-INIT

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +26
              IF (CR-JOINT-NAME (S1:1) < 'A'
                 OR > 'Z')
                 AND (CR-JOINT-NAME (S1:1) NOT = ' ' AND '-' AND ','
                    AND '.')
                 DISPLAY ' FIXING CR JOINT NAME ' CR-CERT-NO
                    ' ' CR-JOINT-NAME (S1:1)
                 MOVE SPACES             TO CR-JOINT-NAME (S1:1)
              END-IF
           END-PERFORM
           MOVE CR-JT-LNAME            TO EXT-JNT-LAST-NAME
           MOVE CR-JT-FNAME            TO EXT-JNT-FIRST-NAME
           MOVE CR-JT-INIT             TO EXT-JNT-MID-INIT

           MOVE CR-JOINT-AGE           TO EXT-JNT-AGE
           MOVE CR-PMT-EXTENSION-DAYS  TO EXT-EXT-DAYS

           MOVE CR-LFTYP               TO EXT-LF-BEN
           MOVE CR-LF-CURRENT-STATUS   TO EXT-LF-CUR-STATUS
           IF CR-LFAMT NOT NUMERIC
              DISPLAY ' CR-LFAMT NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-LFAMT
           END-IF
           MOVE CR-LFAMT               TO EXT-LF-AMT
           IF CR-LFAMT-ALT NOT NUMERIC
              DISPLAY ' CR-LFAMT-ALT NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-LFAMT-ALT
           END-IF
           MOVE CR-LFAMT-ALT           TO EXT-LF-AMT-ALT
           MOVE CR-LF-TERM             TO EXT-LF-TERM
           IF CR-LFPRM NOT NUMERIC
              DISPLAY ' CR-LFPRM NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-LFPRM
           END-IF
           MOVE CR-LFPRM               TO EXT-LF-PREM
           IF CR-LFPRM-ALT NOT NUMERIC
              DISPLAY ' CR-LFPRM-ALT NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-LFPRM-ALT
           END-IF
           MOVE CR-LFPRM-ALT           TO EXT-LF-PREM-ALT
           MOVE ZEROS                  TO EXT-LF-REM-AMT
           MOVE CR-LF-NSP-PRM          TO EXT-LF-NSP-PREM
           MOVE CR-LFPRM-RATE          TO EXT-LF-PREM-RATE
           MOVE CR-LFPRM-RATE-ALT      TO EXT-LF-PREM-RATE-ALT

           MOVE CR-LFRFND              TO EXT-LF-REFUND
           MOVE CR-DTHAMT              TO EXT-LF-DTH-AMT

           IF CR-LF-EXPIRE-DATE = ZEROS
              MOVE SPACES              TO EXT-LF-EXP-DT
           ELSE
              MOVE CR-LF-EXPIRE-DATE   TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-LF-EXP-DT
              END-STRING
           END-IF

           IF CR-LF-CANC-DT = ZEROS
              MOVE SPACES              TO EXT-LF-CAN-DT
           ELSE
              MOVE CR-LF-CANC-DT       TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-LF-CAN-DT
              END-STRING
           END-IF

           IF CR-LF-CANCEL-EXIT-DATE = ZEROS
              MOVE SPACES              TO EXT-LF-CAN-EXT-DT
           ELSE
              MOVE CR-LF-CANCEL-EXIT-DATE
                                       TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-LF-CAN-EXT-DT
              END-STRING
           END-IF

           IF CR-DTH-DT = ZEROS
              MOVE SPACES              TO EXT-LF-DTH-DT
           ELSE
              MOVE CR-DTH-DT           TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-LF-DTH-DT
              END-STRING
           END-IF

           IF CR-LF-CLAIM-EXIT-DATE = ZEROS
              MOVE SPACES              TO EXT-LF-DTH-EXT-DT
           ELSE
              MOVE CR-LF-CLAIM-EXIT-DATE
                                       TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-LF-DTH-EXT-DT
              END-STRING
           END-IF

           MOVE CR-AHTYP               TO EXT-AH-BEN
           MOVE CR-AH-CURRENT-STATUS   TO EXT-AH-CUR-STATUS
           IF CR-AHAMT NOT NUMERIC
              DISPLAY ' CR-AHAMT NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-AHAMT
           END-IF
           MOVE CR-AHAMT               TO EXT-AH-AMT
           MOVE CR-AH-TERM             TO EXT-AH-TERM
           IF CR-AHPRM NOT NUMERIC
              DISPLAY ' CR-AHPRM NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-AHPRM
           END-IF
           MOVE CR-AHPRM               TO EXT-AH-PREM
           IF CR-MOB-NET-TOT-FEES NOT NUMERIC
              MOVE ZEROS               TO CR-MOB-NET-TOT-FEES
           END-IF
           IF CR-MOB-NET-TOT-FEES NOT NUMERIC
              DISPLAY ' CR-MOB-NET-TOT-FEES NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-MOB-NET-TOT-FEES
           END-IF
           MOVE CR-MOB-NET-TOT-FEES    TO EXT-TOT-FEES
           MOVE CR-AH-NSP-PRM          TO EXT-AH-NSP-PREM
           MOVE CR-AHPRM-RATE          TO EXT-AH-PREM-RATE
           MOVE CR-AH-CRIT-PERIOD      TO EXT-AH-CRIT-PER

           MOVE CR-AHRFND              TO EXT-AH-REFUND

           IF CR-AH-EXPIRE-DATE = ZEROS
              MOVE SPACES              TO EXT-AH-EXP-DT
           ELSE
              MOVE CR-AH-EXPIRE-DATE   TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-AH-EXP-DT
              END-STRING
           END-IF

           IF CR-AH-CANC-DT = ZEROS
              MOVE SPACES              TO EXT-AH-CAN-DT
           ELSE
              MOVE CR-AH-CANC-DT       TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                 INTO EXT-AH-CAN-DT
              END-STRING
           END-IF

           IF CR-AH-CANCEL-EXIT-DATE = ZEROS
              MOVE SPACES              TO EXT-AH-CAN-EXT-DT
           ELSE
              MOVE CR-AH-CANCEL-EXIT-DATE
                                       TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-AH-CAN-EXT-DT
              END-STRING
           END-IF

           IF CR-DIS-DT = ZEROS
              MOVE SPACES              TO EXT-AH-DIS-DT
           ELSE
              MOVE CR-DIS-DT           TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-AH-DIS-DT
              END-STRING
           END-IF

           IF CR-DISAMT NOT NUMERIC
              MOVE ZEROS               TO CR-DISAMT
           END-IF
           MOVE CR-DISAMT              TO EXT-AH-CLM-AMT           

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > 5
              IF CR-COM-AGT (S1) = LOW-VALUES OR HIGH-VALUES
                                OR SPACES
                 MOVE ZEROS            TO CR-COM-AGT (S1)
              END-IF
           END-PERFORM
           MOVE CR-COM-AGT (1)         TO EXT-AGT1
           MOVE CR-AGT-TYPE (1)        TO EXT-TYP1
           MOVE CR-LCOM-L (1)          TO EXT-LF-COM1
           MOVE CR-LCOM-AH (1)         TO EXT-AH-COM1
           MOVE CR-COM-AGT (2)         TO EXT-AGT2
           MOVE CR-AGT-TYPE (2)        TO EXT-TYP2
           MOVE CR-LCOM-L (2)          TO EXT-LF-COM2
           MOVE CR-LCOM-AH (2)         TO EXT-AH-COM2
           MOVE CR-COM-AGT (3)         TO EXT-AGT3
           MOVE CR-AGT-TYPE (3)        TO EXT-TYP3
           MOVE CR-LCOM-L (3)          TO EXT-LF-COM3
           MOVE CR-LCOM-AH (3)         TO EXT-AH-COM3
           MOVE CR-COM-AGT (4)         TO EXT-AGT4
           MOVE CR-AGT-TYPE (4)        TO EXT-TYP4
           MOVE CR-LCOM-L (4)          TO EXT-LF-COM4
           MOVE CR-LCOM-AH (4)         TO EXT-AH-COM4
           MOVE CR-COM-AGT (5)         TO EXT-AGT5
           MOVE CR-AGT-TYPE (5)        TO EXT-TYP5
           MOVE CR-LCOM-L (5)          TO EXT-LF-COM5
           MOVE CR-LCOM-AH (5)         TO EXT-AH-COM5
           MOVE CR-BANK-NO             TO EXT-BANK-NO
           MOVE CR-LOAN-OFFICER        TO EXT-LOAN-OFF
           MOVE CR-LF-ISS-PREM-TAX     TO EXT-LF-ISS-TAX
           MOVE CR-LF-CNC-PREM-TAX     TO EXT-LF-REF-TAX
           MOVE CR-AH-ISS-PREM-TAX     TO EXT-AH-ISS-TAX
           MOVE CR-AH-CNC-PREM-TAX     TO EXT-AH-REF-TAX
           MOVE CR-REIN-TABLE          TO EXT-REIN-TABLE
           if cr-ah-clp not numeric
              move zeros               to cr-ah-clp
           end-if
           if cr-ah-rfnd-clp not numeric
              move zeros               to cr-ah-rfnd-clp
           end-if
           if cr-addl-clp not numeric
              move zeros               to cr-addl-clp
           end-if
              
           if cr-lf-clp not numeric
              move zeros               to cr-lf-clp
           end-if
           if cr-lf-rfnd-clp not numeric
              move zeros               to cr-lf-rfnd-clp
           end-if
           IF CR-ENTRY-STATUS <> '5'
              MOVE CR-AH-CLP           TO EXT-AH-CLP
              move cr-lf-clp           to ext-lf-clp
           else
              move zeros               to ext-ah-clp
                                          ext-lf-clp
           END-IF

           move cr-ah-clp              to ext-act-ah-clp
           move cr-ah-rfnd-clp         to ext-act-ah-rfnd-clp
           move cr-ah-rfnd-clp         to ext-ah-rfnd-clp

           move cr-lf-clp              to ext-act-lf-clp
      *    move cr-lf-nsp-prm          to ext-lf-clp
           move cr-lf-rfnd-clp         to ext-lf-rfnd-clp

           move cr-lf-rfnd-clp         to ext-act-lf-rfnd-clp

      *    perform 0095-calc-clp       thru 0095-exit
           MOVE CR-ADDL-CLP            TO EXT-ADDL-FEES
070722     MOVE CR-CANCEL-REASON       TO EXT-CANCEL-REASON           

           PERFORM 0110-GET-ERMAIL  THRU 0110-EXIT
           
           IF ERMAIL-FILE-STATUS = '00'
              MOVE MA-ADDRESS-LINE-1   TO EXT-INS-ADDR1
              MOVE MA-ADDRESS-LINE-2   TO EXT-INS-ADDR2
              MOVE MA-CITY-STATE       TO EXT-INS-CITY-ST
              move ma-city             to ext-ins-city
              move ma-addr-state       to ext-ins-state
              MOVE MA-ZIP              TO EXT-INS-ZIP
           END-IF

           PERFORM 0100-WRITE-EXTRACT  THRU 0100-EXIT

           .
       0090-EXIT.
           EXIT.

       0095-calc-clp.

           if cr-carrier <> '7'
              go to 0095-try-carrier-5
           end-if

           if (cr-entry-status = '5')
              and (cr-ahrfnd = zeros)
              go to 0095-exit
           end-if

           perform varying a1 from +1 by +1 until a1 > +10
              evaluate true
                 when cr-agt-type (a1) = 'C' or 'D'
                    compute ws-acct-fee rounded =
                       ws-acct-fee + (cr-ahprm * cr-lcom-ah (a1))
                 when cr-agt-type (a1) = 'S'
                    compute ws-cso-admin-fee rounded =
                       ws-cso-admin-fee + (cr-ahprm * cr-lcom-ah (a1))
                 when cr-agt-type (a1) = 'O' OR 'P'
                    compute ws-ga-fee rounded =
                       ws-ga-fee + (cr-ahprm * cr-lcom-ah (a1))
              end-evaluate
           end-perform

           if cr-ahrfnd = zeros
              compute ws-clp-prem rounded =
                 cr-ahprm - ws-cso-admin-fee -
                    ws-ga-fee - ws-acct-fee
              move ws-clp-prem      to ext-ah-clp
           else
              perform varying a1 from +1 by +1 until a1 > +10
                 evaluate true
                    when cr-agt-type (a1) = 'C' or 'D'
                       compute ws-acct-fee rounded =
                          ws-acct-fee + (cr-ahrfnd * cr-lcom-ah (a1))
                    when cr-agt-type (a1) = 'S'
                       compute ws-cso-admin-fee rounded =
                       ws-cso-admin-fee + (cr-ahrfnd * cr-lcom-ah (a1))
                    when cr-agt-type (a1) = 'O' OR 'P'
                       compute ws-ga-fee rounded =
                          ws-ga-fee + (cr-ahrfnd * cr-lcom-ah (a1))
                 end-evaluate
              end-perform
              compute ws-clp-prem rounded =
                 cr-ahrfnd - ws-cso-admin-fee - ws-ga-fee - ws-acct-fee
              move ws-clp-prem         to ext-ah-rfnd-clp
           end-if

           go to 0095-exit

           .
       0095-try-carrier-5.

           if cr-carrier <> '1' and '3' and '5'
              go to 0095-try-other
           end-if

           if cr-lftyp = '00' or '  '
              go to 0095-do-ah
           end-if

           move cr-lf-nsp-prm          to ext-lf-clp
           move zeros                  to ws-acct-fee

           if (cr-carrier = '1')
                    or
                 ((cr-carrier = '5')
                 and (cr-lftyp (1:1) = 'P' or 'N' OR 'S'))
                     or
                 ((cr-carrier = '3')
                 and (cr-lftyp (1:1) = 'H'))
              perform varying a1 from +1 by +1 until a1 > +10
                 evaluate true
                    when cr-agt-type (a1) = 'C' or 'D'
                       compute ws-acct-fee rounded =
                       ws-acct-fee + (cr-lfprm * cr-lcom-l (a1))
                 end-evaluate
              end-perform
              compute ws-clp-prem = cr-lfprm - ws-acct-fee
              move ws-clp-prem         to ext-lf-clp
           end-if


           move zeros                  to ws-acct-fee
           if cr-lfrfnd not = zeros
              perform varying a1 from +1 by +1 until a1 > +10
                 evaluate true
                    when cr-agt-type (a1) = 'C' or 'D'
                       compute ws-acct-fee rounded =
                       ws-acct-fee + (cr-lfrfnd * cr-lcom-l (a1))
                 end-evaluate
              end-perform
              compute ws-lf-refund = cr-lfrfnd - ws-acct-fee
              move ws-lf-refund        to ext-lf-rfnd-clp
           end-if

      *     if cr-lfrfnd not = zeros
      *        if (cr-lfprm > zero)
      *           compute ws-factor rounded = cr-lfrfnd / cr-lfprm
      *           compute ws-lf-refund rounded =
      *              cr-lf-nsp-prm * ws-factor
      *           move ws-lf-refund     to ext-lf-rfnd-clp
      *        else
      *           move cr-lfrfnd        to ext-lf-rfnd-clp
      *     end-if

           .
       0095-do-ah.

           if cr-ahtyp = '00' or '  '
              go to 0095-exit
           end-if

           move cr-ah-nsp-prm          to ext-ah-clp
           move zeros                  to ws-acct-fee

           if (cr-carrier = '1')
                    or
                 ((cr-carrier = '5')
                 and (cr-ahtyp (1:1) = 'P' or 'N' OR 'S'))
                     or
                 ((cr-carrier = '3')
                 and (cr-ahtyp (1:1) = 'H'))
              perform varying a1 from +1 by +1 until a1 > +10
                 evaluate true
                    when cr-agt-type (a1) = 'C' or 'D'
                       compute ws-acct-fee rounded =
                       ws-acct-fee + (cr-ahprm * cr-lcom-ah (a1))
                 end-evaluate
              end-perform
              compute ws-clp-prem = cr-ahprm - ws-acct-fee
              move ws-clp-prem         to ext-ah-clp
           end-if

           move zeros                  to ws-acct-fee
           if cr-ahrfnd not = zeros
              perform varying a1 from +1 by +1 until a1 > +10
                 evaluate true
                    when cr-agt-type (a1) = 'C' or 'D'
                       compute ws-acct-fee rounded =
                       ws-acct-fee + (cr-ahrfnd * cr-lcom-ah (a1))
                 end-evaluate
              end-perform
              compute ws-ah-refund = cr-ahrfnd - ws-acct-fee
              move ws-ah-refund        to ext-ah-rfnd-clp
           end-if

      *    if cr-ahrfnd <> zeros
      *       compute ws-clp-prem rounded =
      *          (cr-ah-nsp-prm / cr-ahprm) * cr-ahrfnd
      *       move ws-clp-prem         to ext-ah-rfnd-clp
      *    end-if

           .
       0095-try-other.

           if cr-carrier <> '2' and '4' and '6'
              go to 0095-exit
           end-if

           if cr-lftyp = '00' or '  '
              go to 0095-do-other-ah
           end-if

           move cr-lf-nsp-prm          to ext-lf-clp
           move zeros                  to ws-acct-fee

           if cr-lf-nsp-prm not = zeros
              move cr-lf-nsp-prm       to ext-lf-clp
           end-if

           if cr-lfrfnd <> zeros
              if cr-lfprm > zeros
                 compute ws-factor =
                    cr-lfrfnd / cr-lfprm
                 compute ws-lf-refund =
                    cr-lf-nsp-prm * ws-factor

                 if cr-lf-rfnd-clp <> zeros
                    move cr-lf-rfnd-clp
                                       to ext-lf-rfnd-clp
                 else
                    move ws-lf-refund  to ext-lf-rfnd-clp
                 end-if
              end-if
           end-if

           .
       0095-do-other-ah.

           if cr-ahtyp = '00' or '  '
              go to 0095-exit
           end-if

      *    if cr-ah-nsp-prm not = zeros
      *       move cr-ah-nsp-prm       to ext-ah-clp
      *    end-if
      *
      *    if cr-ahrfnd <> zeros
      *       if cr-ahprm > zeros
      *          compute ws-factor =
      *             cr-ahrfnd / cr-ahprm
      *          compute ws-ah-refund =
      *             cr-ah-nsp-prm * ws-factor
      *          if cr-ah-rfnd-clp <> zeros
      *             move cr-ah-rfnd-clp
      *                                to ext-ah-rfnd-clp
      *          else
      *             move ws-ah-refund  to ext-ah-rfnd-clp
      *          end-if
      *       end-if
      *    end-if

           .
       0095-exit.
           exit.

       0100-WRITE-EXTRACT.

           display ' made it to 0100-write '

           INSPECT EXTRACT-RECORD REPLACING
              ALL ';'         BY ' '
              ALL X'00'       BY ' '
              ALL X'01'       BY ' '
              ALL X'02'       BY ' '
              ALL X'03'       BY ' '
              ALL X'04'       BY ' '
              ALL X'05'       BY ' '
              ALL X'06'       BY ' '
              ALL X'07'       BY ' '
              ALL X'08'       BY ' '
              ALL X'0C'       BY ' '
              ALL X'14'       BY ' '
              ALL X'1B'       BY ' '
              ALL X'1C'       BY ' '
              ALL X'1E'       BY ' '
              ALL X'9C'       BY ' '
              ALL X'09'       BY ';'
           PERFORM 1040-INSERT-ROW     THRU 1040-EXIT
           ADD 1 TO EXTR-OUT-CNT

           .
       0100-EXIT.
           EXIT.

       0110-GET-ERMAIL.

           MOVE LOW-VALUES             TO MA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO MA-COMPANY-CD
           MOVE CR-CARRIER             TO MA-CARRIER
           MOVE CR-GROUPING            TO MA-GROUPING
           MOVE CR-STATE               TO MA-STATE
           MOVE CR-ACCOUNT             TO MA-ACCOUNT
           MOVE CR-CERT-NO             TO MA-CERT-NO
           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO MA-CERT-EFF-DT
              READ ERMAIL
              IF ERMAIL-FILE-STATUS = '00' OR '23' OR '22' OR '10'
                 CONTINUE
              ELSE
                 DISPLAY ' ERMAIL ERROR - READ ' ERMAIL-FILE-STATUS
                    '  ' CR-CERT-NO
           ELSE
              DISPLAY ' BAD EFF DATE ' CR-DT '  ' CR-CERT-NO   
           END-IF
           
           .
       0110-EXIT.
           EXIT.

       1000-connect.

           display ' about to connect to Logic '

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

NTTDel**       CONNECT TO :svr
NTTDel**             USER :usr-pass
NTTIns*        CONNECT TO :svr
NTTIns*          USER     :usr
NTTIns*          USING    :pass
      *     END-EXEC

           if sqlcode not = 0
              display "Error: cannot connect to Logic"
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              perform abend-pgm
           end-if

           .
       1000-exit.
           exit.

       1010-drop-table.

           move spaces                 to sqlcmd
           string
              'drop table '
              ws-table-name
              delimited by size into sqlcmd
           end-string

           display 'Begin Drop table' sqlcmd

            EXEC SQL
               EXECUTE IMMEDIATE :sqlcmd
            END-EXEC.

           if sqlcode not = 0
              display "Error(anticipated) : cannot drop table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       1010-exit.
           exit.

       1020-truncate-table.

           move spaces                 to sqlcmd
           string
              'truncate table '
              ws-table-name
              delimited by size into sqlcmd
           end-string

           display 'Begin Truncate table'

           EXEC SQL
              EXECUTE IMMEDIATE :sqlcmd
           END-EXEC.

           if sqlcode not = 0
              display "Error : cannot truncate table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       1020-exit.
           exit.

       1030-create-table.

           display ' Begin Create table '
           move spaces                 to sqlcmd

           string
              'create table '
              ws-table-name       ' ('
                 'CARRIER char(1) NOT NULL, '
                 'GROUPING char(6) NOT NULL, '
                 'CLP_STATE char(2) NOT NULL, '
                 'STATE char(2) NOT NULL, '
                 'ACCOUNT char(10) NOT NULL, '
                 'EFF_DATE datetime NOT NULL, '
                 'CERT_NO char(11) NOT NULL, '
                 'ENT_DATE datetime NULL, '
                 'FIRST_PAY_DATE datetime NULL, '
                 'ENT_STATUS varchar(1) NULL, '
                 'LAST_NAME varchar(15) NULL, '
                 'FIRST_NAME varchar(10) NULL, '
                 'MID_INIT varchar(1) NULL, '
                 'JNT_LAST_NAME varchar(15) NULL, '
                 'JNT_FIRST_NAME varchar(10) NULL, '
                 'JNT_MID_INIT varchar(1) NULL, '
                 'SSN varchar(11) NULL, '
                 'MEMBER_NO varchar(12) NULL, '
                 'SEX varchar(1) NULL, '
                 'INS_AGE tinyint NULL, '
                 'JNT_AGE tinyint NULL, '
                 'APR decimal(7, 4) NULL, '
                 'LOAN_TERM smallint NULL, '
                 'EXT_DAYS smallint NULL, '
                 'IND_GRP varchar(1) NULL, '
                 'FREQ tinyint NULL, '
                 'LF_BEN_CODE varchar(2) NULL, '
                 'LF_CUR_STATUS varchar(1) NULL, '
                 'LF_TERM smallint NULL, '
                 'LF_AMT decimal(11, 2) NULL, '
                 'LF_PREM decimal(11, 2) NULL, '
                 'LF_AMT_ALT  decimal(11, 2) NULL, '
                 'LF_PREM_ALT decimal(11, 2) NULL, '
                 'LF_NSP_PREM decimal(11, 2) NULL, '
                 'DEATH_AMT  decimal(11, 2) NULL, '
                 'LF_REM_AMT decimal(11, 2) NULL, '
                 'LF_PREM_RATE decimal(7, 5) NULL, '
                 'LF_PREM_RATE_ALT   decimal(7, 5) NULL, '
                 'LF_REFUND decimal(11, 2) NULL, '
                 'LF_EXP_DATE datetime NULL, '
                 'LF_CAN_DATE datetime NULL, '
                 'LF_CAN_EXT_DT datetime NULL, '
                 'LF_DTH_DATE  datetime NULL, '
                 'LF_DTH_EXT_DT datetime NULL, '
                 'AH_BEN_CODE   varchar(2) NULL, '
                 'AH_CUR_STATUS varchar(1) NULL, '
                 'AH_TERM smallint NULL, '
                 'AH_AMT  decimal(11, 2) NULL, '
                 'AH_PREM decimal(11, 2) NULL, '
                 'NET_TOT_FEES decimal(11, 2) NULL, '
                 'AH_NSP_PREM  decimal(11, 2) NULL, '
                 'AH_CLM_AMT   decimal(11, 2) NULL, '
                 'AH_PREM_RATE decimal(7, 5) NULL, '
                 'AH_CRIT_PER  smallint NULL, '
                 'AH_REFUND    decimal(11, 2) NULL, '
                 'AH_EXP_DATE  datetime NULL, '
                 'AH_CAN_DATE  datetime NULL, '
                 'AH_CAN_EXT_DT datetime NULL, '
                 'AH_DIS_DATE datetime NULL, '
                 'AGT_1 varchar(10) NULL, '
                 'TYP_1 varchar(1) NULL, '
                 'LF_1  decimal(6, 5) NULL, '
                 'AH_1  decimal(6, 5) NULL, '
                 'AGT_2 varchar(10) NULL, '
                 'TYP_2 varchar(1) NULL, '
                 'LF_2  decimal(6, 5) NULL, '
                 'AH_2  decimal(6, 5) NULL, '
                 'AGT_3 varchar(10) NULL, '
                 'TYP_3 varchar(1) NULL, '
                 'LF_3  decimal(6, 5) NULL, '
                 'AH_3  decimal(6, 5) NULL, '
                 'AGT_4 varchar(10) NULL, '
                 'TYP_4 varchar(1) NULL, '
                 'LF_4  decimal(6, 5) NULL, '
                 'AH_4  decimal(6, 5) NULL, '
                 'AGT_5 varchar(10) NULL, '
                 'TYP_5 varchar(1) NULL, '
                 'LF_5  decimal(6, 5) NULL, '
                 'AH_5  decimal(6, 5) NULL, '
                 'ISS_CCYY  varchar(4) NULL, '
                 'BANK_NO   varchar(10) NULL, '
                 'LOAN_OFF  varchar(5) NULL, '
                 'INS_ADDR1 varchar(30) NULL, '
                 'INS_ADDR2 varchar(30) NULL, '
                 'INS_CITY_ST varchar(30) NULL, '
                 'INS_CITY    varchar(30) NULL, '
                 'INS_ST   varchar(2) NULL, '
                 'INS_ZIP  varchar(9) NULL, '
                 'LF_ISS_TAX decimal(6, 5) NULL, '
                 'LF_REF_TAX decimal(6, 5) NULL, '
                 'AH_ISS_TAX decimal(6, 5) NULL, '
                 'AH_REF_TAX decimal(6, 5) NULL, '
                 'REIN_TABLE varchar(3), '
                 'AH_CLP  decimal(7, 2) NULL, '
                 'AH_RFND_CLP decimal(7, 2) NULL, '
                 'ADDL_FEES   decimal(7, 2) NULL, '
                 'LF_CLP decimal(7, 2) NULL, '
                 'LF_RFND_CLP decimal(7, 2) NULL '
                 'CONSTRAINT PK_'WS-TABLE-NAME
                 ' PRIMARY KEY CLUSTERED'
                  '(CARRIER, GROUPING, CLP_STATE, STATE,'
                  ' ACCOUNT, EFF_DATE, CERT_NO)'
                  ')'
              delimited by size into sqlcmd
           end-string

           display ' Create table sql stmt ' sqlcmd

           exec sql
              EXECUTE IMMEDIATE :sqlcmd
           end-exec

           if sqlcode not = 0
              display "Error: cannot create table "
              move sqlcode             to ws-disp-code
              display ' sql return code ' ws-disp-code
              display ' sql err mess    ' sqlerrmc
              PERFORM 1050-FINISH-UP   THRU 1050-EXIT
              PERFORM ABEND-PGM
           end-if

           .
       1030-exit.
           exit.

       1040-INSERT-ROW.

           display ' made it to 1040- ' ws-table-name


           perform 1045-check-dates    thru 1045-exit

           move spaces                 to sqlcmd
           string
              'INSERT INTO '
               ws-table-name
               ' ('
               'CARRIER, '
               'GROUPING, '
               'CLP_STATE, '
               'STATE, '
               'ACCOUNT, '
               'EFF_DATE, '
               'CERT_NO, '
               'ENT_DATE, '
               'FIRST_PAY_DATE, '
               'ENT_STATUS, '
               'LAST_NAME, '
               'FIRST_NAME, '
               'MID_INIT, '
               'JNT_LAST_NAME, '
               'JNT_FIRST_NAME, '
               'JNT_MID_INIT, '
               'SSN, '
               'MEMBER_NO, '
               'SEX, '
               'INS_AGE, '
               'JNT_AGE, '
               'APR, '
               'LOAN_TERM, '
               'EXT_DAYS, '
               'IND_GRP, '
               'FREQ, '
               'LF_BEN_CODE, '
               'LF_CUR_STATUS, '
               'LF_TERM, '
               'LF_AMT, '
               'LF_PREM, '
               'LF_AMT_ALT, '
               'LF_PREM_ALT, '
               'LF_NSP_PREM, '
               'DEATH_AMT, '
               'LF_REM_AMT, '
               'LF_PREM_RATE, '
               'LF_PREM_RATE_ALT, '
               'LF_REFUND, '
               'LF_EXP_DATE, '
               'LF_CAN_DATE, '
               'LF_CAN_EXT_DT, '
               'LF_DTH_DATE, '
               'LF_DTH_EXT_DT, '
               'AH_BEN_CODE, '
               'AH_CUR_STATUS, '
               'AH_TERM, '
               'AH_AMT, '
               'AH_PREM, '
               'NET_TOT_FEES, '
               'AH_NSP_PREM, '
               'AH_CLM_AMT, '
               'AH_PREM_RATE, '
               'AH_CRIT_PER, '
               'AH_REFUND, '
               'AH_EXP_DATE, '
               'AH_CAN_DATE, '
               'AH_CAN_EXT_DT, '
               'AH_DIS_DATE, '
               'AGT_1, '
               'TYP_1, '
               'LF_1, '
               'AH_1, '
               'AGT_2, '
               'TYP_2, '
               'LF_2, '
               'AH_2, '
               'AGT_3, '
               'TYP_3, '
               'LF_3, '
               'AH_3, '
               'AGT_4, '
               'TYP_4, '
               'LF_4, '
               'AH_4, '
               'AGT_5, '
               'TYP_5, '
               'LF_5, '
               'AH_5, '
               'ISS_CCYY, '
               'BANK_NO, '
               'LOAN_OFF, '
               'INS_ADDR1, '
               'INS_ADDR2, '
               'INS_CITY_ST, '
               'INS_CITY, '
               'INS_ST, '
               'INS_ZIP, '
               'LF_ISS_TAX, '
               'LF_REF_TAX, '
               'AH_ISS_TAX, '
               'AH_REF_TAX, '
               'REIN_TABLE, '
               'AH_CLP, '
               'AH_RFND_CLP, '
               'ADDL_FEES, '
               'LF_CLP, '
070722         'LF_RFND_CLP, '
070722         'CANCEL_REASON'
               ')'
	             ' values ('
                "'"EXT-CARRIER"', '"
                   EXT-GROUP"', '"
                   ext-clp-state"', '"
                   EXT-STATE"', '"
                   EXT-ACCOUNT"', '"
                   EXT-EFF-DT"', '"
                   EXT-CERT-NO"', '"
                   EXT-ENT-DT"', '"
                   EXT-1ST-PAY-DT"', '"
                   EXT-ENTRY-STATUS"', '"
                   EXT-LAST-NAME"', '"
                   EXT-FIRST-NAME"', '"
                   EXT-MID-INIT"', '"
                   EXT-JNT-LAST-NAME"', '"
                   EXT-JNT-FIRST-NAME"', '"
                   EXT-JNT-MID-INIT"', '"
                   EXT-SSN"', '"
                   EXT-MEMBER-NO"', '"
                   EXT-SEX"', '"
                   EXT-INS-AGE"', '"
                   EXT-JNT-AGE"', '"
                   EXT-APR-a"', '"
                   EXT-LOAN-TERM"', '"
                   EXT-EXT-DAYS"', '"
                   EXT-IND-GRP"', '"
                   EXT-FREQ"', '"
                   EXT-LF-BEN"', '"
                   EXT-LF-CUR-STATUS"', '"
                   EXT-LF-TERM"', '"
                   EXT-LF-AMT-a"', '"
                   EXT-LF-PREM-a"', '"
                   EXT-LF-AMT-ALT-a"', '"
                   EXT-LF-PREM-ALT-a"', '"
                   EXT-LF-NSP-PREM-a"', '"
                   EXT-LF-DTH-AMT-a"', '"
                   EXT-LF-REM-AMT-a"', '"
                   EXT-LF-PREM-RATE-a"', '"
                   EXT-LF-PREM-RATE-ALT-a"', '"
                   EXT-LF-REFUND-a"', '"
                   EXT-LF-EXP-DT"', '"
                   EXT-LF-CAN-DT"', '"
                   EXT-LF-CAN-EXT-DT"', '"
                   EXT-LF-DTH-DT"', '"
                   EXT-LF-DTH-EXT-DT"', '"
                   EXT-AH-BEN"', '"
                   EXT-AH-CUR-STATUS"', '"
                   EXT-AH-TERM"', '"
                   EXT-AH-AMT-a"', '"
                   EXT-AH-PREM-a"', '"
                   EXT-TOT-FEES-a"', '"
                   EXT-AH-NSP-PREM-a"', '"
                   EXT-AH-CLM-AMT-a"', '"
                   EXT-AH-PREM-RATE-a"', '"
                   EXT-AH-CRIT-PER"', '"
                   EXT-AH-REFUND-a"', '"
                   EXT-AH-EXP-DT"', '"
                   EXT-AH-CAN-DT"', '"
                   EXT-AH-CAN-EXT-DT"', '"
                   EXT-AH-DIS-DT"', '"
                   EXT-AGT1"', '"
                   EXT-TYP1"', '"
                   EXT-LF-COM1-a"', '"
                   EXT-AH-COM1-a"', '"
                   EXT-AGT2"', '"
                   EXT-TYP2"', '"
                   EXT-LF-COM2-a"', '"
                   EXT-AH-COM2-a"', '"
                   EXT-AGT3"', '"
                   EXT-TYP3"', '"
                   EXT-LF-COM3-a"', '"
                   EXT-AH-COM3-a"', '"
                   EXT-AGT4"', '"
                   EXT-TYP4"', '"
                   EXT-LF-COM4-a"', '"
                   EXT-AH-COM4-a"', '"
                   EXT-AGT5"', '"
                   EXT-TYP5"', '"
                   EXT-LF-COM5-a"', '"
                   EXT-AH-COM5-a"', '"
                   EXT-ISSUE-CCYY"', '"
                   EXT-BANK-NO"', '"
                   EXT-LOAN-OFF"', '"
                   EXT-INS-ADDR1"', '"
                   EXT-INS-ADDR2"', '"
                   EXT-INS-CITY-ST"', '"
                   EXT-INS-CITY"', '"
                   EXT-INS-STATE"', '"
                   EXT-INS-ZIP"', '"
                   EXT-LF-ISS-TAX-a"', '"
                   EXT-LF-REF-TAX-a"', '"
                   EXT-AH-ISS-TAX-a"', '"
                   EXT-AH-REF-TAX-a"', '"
                   EXT-REIN-TABLE"', '"
                   EXT-AH-CLP-A"', '"
                   EXT-AH-RFND-CLP-A"', '"
                   EXT-ADDL-FEES-A"', '"
                   EXT-LF-CLP-A"', '"
070722             EXT-LF-RFND-CLP-A"', '"
070722             EXT-CANCEL-REASON"')"
              delimited by size into ws-sqlcmd-in
           end-string

           display ' insert sql cmd ' ws-sqlcmd-in

           perform 1046-find-nulls     thru 1046-exit
           move ws-sqlcmd-out          to sqlcmd

           display ' cmd **' sqlcmd '**'
      
      *    display ' Begin Exp Dt Updates '

           exec sql
              EXECUTE IMMEDIATE :sqlcmd
           end-exec

           if sqlcode not = 0
              display "Error: cannot insert row "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' in out cnt ' ws-recs-in ' ' rec-cnt
              display ' offending rec ' extract-record
              PERFORM 1050-FINISH-UP   THRU 1050-EXIT
              PERFORM ABEND-PGM
           end-if

           add 1 to ws-records-inserted

           .
       1040-EXIT.
           EXIT.

       1045-check-dates.

           if ext-lf-exp-dt = spaces
              move 'NULL'              to ext-lf-exp-dt
           end-if
           if ext-lf-can-dt = spaces
              move 'NULL'              to ext-lf-can-dt
           end-if
           if ext-lf-can-ext-dt = spaces
              move 'NULL'              to ext-lf-can-ext-dt
           end-if
           if ext-lf-dth-dt = spaces
              move 'NULL'              to ext-lf-dth-dt
           end-if
           if ext-lf-dth-ext-dt = spaces
              move 'NULL'              to ext-lf-dth-ext-dt
           end-if
           if ext-ah-exp-dt = spaces
              move 'NULL'              to ext-ah-exp-dt
           end-if
           if ext-ah-can-dt = spaces
              move 'NULL'              to ext-ah-can-dt
           end-if
           if ext-ah-can-ext-dt = spaces
              move 'NULL'              to ext-ah-can-ext-dt
           end-if
           if ext-ah-dis-dt = spaces
              move 'NULL'              to ext-ah-dis-dt
           end-if

           .
       1045-EXIT.
           EXIT.

       1046-find-nulls.

           move +1                     to o1
           perform varying i1 from +1 by +1 until
              (i1 > +3072)
              if ws-sqlcmd-in (i1:12) = "'NULL      '"
                 move 'NULL'           TO WS-SQLCMD-OUT (O1:4)
                 add +4 to o1
                 add +11 to i1
              else
                 move ws-sqlcmd-in (i1:1) to ws-sqlcmd-out (o1:1)
                 add +1 to o1
              end-if
           end-perform

           .
       1046-exit.
           exit.

       1050-finish-up.

          if dte-client = 'DCC'
             move 'CERTALL_DCC'        to ws-up-table-name
             move 'DC.XX.CERT_00'      to ws-up-file-name
          else
             move 'CERTALL_VPP'        to ws-up-table-name
             move 'VP.XX.CERT_00'      to ws-up-file-name
          end-if
          move '/data/seqfiles'        to ws-up-full-path
          move ws-records-inserted     to ws-up-rec-count
          move ws-sql-date-time        to ws-up-create-date
                                          ws-up-modify-date

          EXEC SQL
             CALL logic_Insert_FilesUploadedData
NTTIns                (
	              @table_name          = :ws-up-table-name,
	              @file_name           = :ws-up-file-name,
	              @file_created_date   = :ws-up-create-date,
	              @file_modified_date  = :ws-up-modify-date,
	              @file_full_path      = :ws-up-full-path,
	              @loaded_record_count = :ws-up-rec-count
NTTIns                )
          END-EXEC

           IF SQLCODE NOT = 0
              DISPLAY "ERROR: DID NOT update upload info "
              DISPLAY ' SQL RETURN CODE ' SQLCODE
              DISPLAY ' SQL ERR MESS    ' SQLERRMC
           END-IF

           EXEC SQL
NTTDel*        commit work release
NTTIns         commit work
           END-EXEC

           if sqlcode not = 0
              display "Error: commit work release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           EXEC SQL
              DISCONNECT
           END-EXEC

           if sqlcode not = 0
              display "Error: disconnect  "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       1050-exit.
           exit.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
