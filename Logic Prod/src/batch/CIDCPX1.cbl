       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDCPX1.
       AUTHOR.        Cowtown.
       DATE-COMPILED.

      *REMARKS. This program builds the PREM_COMM
      * every month.

082222******************************************************************
082222*                   C H A N G E   L O G
082222*
082222* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082222*-----------------------------------------------------------------
082222*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082222* EFFECTIVE    NUMBER
082222*-----------------------------------------------------------------
082222* 082222  CR2019????00003  PEMA  New program
082222******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT COMM-TRAN-IN ASSIGN TO SYS010.

           select eracct           ASSIGN TO ERACCTT
              organization is indexed
              access is dynamic
              record key is am-control-primary
              file status is eracct-file-status.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT FILE-OUT             ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  COMM-TRAN-IN 
                                       COPY ECSCOMFD.
       01  cp-record-in                pic x(270).

       FD  ERACCT.
                                       COPY ERCACCT.
       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  FILE-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.
      
       01  FILE-OUT-REC                PIC X(260).

       WORKING-STORAGE SECTION.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  INPUT-CNT                   PIC 9(11)  VALUE ZEROS.
       77  OUTPUT-CNT                  PIC 9(11)  VALUE ZEROS.
       77  ws-sql-date-time            pic x(24) value spaces.
       77  eracct-file-status          pic xx value low-values.

       01  WS-MOE-DATE                 pic x(10).


       01  EXTRACT-RECORD.
           05  EXT-MOE-DATE            PIC X(10).
           05  EXT-CARRIER             PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-ACCOUNT             PIC X(10).
           05  ext-remit               pic x(10).
           05  EXT-STATE               PIC XX.
           05  EXT-EFF-DT              PIC X(10).
           05  EXT-CERT-NO             PIC X(11).
           05  ext-trans               pic x.
           05  ext-bus-type            pic xx.
           05  ext-comm-type           pic x.
           05  ext-last-name           pic x(15).
           05  ext-first-name          pic x(15).
           05  ext-init                pic x.
           05  ext-lf-type             pic xx.
           05  ext-lf-term             pic 999.
           05  ext-lf-amt-n            pic -9(9).99.
           05  ext-lf-amt redefines
               ext-lf-amt-n            pic x(13).
           05  ext-lf-prm-n            pic -9(7).99.
           05  ext-lf-prm redefines
               ext-lf-prm-n            pic x(11).
           05  ext-lf-com-n            pic -9(7).99.
           05  ext-lf-com redefines
               ext-lf-com-n            pic x(11).
           05  ext-lf-amt-alt-n        pic -9(9).99.
           05  ext-lf-amt-alt redefines
               ext-lf-amt-alt-n        pic x(13).
           05  ext-lf-prm-alt-n        pic -9(7).99.
           05  ext-lf-prm-alt redefines
               ext-lf-prm-alt-n        pic x(11).
           05  ext-lf-com-alt-n        pic -9(7).99.
           05  ext-lf-com-alt redefines
               ext-lf-com-alt-n        pic x(11).
           05  ext-ah-type             pic xx.
           05  ext-ah-term             pic 999.
           05  ext-ah-amt-n            pic -9(9).99.
           05  ext-ah-amt redefines
               ext-ah-amt-n            pic x(13).
           05  ext-ah-prm-n            pic -9(7).99.
           05  ext-ah-prm redefines
               ext-ah-prm-n            pic x(11).
           05  ext-ah-com-n            pic -9(7).99.
           05  ext-ah-com redefines
               ext-ah-com-n            pic x(11).


                                       copy ECSCOM01.

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

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

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
                 (END-OF-INPUT)

           PERFORM 1050-FINISH-UP      THRU 1050-EXIT
           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           GOBACK
           .
       0002-EXIT.
           EXIT.

       0010-INITIALIZE.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           move spaces to file-out-rec

           string
              'MOE_DATE;'
              'CARRIER;'
              'GROUPING;'
              'ACCOUNT;'
              'REMIT;'
              'STATE;'
              'EFF_DATE;'
              'CERT_NO;'
              'TRANS;'
              'BUS_TYPE;'
              'COMM_TYPE;'
              'LAST_NAME;'
              'FIRST_NAME;'
              'MID_INIT;'
              'LF_TYPE;'
              'LF_TERM;'
              'LF_AMT;'
              'LF_PREM;'
              'LF_COMM;'
              'LF_AMT_ALT;'
              'LF_PREM_ALT;'
              'LF_COMM_ALT;'
              'AH_TYPE;'
              'AH_TERM;'
              'AH_AMT;'
              'AH_PREM;'
              'AH_COMM;EOR'
              delimited by size into file-out-rec
           end-string

           write file-out-rec
           add 1 to output-cnt

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT comm-tran-in eracct 
           if eracct-file-status <> '00'
              display 'error-eracct-open ' eracct-file-status
              perform abend-pgm
           end-if

           OPEN OUTPUT FILE-OUT

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' INPUT RECORDS  ' INPUT-CNT
           DISPLAY ' OUTPUT RECORDS ' OUTPUT-CNT
           CLOSE COMM-TRAN-IN eracct FILE-OUT
           if eracct-file-status <> '00'
              display 'error-eracct-open ' eracct-file-status
              perform abend-pgm
           end-if

           .
       0030-EXIT.
           EXIT.

       0060-READ-INPUT.

           READ COMM-TRAN-IN into cp-record AT END
               SET END-OF-INPUT TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1 TO INPUT-CNT
           END-IF

           .
       0060-EXIT.
           EXIT.

       0080-PROCESS-CERT.

           PERFORM 0090-BUILD-EXTRACT  THRU 0090-EXIT
           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.

           if cp-trans = 'I' OR 'C' OR '8' OR '7'
              continue
           else
              go to 0090-exit
           end-if

           move dte-clasic-company-cd  to am-control-primary
           move cp-carrier             to am-carrier
           move cp-grouping            to am-grouping
           move cp-state               to am-state
           move cp-account             to am-account
           move cp-eff                 to am-expire-dt
           start eracct key > am-control-primary
           if eracct-file-status <> '00'
              display ' error-eracct-start ' eracct-file-status ' '
                 cp-state ' ' cp-account ' ' cp-eff
                 perform abend-pgm
              go to 0090-exit
           end-if
           read eracct next record
           if eracct-file-status <> '00' and '10' and '23'
              display ' error-eracct-read  ' eracct-file-status ' '
                 cp-state ' ' cp-account ' ' cp-eff
                 perform abend-pgm
              go to 0090-exit
           end-if
           if cp-carrier = am-carrier
              and cp-state = am-state
              and cp-account = am-account
              and cp-eff < am-expire-dt
              and cp-eff >= am-effect-dt
              continue
           else
              display ' error-eracct-notfnd  ' eracct-file-status ' '
                 cp-state ' ' cp-account ' ' cp-eff
              go to 0090-exit
           end-if

           if am-report-code-3 = 'LDS'
              continue
           else
              go to 0090-exit
           end-if
              
           MOVE SPACES                 TO EXTRACT-RECORD
           MOVE run-date               TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING
              WS-MM '/'
              WS-DD '/'
              WS-CCYY
              DELIMITED BY SIZE INTO EXT-moe-date
           END-STRING

           MOVE CP-CARRIER             TO EXT-CARRIER
           MOVE CP-GROUPING            TO EXT-GROUP
           MOVE CP-ACCOUNT             TO EXT-ACCOUNT
           MOVE CP-REMIT               TO EXT-REMIT
           MOVE CP-STATE               TO EXT-STATE
           MOVE CP-CERT                TO EXT-CERT-NO
           move cp-trans               to ext-trans

           MOVE CP-EFF                 TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING
              WS-MM '/'
              WS-DD '/'
              WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-EFF-DT
           END-STRING

           move cp-gpcd                to ext-bus-type
           move cp-com-type            to ext-comm-type
           move cp-lname               to ext-last-name
           move cp-fname               to ext-first-name
           move cp-initial             to ext-init
           move cp-lf-type             to ext-lf-type
           move cp-lf-term             to ext-lf-term
           move cp-lf-amt              to ext-lf-amt-n
           move cp-lf-prm              to ext-lf-prm-n
           move cp-lf-com              to ext-lf-com-n
           move cp-lf-amt-alt          to ext-lf-amt-alt-n
           move cp-lf-prm-alt          to ext-lf-prm-alt-n
           move cp-lf-com-alt          to ext-lf-com-alt-n
           move cp-ah-type             to ext-ah-type
           move cp-ah-term             to ext-ah-term
           move cp-ah-amt              to ext-ah-amt-n
           move cp-ah-prm              to ext-ah-prm-n
           move cp-ah-com              to ext-ah-com-n
           
           PERFORM 0100-WRITE-EXTRACT  THRU 0100-EXIT

           .
       0090-EXIT.
           EXIT.

       0100-WRITE-EXTRACT.

           PERFORM 1040-INSERT-ROW     THRU 1040-EXIT

           .
       0100-EXIT.
           EXIT.

       1040-INSERT-ROW.

           move spaces to file-out-rec
           string
              EXT-MOE-DATE    ';'
              EXT-CARRIER     ';'
              EXT-GROUP       ';'
              EXT-ACCOUNT     ';'
              ext-remit       ';'
              EXT-STATE       ';'
              EXT-EFF-DT      ';'
              EXT-CERT-NO     ';'
              ext-trans       ';'
              ext-bus-type    ';'
              ext-comm-type   ';'
              ext-last-name   ';'
              ext-first-name  ';'
              ext-init        ';'
              ext-lf-type     ';'
              ext-lf-term     ';'
              ext-lf-amt      ';'
              ext-lf-prm      ';'
              ext-lf-com      ';'
              ext-lf-amt-alt  ';'
              ext-lf-prm-alt  ';'
              ext-lf-com-alt  ';'
              ext-ah-type     ';'
              ext-ah-term     ';'
              ext-ah-amt      ';'
              ext-ah-prm      ';'
              ext-ah-com      ';E'
              delimited by size into file-out-rec
           end-string

           write file-out-rec

           add 1 to output-cnt

           .
       1040-EXIT.
           EXIT.

       1050-finish-up.

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
