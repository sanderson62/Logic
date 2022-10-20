       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCSX1.
       AUTHOR.   CSO.
      *REMARKS.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2022 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
081022* 081022    2019012500003  PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      
           SELECT FILE-OUT         ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ELCRTT           ASSIGN TO ELCRTT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CS-CONTROL-primary
                                   FILE STATUS IS ELCRTT-FILE-STATUS.
      
           SELECT DISK-DATE        ASSIGN TO SYS019.
      
       DATA DIVISION.
       FILE SECTION.
      
       FD  FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FILE-OUT-REC                PIC X(228).

       FD  ELCRTT.
      
                                       copy ELCCRTT.
      
       FD  DISK-DATE
                                       COPY ELCDTEFD.
      
       working-storage section.
      
       77  s1                          pic s999 comp-3 value +0.
       77  rec-cnt                     pic 9(9) value zeros.
       77  ws-recs-in                  pic 9(7) value zeros.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  elcrtt-file-status          pic xx value low-values.
       77  eof-sw                      pic x value ' '.
           88  end-of-input               value 'Y'.
       77  ws-records-inserted         pic 9(9) value zeros.
       77  ws-sql-date-time            pic x(24) value spaces.
      
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
      
       01  filler.
           05  ws-work-date.
               10  ws-work-ccyy        pic 9999.
               10  ws-work-mm          pic 99.
               10  ws-work-dd          pic 99.
           05  ws-work-date-moyr       pic 9999  comp value zeros.

       01  ELCRTT-TABLE-RECORD.
           05  TB-CARRIER              PIC X.
           05  TB-GROUPING             PIC X(6).
           05  TB-STATE                PIC XX.
           05  TB-ACCOUNT              PIC X(10).
           05  TB-CERT-EFF-DT          PIC X(10).
           05  TB-CERT-NO              PIC X(11).
           05  TB-VEHICLE-ID-NO        PIC X(20).
           05  tb-year                 pic 9999.
           05  tb-make                 pic x(20).
           05  tb-model                pic x(20).
           05  tb-series               pic x(20).
           05  tb-odometer             pic 9(7).
           05  TB-AGENT-NAME           PIC X(50).
           05  TB-LICENSE-NO           PIC X(15).
           05  TB-NATIONAL-PRODUCER-NO PIC X(10).
           05  TB-AGT-SIG-VER-STATUS   PIC X.
           05  tb-lf-ref-method        pic x.
           05  tb-ah-ref-method        pic x.
      
       01  FILLER.
           05  ABEND-CODE              PIC X(4)  VALUE SPACES.
           05  ABEND-OPTION            PIC X     VALUE 'Y'.
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-ABEND-FILE-STATUS    PIC XX    VALUE ZERO.
           05  WS-RETURN-CODE          PIC S9(3) VALUE ZERO COMP-3.
           05  PGM-SUB                 PIC S999  VALUE +344 COMP-3.
      
                                       COPY ELCDTECX.
                                       copy ELCDTEVR.
                                       copy ELCFUNDT.
                                       COPY ELCDATE.
      
       procedure division.
                                       COPY ELCDTERX.
       0000-begin.

           display ' Begin Program CIDCSX1  '

           perform 0000-open-files     thru 0000-exit
           perform 0010-init           thru 0010-exit
      
           perform 0045-process-input  thru 0045-exit until
              end-of-input
      *      or ws-recs-in > 10000
      
           perform 0060-finish-up      thru 0060-exit
           goback
      
           .
       0000-open-files.
      
           open input ELCRTT
           OPEN OUTPUT FILE-OUT

           if ELCRTT-file-status not = '00'
              display ' error-ELCRTT-open ' ELCRTT-file-status
              perform abend-pgm
           end-if
      
           .
       0000-exit.
           exit.

       0010-init.

           move FUNCTION CURRENT-DATE  TO FUNCTION-DATE
      *    display ' function date = ' function-date

           string
              ws-fn-ccyr '-'
              ws-fn-mo   '-'
              ws-fn-da   '  '
              ws-fn-hours ':'
              ws-fn-minutes ':'
              ws-fn-seconds '.000'
                 delimited by size into ws-sql-date-time
           end-string

      *    display ' sql date time ' ws-sql-date-time

           perform 0030-start-input    thru 0030-exit
           perform 0046-read-input     thru 0046-exit

           .
       0010-exit.
           exit.
      
       0030-start-input.

           move low-values             to cs-control-primary
           move dte-clasic-company-cd  to cs-company-cd
           start ELCRTT key >= cs-control-primary
           if ELCRTT-file-status = '10' or '23'
              set end-of-input to true
           else
              if ELCRTT-file-status not = '00'
                 display ' error-ELCRTT-start ' ELCRTT-file-status
                 perform abend-pgm
              end-if
           end-if
      
           .
       0030-exit.
           exit.
      
       0045-process-input.
      
           if cs-trailer-type = 'C'
              perform 0050-build-row   thru 0050-exit
           end-if

           perform 0046-read-input     thru 0046-exit
      
           .
       0045-exit.
           exit.
      
       0046-read-input.
      
           read ELCRTT next record
           if (elcrtt-file-status = '10' or '23')
              or (dte-clasic-company-cd not = cs-company-cd)
              set end-of-input to true
           else
              if elcrtt-file-status not = '00'
                 display ' error-ELCRTT-read ' ELCRTT-file-status
                 perform abend-pgm
              end-if
           end-if

           if not end-of-input
              add +1 to ws-recs-in
           end-if

           .
       0046-exit.
           exit.
      
       0050-build-row.
      

           perform 0052-build-values   thru 0052-exit
           if tb-vehicle-id-no = spaces
              and tb-agent-name = spaces
              and tb-license-no = spaces
              and tb-national-producer-no = spaces
              and tb-agt-sig-ver-status = spaces
092118        and tb-lf-ref-method = spaces
092118        and tb-ah-ref-method = spaces
              continue
           else
              perform 0057-build-extr  thru 0057-exit
           end-if
      
           .
       0050-exit.
           exit.
      
       0052-build-values.

           move spaces                 to elcrtt-table-record
           move cs-carrier             to tb-carrier
           move cs-grouping            to tb-grouping
           move cs-state               to tb-state
           move cs-account             to tb-account

           move cs-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-b-edit to tb-cert-eff-dt
           end-if

           move cs-cert-no             to tb-cert-no
           move cs-vin-number          to tb-vehicle-id-no
           if cs-year not numeric
              move zeros               to cs-year
           end-if
           if cs-vehicle-odometer not numeric
              move zeros               to cs-vehicle-odometer
           end-if
           move cs-year                to tb-year
           move cs-make                to tb-make
           move cs-model               to tb-model
           move cs-future              to tb-series
           move cs-vehicle-odometer    to tb-odometer

           if cs-agent-edit-status = 'U' or 'N' or 'R'
              move cs-agent-name       to tb-agent-name
           else
              string
                 cs-agent-fname ' '
                 cs-agent-mi    ' '
                 cs-agent-lname ' '
                 delimited by '  ' into tb-agent-name
              end-string
           end-if

           move cs-license-no          to tb-license-no
           move cs-npn-number          to tb-national-producer-no
           move cs-agent-edit-status   to tb-agt-sig-ver-status
092118     move cs-lf-refund-method    to tb-lf-ref-method
092118     move cs-ah-refund-method    to tb-ah-ref-method

           .
       0052-exit.
           exit.
      
       0057-build-extr.

           string
              TB-CARRIER              ';'
              TB-GROUPING             ';'
              TB-STATE                ';'
              TB-ACCOUNT              ';'
              TB-CERT-EFF-DT          ';'
              TB-CERT-NO              ';'
              TB-VEHICLE-ID-NO        ';'
              TB-YEAR                 ';'
              TB-MAKE                 ';'
              TB-MODEL                ';'
              TB-SERIES               ';'
              TB-ODOMETER             ';'
              TB-AGENT-NAME           ';'
              TB-LICENSE-NO           ';'
              TB-NATIONAL-PRODUCER-NO ';'
              TB-AGT-SIG-VER-STATUS   ';'
              tb-lf-ref-method        ';'
              tb-ah-ref-method        ';'
              'E'
              delimited by size into file-out-rec
           end-string

           write file-out-rec              

           add 1 to ws-records-inserted
           add 1 to rec-cnt
      
           .
       0057-exit.
           exit.
      
       0060-finish-up.
      
           close ELCRTT file-out
           display ' End Program '
           display ' rows inserted ' rec-cnt
           display ' records read  ' ws-recs-in
      
           .
       0060-exit.
           exit.

       8500-DATE-CONVERT.              
                                       COPY ELCDCS.
      
       abend-pgm.
      
            call 'ABORTME'.
            
            goback.
