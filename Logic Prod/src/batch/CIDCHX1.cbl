      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      *****************************************************************
      *                                                               *
      * Copyright (c) 2022 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCHX1.
       AUTHOR.   Pablo.
      
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 081122                   PEMA  New program
      ******************************************************************
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      
           SELECT ERCHEK           ASSIGN TO ERCHEK
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CH-CONTROL-primary
                                   FILE STATUS IS ERCHEK-FILE-STATUS.
      
           SELECT FILE-OUT             ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.
      
           SELECT DISK-DATE            ASSIGN TO SYS019.
      
       DATA DIVISION.
       FILE SECTION.
      
       FD  ERCHEK.
      
                                       copy ERCCHEK.
      
       FD  FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FILE-OUT-REC                PIC X(700).
      
       FD  DISK-DATE
                                       COPY ELCDTEFD.
      
       working-storage section.
      
       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
      
       77  s1                          pic s999 comp-3 value +0.
       77  rec-cnt                     pic 9(9) value zeros.
       77  ws-recs-in                  pic 9(7) value zeros.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  erchek-file-status          pic xx value low-values.
       77  eof-sw                      pic x value ' '.
           88  end-of-input               value 'Y'.
      
       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC
      
       01  sqlcmd                      pic x(1024).
       01  WS-MOE-DATE                 pic x(10).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
      
       01  ws-paid-bank-work-area.
           05  ws-pb-compid            pic x(9).
           05  ws-pb-check-no          pic x(10).
           05  ws-pb-bad-check-no      pic x(10).
           05  ws-check-amount         pic x(10).
      
       01  Paid-Bank-Info.
           05  pb-check-no             pic 9(10).
           05  pb-tran-type            pic x.
           05  pb-bank-acct-desc       pic x(50).
           05  pb-amount               pic x(12).
           05  pb-paid-date            pic x(25).
      
       EXEC SQL
          END DECLARE SECTION
       END-EXEC
      
       01  ERCHEK-TABLE-RECORD.
           05  TB-TPA-ID               pic 9.
           05  TB-CARRIER              PIC X.               
           05  TB-GROUPING             PIC X(6).            
           05  TB-STATE                PIC XX.              
           05  TB-ACCOUNT              PIC X(10).           
           05  TB-CERT-EFF-DT          PIC X(10).
           05  TB-CERT-NO              PIC X(10).
           05  TB-CERT-SUFFIX          PIC X.
           05  TB-SEQUENCE-NO          PIC 9(7).
           05  TB-RECORDED-DT          PIC X(10).
           05  TB-RECORDED-BY          PIC X(4).
           05  TB-AMOUNT-PAID          PIC 9(7).99.
           05  TB-AMOUNT-PAID-A REDEFINES
               TB-AMOUNT-PAID          PIC X(10).
           05  TB-CHECK-NO             PIC X(7).
           05  TB-CHECK-WRITTEN-DT     PIC X(10).
           05  TB-CHECK-CASHED-DT      PIC X(10).
           05  TB-PAYEE-NAME-1         PIC X(30).           
           05  TB-PAYEE-NAME-2         PIC X(30).           
           05  TB-PAYEE-ADDRESS-1      PIC X(30).           
           05  TB-PAYEE-ADDRESS-2      PIC X(30).           
           05  TB-PAYEE-CITY           PIC X(28).
           05  TB-PAYEE-STATE          PIC XX.
           05  TB-PAYEE-ZIP-CODE       PIC X(9).
           05  TB-STUB-LINE-1          PIC X(30).           
           05  TB-TEXT-LINE-1          PIC X(50).           
           05  TB-TEXT-LINE-2          PIC X(50).           
           05  TB-TEXT-LINE-3          PIC X(40).           
           05  TB-RETURN-TO            PIC X(30).           
           05  TB-COMP-CARRIER         PIC X.               
           05  TB-COMP-GROUPING        PIC X(6).            
           05  TB-COMP-FIN-RESP        PIC X(10).           
           05  TB-COMP-ACCOUNT         PIC X(10).           
           05  TB-CREDIT-SELECT-DT     PIC X(10).
           05  TB-CREDIT-ACCEPT-DT     PIC X(10).
           05  TB-PAYEE-CODE           PIC X(6).
           05  TB-VOID-DT              PIC X(10).
           05  TB-VOID-BY              PIC X(4).            
           05  TB-VOID-REASON          PIC X(25).           
           05  TB-APPROVAL-DT          PIC X(10).
           05  TB-APPROVAL-STATUS      PIC X.
           05  TB-APPROVED-BY          PIC XXXX.
           05  TB-CANC-DT              PIC X(10).
           05  TB-LF-REFUND            PIC 9(7).99.
           05  TB-LF-REFUND-A REDEFINES
               TB-LF-REFUND            PIC X(10).
           05  TB-AH-REFUND            PIC 9(7).99.
           05  TB-AH-REFUND-A REDEFINES
               TB-AH-REFUND            PIC X(10).
           05  TB-INSURED-NAME         PIC X(28).           
           05  TB-CSR                  PIC X(4).
           05  TB-DEDUCT-COMMISSION    PIC X.
      
       01  FILLER.
           05  WS-CHECK-AMT-TMP        PIC Z(7).99.
           05  WS-CHECK-AMT-TMPX REDEFINES             
               WS-CHECK-AMT-TMP        PIC X(10).  
           05  ABEND-CODE              PIC X(4)  VALUE SPACES.
           05  ABEND-OPTION            PIC X     VALUE 'Y'.
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-ABEND-FILE-STATUS    PIC XX    VALUE ZERO.
           05  WS-RETURN-CODE          PIC S9(3) VALUE ZERO COMP-3.
           05  PGM-SUB                 PIC S999  VALUE +344 COMP-3.
      
                                       COPY ELCDTECX.
                                       copy ELCDTEVR.
                                       COPY ELCDATE.
      
       procedure division.
                                       COPY ELCDTERX.
       0000-begin.
      
      *    display ' Begin Program '
      
           perform 0010-init           thru 0010-exit
           perform 2010-connect-to-paidbank
                                       thru 2010-exit
      
           perform 0020-open-files     thru 0020-exit

           move spaces to file-out-rec
           string
              'CARRIER;'
              'GROUPING;'
              'STATE;'
              'ACCOUNT;'
              'CERT_EFF_DT;'
              'CERT_NO;'
              'CERT_SUFFIX;'
              'SEQUENCE_NO;'
              'RECORDED_DT;'
              'RECORDED_BY;'
              'AMOUNT_PAID;'
              'CHECK_NO;'
              'CHECK_WRITTEN_DT;'
              'CHECK_CASHED_DT;'
              'PAYEE_NAME_1;'
              'PAYEE_NAME_2;'
              'PAYEE_ADDRESS_1;'
              'PAYEE_ADDRESS_2;'
              'PAYEE_CITY;'
              'PAYEE_STATE;'
              'PAYEE_ZIP_CODE;'
              'STUB_LINE_1;'
              'TEXT_LINE_1;'
              'TEXT_LINE_2;'
              'TEXT_LINE_3;'
              'RETURN_TO;'
              'COMP_CARRIER;'
              'COMP_GROUPING;'
              'COMP_FIN_RESP;'
              'COMP_ACCOUNT;'
              'CREDIT_SELECT_DT;'
              'CREDIT_ACCEPT_DT;'
              'PAYEE_CODE;'
              'VOID_DT;'
              'VOID_BY;'
              'VOID_REASON;'
              'APPROVAL_DT;'
              'APPROVAL_STATUS;'
              'APPROVED_BY;'
              'CANC_DT;'
              'LF_REFUND;'
              'AH_REFUND;'
              'INSURED_NAME;'
              'CSR;'
              'DEDUCT_COMMISSION;'
              'EOR'
              delimited by size into file-out-rec
           end-string

           write file-out-rec
      
           perform 0030-start-input    thru 0030-exit
           if not end-of-input
              perform 0046-read-input  thru 0046-exit
           end-if
           perform 0045-process-input  thru 0045-exit until
              end-of-input
      *      or ws-recs-in > 10000
      
           perform 0060-finish-up      thru 0060-exit
           close ERCHEK file-out
           display ' End Program '
           display ' extracts written ' rec-cnt
           display ' records read  ' ws-recs-in
           goback
      
           .
       0010-init.
      
           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE    ' '                 TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           move dc-greg-date-a-edit    to ws-moe-date
      
           evaluate dte-client
              when 'AHL'
                 move 'AHL - AP%'      to ws-pb-compid
              when 'FNL'
                 move 'FNL - AP%'      to ws-pb-compid
              when other
                 move 'CSO - AP%'      to ws-pb-compid
           end-evaluate
      
           .
       0010-exit.
           exit.
      
       0020-open-files.
      
           open input ERCHEK
              output file-out
      
           if erchek-file-status not = '00'
              display ' error-erchek-open ' erchek-file-status
              perform abend-pgm
           end-if
      
           .
       0020-exit.
           exit.
      
       0030-start-input.
      
           move dte-clasic-company-cd  to ch-control-primary
           start erchek key >= ch-control-primary
           if erchek-file-status = '10' or '23'
              set end-of-input to true
           else
              if erchek-file-status not = '00'
                 display ' error-erchek-start ' erchek-file-status
                 perform abend-pgm
              end-if
           end-if
      
           .
       0030-exit.
           exit.
      
       0045-process-input.
      
           perform 0050-insert-row     thru 0050-exit
           perform 0046-read-input     thru 0046-exit
      
           .
       0045-exit.
           exit.
      
       0046-read-input.
      
           read erchek next record
           
           evaluate true
              when (erchek-file-status = '10' or '23')
                 or (ch-company-cd not = dte-clasic-company-cd)
                 set end-of-input      to true
              when erchek-file-status not = '00'
                 display ' error-erchek-read ' erchek-file-status
                 perform abend-pgm
              when other
                 add 1 to ws-recs-in
           end-evaluate
      
           .
       0046-exit.
           exit.
      
       0050-insert-row.
      
           perform 0052-build-values   thru 0052-exit
           perform 0055-get-cashed-dt  thru 0055-exit
           perform 0057-insert-row     thru 0057-exit
      
           .
       0050-exit.
           exit.
      
       0052-build-values.
      
           if dte-client = 'AHL'
              move 1                   to tb-tpa-id
           else
              if dte-client = 'FNL'
                 move 2                to tb-tpa-id
              end-if
           end-if
           move ch-carrier             to tb-carrier
           move ch-grouping            to tb-grouping
           move ch-state               to tb-state
           move ch-account             to tb-account
           move ch-cert-prime          to tb-cert-no
           move ch-cert-sfx            to tb-cert-suffix           
      
           move ch-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to tb-cert-eff-dt
           end-if
      
           move ch-sequence-no         to tb-sequence-no
           
           move ch-recorded-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to tb-recorded-dt
      *       move +0                  to nu-recorded-dt
           else
              move spaces              to tb-recorded-dt
      *       move -1                  to nu-recorded-dt
           end-if
      
           move ch-recorded-by         to tb-recorded-by
           move ch-amount-paid         to tb-amount-paid
           move ch-check-no            to tb-check-no
      
           move ch-check-written-dt    to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to tb-check-written-dt
      *       move +0                  to nu-check-written-dt
           else
              move spaces              to tb-check-written-dt
      *       move -1                  to nu-check-written-dt
           end-if
      
           move ch-payee-name-1        to tb-payee-name-1
           move ch-payee-name-2        to tb-payee-name-2
           move ch-payee-address-1     to tb-payee-address-1
           move ch-payee-address-2     to tb-payee-address-2
           move ch-payee-city          to tb-payee-city
           move ch-payee-state         to tb-payee-state
           move ch-payee-zip-code      to tb-payee-zip-code
      
           move ch-STUB-LINE-1         to TB-STUB-LINE-1      
           move ch-TEXT-LINE-1         to TB-TEXT-LINE-1      
           move ch-TEXT-LINE-2         to TB-TEXT-LINE-2      
           move ch-TEXT-LINE-3         to TB-TEXT-LINE-3      
           move ch-RETURN-TO           to TB-RETURN-TO        
           move ch-COMP-CARRIER        to TB-COMP-CARRIER     
           move ch-COMP-GROUPING       to TB-COMP-GROUPING    
           move ch-COMP-FIN-RESP       to TB-COMP-FIN-RESP    
           move ch-COMP-ACCOUNT        to TB-COMP-ACCOUNT     
      
           move ch-credit-select-dt    to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to tb-credit-select-dt
      *       move +0                  to nu-credit-select-dt
           else
              move spaces              to tb-credit-select-dt
      *       move -1                  to nu-credit-select-dt
           end-if
      
           move ch-credit-accept-dt    to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to tb-credit-accept-dt
      *       move +0                  to nu-credit-accept-dt
           else
              move spaces              to tb-credit-accept-dt
      *       move -1                  to nu-credit-accept-dt
           end-if
      
           move ch-PAYEE-CODE          to TB-PAYEE-CODE       
           move ch-void-dt             to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to tb-void-dt
      *       move +0                  to nu-void-dt
           else
              move spaces              to tb-void-dt
      *       move -1                  to nu-void-dt
           end-if
      
           move ch-approval-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to tb-approval-dt
      *       move +0                  to nu-approval-dt
           else
              move spaces              to tb-approval-dt
      *       move -1                  to nu-approval-dt
           end-if
      
           move ch-canc-dt             to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to tb-canc-dt
      *       move +0                  to nu-canc-dt
           else
              move spaces              to tb-canc-dt
      *       move -1                  to nu-canc-dt
           end-if
      
           move ch-VOID-BY             to TB-VOID-BY          
           move ch-VOID-REASON         to TB-VOID-REASON      
           move ch-APPROVAL-STATUS     to TB-APPROVAL-STATUS  
           move ch-APPROVED-BY         to TB-APPROVED-BY      
      
           move ch-LF-REFUND           to TB-LF-REFUND        
           move ch-AH-REFUND           to TB-AH-REFUND        
           move ch-INSURED-NAME        to TB-INSURED-NAME     
           move ch-CSR                 to TB-CSR              
           move ch-DEDUCT-COMMISSION   to TB-DEDUCT-COMMISSION
      
           .
       0052-exit.
           exit.
      
       0055-get-cashed-dt.
      
           move spaces                 to tb-check-cashed-dt
      *    move -1                     to nu-check-cashed-dt
           move '000'                  to ws-pb-check-no (1:3)
           move ch-check-no            to ws-pb-check-no (4:7)
      
           move spaces                 to ws-pb-bad-check-no
           perform varying s1 from +1 by +1 until
              (s1 > +10)
              or (ws-pb-check-no (s1:1) not = '0')
           end-perform
      
           if s1 < +11
              move ws-pb-check-no (s1:11 - s1)
                                  to ws-pb-bad-check-no (1: 11 - s1)
           end-if
      
           MOVE ch-AMOUNT-PAID      TO WS-CHECK-AMT-TMP
           MOVE WS-CHECK-AMT-TMPX   TO WS-CHECK-AMOUNT
           MOVE SPACES              TO pb-paid-date
      
           EXEC SQL
              CALL pbi_GetCashDate_by_TransactionNbr
                 @compid          = :ws-pb-compid,
                 @checkno         = :ws-pb-check-no,
                 @badcheckno      = :ws-pb-bad-check-no,
                 @checkamount     = :ws-check-amount,
                 @checkcasheddate = :pb-paid-date OUT
           END-EXEC
      
           if sqlcode not = 0
              display "Error: cannot find cashed dt " ws-pb-check-no
                 ' ' ws-check-amount
              move sqlcode            to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              go to 0055-exit
           end-if
      
      *    display ' chkno  ' pb-check-no
      *    display ' desc   ' pb-bank-acct-desc
      *    display ' trn typ' pb-tran-type
      *    display ' amt   *' pb-amount '**'
      *    display ' pd dte ' pb-paid-date
      
           string pb-paid-date (1:4)
                  pb-paid-date (6:2)
                  pb-paid-date (9:2)
              delimited by size into dc-greg-date-cymd-r
           end-string
           move 'L'                    to dc-option-code
           perform 8500-date-convert   thru 8590-exit
           if no-conversion-error
      *       move dc-bin-date-1       to ws-bin-cashed-dt
              move dc-greg-date-a-edit to tb-check-cashed-dt
      *       move +0                  to nu-check-cashed-dt
           else
              display ' error cvtdte cash dt ' pb-paid-date ' '
                 dc-error-code
           end-if
      
           .
       0055-exit.
           exit.
      
       0057-insert-row.
      
           INSPECT erchek-table-record replacing
              ALL X'00' BY SPACES
              ALL ';'   BY ':'
           move spaces to file-out-rec
           string
              TB-CARRIER          ';'
              TB-GROUPING         ';'
              TB-STATE            ';'
              TB-ACCOUNT          ';'
              TB-CERT-EFF-DT      ';'
              TB-CERT-NO          ';'
              TB-CERT-SUFFIX      ';'
              TB-SEQUENCE-NO      ';'
              TB-RECORDED-DT      ';'
              TB-RECORDED-BY      ';'
              TB-AMOUNT-PAID-A    ';'
              TB-CHECK-NO         ';'
              TB-CHECK-WRITTEN-DT ';'
              TB-CHECK-CASHED-DT  ';'
              TB-PAYEE-NAME-1     ';'
              TB-PAYEE-NAME-2     ';'
              TB-PAYEE-ADDRESS-1  ';'
              TB-PAYEE-ADDRESS-2  ';'
              TB-PAYEE-CITY       ';'
              TB-PAYEE-STATE      ';'
              TB-PAYEE-ZIP-CODE   ';'
              TB-STUB-LINE-1      ';'
              TB-TEXT-LINE-1      ';'
              TB-TEXT-LINE-2      ';'
              TB-TEXT-LINE-3      ';'
              TB-RETURN-TO        ';'
              TB-COMP-CARRIER     ';'
              TB-COMP-GROUPING    ';'
              TB-COMP-FIN-RESP    ';'
              TB-COMP-ACCOUNT     ';'
              TB-CREDIT-SELECT-DT ';'
              TB-CREDIT-ACCEPT-DT ';'
              TB-PAYEE-CODE       ';'
              TB-VOID-DT          ';'
              TB-VOID-BY          ';'
              TB-VOID-REASON      ';'
              TB-APPROVAL-DT      ';'
              TB-APPROVAL-STATUS  ';'
              TB-APPROVED-BY      ';'
              TB-CANC-DT          ';'
              TB-LF-REFUND-A      ';'
              TB-AH-REFUND-A      ';'
              TB-INSURED-NAME     ';'
              TB-CSR              ';'
              TB-DEDUCT-COMMISSION ';E'
              delimited by size into file-out-rec
           end-string

           write file-out-rec
      
      
           .
       0057-exit.
           exit.
      
       0060-finish-up.
      
           if sqlcode not = 0
              display "Error: commit release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if
      
           EXEC SQL
              DISCONNECT
           END-EXEC
      
           .
       0060-exit.
           exit.
      
       2010-connect-to-paidbank.
      
           display ' about to connect to paid bank '
      
           move 'appuser'              to usr
           move 'appuser@cso'          to pass
           move 'SDVDB01_PdBnkInfo'    to svr
      
           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string
      
           EXEC SQL
              CONNECT TO :svr
                    USER :usr-pass
           END-EXEC
      
           if sqlcode not = 0
              display "Error: cannot connect to PaidBank"
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              goback
           end-if
      
           .
       2010-exit.
           exit.
      
       8500-DATE-CONVERT.              
                                       COPY ELCDCS.
      
       abend-pgm.
      
            call 'ABORTME'.
            
            goback.
