      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      *****************************************************************
      *                                                               *
      * Copyright (c) 2014 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLDCCCRXCUP.
       AUTHOR.   Cowtown.

      *REMARKS.                                                         
      *  This program runs monthly after the dcc_certall and dcc_certall_dw
      *  are built. Here is what happens.
      *   1. Queries certall and cerall_dw (last month) and looks for any
      *      cancelled cert that matches the current month but the cancel
      *      amounts are different. Later in the program, it inserts those
      *      differences into dcc_cu as adjustments.
      *   2. queries the dcc_cu table to check for existing rows with the same
      *      run date, if there are any it assumes re-run and deletes them all
      *   3. Reads the dcc logic cert file and pulls off current month 
      *      activity and inserts that into dcc_cu table.
      *   4. Gets the adjustments from step 1 and appends them to dcc_cu.
      ******************************************************************

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 011018                   PEMA  New Program
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILE-IN          ASSIGN TO SYS010.

           SELECT ERACCTT          ASSIGN TO ERACCTT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-primary
                                   FILE STATUS IS ERACCTT-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  FILE-IN
           RECORDING MODE F.

                                       copy ECSCRT01.

       FD  ERACCTT.

                                       copy ERCACCT.

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
       77  ws-ref-factor               pic s99v9(5) comp-3 value +0.
       77  eracctt-file-status         pic xx value '00'.
       77  ws-acct-sw                  pic x value ' '.
           88  acct-found                    value 'Y'.
       77  eof-sw                      pic x value ' '.
           88  end-of-input               value 'Y'.
       77  a1                          pic s999 value +0 comp-3.
       77  ws-adj-sw                   pic x  value ' '.
           88  special-adjustment        value 'Y'.
021714 01  P pointer.
021714 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
021714 01  var-ptr pointer.
021714 01  env-var-len                 pic 9(4)  binary.
021714 01  rc                          pic 9(9)  binary.
021714
021714 01  WS-KIXSYS.
021714     05  WS-KIX-FIL1             PIC X(10).
021714     05  WS-KIX-APPS             PIC X(10).
021714     05  WS-KIX-ENV              PIC X(10).
021714     05  WS-KIX-MYENV            PIC X(10).
021714     05  WS-KIX-SYS              PIC X(10).

       01  filler.
           05  ws-work-date.
               10  ws-work-ccyy        pic 9999.
               10  ws-work-mm          pic 99.
               10  ws-work-dd          pic 99.
           05  ws-work-date-moyr       pic 9999  comp value zeros.

       01  filler.
           05  ws-retention            pic s9(9)v9(5) comp-3 value +0.
           05  ws-wholesale-fee        pic s9(9)v9(5) comp-3 value +0.
           05  ws-acct-fee             pic s9(9)v9(5) comp-3 value +0.
           05  ws-cso-admin-fee        pic s9(9)v9(5) comp-3 value +0.
           05  ws-ccc-admin-fee        pic s9(9)v9(5) comp-3 value +0.
           05  ws-total-fees           pic s9(9)v9(5) comp-3 value +0.
           05  ws-ga-fee               pic s9(9)v9(5) comp-3 value +0.
           05  ws-clp-prem             pic s9(9)v9(5) comp-3 value +0.

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
       01  ws-last-month-dt            pic x(10).
       01  ws-carr-7                   pic x.
       01  ws-carr-9                   pic x.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to tell sql server that i am    ***
      ***  passing it a null value.  The indicator will be -1        ***
      ***  if the value is nulls and +0 if the value is other than   ***
      ***  nulls.  Here is a sample on how to use it.                ***
      ***                                                            ***
      ***      if db-date1 = spaces move -1 to nu-date1 end-if       *** 
      ***     EXEC SQL                                               ***
      ***        insert into ERMEBL (                                ***
      ***           date1,                                           ***
      ***           date2)                                           ***
      ***        values (                                            ***
      ***           :db-date1      :nu-date1,                        ***
      ***           :db-date2      :nu-date2)                        ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  NU-RECORDED-DT          PIC s9(4) comp value +0.
           05  NU-CHECK-WRITTEN-DT     PIC s9(4) comp value +0.
           05  nu-check-cashed-dt      pic s9(4) comp value +0.
           05  NU-CREDIT-SELECT-DT     PIC s9(4) comp value +0.
           05  NU-CREDIT-ACCEPT-DT     PIC s9(4) comp value +0.
           05  NU-VOID-DT              PIC s9(4) comp value +0.
           05  NU-APPROVAL-DT          PIC s9(4) comp value +0.
           05  NU-CANC-DT              PIC s9(4) comp value +0.
       01  ws-rec-cntr                 pic s9(4) comp value +0.
       01  ws-test-date                pic x(10) value spaces.
       01  ar1                         pic s999 comp-3 value +0.
       01  ar1-max                     pic s999 comp-3 value +0.

       01  ws-addl-rec.
           05  ar-carrier              pic x.
           05  ar-state                pic xx.
           05  ar-account              pic x(10).
           05  ar-eff-dt               pic x(8).
           05  ar-cert-no              pic x(11).
           05  ar-ah-ben-cd            pic xx.
      *    05  ar-refund               pic x(12).
           05  ar-refund-n
                                       pic s9(9)v99.
      *    05  ar-prev-refund          pic x(12).
           05  ar-prev-refund-n
                                       pic s9(9)v99.

       01  DCC-CREDIT-UNION-TABLE.
           05  TB-MONTH-END-DT         PIC X(10).
           05  TB-CARRIER              PIC X.
           05  TB-GROUPING             PIC X(6).
           05  TB-STATE                PIC XX.
           05  TB-CLP-STATE            PIC XX.
           05  TB-ACCOUNT              PIC X(10).
           05  TB-EFF-DT               PIC X(10).
           05  TB-ADDENDUM-NO          PIC X(11).
           05  TB-RETRO-POOL           PIC X(6).
           05  TB-BENEFIT-CD           PIC XX.
           05  TB-RETAIL-FEE-N         PIC -9(9).99.
           05  TB-RETAIL-FEE REDEFINES
               TB-RETAIL-FEE-N         PIC X(13).
           05  TB-WHOLESALE-FEE-N      PIC -9(9).99.
           05  TB-WHOLESALE-FEE REDEFINES
               TB-WHOLESALE-FEE-N      PIC X(13).
           05  TB-ACCOUNT-FEE-N        PIC -9(9).99.
           05  TB-ACCOUNT-FEE REDEFINES
               TB-ACCOUNT-FEE-N        PIC X(13).
           05  TB-CSO-ADMIN-FEE-N      PIC -9(9).99.
           05  TB-CSO-ADMIN-FEE REDEFINES
               TB-CSO-ADMIN-FEE-N      PIC X(13).
           05  TB-CCC-ADMIN-FEE-N      PIC -9(9).99.
           05  TB-CCC-ADMIN-FEE REDEFINES
               TB-CCC-ADMIN-FEE-N      PIC X(13).
           05  TB-TOTAL-CSO-FEE-N      PIC -9(9).99.
           05  TB-TOTAL-CSO-FEE REDEFINES
               TB-TOTAL-CSO-FEE-N      PIC X(13).
           05  TB-GA-FEE-N             PIC -9(9).99.
           05  TB-GA-FEE REDEFINES
               TB-GA-FEE-N             PIC X(13).
           05  TB-OTHER1-FEE-N         PIC -9(9).99.
           05  TB-OTHER1-FEE REDEFINES
               TB-OTHER1-FEE-N         PIC X(13).
           05  TB-OTHER2-FEE-N         PIC -9(9).99.
           05  TB-OTHER2-FEE REDEFINES
               TB-OTHER2-FEE-N         PIC X(13).
           05  TB-CLP-PREMIUM-N        PIC -9(9).99.
           05  TB-CLP-PREMIUM REDEFINES
               TB-CLP-PREMIUM-N        PIC X(13).
           05  TB-MEMBERS-N            PIC -9(7).
           05  TB-MEMBERS REDEFINES
               TB-MEMBERS-N            PIC X(8).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  filler.
           05  ws-addl-records-table occurs 50.
               10  art-carrier         pic x.
               10  art-state           pic xx.
               10  art-account         pic x(10).
               10  art-eff-dt          pic x(8).
               10  art-eff-dt-n redefines
                   art-eff-dt          pic 9(8).
               10  art-cert-no         pic x(11).
               10  art-ah-ben-cd       pic xx.
               10  art-refund          pic s9(9)v99.
               10  art-prev-refund     pic s9(9)v99.


       01  FILLER.
           05  ABEND-CODE              PIC X(4)  VALUE SPACES.
           05  ABEND-OPTION            PIC X     VALUE 'Y'.
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-ABEND-FILE-STATUS    PIC XX    VALUE ZERO.
           05  WS-RETURN-CODE          PIC S9(3) VALUE ZERO COMP-3.
           05  PGM-SUB                 PIC S999  VALUE +344 COMP-3.

                                       COPY ELCDTECX.
                                       copy ELCDTEVR.
                                       COPY ELCDATE.

       LINKAGE SECTION.                                                 
       01  parm.
           05  parm-length             pic s9(4) comp.
           05  parm-current-month-end  pic 9(8).

       01  var  pic x(30).

       procedure division using parm.
                                       COPY ELCDTERX.
       0000-begin.

           display ' Begin Program '

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

           display ' KIXSYS  ' ws-kix-myenv

           perform 0010-open-files     thru 0010-exit
           perform 0020-init           thru 0020-exit
           perform 2000-connect-to-logic
                                       thru 2000-exit

           perform 3000-go-get-refund-differences
                                       thru 3000-exit
           perform 2100-disconnect-logic
                                       thru 2100-exit

           perform 2500-connect-to-Actuary
                                       thru 2500-exit
           perform 1030-check-for-rerun thru 1030-exit

           if ws-rec-cntr > 0
              display ' This must be a re-run, about to delete '
              perform 1040-delete-rows thru 1040-exit
           end-if

      *    perform 0120-finish-up      thru 0120-exit
      *    goback


pfirst*    perform 1010-drop-table     thru 1010-exit
      *    perform 1020-truncate-table thru 1020-exit
pfirst*    perform 1000-create-table   thru 1000-exit

           EXEC SQL
              SET AUTOCOMMIT OFF
           END-EXEC

           perform 0040-process-input  thru 0040-exit until
              end-of-input
      *      or ws-recs-in > 10000

           perform 0120-finish-up      thru 0120-exit
           close ERACCTT
           display ' End Program '
           display ' rows inserted ' rec-cnt
           display ' records read  ' ws-recs-in
           goback

           .
       0010-open-files.

           open input FILE-IN ERACCTT
           IF ERACCTT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR-ERACCT-OPEN ' ERACCTT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-exit.
           exit.

       0020-init.

           move spaces                 to ws-addl-rec
           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE    ' '                 TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           move dc-greg-date-a-edit    to ws-moe-date
           display ' parm length '     parm-length ' '
              parm-current-month-end

           move parm-current-month-end to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to ws-moe-date
              display ' init month end date ' ws-moe-date
              string
                 dc-greg-date-a-edit (7:4) '.'
                 dc-greg-date-a-edit (1:2) '.'
                 dc-greg-date-a-edit (4:2)
                    delimited by size into ws-test-date
              end-string
              display ' test date ' ws-test-date
           else
              display ' invalid parm date - aborting '
                 parm-current-month-end
              perform abend-pgm
           end-if

           move parm-current-month-end to ws-work-date
           perform 0050-read-input     thru 0050-exit
           perform 0030-start-eracct   thru 0030-exit

           .
       0020-exit.
           exit.

       0030-start-eracct.

           move low-values             to am-control-primary
           start eracctt key >= am-control-primary
           if eracctt-file-status not = '00'
              display ' error-eracctt-start ' eracctt-file-status
              perform abend-pgm
           end-if

           .
       0030-exit.
           exit.

       0035-read-eracctt.

           read eracctt next record
           if eracctt-file-status = '10' or '23'
              move high-values         to am-control-primary
           else
              if eracctt-file-status not = zeros
                 display ' eracct read ' eracctt-file-status
                 perform abend-pgm
              end-if
           end-if

           .
       0035-exit.
           exit.

       0040-process-input.

           if (parm-current-month-end = cr-entry-date or
              cr-ah-cancel-exit-date)
              and (cr-carrier = '7' or '9')
      *       and (cr-entry-status = 'C' or '5')
      *       and (cr-entry-status not = 'M')
              perform 0060-insert-row  thru 0060-exit
           else
              perform varying ar1 from +1 by +1 until ar1 > ar1-max
                 if (cr-carrier = art-carrier (ar1))
                    and (cr-state = art-state (ar1))
                    and (cr-account = art-account (ar1))
                    and (cr-dt = art-eff-dt-n (ar1))
                    and (cr-cert-no = art-cert-no (ar1))
                    and (cr-ahrfnd = art-refund (ar1))
                    display ' we got a hit ' cr-cert-no ' ' cr-ahrfnd
                    compute cr-ahrfnd =
                       cr-ahrfnd - art-prev-refund (ar1)
                    set special-adjustment to true
                    perform 0060-insert-row  thru 0060-exit
                    move +999 to ar1
                    move ' ' to ws-adj-sw
                 end-if
              end-perform
           end-if

           perform 0050-read-input     thru 0050-exit

           .
       0040-exit.
           exit.

       0050-read-input.

           read file-in at end
              set end-of-input to true
           end-read
              
           if not end-of-input
              add 1 to ws-recs-in
           end-if

           .
       0050-exit.
           exit.

       0060-insert-row.

           display ' insert row ' cr-cert-no ' ' cr-entry-status ' '
              cr-ah-current-status
           move ' '                    to ws-acct-sw
           move spaces                 to DCC-CREDIT-UNION-TABLE
           perform 0130-sync-to-acct   thru 0130-exit
           if not acct-found
              go to 0060-exit
           end-if
           perform 0070-build-values   thru 0070-exit
           if dcc-credit-union-table <> spaces
              perform 0110-insert-row     thru 0110-exit
           else
              display ' bypassing spaces ' cr-state ' ' cr-account
                 ' ' cr-cert-no
           end-if

           .
       0060-exit.
           exit.

       0070-build-values.

           if (cr-entry-date = parm-current-month-end)
              and (cr-entry-status <> '5' AND 'M')
              perform 0080-build-common
                                       thru 0080-exit
              perform 0090-build-issue thru 0090-exit
           end-if

           if (cr-ah-cancel-exit-date = parm-current-month-end)
              or (special-adjustment)
              perform 0080-build-common
                                       thru 0080-exit
              perform 0100-build-cancel
                                       thru 0100-exit
           end-if

           .
       0070-exit.
           exit.

       0080-build-common.

           move zeros                  to ws-retention     
           move zeros                  to ws-wholesale-fee 
           move zeros                  to ws-acct-fee      
           move zeros                  to ws-cso-admin-fee 
           move zeros                  to ws-ccc-admin-fee 
           move zeros                  to ws-total-fees    
           move zeros                  to ws-ga-fee        
           move zeros                  to ws-clp-prem      

           move ws-moe-date            to tb-month-end-dt
           move cr-carrier             to tb-carrier
           move cr-grouping            to tb-grouping
           move cr-state               to tb-state
           if cr-clp-state = spaces or low-values
              move cr-state            to cr-clp-state
           end-if
           move cr-clp-state           to tb-clp-state
           move cr-account             to tb-account
           move cr-dt                  to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if not no-conversion-error
              display ' error-dtecnvt-eff dt ' cr-cert-no ' 'cr-dt
           else
              move dc-greg-date-a-edit to tb-eff-dt
           end-if
           move cr-cert-no             to tb-addendum-no
           move am-retro-pool          to tb-retro-pool
           move cr-ahtyp               to tb-benefit-cd
           move zeros                  to tb-other1-fee-n
                                          tb-other2-fee-n

           .
       0080-exit.
           exit.

       0090-build-issue.

           move cr-ahprm               to tb-retail-fee-n
           if cr-lives not numeric
              move zeros               to cr-lives
           end-if
           move cr-lives               to tb-members-n
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
           compute ws-wholesale-fee = cr-ahprm - ws-acct-fee
           compute ws-clp-prem rounded =
              cr-ahprm - ws-cso-admin-fee - ws-ga-fee - ws-acct-fee
           compute ws-retention rounded = cr-ahprm * am-ah-ret

           compute tb-wholesale-fee-n rounded =
              ws-wholesale-fee * +1
           compute tb-account-fee-n rounded =
              ws-acct-fee * +1
           compute tb-cso-admin-fee-n rounded =
              ws-cso-admin-fee * +1
           compute ws-ccc-admin-fee rounded = 
              (ws-retention - ws-cso-admin-fee)
           compute tb-ccc-admin-fee-n rounded =
              ws-ccc-admin-fee * +1
           compute tb-total-cso-fee-n rounded =
              (ws-ccc-admin-fee + ws-cso-admin-fee) * +1
           compute tb-ga-fee-n rounded =
              ws-ga-fee * +1
           compute tb-clp-premium-n rounded =
              ws-clp-prem * +1

           .
       0090-exit.
           exit.

       0100-build-cancel.

      *    if cr-cert-no = '00714ACASH '
      *       move +96.77 to cr-ahrfnd
      *    end-if

           compute tb-retail-fee-n rounded = cr-ahrfnd * -1

           if cr-lives not numeric
              move zeros               to cr-lives
           end-if
           compute tb-members-n = cr-lives * -1

           compute ws-ref-factor rounded = cr-ahrfnd / cr-ahprm

           display ' ref-factor ' ws-ref-factor

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
           compute ws-wholesale-fee = (cr-ahrfnd - ws-acct-fee)

           compute ws-clp-prem rounded =
              cr-ahrfnd - ws-cso-admin-fee - ws-ga-fee - ws-acct-fee
           compute ws-retention rounded = cr-ahrfnd * am-ah-ret

           compute tb-wholesale-fee-n rounded =
              ws-wholesale-fee * -1
           compute tb-account-fee-n rounded =
              ws-acct-fee * -1
           compute tb-cso-admin-fee-n rounded =
              ws-cso-admin-fee * -1
           compute ws-ccc-admin-fee rounded = 
              (ws-retention - ws-cso-admin-fee)
           compute tb-ccc-admin-fee-n rounded =
              ws-ccc-admin-fee * -1
           compute tb-total-cso-fee-n rounded =
              (ws-ccc-admin-fee + ws-cso-admin-fee) * -1
           compute tb-ga-fee-n rounded =
              ws-ga-fee * -1
           compute tb-clp-premium-n rounded =
              ws-clp-prem * -1

           .
       0100-exit.
           exit.

       0110-insert-row.

      *    go to 0110-exit

           EXEC SQL
              insert into DCC_CU (
                MONTH_END_DT    ,
                CARRIER         ,
                GROUPING        ,
                ACCT_STATE      ,
                CLP_STATE       ,
                ACCOUNT_NO      ,
                EFFECTIVE_DT    ,
                ADDENDUM_NO     ,
                RETRO_POOL      ,
                BENEFIT_CD      ,
                RETAIL_FEE      ,
                WHOLESALE_FEE   ,
                ACCOUNT_FEE     ,
                CSO_ADMIN_FEE   ,
                CCC_ADMIN_FEE   ,
                TOTAL_CSO_FEES  ,
                GA_FEE          ,
                OTHER1_FEE      ,
                OTHER2_FEE      ,
                CLP_PREMIUM     ,
                MEMBERS)
	             values (
                  :TB-MONTH-END-DT     ,
                  :TB-CARRIER          ,
                  :TB-GROUPING         ,
                  :TB-STATE            ,
                  :TB-CLP-STATE        ,
                  :TB-ACCOUNT          ,
                  :TB-EFF-DT           ,
                  :TB-ADDENDUM-NO      ,
                  :TB-RETRO-POOL       ,
                  :TB-BENEFIT-CD       ,
                  :TB-RETAIL-FEE       ,
                  :TB-WHOLESALE-FEE    ,
                  :TB-ACCOUNT-FEE      ,
                  :TB-CSO-ADMIN-FEE    ,
                  :TB-CCC-ADMIN-FEE    ,
                  :TB-TOTAL-CSO-FEE    ,
                  :TB-GA-FEE           ,
                  :TB-OTHER1-FEE       ,
                  :TB-OTHER2-FEE       ,
                  :TB-CLP-PREMIUM      ,
                  :TB-MEMBERS)
           end-exec

           if sqlcode not = 0
              display "Error: cannot insert row "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' in out cnt ' ws-recs-in ' ' rec-cnt
              display ' offending rec ' DCC-CREDIT-UNION-TABLE
           else
              add 1 to rec-cnt
           end-if

           .
       0110-exit.
           exit.

       0120-finish-up.

           EXEC SQL
               commit work release
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
       0120-exit.
           exit.

       0130-sync-to-acct.

           evaluate true
              when am-control-a < cr-acct-control
                 perform 0035-read-eracctt
                                       thru 0035-exit
                 go to 0130-sync-to-acct
              when (am-control-a = cr-acct-control)
                 and (am-expire-dt < cr-dt)
                 perform 0035-read-eracctt
                                       thru 0035-exit
                 go to 0130-sync-to-acct
              when am-control-a > cr-acct-control
                 display ' no matching account master ' cr-account
                    ' ' cr-cert-no
                 go to 0130-exit
              when (am-control-a = cr-acct-control)
                 and (am-effect-dt > cr-dt)
                 display ' no matching account master ' cr-account
                    ' ' cr-cert-no
                 go to 0130-exit
              when other
                 set acct-found to true
           end-evaluate

           .
       0130-exit.
           exit.

       1000-create-table.

           go to 1000-exit

           display ' Begin Create table '

           EXEC SQL
              create table DCC_CU (
                MONTH_END_DT        datetime NOT null,
                CARRIER             char(1) NOT NULL,
                GROUPING            char(6),
                ACCT_STATE          char(2),
                CLP_STATE           char(2) not null,
                ACCOUNT_NO          char(10) not null,
                EFFECTIVE_DT        datetime not null,
                ADDENDUM_NO         char(11) not null,
                RETRO_POOL          char(6),
                BENEFIT_CD          char(2),
                RETAIL_FEE          decimal(11,2),
                WHOLESALE_FEE       decimal(11,2),
                ACCOUNT_FEE         decimal(11,2),
                CSO_ADMIN_FEE       decimal(11,2),
                CCC_ADMIN_FEE       decimal(11,2),
                TOTAL_CSO_FEES      decimal(11,2),
                GA_FEE              decimal(11,2),
                OTHER1_FEE          decimal(11,2),
                OTHER2_FEE          decimal(11,2),
                CLP_PREMIUM         decimal(11,2),
                MEMBERS             int
                 CONSTRAINT PK_DCC_CU PRIMARY KEY CLUSTERED
                   (MONTH_END_DT, CARRIER, CLP_STATE, ACCOUNT_NO, 
                   EFFECTIVE_DT, ADDENDUM_NO)
             	   )
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot create table "
              move sqlcode             to ws-disp-code
              display ' sql return code ' ws-disp-code
              display ' sql err mess    ' sqlerrmc
              goback
           end-if

           .
       1000-exit.
           exit.

       1010-drop-table.

           go to 1010-exit

           display 'Begin Drop table'
           EXEC SQL
               drop table DCC_CU
           END-EXEC
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

           go to 1020-exit

           display 'Begin Truncate table'
           EXEC SQL
               truncate table DCC_CU
           END-EXEC

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

       1030-check-for-rerun.

           exec sql
              SELECT
                 MONTH_END_DT,
                 Count(*)
              INTO
                 :TB-MONTH-END-DT,
                 :WS-REC-CNTR
              FROM
                 DCC_CU
              GROUP BY MONTH_END_DT
              HAVING convert(varchar(10),MONTH_END_DT,102)
                    = :ws-test-date
           end-exec

           if sqlcode not = 0 and 1
              display "Error : check for rerun  "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           display ' counter ' ws-rec-cntr

           .
       1030-exit.
           exit.

       1040-delete-rows.

           exec sql delete
              FROM
                 DCC_CU
              where convert(varchar(10),MONTH_END_DT,102)
                    = :ws-test-date
           end-exec

           if sqlcode not = 0 and 1
              display "Error : deleting rows  "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
              go to 1040-exit
           end-if

           EXEC SQL
               commit
           END-EXEC

           if sqlcode not = 0 and 1
              display "Error : commit of delete  "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       1040-exit.
           exit.

       2000-connect-to-logic.

           display ' about to connect to Logic '

063022     move 'TEST_Logic'           to svr
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass

           if ws-kix-myenv = 'cid1p'
063022        move 'PROD_Logic'        to svr
           end-if

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           EXEC SQL
              SET OPTION logintime 5
           END-EXEC

           EXEC SQL
              CONNECT TO :svr
                    USER :usr-pass
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot connect to Logic"
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              perform abend-pgm
           end-if

           .
       2000-exit.
           exit.

       2100-disconnect-logic.

           EXEC SQL
              DISCONNECT
           END-EXEC

           if sqlcode not = 0
              display "Error: disconnect Logic "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       2100-exit.
           exit.

       2500-connect-to-Actuary.

           display ' about to connect to Actuary '

063022     move 'TEST_Actuary'         to svr
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022     if ws-kix-myenv = 'cid1p'  *> or 'mdoff'
063022        move 'PROD_Actuary'      to svr
063022     end-if

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           EXEC SQL
              SET OPTION logintime 5
           END-EXEC

           EXEC SQL
              CONNECT TO :svr
                    USER :usr-pass
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot connect to Actuary"
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              perform abend-pgm
           end-if

           .
       2500-exit.
           exit.

       3000-go-get-refund-differences.

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE -1                     TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8500-DATE-CONVERT   thru 8590-exit
           IF NO-CONVERSION-ERROR
              display ' greg date a af ' dc-greg-date-a-edit
              MOVE DC-GREG-DATE-A-EDIT TO WS-LAST-MONTH-DT
           else
              display ' -1 dt error        ' dc-error-code
           END-IF

           display ' declare cursor ' ws-last-month-dt
           move +0     to ar1 ws-recs-in ar1-max sqlcode
      *    move '12/31/2016'           to ws-last-month-dt
           move '7' to ws-carr-7
           move '9' to ws-carr-9
           exec sql
              declare addlrefs cursor for
                 SELECT
                    dbo.DCC_CERTALL.CARRIER,
                    dbo.DCC_CERTALL.STATE,
                    dbo.DCC_CERTALL.ACCOUNT,
                    convert(varchar(8),dbo.DCC_CERTALL.EFF_DATE,112),
                    dbo.DCC_CERTALL.CERT_NO, 
                    dbo.DCC_CERTALL.AH_BEN_CODE,
                    dbo.DCC_CERTALL.AH_REFUND,
                    dbo.DCC_CERTALL_DW.AH_REFUND
                 FROM dbo.DCC_CERTALL LEFT OUTER JOIN
                    dbo.DCC_CERTALL_DW ON
                    dbo.DCC_CERTALL.CERT_NO =
                    dbo.DCC_CERTALL_DW.CERT_NO AND 
                    dbo.DCC_CERTALL.EFF_DATE =
                    dbo.DCC_CERTALL_DW.EFF_DATE AND
                    dbo.DCC_CERTALL.ACCOUNT =
                    dbo.DCC_CERTALL_DW.ACCOUNT AND 
                    dbo.DCC_CERTALL.STATE =
                    dbo.DCC_CERTALL_DW.STATE AND
                    dbo.DCC_CERTALL.GROUPING =
                    dbo.DCC_CERTALL_DW.GROUPING AND
                    dbo.DCC_CERTALL.CARRIER =
                    dbo.DCC_CERTALL_DW.CARRIER
                 WHERE
                    (dbo.DCC_CERTALL_DW.MOE_DATE =
                    :ws-last-month-dt) AND
                    (dbo.DCC_CERTALL.CARRIER = :ws-carr-7 OR
                    dbo.DCC_CERTALL.CARRIER = :ws-carr-9) AND
                    (dbo.DCC_CERTALL.AH_REFUND <>
                    dbo.DCC_CERTALL_DW.AH_REFUND)
           end-exec
           display ' sql code = ' sqlcode
           if sqlcode not = 0 and 100
              display "Error: cannot declare cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform abend-pgm
           end-if

           display ' open cursor '

           EXEC SQL
              open addlrefs
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform abend-pgm
           end-if


           perform until sqlcode not = 0 and 1
              EXEC SQL
                 fetch addlrefs into :ar-carrier,
                                     :ar-state,
                                     :ar-account,
                                     :ar-eff-dt,
                                     :ar-cert-no,
                                     :ar-ah-ben-cd,
                                     :ar-refund-n,
                                     :ar-prev-refund-n
              END-EXEC

              if sqlcode not = 0 and 100 and 1
                 display "Error: cannot read row "
                 display ' sql retrun code ' sqlcode
                 display ' sql err mess    ' sqlerrmc
                 goback
              else
                 if sqlcode = 0 or 1
                    add 1                 to ws-recs-in
                                             ar1
                    display ' cert ' ar-cert-no ' ' ar-refund-n ' '
                    ar-prev-refund-n ' '
                    ws-recs-in ' ' ar1
                    move ws-addl-rec to ws-addl-records-table (ar1)
                 end-if
              end-if
           end-perform

           if sqlcode = 100
              display ' Normal end of record set '
              display ' number of records        ' ws-recs-in
           end-if

           display ' num of records ' ws-recs-in

      *    set end-of-input            to true

           EXEC SQL
              close addlrefs
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              goback
           end-if

           move ar1 to ar1-max
           perform varying ar1 from +1 by +1 until ar1 > ar1-max
              display ' state ' art-state (ar1)
              display ' acct  ' art-account (ar1)
              display ' cert  ' art-cert-no (ar1)
              display ' ref **' art-refund (ar1) '**'
              display ' oref *' art-prev-refund (ar1) '**'
           end-perform

           move zeros to ws-recs-in
           .
       3000-exit.
           exit.

       8500-DATE-CONVERT.              
                                       COPY ELCDCS.

       abend-pgm.

            call 'ABORTME'.
            
            goback.
