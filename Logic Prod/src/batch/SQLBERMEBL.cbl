      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      *****************************************************************
      *                                                               *
      * Copyright (c) 2014 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLBERMEBL.
       AUTHOR.   Cowtown.

040418******************************************************************
040418*                   C H A N G E   L O G
040418*
040418* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
040418*-----------------------------------------------------------------
040418*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
040418* EFFECTIVE    NUMBER
040418*-----------------------------------------------------------------
040418* 040418  CR2018040400001  PEMA  ADD SIGN TO NET PREM & COMM FIELDS
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
040418******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERMEBL           ASSIGN TO ERMEBL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS ME-CONTROL-primary
                                   FILE STATUS IS ERMEBL-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERMEBL.

                                       copy ERCMEBL.

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
       77  ermebl-file-status          pic xx value low-values.
       77  eof-sw                      pic x value ' '.
           88  end-of-input               value 'Y'.

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


       01  ERMEBL-TABLE-RECORD.
           05  TB-MONTH-END-DT         PIC X(10).
           05  TB-COMPANY-ID           PIC XXX.
           05  TB-CERT-IN-CNT-N        PIC 9(7).
           05  TB-CERT-IN-CNT REDEFINES
               TB-CERT-IN-CNT-N        PIC X(7).
           05  TB-CERT-OUT-CNT-N       PIC 9(7).
           05  TB-CERT-OUT-CNT REDEFINES
               TB-CERT-OUT-CNT-N       PIC X(7).

040418     05  TB-ECS010-NET-PREM-N    PIC -9(9).99.
           05  TB-ECS010-NET-PREM REDEFINES
040418         TB-ECS010-NET-PREM-N    PIC X(13).
040418     05  TB-ECS010-NET-COMM-N    PIC -9(9).99.
           05  TB-ECS010-NET-COMM REDEFINES
040418         TB-ECS010-NET-COMM-N    PIC X(13).
           05  TB-ECS010-TOT-CLMS-N    PIC 9(9).99.
           05  TB-ECS010-TOT-CLMS REDEFINES
               TB-ECS010-TOT-CLMS-N    PIC X(12).
040418     05  TB-ECS010-MM-NET-PREM-N PIC -9(9).99.
           05  TB-ECS010-MM-NET-PREM REDEFINES
040418         TB-ECS010-MM-NET-PREM-N PIC X(13).
           05  TB-ECS010-MM-TOT-CLMS-N PIC 9(9).99.
           05  TB-ECS010-MM-TOT-CLMS REDEFINES
               TB-ECS010-MM-TOT-CLMS-N PIC X(12).
           05  TB-ECS010-MM-TOT-RESV-N PIC 9(9).99.
           05  TB-ECS010-MM-TOT-RESV REDEFINES
               TB-ECS010-MM-TOT-RESV-N PIC X(12).

           05  TB-ECS018-NET-COMM-Y-N  PIC -9(9).99.
           05  TB-ECS018-NET-COMM-Y REDEFINES
               TB-ECS018-NET-COMM-Y-N  PIC X(13).
           05  TB-ECS018-NET-OW-Y-N    PIC -9(9).99.
           05  TB-ECS018-NET-OW-Y REDEFINES
               TB-ECS018-NET-OW-Y-N    PIC X(13).

           05  TB-ECS018-NET-COMM-1-N  PIC -9(9).99.
           05  TB-ECS018-NET-COMM-1 REDEFINES
               TB-ECS018-NET-COMM-1-N  PIC X(13).
           05  TB-ECS018-NET-OW-1-N    PIC -9(9).99.
           05  TB-ECS018-NET-OW-1 REDEFINES
               TB-ECS018-NET-OW-1-N    PIC X(13).

040418     05  TB-ECS019-NET-PREM-N    PIC -9(9).99.
           05  TB-ECS019-NET-PREM REDEFINES
040418         TB-ECS019-NET-PREM-N    PIC X(13).
040418     05  TB-ECS019-NET-COMM-N    PIC -9(9).99.
           05  TB-ECS019-NET-COMM REDEFINES
040418         TB-ECS019-NET-COMM-N    PIC X(13).
040418     05  TB-ECS019-NET-OW-N      PIC -9(9).99.
           05  TB-ECS019-NET-OW REDEFINES
040418         TB-ECS019-NET-OW-N      PIC X(13).
           05  TB-ECS019-TOT-CLMS-N    PIC 9(9).99.
           05  TB-ECS019-TOT-CLMS REDEFINES
               TB-ECS019-TOT-CLMS-N    PIC X(12).

040418     05  TB-EL522-NET-PREM-N    PIC -9(9).99.
           05  TB-EL522-NET-PREM REDEFINES
040418         TB-EL522-NET-PREM-N    PIC X(13).
           05  TB-EL522-TOT-CLMS-N    PIC 9(9).99.
           05  TB-EL522-TOT-CLMS REDEFINES
               TB-EL522-TOT-CLMS-N    PIC X(12).
           05  TB-EL522-TOT-RESV-N    PIC 9(9).99.
           05  TB-EL522-TOT-RESV REDEFINES
               TB-EL522-TOT-RESV-N    PIC X(12).
           05  TB-EL522-TOT-PYAJ-N    PIC 9(9).99.
           05  TB-EL522-TOT-PYAJ REDEFINES
               TB-EL522-TOT-PYAJ-N    PIC X(12).

           05  TB-EL524-TOT-CLMS-N    PIC 9(9).99.
           05  TB-EL524-TOT-CLMS REDEFINES
               TB-EL524-TOT-CLMS-N    PIC X(12).
           05  TB-EL524-TOT-RESV-N    PIC 9(9).99.
           05  TB-EL524-TOT-RESV REDEFINES
               TB-EL524-TOT-RESV-N    PIC X(12).
           05  TB-EL524-TOT-CLMS-CM-N PIC 9(9).99.
           05  TB-EL524-TOT-CLMS-CM REDEFINES
               TB-EL524-TOT-CLMS-CM-N PIC X(12).

           05  TB-EL315-TOT-RESV-N    PIC 9(9).99.
           05  TB-EL315-TOT-RESV REDEFINES
               TB-EL315-TOT-RESV-N    PIC X(12).

           05  TB-ECS030-TOT-CLMS-N    PIC 9(9).99.
           05  TB-ECS030-TOT-CLMS REDEFINES
               TB-ECS030-TOT-CLMS-N    PIC X(12).

           05  TB-ECS032-TOT-RESV-N    PIC 9(9).99.
           05  TB-ECS032-TOT-RESV REDEFINES
               TB-ECS032-TOT-RESV-N    PIC X(12).

040418     05  TB-ECS035-NET-PREM-N    PIC -9(9).99.
           05  TB-ECS035-NET-PREM REDEFINES
040418         TB-ECS035-NET-PREM-N    PIC X(13).
040418     05  TB-ECS035-NET-COMM-N    PIC -9(9).99.
           05  TB-ECS035-NET-COMM REDEFINES
040418         TB-ECS035-NET-COMM-N    PIC X(13).
           05  TB-ECS035-TOT-CLMS-N    PIC 9(9).99.
           05  TB-ECS035-TOT-CLMS REDEFINES
               TB-ECS035-TOT-CLMS-N    PIC X(12).

040418     05  TB-ECS061-NET-PREM-N    PIC -9(9).99.
           05  TB-ECS061-NET-PREM REDEFINES
040418         TB-ECS061-NET-PREM-N    PIC X(13).
040418     05  TB-ECS061-NET-COMM-N    PIC -9(9).99.
           05  TB-ECS061-NET-COMM REDEFINES
040418         TB-ECS061-NET-COMM-N    PIC X(13).
040418     05  TB-ECS061-NET-OW-N      PIC -9(9).99.
           05  TB-ECS061-NET-OW REDEFINES
040418         TB-ECS061-NET-OW-N      PIC X(13).
           05  TB-ECS061-TOT-CLMS-N    PIC 9(9).99.
           05  TB-ECS061-TOT-CLMS REDEFINES
               TB-ECS061-TOT-CLMS-N    PIC X(12).
           05  TB-ECS061-TOT-PYAJ-N    PIC 9(9).99.
           05  TB-ECS061-TOT-PYAJ REDEFINES
               TB-ECS061-TOT-PYAJ-N    PIC X(12).

040418     05  TB-ECS063-NET-OW-N      PIC -9(9).99.
           05  TB-ECS063-NET-OW REDEFINES
040418         TB-ECS063-NET-OW-N      PIC X(13).

           05  TB-ECS050-ACT-L-N   PIC 9(7).
           05  TB-ECS050-ACT-L REDEFINES
               TB-ECS050-ACT-L-N   PIC X(7).
           05  TB-ECS050-ACT-AH-N   PIC 9(7).
           05  TB-ECS050-ACT-AH REDEFINES
               TB-ECS050-ACT-AH-N   PIC X(7).

           05  TB-ECS082-ACT-L-N   PIC 9(7).
           05  TB-ECS082-ACT-L REDEFINES
               TB-ECS082-ACT-L-N   PIC X(7).
           05  TB-ECS082-ACT-AH-N   PIC 9(7).
           05  TB-ECS082-ACT-AH REDEFINES
               TB-ECS082-ACT-AH-N   PIC X(7).

           05  TB-ECS016-IN-CNT-N        PIC 9(9).
           05  TB-ECS016-IN-CNT REDEFINES
               TB-ECS016-IN-CNT-N        PIC X(9).
           05  TB-ECS016-OUT-CNT-N       PIC 9(9).
           05  TB-ECS016-OUT-CNT REDEFINES
               TB-ECS016-OUT-CNT-N       PIC X(9).

           05  TB-ECS041-IN-CNT-N        PIC 9(9).
           05  TB-ECS041-IN-CNT REDEFINES
               TB-ECS041-IN-CNT-N        PIC X(9).
           05  TB-ECS041-OUT-CNT-N       PIC 9(9).
           05  TB-ECS041-OUT-CNT REDEFINES
               TB-ECS041-OUT-CNT-N       PIC X(9).

040418     05  TB-EL562-NET-PREM-N    PIC -9(9).99.
           05  TB-EL562-NET-PREM REDEFINES
040418         TB-EL562-NET-PREM-N    PIC X(13).
040418     05  TB-EL562-NET-COMM-N    PIC -9(9).99.
           05  TB-EL562-NET-COMM REDEFINES
040418         TB-EL562-NET-COMM-N    PIC X(13).

           05  TB-EL341-TOT-CLMS-N    PIC 9(9).99.
           05  TB-EL341-TOT-CLMS REDEFINES
               TB-EL341-TOT-CLMS-N    PIC X(12).

           05  TB-EL317-TOT-CLMS-N    PIC 9(9).99.
           05  TB-EL317-TOT-CLMS REDEFINES
               TB-EL317-TOT-CLMS-N    PIC X(12).

           05  TB-EL325-TOT-CLMS-N    PIC 9(9).99.
           05  TB-EL325-TOT-CLMS REDEFINES
               TB-EL325-TOT-CLMS-N    PIC X(12).

           05  TB-ECS020-NET-PREM-N    PIC 9(11).99.
           05  TB-ECS020-NET-PREM REDEFINES
               TB-ECS020-NET-PREM-N    PIC X(14).
           05  TB-ECS020-TOT-CLMS-N    PIC 9(9).99.
           05  TB-ECS020-TOT-CLMS REDEFINES
               TB-ECS020-TOT-CLMS-N    PIC X(12).
           05  TB-ECS020-TOT-RESV-N    PIC 9(9).99.
           05  TB-ECS020-TOT-RESV REDEFINES
               TB-ECS020-TOT-RESV-N    PIC X(12).

           05  TB-EL515-CLMS-WRAP-N    PIC 9(9).99.
           05  TB-EL515-CLMS-WRAP REDEFINES
               TB-EL515-CLMS-WRAP-N    PIC X(12).
           05  TB-EL515-RESV-WRAP-N    PIC 9(9).99.
           05  TB-EL515-RESV-WRAP REDEFINES
               TB-EL515-RESV-WRAP-N    PIC X(12).

040418     05  TB-EL523-TOT-PREM-N    PIC -9(9).99.
           05  TB-EL523-TOT-PREM REDEFINES
040418         TB-EL523-TOT-PREM-N    PIC X(13).

           05  TB-ECS064-BEG-BAL-N    PIC -9(10).99.
           05  TB-ECS064-BEG-BAL REDEFINES
               TB-ECS064-BEG-BAL-N    PIC X(14).
           05  TB-ECS064-END-BAL-N    PIC -9(10).99.
           05  TB-ECS064-END-BAL REDEFINES
               TB-ECS064-END-BAL-N    PIC X(14).
           05  TB-ECS080-MORT-RESV-N  PIC 9(11).99.
           05  TB-ECS080-MORT-RESV REDEFINES
               TB-ECS080-MORT-RESV-N  PIC X(14).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

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
           05  parm-program-option     pic x.

       01  var  pic x(30).

       procedure division using parm.
                                       COPY ELCDTERX.
       0000-begin.

      **  program option 1 
      **     Build the entire table including previous months
      **  program option 2
      **     Just build / rebuild the current month
      **     based on the parm date

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

           perform 0010-init           thru 0010-exit
           perform 2000-connect-to-logic
                                       thru 2000-exit

           if parm-program-option = '1'
              perform 1010-drop-table  thru 1010-exit
           end-if

      *    perform 1020-truncate-table thru 1020-exit

           if parm-program-option = '1'
              perform 1000-create-table
                                       thru 1000-exit
           end-if

           perform 0020-open-files     thru 0020-exit

           EXEC SQL
              SET AUTOCOMMIT OFF
           END-EXEC

           perform 0030-start-input    thru 0030-exit
040418     if not end-of-input
040418        perform 0046-read-input  thru 0046-exit
040418     end-if
           perform 0045-process-input  thru 0045-exit until
              end-of-input
      *      or ws-recs-in > 10000

           perform 0060-finish-up      thru 0060-exit
           close ERMEBL
           display ' End Program '
           display ' rows inserted ' rec-cnt
           display ' records read  ' ws-recs-in
           goback

           .
       0010-init.

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE    ' '                 TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           move dc-greg-date-a-edit    to ws-moe-date
           display ' parm length '     parm-length ' '
              parm-current-month-end

           string
              parm-current-month-end (5:4)
              parm-current-month-end (1:2)
              parm-current-month-end (3:2)
              delimited by size into dc-greg-date-cymd-r
           end-string           

           move 'L'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to tb-month-end-dt
              display ' init month end date ' tb-month-end-dt
           else
              display ' invalid parm date - aborting '
                 parm-current-month-end
              perform abend-pgm
           end-if

           move dc-greg-date-cymd      to ws-work-date
       
           compute ws-work-date-moyr =
              (ws-work-ccyy * 12) + ws-work-mm
       
           display ' moyr ' ws-work-date-moyr

           if parm-program-option = '1'
              display ' Build the whole file '
           else
              if parm-program-option = '2'
                 display ' Build current month '
              else
                 display ' invalid program option '
                 perform abend-pgm
              end-if
           end-if

           .
       0010-exit.
           exit.

       0020-open-files.

           open input ERMEBL
           if ermebl-file-status not = '00'
              display ' error-ermebl-open ' ermebl-file-status
              perform abend-pgm
           end-if

           .
       0020-exit.
           exit.

       0030-start-input.

           if parm-program-option = '1'
              move low-values to me-control-primary
           else
              move dte-client          to me-company
              move ws-work-date-moyr   to me-moyr
           end-if
           start ermebl key >= me-control-primary
           if ermebl-file-status = '10' or '23'
              set end-of-input to true
           else
              if ermebl-file-status not = '00'
                 display ' error-ermebl-start ' ermebl-file-status
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

           read ermebl next record
           
           evaluate true
              when (parm-program-option = '1' or '2')
                 and (ermebl-file-status = '10' or '23')
                 set end-of-input      to true
              when (parm-program-option = '2')
                 and ((me-company not = dte-client)
                 or (me-moyr not = ws-work-date-moyr))
                 set end-of-input      to true
              when ermebl-file-status not = '00'
                 display ' error-ermebl-read ' ermebl-file-status
                 perform abend-pgm
              when other
                 add 1 to ws-recs-in
           end-evaluate

           .
       0046-exit.
           exit.

       0050-insert-row.

           perform 0052-build-values   thru 0052-exit
           perform 0057-insert-row     thru 0057-exit

           .
       0050-exit.
           exit.

       0052-build-values.

           move me-company             to tb-company-id

           if parm-program-option = '2'
              go to 0052-continue
           end-if

           divide me-moyr by 12 giving ws-work-ccyy
              remainder ws-work-mm
           if ws-work-mm = zeros
              move 12 to ws-work-mm
              subtract 1 from ws-work-ccyy
           end-if
           move 01                     to ws-work-dd
           move ws-work-date           to dc-greg-date-cymd-r
           display ' ws-work-date ' ws-work-date
           move 'L'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if not no-conversion-error
              display ' bad me eom dt ' me-moyr
              perform abend-pgm
           end-if
           move dc-days-in-month    to ws-work-dd
           string ws-work-mm '/'
              ws-work-dd     '/'
              ws-work-ccyy delimited by size into tb-month-end-dt
           end-string
       
           display ' tb month end ' tb-month-end-dt ' '
              dc-days-in-month

           .
       0052-continue.

           move me-010-cert-in         to tb-cert-in-cnt-n
           move me-010-cert-out        to tb-cert-out-cnt-n

           compute tb-ecs010-net-prem-n =
              me-010-net-l + me-010-net-ah
           compute tb-ecs010-net-comm-n =
              me-010-comm-l + me-010-comm-ah
           compute tb-ecs010-tot-clms-n =
              me-010-pmt-l + me-010-pmt-ah

           compute tb-ecs018-net-comm-y-n = me-018-comm-y
           compute tb-ecs018-net-comm-1-n = me-018-comm-1
           compute tb-ecs018-net-ow-y-n   = me-018-ow-y
           compute tb-ecs018-net-ow-1-n   = me-018-ow-1

           compute tb-ecs019-net-prem-n =
              (me-019-prem-l + me-019-prem-ah) -
              (me-019-ref-l + me-019-ref-ah)
           compute tb-ecs019-net-comm-n =
              me-019-comm-l + me-019-comm-ah
           compute tb-ecs019-net-ow-n =
              me-019-or-l   + me-019-or-ah

            display ' me 019 claims ' me-019-clms-l ' ' me-019-clms-ah
            compute tb-ecs019-tot-clms-n =
               me-019-clms-l + me-019-clms-ah
      *    move zeros                  to tb-ecs019-tot-clms-n

           compute tb-el522-net-prem-n =
              (me-522-prem-l + me-522-prem-ah) -
              (me-522-ref-l + me-522-ref-ah)
           compute tb-el522-tot-clms-n =
              me-522-proc-clm-l + me-522-proc-clm-ah
           compute tb-el522-tot-resv-n =
              me-522-proc-rsv-l + me-522-proc-rsv-ah
           compute tb-el522-tot-pyaj-n = me-522-py-adj

           compute tb-el524-tot-clms-n =
              me-524-clms-l + me-524-clms-ah
           compute tb-el524-tot-resv-n =
              me-524-resv-l + me-524-resv-ah
           compute tb-el524-tot-clms-cm-n = me-524-clms-tot-cm

           compute tb-el315-tot-resv-n =
              me-315-resv-l + me-315-resv-ah

           compute tb-ecs030-tot-clms-n =
              me-030-clms-l + me-030-clms-ah

           compute tb-ecs032-tot-resv-n =
              me-032-resv-l + me-032-resv-ah

           compute tb-ecs035-net-prem-n =
              me-035-net-l + me-035-net-ah
            compute tb-ecs035-net-comm-n = me-035-comm-tot
            compute tb-ecs035-tot-clms-n =
               me-035-clms-l + me-035-clms-ah
      *     move zeros                 to tb-ecs035-net-comm-n
      *                                   tb-ecs035-tot-clms-n

           compute tb-ecs061-net-prem-n = me-061-prem
           compute tb-ecs061-net-comm-n = me-061-comm
           compute tb-ecs061-net-ow-n   = me-061-or
           compute tb-ecs061-tot-pyaj-n = me-061-py-adj
           display ' ecs061 claims ' me-061-clms
           compute tb-ecs061-tot-clms-n = me-061-clms
      *    move zeros                  to tb-ecs061-tot-clms-n

           compute tb-ecs063-net-ow-n   = me-063-ow
      *    move zeros                  to tb-ecs063-net-ow-n

           compute tb-ecs050-act-l-n = me-050-act-l
           compute tb-ecs050-act-ah-n = me-050-act-ah
      *    move zeros                  to tb-ecs050-act-l-n
      *                                   tb-ecs050-act-ah-n

           compute tb-ecs082-act-l-n = me-082-act-l
           compute tb-ecs082-act-ah-n = me-082-act-ah
      *    move zeros                  to tb-ecs082-act-l-n
      *                                   tb-ecs082-act-ah-n

           compute tb-ecs016-in-cnt-n = me-016-recs-in
           compute tb-ecs016-out-cnt-n = me-016-recs-out
      *    move zeros                  to tb-ecs016-in-cnt-n
      *                                   tb-ecs016-out-cnt-n

           compute tb-ecs041-in-cnt-n = me-041-recs-in
           compute tb-ecs041-out-cnt-n = me-041-recs-out
      *    move zeros                  to tb-ecs041-in-cnt-n
      *                                   tb-ecs041-out-cnt-n

           compute tb-el562-net-prem-n = me-562-prem-tot
           compute tb-el562-net-comm-n = me-562-comm-tot
      *    move zeros                  to tb-el562-net-prem-n
      *                                   tb-el562-net-comm-n

           compute tb-el341-tot-clms-n =
              me-341-clms-l + me-341-clms-ah

           compute tb-el317-tot-clms-n = me-317-clms-tot

           compute tb-el325-tot-clms-n = me-325-clms-tot

           compute tb-ecs020-net-prem-n = me-020-prem-tot
           compute tb-ecs020-tot-clms-n = me-020-clms-tot
           compute tb-ecs020-tot-resv-n = me-020-resv-tot
           compute tb-el515-clms-wrap-n = me-515-clms-wrap
           compute tb-el515-resv-wrap-n = me-515-resv-wrap
           compute tb-el523-tot-prem-n  = me-523-prem-tot

           display ' 523 tb me ' tb-el523-tot-prem ' ' me-523-prem-tot

           compute tb-ecs064-beg-bal-n  = me-064-beg-bal
           compute tb-ecs064-end-bal-n  = me-064-end-bal
           compute tb-ecs010-mm-net-prem-n = me-010-mm-prem-tot
           compute tb-ecs010-mm-tot-clms-n = me-010-mm-tot-clms
           compute tb-ecs010-mm-tot-resv-n = me-010-mm-tot-resv
           if me-080-mort-resv not numeric
              move zeros               to me-080-mort-resv
           end-if
           compute tb-ecs080-mort-resv-n  = me-080-mort-resv

           .
       0052-exit.
           exit.

       0057-insert-row.

           EXEC SQL
              insert into ERMEBL (
                MONTH_END_DT        ,
                COMPANY_ID          ,
                CERT_IN_CNT         ,
                CERT_OUT_CNT        ,
                ECS010_NET_PREM     ,
                ECS010_NET_COMM     ,
                ECS010_TOT_CLMS     ,
                ECS010_MM_NET_PREM  ,
                ECS010_MM_TOT_CLMS  ,
                ECS010_MM_TOT_RESV  ,
                ECS018_NET_COMM_Y   ,
                ECS018_NET_OW_Y     ,
                ECS018_NET_COMM_1   ,
                ECS018_NET_OW_1     ,
                ECS019_NET_PREM     ,
                ECS019_NET_COMM     ,
                ECS019_NET_OW       ,
                ECS019_TOT_CLMS     ,
                EL522_NET_PREM      ,
                EL522_TOT_CLMS      ,
                EL522_TOT_RESV      ,
                EL522_TOT_PYAJ      ,
                EL524_TOT_CLMS      ,
                EL524_TOT_RESV      ,
                EL524_TOT_CLMS_CM   ,
                EL315_TOT_RESV      ,
                ECS030_TOT_CLMS     ,
                ECS032_TOT_RESV     ,
                ECS035_NET_PREM     ,
                ECS035_NET_COMM     ,
                ECS035_TOT_CLMS     ,
                ECS061_NET_PREM     ,
                ECS061_NET_COMM     ,
                ECS061_NET_OW       ,
                ECS061_TOT_CLMS     ,
                ECS061_TOT_PYAJ     ,
                ECS063_NET_OW       ,
                ECS050_ACT_L        ,
                ECS050_ACT_AH       ,
                ECS082_ACT_L        ,
                ECS082_ACT_AH       ,
                ECS016_IN_CNT       ,
                ECS016_OUT_CNT      ,
                ECS041_IN_CNT       ,
                ECS041_OUT_CNT      ,
                EL562_NET_PREM      ,
                EL562_NET_COMM      ,
                EL341_TOT_CLMS      ,
                EL317_TOT_CLMS      ,
                EL325_TOT_CLMS      ,
                ECS020_ITD_PREM     ,
                ECS020_ITD_CLMS     ,
                ECS020_ITD_RESV     ,
                EL515_CLMS_WRAP     ,
                EL515_RESV_WRAP     ,
                EL523_TOT_PREM      ,
                ECS064_BEG_BAL      ,
                ECS064_END_BAL      ,
                ECS080_MORT_RESV)
	             values (
                  :TB-MONTH-END-DT       ,
                  :TB-COMPANY-ID         ,
                  :TB-CERT-IN-CNT        ,
                  :TB-CERT-OUT-CNT       ,
                  :TB-ECS010-NET-PREM    ,
                  :TB-ECS010-NET-COMM    ,
                  :TB-ECS010-TOT-CLMS    ,
                  :TB-ECS010-MM-NET-PREM ,
                  :TB-ECS010-MM-TOT-CLMS ,
                  :TB-ECS010-MM-TOT-RESV ,
                  :TB-ECS018-NET-COMM-Y  ,
                  :TB-ECS018-NET-OW-Y    ,
                  :TB-ECS018-NET-COMM-1  ,
                  :TB-ECS018-NET-OW-1    ,
                  :TB-ECS019-NET-PREM    ,
                  :TB-ECS019-NET-COMM    ,
                  :TB-ECS019-NET-OW      ,
                  :TB-ECS019-TOT-CLMS    ,
                  :TB-EL522-NET-PREM     ,
                  :TB-EL522-TOT-CLMS     ,
                  :TB-EL522-TOT-RESV     ,
                  :TB-EL522-TOT-PYAJ     ,
                  :TB-EL524-TOT-CLMS     ,
                  :TB-EL524-TOT-RESV     ,
                  :TB-EL524-TOT-CLMS-CM  ,
                  :TB-EL315-TOT-RESV     ,
                  :TB-ECS030-TOT-CLMS    ,
                  :TB-ECS032-TOT-RESV    ,
                  :TB-ECS035-NET-PREM    ,
                  :TB-ECS035-NET-COMM    ,
                  :TB-ECS035-TOT-CLMS    ,
                  :TB-ECS061-NET-PREM    ,
                  :TB-ECS061-NET-COMM    ,
                  :TB-ECS061-NET-OW      ,
                  :TB-ECS061-TOT-CLMS    ,
                  :TB-ECS061-TOT-PYAJ    ,
                  :TB-ECS063-NET-OW      ,
                  :TB-ECS050-ACT-L       ,
                  :TB-ECS050-ACT-AH      ,
                  :TB-ECS082-ACT-L       ,
                  :TB-ECS082-ACT-AH      ,
                  :TB-ECS016-IN-CNT      ,
                  :TB-ECS016-OUT-CNT     ,
                  :TB-ECS041-IN-CNT      ,
                  :TB-ECS041-OUT-CNT     ,
                  :TB-EL562-NET-PREM     ,
                  :TB-EL562-NET-COMM     ,
                  :TB-EL341-TOT-CLMS     ,
                  :TB-EL317-TOT-CLMS     ,
                  :TB-EL325-TOT-CLMS     ,
                  :TB-ECS020-NET-PREM    ,
                  :TB-ECS020-TOT-CLMS    ,
                  :TB-ECS020-TOT-RESV    ,
                  :TB-EL515-CLMS-WRAP    ,
                  :TB-EL515-RESV-WRAP    ,
                  :TB-EL523-TOT-PREM     ,
                  :TB-ECS064-BEG-BAL     ,
                  :TB-ECS064-END-BAL     ,
                  :TB-ECS080-MORT-RESV)
           end-exec

           if sqlcode not = 0
              display "Error: cannot insert row "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' in out cnt ' ws-recs-in ' ' rec-cnt
              display ' offending rec ' ermebl-table-record
           else
              add 1 to rec-cnt
           end-if

           .
       0057-exit.
           exit.

       0060-finish-up.

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
       0060-exit.
           exit.

       1000-create-table.

           display ' Begin Create table '

           EXEC SQL
              create table ERMEBL (
                MONTH_END_DT        datetime NOT null,
                COMPANY_ID          char(3) NOT NULL,
                CERT_IN_CNT         int not null,
                CERT_OUT_CNT        int not null,
                ECS010_NET_PREM     decimal(11,2),
                ECS010_NET_COMM     decimal(11,2),
                ECS010_TOT_CLMS     decimal(11,2),
                ECS010_MM_NET_PREM  decimal(11,2),
                ECS010_MM_TOT_CLMS  decimal(11,2),
                ECS010_MM_TOT_RESV  decimal(11,2),
                ECS018_NET_COMM_Y   decimal(11,2),
                ECS018_NET_OW_Y     decimal(11,2),
                ECS018_NET_COMM_1   decimal(11,2),
                ECS018_NET_OW_1     decimal(11,2),
                ECS019_NET_PREM     decimal(11,2),
                ECS019_NET_COMM     decimal(11,2),
                ECS019_NET_OW       decimal(11,2),
                ECS019_TOT_CLMS     decimal(11,2),
                EL522_NET_PREM      decimal(11,2),
                EL522_TOT_CLMS      decimal(11,2),
                EL522_TOT_RESV      decimal(11,2),
                EL522_TOT_PYAJ      decimal(11,2),
                EL524_TOT_CLMS      decimal(11,2),
                EL524_TOT_RESV      decimal(11,2),
                EL524_TOT_CLMS_CM   decimal(11,2),
                EL315_TOT_RESV      decimal(11,2),
                ECS030_TOT_CLMS     decimal(11,2),
                ECS032_TOT_RESV     decimal(11,2),
                ECS035_NET_PREM     decimal(11,2),
                ECS035_NET_COMM     decimal(11,2),
                ECS035_TOT_CLMS     decimal(11,2),
                ECS061_NET_PREM     decimal(11,2),
                ECS061_NET_COMM     decimal(11,2),
                ECS061_NET_OW       decimal(11,2),
                ECS061_TOT_CLMS     decimal(11,2),
                ECS061_TOT_PYAJ     decimal(11,2),
                ECS063_NET_OW       decimal(11,2),
                ECS050_ACT_L        int,
                ECS050_ACT_AH       int,
                ECS082_ACT_L        int,
                ECS082_ACT_AH       int,
                ECS016_IN_CNT       int not null,
                ECS016_OUT_CNT      int not null,
                ECS041_IN_CNT       int not null,
                ECS041_OUT_CNT      int not null,
                EL562_NET_PREM      decimal(11,2),
                EL562_NET_COMM      decimal(11,2),
                EL341_TOT_CLMS      decimal(11,2),
                EL317_TOT_CLMS      decimal(11,2),
                EL325_TOT_CLMS      decimal(11,2),
                ECS020_ITD_PREM     decimal(13,2),
                ECS020_ITD_CLMS     decimal(11,2),
                ECS020_ITD_RESV     decimal(11,2),
                EL515_CLMS_WRAP     decimal(11,2),
                EL515_RESV_WRAP     decimal(11,2),
                EL523_TOT_PREM      decimal(11,2),
                ECS064_BEG_BAL      decimal(11,2),
                ECS064_END_BAL      decimal(11,2),
                ECS080_MORT_RESV    decimal(11,2)
                 CONSTRAINT PK_ERMEBL PRIMARY KEY CLUSTERED
                   (MONTH_END_DT, COMPANY_ID)
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

           display 'Begin Drop table'
           EXEC SQL
               drop table ERMEBL
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

           display 'Begin Truncate table'
           EXEC SQL
               truncate table ERMEBL
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

       2000-connect-to-logic.

           display ' about to connect to Logic '

063022     move 'TEST_Logic'           to svr
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022     if ws-kix-myenv = 'cid1p'
063022        move 'PROD_Logic'        to svr
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

       8500-DATE-CONVERT.              
                                       COPY ELCDCS.

       abend-pgm.

            call 'ABORTME'.
            
            goback.
