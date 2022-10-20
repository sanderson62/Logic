      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK07.
       AUTHOR.     Cowtown.
       DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************

020617******************************************************************
020617*REMARKS.                                                        *
020617*  Returns premium calc based on info passed to me.              *
020617******************************************************************
020617*                   C H A N G E   L O G
020617*
020617* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020617*-----------------------------------------------------------------
020617*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020617* EFFECTIVE    NUMBER
020617*-----------------------------------------------------------------
020617* 020617   2017020300002   PEMA  New Program
061515******************************************************************
       ENVIRONMENT DIVISION.
       data division.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SOCK07   WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
      *
      * program buffers
      *
       77 ws-send-msg-size             pic s9(8) comp value 256.
       77 ws-recv-msg-size             pic s9(8) comp value 256.
       77 ws-recv-buf                  pic x(256).
       77 ws-send-buf                  pic x(256) VALUE SPACES.
       77 ws-recv-total                pic s9(8) comp value 0.
       77 ws-recv-left                 pic s9(8) comp value 0.
       77 ws-seq-num                   pic s9(8) comp value 0.
       77 ws-flags                     pic s9(8) comp value 0.
       77 WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
       77  ws-comp-id                  pic xxx value spaces.
       77 WS-SAVE-ACCOUNT              PIC X(10)  VALUE SPACES.
       77 WS-BIN-ORIG-EFF-DT           PIC XX  VALUE LOW-VALUES.
       77 WS-ORIG-EFF-DT               PIC X(10)  VALUE SPACES.
       77 WS-EFF-DATE                  PIC X(10)  VALUE SPACES.
       77 WS-EXP-DATE                  PIC X(10)  VALUE SPACES.
       77 X1                           PIC S999 COMP-3 VALUE +0.
       77 S1                           PIC S999 COMP-3 VALUE +0.
       77 S2                           PIC S999 COMP-3 VALUE +0.
       77 WS-BUILD-SW                  PIC X.
          88  TIME-TO-BUILD               VALUE 'Y'.
       77 WS-SAVE-ERACCT               PIC X(2000).
       77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
       77 WS-PERFORM-SW                PIC X VALUE SPACES.
          88  GET-RATES                    VALUE 'R'.
          88  GET-ACT-ACCTS                VALUE 'A'.
       77  ws-bin-current-dt           pic xx  value low-values.
       77 ws-bin-eff-dt                pic xx  value low-values.
       77  ws-bin-exp-dt               pic xx  value low-values.
       77 ws-bin-1st-pmt-dt            pic xx  value low-values.
       77 ws-bin-pri-birth-dt          pic xx  value low-values.
       77 ws-bin-cob-birth-dt          pic xx  value low-values.
       77 WS-DISP-AMT                  PIC Z,ZZZ,Z99.99.
       77 ws-disp-rate                 pic z9.99999.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  WS-ERCTBL-SW                PIC X VALUE ' '.
           88  END-OF-ERCTBL                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       77  ws-socket-sw                pic x value ' '.
           88  end-of-socket              value 'Y'.
       77  rec-cnt                     pic 9(5) value zeros.
       77  ws-stop-sw                  pic x value ' '.
           88  i-say-stop                 value 'Y'.
       77  ws-browse-sw                pic x value ' '.
           88  browse-started            value 'Y'.
       77  ws-contract-sw              pic x  value ' '.
           88  contract-no-assigned      value 'Y'.
       77  ws-error-sw                 pic x value ' '.
           88  error-in-one-coverage     value 'Y'.
       77  ws-connect-sw               pic x value ' '.
           88  connected-to-db           value 'Y'.

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
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  ws-dealer-state             pic xx.
       01  ws-dealer-id                pic x(10).
       01  ws-contract-eff-dt          pic x(10).
       01  ws-contract-no              pic x(10) value spaces.
       01  ws-contract-suffix          pic x     value spaces.

       01  ws-key-stuff.
           05  ws-ks-contract-no       pic x(10) value spaces.
           05  ws-batch-no             pic x(6) value zeros.
           05  ws-batch-no-n redefines
               ws-batch-no             pic 9(6).
           05  ws-check-key            pic 9(7).
           05  ws-check-no             pic x(7).
           05  ws-compid               pic xxx.
           05  ws-carrier              pic x.
           05  ws-grouping             pic x(6).
           05  ws-state                pic xx.
           05  ws-account              pic x(10).
           05  ws-eff-date             pic x(10).
           05  ws-certificate          pic x(10).
           05  ws-cert-sfx             pic x.
           05  ws-seq-no               pic 999.
           05  ws-type                 pic 999.

       01  ws-status-code-a            pic x(7) value zeros.
       01  ws-status-code redefines
           ws-status-code-a            pic 9(7).
       01  ws-error-message            pic x(50) value spaces.
       01  ws-status-date              pic x(10).
       01  sqlcmd                      pic x(1024).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is        null.           The indicator will be -1        ***
      ***  if the value        is null  and +0 if the value is       ***
      ***  something other than null.  Here is an example on how     ***
      ***  to use the indicator variables.                           ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        fetch checkapp into                                 ***
      ***           :db-app-status :nu-app-status,                   ***
      ***           :db-app-by     :nu-app-by,                       ***
      ***           :db-app-date   :nu-app-date,                     ***
      ***           :db-app-batch  :nu-app-batch                     ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***           OR This way on an update                         ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        UPDATE                                              ***
      ***           CUC_Logic_Remittance                             ***
      ***        SET                                                 ***
      ***           LogicStatus     = :ws-status-code,               ***
      ***           LogicStatusDate = :ws-status-date,               ***
      ***           BatchNumber     = :ws-batch-no :nu-batchno       ***
      ***        WHERE                                               ***
      ***           RemitId = :ws-remit-id                           ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***    Also, when the table has a column with a data type of   ***
      ***  "BIT" and used as true/false move the 1 byte receiving    ***
      ***  field to ws-bit and check out ws-bit-comp. if = zeros,    ***
      ***  then its false. I think true would be 256.                ***
      ***                                                            ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  ws-bit                      pic x.
       01  ws-bit-comp redefines ws-bit pic s9(4) comp.
       01  indicator-vaiables-for-nulls.
           05  nu-app-status           pic s9(4) comp value +0.
           05  nu-app-by               pic s9(4) comp value +0.
           05  nu-app-date             pic s9(4) comp value +0.
           05  nu-app-batch            pic s9(4) comp value +0.
           05  nu-fincar               pic s9(4) comp value +0.
           05  nu-batchno              pic s9(4) comp value +0.
           05  nu-error-message        pic s9(4) comp value +0.

       01  sql-cert-records.
           05  sql-dlr-state           pic xx.
           05  sql-dlr-id              pic x(10).
           05  sql-eff-dt              pic x(10).
           05  sql-contr-no            pic x(10).
           05  sql-contr-suffix        pic x.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  filler.
           05  ws-last-suffix          pic x value low-values.
           05  ws-tbl-last-suffix      pic x value low-values.
           05  filler.  *> Use X1 for this table.
               10  ws-codes            pic x(26) value
               ' ABCDEFGHIJKLMNOPQRSTUVWXY'.
               10  ws-suffix-value redefines ws-codes
                 occurs 26             pic x.

       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).

       01  ws-rate-work-area.
           05  ws-rate-benefit-type   pic x.
           05  ws-rate-benefit-cd     pic xx.
           05  ws-rate-benefit-a      pic x(8).
           05  ws-rate-benefit redefines
               ws-rate-benefit-a      pic 9(6)v99.
           05  ws-rate-premium-a      pic x(7).
           05  ws-rate-premium redefines
               ws-rate-premium-a      pic 9(5)v99.
           05  ws-rate-apr-a          pic x(7).
           05  ws-rate-apr redefines
               ws-rate-apr-a          pic 99v9(5).
           05  ws-rate-loan-term      pic 999  value zeros.
           05  ws-rate-ins-term       pic 999  value zeros.
           05  ws-issue-age           pic 999  value zeros.
           05  ws-cob-age             pic 999  value zeros.
           05  ws-rate-age            pic 999  value zeros.
           05  ws-max-benefit         pic 9(7)v99 value zeros.
           05  ws-rate-max-bens       pic 99.

       01  soc-client-in-data.
           05  rate-state              pic xx.
           05  rate-acct-no            pic x(10).
           05  rate-vin                pic x(17).
           05  rate-lf-ah              pic x.
           05  rate-ben-code           pic xx.
           05  rate-earn-meth          pic x.
           05  rate-pri-birth-date     pic x(10).
           05  rate-cob-birth-date     pic x(10).
           05  rate-benefit.
               10  rate-ben-dollars    pic x(6).
               10  f                   pic x.
               10  rate-ben-pennies    pic xx.
           05  rate-eff-date           pic x(10).
           05  rate-1st-pmt-dt         pic x(10).
           05  rate-premium.
               10  rate-prem-dollars   pic x(5).
               10  f                   pic x.
               10  rate-prem-pennies   pic xx.
           05  rate-loan-term          pic xxx.
           05  rate-loan-term-n redefines
                   rate-loan-term      pic 999.
           05  rate-ins-term           pic XXX.
           05  rate-ins-term-n redefines
               rate-ins-term           pic 999.
           05  rate-apr.
               10  rate-apr-whole      pic xx.
               10  f                   pic x.
               10  rate-apr-dec        pic x(5).
           05  rate-sin-jnt-ind        pic x.
           05  rate-dismemberment      pic x.
           05  rate-retro-elim         pic x.
           05  rate-waiting-days       pic xx.
           05  rate-waiting-days-n redefines
               rate-waiting-days       pic 99.
           05  rate-crit-per           pic xx.
           05  rate-crit-per-n redefines
               rate-crit-per           pic 99.

       01  WS-AM-KEY.
           05  WS-AM-COMPANY-CD        PIC X.                                       
           05  WS-AM-CARRIER           PIC X.                                       
           05  WS-AM-GROUP             PIC X(6).                                    
           05  WS-AM-STATE             PIC XX.   
           05  WS-AM-ACCOUNT           PIC X(10).
           05  WS-AM-EXP-DT            PIC XX.
           05  FILLER                  PIC X(4).

       01  ws-cm5-compare-key          pic x(12).
       01  WS-CM5-KEY.
           05  WS-CM5-COMPANY-CD       PIC X.                                       
           05  WS-CM5-CERT-NO          PIC X(11).

       01  ws-cm-compare-key           pic x(33).
       01  WS-CM-KEY.
           05  WS-CM-COMPANY-CD        PIC X.                                       
           05  WS-CM-CARRIER           PIC X.                                       
           05  WS-CM-GROUP             PIC X(6).                                    
           05  WS-CM-STATE             PIC XX.   
           05  WS-CM-ACCOUNT           PIC X(10).
           05  WS-CM-EFF-DT            PIC XX.
           05  WS-CM-CERT-NO.
               10  ws-cm-cert-ten      pic x(10).
               10  ws-cm-cert-suffix   pic x.

       01  WS-CS-KEY.
           05  WS-CS-COMPANY-CD        PIC X.                                       
           05  WS-CS-CARRIER           PIC X.                                       
           05  WS-CS-GROUP             PIC X(6).                                    
           05  WS-CS-STATE             PIC XX.   
           05  WS-CS-ACCOUNT           PIC X(10).
           05  WS-CS-EFF-DT            PIC XX.
           05  WS-CS-CERT-NO           PIC X(11).
           05  WS-CS-TRLR-TYPE         PIC X.

       01  WS-AM-ALT-KEY.
           05  WS-AM-ALT-ACCOUNT       PIC X(10).
           05  WS-AM-ALT-EXP-DT        PIC XX.

       01  WS-CF-KEY-SAVE              PIC X(10).
       01  WS-CF-KEY.
           05  WS-CF-COMPANY-ID        PIC XXX.
           05  WS-CF-RECORD-TYPE       PIC X.
           05  WS-CF-ACCESS            PIC X(4).
           05  WS-CF-SEQ-NO            PIC S9(4) COMP.
           
       01  filler.
           05  ws-errors-table.
               10  filler              pic x(50) value
               '0000Transaction successfully completed'.
               10  filler              pic x(50) value
               '0101Problem with amounts'.
               10  filler              pic x(50) value
               '0102Problem with dates'.
               10  filler              pic x(50) value
               '0103Issue age outside limits'.
               10  filler              pic x(50) value
               '0104Attained age outside limits'.
               10  filler              pic x(50) value
               '0105Term is not valid must be gt 0 and lt 361'.
               10  filler              pic x(50) value
               '0106Term is outside limits'.
               10  filler              pic x(50) value
               '0107Amount is outside limits'.
               10  filler              pic x(50) value
               '0108Loan term must be ge insurance term'.
               10  filler              pic x(50) value
               '0109Total benefit is outside limits'.
               10  filler              pic x(50) value
               '0110Rates found were zero'.
               10  filler              pic x(50) value
               '0111Rate table was not found'.
               10  filler              pic x(50) value
               '0112Rate file not open'.
               10  filler              pic x(50) value
               '0113Premium is out of tolerence'.
               10  filler              pic x(50) value
               '0114Duplicate contract and VIN on Logic'.
               10  filler              pic x(50) value
               '0115Problem with loan or insurance term'.
           05  filler redefines ws-errors-table occurs 16.
               10  ws-table-error-no   pic x(4).
               10  ws-table-error-mess pic x(46).

       01  ws-return-string.
           05  ws-return-error-no      pic x(4).
           05  ws-sc1                  pic x.
           05  ws-return-error-mess    pic x(46).
           05  ws-sc2                  pic x.
           05  ws-return-contract-no   pic x(11).
           05  ws-sc3                  pic x.
           05  ws-return-max-ben       pic z,zzz,z99.99.
           05  ws-sc4                  pic x.
           05  ws-return-prem          pic z,zzz,z99.99.
           05  ws-sc5                  pic x.
           05  ws-return-rate          pic z9.99999.
           05  ws-sc6                  pic x.
           05  ws-return-exp-dt        pic x(10).
           05  ws-sc7                  pic x.
           05  ws-return-benefit-cd    pic xx.

       01  WS-CID-NO                   PIC X(8).

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  resp-duprec                  value +14.
           88  resp-dupkey                  value +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

                                        COPY ERCACCT.
                                        COPY ELCCERT.
                                        COPY ELCCRTT.
                                        COPY ELCCNTL.
                                        COPY ELCFUNDT.
                                        COPY ELCDATE.
                                        COPY ELCCALC.
      
       linkage section.
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).
      ****   client-in-data must be 36 characters.  ****
         05 CLIENT-IN-DATA.
            15  CLIENT-KICK-OFF      PIC X(8).
            15  CLIENT-ID            PIC XXX.
            15  FILLER               PIC X(25).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).

       01  var  pic x(30).

       procedure division.

           display 'SOCK07:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK07:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK07:socket name      =', lstn-name ' '
              lstn-subname

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           move ws-fn-cymd             to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-current-dt
           else
              display ' error current dt invalid ' dc-error-code
           end-if

           display ' current date/time '
              ws-fn-mo '/' ws-fn-da '/' ws-fn-ccyy ' - '
                 ws-fn-hours ':' ws-fn-minutes ':' ws-fn-seconds

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

           perform 0000-init-contact   thru 0000-exit
           perform 0100-process-socket thru 0100-exit until
              end-of-socket

           go to 0300-close-socket

           .
       0000-init-contact.

           if client-kick-off = 'SOCKET07'
              continue
           else
              move '9999;Unknown origin, who are you? '
                                       to ws-return-string
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              display ' Unknown origin ' client-in-data
              go to 0300-close-socket
           end-if

           evaluate client-id
              when 'DCC'
                 MOVE X'05'            TO WS-COMP-CD
                 MOVE 'DCC'            TO WS-COMP-ID
              when 'CID'
                 MOVE X'04'            TO WS-COMP-CD
                 MOVE 'CID'            TO WS-COMP-ID
              when 'AHL'
                 MOVE X'06'            TO WS-COMP-CD
                 MOVE 'AHL'            TO WS-COMP-ID
              when other
                 move '0113;Invalid company id ' to ws-return-string
                 PERFORM 0200-SEND-BUFFER thru 0200-exit
                 display ' Invalid company id ' client-id
                 go to 0300-close-socket
           END-evaluate

           move 'SOCKET07READY'        to ws-send-buf
           move +25                    to ws-send-msg-size

           display 'SOCK07:sequence number  =', ws-seq-num.
           display 'SOCK07:send buffer      =', ws-send-buf(1:25)

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags

           if return-code <= zero
              display 'SOCK07:send error ' return-code
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if

           .
       0000-exit.
           exit.

       0010-receive.

           display 'SOCK07:About to recv '

           call "recv" using by value GIVE-TAKE-SOCKET
               by reference ws-recv-buf
               by value ws-recv-msg-size
               by value ws-flags.

           if return-code < zero
              display 'SOCK07:recv error ' return-code
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0010-exit
           end-if

           if return-code = zero
              display 'SOCK07:client disconnected',
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0010-exit
           end-if

           display 'SOCK07:Good recv  '
           display 'SOCK07:return code      = ', return-code
           display 'SOCK07:receive buffer   = ', ws-recv-buf(1:51)

021516     move +112                   to ws-send-msg-size

           move ws-recv-buf (1:111)     to soc-client-in-data

           if ws-recv-buf (1:4) = 'DONE' or 'done' or 'Done'
              set end-of-socket to true
           end-if

           .
       0010-exit.
           exit.

       0020-edit-received.

           if rate-sin-jnt-ind = 'S' OR 'J'
              continue
           else
              move 'S'                 to rate-sin-jnt-ind
           end-if
           if rate-dismemberment = ' ' or 'Y' or 'N'
              continue
           else
              move 'N'                 to rate-dismemberment
           end-if
           if rate-retro-elim = 'R' or 'E'
              continue
           else
              move 'R'                 to rate-retro-elim
           end-if

           inspect
              rate-benefit replacing all spaces by zeros
           inspect
              rate-premium replacing all spaces by zeros
           inspect
              rate-loan-term replacing all spaces by zeros
           inspect
              rate-ins-term replacing all spaces by zeros
           inspect
              rate-apr replacing all spaces by zeros
           inspect
              rate-waiting-days replacing all spaces by zeros
           inspect
              rate-crit-per replacing all spaces by zeros

           if rate-ben-dollars not numeric
              move zeros               to rate-ben-dollars
           end-if
           if rate-ben-pennies not numeric
              move zeros               to rate-ben-pennies
           end-if
           string
              rate-ben-dollars
              rate-ben-pennies delimited by size
                 into                  ws-rate-benefit-a
           end-string

           if rate-prem-dollars not numeric
              move zeros               to rate-prem-dollars
           end-if
           if rate-prem-pennies not numeric
              move zeros               to rate-prem-pennies
           end-if
           string
              rate-prem-dollars
              rate-prem-pennies delimited by size
                 into                  ws-rate-premium-a
           end-string

           if rate-loan-term not numeric
              move zeros               to rate-loan-term
           end-if
           move rate-loan-term         to ws-rate-loan-term
           move rate-ins-term          to ws-rate-ins-term

           if rate-apr-whole not numeric
              move zeros               to rate-apr-whole
           end-if
           if rate-apr-dec not numeric
              move zeros               to rate-apr-dec
           end-if
           string
              rate-apr-whole
              rate-apr-dec delimited by size
                 into                  ws-rate-apr-a
           end-string
           if rate-waiting-days-n not numeric
              move zeros               to rate-waiting-days-n
           end-if
           if rate-crit-per-n not numeric
              move zeros               to rate-crit-per-n
           end-if

           if ws-rate-ins-term = zeros
              move '0115;Problem with insurance term '
                                       to ws-return-string
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-if

           if rate-lf-ah = 'L'
              move 'L'                 to ws-rate-benefit-type
           else
              move 'A'                 to ws-rate-benefit-type
           end-if

      **==============================================================**
      **                                                              **
      **  Validate passed effective date. Probably should add edits   **
      **                                                              **
      **==============================================================**

           move rate-eff-date (7:4)    to ws-work-date (1:4)
           move rate-eff-date (1:2)    to ws-work-date (5:2)
           move rate-eff-date (4:2)    to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-eff-dt
           else
              move '0102;Problem with Eff Date' to ws-return-string
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-if

           if ws-bin-eff-dt > ws-bin-current-dt
              move '0102;Future Effective Date' to ws-return-string
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-if

      **==============================================================**
      **                                                              **
      **  Validate passed 1st pmt date.   Probably should add edits   **
      **                                                              **
      **==============================================================**

           move rate-1st-pmt-dt (7:4)  to ws-work-date (1:4)
           move rate-1st-pmt-dt (1:2)  to ws-work-date (5:2)
           move rate-1st-pmt-dt (4:2)  to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-1st-pmt-dt
           else
              move low-values          to ws-bin-1st-pmt-dt
              move '0102;Problem with 1st pmt Date'
                                       to ws-return-string
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-if

           if ws-bin-1st-pmt-dt <= ws-bin-eff-dt
              move low-values          to ws-bin-1st-pmt-dt
              move '0102;1st Pmt Date not > Eff Dt '
                                       to ws-return-string
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-if

      **==============================================================**
      **                                                              **
      **  Only use the code below if there is a time they don't       **
      **  send the 1st pmt date.                                      **
      **                                                              **
      **==============================================================**

           if ws-bin-1st-pmt-dt = low-values
              move +1                  to dc-elapsed-months
              move +0                  to dc-elapsed-days
              move '6'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-bin-date-2    to ws-bin-1st-pmt-dt
              else
                 move '0102;Problem with 1st pmt Date'
                                       to ws-return-string
                 perform 0200-send-buffer
                                       thru 0200-exit
                 go to 0300-close-socket
              end-if
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate the expiration date to be passed back.            **
      **                                                              **
      **==============================================================**

           move ws-bin-1st-pmt-dt      to dc-bin-date-1
           compute dc-elapsed-months = ws-rate-ins-term - 1
           move +0                     to dc-elapsed-days
           move '6'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-2       to ws-bin-exp-dt
              move dc-greg-date-a-edit to ws-exp-date
           else
              move '0102;Problem with calculating exp date'
                                    to ws-return-string
              perform 0200-send-buffer
                                    thru 0200-exit
              go to 0300-close-socket
           end-if

      **==============================================================**
      **                                                              **
      **  Validate primary borrower birth date.                       **
      **                                                              **
      **==============================================================**

           move rate-pri-birth-date (7:4)
                                       to ws-work-date (1:4)
           move rate-pri-birth-date (1:2)
                                       to ws-work-date (5:2)
           move rate-pri-birth-date (4:2)
                                       to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-pri-birth-dt
           else
              move '0102;Problem with Pri Birth Date'
                                       to ws-return-string
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-if

      **==============================================================**
      **                                                              **
      **  Validate co borrowers birth date.                           **
      **                                                              **
      **==============================================================**

           if rate-cob-birth-date (7:4) = spaces
              move zeros               to rate-cob-birth-date
           end-if
           move rate-cob-birth-date (7:4)
                                       to ws-work-date (1:4)
           move rate-cob-birth-date (1:2)
                                       to ws-work-date (5:2)
           move rate-cob-birth-date (4:2)
                                       to ws-work-date (7:2)
           move low-values             to ws-bin-cob-birth-dt
           if ws-work-date-num not = zeros
              move ws-work-date-num    to dc-greg-date-cymd
              move 'L'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-bin-date-1    to ws-bin-cob-birth-dt
              else
                 move '0102;Problem with Cob Birth Date'
                                       to ws-return-string
                 perform 0200-send-buffer
                                       thru 0200-exit
                 go to 0300-close-socket
              end-if
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate primary borrowers age.                            **
      **                                                              **
      **==============================================================**

           move ws-bin-pri-birth-dt    to dc-bin-date-1
           move ws-bin-eff-dt          to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              compute ws-issue-age = dc-elapsed-months / +12
              display ' issue age = 'ws-issue-age
           else
              move '0102;Problem with Pri Age calculation'
                                       to ws-return-string
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate co borrowers age.                                 **
      **                                                              **
      **==============================================================**

           move zeros                  to ws-cob-age
           if ws-bin-cob-birth-dt not = low-values
              move ws-bin-cob-birth-dt to dc-bin-date-1
              move ws-bin-eff-dt       to dc-bin-date-2
              move '1'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 compute ws-cob-age = dc-elapsed-months / +12
                 display ' cob   age = 'ws-cob-age
              else
                 move '0102;Problem with Cob Age calculation'
                                       to ws-return-string
                 perform 0200-send-buffer
                                       thru 0200-exit
                 go to 0300-close-socket
              end-if
           end-if

           move ws-issue-age           to ws-rate-age
           if ws-cob-age > ws-rate-age
              display ' Using Co borrower age ' ws-issue-age
                 ' ' ws-cob-age
              move ws-cob-age          to ws-rate-age
           end-if

           .
       0020-exit.
           exit.

       0025-assign-ben-cd.

      **==============================================================**
      **                                                              **
      **     Eventually, depending on how well this project takes off **
      **  we should figure out a much better way to assign benefit    **
      **  codes. For now since we only have 1 or 2 accounts in TX it  **
      **  will be hard coded.                                         **
      **                                                              **
      **==============================================================**

           if ws-rate-benefit-type = 'L'
              if rate-sin-jnt-ind = 'S'
                 move '01'             to ws-rate-benefit-cd
              else
                 move '03'             to ws-rate-benefit-cd
              end-if
           else
              if rate-sin-jnt-ind = 'J'
                 if rate-retro-elim = 'R'
                    if rate-waiting-days-n = 14
                       move '54'       to ws-rate-benefit-cd
                    else
                       if rate-waiting-days-n = 30
                          move '55'    to ws-rate-benefit-cd
                       end-if
                    end-if
                 else
                    if rate-retro-elim = 'E'
                       if rate-waiting-days-n = 14
                          move '56'    to ws-rate-benefit-cd
                       else
                          if rate-waiting-days-n = 30
                             move '57' to ws-rate-benefit-cd
                          end-if
                       end-if
                    end-if
                 end-if
              else
                 if rate-retro-elim = 'R'
                    if rate-waiting-days-n = 14
                       move '01'       to ws-rate-benefit-cd
                    else
                       if rate-waiting-days-n = 30
                          move '04'    to ws-rate-benefit-cd
                       end-if
                    end-if
                 else
                    if rate-retro-elim = 'E'
                       if rate-waiting-days-n = 14
                          move '05'    to ws-rate-benefit-cd
                       else
                          if rate-waiting-days-n = 30
                             move '02' to ws-rate-benefit-cd
                          end-if
                       end-if
                    end-if
                 end-if
              end-if
           end-if

           .
       0025-exit.
           exit.

       0030-get-rate.

           perform 0050-get-account    thru 0050-exit

      *    move spaces                 to calculation-pass-area
           move zeros                  to cp-r-max-mon-ben
                                          cp-r-max-tot-ben
                                          cp-rate-dev-pct
                                          cp-original-premium
                                          cp-critical-months
                                          cp-term-or-ext-days
           move am-cal-table           to cp-class-CODE

           if ws-rate-benefit-type = 'L'
              move am-lf-deviation     to cp-deviation-code
           else
              move am-ah-deviation     to cp-deviation-code
           end-if

           move rate-state             to cp-state
                                          cp-state-std-abbrv
           move ws-rate-benefit-type   to cp-benefit-type
           move ws-rate-benefit-cd     to cp-benefit-cd
           move ws-rate-benefit        to cp-original-benefit
                                          cp-rating-benefit-amt
           move ws-rate-age            to cp-issue-age
           move ws-comp-id             to cp-company-id
           move ws-comp-cd             to cp-company-cd

           move ws-rate-apr            to cp-loan-apr

           if ws-rate-loan-term = zeros
              move ws-rate-ins-term    to ws-rate-loan-term
           end-if
           move ws-rate-ins-term       to cp-original-term
           move ws-rate-loan-term      to cp-loan-term

           move 'L'                    to cp-life-override-code
           move 'A'                    to cp-ah-override-code
           move '3'                    to cp-process-type
           move rate-earn-meth         to cp-earning-method
                                          cp-rating-method
           move 'R'                    to cp-rate-file
           move ws-bin-eff-dt          to cp-cert-eff-dt
           move ws-bin-1st-pmt-dt      to cp-first-pay-date
           
           PERFORM 7000-GET-RATE       THRU 7000-EXIT

           if no-cp-error
              if ((cp-calc-premium - ws-rate-premium) > 9.99)
                              or
                 ((ws-rate-premium - cp-calc-premium) > 9.99)
                 move 'Z'              to cp-return-code
              end-if
           end-if

      **==============================================================**
      **                                                              **
      ** The below code will change as we add more states and forms   **
      ** to this process.                                             **
      **                                                              **
      **==============================================================**
           if ws-rate-benefit-type = 'L'
              move cp-original-benefit to ws-max-benefit
           else
              compute ws-max-benefit =
                 cp-original-term * cp-original-benefit
           end-if

           .
       0030-exit.
           exit.

       0040-format-buffer.

           move spaces                 to ws-return-string

           perform 0045-format-error   thru 0045-exit
           move ';'                    to ws-sc1
                                          ws-sc2
                                          ws-sc3
                                          ws-sc4
                                          ws-sc5
                                          ws-sc6
                                          ws-sc7
           if no-cp-error
              if ws-contract-suffix = spaces or low-values
                 move ws-contract-no   to ws-return-contract-no (2:10)
              else
                 move ws-contract-no   to ws-return-contract-no (1:10)
                 move ws-contract-suffix
                                       to ws-return-contract-no (11:1)
              end-if
              move cp-calc-premium     to ws-return-prem
              move cp-premium-rate     to ws-return-rate
              move ws-max-benefit      to ws-return-max-ben
              move ws-exp-date         to ws-return-exp-dt
              move ws-rate-benefit-cd  to ws-return-benefit-cd
           else
              set error-in-one-coverage to true
              move spaces              to ws-contract-no
                                          ws-contract-suffix
           end-if

           .
       0040-exit.
           exit.

       0045-format-error.

           evaluate true
              when cp-return-code = '0'
                 move +1               to s1
              when cp-return-code = '1'
                 move +2               to s1
              when cp-return-code = '2'
                 move +3               to s1
              when cp-return-code = 'A'
                 move +4               to s1
              when cp-return-code = 'B'
                 move +5               to s1
              when cp-return-code = '4'
                 move +6               to s1
              when cp-return-code = '9'
                 move +7               to s1
              when cp-return-code = '8'
                 move +8               to s1
              when cp-return-code = 'H'
                 move +9               to s1
              when cp-return-code = 'C'
                 move +10              to s1
              when cp-return-code = '7'
                 move +11              to s1
              when cp-return-code = '6'
                 move +12              to s1
              when cp-return-code = 'D'
                 move +13              to s1
              when cp-return-code = 'Z'
                 move +14              to s1
                 move cp-calc-premium  to ws-return-prem
                 move cp-premium-rate  to ws-return-rate
                 move ws-max-benefit   to ws-return-max-ben
                 move ws-exp-date      to ws-return-exp-dt
                 move ws-rate-benefit-cd to ws-return-benefit-cd
              when cp-return-code = 'Y'
                 move +15              to s1
                 move cp-calc-premium  to ws-return-prem
                 move cp-premium-rate  to ws-return-rate
                 move ws-max-benefit   to ws-return-max-ben
                 move ws-exp-date      to ws-return-exp-dt
                 move ws-rate-benefit-cd to ws-return-benefit-cd
           end-evaluate

           move ws-table-error-no (s1) to ws-return-error-no
           move ws-table-error-mess (s1)
                                       to ws-return-error-mess

           .
       0045-exit.
           exit.
           
       0050-get-account.

           move ws-comp-cd             to ws-am-company-cd
           move '9'                    to ws-am-carrier
           move '000000'               to ws-am-group
           move rate-state             to ws-am-state
           move rate-acct-no           to ws-am-account
           move ws-bin-eff-dt          to ws-am-exp-dt

           exec cics read
              dataset      ('ERACCT')
              ridfld       (ws-am-key)
              into         (account-master)
              GTEQ
              resp         (ws-response)
           end-exec

           if resp-normal
              and (ws-comp-cd = am-company-cd)
              and (rate-state = am-state)
              and (rate-acct-no = am-account)
              and (ws-bin-eff-dt >= am-effective-dt)
              and (ws-bin-eff-dt < am-expiration-dt)
              continue
           else
              move '0114;No account mstr found ' to ws-return-string
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-if

           .
       0050-exit.
           exit.

       0060-check-for-dup.

      **==============================================================**
      **                                                              **
      **    All i'm going to do here is check for a dup using         **
      **  the state, account, eff dt and last six of vin and a space  **
      **  in the cert suffix. If I do find one then I will have to    **
      **  find the last suffix and use the next available one in the  **
      **  suffix table.                                               **
      **                                                              **
      **==============================================================**

           move +1                     to x1
           move ' '                    to ws-browse-sw

           move ws-comp-cd             to ws-cm-key
           move '9'                    to ws-cm-carrier
           move '000000'               to ws-cm-group
           move rate-state             to ws-cm-state
           move rate-acct-no           to ws-cm-account
           move ws-bin-eff-dt          to ws-cm-eff-dt
           string
              '0000'
              rate-vin (12:6)
              ' ' delimited by size into ws-cm-cert-no
           end-string
           move ws-cm-key              to ws-cm-compare-key
           move ws-cm-cert-ten         to ws-contract-no
           move low-values             to ws-last-suffix

           display ' about to startbr ' ws-cm-key (2:19) ' '
              rate-eff-date  ' ' ws-cm-key (23:11)
              
           exec cics startbr
              dataset      ('ELCERT')
              ridfld       (ws-cm-key)
              gteq 
              resp         (ws-response)
           end-exec

           evaluate true
              when resp-normal
                 set browse-started to true
              when resp-notfnd or resp-endfile
                 set i-say-stop to true
              when other
                 move ws-response to ws-disp-resp
                 string
                    '9999' ';'
                    'Bad elcert startbr ' ';'
                    ws-disp-resp delimited by size
                                       into ws-return-string
                 end-string
                 display ' something went wrong with start br '
                    ws-response
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-evaluate

           perform until i-say-stop
              exec cics readnext
                 dataset      ('ELCERT')
                 ridfld       (ws-cm-key)
                 into         (certificate-master)
                 resp         (ws-response)
              end-exec
              if resp-normal
                 if ws-cm-key (1:32) = ws-cm-compare-key (1:32)
                    move ws-cm-cert-suffix
                                       to ws-last-suffix
                 else
                    set i-say-stop to true
                 end-if
              else
                 set i-say-stop to true
              end-if
           end-perform

           if browse-started
              exec cics endbr
                 dataset   ('ELCERT')
              END-EXEC
           end-if

           if ws-last-suffix not = low-values
              perform varying x1 from +1 by +1 until
                 (x1 > +26)
                 or (ws-last-suffix = ws-suffix-value (x1))
              end-perform
              if x1 < +27
                 move ws-suffix-value (x1 + 1)
                                       to ws-contract-suffix
              else
                 MOVE 'BLEW SUFFIX TABLE ' to ws-return-string
                 display ' more than 26 suffix codes ' ws-last-suffix
                 perform 0200-send-buffer thru 0200-exit
                 go to 0300-close-socket
              end-if
           end-if

           .
       0060-exit.
           exit.


       0070-open-cursor.

pemtst*    display ' declare cursor ' ws-begin-dt ' ' ws-end-dt

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  The dates on the sql table have values in the time        ***
      ***  so I convert it to a string and just use mm/dd/yyyy       ***
      ***  to perform the comparison.                                ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if

           move rate-state             to ws-dealer-state
           move rate-acct-no           to ws-dealer-id
           move rate-eff-date          to ws-contract-eff-dt
           move zeros                  to ws-ks-contract-no
           move rate-vin (12:6)        to ws-ks-contract-no (5:6)
           
           EXEC SQL
              DECLARE
                 contracts cursor for
              SELECT
                 DlrState,
                 DlrId,
                 EffDt,
                 ContractNo,
                 ContractSuffix
              FROM
                 PendingContracts
              WHERE
                 DlrState     = :ws-dealer-state
                 and DlrId    = :ws-dealer-id
                 and EffDt    = :ws-contract-eff-dt
                 and ContractNo  = :ws-ks-contract-no
              ORDER BY
                 dlrstate,
                 dlrid,
                 effdt,
                 contractno,
                 contractsuffix
           end-exec

           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 0070-exit
           end-if

           EXEC SQL
              open contracts
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 0070-exit
           end-if

           .
       0070-exit.
           exit.
       0080-process-input.

           perform until sqlcode not = 0
              EXEC SQL
                 fetch contracts into
                    :sql-dlr-state,
                    :sql-dlr-id,
                    :sql-eff-dt,
                    :sql-contr-no,
                    :sql-contr-suffix
              END-EXEC

              if sqlcode = 0
                 display ' got a hit ' sql-contr-no ' '
                    sql-contr-suffix
                 move sql-contr-suffix to ws-tbl-last-suffix
              else
                 if sqlcode not = 0 and 100
                    display "Error: cannot fetch row " 
                    display ' sql return code ' sqlcode
                    display ' sql err mess    ' sqlerrmc
                 end-if
              end-if
           end-perform
           if ws-tbl-last-suffix = low-values
              display ' tbl last suffix = LV '
           else
              if ws-tbl-last-suffix = spaces
                 display ' tbl last suffix = SPACES '
              else
                 display ' tbl last suffix = ' ws-tbl-last-suffix
              end-if
           end-if

           if ws-last-suffix = low-values
              display ' last suffix = LV '
           else
              if ws-last-suffix = spaces
                 display ' last suffix = SPACES '
              else
                 display ' last suffix = ' ws-last-suffix
              end-if
           end-if

           if sqlcode = 100
              display ' Normal end of record set '
           end-if

           if ws-tbl-last-suffix not = low-values
              and (ws-tbl-last-suffix > ws-last-suffix)
              perform varying x1 from +1 by +1 until
                 (x1 > +26)
                 or (ws-tbl-last-suffix = ws-suffix-value (x1))
              end-perform
              if x1 < +27
                 move ws-suffix-value (x1 + 1)
                                       to ws-contract-suffix
              else
                 MOVE 'BLEW SUFFIX TABLE ' to ws-return-string
                 display ' more than 26 suffix codes ' ws-last-suffix
                 perform 0200-send-buffer thru 0200-exit
                 go to 0300-close-socket
              end-if
           end-if

           EXEC SQL
               close contracts
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0080-exit.
           exit.

       0100-process-socket.

           perform 0010-receive        thru 0010-exit
           if end-of-socket
              go to 0100-exit
           end-if
           perform 0020-edit-received  thru 0020-exit
           perform 0025-assign-ben-cd  thru 0025-exit
           perform 0030-get-rate       thru 0030-exit
           if (contract-no-assigned)
                  or
              (error-in-one-coverage)
              continue
           else
              perform 0060-check-for-dup
                                       thru 0060-exit
              perform 0070-open-cursor thru 0070-exit
              perform 0080-process-input
                                       thru 0080-exit
           end-if
           perform 0040-format-buffer  thru 0040-exit
           perform 0200-send-buffer    thru 0200-exit
           set contract-no-assigned to true

           .
       0100-exit.
           exit.

       0170-PROCESS-BEN-CODE.

           MOVE SPACES                 TO WS-CF-KEY
           MOVE 'CID'                  TO WS-CF-COMPANY-ID
           MOVE '5'                    TO WS-CF-RECORD-TYPE
           MOVE ws-rate-benefit-cd     TO WS-CF-ACCESS (3:2)
           MOVE +0                     TO WS-CF-SEQ-NO
           MOVE WS-CF-KEY              TO WS-CF-KEY-SAVE
           
           EXEC CICS READ
                DATASET    ('ELCNTL')
                RIDFLD     (WS-CF-KEY)
                INTO       (CONTROL-FILE)
                GTEQ
                RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE WS-RESPONSE         TO WS-DISP-RESP
              DISPLAY ' ERROR - ELCNTL - READ ' WS-DISP-RESP
              GO TO 0170-EXIT
           END-IF
           
           IF WS-CF-KEY (1:4) = WS-CF-KEY-SAVE (1:4)
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                 (S2 > +8)
                 OR (ws-rate-BENefit-CD = CF-BENEFIT-CODE (S2))
              END-PERFORM
           END-IF

           .
       0170-EXIT.
           EXIT.

       0200-send-buffer.

           move ws-return-string       to ws-send-buf
           display 'SOCK07:About to send      ' ws-send-buf
           display 'SOCK07:sequence number  =', ws-seq-num.
           display ' msg size ' ws-send-msg-size

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.

           if return-code <= zero
              display 'SOCK07:send error ',
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if

           .
       0200-exit.
           exit.

       0250-socket-error.

           display "SOCK07:did not complete"
           display 'SOCK07:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK07:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK07:socket name      =', lstn-name ' '
              lstn-subname
           display ' return code = ' return-code

           .
       0250-exit.
           exit.

       0300-close-socket.

      *    call "close" using by value GIVE-TAKE-SOCKET .
           display 'SOCK07:done'
           exec cics return end-exec.
           goback.

           .
       0300-exit.
           exit.

       6000-CONNECT-TO-DB.
      
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           MOVE 'NTSQLTST2_CrtManage'  TO SVR
           move 'sa'                   to usr
           move 'sql2008r2'            to pass

           if ws-kix-myenv = 'cid1p'
              MOVE 'NTCSO2_CrtManage'  TO SVR
              move 'sa'                to usr
              move 'ntcso2'            to pass
           end-if

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           display ' About to connect to ' svr ' ' usr-pass

           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
       
           if sqlcode not = 0
              display "Error: cannot connect to " svr
              display sqlcode
              display sqlerrmc
           else
              display ' Successful Connect ' sqlcode
              set connected-to-db to true
           end-if

           .
       6000-EXIT.
           EXIT.

       7000-GET-RATE.
       
           EXEC CICS LINK
              PROGRAM ('ELRATE')
              COMMAREA (CALCULATION-PASS-AREA)
              LENGTH   (CP-COMM-LENGTH)
           END-EXEC
           
           .
       7000-EXIT.
           EXIT.

       8000-code-storage.

      *     move am-company-cd          to ws-cm5-company-cd
      *
      *     move spaces                 to ws-cm5-cert-no
      *     string
      *        '0000'
      *        rate-vin (12:6)
      *        ' ' delimited by size into ws-cm5-cert-no
      *     end-string
      *     move ws-cm5-key to ws-cm5-compare-key
      *     display ' about to startbr **' ws-cm5-cert-no '**'
      *     exec cics startbr
      *        dataset      ('ELCERT5')
      *        ridfld       (ws-cm5-key)
      *        equal
      *        resp         (ws-response)
      *     end-exec
      *     move ' ' to ws-stop-sw
      *
      *     if resp-normal or resp-dupkey
      *        perform until i-say-stop
      *           exec cics readnext
      *              dataset      ('ELCERT5')
      *              ridfld       (ws-cm5-key)
      *              into         (certificate-master)
      *              resp         (ws-response)
      *           end-exec
      *           if (resp-normal or resp-dupkey)
      *              and (ws-cm5-key = ws-cm5-compare-key)
      *              display ' found cert ' cm-cert-no ' ' cm-state
      *                 ' ' cm-account ' ' cm-insured-last-name
      *              move cm-control-primary  to ws-cs-key
      *              move 'C'                 to ws-cs-trlr-type
      *              exec cics read
      *                 dataset      ('ELCRTT')
      *                 ridfld       (ws-cs-key)
      *                 into         (certificate-trailers)
      *                 resp         (ws-response)
      *              end-exec
      *              if resp-normal
      *                 if rate-vin = cs-vin-number
      *                    move 'Y'           to cp-return-code
      *                    set i-say-stop to true
      *                 end-if
      *              end-if
      *           else
      *              set i-say-stop to true
      *           end-if
      *        end-perform
      *        exec cics endbr
      *           dataset ('ELCERT5')
      *        end-exec
      *     else
      *        display ' bad resp on startbr ' ws-response
      *     end-if
      *
      *     if resp-normal
      *        move cm-control-primary  to ws-cs-key
      *        move 'C'                 to ws-cs-trlr-type
      *        exec cics read
      *           dataset      ('ELCRTT')
      *           ridfld       (ws-cs-key)
      *           into         (certificate-trailers)
      *           resp         (ws-response)
      *        end-exec
      *        if resp-normal
      *           if rate-vin = cs-vin-number
      *              move 'Y'           to cp-return-code
      *           end-if
      *        end-if
      *     end-if

           .
       8000-exit.
           exit.

       9700-DATE-LINK.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.

