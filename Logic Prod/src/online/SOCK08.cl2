      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK08.
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
020617*  Broker program for webservices for premium rating.            *
020617******************************************************************
020617*                   C H A N G E   L O G
020617*
020617* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020617*-----------------------------------------------------------------
020617*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020617* EFFECTIVE    NUMBER
020617*-----------------------------------------------------------------
020617* 020617   2017020300002   PEMA  New Program version 2 of SOCK07
100917* 100917   2017020300002   PEMA  Add message003
061515******************************************************************
       ENVIRONMENT DIVISION.
       data division.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SOCK08   WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
      *
      * program buffers
      *
100917 77 ws-send-msg-size             pic s9(8) comp value 512.
       77 ws-recv-msg-size             pic s9(8) comp value 256.
       77 ws-recv-buf                  pic x(256).
100917 77 ws-send-buf                  pic x(512) VALUE SPACES.
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
       77  b1                          pic s999 comp-3 value +0.
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

       01  a-angle-n                   pic s9(7)v9(11) comp-3 value +0.
       01  a-angle-n-m                 pic s9(7)v9(11) comp-3 value +0.
       01  a-prm-angle-n               pic s9(7)v9(11) comp-3 value +0.
       01  a-prm-angle-n-m             pic s9(7)v9(11) comp-3 value +0.
       01  gamma                       pic s9(7)v9(11) comp-3 value +0.
       01  temp-value-1                pic s9(7)v9(11) comp-3 value +0.
       01  temp-value-2                pic s9(7)v9(11) comp-3 value +0.
       01  temp-value-3                pic s9(7)v9(11) comp-3 value +0.
       01  temp-value-4                pic s9(7)v9(11) comp-3 value +0.
       01  temp-value-5                pic s9(7)v9(11) comp-3 value +0.
       01  temp-value-6                pic s9(7)v9(11) comp-3 value +0.
       01  m                           pic s9(4)v9(4)  comp-3 value +0.
       01  n                           pic s9(4)v9(4)  comp-3 value +0.
       01  i                           pic s9(7)v9(11) comp-3 value +0.
       01  ppy                         pic s9(5)       comp-3 value +0.
       01  dpp                         pic s9(5)       comp-3 value +0.

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

       01  soc-client-in-data.
           05  message-queuename       pic x(10).
           05  message-area            pic x(1024).



       01  WS-CID-NO                   PIC X(8).

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  resp-duprec                  value +14.
           88  resp-dupkey                  value +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.


       01  ws-return-string            pic x(512).
       01  ws-mess001-length           pic s9(4) comp value +1024.
       01  WS-MESS001-PASS-AREA        PIC X(1024).
       01  ws-mess002-length           pic s9(4) comp value +1024.
       01  WS-MESS002-PASS-AREA        PIC X(1024).
100917 01  ws-mess003-length           pic s9(4) comp value +1024.
100917 01  WS-MESS003-PASS-AREA        PIC X(1024).

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

           display ' Entering program SOCK08.cl2 '
      *    display 'SOCK08:transaction data =', CLIENT-IN-DATA '**'
      *    display 'SOCK08:socket number    =', GIVE-TAKE-SOCKET.
      *    display 'SOCK08:socket name      =', lstn-name ' '
      *       lstn-subname

           perform 0000-init           thru 0000-exit

           perform 0010-init-contact   thru 0010-exit
           perform 0100-process-socket thru 0100-exit until
              end-of-socket

           go to 0300-close-socket

           .
       0000-init.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           move ws-fn-cymd             to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-current-dt
           else
              display ' error current dt invalid ' dc-error-code
           end-if

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

           .
       0000-exit.
           exit.

       0010-init-contact.

           if client-kick-off = 'SOCKET08'
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

           move 'SOCKET08READY'        to ws-send-buf
           move +25                    to ws-send-msg-size

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags

           if return-code <= zero
              display 'SOCK08:send error ' return-code
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if

           .
       0010-exit.
           exit.

       0100-process-socket.

           perform 0110-receive        thru 0110-exit
           if end-of-socket
              go to 0100-exit
           end-if

           evaluate message-queuename
           
              when 'message001'
                 perform 1000-link-mess001
                                       thru 1000-exit
                 move ws-mess001-pass-area
                                       to ws-return-string
              when 'message002'
                 perform 2000-link-mess002
                                       thru 2000-exit
                 move ws-mess002-pass-area
                                       to ws-return-string
100917        when 'message003'
100917           perform 3000-link-mess003
100917                                 thru 3000-exit
100917           move ws-mess003-pass-area
100917                                 to ws-return-string
              when other
                 move 'Invalid message number '
                                       to ws-return-string
                 set end-of-socket to true
           end-evaluate

           perform 0200-send-buffer    thru 0200-exit

           .
       0100-exit.
           exit.

       0110-receive.

100917     move spaces                 to ws-recv-buf

           call "recv" using by value GIVE-TAKE-SOCKET
               by reference ws-recv-buf
               by value ws-recv-msg-size
               by value ws-flags.

           if return-code < zero
              display 'SOCK08:recv error ' return-code
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0110-exit
           end-if

           if return-code = zero
              display 'SOCK08:client disconnected',
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0110-exit
           end-if

100917     move +512                   to ws-send-msg-size

           move ws-recv-buf (1:170)     to soc-client-in-data

           if ws-recv-buf (1:4) = 'DONE' or 'done' or 'Done'
              set end-of-socket to true
           end-if

           .
       0110-exit.
           exit.

       0200-send-buffer.

           move ws-return-string       to ws-send-buf

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.

           if return-code <= zero
              display 'SOCK08:send error ',
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if

           .
       0200-exit.
           exit.

       0250-socket-error.

           display "SOCK08:did not complete"
           display 'SOCK08:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK08:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK08:socket name      =', lstn-name ' '
              lstn-subname
           display ' return code = ' return-code

           .
       0250-exit.
           exit.

       0300-close-socket.

           exec cics return end-exec.
           goback.

           .
       0300-exit.
           exit.

       1000-link-mess001.

           move soc-client-in-data     to ws-mess001-pass-area

           EXEC CICS LINK
                PROGRAM  ('WSMESS01')
                COMMAREA (WS-MESS001-PASS-AREA)
                LENGTH   (ws-mess001-length)
           END-EXEC.

           .
       1000-exit.
           exit.

       2000-link-mess002.

           move soc-client-in-data     to ws-mess002-pass-area

           EXEC CICS LINK
                PROGRAM  ('WSMESS02')
                COMMAREA (WS-MESS002-PASS-AREA)
                LENGTH   (ws-mess002-length)
           END-EXEC.

           .
       2000-exit.
           exit.

100917 3000-link-mess003.
100917
100917     move soc-client-in-data     to ws-mess003-pass-area
100917
100917     EXEC CICS LINK
100917          PROGRAM  ('WSMESS03')
100917          COMMAREA (WS-MESS003-PASS-AREA)
100917          LENGTH   (ws-mess003-length)
100917     END-EXEC.
100917
100917     .
100917 3000-exit.
100917     exit.

       9700-DATE-LINK.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.

