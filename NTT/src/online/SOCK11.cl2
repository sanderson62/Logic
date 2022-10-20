      *$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK11.
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

080818******************************************************************
080818*REMARKS.                                                        *
080818*  This program gets kicked off by C# APP SpecAcctHandling via   *
080818*     Transaction SO11. Pulls all table rows from table          *
080818*     RefSpecHand in the Logic DB into an array. Next reads the  *
080818*     account master (ERACCT) one record at a time, compares to  *
080818*     all entries in array and updates the ERACCT accordingly.   *
080818******************************************************************
080818*                   C H A N G E   L O G
080818*
080818* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080818*-----------------------------------------------------------------
080818*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080818* EFFECTIVE    NUMBER
080818*-----------------------------------------------------------------
080818* 080818   2018040600002   PEMA  New Program
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
080818******************************************************************
       ENVIRONMENT DIVISION.
       data division.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SOCK11   WORKING STORAGE     '.
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
       77 X1                           PIC S999 COMP-3 VALUE +0.
       77 S1                           PIC S999 COMP-3 VALUE +0.
       77 S2                           PIC S999 COMP-3 VALUE +0.
       77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  ws-socket-sw                pic x value ' '.
           88  end-of-socket              value 'Y'.
       77  ws-browse-sw                pic x value ' '.
           88  browse-started            value 'Y'.
       77  ws-connect-sw               pic x value ' '.
           88  connected-to-db           value 'Y'.
       77  ws-bin-current-dt           pic xx value low-values.
       77  WS-MATCH-SW                 PIC X value ' '.
           88  found-match               value 'Y'.
       77  ws-acct-reads               pic 9(7) value zeros.
       77  ws-acnt-reads               pic 9(7) value zeros.
       77  ws-acnt-updates             pic 9(7) value zeros.
       77  ws-acnt-deletes             pic 9(7) value zeros.
       77  ws-batch-job-sw             pic x value spaces.
           88  batch-job                  value 'Y'.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.

       01  ws-save-report-code-1       pic x(10) value spaces.
       01  ws-save-report-code-2       pic x(10) value spaces.
       01  ws-save-report-code-3       pic x(10) value spaces.
       01  ws-save-user-select-2       pic x(10) value spaces.
       01  ws-save-user-select-5       pic x(10) value spaces.

       01  ws-save-eracct-key.
           05  ws-save-comp-cd         pic x.
           05  ws-save-carrier         pic x.
           05  f                       pic x(6).
           05  ws-save-state           pic xx.
           05  ws-save-account         pic x(10).

       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  ws-error-message            pic x(50) value spaces.
       01  ws-status-date              pic x(10).
       01  sqlcmd                      pic x(1024).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
       01  ws-display-response         pic s9(9) value zeros.

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
           05  nu-report-code1         pic s9(4) comp value +0.
           05  nu-report-code2         pic s9(4) comp value +0.
           05  nu-report-code3         pic s9(4) comp value +0.
           05  nu-user-select2         pic s9(4) comp value +0.
           05  nu-user-select5         pic s9(4) comp value +0.
           05  nu-carrier              pic s9(4) comp value +0.
           05  nu-state                pic s9(4) comp value +0.
           05  nu-account              pic s9(4) comp value +0.
           05  nu-status               pic s9(4) comp value +0.

       01  table-row.
           05  tr-report-code1         pic x(10).
           05  tr-report-code2         pic x(10).
           05  tr-report-code3         pic x(10).
           05  tr-user-select2         pic x(10).
           05  tr-user-select5         pic x(10).
           05  tr-carrier              pic x.
           05  tr-state                pic xx.
           05  tr-account              pic x(10).
           05  tr-status               pic x.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  a1                          pic s9(5) comp-3 value +0.
       01  ma1                         pic s9(5) comp-3 value +0.
       01  sql-table.
           05  sql-table-array occurs 3000.
               10  a1-report-code1     pic x(10).
               10  a1-report-code2     pic x(10).
               10  a1-report-code3     pic x(10).
               10  a1-user-select2     pic x(10).
               10  a1-user-select5     pic x(10).
               10  a1-carrier          pic x.
               10  a1-state            pic xx.
               10  a1-account          pic x(10).
               10  a1-status           pic x.

       01  soc-client-in-data          pic x(50).

       01  WS-AM-KEY.
           05  WS-AM-COMPANY-CD        PIC X.                                       
           05  WS-AM-CARRIER           PIC X.                                       
           05  WS-AM-GROUP             PIC X(6).                                    
           05  WS-AM-STATE             PIC XX.   
           05  WS-AM-ACCOUNT           PIC X(10).
           05  WS-AM-EXP-DT            PIC XX.
           05  FILLER                  PIC X(4).

       01  WS-NT-KEY.
           05  WS-NT-COMPANY-CD        PIC X.                                       
           05  WS-NT-CARRIER           PIC X.                                       
           05  WS-NT-GROUP             PIC X(6).                                    
           05  WS-NT-STATE             PIC XX.   
           05  WS-NT-ACCOUNT           PIC X(10).
           05  WS-NT-rec-type          PIC X.
           05  ws-nt-seq-no            pic s9(4) comp.

       01  ws-return-string.
           05  ws-return-error-no      pic x(4).
           05  ws-sc1                  pic x.
           05  ws-return-error-mess    pic x(45).
           05  ws-sc2                  pic x.
           05  ws-return-accts-read    pic zzzzzz9.
           05  ws-sc3                  pic x.
           05  ws-return-acnts-read    pic zzzzzz9.
           05  ws-sc4                  pic x.
           05  ws-return-acnts-updated pic zzzzzz9.
           05  ws-sc5                  pic x.
           05  ws-return-acnts-deletes pic zzzzzz9.

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
                                        copy ERCACNT.
                                        COPY ELCFUNDT.
                                        COPY ELCDATE.
      
       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

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

           display 'SOCK11:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK11:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK11:socket name      =', lstn-name ' '
              lstn-subname

           perform 0000-init           thru 0000-exit
           perform 0010-init-contact   thru 0010-exit
      *    perform 0020-receive        thru 0020-exit
      *    if end-of-socket
      *       display ' Premature End of Socket '
      *       go to 0300-close-socket
      *    end-if
           perform 0030-build-array-from-sql
                                       thru 0030-exit
           perform 0040-process-eracct thru 0040-exit
      *    display ' acct reads   ' ws-acct-reads
      *    display ' acnt reads   ' ws-acnt-reads
      *    display ' acnt updates ' ws-acnt-updates
      *    display ' acnt deletes ' ws-acnt-deletes
           perform 0070-format-buffer  thru 0070-exit
           perform 0200-send-buffer    thru 0200-exit

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

      *    display ' current date/time '
      *       ws-fn-mo '/' ws-fn-da '/' ws-fn-ccyy ' - '
      *          ws-fn-hours ':' ws-fn-minutes ':' ws-fn-seconds

           move spaces                 to sql-table
                                          ws-return-string
           move ';'                    to ws-sc1
                                          ws-sc2
                                          ws-sc3
                                          ws-sc4
                                          ws-sc5
           move zeros                  to ws-return-accts-read
                                          ws-return-acnts-read
                                          ws-return-acnts-updated
                                          ws-return-acnts-deletes

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

           if client-kick-off = 'SOCKET11'
              continue
           else
      *       display ' Must be from batch job '
              set batch-job to true
      *       move '9999'              to ws-return-error-no
      *       move 'Unknown origin, who are you? '
      *                                to ws-return-error-mess
      *       PERFORM 0200-SEND-BUFFER thru 0200-exit
      *       display ' Unknown origin ' client-in-data
      *       go to 0300-close-socket
           end-if

           MOVE X'04'                  TO WS-COMP-CD
           MOVE 'CID'                  TO WS-COMP-ID

      *    move 'SOCKET11READY'        to ws-send-buf
      *    move +25                    to ws-send-msg-size
      *
      *    display 'SOCK11:sequence number  =', ws-seq-num.
      *    display 'SOCK11:send buffer      =', ws-send-buf(1:25)
      *
      *    call "send" using by value GIVE-TAKE-SOCKET,
      *        by reference ws-send-buf,
      *        by value ws-send-msg-size,
      *        by value ws-flags
      *
      *    if return-code <= zero
      *       display 'SOCK11:send error ' return-code
      *       perform 0250-socket-error thru 0250-exit
      *       go to 0300-close-socket
      *    end-if

           .
       0010-exit.
           exit.

       0020-receive.

      *    display 'SOCK11:About to recv '

           call "recv" using by value GIVE-TAKE-SOCKET
               by reference ws-recv-buf
               by value ws-recv-msg-size
               by value ws-flags.

           if return-code < zero
              display 'SOCK11:recv error ' return-code
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0010-exit
           end-if

           if return-code = zero
              display 'SOCK11:client disconnected',
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0010-exit
           end-if

      *    display 'SOCK11:Good recv  '
      *    display 'SOCK11:return code      = ', return-code
      *    display 'SOCK11:receive buffer   = ', ws-recv-buf(1:51)

           move +50                    to ws-send-msg-size

           move ws-recv-buf (1:50)     to soc-client-in-data

           if ws-recv-buf (1:4) = 'DONE' or 'done' or 'Done'
              set end-of-socket to true
           end-if

           .
       0020-exit.
           exit.

       0030-build-array-from-sql.

           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if

           EXEC SQL
              DECLARE
                 getSpecInstr cursor for
              SELECT
                 ReportCode1,
                 ReportCode2,
                 ReportCode3,
                 UserSelect2,
                 UserSelect5,
                 Carrier,
                 State,
                 Account,
                 Status
              FROM
                 RefSpecHand
           end-exec

           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move sqlcode             to ws-display-response
              move '9999'              to ws-return-error-no
              move 'Could not declare cursor'
                                       to ws-return-error-mess
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              go to 0300-close-socket
           end-if

           EXEC SQL
              open getSpecInstr
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move '9999'              to ws-return-error-no
              move 'Could not Open cursor'
                                       to ws-return-error-mess
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              go to 0300-close-socket
           end-if

           perform until sqlcode not = 0
              EXEC SQL
                 fetch getSpecInstr into
                    :tr-report-code1  :nu-report-code1,
                    :tr-report-code2  :nu-report-code2,
                    :tr-report-code3  :nu-report-code3,
                    :tr-user-select2  :nu-user-select2,
                    :tr-user-select5  :nu-user-select5,
                    :tr-carrier       :nu-carrier,
                    :tr-state         :nu-state,
                    :tr-account       :nu-account,
                    :tr-status        :nu-status
              END-EXEC

      *       display ' rpt cd1 ' tr-report-code1 ' ' nu-report-code1
      *       display ' rpt cd2 ' tr-report-code2 ' ' nu-report-code2
      *       display ' rpt cd3 ' tr-report-code3 ' ' nu-report-code3
      *       display ' account ' tr-account ' ' nu-account

              if sqlcode = 0
                 add +1 to a1
                 if nu-report-code1 = +0
                    move tr-report-code1 to a1-report-code1(a1)
                 end-if
                 if nu-report-code2 = +0
                    move tr-report-code2 to a1-report-code2(a1)
                 end-if
                 if nu-report-code3 = +0
                    move tr-report-code3 to a1-report-code3(a1)
                 end-if
                 if nu-user-select2 = +0
                    move tr-user-select2 to a1-user-select2(a1)
                 end-if
                 if nu-user-select5 = +0
                    move tr-user-select5 to a1-user-select5(a1)
                 end-if
                 if nu-carrier = +0
                    move tr-carrier      to a1-carrier(a1)
                 end-if
                 if nu-state = +0
                    move tr-state        to a1-state(a1)
                 end-if
                 if nu-account = +0
                    move tr-account      to a1-account(a1)
                 end-if
                 if nu-status = +0
                    move tr-status       to a1-status(a1)
                 end-if
              else
                 if sqlcode not = 0 and 100
                    display "Error: cannot fetch row " a1
                    display ' sql return code ' sqlcode
                    display ' sql err mess    ' sqlerrmc
                    move '9999'        to ws-return-error-no
                    move 'Could not fetch rows'
                                       to ws-return-error-mess
                    PERFORM 0200-SEND-BUFFER thru 0200-exit
                    go to 0300-close-socket
                 end-if
              end-if
           end-perform

           EXEC SQL
               close getSpecInstr
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           move a1                     to ma1
      *    display ' loaded ' a1 ' rows to table '

           if connected-to-db
              EXEC SQL
                  disconnect all
              END-EXEC
              move ' '                 to ws-connect-sw
           end-if

      *    perform varying a1 from +1 by +1 until a1 > ma1
      *       display ' rpt cd1 ' a1-report-code1(a1)
      *       display ' rpt cd2 ' a1-report-code2(a1)
      *       display ' rpt cd3 ' a1-report-code3(a1)
      *       display '     us2 ' a1-user-select2(a1)
      *       display '     us5 ' a1-user-select5(a1)
      *       display '   carr  ' a1-carrier(a1)
      *       display '  state  ' a1-state(a1)
      *       display ' account ' a1-account(a1)
      *       display ' status  ' a1-status(a1)
      *    end-perform

           .
       0030-exit.
           exit.

       0040-process-eracct.

           move X'04'                  to ws-am-key

           exec cics startbr
              dataset       ('ERACCT')
              ridfld        (ws-am-key)
              resp          (ws-response)
              gteq
           end-exec

           if not resp-normal
              display 'error-eracct-startbr ' ws-response
              go to 0040-exit
           end-if

           exec cics readnext
              dataset       ('ERACCT')
              ridfld        (ws-am-key)
              into          (account-master)
              resp          (ws-response)
           end-exec

           if resp-normal
              move am-control-primary(1:20)
                                       to ws-save-eracct-key
              perform until end-of-eracct
                 perform 0050-check-table
                                       thru 0050-exit
      *          display ' key b4 readnext ' ws-am-key (2:19)
                 exec cics readnext
                    dataset    ('ERACCT')
                    ridfld     (ws-am-key)
                    into       (account-master)
                    resp       (ws-response)
                 end-exec
                 if (not resp-normal)
                    or (am-company-cd not = X'04')
                    set end-of-eracct to true
                 else
                    add 1 to ws-acct-reads
                 end-if
      *          if ws-acct-reads > +400
      *             set end-of-eracct to true
      *          end-if
              end-perform
              perform 0050-check-table thru 0050-exit
           end-if

           .
       0040-exit.
           exit.

       0050-check-table.

           if am-control-primary(1:20) = ws-save-eracct-key
              move am-report-code-1    to ws-save-report-code-1
              move am-report-code-2    to ws-save-report-code-2
              move am-report-code-3    to ws-save-report-code-3
              move am-user-select-2    to ws-save-user-select-2
              move am-user-select-5    to ws-save-user-select-5
              go to 0050-exit
           end-if

           move ' '                    to ws-match-sw
       
           perform varying a1 from +1 by +1 until
              (a1 > ma1)
              or (found-match)
      **      display ' array values ' sql-table-array(a1) ' ' a1
       
              if ((a1-report-code1(a1) = spaces)
                 or (a1-report-code1(a1) = ws-save-report-code-1))
                            and
                 ((a1-report-code2(a1) = spaces)
                 or (a1-report-code2(a1) = ws-save-report-code-2))
                            and
                 ((a1-report-code3(a1) = spaces)
                 or (a1-report-code3(a1) = ws-save-report-code-3))
                            and
                 ((a1-user-select2(a1) = spaces)
                 or (a1-user-select2(a1) = ws-save-user-select-2))
                            and
                 ((a1-user-select5(a1) = spaces)
                 or (a1-user-select5(a1) = ws-save-user-select-5))
                            and
                 ((a1-carrier(a1) = spaces)
                 or (a1-carrier(a1) = ws-save-carrier))
                            and
                 ((a1-state(a1) = spaces)
                 or (a1-state(a1) = ws-save-state))
                            and
                 ((a1-account(a1) = spaces)
                 or (a1-account(a1) = ws-save-account))
                 perform 0060-update-eracnt
                                       thru 0060-exit
                 set found-match to true
      *          move high-values      to ws-am-exp-dt
              end-if
           end-perform
       
           if not found-match  *> may have been a delete
              move ws-save-eracct-key  to ws-nt-key
              move '2'                 to ws-nt-rec-type
              move +3                  to ws-nt-seq-no
              perform 0064-read-eracnt-update
                                       thru 0064-exit
              if resp-normal  *>  found a note
                 if nt-account-special <> 'Y'
                    perform 0061-unlock-eracnt
                                       thru 0061-exit
                 else  *> need to remove setting
                    move ' '           to nt-account-special
                    perform 0063-rewrite-eracnt
                                       thru 0063-exit
                    if not resp-normal
                       display 'error-rewrite-eracnt-D ' ws-response
                          ' ' am-account
                    else
                       add 1 to ws-acnt-deletes
      *                display ' Updated note D' am-state ' '
      *                   am-account
                    end-if
                 end-if
              end-if
           end-if

           move am-control-primary(1:20)
                                       to ws-save-eracct-key
           move am-report-code-1       to ws-save-report-code-1
           move am-report-code-2       to ws-save-report-code-2
           move am-report-code-3       to ws-save-report-code-3
           move am-user-select-2       to ws-save-user-select-2
           move am-user-select-5       to ws-save-user-select-5

      *    move ' '                    to ws-match-sw
      *
      *    perform varying a1 from +1 by +1 until
      *       (a1 > ma1)
      *       or (found-match)
      **      display ' array values ' sql-table-array(a1) ' ' a1
      *
      *       if ((a1-report-code1(a1) = spaces)
      *          or (a1-report-code1(a1) = am-report-code-1))
      *                     and
      *          ((a1-report-code2(a1) = spaces)
      *          or (a1-report-code2(a1) = am-report-code-2))
      *                     and
      *          ((a1-report-code3(a1) = spaces)
      *          or (a1-report-code3(a1) = am-report-code-3))
      *                     and
      *          ((a1-user-select2(a1) = spaces)
      *          or (a1-user-select2(a1) = am-user-select-2))
      *                     and
      *          ((a1-user-select5(a1) = spaces)
      *          or (a1-user-select5(a1) = am-user-select-5))
      *                     and
      *          ((a1-carrier(a1) = spaces)
      *          or (a1-carrier(a1) = am-carrier))
      *                     and
      *          ((a1-state(a1) = spaces)
      *          or (a1-state(a1) = am-state))
      *                     and
      *          ((a1-account(a1) = spaces)
      *          or (a1-account(a1) = am-account))
      *          perform 0060-update-eracnt
      *                                thru 0060-exit
      *          set found-match to true
      *          move high-values      to ws-am-exp-dt
      *       end-if
      *    end-perform
      *
      *    if not found-match  *> may have been a delete
      *       move ws-am-key(1:20)     to ws-nt-key
      *       move '2'                 to ws-nt-rec-type
      *       move +3                  to ws-nt-seq-no
      *       perform 0064-read-eracnt-update
      *                                thru 0064-exit
      *       if resp-normal  *>  found a note
      *          if nt-account-special <> 'Y'
      *             perform 0061-unlock-eracnt
      *                                thru 0061-exit
      *          else  *> need to remove setting
      *             move ' '           to nt-account-special
      *             perform 0063-rewrite-eracnt
      *                                thru 0063-exit
      *             if not resp-normal
      *                display 'error-rewrite-eracnt-D ' ws-response
      *                   ' ' am-account
      *             else
      *                add 1 to ws-acnt-deletes
      *                display ' Updated note D' am-state ' '
      *                   am-account
      *             end-if
      *          end-if
      *       end-if
      *    end-if

           .
       0050-exit.
           exit.

       0060-update-eracnt.
           if sql-table-array(a1) = spaces
              display ' bypass 0060, all spaces '
              go to 0060-exit
           end-if
      *    display ' made it to 0060 ' ws-save-state ' '
      *       ws-save-account
      *    move ws-am-key(1:20)        to ws-nt-key
           move ws-save-eracct-key     to ws-nt-key
           move '2'                    to ws-nt-rec-type
           move +3                     to ws-nt-seq-no
           perform 0064-read-eracnt-update
                                       thru 0064-exit

           if resp-normal     *>  Found a note
      *       display ' found note ' ws-save-state ' ' ws-save-account
              add 1 to ws-acnt-reads
              if a1-status(a1) = 'A'  *> special instr active
                 if nt-account-special = 'Y' *> note already set to Y
                                             *> so go on about ur bus
                    perform 0061-unlock-eracnt
                                       thru 0061-exit
                 else  *> note not active so set it to active
                    move 'Y'           to nt-account-special
                    perform 0063-rewrite-eracnt
                                       thru 0063-exit
                    if not resp-normal
                       display 'error-rewrite-eracnt-A ' ws-response
                          ' ' ws-save-account
                    else
                       add 1 to ws-acnt-updates
      *                display ' Updated note A ' ws-save-state ' '
      *                   ws-save-account ' ' sql-table-array(a1)
                    end-if
                 end-if
              else  *> special instr inactive
                 if nt-account-special <> 'Y' *> note inactive
                                              *> so go on about ur bus
                    perform 0061-unlock-eracnt
                                       thru 0061-exit
                 else *> note active so set it to inactive
                    move ' '           to nt-account-special
                    perform 0063-rewrite-eracnt
                                       thru 0063-exit
                    if not resp-normal
                       display 'error-rewrite-eracnt-I ' ws-response
                          ' ' ws-save-account
                    else
                       add 1 to ws-acnt-updates
      *                display ' Updated note I ' ws-save-state ' '
      *                   ws-save-account ' ' sql-table-array(a1)
                    end-if
                 end-if
              end-if
           else
              if resp-notfnd  *> no note record
                 if a1-status(a1) = 'A'  *> spec instr is active
                    move 'NT'             to note-file
                    move ws-save-eracct-key
                                       to nt-control-primary
                    move '2'           to nt-record-type
                    move +3            to nt-line-sequence
                    move 'Y'           to nt-account-special
                    perform 0062-write-eracnt
                                       thru 0062-exit
                    if not resp-normal
                       display 'error-write-eracnt ' ws-response
                    else
                       add 1 to ws-acnt-updates
      *                display ' Add note ' ws-save-state ' '
      *                   ws-save-account ' ' sql-table-array(a1)
                    end-if
                 end-if
              end-if
           end-if

           .
       0060-exit.
           exit.

       0061-unlock-eracnt.

           exec cics unlock
              dataset   ('ERACNT')
           end-exec

           .
       0061-exit.
           exit.

       0062-write-eracnt.

           exec cics write
              dataset ('ERACNT')
              from    (note-file)
              ridfld  (nt-control-primary)
              resp    (ws-response)
           end-exec

           .
       0062-exit.
           exit.

       0063-rewrite-eracnt.

           exec cics rewrite
              dataset    ('ERACNT')
              from       (note-file)
              resp       (ws-response)
           end-exec

           .
       0063-exit.
           exit.

       0064-read-eracnt-update.

           exec cics read
              update
              dataset  ('ERACNT')
              RIDFLD   (WS-NT-KEY)
              into     (note-file)
              resp     (ws-response)
           end-exec

           .
       0064-exit.
           exit.

       0070-format-buffer.

           move '0000'                 to ws-return-error-no
           move 'Logic Updated completed successful '
                                       to ws-return-error-mess
           move ws-acct-reads          to ws-return-accts-read
           move ws-acnt-reads          to ws-return-acnts-read
           move ws-acnt-updates        to ws-return-acnts-updated
           move ws-acnt-deletes        to ws-return-acnts-deletes

           .
       0070-exit.
           exit.

       0200-send-buffer.

           if batch-job
              go to 0200-exit
           end-if

           move ws-return-string       to ws-send-buf
           display 'sock11:About to send      ' ws-send-buf
           display 'sock11:sequence number  =', ws-seq-num.
           display ' msg size ' ws-send-msg-size

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.

           if return-code <= zero
              display 'sock11:send error ',
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if

           .
       0200-exit.
           exit.

       0250-socket-error.

           display "sock11:did not complete"
           display 'sock11:transaction data =', CLIENT-IN-DATA '**'
           display 'sock11:socket number    =', GIVE-TAKE-SOCKET.
           display 'sock11:socket name      =', lstn-name ' '
              lstn-subname
           display ' return code = ' return-code

           .
       0250-exit.
           exit.

       0300-close-socket.

      *    call "close" using by value GIVE-TAKE-SOCKET .
           display 'sock11:done'
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

      *     EXEC SQL
NTTDel**       CONNECT TO :svr USER :usr-pass
NTTIns*        CONNECT TO :svr
NTTIns*          USER     :usr
NTTIns*          USING    :pass
      *     END-EXEC
       
           if sqlcode not = 0
              display "Error: cannot connect to " svr
              display sqlcode
              display sqlerrmc
           else
      *       display ' Successful Connect ' sqlcode
              set connected-to-db to true
           end-if

           .
       6000-EXIT.
           EXIT.

       9700-DATE-LINK.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.
