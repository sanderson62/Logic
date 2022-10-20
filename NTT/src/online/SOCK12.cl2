       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK12.
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

110918******************************************************************
110918*REMARKS.                                                        *
110918*  This program gets kicked off by C# APP EmployeeManagement via *
110918*     Transaction SO12. Reads all control file user records      *
110918*     and sets the credit and claim access to "N" on all         *
110918*     companies. The C# app updates the sql tables on Logic.     *
110918*                                                                *
110918******************************************************************
110918*                   C H A N G E   L O G
110918*
110918* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
110918*-----------------------------------------------------------------
110918*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
110918* EFFECTIVE    NUMBER
110918*-----------------------------------------------------------------
110918* 110918 CR2018050300001   PEMA  New Program
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
110918******************************************************************
       ENVIRONMENT DIVISION.
       data division.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SOCK12   WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
      *
      * program buffers
      *
       77  ws-send-msg-size            pic s9(8) comp value 256.
       77  ws-recv-msg-size            pic s9(8) comp value 256.
       77  ws-recv-buf                 pic x(256).
       77  ws-send-buf                 pic x(256) VALUE SPACES.
       77  ws-recv-total               pic s9(8) comp value 0.
       77  ws-recv-left                pic s9(8) comp value 0.
       77  ws-seq-num                  pic s9(8) comp value 0.
       77  ws-flags                    pic s9(8) comp value 0.
       77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
       77  ws-comp-id                  pic xxx value spaces.
       77  WS-USER-ID                  PIC XXXX VALUE SPACES.
       77  X1                          PIC S999 COMP-3 VALUE +0.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  S2                          PIC S999 COMP-3 VALUE +0.
       77  ws-eof-sw                   pic x value ' '.
           88  end-of-input               value 'Y'.
       77  WS-DIS-RESP                 PIC 9(05) VALUE ZEROS.
       77  ws-socket-sw                pic x value ' '.
           88  end-of-socket              value 'Y'.
       77  ws-browse-sw                pic x value ' '.
           88  browse-started            value 'Y'.
       77  ws-connect-sw               pic x value ' '.
           88  connected-to-db           value 'Y'.
       77  ws-bin-current-dt           pic xx value low-values.
       77  WS-MATCH-SW                 PIC X value ' '.
           88  found-match               value 'Y'.
       77  ws-logic-cnt                pic 9(5) value zeros.
       77  ws-sql-cnt                  pic 9(5) value zeros.

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

       01  elcntl-key.
           05  ws-cf-company-id        pic xxx.
           05  ws-cf-record-type       pic x.
           05  ws-cf-access-cd         pic xxxx.
           05  ws-cf-sequence-no       pic s9(4) comp value +0.

       01  ws-return-string.
           05  ws-return-error-no      pic x(4).
           05  ws-sc1                  pic x.
           05  ws-return-error-mess    pic x(45).
           05  ws-sc2                  pic x.
           05  ws-return-logic-cnt     pic zzzzzz9.
           05  ws-sc3                  pic x.
           05  ws-return-sql-cnt       pic zzzzzz9.

       01  filler.
           05  ws-function-time.
               10  ws-func-hh          pic 99.
               10  ws-func-mm          pic 99.
               10  ws-func-ss          pic 99.
           05  ws-function-time-n redefines
               ws-function-time        pic 9(6).

       01  WS-CID-NO                   PIC X(8).

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  resp-duprec                  value +14.
           88  resp-dupkey                  value +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

                                        COPY ELCCNTL.
                                        COPY ELCFUNDT.
                                        COPY ELCDATE.

       linkage section.
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).
      ****   client-in-data must be 36 characters.  ****
         05 CLIENT-IN-DATA.
            15  CLIENT-KICK-OFF      PIC X(11).
            15  CLIENT-USER-ID       PIC XXXX.
            15  client-maint-user    pic xxxx.
            15  FILLER               PIC X(17).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).

       01  var  pic x(30).

       procedure division.

           display 'SOCK12:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK12:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK12:socket name      =', lstn-name ' '
              lstn-subname

           perform 0000-init           thru 0000-exit
           perform 0010-init-contact   thru 0010-exit
           perform 0020-process-elcntl thru 0020-exit until
              end-of-input

           display ' logic count ' ws-logic-cnt

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

           move ws-fn-hours            to ws-func-hh
           move ws-fn-minutes          to ws-func-mm
           move ws-fn-seconds          to ws-func-ss          

           move spaces                 to sql-table
                                          ws-return-string
           move ';'                    to ws-sc1
                                          ws-sc2
                                          ws-sc3
           move zeros                  to ws-return-logic-cnt
                                          ws-return-sql-cnt

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

           if client-kick-off = 'DISABLEUSER'
              continue
           else
              move '9999'              to ws-return-error-no
              move ' unknown origin '  to ws-return-error-mess
              display ' UNKNOWN ORIGIN '
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-if

           if client-user-id = spaces or low-values
              move '9999'              to ws-return-error-no
              move ' Invalid user id'  to ws-return-error-mess
              display ' Invalid user id '
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-if

           MOVE CLIENT-USER-ID         TO WS-USER-ID
           MOVE spaces                 TO WS-COMP-ID

           display 'SOCK12:sequence number  =', ws-seq-num.
           display 'SOCK12:send buffer      =', ws-send-buf(1:25)

           .
       0010-exit.
           exit.

       0020-process-elcntl.

           move spaces                 to elcntl-key

           exec cics startbr
              dataset      ('ELCNTL')
              ridfld       (elcntl-key)
              gteq
              resp         (ws-response)
           end-exec

           if not resp-normal
              display ' bad start on elcntl ' ws-response
              move '9999'              to ws-return-error-no
              move ' Bad start on elcntl'
                                       to ws-return-error-mess
              perform 0200-send-buffer thru 0200-exit
              go to 0300-close-socket
           end-if

           .
       0020-readnext.

           exec cics readnext
              dataset     ('ELCNTL')
              ridfld      (elcntl-key)
              into        (control-file)
              resp        (ws-response)
           end-exec

           if resp-endfile or resp-notfnd
              set end-of-input to true
              exec cics endbr
                 dataset   ('ELCNTL')
              end-exec
              go to 0020-exit
           end-if

           if not resp-normal
              display ' bad readnext on elcntl ' ws-response
              go to 0020-exit
           end-if

           if cf-record-type = '2'
              if (cf-access-of-processor = ws-user-id)
                 and (cf-sequence-no = zeros)
                 perform 0025-update-elcntl
                                       thru 0025-exit
              end-if
           end-if

           go to 0020-readnext

           .
       0020-exit.
           exit.

       0025-update-elcntl.

           exec cics read update
              dataset     ('ELCNTL')
              ridfld      (elcntl-key)
              into        (control-file)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' bad read update on elcntl ' ws-response
              go to 0025-exit
           end-if

           move 'NNNN'                 to cf-proc-sys-access-all
           move client-maint-user      to cf-last-maint-by
           move ws-bin-current-dt      to cf-last-maint-dt
           move ws-function-time-n     to cf-last-maint-hhmmss           

           exec cics rewrite
              dataset      ('ELCNTL')
              from         (control-file)
              resp         (ws-response)
           end-exec

           display ' would have updated record ' cf-record-type ' '
              cf-access-of-processor ' ' cf-sequence-no ' '
              cf-proc-sys-access-all

           if not resp-normal
              display ' bad rewrite on elcntl ' ws-response
              go to 0025-exit
           end-if

           add 1 to ws-logic-cnt

           .
       0025-exit.
           exit.

       0070-format-buffer.

           move '0000'                 to ws-return-error-no
           move ' User successfully disabled  '
                                       to ws-return-error-mess
           move ws-logic-cnt           to ws-return-logic-cnt
           move ws-sql-cnt             to ws-return-sql-cnt

           .
       0070-exit.
           exit.

       0200-send-buffer.

           move ws-return-string       to ws-send-buf
           display 'SOCK12:About to send      ' ws-send-buf
           display 'SOCK12:sequence number  =', ws-seq-num.
           display ' msg size ' ws-send-msg-size

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.

           if return-code <= zero
              display 'SOCK12:send error ',
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if

           .
       0200-exit.
           exit.

       0250-socket-error.

           display "SOCK12:did not complete"
           display 'SOCK12:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK12:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK12:socket name      =', lstn-name ' '
              lstn-subname
           display ' return code = ' return-code

           .
       0250-exit.
           exit.

       0300-close-socket.

      *    call "close" using by value GIVE-TAKE-SOCKET .
           display 'SOCK12:done'
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

063022     MOVE 'TEST_Logic'           TO SVR
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022     if ws-kix-myenv = 'cid1p'
063022        MOVE 'PROD_Logic'        TO SVR
063022     end-if

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

      *    display ' About to connect to ' svr ' ' usr-pass

           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
       
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
