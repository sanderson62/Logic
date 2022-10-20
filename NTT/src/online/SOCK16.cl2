      *$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK16.
       AUTHOR.     Paul.
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
      
      ******************************************************************
      *REMARKS.                                                        *
      *       Receives a call from the GA withholding app and          *
      *   will update the ercomp table with the latest values based on *
      *   the key(s) passed to this program.                           *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 020119 CR2020060800001   PEMA  New Program
      ******************************************************************
       ENVIRONMENT DIVISION.
      
       DATA DIVISION.

       WORKING-STORAGE SECTION.
      *
      * program buffers
      *
       77 ws-send-msg-size             pic s9(8) comp value 4096.
       77 ws-recv-msg-size             pic s9(8) comp value 4096.
       77 ws-recv-buf                  pic x(4096).
       77 ws-send-buf                  pic x(4096) VALUE SPACES.
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
       77 S1                           PIC S999 COMP-3 VALUE +0.
       77 S2                           PIC S999 COMP-3 VALUE +0.
       77 WS-BUILD-SW                  PIC X.
          88  TIME-TO-BUILD               VALUE 'Y'.
       77 WS-SAVE-ERACCT               PIC X(2000).
       77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
       77 WS-PERFORM-SW                PIC X VALUE SPACES.
          88  GET-RATES                    VALUE 'R'.
          88  GET-ACT-ACCTS                VALUE 'A'.
       77  ws-bin-current-dt           pic xx value low-values.
       77 ws-bin-eff-dt                pic xx  value low-values.
       77 ws-bin-1st-pay-dt            pic xx  value low-values.
       77 WS-DISP-AMT                  PIC Z,ZZZ,Z99.99.
       77 ws-disp-rate                 pic z9.99999.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  WS-ERCTBL-SW                PIC X VALUE ' '.
           88  END-OF-ERCTBL                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       77  ws-connect-sw               pic x value ' '.
           88  connected-to-db           value 'Y'.
       77  ws-socket-sw                pic x value ' '.
           88  end-of-socket              value 'Y'.

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

NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       EXEC SQL                
          BEGIN DECLARE SECTION
       END-EXEC                

       01  sqlcmd                      pic x(1024).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
       01  ws-display-response         pic s9(9) value zeros.

       01  indicator-vaiables-for-nulls.
           05  nu-last-maint-dt        pic s9(4) comp value +0.
           05  nu-rolodex-print-dt     pic s9(4) comp value +0.
           05  nu-last-eom-stmt-dt     pic s9(4) comp value +0.
           05  nu-last-activity-date   pic s9(4) comp value +0.
           05  nu-last-stmt-dt         pic s9(4) comp value +0.
           05  nu-current-last-stmt-dt pic s9(4) comp value +0.
           05  nu-ga-effective-dt      pic s9(4) comp value +0.
           05  nu-ga-termination-dt    pic s9(4) comp value +0.
           05  nu-first-written-dt     pic s9(4) comp value +0.

       01  comp-record.
           12  cr-carrier                            pic x.
           12  cr-grouping                           pic x(6).
           12  cr-resp-no                            pic x(10).
           12  cr-account                            pic x(10).
           12  cr-type                               pic x.
           12  CR-LAST-MAINT-DT                      PIC x(10).
           12  CR-LAST-MAINT-HHMMSS                  PIC x(8).
           12  cr-last-maint-hhmmss-n redefines
               CR-LAST-MAINT-HHMMSS                  PIC -9(7).
           12  CR-LAST-MAINT-USER                    PIC X(4).          
           12  CR-STMT-TYPE                          PIC XXX.
           12  CR-COMP-TYPE                          PIC X.
           12  CR-STMT-OWNER                         PIC X(4).
           12  CR-BALANCE-CONTROL                    PIC X.             
           12  CR-INTERNAL-CONTROL-1                 PIC X.             
           12  CR-INTERNAL-CONTROL-2                 PIC X.             
           12  cr-ga-withold-pct                     pic x(7).
           12  CR-GA-WITHOLD-PCT-n redefines
               cr-ga-withold-pct                     PIC -9.9999.
           12  CR-GA-DIRECT-DEP                      PIC X.
           12  CR-ACCT-NAME                          PIC X(30).         
           12  CR-MAIL-NAME                          PIC X(30).         
           12  CR-ADDR-1                             PIC X(30).         
           12  CR-ADDR-2                             PIC X(30).         
           12  CR-ADDR-3                             pic x(29).
           12  CR-CSO-1099                           PIC X.             
           12  CR-ZIP                                pic x(9).
           12  CR-SOC-SEC                            PIC X(13).         
           12  CR-TELEPHONE                          pic x(10).
           12  CR-ROLODEX-PRINT-DT                   PIC X(10).
           12  CR-AR-BAL-LEVEL                       PIC X.             
           12  CR-AR-NORMAL-PRINT                    PIC X.             
           12  CR-AR-SUMMARY-CODE                    PIC X(6).          
           12  CR-AR-REPORTING                       PIC X.             
           12  CR-AR-PULL-CHECK                      PIC X.             
           12  CR-AR-BALANCE-PRINT                   PIC X.             
           12  CR-AR-LAST-RUN-CODE                   PIC X.             
           12  CR-LAST-EOM-STMT-DT                   PIC X(10).
           12  CR-USER-CODE                          PIC X.             
           12  CR-REPORT-GROUP-ID                    PIC X(12).         
           12  CR-LAST-ACTIVITY-DATE                 pic x(10).
           12  CR-LAST-STMT-DT                       pic x(10).
           12  CR-BAL-FWD                            PIC -9(7).99.
           12  CR-CUR-COM                            PIC -9(7).99.
           12  CR-CUR-CHG                            PIC -9(7).99.
           12  CR-CUR-PMT                            PIC -9(7).99.
           12  CR-END-BAL                            PIC -9(7).99.
           12  CR-CUR                                PIC -9(7).99. 
           12  CR-OV30                               PIC -9(7).99. 
           12  CR-OV60                               PIC -9(7).99. 
           12  CR-OV90                               PIC -9(7).99.
           12  CR-YTD-COM                            PIC -9(7).99.
           12  CR-YTD-OV                             PIC -9(7).99.
           12  CR-CUR-OVR-UNDR                       PIC -9(7).99.
           12  CR-YTD-OVR-UNDR                       PIC -9(7).99.
           12  CR-CUR-FICA                           PIC -9(7).99.
           12  CR-YTD-FICA                           PIC -9(7).99.
           12  CR-LF-CLM-AMT                         PIC -9(9).99.
           12  CR-AH-CLM-AMT                         PIC -9(9).99.
           12  CR-CURRENT-LAST-STMT-DT               pic x(10).
           12  CR-CURRENT-BAL-FWD                    PIC -9(7).99.
           12  CR-CURRENT-CUR-COM                    PIC -9(7).99.
           12  CR-CURRENT-CUR-CHG                    PIC -9(7).99.
           12  CR-CURRENT-CUR-PMT                    PIC -9(7).99.
           12  CR-CURRENT-END-BAL                    PIC -9(7).99.
           12  CR-CURRENT-CUR                        PIC -9(7).99.
           12  CR-CURRENT-OV30                       PIC -9(7).99.
           12  CR-CURRENT-OV60                       PIC -9(7).99.
           12  CR-CURRENT-OV90                       PIC -9(7).99.
           12  CR-CURRENT-YTD-COM                    PIC -9(7).99.
           12  CR-CURRENT-YTD-OV                     PIC -9(7).99.
           12  CR-YTD-PAID-COM                       PIC -9(7).99.
           12  CR-YTD-PAID-OV                        PIC -9(7).99.
           12  CR-CURRENT-MONTH-ACTIVITY             PIC X.                 
           12  CR-DELINQUENT-LETTER-CODE             PIC X.                 
           12  CR-CSR-CODE                           PIC X(4).              
           12  CR-GA-EFFECTIVE-DT                    PIC X(10).
           12  CR-GA-TERMINATION-DT                  PIC X(10).
           12  CR-GA-STATUS-CODE                     PIC X.                 
           12  CR-GA-COMMENT-1                       PIC X(40).             
           12  CR-GA-COMMENT-2                       PIC X(40).             
           12  CR-GA-COMMENT-3                       PIC X(40).             
           12  CR-GA-COMMENT-4                       PIC X(40).             
           12  CR-RPTCD2                             PIC X(10).
           12  CR-OV120                              PIC -9(7).99.
           12  CR-CURRENT-OV120                      PIC -9(7).99.
           12  CR-TYPE-AGENT                         PIC X(01).             
           12  CR-FAXNO                              pic x(10).
           12  CR-MD-GL-ACCT                         PIC X(10).
           12  CR-MD-DIV                             PIC XX.
           12  CR-MD-CENTER                          PIC X(4).
           12  cr-md-amt                             pic x(9).
           12  CR-MD-AMT-n redefines
               cr-md-amt                             PIC -9(5).99.
           12  CR-CREATE-AP-CHECK                    PIC X.
           12  CR-DELIVER-CK-TO-MEL                  PIC X.
           12  CR-ACH-STATUS                         PIC X.             
           12  CR-BILL-SW                            PIC X.             
           12  CR-CONTROL-NAME                       PIC X(30).
           12  CR-MAX-BANK-FEE-LEASE                 PIC -9(5).99.
           12  CR-MAX-BANK-FEE                       PIC -9(5).99.
           12  CR-CLP-STATE                          PIC XX.
           12  CR-FIRST-WRITTEN-DT                   PIC X(10).
           12  CR-SPP-REFUND-EDIT                    PIC X.
                                                                        
       EXEC SQL              
          END DECLARE SECTION
       END-EXEC              

       01  kt-sub                      pic s9(5) comp-3 value +0.
       01  kt-max                      pic s9(5) comp-3 value +0.
       01  WS-KEY-TABLE.
           05  key-table occurs 10.
               10  kt-carrier          pic x.
               10  kt-group            pic x(6).
               10  kt-fin-resp         pic x(10).
               10  kt-account          pic x(10).
               10  kt-type             pic x.
               10  kt-delim            pic x.
       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).

       01  ws-work-time                pic 9(7).
       01  filler redefines ws-work-time.
           05  filler                  pic x.
           05  ws-hh                   pic 99.
           05  ws-mm                   pic 99.
           05  ws-ss                   pic 99.

       01  WS-FIN-RESP                 PIC X(10).
       01  WS-CO-DATA.
           05  WS-CO-NUM               PIC X(10).
           05  WS-CO-PRIMARY-CONTACT   PIC X(30).
           05  WS-CO-NAME              PIC X(30).
           05  WS-CO-MAIL-NAME         PIC X(30).
           05  WS-CO-ADDR1             PIC X(30).
           05  WS-CO-ADDR2             PIC X(30).
           05  WS-CO-ADDR3             PIC X(30).
           05  WS-CO-ZIP               PIC X(9).
           05  WS-CO-PHONE             PIC X(10).

           
       01  WS-CO-KEY.
           05  WS-CO-COMPANY-CD        PIC X.                                       
           05  WS-CO-CARRIER           PIC X.                                       
           05  WS-CO-GROUP             PIC X(6).                                    
           05  WS-CO-FIN-RESP          PIC X(10).   
           05  WS-CO-ACCOUNT           PIC X(10).
           05  WS-CO-TYPE              PIC X.

       01  WS-CID-NO                   PIC X(8).

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

                                        COPY ERCCOMP.
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
         05 CLIENT-IN-DATA.
            15  CLIENT-KICK-OFF      PIC X(8).
            15  CLIENT-ID            PIC XXX.
            15  client-cntr          pic 999.
            15  FILLER               PIC X(22).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).

       01  var  pic x(30).

       procedure division.

           display 'SOCK16:transaction data = ', CLIENT-IN-DATA.
           display 'SOCK16:socket number    =', GIVE-TAKE-SOCKET.
           display ' client kick off **'client-kick-off '**'
           display ' client in data **' client-in-data '**'

           perform 0000-init           thru 0000-exit
           perform 0010-init-contact   thru 0010-exit

           perform 0100-process-socket thru 0100-exit until
              end-of-socket
           display ' after 0100 '
           move '0000;Success '        to ws-return-string
           PERFORM 0200-SEND-BUFFER    thru 0200-exit
           go to 0300-close-socket

           .
       0000-init.

           display ' made it to 0000-init '
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
           display ' client kick off **'client-kick-off '**'

           .
       0000-exit.
           exit.

       0010-init-contact.
           display ' made it to 0010-init '
           display ' client kick off **'client-kick-off '**'

           if client-kick-off = 'SOCKET16'
              continue
           else
              move '9999;Unknown origin, who are you? '
                                       to ws-return-string
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              display ' Unknown origin ' client-in-data
              go to 0300-close-socket
           end-if

           evaluate client-id
              when 'VPP'
                 MOVE X'07'            TO WS-COMP-CD
                 MOVE 'VPP'            TO WS-COMP-ID
              when 'AHL'
                 MOVE X'06'            TO WS-COMP-CD
                 MOVE 'AHL'            TO WS-COMP-ID
              when 'DCC'
                 MOVE X'05'            TO WS-COMP-CD
                 MOVE 'DCC'            TO WS-COMP-ID
              when 'CID'
                 MOVE X'04'            TO WS-COMP-CD
                 MOVE 'CID'            TO WS-COMP-ID
              when other
                 move '0113;Invalid company id ' to ws-return-string
                 PERFORM 0200-SEND-BUFFER thru 0200-exit
                 display ' Invalid company id ' client-id
                 go to 0300-close-socket
           END-evaluate

           if client-cntr numeric
              move client-cntr         to kt-max
           else
              move '0117;Invalid Counter ' to ws-return-string
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              display ' Invalid Counter  ' client-cntr
              go to 0300-close-socket
           end-if

           move 'SOCKET16READY'        to ws-send-buf
           move +25                    to ws-send-msg-size

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags

           if return-code <= zero
              display 'SOCK16:send error ' return-code
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if

           .
       0010-exit.
           exit.

       0100-process-socket.
           display ' made it to 0100-process '

           if kt-sub = +0
              perform 0110-receive     thru 0110-exit
           end-if

           compute kt-sub = kt-sub + +1

           if kt-sub > kt-max
              set end-of-socket to true
           end-if

           if end-of-socket
              go to 0100-exit
           end-if

           move ws-comp-cd             to ws-co-key
           move key-table(kt-sub)(1:28) to ws-co-key (2:28)

           perform 0400-get-ercomp     thru 0400-exit

           .
       0100-exit.
           exit.

       0110-receive.
           display ' made it to 0110-receive '

100917     move spaces                 to ws-recv-buf

           call "recv" using by value GIVE-TAKE-SOCKET
               by reference ws-recv-buf
               by value ws-recv-msg-size
               by value ws-flags.

           display ' ret code ' return-code
           if return-code < zero
              display 'SOCK16:recv error ' return-code
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0110-exit
           end-if

           if return-code = zero
              display 'SOCK16:client disconnected',
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0110-exit
           end-if

           display ' recv buf ' ws-recv-buf (1:50)
           move ws-recv-buf (1:290)     to ws-key-table

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
              display 'SOCK16:send error ',
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if

           .
       0200-exit.
           exit.

       0250-socket-error.

           display "SOCK16:did not complete"
           display 'SOCK16:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK16:socket number    =', GIVE-TAKE-SOCKET.
           display ' return code = ' return-code

           .
       0250-exit.
           exit.

       0300-close-socket.

           if connected-to-db
              EXEC SQL
                  commit work
              END-EXEC
              if sqlcode not = 0
                 move ' Failed to Commit DB '
                                       to ws-return-string
                 display "Error: commit release "
                 display ' sql return code ' sqlcode
                 display ' sql err mess    ' sqlerrmc
              end-if
           end-if

           if connected-to-db
              EXEC SQL
                  disconnect all
              END-EXEC
              move ' '                 to ws-connect-sw
           end-if
           display ' about to return cics '
           exec cics return end-exec
           goback

           .
       0300-exit.
           exit.

       0400-GET-ERCOMP.
           display ' made it to 0400-get  '

      *    move ws-comp-cd to ws-co-company-cd
      *    move kt-carrier(1) to ws-co-carrier
      *    move kt-group(1) to ws-co-group
      *    move kt-fin-resp(1) to ws-co-fin-resp
      *    move kt-account(1) to ws-co-account
      *    move kt-type(1) to ws-co-type

           display ' ws co key ' ws-co-key (2:28)

           IF WS-CO-ACCOUNT = SPACES OR ZEROS OR LOW-VALUES
              MOVE LOW-VALUES          TO WS-CO-ACCOUNT
              MOVE 'G'                 TO WS-CO-TYPE
           END-IF

           EXEC CICS READ
                INTO    (COMPENSATION-MASTER)
                DATASET ('ERCOMP')
                RIDFLD  (WS-CO-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

           display ' read response ' ws-response

           if not resp-normal
              go to 0400-exit
           end-if

           move co-carrier             to cr-carrier
           move co-grouping            to cr-grouping
           move co-resp-no             to cr-resp-no 
           move co-account             to cr-account
           move co-type                to cr-type

           move co-last-maint-hhmmss   to ws-work-time
           string
              ws-hh   ':'
              ws-mm   ':'
              ws-ss delimited by size into cr-last-maint-hhmmss
           end-string

      *    move co-last-maint-hhmmss   to cr-last-maint-hhmmss-n
           move co-last-maint-user     to cr-last-maint-user

           if co-last-maint-dt <> spaces and low-values
              move co-last-maint-dt    to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-last-maint-dt
                 move +0               to nu-last-maint-dt
              else
                 display ' error maint dt invalid ' dc-error-code
                 move -1               to nu-last-maint-dt
              end-if
           else
              move spaces              to cr-last-maint-dt
              move -1                  to nu-last-maint-dt
           end-if

           move co-stmt-type           to cr-stmt-type
           move co-comp-type           to cr-comp-type
           move co-stmt-owner          to cr-stmt-owner
           move co-balance-control     to cr-balance-control
           move co-internal-control-1  to cr-internal-control-1
           move CO-INTERNAL-CONTROL-1  to cr-INTERNAL-CONTROL-1  
           move CO-INTERNAL-CONTROL-2  to cr-INTERNAL-CONTROL-2  
           move CO-GA-WITHOLD-PCT      to cr-GA-WITHOLD-PCT-n
           move CO-GA-DIRECT-DEP       to cr-GA-DIRECT-DEP       
           move CO-ACCT-NAME           to cr-ACCT-NAME           
           move CO-MAIL-NAME           to cr-MAIL-NAME           
           move CO-ADDR-1              to cr-ADDR-1              
           move CO-ADDR-2              to cr-ADDR-2              
           move CO-ADDR-3              to cr-ADDR-3              
           move CO-CSO-1099            to cr-CSO-1099            
           move CO-ZIP                 to cr-ZIP                 
           move CO-SOC-SEC             to cr-SOC-SEC             
           move CO-TELEPHONE           to cr-TELEPHONE           

           if co-roladex-print-dt <> spaces and low-values
              move co-roladex-print-dt to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-rolodex-print-dt
                 move +0               to nu-rolodex-print-dt
              else
                 display ' error rolodex print dt invalid '
                    dc-error-code
                 move -1               to nu-rolodex-print-dt
              end-if
           else
              move spaces              to cr-rolodex-print-dt
              move -1                  to nu-rolodex-print-dt
           end-if

           move CO-AR-BAL-LEVEL        to cr-AR-BAL-LEVEL        
           move CO-AR-NORMAL-PRINT     to cr-AR-NORMAL-PRINT     
           move CO-AR-SUMMARY-CODE     to cr-AR-SUMMARY-CODE     
           move CO-AR-REPORTING        to cr-AR-REPORTING        
           move CO-AR-PULL-CHECK       to cr-AR-PULL-CHECK       
           move CO-AR-BALANCE-PRINT    to cr-AR-BALANCE-PRINT    
           move CO-AR-LAST-RUN-CODE    to cr-AR-LAST-RUN-CODE    

           if co-LAST-EOM-STMT-DT <> spaces and low-values
              move co-LAST-EOM-STMT-DT to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-LAST-EOM-STMT-DT
                 move +0               to nu-LAST-EOM-STMT-DT
              else
                 display ' error last eom stmt dt invalid '
                    dc-error-code
                 move -1               to nu-LAST-EOM-STMT-DT
              end-if
           else
              move spaces              to cr-LAST-EOM-STMT-DT
              move -1                  to nu-LAST-EOM-STMT-DT
           end-if

           move CO-USER-CODE           to cr-USER-CODE           
           move CO-REPORT-GROUP-ID     to cr-REPORT-GROUP-ID     

           if co-LAST-ACTIVITY-DATE <> spaces and low-values
              move co-LAST-ACTIVITY-DATE
                                       to dc-greg-date-1-ymd-r
              set ymd-greg-to-bin to true *> move '3' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-LAST-ACTIVITY-DATE
                 move +0               to nu-LAST-ACTIVITY-DATE
              else
                 display ' error last activity dt invalid '
                    dc-error-code
                 move -1               to nu-LAST-ACTIVITY-DATE
              end-if
           else
              move spaces              to cr-LAST-ACTIVITY-DATE
              move -1                  to nu-LAST-ACTIVITY-DATE
           end-if

           if co-LAST-STMT-DT <> spaces and low-values
              move co-LAST-STMT-DT     to dc-greg-date-1-ymd-r
              set ymd-greg-to-bin to true *> move '3' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-LAST-STMT-DT
                 move +0               to nu-LAST-STMT-DT
              else
                 display ' error last stmt dt invalid '
                    dc-error-code
                 move -1               to nu-LAST-STMT-DT
              end-if
           else
              move spaces              to cr-LAST-STMT-DT
              move -1                  to nu-LAST-STMT-DT
           end-if

           move CO-BAL-FWD             to cr-BAL-FWD             
           move CO-CUR-COM             to cr-CUR-COM             
           move CO-CUR-CHG             to cr-CUR-CHG             
           move CO-CUR-PMT             to cr-CUR-PMT             
           move CO-END-BAL             to cr-END-BAL             
           move CO-CUR                 to cr-CUR                 
           move CO-OV30                to cr-OV30                
           move CO-OV60                to cr-OV60                
           move CO-OV90                to cr-OV90                
           move CO-YTD-COM             to cr-YTD-COM             
           move CO-YTD-OV              to cr-YTD-OV              
           move CO-CUR-OVR-UNDR        to cr-CUR-OVR-UNDR        
           move CO-YTD-OVR-UNDR        to cr-YTD-OVR-UNDR        
           move CO-CUR-FICA            to cr-CUR-FICA            
           move CO-YTD-FICA            to cr-YTD-FICA            
           move CO-LF-CLM-AMT          to cr-LF-CLM-AMT          
           move CO-AH-CLM-AMT          to cr-AH-CLM-AMT          

           if co-current-LAST-STMT-DT <> spaces and low-values
              move co-current-LAST-STMT-DT
                                       to dc-greg-date-1-ymd-r
              set ymd-greg-to-bin to true *> move '3' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-current-LAST-STMT-DT
                 move +0               to nu-current-LAST-STMT-DT
              else
                 display ' error current last stmt dt invalid '
                    dc-error-code
                 move -1               to nu-current-LAST-STMT-DT
              end-if
           else
              move spaces              to cr-current-LAST-STMT-DT
              move -1                  to nu-current-LAST-STMT-DT
           end-if

           move CO-CURRENT-BAL-FWD     to cr-CURRENT-BAL-FWD     
           move CO-CURRENT-CUR-COM     to cr-CURRENT-CUR-COM     
           move CO-CURRENT-CUR-CHG     to cr-CURRENT-CUR-CHG     
           move CO-CURRENT-CUR-PMT     to cr-CURRENT-CUR-PMT     
           move CO-CURRENT-END-BAL     to cr-CURRENT-END-BAL     
           move CO-CURRENT-CUR         to cr-CURRENT-CUR         
           move CO-CURRENT-OV30        to cr-CURRENT-OV30        
           move CO-CURRENT-OV60        to cr-CURRENT-OV60        
           move CO-CURRENT-OV90        to cr-CURRENT-OV90        
           move CO-CURRENT-YTD-COM     to cr-CURRENT-YTD-COM     
           move CO-CURRENT-YTD-OV      to cr-CURRENT-YTD-OV      
           move CO-YTD-PAID-COM        to cr-YTD-PAID-COM        
           move CO-YTD-PAID-OV         to cr-YTD-PAID-OV         
           move CO-CURRENT-MONTH-ACTIVITY
                                       to cr-CURRENT-MONTH-ACTIVIty
           move CO-DELINQUENT-LETTER-CODE
                                       to cr-DELINQUENT-LETTER-COde 
           move CO-CSR-CODE            to cr-CSR-CODE            

           if co-GA-EFFECTIVE-DT <> spaces and low-values
              move co-GA-EFFECTIVE-DT to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-GA-EFFECTIVE-DT
                 move +0               to nu-GA-EFFECTIVE-DT
              else
                 display ' error GA EFF dt invalid '
                    dc-error-code
                 move -1               to nu-GA-EFFECTIVE-DT
              end-if
           else
              move spaces              to cr-GA-EFFECTIVE-DT
              move -1                  to nu-GA-EFFECTIVE-DT
           end-if

           if co-GA-termination-DT <> spaces and low-values
              move co-GA-TERMINATION-DT to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-GA-TERMINATION-DT
                 move +0               to nu-GA-TERMINATION-DT
              else
                 display ' error GA termination dt invalid '
                    dc-error-code
                 move -1               to nu-GA-TERMINATION-DT
              end-if
           else
              move spaces              to cr-GA-TERMINATION-DT
              move -1                  to nu-GA-TERMINATION-DT
           end-if

           move CO-GA-STATUS-CODE      to cr-GA-STATUS-CODE      
           move CO-GA-COMMENT-1        to cr-GA-COMMENT-1        
           move CO-GA-COMMENT-2        to cr-GA-COMMENT-2        
           move CO-GA-COMMENT-3        to cr-GA-COMMENT-3        
           move CO-GA-COMMENT-4        to cr-GA-COMMENT-4        
           move CO-RPTCD2              to cr-RPTCD2              
           move CO-OV120               to cr-OV120               
           move CO-CURRENT-OV120       to cr-CURRENT-OV120       
           move CO-TYPE-AGENT          to cr-TYPE-AGENT          
           move CO-FAXNO               to cr-FAXNO               
           move CO-MD-GL-ACCT          to cr-MD-GL-ACCT          
           move CO-MD-DIV              to cr-MD-DIV              
           move CO-MD-CENTER           to cr-MD-CENTER           
           move CO-MD-AMT              to cr-MD-AMT-n
           move CO-CREATE-AP-CHECK     to cr-CREATE-AP-CHECK     
           move CO-DELIVER-CK-TO-MEL   to cr-DELIVER-CK-TO-MEL   
           move CO-ACH-STATUS          to cr-ACH-STATUS          
           display ' co bill sw ' co-bill-sw
           move CO-BILL-SW             to cr-BILL-SW             
           move CO-CONTROL-NAME        to cr-CONTROL-NAME        
           move CO-MAX-BANK-FEE-LEASE  to cr-MAX-BANK-FEE-LEASE  
           move CO-MAX-BANK-FEE        to cr-MAX-BANK-FEE        
           move CO-CLP-STATE           to cr-CLP-STATE           

           if co-FIRST-WRITTEN-DT <> spaces and low-values
              move co-FIRST-WRITTEN-DT to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-FIRST-WRITTEN-DT
                 move +0               to nu-FIRST-WRITTEN-DT
              else
                 display ' error 1st written dt invalid '
                    dc-error-code
                 move -1               to nu-FIRST-WRITTEN-DT
              end-if
           else
              move spaces              to cr-FIRST-WRITTEN-DT
              move -1                  to nu-FIRST-WRITTEN-DT
           end-if

           move CO-SPP-REFUND-EDIT     to cr-SPP-REFUND-EDIT

           perform 0500-update-table   thru 0500-exit

           .
       0400-EXIT.
           EXIT.

       0500-update-table.

           display ' made it to 0500-update '

           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if

           display ' bill sw ' cr-bill-sw
           display ' key *' cr-carrier '*' cr-grouping '*'
              cr-resp-no '*' cr-account '*' cr-type '*'

           exec sql
              update
                 ERCOMP
              set
                 ex_stmt_type = :cr-stmt-type,
                 ex_comp_spp = :cr-comp-type,
                 ex_balance_control = :cr-balance-control,
                 ex_bill_sw = :cr-bill-sw,
                 ex_ga_withhold_pct = :cr-ga-withold-pct,
                 ex_ga_direct_deposit = :cr-ga-direct-dep,
                 ex_create_ap_check = :cr-create-ap-check,
                 ex_deliver_to_mel = :cr-deliver-ck-to-mel,
                 ex_md_gl_acct = :cr-md-gl-acct,
                 ex_md_div = :cr-md-div,
                 ex_md_center = :cr-md-center,
                 ex_md_amount = :cr-md-amt,
                 ex_report_group_id = :cr-report-group-id,
                 ex_comment1 = :cr-ga-comment-1,
                 ex_comment2 = :cr-ga-comment-2,
                 ex_comment3 = :cr-ga-comment-3,
                 ex_comment4 = :cr-ga-comment-4,
                 ex_status = :cr-ga-status-code,
                 ex_effect = :cr-GA-EFFECTIVE-DT :nu-GA-EFFECTIVE-DT,
                 ex_expire = :CR-GA-TERMINATION-DT
                             :nu-GA-TERMINATION-DT,
                 ex_last_maint_dt = :cr-last-maint-dt
                             :nu-last-maint-dt,
                 ex_last_maint_user = :cr-last-maint-user,
                 ex_last_maint_time = :cr-last-maint-hhmmss,
                 ex_stmt_owner = :cr-stmt-owner
              where
                 ex_carrier  = :cr-carrier and
                 ex_grouping = :cr-grouping and
                 ex_resp_no  = :cr-resp-no and
                 ex_account  = :cr-account and
                 ex_type     = :cr-type
           end-exec

           if sqlcode not = 0
              display "Error: did not update table "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move sqlcode             to ws-display-response
              move '9999;Did not update ' to ws-return-string
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if
           display ' sql code upd ' sqlcode
           .
       0500-exit.
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

       
           if sqlcode not = 0
              display "Error: cannot connect to " svr
              display sqlcode
              display sqlerrmc
              move '9999;Could not connect to DB ' to ws-return-string
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           else
              display ' Successful Connect ' sqlcode
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
