      *****************************************************************
      *                                                               *
      * Copyright (c) 2015 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. SOCK04.
      *
      *AUTHOR.    Cowtown.
      *           Colleyville, TEXAS.

      *REMARKS.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  This program acts as a Socket Server and expects a key    ***
      ***  to read an account master and a compensation master and   ***
      ***  will pass back a few items. It currently is designed to   ***
      ***  work with the Agent Signature Verification process.       ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

031115******************************************************************
031115*                   C H A N G E   L O G
031115*
031115* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031115*-----------------------------------------------------------------
031115*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031115* EFFECTIVE    NUMBER
031115*-----------------------------------------------------------------
031115* 031115  CR               PEMA  NEW PROGRAM
031115*-----------------------------------------------------------------

       environment division.
       data division.
       working-storage section.
      *
      * program buffers
      *
       77 ws-send-msg-size           pic s9(8) comp value 4096.
       77 ws-recv-msg-size           pic s9(8) comp value 4096.
       77 ws-recv-buf                pic x(4096).
       77 ws-send-buf                pic x(4096) VALUE SPACES.
       77 ws-recv-total              pic s9(8) comp value 0.
       77 ws-recv-left               pic s9(8) comp value 0.
       77 ws-seq-num                 pic s9(8) comp value 0.
       77 ws-flags                   pic s9(8) comp value 0.
       77 WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
       77 WS-COMP-ID                   PIC XXX VALUE 'CID'.
       77 WS-SAVE-ACCOUNT              PIC X(10)  VALUE SPACES.
       77 WS-BIN-ORIG-EFF-DT           PIC XX  VALUE LOW-VALUES.
       77 WS-ORIG-EFF-DT               PIC X(10)  VALUE SPACES.
       77 WS-EFF-DATE                  PIC X(10)  VALUE SPACES.
       77 WS-EXP-DATE                  PIC X(10)  VALUE SPACES.
       77 a1                           PIC S999 COMP-3 VALUE +0.
       77 c1                           PIC S999 COMP-3 VALUE +0.
       77 WS-BUILD-SW                  PIC X.
          88  TIME-TO-BUILD               VALUE 'Y'.
       77 WS-SAVE-ERACCT               PIC X(2000).
       77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
       77 WS-PERFORM-SW                PIC X VALUE SPACES.
          88  GET-RATES                    VALUE 'R'.
          88  GET-ACT-ACCTS                VALUE 'A'.
       77 ws-bin-eff-dt                pic xx  value low-values.
       77 ws-bin-1st-pay-dt            pic xx  value low-values.
       77 WS-DISP-AMT                  PIC Z,ZZZ,Z99.99.
       77 ws-disp-rate                 pic z9.99999.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  WS-ERCTBL-SW                PIC X VALUE ' '.
           88  END-OF-ERCTBL                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       77  ws-bin-cert-eff-dt          pic xx   value low-values.
       77  ws-current-bin-dt           pic xx   value low-values.
       77  ws-cert-note-generic-key-len pic s9(4) comp value +34.
       77  note-count                  pic s999 comp-3 value +0.
       77  ws-build-note-sw            pic x value ' '.
           88  finished-with-notes      value 'Y'.
       77  ws-ercnot-sw                pic x  value spaces.
           88  ercnot-startbr            value 'Y'.
       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).
       01  cert-note-records-holder.
           05  cert-note-record occurs 300.
               10  filler              pic x(48).
               10  cnr-rest            pic x(102).



       01  socket04-commarea.
                                       copy SOCK04-COMMAREA.

       01  WS-AM-KEY.
           05  WS-AM-COMPANY-CD        PIC X.                                       
           05  WS-AM-CARRIER           PIC X.                                       
           05  WS-AM-GROUP             PIC X(6).                                    
           05  WS-AM-STATE             PIC XX.   
           05  WS-AM-ACCOUNT           PIC X(10).
           05  WS-AM-EXP-DT            PIC XX.
           05  FILLER                  PIC X(4).

       01  WS-CO-KEY.
           05  WS-CO-COMPANY-CD        PIC X.                                       
           05  WS-CO-CARRIER           PIC X.                                       
           05  WS-CO-GROUP             PIC X(6).                                    
           05  WS-CO-FIN-RESP          PIC X(10).   
           05  WS-CO-ACCOUNT           PIC X(10).
           05  WS-CO-TYPE              PIC X.

       01  WS-CM-KEY.
           05  WS-CM-COMPANY-CD        PIC X.                                       
           05  WS-CM-CARRIER           PIC X.                                       
           05  WS-CM-GROUP             PIC X(6).                                    
           05  WS-CM-STATE             PIC XX.   
           05  WS-CM-ACCOUNT           PIC X(10).
           05  WS-CM-EFF-DT            PIC XX.
           05  WS-CM-CERT-NO           PIC X(11).

       01  WS-CZ-KEY.
           05  WS-CZ-COMPANY-CD        PIC X.                                       
           05  WS-CZ-CARRIER           PIC X.                                       
           05  WS-CZ-GROUP             PIC X(6).                                    
           05  WS-CZ-STATE             PIC XX.   
           05  WS-CZ-ACCOUNT           PIC X(10).
           05  WS-CZ-EFF-DT            PIC XX.
           05  WS-CZ-CERT-NO           PIC X(11).
           05  WS-CZ-REC-TYPE          PIC X.
           05  ws-cz-note-seq          pic s9(4) comp.

       01  WS-CS-KEY.
           05  WS-CS-COMPANY-CD        PIC X.                                       
           05  WS-CS-CARRIER           PIC X.                                       
           05  WS-CS-GROUP             PIC X(6).                                    
           05  WS-CS-STATE             PIC XX.   
           05  WS-CS-ACCOUNT           PIC X(10).
           05  WS-CS-EFF-DT            PIC XX.
           05  WS-CS-CERT-NO           PIC X(11).
           05  WS-CS-TRLR-TYPE         PIC X.

       01  ws-return-stuff.
           05  ws-am-name              pic x(30).
           05  ws-am-person            pic x(30).
           05  ws-am-control-name      pic x(30).
           05  ws-am-phone             pic x(10).
           05  ws-co-fax               pic x(10).
           05  ws-co-contact           pic x(30).

       01  WS-CID-NO                   PIC X(8).

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

                                        COPY ELCCERT.
                                        COPY ELCCRTT.
                                        COPY ERCACCT.
                                        COPY ERCCOMP.
                                        COPY ERCCNOT.
                                        COPY ELCDATE.

       01 soc-CLIENT-IN-DATA.
          05  soc-CLIENT-CAR           PIC X.
          05  soc-CLIENT-GRP           PIC X(6).
          05  soc-CLIENT-ST            PIC XX.
          05  soc-CLIENT-ACT           PIC X(10).
          05  soc-CLIENT-EFF-DT        PIC X(8).
          05  soc-Client-cert-no       pic x(11).
          05  soc-CLIENT-ID            PIC XXX.
          05  soc-client-proc-id       pic x(4).
          05  soc-CLIENT-ACTION-TAKEN  PIC X.
      
       linkage section.
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).
         05 CLIENT-IN-DATA           pic x(36).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).

       procedure division.
      *
      * when calling a C function the function returns its value
      * in the system variable return code.
      *
           display 'SOCK04:transaction data =', CLIENT-IN-DATA.
           display 'SOCK04:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK04:socket name      =', lstn-name ' '
              lstn-subname

           perform 0010-init-contact   thru 0010-exit
           perform 0020-receive        thru 0020-exit
           perform 0030-link-business-logic
                                       thru 0030-exit

           string
              sock-am-name          ';'
              sock-am-person        ';'
              sock-am-cntrl-name    ';'
              sock-am-phone         ';'
              sock-co-fax           ';'
              sock-co-contact
                 DELIMITED BY SIZE INTO WS-SEND-BUF
             END-STRING

           display 'SOCK04:About to send      '
           display 'SOCK04:sequence number  =', ws-seq-num.
           display 'SOCK04:send buffer      =', ws-send-buf(1:80).

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.

           if return-code <= zero
               display 'SOCK04:send error ',
               go to socket-error.
           GO TO SOCKET-FIN

           .
       0010-init-contact.

           if client-in-data (1:8) = 'SOCKET04'
              continue
           else
              display ' Unknown origin ' client-in-data
              go to socket-fin
           end-if

           move 'SOCKET04READY'        to ws-send-buf
           move +25                    to ws-send-msg-size


           display 'SOCK04:sequence number  =', ws-seq-num.
           display 'SOCK04:send buffer      =', ws-send-buf(1:25).

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags

           if return-code <= zero
              display 'SOCK04:send error ' return-code
              go to socket-error
           end-if

           display 'SOCK04:About to recv '

           .
       0010-exit.
           exit.

       0020-receive.

           call "recv" using by value GIVE-TAKE-SOCKET
               by reference ws-recv-buf
               by value ws-recv-msg-size
               by value ws-flags.

           if return-code < zero
              display 'SOCK04:recv error ' return-code
              go to socket-error
           end-if

           if return-code = zero
              display 'SOCK04:client disconnected',
              go to socket-error
           end-if

           display 'SOCK04:Good recv  '
           display 'SOCK04:return code      = ', return-code
           display 'SOCK04:receive buffer   = ', ws-recv-buf(1:42)

           move +1024                  to ws-send-msg-size

           move ws-recv-buf (1:46)     to soc-client-in-data

           .
       0020-exit.
           exit.

       0030-link-business-logic.

           move soc-client-in-data     to sock-in-data
           move spaces                 to sock-return-area

           exec cics link
              program  ('SOCK04BL')
              commarea (socket-commarea)
           end-exec

           .
       0030-exit.
           exit.


       socket-error.
           if ws-seq-num <> 0
               display "SOCK04:did not complete".
      *
      * flush the send buffer and deallocate
      *

          .
       socket-fin.

      *    call "close" using by value GIVE-TAKE-SOCKET .
           display 'SOCK04:done'.
           exec cics
              return
           end-exec.
           goback.

       9700-DATE-LINK.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.

