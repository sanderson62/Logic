      *****************************************************************
      *                                                               *
      * Copyright (c) 2015 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. SOCK05.
      *
      *AUTHOR.    Cowtown.
      *           Colleyville, TEXAS.

      *REMARKS.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  This program acts as a Socket Server and expects a key    ***
      ***  to read the pending business file and will pass back all  ***
      ***  issue certs that are in that batch.                       ***
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
111115* 111115  CR2015101500001  PEMA  ALLOW FROM EL127C
021516* 021516  IR2016021500003  PEMA  INCREASE BUFF SIZE FOR LG BATCHES
031115*-----------------------------------------------------------------

       environment division.
       data division.
       working-storage section.
      *
      * program buffers
      *
      *77 ws-send-msg-size           pic s9(8) comp value 19200.
021516 77 ws-send-msg-size           pic s9(8) comp value 48000.
       77 ws-recv-msg-size           pic s9(8) comp value 19200.
       77 ws-recv-buf                pic x(4096).
021516 77 ws-send-buf                pic x(48000) VALUE SPACES.
      *77 ws-send-buf                pic x(19200) VALUE SPACES.
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
       77 S1                           PIC S999 COMP-3 VALUE +0.
       77 S2                           PIC S999 COMP-3 VALUE +0.
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
       77  WS-ERPNDB-SW                PIC X VALUE ' '.
           88  END-OF-ERPNDB                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       77  ws-manager-id               pic x value 'N'.
       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).

       01  ws-cf-key.
           05  ws-cf-comp-id           pic xxx.
           05  ws-cf-rec-type          pic x.
           05  ws-cf-access-cd         pic x(4).
           05  ws-cf-seq-no            pic s9(4) comp value +0.
       01  WS-CS-KEY.
           05  WS-CS-COMPANY-CD        PIC X.
           05  WS-CS-CARRIER           PIC X.
           05  WS-CS-GROUPING          PIC X(6).
           05  WS-CS-STATE             PIC XX.
           05  WS-CS-ACCOUNT           PIC X(10).
           05  WS-CS-CERT-EFF-DT       PIC XX.
           05  WS-CS-CERT-NO           PIC X(10).
           05  WS-CS-CERT-SFX          PIC X.
           05  WS-CS-TRLR-TYPE         PIC X.

       01  WS-CM-KEY.
           05  WS-CM-COMPANY-CD        PIC X.
           05  WS-CM-CARRIER           PIC X.
           05  WS-CM-GROUPING          PIC X(6).
           05  WS-CM-STATE             PIC XX.
           05  WS-CM-ACCOUNT           PIC X(10).
           05  WS-CM-CERT-EFF-DT       PIC XX.
           05  WS-CM-CERT-NO           PIC X(11).

       01  WS-PB-KEY.
           05  WS-PB-COMPANY-CD        PIC X.
           05  WS-PB-BATCH-NO          PIC X(6).
           05  WS-PB-SEQ-NO            PIC S9(4) COMP.
           05  WS-PB-CHG-SEQ-NO        PIC S9(4) COMP.

       01  p1                          pic s999 comp-3 value +0.
       01  ws-return-stuff.
021516     05  ws-pend-records occurs 500.
               10  ws-pend-eff-dte     pic x(10).
               10  ws-pend-cert-no     pic x(10).
               10  ws-pend-cert-sfx    pic x.
               10  ws-pend-first-name  pic x(10).
               10  ws-pend-last-name   pic x(15).
               10  ws-pend-status      pic x.
               10  ws-pend-manager     pic x.
               10  ws-fill             pic x.
               10  ws-pend-agent-name  pic x(46).
               10  ws-pend-semi        pic x.

       01  WS-CID-NO                   PIC X(8).

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

                                        copy ELCCERT.
                                        COPY ERCPNDB.
                                        COPY ELCCRTT.
                                        copy ELCCNTL.
                                        COPY ELCDATE.

       01 soc-CLIENT-IN-DATA.
          05  soc-client-batch-no      pic x(6).
          05  soc-client-comp-id       pic xxx.
          05  soc-CLIENT-CAR           PIC X.
          05  soc-CLIENT-GRP           PIC X(6).
          05  soc-CLIENT-ST            PIC XX.
          05  soc-CLIENT-ACT           PIC X(10).
          05  soc-CLIENT-EFF-DT        PIC X(8).
          05  soc-Client-cert-no       pic x(11).
          05  soc-client-proc-id       pic x(4).
      
       linkage section.
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).

      **********  SOCK04   ************
         05 CLIENT-IN-DATA           pic x(36).
      **********  SOCK04   ************

      **********  SOCK05   ************
      *  05 CLIENT-IN-DATA.
      *     15  CLIENT-BATCH-NO      PIC X(6).
      *     15  client-comp-id       pic xxx.
      *     15  client-proc-id       pic x(4).
      *     15  FILLER               PIC X(23).
      **********  SOCK05   ************
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
           display 'SOCK05:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK05:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK05:socket name      =', lstn-name ' '
              lstn-subname

           perform 0000-init-contact   thru 0000-exit
           perform 0010-receive        thru 0010-exit

           if soc-client-batch-no not = 'N/A'     *>  from EL631B
              perform 0020-init        thru 0020-exit
              perform 0100-process-erpndb
                                       thru 0100-exit until
                 end-of-erpndb
           else                                   *>  from EL127C
              perform 1000-init        thru 1000-exit
              perform 1100-process-elcert
                                       thru 1100-exit
           end-if
           perform 0200-send-buffer    thru 0200-exit
           perform 0300-close-socket   thru 0300-exit
           goback.

       0000-init-contact.

           if client-in-data (1:8) = 'SOCKET05'
              continue
           else
              display ' Unknown origin ' client-in-data
              go to 0300-close-socket
           end-if

           move 'SOCKET05READY'        to ws-send-buf
           move +25                    to ws-send-msg-size


           display 'SOCK05:sequence number  =', ws-seq-num.
           display 'SOCK05:send buffer      =', ws-send-buf(1:25).

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags

           if return-code <= zero
              display 'SOCK05:send error ' return-code
              go to 0200-socket-error
           end-if

           display 'SOCK05:About to recv '

           .
       0000-exit.
           exit.

       0010-receive.

           call "recv" using by value GIVE-TAKE-SOCKET
               by reference ws-recv-buf
               by value ws-recv-msg-size
               by value ws-flags.

           if return-code < zero
              display 'SOCK05:recv error ' return-code
              go to 0200-socket-error
           end-if

           if return-code = zero
              display 'SOCK05:client disconnected',
              go to 0200-socket-error
           end-if

           display 'SOCK05:Good recv  '
           display 'SOCK05:return code      = ', return-code
           display 'SOCK05:receive buffer   = ', ws-recv-buf(1:51)

021516     move +48000                 to ws-send-msg-size

           move ws-recv-buf (1:51)     to soc-client-in-data

           .
       0010-exit.
           exit.

           .           
       0020-init.

           move low-values             to ws-pb-key
           perform 0030-get-user       thru 0030-exit

           move ws-comp-cd             to ws-pb-company-cd
           move soc-client-batch-no    to ws-pb-batch-no
           move +0                     to ws-pb-seq-no
                                          ws-pb-chg-seq-no

           move spaces                 to ws-return-stuff
           perform 0040-startbr        thru 0040-exit
           perform 0050-readnext       thru 0050-exit
           if not resp-normal
              display ' Error-ERPNDB-Readnext ' ws-response
                 ' ' ws-pb-batch-no
              perform 0300-close-socket
                                       thru 0300-exit
           end-if

           .
       0020-exit.
           exit.

       0030-get-user.

           move soc-client-comp-id     to ws-comp-id
           evaluate true
              when ws-comp-id = 'AHL'
                 MOVE X'06'            TO ws-comp-cd
              when ws-comp-id = 'DCC'
                 move X'05'            to ws-comp-cd
              when ws-comp-id = 'CAP'
                 move X'07'            to ws-comp-cd
              when other
                 move X'04'            to ws-comp-cd
           end-evaluate

           move ws-comp-id             to ws-cf-comp-id
           move '2'                    to ws-cf-rec-type   *> processor
           move soc-client-proc-id     to ws-cf-access-cd
           move +0                     to ws-cf-seq-no
           move 'N'                    to ws-manager-id

           exec cics read
              dataset     ('ELCNTL')
              ridfld      (ws-cf-key)
              into        (control-file)
              resp        (ws-response)
           end-exec
           if resp-normal
              if cf-csr-ind = 'S'
                 move 'Y'              to ws-manager-id
              end-if
           else
              display 'ERROR-ELCNTL-READ ' ws-response ' '
                 ws-cf-key (1:8)
           end-if

           .
       0030-exit.
           exit.

       0040-startbr.

           EXEC CICS STARTBR
                DATASET    ('ERPNDB')
                RIDFLD     (WS-PB-KEY)
                GTEQ
                RESP       (WS-RESPONSE)
           END-EXEC

           if not resp-normal
              display ' Error-ERPNDB-Startbr ' ws-response
                 ' ' ws-pb-batch-no
              perform 0300-close-socket
                                       thru 0300-exit
           end-if

           .
       0040-exit.
           exit.

       0050-readnext.

           EXEC CICS READNEXT
                INTO    (PENDING-BUSINESS)
                DATASET ('ERPNDB')
                RIDFLD  (WS-PB-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

           .
       0050-exit.
           exit.

       0100-process-erpndb.

           if (resp-normal)
              and (ws-pb-company-cd = ws-comp-cd)
              and (ws-pb-batch-no   = soc-client-batch-no)
              continue
           else
              set end-of-erpndb to true
              if p1 > +0
                 move ' ' to ws-pend-semi (p1)
              end-if
              go to 0100-exit
           end-if

           if (not pb-issue)
              or (pb-i-entry-status = 'D' or 'V' or '9')
              go to 0100-continue
           end-if

           add +1 to p1
021516     if p1 > +499
              set end-of-erpndb to true
              display ' Exceeded table ' soc-client-batch-no
              go to 0100-exit
           end-if

           move pb-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           move dc-greg-date-a-edit    to ws-pend-eff-dte (p1)
           move pb-cert-prime          to ws-pend-cert-no (p1)
           move pb-cert-sfx            to ws-pend-cert-sfx (p1)
           move pb-i-insured-first-name
                                       to ws-pend-first-name (p1)
           move pb-i-insured-last-name to ws-pend-last-name (p1)
           move ws-manager-id          to ws-pend-manager (p1)
           move pb-control-by-account (1:33)
                                       to ws-cs-key
           move 'C'                    to ws-cs-trlr-type

           move spaces                 to ws-pend-agent-name (p1)
      *    move all '*'                to ws-pend-agent-name (p1)

           EXEC CICS READ
                INTO    (certificate-trailers)
                DATASET ('ELCRTT')
                RIDFLD  (WS-CS-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

           if resp-normal
              move cs-agent-edit-status to ws-pend-status (p1)
              if cs-ae-verified
                 string
                    cs-agent-fname ' '
                    cs-agent-lname
                       delimited by '  '
                    into ws-pend-agent-name (p1)
                 end-string
              end-if
           else
              display ' no elcrtt ' ws-cs-cert-no
              move ' '                 to ws-pend-status (p1)
           end-if

           move ';'                    to ws-pend-semi (p1)

           .
       0100-continue.

           perform 0050-readnext       thru 0050-exit

           .
       0100-exit.
           exit.

       0200-send-buffer.

           move ws-return-stuff        to ws-send-buf
           display 'SOCK05:About to send      '
           display 'SOCK05:sequence number  =', ws-seq-num.
           display ' msg size ' ws-send-msg-size

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.

           if return-code <= zero
              display 'SOCK05:send error ',
              go to 0200-socket-error
           end-if
           go to 0200-exit

           .
       0200-socket-error.
           if ws-seq-num <> 0
              display "SOCK05:did not complete"
           end-if

           .
       0200-exit.
           exit.

       0300-close-socket.

      *    call "close" using by value GIVE-TAKE-SOCKET .
           display 'SOCK05:done'
           exec cics return end-exec.
           goback

           .
       0300-exit.
           exit.

       1000-init.

           move spaces                 to ws-return-stuff
           perform 0030-get-user       thru 0030-exit

           .
       1000-exit.
           exit.

       1100-process-elcert.

           move ws-comp-cd             to ws-cm-company-cd
           move soc-client-car         to ws-cm-carrier
           move soc-client-grp         to ws-cm-grouping
           move soc-client-st          to ws-cm-state
           move soc-client-act         to ws-cm-account

           move '2'                    to dc-option-code
           move soc-client-eff-dt      to dc-greg-date-1-edit
           perform 9700-date-link      thru 9700-exit
           if not no-conversion-error
              display ' error datecnvt eff dt ' soc-client-eff-dt ' '
              dc-error-code ' ' soc-client-cert-no
           else
              move dc-bin-date-1       to ws-cm-cert-eff-dt
           end-if

           move soc-client-cert-no     to ws-cm-cert-no
           
           EXEC CICS READ
                INTO    (certificate-master)
                DATASET ('ELCERT')
                RIDFLD  (WS-CM-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

           if resp-normal
              add +1 to p1
              move cm-cert-eff-dt      to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              move dc-greg-date-a-edit to ws-pend-eff-dte (p1)
              move cm-cert-prime       to ws-pend-cert-no (p1)
              move cm-cert-sfx         to ws-pend-cert-sfx (p1)
              move cm-insured-first-name
                                       to ws-pend-first-name (p1)
              move cm-insured-last-name
                                       to ws-pend-last-name (p1)
              move ws-manager-id       to ws-pend-manager (p1)
              move cm-control-primary  to ws-cs-key
              move 'C'                 to ws-cs-trlr-type
              
              move spaces              to ws-pend-agent-name (p1)
              EXEC CICS READ
                   INTO    (certificate-trailers)
                   DATASET ('ELCRTT')
                   RIDFLD  (WS-CS-KEY)
                   RESP    (WS-RESPONSE)
              END-EXEC
              
              if resp-normal
                 move cs-agent-edit-status
                                       to ws-pend-status (p1)
                 if cs-ae-verified
                    string
                       cs-agent-fname ' '
                       cs-agent-lname
                          delimited by '  '
                       into ws-pend-agent-name (p1)
                    end-string
                 end-if
              else
                 move ' '              to ws-pend-status (p1)
              end-if
              move ';'                 to ws-pend-semi (p1)
           else
              display ' error cert not found ' soc-client-cert-no
           end-if
           move ';'                 to ws-pend-semi (p1)

           .
       1100-exit.
           exit.

       9700-DATE-LINK.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.

