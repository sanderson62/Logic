       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK14.
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
      
      ******************************************************************
      *REMARKS.                                                        *
      *       Receives a call from Phone App with Company and User ID. *
      *   My main purpose is to gather all the unapproved claim        *
      *   payments that the user is aurhorized to approve and pass     *
      *   back the necessary data for user to approve or deny the      *
      *   claim payment.                                               *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 061515   2015022600002   PEMA  New Program
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
      ******************************************************************
       ENVIRONMENT DIVISION.
      
       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SOCK14   WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
      
       77 ws-send-msg-size           pic s9(8) comp value 19200.
       77 ws-recv-msg-size           pic s9(8) comp value 19200.
       77 ws-recv-buf                pic x(4096).
       77 ws-send-buf                pic x(19200) VALUE SPACES.
       77 ws-recv-total              pic s9(8) comp value 0.
       77 ws-recv-left               pic s9(8) comp value 0.
       77 ws-seq-num                 pic s9(8) comp value 0.
       77 ws-flags                   pic s9(8) comp value 0.
       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ERPNDB                VALUE 'Y'.
       77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
       77  WS-COMP-ID                  PIC XXX VALUE 'CID'.
       77  SUB1                        PIC S9(5) VALUE +0 COMP-3.
       77  ws-elactq-sw                pic x value ' '.
           88  end-of-elactq             value 'Y'.
       77  ws-elactq-browse-sw         pic x  value ' '.
           88  elactq-browse-started     value 'Y'.
       77  ws-eltrlr-browse-sw         pic x  value ' '.
           88  eltrlr-browse-started     value 'Y'.

       77  c1                          pic s999 value +0 comp-3.
       77  p1                          pic s999 value +0 comp-3.
       77  n1                          pic s999 value +0 comp-3.
       77  n2                          pic s999 value +0 comp-3.
       77  t1                          pic s999 value +0 comp-3.
       77  wi                          pic s999 value +0 comp-3.
       77  wo                          pic s999 value +0 comp-3.
       77  ws-match-sw                 pic x value spaces.
           88  no-matching-alpha           value 'N'.
       77  save-bin-date               pic xx value low-values.
       77  ws-current-bin-dt           pic xx value low-values.

      ******************************************************************

       01  ws-header-return-record.
           05  ws-hr-comp-id           pic xxx.
           05  ws-hr-user-id           pic xxxx.
           05  ws-hr-pmt-cntr          pic 999.
           
       01  ws-work-area-in             pic x(280) value spaces.
       01  ws-work-area-out            pic x(280) value spaces.
       01  a1                          pic s999 comp-3 value +0.
       01  ma1                         pic s999 comp-3 value +0.
       01  ws-record-area.
         02 ws-pmt-approval-record occurs 50.
           05  ar-comp-id              pic xxx.
           05  ar-carrier              pic x.
           05  ar-claim-no             pic x(7).
           05  ar-cert-no              pic x(11).
           05  ar-seq-no               pic 9999.
           05  ar-status               pic x(5).
           05  ar-claim-type           pic xxxx.
           05  ar-insured-name         pic x(30).
           05  ar-check-amt            pic $$$,$$$.99.
           05  ar-payee                pic x(30).
           05  ar-diag                 pic x(50).
           05  ar-inc-dt               pic x(10).
           05  ar-rpt-dt               pic x(10).
           05  ar-pd-from-dt           pic x(10).
           05  ar-pd-thru-dt           pic x(10).
           05  ar-rec-dt               pic x(10).
           05  ar-last-pd-dt           pic x(10).
           05  ar-no-of-pmts           pic z99.
           05  ar-total-days           pic zz99.
           05  ar-proc                 pic xxxx.
           05  ar-pd-by                pic xxxx.
           05  ar-forced-y-n           pic x.
           05  ar-pmt-type             pic x(10).
           05  ar-filler               pic x(9).

       01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
           88  RESP-NORMAL                    VALUE +0.
           88  resp-file-notfnd               value +12.
           88  RESP-NOTFND                    VALUE +13.
           88  resp-duprec                    value +14.
           88  resp-dupkey                    value +15.
           88  resp-invreq                    value +16.
           88  RESP-NOTOPEN                   VALUE +19.
           88  RESP-ENDFILE                   VALUE +20.
           88  resp-lengtherr                 value +22.
      
       01  ws-return-stuff             pic x(48000) value spaces.
       
       01  ws-name-work                pic x(30) value spaces.
       01  ws-diagnosis                pic x(50) value spaces.

       01  WS-MISC.
           05  PGM-SUB                 PIC S9(4)   VALUE +515.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  WS-ZERO                 PIC S9      VALUE ZERO.
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.
           05  ERPNDB-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ERPNDB5-FILE-STATUS     PIC XX    VALUE ZEROS.
           05  ERALPH-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ELMSTR5-FILE-STATUS     PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
      
       01  ws-cf-key.
           05  ws-cf-comp-id           pic xxx.
           05  ws-cf-rec-type          pic x.
           05  ws-cf-user              pic xxxx.
           05  ws-cf-seq-no            pic s9(4) comp value +0.
      
       01  ws-aq-key.
           05  ws-aq-company-cd        pic x.
           05  ws-aq-carrier           pic x.
           05  ws-aq-claim-no          pic x(7).
           05  ws-aq-cert-no           pic x(11).
      
       01  ws-cl-key.
           05  ws-cl-company-cd        pic x.
           05  ws-cl-carrier           pic x.
           05  ws-cl-claim-no          pic x(7).
           05  ws-cl-cert-no           pic x(11).
      
       01  ws-at-key.
           05  ws-at-company-cd        pic x.
           05  ws-at-carrier           pic x.
           05  ws-at-claim-no          pic x(7).
           05  ws-at-cert-no           pic x(11).
           05  ws-at-seq-no            pic s9(4) comp value +0.
      
                                       copy ELCACTQ.
                                       copy ELCCNTL.
                                       copy ELCMSTR.
                                       copy ELCTRLR.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
       linkage section.
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).
      ****   client-in-data must be 36 characters.  ****
         05 CLIENT-IN-DATA.
            15  CLIENT-comp-id       pic xxx.
            15  CLIENT-user-id       pic xxxx.
            15  filler               pic x(11).
            15  FILLER               PIC X(18).
            15  FILLER               PIC X(17).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).
      
       PROCEDURE DIVISION.

           perform 0000-INITIALIZE     thru 0000-exit
           perform 0010-process-elactq thru 0010-exit

           perform 0200-send-buffer    thru 0200-exit
           perform 0300-close-socket   thru 0300-exit
           exec cics return end-exec

           GOBACK

           .
       0000-INITIALIZE.
      
           exec cics
              asktime
           end-exec
      
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           perform 9700-date-convert   thru 9700-exit
      
           IF DATE-CONVERSION-ERROR
              display ' error converting eibdate '
              MOVE LOW-VALUES          TO save-bin-date
           ELSE
              MOVE DC-BIN-DATE-1       TO save-bin-date
                                          ws-current-bin-dt
           end-if
      
           move function upper-case(client-comp-id)
                                       to ws-comp-id
           evaluate true
              when ws-comp-id = 'FNL'
                 MOVE X'08'            TO ws-comp-cd
              when ws-comp-id = 'VPP'
                 MOVE X'07'            TO ws-comp-cd
              when ws-comp-id = 'AHL'
                 MOVE X'06'            TO ws-comp-cd
              when ws-comp-id = 'DCC'
                 move X'05'            to ws-comp-cd
              when other
                 move X'04'            to ws-comp-cd
           end-evaluate
      
           move ws-comp-id             to ws-cf-comp-id
           move function upper-case(client-user-id)
                                       to ws-cf-user
           move '2'                    to ws-cf-rec-type
           move +0                     to ws-cf-seq-no
           
           exec cics read
              dataset       ('ELCNTL')
              ridfld        (ws-cf-key)
              into          (control-file)
              resp          (ws-response)
           end-exec
      
           if not resp-normal
              display ' invalid user id ' ws-cf-user
              go to 0000-exit
           end-if
      
      *    display ' approval level ' cf-approval-level
           move spaces                 to ws-record-area

           .
       0000-EXIT.
           EXIT.

       0010-process-elactq.

           move ws-comp-cd             to ws-aq-key
           exec cics startbr
              dataset     ('ELACTQ')
              ridfld      (ws-aq-key)
              gteq
              resp        (ws-response)
           end-exec
           if not resp-normal
              display ' error-elactq-startbr ' ws-response
              set end-of-elactq to true
              go to 0010-exit
           end-if

           set elactq-browse-started to true

           .
       0010-readnext.

           exec cics readnext
              dataset     ('ELACTQ')
              into        (activity-que)
              ridfld      (ws-aq-key)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' error-elactq-readnext ' ws-response
              set end-of-elactq to true
              go to 0010-endbr
           end-if

           if aq-company-cd <> ws-comp-cd
              set end-of-elactq to true
              go to 0010-endbr
           end-if

           if pending-payments
              and aq-payment-counter > +0
              and aq-pmt-unapproved-count > +0
              continue
           else
              go to 0010-readnext
           end-if

      ***    Okay, we found a claim with a pending payment that is 
      *** waiting to be approved. Now, let's get the pmt trlr record
      *** to see if this user can approve this payment.

           perform 0020-process-eltrlr thru 0020-exit
           if ma1 < +50
              go to 0010-readnext
           end-if

           .
       0010-endbr.

           if elactq-browse-started
              exec cics endbr
                 dataset     ('ELACTQ')
              end-exec
           end-if

           .
       0010-exit.
           exit.

       0020-process-eltrlr.

           move spaces                 to ws-diagnosis
           move ws-aq-key              to ws-at-key
           move +90                    to ws-at-seq-no
      
           exec cics startbr
              dataset    ('ELTRLR')
              ridfld     (ws-at-key)
              resp       (ws-response)
           end-exec
      
           if not resp-normal
              display ' error-eltrlr-startbr ' ws-response
              go to 0020-exit
           end-if
           set eltrlr-browse-started to true

           .
       0020-readnext.

           exec cics readnext
              dataset    ('ELTRLR')
              into       (activity-trailers)
              ridfld     (ws-at-key)
              resp       (ws-response)
           end-exec
      
           if not resp-normal
              display ' error-eltrlr-readnext ' ws-response
              go to 0020-endbr
           end-if
      
           if at-company-cd = aq-company-cd
              and at-carrier = aq-carrier
              and at-claim-no = aq-claim-no
              and at-cert-no = aq-cert-no
              continue
           else
              go to 0020-endbr
           end-if
      
           if at-sequence-no = +90
              move at-info-line-1      to ws-diagnosis
              go to 0020-readnext
           end-if
      
           if at-trailer-type <> '2'
              go to 0020-readnext
           end-if
      
           if at-payment-approval-sw = 'U'
              and at-approved-level <= cf-approval-level
              continue
           else
              go to 0020-readnext
           end-if

      ***  Thought I would bypass this so I wouldn't have to deal
      ***  with it later.  Logic displays this on the 143A screen
      ***  but when you go to approve it, it throws and error
      ***  0629-X user cannot approve their own payments

           if at-recorded-by = ws-cf-user
              go to 0020-readnext
           end-if

      *** At this point, I believe we found a claim that the user
      *** can either approve or at least send it up to the next
      *** approval level. So let's gather all the info and send
      *** it to the phone app.           
      
           move ws-aq-key              to ws-cl-key
           exec cics read
              dataset     ('ELMSTR')
              ridfld      (ws-cl-key)
              into        (claim-master)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' error-elmstr-read ' ws-response
              go to 0020-endbr
           end-if

           add +1 to a1
           perform 0100-build-buffer   thru 0100-exit
           go to 0020-readnext

           .
       0020-endbr.

           if eltrlr-browse-started
              exec cics endbr
                 dataset      ('ELTRLR')
              end-exec
           end-if

           .
       0020-exit.
           exit.

       0100-build-buffer.
      
           move ws-comp-id             to ar-comp-id (a1)
           move at-carrier             to ar-carrier (a1)
           move at-claim-no            to ar-claim-no (a1)
           move at-cert-no             to ar-cert-no (a1)
           move at-sequence-no         to ar-seq-no (a1)
           move 'OPEN '                to ar-status (a1)
           if cl-claim-status = 'C'
              move 'CLOSE'             to ar-status (a1)
           end-if

           evaluate cl-claim-type
              when 'L'
                 move 'LIFE'           to ar-claim-type (a1)
              when 'I'
                 move ' IU '           to ar-claim-type (a1)
              when 'F'
                 move 'FAM'            to ar-claim-type (a1)
              when 'G'
                 move 'GAP'            to ar-claim-type (a1)
              when 'O'
                 move 'OTH'            to ar-claim-type (a1)
              when 'P'
                 move 'LIFE'           to ar-claim-type (a1)
              when other
                 move 'A&H'            to ar-claim-type (a1)
           end-evaluate
           
           move spaces                 to ws-name-work
      
           perform varying n1 from +15 by -1 until
              (n1 < +1)
              or (cl-insured-last-name (n1:1) <> ' ')
           end-perform
           if n1 > +0
              move cl-insured-last-name (1:n1)
                                       to ws-name-work
              move ','                 to ws-name-work(n1 + 1:1)
              add +3 to n1
           end-if
      
           perform varying n2 from +12 by -1 until
              (n2 < +1)
              or (cl-insured-1st-name (n2:1) <> ' ')
           end-perform
           if n2 > +0
              move cl-insured-1st-name (1:n2)
                                       to ws-name-work (n1:n2)
           end-if
           move ws-name-work           to ar-insured-name (a1)
      
           move at-amount-paid         to ar-check-amt (a1)
           move at-payees-name         to ar-payee (a1)
           move ws-diagnosis           to ar-diag (a1)
           
           move cl-incurred-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ar-inc-dt (a1)
           end-if
      
           move cl-reported-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ar-rpt-dt (a1)
           end-if
      
           move at-paid-from-dt        to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ar-pd-from-dt (a1)
           end-if
      
           move at-paid-thru-dt        to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ar-pd-thru-dt (a1)
           end-if
      
           move at-recorded-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ar-rec-dt (a1)
           end-if
      
           move cl-last-pmt-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ar-last-pd-dt (a1)
           end-if
      
           move cl-no-of-pmts-made     to ar-no-of-pmts (a1)

           move cl-no-of-days-paid     to ar-total-days (a1)
           move cl-processor-id        to ar-proc (a1)
           move at-recorded-by         to ar-pd-by (a1)
           if payment-was-forced
              move 'Y'                 to ar-forced-y-n (a1)
           else
              move 'N'                 to ar-forced-y-n (a1)
           end-if

           evaluate true
              when at-payment-type = '1'
                 move 'PARTIAL'        to ar-pmt-type(a1)
              when at-payment-type = '2'
                 move 'FINAL'          to ar-pmt-type(a1)
              when at-payment-type = '3'
                 move 'SETTLEMENT'     to ar-pmt-type(a1)
              when at-payment-type = '4'
                 move 'ADDITIONAL'     to ar-pmt-type(a1)
              when at-payment-type = '5'
                 move 'CHG EXP'        to ar-pmt-type(a1)
              when at-payment-type = '6'
                 move 'N-CHG EXP'      to ar-pmt-type(a1)
              when other
                 move at-payment-type  to ar-pmt-type(a1)
           end-evaluate

           move a1                     to ma1
      
           .
       0100-exit.
           exit.

       0200-send-buffer.

           move ws-comp-id             to ws-hr-comp-id
           move ws-cf-user             to ws-hr-user-id
           move ma1                    to ws-hr-pmt-cntr

           string
              ws-hr-comp-id   ';'
              ws-hr-user-id   ';'
              ws-hr-pmt-cntr  '~'
              delimited by size into ws-return-stuff
           end-string

           if ma1 = zeros
              go to 0200-send-it
           end-if


      *    perform varying a1 from +1 by +1 until a1 > ma1
      *       string
      *          ws-return-stuff delimited by '   '
      *          ar-comp-id (a1)     ';'
      *          ar-carrier (a1)     ';'
      *          ar-claim-no(a1)     ';'
      *          ar-cert-no (a1)     ';'
      *          ar-seq-no  (a1)     ';'
      *          ar-status  (a1)     ';'
      *          ar-claim-type (a1)  ';'
      *          ar-insured-name (a1) delimited by '  '
      *                              ';'
      *          ar-check-amt (a1)   delimited by '  '
      *                              ';'
      *          ar-payee (a1) delimited by '  '
      *                              ';'
      *          ar-diag (a1) delimited by '   '
      *                             ';'
      *          ar-inc-dt   (a1)   ';'
      *          ar-rpt-dt   (a1)   ';'
      *          ar-pd-from-dt (a1) delimited by ' '
      *                             ';'
      *          ar-pd-thru-dt (a1) delimited by ' '
      *                             ';'
      *          ar-rec-dt (a1)     ';'
      *          ar-last-pd-dt (a1) ';'
      *          ar-no-of-pmts (a1) delimited by size
      *                             ';'
      *          ar-total-days (a1) delimited by size
      *                             ';'
      *          ar-proc (a1)       ';'
      *          ar-pd-by  (a1)     ';'
      *          ar-forced-y-n (a1) ';'
      *          ar-pmt-type (a1)   delimited by ' '
      *                             '~'
      *             into ws-return-stuff
      *       end-string
      *    end-perform











           perform varying a1 from +1 by +1 until a1 > ma1
              string
                 ar-comp-id (a1)      ';'
                 ar-carrier (a1)      ';'
                 ar-claim-no(a1)      ';'
                 ar-cert-no (a1)      ';'
                 ar-seq-no  (a1)      ';'
                 ar-status  (a1)      ';'
                 ar-claim-type (a1)   ';'
                 ar-insured-name (a1) ';'
                 ar-check-amt (a1)    ';'
                 ar-payee (a1)        ';'
                 ar-diag (a1)         ';'
                 ar-inc-dt   (a1)     ';'
                 ar-rpt-dt   (a1)     ';'
                 ar-pd-from-dt (a1)   ';'
                 ar-pd-thru-dt (a1)   ';'
                 ar-rec-dt (a1)       ';'
                 ar-last-pd-dt (a1)   ';'
                 ar-no-of-pmts (a1)   ';'
                 ar-total-days (a1)   ';'
                 ar-proc (a1)         ';'
                 ar-pd-by  (a1)       ';'
                 ar-forced-y-n (a1)   ';'
                 ar-pmt-type (a1)     
                 ar-filler (a1)       '~'
                    delimited by size into ws-work-area-in
              end-string

              move +1                     to wo
              move spaces                 to ws-work-area-out
              perform varying wi from +1 by +1 until wi > +280
                 if ws-work-area-in (wi:2) = spaces
                    add +1 to wi
                 else
                    move ws-work-area-in(wi:1)
                                       to ws-work-area-out(wo:1)
                    add +1 to wo
                 end-if
              end-perform

              string
                 ws-return-stuff delimited by '   '
                 ws-work-area-out delimited by '  '
                    into ws-return-stuff
              end-string

           end-perform












           .
       0200-send-it.

           move ws-return-stuff        to ws-send-buf
           display 'SOCK14:About to send      '
           display 'SOCK14:sequence number  =', ws-seq-num.
           display 'SOCK14:send buffer      =', ws-send-buf(1:80).
      
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.
      
           if return-code <= zero
              display 'SOCK14:send error ',
              go to 0200-socket-error
           end-if
           go to 0200-exit
      
           .
       0200-socket-error.
           if ws-seq-num <> 0
              display "SOCK14:did not complete"
           end-if
      
           .
       0200-exit.
           exit.

       0300-close-socket.
      
           .
       0300-exit.
           exit.

       9700-DATE-CONVERT.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
