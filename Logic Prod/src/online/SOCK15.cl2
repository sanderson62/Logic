       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK15.
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
      *       Receives a call from Phone App with Company, User id,    *
      *   Carrier, Claim No, Cert No, trailer sequence no and Action   *
      *   Action being (A)pprove or (V)oid. Program will take action   *
      *   and update necessary files as per action.                    *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 020119 CR2018110200005   PEMA  New Program
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
      ******************************************************************
       ENVIRONMENT DIVISION.
      
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SOCK15   WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
      
       77 ws-send-msg-size           pic s9(8) comp value 256.
       77 ws-recv-msg-size           pic s9(8) comp value 256.
       77 ws-recv-buf                pic x(256).
       77 ws-send-buf                pic x(256) VALUE SPACES.
       77 ws-recv-total              pic s9(8) comp value 0.
       77 ws-recv-left               pic s9(8) comp value 0.
       77 ws-seq-num                 pic s9(8) comp value 0.
       77 ws-flags                   pic s9(8) comp value 0.
       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ERPNDB                VALUE 'Y'.
       77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
       77  WS-COMP-ID                  PIC XXX VALUE 'CID'.
       77  SUB1                        PIC S9(5) VALUE +0 COMP-3.

       77  b1                          pic s999 value +0 comp-3.
       77  p1                          pic s999 value +0 comp-3.
       77  n1                          pic s999 value +0 comp-3.
       77  n2                          pic s999 value +0 comp-3.
       77  t1                          pic s999 value +0 comp-3.
       77  s1                          pic s999 value +0 comp-3.
       77  s2                          pic s999 value +0 comp-3.
       77  save-bin-date               pic xx value low-values.
       77  ws-current-bin-dt           pic xx value low-values.
       77  ws-user-id                  pic xxxx value spaces.
       77  ws-user-sw                  pic x value spaces.
           88  valid-user-id             value 'Y'.
       77  ws-approval-level           pic x  value spaces.
       77  ws-amt-paid                 pic s9(9)v99 comp-3 value +0.
       77  ws-pay-type                 pic x value spaces.
       77  ws-days-in-period           pic s9(5) comp-3 value +0.
       77  ws-lf-coverage-type         pic x value ' '.
       77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
       77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
       77  ws-max-bens                 pic s999 comp-3 value +0.
       77  ws-pd-bens                  pic s9(5) comp-3 value +0.

      ******************************************************************

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
      

       01  ws-return-stuff.
           05  ws-return-num           pic x(5)  value '0000;'.
           05  ws-return-message       pic x(100) value
                      'Process successfully completed'.

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
           05  ws-display-resp         pic 9(5)  value zeros.
           05  ws-resp-message         pic x(30) value spaces.
      
       01  ws-cf-key.
           05  ws-cf-comp-id           pic xxx.
           05  ws-cf-rec-type          pic x.
           05  ws-cf-access.
               10  ws-cf-access-1-2    pic xx.
               10  ws-cf-access-3-4    pic xx.
           05  ws-cf-seq-no            pic s9(4) comp value +0.
      
       01  ws-aq-update-sw             pic x  value spaces.
           88  aq-update-on               value 'Y'.
           88  aq-update-off              value 'N'.
       01  ws-aq-key.
           05  ws-aq-company-cd        pic x.
           05  ws-aq-carrier           pic x.
           05  ws-aq-claim-no          pic x(7).
           05  ws-aq-cert-no           pic x(11).
      
       01  ws-cl-update-sw             pic x  value spaces.
           88  cl-update-on               value 'Y'.
           88  cl-update-off              value 'N'.
       01  ws-cl-key.
           05  ws-cl-company-cd        pic x.
           05  ws-cl-carrier           pic x.
           05  ws-cl-claim-no          pic x(7).
           05  ws-cl-cert-no           pic x(11).
      
       01  ws-cm-update-sw             pic x  value spaces.
           88  cm-update-on               value 'Y'.
           88  cm-update-off              value 'N'.
       01  ws-cm-key.
           05  ws-cm-company-cd        pic x.
           05  ws-cm-carrier           pic x.
           05  ws-cm-grouping          pic x(6).
           05  ws-cm-state             pic xx.
           05  ws-cm-account           pic x(10).
           05  ws-cm-eff-dt            pic xx.
           05  ws-cm-cert-no           pic x(11).

       01  ws-cs-update-sw             pic x  value spaces.
           88  cs-update-on               value 'Y'.
           88  cs-update-off              value 'N'.
       01  ws-cs-key.
           05  ws-cs-company-cd        pic x.
           05  ws-cs-carrier           pic x.
           05  ws-cs-grouping          pic x(6).
           05  ws-cs-state             pic xx.
           05  ws-cs-account           pic x(10).
           05  ws-cs-eff-dt            pic xx.
           05  ws-cs-cert-no           pic x(11).
           05  ws-cs-trlr-type         pic x.

       01  ws-at-update-sw             pic x  value spaces.
           88  at-update-on               value 'Y'.
           88  at-update-off              value 'N'.
       01  ws-at-key.
           05  ws-at-company-cd        pic x.
           05  ws-at-carrier           pic x.
           05  ws-at-claim-no          pic x(7).
           05  ws-at-cert-no           pic x(11).
           05  ws-at-seq-no            pic s9(4) comp value +0.
      
                                       copy ELCACTQ.
                                       copy ELCCNTL.
                                       copy ELCMSTR.
                                       copy ELCCERT.
                                       copy ELCTRLR.
                                       copy ELCCRTT.
                                       copy ELCDAR.
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
            15  CLIENT-carrier       pic x.
            15  client-claim-no      pic x(7).
            15  client-cert-no       pic x(11).
            15  client-seq-no        pic 9999.
            15  client-action        pic x.
            15  filler               pic x(5).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).
      
       PROCEDURE DIVISION.

           perform 0000-INITIALIZE     thru 0000-exit

           display ' Begin Processing For ' ws-comp-id ' '
              client-carrier ' ' client-claim-no ' '
              client-cert-no ' ' client-seq-no ' ' client-action

           if valid-user-id
              if client-action = 'A'
                 perform 0010-approve  thru 0010-exit
              else
                 if client-action = 'V'
                    perform 0020-void  thru 0020-exit
                 else
                    move '0099;'       to ws-return-num
                    move 'Invalid Action code, expecting A or V '
                                       to ws-return-message
                    display ' Invalid Action ' client-action
                 end-if
              end-if
           end-if

           if aq-update-on
              exec cics unlock
                 dataset   ('ELACTQ')
              end-exec
           end-if

           if at-update-on
              exec cics unlock
                 dataset   ('ELTRLR')
              end-exec
           end-if

           if cm-update-on
              exec cics unlock
                 dataset   ('ELCERT')
              end-exec
           end-if

           if cl-update-on
              exec cics unlock
                 dataset   ('ELMSTR')
              end-exec
           end-if

           if cs-update-on
              exec cics unlock
                 dataset   ('ELCRTT')
              end-exec
           end-if

           perform 2000-send-buffer    thru 2000-exit
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
      
           move ws-comp-id             to ws-cf-comp-id
           move spaces                 to ws-cf-access
           move '1'                    to ws-cf-rec-type
           move +0                     to ws-cf-seq-no
           
           exec cics read
              dataset       ('ELCNTL')
              ridfld        (ws-cf-key)
              into          (control-file)
              resp          (ws-response)
           end-exec
      
           if not resp-normal
              display ' invalid Company id ' ws-comp-id
              move '0099;'             to ws-return-num
              move 'Invalid Company ID '
                                       to ws-return-message
              go to 0000-exit
           end-if
           move cf-company-cd          to ws-comp-cd

           move ws-comp-id             to ws-cf-comp-id
           move function upper-case(client-user-id)
                                       to ws-cf-access
           move '2'                    to ws-cf-rec-type
           move +0                     to ws-cf-seq-no
           
           exec cics read
              dataset       ('ELCNTL')
              ridfld        (ws-cf-key)
              into          (control-file)
              resp          (ws-response)
           end-exec
      
           if not resp-normal
              display ' invalid user id ' ws-user-id
              move '0099;'             to ws-return-num
              move 'Invalid User ID '  to ws-return-message
              go to 0000-exit
           end-if

           set valid-user-id to true
           move ws-cf-access           to ws-user-id
           move cf-approval-level      to ws-approval-level
      *    display ' approval level ' cf-approval-level

           .
       0000-EXIT.
           EXIT.

       0010-approve.

           move ws-comp-cd             to ws-aq-key
           move client-carrier         to ws-aq-carrier
           move client-claim-no        to ws-aq-claim-no
           move client-cert-no         to ws-aq-cert-no

           exec cics read
              update
              dataset     ('ELACTQ')
              into        (activity-que)
              ridfld      (ws-aq-key)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' elactq-error-read ' ws-response
              move '0009;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'Read on ELACTQ, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0010-exit
           end-if
           set aq-update-on to true

           if aq-pmt-unapproved-count not numeric
              move +0                  to aq-pmt-unapproved-count
           end-if

           if aq-pmt-unapproved-count = +0
              display ' nothing to approve ' ws-aq-claim-no
              move '0009;'             to ws-return-num
              move 'Nothing to Approve '
                                       to ws-return-message
              go to 0010-exit
           end-if

           move ws-aq-key              to ws-at-key
           move client-seq-no          to ws-at-seq-no

           exec cics read
              update
              dataset     ('ELTRLR')
              into        (activity-trailers)
              ridfld      (ws-at-key)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' couldn''t find eltrlr ' ws-at-key (2:19) ' '
                 ws-at-seq-no
              move '0099;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'Read on ELTRLR, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0010-exit
           end-if
           set at-update-on to true

           if (at-trailer-type <> '2')
              or (at-payment-approval-sw <> 'U')
              display ' not pmt trlr or not unapproved '
              move '0009;'             to ws-return-num
              move 'Pmt Not Pending Approval '
                                       to ws-return-message
              go to 0010-exit
           end-if

           if at-approved-level < ws-approval-level
              move ws-approval-level   to at-approved-level
           end-if

           if at-approved-level <> ws-approval-level
              display ' User cannot approve payment ' at-claim-no
              move '0009;'             to ws-return-num
              move 'Invalid Level For Pmt Approval '
                                       to ws-return-message
              go to 0010-exit
           end-if

           evaluate true
              when at-approved-level = '1'
                 move '2'              to at-approved-level
              when at-approved-level = '2'
                 move '3'              to at-approved-level
              when at-approved-level = '3'
                 move '4'              to at-approved-level
              when at-approved-level = '4'
                 move '5'              to at-approved-level
              when at-approved-level = '5'
                 move '6'              to at-approved-level
           end-evaluate

           if at-approved-level > at-approval-level-reqd
              go to 0010-continue-approve
           end-if

           move ws-user-id             to at-payment-last-updated-by
           move save-bin-date          to at-payment-last-maint-dt
           move eibtime                to at-last-maint-hhmmss

      *    display ' About to rewrite ELTRLR-NEXT LEVEL'
           exec cics rewrite
              dataset     ('ELTRLR')
              from        (activity-trailers)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' error-eltrlr-rewrite ' ws-response
              move '0099;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'ReWrite on ELTRLR, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
           else
              set at-update-off to true
           end-if

           go to 0010-exit

           .
       0010-continue-approve.

           move 'A'                    to at-payment-approval-sw
           move ws-user-id             to at-payment-last-updated-by
                                          at-pmt-approved-by
           move save-bin-date          to at-payment-last-maint-dt
           move eibtime                to at-last-maint-hhmmss

      *    display ' About to rewrite ELTRLR-APPROVE'
           exec cics rewrite
              dataset     ('ELTRLR')
              from        (activity-trailers)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' error-eltrlr-rewrite-approve ' ws-response
              move '0099;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'Rewrite on ELTRLR, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0010-exit
           end-if
           set at-update-off to true

      ** The count should be > 0 but hey.....

           if aq-pmt-unapproved-count > +0
              subtract +1 from aq-pmt-unapproved-count
           end-if

      *    display ' About to rewrite ELACTQ '
           exec cics rewrite
              dataset     ('ELACTQ')
              from        (activity-que)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' error-elactq-rewrite-approve ' ws-response
              move '0099;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'Rewrite on ELACTQ, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0010-exit
           end-if
           set aq-update-off to true

           .
       0010-exit.
           exit.

       0020-void.

           move ws-comp-cd             to ws-aq-key
           move client-carrier         to ws-aq-carrier
           move client-claim-no        to ws-aq-claim-no
           move client-cert-no         to ws-aq-cert-no

           exec cics read
              update
              dataset     ('ELACTQ')
              into        (activity-que)
              ridfld      (ws-aq-key)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' elactq-error-read ' ws-response
              move '0009;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'Read on ELACTQ, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0020-exit
           end-if
           set aq-update-on  to true

           if aq-pmt-unapproved-count not numeric
              move +0                  to aq-pmt-unapproved-count
           end-if

           if aq-pmt-unapproved-count = +0
              display ' nothing to Void ' ws-aq-claim-no
              move '0009;'             to ws-return-num
              move 'Payment already processed '
                                       to ws-return-message
              go to 0020-exit
           end-if

           move ws-aq-key              to ws-at-key
           move client-seq-no          to ws-at-seq-no

           exec cics read
              update
              dataset     ('ELTRLR')
              into        (activity-trailers)
              ridfld      (ws-at-key)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' couldn''t find eltrlr ' ws-at-key (2:19) ' '
                 ws-at-seq-no
              move '0009;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'Read on ELTRLR, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0020-exit
           end-if
           set at-update-on to true

           if at-payment-approval-sw <> 'U'
              display ' pmt not unapproved '
              move '0009;'             to ws-return-num
              move 'Pmt Not Pending Approval '
                                       to ws-return-message
              go to 0020-exit
           end-if

           move at-control-primary (1:20)
                                       to ws-cl-key

           exec cics read
              update
              dataset     ('ELMSTR')
              into        (claim-master)
              ridfld      (ws-cl-key)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' couldn''t find elmstr ' ws-cl-key (2:19)
              move '0099;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'Read on ELMSTR, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0020-exit
           end-if
           set cl-update-on to true

           move at-payment-type        to ws-pay-type
           move at-amount-paid         to ws-amt-paid
           move at-days-in-period      to ws-days-in-period

           IF ws-pay-type <> '5' AND '6'
              SUBTRACT AT-AMOUNT-PAID    FROM CL-TOTAL-PAID-AMT
              SUBTRACT AT-DAYS-IN-PERIOD FROM CL-NO-OF-DAYS-PAID
              IF AT-PAYMENT-TYPE <> '4'
                 SUBTRACT +1             FROM CL-NO-OF-PMTS-MADE
                 IF (AT-PAID-THRU-DT <> CL-PAID-THRU-DT)
                     or (AT-RECORDED-BY = 'ZZZZ')
                     continue
                 ELSE
                    MOVE AT-PREV-LAST-PMT-DT
                                       TO CL-LAST-PMT-DT
                    MOVE AT-PREV-PAID-THRU-DT
                                       TO CL-PAID-THRU-DT
                    MOVE AT-PREV-LAST-PMT-AMT
                                       TO CL-LAST-PMT-AMT
                 end-if
              end-if
           end-if

           IF CL-TOTAL-PAID-AMT < +0
              MOVE +0                  TO CL-TOTAL-PAID-AMT
           end-if

           IF CL-NO-OF-DAYS-PAID < +0                          
              MOVE +0                  TO CL-NO-OF-DAYS-PAID
           end-if
                                                                       
           IF CL-NO-OF-PMTS-MADE < +0
              MOVE +0                  TO CL-NO-OF-PMTS-MADE
           end-if

           MOVE SAVE-BIN-DATE          TO AT-VOID-DT
                                          CL-LAST-REOPEN-DT

           MOVE 'PAYMENT DISAPPROVED'  TO AT-VOID-REASON
           MOVE 'V'                    TO AT-PAYMENT-APPROVAL-SW

           MOVE LOW-VALUES             TO AT-PMT-SELECT-DT
                                          AT-VOID-SELECT-DT

           MOVE ws-user-id             TO AT-PAYMENT-LAST-UPDATED-BY
           MOVE SAVE-BIN-DATE          TO AT-PAYMENT-LAST-MAINT-DT
           MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS

062121     IF ws-comp-id = 'CID' OR 'AHL' or 'VPP'
              move spaces              to daily-activity-record
              move ws-cl-key           to da-key
              move cl-trailer-seq-cnt  to da-trailer-seq-no
              move 'V'                 to da-record-type
      *       display ' About to write DLYACTV '
              exec cics write
                 dataset    ('DLYACTV')
                 ridfld     (da-key)
                 from       (daily-activity-record)
                 length     (25)
                 resp       (ws-response)
              end-exec
              move zeros to ws-response
              if not resp-normal
                 display ' error-dlyactv-write ' ws-response
                 move '0009;'             to ws-return-num
                 perform 0100-format-resp thru 0100-exit
                 string
                    'Write on DLYACTV, RESP= '
                    ws-resp-message delimited by size
                                       into ws-return-message
                 end-string
              end-if
           end-if

      *    display ' About to rewrite ELTRLR-PMT '
           exec cics rewrite
              dataset     ('ELTRLR')
              from        (activity-trailers)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' error-eltrlr-rewrite ' ws-response
              move '0099;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'ReWrite on ELTRLR, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0020-exit
           end-if
           set at-update-off to true

           move cl-control-primary     to ws-at-key
           move +0                     to ws-at-seq-no
           exec cics read
              update
              dataset     ('ELTRLR')
              ridfld      (ws-at-key)
              into        (activity-trailers)
              resp        (ws-response)
           end-exec
           if not resp-normal
              display ' error-eltrlr-read zero trlr ' ws-response
              move '0099;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'Read on ELTRLR-0TRLR, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0020-elcert
           end-if
           set at-update-on to true

           if ws-pay-type = '5'
              subtract ws-amt-paid from at-itd-chargeable-expense
           end-if
           if ws-pay-type = '6'
              subtract ws-amt-paid from at-itd-paid-expenses
           end-if
           if at-initial-manual-reserve <> +0
              add ws-amt-paid to at-current-manual-reserve
           end-if

           if not claim-is-open
              move +1                  to t1
              perform varying t1 from +1 by +1 until
                 (t1 > +6)
                 or (at-open-close-type (t1) = spaces)
              end-perform
              if t1 > +6
                 move +6               to t1
                 move at-open-close-history (2)
                                       to at-open-close-history (1)
                 move at-open-close-history (3)
                                       to at-open-close-history (2)
                 move at-open-close-history (4)
                                       to at-open-close-history (3)
                 move at-open-close-history (5)
                                       to at-open-close-history (4)
                 move at-open-close-history (6)
                                       to at-open-close-history (5)
                 move spaces           to at-open-close-history (6)
              end-if
              move save-bin-date       to at-open-close-date (t1)
              move 'O'                 to at-open-close-type (t1)
              move 'FORCE'             to at-open-close-reason (t1)
              MOVE ws-user-id          TO AT-reserves-LAST-UPDATED-BY
              MOVE SAVE-BIN-DATE       TO AT-reserves-LAST-MAINT-DT
              MOVE EIBTIME             TO AT-LAST-MAINT-HHMMSS

      *       display ' About to rewrite ELTRLR-0TRLR '
              exec cics rewrite
                 dataset   ('ELTRLR')
                 from      (activity-trailers)
                 resp      (ws-response)
              end-exec
              if not resp-normal
                 display ' error-eltrlr-rewrite zero trlr '
                    ws-response
                 move '0099;'          to ws-return-num
                 perform 0100-format-resp
                                       thru 0100-exit
                 string
                    'ReWrite on ELTRLR-0TRLR, RESP= '
                    ws-resp-message delimited by size
                                       into ws-return-message
                 end-string
              else
                 set at-update-off to true
              end-if
           end-if

           .
       0020-elcert.

           move ws-comp-cd             to ws-cm-key
           move cl-cert-key-data       to ws-cm-key (2:21)
           move cl-cert-no             to ws-cm-cert-no

           exec cics read
              update
              dataset    ('ELCERT')
              ridfld     (ws-cm-key)
              into       (certificate-master)
              resp       (ws-response)
           end-exec

           if not resp-normal
              display ' error-elcert-read ' ws-response
              move '0099;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'Read on ELCERT, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0020-exit
           end-if
           set cm-update-on to true

           if cl-claim-type <> 'L' and 'O' and 'P'
              go to 0020-void-disability
           end-if

           move ws-comp-id             to ws-cf-comp-id
           move spaces                 to ws-cf-access
           move cm-lf-benefit-cd       to ws-cf-access-3-4
           move '4'                    to ws-cf-rec-type
           move +0                     to ws-cf-seq-no
           
           exec cics read
              dataset       ('ELCNTL')
              ridfld        (ws-cf-key)
              into          (control-file)
              resp          (ws-response)
              gteq
           end-exec
      
           if not resp-normal
              display ' invalid Lf Benefit cd ' ws-cf-access
              move '0099;'             to ws-return-num
              move 'Invalid LF Benefit CD '
                                       to ws-return-message
              go to 0020-exit
           end-if

           perform varying b1 from +1 by +1 until
              (b1 > +8)
              or (cf-benefit-code (b1) = cm-lf-benefit-cd)
           end-perform
           
           if b1 <= +8
              move cf-lf-coverage-type (b1)
                                       to ws-lf-coverage-type
           end-if

           if cl-claim-type = 'P'
              or ws-lf-coverage-type = 'P'
              if ws-pay-type = '4'
                 subtract ws-amt-paid from cm-lf-itd-death-amt
                 if cm-lf-current-status <> '1' and '2'
                    move cm-lf-status-at-death
                                       to cm-lf-current-status
                    move spaces        to cm-lf-status-at-death
                    move low-values    to cm-lf-death-exit-dt
                                          cm-lf-death-dt
                 end-if
                 go to 0020-rewrite-cert
              end-if
           end-if

           if ws-pay-type = '4'
              subtract ws-amt-paid from cm-lf-itd-death-amt
              go to 0020-rewrite-cert
           end-if

           if ws-pay-type = '2'
              if cm-lf-current-status = '1' or '2'
                 subtract ws-amt-paid from cm-lf-itd-death-amt
                 go to 0020-rewrite-cert
              else
                 move cm-lf-status-at-death
                                       to cm-lf-current-status
                 subtract ws-amt-paid from cm-lf-itd-death-amt
                 move spaces           to cm-lf-status-at-death
                 move low-values       to cm-lf-death-exit-dt
                                          cm-lf-death-dt
                 go to 0020-rewrite-cert
              end-if
           else
              go to 0020-update-elactq
           end-if

           .
       0020-void-disability.

           if ws-pay-type = '4'
              subtract ws-amt-paid from cm-ah-itd-ah-pmt
              go to 0020-rewrite-cert
           end-if

           if ws-pay-type <> '3'
              go to 0020-update-elactq
           end-if

           move cm-ah-status-at-settlement
                                       to cm-ah-current-status
           move spaces                 to cm-ah-status-at-settlement
           subtract ws-amt-paid from cm-ah-itd-lump-pmt
           move low-values             to cm-ah-settlement-dt
                                          cm-ah-settlement-exit-dt

           .
       0020-rewrite-cert.

      *    display ' About to rewrite ELCERT '
           exec cics rewrite
              dataset     ('ELCERT')
              from        (certificate-master)
              resp        (ws-response)
           end-exec

           if not resp-normal
              display ' error-elcert-rewrite ' ws-response
              move '0099;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'ReWrite on ELCERT, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0020-exit
           end-if
           set cm-update-off to true

           .
       0020-update-elactq.

           if aq-pmt-unapproved-count not numeric
              move zeros               to aq-pmt-unapproved-count
           end-if
           if aq-payment-counter not numeric
              move zeros               to aq-pmt-unapproved-count
           end-if

           if aq-pmt-unapproved-count > +0
              subtract +1 from aq-pmt-unapproved-count
           end-if
           if aq-payment-counter > +0
              subtract +1 from aq-payment-counter
           end-if

           if aq-payment-counter = +0
              move spaces             to aq-pending-payment-flag
           end-if

           if aq-pending-activity-flags = spaces
      *       display ' About to DELETE ELACTQ '
              exec cics delete
                 dataset   ('ELACTQ')
                 resp      (ws-response)
              end-exec
              if not resp-normal
                 display ' error-elactq-delete ' ws-response
                 move '0099;'          to ws-return-num
                 perform 0100-format-resp
                                       thru 0100-exit
                 string
                    'Delete on ELACTQ, RESP= '
                    ws-resp-message delimited by size
                                       into ws-return-message
                 end-string
                 go to 0020-rewrite-elmstr
              end-if
              set aq-update-off to true
           else
      *       display ' About to rewrite ELACTQ '
              exec cics rewrite
                 dataset   ('ELACTQ')
                 from      (activity-que)
                 resp      (ws-response)
              end-exec

              if not resp-normal
                 display ' error-elactq-rewrite ' ws-response
                 move '0099;'          to ws-return-num
                 perform 0100-format-resp
                                       thru 0100-exit
                 string
                    'ReWrite on ELACTQ, RESP= '
                    ws-resp-message delimited by size
                                       into ws-return-message
                 end-string
                 go to 0020-rewrite-elmstr
              end-if
              set aq-update-off to true
           end-if

           .
       0020-rewrite-elmstr.

           if ws-pay-type = '4' or '5' or '6'
              continue
           else
              move 'O'                 to cl-claim-status
           end-if

           if ws-pay-type <> '5' and '6' and 'I'
              perform 0500-update-elcrtt thru 0500-exit
           end-if

      *    display ' About to rewrite ELMSTR '
           exec cics rewrite
              dataset   ('ELMSTR')
              from      (claim-master)
              resp      (ws-response)
           end-exec

           if not resp-normal
              display ' error-elmstr-rewrite ' ws-response
              move '0099;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'ReWrite on ELMSTR, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
           end-if
           set cl-update-off to true

           .
       0020-exit.
           exit.

       0100-format-resp.

           evaluate true
              when ws-response = 12
                 move ' File Not Found '
                                       to ws-resp-message
              when ws-response = 13
                 move ' Not Found '    to ws-resp-message
              when ws-response = 14
                 move ' Dup Record '   to ws-resp-message
              when ws-response = 15
                 move ' Dup Key '      to ws-resp-message
              when ws-response = 16
                 move ' Invalid Req '  to ws-resp-message
              when ws-response = 19
                 move ' File Not Open '
                                       to ws-resp-message
              when ws-response = 20
                 move ' End Of File '  to ws-resp-message
              when ws-response = 22
                 move ' Length Error ' to ws-resp-message
              when other
                 move ws-response      to ws-display-resp
                 move ws-display-resp  to ws-resp-message
           end-evaluate

           .
       0100-exit.
           exit.

       0500-update-elcrtt.

           move ws-cm-key              to ws-cs-key
           move 'B'                    to ws-cs-trlr-type

           exec cics read
              update
              dataset   ('ELCRTT')
              ridfld    (ws-cs-key)
              into      (certificate-trailers)
              resp      (ws-response)
           end-exec

           if not resp-normal
              display ' error-elcrtt-rewrite ' ws-response
              move '0099;'             to ws-return-num
              perform 0100-format-resp thru 0100-exit
              string
                 'Read on ELCRTT, RESP= '
                 ws-resp-message delimited by size
                                       into ws-return-message
              end-string
              go to 0500-exit
           end-if
           set cs-update-on to true

           perform varying s1 from +1 by +1 until
              (s1 > +24)
              or (cl-claim-no = cs-claim-no (s1))
           end-perform

           if s1 < +25
              subtract ws-amt-paid from cs-total-paid (s1)
              if cs-total-paid (s1) < zeros
                 move zeros            to cs-total-paid (s1)
              end-if
              subtract at-days-in-period from cs-days-paid (s1)
              if cs-days-paid (s1) < zeros
                 move zeros            to cs-days-paid (s1)
              end-if
              if cl-claim-type not = 'L' and 'P' and 'O'
                 perform 0600-calc-rem-bens
                                       thru 0600-exit
              end-if
      *       display ' About to rewrite ELCRTT '
              exec cics rewrite
                 dataset    ('ELCRTT')
                 from       (certificate-trailers)
                 resp       (ws-response)
              end-exec

              if not resp-normal
                 display ' error-elcrtt-rewrite ' ws-response
                 move '0099;'          to ws-return-num
                 perform 0100-format-resp
                                       thru 0100-exit
                 string
                    'ReWrite on ELCRTT, RESP= '
                    ws-resp-message delimited by size
                                       into ws-return-message
                 end-string
                 go to 0500-exit
              end-if
              set cs-update-off to true
           end-if              

           .
       0500-exit.
           exit.

       0600-calc-rem-bens.

           move cm-ah-orig-term        to ws-max-bens
           if cl-critical-period not = zeros and spaces
              move cl-critical-period  to ws-max-bens
           end-if

           move zeros                  to ws-tot-days-paid
                                          ws-tot-amt-paid

           perform varying s2 from +1 by +1 until
              (s2 > +24)
              or (cs-claim-no (s2) = spaces)
              if (cs-benefit-period (s2) = cl-benefit-period)
                 and (cs-insured-type (s2) = cl-insured-type)
                 and (cs-claim-type (s2) = cl-claim-type)
                 compute ws-tot-days-paid =
                    ws-tot-days-paid + cs-days-paid (s2)
                 compute ws-tot-amt-paid =
                    ws-tot-amt-paid + cs-total-paid (s2)
              end-if
           end-perform
           compute cs-remaining-bens (s1) =
              ws-max-bens / cm-ah-benefit-amt
           if cs-remaining-bens (s1) < zeros
              move zeros            to cs-remaining-bens (s1)
           end-if

           .
       0600-exit.
           exit.

       2000-send-buffer.

           move ws-return-stuff        to ws-send-buf
      
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.
      
           if return-code <= zero
              display 'SOCK15:send error ',
              go to 2000-socket-error
           end-if
           go to 2000-exit
      
           .
       2000-socket-error.
           if ws-seq-num <> 0
              display "SOCK15:did not complete"
           end-if
      
           .
       2000-exit.
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
