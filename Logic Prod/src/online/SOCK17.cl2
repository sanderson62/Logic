       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK17.
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
      *        Reads the pending issue record passed to it.            *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 111020 CR2020061000002   PEMA  New Program
      ******************************************************************
       ENVIRONMENT DIVISION.
      
       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SOCK17   WORKING STORAGE     '.
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
       77  WS-ERALPH-SW                PIC X VALUE SPACES.
           88  END-OF-ERALPH                VALUE 'Y'.
       77  WS-ERPNDB5-SW               PIC X VALUE SPACES.
           88  END-OF-ERPNDB5               VALUE 'Y'.
       77  ERPNDB-RECS-IN              PIC 9(9) VALUE ZEROS.
       77  ERPNDB-RECS-OUT             PIC 9(9) VALUE ZEROS.
       77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
       77  WS-COMP-ID                  PIC XXX VALUE 'CID'.
       77  SUB1                        PIC S9(5) VALUE +0 COMP-3.
       77  WS-PREV-PNDB-KEY            PIC X(23) VALUE LOW-VALUES.
       77  c1                          pic s999 value +0 comp-3.
       77  a1                          pic s999 value +0 comp-3.
       77  p1                          pic s999 value +0 comp-3.
       77  n1                          pic s999 value +0 comp-3.
       77  t1                          pic s999 value +0 comp-3.
       77  ws-match-sw                 pic x value spaces.
           88  no-matching-alpha           value 'N'.
       77  ws-save-issue-key           pic x(36)  value spaces.
       77  ws-last-name                pic x(15)  value spaces.
       77  ws-elcert-last-name         pic x(15)  value spaces.
       77  ws-first-three              pic xxx  value spaces.
       77  ws-age                      pic 999 value zeros.
       77  ws-cov-sw                   pic x value spaces.
           88  processing-primary         value 'P'.
           88  processing-secondary        value 'S'.
       77  ws-erpndb2-startbr-sw       pic x  value spaces.
           88  erpndb2-startbr           value 'Y'.
       77  ws-eralph2-startbr-sw       pic x  value spaces.
           88  eralph2-startbr           value 'Y'.
       77  ws-pend-match               pic x value ' '.
           88  pend-match                 value 'Y'.
       77  ws-elmstr5-startbr-sw       pic x  value spaces.
           88  elmstr5-startbr           value 'Y'.
       77  save-bin-date               pic xx value low-values.
       77  ws-cert-note-generic-key-len pic s9(4) comp value +34.
       77  note-count                  pic s999 comp-3 value +0.
       77  ws-build-note-sw            pic x value ' '.
           88  finished-with-notes      value 'Y'.
       77  ws-ercnot-sw                pic x  value spaces.
           88  ercnot-startbr            value 'Y'.
       77  ws-current-bin-dt           pic xx value low-values.
       77  ws-work-age                 pic s999 comp-3 value zeros.
       77  ws-cm-pri-curr-age          pic s999 comp-3 value +0.
       77  ws-cm-cob-curr-age          pic s999 comp-3 value +0.
       77  ws-p5-pri-curr-age          pic s999 comp-3 value +0.
       77  ws-p5-cob-curr-age          pic s999 comp-3 value +0.
       77  ws-alph-curr-age            pic s999 comp-3 value +0.
      
       01  cert-note-records-holder.
           05  cert-note-record occurs 300.
               10  filler              pic x(48).
               10  cnr-rest            pic x(102).
      
      ******************************************************************
      
       01  ws-hold-elcert              pic x(450) value spaces.
       01  ws-return-stuff.
           05  ws-eralph-table occurs 25.
               10  ws-cov-type         pic x.
               10  ws-tbl-last-name    pic x(15).
               10  ws-first-name       pic x(10).
               10  ws-eff-dt           pic x(10).
               10  ws-eralph-cert      pic x(11).
               10  ws-eralph-age       pic 99.
               10  ws-eralph-prm-sec   pic x.
               10  ws-eralph-lf-cd     pic xx.
               10  ws-eralph-lf-remamt pic 9(9).99.
               10  ws-eralph-lf-remterm pic 999.
               10  ws-lf-exp-dt        pic x(10).
               10  ws-eralph-ah-cd     pic xx.
               10  ws-eralph-ah-amt    pic 9(7).99.
               10  ws-eralph-ah-remamt pic 9(9).99.
               10  ws-eralph-ah-remterm pic 999.
               10  ws-ah-exp-dt        pic x(10).
               10  ws-pend             pic x.
               10  ws-claim            pic x.
               10  ws-delimiter        pic x.
      
       01  ws-alpha-table.
           05  ws-eralph-print-table occurs 25.
               10  pt-eralph-cert      pic x(11).
               10  pt-eralph-lf-cd     pic xx.
               10  pt-eralph-lf-remamt pic -zzz,zz9,999.99.
               10  pt-eralph-ah-cd     pic xx.
               10  pt-eralph-ah-amt    pic -z,zzz,z99.99.
               10  pt-eralph-ah-remamt pic -zzz,zzz,999.99.
      
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
      
       01  ws-alpha-last-name          pic x(15) value spaces.
       01  ws-p5-last-name             pic x(15) value spaces.
       01  ws-p5-jnt-last-name         pic x(15) value spaces.
       01  ws-name-in                  pic x(15) value spaces.
       01  ws-name-out                 pic x(15) value spaces.
       01  ws-age-from-pndb            pic 99.
       01  ws-lf-tot-benefit           pic s9(9)v99 comp-3 value +0.
       01  ws-ah-tot-benefit           pic s9(9)v99 comp-3 value +0.
       01  pt-lf-tot-benefit           pic -zzz,zzz,999.99.
       01  pt-ah-tot-benefit           pic -zzz,zzz,999.99.
       01  WS-ERPNDB-REC-HOLD          PIC X(585) VALUE SPACES.
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
           05  lf-joint-ind            pic x   value spaces.
           05  ah-joint-ind            pic x   value spaces.
           05  ws-lf-ben-code          pic xx.
           05  ws-ah-ben-code          pic xx.
           05  ws-lf-calc-cd           pic x   value spaces.
           05  ws-lf-coverage-type     pic x   value spaces.
           05  ws-lf-earnings-calc     pic x   value spaces.
           05  ws-lf-kind              pic x   value spaces.
           05  ws-lf-ben-descrip       pic x(10) value spaces.
           05  ws-ah-calc-cd           pic x   value spaces.
           05  ws-ah-coverage-type     pic x   value spaces.
           05  ws-ah-earnings-calc     pic x   value spaces.
           05  ws-ah-kind              pic x   value spaces.
           05  ws-ah-ben-descrip       pic x(10) value spaces.
      
       01  ws-elcntl-key.
           05  ws-elcntl-company-id    pic xxx.
           05  ws-elcntl-rec-type      pic x.
               88  ws-elcntl-lf-ben-cd    value '4'.
               88  ws-elcntl-ah-ben-cd    value '5'.
           05  filler                  pic xx.
           05  ws-elcntl-hi-ben-cd     pic xx.
           05  ws-elcntl-seq-no        pic s9(4) comp.
      
       01  ws-erpndb-key.
           05  ws-erpndb-company-cd    pic x.
           05  ws-erpndb-batch         pic x(6).
           05  ws-erpndb-seq-no        pic s9(4) comp.
           05  ws-erpndb-chg-seq-no    pic s9(4) comp.
      
       01  ws-elcert2-orig-key        pic x(18).
       01  ws-elcert-orig-key         pic x(33).
       01  ws-elcert-key.
           05  ws-elcert-company-cd   pic x.
           05  ws-elcert-carrier      pic x.
           05  ws-elcert-grouping     pic x(6).
           05  ws-elcert-state        pic xx.
           05  ws-elcert-account      pic x(10).
           05  ws-elcert-eff-dt       pic xx.
           05  ws-elcert-cert-no      pic x(11).

       01  ws-elcert2-key.
           05  ws-elcert2-company-cd  pic x.
           05  ws-elcert2-last-name   pic x(15).
           05  ws-elcert2-initials    pic xx.

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
      
       01  ws-erpndb2-key.
           05  ws-erpndb2-company-cd   pic x.
           05  ws-erpndb2-carrier      pic x.
           05  ws-erpndb2-grouping     pic x(6).
           05  ws-erpndb2-state        pic xx.
           05  ws-erpndb2-account      pic x(10).
           05  ws-erpndb2-eff-dt       pic xx.
           05  ws-erpndb2-cert-no      pic x(11).
           05  ws-erpndb2-seq-no       pic s9(4) comp.
           05  ws-erpndb2-rec-type     pic x.
      
       01  ws-erpndb5-key.
           05  ws-erpndb5-company-cd   pic x.
           05  ws-erpndb5-carrier      pic x.
           05  ws-erpndb5-grouping     pic x(6).
           05  ws-erpndb5-state        pic xx.
           05  ws-erpndb5-account      pic x(10).
           05  ws-erpndb5-eff-dt       pic xx.
           05  ws-erpndb5-cert-no      pic x(11).
           05  ws-erpndb5-seq-no       pic s9(4) comp.
           05  ws-erpndb5-rec-type     pic x.
      
       01  ws-elmstr5-key.
           05  ws-elmstr5-company-cd  pic x.
           05  ws-elmstr5-cert-no     pic x(11).
      
       01  ws-eralph-aix-key.
           05  ws-eralph-aix-company-cd pic x.
           05  ws-eralph-aix-account   pic x(10).
           05  ws-eralph-aix-lname     pic x(15).
           05  ws-eralph-aix-1st-three pic xxx.
           05  filler                  pic x(8).
      
       01  ws-passed-key-area.
           05  ws-passed-pend-key.
               10  ws-pass-company-cd  pic x.
               10  ws-pass-carrier     pic x.
               10  ws-pass-grouping    pic x(6).
               10  ws-pass-state       pic xx.
               10  ws-pass-account     pic x(10).
               10  ws-pass-eff-dt      pic xx.
               10  ws-pass-cert-no     pic x(11).
               10  ws-pass-seq-no      pic s9(4) comp.
               10  ws-pass-rec-type    pic x.
      
           05  ws-return-area.
               10  ws-exceed-limit-yn  pic x.
               10  ws-has-claim-yn     pic x.
           05  filler                  pic x(62).
      
                                       copy ELCCALC.
                                       copy ELCCNTL.
                                       copy ELCCERT.
                                       copy ELCMSTR.
                                       copy ERCALPH.
                                       COPY ERCPNDB.
                                       COPY ERCPNDB
            REPLACING LEADING ==PB== BY ==P5==
            PENDING-BUSINESS BY P5-PENDING-BUSINESS.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
                                       COPY ERCCNOT.
       linkage section.
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).
      ****   client-in-data must be 36 characters.  ****
         05 CLIENT-IN-DATA.
            15  client-comp-id       pic xxx.
            15  client-carrier       pic x.
            15  client-state         pic xx.
            15  client-account       pic x(10).
            15  client-eff-dt        pic 9(08).
            15  client-cert-no       pic x(11).
            15  filler               pic x.
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).
      
      
       PROCEDURE DIVISION.
      
      * when calling a C function the function returns its value
      * in the system variable return code.
      *
           perform 0000-INITIALIZE     thru 0000-exit
      
           if resp-normal
              PERFORM 0050-PROCESS-INPUT
                                       THRU 0050-EXIT
           else
              display ' certificate   not found ' client-carrier ' '
              client-state ' ' client-account ' ' client-cert-no
              move ' issue rec not found '
                                       to ws-return-stuff
           end-if
           
           perform 0200-send-buffer    thru 0200-exit
           perform 0300-close-socket   thru 0300-exit
           exec cics return end-exec.
      
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
      
           move client-comp-id         to ws-comp-id
           evaluate true
              when ws-comp-id = 'AHL'
                 MOVE X'06'            TO ws-comp-cd
              when ws-comp-id = 'DCC'
                 move X'05'            to ws-comp-cd
              when other
                 move X'04'            to ws-comp-cd
           end-evaluate
           move spaces                 to ws-alpha-table
      
           PERFORM 0040-READ-ELCERT    THRU 0040-EXIT
           if resp-normal
              perform 0045-get-ben-codes
                                       thru 0045-exit
           end-if
      
           .
       0000-EXIT.
           EXIT.
      
       0040-READ-ELCERT.
      
           move ws-comp-cd             to ws-elcert-company-cd
           move client-carrier         to ws-elcert-carrier
           move '000000'               to ws-elcert-grouping
           move client-state           to ws-elcert-state
           move client-account         to ws-elcert-account
           move client-eff-dt          to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-elcert-eff-dt
           end-if
           move client-cert-no         to ws-elcert-cert-no
      
           exec cics read
              dataset     ('ELCERT')
              into        (certificate-master)
              ridfld      (ws-elcert-key)
              resp        (ws-response)
           end-exec
      
           .
       0040-EXIT.
           EXIT.
      
       0045-get-ben-codes.

           if cm-lf-benefit-cd not = '00' and spaces
              move cm-lf-benefit-cd    to ws-lf-ben-code
              perform 0400-get-lf-code thru 0400-exit
              if c1 < +9
                 move cf-joint-indicator (c1)
                                       to lf-joint-ind
              end-if
           end-if
           if cm-ah-benefit-cd not = '00' and spaces
              move cm-ah-benefit-cd    to ws-ah-ben-code
              perform 0410-get-ah-code thru 0410-exit
              if c1 < +9
                 move cf-joint-indicator (c1)
                                       to ah-joint-ind
              end-if
           end-if
      
           .
       0045-exit.
           exit.
      
       0050-PROCESS-INPUT.

           move cm-control-primary     to ws-elcert-orig-key
           perform 0100-check-cancel   thru 0100-exit

           if ((cm-lf-benefit-cd <> spaces and zeros)
              or (cm-ah-benefit-cd <> spaces and zeros))
              and (cm-entry-status <> '5' and '9' and 'D' and 'V')
              display ' must not be cancelled '
              move +0                  to a1
              move spaces              to ws-alpha-table
                                          ws-return-stuff
              move 'P'                 to ws-cov-sw
              move cm-insured-last-name
                                       to ws-name-in
              perform 0650-set-last-name
                                       thru 0650-exit
              move ws-name-out         to ws-last-name
      *       display ' ws last name ' ws-last-name
              move cm-insured-first-name (1:3)
                                       to ws-first-three
      *       display ' ws 1st 3 ' ws-first-three
              perform 0700-calc-cur-cm-ages
                                       thru 0700-exit
              move ws-cm-pri-curr-age  to ws-age
      *       display ' pri age ' ws-age
              perform 0150-process-continue
                                       thru 0150-exit
              if ((lf-joint-ind = 'J')
                 or (ah-joint-ind = 'J'))
                         and 
                    (cm-jt-last-name not = spaces)
                 move ws-hold-elcert   to certificate-master
                 move ' '              to WS-ERPNDB5-SW
                 move 'S'              to ws-cov-sw
                 move cm-jt-last-name  to ws-name-in
                 perform 0650-set-last-name
                                       thru 0650-exit
                 move ws-name-out      to ws-last-name
                 move cm-jt-first-name (1:3)
                                       to ws-first-three
                 move ws-cm-cob-curr-age
                                       to ws-age
      *          display ' COB age ' ws-age
                 perform 0150-process-continue
                                       thru 0150-exit
              end-if
           else
              display ' certificate cancelled ' client-carrier ' '
              client-state ' ' client-account ' ' client-cert-no
              move ' certificate cancelled '
                                       to ws-return-stuff

           end-if
      
           .
       0050-EXIT.
           EXIT.
      
       0100-check-cancel.

      *** check to see if the certificate   is cancelled

           move certificate-master     to ws-hold-elcert
           if cm-lf-benefit-cd <> '00' and '  '
              if cm-lf-cancel-dt <> low-values
                 move '00'             to cm-lf-benefit-cd
              end-if
           end-if
           if cm-ah-benefit-cd <> '00' and '  '
              if cm-ah-cancel-dt <> low-values
                 move '00'             to cm-ah-benefit-cd
              end-if
           end-if
      
           .
       0100-exit.
           exit.
      
       0150-process-continue.

      ****  First, move elcert  record to first occurance of table
      
           move ' '                    to ws-match-sw
      
           move low-values             to ws-eralph-aix-key
           move cm-company-cd          TO ws-eralph-aix-company-cd
           MOVE cm-ACCOUNT             TO ws-eralph-aix-account
           MOVE ws-last-name           to ws-eralph-aix-lname
           move ws-first-three         to ws-eralph-aix-1st-three
      
           add +1                      to a1
           move ws-cov-sw              to ws-cov-type (a1)
           if cm-lf-policy-pending or
              cm-ah-policy-pending
              move 'Y'                 to ws-pend (a1)
           end-if
           move ws-age                 to ws-eralph-age (a1)
           if processing-primary
              move cm-insured-first-name
                                       to ws-first-name (a1)
           else
              move cm-jt-first-name    to ws-first-name (a1)
           end-if
           move ws-last-name           to ws-tbl-last-name (a1)
           move cm-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ws-eff-dt (a1)
           end-if
           move cm-cert-no             to ws-eralph-cert (a1)
      
           move cm-lf-benefit-cd       to ws-eralph-lf-cd (a1)
      
           if ((ws-cov-sw = 'P')
                    or
              ((ws-cov-sw = 'S')
               and (lf-joint-ind = 'J')))
               and (cm-lf-benefit-cd <> '00' and '  ')
              perform 0500-get-lf-rem  thru 0500-exit
              move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
              move ws-eralph-lf-remamt (a1)
                                       to ws-lf-tot-benefit
              move cp-remaining-term-2 to ws-eralph-lf-remterm (a1)
           else
              move zeros               to ws-eralph-lf-remamt (a1)
                                          ws-eralph-lf-remterm (a1)
           end-if
      
           if cm-lf-loan-expire-dt = low-values or spaces
              move spaces              to ws-lf-exp-dt (a1)
           else
              move cm-lf-loan-expire-dt to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-date-convert thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to ws-lf-exp-dt (a1)
              end-if
           end-if
      
           move cm-ah-benefit-cd       to ws-eralph-ah-cd (a1)
      
           if ((ws-cov-sw = 'P')
                    or
              ((ws-cov-sw = 'S')
               and (ah-joint-ind = 'J')))
               and (cm-ah-benefit-cd <> '00' and '  ')
              perform 0510-get-ah-rem  thru 0510-exit
              move cp-remaining-amt    to ws-eralph-ah-remamt (a1)
              move cp-remaining-term-2 to ws-eralph-ah-remterm (a1)
              move cm-ah-benefit-amt   to ws-eralph-ah-amt (a1)
                                          ws-ah-tot-benefit
           else
              move zeros               to ws-eralph-ah-amt (a1)
                                          ws-eralph-ah-remamt (a1)
                                          ws-eralph-ah-remterm (a1)
           end-if
      
           if cm-ah-loan-expire-dt = low-values or spaces
              move spaces              to ws-ah-exp-dt (a1)
           else
              move cm-ah-loan-expire-dt to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-date-convert thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to ws-ah-exp-dt (a1)
              end-if
           end-if
      
           move ';'                    to ws-delimiter (a1)
      
           perform 0170-ERpndb5        thru 0170-exit

           if erpndb2-startbr
              exec cics endbr
                 dataset     ('ELCERT')
              end-exec
           end-if
      
           compute p1 = a1 + 1
      
           move spaces                 to ws-eralph2-startbr-sw
                                          ws-eralph-sw
      
           exec cics startbr
              dataset     ('ERALPH2')
              ridfld      (ws-eralph-aix-key)
              gteq
              resp        (ws-response)
           end-exec
      
           if (resp-endfile)
              or (resp-notfnd)
      *       display ' no matching eralph record ' 
              set no-matching-alpha to true
              go to 0150-exit
           else
              if not resp-normal
                 display ' error-eralph-start ' ws-response
                 set no-matching-alpha to true
                 go to 0150-exit
              end-if
           end-if
      
           set eralph2-startbr to true
           perform 0155-read-eralph    thru 0155-exit
           if (not resp-normal)
              and (not resp-dupkey)
              display ' Error-eralph-1stread ' ws-response
              go to 0150-exit
           end-if
           
           perform 0160-accum-eralph   thru 0160-exit until
              (af-company-cd-a1  not = ws-comp-cd)
              or (af-account-a1  not = client-account)
              or (ws-alpha-last-name not = ws-last-name)
              or (af-fname (1:3) not = ws-first-three)
              or (end-of-eralph)
              or ((not resp-normal) and (not resp-dupkey))
      
           if eralph2-startbr
              exec cics endbr
                 dataset     ('ERALPH2')
              end-exec
           end-if

           if a1 = +1
              perform 0166-check-for-ah-claim
                                       thru 0166-exit
           end-if

           if a1 > +1
              perform varying t1 from +1 by +1 until t1 > +20
                 move ws-eralph-cert (t1)
                                       to pt-eralph-cert (t1)
                 move ws-eralph-lf-cd (t1)
                                       to pt-eralph-lf-cd (t1)
                 move ws-eralph-lf-remamt (t1)
                                       to pt-eralph-lf-remamt (t1)
                 move ws-eralph-ah-cd (t1)
                                       to pt-eralph-ah-cd (t1)
                 move ws-eralph-ah-amt (t1)
                                       to pt-eralph-ah-amt (t1)
                 move ws-eralph-ah-remamt (t1)
                                       to pt-eralph-ah-remamt (t1)
      *          display ws-cov-type (t1) ' ' 
      *             pt-eralph-cert      (t1) '   '
      *             pt-eralph-lf-cd     (t1) '   '
      *             pt-eralph-lf-remamt (t1) '    A&H   '
      *             pt-eralph-ah-cd     (t1) '   '
      *             pt-eralph-ah-amt    (t1) '   '
      *             pt-eralph-ah-remamt (t1) '   '
              end-perform
           end-if
      
           .
       0150-exit.
           exit.
      
       0155-READ-ERALPH.

           exec cics readnext
              dataset      ('ERALPH2')
              ridfld       (ws-eralph-aix-key)
              into         (alpha-file-rec)
              resp         (ws-response)
           end-exec
      
           if (not resp-normal)
              and (not resp-dupkey)
              display 'setting end-of-eralph to true '
              set end-of-eralph to true
              go to 0155-exit
           end-if
      
           if (ws-comp-cd = ws-eralph-aix-company-cd)
              and (client-account = ws-eralph-aix-account)
              continue
           else
              set end-of-eralph to true
              go to 0155-exit
           end-if

           move af-lname               to ws-name-in
           perform 0650-set-last-name  thru 0650-exit
           move ws-name-out            to ws-alpha-last-name
           
      
           evaluate true
              when (ws-last-name = ws-alpha-last-name)
                 and (ws-first-three not = ws-eralph-aix-1st-three)
                 go to 0155-read-eralph
              when (ws-last-name not = ws-alpha-last-name)
                 set end-of-eralph to true
              when other
                 continue
           end-evaluate

           .
       0155-EXIT.
           EXIT.
      
       0160-accum-eralph.

      ***** calc the current age on the alpha record here
      ***** then compare it to the pb ages, if there not within
      ***** a few years then go to 0160-continue.
      
      *** check to see if there is a pending cancel for the alpha rec **
      *** if not, then accumulate alpha's in table.
      
           if ws-elcert-orig-key = af-control-primary(1:33)
              go to 0160-continue
           end-if
      
           move af-control-primary     to ws-elcert-key
           perform 0600-get-elcert     thru 0600-exit
      
           if resp-normal
              if cm-lf-benefit-cd not = '00' and '  '
                 if cm-lf-cancel-dt <> low-values
                    move '00'             to af-lf-typ
                 end-if
              end-if
              if cm-ah-benefit-cd not = '00' and '  '
                 if cm-ah-cancel-dt <> low-values
                    move '00'             to af-ah-typ
                 end-if
              end-if
           end-if

           if (af-lf-typ = '00' or spaces)
              and (af-ah-typ = '00' or spaces)
              go to 0160-continue
           end-if

           perform 0720-calc-cur-alph-age
                                       thru 0720-exit
      
           if (ws-age - ws-alph-curr-age < 4
              and >= 0)
                            or
              (ws-alph-curr-age - ws-age < 4
              and >= 0)
              add +1 to a1
           else
              go to 0160-continue
           end-if
      
           move zeros to ws-eralph-lf-remamt (a1)
                         ws-eralph-lf-remterm (a1)
                         ws-eralph-ah-amt (a1)
                         ws-eralph-ah-remamt (a1)
                         ws-eralph-ah-remterm (a1)
      
           move ' '                    to ws-pend (a1)
           move af-fname               to ws-first-name (a1)
           move ws-alph-curr-age       to ws-eralph-age (a1)
           move ws-last-name           to ws-tbl-last-name (a1)
           if af-alpha-type-code = 'I'
              move 'P'                 to ws-eralph-prm-sec (a1)
           else
              move 'C'                 to ws-eralph-prm-sec (a1)
           end-if
           move af-dt                  to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ws-eff-dt (a1)
           end-if
      
           if af-lf-typ not = '00' and '  '
              move ws-cov-sw           to ws-cov-type (a1)
              move af-cert-no          to ws-eralph-cert (a1)
              move af-lf-typ           to ws-eralph-lf-cd (a1)
              perform 0500-get-lf-rem  thru 0500-exit
              move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
              compute ws-lf-tot-benefit =
                 ws-lf-tot-benefit + cp-remaining-amt
              move cp-remaining-term-3 to ws-eralph-lf-remterm (a1)
      
              compute ws-lf-tot-benefit =
                 ws-lf-tot-benefit + af-lf-remamt + af-lf-remamt-alt
              move af-lf-expires       to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-date-convert thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to ws-lf-exp-dt (a1)
              end-if
              move ';'                 to ws-delimiter (a1)
           end-if
      
           if af-ah-typ not = '00' and '  '
              move ws-cov-sw           to ws-cov-type (a1)
              move af-cert-no          to ws-eralph-cert (a1)
              move af-ah-typ           to ws-eralph-ah-cd (a1)
              if af-ah-status not = '8'
                 move af-ah-amt        to ws-eralph-ah-amt (a1)
                 perform 0510-get-ah-rem
                                       thru 0510-exit
                 move cp-remaining-amt to ws-eralph-ah-remamt (a1)
                 compute ws-ah-tot-benefit =
                    ws-ah-tot-benefit + af-ah-amt
                 move cp-remaining-term-3 to ws-eralph-ah-remterm (a1)
                 move af-ah-expires    to dc-bin-date-1
                 move ' '              to dc-option-code
                 perform 9700-date-convert
                                       thru 9700-exit
                 if no-conversion-error
                    move dc-greg-date-a-edit
                                       to ws-ah-exp-dt (a1)
                 end-if
              end-if
              move ';'                 to ws-delimiter (a1)
              perform 0165-check-for-ah-claim
                                       thru 0165-exit
           end-if
      
           .
       0160-continue.
      
           perform 0155-read-eralph    thru 0155-exit
      
           .
       0160-exit.
           exit.
      
       0165-check-for-ah-claim.

           move ws-comp-cd             to ws-elmstr5-company-cd
           move af-cert-no             to ws-elmstr5-cert-no
           move spaces                 to ws-elmstr5-startbr-sw
           exec cics startbr
              dataset         ('ELMSTR5')
              ridfld          (ws-elmstr5-key)
              gteq
              resp            (ws-response)
           end-exec
      
           if resp-normal
              set elmstr5-startbr to true
           else
              if (resp-notfnd)
                 or (resp-endfile)
                 go to 0165-exit
              else
                 display 'error-elmstr5-startbr '
                 ws-response ' ' af-cert-no
              end-if
           end-if
      
           .
       0165-read-next.
       
           exec cics readnext
              dataset     ('ELMSTR5')
              ridfld      (ws-elmstr5-key)
              into        (claim-master)
              resp        (ws-response)
           end-exec
      
           if resp-normal or resp-dupkey
              continue
           else
              display ' error-elmstr5-readnext ' ws-response ' '
                 af-cert-no
              go to 0165-endbr
           end-if
      
           if (cl-company-cd-a4 not = af-company-cd)
              or (cl-cert-no-a4 not = af-cert-no)
              go to 0165-endbr
           end-if
      
           if (cl-cert-carrier = af-carrier)
              and (cl-cert-grouping = af-grouping)
              and (cl-cert-state    = af-state)
              and (cl-cert-account  = af-account)
              and (cl-cert-eff-dt   = af-dt)
              and (cl-cert-no-a4    = af-cert-no)
      *       move cl-incurred-dt      to dc-bin-date-1
      *       move pb-cert-eff-dt      to dc-bin-date-2
      *       move '1'                 to dc-option-code
      *       perform 9700-date-convert thru 9700-exit
      *       if (no-conversion-error)
      *          and (dc-elapsed-months > 11)
      *          display ' Found A & H claim ' cl-cert-no-a4 ' '
      *             cl-claim-status ' ' cl-total-paid-amt
                 move 'Y'              to ws-claim (a1)
      *       end-if
           end-if
      
           go to 0165-read-next
      
           .
       0165-endbr.
      
           if elmstr5-startbr
              exec cics endbr
                 dataset     ('ELMSTR5')
              end-exec
           end-if
      
           .
       0165-exit.
           exit.
      
       0166-check-for-ah-claim.

           move ws-elcert-orig-key to ws-elcert-key

           move ws-comp-cd             to ws-elmstr5-company-cd
           move ws-elcert-cert-no      to ws-elmstr5-cert-no
           move spaces                 to ws-elmstr5-startbr-sw
           exec cics startbr
              dataset         ('ELMSTR5')
              ridfld          (ws-elmstr5-key)
              gteq
              resp            (ws-response)
           end-exec
      
           if resp-normal
              set elmstr5-startbr to true
           else
              if (resp-notfnd)
                 or (resp-endfile)
                 go to 0166-exit
              else
                 display 'error-elmstr5-startbr '
                 ws-response ' ' ws-elcert-cert-no
              end-if
           end-if

           .
       0166-read-next.
       
           exec cics readnext
              dataset     ('ELMSTR5')
              ridfld      (ws-elmstr5-key)
              into        (claim-master)
              resp        (ws-response)
           end-exec
      
           if resp-normal or resp-dupkey
              continue
           else
              display ' error-elmstr5-readnext ' ws-response ' '
                 cm-cert-no
              go to 0166-endbr
           end-if
      
           if (cl-company-cd-a4 not = ws-elcert-company-cd)
              or (cl-cert-no-a4 not = ws-elcert-cert-no)
              go to 0166-endbr
           end-if

           if (cl-cert-carrier = ws-elcert-carrier)
              and (cl-cert-grouping = ws-elcert-grouping)
              and (cl-cert-state    = ws-elcert-state)
              and (cl-cert-account  = ws-elcert-account)
              and (cl-cert-eff-dt   = ws-elcert-eff-dt)
              and (cl-cert-no-a4    = ws-elcert-cert-no)
      *       move cl-incurred-dt      to dc-bin-date-1
      *       move pb-cert-eff-dt      to dc-bin-date-2
      *       move '1'                 to dc-option-code
      *       perform 9700-date-convert thru 9700-exit
      *       if (no-conversion-error)
      *          and (dc-elapsed-months > 11)
      *          display ' Found A & H claim ' cl-cert-no-a4 ' '
      *             cl-claim-status ' ' cl-total-paid-amt
                 move 'Y'              to ws-claim (a1)
      *       end-if
           end-if
      
           go to 0166-read-next
      
           .
       0166-endbr.
      
           if elmstr5-startbr
              exec cics endbr
                 dataset     ('ELMSTR5')
              end-exec
           end-if
      
           .
       0166-exit.
           exit.
      
       0170-erpndb5.

      ***  this routine reads all the elcert  records that 
      ***  match the acct#, last name & 1st init
      ***  and if it's not cancelled add to the accum table
      
           move spaces                 to ws-erpndb2-startbr-sw
           move low-values             to ws-elcert-eff-dt
                                          ws-elcert-cert-no

           exec cics startbr
              dataset      ('ELCERT')
              ridfld       (ws-elcert-key)
              gteq
              resp         (ws-response)
           end-exec
      
           if not resp-normal
              display ' error-erpndb5-start ' ws-response ' '
                 ws-elcert-key (2:19)
              go to 0170-exit
           end-if
           set erpndb2-startbr to true
           perform 0175-read-erpndb5   thru 0175-exit
           if not resp-normal
              display ' Error-erpndb5-1stread '
                 ws-passed-pend-key (2:19) ' '
                 ws-passed-pend-key (22:11)
              go to 0170-exit
           end-if
           perform 0180-accum-erpndb5  thru 0180-exit until
              (ws-elcert-key(1:20) <> ws-elcert-orig-key(1:20))
              or (end-of-erpndb5)
              or (not resp-normal)
      *    end-perform
      
           .
       0170-exit.
           exit.
      
       0175-READ-ERPNDB5.

           exec cics readnext
              dataset    ('ELCERT')
              ridfld     (ws-elcert-key)
              into       (certificate-master)
              resp       (ws-response)
           end-exec

           if (resp-endfile)
              or (resp-notfnd)
              or (ws-elcert-key(1:20) <> ws-elcert-orig-key(1:20))
      *       or (ws-last-name <> ws-elcert-last-name)
      *       display ' end of file on read next ' 
              set end-of-erpndb5       to true
           else
              if not resp-normal
                 display ' error-erpndb5-readnext ' ws-response
                    ' ' ws-erpndb5-key (2:19)
                 set end-of-erpndb5    to true
              end-if
           end-if
      
           if not end-of-erpndb5
      *       display ' not end of erpndb5 '
              if ws-cov-sw = 'P'
                 move cm-insured-last-name
                                       to ws-name-in
                 perform 0650-set-last-name
                                       thru 0650-exit
                 move ws-name-out      to ws-elcert-last-name
              else
                 move cm-jt-last-name  to ws-name-in
                 perform 0650-set-last-name
                                       thru 0650-exit
                 move ws-name-out      to ws-elcert-last-name
              end-if
           end-if

           .
       0175-EXIT.
           EXIT.
      
       0180-accum-erpndb5.
      *    display ' made it to 0180- '
           if ws-elcert-key = ws-elcert-orig-key
              go to 0180-read  *> I found myself
           end-if

           if ws-last-name <> ws-elcert-last-name
              go to 0180-read
           end-if

           if cm-lf-policy-pending or
              cm-ah-policy-pending
              continue
           else
              go to 0180-read  *> Will be on elaph
           end-if

           if ((ws-cov-sw = 'P')
              and (cm-insured-first-name(1:3) = ws-first-three))
                            or
              ((ws-cov-sw = 'S')
              and (cm-jt-first-name(1:3) = ws-first-three))
              continue
           else
              go to 0180-read
           end-if

           move spaces                 to ws-p5-last-name
                                          ws-p5-jnt-last-name
           move zeros                  to ws-p5-pri-curr-age
                                          ws-p5-cob-curr-age
      
           move ' '                    to ws-pend-match

           if cm-lf-benefit-cd not = '00' and spaces
              move cm-lf-benefit-cd    to ws-lf-ben-code
              perform 0400-get-lf-code thru 0400-exit
           end-if
           if cm-ah-benefit-cd not = '00' and spaces
              move cm-ah-benefit-cd    to ws-ah-ben-code
              perform 0410-get-ah-code thru 0410-exit
           end-if

           if (cm-lf-benefit-cd <> '00' and spaces)
              and (cm-lf-cancel-dt <> low-values)
              move '00'                to cm-lf-benefit-cd
           end-if
           if (cm-ah-benefit-cd <> '00' and spaces)
              and (cm-ah-cancel-dt <> low-values)
              move '00'                to cm-ah-benefit-cd
           end-if
           if ((cm-lf-benefit-cd = '00' or spaces)
              and (cm-ah-benefit-cd = '00' or spaces))
                       or
              (cm-entry-status = '5' or '9' or
                                'D' or 'V')                 
              go to 0180-read   *>  Must be cancelled or something
           end-if

           move cm-insured-last-name to ws-name-in
           perform 0650-set-last-name  thru 0650-exit
           move ws-name-out            to ws-p5-last-name
           move cm-jt-last-name   to ws-name-in
           perform 0650-set-last-name  thru 0650-exit
           move ws-name-out            to ws-p5-jnt-last-name

           perform 0710-calc-cur-p5-ages
                                    thru 0710-exit

           if ws-cov-sw = 'P'
              if (ws-age - ws-p5-pri-curr-age < 4 and > -4)
                 add +1 to a1
                 set pend-match to true
                 move 'Y'           to ws-pend (a1)
                 move 'P'           to ws-eralph-prm-sec (a1)
                 move ws-p5-pri-curr-age
                                    to ws-eralph-age (a1)
                 move cm-insured-first-name
                                    to ws-first-name (a1)
                 perform 0185-build-common
                                    thru 0185-exit
              end-if
           end-if 
      
           if ws-cov-sw = 'S'
              if (ws-age - ws-p5-cob-curr-age < 4 and > -4)
                 add +1 to a1
                 set pend-match to true
                 move 'Y'           to ws-pend (a1)
                 move 'C'           to ws-eralph-prm-sec (a1)
                 move ws-p5-cob-curr-age
                                    to ws-eralph-age (a1)
                 move cm-jt-first-name
                                    to ws-first-name (a1)
                 perform 0185-build-common
                                    thru 0185-exit
              end-if
           end-if
      
           .
       0180-read.
      
           perform 0175-read-erpndb5   thru 0175-exit
      
           .
       0180-exit.
           exit.
      
       0185-build-common.

           move ws-cov-sw              to ws-cov-type (a1)
           move ws-last-name           to ws-tbl-last-name (a1)
           move cm-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ws-eff-dt (a1)
           end-if
           move cm-cert-no             to ws-eralph-cert  (a1)
      
           move cm-lf-benefit-cd     to ws-eralph-lf-cd (a1)
      
           if ((ws-cov-sw = 'P')
                      or
              ((ws-cov-sw = 'S')
               and (lf-joint-ind = 'J')))
               and (cm-lf-benefit-cd <> '00' and '  ')
              perform 0500-get-lf-rem  thru 0500-exit
              move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
              compute ws-lf-tot-benefit =
                 ws-lf-tot-benefit + cp-remaining-amt
              move cp-remaining-term-3 to ws-eralph-lf-remterm (a1)
           else
              move zeros               to ws-eralph-lf-remamt (a1)
                                          ws-eralph-lf-remterm (a1)
           end-if
      
           if cm-lf-loan-expire-dt = low-values or spaces
              move spaces              to ws-lf-exp-dt (a1)
           else
              move cm-lf-loan-expire-dt   to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-date-convert thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to ws-lf-exp-dt (a1)
              end-if
           end-if
      
           move cm-ah-benefit-cd     to ws-eralph-ah-cd (a1)
           if ((ws-cov-sw = 'P')
                      or
              ((ws-cov-sw = 'S')
               and (ah-joint-ind = 'J')))
               and (cm-ah-benefit-cd <> '00' and '  ')
                 perform 0510-get-ah-rem
                                       thru 0510-exit
                 move cp-remaining-amt to ws-eralph-ah-remamt (a1)
                 compute ws-ah-tot-benefit =
                    ws-ah-tot-benefit + cm-ah-benefit-amt
                 move cp-remaining-term-3 to ws-eralph-ah-remterm (a1)
              move cm-ah-benefit-amt to ws-eralph-ah-amt (a1)
           else
              move zeros               to ws-eralph-ah-amt (a1)
                                          ws-eralph-ah-remamt (a1)
                                          ws-eralph-ah-remterm (a1)
           end-if
           if cm-ah-loan-expire-dt = low-values or spaces
              move spaces              to ws-ah-exp-dt (a1)
           else
              move cm-ah-loan-expire-dt   to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-date-convert thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to ws-ah-exp-dt (a1)
              end-if
           end-if
      
           move ';'                    to ws-delimiter (a1)
      
           .
       0185-exit.
           exit.
      
       0200-send-buffer.

           move ws-return-stuff        to ws-send-buf
      *    display 'SOCK06:About to send      '
      *    display 'SOCK06:sequence number  =', ws-seq-num.
      *    display 'SOCK06:send buffer      =', ws-send-buf(1:80).
      
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.
      
           if return-code <= zero
              display 'SOCK06:send error ',
              go to 0200-socket-error
           end-if
           go to 0200-exit
      
           .
       0200-socket-error.

           if ws-seq-num <> 0
              display "SOCK06:did not complete"
           end-if
      
           .
       0200-exit.
           exit.

       0300-close-socket.
      
      *    display 'SOCK06:closing socket'.
      *    call "close" using by value GIVE-TAKE-SOCKET .
      *    display 'SOCK06:done'
      
           .
       0300-exit.
           exit.
      
       0400-get-lf-code.
      
           move ws-comp-id             to ws-elcntl-key
           set ws-elcntl-lf-ben-cd     to true
           move ws-lf-ben-code         to ws-elcntl-hi-ben-cd
           move zeros                  to ws-elcntl-seq-no
      
           exec cics read
              dataset     ('ELCNTL')
              into        (control-file)
              ridfld      (ws-elcntl-key)
              gteq
              resp        (ws-response)
           end-exec
      
           if resp-normal
              perform varying c1 from +1 by +1 until
                 (c1 > +8)
                 or (cf-benefit-code (c1) = ws-lf-ben-code)
              end-perform
      
              if c1 < +9
                 MOVE CF-BENEFIT-ALPHA (c1)
                                       TO WS-lf-KIND
                 MOVE CF-SPECIAL-CALC-CD (c1)
                                       TO WS-lf-CALC-CD
                 MOVE CF-BENEFIT-DESCRIP (c1)
                                       TO WS-lf-BEN-DESCRIP
                 MOVE CF-LF-COVERAGE-TYPE (c1)
                                       TO WS-LF-COVERAGE-TYPE
                 MOVE CF-CO-EARNINGS-CALC (c1)
                                       TO WS-lf-EARNINGS-CALC
              end-if
           end-if
      
           .
       0400-exit.
           exit.
      
       0410-get-ah-code.
      
           move ws-comp-id             to ws-elcntl-key
           set ws-elcntl-ah-ben-cd     to true
           move ws-ah-ben-code         to ws-elcntl-hi-ben-cd
           move zeros                  to ws-elcntl-seq-no
      
           exec cics read
              dataset     ('ELCNTL')
              into        (control-file)
              ridfld      (ws-elcntl-key)
              gteq
              resp        (ws-response)
           end-exec
      
           if resp-normal
              perform varying c1 from +1 by +1 until
                 (c1 > +8)
                 or (cf-benefit-code (c1) = ws-ah-ben-code)
              end-perform
              if c1 < +9
                 MOVE CF-BENEFIT-ALPHA (c1)
                                       TO WS-ah-KIND
                 MOVE CF-SPECIAL-CALC-CD (c1)
                                       TO WS-ah-CALC-CD
                 MOVE CF-BENEFIT-DESCRIP (c1)
                                       TO WS-ah-BEN-DESCRIP
                 MOVE CF-LF-COVERAGE-TYPE (c1)
                                       TO WS-ah-COVERAGE-TYPE
                 MOVE CF-CO-EARNINGS-CALC (c1)
                                       TO WS-ah-EARNINGS-CALC
              end-if
           .
       0410-exit.
           exit.
      
       0500-get-lf-rem.
      
           MOVE '2'                    TO CP-PROCESS-TYPE
           MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
           MOVE WS-lf-EARNINGS-CALC    TO CP-EARNING-METHOD
                                          CP-RATING-METHOD
           MOVE WS-lf-CALC-CD          TO CP-SPECIAL-CALC-CD
           MOVE cm-cert-eff-dt         TO CP-CERT-EFF-DT
           MOVE cm-loan-1st-pmt-dt     TO CP-FIRST-PAY-DATE
           MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT
           IF cm-lf-orig-term = 0
              MOVE 1                   TO CP-ORIGINAL-TERM
           ELSE
              MOVE cm-lf-orig-term     TO CP-ORIGINAL-TERM
           end-if
           MOVE cm-loan-term           TO CP-LOAN-TERM
           MOVE '1'                    TO CP-REM-TRM-CALC-OPTION
           MOVE '4'                    TO CP-REM-TERM-METHOD
           MOVE ws-comp-id             TO CP-COMPANY-ID
           MOVE ws-comp-cd             TO CP-COMPANY-CD
      
           PERFORM 9800-LINK-REM-TERM  thru 9800-exit
      
           MOVE cm-lf-benefit-amt      TO CP-ORIGINAL-BENEFIT
                                          CP-RATING-BENEFIT-AMT
           MOVE cm-lf-premium-amt      TO CP-ORIGINAL-PREMIUM
           MOVE cm-lf-alt-benefit-amt  TO CP-ALTERNATE-BENEFIT
           MOVE cm-lf-alt-premium-amt  TO CP-ALTERNATE-PREMIUM
           MOVE cm-loan-apr            TO CP-LOAN-APR
           MOVE cm-pay-frequency       TO CP-PAY-FREQUENCY
      
           MOVE cm-rate-class          TO CP-CLASS-CODE
           MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
           perform 9500-link-rem-amt   thru 9500-exit
      
           .
       0500-exit.
           exit.
      
       0510-get-ah-rem.
      
           MOVE '2'                    TO CP-PROCESS-TYPE
           MOVE WS-ah-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
           MOVE WS-ah-EARNINGS-CALC    TO CP-EARNING-METHOD
                                          CP-RATING-METHOD
           MOVE WS-ah-CALC-CD          TO CP-SPECIAL-CALC-CD
           MOVE cm-cert-eff-dt         TO CP-CERT-EFF-DT
           MOVE cm-loan-1st-pmt-dt     TO CP-FIRST-PAY-DATE
           MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT
           MOVE cm-ah-orig-term        TO CP-ORIGINAL-TERM
           MOVE cm-loan-term           TO CP-LOAN-TERM
           MOVE '1'                    TO CP-REM-TRM-CALC-OPTION
           MOVE '4'                    TO CP-REM-TERM-METHOD
           MOVE ws-comp-id             TO CP-COMPANY-ID
           MOVE ws-comp-cd             TO CP-COMPANY-CD
      
           PERFORM 9800-LINK-REM-TERM  thru 9800-exit
      
           MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
           compute cp-remaining-amt =
              cm-ah-benefit-amt * cp-remaining-term-3
      *    perform 9500-link-rem-amt   thru 9500-exit
      
           .
       0510-exit.
           exit.
      
       0600-get-elcert.
      
           exec cics read
              dataset   ('ELCERT')
              ridfld    (ws-elcert-key)
              into      (certificate-master)
              resp      (ws-response)
           end-exec
      
           if not resp-normal
              display ' bad read on elcert ' ws-response
              go to 0600-exit
           end-if
      
           if cm-lf-benefit-cd not = '00' and spaces
              move cm-lf-benefit-cd    to ws-lf-ben-code
              perform 0400-get-lf-code thru 0400-exit
           end-if
           if cm-ah-benefit-cd not = '00' and spaces
              move cm-ah-benefit-cd    to ws-ah-ben-code
              perform 0410-get-ah-code thru 0410-exit
           end-if
      
           .
       0600-exit.
           exit.
      
       0650-set-last-name.
      
           move ws-name-in             to ws-name-out
           perform varying n1 from +13 by -1 until n1 < +3
              if (ws-name-in (n1:3) = ' SR' or ' JR' or ' II' or
                 ' IV' or ' VI' or ' I ' or ' V ')
                 or (ws-name-in (n1:4) = ' III')
                 or (ws-name-in (n1:5) = ' IIII')
                 or (ws-name-in (14:2) = ' I')
                 or (ws-name-in (14:2) = ' V')
                 move ws-name-in (1:n1 - 1)
                                       to ws-name-out
                 move +3               to n1
              end-if
           end-perform
      
           .
       0650-exit.
           exit.
      
       0700-calc-cur-cm-ages.
      
           move zeros                  to ws-cm-pri-curr-age
                                          ws-cm-cob-curr-age
           if (cm-insured-issue-age not numeric)
                     or
              (cm-insured-issue-age = zeros)
              move 42                  to cm-insured-issue-age
           end-if
      
           move cm-insured-issue-age   to ws-cm-pri-curr-age

           move cm-cert-eff-dt         to dc-bin-date-1
           move ws-current-bin-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              compute ws-work-age =
                 cm-insured-issue-age + (dc-elapsed-months / 12)
              move ws-work-age         to ws-cm-pri-curr-age
           end-if
      
           .
       0700-joint-stuff.
      
           if cm-jt-last-name = spaces
              and cm-jt-first-name = spaces
              go to 0700-exit
           end-if
      
           if (cm-insured-joint-age not numeric)
              move zeros               to cm-insured-joint-age
           end-if
      
           move cm-insured-joint-age   to ws-cm-cob-curr-age

           move cm-cert-eff-dt         to dc-bin-date-1
           move ws-current-bin-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              compute ws-work-age =
                 cm-insured-joint-age + (dc-elapsed-months / 12)
              move ws-work-age         to ws-cm-cob-curr-age
           end-if
      
           .
       0700-exit.
           exit.
      
       0710-calc-cur-p5-ages.

           move zeros                  to ws-p5-pri-curr-age
                                          ws-p5-cob-curr-age

           if (cm-insured-issue-age not numeric)
                     or
              (cm-insured-issue-age = zeros)
              move 42                  to cm-insured-issue-age
           end-if
      
           move cm-insured-issue-age   to ws-p5-pri-curr-age
      
      *    if p5-i-birthday <> low-values
      *       move p5-i-birthday       to dc-bin-date-1
      *       move ws-current-bin-dt   to dc-bin-date-2
      *       move '1'                 to dc-option-code
      *       perform 9700-DATE-CONVERT
      *                                thru 9700-exit
      *       if no-conversion-error
      *          compute ws-work-age = dc-elapsed-months / 12
      *          move ws-work-age      to ws-p5-pri-curr-age
      *          go to 0710-joint-stuff
      *       end-if
      *    end-if
      
           move cm-cert-eff-dt         to dc-bin-date-1
           move ws-current-bin-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              compute ws-work-age =
                 cm-insured-issue-age + (dc-elapsed-months / 12)
              move ws-work-age         to ws-p5-pri-curr-age
           end-if
      
           .
       0710-joint-stuff.
      
           if cm-jt-last-name = spaces
              and cm-jt-first-name = spaces
              go to 0710-exit
           end-if
      
           if (cm-insured-joint-age not numeric)
              move zeros               to cm-insured-joint-age
           end-if
      
           move cm-insured-joint-age   to ws-p5-cob-curr-age
      
      *    if p5-i-joint-birthday <> low-values
      *       move p5-i-joint-birthday to dc-bin-date-1
      *       move ws-current-bin-dt   to dc-bin-date-2
      *       move '1'                 to dc-option-code
      *       perform 9700-DATE-CONVERT
      *                                thru 9700-exit
      *       if no-conversion-error
      *          compute ws-work-age = dc-elapsed-months / 12
      *          move ws-work-age      to ws-p5-cob-curr-age
      *          go to 0710-exit
      *       end-if
      *    end-if
      
           move cm-cert-eff-dt         to dc-bin-date-1
           move ws-current-bin-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              compute ws-work-age =
                 cm-insured-joint-age + (dc-elapsed-months / 12)
              move ws-work-age         to ws-p5-cob-curr-age
           end-if
      
           .
       0710-exit.
           exit.
      
       0720-calc-cur-alph-age.
      
           move zeros                  to ws-alph-curr-age
      
           if (af-age not numeric)
                     or
              (af-age = zeros)
              move 42                  to af-age
           end-if
      
           move af-age                 to ws-alph-curr-age
      
           move af-dt                  to dc-bin-date-1
           move ws-current-bin-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              compute ws-work-age = af-age + (dc-elapsed-months / 12)
              move ws-work-age         to ws-alph-curr-age
           end-if
      
           .
       0720-exit.
           exit.
      
       9500-LINK-REM-AMT.
      
           EXEC CICS LINK
               PROGRAM   ('ELRAMT')
               COMMAREA  (CALCULATION-PASS-AREA)
               LENGTH    (CP-COMM-LENGTH)
           END-EXEC
      
           .
       9500-EXIT.
           EXIT.
      
       9700-DATE-CONVERT.
      
           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.
      
       9700-EXIT.
            EXIT.
       9800-LINK-REM-TERM.
      
           EXEC CICS LINK
               PROGRAM   ('ELRTRM')
               COMMAREA  (CALCULATION-PASS-AREA)
               LENGTH    (CP-COMM-LENGTH)
           END-EXEC
      
           .
       9800-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
