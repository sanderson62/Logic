       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK06.
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

061515******************************************************************
061515*REMARKS.                                                        *
061515*        Reads the pending issue record passed to it.            *
061515******************************************************************
061515*                   C H A N G E   L O G
061515*
061515* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
061515*-----------------------------------------------------------------
061515*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
061515* EFFECTIVE    NUMBER
061515*-----------------------------------------------------------------
061515* 061515   2015022600002   PEMA  New Program
092215* 092215 IR2015092200002   PEMA  EXPAND TO 1ST 3 OF 1ST NAME
033116* 033116 IR2016033100001   PEMA  CORRECT CLAIM IND
052016* 052016 IR2016052000001   PEMA  SKIP ENTCD V,9,D,5
010517* 010517 CR2016021600005   PEMA  NEW FLDS, ADD CRT NOTE, FORCE CD
012417* 012417 IR2017012400001   PEMA  Correct ah rem term
082718* 082718 CR2018082400001   PEMA  Incorp pri & co borw current age
062719* 062719 IR2019062700003   PEMA  Correct issue with suffix in lname
061515******************************************************************
       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SOCK06   WORKING STORAGE     '.
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
082718 77  n1                          pic s999 value +0 comp-3.
       77  t1                          pic s999 value +0 comp-3.
       77  ws-match-sw                 pic x value spaces.
           88  no-matching-alpha           value 'N'.
       77  ws-save-issue-key           pic x(36)  value spaces.
       77  ws-last-name                pic x(15)  value spaces.
092215 77  ws-first-three              pic xxx  value spaces.
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
010517 77  save-bin-date               pic xx value low-values.
010517 77  ws-cert-note-generic-key-len pic s9(4) comp value +34.
010517 77  note-count                  pic s999 comp-3 value +0.
010517 77  ws-build-note-sw            pic x value ' '.
010517     88  finished-with-notes      value 'Y'.
010517 77  ws-ercnot-sw                pic x  value spaces.
010517     88  ercnot-startbr            value 'Y'.
082718 77  ws-current-bin-dt           pic xx value low-values.
082718 77  ws-work-age                 pic s999 comp-3 value zeros.
082718 77  ws-pb-pri-curr-age          pic s999 comp-3 value +0.
082718 77  ws-pb-cob-curr-age          pic s999 comp-3 value +0.
082718 77  ws-p5-pri-curr-age          pic s999 comp-3 value +0.
082718 77  ws-p5-cob-curr-age          pic s999 comp-3 value +0.
082718 77  ws-alph-curr-age            pic s999 comp-3 value +0.
010517
010517 01  cert-note-records-holder.
010517     05  cert-note-record occurs 300.
010517         10  filler              pic x(48).
010517         10  cnr-rest            pic x(102).

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
010517         10  ws-eralph-lf-remterm pic 999.
010517         10  ws-lf-exp-dt        pic x(10).
               10  ws-eralph-ah-cd     pic xx.
               10  ws-eralph-ah-amt    pic 9(7).99.
               10  ws-eralph-ah-remamt pic 9(9).99.
010517         10  ws-eralph-ah-remterm pic 999.
010517         10  ws-ah-exp-dt        pic x(10).
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

082718 01  ws-alpha-last-name          pic x(15) value spaces.
082718 01  ws-p5-last-name             pic x(15) value spaces.
082718 01  ws-p5-jnt-last-name         pic x(15) value spaces.
082718 01  ws-name-in                  pic x(15) value spaces.
082718 01  ws-name-out                 pic x(15) value spaces.
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
010517     05  ws-lf-ben-code          pic xx.
010517     05  ws-ah-ben-code          pic xx.
010517     05  ws-lf-calc-cd           pic x   value spaces.
010517     05  ws-lf-coverage-type     pic x   value spaces.
010517     05  ws-lf-earnings-calc     pic x   value spaces.
010517     05  ws-lf-kind              pic x   value spaces.
010517     05  ws-lf-ben-descrip       pic x(10) value spaces.
010517     05  ws-ah-calc-cd           pic x   value spaces.
010517     05  ws-ah-coverage-type     pic x   value spaces.
010517     05  ws-ah-earnings-calc     pic x   value spaces.
010517     05  ws-ah-kind              pic x   value spaces.
010517     05  ws-ah-ben-descrip       pic x(10) value spaces.

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

010517 01  ws-elcert-key.
010517     05  ws-elcert-company-cd   pic x.
010517     05  ws-elcert-carrier      pic x.
010517     05  ws-elcert-grouping     pic x(6).
010517     05  ws-elcert-state        pic xx.
010517     05  ws-elcert-account      pic x(10).
010517     05  ws-elcert-eff-dt       pic xx.
010517     05  ws-elcert-cert-no      pic x(11).
010517
010517 01  WS-CZ-KEY.
010517     05  WS-CZ-COMPANY-CD        PIC X.                                       
010517     05  WS-CZ-CARRIER           PIC X.                                       
010517     05  WS-CZ-GROUP             PIC X(6).                                    
010517     05  WS-CZ-STATE             PIC XX.   
010517     05  WS-CZ-ACCOUNT           PIC X(10).
010517     05  WS-CZ-EFF-DT            PIC XX.
010517     05  WS-CZ-CERT-NO           PIC X(11).
010517     05  WS-CZ-REC-TYPE          PIC X.
010517     05  ws-cz-note-seq          pic s9(4) comp.

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
092215     05  ws-eralph-aix-1st-three pic xxx.
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

010517                                 copy ELCCALC.
                                       copy ELCCNTL.
010517                                 copy ELCCERT.
                                       copy ELCMSTR.
                                       copy ERCALPH.
                                       COPY ERCPNDB.
                                       COPY ERCPNDB
            REPLACING LEADING ==PB== BY ==P5==
            PENDING-BUSINESS BY P5-PENDING-BUSINESS.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
010517                                 COPY ERCCNOT.
       linkage section.
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).
      ****   client-in-data must be 36 characters.  ****
         05 CLIENT-IN-DATA.
            15  CLIENT-BATCH-NO      PIC X(6).
            15  client-seq-no-a      pic xxxx.
            15  client-seq-no redefines 
                client-seq-no-a      pic 9(4).
            15  client-comp-id       pic xxx.
            15  client-proc-id       pic x(4).
010517      15  client-note          pic xx.
010517      15  FILLER               PIC X(17).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).


       PROCEDURE DIVISION.

      *    display ' Entering SOCK06  '
      * when calling a C function the function returns its value
      * in the system variable return code.
      *
      *    display 'SOCK06:transaction data =', CLIENT-IN-DATA '**'
      *    display 'SOCK06:socket number    =', GIVE-TAKE-SOCKET.

           perform 0000-INITIALIZE     thru 0000-exit

010517     if client-note = 'OK'
010517        continue
           else
              if resp-normal
                 PERFORM 0050-PROCESS-INPUT
                                       THRU 0050-EXIT
              else
                 display ' pending issue not found ' client-batch-no ' '
                 client-seq-no-a
                 move ' issue rec not found '
                                       to ws-return-stuff
              end-if
010517     end-if
           
           perform 0200-send-buffer    thru 0200-exit
           perform 0300-close-socket   thru 0300-exit
           exec cics return end-exec.

           GOBACK
           .
       0000-INITIALIZE.

010517     exec cics
010517        asktime
010517     end-exec
010517
010517     MOVE EIBDATE                TO DC-JULIAN-YYDDD
010517     MOVE '5'                    TO DC-OPTION-CODE
010517     perform 9700-date-convert   thru 9700-exit
010517
010517     IF DATE-CONVERSION-ERROR
010517        display ' error converting eibdate '
010517        MOVE LOW-VALUES          TO save-bin-date
010517     ELSE
010517        MOVE DC-BIN-DATE-1       TO save-bin-date
082718                                    ws-current-bin-dt
010517     end-if

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

           PERFORM 0040-READ-ERPNDB    THRU 0040-EXIT
010517     if client-note = 'OK'
010517        perform 0250-process-cert-notes
010517                                 thru 0250-exit
010517     else
010517        perform 0045-get-ben-codes
010517                                 thru 0045-exit
010517     end-if

           .
       0000-EXIT.
           EXIT.

       0040-READ-ERPNDB.

           move ws-comp-cd             to ws-erpndb-company-cd
           move client-batch-no        to ws-erpndb-batch
           move client-seq-no          to ws-erpndb-seq-no
           move +0                     to ws-erpndb-chg-seq-no

           exec cics read
              dataset     ('ERPNDB')
              into        (pending-business)
              ridfld      (ws-erpndb-key)
              resp        (ws-response)
           end-exec

           .
       0040-EXIT.
           EXIT.

       0045-get-ben-codes.

010517     if pb-i-lf-benefit-cd not = '00' and spaces
010517        move pb-i-lf-benefit-cd   to ws-lf-ben-code
010517        perform 0400-get-lf-code thru 0400-exit
010517        if c1 < +9
010517           move cf-joint-indicator (c1)
010517                                 to lf-joint-ind
010517        end-if
010517     end-if
010517     if pb-i-ah-benefit-cd not = '00' and spaces
010517        move pb-i-ah-benefit-cd   to ws-ah-ben-code
010517        perform 0410-get-ah-code thru 0410-exit
010517        if c1 < +9
010517           move cf-joint-indicator (c1)
010517                                 to ah-joint-ind
010517        end-if
010517     end-if

           .
       0045-exit.
           exit.

       0050-PROCESS-INPUT.

           perform 0100-check-cancel   thru 0100-exit
           if (pb-valid-life or pb-valid-ah)
092215        and (pb-i-entry-status not = '5' AND '9' AND
092215                             'D' AND 'V')
              move +0                  to a1
              move spaces              to ws-alpha-table
                                          ws-return-stuff
              move 'P'                 to ws-cov-sw
082718*       MOVE PB-i-insured-last-name
082718*                                to ws-last-name
082718        move pb-i-insured-last-name
082718                                 to ws-name-in
082718        perform 0650-set-last-name
082718                                 thru 0650-exit
082718        move ws-name-out         to ws-last-name
092215        move pb-i-insured-first-name (1:3)
092215                                 to ws-first-three
082718        perform 0700-calc-cur-pb-ages
082718                                 thru 0700-exit
              move ws-pb-pri-curr-age  to ws-age
              perform 0150-process-eralph
                                       thru 0150-exit
              if ((lf-joint-ind = 'J')
                 or (ah-joint-ind = 'J'))
092215                   and 
092215              (pb-i-joint-last-name not = spaces)
                 move ws-hold-elcert   to certificate-master
                 move ' '              to WS-ERPNDB5-SW
                 move 'S'              to ws-cov-sw
082718*          move pb-i-joint-last-name
082718*                                to ws-last-name
082718           move pb-i-joint-last-name
082718                                 to ws-name-in
082718           perform 0650-set-last-name
082718                                 thru 0650-exit
082718           move ws-name-out      to ws-last-name
092215           move pb-i-joint-first-name (1:3)
092215                                 to ws-first-three
082718           move ws-pb-cob-curr-age
082718                                 to ws-age
                 perform 0150-process-eralph
                                       thru 0150-exit
              end-if
           end-if

           .
       0050-EXIT.
           EXIT.

       0100-check-cancel.

      *** check to see if the pending issue is cancelled

082718     move pb-control-by-account (1:33)
082718                                 to ws-elcert-key
082718     perform 0600-get-elcert     thru 0600-exit

           if resp-normal
              move certificate-master  to ws-hold-elcert
082718        if cm-lf-benefit-cd <> '00' and '  '
082718           if cm-lf-cancel-dt <> low-values
082718              move '00'          to pb-i-lf-benefit-cd
                 end-if
              end-if
082718        if cm-ah-benefit-cd <> '00' and '  '
082718           if cm-ah-cancel-dt <> low-values
082718              move '00'          to pb-i-ah-benefit-cd
                 end-if
              end-if
           end-if

           .
       0100-exit.
           exit.

       0150-process-eralph.

      ****  First, move pending record to first occurance of table

           move ' '                    to ws-match-sw

           move low-values             to ws-eralph-aix-key
           move pb-company-cd          TO ws-eralph-aix-company-cd
           MOVE PB-ACCOUNT             TO ws-eralph-aix-account
           MOVE ws-last-name           to ws-eralph-aix-lname
092215     move ws-first-three         to ws-eralph-aix-1st-three

           add +1                      to a1
           move ws-cov-sw              to ws-cov-type (a1)
           move 'Y'                    to ws-pend (a1)
           move ws-age                 to ws-eralph-age (a1)
           if processing-primary
              move pb-i-insured-first-name
                                       to ws-first-name (a1)
           else
              move pb-i-joint-first-name
                                       to ws-first-name (a1)
           end-if
           move ws-last-name           to ws-tbl-last-name (a1)
           move pb-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ws-eff-dt (a1)
           end-if
           move pb-cert-no             to ws-eralph-cert (a1)

010517     move pb-i-lf-benefit-cd     to ws-eralph-lf-cd (a1)

           if ((ws-cov-sw = 'P')
                    or
              ((ws-cov-sw = 'S')
               and (lf-joint-ind = 'J')))
012417         and (cm-lf-benefit-cd <> '00' and '  ')
010517        perform 0500-get-lf-rem  thru 0500-exit
010517        move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
              move ws-eralph-lf-remamt (a1)
                                       to ws-lf-tot-benefit
010517        move cp-remaining-term-2 to ws-eralph-lf-remterm (a1)
           else
              move zeros               to ws-eralph-lf-remamt (a1)
010517                                    ws-eralph-lf-remterm (a1)
           end-if

010517     if pb-i-lf-expire-dt = low-values or spaces
010517        move spaces              to ws-lf-exp-dt (a1)
010517     else
010517        move pb-i-lf-expire-dt   to dc-bin-date-1
010517        move ' '                 to dc-option-code
010517        perform 9700-date-convert thru 9700-exit
010517        if no-conversion-error
010517           move dc-greg-date-a-edit
010517                                 to ws-lf-exp-dt (a1)
010517        end-if
010517     end-if

           move pb-i-ah-benefit-cd     to ws-eralph-ah-cd (a1)

           if ((ws-cov-sw = 'P')
                    or
              ((ws-cov-sw = 'S')
               and (ah-joint-ind = 'J')))
               and (cm-ah-benefit-cd <> '00' and '  ')
010517        perform 0510-get-ah-rem  thru 0510-exit
010517        move cp-remaining-amt    to ws-eralph-ah-remamt (a1)
010517        move cp-remaining-term-2 to ws-eralph-ah-remterm (a1)
              move cm-ah-benefit-amt   to ws-eralph-ah-amt (a1)
                                          ws-ah-tot-benefit
           else
              move zeros               to ws-eralph-ah-amt (a1)
                                          ws-eralph-ah-remamt (a1)
010517                                    ws-eralph-ah-remterm (a1)
           end-if

010517     if pb-i-ah-expire-dt = low-values or spaces
010517        move spaces              to ws-ah-exp-dt (a1)
010517     else
010517        move pb-i-ah-expire-dt   to dc-bin-date-1
010517        move ' '                 to dc-option-code
010517        perform 9700-date-convert thru 9700-exit
010517        if no-conversion-error
010517           move dc-greg-date-a-edit
010517                                 to ws-ah-exp-dt (a1)
010517        end-if
010517     end-if

           move ';'                    to ws-delimiter (a1)

           perform 0170-ERpndb5        thru 0170-exit

           if erpndb2-startbr
              exec cics endbr
                 dataset     ('ERPNDB2')
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
      *          display ' error-eralph-start ' ws-response
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
              (af-company-cd-a1  not = pb-company-cd)
              or (af-account-a1  not = pb-account)
082718        or (ws-alpha-last-name not = ws-last-name)
092215        or (af-fname (1:3) not = ws-first-three)
              or (end-of-eralph)
              or ((not resp-normal) and (not resp-dupkey))

           if eralph2-startbr
              exec cics endbr
                 dataset     ('ERALPH2')
              end-exec
           end-if

      *    if a1 > +1
      *       perform varying t1 from +1 by +1 until t1 > +20
      *          move ws-eralph-cert (t1)
      *                                to pt-eralph-cert (t1)
      *          move ws-eralph-lf-cd (t1)
      *                                to pt-eralph-lf-cd (t1)
      *          move ws-eralph-lf-remamt (t1)
      *                                to pt-eralph-lf-remamt (t1)
      *          move ws-eralph-ah-cd (t1)
      *                                to pt-eralph-ah-cd (t1)
      *          move ws-eralph-ah-amt (t1)
      *                                to pt-eralph-ah-amt (t1)
      *          move ws-eralph-ah-remamt (t1)
      *                                to pt-eralph-ah-remamt (t1)
      *          display ws-cov-type (t1) ' ' 
      *             pt-eralph-cert      (t1) '   '
      *             pt-eralph-lf-cd     (t1) '   '
      *             pt-eralph-lf-remamt (t1) '    A&H   '
      *             pt-eralph-ah-cd     (t1) '   '
      *             pt-eralph-ah-amt    (t1) '   '
      *             pt-eralph-ah-remamt (t1) '   '
      *       end-perform
      *    end-if

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
              set end-of-eralph to true
082718        go to 0155-exit
082718     end-if

082718     if (pb-company-cd = ws-eralph-aix-company-cd)
082718        and (pb-account = ws-eralph-aix-account)
082718        continue
           else
082718        set end-of-eralph to true
082718        go to 0155-exit
082718     end-if
082718
082718     move af-lname               to ws-name-in
082718     perform 0650-set-last-name  thru 0650-exit
082718     move ws-name-out            to ws-alpha-last-name
082718     

062719     evaluate true
062719        when (ws-last-name = ws-alpha-last-name)
062719           and (ws-first-three not = ws-eralph-aix-1st-three)
062719           go to 0155-read-eralph
062719        when (ws-last-name not = ws-alpha-last-name)
062719           set end-of-eralph to true
062719        when other
062719           continue
062719     end-evaluate

           .
       0155-EXIT.
           EXIT.

       0160-accum-eralph.

082718***** calc the current age on the alpha record here
082718***** then compare it to the pb ages, if there not within
082718***** a few years then go to 0160-continue.

      *** check to see if there is a pending cancel for the alpha rec **
      *** if not, then accumulate alpha's in table.

082718     if pb-control-by-account (1:33) = af-control-primary(1:33)
082718        go to 0160-continue
082718     end-if
082718
082718     move af-control-primary     to ws-elcert-key
082718     perform 0600-get-elcert     thru 0600-exit

           if resp-normal
082718        if cm-lf-benefit-cd not = '00' and '  '
082718           if cm-lf-cancel-dt <> low-values
                    move '00'             to af-lf-typ
                 end-if
              end-if
082718        if cm-ah-benefit-cd not = '00' and '  '
082718           if cm-ah-cancel-dt <> low-values
                    move '00'             to af-ah-typ
                 end-if
              end-if
           end-if

082718     if (af-lf-typ = '00' or spaces)
082718        and (af-ah-typ = '00' or spaces)
              go to 0160-continue
           end-if

082718     perform 0720-calc-cur-alph-age
082718                                 thru 0720-exit
082718
082718     if (ws-age - ws-alph-curr-age < 4
082718        and >= 0)
082718                      or
082718        (ws-alph-curr-age - ws-age < 4
082718        and >= 0)
082718        add +1 to a1
082718     else
082718        go to 0160-continue
082718     end-if

           move zeros to ws-eralph-lf-remamt (a1)
010517                   ws-eralph-lf-remterm (a1)
                         ws-eralph-ah-amt (a1)
                         ws-eralph-ah-remamt (a1)
010517                   ws-eralph-ah-remterm (a1)

           move af-fname               to ws-first-name (a1)
082718     move ws-alph-curr-age       to ws-eralph-age (a1)
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
010517        perform 0500-get-lf-rem  thru 0500-exit
010517        move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
010517        compute ws-lf-tot-benefit =
010517           ws-lf-tot-benefit + cp-remaining-amt
010517        move cp-remaining-term-3 to ws-eralph-lf-remterm (a1)

              compute ws-lf-tot-benefit =
                 ws-lf-tot-benefit + af-lf-remamt + af-lf-remamt-alt
010517        move af-lf-expires       to dc-bin-date-1
010517        move ' '                 to dc-option-code
010517        perform 9700-date-convert thru 9700-exit
010517        if no-conversion-error
010517           move dc-greg-date-a-edit
010517                                 to ws-lf-exp-dt (a1)
010517        end-if
              move ';'                 to ws-delimiter (a1)
           end-if

           if af-ah-typ not = '00' and '  '
              move ws-cov-sw           to ws-cov-type (a1)
              move af-cert-no          to ws-eralph-cert (a1)
              move af-ah-typ           to ws-eralph-ah-cd (a1)
              if af-ah-status not = '8'
                 move af-ah-amt        to ws-eralph-ah-amt (a1)
010517           perform 0510-get-ah-rem
010517                                 thru 0510-exit
010517           move cp-remaining-amt to ws-eralph-ah-remamt (a1)
                 compute ws-ah-tot-benefit =
010517              ws-ah-tot-benefit + af-ah-amt
010517           move cp-remaining-term-3 to ws-eralph-ah-remterm (a1)
010517           move af-ah-expires    to dc-bin-date-1
010517           move ' '              to dc-option-code
010517           perform 9700-date-convert
010517                                 thru 9700-exit
010517           if no-conversion-error
010517              move dc-greg-date-a-edit
010517                                 to ws-ah-exp-dt (a1)
010517           end-if
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

           move pb-company-cd          to ws-elmstr5-company-cd
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

033116     if resp-normal or resp-dupkey
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

       0170-erpndb5.

      ***  this routine reads all the pending records that 
      ***  match the acct#, last name & 1st init
      ***  and if it's not cancelled add to the accum table

           move spaces                 to ws-erpndb2-startbr-sw
           move low-values             to ws-erpndb5-key
           move pb-company-cd-a1       to ws-erpndb5-company-cd
           move pb-carrier             to ws-erpndb5-carrier
           move pb-grouping            to ws-erpndb5-grouping
           move pb-state               to ws-erpndb5-state
           move pb-account             to ws-erpndb5-account

           exec cics startbr
              dataset      ('ERPNDB2')
              ridfld       (ws-erpndb5-key)
              gteq
              resp         (ws-response)
           end-exec

           if not resp-normal
              display ' error-erpndb5-start ' ws-response ' '
                 ws-erpndb5-key (2:19)
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
              (p5-company-cd-a1  not = pb-company-cd)
              or (p5-account     not = pb-account)
              or (end-of-erpndb5)
              or (not resp-normal)
      *    end-perform

           .
       0170-exit.
           exit.

       0175-READ-ERPNDB5.

           exec cics readnext
              dataset    ('ERPNDB2')
              ridfld     (ws-erpndb5-key)
              into       (p5-pending-business)
              resp       (ws-response)
           end-exec
               
           if (resp-endfile)
              or (resp-notfnd)
              or (ws-erpndb5-key (1:20) <> pb-control-by-account (1:20))
      *       display ' end of file on read next ' 
              set end-of-erpndb5       to true
           else
              if not resp-normal
                 display ' error-erpndb5-readnext ' ws-response
                    ' ' ws-erpndb5-key (2:19)
                 set end-of-erpndb5    to true
              end-if
           end-if

           .
       0175-EXIT.
           EXIT.

       0180-accum-erpndb5.
           
082718     if (pb-control-by-account = p5-control-by-account)
082718        or (p5-record-type <> '1')
082718        go to 0180-read  *> I found myself or a batch or cancel record
082718     end-if

082718     move spaces                 to ws-p5-last-name
082718                                    ws-p5-jnt-last-name
082718     move zeros                  to ws-p5-pri-curr-age
082718                                    ws-p5-cob-curr-age

082718     move ' '                    to ws-pend-match
082718     move p5-control-by-account (1:33)
082718                                 to ws-elcert-key
082718     perform 0600-get-elcert     thru 0600-exit
082718     if (cm-lf-benefit-cd <> '00' and spaces)
082718        and (cm-lf-cancel-dt <> low-values)
082718        move '00'                to p5-i-lf-benefit-cd
082718     end-if
082718     if (cm-ah-benefit-cd <> '00' and spaces)
082718        and (cm-ah-cancel-dt <> low-values)
082718        move '00'                to p5-i-ah-benefit-cd
082718     end-if
082718     if ((p5-i-lf-benefit-cd = '00' or spaces)
082718        and (p5-i-ah-benefit-cd = '00' or spaces))
082718                 or
082718        (p5-i-entry-status = '5' or '9' or
082718                          'D' or 'V')                 
082718        go to 0180-read   *>  Must be cancelled or something
082718     end-if
082718     move p5-i-insured-last-name to ws-name-in
082718     perform 0650-set-last-name  thru 0650-exit
082718     move ws-name-out            to ws-p5-last-name
082718     move p5-i-joint-last-name   to ws-name-in
082718     perform 0650-set-last-name  thru 0650-exit
082718     move ws-name-out            to ws-p5-jnt-last-name

082718     if ((ws-p5-last-name = ws-last-name)
082718        and (p5-i-insured-first-name (1:3) = ws-first-three))
082718                    or
082718       ((ws-p5-jnt-last-name = ws-last-name)
082718        and (p5-i-joint-first-name (1:3) = ws-first-three))
082718        perform 0710-calc-cur-p5-ages
082718                              thru 0710-exit
082718     end-if

           if (ws-p5-last-name = ws-last-name)
092215        and (p5-i-insured-first-name (1:3) = ws-first-three)
082718        if (ws-age - ws-p5-pri-curr-age < 4 and > -4)
                 add +1 to a1
                 set pend-match to true
                 move 'Y'           to ws-pend (a1)
                 move 'P'           to ws-eralph-prm-sec (a1)
                 move ws-p5-pri-curr-age
                                    to ws-eralph-age (a1)
                 move p5-i-insured-first-name
                                    to ws-first-name (a1)
                 perform 0185-build-common
                                    thru 0185-exit
              end-if
           end-if 

           if (ws-p5-jnt-last-name = ws-last-name)
092215        and (p5-i-joint-first-name (1:3) = ws-first-three)
082718        if (ws-age - ws-p5-cob-curr-age < 4 and > -4)
                 add +1 to a1
                 set pend-match to true
                 move 'Y'           to ws-pend (a1)
                 move 'C'           to ws-eralph-prm-sec (a1)
                 move ws-p5-cob-curr-age
                                    to ws-eralph-age (a1)
                 move p5-i-joint-first-name
                                    to ws-first-name (a1)
                 perform 0185-build-common
                                    thru 0185-exit
              end-if
           end-if

082718     .
082718 0180-read.

           perform 0175-read-erpndb5   thru 0175-exit

           .
       0180-exit.
           exit.

       0185-build-common.

           move ws-cov-sw              to ws-cov-type (a1)
           move ws-last-name           to ws-tbl-last-name (a1)
           move p5-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ws-eff-dt (a1)
           end-if
           move p5-cert-no             to ws-eralph-cert  (a1)

           move p5-i-lf-benefit-cd     to ws-eralph-lf-cd (a1)

           if ((ws-cov-sw = 'P')
                      or
              ((ws-cov-sw = 'S')
               and (lf-joint-ind = 'J')))
082718         and (p5-i-lf-benefit-cd <> '00' and '  ')
010517        perform 0500-get-lf-rem  thru 0500-exit
010517        move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
010517        compute ws-lf-tot-benefit =
010517           ws-lf-tot-benefit + cp-remaining-amt
010517        move cp-remaining-term-3 to ws-eralph-lf-remterm (a1)
           else
              move zeros               to ws-eralph-lf-remamt (a1)
012417                                    ws-eralph-lf-remterm (a1)
           end-if

010517     if p5-i-lf-expire-dt = low-values or spaces
010517        move spaces              to ws-lf-exp-dt (a1)
010517     else
010517        move p5-i-lf-expire-dt   to dc-bin-date-1
010517        move ' '                 to dc-option-code
010517        perform 9700-date-convert thru 9700-exit
010517        if no-conversion-error
010517           move dc-greg-date-a-edit
010517                                 to ws-lf-exp-dt (a1)
010517        end-if
010517     end-if

           move p5-i-ah-benefit-cd     to ws-eralph-ah-cd (a1)
           if ((ws-cov-sw = 'P')
                      or
              ((ws-cov-sw = 'S')
               and (ah-joint-ind = 'J')))
082718         and (p5-i-ah-benefit-cd <> '00' and '  ')
010517           perform 0510-get-ah-rem
010517                                 thru 0510-exit
010517           move cp-remaining-amt to ws-eralph-ah-remamt (a1)
010517           compute ws-ah-tot-benefit =
010517              ws-ah-tot-benefit + p5-i-ah-benefit-amt
010517           move cp-remaining-term-3 to ws-eralph-ah-remterm (a1)
              move p5-i-ah-benefit-amt to ws-eralph-ah-amt (a1)
           else
              move zeros               to ws-eralph-ah-amt (a1)
                                          ws-eralph-ah-remamt (a1)
010517                                    ws-eralph-ah-remterm (a1)
           end-if
010517     if p5-i-ah-expire-dt = low-values or spaces
010517        move spaces              to ws-ah-exp-dt (a1)
010517     else
010517        move p5-i-ah-expire-dt   to dc-bin-date-1
010517        move ' '                 to dc-option-code
010517        perform 9700-date-convert thru 9700-exit
010517        if no-conversion-error
010517           move dc-greg-date-a-edit
010517                                 to ws-ah-exp-dt (a1)
010517        end-if
010517     end-if

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

010517 0250-process-cert-notes.
010517
010517     move spaces                 to cert-note-records-holder
010517                                    ws-build-note-sw
010517                                    ws-ercnot-sw
010517     move pb-control-by-account (1:33)
010517                                 to ws-cz-key
010517     move '1'                    to ws-cz-rec-type
010517     move +0                     to ws-cz-note-seq
010517                                    c1
010517
010517     EXEC CICS STARTBR
010517          DATASET    ('ERCNOT')
010517          RIDFLD     (WS-CZ-KEY)
010517          GTEQ
010517          RESP       (WS-RESPONSE)
010517     END-EXEC
010517
010517     IF RESP-NORMAL
010517        set ercnot-startbr to true
010517*       display ' resp normal startbr '
010517        perform until finished-with-notes
010517           EXEC CICS READNEXT
010517              DATASET    ('ERCNOT')
010517              RIDFLD     (WS-CZ-KEY)
010517              INTO       (cert-note-file)
010517              resp       (ws-response)
010517           end-exec
010517           if (resp-normal)
010517              and (cz-control-primary (1:33) =
010517                 pb-control-by-account (1:33))
010517              if cz-record-type = '1'
010517                 add +1 to c1
010517                 move cert-note-file
010517                                 to cert-note-record (c1)
010517              end-if
010517           else
010517              set finished-with-notes to true
010517           end-if
010517        end-perform
010517     end-if
010517
010517     if ercnot-startbr
010517*       display ' about to endbr ercnot '
010517        exec cics endbr
010517           dataset    ('ERCNOT')
010517        end-exec
010517     end-if
010517     move c1                     to note-count
010517
010517     if c1 = +0
010517        perform 0251-add-note    thru 0251-exit
010517        perform 0254-update-cert thru 0254-exit
010517     else
010517        if cnr-rest (1) (1:30) = 'ALL CURRENT COVERAGES REVIEWED'
010517           move 'NOTE PREVIOUSLY ADDED'
010517                                 TO ws-return-stuff
010517           perform 0255-update-erpndb
010517                                 thru 0255-exit
010517           go to 0250-exit
010517        else
010517           perform 0252-delete-cert-notes
010517                                 thru 0252-exit
010517           if resp-normal
010517              perform 0251-add-note
010517                                 thru 0251-exit
010517              if resp-normal
010517                 perform 0253-put-back-cert-notes
010517                                 thru 0253-exit
010517                 if resp-normal
010517                    move 'NOTE SUCCESSFULLY ADDED'
010517                                 to ws-return-stuff
010517                 else
010517                    display ' something wrong with put back '
010517                       pb-cert-no
010517                 end-if
010517              else
010517                 display ' something went wrong with adding note '
010517                    pb-cert-no
010517              end-if
010517           else
010517              display ' something went wrong with generic delete '
010517                 pb-cert-no
010517           end-if
010517        end-if
010517     end-if
010517
010517     .
010517 0250-exit.
010517     exit.
010517
010517 0251-add-note.
010517
010517     move 'CZ'                to cert-note-file
010517     move pb-control-by-account (1:33)
010517                              to cz-control-primary
010517     move '1'                 to cz-record-type
010517     move +1                  to cz-note-sequence
010517     move save-bin-date       to cz-last-maint-dt
010517     move eibtime             to cz-last-maint-hhmmss
010517     move client-proc-id      to cz-last-maint-user
010517     move 'ALL CURRENT COVERAGES REVIEWED, OK TO PROCESS AS IS'
010517                              to cz-note
010517     exec cics write
010517        dataset   ('ERCNOT')
010517        from      (cert-note-file)
010517        ridfld    (cz-control-primary)
010517        resp      (ws-response)
010517     end-exec
010517     if not resp-normal
010517        display ' error-ercnot-write ' ws-response ' '
010517           cz-control-primary (2:33)
010517        move 'NOTE WAS NOT ADDED!!! '
010517                                 to ws-return-stuff
010517     else
010517        move 'NOTE SUCCESSFULLY ADDED'
010517                                 to ws-return-stuff
010517     end-if
010517
010517     perform 0255-update-erpndb  thru 0255-exit
010517
010517     .
010517 0251-exit.
010517     exit.

010517 0252-delete-cert-notes.
010517 
010517     move pb-control-by-account (1:33)
010517                                 to ws-cz-key
010517     move '1'                    to ws-cz-rec-type
010517     move +0                     to ws-cz-note-seq
010517     exec cics delete
010517        dataset    ('ERCNOT')
010517        keylength  (ws-cert-note-generic-key-len)
010517        ridfld     (ws-cz-key (1:34))
010517        generic
010517        resp       (ws-response)
010517     end-exec
010517 
010517     .
010517 0252-exit.
010517     exit.
010517
010517 0253-put-back-cert-notes.
010517
010517     perform varying c1 from +1 by +1 until
010517        c1 > note-count
010517        move cert-note-record (c1)
010517                                 to cert-note-file
010517        add +1                   to cz-note-sequence
010517*       display ' about to write ' cz-control-primary (2:19) ' '
010517*          cz-control-primary (23:11) ' ' cz-note-sequence ' '
010517*           cz-record-type ' ' cz-note-information
010517        exec cics write
010517           dataset ('ERCNOT')
010517           FROM    (cert-note-file)
010517           ridfld  (cz-control-primary)
010517           resp    (ws-response)
010517        end-exec
010517        if not resp-normal
010517           display ' error-ercnot-write subsequ ' ws-response ' '
010517              cz-control-primary (2:33)
010517           move +999             to c1
010517        end-if
010517     end-perform
010517
010517    .
010517 0253-exit.
010517     exit.
010517
010517 0254-update-cert.
010517
010517     move pb-control-by-account (1:33)
010517                                 to ws-elcert-key
010517
010517     exec cics read
010517        update
010517        dataset   ('ELCERT')
010517        ridfld    (ws-elcert-key)
010517        into      (certificate-master)
010517        resp      (ws-response)
010517     end-exec
010517
010517     if resp-normal
010517
010517        evaluate cm-note-sw
010517           when ' '
010517              move '1'           to cm-note-sw
010517           when '2'
010517              move '3'           to cm-note-sw
010517           when '4'
010517              move '5'           to cm-note-sw
010517           when '6'
010517              move '7'           to cm-note-sw
010517        end-evaluate
010517
010517        exec cics rewrite
010517           dataset   ('ELCERT')
010517           from      (certificate-master)
010517           resp      (ws-response)
010517        end-exec
010517
010517        if resp-normal
010517           go to 0254-exit
010517        end-if
010517     end-if
010517
010517     move 'Cert Note switch not updated '
010517                                 to ws-return-stuff
010517
010517     .
010517 0254-exit.
010517     exit.

010517 0255-update-erpndb.
010517
010517     move pb-control-by-account (1:33)
010517                                 to ws-elcert-key
010517
010517     move ws-comp-cd             to ws-erpndb-company-cd
010517     move client-batch-no        to ws-erpndb-batch
010517     move client-seq-no          to ws-erpndb-seq-no
010517     move +0                     to ws-erpndb-chg-seq-no
010517
010517     exec cics read
010517        update
010517        dataset     ('ERPNDB')
010517        into        (pending-business)
010517        ridfld      (ws-erpndb-key)
010517        resp        (ws-response)
010517     end-exec
010517
010517     if resp-normal
010517        if pb-force-code = ' '
010517           move 'L'              to pb-force-code
010517           exec cics rewrite
010517              dataset   ('ERPNDB')
010517              from      (pending-business)
010517              resp      (ws-response)
010517           end-exec
010517
010517           if resp-normal
010517              go to 0255-exit
010517           end-if
010517        end-if
010517     end-if
010517
010517     .
010517 0255-exit.
010517     exit.

       0300-close-socket.

      *    display 'SOCK06:closing socket'.
      *    call "close" using by value GIVE-TAKE-SOCKET .
      *    display 'SOCK06:done'

           .
       0300-exit.
           exit.

010517 0400-get-lf-code.
010517
010517     move ws-comp-id             to ws-elcntl-key
010517     set ws-elcntl-lf-ben-cd     to true
010517     move ws-lf-ben-code         to ws-elcntl-hi-ben-cd
010517     move zeros                  to ws-elcntl-seq-no
010517
010517     exec cics read
010517        dataset     ('ELCNTL')
010517        into        (control-file)
010517        ridfld      (ws-elcntl-key)
010517        gteq
010517        resp        (ws-response)
010517     end-exec
010517
010517     if resp-normal
010517        perform varying c1 from +1 by +1 until
010517           (c1 > +8)
010517           or (cf-benefit-code (c1) = ws-lf-ben-code)
010517        end-perform
010517
010517        if c1 < +9
010517           MOVE CF-BENEFIT-ALPHA (c1)
010517                                 TO WS-lf-KIND
010517           MOVE CF-SPECIAL-CALC-CD (c1)
010517                                 TO WS-lf-CALC-CD
010517           MOVE CF-BENEFIT-DESCRIP (c1)
010517                                 TO WS-lf-BEN-DESCRIP
010517           MOVE CF-LF-COVERAGE-TYPE (c1)
010517                                 TO WS-LF-COVERAGE-TYPE
010517           MOVE CF-CO-EARNINGS-CALC (c1)
010517                                 TO WS-lf-EARNINGS-CALC
010517        end-if
010517     end-if
010517
010517     .
010517 0400-exit.
010517     exit.
010517
010517 0410-get-ah-code.
010517
010517     move ws-comp-id             to ws-elcntl-key
010517     set ws-elcntl-ah-ben-cd     to true
010517     move ws-ah-ben-code         to ws-elcntl-hi-ben-cd
010517     move zeros                  to ws-elcntl-seq-no
010517
010517     exec cics read
010517        dataset     ('ELCNTL')
010517        into        (control-file)
010517        ridfld      (ws-elcntl-key)
010517        gteq
010517        resp        (ws-response)
010517     end-exec
010517
010517     if resp-normal
010517        perform varying c1 from +1 by +1 until
010517           (c1 > +8)
010517           or (cf-benefit-code (c1) = ws-ah-ben-code)
010517        end-perform
010517        if c1 < +9
010517           MOVE CF-BENEFIT-ALPHA (c1)
010517                                 TO WS-ah-KIND
010517           MOVE CF-SPECIAL-CALC-CD (c1)
010517                                 TO WS-ah-CALC-CD
010517           MOVE CF-BENEFIT-DESCRIP (c1)
010517                                 TO WS-ah-BEN-DESCRIP
010517           MOVE CF-LF-COVERAGE-TYPE (c1)
010517                                 TO WS-ah-COVERAGE-TYPE
010517           MOVE CF-CO-EARNINGS-CALC (c1)
010517                                 TO WS-ah-EARNINGS-CALC
010517        end-if
010517     .
010517 0410-exit.
010517     exit.

010517 0500-get-lf-rem.
010517
010517     MOVE '2'                    TO CP-PROCESS-TYPE
010517     MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
010517     MOVE WS-lf-EARNINGS-CALC    TO CP-EARNING-METHOD
010517                                    CP-RATING-METHOD
010517     MOVE WS-lf-CALC-CD          TO CP-SPECIAL-CALC-CD
010517     MOVE cm-cert-eff-dt         TO CP-CERT-EFF-DT
010517     MOVE cm-loan-1st-pmt-dt     TO CP-FIRST-PAY-DATE
010517     MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT
010517     IF cm-lf-orig-term = 0
010517        MOVE 1                   TO CP-ORIGINAL-TERM
010517     ELSE
010517        MOVE cm-lf-orig-term     TO CP-ORIGINAL-TERM
010517     end-if
010517     MOVE cm-loan-term           TO CP-LOAN-TERM
010517     MOVE '1'                    TO CP-REM-TRM-CALC-OPTION
010517     MOVE '4'                    TO CP-REM-TERM-METHOD
010517     MOVE ws-comp-id             TO CP-COMPANY-ID
010517     MOVE ws-comp-cd             TO CP-COMPANY-CD
010517
010517     PERFORM 9800-LINK-REM-TERM  thru 9800-exit
010517
010517     MOVE cm-lf-benefit-amt      TO CP-ORIGINAL-BENEFIT
010517                                    CP-RATING-BENEFIT-AMT
010517     MOVE cm-lf-premium-amt      TO CP-ORIGINAL-PREMIUM
010517     MOVE cm-lf-alt-benefit-amt  TO CP-ALTERNATE-BENEFIT
010517     MOVE cm-lf-alt-premium-amt  TO CP-ALTERNATE-PREMIUM
010517     MOVE cm-loan-apr            TO CP-LOAN-APR
010517     MOVE cm-pay-frequency       TO CP-PAY-FREQUENCY
010517
010517     MOVE cm-rate-class          TO CP-CLASS-CODE
010517     MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
010517     perform 9500-link-rem-amt   thru 9500-exit
010517
010517     .
010517 0500-exit.
010517     exit.
010517
010517 0510-get-ah-rem.
010517
010517     MOVE '2'                    TO CP-PROCESS-TYPE
010517     MOVE WS-ah-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
010517     MOVE WS-ah-EARNINGS-CALC    TO CP-EARNING-METHOD
010517                                    CP-RATING-METHOD
010517     MOVE WS-ah-CALC-CD          TO CP-SPECIAL-CALC-CD
010517     MOVE cm-cert-eff-dt         TO CP-CERT-EFF-DT
010517     MOVE cm-loan-1st-pmt-dt     TO CP-FIRST-PAY-DATE
010517     MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT
012417     MOVE cm-ah-orig-term        TO CP-ORIGINAL-TERM
010517     MOVE cm-loan-term           TO CP-LOAN-TERM
010517     MOVE '1'                    TO CP-REM-TRM-CALC-OPTION
010517     MOVE '4'                    TO CP-REM-TERM-METHOD
010517     MOVE ws-comp-id             TO CP-COMPANY-ID
010517     MOVE ws-comp-cd             TO CP-COMPANY-CD
010517
010517     PERFORM 9800-LINK-REM-TERM  thru 9800-exit
010517
010517     MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
010517     compute cp-remaining-amt =
010517        cm-ah-benefit-amt * cp-remaining-term-3
010517*    perform 9500-link-rem-amt   thru 9500-exit
010517
010517     .
010517 0510-exit.
010517     exit.
010517
010517 0600-get-elcert.
010517
010517     exec cics read
010517        dataset   ('ELCERT')
010517        ridfld    (ws-elcert-key)
010517        into      (certificate-master)
010517        resp      (ws-response)
010517     end-exec
010517
010517     if not resp-normal
010517        display ' bad read on elcert ' ws-response
010517        go to 0600-exit
010517     end-if
010517
010517     if cm-lf-benefit-cd not = '00' and spaces
010517        move cm-lf-benefit-cd    to ws-lf-ben-code
010517        perform 0400-get-lf-code thru 0400-exit
010517     end-if
010517     if cm-ah-benefit-cd not = '00' and spaces
010517        move cm-ah-benefit-cd    to ws-ah-ben-code
010517        perform 0410-get-ah-code thru 0410-exit
010517     end-if
010517
010517     .
010517 0600-exit.
010517     exit.
010517
082718 0650-set-last-name.
082718
082718     move ws-name-in             to ws-name-out
082718     perform varying n1 from +13 by -1 until n1 < +3
082718        if (ws-name-in (n1:3) = ' SR' or ' JR' or ' II' or
082718           ' IV' or ' VI' or ' I ' or ' V ')
082718           or (ws-name-in (n1:4) = ' III')
082718           or (ws-name-in (n1:5) = ' IIII')
082718           or (ws-name-in (14:2) = ' I')
082718           or (ws-name-in (14:2) = ' V')
082718           move ws-name-in (1:n1 - 1)
082718                                 to ws-name-out
082718           move +3               to n1
082718        end-if
082718     end-perform
082718
082718     .
082718 0650-exit.
082718     exit.

082718 0700-calc-cur-pb-ages.
082718
082718     move zeros                  to ws-pb-pri-curr-age
082718                                    ws-pb-cob-curr-age
082718     if (pb-i-age not numeric)
082718               or
082718        (pb-i-age = zeros)
082718        move 42                  to pb-i-age
082718     end-if
082718
082718     move pb-i-age               to ws-pb-pri-curr-age
082718
082718     if pb-i-birthday <> low-values
082718        move pb-i-birthday       to dc-bin-date-1
082718        move ws-current-bin-dt   to dc-bin-date-2
082718        move '1'                 to dc-option-code
082718        perform 9700-DATE-CONVERT
082718                                 thru 9700-exit
082718        if no-conversion-error
082718           compute ws-work-age = dc-elapsed-months / 12
082718           move ws-work-age      to ws-pb-pri-curr-age
082718           go to 0700-joint-stuff
082718        end-if
082718     end-if
082718
082718     move pb-cert-eff-dt         to dc-bin-date-1
082718     move ws-current-bin-dt      to dc-bin-date-2
082718     move '1'                    to dc-option-code
082718     perform 9700-DATE-CONVERT   thru 9700-exit
082718     if no-conversion-error
082718        compute ws-work-age = pb-i-age + (dc-elapsed-months / 12)
082718        move ws-work-age         to ws-pb-pri-curr-age
082718     end-if
082718
082718     .
082718 0700-joint-stuff.
082718
082718     if pb-i-joint-last-name = spaces
082718        and pb-i-joint-first-name = spaces
082718        go to 0700-exit
082718     end-if
082718
082718     if (pb-i-joint-age not numeric)
082718        move zeros               to pb-i-joint-age
082718     end-if
082718
082718     move pb-i-joint-age         to ws-pb-cob-curr-age
082718
082718     if pb-i-joint-birthday <> low-values
082718        move pb-i-joint-birthday to dc-bin-date-1
082718        move ws-current-bin-dt   to dc-bin-date-2
082718        move '1'                 to dc-option-code
082718        perform 9700-DATE-CONVERT
082718                                 thru 9700-exit
082718        if no-conversion-error
082718           compute ws-work-age = dc-elapsed-months / 12
082718           move ws-work-age      to ws-pb-cob-curr-age
082718           go to 0700-exit
082718        end-if
082718     end-if
082718
082718     move pb-cert-eff-dt         to dc-bin-date-1
082718     move ws-current-bin-dt      to dc-bin-date-2
082718     move '1'                    to dc-option-code
082718     perform 9700-DATE-CONVERT   thru 9700-exit
082718     if no-conversion-error
082718        compute ws-work-age =
082718           pb-i-joint-age + (dc-elapsed-months / 12)
082718        move ws-work-age         to ws-pb-cob-curr-age
082718     end-if
082718
082718     .
082718 0700-exit.
082718     exit.
082718
082718 0710-calc-cur-p5-ages.
082718
082718     move zeros                  to ws-p5-pri-curr-age
082718                                    ws-p5-cob-curr-age
082718
082718
082718     if (p5-i-age not numeric)
082718               or
082718        (p5-i-age = zeros)
082718        move 42                  to p5-i-age
082718     end-if
082718
082718     move p5-i-age               to ws-p5-pri-curr-age
082718
082718     if p5-i-birthday <> low-values
082718        move p5-i-birthday       to dc-bin-date-1
082718        move ws-current-bin-dt   to dc-bin-date-2
082718        move '1'                 to dc-option-code
082718        perform 9700-DATE-CONVERT
082718                                 thru 9700-exit
082718        if no-conversion-error
082718           compute ws-work-age = dc-elapsed-months / 12
082718           move ws-work-age      to ws-p5-pri-curr-age
082718           go to 0710-joint-stuff
082718        end-if
082718     end-if
082718
082718     move p5-cert-eff-dt         to dc-bin-date-1
082718     move ws-current-bin-dt      to dc-bin-date-2
082718     move '1'                    to dc-option-code
082718     perform 9700-DATE-CONVERT   thru 9700-exit
082718     if no-conversion-error
082718        compute ws-work-age = p5-i-age + (dc-elapsed-months / 12)
082718        move ws-work-age         to ws-p5-pri-curr-age
082718     end-if
082718
082718     .
082718 0710-joint-stuff.
082718
082718     if p5-i-joint-last-name = spaces
082718        and p5-i-joint-first-name = spaces
082718        go to 0710-exit
082718     end-if
082718
082718     if (p5-i-joint-age not numeric)
082718        move zeros               to p5-i-joint-age
082718     end-if
082718
082718     move p5-i-joint-age         to ws-p5-cob-curr-age
082718
082718     if p5-i-joint-birthday <> low-values
082718        move p5-i-joint-birthday to dc-bin-date-1
082718        move ws-current-bin-dt   to dc-bin-date-2
082718        move '1'                 to dc-option-code
082718        perform 9700-DATE-CONVERT
082718                                 thru 9700-exit
082718        if no-conversion-error
082718           compute ws-work-age = dc-elapsed-months / 12
082718           move ws-work-age      to ws-p5-cob-curr-age
082718           go to 0710-exit
082718        end-if
082718     end-if
082718
082718     move p5-cert-eff-dt         to dc-bin-date-1
082718     move ws-current-bin-dt      to dc-bin-date-2
082718     move '1'                    to dc-option-code
082718     perform 9700-DATE-CONVERT   thru 9700-exit
082718     if no-conversion-error
082718        compute ws-work-age =
082718           p5-i-joint-age + (dc-elapsed-months / 12)
082718        move ws-work-age         to ws-p5-cob-curr-age
082718     end-if
082718
082718     .
082718 0710-exit.
082718     exit.
082718
082718 0720-calc-cur-alph-age.
082718
082718     move zeros                  to ws-alph-curr-age
082718
082718     if (af-age not numeric)
082718               or
082718        (af-age = zeros)
082718        move 42                  to af-age
082718     end-if
082718
082718     move af-age                 to ws-alph-curr-age
082718
082718     move af-dt                  to dc-bin-date-1
082718     move ws-current-bin-dt      to dc-bin-date-2
082718     move '1'                    to dc-option-code
082718     perform 9700-DATE-CONVERT   thru 9700-exit
082718     if no-conversion-error
082718        compute ws-work-age = af-age + (dc-elapsed-months / 12)
082718        move ws-work-age         to ws-alph-curr-age
082718     end-if
082718
082718     .
082718 0720-exit.
082718     exit.

010517 9500-LINK-REM-AMT.
010517
010517     EXEC CICS LINK
010517         PROGRAM   ('ELRAMT')
010517         COMMAREA  (CALCULATION-PASS-AREA)
010517         LENGTH    (CP-COMM-LENGTH)
010517     END-EXEC
010517
010517     .
010517 9500-EXIT.
010517     EXIT.

       9700-DATE-CONVERT.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.
010517 9800-LINK-REM-TERM.
010517
010517     EXEC CICS LINK
010517         PROGRAM   ('ELRTRM')
010517         COMMAREA  (CALCULATION-PASS-AREA)
010517         LENGTH    (CP-COMM-LENGTH)
010517     END-EXEC
010517
010517     .
010517 9800-EXIT.
010517     EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
