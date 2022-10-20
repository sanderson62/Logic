       IDENTIFICATION DIVISION.
       PROGRAM-ID. ELAGGO.
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

051115******************************************************************
051115*REMARKS.                                                        *
051115*        Reads the pending issue record passed to it.            *
051115*        Checks to see if there is a pending cancel record.
051115*        Next, reads all the pending issues that match the
051115*        acct#, last name and 1st initial and builds the table
051115*        with the matching issues. However, if a cancel is found,
051115*        the program move zeros to that benefit code in the table.
051115*        Next, reads the eralph looking for matching accout#, 
051115*        last name, 1st initial that don't have a pending cancel
051115*        record and adds those to the table as well. if not
051115*        cancelled looks for an ah claim.
051115******************************************************************
051115*                   C H A N G E   L O G
051115*
051115* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
051115*-----------------------------------------------------------------
051115*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
051115* EFFECTIVE    NUMBER
051115*-----------------------------------------------------------------
051115* 051115   2015022600002   PEMA  New Program
092215* 092215 IR2015092200002   PEMA  EXPAND TO 1ST 3 OF 1ST NAME
033116* 033116 IR2016033100001   PEMA  CORRECT PROBLEM WITH ER-9813
082718* 082718 CR2018082400001   PEMA  Incorp pri & co borw current age
051115******************************************************************
       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
082718 77  FILLER  PIC X(32) VALUE '   ELAGGO   WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ERPNDB                VALUE 'Y'.
       77  WS-ERALPH-SW                PIC X VALUE SPACES.
           88  END-OF-ERALPH                VALUE 'Y'.
       77  WS-ERPNDB5-SW               PIC X VALUE SPACES.
           88  END-OF-ERPNDB5               VALUE 'Y'.
       77  ERPNDB-RECS-IN              PIC 9(9) VALUE ZEROS.
       77  ERPNDB-RECS-OUT             PIC 9(9) VALUE ZEROS.
082718 77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
082718 77  WS-COMP-ID                  PIC XXX VALUE 'CID'.
       77  SUB1                        PIC S9(5) VALUE +0 COMP-3.
       77  WS-PREV-PNDB-KEY            PIC X(23) VALUE LOW-VALUES.
       77  a1                          pic s999 value +0 comp-3.
       77  c1                          pic s999 value +0 comp-3.
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
       77  ws-elmstr5-startbr-sw       pic x  value spaces.
           88  elmstr5-startbr           value 'Y'.
082718 77  save-bin-date               pic xx value low-values.           
082718 77  ws-current-bin-dt           pic xx value low-values.
082718 77  ws-work-age                 pic s999 comp-3 value zeros.
082718 77  ws-pb-pri-curr-age          pic s999 comp-3 value +0.
082718 77  ws-pb-cob-curr-age          pic s999 comp-3 value +0.
082718 77  ws-p5-pri-curr-age          pic s999 comp-3 value +0.
082718 77  ws-p5-cob-curr-age          pic s999 comp-3 value +0.
082718 77  ws-alph-curr-age            pic s999 comp-3 value +0.

      ******************************************************************
       01  ws-hold-elcert              pic x(450) value spaces.
       01  ws-alpha-table.
           05  ws-eralph-table occurs 20.
               10  ws-eralph-cert      pic x(11).
               10  ws-eralph-lf-cd     pic xx.
               10  ws-eralph-lf-remamt pic s9(9)v99.
               10  ws-eralph-ah-cd     pic xx.
               10  ws-eralph-ah-amt    pic s9(7)v99.
               10  ws-eralph-ah-remamt pic s9(9)v99.

           05  ws-eralph-print-table occurs 20.
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
082718     05  ws-lf-ben-code          pic xx.
082718     05  ws-ah-ben-code          pic xx.
082718     05  ws-lf-calc-cd           pic x   value spaces.
082718     05  ws-lf-coverage-type     pic x   value spaces.
082718     05  ws-lf-earnings-calc     pic x   value spaces.
082718     05  ws-lf-kind              pic x   value spaces.
082718     05  ws-lf-ben-descrip       pic x(10) value spaces.
082718     05  ws-ah-calc-cd           pic x   value spaces.
082718     05  ws-ah-coverage-type     pic x   value spaces.
082718     05  ws-ah-earnings-calc     pic x   value spaces.
082718     05  ws-ah-kind              pic x   value spaces.
082718     05  ws-ah-ben-descrip       pic x(10) value spaces.

       01  ws-elcntl-key.
           05  ws-elcntl-company-id    pic xxx.
           05  ws-elcntl-rec-type      pic x.
               88  ws-elcntl-lf-ben-cd    value '4'.
               88  ws-elcntl-ah-ben-cd    value '5'.
           05  filler                  pic xx.
           05  ws-elcntl-hi-ben-cd     pic xx.
           05  ws-elcntl-seq-no        pic s9(4) comp.

082718 01  ws-elcert-key.
082718     05  ws-elcert-company-cd   pic x.
082718     05  ws-elcert-carrier      pic x.
082718     05  ws-elcert-grouping     pic x(6).
082718     05  ws-elcert-state        pic xx.
082718     05  ws-elcert-account      pic x(10).
082718     05  ws-elcert-eff-dt       pic xx.
082718     05  ws-elcert-cert-no      pic x(11).

       01  ws-erpndb-key.
           05  ws-erpndb-company-cd    pic x.
           05  ws-erpndb-carrier       pic x.
           05  ws-erpndb-grouping      pic x(6).
           05  ws-erpndb-state         pic xx.
           05  ws-erpndb-account       pic x(10).
           05  ws-erpndb-eff-dt        pic xx.
           05  ws-erpndb-cert-no       pic x(11).
           05  ws-erpndb-seq-no        pic s9(4) comp.
           05  ws-erpndb-rec-type      pic x.

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
           05  ws-eralph-aix-fname.
092215         10  ws-eralph-aix-1st-three
092215                                 pic xxx.
               10  filler              pic x(7).
           05  ws-eralph-aix-mid-init  pic x.

       01  ws-eralph-key.
           05  ws-eralph-company-cd    pic x.
           05  ws-eralph-carrier       pic x.
           05  ws-eralph-grouping      pic x(6).
           05  ws-eralph-state         pic xx.
           05  ws-eralph-account       pic x(10).
           05  ws-eralph-eff-dt        pic xx.
           05  ws-eralph-cert-no       pic x(11).
           05  ws-eralph-rec-type      pic x.

082718 01  ws-pass-to-elaggo.
           05  ws-passed-erpndb        pic x(585).
           05  ws-exceed-limit-yn      pic x.
           05  ws-has-claim-yn         pic x.

082718                                 copy ELCCALC.
                                       copy ELCCNTL.
                                       copy ELCMSTR.
                                       copy ERCALPH.
                                       COPY ERCPNDB.
                                       COPY ERCPNDB
            REPLACING LEADING ==PB== BY ==P5==
            PENDING-BUSINESS BY P5-PENDING-BUSINESS.
082718                                 COPY ELCCERT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
       LINKAGE SECTION.
       
       01  DFHCOMMAREA                 PIC X(587).

       PROCEDURE DIVISION.

      *    display ' Entering ELAGGO  '
           move dfhcommarea            to ws-pass-to-elaggo
      *    display ' exceed 1 ' ws-exceed-limit-yn

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT

           move ws-pass-to-elaggo      to dfhcommarea
           exec cics return
           end-exec

           GOBACK

           .
       0020-INITIALIZE.

           move spaces                 to ws-alpha-table

           move ws-passed-erpndb       to pending-business
           move pb-control-by-account  to ws-erpndb-key
           perform 0045-get-ben-codes  thru 0045-exit

082718     exec cics
082718        asktime
082718     end-exec
082718
082718     MOVE EIBDATE                TO DC-JULIAN-YYDDD
082718     MOVE '5'                    TO DC-OPTION-CODE
082718     perform 9700-date-convert   thru 9700-exit
082718
082718     IF DATE-CONVERSION-ERROR
082718        display ' error converting eibdate '
082718        MOVE LOW-VALUES          TO save-bin-date
082718     ELSE
082718        MOVE DC-BIN-DATE-1       TO save-bin-date
082718                                    ws-current-bin-dt
082718     end-if

           .
       0020-EXIT.
           EXIT.

       0045-get-ben-codes.

082718     if pb-i-lf-benefit-cd not = '00' and spaces
082718        move pb-i-lf-benefit-cd   to ws-lf-ben-code
082718        perform 0400-get-lf-code thru 0400-exit
082718        if c1 < +9
082718           move cf-joint-indicator (c1)
082718                                 to lf-joint-ind
082718        end-if
082718     end-if
082718     if pb-i-ah-benefit-cd not = '00' and spaces
082718        move pb-i-ah-benefit-cd   to ws-ah-ben-code
082718        perform 0410-get-ah-code thru 0410-exit
082718        if c1 < +9
082718           move cf-joint-indicator (c1)
082718                                 to ah-joint-ind
082718        end-if
082718     end-if

           .
       0045-exit.
           exit.

       0050-PROCESS-INPUT.

           move ' ' to ws-exceed-limit-yn
           perform 0100-check-cancel   thru 0100-exit
           if (pb-valid-life or pb-valid-ah)
092215        and (pb-i-entry-status not = '5' AND '9' AND
092215                             'D' AND 'V')
              move +0                  to a1
              move spaces              to ws-alpha-table
              move 'P'                 to ws-cov-sw
082718        move pb-i-insured-last-name
082718                                 to ws-name-in
082718        perform 0650-set-last-name
082718                                 thru 0650-exit
082718        move ws-name-out         to ws-last-name
092215        move pb-i-insured-first-name (1:3)
092215                                 to ws-first-three
082718        perform 0700-calc-cur-pb-ages
082718                                 thru 0700-exit
082718        move ws-pb-pri-curr-age  to ws-age
              perform 0150-process-eralph
                                       thru 0150-exit
              if ((lf-joint-ind = 'J')
                 or (ah-joint-ind = 'J'))
                    and
                 (pb-i-joint-last-name not = spaces)
                 move ws-hold-elcert   to certificate-master
                 move zeros to ws-lf-tot-benefit ws-ah-tot-benefit
                 move ' '              to WS-ERPNDB5-SW
                 move 'S'              to ws-cov-sw
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

      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ****                                                           ***
      ****     All we are doing below is setting up the Pending      ***
      ****     issue from review and corrections.                    ***
      ****                                                           ***
      ****                                                           ***
      ****                                                           ***
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
           move ' '                    to ws-match-sw
           move ' '                    to ws-eralph-sw

           move low-values             to ws-eralph-aix-key
           move pb-company-cd          TO ws-eralph-aix-company-cd
           MOVE PB-ACCOUNT             TO ws-eralph-aix-account
           MOVE ws-last-name           to ws-eralph-aix-lname
092215     move ws-first-three         to ws-eralph-aix-1st-three
           add +1                      to a1

           move pb-cert-no             to ws-eralph-cert (a1)

           move zeros                  to ws-eralph-lf-remamt (a1)
                                          ws-eralph-ah-amt (a1)
                                          ws-eralph-ah-remamt (a1)

082718     if pb-i-lf-benefit-cd <> '00' and '  '
082718        if (processing-primary)
082718                    or
082718           ((processing-secondary)
082718           and (lf-joint-ind = 'J'))
082718           move pb-i-lf-benefit-cd
082718                                 to ws-eralph-lf-cd (a1)
082718           perform 0500-get-lf-rem
082718                                 thru 0500-exit
082718           move cp-remaining-amt to ws-eralph-lf-remamt (a1)
082718           move ws-eralph-lf-remamt (a1)
082718                                 to ws-lf-tot-benefit
082718        end-if
082718     end-if

082718     if pb-i-ah-benefit-cd <> '00' and '  '
082718        if (processing-primary)
082718                    or
082718           ((processing-secondary)
082718           and (ah-joint-ind = 'J'))
082718           move pb-i-ah-benefit-cd
082718                                 to ws-eralph-ah-cd (a1)
082718           perform 0510-get-ah-rem
082718                                 thru 0510-exit
082718           move cp-remaining-amt to ws-eralph-ah-remamt (a1)
082718           move cm-ah-benefit-amt
082718                                 to ws-eralph-ah-amt (a1)
082718           move ws-eralph-ah-amt (a1)
082718                                 to ws-ah-tot-benefit
082718        end-if
082718     end-if

      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ****    0170-ERPNDB5 reads thru all the erpndb issues records  ***
      ****    and looks for a match on the account number, last      ***
      ****    name and the 1st 3 characters of the first name and    ***
      ****    the age as of today to be within 4 years of this one   ***
      ****                                                           ***
      ****                                                           ***
      ****                                                           ***
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

           perform 0170-ERpndb5        thru 0170-exit

           if erpndb2-startbr
              exec cics endbr
                 dataset     ('ERPNDB2')
              end-exec
           end-if

           compute p1 = a1 + 1

           move spaces                 to ws-eralph2-startbr-sw

           exec cics startbr
              dataset     ('ERALPH2')
              ridfld      (ws-eralph-aix-key)
              gteq
              resp        (ws-response)
           end-exec

           if (resp-endfile)
              or (resp-notfnd)
              display ' no matching eralph record ' 
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
           if (not resp-normal) and (not resp-dupkey)
              display ' Error-eralph-1stread ' eralph-file-status
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

      *    move ws-lf-tot-benefit to pt-lf-tot-benefit
      *    move ws-ah-tot-benefit to pt-ah-tot-benefit
      *    display ' account cert ' pb-account ' ' pb-cert-no
      *    display ' a1           ' a1 ' ' ws-cov-sw ' ' ws-age
      *    display ' tot bens     ' pt-lf-tot-benefit ' '
      *       pt-ah-tot-benefit
      *
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
      *          display 
      *             pt-eralph-cert      (t1) '   '
      *             pt-eralph-lf-cd     (t1) '   '
      *             pt-eralph-lf-remamt (t1) '    A&H   '
      *             pt-eralph-ah-cd     (t1) '   '
      *             pt-eralph-ah-amt    (t1) '   '
      *             pt-eralph-ah-remamt (t1) '   '
      *       end-perform
      *    end-if

           if ((ws-cov-sw = 'P')
              and (a1 > +1))
                   or
              ((ws-cov-sw = 'S')
              and (a1 > +2))
              if ((ws-age > 65)
                 and ((ws-lf-tot-benefit > 25000.00)
                 or (ws-ah-tot-benefit > 400.00)))
                           or

                 ((ws-lf-tot-benefit > 50000.00)
                 or (ws-ah-tot-benefit > 750.00))
                 move 'Y'                 to ws-exceed-limit-yn
              end-if
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

082718     if (not resp-normal)
082718        and (not resp-dupkey)
082718        set end-of-eralph to true
082718        go to 0155-exit
082718     end-if

082718     if (pb-company-cd = ws-eralph-aix-company-cd)
082718        and (pb-account = ws-eralph-aix-account)
082718        continue
082718     else
082718        set end-of-eralph to true
082718        go to 0155-exit
082718     end-if

082718     move af-lname               to ws-name-in
082718     perform 0650-set-last-name  thru 0650-exit
082718     move ws-name-out            to ws-alpha-last-name
082718
082718     if (ws-last-name not = ws-alpha-last-name)
082718        or (ws-first-three not = ws-eralph-aix-1st-three)
082718        set end-of-eralph to true
082718     end-if

           .
       0155-EXIT.
           EXIT.

       0160-accum-eralph.

      *** check to see if the alpha record is cancelled
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
           else
              go to 0160-continue
           end-if

082718     if (af-lf-typ = '00' or spaces)
082718        and (af-ah-typ = '00' or spaces)
              go to 0160-continue
           end-if

082718     perform 0720-calc-cur-alph-age
082718                                 thru 0720-exit

           if ws-age - ws-alph-curr-age < 4 and > -4
              continue
           else
              go to 0160-continue
           end-if

           if af-lf-remamt = zeros
              and af-ah-remamt = zeros
              go to 0160-continue
           end-if

           add +1 to a1
           if af-lf-typ not = '00' and '  '
              move af-cert-no          to ws-eralph-cert (a1)
              move af-lf-typ           to ws-eralph-lf-cd (a1)
010517        perform 0500-get-lf-rem  thru 0500-exit
010517        move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
              compute ws-lf-tot-benefit =
                 ws-lf-tot-benefit + ws-eralph-lf-remamt (a1)
           end-if

           if af-ah-typ not = '00' and '  '
              move af-cert-no          to ws-eralph-cert (a1)
              move af-ah-typ           to ws-eralph-ah-cd (a1)
              if af-ah-status <> '8'
                 move af-ah-amt        to ws-eralph-ah-amt (a1)
010517           perform 0510-get-ah-rem
010517                                 thru 0510-exit
010517           move cp-remaining-amt to ws-eralph-ah-remamt (a1)
                 compute ws-ah-tot-benefit =
010517              ws-ah-tot-benefit + af-ah-amt
              end-if
           end-if

           .
       0160-continue.

           perform 0155-read-eralph    thru 0155-exit

           .
       0160-exit.
           exit.

       0165-check-for-ah-claim.
      **** this para not being executed only needed on socket call.

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

           if resp-normal
              continue
           else
              display ' error-elmstr5-readnext ' ws-response ' '
                 af-cert-no
              go to 0165-exit
           end-if

           if (cl-company-cd-a4 not = af-company-cd)
              or (cl-cert-no-a4 not = af-cert-no)
              go to 0165-exit
           end-if

           if (cl-cert-carrier = af-carrier)
              and (cl-cert-grouping = af-grouping)
              and (cl-cert-state    = af-state)
              and (cl-cert-account  = af-account)
              and (cl-cert-eff-dt   = af-dt)
              and (cl-cert-no-a4    = af-cert-no)
              move cl-incurred-dt      to dc-bin-date-1
              move pb-cert-eff-dt      to dc-bin-date-2
              move '1'                 to dc-option-code
              perform 9700-date-convert thru 9700-exit
              if (no-conversion-error)
                 and (dc-elapsed-months > 11)
                 display ' Found A & H claim ' cl-cert-no-a4 ' '
                    cl-claim-status ' ' cl-total-paid-amt
              end-if
           end-if

           go to 0165-read-next

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
                 pb-control-by-account (2:19) ' '
                 pb-control-by-account (22:11)
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
              or (ws-erpndb5-key (1:20) <>
                 pb-control-by-account (1:20))
              display ' end of file on read next ' 
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

082718     move p5-control-by-account (1:33)
082718                                 to ws-elcert-key
082718     perform 0600-get-elcert     thru 0600-exit
082718
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
082718
082718     move p5-i-insured-last-name to ws-name-in
082718     perform 0650-set-last-name  thru 0650-exit
082718     move ws-name-out            to ws-p5-last-name
082718     move p5-i-joint-last-name   to ws-name-in
082718     perform 0650-set-last-name  thru 0650-exit
082718     move ws-name-out            to ws-p5-jnt-last-name

           if ((ws-last-name = ws-p5-last-name)
092215        and (ws-first-three = p5-i-insured-first-name (1:3)))
                          or
              ((ws-last-name = ws-p5-jnt-last-name)
092215        and (ws-first-three = p5-i-joint-first-name (1:3)))
082718        perform 0710-calc-cur-p5-ages
082718                                 thru 0710-exit
           end-if

082718     if ((ws-last-name = ws-p5-last-name)
082718        and (ws-first-three = p5-i-insured-first-name (1:3)))
082718        and  (ws-age - ws-p5-pri-curr-age < 4 and > -4)
082718        add +1 to a1
082718        move p5-cert-no          to ws-eralph-cert  (a1)
082718        if (p5-i-lf-benefit-cd <> '  ' and '00')
082718           move p5-i-lf-benefit-cd
082718                                 to ws-eralph-lf-cd (a1)
082718           perform 0500-get-lf-rem
082718                                 thru 0500-exit
082718           move cp-remaining-amt to ws-eralph-lf-remamt (a1)
082718           compute ws-lf-tot-benefit =
082718              ws-lf-tot-benefit + cp-remaining-amt
082718        end-if
082718        if p5-i-ah-benefit-cd <> '  ' and '00'
082718           move p5-i-ah-benefit-cd
082718                              to ws-eralph-ah-cd (a1)
082718           perform 0510-get-ah-rem
082718                              thru 0510-exit
082718           move cp-remaining-amt
082718                              to ws-eralph-ah-remamt (a1)
082718           compute ws-ah-tot-benefit =
082718              ws-ah-tot-benefit + p5-i-ah-benefit-amt
082718           move p5-i-ah-benefit-amt to ws-eralph-ah-amt (a1)
082718        end-if
082718        go to 0180-read  *> They can't possibly be pri & cob
082718     end-if

082718     if ((ws-last-name = ws-p5-jnt-last-name)
082718        and (ws-first-three =
082718                       p5-i-joint-first-name (1:3)))
082718        and  (ws-age - ws-p5-cob-curr-age < 4 and > -4)
082718        add +1 to a1
082718        move p5-control-by-account to ws-save-issue-key
082718        move p5-cert-no       to ws-eralph-cert  (a1)
082718        if p5-i-lf-benefit-cd <> '  ' and '00'
082718           move p5-i-lf-benefit-cd
082718                              to ws-eralph-lf-cd (a1)
082718           perform 0500-get-lf-rem
082718                              thru 0500-exit
082718           move cp-remaining-amt
082718                              to ws-eralph-lf-remamt (a1)
082718           compute ws-lf-tot-benefit =
082718              ws-lf-tot-benefit + cp-remaining-amt
082718        end-if
082718        if p5-i-ah-benefit-cd <> '  ' and '00'
082718           move p5-i-ah-benefit-cd
082718                              to ws-eralph-ah-cd (a1)
082718           perform 0510-get-ah-rem
082718                              thru 0510-exit
082718           move cp-remaining-amt
082718                              to ws-eralph-ah-remamt (a1)
082718           compute ws-ah-tot-benefit =
082718              ws-ah-tot-benefit + p5-i-ah-benefit-amt
082718           move p5-i-ah-benefit-amt to ws-eralph-ah-amt (a1)
082718        end-if
082718     end-if

082718     .
082718 0180-read.

           perform 0175-read-erpndb5   thru 0175-exit

           .
       0180-exit.
           exit.

082718 0400-get-lf-code.
082718
082718     move ws-comp-id             to ws-elcntl-key
082718     set ws-elcntl-lf-ben-cd     to true
082718     move ws-lf-ben-code         to ws-elcntl-hi-ben-cd
082718     move zeros                  to ws-elcntl-seq-no
082718
082718     exec cics read
082718        dataset     ('ELCNTL')
082718        into        (control-file)
082718        ridfld      (ws-elcntl-key)
082718        gteq
082718        resp        (ws-response)
082718     end-exec
082718
082718     if resp-normal
082718        perform varying c1 from +1 by +1 until
082718           (c1 > +8)
082718           or (cf-benefit-code (c1) = ws-lf-ben-code)
082718        end-perform
082718
082718        if c1 < +9
082718           MOVE CF-BENEFIT-ALPHA (c1)
082718                                 TO WS-lf-KIND
082718           MOVE CF-SPECIAL-CALC-CD (c1)
082718                                 TO WS-lf-CALC-CD
082718           MOVE CF-BENEFIT-DESCRIP (c1)
082718                                 TO WS-lf-BEN-DESCRIP
082718           MOVE CF-LF-COVERAGE-TYPE (c1)
082718                                 TO WS-LF-COVERAGE-TYPE
082718           MOVE CF-CO-EARNINGS-CALC (c1)
082718                                 TO WS-lf-EARNINGS-CALC
082718        end-if
082718     end-if
082718
082718     .
082718 0400-exit.
082718     exit.
082718
082718 0410-get-ah-code.
082718
082718     move ws-comp-id             to ws-elcntl-key
082718     set ws-elcntl-ah-ben-cd     to true
082718     move ws-ah-ben-code         to ws-elcntl-hi-ben-cd
082718     move zeros                  to ws-elcntl-seq-no
082718
082718     exec cics read
082718        dataset     ('ELCNTL')
082718        into        (control-file)
082718        ridfld      (ws-elcntl-key)
082718        gteq
082718        resp        (ws-response)
082718     end-exec
082718
082718     if resp-normal
082718        perform varying c1 from +1 by +1 until
082718           (c1 > +8)
082718           or (cf-benefit-code (c1) = ws-ah-ben-code)
082718        end-perform
082718        if c1 < +9
082718           MOVE CF-BENEFIT-ALPHA (c1)
082718                                 TO WS-ah-KIND
082718           MOVE CF-SPECIAL-CALC-CD (c1)
082718                                 TO WS-ah-CALC-CD
082718           MOVE CF-BENEFIT-DESCRIP (c1)
082718                                 TO WS-ah-BEN-DESCRIP
082718           MOVE CF-LF-COVERAGE-TYPE (c1)
082718                                 TO WS-ah-COVERAGE-TYPE
082718           MOVE CF-CO-EARNINGS-CALC (c1)
082718                                 TO WS-ah-EARNINGS-CALC
082718        end-if
082718     .
082718 0410-exit.
082718     exit.
082718
082718 0500-get-lf-rem.
082718
082718     MOVE '2'                    TO CP-PROCESS-TYPE
082718     MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
082718     MOVE WS-lf-EARNINGS-CALC    TO CP-EARNING-METHOD
082718                                    CP-RATING-METHOD
082718     MOVE WS-lf-CALC-CD          TO CP-SPECIAL-CALC-CD
082718     MOVE cm-cert-eff-dt         TO CP-CERT-EFF-DT
082718     MOVE cm-loan-1st-pmt-dt     TO CP-FIRST-PAY-DATE
082718     MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT
082718     IF cm-lf-orig-term = 0
082718        MOVE 1                   TO CP-ORIGINAL-TERM
082718     ELSE
082718        MOVE cm-lf-orig-term     TO CP-ORIGINAL-TERM
082718     end-if
082718     MOVE cm-loan-term           TO CP-LOAN-TERM
082718     MOVE '1'                    TO CP-REM-TRM-CALC-OPTION
082718     MOVE '4'                    TO CP-REM-TERM-METHOD
082718     MOVE ws-comp-id             TO CP-COMPANY-ID
082718     MOVE ws-comp-cd             TO CP-COMPANY-CD
082718
082718     PERFORM 9800-LINK-REM-TERM  thru 9800-exit
082718
082718     MOVE cm-lf-benefit-amt      TO CP-ORIGINAL-BENEFIT
082718                                    CP-RATING-BENEFIT-AMT
082718     MOVE cm-lf-premium-amt      TO CP-ORIGINAL-PREMIUM
082718     MOVE cm-lf-alt-benefit-amt  TO CP-ALTERNATE-BENEFIT
082718     MOVE cm-lf-alt-premium-amt  TO CP-ALTERNATE-PREMIUM
082718     MOVE cm-loan-apr            TO CP-LOAN-APR
082718     MOVE cm-pay-frequency       TO CP-PAY-FREQUENCY
082718
082718     MOVE cm-rate-class          TO CP-CLASS-CODE
082718     MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
082718     perform 9500-link-rem-amt   thru 9500-exit
082718
082718     .
082718 0500-exit.
082718     exit.
082718
082718 0510-get-ah-rem.
082718
082718     MOVE '2'                    TO CP-PROCESS-TYPE
082718     MOVE WS-ah-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
082718     MOVE WS-ah-EARNINGS-CALC    TO CP-EARNING-METHOD
082718                                    CP-RATING-METHOD
082718     MOVE WS-ah-CALC-CD          TO CP-SPECIAL-CALC-CD
082718     MOVE cm-cert-eff-dt         TO CP-CERT-EFF-DT
082718     MOVE cm-loan-1st-pmt-dt     TO CP-FIRST-PAY-DATE
082718     MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT
082718     MOVE cm-ah-orig-term        TO CP-ORIGINAL-TERM
082718     MOVE cm-loan-term           TO CP-LOAN-TERM
082718     MOVE '1'                    TO CP-REM-TRM-CALC-OPTION
082718     MOVE '4'                    TO CP-REM-TERM-METHOD
082718     MOVE ws-comp-id             TO CP-COMPANY-ID
082718     MOVE ws-comp-cd             TO CP-COMPANY-CD
082718
082718     PERFORM 9800-LINK-REM-TERM  thru 9800-exit
082718
082718     MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
082718     compute cp-remaining-amt =
082718        cm-ah-benefit-amt * cp-remaining-term-3
082718
082718     .
082718 0510-exit.
082718     exit.
082718
082718 0600-get-elcert.
082718
082718     exec cics read
082718        dataset   ('ELCERT')
082718        ridfld    (ws-elcert-key)
082718        into      (certificate-master)
082718        resp      (ws-response)
082718     end-exec
082718
082718     if not resp-normal
082718        display ' bad read on elcert ' ws-response
082718        go to 0600-exit
082718     end-if
082718
082718     if cm-lf-benefit-cd not = '00' and spaces
082718        move cm-lf-benefit-cd    to ws-lf-ben-code
082718        perform 0400-get-lf-code thru 0400-exit
082718     end-if
082718     if cm-ah-benefit-cd not = '00' and spaces
082718        move cm-ah-benefit-cd    to ws-ah-ben-code
082718        perform 0410-get-ah-code thru 0410-exit
082718     end-if
082718
082718     .
082718 0600-exit.
082718     exit.
082718
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
082718
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