       IDENTIFICATION DIVISION.
       PROGRAM-ID. ELAGGB.
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
051115*        THIS PROGRAM RUNS DAILY AND CREATES AN EXTRACT          *
051115*        OF PENDING BUSINESS (ERPNDB)       RECORDS, BYPASSING   *
051115*        ANY CLAIM CREATED ISSUES.                               *
051115******************************************************************
051115*                   C H A N G E   L O G
051115*
051115* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
051115*-----------------------------------------------------------------
051115*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
051115* EFFECTIVE    NUMBER
051115*-----------------------------------------------------------------
051115* 051115   2015022600002   PEMA  New Program
091615* 091615 IR2015091600001   PEMA  FIX EOF INDICATOR
092215* 092215 IR2015092200002   PEMA  EXPAND TO 1ST 3 OF 1ST NAME
033116* 033116 IR2016033100001   PEMA  CORRECT PROBLEM WITH ER-9813
082718* 082718 CR2018082400001   PEMA  Incorp pri & co borw current age
051115******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERPNDB2          ASSIGN TO ERPNDB2
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS PB-CONTROL-BY-ACCOUNT
                                   FILE STATUS IS ERPNDB-FILE-STATUS.

           SELECT ERPNDB5          ASSIGN TO ERPNDB5
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS P5-CONTROL-BY-ACCOUNT
                                   FILE STATUS IS ERPNDB5-FILE-STATUS.

           SELECT ERALPH2          ASSIGN TO ERALPH2
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AF-CONTROL-by-ACCT-NAME
                                   FILE STATUS IS ERALPH-FILE-STATUS.

           SELECT ELCNTL           ASSIGN TO ELCNTL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CF-CONTROL-PRIMARY
                                   FILE STATUS IS ELCNTL-FILE-STATUS.

           SELECT ELCERT           ASSIGN TO ELCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CM-CONTROL-PRIMARY
                                   FILE STATUS IS ELCERT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  ERPNDB2.

                                       COPY ERCPNDB.

       FD  ERPNDB5.

                                       COPY ERCPNDB
            REPLACING LEADING ==PB== BY ==P5==
            PENDING-BUSINESS BY P5-PENDING-BUSINESS.

       FD  ERALPH2.

                                       COPY ERCALPH.

       FD  ELCNTL.

                                       COPY ELCCNTL.

       FD  ELCERT.

                                       COPY ELCCERT.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   ELAGGB   WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

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
       77  a1                          pic s999 value +0 comp-3.
       77  c1                          pic s999 value +0 comp-3.
       77  p1                          pic s999 value +0 comp-3.
082718 77  n1                          pic s999 value +0 comp-3.
       77  ws-match-sw                 pic x value spaces.
           88  no-matching-alpha           value 'N'.
       77  ws-save-issue-key           pic x(36)  value spaces.
       77  ws-last-name                pic x(15)  value spaces.
010517 77  save-bin-date               pic xx value low-values.           
082718 77  ws-current-bin-dt           pic xx value low-values.
092215 77  ws-first-three              pic xxx  value spaces.
       77  ws-age                      pic 999 value zeros.
       77  ws-cov-sw                   pic x value spaces.
           88  processing-primary         value 'P'.
           88  processing-secondary        value 'S'.
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
           05  ELCNTL-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ELCERT-FILE-STATUS      PIC XX    VALUE ZEROS.
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

                                       copy ELCCALC.
                                       copy ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       LINKAGE SECTION.

       01  ws-pass-to-pemaggb.
           05  ws-passed-erpndb        pic x(585).
           05  ws-exceed-limit-yn      pic x.
           05  ws-has-claim-yn         pic x.

       PROCEDURE DIVISION USING WS-PASS-TO-PEMAGGB.

           if ws-pass-to-pemaggb (1:4) = 'OPEN'
              PERFORM 0010-OPEN-FILES  THRU 0010-EXIT
              goback
           else
              if ws-pass-to-pemaggb (1:5) = 'CLOSE'
                 perform 1000-close-files
                                       thru 1000-exit
                 goback
              end-if
           end-if

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT

           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT ERALPH2 ERPNDB2 ERPNDB5 ELCNTL ELCERT

           IF ERPNDB-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERPNDB - OPEN   '
                 ERPNDB-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPNDB5-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERPNDB5 - OPEN   '
                 ERPNDB5-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERALPH-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERALPH - OPEN   '
                 ERALPH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCNTL-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ELCNTL - OPEN   '
                 ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCERT-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ELCERT - OPEN   '
                 ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
      *    DISPLAY ' WS-FN-DATE ' WS-FN-DATE
           MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DT
                                          save-bin-date
           ELSE
              DISPLAY 'BAD CURRENT DATE ' WS-FN-DATE
              PERFORM ABEND-PGM
           END-IF

           move spaces                 to ws-alpha-table
                                          ws-exceed-limit-yn
           move zeros                  to ws-lf-tot-benefit
                                          ws-ah-tot-benefit

           move ws-passed-erpndb       to pending-business
           perform 0045-get-ben-codes  thru 0045-exit

           .
       0020-EXIT.
           EXIT.

       0045-get-ben-codes.

           move spaces                 to lf-joint-ind ah-joint-ind
           if pb-i-lf-benefit-cd <> '00' and spaces
              move pb-i-lf-benefit-cd  to ws-lf-ben-code
              perform 0400-get-lf-code thru 0400-exit
              if c1 < +9
                 move cf-joint-indicator (c1)
                                       to lf-joint-ind
              end-if
           end-if

           if pb-i-ah-benefit-cd <> '00' and spaces
              move pb-i-ah-benefit-cd  to ws-ah-ben-code
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

           perform 0100-check-cancel   thru 0100-exit
           if (pb-valid-life or pb-valid-ah)
092215        and (pb-i-entry-status not = '5' AND '9' AND
092215                             'D' AND 'V')
              move +0                  to a1
              move 'P'                 to ws-cov-sw
      *       MOVE PB-i-insured-last-name
      *                                to ws-last-name
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

              if pb-i-joint-insured not = spaces
                 move ws-hold-elcert   to certificate-master
                 move zeros to ws-lf-tot-benefit ws-ah-tot-benefit
                 move 'S'              to ws-cov-sw
      *          move pb-i-joint-age   to ws-age
      *          move pb-i-joint-last-name
      *                                to ws-last-name
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

      *** check to see if the cert is cancelled

           move pb-control-by-account (1:33)
                                       to cm-control-primary
           perform 0600-get-elcert     thru 0600-exit

           if elcert-file-status = '00'
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

      *    move pb-control-by-account  to p5-control-by-account
      *    move +0                     to p5-alt-chg-seq-no
      *    move '2'                    to p5-record-type
      *    read erpndb5
      *    if erpndb5-file-status = '00'
      *       if p5-ci-lf-benefit-cd not = '00' and '  '
      *          if p5-c-lf-cancel-dt <> low-values
      *             move '00'             to pb-i-lf-benefit-cd
      *          end-if
      *       end-if
      *       if p5-ci-ah-benefit-cd not = '00' and '  '
      *          if p5-c-ah-cancel-dt <> low-values
      *             move '00'             to pb-i-ah-benefit-cd
      *          end-if
      *       end-if
      *    end-if

           .
       0100-exit.
           exit.

       0150-process-eralph.

           move ' '                    to ws-eralph-sw

           add +1                      to a1
           move spaces                 to ws-alpha-table
           move ' '                    to ws-match-sw
           move low-values             to af-control-by-acct-name
           move pb-company-cd          TO AF-COMPANY-CD-A1
           MOVE PB-ACCOUNT             TO AF-ACCOUNT-A1
           MOVE ws-last-name           to af-lname
092215     move ws-first-three         to af-fname

           move ws-age                 to ws-age-from-pndb

      *    move pb-control-by-account (1:33)
      *                                to cm-control-primary
010517*    perform 0600-get-elcert     thru 0600-exit
      *    display ' got cert ' cm-account ' ' cm-cert-no

           move zeros                  to ws-eralph-lf-remamt (a1)
                                          ws-eralph-ah-amt (a1)
                                          ws-eralph-ah-remamt (a1)

           move pb-cert-no             to ws-eralph-cert (a1)

          if pb-i-lf-benefit-cd <> '00' and '  '
              if (processing-primary)
                          or
                 ((processing-secondary)
                 and (lf-joint-ind = 'J'))
                 move pb-i-lf-benefit-cd
                                       to ws-eralph-lf-cd (a1)
                 perform 0500-get-lf-rem
                                       thru 0500-exit
                 move cp-remaining-amt to ws-eralph-lf-remamt (a1)
                 move ws-eralph-lf-remamt (a1)
                                       to ws-lf-tot-benefit
              end-if
           end-if

           if pb-i-ah-benefit-cd <> '00' and '  '
              if (processing-primary)
                          or
                 ((processing-secondary)
                 and (ah-joint-ind = 'J'))
                 move pb-i-ah-benefit-cd
                                       to ws-eralph-ah-cd (a1)
                 perform 0510-get-ah-rem
                                       thru 0510-exit
                 move cp-remaining-amt to ws-eralph-ah-remamt (a1)
                 move cm-ah-benefit-amt
                                       to ws-eralph-ah-amt (a1)
                 move ws-eralph-ah-amt (a1)
                                       to ws-ah-tot-benefit
              end-if
           end-if

           perform 0170-ERpndb5        thru 0170-exit

           compute p1 = a1 + 1

           start eralph2 key >= af-control-by-acct-name
           if eralph-file-status not = '00'
              set no-matching-alpha to true
              go to 0150-exit
           end-if
           perform 0155-read-eralph    thru 0155-exit
           if eralph-file-status not = '00' and '02'
              display ' Error-eralph-1stread ' eralph-file-status
              perform abend-pgm
           end-if
      *    display ' accounts ' af-account-a1 ' ' pb-account
      *    display ' lname    ' af-lname ' ' ws-last-name
      *    display ' fname    ' af-fname ' ' ws-first-three ' '
      *     eralph-file-status ' ' ws-eralph-sw
           perform 0160-accum-eralph   thru 0160-exit until
              (af-company-cd-a1  not = pb-company-cd)
              or (af-account-a1  not = pb-account)
              or (ws-alpha-last-name not = ws-last-name)
092215        or (af-fname (1:3) not = ws-first-three)
              or (end-of-eralph)
              or (eralph-file-status not = '00' and '02')

           if ((ws-cov-sw = 'P')
              and (a1 > +1))
                   or
              ((ws-cov-sw = 'S')
              and (a1 > +2))


      *    if a1 > +1
              if ((ws-age > 65)
                 and ((ws-lf-tot-benefit > 25000.00)
                 or (ws-ah-tot-benefit > 400.00)))
                             or 
                 ((ws-lf-tot-benefit > 50000.00)
                 or (ws-ah-tot-benefit > 750.00))
                 move 'Y'                 to ws-exceed-limit-yn
      *          display ' '
      *          if processing-primary
      *             display ' *********  P R I M A R Y  **********'
      *          else
      *             display ' ******** S E C O N D A R Y  ********'
      *          end-if
      *          display ' '
      *          display '   ' pb-state ' ' pb-account ' '
      *             ws-last-name ' ' ws-first-three
      *          display ' **********  P E N D I N G   ************'
                 perform varying a1 from +1 by +1 until
                    (ws-eralph-cert (a1) = spaces)
                    or (a1 > +20)
                    move ws-eralph-cert (a1)
                                          to pt-eralph-cert (a1)
                    move ws-eralph-lf-cd (a1)
                                          to pt-eralph-lf-cd (a1)
                    move ws-eralph-lf-remamt (a1)
                                          to pt-eralph-lf-remamt (a1)
                    move ws-eralph-ah-cd (a1)
                                          to pt-eralph-ah-cd (a1)
                    move ws-eralph-ah-amt (a1)
                                          to pt-eralph-ah-amt (a1)
                    move ws-eralph-ah-remamt (a1)
                                          to pt-eralph-ah-remamt (a1)
                 end-perform
      *          perform varying a1 from +1 by +1 until
      *             (ws-eralph-cert (a1) = spaces)
      *             or (a1 > +20)
      *             if a1 = p1
      *                display ' **********  A L P H A   ************'
      *             end-if
      *             display
      *                pt-eralph-cert      (a1) '   '
      *                pt-eralph-lf-cd     (a1) '   '
      *                pt-eralph-lf-remamt (a1) '    A&H   '
      *                pt-eralph-ah-cd     (a1) '   '
      *                pt-eralph-ah-amt    (a1) '   '
      *                pt-eralph-ah-remamt (a1) '   '
      *          end-perform
      *          display ' '
                 move ws-lf-tot-benefit   to pt-lf-tot-benefit
                 move ws-ah-tot-benefit   to pt-ah-tot-benefit
      *          display ' ' ws-age-from-pndb ' ' pt-lf-tot-benefit
      *             ' ' pt-ah-tot-benefit
              end-if
           end-if

           .
       0150-exit.
           exit.

       0155-READ-ERALPH.

           READ ERALPH2 NEXT RECORD

           IF (ERALPH-FILE-STATUS = '10' OR '23')
              SET END-OF-ERALPH        TO TRUE
              go to 0155-exit
           ELSE
              IF ERALPH-FILE-STATUS NOT = '00' and '02'
                 DISPLAY ' ERROR ON ERALPH - READNEXT '
                    ERALPH-FILE-STATUS
                 PERFORM ABEND-PGM
                 go to 0155-exit
              END-IF
           END-IF

           if pb-company-cd = af-company-cd-a1
              and pb-account = af-account-a1
              continue
           else
              set end-of-eralph to true
              go to 0155-exit
           end-if

082718     move af-lname               to ws-name-in
082718     perform 0650-set-last-name  thru 0650-exit
082718     move ws-name-out            to ws-alpha-last-name

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
082718     move af-control-primary(1:33)
                                       to cm-control-primary
082718     perform 0600-get-elcert     thru 0600-exit

           if elcert-file-status = '00'
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

      *    move af-control-primary     to p5-control-by-account
      *    move +0                     to p5-alt-chg-seq-no
      *    move '2'                    to p5-record-type
      *    read erpndb5
      *    if erpndb5-file-status = '00'
      *       if p5-ci-lf-benefit-cd not = '00' and '  '
      *          if p5-c-lf-cancel-dt <> low-values
      *             move '00'             to af-lf-typ
      *          end-if
      *       end-if
      *       if p5-ci-ah-benefit-cd not = '00' and '  '
      *          if p5-c-ah-cancel-dt <> low-values
      *             move '00'             to af-ah-typ
      *          end-if
      *       end-if
      *    end-if

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

010517*    move af-control-primary     to cm-control-primary
010517*    perform 0600-get-elcert     thru 0600-exit
      *    if elcert-file-status <> '00'
      *       go to 0160-continue  *> probably not in force
      *    end-if                  *> anyway

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

       0170-erpndb5.

      ***  this routine reads all the pending records that 
      ***  match the acct#, last name & 1st 3 of 1st name
      ***  and if it's not cancelled add to the accum table

091615     move spaces                 to ws-erpndb5-sw
           move low-values             to p5-control-by-account
           move pb-company-cd-a1       to p5-company-cd-a1
           move pb-carrier             to p5-carrier
           move pb-grouping            to p5-grouping
           move pb-state               to p5-state
           move pb-account             to p5-account

           start erpndb5 key >= p5-control-by-account
           if erpndb5-file-status not = '00'
              display ' error-erpndb5-start ' erpndb5-file-status
              go to 0170-exit
           end-if
           perform 0175-read-erpndb5   thru 0175-exit
           if erpndb5-file-status not = '00' and '02'
              display ' Error-erpndb5-1stread ' erpndb5-file-status
              perform abend-pgm
           end-if
           perform 0180-accum-erpndb5  thru 0180-exit until
              (p5-company-cd-a1  not = pb-company-cd)
              or (p5-account     not = pb-account)
              or (end-of-erpndb5)
              or (erpndb5-file-status not = '00' and '02')
      *    end-perform

           .
       0170-exit.
           exit.

       0175-READ-ERPNDB5.

           READ ERPNDB5 NEXT RECORD

           IF (ERpndb5-FILE-STATUS = '10' OR '23')
              OR (p5-COMPANY-CD-A1 NOT = pb-company-cd)
              SET END-OF-erpndb5       TO TRUE
           ELSE
              IF ERpndb5-FILE-STATUS NOT = '00' and '02'
                 DISPLAY ' ERROR ON ERpndb5 - READNEXT '
                    ERpndb5-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0175-EXIT.
           EXIT.

       0180-accum-erpndb5.

           if (pb-control-by-account = p5-control-by-account)
              or (p5-record-type <> '1')
              go to 0180-read
           end-if

           move p5-control-by-account (1:33)
                                    to cm-control-primary
           perform 0600-get-elcert  thru 0600-exit
           if (cm-lf-benefit-cd <> '00' and spaces)
              and (cm-lf-cancel-dt <> low-values)
              move '00'             to p5-i-lf-benefit-cd
           end-if
           if (cm-ah-benefit-cd <> '00' and spaces)
              and (cm-ah-cancel-dt <> low-values)
              move '00'             to p5-i-ah-benefit-cd
           end-if
           if ((p5-i-lf-benefit-cd = '00' or spaces)
              and (p5-i-ah-benefit-cd = '00' or spaces))
                       or
              (p5-i-entry-status = '5' or '9' or
                                'D' or 'V')                 
              go to 0180-read   *>  Must be cancelled or something
           end-if

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

           .
       0180-read.

           perform 0175-read-erpndb5   thru 0175-exit

           .
       0180-exit.
           exit.

       0400-get-lf-code.

           move pb-company-id          to cf-company-id
           move '4'                    to cf-record-type
           move ws-lf-ben-code         to cf-hi-ben-in-rec
           move zeros                  to cf-sequence-no

           start elcntl key >= cf-control-primary
           if elcntl-file-status not = '00'
              go to 0400-exit
           end-if
           read elcntl next record
           if (elcntl-file-status not = '00')
              or (pb-company-id not = cf-company-id)
              or (cf-record-type not = '4')
              go to 0400-exit
           end-if
           perform varying c1 from +1 by +1 until
              (c1 > +8)
              or (cf-benefit-code (c1) = ws-lf-ben-code)
           end-perform
           if c1 < +9
010517        MOVE CF-BENEFIT-ALPHA (c1)
010517                                 TO WS-lf-KIND
010517        MOVE CF-SPECIAL-CALC-CD (c1)
010517                                 TO WS-lf-CALC-CD
010517        MOVE CF-BENEFIT-DESCRIP (c1)
010517                                 TO WS-lf-BEN-DESCRIP
010517        MOVE CF-LF-COVERAGE-TYPE (c1)
010517                                 TO WS-LF-COVERAGE-TYPE
010517        MOVE CF-CO-EARNINGS-CALC (c1)
010517                                 TO WS-lf-EARNINGS-CALC
           end-if

           .
       0400-exit.
           exit.

       0410-get-ah-code.

           move pb-company-id          to cf-company-id
           move '5'                    to cf-record-type
           move ws-ah-ben-code         to cf-hi-ben-in-rec
           move zeros                  to cf-sequence-no

           start elcntl key >= cf-control-primary
           if elcntl-file-status not = '00'
              go to 0410-exit
           end-if
           read elcntl next record
           if (elcntl-file-status not = '00')
              or (pb-company-id not = cf-company-id)
              or (cf-record-type not = '5')
              go to 0410-exit
           end-if
           perform varying c1 from +1 by +1 until
              (c1 > +8)
              or (cf-benefit-code (c1) = ws-ah-ben-code)
           end-perform

           if c1 < +9
010517        MOVE CF-BENEFIT-ALPHA (c1)
010517                                 TO WS-ah-KIND
010517        MOVE CF-SPECIAL-CALC-CD (c1)
010517                                 TO WS-ah-CALC-CD
010517        MOVE CF-BENEFIT-DESCRIP (c1)
010517                                 TO WS-ah-BEN-DESCRIP
010517        MOVE CF-LF-COVERAGE-TYPE (c1)
010517                                 TO WS-ah-COVERAGE-TYPE
010517        MOVE CF-CO-EARNINGS-CALC (c1)
010517                                 TO WS-ah-EARNINGS-CALC
           end-if

           .
       0410-exit.
           exit.

       0500-get-lf-rem.
      
           move zeros to cp-remaining-benefit
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

           CALL 'ELRTRMX' USING CALCULATION-PASS-AREA

           if not no-cp-error
              display ' rem term error ' cp-return-code ' '
              pb-cert-no
           end-if
           MOVE cm-lf-benefit-amt      TO CP-ORIGINAL-BENEFIT
                                          CP-RATING-BENEFIT-AMT
           MOVE cm-lf-premium-amt      TO CP-ORIGINAL-PREMIUM
           MOVE cm-lf-alt-benefit-amt  TO CP-ALTERNATE-BENEFIT
           MOVE cm-lf-alt-premium-amt  TO CP-ALTERNATE-PREMIUM
           MOVE cm-loan-apr            TO CP-LOAN-APR
           MOVE cm-pay-frequency       TO CP-PAY-FREQUENCY
      
           MOVE cm-rate-class          TO CP-CLASS-CODE
           MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
           CALL 'ELRAMTX' using calculation-pass-area
           if not no-cp-error
              display ' rem amt error ' cp-return-code ' '
              pb-cert-no
           end-if
      
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
      
           CALL 'ELRTRMX' using calculation-pass-area
      
           MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
           compute cp-remaining-amt =
              cm-ah-benefit-amt * cp-remaining-term-3
      
           .
       0510-exit.
           exit.

       0600-get-elcert.

           read elcert
      
           if elcert-file-status not = '00'
              display ' bad read on elcert ' elcert-file-status
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
082718        perform 8500-DATE-CONVERT
082718                                 thru 8500-exit
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
082718     perform 8500-DATE-CONVERT   thru 8500-exit
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
082718        perform 8500-DATE-CONVERT
082718                                 thru 8500-exit
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
082718     perform 8500-DATE-CONVERT   thru 8500-exit
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
082718        perform 8500-DATE-CONVERT
082718                                 thru 8500-exit
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
082718     perform 8500-DATE-CONVERT   thru 8500-exit
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
082718        perform 8500-DATE-CONVERT
082718                                 thru 8500-exit
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
082718     perform 8500-DATE-CONVERT   thru 8500-exit
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
082718     perform 8500-DATE-CONVERT   thru 8500-exit
082718     if no-conversion-error
082718        compute ws-work-age = af-age + (dc-elapsed-months / 12)
082718        move ws-work-age         to ws-alph-curr-age
082718     end-if
082718
082718     .
082718 0720-exit.
082718     exit.


       1000-CLOSE-FILES.

           CLOSE ERPNDB2 ERALPH2 ERPNDB5 ELCERT

           .
       1000-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
