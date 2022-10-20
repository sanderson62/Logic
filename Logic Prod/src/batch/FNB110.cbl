       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FNB110.
       AUTHOR.        Cowtown.
       DATE-COMPILED.

013017*-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**
013017*                   C H A N G E   L O G
013017*
013017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
013017*-----------------------------------------------------------------
013017*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
013017* EFFECTIVE    NUMBER
013017*-----------------------------------------------------------------
013017* 100614 CR2016053100001   PEMA  NEW PROGRAM - FNB ACH FILE
082217* 082217 CR2017082100003   PEMA  Change HFCU sub type to 42
013017*-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DAILY-DRAFTS         ASSIGN TO SYS010.

           SELECT FNB-ACH-FILE         ASSIGN TO SYS011.

           SELECT DISK-DATE            ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  DAILY-DRAFTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F.
                                       COPY CLO420.

       FD  FNB-ACH-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F.
       01  FNB-ACH-RECORD              PIC X(94).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.                                         
                                                                        
       77  ws-grand-total              pic 9(8)v99 value zeros.
       77  ws-in-recs                  pic 9(5) value zeros.
       77  ws-tot-6-cnt                pic 9(5) value zeros.
       77  ws-sel-recs                 pic 9(5) value zeros.
       77  ws-out-recs                 pic 9(5) value zeros.
       77  ws-file-sw                  pic x value spaces.
           88  end-of-input              value 'Y'.
       77  PGM-SUB                     PIC S9(4) COMP VALUE +310.


       01  filler.
           05  ws-work-aba-no-a        pic x(8).
           05  ws-work-aba-no redefines
               ws-work-aba-no-a        pic 9(8).
           05  ws-sum-of-aba-nums      pic 9(9).
       01  WS-ABEND-MESSAGE            PIC X(80)  VALUE SPACES.
       01  WS-RETURN-CODE              PIC S999  COMP-3 VALUE +0.

                                       copy ELCACH.
                                       COPY ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       LINKAGE SECTION.
                                                                        
       01  PARM.                                                        
           05  PARM-LENGTH      PIC S9(4)   COMP.                       
           05  CYCLE-DATE       PIC X(8).

       PROCEDURE DIVISION USING PARM.                                   

           display ' Entering PGM   FNB110 '

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT
           PERFORM 1000-PROCESS-INPUT  THRU 1000-EXIT UNTIL
              END-OF-INPUT
           perform 1200-post-total-recs thru 1200-exit
           PERFORM 2000-CLOSE-FILES    THRU 2000-EXIT

           display '     Records read ' ws-in-recs
           display ' Records selected ' ws-sel-recs
           display '  Records written ' ws-out-recs
           goback

           .
       0010-OPEN-FILES.

           OPEN INPUT  DAILY-DRAFTS
                OUTPUT FNB-ACH-FILE

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           display ' made it to 0020-init '

           IF PARM-LENGTH = +0                                          
              DISPLAY 'PARAMETER DATE IS MISSING'                      
              perform abend-pgm
           end-if

           move 'L'                    to dc-option-code
           move cycle-date             to DC-GREG-DATE-CYMD-R
           perform 8500-date-convert   thru 8500-exit
           if not no-conversion-error
              display ' invalid parm date - abort '
              perform abend-pgm
           end-if

           move FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           move zeros                  to ws-work-aba-no-a
                                          ws-grand-total
                                          ws-tot-6-cnt

           move spaces                 to claim-ach-record-layout
           move '1'                    to ach-record-type
           move '01'                   to ach-t1-sub-type
           move ' 104000016'           to ach-t1-aba-noa
                                          ach-t1-aba-nob
           move cycle-date (3:6)       to ach-t1-date
           compute ach-t1-time = (ws-fn-hours * 100) + ws-fn-minutes
           move ' 094101'              to ach-t1-uf1
           move 'FIRST NATIONAL BANK'  to ach-t1-fnb-name
           if dte-client = 'DCC'
              move 'CENSTAT CASUALTY'  TO ACH-t1-our-name
           else
              move 'CENTRAL STATES'    TO ACH-t1-our-name
           end-if

           perform 1100-write-fnb-ach-record
                                       thru 1100-exit

           move spaces                 to claim-ach-record-layout
           move '5'                    to ach-record-type
           move '20'                   to ach-t5-sub-type
           move '0'                    to ach-t5-zero
           if dte-client = 'DCC'
              move 'CENSTAT CASUALTY'  TO ACH-t5-our-name
              move '3331010163'        to ach-t5-acct-no
           else
              move 'CENTRAL STATES'    TO ACH-t5-our-name
              move '3470123035'        to ach-t5-acct-no
           end-if
           move 'CCD'                  to ach-t5-ccd
           move 'ACHPAYMENT'           to ach-t5-desc
           move cycle-date (3:6)       to ach-t5-nxt-bus-dte
           move '1'                    to ach-t5-uf1
           move '10400001'             to ach-t5-uf2
           move 1                      to ach-t5-seq-no
           perform 1100-write-fnb-ach-record
                                       thru 1100-exit

           PERFORM 0050-READ-INPUT     THRU 0050-EXIT

           .
       0020-EXIT.
           EXIT.

       0050-READ-INPUT.

           READ DAILY-DRAFTS AT END
              SET END-OF-INPUT TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1 TO WS-IN-RECS
           END-IF

           .
       0050-EXIT.
           EXIT.

       1000-PROCESS-INPUT.

           IF (PR-POLICY-TYPE NOT = 'CR')
                        OR
              (CR-ACH-PAYMENT <> 'Y')
              GO TO 1000-continue
           END-IF

           if dr-draft-status = 'S'
              display ' bypassing Manual negative adjustment '
                 dr-record-key
              go to 1000-continue
           end-if
              
      *    if dr-draft-status <> 'M'
      *       continue
      *    else
      *       display ' bypassing Manual record ' dr-record-key
      *       go to 1000-continue
      *    end-if
      *
      *    if (dr-draft-status = 'S')
      *       and (dr-note-code = 01)
      *       display ' bypassing Manual negative adjustment '
      *          dr-record-key
      *       go to 1000-continue
      *    end-if

           move spaces                 to claim-ach-record-layout

      *    move '104008348'            to dr-aba-routing-number
      *    move '53655566112233'       to dr-ach-account-number
           add 1 to ws-tot-6-cnt
           move '6'                    to ach-record-type
           move '22'                   to ach-t6-sub-type
082317     if dr-ach-sub-type not = low-values and spaces and zeros
082317        move dr-ach-sub-type     to ach-t6-sub-type
082317     end-if

082217*    if (dr-aba-routing-number = '313183368')
082217*       and (dr-ach-account-number = '801430')
082217*       display ' Found Houston Federal Credit Union '
082217*       dr-policy-number ' ' dr-amount-paid
082217*       move '42'                to ach-t6-sub-type
082217*    end-if
           move dr-aba-routing-number  to ach-t6-payee-aba-no
           move dr-ach-account-number  to ach-t6-payee-act-no
           move dr-amount-paid         to ach-t6-pay-amt
           move dr-policy-number       to ach-t6-free-form
           move dr-payee               to ach-t6-bene-name
           move '010400001'            to ach-t6-uf1
           move ws-tot-6-cnt           to ach-t6-seq-no

           ADD +1 TO WS-sel-RECS

           compute ws-grand-total = ws-grand-total +
              dr-amount-paid

           move dr-aba-routing-number  to ws-work-aba-no-a
           compute ws-sum-of-aba-nums =
              ws-sum-of-aba-nums + ws-work-aba-no
           perform 1100-write-fnb-ach-record
                                       thru 1100-exit

           .
       1000-continue.

           PERFORM 0050-READ-INPUT     THRU 0050-EXIT

           .
       1000-EXIT.
           EXIT.

       1100-write-fnb-ach-record.

           write fnb-ach-record from claim-ach-record-layout
           add 1 to ws-out-recs

           .
       1100-exit.
           exit.

       1200-post-total-recs.

           move spaces                 to claim-ach-record-layout
           add 1 to ws-tot-6-cnt
           move '6'                    to ach-record-type
           move '27'                   to ach-t6-sub-type
           move '104000016'            to ach-t6-payee-aba-no
           if dte-client = 'DCC'
              MOVE '110382498'         TO ach-t6-payee-act-no
              move 'CENSTAT CASUALTY'  to ach-t6-bene-name
           else
              move '110428873'         to ach-t6-payee-act-no
              move 'CENTRAL STATES'    to ach-t6-bene-name
           end-if
           move ws-grand-total         to ach-t6-pay-amt
           move 'OFFSET'               to ach-t6-free-form
           move '010400001'            to ach-t6-uf1.
           move ws-tot-6-cnt           to ach-t6-seq-no

           move '104000016'            to ws-work-aba-no-a
           compute ws-sum-of-aba-nums =
              ws-sum-of-aba-nums + ws-work-aba-no

           perform 1100-write-fnb-ach-record
                                       thru 1100-exit

           .
       1200-rec-type-8.

           move spaces                 to claim-ach-record-layout
           move '8'                    to ach-record-type
           move '20'                   to ach-t8-sub-type
           move ws-tot-6-cnt           to ach-t8-tot-6-cnt
           move ws-sum-of-aba-nums     to ach-t8-sum-of-abas
           move ws-grand-total         to ach-t8-sum-of-amtsa
                                          ach-t8-sum-of-amtsb
           if dte-client = 'DCC'
              move '1331010163'        to ach-t8-acct-no
           else
              move '1470123035'        to ach-t8-acct-no
           end-if
           move ' 10400001'            to ach-t8-uf1
           move 1                      to ach-t8-seq-no
           
           perform 1100-write-fnb-ach-record
                                       thru 1100-exit

           .
       1200-rec-type-9.

           move spaces                 to claim-ach-record-layout
           move '9'                    to ach-record-type
           move 1                      to ach-t9-tot-8-cnt
           move '0000020'              to ach-t9-uf1
           move ws-tot-6-cnt           to ach-t9-tot-6-cnt
           move ws-sum-of-aba-nums     to ach-t9-sum-of-abas
           move ws-grand-total         to ach-t9-sum-of-amtsa
                                          ach-t9-sum-of-amtsb

           perform 1100-write-fnb-ach-record
                                       thru 1100-exit

           .
       1200-exit.
           exit.

       2000-close-files.

           close daily-drafts fnb-ach-file

           .
       2000-exit.
           exit.

       8500-date-convert.

           call 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-exit.
           exit.

       ABEND-PGM.

           call 'ABORTME'
           goback.                                                                      
