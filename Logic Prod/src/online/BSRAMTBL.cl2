      *****************************************************************
      *                                                               *
      * Copyright (c) 2020 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. BSRAMTBL.
      *
      *AUTHOR.    Pablo.
      *           Omaha, Nebraska        
      
      ********************************************
      *   This program was written with the assumption that it
      *  is for company CID only. 
      *   Calculate the remaing amount on all in-force certificates
      *  residing on the ELCERT file. 
      ********************************************
100220******************************************************************
100220*                   C H A N G E   L O G
100220*
100220* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100220*-----------------------------------------------------------------
100220*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100220* EFFECTIVE    NUMBER
100220*-----------------------------------------------------------------
100220* 100520  CR2020092400001  PEMA  New Program.
110220* 110220  IR2020110200001  PEMA  Correct mort estimate
100220******************************************************************
      
       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
       SELECT DISK-DATE ASSIGN TO
          '/data/seqfiles/CI.DD.ER.DATECARD'
          FILE STATUS IS DATE-STATUS.
       
       data division.
       file section.
       FD  DISK-DATE
                                       COPY ELCDTEFD.
       
       working-storage section.
       77  sub                         pic s999 comp-3 value +0.
       77  s1                          pic s999 value +0 comp-3.
       77  ws-eom-bin-dt               pic xx value low-values.
       77  ws-cur-bin-dt               pic xx value low-values.
       77  ws-bin-6-months-ago         pic xx value low-values.
       77  ws-company-cd               pic x  value low-values.
       77  GOOD-STARTBR                pic xxx value spaces.
       77  GOOD-TRLR-STARTBR           pic xxx value spaces.
       77  cnc-fact                    pic s999v9(7) comp-3 value +0.
       77  DATE-STATUS                 pic xx  value zeros.
       77  Comm-STATUS                 pic xx  value zeros.
       77  b1                          pic s999 value +0 comp-3.
       77  ws-cert-status              pic x   value spaces.
           88  good-cert                   value 'Y'.
      
       01  ws-work-date.
           05  ws-work-mo              pic xx.
           05  f                       pic x.
           05  ws-work-da              pic xx.
           05  f                       pic x.
           05  ws-work-yr              pic xxxx.
      
       01  response-code         pic s9(8) comp.
       01  display-response      pic 9(8).
       01  bl-index              pic 9(8) comp.
       01  ws-tot-inforce        pic s9(12)v99 comp-3 value +0.

       01  TEXAS-REG-WORK-AREAS.                                        
           12  TEX-FACT-1          PIC S9(9)V9(2)  COMP-3.              
           12  TEX-FACT-2          PIC S9(3)       COMP-3.              
           12  TEX-FACT-3          PIC S9(3)       COMP-3.              
           12  TEX-FACT-4          PIC S9(7)       COMP-3.              
           12  TEX-FACT-5          PIC S9(3)       COMP-3.              
           12  TEX-FACT-6          PIC S9(3)       COMP-3.              
           12  TEX-FACT-7          PIC S9(7)       COMP-3.              
           12  TEX-FACT-8          PIC S9V9(6)     COMP-3.              
           12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.              
                                                                        
       01  NET-PAY-INTERFACE.                                           
           12  NP-APR              PIC S9(3)V9(4)  COMP-3.              
           12  NP-ORIG             PIC S9(3)       COMP-3.              
           12  NP-REM              PIC S9(3)       COMP-3.              
           12  NP-OPT              PIC X.                               
           12  NP-CAP              PIC S9(3)       COMP-3.              
           12  NP-FACTOR           PIC S9(4)V9(9)  COMP-3.              
           12  NP-WORK1            PIC S9(9)V9(6)  COMP-3.              
           12  NP-BENEFIT          PIC S9(9)V99    COMP-3.              
           12  NP-REMAINING        PIC S9(9)V99    COMP-3.              
           12  NP-AHPRM            PIC S9(7)V99    COMP-3.              
           12  NP-ACCOUNT          PIC X(10).                           
                                                                        

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
      
       01  t1 pic s999 comp-3 value +0.
       01  t2 pic s999 comp-3 value +0.
       01  work-table.
           02  work-states occurs 70.
               03  work-bene-codes occurs 170.
                   05  work-act-cntr pic s9(7) comp-3 value zeros.
                   05  work-rem-amt  pic s9(11)v99 comp-3 value zeros.



       01  filler     pic x(4096).

       01  ELCNTL-KEY.
           12  CNTL-COMP-ID            PIC  X(3).             
           12  CNTL-REC-TYPE           PIC  X.                
           12  CNTL-ACCESS             PIC  X(4).             
           12  CNTL-SEQ-NO             PIC S9(4)    COMP.     
      
       01  ELCERT-KEY.
           12  CERT-COMPANY-CD         PIC X.
           12  CERT-CARRIER            PIC X.
           12  CERT-GROUPING           PIC X(6).
           12  CERT-STATE              PIC XX.
           12  CERT-ACCOUNT            PIC X(10).
           12  CERT-EFF-DT             PIC XX.
           12  CERT-CERT-NO            PIC X(11).

      
       01  filler.
           05  ws-select-cntr          pic 9(9) value zeros.
           05  ws-inforce-cntr         pic 9(9) value zeros.
           05  ws-abend-message        pic x(80) value spaces.
           05  elcert-sw               pic x value ' '.
               88  end-of-elcert        value 'Y'.
           05  PGM-SUB                 PIC S999  COMP VALUE +012.
           05  ws-return-code          pic s9(4) comp value +0.
           05  LF-REM-TRM1             PIC S999V99 COMP-3 VALUE +0.
           05  LF-REM-TRM2             PIC S999V99 COMP-3 VALUE +0.
           05  intermed                pic s9(9)v9(6) comp-3 value +0.
           05  clm-type                pic x.
           05  clm-no                  pic x(7).
           05  crt-no                  pic x(11).
           05  chk-no                  pic x(10).
           05  chk-amt                 pic -9(9).99.
           05  chk-good-bad            pic x(4).
           05  chk-no-sel-dt           pic x.
           05  chk-who                 pic x(4).
           05  chk-when                pic x(4).

       01  filler.
           12  WS-AH-CATEGORY              PIC X       VALUE ' '.
           12  CLAS-LOOK                   PIC XX      VALUE '  '.

       01  FILLER.
           12  BAL-AH-CLMS-PD-CUR      PIC S9(9)V99 VALUE +0 comp-3.
           12  BAL-LF-CLMS-VOID-CUR    PIC S9(9)V99 VALUE +0 comp-3.
           12  BAL-AH-CLMS-VOID-CUR    PIC S9(9)V99 VALUE +0 comp-3.
      
                                       copy ELCFUNDT.
                                       COPY ELCCNTL.
                                       copy ELCCERT.
                                       copy ELCDATE.
                                       copy ELCCALC.
                                       copy ELCDTECX.
                                       copy ELCDTEVR.

       linkage section.
       
       01 dfhcommarea. 
                                       copy BSSRCH-COMMAREA.
      
       procedure division.
                                       copy ELCDTERX.      
      *****************************************************
      * Using the month end date and company id provided
      * by the user, read the control file company and carrier
      * records then start at the beginning of the ERPNDB
      * file for that company and read until end of file
      * or new company reached. Accumulate processable data of
      * gross premium, refunds and commissions.
      *****************************************************
      
           display ' Begin BSRAMTBL for ' dte-client
           display ' input date ' bl-input-eom-date

           perform 0000-initialize     thru 0000-exit
           perform 0100-process        thru 0100-exit until
              end-of-elcert

           display ' tot inforce ' ws-tot-inforce
           move ws-tot-inforce         to bl-tot-inforce
           display ' selected ' ws-select-cntr
           display ' inforce  ' ws-inforce-cntr

           IF GOOD-STARTBR = 'YES'
              EXEC CICS ENDBR
                 DATASET   ('ELCERT')
              END-EXEC
           END-IF

      *    perform varying t1 from +1 by +1 until t1 > +70
      *       perform varying t2 from +1 by +1 until t2 > +170
      *          if work-act-cntr(t1,t2) > zeros
      *             display state-abbr(t1) ' '
      *             clas-i-ben(t2) ' ' work-act-cntr(t1,t2) ' '
      *             work-rem-amt(t1,t2)
      *          end-if
      *       end-perform
      *    end-perform

           exec cics return
           end-exec

           .
       0000-initialize.

110220     string
110220        bl-input-eom-date(7:4)
110220        bl-input-eom-date(1:2)
110220        bl-input-eom-date(4:2)
110220          delimited by size into dc-greg-date-cymd-r
110220     end-string
110220
110220     move 'L'                    to dc-option-code
110220     perform 8500-date-convert   thru 8500-exit
110220     if no-conversion-error
110220        move dc-bin-date-1       to ws-eom-bin-dt
110220     end-if

      *    display ' clas start s,l ' clas-starts ' ' clas-startl
           move clas-starts            to clas-indexs
           move clas-startl            to clas-indexl

           perform 0010-start-elcert   thru 0010-exit
           if not resp-normal
              display ' bad start elcert ' ws-response
              set end-of-elcert to true
              go to 0000-exit
           end-if
           move 'YES'                  to good-startbr
           perform 0020-read-next      thru 0020-exit
           if not resp-normal
              display ' bad readnext elcert ' ws-response
              set end-of-elcert to true
              go to 0000-exit
           end-if

           .
       0000-EXIT.
           EXIT.

       0010-start-elcert.

           move dte-clasic-company-cd  to elcert-key

           exec cics startbr
              dataset    ('ELCERT')
              ridfld     (elcert-key)
              gteq
              resp       (ws-response)
           end-exec

           .
       0010-exit.
           exit.

       0020-read-next.

           exec cics readnext
              dataset   ('ELCERT')
              ridfld    (elcert-key)
              into      (certificate-master)
              resp      (ws-response)
           end-exec

           if (not resp-normal)
                  or
              (cm-company-cd <> dte-clasic-company-cd)
              set end-of-elcert to true
           end-if

           .
       0020-exit.
           exit.

       0100-process.

           perform 0200-accum-inforce  thru 0200-exit
           perform 0020-read-next      thru 0020-exit

      *    if ws-inforce-cntr > 99
      *       set end-of-elcert to true
      *    end-if

           .
       0100-exit.
           exit.

       0200-accum-inforce.

           move zeros to cp-remaining-amt

           if (cm-lf-benefit-cd = spaces or zeros)
                        or
              (cm-entry-status <> '1' and '4')
                        or
              (cert-pend-issue-error)
                        or
              (cm-lf-current-status <> '1' and '2' and '4')
              go to 0200-exit
           end-if

           add 1 to ws-select-cntr

           if cm-state <> state-sub(clas-indexs)
              perform varying clas-indexs from clas-starts by +1 until
                 cm-state = state-sub(clas-indexs)
                 or clas-indexs > clas-maxs
              end-perform
              if clas-indexs > clas-maxs
                 display ' Invalid state cd ' cm-state
                 go to 0200-exit
              end-if
           end-if

           if cm-lf-benefit-cd <> clas-i-ben(clas-indexl)
              perform varying clas-indexl from clas-startl by +1 until
                 cm-lf-benefit-cd = clas-i-ben(clas-indexl)
                 or clas-indexl > clas-maxl
              end-perform
              if clas-indexl > clas-maxl
                 display ' Invalid lf bene cd ' cm-lf-benefit-cd
                 go to 0200-exit
              end-if
           end-if

           move cm-cert-eff-dt         to cp-cert-eff-dt
           move cm-loan-1st-pmt-dt     to cp-first-pay-date
110220*    move bin-run-date           to cp-valuation-dt
110220     move ws-eom-bin-dt          to cp-valuation-dt

           move state-sub(clas-indexs) to cp-state
           move state-abbr(clas-indexs)
                                       to cp-state-std-abbrv
           move '3'                    to cp-process-type   
           move dte-clasic-company-cd  to cp-company-cd
           move dte-client             to cp-company-id
           move spaces                 to cp-acct-fld-5
           move dte-rem-trm            to cp-rem-term-method


           MOVE CLAS-I-RL-AH (CLAS-INDEXL)
                                       TO CP-BENEFIT-TYPE
           MOVE CLAS-I-BAL (CLAS-INDEXL)
                                       TO CP-SPECIAL-CALC-CD
           MOVE cm-lf-orig-term        TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM
                                                                        
           IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')
              AND (CLAS-I-CALC-TYPE (CLAS-INDEXL) <> 'L')
              ADD +1                   TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM
           end-if

           IF CP-TERM-IS-DAYS
              MOVE CM-LF-TERM-IN-DAYS  TO CP-TERM-OR-EXT-DAYS
           ELSE
              MOVE ZEROS               TO CP-TERM-OR-EXT-DAYS
           end-if
                                                                        
           MOVE DTE-REM-TRM-CALC-OPTION
                                       TO CP-REM-TRM-CALC-OPTION

           exec cics link
              program  ('ELRTRM')
              commarea (calculation-pass-area)
           end-exec
           if no-cp-error
              continue
           else
              display 'error return from elrtrm ' cp-return-code
              go to 0200-exit
           end-if

           IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')
              AND (CLAS-I-CALC-TYPE (CLAS-INDEXL) <> 'L')
      *       MOVE CP-REMAINING-TERM-2 TO LF-BAL-REMTERM        
              COMPUTE CP-REMAINING-TERM-1 = CP-REMAINING-TERM-1 - 1
              COMPUTE CP-REMAINING-TERM-2 = CP-REMAINING-TERM-2 - 1
           end-if
                                                                        
           IF CP-REMAINING-TERM-1 NEGATIVE
              MOVE ZEROS               TO CP-REMAINING-TERM-1
           end-if

           IF CP-REMAINING-TERM-2 NEGATIVE
              MOVE ZEROS               TO CP-REMAINING-TERM-2
           end-if

           MOVE CP-REMAINING-TERM-1    TO LF-REM-TRM1
           MOVE CP-REMAINING-TERM-2    TO LF-REM-TRM2

           if lf-rem-trm2 = zeros
              go to 0200-exit
           end-if

           if cm-lf-loan-expire-dt <= bin-run-date
      *       display ' found one ' cm-cert-no ' ' lf-rem-trm2
              go to 0200-exit
           end-if

           add 1 to ws-inforce-cntr

           if clas-i-rl-ah(clas-indexl) = 'L' or 'P'
              move cm-lf-benefit-amt   to cp-remaining-amt
              go to 0200-compute
           end-if

           COMPUTE INTERMED ROUNDED =
              cm-lf-benefit-amt / cm-lf-orig-term

           if lf-rem-trm2 = cm-lf-orig-term
              move cm-lf-benefit-amt   to cp-remaining-amt
           end-if

           if clas-i-ep(clas-indexl) = 'B' or 'K' or 'L'
              go to 0200-ordinary-rem
           end-if
           if clas-i-ep(clas-indexl) = 'T'
              go to 0200-calc-texas-rem
           end-if
           if clas-i-ep(clas-indexl) = 'N'
              go to 0200-calc-net-pay-rem
           end-if

           IF (STATE-ABBR (CLAS-INDEXS) = 'OH')
              AND (cm-rate-class <> 'L ')
              IF (cm-lf-orig-term > +60)
                 AND (cm-loan-apr > ZEROS)
                 GO TO 0200-CALC-NET-PAY-REM
              end-if
           end-if

           IF STATE-ABBR (CLAS-INDEXS) = 'MT'
              IF (cm-lf-orig-term > +61)
                 AND (cm-loan-apr > ZEROS)
                 GO TO 0200-CALC-NET-PAY-REM
              end-if
           end-if

           IF STATE-ABBR (CLAS-INDEXS) = 'RI'
              IF (cm-lf-orig-term > +60)
                 AND (cm-loan-apr > ZEROS)
                 GO TO 0200-CALC-NET-PAY-REM
              end-if
           end-if
                                                                        
           .
       0200-ordinary-rem.                                                                       

           IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'
              COMPUTE cp-remaining-amt ROUNDED =                             
                  (INTERMED * lf-rem-trm2) + cm-lf-alt-benefit-amt
           ELSE                                                         
              COMPUTE cp-remaining-amt ROUNDED =                             
                 INTERMED * lf-rem-trm2
           end-if
                                                                        
           go to 0200-compute

           .
       0200-CALC-TEXAS-REM.                                             

           DIVIDE cm-lf-benefit-amt BY cm-lf-orig-term
               GIVING TEX-FACT-1
               
           DIVIDE LF-REM-TRM2 BY cm-pay-frequency
               GIVING TEX-FACT-2
               REMAINDER TEX-FACT-3

           IF TEX-FACT-3 NOT = ZERO                                     
              ADD +1 TO TEX-FACT-2
           end-if

           IF (TEX-FACT-2 * cm-pay-frequency) = cm-lf-orig-term
              MOVE cm-lf-benefit-amt TO cp-remaining-amt
           ELSE                                                         
              COMPUTE cp-remaining-amt ROUNDED =                             
                   (TEX-FACT-1 * (TEX-FACT-2 * cm-pay-frequency))
           end-if
                                                                        
           go to 0200-compute

           .                                                            
       0200-CALC-NET-PAY-REM.                                           
                                                                        
           MOVE cm-lf-benefit-amt      TO  CP-ORIGINAL-BENEFIT
           MOVE cm-loan-apr            TO  CP-LOAN-APR

           MOVE cm-lf-orig-term        TO  CP-ORIGINAL-TERM
           MOVE cm-LOAN-TERM           TO  CP-LOAN-TERM
           MOVE LF-REM-TRM2            TO  CP-REMAINING-TERM
           MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL)
                                       TO CP-SPECIAL-CALC-CD
           MOVE CLAS-I-RL-AH (CLAS-INDEXL)
                                       TO  CP-BENEFIT-TYPE
           MOVE CLAS-I-EP(CLAS-INDEXL) TO  CP-EARNING-METHOD

           MOVE ZEROS                  TO CP-R-MAX-TOT-BEN


           exec cics link
              program  ('ELRAMT')
              commarea (calculation-pass-area)
           end-exec
           if no-cp-error
              continue
           else
              display 'error return from elrAMT ' cp-return-code
              go to 0200-exit
           end-if

           .
       0200-compute.

           add 1 to work-act-cntr(clas-indexs,clas-indexl)
           add cp-remaining-amt to
              work-rem-amt (clas-indexs,clas-indexl)

      *    display ' inforce ' cm-cert-no ' ' lf-rem-trm2 ' '
      *       cp-remaining-amt ' ' clas-indexs ' ' clas-indexl

           compute ws-tot-inforce =
              ws-tot-inforce + cp-remaining-amt

           .
       0200-exit.
           exit.

       8500-DATE-CONVERT.                         

           EXEC CICS LINK
               PROGRAM   ('ELDATCV')
               COMMAREA  (DATE-CONVERSION-DATA)
               LENGTH    (DC-COMM-LENGTH)
           END-EXEC

           .
       8500-EXIT.
           EXIT.

       abend-pgm.
          call 'ABORTME'
          .
