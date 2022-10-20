      *****************************************************************
      *                                                               *
      * Copyright (c) 2022 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCCCFX3.
       AUTHOR.   Cowtown.

       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELCNTL           ASSIGN TO ELCNTL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CF-CONTROL-PRIMARY
                                   FILE STATUS IS ELCNTL-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT FILE-OUT         ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ELCNTL.

                                       COPY ELCCNTL.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FILE-OUT-REC                PIC X(250).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   DCCCFX3   WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ELCNTL                 VALUE 'Y'.
       77  ELCNTL-RECS-IN              PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S999  VALUE +0 COMP-3.
       77  S2                          PIC S999  VALUE +0 COMP-3.
       77  C1                          PIC S999  VALUE +0 COMP-3.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  PGM-SUB                     PIC S9(5) COMP-3 VALUE +515.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WS-MOE-DATE                 pic x(10).

       01  ben-code-table.
           05  bc-lf-ah                pic x.
           05  bc-ben-code             pic xx.
           05  bc-alpha                pic xxx.
           05  bc-desc                 pic x(10).
           05  bc-comment              pic x(10).
           05  bc-cov-type             pic x.
           05  bc-spec-calc            pic x.
           05  bc-jnt-ind              pic x.
           05  bc-max-bens             pic 999.
           05  bc-category             pic x.
           05  bc-loan-type            pic x(10).
           05  bc-rem-term             pic x.
           05  bc-earn-calc            pic x.
           05  bc-ref-method           pic x.
           05  bc-ovrd-earn            pic x.
           05  bc-ind-grp              pic x.

      ******************************************************************
       01  WS-MISC.
           05  WS-CNTR                 PIC 9(4) VALUE ZEROS.
           05  ELCNTL-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

       0000-here-we-go.

           display ' Begin Program DCCCFX3 '

           perform 0100-open-files     thru 0100-exit

           PERFORM 0110-INITIALIZE     THRU 0110-EXIT

           PERFORM 0500-PROCESS-ELCNTL THRU 0500-EXIT UNTIL
              (END-OF-ELCNTL)
PEMTST*       OR (CLM-RECS-IN > 1000)

           PERFORM 5000-CLOSE-FILES    THRU 5000-EXIT

           GOBACK

           .
       0100-OPEN-FILES.

           OPEN INPUT ELCNTL OUTPUT FILE-OUT

           if elcntl-file-status not = '00' and '97'
              display ' Error-ELCNTL-Open ' elcntl-file-status
              perform abend-pgm
           end-if

           .
       0100-EXIT.
           EXIT.

       0110-INITIALIZE.

           PERFORM 0140-START-ELCNTL   THRU 0140-EXIT
           PERFORM 0150-READ-ELCNTL    THRU 0150-EXIT

           move spaces to file-out-rec
           string
              'LF_AH_TYP;'
              'BEN_CODE;'
              'ALPHA;'
              'DESCRIPTION;'
              'COMMENT;'
              'COV_TYPE;'
              'SPEC_CALC_CD;'
              'IND_JNT;'
              'MAX_BENS;'
              'CATEGORY;'
              'LOAN_TYPE;'
              'REM_TERM;'
              'EARN_CALC;'
              'REF_METHOD;'
              'OVRD_EARN;'
              'IND_GRP;EOR'
              delimited by size into file-out-rec
           end-string
           write file-out-rec

           .
       0110-EXIT.
           EXIT.


       0140-START-ELCNTL.

           MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '4'                    TO CF-RECORD-TYPE  *> lf bencds
           START ELCNTL KEY >= CF-CONTROL-PRIMARY

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCNTL        TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL START     ' ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              END-IF
           END-IF

           .
       0140-EXIT.
           EXIT.

       0150-READ-ELCNTL.

           READ ELCNTL NEXT RECORD

           IF (ELCNTL-FILE-STATUS = '10' OR '23')
              or (cf-company-id <> dte-client)
              or (cf-record-type <> '4' and '5')
              SET END-OF-ELCNTL        TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL READ NEXT ' ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELCNTL
              ADD 1 TO ELCNTL-RECS-IN
           END-IF

           .
       0150-EXIT.
           EXIT.


       0500-PROCESS-ELCNTL.

           if cf-record-type = '4'
              perform varying s1 from +1 by +1 until s1 > +8
                 perform 0510-lf-ben   thru 0510-exit
              end-perform
           else
              perform varying s1 from +1 by +1 until s1 > +8
                 perform 0520-ah-ben   thru 0520-exit
              end-perform
           end-if

           PERFORM 0150-READ-ELCNTL    THRU 0150-EXIT

           .
       0500-EXIT.
           EXIT.

       0510-lf-ben.

           if cf-benefit-code (s1) = '00' or '  '
              go to 0510-exit
           end-if

           move 'L'                    to bc-lf-ah
           move cf-benefit-code (s1)   to bc-ben-code
           move cf-benefit-alpha (s1)  to bc-alpha
           move cf-benefit-descrip (s1)
                                       to bc-desc
           move cf-benefit-comment (s1)
                                       to bc-comment
           move cf-lf-coverage-type (s1)
                                       to bc-cov-type
           move cf-special-calc-cd (s1)
                                       to bc-spec-calc
           move cf-joint-indicator (s1)
                                       to bc-jnt-ind
           if cf-maximum-benefits (s1) not numeric
              move zeros               to cf-maximum-benefits (s1)
           end-if
           move cf-maximum-benefits (s1)
                                       to bc-max-bens
           move cf-benefit-category (s1)
                                       to bc-category
           move cf-loan-type (s1)      to bc-loan-type
           move cf-co-rem-term-calc (s1)
                                       to bc-rem-term
           move cf-co-earnings-calc (s1)
                                       to bc-earn-calc
           move cf-co-refund-calc (s1) to bc-ref-method
           move cf-co-ovrd-earnings-calc (s1)
                                       to bc-ovrd-earn
           move cf-co-ben-i-g-cd (s1)  to bc-ind-grp
           
           perform 0530-insert-row     thru 0530-exit

           .
       0510-exit.
           exit.

       0520-ah-ben.

           if cf-benefit-code (s1) = '00' or '  '
              go to 0520-exit
           end-if

           move 'A'                    to bc-lf-ah
           move cf-benefit-code (s1)   to bc-ben-code
           move cf-benefit-alpha (s1)  to bc-alpha
           move cf-benefit-descrip (s1)
                                       to bc-desc
           move cf-benefit-comment (s1)
                                       to bc-comment
           move spaces                 to bc-cov-type
           move cf-special-calc-cd (s1)
                                       to bc-spec-calc
           move cf-joint-indicator (s1)
                                       to bc-jnt-ind
           if cf-maximum-benefits (s1) not numeric
              move zeros               to cf-maximum-benefits (s1)
           end-if
           move cf-maximum-benefits (s1)
                                       to bc-max-bens
           move cf-benefit-category (s1)
                                       to bc-category
           move cf-loan-type (s1)      to bc-loan-type
           move cf-co-rem-term-calc (s1)
                                       to bc-rem-term
           move cf-co-earnings-calc (s1)
                                       to bc-earn-calc
           move cf-co-refund-calc (s1) to bc-ref-method
           move cf-co-ovrd-earnings-calc (s1)
                                       to bc-ovrd-earn
           move cf-co-ben-i-g-cd (s1)  to bc-ind-grp
           
           perform 0530-insert-row     thru 0530-exit

           .
       0520-exit.
           exit.

       0530-insert-row.

           move spaces                 to FILE-OUT-REC
           string
              bc-lf-ah      ';'
              bc-ben-code   ';'
              bc-alpha      ';'
              bc-desc       ';'
              bc-comment    ';'
              bc-cov-type   ';'
              bc-spec-calc  ';'
              bc-jnt-ind    ';'
              bc-max-bens   ';'
              bc-category   ';'
              bc-loan-type  ';'
              bc-rem-term   ';'
              bc-earn-calc  ';'
              bc-ref-method ';'
              bc-ovrd-earn  ';'
              bc-ind-grp    ';E'
              delimited by size into FILE-OUT-REC
           end-string

           WRITE FILE-OUT-REC

           .
       0530-exit.
           exit.

       5000-CLOSE-FILES.

           CLOSE ELCNTL FILE-OUT

           .
       5000-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
