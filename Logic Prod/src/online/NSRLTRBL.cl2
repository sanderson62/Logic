       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   NSRLTRBL.

      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.                                       

      *REMARKS.    EXECUTED FROM NSREQLTR

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 121802    2009122800001  PEMA  NEW PROGRAM
022212* 022212    2011120900003  AJRA  ADD AHL
031912* 031912    2011120900003  AJRA  ADD AHL CLAIM NO TO EXTRACT
020513* 020513    2011090100001  PEMA  CORRECT ISS WITH MULT &
020613* 020613    2012110800002  AJRA  ADD BUS TYPE (GPCD) TO NAPERSOFT
031116* 031116    2015110400001  TANA  ADD EL150D FIELDS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
031319* 031319  IR2019031100002  TANA  Correct iss w/ letters with ???
071719* 071719    2019011600010  TANA  ADD VERIFICATION CODE
061421* 061421  CR2017031500001  PEMA  Update to CCM8
      ******************************************************************
       ENVIRONMENT DIVISION.                                            

       DATA DIVISION.                                                   

       working-storage section.
       
       77  SAVE-DATE                   PIC X(8)    VALUE SPACES.
       77  WS-SAVE-EDIT-A-DATE         PIC X(10)   VALUE SPACES.
       77  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
       77  S1                          PIC S999 COMP-3 VALUE +0.
031116 77  S2                          PIC S999 COMP-3 VALUE +0.
031116 77  S3                          PIC S999 COMP-3 VALUE +0.
       77  E1                          PIC S999 COMP-3 VALUE +0.
       77  A1                          PIC S9(5) COMP-3 VALUE +0.
       77  M1                          PIC S999 COMP-3 VALUE +0.
031116 77  P1                          PIC S999 COMP-3 VALUE +0.
031116 77  WS-HOLD-KEY                 PIC X(20).
031116 77  WS-HOLD-S1                  PIC S999 COMP-3 VALUE +0.
       77  WS-WORK-FIELD               PIC X(80)    VALUE SPACES.
       77  WS-ARCHIVE-NO               PIC S9(8)  COMP VALUE +0.
       77  WS-FOLLOW-UP-DT             PIC XX  VALUE LOW-VALUES.
       77  WS-RESEND-DT                PIC XX  VALUE LOW-VALUES.
       77  WS-AUTO-LAST-SCHED-DT       PIC XX  VALUE LOW-VALUES.
       77  WS-LAST-ACT-DT              PIC XX  VALUE LOW-VALUES.
       77  WS-ACTIVITY-DT              PIC XX  VALUE LOW-VALUES.
       77  WS-FORM                     PIC XXXX  VALUE SPACES.
       77  WS-LAST-ACT-TYPE            PIC X   VALUE ' '.
       77  WS-FOUND-BENE-SW            PIC X   VALUE ' '.
           88  FOUND-BENE                  VALUE 'Y'.
       77  WS-TALLY                    PIC S999 COMP-3 VALUE +0.
       77  WS-TALLY1                   PIC S999 COMP-3 VALUE +0.
       77  NS-LEN                      PIC S9(5) COMP-3 VALUE +0.
031116 77  WS-CNTR                     pic s999 comp-3 value +0.
031116 77  WS-MONTHS-BETWEEN           PIC S999 COMP-3 VALUE +0.
031116 77  WS-ACCUM-DAYS               PIC S9(5) COMP-3 VALUE +0.
031116 77  WS-ACCUM-AMT                PIC S9(9)V99 COMP-3 VALUE +0.
031116 77  WS-ACCUM-PD-BENS            PIC S999 COMP-3 VALUE +0.
031116 77  WS-PREV-CLM-TYPE            PIC X   VALUE ' '.
031116 77  WS-PREV-INS-TYPE            PIC X   VALUE ' '.
031116 77  WS-PREV-BEN-PER             PIC 99 VALUE ZEROS.
031116 77  WS-PDEF-RECORD-SW           PIC X  VALUE ' '.
031116     88  PDEF-FOUND                   VALUE 'Y'.
031116 77  wk1                         pic 999 value zeros.
031116 77  wk2                         pic 999 value zeros.
031116 77  WS-WORK-BEN-PCT             PIC S9V999 COMP-3 VALUE +0.
031116 77  WS-MAX-SUB                  PIC 9(4)    VALUE ZEROS.

031116 01  OUTPUT-SCREEN-WORK-AREA.
031116     05  OS-PREV-KEY.
031116         10  OS-PREV-CLM-TYPE        PIC X.
031116         10  OS-PREV-INS-TYPE        PIC X.
031116     05  WS-PD-BENS                  PIC 999 VALUE ZEROS.
031116     05  WS-COV-REM-BENS             PIC S999 VALUE ZEROS.


       01  WS-XML-WORK                 PIC X(2500)  VALUE SPACES.
       01 response-code         pic s9(8) comp.
       01 display-response      pic 9(8).
       01 bl-index              pic 9(8) comp.
       01 max-last-name         pic x(18).
       01 first-initial         pic x.
       01 name-in-range-flag    pic 9.
       01 max-entries           pic s9(8) comp value 100.
       
       01 lower-case    pic x(26) value
                  "abcdefghijklmnopqrstuvwxyz".
       01 upper-case    pic x(26) value
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

       01  W-Z-CONTROL-DATA.
           05  W-NUMBER-OF-COPIES  PIC  9(01).
           05  FILLER              PIC  X(01).
           05  W-DAYS-TO-FOLLOW-UP PIC  9(03).
           05  FILLER              PIC  X(01).
           05  W-DAYS-TO-RESEND-1  PIC  9(03).
           05  FILLER              PIC  X(01).
           05  W-FORM-TO-RESEND    PIC  X(04).
           05  FILLER              PIC  X(01).
           05  W-PROMPT-LETTER     PIC  X(01).
           05  FILLER              PIC  X(01).
           05  W-ENCLOSURE-CD      PIC  X(03).
           05  FILLER              PIC  X(1).
           05  W-AUTO-CLOSE-IND    PIC  X(1).
           05  FILLER              PIC  X(1).
           05  W-LETTER-TO-BENE    PIC  X(1).

       01  MISC.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPREC                  VALUE +14.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.

       01  WS-ELENCC-KEY.
           05  WS-ELENCC-COMPANY-CD    PIC X.
           05  WS-ELENCC-REC-TYPE      PIC X.
           05  WS-ELENCC-ENC-CODE      PIC X(5).
           05  F                       PIC X(09).

       01  WS-ELMSTR-KEY.
           05  WS-ELMSTR-COMPANY-CD    PIC X.
           05  WS-ELMSTR-CARRIER       PIC X.
           05  WS-ELMSTR-CLAIM-NO      PIC X(7).
           05  WS-ELMSTR-CERT-NO       PIC X(11).
       01  WS-ELTRLR-KEY.
           05  WS-ELTRLR-COMPANY-CD    PIC X.
           05  WS-ELTRLR-CARRIER       PIC X.
           05  WS-ELTRLR-CLAIM-NO      PIC X(7).
           05  WS-ELTRLR-CERT-NO       PIC X(11).
           05  WS-ELTRLR-SEQ-NO        PIC S9(4) COMP VALUE +0.
       01  WS-ELCERT-KEY.
           05  WS-ELCERT-COMPANY-CD    PIC X.
           05  WS-ELCERT-CARRIER       PIC X.
           05  WS-ELCERT-GROUP         PIC X(6).
           05  WS-ELCERT-STATE         PIC XX.
           05  WS-ELCERT-ACCOUNT       PIC X(10).
           05  WS-ELCERT-EFF-DT        PIC XX.
           05  WS-ELCERT-CERT-NO       PIC X(11).
       01  WS-ERACCT-KEY.
           05  WS-ERACCT-COMPANY-CD    PIC X.
           05  WS-ERACCT-CARRIER       PIC X.
           05  WS-ERACCT-GROUP         PIC X(6).
           05  WS-ERACCT-STATE         PIC XX.
           05  WS-ERACCT-ACCOUNT       PIC X(10).
           05  WS-ERACCT-EXP-DT        PIC XX.
           05  FILLER                  PIC XXXX.
       01  WS-ELBENE-KEY.
           05  WS-ELBENE-COMPANY-CD    PIC X.
           05  WS-ELBENE-REC-TYPE      PIC X.
           05  WS-ELBENE-BENE          PIC X(10).
       01  WS-ELLETR-KEY.
           05  WS-ELLETR-COMPANY-CD    PIC X.
           05  WS-ELLETR-LETTER-ID     PIC X(12).
           05  WS-ELLETR-SEQ-NO        PIC S9(4) COMP VALUE +0.
       01  WS-ELCNTL-KEY.
           05  WS-ELCNTL-COMPANY-ID    PIC XXX.
           05  WS-ELCNTL-REC-TYPE      PIC X.
           05  WS-ELCNTL-GENL.
               10  FILLER              PIC XX  VALUE SPACES.
               10  WS-ELCNTL-BEN-CD    PIC XX.
           05  WS-ELCNTL-SEQ-NO        PIC S9(4) COMP.
031116 01  ACCESS-KEYS.
031116     12  ELCRTT-KEY.
031116         16  CRTT-COMP-CD            PIC X.
031116         16  CRTT-CARRIER            PIC X.
031116         16  CRTT-GROUPING           PIC X(6).
031116         16  CRTT-STATE              PIC XX.
031116         16  CRTT-ACCOUNT            PIC X(10).
031116         16  CRTT-EFF-DT             PIC XX.
031116         16  CRTT-CERT-NO            PIC X(11).
031116         16  CRTT-REC-TYPE           PIC X.
031116
031116 01  ERPDEF-KEY-SAVE             PIC X(18).
031116 01  ERPDEF-KEY.
031116     12  ERPDEF-COMPANY-CD       PIC X.
031116     12  ERPDEF-STATE            PIC XX.
031116     12  ERPDEF-PROD-CD          PIC XXX.
031116     12  F                       PIC X(7).
031116     12  ERPDEF-BEN-TYPE         PIC X.
031116     12  ERPDEF-BEN-CODE         PIC XX.
031116     12  ERPDEF-EXP-DT           PIC XX.
031116 01  TEXT-WORK-AREAS.
031116     05  WS-COV-TYPE             PIC X(4) VALUE SPACES.

031116 01  WS-UNSORTED-TABLE.
031116     12  WS-UNSRTD-TABLE   OCCURS 25 TIMES.
031116         16  WS-KEY.
031116             20  WS-CLM-TYPE     PIC X.
031116             20  WS-INS-TYPE     PIC X.
031116             20  WS-BEN-PER      PIC 99.
031116             20  WS-INC-DT       PIC XX.
031116         16  WS-EXCL-PER         PIC 999.
031116         16  WS-COV-ENDS         PIC 999.
031116         16  WS-ACC-PER          PIC 999.
031116         16  WS-MAX-BENS         PIC 999.
031116         16  WS-REC-MOS          PIC 99.
031116         16  WS-MAX-EXTEN        PIC 99.
031116         16  WS-STATUS           PIC X.
031116         16  WS-PD-THRU-DT       PIC XX.
031116         16  WS-CLAIM-NO         PIC X(7).
031116         16  WS-MAX-MOBEN        PIC S9(7)V99 COMP-3.
031116         16  WS-TOTAL-PAID       PIC S9(7)V99 COMP-3.
031116         16  WS-REM-BENS         PIC 999.
031116         16  WS-SORTED-SW        PIC X.

031116 01  WS-SORTED-TABLE.
031116     12  WS-SRTD-TABLE OCCURS 25 TIMES.
031116         16  WS-SRTD-KEY.
031116             20  WS-SRTD-CLM-TYPE PIC X.
031116             20  WS-SRTD-INS-TYPE PIC X.
031116             20  WS-SRTD-BEN-PER PIC 99.
031116             20  WS-SRTD-INC-DT  PIC XX.
031116         16  WS-SRTD-EXCL-PER    PIC 999.
031116         16  WS-SRTD-COV-ENDS    PIC 999.
031116         16  WS-SRTD-ACC-PER     PIC 999.
031116         16  WS-SRTD-MAX-BENS    PIC 999.
031116         16  WS-SRTD-REC-MOS     PIC 99.
031116         16  WS-SRTD-MAX-EXTEN   PIC 99.
031116         16  WS-SRTD-STATUS      PIC X.
031116         16  WS-SRTD-PD-THRU-DT  PIC XX.
031116         16  WS-SRTD-CLAIM-NO    PIC X(7).
031116         16  WS-SRTD-MAX-MOBEN   PIC S9(7)V99 COMP-3.
031116         16  WS-SRTD-TOTAL-PAID  PIC S9(7)V99 COMP-3.
031116         16  WS-SRTD-REM-BENS    PIC 999.
031116         16  WS-SRTD-SW          PIC X.

031116 01  SAVE-ACCOUNT-MASTER         PIC X(2000) VALUE SPACES.

                                       COPY ELCDATE.
                                       COPY ELCMSTR.
                                       COPY ELCTRLR.
                                       COPY ELCCERT.
                                       COPY ERCACCT.
                                       COPY ELCBENE.
                                       COPY ELCTEXT.
                                       COPY ELCCNTL.
                                       COPY NSCVARS.
       01  FILLER                      PIC X(500)  VALUE SPACES.
                                       COPY ELCNAPS.
                                       COPY ELCENCC.
031116                                 COPY ELCCRTT.
031116                                 COPY ERCPDEF.
       linkage section.
       
       01 dfhcommarea. 
                                       copy ELCLTRSPI.

       procedure division.

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE
           MOVE DC-GREG-DATE-A-EDIT    TO WS-SAVE-EDIT-A-DATE
           MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE

      *****************************************************
      * The full claim key has been established and passed
      * to this program via NSREQLTR
      *****************************************************

031319     INITIALIZE NAPER-OUTPUT-DATA
           evaluate bl-comp-id
              when 'DCC'
                 MOVE X'05'            TO WS-ELMSTR-COMPANY-CD
              when 'AHL'
                 MOVE X'06'            TO WS-ELMSTR-COMPANY-CD
              when 'VPP'
                 MOVE X'07'            TO WS-ELMSTR-COMPANY-CD
061421        when 'FNL'
061421           MOVE X'08'            TO WS-ELMSTR-COMPANY-CD
              when other
                 MOVE X'04'            TO WS-ELMSTR-COMPANY-CD
           end-evaluate

           MOVE BL-CARRIER             TO WS-ELMSTR-CARRIER
           MOVE BL-CLAIM-NO            TO WS-ELMSTR-CLAIM-NO
           MOVE BL-CERT-NO             TO WS-ELMSTR-CERT-NO
           SET BL-FAIL TO TRUE

           EXEC CICS READ                                               
                DATASET    ('ELMSTR') 
                INTO       (CLAIM-MASTER)                    
                RIDFLD     (WS-ELMSTR-KEY)                                   
                RESP       (WS-RESPONSE)
           END-EXEC.                                                    

           IF RESP-NORMAL
              MOVE BL-LETTER-ID        TO OUT-TEMP
                                          OUT-LETTER
              MOVE BL-PROC-ID          TO OUT-PROCID
              MOVE BL-COMP-ID          TO OUT-COMPID
              MOVE BL-PRINT-NOW-SW     TO OUT-PRINT-NOW-SW
              MOVE CL-CARRIER          TO OUT-CARR
                                          OUT-CARR-LU
              MOVE CL-INSURED-LAST-NAME
                                       TO OUT-ILNAME
              MOVE CL-INSURED-1ST-NAME TO OUT-IFNAME
              MOVE CL-CLAIM-NO         TO OUT-CLMNO
              MOVE CL-CERT-NO          TO OUT-CRTNO
              MOVE CL-CERT-ACCOUNT     TO OUT-ACTNO
              MOVE CL-SOC-SEC-NO       TO OUT-SSN
              MOVE CL-CLAIM-STATUS     TO OUT-CLMSTAT
031912        MOVE CL-CCN (1:9)        TO OUT-AHL-CLAIM-NO              

              MOVE WS-SAVE-EDIT-A-DATE TO DC-GREG-DATE-A-EDIT 
              STRING DC-EDITA-CCYY  DC-EDITA-MONTH DC-EDITA-DAY
                DELIMITED BY SIZE INTO OUT-CYCLE-DATE
              END-STRING

      *       IF CLAIM-IS-CLOSED
      *          MOVE 'CLOSED'         TO OUT-CLMSTAT
      *       ELSE
      *          MOVE 'OPEN'           TO OUT-CLMSTAT
      *       END-IF
              MOVE CL-CLAIM-TYPE       TO OUT-CLMTYPE
              MOVE CL-FILE-ESTABLISH-DT
                                       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE             
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                    DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-EST-DT
                 END-STRING
              ELSE
                 MOVE SPACES           TO OUT-EST-DT
              END-IF
              MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE             
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                    DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-PD-THRU-DT
                 END-STRING
              ELSE
                 MOVE SPACES           TO OUT-PD-THRU-DT
              END-IF

              MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1
              MOVE +0                  TO DC-ELAPSED-MONTHS
              MOVE +30                 TO DC-ELAPSED-DAYS
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                    DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-FORM-DUE-DT
                 END-STRING
              ELSE
                 MOVE SPACES           TO OUT-FORM-DUE-DT
              END-IF

              MOVE CL-TOTAL-PAID-AMT   TO OUT-TOT-PAID
              MOVE CL-INSURED-SEX-CD   TO OUT-SEX-CD
              MOVE CL-CERT-STATE       TO OUT-CRT-ST
                                          OUT-ENC-ST
                                          OUT-ENC-ST-NAME

              MOVE CL-CERT-EFF-DT      TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE             
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                    DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-EFF-DT
                 END-STRING
              ELSE
                 MOVE SPACES           TO OUT-EFF-DT
              END-IF
              MOVE CL-INCURRED-DT      TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE             
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                    DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-INC-DT
                 END-STRING
              ELSE
                 MOVE SPACES           TO OUT-INC-DT
              END-IF
              MOVE CL-REPORTED-DT      TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE             
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                    DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-RPT-DT
                 END-STRING
              ELSE
                 MOVE SPACES           TO OUT-RPT-DT
              END-IF
              MOVE CL-LAST-PMT-DT      TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE             
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                    DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-LST-PMT-DT
                 END-STRING
              ELSE
                 MOVE SPACES           TO OUT-LST-PMT-DT
              END-IF
              MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE             
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                    DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-BIRTH-DT
                 END-STRING
              ELSE
                 MOVE SPACES           TO OUT-BIRTH-DT
              END-IF
              MOVE CL-LAST-MAINT-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE             
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                    DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-LST-MAINT-DT
                 END-STRING
              ELSE
                 MOVE SPACES           TO OUT-LST-MAINT-DT
              END-IF
              MOVE CL-LAST-MAINT-USER  TO OUT-LST-MAINT-USER
              MOVE CL-LAST-MAINT-TYPE  TO OUT-LST-MAINT-TYPE
              MOVE CL-NO-OF-PMTS-MADE  TO OUT-NO-OF-PMTS
              MOVE CL-LAST-PMT-AMT     TO OUT-LST-PD-AMT
              MOVE CL-ACCOUNT-ADDR-CNT TO OUT-ADDR-CNT
              MOVE CL-LAST-CLOSE-REASON TO OUT-LST-CLS-REA
              MOVE BL-ENC-CD           TO OUT-ENC-CODE
              PERFORM 1000-GET-FILES   THRU 1000-EXIT
031116        PERFORM 1800-SET-EL150D-FIELDS THRU 1800-EXIT
071719        MOVE FUNCTION REVERSE(CL-CLAIM-NO(3:5)) TO OUT-VER-CD
              SET BL-OK TO TRUE
           END-IF

           IF BL-OK
              PERFORM 1500-GET-ARCH-NO THRU 1500-EXIT
              MOVE WS-ARCHIVE-NO       TO OUT-ARCHNO
              PERFORM 0500-SCRUB-DATA  THRU 0500-EXIT
              MOVE NAPER-OUTPUT-DATA   TO BL-RECORD-PASSED-DATA
      *       PERFORM 1600-ADD-CORR-TRLR
      *                                THRU 1600-EXIT
           END-IF

           exec cics return end-exec.	

       0500-SCRUB-DATA.

      *   THE DECODER DOES NOT LIKE & AND I TRIED TO USE
      *   A DIFFERENT DELIMITER IN PROGRAM NSREQLTR BUT
      *   COULD NOT GET IT TO WORK SO I AM CONVERTING IT
      *   TO A HEX 26 TO GET BY THE DECODER

           MOVE FUNCTION LENGTH(OUT-ILNAME)
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD
020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-ilname (a1:1) = '&'
020513           MOVE OUT-ILNAME (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE OUT-ILNAME (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO OUT-ILNAME
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-ACCT-ADDR1)
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-acct-addr1 (a1:1) = '&'
020513           MOVE out-acct-addr1 (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-acct-addr1 (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-acct-addr1
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-ACCT-ADDR2)
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-acct-addr2 (a1:1) = '&'
020513           MOVE out-acct-addr2 (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-acct-addr2 (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-acct-addr2
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-ACCT-CITY)
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-acct-city (a1:1) = '&'
020513           MOVE out-acct-city (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-acct-city (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-acct-city
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-ACCT-NAME)
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-acct-name (a1:1) = '&'
020513           MOVE out-acct-name (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-acct-name (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-acct-name
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-ORIG-NAME)
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-orig-name (a1:1) = '&'
020513           MOVE out-orig-name (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-orig-name (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-orig-name
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-INS-NAME)
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-ins-name (a1:1) = '&'
020513           MOVE out-ins-name (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-ins-name (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-ins-name
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-INS-ADDR1)
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-ins-addr1 (a1:1) = '&'
020513           MOVE out-ins-addr1 (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-ins-addr1 (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-ins-addr1
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-INS-ADDR2)
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-ins-addr2 (a1:1) = '&'
020513           MOVE out-ins-addr2 (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-ins-addr2 (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-ins-addr2
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-BEN-NAME)   TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-ben-name (a1:1) = '&'
020513           MOVE out-ben-name (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-ben-name (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-ben-name
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-BEN-ADDR1)
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-ben-addr1 (a1:1) = '&'
020513           MOVE out-ben-addr1 (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-ben-addr1 (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-ben-addr1
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-BEN-ADDR2)
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-ben-addr2 (a1:1) = '&'
020513           MOVE out-ben-addr2 (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-ben-addr2 (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-ben-addr2
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE FUNCTION LENGTH(OUT-DIAG)       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD

020513     perform varying a1 from +1 by +1 until a1 > m1
020513        if out-diag (a1:1) = '&'
020513           MOVE out-diag (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
020513           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
020513           MOVE out-diag (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
020513           MOVE WS-WORK-FIELD    TO out-diag
020513           add +2 to a1
020513        end-if
020513     end-perform

           MOVE ZEROS TO WS-TALLY WS-TALLY1
           INSPECT NAPER-OUTPUT-DATA TALLYING WS-TALLY
              FOR ALL '%'
           INSPECT NAPER-OUTPUT-DATA TALLYING WS-TALLY1
              FOR ALL '%26'
           IF (WS-TALLY > 0)
              AND (WS-TALLY NOT = WS-TALLY1)
              MOVE +1                  TO A1
              MOVE FUNCTION LENGTH(NAPER-OUTPUT-DATA)
                                       TO NS-LEN
              PERFORM WS-TALLY TIMES
                 PERFORM VARYING A1 FROM A1 BY +1 UNTIL
                    (A1 > NS-LEN)
                    OR (NAPER-OUTPUT-DATA (A1:1) = '%')
                 END-PERFORM
                 IF A1 > NS-LEN
                    CONTINUE
                 ELSE
                    IF NAPER-OUTPUT-DATA (A1:3) = '%26'
                       ADD +1 TO A1
                    ELSE
                       MOVE NAPER-OUTPUT-DATA (1:A1 - 1)
                                       TO WS-XML-WORK
                       MOVE '%25'      TO WS-XML-WORK (A1:3)
                       MOVE NAPER-OUTPUT-DATA (A1 + 1:NS-LEN - A1)
                                    TO WS-XML-WORK (A1 + 3:NS-LEN - A1)
                       MOVE WS-XML-WORK TO NAPER-OUTPUT-DATA
                       ADD +3 TO A1
                    END-IF
                 END-IF
              END-PERFORM
           END-IF

           .
       0500-EXIT.
           EXIT.

       1000-GET-FILES.

           PERFORM 1100-GET-ELCERT     THRU 1100-EXIT
           PERFORM 1200-GET-ERACCT     THRU 1200-EXIT

           MOVE ' '                    TO WS-FOUND-BENE-SW
           PERFORM 1250-GET-ELLETR     THRU 1250-EXIT
           PERFORM 1300-GET-ELTRLRS    THRU 1300-EXIT
           PERFORM 1700-GET-ELENCC     THRU 1700-EXIT

100518     IF CL-CLAIM-TYPE NOT = 'L' AND 'P' AND 'O'
              PERFORM 1400-GET-WAIT-PER THRU 1400-EXIT
           END-IF

           IF NOT FOUND-BENE
              MOVE CL-COMPANY-CD       TO WS-ELBENE-COMPANY-CD
              MOVE 'B'                 TO WS-ELBENE-REC-TYPE
              MOVE CL-BENEFICIARY      TO WS-ELBENE-BENE
              EXEC CICS READ                                               
                   DATASET    ('ELBENE') 
                   INTO       (BENEFICIARY-MASTER)              
                   RIDFLD     (WS-ELBENE-KEY)                                   
                   RESP       (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 MOVE BE-MAIL-TO-NAME   TO OUT-BEN-NAME
                 MOVE BE-ADDRESS-LINE-1 TO OUT-BEN-ADDR1
                 MOVE BE-ADDRESS-LINE-2 TO OUT-BEN-ADDR2
                 MOVE BE-CITY           TO OUT-BEN-CITY
                 MOVE BE-STATE          TO OUT-BEN-STATE
                 MOVE BE-ZIP-PRIME      TO OUT-BEN-ZIP
                 IF BE-ZIP-PLUS4 NOT = SPACES AND ZEROS
                    STRING '-' BE-ZIP-PLUS4 DELIMITED BY SIZE
                       INTO OUT-BEN-ZIP (6:5)
                    END-STRING
                 END-IF
                 MOVE BE-PHONE-NO       TO OUT-BEN-PHONE
              END-IF
           END-IF

           .
       1000-EXIT.
           EXIT.

       1100-GET-ELCERT.

           MOVE CL-COMPANY-CD          TO WS-ELCERT-COMPANY-CD
           MOVE CL-CERT-KEY-DATA       TO WS-ELCERT-KEY (2:21)
           MOVE CL-CERT-NO             TO WS-ELCERT-CERT-NO

           EXEC CICS READ                                               
                DATASET    ('ELCERT') 
                INTO       (CERTIFICATE-MASTER)              
                RIDFLD     (WS-ELCERT-KEY)                                   
                RESP       (WS-RESPONSE)
           END-EXEC.                                                    

           IF RESP-NORMAL
100518        IF CL-CLAIM-TYPE = 'L' OR 'P' OR 'O'
                 MOVE CM-LF-BENEFIT-AMT TO OUT-BENAMT
                 MOVE CM-LF-ORIG-TERM  TO OUT-TERM
                 MOVE CM-LF-BENEFIT-CD TO OUT-BEN-CD
                 MOVE CM-LF-CRITICAL-PERIOD
                                       TO OUT-CRIT-PER
                 MOVE CM-LF-LOAN-EXPIRE-DT
                                       TO DC-BIN-DATE-1
                 MOVE ' '              TO DC-OPTION-CODE             
                 PERFORM 9700-DATE-LINK
                                       THRU 9700-EXIT
                 IF NO-CONVERSION-ERROR
                    STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                       DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                       INTO OUT-EXP-DT
                    END-STRING
                 ELSE
                    MOVE SPACES        TO OUT-EXP-DT
                 END-IF
              ELSE
                 MOVE CM-AH-BENEFIT-AMT TO OUT-BENAMT
                 MOVE CM-AH-ORIG-TERM  TO OUT-TERM
                 MOVE CM-AH-BENEFIT-CD TO OUT-BEN-CD
                 MOVE CM-AH-CRITICAL-PERIOD
                                       TO OUT-CRIT-PER
                 MOVE CM-AH-LOAN-EXPIRE-DT
                                       TO DC-BIN-DATE-1
                 MOVE ' '              TO DC-OPTION-CODE             
                 PERFORM 9700-DATE-LINK
                                       THRU 9700-EXIT
                 IF NO-CONVERSION-ERROR
                    STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                       DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                       INTO OUT-EXP-DT
                    END-STRING
                 ELSE
                    MOVE SPACES        TO OUT-EXP-DT
                 END-IF
              END-IF
              MOVE CM-ACCOUNT          TO OUT-CERTACCT
              IF CL-INSURED-1ST-NAME = CM-INSURED-FIRST-NAME
                  MOVE CM-JT-FIRST-NAME TO OUT-JFNAME
                  MOVE CM-JT-LAST-NAME  TO OUT-JLNAME
              ELSE
                  MOVE CM-INSURED-FIRST-NAME TO OUT-JFNAME
                  MOVE CM-INSURED-LAST-NAME TO OUT-JLNAME
              END-IF


              MOVE CM-LOAN-APR         TO OUT-APR
           END-IF

           .
       1100-EXIT.
           EXIT.

       1200-GET-ERACCT.

          MOVE LOW-VALUES              TO WS-ERACCT-KEY
          MOVE WS-ELCERT-KEY (1:22)    TO WS-ERACCT-KEY (1:22)

           EXEC CICS STARTBR                                            
               DATASET   ('ERACCT')
               RIDFLD    (WS-ERACCT-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              EXEC CICS READNEXT
                 INTO    (ACCOUNT-MASTER)
                 DATASET ('ERACCT')
                 RIDFLD  (WS-ERACCT-KEY)
                 RESP    (WS-RESPONSE)
              END-EXEC
           END-IF

           IF RESP-NORMAL
              PERFORM WITH TEST AFTER UNTIL
                 NOT (RESP-NORMAL)
                 OR (CM-CONTROL-PRIMARY (1:20) NOT =
                              AM-CONTROL-PRIMARY (1:20))
                 IF (CM-CONTROL-PRIMARY (1:20)
                    = AM-CONTROL-PRIMARY (1:20))
                    IF (CL-CERT-EFF-DT >= AM-EFFECTIVE-DT)
                       AND (CM-CERT-EFF-DT < AM-EXPIRATION-DT)
                       MOVE AM-NAME    TO OUT-ORIG-NAME
031116                 MOVE ACCOUNT-MASTER TO SAVE-ACCOUNT-MASTER
                    END-IF
                    MOVE AM-NAME       TO OUT-ACCT-NAME
                    MOVE AM-ADDRS      TO OUT-ACCT-ADDR1
                    MOVE SPACES        TO OUT-ACCT-ADDR2
                    MOVE AM-ADDR-CITY  TO OUT-ACCT-CITY
                    MOVE AM-ADDR-STATE TO OUT-ACCT-STATE
                    MOVE AM-ZIP-PRIME  TO OUT-ACCT-ZIP
                    IF AM-ZIP-PLUS4 NOT = SPACES AND ZEROS
                       STRING '-' AM-ZIP-PLUS4 DELIMITED BY SIZE
                          INTO OUT-ACCT-ZIP (6:5)
                       END-STRING
                    END-IF
                    MOVE AM-TEL-NO     TO OUT-ACCT-PHONE
020613              MOVE AM-GPCD       TO OUT-ACCT-GPCD
                    EXEC CICS READNEXT
                       INTO    (ACCOUNT-MASTER)
                       DATASET ('ERACCT')
                       RIDFLD  (WS-ERACCT-KEY)
                       RESP    (WS-RESPONSE)
                    END-EXEC
                 END-IF
              END-PERFORM
           END-IF

031116     MOVE SAVE-ACCOUNT-MASTER  TO ACCOUNT-MASTER

          .
       1200-EXIT.
           EXIT.

       1250-GET-ELLETR.

           MOVE CL-COMPANY-CD          TO WS-ELLETR-COMPANY-CD
           MOVE BL-LETTER-ID           TO WS-ELLETR-LETTER-ID
           MOVE +0                     TO WS-ELLETR-SEQ-NO

           EXEC CICS READ                                               
                DATASET    ('ELLETR') 
                INTO       (TEXT-FILES)
                RIDFLD     (WS-ELLETR-KEY)
                RESP       (WS-RESPONSE)
                GTEQ
           END-EXEC

           IF RESP-NORMAL
              IF (LETTER-FILE-TEXT)
                 AND (BL-LETTER-ID = TX-LETTER-NO)
                 AND (TX-LINE-SQUEEZE-CONTROL = 'Z')
                 PERFORM 1280-PROCESS-Z-CONTROLS
                                       THRU 1280-EXIT
              END-IF
           END-IF

           .
       1250-EXIT.
           EXIT.

       1280-PROCESS-Z-CONTROLS.

           MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA

           IF W-FORM-TO-RESEND > SPACES
              MOVE W-FORM-TO-RESEND    TO OUT-FORM
           ELSE
              MOVE SPACES              TO OUT-FORM
           END-IF.
           
      *    MOVE W-ENCLOSURE-CD         TO OUT-ENC-CODE

           .
       1280-EXIT.
           EXIT.


       1300-GET-ELTRLRS.

           MOVE CL-CONTROL-PRIMARY     TO WS-ELTRLR-KEY
           MOVE +0                     TO WS-ELTRLR-SEQ-NO

           EXEC CICS STARTBR                                            
               DATASET   ('ELTRLR')
               RIDFLD    (WS-ELTRLR-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

          IF RESP-NORMAL
             EXEC CICS READNEXT
                INTO    (ACTIVITY-TRAILERS)
                DATASET ('ELTRLR')
                RIDFLD  (WS-ELTRLR-KEY)
                RESP    (WS-RESPONSE)
             END-EXEC
             PERFORM 1310-GET-ELTRLRS THRU 1310-EXIT
          END-IF

           .
       1300-EXIT.
           EXIT.

       1310-GET-ELTRLRS.

           IF RESP-NORMAL

           IF CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20)
              EVALUATE TRUE
                 WHEN AT-INSURED-ADDR-TRL
                    MOVE AT-MAIL-TO-NAME   TO OUT-INS-NAME
                    MOVE AT-ADDRESS-LINE-1 TO OUT-INS-ADDR1
                    MOVE AT-ADDRESS-LINE-2 TO OUT-INS-ADDR2
                    MOVE AT-CITY           TO OUT-INS-CITY
                    MOVE AT-STATE          TO OUT-INS-STATE
                    MOVE AT-ZIP-CODE       TO OUT-INS-ZIP
                    IF AT-ZIP-PLUS4 NOT = SPACES AND ZEROS
                       STRING '-' AT-ZIP-PLUS4 DELIMITED BY SIZE
                          INTO OUT-INS-ZIP (6:5)
                       END-STRING
                    END-IF
                    MOVE AT-PHONE-NO       TO OUT-INS-PHONE
                 WHEN AT-BENEFICIARY-ADDR-TRL
                    SET FOUND-BENE TO TRUE
                    MOVE AT-MAIL-TO-NAME   TO OUT-BEN-NAME
                    MOVE AT-ADDRESS-LINE-1 TO OUT-BEN-ADDR1
                    MOVE AT-ADDRESS-LINE-2 TO OUT-BEN-ADDR2
                    MOVE AT-CITY           TO OUT-BEN-CITY
                    MOVE AT-STATE          TO OUT-BEN-STATE
                    MOVE AT-ZIP-CODE       TO OUT-BEN-ZIP
                    IF AT-ZIP-PLUS4 NOT = SPACES AND ZEROS
                       STRING '-' AT-ZIP-PLUS4 DELIMITED BY SIZE
                          INTO OUT-BEN-ZIP (6:5)
                       END-STRING
                    END-IF
                    MOVE AT-PHONE-NO       TO OUT-BEN-PHONE
                 WHEN AT-ACCOUNT-ADDR-TRL
                    MOVE AT-MAIL-TO-NAME   TO OUT-ACCT-NAME
                    MOVE AT-ADDRESS-LINE-1 TO OUT-ACCT-ADDR1
                    MOVE AT-ADDRESS-LINE-2 TO OUT-ACCT-ADDR2
                    MOVE AT-CITY           TO OUT-ACCT-CITY
                    MOVE AT-STATE          TO OUT-ACCT-STATE
                    MOVE AT-ZIP-CODE       TO OUT-ACCT-ZIP
                    IF AT-ZIP-PLUS4 NOT = SPACES AND ZEROS
                       STRING '-' AT-ZIP-PLUS4 DELIMITED BY SIZE
                          INTO OUT-ACCT-ZIP (6:5)
                       END-STRING
                    END-IF
                    MOVE AT-PHONE-NO       TO OUT-ACCT-PHONE
                 WHEN AT-SEQUENCE-NO = +90
                    MOVE AT-INFO-LINE-1   TO OUT-DIAG
                 WHEN AT-SEQUENCE-NO = +91
                    MOVE AT-INFO-LINE-1 TO OUT-LOAN-NO
                 WHEN AT-TRAILER-TYPE = '2' AND AT-PAYMENT-TYPE = 'I'
                    IF AT-INT-RATE NUMERIC AND
                       AT-INT-RATE NOT EQUAL ZEROS
                          COMPUTE OUT-LF-INT-RATE = AT-INT-RATE * 100
                    END-IF

                 WHEN AT-TRAILER-TYPE = '3'
                    IF (SAVE-BIN-DATE >= AT-SCHEDULE-START-DT)
                       AND (SAVE-BIN-DATE <= AT-SCHEDULE-END-DT)
                       AND (AT-TERMINATED-DT = LOW-VALUES OR SPACES)
                       MOVE AT-SCHEDULE-END-DT TO WS-AUTO-LAST-SCHED-DT
                    END-IF
                 WHEN AT-TRAILER-TYPE = '8'
                    IF AT-RECORDED-DT > WS-LAST-ACT-DT
                       MOVE AT-RECORDED-DT
                                    TO WS-LAST-ACT-DT
                       MOVE 'DENIAL'
                                    TO WS-LAST-ACT-TYPE
                    END-IF
                 WHEN AT-TRAILER-TYPE = 'A'
                    IF AT-RECORDED-DT > WS-LAST-ACT-DT
                       MOVE AT-RECORDED-DT
                                    TO WS-LAST-ACT-DT
                       MOVE 'FORMS' TO WS-LAST-ACT-TYPE
                       IF INITIAL-FORM
                          MOVE 'INIT'
                                    TO WS-FORM
                       END-IF
                       IF PROGRESS-FORM
                          MOVE 'PROG'
                                    TO WS-FORM
                       END-IF
                    END-IF
                    IF AT-FORM-SEND-ON-DT > WS-ACTIVITY-DT
                       MOVE AT-FORM-SEND-ON-DT
                                    TO WS-ACTIVITY-DT
                    END-IF
                    IF AT-FORM-REPRINT-DT > WS-ACTIVITY-DT
                       MOVE AT-FORM-REPRINT-DT
                                    TO WS-ACTIVITY-DT
                    END-IF
                    IF AT-FORM-ANSWERED-DT > WS-ACTIVITY-DT
                       MOVE AT-FORM-ANSWERED-DT
                                    TO WS-ACTIVITY-DT
                    END-IF
              END-EVALUATE
              EXEC CICS READNEXT
                INTO    (ACTIVITY-TRAILERS)
                DATASET ('ELTRLR')
                RIDFLD  (WS-ELTRLR-KEY)
                RESP    (WS-RESPONSE)
              END-EXEC
              
              GO TO 1310-GET-ELTRLRS
           END-IF
           
           END-IF
           .
       1310-EXIT.
           EXIT.

       1400-GET-WAIT-PER.

           MOVE BL-COMP-ID             TO WS-ELCNTL-COMPANY-ID
           MOVE CM-AH-BENEFIT-CD       TO WS-ELCNTL-BEN-CD
           MOVE '5'                    TO WS-ELCNTL-REC-TYPE
           MOVE +0                     TO WS-ELCNTL-SEQ-NO

           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-ELCNTL-KEY)
              GTEQ
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              AND (CF-COMPANY-ID  = BL-COMP-ID)
              AND (CF-RECORD-TYPE = '5')
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (S1 > +8)
                 OR (CM-AH-BENEFIT-CD = CF-BENEFIT-CODE (S1))
              END-PERFORM
              IF S1 > +8
                 MOVE '**'             TO OUT-WAIT-PER
              ELSE
                 MOVE CF-BENEFIT-ALPHA (S1) (1:2)
                                       TO OUT-WAIT-PER
              END-IF
           ELSE
              MOVE '**'                TO OUT-WAIT-PER
           END-IF

          .
       1400-EXIT.
           EXIT.

       1500-GET-ARCH-NO.

      *    DISPLAY ' MADE IT TO 1500 '
           MOVE BL-COMP-ID             TO WS-ELCNTL-COMPANY-ID
           MOVE '1'                    TO WS-ELCNTL-REC-TYPE
           MOVE SPACES                 TO WS-ELCNTL-GENL
           MOVE +0                     TO WS-ELCNTL-SEQ-NO

           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-ELCNTL-KEY)
              UPDATE
              RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              AND (CF-COMPANY-ID  = BL-COMP-ID)
              AND (CF-RECORD-TYPE = '1')
              ADD +1                   TO CF-CO-ARCHIVE-COUNTER
              MOVE CF-CO-ARCHIVE-COUNTER TO WS-ARCHIVE-NO
              MOVE WS-ARCHIVE-NO       TO BL-ARCHIVE-NO
              EXEC CICS REWRITE
                 FROM    (CONTROL-FILE)
                 DATASET ('ELCNTL')
              END-EXEC
           ELSE
              MOVE +0                  TO WS-ARCHIVE-NO
           END-IF

          .
       1500-EXIT.
           EXIT.

       1600-ADD-CORR-TRLR.

           EXEC CICS READ
                DATASET    ('ELMSTR')
                INTO       (CLAIM-MASTER)
                RIDFLD     (WS-ELMSTR-KEY)
                UPDATE
                RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              DISPLAY ' 1600 READ UPD MSTR ' WS-RESPONSE
              GO TO 1600-EXIT
           END-IF

           SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT

           PERFORM 2000-BUILD-ELNAPS   THRU 2000-EXIT

           IF WS-FOLLOW-UP-DT > CL-NEXT-FOLLOWUP-DT
              MOVE WS-FOLLOW-UP-DT     TO CL-NEXT-FOLLOWUP-DT
           END-IF
                                                                        
           IF WS-RESEND-DT > CL-NEXT-FOLLOWUP-DT
              MOVE WS-RESEND-DT        TO CL-NEXT-FOLLOWUP-DT
           END-IF
           MOVE '2'                    TO CL-LAST-MAINT-TYPE

           MOVE 'AT'                   TO ACTIVITY-TRAILERS
           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO                
           MOVE  4                     TO AT-TRAILER-TYPE
           MOVE SAVE-BIN-DATE          TO AT-RECORDED-DT                
                                          CL-LAST-MAINT-DT              
                                          AT-CORR-LAST-MAINT-DT         
           MOVE BL-PROC-ID             TO AT-RECORDED-BY                
                                          CL-LAST-MAINT-USER            
                                          AT-CORR-LAST-UPDATED-BY       
           MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS          
                                          CL-LAST-MAINT-HHMMSS.         
           MOVE SAVE-BIN-DATE          TO AT-LETTER-SENT-DT.            
           MOVE WS-FOLLOW-UP-DT        TO AT-RECEIPT-FOLLOW-UP.         
           MOVE WS-RESEND-DT           TO AT-AUTO-RE-SEND-DT.           
           MOVE LOW-VALUES             TO AT-LETTER-ANSWERED-DT         
                                          AT-LETTER-PURGED-DT.          
           MOVE WS-ARCHIVE-NO          TO AT-LETTER-ARCHIVE-NO.
           MOVE '3'                    TO AT-LETTER-ORIGIN
                                                                        
           MOVE BL-LETTER-ID           TO AT-STD-LETTER-FORM

           MOVE SAVE-BIN-DATE          TO AT-INITIAL-PRINT-DATE         

           MOVE LOW-VALUES             TO AT-RESEND-PRINT-DATE

           EXEC CICS WRITE                                              
                DATASET    ('ELTRLR')
                FROM       (ACTIVITY-TRAILERS)                          
                RIDFLD     (AT-CONTROL-PRIMARY)
                RESP       (WS-RESPONSE)
           END-EXEC
                                                                        
           IF NOT RESP-NORMAL
              DISPLAY ' 1600 WRITE TRLR ' WS-RESPONSE
              GO TO 1600-EXIT
           END-IF
       
           EXEC CICS REWRITE                                            
                DATASET    ('ELMSTR')
                FROM       (CLAIM-MASTER)                               
                RESP       (WS-RESPONSE)
           END-EXEC
       
           IF NOT RESP-NORMAL
              DISPLAY ' 1600 REWRITE MSTR ' WS-RESPONSE
           END-IF

           .
       1600-EXIT.
           EXIT.

       1700-GET-ELENCC.

           MOVE CL-COMPANY-CD          TO WS-ELENCC-KEY
           MOVE '1'                    TO WS-ELENCC-REC-TYPE
           MOVE BL-ENC-CD              TO WS-ELENCC-ENC-CODE
           PERFORM VARYING E1 FROM +1 BY +1 UNTIL
              (WS-ELENCC-ENC-CODE (E1:1) = ' ')
              OR (E1 > +5)
           END-PERFORM

           IF E1 < +5
              MOVE CL-CERT-STATE       TO WS-ELENCC-ENC-CODE (E1:2)
           END-IF

           EXEC CICS READ
               DATASET   ('ELENCC')
               INTO      (ENCLOSURE-CODES)
               RIDFLD    (WS-ELENCC-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE BL-ENC-CD           TO WS-ELENCC-ENC-CODE
              EXEC CICS READ
                  DATASET   ('ELENCC')
                  INTO      (ENCLOSURE-CODES)
                  RIDFLD    (WS-ELENCC-KEY)
                  RESP      (WS-RESPONSE)
              END-EXEC
           END-IF
          
           IF RESP-NORMAL
              MOVE NC-OUTPUT-STACK     TO OUT-OUTSTACK
              MOVE NC-ENCLOSURE-LINE   TO OUT-ENCLINE
              MOVE NC-ATTACHMENTS      TO OUT-ENCATTS
           END-IF

           .
       1700-EXIT.
           EXIT.
031116 1800-SET-EL150D-FIELDS.
031116     MOVE SPACES TO WS-UNSORTED-TABLE
031116                    WS-SORTED-TABLE
031116
031116     MOVE ' '                    TO WS-PDEF-RECORD-SW
031116
031116     MOVE CM-CONTROL-PRIMARY     TO ELCRTT-KEY
031116     MOVE 'B'                    TO CRTT-REC-TYPE
031116
031116     EXEC CICS READ
031116        DATASET   ('ELCRTT')
031116        INTO      (CERTIFICATE-TRAILERS)
031116        RIDFLD    (ELCRTT-KEY)
031116        RESP      (WS-RESPONSE)
031116     END-EXEC
031116
031116     IF NOT RESP-NORMAL
031116        DISPLAY ' NSRLTRBL- NO TRLRS ' CL-CERT-NO
031116        GO TO 1800-EXIT
031116     END-IF
031116
031116     DISPLAY ' NSRLTRBL- GOOD READ ON ELCRTT '
031116     PERFORM VARYING S1 FROM +1 BY +1 UNTIL
031116       (S1 > +24)
031116       OR (CS-CLAIM-NO (S1) = SPACES)
031116        MOVE CS-CLAIM-NO (S1)    TO WS-CLAIM-NO (S1)
031116        MOVE CS-CLAIM-TYPE (S1)  TO WS-CLM-TYPE (S1)
031116        IF CS-INSURED-TYPE (S1) = 'C'
031116           MOVE '2'              TO WS-INS-TYPE (S1)
031116        ELSE
031116           MOVE '1'              TO WS-INS-TYPE (S1)
031116        END-IF
031116        MOVE CS-BENEFIT-PERIOD (S1)
031116                                 TO WS-BEN-PER (S1)
031116*       MOVE CS-DAYS-PAID (S1)   TO WS-DAYS-PAID (S1)
031116        MOVE CS-TOTAL-PAID (S1)  TO WS-TOTAL-PAID (S1)
031116        MOVE ' '                 TO WS-SORTED-SW (S1)
031116
031116        MOVE CS-COMPANY-CD          TO WS-ELMSTR-COMPANY-CD
031116        MOVE CS-CARRIER             TO WS-ELMSTR-CARRIER
031116        MOVE CS-CLAIM-NO (S1)       TO WS-ELMSTR-CLAIM-NO
031116        MOVE CS-CERT-NO             TO WS-ELMSTR-CERT-NO
031116
031116        EXEC CICS READ
031116           DATASET        ('ELMSTR')
031116           RIDFLD         (WS-ELMSTR-KEY)
031116           INTO           (CLAIM-MASTER)
031116           RESP           (WS-RESPONSE)
031116        END-EXEC
031116        IF RESP-NORMAL
031116           MOVE CL-INCURRED-DT   TO WS-INC-DT (S1)
031116           MOVE CL-PAID-THRU-DT  TO WS-PD-THRU-DT (S1)
031116           MOVE CL-CLAIM-STATUS  TO WS-STATUS (S1)
031116           IF CL-CRITICAL-PERIOD NOT NUMERIC
031116              MOVE ZEROS TO CL-CRITICAL-PERIOD
031116           END-IF
031116           MOVE CL-CRITICAL-PERIOD
031116                                 TO WS-MAX-BENS (S1)
031116           IF CL-CRITICAL-PERIOD = ZEROS
100518              IF CL-CLAIM-TYPE = 'L' OR 'P' OR 'O'
031116                 MOVE 01         TO WS-MAX-BENS (S1)
031116              ELSE
031116                 MOVE CM-AH-ORIG-TERM
031116                                 TO WS-MAX-BENS (S1)
031116              END-IF
031116           END-IF
031116           IF CL-DENIAL-TYPE = '1' OR '2' OR '3' OR '4'
031116              MOVE 'D'           TO WS-STATUS (S1)
031116           END-IF
031116           IF CS-CLAIM-NO (S1) = BL-CLAIM-NO
031116              MOVE CL-ACCIDENT-CLAIM-SW TO OUT-ACC-SW
031116           END-IF
031116
031116           MOVE ' '                    TO WS-PDEF-RECORD-SW
031116           IF (AM-DCC-PRODUCT-CODE NOT = SPACES)
031116              PERFORM 1850-GET-DDF-LIMITS
031116                                 THRU 1850-EXIT
031116           END-IF
031116           IF PDEF-FOUND
031116              PERFORM VARYING P1 FROM +1 BY +1 UNTIL
031116                 (PD-PROD-CODE (P1) = CL-CLAIM-TYPE)
031116                 OR (P1 > +8)
031116              END-PERFORM
031116              IF P1 < +9
100518                 IF CL-CLAIM-TYPE = 'L' OR 'P' OR 'O'
031116                    IF (PD-MAX-AMT (P1) NOT = ZEROS)
031116                       AND (PD-MAX-AMT (P1) < CM-LF-BENEFIT-AMT)
031116                       MOVE PD-MAX-AMT (P1)
031116                                 TO WS-MAX-MOBEN (S1)
031116                    ELSE
031116                       MOVE CM-LF-BENEFIT-AMT
031116                                 TO WS-MAX-MOBEN (S1)
031116                    END-IF
031116                 ELSE
031116                    IF PD-BEN-PCT (P1) NOT NUMERIC
031116                       MOVE ZEROS   TO PD-BEN-PCT (P1)
031116                    END-IF
031116                    IF PD-BEN-PCT (P1) = ZEROS
031116                       MOVE +1      TO WS-WORK-BEN-PCT
031116                    ELSE
031116                       MOVE PD-BEN-PCT (P1)
031116                                    TO WS-WORK-BEN-PCT
031116                    END-IF
031116                    COMPUTE WS-MAX-MOBEN (S1) =
031116                       CM-AH-BENEFIT-AMT * WS-WORK-BEN-PCT
031116                    IF (PD-MAX-AMT (P1) NOT = ZEROS)
031116                       AND (PD-MAX-AMT (P1) < WS-MAX-MOBEN (S1))
031116                       MOVE PD-MAX-AMT (P1)
031116                                 TO WS-MAX-MOBEN (S1)
031116                    END-IF
031116                 END-IF
031116*                IF (PD-MAX-AMT (P1) NOT = ZEROS)
031116*                   AND (PD-MAX-AMT (P1) < CM-AH-BENEFIT-AMT)
031116*                   MOVE PD-MAX-AMT (P1) TO WS-MAX-MOBEN (S1)
031116*                ELSE
031116*                   MOVE CM-AH-BENEFIT-AMT TO WS-MAX-MOBEN (S1)
031116*                END-IF
031116                 MOVE PD-EXCLUSION-PERIOD-DAYS (P1)
031116                                 TO WS-EXCL-PER (S1)
031116                 MOVE PD-COVERAGE-ENDS-MOS (P1)
031116                                 TO WS-COV-ENDS (S1)
031116                 MOVE PD-ACCIDENT-ONLY-MOS (P1)
031116                                 TO WS-ACC-PER (S1)
031116                 MOVE PD-MAX-EXTENSION (P1)
031116                                 TO WS-MAX-EXTEN (S1)
031116                 EVALUATE TRUE
031116                    WHEN PD-RECURRING-YN (P1) = 'N'
031116                       MOVE 00   TO WS-REC-MOS (S1)
031116                    WHEN PD-RECURRING-YN (P1) = 'Y'
031116                       MOVE 99   TO WS-REC-MOS (S1)
031116                    WHEN PD-REC-CRIT-PERIOD (P1) NUMERIC
031116                       MOVE PD-REC-CRIT-PERIOD (P1)
031116                                 TO WS-REC-MOS (S1)
031116                    WHEN OTHER
031116                       MOVE ZEROS TO WS-REC-MOS (S1)
031116                 END-EVALUATE
031116                 IF WS-REC-MOS (S1) = ZEROS
031116                    MOVE 01      TO WS-REC-MOS (S1)
031116                 END-IF
031116                 DISPLAY ' NSRLTRBL- PDEF FOUND ' WS-EXCL-PER (S1)
031116              END-IF
031116           ELSE
100518              IF CL-CLAIM-TYPE NOT = 'L' AND 'P' AND 'O'
031116                 MOVE CM-AH-BENEFIT-AMT
031116                                 TO WS-MAX-MOBEN (S1)
031116              ELSE
031116                 MOVE ZEROS      TO WS-MAX-MOBEN (S1)
031116              END-IF
031116              MOVE ZEROS         TO WS-EXCL-PER (S1)
031116                                    WS-MAX-EXTEN (S1)
031116                                    WS-ACC-PER  (S1)
031116              MOVE 999           TO WS-COV-ENDS (S1)
031116              MOVE 01            TO WS-REC-MOS  (S1)
031116           END-IF
031116        END-IF
031116     END-PERFORM
031116
031116*      SORT THE TABLE
031116     MOVE HIGH-VALUES            TO WS-KEY (S1)
031116     COMPUTE WS-MAX-SUB = S1 - +1
031116     DISPLAY ' NSRLTRBL- JUST BUILT TABLE '   WS-MAX-SUB
031116
031116     MOVE WS-MAX-SUB             TO WS-CNTR
031116
031116     MOVE +1                     TO S2
031116     MOVE +0                     TO S3
031116     PERFORM UNTIL (WS-CNTR = ZERO) OR (S3 > 700)
031116        MOVE +0 TO WS-HOLD-S1
031116        MOVE HIGH-VALUES TO WS-HOLD-KEY
031116        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
031116           (S1 > WS-MAX-SUB)
031116           IF (WS-SORTED-SW (S1) NOT = 'Y')
031116              AND (WS-KEY (S1) <= WS-HOLD-KEY)
031116              MOVE WS-KEY (S1)   TO WS-HOLD-KEY
031116              MOVE S1            TO WS-HOLD-S1
031116           END-IF
031116        END-PERFORM
031116        IF WS-HOLD-S1 NOT = ZEROS
031116           MOVE WS-HOLD-S1         TO S1
031116           MOVE WS-KEY        (S1) TO WS-SRTD-KEY        (S2)
031116           MOVE WS-STATUS     (S1) TO WS-SRTD-STATUS     (S2)
031116           MOVE WS-MAX-BENS   (S1) TO WS-SRTD-MAX-BENS   (S2)
031116           MOVE WS-EXCL-PER   (S1) TO WS-SRTD-EXCL-PER   (S2)
031116           MOVE WS-COV-ENDS   (S1) TO WS-SRTD-COV-ENDS   (S2)
031116           MOVE WS-ACC-PER    (S1) TO WS-SRTD-ACC-PER    (S2)
031116           MOVE WS-REC-MOS    (S1) TO WS-SRTD-REC-MOS    (S2)
031116           MOVE WS-MAX-EXTEN  (S1) TO WS-SRTD-MAX-EXTEN  (S2)
031116           MOVE WS-PD-THRU-DT (S1) TO WS-SRTD-PD-THRU-DT (S2)
031116           MOVE WS-CLAIM-NO   (S1) TO WS-SRTD-CLAIM-NO   (S2)
031116           MOVE WS-MAX-MOBEN  (S1) TO WS-SRTD-MAX-MOBEN  (S2)
031116           MOVE WS-TOTAL-PAID (S1) TO WS-SRTD-TOTAL-PAID (S2)
031116*          MOVE WS-REM-BENS   (S1) TO WS-SRTD-REM-BENS   (S2)
031116           MOVE 'Y'                TO WS-SORTED-SW       (S1)
031116           SUBTRACT 1 FROM WS-CNTR
031116           ADD +1 TO S2
031116        END-IF
031116        ADD 1 TO S3
031116     END-PERFORM
031116
031116     DISPLAY ' NSRLTRBL- JUST SORTED TABLE '
031116
031116***  -----------------------
031116     IF BL-COMP-ID NOT = 'DCC' AND 'VPP'
031116        PERFORM 1900-BUILD-NON-DCC
031116        GO TO 1800-EXIT
031116     END-IF
031116
031116     MOVE SPACES                 TO WS-PREV-CLM-TYPE
031116                                    WS-PREV-INS-TYPE
031116     MOVE ZEROS                  TO WS-PREV-BEN-PER
031116                                    WS-ACCUM-DAYS
031116                                    WS-ACCUM-AMT
031116                                    WS-ACCUM-PD-BENS
031116
031116     MOVE +1                     TO M1
031116     PERFORM VARYING S1 FROM +1 BY +1 UNTIL
031116        S1 > WS-MAX-SUB
031116        IF (WS-SRTD-CLM-TYPE (S1) = WS-PREV-CLM-TYPE)
031116           AND (WS-SRTD-BEN-PER (S1) = WS-PREV-BEN-PER)
031116           AND (WS-SRTD-INS-TYPE (S1) NOT = WS-PREV-INS-TYPE)
031116           MOVE ZEROS            TO WS-ACCUM-DAYS
031116                                    WS-ACCUM-AMT
031116                                    WS-ACCUM-PD-BENS
031116        END-IF
031116        MOVE WS-SRTD-INS-TYPE (S1) TO WS-PREV-INS-TYPE
031116        IF (WS-SRTD-CLM-TYPE (S1) = WS-PREV-CLM-TYPE)
031116           AND (WS-SRTD-BEN-PER (S1) NOT = WS-PREV-BEN-PER)
031116           MOVE ZEROS            TO WS-ACCUM-DAYS
031116                                    WS-ACCUM-AMT
031116                                    WS-ACCUM-PD-BENS
031116        END-IF
031116        MOVE WS-SRTD-BEN-PER (S1) TO WS-PREV-BEN-PER
031116        IF WS-SRTD-CLM-TYPE (S1) NOT = WS-PREV-CLM-TYPE
031116           PERFORM 1810-SET-NEW-HEAD
031116                                 THRU 1810-EXIT
031116           MOVE WS-SRTD-CLM-TYPE (S1)
031116                                 TO WS-PREV-CLM-TYPE
031116           ADD +1                TO M1
031116        END-IF
031116
031116        MOVE WS-SRTD-BEN-PER (S1)  TO OUT-BEN-PER
031116        IF WS-SRTD-INS-TYPE (S1) = '1'
031116           MOVE 'PRIM'             TO OUT-INS-TYP
031116        ELSE
031116           MOVE 'COBO'             TO OUT-INS-TYP
031116        END-IF
031116*       EVALUATE WS-SRTD-STATUS (S1)
031116*          WHEN 'C'
031116*             MOVE 'CLOSED'        TO WSM-STATUS (M1)
031116*          WHEN 'D'
031116*             MOVE 'DENIED'        TO WSM-STATUS (M1)
031116*          WHEN 'O'
031116*             MOVE 'OPEN'          TO WSM-STATUS (M1)
031116*          WHEN OTHER
031116*             MOVE 'OTHER'         TO WSM-STATUS (M1)
031116*       END-EVALUATE
031116*       MOVE WS-SRTD-CLAIM-NO (S1) TO WSM-CLAIM-NO (M1)
031116*                                     PI-WSM-CLAIM-NOS (S1)
031116*       MOVE WS-SRTD-TOTAL-PAID (S1) TO WSM-TOTAL-PAID (M1)
031116
031116        COMPUTE OUT-MAX-BENS =
031116           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
031116
031116        DISPLAY ' NSRLTRBL- MAX BENS 1 ' OUT-MAX-BENS ' '
031116           WS-SRTD-MAX-BENS (S1) ' ' WS-ACCUM-PD-BENS
031116
031116        MOVE ZEROS TO WK1 WK2
031116        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
031116           COMPUTE WS-PD-BENS ROUNDED =
031116              WS-SRTD-TOTAL-PAID (S1) / WS-SRTD-MAX-MOBEN (S1)
031116           IF WS-SRTD-CLM-TYPE (S1) NOT = 'L' AND 'P'
031116              DIVIDE WS-SRTD-TOTAL-PAID (S1) BY
031116                 WS-SRTD-MAX-MOBEN(S1) GIVING WK1
031116                 REMAINDER WK2
031116           END-IF
031116        ELSE
031116           MOVE ZEROS            TO WS-PD-BENS
031116        END-IF
031116        IF (WS-PD-BENS = ZEROS)
031116           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
031116           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
031116           MOVE 1                TO WS-PD-BENS
031116        END-IF
031116        MOVE WS-PD-BENS          TO OUT-PAID-BENS
031116*       if wk2 not = zeros
031116*          move '*'              to wsm-part (m1)
031116*       end-if
031116*       compute ws-accum-days =
031116*          ws-accum-days + ws-srtd-days-paid (s1)
031116        COMPUTE WS-ACCUM-AMT =
031116           WS-ACCUM-AMT + WS-SRTD-TOTAL-PAID (S1)
031116        DISPLAY ' NSRLTRBL- COMPUTE A PD BENS ' M1 ' '
031116           WS-ACCUM-AMT ' ' WS-SRTD-MAX-MOBEN (S1)
031116        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
031116           COMPUTE WS-ACCUM-PD-BENS ROUNDED =
031116              WS-ACCUM-AMT / WS-SRTD-MAX-MOBEN (S1)
031116        ELSE
031116           MOVE ZEROS            TO WS-ACCUM-PD-BENS
031116        END-IF
031116        IF (WS-ACCUM-PD-BENS = ZEROS)
031116           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
031116           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
031116           MOVE 1                TO WS-ACCUM-PD-BENS
031116        END-IF
031116        DISPLAY ' NSRLTRBL- COMPUTE REM BENS ' M1 ' '
031116           WS-SRTD-MAX-BENS (S1) ' ' WS-ACCUM-PD-BENS
031116        COMPUTE OUT-REM-BENS =
031116           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
031116        PERFORM 1820-FIND-QUALIFY THRU 1820-EXIT
031116
031116*       MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-1
031116*       MOVE ' '                    TO DC-OPTION-CODE
031116*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
031116*       IF NO-CONVERSION-ERROR
031116*          MOVE DC-GREG-DATE-A-EDIT TO wsm-inc-date (m1)
031116*       ELSE
031116*          MOVE SPACES              TO wsm-inc-date (m1)
031116*       END-IF
031116*       MOVE ws-srtd-pd-thru-dt (s1) TO DC-BIN-DATE-1
031116*       MOVE ' '                     TO DC-OPTION-CODE
031116*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
031116*       IF NO-CONVERSION-ERROR
031116*          MOVE DC-GREG-DATE-A-EDIT TO wsm-pd-thru-dt (m1)
031116*       ELSE
031116*          MOVE SPACES              TO wsm-pd-thru-dt (m1)
031116*       END-IF
031116        ADD +1                   TO M1
031116        IF WS-SRTD-CLAIM-NO (S1) = BL-CLAIM-NO
031116           MOVE WS-SRTD-EXCL-PER (S1) TO OUT-EXCL-PER
031116           MOVE 26 TO S1
031116        END-IF
031116
031116     END-PERFORM
031116
031116*   DISPLAY ' NSRLTRBL- 1800 end MAX BENS '  OUT-MAX-BENS
031116*   display  ' PAID BENS ' OUT-PAID-BENS ' REM BENS ' OUT-REM-BENS
031116*   display  ' EXCL PER ' OUT-EXCL-PER ' REC MOS ' OUT-REC-MOS
031116*   display  ' INS TYP ' OUT-INS-TYP ' BEN PER ' OUT-BEN-PER
031116*   display  ' ACC SW ' OUT-ACC-SW
031116
031116     .
031116 1800-EXIT.
031116     EXIT.
031116
031116     EJECT
031116
031116 1810-SET-NEW-HEAD.
031116     MOVE ZEROS                  TO WS-ACCUM-DAYS
031116                                    WS-ACCUM-AMT
031116                                    WS-ACCUM-PD-BENS
031116     EVALUATE WS-SRTD-CLM-TYPE (S1)
031116        WHEN 'A'
031116           MOVE 'A&H'            TO WS-COV-TYPE
031116        WHEN 'F'
031116           MOVE 'FAM '           TO WS-COV-TYPE
031116        WHEN 'I'
031116           MOVE ' IU '           TO WS-COV-TYPE
031116        WHEN 'L'
031116           MOVE 'LIFE'           TO WS-COV-TYPE
031116        WHEN OTHER
031116           MOVE WS-SRTD-CLM-TYPE (S1)
031116                                 TO WS-COV-TYPE
031116     END-EVALUATE
031116
031116     MOVE WS-SRTD-REC-MOS (S1) TO OUT-REC-MOS
031116     MOVE WS-SRTD-EXCL-PER (S1) TO OUT-EXCL-PER
031116
031116*    string '  ' ws-cov-type ' ExPer ' OUT-EXCL-PER
031116*       ' CovEnd ' ws-srtd-cov-ends (s1)
031116*       ' MaxBens ' ws-srtd-max-bens (s1)
031116*       ' Recurring ' OUT-REC-MOS
031116*       delimited by size into ws-map-output (m1)
031116*    end-string
031116
031116     .
031116 1810-EXIT.
031116     EXIT.
031116
031116 1820-FIND-QUALIFY.
031116     IF NOT PDEF-FOUND
031116        GO TO 1820-EXIT
031116     END-IF
031116
031116*    DISPLAY ' NSRLTRBL- MAX BENS 2 ' OUT-MAX-BENS ' '
031116*       WS-SRTD-MAX-BENS (S1) ' ' WS-ACCUM-PD-BENS
031116
031116     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
031116     MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-2
031116     MOVE '1'                    TO DC-OPTION-CODE
031116     MOVE +0                     TO DC-ELAPSED-MONTHS
031116                                    DC-ELAPSED-DAYS
031116     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
031116
031116     IF NO-CONVERSION-ERROR
031116        DISPLAY ' NSRLTRBL- GOOD DATE CONVERT '
031116        MOVE DC-ELAPSED-MONTHS   TO WS-MONTHS-BETWEEN
031116        IF DC-ELAPSED-DAYS > 1
031116           ADD 1 TO WS-MONTHS-BETWEEN
031116        END-IF
031116     ELSE
031116        MOVE ZEROS               TO WS-MONTHS-BETWEEN
031116     END-IF
031116
031116*    DISPLAY ' NSRLTRBL- DISPLAY 3 ' OUT-EXCL-PER
031116*       ' ' WS-SRTD-COV-ENDS (S1) ' ' WS-MONTHS-BETWEEN
031116
031116     EVALUATE TRUE
031116        WHEN (OUT-EXCL-PER NOT = ZEROS)
031116           AND (WS-MONTHS-BETWEEN <= OUT-EXCL-PER)
031116           DISPLAY ' NSRLTRBL- ZERO 1 '
031116           MOVE ZEROS TO OUT-MAX-BENS
031116                         OUT-REM-BENS
031116        WHEN (WS-SRTD-COV-ENDS (S1) NOT = ZEROS)
031116           AND (WS-MONTHS-BETWEEN > WS-SRTD-COV-ENDS (S1))
031116           DISPLAY ' NSRLTRBL- ZERO 2 '
031116           MOVE ZEROS TO OUT-MAX-BENS
031116     END-EVALUATE
031116     IF OUT-REC-MOS < WS-SRTD-BEN-PER (S1)
031116        MOVE ZEROS               TO OUT-REM-BENS
031116     END-IF
031116
031116     .
031116 1820-EXIT.
031116     EXIT.
031116
031116 1850-GET-DDF-LIMITS.
031116
031116     IF CM-CLP-STATE = SPACES OR LOW-VALUES OR ZEROS
031116        MOVE CM-STATE            TO CM-CLP-STATE
031116     END-IF
031116
031116     MOVE CL-COMPANY-CD          TO ERPDEF-KEY
031116     MOVE CM-CLP-STATE           TO ERPDEF-STATE
031116     MOVE AM-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
031116
100518     IF (CL-CLAIM-TYPE = 'L' OR 'P' OR 'O')
031116        AND (CM-LF-BENEFIT-CD NOT = '00' AND '  ' AND 'DD')
031116        MOVE 'L'                 TO ERPDEF-BEN-TYPE
031116        MOVE CM-LF-BENEFIT-CD    TO ERPDEF-BEN-CODE
031116     ELSE
031116        MOVE 'A'                 TO ERPDEF-BEN-TYPE
031116        MOVE CM-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
031116     END-IF
031116
031116     MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
031116
031116*    DISPLAY ' NSRLTRBL- MADE 1850 ' ERPDEF-KEY (2:15)
031116     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
031116
031116     EXEC CICS STARTBR
031116         DATASET  ('ERPDEF')
031116         RIDFLD   (ERPDEF-KEY)
031116         GTEQ
031116         RESP     (WS-RESPONSE)
031116     END-EXEC
031116
031116     IF NOT RESP-NORMAL
031116        GO TO 1850-EXIT
031116     END-IF
031116
031116     .
031116 1850-READNEXT.
031116
031116     EXEC CICS READNEXT
031116        DATASET  ('ERPDEF')
031116        INTO     (PRODUCT-MASTER)
031116        RIDFLD   (ERPDEF-KEY)
031116        RESP     (WS-RESPONSE)
031116     END-EXEC
031116
031116     IF NOT RESP-NORMAL
031116        GO TO 1850-ENDBR
031116     END-IF
031116
031116     IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
031116        IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
031116           MOVE 'Y'              TO WS-PDEF-RECORD-SW
031116           DISPLAY ' NSRLTRBL- SETTING PDEF FOUND TO TRUE '
031116        ELSE
031116           GO TO 1850-READNEXT
031116        END-IF
031116     ELSE
031116        GO TO 1850-ENDBR
031116     END-IF
031116
031116     .
031116 1850-ENDBR.
031116
031116     EXEC CICS ENDBR
031116        DATASET  ('ERPDEF')
031116     END-EXEC
031116
031116     .
031116 1850-EXIT.
031116     EXIT.
031116
031116 1900-BUILD-NON-DCC.
031116
031116     MOVE SPACES                 TO WS-PREV-CLM-TYPE
031116                                    WS-PREV-INS-TYPE
031116     MOVE ZEROS                  TO WS-PREV-BEN-PER
031116                                    WS-ACCUM-DAYS
031116                                    WS-ACCUM-AMT
031116                                    WS-ACCUM-PD-BENS
031116
031116     MOVE +1                     TO M1
031116     PERFORM VARYING S1 FROM +1 BY +1 UNTIL
031116        S1 > WS-MAX-SUB
031116        IF WS-SRTD-CLM-TYPE (S1) NOT = WS-PREV-CLM-TYPE
031116           PERFORM 1810-SET-NEW-HEAD
031116                                 THRU 1810-EXIT
031116           MOVE WS-SRTD-CLM-TYPE (S1)
031116                                 TO WS-PREV-CLM-TYPE
031116           ADD +1                TO M1
031116        END-IF
031116        MOVE WS-SRTD-BEN-PER (S1)  TO OUT-BEN-PER
031116        IF WS-SRTD-INS-TYPE (S1) = '1'
031116           MOVE 'PRIM'             TO OUT-INS-TYP
031116        ELSE
031116           MOVE 'COBO'             TO OUT-INS-TYP
031116        END-IF
031116*       evaluate ws-srtd-status (s1)
031116*          when 'C'
031116*             MOVE 'CLOSED'        TO WSM-STATUS (M1)
031116*          WHEN 'D'
031116*             MOVE 'DENIED'        TO WSM-STATUS (M1)
031116*          WHEN 'O'
031116*             MOVE 'OPEN'          TO WSM-STATUS (M1)
031116*          WHEN OTHER
031116*             MOVE 'OTHER'         TO WSM-STATUS (M1)
031116*       END-EVALUATE
031116*       move ws-srtd-claim-no (s1) to wsm-claim-no (m1)
031116*                                     pi-wsm-claim-nos (s1)
031116*       move ws-srtd-total-paid (s1) to wsm-total-paid (m1)
031116
031116        COMPUTE OUT-MAX-BENS =
031116           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
031116
031116        DISPLAY ' NSRLTRBL- MAX BENS 1 ' OUT-MAX-BENS ' '
031116           WS-SRTD-MAX-BENS (S1) ' ' WS-ACCUM-PD-BENS
031116
031116        MOVE ZEROS TO WK1 WK2
031116        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
031116           COMPUTE WS-PD-BENS ROUNDED =
031116              WS-SRTD-TOTAL-PAID (S1) / WS-SRTD-MAX-MOBEN (S1)
031116           IF WS-SRTD-CLM-TYPE (S1) NOT = 'L' AND 'P'
031116              DIVIDE WS-SRTD-TOTAL-PAID (S1) BY
031116                 WS-SRTD-MAX-MOBEN(S1) GIVING WK1
031116                 REMAINDER WK2
031116           END-IF
031116        ELSE
031116           MOVE ZEROS            TO WS-PD-BENS
031116        END-IF
031116        IF (WS-PD-BENS = ZEROS)
031116           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
031116           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
031116           MOVE 1                TO WS-PD-BENS
031116        END-IF
031116        MOVE WS-PD-BENS          TO OUT-PAID-BENS
031116*       IF WK2 NOT = ZEROS
031116*          MOVE '*'              TO WSM-PART (M1)
031116*       END-IF
031116        COMPUTE WS-ACCUM-AMT =
031116           WS-ACCUM-AMT + WS-SRTD-TOTAL-PAID (S1)
031116        DISPLAY ' NSRLTRBL- COMPUTE A PD BENS ' M1 ' '
031116           WS-ACCUM-AMT ' ' WS-SRTD-MAX-MOBEN (S1)
031116        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
031116           COMPUTE WS-ACCUM-PD-BENS ROUNDED =
031116              WS-ACCUM-AMT / WS-SRTD-MAX-MOBEN (S1)
031116        ELSE
031116           MOVE ZEROS            TO WS-ACCUM-PD-BENS
031116        END-IF
031116        IF (WS-ACCUM-PD-BENS = ZEROS)
031116           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
031116           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
031116           MOVE 1                TO WS-ACCUM-PD-BENS
031116        END-IF
031116        DISPLAY ' NSRLTRBL- COMPUTE REM BENS ' M1 ' '
031116           WS-SRTD-MAX-BENS (S1) ' ' WS-ACCUM-PD-BENS
031116        COMPUTE OUT-REM-BENS =
031116           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
031116
031116*       MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-1
031116*       MOVE ' '                    TO DC-OPTION-CODE
031116*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
031116*       IF NO-CONVERSION-ERROR
031116*          MOVE DC-GREG-DATE-A-EDIT TO wsm-inc-date (m1)
031116*       ELSE
031116*          MOVE SPACES              TO wsm-inc-date (m1)
031116*       END-IF
031116*       MOVE ws-srtd-pd-thru-dt (s1) TO DC-BIN-DATE-1
031116*       MOVE ' '                     TO DC-OPTION-CODE
031116*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
031116*       IF NO-CONVERSION-ERROR
031116*          MOVE DC-GREG-DATE-A-EDIT TO wsm-pd-thru-dt (m1)
031116*       ELSE
031116*          MOVE SPACES              TO wsm-pd-thru-dt (m1)
031116*       END-IF
031116*       move WS-MAP-OUTPUT (m1)  to replineo (m1)
031116        IF WS-SRTD-CLAIM-NO (S1) = BL-CLAIM-NO
031116           MOVE WS-SRTD-EXCL-PER (S1) TO OUT-EXCL-PER
031116           MOVE 26 TO S1
031116        END-IF
031116
031116        ADD +1                   TO M1
031116     END-PERFORM
031116*    DISPLAY ' NSRLTRBL- 1900 end MAX BENS '  OUT-MAX-BENS
031116*    display ' PAID BENS ' OUT-PAID-BENS ' REM BENS ' OUT-REM-BENS
031116*    display ' EXCL PER ' OUT-EXCL-PER ' REC MOS ' OUT-REC-MOS
031116*    display ' INS TYP ' OUT-INS-TYP ' BEN PER ' OUT-BEN-PER
031116*    display ' ACC SW ' OUT-ACC-SW

           .
       2000-BUILD-ELNAPS.

           PERFORM 1500-GET-ARCH-NO    THRU 1500-EXIT

           MOVE 'NA'                   TO NAPERSOFT-FILE
           MOVE CL-COMPANY-CD          TO NA-COMPANY-CD
           MOVE BL-CARRIER             TO NA-CARRIER
           MOVE BL-CLAIM-NO            TO NA-CLAIM-NO
           MOVE BL-CERT-NO             TO NA-CERT-NO
           MOVE WS-ARCHIVE-NO          TO NA-ARCHIVE-NO
                                          BL-ARCHIVE-NO
           MOVE BL-LETTER-ID           TO NA-LETTER-ID
           MOVE BL-NO-OF-COPIES        TO NA-NO-OF-COPIES
           MOVE SAVE-BIN-DATE          TO NA-CREATION-DT
                                          NA-INITIAL-PRINT-DT

           MOVE LOW-VALUES             TO NA-FOLLOW-UP-DT
                                          NA-RESEND-DT
                                          NA-RESEND-PRINT-DT
                                          NA-1ST-LTR-PRINT-DT
                                          NA-NEXT-DUE-DT   
                                          NA-AUTOPYDT

           MOVE CL-TRAILER-SEQ-CNT     TO NA-CORR-TRLR-SEQ

           IF BL-FOLLOW-UP-DT NOT = SPACES AND LOW-VALUES
              STRING BL-FOLLOW-UP-DT (7:2) BL-FOLLOW-UP-DT (1:2)
                 BL-FOLLOW-UP-DT (4:2) DELIMITED BY SIZE
                   INTO DC-GREG-DATE-1-YMD-R
              END-STRING
              MOVE '3'                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO NA-FOLLOW-UP-DT
                                          WS-FOLLOW-UP-DT
              ELSE
                 MOVE LOW-VALUES       TO NA-FOLLOW-UP-DT
                                          WS-FOLLOW-UP-DT
              END-IF
           END-IF

           IF BL-RESEND-DT NOT = SPACES AND LOW-VALUES
              STRING BL-RESEND-DT (7:2) BL-RESEND-DT (1:2)
                 BL-RESEND-DT (4:2) DELIMITED BY SIZE
                   INTO DC-GREG-DATE-1-YMD-R
              END-STRING
              MOVE '3'                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO NA-RESEND-DT
                                          WS-RESEND-DT
              ELSE
                 MOVE LOW-VALUES       TO NA-RESEND-DT
                                          WS-RESEND-DT
              END-IF
           END-IF

           PERFORM WITH TEST AFTER UNTIL RESP-NORMAL
              EXEC CICS WRITE
                 DATASET   ('ELNAPS')
                 FROM      (NAPERSOFT-FILE)
                 RIDFLD    (NA-CONTROL-PRIMARY)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 CONTINUE
              ELSE
                 IF RESP-DUPREC OR RESP-DUPKEY
                    ADD +1 TO NA-ARCHIVE-NO
                 ELSE
                    DISPLAY ' BAD WRITE ELNAPS ' WS-RESPONSE
                       ' ' NA-CONTROL-PRIMARY (2:19)
                    SET RESP-NORMAL TO TRUE
                 END-IF
              END-IF
           END-PERFORM

          .
       2000-EXIT.
           EXIT.

       9700-DATE-LINK.                                                  

           EXEC CICS LINK                                               
               PROGRAM   ('ELDATCV')
               COMMAREA  (DATE-CONVERSION-DATA)                         
               LENGTH    (DC-COMM-LENGTH)                               
           END-EXEC.                                                    
                                                                        
                                                                        
       9700-EXIT.                                                       
            EXIT.
