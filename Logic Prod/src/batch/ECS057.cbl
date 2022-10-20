00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS057
00003  PROGRAM-ID.                ECS057.                                  LV004
00009                                                                   ECS057
00010 *AUTHOR.       SUZAN VUKOV.                                       ECS057
00012                                                                   ECS057
00025 *REMARKS.                                                         ECS057
00026 *        CREATE PERIODIC FEE PAYMENT HISTORY REPORT
      * 
032703******************************************************************
032703*                   C H A N G E   L O G
032703*
032703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
032703*-----------------------------------------------------------------
032703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
032703* EFFECTIVE    NUMBER
032703*-----------------------------------------------------------------
032703* 032703   2001061800003   SMVA  NEW PROGRAM
033104* 033104                 SMVA  ADD ME DATE TO HEADER & CHG RPT
033104*                              AMTS TO CASH RECEIVED FROM GROSS
062304* 062304   2004061600004   PEMA  ADD CANCELATION PROCESSING
060308* 060308 IR2008050500002   PEMA  PROCESS GIANTS ONLY
070808* 070808 IR2008070700001   PEMA  ADD CODES TO GIANT LIST
071212* 071212 CR2012020100004   PEMA  ADD 10C BENEFIT CODES TO LIST
011515* 011515 CR2015010900003   PEMA  ADD CLP RPT AND CASH CERTS
070715* 070715 IR2015070600002   PEMA  FIX CARR 7 CLP CALCS
011116* 011116  CR2015082400003  PEMA  ADD CARRIER 9 PROCESSING
080416* 080416  IR2016080200002  PEMA  Separate Date from CLP in parm
032703******************************************************************
00027                                                                   ECS057
00028  ENVIRONMENT DIVISION.                                            ECS057
00029  CONFIGURATION SECTION.                                           ECS057
00030                                                                      CL**3
00031  INPUT-OUTPUT SECTION.                                            ECS057
00032  FILE-CONTROL.                                                    ECS057
00033                                                                   ECS057
00034      SELECT PRT-RPT          ASSIGN TO SYS008.
062304
062304     SELECT SORTFL           ASSIGN TO SORTWK1.
062304
           SELECT OFFLN-CERT-IN    ASSIGN TO SYS010.

00036      SELECT DISK-DATE        ASSIGN TO SYS019. 

00038  EJECT                                                            ECS057
00039  DATA DIVISION.                                                   ECS057
00040  FILE SECTION.                                                    ECS057
00041                                                                   ECS057
00042  FD  PRT-RPT                                                      ECS057
00043                              COPY ELCPRTFD.                       ECS057
00044  EJECT                                                            ECS057
00045  FD  OFFLN-CERT-IN.
                                   COPY ECSCRT01.
00066                                                                   ECS057
00067  FD  DISK-DATE                                                    ECS057
00068                              COPY ELCDTEFD.                       ECS057

       SD  SORTFL.

062304 01  SRT-REC.
062304     12  SRT-STATE               PIC XX.
062304     12  SRT-ACCOUNT             PIC X(10).
062304     12  SRT-ENTRY-DT            PIC 9(11)  COMP-3.
062304     12  SRT-BATCH-NO            PIC X(6).
062304     12  SRT-EFF-DT              PIC 9(11)  COMP-3.
062304     12  SRT-AMOUNT              PIC S9(9)V99 COMP-3.

00073  WORKING-STORAGE SECTION.                                         ECS057
00074  77  FILLER  PIC  X(32) VALUE '********************************'. ECS057
00075  77  FILLER  PIC  X(32) VALUE '     ECS057 WORKING STORAGE     '. ECS057
00076  77  FILLER  PIC  X(32) VALUE '********** VMOD=2.008 **********'.    CL**3
00077                                                                   ECS057
00082  77  WK-YR                   PIC S9(5)   COMP-3  VALUE +0.        ECS057
00083  77  PAGER                   PIC S9(5)   COMP-3  VALUE +1.        ECS057
00088  77  X                       PIC  X.                              ECS057
070715 77  a1                      pic s999    comp-3  value +0.
070715 77  ws-fees-outside-clp     pic s9(7)v99 comp-3 value +0.
070715 77  ws-lf-canc-fees-outside-clp
070715                             pic s9(7)v99 comp-3 value +0.
070715 77  ws-ah-canc-fees-outside-clp
070715                             pic s9(7)v99 comp-3 value +0.

00082  01  WS-COUNTERS-AND-SUBS.
           05  MONTH-SUB            COMP    PIC S9(03) VALUE +0.   
           05  SAVE-1ST-EFF-MONTH           PIC 9(03)  VALUE 0.   
00080      05  COLUMN-SUB           COMP    PIC S9(02) VALUE +0.
           05  SUB                  COMP    PIC S9(03) VALUE +0.   
           05  SUB1                 COMP    PIC S9(03) VALUE +0.   
           05  SUB2                 COMP    PIC S9(03) VALUE +0.   
           05  SUB3                 COMP    PIC S9(03) VALUE +0.   
           05  PROCMO               COMP    PIC S9(03) VALUE +0.   
           05  EFFMO                COMP    PIC S9(03) VALUE +0.   
           05  EFFMOCOL             COMP    PIC S9(03) VALUE +0.   
           05  MOHDR                COMP    PIC S9(03) VALUE +0.   
00081      05  PGM-SUB              COMP    PIC S9(03) VALUE +57.   
00081      05  WS-LINE-COUNT        COMP-3  PIC S9(03) VALUE +56.   
00081      05  WS-LINE-COUNT-MAX    COMP-3  PIC S9(03) VALUE +55.   
00081      05  WS-PAGE              COMP-3  PIC S9(05) VALUE +0.   

00082  01  WS-DATES.
00082      05  WS-HDR-EFF-CCYY              PIC 9(04)  VALUE 0.
           05  WS-HDR-EFF-CCYY-2                       REDEFINES
               WS-HDR-EFF-CCYY.
               10  WS-HDR-EFF-CC            PIC 9(02).
               10  WS-HDR-EFF-YY            PIC 9(02).

00082      05  WS-CR-ENTRY-DATE             PIC 9(11).
           05  WS-CR-ENTRY-DATE-R                      REDEFINES
               WS-CR-ENTRY-DATE.
               10  FILLER                   PIC 9(03).
               10  WS-CR-ENTRY-CYMD.
                   15  WS-CR-ENTRY-CC       PIC 9(02).
                   15  WS-CR-ENTRY-YY       PIC 9(02).
                   15  WS-CR-ENTRY-MM       PIC 9(02).
                   15  WS-CR-ENTRY-DD       PIC 9(02).

062304     05  WS-LF-EXIT-DATE              PIC 9(11).
062304     05  WS-LF-EXIT-DATE-R                      REDEFINES
062304         WS-LF-EXIT-DATE.
062304         10  FILLER                   PIC 9(03).
062304         10  WS-LF-EXIT-CYMD.
062304             15  WS-LF-EXIT-CC       PIC 9(02).
062304             15  WS-LF-EXIT-YY       PIC 9(02).
062304             15  WS-LF-EXIT-MM       PIC 9(02).
062304             15  WS-LF-EXIT-DD       PIC 9(02).

062304     05  WS-AH-EXIT-DATE              PIC 9(11).
062304     05  WS-AH-EXIT-DATE-R                      REDEFINES
062304         WS-AH-EXIT-DATE.
062304         10  FILLER                   PIC 9(03).
062304         10  WS-AH-EXIT-CYMD.
062304             15  WS-AH-EXIT-CC       PIC 9(02).
062304             15  WS-AH-EXIT-YY       PIC 9(02).
062304             15  WS-AH-EXIT-MM       PIC 9(02).
062304             15  WS-AH-EXIT-DD       PIC 9(02).

00082      05  WS-EARLIEST-PROCESS-DT. 
               10  WS-EARLIEST-PROC-DT-CCYY PIC 9(04)  VALUE 0.
               10  WS-EARLIEST-PROC-DT-MM   PIC 9(02)  VALUE 0.
               10  WS-EARLIEST-PROC-DT-DD   PIC 9(02)  VALUE 0.
           
           05  WS-CERT-EFF-DT               PIC 9(11).
           05  WS-CERT-EFF-DT-R                        REDEFINES
               WS-CERT-EFF-DT.
               10  FILLER                   PIC 9(03).
               10  WS-CERT-EFF-CYMD.
                   15  WS-CERT-EFF-CC       PIC 9(02).
                   15  WS-CERT-EFF-YY       PIC 9(02).
                   15  WS-CERT-EFF-MM       PIC 9(02).
                   15  WS-CERT-EFF-DD       PIC 9(02).

           05  WS-HOLD-ENTRY-DATE           PIC 9(11)  VALUE 0.
           05  WS-HOLD-ENTRY-DATE-R                    REDEFINES
               WS-HOLD-ENTRY-DATE.
               10  FILLER                   PIC 9(03).
               10  WS-HOLD-ENTRY-CYMD.
                   15  WS-HOLD-ENTRY-CC     PIC X(02).
                   15  WS-HOLD-ENTRY-YY     PIC X(02).
                   15  WS-HOLD-ENTRY-MM     PIC X(02).
                   15  FILLER               PIC X(02).

062304     05  WS-HOLD-EXIT-DATE            PIC 9(11)  VALUE 0.
062304     05  WS-HOLD-EXIT-DATE-R                     REDEFINES
062304         WS-HOLD-EXIT-DATE.
062304         10  FILLER                   PIC 9(03).
062304         10  WS-HOLD-EXIT-CYMD.
062304             15  WS-HOLD-EXIT-CC      PIC X(02).
062304             15  WS-HOLD-EXIT-YY      PIC X(02).
062304             15  WS-HOLD-EXIT-MM      PIC X(02).
062304             15  FILLER               PIC X(02).

00082  01  WS-SWITCHES.
           05  WS-EOF-OFFLN-CERT            PIC X(01)  VALUE SPACE.
               88  EOF                                 VALUE '1'.

           05  WS-FIRST-TIME-SW             PIC X(01)  VALUE SPACE.
               88  NOT-FIRST-TIME                      VALUE 'X'.

           05  WS-DETAIL-LINE-SW            PIC X(01)  VALUE SPACE.
               88  DETAIL-LN-NOT-DONE                  VALUE '1'.
               88  DETAIL-LN-WITH-PREMIUM-DONE         VALUE '2'.

           05  WS-SUBTOTAL-SW               PIC X(01)  VALUE SPACE.
               88  SUBTOT-NOT-NEEDED                   VALUE '1'.
               88  SUBTOT-REQUIRED-FOR-PROCMO          VALUE '2'.

           05  WS-MON-TOTAL-SW              PIC X(01)  VALUE SPACE.
               88  NEW-MONTH-TOTAL                     VALUE '1'.
               88  TOTAL-FOR-MONTH-DONE                VALUE '2'.

00082  01  WS-MISC.
           05  WS-HOLD-ACCT                 PIC X(10)  VALUE SPACES.
           05  WS-HOLD-STATE                PIC X(02)  VALUE SPACES.
           05  WS-HOLD-ENTRY-BATCH          PIC X(06)  VALUE SPACES.
           05  WS-HOLD-ALPHA-MO             PIC X(03)  VALUE SPACES.
           05  WS-HOLD-PREV-ACCT            PIC X(10)  VALUE SPACES.
           05  WS-HOLD-PREV-MM              PIC X(02)  VALUE SPACES.
           05  WS-HOLD-PREV-YY              PIC X(02)  VALUE SPACES.

           05  WS-PREMIUM-PAYMENT           PIC S9(07)V99 
                                                       VALUE ZEROS.




      ***** THE FOLLOWING TOTAL TABLES DEFINED FOR EACH ACCOUNT
      ******************************************************************
       01  WS-PAYMENT-BYBATCH-TABLE.
           05  WS-PAYMENT-BYBATCH         COMP-3   PIC S9(07)V99
                                                       VALUE ZEROS
                                                       OCCURS 12 TIMES.

       01  WS-PAYMENT-BYPROCMO-TABLE.
           05  WS-PAYMENT-BYPROCMO        COMP-3   PIC S9(07)V99
                                                       VALUE ZEROS
                                                       OCCURS 12 TIMES.

       01  WS-12MO-TOTALS-TABLE.
           05  WS-PAYMENT-12MOTOTS-BYACCT COMP-3   PIC S9(07)V99
                                                       VALUE ZEROS
                                                       OCCURS 12 TIMES.


      ***** THE FOLLOWING TOTAL TABLES DEFINED TO SUMMARIZE ALL ACCOUNTS
      ******************************************************************
       01  WS-ALLACCT-PAYMENTS-TABLE.
           05  WS-ALLACCT-BYPROCMO  OCCURS 12 TIMES.
               10  WS-ALLACCT-BYPROCMO-EFFMO COMP-3 PIC S9(07)V99
                                                       VALUE ZEROS
                                                       OCCURS 12 TIMES.

       01  WS-ALLACCT-12MO-TOTALS-TABLE.
           05  WS-ALLACCT-12MOTOTS          COMP-3 PIC S9(07)V99
                                                       VALUE ZEROS
                                                       OCCURS 12 TIMES.
      ******************************************************************

       01  WS-TOT-PAYMENTS-PROCMO          COMP-3  PIC S9(07)V99
                                                       VALUE ZEROS.

       01  WS-12MO-ACCT-TOTAL              COMP-3  PIC S9(07)V99
                                                       VALUE ZEROS.

       01  WS-12MO-ALLACCT-GRAND-TOTAL     COMP-3  PIC S9(07)V99
                                                       VALUE ZEROS.

      ******************************************************************

00259  01  MONTH-TAB.                                
00260      05  MONTH-TB1.                           
00261          10  FILLER                   PIC X(03)  VALUE 'JAN'. 
00262          10  FILLER                   PIC X(03)  VALUE 'FEB'.
00263          10  FILLER                   PIC X(03)  VALUE 'MAR'. 
00264          10  FILLER                   PIC X(03)  VALUE 'APR'.
00265          10  FILLER                   PIC X(03)  VALUE 'MAY'.   
00266          10  FILLER                   PIC X(03)  VALUE 'JUN'.  
00267          10  FILLER                   PIC X(03)  VALUE 'JUL'. 
00268          10  FILLER                   PIC X(03)  VALUE 'AUG'.
00269          10  FILLER                   PIC X(03)  VALUE 'SEP'.   
00270          10  FILLER                   PIC X(03)  VALUE 'OCT'.  
00271          10  FILLER                   PIC X(03)  VALUE 'NOV'. 
00272          10  FILLER                   PIC X(03)  VALUE 'DEC'.
00273      05  MONTH-TB2  REDEFINES  MONTH-TB1.                     
00274          10  ALPHA-MONTH              PIC X(03)  OCCURS 12 TIMES. 

00259  01  WS-NUMERIC-MONTH-TAB.  
00260      05  MONTH-TB3.                          
00261          10  FILLER                   PIC X(02)  VALUE '01'. 
00262          10  FILLER                   PIC X(02)  VALUE '02'.
00263          10  FILLER                   PIC X(02)  VALUE '03'.     
00264          10  FILLER                   PIC X(02)  VALUE '04'.    
00265          10  FILLER                   PIC X(02)  VALUE '05'.   
00266          10  FILLER                   PIC X(02)  VALUE '06'.  
00267          10  FILLER                   PIC X(02)  VALUE '07'. 
00268          10  FILLER                   PIC X(02)  VALUE '08'.     
00269          10  FILLER                   PIC X(02)  VALUE '09'.    
00270          10  FILLER                   PIC X(02)  VALUE '10'.   
00271          10  FILLER                   PIC X(02)  VALUE '11'.  
00272          10  FILLER                   PIC X(02)  VALUE '12'. 
00273      05  MONTH-TB4                               REDEFINES 
               MONTH-TB3.    
00274          10  NUMERIC-MONTH            PIC X(02)  OCCURS 12 TIMES.   
00275                                         
       01  WS-ACCOUNT-HEADING.
           05  FILLER                       PIC X(01)  VALUE '0'.
           05  FILLER                       PIC X(10)  VALUE
               'ACCOUNT - '.
           05  WS-H-ACCOUNT                 PIC X(10)  VALUE SPACES.
           05  FILLER                       PIC X(06)  VALUE SPACES.
           05  WS-H-LITERAL                 PIC X(08)  VALUE
               'STATE - '.
           05  WS-H-STATE                   PIC X(02)  VALUE SPACES.
           05  FILLER                       PIC X(96)  VALUE SPACES.


       01  WS-HEADING1.
           05  FILLER                       PIC X(01)  VALUE '1'.
           05  FILLER                       PIC X(47)  VALUE SPACES.
           05  WS-H1-TITLE                  PIC X(28)  VALUE
               'PERIODIC FEE PAYMENT HISTORY'.
           05  FILLER                       PIC X(5)  VALUE SPACES.
           05  WS-H1-CLP-TITLE              PIC X(40)  VALUE SPACES.
           05  WS-H1-REPORT-ID              PIC X(06)  VALUE 'ECS057'.
           05  WS-H1-CLP-ID                 PIC XXX    VALUE SPACES.
           05  FILLER                       PIC XXX  VALUE SPACES.


       01  WS-HEADING2.
           05  FILLER                       PIC X(01)  VALUE SPACE.
           05  FILLER                       PIC X(46)  VALUE SPACES.
           05  WS-H2-COMPANY-NAME           PIC X(30)  VALUE SPACES.
           05  FILLER                       PIC X(44)  VALUE SPACES.
           05  WS-H2-DATE                   PIC X(08)  VALUE SPACES.
           05  FILLER                       PIC X(04)  VALUE SPACES.


       01  WS-HEADING3.
           05  FILLER                       PIC X(01)  VALUE SPACE.
033104     05  FILLER                       PIC X(52)  VALUE SPACES.
033104     05  WS-H3-MONTHEND-DT            PIC X(18)  VALUE SPACES.
033104     05  FILLER                       PIC X(50)  VALUE SPACES.
           05  FILLER                       PIC X(04)  VALUE 'PAGE'.
           05  FILLER                       PIC X(01)  VALUE SPACES.
           05  WS-H3-PAGE                   PIC ZZ,ZZ9.
           05  FILLER                       PIC X(01)  VALUE SPACE.


       01  WS-HEADING4.
           05  FILLER                       PIC X(01)  VALUE '0'.
           05  FILLER                       PIC X(44)  VALUE SPACES.
           05  FILLER                       PIC X(33)  VALUE 
               'MASTER CERTIFICATE EFFECTIVE DATE'.
           05  FILLER                       PIC X(55)  VALUE SPACES.


       01  WS-HEADING5.
033104     05  FILLER                       PIC X(01)  VALUE ' '.
00163      05  FILLER                       PIC X(12)  VALUE SPACES.
00164      05  WS-H5-EFFECTIVE-MONTHS       PIC X(120) VALUE SPACES. 
00165      05  WS-H5-EFFECTIVE-MONTHS-R                REDEFINES    
00166          WS-H5-EFFECTIVE-MONTHS.
               10  WS-H5-EFF-MOYY                      OCCURS 12 TIMES.
                   15  FILLER               PIC X(05).
                   15  WS-H5-EFF-MO         PIC X(03).
                   15  WS-H5-EFF-YY         PIC X(02).
00169                                                             

       01  WS-HEADING6.
           05  FILLER                       PIC X(01)  VALUE SPACE.
           05  FILLER                       PIC X(09)  VALUE 
               'PROCESSED'.
           05  FILLER                       PIC X(125) VALUE SPACES.


       01  WS-HEADING7.
           05  FILLER                       PIC X(01)  VALUE SPACE.
           05  WS-H7-VARIABLE-HDR          PIC X(15)  VALUE 
               'DATE AND BATCH#'.
           05  FILLER                       PIC X(117) VALUE SPACES.


00170  01  WS-DETAIL.
00171      05  WS-D1-PRINT-CTL              PIC X(01)   VALUE ' '.   
           05  WS-D1-PROCESS-DATE.
00172          10  WS-D1-PROCESS-MO         PIC X(03)   VALUE SPACES.
               10  WS-D1-PROCESS-YY         PIC X(02)   VALUE SPACES.
00174      05  WS-D1-BATCH-NO               PIC X(06)   VALUE SPACES.
00175      05  FILLER                       PIC X(01)   VALUE SPACES.   
           05  WS-D1-PAYMENT                PIC ZZZZZ9.99-
                                                        OCCURS 12 TIMES.
00180                              

00276  01  WS-WK.                               
00277      12  WS-RETURN-CODE               PIC X(04)   VALUE SPACES. 
00278      12  WS-ABEND-MESSAGE             PIC X(80)   VALUE SPACES.
00279      12  WS-ZERO                      PIC S9(01)  VALUE ZERO. 
00280      12  WS-ABEND-FILE-STATUS         PIC X(02)   VALUE SPACES. 
00281                                                                   ECS057
00282      COPY ELCDTECX.                                               ECS057
00283                                                                   ECS057
00284      COPY ELCDTEVR.                                               ECS057
00285                                                                   ECS057
00286      COPY ELCDATE.                                                   CL**4

033104 LINKAGE SECTION.

033104 01 PARM.
033104    05  PARM-LENGTH          COMP     PIC S9(04)  VALUE ZEROS.
033104    05  PARM-CYCLE-DT-CYMD            PIC X(08)   VALUE SPACES.
080416    05  parm-filler                   pic x       value spaces.
011515    05  PARM-RPT-TYPE                 PIC XXX     VALUE SPACES.

033104 PROCEDURE DIVISION USING PARM.
00289                                                                   ECS057
00290  0000-MAIN. 
00291                              COPY ELCDTERX.                       ECS057
00292                                                                   ECS057
033104     PERFORM 0050-EDIT-ME-CYCLE-DT    THRU 0050-EXIT 
00294      MOVE COMPANY-NAME                TO WS-H2-COMPANY-NAME
           MOVE WS-CURRENT-DATE             TO WS-H2-DATE
00293      MOVE SPACES                      TO WS-H5-EFFECTIVE-MONTHS 

           SUBTRACT +1                      FROM RUN-CCYY 
               GIVING WS-HDR-EFF-CCYY  

00296      COMPUTE MONTH-SUB = RUN-MO + +1
00297      IF MONTH-SUB > +12         
00298          MOVE +1                      TO MONTH-SUB
               MOVE RUN-CCYY                TO WS-HDR-EFF-CCYY
           END-IF

           MOVE MONTH-SUB                   TO SAVE-1ST-EFF-MONTH
           MOVE WS-HDR-EFF-CCYY             TO WS-EARLIEST-PROC-DT-CCYY
           MOVE NUMERIC-MONTH (MONTH-SUB)   TO WS-EARLIEST-PROC-DT-MM
           MOVE '01'                        TO WS-EARLIEST-PROC-DT-DD

00299      MOVE +1                          TO COLUMN-SUB
           PERFORM 0100-EFF-MO-HDR          THRU 0100-EXIT
               UNTIL COLUMN-SUB > +12

062304     SORT SORTFL
062304             ON ASCENDING KEY SRT-STATE
062304                              SRT-ACCOUNT
062304                              SRT-ENTRY-DT
062304                              SRT-BATCH-NO
062304                              SRT-EFF-DT
062304         INPUT PROCEDURE 0010-INPUT-RTN  THRU 0019-EXIT
062304        OUTPUT PROCEDURE 0020-OUTPUT-RTN THRU 0029-EXIT.

062304     IF SORT-RETURN NOT = ZEROES
062304         MOVE +0101                     TO  WS-RETURN-CODE
062304         MOVE 'INTERNAL SORT ABORTED'   TO  WS-ABEND-MESSAGE
062304         GO TO ABEND-PGM
062304     END-IF
062304     
062304     GOBACK

062304     .           
062304 0010-INPUT-RTN SECTION.
062304 
062304     PERFORM 8800-OPEN-FILES             THRU 8800-EXIT 
062304     PERFORM 0200-READ-SORTED-OFFLN-CERT THRU 0200-EXIT
062304         UNTIL EOF
062304     .
062304 0019-EXIT.
062304     EXIT.

062304 0020-OUTPUT-RTN SECTION.
062304 

062304     MOVE SPACES                 TO WS-EOF-OFFLN-CERT

062304     PERFORM 0300-RETURN         THRU 0300-EXIT
062304         UNTIL EOF

062304********
      ***** Need to print totals for last month of last account
      ********
           MOVE WS-CR-ENTRY-CYMD              TO WS-HOLD-ENTRY-CYMD
           PERFORM 0225-WRITE-PREV-ACCUM-DATA THRU 0225-EXIT

           IF SUBTOT-REQUIRED-FOR-PROCMO
               PERFORM 0350-SUBTOTAL          THRU 0350-EXIT
               SET SUBTOT-NOT-NEEDED          TO TRUE
           END-IF

           PERFORM 0240-TOTAL-FOR-PROCMO      THRU 0240-EXIT
           PERFORM 0360-12MO-TOTAL-FOR-ACCT   THRU 0360-EXIT 

           PERFORM 0380-ALLACCT-SUMMARY       THRU 0380-EXIT
           PERFORM 8900-CLOSE-FILES           THRU 8900-EXIT 

           .
       0029-EXIT.
           EXIT.

00300                                                                   ECS057
033104 0050-EDIT-ME-CYCLE-DT.
033104
033104   IF PARM-LENGTH = +0
033104       DISPLAY 'CYCLE DATE INPUT PARM MISSING'
033104       PERFORM ABEND-PGM            THRU APS-EXIT
033104   END-IF
033104
033104   MOVE PARM-CYCLE-DT-CYMD          TO DC-GREG-DATE-CYMD
033104   MOVE 'L'                         TO DC-OPTION-CODE
033104   PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT
033104
033104   IF NO-CONVERSION-ERROR
033104       MOVE DC-GREG-DATE-1-ALPHA    TO WS-H3-MONTHEND-DT
033104   ELSE
033104       DISPLAY 'INVALID ME CYCLE DATE ' PARM-CYCLE-DT-CYMD
033104       PERFORM ABEND-PGM            THRU APS-EXIT
033104   END-IF

         IF PARM-RPT-TYPE = 'CLP'
            MOVE ' BY CLP STATE '      TO WS-H1-CLP-TITLE
            MOVE 'CLP'                 TO WS-H1-CLP-ID
         END-IF

033104   .
033104 0050-EXIT.
033104     EXIT.

00301  0100-EFF-MO-HDR.  

00302      MOVE ALPHA-MONTH (MONTH-SUB)   TO WS-H5-EFF-MO (COLUMN-SUB)
           MOVE WS-HDR-EFF-YY             TO WS-H5-EFF-YY (COLUMN-SUB)
00303                                                                   ECS057
00304      ADD +1                         TO MONTH-SUB
00305                                                                   ECS057
00306      IF MONTH-SUB > +12      
00307          MOVE +1                    TO MONTH-SUB
               MOVE RUN-CCYY              TO WS-HDR-EFF-CCYY
           END-IF 
00308                                                                   ECS057
00309      ADD +1                         TO COLUMN-SUB
00310                                                                   ECS057
           .

       0100-EXIT.
           EXIT.
00451  0200-READ-SORTED-OFFLN-CERT.

00452      READ OFFLN-CERT-IN 
               AT END 
                   SET EOF                        TO TRUE
                   GO TO 0200-EXIT
           END-READ

           MOVE CR-ENTRY-DATE                     TO WS-CR-ENTRY-DATE
062304     MOVE CR-LF-CANCEL-EXIT-DATE            TO WS-LF-EXIT-DATE
062304     MOVE CR-AH-CANCEL-EXIT-DATE            TO WS-AH-EXIT-DATE

     ********* 
     *** Report goes back 12 months from current processing month 
     *********      
           IF (WS-CR-ENTRY-CYMD >= WS-EARLIEST-PROCESS-DT)
062304        OR (WS-LF-EXIT-CYMD >= WS-EARLIEST-PROCESS-DT)
062304        OR (WS-AH-EXIT-CYMD >= WS-EARLIEST-PROCESS-DT)
              CONTINUE
           ELSE
               GO TO 0200-EXIT
           END-IF

           if (cr-policy-is-cash)
060308        or (CR-LFTYP = 'MJ' OR 'MS'
060308         OR 'NJ' OR 'NL'
060308         OR 'SJ' OR 'SL'
070708         OR 'HJ' OR 'HL'
071212         OR 'PL' OR 'PJ')
060308        OR (CR-AHTYP =
060308               'MJ' OR 'MS' OR 'MI' OR 'MU'
060308            OR 'MX' OR 'MY' OR 'MZ'
060308            OR 'NI' OR 'NJ' OR 'NL' OR 'NU'
060308            OR 'SI' OR 'SJ' OR 'SL' OR 'SU'
070708            OR 'HD' OR 'HE' OR 'HF' OR 'HG'
070708            OR 'HJ' OR 'HL'
071212            OR 'PL' OR 'PJ' OR 'PI' OR 'PU')
060308        CONTINUE
060308     ELSE
060308        GO TO 0200-EXIT
060308     END-IF

011116     if cr-carrier = '7' or '9'
070715        move +0                  to ws-fees-outside-clp
070715                                    ws-lf-canc-fees-outside-clp
070715                                    ws-ah-canc-fees-outside-clp
070715        perform varying a1 from +1 by +1 until a1 > +10
070715           if cr-agt-type (A1) = 'C' OR 'D' OR 'S' OR 'O'
070715              compute ws-fees-outside-clp rounded =
070715                 ws-fees-outside-clp +
070715                 cr-lfprm * cr-lcom-l (a1) +
070715                 cr-ahprm * cr-lcom-ah (a1)
070715              compute ws-lf-canc-fees-outside-clp rounded =
070715                 ws-lf-canc-fees-outside-clp +
070715                 cr-lfrfnd * cr-lcom-l (a1)
070715              compute ws-ah-canc-fees-outside-clp rounded =
070715                 ws-ah-canc-fees-outside-clp +
070715                 cr-ahrfnd * cr-lcom-ah (a1)
070715           end-if
070715        end-perform
070715     end-if

062304     IF WS-CR-ENTRY-CYMD >= WS-EARLIEST-PROCESS-DT
062304        MOVE CR-STATE            TO SRT-STATE
062304        MOVE CR-ACCOUNT          TO SRT-ACCOUNT
070715        if cr-carrier = 7
070715           compute srt-amount =
070715              cr-lfprm + cr-ahprm - ws-fees-outside-clp
070715        else
033104           COMPUTE SRT-AMOUNT = (CR-LFPRM + CR-AHPRM) -
033104                                 ((CR-LFPRM * CR-LCOM-L (1)) +
033104                                 (CR-AHPRM * CR-LCOM-AH (1)))
070715        end-if
              MOVE CR-DT               TO SRT-EFF-DT
              MOVE CR-ENTRY-DATE       TO SRT-ENTRY-DT
              MOVE CR-ENTRY-BATCH      TO SRT-BATCH-NO
              RELEASE SRT-REC
062304     END-IF

062304     IF WS-LF-EXIT-CYMD >= WS-EARLIEST-PROCESS-DT
062304        MOVE CR-STATE            TO SRT-STATE
062304        MOVE CR-ACCOUNT          TO SRT-ACCOUNT
070715        if cr-carrier = 7
070715           compute srt-amount = (cr-lfrfnd -
070715              ws-lf-canc-fees-outside-clp) * -1
070715        else
062304           COMPUTE SRT-AMOUNT = (CR-LFRFND
062304           - (CR-LFRFND * CR-LCOM-L (1))) * -1
070715        end-if
062304        MOVE CR-LF-CANC-DT       TO SRT-EFF-DT
062304        MOVE CR-LF-CANCEL-EXIT-DATE
062304                                 TO SRT-ENTRY-DT
062304        MOVE CR-LF-EXIT-BATCH    TO SRT-BATCH-NO
062304        RELEASE SRT-REC
062304     END-IF

062304     IF WS-AH-EXIT-CYMD >= WS-EARLIEST-PROCESS-DT
062304        MOVE CR-STATE            TO SRT-STATE
062304        MOVE CR-ACCOUNT          TO SRT-ACCOUNT
070715        if cr-carrier = 7
070715           compute srt-amount = (cr-ahrfnd -
070715              ws-ah-canc-fees-outside-clp) * -1
070715        else
062304           COMPUTE SRT-AMOUNT = (CR-AHRFND
062304              - (CR-AHRFND * CR-LCOM-AH (1))) * -1
070715        end-if
062304        MOVE CR-AH-CANC-DT       TO SRT-EFF-DT
062304        MOVE CR-AH-CANCEL-EXIT-DATE
062304                                 TO SRT-ENTRY-DT
062304        MOVE CR-AH-EXIT-BATCH    TO SRT-BATCH-NO
062304        RELEASE SRT-REC
062304     END-IF

           .
       0200-EXIT.
           EXIT.

00451  0300-RETURN.

062304     RETURN SORTFL 
062304         AT END 
062304             SET EOF                        TO TRUE
062304             GO TO 0300-EXIT
062304     END-RETURN

           MOVE SRT-ENTRY-DT                    TO WS-CR-ENTRY-DATE

     ********* 
     *** Report goes back 12 months from current processing month 
     *********      

           IF SRT-ACCOUNT = WS-HOLD-ACCT    
              AND WS-CR-ENTRY-CYMD = WS-HOLD-ENTRY-CYMD 
              AND SRT-BATCH-NO = WS-HOLD-ENTRY-BATCH
               CONTINUE
           ELSE
               IF NOT-FIRST-TIME 
                   PERFORM 0225-WRITE-PREV-ACCUM-DATA
                                                  THRU 0225-EXIT
               ELSE
                   MOVE +1                        TO SUB
                   SET NOT-FIRST-TIME             TO TRUE
                   MOVE SRT-ACCOUNT               TO WS-H-ACCOUNT
                   MOVE SRT-STATE                 TO WS-H-STATE
               END-IF

               MOVE SRT-ACCOUNT                   TO WS-HOLD-ACCT
               MOVE SRT-STATE                     TO WS-HOLD-STATE   
               MOVE WS-CR-ENTRY-CYMD              TO WS-HOLD-ENTRY-CYMD
               MOVE SRT-BATCH-NO                  TO WS-HOLD-ENTRY-BATCH
           END-IF

           MOVE SRT-AMOUNT             TO WS-PREMIUM-PAYMENT
           MOVE SRT-EFF-DT             TO WS-CERT-EFF-DT
           MOVE WS-HOLD-ENTRY-MM       TO SUB1
           
           PERFORM 0250-ACCUM-PAYMENTS
                                       THRU 0250-EXIT


           .
       0300-EXIT.
           EXIT.

00451  0225-WRITE-PREV-ACCUM-DATA.

      **** Entry dates correspond to process dates on the report   
      **** The input file is presorted in the job step prior to executing
      **** this program.  The sort order is account number, entry date, 
      **** batch#, & eff date.

      ***** IF there are multiple batches in the same month for the
      ***** same account, don't bump to next process month.
      *    IF WS-HOLD-ENTRY-MM = WS-HOLD-PREV-MM
      *       AND WS-HOLD-ACCT = WS-HOLD-PREV-ACCT
      *        SUBTRACT +1                        FROM SUB
      *        SET SUBTOT-REQUIRED-FOR-PROCMO     TO TRUE
      *    END-IF

           MOVE WS-HOLD-ENTRY-MM                  TO SUB2   
           PERFORM 0340-DETAIL-LINES              THRU 0340-EXIT
               VARYING SUB FROM SUB BY +1 
               UNTIL DETAIL-LN-WITH-PREMIUM-DONE
                     OR SUB > +12
           SUBTRACT +1                            FROM SUB

           PERFORM 0370-CLEAR-PAYMENT-TABLE       THRU 0370-EXIT
               VARYING COLUMN-SUB FROM +1 BY +1
               UNTIL COLUMN-SUB > +12

           SET DETAIL-LN-NOT-DONE                 TO TRUE  

           IF SUBTOT-REQUIRED-FOR-PROCMO
              AND (WS-CR-ENTRY-MM NOT = WS-HOLD-ENTRY-MM
                  OR SRT-ACCOUNT NOT = WS-HOLD-ACCT)
               PERFORM 0350-SUBTOTAL              THRU 0350-EXIT
               SET SUBTOT-NOT-NEEDED              TO TRUE
               PERFORM 0240-TOTAL-FOR-PROCMO      THRU 0240-EXIT
               SET TOTAL-FOR-MONTH-DONE           TO TRUE
           END-IF

           IF WS-TOT-PAYMENTS-PROCMO NOT = +0
              AND NOT TOTAL-FOR-MONTH-DONE
               IF WS-CR-ENTRY-MM NOT = WS-HOLD-ENTRY-MM 
                  OR SRT-ACCOUNT NOT = WS-HOLD-ACCT
                   PERFORM 0240-TOTAL-FOR-PROCMO  THRU 0240-EXIT
               END-IF
           END-IF  

           IF TOTAL-FOR-MONTH-DONE
               SET NEW-MONTH-TOTAL                TO TRUE
           END-IF

           MOVE WS-HOLD-ENTRY-MM                  TO WS-HOLD-PREV-MM
           MOVE WS-HOLD-ACCT                      TO WS-HOLD-PREV-ACCT

           IF SRT-ACCOUNT = WS-HOLD-ACCT
               CONTINUE
           ELSE
               PERFORM 0360-12MO-TOTAL-FOR-ACCT   THRU 0360-EXIT
               MOVE +1                            TO SUB
               MOVE SRT-ACCOUNT                   TO WS-H-ACCOUNT
               MOVE SRT-STATE                     TO WS-H-STATE
               MOVE +56                           TO WS-LINE-COUNT
           END-IF

           .
       0225-EXIT.
           EXIT.


00451  0240-TOTAL-FOR-PROCMO.

           MOVE SPACES                         TO WS-DETAIL
           MOVE WS-HOLD-ALPHA-MO               TO WS-D1-PROCESS-MO  
           MOVE WS-HOLD-ENTRY-YY               TO WS-D1-PROCESS-YY
           MOVE 'SUBTOT'                       TO WS-D1-BATCH-NO  
           MOVE WS-TOT-PAYMENTS-PROCMO         TO WS-D1-PAYMENT (1)
           PERFORM 0400-PRINT-DETAIL           THRU 0400-EXIT
           MOVE ZEROS                          TO WS-TOT-PAYMENTS-PROCMO
           ADD +1                              TO SUB

           MOVE SPACES                         TO WS-DETAIL
           PERFORM 0400-PRINT-DETAIL           THRU 0400-EXIT

           .
       0240-EXIT.
           EXIT.


       0250-ACCUM-PAYMENTS.

           EVALUATE TRUE
           WHEN WS-CERT-EFF-MM = 01
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (1)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (1)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (1)
               ADD WS-PREMIUM-PAYMENT 
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 1)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (1)
           
           WHEN WS-CERT-EFF-MM = 02
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (2)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (2)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (2)
               ADD WS-PREMIUM-PAYMENT
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 2)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (2)
           
           WHEN WS-CERT-EFF-MM = 03
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (3)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (3)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (3)
               ADD WS-PREMIUM-PAYMENT 
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 3)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (3)
           
           WHEN WS-CERT-EFF-MM = 04
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (4)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (4)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (4)
               ADD WS-PREMIUM-PAYMENT 
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 4)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (4)

           WHEN WS-CERT-EFF-MM = 05
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (5)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (5)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (5)
               ADD WS-PREMIUM-PAYMENT 
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 5)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (5)
           
           WHEN WS-CERT-EFF-MM = 06
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (6)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (6)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (6)
               ADD WS-PREMIUM-PAYMENT 
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 6)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (6)
           
           WHEN WS-CERT-EFF-MM = 07
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (7)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (7)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (7)
               ADD WS-PREMIUM-PAYMENT 
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 7)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (7)
           
           WHEN WS-CERT-EFF-MM = 08
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (8)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (8)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (8)
               ADD WS-PREMIUM-PAYMENT 
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 8)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (8)
           
           WHEN WS-CERT-EFF-MM = 09
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (9)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (9)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (9)
               ADD WS-PREMIUM-PAYMENT 
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 9)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (9)

           WHEN WS-CERT-EFF-MM = 10
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (10)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (10)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (10)
               ADD WS-PREMIUM-PAYMENT 
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 10)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (10)
           
           WHEN WS-CERT-EFF-MM = 11
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (11)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (11)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (11)
               ADD WS-PREMIUM-PAYMENT 
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 11)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (11)

           WHEN WS-CERT-EFF-MM = 12
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYBATCH (12)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-BYPROCMO (12)
               ADD WS-PREMIUM-PAYMENT TO WS-PAYMENT-12MOTOTS-BYACCT (12)
               ADD WS-PREMIUM-PAYMENT
                                 TO WS-ALLACCT-BYPROCMO-EFFMO (SUB1 12)
               ADD WS-PREMIUM-PAYMENT TO WS-ALLACCT-12MOTOTS (12)
           END-EVALUATE

           ADD WS-PREMIUM-PAYMENT     TO WS-TOT-PAYMENTS-PROCMO
           
           .
       0250-EXIT.
           EXIT.


       0340-DETAIL-LINES.

           IF WS-H5-EFF-MO (SUB) = ALPHA-MONTH (SUB2)
              AND WS-H5-EFF-YY (SUB) = WS-HOLD-ENTRY-YY
               SET DETAIL-LN-WITH-PREMIUM-DONE   TO TRUE
               MOVE ALPHA-MONTH (SUB2)           TO WS-HOLD-ALPHA-MO
               MOVE WS-HOLD-ENTRY-BATCH          TO WS-D1-BATCH-NO
               MOVE SAVE-1ST-EFF-MONTH           TO SUB3
               MOVE +1                           TO COLUMN-SUB 
               PERFORM 0345-MOVE-PAYMENTS-TO-RPT THRU 0345-EXIT
                   VARYING SUB3 FROM SUB3 BY +1
                   UNTIL SUB3 > +12
               IF SAVE-1ST-EFF-MONTH > +1
                   PERFORM 0345-MOVE-PAYMENTS-TO-RPT 
                                                 THRU 0345-EXIT
                       VARYING SUB3 FROM +1 BY +1
                       UNTIL SUB3 = SAVE-1ST-EFF-MONTH
               END-IF
           ELSE
               MOVE SPACES                       TO WS-D1-BATCH-NO
               PERFORM 0347-ZERO-PAYMENTS        THRU 0347-EXIT
                   VARYING COLUMN-SUB FROM +1 BY +1
                   UNTIL COLUMN-SUB > +12
           END-IF

           MOVE WS-H5-EFF-MO (SUB)              TO WS-D1-PROCESS-MO
           MOVE WS-H5-EFF-YY (SUB)              TO WS-D1-PROCESS-YY
           PERFORM 0400-PRINT-DETAIL            THRU 0400-EXIT
           
           .
       0340-EXIT.
           EXIT.


       0345-MOVE-PAYMENTS-TO-RPT. 

           MOVE WS-PAYMENT-BYBATCH (SUB3) TO WS-D1-PAYMENT (COLUMN-SUB)
           ADD +1                         TO COLUMN-SUB
            
           .
       0345-EXIT.
           EXIT.

       0347-ZERO-PAYMENTS. 

           MOVE ZEROS                     TO WS-D1-PAYMENT (COLUMN-SUB)
 
           .
       0347-EXIT.
           EXIT.

       0350-SUBTOTAL.

           MOVE SPACES                          TO WS-DETAIL
           MOVE WS-H5-EFF-MO (SUB)              TO WS-D1-PROCESS-MO
           MOVE WS-H5-EFF-YY (SUB)              TO WS-D1-PROCESS-YY
           MOVE 'TOTALS'                        TO WS-D1-BATCH-NO
           PERFORM 0400-PRINT-DETAIL            THRU 0400-EXIT
           MOVE SPACES                          TO WS-DETAIL
           MOVE 'BY'                            TO WS-D1-PROCESS-YY
           MOVE 'EFF MO'                        TO WS-D1-BATCH-NO

           MOVE SAVE-1ST-EFF-MONTH              TO SUB3
           MOVE +1                              TO COLUMN-SUB 
           PERFORM 0355-MOVE-PROCMO-TOTS-TO-RPT THRU 0355-EXIT
               VARYING SUB3 FROM SUB3 BY +1
               UNTIL SUB3 > +12
           IF SAVE-1ST-EFF-MONTH > +1
               PERFORM 0355-MOVE-PROCMO-TOTS-TO-RPT THRU 0355-EXIT
                   VARYING SUB3 FROM +1 BY +1
                   UNTIL SUB3 = SAVE-1ST-EFF-MONTH
           END-IF

           PERFORM 0400-PRINT-DETAIL            THRU 0400-EXIT

           MOVE SPACES                          TO WS-DETAIL
           MOVE 'SUBTOT'                        TO WS-D1-BATCH-NO
           MOVE WS-TOT-PAYMENTS-PROCMO          TO WS-D1-PAYMENT (1)
           PERFORM 0400-PRINT-DETAIL            THRU 0400-EXIT

           PERFORM 0373-CLEAR-SUBTOTALS         THRU 0373-EXIT
               VARYING COLUMN-SUB FROM +1 BY +1
               UNTIL COLUMN-SUB > +12
           
           .
       0350-EXIT.
           EXIT.

       0355-MOVE-PROCMO-TOTS-TO-RPT.

           MOVE WS-PAYMENT-BYPROCMO (SUB3) 
                                          TO WS-D1-PAYMENT (COLUMN-SUB)
           ADD +1                         TO COLUMN-SUB
            
           .
       0355-EXIT.
           EXIT.


       0360-12MO-TOTAL-FOR-ACCT.

           MOVE SPACES                        TO WS-DETAIL
           MOVE '-'                           TO WS-D1-PRINT-CTL
           MOVE '12M'                         TO WS-D1-PROCESS-MO
           MOVE 'O '                          TO WS-D1-PROCESS-YY
           MOVE 'TOTALS'                      TO WS-D1-BATCH-NO
           PERFORM 0400-PRINT-DETAIL          THRU 0400-EXIT

           MOVE SPACE                         TO WS-D1-PRINT-CTL
           MOVE 'BY'                          TO WS-D1-PROCESS-YY
           MOVE 'EFF MO'                      TO WS-D1-BATCH-NO
           MOVE SAVE-1ST-EFF-MONTH            TO SUB3
           MOVE +1                            TO COLUMN-SUB 
           PERFORM 0365-MOVE-12MO-TOTS        THRU 0365-EXIT 
               VARYING SUB3 FROM SUB3 BY +1
               UNTIL SUB3 > +12

           IF SAVE-1ST-EFF-MONTH > +1
               PERFORM 0365-MOVE-12MO-TOTS    THRU 0365-EXIT
                   VARYING SUB3 FROM +1 BY +1
                   UNTIL SUB3 = SAVE-1ST-EFF-MONTH
           END-IF

           PERFORM 0400-PRINT-DETAIL          THRU 0400-EXIT

033104     MOVE SPACES                        TO WS-DETAIL
033104     MOVE '0'                           TO WS-D1-PRINT-CTL
033104     MOVE 'ACC'                         TO WS-D1-PROCESS-MO
033104     MOVE 'T '                          TO WS-D1-PROCESS-YY
033104     MOVE 'TOTALS'                      TO WS-D1-BATCH-NO   
033104     PERFORM 0400-PRINT-DETAIL          THRU 0400-EXIT

           MOVE SPACES                        TO WS-DETAIL
           MOVE 'LAS'                         TO WS-D1-PROCESS-MO
           MOVE 'T '                          TO WS-D1-PROCESS-YY
           MOVE '12 MOS'                      TO WS-D1-BATCH-NO
           MOVE WS-12MO-ACCT-TOTAL            TO WS-D1-PAYMENT (1)
           PERFORM 0400-PRINT-DETAIL          THRU 0400-EXIT

           PERFORM 0375-CLEAR-12MO-TOTALS     THRU 0375-EXIT
               VARYING COLUMN-SUB FROM +1 BY +1
               UNTIL COLUMN-SUB > +12

           MOVE ZEROS                         TO WS-12MO-ACCT-TOTAL

           
           .
       0360-EXIT.
           EXIT.

       0365-MOVE-12MO-TOTS. 

           MOVE WS-PAYMENT-12MOTOTS-BYACCT (SUB3)
                                          TO WS-D1-PAYMENT (COLUMN-SUB)
           ADD +1                         TO COLUMN-SUB
           ADD WS-PAYMENT-12MOTOTS-BYACCT (SUB3)  TO WS-12MO-ACCT-TOTAL
            
           .
       0365-EXIT.
           EXIT.


       0370-CLEAR-PAYMENT-TABLE. 

           MOVE ZEROS                TO WS-PAYMENT-BYBATCH (COLUMN-SUB)
 
           .
       0370-EXIT.
           EXIT.

       0373-CLEAR-SUBTOTALS. 

           MOVE ZEROS                TO WS-PAYMENT-BYPROCMO (COLUMN-SUB)
 
           .
       0373-EXIT.
           EXIT.

       0375-CLEAR-12MO-TOTALS. 

           MOVE ZEROS        TO WS-PAYMENT-12MOTOTS-BYACCT (COLUMN-SUB)
 
           .
       0375-EXIT.
           EXIT.


       0380-ALLACCT-SUMMARY.

           MOVE 'ALL ACCTS'                      TO WS-H-ACCOUNT
           MOVE SPACES                           TO WS-H-LITERAL
           MOVE SPACES                           TO WS-H-STATE  
           MOVE +56                              TO WS-LINE-COUNT 
           MOVE 'DATE           '                TO WS-H7-VARIABLE-HDR

           MOVE SAVE-1ST-EFF-MONTH               TO PROCMO
           MOVE +1                               TO MOHDR
           PERFORM 0382-ALLACCT-PROCMO-TOTS      THRU 0382-EXIT
               VARYING PROCMO FROM PROCMO BY +1
                   UNTIL PROCMO > +12
           IF SAVE-1ST-EFF-MONTH > +1
               PERFORM 0382-ALLACCT-PROCMO-TOTS  THRU 0382-EXIT
                   VARYING PROCMO FROM +1 BY +1
                       UNTIL PROCMO = SAVE-1ST-EFF-MONTH
           END-IF

           MOVE SPACES                           TO WS-DETAIL
           MOVE '0'                              TO WS-D1-PRINT-CTL
           MOVE '12M'                            TO WS-D1-PROCESS-MO
           MOVE 'O '                             TO WS-D1-PROCESS-YY
           MOVE 'TOTALS'                         TO WS-D1-BATCH-NO
           PERFORM 0400-PRINT-DETAIL             THRU 0400-EXIT

           MOVE SPACE                            TO WS-D1-PRINT-CTL
           MOVE 'BY'                             TO WS-D1-PROCESS-YY
           MOVE 'EFF MO'                         TO WS-D1-BATCH-NO
           MOVE SAVE-1ST-EFF-MONTH               TO SUB3
           MOVE +1                               TO COLUMN-SUB 
           PERFORM 0385-MOVE-ALLACCTS-12MO-TOTS  THRU 0385-EXIT 
               VARYING SUB3 FROM SUB3 BY +1
               UNTIL SUB3 > +12

           IF SAVE-1ST-EFF-MONTH > +1
               PERFORM 0385-MOVE-ALLACCTS-12MO-TOTS THRU 0385-EXIT
                   VARYING SUB3 FROM +1 BY +1
                   UNTIL SUB3 = SAVE-1ST-EFF-MONTH
           END-IF

           PERFORM 0400-PRINT-DETAIL             THRU 0400-EXIT

033104     MOVE SPACES                           TO WS-DETAIL
033104     MOVE '0'                              TO WS-D1-PRINT-CTL
033104     MOVE 'GRA'                            TO WS-D1-PROCESS-MO
033104     MOVE 'ND'                             TO WS-D1-PROCESS-YY
033104     MOVE ' TOTAL'                         TO WS-D1-BATCH-NO
033104     PERFORM 0400-PRINT-DETAIL             THRU 0400-EXIT

           MOVE SPACES                           TO WS-DETAIL
           MOVE 'LAS'                            TO WS-D1-PROCESS-MO
           MOVE 'T '                             TO WS-D1-PROCESS-YY
           MOVE '12 MOS'                         TO WS-D1-BATCH-NO
           MOVE WS-12MO-ALLACCT-GRAND-TOTAL      TO WS-D1-PAYMENT (1)
           PERFORM 0400-PRINT-DETAIL             THRU 0400-EXIT

           .
       0380-EXIT.
           EXIT.


       0382-ALLACCT-PROCMO-TOTS.
   
           MOVE SPACES                           TO WS-DETAIL
           MOVE WS-H5-EFF-MOYY (MOHDR) (6:5)     TO WS-D1-PROCESS-DATE

           MOVE +1                               TO EFFMOCOL
           PERFORM 0384-ALLACCT-EFFMO-TOTS       THRU 0384-EXIT
               VARYING EFFMO FROM SAVE-1ST-EFF-MONTH BY +1
                   UNTIL EFFMO > +12
           IF SAVE-1ST-EFF-MONTH > +1
               PERFORM 0384-ALLACCT-EFFMO-TOTS   THRU 0384-EXIT
                   VARYING EFFMO FROM +1 BY +1
                       UNTIL EFFMO = SAVE-1ST-EFF-MONTH
           END-IF
           PERFORM 0400-PRINT-DETAIL             THRU 0400-EXIT
           ADD +1                                TO MOHDR
 
           .
       0382-EXIT.
           EXIT.


       0384-ALLACCT-EFFMO-TOTS.

           MOVE WS-ALLACCT-BYPROCMO-EFFMO (PROCMO EFFMO)
                                           TO WS-D1-PAYMENT (EFFMOCOL)
           ADD +1                          TO EFFMOCOL

           .
       0384-EXIT.
           EXIT.


       0385-MOVE-ALLACCTS-12MO-TOTS. 

           MOVE WS-ALLACCT-12MOTOTS (SUB3) 
                                          TO WS-D1-PAYMENT (COLUMN-SUB)
           ADD +1                         TO COLUMN-SUB
           ADD WS-ALLACCT-12MOTOTS (SUB3) TO WS-12MO-ALLACCT-GRAND-TOTAL
            
           .
       0385-EXIT.
           EXIT.


       0400-PRINT-DETAIL.

           IF WS-LINE-COUNT >= WS-LINE-COUNT-MAX
               PERFORM 4500-PRINT-HEADINGS  THRU 4500-EXIT
           END-IF

           MOVE WS-DETAIL                   TO PRT
           PERFORM 4900-WRITE               THRU 4900-EXIT

           INITIALIZE WS-DETAIL

           .
       0400-EXIT.
           EXIT.


       4500-PRINT-HEADINGS.

           MOVE WS-HEADING1                 TO PRT
           PERFORM 4900-WRITE               THRU 4900-EXIT

           MOVE WS-HEADING2                 TO PRT
           PERFORM 4900-WRITE               THRU 4900-EXIT

           ADD +1                           TO WS-PAGE
           MOVE WS-PAGE                     TO WS-H3-PAGE
           MOVE WS-HEADING3                 TO PRT
           PERFORM 4900-WRITE               THRU 4900-EXIT

           MOVE WS-ACCOUNT-HEADING          TO PRT
           PERFORM 4900-WRITE               THRU 4900-EXIT

           MOVE WS-HEADING4                 TO PRT
           PERFORM 4900-WRITE               THRU 4900-EXIT

           MOVE WS-HEADING5                 TO PRT
           PERFORM 4900-WRITE               THRU 4900-EXIT

           MOVE WS-HEADING6                 TO PRT
           PERFORM 4900-WRITE               THRU 4900-EXIT

           MOVE WS-HEADING7                 TO PRT
           PERFORM 4900-WRITE               THRU 4900-EXIT

           MOVE SPACES                      TO PRT
           PERFORM 4900-WRITE               THRU 4900-EXIT

           .
       4500-EXIT.
           EXIT.
 

       4900-WRITE.

           EVALUATE TRUE
           WHEN P-CTL = '1'
               MOVE +1                      TO WS-LINE-COUNT

           WHEN P-CTL = SPACE
               ADD +1                       TO WS-LINE-COUNT

           WHEN P-CTL = '0'
               ADD +2                       TO WS-LINE-COUNT

           WHEN OTHER
               ADD +3                       TO WS-LINE-COUNT
           END-EVALUATE

           WRITE PRT

           .
       4900-EXIT.
           EXIT.

033104 8500-DATE-CONVERSION.
033104
033104     CALL 'ELDATCX' USING DATE-CONVERSION-DATA
033104
033104     .
033104 8500-EXIT.
033104     EXIT.

00828  8800-OPEN-FILES.
00345                                                                   ECS057
00346      OPEN INPUT   OFFLN-CERT-IN                                   ECS057
00347           OUTPUT  PRT-RPT

           .
       8800-EXIT.
           EXIT.
00348                                                                   ECS057
00828  8900-CLOSE-FILES.
00345                                                                   ECS057
00346      CLOSE OFFLN-CERT-IN    
00347            PRT-RPT

           .
       8900-EXIT.
           EXIT.
00348                                                                   ECS057
           
00831  ABEND-PGM SECTION.                                               ECS057
00832                              COPY ELCABEND.                       ECS057
