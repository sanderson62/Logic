      *$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
NTTDel*PROGRAM-ID.    CIDEPX24.
NTTIns PROGRAM-ID.    SQLBEPEC24.
       AUTHOR.        Cowtown.
       DATE-COMPILED.

      *REMARKS.

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
012417*  012417 CR2017011700001   PEMA  NEW PROGRAM
111317*  111317 IR2017111300001   PEMA  CORRECT BRANCH (GOTO)
063022*  063022 CR2019012500003   PEMA  Migrate DB's to SQLSERVER 2016
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EPECS                ASSIGN TO SYS010.
           SELECT DISK-DATE            ASSIGN TO SYS019.

           SELECT ERACCTT              ASSIGN TO ERACCTT
                                       ACCESS IS SEQUENTIAL
                                       ORGANIZATION IS INDEXED
                                      FILE STATUS IS ERACCT-FILE-STATUS
                                      RECORD KEY IS AM-CONTROL-PRIMARY.

       DATA DIVISION.
       FILE SECTION.

       FD  EPECS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
                                       COPY ECSEPC01.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERACCTT.    
                                                                        
                                       COPY ERCACCT.                        


       WORKING-STORAGE SECTION.

NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '    CIDEPX24 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EP-CODE                  PIC X  VALUE SPACES.
       77  WS-DISP-AMT                 PIC -ZZZ,ZZZ,999.99.
       77  WS-DISP-AMTA                PIC -ZZZ,ZZZ,999.99.
       77  WS-DISP-AMTB                PIC -ZZZ,ZZZ,999.99.
       77  WS-DISP-DATE                PIC 9(11)  VALUE ZEROS.
       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-EPEC                VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  EPEC-IN-CNT                 PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.
       77  A1                          PIC S9(5)  VALUE +0 COMP-3.
       77  d1                          PIC S9(5)  VALUE +0 COMP-3.
       77  w1                          PIC S9(5)  VALUE +0 COMP-3.
       77  S1                          PIC S9(5)  VALUE +0 COMP-3.
       77  WS-S1                       PIC S999   VALUE +0 COMP-3.
       77  WS-S2                       PIC S999   VALUE +0 COMP-3.
       77  WS-S3                       PIC S999   VALUE +0 COMP-3.
       77  WS-S4                       PIC S999   VALUE +0 COMP-3.
       77  WS-WORK-TAX                 PIC S9(7)V9(5) VALUE +0 COMP-3.
       77  ERACCT-FILE-STATUS          PIC XX VALUE LOW-VALUES.
       77  ERRTBL-FILE-STATUS          PIC XX VALUE LOW-VALUES.
       77  ERACCT-EOF-SW               PIC X  VALUE SPACES.
           88  END-OF-ERACCT                  VALUE 'Y'.
       77  ERRTBL-EOF-SW               PIC X  VALUE SPACES.
           88  END-OF-ERRTBL                  VALUE 'Y'.
       77  WS-ERN-PRM                  PIC S9(9)V99 VALUE +0 COMP-3.
       77  WS-ERN-COMM                 PIC S9(9)V99 VALUE +0 COMP-3.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  WS-COMM-PCT                 PIC S9(5)V99    VALUE +0 COMP-3.
       77  WS-WRK-LR-LO         PIC S9(5)V99    VALUE  -999.99 COMP-3.
       77  WS-WRK-LR-HI         PIC S9(5)V99    VALUE  +999.99 COMP-3.
       77  WS-WRK-LR                   PIC S9(5)V99    VALUE +0 COMP-3.
       77  SAVE-BEN-INDEX              PIC S999    VALUE +0 COMP-3.
       77  ws-total-net-prem           pic s9(11)v99 comp-3 value +0.
       77  ws-avg                      pic s9(11)v99 comp-3 value +0.
       77  ws-trend                    pic s9v9(4) comp-3 value +0.
       77  ws-records-inserted         pic s9(9) value +0.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
      
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  sqlcmd                      pic x(1024).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
       01  ws-rec-cntr                 pic s9(4) comp value +0.
       01  ws-test-date                pic x(10) value spaces.
       01  ws-moe-date                 PIC X(10).

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to tell sql server that i am    ***
      ***  passing it a null value.  The indicator will be -1        ***
      ***  if the value is nulls and +0 if the value is other than   ***
      ***  nulls.  Here is a sample on how to use it.                ***
      ***                                                            ***
      ***      if db-date1 = spaces move -1 to nu-date1 end-if       *** 
      ***     EXEC SQL                                               ***
      ***        insert into TABLE_NAME (                            ***
      ***           date1,                                           ***
      ***           date2)                                           ***
      ***        values (                                            ***
      ***           :db-date1      :nu-date1,                        ***
      ***           :db-date2      :nu-date2)                        ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-LF-EXP-DT            PIC s9(4) comp value +0.
           05  nu-LF-CAN-DT            PIC s9(4) comp value +0.
           05  nu-LF-CAN-EXT-DT        PIC s9(4) comp value +0.
           05  nu-LF-DTH-DT            PIC s9(4) comp value +0.
           05  nu-LF-DTH-EXT-DT        PIC s9(4) comp value +0.
           05  nu-AH-EXP-DT            PIC s9(4) comp value +0.
           05  nu-AH-CAN-DT            PIC s9(4) comp value +0.
           05  nu-AH-CAN-EXT-DT        PIC s9(4) comp value +0.
           05  nu-AH-DIS-DT            PIC s9(4) comp value +0.

       01  EXTRACT-RECORD.
           05  EXT-MOE-DATE            PIC X(10).
           05  EXT-CARRIER             PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-STATE               PIC XX.
           05  EXT-ACCOUNT             PIC X(10).
           05  ext-prem-trend          pic -99.9999.
           05  ext-prem-trend-a redefines
               ext-prem-trend          pic x(8).
           05  ext-curr-12mos          pic -9(11).99.
           05  ext-curr-12mos-a redefinEs
               ext-curr-12mos          pic x(15).
           05  ext-prev-12mos          pic -9(11).99.
           05  ext-prev-12mos-a redefinEs
               ext-prev-12mos          pic x(15).
           05  ext-net-prem-01         pic -9(11).99.
           05  ext-net-prem-01-a redefines
               ext-net-prem-01         pic x(15).
           05  ext-net-prem-02         pic -9(11).99.
           05  ext-net-prem-02-a redefines
               ext-net-prem-02         pic x(15).
           05  ext-net-prem-03         pic -9(11).99.
           05  ext-net-prem-03-a redefines
               ext-net-prem-03         pic x(15).
           05  ext-net-prem-04         pic -9(11).99.
           05  ext-net-prem-04-a redefines
               ext-net-prem-04         pic x(15).
           05  ext-net-prem-05         pic -9(11).99.
           05  ext-net-prem-05-a redefines
               ext-net-prem-05         pic x(15).
           05  ext-net-prem-06         pic -9(11).99.
           05  ext-net-prem-06-a redefines
               ext-net-prem-06         pic x(15).
           05  ext-net-prem-07         pic -9(11).99.
           05  ext-net-prem-07-a redefines
               ext-net-prem-07         pic x(15).
           05  ext-net-prem-08         pic -9(11).99.
           05  ext-net-prem-08-a redefines
               ext-net-prem-08         pic x(15).
           05  ext-net-prem-09         pic -9(11).99.
           05  ext-net-prem-09-a redefines
               ext-net-prem-09         pic x(15).
           05  ext-net-prem-10         pic -9(11).99.
           05  ext-net-prem-10-a redefines
               ext-net-prem-10         pic x(15).
           05  ext-net-prem-11         pic -9(11).99.
           05  ext-net-prem-11-a redefines
               ext-net-prem-11         pic x(15).
           05  ext-net-prem-12         pic -9(11).99.
           05  ext-net-prem-12-a redefines
               ext-net-prem-12         pic x(15).
           05  ext-net-prem-13         pic -9(11).99.
           05  ext-net-prem-13-a redefines
               ext-net-prem-13         pic x(15).
           05  ext-net-prem-14         pic -9(11).99.
           05  ext-net-prem-14-a redefines
               ext-net-prem-14         pic x(15).
           05  ext-net-prem-15         pic -9(11).99.
           05  ext-net-prem-15-a redefines
               ext-net-prem-15         pic x(15).
           05  ext-net-prem-16         pic -9(11).99.
           05  ext-net-prem-16-a redefines
               ext-net-prem-16         pic x(15).
           05  ext-net-prem-17         pic -9(11).99.
           05  ext-net-prem-17-a redefines
               ext-net-prem-17         pic x(15).
           05  ext-net-prem-18         pic -9(11).99.
           05  ext-net-prem-18-a redefines
               ext-net-prem-18         pic x(15).
           05  ext-net-prem-19         pic -9(11).99.
           05  ext-net-prem-19-a redefines
               ext-net-prem-19         pic x(15).
           05  ext-net-prem-20         pic -9(11).99.
           05  ext-net-prem-20-a redefines
               ext-net-prem-20         pic x(15).
           05  ext-net-prem-21         pic -9(11).99.
           05  ext-net-prem-21-a redefines
               ext-net-prem-21         pic x(15).
           05  ext-net-prem-22         pic -9(11).99.
           05  ext-net-prem-22-a redefines
               ext-net-prem-22         pic x(15).
           05  ext-net-prem-23         pic -9(11).99.
           05  ext-net-prem-23-a redefines
               ext-net-prem-23         pic x(15).
           05  ext-net-prem-24         pic -9(11).99.
           05  ext-net-prem-24-a redefines
               ext-net-prem-24         pic x(15).
           05  ext-net-prem-25         pic -9(11).99.
           05  ext-net-prem-25-a redefines
               ext-net-prem-25         pic x(15).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  WS-CURRENT-KEY.
           05  WS-CONTROL              PIC X(19)  VALUE LOW-VALUES.

       01  WS-PREVIOUS-KEY.
           05  WS-PREV-CARRIER         PIC X.
           05  WS-PREV-GROUP           PIC X(6).
           05  WS-PREV-STATE           PIC XX.
           05  WS-PREV-ACCOUNT         PIC X(10).

       01  WS-PREV-ACCT-STUFF.
           05  PRV-REPORT-CDE1         PIC X(10)  VALUE SPACES.
           05  PRV-REPORT-CDE2         PIC X(10)  VALUE SPACES.
           05  PRV-BUS-CLASS           PIC XX     VALUE SPACES.
           05  PRV-HI-CERT-DT          PIC 9(11)  VALUE ZEROS.
           05  PRV-LO-CERT-DT          PIC 9(11)  VALUE 999999999.
           05  PRV-ACCT-STATUS         PIC X      VALUE SPACES.
           05  PRV-ACCT-NAME           PIC X(30)  VALUE SPACES.

       01  WS-DISPLAY-AMT              PIC ZZZZZZZZZ.ZZ.
      *  FIRST LEVEL
      *     1 = ITD
      *     2 = MTD
      *     3 = YTD
      *     4 = L12
      *     5 = PREVIOUS L12
      *     6 = PREVIOUS YTD
      *     THE REST ARE FOR FUTURE USE

       01  WS-DATE-TABLE.
           05  FILLER OCCURS 26.
               10  WS-TABLE-DATE      PIC 9(8) VALUE ZEROS.

      * occurenc 1 = itd, 2 = last months itd, etc.

       01  ws-curr-12mos               pic s9(11)v99 comp-3 value +0.
       01  ws-prev-12mos               pic s9(11)v99 comp-3 value +0.
       01  ws-curr-6mos                pic s9(11)v99 comp-3 value +0.
       01  ws-prev-6mos                pic s9(11)v99 comp-3 value +0.

       01  WS-ACCUM-TOTALS.
           05  FILLER OCCURS 26.
               10  WS-net-prm          PIC S9(11)V99   COMP-3.
               10  WS-EARN-PRM         PIC S9(11)V99   COMP-3.
               10  WS-PAID-CLMS        PIC S9(11)V99   COMP-3.


       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WS-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.

       01  DATE-AREAS.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC X(4).
               10  WS-WORK-CCYY-N REDEFINES WS-WORK-CCYY
                                       PIC 9(4).
               10  WS-WORK-MM          PIC XX.
               10  WS-WORK-DD          PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       LINKAGE SECTION.                                                 
       01  var                         pic x(30).

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.
       0002-INPUT.

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if

           display ' KIXSYS = ' ws-kix-myenv

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           perform 0025-check-for-rerun thru 0025-exit
           if ws-rec-cntr > 0
              display ' This must be a re-run, about to delete '
              perform 1045-delete-rows thru 1045-exit
           end-if

           move ws-moe-date            to ext-moe-date
           PERFORM 0075-READ-ERACCT    THRU 0075-EXIT
           MOVE AM-REPORT-CODE-1       TO PRV-REPORT-CDE1
           MOVE AM-REPORT-CODE-2       TO PRV-REPORT-CDE2
           MOVE AM-HI-CERT-DATE        TO PRV-HI-CERT-DT
           MOVE AM-LO-CERT-DATE        TO PRV-LO-CERT-DT
           MOVE AM-GPCD                TO PRV-BUS-CLASS
           MOVE AM-STATUS              TO PRV-ACCT-STATUS
           MOVE AM-NAME                TO PRV-ACCT-NAME

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT
           MOVE WS-CURRENT-KEY         TO WS-PREVIOUS-KEY

           PERFORM 0077-PROCESS-EPEC   THRU 0077-EXIT UNTIL
                 (END-OF-EPEC)
      *          OR (EPEC-IN-CNT > 5000)

           PERFORM 0090-BUILD-EXTRACT  THRU 0090-EXIT
           PERFORM 1050-FINISH-UP      THRU 1050-EXIT
           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT
           GOBACK
           .
       0002-EXIT.
           EXIT.

       0010-INITIALIZE.

           PERFORM 0015-INIT-TABLE     THRU 0015-EXIT
           MOVE LOW-VALUES             TO WS-PREVIOUS-KEY
                                          WS-CURRENT-KEY

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO WS-table-date (1)
              move dc-greg-date-a-edit to ws-moe-date
              string
                 dc-greg-date-a-edit (7:4) '.'
                 dc-greg-date-a-edit (1:2) '.'
                 dc-greg-date-a-edit (4:2)
                    delimited by size into ws-test-date
              end-string
              display ' test date ' ws-test-date
           ELSE
              DISPLAY ' Current date  ERROR '
              PERFORM ABEND-PGM
           END-IF

           move -1 to w1
           perform varying d1 from +2 by +1 until d1 > +26
              MOVE BIN-RUN-DATE        TO DC-BIN-DATE-1
              MOVE w1                  TO DC-ELAPSED-MONTHS
              MOVE +0                  TO DC-ELAPSED-DAYS
              MOVE '6'                 TO DC-OPTION-CODE
              MOVE '1'                 TO DC-END-OF-MONTH
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD
                                       TO WS-table-date (d1)
              ELSE
                 DISPLAY ' Date Error ' d1 ' ' w1
                 PERFORM ABEND-PGM
              END-IF
              subtract 1 from w1
           end-perform

           perform varying d1 from +1 by +1 until d1 > +26
              display ' date ' d1 ' ' ws-table-date (d1)
           end-perform

           MOVE +1                     TO S1
           PERFORM 1000-CONNECT        THRU 1000-EXIT
      *    PERFORM 1010-drop-table     THRU 1010-EXIT
      *    perform 1020-truncate-table thru 1020-exit
      *    PERFORM 1030-create-table   THRU 1030-EXIT

           .
       0010-EXIT.
           EXIT.

       0015-INIT-TABLE.

           PERFORM VARYING WS-S2 FROM +1 BY +1 UNTIL
              WS-S2 > +26
              MOVE +0                  TO WS-net-prm (WS-S2)
                                          ws-earn-prm (ws-s2)
                                          WS-PAID-CLMS (WS-S2)
           END-PERFORM

           .
       0015-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EPECS ERACCTT

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' BAD OPEN FOR ERACCT ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0025-check-for-rerun.

           exec sql
              SELECT
                 MOE_DATE,
                 Count(*)
              INTO
                 :EXT-MOE-DATE,
                 :WS-REC-CNTR
              FROM
                 TREND_ANALYSIS
              GROUP BY MOE_DATE
              HAVING convert(varchar(10),MOE_DATE,102)
                    = :ws-test-date
           end-exec

           if sqlcode not = 0 and 1
              display "Error : check for rerun  "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           display ' counter ' ws-rec-cntr

           .
       0025-exit.
           exit.

       0030-CLOSE-FILES.

           DISPLAY ' EPEC IN RECORDS  ' EPEC-IN-CNT
           DISPLAY ' EXTR OUT RECORDS ' EXTR-OUT-CNT
           CLOSE EPECS ERACCTT

           .
       0030-EXIT.
           EXIT.

       0060-READ-EPEC.

           READ EPECS AT END
               SET END-OF-EPEC         TO TRUE
           END-READ

           IF NOT END-OF-EPEC
              IF EP-REIN = 'R'
                 GO TO 0060-READ-EPEC
              END-IF
              ADD 1                    TO EPEC-IN-CNT
              MOVE EP-CONTROL          TO WS-CONTROL
              PERFORM 0070-MATCH-TO-ERACCT
                                       THRU 0070-EXIT
              PERFORM 0062-SEARCH-BENEFIT-TABLE
                                       THRU 0069-EXIT
           END-IF

           .
       0060-EXIT.
           EXIT.

       0062-SEARCH-BENEFIT-TABLE.

           IF EP-RCD-TYPE = AH-OVERRIDE-L1
              MOVE CLAS-STARTA         TO CLAS-INDEXA
              GO TO 0064-AH-LOOP
           END-IF

           MOVE CLAS-STARTL            TO CLAS-INDEXL

           .
       0063-LIFE-LOOP.

           IF (CLAS-INDEXL > CLAS-MAXL)
              OR (CLAS-INDEXL = +0)
              DISPLAY 'INVALID LIFE BENEFIT TYPE - ' EP-BEN-CODE
              MOVE '**** INVALID LIFE BENEFIT TYPE ****'
                                       TO WS-ABEND-MESSAGE
              MOVE '0'                 TO WAC-1
              MOVE '4'                 TO WAC-2
              MOVE '01'                TO WAC-3-4
              MOVE WS-ABEND-CODE       TO WS-RETURN-CODE
              PERFORM ABEND-PGM
           END-IF

           IF EP-BEN-CODE = CLAS-I-BEN (CLAS-INDEXL)
              MOVE CLAS-I-EP (CLAS-INDEXL)
                                       TO WS-EP-CODE
              MOVE CLAS-INDEXL         TO SAVE-BEN-INDEX
              GO TO 0065-SET-EP-CODE
           ELSE
              ADD +1 TO CLAS-INDEXL
              GO TO 0063-LIFE-LOOP
           END-IF

           .
       0064-AH-LOOP.

           IF (CLAS-INDEXA > CLAS-MAXA)
              OR (CLAS-INDEXA = +0)
              DISPLAY 'INVALID AH BENEFIT TYPE - ' EP-BEN-CODE
              MOVE '**** INVALID AH BENEFIT TYPE ****'
                                       TO WS-ABEND-MESSAGE
              MOVE '0'                 TO WAC-1
              MOVE '4'                 TO WAC-2
              MOVE '01'                TO WAC-3-4
              MOVE WS-ABEND-CODE       TO WS-RETURN-CODE
              PERFORM ABEND-PGM
           END-IF

           IF EP-BEN-CODE = CLAS-I-BEN (CLAS-INDEXA)
              MOVE CLAS-I-EP (CLAS-INDEXA)
                                       TO WS-EP-CODE
              MOVE CLAS-INDEXA         TO SAVE-BEN-INDEX
              GO TO 0065-SET-EP-CODE
           ELSE
              ADD +1 TO CLAS-INDEXA
              GO TO 0064-AH-LOOP
           END-IF

           .
       0065-SET-EP-CODE.

           IF EP-STATE = 'WY'
              MOVE 'P'                 TO WS-EP-CODE
           END-IF

           .
       0069-EXIT.
           EXIT.

       0070-MATCH-TO-ERACCT.
       
           IF (AM-CONTROL-A = EP-CNTRL-1)
              AND (AM-EXPIRE-DT = EP-EXP-DTE)
              AND (AM-EFFECT-DT = EP-EFF-DTE)
              GO TO 0070-EXIT
           ELSE
              IF AM-CONTROL-A > EP-CNTRL-1
                 DISPLAY 'EPEC AND ERACCT MESSED UP '
                 DISPLAY ' EPEC ' EP-CNTRL-1
                 DISPLAY ' ACCT ' AM-CONTROL-A
                 PERFORM ABEND-PGM
              ELSE
                 PERFORM 0075-READ-ERACCT 
                                       THRU 0075-EXIT
                 GO TO 0070-MATCH-TO-ERACCT
              END-IF
           END-IF
           
           .
       0070-EXIT.
           EXIT.
           
       0075-READ-ERACCT.
       
           IF NOT END-OF-ERACCT
              READ ERACCTT
           END-IF
           
           IF ERACCT-FILE-STATUS = '10' OR '23'
              SET END-OF-ERACCT        TO TRUE
              MOVE HIGH-VALUES         TO AM-CONTROL-A
           ELSE
              IF ERACCT-FILE-STATUS = '00'
                 CONTINUE
              ELSE
                 DISPLAY ' BAD READ ON ERACCTT ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
           
           .
       0075-EXIT.
           EXIT.
           
       0077-PROCESS-EPEC.

           IF EP-RECORD-ID = 'EP'
              PERFORM 0080-PROCESS-EPEC
                                       THRU 0080-EXIT
           END-IF

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT

           .
       0077-EXIT.
           EXIT.

       0080-PROCESS-EPEC.

           IF WS-CURRENT-KEY NOT = WS-PREVIOUS-KEY
              PERFORM 0090-BUILD-EXTRACT
                                       THRU 0090-EXIT
              PERFORM 0015-INIT-TABLE  THRU 0015-EXIT
              MOVE 99999999999         TO PRV-LO-CERT-DT
              MOVE WS-CURRENT-KEY      TO WS-PREVIOUS-KEY
           END-IF

           MOVE AM-REPORT-CODE-1       TO PRV-REPORT-CDE1
           MOVE AM-REPORT-CODE-2       TO PRV-REPORT-CDE2
           MOVE AM-HI-CERT-DATE        TO PRV-HI-CERT-DT
           IF (AM-LO-CERT-DATE < PRV-LO-CERT-DT)
              OR (PRV-LO-CERT-DT = ZERO)
              MOVE AM-LO-CERT-DATE     TO PRV-LO-CERT-DT
           END-IF
      *    MOVE AM-LO-CERT-DATE        TO PRV-LO-CERT-DT
           MOVE AM-GPCD                TO PRV-BUS-CLASS
           MOVE AM-STATUS              TO PRV-ACCT-STATUS
           MOVE AM-NAME                TO PRV-ACCT-NAME

           IF EP-RECORD-ID = 'EP'
              perform varying d1 from +1 by +1 until d1 > +26
                 if (ep-run-dte = ws-table-date (d1))
                              or
                    ((ep-run-dte < ws-table-date (d1))
                    and (ep-purge = 'P'))
                    PERFORM 0082-ACCUM-EP-TOTS
                                       THRU 0082-EXIT
                 end-if
              end-perform
           end-if

           .
       0080-EXIT.
           EXIT.

       0082-ACCUM-EP-TOTS.

           compute ws-net-prm (d1) =
              ws-net-prm (d1) + (ep-iss-prm - ep-cnc-prm)
           compute ws-paid-clms (d1) =
              ws-paid-clms (d1) + ep-clm-amt

           EVALUATE TRUE
              WHEN WS-EP-CODE = 'R' OR 'N' OR 'U' OR 'A' OR 'T'
                 MOVE EP-PRM-78        TO WS-ERN-PRM
              WHEN WS-EP-CODE = 'P'
                 MOVE EP-PRM-PR        TO WS-ERN-PRM
              WHEN WS-EP-CODE = 'B' OR 'K' OR 'L'
                 MOVE EP-PRM-ST        TO WS-ERN-PRM
              WHEN WS-EP-CODE = 'M'
                 COMPUTE WS-ERN-PRM ROUNDED =
                    (EP-PRM-PR + EP-PRM-78)  *  +.5
              WHEN WS-EP-CODE = '1'
                 COMPUTE WS-ERN-PRM ROUNDED =
                    (EP-PRM-PR * +.6667 ) + (EP-PRM-78 * +.3333)
              WHEN WS-EP-CODE = '2'
                 COMPUTE WS-ERN-PRM ROUNDED =
                    (EP-PRM-PR * +.80 ) + (EP-PRM-78 * +.20)
              WHEN OTHER
                 MOVE EP-PRM-78        TO WS-ERN-PRM
                 DISPLAY ' USING DEFAULT EP-CODE ' WS-EP-CODE
           END-EVALUATE

           COMPUTE WS-EARN-PRM (d1) =
              WS-EARN-PRM (d1) + WS-ERN-PRM

           .
       0082-EXIT.
           EXIT.


       0090-BUILD-EXTRACT.

           MOVE WS-PREV-CARRIER        TO EXT-CARRIER
           MOVE WS-PREV-GROUP          TO EXT-GROUP
           MOVE WS-PREV-STATE          TO EXT-STATE
           MOVE WS-PREV-ACCOUNT        TO EXT-ACCOUNT

           PERFORM 0095-MOVE-TABLE     THRU 0095-EXIT
           PERFORM 1040-insert-row     THRU 1040-EXIT

           .
       0090-EXIT.
           EXIT.

       0095-MOVE-TABLE.

           move zeros                  to ws-total-net-prem
                                          ws-avg
                                          ws-trend

           compute ext-net-prem-01 = ws-net-prm (01) - ws-net-prm (02)
           compute ext-net-prem-02 = ws-net-prm (02) - ws-net-prm (03)
           compute ext-net-prem-03 = ws-net-prm (03) - ws-net-prm (04)
           compute ext-net-prem-04 = ws-net-prm (04) - ws-net-prm (05)
           compute ext-net-prem-05 = ws-net-prm (05) - ws-net-prm (06)
           compute ext-net-prem-06 = ws-net-prm (06) - ws-net-prm (07)
           compute ext-net-prem-07 = ws-net-prm (07) - ws-net-prm (08)
           compute ext-net-prem-08 = ws-net-prm (08) - ws-net-prm (09)
           compute ext-net-prem-09 = ws-net-prm (09) - ws-net-prm (10)
           compute ext-net-prem-10 = ws-net-prm (10) - ws-net-prm (11)
           compute ext-net-prem-11 = ws-net-prm (11) - ws-net-prm (12)
           compute ext-net-prem-12 = ws-net-prm (12) - ws-net-prm (13)
           compute ext-net-prem-13 = ws-net-prm (13) - ws-net-prm (14)
           compute ext-net-prem-14 = ws-net-prm (14) - ws-net-prm (15)
           compute ext-net-prem-15 = ws-net-prm (15) - ws-net-prm (16)
           compute ext-net-prem-16 = ws-net-prm (16) - ws-net-prm (17)
           compute ext-net-prem-17 = ws-net-prm (17) - ws-net-prm (18)
           compute ext-net-prem-18 = ws-net-prm (18) - ws-net-prm (19)
           compute ext-net-prem-19 = ws-net-prm (19) - ws-net-prm (20)
           compute ext-net-prem-20 = ws-net-prm (20) - ws-net-prm (21)
           compute ext-net-prem-21 = ws-net-prm (21) - ws-net-prm (22)
           compute ext-net-prem-22 = ws-net-prm (22) - ws-net-prm (23)
           compute ext-net-prem-23 = ws-net-prm (23) - ws-net-prm (24)
           compute ext-net-prem-24 = ws-net-prm (24) - ws-net-prm (25)
           compute ext-net-prem-25 = ws-net-prm (25) - ws-net-prm (26)

           compute ws-curr-6mos =  ws-net-prm (1) - ws-net-prm (7)
           compute ws-prev-6mos = ws-net-prm (7) - ws-net-prm (13)

           compute ws-curr-12mos =  ws-net-prm (1) - ws-net-prm (13)
           move ws-curr-12mos          to ext-curr-12mos
           compute ws-prev-12mos = ws-net-prm (13) - ws-net-prm (25)
           move ws-prev-12mos          to ext-prev-12mos

           perform varying s1 from +1 by +1 until s1 > +25
              compute ws-total-net-prem =
                 ws-total-net-prem + (ws-net-prm (s1) -
                 ws-net-prm (s1 + 1))
           end-perform
           display ' Total ' ext-state ' ' ext-account ' '
              ws-total-net-prem

           evaluate true
              when (ws-curr-12mos > zeros)
                 and (ws-prev-12mos > zeros)
                 compute ws-trend rounded =
                    (ws-curr-12mos - ws-prev-12mos) / ws-curr-12mos
              when (ws-curr-12mos < zeros)
                 and (ws-prev-12mos > zeros)
                 compute ws-trend rounded =
                    (ws-curr-12mos - ws-prev-12mos) /
                       ws-curr-12mos * -1
              when (ws-curr-6mos > zeros)
                 and (ws-prev-6mos > zeros)
                 compute ws-trend rounded =
                    (ws-curr-6mos - ws-prev-6mos) / ws-curr-6mos
              when (ws-curr-6mos < zeros)
                 and (ws-prev-6mos > zeros)
                 compute ws-trend rounded =
                    (ws-curr-6mos - ws-prev-6mos) /
                       ws-curr-6mos * -1
              when (ws-curr-12mos > zeros)
                 and (ws-prev-12mos <= zeros)
                 compute ws-trend rounded =
                    (ws-curr-12mos - ws-prev-12mos) / ws-curr-12mos
              when (ws-curr-12mos = zeros)
                 and (ws-prev-12mos > zeros)
                 move -01.1111         to ws-trend
              when (ws-curr-12mos < zeros)
                 and (ws-prev-12mos = zeros)
                 move -01.1111         to ws-trend
              when (ws-curr-12mos < zeros)
                 and (ws-prev-12mos < zeros)
                 move -00.1111         to ws-trend
              when other
                 move +55.5555         to ws-trend
           end-evaluate

      *    compute ws-avg = ws-total-net-prem / 25
      *
      *    if (ws-net-prm (1) - ws-net-prm (2)) = zeros
      *       move -99.9999             to ws-trend
      *    else
      *       compute ws-trend rounded = ((ws-net-prm (1)
      *          - ws-net-prm (2)) - ws-avg) / (ws-net-prm (1) -
      *          ws-net-prm (2))
      *    end-if

           move ws-trend               to ext-prem-trend
           .
       0095-EXIT.
           EXIT.
           
       1000-connect.

           display ' about to connect to Logic '

      ****  The below code is for when the db is still
      ****  on sql server 2008 R2
           evaluate ws-kix-myenv
              when 'cid1p'
                 move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
              when 'mdoff'
                 move '//hov-tstdb01.cso.local:55330;'
                                       to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate


           move 'Logic'                to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc


NTTDel**       CONNECT TO :svr
NTTDel**             USER :usr-pass
NTTIns*        CONNECT TO :svr
NTTIns*          USER     :usr
NTTIns*          USING    :pass
      *     END-EXEC

           if sqlcode not = 0
              display "Error: cannot connect to Logic"
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              perform abend-pgm
           end-if

           .
       1000-exit.
           exit.

       1010-drop-table.

           display 'Begin Drop table'
           EXEC SQL
              drop table TREND_ANALYSIS
           END-EXEC
           if sqlcode not = 0
              display "Error(anticipated) : cannot drop table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       1010-exit.
           exit.

       1020-truncate-table.

           display 'Begin Truncate table'
           EXEC SQL
NTTDel*        truncate table TREND_ANALYSIS
NTTIns         EXECUTE IMMEDIATE 'truncate table TREND_ANALYSIS'
           END-EXEC

           if sqlcode not = 0
              display "Error : cannot truncate table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       1020-exit.
           exit.

       1030-create-table.

           display ' Begin Create table '

           EXEC SQL
              CREATE TABLE TREND_ANALYSIS(
                 MOE_DATE           datetime NOT NULL,
                 CARRIER            char(1) NOT NULL,
                 GROUPING           char(6) NOT NULL,
                 STATE              char(2) NOT NULL,
                 ACCOUNT            char(10) NOT NULL,
                 NET_PREM_TREND     decimal(06, 4) null,
                 CURR_12MOS         decimal(11, 2) NULL,
                 PREV_12MOS         decimal(11, 2) NULL,
                 NET_PREM           decimal(11, 2) NULL,
                 NET_PREM_1         decimal(11, 2) NULL,
                 NET_PREM_2         decimal(11, 2) NULL,
                 NET_PREM_3         decimal(11, 2) NULL,
                 NET_PREM_4         decimal(11, 2) NULL,
                 NET_PREM_5         decimal(11, 2) NULL,
                 NET_PREM_6         decimal(11, 2) NULL,
                 NET_PREM_7         decimal(11, 2) NULL,
                 NET_PREM_8         decimal(11, 2) NULL,
                 NET_PREM_9         decimal(11, 2) NULL,
                 NET_PREM_10        decimal(11, 2) NULL,
                 NET_PREM_11        decimal(11, 2) NULL,
                 NET_PREM_12        decimal(11, 2) NULL,
                 NET_PREM_13        decimal(11, 2) NULL,
                 NET_PREM_14        decimal(11, 2) NULL,
                 NET_PREM_15        decimal(11, 2) NULL,
                 NET_PREM_16        decimal(11, 2) NULL,
                 NET_PREM_17        decimal(11, 2) NULL,
                 NET_PREM_18        decimal(11, 2) NULL,
                 NET_PREM_19        decimal(11, 2) NULL,
                 NET_PREM_20        decimal(11, 2) NULL,
                 NET_PREM_21        decimal(11, 2) NULL,
                 NET_PREM_22        decimal(11, 2) NULL,
                 NET_PREM_23        decimal(11, 2) NULL,
                 NET_PREM_24        decimal(11, 2) NULL,
                 NET_PREM_25        decimal(11, 2) NULL
                 CONSTRAINT PK_TREND_ANALYSIS PRIMARY KEY CLUSTERED
                   (MOE_DATE, CARRIER, GROUPING, STATE, ACCOUNT)
                   )                                             
           END-EXEC                                              

           if sqlcode not = 0
              display "Error: cannot create table "
              move sqlcode             to ws-disp-code
              display ' sql return code ' ws-disp-code
              display ' sql err mess    ' sqlerrmc
              PERFORM 1050-FINISH-UP   THRU 1050-EXIT
              PERFORM ABEND-PGM
           end-if

           .
       1030-exit.
           exit.

       1040-INSERT-ROW.

           if (ws-curr-12mos = 0)
              and (ws-prev-12mos = 0)
              go to 1040-exit
           end-if

           EXEC SQL
              insert into TREND_ANALYSIS (
                 MOE_DATE           ,
                 CARRIER            ,
                 GROUPING           ,
                 STATE              ,
                 ACCOUNT            ,
                 NET_PREM_TREND     ,
                 CURR_12MOS         ,
                 PREV_12MOS         ,
                 net_prem           ,
                 NET_PREM_1         ,
                 NET_PREM_2         ,
                 NET_PREM_3         ,
                 NET_PREM_4         ,
                 NET_PREM_5         ,
                 NET_PREM_6         ,
                 NET_PREM_7         ,
                 NET_PREM_8         ,
                 NET_PREM_9         ,
                 NET_PREM_10        ,
                 NET_PREM_11        ,
                 NET_PREM_12        ,
                 NET_PREM_13        ,
                 NET_PREM_14        ,
                 NET_PREM_15        ,
                 NET_PREM_16        ,
                 NET_PREM_17        ,
                 NET_PREM_18        ,
                 NET_PREM_19        ,
                 NET_PREM_20        ,
                 NET_PREM_21        ,
                 NET_PREM_22        ,
                 NET_PREM_23        ,
                 NET_PREM_24)
               values (
                 :EXT-MOE-DATE        ,
                 :EXT-CARRIER         ,
                 :EXT-GROUP           ,
                 :EXT-STATE           ,
                 :EXT-ACCOUNT         ,
                 :ext-prem-trend-a    ,
                 :EXT-CURR-12MOS-A    ,
                 :EXT-PREV-12MOS-A    ,
                 :EXT-net-prem-01-a   ,
                 :EXT-net-prem-02-a   ,
                 :EXT-net-prem-03-a   ,
                 :EXT-net-prem-04-a   ,
                 :EXT-net-prem-05-a   ,
                 :EXT-net-prem-06-a   ,
                 :EXT-net-prem-07-a   ,
                 :EXT-net-prem-08-a   ,
                 :EXT-net-prem-09-a   ,
                 :EXT-net-prem-10-a   ,
                 :EXT-net-prem-11-a   ,
                 :EXT-net-prem-12-a   ,
                 :EXT-net-prem-13-a   ,
                 :EXT-net-prem-14-a   ,
                 :EXT-net-prem-15-a   ,
                 :EXT-net-prem-16-a   ,
                 :EXT-net-prem-17-a   ,
                 :EXT-net-prem-18-a   ,
                 :EXT-net-prem-19-a   ,
                 :EXT-net-prem-20-a   ,
                 :EXT-net-prem-21-a   ,
                 :EXT-net-prem-22-a   ,
                 :EXT-net-prem-23-a   ,
                 :EXT-net-prem-24-a   ,
                 :ext-net-prem-25-a)
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' in out cnt ' epec-in-cnt ' '
                 ws-records-inserted
              display ' offending rec ' extract-record
              PERFORM 1050-FINISH-UP   THRU 1050-EXIT
              PERFORM ABEND-PGM
           end-if

           add 1 to ws-records-inserted

           .
       1040-EXIT.
           EXIT.

       1045-delete-rows.

           exec sql delete
              FROM
                 TREND_ANALYSIS
              where convert(varchar(10),MOE_DATE,102)
                    = :ws-test-date
           end-exec

           if sqlcode not = 0 and 1
              display "Error : deleting rows  "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
111317        go to 1045-exit
           end-if

           EXEC SQL
               commit
           END-EXEC

           if sqlcode not = 0 and 1
              display "Error : commit of delete  "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       1045-exit.
           exit.

       1050-finish-up.

           EXEC SQL
NTTDel*        commit work release
NTTIns         commit work
           END-EXEC

           if sqlcode not = 0
              display "Error: commit work release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           EXEC SQL
              DISCONNECT
           END-EXEC

           if sqlcode not = 0
              display "Error: disconnect  "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       1050-exit.
           exit.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

