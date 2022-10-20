       PROGRAM-ID.    CIDEPX2.
       AUTHOR.        PABLO.
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
061908* 061908    2007071700002  PEMA  ADD CLP PREMIUM
031811* 031811 CR2011012700001   PEMA  ADD ACCT STATUS S - SUSPENDED
021916* 021916 CR2014010900001   TANA  ADD ACCT STATUS D,L,R,P
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

           SELECT EXTRACT              ASSIGN TO SYS011.
      *       ORGANIZATION IS LINE SEQUENTIAL.
                                                                        
              

       DATA DIVISION.
       FILE SECTION.

       FD  EPECS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
                                       COPY ECSEPC01.

           EJECT
       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERACCTT.    
                                                                        
                                       COPY ERCACCT.                        

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

      *01  EXTRACT-HEAD-OUT            PIC X(480).
       01  EXTRACT-RECORD-OUT          PIC X(698).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDEPX2 WORKING STORAGE    '.
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
       77  WS-LAST-MONTH-DT            PIC 9(11)  VALUE ZEROS.
       77  WS-LAST-YEAR-END-DT         PIC 9(11)  VALUE ZEROS.
       77  WS-ONE-YEAR-AGO-DT          PIC 9(11)  VALUE ZEROS.
       77  WS-TWO-YEARS-AGO-DT         PIC 9(11)  VALUE ZEROS.
       77  WS-TWO-YEAR-ENDS-AGO-DT     PIC 9(11)  VALUE ZEROS.
       77  A1                          PIC S9(5)  VALUE +0 COMP-3.
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
       77  WS-COMM-PCT                 PIC S9(5)V99    VALUE +0 COMP-3.
       77  WS-WRK-LR-LO         PIC S9(5)V99    VALUE  -999.99 COMP-3.
       77  WS-WRK-LR-HI         PIC S9(5)V99    VALUE  +999.99 COMP-3.
       77  WS-WRK-LR                   PIC S9(5)V99    VALUE +0 COMP-3.
       77  SAVE-BEN-INDEX              PIC S999    VALUE +0 COMP-3.
       01  WS-CURRENT-KEY.
           05  WS-CONTROL              PIC X(19)  VALUE LOW-VALUES.
           05  WS-RPTCD1               PIC X(10)  VALUE LOW-VALUES.
           05  WS-RPTCD2               PIC X(10)  VALUE LOW-VALUES.
           05  WS-RPTCD3               PIC X(10)  VALUE LOW-VALUES.

       01  WS-PREVIOUS-KEY.
           05  WS-PREV-CARRIER         PIC X.
           05  WS-PREV-GROUP           PIC X(6).
           05  WS-PREV-STATE           PIC XX.
           05  WS-PREV-ACCOUNT         PIC X(10).
           05  WS-PREV-RPTCD1          PIC X(10).
           05  WS-PREV-RPTCD2          PIC X(10).
           05  WS-PREV-RPTCD3          PIC X(10).


       01  WS-PREV-ACCT-STUFF.
           05  PRV-REPORT-CDE1         PIC X(10)  VALUE SPACES.
           05  PRV-REPORT-CDE2         PIC X(10)  VALUE SPACES.
           05  PRV-REPORT-CDE3         PIC X(10)  VALUE SPACES.
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

       01  WS-ACCUM-TOTALS.
           05  FILLER OCCURS 10.
               10  WS-NET-PRM          PIC S9(9)V99   COMP-3.
               10  WS-EARN-PRM         PIC S9(9)V99   COMP-3.
               10  WS-CLM-INC          PIC S9(9)V99   COMP-3.
               10  WS-NET-COMM         PIC S9(9)V99   COMP-3.
               10  WS-EARN-COMM        PIC S9(9)V99   COMP-3.
               10  WS-PAID-CLMS        PIC S9(9)V99   COMP-3.
               10  WS-CLP              PIC S9(9)V99   COMP-3.

       01  HEAD-RECORD.
           05  FILLER                 PIC X(4)    VALUE 'CARR'.
           05  HD-TAB1                PIC X.
           05  FILLER                 PIC X(5)    VALUE 'GROUP'.
           05  HD-TAB2                PIC X.
           05  FILLER                 PIC X(5)    VALUE 'STATE'.
           05  HD-TAB3                PIC X.
           05  FILLER                 PIC X(7)    VALUE 'ACCOUNT'.
           05  HD-TAB4                PIC X.
           05  FILLER                 PIC X(11)   VALUE 'REPORT CDE1'.
           05  HD-TAB5                PIC X.
           05  FILLER                 PIC X(11)   VALUE 'REPORT CDE2'.
           05  HD-TAB6                PIC X.
           05  FILLER                 PIC X(11)   VALUE 'REPORT CDE3'.
           05  HD-TAB6A               PIC X.
           05  FILLER                 PIC X(14) VALUE 'MONTH END DATE'.
           05  HD-TAB7                PIC X.
           05  FILLER                 PIC X(12)   VALUE 'MTD NET PREM'.
           05  HD-TAB8                PIC X.
           05  FILLER                 PIC X(13)   VALUE 'MTD EARN PREM'.
           05  HD-TAB9                PIC X.
           05  FILLER                 PIC X(12)   VALUE 'MTD INC CLMS'.
           05  HD-TAB10               PIC X.
           05  FILLER                 PIC X(14) VALUE 'MTD LOSS RATIO'.
           05  HD-TAB11               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'ITD NET PREM'.
           05  HD-TAB12               PIC X.
           05  FILLER                 PIC X(13)   VALUE 'ITD EARN PREM'.
           05  HD-TAB13               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'ITD INC CLMS'.
           05  HD-TAB14               PIC X.
           05  FILLER                 PIC X(14) VALUE 'ITD LOSS RATIO'.
           05  HD-TAB15               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'YTD NET PREM'.
           05  HD-TAB16               PIC X.
           05  FILLER                 PIC X(13)   VALUE 'YTD EARN PREM'.
           05  HD-TAB17               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'YTD INC CLMS'.
           05  HD-TAB18               PIC X.
           05  FILLER                 PIC X(14) VALUE 'YTD LOSS RATIO'.
           05  HD-TAB19               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'L12 NET PREM'.
           05  HD-TAB20               PIC X.
           05  FILLER                 PIC X(13)   VALUE 'L12 EARN PREM'.
           05  HD-TAB21               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'L12 INC CLMS'.
           05  HD-TAB22               PIC X.
           05  FILLER                 PIC X(14) VALUE 'L12 LOSS RATIO'.
           05  HD-TAB23               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'P12 NET PREM'.
           05  HD-TAB24               PIC X.
           05  FILLER                 PIC X(13)   VALUE 'P12 EARN PREM'.
           05  HD-TAB25               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'P12 INC CLMS'.
           05  HD-TAB26               PIC X.
           05  FILLER                 PIC X(14) VALUE 'P12 LOSS RATIO'.
           05  HD-TAB27               PIC X.
           05  FILLER                 PIC X(13)  VALUE 'PYTD NET PREM'.
           05  HD-TAB28               PIC X.
           05  FILLER                 PIC X(14)  VALUE 'PYTD EARN PREM'.
           05  HD-TAB29               PIC X.
           05  FILLER                 PIC X(13)  VALUE 'PYTD INC CLMS'.
           05  HD-TAB30               PIC X.
           05  FILLER                PIC X(15) VALUE 'PYTD LOSS RATIO'.
           05  HD-TAB31               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'LO CERT DT'.
           05  HD-TAB32               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'HI CERT DT'.    
           05  HD-TAB33               PIC X.
           05  FILLER                 PIC X(9)    VALUE 'BUS CLASS'.
           05  HD-TAB34               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'ACCT STATUS'.
           05  HD-TAB35               PIC X.
           05  FILLER                 PIC X(9)    VALUE 'ACCT NAME'.
           05  HD-TAB36               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'MTD NET COMM'.
           05  HD-TAB37               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'MTD COMM %'.
           05  HD-TAB38               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'ITD NET COMM'.
           05  HD-TAB39               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'ITD COMM %'.
           05  HD-TAB40               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'YTD NET COMM'.
           05  HD-TAB41               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'YTD COMM %'.
           05  HD-TAB42               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'L12 NET COMM'.
           05  HD-TAB43               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'L12 COMM %'.
           05  HD-TAB44               PIC X.
           05  FILLER                 PIC X(12)   VALUE 'P12 NET COMM'.
           05  HD-TAB45               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'P12 COMM %'.
           05  HD-TAB46               PIC X.
           05  FILLER                 PIC X(13)   VALUE 'PYTD NET COMM'.
           05  HD-TAB47               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'PYTD COMM %'.
           05  HD-TAB48               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'MTD CLAIMS'.
           05  HD-TAB49               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'ITD CLAIMS'.
           05  HD-TAB50               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'YTD CLAIMS'.
           05  HD-TAB51               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'L12 CLAIMS'.
           05  HD-TAB52               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'P12 CLAIMS'.
           05  HD-TAB53               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'PYTD CLAIMS'.
           05  HD-TAB54               PIC X.
           05  FILLER                 PIC X(7)    VALUE 'MTD CLP'.
           05  HD-TAB55               PIC X.
           05  FILLER                 PIC X(7)    VALUE 'ITD CLP'.
           05  HD-TAB56               PIC X.
           05  FILLER                 PIC X(7)    VALUE 'YTD CLP'.
           05  HD-TAB57               PIC X.
           05  FILLER                 PIC X(7)    VALUE 'L12 CLP'.
           05  HD-TAB58               PIC X.
           05  FILLER                 PIC X(7)    VALUE 'P12 CLP'.
           05  HD-TAB59               PIC X.
           05  FILLER                 PIC X(8)    VALUE 'PYTD CLP'.
           05  HD-TAB60               PIC X.
           05  FILLER                 PIC XXX     VALUE 'EOR'.

       01  WS-INIT-EXTRACT             PIC X(698).
       01  EXTRACT-RECORD.
           05  EXT-CARRIER             PIC X.
           05  EXT-TAB1                PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-TAB2                PIC X.
           05  EXT-STATE               PIC XX.
           05  EXT-TAB3                PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-TAB4                PIC X.
           05  EXT-REPORT-CDE1         PIC X(10).
           05  EXT-TAB5                PIC X.
           05  EXT-REPORT-CDE2         PIC X(10).
           05  EXT-TAB6                PIC X.
           05  EXT-REPORT-CDE3         PIC X(10).
           05  EXT-TAB6A               PIC X.
           05  EXT-EOM-DT              PIC X(10).
           05  EXT-TAB7                PIC X.
           05  EXT-MTD-NET-PRM         PIC -----------9.
           05  EXT-TAB8                PIC X.
           05  EXT-MTD-ERN-PRM         PIC -----------9.
           05  EXT-TAB9                PIC X.
           05  EXT-MTD-INC-CLMS        PIC -----------9.
           05  EXT-TAB10               PIC X.
           05  EXT-MTD-LR              PIC -----.9.
           05  EXT-TAB11               PIC X.
           05  EXT-ITD-NET-PRM         PIC -----------9.
           05  EXT-TAB12               PIC X.
           05  EXT-ITD-ERN-PRM         PIC -----------9.
           05  EXT-TAB13               PIC X.
           05  EXT-ITD-INC-CLMS        PIC -----------9.
           05  EXT-TAB14               PIC X.
           05  EXT-ITD-LR              PIC ----9.9.
           05  EXT-TAB15               PIC X.
           05  EXT-YTD-NET-PRM         PIC -----------9.
           05  EXT-TAB16               PIC X.
           05  EXT-YTD-ERN-PRM         PIC -----------9.
           05  EXT-TAB17               PIC X.
           05  EXT-YTD-INC-CLMS        PIC -----------9.
           05  EXT-TAB18               PIC X.
           05  EXT-YTD-LR              PIC ----9.9.
           05  EXT-TAB19               PIC X.
           05  EXT-L12-NET-PRM         PIC -----------9.
           05  EXT-TAB20               PIC X.
           05  EXT-L12-ERN-PRM         PIC -----------9.
           05  EXT-TAB21               PIC X.
           05  EXT-L12-INC-CLMS        PIC -----------9.
           05  EXT-TAB22               PIC X.
           05  EXT-L12-LR              PIC ----9.9.
           05  EXT-TAB23               PIC X.
           05  EXT-P12-NET-PRM         PIC -----------9.
           05  EXT-TAB24               PIC X.
           05  EXT-P12-ERN-PRM         PIC -----------9.
           05  EXT-TAB25               PIC X.
           05  EXT-P12-INC-CLMS        PIC -----------9.
           05  EXT-TAB26               PIC X.
           05  EXT-P12-LR              PIC ----9.9.
           05  EXT-TAB27               PIC X.
           05  EXT-PYTD-NET-PRM        PIC -----------9.
           05  EXT-TAB28               PIC X.
           05  EXT-PYTD-ERN-PRM        PIC -----------9.
           05  EXT-TAB29               PIC X.
           05  EXT-PYTD-INC-CLMS       PIC -----------9.
           05  EXT-TAB30               PIC X.
           05  EXT-PYTD-LR             PIC ----9.9.
           05  EXT-TAB31               PIC X.
           05  EXT-LO-CERT-DT          PIC X(10).
           05  EXT-TAB32               PIC X.
           05  EXT-HI-CERT-DT          PIC X(10).
           05  EXT-TAB33               PIC X.
           05  EXT-BUS-CLASS           PIC XX.
           05  EXT-TAB34               PIC X.
           05  EXT-ACCT-STATUS         PIC X(11).
           05  EXT-TAB35               PIC X.
           05  EXT-ACCT-NAME           PIC X(30).
           05  EXT-TAB36               PIC X.
           05  EXT-MTD-NET-COMM        PIC --------9.
           05  EXT-TAB37               PIC X.
           05  EXT-MTD-COMM-PCT        PIC --99.9.
           05  EXT-TAB38               PIC X.
           05  EXT-ITD-NET-COMM        PIC --------9.
           05  EXT-TAB39               PIC X.
           05  EXT-ITD-COMM-PCT        PIC --99.9.
           05  EXT-TAB40               PIC X.
           05  EXT-YTD-NET-COMM        PIC --------9.
           05  EXT-TAB41               PIC X.
           05  EXT-YTD-COMM-PCT        PIC --99.9.
           05  EXT-TAB42               PIC X.
           05  EXT-L12-NET-COMM        PIC --------9.
           05  EXT-TAB43               PIC X.
           05  EXT-L12-COMM-PCT        PIC --99.9.
           05  EXT-TAB44               PIC X.
           05  EXT-P12-NET-COMM        PIC --------9.
           05  EXT-TAB45               PIC X.
           05  EXT-P12-COMM-PCT        PIC --99.9.
           05  EXT-TAB46               PIC X.
           05  EXT-PYTD-NET-COMM       PIC --------9.
           05  EXT-TAB47               PIC X.
           05  EXT-PYTD-COMM-PCT       PIC --99.9.
           05  EXT-TAB48               PIC X.
           05  EXT-MTD-PD-CLMS         PIC -----------9.
           05  EXT-TAB49               PIC X.
           05  EXT-ITD-PD-CLMS         PIC -----------9.
           05  EXT-TAB50               PIC X.
           05  EXT-YTD-PD-CLMS         PIC -----------9.
           05  EXT-TAB51               PIC X.
           05  EXT-L12-PD-CLMS         PIC -----------9.
           05  EXT-TAB52               PIC X.
           05  EXT-P12-PD-CLMS         PIC -----------9.
           05  EXT-TAB53               PIC X.
           05  EXT-PYTD-PD-CLMS        PIC -----------9.
           05  EXT-TAB54               PIC X.
           05  EXT-MTD-CLP             PIC -----------9.
           05  EXT-TAB55               PIC X.
           05  EXT-ITD-CLP             PIC -----------9.
           05  EXT-TAB56               PIC X.
           05  EXT-YTD-CLP             PIC -----------9.
           05  EXT-TAB57               PIC X.
           05  EXT-L12-CLP             PIC -----------9.
           05  EXT-TAB58               PIC X.
           05  EXT-P12-CLP             PIC -----------9.
           05  EXT-TAB59               PIC X.
           05  EXT-PYTD-CLP            PIC -----------9.
           05  EXT-TAB60               PIC X.
           05  EXT-EOR                 PIC X.

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

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.
       0002-INPUT.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0075-READ-ERACCT    THRU 0075-EXIT
           MOVE AM-REPORT-CODE-1       TO PRV-REPORT-CDE1
           MOVE AM-REPORT-CODE-2       TO PRV-REPORT-CDE2
           MOVE AM-REPORT-CODE-3       TO PRV-REPORT-CDE3
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
           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT
           GOBACK
           .
       0002-EXIT.
           EXIT.

       0010-INITIALIZE.

           MOVE SPACES                 TO EXTRACT-RECORD
           MOVE ';'                    TO EXT-TAB1
                                          EXT-TAB2
                                          EXT-TAB3
                                          EXT-TAB4
                                          EXT-TAB5
                                          EXT-TAB6
                                          EXT-TAB6A
                                          EXT-TAB7
                                          EXT-TAB8
                                          EXT-TAB9
                                          EXT-TAB10
                                          EXT-TAB11
                                          EXT-TAB12
                                          EXT-TAB13
                                          EXT-TAB14
                                          EXT-TAB15
                                          EXT-TAB16
                                          EXT-TAB17
                                          EXT-TAB18
                                          EXT-TAB19
                                          EXT-TAB20
                                          EXT-TAB21
                                          EXT-TAB22
                                          EXT-TAB23
                                          EXT-TAB24
                                          EXT-TAB25
                                          EXT-TAB26
                                          EXT-TAB27
                                          EXT-TAB28
                                          EXT-TAB29
                                          EXT-TAB30
                                          EXT-TAB31
                                          EXT-TAB32
                                          EXT-TAB33
                                          EXT-TAB34
                                          EXT-TAB35
                                          EXT-TAB36
                                          EXT-TAB37
                                          EXT-TAB38
                                          EXT-TAB39
                                          EXT-TAB40
                                          EXT-TAB41
                                          EXT-TAB42
                                          EXT-TAB43
                                          EXT-TAB44
                                          EXT-TAB45
                                          EXT-TAB46
                                          EXT-TAB47
                                          EXT-TAB48
                                          EXT-TAB49
                                          EXT-TAB50
                                          EXT-TAB51
                                          EXT-TAB52
                                          EXT-TAB53
                                          EXT-TAB54
                                          EXT-TAB55
                                          EXT-TAB56
                                          EXT-TAB57
                                          EXT-TAB58
                                          EXT-TAB59
                                          EXT-TAB60

           MOVE 'E'                    TO EXT-EOR

           MOVE RUN-DATE               TO WS-WORK-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO EXT-EOM-DT
           END-STRING

           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT

           MOVE ';'                    TO HD-TAB1
                                          HD-TAB2
                                          HD-TAB3
                                          HD-TAB4
                                          HD-TAB5
                                          HD-TAB6
                                          HD-TAB6A
                                          HD-TAB7
                                          HD-TAB8
                                          HD-TAB9
                                          HD-TAB10
                                          HD-TAB11
                                          HD-TAB12
                                          HD-TAB13
                                          HD-TAB14
                                          HD-TAB15
                                          HD-TAB16
                                          HD-TAB17
                                          HD-TAB18
                                          HD-TAB19
                                          HD-TAB20
                                          HD-TAB21
                                          HD-TAB22
                                          HD-TAB23
                                          HD-TAB24
                                          HD-TAB25
                                          HD-TAB26
                                          HD-TAB27
                                          HD-TAB28
                                          HD-TAB29
                                          HD-TAB30
                                          HD-TAB31
                                          HD-TAB32
                                          HD-TAB33
                                          HD-TAB34
                                          HD-TAB35
                                          HD-TAB36
                                          HD-TAB37
                                          HD-TAB38
                                          HD-TAB39
                                          HD-TAB40
                                          HD-TAB41
                                          HD-TAB42
                                          HD-TAB43
                                          HD-TAB44
                                          HD-TAB45
                                          HD-TAB46
                                          HD-TAB47
                                          HD-TAB48
                                          HD-TAB49
                                          HD-TAB50
                                          HD-TAB51
                                          HD-TAB52
                                          HD-TAB53
                                          HD-TAB54
                                          HD-TAB55
                                          HD-TAB56
                                          HD-TAB57
                                          HD-TAB58
                                          HD-TAB59
                                          HD-TAB60
                                          
      *    WRITE EXTRACT-HEAD-OUT      FROM HEAD-RECORD
           
           PERFORM 0015-INIT-TABLE     THRU 0015-EXIT
           MOVE LOW-VALUES             TO WS-PREVIOUS-KEY
                                          WS-CURRENT-KEY
           
           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE -1                     TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO WS-LAST-MONTH-DT
           ELSE
              DISPLAY ' LAST MONTH DATE ERROR '
              PERFORM ABEND-PGM
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE -12                    TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO WS-ONE-YEAR-AGO-DT
           ELSE
              DISPLAY ' ONE YEAR AGO DATE ERROR '
              PERFORM ABEND-PGM
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE -24                    TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO WS-TWO-YEARS-AGO-DT
           ELSE
              DISPLAY ' TWO YEARS AGO DATE ERROR '
              PERFORM ABEND-PGM
           END-IF

           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE 12                     TO WS-WORK-MM
           MOVE 31                     TO WS-WORK-DD
           SUBTRACT 1                  FROM WS-WORK-CCYY-N
           MOVE WS-WORK-DATE           TO WS-LAST-YEAR-END-DT

           MOVE WS-TWO-YEARS-AGO-DT    TO WS-WORK-DATE
           MOVE 12                     TO WS-WORK-MM
           MOVE 31                     TO WS-WORK-DD
           MOVE WS-WORK-DATE           TO WS-TWO-YEAR-ENDS-AGO-DT

           DISPLAY '   CURRENT MONTH - ' RUN-DATE
           DISPLAY '      LAST MONTH - ' WS-LAST-MONTH-DT
           DISPLAY '       LAST YEAR - ' WS-LAST-YEAR-END-DT
           DISPLAY '    ONE YEAR AGO - ' WS-ONE-YEAR-AGO-DT
           DISPLAY '   TWO YEARS AGO - ' WS-TWO-YEARS-AGO-DT
           DISPLAY '   LAST YEAR END - ' WS-LAST-YEAR-END-DT
           DISPLAY ' 2 YEAR ENDS AGO - ' WS-TWO-YEAR-ENDS-AGO-DT

           MOVE +1                     TO S1

           .
       0010-EXIT.
           EXIT.

       0015-INIT-TABLE.
        
      *    INITIALIZE (1 THRU 10)
      
           PERFORM VARYING WS-S2 FROM +1 BY +1 UNTIL
              WS-S2 > +10
              MOVE +0                  TO WS-NET-PRM   (WS-S2)
                                          WS-EARN-PRM  (WS-S2)
                                          WS-CLM-INC   (WS-S2)
                                          WS-NET-COMM  (WS-S2)
                                          WS-EARN-COMM (WS-S2)
                                          WS-PAID-CLMS (WS-S2)
                                          WS-CLP       (WS-S2)
           END-PERFORM

           .
       0015-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EPECS ERACCTT
               OUTPUT EXTRACT

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' BAD OPEN FOR ERACCT ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' EPEC IN RECORDS  ' EPEC-IN-CNT
           DISPLAY ' EXTR OUT RECORDS ' EXTR-OUT-CNT
           CLOSE EPECS EXTRACT ERACCTT

           .
       0030-EXIT.
           EXIT.

       0060-READ-EPEC.

           READ EPECS AT END
               SET END-OF-EPEC         TO TRUE
           END-READ

           IF NOT END-OF-EPEC
              IF (EP-REIN = 'R')
                 IF (DTE-CLIENT = 'CID')
                    OR (EP-REINCO NOT = '300' AND '500' AND '700')
                    GO TO 0060-READ-EPEC
                 END-IF
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
              MOVE AM-REPORT-CODE-1    TO WS-RPTCD1
              MOVE AM-REPORT-CODE-2    TO WS-RPTCD2
              MOVE AM-REPORT-CODE-3    TO WS-RPTCD3
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

           IF EP-RECORD-ID = 'EP' OR 'EC'
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
           MOVE AM-REPORT-CODE-3       TO PRV-REPORT-CDE3
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
              IF (EP-RUN-DTE = RUN-DATE)
                          OR
                 ((EP-RUN-DTE < RUN-DATE)
                 AND (EP-PURGE = 'P'))
                 MOVE +1               TO WS-S2
                 PERFORM 0082-ACCUM-EP-TOTS
                                       THRU 0082-EXIT
              END-IF
              IF (EP-RUN-DTE = WS-LAST-MONTH-DT)
                            OR
                 ((EP-RUN-DTE < WS-LAST-MONTH-DT)
                 AND (EP-PURGE = 'P'))
                 MOVE +2            TO WS-S2
                 PERFORM 0082-ACCUM-EP-TOTS
                                       THRU 0082-EXIT
              END-IF
              IF (EP-RUN-DTE = WS-LAST-YEAR-END-DT)
                            OR
                 ((EP-RUN-DTE < WS-LAST-YEAR-END-DT)
                 AND (EP-PURGE = 'P'))
                 MOVE +3            TO WS-S2
                 PERFORM 0082-ACCUM-EP-TOTS
                                       THRU 0082-EXIT
              END-IF
              IF (EP-RUN-DTE = WS-ONE-YEAR-AGO-DT)
                            OR
                 ((EP-RUN-DTE < WS-ONE-YEAR-AGO-DT)
                 AND (EP-PURGE = 'P'))
                 MOVE +4            TO WS-S2
                 PERFORM 0082-ACCUM-EP-TOTS
                                       THRU 0082-EXIT
              END-IF
              IF (EP-RUN-DTE = WS-TWO-YEARS-AGO-DT)
                            OR
                 ((EP-RUN-DTE < WS-TWO-YEARS-AGO-DT)
                 AND (EP-PURGE = 'P'))
                 MOVE +5            TO WS-S2
                 PERFORM 0082-ACCUM-EP-TOTS
                                       THRU 0082-EXIT
              END-IF
              IF (EP-RUN-DTE = WS-TWO-YEAR-ENDS-AGO-DT)
                            OR
                 ((EP-RUN-DTE < WS-TWO-YEAR-ENDS-AGO-DT)
                 AND (EP-PURGE = 'P'))
                 MOVE +6            TO WS-S2
                 PERFORM 0082-ACCUM-EP-TOTS
                                       THRU 0082-EXIT
              END-IF
           END-IF

           IF (EP-RECORD-ID = 'EC')
              AND (EP-REIN NOT = 'R')
              IF (EC-RUN-DTE = RUN-DATE)
                          OR
                 ((EC-RUN-DTE < RUN-DATE)
                 AND (EC-PURGE = 'P'))
                 MOVE +1               TO WS-S2
                 PERFORM 0083-ACCUM-EC-TOTS
                                       THRU 0083-EXIT
              END-IF
              IF (EC-RUN-DTE = WS-LAST-MONTH-DT)
                            OR
                 ((EC-RUN-DTE < WS-LAST-MONTH-DT)
                 AND (EC-PURGE = 'P'))
                 MOVE +2            TO WS-S2
                 PERFORM 0083-ACCUM-EC-TOTS
                                       THRU 0083-EXIT
              END-IF
              IF (EC-RUN-DTE = WS-LAST-YEAR-END-DT)
                            OR
                 ((EC-RUN-DTE < WS-LAST-YEAR-END-DT)
                 AND (EC-PURGE = 'P'))
                 MOVE +3            TO WS-S2
                 PERFORM 0083-ACCUM-EC-TOTS
                                       THRU 0083-EXIT
              END-IF
              IF (EC-RUN-DTE = WS-ONE-YEAR-AGO-DT)
                            OR
                 ((EC-RUN-DTE < WS-ONE-YEAR-AGO-DT)
                 AND (EC-PURGE = 'P'))
                 MOVE +4            TO WS-S2
                 PERFORM 0083-ACCUM-EC-TOTS
                                       THRU 0083-EXIT
              END-IF
              IF (EC-RUN-DTE = WS-TWO-YEARS-AGO-DT)
                            OR
                 ((EC-RUN-DTE < WS-TWO-YEARS-AGO-DT)
                 AND (EC-PURGE = 'P'))
                 MOVE +5            TO WS-S2
                 PERFORM 0083-ACCUM-EC-TOTS
                                       THRU 0083-EXIT
              END-IF
              IF (EC-RUN-DTE = WS-TWO-YEAR-ENDS-AGO-DT)
                            OR
                 ((EC-RUN-DTE < WS-TWO-YEAR-ENDS-AGO-DT)
                 AND (EC-PURGE = 'P'))
                 MOVE +6            TO WS-S2
                 PERFORM 0083-ACCUM-EC-TOTS
                                       THRU 0083-EXIT
              END-IF
           END-IF

           .
       0080-EXIT.
           EXIT.

       0082-ACCUM-EP-TOTS.

           IF (DTE-CLIENT = 'DCC')
              AND (EP-REIN = 'R')
              COMPUTE WS-CLP (WS-S2) =
                 WS-CLP (WS-S2) + (EP-ISS-PRM - EP-CNC-PRM)
              GO TO 0082-EXIT
           END-IF

           COMPUTE WS-NET-PRM (WS-S2) =
              WS-NET-PRM (WS-S2) + (EP-ISS-PRM - EP-CNC-PRM)

           COMPUTE WS-CLM-INC (WS-S2) =
              WS-CLM-INC (WS-S2) + EP-CLM-AMT + EP-CLM-DU +
                 EP-CLM-PV + EP-CLM-IBNR + EP-LOSS-RESV

           COMPUTE WS-PAID-CLMS (WS-S2) =
              WS-PAID-CLMS (WS-S2) + EP-CLM-AMT

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

           COMPUTE WS-EARN-PRM (WS-S2) =
              WS-EARN-PRM (WS-S2) + WS-ERN-PRM

           .
       0082-EXIT.
           EXIT.

       0083-ACCUM-EC-TOTS.

           PERFORM VARYING A1 FROM +1 BY +1 UNTIL
              (A1 > +5)
              IF EC-AGT-TYPE (A1) = 'C' OR 'D' OR 'O' OR 'P'
                 COMPUTE WS-NET-COMM (WS-S2) =
                 WS-NET-COMM (WS-S2) + (EC-ISS-COMM (A1)
                    - EC-CNC-COMM (A1))
                 EVALUATE TRUE
                    WHEN WS-EP-CODE = 'R' OR 'N' OR 'U' OR 'A' OR 'T'
                       MOVE EC-COMM-78 (A1)   TO WS-ERN-COMM
                    WHEN WS-EP-CODE = 'P'
                       MOVE EC-COMM-PR (A1)   TO WS-ERN-COMM
                    WHEN WS-EP-CODE = 'B' OR 'K' OR 'L'
                       MOVE EC-COMM-ST (A1)   TO WS-ERN-COMM
                    WHEN WS-EP-CODE = 'M'
                       COMPUTE WS-ERN-COMM ROUNDED =
                          (EC-COMM-PR (A1) + EC-COMM-78 (A1))  *  +.5
                    WHEN WS-EP-CODE = '1'
                       COMPUTE WS-ERN-COMM ROUNDED =
                          (EC-COMM-PR (A1) * +.6667 )
                             + (EC-COMM-78 (A1) * +.3333)
                    WHEN WS-EP-CODE = '2'
                       COMPUTE WS-ERN-COMM ROUNDED =
                          (EC-COMM-PR (A1) * +.80 )
                             + (EC-COMM-78 (A1) * +.20)
                    WHEN OTHER
                       MOVE EC-COMM-78 (A1)   TO WS-ERN-COMM
                       DISPLAY ' USING DEFAULT EP-CODE ' WS-EP-CODE
                 END-EVALUATE
                 COMPUTE WS-EARN-COMM (WS-S2) =
                 WS-EARN-COMM (WS-S2) + WS-ERN-COMM
              END-IF
           END-PERFORM

           .
       0083-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.

           MOVE WS-INIT-EXTRACT        TO EXTRACT-RECORD
           MOVE WS-PREV-CARRIER        TO EXT-CARRIER
           MOVE WS-PREV-GROUP          TO EXT-GROUP
           MOVE WS-PREV-STATE          TO EXT-STATE
           MOVE WS-PREV-ACCOUNT        TO EXT-ACCOUNT
           MOVE WS-PREV-RPTCD1         TO EXT-REPORT-CDE1

      *    MOVE PRV-REPORT-CDE1        TO EXT-REPORT-CDE1
           MOVE PRV-REPORT-CDE2        TO EXT-REPORT-CDE2
           MOVE PRV-REPORT-CDE3        TO EXT-REPORT-CDE3
           MOVE PRV-BUS-CLASS          TO EXT-BUS-CLASS
           EVALUATE PRV-ACCT-STATUS
              WHEN '0'
                 MOVE 'ACTIVE'         TO EXT-ACCT-STATUS
              WHEN '1'
                 MOVE 'INACTIVE'       TO EXT-ACCT-STATUS
              WHEN '2'
                 MOVE 'TRANSFERRED'    TO EXT-ACCT-STATUS
              WHEN '3'
                 MOVE 'CANCELLED'      TO EXT-ACCT-STATUS
              WHEN '4'
                 MOVE 'FROZEN   '      TO EXT-ACCT-STATUS
              WHEN '5'
                 MOVE 'SUSPENDED'      TO EXT-ACCT-STATUS
021916        WHEN '6'
021916           MOVE 'DROPPED'        TO EXT-ACCT-STATUS
021916        WHEN '7'
021916           MOVE 'LAPSED'         TO EXT-ACCT-STATUS
021916        WHEN '8'
021916           MOVE 'RUN-OFF'        TO EXT-ACCT-STATUS
021916        WHEN '9'
021916           MOVE 'PENDING'        TO EXT-ACCT-STATUS
              WHEN OTHER
                 MOVE 'INVALID'        TO EXT-ACCT-STATUS
           END-EVALUATE

      *    MOVE PRV-ACCT-STATUS        TO EXT-ACCT-STATUS
           MOVE PRV-ACCT-NAME          TO EXT-ACCT-NAME
 
           MOVE PRV-HI-CERT-DT         TO WS-WORK-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO EXT-HI-CERT-DT
           END-STRING

           MOVE PRV-LO-CERT-DT         TO WS-WORK-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO EXT-LO-CERT-DT
           END-STRING

           PERFORM 0095-MOVE-TABLE     THRU 0095-EXIT
           PERFORM 0100-WRITE-EXTRACT  THRU 0100-EXIT

           .
       0090-EXIT.
           EXIT.

       0095-MOVE-TABLE.

      *********** I T D *********
           MOVE WS-NET-PRM (1)         TO EXT-ITD-NET-PRM
           MOVE WS-EARN-PRM (1)        TO EXT-ITD-ERN-PRM
           MOVE WS-CLM-INC (1)         TO EXT-ITD-INC-CLMS
           MOVE WS-PAID-CLMS (1)       TO EXT-ITD-PD-CLMS
           MOVE WS-CLP (1)             TO EXT-ITD-CLP
      *    MOVE WS-NET-COMM (1)        TO EXT-ITD-NET-COMM
           MOVE WS-EARN-COMM (1)       TO EXT-ITD-NET-COMM
           IF WS-EARN-PRM (1) = ZEROS
              MOVE ZEROS               TO WS-WRK-LR
           ELSE
              COMPUTE WS-WRK-LR ROUNDED = 
                 (WS-CLM-INC (1) / WS-EARN-PRM (1)) * 100
           END-IF
           IF WS-WRK-LR > WS-WRK-LR-HI
              MOVE WS-WRK-LR-HI        TO WS-WRK-LR
           ELSE
              IF WS-WRK-LR < WS-WRK-LR-LO
                 MOVE WS-WRK-LR-LO     TO WS-WRK-LR
              END-IF
           END-IF
           MOVE WS-WRK-LR              TO EXT-ITD-LR
      *    COMPUTE WS-COMM-PCT =
      *       (WS-NET-COMM (1) / WS-NET-PRM (1)) * +100
           IF WS-EARN-PRM (1) = ZEROS
              MOVE ZEROS               TO WS-COMM-PCT
           ELSE
              COMPUTE WS-COMM-PCT ROUNDED =
                 (WS-EARN-COMM (1) / WS-EARN-PRM (1)) * +100
           END-IF

           MOVE WS-COMM-PCT            TO EXT-ITD-COMM-PCT

      *********** M T D *********
           COMPUTE EXT-MTD-NET-PRM = WS-NET-PRM (1)
              - WS-NET-PRM (2)
           COMPUTE EXT-MTD-ERN-PRM = WS-EARN-PRM (1)
              - WS-EARN-PRM (2)
           COMPUTE EXT-MTD-INC-CLMS = WS-CLM-INC (1)
              - WS-CLM-INC (2)
           COMPUTE EXT-MTD-PD-CLMS = WS-PAID-CLMS (1)
              - WS-PAID-CLMS (2)
           COMPUTE EXT-MTD-CLP = WS-CLP (1)
              - WS-CLP (2)
           COMPUTE EXT-MTD-NET-COMM = WS-EARN-COMM (1)
              - WS-EARN-COMM (2)
           IF (WS-EARN-PRM (1) - WS-EARN-PRM (2)) = ZEROS
              MOVE ZEROS               TO WS-WRK-LR
           ELSE
              COMPUTE WS-WRK-LR ROUNDED =
                 ((WS-CLM-INC (1) - WS-CLM-INC (2))
                 / (WS-EARN-PRM (1) - WS-EARN-PRM (2))) * 100
           END-IF
           IF WS-WRK-LR > WS-WRK-LR-HI
              MOVE WS-WRK-LR-HI        TO WS-WRK-LR
           ELSE
              IF WS-WRK-LR < WS-WRK-LR-LO
                 MOVE WS-WRK-LR-LO     TO WS-WRK-LR
              END-IF
           END-IF
           MOVE WS-WRK-LR              TO EXT-MTD-LR
      *    COMPUTE WS-COMM-PCT =
      *       ((WS-NET-COMM (1) - WS-NET-COMM (2))
      *                      /
      *       (WS-NET-PRM (1) - WS-NET-PRM (2)))
      *                      * +100
           IF (WS-EARN-PRM (1) - WS-EARN-PRM (2)) = ZEROS
              MOVE ZEROS               TO WS-COMM-PCT
           ELSE
              COMPUTE WS-COMM-PCT ROUNDED =
                 ((WS-EARN-COMM (1) - WS-EARN-COMM (2))
                                /
                 (WS-EARN-PRM (1) - WS-EARN-PRM (2)))
                                * +100
           END-IF
           MOVE WS-COMM-PCT            TO EXT-MTD-COMM-PCT

      *********** Y T D *********
           COMPUTE EXT-YTD-NET-PRM = WS-NET-PRM (1)
              - WS-NET-PRM (3)
           COMPUTE EXT-YTD-ERN-PRM = WS-EARN-PRM (1)
              - WS-EARN-PRM (3)
           COMPUTE EXT-YTD-INC-CLMS = WS-CLM-INC (1)
              - WS-CLM-INC (3)
           COMPUTE EXT-YTD-PD-CLMS = WS-PAID-CLMS (1)
              - WS-PAID-CLMS (3)
           COMPUTE EXT-YTD-CLP = WS-CLP (1)
              - WS-CLP (3)
           COMPUTE EXT-YTD-NET-COMM = WS-EARN-COMM (1)
              - WS-EARN-COMM (3)
           IF (WS-EARN-PRM (1) - WS-EARN-PRM (3)) = ZEROS
              MOVE ZEROS               TO WS-WRK-LR
           ELSE
              COMPUTE WS-WRK-LR ROUNDED =
                 ((WS-CLM-INC (1) - WS-CLM-INC (3))
                 / (WS-EARN-PRM (1) - WS-EARN-PRM (3))) * 100
           END-IF
           IF EXT-ACCOUNT = '00009727OB'
              DISPLAY ' FOUND 9727OB '
              MOVE WS-EARN-PRM (1) TO WS-DISPLAY-AMT
              DISPLAY ' EARN PRM ITD ' WS-DISPLAY-AMT
              MOVE WS-EARN-PRM (3) TO WS-DISPLAY-AMT
              DISPLAY ' EARN PRM DEC06 ' WS-DISPLAY-AMT
              MOVE WS-CLM-INC (1) TO WS-DISPLAY-AMT
              DISPLAY ' CLM INC  ITD ' WS-DISPLAY-AMT
              MOVE WS-CLM-INC (3) TO WS-DISPLAY-AMT
              DISPLAY ' CLM INC DECC06 ' WS-DISPLAY-AMT
              MOVE WS-WRK-LR TO WS-DISPLAY-AMT
              DISPLAY ' LOSS RATIO ' WS-DISPLAY-AMT
           END-IF
           IF WS-WRK-LR > WS-WRK-LR-HI
              MOVE WS-WRK-LR-HI        TO WS-WRK-LR
           ELSE
              IF WS-WRK-LR < WS-WRK-LR-LO
                 MOVE WS-WRK-LR-LO     TO WS-WRK-LR
              END-IF
           END-IF
           MOVE WS-WRK-LR              TO EXT-YTD-LR
      *    COMPUTE WS-COMM-PCT =
      *       ((WS-NET-COMM (1) - WS-NET-COMM (3))
      *                      /
      *       (WS-NET-PRM (1) - WS-NET-PRM (3)))
      *                      * +100
           IF (WS-EARN-PRM (1) - WS-EARN-PRM (3)) = ZEROS
              MOVE ZEROS               TO WS-COMM-PCT
           ELSE
              COMPUTE WS-COMM-PCT ROUNDED =
                 ((WS-EARN-COMM (1) - WS-EARN-COMM (3))
                                /
                 (WS-EARN-PRM (1) - WS-EARN-PRM (3)))
                                * +100
           END-IF
           MOVE WS-COMM-PCT            TO EXT-YTD-COMM-PCT

      *********** L 1 2 *********
           COMPUTE EXT-L12-NET-PRM = WS-NET-PRM (1)
              - WS-NET-PRM (4)
           COMPUTE EXT-L12-ERN-PRM = WS-EARN-PRM (1)
              - WS-EARN-PRM (4)
           COMPUTE EXT-L12-INC-CLMS = WS-CLM-INC (1)
              - WS-CLM-INC (4)
           COMPUTE EXT-L12-PD-CLMS = WS-PAID-CLMS (1)
              - WS-PAID-CLMS (4)
           COMPUTE EXT-L12-CLP = WS-CLP (1)
              - WS-CLP (4)
           COMPUTE EXT-L12-NET-COMM = WS-EARN-COMM (1)
              - WS-EARN-COMM (4)
           IF (WS-EARN-PRM (1) - WS-EARN-PRM (4)) = ZEROS
              MOVE ZEROS               TO WS-WRK-LR
           ELSE
              COMPUTE WS-WRK-LR ROUNDED =
                 ((WS-CLM-INC (1) - WS-CLM-INC (4))
                 / (WS-EARN-PRM (1) - WS-EARN-PRM (4))) * 100
           END-IF

           IF WS-WRK-LR > WS-WRK-LR-HI
              MOVE WS-WRK-LR-HI        TO WS-WRK-LR
           ELSE
              IF WS-WRK-LR < WS-WRK-LR-LO
                 MOVE WS-WRK-LR-LO     TO WS-WRK-LR
              END-IF
           END-IF
           MOVE WS-WRK-LR              TO EXT-L12-LR
      *    COMPUTE WS-COMM-PCT =
      *       ((WS-NET-COMM (1) - WS-NET-COMM (4))
      *                      /
      *       (WS-NET-PRM (1) - WS-NET-PRM (4)))
      *                      * +100
           IF (WS-EARN-PRM (1) - WS-EARN-PRM (4)) = ZEROS
              MOVE ZEROS               TO WS-COMM-PCT
           ELSE
              COMPUTE WS-COMM-PCT ROUNDED =
                 ((WS-EARN-COMM (1) - WS-EARN-COMM (4))
                                /
                 (WS-EARN-PRM (1) - WS-EARN-PRM (4)))
                                * +100
           END-IF
           MOVE WS-COMM-PCT            TO EXT-L12-COMM-PCT

           IF EXT-ACCOUNT = '0000972400'
              DISPLAY ' FOUND 972400 '
              MOVE WS-EARN-PRM (1) TO WS-DISPLAY-AMT
              DISPLAY ' EARN PRM ITD ' WS-DISPLAY-AMT
              MOVE WS-EARN-PRM (4) TO WS-DISPLAY-AMT
              DISPLAY ' EARN PRM JUL06 ' WS-DISPLAY-AMT
              MOVE WS-EARN-COMM (1) TO WS-DISPLAY-AMT
              DISPLAY ' ERN COM  ITD ' WS-DISPLAY-AMT
              MOVE WS-EARN-COMM (4) TO WS-DISPLAY-AMT
              DISPLAY ' ERN COM JUL06 ' WS-DISPLAY-AMT
              MOVE WS-COMM-PCT TO WS-DISPLAY-AMT
              DISPLAY ' COMM PCT   ' WS-DISPLAY-AMT
           END-IF

      *********** P 1 2 *********
           COMPUTE EXT-P12-NET-PRM = WS-NET-PRM (4)
              - WS-NET-PRM (5)
           COMPUTE EXT-P12-ERN-PRM = WS-EARN-PRM (4)
              - WS-EARN-PRM (5)
           COMPUTE EXT-P12-INC-CLMS = WS-CLM-INC (4)
              - WS-CLM-INC (5)
           COMPUTE EXT-P12-PD-CLMS = WS-PAID-CLMS (4)
              - WS-PAID-CLMS (5)
           COMPUTE EXT-P12-CLP = WS-CLP (4)
              - WS-CLP (5)
           COMPUTE EXT-P12-NET-COMM = WS-EARN-COMM (4)
              - WS-EARN-COMM (5)
           IF (WS-EARN-PRM (4) - WS-EARN-PRM (5)) = ZEROS
              MOVE ZEROS               TO WS-WRK-LR
           ELSE
              COMPUTE WS-WRK-LR ROUNDED =
                 ((WS-CLM-INC (4) - WS-CLM-INC (5))
                 / (WS-EARN-PRM (4) - WS-EARN-PRM (5))) * 100
           END-IF
           IF WS-WRK-LR > WS-WRK-LR-HI
              MOVE WS-WRK-LR-HI        TO WS-WRK-LR
           ELSE
              IF WS-WRK-LR < WS-WRK-LR-LO
                 MOVE WS-WRK-LR-LO     TO WS-WRK-LR
              END-IF
           END-IF
           MOVE WS-WRK-LR              TO EXT-P12-LR
      *    COMPUTE WS-COMM-PCT =
      *       ((WS-NET-COMM (4) - WS-NET-COMM (5))
      *                      /
      *       (WS-NET-PRM (4) - WS-NET-PRM (5)))
      *                      * +100
           IF (WS-EARN-PRM (4) - WS-EARN-PRM (5)) = ZEROS
              MOVE ZEROS               TO WS-COMM-PCT
           ELSE
              COMPUTE WS-COMM-PCT ROUNDED =
                 ((WS-EARN-COMM (4) - WS-EARN-COMM (5))
                                /
                 (WS-EARN-PRM (4) - WS-EARN-PRM (5)))
                                * +100
           END-IF
           MOVE WS-COMM-PCT            TO EXT-P12-COMM-PCT


      *********** P Y T D *********
           COMPUTE EXT-PYTD-NET-PRM = WS-NET-PRM (4)
              - WS-NET-PRM (6)
           COMPUTE EXT-PYTD-ERN-PRM = WS-EARN-PRM (4)
              - WS-EARN-PRM (6)
           COMPUTE EXT-PYTD-INC-CLMS = WS-CLM-INC (4)
              - WS-CLM-INC (6)
           COMPUTE EXT-PYTD-PD-CLMS = WS-PAID-CLMS (4)
              - WS-PAID-CLMS (6)
           COMPUTE EXT-PYTD-CLP = WS-CLP (4)
              - WS-CLP (6)
           COMPUTE EXT-PYTD-NET-COMM = WS-EARN-COMM (4)
              - WS-EARN-COMM (6)
           IF (WS-EARN-PRM (4) - WS-EARN-PRM (6)) = ZEROS
              MOVE ZEROS               TO WS-WRK-LR
           ELSE
              COMPUTE WS-WRK-LR ROUNDED =
                 ((WS-CLM-INC (4) - WS-CLM-INC (6))
                 / (WS-EARN-PRM (4) - WS-EARN-PRM (6))) * 100
           END-IF
           IF WS-WRK-LR > WS-WRK-LR-HI
              MOVE WS-WRK-LR-HI        TO WS-WRK-LR
           ELSE
              IF WS-WRK-LR < WS-WRK-LR-LO
                 MOVE WS-WRK-LR-LO     TO WS-WRK-LR
              END-IF
           END-IF
           MOVE WS-WRK-LR              TO EXT-PYTD-LR
      *    COMPUTE WS-COMM-PCT =
      *       ((WS-NET-COMM (4) - WS-NET-COMM (6))
      *                      /
      *       (WS-NET-PRM (4) - WS-NET-PRM (6)))
      *                      * +100
           IF (WS-EARN-PRM (4) - WS-EARN-PRM (6)) = ZEROS
              MOVE ZEROS               TO WS-COMM-PCT
           ELSE
              COMPUTE WS-COMM-PCT ROUNDED =
                 ((WS-EARN-COMM (4) - WS-EARN-COMM (6))
                                /
                 (WS-EARN-PRM (4) - WS-EARN-PRM (6)))
                                * +100
           END-IF
           MOVE WS-COMM-PCT            TO EXT-PYTD-COMM-PCT

           .
       0095-EXIT.
           EXIT.
           
       0100-WRITE-EXTRACT.

           WRITE EXTRACT-RECORD-OUT    FROM EXTRACT-RECORD
           ADD 1                       TO EXTR-OUT-CNT

           .
       0100-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

