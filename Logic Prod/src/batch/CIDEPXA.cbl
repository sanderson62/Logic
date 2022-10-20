       PROGRAM-ID.    CIDEPXA.
       AUTHOR.        AJRA.
       DATE-COMPILED.

      *REMARKS. 

      * Data Extract from EPEC file for Profitability Report
      
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 111313  CR2008111400002  AJRA  NEW EXTRACT PROGRAM
052214* 052214  IR2014052200001  AJRA  REMOVE REIN ONLY COMMISSION TYPE
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EPECS              ASSIGN TO SYS010.
           SELECT SORT-EPECS         ASSIGN TO SORTWK1.
           SELECT DISK-DATE          ASSIGN TO SYS019.

           SELECT ERACCTT            ASSIGN TO ERACCTT
                                     ACCESS IS SEQUENTIAL
                                     ORGANIZATION IS INDEXED
                                     FILE STATUS IS ERACCT-FILE-STATUS
                                     RECORD KEY IS AM-CONTROL-PRIMARY.

           SELECT ERRTBLT            ASSIGN TO ERRTBLT
                                     ACCESS IS DYNAMIC
                                     ORGANIZATION IS INDEXED
                                     FILE STATUS IS ERRTBL-FILE-STATUS
                                     RECORD KEY IS RE-CONTROL-PRIMARY.

           SELECT EXTRACT            ASSIGN TO SYS011
                                     ORGANIZATION IS LINE SEQUENTIAL.
                                                                        

           SELECT REINEXT            ASSIGN TO SYS012
                                     ORGANIZATION IS LINE SEQUENTIAL.
              

       DATA DIVISION.
       FILE SECTION.

       FD  EPECS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
                                       COPY ECSEPC01.

       SD  SORT-EPECS.

       01  SORT-RECORD.
           05  SRT-CARRIER             PIC X.
           05  SRT-GROUP               PIC X(6).
           05  SRT-STATE               PIC XX.
           05  SRT-ACCOUNT             PIC X(10).
           05  SRT-EFF-DATE            PIC 9(11)  VALUE ZEROS COMP-3.
           05  SRT-EXP-DATE            PIC 9(11)  VALUE ZEROS COMP-3.
           05  SRT-REPORT-CDE1         PIC X(10).
           05  SRT-REPORT-CDE2         PIC X(10).
           05  SRT-REPORT-CDE3         PIC X(10).
           05  SRT-REIN                PIC X.
           05  SRT-REIN-CO             PIC X(3).
           05  SRT-REIN-SUB            PIC X(3).
           05  SRT-BEN-TYPE            PIC X.
           05  SRT-BEN-CODE            PIC XX.
           05  SRT-REIN-TABLE          PIC X(3).
           05  SRT-RETRO-POOL          PIC X(6).
           05  SRT-EP-CODE             PIC X.
           05  SRT-REF-METH            PIC X.

      *  FIRST LEVEL
      *     1 = ITD
      *     2 = MTD
      *     3 = YTD
      *     4 = L12
      *     5 = PREVIOUS L12
      *     6 = PREVIOUS YTD
      *     THE REST ARE FOR FUTURE USE

           05  SRT-ACCUM-TOTALS.
               10  FILLER OCCURS 10.
                   15  SRT-ISS-CNT         PIC S9(9)     COMP-3.
                   15  SRT-ISS-BEN         PIC S9(11)V99 COMP-3.
                   15  SRT-ISS-BEN-GROSS   PIC S9(11)V99 COMP-3.
                   15  SRT-CNC-CNT         PIC S9(9)     COMP-3.
                   15  SRT-CNC-BEN         PIC S9(11)V99 COMP-3.
                   15  SRT-CNC-BEN-GROSS   PIC S9(11)V99 COMP-3.
                   15  SRT-ISS-PRM         PIC S9(9)V99  COMP-3.
                   15  SRT-ISS-PRM-GROSS   PIC S9(9)V99  COMP-3.
                   15  SRT-CNC-PRM         PIC S9(9)V99  COMP-3.
                   15  SRT-CNC-PRM-GROSS   PIC S9(9)V99  COMP-3.
                   15  SRT-PRM-78          PIC S9(9)V99  COMP-3.
                   15  SRT-PRM-PR          PIC S9(9)V99  COMP-3.
                   15  SRT-PRM-ST          PIC S9(9)V99  COMP-3.
                   15  SRT-CLM-AMT         PIC S9(9)V99  COMP-3.
                   15  SRT-CLM-CNT         PIC S9(9)     COMP-3.
                   15  SRT-CLM-CRT         PIC S9(9)     COMP-3.
                   15  SRT-CLM-DU          PIC S9(7)V99  COMP-3.
                   15  SRT-CLM-PV          PIC S9(7)V99  COMP-3.
                   15  SRT-CLM-IBNR        PIC S9(7)V99  COMP-3.
                   15  SRT-LOSS-RESV       PIC S9(7)V99  COMP-3.
                   15  SRT-CLAIM-ADJ       PIC S9(7)V99  COMP-3.
                   15  SRT-RETRO-EXP       PIC S9(7)V99  COMP-3.
                   15  SRT-RETRO-PMT       PIC S9(7)V99  COMP-3.
                   15  SRT-RETRO-OTH-COMM  PIC S9(7)V99  COMP-3.
                   15  SRT-MORT-RESV       PIC S9(11)V9(6) COMP-3.
                   15  SRT-IN-FORCE        PIC S9(11)V99 COMP-3.
                   15  SRT-ADJUST          PIC S9(7)V99  COMP-3.
                   15  SRT-LIFE-YEARS      PIC S9(9)     COMP-3.
                   15  SRT-CLM-EXP         PIC S9(9)V99  COMP-3.
                   15  SRT-PRM-78-ADJ      PIC S9(9)V99  COMP-3.
                   15  SRT-PRM-PR-ADJ      PIC S9(9)V99  COMP-3.
                   15  SRT-PRM-ST-ADJ      PIC S9(9)V99  COMP-3.
                   15  SRT-PRM-TAX         PIC S9(7)V99  COMP-3.
                   15  SRT-INFORCE-CNT     PIC S9(9)     COMP-3.
                   15  SRT-AGT-ISS-COMM    PIC S9(9)V99  COMP-3.
                   15  SRT-AGT-CNC-COMM    PIC S9(9)V99  COMP-3.
                   15  SRT-AGT-COMM-78     PIC S9(9)V99  COMP-3.
                   15  SRT-AGT-COMM-PR     PIC S9(9)V99  COMP-3.
                   15  SRT-AGT-COMM-ST     PIC S9(9)V99  COMP-3.
                   15  SRT-AGT-COMM-78-ADJ PIC S9(9)V99  COMP-3.
                   15  SRT-AGT-COMM-PR-ADJ PIC S9(9)V99  COMP-3.
                   15  SRT-OW-ISS-COMM     PIC S9(9)V99  COMP-3.
                   15  SRT-OW-CNC-COMM     PIC S9(9)V99  COMP-3.
                   15  SRT-OW-COMM-78      PIC S9(9)V99  COMP-3.
                   15  SRT-OW-COMM-PR      PIC S9(9)V99  COMP-3.
                   15  SRT-OW-COMM-ST      PIC S9(9)V99  COMP-3.
                   15  SRT-OW-COMM-78-ADJ  PIC S9(9)V99  COMP-3.
                   15  SRT-OW-COMM-PR-ADJ  PIC S9(9)V99  COMP-3.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERACCTT.    
                                                                        
                                       COPY ERCACCT.                        

       FD  ERRTBLT
                                       COPY ECSRTFDD.

                                       COPY ERCREIN.

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD-OUT          PIC X(792).


       FD  REINEXT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  REIN-EXTRACT-RECORD-OUT     PIC X(792).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDEPX3 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-DISP-AMT                 PIC -ZZZ,ZZZ,999.99.
       77  WS-DISP-AMTA                PIC -ZZZ,ZZZ,999.99.
       77  WS-DISP-AMTB                PIC -ZZZ,ZZZ,999.99.
       77  WS-DISP-DATE                PIC 9(11)  VALUE ZEROS.
       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  EPEC-IN-CNT                 PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.
       77  REIN-EXTR-OUT-CNT           PIC 9(11)  VALUE ZEROS.
       77  WS-RET-RECS                 PIC 9(11)  VALUE ZEROS.
       77  WS-REL-RECS                 PIC 9(11)  VALUE ZEROS.
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
       77  WS-SUB                      PIC S999   VALUE +0 COMP-3.
       77  WS-FOUND                    PIC X  VALUE SPACES.
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
           05  WS-CONTROL-DATES.
               10  WS-CONT-EFF-DATE    PIC 9(11) COMP-3.
               10  WS-CONT-EXP-DATE    PIC 9(11) COMP-3.
           05  WS-RPTCD1               PIC X(10)  VALUE LOW-VALUES.
           05  WS-RPTCD2               PIC X(10)  VALUE LOW-VALUES.
           05  WS-RPTCD3               PIC X(10)  VALUE LOW-VALUES.
           05  WS-REIN                 PIC X.
           05  WS-REIN-CO              PIC X(3).
           05  WS-REIN-SUB             PIC X(3).
           05  WS-BEN-TYPE             PIC X.
           05  WS-BEN-CODE             PIC XX.
           
       01  WS-ACCT-STUFF.
           05  WS-REIN-TABLE      PIC X(3)   VALUE SPACES.
           05  WS-RETRO-POOL      PIC X(6)   VALUE SPACES.
           05  WS-EP-CODE         PIC X      VALUE SPACES.
           05  WS-REF-METH        PIC X      VALUE SPACES.

       01  WS-PREVIOUS-KEY.
           05  WS-PREV-CARRIER         PIC X.
           05  WS-PREV-GROUP           PIC X(6).
           05  WS-PREV-STATE           PIC XX.
           05  WS-PREV-ACCOUNT         PIC X(10).
           05  WS-PREV-CONT-DATES.
               10  WS-PREV-EFF-DATE    PIC 9(11) COMP-3.
               10  WS-PREV-EXP-DATE    PIC 9(11) COMP-3.
           05  WS-PREV-RPTCD1          PIC X(10).
           05  WS-PREV-RPTCD2          PIC X(10).
           05  WS-PREV-RPTCD3          PIC X(10).
           05  WS-PREV-REIN            PIC X.
           05  WS-PREV-REIN-CO         PIC X(3).
           05  WS-PREV-REIN-SUB        PIC X(3).
           05  WS-PREV-BEN-TYPE        PIC X.
           05  WS-PREV-BEN-CODE        PIC XX.


       01  WS-PREV-ACCT-STUFF.
           05  WS-PREV-REIN-TABLE      PIC X(3)   VALUE SPACES.
           05  WS-PREV-RETRO-POOL      PIC X(6)   VALUE SPACES.
           05  WS-PREV-EP-CODE         PIC X      VALUE SPACES.
           05  WS-PREV-REF-METH        PIC X      VALUE SPACES.


       01  WS-HOLD-REIN-COMP.
           05  WS-HOLD-REIN-PRIME      PIC X(3)   VALUE SPACES.
           05  WS-HOLD-REIN-SUB        PIC X(3)   VALUE SPACES.
           05  WS-HOLD-EP-CODE         PIC X      VALUE SPACES.
           05  WS-HOLD-REF-METH        PIC X      VALUE SPACES.


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
               10  WS-ISS-CNT         PIC S9(9)     COMP-3.
               10  WS-ISS-BEN         PIC S9(11)V99 COMP-3.
               10  WS-ISS-BEN-GROSS   PIC S9(11)V99 COMP-3.
               10  WS-CNC-CNT         PIC S9(9)     COMP-3.
               10  WS-CNC-BEN         PIC S9(11)V99 COMP-3.
               10  WS-CNC-BEN-GROSS   PIC S9(11)V99 COMP-3.
               10  WS-ISS-PRM         PIC S9(9)V99  COMP-3.
               10  WS-ISS-PRM-GROSS   PIC S9(9)V99  COMP-3.
               10  WS-CNC-PRM         PIC S9(9)V99  COMP-3.
               10  WS-CNC-PRM-GROSS   PIC S9(9)V99  COMP-3.
               10  WS-PRM-78          PIC S9(9)V99  COMP-3.
               10  WS-PRM-PR          PIC S9(9)V99  COMP-3.
               10  WS-PRM-ST          PIC S9(9)V99  COMP-3.
               10  WS-CLM-AMT         PIC S9(9)V99  COMP-3.
               10  WS-CLM-CNT         PIC S9(9)     COMP-3.
               10  WS-CLM-CRT         PIC S9(9)     COMP-3.
               10  WS-CLM-DU          PIC S9(7)V99  COMP-3.
               10  WS-CLM-PV          PIC S9(7)V99  COMP-3.
               10  WS-CLM-IBNR        PIC S9(7)V99  COMP-3.
               10  WS-LOSS-RESV       PIC S9(7)V99  COMP-3.
               10  WS-CLAIM-ADJ       PIC S9(7)V99  COMP-3.
               10  WS-RETRO-EXP       PIC S9(7)V99  COMP-3.
               10  WS-RETRO-PMT       PIC S9(7)V99  COMP-3.
               10  WS-RETRO-OTH-COMM  PIC S9(7)V99  COMP-3.
               10  WS-MORT-RESV       PIC S9(11)V9(6) COMP-3.
               10  WS-IN-FORCE        PIC S9(11)V99 COMP-3.
               10  WS-ADJUST          PIC S9(7)V99  COMP-3.
               10  WS-LIFE-YEARS      PIC S9(9)     COMP-3.
               10  WS-CLM-EXP         PIC S9(9)V99  COMP-3.
               10  WS-PRM-78-ADJ      PIC S9(9)V99  COMP-3.
               10  WS-PRM-PR-ADJ      PIC S9(9)V99  COMP-3.
               10  WS-PRM-ST-ADJ      PIC S9(9)V99  COMP-3.
               10  WS-PRM-TAX         PIC S9(7)V99  COMP-3.
               10  WS-INFORCE-CNT     PIC S9(9)     COMP-3.
               10  WS-AGT-ISS-COMM    PIC S9(9)V99  COMP-3.
               10  WS-AGT-CNC-COMM    PIC S9(9)V99  COMP-3.
               10  WS-AGT-COMM-78     PIC S9(9)V99  COMP-3.
               10  WS-AGT-COMM-PR     PIC S9(9)V99  COMP-3.
               10  WS-AGT-COMM-ST     PIC S9(9)V99  COMP-3.
               10  WS-AGT-COMM-78-ADJ PIC S9(9)V99  COMP-3.
               10  WS-AGT-COMM-PR-ADJ PIC S9(9)V99  COMP-3.
               10  WS-OW-ISS-COMM     PIC S9(9)V99  COMP-3.
               10  WS-OW-CNC-COMM     PIC S9(9)V99  COMP-3.
               10  WS-OW-COMM-78      PIC S9(9)V99  COMP-3.
               10  WS-OW-COMM-PR      PIC S9(9)V99  COMP-3.
               10  WS-OW-COMM-ST      PIC S9(9)V99  COMP-3.
               10  WS-OW-COMM-78-ADJ  PIC S9(9)V99  COMP-3.
               10  WS-OW-COMM-PR-ADJ  PIC S9(9)V99  COMP-3.


       01  WS-INIT-EXTRACT             PIC X(792).
       01  EXTRACT-RECORD.
           05  EXT-MOE-DATE            PIC X(10).
           05  EXT-TAB1                PIC X.
           05  EXT-CARRIER             PIC X.
           05  EXT-TAB2                PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-TAB3                PIC X.
           05  EXT-STATE               PIC XX.
           05  EXT-TAB4                PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-TAB5                PIC X.
           05  EXT-EFF-DATE            PIC X(10).
           05  EXT-TAB6                PIC X.
           05  EXT-EXP-DATE            PIC X(10).
           05  EXT-TAB7                PIC X.
           05  EXT-REPORT-CDE1         PIC X(10).
           05  EXT-TAB8                PIC X.
           05  EXT-REPORT-CDE2         PIC X(10).
           05  EXT-TAB9                PIC X.
           05  EXT-REPORT-CDE3         PIC X(10).
           05  EXT-TAB10               PIC X.
           05  EXT-REIN-CO             PIC X(3).
           05  EXT-TAB11               PIC X.
           05  EXT-REIN-SUB            PIC X(3).
           05  EXT-TAB12               PIC X.
           05  EXT-REIN-TABLE          PIC X(3).
           05  EXT-TAB13               PIC X.
           05  EXT-REIN-NAME           PIC X(30).
           05  EXT-TAB14               PIC X.
           05  EXT-RETRO-POOL          PIC X(6).
           05  EXT-TAB15               PIC X.
           05  EXT-BEN-TYPE            PIC X(1).
           05  EXT-TAB16               PIC X.
           05  EXT-BEN-CODE            PIC X(2).
           05  EXT-TAB17               PIC X.
           05  EXT-YTD-ISS-CNT         PIC -------9.
           05  EXT-TAB18               PIC X.
           05  EXT-YTD-ISS-BEN         PIC -----------9.99.
           05  EXT-TAB19               PIC X.
           05  EXT-YTD-ISS-BEN-GROSS   PIC -----------9.99.
           05  EXT-TAB20               PIC X.
           05  EXT-YTD-CNC-CNT         PIC -------9.
           05  EXT-TAB21               PIC X.
           05  EXT-YTD-CNC-BEN         PIC -----------9.99.
           05  EXT-TAB22               PIC X.
           05  EXT-YTD-CNC-BEN-GROSS   PIC -----------9.99.
           05  EXT-TAB23               PIC X.
           05  EXT-YTD-ISS-PRM         PIC ---------9.99.
           05  EXT-TAB24               PIC X.
           05  EXT-YTD-ISS-PRM-GROSS   PIC ---------9.99.
           05  EXT-TAB25               PIC X.
           05  EXT-YTD-CNC-PRM         PIC ---------9.99.
           05  EXT-TAB26               PIC X.
           05  EXT-YTD-CNC-PRM-GROSS   PIC ---------9.99.
           05  EXT-TAB27               PIC X.
           05  EXT-YTD-PRM-78          PIC ---------9.99.
           05  EXT-TAB28               PIC X.
           05  EXT-YTD-PRM-PR          PIC ---------9.99.
           05  EXT-TAB29               PIC X.
           05  EXT-YTD-PRM-ST          PIC ---------9.99.
           05  EXT-TAB30               PIC X.
           05  EXT-YTD-CLM-AMT         PIC ---------9.99.
           05  EXT-TAB31               PIC X.
           05  EXT-YTD-CLM-CNT         PIC -------9.
           05  EXT-TAB32               PIC X.
           05  EXT-YTD-CLM-CRT         PIC -------9.
           05  EXT-TAB33               PIC X.
           05  EXT-YTD-CLM-DU          PIC -------9.99.
           05  EXT-TAB34               PIC X.
           05  EXT-YTD-CLM-PV          PIC -------9.99.
           05  EXT-TAB35               PIC X.
           05  EXT-YTD-CLM-IBNR        PIC -------9.99.
           05  EXT-TAB36               PIC X.
           05  EXT-YTD-LOSS-RESV       PIC -------9.99.
           05  EXT-TAB37               PIC X.
           05  EXT-YTD-CLAIM-ADJ       PIC -------9.99.
           05  EXT-TAB38               PIC X.
           05  EXT-YTD-RETRO-EXP       PIC -------9.99.
           05  EXT-TAB39               PIC X.
           05  EXT-YTD-RETRO-PMT       PIC -------9.99.
           05  EXT-TAB40               PIC X.
           05  EXT-YTD-RETRO-OTH-COMM  PIC -------9.99.
           05  EXT-TAB41               PIC X.
           05  EXT-YTD-MORT-RESV       PIC -----------9.9(6).
           05  EXT-TAB42               PIC X.
           05  EXT-YTD-IN-FORCE        PIC -----------9.99.
           05  EXT-TAB43               PIC X.
           05  EXT-YTD-ADJUST          PIC -------9.99.
           05  EXT-TAB44               PIC X.
           05  EXT-YTD-LIFE-YEARS      PIC ---------9.
           05  EXT-TAB45               PIC X.
           05  EXT-YTD-CLM-EXP         PIC ---------9.99.
           05  EXT-TAB46               PIC X.
           05  EXT-YTD-PRM-78-ADJ      PIC ---------9.99.
           05  EXT-TAB47               PIC X.
           05  EXT-YTD-PRM-PR-ADJ      PIC ---------9.99.
           05  EXT-TAB48               PIC X.
           05  EXT-YTD-PRM-ST-ADJ      PIC ---------9.99.
           05  EXT-TAB49               PIC X.
           05  EXT-YTD-PRM-TAX         PIC -------9.99.
           05  EXT-TAB50               PIC X.
           05  EXT-YTD-INFORCE-CNT     PIC ---------9.
           05  EXT-TAB51               PIC X.
           05  EXT-YTD-AGT-ISS-COMM    PIC ---------9.99.
           05  EXT-TAB52               PIC X.
           05  EXT-YTD-AGT-CNC-COMM    PIC ---------9.99.
           05  EXT-TAB53               PIC X.
           05  EXT-YTD-AGT-COMM-78     PIC ---------9.99.
           05  EXT-TAB54               PIC X.
           05  EXT-YTD-AGT-COMM-PR     PIC ---------9.99.
           05  EXT-TAB55               PIC X.
           05  EXT-YTD-AGT-COMM-ST     PIC ---------9.99.
           05  EXT-TAB56               PIC X.
           05  EXT-YTD-AGT-COMM-78-ADJ PIC ---------9.99.
           05  EXT-TAB57               PIC X.
           05  EXT-YTD-AGT-COMM-PR-ADJ PIC ---------9.99.
           05  EXT-TAB58               PIC X.
           05  EXT-YTD-OW-ISS-COMM     PIC ---------9.99.
           05  EXT-TAB59               PIC X.
           05  EXT-YTD-OW-CNC-COMM     PIC ---------9.99.
           05  EXT-TAB60               PIC X.
           05  EXT-YTD-OW-COMM-78      PIC ---------9.99.
           05  EXT-TAB61               PIC X.
           05  EXT-YTD-OW-COMM-PR      PIC ---------9.99.
           05  EXT-TAB62               PIC X.
           05  EXT-YTD-OW-COMM-ST      PIC ---------9.99.
           05  EXT-TAB63               PIC X.
           05  EXT-YTD-OW-COMM-78-ADJ  PIC ---------9.99.
           05  EXT-TAB64               PIC X.
           05  EXT-YTD-OW-COMM-PR-ADJ  PIC ---------9.99.
           05  EXT-TAB65               PIC X.
           05  EXT-EP-CODE             PIC X.
           05  EXT-TAB66               PIC X.
           05  EXT-REF-METH            PIC X.
           05  EXT-TAB67               PIC X.
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

           SORT SORT-EPECS ON ASCENDING KEY
                                    SRT-CARRIER
                                    SRT-GROUP
                                    SRT-STATE
                                    SRT-ACCOUNT
                                    SRT-EFF-DATE
                                    SRT-EXP-DATE
                                    SRT-REPORT-CDE1
                                    SRT-REPORT-CDE2
                                    SRT-REPORT-CDE3
                                    SRT-BEN-TYPE
                                    SRT-BEN-CODE

                INPUT PROCEDURE 0002-INPUT THRU 0002-EXIT
                OUTPUT PROCEDURE 0003-OUTPUT THRU 0003-EXIT

           GOBACK

           .
       0002-INPUT.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0075-READ-ERACCT    THRU 0075-EXIT

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT
           MOVE WS-CURRENT-KEY         TO WS-PREVIOUS-KEY
           MOVE WS-ACCT-STUFF          TO WS-PREV-ACCT-STUFF

           PERFORM 0077-PROCESS-EPEC   THRU 0077-EXIT UNTIL
                 (END-OF-INPUT)
      *           OR (EPEC-IN-CNT > 5000)

           PERFORM 0085-RELEASE-EXTRACT
                                       THRU 0085-EXIT
           DISPLAY ' END OF SORT INPUT '
           .
       0002-EXIT.
           EXIT.


       0003-OUTPUT SECTION.

           DISPLAY ' BEGIN SORT OUTPUT '
           MOVE SPACES                 TO WS-EOF-SW

           PERFORM 0110-RETURN         THRU 0110-EXIT

           MOVE SORT-RECORD (1:71)     TO WS-CURRENT-KEY
                                          WS-PREVIOUS-KEY
                                          
           MOVE SRT-REIN-TABLE         TO WS-PREV-REIN-TABLE
           MOVE SRT-RETRO-POOL         TO WS-PREV-RETRO-POOL
           MOVE SRT-EP-CODE            TO WS-PREV-EP-CODE
           MOVE SRT-REF-METH           TO WS-PREV-REF-METH

           PERFORM 0015-INIT-TABLE     THRU 0015-EXIT

           PERFORM 0100-PROCESS-RETURN THRU 0100-EXIT UNTIL
                END-OF-INPUT

           PERFORM 0190-BUILD-EXTRACT  THRU 0190-EXIT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           .
       0003-EXIT.
           EXIT.



       0010-INITIALIZE.

           MOVE SPACES                 TO EXTRACT-RECORD
           MOVE ';'                    TO EXT-TAB1
                                          EXT-TAB2
                                          EXT-TAB3
                                          EXT-TAB4
                                          EXT-TAB5
                                          EXT-TAB6
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
                                          EXT-TAB61
                                          EXT-TAB62
                                          EXT-TAB63
                                          EXT-TAB64
                                          EXT-TAB65
                                          EXT-TAB66
                                          EXT-TAB67

           MOVE 'E'                    TO EXT-EOR

           MOVE RUN-DATE               TO WS-WORK-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO EXT-MOE-DATE
           END-STRING

           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT
           
           PERFORM 0015-INIT-TABLE     THRU 0015-EXIT
           MOVE LOW-VALUES             TO WS-PREVIOUS-KEY
                                          WS-CURRENT-KEY
           MOVE SPACES                 TO WS-ACCT-STUFF
                                          WS-PREV-ACCT-STUFF
           
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

           .
       0010-EXIT.
           EXIT.

       0015-INIT-TABLE.
        
      *    INITIALIZE (1 THRU 10)
      
           PERFORM VARYING WS-S2 FROM +1 BY +1 UNTIL
              WS-S2 > +10
              MOVE +0                  TO WS-ISS-CNT (WS-S2)     
                                          WS-ISS-BEN (WS-S2)    
                                          WS-ISS-BEN-GROSS (WS-S2)
                                          WS-CNC-CNT (WS-S2)
                                          WS-CNC-BEN (WS-S2)
                                          WS-CNC-BEN-GROSS (WS-S2)
                                          WS-ISS-PRM (WS-S2)
                                          WS-ISS-PRM-GROSS (WS-S2)
                                          WS-CNC-PRM (WS-S2)
                                          WS-CNC-PRM-GROSS (WS-S2)
                                          WS-PRM-78 (WS-S2)
                                          WS-PRM-PR (WS-S2)
                                          WS-PRM-ST (WS-S2)
                                          WS-CLM-AMT (WS-S2)
                                          WS-CLM-CNT (WS-S2)
                                          WS-CLM-CRT (WS-S2)
                                          WS-CLM-DU (WS-S2)
                                          WS-CLM-PV (WS-S2)
                                          WS-CLM-IBNR (WS-S2)
                                          WS-LOSS-RESV (WS-S2)
                                          WS-CLAIM-ADJ (WS-S2)
                                          WS-RETRO-EXP (WS-S2)
                                          WS-RETRO-PMT (WS-S2)
                                          WS-RETRO-OTH-COMM (WS-S2)
                                          WS-MORT-RESV (WS-S2)
                                          WS-IN-FORCE (WS-S2)
                                          WS-ADJUST (WS-S2)
                                          WS-LIFE-YEARS (WS-S2)
                                          WS-CLM-EXP (WS-S2)
                                          WS-PRM-78-ADJ (WS-S2)
                                          WS-PRM-PR-ADJ (WS-S2)
                                          WS-PRM-ST-ADJ (WS-S2)
                                          WS-PRM-TAX (WS-S2)
                                          WS-INFORCE-CNT (WS-S2)
                                          WS-AGT-ISS-COMM (WS-S2)
                                          WS-AGT-CNC-COMM (WS-S2)
                                          WS-AGT-COMM-78 (WS-S2)
                                          WS-AGT-COMM-PR (WS-S2)
                                          WS-AGT-COMM-ST (WS-S2)
                                          WS-AGT-COMM-78-ADJ (WS-S2)
                                          WS-AGT-COMM-PR-ADJ (WS-S2)
                                          WS-OW-ISS-COMM (WS-S2)
                                          WS-OW-CNC-COMM (WS-S2)
                                          WS-OW-COMM-78 (WS-S2)
                                          WS-OW-COMM-PR (WS-S2)
                                          WS-OW-COMM-ST (WS-S2)
                                          WS-OW-COMM-78-ADJ (WS-S2)
                                          WS-OW-COMM-PR-ADJ (WS-S2)
           END-PERFORM

           .
       0015-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EPECS ERACCTT ERRTBLT
               OUTPUT EXTRACT REINEXT

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' BAD OPEN FOR ERACCT ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERRTBL-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' BAD OPEN FOR ERRTBL ' ERRTBL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' EPEC IN RECORDS  ' EPEC-IN-CNT
           DISPLAY ' RECORDS RELEASED ' WS-REL-RECS
           DISPLAY ' RECORDS RETURNED ' WS-RET-RECS
           DISPLAY ' EXTR OUT RECORDS ' EXTR-OUT-CNT
           DISPLAY ' REIN OUT RECORDS ' REIN-EXTR-OUT-CNT
           CLOSE EPECS EXTRACT ERACCTT ERRTBLT REINEXT

           .
       0030-EXIT.
           EXIT.

       0060-READ-EPEC.

           READ EPECS AT END
               SET END-OF-INPUT        TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO EPEC-IN-CNT
              MOVE EP-CONTROL          TO WS-CONTROL
              MOVE EP-EFF-DTE          TO WS-CONT-EFF-DATE
              MOVE EP-EXP-DTE          TO WS-CONT-EXP-DATE
              MOVE EP-REIN             TO WS-REIN
              MOVE EP-REINCO           TO WS-REIN-CO
              MOVE EP-REINCO-SUB       TO WS-REIN-SUB
              MOVE EP-RCD-TYPE         TO WS-BEN-TYPE
              MOVE EP-BEN-CODE         TO WS-BEN-CODE
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
              MOVE CLAS-I-REFUND-METHOD (CLAS-INDEXL)
                                       TO WS-REF-METH
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
              MOVE CLAS-I-REFUND-METHOD (CLAS-INDEXA)
                                       TO WS-REF-METH
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
       
           if  AM-ACCOUNT = '0600001970'
               and am-effect-dt = 20100110
                  display 'fixing date for 0600001970'
                  move 20101001 to am-effect-dt
           end-if.
           
           IF (AM-CONTROL-A = EP-CNTRL-1)
              AND (AM-EFFECT-DT = EP-EFF-DTE)
      *        AND ((AM-EXPIRE-DT = EP-EXP-DTE) OR
      *             (EP-EXP-DTE = '99999999999'))
                MOVE AM-REPORT-CODE-1    TO WS-RPTCD1
                MOVE AM-REPORT-CODE-2    TO WS-RPTCD2
                MOVE AM-REPORT-CODE-3    TO WS-RPTCD3
                MOVE AM-REI-TABLE        TO WS-REIN-TABLE
                MOVE AM-RETRO-POOL       TO WS-RETRO-POOL
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
              PERFORM 0085-RELEASE-EXTRACT
                                       THRU 0085-EXIT
              PERFORM 0015-INIT-TABLE  THRU 0015-EXIT
              MOVE WS-CURRENT-KEY      TO WS-PREVIOUS-KEY
              MOVE WS-ACCT-STUFF       TO WS-PREV-ACCT-STUFF
           END-IF

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

           COMPUTE WS-ISS-CNT (WS-S2) = 
               WS-ISS-CNT (WS-S2) + EP-ISS-CNT

           COMPUTE WS-ISS-BEN (WS-S2) = 
               WS-ISS-BEN (WS-S2) + EP-ISS-BEN

           COMPUTE WS-ISS-BEN-GROSS (WS-S2) =
               WS-ISS-BEN-GROSS (WS-S2) + EP-ISS-BEN-GROSS
               
           COMPUTE WS-CNC-CNT (WS-S2) = 
               WS-CNC-CNT (WS-S2) + EP-CNC-CNT

           COMPUTE WS-CNC-BEN (WS-S2) = 
               WS-CNC-BEN (WS-S2) + EP-CNC-BEN

           COMPUTE WS-CNC-BEN-GROSS (WS-S2) =
               WS-CNC-BEN-GROSS (WS-S2) + EP-CNC-BEN-GROSS

           COMPUTE WS-ISS-PRM (WS-S2) = 
               WS-ISS-PRM (WS-S2) + EP-ISS-PRM

           COMPUTE WS-ISS-PRM-GROSS (WS-S2) = 
               WS-ISS-PRM-GROSS (WS-S2) + EP-ISS-PRM-GROSS

           COMPUTE WS-CNC-PRM (WS-S2) = 
               WS-CNC-PRM (WS-S2) + EP-CNC-PRM

           COMPUTE WS-CNC-PRM-GROSS (WS-S2) = 
               WS-CNC-PRM-GROSS (WS-S2) + EP-CNC-PRM-GROSS

           COMPUTE WS-PRM-78 (WS-S2) = 
               WS-PRM-78 (WS-S2) + EP-PRM-78

           COMPUTE WS-PRM-PR (WS-S2) = 
               WS-PRM-PR (WS-S2) + EP-PRM-PR
               
           COMPUTE WS-PRM-ST (WS-S2) = 
               WS-PRM-ST (WS-S2) + EP-PRM-ST

           COMPUTE WS-CLM-AMT (WS-S2) = 
               WS-CLM-AMT (WS-S2) + EP-CLM-AMT

           COMPUTE WS-CLM-CNT (WS-S2) = 
               WS-CLM-CNT (WS-S2) + EP-CLM-CNT
               
           COMPUTE WS-CLM-CRT (WS-S2) = 
               WS-CLM-CRT (WS-S2) + EP-CLM-CRT
               
           COMPUTE WS-CLM-DU (WS-S2) = 
               WS-CLM-DU (WS-S2) + EP-CLM-DU
               
           COMPUTE WS-CLM-PV (WS-S2) = 
               WS-CLM-PV (WS-S2) + EP-CLM-PV
               
           COMPUTE WS-CLM-IBNR (WS-S2) = 
               WS-CLM-IBNR (WS-S2) + EP-CLM-IBNR
               
           COMPUTE WS-LOSS-RESV (WS-S2) = 
               WS-LOSS-RESV (WS-S2) + EP-LOSS-RESV
               
           COMPUTE WS-CLAIM-ADJ (WS-S2) = 
               WS-CLAIM-ADJ (WS-S2) + EP-CLAIM-ADJ
               
           COMPUTE WS-RETRO-EXP (WS-S2) = 
               WS-RETRO-EXP (WS-S2) + EP-RETRO-EXPENSES
               
           COMPUTE WS-RETRO-PMT (WS-S2) = 
               WS-RETRO-PMT (WS-S2) + EP-RETRO-PAYMENTS
               
           COMPUTE WS-RETRO-OTH-COMM (WS-S2) = 
               WS-RETRO-OTH-COMM (WS-S2) + EP-RETRO-OTH-COMM
               
           COMPUTE WS-MORT-RESV (WS-S2) = 
               WS-MORT-RESV (WS-S2) + EP-MORT-RESV
               
           COMPUTE WS-IN-FORCE (WS-S2) = 
               WS-IN-FORCE (WS-S2) + EP-IN-FORCE
               
           COMPUTE WS-ADJUST (WS-S2) = 
               WS-ADJUST (WS-S2) + EP-ADJUST
               
           IF EP-LIFE-YEARS NOT NUMERIC
               MOVE ZEROS TO EP-LIFE-YEARS
           END-IF
           
           COMPUTE WS-LIFE-YEARS (WS-S2) = 
               WS-LIFE-YEARS (WS-S2) + EP-LIFE-YEARS
           
           IF EP-CLM-EXP NOT NUMERIC
               MOVE ZEROS TO EP-CLM-EXP
           END-IF
               
           COMPUTE WS-CLM-EXP (WS-S2) = 
               WS-CLM-EXP (WS-S2) + EP-CLM-EXP
               
           COMPUTE WS-PRM-78-ADJ (WS-S2) = 
               WS-PRM-78-ADJ (WS-S2) + EP-PRM-78-ADJ
               
           COMPUTE WS-PRM-PR-ADJ (WS-S2) = 
               WS-PRM-PR-ADJ (WS-S2) + EP-PRM-PR-ADJ
               
           COMPUTE WS-PRM-ST-ADJ (WS-S2) = 
               WS-PRM-ST-ADJ (WS-S2) + EP-PRM-ST-ADJ
               
           COMPUTE WS-PRM-TAX (WS-S2) = 
               WS-PRM-TAX (WS-S2) + EP-PRM-TAX
               
           COMPUTE WS-INFORCE-CNT (WS-S2) = 
               WS-INFORCE-CNT (WS-S2) + EP-INFORCE-CNT

           .
       0082-EXIT.
           EXIT.

       0083-ACCUM-EC-TOTS.

           PERFORM VARYING A1 FROM +1 BY +1 UNTIL
              (A1 > +5)
052214        IF EC-AGT-TYPE (A1) = 'C' OR 'D'
                 COMPUTE WS-AGT-ISS-COMM (WS-S2) =
                    WS-AGT-ISS-COMM (WS-S2) + EC-ISS-COMM (A1)
                 COMPUTE WS-AGT-CNC-COMM (WS-S2) =
                    WS-AGT-CNC-COMM (WS-S2) + EC-CNC-COMM (A1)
                 COMPUTE WS-AGT-COMM-78 (WS-S2) =
                    WS-AGT-COMM-78 (WS-S2) + EC-COMM-78 (A1)
                 COMPUTE WS-AGT-COMM-PR (WS-S2) =
                    WS-AGT-COMM-PR (WS-S2) + EC-COMM-PR (A1)
                 COMPUTE WS-AGT-COMM-ST (WS-S2) =
                    WS-AGT-COMM-ST (WS-S2) + EC-COMM-ST (A1)
                 COMPUTE WS-AGT-COMM-78-ADJ (WS-S2) =
                    WS-AGT-COMM-78-ADJ (WS-S2) + EC-COMM-78-ADJ (A1)
                 COMPUTE WS-AGT-COMM-PR-ADJ (WS-S2) =
                    WS-AGT-COMM-PR-ADJ (WS-S2) + EC-COMM-PR-ADJ (A1)
              ELSE
052214          IF EC-AGT-TYPE (A1) = 'O' OR 'P'
                  COMPUTE WS-OW-ISS-COMM (WS-S2) =
                     WS-OW-ISS-COMM (WS-S2) + EC-ISS-COMM (A1)
                  COMPUTE WS-OW-CNC-COMM (WS-S2) =
                     WS-OW-CNC-COMM (WS-S2) + EC-CNC-COMM (A1)
                  COMPUTE WS-OW-COMM-78 (WS-S2) =
                     WS-OW-COMM-78 (WS-S2) + EC-COMM-78 (A1)
                  COMPUTE WS-OW-COMM-PR (WS-S2) =
                     WS-OW-COMM-PR (WS-S2) + EC-COMM-PR (A1)
                  COMPUTE WS-OW-COMM-ST (WS-S2) =
                     WS-OW-COMM-ST (WS-S2) + EC-COMM-ST (A1)
                  COMPUTE WS-OW-COMM-78-ADJ (WS-S2) =
                     WS-OW-COMM-78-ADJ (WS-S2) + EC-COMM-78-ADJ (A1)
                  COMPUTE WS-OW-COMM-PR-ADJ (WS-S2) =
                     WS-OW-COMM-PR-ADJ (WS-S2) + EC-COMM-PR-ADJ (A1)
                END-IF
              END-IF
           END-PERFORM

           .
       0083-EXIT.
           EXIT.

       0085-RELEASE-EXTRACT.

           MOVE WS-PREV-CARRIER        TO SRT-CARRIER
           MOVE WS-PREV-GROUP          TO SRT-GROUP
           MOVE WS-PREV-STATE          TO SRT-STATE
           MOVE WS-PREV-ACCOUNT        TO SRT-ACCOUNT
           MOVE WS-PREV-EXP-DATE       TO SRT-EXP-DATE
           MOVE WS-PREV-EFF-DATE       TO SRT-EFF-DATE
           MOVE WS-PREV-REIN           TO SRT-REIN
           MOVE WS-PREV-REIN-CO        TO SRT-REIN-CO
           MOVE WS-PREV-REIN-SUB       TO SRT-REIN-SUB
           MOVE WS-PREV-BEN-TYPE       TO SRT-BEN-TYPE
           MOVE WS-PREV-BEN-CODE       TO SRT-BEN-CODE

           MOVE WS-PREV-RPTCD1         TO SRT-REPORT-CDE1
           MOVE WS-PREV-RPTCD2         TO SRT-REPORT-CDE2
           MOVE WS-PREV-RPTCD3         TO SRT-REPORT-CDE3
           
           MOVE WS-PREV-REIN-TABLE     TO SRT-REIN-TABLE
           MOVE WS-PREV-RETRO-POOL     TO SRT-RETRO-POOL
           MOVE WS-PREV-EP-CODE        TO SRT-EP-CODE
           MOVE WS-PREV-REF-METH       TO SRT-REF-METH
           
           MOVE WS-ACCUM-TOTALS        TO SRT-ACCUM-TOTALS

           RELEASE SORT-RECORD
           ADD 1                       TO WS-REL-RECS

           .
       0085-EXIT.
           EXIT.

       0100-PROCESS-RETURN.

           IF WS-CURRENT-KEY NOT = WS-PREVIOUS-KEY
              PERFORM 0190-BUILD-EXTRACT
                                       THRU 0190-EXIT
              PERFORM 0015-INIT-TABLE  THRU 0015-EXIT
              MOVE WS-CURRENT-KEY      TO WS-PREVIOUS-KEY
              MOVE SRT-REIN-TABLE      TO WS-PREV-REIN-TABLE
              MOVE SRT-RETRO-POOL      TO WS-PREV-RETRO-POOL
              MOVE SRT-EP-CODE         TO WS-PREV-EP-CODE
              MOVE SRT-REF-METH        TO WS-REF-METH
           END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +10
              ADD SRT-ISS-CNT (S1)       TO  WS-ISS-CNT (S1)       
              ADD SRT-ISS-BEN (S1)       TO  WS-ISS-BEN (S1)       
              ADD SRT-ISS-BEN-GROSS (S1) TO  WS-ISS-BEN-GROSS (S1) 
              ADD SRT-CNC-CNT (S1)       TO  WS-CNC-CNT (S1)       
              ADD SRT-CNC-BEN (S1)       TO  WS-CNC-BEN (S1)       
              ADD SRT-CNC-BEN-GROSS (S1) TO  WS-CNC-BEN-GROSS (S1) 
              ADD SRT-ISS-PRM (S1)       TO  WS-ISS-PRM (S1)       
              ADD SRT-ISS-PRM-GROSS (S1) TO  WS-ISS-PRM-GROSS (S1) 
              ADD SRT-CNC-PRM (S1)       TO  WS-CNC-PRM (S1)       
              ADD SRT-CNC-PRM-GROSS (S1) TO  WS-CNC-PRM-GROSS (S1) 
              ADD SRT-PRM-78 (S1)        TO  WS-PRM-78 (S1)        
              ADD SRT-PRM-PR (S1)        TO  WS-PRM-PR (S1)        
              ADD SRT-PRM-ST (S1)        TO  WS-PRM-ST (S1)        
              ADD SRT-CLM-AMT (S1)       TO  WS-CLM-AMT (S1)       
              ADD SRT-CLM-CNT (S1)       TO  WS-CLM-CNT (S1)       
              ADD SRT-CLM-CRT (S1)       TO  WS-CLM-CRT (S1)       
              ADD SRT-CLM-DU (S1)        TO  WS-CLM-DU (S1)        
              ADD SRT-CLM-PV (S1)        TO  WS-CLM-PV (S1)        
              ADD SRT-CLM-IBNR (S1)      TO  WS-CLM-IBNR (S1)      
              ADD SRT-LOSS-RESV (S1)     TO  WS-LOSS-RESV (S1)     
              ADD SRT-CLAIM-ADJ (S1)     TO  WS-CLAIM-ADJ (S1)     
              ADD SRT-RETRO-EXP (S1)     TO  WS-RETRO-EXP (S1)     
              ADD SRT-RETRO-PMT (S1)     TO  WS-RETRO-PMT (S1)     
              ADD SRT-RETRO-OTH-COMM (S1) TO WS-RETRO-OTH-COMM (S1)
              ADD SRT-MORT-RESV (S1)     TO  WS-MORT-RESV (S1)     
              ADD SRT-IN-FORCE (S1)      TO  WS-IN-FORCE (S1)      
              ADD SRT-ADJUST (S1)        TO  WS-ADJUST (S1)        
              ADD SRT-LIFE-YEARS (S1)    TO  WS-LIFE-YEARS (S1)    
              ADD SRT-CLM-EXP (S1)       TO  WS-CLM-EXP (S1)       
              ADD SRT-PRM-78-ADJ (S1)    TO  WS-PRM-78-ADJ (S1)    
              ADD SRT-PRM-PR-ADJ (S1)    TO  WS-PRM-PR-ADJ (S1)    
              ADD SRT-PRM-ST-ADJ (S1)    TO  WS-PRM-ST-ADJ (S1)    
              ADD SRT-PRM-TAX (S1)       TO  WS-PRM-TAX (S1)       
              ADD SRT-INFORCE-CNT (S1)   TO  WS-INFORCE-CNT (S1)   
              ADD SRT-AGT-ISS-COMM (S1)  TO  WS-AGT-ISS-COMM (S1)  
              ADD SRT-AGT-CNC-COMM (S1)  TO  WS-AGT-CNC-COMM (S1)  
              ADD SRT-AGT-COMM-78 (S1)   TO  WS-AGT-COMM-78 (S1)   
              ADD SRT-AGT-COMM-PR (S1)   TO  WS-AGT-COMM-PR (S1)   
              ADD SRT-AGT-COMM-ST (S1)   TO  WS-AGT-COMM-ST (S1)   
              ADD SRT-AGT-COMM-78-ADJ (S1) TO WS-AGT-COMM-78-ADJ (S1)
              ADD SRT-AGT-COMM-PR-ADJ (S1) TO WS-AGT-COMM-PR-ADJ (S1)
              ADD SRT-OW-ISS-COMM (S1)   TO  WS-OW-ISS-COMM (S1)  
              ADD SRT-OW-CNC-COMM (S1)   TO  WS-OW-CNC-COMM (S1)  
              ADD SRT-OW-COMM-78 (S1)    TO  WS-OW-COMM-78 (S1) 
              ADD SRT-OW-COMM-PR (S1)    TO  WS-OW-COMM-PR (S1) 
              ADD SRT-OW-COMM-ST (S1)    TO  WS-OW-COMM-ST (S1)    
              ADD SRT-OW-COMM-78-ADJ (S1) TO WS-OW-COMM-78-ADJ (S1)
              ADD SRT-OW-COMM-PR-ADJ (S1) TO WS-OW-COMM-PR-ADJ (S1)
           END-PERFORM

           PERFORM 0110-RETURN         THRU 0110-EXIT

           .
       0100-EXIT.
           EXIT.

       0110-RETURN.

           RETURN SORT-EPECS    AT END
               SET END-OF-INPUT        TO TRUE
           END-RETURN

           IF NOT END-OF-INPUT
              MOVE SORT-RECORD (1:71)  TO WS-CURRENT-KEY
              ADD 1                    TO WS-RET-RECS
           END-IF

           .
       0110-EXIT.
           EXIT.




       0190-BUILD-EXTRACT.

           MOVE WS-INIT-EXTRACT        TO EXTRACT-RECORD
           MOVE WS-PREV-CARRIER        TO EXT-CARRIER
           MOVE WS-PREV-GROUP          TO EXT-GROUP
           MOVE WS-PREV-STATE          TO EXT-STATE
           MOVE WS-PREV-ACCOUNT        TO EXT-ACCOUNT
           IF WS-PREV-EXP-DATE = 99999999999
              MOVE 99999991231         TO WS-WORK-DATE
           ELSE
              MOVE WS-PREV-EXP-DATE    TO WS-WORK-DATE
           END-IF
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO EXT-EXP-DATE
           MOVE WS-PREV-EFF-DATE       TO WS-WORK-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO EXT-EFF-DATE
           MOVE WS-PREV-REIN-CO        TO EXT-REIN-CO
           MOVE WS-PREV-REIN-SUB       TO EXT-REIN-SUB
           MOVE WS-PREV-BEN-TYPE       TO EXT-BEN-TYPE
           MOVE WS-PREV-BEN-CODE       TO EXT-BEN-CODE

           MOVE SPACES                 TO EXT-REIN-NAME
           IF WS-PREV-REIN-CO GREATER THAN SPACES
               MOVE LOW-VALUES TO RE-CONTROL-PRIMARY
               MOVE SPACES TO RE-NAME
               MOVE DTE-CLASIC-COMPANY-CD TO RE-COMPANY-CD              
               MOVE 'B'                TO  RE-CODE
               MOVE WS-PREV-REIN-CO    TO  RE-COMP-PRIME
                                           WS-HOLD-REIN-PRIME
               MOVE WS-PREV-REIN-SUB   TO  RE-COMP-SUB
                                           WS-HOLD-REIN-SUB
               PERFORM 0350-FIND-REIN-NAME THRU 0350-EXIT
               MOVE RE-NAME            TO  EXT-REIN-NAME
           END-IF
           IF WS-PREV-RPTCD1 EQUAL LOW-VALUES
               MOVE SPACES             TO EXT-REPORT-CDE1
           ELSE
               MOVE WS-PREV-RPTCD1     TO EXT-REPORT-CDE1
           END-IF
           IF WS-PREV-RPTCD2 EQUAL LOW-VALUES
               MOVE SPACES             TO EXT-REPORT-CDE2
           ELSE
               MOVE WS-PREV-RPTCD2     TO EXT-REPORT-CDE2
           END-IF
           IF WS-PREV-RPTCD3 EQUAL LOW-VALUES
               MOVE SPACES             TO EXT-REPORT-CDE3
           ELSE
               MOVE WS-PREV-RPTCD3     TO EXT-REPORT-CDE3
           END-IF
           
           IF WS-PREV-REIN-TABLE EQUAL LOW-VALUES
               MOVE SPACES             TO EXT-REIN-TABLE
           ELSE
               MOVE WS-PREV-REIN-TABLE TO EXT-REIN-TABLE
           END-IF
           IF WS-PREV-REIN-TABLE GREATER THAN SPACES  AND
              WS-PREV-REIN-CO NOT GREATER THAN SPACES
                 MOVE LOW-VALUES TO RE-CONTROL-PRIMARY
                 MOVE DTE-CLASIC-COMPANY-CD TO RE-COMPANY-CD              
                 MOVE 'A'                TO  RE-CODE
                 MOVE WS-PREV-REIN-TABLE TO  RE-TABLE
                 PERFORM 0300-FIND-REIN-TABLE THRU 0300-EXIT
                 MOVE RE-NAME            TO  EXT-REIN-NAME
           END-IF
           
           IF WS-PREV-RETRO-POOL EQUAL LOW-VALUES
               MOVE SPACES             TO EXT-RETRO-POOL
           ELSE
               MOVE WS-PREV-RETRO-POOL TO EXT-RETRO-POOL
           END-IF
           
           IF WS-PREV-EP-CODE EQUAL LOW-VALUES
               MOVE SPACES             TO EXT-EP-CODE
           ELSE
               MOVE WS-PREV-EP-CODE    TO EXT-EP-CODE
           END-IF

           IF WS-PREV-REF-METH EQUAL LOW-VALUES
               MOVE SPACES             TO EXT-REF-METH
           ELSE
               MOVE WS-PREV-REF-METH   TO EXT-REF-METH
           END-IF

           PERFORM 0195-MOVE-TABLE     THRU 0195-EXIT
           IF WS-PREV-REIN = 'R'
               PERFORM 0250-WRITE-REIN-EXTRACT THRU 0250-EXIT
           ELSE
               PERFORM 0200-WRITE-EXTRACT  THRU 0200-EXIT
           END-IF

           .
       0190-EXIT.
           EXIT.

       0195-MOVE-TABLE.

      *********** Y T D *********
           
           COMPUTE EXT-YTD-ISS-CNT =        WS-ISS-CNT (1)        - 
                                WS-ISS-CNT (3)         
           COMPUTE EXT-YTD-ISS-BEN =        WS-ISS-BEN (1)        - 
                                WS-ISS-BEN (3)         
           COMPUTE EXT-YTD-ISS-BEN-GROSS =  WS-ISS-BEN-GROSS (1)  - 
                                WS-ISS-BEN-GROSS (3)   
           COMPUTE EXT-YTD-CNC-CNT =        WS-CNC-CNT (1)        - 
                                WS-CNC-CNT (3)         
           COMPUTE EXT-YTD-CNC-BEN =        WS-CNC-BEN (1)        - 
                                WS-CNC-BEN (3)         
           COMPUTE EXT-YTD-CNC-BEN-GROSS =  WS-CNC-BEN-GROSS (1)  - 
                                WS-CNC-BEN-GROSS (3)   
           COMPUTE EXT-YTD-ISS-PRM =        WS-ISS-PRM (1)        - 
                                WS-ISS-PRM (3)         
           COMPUTE EXT-YTD-ISS-PRM-GROSS =  WS-ISS-PRM-GROSS (1)  - 
                                WS-ISS-PRM-GROSS (3)   
           COMPUTE EXT-YTD-CNC-PRM =        WS-CNC-PRM (1)        -
                                WS-CNC-PRM (3)         
           COMPUTE EXT-YTD-CNC-PRM-GROSS =  WS-CNC-PRM-GROSS (1)  - 
                                WS-CNC-PRM-GROSS (3)   
           COMPUTE EXT-YTD-PRM-78 =         WS-PRM-78 (1)         - 
                                WS-PRM-78 (3)          
           COMPUTE EXT-YTD-PRM-PR =         WS-PRM-PR (1)         - 
                                WS-PRM-PR (3)          
           COMPUTE EXT-YTD-PRM-ST =         WS-PRM-ST (1)         - 
                                WS-PRM-ST (3)          
           COMPUTE EXT-YTD-CLM-AMT=         WS-CLM-AMT (1)        - 
                                WS-CLM-AMT (3)         
           COMPUTE EXT-YTD-CLM-CNT=         WS-CLM-CNT (1)        - 
                                WS-CLM-CNT (3)         
           COMPUTE EXT-YTD-CLM-CRT=         WS-CLM-CRT (1)        - 
                                WS-CLM-CRT (3)         
           COMPUTE EXT-YTD-CLM-DU =         WS-CLM-DU (1)         - 
                                WS-CLM-DU (3)          
           COMPUTE EXT-YTD-CLM-PV =         WS-CLM-PV (1)         - 
                                WS-CLM-PV (3)          
           COMPUTE EXT-YTD-CLM-IBNR =       WS-CLM-IBNR (1)       - 
                                WS-CLM-IBNR (3)        
           COMPUTE EXT-YTD-LOSS-RESV =      WS-LOSS-RESV (1)      - 
                                WS-LOSS-RESV (3)       
           COMPUTE EXT-YTD-CLAIM-ADJ =      WS-CLAIM-ADJ (1)      - 
                                WS-CLAIM-ADJ (3)       
           COMPUTE EXT-YTD-RETRO-EXP =      WS-RETRO-EXP (1)      - 
                                WS-RETRO-EXP (3)       
           COMPUTE EXT-YTD-RETRO-PMT =      WS-RETRO-PMT (1)      - 
                                WS-RETRO-PMT (3)       
           COMPUTE EXT-YTD-RETRO-OTH-COMM = WS-RETRO-OTH-COMM (1) - 
                                WS-RETRO-OTH-COMM (3)  
           COMPUTE EXT-YTD-MORT-RESV =      WS-MORT-RESV (1)      - 
                                WS-MORT-RESV (3)       
           COMPUTE EXT-YTD-IN-FORCE =       WS-IN-FORCE (1)       - 
                                WS-IN-FORCE (3)        
           COMPUTE EXT-YTD-ADJUST =         WS-ADJUST (1)         - 
                                WS-ADJUST (3)          
           COMPUTE EXT-YTD-LIFE-YEARS =     WS-LIFE-YEARS (1)     - 
                                WS-LIFE-YEARS (3)      
           COMPUTE EXT-YTD-CLM-EXP =        WS-CLM-EXP (1)        - 
                                WS-CLM-EXP (3)         
           COMPUTE EXT-YTD-PRM-78-ADJ =     WS-PRM-78-ADJ (1)     - 
                                WS-PRM-78-ADJ (3)      
           COMPUTE EXT-YTD-PRM-PR-ADJ =     WS-PRM-PR-ADJ (1)     - 
                                WS-PRM-PR-ADJ (3)      
           COMPUTE EXT-YTD-PRM-ST-ADJ =     WS-PRM-ST-ADJ (1)     - 
                                WS-PRM-ST-ADJ (3)      
           COMPUTE EXT-YTD-PRM-TAX =        WS-PRM-TAX (1)        - 
                                WS-PRM-TAX (3)         
           COMPUTE EXT-YTD-INFORCE-CNT  =   WS-INFORCE-CNT (1)    - 
                                WS-INFORCE-CNT (3)     
           COMPUTE EXT-YTD-AGT-ISS-COMM =   WS-AGT-ISS-COMM (1)   - 
                                WS-AGT-ISS-COMM (3)    
           COMPUTE EXT-YTD-AGT-CNC-COMM =   WS-AGT-CNC-COMM (1)   - 
                                WS-AGT-CNC-COMM (3)    
           COMPUTE EXT-YTD-AGT-COMM-78 =    WS-AGT-COMM-78 (1)    - 
                                WS-AGT-COMM-78 (3)     
           COMPUTE EXT-YTD-AGT-COMM-PR =    WS-AGT-COMM-PR (1)    - 
                                WS-AGT-COMM-PR (3)     
           COMPUTE EXT-YTD-AGT-COMM-ST =    WS-AGT-COMM-ST (1)    - 
                                WS-AGT-COMM-ST (3)     
           COMPUTE EXT-YTD-AGT-COMM-78-ADJ = WS-AGT-COMM-78-ADJ (1) -
                                 WS-AGT-COMM-78-ADJ (3)
           COMPUTE EXT-YTD-AGT-COMM-PR-ADJ = WS-AGT-COMM-PR-ADJ (1) -
                                 WS-AGT-COMM-PR-ADJ (3)
           COMPUTE EXT-YTD-OW-ISS-COMM =    WS-OW-ISS-COMM (1)    - 
                                WS-OW-ISS-COMM (3)     
           COMPUTE EXT-YTD-OW-CNC-COMM =    WS-OW-CNC-COMM (1)    - 
                                WS-OW-CNC-COMM (3)     
           COMPUTE EXT-YTD-OW-COMM-78 =     WS-OW-COMM-78 (1)     - 
                                WS-OW-COMM-78 (3)      
           COMPUTE EXT-YTD-OW-COMM-PR =     WS-OW-COMM-PR (1)     - 
                                WS-OW-COMM-PR (3)      
           COMPUTE EXT-YTD-OW-COMM-ST =     WS-OW-COMM-ST (1)     - 
                                WS-OW-COMM-ST (3)      
           COMPUTE EXT-YTD-OW-COMM-78-ADJ = WS-OW-COMM-78-ADJ (1) - 
                                WS-OW-COMM-78-ADJ (3)  
           COMPUTE EXT-YTD-OW-COMM-PR-ADJ = WS-OW-COMM-PR-ADJ (1) - 
                                WS-OW-COMM-PR-ADJ (3)  
                     

           .
       0195-EXIT.
           EXIT.
           
       0200-WRITE-EXTRACT.

           WRITE EXTRACT-RECORD-OUT    FROM EXTRACT-RECORD
           ADD 1                       TO EXTR-OUT-CNT

           .
       0200-EXIT.
           EXIT.


       0250-WRITE-REIN-EXTRACT.

           WRITE REIN-EXTRACT-RECORD-OUT FROM EXTRACT-RECORD
           ADD 1                       TO REIN-EXTR-OUT-CNT

           .
       0250-EXIT.
           EXIT.

       0300-FIND-REIN-TABLE.

           READ ERRTBLT.

           IF ERRTBL-FILE-STATUS = '00'
               NEXT SENTENCE
           ELSE
               IF ERRTBL-FILE-STATUS = '23'
      *             display 'rein table not found ' ws-prev-rein-table
                   MOVE SPACES   TO  RE-NAME
                   GO TO 0300-EXIT
               ELSE
                  MOVE ' READ ERROR ON REIN TABLE' TO WS-ABEND-MESSAGE
                  MOVE ERRTBL-FILE-STATUS    TO WS-ABEND-FILE-STATUS
                  GO TO ABEND-PGM
               END-IF
           END-IF.
           
           MOVE 'N' TO WS-FOUND
           MOVE +1  TO WS-SUB
           MOVE SPACES TO WS-HOLD-REIN-COMP.
           PERFORM UNTIL WS-FOUND = 'Y' OR WS-SUB > 30
              IF WS-PREV-EXP-DATE NOT LESS THAN RE-LO-DATE (WS-SUB) AND
                 WS-PREV-EFF-DATE NOT GREATER THAN RE-HI-DATE (WS-SUB)
                   MOVE 'Y' TO WS-FOUND
                   MOVE RE-REI-COMP (WS-SUB) TO WS-HOLD-REIN-PRIME
                   MOVE RE-REI-COMP-SUB (WS-SUB) TO WS-HOLD-REIN-SUB
              END-IF
              ADD +1 TO WS-SUB
           END-PERFORM.
           
      *     IF WS-FOUND = 'N'
      *         DISPLAY 'NO REIN NAME FOUND FOR TABLE ' 
      *         ws-prev-rein-table ' '
      *                 WS-PREVIOUS-KEY   
      *     END-IF
           
           IF WS-HOLD-REIN-COMP GREATER THAN SPACES
               MOVE LOW-VALUES TO RE-CONTROL-PRIMARY
               MOVE SPACES TO RE-NAME
               MOVE DTE-CLASIC-COMPANY-CD TO RE-COMPANY-CD              
               MOVE 'B'                TO  RE-CODE
               MOVE WS-HOLD-REIN-PRIME TO  RE-COMP-PRIME
               MOVE WS-HOLD-REIN-SUB   TO  RE-COMP-SUB
               PERFORM 0350-FIND-REIN-NAME THRU 0350-EXIT
           ELSE
               MOVE SPACES TO RE-NAME
           END-IF
           
           .
       0300-EXIT.
           EXIT.

       0350-FIND-REIN-NAME.

           READ ERRTBLT.

           IF ERRTBL-FILE-STATUS = '00'
               NEXT SENTENCE
           ELSE
               IF ERRTBL-FILE-STATUS = '23'
      *             display 'rein name not found ' WS-HOLD-REIN-PRIME 
      *                                  WS-HOLD-REIN-SUB
      *                     ' ' WS-PREVIOUS-KEY             
                   MOVE SPACES   TO  RE-NAME
                   GO TO 0350-EXIT
               ELSE
                  MOVE ' READ ERROR ON REIN' TO WS-ABEND-MESSAGE
                  MOVE ERRTBL-FILE-STATUS    TO WS-ABEND-FILE-STATUS
                  GO TO ABEND-PGM
               END-IF
           END-IF.

       0350-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

