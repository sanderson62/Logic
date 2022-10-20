       PROGRAM-ID.    PEMEPX4.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EPECS            ASSIGN TO SYS010.
           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ERACCTT      ASSIGN TO ERACCTT
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERACCT-FILE-STATUS
                               RECORD KEY IS AM-CONTROL-PRIMARY.

           SELECT ERRTBLT      ASSIGN TO ERRTBLT
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERRTBL-FILE-STATUS
                               RECORD KEY IS RE-CONTROL-PRIMARY.

           SELECT EXTRACT          ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.
                                                                        
              

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

       FD  ERRTBLT.   
                                                                        
                                   COPY ERCREIN.                        
       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS V.

       01  EXTRACT-HEAD-OUT            PIC X(911).
       01  EXTRACT-RECORD-OUT          PIC X(1030).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     PEMEPX4 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-EPEC                VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  EPEC-IN-CNT                 PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.
       77  WS-LAST-MONTH-DT            PIC 9(11)  VALUE ZEROS.
       77  S1                          PIC S9(5)  VALUE +0 COMP-3.
       77  WS-S1                       PIC S999   VALUE +0 COMP-3.
       77  WS-S2                       PIC S999   VALUE +0 COMP-3.
       77  WS-S3                       PIC S999   VALUE +0 COMP-3.
       77  WS-WORK-TAX                 PIC S9(7)V9(5) VALUE +0 COMP-3.
       77  ERACCT-FILE-STATUS          PIC XX VALUE LOW-VALUES.
       77  ERRTBL-FILE-STATUS          PIC XX VALUE LOW-VALUES.
       77  ERACCT-EOF-SW               PIC X  VALUE SPACES.
           88  END-OF-ERACCT                  VALUE 'Y'.
       77  ERRTBL-EOF-SW               PIC X  VALUE SPACES.
           88  END-OF-ERRTBL                  VALUE 'Y'.
       01  WS-LAST-YEAR-END-DT         PIC 9(11)  VALUE ZEROS.
       01  WS-CURRENT-KEY.
           05  WS-CONTROL              PIC X(31)  VALUE LOW-VALUES.
           05  WS-REI-CO               PIC X(6)   VALUE LOW-VALUES.

       01  REIN-COMP-TABLE.
           05  REIN-COMPANIES OCCURS 1500.
               10  RCT-REIN-COMP       PIC X(6).
               10  RCT-REIN-NAME       PIC X(30).
               10  RCT-LF-PE           PIC X.
               10  RCT-LF-FEE          PIC S9V9999 COMP-3.
               10  RCT-LF-PR-PCT       PIC S9V9999 COMP-3.
               10  RCT-LF-78-PCT       PIC S9V9999 COMP-3.
               10  RCT-AH-PE           PIC X.
               10  RCT-AH-FEE          PIC S9V9999 COMP-3.
               10  RCT-AH-PR-PCT       PIC S9V9999 COMP-3.
               10  RCT-AH-78-PCT       PIC S9V9999 COMP-3.
               10  RCT-CLM-CODE        PIC X.
               10  RCT-PRT-ST          PIC X.
               10  RCT-PRT-OW          PIC X.
               10  RCT-MORT-SW         PIC X.
               10  RCT-ZERO-LF-FEE     PIC X.
               10  RCT-ZERO-AH-FEE     PIC X.
               10  RCT-LF-COMM         PIC X.
               10  RCT-AH-COMM         PIC X.
               10  RCT-LF-TAX          PIC X.
               10  RCT-AH-TAX          PIC X.
               10  RCT-LF-FEE-METHOD   PIC X.
               10  RCT-AH-FEE-METHOD   PIC X.
               10  RCT-LF-FEE-BASIS    PIC X.
               10  RCT-AH-FEE-BASIS    PIC X.

       01  WS-PREVIOUS-KEY.
           05  WS-PREV-CARRIER         PIC X.
           05  WS-PREV-GROUP           PIC X(6).
           05  WS-PREV-STATE           PIC XX.
           05  WS-PREV-ACCOUNT         PIC X(10).
           05  WS-PREV-EXP-DTE         PIC 9(11)  COMP-3.
           05  WS-PREV-EFF-DTE         PIC 9(11)  COMP-3.
           05  WS-PREV-REI-CO          PIC X(6).


       01  WS-PREV-ACCT-STUFF.
           05  PRV-REPORT-CDE1         PIC X(10).
           05  PRV-REPORT-CDE2         PIC X(10).
           05  PRV-GROUP-A             PIC X(6).
           05  PRV-RETRO-POOL          PIC X(6).
           05  PRV-REIN-TABLE          PIC XXX.


       01  WS-ACCUM-TOTALS.
           02  WS-TYP-ACCUMS OCCURS 2.
             05  FILLER OCCURS 10.
               10  WS-ISS-BEN          PIC S9(11)V99  COMP-3.
               10  WS-CNC-BEN          PIC S9(11)V99  COMP-3.
               10  WS-ISS-PRM          PIC S9(9)V99   COMP-3.
               10  WS-CNC-PRM          PIC S9(9)V99   COMP-3.
               10  WS-PRM-78           PIC S9(9)V99   COMP-3.
               10  WS-PRM-PR           PIC S9(9)V99   COMP-3.
               10  WS-PRM-ST           PIC S9(9)V99   COMP-3.
               10  WS-CLM-AMT          PIC S9(9)V99   COMP-3.
               10  WS-CLM-DU           PIC S9(7)V99   COMP-3.
               10  WS-CLM-PV           PIC S9(7)V99   COMP-3.
               10  WS-CLM-IBNR         PIC S9(7)V99   COMP-3.
               10  WS-LOSS-RESV        PIC S9(7)V99   COMP-3.
               10  WS-RETRO-PAYMENTS   PIC S9(7)V99   COMP-3.
               10  WS-MORT-RESV        PIC S9(11)V9(6) COMP-3.
               10  WS-IN-FORCE         PIC S9(11)V99   COMP-3.
               10  WS-PRM-TAX          PIC S9(5)V99   COMP-3.
               10  WS-PRM-TAX-R78      PIC S9(5)V99   COMP-3.
               10  WS-PRM-TAX-PRO      PIC S9(5)V99   COMP-3.
               10  WS-PRM-TAX-ST       PIC S9(5)V99   COMP-3.
               10  WS-DIRECT-COMM      PIC S9(7)V99   COMP-3.
               10  WS-DIRECT-COMM-R78  PIC S9(7)V99   COMP-3.
               10  WS-DIRECT-COMM-PRO  PIC S9(7)V99   COMP-3.
               10  WS-DIRECT-COMM-ST   PIC S9(7)V99   COMP-3.
               10  WS-OW-COMM          PIC S9(7)V99   COMP-3.
               10  WS-OW-COMM-R78      PIC S9(7)V99   COMP-3.
               10  WS-OW-COMM-PRO      PIC S9(7)V99   COMP-3.
               10  WS-OW-COMM-ST       PIC S9(7)V99   COMP-3.

       01  HEAD-RECORD.
           05  HD-CARRIER             PIC X(4)    VALUE 'CARR'.
           05  HD-TAB1                PIC X.
           05  HD-GROUP               PIC X(5)    VALUE 'GROUP'.
           05  HD-TAB2                PIC X.
           05  HD-STATE               PIC X(5)    VALUE 'STATE'.
           05  HD-TAB3                PIC X.
           05  HD-ACCOUNT             PIC X(7)    VALUE 'ACCOUNT'.
           05  HD-TAB4                PIC X.
           05  HD-EXP-DT              PIC X(6)    VALUE 'EXP DT'.
           05  HD-TAB5                PIC X.
           05  HD-EFF-DT              PIC X(6)    VALUE 'EFF DT'.
           05  HD-TAB6                PIC X.
           05  HD-REIN-COMP           PIC X(9)    VALUE 'REIN COMP'.
           05  HD-TAB7                PIC X.
           05  HD-PERIOD              PIC X(6)    VALUE 'PERIOD'.
           05  HD-TAB8                PIC X.
           05  HD-RCD-TYPE            PIC X(4)    VALUE 'TYPE'.
           05  HD-TAB9                PIC X.
           05  HD-ISS-BEN             PIC X(9)    VALUE 'ISSUE BEN'.
           05  HD-TAB10               PIC X.
           05  HD-CNC-BEN             PIC X(10)   VALUE 'CANCEL BEN'.
           05  HD-TAB11               PIC X.
           05  HD-ISS-PRM             PIC X(10)   VALUE 'ISSUE PREM'.
           05  HD-TAB12               PIC X.
           05  HD-CNC-PRM             PIC X(11)   VALUE 'CANCEL PREM'.
           05  HD-TAB13               PIC X.
           05  HD-EPR-78-B            PIC X(10)   VALUE 'BEG EP R78'.
           05  HD-TAB14               PIC X.
           05  HD-EPR-78              PIC X(13)   VALUE 'EARN PREM R78'.
           05  HD-TAB15               PIC X.
           05  HD-EPR-78-E            PIC X(10)   VALUE 'END EP R78'.
           05  HD-TAB16               PIC X.
           05  HD-EPR-PR-B            PIC X(10)   VALUE 'BEG EP PRO'.
           05  HD-TAB17               PIC X.
           05  HD-EPR-PR              PIC X(13)   VALUE 'EARN PREM PRO'.
           05  HD-TAB18               PIC X.
           05  HD-EPR-PR-E            PIC X(10)   VALUE 'END EP PRO'.
           05  HD-TAB19               PIC X.
           05  HD-EPR-ST-B            PIC X(11)   VALUE
               'BEG EP STAT'.   
           05  HD-TAB20               PIC X.
           05  HD-EPR-ST              PIC X(14)   VALUE
               'EARN PREM STAT'.
           05  HD-TAB21               PIC X.
           05  HD-EPR-ST-E            PIC X(11)   VALUE
               'END EP STAT'.   
           05  HD-TAB22               PIC X.
           05  HD-DU-B                PIC X(12)   VALUE
               'BEG PTC RESV'.   
           05  HD-TAB23               PIC X.
           05  HD-DU                  PIC X(15)   VALUE
               'PAY TO CUR RESV'.
           05  HD-TAB24               PIC X.
           05  HD-DU-E                PIC X(12)   VALUE
               'END PTC RESV'.   
           05  HD-TAB25               PIC X.
           05  HD-PV                  PIC X(5)    VALUE 'ZEROS'.
           05  HD-TAB26               PIC X.
           05  HD-IBNR-B              PIC X(13)   VALUE
               'BEG IBNR RESV'.
           05  HD-TAB27               PIC X.
           05  HD-IBNR                PIC X(9)    VALUE 'IBNR RESV'.
           05  HD-TAB28               PIC X.
           05  HD-IBNR-E              PIC X(13)   VALUE
               'END IBNR RESV'.
           05  HD-TAB29               PIC X.
           05  HD-LR-B                PIC X(15)   VALUE
               'BEG FUTURE RESV'.
           05  HD-TAB30               PIC X.
           05  HD-LR                  PIC X(11)   VALUE 'FUTURE RESV'.
           05  HD-TAB31               PIC X.
           05  HD-LR-E                PIC X(15)   VALUE
               'END FUTURE RESV'.
           05  HD-TAB32               PIC X.
           05  HD-CLM-AMT             PIC X(10)   VALUE 'CLAIM PMTS'.
           05  HD-TAB33               PIC X.
           05  HD-MORT-RESV-B         PIC X(13)   VALUE
               'BEG MORT RESV'. 
           05  HD-TAB34               PIC X.
           05  HD-MORT-RESV           PIC X(14)   VALUE
               'MORTALITY RESV'.
           05  HD-TAB35               PIC X.
           05  HD-MORT-RESV-E         PIC X(13)   VALUE
               'END MORT RESV'. 
           05  HD-TAB36               PIC X.
           05  HD-IN-FORCE            PIC X(12)   VALUE 'INS IN FORCE'.
           05  HD-TAB37               PIC X.
           05  HD-RETRO-PAYMENTS      PIC X(10)   VALUE 'RETRO PMTS'.
           05  HD-TAB38               PIC X.
           05  HD-PRM-TAX             PIC X(8)    VALUE 'PREM TAX'.
           05  HD-TAB39               PIC X.
           05  HD-TAX-R78             PIC X(12)   VALUE 'PREM TAX R78'.
           05  HD-TAB40               PIC X.
           05  HD-TAX-PRO             PIC X(12)   VALUE 'PREM TAX PRO'.
           05  HD-TAB41               PIC X.
           05  HD-TAX-ST              PIC X(12)   VALUE 'PREM TAX ST '.
           05  HD-TAB42               PIC X.
           05  HD-DIRECT-COMM         PIC X(11)   VALUE 'DIRECT COMM'.
           05  HD-TAB43               PIC X.
           05  HD-OW-COMM             PIC X(7)    VALUE 'OW COMM'.
           05  HD-TAB44               PIC X.
           05  HD-COMM-78             PIC X(13)   VALUE 'EARN COMM R78'.
           05  HD-TAB45               PIC X.
           05  HD-COMM-PR             PIC X(13)   VALUE 'EARN COMM PRO'.
           05  HD-TAB46               PIC X.
           05  HD-COMM-ST             PIC X(14)   VALUE
               'EARN COMM STAT'.
           05  HD-TAB47               PIC X.
           05  HD-OW-COMM-R78         PIC X(16)   VALUE
               'EARN OW COMM R78'.
           05  HD-TAB48               PIC X.
           05  HD-OW-COMM-PRO         PIC X(16)   VALUE
               'EARN OW COMM PRO'.
           05  HD-TAB49               PIC X.
           05  HD-OW-COMM-ST          PIC X(15)   VALUE
               'EARN OW COMM ST'.
           05  HD-TAB50               PIC X.
           05  HD-RPT-CDE1            PIC X(11)   VALUE 'REPORT CDE1'.
           05  HD-TAB51               PIC X.
           05  HD-RPT-CDE2            PIC X(11)   VALUE 'REPORT CDE2'.
           05  HD-TAB52               PIC X.
           05  HD-GROUPA              PIC X(7)    VALUE 'GROUP A'.
           05  HD-TAB53               PIC X.
           05  HD-RETRO-POOL          PIC X(10)   VALUE 'RETRO POOL'.
           05  HD-TAB54               PIC X.
           05  HD-REIN-TABLE          PIC X(10)   VALUE 'REIN TABLE'.
           05  HD-TAB55               PIC X.
           05  HD-REIN-COMP-NAME      PIC X(14)   VALUE
               'REIN COMP NAME'.
           05  HD-TAB56               PIC X.
           05  HD-LF-PE               PIC X(6)    VALUE 'LF P-E'.
           05  HD-TAB57               PIC X.
           05  HD-LF-FEE              PIC X(6)    VALUE 'LF FEE'.
           05  HD-TAB58               PIC X.
           05  HD-LF-PR-PCT           PIC X(9)    VALUE 'LF PR PCT'.
           05  HD-TAB59               PIC X.
           05  HD-LF-78-PCT           PIC X(9)    VALUE 'LF 78 PCT'.
           05  HD-TAB60               PIC X.
           05  HD-AH-PE               PIC X(6)    VALUE 'AH P-E'.
           05  HD-TAB61               PIC X.
           05  HD-AH-FEE              PIC X(6)    VALUE 'AH FEE'.
           05  HD-TAB62               PIC X.
           05  HD-AH-PR-PCT           PIC X(9)    VALUE 'AH PR PCT'.
           05  HD-TAB63               PIC X.
           05  HD-AH-78-PCT           PIC X(9)    VALUE 'AH 78 PCT'.
           05  HD-TAB64               PIC X.
           05  HD-CLM-CODE            PIC X(9)    VALUE 'CLAIM P-I'.
           05  HD-TAB65               PIC X.
PEM        05  HD-PRT-ST              PIC X(6)    VALUE 'PRT ST'.
           05  HD-TAB66               PIC X.
           05  HD-PRT-OW              PIC X(6)    VALUE 'PRT OW'.
           05  HD-TAB67               PIC X.
           05  HD-MORT-SW             PIC X(7)    VALUE 'MORT SW'.
           05  HD-TAB68               PIC X.
           05  HD-ZERO-LF-FEE         PIC X(11)   VALUE 'ZERO LF FEE'.
           05  HD-TAB69               PIC X.
           05  HD-ZERO-AH-FEE         PIC X(11)   VALUE 'ZERO AH FEE'.
           05  HD-TAB70               PIC X.
           05  HD-LF-COMM             PIC X(7)    VALUE 'LF COMM'.
           05  HD-TAB71               PIC X.
           05  HD-AH-COMM             PIC X(7)    VALUE 'AH COMM'.
           05  HD-TAB72               PIC X.
           05  HD-LF-TAX              PIC X(6)    VALUE 'LF TAX'.
           05  HD-TAB73               PIC X.
           05  HD-AH-TAX              PIC X(6)    VALUE 'AH TAX'.
           05  HD-TAB74               PIC X.
           05  HD-LF-FEE-METHOD       PIC X(13)   VALUE 'LF FEE METHOD'.
           05  HD-TAB75               PIC X.
           05  HD-AH-FEE-METHOD       PIC X(13)   VALUE 'AH FEE METHOD'.
           05  HD-TAB76               PIC X.
           05  HD-LF-FEE-BASIS        PIC X(12)   VALUE 'LF FEE BASIS'.
           05  HD-TAB77               PIC X.
           05  HD-AH-FEE-BASIS        PIC X(12)   VALUE 'AH FEE BASIS'.
           05  HD-TAB78               PIC X.
           05  HD-END                 PIC X.

       01  WS-INIT-EXTRACT             PIC X(1030).
       01  EXTRACT-RECORD.
           05  EXT-CARRIER             PIC X.
           05  EXT-TAB1                PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-TAB2                PIC X.
           05  EXT-STATE               PIC XX.
           05  EXT-TAB3                PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-TAB4                PIC X.
           05  EXT-EXP-DT              PIC X(10).
           05  EXT-TAB5                PIC X.
           05  EXT-EFF-DT              PIC X(10).
           05  EXT-TAB6                PIC X.
           05  EXT-REIN-COMP           PIC X(6).
           05  EXT-TAB7                PIC X.
           05  EXT-PERIOD              PIC XXX.
           05  EXT-TAB8                PIC X.
           05  EXT-RCD-TYPE            PIC X.
           05  EXT-TAB9                PIC X.
           05  EXT-ISS-BEN             PIC ---------9.99.
           05  EXT-TAB10               PIC X.
           05  EXT-CNC-BEN             PIC ---------9.99.
           05  EXT-TAB11               PIC X.
           05  EXT-ISS-PRM             PIC ---------9.99.
           05  EXT-TAB12               PIC X.
           05  EXT-CNC-PRM             PIC ---------9.99.
           05  EXT-TAB13               PIC X.
           05  EXT-EPR-78-B            PIC ---------9.99.
           05  EXT-TAB14               PIC X.
           05  EXT-EPR-78              PIC ---------9.99.
           05  EXT-TAB15               PIC X.
           05  EXT-EPR-78-E            PIC ---------9.99.
           05  EXT-TAB16               PIC X.
           05  EXT-EPR-PR-B            PIC ---------9.99.
           05  EXT-TAB17               PIC X.
           05  EXT-EPR-PR              PIC ---------9.99.
           05  EXT-TAB18               PIC X.
           05  EXT-EPR-PR-E            PIC ---------9.99.
           05  EXT-TAB19               PIC X.
           05  EXT-EPR-ST-B            PIC ---------9.99.
           05  EXT-TAB20               PIC X.
           05  EXT-EPR-ST              PIC ---------9.99.
           05  EXT-TAB21               PIC X.
           05  EXT-EPR-ST-E            PIC ---------9.99.
           05  EXT-TAB22               PIC X.
           05  EXT-DU-B                PIC -------9.99.
           05  EXT-TAB23               PIC X.
           05  EXT-DU                  PIC -------9.99.
           05  EXT-TAB24               PIC X.
           05  EXT-DU-E                PIC -------9.99.
           05  EXT-TAB25               PIC X.
           05  EXT-PV                  PIC -------9.99.
           05  EXT-TAB26               PIC X.
           05  EXT-IBNR-B              PIC -------9.99.
           05  EXT-TAB27               PIC X.
           05  EXT-IBNR                PIC -------9.99.
           05  EXT-TAB28               PIC X.
           05  EXT-IBNR-E              PIC -------9.99.
           05  EXT-TAB29               PIC X.
           05  EXT-LR-B                PIC -------9.99.
           05  EXT-TAB30               PIC X.
           05  EXT-LR                  PIC -------9.99.
           05  EXT-TAB31               PIC X.
           05  EXT-LR-E                PIC -------9.99.
           05  EXT-TAB32               PIC X.
           05  EXT-CLM-AMT             PIC ---------9.99.
           05  EXT-TAB33               PIC X.
           05  EXT-MORT-RESV-B         PIC ---------9.99.
           05  EXT-TAB34               PIC X.
           05  EXT-MORT-RESV           PIC ---------9.99.
           05  EXT-TAB35               PIC X.
           05  EXT-MORT-RESV-E         PIC ---------9.99.
           05  EXT-TAB36               PIC X.
           05  EXT-IN-FORCE            PIC ---------9.99.
           05  EXT-TAB37               PIC X.
           05  EXT-RETRO-PAYMENTS      PIC -------9.99.
           05  EXT-TAB38               PIC X.
           05  EXT-PRM-TAX             PIC -------9.99.
           05  EXT-TAB39               PIC X.
           05  EXT-TAX-R78             PIC -------9.99.
           05  EXT-TAB40               PIC X.
           05  EXT-TAX-PRO             PIC -------9.99.
           05  EXT-TAB41               PIC X.
           05  EXT-TAX-ST              PIC -------9.99.
           05  EXT-TAB42               PIC X.
           05  EXT-DIRECT-COMM         PIC -------9.99.
           05  EXT-TAB43               PIC X.
           05  EXT-OW-COMM             PIC -------9.99.
           05  EXT-TAB44               PIC X.
           05  EXT-COMM-R78            PIC ------9.99.
           05  EXT-TAB45               PIC X.
           05  EXT-COMM-PRO            PIC ------9.99.
           05  EXT-TAB46               PIC X.
           05  EXT-COMM-ST             PIC ------9.99.
           05  EXT-TAB47               PIC X.
           05  EXT-OW-COMM-R78         PIC ------9.99.
           05  EXT-TAB48               PIC X.
           05  EXT-OW-COMM-PRO         PIC ------9.99.
           05  EXT-TAB49               PIC X.
           05  EXT-OW-COMM-ST          PIC ------9.99.
           05  EXT-TAB50               PIC X.
           05  EXT-REPORT-CDE1         PIC X(10).
           05  EXT-TAB51               PIC X.
           05  EXT-REPORT-CDE2         PIC X(10).
           05  EXT-TAB52               PIC X.
           05  EXT-GROUP-A             PIC X(6).
           05  EXT-TAB53               PIC X.
           05  EXT-RETRO-POOL          PIC X(6).
           05  EXT-TAB54               PIC X.
           05  EXT-REIN-TABLE          PIC XXX.
           05  EXT-TAB55               PIC X.
           05  EXT-REIN-NAME           PIC X(30).
           05  EXT-TAB56               PIC X.
           05  EXT-LF-PE               PIC X.
           05  EXT-TAB57               PIC X.
           05  EXT-LF-FEE              PIC --.9999.
           05  EXT-TAB58               PIC X.
           05  EXT-LF-PR-PCT           PIC --.9999.
           05  EXT-TAB59               PIC X.
           05  EXT-LF-78-PCT           PIC --.9999.
           05  EXT-TAB60               PIC X.
           05  EXT-AH-PE               PIC X.
           05  EXT-TAB61               PIC X.
           05  EXT-AH-FEE              PIC --.9999.
           05  EXT-TAB62               PIC X.
           05  EXT-AH-PR-PCT           PIC --.9999.
           05  EXT-TAB63               PIC X.
           05  EXT-AH-78-PCT           PIC --.9999.
           05  EXT-TAB64               PIC X.
           05  EXT-CLM-CODE            PIC X.
           05  EXT-TAB65               PIC X.
PEM        05  EXT-PRT-ST              PIC X.
           05  EXT-TAB66               PIC X.
           05  EXT-PRT-OW              PIC X.
           05  EXT-TAB67               PIC X.
           05  EXT-MORT-SW             PIC X.
           05  EXT-TAB68               PIC X.
           05  EXT-ZERO-LF-FEE         PIC X.
           05  EXT-TAB69               PIC X.
           05  EXT-ZERO-AH-FEE         PIC X.
           05  EXT-TAB70               PIC X.
           05  EXT-LF-COMM             PIC X.
           05  EXT-TAB71               PIC X.
           05  EXT-AH-COMM             PIC X.
           05  EXT-TAB72               PIC X.
           05  EXT-LF-TAX              PIC X.
           05  EXT-TAB73               PIC X.
           05  EXT-AH-TAX              PIC X.
           05  EXT-TAB74               PIC X.
           05  EXT-LF-FEE-METHOD       PIC X.
           05  EXT-TAB75               PIC X.
           05  EXT-AH-FEE-METHOD       PIC X.
           05  EXT-TAB76               PIC X.
           05  EXT-LF-FEE-BASIS        PIC X.
           05  EXT-TAB77               PIC X.
           05  EXT-AH-FEE-BASIS        PIC X.
           05  EXT-TAB78               PIC X.
           05  EXT-END                 PIC X.

           EJECT

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
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

           EJECT
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
      /
       PROCEDURE DIVISION.

                                       COPY ELCDTERX.
       0002-INPUT.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0075-READ-ERACCT    THRU 0075-EXIT
           MOVE AM-REPORT-CODE-1       TO PRV-REPORT-CDE1
           MOVE AM-REPORT-CODE-2       TO PRV-REPORT-CDE2
           MOVE AM-REI-GROUP-A         TO PRV-GROUP-A
           MOVE AM-RETRO-POOL          TO PRV-RETRO-POOL
           MOVE AM-REI-TABLE           TO PRV-REIN-TABLE

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT
           MOVE WS-CURRENT-KEY         TO WS-PREVIOUS-KEY

           PERFORM 0080-PROCESS-EPEC   THRU 0080-EXIT UNTIL
                 (END-OF-EPEC)
      *          OR (EPEC-IN-CNT > 10000)

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
                                          EXT-TAB68
                                          EXT-TAB69
                                          EXT-TAB70
                                          EXT-TAB71
                                          EXT-TAB72
                                          EXT-TAB73
                                          EXT-TAB74
                                          EXT-TAB75
                                          EXT-TAB76
                                          EXT-TAB77
                                          EXT-TAB78

           MOVE '*'                    TO EXT-END
           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT

           MOVE ';'                    TO HD-TAB1
                                          HD-TAB2
                                          HD-TAB3
                                          HD-TAB4
                                          HD-TAB5
                                          HD-TAB6
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
                                          HD-TAB61
                                          HD-TAB62
                                          HD-TAB63
                                          HD-TAB64
                                          HD-TAB65
                                          HD-TAB66
                                          HD-TAB67
                                          HD-TAB68
                                          HD-TAB69
                                          HD-TAB70
                                          HD-TAB71
                                          HD-TAB72
                                          HD-TAB73
                                          HD-TAB74
                                          HD-TAB75
                                          HD-TAB76
                                          HD-TAB77
                                          HD-TAB78
                                          
           WRITE EXTRACT-HEAD-OUT      FROM HEAD-RECORD
           
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

           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE 12                     TO WS-WORK-MM
           MOVE 31                     TO WS-WORK-DD
           SUBTRACT 1                  FROM WS-WORK-CCYY-N
           MOVE WS-WORK-DATE           TO WS-LAST-YEAR-END-DT

           DISPLAY ' LAST MONTH ' WS-LAST-MONTH-DT
           DISPLAY ' LAST YEAR  ' WS-LAST-YEAR-END-DT

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +1500
              MOVE SPACES              TO RCT-REIN-COMP    (S1)
                                          RCT-REIN-NAME    (S1)
                                          RCT-LF-PE        (S1)
                                          RCT-AH-PE        (S1)
                                          RCT-CLM-CODE     (S1)
                                          RCT-PRT-ST       (S1)
                                          RCT-PRT-OW       (S1)
                                          RCT-MORT-SW      (S1)
                                          RCT-ZERO-LF-FEE  (S1)
                                          RCT-ZERO-AH-FEE  (S1)
                                          RCT-LF-COMM      (S1)
                                          RCT-AH-COMM      (S1)
                                          RCT-LF-TAX       (S1)
                                          RCT-AH-TAX       (S1)
                                          RCT-LF-FEE-METHOD (S1)
                                          RCT-AH-FEE-METHOD (S1)
                                          RCT-LF-FEE-BASIS  (S1)
                                          RCT-AH-FEE-BASIS  (S1)
              MOVE +0                  TO RCT-LF-FEE       (S1)
                                          RCT-LF-PR-PCT    (S1)
                                          RCT-LF-78-PCT    (S1)
                                          RCT-AH-FEE       (S1)
                                          RCT-AH-PR-PCT    (S1)
                                          RCT-AH-78-PCT    (S1)
           END-PERFORM

           PERFORM 0016-BUILD-REIN-TABLE
                                       THRU 0016-EXIT.
           MOVE +1                     TO S1

           .
       0010-EXIT.
           EXIT.

       0015-INIT-TABLE.
        
           PERFORM VARYING WS-S2 FROM +1 BY +1 UNTIL
              WS-S2 > +10
              MOVE +0                  TO WS-ISS-BEN (1 WS-S2)
                                          WS-CNC-BEN (1 WS-S2)
                                          WS-ISS-PRM (1 WS-S2)
                                          WS-CNC-PRM (1 WS-S2)
                                          WS-PRM-78 (1 WS-S2)
                                          WS-PRM-PR (1 WS-S2)
                                          WS-PRM-ST (1 WS-S2)
                                          WS-CLM-AMT (1 WS-S2)
                                          WS-CLM-DU (1 WS-S2)
                                          WS-CLM-PV (1 WS-S2)
                                          WS-CLM-IBNR (1 WS-S2)
                                          WS-LOSS-RESV (1 WS-S2)
                                          WS-RETRO-PAYMENTS (1 WS-S2)
                                          WS-MORT-RESV (1 WS-S2)
                                          WS-IN-FORCE (1 WS-S2)
                                          WS-PRM-TAX (1 WS-S2)
                                          WS-PRM-TAX-R78 (1 WS-S2)
                                          WS-PRM-TAX-PRO (1 WS-S2)
                                          WS-PRM-TAX-ST  (1 WS-S2)
                                          WS-DIRECT-COMM (1 WS-S2)
                                          WS-OW-COMM     (1 WS-S2)
                                          WS-DIRECT-COMM-R78 (1 WS-S2)
                                          WS-DIRECT-COMM-PRO (1 WS-S2)
                                          WS-DIRECT-COMM-ST (1 WS-S2)
                                          WS-OW-COMM-R78 (1 WS-S2)
                                          WS-OW-COMM-PRO (1 WS-S2)
                                          WS-OW-COMM-ST  (1 WS-S2)
           END-PERFORM
           MOVE WS-TYP-ACCUMS (1)      TO WS-TYP-ACCUMS (2)

           .
       0015-EXIT.
           EXIT.
           
       0016-BUILD-REIN-TABLE.

           MOVE +0                     TO S1
           MOVE LOW-VALUES             TO RE-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO RE-COMPANY-CD
           MOVE 'B'                    TO RE-CODE
           
           START ERRTBLT KEY IS NOT < RE-CONTROL-PRIMARY
           IF ERRTBL-FILE-STATUS NOT = '00'
              DISPLAY ' BAD START ON ERRTBL ' ERRTBL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
           PERFORM 0017-READ-ERRTBL    THRU 0017-EXIT UNTIL
              END-OF-ERRTBL
                         
           .
       0016-EXIT.
           EXIT.

       0017-READ-ERRTBL.
      ***  DON'T WORRY ABOUT CHECKING COMPANY CODE HERE
      **** BECAUSE WE ARE USING ERRTBL OUT OF M05.

           READ ERRTBLT NEXT RECORD
           
           IF (ERRTBL-FILE-STATUS = '10' OR '23')
                       OR
              (RE-CODE NOT = 'B')
              SET END-OF-ERRTBL        TO TRUE
           ELSE
              IF ERRTBL-FILE-STATUS NOT = '00'
                 DISPLAY ' BAD READ NEXT ON ERRTBL '
                                       ERRTBL-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 ADD +1                TO S1
                 MOVE RE-COMPANY       TO RCT-REIN-COMP    (S1)
                 MOVE RE-NAME          TO RCT-REIN-NAME    (S1)
                 MOVE RE-LF-PE         TO RCT-LF-PE        (S1)
                 MOVE RE-LF-FEE        TO RCT-LF-FEE       (S1)
                 MOVE RE-LF-PR-PCT     TO RCT-LF-PR-PCT    (S1)
                 MOVE RE-LF-78-PCT     TO RCT-LF-78-PCT    (S1)
                 MOVE RE-AH-PE         TO RCT-AH-PE        (S1)
                 MOVE RE-AH-FEE        TO RCT-AH-FEE       (S1)
                 MOVE RE-AH-PR-PCT     TO RCT-AH-PR-PCT    (S1)
                 MOVE RE-AH-78-PCT     TO RCT-AH-78-PCT    (S1)
                 MOVE RE-CLAIM-CODE    TO RCT-CLM-CODE     (S1)
                 MOVE RE-PRT-ST        TO RCT-PRT-ST       (S1)
                 MOVE RE-PRT-OW        TO RCT-PRT-OW       (S1)
                 MOVE RE-MORT-SW       TO RCT-MORT-SW      (S1)
                 MOVE RE-ZERO-LF-FEE   TO RCT-ZERO-LF-FEE  (S1)
                 MOVE RE-ZERO-AH-FEE   TO RCT-ZERO-AH-FEE  (S1)
                 MOVE RE-LF-COMM       TO RCT-LF-COMM      (S1)
                 MOVE RE-AH-COMM       TO RCT-AH-COMM      (S1)
                 MOVE RE-LF-TAX        TO RCT-LF-TAX       (S1)
                 MOVE RE-AH-TAX        TO RCT-AH-TAX       (S1)
                 MOVE RE-LF-FEE-METHOD TO RCT-LF-FEE-METHOD (S1)
                 MOVE RE-AH-FEE-METHOD TO RCT-AH-FEE-METHOD (S1)
                 MOVE RE-LF-FEE-BASIS  TO RCT-LF-FEE-BASIS  (S1)
                 MOVE RE-AH-FEE-BASIS  TO RCT-AH-FEE-BASIS  (S1)
              END-IF
           END-IF
              
           .
       0017-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EPECS ERACCTT ERRTBLT
               OUTPUT EXTRACT

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
           DISPLAY ' EXTR OUT RECORDS ' EXTR-OUT-CNT
           CLOSE EPECS
               EXTRACT ERACCTT ERRTBLT

           .

       0030-EXIT.
           EXIT.

       0060-READ-EPEC.

           READ EPECS AT END
               SET END-OF-EPEC         TO TRUE
           END-READ

           IF NOT END-OF-EPEC
      *       IF (EP-ACCOUNT = '0000593000')
      *          AND (EP-RECORD-ID = 'EP')
      *          CONTINUE
      *       ELSE
      *          GO TO 0060-READ-EPEC
      *       END-IF
              ADD 1                    TO EPEC-IN-CNT
              MOVE EP-CONTROL          TO WS-CONTROL
              MOVE EP-REI-CO           TO WS-REI-CO
              PERFORM 0070-MATCH-TO-ERACCT
                                       THRU 0070-EXIT
           END-IF

           .

       0060-EXIT.
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
           
       0080-PROCESS-EPEC.

           IF WS-CURRENT-KEY NOT = WS-PREVIOUS-KEY
              PERFORM 0090-BUILD-EXTRACT
                                       THRU 0090-EXIT
              PERFORM 0015-INIT-TABLE  THRU 0015-EXIT
              MOVE WS-CURRENT-KEY      TO WS-PREVIOUS-KEY
           END-IF

           MOVE AM-REPORT-CODE-1       TO PRV-REPORT-CDE1
           MOVE AM-REPORT-CODE-2       TO PRV-REPORT-CDE2
           MOVE AM-REI-GROUP-A         TO PRV-GROUP-A
           MOVE AM-RETRO-POOL          TO PRV-RETRO-POOL
           MOVE AM-REI-TABLE           TO PRV-REIN-TABLE

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
           END-IF
           
           IF EP-RECORD-ID = 'EC'
              IF (EC-RUN-DTE = RUN-DATE)
                          OR
                 ((EC-RUN-DTE < RUN-DATE)
                 AND (EC-PURGE = 'P'))
                 MOVE +1               TO WS-S2
                 PERFORM 0087-ACCUM-EC-TOTS
                                       THRU 0087-EXIT
              END-IF
              IF (EC-RUN-DTE = WS-LAST-MONTH-DT)
                            OR
                 ((EC-RUN-DTE < WS-LAST-MONTH-DT)
                 AND (EC-PURGE = 'P'))
                 MOVE +2            TO WS-S2
                 PERFORM 0087-ACCUM-EC-TOTS
                                       THRU 0087-EXIT
              END-IF
              IF (EC-RUN-DTE = WS-LAST-YEAR-END-DT)
                            OR
                 ((EC-RUN-DTE < WS-LAST-YEAR-END-DT)
                 AND (EC-PURGE = 'P'))
                 MOVE +3            TO WS-S2
                 PERFORM 0087-ACCUM-EC-TOTS
                                       THRU 0087-EXIT
              END-IF
           END-IF

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0082-ACCUM-EP-TOTS.
       
           IF EP-RCD-TYPE = 'L'
              MOVE +1                  TO WS-S1
           ELSE
              MOVE +2                  TO WS-S1
           END-IF
           
           COMPUTE WS-ISS-BEN (WS-S1 WS-S2) =
              WS-ISS-BEN (WS-S1 WS-S2) + EP-ISS-BEN
           COMPUTE WS-CNC-BEN (WS-S1 WS-S2) =
              WS-CNC-BEN (WS-S1 WS-S2) + EP-CNC-BEN
           COMPUTE WS-ISS-PRM (WS-S1 WS-S2) =
              WS-ISS-PRM (WS-S1 WS-S2) + EP-ISS-PRM
           COMPUTE WS-CNC-PRM (WS-S1 WS-S2) =
              WS-CNC-PRM (WS-S1 WS-S2) + EP-CNC-PRM
           COMPUTE WS-PRM-78  (WS-S1 WS-S2) =
              WS-PRM-78  (WS-S1 WS-S2) + EP-PRM-78
           COMPUTE WS-PRM-PR  (WS-S1 WS-S2) =
              WS-PRM-PR  (WS-S1 WS-S2) + EP-PRM-PR
           COMPUTE WS-PRM-ST  (WS-S1 WS-S2) =
              WS-PRM-ST  (WS-S1 WS-S2) + EP-PRM-ST
           COMPUTE WS-CLM-AMT (WS-S1 WS-S2) =
              WS-CLM-AMT (WS-S1 WS-S2) + EP-CLM-AMT
           COMPUTE WS-CLM-DU (WS-S1 WS-S2) =
              WS-CLM-DU (WS-S1 WS-S2) + EP-CLM-DU
           COMPUTE WS-CLM-PV (WS-S1 WS-S2) =
              WS-CLM-PV (WS-S1 WS-S2) + EP-CLM-PV
           COMPUTE WS-CLM-IBNR (WS-S1 WS-S2) =
              WS-CLM-IBNR (WS-S1 WS-S2) + EP-CLM-IBNR
           COMPUTE WS-LOSS-RESV (WS-S1 WS-S2) =
              WS-LOSS-RESV (WS-S1 WS-S2) + EP-LOSS-RESV
           COMPUTE WS-RETRO-PAYMENTS (WS-S1 WS-S2) =
              WS-RETRO-PAYMENTS (WS-S1 WS-S2) + EP-RETRO-PAYMENTS
           COMPUTE WS-MORT-RESV (WS-S1 WS-S2) =
              WS-MORT-RESV (WS-S1 WS-S2) + EP-MORT-RESV
           COMPUTE WS-IN-FORCE (WS-S1 WS-S2) =
              WS-IN-FORCE (WS-S1 WS-S2) + EP-IN-FORCE
           COMPUTE WS-PRM-TAX (WS-S1 WS-S2) =
              WS-PRM-TAX (WS-S1 WS-S2) + EP-PRM-TAX
           COMPUTE WS-WORK-TAX ROUNDED = 
              EP-PRM-78 / (EP-ISS-PRM - EP-CNC-PRM)
              * EP-PRM-TAX
           COMPUTE WS-PRM-TAX-R78 (WS-S1 WS-S2) =
              WS-PRM-TAX-R78 (WS-S1 WS-S2) +
              WS-WORK-TAX
           COMPUTE WS-WORK-TAX ROUNDED = 
              EP-PRM-PR / (EP-ISS-PRM - EP-CNC-PRM)
              * EP-PRM-TAX
           COMPUTE WS-PRM-TAX-PRO (WS-S1 WS-S2) =
              WS-PRM-TAX-PRO (WS-S1 WS-S2) +
              WS-WORK-TAX
           COMPUTE WS-WORK-TAX ROUNDED = 
              EP-PRM-ST / (EP-ISS-PRM - EP-CNC-PRM)
              * EP-PRM-TAX
           COMPUTE WS-PRM-TAX-ST (WS-S1 WS-S2) =
              WS-PRM-TAX-ST (WS-S1 WS-S2) +
              WS-WORK-TAX
           
           .
       0082-EXIT.
           EXIT.
           
       0087-ACCUM-EC-TOTS.
       
           IF EC-RCD-TYPE = 'L'
              MOVE +1                  TO WS-S1
           ELSE
              MOVE +2                  TO WS-S1
           END-IF
           
           PERFORM VARYING WS-S3 FROM +1 BY +1 UNTIL
              WS-S3 > +5
              IF ((EC-REIN = 'R')
                 AND (EC-AGT-TYPE (WS-S3) = 'D' OR 'R'))
                              OR
                 ((EC-REIN NOT = 'R')
                 AND (EC-AGT-TYPE (WS-S3) = 'D' OR 'C'))
                 COMPUTE WS-DIRECT-COMM (WS-S1 WS-S2) =
                  WS-DIRECT-COMM (WS-S1 WS-S2) + (EC-ISS-COMM (WS-S3)
                   - EC-CNC-COMM (WS-S3))
                 COMPUTE WS-DIRECT-COMM-R78 (WS-S1 WS-S2) =
                  WS-DIRECT-COMM-R78 (WS-S1 WS-S2) + EC-COMM-78 (WS-S3)
                 COMPUTE WS-DIRECT-COMM-PRO (WS-S1 WS-S2) =
                  WS-DIRECT-COMM-PRO (WS-S1 WS-S2) + EC-COMM-PR (WS-S3)
                 COMPUTE WS-DIRECT-COMM-ST  (WS-S1 WS-S2) =
                  WS-DIRECT-COMM-ST  (WS-S1 WS-S2) + EC-COMM-ST (WS-S3)
              ELSE
                 IF ((EC-REIN = 'R')
                    AND (EC-AGT-TYPE (WS-S3) = 'T' OR 'P'))
                                 OR
                    ((EC-REIN NOT = 'R')
                    AND (EC-AGT-TYPE (WS-S3) = 'O' OR 'P'))
                    COMPUTE WS-OW-COMM (WS-S1 WS-S2) =
                     WS-OW-COMM (WS-S1 WS-S2) + (EC-ISS-COMM (WS-S3)
                      - EC-CNC-COMM (WS-S3))
                    COMPUTE WS-OW-COMM-R78 (WS-S1 WS-S2) =
                     WS-OW-COMM-R78 (WS-S1 WS-S2)
                        + EC-COMM-78 (WS-S3)
                    COMPUTE WS-OW-COMM-PRO (WS-S1 WS-S2) =
                     WS-OW-COMM-PRO (WS-S1 WS-S2)
                        + EC-COMM-PR (WS-S3)
                    COMPUTE WS-OW-COMM-ST (WS-S1 WS-S2) =
                     WS-OW-COMM-ST (WS-S1 WS-S2)
                        + EC-COMM-ST (WS-S3)
                 END-IF
              END-IF
           END-PERFORM

           .
       0087-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.


           PERFORM VARYING WS-S1 FROM +1 BY +1 UNTIL
              WS-S1 > +2
             MOVE WS-INIT-EXTRACT      TO EXTRACT-RECORD
             MOVE WS-PREV-CARRIER      TO EXT-CARRIER
             MOVE WS-PREV-GROUP        TO EXT-GROUP
             MOVE WS-PREV-STATE        TO EXT-STATE
             MOVE WS-PREV-ACCOUNT      TO EXT-ACCOUNT

             MOVE PRV-REPORT-CDE1      TO EXT-REPORT-CDE1
             MOVE PRV-REPORT-CDE2      TO EXT-REPORT-CDE2
             MOVE PRV-GROUP-A          TO EXT-GROUP-A
             MOVE PRV-RETRO-POOL       TO EXT-RETRO-POOL
             MOVE PRV-REIN-TABLE       TO EXT-REIN-TABLE
 
             MOVE WS-PREV-EXP-DTE      TO WS-WORK-DATE
             STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
                DELIMITED BY SIZE INTO EXT-EXP-DT
             END-STRING

             MOVE WS-PREV-EFF-DTE      TO WS-WORK-DATE
             STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
                DELIMITED BY SIZE INTO EXT-EFF-DT
             END-STRING

      *      MOVE EP-RUN-DTE           TO WS-WORK-DATE
      *      STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
      *         DELIMITED BY SIZE INTO EXT-RUN-DTE
      *      END-STRING

             MOVE WS-PREV-REI-CO       TO EXT-REIN-COMP
             IF EXT-REIN-COMP NOT = SPACES AND ZEROS
                IF EXT-REIN-COMP = RCT-REIN-COMP (S1)
                   CONTINUE
                ELSE
                   PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                      (RCT-REIN-COMP (S1) = EXT-REIN-COMP)
                      OR (RCT-REIN-COMP (S1) = SPACES)
                      OR (S1 > +1500)
                   END-PERFORM
                END-IF
                IF (S1 NOT > +1500)
                   AND (RCT-REIN-COMP (S1) NOT = SPACES)
                   MOVE RCT-REIN-NAME (S1)
                                       TO EXT-REIN-NAME
                   MOVE RCT-LF-PE (S1) TO EXT-LF-PE
                   MOVE RCT-LF-FEE (S1)
                                       TO EXT-LF-FEE
                   MOVE RCT-LF-PR-PCT (S1)
                                       TO EXT-LF-PR-PCT
                   MOVE RCT-LF-78-PCT (S1)
                                       TO EXT-LF-78-PCT
                   MOVE RCT-AH-PE (S1) TO EXT-AH-PE
                   MOVE RCT-AH-FEE (S1)
                                       TO EXT-AH-FEE
                   MOVE RCT-AH-PR-PCT (S1)
                                       TO EXT-AH-PR-PCT
                   MOVE RCT-AH-78-PCT (S1)
                                       TO EXT-AH-78-PCT
                   MOVE RCT-CLM-CODE (S1)
                                       TO EXT-CLM-CODE
                   MOVE RCT-PRT-ST (S1) TO EXT-PRT-ST
                   MOVE RCT-PRT-OW (S1) TO EXT-PRT-OW
                   MOVE RCT-MORT-SW (S1) TO EXT-MORT-SW
                   MOVE RCT-ZERO-LF-FEE (S1) TO EXT-ZERO-LF-FEE
                   MOVE RCT-ZERO-AH-FEE (S1) TO EXT-ZERO-AH-FEE
                   MOVE RCT-LF-COMM (S1) TO EXT-LF-COMM
                   MOVE RCT-AH-COMM (S1) TO EXT-AH-COMM
                   MOVE RCT-LF-TAX (S1) TO EXT-LF-TAX
                   MOVE RCT-AH-TAX (S1) TO EXT-AH-TAX
                   MOVE RCT-LF-FEE-METHOD (S1) TO EXT-LF-FEE-METHOD
                   MOVE RCT-AH-FEE-METHOD (S1) TO EXT-AH-FEE-METHOD
                   MOVE RCT-LF-FEE-BASIS (S1) TO EXT-LF-FEE-BASIS
                   MOVE RCT-AH-FEE-BASIS (S1) TO EXT-AH-FEE-BASIS
                ELSE
                   MOVE ZEROS    TO EXT-LF-FEE     EXT-AH-FEE
                                    EXT-LF-PR-PCT  EXT-AH-PR-PCT
                                    EXT-LF-78-PCT  EXT-AH-78-PCT
                END-IF
            ELSE
               MOVE ZEROS    TO EXT-LF-FEE     EXT-AH-FEE
                                EXT-LF-PR-PCT  EXT-AH-PR-PCT
                                EXT-LF-78-PCT  EXT-AH-78-PCT
             END-IF

             IF WS-S1 = +1
                MOVE 'L'               TO EXT-RCD-TYPE
             ELSE
                MOVE 'A'               TO EXT-RCD-TYPE
             END-IF
             PERFORM VARYING WS-S2 FROM +1 BY +1 UNTIL
                WS-S2 > +3
               EVALUATE WS-S2
                  WHEN +1
                     MOVE 'ITD'        TO EXT-PERIOD
                     PERFORM 0095-MOVE-TABLE
                                       THRU 0095-EXIT
                  WHEN +2
                     MOVE 'MTD'        TO EXT-PERIOD
                     PERFORM 0096-CALC-TABLE
                                       THRU 0096-EXIT
                  WHEN +3
                     MOVE 'YTD'        TO EXT-PERIOD
                     PERFORM 0096-CALC-TABLE
                                       THRU 0096-EXIT
                  WHEN OTHER
                     MOVE 'ERR'        TO EXT-PERIOD
               END-EVALUATE
          
               PERFORM 0100-WRITE-EXTRACT
                                       THRU 0100-EXIT
             END-PERFORM
           END-PERFORM
                                       
                  
           .
       0090-EXIT.
           EXIT.

       0095-MOVE-TABLE.

           MOVE ZEROS                  TO EXT-EPR-78-B
                                          EXT-EPR-PR-B
                                          EXT-EPR-ST-B
                                          EXT-DU-B
                                          EXT-IBNR-B
                                          EXT-LR-B
                                          EXT-MORT-RESV-B
           MOVE WS-ISS-BEN   (WS-S1 WS-S2)
                                       TO EXT-ISS-BEN
           MOVE WS-CNC-BEN   (WS-S1 WS-S2)
                                       TO EXT-CNC-BEN
           MOVE WS-ISS-PRM   (WS-S1 WS-S2)
                                       TO EXT-ISS-PRM
           MOVE WS-CNC-PRM   (WS-S1 WS-S2)
                                       TO EXT-CNC-PRM
           MOVE WS-PRM-78    (WS-S1 WS-S2)
                                       TO EXT-EPR-78
                                          EXT-EPR-78-E
           MOVE WS-PRM-PR    (WS-S1 WS-S2)
                                       TO EXT-EPR-PR
                                          EXT-EPR-PR-E
           MOVE WS-PRM-ST    (WS-S1 WS-S2)
                                       TO EXT-EPR-ST
                                          EXT-EPR-ST-E
           MOVE WS-CLM-DU    (WS-S1 WS-S2)
                                       TO EXT-DU
                                          EXT-DU-E
           MOVE WS-CLM-PV    (WS-S1 WS-S2)
                                       TO EXT-PV
           MOVE WS-CLM-IBNR  (WS-S1 WS-S2)
                                       TO EXT-IBNR
                                          EXT-IBNR-E
           MOVE WS-LOSS-RESV (WS-S1 WS-S2)
                                       TO EXT-LR
                                          EXT-LR-E
           MOVE WS-CLM-AMT   (WS-S1 WS-S2)
                                       TO EXT-CLM-AMT
           MOVE WS-PRM-TAX   (WS-S1 WS-S2)
                                       TO EXT-PRM-TAX
           MOVE WS-PRM-TAX-R78 (WS-S1 WS-S2)
                                       TO EXT-TAX-R78
           MOVE WS-PRM-TAX-PRO (WS-S1 WS-S2)
                                       TO EXT-TAX-PRO
           MOVE WS-PRM-TAX-ST  (WS-S1 WS-S2)
                                       TO EXT-TAX-ST
           MOVE WS-RETRO-PAYMENTS (WS-S1 WS-S2)
                                       TO EXT-RETRO-PAYMENTS
           MOVE WS-MORT-RESV (WS-S1 WS-S2)
                                       TO EXT-MORT-RESV
                                          EXT-MORT-RESV-E
           MOVE WS-IN-FORCE  (WS-S1 WS-S2)
                                       TO EXT-IN-FORCE
           MOVE WS-DIRECT-COMM (WS-S1 WS-S2)
                                       TO EXT-DIRECT-COMM
           MOVE WS-DIRECT-COMM-R78 (WS-S1 WS-S2)
                                       TO EXT-COMM-R78
           MOVE WS-DIRECT-COMM-PRO (WS-S1 WS-S2)
                                       TO EXT-COMM-PRO
           MOVE WS-DIRECT-COMM-ST  (WS-S1 WS-S2)
                                       TO EXT-COMM-ST
           MOVE WS-OW-COMM (WS-S1 WS-S2)
                                       TO EXT-OW-COMM
           MOVE WS-OW-COMM-R78 (WS-S1 WS-S2)
                                       TO EXT-OW-COMM-R78
           MOVE WS-OW-COMM-PRO (WS-S1 WS-S2)
                                       TO EXT-OW-COMM-PRO
           MOVE WS-OW-COMM-ST  (WS-S1 WS-S2)
                                       TO EXT-OW-COMM-ST
           MOVE '*'                    TO EXT-END
       
           .
       0095-EXIT.
           EXIT.
           
       0096-CALC-TABLE.

           COMPUTE EXT-ISS-BEN = WS-ISS-BEN (WS-S1 1)
              - WS-ISS-BEN (WS-S1 WS-S2)
           COMPUTE EXT-CNC-BEN = WS-CNC-BEN (WS-S1 1)
              - WS-CNC-BEN (WS-S1 WS-S2)
           COMPUTE EXT-ISS-PRM = WS-ISS-PRM (WS-S1 1)
              - WS-ISS-PRM (WS-S1 WS-S2)
           COMPUTE EXT-CNC-PRM = WS-CNC-PRM (WS-S1 1)
              - WS-CNC-PRM (WS-S1 WS-S2)
           COMPUTE EXT-EPR-78 = WS-PRM-78 (WS-S1 1)
              - WS-PRM-78 (WS-S1 WS-S2)
           MOVE WS-PRM-78 (WS-S1 WS-S2)
                                       TO EXT-EPR-78-B
           MOVE WS-PRM-78 (WS-S1 1)    TO EXT-EPR-78-E
           COMPUTE EXT-EPR-PR = WS-PRM-PR (WS-S1 1)
              - WS-PRM-PR (WS-S1 WS-S2)
           MOVE WS-PRM-PR (WS-S1 WS-S2)
                                       TO EXT-EPR-PR-B
           MOVE WS-PRM-PR (WS-S1 1)    TO EXT-EPR-PR-E
           COMPUTE EXT-EPR-ST = WS-PRM-ST (WS-S1 1)
              - WS-PRM-ST (WS-S1 WS-S2)
           MOVE WS-PRM-ST (WS-S1 WS-S2)
                                       TO EXT-EPR-ST-B
           MOVE WS-PRM-ST (WS-S1 1)    TO EXT-EPR-ST-E
           COMPUTE EXT-DU = WS-CLM-DU (WS-S1 1)
              - WS-CLM-DU (WS-S1 WS-S2)
           MOVE WS-CLM-DU (WS-S1 1)    TO EXT-DU-E
           MOVE WS-CLM-DU (WS-S1 WS-S2)
                                       TO EXT-DU-B
           COMPUTE EXT-PV = WS-CLM-PV (WS-S1 1)
              - WS-CLM-PV (WS-S1 WS-S2)
           COMPUTE EXT-IBNR = WS-CLM-IBNR (WS-S1 1)
              - WS-CLM-IBNR (WS-S1 WS-S2)
           MOVE WS-CLM-IBNR (WS-S1 1)  TO EXT-IBNR-E
           MOVE WS-CLM-IBNR (WS-S1 WS-S2)
                                       TO EXT-IBNR-B
           COMPUTE EXT-LR = WS-LOSS-RESV (WS-S1 1)
              - WS-LOSS-RESV (WS-S1 WS-S2)
           MOVE WS-LOSS-RESV (WS-S1 1) TO EXT-LR-E
           MOVE WS-LOSS-RESV (WS-S1 WS-S2)
                                       TO EXT-LR-B
           COMPUTE EXT-CLM-AMT = WS-CLM-AMT (WS-S1 1)
              - WS-CLM-AMT (WS-S1 WS-S2)
           COMPUTE EXT-PRM-TAX = WS-PRM-TAX (WS-S1 1)
              - WS-PRM-TAX (WS-S1 WS-S2)
           COMPUTE EXT-TAX-R78 = WS-PRM-TAX-R78 (WS-S1 1)
              - WS-PRM-TAX-R78 (WS-S1 WS-S2)
           COMPUTE EXT-TAX-PRO = WS-PRM-TAX-PRO (WS-S1 1)
              - WS-PRM-TAX-PRO (WS-S1 WS-S2)
           COMPUTE EXT-TAX-ST = WS-PRM-TAX-ST (WS-S1 1)
              - WS-PRM-TAX-ST (WS-S1 WS-S2)
           COMPUTE EXT-RETRO-PAYMENTS = WS-RETRO-PAYMENTS (WS-S1 1)
              - WS-RETRO-PAYMENTS (WS-S1 WS-S2)
           COMPUTE EXT-MORT-RESV = WS-MORT-RESV (WS-S1 1)
              - WS-MORT-RESV (WS-S1 WS-S2)
           MOVE WS-MORT-RESV (WS-S1 1) TO EXT-MORT-RESV-E
           MOVE WS-MORT-RESV (WS-S1 WS-S2)
                                       TO EXT-MORT-RESV-B
           COMPUTE EXT-IN-FORCE = WS-IN-FORCE (WS-S1 1)
              - WS-IN-FORCE (WS-S1 WS-S2)
           COMPUTE EXT-DIRECT-COMM = WS-DIRECT-COMM (WS-S1 1)
              - WS-DIRECT-COMM (WS-S1 WS-S2)
           COMPUTE EXT-COMM-R78 = WS-DIRECT-COMM-R78 (WS-S1 1)
              - WS-DIRECT-COMM-R78 (WS-S1 WS-S2)
           COMPUTE EXT-COMM-PRO = WS-DIRECT-COMM-PRO (WS-S1 1)
              - WS-DIRECT-COMM-PRO (WS-S1 WS-S2)
           COMPUTE EXT-COMM-ST  = WS-DIRECT-COMM-ST (WS-S1 1)
              - WS-DIRECT-COMM-ST (WS-S1 WS-S2)
           COMPUTE EXT-OW-COMM = WS-OW-COMM (WS-S1 1)
              - WS-OW-COMM (WS-S1 WS-S2)
           COMPUTE EXT-OW-COMM-R78 = WS-OW-COMM-R78 (WS-S1 1)
              - WS-OW-COMM-R78 (WS-S1 WS-S2)
           COMPUTE EXT-OW-COMM-PRO = WS-OW-COMM-PRO (WS-S1 1)
              - WS-OW-COMM-PRO (WS-S1 WS-S2)
           COMPUTE EXT-OW-COMM-ST  = WS-OW-COMM-ST (WS-S1 1)
              - WS-OW-COMM-ST (WS-S1 WS-S2)
                         
           MOVE '*'                    TO EXT-END
       
           .
       0096-EXIT.
           EXIT.

       0100-WRITE-EXTRACT.
           
           IF WS-S2 > 1
               IF (WS-ISS-BEN (WS-S1 1) - WS-ISS-BEN (WS-S1 WS-S2) 
                               NOT = ZEROS)
                  OR (WS-CNC-BEN (WS-S1 1) - WS-CNC-BEN (WS-S1 WS-S2)
                               NOT = ZEROS)
                  OR (WS-ISS-PRM (WS-S1 1) - WS-ISS-PRM (WS-S1 WS-S2)
                               NOT = ZEROS)
                  OR (WS-CNC-PRM (WS-S1 1) - WS-CNC-PRM (WS-S1 WS-S2)
                               NOT = ZEROS)
                  OR (WS-CLM-AMT (WS-S1 1) - WS-CLM-AMT (WS-S1 WS-S2)
                               NOT = ZEROS)
                  OR (WS-PRM-78 (WS-S1 1) - WS-PRM-78 (WS-S1 WS-S2)         
                               NOT = ZEROS)
                  OR (WS-PRM-PR (WS-S1 1) - WS-PRM-PR (WS-S1 WS-S2)        
                               NOT = ZEROS)
                  OR (WS-PRM-ST (WS-S1 1) - WS-PRM-ST (WS-S1 WS-S2)        
                               NOT = ZEROS)
                  OR (WS-CLM-DU (WS-S1 1) - WS-CLM-DU (WS-S1 WS-S2)       
                               NOT = ZEROS)
                  OR (WS-CLM-PV (WS-S1 1) - WS-CLM-PV (WS-S1 WS-S2)       
                               NOT = ZEROS)
                  OR (WS-CLM-IBNR (WS-S1 1) - 
                      WS-CLM-IBNR (WS-S1 WS-S2) NOT = ZEROS)
                  OR (WS-MORT-RESV (WS-S1 1) - 
                     WS-MORT-RESV (WS-S1 WS-S2) NOT = ZEROS)
                  OR (WS-PRM-TAX  (WS-S1 1) - 
                      WS-PRM-TAX (WS-S1 WS-S2)  NOT = ZEROS)
                  OR (WS-PRM-TAX-R78 (WS-S1 1) - 
                      WS-PRM-TAX-R78 (WS-S1 WS-S2) NOT = ZEROS)
                  OR (WS-PRM-TAX-PRO (WS-S1 1) - 
                      WS-PRM-TAX-PRO (WS-S1 WS-S2) NOT = ZEROS)
                  OR (WS-PRM-TAX-ST  (WS-S1 1) - 
                      WS-PRM-TAX-ST (WS-S1 WS-S2)  NOT = ZEROS)
                  OR (WS-DIRECT-COMM (WS-S1 1) - 
                      WS-DIRECT-COMM (WS-S1 WS-S2) NOT = ZEROS)
                  OR (WS-DIRECT-COMM-R78 (WS-S1 1) - 
                      WS-DIRECT-COMM-R78 (WS-S1 WS-S2) NOT = ZEROS)
                  OR (WS-DIRECT-COMM-PRO (WS-S1 1) - 
                      WS-DIRECT-COMM-PRO (WS-S1 WS-S2) NOT = ZEROS)
                  OR (WS-DIRECT-COMM-ST (WS-S1 1) - 
                      WS-DIRECT-COMM-ST (WS-S1 WS-S2) NOT = ZEROS)
                  OR (WS-OW-COMM (WS-S1 1) - 
                      WS-OW-COMM (WS-S1 WS-S2) NOT = ZEROS)
                  OR (WS-OW-COMM-R78 (WS-S1 1) - 
                      WS-OW-COMM-R78 (WS-S1 WS-S2) NOT = ZEROS)
                  OR (WS-OW-COMM-PRO (WS-S1 1) - 
                      WS-OW-COMM-PRO (WS-S1 WS-S2) NOT = ZEROS)
                  OR (WS-OW-COMM-ST (WS-S1 1) - 
                      WS-OW-COMM-ST (WS-S1 WS-S2) NOT = ZEROS)                                                           
                    WRITE EXTRACT-RECORD-OUT
                                       FROM EXTRACT-RECORD
                    ADD 1              TO EXTR-OUT-CNT
               END-IF
           ELSE 
              IF (WS-ISS-BEN (WS-S1 1) NOT = ZEROS)
                 OR (WS-CNC-BEN (WS-S1 1) NOT = ZEROS)
                 OR (WS-ISS-PRM (WS-S1 1) NOT = ZEROS)
                 OR (WS-CNC-PRM (WS-S1 1) NOT = ZEROS)
                 OR (WS-CLM-AMT (WS-S1 1) NOT = ZEROS)
                 OR (WS-PRM-78 (WS-S1 1) NOT = ZEROS)          
                 OR (WS-PRM-PR (WS-S1 1) NOT = ZEROS)          
                 OR (WS-PRM-ST (WS-S1 1) NOT = ZEROS)          
                 OR (WS-CLM-DU (WS-S1 1) NOT = ZEROS)          
                 OR (WS-CLM-PV (WS-S1 1) NOT = ZEROS)          
                 OR (WS-CLM-IBNR (WS-S1 1) NOT = ZEROS)        
                 OR (WS-MORT-RESV (WS-S1 1) NOT = ZEROS)       
                 OR (WS-PRM-TAX (WS-S1 1) NOT = ZEROS)         
                 OR (WS-PRM-TAX-R78 (WS-S1 1) NOT = ZEROS)     
                 OR (WS-PRM-TAX-PRO (WS-S1 1) NOT = ZEROS)     
                 OR (WS-PRM-TAX-ST (WS-S1 1) NOT = ZEROS)     
                 OR (WS-DIRECT-COMM (WS-S1 1) NOT = ZEROS)     
                 OR (WS-OW-COMM (WS-S1 1) NOT = ZEROS)     
                 OR (WS-DIRECT-COMM-R78 (WS-S1 1) NOT = ZEROS) 
                 OR (WS-DIRECT-COMM-PRO (WS-S1 1) NOT = ZEROS) 
                 OR (WS-DIRECT-COMM-ST (WS-S1 1) NOT = ZEROS)  
                 OR (WS-OW-COMM-R78 (WS-S1 1) NOT = ZEROS)     
                 OR (WS-OW-COMM-PRO (WS-S1 1) NOT = ZEROS)     
                 OR (WS-OW-COMM-ST (WS-S1 1) NOT = ZEROS)                                       
                     WRITE EXTRACT-RECORD-OUT
                                       FROM EXTRACT-RECORD
                     ADD 1             TO EXTR-OUT-CNT
              END-IF
           END-IF

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

           EJECT
