       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDCRX4.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERTS        ASSIGN TO SYS010.

           SELECT ERMAIL       ASSIGN TO ERMAIL
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED              
                               FILE STATUS IS ERMAIL-FILE-STATUS   
                               RECORD KEY IS MA-CONTROL-PRIMARY.    

           SELECT EXTRACT      ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.
                                                                        
              

       DATA DIVISION.
       FILE SECTION.

       FD  CERTS
                                       COPY ECSCRIFD.
                                       COPY ECSCRT01.

       FD  ERMAIL.                                                      
                                       COPY ERCMAIL.

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD-HEAD         PIC x(1010).
       01  EXTRACT-RECORD-OUT          PIC x(901).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDCRX4 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-CERT                VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  CERT-IN-CNT                 PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.
       77  WS-CANC-DATE                PIC 9(8)  VALUE ZEROS.
       77  S1                          PIC S999  VALUE +0 COMP-3.
       77  ERMAIL-FILE-STATUS          PIC XX    VALUE LOW-VALUES.
       01  EXTRACT-HEAD-RECORD.
           05  FILLER                  PIC X(7)  VALUE 'CARRIER'.
           05  EXT-HTAB1               PIC X.
           05  FILLER                  PIC X(5)  VALUE 'GROUP'.
           05  EXT-HTAB2               PIC X.
           05  FILLER                  PIC X(5)  VALUE 'STATE'.
           05  EXT-HTAB3               PIC X.
           05  FILLER                  PIC X(7)  VALUE 'ACCOUNT'.
           05  EXT-HTAB4               PIC X.
           05  FILLER                  PIC X(8)  VALUE 'EFF DATE'.
           05  EXT-HTAB5               PIC X.
           05  FILLER                  PIC X(7)  VALUE 'CERT NO'.
           05  EXT-HTAB6               PIC X.
           05  FILLER                  PIC X(8)  VALUE 'ENT DATE'.
           05  EXT-HTAB7               PIC X.
           05  FILLER                  PIC X(12) VALUE '1ST PAY DATE'.
           05  EXT-HTAB8               PIC X.
           05  FILLER                  PIC X(10) VALUE 'ENT STATUS'.
           05  EXT-HTAB9               PIC X.
           05  FILLER                  PIC X(9)  VALUE 'LAST NAME'.
           05  EXT-HTAB10              PIC X.
           05  FILLER                  PIC X(10) VALUE 'FIRST NAME'.
           05  EXT-HTAB11              PIC X.
           05  FILLER                  PIC X(8)  VALUE 'MID INIT'.
           05  EXT-HTAB12              PIC X.
           05  FILLER                  PIC X(13) VALUE 'JNT LAST NAME'.
           05  EXT-HTAB13              PIC X.
           05  FILLER                  PIC X(14) VALUE 'JNT FIRST NAME'.
           05  EXT-HTAB14              PIC X.
           05  FILLER                  PIC X(12) VALUE 'JNT MID INIT'.
           05  EXT-HTAB15              PIC X.
           05  FILLER                  PIC X(3)  VALUE 'SSN'.
           05  EXT-HTAB16              PIC X.
           05  FILLER                  PIC X(9)  VALUE 'MEMBER NO'.
           05  EXT-HTAB17              PIC X.
           05  FILLER                  PIC X(3)  VALUE 'SEX'.
           05  EXT-HTAB18              PIC X.
           05  FILLER                  PIC X(7)  VALUE 'INS AGE'.
           05  EXT-HTAB19              PIC X.
           05  FILLER                  PIC X(7)  VALUE 'JNT AGE'.
           05  EXT-HTAB20              PIC X.
           05  FILLER                  PIC X(3)  VALUE 'APR'.    
           05  EXT-HTAB21              PIC X.
           05  FILLER                  PIC X(9)  VALUE 'LOAN TERM'.
           05  EXT-HTAB22              PIC X.
           05  FILLER                  PIC X(8)  VALUE 'EXT DAYS'.
           05  EXT-HTAB23              PIC X.
           05  FILLER                  PIC X(7)  VALUE 'IND GRP'.
           05  EXT-HTAB24              PIC X.
           05  FILLER                  PIC X(4)  VALUE 'FREQ'.
           05  EXT-HTAB25              PIC X.
           05  FILLER                  PIC X(11) VALUE 'LF BEN CODE'.
           05  EXT-HTAB26              PIC X.
           05  FILLER                  PIC X(13) VALUE 'LF CUR STATUS'.
           05  EXT-HTAB27              PIC X.
           05  FILLER                  PIC X(7)  VALUE 'LF TERM'.
           05  EXT-HTAB28              PIC X.
           05  FILLER                  PIC X(6)  VALUE 'LF AMT'.
           05  EXT-HTAB29              PIC X.
           05  FILLER                  PIC X(7)  VALUE 'LF PREM'.
           05  EXT-HTAB30              PIC X.
           05  FILLER                  PIC X(10) VALUE 'LF AMT ALT'.
           05  EXT-HTAB31              PIC X.
           05  FILLER                  PIC X(11) VALUE 'LF PREM ALT'.
           05  EXT-HTAB32              PIC X.
           05  FILLER                  PIC X(11) VALUE 'LF NSP PREM'.
           05  EXT-HTAB33              PIC X.
           05  FILLER                  PIC X(9)  VALUE 'DEATH AMT'.
           05  EXT-HTAB34              PIC X.
           05  FILLER                  PIC X(10) VALUE 'LF REM AMT'.
           05  EXT-HTAB35              PIC X.
           05  FILLER                  PIC X(12) VALUE 'LF PREM RATE'.
           05  EXT-HTAB36              PIC X.
           05  FILLER                  PIC X(16) VALUE
                                              'LF PREM RATE ALT'.
           05  EXT-HTAB37              PIC X.
           05  FILLER                  PIC X(9)  VALUE 'LF REFUND'.
           05  EXT-HTAB38              PIC X.
           05  FILLER                  PIC X(11) VALUE 'LF EXP DATE'.
           05  EXT-HTAB39              PIC X.
           05  FILLER                  PIC X(11) VALUE 'LF CAN DATE'.
           05  EXT-HTAB40              PIC X.
           05  FILLER                  PIC X(13) VALUE 'LF CAN EXT DT'.
           05  EXT-HTAB41              PIC X.
           05  FILLER                  PIC X(11) VALUE 'LF DTH DATE'.
           05  EXT-HTAB42              PIC X.
           05  FILLER                  PIC X(13) VALUE 'LF DTH EXT DT'.
           05  EXT-HTAB43              PIC X.
           05  FILLER                  PIC X(11) VALUE 'AH BEN CODE'.
           05  EXT-HTAB44              PIC X.
           05  FILLER                  PIC X(13) VALUE 'AH CUR STATUS'.
           05  EXT-HTAB45              PIC X.
           05  FILLER                  PIC X(7)  VALUE 'AH TERM'.
           05  EXT-HTAB46              PIC X.
           05  FILLER                  PIC X(6)  VALUE 'AH AMT'.
           05  EXT-HTAB47              PIC X.
           05  FILLER                  PIC X(7)  VALUE 'AH PREM'.
           05  EXT-HTAB48              PIC X.
010307     05  FILLER                  PIC X(12) VALUE 'NET TOT FEES'.
           05  EXT-HTAB48A             PIC X.
           05  FILLER                  PIC X(11) VALUE 'AH NSP PREM'.
           05  EXT-HTAB49              PIC X.
           05  FILLER                  PIC X(10) VALUE 'AH CLM AMT'.
           05  EXT-HTAB49A             PIC X.
           05  FILLER                  PIC X(12) VALUE 'AH PREM RATE'.
           05  EXT-HTAB50              PIC X.
           05  FILLER                  PIC X(11) VALUE 'AH CRIT PER'.
           05  EXT-HTAB51              PIC X.
           05  FILLER                  PIC X(9)  VALUE 'AH REFUND'.
           05  EXT-HTAB52              PIC X.
           05  FILLER                  PIC X(11) VALUE 'AH EXP DATE'.
           05  EXT-HTAB53              PIC X.
           05  FILLER                  PIC X(11) VALUE 'AH CAN DATE'.
           05  EXT-HTAB54              PIC X.
           05  FILLER                  PIC X(13) VALUE 'AH CAN EXT DT'.
           05  EXT-HTAB55              PIC X.
           05  FILLER                  PIC X(11) VALUE 'AH DIS DATE'.
           05  EXT-HTAB56              PIC X.
           05  FILLER                  PIC X(5)  VALUE 'AGT 1'.
           05  EXT-HTAB57              PIC X.
           05  FILLER                  PIC X(5)  VALUE 'TYP 1'.
           05  EXT-HTAB58              PIC X.
           05  FILLER                  PIC X(4)  VALUE 'LF 1'.  
           05  EXT-HTAB59              PIC X.
           05  FILLER                  PIC X(4)  VALUE 'AH 1'. 
           05  EXT-HTAB60              PIC X.
           05  FILLER                  PIC X(5)  VALUE 'AGT 2'.
           05  EXT-HTAB61              PIC X.
           05  FILLER                  PIC X(5)  VALUE 'TYP 2'.
           05  EXT-HTAB62              PIC X.
           05  FILLER                  PIC X(4)  VALUE 'LF 2'.  
           05  EXT-HTAB63              PIC X.
           05  FILLER                  PIC X(4)  VALUE 'AH 2'.  
           05  EXT-HTAB64              PIC X.
           05  FILLER                  PIC X(5)  VALUE 'AGT 3'.
           05  EXT-HTAB65              PIC X.
           05  FILLER                  PIC X(5)  VALUE 'TYP 3'.
           05  EXT-HTAB66              PIC X.
           05  FILLER                  PIC X(4)  VALUE 'LF 3'. 
           05  EXT-HTAB67              PIC X.
           05  FILLER                  PIC X(4)  VALUE 'AH 3'.  
           05  EXT-HTAB68              PIC X.
           05  FILLER                  PIC X(5)  VALUE 'AGT 4'. 
           05  EXT-HTAB69              PIC X.
           05  FILLER                  PIC X(5)  VALUE 'TYP 4'.
           05  EXT-HTAB70              PIC X.
           05  FILLER                  PIC X(4)  VALUE 'LF 4'. 
           05  EXT-HTAB71              PIC X.
           05  FILLER                  PIC X(4)  VALUE 'AH 4'. 
           05  EXT-HTAB72              PIC X.
           05  FILLER                  PIC X(5)  VALUE 'AGT 5'.
           05  EXT-HTAB73              PIC X.
           05  FILLER                  PIC X(5)  VALUE 'TYP 5'.
           05  EXT-HTAB74              PIC X.
           05  FILLER                  PIC X(4)  VALUE 'LF 5'.
           05  EXT-HTAB75              PIC X.
           05  FILLER                  PIC X(4)  VALUE 'AH 5'.
           05  EXT-HTAB76              PIC X.
           05  FILLER                  PIC X(8)  VALUE 'ISS CCYY'.
           05  EXT-HTAB77              PIC X.
           05  FILLER                  PIC X(7)  VALUE 'BANK NO'.
           05  EXT-HTAB78              PIC X.
           05  FILLER                  PIC X(8)  VALUE 'LOAN OFF'.
           05  EXT-HTAB79              PIC X.
           05  FILLER                  PIC X(9)  VALUE 'INS ADDR1'.
           05  EXT-HTAB80              PIC X.
           05  FILLER                  PIC X(9)  VALUE 'INS ADDR2'.
           05  EXT-HTAB81              PIC X.
           05  FILLER                  PIC X(11) VALUE 'INS CITY ST'.
           05  EXT-HTAB82              PIC X.
           05  FILLER                  PIC X(7)  VALUE 'INS ZIP'.
           05  EXT-HTAB83              PIC X.
           05  FILLER                  PIC X(3)  VALUE 'EOR'.
       01  WS-INIT-EXTRACT             PIC X(900).
       01  EXTRACT-RECORD.
           05  EXT-CARRIER             PIC X.
           05  EXT-TAB1                PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-TAB2                PIC X.
           05  EXT-STATE               PIC XX.
           05  EXT-TAB3                PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-TAB4                PIC X.
           05  EXT-EFF-DT              PIC X(10).
           05  EXT-TAB5                PIC X.
           05  EXT-CERT-NO             PIC X(11).
           05  EXT-TAB6                PIC X.
           05  EXT-ENT-DT              PIC X(10).
           05  EXT-TAB7                PIC X.
           05  EXT-1ST-PAY-DT          PIC X(10).
           05  EXT-TAB8                PIC X.
           05  EXT-ENTRY-STATUS        PIC X.
           05  EXT-TAB9                PIC X.
           05  EXT-LAST-NAME           PIC X(15).
           05  EXT-TAB10               PIC X.
           05  EXT-FIRST-NAME          PIC X(10).
           05  EXT-TAB11               PIC X.
           05  EXT-MID-INIT            PIC X.
           05  EXT-TAB12               PIC X.
           05  EXT-JNT-LAST-NAME       PIC X(15).
           05  EXT-TAB13               PIC X.
           05  EXT-JNT-FIRST-NAME      PIC X(10).
           05  EXT-TAB14               PIC X.
           05  EXT-JNT-MID-INIT        PIC X.
           05  EXT-TAB15               PIC X.
           05  EXT-SSN                 PIC X(11).
           05  EXT-TAB16               PIC X.
           05  EXT-MEMBER-NO           PIC X(12).
           05  EXT-TAB17               PIC X.
           05  EXT-SEX                 PIC X.
           05  EXT-TAB18               PIC X.
           05  EXT-INS-AGE             PIC 99.
           05  EXT-TAB19               PIC X.
           05  EXT-JNT-AGE             PIC 99.
           05  EXT-TAB20               PIC X.
           05  EXT-APR                 PIC ZZ9.9999.
           05  EXT-TAB21               PIC X.
           05  EXT-LOAN-TERM           PIC 999.
           05  EXT-TAB22               PIC X.
           05  EXT-EXT-DAYS            PIC 999.
           05  EXT-TAB23               PIC X.
           05  EXT-IND-GRP             PIC X.
           05  EXT-TAB24               PIC X.
           05  EXT-FREQ                PIC 99.
           05  EXT-TAB25               PIC X.
           05  EXT-LF-BEN              PIC XX.
           05  EXT-TAB26               PIC X.
           05  EXT-LF-CUR-STATUS       PIC X.
           05  EXT-TAB27               PIC X.
           05  EXT-LF-TERM             PIC 999.
           05  EXT-TAB28               PIC X.
           05  EXT-LF-AMT              PIC ZZZZZZ9.99.
           05  EXT-TAB29               PIC X.
           05  EXT-LF-PREM             PIC ZZZZZZ9.99.
           05  EXT-TAB30               PIC X.
           05  EXT-LF-AMT-ALT          PIC ZZZZZZ9.99.
           05  EXT-TAB31               PIC X.
           05  EXT-LF-PREM-ALT         PIC ZZZZZZ9.99.
           05  EXT-TAB32               PIC X.
           05  EXT-LF-NSP-PREM         PIC ZZZZZZ9.99.
           05  EXT-TAB33               PIC X.
           05  EXT-LF-DTH-AMT          PIC ZZZZZZ9.99.
           05  EXT-TAB34               PIC X.
           05  EXT-LF-REM-AMT          PIC ZZZZZZ9.99.
           05  EXT-TAB35               PIC X.
           05  EXT-LF-PREM-RATE        PIC Z9.9(5).
           05  EXT-TAB36               PIC X.
           05  EXT-LF-PREM-RATE-ALT    PIC Z9.9(5).
           05  EXT-TAB37               PIC X.
           05  EXT-LF-REFUND           PIC ZZZZZZ9.99.
           05  EXT-TAB38               PIC X.
           05  EXT-LF-EXP-DT           PIC X(10).
           05  EXT-TAB39               PIC X.
           05  EXT-LF-CAN-DT           PIC X(10).
           05  EXT-TAB40               PIC X.
           05  EXT-LF-CAN-EXT-DT       PIC X(10).
           05  EXT-TAB41               PIC X.
           05  EXT-LF-DTH-DT           PIC X(10).
           05  EXT-TAB42               PIC X.
           05  EXT-LF-DTH-EXT-DT       PIC X(10).
           05  EXT-TAB43               PIC X.
           05  EXT-AH-BEN              PIC XX.
           05  EXT-TAB44               PIC X.
           05  EXT-AH-CUR-STATUS       PIC X.
           05  EXT-TAB45               PIC X.
           05  EXT-AH-TERM             PIC 999.
           05  EXT-TAB46               PIC X.
           05  EXT-AH-AMT              PIC ZZZZZZ9.99.
           05  EXT-TAB47               PIC X.
           05  EXT-AH-PREM             PIC ZZZZZZ9.99.
           05  EXT-TAB48               PIC X.
           05  EXT-TOT-FEES            PIC ZZZZZZZ9.99.
           05  EXT-TAB48A              PIC X.
           05  EXT-AH-NSP-PREM         PIC ZZZZZZ9.99.
           05  EXT-TAB49               PIC X.
           05  EXT-AH-CLM-AMT          PIC ZZZZZZ9.99.
           05  EXT-TAB49A              PIC X.
           05  EXT-AH-PREM-RATE        PIC Z9.9(5).
           05  EXT-TAB50               PIC X.
           05  EXT-AH-CRIT-PER         PIC 999.
           05  EXT-TAB51               PIC X.
           05  EXT-AH-REFUND           PIC ZZZZZZ9.99.
           05  EXT-TAB52               PIC X.
           05  EXT-AH-EXP-DT           PIC X(10).
           05  EXT-TAB53               PIC X.
           05  EXT-AH-CAN-DT           PIC X(10).
           05  EXT-TAB54               PIC X.
           05  EXT-AH-CAN-EXT-DT       PIC X(10).
           05  EXT-TAB55               PIC X.
           05  EXT-AH-DIS-DT           PIC X(10).
           05  EXT-TAB56               PIC X.
           05  EXT-AGT1                PIC X(10).
           05  EXT-TAB57               PIC X.
           05  EXT-TYP1                PIC X.
           05  EXT-TAB58               PIC X.
           05  EXT-LF-COM1             PIC .99999.
           05  EXT-TAB59               PIC X.
           05  EXT-AH-COM1             PIC .99999.
           05  EXT-TAB60               PIC X.
           05  EXT-AGT2                PIC X(10).
           05  EXT-TAB61               PIC X.
           05  EXT-TYP2                PIC X.
           05  EXT-TAB62               PIC X.
           05  EXT-LF-COM2             PIC .99999.
           05  EXT-TAB63               PIC X.
           05  EXT-AH-COM2             PIC .99999.
           05  EXT-TAB64               PIC X.
           05  EXT-AGT3                PIC X(10).
           05  EXT-TAB65               PIC X.
           05  EXT-TYP3                PIC X.
           05  EXT-TAB66               PIC X.
           05  EXT-LF-COM3             PIC .99999.
           05  EXT-TAB67               PIC X.
           05  EXT-AH-COM3             PIC .99999.
           05  EXT-TAB68               PIC X.
           05  EXT-AGT4                PIC X(10).
           05  EXT-TAB69               PIC X.
           05  EXT-TYP4                PIC X.
           05  EXT-TAB70               PIC X.
           05  EXT-LF-COM4             PIC .99999.
           05  EXT-TAB71               PIC X.
           05  EXT-AH-COM4             PIC .99999.
           05  EXT-TAB72               PIC X.
           05  EXT-AGT5                PIC X(10).
           05  EXT-TAB73               PIC X.
           05  EXT-TYP5                PIC X.
           05  EXT-TAB74               PIC X.
           05  EXT-LF-COM5             PIC .99999.
           05  EXT-TAB75               PIC X.
           05  EXT-AH-COM5             PIC .99999.
           05  EXT-TAB76               PIC X.
           05  EXT-ISSUE-CCYY          PIC 9999.
           05  EXT-TAB77               PIC X.
           05  EXT-BANK-NO             PIC X(10).
           05  EXT-TAB78               PIC X.
           05  EXT-LOAN-OFF            PIC X(5).
           05  EXT-TAB79               PIC X.
           05  EXT-INS-ADDR1           PIC X(30).
           05  EXT-TAB80               PIC X.
           05  EXT-INS-ADDR2           PIC X(30).
           05  EXT-TAB81               PIC X.
           05  EXT-INS-CITY-ST         PIC X(30).
           05  EXT-TAB82               PIC X.
           05  EXT-INS-ZIP             PIC X(9). 
           05  EXT-TAB83               PIC X.
           05  EXT-EOR                 PIC X.

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
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC X(4).
               10  WS-WORK-MM          PIC XX.
               10  WS-WORK-DD          PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

       PROCEDURE DIVISION.

       0002-INPUT.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           PERFORM 0080-PROCESS-CERT   THRU 0080-EXIT UNTIL
PEMTST           (END-OF-CERT)
PEMTST*          OR (CERT-IN-CNT > 100000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           GOBACK
           .
       0002-EXIT.
           EXIT.

       0010-INITIALIZE.

           MOVE SPACES                 TO EXTRACT-RECORD
           MOVE ';'                    TO EXT-HTAB1
                                          EXT-HTAB2
                                          EXT-HTAB3
                                          EXT-HTAB4 
                                          EXT-HTAB5 
                                          EXT-HTAB6
                                          EXT-HTAB7 
                                          EXT-HTAB8 
                                          EXT-HTAB9 
                                          EXT-HTAB10
                                          EXT-HTAB11
                                          EXT-HTAB12
                                          EXT-HTAB13
                                          EXT-HTAB14
                                          EXT-HTAB15
                                          EXT-HTAB16 
                                          EXT-HTAB17
                                          EXT-HTAB18
                                          EXT-HTAB19
                                          EXT-HTAB20
                                          EXT-HTAB21
                                          EXT-HTAB22
                                          EXT-HTAB23
                                          EXT-HTAB24
                                          EXT-HTAB25
                                          EXT-HTAB26 
                                          EXT-HTAB27
                                          EXT-HTAB28
                                          EXT-HTAB29
                                          EXT-HTAB30
                                          EXT-HTAB31
                                          EXT-HTAB32
                                          EXT-HTAB33
                                          EXT-HTAB34 
                                          EXT-HTAB35
                                          EXT-HTAB36
                                          EXT-HTAB37
                                          EXT-HTAB38 
                                          EXT-HTAB39
                                          EXT-HTAB40
                                          EXT-HTAB41
                                          EXT-HTAB42 
                                          EXT-HTAB43
                                          EXT-HTAB44
                                          EXT-HTAB45
                                          EXT-HTAB46
                                          EXT-HTAB47
                                          EXT-HTAB48
                                          EXT-HTAB48A
                                          EXT-HTAB49
                                          EXT-HTAB49A
                                          EXT-HTAB50
                                          EXT-HTAB51
                                          EXT-HTAB52
                                          EXT-HTAB53
                                          EXT-HTAB54
                                          EXT-HTAB55
                                          EXT-HTAB56
                                          EXT-HTAB57
                                          EXT-HTAB58
                                          EXT-HTAB59
                                          EXT-HTAB60
                                          EXT-HTAB61
                                          EXT-HTAB62
                                          EXT-HTAB63
                                          EXT-HTAB64
                                          EXT-HTAB65
                                          EXT-HTAB66
                                          EXT-HTAB67
                                          EXT-HTAB68
                                          EXT-HTAB69
                                          EXT-HTAB70
                                          EXT-HTAB71
                                          EXT-HTAB72
                                          EXT-HTAB73
                                          EXT-HTAB74
                                          EXT-HTAB75
                                          EXT-HTAB76
                                          EXT-HTAB77
                                          EXT-HTAB78
                                          EXT-HTAB79
                                          EXT-HTAB80
                                          EXT-HTAB81
                                          EXT-HTAB82
                                          EXT-HTAB83
           MOVE X'09'                  TO EXT-TAB1
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
                                          EXT-TAB48A
                                          EXT-TAB49
                                          EXT-TAB49A
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
                                          EXT-TAB79
                                          EXT-TAB80
                                          EXT-TAB81
                                          EXT-TAB82
                                          EXT-TAB83
           MOVE 'E'                    TO EXT-EOR
           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           WRITE EXTRACT-RECORD-HEAD   FROM EXTRACT-HEAD-RECORD

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT CERTS ERMAIL
               OUTPUT EXTRACT

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL ERROR - OPEN ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' CERT IN RECORDS  ' CERT-IN-CNT
           DISPLAY ' EXTR OUT RECORDS ' EXTR-OUT-CNT
           CLOSE CERTS ERMAIL
               EXTRACT

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL ERROR - CLOSE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0060-READ-CERT.

           READ CERTS AT END
               SET END-OF-CERT TO TRUE
           END-READ

           IF NOT END-OF-CERT
              ADD 1 TO CERT-IN-CNT
           END-IF

           .

       0060-EXIT.
           EXIT.


       0080-PROCESS-CERT.

           IF CR-ACCOUNT = '0000869200'
              PERFORM 0090-BUILD-EXTRACT  THRU 0090-EXIT
           END-IF
           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.

           MOVE WS-INIT-EXTRACT        TO EXTRACT-RECORD
           MOVE CR-CARRIER             TO EXT-CARRIER
           MOVE CR-GROUPING            TO EXT-GROUP
           MOVE CR-STATE               TO EXT-STATE
           MOVE CR-ACCOUNT             TO EXT-ACCOUNT
           MOVE CR-CERT-NO             TO EXT-CERT-NO

           IF CR-ENTRY-STATUS = '5'
              MOVE +0                  TO CR-LFPRM
                                          CR-LFPRM-ALT
                                          CR-AHPRM
                                          CR-MOB-NET-TOT-FEES
           END-IF

           MOVE CR-DT                  TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-EFF-DT
           END-STRING

           MOVE WS-CCYY                TO EXT-ISSUE-CCYY

           STRING CR-1ST-PMT-MO '/' CR-1ST-PMT-DA '/'
              CR-1ST-PMT-YR DELIMITED BY SIZE
               INTO EXT-1ST-PAY-DT
           END-STRING

           MOVE CR-ENTRY-DATE          TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-ENT-DT
           END-STRING
           MOVE CR-ENTRY-STATUS        TO EXT-ENTRY-STATUS

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +11
              IF (CR-SOC-SEC (S1:1) < 'A'
                 OR > 'Z')
                 AND (CR-SOC-SEC (S1:1) NOT NUMERIC)
                 AND (CR-SOC-SEC (S1:1) NOT = ' ')
      *          DISPLAY ' FIXING SSN ' CR-CERT-NO
      *             ' ' CR-SOC-SEC (S1:1)
                 MOVE ' '              TO CR-SOC-SEC (S1:1)
              END-IF
           END-PERFORM
           MOVE CR-SOC-SEC             TO EXT-SSN

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +12
              IF (CR-MEMBER-NO (S1:1) < 'A'
                 OR > 'Z')
                 AND (CR-MEMBER-NO (S1:1) NOT NUMERIC)
                 AND (CR-MEMBER-NO (S1:1) NOT = ' ')
                 DISPLAY ' FIXING MEM NO ' CR-CERT-NO
                    ' ' CR-MEMBER-NO (S1:1)
                 MOVE ' '              TO CR-MEMBER-NO (S1:1)
              END-IF
           END-PERFORM
           MOVE CR-MEMBER-NO           TO EXT-MEMBER-NO

           IF CR-APR > 100.00
              MOVE ZEROS               TO CR-APR
           END-IF
           MOVE CR-APR                 TO EXT-APR
           MOVE CR-LOAN-TERM           TO EXT-LOAN-TERM
           MOVE CR-AGE                 TO EXT-INS-AGE
           MOVE CR-SEX                 TO EXT-SEX
           MOVE CR-IND-GRP             TO EXT-IND-GRP
           MOVE CR-PMT-FREQ            TO EXT-FREQ

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +26
              IF (CR-NAME (S1:1) < 'A'
                 OR > 'Z')
                 AND (CR-NAME (S1:1) NOT = ' ' AND '-' AND ',' AND '.')
                 DISPLAY ' FIXING CR NAME ' CR-CERT-NO
                    ' ' CR-NAME (S1:1)
                 MOVE SPACES             TO CR-NAME (S1:1)
              END-IF
           END-PERFORM
           MOVE CR-LNAME               TO EXT-LAST-NAME
           MOVE CR-FNAME               TO EXT-FIRST-NAME
           MOVE CR-INIT                TO EXT-MID-INIT

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +26
              IF (CR-JOINT-NAME (S1:1) < 'A'
                 OR > 'Z')
                 AND (CR-JOINT-NAME (S1:1) NOT = ' ' AND '-' AND ','
                    AND '.')
                 DISPLAY ' FIXING CR JOINT NAME ' CR-CERT-NO
                    ' ' CR-JOINT-NAME (S1:1)
                 MOVE SPACES             TO CR-JOINT-NAME (S1:1)
              END-IF
           END-PERFORM
           MOVE CR-JT-LNAME            TO EXT-JNT-LAST-NAME
           MOVE CR-JT-FNAME            TO EXT-JNT-FIRST-NAME
           MOVE CR-JT-INIT             TO EXT-JNT-MID-INIT

           MOVE CR-JOINT-AGE           TO EXT-JNT-AGE
           MOVE CR-PMT-EXTENSION-DAYS  TO EXT-EXT-DAYS

           MOVE CR-LFTYP               TO EXT-LF-BEN
           MOVE CR-LF-CURRENT-STATUS   TO EXT-LF-CUR-STATUS
           IF CR-LFAMT NOT NUMERIC
              DISPLAY ' CR-LFAMT NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-LFAMT
           END-IF
           MOVE CR-LFAMT               TO EXT-LF-AMT
           IF CR-LFAMT-ALT NOT NUMERIC
              DISPLAY ' CR-LFAMT-ALT NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-LFAMT-ALT
           END-IF
           MOVE CR-LFAMT-ALT           TO EXT-LF-AMT-ALT
           MOVE CR-LF-TERM             TO EXT-LF-TERM
           IF CR-LFPRM NOT NUMERIC
              DISPLAY ' CR-LFPRM NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-LFPRM
           END-IF
           MOVE CR-LFPRM               TO EXT-LF-PREM
           IF CR-LFPRM-ALT NOT NUMERIC
              DISPLAY ' CR-LFPRM-ALT NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-LFPRM-ALT
           END-IF
           MOVE CR-LFPRM-ALT           TO EXT-LF-PREM-ALT
           MOVE ZEROS                  TO EXT-LF-REM-AMT
           MOVE CR-LF-NSP-PRM          TO EXT-LF-NSP-PREM
           MOVE CR-LFPRM-RATE          TO EXT-LF-PREM-RATE
           MOVE CR-LFPRM-RATE-ALT      TO EXT-LF-PREM-RATE-ALT

           MOVE CR-LFRFND              TO EXT-LF-REFUND
           MOVE CR-DTHAMT              TO EXT-LF-DTH-AMT

           IF CR-LF-EXPIRE-DATE = ZEROS
              MOVE SPACES              TO EXT-LF-EXP-DT
           ELSE
              MOVE CR-LF-EXPIRE-DATE   TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-LF-EXP-DT
              END-STRING
           END-IF

           IF CR-LF-CANC-DT = ZEROS
              MOVE SPACES              TO EXT-LF-CAN-DT
           ELSE
              MOVE CR-LF-CANC-DT       TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-LF-CAN-DT
              END-STRING
           END-IF

           IF CR-LF-CANCEL-EXIT-DATE = ZEROS
              MOVE SPACES              TO EXT-LF-CAN-EXT-DT
           ELSE
              MOVE CR-LF-CANCEL-EXIT-DATE
                                       TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-LF-CAN-EXT-DT
              END-STRING
           END-IF

           IF CR-DTH-DT = ZEROS
              MOVE SPACES              TO EXT-LF-DTH-DT
           ELSE
              MOVE CR-DTH-DT           TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-LF-DTH-DT
              END-STRING
           END-IF

           IF CR-LF-CLAIM-EXIT-DATE = ZEROS
              MOVE SPACES              TO EXT-LF-DTH-EXT-DT
           ELSE
              MOVE CR-LF-CLAIM-EXIT-DATE
                                       TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-LF-DTH-EXT-DT
              END-STRING
           END-IF



           MOVE CR-AHTYP               TO EXT-AH-BEN
           MOVE CR-AH-CURRENT-STATUS   TO EXT-AH-CUR-STATUS
           IF CR-AHAMT NOT NUMERIC
              DISPLAY ' CR-AHAMT NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-AHAMT
           END-IF
           MOVE CR-AHAMT               TO EXT-AH-AMT
           MOVE CR-AH-TERM             TO EXT-AH-TERM
           IF CR-AHPRM NOT NUMERIC
              DISPLAY ' CR-AHPRM NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-AHPRM
           END-IF
           MOVE CR-AHPRM               TO EXT-AH-PREM
           IF CR-MOB-NET-TOT-FEES NOT NUMERIC
              MOVE ZEROS               TO CR-MOB-NET-TOT-FEES
           END-IF
           IF CR-MOB-NET-TOT-FEES NOT NUMERIC
              DISPLAY ' CR-MOB-NET-TOT-FEES NOT NUMERIC ' CR-CERT-NO
              MOVE ZEROS               TO CR-MOB-NET-TOT-FEES
           END-IF
           MOVE CR-MOB-NET-TOT-FEES    TO EXT-TOT-FEES
           MOVE CR-AH-NSP-PRM          TO EXT-AH-NSP-PREM
           MOVE CR-AHPRM-RATE          TO EXT-AH-PREM-RATE
           MOVE CR-AH-CRIT-PERIOD      TO EXT-AH-CRIT-PER

           MOVE CR-AHRFND              TO EXT-AH-REFUND

           IF CR-AH-EXPIRE-DATE = ZEROS
              MOVE SPACES              TO EXT-AH-EXP-DT
           ELSE
              MOVE CR-AH-EXPIRE-DATE   TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-AH-EXP-DT
              END-STRING
           END-IF

           IF CR-AH-CANC-DT = ZEROS
              MOVE SPACES              TO EXT-AH-CAN-DT
           ELSE
              MOVE CR-AH-CANC-DT       TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                 INTO EXT-AH-CAN-DT
              END-STRING
           END-IF

           IF CR-AH-CANCEL-EXIT-DATE = ZEROS
              MOVE SPACES              TO EXT-AH-CAN-EXT-DT
           ELSE
              MOVE CR-AH-CANCEL-EXIT-DATE
                                       TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-AH-CAN-EXT-DT
              END-STRING
           END-IF

           IF CR-DIS-DT = ZEROS
              MOVE SPACES              TO EXT-AH-DIS-DT
           ELSE
              MOVE CR-DIS-DT           TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-AH-DIS-DT
              END-STRING
           END-IF

           IF CR-DISAMT NOT NUMERIC
              MOVE ZEROS               TO CR-DISAMT
           END-IF
           MOVE CR-DISAMT              TO EXT-AH-CLM-AMT           

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > 5
              IF CR-COM-AGT (S1) = LOW-VALUES OR HIGH-VALUES
                                OR SPACES
                 MOVE ZEROS            TO CR-COM-AGT (S1)
              END-IF
           END-PERFORM
           MOVE CR-COM-AGT (1)         TO EXT-AGT1
           MOVE CR-AGT-TYPE (1)        TO EXT-TYP1
           MOVE CR-LCOM-L (1)          TO EXT-LF-COM1
           MOVE CR-LCOM-AH (1)         TO EXT-AH-COM1
           MOVE CR-COM-AGT (2)         TO EXT-AGT2
           MOVE CR-AGT-TYPE (2)        TO EXT-TYP2
           MOVE CR-LCOM-L (2)          TO EXT-LF-COM2
           MOVE CR-LCOM-AH (2)         TO EXT-AH-COM2
           MOVE CR-COM-AGT (3)         TO EXT-AGT3
           MOVE CR-AGT-TYPE (3)        TO EXT-TYP3
           MOVE CR-LCOM-L (3)          TO EXT-LF-COM3
           MOVE CR-LCOM-AH (3)         TO EXT-AH-COM3
           MOVE CR-COM-AGT (4)         TO EXT-AGT4
           MOVE CR-AGT-TYPE (4)        TO EXT-TYP4
           MOVE CR-LCOM-L (4)          TO EXT-LF-COM4
           MOVE CR-LCOM-AH (4)         TO EXT-AH-COM4
           MOVE CR-COM-AGT (5)         TO EXT-AGT5
           MOVE CR-AGT-TYPE (5)        TO EXT-TYP5
           MOVE CR-LCOM-L (5)          TO EXT-LF-COM5
           MOVE CR-LCOM-AH (5)         TO EXT-AH-COM5
           MOVE CR-BANK-NO             TO EXT-BANK-NO
           MOVE CR-LOAN-OFFICER        TO EXT-LOAN-OFF
                  
           MOVE 'E'                    TO EXT-EOR

           PERFORM 0110-GET-ERMAIL  THRU 0110-EXIT
           
           IF ERMAIL-FILE-STATUS = '00'
              MOVE MA-ADDRESS-LINE-1   TO EXT-INS-ADDR1
              MOVE MA-ADDRESS-LINE-2   TO EXT-INS-ADDR2
              MOVE MA-CITY-STATE       TO EXT-INS-CITY-ST
              MOVE MA-ZIP              TO EXT-INS-ZIP
           END-IF

           PERFORM 0100-WRITE-EXTRACT  THRU 0100-EXIT

           .
       0090-EXIT.
           EXIT.

       0100-WRITE-EXTRACT.

           INSPECT EXTRACT-RECORD REPLACING
              ALL ';'         BY ' '
              ALL X'00'       BY ' '
              ALL X'01'       BY ' '
              ALL X'02'       BY ' '
              ALL X'03'       BY ' '
              ALL X'04'       BY ' '
              ALL X'05'       BY ' '
              ALL X'06'       BY ' '
              ALL X'07'       BY ' '
              ALL X'08'       BY ' '
              ALL X'0C'       BY ' '
              ALL X'14'       BY ' '
              ALL X'1B'       BY ' '
              ALL X'1C'       BY ' '
              ALL X'1E'       BY ' '
              ALL X'9C'       BY ' '
              ALL X'09'       BY ';'
           WRITE EXTRACT-RECORD-OUT    FROM EXTRACT-RECORD
           ADD 1 TO EXTR-OUT-CNT

           .
       0100-EXIT.
           EXIT.

       0110-GET-ERMAIL.

           MOVE LOW-VALUES             TO MA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO MA-COMPANY-CD
           MOVE X'04'                  TO MA-COMPANY-CD
           MOVE CR-CARRIER             TO MA-CARRIER
           MOVE CR-GROUPING            TO MA-GROUPING
           MOVE CR-STATE               TO MA-STATE
           MOVE CR-ACCOUNT             TO MA-ACCOUNT
           MOVE CR-CERT-NO             TO MA-CERT-NO
           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO MA-CERT-EFF-DT
              READ ERMAIL
              IF ERMAIL-FILE-STATUS = '00' OR '23' OR '22' OR '10'
                 CONTINUE
              ELSE
                 DISPLAY ' ERMAIL ERROR - READ ' ERMAIL-FILE-STATUS
                    '  ' CR-CERT-NO
           ELSE
              DISPLAY ' BAD EFF DATE ' CR-DT '  ' CR-CERT-NO   
           END-IF
           
           .
       0110-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
