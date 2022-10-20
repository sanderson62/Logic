       IDENTIFICATION DIVISION.                                         00000010
       PROGRAM-ID.              LGCIFCE.                                00000011
      *AUTHOR. MARK FERRIS.                                             00000030
      *DATE-COMPILED.                                                   00000040
      *REMARKS.                                                         00000050
      ******************************************************************00000051
      * REWRITTEN FOR CLAS-IC II LOGIC IN SEPT.1988.                   *00000052
      *                                                                *00000053
      * COPYBOOKS WERE NOT USED FOR RECORD FORMATS BECAUSE OF THE NEED *00000054
      * TO HAVE ALL RECORD FORMATS AVAILABLE AT THE SAME TIME.         *00000055
      * LOGIC COPYBOOKS USE REDEFINES FOR THE VARIOUS RECORD LAYOUTS.  *00000056
      *                                                                *00000057
      *                                                                *00000058
      *                        IMPORTANT                               *00000059
      *        PROCESSING DATE MUST BE THE DATE THAT THE USERS         *00000060
      *        HAVE ENTERED THEIR DATA.  ALSO THE EXTRACTS MUST BE     *00000061
      *        SORTED INTO FA, FB, AND FC ORDER.                       *00000062
      *                        IMPORTANT                               *00000063
      *                                                                *00000064
      ******************************************************************00000065
      *    DATE    PGMR DESCRIPTION                                    *00000066
      * ---------- ---- -----------------------------------------------*00000067
      * 12/01/1993 DANA ADDED ROUTINE B011-CALC-DATE (IR#933217683)    *00000068
      * 12/13/1997 DJNA Y2K PHASE 2                                    *00000070
      * 03/01/1998 DANA IR#1998020200003 FLAG NEGATIVE ADJUSTMENTS FOR *00000070
      *                                  MSA                           *00000070
      * 04/01/2000 DJNA CR#2000030100009 DRAFT NUMBER EXPANSION        *00000070
      * 08/26/2000 DJNA CR#2000041300004 DATA EXPANSION                *00000070
030702* 03/07/2002 SMVA 2001112600001    REMOVE PLAN CODE TABLE
030702*                                  DEPENDENCIES
103002* 103002                   PEMA    ADD DATE FILE PROCESSING TO SPLIT
103002*                                  CID AND DCC TRANSACTIONS     
122402* 122402     SMVA 2001061800003    ADD PROCESSING FOR NEW CLM TYP I
012803* 012803     SMVA 2001061800003    KEEP DRAFT NUMBER AT 7 BYTES    
012803*                                  FOR DCC 
121703* 121703     SMVA 2003080800002    ADD PROCESSING FOR NEW CLM TYP G
052804* 052804     SMVA 2004051200002    INCR SZ OF ELCEXTR TO ADD RPT CD 1
090804* 090804     SMVA 2004063000008    FIX DAR NOTE CODE SHOULDN'T BE SET
090804*                                  ON POSITIVE AMTS 
122205* 122205    2005033100001  PEMA  ADD PROCESSING FOR CSI
091808* 091808    2008022800002  AJRA  FIND STATE CODE FOR STATE NAME IN ADDR
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
032012* 032012   2011110200001   AJRA  AHL CHANGES
051412* 051412 IR2012050700003   AJRA  FIX AHL VOIDS
052614* 052614 CR2014022100001   AJRA  ADD FAMILY LEAVE CLAIM TYPE
111714* 111714  CR2014073000001  PEMA  DRAFTS TO CHECKS
021215* 021215 IR2015020900001   PEMA  PASS CARRIER CODE ON DFTACT FILE
090815* 090815 IR2015090200002   PEMA  Correct Payee info on checks
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
022017* 022017 CR2017022000001   PEMA  DCC DRAFTS TO CHECKS
082317* 082317  CR2017082100003  PEMA  ADD SUB TYPE to ACH processing
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
062821* 063021  CR2021021600001  TANA  ADD FNL COMPANY CODE
070921* 070921 IR2021070900001   PEMA  Add CID coding for ACH void.
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
      ******************************************************************00000071
       ENVIRONMENT DIVISION.                                            00000110
       INPUT-OUTPUT SECTION.                                            00000120
       FILE-CONTROL.                                                    00000130
                                                                        00000140
           SELECT CYCLE-DATES   ASSIGN TO SYS006-UR-2540R-S-SYS006.     00000150
                                                                        00000160
           SELECT ELBENE                                                00000161
               ASSIGN TO SYS021-FBA1-ELBENE                             00000162
               ORGANIZATION IS INDEXED                                  00000190
               ACCESS IS RANDOM                                         00000200
               RECORD KEY IS BE-CONTROL-PRIMARY                         00000210
               FILE STATUS IS BENE-STATUS.                              00000220
                                                                        00000230
           SELECT ELCNTL                                                00000231
               ASSIGN TO SYS016-FBA1-ELCNTL                             00000232
               ORGANIZATION IS INDEXED                                  00000260
               ACCESS IS DYNAMIC                                        00000270
               RECORD KEY IS CF-CONTROL-PRIMARY                         00000280
               FILE STATUS IS CF-STATUS.                                00000290
                                                                        00000300
           SELECT EXTRACT-FILE ASSIGN TO SYS015.                        00000380
                                                                        00000390
           SELECT LG-CLM-ACT       ASSIGN TO SYS010-FBA1-CLMACT.        00000400
           SELECT LG-DFT-ACT       ASSIGN TO SYS011-FBA1-DFTACT.        00000410
103002     SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   
                                                                        00000420
       EJECT                                                            00000430
       DATA DIVISION.                                                   00000440
       FILE SECTION.                                                    00000450
                                                                        00000460
       FD  CYCLE-DATES                                                  00000470
           RECORDING MODE IS F.                                         00000480
       01  FD-CYCLE-DATE-REC              PIC X(80).                    00000520
                                                                        00000530
       FD  ELBENE.                                                      00000531
                                                                        00000580
                            COPY   ELCBENE.                             00000581
                                                                        00000620
                                                                        00000660
       FD  ELCNTL.                                                      00000661
                                                                        00000830
                             COPY   ELCCNTL.                            00000831
                                                                        00001030
                                                                        00001390
       FD  EXTRACT-FILE
                                                                        00006700
           LABEL RECORDS ARE STANDARD                                   00006710
           BLOCK CONTAINS 0 RECORDS                                     00006730
           RECORDING MODE IS F                                          00006730
           DATA RECORD IS EXTRACT-RECORD.                               00006740
      * ELCEXTR IS THE COPYBOOK WHERE THIS FORMAT CAME FROM             00006741
                                                                        00006760
       01  EXTRACT-RECORD.                                              00006770
           05  IN-WORK-DATA.                                            00006780
               10  IN-RECORD-ID                    PIC XX.              00006790
               10  IN-POSITIONING-CODE             PIC X.               00006800
               10  IN-EXTRACT-CODE                 PIC X.               00006810
               10  IN-COMPANY-CD                   PIC X.               00006820
               10  IN-COMPANY-ID                   PIC XXX.             00006830
               10  IN-RECORD-TYPE                  PIC X.               00006840
               10  IN-CARRIER                      PIC X.               00006850
               10  IN-STATE                        PIC X(02).           00006851
               10  IN-ACCOUNT-NO                   PIC X(10).           00006852
               10  IN-CLAIM-NO                     PIC X(7).            00006870
LGC003         10  IN-CERT-NO-OLD.                                      00006871
LGC003             15  IN-CERT-PRIME-OLD           PIC X(10).           00006872
LGC003             15  IN-CERT-SUFFIX-OLD          PIC X(1).            00006873
LGC003         10  IN-CERT-NO  REDEFINES IN-CERT-NO-OLD.                00006874
LGC003             15  IN-CERT-PRIME-1             PIC X.               00006875
LGC003             15  IN-CERT-PRIME.                                   00006876
LGC003                 20  IN-CERT-PRIME-9             PIC X(9).        00006877
LGC003                 20  IN-CERT-SUFFIX              PIC X(1).        00006878
               10  IN-TRAILER-SEQ-NO               PIC S9(4)    COMP.   00006890
052804     05  IN-DATA-AREA                    PIC X(277).              00006891
           EJECT                                                        00006920
       FD  LG-CLM-ACT                                                   00006930
           RECORDING MODE IS F                                          00006940
           LABEL RECORDS ARE STANDARD                                   00006950
           BLOCK CONTAINS 0 RECORDS                                     00006951
           RECORD CONTAINS 675 CHARACTERS.                              00006952
       01  CLAIM-ACTIVITY-RECORD               PIC X(675).              00006953
       FD  LG-DFT-ACT                                                   00006990
           RECORDING MODE IS F                                          00007000
           LABEL RECORDS ARE STANDARD                                   00007010
           BLOCK CONTAINS 0 RECORDS                                     00007011
           RECORD CONTAINS 1194 CHARACTERS.                             00007012
       01  DRAFT-ACTIVITY-RECORD               PIC X(1194).             00007013
                                                                        00007050
103002 FD  DISK-DATE                                                    
103002     COPY ELCDTEFD.                                               
103002                                                                  
       EJECT                                                            00007060
       WORKING-STORAGE SECTION.                                         00007070

NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
                                                                        00007080
       77  FILLER            PIC X(15)     VALUE '***************'.     00007090
       77  FILLER            PIC X(15)     VALUE 'WORKING STORAGE'.     00007100
       77  FILLER            PIC X(15)     VALUE '***************'.     00007110
                                                                        00007111
       77  ZIP-9             PIC 9(9)      VALUE ZERO.                  00007112
       77  ws-test-conv-date pic 9(8)      value zeros.

013017 77  ws-sql-code                 pic s9(7) value zeros.
013017 77  ws-dis-sql-code             pic -9999999 value zeros.
013017 77  ws-connect-sw               pic x value spaces.
013017     88  connected-to-db           value 'Y'.

013017 01  P pointer.
013017 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
013017 01  var-ptr pointer.
013017 01  env-var-len                 pic 9(4)  binary.
013017 01  rc                          pic 9(9)  binary.
013017
013017 01  WS-KIXSYS.
013017     05  WS-KIX-FIL1             PIC X(10).
013017     05  WS-KIX-APPS             PIC X(10).
013017     05  WS-KIX-ENV              PIC X(10).
013017     05  WS-KIX-MYENV            PIC X(10).
013017     05  WS-KIX-SYS              PIC X(10).

013017 EXEC SQL
013017    BEGIN DECLARE SECTION
013017 END-EXEC
013017
013017 01  sqlcmd                      pic x(1024).
013017 01  svr                         pic x(32).
013017 01  usr                         pic x(32).
013017 01  pass                        pic x(32).
013017 01  usr-pass                    pic x(64).
013017 01  ws-disp-code                pic s9(11).
013017 01  ws-rec-cntr                 pic s9(4) comp value +0.
013017 01  ws-test-date                pic x(10) value spaces.
013017 01  ws-moe-date                 PIC X(10).

       01  ws-pmt-info.
           05  ws-void-dt              pic x(10).
           05  ws-pmt-carrier          pic x.
           05  ws-pmt-group            pic x(6).
           05  ws-pmt-state            pic xx.
           05  ws-pmt-account          pic x(10).
           05  ws-pmt-cert-eff-dt      pic x(10).
           05  ws-pmt-cert-no          pic x(11).
           05  ws-pmt-claim-no         pic x(7).
           05  ws-pmt-check-no         pic x(10).

013017 EXEC SQL
013017    END DECLARE SECTION
013017 END-EXEC

       01  FILLER                    PIC X(15) VALUE '***************'. 00007116
       01  FILLER                    PIC X(15) VALUE 'ZIP-CODE FIELD '. 00007117
       01  FILLER                    PIC X(15) VALUE '***************'. 00007118
                                                                        00007119
                                                                        00007120
                                                                        00007121
       01  PAC-ZIP.                                                     00007122
           03  ZIP-PAC              PIC S9(9) VALUE ZEROS    COMP-3.    00007123
                                                                        00007124
                                                                        00007125
       01  UNPAC-ZIP.                                                   00007126
           03  ZIP-X.                                                   00007127
               05  ZIP-X-5          PIC XXXXX   VALUE ZEROS.            00007128
               05  ZIP-X-4          PIC XXXX    VALUE ZEROS.            00007129
                                                                        00007130
                                                                        00007131
       01  IN-ZIP.                                                      00007132
           03  IN-X.                                                    00007133
               05  IN-X-4          PIC XXXX     VALUE ZEROS.            00007134
               05  IN-X-5          PIC XXXXX    VALUE ZEROS.            00007135
           03  ZIP-N REDEFINES IN-X PIC 9(9).                           00007136
                                                                        00007139
       01  DIAG-COMP-01.                                                00007140
           05  DIAG-COMP         PIC S9999     COMP  VALUE +90.         00007141
                                                                        00007142
       01  TEST-SEQ-01.                                                 00007143
           05  TEST-SEQ          PIC S9999     COMP  VALUE +00.         00007144
                                                                        00007145
       01  CYCLE-DATE-REC.                                              00007146
           05  CARD-ID        PIC X(16)  VALUE '*ABEND THE RUN* '.      00007147
           05  PREV-CYC-DATE.                                           00007150
               10  P-C-MO     PIC 99     VALUE ZEROS.                   00007160
               10  P-C-DA     PIC 99     VALUE ZEROS.                   00007170
               10  P-C-YR     PIC 9999   VALUE ZEROS.                   00007171
           05  CURR-CYC-DATE.                                           00007190
               10  C-C-MO     PIC 99     VALUE ZEROS.                   00007200
               10  C-C-DA     PIC 99     VALUE ZEROS.                   00007210
               10  C-C-YR     PIC 9999   VALUE ZEROS.                   00007211
           05  FILLER         PIC X(48)  VALUE SPACES.                  00007212
                                                                        00007213
       01  WS-WORK-DATES.                                               00007214
           05  WS-TEMP-DATE.                                            00007215
               10  WS-TEMP-MM       PIC XX.                             00007216
               10  WS-TEMP-DD       PIC XX.                             00007217
               10  WS-TEMP-YYYY.                                        00007218
                   15  WS-TEMP-CC   PIC XX.                             00007219
                   15  WS-TEMP-YY   PIC XX.                             00007220
           05  WS-MMDDYY.                                               00007221
               10  WS-MDY-MM        PIC XX.                             00007222
               10  WS-MDY-DD        PIC XX.                             00007223
               10  WS-MDY-YY        PIC XX.                             00007224
                                                                        00007231
                                                                        00007240
       01  WS-WORK-DATA.                                                00007250
           05  WS-WD-RECORD-ID                    PIC XX.               00007260
           05  WS-WD-POSITIONING-CODE             PIC X.                00007270
           05  WS-WD-EXTRACT-CODE                 PIC X.                00007280
           05  WS-WD-COMPANY-CD                   PIC X.                00007290
           05  WS-WD-COMPANY-ID                   PIC XXX.              00007300
           05  WS-WD-RECORD-TYPE                  PIC X.                00007310
           05  WS-VAR-FLDS.                                             00007311
               10  WS-WD-CARRIER                  PIC X.                00007312
               10  WS-WD-STATE                    PIC X(02).            00007313
               10  WS-WD-ACCOUNT-NO               PIC X(10).            00007314
               10  WS-WD-ACCOUNT-NO-N REDEFINES  WS-WD-ACCOUNT-NO.      00007315
                   15  WS-WD-ACCOUNT-N            PIC 9(10).            00007316
               10  WS-WD-CLAIM-NO                 PIC X(7).             00007317
               10  WS-WD-CERT-NO                  PIC X(11).            00007318
               10  WS-WD-CERT-NO-CSO  REDEFINES  WS-WD-CERT-NO.         00007319
                   15  WS-WD-CERT-NO-1            PIC X(1).             00007320
                   15  WS-WD-CERT-NO-10           PIC X(10).            00007321
               10  WS-WD-TRAILER-SEQ-NO           PIC S9(4)    COMP.    00007322
           05  SG-FLDS     REDEFINES    WS-VAR-FLDS.                    00007323
               10  EX-SG-CARRIER                  PIC X.                00007324
               10  EX-SG-STATE                    PIC X(02).            00007325
               10  EX-SG-ACCOUNT-NO               PIC X(10).            00007326
               10  EX-SG-CLAIM-NO                 PIC X(7).             00007327
               10  EX-SG-CERT-NO                  PIC X(11).            00007328
               10  EX-SG-TRAILER-SEQ-NO           PIC S9(4)    COMP.    00007329
                                                                        00007380
       01  EX-EXTRACT-F-RECORD-A.                                       00007390
052804* 277 CHARACTERS                                                  00007391
               16  EX-FA-INSURED-NAME              PIC X(30).           00007392
               16  EX-FA-ACCOUNT-NAME              PIC X(30).           00007410
               16  EX-FA-INSURED-BIRTH-DT          PIC XX.              00007430
               16  EX-FA-INSURED-SEX-CD            PIC X.               00007440
               16  EX-FA-INSURED-OCC-CD            PIC XX.              00007450
               16  EX-FA-SOC-SEC-NO                PIC X(11).           00007460
               16  EX-FA-PROCESSOR-ID              PIC X(4).            00007470
               16  EX-FA-PROCESSING-INFO.                               00007480
                   20  EX-FA-CLAIM-STATUS         PIC X.                00007490
                       88  EX-FA-CLAIM-IS-OPEN          VALUE 'O'.      00007500
                       88  EX-FA-CLAIM-IS-CLOSED        VALUE 'C'.      00007510
                   20  EX-FA-CLAIM-TYPE           PIC X.                00007520
                       88  EX-FA-AH-CLAIM               VALUE 'A'.      00007530
121703                 88  EX-FA-GP-CLAIM               VALUE 'G'.      00007530
122402                 88  EX-FA-IU-CLAIM               VALUE 'I'.      00007530
052614                 88  EX-FA-FL-CLAIM               VALUE 'F'.
022122                 88  EX-FA-BR-CLAIM               VALUE 'B'.
022122                 88  EX-FA-HS-CLAIM               VALUE 'H'.
100518                 88  EX-FA-OT-CLAIM               VALUE 'O'.
                       88  EX-FA-LIFE-CLAIM             VALUE 'L'.      00007540
                   20  EX-FA-CLAIM-PREM-TYPE      PIC X.                00007550
                       88  EX-FA-SINGLE-PREMIUM         VALUE '1'.      00007560
                       88  EX-FA-O-B-COVERAGE           VALUE '2'.      00007570
                       88  EX-FA-OPEN-END-COVERAGE      VALUE '3'.      00007580
                   20  EX-FA-INCURRED-DT          PIC XX.               00007590
                   20  EX-FA-REPORTED-DT          PIC XX.               00007600
                   20  EX-FA-FILE-ESTABLISH-DT    PIC XX.               00007610
                   20  EX-FA-EST-END-OF-DISAB-DT  PIC XX.               00007620
                   20  EX-FA-LAST-PMT-DT          PIC XX.               00007630
                   20  EX-FA-LAST-PMT-AMT         PIC S9(7)V99  COMP-3. 00007640
                   20  EX-FA-PAID-THRU-DT         PIC XX.               00007650
                   20  EX-FA-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3. 00007660
                   20  EX-FA-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3. 00007670
                   20  EX-FA-NO-OF-DAYS-PAID      PIC S9(4)     COMP.   00007680
                   20  EX-FA-PMT-CALC-METHOD      PIC X.                00007690
                       88  EX-FA-360-DAY-YR          VALUE '1'.         00007700
                       88  EX-FA-365-DAY-YR          VALUE '2'.         00007710
                       88  EX-FA-FULL-MONTHS         VALUE '3'.         00007720
                   20  EX-FA-CAUSE-CD             PIC X(6).             00007730
                   20  EX-FA-DIAGNOSIS-DESCRIP    PIC X(26).            00007740
                   20  EX-FA-LAST-REOPEN-DT       PIC XX.               00007750
                   20  EX-FA-LAST-CLOSE-DT        PIC XX.               00007760
                   20  EX-FA-LAST-CLOSE-REASON    PIC X.                00007770
                       88  EX-FA-FINAL-PAID             VALUE '1'.      00007780
                       88  EX-FA-CLAIM-DENIED           VALUE '2'.      00007790
                       88  EX-FA-AUTO-CLOSE             VALUE '3'.      00007800
               16  EX-FA-CERTIFICATE-DATA.                              00007810
                   20  EX-FA-CERT-ORIGIN          PIC X.                00007820
                       88  EX-FA-CERT-WAS-ONLINE        VALUE '1'.      00007830
                       88  EX-FA-CERT-WAS-CREATED       VALUE '2'.      00007840
                       88  EX-FA-COVERAGE-WAS-ADDED     VALUE '3'.      00007850
               16  EX-FA-STATUS-CONTROLS.                               00007860
                   20  EX-FA-PRIORITY-CD          PIC X.                00007870
                       88  EX-FA-HIGHEST-PRIORITY       VALUE '9'.      00007880
                   20  EX-FA-SUPV-ATTN-CD         PIC X.                00007890
                       88  EX-FA-SUPV-NOT-REQUIRED      VALUE ' ' 'N'.  00007900
                       88  EX-FA-SUPV-IS-REQUIRED       VALUE 'Y'.      00007910
                   20  EX-FA-PURGED-DT            PIC XX.               00007920
                   20  EX-FA-RESTORED-DT          PIC XX.               00007930
                   20  EX-FA-NEXT-AUTO-PAY-DT     PIC XX.               00007940
                   20  EX-FA-NEXT-RESEND-DT       PIC XX.               00007950
                   20  EX-FA-NEXT-FOLLOWUP-DT     PIC XX.               00007960
                   20  FILLER                  PIC XX.                  00007970
                   20  EX-FA-LAST-MAINT-DT        PIC XX.               00007980
                   20  EX-FA-LAST-MAINT-USER      PIC X(4).             00007990
                   20  EX-FA-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3. 00008000
                   20  EX-FA-LAST-MAINT-TYPE      PIC X.                00008010
                       88  EX-FA-CLAIM-SET-UP           VALUE ' '.      00008020
                       88  EX-FA-PAYMENT-MADE           VALUE '1'.      00008030
                       88  EX-FA-LETTER-SENT            VALUE '2'.      00008040
                       88  EX-FA-MASTER-WAS-ALTERED     VALUE '3'.      00008050
                       88  EX-FA-MASTER-WAS-RESTORED    VALUE '4'.      00008060
                       88  EX-FA-INCURRED-DATE-CHANGED  VALUE '5'.      00008070
                       88  EX-FA-FILE-CONVERTED         VALUE '6'.      00008080
                   20  EX-FA-RELATED-CLAIM-NO     PIC X(7).             00008090
                   20  EX-FA-HISTORY-ARCHIVE-DT   PIC XX.               00008100
               16  EX-FA-TRAILER-CONTROLS.                              00008110
                   20  EX-FA-TRAILER-SEQ-CNT      PIC S9(4)     COMP.   00008120
                       88  EX-FA-1ST-TRL-AVAIL       VALUE +4095.       00008130
                       88  EX-FA-LAST-TRL-AVAIL      VALUE +1.          00008140
                       88  EX-FA-RESV-EXP-HIST-TRLR  VALUE +0.          00008150
                   20  EX-FA-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.   00008160
                   20  FILLER                  PIC XX.                  00008170
                   20  EX-FA-AUTO-PAY-SEQ         PIC S9(4)     COMP.   00008180
                   20  EX-FA-ACCOUNT-ADDR-CNT PIC S9(1).                00008181
                       88  EX-FA-ACCOUNT-IS-ONLINE  VALUE ZERO.         00008200
                   20  FILLER                  PIC  X.                  00008201
               16  EX-FA-FILE-LOCATION            PIC X(4).             00008210
               16  EX-FA-BENEFICIARY              PIC X(10).            00008211
052804         16  FILLER                         PIC X(71).            00008212
                                                                        00008230
                                                                        00008240
       01  EX-EXTRACT-F-RECORD-B.                                       00008250
               16  EX-FB-INSURED-NAME              PIC X(30).           00008251
               16  EX-FB-EFFECTIVE-DT              PIC XX.              00008280
               16  EX-FB-SSN                       PIC X(11).           00008310
               16  EX-FB-MEMBER-NO                 PIC X(12).           00008320
               16  EX-FB-INSURED-ISSUE-AGE         PIC 99.              00008330
               16  EX-FB-SPOUSE-ISSUE-AGE          PIC 99.              00008340
               16  EX-FB-INSURED-SEX               PIC X.               00008350
               16  EX-FB-LIFE-DATA.                                     00008360
                   20  EX-FB-LF-BENEFIT-CD        PIC XX.               00008361
                   20  EX-FB-LF-ORIG-TERM         PIC S9(3)     COMP-3. 00008380
                   20  EX-FB-LF-BENEFIT-AMT       PIC S9(9)V99  COMP-3. 00008381
                   20  EX-FB-LF-PREMIUM-AMT       PIC S9(7)V99  COMP-3. 00008382
                   20  EX-FB-LF-REMAINING-AMT     PIC S9(9)V99  COMP-3. 00008383
                   20  EX-FB-LF-ITD-CANCEL-AMT    PIC S9(5)V99  COMP-3. 00008420
                   20  EX-FB-LF-ITD-DEATH-AMT     PIC S9(7)V99  COMP-3. 00008430
                   20  EX-FB-LF-DEV-CODE          PIC X(03).            00008431
               16  EX-FB-AH-DATA.                                       00008450
                   20  EX-FB-AH-BENEFIT-CD        PIC XX.               00008451
                   20  EX-FB-AH-ORIG-TERM         PIC S9(3)     COMP-3. 00008470
                   20  EX-FB-AH-BENEFIT-AMT       PIC S9(9)V99  COMP-3. 00008471
                   20  EX-FB-AH-PREMIUM-AMT       PIC S9(7)V99  COMP-3. 00008472
                   20  EX-FB-AH-ITD-CANCEL-AMT    PIC S9(5)V99  COMP-3. 00008500
                   20  EX-FB-AH-ITD-LUMP-PMT      PIC S9(7)V99  COMP-3. 00008510
                   20  EX-FB-AH-DEV-CODE          PIC X(03).            00008511
               16  EX-FB-LOAN-INFORMATION.                              00008530
                   20  EX-FB-LOAN-APR             PIC S999V9999 COMP-3. 00008540
                   20  EX-FB-PAY-FREQUENCY        PIC S99.              00008550
                   20  EX-FB-LOAN-TERM            PIC S999      COMP-3. 00008560
                   20  EX-FB-RATE-CLASS           PIC XX.               00008570
                   20  EX-FB-POLICY-FORM-NO       PIC X(12).            00008571
                   20  EX-FB-PREMIUM-TYPE         PIC X.                00008600
                       88  EX-FB-SING-PRM            VALUE '1'.         00008610
                       88  EX-FB-O-B-COVERAGE        VALUE '2'.         00008620
                       88  EX-FB-OPEN-END            VALUE '3'.         00008630
                   20  EX-FB-IND-GRP-TYPE         PIC X.                00008640
                       88  EX-FB-INDIVIDUAL          VALUE 'I'.         00008650
                       88  EX-FB-GROUP               VALUE 'G'.         00008660
                   20  EX-FB-SKIP-CODE            PIC X.                00008670
                       88  NO-MONTHS-SKIPPED      VALUE SPACE.          00008680
                       88  SKIP-JULY              VALUE '1'.            00008690
                       88  SKIP-AUGUST            VALUE '2'.            00008700
                       88  SKIP-SEPTEMBER         VALUE '3'.            00008710
                       88  SKIP-JULY-AUG          VALUE '4'.            00008720
                       88  SKIP-AUG-SEPT          VALUE '5'.            00008730
                       88  SKIP-JULY-AUG-SEPT     VALUE '6'.            00008740
                   20  EX-FB-PAYMENT-MODE         PIC X.                00008750
                       88  PAY-MONTHLY            VALUE SPACE.          00008760
                       88  PAY-WEEKLY             VALUE '1'.            00008770
                       88  PAY-SEMI-MONTHLY       VALUE '2'.            00008780
                       88  PAY-BI-WEEKLY          VALUE '3'.            00008790
                       88  PAY-SEMI-ANUALLY       VALUE '4'.            00008800
                   20  EX-FB-LOAN-NUMBER          PIC X(8).             00008810
                   20  EX-FB-LOAN-BALANCE         PIC S9(7)V99  COMP-3. 00008820
                   20  EX-FB-REIN-TABLE           PIC X(3).             00008830
                   20  EX-FB-SPECIAL-REIN-CODE    PIC X.                00008840
               16  EX-FB-STATUS-DATA.                                   00008850
                   20  EX-FB-AH-CANCEL-DT         PIC XX.               00008860
                   20  EX-FB-LF-CANCEL-DT         PIC XX.               00008870
                   20  EX-FB-AH-SETTLEMENT-DT     PIC XX.               00008880
                   20  EX-FB-LF-DEATH-DT          PIC XX.               00008890
                   20  EX-FB-ENTRY-DT             PIC XX.               00008900
LGC003             20  EX-FB-AH-CANCEL-EXIT-DT    PIC XX.               00008901
LGC003             20  EX-FB-AH-SETTLEMENT-EXIT-DT PIC XX.              00008902
LGC003             20  EX-FB-LF-CANCEL-EXIT-DT    PIC XX.               00008903
LGC003             20  EX-FB-LF-DEATH-EXIT-DT     PIC XX.               00008904
                   20  EX-FB-LF-CURRENT-STATUS    PIC X.                00008930
                       88  EX-FB-LF-POLICY-IS-ACTIVE     VALUE '1' '2'  00008940
                                                       '3' '4' '5' '9'. 00008950
                       88  EX-FB-LF-NORMAL-ENTRY         VALUE '1'.     00008960
                       88  EX-FB-LF-POLICY-PENDING       VALUE '2'.     00008970
                       88  EX-FB-LF-POLICY-IS-RESTORE    VALUE '3'.     00008980
                       88  EX-FB-LF-CONVERSION-ENTRY     VALUE '4'.     00008990
                       88  EX-FB-LF-POLICY-IS-REISSUE    VALUE '5'.     00009000
                       88  EX-FB-LF-LUMP-SUM-DISAB       VALUE '6'.     00009010
                       88  EX-FB-LF-DEATH-CLAIM-APPLIED  VALUE '7'.     00009020
                       88  EX-FB-LF-CANCEL-APPLIED       VALUE '8'.     00009030
                       88  EX-FB-LF-IS-REIN-ONLY         VALUE '9'.     00009040
                   20  EX-FB-LF-ENTRY-STATUS      PIC X.                00009050
                   20  EX-FB-LF-STATUS-AT-DEATH   PIC X.                00009060
                   20  EX-FB-LF-STATUS-AT-CANCEL  PIC X.                00009070
                   20  EX-FB-AH-CURRENT-STATUS    PIC X.                00009080
                       88  EX-FB-AH-POLICY-IS-ACTIVE     VALUE '1' '2'  00009090
                                                       '3' '4' '5' '9'. 00009100
                       88  EX-FB-AH-NORMAL-ENTRY         VALUE '1'.     00009110
                       88  EX-FB-AH-POLICY-PENDING       VALUE '2'.     00009120
                       88  EX-FB-AH-POLICY-IS-RESTORE    VALUE '3'.     00009130
                       88  EX-FB-AH-CONVERSION-ENTRY     VALUE '4'.     00009140
                       88  EX-FB-AH-POLICY-IS-REISSUE    VALUE '5'.     00009150
                       88  EX-FB-AH-LUMP-SUM-DISAB       VALUE '6'.     00009160
                       88  EX-FB-AH-DEATH-CLAIM-APPLIED  VALUE '7'.     00009170
                       88  EX-FB-AH-CANCEL-APPLIED       VALUE '8'.     00009180
                       88  EX-FB-AH-IS-REIN-ONLY         VALUE '9'.     00009190
                   20  EX-FB-AH-ENTRY-STATUS       PIC X.               00009200
                   20  EX-FB-AH-STATUS-AT-LUMP-SUM PIC X.               00009210
                   20  EX-FB-AH-STATUS-AT-CANCEL   PIC X.               00009220
                   20  EX-FB-CLAIM-INTERFACE-SW  PIC X.                 00009230
                       88  EX-FB-NO-CLAIM-ATTACHED          VALUE SPACE.00009240
                       88  EX-FB-CERT-AND-CLAIM-ONLINE      VALUE '1'.  00009250
                       88  EX-FB-CERT-WAS-CREATED           VALUE '2'.  00009260
                   20  EX-FB-CLAIM-ATTACHED-COUNT PIC S9(4)       COMP. 00009270
                   20  EX-FB-ENTRY-BATCH          PIC X(6).             00009280
                   20  EX-FB-LAST-MONTH-END       PIC XX.               00009290
                   20  EX-FB-AH-PAID-THRU-DT      PIC XX.               00009300
                       88  EX-FB-NO-AH-CLAIMS-PAID    VALUE LOW-VALUE.  00009310
               16  EX-FB-CREDIT-INTERFACE-SW-1    PIC X.                00009320
                   88  EX-FB-CERT-ADDED-BATCH           VALUE ' '.      00009330
                   88  EX-FB-CERT-ADDED-ONLINE          VALUE '1'.      00009340
                   88  EX-FB-CERT-PEND-ISSUE-ERROR      VALUE '2'.      00009350
                   88  EX-FB-CERT-PURGED-OFFLINE        VALUE '3'.      00009360
               16  EX-FB-CREDIT-INTERFACE-SW-2    PIC X.                00009370
                   88  EX-FB-CERT-AS-LOADED             VALUE ' '.      00009380
                   88  EX-FB-CERT-CANCELLED-ONLINE      VALUE '1'.      00009390
                   88  EX-FB-CERT-CLAIM-ONLINE          VALUE '2'.      00009400
                   88  EX-FB-CERT-CLAIM-CANCEL-ONLINE   VALUE '3'.      00009410
                   88  EX-FB-CERT-PEND-CANCEL-ERROR     VALUE '4'.      00009420
                   88  EX-FB-CERT-PEND-CANCEL-VOID      VALUE '5'.      00009430
                   88  EX-FB-CERT-PEND-CAN-VOID-ERROR   VALUE '6'.      00009440
               16  EX-FB-ACCOUNT-COMM-PCTS.                             00009450
                   20  EX-FB-LIFE-COMM-PCT        PIC SV9(5)    COMP-3. 00009460
                   20  EX-FB-AH-COMM-PCT          PIC SV9(5)    COMP-3. 00009470
                                                                        00009471
               16  CSO-FILLER-CHG.                                      00009472
                   20  EX-FB-GROUPING             PIC X(6).             00009473
                   20  EX-FB-STATE                PIC XX.               00009474
                   20  EX-FB-INSURED-LAST-NAME    PIC X(15).            00009475
                   20  EX-FB-INSURED-INITIALS     PIC XX.               00009476
                   20  FILLER                     PIC X(32).            00009477
                                                                        00009478
                                                                        00009480
                                                                        00009490
       01  EX-EXTRACT-F-RECORD-C.                                       00009500
      * 165  CHARACTERS                                                 00009501
               16  EX-FC-INSURED-NAME              PIC X(30).           00009502
               16  EX-FC-TRAILER-TYPE              PIC X.               00009520
                   88  EX-FC-RESERVE-EXPENSE-TR         VALUE '1'.      00009530
                   88  EX-FC-PAYMENT-TR                 VALUE '2'.      00009540
                   88  EX-FC-AUTO-PAY-TR                VALUE '3'.      00009550
                   88  EX-FC-CORRESPONDENCE-TR          VALUE '4'.      00009560
                   88  EX-FC-ADDRESS-TR                 VALUE '5'.      00009570
                   88  EX-FC-GENERAL-INFO-TR            VALUE '6'.      00009580
                   88  EX-FC-AUTO-PROMPT-TR             VALUE '7'.      00009590
                   88  EX-FC-DENIAL-TR                  VALUE '8'.      00009600
                   88  EX-FC-INCURRED-CHG-TR            VALUE '9'.      00009610
                   88  EX-FC-FORM-CONTROL-TR            VALUE 'A'.      00009620
               16  EX-FC-RECORDED-DT               PIC XX.              00009630
               16  EX-FC-RECORDED-BY               PIC X(4).            00009640
               16  EX-FC-LAST-MAINT-HHMMSS         PIC S9(6)    COMP-3. 00009650
               16  FILLER                          PIC X(66).           00009651
               16  EX-FC-TRAILER-BODY              PIC X(165).          00009652
                                                                        00009670
      ******************************************************************00009680
       01  FILLER                    PIC X(15) VALUE '***************'. 00009690
       01  FILLER                    PIC X(15) VALUE 'CLAIM  ACTIVITY'. 00009700
       01  FILLER                    PIC X(15) VALUE '***************'. 00009710
                                                                        00009720
       01  WS-CLAIM-ACTIVITY-RECORD.                                    00009730
           03  CLAIM-WORK-RECORD.                                       00009740
               05  CW-RECORD-KEY.                                       00009750
                   07  CW-CLAIM-NUMBER         PIC X(10).               00009751
               05  CW-CLAIMANT                 PIC X(20).               00009780
               05  CW-CLAIMANT-DOB             PIC X(8).                00009781
               05  CW-CLAIM-STATUS             PIC X.                   00009800
               05  CW-ENTRY-DATE               PIC X(8).                00009801
               05  CW-REVISION-DATE            PIC X(8).                00009802
               05  CW-CLAIMANT-STATE-OF-RES    PIC XX.                  00009830
               05  CW-DATE-INCURRED            PIC X(6).                00009831
               05  CW-FIRST-NOTICE             PIC X(8).                00009832
               05  CW-DIAGNOSIS                PIC X(50).               00009860
               05  CW-AMOUNT-PAID              PIC S9(7)V99.            00009870
               05  CW-LAST-PAID                PIC X(8).                00009871
               05  FILLER                      PIC X(52).               00009890
               05  CW-CLAIM-TYPE               PIC X.                   00009900
               05  CW-CLAIM-ACTION             PIC X.                   00009910
               05  CW-ACTION-TIME              PIC X(6).                00009911
               05  CW-CLAIMANT-DOD             PIC X(8).                00009912
           03  POLICY-WORK-RECORD.                                      00009940
               05  PW-RECORD-KEY.                                       00009950
                   07  PW-CLAIM-NUMBER         PIC X(10).               00009951
                   07  PW-POLICY-NUMBER        PIC X(10).               00009970
               05  PW-CLAIMANT                 PIC X(20).               00009980
               05  PW-CLAIMANT-DOB             PIC X(8).                00009981
               05  PW-POLICY-STATUS            PIC X.                   00010000
               05  PW-POLICY-OWNER             PIC X(30).               00010010
               05  PW-PO-ADDR1                 PIC X(30).               00010020
               05  PW-PO-ADDR2                 PIC X(30).               00010030
               05  PW-PO-CITY                  PIC X(20).               00010040
               05  PW-PO-STATE                 PIC XX.                  00010050
               05  PW-PO-ZIP                   PIC 9(9).                00010060
               05  PW-PLAN-NUMBER              PIC X(6).                00010070
               05  PW-POLICY-TYPE              PIC XX.                  00010080
               05  PW-ISSUE-DATE               PIC X(8).                00010081
               05  PW-INDEMNITY                PIC S9(7)V99.            00010100
               05  CHAR-ZEROS-3                PIC 9(7).                00010110
               05  FILLER                      PIC X(20).               00010120
               05  PW-PAID-TO                  PIC X(8).                00010121
               05  FILLER                      PIC X.                   00010140
               05  PW-COLL-AGT-NO              PIC 9(7).                00010150
               05  PW-COLL-AGT-STATE           PIC XX.                  00010160
               05  FILLER                      PIC XX.                  00010170
               05  PW-COLL-AGT-CODE            PIC X(10).               00010171
               05  FILLER                      PIC X(102).              00010172
               05  CHAR-ZEROS-4                PIC 9(9).                00010200
               05  FILLER                      PIC X(50).               00010210
               05  PW-AMOUNT-PAID              PIC S9(7)V99.            00010220
               05  PW-LAST-PAID                PIC X(8).                00010221
               05  FILLER                      PIC X.                   00010240
               05  PW-PROOF-DATE               PIC X(8).                00010241
               05  PW-AUDITOR                  PIC XXX.                 00010260
               05  PW-POLICY-ACTION            PIC X.                   00010270
               05  PW-ACTION-TIME              PIC X(6).                00010271
               05  PW-REVISION-DATE            PIC X(8).                00010272
               05  PW-HOME-OFFICE-CATEGORY     PIC XXX.                 00010300
               05  PW-ANNUAL-STATE-CATEGORY    PIC XXX.                 00010310
               05  PW-CLAIM-TYPE               PIC X.                   00010320
               05  PW-OPER-COMPANY             PIC XX.                  00010330
               05  PW-BENEFIT-DURATION         PIC 9(3).                00010340
                                                                        00010350
            SKIP3                                                       00010360
           EJECT                                                        00010370
                                                                        00010380
      ******************************************************************00010390
       01  FILLER                    PIC X(15) VALUE '***************'. 00010400
       01  FILLER                    PIC X(15) VALUE 'DRAFT  ACTIVITY'. 00010410
       01  FILLER                    PIC X(15) VALUE '***************'. 00010420
                                                                        00010430
       01  WS-DRAFT-ACTIVITY-RECORD.                                    00010440
           03  DAR-RECORD-KEY.                                          00010450
               05  DAR-DRAFT-ACCOUNT       PIC XX.                      00010460
               05  DAR-DRAFT-NUMBER        PIC X(8).                    00010480
               05  DAR-DRAFT-STATUS        PIC X.                       00010490
           03  DAR-POLICY-NUMBER           PIC X(10).                   00010500
           03  DAR-CLAIM-NUMBER            PIC X(10).                   00010501
           03  DAR-AMOUNT-PAID             PIC 9(7)V99.                 00010520
           03  DAR-AUDITOR                 PIC XXX.                     00010530
           03  DAR-PART-FINAL              PIC X.                       00010540
           03  DAR-FROM-DATE               PIC X(8).                    00010541
           03  DAR-TO-DATE                 PIC X(8).                    00010542
           03  CHAR-ZEROS-5                PIC 9(3).                    00010570
           03  CHAR-ZEROS-6                PIC 9(9).                    00010580
           03  CHAR-ZEROS-7                PIC 9(9).                    00010590
           03  CHAR-ZEROS-8                PIC 9(9).                    00010600
           03  CHAR-ZEROS-9                PIC 9(9).                    00010610
           03  CHAR-ZEROS-10               PIC 9(9).                    00010620
DANA       03  DAR-NOTE-CODE               PIC 99.                      00010630
090804     03  DAR-NOTE-CODE-XX REDEFINES DAR-NOTE-CODE  
090804                                     PIC X(02).
           03  dar-aba-routing-number      pic x(10).
           03  dar-ach-account-number      pic x(20).
082317     03  dar-ach-sub-type            pic xx.
013017     03  dar-notes occurs 7          pic x(32).
013017*    03  FILLER                      PIC X(256).                  00010630
           03  DAR-PAYEE-ADDRESS.                                       00010640
               04  DAR-PAYEE                   PIC X(30).               00010650
               04  DAR-PAYEE-ADDR1             PIC X(30).               00010660
               04  DAR-PAYEE-ADDR2             PIC X(30).               00010670
               04  DAR-PAYEE-CITY              PIC X(20).               00010680
               04  DAR-PAYEE-STATE             PIC XX.                  00010690
           03  DAR-PAYEE-ZIP               PIC 9(9).                    00010700
           03  DAR-ENTERED-DATE            PIC X(8).                    00010701
           03  DAR-PRINT-FLAG              PIC X.                       00010702
           03  DAR-PROOF                   PIC X(8).                    00010703
           03  DAR-SSN-TIN.                                             00010704
               05  DAR-TAX-NO              PIC 9(09).                   00010705
               05  DAR-TAX-TYPE            PIC X(01).                   00010706
           03  DAR-EOB-AUDITOR             PIC X(04).                   00010707
           03  DAR-RETURN-DFT              PIC X(01).                   00010708
           03  DRAFT-CLAIM-WORK-RECORD.                                 00010750
               05  DCW-RECORD-KEY.                                      00010760
                   07  DCW-CLAIM-NUMBER        PIC X(10).               00010761
               05  DCW-CLAIMANT                PIC X(20).               00010790
               05  DCW-CLAIMANT-DOB            PIC X(8).                00010791
               05  DCW-CLAIM-STATUS            PIC X.                   00010810
               05  DCW-ENTRY-DATE              PIC X(8).                00010811
               05  DCW-REVISION-DATE           PIC X(8).                00010812
               05  DCW-CLAIMANT-STATE-OF-RES   PIC XX.                  00010840
               05  DCW-DATE-INCURRED           PIC X(6).                00010841
               05  DCW-FIRST-NOTICE            PIC X(8).                00010842
               05  DCW-DIAGNOSIS               PIC X(50).               00010870
               05  DCW-AMOUNT-PAID             PIC S9(7)V99.            00010880
               05  DCW-LAST-PAID               PIC X(8).                00010881
               05  FILLER                      PIC X(52).               00010900
               05  DCW-CLAIM-TYPE              PIC X.                   00010910
122205         05  DCW-CARRIER                 PIC X.                   00010920
               05  DCW-ACTION-TIME             PIC X(6).                00010921
CS1019         05  DCW-EXC-STATE               PIC X(2).                00010922
CS1019         05  DCW-EXC-BEN-TYPE            PIC X(2).                00010923
013017         05  DCW-ACH-PAYMENT             PIC X.
013017         05  FILLER                      PIC X(3).                00010924
           03  DRAFT-POLICY-WORK-RECORD.                                00010950
               05  DPW-RECORD-KEY.                                      00010960
                   07  DPW-CLAIM-NUMBER        PIC X(10).               00010961
                   07  DPW-POLICY-NUMBER       PIC X(10).               00010980
               05  DPW-CLAIMANT                PIC X(20).               00010990
               05  DPW-CLAIMANT-DOB            PIC X(8).                00010991
               05  DPW-POLICY-STATUS           PIC X.                   00011010
               05  DPW-PO-ADDRESS.                                      00011020
                   06  DPW-POLICY-OWNER            PIC X(30).           00011030
                   06  DPW-PO-ADDR1                PIC X(30).           00011040
                   06  DPW-PO-ADDR2                PIC X(30).           00011050
                   06  DPW-PO-CITY                 PIC X(20).           00011060
                   06  DPW-PO-STATE                PIC XX.              00011070
               05  DPW-PO-ZIP                  PIC 9(9).                00011080
               05  DPW-PLAN-NUMBER             PIC X(6).                00011090
               05  DPW-POLICY-TYPE             PIC XX.                  00011100
               05  DPW-ISSUE-DATE              PIC X(8).                00011101
               05  DPW-INDEMNITY               PIC S9(7)V99.            00011120
               05  CHAR-ZEROS-15               PIC 9(7).                00011130
               05  FILLER                      PIC X(20).               00011140
               05  DPW-PAID-TO                 PIC X(8).                00011141
               05  FILLER                      PIC X.                   00011160
               05  DPW-COLL-AGT-NO             PIC 9(7).                00011170
               05  DPW-COLL-AGT-STATE          PIC XX.                  00011180
               05  FILLER                      PIC XX.                  00011190
               05  DPW-COLL-AGT-CODE           PIC X(10).               00011191
               05  FILLER                      PIC X(102).              00011192
               05  CHAR-ZEROS-17               PIC 9(9).                00011220
               05  FILLER                      PIC X(50).               00011230
               05  DPW-AMOUNT-PAID             PIC S9(7)V99.            00011240
               05  DPW-LAST-PAID               PIC X(8).                00011241
               05  FILLER                      PIC X.                   00011260
               05  DPW-PROOF-DATE              PIC X(8).                00011261
               05  FILLER                      PIC X(3).                00011280
               05  DPW-POLICY-ACTION           PIC X.                   00011290
               05  DPW-ACTION-TIME             PIC X(6).                00011291
               05  DPW-REVISION-DATE           PIC X(8).                00011292
               05  DPW-HOME-OFFICE-CATEGORY    PIC XXX.                 00011320
               05  DPW-ANNUAL-STATE-CATEGORY   PIC XXX.                 00011330
               05  DPW-CLAIM-TYPE              PIC X.                   00011340
               05  DPW-OPER-COMPANY            PIC XX.                  00011350
               05  DPW-BENEFIT-DURATION        PIC 9(3).                00011360
                                                                        00011370
      ******************************************************************00011380
                                                                        00011390
       01  SUB1                          PIC S9(4) VALUE +0  COMP-3.    00011391
                                                                        00011410
       01  WS-CLAIM-TYPE-FA              PIC X.                         00011420
      *    88  EX-FA-AH-CLAIM               VALUE 'A'.                  00011430
      *    88  EX-FA-LIFE-CLAIM             VALUE 'L'.                  00011440
                                                                        00011450
       01  EXTRACT-EOF-SW            PIC X   VALUE 'N'.                 00011460
           88  EXTRACT-EOF                   VALUE 'Y'.                 00011470
           88  NOT-EXTRACT-EOF               VALUE 'N'.                 00011480
                                                                        00011490
       01  CORR-TRLR-FOUND-SW       PIC X   VALUE 'N'.                  00011500
           88  NO-CORR-TRLR-FOUND           VALUE 'N'.                  00011510
           88  CORR-TRLR-FOUND              VALUE 'Y'.                  00011520
                                                                        00011530
       01  WRITE-DRAFT-SW           PIC X   VALUE 'Y'.                  00011540
           88  DONT-WRITE-DRAFT             VALUE 'N'.                  00011550
           88  WRITE-DRAFT                  VALUE 'Y'.                  00011560
                                                                        00011570
       01  SET-UP-SW                PIC X   VALUE 'N'.                  00011580
           88  NO-SET-UP-NEEDED             VALUE 'N'.                  00011590
           88  SET-UP-NEEDED                VALUE 'Y'.                  00011600
                                                                        00011610
       01  PAYMENT-VOID-SW          PIC X   VALUE 'N'.                  00011620
           88  NO-PAYMENT-VOID-NEEDED       VALUE 'N'.                  00011630
           88  PAYMENT-VOID-NEEDED          VALUE 'Y'.                  00011640
                                                                        00011650
       01  DENIAL-DROP-SW           PIC X   VALUE 'N'.                  00011660
           88  NO-DENIAL-DROP-NEEDED        VALUE 'N'.                  00011670
           88  DENIAL-DROP-NEEDED           VALUE 'Y'.                  00011680
                                                                        00011690
       01  PAYEE-ADDR-SW            PIC X   VALUE 'Y'.                  00011700
           88  PAYEE-ADDR-NEEDED            VALUE 'Y'.                  00011710
           88  NO-PAYEE-ADDR-NEEDED         VALUE 'N'.                  00011720
                                                                        00011730
       01  NUMBER-OF-PAY-TRLRS      PIC 999  VALUE ZERO.                00011731
                                                                        00011750
       01  PC-STATUS.                                                   00011760
           03  PC-STAT-1 PIC X.                                         00011770
           03  PC-STAT-2 PIC X.                                         00011780
      ******************************************************************00011781
                                                                        00011782
       01  UNPAC-AMT.                                                   00011783
           05  AMT-9            PIC  9(11)      VALUE ZEROS.            00011784
           05  AMT-X REDEFINES AMT-9  PIC  X(11).                       00011785
                                                                        00011786
                                                                        00011790
       01  CF-STATUS.                                                   00011800
           03  CF-STAT-1 PIC X.                                         00011810
           03  CF-STAT-2 PIC X.                                         00011820
                                                                        00011830
       01  EX-STATUS.                                                   00011840
           03  EX-STAT-1 PIC X.                                         00011850
           03  EX-STAT-2 PIC X.                                         00011860
                                                                        00011870
       01  BENE-STATUS.                                                 00011880
           03  BENE-STAT-1 PIC X.                                       00011890
           03  BENE-STAT-2 PIC X.                                       00011900
                                                                        00011910
       01  GEN-INFO-TRLR.                                               00011926
           05  FILLER                  PIC X(107)  VALUE SPACES.        00011927
           05  DIAG-FLD                PIC X(50)   VALUE SPACES.        00011928
           05  FILLER                  PIC X(105)  VALUE SPACES.        00011929
                                                                        00011960
090815 01  WS-PAYEE-TYPE-CD.
090815     05  ws-payee-type           pic x.
090815     05  ws-payee-type-seq       pic x.
      *    88  INSURED-PAID           VALUE '1'.                        00011980
      *    88  BENEFICIARY-PAID       VALUE '2'.                        00011990
      *    88  ACCOUNT-PAID           VALUE '3'.                        00012000
      *    88  OTHER-1-PAID           VALUE '4'.                        00012010
      *    88  OTHER-2-PAID           VALUE '5'.                        00012020
                                                                        00012030
103002 01  WS-MISC.                                                     00012040
103002     05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    
103002     05  PGM-SUB                     PIC S9(4) COMP VALUE +310.
103002     05  WS-RETURN-CODE              PIC S999  COMP-3 VALUE +0.
       01  WS-CF-RECORD-TYPE                 PIC X    VALUE ZERO.       00012050
       01  WS-ZERO                PIC S9(3)   COMP-3  VALUE +0.         00012060
       01  ACCUMULATORS.                                                00012070
           05  CLAIM-ENTRIES-OUT      PIC S9(5)   COMP-3  VALUE +0.     00012080
           05  DRAFT-ENTRIES-OUT      PIC S9(5)   COMP-3  VALUE +0.     00012090
       01  WS-ORIGINAL-TERM       PIC S9(3)   COMP-3.                   00012100
                                                                        00012110
       01  WS-HEX-00           PIC XX               VALUE LOW-VALUES.   00012120
                                                                        00012130
       01  WS-FIVE             PIC S9(4)  COMP      VALUE +5.           00012140
       01  WS-FIVE-R REDEFINES WS-FIVE.                                 00012150
           03  FILLER          PIC X.                                   00012160
           03  WS-HEX-05       PIC X.                                   00012170
                                                                        00012180
       01  PROGRAM-CONSTANTS.                                           00012190
           05  WS-CONSTANT-I       PIC X       VALUE 'I'.               00012200
           05  WS-CONSTANT-D       PIC X       VALUE 'D'.               00012210
                                                                        00012220
       01  DATE-WORK-AREAS.                                             00012230
           05  WS-DATE-IN-AREA     PIC XX      VALUE SPACE.             00012240
           05  WS-BIN-PROC-DT      PIC XX      VALUE SPACE.             00012250
           05  WS-BIN-PREV-CYC-DT  PIC XX      VALUE SPACE.             00012260
           05  WS-BIN-CURR-CYC-DT  PIC XX      VALUE SPACE.             00012270
           05  WS-BIN-MAINT-DT     PIC XX.                              00012280
           05  WS-HOLD-CERT-EFF-DT PIC XX.                              00012290
           05  WS-HOLD-PAID-TO     PIC XX.                              00012300
           05  WS-HOLD-INCUR-DATE  PIC XX.                              00012310
                                                                        00012350
       01  WS-DATE-BACK-AREA            PIC 9(8)          VALUE ZERO.   00012351
       01  WS-DATE-BACK-AREA-R1 REDEFINES WS-DATE-BACK-AREA.            00012352
           05  WS-DATE-BACK-AREA-CC     PIC 9(2).                       00012353
           05  WS-DATE-BACK-AREA-YMD    PIC 9(6).                       00012354
           05  WS-DATE-BACK-AREA-YMD-R REDEFINES WS-DATE-BACK-AREA-YMD. 00012355
               10  WS-DATE-BACK-AREA-YY PIC 9(2).                       00012356
               10  WS-DATE-BACK-AREA-MM PIC 9(2).                       00012357
               10  WS-DATE-BACK-AREA-DD PIC 9(2).                       00012358
       01  WS-DATE-BACK-AREA-R2 REDEFINES WS-DATE-BACK-AREA.            00012359
           05  WS-DATE-BACK-AREA-YM     PIC 9(6).                       00012360
           05  WS-DATE-BACK-AREA-D      PIC 9(2).                       00012361
                                                                        00012362
       01  WS-DATE-WORK.                                                00012363
           05  WS-DATE-1       PIC S9(3)  COMP-3  VALUE +0.             00012364
           05  WS-DATE-2       PIC S9     COMP-3  VALUE +0.             00012365
               88  LEAP-YEAR                      VALUE +0.             00012366
                                                                        00012470
       01  WS-CITY-STATE-WORK.                                          00012520
           05  WS-CS-WORK.                                              00012530
               10  WS-CSW                  PICTURE X                    00012540
                   OCCURS 30 TIMES INDEXED BY CSW-INDEX                 00012550
                                              CSW-INDEX2.               00012560
       01  WS-CITY-FOUND.                                               00012570
           05  WS-CTY                 PICTURE X                         00012580
               OCCURS 20 TIMES INDEXED BY CTY-INDEX.                    00012590
                                                                        00012600
       01  WS-STATE-FOUND.                                              00012610
           05  WS-STATE-1      PIC X.                                   00012620
           05  WS-STATE-2      PIC X.                                   00012630
091808
091808 01  WS-STATE-WORK.
091808     05  WS-STATE-TEMP-NAME  PIC X(20).
091808     05  WS-STATE-TEMP-LEN   PIC 9(3).
091808     05  WS-SAVE-ST-IDX-BEG  PIC 9(3).
091808     05  WS-SAVE-ST-IDX-END  PIC 9(3).
091808
091808* STATE EDIT TABLE
091808 01  STATE-VALUES.                                    
091808     05  PIC X(22)  VALUE 'ALABAMA             AL'. 
091808     05  PIC X(22)  VALUE 'ALASKA              AK'. 
091808     05  PIC X(22)  VALUE 'AMERICAN SAMOA      AS'. 
091808     05  PIC X(22)  VALUE 'ARIZONA             AZ'. 
091808     05  PIC X(22)  VALUE 'ARKANSAS            AR'. 
091808     05  PIC X(22)  VALUE 'CALIFORNIA          CA'. 
091808     05  PIC X(22)  VALUE 'CANADA              CN'. 
091808     05  PIC X(22)  VALUE 'COLORADO            CO'. 
091808     05  PIC X(22)  VALUE 'CONNECTICUT         CT'. 
091808     05  PIC X(22)  VALUE 'DELAWARE            DE'. 
091808     05  PIC X(22)  VALUE 'DISTRICT OF COLUMBIADC'. 
091808     05  PIC X(22)  VALUE 'FLORIDA             FL'. 
091808     05  PIC X(22)  VALUE 'GEORGIA             GA'. 
091808     05  PIC X(22)  VALUE 'GUAM                GU'. 
091808     05  PIC X(22)  VALUE 'HAWAII              HI'. 
091808     05  PIC X(22)  VALUE 'IDAHO               ID'. 
091808     05  PIC X(22)  VALUE 'ILLINOIS            IL'. 
091808     05  PIC X(22)  VALUE 'INDIANA             IN'. 
091808     05  PIC X(22)  VALUE 'IOWA                IA'. 
091808     05  PIC X(22)  VALUE 'KANSAS              KS'. 
091808     05  PIC X(22)  VALUE 'KENTUCKY            KY'. 
091808     05  PIC X(22)  VALUE 'LOUISIANA           LA'. 
091808     05  PIC X(22)  VALUE 'MAINE               ME'. 
091808     05  PIC X(22)  VALUE 'MARYLAND            MD'. 
091808     05  PIC X(22)  VALUE 'MASSACHUSETTS       MA'. 
091808     05  PIC X(22)  VALUE 'MICHIGAN            MI'. 
091808     05  PIC X(22)  VALUE 'MINNESOTA           MN'. 
091808     05  PIC X(22)  VALUE 'MISSISSIPPI         MS'. 
091808     05  PIC X(22)  VALUE 'MISSOURI            MO'. 
091808     05  PIC X(22)  VALUE 'MONTANA             MT'. 
091808     05  PIC X(22)  VALUE 'NEBRASKA            NE'. 
091808     05  PIC X(22)  VALUE 'NEVADA              NV'. 
091808     05  PIC X(22)  VALUE 'NEW HAMPSHIRE       NH'. 
091808     05  PIC X(22)  VALUE 'NEW JERSEY          NJ'. 
091808     05  PIC X(22)  VALUE 'NEW MEXICO          NM'. 
091808     05  PIC X(22)  VALUE 'NEW YORK            NY'. 
091808     05  PIC X(22)  VALUE 'NORTH CAROLINA      NC'. 
091808     05  PIC X(22)  VALUE 'NORTH DAKOTA        ND'. 
091808     05  PIC X(22)  VALUE 'OHIO                OH'. 
091808     05  PIC X(22)  VALUE 'OKLAHOMA            OK'. 
091808     05  PIC X(22)  VALUE 'OREGON              OR'. 
091808     05  PIC X(22)  VALUE 'OTHER ALIEN         OT'. 
091808     05  PIC X(22)  VALUE 'PENNSYLVANIA        PA'. 
091808     05  PIC X(22)  VALUE 'PUERTO RICO         PR'. 
091808     05  PIC X(22)  VALUE 'RHODE ISLAND        RI'. 
091808     05  PIC X(22)  VALUE 'SOUTH CAROLINA      SC'. 
091808     05  PIC X(22)  VALUE 'SOUTH DAKOTA        SD'. 
091808     05  PIC X(22)  VALUE 'TENNESSEE           TN'. 
091808     05  PIC X(22)  VALUE 'TEXAS               TX'. 
091808     05  PIC X(22)  VALUE 'US VIRGIN ISLANDS   VI'. 
091808     05  PIC X(22)  VALUE 'UTAH                UT'. 
091808     05  PIC X(22)  VALUE 'VERMONT             VT'. 
091808     05  PIC X(22)  VALUE 'VIRGINIA            VA'. 
091808     05  PIC X(22)  VALUE 'WASHINGTON          WA'. 
091808     05  PIC X(22)  VALUE 'WEST VIRGINIA       WV'. 
091808     05  PIC X(22)  VALUE 'WISCONSIN           WI'. 
091808     05  PIC X(22)  VALUE 'WYOMING             WY'. 
091808
091808 01  FILLER REDEFINES STATE-VALUES.
091808     05  STATE-TABLE
091808             OCCURS 57 TIMES
091808             ASCENDING KEY IS ST-NAME
091808             INDEXED BY ST-INDEX.
091808         10  ST-NAME           PIC X(20).
091808         10  ST-STATE          PIC XX.
                                                                        00012640
       01  WS-CERT-TEST-AREA.                                           00012650
           05  WS-CERT-FIRST-2   PIC XX.                                00012660
           05  FILLER            PIC X(6).                              00012670
                                                                        00012680
       01  WS-COMPANY          PIC XXX     VALUE SPACE.                 00012690
                                                                        00012700
       01  WS-DRAFT-NUMBER     PIC X(7)    VALUE SPACE.                 00012710
       01  WS-DRAFT-NUMBER-REDEF REDEFINES WS-DRAFT-NUMBER.             00012720
BOA        05  WS-DNR-1        PIC X.                                   00012730
BOA        05  WS-DNR-2        PIC X(6).                                00012740
                                                                        00012750
       01  PROGRAM-CODES.                                               00012760
           05  WS-INDIV-GRP-CD     PIC X       VALUE SPACE.             00012770
           05  WS-BENEFIT-CD       PIC XX      VALUE ZEROS.             00012780
                                                                        00012790
       01  WS-PLAN-CODE-AREA.                                           00012800
           05  WS-PC-KEY-PT1               PIC X.                       00012810
           05  WS-PC-KEY-PT2               PIC XX.                      00012820
           05  WS-PC-KEY-PT3               PIC XXX.                     00012830
                                                                        00012840
       01  WS-BE-CONTROL-PRIMARY.                                       00012850
           05  WS-COMPANY-CD               PIC X.                       00012860
           05  WS-BE-B                     PIC X      VALUE 'B'.        00012861
           05  WS-BENEFICIARY-HOLD         PIC X(10).                   00012862
                                                                        00012880
           EJECT                                                        00012890
       01  FILLER                    PIC X(15) VALUE '***************'. 00012900
       01  FILLER                    PIC X(15) VALUE 'PROGRAM TABLES '. 00012910
       01  FILLER                    PIC X(15) VALUE '***************'. 00012920
                                                                        00012930
       01  PROGRAM-TABLES.                                              00012940
052804     05  PAYMENT-TRAILER-HOLD-TABLE     PIC X(2770).              00012941
           05  PAYMENT-TRAILER-TABLE REDEFINES                          00012960
                   PAYMENT-TRAILER-HOLD-TABLE                           00012970
                                       OCCURS 10 TIMES                  00012980
                                       INDEXED BY PT-INDEX              00012990
                                                  PT-INDEX2.            00013000
               10  PT-INSURED-NAME            PIC X(30).                00013001
               10  PT-TRAILER-TYPE            PIC X.                    00013010
               10  PT-RECORDED-DT             PIC XX.                   00013020
               10  PT-RECORDED-BY             PIC X(4).                 00013030
               10  PT-LAST-MAINT-HHMMSS       PIC S9(6)     COMP-3.     00013040
               10  FILLER                     PIC X(66).                00013041
               10  PT-PAYMENT-TYPE            PIC X.                    00013050
               10  PT-CLAIM-TYPE              PIC X.                    00013060
                   88 PT-AH-CLAIM                    VALUE 'A'.         00013070
121703             88 PT-GP-CLAIM                    VALUE 'G'.         00013070
122402             88 PT-IU-CLAIM                    VALUE 'I'.         00013070
052614             88 PT-FL-CLAIM                    VALUE 'F'.
022122             88 PT-BR-CLAIM                    VALUE 'B'.
022122             88 PT-HS-CLAIM                    VALUE 'H'.
100518             88 PT-OT-CLAIM                    VALUE 'O'.
                   88 PT-LIFE-CLAIM                  VALUE 'L'.         00013080
               10  PT-CLAIM-PREM-TYPE         PIC X.                    00013090
               10  PT-AMOUNT-PAID             PIC S9(7)V99  COMP-3.     00013100
               10  PT-CHECK-NO                PIC X(7).                 00013110
               10  PT-PAID-FROM-DT            PIC XX.                   00013120
               10  PT-PAID-THRU-DT            PIC XX.                   00013130
               10  PT-DAYS-IN-PERIOD          PIC S9(4)     COMP.       00013140
013017         10  PT-ACH-PAYMENT             PIC X.                    00013141
               10  PT-PAYEES-NAME             PIC X(30).                00013160
               10  PT-PAYMENT-ORIGIN          PIC X.                    00013170
               10  PT-CHECK-WRITTEN-DT        PIC XX.                   00013180
               10  PT-TO-BE-WRITTEN-DT        PIC XX.                   00013190
               10  PT-VOID-DATA.                                        00013200
                   15  PT-VOID-DT             PIC XX.                   00013210
                   15  PT-VOID-REASON         PIC X(30).                00013220
               10  PT-ADDL-RESERVE            PIC S9(5)V99  COMP-3.     00013230
               10  PT-EXPENSE-PER-PMT         PIC S9(5)V99  COMP-3.     00013240
               10  PT-CREDIT-INTERFACE.                                 00013250
                   15  PT-PMT-SELECT-DT       PIC XX.                   00013260
                   15  PT-PMT-ACCEPT-DT       PIC XX.                   00013270
                   15  PT-VOID-SELECT-DT      PIC XX.                   00013280
                   15  PT-VOID-ACCEPT-DT      PIC XX.                   00013290
               10  PT-CHECK-QUE-CONTROL       PIC S9(8)     COMP.       00013300
               10  PT-CHECK-QUE-SEQUENCE      PIC S9(4)     COMP.       00013310
               10  PT-FORCE-CONTROL           PIC X.                    00013320
               10  PT-PREV-LAST-PMT-DT        PIC XX.                   00013330
               10  PT-PREV-PAID-THRU-DT       PIC XX.                   00013340
               10  PT-PREV-LAST-PMT-AMT       PIC S9(7)V99  COMP-3.     00013350
               10  PT-ELIMINATION-DAYS        PIC S999      COMP-3.     00013360
               10  PT-DAILY-RATE              PIC S9(3)V99  COMP-3.     00013370
               10  PT-BENEFIT-TYPE            PIC X.                    00013380
               10  PT-EXPENSE-TYPE            PIC X.                    00013390
               10  PT-PAYMENT-APPROVAL-SW     PIC X.                    00013391
               10  PT-PAYEE-TYPE-CD.                                    00013392
                   15  PT-PAYEE-TYPE          PIC X.                    00013393
                   15  PT-PAYEE-SEQ           PIC X.                    00013394
               10  PT-CASH-PAYMENT            PIC X.                    00013395
               10  PT-GROUPED-PAYMENT         PIC X.                    00013396
               10  PT-PAYMENT-NOTE-SEQ-NO     PIC S9(4)     COMP.       00013397
               10  FILLER                     PIC X(24).                00013398
               10  PT-PAYMENT-LAST-MAINT-DT   PIC XX.                   00013399
               10  PT-PAYMENT-LAST-UPDATED-BY PIC X(4).                 00013400
052804         10  FILLER                     PIC X(05).
                                                                        00013401
                                                                        00013430
032012     05  ADDRESS-TRAILER-HOLD-TABLE     PIC X(5540).              00013431
           05  ADDRESS-TRAILER REDEFINES ADDRESS-TRAILER-HOLD-TABLE     00013450
032012                                    OCCURS 20 TIMES               00013460
                                          INDEXED BY AT-INDEX           00013470
                                                     AT-INDEX2.         00013480
090815         10  FILLER                  PIC X(106).                  00013481
090815         10  at-address-seq-no       pic 9.           
090815         10  at-address-seq redefines 
090815             at-address-seq-no       pic x.
               10  AT-ADDRESS-TYPE         PIC X.                       00013530
               10  AT-MAIL-TO-NAME         PIC X(30).                   00013540
               10  AT-ADDRESS-LINE-1       PIC X(30).                   00013550
               10  AT-ADDRESS-LINE-2       PIC X(30).                   00013560
051810         10  AT-CITY-STATE.
051810             15  AT-CITY             PIC X(28).
051810             15  AT-STATE            PIC XX.
               10  AT-ZIP-CODE             PIC X(9).                    00013571
               10  AT-ZIP-CODE-N                                        00013572
                                 REDEFINES                              00013573
                                           AT-ZIP-CODE PIC 9(9).        00013574
               10  AT-PHONE-NO             PIC 9(11)     COMP-3.        00013590
               10  FILLER                  PIC X(23).                   00013591
               10  AT-ADDRESS-LAST-MAINT-DT   PIC XX.                   00013592
               10  AT-ADDRESS-LAST-UPDATED-BY PIC X(4).                 00013593
052804         10  FILLER                  PIC X(05).
                                                                        00013594
                                                                        00013620
                                                                        00013630
                                       copy ELCDATE.
      *01  DATE-CONVERSION-DATA.                                        00013640
      *    12  DC-COMM-LENGTH              PIC S9(4) COMP  VALUE +100.  00013650
      *    12  DC-OPTION-CODE              PIC X           VALUE SPACES.00013660
      *        88  BIN-TO-GREG                VALUE ' '.                00013670
      *        88  ELAPSED-BETWEEN-BIN        VALUE '1'.                00013680
      *        88  EDIT-GREG-TO-BIN           VALUE '2'.                00013690
      *        88  YMD-GREG-TO-BIN            VALUE '3'.                00013700
      *        88  MDY-GREG-TO-BIN            VALUE '4'.                00013710
      *        88  JULIAN-TO-BIN              VALUE '5'.                00013720
      *        88  BIN-PLUS-ELAPSED           VALUE '6'.                00013730
      *    12  DC-ERROR-CODE               PIC X           VALUE SPACES.00013740
      *        88  NO-CONVERSION-ERROR        VALUE ' '.                00013750
      *        88  DATE-CONVERSION-ERROR      VALUE '1' '2' '3' '4' '5' 00013760
      *                                             '9'.                00013770
      *        88  DATE-IS-ZERO               VALUE '1'.                00013780
      *        88  DATE-IS-NON-NUMERIC        VALUE '2'.                00013790
      *        88  DATE-IS-INVALID            VALUE '3'.                00013800
      *        88  DATE1-GREATER-DATE2        VALUE '4'.                00013810
      *        88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.                00013820
      *        88  DATE-INVALID-OPTION        VALUE '9'.                00013830
      *    12  DC-END-OF-MONTH             PIC X     VALUE SPACES.      00013840
      *        88  CALCULATE-END-OF-MONTH     VALUE '1'.                00013850
      *    12  FILLER                      PIC X(2)  VALUE SPACES.      00013860
      *    12  DC-CONVERSION-DATES.                                     00013870
      *        16  DC-BIN-DATE-1           PIC XX    VALUE LOW-VALUES.  00013880
      *        16  DC-BIN-DATE-2           PIC XX    VALUE LOW-VALUES.  00013890
      *        16  DC-GREG-DATE-1-EDIT     PIC X(8)  VALUE '01/01/00'.  00013900
      *        16  DC-GREG-DATE-2-EDIT     PIC X(8)  VALUE '01/01/00'.  00013910
      *        16  DC-GREG-DATE-1-YMD      PIC 9(6)  VALUE 010100.      00013920
      *        16  DC-GREG-DATE-1-MDY      PIC 9(6)  VALUE 010100.      00013930
      *        16  DC-GREG-DATE-1-ALPHA    PIC X(18) VALUE SPACES.      00013940
      *        16  DC-ELAPSED-MONTHS       PIC S9(4) VALUE ZERO COMP.   00013950
      *        16  DC-ODD-DAYS-OVER        PIC S9(4) VALUE ZERO COMP.   00013960
      *        16  DC-ELAPSED-DAYS         PIC S9(4) VALUE ZERO COMP.   00013970
      *        16  DC-JULIAN-YYDDD         PIC 9(5)  VALUE ZERO.        00013980
      *        16  DC-DAYS-IN-MONTH        PIC S9(3) VALUE ZERO COMP-3. 00013990
      *        16  DC-DAY-OF-WEEK          PIC S9    VALUE ZERO COMP-3. 00014000
      *        16  DC-DAY-OF-WEEK2         PIC S9    VALUE ZERO COMP-3. 00014010
      *    12  FILLER                      PIC X(28) VALUE SPACES.      00014020
                                                                        00014030
       01  FILLER                    PIC X(15) VALUE '***************'. 00014040
       01  FILLER                    PIC X(15) VALUE 'PAYMENT TRAILER'. 00014050
       01  FILLER                    PIC X(15) VALUE '***************'. 00014060
                                                                        00014070
       01  WS-FC-PAYMENT-TR.                                            00014080
               16  WS-FC-PAYMENT-TYPE      PIC X.                       00014090
                   88  PARTIAL-PAYMENT        VALUE '1'.                00014100
                   88  FINAL-PAYMENT          VALUE '2'.                00014110
                   88  LUMP-SUM-PAYMENT       VALUE '3'.                00014120
                   88  ADDITIONAL-PAYMENT     VALUE '4'.                00014130
                   88  CHARGEABLE-EXPENSE     VALUE '5'.                00014140
                   88  NON-CHARGEABLE-EXPENSE VALUE '6'.                00014150
               16  WS-FC-CLAIM-TYPE        PIC X.                       00014160
                   88  PAID-FOR-AH            VALUE 'A'.                00014170
121703             88  PAID-FOR-GP            VALUE 'G'.                00014170
122402             88  PAID-FOR-IU            VALUE 'I'.                00014170
052614             88  PAID-FOR-FL            VALUE 'F'.
022122             88  PAID-FOR-BR            VALUE 'B'.
022122             88  PAID-FOR-HS            VALUE 'H'.
100518             88  PAID-FOR-OT            VALUE 'O'.
                   88  PAID-FOR-LIFE          VALUE 'L'.                00014180
               16  WS-FC-CLAIM-PREM-TYPE   PIC X.                       00014190
                   88  WS-FC-SINGLE-PREMIUM   VALUE '1'.                00014200
                   88  WS-FC-O-B-COVERAGE     VALUE '2'.                00014210
                   88  WS-FC-OPEN-END-COVERAGE VALUE '3'.               00014220
               16  WS-FC-AMOUNT-PAID       PIC S9(7)V99  COMP-3.        00014230
               16  WS-FC-CHECK-NO          PIC X(7).                    00014240
               16  WS-FC-PAID-FROM-DT      PIC XX.                      00014250
               16  WS-FC-PAID-THRU-DT      PIC XX.                      00014260
               16  WS-FC-DAYS-IN-PERIOD    PIC S9(4)     COMP.          00014270
013017         16  WS-FC-ACH-PAYMENT       PIC X.                       00014271
               16  WS-FC-PAYEES-NAME       PIC X(30).                   00014340
               16  WS-FC-PAYMENT-ORIGIN    PIC X.                       00014350
                   88  ONLINE-MANUAL-PMT      VALUE '1'.                00014360
                   88  ONLINE-AUTO-PMT        VALUE '2'.                00014370
                   88  OFFLINE-PMT            VALUE '3'.                00014380
               16  WS-FC-CHECK-WRITTEN-DT  PIC XX.                      00014390
               16  WS-FC-TO-BE-WRITTEN-DT  PIC XX.                      00014400
               16  WS-FC-VOID-DATA.                                     00014410
                   20  WS-FC-VOID-DT       PIC XX.                      00014420
                   20  WS-FC-VOID-REASON   PIC X(30).                   00014430
               16  WS-FC-ADDL-RESERVE      PIC S9(5)V99  COMP-3.        00014440
               16  WS-FC-EXPENSE-PER-PMT   PIC S9(5)V99  COMP-3.        00014450
               16  WS-FC-CREDIT-INTERFACE.                              00014460
                   20  WS-FC-PMT-SELECT-DT PIC XX.                      00014470
                       88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.       00014480
                   20  WS-FC-PMT-ACCEPT-DT PIC XX.                      00014490
                       88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.       00014500
                   20  WS-FC-VOID-SELECT-DT PIC XX.                     00014510
                       88  VOID-NOT-SELECTED     VALUE LOW-VALUE.       00014520
                   20  WS-FC-VOID-ACCEPT-DT PIC XX.                     00014530
                       88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.       00014540
               16  WS-FC-CHECK-QUE-CONTROL PIC S9(8)     COMP.          00014550
                       88  PAYMENT-NOT-QUEUED    VALUE ZERO.            00014560
                       88  CONVERSION-PAYMENT    VALUE +99999999.       00014570
               16  WS-FC-CHECK-QUE-SEQUENCE PIC S9(4)    COMP.          00014580
               16  WS-FC-FORCE-CONTROL     PIC X.                       00014590
                   88  PAYMENT-WAS-FORCED        VALUE '1'.             00014600
               16  WS-FC-PREV-LAST-PMT-DT  PIC XX.                      00014610
               16  WS-FC-PREV-PAID-THRU-DT PIC XX.                      00014620
               16  WS-FC-PREV-LAST-PMT-AMT PIC S9(7)V99  COMP-3.        00014630
               16  WS-FC-ELIMINATION-DAYS  PIC S999      COMP-3.        00014640
               16  WS-FC-DAILY-RATE        PIC S9(3)V99  COMP-3.        00014650
               16  WS-FC-BENEFIT-TYPE      PIC X.                       00014660
               16  WS-FC-EXPENSE-TYPE      PIC X.                       00014670
               16  WS-FC-PAYMENT-APPROVAL-SW  PIC X.                    00014671
090815         16  WS-FC-PAYEE-TYPE-CD.                                 00014672
090815             20  WS-FC-PAYEE-TYPE           PIC X(01).            00014673
090815                 88  INSURED-PAID          VALUE 'I'.             00014674
090815                 88  BENEFICIARY-PAID      VALUE 'B'.             00014675
090815                 88  ACCOUNT-PAID          VALUE 'A'.             00014676
090815                 88  OTHER-1-PAID          VALUE 'O'.             00014677
090815                 88  OTHER-2-PAID          VALUE 'Q'.             00014678
090815                 88  DOCTOR-PAID           VALUE 'P'.             00014679
090815             20  WS-FC-PAYEE-SEQ            PIC X(01).            00014680
               16  WS-CASH-PAYMENT             PIC X(01).               00014681
               16  WS-GROUPED-PAYMENT          PIC X(01).               00014682
               16  WS-PAYMENT-NOTE-SEQ-NO      PIC S9(04)  COMP.        00014683
               16  FILLER                      PIC X(24).               00014684
               16  WS-FC-PAYMENT-LAST-MAINT-DT PIC XX.                  00014685
               16  WS-FC-PAYMENT-LAST-UPDATED-BY PIC X(4).              00014686
                                                                        00014700
       01  FILLER                    PIC X(15) VALUE '***************'. 00014710
       01  FILLER                    PIC X(15) VALUE 'CORRESP TRAILER'. 00014720
       01  FILLER                    PIC X(15) VALUE '***************'. 00014730
                                                                        00014740
       01  WS-FC-CORRESPONDENCE-TR.                                     00014750
               16  WS-FC-LETTER-SENT-DT    PIC XX.                      00014800
               16  WS-FC-RECEIPT-FOLLOW-UP PIC XX.                      00014810
               16  WS-FC-AUTO-RE-SEND-DT   PIC XX.                      00014820
               16  WS-FC-LETTER-ANSWERED-DT PIC XX.                     00014830
               16  WS-FC-LETTER-ARCHIVE-NO PIC S9(8)     COMP.          00014840
               16  WS-FC-LETTER-ORIGIN     PIC X.                       00014850
                   88  ONLINE-CREATION        VALUE '1'.                00014860
                   88  OFFLINE-CREATION       VALUE '2'.                00014870
               16  WS-FC-STD-LETTER-FORM   PIC X(4).                    00014880
               16  WS-FC-REASON-TEXT       PIC X(70).                   00014890
               16  WS-FC-ADDRESS-REC-SEQ-NO PIC S9(4)    COMP.          00014900
               16  WS-FC-ADDRESEE-TYPE     PIC X.                       00014910
                   88  INSURED-ADDRESEE       VALUE '1'.                00014920
                   88  BENEFICIARY-ADDRESEE   VALUE '2'.                00014930
                   88  ACCOUNT-ADDRESEE       VALUE '3'.                00014940
                   88  PHYSICIAN-ADDRESEE     VALUE '4'.                00014950
                   88  EMPLOYER-ADDRESEE      VALUE '5'.                00014960
                   88  OTHER-ADDRESEE-1       VALUE '6'.                00014970
                   88  OTHER-ADDRESEE-2       VALUE '7'.                00014980
               16  WS-FC-ADDRESSEE-NAME    PIC X(30).                   00014990
               16  WS-FC-INITIAL-PRINT-DATE PIC XX.                     00015000
               16  WS-FC-RESEND-PRINT-DATE PIC XX.                      00015010
               16  WS-FC-CORR-SOL-UNSOL       PIC X.                    00015011
               16  WS-FC-LETTER-PURGED-DT     PIC XX.                   00015012
               16  FILLER                  PIC X(32).                   00015013
               16  WS-FC-CSO-RETENTION.                                 00015014
                   20  FILLER              PIC X(27).                   00015015
                   20  WS-FC-CSO-LETTER-STATUS PIC X.                   00015016
                       88  WS-CSO-LETTER-ONLINE   VALUE '1'.            00015017
                       88  WS-CSO-LETTER-PURGED   VALUE '2'.            00015018
                       88  WS-CSO-LETTER-RELOADED VALUE '3'.            00015019
                   20  WS-FC-CSO-LETTER-PURGE-DATE  PIC XX.             00015020
                   20  WS-FC-CSO-LETTER-RELOAD-DATE PIC XX.             00015021
               16  WS-FC-CORR-LAST-MAINT-DT   PIC XX.                   00015022
               16  WS-FC-CORR-LAST-UPDATED-BY PIC XXXX.                 00015023
                                                                        00015040
       01  FILLER                    PIC X(15) VALUE '***************'. 00015050
       01  FILLER                    PIC X(15) VALUE 'ADDRESS TRAILER'. 00015060
       01  FILLER                    PIC X(15) VALUE '***************'. 00015070
                                                                        00015080
       01  WS-FC-ADDRESS-TR.                                            00015090
               16  WS-FC-ADDRESS-TYPE      PIC X.                       00015100
                   88  INSURED-ADDRESS        VALUE '1'.                00015110
                   88  BENEFICIARY-ADDRESS    VALUE '2'.                00015120
                   88  ACCOUNT-ADDRESS        VALUE '3'.                00015130
                   88  PHYSICIAN-ADDRESS      VALUE '4'.                00015140
                   88  EMPLOYER-ADDRESS       VALUE '5'.                00015150
                   88  OTHER-ADDRESS-1        VALUE '6'.                00015160
                   88  OTHER-ADDRESS-2        VALUE '7'.                00015170
               16  WS-FC-MAIL-TO-NAME      PIC X(30).                   00015180
               16  WS-FC-ADDRESS-LINE-1    PIC X(30).                   00015190
               16  WS-FC-ADDRESS-LINE-2    PIC X(30).                   00015200
               16  WS-FC-CITY-STATE        PIC X(30).                   00015210
               16  WS-FC-ZIP-CODE          PIC 9(9)      COMP-3.        00015220
               16  WS-FC-PHONE-NO          PIC 9(11)     COMP-3.        00015230
               16  FILLER                  PIC X(23).                   00015231
               16  WS-FC-ADDRESS-LAST-MAINT-DT   PIC XX.                00015232
               16  WS-FC-ADDRESS-LAST-UPDATED-BY PIC XXXX.              00015233
           EJECT                                                        00015250

103002     COPY ELCDTECX.                                               
103002     COPY ELCDTEVR.                                               

       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       LINKAGE SECTION.
013017 01  var                         pic x(30).


      **************************************************************    00015270
      *                                                            *    00015280
      *  THIS PROGRAM WAS WRITTEN TO TAKE THE DAILY EXTRACTS FROM  *    00015290
      *  EL310 IN CLDAILY AND FORMAT TWO OUTPUTS:  A DRAFT ENTRY   *    00015291
      *  AND A CLAIM ENTRY.                                        *    00015310
      *                                                            *    00015320
      *  THE INPUT TO THIS PROGRAM MUST BE SORTED INTO EXTRACTS    *    00015330
      *  FA FB AND FC THESE ARE THE ONLY EXTRACTS USED IN THE      *    00015340
      *  ENTIRE PROGRAM.  THE PROGRAM FIRST DETERMINES WHAT        *    00015350
      *  OUTPUTS NEED TO BE PROCESSED. THERE CAN BE MORE THAN ONE  *    00015360
      *  OUTPUT PER INPUT. AFTER THE DETERMINATION HAS BEEN MADE,  *    00015370
      *  THE FA EXTRACTS ARE PROCESSED FIRST, THEN THE FB EXTR.    *    00015380
      *  MUST COME NEXT.  THE FCS ARE TRAILERS--- THE ADDRESS AND  *    00015390
      *  PAYMENT TRAILERS ARE PUT INTO TABLES, WE ONLY NEED ONE    *    00015400
      *  CORRESPONDENCE TRAILER FOR THE OUTPUTS.                   *    00015410
      *                                                            *    00015420
      **************************************************************    00015430
                                                                        00015440
       PROCEDURE DIVISION.                                              00015450
                                                                        00015460
       A000-MAINLINE.

           display ' Begin Program LGCIFCE  '

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

103002*************************************************************     
103002                                 COPY ELCDTERX.                   
103002*************************************************************     
           PERFORM B000-OPEN-FILES THRU B000-EXIT.                      00015480
           PERFORM B010-LOAD-DATE  THRU B010-EXIT.                      00015490
           PERFORM B020-PROCESSING THRU B020-EXIT                       00015500
               UNTIL EXTRACT-EOF.                                       00015510
           PERFORM B030-CLOSE-FILES THRU B030-EXIT.
           perform S050-finish-up      thru s050-exit
           GOBACK.                                                      00015530
       A000-EXIT.                                                       00015540
           EXIT.                                                        00015550
                                                                        00015560
       B000-OPEN-FILES.                                                 00015570

           OPEN INPUT CYCLE-DATES.                                      00015580

           MOVE ZEROS TO CYCLE-DATE-REC.                                00015590
           READ CYCLE-DATES INTO CYCLE-DATE-REC                         00015600
              AT END                                                    00015610
                 DISPLAY                                                00015620
                 'NO REQUIRED CYCLE-DATE-RANGE CARD - CHECK JOB JCL'    00015630
                    UPON CONSOLE                                        00015640
                     GO TO ABEND-PGM.                                   00015650
                                                                        00015660
           IF CARD-ID EQUAL '*ABEND THE RUN* '                          00015670
                 DISPLAY                                                00015680
                 'NO REQUIRED CYCLE-DATE-RANGE CARD - CHECK JOB JCL'    00015690
                         UPON CONSOLE                                   00015700
                                                                        00015710
                 DISPLAY                                                00015720
                 'NO REQUIRED CYCLE-DATE-RANGE CARD - CHECK JOB JCL'    00015730
                     GO TO ABEND-PGM.                                   00015740
                                                                        00015750
                                                                        00015751
           CLOSE CYCLE-DATES.                                           00015760
                                                                        00015770
           OPEN INPUT EXTRACT-FILE                                      00015780
                      ELBENE                                            00015781
                      ELCNTL.                                           00015782

           IF BENE-STATUS NOT = '00'                                    00015880
               DISPLAY 'ERROR--OPEN-ELBENE FILE --CODE => ' BENE-STATUS 00015881
               GO TO ABEND-PGM.                                         00015900

           IF CF-STATUS NOT = '00'                                      00015910
               DISPLAY 'ERROR--OPEN-ELCNTL FILE --CODE => ' CF-STATUS   00015911
               GO TO ABEND-PGM.                                         00015930

           OPEN OUTPUT LG-CLM-ACT                                       00015940
                       LG-DFT-ACT.                                      00015950
       B000-EXIT.                                                       00015960
           EXIT.                                                        00015970
                                                                        00015980
       B010-LOAD-DATE.                                                  00015990
                                                                        00016000
12/93      PERFORM B011-CALC-DATE THRU B011-EXIT.                       00016001
                                                                        00016010
           DISPLAY 'PREV-CYCLE DATE IN => ' PREV-CYC-DATE.              00016020
           MOVE PREV-CYC-DATE  TO WS-TEMP-DATE.                         00016021
           MOVE WS-TEMP-MM     TO WS-MDY-MM.                            00016022
           MOVE WS-TEMP-DD     TO WS-MDY-DD.                            00016023
           MOVE WS-TEMP-YY     TO WS-MDY-YY.                            00016024
           MOVE WS-MMDDYY      TO DC-GREG-DATE-1-MDY.                   00016025
           MOVE '4' TO DC-OPTION-CODE.                                  00016040
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   00016050
           IF NO-CONVERSION-ERROR                                       00016060
               MOVE DC-BIN-DATE-1   TO WS-BIN-PREV-CYC-DT               00016070
           ELSE                                                         00016080
               DISPLAY 'PREV-CYC-DATE CONVERSION ERROR DATE-CODE = '    00016090
                      DC-ERROR-CODE                                     00016100
               GO TO ABEND-PGM.                                         00016110
                                                                        00016120
           DISPLAY 'CURR-CYCLE DATE IN => ' CURR-CYC-DATE.              00016130
           MOVE CURR-CYC-DATE  TO WS-TEMP-DATE.                         00016131
           MOVE WS-TEMP-MM     TO WS-MDY-MM.                            00016132
           MOVE WS-TEMP-DD     TO WS-MDY-DD.                            00016133
           MOVE WS-TEMP-YY     TO WS-MDY-YY.                            00016134
           MOVE WS-MMDDYY      TO DC-GREG-DATE-1-MDY.                   00016135
           MOVE '4' TO DC-OPTION-CODE.                                  00016150
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   00016160
           IF NO-CONVERSION-ERROR                                       00016170
               MOVE DC-BIN-DATE-1   TO WS-BIN-CURR-CYC-DT               00016180
           ELSE                                                         00016190
               DISPLAY 'CURR-CYC-DATE CONVERSION ERROR DATE-CODE = '    00016200
                      DC-ERROR-CODE                                     00016210
               GO TO ABEND-PGM.                                         00016220
                                                                        00016230
       B010-EXIT.                                                       00016240
           EXIT.                                                        00016250
                                                                        00016251
                                                                        00016252
                                                                        00016253
12/93  B011-CALC-DATE.                                                  00016254
      ***---------------------------------------------------------------00016255
      ***  FORCE THE PROGRAM TO USE THE LAST DAY OF THE PREVIOUS        00016256
      ***  MONTH AS THE PREVIOUS CYCLE DATE IF THIS IS THE FIRST        00016257
      ***  RUN OF A NEW MONTH.                                          00016258
      ***---------------------------------------------------------------00016259
           IF P-C-MO = C-C-MO                                           00016260
               GO TO B011-EXIT.                                         00016261
                                                                        00016262
           IF P-C-MO = (01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12)         00016263
               MOVE 31 TO P-C-DA                                        00016264
               GO TO B011-EXIT.                                         00016265
                                                                        00016266
           IF P-C-MO = (04 OR 06 OR 09 OR 11)                           00016267
               MOVE 30 TO P-C-DA                                        00016268
               GO TO B011-EXIT.                                         00016269
                                                                        00016270
           DIVIDE P-C-YR BY 4 GIVING WS-DATE-1 REMAINDER WS-DATE-2.     00016271
           IF LEAP-YEAR                                                 00016272
               MOVE 29 TO P-C-DA                                        00016273
           ELSE                                                         00016274
               MOVE 28 TO P-C-DA.                                       00016275
                                                                        00016276
12/93  B011-EXIT.                                                       00016277
           EXIT.                                                        00016278
                                                                        00016279
                                                                        00016280
       B020-PROCESSING.                                                 00016281
                                                                        00016282
           PERFORM C030-PRE-INIT THRU C030-EXIT.                        00016290
                                                                        00016300
           PERFORM C000-READ-EX-F-TAPE THRU C000-EXIT                   00016310
               UNTIL IN-RECORD-TYPE = 'A' OR EXTRACT-EOF.               00016320
           IF EXTRACT-EOF                                               00016330
               GO TO B020-EXIT.                                         00016340
      * MOVES FA TO WORKING STORAGE HOLD AREA                           00016350
           MOVE SPACES  TO  GEN-INFO-TRLR.                              00016351
           MOVE IN-WORK-DATA   TO WS-WORK-DATA.                         00016360
           MOVE IN-DATA-AREA   TO EX-EXTRACT-F-RECORD-A.                00016370
                                                                        00016380
        B020-CK-DENIAL.                                                 00016390
                                                                        00016400
           IF   EX-FA-LAST-CLOSE-REASON = '2' OR = '3' OR = '4'         00016410
                NEXT SENTENCE                                           00016420
              ELSE                                                      00016430
                GO TO B020-CK-CLAIM.                                    00016440
                                                                        00016450
           IF EX-FA-LAST-CLOSE-DT GREATER THAN WS-BIN-PREV-CYC-DT       00016460
                NEXT SENTENCE                                           00016470
              ELSE                                                      00016480
                GO TO B020-CK-CLAIM.                                    00016490
                                                                        00016500
           IF EX-FA-LAST-CLOSE-DT GREATER THAN WS-BIN-CURR-CYC-DT       00016510
                GO TO B020-CK-CLAIM.                                    00016520
                                                                        00016530
      * DENIAL--DROP MADE                                               00016540
           MOVE 'Y' TO DENIAL-DROP-SW.                                  00016550
                                                                        00016560
        B020-CK-CLAIM.                                                  00016570
                                                                        00016580
                                                                        00016590
           IF EX-FA-FILE-ESTABLISH-DT GREATER THAN WS-BIN-PREV-CYC-DT   00016600
                NEXT SENTENCE                                           00016610
              ELSE                                                      00016620
                GO TO B020-CK-VOIDS.                                    00016630
                                                                        00016640
           IF EX-FA-FILE-ESTABLISH-DT GREATER THAN WS-BIN-CURR-CYC-DT   00016650
                GO TO B020-CK-VOIDS.                                    00016660
                                                                        00016670
      * CLAIM--SETUP'                                                   00016680
               MOVE 'Y' TO SET-UP-SW.                                   00016690
                                                                        00016700
        B020-CK-VOIDS.                                                  00016710
                                                                        00016720
           MOVE 'N' TO PAYMENT-VOID-SW.                                 00016730
           PERFORM C002-CHECK-FOR-VOIDS THRU C002-EXIT.                 00016740
      * C002 BRINGS IN THE FB AND FC'S TO WORKING STORAGE AND CHECKS    00016750
      * TO SEE IF ANY VOIDS OR PAYMENTS HAVE BEEN MADE.                 00016760
                                                                        00016770
           IF SET-UP-NEEDED OR DENIAL-DROP-NEEDED OR                    00016780
               PAYMENT-VOID-NEEDED                                      00016790
                   PERFORM C005-PROCESS THRU C005-EXIT.                 00016800
                                                                        00016810
       B020-EXIT.                                                       00016820
           EXIT.                                                        00016830
                                                                        00016840
       C000-READ-EX-F-TAPE.                                             00016850
           READ EXTRACT-FILE                                            00016860
               AT END                                                   00016870
                   MOVE 'Y' TO EXTRACT-EOF-SW.                          00016880
       C000-EXIT.                                                       00016940
           EXIT.                                                        00016950
                                                                        00016960
       C002-CHECK-FOR-VOIDS.                                            00016970
      ****************************************************************  00016980
      *                                                              *  00016990
      *   THIS MODULE BRINGS IN THE FB AND FC'S FOR A CERTAIN        *  00017000
      *   RECORD AND MOVES THEM INTO WORKING STORAGE AREAS.  IT      *  00017010
      *   ALSO DETERMINES IF THERE ARE ANY VOIDS IN THE PAYMENT      *  00017020
      *   TRAILERS WHILE BUILDING THE PAYMENT TABLE.                 *  00017030
      *                                                              *  00017040
      ****************************************************************  00017050
                                                                        00017060
      * AN FB EXTRACT MUST COME NEXT IN THE PROCESSING FLOW             00017070
           PERFORM C000-READ-EX-F-TAPE THRU C000-EXIT.                  00017080
           IF IN-RECORD-TYPE = 'B'                                      00017090
               MOVE IN-DATA-AREA TO EX-EXTRACT-F-RECORD-B               00017100
           ELSE                                                         00017110
               DISPLAY '*** NO EXTRACT FB OR NOT IN PROPER ORDER ***'   00017120
                   UPON CONSOLE                                         00017130
               GO TO ABEND-PGM.                                         00017140
           SET PT-INDEX TO 1.                                           00017150
           SET AT-INDEX TO 1.                                           00017160
      * NUMBER-OF-PAY-TRLRS = THE # OF PYMNT-VOID TRLRS IN THE TABLE    00017170
           MOVE ZEROS TO NUMBER-OF-PAY-TRLRS.                           00017180
           PERFORM D060-BUILD-FC THRU D060-EXIT                         00017190
               UNTIL IN-RECORD-TYPE = 'A' OR EXTRACT-EOF.               00017200
      *  PERFORM PROCESSING OF THE FC TRLRS UNTIL WE COME ACROSS A      00017210
      *  A NEW SET OF FA FB AND FCS.                                    00017220
                                                                        00017230
       C002-EXIT.                                                       00017240
           EXIT.                                                        00017250
                                                                        00017260
       C005-PROCESS.                                                    00017270
                                                                        00017280
           IF SET-UP-NEEDED                                             00017290
               PERFORM C020-CLAIM-ACTIVITY-ENTRY THRU C020-EXIT         00017300
               MOVE 'N' TO SET-UP-SW.                                   00017310
                                                                        00017320
           IF DENIAL-DROP-NEEDED                                        00017330
               PERFORM C020-CLAIM-ACTIVITY-ENTRY THRU C020-EXIT         00017340
               MOVE 'N' TO DENIAL-DROP-SW.                              00017350
                                                                        00017360
           IF PAYMENT-VOID-NEEDED                                       00017370
               PERFORM C010-DRAFT-ACTIVITY-ENTRY THRU C010-EXIT         00017380
               MOVE 'N' TO PAYMENT-VOID-SW.                             00017390
                                                                        00017400
       C005-EXIT.                                                       00017410
           EXIT.                                                        00017420
                                                                        00017430
       C010-DRAFT-ACTIVITY-ENTRY.                                       00017440
      ****************************************************************  00017450
      *                                                              *  00017460
      * THIS MODULE PERFORMS THE MODULES THAT PROCESS THE FA,FC      *  00017470
      * RECORDS.  NOTE THAT THE FB RECORDS ARE PROCESSED WITHIN      *  00017480
      * THE FC'S, BECAUSE THE CLAIM-TYPE FIELD IN THE FC IS USED     *  00017490
      * TO DETERMINE WHAT FIELDS IN THE FB ARE TO BE MOVED.          *  00017500
      *                                                              *  00017510
      ****************************************************************  00017520
                                                                        00017530
           PERFORM D000-DRAFT-PROCESS-FA THRU D000-EXIT.                00017540
                                                                        00017550
           IF CORR-TRLR-FOUND                                           00017560
               PERFORM E020-DRAFT-CORR-TRLR THRU E020-EXIT              00017570
           ELSE                                                         00017580
               MOVE ZEROS TO DAR-PROOF.                                 00017590
                                                                        00017600
           SET PT-INDEX2 TO 1.                                          00017610
                                                                        00017620
           PERFORM D020-DRAFT-PROCESS-PAYMENTS THRU D020-EXIT           00017630
               UNTIL NUMBER-OF-PAY-TRLRS = 0.                           00017640
                                                                        00017650
       C010-EXIT.                                                       00017660
           EXIT.                                                        00017670
                                                                        00017680
       C020-CLAIM-ACTIVITY-ENTRY.                                       00017690

           PERFORM D030-CLAIM-PROCESS-FA THRU D030-EXIT.                00017700
           PERFORM D040-CLAIM-PROCESS-FB THRU D040-EXIT.                00017710

      * IF AT-INDEX = 1 THAT MEANS THERE ARE NO ADDRESSES IN THE TABLE  00017720
           IF AT-INDEX > 1                                              00017730
               SET AT-INDEX2 TO 1                                       00017740
               PERFORM D050-CLAIM-PROCESS-ADDR THRU D050-EXIT.          00017750
                                                                        00017760
           IF CORR-TRLR-FOUND                                           00017770
               PERFORM E050-CLAIM-CORR-TRLR THRU E050-EXIT              00017780
           ELSE                                                         00017790
               MOVE ZEROS TO PW-PROOF-DATE.                             00017800
                                                                        00017810
           WRITE CLAIM-ACTIVITY-RECORD FROM WS-CLAIM-ACTIVITY-RECORD.   00017820
           ADD 1 TO CLAIM-ENTRIES-OUT.                                  00017830
                                                                        00017840
       C020-EXIT.                                                       00017850
           EXIT.                                                        00017860
                                                                        00017870
       C030-PRE-INIT.                                                   00017880
                                                                        00017890
           MOVE SPACES TO WS-DRAFT-ACTIVITY-RECORD                      00017900
                          WS-CLAIM-ACTIVITY-RECORD.                     00017910
                                                                        00017920
           MOVE '00000000' TO CW-REVISION-DATE                          00017921
                             DCW-REVISION-DATE                          00017922
                             DPW-PROOF-DATE.                            00017923
                                                                        00017924
           MOVE '000000'   TO CW-ACTION-TIME                            00017925
                             DCW-ACTION-TIME                            00017926
                              PW-ACTION-TIME                            00017927
                             DPW-ACTION-TIME.                           00017928
                                                                        00017929
           MOVE ZEROS TO  CHAR-ZEROS-3                                  00017930
                          CHAR-ZEROS-4                                  00017960
                          CHAR-ZEROS-5                                  00017970
                          CHAR-ZEROS-6                                  00017980
                          CHAR-ZEROS-7                                  00017990
                          CHAR-ZEROS-8                                  00018000
                          CHAR-ZEROS-9                                  00018010
                          CHAR-ZEROS-10                                 00018020
                          CHAR-ZEROS-15                                 00018070
                          CHAR-ZEROS-17.                                00018090
                                                                        00018100
           MOVE ZEROS TO DAR-PAYEE-ZIP.                                 00018110
           MOVE ZEROS TO DPW-PO-ZIP.                                    00018120
           MOVE ZEROS TO PW-PO-ZIP.                                     00018130
                                                                        00018140
           MOVE 'N'   TO CORR-TRLR-FOUND-SW.                            00018150
                                                                        00018160
090815     MOVE SPACES TO WS-PAYEE-TYPE-CD
                                                                        00018180
           MOVE ZEROS TO  WS-BIN-MAINT-DT                               00018190
                          WS-HOLD-CERT-EFF-DT                           00018200
                          WS-HOLD-PAID-TO                               00018210
                          WS-HOLD-INCUR-DATE.                           00018211
                                                                        00018250
       C030-EXIT.                                                       00018260
           EXIT.                                                        00018270
                                                                        00018280
       D000-DRAFT-PROCESS-FA.                                           00018290
      * MOVES ALL THE FA DATA TO THE DRAFT OUTPUT RECORD                00018300
           MOVE WS-WD-CLAIM-NO          TO DAR-CLAIM-NUMBER             00018310
                                           DPW-CLAIM-NUMBER             00018320
                                           DCW-CLAIM-NUMBER.            00018330
           MOVE SPACES                  TO DAR-POLICY-NUMBER            00018340
                                           DPW-POLICY-NUMBER.           00018341
LGC003     MOVE WS-WD-CERT-NO-10        TO DAR-POLICY-NUMBER            00018361
                                           DPW-POLICY-NUMBER.           00018362
           MOVE EX-FA-INSURED-BIRTH-DT  TO WS-DATE-IN-AREA.             00018400
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      00018410
           MOVE  19 TO WS-DATE-BACK-AREA-CC.                            00018411
           MOVE WS-DATE-BACK-AREA       TO DPW-CLAIMANT-DOB             00018420
                                           DCW-CLAIMANT-DOB.            00018430
                                                                        00018440
           MOVE EX-FA-TOTAL-PAID-AMT    TO DPW-AMOUNT-PAID              00018450
                                           DCW-AMOUNT-PAID.             00018460
           MOVE EX-FA-INSURED-NAME TO DPW-CLAIMANT                      00018470
                                      DCW-CLAIMANT.                     00018480

      ***** HEX-05 NOW ASSIGNED TO DCC WHICH IS STILL IN TESTING        00018490
      *    IF WS-WD-COMPANY-CD = WS-HEX-05                              00018500
      *        MOVE 'DMD'  TO WS-COMPANY                                00018510
      *        MOVE 'CC'   TO DPW-POLICY-TYPE                           00018520
      *    ELSE                                                         00018530
103002     MOVE DTE-CLIENT             TO WS-COMPANY
103002*    MOVE 'CID'  TO WS-COMPANY.                                   00018540
           MOVE 'CR' TO DPW-POLICY-TYPE.                                00018550
                                                                        00018551
           MOVE ZEROS           TO DPW-COLL-AGT-NO.                     00018552
           MOVE WS-WD-ACCOUNT-NO TO DPW-COLL-AGT-CODE.                  00018611
                                                                        00018620
           MOVE EX-FA-LAST-PMT-DT    TO WS-DATE-IN-AREA.                00018630
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      00018640
           MOVE WS-DATE-BACK-AREA TO DPW-LAST-PAID.                     00018650

           IF EX-FA-LAST-REOPEN-DT = WS-HEX-00                          00018660
               MOVE 'A' TO DPW-POLICY-ACTION                            00018670
           ELSE                                                         00018680
               MOVE 'O' TO DPW-POLICY-ACTION.                           00018690

           MOVE EX-FA-LAST-REOPEN-DT TO WS-DATE-IN-AREA.                00018700
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      00018701
           MOVE WS-DATE-BACK-AREA TO DPW-REVISION-DATE.                 00018702
                                                                        00018730
           MOVE EX-FA-CLAIM-STATUS         TO DCW-CLAIM-STATUS.         00018740

           MOVE EX-FA-FILE-ESTABLISH-DT TO WS-DATE-IN-AREA.             00018750
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      00018760
           MOVE WS-DATE-BACK-AREA TO DCW-ENTRY-DATE.                    00018770

           MOVE EX-FA-INCURRED-DT       TO WS-DATE-IN-AREA.             00018780
           MOVE EX-FA-INCURRED-DT       TO WS-HOLD-INCUR-DATE.          00018790
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      00018800
           MOVE  WS-DATE-BACK-AREA-YM   TO  DCW-DATE-INCURRED.          00018801
                                                                        00018860
           MOVE EX-FA-REPORTED-DT          TO WS-DATE-IN-AREA           00018870
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       00018880
           MOVE WS-DATE-BACK-AREA          TO DCW-FIRST-NOTICE          00018890

           MOVE SPACES                     TO DCW-DIAGNOSIS.            00018900
           MOVE DIAG-FLD                   TO DCW-DIAGNOSIS.            00018901

           MOVE EX-FA-LAST-PMT-DT          TO WS-DATE-IN-AREA           00018920
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       00018930
           MOVE WS-DATE-BACK-AREA          TO DCW-LAST-PAID             00018940

122205     MOVE WS-WD-CARRIER          TO DCW-CARRIER
021215*    IF WS-WD-CARRIER = '6'                                       00018950
021215*        MOVE 'W' TO DCW-CLAIM-TYPE                               00018960
021215*    ELSE                                                         00018970
021215*        MOVE ' ' TO DCW-CLAIM-TYPE.                              00018980

           .
       D000-CONTINUE.                                                   00019310

100518     if ex-fa-claim-type = 'L' or 'P' OR 'O'
              move ex-fb-lf-benefit-cd to dcw-exc-ben-type
           else
              move ex-fb-ah-benefit-cd to dcw-exc-ben-type
           end-if

           MOVE EX-FA-CLAIM-TYPE TO WS-CLAIM-TYPE-FA.                   00019320
      *BUILD BENE KEY TO SEARCH FILE ELBENE IN CASE AN ADDR TRLR CAN    00019321
      *NOT BE FOUND                                                     00019340
           MOVE EX-FA-BENEFICIARY TO WS-BENEFICIARY-HOLD.               00019350
           MOVE 'B'    TO   WS-BE-B.                                    00019351
           MOVE WS-WD-COMPANY-CD  TO WS-COMPANY-CD.                     00019360
                                                                        00019370
       D000-EXIT.                                                       00019380
           EXIT.                                                        00019390
                                                                        00019400
       D020-DRAFT-PROCESS-PAYMENTS.                                     00019410
      **************************************************************    00019420
      *   - - DRAFT PROCESSING ONLY - -                            *    00019430
      *  THIS MODULE PROCESSES THE PAYMENTS THAT WERE PUT INTO     *    00019440
      *  THE PAYMENT TABLE. IT ALSO PROCESSES THE FB EXTRACTS      *    00019450
      *  DIFFERENT THAN FOR A CLAIM ACTIVITY, IN THAT, THE         *    00019460
      *  CLAIM PROCESSES FA,FB,FC ORDER, WHEREAS, DRAFT PROCESSING *    00019470
      *  IS FA,FC,FB, BECAUSE THE FC (TRAILERS) CONTAIN THE        *    00019480
      *  CLAIM TYPE FOR EACH INDIVIDUAL DRAFT. THIS FIELD IS       *    00019490
      *  USED TO DETERMINE WHAT FIELDS IN THE FB RECORD ARE TO     *    00019500
      *  BE MOVED.                                                 *    00019510
      *                                                            *    00019520
      **************************************************************    00019530
                                                                        00019540
      * SET SWITCH INITIALLY TO YES                                     00019550
           MOVE 'Y' TO WRITE-DRAFT-SW.                                  00019560
                                                                        00019570
      * IF AT-INDEX = 1 THAT MEANS THERE ARE NO ADDRESSES               00019580
           IF AT-INDEX > 1                                              00019590
               SET AT-INDEX2 TO 1                                       00019600
               PERFORM D021-DRAFT-INSURED-ADDR THRU D021-EXIT.          00019610
                                                                        00019620
           IF NUMBER-OF-PAY-TRLRS > 0                                   00019630
               NEXT SENTENCE                                            00019640
           ELSE                                                         00019650
               GO TO D020-EXIT.                                         00019660
032012
032012     IF WS-COMPANY = 'AHL'
032012        IF PT-PMT-ACCEPT-DT(PT-INDEX2) = LOW-VALUES OR SPACES 
032012           NEXT SENTENCE
032012        ELSE
051412        IF (PT-VOID-DT(PT-INDEX2) NOT = LOW-VALUES AND SPACES) 
051412         AND (PT-VOID-ACCEPT-DT(PT-INDEX2) = 
051412                          LOW-VALUES OR SPACES)
051412         AND (PT-RECORDED-DT(PT-INDEX2) > X'A85F')
051412           NEXT SENTENCE
051412        ELSE
032012           MOVE 'N' TO WRITE-DRAFT-SW
032012           GO TO 020-WRITE-ROUTINE
051412        END-IF
032012        END-IF
032012     END-IF.
                                                                        00019670
           PERFORM E090-DRAFT-PROCESS-FB THRU E090-EXIT.                00019680

111714     move pt-check-written-dt (pt-index2)
111714                                 to ws-date-in-area
111714     PERFORM CONVERT-DATE THRU CONVERT-EXIT
111714     MOVE WS-DATE-BACK-AREA       TO ws-test-conv-date
062821     if ws-company = 'CID' or 'AHL' OR 'FNL'
111714        if ws-test-conv-date > 20150430
111714           move zeros            to dar-draft-account
111714           move pt-check-no (pt-index2)
111714                                 to dar-draft-number
111714        else
111714           MOVE '0'              TO DAR-DRAFT-ACCOUNT(1:1)
111714           MOVE PT-CHECK-NO(PT-INDEX2)(1:1)
111714                                 TO DAR-DRAFT-ACCOUNT(2:1)
111714           MOVE '00'             TO DAR-DRAFT-NUMBER(1:2)
111714           MOVE PT-CHECK-NO(PT-INDEX2)(2:6)
111714                                 TO DAR-DRAFT-NUMBER(3:6)
111714        end-if
111714     ELSE
111714********** This is DCC company
022017        if ws-test-conv-date > 20170522
022017           move zeros            to dar-draft-account
022017           move pt-check-no (pt-index2)
022017                                 to dar-draft-number
022017        else
111714           MOVE '00'             TO DAR-DRAFT-ACCOUNT
111714           MOVE '0'              TO DAR-DRAFT-NUMBER(1:1)
111714           MOVE PT-CHECK-NO(PT-INDEX2)
                                       TO DAR-DRAFT-NUMBER(2:7)
022017        end-if
111714     end-if

           IF PT-VOID-DT(PT-INDEX2)                                     00019730
                IS GREATER THAN WS-BIN-PREV-CYC-DT                      00019740
                NEXT SENTENCE                                           00019750
             ELSE                                                       00019760
                GO TO 020-CK-PT-PAYMENT.                                00019770
                                                                        00019780
           IF PT-VOID-DT(PT-INDEX2)                                     00019790
                IS GREATER THAN WS-BIN-CURR-CYC-DT                      00019800
                NEXT SENTENCE                                           00019810
             ELSE                                                       00019820
               MOVE 'S' TO DAR-DRAFT-STATUS
               IF PT-ACH-PAYMENT (PT-INDEX2) = 'Y'
                  perform S000-update-sql-table
                                       thru S000-exit
               END-IF
               GO TO 020-STATUS-IS-SET.                                 00019840
                                                                        00019850
       020-CK-PT-PAYMENT.                                               00019860
               IF PT-PAYMENT-ORIGIN(PT-INDEX2) = '3'                    00019870
                   MOVE 'M' TO DAR-DRAFT-STATUS                         00019880
               ELSE                                                     00019890
                   MOVE 'O' TO DAR-DRAFT-STATUS.                        00019900
                                                                        00019910
       020-STATUS-IS-SET.                                               00019920
                                                                        00019930
           MOVE PT-AMOUNT-PAID(PT-INDEX2) TO DAR-AMOUNT-PAID
                                                                        00019950
    ******  THE FOLLOWING IF IS USED TO RECORD CLAIM REFUNDS            00019960
    ******  AND ADJUSTMENTS FOR CID AND DMD CLAIMS                      00019970
                                                                        00019980
           IF PT-AMOUNT-PAID(PT-INDEX2) IS NEGATIVE                     00019990
              AND PT-PAYMENT-TYPE(PT-INDEX2) = '4'                      00020000
                 DISPLAY 'AMOUNT NEGATIVE AND PMT TYPE=4'               00020010
                 MOVE 'S' TO DAR-DRAFT-STATUS.                          00020020
                                                                        00020030
DANA**** THE DAR-NOTE-CODE IS CHECKED IN PROGRAM AICB01CI TO DETERMINE  00020030
DANA**** IF THE MSA RECORD SHOULD BE A CREDIT.                          00020030
DANA****                                                                00020030
DANA       IF PT-AMOUNT-PAID(PT-INDEX2) IS NEGATIVE                     00019990
DANA         AND PT-PAYMENT-ORIGIN(PT-INDEX2) = '3'                     00019870
DANA           DISPLAY 'NEGATIVE ADJUSTMENT:  '                         00020010
DANA             PT-CHECK-NO(PT-INDEX2) '  ' PT-AMOUNT-PAID(PT-INDEX2)  00020010
DANA           MOVE 01 TO DAR-NOTE-CODE.                                00020020
                                                                        00020030
           MOVE PT-RECORDED-BY(PT-INDEX2) TO DAR-AUDITOR.               00020040
           IF PT-PAYMENT-TYPE(PT-INDEX2) = ('1' OR '4')                 00020050
               MOVE 'P' TO DAR-PART-FINAL                               00020060
                           DPW-POLICY-STATUS                            00020070
           ELSE                                                         00020080
               IF PT-PAYMENT-TYPE(PT-INDEX2) = ('2' OR '3')             00020090
                   MOVE 'F' TO DAR-PART-FINAL                           00020100
                               DPW-POLICY-STATUS                        00020110
               ELSE                                                     00020120
                   MOVE 'N' TO WRITE-DRAFT-SW.                          00020130
                                                                        00020140
           MOVE PT-PAID-FROM-DT(PT-INDEX2) TO WS-DATE-IN-AREA.          00020150
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      00020160
           MOVE WS-DATE-BACK-AREA          TO DAR-FROM-DATE.            00020170

           MOVE PT-PAID-THRU-DT(PT-INDEX2) TO WS-DATE-IN-AREA.          00020180
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      00020190
           MOVE WS-DATE-BACK-AREA          TO DAR-TO-DATE.              00020200
                                                                        00020210
090815     MOVE PT-PAYEE-TYPE-CD(PT-INDEX2) TO WS-PAYEE-TYPE-CD
                                                                        00020221
           MOVE 'Y' TO PAYEE-ADDR-SW.                                   00020240
           IF AT-INDEX > 1                                              00020250
               SET AT-INDEX2 TO 1                                       00020260
               PERFORM D022-DRAFT-PAYEE-ADDR THRU D022-EXIT.            00020270

      *    if not payee-addr-needed
      *       display ' found other than bene ' dar-claim-number ' '
      *          dar-policy-number ' ' dar-draft-number ' '
      *          WS-PAYEE-TYPE-CD
      *    end-if
                                                                        00020272
           IF PAYEE-ADDR-NEEDED
      *       display ' about to hunt for bene ' dar-claim-number ' '
      *          dar-policy-number ' ' dar-draft-number ' '
      *          WS-PAYEE-TYPE-CD
               PERFORM E040-READ-BENEFICIARY THRU E040-EXIT.            00020290
                                                                        00020300
           MOVE WS-BIN-CURR-CYC-DT      TO WS-DATE-IN-AREA.             00020310
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      00020311
           MOVE WS-DATE-BACK-AREA       TO DAR-ENTERED-DATE.            00020312
                                                                        00020340
           IF PT-VOID-DT(PT-INDEX2) GREATER THAN WS-BIN-CURR-CYC-DT     00020350
              GO TO 020-WRITE-ROUTINE.                                  00020360
                                                                        00020370
PEMMOD*    IF PT-RECORDED-DT(PT-INDEX2) GREATER THAN WS-BIN-CURR-CYC-DT 00020380
PEMMOD     IF PT-CHECK-WRITTEN-DT(PT-INDEX2) > WS-BIN-CURR-CYC-DT       00020380
              GO TO 020-WRITE-ROUTINE.                                  00020390
                                                                        00020400
           IF PT-VOID-DT(PT-INDEX2) GREATER THAN WS-BIN-PREV-CYC-DT     00020410
              NEXT SENTENCE                                             00020420
            ELSE                                                        00020430
              GO TO 020-WRITE-ROUTINE.                                  00020440
                                                                        00020450
PEMMOD*    IF PT-RECORDED-DT(PT-INDEX2) GREATER THAN WS-BIN-PREV-CYC-DT 00020460
PEMMOD     IF PT-CHECK-WRITTEN-DT(PT-INDEX2) > WS-BIN-PREV-CYC-DT       00020460
              NEXT SENTENCE                                             00020470
            ELSE                                                        00020480
              GO TO 020-WRITE-ROUTINE.                                  00020490
                                                                        00020500
           DISPLAY                                                      00020510
             'RECORDED, VOID, PROCESS ARE = CLAIM#= ' WS-WD-CLAIM-NO.   00020520
           MOVE 'N' TO WRITE-DRAFT-SW.                                  00020530
                                                                        00020540
       020-WRITE-ROUTINE.                                               00020550

013017     move pt-ach-payment (pt-index2)
013017                                 to dcw-ach-payment
111714     move pt-check-written-dt (pt-index2)
111714                                 to ws-date-in-area
111714     PERFORM CONVERT-DATE THRU CONVERT-EXIT
111714     MOVE WS-DATE-BACK-AREA       TO dcw-last-paid
                                                                        00020560
           IF WRITE-DRAFT                                               00020570
               WRITE DRAFT-ACTIVITY-RECORD                              00020580
                   FROM WS-DRAFT-ACTIVITY-RECORD                        00020590
               ADD 1 TO DRAFT-ENTRIES-OUT.                              00020600
                                                                        00020610
           SET PT-INDEX2 UP BY 1.                                       00020620
           SUBTRACT 1 FROM NUMBER-OF-PAY-TRLRS.                         00020630
090804     MOVE SPACES             TO DAR-NOTE-CODE-XX.
                                                                        00020640
       D020-EXIT.                                                       00020650
           EXIT.                                                        00020660
                                                                        00020670
       D021-DRAFT-INSURED-ADDR.                                         00020680
      * BRINGS IN THE DRAFT RECORD INSURED'S ADDRESS                    00020690
           IF AT-INDEX2 < AT-INDEX                                      00020700
               IF AT-ADDRESS-TYPE(AT-INDEX2) = 'I'                      00020701
                   MOVE AT-MAIL-TO-NAME(AT-INDEX2) TO DPW-POLICY-OWNER  00020720
                   MOVE AT-ADDRESS-LINE-1(AT-INDEX2) TO DPW-PO-ADDR1    00020730
                   MOVE AT-ADDRESS-LINE-2(AT-INDEX2) TO DPW-PO-ADDR2
051810             MOVE AT-CITY (AT-INDEX2) TO DPW-PO-CITY
051810             MOVE AT-STATE (AT-INDEX2) TO DPW-PO-STATE
051810                                        DCW-CLAIMANT-STATE-OF-RES
051810*            MOVE AT-CITY-STATE(AT-INDEX2) TO WS-CITY-STATE-WORK  00020750
051810*            PERFORM G000-BREAK-CITY-STATE THRU G000-EXIT         00020760
051810*            PERFORM G100-CHECK-STATE-CODE THRU G100-EXIT                   
051810*            MOVE WS-CITY-FOUND           TO DPW-PO-CITY          00020770
051810*            MOVE WS-STATE-FOUND     TO DPW-PO-STATE              00020780
051810*                                       DCW-CLAIMANT-STATE-OF-RES 00020790
                   MOVE AT-ZIP-CODE(AT-INDEX2) TO UNPAC-ZIP             00020791
LGC035                IF ZIP-X-5 NOT NUMERIC                            00020792
LGC035                    MOVE ZERO TO ZIP-X-5                          00020793
LGC035                    DISPLAY WS-WD-CLAIM-NO                        00020794
LGC035                    MOVE ZIP-X-5                TO IN-X-5         00020795
LGC035                    MOVE ZEROS                  TO IN-X-4         00020797
LGC035                    MOVE  ZIP-N  TO  ZIP-PAC                      00020798
LGC035                    MOVE ZIP-PAC                TO DPW-PO-ZIP     00020799
LGC035                ELSE                                              00020800
                   MOVE ZIP-X-5                TO IN-X-5                00020801
030702*            MOVE ZIP-X-4                TO IN-X-4                00020802
                   MOVE ZEROS                  TO IN-X-4                00020803
                   MOVE ZIP-N                  TO ZIP-PAC               00020804
                   MOVE ZIP-PAC                TO DPW-PO-ZIP            00020807
               ELSE                                                     00020810
                   SET AT-INDEX2 UP BY 1                                00020820
                   GO TO D021-DRAFT-INSURED-ADDR.                       00020830

       D021-EXIT.                                                       00020840
           EXIT.                                                        00020850
                                                                        00020860
       D022-DRAFT-PAYEE-ADDR.                                           00020870
      * BRINGS IN THE DRAFT RECORD PAYEE ADDRESS                        00020880
           IF AT-INDEX2 < AT-INDEX                                      00020890
090815         IF (AT-ADDRESS-TYPE(AT-INDEX2) = WS-PAYEE-TYPE)
090815            and (at-address-seq (at-index2)
090815               = ws-payee-type-seq)
                   MOVE 'N' TO PAYEE-ADDR-SW                            00020910
                   MOVE AT-MAIL-TO-NAME(AT-INDEX2) TO DAR-PAYEE         00020920
                   MOVE AT-ADDRESS-LINE-1(AT-INDEX2) TO DAR-PAYEE-ADDR1 00020930
                   MOVE AT-ADDRESS-LINE-2(AT-INDEX2) TO DAR-PAYEE-ADDR2
051810             MOVE AT-CITY (AT-INDEX2) TO DAR-PAYEE-CITY
051810             MOVE AT-STATE (AT-INDEX2) TO DAR-PAYEE-STATE
051810*            MOVE AT-CITY-STATE(AT-INDEX2) TO WS-CITY-STATE-WORK  00020950
051810*            PERFORM G000-BREAK-CITY-STATE THRU G000-EXIT         00020960
051810*            PERFORM G100-CHECK-STATE-CODE THRU G100-EXIT                   
051810*            MOVE WS-CITY-FOUND           TO DAR-PAYEE-CITY       00020970
051810*            MOVE WS-STATE-FOUND     TO DAR-PAYEE-STATE           00020980
                   MOVE AT-ZIP-CODE(AT-INDEX2) TO UNPAC-ZIP             00020981
LGC035                IF ZIP-X-5 NOT NUMERIC                            00020982
LGC035                    MOVE ZERO TO ZIP-X-5                          00020983
LGC035                    DISPLAY WS-WD-CLAIM-NO                        00020984
LGC035                    MOVE ZIP-X-5                TO IN-X-5         00020985
LGC035                    MOVE ZEROS                  TO IN-X-4         00020987
LGC035                    MOVE  ZIP-N  TO  ZIP-PAC                      00020988
LGC035                    MOVE ZIP-PAC                TO DAR-PAYEE-ZIP  00020989
LGC035                ELSE                                              00020990
                   MOVE ZIP-X-5                TO IN-X-5                00020991
030702*            MOVE ZIP-X-4                TO IN-X-4                00020992
                   MOVE ZEROS                  TO IN-X-4                00020993
030702             MOVE ZIP-N                  TO ZIP-PAC               00020994
                   MOVE ZIP-PAC                TO DAR-PAYEE-ZIP         00020996
               ELSE                                                     00021000
                   SET AT-INDEX2 UP BY 1                                00021010
                   GO TO D022-DRAFT-PAYEE-ADDR.
                                                                        00021020
       D022-EXIT.                                                       00021030
           EXIT.                                                        00021040
                                                                        00021050
       D030-CLAIM-PROCESS-FA.                                           00021060
      * MOVES ALL FA DATA TO THE CLAIM ACTIVITY RECORD                  00021070
           MOVE WS-WD-CLAIM-NO          TO CW-CLAIM-NUMBER              00021080
                                           PW-CLAIM-NUMBER.             00021090
           MOVE SPACES                  TO PW-POLICY-NUMBER.            00021091
LGC003     MOVE WS-WD-CERT-NO-10        TO PW-POLICY-NUMBER.            00021092
           MOVE EX-FA-INSURED-BIRTH-DT  TO WS-DATE-IN-AREA.             00021140
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      00021150
           MOVE 19 TO WS-DATE-BACK-AREA-CC.                             00021151
           MOVE WS-DATE-BACK-AREA       TO CW-CLAIMANT-DOB              00021160
                                           PW-CLAIMANT-DOB.             00021170
           MOVE EX-FA-CLAIM-STATUS      TO CW-CLAIM-STATUS.             00021180
           MOVE EX-FA-FILE-ESTABLISH-DT TO WS-DATE-IN-AREA.             00021190
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      00021200
           MOVE WS-DATE-BACK-AREA TO CW-ENTRY-DATE.                     00021210
                                                                        00021220
           MOVE EX-FA-INCURRED-DT       TO WS-DATE-IN-AREA              00021230
                                        WS-HOLD-INCUR-DATE.             00021240
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       00021250
           MOVE  WS-DATE-BACK-AREA-YM   TO  CW-DATE-INCURRED.           00021251
                                                                        00021310
           MOVE EX-FA-REPORTED-DT          TO WS-DATE-IN-AREA           00021320
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       00021330
           MOVE WS-DATE-BACK-AREA          TO CW-FIRST-NOTICE           00021340
           MOVE SPACES                     TO CW-DIAGNOSIS.             00021350
           MOVE DIAG-FLD                   TO CW-DIAGNOSIS.             00021351
           MOVE EX-FA-TOTAL-PAID-AMT       TO CW-AMOUNT-PAID.           00021370
           MOVE EX-FA-LAST-PMT-DT          TO WS-DATE-IN-AREA           00021380
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       00021390
           MOVE WS-DATE-BACK-AREA          TO CW-LAST-PAID              00021400
           IF WS-WD-CARRIER = '6'                                       00021410
               MOVE 'W' TO CW-CLAIM-TYPE                                00021420
           ELSE                                                         00021430
               MOVE ' ' TO CW-CLAIM-TYPE.                               00021440

030702***** HEX-05 NOW ASSIGNED TO DCC                                  00018490
030702*    IF WS-WD-COMPANY-CD = WS-HEX-05                              00018500
030702*        MOVE 'DMD'  TO WS-COMPANY                                00018510
030702*        MOVE 'CC'   TO PW-POLICY-TYPE                            00018520
030702*    ELSE                                                         00018530
103002*    MOVE 'CID'  TO WS-COMPANY.                                   00018540
103002     MOVE DTE-CLIENT             TO WS-COMPANY
           MOVE 'CR'   TO PW-POLICY-TYPE.                               00021510
                                                                        00021511
           MOVE ZEROS           TO PW-COLL-AGT-NO.                      00021512
           MOVE WS-WD-ACCOUNT-NO TO PW-COLL-AGT-CODE.                   00021571
                                                                        00021572
           MOVE EX-FA-TOTAL-PAID-AMT   TO PW-AMOUNT-PAID.               00021580
           MOVE EX-FA-INSURED-NAME TO CW-CLAIMANT                       00021590
                                      PW-CLAIMANT.                      00021600
                                                                        00021610
           IF SET-UP-NEEDED                                             00021620
               MOVE 'O' TO PW-POLICY-STATUS                             00021630
           ELSE                                                         00021640
               IF EX-FA-LAST-CLOSE-REASON = '2'                         00021650
                   MOVE 'R' TO PW-POLICY-STATUS                         00021660
               ELSE                                                     00021670
                   MOVE 'D' TO PW-POLICY-STATUS.                        00021680
           MOVE EX-FA-LAST-CLOSE-DT     TO WS-DATE-IN-AREA              00021690
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       00021700
           MOVE WS-DATE-BACK-AREA TO PW-LAST-PAID                       00021710
           MOVE EX-FA-LAST-MAINT-USER         TO PW-AUDITOR.            00021720
           IF SET-UP-NEEDED                                             00021730
               MOVE 'A' TO CW-CLAIM-ACTION PW-POLICY-ACTION             00021740
           ELSE                                                         00021750
               MOVE 'U' TO CW-CLAIM-ACTION PW-POLICY-ACTION.            00021760
030702*    MOVE EX-FA-LAST-CLOSE-DT     TO WS-DATE-IN-AREA              00021770
030702*    PERFORM CONVERT-DATE THRU CONVERT-EXIT                       00021771
           MOVE WS-DATE-BACK-AREA TO PW-REVISION-DATE.                  00021772
                                                                        00021800
           MOVE EX-FA-LAST-MAINT-DT TO WS-BIN-MAINT-DT.                 00021810
           MOVE EX-FA-CLAIM-TYPE TO WS-CLAIM-TYPE-FA.                   00021820
                                                                        00021830
       D030-EXIT.                                                       00021840
           EXIT.                                                        00021850
                                                                        00021860
       D040-CLAIM-PROCESS-FB.                                           00021870
      * MOVES ALL FB DATA TO THE CLAIM ACTIVITY RECORD                  00021880
           IF EX-FB-IND-GRP-TYPE = 'I'                                  00021890
               MOVE EX-FB-IND-GRP-TYPE      TO WS-INDIV-GRP-CD          00021900
           ELSE                                                         00021910
               MOVE 'G'                     TO WS-INDIV-GRP-CD.         00021920

100518     IF WS-CLAIM-TYPE-FA = 'L' OR 'O'                             00021930
               MOVE EX-FB-LF-REMAINING-AMT  TO PW-INDEMNITY             00021940
               MOVE EX-FB-LF-ORIG-TERM      TO WS-ORIGINAL-TERM         00021950

062821         IF WS-COMPANY = 'CID' OR 'DCC' OR 'AHL' OR 'FNL'
                   PERFORM E080-DFT-CLM-BENEFIT-CD THRU E080-EXIT       00021970
               ELSE                                                     00021980
                   MOVE EX-FB-LF-BENEFIT-CD     TO WS-BENEFIT-CD        00021990
           ELSE                                                         00022000
               MOVE EX-FB-AH-BENEFIT-AMT    TO PW-INDEMNITY             00022010
               MOVE EX-FB-AH-BENEFIT-CD     TO WS-BENEFIT-CD            00022020
               MOVE EX-FB-AH-ORIG-TERM      TO WS-ORIGINAL-TERM.        00022030
                                                                        00022040
           MOVE EX-FB-STATE       TO PW-COLL-AGT-STATE.                 00022050
                                                                        00022060
           MOVE EX-FB-EFFECTIVE-DT         TO WS-DATE-IN-AREA           00022070
                                           WS-HOLD-CERT-EFF-DT.         00022080
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       00022090
           MOVE WS-DATE-BACK-AREA       TO PW-ISSUE-DATE                00022100
           MOVE WS-HOLD-CERT-EFF-DT TO DC-BIN-DATE-1                    00022110
           MOVE WS-ORIGINAL-TERM    TO DC-ELAPSED-MONTHS                00022120
           MOVE '6'                 TO DC-OPTION-CODE                   00022130
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA                    00022140
           IF NO-CONVERSION-ERROR                                       00022150
               MOVE  DC-BIN-DATE-2          TO  WS-HOLD-PAID-TO         00022151
               MOVE  DC-GREG-DATE-1-YMD     TO  WS-DATE-BACK-AREA-YMD   00022152
               IF WS-DATE-BACK-AREA-YY > 70                             00022153
                   MOVE 19 TO WS-DATE-BACK-AREA-CC                      00022154
               ELSE                                                     00022155
                   MOVE 20 TO WS-DATE-BACK-AREA-CC                      00022156
           ELSE                                                         00022180
               MOVE  ZERO         TO  WS-DATE-BACK-AREA.                00022181
           MOVE  WS-DATE-BACK-AREA      TO  PW-PAID-TO.                 00022182
           MOVE WS-HOLD-INCUR-DATE  TO DC-BIN-DATE-1                    00022200
           MOVE WS-HOLD-PAID-TO     TO DC-BIN-DATE-2                    00022210
           MOVE '1'                 TO DC-OPTION-CODE                   00022220
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA                    00022230
           IF NO-CONVERSION-ERROR                                       00022240
               IF DC-ODD-DAYS-OVER = 0                                  00022250
                   ADD 1 TO DC-ELAPSED-MONTHS                           00022260
                   MOVE DC-ELAPSED-MONTHS  TO PW-BENEFIT-DURATION       00022270
               ELSE                                                     00022280
                   MOVE DC-ELAPSED-MONTHS  TO PW-BENEFIT-DURATION       00022290
           ELSE                                                         00022300
               MOVE ZEROS TO PW-BENEFIT-DURATION.                       00022310

030702*    PERFORM E070-CLAIM-ACCESS-PLAN-CODE THRU E070-EXIT.          00022330
030702     MOVE WS-INDIV-GRP-CD          TO WS-PC-KEY-PT1.              00023710
030702     MOVE WS-BENEFIT-CD            TO WS-PC-KEY-PT2.              00023730
030702     MOVE SPACES                   TO WS-PC-KEY-PT3.              00023750
030702     MOVE WS-PLAN-CODE-AREA        TO PW-PLAN-NUMBER.             00025560

       D040-EXIT.                                                       00022350
           EXIT.                                                        00022360
                                                                        00022370
       D050-CLAIM-PROCESS-ADDR.                                         00022380
      * TRIES TO FIND AN INSURED ADDRESS FROM THE ADDRESS TRAILERS      00022390
      * (FC) THAT WERE BUILT IN A TABLE                                 00022400
           IF AT-INDEX2 < AT-INDEX                                      00022410
               IF AT-ADDRESS-TYPE(AT-INDEX2) = 'I'                      00022411
                   MOVE AT-MAIL-TO-NAME(AT-INDEX2) TO PW-POLICY-OWNER   00022430
                   MOVE AT-ADDRESS-LINE-1(AT-INDEX2) TO PW-PO-ADDR1     00022440
                   MOVE AT-ADDRESS-LINE-2(AT-INDEX2) TO PW-PO-ADDR2
051810             MOVE AT-CITY (AT-INDEX2) TO PW-PO-CITY
051810             MOVE AT-STATE (AT-INDEX2) TO PW-PO-STATE
051810                                        CW-CLAIMANT-STATE-OF-RES
051810*            MOVE AT-CITY-STATE(AT-INDEX2) TO WS-CITY-STATE-WORK  00022460
051810*            PERFORM G000-BREAK-CITY-STATE THRU G000-EXIT         00022470
051810*            PERFORM G100-CHECK-STATE-CODE THRU G100-EXIT                   
051810*            MOVE WS-CITY-FOUND           TO PW-PO-CITY           00022480
051810*            MOVE WS-STATE-FOUND     TO PW-PO-STATE               00022490
051810*                                       CW-CLAIMANT-STATE-OF-RES  00022500
                   MOVE AT-ZIP-CODE(AT-INDEX2) TO UNPAC-ZIP             00022501
LGC035                IF ZIP-X-5 NOT NUMERIC                            00022502
LGC035                    MOVE ZERO TO ZIP-X-5                          00022503
LGC035                    DISPLAY WS-WD-CLAIM-NO                        00022504
LGC035                    MOVE ZIP-X-5                TO IN-X-5         00022505
LGC035                    MOVE ZEROS                  TO IN-X-4         00022507
LGC035                    MOVE  ZIP-N  TO  ZIP-PAC                      00022508
LGC035                    MOVE ZIP-PAC                TO PW-PO-ZIP      00022509
LGC035                ELSE                                              00022510
                   MOVE ZIP-X-5                TO IN-X-5                00022511
030702*            MOVE ZIP-X-4                TO IN-X-4                00022512
                   MOVE ZEROS                  TO IN-X-4                00022513
030702             MOVE ZIP-N                  TO ZIP-PAC               00022514
                   MOVE ZIP-PAC                TO PW-PO-ZIP             00022516
               ELSE                                                     00022520
                   SET AT-INDEX2 UP BY 1                                00022530
                   GO TO D050-CLAIM-PROCESS-ADDR.                       00022540

       D050-EXIT.                                                       00022550
           EXIT.                                                        00022560
                                                                        00022570
       D060-BUILD-FC.                                                   00022580
      ******************************************************************00022590
      * BRINGS IN ALL THE FC TRAILERS---PUTS THE PAYENTS AND ADDRESSES *00022600
      * INTO WORKING STORAGE TABLES.  LOOKS FOR ONLY ONE CORRESPONDENCE*00022610
      * TRAILER                                                        *00022620
      ******************************************************************00022630
                                                                        00022640
           PERFORM C000-READ-EX-F-TAPE THRU C000-EXIT.                  00022650
           IF EXTRACT-EOF OR IN-RECORD-TYPE = 'A'                       00022660
               GO TO D060-EXIT.                                         00022670
                                                                        00022671
           MOVE IN-DATA-AREA TO EX-EXTRACT-F-RECORD-C.                  00022680
                                                                        00022681
           IF EX-FC-TRAILER-TYPE = '6'                                  00022682
               NEXT SENTENCE                                            00022700
             ELSE                                                       00022710
               GO TO D059-NOT-TRLR.                                     00022711
                                                                        00022730
                                                                        00022731
           IF     IN-TRAILER-SEQ-NO = DIAG-COMP                         00022732
               NEXT SENTENCE                                            00022733
             ELSE                                                       00022734
               GO TO D059-NOT-TRLR.                                     00022735
                                                                        00022736
                                                                        00022737
                                                                        00022738
           MOVE  IN-DATA-AREA  TO  GEN-INFO-TRLR.                       00022739
                                                                        00022740
                                                                        00022741
                                                                        00022742
       D059-NOT-TRLR.                                                   00022743
                                                                        00022744
           IF EX-FC-TRAILER-TYPE = '2'                                  00022745
               NEXT SENTENCE                                            00022746
             ELSE                                                       00022747
               GO TO D060-CK-TYPE-5.                                    00022748
                                                                        00022749
           MOVE EX-FC-TRAILER-BODY TO WS-FC-PAYMENT-TR.                 00022750
                                                                        00022751

PEMMOD     IF (WS-FC-CHECK-WRITTEN-DT > WS-BIN-PREV-CYC-DT)
PEMMOD        AND (EX-FC-RECORDED-DT < X'9828') *> 06/08/2001??
PEMMOD        GO TO D060-CK-VOID-DT
PEMMOD     END-IF
PEMMOD
PEMMOD*    IF EX-FC-RECORDED-DT > WS-BIN-PREV-CYC-DT                    00022760
PEMMOD     IF WS-FC-CHECK-WRITTEN-DT > WS-BIN-PREV-CYC-DT               00022760
                NEXT SENTENCE                                           00022770
              ELSE                                                      00022780
                GO TO D060-CK-VOID-DT.                                  00022790
                                                                        00022800
PEMMOD*    IF EX-FC-RECORDED-DT > WS-BIN-CURR-CYC-DT                    00022810
PEMMOD     IF WS-FC-CHECK-WRITTEN-DT > WS-BIN-CURR-CYC-DT               00022810
                GO TO D060-CK-VOID-DT.                                  00022820
                                                                        00022830
           GO TO D060-DO-MOVES.                                         00022840
                                                                        00022850
       D060-CK-VOID-DT.                                                 00022860
                                                                        00022870
           IF WS-FC-VOID-DT > WS-BIN-PREV-CYC-DT                        00022880
                NEXT SENTENCE                                           00022890
              ELSE                                                      00022900
                GO TO D060-EXIT.                                        00022910
                                                                        00022920
           IF WS-FC-VOID-DT > WS-BIN-CURR-CYC-DT                        00022930
                GO TO D060-EXIT.                                        00022940
                                                                        00022950
       D060-DO-MOVES.                                                   00022960
                                                                        00022970
      * WE USE ONLY THOSE PAYMENTS OR VOIDS RECORDED WITHIN THE DATES   00022980
      *    FROM THE DATE-RANGE DATE CARD READ IN FROM THE JCL.          00022990
                                                                        00023000
           MOVE IN-DATA-AREA TO PAYMENT-TRAILER-TABLE(PT-INDEX)         00023010
           MOVE 'Y' TO PAYMENT-VOID-SW                                  00023020
           ADD +1 TO NUMBER-OF-PAY-TRLRS                                00023030
           SET PT-INDEX UP BY 1.                                        00023040
                                                                        00023041
                                                                        00023050
           GO TO D060-EXIT.                                             00023060
                                                                        00023070
       D060-CK-TYPE-5.                                                  00023080
                                                                        00023090
032012     IF AT-INDEX > 20
032012         DISPLAY 'MORE THAN 20 ADDRESSES FOUND '
032012         GO TO D060-EXIT
032012     END-IF.
032012
           IF EX-FC-TRAILER-TYPE = '5'                                  00023100
               MOVE IN-DATA-AREA TO ADDRESS-TRAILER(AT-INDEX)
090815         evaluate at-address-type (at-index)
090815            when 'I'
090815               compute at-address-seq-no (at-index) =
090815                  in-trailer-seq-no - 0
090815            when 'B'
090815               compute at-address-seq-no (at-index) =
090815                  in-trailer-seq-no - +10
090815            when 'A'
090815               compute at-address-seq-no (at-index) =
090815                  in-trailer-seq-no - +20
090815            when 'P'
090815               compute at-address-seq-no (at-index) =
090815                  in-trailer-seq-no - +30
090815            when 'E'
090815               compute at-address-seq-no (at-index) =
090815                  in-trailer-seq-no - +40
090815            when 'O'
090815               compute at-address-seq-no (at-index) =
090815                  in-trailer-seq-no - +50
090815            when 'Q'
090815               compute at-address-seq-no (at-index) =
090815                  in-trailer-seq-no - +60
090815            when other
090815               move 1            to at-address-seq-no (at-index)
090815         end-evaluate
               SET AT-INDEX UP BY 1                                     00023120
               GO TO D060-EXIT.                                         00023130
                                                                        00023140
                                                                        00023141
           IF EX-FC-TRAILER-TYPE = '4'                                  00023150
               AND NO-CORR-TRLR-FOUND                                   00023160
RWSTST             MOVE EX-FC-TRAILER-BODY TO WS-FC-CORRESPONDENCE-TR   00023162
               IF WS-FC-LETTER-ANSWERED-DT > LOW-VALUES                 00023180
                   MOVE 'Y' TO CORR-TRLR-FOUND-SW.                      00023190
       D060-EXIT.                                                       00023200
           EXIT.                                                        00023210
                                                                        00023220
       E020-DRAFT-CORR-TRLR.                                            00023230
      * MOVES THE DATE TO THE PROOF DATE ON THE DRAFT RECORD            00023240
      * IF NO TRAILER CAN BE FOUND THEN ZEROS ARE MOVED                 00023250

           MOVE WS-FC-LETTER-ANSWERED-DT TO WS-DATE-IN-AREA             00023260
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       00023261
           MOVE WS-DATE-BACK-AREA       TO DAR-PROOF.                   00023262

       E020-EXIT.                                                       00023290
           EXIT.                                                        00023300
                                                                        00023310
       E040-READ-BENEFICIARY.                                           00023320
      * READS ELBENE IN SEARCH OF A PAYEE ADDRESS                       00023321
           MOVE 'B'       TO   WS-BE-B.                                 00023322
           MOVE WS-BE-CONTROL-PRIMARY TO BE-CONTROL-PRIMARY.            00023323
           READ ELBENE                                                  00023324
           IF BENE-STATUS = '00'                                        00023360
      *        display ' Found the bene ' BE-BENEFICIARY
               MOVE BE-MAIL-TO-NAME TO DAR-PAYEE                        00023370
               MOVE BE-ADDRESS-LINE-1 TO DAR-PAYEE-ADDR1                00023380
               MOVE BE-ADDRESS-LINE-2 TO DAR-PAYEE-ADDR2
051810         MOVE BE-CITY TO DAR-PAYEE-CITY
051810         MOVE BE-STATE TO DAR-PAYEE-STATE
051810*        MOVE BE-CITY-STATE     TO WS-CITY-STATE-WORK             00023400
051810*        PERFORM G000-BREAK-CITY-STATE THRU G000-EXIT             00023410
051810*        PERFORM G100-CHECK-STATE-CODE THRU G100-EXIT                   
051810*        MOVE WS-CITY-FOUND           TO DAR-PAYEE-CITY           00023420
051810*        MOVE WS-STATE-FOUND          TO DAR-PAYEE-STATE          00023430
               MOVE BE-ZIP-CODE TO UNPAC-ZIP                            00023431
               MOVE ZIP-X-5     TO IN-X-5                               00023432
030702*        MOVE ZIP-X-4     TO IN-X-4                               00023433
               MOVE ZEROS       TO IN-X-4                               00023434
030702         MOVE ZIP-N       TO ZIP-PAC                              00023435
               MOVE ZIP-PAC     TO DAR-PAYEE-ZIP                        00023437
               IF BE-ACH-YES-OR-NO = 'Y'
      *           DISPLAY ' BENE IS ACH ' BE-ACH-ABA-ROUTING-NUMBER
                  MOVE BE-ACH-ABA-ROUTING-NUMBER
                                       TO dar-aba-routing-number
                  MOVE BE-ACH-BANK-ACCOUNT-NUMBER
                                       TO dar-ach-account-number
082317            move be-ach-sub-type to dar-ach-sub-type
               END-IF
           ELSE                                                         00023450
               IF BENE-STATUS = '23'                                    00023460
      * NO BENE FILE ADDRESS FOUND                                      00023470
                   NEXT SENTENCE                                        00023480
               ELSE                                                     00023490
                   DISPLAY 'ERROR READING BENE STATUS= '                00023500
                            BENE-STATUS                                 00023510
                   GO TO ABEND-PGM.                                     00023520
       E040-EXIT.                                                       00023530
           EXIT.                                                        00023540
                                                                        00023550
       E050-CLAIM-CORR-TRLR.                                            00023560

      * MOVES THE DATE TO THE PROOF DATE ON THE DRAFT RECORD            00023570
      * IF NO TRAILER CAN BE FOUND THEN ZEROS ARE MOVED                 00023580
           MOVE WS-FC-LETTER-ANSWERED-DT TO WS-DATE-IN-AREA             00023590
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       00023591
           MOVE WS-DATE-BACK-AREA       TO PW-PROOF-DATE.               00023592

       E050-EXIT.                                                       00023620
           EXIT.                                                        00023630
                                                                        00023640
                                                                        00023990
       E080-DFT-CLM-BENEFIT-CD.                                         00024000
      * THIS MODULE USES A NUMERIC BENEFIT CODE TO READ THE ELCNTL      00024001
      * FILE IN ORDER TO FIND ITS CORRESPONDING ALPHA COUNTERPART.      00024020
      * THIS PROCESSES FOR EITHER A CLAIM AND OR A DRAFT RECORD.        00024030
                                                                        00024040              
           MOVE SPACES TO CONTROL-FILE.                                 00024050
           MOVE WS-WD-COMPANY-ID     TO CF-COMPANY-ID.                  00024060
           MOVE EX-FB-LF-BENEFIT-CD  TO CF-HI-BEN-IN-REC.               00024070
122402     IF EX-FA-AH-CLAIM OR
121703        EX-FA-IU-CLAIM OR
121703        EX-FA-GP-CLAIM
                MOVE '5'            TO WS-CF-RECORD-TYPE                00024090
           ELSE                                                         00024100
                MOVE '4'            TO WS-CF-RECORD-TYPE.               00024110
           MOVE +0                  TO CF-SEQUENCE-NO.                  00024120
           MOVE WS-CF-RECORD-TYPE   TO CF-RECORD-TYPE.                  00024130
                                                                        00024140
           START ELCNTL KEY NOT < CF-CONTROL-PRIMARY                    00024141
           IF CF-STAT-1 NOT EQUAL '0'                                   00024160
               DISPLAY '*** START ERROR - ELCNTL..STATUS = ' CF-STATUS  00024161
               DISPLAY '*** START ERROR - ELCNTL..STATUS = ' CF-STATUS  00024162
                   UPON CONSOLE                                         00024190
               GO TO ABEND-PGM.                                         00024200
           READ ELCNTL NEXT RECORD                                      00024201
           IF CF-STAT-1 NOT EQUAL '0'                                   00024220
               DISPLAY '*** READ ERROR - ELCNTL..STATUS = ' CF-STATUS   00024221
               DISPLAY '*** READ ERROR - ELCNTL..STATUS = ' CF-STATUS   00024222
                   UPON CONSOLE                                         00024250
               GO TO ABEND-PGM.                                         00024260
           IF (WS-WD-COMPANY-ID NOT = CF-COMPANY-ID) OR                 00024270
              (WS-CF-RECORD-TYPE NOT = CF-RECORD-TYPE)                  00024280
                  MOVE ZEROS TO WS-BENEFIT-CD                           00024290
      * NEED DEFAULT OR MOVE ZEROS AND DEFAULT LATER                    00024300
                  GO TO E080-EXIT.                                      00024310
           PERFORM E080-DUMMY THRU E080-DUMMY-EXIT                      00024320
              VARYING SUB1 FROM 1 BY 1 UNTIL                            00024330
              ((SUB1 GREATER 8) OR                                      00024340
               (CF-BENEFIT-NUMERIC (SUB1) = EX-FB-LF-BENEFIT-CD)).      00024350
                                                                        00024360
           IF SUB1 NOT = 9                                              00024370
               MOVE CF-BENEFIT-ALPHA(SUB1) TO WS-BENEFIT-CD             00024380
           ELSE                                                         00024390
      * NEED A DEFAULT OR MOVE ZEROS AND CHECK DEFAULT LATER            00024400
      * NO CODE FOUND                                                   00024410
               MOVE ZEROS TO WS-BENEFIT-CD.                             00024420
       E080-EXIT.                                                       00024440
           EXIT.                                                        00024450

       E080-DUMMY.                                                      00024460
       E080-DUMMY-EXIT.                                                 00024470
           EXIT.                                                        00024480
                                                                        00024490
                                                                        00024500
       E090-DRAFT-PROCESS-FB.                                           00024510
      ******************************************************************00024520
      *                                                                *00024530
      * THIS MODULE MOVES FB INFORMATION TO OUTPUT AREA. IT MUST BE    *00024540
      * ACCESSED DURING THE PAYMENT (FC) PROCESSING BECAUSE THE        *00024550
      * CLAIM TYPE COULD BE DIFFERENT FOR EACH INDIVIDUAL RECORD.      *00024560
      * THE FB RECORDS WERE MOVED TO WORKING STORAGE AS THEY WERE      *00024570
      * READ IN.                                                       *00024580
      *                                                                *00024590
      ******************************************************************00024600
                                                                        00024610
           IF EX-FB-IND-GRP-TYPE = 'I'                                  00024620
               MOVE EX-FB-IND-GRP-TYPE      TO WS-INDIV-GRP-CD          00024630
           ELSE                                                         00024640
               MOVE 'G'                     TO WS-INDIV-GRP-CD.         00024650
                                                                        00024660
100518     IF PT-CLAIM-TYPE (PT-INDEX2) = 'L' OR 'O'                    00024670
               MOVE EX-FB-LF-REMAINING-AMT  TO DPW-INDEMNITY            00024680
               MOVE EX-FB-LF-ORIG-TERM      TO WS-ORIGINAL-TERM         00024690
062821         IF WS-COMPANY = 'CID' OR 'DCC' OR 'AHL' OR 'FNL'
                   PERFORM E080-DFT-CLM-BENEFIT-CD THRU E080-EXIT       00024710
               ELSE                                                     00024720
                   MOVE EX-FB-LF-BENEFIT-CD TO WS-BENEFIT-CD            00024730
           ELSE                                                         00024740
               MOVE EX-FB-AH-BENEFIT-AMT    TO DPW-INDEMNITY            00024750
               MOVE EX-FB-AH-BENEFIT-CD     TO WS-BENEFIT-CD            00024760
               MOVE EX-FB-AH-ORIG-TERM      TO WS-ORIGINAL-TERM.        00024770
                                                                        00024780
030702*    PERFORM G010-DRAFT-ACCESS-PLAN-CODE THRU G010-EXIT.          00024790
030702     MOVE WS-INDIV-GRP-CD          TO WS-PC-KEY-PT1.              00025510
030702     MOVE WS-BENEFIT-CD            TO WS-PC-KEY-PT2.              00025530
030702     MOVE SPACES                   TO WS-PC-KEY-PT3.              00025550
030702     MOVE WS-PLAN-CODE-AREA        TO DPW-PLAN-NUMBER.            00025560

           MOVE EX-FB-STATE              TO DPW-COLL-AGT-STATE.         00024810
           MOVE EX-FB-EFFECTIVE-DT       TO WS-DATE-IN-AREA             00024820
                                            WS-HOLD-CERT-EFF-DT.        00024830
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       00024840
           MOVE WS-DATE-BACK-AREA       TO DPW-ISSUE-DATE               00024850

           MOVE WS-HOLD-CERT-EFF-DT     TO DC-BIN-DATE-1                00024860
           MOVE WS-ORIGINAL-TERM        TO DC-ELAPSED-MONTHS            00024870
           MOVE '6'                     TO DC-OPTION-CODE               00024880
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA                    00024890
           IF NO-CONVERSION-ERROR                                       00024900
               MOVE  DC-BIN-DATE-2          TO  WS-HOLD-PAID-TO         00024901
               MOVE  DC-GREG-DATE-1-YMD     TO  WS-DATE-BACK-AREA-YMD   00024902
               IF WS-DATE-BACK-AREA-YY > 70                             00024903
                   MOVE 19 TO WS-DATE-BACK-AREA-CC                      00024904
               ELSE                                                     00024905
                   MOVE 20 TO WS-DATE-BACK-AREA-CC                      00024906
           ELSE                                                         00024930
               MOVE  ZERO         TO  WS-DATE-BACK-AREA.                00024931
           MOVE  WS-DATE-BACK-AREA      TO  DPW-PAID-TO.                00024932

           MOVE WS-HOLD-INCUR-DATE      TO DC-BIN-DATE-1                00024950
           MOVE WS-HOLD-PAID-TO     TO DC-BIN-DATE-2                    00024960
           MOVE '1'                 TO DC-OPTION-CODE                   00024970
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA                    00024980
           IF NO-CONVERSION-ERROR                                       00024990
               IF DC-ODD-DAYS-OVER = 0                                  00025000
                   ADD 1 TO DC-ELAPSED-MONTHS                           00025010
                   MOVE DC-ELAPSED-MONTHS  TO DPW-BENEFIT-DURATION      00025020
               ELSE                                                     00025030
                   MOVE DC-ELAPSED-MONTHS  TO DPW-BENEFIT-DURATION      00025040
           ELSE                                                         00025050
               MOVE ZEROS TO DPW-BENEFIT-DURATION.                      00025060
                                                                        00025070
       E090-EXIT.                                                       00025080
           EXIT.                                                        00025090

       S000-update-sql-table.

           if not connected-to-db
              perform S010-connect     thru S010-exit
           end-if

           move ws-wd-carrier          to ws-pmt-carrier
           move '000000'               to ws-pmt-group
           move ws-wd-state            to ws-pmt-state
           move ws-wd-account-no       to ws-pmt-account
           move ws-wd-claim-no         to ws-pmt-claim-no
           move ws-wd-cert-no          to ws-pmt-cert-no
           move zeros                  to ws-pmt-check-no
           move pt-check-no (pt-index2) to ws-pmt-check-no (4:7)

           move PT-VOID-DT(PT-INDEX2)  to dc-bin-date-1
           MOVE ' '                    TO DC-OPTION-CODE
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA
           IF NO-CONVERSION-ERROR
              move dc-greg-date-a-edit to ws-void-dt
           else
              display ' Bad void date conv ' ws-pmt-state ' '
                 ws-pmt-account ' ' ws-pmt-cert-no ' '
                 ws-pmt-claim-no
              go to s000-exit
           end-if

           move ws-hold-cert-eff-dt    to dc-bin-date-1
           MOVE ' '                    TO DC-OPTION-CODE
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA
           IF NO-CONVERSION-ERROR
              move dc-greg-date-a-edit to ws-pmt-cert-eff-dt
           else
              display ' Bad cert eff date conv ' ws-pmt-state ' '
                 ws-pmt-account ' ' ws-pmt-cert-eff-dt ' '
                 ws-pmt-claim-no
              go to s000-exit
           end-if

           display ' carrier    ' ws-pmt-carrier
           display ' state      ' ws-pmt-state
           display ' account    ' ws-pmt-account
           display ' cert no    ' ws-pmt-cert-no
           display ' claim no   ' ws-pmt-claim-no
           display ' check no   ' ws-pmt-check-no
           display ' void dt    ' ws-void-dt
           display ' crt eff dt ' ws-pmt-cert-eff-dt

070921     if ws-company = 'DCC'
070921        exec sql
070921           update DCC_CLM_PMTS_ACH
070921             set void_date = :ws-void-dt
070921             where carrier   = :ws-pmt-carrier
070921                and account  = :ws-pmt-account
070921*               and state    = :ws-pmt-state
070921                and cert_no  = :ws-pmt-cert-no
070921                and claim_no = :ws-pmt-claim-no
070921                and check_no = :ws-pmt-check-no
070921                and eff_date = :ws-pmt-cert-eff-dt
070921        end-exec
070921     else
070921        if ws-company = 'CID'
070921           exec sql
070921              update CLM_PMTS_ACH
070921                set void_date = :ws-void-dt
070921                where carrier   = :ws-pmt-carrier
070921                   and account  = :ws-pmt-account
070921*                  and state    = :ws-pmt-state
070921                   and cert_no  = :ws-pmt-cert-no
070921                   and claim_no = :ws-pmt-claim-no
070921                   and check_no = :ws-pmt-check-no
070921                   and eff_date = :ws-pmt-cert-eff-dt
070921           end-exec
070921        end-if
070921     end-if

013017     if sqlcode not = 0
013017        display "Error: cannot update record   "
013017        move sqlcode             to ws-sql-code
013017        move ws-sql-code         to ws-dis-sql-code
013017        display ' sqlcode ' ws-dis-sql-code
013017        display sqlerrmc
013017        perform abend-pgm
013017     end-if
013017
           .
       S000-exit.
           exit.

013017 S010-connect.
013017
013017     display ' about to connect to Logic '
013017

      ****  The below code is for when the db has been
      ****  converted to sql server 2016
           evaluate ws-kix-myenv
              when 'cid1p'
                 move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
      *       when 'mdoff'
      *          move '//hov-tstdb01.cso.local:55330;'
      *                                to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate
		  
      ****  The below code is for when the db is still
      ****  on sql server 2008 R2

           move 'Logic'                to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc

013017 
013017      if sqlcode not = 0
013017         display "Error: cannot connect to Logic"
013017         move sqlcode             to ws-sql-code
013017         move ws-sql-code         to ws-dis-sql-code
013017         display ' sqlcode ' ws-dis-sql-code
013017         display sqlerrmc
013017         perform abend-pgm
013017      end-if
013017
013017     set connected-to-db to true
013017
013017     .
013017 S010-exit.
013017     exit.

       S050-finish-up.

           if not connected-to-db
              go to s050-exit
           end-if

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
       S050-exit.
           exit.

       G000-BREAK-CITY-STATE.                                           00025110
      * THIS MODULE TAKES THE CITY-STATE (PIC X(30)) AND BREAKS         00025120
      * INTO SEPARATE FIELDS---CITY (PIC X(20)) AND STATE (PIC X(2))    00025130
091808***THERE ARE SOME ADDRESSES THAT HAVE THE STATE SPELLED OUT. 
091808***ADDED LINES TO STORE THE INDEXES FOR THE BEGINNNIG AND END OF
091808***THE STATE PART OF THE ADDRESS.      
           MOVE SPACES TO WS-CITY-FOUND.                                00025140
           MOVE SPACES TO WS-STATE-FOUND.                               00025150
091808     MOVE SPACES TO WS-STATE-TEMP-NAME.           
                                                                        00025160
           SET CSW-INDEX TO 30.                                         00025170
       7551-CONTINUE.                                                   00025180
           IF WS-CSW(CSW-INDEX) = SPACE OR = '.'                        00025190
               SET CSW-INDEX DOWN BY 1                                  00025200
               GO TO 7551-CONTINUE.                                     00025210
           MOVE WS-CSW(CSW-INDEX) TO WS-STATE-2.                        00025220
091808     SET WS-SAVE-ST-IDX-END TO CSW-INDEX.
           SET CSW-INDEX DOWN BY 1.                                     00025230
           IF WS-CSW(CSW-INDEX) = '.'                                   00025240
               SET CSW-INDEX DOWN BY 1.                                 00025250
           MOVE WS-CSW(CSW-INDEX) TO WS-STATE-1.                        00025260
       7552-CONTINUE.                                                   00025270
           SET CSW-INDEX DOWN BY 1.                                     00025280
091808     IF WS-CSW(CSW-INDEX) NOT = ' ' AND ','
091808         GO TO 7552-CONTINUE.
091808     SET WS-SAVE-ST-IDX-BEG TO CSW-INDEX.
091808 7552-X-CONTINUE.
091808     SET CSW-INDEX DOWN BY 1.
           IF WS-CSW(CSW-INDEX) = ' ' OR ','                            00025290
091808         GO TO 7552-X-CONTINUE.                                     00025300
           SET CTY-INDEX TO 1.                                          00025310
           SET CSW-INDEX2 TO 1.                                         00025320
       7553-CONTINUE.                                                   00025330
           IF CTY-INDEX > 19                                            00025340
               GO TO G000-EXIT.                                         00025350
           IF CSW-INDEX2 NOT > CSW-INDEX                                00025360
               MOVE WS-CSW(CSW-INDEX2)  TO WS-CTY(CTY-INDEX)            00025370
               SET CSW-INDEX2 UP BY 1                                   00025380
               SET CTY-INDEX UP BY 1                                    00025390
               GO TO 7553-CONTINUE.                                     00025400
                                                                        00025410
       G000-EXIT.                                                       00025420
           EXIT.                                                        00025430
                                                                        00025440
091808 G100-CHECK-STATE-CODE.
091808
091808    COMPUTE WS-STATE-TEMP-LEN = WS-SAVE-ST-IDX-END -
091808                     WS-SAVE-ST-IDX-BEG.
091808    IF WS-STATE-TEMP-LEN = 2
091808       GO TO G100-EXIT
091808    END-IF.
091808
091808    MOVE WS-CITY-STATE-WORK (WS-SAVE-ST-IDX-BEG + 1: 
091808         WS-STATE-TEMP-LEN) TO WS-STATE-TEMP-NAME.
091808
091808    SEARCH ALL STATE-TABLE
091808         AT END DISPLAY 'INVALID STATE NAME: ' 
091808              WS-STATE-TEMP-NAME
091808         WHEN ST-NAME (ST-INDEX) = WS-STATE-TEMP-NAME
091808           MOVE ST-STATE (ST-INDEX) TO WS-STATE-FOUND
091808    END-SEARCH.
091808
091808 G100-EXIT.
091808     EXIT.                                                                        
                                                                        00025790
       CONVERT-DATE.                                                    00025800
      ******************************************************************00025801
      * CHANGES BINARY TO GREG                                         *00025802
      ******************************************************************00025803
           MOVE  WS-DATE-IN-AREA  TO  DC-BIN-DATE-1.                    00025804
           MOVE  SPACE  TO  DC-OPTION-CODE.                             00025805
           CALL 'ELDATCX'        USING DATE-CONVERSION-DATA.            00025806
           IF NO-CONVERSION-ERROR                                       00025850
               MOVE  DC-GREG-DATE-1-YMD     TO  WS-DATE-BACK-AREA-YMD   00025851
               IF WS-DATE-BACK-AREA-YY > 70                             00025852
                   MOVE 19 TO WS-DATE-BACK-AREA-CC                      00025853
               ELSE                                                     00025854
                   MOVE 20 TO WS-DATE-BACK-AREA-CC                      00025855
           ELSE                                                         00025870
               MOVE  ZERO         TO  WS-DATE-BACK-AREA.                00025871
                                                                        00025910
       CONVERT-EXIT.                                                    00025911
               EXIT.                                                    00025912
                                                                        00026020
       B030-CLOSE-FILES.                                                00026030

           DISPLAY 'NUMBER OF CLAIMS WRITTEN=> ' CLAIM-ENTRIES-OUT.     00026040
           DISPLAY 'NUMBER OF DRAFTS WRITTEN=> ' DRAFT-ENTRIES-OUT.     00026050

           CLOSE ELBENE.                                                00026091
           IF BENE-STATUS NOT = '00'                                    00026110
               DISPLAY 'ERROR-CLOSE--ELBENE FILE--CODE => ' BENE-STATUS 00026111
               GO TO ABEND-PGM.                                         00026130

           CLOSE ELCNTL.                                                00026131
           IF CF-STATUS NOT = '00'                                      00026150
               DISPLAY 'ERROR-CLOSE--ELCNTL FILE--CODE => ' CF-STATUS   00026151
               GO TO ABEND-PGM.                                         00026170

           CLOSE EXTRACT-FILE                                           00026180
                 LG-CLM-ACT                                             00026190
                 LG-DFT-ACT.                                            00026200
                                                                        00026240
       B030-EXIT.                                                       00026250
           EXIT.                                                        00026260
                                                                        00026270
       ABEND-PGM SECTION.                                               00026280
           DIVIDE WS-ZERO BY WS-ZERO GIVING WS-ZERO.                    00026290
       ABEND-EXIT.                                                      00026300
