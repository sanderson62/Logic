       IDENTIFICATION DIVISION.
       PROGRAM-ID.   NSREQLTR.
      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.
      *REMARKS.    EXECUTED FROM INDEX.HTML
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
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
061421* 061421  CR2017031500001  PEMA  Update to CCM8
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       working-storage section.
       01  DFH-START PIC X(04).
       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
061421 01  filler.
061421     05  a1                      pic s999 comp-3 value +0.
061421     05  a2                      pic s999 comp-3 value +0.
061421     05  m1                      pic s999 comp-3 value +0.
061421     05  v1                      pic s999 comp-3 value +0.
061421     05  v2                      pic s999 comp-3 value +0.
061421     05  WS-WORK-FIELD           PIC X(90)    VALUE SPACES.
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).
      ************************************************
      * commarea passed to the business logic
      ************************************************
       01 srch-commarea.
      *                                copy ELCLTRSPI.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 121802    2009122800001  PEMA  NEW COPYBOOK
      ******************************************************************
      ****************************************
      *  commarea for NaperSoft On Demand Claim letters
      *  (business logic input & output)
      ****************************************
           03  BL-INPUT.
               05  BL-CARRIER          PIC X.
               05  BL-CLAIM-NO         PIC X(7).
               05  BL-CERT-NO          PIC X(11).
               05  BL-LETTER-ID        PIC XXXX.
               05  BL-FOLLOW-UP-DT     PIC X(10).
               05  BL-RESEND-DT        PIC X(10).
               05  BL-NO-OF-COPIES     PIC 99.
               05  BL-PROC-ID          PIC XXXX.
               05  BL-COMP-ID          PIC XXX.
               05  BL-PRINT-NOW-SW     PIC X.
               05  BL-ENC-CD           PIC XXX.
               05  BL-ARCHIVE-NO       PIC 9(8).
               05  BL-REGARDING        PIC X(70).
           03  BL-OUTPUT.
               05  BL-STATUS                   PIC X.
                   88  BL-OK                      VALUE "P".
                   88  BL-FAIL                  VALUE "F".
               05  BL-MESSAGE          PIC X(50).
           03  BL-RECORD-PASSED-DATA   PIC X(2500).
       01  INPUT-FROM-FORM.
           05  IFF-CARRIER             PIC X.
           05  IFF-CLAIM-NO            PIC X(7).
           05  IFF-CERT-NO             PIC X(11).
           05  IFF-LETTER-ID           PIC XXXX.
           05  IFF-FOLLOW-UP-DT        PIC X(10).
           05  IFF-RESEND-DT           PIC X(10).
           05  IFF-NO-OF-COPIES        PIC 99.
           05  IFF-PROC-ID             PIC XXXX.
           05  IFF-COMP-ID             PIC XXX.
           05  IFF-PRINT-NOW-SW        PIC X.
           05  IFF-ENC-CD              PIC XXX.
           05  IFF-REGARDING           PIC X(70).
      ************************************
      * fields used to read web data
      ************************************
       01  w-form-name       pic x(80).
       01  w-form-value      pic x(80).
       01  w-form-name-len   pic s9(8) comp.
       01  w-form-value-len  pic s9(8) comp.
       01  w-resp            pic s9(8) comp.
       01  w-doctoken        pic x(16).
      * COMP ID TPE REGION   GROUP NAME       HOST          HTTP PORT
      *
061421*  CID      CID1P      BatchClaims     sdv-nsft01       7001
061421*  CID      MDOFF      BatchClaims     hov-nsft01       7003
061421*  CID      CID1T      BatchClaims     hov-nsft01       6002
061421*  CID      PAUL       BatchClaims     hov-nsft01       5002
061421*  CID      TONY       BatchClaims     hov-nsft01       6003
061421*  DCC      CID1P      BatchDCCClaims  sdv-nsft01       7001
061421*  DCC      MDOFF      BatchDCCClaims  hov-nsft01       7003
061421*  DCC      CID1T      BatchDCCClaims  hov-nsft01       6002
061421*  DCC      PAUL       BatchDCCClaims  hov-nsft01       5002
061421*  DCC      TONY       BatchDCCClaims  hov-nsft01       6003
061421*  AHL      CID1P      BatchAHLClaims  sdv-nsft01       7001
061421*  AHL      MDOFF      BatchAHLClaims  hov-nsft01       7003
061421*  AHL      CID1T      BatchAHLClaims  hov-nsft01       6002
061421*  AHL      PAUL       BatchAHLClaims  hov-nsft01       5002
061421*  AHL      TONY       BatchAHLClaims  hov-nsft01       6003
061421*  AHL      AHLTST     BatchAHLClaims  hov-nsft01       6007
061421*  FNL      CID1P      BatchFNLClaims  sdv-nsft01       7001
061421*  FNL      MDOFF      BatchFNLClaims  hov-nsft01       7003
061421*  FNL      CID1T      BatchFNLClaims  hov-nsft01       6002
061421*  FNL      PAUL       BatchFNLClaims  hov-nsft01       5002
061421*  FNL      TONY       BatchFNLClaims  hov-nsft01       6003
061421*  FNL      AHLTST     BatchFNLClaims  hov-nsft01       6007
      ******************************************
      * symbol list text for PEMHDR template
      ******************************************
       01  WS-PROD-CID-PEMHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
           05  F                       PIC X(11) VALUE '&GROUPNAME='.
           05  F                       PIC X(11) VALUE 'BatchClaims'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
       01  WS-PROD-DCC-PEMHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
           05  F                       PIC X(11) VALUE "&GROUPNAME=".
           05  F                       PIC X(14) VALUE 'BatchDCCClaims'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
020816 01  WS-PROD-VPP-PEMHDR.
020816     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
020816     05  F                       PIC X(11) VALUE "&GROUPNAME=".
020816     05  F                       PIC X(14) VALUE 'BatchVPPClaims'.
020816     05  F                       PIC X(7)  VALUE '&UNAME='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
020816     05  F                       PIC X(11) VALUE '&UPASSWORD='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
022212 01  WS-PROD-AHL-PEMHDR.
022212     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
022212     05  F                       PIC X(11) VALUE "&GROUPNAME=".
022212     05  F                       PIC X(14) VALUE 'BatchAHLClaims'.
022212     05  F                       PIC X(7)  VALUE '&UNAME='.
022212     05  F                       PIC X(8)  VALUE 'batchjob'.
022212     05  F                       PIC X(11) VALUE '&UPASSWORD='.
022212     05  F                       PIC X(8)  VALUE 'batchjob'.
022212
061421 01  WS-PROD-FNL-PEMHDR.
061421     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
061421     05  F                       PIC X(11) VALUE '&GROUPNAME='.
061421     05  F                       PIC X(14) VALUE 'BatchFNLClaims'.
061421     05  F                       PIC X(7)  VALUE '&UNAME='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
061421     05  F                       PIC X(11) VALUE '&UPASSWORD='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
       01  WS-TEST-CID-PEMHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
           05  F                       PIC X(11) VALUE '&GROUPNAME='.
           05  F                       PIC X(11) VALUE 'BatchClaims'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
       01  WS-TEST-DCC-PEMHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
           05  F                       PIC X(11) VALUE "&GROUPNAME=".
           05  F                       PIC X(14) VALUE 'BatchDCCClaims'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
020816 01  WS-TEST-VPP-PEMHDR.
020816     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
020816     05  F                       PIC X(11) VALUE "&GROUPNAME=".
020816     05  F                       PIC X(14) VALUE 'BatchVPPClaims'.
020816     05  F                       PIC X(7)  VALUE '&UNAME='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
020816     05  F                       PIC X(11) VALUE '&UPASSWORD='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
022212 01  WS-TEST-AHL-PEMHDR.
022212     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
022212     05  F                       PIC X(11) VALUE "&GROUPNAME=".
022212     05  F                       PIC X(14) VALUE 'BatchAHLClaims'.
022212     05  F                       PIC X(7)  VALUE '&UNAME='.
022212     05  F                       PIC X(8)  VALUE 'batchjob'.
022212     05  F                       PIC X(11) VALUE '&UPASSWORD='.
022212     05  F                       PIC X(8)  VALUE 'batchjob'.
061421 01  WS-TEST-FNL-PEMHDR.
061421     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
061421     05  F                       PIC X(11) VALUE '&GROUPNAME='.
061421     05  F                       PIC X(14) VALUE 'BatchFNLClaims'.
061421     05  F                       PIC X(7)  VALUE '&UNAME='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
061421     05  F                       PIC X(11) VALUE '&UPASSWORD='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
      ******************************************
      * symbol list text for PEMFTR template
      ******************************************
061421 01  ws-sl-var-length pic s9(8) comp value +33.
       01  WS-VAR-SLUNIKIX.
           05  FILLER                  PIC X(09) VALUE "HOSTINFO=".
           05  WS-SL-HOST-INFO         PIC X(09) VALUE 'slunikix:'.
           05  WS-SL-PORT              PIC XXXX  VALUE '7001'.
061421     05  WS-SL-REST              PIC X(200) VALUE SPACES.
061421 01  ws-lt-var-length pic s9(8) comp value +34.
       01  WS-VAR-LOGICTEST.
           05  FILLER                  PIC X(09) VALUE "HOSTINFO=".
           05  WS-LT-HOST-INFO         PIC X(10) VALUE 'logictest:'.
           05  WS-LT-PORT              PIC XXXX  VALUE '6002'.
061421     05  WS-LT-REST              PIC X(200) VALUE SPACES.
061421 01  finished-string.
061421     05  FILLER                   PIC X(11)  VALUE "&URLVARLST=".
061421     05  var-string               pic x(200) value spaces.
       01 WS-TEST-TESTER.
          05  FILLER                   PIC X(11)  VALUE "&URLVARLST=".
          05  WS-KEY.
              10  OT-CARRIER           PIC X.
              10  OT-CLMNO             PIC X(7).
              10  OT-CRTNO             PIC X(11).
              10  OT-LETTER-ID         PIC X(4).
              10  OT-FOLLOW-UP-DT      PIC X(10).
              10  OT-RESEND-DT         PIC X(10).
              10  OT-NO-OF-COPIES      PIC 99.
              10  OT-PROC-ID           PIC XXXX.
              10  OT-COMP-ID           PIC XXX.
              10  OT-PRINT-NOW-SW      PIC X.
              10  OT-ENC-CD            PIC XXX.
              10  OT-ARCHIVE-NO        PIC 9(08).
              10  OT-REGARDING         PIC X(70).
       01  WS-ELMSTR-KEY.
           05  WS-ELMSTR-COMPANY-CD    PIC X.
           05  WS-ELMSTR-CARRIER       PIC X.
           05  WS-ELMSTR-CLAIM-NO      PIC X(7).
           05  WS-ELMSTR-CERT-NO       PIC X(11).
      *                                COPY ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
031213* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
052614*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
100518*            88  OTHER-CLAIM            VALUE 'O'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
051414         16  FILLER REDEFINES CL-MICROFILM-NO.
051414             20  CL-BENEFIT-PERIOD   PIC 99.
051414             20  FILLER              PIC X(8).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
031213         16  CL-CRITICAL-PERIOD      PIC 99.
031213*        16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
081817         16  CL-NO-OF-EXTENSIONS     PIC 99.
081817         16  filler                  pic x(3).
      *        16  CL-CRIT-PER-RECURRENT   PIC X.
      *        16  CL-CRIT-PER-RTW-MOS     PIC 99.
      *        16  CL-RTW-DT               PIC XX.
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD     12  CL-YESNOSW                  PIC X.
031213     12  CL-ACCIDENT-CLAIM-SW        PIC X.
031213         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
031213         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
031213         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
051414     12  cl-insured-type             pic x.
051414         88  cl-claim-on-primary         value 'P'.
051414         88  cl-claim-on-co-borrower     value 'C'.
031213     12  cl-benefit-expiration-dt    PIC XX.
       01 output-msg.
          05 filler              pic x(4) value "MSG=".
          05 out-msg-text        pic x(50).
       01  MISC.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPREC                  VALUE +14.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
      *****************************************
      * symbol list for the PEMBOD template
      *****************************************
      *                                COPY NSCVARS.
031912******************************************************************
031912*                   C H A N G E   L O G
031912*
031912* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031912*-----------------------------------------------------------------
031912*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031912* EFFECTIVE    NUMBER
031912*-----------------------------------------------------------------
031912* 031912    2011110200002  AJRA  AHL CLAIM NUM
013013* 013013    2012110080002  AJRA  ADD ACCOUNT GPCD
031116* 031116    2015110400001  TANA  ADD EL150D FIELDS
061217* 061217    2017060900001  TANA  INCREASE ATTACHMENTS FIELD SIZE
071719* 071719    2019011600010  TANA  ADD VERIF CODE
031912******************************************************************
       01 NAPER-OUTPUT-DATA.
          05 FILLER              PIC X(6) VALUE "TEMPL=".
          05 OUT-TEMP            PIC X(4).
          05 FILLER              PIC X(8) VALUE "&LETTER=".
          05 OUT-LETTER          PIC X(4).
          05 FILLER              PIC X(6) VALUE "&CARR=".
          05 OUT-CARR            PIC X.
          05 FILLER              PIC X(7) VALUE "&CLMNO=".
          05 OUT-CLMNO           PIC X(7).
          05 FILLER              PIC X(7) VALUE "&CRTNO=".
          05 OUT-CRTNO           PIC X(11).
          05 FILLER              PIC X(7) VALUE "&ACTNO=".
          05 OUT-ACTNO           PIC X(10).
          05 FILLER              PIC X(8) VALUE "&ILNAME=".
          05 OUT-ILNAME          PIC X(15).
          05 FILLER              PIC X(8) VALUE "&IFNAME=".
          05 OUT-IFNAME          PIC X(10).
          05 FILLER              PIC X(7) VALUE "&SEXCD=".
          05 OUT-SEX-CD          PIC X.
          05 FILLER              PIC X(5) VALUE "&SSN=".
          05 OUT-SSN             PIC X(11).
          05 FILLER              PIC X(9) VALUE "&CLMSTAT=".
          05 OUT-CLMSTAT         PIC X.
          05 FILLER              PIC X(8) VALUE "&CLMTYP=".
          05 OUT-CLMTYPE         PIC X.
          05 FILLER              PIC X(7) VALUE "&ESTDT=".
          05 OUT-EST-DT          PIC X(21).
          05 FILLER              PIC X(7) VALUE "&INCDT=".
          05 OUT-INC-DT          PIC X(21).
          05 FILLER              PIC X(7) VALUE "&RPTDT=".
          05 OUT-RPT-DT          PIC X(21).
          05 FILLER              PIC X(10) VALUE "&PDTHRUDT=".
          05 OUT-PD-THRU-DT      PIC X(21).
          05 FILLER              PIC X(11) VALUE "&FORMDUEDT=".
          05 OUT-FORM-DUE-DT     PIC X(21).
          05 FILLER              PIC X(8) VALUE "&LPMTDT=".
          05 OUT-LST-PMT-DT      PIC X(21).
          05 FILLER              PIC X(8) VALUE "&NOPMTS=".
          05 OUT-NO-OF-PMTS      PIC X(10).
          05 FILLER              PIC X(9) VALUE "&TOTPAID=".
          05 OUT-TOT-PAID        PIC 9999999.99.
          05 FILLER              PIC X(8) VALUE "&LPDAMT=".
          05 OUT-LST-PD-AMT      PIC 9999999.99.
          05 FILLER              PIC X(9) VALUE "&ADDRCNT=".
          05 OUT-ADDR-CNT        PIC 9.
          05 FILLER              PIC X(7) VALUE "&CRTST=".
          05 OUT-CRT-ST          PIC XX.
          05 FILLER              PIC X(10) VALUE "&CERTACCT=".
          05 OUT-CERTACCT        PIC X(10).
          05 FILLER              PIC X(7) VALUE "&EFFDT=".
          05 OUT-EFF-DT          PIC X(21).
          05 FILLER              PIC X(8) VALUE "&LMNTDT=".
          05 OUT-LST-MAINT-DT    PIC X(21).
          05 FILLER              PIC X(10) VALUE "&LMNTUSER=".
          05 OUT-LST-MAINT-USER  PIC X(10).
          05 FILLER              PIC X(10) VALUE "&LMNTTYPE=".
          05 OUT-LST-MAINT-TYPE  PIC X(10).
          05 FILLER              PIC X(6) VALUE "&DIAG=".
          05 OUT-DIAG            PIC X(66).
          05 FILLER              PIC X(7) VALUE "&EXPDT=".
          05 OUT-EXP-DT          PIC X(21).
          05 FILLER              PIC X(12) VALUE "&LCLSREASON=".
          05 OUT-LST-CLS-REA     PIC X(10).
          05 FILLER              PIC X(8) VALUE "&BRTHDT=".
          05 OUT-BIRTH-DT        PIC X(21).
          05 FILLER              PIC X(6) VALUE "&TERM=".
          05 OUT-TERM            PIC 999.
          05 FILLER              PIC X(7) VALUE "&BENCD=".
          05 OUT-BEN-CD          PIC XX.
          05 FILLER              PIC X(8) VALUE "&BENAMT=".
          05 OUT-BENAMT          PIC 9999999.99.
          05 FILLER              PIC X(9) VALUE "&CRITPER=".
          05 OUT-CRIT-PER        PIC 999.
          05 FILLER              PIC X(9) VALUE "&WAITPER=".
          05 OUT-WAIT-PER        PIC XX.
          05 FILLER              PIC X(11)  VALUE "&LFINTRATE=".
          05 OUT-LF-INT-RATE     PIC 99.99999.
          05 FILLER              PIC X(10) VALUE "&AUTOPYDT=".
          05 OUT-AUTO-PY-DT      PIC X(21).
          05 FILLER              PIC X(12) VALUE "&AUTOSCHEND=".
          05 OUT-AUTO-SCHD-END   PIC X(21).
          05 FILLER              PIC X(8) VALUE "&LOANNO=".
          05 OUT-LOAN-NO         PIC X(25).
          05 FILLER              PIC X(5) VALUE "&APR=".
          05 OUT-APR             PIC 99.99999.
          05 FILLER              PIC X(8) VALUE "&LACTDT=".
          05 OUT-LST-ACT-DT      PIC X(21).
          05 FILLER              PIC X(7) VALUE "&ACTDT=".
          05 OUT-ACT-DT          PIC X(21).
          05 FILLER              PIC X(10) VALUE "&LACTTYPE=".
          05 OUT-LST-ACT-TYPE    PIC X(15).
          05 FILLER              PIC X(9) VALUE "&ACTTYPE=".
          05 OUT-ACT-TYPE        PIC X(15).
          05 FILLER              PIC X(6) VALUE "&FORM=".
          05 OUT-FORM            PIC X(4).
          05 FILLER              PIC X(9) VALUE "&INSNAME=".
          05 OUT-INS-NAME        PIC X(36).
          05 FILLER              PIC X(10) VALUE "&INSADDR1=".
          05 OUT-INS-ADDR1       PIC X(36).
          05 FILLER              PIC X(10) VALUE "&INSADDR2=".
          05 OUT-INS-ADDR2       PIC X(36).
          05 FILLER              PIC X(9) VALUE "&INSCITY=".
          05 OUT-INS-CITY        PIC X(28).
          05 FILLER              PIC X(10) VALUE "&INSSTATE=".
          05 OUT-INS-STATE       PIC XX.
          05 FILLER              PIC X(8) VALUE "&INSZIP=".
          05 OUT-INS-ZIP         PIC X(10).
          05 FILLER              PIC X(10) VALUE "&INSPHONE=".
          05 OUT-INS-PHONE       PIC X(13).
          05 FILLER              PIC X(10) VALUE "&BENENAME=".
          05 OUT-BEN-NAME        PIC X(32).
          05 FILLER              PIC X(11) VALUE "&BENEADDR1=".
          05 OUT-BEN-ADDR1       PIC X(36).
          05 FILLER              PIC X(11) VALUE "&BENEADDR2=".
          05 OUT-BEN-ADDR2       PIC X(32).
          05 FILLER              PIC X(10) VALUE "&BENECITY=".
          05 OUT-BEN-CITY        PIC X(28).
          05 FILLER              PIC X(11) VALUE "&BENESTATE=".
          05 OUT-BEN-STATE       PIC XX.
          05 FILLER              PIC X(9) VALUE "&BENEZIP=".
          05 OUT-BEN-ZIP         PIC X(10).
          05 FILLER              PIC X(11) VALUE "&BENEPHONE=".
          05 OUT-BEN-PHONE       PIC X(13).
          05 FILLER              PIC X(8) VALUE "&OANAME=".
          05 OUT-ORIG-NAME       PIC X(32).
          05 FILLER              PIC X(10) VALUE "&ACCTNAME=".
          05 OUT-ACCT-NAME       PIC X(32).
          05 FILLER              PIC X(11) VALUE "&ACCTADDR1=".
          05 OUT-ACCT-ADDR1      PIC X(36).
          05 FILLER              PIC X(11) VALUE "&ACCTADDR2=".
          05 OUT-ACCT-ADDR2      PIC X(32).
          05 FILLER              PIC X(10) VALUE "&ACCTCITY=".
          05 OUT-ACCT-CITY       PIC X(30).
          05 FILLER              PIC X(11) VALUE "&ACCTSTATE=".
          05 OUT-ACCT-STATE      PIC XX.
          05 FILLER              PIC X(9)  VALUE "&ACCTZIP=".
          05 OUT-ACCT-ZIP        PIC X(10).
          05 FILLER              PIC X(11) VALUE "&ACCTPHONE=".
          05 OUT-ACCT-PHONE      PIC X(13).
          05 FILLER              PIC X(7) VALUE "&ENCCD=".
          05 OUT-ENC-CODE        PIC XXX.
          05 FILLER              PIC X(7) VALUE "&ENCST=".
          05 OUT-ENC-ST          PIC XX.
          05 FILLER              PIC X(8) VALUE "&ARCHNO=".
          05 OUT-ARCHNO          PIC 999999999.
          05 FILLER              PIC X(11) VALUE "&FLTRPRTDT=".
          05 OUT-1ST-LTR-PRT-DT  PIC X(21).
          05 FILLER              PIC X(11) VALUE "&NEXTDUEDT=".
          05 OUT-NEXT-DUE-DT     PIC X(21).
          05 FILLER              PIC X(8) VALUE "&JLNAME=".
          05 OUT-JLNAME          PIC X(15).
          05 FILLER              PIC X(8) VALUE "&JFNAME=".
          05 OUT-JFNAME          PIC X(10).
          05 FILLER              PIC X(9)  VALUE "&ENCLINE=".
          05 OUT-ENCLINE         PIC X(100).
          05 FILLER              PIC X(9) VALUE "&ENCATTS=".
061217    05 OUT-ENCATTS         PIC X(255).
          05 FILLER              PIC X(10) VALUE "&OUTSTACK=".
          05 OUT-OUTSTACK        PIC X.
          05 FILLER              PIC X(8) VALUE "&PROCID=".
          05 OUT-PROCID          PIC XXXX.
          05 FILLER              PIC X(8) VALUE "&COMPID=".
          05 OUT-COMPID          PIC XXX.
          05 FILLER              PIC X(8) VALUE "&PRTNOW=".
          05 OUT-PRINT-NOW-SW    PIC X.
          05 FILLER              PIC X(11) VALUE "&CYCLEDATE=".
          05 OUT-CYCLE-DATE      PIC X(8).
          05 FILLER              PIC X(11) VALUE "&ENCSTNAME=".
          05 OUT-ENC-ST-NAME     PIC XX.
          05 FILLER              PIC X(10) VALUE "&CARRIDLU=".
          05 OUT-CARR-LU         PIC X.
031912    05 FILLER              PIC X(5)  VALUE "&CCN=".
031912    05 OUT-AHL-CLAIM-NO    PIC X(9).
013013    05 FILLER              PIC X(4) VALUE "&GP=".
013013    05 OUT-ACCT-GPCD       PIC 99.
031116    05 FILLER              PIC X(9) VALUE "&MAXBENS=".
031116    05 OUT-MAX-BENS        PIC 9(3).
031116    05 FILLER              PIC X(10) VALUE "&PAIDBENS=".
031116    05 OUT-PAID-BENS       PIC 9(3).
031116    05 FILLER              PIC X(9) VALUE "&REMBENS=".
031116    05 OUT-REM-BENS        PIC 9(3).
031116    05 FILLER              PIC X(9) VALUE "&EXCLPER=".
031116    05 OUT-EXCL-PER        PIC 9(3).
031116    05 FILLER              PIC X(8) VALUE "&RECMOS=".
031116    05 OUT-REC-MOS         PIC 9(2).
031116    05 FILLER              PIC X(8) VALUE "&INSTYP=".
031116    05 OUT-INS-TYP         PIC X(4).
031116    05 FILLER              PIC X(8) VALUE "&BENPER=".
031116    05 OUT-BEN-PER         PIC 9(2).
031116    05 FILLER              PIC X(7) VALUE "&ACCSW=".
031116    05 OUT-ACC-SW          PIC X.
071719    05 FILLER              PIC X(7) VALUE "&VERCD=".
071719    05 OUT-VER-CD          PIC X(5).
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
       01  DFHCOMMAREA       PIC X(01).
       01  var  pic x(30).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'NSREQLTR' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
      *********************
      * Receive web input
      *********************
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00'
PEMTST*       DISPLAY '  KIXSYS = ' var (1:env-var-len)
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
PEMTST*       DISPLAY ' WS KIX SYS ' WS-KIXSYS
PEMTST*       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if
           
      * exec cics web
      *       startbr formfield resp(w-resp)
      *     end-exec.
      *    MOVE 'X(f                   &  N#00000891' TO DFHEIV0
           MOVE X'582866202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303030383931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
            perform read-form thru read-form-exit
               until w-resp not = 0 .
      *   dfhresp(normal)
            
      * exec cics web
      *       endbr formfield
      *     end-exec.
      *    MOVE 'X,f                   #   #00000896' TO DFHEIV0
           MOVE X'582C66202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303030383936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE IFF-CARRIER             TO OT-CARRIER
           MOVE IFF-CLAIM-NO            TO OT-CLMNO
           MOVE IFF-CERT-NO             TO OT-CRTNO
           MOVE IFF-LETTER-ID           TO OT-LETTER-ID
           MOVE IFF-FOLLOW-UP-DT        TO OT-FOLLOW-UP-DT
           MOVE IFF-RESEND-DT           TO OT-RESEND-DT
           MOVE IFF-NO-OF-COPIES        TO OT-NO-OF-COPIES
           MOVE IFF-PROC-ID             TO OT-PROC-ID
           MOVE IFF-COMP-ID             TO OT-COMP-ID
           MOVE IFF-PRINT-NOW-SW        TO OT-PRINT-NOW-SW
           MOVE IFF-ENC-CD              TO OT-ENC-CD
           MOVE ZEROS                   TO OT-ARCHIVE-NO
           MOVE IFF-REGARDING           TO OT-REGARDING
PEMTST*    DISPLAY ' INPUT FORM ' WS-KEY
      *****************************************
      * Invoke the LETTER business logic
      *****************************************
           
      * exec cics link
      *       program('NSRLTRBL')
      *       commarea(srch-commarea)
      *    end-exec.
           MOVE LENGTH OF
            srch-commarea
             TO DFHEIV11
           MOVE 'NSRLTRBL' TO DFHEIV1
      *    MOVE '."C                   (   #00000916' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 srch-commarea, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF BL-OK
PEMTST*       DISPLAY ' BL OK ' BL-ARCHIVE-NO
              MOVE BL-RECORD-PASSED-DATA
                                       TO NAPER-OUTPUT-DATA
              MOVE BL-ARCHIVE-NO          TO OT-ARCHIVE-NO
           END-IF
061421     move +1 to v2
061421     perform varying v1 from +1 by +1 until v1 > +1
061421        if ot-carrier (v1:1) <> spaces
061421           move ot-carrier(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +7
061421        if ot-clmno (v1:1) <> spaces
061421           move ot-clmno(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +11
061421        if ot-crtno (v1:1) <> spaces
061421           move ot-crtno(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +4
061421        if ot-letter-id (v1:1) <> spaces
061421           move ot-letter-id(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +10
061421        if ot-follow-up-dt (v1:1) <> spaces
061421           move ot-follow-up-dt(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +10
061421        if ot-resend-dt (v1:1) <> spaces
061421           move ot-resend-dt(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +2
061421        if ot-no-of-copies (v1:1) <> spaces
061421           move ot-no-of-copies(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +4
061421        if ot-proc-id (v1:1) <> spaces
061421           move ot-proc-id(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +3
061421        if ot-comp-id (v1:1) <> spaces
061421           move ot-comp-id(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +1
061421        if ot-print-now-sw (v1:1) <> spaces
061421           move ot-print-now-sw(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +3
061421        if ot-enc-cd (v1:1) <> spaces
061421           move ot-enc-cd(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +8
061421        if ot-archive-no (v1:1) <> spaces
061421           move ot-archive-no(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     perform varying m1 from +70 by -1 until
061421        (m1 < +1)
061421        or (ot-regarding(m1:1) <> spaces)
061421     end-perform
061421
061421     move +1 to a2
061421     if m1 > +0
061421        MOVE SPACES              TO WS-WORK-FIELD
061421        perform varying a1 from +1 by +1 until a1 > m1
061421           if ot-regarding(a1:1) = ' '
061421              move '~'           to ws-work-field(a2:1)
061421              add +1 to a2
061421           else
061421              move ot-regarding(a1:1) to ws-work-field(a2:1)
061421              add +1 to a2
061421           end-if
061421        end-perform
061421     end-if
061421
061421     move ws-work-field to ot-regarding
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +70
061421        if ot-regarding (v1:1) <> spaces and '&'
061421           move ot-regarding(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421     move finished-string        to ws-lt-rest
           evaluate ws-kix-myenv
              when 'cid1p'
061421           move finished-string  to ws-sl-rest
                 move '7001'           to ws-sl-port
              when 'mdoff'
061421           move finished-string  to ws-sl-rest
                 move '7003'           to ws-sl-port
              when 'paul'
                 move '5002'           to ws-lt-port
022212        when 'tony'
022212           move '6003'           to ws-lt-port
022212        when 'ahltst'
022212           move '6007'           to ws-lt-port
              when other
                 move '6002'           to ws-lt-port
           end-evaluate
      *    MOVE X'04'                  TO WS-ELMSTR-COMPANY-CD
      *    MOVE IFF-CARRIER            TO WS-ELMSTR-CARRIER
      *    MOVE IFF-CLAIM-NO           TO WS-ELMSTR-CLAIM-NO
      *    MOVE IFF-CERT-NO            TO WS-ELMSTR-CERT-NO
      *
02761 *    EXEC CICS READ
02762 *         DATASET    ('ELMSTR')
02763 *         INTO       (CLAIM-MASTER)
02764 *         RIDFLD     (WS-ELMSTR-KEY)
      *         RESP       (WS-RESPONSE)
02766 *    END-EXEC.
      *
      *    IF RESP-NORMAL
      *       MOVE IFF-LETTER-ID       TO OUT-TEMP
      *                                   OUT-LETTER
      *       MOVE CL-CARRIER          TO OUT-CARR
      *       MOVE CL-INSURED-LAST-NAME
      *                                TO OUT-ILNAME
      *       MOVE CL-INSURED-1ST-NAME TO OUT-IFNAME
      *       MOVE CL-CLAIM-NO         TO OUT-CLMNO
      *       MOVE CL-CERT-NO          TO OUT-CRTNO
      *       MOVE CL-CERT-ACCOUNT     TO OUT-ACTNO
      *    END-IF
02767
      *    exec cics link program('NSRLTRBL')
      *       commarea(srch-commarea)
      *    end-exec.
      ***********************************************************
      * Build output document.  There are three templates used
      * for this document.  PEMHDR and PEMFTR are the header
      * and footer, respectively.  PEMBOD is used for the
      * actual data.  For each array entry in the business
      * logic output, set the symbol list from the array
      * entry and insert into the document using the PEMBOD
      * template.
      ***********************************************************
      *    move bl-output-message to out-msg-text.
           evaluate true
              when iff-comp-id = 'DCC'
                 IF WS-KIX-MYENV = 'cid1p'
                    
      * exec cics document create
      *                doctoken   (w-doctoken)
      *                template   ('PEMHDR')
      *                symbollist (WS-PROD-DCC-PEMHDR)
      *                resp       (WS-RESPONSE)
      *             end-exec
           MOVE LENGTH OF
            WS-PROD-DCC-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001120' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-PROD-DCC-PEMHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 ELSE
                    
      * exec cics document create
      *                doctoken   (w-doctoken)
      *                template   ('PEMHDR')
      *                symbollist (WS-TEST-DCC-PEMHDR)
      *                resp       (WS-RESPONSE)
      *             end-exec
           MOVE LENGTH OF
            WS-TEST-DCC-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001127' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-TEST-DCC-PEMHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 END-IF
022212        when IFF-COMP-ID = 'AHL'
022212           IF WS-KIX-MYENV = 'cid1p'
022212              
      * exec cics document create
022212*                doctoken   (w-doctoken)
022212*                template   ('PEMHDR')
022212*                symbollist (WS-PROD-AHL-PEMHDR)
022212*                resp       (WS-RESPONSE)
022212*             end-exec
           MOVE LENGTH OF
            WS-PROD-AHL-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001136' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-PROD-AHL-PEMHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
022212           ELSE
022212              
      * exec cics document create
022212*                doctoken   (w-doctoken)
022212*                template   ('PEMHDR')
022212*                symbollist (WS-TEST-AHL-PEMHDR)
022212*                resp       (WS-RESPONSE)
022212*             end-exec
           MOVE LENGTH OF
            WS-TEST-AHL-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001143' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-TEST-AHL-PEMHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
022212           END-IF
020816        when IFF-COMP-ID = 'VPP'
020816           IF WS-KIX-MYENV = 'cid1p'
020816              
      * exec cics document create
020816*                doctoken   (w-doctoken)
020816*                template   ('PEMHDR')
020816*                symbollist (WS-PROD-VPP-PEMHDR)
020816*                resp       (WS-RESPONSE)
020816*             end-exec
           MOVE LENGTH OF
            WS-PROD-VPP-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001152' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-PROD-VPP-PEMHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020816           ELSE
020816              
      * exec cics document create
020816*                doctoken   (w-doctoken)
020816*                template   ('PEMHDR')
020816*                symbollist (WS-TEST-VPP-PEMHDR)
020816*                resp       (WS-RESPONSE)
020816*             end-exec
           MOVE LENGTH OF
            WS-TEST-VPP-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001159' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-TEST-VPP-PEMHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020816           END-IF
061421        when IFF-COMP-ID = 'FNL'
061421           IF WS-KIX-MYENV = 'cid1p'
061421              
      * exec cics document create
061421*                doctoken   (w-doctoken)
061421*                template   ('PEMHDR')
061421*                symbollist (WS-PROD-FNL-PEMHDR)
061421*                resp       (WS-RESPONSE)
061421*             end-exec
           MOVE LENGTH OF
            WS-PROD-FNL-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001168' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-PROD-FNL-PEMHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061421           ELSE
061421              
      * exec cics document create
061421*                doctoken   (w-doctoken)
061421*                template   ('PEMHDR')
061421*                symbollist (WS-TEST-FNL-PEMHDR)
061421*                resp       (WS-RESPONSE)
061421*             end-exec
           MOVE LENGTH OF
            WS-TEST-FNL-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001175' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-TEST-FNL-PEMHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061421           END-IF
              when other
                 IF WS-KIX-MYENV = 'cid1p'
                    
      * exec cics document create
      *                doctoken   (w-doctoken)
      *                template   ('PEMHDR')
      *                symbollist (WS-PROD-CID-PEMHDR)
      *                resp       (WS-RESPONSE)
      *             end-exec
           MOVE LENGTH OF
            WS-PROD-CID-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001184' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-PROD-CID-PEMHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 ELSE
                    
      * exec cics document create
      *                doctoken   (w-doctoken)
      *                template   ('PEMHDR')
      *                symbollist (WS-TEST-CID-PEMHDR)
      *                resp       (WS-RESPONSE)
      *             end-exec
           MOVE LENGTH OF
            WS-TEST-CID-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001191' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-TEST-CID-PEMHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 END-IF
           end-evaluate
      *    DISPLAY ' PEMHDR DOC CREATE ' WS-RESPONSE
      *    move 1 to bl-index.
      *
      *    perform bl-output-record-count times
      *
      *        move bl-output-account-number(bl-index)
      *          to out-account-number
      *        move bl-output-last-name(bl-index)
      *          to out-last-name
      *        move bl-output-first-name(bl-index)
      *          to out-first-name
      *        move bl-output-middle-initial(bl-index)
      *          to out-middle-initial
      *
      *    MOVE 'CICM'                 TO OUT-TEMP
      *    MOVE 'CICM'                 TO OUT-LETTER
      *    MOVE '9'                    TO OUT-CARR
      *    MOVE 'MCDANIEL'             TO OUT-ILNAME
      *    MOVE 'KATHI'                TO OUT-IFNAME
      *    MOVE '0151216'              TO OUT-CLMNO
      *    MOVE '0009235906 '          TO OUT-CRTNO
      *    MOVE '0990000208'           TO OUT-ACTNO
           
      * exec cics document set
      *       doctoken(w-doctoken)
      *       symbollist(NAPER-OUTPUT-DATA)
      *       length(length of NAPER-OUTPUT-DATA)
      *                   resp   (WS-RESPONSE)
      *    end-exec
           MOVE LENGTH OF
            NAPER-OUTPUT-DATA TO DFHEIV16
      *    MOVE '\(Ds L                ''  N#00001221' TO DFHEIV0
           MOVE X'5C284473204C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 NAPER-OUTPUT-DATA, 
                 DFHEIV99, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * exec cics document insert
      *       doctoken(w-doctoken)
      *       template('PEMBOD')
      *                   resp   (WS-RESPONSE)
      *    end-exec
           MOVE 'PEMBOD' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001227' TO DFHEIV0
           MOVE X'5C2444742020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           INSPECT OT-REGARDING REPLACING ALL '&' BY ' '
           if ws-kix-myenv = 'cid1p' or 'mdoff'
061421        compute ws-sl-var-length =
061421           (ws-sl-var-length + v2) - +1
              
      * exec cics document set
      *          doctoken(w-doctoken)
      *          symbollist(WS-VAR-SLUNIKIX)
061421*          length(ws-sl-var-length)
      *                      resp   (WS-RESPONSE)
      *       end-exec
      *    MOVE '\(Ds L                ''  N#00001236' TO DFHEIV0
           MOVE X'5C284473204C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 WS-VAR-SLUNIKIX, 
                 DFHEIV99, 
                 ws-sl-var-length, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           else
061421        compute ws-lt-var-length =
061421           (ws-lt-var-length + v2) - +1
              
      * exec cics document set
      *          doctoken(w-doctoken)
      *          symbollist(WS-VAR-LOGICTEST)
061421*          length(ws-lt-var-length)
      *                      resp   (WS-RESPONSE)
      *       end-exec
      *    MOVE '\(Ds L                ''  N#00001245' TO DFHEIV0
           MOVE X'5C284473204C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 WS-VAR-LOGICTEST, 
                 DFHEIV99, 
                 ws-lt-var-length, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           
      * exec cics document insert
      *       doctoken(w-doctoken)
      *       template('PEMFTR')
      *                   resp   (WS-RESPONSE)
      *    end-exec
           MOVE 'PEMFTR' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001252' TO DFHEIV0
           MOVE X'5C2444742020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
      ****************************************
      * Send the document and return.
      ****************************************
           
      * exec cics web send
      *       doctoken(w-doctoken)
      *                   resp   (WS-RESPONSE)
      *    end-exec.
      *    MOVE 'X$D                   *  N#00001260' TO DFHEIV0
           MOVE X'582444202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202A20' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
PEMTST*    DISPLAY ' WEB SEND  ' WS-RESPONSE
PEMTST*    DISPLAY ' ABOUT TO RETURN '
           
      * exec cics
      *       return
      *    end-exec.
      *    MOVE '.(                    ''   #00001266' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
      ******************************************************
      * Read all fields of the incoming form, moving
      * each to the corresponding field of the commarea
      * (business logic input fields).
      ******************************************************
       read-form.
           move spaces to w-form-name.
           move length of w-form-name to w-form-name-len.
                 move spaces to w-form-value.
           move length of w-form-value to w-form-value-len.
           
      * exec cics web readnext
      *                  formfield(w-form-name)
      *                  namelength(w-form-name-len)
      *                  value(w-form-value)
      *                  valuelength(w-form-value-len)
      *                  resp(w-resp)
      *    end-exec.
      *    MOVE 'X*FLVL                &  N#00001279' TO DFHEIV0
           MOVE X'582A464C564C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-form-name, 
                 w-form-name-len, 
                 w-form-value, 
                 w-form-value-len, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           evaluate w-resp
              when 0 
      *   dfhresp(normal)
                 evaluate w-form-name(1:w-form-name-len)
                    when 'carrier'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                                         to IFF-CARRIER
                                                            BL-CARRIER
                       else
                                      move spaces to IFF-CARRIER
                       end-if
                    when 'claim_no'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                                         to IFF-CLAIM-NO
                                                            BL-CLAIM-NO
                       else
                                      move spaces  to IFF-CLAIM-NO
                       end-if
                    when 'cert_no'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-CERT-NO
                                          BL-CERT-NO
                       else
                                      move spaces  to IFF-CERT-NO
                       end-if
                    when 'letter_id'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-LETTER-ID
                                          BL-LETTER-ID
                       else
                          move spaces  to IFF-LETTER-ID
                       end-if
                    when 'fu_date'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-FOLLOW-UP-DT
                                          BL-FOLLOW-UP-DT
                       else
                                      move spaces  to IFF-FOLLOW-UP-DT
                       end-if
                    when 'rs_date'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-RESEND-DT
                                          BL-RESEND-DT
                       else
                                      move spaces  to IFF-RESEND-DT
                       end-if
                    when 'no_copies'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-NO-OF-COPIES
                                          BL-NO-OF-COPIES
                       else
                          move zeros   to IFF-NO-OF-COPIES
                       end-if
                    when 'proc_id'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-PROC-ID
                                          BL-PROC-ID
                       else
                          move 'KMSB'  to IFF-PROC-ID
                       end-if
                    when 'comp_id'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-COMP-ID
                                          BL-COMP-ID
                       else
                          move 'CID'   to IFF-COMP-ID
                       end-if
                    when 'prt_now'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-PRINT-NOW-SW
                                          BL-PRINT-NOW-SW
                       else
                          move 'N'     to IFF-PRINT-NOW-SW
                       end-if
                    when 'enc_cd'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-ENC-CD
                                          BL-ENC-CD
                       else
                          move '0'     to IFF-ENC-CD
                       end-if
                    when 'regarding'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-REGARDING
                                          BL-REGARDING
                       else
                          move SPACES  to IFF-REGARDING BL-REGARDING
                       end-if
                 end-evaluate
              when other
                 continue
           end-evaluate.
       read-form-exit.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSREQLTR' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSREQLTR' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
