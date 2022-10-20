      *((program: NSREQLTR.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. NSREQLTR.
000004
000005*AUTHOR.     PABLO
000006*            COLLEYVILLE, TEXAS.
000007
000008*REMARKS.    EXECUTED FROM INDEX.HTML
000009
000010******************************************************************
000011*                   C H A N G E   L O G
000012*
000013* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000014*-----------------------------------------------------------------
000015*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000016* EFFECTIVE    NUMBER
000017*-----------------------------------------------------------------
000018* 121802    2009122800001  PEMA  NEW PROGRAM
000019* 022212    2011120900003  AJRA  ADD AHL
000020* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
000021* 061421  CR2017031500001  PEMA  Update to CCM8
000022******************************************************************
000023 ENVIRONMENT DIVISION.
000024
000025 DATA DIVISION.
000026 working-storage section.
       01  DFH-START PIC X(04).
000027
000028 01  P pointer.
000029 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000030 01  var-ptr pointer.
000031 01  env-var-len                 pic 9(4)  binary.
000032 01  rc                          pic 9(9)  binary.
000033
000034 01  filler.
000035     05  a1                      pic s999 comp-3 value +0.
000036     05  a2                      pic s999 comp-3 value +0.
000037     05  m1                      pic s999 comp-3 value +0.
000038     05  v1                      pic s999 comp-3 value +0.
000039     05  v2                      pic s999 comp-3 value +0.
000040     05  WS-WORK-FIELD           PIC X(90)    VALUE SPACES.
000041
000042 01  WS-KIXSYS.
000043     05  WS-KIX-FIL1             PIC X(10).
000044     05  WS-KIX-APPS             PIC X(10).
000045     05  WS-KIX-ENV              PIC X(10).
000046     05  WS-KIX-MYENV            PIC X(10).
000047     05  WS-KIX-SYS              PIC X(10).
000048
000049************************************************
000050* commarea passed to the business logic
000051************************************************
000052
000053 01 srch-commarea.
000054*                                copy ELCLTRSPI.
      *>>((file: ELCLTRSPI))
000001******************************************************************
000002*                   C H A N G E   L O G
000003*
000004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000005*-----------------------------------------------------------------
000006*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000007* EFFECTIVE    NUMBER
000008*-----------------------------------------------------------------
000009* 121802    2009122800001  PEMA  NEW COPYBOOK
000010******************************************************************
000011****************************************
000012*  commarea for NaperSoft On Demand Claim letters
000013*  (business logic input & output)
000014****************************************
000015
000016     03  BL-INPUT.
000017         05  BL-CARRIER          PIC X.
000018         05  BL-CLAIM-NO         PIC X(7).
000019         05  BL-CERT-NO          PIC X(11).
000020         05  BL-LETTER-ID        PIC XXXX.
000021         05  BL-FOLLOW-UP-DT     PIC X(10).
000022         05  BL-RESEND-DT        PIC X(10).
000023         05  BL-NO-OF-COPIES     PIC 99.
000024         05  BL-PROC-ID          PIC XXXX.
000025         05  BL-COMP-ID          PIC XXX.
000026         05  BL-PRINT-NOW-SW     PIC X.
000027         05  BL-ENC-CD           PIC XXX.
000028         05  BL-ARCHIVE-NO       PIC 9(8).
000029         05  BL-REGARDING        PIC X(70).
000030
000031     03  BL-OUTPUT.
000032         05  BL-STATUS                   PIC X.
000033             88  BL-OK                      VALUE "P".
000034             88  BL-FAIL                  VALUE "F".
000035         05  BL-MESSAGE          PIC X(50).
000036     03  BL-RECORD-PASSED-DATA   PIC X(2500).
      *<<((file: ELCLTRSPI))
000055
000056 01  INPUT-FROM-FORM.
000057     05  IFF-CARRIER             PIC X.
000058     05  IFF-CLAIM-NO            PIC X(7).
000059     05  IFF-CERT-NO             PIC X(11).
000060     05  IFF-LETTER-ID           PIC XXXX.
000061     05  IFF-FOLLOW-UP-DT        PIC X(10).
000062     05  IFF-RESEND-DT           PIC X(10).
000063     05  IFF-NO-OF-COPIES        PIC 99.
000064     05  IFF-PROC-ID             PIC XXXX.
000065     05  IFF-COMP-ID             PIC XXX.
000066     05  IFF-PRINT-NOW-SW        PIC X.
000067     05  IFF-ENC-CD              PIC XXX.
000068     05  IFF-REGARDING           PIC X(70).
000069
000070************************************
000071* fields used to read web data
000072************************************
000073
000074 01  w-form-name       pic x(80).
000075 01  w-form-value      pic x(80).
000076 01  w-form-name-len   pic s9(8) comp.
000077 01  w-form-value-len  pic s9(8) comp.
000078 01  w-resp            pic s9(8) comp.
000079 01  w-doctoken        pic x(16).
000080
000081* COMP ID TPE REGION   GROUP NAME       HOST          HTTP PORT
000082*
000083*  CID      CID1P      BatchClaims     sdv-nsft01       7001
000084*  CID      MDOFF      BatchClaims     hov-nsft01       7003
000085*  CID      CID1T      BatchClaims     hov-nsft01       6002
000086*  CID      PAUL       BatchClaims     hov-nsft01       5002
000087*  CID      TONY       BatchClaims     hov-nsft01       6003
000088*  DCC      CID1P      BatchDCCClaims  sdv-nsft01       7001
000089*  DCC      MDOFF      BatchDCCClaims  hov-nsft01       7003
000090*  DCC      CID1T      BatchDCCClaims  hov-nsft01       6002
000091*  DCC      PAUL       BatchDCCClaims  hov-nsft01       5002
000092*  DCC      TONY       BatchDCCClaims  hov-nsft01       6003
000093*  AHL      CID1P      BatchAHLClaims  sdv-nsft01       7001
000094*  AHL      MDOFF      BatchAHLClaims  hov-nsft01       7003
000095*  AHL      CID1T      BatchAHLClaims  hov-nsft01       6002
000096*  AHL      PAUL       BatchAHLClaims  hov-nsft01       5002
000097*  AHL      TONY       BatchAHLClaims  hov-nsft01       6003
000098*  AHL      AHLTST     BatchAHLClaims  hov-nsft01       6007
000099*  FNL      CID1P      BatchFNLClaims  sdv-nsft01       7001
000100*  FNL      MDOFF      BatchFNLClaims  hov-nsft01       7003
000101*  FNL      CID1T      BatchFNLClaims  hov-nsft01       6002
000102*  FNL      PAUL       BatchFNLClaims  hov-nsft01       5002
000103*  FNL      TONY       BatchFNLClaims  hov-nsft01       6003
000104*  FNL      AHLTST     BatchFNLClaims  hov-nsft01       6007
000105
000106******************************************
000107* symbol list text for PEMHDR template
000108******************************************
000109
000110 01  WS-PROD-CID-PEMHDR.
000111     05  F                       PIC X(7)  VALUE "SERVER=".
000112     05  F                       PIC X(20) VALUE
000113                                    'sdv-nsft01.cso.local'.
000114     05  F                       PIC X(11) VALUE '&GROUPNAME='.
000115     05  F                       PIC X(11) VALUE 'BatchClaims'.
000116     05  F                       PIC X(7)  VALUE '&UNAME='.
000117     05  F                       PIC X(8)  VALUE 'batchjob'.
000118     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000119     05  F                       PIC X(8)  VALUE 'batchjob'.
000120
000121 01  WS-PROD-DCC-PEMHDR.
000122     05  F                       PIC X(7)  VALUE "SERVER=".
000123     05  F                       PIC X(20) VALUE
000124                                    'sdv-nsft01.cso.local'.
000125     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000126     05  F                       PIC X(14) VALUE 'BatchDCCClaims'.
000127     05  F                       PIC X(7)  VALUE '&UNAME='.
000128     05  F                       PIC X(8)  VALUE 'batchjob'.
000129     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000130     05  F                       PIC X(8)  VALUE 'batchjob'.
000131
000132 01  WS-PROD-VPP-PEMHDR.
000133     05  F                       PIC X(7)  VALUE "SERVER=".
000134     05  F                       PIC X(20) VALUE
000135                                    'sdv-nsft01.cso.local'.
000136     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000137     05  F                       PIC X(14) VALUE 'BatchVPPClaims'.
000138     05  F                       PIC X(7)  VALUE '&UNAME='.
000139     05  F                       PIC X(8)  VALUE 'batchjob'.
000140     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000141     05  F                       PIC X(8)  VALUE 'batchjob'.
000142
000143 01  WS-PROD-AHL-PEMHDR.
000144     05  F                       PIC X(7)  VALUE "SERVER=".
000145     05  F                       PIC X(20) VALUE
000146                                    'sdv-nsft01.cso.local'.
000147     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000148     05  F                       PIC X(14) VALUE 'BatchAHLClaims'.
000149     05  F                       PIC X(7)  VALUE '&UNAME='.
000150     05  F                       PIC X(8)  VALUE 'batchjob'.
000151     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000152     05  F                       PIC X(8)  VALUE 'batchjob'.
000153
000154 01  WS-PROD-FNL-PEMHDR.
000155     05  F                       PIC X(7)  VALUE "SERVER=".
000156     05  F                       PIC X(20) VALUE
000157                                    'sdv-nsft01.cso.local'.
000158     05  F                       PIC X(11) VALUE '&GROUPNAME='.
000159     05  F                       PIC X(14) VALUE 'BatchFNLClaims'.
000160     05  F                       PIC X(7)  VALUE '&UNAME='.
000161     05  F                       PIC X(8)  VALUE 'batchjob'.
000162     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000163     05  F                       PIC X(8)  VALUE 'batchjob'.
000164
000165 01  WS-TEST-CID-PEMHDR.
000166     05  F                       PIC X(7)  VALUE "SERVER=".
000167     05  F                       PIC X(20) VALUE
000168                          'hov-nsft02.cso.local'.
000169     05  F                       PIC X(11) VALUE '&GROUPNAME='.
000170     05  F                       PIC X(11) VALUE 'BatchClaims'.
000171     05  F                       PIC X(7)  VALUE '&UNAME='.
000172     05  F                       PIC X(8)  VALUE 'batchjob'.
000173     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000174     05  F                       PIC X(8)  VALUE 'batchjob'.
000175
000176 01  WS-TEST-DCC-PEMHDR.
000177     05  F                       PIC X(7)  VALUE "SERVER=".
000178     05  F                       PIC X(20) VALUE
000179                          'hov-nsft02.cso.local'.
000180     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000181     05  F                       PIC X(14) VALUE 'BatchDCCClaims'.
000182     05  F                       PIC X(7)  VALUE '&UNAME='.
000183     05  F                       PIC X(8)  VALUE 'batchjob'.
000184     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000185     05  F                       PIC X(8)  VALUE 'batchjob'.
000186
000187 01  WS-TEST-VPP-PEMHDR.
000188     05  F                       PIC X(7)  VALUE "SERVER=".
000189     05  F                       PIC X(20) VALUE
000190                          'hov-nsft02.cso.local'.
000191     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000192     05  F                       PIC X(14) VALUE 'BatchVPPClaims'.
000193     05  F                       PIC X(7)  VALUE '&UNAME='.
000194     05  F                       PIC X(8)  VALUE 'batchjob'.
000195     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000196     05  F                       PIC X(8)  VALUE 'batchjob'.
000197
000198 01  WS-TEST-AHL-PEMHDR.
000199     05  F                       PIC X(7)  VALUE "SERVER=".
000200     05  F                       PIC X(20) VALUE
000201                          'hov-nsft02.cso.local'.
000202     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000203     05  F                       PIC X(14) VALUE 'BatchAHLClaims'.
000204     05  F                       PIC X(7)  VALUE '&UNAME='.
000205     05  F                       PIC X(8)  VALUE 'batchjob'.
000206     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000207     05  F                       PIC X(8)  VALUE 'batchjob'.
000208
000209 01  WS-TEST-FNL-PEMHDR.
000210     05  F                       PIC X(7)  VALUE "SERVER=".
000211     05  F                       PIC X(20) VALUE
000212                          'hov-nsft02.cso.local'.
000213     05  F                       PIC X(11) VALUE '&GROUPNAME='.
000214     05  F                       PIC X(14) VALUE 'BatchFNLClaims'.
000215     05  F                       PIC X(7)  VALUE '&UNAME='.
000216     05  F                       PIC X(8)  VALUE 'batchjob'.
000217     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000218     05  F                       PIC X(8)  VALUE 'batchjob'.
000219
000220******************************************
000221* symbol list text for PEMFTR template
000222******************************************
000223
000224 01  ws-sl-var-length pic s9(8) comp value +33.
000225 01  WS-VAR-SLUNIKIX.
000226     05  FILLER                  PIC X(09) VALUE "HOSTINFO=".
000227     05  WS-SL-HOST-INFO         PIC X(09) VALUE 'slunikix:'.
000228     05  WS-SL-PORT              PIC XXXX  VALUE '7001'.
000229     05  WS-SL-REST              PIC X(200) VALUE SPACES.
000230
000231 01  ws-lt-var-length pic s9(8) comp value +34.
000232 01  WS-VAR-LOGICTEST.
000233     05  FILLER                  PIC X(09) VALUE "HOSTINFO=".
000234     05  WS-LT-HOST-INFO         PIC X(10) VALUE 'logictest:'.
000235     05  WS-LT-PORT              PIC XXXX  VALUE '6002'.
000236     05  WS-LT-REST              PIC X(200) VALUE SPACES.
000237
000238 01  finished-string.
000239     05  FILLER                   PIC X(11)  VALUE "&URLVARLST=".
000240     05  var-string               pic x(200) value spaces.
000241
000242 01 WS-TEST-TESTER.
000243    05  FILLER                   PIC X(11)  VALUE "&URLVARLST=".
000244    05  WS-KEY.
000245        10  OT-CARRIER           PIC X.
000246        10  OT-CLMNO             PIC X(7).
000247        10  OT-CRTNO             PIC X(11).
000248        10  OT-LETTER-ID         PIC X(4).
000249        10  OT-FOLLOW-UP-DT      PIC X(10).
000250        10  OT-RESEND-DT         PIC X(10).
000251        10  OT-NO-OF-COPIES      PIC 99.
000252        10  OT-PROC-ID           PIC XXXX.
000253        10  OT-COMP-ID           PIC XXX.
000254        10  OT-PRINT-NOW-SW      PIC X.
000255        10  OT-ENC-CD            PIC XXX.
000256        10  OT-ARCHIVE-NO        PIC 9(08).
000257        10  OT-REGARDING         PIC X(70).
000258
000259
000260 01  WS-ELMSTR-KEY.
000261     05  WS-ELMSTR-COMPANY-CD    PIC X.
000262     05  WS-ELMSTR-CARRIER       PIC X.
000263     05  WS-ELMSTR-CLAIM-NO      PIC X(7).
000264     05  WS-ELMSTR-CERT-NO       PIC X(11).
000265
000266*                                COPY ELCMSTR.
      *>>((file: ELCMSTR))
000001******************************************************************
000002*                                                                *
000003*                            ELCMSTR.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.012                          *
000006*                                                                *
000007*   FILE DESCRIPTION = CLAIM MASTER FILE                         *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 350  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
000013*       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
000014*       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
000015*       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
000016*       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
000017*                                                 RKP=75,LEN=21  *
000018*                                                                *
000019*   **** NOTE ****                                               *
000020*             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
000021*             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
000022*                                                                *
000023*   LOG = YES                                                    *
000024*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000025******************************************************************
000026*                   C H A N G E   L O G
000027*
000028* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000029*-----------------------------------------------------------------
000030*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000031* EFFECTIVE    NUMBER
000032*-----------------------------------------------------------------
000033* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
000034* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
000035* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
000036* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000037* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000038* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
000039* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000040******************************************************************
000041 01  CLAIM-MASTER.
000042     12  CL-RECORD-ID                PIC XX.
000043         88  VALID-CL-ID         VALUE 'CL'.
000044
000045     12  CL-CONTROL-PRIMARY.
000046         16  CL-COMPANY-CD           PIC X.
000047         16  CL-CARRIER              PIC X.
000048         16  CL-CLAIM-NO             PIC X(7).
000049         16  CL-CERT-NO.
000050             20  CL-CERT-PRIME       PIC X(10).
000051             20  CL-CERT-SFX         PIC X.
000052
000053     12  CL-CONTROL-BY-NAME.
000054         16  CL-COMPANY-CD-A1        PIC X.
000055         16  CL-INSURED-LAST-NAME    PIC X(15).
000056         16  CL-INSURED-NAME.
000057             20  CL-INSURED-1ST-NAME PIC X(12).
000058             20  CL-INSURED-MID-INIT PIC X.
000059
000060     12  CL-CONTROL-BY-SSN.
000061         16  CL-COMPANY-CD-A2        PIC X.
000062         16  CL-SOC-SEC-NO.
000063             20  CL-SSN-STATE        PIC XX.
000064             20  CL-SSN-ACCOUNT      PIC X(6).
000065             20  CL-SSN-LN3          PIC X(3).
000066
000067     12  CL-CONTROL-BY-CERT-NO.
000068         16  CL-COMPANY-CD-A4        PIC X.
000069         16  CL-CERT-NO-A4.
000070             20  CL-CERT-A4-PRIME    PIC X(10).
000071             20  CL-CERT-A4-SFX      PIC X.
000072
000073     12  CL-CONTROL-BY-CCN.
000074         16  CL-COMPANY-CD-A5        PIC X.
000075         16  CL-CCN-A5.
000076             20  CL-CCN.
000077                 24  CL-CCN-PREFIX-A5 PIC X(4).
000078                 24  CL-CCN-PRIME-A5 PIC X(12).
000079             20  CL-CCN-FILLER-A5    PIC X(4).
000080
000081     12  CL-INSURED-PROFILE-DATA.
000082         16  CL-INSURED-BIRTH-DT     PIC XX.
000083         16  CL-INSURED-SEX-CD       PIC X.
000084             88  INSURED-IS-MALE        VALUE 'M'.
000085             88  INSURED-IS-FEMALE      VALUE 'F'.
000086             88  INSURED-SEX-UNKNOWN    VALUE ' '.
000087         16  CL-INSURED-OCC-CD       PIC X(6).
000088         16  FILLER                  PIC X(5).
000089
000090     12  CL-PROCESSING-INFO.
000091         16  CL-PROCESSOR-ID         PIC X(4).
000092         16  CL-CLAIM-STATUS         PIC X.
000093             88  CLAIM-IS-OPEN          VALUE 'O'.
000094             88  CLAIM-IS-CLOSED        VALUE 'C'.
000095         16  CL-CLAIM-TYPE           PIC X.
000096*            88  AH-CLAIM               VALUE 'A'.
000097*            88  LIFE-CLAIM             VALUE 'L'.
000098*            88  PROPERTY-CLAIM         VALUE 'P'.
000099*            88  IUI-CLAIM              VALUE 'I'.
000100*            88  GAP-CLAIM              VALUE 'G'.
000101*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
000102*            88  OTHER-CLAIM            VALUE 'O'.
000103         16  CL-CLAIM-PREM-TYPE      PIC X.
000104             88  SINGLE-PREMIUM         VALUE '1'.
000105             88  O-B-COVERAGE           VALUE '2'.
000106             88  OPEN-END-COVERAGE      VALUE '3'.
000107         16  CL-INCURRED-DT          PIC XX.
000108         16  CL-REPORTED-DT          PIC XX.
000109         16  CL-FILE-ESTABLISH-DT    PIC XX.
000110         16  CL-EST-END-OF-DISAB-DT  PIC XX.
000111         16  CL-LAST-PMT-DT          PIC XX.
000112         16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
000113         16  CL-PAID-THRU-DT         PIC XX.
000114         16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
000115         16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
000116         16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
000117         16  CL-PMT-CALC-METHOD      PIC X.
000118             88  CL-360-DAY-YR          VALUE '1'.
000119             88  CL-365-DAY-YR          VALUE '2'.
000120             88  CL-FULL-MONTHS         VALUE '3'.
000121         16  CL-CAUSE-CD             PIC X(6).
000122
000123         16  CL-PRIME-CERT-NO.
000124             20  CL-PRIME-CERT-PRIME PIC X(10).
000125             20  CL-PRIME-CERT-SFX   PIC X.
000126
000127         16  CL-SYSTEM-IDENTIFIER    PIC XX.
000128             88  CL-CREDIT-CLAIM        VALUE 'CR'.
000129             88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
000130
000131         16  CL-MICROFILM-NO         PIC X(10).
000132         16  FILLER REDEFINES CL-MICROFILM-NO.
000133             20  CL-BENEFIT-PERIOD   PIC 99.
000134             20  FILLER              PIC X(8).
000135         16  CL-PROG-FORM-TYPE       PIC X.
000136         16  CL-LAST-ADD-ON-DT       PIC XX.
000137
000138         16  CL-LAST-REOPEN-DT       PIC XX.
000139         16  CL-LAST-CLOSE-DT        PIC XX.
000140         16  CL-LAST-CLOSE-REASON    PIC X(01).
000141             88  FINAL-PAID             VALUE '1'.
000142             88  CLAIM-DENIED           VALUE '2'.
000143             88  AUTO-CLOSE             VALUE '3'.
000144             88  MANUAL-CLOSE           VALUE '4'.
000145             88  BENEFITS-CHANGED       VALUE 'C'.
000146             88  SETUP-ERRORS           VALUE 'E'.
000147         16  CL-ASSOC-CERT-SEQU      PIC S99.
000148         16  CL-ASSOC-CERT-TOTAL     PIC S99.
000149         16  CL-CLAIM-PAYMENT-STATUS PIC 9.
000150             88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
000151         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
000152         16  FILLER                  PIC X.
000153
000154     12  CL-CERTIFICATE-DATA.
000155         16  CL-CERT-ORIGIN          PIC X.
000156             88  CERT-WAS-ONLINE        VALUE '1'.
000157             88  CERT-WAS-CREATED       VALUE '2'.
000158             88  COVERAGE-WAS-ADDED     VALUE '3'.
000159         16  CL-CERT-KEY-DATA.
000160             20  CL-CERT-CARRIER     PIC X.
000161             20  CL-CERT-GROUPING    PIC X(6).
000162             20  CL-CERT-STATE       PIC XX.
000163             20  CL-CERT-ACCOUNT.
000164                 24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
000165                 24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
000166             20  CL-CERT-EFF-DT      PIC XX.
000167
000168     12  CL-STATUS-CONTROLS.
000169         16  CL-PRIORITY-CD          PIC X.
000170             88  CONFIDENTIAL-DATA      VALUE '8'.
000171             88  HIGHEST-PRIORITY       VALUE '9'.
000172         16  CL-SUPV-ATTN-CD         PIC X.
000173             88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
000174             88  SUPV-IS-REQUIRED       VALUE 'Y'.
000175         16  CL-PURGED-DT            PIC XX.
000176         16  CL-RESTORED-DT          PIC XX.
000177         16  CL-NEXT-AUTO-PAY-DT     PIC XX.
000178         16  CL-NEXT-RESEND-DT       PIC XX.
000179         16  CL-NEXT-FOLLOWUP-DT     PIC XX.
000180         16  CL-CRITICAL-PERIOD      PIC 99.
000181*        16  FILLER                  PIC XX.
000182         16  CL-LAST-MAINT-DT        PIC XX.
000183         16  CL-LAST-MAINT-USER      PIC X(4).
000184         16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
000185         16  CL-LAST-MAINT-TYPE      PIC X.
000186             88  CLAIM-SET-UP           VALUE ' '.
000187             88  PAYMENT-MADE           VALUE '1'.
000188             88  LETTER-SENT            VALUE '2'.
000189             88  MASTER-WAS-ALTERED     VALUE '3'.
000190             88  MASTER-WAS-RESTORED    VALUE '4'.
000191             88  INCURRED-DATE-CHANGED  VALUE '5'.
000192             88  FILE-CONVERTED         VALUE '6'.
000193             88  CHANGE-OF-BENEFITS     VALUE 'C'.
000194             88  ERROR-CORRECTION       VALUE 'E'.
000195         16  CL-RELATED-CLAIM-NO     PIC X(7).
000196         16  CL-HISTORY-ARCHIVE-DT   PIC XX.
000197         16  CL-BENEFICIARY          PIC X(10).
000198         16  CL-FILE-ESTABLISHED-BY  PIC X(4).
000199         16  CL-DENIAL-TYPE          PIC X.
000200             88  CL-TYPE-DENIAL          VALUE '1'.
000201             88  CL-TYPE-RESCISSION      VALUE '2'.
000202             88  CL-TYPE-REFORMATION     VALUE '3'.
000203             88  CL-TYPE-REF-TO-RES      VALUE '4'.
000204             88  CL-TYPE-RECONSIDERED    VALUE '5'.
000205         16  CL-NO-OF-EXTENSIONS     PIC 99.
000206
000207         16  filler                  pic x(3).
000208*        16  CL-CRIT-PER-RECURRENT   PIC X.
000209*        16  CL-CRIT-PER-RTW-MOS     PIC 99.
000210*        16  CL-RTW-DT               PIC XX.
000211
000212     12  CL-TRAILER-CONTROLS.
000213         16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
000214             88  CL-1ST-TRL-AVAIL       VALUE +4095.
000215             88  CL-LAST-TRL-AVAIL      VALUE +100.
000216             88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
000217         16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
000218         16  FILLER                  PIC XX.
000219         16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
000220         16  CL-ADDRESS-TRAILER-CNT.
000221             20  CL-INSURED-ADDR-CNT  PIC S9(1).
000222                 88  NO-INSURED-AVAILABLE    VALUE ZERO.
000223             20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
000224                 88  ACCOUNT-IS-ONLINE       VALUE ZERO.
000225             20  CL-BENIF-ADDR-CNT    PIC S9(1).
000226                 88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
000227             20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
000228                 88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
000229             20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
000230                 88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
000231             20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
000232                 88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
000233             20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
000234                 88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
000235
000236     12  CL-CV-REFERENCE-NO.
000237         16  CL-CV-REFNO-PRIME       PIC X(18).
000238         16  CL-CV-REFNO-SFX         PIC XX.
000239
000240     12  CL-FILE-LOCATION            PIC X(4).
000241
000242     12  CL-PROCESS-ERRORS.
000243         16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
000244             88  NO-FATAL-ERRORS        VALUE ZERO.
000245         16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
000246             88  NO-FORCABLE-ERRORS     VALUE ZERO.
000247
000248     12  CL-PRODUCT-CD               PIC X.
000249
000250     12  CL-CURRENT-KEY-DATA.
000251         16  CL-CURRENT-CARRIER      PIC X.
000252         16  CL-CURRENT-GROUPING     PIC X(6).
000253         16  CL-CURRENT-STATE        PIC XX.
000254         16  CL-CURRENT-ACCOUNT      PIC X(10).
000255
000256     12  CL-ASSOCIATES               PIC X.
000257         88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
000258         88  CL-ASSOC-INTERFACE         VALUE 'I'.
000259         88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
000260         88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
000261
000262     12  CL-ACTIVITY-CODE            PIC 99.
000263     12  CL-ACTIVITY-MAINT-DT        PIC XX.
000264     12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
000265
000266     12  CL-LAPSE-REPORT-CODE        PIC 9.
000267     12  CL-LAG-REPORT-CODE          PIC 9.
000268     12  CL-LOAN-TYPE                PIC XX.
000269     12  CL-LEGAL-STATE              PIC XX.
000270
000271     12  CL-YESNOSW                  PIC X.
000272     12  CL-ACCIDENT-CLAIM-SW        PIC X.
000273         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
000274         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
000275         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
000276     12  cl-insured-type             pic x.
000277         88  cl-claim-on-primary         value 'P'.
000278         88  cl-claim-on-co-borrower     value 'C'.
000279     12  cl-benefit-expiration-dt    PIC XX.
      *<<((file: ELCMSTR))
000267
000268 01 output-msg.
000269    05 filler              pic x(4) value "MSG=".
000270    05 out-msg-text        pic x(50).
000271
000272 01  MISC.
000273     12  WS-RESPONSE             PIC S9(8)   COMP.
000274         88  RESP-NORMAL                  VALUE +00.
000275         88  RESP-NOTFND                  VALUE +13.
000276         88  RESP-DUPREC                  VALUE +14.
000277         88  RESP-DUPKEY                  VALUE +15.
000278         88  RESP-NOTOPEN                 VALUE +19.
000279         88  RESP-ENDFILE                 VALUE +20.
000280
000281*****************************************
000282* symbol list for the PEMBOD template
000283*****************************************
000284
000285*                                COPY NSCVARS.
      *>>((file: NSCVARS))
000001******************************************************************
000002*                   C H A N G E   L O G
000003*
000004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000005*-----------------------------------------------------------------
000006*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000007* EFFECTIVE    NUMBER
000008*-----------------------------------------------------------------
000009* 031912    2011110200002  AJRA  AHL CLAIM NUM
000010* 013013    2012110080002  AJRA  ADD ACCOUNT GPCD
000011* 031116    2015110400001  TANA  ADD EL150D FIELDS
000012* 061217    2017060900001  TANA  INCREASE ATTACHMENTS FIELD SIZE
000013* 071719    2019011600010  TANA  ADD VERIF CODE
000014******************************************************************
000015 01 NAPER-OUTPUT-DATA.
000016    05 FILLER              PIC X(6) VALUE "TEMPL=".
000017    05 OUT-TEMP            PIC X(4).
000018    05 FILLER              PIC X(8) VALUE "&LETTER=".
000019    05 OUT-LETTER          PIC X(4).
000020    05 FILLER              PIC X(6) VALUE "&CARR=".
000021    05 OUT-CARR            PIC X.
000022    05 FILLER              PIC X(7) VALUE "&CLMNO=".
000023    05 OUT-CLMNO           PIC X(7).
000024    05 FILLER              PIC X(7) VALUE "&CRTNO=".
000025    05 OUT-CRTNO           PIC X(11).
000026    05 FILLER              PIC X(7) VALUE "&ACTNO=".
000027    05 OUT-ACTNO           PIC X(10).
000028    05 FILLER              PIC X(8) VALUE "&ILNAME=".
000029    05 OUT-ILNAME          PIC X(15).
000030    05 FILLER              PIC X(8) VALUE "&IFNAME=".
000031    05 OUT-IFNAME          PIC X(10).
000032    05 FILLER              PIC X(7) VALUE "&SEXCD=".
000033    05 OUT-SEX-CD          PIC X.
000034    05 FILLER              PIC X(5) VALUE "&SSN=".
000035    05 OUT-SSN             PIC X(11).
000036    05 FILLER              PIC X(9) VALUE "&CLMSTAT=".
000037    05 OUT-CLMSTAT         PIC X.
000038    05 FILLER              PIC X(8) VALUE "&CLMTYP=".
000039    05 OUT-CLMTYPE         PIC X.
000040    05 FILLER              PIC X(7) VALUE "&ESTDT=".
000041    05 OUT-EST-DT          PIC X(21).
000042    05 FILLER              PIC X(7) VALUE "&INCDT=".
000043    05 OUT-INC-DT          PIC X(21).
000044    05 FILLER              PIC X(7) VALUE "&RPTDT=".
000045    05 OUT-RPT-DT          PIC X(21).
000046    05 FILLER              PIC X(10) VALUE "&PDTHRUDT=".
000047    05 OUT-PD-THRU-DT      PIC X(21).
000048    05 FILLER              PIC X(11) VALUE "&FORMDUEDT=".
000049    05 OUT-FORM-DUE-DT     PIC X(21).
000050    05 FILLER              PIC X(8) VALUE "&LPMTDT=".
000051    05 OUT-LST-PMT-DT      PIC X(21).
000052    05 FILLER              PIC X(8) VALUE "&NOPMTS=".
000053    05 OUT-NO-OF-PMTS      PIC X(10).
000054    05 FILLER              PIC X(9) VALUE "&TOTPAID=".
000055    05 OUT-TOT-PAID        PIC 9999999.99.
000056    05 FILLER              PIC X(8) VALUE "&LPDAMT=".
000057    05 OUT-LST-PD-AMT      PIC 9999999.99.
000058    05 FILLER              PIC X(9) VALUE "&ADDRCNT=".
000059    05 OUT-ADDR-CNT        PIC 9.
000060    05 FILLER              PIC X(7) VALUE "&CRTST=".
000061    05 OUT-CRT-ST          PIC XX.
000062    05 FILLER              PIC X(10) VALUE "&CERTACCT=".
000063    05 OUT-CERTACCT        PIC X(10).
000064    05 FILLER              PIC X(7) VALUE "&EFFDT=".
000065    05 OUT-EFF-DT          PIC X(21).
000066    05 FILLER              PIC X(8) VALUE "&LMNTDT=".
000067    05 OUT-LST-MAINT-DT    PIC X(21).
000068    05 FILLER              PIC X(10) VALUE "&LMNTUSER=".
000069    05 OUT-LST-MAINT-USER  PIC X(10).
000070    05 FILLER              PIC X(10) VALUE "&LMNTTYPE=".
000071    05 OUT-LST-MAINT-TYPE  PIC X(10).
000072    05 FILLER              PIC X(6) VALUE "&DIAG=".
000073    05 OUT-DIAG            PIC X(66).
000074    05 FILLER              PIC X(7) VALUE "&EXPDT=".
000075    05 OUT-EXP-DT          PIC X(21).
000076    05 FILLER              PIC X(12) VALUE "&LCLSREASON=".
000077    05 OUT-LST-CLS-REA     PIC X(10).
000078    05 FILLER              PIC X(8) VALUE "&BRTHDT=".
000079    05 OUT-BIRTH-DT        PIC X(21).
000080    05 FILLER              PIC X(6) VALUE "&TERM=".
000081    05 OUT-TERM            PIC 999.
000082    05 FILLER              PIC X(7) VALUE "&BENCD=".
000083    05 OUT-BEN-CD          PIC XX.
000084    05 FILLER              PIC X(8) VALUE "&BENAMT=".
000085    05 OUT-BENAMT          PIC 9999999.99.
000086    05 FILLER              PIC X(9) VALUE "&CRITPER=".
000087    05 OUT-CRIT-PER        PIC 999.
000088    05 FILLER              PIC X(9) VALUE "&WAITPER=".
000089    05 OUT-WAIT-PER        PIC XX.
000090    05 FILLER              PIC X(11)  VALUE "&LFINTRATE=".
000091    05 OUT-LF-INT-RATE     PIC 99.99999.
000092    05 FILLER              PIC X(10) VALUE "&AUTOPYDT=".
000093    05 OUT-AUTO-PY-DT      PIC X(21).
000094    05 FILLER              PIC X(12) VALUE "&AUTOSCHEND=".
000095    05 OUT-AUTO-SCHD-END   PIC X(21).
000096    05 FILLER              PIC X(8) VALUE "&LOANNO=".
000097    05 OUT-LOAN-NO         PIC X(25).
000098    05 FILLER              PIC X(5) VALUE "&APR=".
000099    05 OUT-APR             PIC 99.99999.
000100    05 FILLER              PIC X(8) VALUE "&LACTDT=".
000101    05 OUT-LST-ACT-DT      PIC X(21).
000102    05 FILLER              PIC X(7) VALUE "&ACTDT=".
000103    05 OUT-ACT-DT          PIC X(21).
000104    05 FILLER              PIC X(10) VALUE "&LACTTYPE=".
000105    05 OUT-LST-ACT-TYPE    PIC X(15).
000106    05 FILLER              PIC X(9) VALUE "&ACTTYPE=".
000107    05 OUT-ACT-TYPE        PIC X(15).
000108    05 FILLER              PIC X(6) VALUE "&FORM=".
000109    05 OUT-FORM            PIC X(4).
000110    05 FILLER              PIC X(9) VALUE "&INSNAME=".
000111    05 OUT-INS-NAME        PIC X(36).
000112    05 FILLER              PIC X(10) VALUE "&INSADDR1=".
000113    05 OUT-INS-ADDR1       PIC X(36).
000114    05 FILLER              PIC X(10) VALUE "&INSADDR2=".
000115    05 OUT-INS-ADDR2       PIC X(36).
000116    05 FILLER              PIC X(9) VALUE "&INSCITY=".
000117    05 OUT-INS-CITY        PIC X(28).
000118    05 FILLER              PIC X(10) VALUE "&INSSTATE=".
000119    05 OUT-INS-STATE       PIC XX.
000120    05 FILLER              PIC X(8) VALUE "&INSZIP=".
000121    05 OUT-INS-ZIP         PIC X(10).
000122    05 FILLER              PIC X(10) VALUE "&INSPHONE=".
000123    05 OUT-INS-PHONE       PIC X(13).
000124    05 FILLER              PIC X(10) VALUE "&BENENAME=".
000125    05 OUT-BEN-NAME        PIC X(32).
000126    05 FILLER              PIC X(11) VALUE "&BENEADDR1=".
000127    05 OUT-BEN-ADDR1       PIC X(36).
000128    05 FILLER              PIC X(11) VALUE "&BENEADDR2=".
000129    05 OUT-BEN-ADDR2       PIC X(32).
000130    05 FILLER              PIC X(10) VALUE "&BENECITY=".
000131    05 OUT-BEN-CITY        PIC X(28).
000132    05 FILLER              PIC X(11) VALUE "&BENESTATE=".
000133    05 OUT-BEN-STATE       PIC XX.
000134    05 FILLER              PIC X(9) VALUE "&BENEZIP=".
000135    05 OUT-BEN-ZIP         PIC X(10).
000136    05 FILLER              PIC X(11) VALUE "&BENEPHONE=".
000137    05 OUT-BEN-PHONE       PIC X(13).
000138    05 FILLER              PIC X(8) VALUE "&OANAME=".
000139    05 OUT-ORIG-NAME       PIC X(32).
000140    05 FILLER              PIC X(10) VALUE "&ACCTNAME=".
000141    05 OUT-ACCT-NAME       PIC X(32).
000142    05 FILLER              PIC X(11) VALUE "&ACCTADDR1=".
000143    05 OUT-ACCT-ADDR1      PIC X(36).
000144    05 FILLER              PIC X(11) VALUE "&ACCTADDR2=".
000145    05 OUT-ACCT-ADDR2      PIC X(32).
000146    05 FILLER              PIC X(10) VALUE "&ACCTCITY=".
000147    05 OUT-ACCT-CITY       PIC X(30).
000148    05 FILLER              PIC X(11) VALUE "&ACCTSTATE=".
000149    05 OUT-ACCT-STATE      PIC XX.
000150    05 FILLER              PIC X(9)  VALUE "&ACCTZIP=".
000151    05 OUT-ACCT-ZIP        PIC X(10).
000152    05 FILLER              PIC X(11) VALUE "&ACCTPHONE=".
000153    05 OUT-ACCT-PHONE      PIC X(13).
000154    05 FILLER              PIC X(7) VALUE "&ENCCD=".
000155    05 OUT-ENC-CODE        PIC XXX.
000156    05 FILLER              PIC X(7) VALUE "&ENCST=".
000157    05 OUT-ENC-ST          PIC XX.
000158    05 FILLER              PIC X(8) VALUE "&ARCHNO=".
000159    05 OUT-ARCHNO          PIC 999999999.
000160    05 FILLER              PIC X(11) VALUE "&FLTRPRTDT=".
000161    05 OUT-1ST-LTR-PRT-DT  PIC X(21).
000162    05 FILLER              PIC X(11) VALUE "&NEXTDUEDT=".
000163    05 OUT-NEXT-DUE-DT     PIC X(21).
000164    05 FILLER              PIC X(8) VALUE "&JLNAME=".
000165    05 OUT-JLNAME          PIC X(15).
000166    05 FILLER              PIC X(8) VALUE "&JFNAME=".
000167    05 OUT-JFNAME          PIC X(10).
000168    05 FILLER              PIC X(9)  VALUE "&ENCLINE=".
000169    05 OUT-ENCLINE         PIC X(100).
000170    05 FILLER              PIC X(9) VALUE "&ENCATTS=".
000171    05 OUT-ENCATTS         PIC X(255).
000172    05 FILLER              PIC X(10) VALUE "&OUTSTACK=".
000173    05 OUT-OUTSTACK        PIC X.
000174    05 FILLER              PIC X(8) VALUE "&PROCID=".
000175    05 OUT-PROCID          PIC XXXX.
000176    05 FILLER              PIC X(8) VALUE "&COMPID=".
000177    05 OUT-COMPID          PIC XXX.
000178    05 FILLER              PIC X(8) VALUE "&PRTNOW=".
000179    05 OUT-PRINT-NOW-SW    PIC X.
000180    05 FILLER              PIC X(11) VALUE "&CYCLEDATE=".
000181    05 OUT-CYCLE-DATE      PIC X(8).
000182    05 FILLER              PIC X(11) VALUE "&ENCSTNAME=".
000183    05 OUT-ENC-ST-NAME     PIC XX.
000184    05 FILLER              PIC X(10) VALUE "&CARRIDLU=".
000185    05 OUT-CARR-LU         PIC X.
000186    05 FILLER              PIC X(5)  VALUE "&CCN=".
000187    05 OUT-AHL-CLAIM-NO    PIC X(9).
000188    05 FILLER              PIC X(4) VALUE "&GP=".
000189    05 OUT-ACCT-GPCD       PIC 99.
000190    05 FILLER              PIC X(9) VALUE "&MAXBENS=".
000191    05 OUT-MAX-BENS        PIC 9(3).
000192    05 FILLER              PIC X(10) VALUE "&PAIDBENS=".
000193    05 OUT-PAID-BENS       PIC 9(3).
000194    05 FILLER              PIC X(9) VALUE "&REMBENS=".
000195    05 OUT-REM-BENS        PIC 9(3).
000196    05 FILLER              PIC X(9) VALUE "&EXCLPER=".
000197    05 OUT-EXCL-PER        PIC 9(3).
000198    05 FILLER              PIC X(8) VALUE "&RECMOS=".
000199    05 OUT-REC-MOS         PIC 9(2).
000200    05 FILLER              PIC X(8) VALUE "&INSTYP=".
000201    05 OUT-INS-TYP         PIC X(4).
000202    05 FILLER              PIC X(8) VALUE "&BENPER=".
000203    05 OUT-BEN-PER         PIC 9(2).
000204    05 FILLER              PIC X(7) VALUE "&ACCSW=".
000205    05 OUT-ACC-SW          PIC X.
000206    05 FILLER              PIC X(7) VALUE "&VERCD=".
000207    05 OUT-VER-CD          PIC X(5).
      *<<((file: NSCVARS))
000286
      ****************************************************************
      *                                                               
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        
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
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        *
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
000288
       01  DFHCOMMAREA       PIC X(01).
000289 01  var  pic x(30).
000290
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'NSREQLTR' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000291 VCOBOL-DUMMY-PROCEDURE.
000292
000293*********************
000294* Receive web input
000295*********************
000296
000297     set P to address of KIXSYS
000298     CALL "getenv" using by value P returning var-ptr
000299     if var-ptr = null then
000300        display ' kixsys not set '
000301     else
000302        set address of var to var-ptr
000303        move 0 to env-var-len
000304        inspect var tallying env-var-len
000305          for characters before X'00'
000306*       DISPLAY '  KIXSYS = ' var (1:env-var-len)
000307        unstring var (1:env-var-len) delimited by '/'
000308           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000309              WS-KIX-SYS
000310        end-unstring
000311*       DISPLAY ' WS KIX SYS ' WS-KIXSYS
000312*       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
000313     end-if
000314
000315     
      * exec cics web
000316*       startbr formfield resp(w-resp)
000317*     end-exec.
      *    MOVE 'X(f                   &  N#00000944' TO DFHEIV0
           MOVE X'582866202020202020202020' &
                X'202020202020202020202620' &
                X'204E233030303030393434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000318
000319      perform read-form thru read-form-exit
000320         until w-resp not = 0 .
      *   dfhresp(normal)
000321
000322      
      * exec cics web
000323*       endbr formfield
000324*     end-exec.
      *    MOVE 'X,f                   #   #00000951' TO DFHEIV0
           MOVE X'582C66202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303030393531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000325
000326     MOVE IFF-CARRIER             TO OT-CARRIER
000327     MOVE IFF-CLAIM-NO            TO OT-CLMNO
000328     MOVE IFF-CERT-NO             TO OT-CRTNO
000329     MOVE IFF-LETTER-ID           TO OT-LETTER-ID
000330     MOVE IFF-FOLLOW-UP-DT        TO OT-FOLLOW-UP-DT
000331     MOVE IFF-RESEND-DT           TO OT-RESEND-DT
000332     MOVE IFF-NO-OF-COPIES        TO OT-NO-OF-COPIES
000333     MOVE IFF-PROC-ID             TO OT-PROC-ID
000334     MOVE IFF-COMP-ID             TO OT-COMP-ID
000335     MOVE IFF-PRINT-NOW-SW        TO OT-PRINT-NOW-SW
000336     MOVE IFF-ENC-CD              TO OT-ENC-CD
000337     MOVE ZEROS                   TO OT-ARCHIVE-NO
000338     MOVE IFF-REGARDING           TO OT-REGARDING
000339
000340*    DISPLAY ' INPUT FORM ' WS-KEY
000341*****************************************
000342* Invoke the LETTER business logic
000343*****************************************
000344
000345     
      * exec cics link
000346*       program('NSRLTRBL')
000347*       commarea(srch-commarea)
000348*    end-exec.
           MOVE LENGTH OF
            srch-commarea
             TO DFHEIV11
           MOVE 'NSRLTRBL' TO DFHEIV1
      *    MOVE '."C                   (   #00000974' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303030393734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 srch-commarea, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000349
000350*    DISPLAY ' MADE IT BACK FROM NSRLTRBL '
000351     IF BL-OK
000352*       DISPLAY ' BL OK ' BL-ARCHIVE-NO
000353        MOVE BL-RECORD-PASSED-DATA
000354                                 TO NAPER-OUTPUT-DATA
000355        MOVE BL-ARCHIVE-NO          TO OT-ARCHIVE-NO
000356     END-IF
000357
000358     move +1 to v2
000359     perform varying v1 from +1 by +1 until v1 > +1
000360        if ot-carrier (v1:1) <> spaces
000361           move ot-carrier(v1:1) to var-string(v2:1)
000362           add +1 to v2
000363        end-if
000364     end-perform
000365
000366     move '~~' to var-string(v2:2)
000367     add +2 to v2
000368     perform varying v1 from +1 by +1 until v1 > +7
000369        if ot-clmno (v1:1) <> spaces
000370           move ot-clmno(v1:1) to var-string(v2:1)
000371           add +1 to v2
000372        end-if
000373     end-perform
000374
000375     move '~~' to var-string(v2:2)
000376     add +2 to v2
000377     perform varying v1 from +1 by +1 until v1 > +11
000378        if ot-crtno (v1:1) <> spaces
000379           move ot-crtno(v1:1) to var-string(v2:1)
000380           add +1 to v2
000381        end-if
000382     end-perform
000383
000384     move '~~' to var-string(v2:2)
000385     add +2 to v2
000386     perform varying v1 from +1 by +1 until v1 > +4
000387        if ot-letter-id (v1:1) <> spaces
000388           move ot-letter-id(v1:1) to var-string(v2:1)
000389           add +1 to v2
000390        end-if
000391     end-perform
000392
000393     move '~~' to var-string(v2:2)
000394     add +2 to v2
000395     perform varying v1 from +1 by +1 until v1 > +10
000396        if ot-follow-up-dt (v1:1) <> spaces
000397           move ot-follow-up-dt(v1:1) to var-string(v2:1)
000398           add +1 to v2
000399        end-if
000400     end-perform
000401
000402     move '~~' to var-string(v2:2)
000403     add +2 to v2
000404     perform varying v1 from +1 by +1 until v1 > +10
000405        if ot-resend-dt (v1:1) <> spaces
000406           move ot-resend-dt(v1:1) to var-string(v2:1)
000407           add +1 to v2
000408        end-if
000409     end-perform
000410
000411     move '~~' to var-string(v2:2)
000412     add +2 to v2
000413     perform varying v1 from +1 by +1 until v1 > +2
000414        if ot-no-of-copies (v1:1) <> spaces
000415           move ot-no-of-copies(v1:1) to var-string(v2:1)
000416           add +1 to v2
000417        end-if
000418     end-perform
000419
000420     move '~~' to var-string(v2:2)
000421     add +2 to v2
000422     perform varying v1 from +1 by +1 until v1 > +4
000423        if ot-proc-id (v1:1) <> spaces
000424           move ot-proc-id(v1:1) to var-string(v2:1)
000425           add +1 to v2
000426        end-if
000427     end-perform
000428
000429     move '~~' to var-string(v2:2)
000430     add +2 to v2
000431     perform varying v1 from +1 by +1 until v1 > +3
000432        if ot-comp-id (v1:1) <> spaces
000433           move ot-comp-id(v1:1) to var-string(v2:1)
000434           add +1 to v2
000435        end-if
000436     end-perform
000437
000438     move '~~' to var-string(v2:2)
000439     add +2 to v2
000440     perform varying v1 from +1 by +1 until v1 > +1
000441        if ot-print-now-sw (v1:1) <> spaces
000442           move ot-print-now-sw(v1:1) to var-string(v2:1)
000443           add +1 to v2
000444        end-if
000445     end-perform
000446
000447     move '~~' to var-string(v2:2)
000448     add +2 to v2
000449     perform varying v1 from +1 by +1 until v1 > +3
000450        if ot-enc-cd (v1:1) <> spaces
000451           move ot-enc-cd(v1:1) to var-string(v2:1)
000452           add +1 to v2
000453        end-if
000454     end-perform
000455
000456     move '~~' to var-string(v2:2)
000457     add +2 to v2
000458     perform varying v1 from +1 by +1 until v1 > +8
000459        if ot-archive-no (v1:1) <> spaces
000460           move ot-archive-no(v1:1) to var-string(v2:1)
000461           add +1 to v2
000462        end-if
000463     end-perform
000464
000465     perform varying m1 from +70 by -1 until
000466        (m1 < +1)
000467        or (ot-regarding(m1:1) <> spaces)
000468     end-perform
000469
000470     move +1 to a2
000471     if m1 > +0
000472        MOVE SPACES              TO WS-WORK-FIELD
000473        perform varying a1 from +1 by +1 until a1 > m1
000474           if ot-regarding(a1:1) = ' '
000475              move '~'           to ws-work-field(a2:1)
000476              add +1 to a2
000477           else
000478              move ot-regarding(a1:1) to ws-work-field(a2:1)
000479              add +1 to a2
000480           end-if
000481        end-perform
000482     end-if
000483
000484     move ws-work-field to ot-regarding
000485
000486     move '~~' to var-string(v2:2)
000487     add +2 to v2
000488     perform varying v1 from +1 by +1 until v1 > +70
000489        if ot-regarding (v1:1) <> spaces and '&'
000490           move ot-regarding(v1:1) to var-string(v2:1)
000491           add +1 to v2
000492        end-if
000493     end-perform
000494
000495     move finished-string        to ws-lt-rest
000496     evaluate ws-kix-myenv
000497        when 'cid1p'
000498           move finished-string  to ws-sl-rest
000499           move '7001'           to ws-sl-port
000500        when 'mdoff'
000501           move finished-string  to ws-sl-rest
000502           move '7003'           to ws-sl-port
000503        when 'paul'
000504           move '5002'           to ws-lt-port
000505        when 'tony'
000506           move '6003'           to ws-lt-port
000507        when 'ahltst'
000508           move '6007'           to ws-lt-port
000509        when other
000510           move '6002'           to ws-lt-port
000511     end-evaluate
000512
000513*    MOVE X'04'                  TO WS-ELMSTR-COMPANY-CD
000514*    MOVE IFF-CARRIER            TO WS-ELMSTR-CARRIER
000515*    MOVE IFF-CLAIM-NO           TO WS-ELMSTR-CLAIM-NO
000516*    MOVE IFF-CERT-NO            TO WS-ELMSTR-CERT-NO
000517*
000518*    EXEC CICS READ
000519*         DATASET    ('ELMSTR')
000520*         INTO       (CLAIM-MASTER)
000521*         RIDFLD     (WS-ELMSTR-KEY)
000522*         RESP       (WS-RESPONSE)
000523*    END-EXEC.
000524*
000525*    IF RESP-NORMAL
000526*       MOVE IFF-LETTER-ID       TO OUT-TEMP
000527*                                   OUT-LETTER
000528*       MOVE CL-CARRIER          TO OUT-CARR
000529*       MOVE CL-INSURED-LAST-NAME
000530*                                TO OUT-ILNAME
000531*       MOVE CL-INSURED-1ST-NAME TO OUT-IFNAME
000532*       MOVE CL-CLAIM-NO         TO OUT-CLMNO
000533*       MOVE CL-CERT-NO          TO OUT-CRTNO
000534*       MOVE CL-CERT-ACCOUNT     TO OUT-ACTNO
000535*    END-IF
000536
000537*    exec cics link program('NSRLTRBL')
000538*       commarea(srch-commarea)
000539*    end-exec.
000540
000541***********************************************************
000542* Build output document.  There are three templates used
000543* for this document.  PEMHDR and PEMFTR are the header
000544* and footer, respectively.  PEMBOD is used for the
000545* actual data.  For each array entry in the business
000546* logic output, set the symbol list from the array
000547* entry and insert into the document using the PEMBOD
000548* template.
000549***********************************************************
000550
000551*    move bl-output-message to out-msg-text.
000552
000553     evaluate true
000554        when iff-comp-id = 'DCC'
000555           IF WS-KIX-MYENV = 'cid1p'
000556              
      * exec cics document create
000557*                doctoken   (w-doctoken)
000558*                template   ('PEMHDR')
000559*                symbollist (WS-PROD-DCC-PEMHDR)
000560*                resp       (WS-RESPONSE)
000561*             end-exec
           MOVE LENGTH OF
            WS-PROD-DCC-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001185' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031313835' TO DFHEIV0
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
000562           ELSE
000563              
      * exec cics document create
000564*                doctoken   (w-doctoken)
000565*                template   ('PEMHDR')
000566*                symbollist (WS-TEST-DCC-PEMHDR)
000567*                resp       (WS-RESPONSE)
000568*             end-exec
           MOVE LENGTH OF
            WS-TEST-DCC-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001192' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031313932' TO DFHEIV0
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
000569           END-IF
000570        when IFF-COMP-ID = 'AHL'
000571           IF WS-KIX-MYENV = 'cid1p'
000572              
      * exec cics document create
000573*                doctoken   (w-doctoken)
000574*                template   ('PEMHDR')
000575*                symbollist (WS-PROD-AHL-PEMHDR)
000576*                resp       (WS-RESPONSE)
000577*             end-exec
           MOVE LENGTH OF
            WS-PROD-AHL-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001201' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031323031' TO DFHEIV0
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
000578           ELSE
000579              
      * exec cics document create
000580*                doctoken   (w-doctoken)
000581*                template   ('PEMHDR')
000582*                symbollist (WS-TEST-AHL-PEMHDR)
000583*                resp       (WS-RESPONSE)
000584*             end-exec
           MOVE LENGTH OF
            WS-TEST-AHL-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001208' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031323038' TO DFHEIV0
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
000585           END-IF
000586        when IFF-COMP-ID = 'VPP'
000587           IF WS-KIX-MYENV = 'cid1p'
000588              
      * exec cics document create
000589*                doctoken   (w-doctoken)
000590*                template   ('PEMHDR')
000591*                symbollist (WS-PROD-VPP-PEMHDR)
000592*                resp       (WS-RESPONSE)
000593*             end-exec
           MOVE LENGTH OF
            WS-PROD-VPP-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001217' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031323137' TO DFHEIV0
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
000594           ELSE
000595              
      * exec cics document create
000596*                doctoken   (w-doctoken)
000597*                template   ('PEMHDR')
000598*                symbollist (WS-TEST-VPP-PEMHDR)
000599*                resp       (WS-RESPONSE)
000600*             end-exec
           MOVE LENGTH OF
            WS-TEST-VPP-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001224' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031323234' TO DFHEIV0
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
000601           END-IF
000602        when IFF-COMP-ID = 'FNL'
000603           IF WS-KIX-MYENV = 'cid1p'
000604              
      * exec cics document create
000605*                doctoken   (w-doctoken)
000606*                template   ('PEMHDR')
000607*                symbollist (WS-PROD-FNL-PEMHDR)
000608*                resp       (WS-RESPONSE)
000609*             end-exec
           MOVE LENGTH OF
            WS-PROD-FNL-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001233' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031323333' TO DFHEIV0
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
000610           ELSE
000611              
      * exec cics document create
000612*                doctoken   (w-doctoken)
000613*                template   ('PEMHDR')
000614*                symbollist (WS-TEST-FNL-PEMHDR)
000615*                resp       (WS-RESPONSE)
000616*             end-exec
           MOVE LENGTH OF
            WS-TEST-FNL-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001240' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031323430' TO DFHEIV0
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
000617           END-IF
000618        when other
000619           IF WS-KIX-MYENV = 'cid1p'
000620              
      * exec cics document create
000621*                doctoken   (w-doctoken)
000622*                template   ('PEMHDR')
000623*                symbollist (WS-PROD-CID-PEMHDR)
000624*                resp       (WS-RESPONSE)
000625*             end-exec
           MOVE LENGTH OF
            WS-PROD-CID-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001249' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031323439' TO DFHEIV0
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
000626           ELSE
000627              
      * exec cics document create
000628*                doctoken   (w-doctoken)
000629*                template   ('PEMHDR')
000630*                symbollist (WS-TEST-CID-PEMHDR)
000631*                resp       (WS-RESPONSE)
000632*             end-exec
           MOVE LENGTH OF
            WS-TEST-CID-PEMHDR
             TO DFHEIV16
           MOVE 'PEMHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001256' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031323536' TO DFHEIV0
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
000633           END-IF
000634     end-evaluate
000635
000636*    DISPLAY ' PEMHDR DOC CREATE ' WS-RESPONSE
000637
000638*    move 1 to bl-index.
000639*
000640*    perform bl-output-record-count times
000641*
000642*        move bl-output-account-number(bl-index)
000643*          to out-account-number
000644*        move bl-output-last-name(bl-index)
000645*          to out-last-name
000646*        move bl-output-first-name(bl-index)
000647*          to out-first-name
000648*        move bl-output-middle-initial(bl-index)
000649*          to out-middle-initial
000650*
000651
000652*    MOVE 'CICM'                 TO OUT-TEMP
000653*    MOVE 'CICM'                 TO OUT-LETTER
000654*    MOVE '9'                    TO OUT-CARR
000655*    MOVE 'MCDANIEL'             TO OUT-ILNAME
000656*    MOVE 'KATHI'                TO OUT-IFNAME
000657*    MOVE '0151216'              TO OUT-CLMNO
000658*    MOVE '0009235906 '          TO OUT-CRTNO
000659*    MOVE '0990000208'           TO OUT-ACTNO
000660
000661     
      * exec cics document set
000662*       doctoken(w-doctoken)
000663*       symbollist(NAPER-OUTPUT-DATA)
000664*       length(length of NAPER-OUTPUT-DATA)
000665*                   resp   (WS-RESPONSE)
000666*    end-exec
           MOVE LENGTH OF
            NAPER-OUTPUT-DATA TO DFHEIV16
      *    MOVE '\(Ds L                ''  N#00001290' TO DFHEIV0
           MOVE X'5C284473204C202020202020' &
                X'202020202020202020202720' &
                X'204E233030303031323930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 NAPER-OUTPUT-DATA, 
                 DFHEIV99, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000667
000668     
      * exec cics document insert
000669*       doctoken(w-doctoken)
000670*       template('PEMBOD')
000671*                   resp   (WS-RESPONSE)
000672*    end-exec
           MOVE 'PEMBOD' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001297' TO DFHEIV0
           MOVE X'5C2444742020202020202020' &
                X'202020202020202020202820' &
                X'204E233030303031323937' TO DFHEIV0
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
000673
000674     INSPECT OT-REGARDING REPLACING ALL '&' BY ' '
000675
000676     if ws-kix-myenv = 'cid1p' or 'mdoff'
000677        compute ws-sl-var-length =
000678           (ws-sl-var-length + v2) - +1
000679        
      * exec cics document set
000680*          doctoken(w-doctoken)
000681*          symbollist(WS-VAR-SLUNIKIX)
000682*          length(ws-sl-var-length)
000683*                      resp   (WS-RESPONSE)
000684*       end-exec
      *    MOVE '\(Ds L                ''  N#00001308' TO DFHEIV0
           MOVE X'5C284473204C202020202020' &
                X'202020202020202020202720' &
                X'204E233030303031333038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 WS-VAR-SLUNIKIX, 
                 DFHEIV99, 
                 ws-sl-var-length, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000685     else
000686        compute ws-lt-var-length =
000687           (ws-lt-var-length + v2) - +1
000688        
      * exec cics document set
000689*          doctoken(w-doctoken)
000690*          symbollist(WS-VAR-LOGICTEST)
000691*          length(ws-lt-var-length)
000692*                      resp   (WS-RESPONSE)
000693*       end-exec
      *    MOVE '\(Ds L                ''  N#00001317' TO DFHEIV0
           MOVE X'5C284473204C202020202020' &
                X'202020202020202020202720' &
                X'204E233030303031333137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 WS-VAR-LOGICTEST, 
                 DFHEIV99, 
                 ws-lt-var-length, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000694     end-if
000695
000696     
      * exec cics document insert
000697*       doctoken(w-doctoken)
000698*       template('PEMFTR')
000699*                   resp   (WS-RESPONSE)
000700*    end-exec
           MOVE 'PEMFTR' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001325' TO DFHEIV0
           MOVE X'5C2444742020202020202020' &
                X'202020202020202020202820' &
                X'204E233030303031333235' TO DFHEIV0
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
000701
000702****************************************
000703* Send the document and return.
000704****************************************
000705
000706     
      * exec cics web send
000707*       doctoken(w-doctoken)
000708*                   resp   (WS-RESPONSE)
000709*    end-exec.
      *    MOVE 'X$D                   *  N#00001335' TO DFHEIV0
           MOVE X'582444202020202020202020' &
                X'202020202020202020202A20' &
                X'204E233030303031333335' TO DFHEIV0
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
           
000710
000711*    DISPLAY ' WEB SEND  ' WS-RESPONSE
000712
000713*    DISPLAY ' ABOUT TO RETURN '
000714     
      * exec cics
000715*       return
000716*    end-exec.
      *    MOVE '.(                    ''   #00001343' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303031333433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000717
000718
000719******************************************************
000720* Read all fields of the incoming form, moving
000721* each to the corresponding field of the commarea
000722* (business logic input fields).
000723******************************************************
000724
000725 read-form.
000726     move spaces to w-form-name.
000727     move length of w-form-name to w-form-name-len.
000728           move spaces to w-form-value.
000729     move length of w-form-value to w-form-value-len.
000730     
      * exec cics web readnext
000731*                  formfield(w-form-name)
000732*                  namelength(w-form-name-len)
000733*                  value(w-form-value)
000734*                  valuelength(w-form-value-len)
000735*                  resp(w-resp)
000736*    end-exec.
      *    MOVE 'X*FLVL                &  N#00001359' TO DFHEIV0
           MOVE X'582A464C564C202020202020' &
                X'202020202020202020202620' &
                X'204E233030303031333539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-form-name, 
                 w-form-name-len, 
                 w-form-value, 
                 w-form-value-len, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000737     evaluate w-resp
000738        when 0 
      *   dfhresp(normal)
000739           evaluate w-form-name(1:w-form-name-len)
000740              when 'carrier'
000741                 if w-form-value-len not = 0
000742                    move w-form-value(1:w-form-value-len)
000743                                                   to IFF-CARRIER
000744                                                      BL-CARRIER
000745                 else
000746                                move spaces to IFF-CARRIER
000747                 end-if
000748              when 'claim_no'
000749                             if w-form-value-len not = 0
000750                    move w-form-value(1:w-form-value-len)
000751                                                   to IFF-CLAIM-NO
000752                                                      BL-CLAIM-NO
000753                 else
000754                                move spaces  to IFF-CLAIM-NO
000755                 end-if
000756              when 'cert_no'
000757                             if w-form-value-len not = 0
000758                    move w-form-value(1:w-form-value-len)
000759                                 to IFF-CERT-NO
000760                                    BL-CERT-NO
000761                 else
000762                                move spaces  to IFF-CERT-NO
000763                 end-if
000764              when 'letter_id'
000765                             if w-form-value-len not = 0
000766                    move w-form-value(1:w-form-value-len)
000767                                 to IFF-LETTER-ID
000768                                    BL-LETTER-ID
000769                 else
000770                    move spaces  to IFF-LETTER-ID
000771                 end-if
000772              when 'fu_date'
000773                             if w-form-value-len not = 0
000774                    move w-form-value(1:w-form-value-len)
000775                                 to IFF-FOLLOW-UP-DT
000776                                    BL-FOLLOW-UP-DT
000777                 else
000778                                move spaces  to IFF-FOLLOW-UP-DT
000779                 end-if
000780              when 'rs_date'
000781                             if w-form-value-len not = 0
000782                    move w-form-value(1:w-form-value-len)
000783                                 to IFF-RESEND-DT
000784                                    BL-RESEND-DT
000785                 else
000786                                move spaces  to IFF-RESEND-DT
000787                 end-if
000788              when 'no_copies'
000789                             if w-form-value-len not = 0
000790                    move w-form-value(1:w-form-value-len)
000791                                 to IFF-NO-OF-COPIES
000792                                    BL-NO-OF-COPIES
000793                 else
000794                    move zeros   to IFF-NO-OF-COPIES
000795                 end-if
000796              when 'proc_id'
000797                             if w-form-value-len not = 0
000798                    move w-form-value(1:w-form-value-len)
000799                                 to IFF-PROC-ID
000800                                    BL-PROC-ID
000801                 else
000802                    move 'KMSB'  to IFF-PROC-ID
000803                 end-if
000804              when 'comp_id'
000805                             if w-form-value-len not = 0
000806                    move w-form-value(1:w-form-value-len)
000807                                 to IFF-COMP-ID
000808                                    BL-COMP-ID
000809                 else
000810                    move 'CID'   to IFF-COMP-ID
000811                 end-if
000812              when 'prt_now'
000813                             if w-form-value-len not = 0
000814                    move w-form-value(1:w-form-value-len)
000815                                 to IFF-PRINT-NOW-SW
000816                                    BL-PRINT-NOW-SW
000817                 else
000818                    move 'N'     to IFF-PRINT-NOW-SW
000819                 end-if
000820              when 'enc_cd'
000821                             if w-form-value-len not = 0
000822                    move w-form-value(1:w-form-value-len)
000823                                 to IFF-ENC-CD
000824                                    BL-ENC-CD
000825                 else
000826                    move '0'     to IFF-ENC-CD
000827                 end-if
000828              when 'regarding'
000829                             if w-form-value-len not = 0
000830                    move w-form-value(1:w-form-value-len)
000831                                 to IFF-REGARDING
000832                                    BL-REGARDING
000833                 else
000834                    move SPACES  to IFF-REGARDING BL-REGARDING
000835                 end-if
000836           end-evaluate
000837        when other
000838           continue
000839     end-evaluate.
000840 read-form-exit.
000841
000842

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
