      *((program: NSRASLTR.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. NSRASLTR.
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
000018* 060611    2011022800001  PEMA  NEW PROGRAM
000019* 080612    2011022800001  AJRA  ADD AHL
000020* 101812  CR2012101700002  AJRA  ADD ENDARCH AND SCREENID
000021* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
000022* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
000023* 061421  CR2017031500001  PEMA  Update to CCM8
000024******************************************************************
000025 ENVIRONMENT DIVISION.
000026
000027 DATA DIVISION.
000028 working-storage section.
       01  DFH-START PIC X(04).
000029
000030 77  ws-comm-length      pic 9(4) BINARY.
000031
000032 01  P pointer.
000033 01  KIXHOST             pic x(9) value Z"HOSTNAME".
000034
000035 01  WS-KIXHOST                  PIC X(10).
000036 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000037 01  var-ptr pointer.
000038 01  env-var-len                 pic 9(4)  binary.
000039 01  rc                          pic 9(9)  binary.
000040 01  bl-length                   pic s9(5) value +0 comp-3.
000041 01  sl-length                   pic 9(8) binary.
000042
000043 01  WS-KIXSYS.
000044     05  WS-KIX-FIL1             PIC X(10).
000045     05  WS-KIX-APPS             PIC X(10).
000046     05  WS-KIX-ENV              PIC X(10).
000047     05  WS-KIX-MYENV            PIC X(10).
000048     05  WS-KIX-SYS              PIC X(10).
000049
000050************************************************
000051* commarea passed to the business logic
000052************************************************
000053
000054 01 srch-commarea.
000055*                                copy ELCADLTRSPI.
      *>>((file: ELCADLTRSPI))
000001******************************************************************
000002*                   C H A N G E   L O G
000003*
000004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000005*-----------------------------------------------------------------
000006*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000007* EFFECTIVE    NUMBER
000008*-----------------------------------------------------------------
000009* 060611    2011022800001  PEMA  NEW COPYBOOK
000010* 101812    2012101700002  AJRA  ADD ENDT ARCHIVE NO, SCREENID
000011* 110612    2012101700002  AJRA  EXPAND PASSED DATA
000012******************************************************************
000013****************************************
000014*  commarea for NaperSoft On Demand Admin services letters
000015*  (business logic input & output)
000016****************************************
000017
000018     03  BL-INPUT.
000019         05  BL-DATA-SRCE        PIC X.
000020         05  BL-LETTER-ID        PIC XXXX.
000021         05  BL-CARRIER          PIC X.
000022         05  BL-GROUP            PIC X(6).
000023         05  BL-STATE            PIC XX.
000024         05  BL-ACCOUNT          PIC X(10).
000025         05  BL-EFF-DT           PIC X(10).
000026         05  BL-CERT-NO          PIC X(11).
000027         05  BL-BATCH-NO         PIC X(6).
000028         05  BL-BATCH-SEQ        PIC 9(8).
000029         05  BL-RESP-NO          PIC X(10).
000030         05  BL-NO-OF-COPIES     PIC 99.
000031         05  BL-PROC-ID          PIC XXXX.
000032         05  BL-COMP-ID          PIC XXX.
000033         05  BL-PRINT-NOW-SW     PIC X.
000034         05  BL-ENC-CD           PIC XXX.
000035         05  BL-RESEND-DT        PIC X(10).
000036         05  BL-FOLLOW-UP-DT     PIC X(10).
000037         05  BL-ARCHIVE-NO       PIC 9(8).
000038         05  BL-FUNC             PIC X(8).
000039         05  BL-COMMENTS         PIC X(100).
000040         05  FILLER REDEFINES BL-COMMENTS.
000041             10  BL-REASON-CODE OCCURS 12 PIC X(4).
000042             10  BL-LETTER-TO-ACCT PIC X.
000043             10  BL-LETTER-TO-BENE PIC X.
000044             10  BL-WRITE-ERARCH   PIC X.
000045                 88  ERARCH-QWS      VALUE 'Q'.
000046                 88  ERARCH-BATCH    VALUE 'B'.
000047                 88  ERARCH-TEMP     VALUE 'T'.
000048             10  BL-PROCESS-TYPE PIC X(07).
000049             10  BL-CERT-FORM-ID PIC X(05).
000050             10  BL-ENDT-ARCH-NO PIC 9(08) BINARY.
000051             10  BL-SOURCE-SCREEN PIC X(8).
000052             10  FILLER          PIC X(25).
000053
000054     03  BL-OUTPUT.
000055         05  BL-STATUS                   PIC X.
000056             88  BL-OK                      VALUE "P".
000057             88  BL-FAIL                  VALUE "F".
000058         05  BL-MESSAGE          PIC X(50).
000059     03  BL-RECORD-PASSED-DATA   PIC X(6200).
000060     03  FILLER                  PIC X(31).
      *<<((file: ELCADLTRSPI))
000056
000057 01  INPUT-FROM-FORM.
000058     05  IFF-DATA-SRCE           PIC X.
000059     05  IFF-LETTER-ID           PIC XXXX.
000060     05  IFF-CARRIER             PIC X.
000061     05  IFF-GROUP               PIC X(6).
000062     05  IFF-STATE               PIC XX.
000063     05  IFF-ACCOUNT             PIC X(10).
000064     05  IFF-EFF-DT              PIC X(10).
000065     05  IFF-CERT-NO             PIC X(11).
000066     05  IFF-BATCH-NO            PIC X(6).
000067     05  IFF-BATCH-SEQ           PIC 9(8).
000068     05  IFF-RESP-NO             PIC X(10).
000069     05  IFF-NO-OF-COPIES        PIC 99.
000070     05  IFF-PROC-ID             PIC XXXX.
000071     05  IFF-COMP-ID             PIC XXX.
000072     05  IFF-PRINT-NOW-SW        PIC X.
000073     05  IFF-ENC-CD              PIC XXX.
000074     05  IFF-RESEND-DT           PIC X(10).
000075     05  IFF-FOLLOW-UP-DT        PIC X(10).
000076     05  IFF-ARCHIVE-NO          PIC 9(08).
000077     05  IFF-FUNC                PIC X(08).
000078*    05  IFF-COMMENTS            PIC X(70).
000079     05  IFF-ENDT-ARCH-NO        PIC 9(08).
000080
000081************************************
000082* fields used to read web data
000083************************************
000084
000085 01  w-form-name       pic x(80).
000086 01  w-form-value      pic x(80).
000087 01  w-form-name-len   pic s9(8) comp.
000088 01  w-form-value-len  pic s9(8) comp.
000089 01  w-resp            pic s9(8) comp.
000090 01  w-doctoken        pic x(16).
000091
000092
000093* COMP ID TPE REGION   GROUP NAME       HOST          HTTP PORT
000094*
000095*  CID      CID1P      AcctServCID  sdv-nsft01       7001
000096*  CID      MDOFF      AcctServCID  hov-nsft01       7003
000097*  CID      CID1T      AcctServCID  hov-nsft01       6002
000098*  CID      PAUL       AcctServCID  hov-nsft01       5002
000099*  CID      TONY       AcctServCID  hov-nsft01       6003
000100*  DCC      CID1P      AcctServDCC  sdv-nsft01       7001
000101*  DCC      MDOFF      AcctServDCC  hov-nsft01       7003
000102*  DCC      CID1T      AcctServDCC  hov-nsft01       6002
000103*  DCC      PAUL       AcctServDCC  hov-nsft01       5002
000104*  DCC      TONY       AcctServDCC  hov-nsft01       6003
000105*  AHL      CID1P      AcctServAHL  sdv-nsft01       7001
000106*  AHL      MDOFF      AcctServAHL  hov-nsft01       7003
000107*  AHL      CID1T      AcctServAHL  hov-nsft01       6002
000108*  AHL      PAUL       AcctServAHL  hov-nsft01       5002
000109*  AHL      TONY       AcctServAHL  hov-nsft01       6003
000110*  AHL      AHLTST     AcctServAHL  hov-nsft01       6007
000111*  FNL      CID1P      AcctServFNL  sdv-nsft01       7001
000112*  FNL      MDOFF      AcctServFNL  hov-nsft01       7003
000113*  FNL      CID1T      AcctServFNL  hov-nsft01       6002
000114*  FNL      PAUL       AcctServFNL  hov-nsft01       5002
000115*  FNL      TONY       AcctServFNL  hov-nsft01       6003
000116*  FNL      AHLTST     AcctServFNL  hov-nsft01       6007
000117
000118
000119******************************************
000120* symbol list text for ASERR template
000121******************************************
000122
000123 01  WS-ASERR.
000124     05  F                       PIC X(6)  VALUE "PGMDT=".
000125     05  ASERR-DT                PIC X(12) VALUE ' '.
000126     05  F                       PIC X(5)  VALUE '&MSG='.
000127     05  ASERR-MESS              PIC X(50) VALUE ' '.
000128
000129******************************************
000130* symbol list text for ASHDR template
000131******************************************
000132
000133 01  WS-PROD-CID-ASHDR.
000134     05  F                       PIC X(7)  VALUE "SERVER=".
000135     05  F                       PIC X(20) VALUE
000136                                    'sdv-nsft01.cso.local'.
000137     05  F                       PIC X(11) VALUE '&GROUPNAME='.
000138     05  F                       PIC X(11) VALUE 'AcctServCID'.
000139     05  F                       PIC X(7)  VALUE '&UNAME='.
000140     05  F                       PIC X(8)  VALUE 'batchjob'.
000141     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000142     05  F                       PIC X(8)  VALUE 'batchjob'.
000143
000144 01  WS-PROD-DCC-ASHDR.
000145     05  F                       PIC X(7)  VALUE "SERVER=".
000146     05  F                       PIC X(20) VALUE
000147                                    'sdv-nsft01.cso.local'.
000148     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000149     05  F                       PIC X(11) VALUE 'AcctServDCC'.
000150     05  F                       PIC X(7)  VALUE '&UNAME='.
000151     05  F                       PIC X(8)  VALUE 'batchjob'.
000152     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000153     05  F                       PIC X(8)  VALUE 'batchjob'.
000154
000155 01  WS-PROD-VPP-ASHDR.
000156     05  F                       PIC X(7)  VALUE "SERVER=".
000157     05  F                       PIC X(20) VALUE
000158                                    'sdv-nsft01.cso.local'.
000159     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000160     05  F                       PIC X(11) VALUE 'AcctServVPP'.
000161     05  F                       PIC X(7)  VALUE '&UNAME='.
000162     05  F                       PIC X(8)  VALUE 'batchjob'.
000163     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000164     05  F                       PIC X(8)  VALUE 'batchjob'.
000165
000166 01  WS-PROD-AHL-ASHDR.
000167     05  F                       PIC X(7)  VALUE "SERVER=".
000168     05  F                       PIC X(20) VALUE
000169                                    'sdv-nsft01.cso.local'.
000170     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000171     05  F                       PIC X(11) VALUE 'AcctServAHL'.
000172     05  F                       PIC X(7)  VALUE '&UNAME='.
000173     05  F                       PIC X(8)  VALUE 'batchjob'.
000174     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000175     05  F                       PIC X(8)  VALUE 'batchjob'.
000176
000177 01  WS-PROD-FNL-ASHDR.
000178     05  F                       PIC X(7)  VALUE "SERVER=".
000179     05  F                       PIC X(20) VALUE
000180                                    'sdv-nsft01.cso.local'.
000181     05  F                       PIC X(11) VALUE '&GROUPNAME='.
000182     05  F                       PIC X(11) VALUE 'AcctServFNL'.
000183     05  F                       PIC X(7)  VALUE '&UNAME='.
000184     05  F                       PIC X(8)  VALUE 'batchjob'.
000185     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000186     05  F                       PIC X(8)  VALUE 'batchjob'.
000187
000188 01  WS-TEST-CID-ASHDR.
000189     05  F                       PIC X(7)  VALUE "SERVER=".
000190     05  F                       PIC X(20) VALUE
000191                          'hov-nsft02.cso.local'.
000192     05  F                       PIC X(11) VALUE '&GROUPNAME='.
000193     05  F                       PIC X(11) VALUE 'AcctServCID'.
000194     05  F                       PIC X(7)  VALUE '&UNAME='.
000195     05  F                       PIC X(8)  VALUE 'batchjob'.
000196     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000197     05  F                       PIC X(8)  VALUE 'batchjob'.
000198
000199 01  WS-TEST-DCC-ASHDR.
000200     05  F                       PIC X(7)  VALUE "SERVER=".
000201     05  F                       PIC X(20) VALUE
000202                          'hov-nsft02.cso.local'.
000203     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000204     05  F                       PIC X(11) VALUE 'AcctServDCC'.
000205     05  F                       PIC X(7)  VALUE '&UNAME='.
000206     05  F                       PIC X(8)  VALUE 'batchjob'.
000207     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000208     05  F                       PIC X(8)  VALUE 'batchjob'.
000209
000210 01  WS-TEST-VPP-ASHDR.
000211     05  F                       PIC X(7)  VALUE "SERVER=".
000212     05  F                       PIC X(20) VALUE
000213                          'hov-nsft02.cso.local'.
000214     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000215     05  F                       PIC X(11) VALUE 'AcctServVPP'.
000216     05  F                       PIC X(7)  VALUE '&UNAME='.
000217     05  F                       PIC X(8)  VALUE 'batchjob'.
000218     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000219     05  F                       PIC X(8)  VALUE 'batchjob'.
000220
000221 01  WS-TEST-AHL-ASHDR.
000222     05  F                       PIC X(7)  VALUE "SERVER=".
000223     05  F                       PIC X(20) VALUE
000224                          'hov-nsft02.cso.local'.
000225     05  F                       PIC X(11) VALUE "&GROUPNAME=".
000226     05  F                       PIC X(11) VALUE 'AcctServAHL'.
000227     05  F                       PIC X(7)  VALUE '&UNAME='.
000228     05  F                       PIC X(8)  VALUE 'batchjob'.
000229     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000230     05  F                       PIC X(8)  VALUE 'batchjob'.
000231
000232 01  WS-TEST-FNL-ASHDR.
000233     05  F                       PIC X(7)  VALUE "SERVER=".
000234     05  F                       PIC X(20) VALUE
000235                          'hov-nsft02.cso.local'.
000236     05  F                       PIC X(11) VALUE '&GROUPNAME='.
000237     05  F                       PIC X(11) VALUE 'AcctServFNL'.
000238     05  F                       PIC X(7)  VALUE '&UNAME='.
000239     05  F                       PIC X(8)  VALUE 'batchjob'.
000240     05  F                       PIC X(11) VALUE '&UPASSWORD='.
000241     05  F                       PIC X(8)  VALUE 'batchjob'.
000242
000243******************************************
000244* symbol list text for ASFTR template
000245******************************************
000246
000247 01  WS-VAR-SLUNIKIX.
000248     05  FILLER                  PIC X(18) VALUE
000249                                 "HOSTINFO=slunikix:".
000250     05  WS-SL-PORT              PIC XXXX  VALUE '7001'.
000251     05  FILLER                  PIC X(11)  VALUE "&URLVARLST=".
000252     05  WS-SL-KEY               PIC X(32)  VALUE SPACES.
000253*    05  FILLER                  PIC X(20) VALUE
000254*                                "&HOSTINFO2=slunikix:".
000255*    05  WS-SL-PORT2             PIC XXXX  VALUE '7001'.
000256*    05  FILLER                  PIC X(12)  VALUE "&URLVARLST2=".
000257*    05  WS-SL-KEY2              PIC X(12)  VALUE SPACES.
000258
000259 01  WS-VAR-LOGICTEST.
000260     05  FILLER                  PIC X(19) VALUE
000261                                 "HOSTINFO=logictest:".
000262     05  WS-LT-PORT              PIC XXXX  VALUE '6002'.
000263     05  FILLER                  PIC X(11)  VALUE "&URLVARLST=".
000264     05  WS-LT-KEY               PIC X(32)  VALUE SPACES.
000265*    05  FILLER                  PIC X(21) VALUE
000266*                                "&HOSTINFO2=logictest:".
000267*    05  WS-LT-PORT2             PIC XXXX  VALUE '6002'.
000268*    05  FILLER                  PIC X(12)  VALUE "&URLVARLST2=".
000269*    05  WS-LT-KEY2              PIC X(12)  VALUE SPACES.
000270
000271 01 WS-ASFTR.
000272    05  OT-COMP-ID           PIC XXX.
000273    05  OT-PRINT-NOW-SW      PIC X.
000274    05  OT-ARCHIVE-NO        PIC 9(08).
000275    05  OT-FUNC              PIC X(06).
000276    05  ot-batch-no          pic x(06).
000277    05  ot-batch-seq-no      pic x(08).
000278
000279
000280 01 output-msg.
000281    05 filler              pic x(4) value "MSG=".
000282    05 out-msg-text        pic x(50).
000283
000284 01  MISC.
000285     12  WS-RESPONSE             PIC S9(8)   COMP.
000286         88  RESP-NORMAL                  VALUE +00.
000287         88  RESP-NOTFND                  VALUE +13.
000288         88  RESP-DUPREC                  VALUE +14.
000289         88  RESP-DUPKEY                  VALUE +15.
000290         88  RESP-NOTOPEN                 VALUE +19.
000291         88  RESP-ENDFILE                 VALUE +20.
000292
000293*                                COPY ELCDATE.
      *>>((file: ELCDATE))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDATE.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003
000007*                                                                *
000008*                                                                *
000009*   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
000010*                 LENGTH = 200                                   *
000011******************************************************************
000012
000013 01  DATE-CONVERSION-DATA.
000014     12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
000015     12  DC-OPTION-CODE                PIC X.
000016         88  BIN-TO-GREG                VALUE ' '.
000017         88  ELAPSED-BETWEEN-BIN        VALUE '1'.
000018         88  EDIT-GREG-TO-BIN           VALUE '2'.
000019         88  YMD-GREG-TO-BIN            VALUE '3'.
000020         88  MDY-GREG-TO-BIN            VALUE '4'.
000021         88  JULIAN-TO-BIN              VALUE '5'.
000022         88  BIN-PLUS-ELAPSED           VALUE '6'.
000023         88  FIND-CENTURY               VALUE '7'.
000024         88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
000025         88  EDIT-GREG-TO-BIN-3         VALUE '9'.
000026         88  YMD-GREG-TO-BIN-3          VALUE 'A'.
000027         88  MDY-GREG-TO-BIN-3          VALUE 'B'.
000028         88  JULIAN-TO-BIN-3            VALUE 'C'.
000029         88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
000030         88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
000031         88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
000032         88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
000033         88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
000034         88  CHECK-LEAP-YEAR            VALUE 'H'.
000035         88  BIN-3-TO-GREG              VALUE 'I'.
000036         88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
000037         88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
000038         88  CYMD-GREG-TO-BIN           VALUE 'L'.
000039         88  MDCY-GREG-TO-BIN           VALUE 'M'.
000040         88  MDY-GREG-TO-JULIAN         VALUE 'N'.
000041         88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
000042         88  YMD-GREG-TO-JULIAN         VALUE 'P'.
000043         88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
000044         88  THREE-CHARACTER-BIN
000045                  VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
000046         88  GREGORIAN-TO-BIN
000047                  VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
000048         88  BIN-TO-GREGORIAN
000049                  VALUES ' ' '1' 'I' '8' 'G'.
000050         88  JULIAN-TO-BINARY
000051                  VALUES '5' 'C' 'E' 'F'.
000052     12  DC-ERROR-CODE                 PIC X.
000053         88  NO-CONVERSION-ERROR        VALUE ' '.
000054         88  DATE-CONVERSION-ERROR
000055                  VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
000056         88  DATE-IS-ZERO               VALUE '1'.
000057         88  DATE-IS-NON-NUMERIC        VALUE '2'.
000058         88  DATE-IS-INVALID            VALUE '3'.
000059         88  DATE1-GREATER-DATE2        VALUE '4'.
000060         88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
000061         88  DATE-INVALID-OPTION        VALUE '9'.
000062         88  INVALID-CENTURY            VALUE 'A'.
000063         88  ONLY-CENTURY               VALUE 'B'.
000064         88  ONLY-LEAP-YEAR             VALUE 'C'.
000065         88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
000066     12  DC-END-OF-MONTH               PIC X.
000067         88  CALCULATE-END-OF-MONTH     VALUE '1'.
000068     12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
000069         88  USE-NORMAL-PROCESS         VALUE ' '.
000070         88  ADJUST-DOWN-100-YRS        VALUE '1'.
000071         88  ADJUST-UP-100-YRS          VALUE '2'.
000072     12  FILLER                        PIC X.
000073     12  DC-CONVERSION-DATES.
000074         16  DC-BIN-DATE-1             PIC XX.
000075         16  DC-BIN-DATE-2             PIC XX.
000076         16  DC-GREG-DATE-1-EDIT       PIC X(08).
000077         16  DC-GREG-DATE-1-EDIT-R REDEFINES
000078                       DC-GREG-DATE-1-EDIT.
000079             20  DC-EDIT1-MONTH        PIC 99.
000080             20  SLASH1-1              PIC X.
000081             20  DC-EDIT1-DAY          PIC 99.
000082             20  SLASH1-2              PIC X.
000083             20  DC-EDIT1-YEAR         PIC 99.
000084         16  DC-GREG-DATE-2-EDIT       PIC X(08).
000085         16  DC-GREG-DATE-2-EDIT-R REDEFINES
000086                     DC-GREG-DATE-2-EDIT.
000087             20  DC-EDIT2-MONTH        PIC 99.
000088             20  SLASH2-1              PIC X.
000089             20  DC-EDIT2-DAY          PIC 99.
000090             20  SLASH2-2              PIC X.
000091             20  DC-EDIT2-YEAR         PIC 99.
000092         16  DC-GREG-DATE-1-YMD        PIC 9(06).
000093         16  DC-GREG-DATE-1-YMD-R  REDEFINES
000094                     DC-GREG-DATE-1-YMD.
000095             20  DC-YMD-YEAR           PIC 99.
000096             20  DC-YMD-MONTH          PIC 99.
000097             20  DC-YMD-DAY            PIC 99.
000098         16  DC-GREG-DATE-1-MDY        PIC 9(06).
000099         16  DC-GREG-DATE-1-MDY-R REDEFINES
000100                      DC-GREG-DATE-1-MDY.
000101             20  DC-MDY-MONTH          PIC 99.
000102             20  DC-MDY-DAY            PIC 99.
000103             20  DC-MDY-YEAR           PIC 99.
000104         16  DC-GREG-DATE-1-ALPHA.
000105             20  DC-ALPHA-MONTH        PIC X(10).
000106             20  DC-ALPHA-DAY          PIC 99.
000107             20  FILLER                PIC XX.
000108             20  DC-ALPHA-CENTURY.
000109                 24 DC-ALPHA-CEN-N     PIC 99.
000110             20  DC-ALPHA-YEAR         PIC 99.
000111         16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
000112         16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
000113         16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
000114         16  DC-JULIAN-DATE            PIC 9(05).
000115         16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
000116                                       PIC 9(05).
000117         16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
000118             20  DC-JULIAN-YEAR        PIC 99.
000119             20  DC-JULIAN-DAYS        PIC 999.
000120         16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
000121         16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
000122         16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
000123     12  DATE-CONVERSION-VARIBLES.
000124         16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
000125         16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
000126             20  FILLER                PIC 9(3).
000127             20  HOLD-CEN-1-CCYY.
000128                 24  HOLD-CEN-1-CC     PIC 99.
000129                 24  HOLD-CEN-1-YY     PIC 99.
000130             20  HOLD-CEN-1-MO         PIC 99.
000131             20  HOLD-CEN-1-DA         PIC 99.
000132         16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
000133             20  HOLD-CEN-1-R-MO       PIC 99.
000134             20  HOLD-CEN-1-R-DA       PIC 99.
000135             20  HOLD-CEN-1-R-CCYY.
000136                 24  HOLD-CEN-1-R-CC   PIC 99.
000137                 24  HOLD-CEN-1-R-YY   PIC 99.
000138             20  FILLER                PIC 9(3).
000139         16  HOLD-CENTURY-1-X.
000140             20  FILLER                PIC X(3)  VALUE SPACES.
000141             20  HOLD-CEN-1-X-CCYY.
000142                 24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
000143                 24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
000144             20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
000145             20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
000146         16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
000147             20  HOLD-CEN-1-R-X-MO     PIC XX.
000148             20  HOLD-CEN-1-R-X-DA     PIC XX.
000149             20  HOLD-CEN-1-R-X-CCYY.
000150                 24  HOLD-CEN-1-R-X-CC PIC XX.
000151                 24  HOLD-CEN-1-R-X-YY PIC XX.
000152             20  FILLER                PIC XXX.
000153         16  DC-BIN-DATE-EXPAND-1      PIC XXX.
000154         16  DC-BIN-DATE-EXPAND-2      PIC XXX.
000155         16  DC-JULIAN-DATE-1          PIC 9(07).
000156         16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
000157             20  DC-JULIAN-1-CCYY.
000158                 24  DC-JULIAN-1-CC    PIC 99.
000159                 24  DC-JULIAN-1-YR    PIC 99.
000160             20  DC-JULIAN-DA-1        PIC 999.
000161         16  DC-JULIAN-DATE-2          PIC 9(07).
000162         16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
000163             20  DC-JULIAN-2-CCYY.
000164                 24  DC-JULIAN-2-CC    PIC 99.
000165                 24  DC-JULIAN-2-YR    PIC 99.
000166             20  DC-JULIAN-DA-2        PIC 999.
000167         16  DC-GREG-DATE-A-EDIT.
000168             20  DC-EDITA-MONTH        PIC 99.
000169             20  SLASHA-1              PIC X VALUE '/'.
000170             20  DC-EDITA-DAY          PIC 99.
000171             20  SLASHA-2              PIC X VALUE '/'.
000172             20  DC-EDITA-CCYY.
000173                 24  DC-EDITA-CENT     PIC 99.
000174                 24  DC-EDITA-YEAR     PIC 99.
000175         16  DC-GREG-DATE-B-EDIT.
000176             20  DC-EDITB-MONTH        PIC 99.
000177             20  SLASHB-1              PIC X VALUE '/'.
000178             20  DC-EDITB-DAY          PIC 99.
000179             20  SLASHB-2              PIC X VALUE '/'.
000180             20  DC-EDITB-CCYY.
000181                 24  DC-EDITB-CENT     PIC 99.
000182                 24  DC-EDITB-YEAR     PIC 99.
000183         16  DC-GREG-DATE-CYMD         PIC 9(08).
000184         16  DC-GREG-DATE-CYMD-R REDEFINES
000185                              DC-GREG-DATE-CYMD.
000186             20  DC-CYMD-CEN           PIC 99.
000187             20  DC-CYMD-YEAR          PIC 99.
000188             20  DC-CYMD-MONTH         PIC 99.
000189             20  DC-CYMD-DAY           PIC 99.
000190         16  DC-GREG-DATE-MDCY         PIC 9(08).
000191         16  DC-GREG-DATE-MDCY-R REDEFINES
000192                              DC-GREG-DATE-MDCY.
000193             20  DC-MDCY-MONTH         PIC 99.
000194             20  DC-MDCY-DAY           PIC 99.
000195             20  DC-MDCY-CEN           PIC 99.
000196             20  DC-MDCY-YEAR          PIC 99.
000197    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
000198        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
000199    12  DC-EL310-DATE                  PIC X(21).
000200    12  FILLER                         PIC X(28).
      *<<((file: ELCDATE))
000294
000295*****************************************
000296* symbol list for the ASBOD template
000297*****************************************
000298
000299*                                COPY NSCASVARS.
      *>>((file: NSCASVARS))
000001******************************************************************
000002*                   C H A N G E   L O G
000003*
000004*        CURRENT SIZE - 6180
000005*
000006* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000007*-----------------------------------------------------------------
000008*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000009* EFFECTIVE    NUMBER
000010*-----------------------------------------------------------------
000011* 101612  CR2011022800001  AJRA  REMOVE SIGN FROM CHG TOTALS
000012* 110612  CR2012101700002  AJRA  ADD NEW FIELDS
000013* 091213  CR2013090300001  AJRA  ADD NEXT BUSINESS DATE
000014* 121015  CR2015100900001  TANA  ADD VIN NUMBER
000015******************************************************************
000016 01 NAPER-OUTPUT-DATA.
000017    05  F                       PIC X(07) VALUE "LETTER=".
000018    05  OUT-LETTER              PIC X(06) VALUE SPACES.
000019    05  F                       PIC X(08) VALUE "~PROCID=".
000020    05  OUT-PROC-ID             PIC X(04) VALUE SPACES.
000021    05  F                       PIC X(10) VALUE "~PROCNAME=".
000022    05  OUT-PROC-NAME           PIC X(30) VALUE SPACES.
000023    05  F                       PIC X(11) VALUE "~PROCTITLE=".
000024    05  OUT-PROC-TITLE          PIC X(30) VALUE SPACES.
000025    05  F                       PIC X(09) VALUE "~CSRNAME=".
000026    05  OUT-CSR-NAME            PIC X(30) VALUE SPACES.
000027    05  F                       PIC X(10) VALUE "~CSRTITLE=".
000028    05  OUT-CSR-TITLE           PIC X(30) VALUE SPACES.
000029    05  F                       PIC X(06) VALUE "~CARR=".
000030    05  OUT-CARRIER             PIC X(01) VALUE SPACES.
000031    05  F                       PIC X(10) VALUE "~GROUPING=".
000032    05  OUT-GROUPING            PIC X(06) VALUE SPACES.
000033    05  F                       PIC X(08) VALUE "~CERTST=".
000034    05  OUT-STATE               PIC X(02) VALUE SPACES.
000035    05  F                       PIC X(09) VALUE "~ACCOUNT=".
000036    05  OUT-ACCOUNT             PIC X(10) VALUE SPACES.
000037    05  F                       PIC X(11) VALUE "~CERTEFFDT=".
000038    05  OUT-CERT-EFF-DT         PIC X(21) VALUE SPACES.
000039    05  F                       PIC X(07) VALUE "~CRTNO=".
000040    05  OUT-CERT-NO             PIC X(11) VALUE SPACES.
000041    05  F                       PIC X(08) VALUE "~CRTSUF=".
000042    05  OUT-CERT-SFX            PIC X(01) VALUE SPACES.
000043    05  F                       PIC X(08) VALUE "~ILNAME=".
000044    05  OUT-ILNAME              PIC X(15) VALUE SPACES.
000045    05  F                       PIC X(08) VALUE "~IFNAME=".
000046    05  OUT-IFNAME              PIC X(10) VALUE SPACES.
000047    05  F                       PIC X(08) VALUE "~IFINIT=".
000048    05  OUT-IFINIT              PIC X(01) VALUE SPACES.
000049    05  F                       PIC X(08) VALUE "~IMINIT=".
000050    05  OUT-IMINIT              PIC X(01) VALUE SPACES.
000051    05  F                       PIC X(09) VALUE "~IISSAGE=".
000052    05  OUT-IAGE                PIC 99    BLANK WHEN ZERO.
000053    05  F                       PIC X(06) VALUE "~ISEX=".
000054    05  OUT-ISEX                PIC X(01) VALUE SPACES.
000055    05  F                       PIC X(10) VALUE "~INSADDR1=".
000056    05  OUT-INS-ADDR1           PIC X(30) VALUE SPACES.
000057    05  F                       PIC X(10) VALUE "~INSADDR2=".
000058    05  OUT-INS-ADDR2           PIC X(30) VALUE SPACES.
000059    05  F                       PIC X(09) VALUE "~INSCITY=".
000060    05  OUT-INS-CITY            PIC X(30) VALUE SPACES.
000061    05  F                       PIC X(10) VALUE "~INSSTATE=".
000062    05  OUT-INS-STATE           PIC X(02) VALUE SPACES.
000063    05  F                       PIC X(08) VALUE "~INSZIP=".
000064    05  OUT-INS-ZIP             PIC X(09) VALUE SPACES.
000065    05  F                       PIC X(05) VALUE "~SSN=".
000066    05  OUT-SOC-SEC-NO          PIC X(11) VALUE SPACES.
000067    05  F                       PIC X(07) VALUE "~MEMNO=".
000068    05  OUT-MEMBER-NO           PIC X(12) VALUE SPACES.
000069    05  F                       PIC X(08) VALUE "~JLNAME=".
000070    05  OUT-JLNAME              PIC X(15) VALUE SPACES.
000071    05  F                       PIC X(08) VALUE "~JFNAME=".
000072    05  OUT-JFNAME              PIC X(10) VALUE SPACES.
000073    05  F                       PIC X(08) VALUE "~JMINIT=".
000074    05  OUT-JMINIT              PIC X(01) VALUE SPACES.
000075    05  F                       PIC X(08) VALUE "~JNTAGE=".
000076    05  OUT-JAGE                PIC 99    BLANK WHEN ZERO.
000077    05  F                       PIC X(10) VALUE "~ACCTNAME=".
000078    05  OUT-ACCT-NAME           PIC X(30) VALUE SPACES.
000079    05  F                       PIC X(11) VALUE "~ACCTADDR1=".
000080    05  OUT-ACCT-ADDR1          PIC X(30) VALUE SPACES.
000081    05  F                       PIC X(11) VALUE "~ACCTADDR2=".
000082    05  OUT-ACCT-ADDR2          PIC X(30) VALUE SPACES.
000083    05  F                       PIC X(10) VALUE "~ACCTCITY=".
000084    05  OUT-ACCT-CITY           PIC X(30) VALUE SPACES.
000085    05  F                       PIC X(11) VALUE "~ACCTSTATE=".
000086    05  OUT-ACCT-STATE          PIC X(02) VALUE SPACES.
000087    05  F                       PIC X(09) VALUE "~ACCTZIP=".
000088    05  OUT-ACCT-ZIP            PIC X(10) VALUE SPACES.
000089    05  F                       PIC X(11) VALUE "~ACCTPHONE=".
000090    05  OUT-ACCT-PHONE          PIC X(10) VALUE SPACES.
000091    05  F                       PIC X(11) VALUE "~ACCTCNTRL=".
000092    05  OUT-ACCT-CNTRL-NAME     PIC X(30) VALUE SPACES.
000093    05  F                       PIC X(09) VALUE "~BUSTYPE=".
000094    05  OUT-ACCT-BUS-TYPE       PIC X(02) VALUE SPACES.
000095    05  F                       PIC X(10) VALUE "~BENENAME=".
000096    05  OUT-BENE-NAME           PIC X(30) VALUE SPACES.
000097    05  F                       PIC X(11) VALUE "~BENEADDR1=".
000098    05  OUT-BENE-ADDR1          PIC X(30) VALUE SPACES.
000099    05  F                       PIC X(11) VALUE "~BENEADDR2=".
000100    05  OUT-BENE-ADDR2          PIC X(30) VALUE SPACES.
000101    05  F                       PIC X(10) VALUE "~BENECITY=".
000102    05  OUT-BENE-CITY           PIC X(30) VALUE SPACES.
000103    05  F                       PIC X(11) VALUE "~BENESTATE=".
000104    05  OUT-BENE-STATE          PIC X(02) VALUE SPACES.
000105    05  F                       PIC X(09) VALUE "~BENEZIP=".
000106    05  OUT-BENE-ZIP            PIC X(09) VALUE SPACES.
000107    05  F                       PIC X(10) VALUE "~CARRNAME=".
000108    05  OUT-CARR-NAME           PIC X(30) VALUE SPACES.
000109    05  F                       PIC X(08) VALUE "~RESPNO=".
000110    05  OUT-RESP-NO             PIC X(10) VALUE SPACES.
000111    05  F                       PIC X(12) VALUE "~COACCTNAME=".
000112    05  OUT-COMP-NAME           PIC X(30) VALUE SPACES.
000113    05  F                       PIC X(12) VALUE "~COMAILNAME=".
000114    05  OUT-COMP-MAIL-TO        PIC X(30) VALUE SPACES.
000115    05  F                       PIC X(11) VALUE "~COMPADDR1=".
000116    05  OUT-COMP-ADDR1          PIC X(30) VALUE SPACES.
000117    05  F                       PIC X(11) VALUE "~COMPADDR2=".
000118    05  OUT-COMP-ADDR2          PIC X(30) VALUE SPACES.
000119    05  F                       PIC X(10) VALUE "~COMPCITY=".
000120    05  OUT-COMP-CITY           PIC X(30) VALUE SPACES.
000121    05  F                       PIC X(11) VALUE "~COMPSTATE=".
000122    05  OUT-COMP-STATE          PIC X(02) VALUE SPACES.
000123    05  F                       PIC X(09) VALUE "~COMPZIP=".
000124    05  OUT-COMP-ZIP            PIC X(09) VALUE SPACES.
000125    05  F                       PIC X(11) VALUE "~COMPPHONE=".
000126    05  OUT-COMP-PHONE          PIC X(10) VALUE SPACES.
000127    05  F                       PIC X(09) VALUE "~COMPFAX=".
000128    05  OUT-COMP-FAX            PIC X(10) VALUE SPACES.
000129    05  F                       PIC X(12) VALUE "~COMPSTATUS=".
000130    05  OUT-COMP-STATUS         PIC X(01) VALUE SPACES.
000131    05  F                       PIC X(12) VALUE "~COMPBILLSW=".
000132    05  OUT-BILL-SW             PIC X(01) VALUE SPACES.
000133    05  F                       PIC X(08) VALUE "~RPTCD1=".
000134    05  OUT-RPT-CD1             PIC X(10) VALUE SPACES.
000135    05  F                       PIC X(08) VALUE "~RPTCD2=".
000136    05  OUT-RPT-CD2             PIC X(10) VALUE SPACES.
000137    05  F                       PIC X(09) VALUE "~ENTRYDT=".
000138    05  OUT-ENTRY-DT            PIC X(21) VALUE SPACES.
000139    05  F                       PIC X(07) VALUE "~BATCH=".
000140    05  OUT-ENTRY-BATCH         PIC X(06) VALUE SPACES.
000141    05  F                       PIC X(11) VALUE "~ENTSTATUS=".
000142    05  OUT-ENTRY-STATUS        PIC X(01) VALUE SPACES.
000143    05  F                       PIC X(10) VALUE "~1STPMTDT=".
000144    05  OUT-1ST-PMT-DT          PIC X(21) VALUE SPACES.
000145    05  F                       PIC X(05) VALUE "~APR=".
000146    05  OUT-LOAN-APR            PIC 999.9(4)  BLANK WHEN ZERO.
000147    05  F                       PIC X(08) VALUE "~LNTERM=".
000148    05  OUT-LOAN-TERM           PIC 999   BLANK WHEN ZERO.
000149    05  F                       PIC X(11) VALUE "~RATECLASS=".
000150    05  OUT-RATE-CLASS          PIC X(02) VALUE SPACES.
000151    05  F                       PIC X(09) VALUE "~EXTDAYS=".
000152    05  OUT-EXT-DAYS            PIC 999   BLANK WHEN ZERO.
000153    05  F                       PIC X(09) VALUE "~CSRCODE=".
000154    05  OUT-CSR-CODE            PIC X(04) VALUE SPACES.
000155    05  F                       PIC X(07) VALUE "~UCODE=".
000156    05  OUT-UCODE               PIC X(01) VALUE SPACES.
000157    05  F                       PIC X(10) VALUE "~PREMTYPE=".
000158    05  OUT-PREM-TYPE           PIC X(01) VALUE SPACES.
000159    05  F                       PIC X(12) VALUE "~INDGRPTYPE=".
000160    05  OUT-IND-GRP             PIC X(01) VALUE SPACES.
000161    05  F                       PIC X(06) VALUE "~SKIP=".
000162    05  OUT-SKIP-CD             PIC X(01) VALUE SPACES.
000163    05  F                       PIC X(06) VALUE "~MODE=".
000164    05  OUT-PMT-MODE            PIC X(01) VALUE SPACES.
000165    05  F                       PIC X(09) VALUE "~LOANOFF=".
000166    05  OUT-LOAN-OFF            PIC X(05) VALUE SPACES.
000167    05  F                       PIC X(06) VALUE "~RTBL=".
000168    05  OUT-REIN-TABLE          PIC X(03) VALUE SPACES.
000169    05  F                       PIC X(10) VALUE "~SPECREIN=".
000170    05  OUT-SPEC-REIN           PIC X(01) VALUE SPACES.
000171    05  F                       PIC X(09) VALUE "~LFBENCD=".
000172    05  OUT-LF-BENCD            PIC X(02) VALUE SPACES.
000173    05  F                       PIC X(08) VALUE "~LFTERM=".
000174    05  OUT-LF-TERM             PIC 999   BLANK WHEN ZERO.
000175    05  F                       PIC X(07) VALUE "~LFDEV=".
000176    05  OUT-LF-DEV-CD           PIC X(03) VALUE SPACES.
000177    05  F                       PIC X(10) VALUE "~LFDEVPCT=".
000178    05  OUT-LF-DEV-PCT          PIC 9.9(6)  BLANK WHEN ZERO.
000179    05  F                       PIC X(07) VALUE "~LFBEN=".
000180    05  OUT-LF-BEN              PIC 9(9).99 BLANK WHEN ZERO.
000181    05  F                       PIC X(08) VALUE "~LFPREM=".
000182    05  OUT-LF-PRM              PIC 9(7).99 BLANK WHEN ZERO.
000183    05  F                       PIC X(08) VALUE "~LFABEN=".
000184    05  OUT-LF-ALT-BEN          PIC 9(9).99 BLANK WHEN ZERO.
000185    05  F                       PIC X(09) VALUE "~LFAPREM=".
000186    05  OUT-LF-ALT-PRM          PIC 9(7).99 BLANK WHEN ZERO.
000187    05  F                       PIC X(07) VALUE "~LFNSP=".
000188    05  OUT-LF-NSP              PIC 9(7).99 BLANK WHEN ZERO.
000189    05  F                       PIC X(08) VALUE "~LFRBEN=".
000190    05  OUT-LF-REM-BEN          PIC 9(9).99 BLANK WHEN ZERO.
000191    05  F                       PIC X(07) VALUE "~LFCAN=".
000192    05  OUT-LF-REF              PIC 9(7).99 BLANK WHEN ZERO.
000193    05  F                       PIC X(07) VALUE "~LFDTH=".
000194    05  OUT-LF-DTH              PIC 9(9).99 BLANK WHEN ZERO.
000195    05  F                       PIC X(08) VALUE "~LFRATE=".
000196    05  OUT-LF-RATE             PIC 99.9(5) BLANK WHEN ZERO.
000197    05  F                       PIC X(09) VALUE "~LFARATE=".
000198    05  OUT-LF-ALT-RATE         PIC 99.9(5) BLANK WHEN ZERO.
000199    05  F                       PIC X(09) VALUE "~LFEXPDT=".
000200    05  OUT-LF-EXP-DT           PIC X(21) VALUE SPACES.
000201    05  F                       PIC X(08) VALUE "~LFSTAT=".
000202    05  OUT-LF-CUR-STATUS       PIC X(01) VALUE SPACES.
000203    05  F                       PIC X(09) VALUE "~LFCANDT=".
000204    05  OUT-LF-CAN-DT           PIC X(21) VALUE SPACES.
000205    05  F                       PIC X(12) VALUE "~LFCANEXTDT=".
000206    05  OUT-LF-CAN-EXIT-DT      PIC X(21) VALUE SPACES.
000207    05  F                       PIC X(07) VALUE "~DTHDT=".
000208    05  OUT-LF-DTH-DT           PIC X(21) VALUE SPACES.
000209    05  F                       PIC X(10) VALUE "~DTHEXTDT=".
000210    05  OUT-LF-DTH-EXIT-DT      PIC X(21) VALUE SPACES.
000211    05  F                       PIC X(12) VALUE "~LFEXTBATCH=".
000212    05  OUT-LF-EXIT-BATCH       PIC X(06) VALUE SPACES.
000213    05  F                       PIC X(08) VALUE "~LFCOMM=".
000214    05  OUT-LF-COMM-PCT         PIC .9(5) BLANK WHEN ZERO.
000215    05  F                       PIC X(11) VALUE "~LFBENDESC=".
000216    05  OUT-LF-DESC             PIC X(02) VALUE SPACES.
000217    05  F                       PIC X(11) VALUE "~LFBENABRV=".
000218    05  OUT-LF-ABBRV            PIC X(03) VALUE SPACES.
000219    05  F                       PIC X(09) VALUE "~AHBENCD=".
000220    05  OUT-AH-BENCD            PIC X(02) VALUE SPACES.
000221    05  F                       PIC X(08) VALUE "~AHTERM=".
000222    05  OUT-AH-TERM             PIC 999   BLANK WHEN ZERO.
000223    05  F                       PIC X(11) VALUE "~AHCRITPER=".
000224    05  OUT-CRIT-PER            PIC 99    BLANK WHEN ZERO.
000225    05  F                       PIC X(09) VALUE "~AHDEVCD=".
000226    05  OUT-AH-DEV-CD           PIC X(03) VALUE SPACES.
000227    05  F                       PIC X(10) VALUE "~AHDEVPCT=".
000228    05  OUT-AH-DEV-PCT          PIC 9.9(6) BLANK WHEN ZERO.
000229    05  F                       PIC X(07) VALUE "~AHBEN=".
000230    05  OUT-AH-BEN              PIC 9(7).99 BLANK WHEN ZERO.
000231    05  F                       PIC X(08) VALUE "~AHPREM=".
000232    05  OUT-AH-PRM              PIC 9(7).99 BLANK WHEN ZERO.
000233    05  F                       PIC X(07) VALUE "~AHNSP=".
000234    05  OUT-AH-NSP              PIC 9(7).99 BLANK WHEN ZERO.
000235    05  F                       PIC X(07) VALUE "~AHCAN=".
000236    05  OUT-AH-REF              PIC 9(7).99 BLANK WHEN ZERO.
000237    05  F                       PIC X(10) VALUE "~AHITDPMT=".
000238    05  OUT-AH-CLM              PIC 9(7).99 BLANK WHEN ZERO.
000239    05  F                       PIC X(10) VALUE "~AHTOTBEN=".
000240    05  OUT-AH-TOT-BEN          PIC 9(9).99 BLANK WHEN ZERO.
000241    05  F                       PIC X(12) VALUE "~AHPDTHRUDT=".
000242    05  OUT-AH-PDTHRU-DT        PIC X(21) VALUE SPACES.
000243    05  F                       PIC X(08) VALUE "~AHRATE=".
000244    05  OUT-AH-RATE             PIC 99.9(5) BLANK WHEN ZERO.
000245    05  F                       PIC X(09) VALUE "~AHEXPDT=".
000246    05  OUT-AH-EXP-DT           PIC X(21) VALUE SPACES.
000247    05  F                       PIC X(08) VALUE "~AHSTAT=".
000248    05  OUT-AH-CUR-STATUS       PIC X(01) VALUE SPACES.
000249    05  F                       PIC X(09) VALUE "~AHCANDT=".
000250    05  OUT-AH-CAN-DT           PIC X(21) VALUE SPACES.
000251    05  F                       PIC X(12) VALUE "~AHCANEXTDT=".
000252    05  OUT-AH-CAN-EXIT-DT      PIC X(21) VALUE SPACES.
000253    05  F                       PIC X(12) VALUE "~AHEXTBATCH=".
000254    05  OUT-AH-EXIT-BATCH       PIC X(06) VALUE SPACES.
000255    05  F                       PIC X(08) VALUE "~AHCOMM=".
000256    05  OUT-AH-COMM-PCT         PIC .9(5) BLANK WHEN ZERO.
000257    05  F                       PIC X(11) VALUE "~AHBENDESC=".
000258    05  OUT-AH-DESC             PIC X(35) VALUE SPACES.
000259    05  F                       PIC X(14) VALUE "~AHBENRETELIM=".
000260    05  OUT-RET-ELIM            PIC X(01) VALUE SPACES.
000261    05  F                       PIC X(11) VALUE "~AHBENDAYS=".
000262    05  OUT-BEN-DAYS            PIC X(02) VALUE SPACES.
000263    05  F                       PIC X(08) VALUE "~AHWAIT=".
000264    05  OUT-WAIT-PER            PIC X(03) VALUE SPACES.
000265    05  F                       PIC X(11) VALUE "~AHMAXPMTS=".
000266    05  OUT-MAX-PMTS            PIC 99    VALUE ZEROS.
000267    05  OUT-MAX-PMTS-A REDEFINES OUT-MAX-PMTS
000268                                PIC X(2).
000269    05  F                       PIC X(09) VALUE "~TOTPREM=".
000270    05  OUT-TOT-PRM             PIC 9(7).99 BLANK WHEN ZERO.
000271    05  F                       PIC X(08) VALUE "~TOTREF=".
000272    05  OUT-TOT-REF             PIC 9(7).99 BLANK WHEN ZERO.
000273    05  F                       PIC X(08) VALUE "~SEXPDT=".
000274    05  OUT-SCHED-EXP-DT        PIC X(21) VALUE SPACES.
000275    05  F                       PIC X(07) VALUE "~STERM=".
000276    05  OUT-SCHED-TERM          PIC 999   BLANK WHEN ZERO.
000277    05  F                       PIC X(12) VALUE "~IORIGLNAME=".
000278    05  OUT-ORIG-ILNAME         PIC X(15) VALUE SPACES.
000279    05  F                       PIC X(12) VALUE "~IORIGFNAME=".
000280    05  OUT-ORIG-IFNAME         PIC X(10) VALUE SPACES.
000281    05  F                       PIC X(11) VALUE "~IORIGINIT=".
000282    05  OUT-ORIG-MINIT          PIC X(01) VALUE SPACES.
000283    05  F                       PIC X(10) VALUE "~IORIGAGE=".
000284    05  OUT-ORIG-IAGE           PIC 99    BLANK WHEN ZERO.
000285    05  F                       PIC X(12) VALUE "~JORIGLNAME=".
000286    05  OUT-ORIG-JLNAME         PIC X(15) VALUE SPACES.
000287    05  F                       PIC X(12) VALUE "~JORIGFNAME=".
000288    05  OUT-ORIG-JFNAME         PIC X(10) VALUE SPACES.
000289    05  F                       PIC X(11) VALUE "~JORIGINIT=".
000290    05  OUT-ORIG-JMINIT         PIC X(01) VALUE SPACES.
000291    05  F                       PIC X(10) VALUE "~JORIGAGE=".
000292    05  OUT-ORIG-JAGE           PIC 99    BLANK WHEN ZERO.
000293    05  F                       PIC X(13) VALUE "~LFORIGBENCD=".
000294    05  OUT-ORIG-LF-BENCD       PIC X(02) VALUE SPACES.
000295    05  F                       PIC X(12) VALUE "~LFORIGTERM=".
000296    05  OUT-ORIG-LF-TERM        PIC 999   BLANK WHEN ZERO.
000297    05  F                       PIC X(14) VALUE "~LFORIGBENAMT=".
000298    05  OUT-ORIG-LF-BEN         PIC 9(9).99 VALUE ZEROS.
000299    05  OUT-ORIG-LF-BEN-A REDEFINES OUT-ORIG-LF-BEN
000300                                PIC X(12).
000301    05  F                       PIC X(14) VALUE "~LFORIGPRMAMT=".
000302    05  OUT-ORIG-LF-PRM         PIC 9(9).99 VALUE ZEROS.
000303    05  OUT-ORIG-LF-PRM-A REDEFINES OUT-ORIG-LF-PRM
000304                                PIC X(12).
000305    05  F                       PIC X(14) VALUE "~LFCALCPRMAMT=".
000306    05  OUT-LF-CALC-PRM         PIC 9(9).99 VALUE ZEROS.
000307    05  F                       PIC X(14) VALUE "~LFORIGREFAMT=".
000308    05  OUT-ORIG-LF-REF         PIC 9(7).99 VALUE ZEROS.
000309    05  F                       PIC X(14) VALUE "~LFCALCREFAMT=".
000310    05  OUT-LF-CALC-REF         PIC 9(7).99 VALUE ZEROS.
000311    05  F                       PIC X(14) VALUE "~LFORIGALTBEN=".
000312    05  OUT-ORIG-LF-ALT-BEN     PIC 9(9).99 VALUE ZEROS.
000313    05  OUT-ORIG-LF-ALT-BEN-A REDEFINES OUT-ORIG-LF-ALT-BEN
000314                                PIC X(12).
000315    05  F                       PIC X(14) VALUE "~LFORIGALTPRM=".
000316    05  OUT-ORIG-LF-ALT-PRM     PIC 9(7).99 VALUE ZEROS.
000317    05  OUT-ORIG-LF-ALT-PRM-A REDEFINES OUT-ORIG-LF-ALT-PRM
000318                                PIC X(10).
000319    05  F                       PIC X(13) VALUE "~LFORIGEXPDT=".
000320    05  OUT-ORIG-LF-EXP-DT      PIC X(21) VALUE SPACES.
000321    05  F                       PIC X(12) VALUE "~LFORIGDESC=".
000322    05  OUT-ORIG-LF-DESC        PIC X(02) VALUE SPACES.
000323    05  F                       PIC X(13) VALUE "~AHORIGBENCD=".
000324    05  OUT-ORIG-AH-BENCD       PIC X(02) VALUE SPACES.
000325    05  F                       PIC X(12) VALUE "~AHORIGTERM=".
000326    05  OUT-ORIG-AH-TERM        PIC 999   BLANK WHEN ZERO.
000327    05  F                       PIC X(13) VALUE "~AHORIGCRITP=".
000328    05  OUT-ORIG-CRIT-PER       PIC 99    BLANK WHEN ZERO.
000329    05  F                       PIC X(14) VALUE "~AHORIGBENAMT=".
000330    05  OUT-ORIG-AH-BEN         PIC 9(7).99 VALUE ZEROS.
000331    05  OUT-ORIG-AH-BEN-A REDEFINES OUT-ORIG-AH-BEN
000332                                PIC X(10).
000333    05  F                       PIC X(14) VALUE "~AHORIGPRMAMT=".
000334    05  OUT-ORIG-AH-PRM         PIC 9(7).99 VALUE ZEROS.
000335    05  OUT-ORIG-AH-PRM-A REDEFINES OUT-ORIG-AH-PRM
000336                                PIC X(10).
000337    05  F                       PIC X(14) VALUE "~AHCALCPRMAMT=".
000338    05  OUT-AH-CALC-PRM         PIC 9(7).99 VALUE ZEROS.
000339    05  F                       PIC X(14) VALUE "~AHORIGREFAMT=".
000340    05  OUT-ORIG-AH-REF         PIC 9(7).99 VALUE ZEROS.
000341    05  F                       PIC X(14) VALUE "~AHCALCREFAMT=".
000342    05  OUT-AH-CALC-REF         PIC 9(7).99 VALUE ZEROS.
000343    05  F                       PIC X(13) VALUE "~AHORIGEXPDT=".
000344    05  OUT-ORIG-AH-EXP-DT      PIC X(21) VALUE SPACES.
000345    05  F                       PIC X(12) VALUE "~AHORIGDESC=".
000346    05  OUT-ORIG-AH-DESC        PIC X(35) VALUE SPACES.
000347    05  F                       PIC X(13) VALUE "~AHORIGRELIM=".
000348    05  OUT-ORIG-RET-ELIM       PIC X(01) VALUE SPACES.
000349    05  F                       PIC X(13) VALUE "~ORIGBENDAYS=".
000350    05  OUT-ORIG-BEN-DAYS       PIC X(02) VALUE ZEROS.
000351    05  F                       PIC X(10) VALUE "~ORIGWAIT=".
000352    05  OUT-ORIG-WAIT-PER       PIC X(03) VALUE SPACES.
000353    05  F                       PIC X(13) VALUE "~ORIGMAXPMTS=".
000354    05  OUT-ORIG-MAX-PMTS       PIC 99    VALUE ZEROS.
000355    05  OUT-ORIG-MAX-PMTS-A REDEFINES OUT-ORIG-MAX-PMTS
000356                                PIC X(2).
000357    05  F                       PIC X(09) VALUE "~OSEXPDT=".
000358    05  OUT-ORIG-SCHED-EXP-DT   PIC X(21) VALUE SPACES.
000359    05  F                       PIC X(08) VALUE "~OSTERM=".
000360    05  OUT-ORIG-SCHED-TERM     PIC 9(09) BLANK WHEN ZERO.
000361    05  F                       PIC X(11) VALUE "~INEWLNAME=".
000362    05  OUT-NEW-ILNAME          PIC X(15) VALUE SPACES.
000363    05  F                       PIC X(11) VALUE "~INEWFNAME=".
000364    05  OUT-NEW-IFNAME          PIC X(10) VALUE SPACES.
000365    05  F                       PIC X(10) VALUE "~INEWINIT=".
000366    05  OUT-NEW-IMINIT          PIC X(01) VALUE SPACES.
000367    05  F                       PIC X(09) VALUE "~INEWAGE=".
000368    05  OUT-NEW-IAGE            PIC 9(09) BLANK WHEN ZERO.
000369    05  OUT-NEW-IAGE-A REDEFINES OUT-NEW-IAGE
000370                                PIC X(09).
000371    05  F                       PIC X(11) VALUE "~JNEWLNAME=".
000372    05  OUT-NEW-JLNAME          PIC X(15) VALUE SPACES.
000373    05  F                       PIC X(11) VALUE "~JNEWFNAME=".
000374    05  OUT-NEW-JFNAME          PIC X(10) VALUE SPACES.
000375    05  F                       PIC X(10) VALUE "~JNEWINIT=".
000376    05  OUT-NEW-JMINIT          PIC X(01) VALUE SPACES.
000377    05  F                       PIC X(09) VALUE "~JNEWAGE=".
000378    05  OUT-NEW-JAGE            PIC 9(09) BLANK WHEN ZERO.
000379    05  OUT-NEW-JAGE-A REDEFINES OUT-NEW-JAGE
000380                                PIC X(09).
000381    05  F                       PIC X(12) VALUE "~LFNEWBENCD=".
000382    05  OUT-NEW-LF-BENCD        PIC X(09) VALUE SPACES.
000383    05  F                       PIC X(11) VALUE "~LFNEWTERM=".
000384    05  OUT-NEW-LF-TERM         PIC 9(09) BLANK WHEN ZERO.
000385    05  OUT-NEW-LF-TERM-A REDEFINES OUT-NEW-LF-TERM
000386                                PIC X(09).
000387    05  F                       PIC X(13) VALUE "~LFNEWBENAMT=".
000388    05  OUT-NEW-LF-BEN          PIC 9(9).99 VALUE ZEROS.
000389    05  OUT-NEW-LF-BEN-A REDEFINES OUT-NEW-LF-BEN
000390                                PIC X(12).
000391    05  F                       PIC X(13) VALUE "~LFNEWPRMAMT=".
000392    05  OUT-NEW-LF-PRM          PIC 9(7).99 VALUE ZEROS.
000393    05  OUT-NEW-LF-PRM-A REDEFINES OUT-NEW-LF-PRM
000394                                PIC X(10).
000395    05  F                       PIC X(13) VALUE "~LFNEWREFAMT=".
000396    05  OUT-NEW-LF-REF          PIC 9(7).99 VALUE ZEROS.
000397    05  F                       PIC X(13) VALUE "~LFNEWALTBEN=".
000398    05  OUT-NEW-LF-ALT-BEN      PIC 9(9).99 VALUE ZEROS.
000399    05  OUT-NEW-LF-ALT-BEN-A REDEFINES OUT-NEW-LF-ALT-BEN
000400                                PIC X(12).
000401    05  F                       PIC X(13) VALUE "~LFNEWALTPRM=".
000402    05  OUT-NEW-LF-ALT-PRM      PIC 9(7).99 VALUE ZEROS.
000403    05  OUT-NEW-LF-ALT-PRM-A REDEFINES OUT-NEW-LF-ALT-PRM
000404                                PIC X(10).
000405    05  F                       PIC X(12) VALUE "~LFNEWEXPDT=".
000406    05  OUT-NEW-LF-EXP-DT       PIC X(21) VALUE SPACES.
000407    05  F                       PIC X(11) VALUE "~LFNEWDESC=".
000408    05  OUT-NEW-LF-DESC         PIC X(10) VALUE SPACES.
000409    05  F                       PIC X(12) VALUE "~AHNEWBENCD=".
000410    05  OUT-NEW-AH-BENCD        PIC X(02) VALUE SPACES.
000411    05  F                       PIC X(11) VALUE "~AHNEWTERM=".
000412    05  OUT-NEW-AH-TERM         PIC 9(09) BLANK WHEN ZERO.
000413    05  OUT-NEW-AH-TERM-A REDEFINES OUT-NEW-AH-TERM
000414                                PIC X(09).
000415    05  F                       PIC X(12) VALUE "~AHNEWCRITP=".
000416    05  OUT-NEW-CRIT-PER        PIC 9(09) BLANK WHEN ZERO.
000417    05  OUT-NEW-CRIT-PER-A REDEFINES OUT-NEW-CRIT-PER
000418                                PIC X(09).
000419    05  F                       PIC X(13) VALUE "~AHNEWBENAMT=".
000420    05  OUT-NEW-AH-BEN          PIC 9(7).99 VALUE ZEROS.
000421    05  OUT-NEW-AH-BEN-A REDEFINES OUT-NEW-AH-BEN
000422                                PIC X(10).
000423    05  F                       PIC X(13) VALUE "~AHNEWPRMAMT=".
000424    05  OUT-NEW-AH-PRM          PIC 9(7).99 VALUE ZEROS.
000425    05  OUT-NEW-AH-PRM-A REDEFINES OUT-NEW-AH-PRM
000426                                PIC X(10).
000427    05  F                       PIC X(13) VALUE "~AHNEWREFAMT=".
000428    05  OUT-NEW-AH-REF          PIC 9(7).99 VALUE ZEROS.
000429    05  F                       PIC X(12) VALUE "~AHNEWEXPDT=".
000430    05  OUT-NEW-AH-EXP-DT       PIC X(21) VALUE SPACES.
000431    05  F                       PIC X(11) VALUE "~AHNEWDESC=".
000432    05  OUT-NEW-AH-DESC         PIC X(35) VALUE SPACES.
000433    05  F                       PIC X(12) VALUE "~AHNEWRELIM=".
000434    05  OUT-NEW-RET-ELIM        PIC X(01) VALUE SPACES.
000435    05  F                       PIC X(12) VALUE "~NEWBENDAYS=".
000436    05  OUT-NEW-BEN-DAYS        PIC X(09) VALUE ZEROS.
000437    05  F                       PIC X(09) VALUE "~NEWWAIT=".
000438    05  OUT-NEW-WAIT-PER        PIC X(09) VALUE SPACES.
000439    05  F                       PIC X(12) VALUE "~NEWMAXPMTS=".
000440    05  OUT-NEW-MAX-PMTS        PIC 9(09) VALUE ZEROS.
000441    05  OUT-NEW-MAX-PMTS-A REDEFINES OUT-NEW-MAX-PMTS
000442                                PIC X(09).
000443    05  F                       PIC X(09) VALUE "~NSEXPDT=".
000444    05  OUT-NEW-SCHED-EXP-DT    PIC X(21) VALUE SPACES.
000445    05  F                       PIC X(08) VALUE "~NSTERM=".
000446    05  OUT-NEW-SCHED-TERM      PIC 9(09) BLANK WHEN ZERO.
000447    05  OUT-NEW-SCHED-TERM-A REDEFINES OUT-NEW-SCHED-TERM
000448                                PIC X(09).
000449    05  F                       PIC X(12) VALUE "~TOTPREMCHG=".
000450    05  OUT-TOT-PRM-CHG         PIC 9(7).99 VALUE ZEROS.
000451    05  F                       PIC X(11) VALUE "~TOTREFCHG=".
000452    05  OUT-TOT-REF-CHG         PIC 9(7).99 VALUE ZEROS.
000453    05  F                       PIC X(11) VALUE "~PAYABLETO=".
000454    05  OUT-PAYEE               PIC X(30) VALUE SPACES.
000455    05  F                       PIC X(11) VALUE "~SIGNEEDED=".
000456    05  OUT-SIG-SW              PIC X(01) VALUE SPACES.
000457    05  F                       PIC X(09) VALUE "~BALLOON=".
000458    05  OUT-BALLOON-IND         PIC X(01) VALUE SPACES.
000459    05  F                       PIC X(10) VALUE "~LEASEIND=".
000460    05  OUT-LEASE-IND           PIC X(01) VALUE SPACES.
000461    05  F                       PIC X(09) VALUE "~RESCIND=".
000462    05  OUT-RESCIND             PIC X(01) VALUE SPACES.
000463    05  F                       PIC X(07) VALUE "~RCD01=".
000464    05  OUT-REA-CD1             PIC X(04) VALUE SPACES.
000465    05  F                       PIC X(07) VALUE "~RCD02=".
000466    05  OUT-REA-CD2             PIC X(04) VALUE SPACES.
000467    05  F                       PIC X(07) VALUE "~RCD03=".
000468    05  OUT-REA-CD3             PIC X(04) VALUE SPACES.
000469    05  F                       PIC X(07) VALUE "~RCD04=".
000470    05  OUT-REA-CD4             PIC X(04) VALUE SPACES.
000471    05  F                       PIC X(07) VALUE "~RCD05=".
000472    05  OUT-REA-CD5             PIC X(04) VALUE SPACES.
000473    05  F                       PIC X(07) VALUE "~RCD06=".
000474    05  OUT-REA-CD6             PIC X(04) VALUE SPACES.
000475    05  F                       PIC X(07) VALUE "~RCD07=".
000476    05  OUT-REA-CD7             PIC X(04) VALUE SPACES.
000477    05  F                       PIC X(07) VALUE "~RCD08=".
000478    05  OUT-REA-CD8             PIC X(04) VALUE SPACES.
000479    05  F                       PIC X(07) VALUE "~RCD09=".
000480    05  OUT-REA-CD9             PIC X(04) VALUE SPACES.
000481    05  F                       PIC X(07) VALUE "~RCD10=".
000482    05  OUT-REA-CD10            PIC X(04) VALUE SPACES.
000483    05  F                       PIC X(07) VALUE "~RCD11=".
000484    05  OUT-REA-CD11            PIC X(04) VALUE SPACES.
000485    05  F                       PIC X(07) VALUE "~RCD12=".
000486    05  OUT-REA-CD12            PIC X(04) VALUE SPACES.
000487    05  F                       PIC X(11) VALUE "~CYCLEDATE=".
000488    05  OUT-CYCLE-DT            PIC X(21) VALUE SPACES.
000489    05  F                       PIC X(08) VALUE "~ARCHNO=".
000490    05  OUT-ARCH-NO             PIC 9(9)  VALUE ZEROS.
000491    05  F                       PIC X(06) VALUE "~FORM=".
000492    05  OUT-FORM                PIC X(03) VALUE SPACES.
000493    05  F                       PIC X(09) VALUE "~ENCLINE=".
000494    05  OUT-ENC-LINE            PIC X(50) VALUE SPACES.
000495    05  F                       PIC X(08) VALUE "~ATTACH=".
000496    05  OUT-ATTACH              PIC X(50) VALUE SPACES.
000497    05  F                       PIC X(10) VALUE "~OUTSTACK=".
000498    05  OUT-STACK               PIC X(10) VALUE SPACES.
000499    05  F                       PIC X(11) VALUE "~STATENAME=".
000500    05  OUT-STATE-NAME          PIC X(25) VALUE SPACES.
000501    05  F                       PIC X(08) VALUE "~PRTNOW=".
000502    05  OUT-PRINT-NOW           PIC X(01) VALUE SPACES.
000503    05  F                       PIC X(05) VALUE "~NCB=".
000504    05  OUT-CHGBACK             PIC X     VALUE ' '.
000505    05  F                       PIC X(8) VALUE "~CSOAMT=".
000506    05  OUT-CSO-PORTION         PIC 9(7).99 VALUE ZEROS.
000507    05  F                       PIC X(9) VALUE "~ACCTAMT=".
000508    05  OUT-ACCT-PORTION        PIC 9(7).99 VALUE ZEROS.
000509    05  F                       PIC X(09) VALUE "~LETRTYP=".
000510    05  OUT-LETTER-TYPE         PIC X(01) VALUE SPACES.
000511    05  F                       PIC X(10) VALUE "~PRNTCERT=".
000512    05  OUT-PRINT-CERTIFICATE   PIC X(01) VALUE SPACES.
000513    05  F                       PIC X(08) VALUE "~INSBDT=".
000514    05  OUT-INS-BIRTHDATE       PIC X(21) VALUE SPACES.
000515    05  F                       PIC X(08) VALUE "~JNTBDT=".
000516    05  OUT-JNT-BIRTHDATE       PIC X(21) VALUE SPACES.
000517    05  F                       PIC X(08) VALUE "~TOTINT=".
000518    05  OUT-TOT-INTEREST        PIC 9(7).99 VALUE ZEROS.
000519    05  F                       PIC X(8) VALUE "~TOPREM=".
000520    05  OUT-ORIG-TOT-PREM       PIC 9(7).99 VALUE ZEROS.
000521    05  F                       PIC X(7) VALUE "~TOREF=".
000522    05  OUT-ORIG-TOT-REF        PIC 9(7).99 VALUE ZEROS.
000523    05  F                       PIC X(7) VALUE "~CANDT=".
000524    05  OUT-CANCEL-DT           PIC X(21) VALUE SPACES.
000525    05  F                       PIC X(9)  VALUE "~HLTHAPP=".
000526    05  OUT-HEALTH-APP          PIC X     VALUE SPACES.
000527    05  F                       PIC X(08) VALUE "~CERTID=".
000528    05  OUT-CERTIFICATE-ID      PIC X(5)  VALUE SPACES.
000529    05  F                       PIC X(08) VALUE "~UNDWID=".
000530    05  OUT-UNDW-ID             PIC X(04) VALUE SPACES.
000531    05  F                       PIC X(8) VALUE "~TNPREM=".
000532    05  OUT-NEW-TOT-PREM        PIC 9(7).99 VALUE ZEROS.
000533    05  F                       PIC X(7) VALUE "~TNREF=".
000534    05  OUT-NEW-TOT-REF         PIC 9(7).99 VALUE ZEROS.
000535    05  F                       PIC X(8)  VALUE "~COVIND=".
000536    05  OUT-COVERAGE-IND        PIC X(4)  VALUE SPACES.
000537    05  F                       PIC X(9)  VALUE "~OCOVIND=".
000538    05  OUT-ORIG-COVERAGE-IND   PIC X(4)  VALUE SPACES.
000539    05  F                       PIC X(9)  VALUE "~NCOVIND=".
000540    05  OUT-NEW-COVERAGE-IND    PIC X(4)  VALUE SPACES.
000541    05  F                       PIC X(10) VALUE "~LFCMBPRM=".
000542    05  OUT-LF-CMB-PREM         PIC 9(7).99 VALUE ZEROS.
000543    05  F                       PIC X(11) VALUE "~LFOCMBPRM=".
000544    05  OUT-ORIG-LF-CMB-PREM    PIC 9(7).99 VALUE ZEROS.
000545    05  F                       PIC X(11) VALUE "~LFNCMBPRM=".
000546    05  OUT-NEW-LF-CMB-PREM     PIC 9(7).99 VALUE ZEROS.
000547    05  OUT-NEW-LF-CMB-PREM-A REDEFINES OUT-NEW-LF-CMB-PREM
000548                                PIC X(10).
000549    05  F                       PIC X(11) VALUE "~LFPREMCHG=".
000550    05  OUT-LF-PREM-CHG         PIC 9(7).99 VALUE ZEROS.
000551    05  F                       PIC X(11) VALUE "~LFAPRMCHG=".
000552    05  OUT-LF-ALT-PREM-CHG     PIC 9(7).99 VALUE ZEROS.
000553    05  F                       PIC X(11) VALUE "~LFCPRMCHG=".
000554    05  OUT-LF-CMB-PREM-CHG     PIC 9(7).99 VALUE ZEROS.
000555    05  F                       PIC X(11) VALUE "~AHPREMCHG=".
000556    05  OUT-AH-PREM-CHG         PIC 9(7).99 VALUE ZEROS.
000557    05  F                       PIC X(10) VALUE "~LFREFCHG=".
000558    05  OUT-LF-REF-CHG          PIC 9(7).99 VALUE ZEROS.
000559    05  F                       PIC X(10) VALUE "~AHREFCHG=".
000560    05  OUT-AH-REF-CHG          PIC 9(7).99 VALUE ZEROS.
000561    05  F                       PIC X(11) VALUE "~ACCTLFAMT=".
000562    05  OUT-ACCT-LF-PORTION     PIC 9(7).99 VALUE ZEROS.
000563    05  F                       PIC X(11) VALUE "~ACTALFAMT=".
000564    05  OUT-ACCT-ALT-LF-PORT    PIC 9(7).99 VALUE ZEROS.
000565    05  F                       PIC X(11) VALUE "~ACTCLFAMT=".
000566    05  OUT-ACCT-CMB-LF-PORT    PIC 9(7).99 VALUE ZEROS.
000567    05  F                       PIC X(11) VALUE "~ACCTAHAMT=".
000568    05  OUT-ACCT-AH-PORTION     PIC 9(7).99 VALUE ZEROS.
000569    05  F                       PIC X(10) VALUE "~CSOLFAMT=".
000570    05  OUT-CSO-LF-PORTION      PIC 9(7).99 VALUE ZEROS.
000571    05  F                       PIC X(11) VALUE "~CSOALFAMT=".
000572    05  OUT-CSO-ALT-LF-PORT     PIC 9(7).99 VALUE ZEROS.
000573    05  F                       PIC X(11) VALUE "~CSOCLFAMT=".
000574    05  OUT-CSO-CMB-LF-PORT     PIC 9(7).99 VALUE ZEROS.
000575    05  F                       PIC X(10) VALUE "~CSOAHAMT=".
000576    05  OUT-CSO-AH-PORTION      PIC 9(7).99 VALUE ZEROS.
000577    05  F                       PIC X(11) VALUE "~ACTOLFAMT=".
000578    05  OUT-ACCT-ORIG-LF-PORT   PIC 9(7).99 VALUE ZEROS.
000579    05  F                       PIC X(12) VALUE "~ACTOALFAMT=".
000580    05  OUT-ACCT-ORIG-ALT-LF-PORT PIC 9(7).99 VALUE ZEROS.
000581    05  F                       PIC X(12) VALUE "~ACTOCLFAMT=".
000582    05  OUT-ACCT-ORIG-CMB-LF-PORT PIC 9(7).99 VALUE ZEROS.
000583    05  F                       PIC X(11) VALUE "~ACTOAHAMT=".
000584    05  OUT-ACCT-ORIG-AH-PORT   PIC 9(7).99 VALUE ZEROS.
000585    05  F                       PIC X(10) VALUE "~ACCTOAMT=".
000586    05  OUT-ACCT-ORIG-PORTION   PIC 9(7).99 VALUE ZEROS.
000587    05  F                       PIC X(11) VALUE "~CSOOLFAMT=".
000588    05  OUT-CSO-ORIG-LF-PORT    PIC 9(7).99 VALUE ZEROS.
000589    05  F                       PIC X(12) VALUE "~CSOOALFAMT=".
000590    05  OUT-CSO-ORIG-ALT-LF-PORT PIC 9(7).99 VALUE ZEROS.
000591    05  F                       PIC X(12) VALUE "~CSOOCLFAMT=".
000592    05  OUT-CSO-ORIG-CMB-LF-PORT PIC 9(7).99 VALUE ZEROS.
000593    05  F                       PIC X(11) VALUE "~CSOOAHAMT=".
000594    05  OUT-CSO-ORIG-AH-PORT    PIC 9(7).99 VALUE ZEROS.
000595    05  F                       PIC X(09) VALUE "~CSOOAMT=".
000596    05  OUT-CSO-ORIG-PORTION    PIC 9(7).99 VALUE ZEROS.
000597    05  F                       PIC X(11) VALUE "~ACTNLFAMT=".
000598    05  OUT-ACCT-NEW-LF-PORT    PIC 9(7).99 VALUE ZEROS.
000599    05  F                       PIC X(12) VALUE "~ACTNALFAMT=".
000600    05  OUT-ACCT-NEW-ALT-LF-PORT PIC 9(7).99 VALUE ZEROS.
000601    05  F                       PIC X(12) VALUE "~ACTNCLFAMT=".
000602    05  OUT-ACCT-NEW-CMB-LF-PORT PIC 9(7).99 VALUE ZEROS.
000603    05  F                       PIC X(11) VALUE "~ACTNAHAMT=".
000604    05  OUT-ACCT-NEW-AH-PORT    PIC 9(7).99 VALUE ZEROS.
000605    05  F                       PIC X(10) VALUE "~ACCTNAMT=".
000606    05  OUT-ACCT-NEW-PORTION    PIC 9(7).99 VALUE ZEROS.
000607    05  F                       PIC X(11) VALUE "~CSONLFAMT=".
000608    05  OUT-CSO-NEW-LF-PORT     PIC 9(7).99 VALUE ZEROS.
000609    05  F                       PIC X(12) VALUE "~CSONALFAMT=".
000610    05  OUT-CSO-NEW-ALT-LF-PORT PIC 9(7).99 VALUE ZEROS.
000611    05  F                       PIC X(12) VALUE "~CSONCLFAMT=".
000612    05  OUT-CSO-NEW-CMB-LF-PORT PIC 9(7).99 VALUE ZEROS.
000613    05  F                       PIC X(11) VALUE "~CSONAHAMT=".
000614    05  OUT-CSO-NEW-AH-PORT     PIC 9(7).99 VALUE ZEROS.
000615    05  F                       PIC X(09) VALUE "~CSONAMT=".
000616    05  OUT-CSO-NEW-PORTION     PIC 9(7).99 VALUE ZEROS.
000617    05  F                       PIC X(10) VALUE "~ACTLFCHG=".
000618    05  OUT-ACCT-LF-PORT-CHG    PIC 9(7).99 VALUE ZEROS.
000619    05  F                       PIC X(11) VALUE "~ACTALFCHG=".
000620    05  OUT-ACCT-ALT-LF-PORT-CHG PIC 9(7).99 VALUE ZEROS.
000621    05  F                       PIC X(11) VALUE "~ACTCLFCHG=".
000622    05  OUT-ACCT-CMB-LF-PORT-CHG PIC 9(7).99 VALUE ZEROS.
000623    05  F                       PIC X(10) VALUE "~ACTAHCHG=".
000624    05  OUT-ACCT-AH-PORT-CHG    PIC 9(7).99 VALUE ZEROS.
000625    05  F                       PIC X(09) VALUE "~ACCTCHG=".
000626    05  OUT-ACCT-PORTION-CHG    PIC 9(7).99 VALUE ZEROS.
000627    05  F                       PIC X(10) VALUE "~CSOLFCHG=".
000628    05  OUT-CSO-LF-PORT-CHG     PIC 9(7).99 VALUE ZEROS.
000629    05  F                       PIC X(11) VALUE "~CSOALFCHG=".
000630    05  OUT-CSO-ALT-LF-PORT-CHG PIC 9(7).99 VALUE ZEROS.
000631    05  F                       PIC X(11) VALUE "~CSOCLFCHG=".
000632    05  OUT-CSO-CMB-LF-PORT-CHG PIC 9(7).99 VALUE ZEROS.
000633    05  F                       PIC X(10) VALUE "~CSOAHCHG=".
000634    05  OUT-CSO-AH-PORT-CHG     PIC 9(7).99 VALUE ZEROS.
000635    05  F                       PIC X(08) VALUE "~CSOCHG=".
000636    05  OUT-CSO-PORTION-CHG     PIC 9(7).99 VALUE ZEROS.
000637    05  F                       PIC X(10) VALUE "~LFOCANDT=".
000638    05  OUT-ORIG-LF-CAN-DT      PIC X(21) VALUE SPACES.
000639    05  F                       PIC X(10) VALUE "~LFNCANDT=".
000640    05  OUT-NEW-LF-CAN-DT       PIC X(21) VALUE SPACES.
000641    05  F                       PIC X(10) VALUE "~AHOCANDT=".
000642    05  OUT-ORIG-AH-CAN-DT      PIC X(21) VALUE SPACES.
000643    05  F                       PIC X(10) VALUE "~AHNCANDT=".
000644    05  OUT-NEW-AH-CAN-DT       PIC X(21) VALUE SPACES.
000645    05  F                       PIC X(11) VALUE "~ORIGCANDT=".
000646    05  OUT-ORIG-CAN-DT         PIC X(21) VALUE SPACES.
000647    05  F                       PIC X(10) VALUE "~NEWCANDT=".
000648    05  OUT-NEW-CAN-DT          PIC X(21) VALUE SPACES.
000649    05  F                       PIC X(8)  VALUE "~SCREEN=".
000650    05  OUT-SCREEN-ID           PIC X(8)  VALUE SPACES.
000651    05  F                       PIC X(9)  VALUE "~NXTBSDT=".
000652    05  OUT-NEXT-BUS-DT         PIC X(21) VALUE SPACES.
000653    05  F                       PIC X(7)  VALUE "~VINNO=".
000654    05  OUT-VIN-NUMBER          PIC X(17) VALUE SPACES.
000655    05  F                       PIC X(11) VALUE 'EndOfString'.
000656    05  f                       pic x(2) value spaces.
      *<<((file: NSCASVARS))
000300
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
000302
       01  DFHCOMMAREA       PIC X(01).
000303 01  var  pic x(30).
000304
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'NSRASLTR' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000305 VCOBOL-DUMMY-PROCEDURE.
000306
000307*********************
000308* Receive web input
000309*********************
000310
000311*    DISPLAY ' ENTERING NSRASLTR '
000312
000313     set P to address of KIXSYS
000314     CALL "getenv" using by value P returning var-ptr
000315     if var-ptr = null then
000316        display ' kixsys not set '
000317     else
000318        set address of var to var-ptr
000319        move 0 to env-var-len
000320        inspect var tallying env-var-len
000321          for characters before X'00'
000322        DISPLAY '  KIXSYS = ' var (1:env-var-len)
000323        unstring var (1:env-var-len) delimited by '/'
000324           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000325              WS-KIX-SYS
000326        end-unstring
000327*       DISPLAY ' WS KIX SYS ' WS-KIXSYS
000328*       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
000329     end-if
000330
000331     set P to address of KIXHOST
000332     CALL "getenv" using by value P returning var-ptr
000333     if var-ptr = null then
000334        display ' kixhost not set '
000335     else
000336        set address of var to var-ptr
000337        move 0 to env-var-len
000338        inspect var tallying env-var-len
000339          for characters before X'00'
000340        MOVE var(1:env-var-len)  to ws-kixhost
000341        DISPLAY ' WS KIX HOST ' WS-KIXHOST
000342     end-if
000343
000344     MOVE EIBDATE                TO DC-JULIAN-YYDDD
000345     MOVE '5'                    TO DC-OPTION-CODE
000346     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
000347     MOVE DC-GREG-DATE-A-EDIT    TO ASERR-DT
000348
000349     
      * exec cics web
000350*       startbr formfield resp(w-resp)
000351*     end-exec.
      *    MOVE 'X(f                   &  N#00001372' TO DFHEIV0
           MOVE X'582866202020202020202020' &
                X'202020202020202020202620' &
                X'204E233030303031333732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000352
000353      perform read-form thru read-form-exit
000354         until w-resp not = 0 .
      *   dfhresp(normal)
000355
000356      
      * exec cics web
000357*       endbr formfield
000358*     end-exec
      *    MOVE 'X,f                   #   #00001379' TO DFHEIV0
           MOVE X'582C66202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303031333739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000359
000360*    display ' form input ' INPUT-FROM-FORM
000361     MOVE SPACES                  TO BL-INPUT
000362     MOVE IFF-DATA-SRCE           TO BL-DATA-SRCE
000363     MOVE IFF-CARRIER             TO BL-CARRIER
000364     MOVE IFF-GROUP               TO BL-GROUP
000365     MOVE IFF-STATE               TO BL-STATE
000366     MOVE IFF-ACCOUNT             TO BL-ACCOUNT
000367     MOVE IFF-EFF-DT              TO BL-EFF-DT
000368     MOVE IFF-CERT-NO             TO BL-CERT-NO
000369     MOVE IFF-BATCH-NO            TO BL-BATCH-NO
000370                                     ot-batch-no
000371     MOVE IFF-BATCH-SEQ           TO BL-BATCH-SEQ
000372                                     ot-batch-seq-no
000373     MOVE IFF-RESP-NO             TO BL-RESP-NO
000374     MOVE IFF-LETTER-ID           TO BL-LETTER-ID
000375     MOVE IFF-NO-OF-COPIES        TO BL-NO-OF-COPIES
000376     MOVE IFF-PROC-ID             TO BL-PROC-ID
000377     MOVE IFF-COMP-ID             TO OT-COMP-ID
000378                                     BL-COMP-ID
000379     MOVE IFF-PRINT-NOW-SW        TO OT-PRINT-NOW-SW
000380                                     BL-PRINT-NOW-SW
000381     MOVE IFF-ENC-CD              TO BL-ENC-CD
000382     MOVE IFF-RESEND-DT           TO BL-RESEND-DT
000383     MOVE IFF-FOLLOW-UP-DT        TO BL-FOLLOW-UP-DT
000384*    MOVE IFF-COMMENTS            TO BL-COMMENTS
000385     MOVE IFF-ARCHIVE-NO          TO BL-ARCHIVE-NO
000386     MOVE IFF-FUNC                TO BL-FUNC
000387                                     OT-FUNC
000388     MOVE IFF-ENDT-ARCH-NO        TO BL-ENDT-ARCH-NO
000389
000390     EVALUATE BL-FUNC
000391         WHEN 'Letter'
000392             MOVE 'LETTER'        TO BL-SOURCE-SCREEN
000393         WHEN 'CrtVerif'
000394             MOVE 'VERIFY'        TO BL-SOURCE-SCREEN
000395         WHEN 'Endorse'
000396             MOVE 'ISS ENDT'      TO BL-SOURCE-SCREEN
000397         WHEN 'CancChg'
000398             MOVE 'CAN ENDT'      TO BL-SOURCE-SCREEN
000399         WHEN OTHER
000400             MOVE 'UNKNOWN'       TO BL-SOURCE-SCREEN
000401     END-EVALUATE
000402
000403*****************************************
000404* Since this program was called from the webservice
000405* we really don't know if a real letter is being created
000406* a "T" below will create temporary erarch and nsasextr
000407* records and "X" we won't create anything
000408*****************************************
000409     MOVE 'T'                    TO BL-WRITE-ERARCH
000410     IF BL-PRINT-NOW-SW = 'P'
000411        MOVE 'X'                 TO BL-WRITE-ERARCH
000412     END-IF
000413
000414     MOVE 'X'                 TO BL-WRITE-ERARCH
000415
000416*    DISPLAY ' INPUT FORM ' WS-KEY
000417*****************************************
000418* Invoke the LETTER business logic
000419*****************************************
000420
000421*    DISPLAY ' BL INPUT ' BL-INPUT
000422     MOVE FUNCTION LENGTH(SRCH-COMMAREA) TO WS-COMM-LENGTH
000423
000424     
      * exec cics link
000425*       program('NSRASBL')
000426*       commarea(srch-commarea)
000427*       LENGTH  (WS-COMM-LENGTH)
000428*    end-exec.
           MOVE 'NSRASBL' TO DFHEIV1
      *    MOVE '."C                   (   #00001447' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303031343437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 srch-commarea, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000429
000430     IF BL-OK
000431        perform varying bl-length from +6200 by -1 until
000432           (bl-record-passed-data (bl-length:1) not = ' ')
000433           or (bl-length = +1)
000434        end-perform
000435*       DISPLAY ' BL OK ' BL-ARCHIVE-NO
000436        move bl-length to sl-length
000437        move spaces to naper-output-data
000438        MOVE BL-RECORD-PASSED-DATA (1:bl-length)
000439                                 TO NAPER-OUTPUT-DATA
000440        MOVE BL-ARCHIVE-NO          TO OT-ARCHIVE-NO
000441     ELSE
000442        MOVE BL-MESSAGE          TO ASERR-MESS
000443        
      * exec cics document create
000444*          doctoken   (w-doctoken)
000445*          template   ('ASERR')
000446*          symbollist (WS-ASERR)
000447*          resp       (WS-RESPONSE)
000448*       end-exec
           MOVE LENGTH OF
            WS-ASERR
             TO DFHEIV16
           MOVE 'ASERR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001466' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031343636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-ASERR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000449        
      * exec cics web send
000450*          doctoken(w-doctoken)
000451*                      resp   (WS-RESPONSE)
000452*       end-exec
      *    MOVE 'X$D                   *  N#00001472' TO DFHEIV0
           MOVE X'582444202020202020202020' &
                X'202020202020202020202A20' &
                X'204E233030303031343732' TO DFHEIV0
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000453        
      * exec cics
000454*          return
000455*       end-exec
      *    MOVE '.(                    ''   #00001476' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303031343736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000456     END-IF
000457
000458     evaluate ws-kixhost
000459        when 'slunikix'
000460           evaluate ws-kix-myenv
000461              when 'cid1p'
000462                 move '7001'     to ws-sl-port
000463*                                   ws-sl-port2
000464              when 'mdoff'
000465                 move '7003'     to ws-sl-port
000466*                                   ws-sl-port2
000467              when 'cid1t'
000468                 move '7002'     to ws-sl-port
000469*                                   ws-sl-port2
000470           end-evaluate
000471           move ws-asftr         to ws-sl-key
000472*                                   ws-sl-key2
000473        when 'logictest'
000474           evaluate ws-kix-myenv
000475              when 'cid1t'
000476                 move '6002'     to ws-lt-port
000477*                                   ws-lt-port2
000478              when 'tony'
000479                 move '6003'     to ws-lt-port
000480*                                   ws-lt-port2
000481              when 'paul'
000482                 move '5002'     to ws-lt-port
000483*                                   ws-lt-port2
000484              when 'ahltst'
000485                 move '6007'     to ws-lt-port
000486*                                   ws-lt-port2
000487           end-evaluate
000488           move ws-asftr         to ws-lt-key
000489*                                   ws-lt-key2
000490     end-evaluate
000491
000492***********************************************************
000493* Build output document.  There are three templates used
000494* for this document.  ASHDR and ASFTR are the header
000495* and footer, respectively.  ASBOD is used for the
000496* actual data.  For each array entry in the business
000497* logic output, set the symbol list from the array
000498* entry and insert into the document using the ASBOD
000499* template.
000500***********************************************************
000501
000502*    move bl-output-message to out-msg-text.
000503
000504     evaluate true
000505        when iff-comp-id = 'DCC'
000506           IF WS-KIX-MYENV = 'cid1p'
000507              
      * exec cics document create
000508*                doctoken   (w-doctoken)
000509*                template   ('ASHDR')
000510*                symbollist (WS-PROD-DCC-ASHDR)
000511*                resp       (WS-RESPONSE)
000512*             end-exec
           MOVE LENGTH OF
            WS-PROD-DCC-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001530' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031353330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-PROD-DCC-ASHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000513           ELSE
000514              
      * exec cics document create
000515*                doctoken   (w-doctoken)
000516*                template   ('ASHDR')
000517*                symbollist (WS-TEST-DCC-ASHDR)
000518*                resp       (WS-RESPONSE)
000519*             end-exec
           MOVE LENGTH OF
            WS-TEST-DCC-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001537' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031353337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-TEST-DCC-ASHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000520           END-IF
000521        when iff-comp-id = 'AHL'
000522           IF WS-KIX-MYENV = 'cid1p'
000523              
      * exec cics document create
000524*                doctoken   (w-doctoken)
000525*                template   ('ASHDR')
000526*                symbollist (WS-PROD-AHL-ASHDR)
000527*                resp       (WS-RESPONSE)
000528*             end-exec
           MOVE LENGTH OF
            WS-PROD-AHL-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001546' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031353436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-PROD-AHL-ASHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000529           ELSE
000530              
      * exec cics document create
000531*                doctoken   (w-doctoken)
000532*                template   ('ASHDR')
000533*                symbollist (WS-TEST-AHL-ASHDR)
000534*                resp       (WS-RESPONSE)
000535*             end-exec
           MOVE LENGTH OF
            WS-TEST-AHL-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001553' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031353533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-TEST-AHL-ASHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000536           END-IF
000537        when iff-comp-id = 'VPP'
000538           IF WS-KIX-MYENV = 'cid1p'
000539              
      * exec cics document create
000540*                doctoken   (w-doctoken)
000541*                template   ('ASHDR')
000542*                symbollist (WS-PROD-VPP-ASHDR)
000543*                resp       (WS-RESPONSE)
000544*             end-exec
           MOVE LENGTH OF
            WS-PROD-VPP-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001562' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031353632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-PROD-VPP-ASHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000545           ELSE
000546              
      * exec cics document create
000547*                doctoken   (w-doctoken)
000548*                template   ('ASHDR')
000549*                symbollist (WS-TEST-VPP-ASHDR)
000550*                resp       (WS-RESPONSE)
000551*             end-exec
           MOVE LENGTH OF
            WS-TEST-VPP-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001569' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031353639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-TEST-VPP-ASHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000552           END-IF
000553        when iff-comp-id = 'FNL'
000554           IF WS-KIX-MYENV = 'cid1p'
000555              
      * exec cics document create
000556*                doctoken   (w-doctoken)
000557*                template   ('ASHDR')
000558*                symbollist (WS-PROD-FNL-ASHDR)
000559*                resp       (WS-RESPONSE)
000560*             end-exec
           MOVE LENGTH OF
            WS-PROD-FNL-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001578' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031353738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-PROD-FNL-ASHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000561           ELSE
000562              
      * exec cics document create
000563*                doctoken   (w-doctoken)
000564*                template   ('ASHDR')
000565*                symbollist (WS-TEST-FNL-ASHDR)
000566*                resp       (WS-RESPONSE)
000567*             end-exec
           MOVE LENGTH OF
            WS-TEST-FNL-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001585' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031353835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-TEST-FNL-ASHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000568           END-IF
000569        when OTHER
000570           IF WS-KIX-MYENV = 'cid1p'
000571             
      * exec cics document create
000572*               doctoken   (w-doctoken)
000573*               template   ('ASHDR')
000574*               symbollist (WS-PROD-CID-ASHDR)
000575*               resp       (WS-RESPONSE)
000576*            end-exec
           MOVE LENGTH OF
            WS-PROD-CID-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001594' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031353934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-PROD-CID-ASHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000577           ELSE
000578             
      * exec cics document create
000579*               doctoken   (w-doctoken)
000580*               template   ('ASHDR')
000581*               symbollist (WS-TEST-CID-ASHDR)
000582*               resp       (WS-RESPONSE)
000583*            end-exec
           MOVE LENGTH OF
            WS-TEST-CID-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001601' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'204E233030303031363031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 WS-TEST-CID-ASHDR, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000584           END-IF
000585     end-evaluate
000586
000587
000588*    DISPLAY ' ASHDR DOC CREATE ' WS-RESPONSE
000589
000590*    move 1 to bl-index.
000591*
000592*    perform bl-output-record-count times
000593*
000594*        move bl-output-account-number(bl-index)
000595*          to out-account-number
000596*        move bl-output-last-name(bl-index)
000597*          to out-last-name
000598*        move bl-output-first-name(bl-index)
000599*          to out-first-name
000600*        move bl-output-middle-initial(bl-index)
000601*          to out-middle-initial
000602*
000603
000604*    MOVE 'EL01'                 TO OUT-TEMP
000605*    MOVE 'ALWA'                 TO OUT-PROC-ID
000606*    MOVE 'EL01'                 TO OUT-LETTER
000607*    MOVE '9'                    TO OUT-CARRIER
000608*    MOVE 'MCDANIEL'             TO OUT-ILNAME
000609*    MOVE 'KATHI'                TO OUT-IFNAME
000610*
000611*    MOVE '0009235906 '          TO OUT-CERT-NO
000612*    MOVE '0990000208'           TO OUT-ACCOUNT
000613
000614     
      * exec cics document set
000615*       doctoken(w-doctoken)
000616*       symbollist(NAPER-OUTPUT-DATA (1:bl-length))
000617*       delimiter ('?')
000618*       length(length of NAPER-OUTPUT-DATA)
000619*       length(sl-length)
000620*                   resp   (WS-RESPONSE)
000621*     unescaped
000622*    end-exec
      *    MOVE '\(Ds L                ''  N#00001637' TO DFHEIV0
           MOVE X'5C284473204C202020202020' &
                X'202020202020202020202720' &
                X'204E233030303031363337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 NAPER-OUTPUT-DATA(1 : bl-length), 
                 DFHEIV99, 
                 sl-length, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000623
000624*    DISPLAY ' DOC SET    ' WS-RESPONSE
000625
000626     
      * exec cics document insert
000627*       doctoken(w-doctoken)
000628*       template('ASBOD')
000629*                   resp   (WS-RESPONSE)
000630*    end-exec
           MOVE 'ASBOD' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001649' TO DFHEIV0
           MOVE X'5C2444742020202020202020' &
                X'202020202020202020202820' &
                X'204E233030303031363439' TO DFHEIV0
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
000631
000632     IF BL-LETTER-TO-ACCT NOT = SPACES
000633        MOVE BL-LETTER-TO-ACCT   TO NAPER-OUTPUT-DATA (12:1)
000634*       MOVE 'EL01T'             TO NAPER-OUTPUT-DATA (8:6)
000635        
      * exec cics document set
000636*          doctoken(w-doctoken)
000637*          symbollist(NAPER-OUTPUT-DATA (1:bl-length))
000638*          length(sl-length)
000639*          resp   (WS-RESPONSE)
000640*       end-exec
      *    MOVE '\(Ds L                ''  N#00001658' TO DFHEIV0
           MOVE X'5C284473204C202020202020' &
                X'202020202020202020202720' &
                X'204E233030303031363538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 NAPER-OUTPUT-DATA(1 : bl-length), 
                 DFHEIV99, 
                 sl-length, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000641
000642        
      * exec cics document insert
000643*          doctoken(w-doctoken)
000644*          template('ASBOD')
000645*          resp   (WS-RESPONSE)
000646*       end-exec
           MOVE 'ASBOD' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001665' TO DFHEIV0
           MOVE X'5C2444742020202020202020' &
                X'202020202020202020202820' &
                X'204E233030303031363635' TO DFHEIV0
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
000647     END-IF
000648
000649     IF BL-LETTER-TO-BENE NOT = SPACES
000650        MOVE BL-LETTER-TO-BENE   TO NAPER-OUTPUT-DATA (12:1)
000651*       MOVE 'EL01T'             TO NAPER-OUTPUT-DATA (8:6)
000652        
      * exec cics document set
000653*          doctoken(w-doctoken)
000654*          symbollist(NAPER-OUTPUT-DATA (1:bl-length))
000655*          length(sl-length)
000656*          resp   (WS-RESPONSE)
000657*       end-exec
      *    MOVE '\(Ds L                ''  N#00001675' TO DFHEIV0
           MOVE X'5C284473204C202020202020' &
                X'202020202020202020202720' &
                X'204E233030303031363735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 NAPER-OUTPUT-DATA(1 : bl-length), 
                 DFHEIV99, 
                 sl-length, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000658
000659        
      * exec cics document insert
000660*          doctoken(w-doctoken)
000661*          template('ASBOD')
000662*          resp   (WS-RESPONSE)
000663*       end-exec
           MOVE 'ASBOD' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001682' TO DFHEIV0
           MOVE X'5C2444742020202020202020' &
                X'202020202020202020202820' &
                X'204E233030303031363832' TO DFHEIV0
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
000664     END-IF
000665
000666*    DISPLAY ' ASBOD DOC INSERT ' WS-RESPONSE
000667
000668*
000669*        add 1 to bl-index
000670*
000671*    end-perform.
000672
000673*    MOVE BL-CARRIER  TO OT-CARRIER
000674*    MOVE BL-CLAIM-NO TO OT-CLMNO
000675*    MOVE BL-CERT-NO  TO OT-CRTNO
000676*    MOVE BL-ARCHIVE-NO TO OT-ARCHNO
000677
000678
000679     IF WS-KIXHOST = 'slunikix'
000680        display ' host slunikix ' ws-kixhost
000681        
      * exec cics document set
000682*          doctoken(w-doctoken)
000683*          symbollist(WS-VAR-SLUNIKIX)
000684*          length(length of WS-VAR-SLUNIKIX)
000685*                      resp   (WS-RESPONSE)
000686*       end-exec
           MOVE LENGTH OF
            WS-VAR-SLUNIKIX TO DFHEIV16
      *    MOVE '\(Ds L                ''  N#00001704' TO DFHEIV0
           MOVE X'5C284473204C202020202020' &
                X'202020202020202020202720' &
                X'204E233030303031373034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 WS-VAR-SLUNIKIX, 
                 DFHEIV99, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000687     else
000688        display ' host logictest ' ws-kixhost
000689        
      * exec cics document set
000690*          doctoken(w-doctoken)
000691*          symbollist(WS-VAR-LOGICTEST)
000692*          length(length of WS-VAR-LOGICTEST)
000693*                      resp   (WS-RESPONSE)
000694*       end-exec
           MOVE LENGTH OF
            WS-VAR-LOGICTEST TO DFHEIV16
      *    MOVE '\(Ds L                ''  N#00001712' TO DFHEIV0
           MOVE X'5C284473204C202020202020' &
                X'202020202020202020202720' &
                X'204E233030303031373132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 WS-VAR-LOGICTEST, 
                 DFHEIV99, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000695     end-if
000696*    DISPLAY ' DOC SET ' WS-RESPONSE
000697
000698     
      * exec cics document insert
000699*       doctoken(w-doctoken)
000700*       template('ASFTR')
000701*                   resp   (WS-RESPONSE)
000702*    end-exec
           MOVE 'ASFTR' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001721' TO DFHEIV0
           MOVE X'5C2444742020202020202020' &
                X'202020202020202020202820' &
                X'204E233030303031373231' TO DFHEIV0
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
000703
000704*    DISPLAY ' ASFTR DOC INSERT ' WS-RESPONSE
000705*    if bl-fail
000706*       exec cics syncpoint rollback
000707*       end-exec
000708*    end-if.
000709
000710****************************************
000711* Send the document and return.
000712****************************************
000713
000714     
      * exec cics web send
000715*       doctoken(w-doctoken)
000716*                   resp   (WS-RESPONSE)
000717*    end-exec.
      *    MOVE 'X$D                   *  N#00001737' TO DFHEIV0
           MOVE X'582444202020202020202020' &
                X'202020202020202020202A20' &
                X'204E233030303031373337' TO DFHEIV0
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
           
000718
000719*    DISPLAY ' WEB SEND  ' WS-RESPONSE
000720
000721*    DISPLAY ' ABOUT TO RETURN '
000722     
      * exec cics
000723*       return
000724*    end-exec.
      *    MOVE '.(                    ''   #00001745' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303031373435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000725
000726
000727******************************************************
000728* Read all fields of the incoming form, moving
000729* each to the corresponding field of the commarea
000730* (business logic input fields).
000731******************************************************
000732
000733 read-form.
000734     move spaces to w-form-name.
000735     move length of w-form-name to w-form-name-len.
000736           move spaces to w-form-value.
000737     move length of w-form-value to w-form-value-len.
000738     
      * exec cics web readnext
000739*                  formfield(w-form-name)
000740*                  namelength(w-form-name-len)
000741*                  value(w-form-value)
000742*                  valuelength(w-form-value-len)
000743*                  resp(w-resp)
000744*    end-exec.
      *    MOVE 'X*FLVL                &  N#00001761' TO DFHEIV0
           MOVE X'582A464C564C202020202020' &
                X'202020202020202020202620' &
                X'204E233030303031373631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-form-name, 
                 w-form-name-len, 
                 w-form-value, 
                 w-form-value-len, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000745     evaluate w-resp
000746        when 0 
      *   dfhresp(normal)
000747           evaluate w-form-name(1:w-form-name-len)
000748              when 'data_src'
000749                 if w-form-value-len not = 0
000750                    move w-form-value(1:w-form-value-len)
000751                                 to IFF-DATA-SRCE
000752                 else
000753                                move spaces to IFF-DATA-SRCE
000754                 end-if
000755              when 'letter_id'
000756                 if w-form-value-len not = 0
000757                    move w-form-value(1:w-form-value-len)
000758                                 to IFF-LETTER-ID
000759                 else
000760                                move spaces to IFF-LETTER-ID
000761                 end-if
000762              when 'carrier'
000763                 if w-form-value-len not = 0
000764                    move w-form-value(1:w-form-value-len)
000765                             to IFF-CARRIER
000766                 else
000767                                move spaces to IFF-CARRIER
000768                 end-if
000769              when 'grouping'
000770                             if w-form-value-len not = 0
000771                    move w-form-value(1:w-form-value-len)
000772                                                   to IFF-GROUP
000773                 else
000774                                move spaces  to IFF-GROUP
000775                 end-if
000776              when 'state_cd'
000777                             if w-form-value-len not = 0
000778                    move w-form-value(1:w-form-value-len)
000779                                 to IFF-STATE
000780                 else
000781                                move spaces  to IFF-STATE
000782                 end-if
000783              when 'acct_no'
000784                             if w-form-value-len not = 0
000785                    move w-form-value(1:w-form-value-len)
000786                                 to IFF-ACCOUNT
000787                 else
000788                    move spaces  to IFF-ACCOUNT
000789                 end-if
000790              when 'eff_date'
000791                             if w-form-value-len not = 0
000792                    move w-form-value(1:w-form-value-len)
000793                                 to IFF-EFF-DT
000794                 else
000795                                move spaces  to IFF-EFF-DT
000796                 end-if
000797              when 'cert_no'
000798                             if w-form-value-len not = 0
000799                    move w-form-value(1:w-form-value-len)
000800                                 to IFF-CERT-NO
000801                 else
000802                                move spaces  to IFF-CERT-NO
000803                 end-if
000804              when 'batch_no'
000805                             if w-form-value-len not = 0
000806                    move w-form-value(1:w-form-value-len)
000807                                 to IFF-BATCH-NO
000808                 else
000809                  move zeros   to IFF-BATCH-NO
000810                 end-if
000811              when 'batch_seq'
000812                             if w-form-value-len not = 0
000813                    move w-form-value(1:w-form-value-len)
000814                                 to IFF-BATCH-SEQ
000815                 else
000816                                move zeros   to IFF-BATCH-SEQ
000817                 end-if
000818              when 'resp_no'
000819                             if w-form-value-len not = 0
000820                    move w-form-value(1:w-form-value-len)
000821                                 to IFF-RESP-NO
000822                 else
000823                                move spaces  to IFF-RESP-NO
000824                 end-if
000825              when 'no_copies'
000826                             if w-form-value-len not = 0
000827                    move w-form-value(1:w-form-value-len)
000828                                 to IFF-NO-OF-COPIES
000829                 else
000830                    move zeros   to IFF-NO-OF-COPIES
000831                 end-if
000832              when 'proc_id'
000833                             if w-form-value-len not = 0
000834                    move w-form-value(1:w-form-value-len)
000835                                 to IFF-PROC-ID
000836                 else
000837                    move 'JJVA'  to IFF-PROC-ID
000838                 end-if
000839              when 'comp_id'
000840                             if w-form-value-len not = 0
000841                    move w-form-value(1:w-form-value-len)
000842                                 to IFF-COMP-ID
000843                 else
000844                    move 'CID'   to IFF-COMP-ID
000845                 end-if
000846              when 'prt_now'
000847                             if w-form-value-len not = 0
000848                    move w-form-value(1:w-form-value-len)
000849                                 to IFF-PRINT-NOW-SW
000850                 else
000851                    move 'N'     to IFF-PRINT-NOW-SW
000852                 end-if
000853              when 'enc_cd'
000854                             if w-form-value-len not = 0
000855                    move w-form-value(1:w-form-value-len)
000856                                 to IFF-ENC-CD
000857                 else
000858                    move '0'     to IFF-ENC-CD
000859                 end-if
000860              when 'resend_dt'
000861                             if w-form-value-len not = 0
000862                    move w-form-value(1:w-form-value-len)
000863                                 to IFF-RESEND-DT
000864                 else
000865                    move SPACES  to IFF-RESEND-DT
000866                 end-if
000867              when 'follow_dt'
000868                             if w-form-value-len not = 0
000869                    move w-form-value(1:w-form-value-len)
000870                                 to IFF-FOLLOW-UP-DT
000871                 else
000872                    move SPACES  to IFF-FOLLOW-UP-DT
000873                 end-if
000874              when 'arch_no'
000875                             if w-form-value-len not = 0
000876                    move w-form-value(1:w-form-value-len)
000877                                 to IFF-ARCHIVE-NO
000878                 else
000879                    move ZEROS   to IFF-ARCHIVE-NO
000880                 end-if
000881              when 'pgm_func'
000882                             if w-form-value-len not = 0
000883                    move w-form-value(1:w-form-value-len)
000884                                 to IFF-FUNC
000885                 else
000886                    move ZEROS   to IFF-FUNC
000887                 end-if
000888*             when 'ltr_comments'
000889*                if w-form-value-len not = 0
000890*                   move w-form-value(1:w-form-value-len)
000891*                                to IFF-COMMENTS
000892*                else
000893*                   move SPACES  to IFF-COMMENTS
000894*                end-if
000895              when 'end_arch'
000896                 if w-form-value-len not = 0
000897                    move w-form-value(1:w-form-value-len)
000898                                 to IFF-ENDT-ARCH-NO
000899                 else
000900                    move ZEROS  to IFF-ENDT-ARCH-NO
000901                 end-if
000902           end-evaluate
000903        when other
000904           continue
000905     end-evaluate.
000906 read-form-exit.
000907
000908 9700-DATE-LINK.
000909
000910     
      * EXEC CICS LINK
000911*        PROGRAM   ('ELDATCV')
000912*        COMMAREA  (DATE-CONVERSION-DATA)
000913*        LENGTH    (DC-COMM-LENGTH)
000914*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00001933' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303031393333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000915
000916
000917 9700-EXIT.
000918      EXIT.
000919

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSRASLTR' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSRASLTR' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
