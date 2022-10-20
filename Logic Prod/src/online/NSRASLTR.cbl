       IDENTIFICATION DIVISION.
       PROGRAM-ID.   NSRASLTR.
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
      * 060611    2011022800001  PEMA  NEW PROGRAM
080612* 080612    2011022800001  AJRA  ADD AHL
101812* 101812  CR2012101700002  AJRA  ADD ENDARCH AND SCREENID
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
061421* 061421  CR2017031500001  PEMA  Update to CCM8
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       working-storage section.
       01  DFH-START PIC X(04).
       77  ws-comm-length      pic 9(4) BINARY.
       01  P pointer.
       01  KIXHOST             pic x(9) value Z"HOSTNAME".
       01  WS-KIXHOST                  PIC X(10).
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
       01  bl-length                   pic s9(5) value +0 comp-3.
       01  sl-length                   pic 9(8) binary.
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
      *                                copy ELCADLTRSPI.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 060611    2011022800001  PEMA  NEW COPYBOOK
101812* 101812    2012101700002  AJRA  ADD ENDT ARCHIVE NO, SCREENID
110612* 110612    2012101700002  AJRA  EXPAND PASSED DATA
      ******************************************************************
      ****************************************
      *  commarea for NaperSoft On Demand Admin services letters
      *  (business logic input & output)
      ****************************************
           03  BL-INPUT.
               05  BL-DATA-SRCE        PIC X.
               05  BL-LETTER-ID        PIC XXXX.
               05  BL-CARRIER          PIC X.
               05  BL-GROUP            PIC X(6).
               05  BL-STATE            PIC XX.
               05  BL-ACCOUNT          PIC X(10).
               05  BL-EFF-DT           PIC X(10).
               05  BL-CERT-NO          PIC X(11).
               05  BL-BATCH-NO         PIC X(6).
               05  BL-BATCH-SEQ        PIC 9(8).
               05  BL-RESP-NO          PIC X(10).
               05  BL-NO-OF-COPIES     PIC 99.
               05  BL-PROC-ID          PIC XXXX.
               05  BL-COMP-ID          PIC XXX.
               05  BL-PRINT-NOW-SW     PIC X.
               05  BL-ENC-CD           PIC XXX.
               05  BL-RESEND-DT        PIC X(10).
               05  BL-FOLLOW-UP-DT     PIC X(10).
               05  BL-ARCHIVE-NO       PIC 9(8).
               05  BL-FUNC             PIC X(8).
110612         05  BL-COMMENTS         PIC X(100).
               05  FILLER REDEFINES BL-COMMENTS.
                   10  BL-REASON-CODE OCCURS 12 PIC X(4).
                   10  BL-LETTER-TO-ACCT PIC X.
                   10  BL-LETTER-TO-BENE PIC X.
                   10  BL-WRITE-ERARCH   PIC X.
                       88  ERARCH-QWS      VALUE 'Q'.
                       88  ERARCH-BATCH    VALUE 'B'.
                       88  ERARCH-TEMP     VALUE 'T'.
                   10  BL-PROCESS-TYPE PIC X(07).
                   10  BL-CERT-FORM-ID PIC X(05).
101812             10  BL-ENDT-ARCH-NO PIC 9(08) BINARY.
101812             10  BL-SOURCE-SCREEN PIC X(8).
110612             10  FILLER          PIC X(25).
           03  BL-OUTPUT.
               05  BL-STATUS                   PIC X.
                   88  BL-OK                      VALUE "P".
                   88  BL-FAIL                  VALUE "F".
               05  BL-MESSAGE          PIC X(50).
110612     03  BL-RECORD-PASSED-DATA   PIC X(6200).
110612     03  FILLER                  PIC X(31).
       01  INPUT-FROM-FORM.
           05  IFF-DATA-SRCE           PIC X.
           05  IFF-LETTER-ID           PIC XXXX.
           05  IFF-CARRIER             PIC X.
           05  IFF-GROUP               PIC X(6).
           05  IFF-STATE               PIC XX.
           05  IFF-ACCOUNT             PIC X(10).
           05  IFF-EFF-DT              PIC X(10).
           05  IFF-CERT-NO             PIC X(11).
           05  IFF-BATCH-NO            PIC X(6).
           05  IFF-BATCH-SEQ           PIC 9(8).
           05  IFF-RESP-NO             PIC X(10).
           05  IFF-NO-OF-COPIES        PIC 99.
           05  IFF-PROC-ID             PIC XXXX.
           05  IFF-COMP-ID             PIC XXX.
           05  IFF-PRINT-NOW-SW        PIC X.
           05  IFF-ENC-CD              PIC XXX.
           05  IFF-RESEND-DT           PIC X(10).
           05  IFF-FOLLOW-UP-DT        PIC X(10).
           05  IFF-ARCHIVE-NO          PIC 9(08).
           05  IFF-FUNC                PIC X(08).
101812*    05  IFF-COMMENTS            PIC X(70).
101812     05  IFF-ENDT-ARCH-NO        PIC 9(08).
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
061421*  CID      CID1P      AcctServCID  sdv-nsft01       7001
061421*  CID      MDOFF      AcctServCID  hov-nsft01       7003
061421*  CID      CID1T      AcctServCID  hov-nsft01       6002
061421*  CID      PAUL       AcctServCID  hov-nsft01       5002
061421*  CID      TONY       AcctServCID  hov-nsft01       6003
061421*  DCC      CID1P      AcctServDCC  sdv-nsft01       7001
061421*  DCC      MDOFF      AcctServDCC  hov-nsft01       7003
061421*  DCC      CID1T      AcctServDCC  hov-nsft01       6002
061421*  DCC      PAUL       AcctServDCC  hov-nsft01       5002
061421*  DCC      TONY       AcctServDCC  hov-nsft01       6003
061421*  AHL      CID1P      AcctServAHL  sdv-nsft01       7001
061421*  AHL      MDOFF      AcctServAHL  hov-nsft01       7003
061421*  AHL      CID1T      AcctServAHL  hov-nsft01       6002
061421*  AHL      PAUL       AcctServAHL  hov-nsft01       5002
061421*  AHL      TONY       AcctServAHL  hov-nsft01       6003
061421*  AHL      AHLTST     AcctServAHL  hov-nsft01       6007
061421*  FNL      CID1P      AcctServFNL  sdv-nsft01       7001
061421*  FNL      MDOFF      AcctServFNL  hov-nsft01       7003
061421*  FNL      CID1T      AcctServFNL  hov-nsft01       6002
061421*  FNL      PAUL       AcctServFNL  hov-nsft01       5002
061421*  FNL      TONY       AcctServFNL  hov-nsft01       6003
061421*  FNL      AHLTST     AcctServFNL  hov-nsft01       6007
      ******************************************
      * symbol list text for ASERR template
      ******************************************
       01  WS-ASERR.
           05  F                       PIC X(6)  VALUE "PGMDT=".
           05  ASERR-DT                PIC X(12) VALUE ' '.
           05  F                       PIC X(5)  VALUE '&MSG='.
           05  ASERR-MESS              PIC X(50) VALUE ' '.
      ******************************************
      * symbol list text for ASHDR template
      ******************************************
       01  WS-PROD-CID-ASHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
           05  F                       PIC X(11) VALUE '&GROUPNAME='.
           05  F                       PIC X(11) VALUE 'AcctServCID'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
       01  WS-PROD-DCC-ASHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
           05  F                       PIC X(11) VALUE "&GROUPNAME=".
           05  F                       PIC X(11) VALUE 'AcctServDCC'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
020816 01  WS-PROD-VPP-ASHDR.
020816     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
020816     05  F                       PIC X(11) VALUE "&GROUPNAME=".
020816     05  F                       PIC X(11) VALUE 'AcctServVPP'.
020816     05  F                       PIC X(7)  VALUE '&UNAME='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
020816     05  F                       PIC X(11) VALUE '&UPASSWORD='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
080612 01  WS-PROD-AHL-ASHDR.
080612     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
080612     05  F                       PIC X(11) VALUE "&GROUPNAME=".
080612     05  F                       PIC X(11) VALUE 'AcctServAHL'.
080612     05  F                       PIC X(7)  VALUE '&UNAME='.
080612     05  F                       PIC X(8)  VALUE 'batchjob'.
080612     05  F                       PIC X(11) VALUE '&UPASSWORD='.
080612     05  F                       PIC X(8)  VALUE 'batchjob'.
061421 01  WS-PROD-FNL-ASHDR.
061421     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
061421     05  F                       PIC X(11) VALUE '&GROUPNAME='.
061421     05  F                       PIC X(11) VALUE 'AcctServFNL'.
061421     05  F                       PIC X(7)  VALUE '&UNAME='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
061421     05  F                       PIC X(11) VALUE '&UPASSWORD='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
       01  WS-TEST-CID-ASHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
           05  F                       PIC X(11) VALUE '&GROUPNAME='.
           05  F                       PIC X(11) VALUE 'AcctServCID'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
       01  WS-TEST-DCC-ASHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
           05  F                       PIC X(11) VALUE "&GROUPNAME=".
           05  F                       PIC X(11) VALUE 'AcctServDCC'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
080612
020816 01  WS-TEST-VPP-ASHDR.
020816     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
020816     05  F                       PIC X(11) VALUE "&GROUPNAME=".
020816     05  F                       PIC X(11) VALUE 'AcctServVPP'.
020816     05  F                       PIC X(7)  VALUE '&UNAME='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
020816     05  F                       PIC X(11) VALUE '&UPASSWORD='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
080612
080612 01  WS-TEST-AHL-ASHDR.
080612     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
080612     05  F                       PIC X(11) VALUE "&GROUPNAME=".
080612     05  F                       PIC X(11) VALUE 'AcctServAHL'.
080612     05  F                       PIC X(7)  VALUE '&UNAME='.
080612     05  F                       PIC X(8)  VALUE 'batchjob'.
080612     05  F                       PIC X(11) VALUE '&UPASSWORD='.
080612     05  F                       PIC X(8)  VALUE 'batchjob'.
061421 01  WS-TEST-FNL-ASHDR.
061421     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
061421     05  F                       PIC X(11) VALUE '&GROUPNAME='.
061421     05  F                       PIC X(11) VALUE 'AcctServFNL'.
061421     05  F                       PIC X(7)  VALUE '&UNAME='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
061421     05  F                       PIC X(11) VALUE '&UPASSWORD='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
      ******************************************
      * symbol list text for ASFTR template
      ******************************************
       01  WS-VAR-SLUNIKIX.
           05  FILLER                  PIC X(18) VALUE
                                       "HOSTINFO=slunikix:".
           05  WS-SL-PORT              PIC XXXX  VALUE '7001'.
           05  FILLER                  PIC X(11)  VALUE "&URLVARLST=".
061421     05  WS-SL-KEY               PIC X(32)  VALUE SPACES.
      *    05  FILLER                  PIC X(20) VALUE
      *                                "&HOSTINFO2=slunikix:".
      *    05  WS-SL-PORT2             PIC XXXX  VALUE '7001'.
      *    05  FILLER                  PIC X(12)  VALUE "&URLVARLST2=".
      *    05  WS-SL-KEY2              PIC X(12)  VALUE SPACES.
       01  WS-VAR-LOGICTEST.
           05  FILLER                  PIC X(19) VALUE
                                       "HOSTINFO=logictest:".
           05  WS-LT-PORT              PIC XXXX  VALUE '6002'.
           05  FILLER                  PIC X(11)  VALUE "&URLVARLST=".
061421     05  WS-LT-KEY               PIC X(32)  VALUE SPACES.
      *    05  FILLER                  PIC X(21) VALUE
      *                                "&HOSTINFO2=logictest:".
      *    05  WS-LT-PORT2             PIC XXXX  VALUE '6002'.
      *    05  FILLER                  PIC X(12)  VALUE "&URLVARLST2=".
      *    05  WS-LT-KEY2              PIC X(12)  VALUE SPACES.
       01 WS-ASFTR.
          05  OT-COMP-ID           PIC XXX.
          05  OT-PRINT-NOW-SW      PIC X.
          05  OT-ARCHIVE-NO        PIC 9(08).
061421    05  OT-FUNC              PIC X(06).
041320    05  ot-batch-no          pic x(06).
041320    05  ot-batch-seq-no      pic x(08).
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
      *                                COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
      *****************************************
      * symbol list for the ASBOD template
      *****************************************
      *                                COPY NSCASVARS.
101612******************************************************************
101612*                   C H A N G E   L O G
101612*
121015*        CURRENT SIZE - 6180
091213*
101612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101612*-----------------------------------------------------------------
101612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101612* EFFECTIVE    NUMBER
101612*-----------------------------------------------------------------
101612* 101612  CR2011022800001  AJRA  REMOVE SIGN FROM CHG TOTALS
110612* 110612  CR2012101700002  AJRA  ADD NEW FIELDS
091213* 091213  CR2013090300001  AJRA  ADD NEXT BUSINESS DATE
121015* 121015  CR2015100900001  TANA  ADD VIN NUMBER
101612******************************************************************
       01 NAPER-OUTPUT-DATA.
          05  F                       PIC X(07) VALUE "LETTER=".
          05  OUT-LETTER              PIC X(06) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~PROCID=".
          05  OUT-PROC-ID             PIC X(04) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~PROCNAME=".
          05  OUT-PROC-NAME           PIC X(30) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~PROCTITLE=".
          05  OUT-PROC-TITLE          PIC X(30) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~CSRNAME=".
          05  OUT-CSR-NAME            PIC X(30) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~CSRTITLE=".
          05  OUT-CSR-TITLE           PIC X(30) VALUE SPACES.
          05  F                       PIC X(06) VALUE "~CARR=".
          05  OUT-CARRIER             PIC X(01) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~GROUPING=".
          05  OUT-GROUPING            PIC X(06) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~CERTST=".
          05  OUT-STATE               PIC X(02) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~ACCOUNT=".
          05  OUT-ACCOUNT             PIC X(10) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~CERTEFFDT=".
          05  OUT-CERT-EFF-DT         PIC X(21) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~CRTNO=".
          05  OUT-CERT-NO             PIC X(11) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~CRTSUF=".
          05  OUT-CERT-SFX            PIC X(01) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~ILNAME=".
          05  OUT-ILNAME              PIC X(15) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~IFNAME=".
          05  OUT-IFNAME              PIC X(10) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~IFINIT=".
          05  OUT-IFINIT              PIC X(01) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~IMINIT=".
          05  OUT-IMINIT              PIC X(01) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~IISSAGE=".
          05  OUT-IAGE                PIC 99    BLANK WHEN ZERO.
          05  F                       PIC X(06) VALUE "~ISEX=".
          05  OUT-ISEX                PIC X(01) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~INSADDR1=".
          05  OUT-INS-ADDR1           PIC X(30) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~INSADDR2=".
          05  OUT-INS-ADDR2           PIC X(30) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~INSCITY=".
          05  OUT-INS-CITY            PIC X(30) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~INSSTATE=".
          05  OUT-INS-STATE           PIC X(02) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~INSZIP=".
          05  OUT-INS-ZIP             PIC X(09) VALUE SPACES.
          05  F                       PIC X(05) VALUE "~SSN=".
          05  OUT-SOC-SEC-NO          PIC X(11) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~MEMNO=".
          05  OUT-MEMBER-NO           PIC X(12) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~JLNAME=".
          05  OUT-JLNAME              PIC X(15) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~JFNAME=".
          05  OUT-JFNAME              PIC X(10) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~JMINIT=".
          05  OUT-JMINIT              PIC X(01) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~JNTAGE=".
          05  OUT-JAGE                PIC 99    BLANK WHEN ZERO.
          05  F                       PIC X(10) VALUE "~ACCTNAME=".
          05  OUT-ACCT-NAME           PIC X(30) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~ACCTADDR1=".
          05  OUT-ACCT-ADDR1          PIC X(30) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~ACCTADDR2=".
          05  OUT-ACCT-ADDR2          PIC X(30) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~ACCTCITY=".
          05  OUT-ACCT-CITY           PIC X(30) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~ACCTSTATE=".
          05  OUT-ACCT-STATE          PIC X(02) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~ACCTZIP=".
          05  OUT-ACCT-ZIP            PIC X(10) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~ACCTPHONE=".
          05  OUT-ACCT-PHONE          PIC X(10) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~ACCTCNTRL=".
          05  OUT-ACCT-CNTRL-NAME     PIC X(30) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~BUSTYPE=".
          05  OUT-ACCT-BUS-TYPE       PIC X(02) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~BENENAME=".
          05  OUT-BENE-NAME           PIC X(30) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~BENEADDR1=".
          05  OUT-BENE-ADDR1          PIC X(30) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~BENEADDR2=".
          05  OUT-BENE-ADDR2          PIC X(30) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~BENECITY=".
          05  OUT-BENE-CITY           PIC X(30) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~BENESTATE=".
          05  OUT-BENE-STATE          PIC X(02) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~BENEZIP=".
          05  OUT-BENE-ZIP            PIC X(09) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~CARRNAME=".
          05  OUT-CARR-NAME           PIC X(30) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~RESPNO=".
          05  OUT-RESP-NO             PIC X(10) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~COACCTNAME=".
          05  OUT-COMP-NAME           PIC X(30) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~COMAILNAME=".
          05  OUT-COMP-MAIL-TO        PIC X(30) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~COMPADDR1=".
          05  OUT-COMP-ADDR1          PIC X(30) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~COMPADDR2=".
          05  OUT-COMP-ADDR2          PIC X(30) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~COMPCITY=".
          05  OUT-COMP-CITY           PIC X(30) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~COMPSTATE=".
          05  OUT-COMP-STATE          PIC X(02) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~COMPZIP=".
          05  OUT-COMP-ZIP            PIC X(09) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~COMPPHONE=".
          05  OUT-COMP-PHONE          PIC X(10) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~COMPFAX=".
          05  OUT-COMP-FAX            PIC X(10) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~COMPSTATUS=".
          05  OUT-COMP-STATUS         PIC X(01) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~COMPBILLSW=".
          05  OUT-BILL-SW             PIC X(01) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~RPTCD1=".
          05  OUT-RPT-CD1             PIC X(10) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~RPTCD2=".
          05  OUT-RPT-CD2             PIC X(10) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~ENTRYDT=".
          05  OUT-ENTRY-DT            PIC X(21) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~BATCH=".
          05  OUT-ENTRY-BATCH         PIC X(06) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~ENTSTATUS=".
          05  OUT-ENTRY-STATUS        PIC X(01) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~1STPMTDT=".
          05  OUT-1ST-PMT-DT          PIC X(21) VALUE SPACES.
          05  F                       PIC X(05) VALUE "~APR=".
          05  OUT-LOAN-APR            PIC 999.9(4)  BLANK WHEN ZERO.
          05  F                       PIC X(08) VALUE "~LNTERM=".
          05  OUT-LOAN-TERM           PIC 999   BLANK WHEN ZERO.
          05  F                       PIC X(11) VALUE "~RATECLASS=".
          05  OUT-RATE-CLASS          PIC X(02) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~EXTDAYS=".
          05  OUT-EXT-DAYS            PIC 999   BLANK WHEN ZERO.
          05  F                       PIC X(09) VALUE "~CSRCODE=".
          05  OUT-CSR-CODE            PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~UCODE=".
          05  OUT-UCODE               PIC X(01) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~PREMTYPE=".
          05  OUT-PREM-TYPE           PIC X(01) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~INDGRPTYPE=".
          05  OUT-IND-GRP             PIC X(01) VALUE SPACES.
          05  F                       PIC X(06) VALUE "~SKIP=".
          05  OUT-SKIP-CD             PIC X(01) VALUE SPACES.
          05  F                       PIC X(06) VALUE "~MODE=".
          05  OUT-PMT-MODE            PIC X(01) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~LOANOFF=".
          05  OUT-LOAN-OFF            PIC X(05) VALUE SPACES.
          05  F                       PIC X(06) VALUE "~RTBL=".
          05  OUT-REIN-TABLE          PIC X(03) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~SPECREIN=".
          05  OUT-SPEC-REIN           PIC X(01) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~LFBENCD=".
          05  OUT-LF-BENCD            PIC X(02) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~LFTERM=".
          05  OUT-LF-TERM             PIC 999   BLANK WHEN ZERO.
          05  F                       PIC X(07) VALUE "~LFDEV=".
          05  OUT-LF-DEV-CD           PIC X(03) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~LFDEVPCT=".
          05  OUT-LF-DEV-PCT          PIC 9.9(6)  BLANK WHEN ZERO.
          05  F                       PIC X(07) VALUE "~LFBEN=".
          05  OUT-LF-BEN              PIC 9(9).99 BLANK WHEN ZERO.
          05  F                       PIC X(08) VALUE "~LFPREM=".
          05  OUT-LF-PRM              PIC 9(7).99 BLANK WHEN ZERO.
          05  F                       PIC X(08) VALUE "~LFABEN=".
          05  OUT-LF-ALT-BEN          PIC 9(9).99 BLANK WHEN ZERO.
          05  F                       PIC X(09) VALUE "~LFAPREM=".
          05  OUT-LF-ALT-PRM          PIC 9(7).99 BLANK WHEN ZERO.
          05  F                       PIC X(07) VALUE "~LFNSP=".
          05  OUT-LF-NSP              PIC 9(7).99 BLANK WHEN ZERO.
          05  F                       PIC X(08) VALUE "~LFRBEN=".
          05  OUT-LF-REM-BEN          PIC 9(9).99 BLANK WHEN ZERO.
          05  F                       PIC X(07) VALUE "~LFCAN=".
          05  OUT-LF-REF              PIC 9(7).99 BLANK WHEN ZERO.
          05  F                       PIC X(07) VALUE "~LFDTH=".
          05  OUT-LF-DTH              PIC 9(9).99 BLANK WHEN ZERO.
          05  F                       PIC X(08) VALUE "~LFRATE=".
          05  OUT-LF-RATE             PIC 99.9(5) BLANK WHEN ZERO.
          05  F                       PIC X(09) VALUE "~LFARATE=".
          05  OUT-LF-ALT-RATE         PIC 99.9(5) BLANK WHEN ZERO.
          05  F                       PIC X(09) VALUE "~LFEXPDT=".
          05  OUT-LF-EXP-DT           PIC X(21) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~LFSTAT=".
          05  OUT-LF-CUR-STATUS       PIC X(01) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~LFCANDT=".
          05  OUT-LF-CAN-DT           PIC X(21) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~LFCANEXTDT=".
          05  OUT-LF-CAN-EXIT-DT      PIC X(21) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~DTHDT=".
          05  OUT-LF-DTH-DT           PIC X(21) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~DTHEXTDT=".
          05  OUT-LF-DTH-EXIT-DT      PIC X(21) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~LFEXTBATCH=".
          05  OUT-LF-EXIT-BATCH       PIC X(06) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~LFCOMM=".
          05  OUT-LF-COMM-PCT         PIC .9(5) BLANK WHEN ZERO.
          05  F                       PIC X(11) VALUE "~LFBENDESC=".
          05  OUT-LF-DESC             PIC X(02) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~LFBENABRV=".
          05  OUT-LF-ABBRV            PIC X(03) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~AHBENCD=".
          05  OUT-AH-BENCD            PIC X(02) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~AHTERM=".
          05  OUT-AH-TERM             PIC 999   BLANK WHEN ZERO.
          05  F                       PIC X(11) VALUE "~AHCRITPER=".
          05  OUT-CRIT-PER            PIC 99    BLANK WHEN ZERO.
          05  F                       PIC X(09) VALUE "~AHDEVCD=".
          05  OUT-AH-DEV-CD           PIC X(03) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~AHDEVPCT=".
          05  OUT-AH-DEV-PCT          PIC 9.9(6) BLANK WHEN ZERO.
          05  F                       PIC X(07) VALUE "~AHBEN=".
          05  OUT-AH-BEN              PIC 9(7).99 BLANK WHEN ZERO.
          05  F                       PIC X(08) VALUE "~AHPREM=".
          05  OUT-AH-PRM              PIC 9(7).99 BLANK WHEN ZERO.
          05  F                       PIC X(07) VALUE "~AHNSP=".
          05  OUT-AH-NSP              PIC 9(7).99 BLANK WHEN ZERO.
          05  F                       PIC X(07) VALUE "~AHCAN=".
          05  OUT-AH-REF              PIC 9(7).99 BLANK WHEN ZERO.
          05  F                       PIC X(10) VALUE "~AHITDPMT=".
          05  OUT-AH-CLM              PIC 9(7).99 BLANK WHEN ZERO.
          05  F                       PIC X(10) VALUE "~AHTOTBEN=".
          05  OUT-AH-TOT-BEN          PIC 9(9).99 BLANK WHEN ZERO.
          05  F                       PIC X(12) VALUE "~AHPDTHRUDT=".
          05  OUT-AH-PDTHRU-DT        PIC X(21) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~AHRATE=".
          05  OUT-AH-RATE             PIC 99.9(5) BLANK WHEN ZERO.
          05  F                       PIC X(09) VALUE "~AHEXPDT=".
          05  OUT-AH-EXP-DT           PIC X(21) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~AHSTAT=".
          05  OUT-AH-CUR-STATUS       PIC X(01) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~AHCANDT=".
          05  OUT-AH-CAN-DT           PIC X(21) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~AHCANEXTDT=".
          05  OUT-AH-CAN-EXIT-DT      PIC X(21) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~AHEXTBATCH=".
          05  OUT-AH-EXIT-BATCH       PIC X(06) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~AHCOMM=".
          05  OUT-AH-COMM-PCT         PIC .9(5) BLANK WHEN ZERO.
          05  F                       PIC X(11) VALUE "~AHBENDESC=".
          05  OUT-AH-DESC             PIC X(35) VALUE SPACES.
          05  F                       PIC X(14) VALUE "~AHBENRETELIM=".
          05  OUT-RET-ELIM            PIC X(01) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~AHBENDAYS=".
          05  OUT-BEN-DAYS            PIC X(02) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~AHWAIT=".
          05  OUT-WAIT-PER            PIC X(03) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~AHMAXPMTS=".
          05  OUT-MAX-PMTS            PIC 99    VALUE ZEROS.
          05  OUT-MAX-PMTS-A REDEFINES OUT-MAX-PMTS
                                      PIC X(2).
          05  F                       PIC X(09) VALUE "~TOTPREM=".
          05  OUT-TOT-PRM             PIC 9(7).99 BLANK WHEN ZERO.
          05  F                       PIC X(08) VALUE "~TOTREF=".
          05  OUT-TOT-REF             PIC 9(7).99 BLANK WHEN ZERO.
          05  F                       PIC X(08) VALUE "~SEXPDT=".
          05  OUT-SCHED-EXP-DT        PIC X(21) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~STERM=".
          05  OUT-SCHED-TERM          PIC 999   BLANK WHEN ZERO.
          05  F                       PIC X(12) VALUE "~IORIGLNAME=".
          05  OUT-ORIG-ILNAME         PIC X(15) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~IORIGFNAME=".
          05  OUT-ORIG-IFNAME         PIC X(10) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~IORIGINIT=".
          05  OUT-ORIG-MINIT          PIC X(01) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~IORIGAGE=".
          05  OUT-ORIG-IAGE           PIC 99    BLANK WHEN ZERO.
          05  F                       PIC X(12) VALUE "~JORIGLNAME=".
          05  OUT-ORIG-JLNAME         PIC X(15) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~JORIGFNAME=".
          05  OUT-ORIG-JFNAME         PIC X(10) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~JORIGINIT=".
          05  OUT-ORIG-JMINIT         PIC X(01) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~JORIGAGE=".
          05  OUT-ORIG-JAGE           PIC 99    BLANK WHEN ZERO.
          05  F                       PIC X(13) VALUE "~LFORIGBENCD=".
          05  OUT-ORIG-LF-BENCD       PIC X(02) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~LFORIGTERM=".
          05  OUT-ORIG-LF-TERM        PIC 999   BLANK WHEN ZERO.
          05  F                       PIC X(14) VALUE "~LFORIGBENAMT=".
          05  OUT-ORIG-LF-BEN         PIC 9(9).99 VALUE ZEROS.
          05  OUT-ORIG-LF-BEN-A REDEFINES OUT-ORIG-LF-BEN
                                      PIC X(12).
          05  F                       PIC X(14) VALUE "~LFORIGPRMAMT=".
          05  OUT-ORIG-LF-PRM         PIC 9(9).99 VALUE ZEROS.
          05  OUT-ORIG-LF-PRM-A REDEFINES OUT-ORIG-LF-PRM
                                      PIC X(12).
          05  F                       PIC X(14) VALUE "~LFCALCPRMAMT=".
          05  OUT-LF-CALC-PRM         PIC 9(9).99 VALUE ZEROS.
          05  F                       PIC X(14) VALUE "~LFORIGREFAMT=".
          05  OUT-ORIG-LF-REF         PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(14) VALUE "~LFCALCREFAMT=".
          05  OUT-LF-CALC-REF         PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(14) VALUE "~LFORIGALTBEN=".
          05  OUT-ORIG-LF-ALT-BEN     PIC 9(9).99 VALUE ZEROS.
          05  OUT-ORIG-LF-ALT-BEN-A REDEFINES OUT-ORIG-LF-ALT-BEN
                                      PIC X(12).
          05  F                       PIC X(14) VALUE "~LFORIGALTPRM=".
          05  OUT-ORIG-LF-ALT-PRM     PIC 9(7).99 VALUE ZEROS.
          05  OUT-ORIG-LF-ALT-PRM-A REDEFINES OUT-ORIG-LF-ALT-PRM
                                      PIC X(10).
          05  F                       PIC X(13) VALUE "~LFORIGEXPDT=".
          05  OUT-ORIG-LF-EXP-DT      PIC X(21) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~LFORIGDESC=".
          05  OUT-ORIG-LF-DESC        PIC X(02) VALUE SPACES.
          05  F                       PIC X(13) VALUE "~AHORIGBENCD=".
          05  OUT-ORIG-AH-BENCD       PIC X(02) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~AHORIGTERM=".
          05  OUT-ORIG-AH-TERM        PIC 999   BLANK WHEN ZERO.
          05  F                       PIC X(13) VALUE "~AHORIGCRITP=".
          05  OUT-ORIG-CRIT-PER       PIC 99    BLANK WHEN ZERO.
          05  F                       PIC X(14) VALUE "~AHORIGBENAMT=".
          05  OUT-ORIG-AH-BEN         PIC 9(7).99 VALUE ZEROS.
          05  OUT-ORIG-AH-BEN-A REDEFINES OUT-ORIG-AH-BEN
                                      PIC X(10).
          05  F                       PIC X(14) VALUE "~AHORIGPRMAMT=".
          05  OUT-ORIG-AH-PRM         PIC 9(7).99 VALUE ZEROS.
          05  OUT-ORIG-AH-PRM-A REDEFINES OUT-ORIG-AH-PRM
                                      PIC X(10).
          05  F                       PIC X(14) VALUE "~AHCALCPRMAMT=".
          05  OUT-AH-CALC-PRM         PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(14) VALUE "~AHORIGREFAMT=".
          05  OUT-ORIG-AH-REF         PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(14) VALUE "~AHCALCREFAMT=".
          05  OUT-AH-CALC-REF         PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(13) VALUE "~AHORIGEXPDT=".
          05  OUT-ORIG-AH-EXP-DT      PIC X(21) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~AHORIGDESC=".
          05  OUT-ORIG-AH-DESC        PIC X(35) VALUE SPACES.
          05  F                       PIC X(13) VALUE "~AHORIGRELIM=".
          05  OUT-ORIG-RET-ELIM       PIC X(01) VALUE SPACES.
          05  F                       PIC X(13) VALUE "~ORIGBENDAYS=".
          05  OUT-ORIG-BEN-DAYS       PIC X(02) VALUE ZEROS.
          05  F                       PIC X(10) VALUE "~ORIGWAIT=".
          05  OUT-ORIG-WAIT-PER       PIC X(03) VALUE SPACES.
          05  F                       PIC X(13) VALUE "~ORIGMAXPMTS=".
          05  OUT-ORIG-MAX-PMTS       PIC 99    VALUE ZEROS.
          05  OUT-ORIG-MAX-PMTS-A REDEFINES OUT-ORIG-MAX-PMTS
                                      PIC X(2).
          05  F                       PIC X(09) VALUE "~OSEXPDT=".
          05  OUT-ORIG-SCHED-EXP-DT   PIC X(21) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~OSTERM=".
          05  OUT-ORIG-SCHED-TERM     PIC 9(09) BLANK WHEN ZERO.
          05  F                       PIC X(11) VALUE "~INEWLNAME=".
          05  OUT-NEW-ILNAME          PIC X(15) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~INEWFNAME=".
          05  OUT-NEW-IFNAME          PIC X(10) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~INEWINIT=".
          05  OUT-NEW-IMINIT          PIC X(01) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~INEWAGE=".
          05  OUT-NEW-IAGE            PIC 9(09) BLANK WHEN ZERO.
          05  OUT-NEW-IAGE-A REDEFINES OUT-NEW-IAGE
                                      PIC X(09).
          05  F                       PIC X(11) VALUE "~JNEWLNAME=".
          05  OUT-NEW-JLNAME          PIC X(15) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~JNEWFNAME=".
          05  OUT-NEW-JFNAME          PIC X(10) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~JNEWINIT=".
          05  OUT-NEW-JMINIT          PIC X(01) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~JNEWAGE=".
          05  OUT-NEW-JAGE            PIC 9(09) BLANK WHEN ZERO.
          05  OUT-NEW-JAGE-A REDEFINES OUT-NEW-JAGE
                                      PIC X(09).
          05  F                       PIC X(12) VALUE "~LFNEWBENCD=".
          05  OUT-NEW-LF-BENCD        PIC X(09) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~LFNEWTERM=".
          05  OUT-NEW-LF-TERM         PIC 9(09) BLANK WHEN ZERO.
          05  OUT-NEW-LF-TERM-A REDEFINES OUT-NEW-LF-TERM
                                      PIC X(09).
          05  F                       PIC X(13) VALUE "~LFNEWBENAMT=".
          05  OUT-NEW-LF-BEN          PIC 9(9).99 VALUE ZEROS.
          05  OUT-NEW-LF-BEN-A REDEFINES OUT-NEW-LF-BEN
                                      PIC X(12).
          05  F                       PIC X(13) VALUE "~LFNEWPRMAMT=".
          05  OUT-NEW-LF-PRM          PIC 9(7).99 VALUE ZEROS.
          05  OUT-NEW-LF-PRM-A REDEFINES OUT-NEW-LF-PRM
                                      PIC X(10).
          05  F                       PIC X(13) VALUE "~LFNEWREFAMT=".
          05  OUT-NEW-LF-REF          PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(13) VALUE "~LFNEWALTBEN=".
          05  OUT-NEW-LF-ALT-BEN      PIC 9(9).99 VALUE ZEROS.
          05  OUT-NEW-LF-ALT-BEN-A REDEFINES OUT-NEW-LF-ALT-BEN
                                      PIC X(12).
          05  F                       PIC X(13) VALUE "~LFNEWALTPRM=".
          05  OUT-NEW-LF-ALT-PRM      PIC 9(7).99 VALUE ZEROS.
          05  OUT-NEW-LF-ALT-PRM-A REDEFINES OUT-NEW-LF-ALT-PRM
                                      PIC X(10).
          05  F                       PIC X(12) VALUE "~LFNEWEXPDT=".
          05  OUT-NEW-LF-EXP-DT       PIC X(21) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~LFNEWDESC=".
          05  OUT-NEW-LF-DESC         PIC X(10) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~AHNEWBENCD=".
          05  OUT-NEW-AH-BENCD        PIC X(02) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~AHNEWTERM=".
          05  OUT-NEW-AH-TERM         PIC 9(09) BLANK WHEN ZERO.
          05  OUT-NEW-AH-TERM-A REDEFINES OUT-NEW-AH-TERM
                                      PIC X(09).
          05  F                       PIC X(12) VALUE "~AHNEWCRITP=".
          05  OUT-NEW-CRIT-PER        PIC 9(09) BLANK WHEN ZERO.
          05  OUT-NEW-CRIT-PER-A REDEFINES OUT-NEW-CRIT-PER
                                      PIC X(09).
          05  F                       PIC X(13) VALUE "~AHNEWBENAMT=".
          05  OUT-NEW-AH-BEN          PIC 9(7).99 VALUE ZEROS.
          05  OUT-NEW-AH-BEN-A REDEFINES OUT-NEW-AH-BEN
                                      PIC X(10).
          05  F                       PIC X(13) VALUE "~AHNEWPRMAMT=".
          05  OUT-NEW-AH-PRM          PIC 9(7).99 VALUE ZEROS.
          05  OUT-NEW-AH-PRM-A REDEFINES OUT-NEW-AH-PRM
                                      PIC X(10).
          05  F                       PIC X(13) VALUE "~AHNEWREFAMT=".
          05  OUT-NEW-AH-REF          PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(12) VALUE "~AHNEWEXPDT=".
          05  OUT-NEW-AH-EXP-DT       PIC X(21) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~AHNEWDESC=".
          05  OUT-NEW-AH-DESC         PIC X(35) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~AHNEWRELIM=".
          05  OUT-NEW-RET-ELIM        PIC X(01) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~NEWBENDAYS=".
          05  OUT-NEW-BEN-DAYS        PIC X(09) VALUE ZEROS.
          05  F                       PIC X(09) VALUE "~NEWWAIT=".
          05  OUT-NEW-WAIT-PER        PIC X(09) VALUE SPACES.
          05  F                       PIC X(12) VALUE "~NEWMAXPMTS=".
          05  OUT-NEW-MAX-PMTS        PIC 9(09) VALUE ZEROS.
          05  OUT-NEW-MAX-PMTS-A REDEFINES OUT-NEW-MAX-PMTS
                                      PIC X(09).
          05  F                       PIC X(09) VALUE "~NSEXPDT=".
          05  OUT-NEW-SCHED-EXP-DT    PIC X(21) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~NSTERM=".
          05  OUT-NEW-SCHED-TERM      PIC 9(09) BLANK WHEN ZERO.
          05  OUT-NEW-SCHED-TERM-A REDEFINES OUT-NEW-SCHED-TERM
                                      PIC X(09).
          05  F                       PIC X(12) VALUE "~TOTPREMCHG=".
101612    05  OUT-TOT-PRM-CHG         PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(11) VALUE "~TOTREFCHG=".
101612    05  OUT-TOT-REF-CHG         PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(11) VALUE "~PAYABLETO=".
          05  OUT-PAYEE               PIC X(30) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~SIGNEEDED=".
          05  OUT-SIG-SW              PIC X(01) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~BALLOON=".
          05  OUT-BALLOON-IND         PIC X(01) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~LEASEIND=".
          05  OUT-LEASE-IND           PIC X(01) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~RESCIND=".
          05  OUT-RESCIND             PIC X(01) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD01=".
          05  OUT-REA-CD1             PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD02=".
          05  OUT-REA-CD2             PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD03=".
          05  OUT-REA-CD3             PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD04=".
          05  OUT-REA-CD4             PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD05=".
          05  OUT-REA-CD5             PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD06=".
          05  OUT-REA-CD6             PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD07=".
          05  OUT-REA-CD7             PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD08=".
          05  OUT-REA-CD8             PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD09=".
          05  OUT-REA-CD9             PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD10=".
          05  OUT-REA-CD10            PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD11=".
          05  OUT-REA-CD11            PIC X(04) VALUE SPACES.
          05  F                       PIC X(07) VALUE "~RCD12=".
          05  OUT-REA-CD12            PIC X(04) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~CYCLEDATE=".
          05  OUT-CYCLE-DT            PIC X(21) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~ARCHNO=".
          05  OUT-ARCH-NO             PIC 9(9)  VALUE ZEROS.
          05  F                       PIC X(06) VALUE "~FORM=".
          05  OUT-FORM                PIC X(03) VALUE SPACES.
          05  F                       PIC X(09) VALUE "~ENCLINE=".
          05  OUT-ENC-LINE            PIC X(50) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~ATTACH=".
          05  OUT-ATTACH              PIC X(50) VALUE SPACES.
          05  F                       PIC X(10) VALUE "~OUTSTACK=".
          05  OUT-STACK               PIC X(10) VALUE SPACES.
          05  F                       PIC X(11) VALUE "~STATENAME=".
          05  OUT-STATE-NAME          PIC X(25) VALUE SPACES.
          05  F                       PIC X(08) VALUE "~PRTNOW=".
          05  OUT-PRINT-NOW           PIC X(01) VALUE SPACES.
          05  F                       PIC X(05) VALUE "~NCB=".
          05  OUT-CHGBACK             PIC X     VALUE ' '.
          05  F                       PIC X(8) VALUE "~CSOAMT=".
          05  OUT-CSO-PORTION         PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(9) VALUE "~ACCTAMT=".
          05  OUT-ACCT-PORTION        PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(09) VALUE "~LETRTYP=".
110612    05  OUT-LETTER-TYPE         PIC X(01) VALUE SPACES.
110612    05  F                       PIC X(10) VALUE "~PRNTCERT=".
110612    05  OUT-PRINT-CERTIFICATE   PIC X(01) VALUE SPACES.
110612    05  F                       PIC X(08) VALUE "~INSBDT=".
110612    05  OUT-INS-BIRTHDATE       PIC X(21) VALUE SPACES.
110612    05  F                       PIC X(08) VALUE "~JNTBDT=".
110612    05  OUT-JNT-BIRTHDATE       PIC X(21) VALUE SPACES.
110612    05  F                       PIC X(08) VALUE "~TOTINT=".
110612    05  OUT-TOT-INTEREST        PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(8) VALUE "~TOPREM=".
          05  OUT-ORIG-TOT-PREM       PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(7) VALUE "~TOREF=".
          05  OUT-ORIG-TOT-REF        PIC 9(7).99 VALUE ZEROS.
          05  F                       PIC X(7) VALUE "~CANDT=".
          05  OUT-CANCEL-DT           PIC X(21) VALUE SPACES.
          05  F                       PIC X(9)  VALUE "~HLTHAPP=".
          05  OUT-HEALTH-APP          PIC X     VALUE SPACES.
110612    05  F                       PIC X(08) VALUE "~CERTID=".
110612    05  OUT-CERTIFICATE-ID      PIC X(5)  VALUE SPACES.
071212    05  F                       PIC X(08) VALUE "~UNDWID=".
071212    05  OUT-UNDW-ID             PIC X(04) VALUE SPACES.
072312    05  F                       PIC X(8) VALUE "~TNPREM=".
072312    05  OUT-NEW-TOT-PREM        PIC 9(7).99 VALUE ZEROS.
072312    05  F                       PIC X(7) VALUE "~TNREF=".
072312    05  OUT-NEW-TOT-REF         PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(8)  VALUE "~COVIND=".
110612    05  OUT-COVERAGE-IND        PIC X(4)  VALUE SPACES.
110612    05  F                       PIC X(9)  VALUE "~OCOVIND=".
110612    05  OUT-ORIG-COVERAGE-IND   PIC X(4)  VALUE SPACES.
110612    05  F                       PIC X(9)  VALUE "~NCOVIND=".
110612    05  OUT-NEW-COVERAGE-IND    PIC X(4)  VALUE SPACES.
110612    05  F                       PIC X(10) VALUE "~LFCMBPRM=".
110612    05  OUT-LF-CMB-PREM         PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~LFOCMBPRM=".
110612    05  OUT-ORIG-LF-CMB-PREM    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~LFNCMBPRM=".
110612    05  OUT-NEW-LF-CMB-PREM     PIC 9(7).99 VALUE ZEROS.
110612    05  OUT-NEW-LF-CMB-PREM-A REDEFINES OUT-NEW-LF-CMB-PREM
110612                                PIC X(10).
110612    05  F                       PIC X(11) VALUE "~LFPREMCHG=".
110612    05  OUT-LF-PREM-CHG         PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~LFAPRMCHG=".
110612    05  OUT-LF-ALT-PREM-CHG     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~LFCPRMCHG=".
110612    05  OUT-LF-CMB-PREM-CHG     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~AHPREMCHG=".
110612    05  OUT-AH-PREM-CHG         PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(10) VALUE "~LFREFCHG=".
110612    05  OUT-LF-REF-CHG          PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(10) VALUE "~AHREFCHG=".
110612    05  OUT-AH-REF-CHG          PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~ACCTLFAMT=".
110612    05  OUT-ACCT-LF-PORTION     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~ACTALFAMT=".
110612    05  OUT-ACCT-ALT-LF-PORT    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~ACTCLFAMT=".
110612    05  OUT-ACCT-CMB-LF-PORT    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~ACCTAHAMT=".
110612    05  OUT-ACCT-AH-PORTION     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(10) VALUE "~CSOLFAMT=".
110612    05  OUT-CSO-LF-PORTION      PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~CSOALFAMT=".
110612    05  OUT-CSO-ALT-LF-PORT     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~CSOCLFAMT=".
110612    05  OUT-CSO-CMB-LF-PORT     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(10) VALUE "~CSOAHAMT=".
110612    05  OUT-CSO-AH-PORTION      PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~ACTOLFAMT=".
110612    05  OUT-ACCT-ORIG-LF-PORT   PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(12) VALUE "~ACTOALFAMT=".
110612    05  OUT-ACCT-ORIG-ALT-LF-PORT PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(12) VALUE "~ACTOCLFAMT=".
110612    05  OUT-ACCT-ORIG-CMB-LF-PORT PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~ACTOAHAMT=".
110612    05  OUT-ACCT-ORIG-AH-PORT   PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(10) VALUE "~ACCTOAMT=".
110612    05  OUT-ACCT-ORIG-PORTION   PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~CSOOLFAMT=".
110612    05  OUT-CSO-ORIG-LF-PORT    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(12) VALUE "~CSOOALFAMT=".
110612    05  OUT-CSO-ORIG-ALT-LF-PORT PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(12) VALUE "~CSOOCLFAMT=".
110612    05  OUT-CSO-ORIG-CMB-LF-PORT PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~CSOOAHAMT=".
110612    05  OUT-CSO-ORIG-AH-PORT    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(09) VALUE "~CSOOAMT=".
110612    05  OUT-CSO-ORIG-PORTION    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~ACTNLFAMT=".
110612    05  OUT-ACCT-NEW-LF-PORT    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(12) VALUE "~ACTNALFAMT=".
110612    05  OUT-ACCT-NEW-ALT-LF-PORT PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(12) VALUE "~ACTNCLFAMT=".
110612    05  OUT-ACCT-NEW-CMB-LF-PORT PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~ACTNAHAMT=".
110612    05  OUT-ACCT-NEW-AH-PORT    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(10) VALUE "~ACCTNAMT=".
110612    05  OUT-ACCT-NEW-PORTION    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~CSONLFAMT=".
110612    05  OUT-CSO-NEW-LF-PORT     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(12) VALUE "~CSONALFAMT=".
110612    05  OUT-CSO-NEW-ALT-LF-PORT PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(12) VALUE "~CSONCLFAMT=".
110612    05  OUT-CSO-NEW-CMB-LF-PORT PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~CSONAHAMT=".
110612    05  OUT-CSO-NEW-AH-PORT     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(09) VALUE "~CSONAMT=".
110612    05  OUT-CSO-NEW-PORTION     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(10) VALUE "~ACTLFCHG=".
110612    05  OUT-ACCT-LF-PORT-CHG    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~ACTALFCHG=".
110612    05  OUT-ACCT-ALT-LF-PORT-CHG PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~ACTCLFCHG=".
110612    05  OUT-ACCT-CMB-LF-PORT-CHG PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(10) VALUE "~ACTAHCHG=".
110612    05  OUT-ACCT-AH-PORT-CHG    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(09) VALUE "~ACCTCHG=".
110612    05  OUT-ACCT-PORTION-CHG    PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(10) VALUE "~CSOLFCHG=".
110612    05  OUT-CSO-LF-PORT-CHG     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~CSOALFCHG=".
110612    05  OUT-CSO-ALT-LF-PORT-CHG PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(11) VALUE "~CSOCLFCHG=".
110612    05  OUT-CSO-CMB-LF-PORT-CHG PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(10) VALUE "~CSOAHCHG=".
110612    05  OUT-CSO-AH-PORT-CHG     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(08) VALUE "~CSOCHG=".
110612    05  OUT-CSO-PORTION-CHG     PIC 9(7).99 VALUE ZEROS.
110612    05  F                       PIC X(10) VALUE "~LFOCANDT=".
110612    05  OUT-ORIG-LF-CAN-DT      PIC X(21) VALUE SPACES.
110612    05  F                       PIC X(10) VALUE "~LFNCANDT=".
110612    05  OUT-NEW-LF-CAN-DT       PIC X(21) VALUE SPACES.
110612    05  F                       PIC X(10) VALUE "~AHOCANDT=".
110612    05  OUT-ORIG-AH-CAN-DT      PIC X(21) VALUE SPACES.
110612    05  F                       PIC X(10) VALUE "~AHNCANDT=".
110612    05  OUT-NEW-AH-CAN-DT       PIC X(21) VALUE SPACES.
110612    05  F                       PIC X(11) VALUE "~ORIGCANDT=".
110612    05  OUT-ORIG-CAN-DT         PIC X(21) VALUE SPACES.
110612    05  F                       PIC X(10) VALUE "~NEWCANDT=".
110612    05  OUT-NEW-CAN-DT          PIC X(21) VALUE SPACES.
110612    05  F                       PIC X(8)  VALUE "~SCREEN=".
110612    05  OUT-SCREEN-ID           PIC X(8)  VALUE SPACES.
091213    05  F                       PIC X(9)  VALUE "~NXTBSDT=".
091213    05  OUT-NEXT-BUS-DT         PIC X(21) VALUE SPACES.
121015    05  F                       PIC X(7)  VALUE "~VINNO=".
121015    05  OUT-VIN-NUMBER          PIC X(17) VALUE SPACES.
          05  F                       PIC X(11) VALUE 'EndOfString'.
091213    05  f                       pic x(2) value spaces.
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
           MOVE 'NSRASLTR' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
      *********************
      * Receive web input
      *********************
PEMTST*    DISPLAY ' ENTERING NSRASLTR '
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00'
PEMTST        DISPLAY '  KIXSYS = ' var (1:env-var-len)
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
PEMTST*       DISPLAY ' WS KIX SYS ' WS-KIXSYS
PEMTST*       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if
           set P to address of KIXHOST
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixhost not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00'
              MOVE var(1:env-var-len)  to ws-kixhost
              DISPLAY ' WS KIX HOST ' WS-KIXHOST
           end-if
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-GREG-DATE-A-EDIT    TO ASERR-DT
           
      * exec cics web
      *       startbr formfield resp(w-resp)
      *     end-exec.
      *    MOVE 'X(f                   &  N#00001315' TO DFHEIV0
           MOVE X'582866202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031333135' TO DFHEIV0(25:11)
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
      *     end-exec
      *    MOVE 'X,f                   #   #00001320' TO DFHEIV0
           MOVE X'582C66202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
      *    display ' form input ' INPUT-FROM-FORM
           MOVE SPACES                  TO BL-INPUT
           MOVE IFF-DATA-SRCE           TO BL-DATA-SRCE
           MOVE IFF-CARRIER             TO BL-CARRIER
           MOVE IFF-GROUP               TO BL-GROUP
           MOVE IFF-STATE               TO BL-STATE
           MOVE IFF-ACCOUNT             TO BL-ACCOUNT
           MOVE IFF-EFF-DT              TO BL-EFF-DT
           MOVE IFF-CERT-NO             TO BL-CERT-NO
           MOVE IFF-BATCH-NO            TO BL-BATCH-NO
041320                                     ot-batch-no
           MOVE IFF-BATCH-SEQ           TO BL-BATCH-SEQ
041320                                     ot-batch-seq-no
           MOVE IFF-RESP-NO             TO BL-RESP-NO
           MOVE IFF-LETTER-ID           TO BL-LETTER-ID
           MOVE IFF-NO-OF-COPIES        TO BL-NO-OF-COPIES
           MOVE IFF-PROC-ID             TO BL-PROC-ID
           MOVE IFF-COMP-ID             TO OT-COMP-ID
                                           BL-COMP-ID
           MOVE IFF-PRINT-NOW-SW        TO OT-PRINT-NOW-SW
                                           BL-PRINT-NOW-SW
           MOVE IFF-ENC-CD              TO BL-ENC-CD
           MOVE IFF-RESEND-DT           TO BL-RESEND-DT
           MOVE IFF-FOLLOW-UP-DT        TO BL-FOLLOW-UP-DT
101812*    MOVE IFF-COMMENTS            TO BL-COMMENTS
           MOVE IFF-ARCHIVE-NO          TO BL-ARCHIVE-NO
           MOVE IFF-FUNC                TO BL-FUNC
                                           OT-FUNC
101812     MOVE IFF-ENDT-ARCH-NO        TO BL-ENDT-ARCH-NO
101812
101812     EVALUATE BL-FUNC
101812         WHEN 'Letter'
101812             MOVE 'LETTER'        TO BL-SOURCE-SCREEN
101812         WHEN 'CrtVerif'
101812             MOVE 'VERIFY'        TO BL-SOURCE-SCREEN
101812         WHEN 'Endorse'
101812             MOVE 'ISS ENDT'      TO BL-SOURCE-SCREEN
101812         WHEN 'CancChg'
101812             MOVE 'CAN ENDT'      TO BL-SOURCE-SCREEN
101812         WHEN OTHER
101812             MOVE 'UNKNOWN'       TO BL-SOURCE-SCREEN
101812     END-EVALUATE
101812
      *****************************************
      * Since this program was called from the webservice
      * we really don't know if a real letter is being created
      * a "T" below will create temporary erarch and nsasextr
      * records and "X" we won't create anything
      *****************************************
           MOVE 'T'                    TO BL-WRITE-ERARCH
           IF BL-PRINT-NOW-SW = 'P'
              MOVE 'X'                 TO BL-WRITE-ERARCH
           END-IF
           MOVE 'X'                 TO BL-WRITE-ERARCH
PEMTST*    DISPLAY ' INPUT FORM ' WS-KEY
      *****************************************
      * Invoke the LETTER business logic
      *****************************************
      *    DISPLAY ' BL INPUT ' BL-INPUT
           MOVE FUNCTION LENGTH(SRCH-COMMAREA) TO WS-COMM-LENGTH
           
      * exec cics link
      *       program('NSRASBL')
      *       commarea(srch-commarea)
      *       LENGTH  (WS-COMM-LENGTH)
      *    end-exec.
           MOVE 'NSRASBL' TO DFHEIV1
      *    MOVE '."C                   (   #00001383' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 srch-commarea, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF BL-OK
              perform varying bl-length from +6200 by -1 until
                 (bl-record-passed-data (bl-length:1) not = ' ')
                 or (bl-length = +1)
              end-perform
PEMTST*       DISPLAY ' BL OK ' BL-ARCHIVE-NO
              move bl-length to sl-length
              move spaces to naper-output-data
              MOVE BL-RECORD-PASSED-DATA (1:bl-length)
                                       TO NAPER-OUTPUT-DATA
              MOVE BL-ARCHIVE-NO          TO OT-ARCHIVE-NO
           ELSE
              MOVE BL-MESSAGE          TO ASERR-MESS
              
      * exec cics document create
      *          doctoken   (w-doctoken)
      *          template   ('ASERR')
      *          symbollist (WS-ASERR)
      *          resp       (WS-RESPONSE)
      *       end-exec
           MOVE LENGTH OF
            WS-ASERR
             TO DFHEIV16
           MOVE 'ASERR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001401' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031343031' TO DFHEIV0(25:11)
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
              
      * exec cics web send
      *          doctoken(w-doctoken)
      *                      resp   (WS-RESPONSE)
      *       end-exec
      *    MOVE 'X$D                   *  N#00001407' TO DFHEIV0
           MOVE X'582444202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202A20' TO DFHEIV0(13:12)
           MOVE X'204E233030303031343037' TO DFHEIV0(25:11)
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
              
      * exec cics
      *          return
      *       end-exec
      *    MOVE '.(                    ''   #00001411' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           evaluate ws-kixhost
              when 'slunikix'
                 evaluate ws-kix-myenv
                    when 'cid1p'
                       move '7001'     to ws-sl-port
      *                                   ws-sl-port2
                    when 'mdoff'
                       move '7003'     to ws-sl-port
      *                                   ws-sl-port2
                    when 'cid1t'
                       move '7002'     to ws-sl-port
      *                                   ws-sl-port2
                 end-evaluate
                 move ws-asftr         to ws-sl-key
      *                                   ws-sl-key2
              when 'logictest'
                 evaluate ws-kix-myenv
                    when 'cid1t'
                       move '6002'     to ws-lt-port
      *                                   ws-lt-port2
                    when 'tony'
                       move '6003'     to ws-lt-port
      *                                   ws-lt-port2
                    when 'paul'
                       move '5002'     to ws-lt-port
      *                                   ws-lt-port2
080612              when 'ahltst'
080612                 move '6007'     to ws-lt-port
080612*                                   ws-lt-port2
                 end-evaluate
                 move ws-asftr         to ws-lt-key
      *                                   ws-lt-key2
           end-evaluate
      ***********************************************************
      * Build output document.  There are three templates used
      * for this document.  ASHDR and ASFTR are the header
      * and footer, respectively.  ASBOD is used for the
      * actual data.  For each array entry in the business
      * logic output, set the symbol list from the array
      * entry and insert into the document using the ASBOD
      * template.
      ***********************************************************
      *    move bl-output-message to out-msg-text.
           evaluate true
              when iff-comp-id = 'DCC'
                 IF WS-KIX-MYENV = 'cid1p'
                    
      * exec cics document create
      *                doctoken   (w-doctoken)
      *                template   ('ASHDR')
      *                symbollist (WS-PROD-DCC-ASHDR)
      *                resp       (WS-RESPONSE)
      *             end-exec
           MOVE LENGTH OF
            WS-PROD-DCC-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001461' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031343631' TO DFHEIV0(25:11)
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
                 ELSE
                    
      * exec cics document create
      *                doctoken   (w-doctoken)
      *                template   ('ASHDR')
      *                symbollist (WS-TEST-DCC-ASHDR)
      *                resp       (WS-RESPONSE)
      *             end-exec
           MOVE LENGTH OF
            WS-TEST-DCC-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001468' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031343638' TO DFHEIV0(25:11)
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
                 END-IF
              when iff-comp-id = 'AHL'
080612           IF WS-KIX-MYENV = 'cid1p'
080612              
      * exec cics document create
080612*                doctoken   (w-doctoken)
080612*                template   ('ASHDR')
080612*                symbollist (WS-PROD-AHL-ASHDR)
080612*                resp       (WS-RESPONSE)
080612*             end-exec
           MOVE LENGTH OF
            WS-PROD-AHL-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001477' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031343737' TO DFHEIV0(25:11)
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
080612           ELSE
080612              
      * exec cics document create
080612*                doctoken   (w-doctoken)
080612*                template   ('ASHDR')
080612*                symbollist (WS-TEST-AHL-ASHDR)
080612*                resp       (WS-RESPONSE)
080612*             end-exec
           MOVE LENGTH OF
            WS-TEST-AHL-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001484' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031343834' TO DFHEIV0(25:11)
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
080612           END-IF
              when iff-comp-id = 'VPP'
020816           IF WS-KIX-MYENV = 'cid1p'
020816              
      * exec cics document create
020816*                doctoken   (w-doctoken)
020816*                template   ('ASHDR')
020816*                symbollist (WS-PROD-VPP-ASHDR)
020816*                resp       (WS-RESPONSE)
020816*             end-exec
           MOVE LENGTH OF
            WS-PROD-VPP-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001493' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031343933' TO DFHEIV0(25:11)
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
020816           ELSE
020816              
      * exec cics document create
020816*                doctoken   (w-doctoken)
020816*                template   ('ASHDR')
020816*                symbollist (WS-TEST-VPP-ASHDR)
020816*                resp       (WS-RESPONSE)
020816*             end-exec
           MOVE LENGTH OF
            WS-TEST-VPP-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001500' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031353030' TO DFHEIV0(25:11)
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
020816           END-IF
061421        when iff-comp-id = 'FNL'
061421           IF WS-KIX-MYENV = 'cid1p'
061421              
      * exec cics document create
061421*                doctoken   (w-doctoken)
061421*                template   ('ASHDR')
061421*                symbollist (WS-PROD-FNL-ASHDR)
061421*                resp       (WS-RESPONSE)
061421*             end-exec
           MOVE LENGTH OF
            WS-PROD-FNL-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001509' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031353039' TO DFHEIV0(25:11)
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
061421           ELSE
061421              
      * exec cics document create
061421*                doctoken   (w-doctoken)
061421*                template   ('ASHDR')
061421*                symbollist (WS-TEST-FNL-ASHDR)
061421*                resp       (WS-RESPONSE)
061421*             end-exec
           MOVE LENGTH OF
            WS-TEST-FNL-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001516' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031353136' TO DFHEIV0(25:11)
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
061421           END-IF
              when OTHER
                 IF WS-KIX-MYENV = 'cid1p'
                   
      * exec cics document create
      *               doctoken   (w-doctoken)
      *               template   ('ASHDR')
      *               symbollist (WS-PROD-CID-ASHDR)
      *               resp       (WS-RESPONSE)
      *            end-exec
           MOVE LENGTH OF
            WS-PROD-CID-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001525' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031353235' TO DFHEIV0(25:11)
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
                 ELSE
                   
      * exec cics document create
      *               doctoken   (w-doctoken)
      *               template   ('ASHDR')
      *               symbollist (WS-TEST-CID-ASHDR)
      *               resp       (WS-RESPONSE)
      *            end-exec
           MOVE LENGTH OF
            WS-TEST-CID-ASHDR
             TO DFHEIV16
           MOVE 'ASHDR' TO DFHEIV1
      *    MOVE '\"D tSL               )  N#00001532' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031353332' TO DFHEIV0(25:11)
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
                 END-IF
           end-evaluate
      *    DISPLAY ' ASHDR DOC CREATE ' WS-RESPONSE
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
      *    MOVE 'EL01'                 TO OUT-TEMP
      *    MOVE 'ALWA'                 TO OUT-PROC-ID
      *    MOVE 'EL01'                 TO OUT-LETTER
      *    MOVE '9'                    TO OUT-CARRIER
      *    MOVE 'MCDANIEL'             TO OUT-ILNAME
      *    MOVE 'KATHI'                TO OUT-IFNAME
      *
      *    MOVE '0009235906 '          TO OUT-CERT-NO
      *    MOVE '0990000208'           TO OUT-ACCOUNT
           
      * exec cics document set
      *       doctoken(w-doctoken)
      *       symbollist(NAPER-OUTPUT-DATA (1:bl-length))
061410*       delimiter ('?')
      *       length(length of NAPER-OUTPUT-DATA)
      *       length(sl-length)
      *                   resp   (WS-RESPONSE)
061410*     unescaped
      *    end-exec
      *    MOVE '\(Ds L                ''  N#00001563' TO DFHEIV0
           MOVE X'5C284473204C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303031353633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 NAPER-OUTPUT-DATA(1 : bl-length), 
                 DFHEIV99, 
                 sl-length, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
      *    DISPLAY ' DOC SET    ' WS-RESPONSE
           
      * exec cics document insert
      *       doctoken(w-doctoken)
      *       template('ASBOD')
      *                   resp   (WS-RESPONSE)
      *    end-exec
           MOVE 'ASBOD' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001573' TO DFHEIV0
           MOVE X'5C2444742020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031353733' TO DFHEIV0(25:11)
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
           IF BL-LETTER-TO-ACCT NOT = SPACES
              MOVE BL-LETTER-TO-ACCT   TO NAPER-OUTPUT-DATA (12:1)
      *       MOVE 'EL01T'             TO NAPER-OUTPUT-DATA (8:6)
              
      * exec cics document set
      *          doctoken(w-doctoken)
      *          symbollist(NAPER-OUTPUT-DATA (1:bl-length))
      *          length(sl-length)
      *          resp   (WS-RESPONSE)
      *       end-exec
      *    MOVE '\(Ds L                ''  N#00001581' TO DFHEIV0
           MOVE X'5C284473204C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303031353831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 NAPER-OUTPUT-DATA(1 : bl-length), 
                 DFHEIV99, 
                 sl-length, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              
      * exec cics document insert
      *          doctoken(w-doctoken)
      *          template('ASBOD')
      *          resp   (WS-RESPONSE)
      *       end-exec
           MOVE 'ASBOD' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001587' TO DFHEIV0
           MOVE X'5C2444742020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031353837' TO DFHEIV0(25:11)
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
           END-IF
           IF BL-LETTER-TO-BENE NOT = SPACES
              MOVE BL-LETTER-TO-BENE   TO NAPER-OUTPUT-DATA (12:1)
      *       MOVE 'EL01T'             TO NAPER-OUTPUT-DATA (8:6)
              
      * exec cics document set
      *          doctoken(w-doctoken)
      *          symbollist(NAPER-OUTPUT-DATA (1:bl-length))
      *          length(sl-length)
      *          resp   (WS-RESPONSE)
      *       end-exec
      *    MOVE '\(Ds L                ''  N#00001596' TO DFHEIV0
           MOVE X'5C284473204C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303031353936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 NAPER-OUTPUT-DATA(1 : bl-length), 
                 DFHEIV99, 
                 sl-length, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              
      * exec cics document insert
      *          doctoken(w-doctoken)
      *          template('ASBOD')
      *          resp   (WS-RESPONSE)
      *       end-exec
           MOVE 'ASBOD' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001602' TO DFHEIV0
           MOVE X'5C2444742020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363032' TO DFHEIV0(25:11)
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
           END-IF
      *    DISPLAY ' ASBOD DOC INSERT ' WS-RESPONSE
      *
      *        add 1 to bl-index
      *
      *    end-perform.
      *    MOVE BL-CARRIER  TO OT-CARRIER
      *    MOVE BL-CLAIM-NO TO OT-CLMNO
      *    MOVE BL-CERT-NO  TO OT-CRTNO
      *    MOVE BL-ARCHIVE-NO TO OT-ARCHNO
           IF WS-KIXHOST = 'slunikix'
              display ' host slunikix ' ws-kixhost
              
      * exec cics document set
      *          doctoken(w-doctoken)
      *          symbollist(WS-VAR-SLUNIKIX)
      *          length(length of WS-VAR-SLUNIKIX)
      *                      resp   (WS-RESPONSE)
      *       end-exec
           MOVE LENGTH OF
            WS-VAR-SLUNIKIX TO DFHEIV16
      *    MOVE '\(Ds L                ''  N#00001619' TO DFHEIV0
           MOVE X'5C284473204C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 WS-VAR-SLUNIKIX, 
                 DFHEIV99, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           else
              display ' host logictest ' ws-kixhost
              
      * exec cics document set
      *          doctoken(w-doctoken)
      *          symbollist(WS-VAR-LOGICTEST)
      *          length(length of WS-VAR-LOGICTEST)
      *                      resp   (WS-RESPONSE)
      *       end-exec
           MOVE LENGTH OF
            WS-VAR-LOGICTEST TO DFHEIV16
      *    MOVE '\(Ds L                ''  N#00001627' TO DFHEIV0
           MOVE X'5C284473204C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 WS-VAR-LOGICTEST, 
                 DFHEIV99, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
      *    DISPLAY ' DOC SET ' WS-RESPONSE
           
      * exec cics document insert
      *       doctoken(w-doctoken)
      *       template('ASFTR')
      *                   resp   (WS-RESPONSE)
      *    end-exec
           MOVE 'ASFTR' TO DFHEIV1
      *    MOVE '\$Dt                  (  N#00001635' TO DFHEIV0
           MOVE X'5C2444742020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363335' TO DFHEIV0(25:11)
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
      *    DISPLAY ' ASFTR DOC INSERT ' WS-RESPONSE
      *    if bl-fail
      *       exec cics syncpoint rollback
      *       end-exec
      *    end-if.
      ****************************************
      * Send the document and return.
      ****************************************
           
      * exec cics web send
      *       doctoken(w-doctoken)
      *                   resp   (WS-RESPONSE)
      *    end-exec.
      *    MOVE 'X$D                   *  N#00001648' TO DFHEIV0
           MOVE X'582444202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202A20' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363438' TO DFHEIV0(25:11)
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
      *    MOVE '.(                    ''   #00001654' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363534' TO DFHEIV0(25:11)
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
      *    MOVE 'X*FLVL                &  N#00001667' TO DFHEIV0
           MOVE X'582A464C564C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363637' TO DFHEIV0(25:11)
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
                    when 'data_src'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-DATA-SRCE
                       else
                                      move spaces to IFF-DATA-SRCE
                       end-if
                    when 'letter_id'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-LETTER-ID
                       else
                                      move spaces to IFF-LETTER-ID
                       end-if
                    when 'carrier'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                   to IFF-CARRIER
                       else
                                      move spaces to IFF-CARRIER
                       end-if
                    when 'grouping'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                                         to IFF-GROUP
                       else
                                      move spaces  to IFF-GROUP
                       end-if
                    when 'state_cd'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-STATE
                       else
                                      move spaces  to IFF-STATE
                       end-if
                    when 'acct_no'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-ACCOUNT
                       else
                          move spaces  to IFF-ACCOUNT
                       end-if
                    when 'eff_date'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-EFF-DT
                       else
                                      move spaces  to IFF-EFF-DT
                       end-if
                    when 'cert_no'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-CERT-NO
                       else
                                      move spaces  to IFF-CERT-NO
                       end-if
                    when 'batch_no'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-BATCH-NO
                       else
061421                  move zeros   to IFF-BATCH-NO
                       end-if
                    when 'batch_seq'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-BATCH-SEQ
                       else
                                      move zeros   to IFF-BATCH-SEQ
                       end-if
                    when 'resp_no'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-RESP-NO
                       else
                                      move spaces  to IFF-RESP-NO
                       end-if
                    when 'no_copies'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-NO-OF-COPIES
                       else
                          move zeros   to IFF-NO-OF-COPIES
                       end-if
                    when 'proc_id'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-PROC-ID
                       else
                          move 'JJVA'  to IFF-PROC-ID
                       end-if
                    when 'comp_id'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-COMP-ID
                       else
                          move 'CID'   to IFF-COMP-ID
                       end-if
                    when 'prt_now'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-PRINT-NOW-SW
                       else
                          move 'N'     to IFF-PRINT-NOW-SW
                       end-if
                    when 'enc_cd'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-ENC-CD
                       else
                          move '0'     to IFF-ENC-CD
                       end-if
                    when 'resend_dt'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-RESEND-DT
                       else
                          move SPACES  to IFF-RESEND-DT
                       end-if
                    when 'follow_dt'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-FOLLOW-UP-DT
                       else
                          move SPACES  to IFF-FOLLOW-UP-DT
                       end-if
                    when 'arch_no'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-ARCHIVE-NO
                       else
                          move ZEROS   to IFF-ARCHIVE-NO
                       end-if
                    when 'pgm_func'
                                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-FUNC
                       else
                          move ZEROS   to IFF-FUNC
                       end-if
101812*             when 'ltr_comments'
101812*                if w-form-value-len not = 0
101812*                   move w-form-value(1:w-form-value-len)
101812*                                to IFF-COMMENTS
101812*                else
101812*                   move SPACES  to IFF-COMMENTS
101812*                end-if
101812              when 'end_arch'
101812                 if w-form-value-len not = 0
101812                    move w-form-value(1:w-form-value-len)
101812                                 to IFF-ENDT-ARCH-NO
101812                 else
101812                    move ZEROS  to IFF-ENDT-ARCH-NO
101812                 end-if
                 end-evaluate
              when other
                 continue
           end-evaluate.
       read-form-exit.
       9700-DATE-LINK.
           
      * EXEC CICS LINK
      *        PROGRAM   ('ELDATCV')
      *        COMMAREA  (DATE-CONVERSION-DATA)
      *        LENGTH    (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00001837' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9700-EXIT.
            EXIT.

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
