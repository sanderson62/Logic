      *((program: EL691.cl2))
000001*>>SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
000002 IDENTIFICATION DIVISION.
000003
000004 PROGRAM-ID. EL691.
000005*
000006*AUTHOR.    LOGIC, INC.
000007*           DALLAS, TEXAS.
000008
000009*DATE-COMPILED.
000010
000011*REMARKS.
000012*        THIS PROGRAM PROVIDES THE FUNCTIONS TO BROWSE AND EDIT
000013*    THE DETAIL OF AN ARCHIVED LETTER.
000014
000015*    TRANS ID = EXN1
000016
000017******************************************************************
000018*                   C H A N G E   L O G
000019*
000020* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000021*-----------------------------------------------------------------
000022*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000023* EFFECTIVE    NUMBER
000024*-----------------------------------------------------------------
000025* 071111    2011022800001  AJRA  NAPERSOFT - NEW SCREEN
000026* 100312    2011022800001  AJRA  CHANGE RECEIVED MSG
000027* 121112    2012101700002  AJRA  ADD PF6 TO CERT NOTE, ENDT ARCH N
000028* 122612    2012101700002  AJRA  UPDATE BILLING NOTE ON RECEIVED
000029* 100813    2013100700002  AJRA  VERITY RESEND LETTER ID ON CHANGE
000030* 102918  CR2018080300002  PEMA  ADD VOID OB LETTER OPTION
000031* 041320  CR2020030500002  PEMA  Issue, cancel billing notes
000032* 102020 IR2020101300001   PEMA  Correct billing notes
000033* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
000034******************************************************************
000035
000036 DATA DIVISION.
000037 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000038 77  FILLER  PIC X(32) VALUE '********************************'.
000039 77  FILLER  PIC X(32) VALUE '*    EL691 WORKING STORAGE     *'.
000040 77  FILLER  PIC X(32) VALUE '*********** VMOD=2.037 *********'.
000041 77  c1                          pic s9(5) comp-3 value +0.
000042 77  n1                          pic s999  comp-3 value +0.
000043 77  w-comment-line-cnt          pic s999 comp-3 value +0.
000044 77  ws-cert-note-generic-key-len pic s9(4) comp value +34.
000045 77  note-count                  pic s999 comp-3 value +0.
000046 77  ws-build-note-sw            pic x value ' '.
000047     88  finished-with-notes      value 'Y'.
000048 77  ws-ercnot-sw                pic x  value spaces.
000049     88  ercnot-startbr            value 'Y'.
000050
000051 01  P pointer.
000052 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000053 01  var-ptr pointer.
000054 01  env-var-len                 pic 9(4)  binary.
000055 01  rc                          pic 9(9)  binary.
000056
000057 01  WS-KIXSYS.
000058     05  WS-KIX-FIL1             PIC X(10).
000059     05  WS-KIX-APPS             PIC X(10).
000060     05  WS-KIX-ENV              PIC X(10).
000061     05  WS-KIX-MYENV            PIC X(10).
000062     05  WS-KIX-SYS              PIC X(10).
000063
000066 EXEC SQL
          INCLUDE SQLDA
       END-EXEC
000067
000070*EXEC SQL
      *   INCLUDE SQLCA
      *END-EXEC
      *>>((file: SQLCA))
000001****************************************************************<*
000002* Copyright (c) 2016-2021 NTT DATA, Inc. All rights reserved.  *<*
000003* Users of NTT DATA Enterprise COBOL may freely                *<*
000004* redistribute this copybook.                                  *<*
000005****************************************************************<*
000006
000007 01  SQLCA GLOBAL.
000008     05  SQLCAID                PIC X(8).
000009     05  SQLCABC                PIC S9(9) COMP-5.
000010     05  SQLCODE                PIC S9(9) COMP-5.
000011     05  SQLERRM.
000012         49  SQLERRML           PIC S9(4) COMP-5.
000013         49  SQLERRMC           PIC X(254).
000014     05  SQLERRP                PIC X(8).
000015     05  SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5.
000016     05  SQLWARN.
000017         10 SQLWARN0            PIC X(1).
000018         10 SQLWARN1            PIC X(1).
000019         10 SQLWARN2            PIC X(1).
000020         10 SQLWARN3            PIC X(1).
000021         10 SQLWARN4            PIC X(1).
000022         10 SQLWARN5            PIC X(1).
000023         10 SQLWARN6            PIC X(1).
000024         10 SQLWARN7            PIC X(1).
000025     05  SQLSTATE               PIC X(5).
000026     05  SQLEXT                 PIC S9(5) COMP-3 VALUE 1.
      *<<((file: SQLCA))
000071
000073 EXEC SQL
          BEGIN DECLARE SECTION
000074 END-EXEC
000075
000076 01  sqlcmd                      pic x(1024).
000077 01  WS-MOE-DATE                 pic x(10).
000078 01  svr                         pic x(32).
000079 01  usr                         pic x(32).
000080 01  pass                        pic x(32).
000081 01  usr-pass                    pic x(64).
000082 01  ws-sql-code                 pic s9(7) value zeros.
000083 01  ws-dis-sql-code             pic -9999999 value zeros.
000084
000085***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000086***                                                            ***
000087***  These indicators are used to determine if a variable      ***
000088***  is passed nulls from sql. The indicator will be -1        ***
000089***  if the value on sql is nulls and +0 if the value is       ***
000090***  something other than nulls. Here is an example on how     ***
000091***  to use the indicator variables.                           ***
000092***                                                            ***
000093***     EXEC SQL                                               ***
000094***        fetch keyword into                                  ***
000095***           :db-Complete-dt :nu-Complete-date,               ***
000096***           :db-app-by     :nu-app-by,                       ***
000097***           :db-app-date   :nu-app-date,                     ***
000098***           :db-app-batch  :nu-app-batch                     ***
000099***     END-EXEC                                               ***
000100***                                                            ***
000101***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000102
000103 01  indicator-vaiables-for-nulls.
000104     05  nu-Complete-date        pic s9(4) comp value +0.
000105
000106 01  key-word-key-data.
000107     05  kd-unique-id            pic 9(7).
000108     05  kd-cert-no              pic x(11).
000109
000110 01  key-word-table-data.
000111     05  db-req-date             pic x(10).
000112     05  db-doc-type             pic x(30).
000113     05  db-Cert-no              pic x(11).
000114     05  db-Print-dt             pic x(10).
000115     05  db-Cert-exp-dt          pic x(10).
000116     05  db-cert-state           pic xx.
000117     05  db-key-word-type        pic x(30).
000118     05  db-key-word-value       pic x(30).
000119     05  db-complete-dt          pic x(10).
000120
000122 EXEC SQL
          END DECLARE SECTION
000123 END-EXEC
000124
000125
000126 01  W-WORK-AREAS.
000127     12  ws-sql-update-sw        pic x value ' '.
000128         88  sql-update-succeeded    value 'Y'.
000129         88  sql-update-failed       value 'N'.
000130     12  ws-connect-sw           pic x  value ' '.
000131         88  connected-to-db        value 'Y'.
000132     12  ws-cert-sw              pic x value ' '.
000133         88  cert-found             value 'Y'.
000134     12  FILLER                  PIC  X(18)
000135                                      VALUE 'PROGRAM WORK AREA:'.
000136     12  W-LAST-ERROR            PIC  9(04) VALUE 9999.
000137     12  W-CALL-PGM              PIC  X(08).
000138     12  W-CURRENT-SAVE          PIC  X(02) VALUE SPACES.
000139     12  W-SAVE-BIN-DATE         PIC  X(02) VALUE SPACES.
000140     12  W-SAVE-DATE             PIC  X(08) VALUE SPACES.
000141
000142     12  W-TIME-IN               PIC S9(07).
000143     12  FILLER REDEFINES W-TIME-IN.
000144         16  FILLER              PIC  X(01).
000145         16  W-TIME-OUT          PIC  9(02)V9(02).
000146         16  FILLER              PIC  X(02).
000147
000148     12  W-DEEDIT-FIELD          PIC  X(15).
000149     12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD PIC S9(15).
000150
000151     12  W-ERROR-COUNT           PIC S9(3)       VALUE ZERO.
000152     12  W-UPDATE-SW             PIC S9          VALUE ZERO.
000153     12  W-RESEND-DATE           PIC XX    VALUE LOW-VALUES.
000154     12  W-SENT-DATE             PIC XX    VALUE LOW-VALUES.
000155     12  W-FINAL-ACT-DATE        PIC XX    VALUE LOW-VALUES.
000156     12  W-RECEIVED-DATE         PIC XX    VALUE LOW-VALUES.
000157     12  W-STOP-LETTER-DATE      PIC XX    VALUE LOW-VALUES.
000158     12  W-RESPONSE              PIC S9(8)   COMP.
000159         88  RESP-NORMAL                  VALUE +00.
000160         88  RESP-NOTFND                  VALUE +13.
000161         88  RESP-DUPREC                  VALUE +14.
000162         88  RESP-DUPKEY                  VALUE +15.
000163         88  RESP-NOTOPEN                 VALUE +19.
000164         88  RESP-ENDFILE                 VALUE +20.
000165         88  resp-lengtherr               value +22.
000166
000167     12  W-ARCH-SAVE-KEY         PIC  X(03).
000168     12  W-ARCH-KEY.
000169         16  W-ARCH-COMPANY-CD   PIC  X(01).
000170         16  W-ARCH-NUMBER       PIC S9(08)      COMP.
000171
000172     12  W-ARCT-KEY.
000173         16  W-ARCT-COMPANY-CD   PIC  X(01).
000174         16  W-ARCT-ARCHIVE-NO   PIC S9(08) COMP.
000175         16  W-ARCT-RECORD-TYPE  PIC  X(01).
000176             88  W-ARCT-COMMENT-DATA   VALUE '3'.
000177         16  W-ARCT-LINE-SEQ-NO  PIC S9(04) COMP.
000178
000179     12  w-stop-letter-comment   pic x(126) value spaces.
000180     12  w-cert-note-comment     pic x(70) value spaces.
000181     12  W-CERT-NOTE-MSG.
000182         16  FILLER              PIC X(29)
000183             VALUE 'REQUESTED DOCUMENT RECEIVED  '.
000184         16  W-CERT-NOTE-RECV-DT PIC X(8).
000185
000186     12  WS-FIND-BILLING-NOTE.
000187         16  WS-FBN-NOTE         PIC X(25).
000188         16  WS-FBN-LTRID        PIC X(4).
000189
000190     12  WS-RECEIVED-NOTE.
000191         16  FILLER              PIC X(12)
000192             VALUE ' - RECEIVED '.
000193         16  WS-BN-RECV-DATE     PIC X(8).
000194
000195     12  WS-Z-RECORD-IND         PIC X(1) VALUE 'N'.
000196         88 Z-RECORD-FOUND                VALUE 'Y'.
000197         88 Z-RECORD-NOT-FOUND            VALUE 'N'.
000198
000199     12  ELCERT-FILE-ID          PIC  X(08)  VALUE 'ELCERT'.
000200     12  ERCNOT-FILE-ID          PIC  X(08)  VALUE 'ERCNOT'.
000201     12  ERCNOT-LENGTH           PIC S9(4)   COMP VALUE +150.
000202     12  ERCNOT-KEY-LENGTH       PIC S9(4)   COMP VALUE +36.
000203     12  ERCNOT-START-LENGTH     PIC S9(4)   COMP VALUE +34.
000204     12  ERCNOT-KEY.
000205         16  ERCNOT-PARTIAL-KEY.
000206             20 ERCNOT-COMPANY-CD    PIC X.
000207             20 ERCNOT-CARRIER       PIC X.
000208             20 ERCNOT-GROUPING      PIC X(06).
000209             20 ERCNOT-STATE         PIC XX.
000210             20 ERCNOT-ACCOUNT       PIC X(10).
000211             20 ERCNOT-EFF-DT        PIC XX.
000212             20 ERCNOT-CERT-NO.
000213                25 ERCNOT-CERT-PRIME PIC X(10).
000214                25 ERCNOT-CERT-SFX   PIC X.
000215             20 ERCNOT-REC-TYP       PIC X.
000216         16 ERCNOT-SEQ           PIC S9(4) COMP.
000217     12  SV-PRIOR-KEY.
000218         16 SV-COMPANY-CD            PIC X.
000219         16 SV-CARRIER               PIC X.
000220         16 SV-GROUPING              PIC X(06).
000221         16 SV-STATE                 PIC XX.
000222         16 SV-ACCOUNT               PIC X(10).
000223         16 SV-EFF-DT                PIC XX.
000224         16 SV-CERT-NO.
000225            20 SV-CERT-PRIME         PIC X(10).
000226            20 SV-CERT-SFX           PIC X(1).
000227         16 SV-REC-TYP               PIC X.
000228         16  SV-NOTE-SEQUENCE        PIC S9(4) COMP.
000229     12  ELCERT-KEY.
000230         16  ELCERT-COMPANY-CD        PIC X.
000231         16  ELCERT-CARRIER           PIC X.
000232         16  ELCERT-GROUPING          PIC X(6).
000233         16  ELCERT-STATE             PIC XX.
000234         16  ELCERT-ACCOUNT           PIC X(10).
000235         16  ELCERT-EFF-DT            PIC XX.
000236         16  ELCERT-CERT-NO.
000237             20  ELCERT-CERT-PRIME    PIC X(10).
000238             20  ELCERT-CERT-SFX      PIC X.
000239     12  ELEOBC-FILE-ID          PIC X(08)  VALUE 'ELEOBC'.
000240     12  ELEOBC-LENGTH           PIC S9(04) VALUE +350 COMP.
000241     12  ELEOBC-KEY.
000242         16  EOBC-COMPANY-CD     PIC X.
000243         16  EOBC-REC-TYPE       PIC X.
000244         16  EOBC-CODE           PIC X(4).
000245         16  FILLER              PIC X(9).
000246     12  ERNOTE-FILE-ID          PIC X(08)  VALUE 'ERNOTE'.
000247     12  ERNOTE-LENGTH           PIC S9(04) VALUE +825 COMP.
000248     12  ERNOTE-KEY.
000249         16  ERNOTE-COMPANY-CD   PIC X.
000250         16  ERNOTE-CARRIER      PIC X.
000251         16  ERNOTE-GROUPING     PIC X(6).
000252         16  ERNOTE-STATE        PIC XX.
000253         16  ERNOTE-ACCOUNT      PIC X(10).
000254         16  ERNOTE-CERT-EFF-DT  PIC XX.
000255         16  ERNOTE-CERT-PRIME   PIC X(10).
000256         16  ERNOTE-CERT-SFX     PIC X.
000257         16  ernote-record-type  pic x.
000258     12  NOTE-SUB PIC S9(5) COMP-3 VALUE +0.
000259     12  ELLETR-KEY.
000260         16  LETR-PART-KEY.
000261             20  LETR-COMPANY-CD PIC X.
000262             20  LETR-LETTER-ID  PIC X(4).
000263         16  LETR-FILLER         PIC X(8).
000264         16  LETR-SEQ-NO         PIC 9(4) BINARY.
000265     12  ELLETR-SAVE-PART-KEY    PIC X(5).
000266
000267 01  w-comment-line-1            pic x(63) value spaces.
000268 01  w-comment-line-2            pic x(63) value spaces.
000269 01  cert-note-records-holder.
000270     05  cert-note-record occurs 500.
000271         10  filler              pic x(48).
000272         10  cnr-rest            pic x(102).
000273
000274
000275
000276 01  FILLER                      PIC  X(22)
000277                                 VALUE 'INTERFACE AREA STARTS:'.
000278*    COPY ELCINTF.
      *>>((file: ELCINTF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCINTF.                            *
000005*                            VMOD=2.017                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
000008*                                                                *
000009*       LENGTH = 1024                                            *
000010*                                                                *
000011******************************************************************
000012*                   C H A N G E   L O G
000013*
000014* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000015*-----------------------------------------------------------------
000016*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000017* EFFECTIVE    NUMBER
000018*-----------------------------------------------------------------
000019* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
000020******************************************************************
000021 01  PROGRAM-INTERFACE-BLOCK.
000022     12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
000023     12  PI-CALLING-PROGRAM              PIC X(8).
000024     12  PI-SAVED-PROGRAM-1              PIC X(8).
000025     12  PI-SAVED-PROGRAM-2              PIC X(8).
000026     12  PI-SAVED-PROGRAM-3              PIC X(8).
000027     12  PI-SAVED-PROGRAM-4              PIC X(8).
000028     12  PI-SAVED-PROGRAM-5              PIC X(8).
000029     12  PI-SAVED-PROGRAM-6              PIC X(8).
000030     12  PI-RETURN-TO-PROGRAM            PIC X(8).
000031     12  PI-COMPANY-ID                   PIC XXX.
000032     12  PI-COMPANY-CD                   PIC X.
000033
000034     12  PI-COMPANY-PASSWORD             PIC X(8).
000035
000036     12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
000037
000038     12  PI-CONTROL-IN-PROGRESS.
000039         16  PI-CARRIER                  PIC X.
000040         16  PI-GROUPING                 PIC X(6).
000041         16  PI-STATE                    PIC XX.
000042         16  PI-ACCOUNT                  PIC X(10).
000043         16  PI-PRODUCER REDEFINES PI-ACCOUNT
000044                                         PIC X(10).
000045         16  PI-CLAIM-CERT-GRP.
000046             20  PI-CLAIM-NO             PIC X(7).
000047             20  PI-CERT-NO.
000048                 25  PI-CERT-PRIME       PIC X(10).
000049                 25  PI-CERT-SFX         PIC X.
000050             20  PI-CERT-EFF-DT          PIC XX.
000051         16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
000052             20  PI-PLAN-CODE            PIC X(2).
000053             20  PI-REVISION-NUMBER      PIC X(3).
000054             20  PI-PLAN-EFF-DT          PIC X(2).
000055             20  PI-PLAN-EXP-DT          PIC X(2).
000056             20  FILLER                  PIC X(11).
000057         16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
000058             20  PI-OE-REFERENCE-1.
000059                 25  PI-OE-REF-1-PRIME   PIC X(18).
000060                 25  PI-OE-REF-1-SUFF    PIC XX.
000061
000062     12  PI-SESSION-IN-PROGRESS          PIC X.
000063         88  CLAIM-SESSION                   VALUE '1'.
000064         88  CREDIT-SESSION                  VALUE '2'.
000065         88  WARRANTY-SESSION                VALUE '3'.
000066         88  MORTGAGE-SESSION                VALUE '4'.
000067         88  GENERAL-LEDGER-SESSION          VALUE '5'.
000068
000069
000070*THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
000071
000072     12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
000073     12  PI-ORIGINAL-COMPANY-CD          PIC X.
000074
000075     12  PI-CREDIT-USER                  PIC X.
000076         88  PI-NOT-CREDIT-USER              VALUE 'N'.
000077         88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
000078
000079     12  PI-CLAIM-USER                   PIC X.
000080         88  PI-NOT-CLAIM-USER               VALUE 'N'.
000081         88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
000082
000083     12  PI-PROCESSOR-SYS-ACCESS         PIC X.
000084         88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
000085         88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
000086         88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
000087         88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
000088         88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
000089
000090     12  PI-PROCESSOR-ID                 PIC X(4).
000091
000092     12  PI-PROCESSOR-PASSWORD           PIC X(11).
000093
000094     12  PI-MEMBER-CAPTION               PIC X(10).
000095
000096     12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
000097         88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
000098
000099     12  PI-LIFE-OVERRIDE-L1             PIC X.
000100     12  PI-LIFE-OVERRIDE-L2             PIC XX.
000101     12  PI-LIFE-OVERRIDE-L6             PIC X(6).
000102     12  PI-LIFE-OVERRIDE-L12            PIC X(12).
000103
000104     12  PI-AH-OVERRIDE-L1               PIC X.
000105     12  PI-AH-OVERRIDE-L2               PIC XX.
000106     12  PI-AH-OVERRIDE-L6               PIC X(6).
000107     12  PI-AH-OVERRIDE-L12              PIC X(12).
000108
000109     12  PI-NEW-SYSTEM                   PIC X(2).
000110
000111     12  PI-PRIMARY-CERT-NO              PIC X(11).
000112     12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
000113         88  PI-USES-PAID-TO                 VALUE '1'.
000114     12  PI-CRDTCRD-SYSTEM.
000115         16  PI-CRDTCRD-USER             PIC X.
000116             88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
000117             88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
000118         16  PI-CC-MONTH-END-DT          PIC XX.
000119     12  PI-PROCESSOR-PRINTER            PIC X(4).
000120
000121     12  PI-OE-REFERENCE-2.
000122         16  PI-OE-REF-2-PRIME           PIC X(10).
000123         16  PI-OE-REF-2-SUFF            PIC X.
000124
000125     12  PI-REM-TRM-CALC-OPTION          PIC X.
000126
000127     12  PI-LANGUAGE-TYPE                PIC X.
000128             88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
000129             88  PI-LANGUAGE-IS-FR           VALUE 'F'.
000130             88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
000131
000132     12  PI-POLICY-LINKAGE-IND           PIC X.
000133         88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
000134         88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
000135                                                   LOW-VALUES.
000136
000137     12  PI-ALT-DMD-PRT-ID               PIC X(4).
000138     12  PI-CLAIM-PW-SESSION             PIC X(1).
000139         88  PI-CLAIM-CREDIT                 VALUE '1'.
000140         88  PI-CLAIM-CONVEN                 VALUE '2'.
000141
000142     12  PI-PROCESSOR-CSR-IND            PIC X.
000143         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
000144         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
000145
000146     12  FILLER                          PIC X(3).
000147
000148     12  PI-SYSTEM-LEVEL                 PIC X(145).
000149
000150     12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
000151         PI-SYSTEM-LEVEL.
000152
000153         16  PI-ENTRY-CODES.
000154             20  PI-ENTRY-CD-1           PIC X.
000155             20  PI-ENTRY-CD-2           PIC X.
000156
000157         16  PI-RETURN-CODES.
000158             20  PI-RETURN-CD-1          PIC X.
000159             20  PI-RETURN-CD-2          PIC X.
000160
000161         16  PI-UPDATE-STATUS-SAVE.
000162             20  PI-UPDATE-BY            PIC X(4).
000163             20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
000164
000165         16  PI-LOWER-CASE-LETTERS       PIC X.
000166             88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
000167
000168*        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
000169*            88  CLAIM-NO-UNIQUE             VALUE '1'.
000170*            88  CARRIER-CLM-CNTL            VALUE '2'.
000171
000172         16  PI-CERT-ACCESS-CONTROL      PIC X.
000173             88  ST-ACCNT-CNTL               VALUE ' '.
000174             88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
000175             88  CARR-ST-ACCNT-CNTL          VALUE '2'.
000176             88  ACCNT-CNTL                  VALUE '3'.
000177             88  CARR-ACCNT-CNTL             VALUE '4'.
000178
000179         16  PI-PROCESSOR-CAP-LIST.
000180             20  PI-SYSTEM-CONTROLS.
000181                24 PI-SYSTEM-DISPLAY     PIC X.
000182                 88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
000183                24 PI-SYSTEM-MODIFY      PIC X.
000184                 88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
000185             20  FILLER                  PIC XX.
000186             20  PI-DISPLAY-CAP          PIC X.
000187                 88  DISPLAY-CAP             VALUE 'Y'.
000188             20  PI-MODIFY-CAP           PIC X.
000189                 88  MODIFY-CAP              VALUE 'Y'.
000190             20  PI-MSG-AT-LOGON-CAP     PIC X.
000191                 88  MSG-AT-LOGON-CAP        VALUE 'Y'.
000192             20  PI-FORCE-CAP            PIC X.
000193                 88  FORCE-CAP               VALUE 'Y'.
000194
000195         16  PI-PROGRAM-CONTROLS.
000196             20  PI-PGM-PRINT-OPT        PIC X.
000197             20  PI-PGM-FORMAT-OPT       PIC X.
000198             20  PI-PGM-PROCESS-OPT      PIC X.
000199             20  PI-PGM-TOTALS-OPT       PIC X.
000200
000201         16  PI-HELP-INTERFACE.
000202             20  PI-LAST-ERROR-NO        PIC X(4).
000203             20  PI-CURRENT-SCREEN-NO    PIC X(4).
000204
000205         16  PI-CARRIER-CONTROL-LEVEL    PIC X.
000206             88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
000207
000208         16  PI-CR-CONTROL-IN-PROGRESS.
000209             20  PI-CR-CARRIER           PIC X.
000210             20  PI-CR-GROUPING          PIC X(6).
000211             20  PI-CR-STATE             PIC XX.
000212             20  PI-CR-ACCOUNT           PIC X(10).
000213             20  PI-CR-FIN-RESP          PIC X(10).
000214             20  PI-CR-TYPE              PIC X.
000215
000216         16  PI-CR-BATCH-NUMBER          PIC X(6).
000217
000218         16  PI-CR-MONTH-END-DT          PIC XX.
000219
000220         16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
000221             88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
000222             88  PI-ZERO-CARRIER             VALUE '1'.
000223             88  PI-ZERO-GROUPING            VALUE '2'.
000224             88  PI-ZERO-CAR-GROUP           VALUE '3'.
000225
000226         16  PI-CARRIER-SECURITY         PIC X.
000227             88  PI-NO-CARRIER-SECURITY      VALUE ' '.
000228
000229         16  PI-ACCOUNT-SECURITY         PIC X(10).
000230             88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
000231             88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
000232
000233         16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
000234             20  PI-ACCESS-CODE          OCCURS 10 TIMES
000235                                         INDEXED BY PI-ACCESS-NDX
000236                                         PIC X.
000237
000238         16  PI-GA-BILLING-CONTROL       PIC X.
000239             88  PI-GA-BILLING               VALUE '1'.
000240
000241         16  PI-MAIL-PROCESSING          PIC X.
000242             88  PI-MAIL-YES                 VALUE 'Y'.
000243
000244         16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
000245
000246         16  PI-AR-SYSTEM.
000247             20  PI-AR-PROCESSING-CNTL   PIC X.
000248                 88  PI-AR-PROCESSING        VALUE 'Y'.
000249             20  PI-AR-SUMMARY-CODE      PIC X(6).
000250             20  PI-AR-MONTH-END-DT      PIC XX.
000251
000252         16  PI-MP-SYSTEM.
000253             20  PI-MORTGAGE-USER            PIC X.
000254                 88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
000255                 88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
000256             20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
000257                 88  PI-MP-ST-PROD-CNTL              VALUE ' '.
000258                 88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
000259                 88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
000260                 88  PI-MP-PROD-CNTL                 VALUE '3'.
000261                 88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
000262             20  PI-MP-MONTH-END-DT          PIC XX.
000263             20  PI-MP-REFERENCE-NO.
000264                 24  PI-MP-REFERENCE-PRIME   PIC X(18).
000265                 24  PI-MP-REFERENCE-SFX     PIC XX.
000266
000267         16  PI-LABEL-CONTROL            PIC X(01).
000268             88  PI-CREATE-LABELS                    VALUE 'Y'.
000269             88  PI-BYPASS-LABELS                    VALUE 'N'.
000270
000271         16  PI-BILL-GROUPING-CODE       PIC X(01).
000272             88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
000273
000274         16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
000275             88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
000276             88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
000277
000278         16  FILLER                      PIC X(14).
000279
000280     12  PI-PROGRAM-WORK-AREA            PIC X(640).
000281******************************************************************
      *<<((file: ELCINTF))
000279     12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
000280*    COPY ELC1042.
      *>>((file: ELC1042))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                           ELC1042                              *
000005*                            VMOD=2.002                          *
000006*                                                                *
000007*    NOTE                                                        *
000008*        THE WORK AREA IS USED BY EL152, EL1522, EL1042, EL153,  *
000009*        EM152, EM1522, EL689, EL6892, EL6311, AND EL690.        *
000010*        THIS COPYBOOK SHOULD NOT BE CHANGED WITHOUT REFERENCE   *
000011*        TO THESE PROGRAMS.                                      *
000012*                                                                *
000013*    NOTE                                                        *
000014*        THE FILLER AREA AT THE BOTTOM ARE FOR FUTURE EL1042     *
000015*        USE ONLY!                                               *
000016*                                                                *
000017******************************************************************
000018
000019         16  PI-1042-WA.
000020             20  PI-ACTION       PIC  X(01).
000021                 88 PI-SHOW-MODE           VALUE '1'.
000022                 88 PI-CLEAR-MODE          VALUE '2'.
000023                 88 PI-CREATE-MODE         VALUE '3'.
000024             20  PI-COMM-CONTROL PIC  X(12).
000025             20  PI-CURRENT-LINE PIC S9(03) COMP-3.
000026             20  PI-EOF-SW       PIC  X(01).
000027                 88  PI-FILE-EOF           VALUE 'Y'.
000028             20  PI-FILETYP      PIC  X(01).
000029             20  PI-FORM-SQUEEZE-CONTROL
000030                                 PIC  X(01).
000031                 88  PI-FORM-SQUEEZE-ON     VALUE 'Y'.
000032                 88  PI-FORM-SQUEEZE-OFF    VALUE ' '.
000033             20  PI-LAST-CONTROL PIC  X(12).
000034             20  PI-TEMP-STOR-ITEMS
000035                                 PIC S9(04) COMP.
000036             20  PI-TOTAL-LINES  PIC S9(03) COMP-3.
000037             20  PI-UPDATE-SW    PIC  9(01).
000038                 88 ANY-UPDATES            VALUE 1.
000039             20  PI-104-SCREEN-SENT-IND
000040                                 PIC  X(01).
000041                 88  PI-104-SCREEN-SENT    VALUE 'Y'.
000042                 88  PI-104-SCREEN-NOT-SENT VALUE 'N'.
000043             20  PI-1042-SCREEN-SENT-IND
000044                                 PIC  X(01).
000045                 88  PI-1042-SCREEN-SENT    VALUE 'Y'.
000046                 88  PI-1042-SCREEN-NOT-SENT VALUE 'N'.
000047             20  PI-1042-ARCHIVE-IND
000048                                 PIC  X(01).
000049                 88  PI-1042-ARCHIVE-LETTER VALUE 'Y'.
000050             20  FILLER          PIC  X(29).
      *<<((file: ELC1042))
000281*    COPY ELC689PI.
      *>>((file: ELC689PI))
000001******************************************************************
000002*                                                                *
000003*                            ELC689PI                            *
000004*                            VMOD=2.003                          *
000005*                                                                *
000006*    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE       *
000007*    CREDIT CORRESPONDENCE SUB-SYSTEM.  ANY CHANGES WILL         *
000008*    WILL EFFECT THE PROGRAMS OF THAT SUB-SYSTEM.                *
000009*                                                                *
000010*    IF THE LENGTH OF THIS PI-AREA CHANGES THE LENGTH MUST       *
000011*    BE CHANGED FOR THE COMM-AREA WHEN PASSING THIS PI-AREA      *
000012*    BETWEEN PROGRAMS.                                           *
000013*                                                                *
000014*    THE FOLLOWING PROGRAMS USE THIS COPYBOOK:                   *
000015*                                                                *
000016*               EL631 - EL689  - EL6891 - EL6892                 *
000017*                                                                *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 081004                   PEMA  CONVERT TO PSUEDO CONVERSATIONAL
000027* 100705  CR2004072800004  PEMA  ADD LETTERS TO BE RESENT
000028* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
000029******************************************************************
000030
000031
000032         16  PI-689-WORK-AREA.
000033             20  PI-689-ALT-PRINTER-ID
000034                                 PIC  X(04).
000035             20  PI-689-ARCHIVE-NUMBER
000036                                 PIC  9(08).
000037             20  PI-689-ARCHIVE-SW
000038                                 PIC  X(01).
000039                 88  PI-689-ARCHIVE-LETTER VALUE 'Y'.
000040             20  PI-689-DATA-SOURCE
000041                                 PIC  X(01).
000042                 88  PI-689-SRC-ACCOUNT        VALUE '1'.
000043                 88  PI-689-SRC-CERTIFICATE    VALUE '2'.
000044                 88  PI-689-SRC-COMPENSATION   VALUE '3'.
000045                 88  PI-689-SRC-PEND-BUSINESS  VALUE '4'.
000046                 88  PI-689-SRC-CHECKS         VALUE '5'.
000047             20  PI-689-ERROR-IND
000048                                 PIC  X(01).
000049                 88  PI-689-ERR-DETECTED-PREV  VALUE 'Y'.
000050             20  PI-689-ERROR    PIC  9(04).
000051                 88  PI-689-NO-ERRORS-DETECTED VALUE 0000.
000052                 88  PI-689-FATAL-ERROR
000053                     VALUES 0004 0006 0008 0013 0023 0029 0033
000054                            0042 0047 0051 0066 0067 0070
000055                            0168 0169 0174 0175 0176 0177 0179
000056                            0180 0181 0182 0184 0185 0189 0190
000057                            0191
000058                            0215 0279 0280
000059                            0412 0413 0454
000060                            0533 0537
000061                            2055 2114 2208 2209 2216 2232 2369
000062                            2398 2433 2908 2999
000063                            3000 3770 3771 3775
000064                            7250 7365 7367 7368 7369 7370 7371
000065                            7272 7373 7374 7376 7377 7378 7379
000066                            7381 7388 7390 7393 7395 7396 7398
000067                            9095 9096 9281 9298 9299 9320 9327
000068                            9426 9427.
000069                 88  PI-689-STOP-ERROR
000070                     VALUES 0004 0008 0013 0023 0029 0033
000071                            0042 0047 0066 0067 0070
000072                            0168 0169 0174 0175 0176 0177
000073                            0181 0182 0184 0185 0189 0190
000074                            0279 0280
000075                            0412 0413 0454
000076                            2055 2208 2209 2216 2232
000077                            2398 2999
000078                            3000 3770 3771 3775
000079                            7250 7365 7369 7370 7371
000080                            7272 7373 7374 7376 7377 7378 7379
000081                            7381 7388 7390 7393 7396 7398
000082                            9095 9096 9299 9320 9426.
000083             20  PI-689-FOLLOW-UP-DATE
000084                                 PIC  X(02).
000085             20  PI-689-FORM-NUMBER
000086                                 PIC  X(04).
000087             20  PI-689-LABEL-SOURCE
000088                                 PIC X(01).
000089                 88  PI-689-SOURCE-ACCOUNT  VALUE '1'.
000090                 88  PI-689-SOURCE-CARRIER  VALUE '2'.
000091                 88  PI-689-SOURCE-COMPANY  VALUE '3'.
000092                 88  PI-689-SOURCE-COMP     VALUE '4'.
000093                 88  PI-689-SOURCE-MAIL     VALUE '5'.
000094                 88  PI-689-SOURCE-CHECK    VALUE '6'.
000095                 88  PI-689-SOURCE-VARIABLE VALUE '7'.
000096             20  PI-689-NUMBER-COPIES
000097                                 PIC  9(01).
000098             20  PI-689-NUMBER-LABEL-LINES
000099                                 PIC  9(01).
000100             20  PI-689-NUMBER-TEXT-RECORDS
000101                                 PIC  9(03).
000102             20  PI-689-PRINT-ORDER-SW
000103                                 PIC  X(01).
000104                 88  PI-689-PRINT-FIRST     VALUE '1'.
000105                 88  PI-689-PRINT-SECOND    VALUE '2'.
000106                 88  PI-689-PRINT-LATER     VALUE '3'.
000107                 88  PI-689-PRINT-ONLY      VALUE '4'.
000108             20  PI-689-PRINT-RESTRICTION
000109                                 PIC  X(01).
000110                 88  PI-689-VALID-RESTRICT     VALUE 'C' 'F'.
000111                 88  PI-689-PRT-ONLY-WITH-CNTL VALUE 'C'.
000112                 88  PI-689-PRT-ONLY-WITH-FORM VALUE 'F'.
000113             20  PI-689-PRINT-SW PIC  X(01).
000114                 88  PI-689-PRINT-PERFORMED VALUE '1'.
000115             20  PI-689-RESEND-DATE-1
000116                                 PIC  X(02).
000117             20  PI-689-RESEND-LETR-1
000118                                 PIC X(4).
000119             20  PI-689-TEMP-STOR-ID
000120                                 PIC  X(08).
000121             20  PI-689-USE-SCREEN-IND
000122                                 PIC  X(01).
000123                 88  PI-689-CREATE-NO-SCREENS VALUE '1'.
000124             20  PI-689-ARCH-POINTER
000125                                 PIC S9(08) COMP.
000126                 88  PI-689-GET-ARCH-MAIN     VALUE +0.
000127             20  PI-689-ARCT-POINTER
000128                                 PIC S9(08) COMP.
000129                 88  PI-689-GET-ARCT-MAIN     VALUE +0.
000130             20  PI-689-VARIABLE-DATA-GRP.
000131                 24  PI-689-VARIABLE-DATA-1
000132                                 PIC  X(30).
000133                 24  PI-689-VARIABLE-DATA-2
000134                                 PIC  X(30).
000135                 24  PI-689-VARIABLE-DATA-3
000136                                 PIC  X(30).
000137                 24  PI-689-VARIABLE-DATA-4
000138                                 PIC  X(30).
000139
000140         16  PI-689-KEY-DATA-FIELDS.
000141             20  PI-689-ACCOUNT  PIC  X(10).
000142             20  PI-689-CARRIER  PIC  X(01).
000143             20  PI-689-CERT-NO.
000144                 24  PI-689-CERT-PRIME
000145                                 PIC  X(10).
000146                 24  PI-689-CERT-SFX
000147                                 PIC  X(01).
000148             20  PI-689-CHG-SEQ-NO
000149                                 PIC S9(04)    COMP.
000150             20  PI-689-CHG-SEQ-NOX REDEFINES PI-689-CHG-SEQ-NO
000151                                 PIC  X(02).
000152             20  PI-689-ENTRY-BATCH
000153                                 PIC  X(06).
000154             20  PI-689-EFF-DATE PIC  X(02).
000155             20  PI-689-EXP-DATE PIC  X(02).
000156             20  PI-689-GROUPING PIC  X(06).
000157             20  PI-689-RESP-PERSON
000158                                 PIC  X(10).
000159             20  PI-689-SEQ-NO   PIC S9(08)    COMP.
000160             20  PI-689-SEQ-NOX REDEFINES PI-689-SEQ-NO
000161                                 PIC  X(04).
000162             20  PI-689-STATE    PIC  X(02).
000163             20  PI-689-TYPE     PIC  X(01).
000164             20  PI-689-CONTROL  PIC S9(08)    COMP.
000165             20  PI-689-ALT-SEQ-NO
000166                                 PIC S9(04)    COMP.
000167         16  PI-689-DATE-EDIT    PIC  X(08).
000168         16  PI-689-FOLLOW-UP-EDIT
000169                                 PIC  X(08).
000170         16  PI-689-RESEND1-EDIT PIC  X(08).
000171         16  PI-689-SEQ-EDIT     PIC  X(08).
000172         16  PI-689-BCSEQ-EDIT   PIC  X(04).
000173         16  PI-689-LBL-OVERRIDE PIC  X(01).
000174             88  PI-689-LABELS-OVERRIDEN  VALUES 'N'.
000175         16  PI-689-FATAL-CTR    PIC 999     COMP-3.
000176         16  PI-689-FORCABLE-CTR PIC 999     COMP-3.
      *<<((file: ELC689PI))
000282         16  PI-690-WORK-AREA.
000283             20  PI-690-ARCHIVE-TABLE.
000284                 24  PI-690-ARCHIVE-NUM OCCURS 12 TIMES
000285                                 PIC S9(08) COMP.
000286             20  PI-690-CURSOR   PIC S9(04) COMP.
000287             20  PI-690-FIRST-DATA.
000288                 24  PI-690-FIRST-CERT-NO.
000289                     28  PI-690-FIRST-CERT-PRIME
000290                                 PIC  X(10).
000291                     28  PI-690-FIRST-SUFFIX
000292                                 PIC  X(01).
000293                 24  PI-690-FIRST-CARRIER
000294                                 PIC  X(01).
000295                 24  PI-690-FIRST-GROUPING
000296                                 PIC  X(06).
000297                 24  PI-690-FIRST-STATE
000298                                 PIC  X(02).
000299                 24  PI-690-FIRST-ACCOUNT
000300                                 PIC  X(10).
000301                 24  PI-690-FIRST-EFFECT-DATE
000302                                 PIC  X(02).
000303                 24  PI-690-FIRST-ENTRY.
000304                     28  PI-690-FIRST-CONTROL-PREFIX
000305                                 PIC  X(02).
000306                     28  PI-690-FIRST-CONTROL
000307                                 PIC S9(08) COMP.
000308                 24  PI-690-FIRST-FORM
000309                                 PIC  X(04).
000310                 24  PI-690-FIRST-PROCESSOR
000311                                 PIC  X(04).
000312                 24  PI-690-FIRST-ARCHIVE-NO
000313                                 PIC S9(08) COMP.
000314             20  PI-690-INIT-DATA.
000315                 24  PI-690-INIT-CERT-NO.
000316                     28  PI-690-INIT-CERT-PRIME
000317                                 PIC  X(10).
000318                     28  PI-690-INIT-SUFFIX
000319                                 PIC  X(01).
000320                 24  PI-690-INIT-CARRIER
000321                                 PIC  X(01).
000322                 24  PI-690-INIT-GROUPING
000323                                 PIC  X(06).
000324                 24  PI-690-INIT-STATE
000325                                 PIC  X(02).
000326                 24  PI-690-INIT-ACCOUNT
000327                                 PIC  X(10).
000328                 24  PI-690-INIT-EFFECT-DATE
000329                                 PIC  X(02).
000330                 24  PI-690-INIT-EFF-DTE
000331                                 PIC  X(08).
000332                 24  PI-690-INIT-ENTRY.
000333                     28  PI-690-INIT-CONTROL-PREFIX
000334                                 PIC  X(02).
000335                     28  PI-690-INIT-CONTROL
000336                                 PIC S9(08) COMP.
000337                 24  PI-690-INIT-FORM
000338                                 PIC  X(04).
000339                 24  PI-690-INIT-PROCESSOR
000340                                 PIC  X(04).
000341                 24  PI-690-INIT-ARCHIVE-NO
000342                                 PIC S9(08) COMP.
000343             20  PI-690-LAST-DATA.
000344                 24  PI-690-LAST-CERT-NO.
000345                     28  PI-690-LAST-CERT-PRIME
000346                                 PIC  X(10).
000347                     28  PI-690-LAST-SUFFIX
000348                                 PIC  X(01).
000349                 24  PI-690-LAST-CARRIER
000350                                 PIC  X(01).
000351                 24  PI-690-LAST-GROUPING
000352                                 PIC  X(06).
000353                 24  PI-690-LAST-STATE
000354                                 PIC  X(02).
000355                 24  PI-690-LAST-ACCOUNT
000356                                 PIC  X(10).
000357                 24  PI-690-LAST-EFFECT-DATE
000358                                 PIC  X(02).
000359                 24  PI-690-LAST-ENTRY.
000360                     28  PI-690-LAST-CONTROL-PREFIX
000361                                 PIC  X(02).
000362                     28  PI-690-LAST-CONTROL
000363                                 PIC S9(08) COMP.
000364                 24  PI-690-LAST-FORM
000365                                 PIC  X(04).
000366                 24  PI-690-LAST-PROCESSOR
000367                                 PIC  X(04).
000368                 24  PI-690-LAST-ARCHIVE-NO
000369                                 PIC S9(08) COMP.
000370             20  PI-690-LAST-ARCH-NDX
000371                                 PIC S9(04) COMP.
000372             20  PI-690-BRWS-TYPE-IND
000373                                 PIC  9(01).
000374                 88  PI-690-BRWS-CERTRP               VALUE 1.
000375                 88  PI-690-BRWS-FORM                 VALUE 2.
000376                 88  PI-690-BRWS-PROCESSOR            VALUE 3.
000377                 88  PI-690-BRWS-ACCOUNT              VALUE 4.
000378                 88  PI-690-BRWS-ENTRY-CNTL           VALUE 5.
000379                 88  PI-690-BRWS-ARCHIVE              VALUE 6.
000380             20  PI-690-LAST-BROWSE-IND
000381                                 PIC  X(01).
000382                 88  PI-690-LAST-BRWS-FWRD            VALUE '1'.
000383                 88  PI-690-LAST-BRWS-BWRD            VALUE '2'.
000384             20  PI-690-STATUS-SELECTION-IND
000385                                 PIC  X(01).
000386                 88  PI-690-SELECT-ALL       VALUE 'N'.
000387                 88  PI-690-VALID-SELECTION  VALUE 'A' 'C' 'H'
000388                                               'X' 'P' 'V' 'N'.
000389         16  PI-ARCHIVE-COMPLETE    PIC X(01).
000390         16  PI-ARCHIVE-RECEIVED    PIC X(01).
000391         16  PI-ARCHIVE-STOPPED     PIC X(01).
000392         16  PI-ARCHIVE-FINAL       PIC X(01).
000393         16  PI-COMMENT-INDEX       PIC S9(4) COMP.
000394         16  PI-ARCHIVE-LTRID       PIC X(4).
000395         16  pi-create-date         pic x(08).
000396         16  pi-initial-print-date  pic xx.
000397         16  FILLER              PIC X(49).
000398
000399 01  W-CONSTANT-AREA.
000400     12  FILLER                  PIC  X(18)
000401                                 VALUE 'PROGRAM CONSTANTS:'.
000402     12  W-APPL-SCRTY-NDX        PIC S9(04)  COMP  VALUE +03.
000403
000404     12  W-ARCH-FILE-ID          PIC  X(08)  VALUE 'ERARCH'.
000405     12  W-ARCT-FILE-ID          PIC  X(08)  VALUE 'ERARCT'.
000406     12  W-ARCT-LENGTH           PIC S9(04)  COMP  VALUE +1640.
000407     12  W-LINK-001              PIC  X(08)  VALUE 'EL001'.
000408     12  W-LINK-004              PIC  X(08)  VALUE 'EL004'.
000409     12  W-MAP                   PIC  X(08)  VALUE 'EL691A'.
000410     12  W-MAP-REDEFINE  REDEFINES   W-MAP.
000411         16  FILLER              PIC  X(02).
000412         16  W-MAP-NUM           PIC  X(06).
000413     12  W-MAPSET                PIC  X(08)  VALUE 'EL691S'.
000414     12  W-THIS-PGM              PIC  X(08)  VALUE 'EL691'.
000415     12  W-TRANSACTION           PIC  X(04)  VALUE 'EXN1'.
000416     12  W-XCTL-005              PIC  X(08)  VALUE 'EL005'.
000417     12  W-XCTL-626              PIC  X(08)  VALUE 'EL626'.
000418     12  W-XCTL-1279             PIC  X(08)  VALUE 'EL1279'.
000419     12  SLASH                   PIC X       VALUE '/'.
000420     12  W-ZEROS                 PIC  S9(03) VALUE +0 COMP-3.
000421     12  W-ADD-ARCT              PIC  X      VALUE 'N'.
000422     12  W-DONE-ADDING           PIC  X      VALUE 'N'.
000423     12  W-NEED-COMMENT          PIC  X      VALUE 'N'.
000424
000425 01  ERROR-MESSAGES.
000426     12  ER-0000                 PIC  X(04) VALUE '0000'.
000427     12  ER-0004                 PIC  X(04) VALUE '0004'.
000428     12  ER-0029                 PIC  X(04) VALUE '0029'.
000429     12  ER-0051                 PIC  X(04) VALUE '0051'.
000430     12  ER-0070                 PIC  X(04) VALUE '0070'.
000431     12  ER-0249                 PIC  X(04) VALUE '0249'.
000432     12  ER-0295                 PIC  X(04) VALUE '0295'.
000433     12  ER-0296                 PIC  X(04) VALUE '0296'.
000434     12  ER-0539                 PIC  X(04) VALUE '0539'.
000435     12  ER-0895                 PIC  X(04) VALUE '0895'.
000436     12  ER-0897                 PIC  X(04) VALUE '0897'.
000437     12  ER-1236                 PIC  X(04) VALUE '1236'.
000438     12  ER-3112                 PIC  X(04) VALUE '3112'.
000439     12  ER-7008                 PIC  X(04) VALUE '7008'.
000440     12  ER-7330                 PIC  X(04) VALUE '7330'.
000441     12  ER-7331                 PIC  X(04) VALUE '7331'.
000442     12  ER-7332                 PIC  X(04) VALUE '7332'.
000443     12  ER-7333                 PIC  X(04) VALUE '7333'.
000444     12  ER-7334                 PIC  X(04) VALUE '7334'.
000445     12  ER-7363                 PIC  X(04) VALUE '7363'.
000446     12  ER-7364                 PIC  X(04) VALUE '7364'.
000447     12  ER-7371                 PIC  X(04) VALUE '7371'.
000448     12  ER-7373                 PIC  X(04) VALUE '7373'.
000449     12  ER-7388                 PIC  X(04) VALUE '7388'.
000450     12  ER-9097                 PIC  X(04) VALUE '9097'.
000451     12  ER-9245                 PIC  X(04) VALUE '9245'.
000452
000453*                                COPY ELCAID.
      *>>((file: ELCAID))
000001******************************************************************
000002*                                                                *
000003*                            ELCAID.                             *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
000007*                                                                *
000008*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
000009*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
000010******************************************************************
000011
000012 01  DFHAID.
000013   02  DFHNULL   PIC  X  VALUE  ' '.
000014   02  DFHENTER  PIC  X  VALUE  QUOTE.
000015   02  DFHCLEAR  PIC  X  VALUE  '_'.
000016   02  DFHPEN    PIC  X  VALUE  '='.
000017   02  DFHOPID   PIC  X  VALUE  'W'.
000018   02  DFHPA1    PIC  X  VALUE  '%'.
000019   02  DFHPA2    PIC  X  VALUE  '>'.
000020   02  DFHPA3    PIC  X  VALUE  ','.
000021   02  DFHPF1    PIC  X  VALUE  '1'.
000022   02  DFHPF2    PIC  X  VALUE  '2'.
000023   02  DFHPF3    PIC  X  VALUE  '3'.
000024   02  DFHPF4    PIC  X  VALUE  '4'.
000025   02  DFHPF5    PIC  X  VALUE  '5'.
000026   02  DFHPF6    PIC  X  VALUE  '6'.
000027   02  DFHPF7    PIC  X  VALUE  '7'.
000028   02  DFHPF8    PIC  X  VALUE  '8'.
000029   02  DFHPF9    PIC  X  VALUE  '9'.
000030   02  DFHPF10   PIC  X  VALUE  ':'.
000031   02  DFHPF11   PIC  X  VALUE  '#'.
000032   02  DFHPF12   PIC  X  VALUE  '@'.
000033   02  DFHPF13   PIC  X  VALUE  'A'.
000034   02  DFHPF14   PIC  X  VALUE  'B'.
000035   02  DFHPF15   PIC  X  VALUE  'C'.
000036   02  DFHPF16   PIC  X  VALUE  'D'.
000037   02  DFHPF17   PIC  X  VALUE  'E'.
000038   02  DFHPF18   PIC  X  VALUE  'F'.
000039   02  DFHPF19   PIC  X  VALUE  'G'.
000040   02  DFHPF20   PIC  X  VALUE  'H'.
000041   02  DFHPF21   PIC  X  VALUE  'I'.
000042*00039    02  DFHPF22   PIC  X  VALUE  '?'.
000043   02  DFHPF22   PIC  X  VALUE  '['.
000044   02  DFHPF23   PIC  X  VALUE  '.'.
000045   02  DFHPF24   PIC  X  VALUE  '<'.
000046   02  DFHMSRE   PIC  X  VALUE  'X'.
000047   02  DFHSTRF   PIC  X  VALUE  'h'.
000048   02  DFHTRIG   PIC  X  VALUE  '"'.
      *<<((file: ELCAID))
000454 01  FILLER    REDEFINES DFHAID.
000455     12  FILLER                  PIC  X(08).
000456     12  PF-VALUES               PIC  X(01) OCCURS 2.
000457
000458*                                COPY ELCATTR.
      *>>((file: ELCATTR))
000001******************************************************************
000002*                                                                *
000003*                            ELCATTR.                            *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*             LIST OF STANDARD ATTRIBUTE VALUES                  *
000007*                                                                *
000008*   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
000009*                                                                *
000010*                   POS 1   P=PROTECTED                          *
000011*                           U=UNPROTECTED                        *
000012*                           S=ASKIP                              *
000013*                   POS 2   A=ALPHA/NUMERIC                      *
000014*                           N=NUMERIC                            *
000015*                   POS 3   N=NORMAL                             *
000016*                           B=BRIGHT                             *
000017*                           D=DARK                               *
000018*                   POS 4-5 ON=MODIFIED DATA TAG ON              *
000019*                           OF=MODIFIED DATA TAG OFF             *
000020*                                                                *
000021*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
000022******************************************************************
000023 01  ATTRIBUTE-LIST.
000024     12  AL-PABOF            PIC X       VALUE 'Y'.
000025     12  AL-PABON            PIC X       VALUE 'Z'.
000026     12  AL-PADOF            PIC X       VALUE '%'.
000027     12  AL-PADON            PIC X       VALUE '_'.
000028     12  AL-PANOF            PIC X       VALUE '-'.
000029     12  AL-PANON            PIC X       VALUE '/'.
000030     12  AL-SABOF            PIC X       VALUE '8'.
000031     12  AL-SABON            PIC X       VALUE '9'.
000032     12  AL-SADOF            PIC X       VALUE '@'.
000033     12  AL-SADON            PIC X       VALUE QUOTE.
000034     12  AL-SANOF            PIC X       VALUE '0'.
000035     12  AL-SANON            PIC X       VALUE '1'.
000036     12  AL-UABOF            PIC X       VALUE 'H'.
000037     12  AL-UABON            PIC X       VALUE 'I'.
000038     12  AL-UADOF            PIC X       VALUE '<'.
000039     12  AL-UADON            PIC X       VALUE '('.
000040     12  AL-UANOF            PIC X       VALUE ' '.
000041     12  AL-UANON            PIC X       VALUE 'A'.
000042     12  AL-UNBOF            PIC X       VALUE 'Q'.
000043     12  AL-UNBON            PIC X       VALUE 'R'.
000044     12  AL-UNDOF            PIC X       VALUE '*'.
000045     12  AL-UNDON            PIC X       VALUE ')'.
000046     12  AL-UNNOF            PIC X       VALUE '&'.
000047     12  AL-UNNON            PIC X       VALUE 'J'.
      *<<((file: ELCATTR))
000459*                                COPY ELCDATE.
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
000460*                                COPY ELCTEXT.
      *>>((file: ELCTEXT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCTEXT.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.008                          *
000007*                                                                *
000008*   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
000009*                                     FORM LETTERS,              *
000010*                                     CERT FORM DISPLAY.
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 100   RECFORM = FIXED                          *
000014*                                                                *
000015*   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
000016*       ALTERNATE INDEX = NONE                                   *
000017*                                                                *
000018*   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
000019*       ALTERNATE INDEX = NONE                                   *
000020*                                                                *
000021*   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
000022*       ALTERNATE INDEX = NONE                                   *
000023*                                                                *
000024*   LOG = NO                                                     *
000025*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000026******************************************************************
000027 01  TEXT-FILES.
000028     12  TEXT-FILE-ID                PIC XX.
000029         88  FORMS-FILE-TEXT            VALUE 'TF'.
000030         88  LETTER-FILE-TEXT           VALUE 'TL'.
000031         88  HELP-FILE-TEXT             VALUE 'TH'.
000032
000033     12  TX-CONTROL-PRIMARY.
000034         16  TX-COMPANY-CD           PIC X.
000035             88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
000036         16  TX-ACCESS-CD-GENL       PIC X(12).
000037
000038         16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
000039             20  TX-LETTER-NO        PIC X(4).
000040             20  FILLER              PIC X(8).
000041
000042         16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
000043             20  TX-FORM-NO          PIC X(12).
000044
000045         16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
000046             20  TX-HELP-TYPE        PIC X.
000047                 88  HELP-FOR-GENERAL   VALUE ' '.
000048                 88  HELP-BY-SCREEN     VALUE 'S'.
000049                 88  HELP-BY-ERROR      VALUE 'E'.
000050             20  TX-SCREEN-OR-ERROR  PIC X(4).
000051                 88  GENERAL-INFO-HELP  VALUE '0000'.
000052             20  TX-HELP-FOR-COMPANY  PIC XXX.
000053                 88  NOT-COMPANY-SPECIFIC VALUE '   '.
000054             20  FILLER              PIC X(4).
000055
000056         16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
000057
000058     12  TX-PROCESS-CONTROL          PIC XX.
000059         88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
000060
000061     12  TX-TEXT-LINE                PIC X(70).
000062
000063     12  TX-FORM-SQUEEZE-CONTROL     PIC X.
000064         88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
000065         88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
000066         88  TX-VALID-FORM-SQUEEZE-VALUE
000067                                        VALUE 'Y' ' '.
000068
000069     12  TX-LINE-SQUEEZE-CONTROL     PIC X.
000070         88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
000071         88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
000072         88  TX-DO-NOT-ADJUST           VALUE 'N'.
000073         88  TX-FORM-CONTROL-LINE       VALUE 'K'.
000074         88  TX-NEW-PARAGRAPH           VALUE 'P'.
000075         88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
000076         88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
000077                                              'K' 'N' ' '
000078                                              'Z'.
000079
000080     12  TX-ARCHIVE-SW               PIC X.
000081         88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
000082         88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
000083         88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
000084
000085     12  TX-LAST-MAINTENANCED-BY     PIC X(4).
000086     12  TX-LAST-MAINTENANCED-DT     PIC X(2).
000087
000088     12  TX-BSR-CODE                 PIC X.
000089         88  TX-BSR-LETTER              VALUE 'B'.
000090         88  TX-NON-BSR-LETTER          VALUE ' '.
000091
000092     12  FILLER                      PIC X.
000093*****************************************************************
      *<<((file: ELCTEXT))
000461*                                COPY ELCNWA.
      *>>((file: ELCNWA))
000001*****************************************************************
000002*                                                               *
000003*                                                               *
000004*                            ELCNWA.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                         *
000007*                                                               *
000008*            M O V E   N A M E   W O R K   A R E A.             *
000009*                                                               *
000010*****************************************************************.
000011
000012 01  WS-NAME-WORK-AREA.
000013     05  WS-INSURED-LAST-NAME        PIC X(15).
000014     05  WS-INSURED-1ST-NAME         PIC X(12).
000015     05  WS-INSURED-MID-INIT         PIC X.
000016
000017     05  WS-NAME-WORK.
000018         10  WS-NW                   PIC X
000019             OCCURS 30 TIMES INDEXED BY NWA-INDEX.
000020
000021     05  WS-NAME-WORK2.
000022         10  WS-NW2                  PIC X
000023             OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
000024                                        NWA-INDEX0.
000025
000026     05  WS-NAME-SW                  PIC S9          VALUE ZERO
000027                                     COMP-3.
000028
      *<<((file: ELCNWA))
000462*                                COPY ELCEMIB.
      *>>((file: ELCEMIB))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCEMIB.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.005                          *
000007*                                                                *
000008*    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
000009*                                                                *
000010******************************************************************
000011 01  ERROR-MESSAGE-INTERFACE-BLOCK.
000012     12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
000013     12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
000014     12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
000015     12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
000016     12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
000017     12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
000018     12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
000019     12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
000020     12  EMI-SWITCH1             PIC X        VALUE '1'.
000021         88  EMI-NO-ERRORS                    VALUE '1'.
000022         88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
000023         88  EMI-ERRORS-COMPLETE              VALUE '3'.
000024     12  EMI-SWITCH2             PIC X        VALUE '1'.
000025         88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
000026     12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
000027         88  EMI-AREA1-EMPTY                  VALUE '1'.
000028         88  EMI-AREA1-FULL                   VALUE '2'.
000029     12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
000030         88  EMI-AREA2-EMPTY                  VALUE '1'.
000031         88  EMI-AREA2-FULL                   VALUE '2'.
000032     12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
000033         88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
000034         88  EMI-BYPASS-NOTES                 VALUE 'N'.
000035         88  EMI-BYPASS-WARNINGS              VALUE 'W'.
000036         88  EMI-BYPASS-FORCABLES             VALUE 'F'.
000037         88  EMI-BYPASS-FATALS                VALUE 'X'.
000038     12  EMI-ERROR-LINES.
000039         16  EMI-LINE1           PIC X(72)   VALUE SPACES.
000040         16  EMI-LINE2           PIC X(72)   VALUE SPACES.
000041         16  EMI-LINE3           PIC X(72)   VALUE SPACES.
000042         16  EMI-CODE-LINE REDEFINES EMI-LINE3.
000043             20  EMI-ERR-CODES OCCURS 10 TIMES.
000044                 24  EMI-ERR-NUM         PIC X(4).
000045                 24  EMI-FILLER          PIC X.
000046                 24  EMI-SEV             PIC X.
000047                 24  FILLER              PIC X.
000048             20  FILLER                  PIC X(02).
000049     12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
000050         16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
000051             20  EMI-ERROR-NUMBER    PIC X(4).
000052             20  EMI-FILL            PIC X.
000053             20  EMI-SEVERITY        PIC X.
000054             20  FILLER              PIC X.
000055             20  EMI-ERROR-TEXT.
000056                 24  EMI-TEXT-VARIABLE   PIC X(10).
000057                 24  FILLER          PIC X(55).
000058     12  EMI-SEVERITY-SAVE           PIC X.
000059         88  EMI-NOTE                    VALUE 'N'.
000060         88  EMI-WARNING                 VALUE 'W'.
000061         88  EMI-FORCABLE                VALUE 'F'.
000062         88  EMI-FATAL                   VALUE 'X'.
000063     12  EMI-MESSAGE-FLAG            PIC X.
000064         88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
000065         88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
000066     12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
000067     12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
000068         88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
000069         88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
000070         88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
000071     12  emi-claim-no                pic x(7).
000072     12  emi-claim-type              pic x(6).
000073     12  FILLER                      PIC X(124)  VALUE SPACES.
000074     12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
000075     12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
000076     12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
000077     12  EMI-AH-OVERRIDE-L6          PIC X(6).
      *<<((file: ELCEMIB))
000463*                                COPY ERCCNOT.
      *>>((file: ERCCNOT))
000001******************************************************************
000002*                                                                *
000003*                            ERCCNOT                             *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.003                          *
000006*                                                                *
000007*        FILE DESCRIPTION = CERTIFICATE NOTES                    *
000008*                                                                *
000009*        FILE TYPE= VSAM,KSDS                                    *
000010*        RECORD SIZE = 150    RECFORM = FIXED                    *
000011*                                                                *
000012*        BASE CLUSTER = ERCNOT        RKP=2,LEN=36               *
000013*                                                                *
000014*        LOG = YES                                               *
000015*        SERVREQ = DELETE,UPDATE,NEWREC                          *
000016*                                                                *
000017******************************************************************
000018*                   C H A N G E   L O G
000019*
000020* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000021*-----------------------------------------------------------------
000022*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000023* EFFECTIVE    NUMBER
000024*-----------------------------------------------------------------
000025* 091509  CR2008100900003  AJRA  NEW FILE FOR CERT NOTES.
000026******************************************************************
000027
000028 01  CERT-NOTE-FILE.
000029     12  CZ-RECORD-ID                PIC  XX.
000030         88  VALID-CZ-ID                  VALUE 'CZ'.
000031
000032     12  CZ-CONTROL-PRIMARY.
000033         16  CZ-COMPANY-CD           PIC X.
000034         16  CZ-CARRIER              PIC X.
000035         16  CZ-GROUPING.
000036             20 CZ-GROUPING-PREFIX   PIC XXX.
000037             20 CZ-GROUPING-PRIME    PIC XXX.
000038         16  CZ-STATE                PIC XX.
000039         16  CZ-ACCOUNT.
000040             20 CZ-ACCOUNT-PREFIX    PIC X(4).
000041             20 CZ-ACCOUNT-PRIME     PIC X(6).
000042         16  CZ-CERT-EFF-DT          PIC XX.
000043         16  CZ-CERT-NO.
000044             20  CZ-CERT-PRIME       PIC X(10).
000045             20  CZ-CERT-SFX         PIC X.
000046         16  CZ-RECORD-TYPE          PIC X.
000047             88  CERT-NOTE           VALUE '1'.
000048             88  CLAIM-CERT-NOTE     VALUE '2'.
000049         16  CZ-NOTE-SEQUENCE        PIC S9(4)     COMP.
000050
000051     12  CZ-LAST-MAINT-DT            PIC XX.
000052     12  CZ-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
000053     12  CZ-LAST-MAINT-USER          PIC X(4).
000054
000055     12  CZ-NOTE-INFORMATION.
000056         16  CZ-NOTE                 PIC X(63).
000057         16  FILLER                  PIC X(39).
000058******************************************************************
      *<<((file: ERCCNOT))
000464
000465 01  EMI-SAVE-AREA               PIC X(400).
000466
000467*                                COPY ELCLOGOF.
      *>>((file: ELCLOGOF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCLOGOF.                           *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
000008*                                                                *
000009******************************************************************
000010 01  CLASIC-LOGOFF.
000011     12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
000012     12  LOGOFF-TEXT.
000013         16  FILLER          PIC X(5)    VALUE SPACES.
000014         16  LOGOFF-MSG.
000015             20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
000016             20  FILLER      PIC X       VALUE SPACES.
000017             20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
000018         16  FILLER          PIC X(80)
000019           VALUE '* YOU ARE NOW LOGGED OFF'.
000020         16  FILLER          PIC X(7)    VALUE '* LOGIC'.
000021         16  FILLER          PIC X       VALUE QUOTE.
000022         16  LOGOFF-SYS-MSG  PIC X(17)
000023           VALUE 'S CLAS-IC SYSTEM '.
000024     12  TEXT-MESSAGES.
000025         16  UNACCESS-MSG    PIC X(29)
000026             VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
000027         16  PGMIDERR-MSG    PIC X(17)
000028             VALUE 'PROGRAM NOT FOUND'.
      *<<((file: ELCLOGOF))
000468*                                COPY ELCSCTM.
      *>>((file: ELCSCTM))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCSCTM                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
000008*                                                                *
000009******************************************************************
000010 01  SECURITY-MESSAGE.
000011     12  FILLER                          PIC X(30)
000012            VALUE '** LOGIC SECURITY VIOLATION -'.
000013     12  SM-READ                         PIC X(6).
000014     12  FILLER                          PIC X(5)
000015            VALUE ' PGM='.
000016     12  SM-PGM                          PIC X(6).
000017     12  FILLER                          PIC X(5)
000018            VALUE ' OPR='.
000019     12  SM-PROCESSOR-ID                 PIC X(4).
000020     12  FILLER                          PIC X(6)
000021            VALUE ' TERM='.
000022     12  SM-TERMID                       PIC X(4).
000023     12  FILLER                          PIC XX   VALUE SPACE.
000024     12  SM-JUL-DATE                     PIC 9(5).
000025     12  FILLER                          PIC X    VALUE SPACE.
000026     12  SM-TIME                         PIC 99.99.
000027
      *<<((file: ELCSCTM))
000469*                                COPY ELCSCRTY.
      *>>((file: ELCSCRTY))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCSCRTY                            *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
000008*        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
000009*        SAVED IN PI-SECURITY-ADDRESS.                           *
000010*                                                                *
000011******************************************************************
000012 01  SECURITY-CONTROL.
000013     12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
000014     12  FILLER                       PIC XX    VALUE 'SC'.
000015     12  SC-CREDIT-CODES.
000016         16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
000017             20  SC-CREDIT-DISPLAY    PIC X.
000018             20  SC-CREDIT-UPDATE     PIC X.
000019     12  SC-CLAIMS-CODES.
000020         16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
000021             20  SC-CLAIMS-DISPLAY    PIC X.
000022             20  SC-CLAIMS-UPDATE     PIC X.
      *<<((file: ELCSCRTY))
000470*                                COPY EL691S.
      *>>((file: EL691S))
000001 01  EL691AI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  RUNDTEL PIC S9(0004) COMP.
000005     05  RUNDTEF PIC  X(0001).
000006     05  FILLER REDEFINES RUNDTEF.
000007         10  RUNDTEA PIC  X(0001).
000008     05  RUNDTEI PIC  X(0008).
000009*    -------------------------------
000010     05  RUNTIMEL PIC S9(0004) COMP.
000011     05  RUNTIMEF PIC  X(0001).
000012     05  FILLER REDEFINES RUNTIMEF.
000013         10  RUNTIMEA PIC  X(0001).
000014     05  RUNTIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  COMPANYL PIC S9(0004) COMP.
000017     05  COMPANYF PIC  X(0001).
000018     05  FILLER REDEFINES COMPANYF.
000019         10  COMPANYA PIC  X(0001).
000020     05  COMPANYI PIC  X(0003).
000021*    -------------------------------
000022     05  USERIDL PIC S9(0004) COMP.
000023     05  USERIDF PIC  X(0001).
000024     05  FILLER REDEFINES USERIDF.
000025         10  USERIDA PIC  X(0001).
000026     05  USERIDI PIC  X(0004).
000027*    -------------------------------
000028     05  CARRIERL PIC S9(0004) COMP.
000029     05  CARRIERF PIC  X(0001).
000030     05  FILLER REDEFINES CARRIERF.
000031         10  CARRIERA PIC  X(0001).
000032     05  CARRIERI PIC  X(0001).
000033*    -------------------------------
000034     05  GROUPL PIC S9(0004) COMP.
000035     05  GROUPF PIC  X(0001).
000036     05  FILLER REDEFINES GROUPF.
000037         10  GROUPA PIC  X(0001).
000038     05  GROUPI PIC  X(0006).
000039*    -------------------------------
000040     05  STATEL PIC S9(0004) COMP.
000041     05  STATEF PIC  X(0001).
000042     05  FILLER REDEFINES STATEF.
000043         10  STATEA PIC  X(0001).
000044     05  STATEI PIC  X(0002).
000045*    -------------------------------
000046     05  ACCTL PIC S9(0004) COMP.
000047     05  ACCTF PIC  X(0001).
000048     05  FILLER REDEFINES ACCTF.
000049         10  ACCTA PIC  X(0001).
000050     05  ACCTI PIC  X(0010).
000051*    -------------------------------
000052     05  CERTL PIC S9(0004) COMP.
000053     05  CERTF PIC  X(0001).
000054     05  FILLER REDEFINES CERTF.
000055         10  CERTA PIC  X(0001).
000056     05  CERTI PIC  X(0010).
000057*    -------------------------------
000058     05  SFXL PIC S9(0004) COMP.
000059     05  SFXF PIC  X(0001).
000060     05  FILLER REDEFINES SFXF.
000061         10  SFXA PIC  X(0001).
000062     05  SFXI PIC  X(0001).
000063*    -------------------------------
000064     05  MAINTL PIC S9(0004) COMP.
000065     05  MAINTF PIC  X(0001).
000066     05  FILLER REDEFINES MAINTF.
000067         10  MAINTA PIC  X(0001).
000068     05  MAINTI PIC  X(0001).
000069*    -------------------------------
000070     05  MAINTBYL PIC S9(0004) COMP.
000071     05  MAINTBYF PIC  X(0001).
000072     05  FILLER REDEFINES MAINTBYF.
000073         10  MAINTBYA PIC  X(0001).
000074     05  MAINTBYI PIC  X(0004).
000075*    -------------------------------
000076     05  MAINTDTL PIC S9(0004) COMP.
000077     05  MAINTDTF PIC  X(0001).
000078     05  FILLER REDEFINES MAINTDTF.
000079         10  MAINTDTA PIC  X(0001).
000080     05  MAINTDTI PIC  X(0008).
000081*    -------------------------------
000082     05  MAINTTML PIC S9(0004) COMP.
000083     05  MAINTTMF PIC  X(0001).
000084     05  FILLER REDEFINES MAINTTMF.
000085         10  MAINTTMA PIC  X(0001).
000086     05  MAINTTMI PIC  X(0005).
000087*    -------------------------------
000088     05  ARCHNOL PIC S9(0004) COMP.
000089     05  ARCHNOF PIC  X(0001).
000090     05  FILLER REDEFINES ARCHNOF.
000091         10  ARCHNOA PIC  X(0001).
000092     05  ARCHNOI PIC  X(0008).
000093*    -------------------------------
000094     05  ENDARCHL PIC S9(0004) COMP.
000095     05  ENDARCHF PIC  X(0001).
000096     05  FILLER REDEFINES ENDARCHF.
000097         10  ENDARCHA PIC  X(0001).
000098     05  ENDARCHI PIC  X(0008).
000099*    -------------------------------
000100     05  FORMNOL PIC S9(0004) COMP.
000101     05  FORMNOF PIC  X(0001).
000102     05  FILLER REDEFINES FORMNOF.
000103         10  FORMNOA PIC  X(0001).
000104     05  FORMNOI PIC  X(0004).
000105*    -------------------------------
000106     05  RESFORML PIC S9(0004) COMP.
000107     05  RESFORMF PIC  X(0001).
000108     05  FILLER REDEFINES RESFORMF.
000109         10  RESFORMA PIC  X(0001).
000110     05  RESFORMI PIC  X(0004).
000111*    -------------------------------
000112     05  CREATDTL PIC S9(0004) COMP.
000113     05  CREATDTF PIC  X(0001).
000114     05  FILLER REDEFINES CREATDTF.
000115         10  CREATDTA PIC  X(0001).
000116     05  CREATDTI PIC  X(0008).
000117*    -------------------------------
000118     05  RESENDL PIC S9(0004) COMP.
000119     05  RESENDF PIC  X(0001).
000120     05  FILLER REDEFINES RESENDF.
000121         10  RESENDA PIC  X(0001).
000122     05  RESENDI PIC  X(0008).
000123*    -------------------------------
000124     05  CREATBYL PIC S9(0004) COMP.
000125     05  CREATBYF PIC  X(0001).
000126     05  FILLER REDEFINES CREATBYF.
000127         10  CREATBYA PIC  X(0001).
000128     05  CREATBYI PIC  X(0004).
000129*    -------------------------------
000130     05  PRINTDTL PIC S9(0004) COMP.
000131     05  PRINTDTF PIC  X(0001).
000132     05  FILLER REDEFINES PRINTDTF.
000133         10  PRINTDTA PIC  X(0001).
000134     05  PRINTDTI PIC  X(0008).
000135*    -------------------------------
000136     05  RESPRNTL PIC S9(0004) COMP.
000137     05  RESPRNTF PIC  X(0001).
000138     05  FILLER REDEFINES RESPRNTF.
000139         10  RESPRNTA PIC  X(0001).
000140     05  RESPRNTI PIC  X(0008).
000141*    -------------------------------
000142     05  STCOMPL PIC S9(0004) COMP.
000143     05  STCOMPF PIC  X(0001).
000144     05  FILLER REDEFINES STCOMPF.
000145         10  STCOMPA PIC  X(0001).
000146     05  STCOMPI PIC  X(0019).
000147*    -------------------------------
000148     05  REPLYL PIC S9(0004) COMP.
000149     05  REPLYF PIC  X(0001).
000150     05  FILLER REDEFINES REPLYF.
000151         10  REPLYA PIC  X(0001).
000152     05  REPLYI PIC  X(0008).
000153*    -------------------------------
000154     05  STRECVL PIC S9(0004) COMP.
000155     05  STRECVF PIC  X(0001).
000156     05  FILLER REDEFINES STRECVF.
000157         10  STRECVA PIC  X(0001).
000158     05  STRECVI PIC  X(0019).
000159*    -------------------------------
000160     05  STOPDTEL PIC S9(0004) COMP.
000161     05  STOPDTEF PIC  X(0001).
000162     05  FILLER REDEFINES STOPDTEF.
000163         10  STOPDTEA PIC  X(0001).
000164     05  STOPDTEI PIC  X(0008).
000165*    -------------------------------
000166     05  STSTOPL PIC S9(0004) COMP.
000167     05  STSTOPF PIC  X(0001).
000168     05  FILLER REDEFINES STSTOPF.
000169         10  STSTOPA PIC  X(0001).
000170     05  STSTOPI PIC  X(0019).
000171*    -------------------------------
000172     05  OBVYNL PIC S9(0004) COMP.
000173     05  OBVYNF PIC  X(0001).
000174     05  FILLER REDEFINES OBVYNF.
000175         10  OBVYNA PIC  X(0001).
000176     05  OBVYNI PIC  X(0001).
000177*    -------------------------------
000178     05  FINDATEL PIC S9(0004) COMP.
000179     05  FINDATEF PIC  X(0001).
000180     05  FILLER REDEFINES FINDATEF.
000181         10  FINDATEA PIC  X(0001).
000182     05  FINDATEI PIC  X(0008).
000183*    -------------------------------
000184     05  STFINLL PIC S9(0004) COMP.
000185     05  STFINLF PIC  X(0001).
000186     05  FILLER REDEFINES STFINLF.
000187         10  STFINLA PIC  X(0001).
000188     05  STFINLI PIC  X(0019).
000189*    -------------------------------
000190     05  OBUIDL PIC S9(0004) COMP.
000191     05  OBUIDF PIC  X(0001).
000192     05  FILLER REDEFINES OBUIDF.
000193         10  OBUIDA PIC  X(0001).
000194     05  OBUIDI PIC  X(0005).
000195*    -------------------------------
000196     05  FINLACTL PIC S9(0004) COMP.
000197     05  FINLACTF PIC  X(0001).
000198     05  FILLER REDEFINES FINLACTF.
000199         10  FINLACTA PIC  X(0001).
000200     05  FINLACTI PIC  X(0001).
000201*    -------------------------------
000202     05  COMMENTL PIC S9(0004) COMP.
000203     05  COMMENTF PIC  X(0001).
000204     05  FILLER REDEFINES COMMENTF.
000205         10  COMMENTA PIC  X(0001).
000206     05  COMMENTI PIC  X(0069).
000207*    -------------------------------
000208     05  ERRMSG1L PIC S9(0004) COMP.
000209     05  ERRMSG1F PIC  X(0001).
000210     05  FILLER REDEFINES ERRMSG1F.
000211         10  ERRMSG1A PIC  X(0001).
000212     05  ERRMSG1I PIC  X(0079).
000213*    -------------------------------
000214     05  ERRMSG2L PIC S9(0004) COMP.
000215     05  ERRMSG2F PIC  X(0001).
000216     05  FILLER REDEFINES ERRMSG2F.
000217         10  ERRMSG2A PIC  X(0001).
000218     05  ERRMSG2I PIC  X(0079).
000219 01  EL691AO REDEFINES EL691AI.
000220     05  FILLER            PIC  X(0012).
000221*    -------------------------------
000222     05  FILLER            PIC  X(0003).
000223     05  RUNDTEO PIC  X(0008).
000224*    -------------------------------
000225     05  FILLER            PIC  X(0003).
000226     05  RUNTIMEO PIC  99.99.
000227*    -------------------------------
000228     05  FILLER            PIC  X(0003).
000229     05  COMPANYO PIC  X(0003).
000230*    -------------------------------
000231     05  FILLER            PIC  X(0003).
000232     05  USERIDO PIC  X(0004).
000233*    -------------------------------
000234     05  FILLER            PIC  X(0003).
000235     05  CARRIERO PIC  X(0001).
000236*    -------------------------------
000237     05  FILLER            PIC  X(0003).
000238     05  GROUPO PIC  X(0006).
000239*    -------------------------------
000240     05  FILLER            PIC  X(0003).
000241     05  STATEO PIC  X(0002).
000242*    -------------------------------
000243     05  FILLER            PIC  X(0003).
000244     05  ACCTO PIC  X(0010).
000245*    -------------------------------
000246     05  FILLER            PIC  X(0003).
000247     05  CERTO PIC  X(0010).
000248*    -------------------------------
000249     05  FILLER            PIC  X(0003).
000250     05  SFXO PIC  X(0001).
000251*    -------------------------------
000252     05  FILLER            PIC  X(0003).
000253     05  MAINTO PIC  X(0001).
000254*    -------------------------------
000255     05  FILLER            PIC  X(0003).
000256     05  MAINTBYO PIC  X(0004).
000257*    -------------------------------
000258     05  FILLER            PIC  X(0003).
000259     05  MAINTDTO PIC  99B99B99.
000260*    -------------------------------
000261     05  FILLER            PIC  X(0003).
000262     05  MAINTTMO PIC  99.99.
000263*    -------------------------------
000264     05  FILLER            PIC  X(0003).
000265     05  ARCHNOO PIC  X(0008).
000266*    -------------------------------
000267     05  FILLER            PIC  X(0003).
000268     05  ENDARCHO PIC  X(0008).
000269*    -------------------------------
000270     05  FILLER            PIC  X(0003).
000271     05  FORMNOO PIC  X(0004).
000272*    -------------------------------
000273     05  FILLER            PIC  X(0003).
000274     05  RESFORMO PIC  X(0004).
000275*    -------------------------------
000276     05  FILLER            PIC  X(0003).
000277     05  CREATDTO PIC  99B99B99.
000278*    -------------------------------
000279     05  FILLER            PIC  X(0003).
000280     05  RESENDO PIC  99B99B99.
000281*    -------------------------------
000282     05  FILLER            PIC  X(0003).
000283     05  CREATBYO PIC  X(0004).
000284*    -------------------------------
000285     05  FILLER            PIC  X(0003).
000286     05  PRINTDTO PIC  99B99B99.
000287*    -------------------------------
000288     05  FILLER            PIC  X(0003).
000289     05  RESPRNTO PIC  99B99B99.
000290*    -------------------------------
000291     05  FILLER            PIC  X(0003).
000292     05  STCOMPO PIC  X(0019).
000293*    -------------------------------
000294     05  FILLER            PIC  X(0003).
000295     05  REPLYO PIC  99B99B99.
000296*    -------------------------------
000297     05  FILLER            PIC  X(0003).
000298     05  STRECVO PIC  X(0019).
000299*    -------------------------------
000300     05  FILLER            PIC  X(0003).
000301     05  STOPDTEO PIC  99B99B99.
000302*    -------------------------------
000303     05  FILLER            PIC  X(0003).
000304     05  STSTOPO PIC  X(0019).
000305*    -------------------------------
000306     05  FILLER            PIC  X(0003).
000307     05  OBVYNO PIC  X(0001).
000308*    -------------------------------
000309     05  FILLER            PIC  X(0003).
000310     05  FINDATEO PIC  99B99B99.
000311*    -------------------------------
000312     05  FILLER            PIC  X(0003).
000313     05  STFINLO PIC  X(0019).
000314*    -------------------------------
000315     05  FILLER            PIC  X(0003).
000316     05  OBUIDO PIC  ZZZ99.
000317*    -------------------------------
000318     05  FILLER            PIC  X(0003).
000319     05  FINLACTO PIC  X(0001).
000320*    -------------------------------
000321     05  FILLER            PIC  X(0003).
000322     05  COMMENTO PIC  X(0069).
000323*    -------------------------------
000324     05  FILLER            PIC  X(0003).
000325     05  ERRMSG1O PIC  X(0079).
000326*    -------------------------------
000327     05  FILLER            PIC  X(0003).
000328     05  ERRMSG2O PIC  X(0079).
000329*    -------------------------------
      *<<((file: EL691S))
000471
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
000473 01  DFHCOMMAREA                 PIC X(1024).
000474*                                COPY ERCARCH.
      *>>((file: ERCARCH))
000001******************************************************************
000002*                                                                *
000003*                            ERCARCH.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.002                          *
000006*                                                                *
000007*   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 250  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ERARCH                        RKP=2,LEN=5     *
000013*     ALTERNATE PATH1 = ERARCH2 (CERT/RESP)      RKP=07,LEN=35   *
000014*     ALTERNATE PATH2 = ERARCH3 (FORM NUMBER)    RKP=44,LEN=28   *
000015*     ALTERNATE PATH3 = ERARCH4 (PROCCESSOR ID)  RKP=73,LEN=28   *
000016*     ALTERNATE PATH4 = ERARCH5 (ACCOUNT KEY)    RKP=100,LEN=24  *
000017*     ALTERNATE PATH5 = ERARCH6 (BTCH/CHK KEY)   RKP=124,LEN=11  *
000018*                                                                *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020******************************************************************
000021*                   C H A N G E   L O G
000022*
000023* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000024*-----------------------------------------------------------------
000025*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000026* EFFECTIVE    NUMBER
000027*-----------------------------------------------------------------
000028* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
000029* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
000030* 110612    2012101700002  AJRA  ADD NEW FIELDS
000031* 102918  CR2018080300002  PEMA  ADD ONBASE STUFF
000032******************************************************************
000033 01  LETTER-ARCHIVE.
000034     12  LA-RECORD-ID                PIC  X(02).
000035         88  LA-VALID-ID                VALUE 'LA'.
000036
000037     12  LA-CONTROL-PRIMARY.
000038         16  LA-COMPANY-CD           PIC  X(01).
000039         16  LA-ARCHIVE-NO           PIC S9(08)    COMP.
000040
000041     12  LA-CONTROL-BY-CERT-RESP.
000042         16  LA-COMPANY-CD-A2        PIC  X(01).
000043         16  LA-CERT-NO-A2.
000044             20  LA-CERT-PRIME-A2    PIC  X(10).
000045             20  LA-CERT-SUFFIX-A2   PIC  X(01).
000046         16  LA-RSP-PERSON-A2 REDEFINES LA-CERT-NO-A2.
000047             20  LA-RESP-PERSON-A2   PIC  X(10).
000048             20  LA-TYPE-A2          PIC  X(01).
000049         16  LA-CARRIER-A2           PIC  X(01).
000050         16  LA-GROUPING-A2          PIC  X(06).
000051         16  LA-STATE-A2             PIC  X(02).
000052         16  LA-ACCOUNT-A2           PIC  X(10).
000053         16  LA-EFFECT-DATE-A2       PIC  X(02).
000054         16  LA-ARCHIVE-NO-A2        PIC S9(08)    COMP.
000055
000056     12  LA-CONTROL-BY-FORM.
000057         16  LA-COMPANY-CD-A3        PIC  X(01).
000058         16  LA-FORM-A3              PIC  X(04).
000059         16  LA-CARRIER-A3           PIC  X(01).
000060         16  LA-GROUPING-A3          PIC  X(06).
000061         16  LA-STATE-A3             PIC  X(02).
000062         16  LA-ACCOUNT-A3           PIC  X(10).
000063         16  LA-ARCHIVE-NO-A3        PIC S9(08)    COMP.
000064
000065     12  LA-CONTROL-BY-PROCESSOR.
000066         16  LA-COMPANY-CD-A4        PIC  X(01).
000067         16  LA-PROCESSOR-CD         PIC  X(04).
000068         16  LA-CARRIER-A4           PIC  X(01).
000069         16  LA-GROUPING-A4          PIC  X(06).
000070         16  LA-STATE-A4             PIC  X(02).
000071         16  LA-ACCOUNT-A4           PIC  X(10).
000072         16  LA-ARCHIVE-NO-A4        PIC S9(08)    COMP.
000073
000074     12  LA-CONTROL-BY-KEY-FIELDS.
000075         16  LA-COMPANY-CD-A5        PIC  X(01).
000076         16  LA-CARRIER-A5           PIC  X(01).
000077         16  LA-GROUPING-A5          PIC  X(06).
000078         16  LA-STATE-A5             PIC  X(02).
000079         16  LA-ACCOUNT-A5           PIC  X(10).
000080         16  LA-ARCHIVE-NO-A5        PIC S9(08)    COMP.
000081
000082     12  LA-CONTROL-BY-GROUP-CODE.
000083         16  LA-COMPANY-CD-A6        PIC  X(01).
000084         16  LA-ENTRY-A6.
000085             20  LA-FILLER           PIC  X(02).
000086             20  LA-QUE-CONTROL-A6   PIC S9(08)    COMP.
000087         16  LA-ARCHIVE-NO-A6        PIC S9(08)    COMP.
000088
000089     12  FILLER                      PIC  X(09).
000090
000091     12  LA-HEADER-RECORD.
000092         16  LA-NUMBER-LABEL-LINES   PIC S9(04)    COMP.
000093         16  LA-CREATION-DATE        PIC  X(02).
000094         16  LA-FOLLOW-UP-DATE       PIC  X(02).
000095         16  LA-FINAL-ACT-DATE       REDEFINES
000096               LA-FOLLOW-UP-DATE     PIC  X(02).
000097         16  LA-INITIAL-PRINT-DATE   PIC  X(02).
000098         16  LA-NO-OF-COPIES         PIC S9(01).
000099         16  LA-NO-OF-TEXT-RECORDS   PIC S9(04)    COMP.
000100         16  LA-REPLY-DATE           PIC  X(02).
000101         16  LA-RESEND-DATES.
000102             20  LA-RESEND-DATE      PIC  X(02).
000103             20  LA-SENT-DATE        PIC  X(02).
000104             20  FILLER              PIC  X(08).
000105         16  LA-SOURCE-INFORMATION.
000106             20  LA-DATA-SOURCE      PIC  X(01).
000107             20  LA-ADDR-SOURCE      PIC  X(01).
000108         16  LA-STATUS               PIC  X(01).
000109             88  LA-STATUS-ACTIVE         VALUE 'A'.
000110             88  LA-STATUS-COMPLETED      VALUE 'C'.
000111             88  LA-STATUS-ON-HOLD        VALUE 'H'.
000112             88  LA-STATUS-TO-BE-PURGED   VALUE 'X'.
000113             88  LA-STATUS-PURGED         VALUE 'P'.
000114             88  LA-STATUS-VOIDED         VALUE 'V'.
000115         16  LA-LAST-RESENT-PRINT-DATE
000116                                     PIC  X(02).
000117         16  LA-PRINT-RESTRICTION    PIC  X(01).
000118             88  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
000119                                          VALUE 'C'.
000120             88  LA-PRINT-ONLY-WHEN-FORM-GIVEN
000121                                          VALUE 'F'.
000122             88  LA-PRINT-ONLY-WHEN-PROC-GIVEN
000123                                          VALUE 'P'.
000124         16  LA-PURGED-DATE          PIC  X(02).
000125         16  LA-VOIDED-DATE          PIC  X(02).
000126         16  LA-RESEND-LETR          PIC  X(4).
000127         16  LA-VOID-ONBASE-YN       PIC  X.
000128         16  LA-ONBASE-UNIQUE-ID     PIC S9(5) COMP-3.
000129         16  FILLER                  PIC  X(04).
000130*        16  LA-RESEND-LETR-2        PIC  X(4).
000131*        16  LA-RESEND-LETR-3        PIC  X(4).
000132*        16  FILLER                  PIC  X(59).
000133         16  LA-ARCHIVE-STATUS       PIC  X.
000134             88  LA-TEMP                VALUE 'T'.
000135             88  LA-QWS                 VALUE 'Q'.
000136             88  LA-BATCH               VALUE 'B'.
000137         16  LA-FINAL-ACT-IND        PIC  X(1).
000138         16  LA-VA-DISCLOSURE-IND    PIC  X(1).
000139         16  LA-ENDT-ARCH-NO         PIC S9(8) COMP.
000140         16  LA-ENDT-ARCH-NO-X REDEFINES LA-ENDT-ARCH-NO
000141                                     PIC X(4).
000142         16  FILLER                  PIC  X(42).
000143*        16  FILLER                  PIC  X(71).
000144         16  LA-LAST-MAINT-DATE      PIC  X(2).
000145         16  LA-LAST-MAINT-TIME      PIC S9(6) COMP-3.
000146         16  LA-LAST-MAINT-TIMEX  REDEFINES LA-LAST-MAINT-TIME
000147                                     PIC  X(4).
000148         16  LA-LAST-UPDATED-BY      PIC  X(4).
000149
000150******************************************************************
      *<<((file: ERCARCH))
000475*                                COPY ERCARCT.
      *>>((file: ERCARCT))
000001******************************************************************
000002*                                                                *
000003*                            ERCARCT.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.002                          *
000006*                                                                *
000007*   FILE DESCRIPTION = TEXT OF ARCHIVED LETTERDS                 *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 1640  RECFORM = FIXED                          *
000011*                                                                *
000012*   BASE CLUSTER = ERARCT                        RKP=2,LEN=8     *
000013*                                                                *
000014*   LOG = NO                                                     *
000015*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000016******************************************************************
000017*                   C H A N G E   L O G
000018*
000019* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000020*-----------------------------------------------------------------
000021*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000022* EFFECTIVE    NUMBER
000023*-----------------------------------------------------------------
000024* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
000025******************************************************************
000026 01  LETTER-ARCHIVE-TEXT.
000027     12  LT-RECORD-ID                PIC  X(02).
000028         88  LT-VALID-ID                VALUE 'LT'.
000029
000030     12  LT-CONTROL-PRIMARY.
000031         16  LT-COMPANY-CD           PIC  X(01).
000032         16  LT-ARCHIVE-NO           PIC S9(08)    COMP.
000033         16  LT-RECORD-TYPE          PIC  X(01).
000034             88  LT-ADDRESS-DATA        VALUE '1'.
000035             88  LT-TEXT-DATA           VALUE '2'.
000036             88  LT-COMMENT-DATA        VALUE '3'.
000037         16  LT-LINE-SEQ-NO          PIC S9(04)    COMP.
000038
000039     12  FILLER                      PIC  X(28).
000040     12  LT-NUM-LINES-ON-RECORD      PIC S9(04)    COMP.
000041
000042     12  LT-TEXT-RECORD.
000043         16  LT-LETTER-TEXT OCCURS 20 TIMES
000044                            INDEXED BY LT-NDX.
000045             20  LT-TEXT-LINE        PIC  X(70).
000046             20  LT-SKIP-CONTROL     PIC  X(02).
000047                 88  LT-NO-LINES-SKIPPED             VALUE SPACES.
000048                 88  LT-SKIP-TO-NEXT-PAGE            VALUE '99'.
000049             20  FILLER              PIC  X(08).
000050
000051     12  LT-COMMENT-RECORD  REDEFINES LT-TEXT-RECORD.
000052         16  LT-LETTER-COMMENT OCCURS 20 TIMES INDEXED BY LC-NDX.
000053             20  LT-COMMENT-LINE     PIC X(69).
000054             20  LT-COMMENT-CHG-DT   PIC X(02).
000055             20  LT-COMMENT-CHG-BY   PIC X(04).
000056             20  FILLER              PIC X(05).
000057
      *<<((file: ERCARCT))
000476*                                COPY ELCCERT.
      *>>((file: ELCCERT))
000001******************************************************************
000002*                                                                *
000003*                            ELCCERT.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.013                          *
000006*                                                                *
000007*   FILE DESCRIPTION = CERTIFICATE MASTER                        *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 450  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
000013*       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
000014*       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
000015*       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
000016*       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
000017*                                                                *
000018*   LOG = YES                                                    *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020******************************************************************
000021*                   C H A N G E   L O G
000022*
000023* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000024*-----------------------------------------------------------------
000025*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000026* EFFECTIVE    NUMBER
000027*-----------------------------------------------------------------
000028* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
000029* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
000030* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
000031* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
000032* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
000033* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
000034* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000035* 032612  CR2011110200001  PEMA  AHL CHANGES
000036* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
000037* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000038* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
000039******************************************************************
000040
000041 01  CERTIFICATE-MASTER.
000042     12  CM-RECORD-ID                      PIC XX.
000043         88  VALID-CM-ID                      VALUE 'CM'.
000044
000045     12  CM-CONTROL-PRIMARY.
000046         16  CM-COMPANY-CD                 PIC X.
000047         16  CM-CARRIER                    PIC X.
000048         16  CM-GROUPING.
000049             20  CM-GROUPING-PREFIX        PIC X(3).
000050             20  CM-GROUPING-PRIME         PIC X(3).
000051         16  CM-STATE                      PIC XX.
000052         16  CM-ACCOUNT.
000053             20  CM-ACCOUNT-PREFIX         PIC X(4).
000054             20  CM-ACCOUNT-PRIME          PIC X(6).
000055         16  CM-CERT-EFF-DT                PIC XX.
000056         16  CM-CERT-NO.
000057             20  CM-CERT-PRIME             PIC X(10).
000058             20  CM-CERT-SFX               PIC X.
000059
000060     12  CM-CONTROL-BY-NAME.
000061         16  CM-COMPANY-CD-A1              PIC X.
000062         16  CM-INSURED-LAST-NAME          PIC X(15).
000063         16  CM-INSURED-INITIALS.
000064             20  CM-INSURED-INITIAL1       PIC X.
000065             20  CM-INSURED-INITIAL2       PIC X.
000066
000067     12  CM-CONTROL-BY-SSN.
000068         16  CM-COMPANY-CD-A2              PIC X.
000069         16  CM-SOC-SEC-NO.
000070             20  CM-SSN-STATE              PIC XX.
000071             20  CM-SSN-ACCOUNT            PIC X(6).
000072             20  CM-SSN-LN3.
000073                 25  CM-INSURED-INITIALS-A2.
000074                     30 CM-INSURED-INITIAL1-A2   PIC X.
000075                     30 CM-INSURED-INITIAL2-A2   PIC X.
000076                 25 CM-PART-LAST-NAME-A2         PIC X.
000077
000078     12  CM-CONTROL-BY-CERT-NO.
000079         16  CM-COMPANY-CD-A4              PIC X.
000080         16  CM-CERT-NO-A4                 PIC X(11).
000081
000082     12  CM-CONTROL-BY-MEMB.
000083         16  CM-COMPANY-CD-A5              PIC X.
000084         16  CM-MEMBER-NO.
000085             20  CM-MEMB-STATE             PIC XX.
000086             20  CM-MEMB-ACCOUNT           PIC X(6).
000087             20  CM-MEMB-LN4.
000088                 25  CM-INSURED-INITIALS-A5.
000089                     30 CM-INSURED-INITIAL1-A5   PIC X.
000090                     30 CM-INSURED-INITIAL2-A5   PIC X.
000091                 25 CM-PART-LAST-NAME-A5         PIC XX.
000092
000093     12  CM-INSURED-PROFILE-DATA.
000094         16  CM-INSURED-FIRST-NAME.
000095             20  CM-INSURED-1ST-INIT       PIC X.
000096             20  FILLER                    PIC X(9).
000097         16  CM-INSURED-ISSUE-AGE          PIC 99.
000098         16  CM-INSURED-SEX                PIC X.
000099             88  CM-SEX-MALE                  VALUE 'M'.
000100             88  CM-SEX-FEMAL                 VALUE 'F'.
000101         16  CM-INSURED-JOINT-AGE          PIC 99.
000102         16  CM-JOINT-INSURED-NAME.
000103             20  CM-JT-LAST-NAME           PIC X(15).
000104             20  CM-JT-FIRST-NAME.
000105                 24  CM-JT-1ST-INIT        PIC X.
000106                 24  FILLER                PIC X(9).
000107             20  CM-JT-INITIAL             PIC X.
000108
000109     12  CM-LIFE-DATA.
000110         16  CM-LF-BENEFIT-CD              PIC XX.
000111         16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
000112         16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
000113         16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
000114         16  CM-LF-DEV-CODE                PIC XXX.
000115         16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
000116         16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
000117         16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
000118         16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
000119         16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000120         16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000121         16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
000122         16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000123         16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
000124         16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
000125         16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
000126         16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
000127         16  cm-temp-epiq                  pic xx.
000128             88  EPIQ-CLASS                  value 'EQ'.
000129*        16  FILLER                        PIC XX.
000130
000131     12  CM-AH-DATA.
000132         16  CM-AH-BENEFIT-CD              PIC XX.
000133         16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
000134         16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
000135         16  CM-AH-DEV-CODE                PIC XXX.
000136         16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
000137         16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
000138         16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
000139         16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000140         16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000141         16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
000142         16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
000143         16  CM-AH-PAID-THRU-DT            PIC XX.
000144             88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
000145         16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
000146         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
000147         16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
000148         16  FILLER                        PIC X.
000149
000150     12  CM-LOAN-INFORMATION.
000151         16  CM-LIVES                      PIC S9(7)     COMP-3.
000152         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
000153                                           PIC S9(5)V99  COMP-3.
000154         16  CM-BILLED                     PIC S9(7)     COMP-3.
000155         16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
000156         16  CM-PAY-FREQUENCY              PIC S99.
000157         16  CM-LOAN-TERM                  PIC S999      COMP-3.
000158         16  CM-RATE-CLASS                 PIC XX.
000159         16  CM-BENEFICIARY                PIC X(25).
000160         16  CM-POLICY-FORM-NO             PIC X(12).
000161         16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
000162         16  CM-LAST-ADD-ON-DT             PIC XX.
000163         16  CM-DEDUCTIBLE-AMOUNTS.
000164             20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
000165             20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
000166         16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
000167             20  CM-RESIDENT-STATE         PIC XX.
000168             20  CM-RATE-CODE              PIC X(4).
000169             20  FILLER                    PIC XX.
000170         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
000171             20  CM-LOAN-OFFICER           PIC X(5).
000172             20  FILLER                    PIC XXX.
000173         16  CM-CSR-CODE                   PIC XXX.
000174         16  CM-UNDERWRITING-CODE          PIC X.
000175             88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
000176         16  CM-POST-CARD-IND              PIC X.
000177         16  CM-REF-INTERFACE-SW           PIC X.
000178         16  CM-PREMIUM-TYPE               PIC X.
000179             88  CM-SING-PRM                  VALUE '1'.
000180             88  CM-O-B-COVERAGE              VALUE '2'.
000181             88  CM-OPEN-END                  VALUE '3'.
000182         16  CM-IND-GRP-TYPE               PIC X.
000183             88  CM-INDIVIDUAL                VALUE 'I'.
000184             88  CM-GROUP                     VALUE 'G'.
000185         16  CM-SKIP-CODE                  PIC X.
000186             88  NO-MONTHS-SKIPPED            VALUE SPACE.
000187             88  SKIP-JULY                    VALUE '1'.
000188             88  SKIP-AUGUST                  VALUE '2'.
000189             88  SKIP-SEPTEMBER               VALUE '3'.
000190             88  SKIP-JULY-AUG                VALUE '4'.
000191             88  SKIP-AUG-SEPT                VALUE '5'.
000192             88  SKIP-JULY-AUG-SEPT           VALUE '6'.
000193             88  SKIP-JUNE-JULY-AUG           VALUE '7'.
000194             88  SKIP-JUNE                    VALUE '8'.
000195             88  SKIP-JUNE-JULY               VALUE '9'.
000196             88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
000197             88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
000198         16  CM-PAYMENT-MODE               PIC X.
000199             88  PAY-MONTHLY                  VALUE SPACE.
000200             88  PAY-WEEKLY                   VALUE '1'.
000201             88  PAY-SEMI-MONTHLY             VALUE '2'.
000202             88  PAY-BI-WEEKLY                VALUE '3'.
000203             88  PAY-SEMI-ANUALLY             VALUE '4'.
000204         16  CM-LOAN-NUMBER                PIC X(8).
000205         16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
000206         16  CM-OLD-LOF                    PIC XXX.
000207*        16  CM-LOAN-OFFICER               PIC XXX.
000208         16  CM-REIN-TABLE                 PIC XXX.
000209         16  CM-SPECIAL-REIN-CODE          PIC X.
000210         16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
000211         16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
000212         16  CM-LOAN-1ST-PMT-DT            PIC XX.
000213
000214     12  CM-STATUS-DATA.
000215         16  CM-ENTRY-STATUS               PIC X.
000216         16  CM-ENTRY-DT                   PIC XX.
000217
000218         16  CM-LF-STATUS-AT-CANCEL        PIC X.
000219         16  CM-LF-CANCEL-DT               PIC XX.
000220         16  CM-LF-CANCEL-EXIT-DT          PIC XX.
000221
000222         16  CM-LF-STATUS-AT-DEATH         PIC X.
000223         16  CM-LF-DEATH-DT                PIC XX.
000224         16  CM-LF-DEATH-EXIT-DT           PIC XX.
000225
000226         16  CM-LF-CURRENT-STATUS          PIC X.
000227             88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
000228                                                'M' '4' '5' '9'.
000229             88  CM-LF-NORMAL-ENTRY           VALUE '1'.
000230             88  CM-LF-POLICY-PENDING         VALUE '2'.
000231             88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
000232             88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
000233             88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
000234             88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
000235             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
000236             88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
000237             88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
000238             88  CM-LF-CANCEL-APPLIED         VALUE '8'.
000239             88  CM-LF-IS-REIN-ONLY           VALUE '9'.
000240             88  CM-LF-DECLINED               VALUE 'D'.
000241             88  CM-LF-VOIDED                 VALUE 'V'.
000242
000243         16  CM-AH-STATUS-AT-CANCEL        PIC X.
000244         16  CM-AH-CANCEL-DT               PIC XX.
000245         16  CM-AH-CANCEL-EXIT-DT          PIC XX.
000246
000247         16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
000248         16  CM-AH-SETTLEMENT-DT           PIC XX.
000249         16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
000250
000251         16  CM-AH-CURRENT-STATUS          PIC X.
000252             88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
000253                                                'M' '4' '5' '9'.
000254             88  CM-AH-NORMAL-ENTRY           VALUE '1'.
000255             88  CM-AH-POLICY-PENDING         VALUE '2'.
000256             88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
000257             88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
000258             88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
000259             88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
000260             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
000261             88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
000262             88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
000263             88  CM-AH-CANCEL-APPLIED         VALUE '8'.
000264             88  CM-AH-IS-REIN-ONLY           VALUE '9'.
000265             88  CM-AH-DECLINED               VALUE 'D'.
000266             88  CM-AH-VOIDED                 VALUE 'V'.
000267
000268         16  CM-CLAIM-INTERFACE-SW         PIC X.
000269             88  NO-CLAIM-ATTACHED            VALUE SPACE.
000270             88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
000271             88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
000272         16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
000273
000274         16  CM-ENTRY-BATCH                PIC X(6).
000275         16  CM-LF-EXIT-BATCH              PIC X(6).
000276         16  CM-AH-EXIT-BATCH              PIC X(6).
000277         16  CM-LAST-MONTH-END             PIC XX.
000278
000279     12  CM-NOTE-SW                        PIC X.
000280         88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
000281         88  CERT-NOTES-PRESENT               VALUE '1'.
000282         88  BILLING-NOTES-PRESENT            VALUE '2'.
000283         88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
000284         88  CLAIM-NOTES-PRESENT              VALUE '4'.
000285         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
000286         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
000287         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
000288     12  CM-COMP-EXCP-SW                   PIC X.
000289         88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
000290         88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
000291     12  CM-INSURED-ADDRESS-SW             PIC X.
000292         88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
000293         88  INSURED-ADDR-PRESENT             VALUE '1'.
000294
000295*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
000296     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
000297     12  FILLER                            PIC X.
000298
000299*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
000300     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
000301     12  FILLER                            PIC X.
000302*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
000303     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
000304
000305     12  CM-CREDIT-INTERFACE-SW-1          PIC X.
000306         88  CERT-ADDED-BATCH                 VALUE ' '.
000307         88  CERT-ADDED-ONLINE                VALUE '1'.
000308         88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
000309         88  CERT-PURGED-OFFLINE              VALUE '3'.
000310         88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
000311     12  CM-CREDIT-INTERFACE-SW-2          PIC X.
000312         88  CERT-AS-LOADED                   VALUE ' '.
000313         88  CERT-CANCELLED-ONLINE            VALUE '1'.
000314         88  CERT-CLAIM-ONLINE                VALUE '2'.
000315         88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
000316         88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
000317         88  CERT-PEND-CANCEL-VOID            VALUE '5'.
000318         88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
000319         88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
000320
000321     12  CM-ACCOUNT-COMM-PCTS.
000322         16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
000323         16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
000324
000325     12  CM-USER-FIELD                     PIC X.
000326     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
000327     12  CM-CLP-STATE                      PIC XX.
000328     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
000329     12  CM-USER-RESERVED                  PIC XXX.
000330     12  FILLER REDEFINES CM-USER-RESERVED.
000331         16  CM-AH-CLASS-CD                PIC XX.
000332         16  F                             PIC X.
000333******************************************************************
      *<<((file: ELCCERT))
000477*                                COPY ELCEOBC.
      *>>((file: ELCEOBC))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCEOBC                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   CLAIM SYSTEM EOB CODE TABLE                                  *
000008*                                                                *
000009*   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
000010*   VSAM EOB CODE TABLE                                          *
000011*                                                                *
000012*   FILE DESCRIPTION = EOB CODE TABLE                            *
000013*                                                                *
000014*   FILE TYPE = VSAM,KSDS                                        *
000015*   RECORD SIZE = 350   RECFORM = FIX                            *
000016*                                                                *
000017*   BASE CLUSTER NAME = ELEOBC                    RKP=2,LEN=15   *
000018*                                                                *
000019*   LOG = NO                                                     *
000020*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000021*                                                                *
000022*                                                                *
000023******************************************************************
000024*                   C H A N G E   L O G
000025*
000026* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000027*-----------------------------------------------------------------
000028*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000029* EFFECTIVE    NUMBER
000030*-----------------------------------------------------------------
000031* 120808    2008100900001  PEMA  NEW COPYBOOK/FILE
000032* 081511    2011022800001  PEMA  CHG FOR ADMIN SERV NAPERSOFT
000033* 091913    2013090300001  AJRA  ADDITIONAL RECORD TYPES
000034******************************************************************
000035
000036 01  EOB-CODES.
000037     12  EO-RECORD-ID                      PIC XX.
000038         88  VALID-DN-ID                      VALUE 'EO'.
000039
000040     12  EO-CONTROL-PRIMARY.
000041         16  EO-COMPANY-CD                 PIC X.
000042         16  EO-RECORD-TYPE                PIC X.
000043             88  EO-EOB-RECS                  VALUE '1'.
000044             88  EO-VERIF-RECS                VALUE '2'.
000045             88  EO-GCE-RECS                  VALUE '3'.
000046             88  EO-CANC-RECS                 VALUE '4'.
000047             88  EO-BILL-NOTE-RECS            VALUE '5'.
000048         16  EO-EOB-CODE                   PIC X(4).
000049         16  FILLER                        PIC X(9).
000050
000051     12  EO-MAINT-INFORMATION.
000052         16  EO-LAST-MAINT-DT              PIC XX.
000053         16  EO-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000054         16  EO-LAST-MAINT-USER            PIC X(4).
000055         16  FILLER                        PIC XX.
000056
000057     12  EO-DESCRIPTION                    PIC X(275).
000058     12  FILLER                            PIC X(46).
000059******************************************************************
      *<<((file: ELCEOBC))
000478*                                COPY ERCNOTE.
      *>>((file: ERCNOTE))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCNOTE                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*        FILE DESCRIPTION = CERTIFICATE AND BILLING NOTES        *
000009*                                                                *
000010*        FILE TYPE= VSAM,KSDS                                    *
000011*        RECORD SIZE = 825    RECFORM = FIXED                    *
000012*                                                                *
000013*        BASE CLUSTER = ERNOTE        RKP=2,LEN=34               *
000014*                                                                *
000015*        LOG = YES                                               *
000016*        SERVREQ = DELETE,UPDATE,NEWREC                          *
000017*                                                                *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 091509  CR2008100900003  AJRA  CERT NOTES MOVED TO NEW FILE. THI
000027*                                FILE WILL CONTAIN BILLING NOTES O
000028* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
000029******************************************************************
000030
000031 01  CERTIFICATE-NOTE.
000032     12  CN-RECORD-ID                PIC  XX.
000033         88  VALID-CN-ID                  VALUE 'CN'.
000034
000035     12  CN-CONTROL-PRIMARY.
000036         16  CN-COMPANY-CD           PIC X.
000037         16  CN-CARRIER              PIC X.
000038         16  CN-GROUPING.
000039             20 CN-GROUPING-PREFIX   PIC XXX.
000040             20 CN-GROUPING-PRIME    PIC XXX.
000041         16  CN-STATE                PIC XX.
000042         16  CN-ACCOUNT.
000043             20 CN-ACCOUNT-PREFIX    PIC X(4).
000044             20 CN-ACCOUNT-PRIME     PIC X(6).
000045         16  CN-CERT-EFF-DT          PIC XX.
000046         16  CN-CERT-NO.
000047             20  CN-CERT-PRIME       PIC X(10).
000048             20  CN-CERT-SFX         PIC X.
000049         16  CN-RECORD-TYPE          PIC X.
000050             88  CN-ISSUE-BILLING-NOTE    VALUE '1'.
000051             88  CN-CANCEL-BILLING-NOTE   VALUE '2'.
000052     12  CN-BILLING-START-LINE-NO    PIC 99.
000053     12  CN-BILLING-END-LINE-NO      PIC 99.
000054
000055     12  CN-LINES.
000056         16  CN-LINE OCCURS 10       PIC X(77).
000057
000058     12  CN-LAST-MAINT-DT            PIC XX.
000059     12  CN-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
000060     12  CN-LAST-MAINT-USER          PIC X(4).
000061     12  FILLER                      PIC X(5).
000062******************************************************************
      *<<((file: ERCNOTE))
000479 01  var                         pic x(30).
000480
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL691' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000481 VCOBOL-DUMMY-PROCEDURE.
000482     MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
000483
000484     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000485     MOVE '5'                    TO DC-OPTION-CODE.
000486     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
000487     MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.
000488     MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE
000489                                    W-CURRENT-SAVE.
000490
000491     MOVE 2                      TO EMI-NUMBER-OF-LINES.
000492     MOVE ERROR-MESSAGE-INTERFACE-BLOCK
000493                                 TO EMI-SAVE-AREA.
000494
000495     IF  EIBCALEN EQUAL 0
000496         MOVE UNACCESS-MSG       TO LOGOFF-MSG
000497         GO TO 8300-SEND-TEXT
000498     END-IF.
000499
000500     set P to address of KIXSYS
000501     CALL "getenv" using by value P returning var-ptr
000502     if var-ptr = null then
000503        display ' kixsys not set '
000504     else
000505        set address of var to var-ptr
000506        move 0 to env-var-len
000507        inspect var tallying env-var-len
000508          for characters before X'00'
000509        unstring var (1:env-var-len) delimited by '/'
000510           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000511              WS-KIX-SYS
000512        end-unstring
000513     end-if
000514
000515     IF  PI-CALLING-PROGRAM NOT EQUAL W-THIS-PGM
000516         IF  PI-RETURN-TO-PROGRAM NOT EQUAL W-THIS-PGM
000517             MOVE PI-SAVED-PROGRAM-5
000518                                 TO PI-SAVED-PROGRAM-6
000519             MOVE PI-SAVED-PROGRAM-4
000520                                 TO PI-SAVED-PROGRAM-5
000521             MOVE PI-SAVED-PROGRAM-3
000522                                 TO PI-SAVED-PROGRAM-4
000523             MOVE PI-SAVED-PROGRAM-2
000524                                 TO PI-SAVED-PROGRAM-3
000525             MOVE PI-SAVED-PROGRAM-1
000526                                 TO PI-SAVED-PROGRAM-2
000527             MOVE PI-RETURN-TO-PROGRAM
000528                                 TO PI-SAVED-PROGRAM-1
000529             MOVE PI-CALLING-PROGRAM
000530                                 TO PI-RETURN-TO-PROGRAM
000531             MOVE W-THIS-PGM     TO PI-CALLING-PROGRAM
000532         ELSE
000533             MOVE PI-CALLING-PROGRAM TO W-CALL-PGM
000534             MOVE PI-RETURN-TO-PROGRAM
000535                                     TO PI-CALLING-PROGRAM
000536             MOVE PI-SAVED-PROGRAM-1 TO PI-RETURN-TO-PROGRAM
000537             MOVE PI-SAVED-PROGRAM-2 TO PI-SAVED-PROGRAM-1
000538             MOVE PI-SAVED-PROGRAM-3 TO PI-SAVED-PROGRAM-2
000539             MOVE PI-SAVED-PROGRAM-4 TO PI-SAVED-PROGRAM-3
000540             MOVE PI-SAVED-PROGRAM-5 TO PI-SAVED-PROGRAM-4
000541             MOVE PI-SAVED-PROGRAM-6 TO PI-SAVED-PROGRAM-5
000542             MOVE SPACES             TO PI-SAVED-PROGRAM-6
000543         END-IF
000544     ELSE
000545         GO TO 0200-RECEIVE
000546     END-IF.
000547
000548     GO TO 1000-SHOW.
000549
000550                                 EJECT
000551
000552 0200-RECEIVE.
000553
000554     
      * EXEC CICS HANDLE AID
000555*        CLEAR    (9300-DFHCLEAR)
000556*        PA1      (9200-PA)
000557*        PA2      (9200-PA)
000558*        PA3      (9200-PA)
000559*    END-EXEC.
      *    MOVE '"&=!"#               V! " #00002845' TO DFHEIV0
           MOVE X'22263D212223202020202020' &
                X'202020202020202020562120' &
                X'2220233030303032383435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000560
000561     
      * EXEC CICS HANDLE CONDITION
000562*        PGMIDERR (9700-PGMID-ERROR)
000563*        ERROR    (9800-ABEND)
000564*    END-EXEC.
      *    MOVE '"$L.                  ! # #00002852' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303032383532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000565
000566     
      * EXEC CICS RECEIVE
000567*        MAP      (W-MAP)
000568*        MAPSET   (W-MAPSET)
000569*        INTO     (EL691AI)
000570*    END-EXEC.
           MOVE LENGTH OF
            EL691AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002857' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303032383537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL691AI, 
                 DFHEIV11, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000571
000572     IF  NOT DISPLAY-CAP
000573         MOVE 'READ'             TO SM-READ
000574         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000575         MOVE ER-9097            TO EMI-ERROR
000576         MOVE -1                 TO MAINTL
000577         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000578         GO TO 8100-SEND-INITIAL-MAP
000579     END-IF.
000580
000581
000582 0300-CHECK-PFKEYS.
000583
000584     IF  EIBAID EQUAL DFHPF23
000585         MOVE EIBAID             TO PI-ENTRY-CD-1
000586         MOVE W-XCTL-005         TO W-CALL-PGM
000587         GO TO 9400-XCTL
000588     END-IF.
000589
000590     IF  EIBAID EQUAL DFHPF24
000591         MOVE W-XCTL-626         TO W-CALL-PGM
000592         GO TO 9400-XCTL
000593     END-IF.
000594
000595     IF  EIBAID EQUAL DFHPF6
000596         move pi-689-chg-seq-nox(1:1)
000597                                 to PI-PROGRAM-WORK-AREA(1:1)
000598         MOVE W-XCTL-1279        TO W-CALL-PGM
000599         GO TO 9400-XCTL
000600     END-IF
000601
000602     IF  MAINTI EQUAL 'C'  AND
000603         EIBAID EQUAL DFHENTER
000604          GO TO 0700-PROCESS-CHANGES
000605     END-IF.
000606
000607     MOVE ER-0029                TO EMI-ERROR.
000608
000609
000610 0320-INPUT-ERROR.
000611
000612     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000613     MOVE -1                     TO  MAINTL.
000614     GO TO 8200-SEND-DATAONLY.
000615
000616
000617 0700-PROCESS-CHANGES.
000618
000619     IF  NOT MODIFY-CAP
000620         MOVE 'UPDATE'           TO SM-READ
000621         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000622         MOVE ER-0070            TO EMI-ERROR
000623         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000624         MOVE -1                 TO MAINTL
000625         GO TO 8100-SEND-INITIAL-MAP
000626     END-IF.
000627
000628     IF RESFORML > ZEROS
000629        AND RESFORMI > SPACES
000630        MOVE PI-COMPANY-CD       TO LETR-COMPANY-CD
000631        MOVE RESFORMI            TO LETR-LETTER-ID
000632        MOVE LETR-PART-KEY       TO ELLETR-SAVE-PART-KEY
000633        MOVE SPACES              TO LETR-FILLER
000634        MOVE 0                   TO LETR-SEQ-NO
000635        PERFORM 1500-CHECK-Z-RECORD THRU 1500-EXIT
000636        IF Z-RECORD-NOT-FOUND
000637           MOVE ER-1236          TO EMI-ERROR
000638           MOVE -1               TO RESFORML
000639           MOVE AL-UABON         TO RESFORMA
000640           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000641        END-IF
000642     END-IF
000643
000644     MOVE 'N'                    TO W-NEED-COMMENT.
000645
000646     IF RESENDL GREATER ZERO
000647         MOVE 'Y'                TO W-NEED-COMMENT
000648         IF RESENDI = SPACES
000649             MOVE AL-UANON       TO  RESENDA
000650             MOVE +1             TO  W-UPDATE-SW
000651             MOVE LOW-VALUES     TO  W-RESEND-DATE
000652         ELSE
000653             MOVE RESENDI       TO  W-DEEDIT-FIELD
000654             PERFORM 8600-DEEDIT THRU 8600-EXIT
000655             IF W-DEEDIT-FIELD-V0 IS NUMERIC
000656                 MOVE W-DEEDIT-FIELD-V0  TO  RESENDO
000657                 INSPECT RESENDI CONVERTING SPACES TO SLASH
000658                 MOVE '4'        TO  DC-OPTION-CODE
000659                 MOVE W-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
000660                 PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
000661                 IF DC-ERROR-CODE NOT = SPACES
000662                     MOVE ER-0295         TO  EMI-ERROR
000663                     MOVE -1              TO  RESENDL
000664                     MOVE AL-UABON        TO  RESENDA
000665                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000666                 ELSE
000667                     MOVE AL-UANON       TO  RESENDA
000668                     MOVE +1             TO  W-UPDATE-SW
000669                     MOVE DC-BIN-DATE-1  TO  W-RESEND-DATE
000670                 END-IF
000671             ELSE
000672                 MOVE ER-0295    TO  EMI-ERROR
000673                 MOVE -1         TO  RESENDL
000674                 MOVE AL-UABON   TO  RESENDA
000675                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000676             END-IF
000677         END-IF
000678     END-IF.
000679
000680     IF FINDATEL GREATER ZERO
000681         IF FINDATEI = SPACES
000682             MOVE AL-UANON       TO  FINDATEA
000683             MOVE +1             TO  W-UPDATE-SW
000684             MOVE LOW-VALUES     TO  W-FINAL-ACT-DATE
000685             MOVE SPACES         TO  PI-ARCHIVE-FINAL
000686         ELSE
000687             MOVE 'Y'             TO  PI-ARCHIVE-FINAL
000688             MOVE FINDATEI        TO  W-DEEDIT-FIELD
000689             PERFORM 8600-DEEDIT THRU 8600-EXIT
000690             IF W-DEEDIT-FIELD-V0 IS NUMERIC
000691                 MOVE W-DEEDIT-FIELD-V0  TO  FINDATEO
000692                 INSPECT FINDATEI CONVERTING SPACES TO SLASH
000693                 MOVE '4'                 TO  DC-OPTION-CODE
000694                 MOVE W-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
000695                 PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
000696                 IF DC-ERROR-CODE NOT = SPACES
000697                     MOVE ER-0296        TO  EMI-ERROR
000698                     MOVE -1             TO  FINDATEL
000699                     MOVE AL-UABON       TO  FINDATEA
000700                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000701                 ELSE
000702                     MOVE AL-UANON       TO  FINDATEA
000703                     MOVE +1             TO  W-UPDATE-SW
000704                     MOVE DC-BIN-DATE-1  TO  W-FINAL-ACT-DATE
000705                 END-IF
000706             ELSE
000707                 MOVE ER-0296    TO  EMI-ERROR
000708                 MOVE -1         TO  FINDATEL
000709                 MOVE AL-UABON   TO  FINDATEA
000710                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000711             END-IF
000712         END-IF
000713     END-IF.
000714
000715     IF REPLYL GREATER ZERO
000716         IF REPLYI = SPACES
000717             MOVE AL-UANON       TO  REPLYA
000718             MOVE +1             TO  W-UPDATE-SW
000719             MOVE LOW-VALUES     TO  W-RECEIVED-DATE
000720             MOVE SPACES         TO  PI-ARCHIVE-RECEIVED
000721                                     WS-RECEIVED-NOTE
000722             PERFORM 4500-UPDATE-BILL-NOTE THRU 4500-EXIT
000723         ELSE
000724             MOVE 'Y'            TO  PI-ARCHIVE-RECEIVED
000725             MOVE REPLYI         TO  W-DEEDIT-FIELD
000726             PERFORM 8600-DEEDIT THRU 8600-EXIT
000727             IF W-DEEDIT-FIELD-V0 IS NUMERIC
000728                 MOVE W-DEEDIT-FIELD-V0  TO  REPLYO
000729                 INSPECT REPLYI CONVERTING SPACES TO SLASH
000730                 MOVE '4'        TO  DC-OPTION-CODE
000731                 MOVE W-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
000732                 PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
000733                 IF DC-ERROR-CODE NOT = SPACES
000734                     MOVE ER-9245        TO  EMI-ERROR
000735                     MOVE -1             TO  REPLYL
000736                     MOVE AL-UABON       TO  REPLYA
000737                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000738                 ELSE
000739                   IF DC-BIN-DATE-1 GREATER THAN W-SAVE-BIN-DATE
000740                       MOVE ER-0539      TO  EMI-ERROR
000741                       MOVE -1           TO  REPLYL
000742                       MOVE AL-UABON     TO  REPLYA
000743                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000744                   ELSE
000745                       MOVE AL-UANON     TO  REPLYA
000746                       MOVE +1           TO  W-UPDATE-SW
000747                       MOVE DC-BIN-DATE-1 TO W-RECEIVED-DATE
000748                       MOVE DC-GREG-DATE-1-EDIT TO
000749                                        W-CERT-NOTE-RECV-DT
000750                                        WS-BN-RECV-DATE
000751                   END-IF
000752                 END-IF
000753             ELSE
000754                 MOVE ER-9245    TO  EMI-ERROR
000755                 MOVE -1         TO  REPLYL
000756                 MOVE AL-UABON   TO  REPLYA
000757                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000758             END-IF
000759         END-IF
000760     END-IF.
000761
000762     IF STOPDTEL GREATER ZERO
000763         IF STOPDTEI = SPACES
000764             MOVE AL-UANON       TO  STOPDTEA
000765             MOVE +1             TO  W-UPDATE-SW
000766             MOVE LOW-VALUES     TO  W-STOP-LETTER-DATE
000767             MOVE SPACES         TO  PI-ARCHIVE-STOPPED
000768             MOVE AL-UANOF       TO  RESENDA
000769                                     RESFORMA
000770                                     FINDATEA
000771                                     FINLACTA
000772                                     REPLYA
000773                                     COMMENTA
000774         ELSE
000775             MOVE 'Y'            TO  PI-ARCHIVE-STOPPED
000776             MOVE 'Y'            TO  W-NEED-COMMENT
000777             MOVE STOPDTEI       TO  W-DEEDIT-FIELD
000778             PERFORM 8600-DEEDIT THRU 8600-EXIT
000779             IF W-DEEDIT-FIELD-V0 IS NUMERIC
000780                 MOVE W-DEEDIT-FIELD-V0  TO  STOPDTEO
000781                 INSPECT STOPDTEI CONVERTING SPACES TO SLASH
000782                 MOVE '4'        TO  DC-OPTION-CODE
000783                 MOVE W-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
000784                 PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
000785                 IF DC-ERROR-CODE NOT = SPACES
000786                     MOVE ER-0897         TO  EMI-ERROR
000787                     MOVE -1              TO  STOPDTEL
000788                     MOVE AL-UABON        TO  STOPDTEA
000789                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000790                 ELSE
000791                   IF DC-BIN-DATE-1 GREATER THAN W-SAVE-BIN-DATE
000792                       MOVE ER-0895      TO  EMI-ERROR
000793                       MOVE -1           TO  STOPDTEL
000794                       MOVE AL-UABON     TO  STOPDTEA
000795                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000796                   ELSE
000797                     MOVE AL-UANON       TO  STOPDTEA
000798                     MOVE +1             TO  W-UPDATE-SW
000799                     MOVE DC-BIN-DATE-1  TO  W-STOP-LETTER-DATE
000800                   END-IF
000801                 END-IF
000802             ELSE
000803                 MOVE ER-0897    TO  EMI-ERROR
000804                 MOVE -1         TO  STOPDTEL
000805                 MOVE AL-UABON   TO  STOPDTEA
000806                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000807             END-IF
000808         END-IF
000809     END-IF.
000810
000811     IF W-NEED-COMMENT = 'Y'  AND
000812        COMMENTL NOT > ZERO
000813           MOVE -1         TO  COMMENTL
000814           MOVE ER-7363    TO  EMI-ERROR
000815           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000816     END-IF.
000817
000818     if (stopdtel > zeros)
000819        and (PI-ARCHIVE-STOPPED = 'Y')
000820        and (pi-initial-print-date <> low-values)
000821        if obvyni not = 'Y' AND 'N'
000822           move -1               to obvynl
000823           move al-uabon         to obvyna
000824           move er-7364          to emi-error
000825           perform 9900-error-format
000826                                 thru 9900-exit
000827        end-if
000828     end-if
000829
000830     if obvynl > zeros
000831        and pi-archive-stopped <> 'Y'
000832        move -1                  to stopdtel
000833        move al-uabon            to stopdtea
000834        move er-7333             to emi-error
000835        perform 9900-error-format
000836                                 thru 9900-exit
000837     end-if
000838
000839     IF COMMENTL GREATER ZERO
000840      OR  RESFORML GREATER ZERO
000841      OR  FINLACTL GREATER ZERO
000842      or obvynl > 0
000843         MOVE +1          TO  W-UPDATE-SW
000844     END-IF.
000845
000846     IF W-ERROR-COUNT GREATER ZERO
000847         GO TO 8200-SEND-DATAONLY
000848     END-IF.
000849
000850     IF W-UPDATE-SW NOT GREATER ZERO
000851         MOVE ER-3112    TO  EMI-ERROR
000852         MOVE -1         TO  MAINTL
000853         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000854         GO TO 8200-SEND-DATAONLY
000855     END-IF.
000856
000857     IF PI-ARCHIVE-STOPPED = 'Y'
000858        MOVE AL-SANOF       TO RESENDA
000859                               RESFORMA
000860                               FINDATEA
000861                               FINLACTA
000862                               REPLYA
000863*                              COMMENTA
000864     END-IF.
000865
000866     PERFORM 3000-READ-FOR-UPDATE THRU 3000-EXIT.
000867
000868     IF RESENDL GREATER ZERO
000869         MOVE W-RESEND-DATE      TO  LA-RESEND-DATE
000870     END-IF.
000871
000872     IF FINDATEL GREATER ZERO
000873         MOVE W-FINAL-ACT-DATE   TO  LA-FINAL-ACT-DATE
000874     END-IF.
000875
000876     IF REPLYL GREATER ZERO  *> its really received date
000877        MOVE W-RECEIVED-DATE     TO LA-REPLY-DATE
000878        IF W-RECEIVED-DATE <> LOW-VALUES
000879           move w-cert-note-msg  to w-comment-line-1
000880           move +1               to w-comment-line-cnt
000881           PERFORM 4000-ADD-CERT-NOTE
000882                                 THRU 4099-EXIT
000883           PERFORM 4500-UPDATE-BILL-NOTE
000884                                 THRU 4500-EXIT
000885        END-IF
000886     END-IF
000887
000888     IF RESFORML GREATER ZERO
000889        MOVE RESFORMI            TO  LA-RESEND-LETR
000890     END-IF.
000891
000892     IF FINLACTL GREATER ZERO
000893        MOVE FINLACTI            TO  LA-FINAL-ACT-IND
000894     END-IF.
000895
000896     IF STOPDTEL GREATER ZERO
000897        MOVE W-STOP-LETTER-DATE  TO  LA-VOIDED-DATE
000898     END-IF.
000899
000900     perform 1620-read-elcert    thru 1620-exit
000901
000902     IF (OBVYNL > ZERO)
000903        and (obvyni <> la-void-onbase-yn)
000904        if (obvyni = 'N')
000905           and (la-void-onbase-yn = spaces)
000906           continue
000907        else
000908           if la-initial-print-date <> low-values
000909              perform 1600-update-obkeywords
000910                                 thru 1600-exit
000911           end-if
000912        end-if
000913        MOVE OBVYNI              TO LA-VOID-ONBASE-YN
000914     end-if
000915
000916     if (not cert-found)
000917        or (sql-update-failed)
000918        or ((la-initial-print-date = low-values)
000919              and
000920            (la-void-onbase-yn = 'Y'))
000921        
      * exec cics unlock
000922*          dataset (W-ARCH-FILE-ID)
000923*       end-exec
      *    MOVE '&*                    #   #00003212' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033323132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000924        if connected-to-db
000925           move ' '              to ws-connect-sw
000926           display ' about to disconnect '
000928           exec sql
                    disconnect
000929           end-exec
000930        end-if
000931     end-if
000932
000933     if not cert-found
000934        move er-0249             to emi-error
000935        move -1                  to maintl
000936        perform 9900-error-format thru 9900-exit
000937        go to 8200-send-dataonly
000938     end-if
000939
000940     if sql-update-failed
000941        move er-7331             to emi-error
000942        move -1                  to maintl
000943        perform 9900-error-format thru 9900-exit
000944        go to 8200-send-dataonly
000945     end-if
000946
000947     if (la-void-onbase-yn = 'Y')
000948        and (la-initial-print-date = low-values)
000949        move er-7334             to emi-error
000950        move -1                  to maintl
000951        perform 9900-error-format thru 9900-exit
000952        go to 8200-send-dataonly
000953     end-if
000954
000955     if sql-update-succeeded
000956        display ' about to commit work '
000958        exec sql
                 commit work
000959        end-exec
000960     end-if
000961     if connected-to-db
000962        display ' about to disconnect all '
000964        exec sql
                 disconnect
000965        end-exec
000966        move ' ' to ws-connect-sw
000967     end-if
000968
000969     PERFORM 3100-REWRITE THRU 3100-EXIT.
000970
000971     IF COMMENTL NOT GREATER ZERO
000972
000973         MOVE ER-0000            TO  EMI-ERROR
000974         MOVE ' '                TO  MAINTO
000975         MOVE -1                 TO  MAINTL
000976         MOVE AL-UANOF           TO  MAINTA
000977         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000978         GO TO 1000-SHOW
000979     END-IF.
000980
000981     if (commentl > zeros)
000982        and (commenti not = spaces)
000983        move commenti            to w-cert-note-comment
000984        perform 4160-check-comment thru 4160-exit
000985        PERFORM 4000-ADD-CERT-NOTE
000986                                 THRU 4099-EXIT
000987     end-if
000988
000989     PERFORM 3200-READ-ARCT-FOR-UPDATE THRU 3200-EXIT.
000990
000991     IF PI-COMMENT-INDEX EQUAL +20
000992         MOVE ER-0051    TO  EMI-ERROR
000993         MOVE -1         TO  MAINTL
000994         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000995         GO TO 8200-SEND-DATAONLY
000996     END-IF
000997
000998     SET PI-COMMENT-INDEX UP BY +1.
000999     SET LC-NDX TO PI-COMMENT-INDEX.
001000     MOVE COMMENTI        TO  LT-COMMENT-LINE (LC-NDX)
001001     MOVE W-SAVE-BIN-DATE TO  LT-COMMENT-CHG-DT (LC-NDX).
001002     MOVE PI-PROCESSOR-ID TO  LT-COMMENT-CHG-BY (LC-NDX).
001003     ADD +1               TO  LT-NUM-LINES-ON-RECORD.
001004
001005
001006     IF W-ADD-ARCT = 'N'
001007         PERFORM 3300-REWRITE-ARCT THRU 3300-EXIT
001008     ELSE
001009         PERFORM 3350-INSERT-ARCT THRU 3350-EXIT
001010     END-IF.
001011
001012
001013     MOVE ER-0000                TO  EMI-ERROR.
001014     MOVE ' '                    TO  MAINTO.
001015     MOVE -1                     TO  MAINTL.
001016     MOVE AL-UANOF               TO  MAINTA.
001017     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001018     GO TO 1000-SHOW.
001019
001020                                 EJECT
001021
001022 1000-SHOW.
001023***************************************************************
001024*     THIS ROUTINE WILL READ THE ARCHIVE FILE WITH THE        *
001025*     ARCHIVE NUMBER SPECIFIED FROM THE PRIOR SCREEN.         *
001026***************************************************************
001027     MOVE LOW-VALUES              TO W-ARCT-KEY.
001028     MOVE PI-COMPANY-CD           TO W-ARCH-COMPANY-CD
001029                                     W-ARCT-COMPANY-CD.
001030     MOVE PI-689-ARCHIVE-NUMBER   TO W-ARCH-NUMBER
001031                                     W-ARCT-ARCHIVE-NO.
001032     MOVE '3'                     TO W-ARCT-RECORD-TYPE.
001033     MOVE +0                      TO W-ARCT-LINE-SEQ-NO.
001034
001035     
      * EXEC CICS HANDLE CONDITION
001036*         NOTOPEN    (8010-ARCH-NOT-OPEN)
001037*         NOTFND     (1070-ARCH-NOT-FOUND)
001038*         ENDFILE    (1070-ARCH-NOT-FOUND)
001039*    END-EXEC.
      *    MOVE '"$JI''                 ! $ #00003326' TO DFHEIV0
           MOVE X'22244A492720202020202020' &
                X'202020202020202020202120' &
                X'2420233030303033333236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001040
001041     
      * EXEC CICS READ
001042*        DATASET (W-ARCH-FILE-ID)
001043*        SET     (ADDRESS OF LETTER-ARCHIVE)
001044*        RIDFLD  (W-ARCH-KEY)
001045*    END-EXEC.
      *    MOVE '&"S        E          (   #00003332' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303033333332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001046
001047     MOVE LA-FORM-A3             TO FORMNOO.
001048     MOVE LA-FORM-A3             TO PI-ARCHIVE-LTRID
001049     MOVE LA-ARCHIVE-NO          TO ARCHNOO.
001050     MOVE LA-CARRIER-A2          TO CARRIERO.
001051     MOVE LA-GROUPING-A2         TO GROUPO.
001052     MOVE LA-STATE-A2            TO STATEO.
001053     MOVE LA-ACCOUNT-A2          TO ACCTO.
001054     MOVE LA-CERT-PRIME-A2       TO CERTO.
001055     MOVE LA-CERT-SUFFIX-A2      TO SFXO.
001056
001057     IF LA-ENDT-ARCH-NO-X EQUAL LOW-VALUES OR SPACES
001058          MOVE SPACES            TO ENDARCHO
001059     ELSE
001060          MOVE LA-ENDT-ARCH-NO   TO ENDARCHO
001061     END-IF
001062
001063     IF  LA-CREATION-DATE EQUAL LOW-VALUES OR SPACES
001064         MOVE SPACES             TO CREATDTI
001065                                    pi-create-date
001066     ELSE
001067         MOVE LA-CREATION-DATE   TO DC-BIN-DATE-1
001068         MOVE ' '                TO DC-OPTION-CODE
001069         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
001070         MOVE DC-GREG-DATE-1-EDIT TO CREATDTI
001071                                     pi-create-date
001072     END-IF.
001073
001074     MOVE LA-PROCESSOR-CD        TO CREATBYI.
001075
001076     IF  LA-INITIAL-PRINT-DATE EQUAL LOW-VALUES OR SPACES
001077         MOVE SPACES             TO PRINTDTI
001078         move low-values         to pi-initial-print-date
001079     ELSE
001080         move la-initial-print-date
001081                                 to pi-initial-print-date
001082         MOVE LA-INITIAL-PRINT-DATE TO DC-BIN-DATE-1
001083         MOVE ' '                TO DC-OPTION-CODE
001084         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
001085         MOVE DC-GREG-DATE-1-EDIT TO PRINTDTI
001086     END-IF.
001087
001088     IF  LA-RESEND-DATE = LOW-VALUES OR SPACES
001089         MOVE SPACES             TO RESENDI
001090     ELSE
001091         MOVE LA-RESEND-DATE     TO DC-BIN-DATE-1
001092         MOVE ' '                TO DC-OPTION-CODE
001093         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
001094         MOVE DC-GREG-DATE-1-EDIT TO RESENDI
001095     END-IF.
001096
001097     MOVE LA-RESEND-LETR         TO RESFORMI.
001098
001099     IF  LA-SENT-DATE = LOW-VALUES OR SPACES
001100         MOVE SPACES             TO RESPRNTI
001101         MOVE SPACES             TO PI-ARCHIVE-COMPLETE
001102     ELSE
001103         MOVE 'Y'                TO PI-ARCHIVE-COMPLETE
001104         MOVE LA-SENT-DATE     TO DC-BIN-DATE-1
001105         MOVE ' '                TO DC-OPTION-CODE
001106         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
001107         MOVE DC-GREG-DATE-1-EDIT TO RESPRNTI
001108     END-IF.
001109
001110     IF  LA-FINAL-ACT-DATE EQUAL LOW-VALUES OR SPACES
001111         MOVE SPACES             TO FINDATEI
001112         MOVE SPACES             TO PI-ARCHIVE-FINAL
001113     ELSE
001114         MOVE 'Y'                TO PI-ARCHIVE-FINAL
001115         MOVE LA-FINAL-ACT-DATE  TO DC-BIN-DATE-1
001116         MOVE ' '                TO DC-OPTION-CODE
001117         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
001118         MOVE DC-GREG-DATE-1-EDIT TO FINDATEI
001119     END-IF.
001120
001121     MOVE LA-FINAL-ACT-IND       TO FINLACTI.
001122
001123     IF  LA-REPLY-DATE = LOW-VALUES OR SPACES
001124         MOVE SPACES             TO REPLYI
001125         MOVE SPACES             TO PI-ARCHIVE-RECEIVED
001126     ELSE
001127         MOVE 'Y'                TO PI-ARCHIVE-RECEIVED
001128         MOVE LA-REPLY-DATE      TO DC-BIN-DATE-1
001129         MOVE ' '                TO DC-OPTION-CODE
001130         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
001131         MOVE DC-GREG-DATE-1-EDIT TO REPLYI
001132     END-IF.
001133
001134     IF  LA-VOIDED-DATE = LOW-VALUES OR SPACES
001135         MOVE SPACES             TO STOPDTEI
001136         MOVE SPACES             TO PI-ARCHIVE-STOPPED
001137     ELSE
001138         MOVE 'Y'                TO PI-ARCHIVE-STOPPED
001139         MOVE LA-VOIDED-DATE     TO DC-BIN-DATE-1
001140         MOVE ' '                TO DC-OPTION-CODE
001141         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
001142         MOVE DC-GREG-DATE-1-EDIT TO STOPDTEI
001143     END-IF.
001144
001145     if LA-VOID-ONBASE-YN not = spaces
001146        move LA-VOID-ONBASE-YN   to obvyno
001147     end-if
001148
001149     if la-onbase-unique-id numeric
001150        move la-onbase-unique-id to obuido
001151     end-if
001152
001153     IF LA-LAST-MAINT-DATE = LOW-VALUES OR SPACES
001154         MOVE SPACES             TO MAINTDTI
001155     ELSE
001156         MOVE LA-LAST-MAINT-DATE TO DC-BIN-DATE-1
001157         MOVE ' '                TO DC-OPTION-CODE
001158         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
001159         MOVE DC-GREG-DATE-1-EDIT TO MAINTDTI
001160     END-IF.
001161
001162     IF LA-LAST-MAINT-TIMEX = LOW-VALUES OR SPACES
001163         MOVE ZEROES             TO MAINTTMO
001164     ELSE
001165         MOVE LA-LAST-MAINT-TIME TO W-TIME-IN
001166         MOVE W-TIME-OUT         TO MAINTTMO
001167     END-IF.
001168
001169     MOVE LA-LAST-UPDATED-BY     TO MAINTBYI.
001170
001171     IF (LA-SENT-DATE NOT EQUAL LOW-VALUES AND SPACES)
001172       OR (LA-VOIDED-DATE  NOT EQUAL LOW-VALUES AND SPACES)
001173          MOVE AL-SANOF          TO RESENDA
001174                                    RESFORMA
001175                                    FINDATEA
001176                                    FINLACTA
001177                                    REPLYA
001178                                    COMMENTA
001179     END-IF.
001180
001181     IF LA-SENT-DATE NOT EQUAL LOW-VALUES AND SPACES
001182         MOVE AL-SANOF           TO STOPDTEA
001183     END-IF.
001184
001185     
      * EXEC CICS HANDLE CONDITION
001186*         NOTOPEN    (8015-ARCT-NOT-OPEN)
001187*         NOTFND     (1000-ARCT-NOT-FOUND)
001188*         ENDFILE    (1000-ARCT-NOT-FOUND)
001189*    END-EXEC.
      *    MOVE '"$JI''                 ! % #00003476' TO DFHEIV0
           MOVE X'22244A492720202020202020' &
                X'202020202020202020202120' &
                X'2520233030303033343736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001190
001191     
      * EXEC CICS READ
001192*        DATASET  (W-ARCT-FILE-ID)
001193*        SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
001194*        RIDFLD   (W-ARCT-KEY)
001195*    END-EXEC.
      *    MOVE '&"S        E          (   #00003482' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303033343832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE-TEXT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001196
001197     SET LC-NDX TO W-ZEROS.
001198     PERFORM 20 TIMES
001199         SET LC-NDX UP BY +1
001200         IF LT-COMMENT-LINE (LC-NDX) > SPACES
001201             MOVE LT-COMMENT-LINE (LC-NDX) TO COMMENTI
001202             SET PI-COMMENT-INDEX TO LC-NDX
001203         END-IF
001204     END-PERFORM.
001205
001206     MOVE -1                     TO MAINTL.
001207     GO TO 8100-SEND-INITIAL-MAP.
001208
001209 1000-ARCT-NOT-FOUND.
001210
001211     SET PI-COMMENT-INDEX        TO W-ZEROS.
001212     MOVE -1                     TO MAINTL.
001213     GO TO 8100-SEND-INITIAL-MAP.
001214
001215 1000-EXIT.
001216     EXIT.
001217
001218
001219 1070-ARCH-NOT-FOUND.
001220
001221     MOVE ER-7371                TO EMI-ERROR.
001222     MOVE -1                     TO ARCHNOL.
001223     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001224     GO TO 8100-SEND-INITIAL-MAP.
001225
001226
001227 1500-CHECK-Z-RECORD.
001228
001229     MOVE 'N' TO WS-Z-RECORD-IND
001230
001231     
      * EXEC CICS STARTBR
001232*         DATASET    ('ELLETR')
001233*         RIDFLD     (ELLETR-KEY)
001234*         GTEQ
001235*         RESP      (W-RESPONSE)
001236*    END-EXEC.
           MOVE 'ELLETR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00003522' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303033353232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001237     IF NOT RESP-NORMAL
001238         GO TO 1500-ENDBR
001239     END-IF.
001240
001241 1500-READNEXT.
001242
001243     
      * EXEC CICS READNEXT
001244*        DATASET   ('ELLETR')
001245*        INTO      (TEXT-FILES)
001246*        RIDFLD    (ELLETR-KEY)
001247*        RESP      (W-RESPONSE)
001248*    END-EXEC
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV12
           MOVE 'ELLETR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00003534' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303033353334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 TEXT-FILES, 
                 DFHEIV12, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001249     IF RESP-NORMAL
001250        IF TX-CONTROL-PRIMARY(1:5) NOT = ELLETR-SAVE-PART-KEY
001251           GO TO 1500-ENDBR
001252        END-IF
001253
001254        IF TX-LINE-SQUEEZE-CONTROL = 'Z'
001255           MOVE 'Y' TO WS-Z-RECORD-IND
001256           GO TO 1500-ENDBR
001257        ELSE
001258           GO TO 1500-READNEXT
001259        END-IF
001260     ELSE
001261        GO TO 1500-ENDBR
001262     END-IF.
001263
001264 1500-ENDBR.
001265
001266     
      * EXEC CICS ENDBR
001267*        DATASET     ('ELLETR')
001268*    END-EXEC.
           MOVE 'ELLETR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003557' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303033353537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001269
001270 1500-EXIT.
001271      EXIT.
001272
001273 1600-update-obkeywords.
001274
001275     if not connected-to-db
001276        perform 1700-CONNECT-TO-DB
001277                                 thru 1700-exit
001278     end-if
001279
001280*    perform 1620-read-elcert    thru 1620-exit
001281*    if not cert-found
001282*       go to 1600-exit
001283*    end-if
001284
001285     if obvyni = 'N'
001286        if la-onbase-unique-id numeric
001287           and la-onbase-unique-id > 0
001288           perform 1610-void-void thru 1610-exit
001289           go to 1600-exit
001290        end-if
001291     end-if
001292
001293***  If I get here I assume they are voiding the letter
001294***  I am using a stored procedure so I can get back the
001295***  unique id from the insert and update the erarch
001296***  record with it.  May come in handy down the road
001297
001298     if obvyni = 'Y'
001299        and (la-onbase-unique-id not numeric
001300           or la-onbase-unique-id = zeros)
001301        continue
001302     else
001303        go to 1600-exit
001304     end-if
001305
001306     move 'CID Certs'            to db-doc-type
001307     move 'Status'               to db-key-word-type
001308     move 'V'                    to db-key-word-value
001309     move cm-state               to db-cert-state
001310     move cm-cert-prime          to db-cert-no
001311     move cm-lf-loan-expire-dt   to dc-bin-date-1
001312     if cm-ah-loan-expire-dt > cm-lf-loan-expire-dt
001313        move cm-ah-loan-expire-dt
001314                                 to dc-bin-date-1
001315     end-if
001316     move ' '                    to dc-option-code
001317     perform 9500-LINK-DATE-CONVERT thru 9500-exit
001318     if no-conversion-error
001319        move dc-greg-date-a-edit to db-cert-exp-dt
001320        inspect db-cert-exp-dt
001321           converting '/' to '-'
001322     end-if
001323     move la-initial-print-date  to dc-bin-date-1
001324     move ' '                    to dc-option-code
001325     perform 9500-LINK-DATE-CONVERT thru 9500-exit
001326     if no-conversion-error
001327        move dc-greg-date-a-edit to db-print-dt
001328        inspect db-print-dt
001329           converting '/' to '-'
001330     end-if
001331
001333     EXEC SQL
              CALL sp_OBKeyWordChgs_ADD
001334           @DocType       =  :db-doc-type,
001335           @CertNo        =  :db-cert-no,
001336           @PrintDt       =  :db-print-dt,
001337           @CertExpDt     =  :db-cert-exp-dt,
001338           @CertSt        =  :db-cert-state,
001339           @KeyWordType   =  :db-key-word-type,
001340           @KeyWordValue  =  :db-key-word-value,
001341           @RetValue      = :kd-unique-id OUT
001342     END-EXEC
001343
001344     if sqlcode not = 0
001345        move sqlcode             to ws-sql-code
001346        move ws-sql-code         to ws-dis-sql-code
001347        display ' Error: Cannot INSERT row ' kd-unique-id
001348        display ' sql return code        ' ws-dis-sql-code
001349        display ' sql err mess           ' sqlerrmc
001350        set sql-update-failed to true
001351     else
001352        move kd-unique-id        to la-onbase-unique-id
001353                                    obuido
001354        set sql-update-succeeded to true
001355     end-if
001356
001357     .
001358 1600-exit.
001359     exit.
001360
001361 1610-void-void.
001362
001363     move la-onbase-unique-id    to kd-unique-id
001365     EXEC SQL
              SELECT
001366           RequestDate,
001367           KeyWordType,
001368           KeyWordValue,
001369           CompletionDate
001370        INTO
001371           :db-req-date,
001372           :db-key-word-type,
001373           :db-key-word-value,
001374           :db-complete-dt  :nu-Complete-date
001375        FROM
001376           OBKeyWordChgs
001377        where
001378           :kd-unique-id = UniqueID
001379     END-EXEC
001380
001381     if sqlcode not = 0
001382        move sqlcode             to ws-sql-code
001383        move ws-sql-code         to ws-dis-sql-code
001384        display ' Error: Cannot read row ' kd-unique-id
001385        display ' sql return code        ' ws-dis-sql-code
001386        display ' sql err mess           ' sqlerrmc
001387        go to 1610-exit
001388     end-if
001389     if nu-complete-date = -1
001390        display ' complete date is null '
001391     end-if
001392     if nu-complete-date = -1  *> isnull
001393        and db-key-word-type = 'Status'
001394        and obvyni <> 'Y'
001395        and db-key-word-value = 'V'
001396
001398        EXEC SQL
                 DELETE
001399              OBKeyWordChgs
001400           WHERE
001401              UniqueID = :kd-unique-id
001402        END-EXEC
001403
001404        if sqlcode = 0
001405           display ' delete succeeded '
001406           set sql-update-succeeded to true
001407           move zeros            to la-onbase-unique-id
001408        else
001409           display ' delete not successful ' sqlcode
001410           set sql-update-failed to true
001411        end-if
001412     end-if
001413
001414     .
001415 1610-exit.
001416     exit.
001417
001418 1620-read-elcert.
001419
001420     move la-company-cd          to elcert-key
001421     move la-carrier-a2          to elcert-carrier
001422     move la-grouping-a2         to elcert-grouping
001423     move la-state-a2            to elcert-state
001424     move la-account-a2          to elcert-account
001425     move la-effect-date-a2      to elcert-eff-dt
001426     move la-cert-no-a2          to elcert-cert-no
001427
001428     
      * exec cics read
001429*       dataset    ('ELCERT')
001430*       ridfld     (elcert-key)
001431*       SET        (ADDRESS OF CERTIFICATE-MASTER)
001432*       resp       (w-response)
001433*    end-exec
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00003719' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303033373139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 elcert-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO w-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001434
001435     if resp-normal
001436        set cert-found to true
001437     end-if
001438
001439     .
001440 1620-exit.
001441     exit.
001442
001443 1700-CONNECT-TO-DB.
001444
001445     move 'TEST_Logic'           to svr
001446     move 'appuser'              to usr
001447     move 'appuser@cso'          to pass
001448
001449     if ws-kix-myenv = 'cid1p'
001450        move 'PROD_Logic'        to svr
001451     end-if
001452
001453     string
001454         usr delimited space
001455         "." delimited size
001456         pass delimited space into usr-pass
001457     end-string
001458
001460     EXEC SQL
              CONNECT TO :svr USER :usr-pass
001461     END-EXEC
001462
001463     if sqlcode not = 0
001464        move sqlcode             to ws-sql-code
001465        move ws-sql-code         to ws-dis-sql-code
001466        display ' Error: Cannot Connect  ' kd-unique-id
001467        display ' sql return code        ' ws-dis-sql-code
001468        display ' sql err mess           ' sqlerrmc
001469        move er-7332             to emi-error
001470        perform 9900-error-format thru 9900-exit
001471        go to 8200-send-dataonly
001472     end-if
001473
001474     set connected-to-db to true
001475     display ' good connection '
001476
001477     .
001478 1700-EXIT.
001479     EXIT.
001480
001481 3000-READ-FOR-UPDATE.
001482
001483     
      * EXEC CICS HANDLE CONDITION
001484*         NOTOPEN    (8010-ARCH-NOT-OPEN)
001485*         NOTFND     (1070-ARCH-NOT-FOUND)
001486*         ENDFILE    (1070-ARCH-NOT-FOUND)
001487*    END-EXEC.
      *    MOVE '"$JI''                 ! & #00003774' TO DFHEIV0
           MOVE X'22244A492720202020202020' &
                X'202020202020202020202120' &
                X'2620233030303033373734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001488
001489     MOVE PI-COMPANY-CD TO W-ARCH-COMPANY-CD
001490     MOVE PI-689-ARCHIVE-NUMBER TO W-ARCH-NUMBER
001491
001492     
      * EXEC CICS READ
001493*        DATASET (W-ARCH-FILE-ID)
001494*        SET     (ADDRESS OF LETTER-ARCHIVE)
001495*        RIDFLD  (W-ARCH-KEY)
001496*        UPDATE
001497*    END-EXEC.
      *    MOVE '&"S        EU         (   #00003783' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303033373833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001498
001499 3000-EXIT.
001500     EXIT.
001501
001502                                 EJECT
001503
001504 3100-REWRITE.
001505
001506     MOVE PI-PROCESSOR-ID        TO  LA-LAST-UPDATED-BY
001507                                     MAINTBYI.
001508     MOVE W-SAVE-BIN-DATE        TO  LA-LAST-MAINT-DATE.
001509     MOVE W-SAVE-DATE            TO  MAINTDTI.
001510
001511     MOVE EIBTIME                TO  LA-LAST-MAINT-TIME
001512                                     W-TIME-IN.
001513     MOVE W-TIME-OUT             TO  MAINTTMO.
001514
001515     
      * EXEC CICS REWRITE
001516*        DATASET (W-ARCH-FILE-ID)
001517*        FROM    (LETTER-ARCHIVE)
001518*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003806' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303033383036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001519
001520 3100-EXIT.
001521     EXIT.
001522                                 EJECT
001523 3200-READ-ARCT-FOR-UPDATE.
001524
001525     
      * EXEC CICS HANDLE CONDITION
001526*         NOTFND     (3200-NOT-FOUND)
001527*         ENDFILE    (3200-NOT-FOUND)
001528*    END-EXEC.
      *    MOVE '"$I''                  ! '' #00003816' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303033383136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001529
001530     MOVE PI-COMPANY-CD TO W-ARCT-COMPANY-CD.
001531     MOVE PI-689-ARCHIVE-NUMBER TO W-ARCT-ARCHIVE-NO.
001532     MOVE '3'                   TO W-ARCT-RECORD-TYPE.
001533     MOVE +0                    TO W-ARCT-LINE-SEQ-NO.
001534
001535     
      * EXEC CICS READ
001536*        DATASET  (W-ARCT-FILE-ID)
001537*        SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
001538*        RIDFLD   (W-ARCT-KEY)
001539*        UPDATE
001540*    END-EXEC.
      *    MOVE '&"S        EU         (   #00003826' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303033383236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE-TEXT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001541
001542     MOVE 'N' TO W-ADD-ARCT.
001543     GO TO 3200-EXIT.
001544
001545 3200-NOT-FOUND.
001546
001547     
      * EXEC CICS GETMAIN
001548*         SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
001549*         LENGTH   (W-ARCT-LENGTH)
001550*    END-EXEC.
      *    MOVE '," L                  $   #00003838' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303033383338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-ARCT-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE-TEXT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001551
001552     MOVE 'Y'                    TO W-ADD-ARCT.
001553     MOVE LOW-VALUES             TO LETTER-ARCHIVE-TEXT.
001554     MOVE 'LT'                   TO LT-RECORD-ID.
001555     MOVE PI-COMPANY-CD          TO LT-COMPANY-CD.
001556     MOVE PI-689-ARCHIVE-NUMBER  TO LT-ARCHIVE-NO.
001557     MOVE '3'                    TO LT-RECORD-TYPE.
001558     MOVE +0                     TO LT-LINE-SEQ-NO.
001559     MOVE +0                     TO LT-NUM-LINES-ON-RECORD.
001560     SET PI-COMMENT-INDEX        TO W-ZEROS.
001561
001562 3200-EXIT.
001563     EXIT.
001564
001565 3300-REWRITE-ARCT.
001566
001567     
      * EXEC CICS REWRITE
001568*        DATASET (W-ARCT-FILE-ID)
001569*        FROM    (LETTER-ARCHIVE-TEXT)
001570*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE-TEXT
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003858' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303033383538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 LETTER-ARCHIVE-TEXT, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001571
001572 3300-EXIT.
001573     EXIT.
001574                                 EJECT
001575
001576 3350-INSERT-ARCT.
001577     
      * EXEC CICS WRITE
001578*         DATASET   (W-ARCT-FILE-ID)
001579*         FROM      (LETTER-ARCHIVE-TEXT)
001580*         RIDFLD    (LT-CONTROL-PRIMARY)
001581*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE-TEXT
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003868' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033383638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 LETTER-ARCHIVE-TEXT, 
                 DFHEIV11, 
                 LT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001582
001583 3350-EXIT.
001584     EXIT.
001585                                 EJECT
001586
001587
001588 4000-ADD-CERT-NOTE.
001589
001590     MOVE SPACES                 TO  ERCNOT-KEY
001591                                     ELCERT-KEY.
001592     move spaces                 to cert-note-records-holder
001593                                    ws-build-note-sw
001594                                    ws-ercnot-sw
001595     move +0                     to c1
001596
001597     MOVE PI-COMPANY-CD          TO  ERCNOT-COMPANY-CD
001598                                     ELCERT-COMPANY-CD.
001599     MOVE PI-CARRIER             TO  ERCNOT-CARRIER
001600                                     ELCERT-CARRIER.
001601     MOVE PI-GROUPING            TO  ERCNOT-GROUPING
001602                                     ELCERT-GROUPING.
001603     MOVE PI-STATE               TO  ERCNOT-STATE
001604                                     ELCERT-STATE.
001605     MOVE PI-ACCOUNT             TO  ERCNOT-ACCOUNT
001606                                     ELCERT-ACCOUNT.
001607     MOVE PI-CERT-EFF-DT         TO  ERCNOT-EFF-DT
001608                                     ELCERT-EFF-DT.
001609     MOVE PI-CERT-PRIME          TO  ERCNOT-CERT-PRIME
001610                                     ELCERT-CERT-PRIME.
001611     MOVE PI-CERT-SFX            TO  ERCNOT-CERT-SFX
001612                                     ELCERT-CERT-SFX.
001613     MOVE '1'                    TO  ERCNOT-REC-TYP
001614     MOVE ZEROS                  TO  ERCNOT-SEQ.
001615     MOVE ERCNOT-KEY             TO  SV-PRIOR-KEY.
001616
001617
001618     
      * EXEC CICS STARTBR
001619*       DATASET    ('ERCNOT')
001620*       RIDFLD     (ercnot-key)
001621*       GTEQ
001622*       RESP       (W-RESPONSE)
001623*    END-EXEC
           MOVE 'ERCNOT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00003909' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303033393039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ercnot-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001624
001625     IF RESP-NORMAL
001626        set ercnot-startbr to true
001627*       display ' resp normal startbr '
001628        perform until finished-with-notes
001629           
      * EXEC CICS READNEXT
001630*             DATASET    ('ERCNOT')
001631*             RIDFLD     (ercnot-key)
001632*             INTO       (cert-note-file)
001633*             resp       (w-response)
001634*          end-exec
           MOVE LENGTH OF
            cert-note-file
             TO DFHEIV12
           MOVE 'ERCNOT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00003920' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303033393230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 cert-note-file, 
                 DFHEIV12, 
                 ercnot-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001635           if (resp-normal)
001636              and (cz-control-primary (1:33) =
001637                 sv-prior-key (1:33))
001638              if cz-record-type = '1'
001639                 add +1 to c1
001640                 move cert-note-file
001641                                 to cert-note-record (c1)
001642              end-if
001643           else
001644              set finished-with-notes to true
001645           end-if
001646        end-perform
001647     end-if
001648
001649     if ercnot-startbr
001650*       display ' about to endbr ercnot '
001651        
      * exec cics endbr
001652*          dataset    ('ERCNOT')
001653*       end-exec
           MOVE 'ERCNOT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003942' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303033393432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001654     end-if
001655     move c1                     to note-count
001656
001657     if c1 = +0
001658        perform 4100-add-note    thru 4100-exit
001659     else
001660        perform 4120-delete-cert-notes
001661                                 thru 4120-exit
001662        if resp-normal
001663           perform 4100-add-note thru 4100-exit
001664           if resp-normal
001665              perform 4140-put-back-cert-notes
001666                                 thru 4140-exit
001667              if resp-normal
001668                 display 'NOTE SUCCESSFULLY ADDED'
001669              else
001670                 display ' something wrong with put back '
001671              end-if
001672           else
001673              display ' something went wrong with adding note '
001674           end-if
001675        else
001676           display ' something went wrong with generic delete '
001677        end-if
001678     end-if
001679
001680     .
001681 4099-EXIT.
001682     EXIT.
001683
001684
001685 4100-add-note.
001686
001687***  Need to check how long the note is
001688
001689     if w-comment-line-cnt = +0
001690        go to 4100-exit
001691     end-if
001692
001693     move 'CZ'                to cert-note-file
001694     move sv-prior-key        to cz-control-primary
001695     move '1'                 to cz-record-type
001696     move +1                  to cz-note-sequence
001697     move w-save-bin-date     to cz-last-maint-dt
001698     move eibtime             to cz-last-maint-hhmmss
001699     move pi-processor-id     to cz-last-maint-user
001700     move w-comment-line-1    to cz-note
001701
001702     .
001703 4100-write.
001704
001705     
      * exec cics write
001706*       dataset   ('ERCNOT')
001707*       from      (cert-note-file)
001708*       ridfld    (cz-control-primary)
001709*       resp      (w-response)
001710*    end-exec
           MOVE LENGTH OF
            cert-note-file
             TO DFHEIV11
           MOVE 'ERCNOT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003996' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303033393936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 cert-note-file, 
                 DFHEIV11, 
                 cz-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001711     if not resp-normal
001712        display ' error-ercnot-write ' w-response ' '
001713           cz-control-primary (2:33)
001714        go to 4100-exit
001715     else
001716        display 'NOTE SUCCESSFULLY ADDED'
001717     end-if
001718
001719     if w-comment-line-cnt > +1
001720        move w-comment-line-2    to cz-note
001721        add +1 to cz-note-sequence
001722        move +0                  to w-comment-line-cnt
001723        go to 4100-write
001724     end-if
001725
001726     .
001727 4100-exit.
001728     exit.
001729
001730
001731
001732 4120-delete-cert-notes.
001733
001734     move sv-prior-key (1:33)    to ercnot-key
001735     move '1'                    to ercnot-rec-typ
001736     move +0                     to ercnot-seq
001737     
      * exec cics delete
001738*       dataset    ('ERCNOT')
001739*       keylength  (ws-cert-note-generic-key-len)
001740*       ridfld     (ercnot-key (1:34))
001741*       generic
001742*       resp       (w-response)
001743*    end-exec
           MOVE 'ERCNOT' TO DFHEIV1
      *    MOVE '&(  RKG               &  N#00004028' TO DFHEIV0
           MOVE X'26282020524B472020202020' &
                X'202020202020202020202620' &
                X'204E233030303034303238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ercnot-key(1 : 34), 
                 ws-cert-note-generic-key-len, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001744
001745     .
001746 4120-exit.
001747     exit.
001748
001749 4140-put-back-cert-notes.
001750
001751     perform varying c1 from +1 by +1 until
001752        c1 > note-count
001753        move cert-note-record (c1)
001754                                 to cert-note-file
001755        add +2                   to cz-note-sequence
001756*       display ' about to write ' cz-control-primary (2:19) ' '
001757*          cz-control-primary (23:11) ' ' cz-note-sequence ' '
001758*           cz-record-type ' ' cz-note-information
001759        
      * exec cics write
001760*          dataset ('ERCNOT')
001761*          FROM    (cert-note-file)
001762*          ridfld  (cz-control-primary)
001763*          resp    (w-response)
001764*       end-exec
           MOVE LENGTH OF
            cert-note-file
             TO DFHEIV11
           MOVE 'ERCNOT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00004050' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303034303530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 cert-note-file, 
                 DFHEIV11, 
                 cz-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001765        if not resp-normal
001766           display ' error-ercnot-write subsequ ' w-response ' '
001767              cz-control-primary (2:33)
001768           move +999             to c1
001769        end-if
001770     end-perform
001771
001772    .
001773 4140-exit.
001774     exit.
001775
001776 4160-check-comment.
001777
001778     string
001779        w-cert-note-comment ' ' delimited by '  '
001780        la-form-a3 ' ' delimited by size
001781        la-processor-cd ' ' delimited by size
001782        pi-create-date delimited by size
001783           into w-stop-letter-comment
001784     end-string
001785
001786     perform varying n1 from +126 by -1 until
001787        w-stop-letter-comment(n1:1) <> space
001788     end-perform
001789     if n1 < +64
001790        move +1                  to w-comment-line-cnt
001791        move w-stop-letter-comment
001792                                 to w-comment-line-1
001793        go to 4160-exit
001794     end-if
001795
001796     move +2                     to w-comment-line-cnt
001797
001798     perform varying n1 from n1 by -1 until
001799        ((w-stop-letter-comment(n1:1) = ' ')
001800            and
001801         (n1 < +64))
001802        or (n1 < +1)
001803     end-perform
001804
001805     if n1 < +1
001806        move w-stop-letter-comment(1:63)
001807                                 to w-comment-line-1
001808        move w-stop-letter-comment(64:63)
001809                                 to w-comment-line-2
001810     else
001811        move w-stop-letter-comment(1:n1)
001812                                 to w-comment-line-1
001813        move w-stop-letter-comment(n1 + 1:126 - n1)
001814                                 to w-comment-line-2
001815     end-if
001816
001817     .
001818 4160-exit.
001819     exit.
001820
001821 4500-UPDATE-BILL-NOTE SECTION.
001822
001823     
      * EXEC CICS GETMAIN
001824*         SET      (ADDRESS OF EOB-CODES)
001825*         LENGTH   (ELEOBC-LENGTH)
001826*    END-EXEC
      *    MOVE '," L                  $   #00004114' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303034313134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELEOBC-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF EOB-CODES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001827
001828     MOVE LOW-VALUES             TO ELEOBC-KEY
001829     MOVE PI-COMPANY-CD          TO EOBC-COMPANY-CD
001830     MOVE '5'                    TO EOBC-REC-TYPE
001831
001832     
      * EXEC CICS STARTBR
001833*        DATASET   (ELEOBC-FILE-ID)
001834*        RIDFLD    (ELEOBC-KEY)
001835*        GTEQ
001836*        RESP      (W-RESPONSE)
001837*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00004123' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303034313233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELEOBC-FILE-ID, 
                 ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001838
001839     IF NOT RESP-NORMAL
001840        GO TO 4500-EXIT
001841     END-IF
001842      .
001843 4500-READNEXT-ELEOBC.
001844
001845     
      * EXEC CICS READNEXT
001846*       INTO    (EOB-CODES)
001847*       DATASET (ELEOBC-FILE-ID)
001848*       RIDFLD  (ELEOBC-KEY)
001849*       RESP    (W-RESPONSE)
001850*    END-EXEC
           MOVE LENGTH OF
            EOB-CODES
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00004136' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303034313336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELEOBC-FILE-ID, 
                 EOB-CODES, 
                 DFHEIV12, 
                 ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001851
001852     IF RESP-NORMAL
001853         IF EO-RECORD-TYPE NOT = '5'
001854             GO TO 4500-EXIT
001855         END-IF
001856     ELSE
001857         GO TO 4500-EXIT
001858     END-IF
001859
001860     IF EO-RECORD-TYPE = '5' AND
001861        EO-EOB-CODE = PI-ARCHIVE-LTRID
001862           CONTINUE
001863     ELSE
001864         GO TO 4500-READNEXT-ELEOBC
001865     END-IF
001866
001867     MOVE SPACES TO WS-FIND-BILLING-NOTE
001868     MOVE EO-DESCRIPTION TO WS-FBN-NOTE
001869     MOVE PI-ARCHIVE-LTRID TO WS-FBN-LTRID
001870
001871     
      * EXEC CICS GETMAIN
001872*         SET      (ADDRESS OF CERTIFICATE-NOTE)
001873*         LENGTH   (ERNOTE-LENGTH)
001874*    END-EXEC
      *    MOVE '," L                  $   #00004162' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303034313632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERNOTE-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001875
001876     MOVE PI-COMPANY-CD          TO  ERNOTE-COMPANY-CD
001877     MOVE PI-CARRIER             TO  ERNOTE-CARRIER
001878     MOVE PI-GROUPING            TO  ERNOTE-GROUPING
001879     MOVE PI-STATE               TO  ERNOTE-STATE
001880     MOVE PI-ACCOUNT             TO  ERNOTE-ACCOUNT
001881     MOVE PI-CERT-EFF-DT         TO  ERNOTE-CERT-EFF-DT
001882     MOVE PI-CERT-PRIME          TO  ERNOTE-CERT-PRIME
001883     MOVE PI-CERT-SFX            TO  ERNOTE-CERT-SFX
001884     move '1'                    to  ernote-record-type
001885
001886     
      * EXEC CICS READ
001887*       DATASET    (ERNOTE-FILE-ID)
001888*       RIDFLD     (ERNOTE-KEY)
001889*       INTO       (CERTIFICATE-NOTE)
001890*       RESP       (W-RESPONSE)
001891*       UPDATE
001892*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&"IL       EU         (  N#00004177' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303034313737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 ERNOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001893
001894     IF RESP-NORMAL
001895       PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
001896           (NOTE-SUB > +10) OR
001897           (CN-LINE (NOTE-SUB) (1:29) =
001898                             WS-FIND-BILLING-NOTE (1:29))
001899       END-PERFORM
001900       IF CN-LINE (NOTE-SUB) (1:29) NOT =
001901                              WS-FIND-BILLING-NOTE (1:29)
001902         
      * EXEC CICS UNLOCK
001903*           DATASET    (ERNOTE-FILE-ID)
001904*        END-EXEC
      *    MOVE '&*                    #   #00004193' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034313933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001905         GO TO 4500-EXIT
001906       END-IF
001907
001908       MOVE WS-RECEIVED-NOTE TO CN-LINE (NOTE-SUB) (50:20)
001909       MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
001910       MOVE W-SAVE-BIN-DATE     TO CN-LAST-MAINT-DT
001911       MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
001912       
      * EXEC CICS REWRITE
001913*         DATASET    (ERNOTE-FILE-ID)
001914*         FROM       (CERTIFICATE-NOTE)
001915*         RESP       (W-RESPONSE)
001916*      END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00004203' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303034323033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001917
001918       .
001919
001920 4500-EXIT.
001921     EXIT.
001922
001923
001924 4800-CERTIFICATE-UPDATE SECTION.
001925
001926     
      * EXEC CICS HANDLE CONDITION
001927*        NOTFND   (4899-EXIT)
001928*    END-EXEC.
      *    MOVE '"$I                   ! ( #00004217' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303034323137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001929
001930     
      * EXEC CICS READ
001931*    EQUAL
001932*    DATASET   (ELCERT-FILE-ID)
001933*    SET       (ADDRESS OF CERTIFICATE-MASTER)
001934*    RIDFLD    (ELCERT-KEY)
001935*    UPDATE
001936*    RESP      (W-RESPONSE)
001937*    END-EXEC.
      *    MOVE '&"S        EU         (  N#00004221' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303034323231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001938
001939     IF RESP-NORMAL
001940        IF CM-NOTE-SW = ' '
001941           MOVE '1'        TO CM-NOTE-SW
001942        ELSE
001943           IF CM-NOTE-SW = '2'
001944              MOVE '3'      TO CM-NOTE-SW
001945           ELSE
001946              IF CM-NOTE-SW = '4'
001947                  MOVE '5'    TO CM-NOTE-SW
001948              ELSE
001949                 IF CM-NOTE-SW = '6'
001950                    MOVE '7'  TO CM-NOTE-SW
001951                 END-IF
001952              END-IF
001953           END-IF
001954        END-IF
001955        
      * EXEC CICS REWRITE
001956*          FROM      (CERTIFICATE-MASTER)
001957*          DATASET   (ELCERT-FILE-ID)
001958*       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004246' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034323436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001959     END-IF.
001960
001961 4899-EXIT.
001962      EXIT.
001963
001964
001965
001966 8010-ARCH-NOT-OPEN.
001967
001968     MOVE ER-7388                TO EMI-ERROR.
001969     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001970     MOVE -1                     TO MAINTL.
001971     GO TO 8200-SEND-DATAONLY.
001972
001973 8015-ARCT-NOT-OPEN.
001974
001975     MOVE ER-7373                TO EMI-ERROR.
001976     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001977     MOVE -1                     TO MAINTL.
001978     GO TO 8200-SEND-DATAONLY.
001979                                 EJECT
001980
001981 8100-SEND-INITIAL-MAP.
001982
001983     PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
001984
001985     IF  NOT EMI-NO-ERRORS
001986         MOVE EMI-MESSAGE-AREA (1)
001987                                 TO ERRMSG1O
001988     ELSE
001989         MOVE SPACES             TO ERRMSG1O
001990     END-IF.
001991
001992     
      * EXEC CICS SEND
001993*        MAP    (W-MAP)
001994*        MAPSET (W-MAPSET)
001995*        FROM   (EL691AO)
001996*        ERASE
001997*        CURSOR
001998*    END-EXEC.
           MOVE LENGTH OF
            EL691AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004283' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303034323833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL691AO, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001999
002000     GO TO 9000-RETURN-TRANS.
002001
002002 8100-EXIT.
002003     EXIT.
002004                                 EJECT
002005
002006 8200-SEND-DATAONLY.
002007
002008     PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
002009
002010     IF  NOT EMI-NO-ERRORS
002011         MOVE EMI-MESSAGE-AREA (1)
002012                                 TO ERRMSG1O
002013     ELSE
002014         MOVE SPACES             TO ERRMSG1O
002015     END-IF.
002016
002017     
      * EXEC CICS SEND
002018*        MAP    (W-MAP)
002019*        MAPSET (W-MAPSET)
002020*        FROM   (EL691AO)
002021*        DATAONLY
002022*        CURSOR
002023*    END-EXEC.
           MOVE LENGTH OF
            EL691AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004308' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303034333038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL691AO, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002024
002025     GO TO 9000-RETURN-TRANS.
002026
002027 8200-EXIT.
002028     EXIT.
002029                                 EJECT
002030
002031 8300-SEND-TEXT.
002032
002033     
      * EXEC CICS SEND TEXT
002034*        FROM    (LOGOFF-TEXT)
002035*        LENGTH  (LOGOFF-LENGTH)
002036*        ERASE
002037*        FREEKB
002038*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004324' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303034333234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002039
002040     GO TO 9000-RETURN-TRANS.
002041
002042 8300-EXIT.
002043     EXIT.
002044                                 EJECT
002045
002046 8600-DEEDIT.
002047
002048     
      * EXEC CICS BIF DEEDIT
002049*         FIELD    (W-DEEDIT-FIELD)
002050*         LENGTH   (15)
002051*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004339' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034333339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002052
002053 8600-EXIT.
002054     EXIT.
002055                                 EJECT
002056
002057 9000-RETURN-TRANS.
002058
002059     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO
002060     MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO
002061     
      * EXEC CICS RETURN
002062*        TRANSID    (W-TRANSACTION)
002063*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
002064*        LENGTH     (PI-COMM-LENGTH)
002065*    END-EXEC.
      *    MOVE '.(CT                  ''   #00004352' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034333532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TRANSACTION, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002066
002067 9000-EXIT.
002068     EXIT.
002069                                 EJECT
002070
002071 9200-PA.
002072
002073     MOVE ER-7008                TO EMI-ERROR.
002074     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002075     MOVE -1                     TO MAINTL.
002076     GO TO 8200-SEND-DATAONLY.
002077
002078 9200-EXIT.
002079     EXIT.
002080                                 EJECT
002081
002082 9300-DFHCLEAR.
002083
002084     MOVE PI-RETURN-TO-PROGRAM TO W-CALL-PGM.
002085     GO TO 9400-XCTL.
002086
002087 9300-EXIT.
002088     EXIT.
002089
002090 9400-XCTL.
002091
002092     
      * EXEC CICS XCTL
002093*        PROGRAM  (W-CALL-PGM)
002094*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
002095*        LENGTH   (PI-COMM-LENGTH)
002096*    END-EXEC.
      *    MOVE '.$C                   %   #00004383' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034333833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002097
002098 9400-EXIT.
002099     EXIT.
002100                                 EJECT
002101
002102 9500-LINK-DATE-CONVERT.
002103
002104     
      * EXEC CICS LINK
002105*        PROGRAM    ('ELDATCV')
002106*        COMMAREA   (DATE-CONVERSION-DATA)
002107*        LENGTH     (DC-COMM-LENGTH)
002108*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00004395' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034333935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002109
002110 9500-EXIT.
002111     EXIT.
002112
002113 9600-FORMAT-DATE-TIME.
002114
002115     MOVE W-SAVE-DATE            TO RUNDTEO.
002116     MOVE EIBTIME                TO W-TIME-IN.
002117     MOVE W-TIME-OUT             TO RUNTIMEO.
002118     MOVE PI-COMPANY-ID          TO COMPANYO.
002119     MOVE PI-PROCESSOR-ID        TO USERIDO.
002120     MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO.
002121
002122     IF PI-ARCHIVE-COMPLETE = 'Y'
002123        MOVE AL-SANOF           TO STCOMPA
002124        MOVE AL-SADOF           TO STRECVA
002125                                   STSTOPA
002126                                   STFINLA
002127     ELSE
002128       IF PI-ARCHIVE-RECEIVED = 'Y'
002129           MOVE AL-SANOF        TO STRECVA
002130           MOVE AL-SADOF        TO STCOMPA
002131                                   STSTOPA
002132                                   STFINLA
002133       ELSE
002134         IF PI-ARCHIVE-STOPPED = 'Y'
002135             MOVE AL-SANOF      TO STSTOPA
002136             MOVE AL-SADOF      TO STCOMPA
002137                                   STRECVA
002138                                   STFINLA
002139         ELSE
002140           IF PI-ARCHIVE-FINAL = 'Y'
002141               MOVE AL-SANOF    TO STFINLA
002142               MOVE AL-SADOF    TO STCOMPA
002143                                   STRECVA
002144                                   STSTOPA
002145           ELSE
002146               MOVE AL-SADOF    TO STCOMPA
002147                                   STRECVA
002148                                   STSTOPA
002149                                   STFINLA
002150           END-IF
002151         END-IF
002152       END-IF
002153     END-IF.
002154
002155
002156 9600-EXIT.
002157     EXIT.
002158                                 EJECT
002159
002160 9700-PGMID-ERROR.
002161
002162     
      * EXEC CICS  HANDLE CONDITION
002163*        PGMIDERR  (8300-SEND-TEXT)
002164*    END-EXEC.
      *    MOVE '"$L                   ! ) #00004453' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303034343533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002165
002166     MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM.
002167     MOVE ' '                    TO PI-ENTRY-CD-1.
002168     MOVE W-XCTL-005             TO W-CALL-PGM
002169                                    LOGOFF-PGM.
002170     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
002171
002172     PERFORM 9400-XCTL THRU 9400-EXIT.
002173
002174 9700-EXIT.
002175     EXIT.
002176
002177 9800-ABEND.
002178
002179     MOVE W-LINK-004             TO W-CALL-PGM.
002180     MOVE DFHEIBLK               TO EMI-LINE1
002181
002182     
      * EXEC CICS  LINK
002183*        PROGRAM   (W-CALL-PGM)
002184*        COMMAREA  (EMI-LINE1)
002185*        LENGTH    (72)
002186*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004473' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034343733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002187
002188     GO TO 8200-SEND-DATAONLY.
002189
002190 9800-EXIT.
002191     EXIT.
002192                                 EJECT
002193
002194 9900-ERROR-FORMAT.
002195
002196     IF  EMI-ERROR EQUAL ER-9097
002197         NEXT SENTENCE
002198     ELSE
002199         IF  EMI-ERRORS-COMPLETE
002200                 OR
002201             EMI-ERROR EQUAL W-LAST-ERROR
002202             GO TO 9900-EXIT
002203     END-IF.
002204
002205     ADD +1  TO  W-ERROR-COUNT.
002206     MOVE W-LINK-001             TO W-CALL-PGM.
002207
002208     
      * EXEC CICS LINK
002209*        PROGRAM    (W-CALL-PGM)
002210*        COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
002211*        LENGTH     (EMI-COMM-LENGTH)
002212*    END-EXEC.
      *    MOVE '."C                   (   #00004499' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034343939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002213
002214     MOVE EMI-ERROR              TO W-LAST-ERROR.
002215
002216 9900-EXIT.
002217     EXIT.
002218
002219 9905-INITIALIZE-SECURITY.
002220******************************************************************
002221*                                                                *
002222*       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
002223*       USER SECURITY RECORD SET UP BY EL125.  BASED ON THE      *
002224*       APPLICATION NUMBER FOUND IN WORKING STORAGE UNDER        *
002225*       W-APPL-SECRTY-NDX (PIC  S9(04) COMP), THIS PROGRAM       *
002226*       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
002227*       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
002228*       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
002229*       ERROR CONDITION AND EXIT THE PROGRAM.                    *
002230*                                                                *
002231*       NOTE:  THE CARRIER/GRP/STATE/ACCOUNT SECURITY DATA       *
002232*       IS ALSO PROVIDED BY THIS LOGIC.                          *
002233*                                                                *
002234******************************************************************
002235
002236     IF  PI-PROCESSOR-ID EQUAL 'LGXX'
002237         MOVE 'Y'                TO PI-DISPLAY-CAP
002238                                    PI-MODIFY-CAP
002239     ELSE
002240         
      * EXEC CICS READQ TS
002241*            QUEUE  (PI-SECURITY-TEMP-STORE-ID)
002242*            INTO   (SECURITY-CONTROL)
002243*            LENGTH (SC-COMM-LENGTH)
002244*            ITEM   (1)
002245*        END-EXEC
           MOVE 1
             TO DFHEIV11
      *    MOVE '*$II   L              ''   #00004531' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303034353331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002246         MOVE SC-CREDIT-DISPLAY (W-APPL-SCRTY-NDX)
002247                                 TO PI-DISPLAY-CAP
002248         MOVE SC-CREDIT-UPDATE (W-APPL-SCRTY-NDX)
002249                                 TO PI-MODIFY-CAP
002250     END-IF.
002251
002252 9905-EXIT.
002253                                 EJECT
002254
002255 9995-SECURITY-VIOLATION.
002256
002257     MOVE EIBDATE                TO SM-JUL-DATE.
002258     MOVE EIBTRMID               TO SM-TERMID.
002259     MOVE W-THIS-PGM             TO SM-PGM.
002260     MOVE EIBTIME                TO W-TIME-IN.
002261     MOVE W-TIME-OUT             TO SM-TIME.
002262     MOVE PI-PROCESSOR-ID        TO SM-PROCESSOR-ID.
002263
002264     
      * EXEC CICS LINK
002265*         PROGRAM  ('EL003')
002266*         COMMAREA (SECURITY-MESSAGE)
002267*         LENGTH   (80)
002268*    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00004555' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034353535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002269
002270 9995-EXIT.
002271     EXIT.
002272
002273 9999-GOBACK.
002274
002275     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL691' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
002276
002277 9999-EXIT.
002278     EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL691' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9300-DFHCLEAR,
                     9200-PA,
                     9200-PA,
                     9200-PA
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9700-PGMID-ERROR,
                     9800-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8010-ARCH-NOT-OPEN,
                     1070-ARCH-NOT-FOUND,
                     1070-ARCH-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8015-ARCT-NOT-OPEN,
                     1000-ARCT-NOT-FOUND,
                     1000-ARCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8010-ARCH-NOT-OPEN,
                     1070-ARCH-NOT-FOUND,
                     1070-ARCH-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3200-NOT-FOUND,
                     3200-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4899-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL691' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
