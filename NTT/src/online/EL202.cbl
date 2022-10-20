      *((program: EL202.cl2))
000001*$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
000002*****************************************************************
000003*                                                               *
000004* Copyright (c) 2014 by Central States Health and Life          *
000005* All rights reserved.                                          *
000006*                                                               *
000007*****************************************************************
000008 identification division.
000009 program-id. EL202.
000010*
000011*AUTHOR.    Cowtown.
000012*           Colleyville, TEXAS.
000013
000014*REMARKS.
000015***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000016***                                                            ***
000017***  This program is waken up by a webservice request/http     ***
000018***  request and is passed a remit id. The program will next   ***
000019***  read tables CUC_Logic_Remittance and CUC_Logic_Remittance_***
000020***  cert using the remit id passed and will in turn generate  ***
000021***  a ERPYAJ record, ERPNDB and ERPNDM records.               ***
000022***                                                            ***
000023***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000024
000025******************************************************************
000026*                   C H A N G E   L O G
000027*
000028* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000029*-----------------------------------------------------------------
000030*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000031* EFFECTIVE    NUMBER
000032*-----------------------------------------------------------------
000033* 091114  CR2013121800001  PEMA  NEW PROGRAM
000034* 111715  IR2015111600001  PEMA  CHG G/L ACCTNO ON CID ACH
000035* 040516  IR2016040100001  PEMA  CNVT ALL TEXT TO UPPER CASE
000036* 080116  IR2016090100005  PEMA  Expand numeric fields
000037* 120921  IR2021120900003  PEMA  Move ezLink_Prod to sdv-db01
000038*-----------------------------------------------------------------
000039
000040 environment division.
000041
000042 data division.
000043
000044 working-storage section.
       01  DFH-START PIC X(04).
000045
000046 77  s1                          pic s999 comp-3 value +0.
000047 77  s2                          pic s999 comp-3 value +0.
000048 77  i1                          pic s999 comp-3 value +0.
000049 77  o1                          pic s999 comp-3 value +0.
000050 77  ws-bin-current-dt           pic xx value low-values.
000051 77  ws-bin-eom-dt               pic xx value low-values.
000052 77  ws-table-sw                 pic x    value spaces.
000053     88  table-found               value 'Y'.
000054 77  ws-sql-code                 pic s9(7) value zeros.
000055 77  ws-dis-sql-code             pic -9999999 value zeros.
000056 77  ws-remit-sw                 pic x value ' '.
000057     88  ws-remit-not-found        value 'N'.
000058 77  work-seq-no                 pic s9(8) comp-3 value +0.
000059 77  ws-rollback-sw              pic x  value spaces.
000060     88  rollback-needed            value 'Y'.
000061 77  ws-commit-sw                pic x  value spaces.
000062     88  commit-needed              value 'Y'.
000063 77  ROLL-BACK                   PIC X(08) VALUE 'ROLLBACK'.
000064
000065 01  P pointer.
000066 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000067 01  var-ptr pointer.
000068 01  env-var-len                 pic 9(4)  binary.
000069 01  rc                          pic 9(9)  binary.
000070
000071 01  WS-KIXSYS.
000072     05  WS-KIX-FIL1             PIC X(10).
000073     05  WS-KIX-APPS             PIC X(10).
000074     05  WS-KIX-ENV              PIC X(10).
000075     05  WS-KIX-MYENV            PIC X(10).
000076     05  WS-KIX-SYS              PIC X(10).
000077
000078*EXEC SQL
000079*   INCLUDE SQLDA
000080*END-EXEC
000081
000084*EXEC SQL
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
000085
000087 EXEC SQL
          BEGIN DECLARE SECTION
000088 END-EXEC
000089
000090 01  ws-remit-id-a               pic x(10).
000091 01  ws-remit-id redefines
000092     ws-remit-id-a               pic 9(10).
000093
000094 01  ws-key-stuff.
000095     05  ws-batch-no             pic x(6) value zeros.
000096     05  ws-batch-no-n redefines
000097         ws-batch-no             pic 9(6).
000098     05  ws-check-key            pic 9(7).
000099     05  ws-check-no             pic x(7).
000100     05  ws-compid               pic xxx.
000101     05  ws-carrier              pic x.
000102     05  ws-grouping             pic x(6).
000103     05  ws-state                pic xx.
000104     05  ws-account              pic x(10).
000105     05  ws-eff-date             pic x(10).
000106     05  ws-certificate          pic x(10).
000107     05  ws-cert-sfx             pic x.
000108     05  ws-seq-no               pic 999.
000109     05  ws-type                 pic 999.
000110
000111 01  ws-status-code-a            pic x(7) value zeros.
000112 01  ws-status-code redefines
000113     ws-status-code-a            pic 9(7).
000114 01  ws-error-message            pic x(50) value spaces.
000115 01  ws-status-date              pic x(10).
000116 01  sqlcmd                      pic x(1024).
000117 01  svr                         pic x(32).
000118 01  usr                         pic x(32).
000119 01  pass                        pic x(32).
000120 01  usr-pass                    pic x(64).
000121 01  ws-disp-code                pic s9(11).
000122
000123***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000124***                                                            ***
000125***  These indicators are used to determine if a variable      ***
000126***  is        null.           The indicator will be -1        ***
000127***  if the value        is null  and +0 if the value is       ***
000128***  something other than null.  Here is an example on how     ***
000129***  to use the indicator variables.                           ***
000130***                                                            ***
000131***     EXEC SQL                                               ***
000132***        fetch checkapp into                                 ***
000133***           :db-app-status :nu-app-status,                   ***
000134***           :db-app-by     :nu-app-by,                       ***
000135***           :db-app-date   :nu-app-date,                     ***
000136***           :db-app-batch  :nu-app-batch                     ***
000137***     END-EXEC                                               ***
000138***                                                            ***
000139***           OR This way on an update                         ***
000140***                                                            ***
000141***     EXEC SQL                                               ***
000142***        UPDATE                                              ***
000143***           CUC_Logic_Remittance                             ***
000144***        SET                                                 ***
000145***           LogicStatus     = :ws-status-code,               ***
000146***           LogicStatusDate = :ws-status-date,               ***
000147***           BatchNumber     = :ws-batch-no :nu-batchno       ***
000148***        WHERE                                               ***
000149***           RemitId = :ws-remit-id                           ***
000150***     END-EXEC                                               ***
000151***                                                            ***
000152***    Also, when the table has a column with a data type of   ***
000153***  "BIT" and used as true/false move the 1 byte receiving    ***
000154***  field to ws-bit and check out ws-bit-comp. if = zeros,    ***
000155***  then its false. I think true would be 256.                ***
000156***                                                            ***
000157***                                                            ***
000158***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000159
000160 01  ws-bit                      pic x.
000161 01  ws-bit-comp redefines ws-bit pic s9(4) comp.
000162 01  indicator-vaiables-for-nulls.
000163     05  nu-app-status           pic s9(4) comp value +0.
000164     05  nu-app-by               pic s9(4) comp value +0.
000165     05  nu-app-date             pic s9(4) comp value +0.
000166     05  nu-app-batch            pic s9(4) comp value +0.
000167     05  nu-fincar               pic s9(4) comp value +0.
000168     05  nu-batchno              pic s9(4) comp value +0.
000169     05  nu-error-message        pic s9(4) comp value +0.
000170
000171 01  sql-remit-record.
000172     05  sql-py-RemitId          pic 9(9).
000173     05  sql-py-Carrier          pic x.
000174     05  sql-py-Group            pic x(6).
000175     05  sql-py-State            pic xx.
000176     05  sql-py-AccountNumber    pic x(10).
000177     05  sql-py-Name             pic x(50).
000178     05  sql-py-Addr1            pic x(50).
000179     05  sql-py-Addr2            pic x(50).
000180     05  sql-py-AddrCity         pic x(50).
000181     05  sql-py-AddrState        pic xx.
000182     05  sql-py-AddrZIP          pic x(9).
000183     05  sql-py-ResponsibleCarrier
000184                                 pic x.
000185     05  sql-py-ResponsibleNumber
000186                                 pic x(10).
000187     05  sql-py-LogicStatus      pic 9(7).
000188     05  sql-py-LogicStatusDate  pic x(30).
000189     05  sql-py-company          pic xxx.
000190     05  sql-py-tot-prem         pic x(12).
000191     05  sql-py-tot-prem-n redefines
000192         sql-py-tot-prem         pic 9(9).99.
000193     05  sql-is-ach              pic x.
000194
000195 01  sql-cert-records.
000196     05  sql-pb-CertId           pic 9(9).
000197     05  sql-pb-RemitId          pic 9(9).
000198     05  sql-pb-CertNumber       pic x(11).
000199     05  sql-pb-EffectiveDate    pic x(30).
000200     05  sql-pb-FirstPaymentDate pic x(30).
000201     05  sql-pb-FirstName        pic x(30).
000202     05  sql-pb-LastName         pic x(30).
000203     05  sql-pb-CashIndicator    pic x.
000204     05  sql-pb-LivesCovered     pic 9(7).
000205     05  sql-pb-LfBenCode        pic xx.
000206     05  sql-pb-LfTerm           pic 999.
000207     05  sql-pb-LfBenefit        pic x(12).
000208     05  sql-pb-lfbenefit-n redefines
000209         sql-pb-lfbenefit        pic 9(9).99.
000210     05  sql-pb-LfPremium        pic x(10).
000211     05  sql-pb-lfpremium-n redefines
000212         sql-pb-lfpremium        pic 9(7).99.
000213     05  sql-pb-AhBenCode        pic xx.
000214     05  sql-pb-AhTerm           pic 999.
000215     05  sql-pb-AhBenefit        pic x(12).
000216     05  sql-pb-ahbenefit-n redefines
000217         sql-pb-ahbenefit        pic 9(9).99.
000218     05  sql-pb-AhPremium        pic x(10).
000219     05  sql-pb-ahpremium-n redefines
000220         sql-pb-ahpremium        pic 9(7).99.
000221     05  sql-pb-ErrorMessage     pic x(50).
000222
000224 EXEC SQL
          END DECLARE SECTION
000225 END-EXEC
000226
000227 01  ws-roll-back-keys.
000228     05  ws-rb-erpyaj-key        pic x(33) value spaces.
000229 01  ws-batch-header-work-area.
000230     05  ws-lf-iss-premium          pic s9(9)v99 comp-3 value +0.
000231     05  ws-ah-iss-premium          pic s9(9)v99 comp-3 value +0.
000232     05  ws-iss-cnt                 pic s9(3) comp-3 value +0.
000233     05  ws-cnc-cnt                 pic s9(3) comp-3 value +0.
000234     05  ws-highest-seq-no          pic s9(4) comp value +0.
000235
000236 01  FILE-KEYS.
000237     12  ELCNTL-KEY.
000238         16 ELCNTL-COMP-ID           PIC XXX     VALUE SPACES.
000239         16 ELCNTL-REC-TYPE          PIC X       VALUE SPACES.
000240         16 ELCNTL-ACCESS.
000241             20 ELCNTL-STATE         PIC XX      VALUE SPACES.
000242             20  FILLER              PIC X       VALUE SPACES.
000243             20 ELCNTL-CARRIER       PIC X       VALUE SPACES.
000244         16 ELCNTL-SEQ               PIC S9(4)   VALUE +0    COMP.
000245
000246 01  ws-init-erpndb-rec          pic x(585) value spaces.
000247 01  ws-init-erpndm              pic x(374) value spaces.
000248 01  f.
000249     05  ws-batch-seq-no         pic s9(4) comp value +0.
000250     05  ws-connect-sw               pic x  value ' '.
000251         88  connected-to-db             value 'Y'.
000252     05  ws-comp-id              pic xxx.
000253     05  ws-comp-cd              pic x.
000254
000255 01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
000256     88  RESP-NORMAL                    VALUE +0.
000257     88  resp-file-notfnd               value +12.
000258     88  RESP-NOTFND                    VALUE +13.
000259     88  resp-duprec                    value +14.
000260     88  resp-dupkey                    value +15.
000261     88  resp-invreq                    value +16.
000262     88  RESP-NOTOPEN                   VALUE +19.
000263     88  RESP-ENDFILE                   VALUE +20.
000264     88  resp-lengtherr                 value +22.
000265
000266 01  ws-display-response         pic s9(5) value zeros.
000267 01  ws-work-time-a              pic x(6).
000268 01  ws-work-time redefines ws-work-time-a
000269                                 pic 9(6).
000270 01  ws-work-amt-alpha           pic x(11).
000271 01  ws-work-amt-num redefines ws-work-amt-alpha
000272                                 pic 9(9)v99.
000273 01  ws-qry-string               pic x(80) value spaces.
000274 01  ws-qrystr-len               pic s9(8) comp value +60.
000275 01  ws-var-alpha                pic x(10).
000276 01  ws-seq-alpha                pic x(5).
000277 01  ws-seq-no-num redefines ws-seq-alpha
000278                                 pic 9(5).
000279
000280*                                copy ELCFUNDT.
      *>>((file: ELCFUNDT))
000001*****************************************************************
000002*                                                               *
000003*                            ELCFUNDT.                          *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE *
000005*                            VMOD=2.001                         *
000006*                                                               *
000007*           COPYBOOK FOR THE FUNCTION DATE FORMAT               *
000008*                                                               *
000009*****************************************************************
000010
000011
000012 01  FUNCTION-DATE.
000013     05  WS-FN-DATE                PIC 9(8)    VALUE ZEROS.
000014     05  WS-FN-CYMD  REDEFINES  WS-FN-DATE.
000015         10  WS-FN-CCYR            PIC 9(4).
000016         10  WS-FN-CCYY  REDEFINES  WS-FN-CCYR.
000017             15  WS-FN-CC          PIC 99.
000018             15  WS-FN-YR          PIC 99.
000019         10  WS-FN-MO              PIC 99.
000020         10  WS-FN-DA              PIC 99.
000021     05  WS-FN-HOURS               PIC 99      VALUE ZEROS.
000022     05  WS-FN-MINUTES             PIC 99      VALUE ZEROS.
000023     05  WS-FN-SECONDS             PIC 99      VALUE ZEROS.
000024     05  WS-FN-HUNDSECS            PIC 99      VALUE ZEROS.
000025     05  WS-FN-GMT-IND             PIC X       VALUE SPACES.
000026         88  WS-BEHIND-GMT                     VALUE '-'.
000027         88  WS-AFTER-GMT                      VALUE '+'.
000028         88  WS-NO-GMT                         VALUE ZERO.
000029     05  WS-FN-GMT-HOURS           PIC 99      VALUE ZEROS.
000030     05  WS-FN-GMT-MINUTES         PIC 99      VALUE ZEROS.
000031
      *<<((file: ELCFUNDT))
000281*                                COPY ELCDATE.
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
000282*                                COPY ELCCNTL.
      *>>((file: ELCCNTL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCCNTL.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.059                          *
000007*                                                                *
000008*   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 750  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
000014*       ALTERNATE INDEX = NONE                                   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 082503                   PEMA  ADD BENEFIT GROUP
000027* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000028* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
000029* 092705    2005050300006  PEMA  ADD SPP LEASES
000030* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
000031* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
000032* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
000033* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
000034* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
000035* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
000036* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
000037* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
000038* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
000039* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000040* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
000041******************************************************************
000042*
000043 01  CONTROL-FILE.
000044     12  CF-RECORD-ID                       PIC XX.
000045         88  VALID-CF-ID                        VALUE 'CF'.
000046
000047     12  CF-CONTROL-PRIMARY.
000048         16  CF-COMPANY-ID                  PIC XXX.
000049         16  CF-RECORD-TYPE                 PIC X.
000050             88  CF-COMPANY-MASTER              VALUE '1'.
000051             88  CF-PROCESSOR-MASTER            VALUE '2'.
000052             88  CF-STATE-MASTER                VALUE '3'.
000053             88  CF-LF-BENEFIT-MASTER           VALUE '4'.
000054             88  CF-AH-BENEFIT-MASTER           VALUE '5'.
000055             88  CF-CARRIER-MASTER              VALUE '6'.
000056             88  CF-MORTALITY-MASTER            VALUE '7'.
000057             88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
000058             88  CF-TERMINAL-MASTER             VALUE '9'.
000059             88  CF-AH-EDIT-MASTER              VALUE 'A'.
000060             88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
000061             88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
000062             88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
000063             88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
000064             88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
000065             88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
000066             88  CF-REMINDERS-MASTER            VALUE 'R'.
000067             88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
000068         16  CF-ACCESS-CD-GENL              PIC X(4).
000069         16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
000070             20  CF-PROCESSOR               PIC X(4).
000071         16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
000072             20  CF-STATE-CODE              PIC XX.
000073             20  FILLER                     PIC XX.
000074         16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
000075             20  FILLER                     PIC XX.
000076             20  CF-HI-BEN-IN-REC           PIC XX.
000077         16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
000078             20  FILLER                     PIC XXX.
000079             20  CF-CARRIER-CNTL            PIC X.
000080         16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
000081             20  FILLER                     PIC XX.
000082             20  CF-HI-TYPE-IN-REC          PIC 99.
000083         16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
000084             20  CF-CRDB-TABLE-INDICATOR    PIC X.
000085                 88  CF-CRDB-NAIC-TABLE         VALUE '9'.
000086             20  CF-CRDB-BENEFIT-TYPE       PIC X.
000087             20  CF-CRDB-WAITING-PERIOD     PIC XX.
000088         16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
000089             20  FILLER                     PIC X.
000090             20  CF-CUSTOM-REPORT-NO        PIC 999.
000091         16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
000092             20  FILLER                     PIC XX.
000093             20  CF-MORTGAGE-PLAN           PIC XX.
000094         16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
000095
000096     12  CF-LAST-MAINT-DT                   PIC XX.
000097     12  CF-LAST-MAINT-BY                   PIC X(4).
000098     12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
000099
000100     12  CF-RECORD-BODY                     PIC X(728).
000101
000102
000103****************************************************************
000104*             COMPANY MASTER RECORD                            *
000105****************************************************************
000106
000107     12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000108         16  CF-COMPANY-ADDRESS.
000109             20  CF-CL-MAIL-TO-NAME         PIC X(30).
000110             20  CF-CL-IN-CARE-OF           PIC X(30).
000111             20  CF-CL-ADDR-LINE-1          PIC X(30).
000112             20  CF-CL-ADDR-LINE-2          PIC X(30).
000113             20  CF-CL-CITY-STATE           PIC X(30).
000114             20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
000115             20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
000116         16  CF-COMPANY-CD                  PIC X.
000117         16  CF-COMPANY-PASSWORD            PIC X(8).
000118         16  CF-SECURITY-OPTION             PIC X.
000119             88  ALL-SECURITY                   VALUE '1'.
000120             88  COMPANY-VERIFY                 VALUE '2'.
000121             88  PROCESSOR-VERIFY               VALUE '3'.
000122             88  NO-SECURITY                    VALUE '4'.
000123             88  ALL-BUT-TERM                   VALUE '5'.
000124         16  CF-CARRIER-CONTROL-LEVEL       PIC X.
000125             88  USE-ACTUAL-CARRIER             VALUE SPACE.
000126         16  CF-LGX-INTERFACE-CNTL          PIC X.
000127             88  LGX-TIME-SHR-COMPANY           VALUE '1'.
000128         16  CF-INFORCE-LOCATION            PIC X.
000129             88  CERTS-ARE-ONLINE               VALUE '1'.
000130             88  CERTS-ARE-OFFLINE              VALUE '2'.
000131             88  NO-CERTS-AVAILABLE             VALUE '3'.
000132         16  CF-LOWER-CASE-LETTERS          PIC X.
000133         16  CF-CERT-ACCESS-CONTROL         PIC X.
000134             88  CF-ST-ACCNT-CNTL               VALUE ' '.
000135             88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
000136             88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
000137             88  CF-ACCNT-CNTL                  VALUE '3'.
000138             88  CF-CARR-ACCNT-CNTL             VALUE '4'.
000139
000140         16  CF-FORMS-PRINTER-ID            PIC X(4).
000141         16  CF-CHECK-PRINTER-ID            PIC X(4).
000142
000143         16  CF-LGX-CREDIT-USER             PIC X.
000144             88  CO-IS-NOT-USER                 VALUE 'N'.
000145             88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
000146
000147         16 CF-CREDIT-CALC-CODES.
000148             20  CF-CR-REM-TERM-CALC PIC X.
000149               88  CR-EARN-AFTER-15TH           VALUE '1'.
000150               88  CR-EARN-ON-HALF-MO           VALUE '2'.
000151               88  CR-EARN-ON-1ST-DAY           VALUE '3'.
000152               88  CR-EARN-ON-FULL-MO           VALUE '4'.
000153               88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
000154               88  CR-EARN-AFTER-14TH           VALUE '6'.
000155               88  CR-EARN-AFTER-16TH           VALUE '7'.
000156             20  CF-CR-R78-METHOD           PIC X.
000157               88  USE-TERM-PLUS-ONE            VALUE SPACE.
000158               88  DONT-USE-PLUS-ONE            VALUE '1'.
000159
000160         16  CF-CLAIM-CONTROL-COUNTS.
000161             20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
000162                 88  CO-CLM-COUNT-RESET         VALUE +99999.
000163
000164             20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
000165                 88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
000166
000167             20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
000168                 88  CO-CHECK-COUNT-RESET       VALUE +9999999.
000169
000170             20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
000171                 88  CO-QUE-COUNT-RESET         VALUE +9999999.
000172
000173         16  CF-CURRENT-MONTH-END           PIC XX.
000174
000175         16  CF-CO-CALC-QUOTE-TOLERANCE.
000176             20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
000177             20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
000178             20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
000179             20  CF-CO-CLAIM-REJECT-SW      PIC X.
000180                 88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
000181                 88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
000182             20  CF-CO-PREM-REJECT-SW       PIC X.
000183                 88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
000184                 88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
000185             20  CF-CO-REF-REJECT-SW        PIC X.
000186                 88 CO-WARN-IF-REF-OUT          VALUE SPACE.
000187                 88 CO-FORCE-IF-REF-OUT         VALUE '1'.
000188
000189         16  CF-CO-REPORTING-DT             PIC XX.
000190         16  CF-CO-REPORTING-MONTH-DT       PIC XX.
000191         16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
000192           88  CF-CO-NOT-MONTH-END              VALUE SPACES.
000193           88  CF-CO-MONTH-END                  VALUE '1'.
000194
000195         16  CF-LGX-CLAIM-USER              PIC X.
000196             88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
000197             88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
000198
000199         16  CF-CREDIT-EDIT-CONTROLS.
000200             20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
000201             20  CF-MIN-AGE                 PIC 99.
000202             20  CF-DEFAULT-AGE             PIC 99.
000203             20  CF-MIN-TERM                PIC S999      COMP-3.
000204             20  CF-MAX-TERM                PIC S999      COMP-3.
000205             20  CF-DEFAULT-SEX             PIC X.
000206             20  CF-JOINT-AGE-INPUT         PIC X.
000207                 88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
000208             20  CF-BIRTH-DATE-INPUT        PIC X.
000209                 88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
000210             20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
000211                 88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
000212                 88  CF-ZERO-CARRIER            VALUE '1'.
000213                 88  CF-ZERO-GROUPING           VALUE '2'.
000214                 88  CF-ZERO-CAR-GROUP          VALUE '3'.
000215             20  CF-EDIT-SW                 PIC X.
000216                 88  CF-START-EDIT-TONIGHT      VALUE '1'.
000217             20  CF-EDIT-RESTART-BATCH      PIC X(6).
000218             20  CF-CR-PR-METHOD            PIC X.
000219               88  USE-NORMAL-PR-METHOD         VALUE SPACE.
000220               88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
000221             20  FILLER                     PIC X.
000222
000223         16  CF-CREDIT-MISC-CONTROLS.
000224             20  CF-REIN-TABLE-SW           PIC X.
000225                 88 REIN-TABLES-ARE-USED        VALUE '1'.
000226             20  CF-COMP-TABLE-SW           PIC X.
000227                 88 COMP-TABLES-ARE-USED        VALUE '1'.
000228             20  CF-EXPERIENCE-RETENTION-AGE
000229                                            PIC S9        COMP-3.
000230             20  CF-CONVERSION-DT           PIC XX.
000231             20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
000232             20  CF-RUN-FREQUENCY-SW        PIC X.
000233                 88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
000234                 88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
000235
000236             20  CF-CR-CHECK-NO-CONTROL.
000237                 24  CF-CR-CHECK-NO-METHOD    PIC X.
000238                     88  CR-CHECK-NO-MANUAL       VALUE '1'.
000239                     88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
000240                     88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
000241                 24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
000242                     88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
000243
000244                 24  CF-CR-CHECK-COUNT       REDEFINES
000245                     CF-CR-CHECK-COUNTER      PIC X(4).
000246
000247                 24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
000248                     88  CR-QUE-COUNT-RESET      VALUE +9999999.
000249
000250                 24  CF-CR-CHECK-QUE-COUNT   REDEFINES
000251                     CF-CR-CHECK-QUE-COUNTER  PIC X(4).
000252                 24  CF-MAIL-PROCESSING       PIC X.
000253                     88  MAIL-PROCESSING          VALUE 'Y'.
000254
000255         16  CF-MISC-SYSTEM-CONTROL.
000256             20  CF-SYSTEM-C                 PIC X.
000257                 88  CONFIRMATION-SYS-USED       VALUE '1'.
000258             20  CF-SYSTEM-D                 PIC X.
000259                 88  DAILY-BILL-SYS-USED         VALUE '1'.
000260             20  CF-SOC-SEC-NO-SW            PIC X.
000261                 88  SOC-SEC-NO-USED             VALUE '1'.
000262             20  CF-MEMBER-NO-SW             PIC X.
000263                 88  MEMBER-NO-USED              VALUE '1'.
000264             20  CF-TAX-ID-NUMBER            PIC X(11).
000265             20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
000266             20  CF-PAYMENT-APPROVAL-SW      PIC X.
000267                 88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
000268                 88  CF-NO-APPROVAL              VALUE ' ' 'N'.
000269                 88  CF-ALL-APPROVED             VALUE 'Y'.
000270                 88  CF-GRADUATED-APPROVAL       VALUE 'G'.
000271             20  CF-SYSTEM-E                 PIC X.
000272                 88  CF-AR-SYSTEM-USED           VALUE 'Y'.
000273
000274         16  CF-LGX-LIFE-USER               PIC X.
000275             88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
000276             88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
000277
000278         16  CF-CR-MONTH-END-DT             PIC XX.
000279
000280         16  CF-FILE-MAINT-DATES.
000281             20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
000282                 88  CF-LAST-BATCH-RESET        VALUE +999999.
000283             20  CF-LAST-BATCH       REDEFINES
000284                 CF-LAST-BATCH-NO               PIC X(4).
000285             20  CF-RATES-FILE-MAINT-DT         PIC XX.
000286             20  CF-RATES-FILE-CREATE-DT        PIC XX.
000287             20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
000288             20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
000289             20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
000290             20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
000291             20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
000292             20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
000293             20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
000294             20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
000295
000296         16  CF-NEXT-COMPANY-ID             PIC XXX.
000297         16  FILLER                         PIC X.
000298
000299         16  CF-ALT-MORT-CODE               PIC X(4).
000300         16  CF-MEMBER-CAPTION              PIC X(10).
000301
000302         16  CF-LIFE-ACCESS-CONTROL         PIC X.
000303             88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
000304             88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
000305             88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
000306             88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
000307             88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
000308
000309         16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
000310
000311         16  CF-LIFE-OVERRIDE-L1            PIC X.
000312         16  CF-LIFE-OVERRIDE-L2            PIC XX.
000313         16  CF-LIFE-OVERRIDE-L6            PIC X(6).
000314         16  CF-LIFE-OVERRIDE-L12           PIC X(12).
000315
000316         16  CF-AH-OVERRIDE-L1              PIC X.
000317         16  CF-AH-OVERRIDE-L2              PIC XX.
000318         16  CF-AH-OVERRIDE-L6              PIC X(6).
000319         16  CF-AH-OVERRIDE-L12             PIC X(12).
000320
000321         16  CF-REPORT-CD1-CAPTION          PIC X(10).
000322         16  CF-REPORT-CD2-CAPTION          PIC X(10).
000323
000324         16  CF-CLAIM-CUTOFF-DATE           PIC XX.
000325         16  CF-AR-LAST-EL860-DT            PIC XX.
000326         16  CF-MP-MONTH-END-DT             PIC XX.
000327
000328         16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
000329         16  CF-CLAIM-PAID-THRU-TO          PIC X.
000330             88  CF-CLAIM-PAID-TO               VALUE '1'.
000331
000332         16  CF-AR-MONTH-END-DT             PIC XX.
000333
000334         16  CF-CRDTCRD-USER                PIC X.
000335             88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
000336             88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
000337
000338         16  CF-CC-MONTH-END-DT             PIC XX.
000339
000340         16  CF-PRINT-ADDRESS-LABELS        PIC X.
000341
000342         16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
000343             88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
000344             88  CF-USE-ALL-AGE-LAST            VALUE '2'.
000345             88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
000346         16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
000347         16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
000348         16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
000349         16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
000350             88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
000351             88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
000352         16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
000353         16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
000354         16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
000355             88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
000356         16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
000357
000358         16  CF-CL-ZIP-CODE.
000359             20  CF-CL-ZIP-PRIME.
000360                 24  CF-CL-ZIP-1ST          PIC X.
000361                     88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
000362                 24  FILLER                 PIC X(4).
000363             20  CF-CL-ZIP-PLUS4            PIC X(4).
000364         16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
000365             20  CF-CL-CAN-POSTAL-1         PIC XXX.
000366             20  CF-CL-CAN-POSTAL-2         PIC XXX.
000367             20  FILLER                     PIC XXX.
000368
000369         16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
000370         16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
000371         16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
000372         16  CF-CO-OPTION-START-DATE        PIC XX.
000373         16  CF-REM-TRM-CALC-OPTION         PIC X.
000374           88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
000375                                                      '3' '4'.
000376           88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
000377           88  CF-30-DAY-MONTH                  VALUE '1' '3'.
000378           88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
000379           88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
000380           88  CF-EXT-30-DAY-MONTH              VALUE '3'.
000381           88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
000382
000383         16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
000384
000385         16  CF-PAYMENT-APPROVAL-LEVELS.
000386             20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
000387             20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
000388             20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
000389             20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
000390             20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
000391             20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
000392
000393         16  CF-END-USER-REPORTING-USER     PIC X.
000394             88  CO-NO-END-USER-REPORTING       VALUE 'N'.
000395             88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
000396
000397         16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
000398             88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
000399             88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
000400
000401         16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
000402
000403         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
000404         16  FILLER                         PIC X.
000405
000406         16  CF-CREDIT-ARCHIVE-CNTL.
000407             20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
000408             20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
000409             20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
000410
000411         16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
000412
000413         16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
000414             88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
000415             88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
000416
000417         16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
000418             88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
000419             88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
000420
000421         16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
000422
000423         16  CF-CO-ACH-ID-CODE              PIC  X.
000424             88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
000425             88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
000426             88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
000427         16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
000428         16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
000429         16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
000430         16  CF-CO-ACH-ADMIN-NO             PIC X(09).
000431         16  CF-CO-ACH-RECV-NAME            PIC X(23).
000432         16  CF-CO-ACH-RECV-NO              PIC X(08).
000433         16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
000434         16  CF-CO-ACH-COMPANY-ID           PIC X(09).
000435         16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
000436                 88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
000437         16  CF-CO-ACH-TRACE-SPACE REDEFINES
000438                 CF-CO-ACH-TRACE-NO         PIC X(4).
000439
000440         16  CF-CO-OVER-SHORT.
000441             20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
000442             20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
000443
000444*         16  FILLER                         PIC X(102).
000445         16  CF-PAYMENT-APPROVAL-LEVELS-2.
000446             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
000447             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
000448
000449         16  CF-AH-APPROVAL-DAYS.
000450             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
000451             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
000452             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
000453             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
000454
000455         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
000456
000457         16  CF-APPROV-LEV-5.
000458             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
000459             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
000460             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
000461
000462         16  FILLER                         PIC X(68).
000463****************************************************************
000464*             PROCESSOR/USER RECORD                            *
000465****************************************************************
000466
000467     12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000468         16  CF-PROCESSOR-NAME              PIC X(30).
000469         16  CF-PROCESSOR-PASSWORD          PIC X(11).
000470         16  CF-PROCESSOR-TITLE             PIC X(26).
000471         16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
000472                 88  MESSAGE-YES                VALUE 'Y'.
000473                 88  MESSAGE-NO                 VALUE ' ' 'N'.
000474
000475*****************************************************
000476****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
000477****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
000478****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
000479****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
000480*****************************************************
000481
000482         16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
000483             20  CF-ADMINISTRATION-CONTROLS PIC XX.
000484             20  CF-APPLICATION-FORCE       PIC X.
000485             20  CF-INDIVIDUAL-APP.
000486                 24  CF-APP-SWITCHES  OCCURS  44 TIMES.
000487                     28  CF-BROWSE-APP      PIC X.
000488                     28  CF-UPDATE-APP      PIC X.
000489
000490         16  CF-CURRENT-TERM-ON             PIC X(4).
000491         16  CF-PROCESSOR-LIMITS-CLAIMS.
000492             20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
000493             20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
000494             20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
000495             20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
000496             20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
000497             20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
000498             20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
000499         16  CF-PROCESSOR-CARRIER           PIC X.
000500             88  NO-CARRIER-SECURITY            VALUE ' '.
000501         16  CF-PROCESSOR-ACCOUNT           PIC X(10).
000502             88  NO-ACCOUNT-SECURITY            VALUE SPACES.
000503         16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
000504             88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
000505         16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
000506             88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
000507
000508         16  CF-PROC-SYS-ACCESS-SW.
000509             20  CF-PROC-CREDIT-CLAIMS-SW.
000510                 24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
000511                     88  ACCESS-TO-CREDIT           VALUE 'Y'.
000512                 24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
000513                     88  ACCESS-TO-CLAIMS           VALUE 'Y'.
000514             20  CF-PROC-CREDIT-CLAIMS   REDEFINES
000515                 CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
000516                 88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
000517             20  CF-PROC-LIFE-GNRLDGR-SW.
000518                 24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
000519                     88  ACCESS-TO-LIFE             VALUE 'Y'.
000520                 24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
000521                     88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
000522             20  CF-PROC-LIFE-GNRLDGR    REDEFINES
000523                 CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
000524                 88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
000525         16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
000526             CF-PROC-SYS-ACCESS-SW              PIC X(4).
000527             88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
000528         16  CF-PROCESSOR-PRINTER               PIC X(4).
000529
000530         16  CF-APPROVAL-LEVEL                  PIC X.
000531             88  APPROVAL-LEVEL-1                   VALUE '1'.
000532             88  APPROVAL-LEVEL-2                   VALUE '2'.
000533             88  APPROVAL-LEVEL-3                   VALUE '3'.
000534             88  APPROVAL-LEVEL-4                   VALUE '4'.
000535             88  APPROVAL-LEVEL-5                   VALUE '5'.
000536
000537         16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
000538
000539         16  CF-LANGUAGE-TYPE                   PIC X.
000540             88  CF-LANG-IS-ENG                     VALUE 'E'.
000541             88  CF-LANG-IS-FR                      VALUE 'F'.
000542
000543         16  CF-CSR-IND                         PIC X.
000544         16  FILLER                             PIC X(239).
000545
000546****************************************************************
000547*             PROCESSOR/REMINDERS RECORD                       *
000548****************************************************************
000549
000550     12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
000551         16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
000552             20  CF-START-REMIND-DT         PIC XX.
000553             20  CF-END-REMIND-DT           PIC XX.
000554             20  CF-REMINDER-TEXT           PIC X(50).
000555         16  FILLER                         PIC X(296).
000556
000557
000558****************************************************************
000559*             STATE MASTER RECORD                              *
000560****************************************************************
000561
000562     12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000563         16  CF-STATE-ABBREVIATION          PIC XX.
000564         16  CF-STATE-NAME                  PIC X(25).
000565         16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
000566         16  CF-ST-CALC-QUOTE-TOLERANCE.
000567             20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
000568             20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
000569             20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
000570             20  CF-ST-CLAIM-REJECT-SW      PIC X.
000571                 88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
000572                 88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
000573             20  CF-ST-PREM-REJECT-SW       PIC X.
000574                 88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
000575                 88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
000576             20  CF-ST-REF-REJECT-SW        PIC X.
000577                 88 ST-WARN-IF-REF-OUT          VALUE SPACE.
000578                 88 ST-FORCE-IF-REF-OUT         VALUE '1'.
000579         16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
000580         16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
000581         16  CF-ST-REFUND-RULES.
000582             20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
000583             20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
000584             20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
000585         16  CF-ST-FST-PMT-EXTENSION.
000586             20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
000587             20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
000588                 88  CF-ST-EXT-NO-CHG           VALUE ' '.
000589                 88  CF-ST-EXT-CHG-LF           VALUE '1'.
000590                 88  CF-ST-EXT-CHG-AH           VALUE '2'.
000591                 88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
000592         16  CF-ST-STATE-CALL.
000593             20  CF-ST-CALL-UNEARNED        PIC X.
000594             20  CF-ST-CALL-RPT-CNTL        PIC X.
000595             20  CF-ST-CALL-RATE-DEV        PIC XXX.
000596         16  CF-REPLACEMENT-LAW-SW          PIC X.
000597             88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
000598             88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
000599         16  CF-REPLACEMENT-LETTER          PIC X(4).
000600         16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
000601         16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
000602         16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
000603         16  CF-ST-SPLIT-PAYMENT            PIC X.
000604         16  FILLER                         PIC X.
000605         16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
000606             20  CF-ST-BENEFIT-CD           PIC XX.
000607             20  CF-ST-BENEFIT-KIND         PIC X.
000608                 88  CF-ST-LIFE-KIND            VALUE 'L'.
000609                 88  CF-ST-AH-KIND              VALUE 'A'.
000610             20  CF-ST-REM-TERM-CALC        PIC X.
000611                 88  ST-REM-TERM-NOT-USED       VALUE SPACE.
000612                 88  ST-EARN-AFTER-15TH         VALUE '1'.
000613                 88  ST-EARN-ON-HALF-MO         VALUE '2'.
000614                 88  ST-EARN-ON-1ST-DAY         VALUE '3'.
000615                 88  ST-EARN-ON-FULL-MO         VALUE '4'.
000616                 88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
000617                 88  ST-EARN-AFTER-14TH         VALUE '6'.
000618                 88  ST-EARN-AFTER-16TH         VALUE '7'.
000619
000620             20  CF-ST-REFUND-CALC          PIC X.
000621                 88  ST-REFUND-NOT-USED         VALUE SPACE.
000622                 88  ST-REFD-BY-R78             VALUE '1'.
000623                 88  ST-REFD-BY-PRO-RATA        VALUE '2'.
000624                 88  ST-REFD-AS-CALIF           VALUE '3'.
000625                 88  ST-REFD-AS-TEXAS           VALUE '4'.
000626                 88  ST-REFD-IS-NET-PAY         VALUE '5'.
000627                 88  ST-REFD-ANTICIPATION       VALUE '6'.
000628                 88  ST-REFD-UTAH               VALUE '7'.
000629                 88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
000630                 88  ST-REFD-REG-BALLOON        VALUE 'B'.
000631                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
000632
000633             20  CF-ST-EARNING-CALC         PIC X.
000634                 88  ST-EARNING-NOT-USED        VALUE SPACE.
000635                 88  ST-EARN-BY-R78             VALUE '1'.
000636                 88  ST-EARN-BY-PRO-RATA        VALUE '2'.
000637                 88  ST-EARN-AS-CALIF           VALUE '3'.
000638                 88  ST-EARN-AS-TEXAS           VALUE '4'.
000639                 88  ST-EARN-IS-NET-PAY         VALUE '5'.
000640                 88  ST-EARN-ANTICIPATION       VALUE '6'.
000641                 88  ST-EARN-MEAN               VALUE '8'.
000642                 88  ST-EARN-REG-BALLOON        VALUE 'B'.
000643
000644             20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
000645                 88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
000646                 88  ST-OVRD-BY-R78             VALUE '1'.
000647                 88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
000648                 88  ST-OVRD-AS-CALIF           VALUE '3'.
000649                 88  ST-OVRD-AS-TEXAS           VALUE '4'.
000650                 88  ST-OVRD-IS-NET-PAY         VALUE '5'.
000651                 88  ST-OVRD-ANTICIPATION       VALUE '6'.
000652                 88  ST-OVRD-MEAN               VALUE '8'.
000653                 88  ST-OVRD-REG-BALLOON        VALUE 'B'.
000654             20  cf-st-extra-periods        pic 9.
000655*            20  FILLER                     PIC X.
000656
000657         16  CF-ST-COMMISSION-CAPS.
000658             20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
000659             20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
000660             20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
000661             20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
000662         16  CF-COMM-CAP-LIMIT-TO           PIC X.
000663                 88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
000664                 88  ST-LIMIT-TO-GA             VALUE 'G'.
000665                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
000666
000667         16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
000668
000669         16  CF-ST-STATUTORY-INTEREST.
000670             20  CF-ST-STAT-DATE-FROM       PIC X.
000671                 88  ST-STAT-FROM-INCURRED      VALUE 'I'.
000672                 88  ST-STAT-FROM-REPORTED      VALUE 'R'.
000673             20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
000674             20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
000675             20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
000676             20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
000677             20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
000678
000679         16  CF-ST-OVER-SHORT.
000680             20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
000681             20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
000682
000683         16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
000684
000685         16  CF-ST-RT-CALC                  PIC X.
000686
000687         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
000688         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
000689         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
000690         16  CF-ST-RF-LR-CALC               PIC X.
000691         16  CF-ST-RF-LL-CALC               PIC X.
000692         16  CF-ST-RF-LN-CALC               PIC X.
000693         16  CF-ST-RF-AH-CALC               PIC X.
000694         16  CF-ST-RF-CP-CALC               PIC X.
000695*        16  FILLER                         PIC X(206).
000696*CIDMOD         16  FILLER                         PIC X(192).
000697         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
000698             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
000699         16  CF-ST-REF-AH-DEATH-IND         PIC X.
000700         16  CF-ST-VFY-2ND-BENE             PIC X.
000701         16  CF-ST-CAUSAL-STATE             PIC X.
000702         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
000703         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
000704         16  CF-ST-AGENT-SIG-EDIT           PIC X.
000705             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
000706         16  CF-ST-NET-ONLY-STATE           PIC X.
000707             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
000708         16  cf-commission-cap-required     pic x.
000709         16  CF-ST-GA-COMMISSION-CAPS.
000710             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
000711             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
000712             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
000713             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
000714         16  CF-ST-TOT-COMMISSION-CAPS.
000715             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
000716             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
000717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
000718             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
000719         16  FILLER                         PIC X(156).
000720
000721****************************************************************
000722*             BENEFIT MASTER RECORD                            *
000723****************************************************************
000724
000725     12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000726         16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
000727             20  CF-BENEFIT-CODE            PIC XX.
000728             20  CF-BENEFIT-NUMERIC  REDEFINES
000729                 CF-BENEFIT-CODE            PIC XX.
000730             20  CF-BENEFIT-ALPHA           PIC XXX.
000731             20  CF-BENEFIT-DESCRIP         PIC X(10).
000732             20  CF-BENEFIT-COMMENT         PIC X(10).
000733
000734             20  CF-LF-COVERAGE-TYPE        PIC X.
000735                 88  CF-REDUCING                VALUE 'R'.
000736                 88  CF-LEVEL                   VALUE 'L' 'P'.
000737
000738             20  CF-SPECIAL-CALC-CD         PIC X.
000739                 88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
000740                 88  CF-NP-0-MO-INT             VALUE 'A'.
000741                 88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
000742                 88  CF-CRITICAL-PERIOD         VALUE 'C'.
000743                 88  CF-TERM-IN-DAYS            VALUE 'D'.
000744                 88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
000745                 88  CF-FARM-PLAN               VALUE 'F'.
000746                 88  CF-RATE-AS-STANDARD        VALUE 'G'.
000747                 88  CF-2-MTH-INTEREST          VALUE 'I'.
000748                 88  CF-3-MTH-INTEREST          VALUE 'J'.
000749                 88  CF-4-MTH-INTEREST          VALUE 'K'.
000750                 88  CF-BALLOON-LAST-PMT        VALUE 'L'.
000751                 88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
000752                 88  CF-PRUDENTIAL              VALUE 'P'.
000753                 88  CF-OUTSTANDING-BAL         VALUE 'O'.
000754                 88  CF-TRUNCATED-LIFE          VALUE 'T'.
000755                 88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
000756                 88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
000757                 88  CF-NET-PAY-SIMPLE          VALUE 'S'.
000758                 88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
000759
000760             20  CF-JOINT-INDICATOR         PIC X.
000761                 88  CF-JOINT-COVERAGE          VALUE 'J'.
000762
000763*            20  FILLER                     PIC X(12).
000764             20  cf-maximum-benefits        pic s999 comp-3.
000765             20  FILLER                     PIC X(09).
000766             20  CF-BENEFIT-CATEGORY        PIC X.
000767             20  CF-LOAN-TYPE               PIC X(8).
000768
000769             20  CF-CO-REM-TERM-CALC        PIC X.
000770                 88  CO-EARN-AFTER-15TH         VALUE '1'.
000771                 88  CO-EARN-ON-HALF-MO         VALUE '2'.
000772                 88  CO-EARN-ON-1ST-DAY         VALUE '3'.
000773                 88  CO-EARN-ON-FULL-MO         VALUE '4'.
000774                 88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
000775
000776             20  CF-CO-EARNINGS-CALC        PIC X.
000777                 88  CO-EARN-BY-R78             VALUE '1'.
000778                 88  CO-EARN-BY-PRO-RATA        VALUE '2'.
000779                 88  CO-EARN-AS-CALIF           VALUE '3'.
000780                 88  CO-EARN-AS-TEXAS           VALUE '4'.
000781                 88  CO-EARN-IS-NET-PAY         VALUE '5'.
000782                 88  CO-EARN-ANTICIPATION       VALUE '6'.
000783                 88  CO-EARN-AS-MEAN            VALUE '8'.
000784                 88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
000785
000786             20  CF-CO-REFUND-CALC          PIC X.
000787                 88  CO-REFUND-NOT-USED         VALUE SPACE.
000788                 88  CO-REFD-BY-R78             VALUE '1'.
000789                 88  CO-REFD-BY-PRO-RATA        VALUE '2'.
000790                 88  CO-REFD-AS-CALIF           VALUE '3'.
000791                 88  CO-REFD-AS-TEXAS           VALUE '4'.
000792                 88  CO-REFD-IS-NET-PAY         VALUE '5'.
000793                 88  CO-REFD-ANTICIPATION       VALUE '6'.
000794                 88  CO-REFD-MEAN               VALUE '8'.
000795                 88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
000796                 88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
000797                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
000798
000799             20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
000800                 88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
000801                 88  CO-OVRD-BY-R78             VALUE '1'.
000802                 88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
000803                 88  CO-OVRD-AS-CALIF           VALUE '3'.
000804                 88  CO-OVRD-AS-TEXAS           VALUE '4'.
000805                 88  CO-OVRD-IS-NET-PAY         VALUE '5'.
000806                 88  CO-OVRD-ANTICIPATION       VALUE '6'.
000807                 88  CO-OVRD-MEAN               VALUE '8'.
000808                 88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
000809
000810             20  CF-CO-BEN-I-G-CD           PIC X.
000811                 88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
000812                 88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
000813                 88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
000814
000815         16  FILLER                         PIC X(304).
000816
000817
000818****************************************************************
000819*             CARRIER MASTER RECORD                            *
000820****************************************************************
000821
000822     12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000823         16  CF-ADDRESS-DATA.
000824             20  CF-MAIL-TO-NAME            PIC X(30).
000825             20  CF-IN-CARE-OF              PIC X(30).
000826             20  CF-ADDRESS-LINE-1          PIC X(30).
000827             20  CF-ADDRESS-LINE-2          PIC X(30).
000828             20  CF-CITY-STATE              PIC X(30).
000829             20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
000830             20  CF-PHONE-NO                PIC 9(11)     COMP-3.
000831
000832         16  CF-CLAIM-NO-CONTROL.
000833             20  CF-CLAIM-NO-METHOD         PIC X.
000834                 88  CLAIM-NO-MANUAL            VALUE '1'.
000835                 88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
000836                 88  CLAIM-NO-SEQ               VALUE '3'.
000837                 88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
000838             20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
000839                 88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
000840                 88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
000841                 88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
000842
000843         16  CF-CHECK-NO-CONTROL.
000844             20  CF-CHECK-NO-METHOD         PIC X.
000845                 88  CHECK-NO-MANUAL            VALUE '1'.
000846                 88  CHECK-NO-AUTO-SEQ          VALUE '2'.
000847                 88  CHECK-NO-CARR-SEQ          VALUE '3'.
000848                 88  CHECK-NO-AT-PRINT          VALUE '4'.
000849             20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
000850                 88  CHECK-CNT-RESET-VALUE      VALUE +999999.
000851
000852         16  CF-DOMICILE-STATE              PIC XX.
000853
000854         16  CF-EXPENSE-CONTROLS.
000855             20  CF-EXPENSE-METHOD          PIC X.
000856                 88  EXPENSE-CALC-MANUAL        VALUE '1'.
000857                 88  DOLLARS-PER-PMT            VALUE '2'.
000858                 88  PERCENT-OF-PAYMENT         VALUE '3'.
000859                 88  DOLLARS-PER-MONTH          VALUE '4'.
000860             20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
000861             20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
000862
000863         16  CF-CORRESPONDENCE-CONTROL.
000864             20  CF-LETTER-RESEND-OPT       PIC X.
000865                 88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
000866                 88  LETTERS-ARE-ARCHIVED       VALUE '1'.
000867             20  FILLER                     PIC X(4).
000868
000869         16  CF-RESERVE-CONTROLS.
000870             20  CF-MANUAL-SW               PIC X.
000871                 88  CF-MANUAL-RESERVES-USED    VALUE '1'.
000872             20  CF-FUTURE-SW               PIC X.
000873                 88  CF-FUTURE-RESERVES-USED    VALUE '1'.
000874             20  CF-PTC-SW                  PIC X.
000875                 88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
000876             20  CF-IBNR-SW                 PIC X.
000877                 88  CF-IBNR-RESERVES-USED      VALUE '1'.
000878             20  CF-PTC-LF-SW               PIC X.
000879                 88  CF-LF-PTC-USED             VALUE '1'.
000880             20  CF-CDT-ACCESS-METHOD       PIC X.
000881                 88  CF-CDT-ROUND-NEAR          VALUE '1'.
000882                 88  CF-CDT-ROUND-HIGH          VALUE '2'.
000883                 88  CF-CDT-INTERPOLATED        VALUE '3'.
000884             20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
000885
000886         16  CF-CLAIM-CALC-METHOD           PIC X.
000887             88  360-PLUS-MONTHS                VALUE '1'.
000888             88  365-PLUS-MONTHS                VALUE '2'.
000889             88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
000890             88  360-DAILY                      VALUE '4'.
000891             88  365-DAILY                      VALUE '5'.
000892
000893         16  CF-LAST-ALPHA-CHARACTER        PIC X.
000894         16  FILLER                         PIC X(11).
000895
000896         16  CF-LIMIT-AMOUNTS.
000897             20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
000898             20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
000899             20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
000900             20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
000901             20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
000902             20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
000903             20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
000904             20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
000905             20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
000906             20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
000907
000908         16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
000909         16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
000910         16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
000911
000912         16  CF-ZIP-CODE.
000913             20  CF-ZIP-PRIME.
000914                 24  CF-ZIP-1ST             PIC X.
000915                     88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000916                 24  FILLER                 PIC X(4).
000917             20  CF-ZIP-PLUS4               PIC X(4).
000918         16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
000919             20  CF-CAN-POSTAL-1            PIC XXX.
000920             20  CF-CAN-POSTAL-2            PIC XXX.
000921             20  FILLER                     PIC XXX.
000922
000923         16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
000924         16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
000925         16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
000926
000927         16  CF-RATING-SWITCH               PIC X.
000928             88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
000929             88  CF-NO-RATING                   VALUE 'N'.
000930
000931         16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
000932
000933         16  CF-CARRIER-OVER-SHORT.
000934             20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
000935             20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
000936
000937         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
000938         16  CF-SECPAY-SWITCH               PIC X.
000939             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
000940             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
000941         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
000942         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
000943         16  FILLER                         PIC X(444).
000944*        16  FILLER                         PIC X(452).
000945
000946
000947****************************************************************
000948*             MORTALITY MASTER RECORD                          *
000949****************************************************************
000950
000951     12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
000952         16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
000953                                INDEXED BY CF-MORT-NDX.
000954             20  CF-MORT-TABLE              PIC X(5).
000955             20  CF-MORT-TABLE-TYPE         PIC X.
000956                 88  CF-MORT-JOINT              VALUE 'J'.
000957                 88  CF-MORT-SINGLE             VALUE 'S'.
000958                 88  CF-MORT-COMBINED           VALUE 'C'.
000959                 88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
000960                 88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
000961             20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
000962             20  CF-MORT-AGE-METHOD         PIC XX.
000963                 88  CF-AGE-LAST                VALUE 'AL'.
000964                 88  CF-AGE-NEAR                VALUE 'AN'.
000965             20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
000966             20  CF-MORT-ADJUSTMENT-DIRECTION
000967                                            PIC X.
000968                 88  CF-MINUS                   VALUE '-'.
000969                 88  CF-PLUS                    VALUE '+'.
000970             20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
000971             20  CF-MORT-JOINT-CODE         PIC X.
000972                 88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
000973             20  CF-MORT-PC-Q               PIC X.
000974                 88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
000975             20  CF-MORT-TABLE-CODE         PIC X(4).
000976             20  CF-MORT-COMMENTS           PIC X(15).
000977             20  FILLER                     PIC X(14).
000978
000979         16  FILLER                         PIC X(251).
000980
000981
000982****************************************************************
000983*             BUSSINESS TYPE MASTER RECORD                     *
000984****************************************************************
000985
000986     12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
000987* FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
000988* RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
000989* AND RECORD 05 IS TYPES 81-99
000990         16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
000991             20  CF-BUSINESS-TITLE          PIC  X(19).
000992             20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
000993                                            PIC S9V9(4) COMP-3.
000994             20  CF-BUS-EXCL-ST-CALL        PIC  X.
000995             20  FILLER                     PIC  X.
000996         16  FILLER                         PIC  X(248).
000997
000998
000999****************************************************************
001000*             TERMINAL MASTER RECORD                           *
001001****************************************************************
001002
001003     12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
001004
001005         16  CF-COMPANY-TERMINALS.
001006             20  CF-TERMINAL-ID  OCCURS 120 TIMES
001007                                  PIC X(4).
001008         16  FILLER               PIC X(248).
001009
001010
001011****************************************************************
001012*             LIFE EDIT MASTER RECORD                          *
001013****************************************************************
001014
001015     12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
001016         16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
001017             20  CF-LIFE-CODE-IN            PIC XX.
001018             20  CF-LIFE-CODE-OUT           PIC XX.
001019         16  FILLER                         PIC X(248).
001020
001021
001022****************************************************************
001023*             AH EDIT MASTER RECORD                            *
001024****************************************************************
001025
001026     12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
001027         16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
001028             20  CF-AH-CODE-IN              PIC XXX.
001029             20  CF-AH-CODE-OUT             PIC XX.
001030         16  FILLER                         PIC X(248).
001031
001032
001033****************************************************************
001034*             CREDIBILITY TABLES                               *
001035****************************************************************
001036
001037     12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
001038         16  CF-CRDB-ENTRY   OCCURS 36 TIMES
001039                             INDEXED BY CF-CRDB-NDX.
001040             20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
001041             20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
001042             20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
001043         16  FILLER                         PIC  X(332).
001044
001045
001046****************************************************************
001047*             REPORT CUSTOMIZATION RECORD                      *
001048****************************************************************
001049
001050     12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
001051         16  CF-ACCOUNT-MASTER-STATUS       PIC X.
001052             88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
001053             88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
001054             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
001055**** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
001056****       A T-TRANSFER.                                   ****
001057             88  CF-ALL-ACCOUNTS                VALUE 'B'.
001058
001059         16  FILLER                         PIC XX.
001060
001061         16  CF-CARRIER-CNTL-OPT.
001062             20  CF-CARRIER-OPT-SEQ         PIC 9.
001063                 88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
001064                 88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
001065             20  CF-CARRIER-SELECT OCCURS 3 TIMES
001066                                            PIC X.
001067         16  CF-GROUP-CNTL-OPT.
001068             20  CF-GROUP-OPT-SEQ           PIC 9.
001069                 88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
001070                 88  CF-GROUP-OPT-NOT-USED      VALUE 0.
001071             20  CF-GROUP-SELECT OCCURS 3 TIMES
001072                                            PIC X(6).
001073         16  CF-STATE-CNTL-OPT.
001074             20  CF-STATE-OPT-SEQ           PIC 9.
001075                 88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
001076                 88  CF-STATE-OPT-NOT-USED      VALUE 0.
001077             20  CF-STATE-SELECT OCCURS 3 TIMES
001078                                            PIC XX.
001079         16  CF-ACCOUNT-CNTL-OPT.
001080             20  CF-ACCOUNT-OPT-SEQ         PIC 9.
001081                 88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
001082                 88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
001083             20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
001084                                            PIC X(10).
001085         16  CF-BUS-TYP-CNTL-OPT.
001086             20  CF-BUS-TYP-OPT-SEQ         PIC 9.
001087                 88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
001088                 88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
001089             20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
001090                                            PIC XX.
001091         16  CF-LF-TYP-CNTL-OPT.
001092             20  CF-LF-TYP-OPT-SEQ          PIC 9.
001093                 88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
001094                 88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
001095             20  CF-BUS-LF-SELECT OCCURS 3 TIMES
001096                                            PIC XX.
001097         16  CF-AH-TYP-CNTL-OPT.
001098             20  CF-AH-TYP-OPT-SEQ          PIC 9.
001099                 88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
001100                 88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
001101             20  CF-BUS-AH-SELECT OCCURS 3 TIMES
001102                                            PIC XX.
001103         16  CF-REPTCD1-CNTL-OPT.
001104             20  CF-REPTCD1-OPT-SEQ         PIC 9.
001105                 88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
001106                 88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
001107             20  CF-REPTCD1-SELECT OCCURS 3 TIMES
001108                                            PIC X(10).
001109         16  CF-REPTCD2-CNTL-OPT.
001110             20  CF-REPTCD2-OPT-SEQ         PIC 9.
001111                 88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
001112                 88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
001113             20  CF-REPTCD2-SELECT OCCURS 3 TIMES
001114                                            PIC X(10).
001115         16  CF-USER1-CNTL-OPT.
001116             20  CF-USER1-OPT-SEQ           PIC 9.
001117                 88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
001118                 88  CF-USER1-OPT-NOT-USED      VALUE 0.
001119             20  CF-USER1-SELECT OCCURS 3 TIMES
001120                                            PIC X(10).
001121         16  CF-USER2-CNTL-OPT.
001122             20  CF-USER2-OPT-SEQ           PIC 9.
001123                 88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
001124                 88  CF-USER2-OPT-NOT-USED      VALUE 0.
001125             20  CF-USER2-SELECT OCCURS 3 TIMES
001126                                            PIC X(10).
001127         16  CF-USER3-CNTL-OPT.
001128             20  CF-USER3-OPT-SEQ           PIC 9.
001129                 88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
001130                 88  CF-USER3-OPT-NOT-USED      VALUE 0.
001131             20  CF-USER3-SELECT OCCURS 3 TIMES
001132                                            PIC X(10).
001133         16  CF-USER4-CNTL-OPT.
001134             20  CF-USER4-OPT-SEQ           PIC 9.
001135                 88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
001136                 88  CF-USER4-OPT-NOT-USED      VALUE 0.
001137             20  CF-USER4-SELECT OCCURS 3 TIMES
001138                                            PIC X(10).
001139         16  CF-USER5-CNTL-OPT.
001140             20  CF-USER5-OPT-SEQ           PIC 9.
001141                 88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
001142                 88  CF-USER5-OPT-NOT-USED      VALUE 0.
001143             20  CF-USER5-SELECT OCCURS 3 TIMES
001144                                            PIC X(10).
001145         16  CF-REINS-CNTL-OPT.
001146             20  CF-REINS-OPT-SEQ           PIC 9.
001147                 88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
001148                 88  CF-REINS-OPT-NOT-USED      VALUE 0.
001149             20  CF-REINS-SELECT OCCURS 3 TIMES.
001150                 24  CF-REINS-PRIME         PIC XXX.
001151                 24  CF-REINS-SUB           PIC XXX.
001152
001153         16  CF-AGENT-CNTL-OPT.
001154             20  CF-AGENT-OPT-SEQ           PIC 9.
001155                 88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
001156                 88  CF-AGENT-OPT-NOT-USED      VALUE 0.
001157             20  CF-AGENT-SELECT OCCURS 3 TIMES
001158                                            PIC X(10).
001159
001160         16  FILLER                         PIC X(43).
001161
001162         16  CF-LOSS-RATIO-SELECT.
001163             20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
001164             20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
001165         16  CF-ENTRY-DATE-SELECT.
001166             20  CF-SEL-LO-ENTRY-DATE       PIC XX.
001167             20  CF-SEL-HI-ENTRY-DATE       PIC XX.
001168         16  CF-EFFECTIVE-DATE-SELECT.
001169             20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
001170             20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
001171
001172         16  CF-EXCEPTION-LIST-IND          PIC X.
001173             88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
001174
001175         16  FILLER                         PIC X(318).
001176
001177****************************************************************
001178*                  EXCEPTION REPORTING RECORD                  *
001179****************************************************************
001180
001181     12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
001182         16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
001183             88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
001184
001185         16  CF-COMBINED-LIFE-AH-OPT.
001186             20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
001187             20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
001188             20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
001189             20  CF-CANCELLATION-RATIO      PIC S9(02).
001190
001191         16  CF-LIFE-OPT.
001192             20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
001193             20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
001194             20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
001195             20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
001196             20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
001197             20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
001198             20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
001199             20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
001200             20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
001201             20  CF-LF-AVG-AGE-MAX          PIC S9(02).
001202
001203         16  CF-AH-OPT.
001204             20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
001205             20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
001206             20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
001207             20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
001208             20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
001209             20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
001210             20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
001211             20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
001212             20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
001213             20  CF-AH-AVG-AGE-MAX          PIC S9(02).
001214
001215         16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
001216             88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
001217             88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
001218             88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
001219
001220         16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
001221
001222         16  FILLER                         PIC X(673).
001223
001224
001225****************************************************************
001226*             MORTGAGE SYSTEM PLAN RECORD                      *
001227****************************************************************
001228
001229     12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
001230         16  CF-PLAN-TYPE                   PIC X.
001231             88  CF-LIFE-MORT-PLAN             VALUE 'L'.
001232             88  CF-DISAB-MORT-PLAN            VALUE 'D'.
001233             88  CF-AD-D-MORT-PLAN             VALUE 'A'.
001234         16  CF-PLAN-ABBREV                 PIC XXX.
001235         16  CF-PLAN-DESCRIPT               PIC X(10).
001236         16  CF-PLAN-NOTES                  PIC X(20).
001237         16  CF-PLAN-ESTABLISH-DATE         PIC XX.
001238         16  CF-PLAN-UNDERWRITING.
001239             20  CF-PLAN-TERM-DATA.
001240                 24  CF-MINIMUM-TERM        PIC S999      COMP-3.
001241                 24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
001242             20  CF-PLAN-AGE-DATA.
001243                 24  CF-MINIMUM-AGE         PIC S999      COMP-3.
001244                 24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
001245                 24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
001246             20  CF-PLAN-BENEFIT-DATA.
001247                 24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
001248                 24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
001249                 24  CF-MAXIMUM-MONTHLY-BENEFIT
001250                                            PIC S9(7)V99  COMP-3.
001251         16  CF-PLAN-POLICY-FORMS.
001252             20  CF-POLICY-FORM             PIC X(12).
001253             20  CF-MASTER-APPLICATION      PIC X(12).
001254             20  CF-MASTER-POLICY           PIC X(12).
001255         16  CF-PLAN-RATING.
001256             20  CF-RATE-CODE               PIC X(5).
001257             20  CF-SEX-RATING              PIC X.
001258                 88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
001259                 88  CF-PLAN-SEX-RATED         VALUE '2'.
001260             20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
001261             20  CF-SUB-STD-TYPE            PIC X.
001262                 88  CF-PCT-OF-PREM            VALUE '1'.
001263                 88  CF-PCT-OF-BENE            VALUE '2'.
001264         16  CF-PLAN-PREM-TOLERANCES.
001265             20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
001266             20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
001267         16  CF-PLAN-PYMT-TOLERANCES.
001268             20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
001269             20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
001270         16  CF-PLAN-MISC-DATA.
001271             20  FILLER                     PIC X.
001272             20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
001273             20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
001274         16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
001275         16  CF-PLAN-IND-GRP                PIC X.
001276             88  CF-MORT-INDIV-PLAN            VALUE 'I'
001277                                                     '1'.
001278             88  CF-MORT-GROUP-PLAN            VALUE 'G'
001279                                                     '2'.
001280         16  CF-MIB-SEARCH-SW               PIC X.
001281             88  CF-MIB-SEARCH-ALL             VALUE '1'.
001282             88  CF-MIB-SEARCH-NONE            VALUE '2'.
001283             88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
001284             88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
001285         16  CF-ALPHA-SEARCH-SW             PIC X.
001286             88  CF-MIB-ALPHA-ALL              VALUE '1'.
001287             88  CF-MIB-ALPHA-NONE             VALUE '2'.
001288             88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
001289             88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
001290             88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
001291             88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
001292             88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
001293             88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
001294             88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
001295             88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
001296                                                     'A' 'B' 'C'
001297                                                     'X' 'Y' 'Z'.
001298         16  CF-EFF-DT-RULE-SW              PIC X.
001299             88  CF-EFF-DT-ENTER               VALUE 'E'.
001300             88  CF-EFF-DT-MONTH               VALUE 'M'.
001301             88  CF-EFF-DT-QTR                 VALUE 'Q'.
001302             88  CF-EFF-DT-SEMI                VALUE 'S'.
001303             88  CF-EFF-DT-ANN                 VALUE 'A'.
001304         16  FILLER                         PIC X(4).
001305         16  CF-HEALTH-QUESTIONS            PIC X.
001306             88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
001307         16  CF-GRACE-PERIOD                PIC S999      COMP-3.
001308         16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
001309         16  CF-PLAN-SNGL-JNT               PIC X.
001310             88  CF-COMBINED-PLAN              VALUE 'C'.
001311             88  CF-JNT-PLAN                   VALUE 'J'.
001312             88  CF-SNGL-PLAN                  VALUE 'S'.
001313         16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
001314         16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
001315         16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
001316         16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
001317         16  CF-RERATE-CNTL                 PIC  X.
001318             88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
001319             88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
001320             88  CF-DO-NOT-RERATE               VALUE '3' ' '.
001321             88  CF-AUTO-RECALC                 VALUE '4'.
001322         16  CF-BENEFIT-TYPE                PIC  X.
001323             88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
001324             88  CF-BENEFIT-REDUCES             VALUE '2'.
001325         16  CF-POLICY-FEE                  PIC S999V99
001326                                                    COMP-3.
001327         16  CF-1ST-NOTICE-FORM             PIC  X(04).
001328         16  CF-2ND-NOTICE-FORM             PIC  X(04).
001329         16  CF-3RD-NOTICE-FORM             PIC  X(04).
001330         16  CF-4TH-NOTICE-FORM             PIC  X(04).
001331         16  FILLER                         PIC  X(32).
001332         16  CF-TERMINATION-FORM            PIC  X(04).
001333         16  FILLER                         PIC  X(08).
001334         16  CF-CLAIM-CAP                   PIC S9(7)V99
001335                                                       COMP-3.
001336         16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
001337         16  CF-ISSUE-LETTER                PIC  X(4).
001338         16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
001339         16  CF-DEPENDENT-COVERAGE          PIC  X.
001340             88  CF-YES-DEP-COV                 VALUE 'Y'.
001341             88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
001342         16  CF-MP-REFUND-CALC              PIC X.
001343             88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
001344             88  CF-MP-REFD-BY-R78              VALUE '1'.
001345             88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
001346             88  CF-MP-REFD-AS-CALIF            VALUE '3'.
001347             88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
001348             88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
001349             88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
001350             88  CF-MP-REFD-MEAN                VALUE '8'.
001351         16  CF-ALT-RATE-CODE               PIC  X(5).
001352
001353
001354         16  FILLER                         PIC X(498).
001355****************************************************************
001356*             MORTGAGE COMPANY MASTER RECORD                   *
001357****************************************************************
001358
001359     12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
001360         16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
001361         16  CF-MORTG-ACCESS-CONTROL        PIC X.
001362             88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
001363             88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
001364             88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
001365             88  CF-MORT-PROD-CNTL                   VALUE '3'.
001366             88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
001367
001368         16  CF-MORTG-CONVERSION-DATE       PIC XX.
001369         16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
001370         16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
001371         16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
001372         16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
001373
001374         16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
001375             88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
001376         16  CF-MP-RECON-USE-IND            PIC X(1).
001377             88  CF-MP-USE-RECON             VALUE 'Y'.
001378         16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
001379             88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
001380         16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
001381             88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
001382             88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
001383         16  FILLER                         PIC X(1).
001384         16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
001385             88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
001386         16  CF-MORTG-MIB-VERSION           PIC X.
001387             88  CF-MORTG-MIB-BATCH         VALUE '1'.
001388             88  CF-MORTG-MIB-ONLINE        VALUE '2'.
001389             88  CF-MORTG-MIB-BOTH          VALUE '3'.
001390         16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
001391             20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
001392                 88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
001393             20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
001394                 88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
001395             20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
001396                 88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
001397             20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
001398                 88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
001399             20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
001400                 88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
001401             20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
001402                 88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
001403         16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
001404         16  FILLER                         PIC X(7).
001405         16  CF-MORTG-DESTINATION-SYMBOL.
001406             20  CF-MORTG-MIB-COMM          PIC X(5).
001407             20  CF-MORTG-MIB-TERM          PIC X(5).
001408         16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
001409             88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
001410         16  FILLER                         PIC X(03).
001411         16  CF-MP-CHECK-NO-CONTROL.
001412             20  CF-MP-CHECK-NO-METHOD      PIC X(01).
001413                 88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
001414                 88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
001415                                                ' ' LOW-VALUES.
001416                 88  CF-MP-CHECK-NO-PRE-PRINTED
001417                                               VALUE '3'.
001418         16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
001419         16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
001420         16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
001421             20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
001422                 88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
001423             20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
001424                 88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
001425             20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
001426                 88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
001427             20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
001428                 88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
001429             20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
001430                 88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
001431             20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
001432                 88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
001433         16  CF-MORTG-BILLING-AREA.
001434             20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
001435                                            PIC X.
001436         16  CF-MORTG-MONTH-END-DT          PIC XX.
001437         16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
001438         16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
001439         16  CF-MORTG-MIB-DEST-SW           PIC X.
001440             88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
001441             88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
001442         16  FILLER                         PIC X.
001443         16  CF-MORTG-LABEL-CONTROL         PIC X.
001444             88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
001445             88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
001446         16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
001447         16  FILLER                         PIC X(8).
001448         16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
001449         16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
001450         16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
001451         16  CF-ACH-COMPANY-ID.
001452             20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
001453                 88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
001454                 88  CF-ACH-ICD-DUNS                VALUE '3'.
001455                 88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
001456             20  CF-ACH-COMPANY-ID-NO       PIC X(9).
001457         16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
001458             88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
001459         16  CF-RATE-DEV-AUTHORIZATION      PIC X.
001460             88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
001461             88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
001462         16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
001463         16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
001464         16  FILLER                         PIC X(536).
001465
001466****************************************************************
001467*             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
001468****************************************************************
001469
001470     12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
001471         16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
001472             20  CF-FEMALE-HEIGHT.
001473                 24  CF-FEMALE-FT           PIC 99.
001474                 24  CF-FEMALE-IN           PIC 99.
001475             20  CF-FEMALE-MIN-WT           PIC 999.
001476             20  CF-FEMALE-MAX-WT           PIC 999.
001477         16  FILLER                         PIC X(428).
001478
001479     12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
001480         16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
001481             20  CF-MALE-HEIGHT.
001482                 24  CF-MALE-FT             PIC 99.
001483                 24  CF-MALE-IN             PIC 99.
001484             20  CF-MALE-MIN-WT             PIC 999.
001485             20  CF-MALE-MAX-WT             PIC 999.
001486         16  FILLER                         PIC X(428).
001487******************************************************************
001488*             AUTOMATIC ACTIVITY RECORD                          *
001489******************************************************************
001490     12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
001491         16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
001492             20  CF-SYS-ACTIVE-SW           PIC X(01).
001493             20  CF-SYS-LETTER-ID           PIC X(04).
001494             20  CF-SYS-RESEND-DAYS         PIC 9(03).
001495             20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
001496             20  CF-SYS-RESET-SW            PIC X(01).
001497             20  CF-SYS-REPORT-DAYS         PIC 9(03).
001498             20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
001499
001500         16  FILLER                         PIC X(50).
001501
001502         16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
001503             20  CF-USER-ACTIVE-SW          PIC X(01).
001504             20  CF-USER-LETTER-ID          PIC X(04).
001505             20  CF-USER-RESEND-DAYS        PIC 9(03).
001506             20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
001507             20  CF-USER-RESET-SW           PIC X(01).
001508             20  CF-USER-REPORT-DAYS        PIC 9(03).
001509             20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
001510             20  CF-USER-ACTIVITY-DESC      PIC X(20).
001511
001512         16  FILLER                         PIC X(246).
      *<<((file: ELCCNTL))
000283*                                copy ERCPYAJ.
      *>>((file: ERCPYAJ))
000001******************************************************************
000002*                                                                *
000003*                            ERCPYAJ                             *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.015                          *
000006*                                                                *
000007*   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
000008*                                                                *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 200  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
000014*       ALTERNATE PATHS = NONE                                   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019******************************************************************
000020*                   C H A N G E   L O G
000021*
000022* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000023*-----------------------------------------------------------------
000024*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000025* EFFECTIVE    NUMBER
000026*-----------------------------------------------------------------
000027* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
000028* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
000029******************************************************************
000030
000031 01  PENDING-PAY-ADJ.
000032     12  PY-RECORD-ID                     PIC XX.
000033         88  VALID-PY-ID                        VALUE 'PY'.
000034
000035     12  PY-CONTROL-PRIMARY.
000036         16  PY-COMPANY-CD                PIC X.
000037         16  PY-CARRIER                   PIC X.
000038         16  PY-GROUPING                  PIC X(6).
000039         16  PY-FIN-RESP                  PIC X(10).
000040         16  PY-ACCOUNT                   PIC X(10).
000041         16  PY-PRODUCER REDEFINES PY-ACCOUNT
000042                                          PIC X(10).
000043         16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
000044         16  PY-RECORD-TYPE               PIC X.
000045             88  PY-REMIT-RECEIVED            VALUE 'R'.
000046             88  PY-DEPOSIT                   VALUE 'D'.
000047             88  PY-CHARGE-TO-AGENT           VALUE 'C'.
000048             88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
000049             88  PY-ADJ-DEPOSIT               VALUE 'T'.
000050             88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
000051             88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
000052             88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
000053             88  PY-ADD-TO-BALANCE            VALUE 'Z'.
000054             88  PY-FICA-ENTRY                VALUE 'F'.
000055             88  PY-REMIT-IND-GROUPING        VALUE 'G'.
000056             88  PY-POLICY-FEE                VALUE 'W'.
000057             88  PY-DUE-PREM-ADJ              VALUE 'P'.
000058
000059     12  PY-PYMT-TYPE                     PIC X.
000060             88  PY-NEW-BUS-PYMT              VALUE 'B'.
000061             88  PY-REINS-PYMT                VALUE 'R'.
000062             88  PY-EXP-PYMT                  VALUE 'E'.
000063
000064     12  PY-BIL-INV                       PIC X(6).
000065     12  PY-REF-NO                        PIC X(12).
000066
000067     12  PY-LAST-MAINT-DT                 PIC XX.
000068     12  PY-LAST-MAINT-BY                 PIC X(4).
000069     12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
000070
000071     12  PY-PYADJ-RECORD.
000072         16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
000073         16  PY-ENTRY-COMMENT             PIC X(30).
000074         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
000075             20  PY-GL-ACCOUNT            PIC X(10).
000076             20  PY-GL-STATE              PIC X(02).
000077             20  PY-GL-CANC-SW            PIC X(01).
000078                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
000079                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
000080             20  PY-GL-COMMENT            PIC X(10).
000081             20  FILLER      REDEFINES PY-GL-COMMENT.
000082                 24  PY-GL-CHECK-NO       PIC 9(06).
000083                 24  FILLER               PIC X(04).
000084             20  FILLER                   PIC X(07).
000085         16  PY-SAVE-ACCOUNT              PIC X(10).
000086         16  PY-SAVE-TYPE                 PIC X(01).
000087
000088         16  PY-LETTERS.
000089             20  PY-LETTER OCCURS 3 TIMES
000090                           INDEXED BY PY-LET-NDX
000091                                          PIC X(04).
000092
000093         16  PY-ERCOMP-TYPE               PIC X.
000094             88  PY-ACCOUNT-TYPE              VALUE 'A'.
000095             88  PY-GA-TYPE                   VALUE 'G'.
000096             88  PY-BANK-TYPE                 VALUE 'B'.
000097         16  FILLER                       PIC X(05).
000098
000099     12  PY-RECORD-STATUS.
000100         16  PY-CREDIT-SELECT-DT          PIC XX.
000101         16  PY-CREDIT-ACCEPT-DT          PIC XX.
000102         16  PY-BILLED-DATE               PIC XX.
000103         16  PY-REPORTED-DT               PIC XX.
000104         16  PY-PMT-APPLIED               PIC X.
000105             88  PY-ACCOUNT-PMT               VALUE 'A'.
000106             88  PY-GA-PMT                    VALUE 'G'.
000107             88  PY-OVWRITE-PMT               VALUE 'O'.
000108             88  PY-NON-AR-PMT                VALUE 'N'.
000109         16  FILLER                       PIC X(5).
000110         16  PY-INPUT-DT                  PIC XX.
000111         16  PY-CHECK-NUMBER              PIC X(6).
000112         16  PY-VOID-SW                   PIC X.
000113             88  PY-CHECK-VOIDED              VALUE 'V'.
000114         16  PY-CHECK-ORIGIN-SW           PIC X.
000115             88  PY-BILLING-CHECK             VALUE 'B'.
000116             88  PY-REFUND-CHECK              VALUE 'R'.
000117             88  PY-GA-CHECK                  VALUE 'G'.
000118             88  PY-CHECK-WRITTEN             VALUE 'W'.
000119             88  PY-CHECK-REVERSAL            VALUE 'V'.
000120         16  PY-CHECK-WRITTEN-DT          PIC XX.
000121         16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
000122         16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
000123         16  PY-BILL-FLAG                 PIC X.
000124             88  PY-BILLED                    VALUE 'B'.
000125         16  PY-AR-FLAG                   PIC X.
000126             88  PY-AR-CYCLE                  VALUE 'C'.
000127             88  PY-AR-MONTH-END              VALUE 'M'.
000128         16  PY-AR-DATE                   PIC XX.
000129
000130     12  PY-GL-CODES.
000131         16  PY-GL-DB                     PIC X(14).
000132         16  PY-GL-CR                     PIC X(14).
000133         16  PY-GL-FLAG                   PIC X.
000134         16  PY-GL-DATE                   PIC XX.
000135
000136     12  PY-CANCEL-FEE-FLAG               PIC X(2).
000137     12  FILLER                           PIC X(3).
000138******************************************************************
000139
      *<<((file: ERCPYAJ))
000284*                                copy ERCPNDB.
      *>>((file: ERCPNDB))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPNDB.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.025                          *
000007*                                                                *
000008*   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
000009*                                                                *
000010******************************************************************
000011*   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
000012*         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
000013******************************************************************
000014*                                                                *
000015*                                                                *
000016*   FILE TYPE = VSAM,KSDS                                        *
000017*   RECORD SIZE = 585  RECFORM = FIXED                           *
000018*                                                                *
000019*   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
000020*       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
000021*                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
000022*                                                 RKP=13,LEN=36  *
000023*       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
000024*                                      AND CHG-SEQ.)             *
000025*                                                RKP=49,LEN=11   *
000026*       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
000027*                                      AND CHG-SEQ.)             *
000028*                                                RKP=60,LEN=15   *
000029*                                                                *
000030*   LOG = NO                                                     *
000031*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000032******************************************************************
000033******************************************************************
000034*                   C H A N G E   L O G
000035*
000036* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000037*-----------------------------------------------------------------
000038*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000039* EFFECTIVE    NUMBER
000040*-----------------------------------------------------------------
000041* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
000042* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000043* 011904                   PEMA  ADD TOTAL FEE PROCESSING
000044* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
000045* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
000046* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
000047* 032306                   PEMA  ADD BOW LOAN NUMBER
000048* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
000049* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
000050* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
000051* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000052* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
000053* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
000054* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000055* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
000056* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
000057* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000058* 010517  CR2016021600005  PEMA ADD NEW FORCE CODE FOR AGG
000059* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
000060* 012220  CR2018092700002  TANA ADD LETTER REQUIRED FIELD
000061******************************************************************
000062
000063 01  PENDING-BUSINESS.
000064     12  PB-RECORD-ID                     PIC XX.
000065         88  VALID-PB-ID                        VALUE 'PB'.
000066
000067     12  PB-CONTROL-PRIMARY.
000068         16  PB-COMPANY-CD                PIC X.
000069         16  PB-ENTRY-BATCH               PIC X(6).
000070         16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
000071         16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
000072
000073     12  PB-CONTROL-BY-ACCOUNT.
000074         16  PB-COMPANY-CD-A1             PIC X.
000075         16  PB-CARRIER                   PIC X.
000076         16  PB-GROUPING.
000077             20  PB-GROUPING-PREFIX       PIC XXX.
000078             20  PB-GROUPING-PRIME        PIC XXX.
000079         16  PB-STATE                     PIC XX.
000080         16  PB-ACCOUNT.
000081             20  PB-ACCOUNT-PREFIX        PIC X(4).
000082             20  PB-ACCOUNT-PRIME         PIC X(6).
000083         16  PB-CERT-EFF-DT               PIC XX.
000084         16  PB-CERT-NO.
000085             20  PB-CERT-PRIME            PIC X(10).
000086             20  PB-CERT-SFX              PIC X.
000087         16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
000088
000089         16  PB-RECORD-TYPE               PIC X.
000090             88  PB-MAILING-DATA                VALUE '0'.
000091             88  PB-ISSUE                       VALUE '1'.
000092             88  PB-CANCELLATION                VALUE '2'.
000093             88  PB-BATCH-TRAILER               VALUE '9'.
000094
000095     12  PB-CONTROL-BY-ORIG-BATCH.
000096         16  PB-ORIGINAL-COMPANY-CD       PIC X.
000097         16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
000098         16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
000099         16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
000100
000101     12  PB-CONTROL-BY-CSR.
000102         16  PB-CSR-COMPANY-CD            PIC X.
000103         16  PB-CSR-ID                    PIC X(4).
000104         16  PB-CSR-ENTRY-BATCH           PIC X(6).
000105         16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
000106         16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
000107******************************************************************
000108*    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
000109******************************************************************
000110
000111     12  PB-LAST-MAINT-DT                 PIC XX.
000112     12  PB-LAST-MAINT-BY                 PIC X(4).
000113     12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
000114
000115     12  PB-RECORD-BODY                   PIC X(375).
000116
000117     12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
000118         16  PB-CERT-ORIGIN               PIC X.
000119             88  CLASIC-CREATED-CERT         VALUE '1'.
000120         16  PB-I-NAME.
000121             20  PB-I-INSURED-LAST-NAME   PIC X(15).
000122             20  PB-I-INSURED-FIRST-NAME.
000123                 24  PB-I-INSURED-1ST-INIT PIC X.
000124                 24  FILLER                PIC X(9).
000125             20  PB-I-INSURED-MIDDLE-INIT PIC X.
000126         16  PB-I-AGE                     PIC S99   COMP-3.
000127         16  PB-I-JOINT-AGE               PIC S99   COMP-3.
000128         16  PB-I-BIRTHDAY                PIC XX.
000129         16  PB-I-INSURED-SEX             PIC X.
000130             88  PB-SEX-MALE     VALUE 'M'.
000131             88  PB-SEX-FEMALE   VALUE 'F'.
000132
000133         16  PB-I-LF-TERM                 PIC S999   COMP-3.
000134         16  PB-I-AH-TERM                 PIC S999   COMP-3.
000135         16  PB-I-LOAN-TERM               PIC S999   COMP-3.
000136         16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
000137         16  PB-I-SKIP-CODE               PIC X.
000138             88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
000139             88  PB-SKIP-JULY              VALUE '1'.
000140             88  PB-SKIP-AUGUST            VALUE '2'.
000141             88  PB-SKIP-SEPTEMBER         VALUE '3'.
000142             88  PB-SKIP-JULY-AUG          VALUE '4'.
000143             88  PB-SKIP-AUG-SEPT          VALUE '5'.
000144             88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
000145             88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
000146             88  PB-SKIP-JUNE              VALUE '8'.
000147             88  PB-SKIP-JUNE-JULY         VALUE '9'.
000148             88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
000149             88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
000150         16  PB-I-TERM-TYPE               PIC X.
000151             88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
000152             88  PB-PAID-WEEKLY            VALUE 'W'.
000153             88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
000154             88  PB-PAID-BI-WEEKLY         VALUE 'B'.
000155             88  PB-PAID-13-YEARLY         VALUE 'T'.
000156         16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
000157         16  PB-I-POLICY-FORM-NO          PIC X(12).
000158         16  PB-I-DATA-ENTRY-SW           PIC X.
000159             88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
000160             88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
000161             88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
000162             88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
000163         16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
000164         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
000165*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
000166         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
000167         16  PB-I-LETTER-REQD             PIC X.
000168
000169         16  PB-I-LIFE-BENEFIT-CD         PIC XX.
000170             88  PB-VALID-LIFE               VALUE '01' THRU '89'.
000171             88  PB-INVALID-LIFE             VALUE '  ' '00'
000172                                                   '90' THRU '99'.
000173         16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
000174                                          PIC XX.
000175         16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
000176         16  PB-I-AMOUNT-FINANCED REDEFINES
000177                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
000178         16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
000179         16  PB-I-UNPAID-CASH-PRICE REDEFINES
000180                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
000181         16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000182         16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
000183         16  PB-I-CLP-AMOUNT REDEFINES
000184                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
000185         16  PB-I-LF-CALC-FLAG            PIC X.
000186             88 PB-COMP-LF-PREM               VALUE '?'.
000187         16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
000188         16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
000189         16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
000190         16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
000191         16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
000192         16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
000193         16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
000194         16  PB-I-LF-ABBR                 PIC XXX.
000195         16  PB-I-LF-INPUT-CD             PIC XX.
000196
000197         16  PB-I-AH-BENEFIT-CD           PIC XX.
000198             88  PB-VALID-AH                 VALUE '01' THRU '89'.
000199             88  PB-INVALID-AH               VALUE '  ' '00'
000200                                                   '90' THRU '99'.
000201         16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
000202         16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000203         16  PB-I-AH-CALC-FLAG            PIC X.
000204             88 PB-COMP-AH-PREM                  VALUE '?'.
000205         16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
000206         16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
000207         16  PB-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
000208         16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
000209         16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
000210         16  PB-I-AH-ABBR                 PIC XXX.
000211         16  PB-I-AH-INPUT-CD             PIC XXX.
000212
000213         16  PB-I-SPECIAL-REIN-CODE       PIC X.
000214         16  PB-I-REIN-TABLE              PIC XXX.
000215         16  PB-I-BUSINESS-TYPE           PIC 99.
000216         16  PB-I-INDV-GRP-CD             PIC X.
000217         16  PB-I-MORT-CODE.
000218             20  PB-I-TABLE               PIC X.
000219             20  PB-I-INTEREST            PIC XX.
000220             20  PB-I-MORT-TYP            PIC X.
000221         16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
000222         16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
000223         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
000224         16  PB-I-INDV-GRP-OVRD           PIC X.
000225         16  PB-I-RATE-CLASS-OVRD         PIC XX.
000226         16  PB-I-SIG-SW                  PIC X.
000227             88  PB-POLICY-SIGNED             VALUE 'Y'.
000228         16  PB-I-RATE-CLASS              PIC XX.
000229         16  PB-I-RATE-DEVIATION-LF       PIC XXX.
000230         16  PB-I-RATE-DEVIATION-AH       PIC XXX.
000231         16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
000232         16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
000233         16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
000234         16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
000235         16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
000236         16  PB-I-BENEFIT-TYPE            PIC XXX.
000237         16  PB-I-OB-FLAG                 PIC X.
000238             88  PB-I-OB                      VALUE 'B'.
000239             88  PB-I-SUMMARY                 VALUE 'Z'.
000240         16  PB-I-ENTRY-STATUS            PIC X.
000241             88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
000242                                              'M' '5' '9' '2'.
000243             88  PB-I-NORMAL-ENTRY            VALUE '1'.
000244             88  PB-I-POLICY-PENDING          VALUE '2'.
000245             88  PB-I-CONVERSION-ENTRY        VALUE '4'.
000246             88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
000247             88  PB-I-POLICY-IS-CASH          VALUE 'C'.
000248             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
000249             88  PB-I-REIN-ONLY               VALUE '9'.
000250             88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
000251             88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
000252             88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
000253             88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
000254         16  PB-I-INT-CODE                PIC X.
000255             88  PB-ADD-ON-INTEREST           VALUE 'A'.
000256             88  PB-SIMPLE-INTEREST           VALUE 'S'.
000257         16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
000258         16  PB-I-SOC-SEC-NO              PIC X(11).
000259         16  PB-I-MEMBER-NO               PIC X(12).
000260         16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
000261*        16  PB-I-LOAN-OFFICER            PIC XXX.
000262         16  PB-I-OLD-LOF                 PIC XXX.
000263         16  PB-I-LF-EXPIRE-DT            PIC XX.
000264         16  PB-I-AH-EXPIRE-DT            PIC XX.
000265         16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
000266         16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
000267         16  PB-I-LIFE-INDICATOR          PIC X.
000268             88  PB-I-JOINT-COVERAGE         VALUE 'J'.
000269         16  PB-I-LIVES                   PIC S9(7)       COMP-3.
000270         16  PB-I-DDF-IU-RATE-UP REDEFINES PB-I-LIVES
000271                                          PIC S9(5)V99    COMP-3.
000272         16  PB-I-MAIL-ADDRS-SW           PIC X.
000273             88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
000274             88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
000275         16  PB-I-1ST-PMT-DT              PIC XX.
000276         16  PB-I-JOINT-INSURED.
000277             20 PB-I-JOINT-LAST-NAME      PIC X(15).
000278             20 PB-I-JOINT-FIRST-NAME.
000279                24  PB-I-JOINT-FIRST-INIT PIC X.
000280                24  FILLER                PIC X(9).
000281             20 PB-I-JOINT-MIDDLE-INIT    PIC X.
000282*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
000283         16  PB-I-BENEFICIARY-NAME.
000284             20  PB-I-BANK-NUMBER         PIC X(10).
000285             20  FILLER                   PIC X(15).
000286         16  PB-I-LAST-ADD-ON-DT          PIC XX.
000287         16  PB-I-REFERENCE               PIC X(12).
000288         16  FILLER REDEFINES PB-I-REFERENCE.
000289             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
000290             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
000291             20  PB-I-CLP-STATE           PIC XX.
000292         16  PB-I-UNDERWRITING-STATUS     PIC X.
000293             88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
000294             88  PB-I-POLICY-DECLINED         VALUE 'D'.
000295             88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
000296         16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
000297         16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
000298         16  PB-I-RESIDENT-STATE          PIC XX.
000299         16  PB-I-RATE-CODE               PIC X(4).
000300         16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
000301         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
000302         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
000303         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
000304         16  PB-I-BANK-NOCHRGB            PIC 99.
000305         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
000306         16  PB-I-JOINT-BIRTHDAY          PIC XX.
000307
000308     12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
000309         16  PB-C-LF-CANCEL-VOID-SW       PIC X.
000310             88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
000311         16  PB-C-CANCEL-ORIGIN           PIC X.
000312             88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
000313         16  PB-C-LF-CANCEL-DT            PIC XX.
000314         16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
000315         16  PB-C-LF-CALC-REQ             PIC X.
000316             88 PB-COMP-LF-CANCEL            VALUE '?'.
000317         16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
000318         16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
000319         16  PB-C-AH-CANCEL-VOID-SW       PIC X.
000320             88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
000321         16  PB-C-AH-CANCEL-DT            PIC XX.
000322         16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
000323         16  PB-C-AH-CALC-REQ             PIC X.
000324             88 PB-COMP-AH-CANCEL            VALUE '?'.
000325         16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
000326         16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
000327         16  PB-C-LAST-NAME               PIC X(15).
000328         16  PB-C-REFUND-SW               PIC X.
000329             88  PB-C-REFUND-CREATED          VALUE 'Y'.
000330             88  PB-C-REFUND-REQUESTED        VALUE 'R'.
000331         16  PB-C-LIVES                   PIC S9(3)       COMP-3.
000332         16  PB-C-PAYEE-CODE              PIC X(6).
000333         16  PB-C-LF-REFUND-OVERRIDE      PIC X.
000334         16  PB-C-AH-REFUND-OVERRIDE      PIC X.
000335         16  PB-C-LF-COMM-CHARGEBACK      PIC X.
000336         16  PB-C-AH-COMM-CHARGEBACK      PIC X.
000337         16  PB-C-REFERENCE               PIC X(12).
000338         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
000339         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
000340         16  PB-C-POST-CARD-IND           PIC X.
000341         16  PB-C-CANCEL-REASON           PIC X.
000342         16  PB-C-REF-INTERFACE-SW        PIC X.
000343         16  PB-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
000344         16  PB-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
000345         16  FILLER                       PIC X(01).
000346*        16  FILLER                       PIC X(18).
000347         16  PB-C-POLICY-FORM-NO          PIC X(12).
000348*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
000349         16  PB-C-INT-ON-REFS             PIC S9(7)V99   COMP-3.
000350         16  PB-CANCELED-CERT-DATA.
000351             20  PB-CI-INSURED-NAME.
000352                 24  PB-CI-LAST-NAME      PIC X(15).
000353                 24  PB-CI-INITIALS       PIC XX.
000354             20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
000355             20  PB-CI-INSURED-SEX        PIC X.
000356             20  PB-CI-LF-TERM            PIC S999        COMP-3.
000357             20  PB-CI-LF-BENEFIT-CD      PIC XX.
000358             20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
000359             20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
000360             20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
000361             20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
000362             20  PB-CI-AH-TERM            PIC S999        COMP-3.
000363             20  PB-CI-AH-BENEFIT-CD      PIC XX.
000364             20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
000365             20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
000366             20  PB-CI-RATE-CLASS         PIC XX.
000367             20  PB-CI-RATE-DEV-LF        PIC XXX.
000368             20  PB-CI-RATE-DEV-AH        PIC XXX.
000369             20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
000370             20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
000371             20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
000372             20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
000373             20  PB-CI-LF-ABBR            PIC X(3).
000374             20  PB-CI-AH-ABBR            PIC X(3).
000375             20  PB-CI-OB-FLAG            PIC X.
000376                 88  PB-CI-OB                VALUE 'B'.
000377             20  PB-CI-LF-POLICY-STATUS   PIC X.
000378                 88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
000379                                           'M' '4' '5' '9' '2'.
000380                 88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
000381                 88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
000382                 88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
000383                 88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
000384                 88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
000385                 88  PB-CI-LF-POLICY-IS-CASH         VALUE 'C'.
000386                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
000387                 88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
000388                 88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
000389                 88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
000390                 88  PB-CI-LF-REIN-ONLY              VALUE '9'.
000391                 88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
000392                 88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
000393             20  PB-CI-AH-POLICY-STATUS   PIC X.
000394                 88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
000395                                           'M' '4' '5' '9' '2'.
000396                 88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
000397                 88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
000398                 88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
000399                 88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
000400                 88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
000401                 88  PB-CI-AH-POLICY-IS-CASH         VALUE 'C'.
000402                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
000403                 88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
000404                 88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
000405                 88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
000406                 88  PB-CI-AH-REIN-ONLY              VALUE '9'.
000407                 88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
000408                 88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
000409             20  PB-CI-PAY-FREQUENCY      PIC 99.
000410             20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
000411             20  PB-CI-SOC-SEC-NO         PIC X(11).
000412             20  PB-CI-MEMBER-NO          PIC X(12).
000413             20  PB-CI-INT-CODE           PIC X.
000414                 88  PB-CI-ADD-ON                  VALUE 'A'.
000415                 88  PB-CI-SIMPLE                  VALUE 'S'.
000416             20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
000417             20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
000418             20  PB-CI-COMP-EXCP-SW       PIC X.
000419                 88  PB-CI-NO-COMP-EXCP            VALUE ' '.
000420                 88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
000421             20  PB-CI-ENTRY-STATUS       PIC X.
000422             20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
000423             20  PB-CI-AH-PAID-THRU-DT    PIC XX.
000424             20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
000425             20  PB-CI-DEATH-DT           PIC XX.
000426             20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
000427             20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
000428             20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
000429             20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
000430             20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
000431             20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
000432             20  PB-CI-ENTRY-DT              PIC XX.
000433             20  PB-CI-ENTRY-BATCH           PIC X(6).
000434             20  PB-CI-LF-EXPIRE-DT          PIC XX.
000435             20  PB-CI-AH-EXPIRE-DT          PIC XX.
000436             20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
000437             20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
000438             20  PB-CI-OLD-LOF               PIC XXX.
000439*            20  PB-CI-LOAN-OFFICER          PIC XXX.
000440             20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
000441             20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
000442             20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
000443             20  PB-CI-INDV-GRP-CD           PIC X.
000444             20  PB-CI-BENEFICIARY-NAME.
000445                 24  PB-CI-BANK-NUMBER       PIC X(10).
000446                 24  FILLER                  PIC X(15).
000447             20  PB-CI-NOTE-SW               PIC X.
000448             20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
000449             20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
000450             20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
000451             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
000452             20  PB-CI-LOAN-OFFICER          PIC X(5).
000453             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
000454             20  PB-CI-FIRST-NAME            PIC X(10).
000455             20  PB-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
000456
000457         16  FILLER                       PIC X(13).
000458*032306  16  FILLER                       PIC X(27).
000459*        16  FILLER                       PIC X(46).
000460
000461     12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
000462         16  FILLER                       PIC X(10).
000463         16  PB-M-INSURED-LAST-NAME       PIC X(15).
000464         16  PB-M-INSURED-FIRST-NAME      PIC X(10).
000465         16  PB-M-INSURED-MID-INIT        PIC X.
000466         16  PB-M-INSURED-AGE             PIC 99.
000467         16  PB-M-INSURED-BIRTHDAY        PIC XX.
000468         16  PB-M-INSURED-SEX             PIC X.
000469         16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
000470         16  PB-M-INSURED-ADDRESS-1       PIC X(30).
000471         16  PB-M-INSURED-ADDRESS-2       PIC X(30).
000472         16  PB-M-INSURED-CITY-STATE.
000473             20  PB-M-INSURED-CITY        PIC X(28).
000474             20  PB-M-INSURED-STATE       PIC XX.
000475         16  PB-M-INSURED-ZIP-CODE.
000476             20  PB-M-INSURED-ZIP-PRIME.
000477                 24  PB-M-INSURED-ZIP-1   PIC X.
000478                     88  PB-M-CANADIAN-POST-CODE
000479                                             VALUE 'A' THRU 'Z'.
000480                 24  FILLER               PIC X(4).
000481             20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
000482         16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
000483                                        PB-M-INSURED-ZIP-CODE.
000484             20  PM-M-INS-CAN-POST1       PIC XXX.
000485             20  PM-M-INS-CAN-POST2       PIC XXX.
000486             20  FILLER                   PIC XXX.
000487         16  PB-M-INSURED-PHONE-NO        PIC 9(10).
000488         16  PB-M-JOINT-BIRTHDAY          PIC XX.
000489         16  PB-M-CRED-BENE-NAME          PIC X(30).
000490         16  PB-M-CRED-BENE-ADDR1         PIC X(30).
000491         16  PB-M-CRED-BENE-ADDR2         PIC X(30).
000492         16  PB-M-CRED-BENE-CITYST.
000493             20  PB-M-CRED-BENE-CITY      PIC X(28).
000494             20  PB-M-CRED-BENE-STATE     PIC XX.
000495
000496         16  FILLER                       PIC X(92).
000497
000498     12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
000499         16  FILLER                       PIC X(10).
000500         16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000501         16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000502         16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000503         16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000504         16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000505         16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000506         16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000507         16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000508         16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000509         16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000510         16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000511         16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000512         16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
000513         16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
000514         16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
000515         16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
000516         16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
000517         16  PB-ACCOUNT-NAME              PIC X(30).
000518         16  PB-PREM-REF-RPT-FLAG         PIC X.
000519         16  PB-REFERENCE                 PIC X(12).
000520         16  PB-B-RECEIVED-DT             PIC XX.
000521         16  FILLER                       PIC X(234).
000522
000523     12  PB-RECORD-STATUS.
000524         16  PB-CREDIT-SELECT-DT          PIC XX.
000525         16  PB-CREDIT-ACCEPT-DT          PIC XX.
000526         16  PB-BILLED-DT                 PIC XX.
000527         16  PB-BILLING-STATUS            PIC X.
000528             88  PB-ENTRY-REVERSED            VALUE 'R'.
000529             88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
000530             88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
000531         16  PB-RECORD-BILL               PIC X.
000532             88  PB-RECORD-ON-HOLD            VALUE 'H'.
000533             88  PB-RECORD-RETURNED           VALUE 'R'.
000534             88  PB-RECORD-ENDORSED           VALUE 'E'.
000535             88  PB-OVERRIDE-LIFE             VALUE 'L'.
000536             88  PB-OVERRIDE-AH               VALUE 'A'.
000537             88  PB-OVERRIDE-BOTH             VALUE 'B'.
000538         16  PB-BATCH-ENTRY               PIC X.
000539             88  PB-POLICY-IS-DECLINED        VALUE 'D'.
000540             88  PB-REIN-ONLY-CERT            VALUE 'R'.
000541             88  PB-REISSUED-CERT             VALUE 'E'.
000542             88  PB-CASH-CERT                 VALUE 'C'.
000543             88  PB-MONTHLY-CERT              VALUE 'M'.
000544             88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
000545             88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
000546             88  PB-POLICY-IS-VOIDED          VALUE 'V'.
000547         16  PB-FORCE-CODE                PIC X.
000548             88  PB-FORCE-OFF                 VALUE ' ' '0'.
000549             88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
000550             88  PB-CANCEL-FORCE              VALUE '8'.
000551             88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
000552             88  PB-ALL-CANCEL-FORCED         VALUE '8'.
000553             88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
000554             88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
000555             88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
000556             88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
000557             88  PB-EXCEEDED-LIMIT-FORCED     VALUE 'L'.
000558             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
000559         16  PB-FATAL-FLAG                PIC X.
000560             88  PB-FATAL-ERRORS              VALUE 'X'.
000561         16  PB-FORCE-ER-CD               PIC X.
000562             88  PB-FORCE-ERRORS              VALUE 'F'.
000563             88  PB-UNFORCED-ERRORS           VALUE 'X'.
000564         16  PB-WARN-ER-CD                PIC X.
000565             88  PB-WARNING-ERRORS            VALUE 'W'.
000566         16  FILLER                       PIC X.
000567         16  PB-OUT-BAL-CD                PIC X.
000568             88  PB-OUT-OF-BAL                VALUE 'O'.
000569         16  PB-LIFE-OVERRIDE-L1          PIC X.
000570         16  PB-AH-OVERRIDE-L1            PIC X.
000571         16  PB-INPUT-DT                  PIC XX.
000572         16  PB-INPUT-BY                  PIC X(4).
000573         16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
000574         16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
000575         16  PB-TOLERANCE-REJECT-SW       PIC X.
000576         16  PB-LF-EARNING-METHOD         PIC X.
000577         16  PB-AH-EARNING-METHOD         PIC X.
000578         16  PB-LF-TERM-CALC-METHOD       PIC X.
000579         16  PB-AH-TERM-CALC-METHOD       PIC X.
000580         16  PB-REIN-CD                   PIC XXX.
000581         16  PB-LF-REFUND-TYPE            PIC X.
000582         16  PB-AH-REFUND-TYPE            PIC X.
000583         16  PB-ACCT-EFF-DT               PIC XX.
000584         16  PB-ACCT-EXP-DT               PIC XX.
000585         16  PB-COMPANY-ID                PIC X(3).
000586         16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
000587         16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
000588         16  PB-SV-CARRIER                PIC X.
000589         16  PB-SV-GROUPING               PIC X(6).
000590         16  PB-SV-STATE                  PIC XX.
000591         16  PB-CONFIRMATION-REPT-DT      PIC XX.
000592         16  PB-GA-BILLING-INFO.
000593             20  PB-GA-BILL-DT OCCURS 5 TIMES
000594                                          PIC XX.
000595         16  PB-SV-REMIT-TO  REDEFINES
000596             PB-GA-BILLING-INFO           PIC X(10).
000597         16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
000598         16  PB-I-LOAN-OFFICER            PIC X(5).
000599         16  PB-I-VIN                     PIC X(17).
000600
000601         16  FILLER                       PIC X(04).
000602         16  IMNET-BYPASS-SW              PIC X.
000603
000604******************************************************************
000605*                COMMON EDIT ERRORS                              *
000606******************************************************************
000607
000608     12  PB-COMMON-ERRORS.
000609         16  PB-COMMON-ERROR    OCCURS 10 TIMES
000610                                           PIC S9(4)     COMP.
000611
000612******************************************************************
      *<<((file: ERCPNDB))
000285*                                copy ERCPNDM.
      *>>((file: ERCPNDM))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPNDM                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*   FILE DESCRIPTION = PENDING MAILING DATA                      *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 374   RECFORM = FIX                            *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERPNDM                 RKP=2,LEN=11      *
000014*   ALTERNATE PATH    = NOT USED                                 *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
000027* 071108  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
000028* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000029* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
000030******************************************************************
000031
000032 01  PENDING-MAILING-DATA.
000033     12  PM-RECORD-ID                      PIC XX.
000034         88  VALID-MA-ID                       VALUE 'PM'.
000035
000036     12  PM-CONTROL-PRIMARY.
000037         16  PM-COMPANY-CD                 PIC X.
000038         16  PM-ENTRY-BATCH                PIC X(6).
000039         16  PM-BATCH-SEQ-NO               PIC S9(4)     COMP.
000040         16  PM-BATCH-CHG-SEQ-NO           PIC S9(4)     COMP.
000041
000042     12  FILLER                            PIC X(14).
000043
000044     12  PM-ACCESS-CONTROL.
000045         16  PM-SOURCE-SYSTEM              PIC XX.
000046             88  PM-FROM-CREDIT                VALUE 'CR'.
000047             88  PM-FROM-VSI                   VALUE 'VS'.
000048             88  PM-FROM-WARRANTY              VALUE 'WA'.
000049             88  PM-FROM-OTHER                 VALUE 'OT'.
000050         16  PM-RECORD-ADD-DT              PIC XX.
000051         16  PM-RECORD-ADDED-BY            PIC XXXX.
000052         16  PM-LAST-MAINT-DT              PIC XX.
000053         16  PM-LAST-MAINT-BY              PIC XXXX.
000054         16  PM-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
000055
000056     12  PM-PROFILE-INFO.
000057         16  PM-QUALIFY-CODE-1             PIC XX.
000058         16  PM-QUALIFY-CODE-2             PIC XX.
000059         16  PM-QUALIFY-CODE-3             PIC XX.
000060         16  PM-QUALIFY-CODE-4             PIC XX.
000061         16  PM-QUALIFY-CODE-5             PIC XX.
000062
000063         16  PM-INSURED-LAST-NAME          PIC X(15).
000064         16  PM-INSURED-FIRST-NAME         PIC X(10).
000065         16  PM-INSURED-MIDDLE-INIT        PIC X.
000066         16  PM-INSURED-ISSUE-AGE          PIC 99.
000067         16  PM-INSURED-BIRTH-DT           PIC XX.
000068         16  PM-INSURED-SEX                PIC X.
000069             88  PM-SEX-MALE                   VALUE 'M'.
000070             88  PM-SEX-FEMALE                 VALUE 'F'.
000071         16  PM-INSURED-SOC-SEC-NO         PIC X(11).
000072
000073         16  PM-ADDRESS-CORRECTED          PIC X.
000074         16  PM-JOINT-BIRTH-DT             PIC XX.
000075*        16  FILLER                        PIC X(12).
000076
000077         16  PM-ADDRESS-LINE-1             PIC X(30).
000078         16  PM-ADDRESS-LINE-2             PIC X(30).
000079         16  PM-CITY-STATE.
000080             20  PM-CITY                   PIC X(28).
000081             20  PM-STATE                  PIC XX.
000082         16  PM-ZIP.
000083             20  PM-ZIP-CODE.
000084                 24  PM-ZIP-1              PIC X.
000085                     88  PM-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000086                 24  FILLER                PIC X(4).
000087             20  PM-ZIP-PLUS4              PIC X(4).
000088         16  PM-CANADIAN-ZIP  REDEFINES  PM-ZIP.
000089             20  PM-CAN-POST1              PIC XXX.
000090             20  PM-CAN-POST2              PIC XXX.
000091             20  FILLER                    PIC XXX.
000092
000093         16  PM-PHONE-NO                   PIC 9(11)       COMP-3.
000094         16  pm-city-st-zip-verified       pic x.
000095         16  FILLER                        PIC XX.
000096
000097     12  PM-CRED-BENE-INFO.
000098         16  PM-CRED-BENE-NAME             PIC X(25).
000099         16  PM-CRED-BENE-ADDR             PIC X(30).
000100         16  PM-CRED-BENE-ADDR2            PIC X(30).
000101         16  PM-CRED-BENE-CTYST.
000102             20  PM-CRED-BENE-CITY         PIC X(28).
000103             20  PM-CRED-BENE-STATE        PIC XX.
000104         16  PM-CRED-BENE-ZIP.
000105             20  PM-CB-ZIP-CODE.
000106                 24  PM-CB-ZIP-1           PIC X.
000107                     88  PM-CB-CANADIAN-POST-CODE
000108                                  VALUE 'A' THRU 'Z'.
000109                 24  FILLER                PIC X(4).
000110             20  PM-CB-ZIP-PLUS4           PIC X(4).
000111         16  PM-CB-CANADIAN-ZIP  REDEFINES  PM-CRED-BENE-ZIP.
000112             20  PM-CB-CAN-POST1           PIC XXX.
000113             20  PM-CB-CAN-POST2           PIC XXX.
000114             20  FILLER                    PIC XXX.
000115     12  PM-POST-CARD-MAIL-DATA.
000116         16  PM-MAIL-DATA OCCURS 7.
000117             20  PM-MAIL-TYPE              PIC X.
000118                 88  PM-12MO-MAILING           VALUE '1'.
000119                 88  PM-EXP-MAILING            VALUE '2'.
000120             20  PM-MAIL-STATUS            PIC X.
000121                 88  PM-MAIL-ST-MAILED         VALUE '1'.
000122                 88  PM-MAIL-ST-RETURNED       VALUE '2'.
000123                 88  PM-MAIL-ST-NOT-MAILED     VALUE '3'.
000124             20  PM-MAIL-DATE              PIC XX.
000125     12  FILLER                            PIC XX.
000126     12  FILLER                            PIC X(12).
000127*    12  FILLER                            PIC X(30).
000128
000129******************************************************************
      *<<((file: ERCPNDM))
000286
000287 01  w-doctoken                  pic x(16).
000288
000289 01  BATCH-TO-PROCESS.
000290     05  EDIT-COMPANY-CD         PIC X       VALUE LOW-VALUES.
000291     05  EDIT-BATCH              PIC X(6)    VALUE SPACES.
000292     05  EDIT-COMPANY-ID         PIC XXX     VALUE SPACES.
000293     05  EDIT-RESTART-BATCH      PIC X(6)    VALUE SPACES.
000294
000295 01 output-data.
000296    05  filler                   pic x(6) value "TITLE=".
000297    05  output-title             pic x(26) value 'Check Posting'.
000298    05  filler                   pic x(8) value "&COMPID=".
000299    05  out-comp-id              pic xxx.
000300    05  filler                   pic x(5) value "&MSG=".
000301    05  output-msg               pic x(50).
000302
000303 01  sqlconnect-parms.
000304     05  p-sql-server            PIC X(30).
000305     05  p-sql-database          PIC X(30).
000306     05  p-connect-return-code   pic s9(5) comp-5.
000307     05  p-sql-return-message    pic x(256).
000308
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
000310 01  DFHCOMMAREA                     PIC X(1024).
000311
000312 01  var  pic x(30).
000313
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL202' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000314 VCOBOL-DUMMY-PROCEDURE.
000315
000316***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000317***                                                            ***
000318***                                                We don't    ***
000319***  have any form fields to browse through, we are getting    ***
000320***  the information we need through the variables that are    ***
000321***  included in the URL. These variables are retrieved by     ***
000322***  the           exec cics web extract                       ***
000323***                   querystring  (data-value)                ***
000324***                   querystrlen  (data-value)                ***
000325***                end-exec                                    ***
000326***  BTW, you must provide the length, it isn't passed to you. ***
000327***  Allow for extra on the length or you will get a length    ***
000328***  error instead of just truncation.                         ***
000329***                                                            ***
000330***  This program is expecting                                 ***
000331*** http://logictest:5002/cics/cwba/EL202?comp=DCC&remit=00001 ***
000332***http://slunikix:7001/cics/cwba/EL202?comp=DCC&remit=0000000001*
000333***                                                            ***
000334***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000335
000336     MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
000337     string ws-fn-hours ws-fn-minutes ws-fn-seconds
000338        delimited by size into ws-work-time-a
000339     end-string
000340     string ws-fn-mo     '/'
000341            ws-fn-da     '/'
000342            ws-fn-ccyr
000343        delimited by size into ws-status-date
000344     end-string
000345     move ws-fn-cymd             to dc-greg-date-cymd
000346     move 'L'                    to dc-option-code
000347     perform 9700-date-convert   thru 9700-exit
000348     if no-conversion-error
000349        move dc-bin-date-1       to ws-bin-current-dt
000350     else
000351        display ' error current dt invalid ' dc-error-code
000352     end-if
000353
000354     set P to address of KIXSYS
000355     CALL "getenv" using by value P returning var-ptr
000356     if var-ptr = null then
000357        display ' kixsys not set '
000358     else
000359        set address of var to var-ptr
000360        move 0 to env-var-len
000361        inspect var tallying env-var-len
000362          for characters before X'00'
000363        unstring var (1:env-var-len) delimited by '/'
000364           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000365              WS-KIX-SYS
000366        end-unstring
000367     end-if
000368
000369     
      * exec cics web extract
000370*       querystring  (ws-qry-string)
000371*       querystrlen  (ws-qrystr-len)
000372*    end-exec
      *    MOVE 'X0  QL                .   #00003132' TO DFHEIV0
           MOVE X'58302020514C202020202020' &
                X'202020202020202020202E20' &
                X'2020233030303033313332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 ws-qry-string, 
                 ws-qrystr-len, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000373
000374     display ' qry string ' ws-qry-string
000375     display ' gry len    ' ws-qrystr-len
000376
000377     move ws-qry-string (6:3)    to ws-comp-id
000378     move ws-qry-string (16:10)  to ws-var-alpha
000379     move zeros                  to ws-remit-id-a
000380     move +10                    to o1
000381     perform varying i1 from +10 by -1 until i1 < +1
000382        if ws-var-alpha (i1:1) numeric
000383           move ws-var-alpha (i1:1) to ws-remit-id-a (o1:1)
000384           subtract +1 from o1
000385        end-if
000386     end-perform
000387
000388     move 'Successful Run   '    to output-msg
000389
000390     move ws-comp-id             to elcntl-key
000391     move '1'                    to elcntl-rec-type
000392     move +0                     to elcntl-seq
000393     
      * exec cics read
000394*       dataset  ('ELCNTL')
000395*       into     (control-file)
000396*       ridfld   (elcntl-key)
000397*       resp     (ws-response)
000398*    end-exec
           MOVE LENGTH OF
            control-file
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00003156' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303033313536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 control-file, 
                 DFHEIV11, 
                 elcntl-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000399
000400     if resp-normal
000401        display ' good cntl read '
000402        move cf-cr-month-end-dt  to ws-bin-eom-dt
000403        move cf-company-cd       to ws-comp-cd
000404     else
000405        move 'Company Rec not found ' to output-msg
000406        display ' error elcntl read ' ws-response ' '
000407           elcntl-key (1:8)
000408        go to 0000-return
000409     end-if
000410
000411     perform 0010-connect-db     thru 0010-exit
000412
000413     perform 0020-process-remit  thru 0020-exit
000414
000415     perform 0050-open-cursor    thru 0050-exit
000416     perform 0060-process-input  thru 0060-exit
000417
000418     perform 0130-start-edit     thru 0130-exit
000419
000420
000421     perform 0226-update-remit-table
000422                                 thru 0226-exit
000423     perform 0225-commit-work    thru 0225-exit
000424
000425     .
000426 0000-return.
000427
000428     if connected-to-db
000429        perform 0230-disconnect  thru 0230-exit
000430     end-if
000431
000432     perform 0220-send-form      thru 0220-exit
000433
000434     if rollback-needed
000435        display ' executing roll back '
000436        perform 0005-rollback    thru 0005-exit
000437*       CALL 'kixvsam' USING ROLL-BACK
000438*       exec cics syncpoint
000439*          rollback
000440*          resp (ws-response)
000441*       end-exec
000442*       display ' rollback resp ' ws-response
000443     end-if
000444
000445     
      * exec cics
000446*       return
000447*    end-exec
      *    MOVE '.(                    ''   #00003208' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033323038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000448     
      * goback

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL202' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback
000449
000450     .
000451 0005-rollback.
000452
000453     if ws-rb-erpyaj-key not = spaces
000454        move ws-rb-erpyaj-key    to py-control-primary
000455        
      * EXEC CICS DELETE
000456*          DATASET  ('ERPYAJ')
000457*          RIDFLD   (PY-CONTROL-PRIMARY)
000458*          resp     (ws-response)
000459*       END-EXEC
           MOVE 'ERPYAJ' TO DFHEIV1
      *    MOVE '&(  R                 &  N#00003218' TO DFHEIV0
           MOVE X'262820205220202020202020' &
                X'202020202020202020202620' &
                X'204E233030303033323138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000460        if resp-normal
000461           display ' good delete on erpyaj '
000462        else
000463           display ' bad delete on erpyaj ' ws-response
000464        end-if
000465     end-if
000466
000467     if ws-batch-no not = zeros
000468        move ws-comp-cd          to pb-company-cd
000469        move ws-batch-no         to pb-entry-batch
000470        move +0                  to pb-batch-seq-no
000471                                    pb-batch-chg-seq-no
000472        
      * exec cics delete
000473*          dataset   ('ERPNDB')
000474*          ridfld    (pb-control-primary)
000475*          keylength (7)
000476*          generic
000477*          resp     (ws-response)
000478*       end-exec
           MOVE 'ERPNDB' TO DFHEIV1
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &  N#00003235' TO DFHEIV0
           MOVE X'26282020524B472020202020' &
                X'202020202020202020202620' &
                X'204E233030303033323335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 pb-control-primary, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000479        if resp-normal
000480           display ' good delete on erpndb '
000481        else
000482           display ' bad delete on erpndp ' ws-response
000483        end-if
000484     end-if
000485
000486     if ws-batch-no not = zeros
000487        move ws-comp-cd          to pm-company-cd
000488        move ws-batch-no         to pm-entry-batch
000489        move +0                  to pm-batch-seq-no
000490                                    pm-batch-chg-seq-no
000491        
      * exec cics delete
000492*          dataset   ('ERPNDM')
000493*          ridfld    (pm-control-primary)
000494*          keylength (7)
000495*          generic
000496*          resp     (ws-response)
000497*       end-exec
           MOVE 'ERPNDM' TO DFHEIV1
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &  N#00003254' TO DFHEIV0
           MOVE X'26282020524B472020202020' &
                X'202020202020202020202620' &
                X'204E233030303033323534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 pm-control-primary, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000498        if resp-normal
000499           display ' good delete on erpndm '
000500        else
000501           display ' bad delete on erpndm ' ws-response
000502        end-if
000503     end-if
000504
000505     .
000506 0005-exit.
000507     exit.
000508
000509 0010-CONNECT-DB.
000510
000511***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000512***                                                            ***
000513***  Even though it's on ntcso2, CUConnect is the test DB      ***
000514***  The production DB is ezLink_Prod.  The table names are    ***
000515***  the same.                                                 ***
000516***                                                            ***
000517***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000518
000519****  The below code is for when the db has been
000520****  converted to sql server 2016
000521     evaluate ws-kix-myenv
000522        when 'cid1p'
000523           move '//sdv-db01.cso.local:1433;'
000524                                 to p-sql-server
000525        when 'mdoff'
000526           move '//hov-tstdb01.cso.local:55330;'
000527                                 to p-sql-server
000528        when other
000529           move '//hov-tstdb01.cso.local:1433;'
000530                                 to p-sql-server
000531     end-evaluate
000532
000533
000534     move 'ezLink_Prod'          to p-sql-database
000535
000536     CALL 'SQLCONNECT' USING sqlconnect-parms
000537     display ' ret code ' p-connect-return-code
000538     move p-connect-return-code  to sqlcode
000539     move p-sql-return-message   to sqlerrmc
000540
000541*
000542*     EXEC SQL
000543**       CONNECT TO :svr USER :usr-pass
000544*        CONNECT TO :svr
000545*          USER     :usr
000546*          USING    :pass
000547*     END-EXEC
000548
000549     if sqlcode not = 0
000550        display "Error: cannot connect to " svr
000551        display sqlcode
000552        display sqlerrmc
000553        move 'Failed to connect to DB'
000554                                 to output-msg
000555        go to 0000-return
000556     else
000557        set connected-to-db to true
000558        display ' Successful Connect ' sqlcode
000559     end-if
000560
000561     .
000562 0010-EXIT.
000563     EXIT.
000564
000565 0020-process-remit.
000566
000567     perform 0030-get-tbl-row    thru 0030-exit
000568     perform 0040-build-erpyaj   thru 0040-exit
000569
000570     .
000571 0020-exit.
000572     exit.
000573
000574 0030-get-tbl-row.
000575
000576***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000577***                                                            ***
000578***  I'm only expecting one row so no cursor is declared       ***
000579***                                                            ***
000580***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000581
000582     display ' about to get table row ' ws-remit-id
000583     move ' '                    to ws-remit-sw
000584
000585*    exec sql
000586*       SELECT
000587*          RemitId,
000588*          Carrier,
000589*          Group,
000590*          State,
000591*          AccountNumber,
000592*          Name,
000593*          Addr1,
000594*          Addr2,
000595*          AddrCity,
000596*          AddrState,
000597*          AddrZIP,
000598*          ResponsibleCarrier,
000599*          ResponsibleNumber,
000600*          LogicStatus,
000601*          LogicStatusDate,
000602*          Company,
000603*          TotalPremium
000604*       INTO
000605*          :sql-py-RemitId,
000606*          :sql-py-Carrier,
000607*          :sql-py-Group,
000608*          :sql-py-State,
000609*          :sql-py-AccountNumber,
000610*          :sql-py-Name,
000611*          :sql-py-Addr1,
000612*          :sql-py-Addr2,
000613*          :sql-py-AddrCity,
000614*          :sql-py-AddrState,
000615*          :sql-py-AddrZIP,
000616*          :sql-py-ResponsibleCarrier,
000617*          :sql-py-ResponsibleNumber,
000618*          :sql-py-LogicStatus,
000619*          :sql-py-LogicStatusDate,
000620*          :sql-py-company,
000621*          :sql-py-tot-prem
000622*       FROM
000623*          CUC_Logic_Remittance
000624*       WHERE
000625*          RemitId = :ws-remit-id
000626*    end-exec
000627
000629     exec sql
              SELECT
000630           CUC_Logic_Remittance.RemitId,
000631           Carrier,
000632           [Group],
000633           [State],
000634           AccountNumber,
000635           Name,
000636           Addr1,
000637           Addr2,
000638           AddrCity,
000639           AddrState,
000640           AddrZIP,
000641           ResponsibleCarrier,
000642           ResponsibleNumber,
000643           LogicStatus,
000644           Company,
000645           TotalPremium,
000646           CUC_Remittance.IsACH
000647        INTO
000648           :sql-py-RemitId,
000649           :sql-py-Carrier,
000650           :sql-py-Group,
000651           :sql-py-State,
000652           :sql-py-AccountNumber,
000653           :sql-py-Name,
000654           :sql-py-Addr1,
000655           :sql-py-Addr2,
000656           :sql-py-AddrCity,
000657           :sql-py-AddrState,
000658           :sql-py-AddrZIP,
000659           :sql-py-ResponsibleCarrier,
000660           :sql-py-ResponsibleNumber,
000661           :sql-py-LogicStatus,
000662           :sql-py-company,
000663           :sql-py-tot-prem,
000664           :sql-is-ach
000665        FROM
000666           CUC_Logic_Remittance inner join CUC_Remittance on
000667              CUC_Logic_Remittance.RemitId =
000668                 CUC_Remittance.RemitId
000669        WHERE
000670           CUC_Logic_Remittance.RemitId = :ws-remit-id
000671     end-exec
000672
000673     if sqlcode not = 0
000674        set ws-remit-not-found   to true
000675        move sqlcode             to ws-sql-code
000676        move ws-sql-code         to ws-dis-sql-code
000677        display ' dis sql code ' ws-dis-sql-code
000678        display "Error: cannot read row "
000679        display ' sql return code ' sqlcode
000680        display ' sql err mess    ' sqlerrmc
000681        move ' Remittance Not Found '
000682                                 to output-msg
000683        go to 0000-return
000684     end-if
000685
000686     display ' good get on table ' sqlcode
000687
000688     if sql-py-LogicStatus = 2
000689        move 'Remit previously processed '
000690                                 to output-msg
000691        go to 0000-return
000692     end-if
000693
000694     .
000695 0030-exit.
000696     exit.
000697
000698 0040-build-erpyaj.
000699
000700     compute work-seq-no = ws-work-time * 3
000701     MOVE 'PY'                   TO pending-pay-adj
000702     move zeros                  to py-file-seq-no
000703                                    py-last-maint-hhmmss
000704                                    py-entry-amt
000705                                    py-check-que-control
000706                                    py-check-que-sequence
000707     move low-values             to py-last-maint-dt
000708                                    py-credit-select-dt
000709                                    py-credit-accept-dt
000710                                    py-billed-date
000711                                    py-reported-dt
000712                                    py-input-dt
000713                                    py-check-written-dt
000714                                    py-ar-date
000715                                    py-gl-date
000716
000717     MOVE cf-COMPANY-CD          TO PY-COMPANY-CD
000718     MOVE function upper-case(sql-py-responsiblecarrier)
000719                                 TO PY-CARRIER
000720     MOVE function upper-case(sql-py-group)
000721                                 TO PY-GROUPING
000722     MOVE function upper-case(sql-py-responsiblenumber)
000723                                 TO PY-FIN-RESP
000724     MOVE function upper-case(sql-py-accountnumber)
000725                                 TO PY-ACCOUNT
000726
000727     ADD +1                      TO WORK-SEQ-NO.
000728     MOVE WORK-SEQ-NO            TO PY-FILE-SEQ-NO.
000729
000730     move 'R'                    to py-record-type
000731     move 'E202'                 to py-last-maint-by
000732     move ws-bin-current-dt      to py-last-maint-dt
000733                                    py-input-dt
000734     move ws-work-time           to py-last-maint-hhmmss
000735*    move +20.00                 to py-entry-amt
000736     move ws-bin-eom-dt          to py-credit-select-dt
000737     move 'A'                    to py-ercomp-type
000738
000739     move sql-is-ach             to ws-bit
000740     display ' bit comp ' ws-bit-comp
000741
000742     evaluate true
000743        when (ws-comp-id = 'DCC')
000744           and (py-carrier = '9')
000745           and (ws-bit-comp = zeros)      *> ACH FALSE
000746           move '2725040310'     to py-gl-account
000747        when (ws-comp-id = 'DCC')
000748           and (py-carrier = '7')
000749           and (ws-bit-comp not = zeros)  *> ACH TRUE
000750           move '1108121010'     to py-gl-account
000751        when (ws-comp-id = 'DCC')
000752           and (py-carrier = '9')
000753           and (ws-bit-comp not = zeros)  *> ACH TRUE
000754           move '1108121250'     to py-gl-account
000755        when (ws-comp-id = 'CID')
000756           and (ws-bit-comp not = zeros)  *> ACH TRUE
000757           move '1108124700'     to py-gl-account
000758        when (ws-comp-id = 'DCC')
000759           move '1825013400'     to py-gl-account
000760        when other
000761           move '1825099050'     to py-gl-account
000762     end-evaluate
000763
000764     if ws-bit-comp not = zeros     *>  ACH TRUE
000765        move 'ACH'               to py-gl-comment
000766     end-if
000767*    if ws-comp-id = 'CID'
000768*       move '1825099050'        to py-gl-account
000769*    else
000770*       move '1825013400'        to py-gl-account
000771*    end-if
000772
000773     move +11                    to s2
000774     move zeros                  to ws-work-amt-alpha
000775     perform varying s1 from +11 by -1 until s1 < +1
000776        if sql-py-tot-prem (s1:1) numeric
000777           move sql-py-tot-prem (s1:1)
000778                                 to ws-work-amt-alpha (s2:1)
000779           subtract +1 from s2
000780        end-if
000781     end-perform
000782     display ' tot premium ' ws-work-amt-num
000783     move ws-work-amt-num        to py-entry-amt
000784
000785     .
000786 0040-write.
000787
000788     display ' about to write pyaj '
000789     
      * EXEC CICS WRITE
000790*        DATASET  ('ERPYAJ')
000791*        FROM     (PENDING-PAY-ADJ)
000792*        RIDFLD   (PY-CONTROL-PRIMARY)
000793*        resp     (ws-response)
000794*    END-EXEC
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
           MOVE 'ERPYAJ' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003552' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303033353532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000795
000796     if resp-normal
000797        move py-control-primary  to ws-rb-erpyaj-key
000798        move 2                   to ws-status-code
000799     else
000800        if resp-duprec
000801           add +1                to py-file-seq-no
000802           go to 0040-write
000803        else
000804           move 3                to ws-status-code
000805           move ws-response      to ws-display-response
000806           move spaces           to ws-error-message
000807           string ' Bad write on ERPYAJ FILE ' ws-display-response
000808              delimited by size into ws-error-message
000809           end-string
000810           perform 0226-update-remit-table
000811                                 thru 0226-exit
000812           perform 0225-commit-work
000813                                 thru 0225-exit
000814           go to 0000-return
000815        end-if
000816     end-if
000817
000818     .
000819 0040-exit.
000820     exit.
000821
000822 0050-open-cursor.
000823
000824*    display ' declare cursor ' ws-begin-dt ' ' ws-end-dt
000825
000826***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000827***                                                            ***
000828***  The dates on the sql table have values in the time        ***
000829***  so I convert it to a string and just use mm/dd/yyyy       ***
000830***  to perform the comparison.                                ***
000831***                                                            ***
000832***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000833
000835     EXEC SQL
              DECLARE
000836           remitcerts cursor for
000837        SELECT
000838           CertId,
000839           RemitId,
000840           CertNumber,
000841           EffectiveDate,
000842           FirstPaymentDate,
000843           FirstName,
000844           LastName,
000845           CashIndicator,
000846           LivesCovered,
000847           LfBenCode,
000848           LfTerm,
000849           LfBenefit,
000850           LfPremium,
000851           AhBenCode,
000852           AhTerm,
000853           AhBenefit,
000854           AhPremium,
000855           ErrorMessage
000856        FROM
000857           CUC_Logic_Remittance_Cert
000858        WHERE
000859           RemitId = :ws-remit-id
000860     end-exec
000861
000862     if sqlcode not = 0
000863        display "Error: cannot declare cursor "
000864        display ' sql retrun code ' sqlcode
000865        display ' sql err mess    ' sqlerrmc
000866        move ' Error declaring cursor '
000867                                 to output-msg
000868        move 3                   to ws-status-code
000869        move sqlcode             to ws-display-response
000870        move spaces              to ws-error-message
000871        string ' Error declaring cursor   ' ws-display-response
000872           delimited by size into ws-error-message
000873        end-string
000874        perform 0226-update-remit-table
000875                                 thru 0226-exit
000876        perform 0225-commit-work thru 0225-exit
000877        go to 0000-return
000878     end-if
000879
000880     display ' good declare cursor ' ws-remit-id
000881
000883     EXEC SQL
              open remitcerts
000884     END-EXEC
000885
000886     if sqlcode not = 0
000887        display "Error: cannot open cursor "
000888        display ' sql retrun code ' sqlcode
000889        display ' sql err mess    ' sqlerrmc
000890        move ' Error opening cursor '
000891                                 to output-msg
000892        move 3                   to ws-status-code
000893        move sqlcode             to ws-display-response
000894        move spaces              to ws-error-message
000895        string ' Error openning cursor    ' ws-display-response
000896           delimited by size into ws-error-message
000897        end-string
000898        perform 0226-update-remit-table
000899                                 thru 0226-exit
000900        perform 0225-commit-work thru 0225-exit
000901        go to 0000-return
000902     end-if
000903
000904     display ' good open cursor ' ws-remit-id
000905
000906     .
000907 0050-exit.
000908     exit.
000909
000910 0060-process-input.
000911
000912     perform until sqlcode not = 0
000914        EXEC SQL
                 fetch remitcerts into
000915              :sql-pb-CertId,
000916              :sql-pb-RemitId,
000917              :sql-pb-CertNumber,
000918              :sql-pb-EffectiveDate,
000919              :sql-pb-FirstPaymentDate,
000920              :sql-pb-FirstName,
000921              :sql-pb-LastName,
000922              :sql-pb-CashIndicator,
000923              :sql-pb-LivesCovered,
000924              :sql-pb-LfBenCode,
000925              :sql-pb-LfTerm,
000926              :sql-pb-LfBenefit,
000927              :sql-pb-LfPremium,
000928              :sql-pb-AhBenCode,
000929              :sql-pb-AhTerm,
000930              :sql-pb-AhBenefit,
000931              :sql-pb-AhPremium,
000932              :sql-pb-ErrorMessage
000933        END-EXEC
000934
000935        if sqlcode = 0
000936           perform 0070-bld-erpndb
000937                                 thru 0070-exit
000938           perform 0110-write-erpndb
000939                                 thru 0110-exit
000940           perform 0120-write-erpndm
000941                                 thru 0120-exit
000942        else
000943           if sqlcode not = 0 and 100
000944              display "Error: cannot fetch row "
000945              display ' sql return code ' sqlcode
000946              display ' sql err mess    ' sqlerrmc
000947              move ' Error fetching cursor '
000948                                 to output-msg
000949              set rollback-needed to true
000950              move 3             to ws-status-code
000951              move sqlcode       to ws-display-response
000952              move spaces        to ws-error-message
000953              string ' Error during fetch cursor '
000954                ws-display-response
000955                 delimited by size into ws-error-message
000956              end-string
000957              perform 0226-update-remit-table
000958                                 thru 0226-exit
000959              perform 0225-commit-work
000960                                 thru 0225-exit
000961              go to 0000-return
000962           end-if
000963        end-if
000964     end-perform
000965
000966     if sqlcode = 100
000967        display ' Normal end of record set '
000968        perform 0100-build-batch-hdr
000969                                 thru 0100-exit
000970        perform 0110-write-erpndb
000971                                 thru 0110-exit
000972     end-if
000973
000975     EXEC SQL
               close remitcerts
000976     END-EXEC
000977
000978     if sqlcode not = 0
000979        display "Error: cannot close cursor "
000980        display ' sql retrun code ' sqlcode
000981        display ' sql err mess    ' sqlerrmc
000982     end-if
000983
000984     .
000985 0060-exit.
000986     exit.
000987
000988 0070-bld-erpndb.
000989
000990     if ws-batch-no-n = zeros
000991        perform 0080-get-batch-no thru 0080-exit
000992     end-if
000993     move ws-init-erpndb-rec     to pending-business
000994     move ws-batch-no            to pb-entry-batch
000995                                    pb-original-entry-batch
000996                                    pb-csr-entry-batch
000997     add +1 to ws-batch-seq-no
000998     move ws-batch-seq-no        to pb-batch-seq-no
000999                                    pb-original-seq-no
001000                                    pb-csr-batch-seq-no
001001
001002     move function upper-case(sql-pb-certnumber)
001003                                 to pb-cert-no
001004
001005     string sql-pb-effectivedate (1:4)
001006            sql-pb-effectivedate (6:2)
001007            sql-pb-effectivedate (9:2)
001008        delimited by size into dc-greg-date-cymd-r
001009     end-string
001010     move 'L'                    to dc-option-code
001011     perform 9700-date-convert   thru 9700-exit
001012     if no-conversion-error
001013        move dc-bin-date-1       to pb-cert-eff-dt
001014     else
001015        display ' error cvtdte eff dt ' sql-pb-effectivedate
001016           ' ' dc-error-code
001017     end-if
001018     move function upper-case(sql-pb-lastname)
001019                                 to pb-i-insured-last-name
001020     move function upper-case(sql-pb-firstname)
001021                                 to pb-i-insured-first-name
001022     if sql-pb-lfbencode = zeros
001023        move spaces              to sql-pb-lfbencode
001024     end-if
001025     move function upper-case(sql-pb-lfbencode)
001026                                 to pb-i-life-benefit-cd
001027                                    pb-i-lf-input-cd
001028     if pb-i-life-benefit-cd = spaces
001029        move zeros               to pb-i-life-benefit-cd
001030     end-if
001031     move sql-pb-lfterm          to pb-i-lf-term
001032
001033     move 'L'                    to pb-ah-override-l1
001034     move +11                    to s2
001035     move zeros                  to ws-work-amt-alpha
001036     perform varying s1 from +12 by -1 until s1 < +1
001037        if sql-pb-lfbenefit (s1:1) numeric
001038           move sql-pb-lfbenefit (s1:1)
001039                                 to ws-work-amt-alpha (s2:1)
001040           subtract +1 from s2
001041        end-if
001042     end-perform
001043     display ' lf benefit  ' ws-work-amt-num
001044     move ws-work-amt-num        to pb-i-lf-benefit-amt
001045
001046     move +11                    to s2
001047     move zeros                  to ws-work-amt-alpha
001048     perform varying s1 from +10 by -1 until s1 < +1
001049        if sql-pb-lfpremium (s1:1) numeric
001050           move sql-pb-lfpremium (s1:1)
001051                                 to ws-work-amt-alpha (s2:1)
001052           subtract +1 from s2
001053        end-if
001054     end-perform
001055     display ' lf premium  ' ws-work-amt-num
001056     move ws-work-amt-num        to pb-i-lf-premium-amt
001057
001058     if sql-pb-ahbencode = zeros
001059        move spaces              to sql-pb-ahbencode
001060     end-if
001061     move function upper-case(sql-pb-ahbencode)
001062                                 to pb-i-ah-benefit-cd
001063                                    pb-i-ah-input-cd
001064     if pb-i-ah-benefit-cd = spaces
001065        move zeros               to pb-i-ah-benefit-cd
001066     end-if
001067     move 'A'                    to pb-ah-override-l1
001068     move sql-pb-ahterm          to pb-i-ah-term
001069
001070     move +11                    to s2
001071     move zeros                  to ws-work-amt-alpha
001072     perform varying s1 from +12 by -1 until s1 < +1
001073        if sql-pb-ahbenefit (s1:1) numeric
001074           move sql-pb-ahbenefit (s1:1)
001075                                 to ws-work-amt-alpha (s2:1)
001076           subtract +1 from s2
001077        end-if
001078     end-perform
001079     display ' ah benefit  ' ws-work-amt-num
001080     move ws-work-amt-num        to pb-i-ah-benefit-amt
001081
001082     move +11                    to s2
001083     move zeros                  to ws-work-amt-alpha
001084     perform varying s1 from +10 by -1 until s1 < +1
001085        if sql-pb-ahpremium (s1:1) numeric
001086           move sql-pb-ahpremium (s1:1)
001087                                 to ws-work-amt-alpha (s2:1)
001088           subtract +1 from s2
001089        end-if
001090     end-perform
001091     display ' ah premium  ' ws-work-amt-num
001092     move ws-bin-eom-dt          to pb-credit-select-dt
001093
001094     move ws-work-amt-num        to pb-i-ah-premium-amt
001095
001096     move sql-pb-LivesCovered    to pb-i-lives
001097     move function upper-case(sql-pb-CashIndicator)
001098                                 to pb-i-entry-status
001099                                    pb-batch-entry
001100
001101     compute ws-lf-iss-premium =
001102        ws-lf-iss-premium + pb-i-lf-premium-amt
001103
001104     compute ws-ah-iss-premium =
001105        ws-ah-iss-premium + pb-i-ah-premium-amt
001106
001107     move 'A'                    to pb-force-code
001108
001109     compute ws-iss-cnt = ws-iss-cnt + +1
001110     move pb-batch-seq-no           to ws-highest-seq-no
001111
001112*    display ' sql-pb-CertId           ' sql-pb-CertId
001113*    display ' sql-pb-RemitId          ' sql-pb-RemitId
001114*    display ' sql-pb-CertNumber       ' sql-pb-CertNumber
001115*    display ' sql-pb-EffectiveDate    ' sql-pb-EffectiveDate
001116*    display ' sql-pb-FirstPaymentDate ' sql-pb-FirstPaymentDate
001117*    display ' sql-pb-FirstName        ' sql-pb-FirstName
001118*    display ' sql-pb-LastName         ' sql-pb-LastName
001119*    display ' sql-pb-CashIndicator    ' sql-pb-CashIndicator
001120*    display ' sql-pb-LivesCovered     ' sql-pb-LivesCovered
001121*    display ' sql-pb-LfBenCode        ' sql-pb-LfBenCode
001122*    display ' sql-pb-LfTerm           ' sql-pb-LfTerm
001123*    display ' sql-pb-LfBenefit        ' sql-pb-LfBenefit
001124*    display ' sql-pb-LfPremium        ' sql-pb-LfPremium
001125*    display ' sql-pb-AhBenCode        ' sql-pb-AhBenCode
001126*    display ' sql-pb-AhTerm           ' sql-pb-AhTerm
001127*    display ' sql-pb-AhBenefit        ' sql-pb-AhBenefit
001128*    display ' sql-pb-AhPremium        ' sql-pb-AhPremium
001129*    display ' sql-pb-ErrorMessage     ' sql-pb-ErrorMessage
001130
001131     .
001132 0070-exit.
001133     exit.
001134
001135 0080-get-batch-no.
001136
001137     perform 0090-init-erpndb    thru 0090-exit
001138     perform 0240-init-erpndm    thru 0240-exit
001139     move ws-comp-id             to elcntl-key
001140     move '1'                    to elcntl-rec-type
001141     move +0                     to elcntl-seq
001142     
      * exec cics read update
001143*       dataset  ('ELCNTL')
001144*       into     (control-file)
001145*       ridfld   (elcntl-key)
001146*       resp     (ws-response)
001147*    end-exec
           MOVE LENGTH OF
            control-file
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00003905' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303033393035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 control-file, 
                 DFHEIV11, 
                 elcntl-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001148
001149     if resp-normal
001150        add +1                   to cf-last-batch-no
001151        move cf-last-batch-no    to ws-batch-no-n
001152        move +0                  to ws-batch-seq-no
001153        
      * exec cics rewrite
001154*          dataset  ('ELCNTL')
001155*          from     (control-file)
001156*          resp     (ws-response)
001157*       end-exec
           MOVE LENGTH OF
            control-file
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&& L                  %  N#00003916' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303033393136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 control-file, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001158        if not resp-normal
001159           display ' bad rewrite on cntl - batch no '
001160              ws-batch-no-n
001161           move ' Error rewrite ELCNTL '
001162                                 to output-msg
001163           set rollback-needed   to true
001164           move ws-response         to ws-display-response
001165           move spaces              to ws-error-message
001166           string ' Bad rewrite  ELCNTL FILE '
001167              ws-display-response
001168              delimited by size into ws-error-message
001169           end-string
001170           move 3                to ws-status-code
001171           perform 0226-update-remit-table
001172                                 thru 0226-exit
001173           perform 0225-commit-work
001174                                 thru 0225-exit
001175           go to 0000-return
001176        end-if
001177     else
001178        display ' bad read upde on cntl - batch no '
001179              elcntl-key (1:4)
001180     end-if
001181
001182     .
001183 0080-exit.
001184     exit.
001185
001186 0090-init-erpndb.
001187
001188     move 'PB'                   to pending-business
001189     INITIALIZE PB-issue-RECORD
001190     initialize pb-record-status
001191
001192     move low-values             to pb-i-lf-expire-dt
001193                                    pb-i-ah-expire-dt
001194                                    pb-i-1st-pmt-dt
001195                                    pb-i-last-add-on-dt
001196                                    pb-i-birthday
001197                                    pb-i-joint-birthday
001198                                    pb-credit-select-dt
001199                                    pb-credit-accept-dt
001200                                    pb-billed-dt
001201                                    pb-input-dt
001202                                    pb-acct-eff-dt
001203                                    pb-acct-exp-dt
001204                                    pb-confirmation-rept-dt
001205                                    pb-ga-bill-dt (1)
001206                                    pb-ga-bill-dt (2)
001207                                    pb-ga-bill-dt (3)
001208                                    pb-ga-bill-dt (4)
001209                                    pb-ga-bill-dt (5)
001210     move ws-comp-id             to pb-company-id
001211     move ws-comp-cd             to pb-company-cd
001212                                    pb-company-cd-a1
001213                                    pb-original-company-cd
001214                                    pb-csr-company-cd
001215     move zeros                  to pb-chg-count
001216                                    pb-batch-chg-seq-no
001217                                    pb-alt-chg-seq-no
001218                                    pb-original-chg-seq-no
001219                                    pb-csr-batch-chg-seq-no
001220                                    pb-i-rate-deviation-lf
001221                                    pb-i-rate-deviation-ah
001222                                    pb-no-of-errors
001223                                    pb-lf-billed-amts
001224                                    pb-ah-billed-amts
001225     move function upper-case(sql-py-carrier)
001226                                 to pb-carrier
001227                                    pb-sv-carrier
001228     move function upper-case(sql-py-group)
001229                                 to pb-grouping
001230                                    pb-sv-grouping
001231     move function upper-case(sql-py-state)
001232                                 to pb-state
001233                                    pb-sv-state
001234     move function upper-case(sql-py-accountnumber)
001235                                 to pb-account
001236     move '1'                    to pb-record-type
001237     move 'E202'                 to pb-last-maint-by
001238                                    pb-input-by
001239     move ws-bin-current-dt      to pb-last-maint-dt
001240                                    pb-input-dt
001241     move ws-work-time           to pb-last-maint-hhmmss
001242     perform varying s1 from +1 by +1 until s1 > +10
001243        move +0                  to pb-common-error (s1)
001244     end-perform
001245     move pending-business       to ws-init-erpndb-rec
001246
001247     .
001248 0090-exit.
001249     exit.
001250
001251 0100-BUILD-BATCH-HDR.
001252
001253     display ' about to build batch hdr '
001254     move ws-init-erpndb-rec     to pending-business
001255     move ws-batch-no            to pb-entry-batch
001256                                    pb-original-entry-batch
001257                                    pb-csr-entry-batch
001258                                    pb-cert-no
001259     add +1 to ws-batch-seq-no
001260     move 9999                   to pb-batch-seq-no
001261                                    pb-original-seq-no
001262                                    pb-csr-batch-seq-no
001263     move high-values            to pb-cert-eff-dt
001264
001265
001266     move zeros                  to PB-B-LF-ISS-PRM-REMITTED
001267                                    PB-B-LF-ISS-PRM-ENTERED
001268                                    PB-B-LF-ISS-PRM-COMPUTED
001269                                    PB-B-LF-CAN-PRM-REMITTED
001270                                    PB-B-LF-CAN-PRM-ENTERED
001271                                    PB-B-LF-CAN-PRM-COMPUTED
001272                                    PB-B-AH-ISS-PRM-REMITTED
001273                                    PB-B-AH-ISS-PRM-ENTERED
001274                                    PB-B-AH-ISS-PRM-COMPUTED
001275                                    PB-B-AH-CAN-PRM-REMITTED
001276                                    PB-B-AH-CAN-PRM-ENTERED
001277                                    PB-B-AH-CAN-PRM-COMPUTED
001278                                    PB-B-ISSUE-CNT-REMITTED
001279                                    PB-B-ISSUE-CNT-ENTERED
001280                                    PB-B-CANCEL-CNT-REMITTED
001281                                    PB-B-CANCEL-CNT-ENTERED
001282                                    PB-B-HIGHEST-SEQ-NO
001283                                    PB-LF-BILLED-AMTS
001284                                    PB-AH-BILLED-AMTS
001285                                    PB-CHG-COUNT
001286                                    PB-CALC-TOLERANCE
001287     move spaces                 to pb-account-name
001288                                    pb-prem-ref-rpt-flag
001289                                    pb-reference
001290
001291     MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT
001292                                    PB-BILLED-DT
001293                                    PB-ACCT-EFF-DT
001294                                    PB-ACCT-EXP-DT
001295
001296     MOVE '9'                    TO PB-RECORD-TYPE
001297     move ws-iss-cnt             to pb-b-issue-cnt-remitted
001298     move ws-lf-iss-premium      to pb-b-lf-iss-prm-remitted
001299     move ws-ah-iss-premium      to pb-b-ah-iss-prm-remitted
001300     move ws-highest-seq-no      to pb-b-highest-seq-no
001301     move ws-bin-current-dt      to pb-b-received-dt
001302     move ws-bin-eom-dt          to pb-credit-select-dt
001303     move function upper-case(sql-py-Name)
001304                                 to PB-ACCOUNT-NAME
001305
001306     .
001307 0100-EXIT.
001308     EXIT.
001309
001310 0110-write-erpndb.
001311
001312     display ' about to write pend bus ' pb-record-type
001313
001314     
      * EXEC CICS WRITE
001315*        DATASET  ('ERPNDB')
001316*        FROM     (PENDING-BUSINESS)
001317*        RIDFLD   (PB-CONTROL-PRIMARY)
001318*        RESP     (WS-RESPONSE)
001319*    END-EXEC
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
           MOVE 'ERPNDB' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00004077' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303034303737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 PB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001320
001321     IF NOT RESP-NORMAL
001322        DISPLAY ' ERPNDB BTCH WRITE ERROR ' WS-RESPONSE
001323        move ' Error writing erpndb  '
001324                           to output-msg
001325        set rollback-needed to true
001326        move ws-response         to ws-display-response
001327        move spaces              to ws-error-message
001328        string ' Bad write on ERPNDB FILE ' ws-display-response
001329           delimited by size into ws-error-message
001330        end-string
001331        move 3             to ws-status-code
001332        perform 0226-update-remit-table
001333                           thru 0226-exit
001334        perform 0225-commit-work
001335                           thru 0225-exit
001336        go to 0000-return
001337     END-IF
001338
001339     .
001340 0110-exit.
001341     exit.
001342
001343 0120-write-erpndm.
001344
001345     display ' about to write pend mail '
001346
001347     move ws-init-erpndm         to pending-mailing-data
001348
001349     move pb-control-primary     to pm-control-primary
001350     move function upper-case(sql-py-Name)
001351                                 to PM-CRED-BENE-NAME
001352     move function upper-case(sql-py-Addr1)
001353                                 to PM-CRED-BENE-ADDR
001354     move function upper-case(sql-py-Addr2)
001355                                 to PM-CRED-BENE-ADDR2
001356     move function upper-case(sql-py-AddrCity)
001357                                 to PM-CRED-BENE-CITY
001358     move function upper-case(sql-py-AddrState)
001359                                 to pm-cred-bene-state
001360     move sql-py-AddrZIP         to pm-cred-bene-zip
001361
001362     
      * EXEC CICS WRITE
001363*        DATASET  ('ERPNDM')
001364*        FROM     (PENDING-MAILING-DATA)
001365*        RIDFLD   (PM-CONTROL-PRIMARY)
001366*        RESP     (WS-RESPONSE)
001367*    END-EXEC
           MOVE LENGTH OF
            PENDING-MAILING-DATA
             TO DFHEIV11
           MOVE 'ERPNDM' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00004125' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303034313235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PENDING-MAILING-DATA, 
                 DFHEIV11, 
                 PM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001368
001369     IF NOT RESP-NORMAL
001370        move ' Error writing erpndm  '
001371                                 to output-msg
001372        set rollback-needed to true
001373        move ws-response         to ws-display-response
001374        move spaces              to ws-error-message
001375        string ' Bad write on ERPNDM FILE ' ws-display-response
001376           delimited by size into ws-error-message
001377        end-string
001378        move 3                   to ws-status-code
001379        perform 0226-update-remit-table
001380                                 thru 0226-exit
001381        perform 0225-commit-work
001382                                 thru 0225-exit
001383        go to 0000-return
001384        DISPLAY ' ERPNDM WRITE ERROR ' WS-RESPONSE
001385     END-IF
001386
001387     .
001388 0120-exit.
001389     exit.
001390
001391 0130-start-edit.
001392
001393     display ' about to start edit '
001394
001395     MOVE ws-comp-cd             TO EDIT-COMPANY-CD
001396     MOVE WS-BATCH-NO            TO EDIT-BATCH
001397     MOVE ws-comp-id             TO EDIT-COMPANY-ID
001398     MOVE SPACES                 TO EDIT-RESTART-BATCH
001399
001400     
      * EXEC CICS START
001401*         TRANSID       ('EXEB')
001402*         FROM          (BATCH-TO-PROCESS)
001403*    END-EXEC
           MOVE LENGTH OF
            BATCH-TO-PROCESS
             TO DFHEIV11
           MOVE 'EXEB' TO DFHEIV5
      *    MOVE '0( LF                 1   #00004163' TO DFHEIV0
           MOVE X'3028204C4620202020202020' &
                X'202020202020202020203120' &
                X'2020233030303034313633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV5, 
                 DFHEIV99, 
                 BATCH-TO-PROCESS, 
                 DFHEIV11, 
                 DFHEIV99, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001404
001405     
      * EXEC CICS DELAY
001406*       INTERVAL    (02)
001407*    END-EXEC
           MOVE 02 TO DFHEIV10
      *    MOVE '0$I                   &   #00004168' TO DFHEIV0
           MOVE X'302449202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303034313638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV10, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001408
001409     .
001410 0130-exit.
001411     exit.
001412
001413 0220-send-form.
001414
001415     move ws-comp-id             to out-comp-id
001416     move 'CU CONNECT '          to output-title
001417     display ' about to send form '
001418
001419     
      * exec cics document create
001420*       doctoken   (w-doctoken)
001421*       template   ('WCHECKS')
001422*       symbollist (output-data)
001423*       listlength (length of output-data)
001424*    end-exec
           MOVE 'WCHECKS' TO DFHEIV1
           MOVE LENGTH OF
            output-data TO DFHEIV16
      *    MOVE '\"D tSL               )   #00004182' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' &
                X'202020202020202020202920' &
                X'2020233030303034313832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV1, 
                 output-data, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001425
001426     
      * exec cics web send
001427*       doctoken(w-doctoken)
001428*    end-exec
      *    MOVE 'X$D                   *   #00004189' TO DFHEIV0
           MOVE X'582444202020202020202020' &
                X'202020202020202020202A20' &
                X'2020233030303034313839' TO DFHEIV0
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001429
001430     .
001431 0220-exit.
001432     exit.
001433
001434 0225-commit-work.
001435
001436     display ' about to commit work   '
001437
001438     if connected-to-db
001440        EXEC SQL
                  commit work
001441        END-EXEC
001442     end-if
001443
001444     .
001445 0225-exit.
001446     exit.
001447
001448 0226-update-remit-table.
001449
001450     if ws-batch-no = zeros
001451        move -1                  to nu-batchno
001452     else
001453        move +0                  to nu-batchno
001454     end-if
001455
001456     if ws-error-message = spaces
001457        move -1                  to nu-error-message
001458     else
001459        move +0                  to nu-error-message
001460     end-if
001461
001463     EXEC SQL
              UPDATE
001464           CUC_Logic_Remittance
001465        SET
001466           LogicStatus = :ws-status-code,
001467           LogicStatusDate = :ws-status-date,
001468           BatchNumber = :ws-batch-no :nu-batchno,
001469           ErrorMessage = :ws-error-message :nu-error-message
001470        WHERE
001471           RemitId = :ws-remit-id
001472     END-EXEC
001473
001474     .
001475 0226-exit.
001476     exit.
001477
001478 0230-disconnect.
001479
001480     display ' about to disconnect DB '
001481
001482     if connected-to-db
001484        EXEC SQL
                  disconnect all
001485        END-EXEC
001486        move ' ' to ws-connect-sw
001487     end-if
001488
001489     .
001490 0230-exit.
001491     exit.
001492
001493 0240-init-erpndm.
001494
001495     display ' made it to init pndm '
001496
001497     move spaces                 to pending-mailing-data
001498     move 'PM'                   to pm-record-id
001499     move zeros                  to pm-batch-seq-no
001500                                    pm-batch-chg-seq-no
001501                                    pm-last-maint-hhmmss
001502                                    pm-insured-issue-age
001503                                    pm-phone-no
001504     move low-values             to pm-record-add-dt
001505                                    pm-last-maint-dt
001506                                    pm-insured-birth-dt
001507                                    pm-joint-birth-dt
001508
001509     perform varying s1 from +1 by +1 until s1 > +7
001510        move low-values          to pm-mail-date (s1)
001511     end-perform
001512
001513     move 'CR'                   to pm-source-system
001514     move ws-bin-current-dt      to pm-record-add-dt
001515                                    pm-last-maint-dt
001516     move 'E202'                 to pm-last-maint-by
001517                                    pm-record-added-by
001518     move ws-work-time           to pm-last-maint-hhmmss
001519     move pending-mailing-data   to ws-init-erpndm
001520
001521     .
001522 0240-exit.
001523     exit.
001524
001525 9700-DATE-CONVERT.
001526
001527     
      * EXEC CICS LINK
001528*         PROGRAM  ('ELDATCV')
001529*         COMMAREA (DATE-CONVERSION-DATA)
001530*         LENGTH   (DC-COMM-LENGTH)
001531*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00004290' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034323930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001532
001533 9700-EXIT.
001534      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL202' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL202' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
