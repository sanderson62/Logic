      *((program: SOCK16.cl2))
000001*$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
000002 IDENTIFICATION DIVISION.
000003 PROGRAM-ID. SOCK16.
000004 AUTHOR. Paul.
000005 DATE-COMPILED.
000006*SECURITY.   *****************************************************
000007*            *                                                   *
000008*            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
000009*            *                                                   *
000010*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000011*            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
000012*            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
000013*            *                                                   *
000014*            *****************************************************
000015
000016******************************************************************
000017*REMARKS.                                                        *
000018*       Receives a call from the GA withholding app and          *
000019*   will update the ercomp table with the latest values based on *
000020*   the key(s) passed to this program.                           *
000021*                                                                *
000022******************************************************************
000023*                   C H A N G E   L O G
000024*
000025* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000026*-----------------------------------------------------------------
000027*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000028* EFFECTIVE    NUMBER
000029*-----------------------------------------------------------------
000030* 020119 CR2020060800001   PEMA  New Program
000031******************************************************************
000032 ENVIRONMENT DIVISION.
000033
000034 DATA DIVISION.
000035
000036 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000037*
000038* program buffers
000039*
000040 77 ws-send-msg-size             pic s9(8) comp value 4096.
000041 77 ws-recv-msg-size             pic s9(8) comp value 4096.
000042 77 ws-recv-buf                  pic x(4096).
000043 77 ws-send-buf                  pic x(4096) VALUE SPACES.
000044 77 ws-recv-total                pic s9(8) comp value 0.
000045 77 ws-recv-left                 pic s9(8) comp value 0.
000046 77 ws-seq-num                   pic s9(8) comp value 0.
000047 77 ws-flags                     pic s9(8) comp value 0.
000048 77 WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
000049 77  ws-comp-id                  pic xxx value spaces.
000050 77 WS-SAVE-ACCOUNT              PIC X(10)  VALUE SPACES.
000051 77 WS-BIN-ORIG-EFF-DT           PIC XX  VALUE LOW-VALUES.
000052 77 WS-ORIG-EFF-DT               PIC X(10)  VALUE SPACES.
000053 77 WS-EFF-DATE                  PIC X(10)  VALUE SPACES.
000054 77 WS-EXP-DATE                  PIC X(10)  VALUE SPACES.
000055 77 S1                           PIC S999 COMP-3 VALUE +0.
000056 77 S2                           PIC S999 COMP-3 VALUE +0.
000057 77 WS-BUILD-SW                  PIC X.
000058    88  TIME-TO-BUILD               VALUE 'Y'.
000059 77 WS-SAVE-ERACCT               PIC X(2000).
000060 77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
000061 77 WS-PERFORM-SW                PIC X VALUE SPACES.
000062    88  GET-RATES                    VALUE 'R'.
000063    88  GET-ACT-ACCTS                VALUE 'A'.
000064 77  ws-bin-current-dt           pic xx value low-values.
000065 77 ws-bin-eff-dt                pic xx  value low-values.
000066 77 ws-bin-1st-pay-dt            pic xx  value low-values.
000067 77 WS-DISP-AMT                  PIC Z,ZZZ,Z99.99.
000068 77 ws-disp-rate                 pic z9.99999.
000069 77  WS-ERACCT-SW                PIC X VALUE ' '.
000070     88  END-OF-ERACCT                 VALUE 'Y'.
000071 77  WS-ERCTBL-SW                PIC X VALUE ' '.
000072     88  END-OF-ERCTBL                 VALUE 'Y'.
000073 77  WS-STATUS                   PIC X.
000074 77  ws-connect-sw               pic x value ' '.
000075     88  connected-to-db           value 'Y'.
000076 77  ws-socket-sw                pic x value ' '.
000077     88  end-of-socket              value 'Y'.
000078
000079 01  P pointer.
000080 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000081 01  var-ptr pointer.
000082 01  env-var-len                 pic 9(4)  binary.
000083 01  rc                          pic 9(9)  binary.
000084
000085 01  WS-KIXSYS.
000086     05  WS-KIX-FIL1             PIC X(10).
000087     05  WS-KIX-APPS             PIC X(10).
000088     05  WS-KIX-ENV              PIC X(10).
000089     05  WS-KIX-MYENV            PIC X(10).
000090     05  WS-KIX-SYS              PIC X(10).
000091
000092 01  ws-return-string.
000093     05  ws-return-error-no      pic x(4).
000094     05  ws-sc1                  pic x.
000095     05  ws-return-error-mess    pic x(45).
000096     05  ws-sc2                  pic x.
000097     05  ws-return-accts-read    pic zzzzzz9.
000098     05  ws-sc3                  pic x.
000099     05  ws-return-acnts-read    pic zzzzzz9.
000100     05  ws-sc4                  pic x.
000101     05  ws-return-acnts-updated pic zzzzzz9.
000102     05  ws-sc5                  pic x.
000103     05  ws-return-acnts-deletes pic zzzzzz9.
000104
000105*EXEC SQL
000106*   INCLUDE SQLDA
000107*END-EXEC
000108
000111*EXEC SQL
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
000112
000114 EXEC SQL
          BEGIN DECLARE SECTION
000115 END-EXEC
000116
000117 01  sqlcmd                      pic x(1024).
000118 01  svr                         pic x(32).
000119 01  usr                         pic x(32).
000120 01  pass                        pic x(32).
000121 01  usr-pass                    pic x(64).
000122 01  ws-disp-code                pic s9(11).
000123 01  ws-display-response         pic s9(9) value zeros.
000124
000125 01  indicator-vaiables-for-nulls.
000126     05  nu-last-maint-dt        pic s9(4) comp value +0.
000127     05  nu-rolodex-print-dt     pic s9(4) comp value +0.
000128     05  nu-last-eom-stmt-dt     pic s9(4) comp value +0.
000129     05  nu-last-activity-date   pic s9(4) comp value +0.
000130     05  nu-last-stmt-dt         pic s9(4) comp value +0.
000131     05  nu-current-last-stmt-dt pic s9(4) comp value +0.
000132     05  nu-ga-effective-dt      pic s9(4) comp value +0.
000133     05  nu-ga-termination-dt    pic s9(4) comp value +0.
000134     05  nu-first-written-dt     pic s9(4) comp value +0.
000135
000136 01  comp-record.
000137     12  cr-carrier                            pic x.
000138     12  cr-grouping                           pic x(6).
000139     12  cr-resp-no                            pic x(10).
000140     12  cr-account                            pic x(10).
000141     12  cr-type                               pic x.
000142     12  CR-LAST-MAINT-DT                      PIC x(10).
000143     12  CR-LAST-MAINT-HHMMSS                  PIC x(8).
000144     12  cr-last-maint-hhmmss-n redefines
000145         CR-LAST-MAINT-HHMMSS                  PIC -9(7).
000146     12  CR-LAST-MAINT-USER                    PIC X(4).
000147     12  CR-STMT-TYPE                          PIC XXX.
000148     12  CR-COMP-TYPE                          PIC X.
000149     12  CR-STMT-OWNER                         PIC X(4).
000150     12  CR-BALANCE-CONTROL                    PIC X.
000151     12  CR-INTERNAL-CONTROL-1                 PIC X.
000152     12  CR-INTERNAL-CONTROL-2                 PIC X.
000153     12  cr-ga-withold-pct                     pic x(7).
000154     12  CR-GA-WITHOLD-PCT-n redefines
000155         cr-ga-withold-pct                     PIC -9.9999.
000156     12  CR-GA-DIRECT-DEP                      PIC X.
000157     12  CR-ACCT-NAME                          PIC X(30).
000158     12  CR-MAIL-NAME                          PIC X(30).
000159     12  CR-ADDR-1                             PIC X(30).
000160     12  CR-ADDR-2                             PIC X(30).
000161     12  CR-ADDR-3                             pic x(29).
000162     12  CR-CSO-1099                           PIC X.
000163     12  CR-ZIP                                pic x(9).
000164     12  CR-SOC-SEC                            PIC X(13).
000165     12  CR-TELEPHONE                          pic x(10).
000166     12  CR-ROLODEX-PRINT-DT                   PIC X(10).
000167     12  CR-AR-BAL-LEVEL                       PIC X.
000168     12  CR-AR-NORMAL-PRINT                    PIC X.
000169     12  CR-AR-SUMMARY-CODE                    PIC X(6).
000170     12  CR-AR-REPORTING                       PIC X.
000171     12  CR-AR-PULL-CHECK                      PIC X.
000172     12  CR-AR-BALANCE-PRINT                   PIC X.
000173     12  CR-AR-LAST-RUN-CODE                   PIC X.
000174     12  CR-LAST-EOM-STMT-DT                   PIC X(10).
000175     12  CR-USER-CODE                          PIC X.
000176     12  CR-REPORT-GROUP-ID                    PIC X(12).
000177     12  CR-LAST-ACTIVITY-DATE                 pic x(10).
000178     12  CR-LAST-STMT-DT                       pic x(10).
000179     12  CR-BAL-FWD                            PIC -9(7).99.
000180     12  CR-CUR-COM                            PIC -9(7).99.
000181     12  CR-CUR-CHG                            PIC -9(7).99.
000182     12  CR-CUR-PMT                            PIC -9(7).99.
000183     12  CR-END-BAL                            PIC -9(7).99.
000184     12  CR-CUR                                PIC -9(7).99.
000185     12  CR-OV30                               PIC -9(7).99.
000186     12  CR-OV60                               PIC -9(7).99.
000187     12  CR-OV90                               PIC -9(7).99.
000188     12  CR-YTD-COM                            PIC -9(7).99.
000189     12  CR-YTD-OV                             PIC -9(7).99.
000190     12  CR-CUR-OVR-UNDR                       PIC -9(7).99.
000191     12  CR-YTD-OVR-UNDR                       PIC -9(7).99.
000192     12  CR-CUR-FICA                           PIC -9(7).99.
000193     12  CR-YTD-FICA                           PIC -9(7).99.
000194     12  CR-LF-CLM-AMT                         PIC -9(9).99.
000195     12  CR-AH-CLM-AMT                         PIC -9(9).99.
000196     12  CR-CURRENT-LAST-STMT-DT               pic x(10).
000197     12  CR-CURRENT-BAL-FWD                    PIC -9(7).99.
000198     12  CR-CURRENT-CUR-COM                    PIC -9(7).99.
000199     12  CR-CURRENT-CUR-CHG                    PIC -9(7).99.
000200     12  CR-CURRENT-CUR-PMT                    PIC -9(7).99.
000201     12  CR-CURRENT-END-BAL                    PIC -9(7).99.
000202     12  CR-CURRENT-CUR                        PIC -9(7).99.
000203     12  CR-CURRENT-OV30                       PIC -9(7).99.
000204     12  CR-CURRENT-OV60                       PIC -9(7).99.
000205     12  CR-CURRENT-OV90                       PIC -9(7).99.
000206     12  CR-CURRENT-YTD-COM                    PIC -9(7).99.
000207     12  CR-CURRENT-YTD-OV                     PIC -9(7).99.
000208     12  CR-YTD-PAID-COM                       PIC -9(7).99.
000209     12  CR-YTD-PAID-OV                        PIC -9(7).99.
000210     12  CR-CURRENT-MONTH-ACTIVITY             PIC X.
000211     12  CR-DELINQUENT-LETTER-CODE             PIC X.
000212     12  CR-CSR-CODE                           PIC X(4).
000213     12  CR-GA-EFFECTIVE-DT                    PIC X(10).
000214     12  CR-GA-TERMINATION-DT                  PIC X(10).
000215     12  CR-GA-STATUS-CODE                     PIC X.
000216     12  CR-GA-COMMENT-1                       PIC X(40).
000217     12  CR-GA-COMMENT-2                       PIC X(40).
000218     12  CR-GA-COMMENT-3                       PIC X(40).
000219     12  CR-GA-COMMENT-4                       PIC X(40).
000220     12  CR-RPTCD2                             PIC X(10).
000221     12  CR-OV120                              PIC -9(7).99.
000222     12  CR-CURRENT-OV120                      PIC -9(7).99.
000223     12  CR-TYPE-AGENT                         PIC X(01).
000224     12  CR-FAXNO                              pic x(10).
000225     12  CR-MD-GL-ACCT                         PIC X(10).
000226     12  CR-MD-DIV                             PIC XX.
000227     12  CR-MD-CENTER                          PIC X(4).
000228     12  cr-md-amt                             pic x(9).
000229     12  CR-MD-AMT-n redefines
000230         cr-md-amt                             PIC -9(5).99.
000231     12  CR-CREATE-AP-CHECK                    PIC X.
000232     12  CR-DELIVER-CK-TO-MEL                  PIC X.
000233     12  CR-ACH-STATUS                         PIC X.
000234     12  CR-BILL-SW                            PIC X.
000235     12  CR-CONTROL-NAME                       PIC X(30).
000236     12  CR-MAX-BANK-FEE-LEASE                 PIC -9(5).99.
000237     12  CR-MAX-BANK-FEE                       PIC -9(5).99.
000238     12  CR-CLP-STATE                          PIC XX.
000239     12  CR-FIRST-WRITTEN-DT                   PIC X(10).
000240     12  CR-SPP-REFUND-EDIT                    PIC X.
000241
000243 EXEC SQL
          END DECLARE SECTION
000244 END-EXEC
000245
000246 01  kt-sub                      pic s9(5) comp-3 value +0.
000247 01  kt-max                      pic s9(5) comp-3 value +0.
000248 01  WS-KEY-TABLE.
000249     05  key-table occurs 10.
000250         10  kt-carrier          pic x.
000251         10  kt-group            pic x(6).
000252         10  kt-fin-resp         pic x(10).
000253         10  kt-account          pic x(10).
000254         10  kt-type             pic x.
000255         10  kt-delim            pic x.
000256 01  ws-work-date.
000257     05  ws-work-ccyy            pic x(4).
000258     05  ws-work-mm              pic xx.
000259     05  ws-work-dd              pic xx.
000260 01  ws-work-date-num redefines ws-work-date
000261                                 pic 9(8).
000262
000263 01  ws-work-time                pic 9(7).
000264 01  filler redefines ws-work-time.
000265     05  filler                  pic x.
000266     05  ws-hh                   pic 99.
000267     05  ws-mm                   pic 99.
000268     05  ws-ss                   pic 99.
000269
000270 01  WS-FIN-RESP                 PIC X(10).
000271 01  WS-CO-DATA.
000272     05  WS-CO-NUM               PIC X(10).
000273     05  WS-CO-PRIMARY-CONTACT   PIC X(30).
000274     05  WS-CO-NAME              PIC X(30).
000275     05  WS-CO-MAIL-NAME         PIC X(30).
000276     05  WS-CO-ADDR1             PIC X(30).
000277     05  WS-CO-ADDR2             PIC X(30).
000278     05  WS-CO-ADDR3             PIC X(30).
000279     05  WS-CO-ZIP               PIC X(9).
000280     05  WS-CO-PHONE             PIC X(10).
000281
000282
000283 01  WS-CO-KEY.
000284     05  WS-CO-COMPANY-CD        PIC X.
000285     05  WS-CO-CARRIER           PIC X.
000286     05  WS-CO-GROUP             PIC X(6).
000287     05  WS-CO-FIN-RESP          PIC X(10).
000288     05  WS-CO-ACCOUNT           PIC X(10).
000289     05  WS-CO-TYPE              PIC X.
000290
000291 01  WS-CID-NO                   PIC X(8).
000292
000293 01  WS-DISP-RESP                PIC 9(5).
000294 01  WS-RESPONSE                 PIC S9(8)   COMP.
000295     88  RESP-NORMAL                  VALUE +00.
000296     88  RESP-NOTFND                  VALUE +13.
000297     88  RESP-NOTOPEN                 VALUE +19.
000298     88  RESP-ENDFILE                 VALUE +20.
000299
000300*                                 COPY ERCCOMP.
      *>>((file: ERCCOMP))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCCOMP                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.019                          *
000007*                                                                *
000008*   ONLINE CREDIT SYSTEM                                         *
000009*                                                                *
000010*   FILE DESCRIPTION = COMPENSATION MASTER                       *
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 700   RECFORM = FIXED                          *
000014*                                                                *
000015*   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
000016*       ALTERNATE PATH = NONE                                    *
000017*                                                                *
000018*   LOG = NO                                                     *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020*                                                                *
000021******************************************************************
000022*                   C H A N G E   L O G
000023*
000024* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000025*-----------------------------------------------------------------
000026*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000027* EFFECTIVE    NUMBER
000028*-----------------------------------------------------------------
000029* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000030* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
000031* 092205    2005050300006  PEMA  ADD LEASE FEE
000032* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
000033* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
000034* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
000035* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000036* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
000037* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
000038******************************************************************
000039
000040 01  COMPENSATION-MASTER.
000041     12  CO-RECORD-ID                          PIC XX.
000042         88  VALID-CO-ID                          VALUE 'CO'.
000043
000044     12  CO-CONTROL-PRIMARY.
000045         16  CO-COMPANY-CD                     PIC X.
000046         16  CO-CONTROL.
000047             20  CO-CTL-1.
000048                 24  CO-CARR-GROUP.
000049                     28  CO-CARRIER            PIC X.
000050                     28  CO-GROUPING.
000051                         32  CO-GROUP-PREFIX   PIC XXX.
000052                         32  CO-GROUP-PRIME    PIC XXX.
000053                 24  CO-RESP-NO.
000054                     28  CO-RESP-PREFIX        PIC X(4).
000055                     28  CO-RESP-PRIME         PIC X(6).
000056             20  CO-CTL-2.
000057                 24  CO-ACCOUNT.
000058                     28  CO-ACCT-PREFIX        PIC X(4).
000059                     28  CO-ACCT-PRIME         PIC X(6).
000060         16  CO-TYPE                           PIC X.
000061             88  CO-COMPANY-TYPE                  VALUE 'C'.
000062             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
000063             88  CO-ACCOUNT-TYPE                  VALUE 'A'.
000064
000065     12  CO-MAINT-INFORMATION.
000066         16  CO-LAST-MAINT-DT                  PIC XX.
000067         16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
000068         16  CO-LAST-MAINT-USER                PIC X(4).
000069     12  FILLER                                PIC XX.
000070     12  CO-STMT-TYPE                          PIC XXX.
000071     12  CO-COMP-TYPE                          PIC X.
000072         88  CO-COMP-IS-SPPDD                    VALUE '1'.
000073     12  CO-STMT-OWNER                         PIC X(4).
000074     12  CO-BALANCE-CONTROL                    PIC X.
000075         88  CO-CARRY-BALANCE                     VALUE 'Y'.
000076         88  CO-NO-BALANCE                        VALUE 'N'.
000077
000078     12  CO-INTERNAL-CONTROL-1                 PIC X.
000079         88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
000080         88  CO-AUTO-GENERATED                    VALUE 'Y'.
000081         88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
000082
000083     12  CO-INTERNAL-CONTROL-2                 PIC X.
000084         88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
000085         88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
000086
000087     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
000088     12  CO-GA-DIRECT-DEP                      PIC X.
000089     12  CO-FUTURE-SPACE                       PIC X.
000090         88  CO-FUTURE-NOT-USED                   VALUE ' '.
000091
000092     12  CO-ACCT-NAME                          PIC X(30).
000093     12  CO-MAIL-NAME                          PIC X(30).
000094     12  CO-ADDR-1                             PIC X(30).
000095     12  CO-ADDR-2                             PIC X(30).
000096     12  CO-ADDR-3.
000097         16  CO-ADDR-CITY                      PIC X(27).
000098         16  CO-ADDR-STATE                     PIC XX.
000099     12  CO-CSO-1099                           PIC X.
000100     12  CO-ZIP.
000101         16  CO-ZIP-PRIME.
000102             20  CO-ZIP-PRI-1ST                PIC X.
000103                 88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
000104             20  FILLER                        PIC X(4).
000105         16  CO-ZIP-PLUS4                      PIC X(4).
000106     12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
000107         16  CO-CAN-POSTAL-1                   PIC XXX.
000108         16  CO-CAN-POSTAL-2                   PIC XXX.
000109         16  FILLER                            PIC XXX.
000110     12  CO-SOC-SEC                            PIC X(13).
000111     12  CO-TELEPHONE.
000112         16  CO-AREA-CODE                      PIC XXX.
000113         16  CO-PREFIX                         PIC XXX.
000114         16  CO-PHONE                          PIC X(4).
000115
000116     12  CO-ROLADEX-PRINT-DT                   PIC XX.
000117
000118     12  CO-AR-BAL-LEVEL                       PIC X.
000119         88  CO-AR-REF-LVL                        VALUE '1'.
000120         88  CO-AR-BILL-REF-LVL                   VALUE '1'.
000121         88  CO-AR-BILL-LVL                       VALUE '2'.
000122         88  CO-AR-AGT-LVL                        VALUE '3'.
000123         88  CO-AR-FR-LVL                         VALUE '4'.
000124
000125     12  CO-AR-NORMAL-PRINT                    PIC X.
000126         88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
000127         88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
000128
000129     12  CO-AR-SUMMARY-CODE                    PIC X(6).
000130
000131     12  CO-AR-REPORTING                       PIC X.
000132         88  CO-AR-NET-REPORT                     VALUE 'N'.
000133         88  CO-AR-GROSS-REPORT                   VALUE 'G'.
000134
000135     12  CO-AR-PULL-CHECK                      PIC X.
000136         88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
000137         88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
000138
000139     12  CO-AR-BALANCE-PRINT                   PIC X.
000140         88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
000141
000142     12  CO-AR-LAST-RUN-CODE                   PIC X.
000143         88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
000144         88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
000145         88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
000146
000147     12  CO-LAST-EOM-STMT-DT                   PIC XX.
000148
000149     12  CO-USER-CODE                          PIC X.
000150     12  CO-REPORT-GROUP-ID                    PIC X(12).
000151
000152******************************************************************
000153*    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
000154*    THE LAST MONTH END RUN.
000155******************************************************************
000156
000157     12  CO-LAST-ACTIVITY-DATE.
000158         16  CO-ACT-YEAR                       PIC 99.
000159         16  CO-ACT-MONTH                      PIC 99.
000160         16  CO-ACT-DAY                        PIC 99.
000161
000162     12  CO-LAST-STMT-DT.
000163         16  CO-LAST-STMT-YEAR                 PIC 99.
000164         16  CO-LAST-STMT-MONTH                PIC 99.
000165         16  CO-LAST-STMT-DAY                  PIC 99.
000166
000167     12  CO-MO-END-TOTALS.
000168         16  CO-MONTHLY-TOTALS.
000169             20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
000170             20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
000171             20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
000172             20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
000173             20  CO-END-BAL                PIC S9(7)V99   COMP-3.
000174
000175         16  CO-AGING-TOTALS.
000176             20  CO-CUR                    PIC S9(7)V99   COMP-3.
000177             20  CO-OV30                   PIC S9(7)V99   COMP-3.
000178             20  CO-OV60                   PIC S9(7)V99   COMP-3.
000179             20  CO-OV90                   PIC S9(7)V99   COMP-3.
000180
000181         16  CO-YTD-TOTALS.
000182             20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
000183             20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
000184
000185         16  CO-OVER-UNDER-TOTALS.
000186             20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
000187             20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
000188
000189     12  CO-MISCELLANEOUS-TOTALS.
000190         16  CO-FICA-TOTALS.
000191             20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
000192             20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
000193
000194         16  CO-CLAIM-TOTALS.
000195             20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
000196             20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
000197
000198******************************************************************
000199*    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
000200*    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
000201******************************************************************
000202
000203     12  CO-CURRENT-TOTALS.
000204         16  CO-CURRENT-LAST-STMT-DT.
000205             20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
000206             20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
000207             20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
000208
000209         16  CO-CURRENT-MONTHLY-TOTALS.
000210             20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
000211             20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
000212             20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
000213             20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
000214             20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
000215
000216         16  CO-CURRENT-AGING-TOTALS.
000217             20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
000218             20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
000219             20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
000220             20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
000221
000222         16  CO-CURRENT-YTD-TOTALS.
000223             20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
000224             20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
000225
000226     12  CO-PAID-COMM-TOTALS.
000227         16  CO-YTD-PAID-COMMS.
000228             20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
000229             20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
000230
000231     12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
000232         88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
000233         88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
000234
000235     12  CO-DELINQUENT-LETTER-CODE         PIC X.
000236         88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
000237         88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
000238         88  CO-AGENT-1ST-LETTER              VALUE 'B'.
000239         88  CO-AGENT-2ND-LETTER              VALUE 'G'.
000240         88  CO-OVERWRITE-LETTER              VALUE 'O'.
000241         88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
000242         88  CO-FINAL-LETTER                  VALUE 'F'.
000243         88  CO-RECONCILING                   VALUE 'R'.
000244         88  CO-PHONE-CALL                    VALUE 'P'.
000245         88  CO-LEGAL                         VALUE 'L'.
000246         88  CO-COLLECTION-AGENCY             VALUE 'C'.
000247         88  CO-WRITE-OFF                     VALUE 'W'.
000248         88  CO-NO-ACTION                     VALUE 'N' ' '.
000249
000250     12  CO-CSR-CODE                       PIC X(4).
000251
000252     12  CO-GA-STATUS-INFO.
000253         16  CO-GA-EFFECTIVE-DT            PIC XX.
000254         16  CO-GA-TERMINATION-DT          PIC XX.
000255         16  CO-GA-STATUS-CODE             PIC X.
000256             88  CO-GA-ACTIVE                 VALUE 'A'.
000257             88  CO-GA-INACTIVE               VALUE 'I'.
000258             88  CO-GA-PENDING                VALUE 'P'.
000259         16  CO-GA-COMMENTS.
000260             20  CO-GA-COMMENT-1           PIC X(40).
000261             20  CO-GA-COMMENT-2           PIC X(40).
000262             20  CO-GA-COMMENT-3           PIC X(40).
000263             20  CO-GA-COMMENT-4           PIC X(40).
000264
000265     12  CO-RPTCD2                         PIC X(10).
000266     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
000267         16  CO-OV120                      PIC S9(7)V99   COMP-3.
000268         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
000269
000270     12  CO-TYPE-AGENT                     PIC X(01).
000271         88  CO-CORPORATION                   VALUE 'C'.
000272         88  CO-PARTNERSHIP                   VALUE 'P'.
000273         88  CO-SOLE-PROPRIETOR               VALUE 'S'.
000274         88  CO-TRUST                         VALUE 'T'.
000275         88  CO-UNKNOWN                       VALUE ' ' 'X'.
000276
000277     12  CO-FAXNO.
000278         16  CO-FAX-AREA-CODE                  PIC XXX.
000279         16  CO-FAX-PREFIX                     PIC XXX.
000280         16  CO-FAX-PHONE                      PIC X(4).
000281
000282     12  CO-BANK-INFORMATION.
000283         16  CO-BANK-TRANSIT-NO                PIC X(8).
000284         16  CO-BANK-TRANSIT-NON REDEFINES
000285             CO-BANK-TRANSIT-NO                PIC 9(8).
000286
000287         16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
000288     12  CO-MISC-DEDUCT-INFO REDEFINES
000289                  CO-BANK-INFORMATION.
000290         16  CO-MD-GL-ACCT                     PIC X(10).
000291         16  CO-MD-DIV                         PIC XX.
000292         16  CO-MD-CENTER                      PIC X(4).
000293         16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
000294         16  CO-CREATE-AP-CHECK                PIC X.
000295         16  CO-DELIVER-CK-TO-MEL              PIC X.
000296         16  FILLER                            PIC XXX.
000297     12  CO-ACH-STATUS                         PIC X.
000298         88  CO-ACH-ACTIVE                         VALUE 'A'.
000299         88  CO-ACH-PENDING                        VALUE 'P'.
000300
000301     12  CO-BILL-SW                            PIC X.
000302     12  CO-CONTROL-NAME                       PIC X(30).
000303     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
000304     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
000305     12  CO-CLP-STATE                          PIC XX.
000306     12  CO-FIRST-WRITTEN-DT                   PIC XX.
000307     12  CO-SPP-REFUND-EDIT                    PIC X.
000308
000309******************************************************************
      *<<((file: ERCCOMP))
000301*                                 COPY ELCFUNDT.
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
000302*                                 COPY ELCDATE.
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
000303
000304 01  sqlconnect-parms.
000305     05  p-sql-server            PIC X(30).
000306     05  p-sql-database          PIC X(30).
000307     05  p-connect-return-code   pic s9(5) comp-5.
000308     05  p-sql-return-message    pic x(256).
000309
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
000311 01  DFHCOMMAREA.
000312   05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
000313   05 LSTN-NAME                PIC X(8).
000314   05 LSTN-SUBNAME             PIC X(8).
000315   05 CLIENT-IN-DATA.
000316      15  CLIENT-KICK-OFF      PIC X(8).
000317      15  CLIENT-ID            PIC XXX.
000318      15  client-cntr          pic 999.
000319      15  FILLER               PIC X(22).
000320   05 SOCKADDR-IN-PARM.
000321     15 SIN-FAMILY             PIC 9(4) COMP.
000322     15 SIN-PORT               PIC 9(4) COMP.
000323     15 SIN-ADDRESS            PIC 9(8) COMP.
000324     15 SIN-ZERO               PIC X(8).
000325
000326 01  var  pic x(30).
000327
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'SOCK16' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000328 VCOBOL-DUMMY-PROCEDURE.
000329
000330     display 'SOCK16:transaction data = ', CLIENT-IN-DATA.
000331     display 'SOCK16:socket number    =', GIVE-TAKE-SOCKET.
000332     display ' client kick off **'client-kick-off '**'
000333     display ' client in data **' client-in-data '**'
000334
000335     perform 0000-init           thru 0000-exit
000336     perform 0010-init-contact   thru 0010-exit
000337
000338     perform 0100-process-socket thru 0100-exit until
000339        end-of-socket
000340     display ' after 0100 '
000341     move '0000;Success '        to ws-return-string
000342     PERFORM 0200-SEND-BUFFER    thru 0200-exit
000343     go to 0300-close-socket
000344
000345     .
000346 0000-init.
000347
000348     display ' made it to 0000-init '
000349     MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
000350     move ws-fn-cymd             to dc-greg-date-cymd
000351     move 'L'                    to dc-option-code
000352     perform 9700-date-link      thru 9700-exit
000353     if no-conversion-error
000354        move dc-bin-date-1       to ws-bin-current-dt
000355     else
000356        display ' error current dt invalid ' dc-error-code
000357     end-if
000358
000359     set P to address of KIXSYS
000360     CALL "getenv" using by value P returning var-ptr
000361     if var-ptr = null then
000362        display ' kixsys not set '
000363     else
000364        set address of var to var-ptr
000365        move 0 to env-var-len
000366        inspect var tallying env-var-len
000367          for characters before X'00'
000368        unstring var (1:env-var-len) delimited by '/'
000369           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000370              WS-KIX-SYS
000371        end-unstring
000372     end-if
000373     display ' client kick off **'client-kick-off '**'
000374
000375     .
000376 0000-exit.
000377     exit.
000378
000379 0010-init-contact.
000380     display ' made it to 0010-init '
000381     display ' client kick off **'client-kick-off '**'
000382
000383     if client-kick-off = 'SOCKET16'
000384        continue
000385     else
000386        move '9999;Unknown origin, who are you? '
000387                                 to ws-return-string
000388        PERFORM 0200-SEND-BUFFER thru 0200-exit
000389        display ' Unknown origin ' client-in-data
000390        go to 0300-close-socket
000391     end-if
000392
000393     evaluate client-id
000394        when 'VPP'
000395           MOVE X'07'            TO WS-COMP-CD
000396           MOVE 'VPP'            TO WS-COMP-ID
000397        when 'AHL'
000398           MOVE X'06'            TO WS-COMP-CD
000399           MOVE 'AHL'            TO WS-COMP-ID
000400        when 'DCC'
000401           MOVE X'05'            TO WS-COMP-CD
000402           MOVE 'DCC'            TO WS-COMP-ID
000403        when 'CID'
000404           MOVE X'04'            TO WS-COMP-CD
000405           MOVE 'CID'            TO WS-COMP-ID
000406        when other
000407           move '0113;Invalid company id ' to ws-return-string
000408           PERFORM 0200-SEND-BUFFER thru 0200-exit
000409           display ' Invalid company id ' client-id
000410           go to 0300-close-socket
000411     END-evaluate
000412
000413     if client-cntr numeric
000414        move client-cntr         to kt-max
000415     else
000416        move '0117;Invalid Counter ' to ws-return-string
000417        PERFORM 0200-SEND-BUFFER thru 0200-exit
000418        display ' Invalid Counter  ' client-cntr
000419        go to 0300-close-socket
000420     end-if
000421
000422     move 'SOCKET16READY'        to ws-send-buf
000423     move +25                    to ws-send-msg-size
000424
000425     call "send" using by value GIVE-TAKE-SOCKET,
000426         by reference ws-send-buf,
000427         by value ws-send-msg-size,
000428         by value ws-flags
000429
000430     if return-code <= zero
000431        display 'SOCK16:send error ' return-code
000432        perform 0250-socket-error thru 0250-exit
000433        go to 0300-close-socket
000434     end-if
000435
000436     .
000437 0010-exit.
000438     exit.
000439
000440 0100-process-socket.
000441     display ' made it to 0100-process '
000442
000443     if kt-sub = +0
000444        perform 0110-receive     thru 0110-exit
000445     end-if
000446
000447     compute kt-sub = kt-sub + +1
000448
000449     if kt-sub > kt-max
000450        set end-of-socket to true
000451     end-if
000452
000453     if end-of-socket
000454        go to 0100-exit
000455     end-if
000456
000457     move ws-comp-cd             to ws-co-key
000458     move key-table(kt-sub)(1:28) to ws-co-key (2:28)
000459
000460     perform 0400-get-ercomp     thru 0400-exit
000461
000462     .
000463 0100-exit.
000464     exit.
000465
000466 0110-receive.
000467     display ' made it to 0110-receive '
000468
000469     move spaces                 to ws-recv-buf
000470
000471     call "recv" using by value GIVE-TAKE-SOCKET
000472         by reference ws-recv-buf
000473         by value ws-recv-msg-size
000474         by value ws-flags.
000475
000476     display ' ret code ' return-code
000477     if return-code < zero
000478        display 'SOCK16:recv error ' return-code
000479        perform 0250-socket-error thru 0250-exit
000480        set end-of-socket to true
000481        go to 0110-exit
000482     end-if
000483
000484     if return-code = zero
000485        display 'SOCK16:client disconnected',
000486        perform 0250-socket-error thru 0250-exit
000487        set end-of-socket to true
000488        go to 0110-exit
000489     end-if
000490
000491     display ' recv buf ' ws-recv-buf (1:50)
000492     move ws-recv-buf (1:290)     to ws-key-table
000493
000494     if ws-recv-buf (1:4) = 'DONE' or 'done' or 'Done'
000495        set end-of-socket to true
000496     end-if
000497
000498     .
000499 0110-exit.
000500     exit.
000501
000502 0200-send-buffer.
000503
000504     move ws-return-string       to ws-send-buf
000505
000506     call "send" using by value GIVE-TAKE-SOCKET,
000507         by reference ws-send-buf,
000508         by value ws-send-msg-size,
000509         by value ws-flags.
000510
000511     if return-code <= zero
000512        display 'SOCK16:send error ',
000513        perform 0250-socket-error thru 0250-exit
000514        go to 0300-close-socket
000515     end-if
000516
000517     .
000518 0200-exit.
000519     exit.
000520
000521 0250-socket-error.
000522
000523     display "SOCK16:did not complete"
000524     display 'SOCK16:transaction data =', CLIENT-IN-DATA '**'
000525     display 'SOCK16:socket number    =', GIVE-TAKE-SOCKET.
000526     display ' return code = ' return-code
000527
000528     .
000529 0250-exit.
000530     exit.
000531
000532 0300-close-socket.
000533
000534     if connected-to-db
000536        EXEC SQL
                  commit work
000537        END-EXEC
000538        if sqlcode not = 0
000539           move ' Failed to Commit DB '
000540                                 to ws-return-string
000541           display "Error: commit release "
000542           display ' sql return code ' sqlcode
000543           display ' sql err mess    ' sqlerrmc
000544        end-if
000545     end-if
000546
000547     if connected-to-db
000549        EXEC SQL
                  disconnect all
000550        END-EXEC
000551        move ' '                 to ws-connect-sw
000552     end-if
000553     display ' about to return cics '
000554     
      * exec cics return end-exec
      *    MOVE '.(                    ''   #00001228' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303031323238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000555     
      * goback

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK16' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback
000556
000557     .
000558 0300-exit.
000559     exit.
000560
000561 0400-GET-ERCOMP.
000562     display ' made it to 0400-get  '
000563
000564*    move ws-comp-cd to ws-co-company-cd
000565*    move kt-carrier(1) to ws-co-carrier
000566*    move kt-group(1) to ws-co-group
000567*    move kt-fin-resp(1) to ws-co-fin-resp
000568*    move kt-account(1) to ws-co-account
000569*    move kt-type(1) to ws-co-type
000570
000571     display ' ws co key ' ws-co-key (2:28)
000572
000573     IF WS-CO-ACCOUNT = SPACES OR ZEROS OR LOW-VALUES
000574        MOVE LOW-VALUES          TO WS-CO-ACCOUNT
000575        MOVE 'G'                 TO WS-CO-TYPE
000576     END-IF
000577
000578     
      * EXEC CICS READ
000579*         INTO    (COMPENSATION-MASTER)
000580*         DATASET ('ERCOMP')
000581*         RIDFLD  (WS-CO-KEY)
000582*         RESP    (WS-RESPONSE)
000583*    END-EXEC
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00001252' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303031323532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 WS-CO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000584
000585     display ' read response ' ws-response
000586
000587     if not resp-normal
000588        go to 0400-exit
000589     end-if
000590
000591     move co-carrier             to cr-carrier
000592     move co-grouping            to cr-grouping
000593     move co-resp-no             to cr-resp-no
000594     move co-account             to cr-account
000595     move co-type                to cr-type
000596
000597     move co-last-maint-hhmmss   to ws-work-time
000598     string
000599        ws-hh   ':'
000600        ws-mm   ':'
000601        ws-ss delimited by size into cr-last-maint-hhmmss
000602     end-string
000603
000604*    move co-last-maint-hhmmss   to cr-last-maint-hhmmss-n
000605     move co-last-maint-user     to cr-last-maint-user
000606
000607     if co-last-maint-dt <> spaces and low-values
000608        move co-last-maint-dt    to dc-bin-date-1
000609        set bin-to-greg to true *> move ' ' to dc-option
000610        perform 9700-date-link   thru 9700-exit
000611        if no-conversion-error
000612           move dc-greg-date-a-edit
000613                                 to cr-last-maint-dt
000614           move +0               to nu-last-maint-dt
000615        else
000616           display ' error maint dt invalid ' dc-error-code
000617           move -1               to nu-last-maint-dt
000618        end-if
000619     else
000620        move spaces              to cr-last-maint-dt
000621        move -1                  to nu-last-maint-dt
000622     end-if
000623
000624     move co-stmt-type           to cr-stmt-type
000625     move co-comp-type           to cr-comp-type
000626     move co-stmt-owner          to cr-stmt-owner
000627     move co-balance-control     to cr-balance-control
000628     move co-internal-control-1  to cr-internal-control-1
000629     move CO-INTERNAL-CONTROL-1  to cr-INTERNAL-CONTROL-1
000630     move CO-INTERNAL-CONTROL-2  to cr-INTERNAL-CONTROL-2
000631     move CO-GA-WITHOLD-PCT      to cr-GA-WITHOLD-PCT-n
000632     move CO-GA-DIRECT-DEP       to cr-GA-DIRECT-DEP
000633     move CO-ACCT-NAME           to cr-ACCT-NAME
000634     move CO-MAIL-NAME           to cr-MAIL-NAME
000635     move CO-ADDR-1              to cr-ADDR-1
000636     move CO-ADDR-2              to cr-ADDR-2
000637     move CO-ADDR-3              to cr-ADDR-3
000638     move CO-CSO-1099            to cr-CSO-1099
000639     move CO-ZIP                 to cr-ZIP
000640     move CO-SOC-SEC             to cr-SOC-SEC
000641     move CO-TELEPHONE           to cr-TELEPHONE
000642
000643     if co-roladex-print-dt <> spaces and low-values
000644        move co-roladex-print-dt to dc-bin-date-1
000645        set bin-to-greg to true *> move ' ' to dc-option
000646        perform 9700-date-link   thru 9700-exit
000647        if no-conversion-error
000648           move dc-greg-date-a-edit
000649                                 to cr-rolodex-print-dt
000650           move +0               to nu-rolodex-print-dt
000651        else
000652           display ' error rolodex print dt invalid '
000653              dc-error-code
000654           move -1               to nu-rolodex-print-dt
000655        end-if
000656     else
000657        move spaces              to cr-rolodex-print-dt
000658        move -1                  to nu-rolodex-print-dt
000659     end-if
000660
000661     move CO-AR-BAL-LEVEL        to cr-AR-BAL-LEVEL
000662     move CO-AR-NORMAL-PRINT     to cr-AR-NORMAL-PRINT
000663     move CO-AR-SUMMARY-CODE     to cr-AR-SUMMARY-CODE
000664     move CO-AR-REPORTING        to cr-AR-REPORTING
000665     move CO-AR-PULL-CHECK       to cr-AR-PULL-CHECK
000666     move CO-AR-BALANCE-PRINT    to cr-AR-BALANCE-PRINT
000667     move CO-AR-LAST-RUN-CODE    to cr-AR-LAST-RUN-CODE
000668
000669     if co-LAST-EOM-STMT-DT <> spaces and low-values
000670        move co-LAST-EOM-STMT-DT to dc-bin-date-1
000671        set bin-to-greg to true *> move ' ' to dc-option
000672        perform 9700-date-link   thru 9700-exit
000673        if no-conversion-error
000674           move dc-greg-date-a-edit
000675                                 to cr-LAST-EOM-STMT-DT
000676           move +0               to nu-LAST-EOM-STMT-DT
000677        else
000678           display ' error last eom stmt dt invalid '
000679              dc-error-code
000680           move -1               to nu-LAST-EOM-STMT-DT
000681        end-if
000682     else
000683        move spaces              to cr-LAST-EOM-STMT-DT
000684        move -1                  to nu-LAST-EOM-STMT-DT
000685     end-if
000686
000687     move CO-USER-CODE           to cr-USER-CODE
000688     move CO-REPORT-GROUP-ID     to cr-REPORT-GROUP-ID
000689
000690     if co-LAST-ACTIVITY-DATE <> spaces and low-values
000691        move co-LAST-ACTIVITY-DATE
000692                                 to dc-greg-date-1-ymd-r
000693        set ymd-greg-to-bin to true *> move '3' to dc-option
000694        perform 9700-date-link   thru 9700-exit
000695        if no-conversion-error
000696           move dc-greg-date-a-edit
000697                                 to cr-LAST-ACTIVITY-DATE
000698           move +0               to nu-LAST-ACTIVITY-DATE
000699        else
000700           display ' error last activity dt invalid '
000701              dc-error-code
000702           move -1               to nu-LAST-ACTIVITY-DATE
000703        end-if
000704     else
000705        move spaces              to cr-LAST-ACTIVITY-DATE
000706        move -1                  to nu-LAST-ACTIVITY-DATE
000707     end-if
000708
000709     if co-LAST-STMT-DT <> spaces and low-values
000710        move co-LAST-STMT-DT     to dc-greg-date-1-ymd-r
000711        set ymd-greg-to-bin to true *> move '3' to dc-option
000712        perform 9700-date-link   thru 9700-exit
000713        if no-conversion-error
000714           move dc-greg-date-a-edit
000715                                 to cr-LAST-STMT-DT
000716           move +0               to nu-LAST-STMT-DT
000717        else
000718           display ' error last stmt dt invalid '
000719              dc-error-code
000720           move -1               to nu-LAST-STMT-DT
000721        end-if
000722     else
000723        move spaces              to cr-LAST-STMT-DT
000724        move -1                  to nu-LAST-STMT-DT
000725     end-if
000726
000727     move CO-BAL-FWD             to cr-BAL-FWD
000728     move CO-CUR-COM             to cr-CUR-COM
000729     move CO-CUR-CHG             to cr-CUR-CHG
000730     move CO-CUR-PMT             to cr-CUR-PMT
000731     move CO-END-BAL             to cr-END-BAL
000732     move CO-CUR                 to cr-CUR
000733     move CO-OV30                to cr-OV30
000734     move CO-OV60                to cr-OV60
000735     move CO-OV90                to cr-OV90
000736     move CO-YTD-COM             to cr-YTD-COM
000737     move CO-YTD-OV              to cr-YTD-OV
000738     move CO-CUR-OVR-UNDR        to cr-CUR-OVR-UNDR
000739     move CO-YTD-OVR-UNDR        to cr-YTD-OVR-UNDR
000740     move CO-CUR-FICA            to cr-CUR-FICA
000741     move CO-YTD-FICA            to cr-YTD-FICA
000742     move CO-LF-CLM-AMT          to cr-LF-CLM-AMT
000743     move CO-AH-CLM-AMT          to cr-AH-CLM-AMT
000744
000745     if co-current-LAST-STMT-DT <> spaces and low-values
000746        move co-current-LAST-STMT-DT
000747                                 to dc-greg-date-1-ymd-r
000748        set ymd-greg-to-bin to true *> move '3' to dc-option
000749        perform 9700-date-link   thru 9700-exit
000750        if no-conversion-error
000751           move dc-greg-date-a-edit
000752                                 to cr-current-LAST-STMT-DT
000753           move +0               to nu-current-LAST-STMT-DT
000754        else
000755           display ' error current last stmt dt invalid '
000756              dc-error-code
000757           move -1               to nu-current-LAST-STMT-DT
000758        end-if
000759     else
000760        move spaces              to cr-current-LAST-STMT-DT
000761        move -1                  to nu-current-LAST-STMT-DT
000762     end-if
000763
000764     move CO-CURRENT-BAL-FWD     to cr-CURRENT-BAL-FWD
000765     move CO-CURRENT-CUR-COM     to cr-CURRENT-CUR-COM
000766     move CO-CURRENT-CUR-CHG     to cr-CURRENT-CUR-CHG
000767     move CO-CURRENT-CUR-PMT     to cr-CURRENT-CUR-PMT
000768     move CO-CURRENT-END-BAL     to cr-CURRENT-END-BAL
000769     move CO-CURRENT-CUR         to cr-CURRENT-CUR
000770     move CO-CURRENT-OV30        to cr-CURRENT-OV30
000771     move CO-CURRENT-OV60        to cr-CURRENT-OV60
000772     move CO-CURRENT-OV90        to cr-CURRENT-OV90
000773     move CO-CURRENT-YTD-COM     to cr-CURRENT-YTD-COM
000774     move CO-CURRENT-YTD-OV      to cr-CURRENT-YTD-OV
000775     move CO-YTD-PAID-COM        to cr-YTD-PAID-COM
000776     move CO-YTD-PAID-OV         to cr-YTD-PAID-OV
000777     move CO-CURRENT-MONTH-ACTIVITY
000778                                 to cr-CURRENT-MONTH-ACTIVIty
000779     move CO-DELINQUENT-LETTER-CODE
000780                                 to cr-DELINQUENT-LETTER-COde
000781     move CO-CSR-CODE            to cr-CSR-CODE
000782
000783     if co-GA-EFFECTIVE-DT <> spaces and low-values
000784        move co-GA-EFFECTIVE-DT to dc-bin-date-1
000785        set bin-to-greg to true *> move ' ' to dc-option
000786        perform 9700-date-link   thru 9700-exit
000787        if no-conversion-error
000788           move dc-greg-date-a-edit
000789                                 to cr-GA-EFFECTIVE-DT
000790           move +0               to nu-GA-EFFECTIVE-DT
000791        else
000792           display ' error GA EFF dt invalid '
000793              dc-error-code
000794           move -1               to nu-GA-EFFECTIVE-DT
000795        end-if
000796     else
000797        move spaces              to cr-GA-EFFECTIVE-DT
000798        move -1                  to nu-GA-EFFECTIVE-DT
000799     end-if
000800
000801     if co-GA-termination-DT <> spaces and low-values
000802        move co-GA-TERMINATION-DT to dc-bin-date-1
000803        set bin-to-greg to true *> move ' ' to dc-option
000804        perform 9700-date-link   thru 9700-exit
000805        if no-conversion-error
000806           move dc-greg-date-a-edit
000807                                 to cr-GA-TERMINATION-DT
000808           move +0               to nu-GA-TERMINATION-DT
000809        else
000810           display ' error GA termination dt invalid '
000811              dc-error-code
000812           move -1               to nu-GA-TERMINATION-DT
000813        end-if
000814     else
000815        move spaces              to cr-GA-TERMINATION-DT
000816        move -1                  to nu-GA-TERMINATION-DT
000817     end-if
000818
000819     move CO-GA-STATUS-CODE      to cr-GA-STATUS-CODE
000820     move CO-GA-COMMENT-1        to cr-GA-COMMENT-1
000821     move CO-GA-COMMENT-2        to cr-GA-COMMENT-2
000822     move CO-GA-COMMENT-3        to cr-GA-COMMENT-3
000823     move CO-GA-COMMENT-4        to cr-GA-COMMENT-4
000824     move CO-RPTCD2              to cr-RPTCD2
000825     move CO-OV120               to cr-OV120
000826     move CO-CURRENT-OV120       to cr-CURRENT-OV120
000827     move CO-TYPE-AGENT          to cr-TYPE-AGENT
000828     move CO-FAXNO               to cr-FAXNO
000829     move CO-MD-GL-ACCT          to cr-MD-GL-ACCT
000830     move CO-MD-DIV              to cr-MD-DIV
000831     move CO-MD-CENTER           to cr-MD-CENTER
000832     move CO-MD-AMT              to cr-MD-AMT-n
000833     move CO-CREATE-AP-CHECK     to cr-CREATE-AP-CHECK
000834     move CO-DELIVER-CK-TO-MEL   to cr-DELIVER-CK-TO-MEL
000835     move CO-ACH-STATUS          to cr-ACH-STATUS
000836     display ' co bill sw ' co-bill-sw
000837     move CO-BILL-SW             to cr-BILL-SW
000838     move CO-CONTROL-NAME        to cr-CONTROL-NAME
000839     move CO-MAX-BANK-FEE-LEASE  to cr-MAX-BANK-FEE-LEASE
000840     move CO-MAX-BANK-FEE        to cr-MAX-BANK-FEE
000841     move CO-CLP-STATE           to cr-CLP-STATE
000842
000843     if co-FIRST-WRITTEN-DT <> spaces and low-values
000844        move co-FIRST-WRITTEN-DT to dc-bin-date-1
000845        set bin-to-greg to true *> move ' ' to dc-option
000846        perform 9700-date-link   thru 9700-exit
000847        if no-conversion-error
000848           move dc-greg-date-a-edit
000849                                 to cr-FIRST-WRITTEN-DT
000850           move +0               to nu-FIRST-WRITTEN-DT
000851        else
000852           display ' error 1st written dt invalid '
000853              dc-error-code
000854           move -1               to nu-FIRST-WRITTEN-DT
000855        end-if
000856     else
000857        move spaces              to cr-FIRST-WRITTEN-DT
000858        move -1                  to nu-FIRST-WRITTEN-DT
000859     end-if
000860
000861     move CO-SPP-REFUND-EDIT     to cr-SPP-REFUND-EDIT
000862
000863     perform 0500-update-table   thru 0500-exit
000864
000865     .
000866 0400-EXIT.
000867     EXIT.
000868
000869 0500-update-table.
000870
000871     display ' made it to 0500-update '
000872
000873     if not connected-to-db
000874        perform 6000-connect-to-db thru 6000-exit
000875     end-if
000876
000877     display ' bill sw ' cr-bill-sw
000878     display ' key *' cr-carrier '*' cr-grouping '*'
000879        cr-resp-no '*' cr-account '*' cr-type '*'
000880
000882     exec sql
              update
000883           ERCOMP
000884        set
000885           ex_stmt_type = :cr-stmt-type,
000886           ex_comp_spp = :cr-comp-type,
000887           ex_balance_control = :cr-balance-control,
000888           ex_bill_sw = :cr-bill-sw,
000889           ex_ga_withhold_pct = :cr-ga-withold-pct,
000890           ex_ga_direct_deposit = :cr-ga-direct-dep,
000891           ex_create_ap_check = :cr-create-ap-check,
000892           ex_deliver_to_mel = :cr-deliver-ck-to-mel,
000893           ex_md_gl_acct = :cr-md-gl-acct,
000894           ex_md_div = :cr-md-div,
000895           ex_md_center = :cr-md-center,
000896           ex_md_amount = :cr-md-amt,
000897           ex_report_group_id = :cr-report-group-id,
000898           ex_comment1 = :cr-ga-comment-1,
000899           ex_comment2 = :cr-ga-comment-2,
000900           ex_comment3 = :cr-ga-comment-3,
000901           ex_comment4 = :cr-ga-comment-4,
000902           ex_status = :cr-ga-status-code,
000903           ex_effect = :cr-GA-EFFECTIVE-DT :nu-GA-EFFECTIVE-DT,
000904           ex_expire = :CR-GA-TERMINATION-DT
000905                       :nu-GA-TERMINATION-DT,
000906           ex_last_maint_dt = :cr-last-maint-dt
000907                       :nu-last-maint-dt,
000908           ex_last_maint_user = :cr-last-maint-user,
000909           ex_last_maint_time = :cr-last-maint-hhmmss,
000910           ex_stmt_owner = :cr-stmt-owner
000911        where
000912           ex_carrier  = :cr-carrier and
000913           ex_grouping = :cr-grouping and
000914           ex_resp_no  = :cr-resp-no and
000915           ex_account  = :cr-account and
000916           ex_type     = :cr-type
000917     end-exec
000918
000919     if sqlcode not = 0
000920        display "Error: did not update table "
000921        display ' sql return code ' sqlcode
000922        display ' sql err mess    ' sqlerrmc
000923        move sqlcode             to ws-display-response
000924        move '9999;Did not update ' to ws-return-string
000925        PERFORM 0200-SEND-BUFFER thru 0200-exit
000926        perform 0250-socket-error thru 0250-exit
000927        go to 0300-close-socket
000928     end-if
000929     display ' sql code upd ' sqlcode
000930     .
000931 0500-exit.
000932     exit.
000933
000934 6000-CONNECT-TO-DB.
000935
000936***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000937***                                                            ***
000938***                                                            ***
000939***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000940
000941
000942****  The below code is for when the db has been
000943****  converted to sql server 2016
000944     evaluate ws-kix-myenv
000945        when 'cid1p'
000946           move '//sdv-db01.cso.local:1433;'
000947                                 to p-sql-server
000948        when 'mdoff'
000949           move '//hov-tstdb01.cso.local:55330;'
000950                                 to p-sql-server
000951        when other
000952           move '//hov-tstdb01.cso.local:1433;'
000953                                 to p-sql-server
000954     end-evaluate
000955
000956     move 'Logic'                to p-sql-database
000957
000958     CALL 'SQLCONNECT' USING sqlconnect-parms
000959     display ' ret code ' p-connect-return-code
000960     move p-connect-return-code  to sqlcode
000961     move p-sql-return-message   to sqlerrmc
000962
000963
000964     if sqlcode not = 0
000965        display "Error: cannot connect to " svr
000966        display sqlcode
000967        display sqlerrmc
000968        move '9999;Could not connect to DB ' to ws-return-string
000969        PERFORM 0200-SEND-BUFFER thru 0200-exit
000970        perform 0250-socket-error thru 0250-exit
000971        go to 0300-close-socket
000972     else
000973        display ' Successful Connect ' sqlcode
000974        set connected-to-db to true
000975     end-if
000976
000977     .
000978 6000-EXIT.
000979     EXIT.
000980
000981 9700-DATE-LINK.
000982
000983     
      * EXEC CICS LINK
000984*         PROGRAM  ('ELDATCV')
000985*         COMMAREA (DATE-CONVERSION-DATA)
000986*         LENGTH   (DC-COMM-LENGTH)
000987*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00001657' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303031363537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000988
000989 9700-EXIT.
000990      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK16' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK16' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
