      *((program: NSRASBL.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. NSRASBL.
000004
000005*AUTHOR.     PABLO
000006*            COLLEYVILLE, TEXAS.
000007
000008*REMARKS.    EXECUTED FROM NSRASLTR
000009
000010******************************************************************
000011*                   C H A N G E   L O G
000012*
000013* ChangeS ARE MARKED BY THE Change EFFECTIVE DATE.
000014*-----------------------------------------------------------------
000015*  Change   Change REQUEST PGMR  DESCRIPTION OF Change
000016* EFFECTIVE    NUMBER
000017*-----------------------------------------------------------------
000018* 060611    2011022800001  PEMA  NEW PROGRAM
000019* 122911    2011022800001  AJRA  ADD VA DISCLOSURE TO CERT PRINT
000020* 071212    2011022800001  AJRA  ADD UNDW ID
000021* 072312    2011022800001  AJRA    ADD NEW FIELDS
000022* 090612  IR2012090600001  AJRA  NAPERSOFT
000023* 091112  IR2012091100002  AJRA  FIX AKA
000024* 091312  IR2012091100004  AJRA  FIX JOINT NAME
000025* 092412  IR2012092350004  AJRA  FIX CANCEL AMT
000026* 100112  IR2012100100002  AJRA  FIX ADDRESS WITH 2 SPACES
000027* 100412  IR2012100300002  AJRA  FIX VA DISC FOR DISAB ONLY CERT
000028* 101412    2011022800001  AJRA  POST IMPLEMENTATION ITEMS
000029* 101812    2012101700002  AJRA  READ ENDT BY ARCHIVE NO
000030* 110612    2012101700002  AJRA  ADD NEW FIELDS
000031* 121112    2012101700002  AJRA  HANDLE .01 PREM AND 9999999 BEN
000032* 121712    2012101700002  AJRA  ADD DEFAULT AGE FLAG
000033* 050213    2013050100001  AJRA  FIX AH REFUND ON DEATH CLAIM
000034* 091213    2013090300001  AJRA  NAPERSOFT PHASE 2
000035* 123113    2013090300001  AJRA  USE NEXT BUS DT FOR RESEND DT CAL
000036* 011314  IR2014011300001  AJRA  FIX NEXT BUS DT AND ENDORSEMENT A
000037* 011514    2013090300001  AJRA  CALL SQL STORED PROC FOR NEXT BUS
000038* 021214  IR2014021200002  AJRA  ADD SQL DISCONNECT
000039* 121015  CR2015100900001  TANA  ADD VIN NUMBER
000040* 020916  CR2016010700001  TANA  CALL SQL LOOKUP NAPERSOFT TABLE
000041* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTERES
000042* 061821  CR2017031500001  TANA  FIX LOOKUP TABLE READ FOR CCM8
000043* 070622  CR2020061200002  TANA  ADD CANCEL REAS, PAYEE, CHK AMT
000044* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
000045* 080522  CR2019120300001  PEMA  Prevent zero UE Commission.
000046******************************************************************
000047 ENVIRONMENT DIVISION.
000048
000049 DATA DIVISION.
000050
000051 working-storage section.
       01  DFH-START PIC X(04).
000052
000053 77  SAVE-DATE                   PIC X(8)    VALUE SPACES.
000054 77  WS-SAVE-EDIT-A-DATE         PIC X(10)   VALUE SPACES.
000055 77  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
000056 77  SAVE-BIN-NEXT-BUS-DT        PIC XX      VALUE SPACES.
000057 77  SAVE-NEXT-BUS-DT-EDIT-A     PIC X(10)   VALUE SPACES.
000058 77  SAVE-CYCLE-DAY-OF-WEEK      PIC S9   COMP-3 VALUE +0.
000059 77  S1                          PIC S999 COMP-3 VALUE +0.
000060 77  E1                          PIC S999 COMP-3 VALUE +0.
000061 77  A1                          PIC S9(5) COMP-3 VALUE +0.
000062 77  M1                          PIC S9(5) COMP-3 VALUE +0.
000063 77  WS-WORK-FIELD               PIC X(80)    VALUE SPACES.
000064 77  WS-ARCHIVE-NO               PIC S9(8)  COMP VALUE +0.
000065 77  WS-FOLLOW-UP-DT             PIC XX  VALUE LOW-VALUES.
000066 77  WS-RESEND-DT                PIC XX  VALUE LOW-VALUES.
000067 77  WS-AUTO-LAST-SCHED-DT       PIC XX  VALUE LOW-VALUES.
000068 77  WS-LAST-ACT-DT              PIC XX  VALUE LOW-VALUES.
000069 77  WS-ACTIVITY-DT              PIC XX  VALUE LOW-VALUES.
000070 77  WS-FORM                     PIC XXXX  VALUE SPACES.
000071 77  WS-LAST-ACT-TYPE            PIC X   VALUE ' '.
000072 77  WS-FOUND-BENE-SW            PIC X   VALUE ' '.
000073     88  FOUND-BENE                  VALUE 'Y'.
000074 77  WS-TALLY                    PIC S999 COMP-3 VALUE +0.
000075 77  WS-TALLY1                   PIC S999 COMP-3 VALUE +0.
000076 77  NS-LEN                      PIC S9(5) COMP-3 VALUE +0.
000077 77  WS-ERPNDB-SW                PIC X  VALUE ' '.
000078     88  END-OF-ERPNDB              VALUE 'Y'.
000079 77  WS-ERACCT-SW                PIC X  VALUE ' '.
000080     88  ACCT-FOUND                 VALUE 'Y'.
000081 77  WS-ELCERT-SW                PIC X  VALUE ' '.
000082     88  CERT-FOUND                 VALUE 'Y'.
000083     88  CERT-NOT-FOUND             VALUE 'N'.
000084 77  WS-ERENDT-SW                PIC X  VALUE ' '.
000085     88  ENDT-FOUND                 VALUE 'Y'.
000086     88  ENDT-NOT-FOUND             VALUE 'N'.
000087 77  WS-CERT-TRL-REC-NOT-FOUND   PIC S9       VALUE +0.
000088     88  CERT-TRL-REC-NOT-FOUND     VALUE +1.
000089 77  WS-CONNECT-SW               PIC X  VALUE ' '.
000090     88  CONNECTED-TO-DB             VALUE 'Y'.
000091
000092
000093 77  WS-LF-ABBRV                 PIC X(10) VALUE SPACES.
000094 77  WS-LF-DESC                  PIC X(02) VALUE SPACES.
000095 77  WS-LF-EARN                  PIC X     VALUE ' '.
000096 77  WS-WAIT-PER                 PIC X(03) VALUE ZEROS.
000097 77  WS-RET-ELIM                 PIC X(01) VALUE SPACES.
000098 77  WS-AH-DESC                  PIC X(35) VALUE SPACES.
000099 77  WS-BEN-DAYS                 PIC X(02) VALUE ZEROS.
000100
000101
000102 77  WS-CSR-ID                   PIC XXXX  VALUE SPACES.
000103 77  WS-LF-BENCD                 PIC XX    VALUE '00'.
000104 77  WS-AH-BENCD                 PIC XX    VALUE '00'.
000105 77  WS-SAVE-ERACCT-KEY          PIC X(20)  VALUE SPACES.
000106 77  WS-COMPANY-CD               PIC X  VALUE LOW-VALUES.
000107 77  WS-WORK-PREM                PIC S9(9)V99 COMP-3 VALUE +0.
000108 77  WS-WORK-LF-PREM             PIC S9(9)V99 COMP-3 VALUE +0.
000109 77  WS-WORK-ALT-LF-PREM         PIC S9(9)V99 COMP-3 VALUE +0.
000110 77  WS-WORK-CMB-LF-PREM         PIC S9(9)V99 COMP-3 VALUE +0.
000111 77  WS-WORK-AH-PREM             PIC S9(9)V99 COMP-3 VALUE +0.
000112 77  WS-WORK-REF                 PIC S9(9)V99 COMP-3 VALUE +0.
000113 77  WS-WORK-LF-REF              PIC S9(9)V99 COMP-3 VALUE +0.
000114 77  WS-WORK-AH-REF              PIC S9(9)V99 COMP-3 VALUE +0.
000115 77  WS-ISSUE-CANCEL-SW          PIC X  VALUE SPACES.
000116     88  PROCESS-ISSUE               VALUE 'I'.
000117     88  PROCESS-CANCEL              VALUE 'C'.
000118 77  WS-SAVE-OUT-LETTER          PIC X(6)  VALUE SPACES.
000119 77  WS-WORK-COMM                PIC 9(7)V99 VALUE ZEROS.
000120 77  WS-DIFF                     PIC 999 VALUE ZEROS.
000121 77  WS-CHGBACK                  PIC 99  VALUE ZEROS.
000122 77  WS-CHGBK-LIFE-PCT           PIC S9V9(5) COMP-3 VALUE +0.
000123 77  WS-CHGBK-AH-PCT             PIC S9V9(5) COMP-3 VALUE +0.
000124 77  WS-CERT-EFF-DT              PIC XX  VALUE SPACES.
000125 77  LINK-ELVADS                 PIC X(8) VALUE 'ELVADS'.
000126
000127*EXEC SQL
000128*   INCLUDE SQLDA
000129*END-EXEC
000130
000133*EXEC SQL
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
000134
000136 EXEC SQL
          BEGIN DECLARE SECTION
000137 END-EXEC
000138
000139 01  SQLCMD                      PIC X(1024).
000140 01  SVR                         PIC X(32).
000141 01  database                    pic x(32).
000142 01  USR                         PIC X(32).
000143 01  PASS                        PIC X(32).
000144 01  USR-PASS                    PIC X(64).
000145
000146 01  WS-SQL-DATA.
000147     05  WS-CYCLE-DATE           PIC X(10).
000148     05  WS-NEXT-BUS-DT          PIC X(10).
000149     05  WS-LOOKUPID             PIC X(4).
000150     05  WS-LOOKUPNAME           PIC X(4).
000151     05  WS-LOOKUP-VALUE         PIC X(100).
000152     05  WS-CARRIER              PIC X.
000153     05  WS-GROUP                PIC X(6).
000154     05  WS-STATE                PIC XX.
000155     05  WS-ACCOUNT              PIC X(10).
000156     05  WS-EFF-DT               PIC XX.
000157     05  WS-CERT-NO              PIC X(10).
000158     05  WS-CERT-NO-SUF          PIC X(01).
000159
000160 01  indicator-vaiables-for-nulls.
000161     05  nu-app-status           pic s9(4) comp value +0.
000162     05  nu-app-by               pic s9(4) comp value +0.
000163     05  nu-app-date             pic s9(4) comp value +0.
000164     05  nu-app-batch            pic s9(4) comp value +0.
000165
000166 01  daily-check-request-rec.
000167     05  db-compid               pic xxx.
000168     05  db-carrier              pic x.
000169     05  db-grouping             pic x(6).
000170     05  db-state                pic xx.
000171     05  db-account              pic x(10).
000172     05  db-effdate              pic x(10).
000173     05  db-certificate          pic x(10).
000174     05  db-cert-sfx             pic x.
000175     05  db-seq-no               pic 999.
000176     05  db-type                 pic x.
000177     05  db-amount-n             PIC S9(13)V99   COMP-3.
000178*    05  db-amount redefines db-amount-n
000179*                                pic x(10).
000180     05  db-checkno              pic x(15).
000181     05  db-checkdate            pic x(10).
000182     05  db-checkstatus          pic 9(5).
000183     05  db-releasebatch         pic 9(5).
000184     05  db-releasedt            pic x(10).
000185     05  db-releaseby            pic x(4).
000186     05  db-payeename1           pic x(30).
000187     05  db-payeename2           pic x(30).
000188     05  db-payeeaddr1           pic x(30).
000189     05  db-payeeaddr2           pic x(30).
000190     05  db-payeecity            pic x(30).
000191     05  db-payeest              pic xx.
000192     05  db-payeezip             pic x(10).
000193     05  db-fincar               pic x.
000194     05  db-fingrp               pic x(6).
000195     05  db-finresp              pic x(10).
000196     05  db-finacct              pic x(10).
000197     05  db-preparer             pic x(4).
000198     05  db-app-status           pic x(9).
000199     05  dp-app-status-n redefines db-app-status
000200                                 pic 9(9).
000201     05  db-app-by               pic x(20).
000202     05  db-app-date             pic x(30).
000203     05  db-app-batch            pic x(10).
000204     05  db-return-to            pic x(30).
000205     05  db-insured-name         pic x(30).
000206     05  db-check-sub-type       pic x.
000207     05  db-payeecode            pic x(10).
000208
000209
000211 EXEC SQL
          END DECLARE SECTION
000212 END-EXEC
000213
000214
000215 01  P pointer.
000216 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000217 01  KIXHOST             pic x(9) value Z"HOSTNAME".
000218 01  var-ptr pointer.
000219 01  env-var-len                 pic 9(4)  binary.
000220 01  rc                          pic 9(9)  binary.
000221
000222 01  WS-KIXHOST                  PIC X(10).
000223 01  WS-KIXSYS.
000224     05  WS-KIX-FIL1             PIC X(10).
000225     05  WS-KIX-APPS             PIC X(10).
000226     05  WS-KIX-ENV              PIC X(10).
000227     05  WS-KIX-MYENV            PIC X(10).
000228     05  WS-KIX-SYS              PIC X(10).
000229
000230 01  WS-CALC-FIELDS.
000231     12  WS-WRK-AMOUNT           PIC S9(7)V99 COMP-3 VALUE +0.
000232     12  WS-WRK-AMOUNT2          PIC S9(7)V99 COMP-3 VALUE +0.
000233     12  WS-CSO-LF-PORTION       PIC S9(7)V99 COMP-3 VALUE +0.
000234     12  WS-CSO-ALT-LF-PORTION   PIC S9(7)V99 COMP-3 VALUE +0.
000235     12  WS-CSO-CMB-LF-PORTION   PIC S9(7)V99 COMP-3 VALUE +0.
000236     12  WS-CSO-AH-PORTION       PIC S9(7)V99 COMP-3 VALUE +0.
000237     12  WS-CSO-PORTION          PIC S9(7)V99 COMP-3 VALUE +0.
000238     12  WS-ACCT-LF-PORTION      PIC S9(7)V99 COMP-3 VALUE +0.
000239     12  WS-ACCT-ALT-LF-PORTION  PIC S9(7)V99 COMP-3 VALUE +0.
000240     12  WS-ACCT-CMB-LF-PORTION  PIC S9(7)V99 COMP-3 VALUE +0.
000241     12  WS-ACCT-AH-PORTION      PIC S9(7)V99 COMP-3 VALUE +0.
000242     12  WS-ACCT-PORTION         PIC S9(7)V99 COMP-3 VALUE +0.
000243     12  WS-CSO-ORIG-LF-PORT     PIC S9(7)V99 COMP-3 VALUE +0.
000244     12  WS-CSO-ORIG-ALT-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
000245     12  WS-CSO-ORIG-CMB-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
000246     12  WS-CSO-ORIG-AH-PORT     PIC S9(7)V99 COMP-3 VALUE +0.
000247     12  WS-CSO-ORIG-PORTION     PIC S9(7)V99 COMP-3 VALUE +0.
000248     12  WS-ACCT-ORIG-LF-PORT    PIC S9(7)V99 COMP-3 VALUE +0.
000249     12  WS-ACCT-ORIG-ALT-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
000250     12  WS-ACCT-ORIG-CMB-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
000251     12  WS-ACCT-ORIG-AH-PORT    PIC S9(7)V99 COMP-3 VALUE +0.
000252     12  WS-ACCT-ORIG-PORTION    PIC S9(7)V99 COMP-3 VALUE +0.
000253     12  WS-CSO-NEW-LF-PORT      PIC S9(7)V99 COMP-3 VALUE +0.
000254     12  WS-CSO-NEW-ALT-LF-PORT  PIC S9(7)V99 COMP-3 VALUE +0.
000255     12  WS-CSO-NEW-CMB-LF-PORT  PIC S9(7)V99 COMP-3 VALUE +0.
000256     12  WS-CSO-NEW-AH-PORT      PIC S9(7)V99 COMP-3 VALUE +0.
000257     12  WS-CSO-NEW-PORTION      PIC S9(7)V99 COMP-3 VALUE +0.
000258     12  WS-ACCT-NEW-LF-PORT     PIC S9(7)V99 COMP-3 VALUE +0.
000259     12  WS-ACCT-NEW-ALT-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
000260     12  WS-ACCT-NEW-CMB-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
000261     12  WS-ACCT-NEW-AH-PORT     PIC S9(7)V99 COMP-3 VALUE +0.
000262     12  WS-ACCT-NEW-PORTION     PIC S9(7)V99 COMP-3 VALUE +0.
000263     12  WS-CSO-LF-PORT-CHG      PIC S9(7)V99 COMP-3 VALUE +0.
000264     12  WS-CSO-ALT-LF-PORT-CHG  PIC S9(7)V99 COMP-3 VALUE +0.
000265     12  WS-CSO-CMB-LF-PORT-CHG  PIC S9(7)V99 COMP-3 VALUE +0.
000266     12  WS-CSO-AH-PORT-CHG      PIC S9(7)V99 COMP-3 VALUE +0.
000267     12  WS-CSO-PORTION-CHG      PIC S9(7)V99 COMP-3 VALUE +0.
000268     12  WS-ACCT-LF-PORT-CHG     PIC S9(7)V99 COMP-3 VALUE +0.
000269     12  WS-ACCT-ALT-LF-PORT-CHG PIC S9(7)V99 COMP-3 VALUE +0.
000270     12  WS-ACCT-CMB-LF-PORT-CHG PIC S9(7)V99 COMP-3 VALUE +0.
000271     12  WS-ACCT-AH-PORT-CHG     PIC S9(7)V99 COMP-3 VALUE +0.
000272     12  WS-ACCT-PORTION-CHG     PIC S9(7)V99 COMP-3 VALUE +0.
000273
000274
000275 01  WS-SAVE-ERACCT-REC          PIC X(2000).
000276 01  WS-XML-WORK                 PIC X(4200)  VALUE SPACES.
000277 01 response-code         pic s9(8) comp.
000278 01 display-response      pic 9(8).
000279 01 bl-index              pic 9(8) comp.
000280 01 max-last-name         pic x(18).
000281 01 first-initial         pic x.
000282 01 name-in-range-flag    pic 9.
000283 01 max-entries           pic s9(8) comp value 100.
000284
000285 01 WS-PASSED-DT          PIC XX   VALUE LOW-VALUES.
000286
000287 01 lower-case    pic x(26) value
000288            "abcdefghijklmnopqrstuvwxyz".
000289 01 upper-case    pic x(26) value
000290            "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
000291
000292
000293*                                COPY ELCVADS.
      *>>((file: ELCVADS))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCVADS.                            *
000005*                                                                *
000006*   FILE DESCRIPTION = VIRGINIA DISLCOSURE FIELDS PASSED TO      *
000007*   ELVADS                  LENGTH = 350                         *
000008******************************************************************
000009******************************************************************
000010*                   C H A N G E   L O G
000011*
000012* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000013*-----------------------------------------------------------------
000014*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000015* EFFECTIVE    NUMBER
000016*-----------------------------------------------------------------
000017* 100111    2011022800001  AJRA  NAPERSOFT
000018******************************************************************
000019
000020 01  VIRGINIA-DISCLOSURE.
000021     12  VD-COMPANY-CD              PIC X.
000022     12  VD-CARRIER                 PIC X.
000023     12  VD-GROUPING.
000024         20  VD-GROUPING-PREFIX     PIC XXX.
000025         20  VD-GROUPING-PRIME      PIC XXX.
000026     12  VD-STATE                   PIC XX.
000027     12  VD-ACCOUNT.
000028         20  VD-ACCOUNT-PREFIX      PIC X(4).
000029         20  VD-ACCOUNT-PRIME       PIC X(6).
000030     12  VD-CERT-EFF-DT             PIC XX.
000031     12  VD-CERT-NO.
000032         20  VD-CERT-PRIME          PIC X(10).
000033         20  VD-CERT-SFX            PIC X.
000034     12  VD-ENTRY-BATCH             PIC X(6).
000035     12  VD-CSR-ID                  PIC X(4).
000036     12  VD-NAME.
000037         20  VD-INSURED-LAST-NAME   PIC X(15).
000038         20  VD-INSURED-FIRST-NAME.
000039             24  VD-INSURED-1ST-INIT PIC X.
000040             24  FILLER             PIC X(9).
000041         20  VD-INSURED-MIDDLE-INIT PIC X.
000042     12  VD-JOINT-INSURED.
000043         20 VD-JOINT-LAST-NAME      PIC X(15).
000044         20 VD-JOINT-FIRST-NAME.
000045            24  VD-JOINT-FIRST-INIT PIC X.
000046            24  FILLER              PIC X(9).
000047         20 VD-JOINT-MIDDLE-INIT    PIC X.
000048     12  VD-INSURED-ADDRESS-1       PIC X(30).
000049     12  VD-INSURED-ADDRESS-2       PIC X(30).
000050     12  VD-INSURED-CITY-STATE.
000051         20  VD-INSURED-CITY        PIC X(28).
000052         20  VD-INSURED-STATE       PIC XX.
000053     12  VD-INSURED-ZIP-CODE.
000054         20  VD-INSURED-ZIP-PRIME.
000055             24  VD-INSURED-ZIP-1   PIC X.
000056                 88  VD-CANADIAN-POST-CODE
000057                                       VALUE 'A' THRU 'Z'.
000058             24  FILLER             PIC X(4).
000059         20  VD-INSURED-ZIP-PLUS4   PIC X(4).
000060     12  VD-BENEFICIARY-NAME        PIC X(30).
000061     12  VD-ACCOUNT-NAME            PIC X(30).
000062     12  VD-LOAN-TERM               PIC S999   COMP-3.
000063     12  VD-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
000064     12  VD-1ST-PMT-DT              PIC XX.
000065     12  VD-LIFE-BENEFIT-CD         PIC XX.
000066         88  VD-VALID-LIFE             VALUE '01' THRU '89'.
000067         88  VD-INVALID-LIFE           VALUE '  ' '00'
000068                                             '90' THRU '99'.
000069     12  VD-LF-BENEFIT-CD   REDEFINES VD-LIFE-BENEFIT-CD
000070                                    PIC XX.
000071     12  VD-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
000072     12  VD-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000073     12  VD-LF-RATE                 PIC S99V9(5)   COMP-3.
000074     12  VD-LF-TERM                 PIC S999   COMP-3.
000075     12  VD-AH-BENEFIT-CD           PIC XX.
000076         88  VD-VALID-AH               VALUE '01' THRU '89'.
000077         88  VD-INVALID-AH             VALUE '  ' '00'
000078                                             '90' THRU '99'.
000079     12  VD-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
000080     12  VD-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000081     12  VD-AH-RATE                 PIC S99V9(5)   COMP-3.
000082     12  VD-AH-TERM                 PIC S999   COMP-3.
000083     12  VD-LETTER-ID               PIC X(4).
000084     12  VD-PROC-ID                 PIC X(4).
000085     12  VD-COMP-ID                 PIC X(3).
000086     12  VD-CURRENT-DATE            PIC XX.
000087     12  VD-CURRENT-TIME            PIC S9(6) COMP-3.
000088     12  VD-ARCHIVE-NO              PIC S9(08) COMP VALUE +0.
000089     12  FILLER                     PIC X(32).
      *<<((file: ELCVADS))
000294 01  WS-PASS-AREA.
000295     05  WS-PASS-AREA-LENGTH PIC S9(4) COMP VALUE +352.
000296     05  WS-PASS-VADS-REC    PIC X(350).
000297
000298*** Z CONTROL LAYOUT MOVED TO COPYBOOK ELCZREC
000299*                         COPY ELCZREC.
      *>>((file: ELCZREC))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCZREC.                            *
000005*                                                                *
000006*   FILE DESCRIPTION = Z CONTROL RECORD LAYOUT                   *
000007*                                                                *
000008******************************************************************
000009*-----------------------------------------------------------------
000010*                   C H A N G E   L O G
000011*
000012* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000013*-----------------------------------------------------------------
000014*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000015* EFFECTIVE    NUMBER
000016*-----------------------------------------------------------------
000017* 122011    2011022800001  AJRA  NEW FILE
000018* 073112    2011022800001  AJRA  ADD ACCT SUMM, CSO SUMM
000019* 122712    2012101700002  AJRA  ADD REASON CODE REQUIRED FLAG
000020* 072313    2013062000003  AJRA  ADD IND FOR INSERTION BAR CODE
000021* 091913    2013090300001  AJRA  ADD SIGNATURE FLAG DEFAULT
000022*-----------------------------------------------------------------
000023 01  W-Z-CONTROL-DATA.
000024     05  W-NUMBER-OF-COPIES      PIC  9.
000025     05  FILLER                  PIC  X.
000026     05  W-DAYS-TO-FOLLOW-UP     PIC  999.
000027     05  FILLER                  PIC  X.
000028     05  W-DAYS-TO-RESEND        PIC  999.
000029     05  FILLER                  PIC  X.
000030     05  W-FORM-TO-RESEND        PIC  X(4).
000031     05  W-ADD-BAR-CODE          PIC  X.
000032     05  W-PROMPT-LETTER         PIC  X.
000033     05  W-HAS-RETURN-ENV        PIC  X.
000034     05  W-ENCLOSURE-CD          PIC  XXX.
000035     05  W-SIG-FLAG-DEFAULT      PIC  X.
000036     05  W-AUTO-CLOSE-IND        PIC  X.
000037     05  FILLER                  PIC  X.
000038     05  W-LETTER-TO-BENE        PIC  X.
000039     05  FILLER                  PIC  X.
000040     05  W-LETTER-TO-ACCT        PIC  X.
000041     05  FILLER                  PIC  X.
000042     05  W-LETTER-TYPE           PIC  X.
000043     05  FILLER                  PIC  X.
000044     05  W-PRINT-CERTIFICATE     PIC  X.
000045     05  FILLER                  PIC  X.
000046     05  W-REFUND-REQUIRED       PIC  X.
000047     05  FILLER                  PIC  X.
000048     05  W-ONBASE-CODE           PIC  XX.
000049     05  FILLER                  PIC  X.
000050     05  W-ACCT-SUMM             PIC  X.
000051     05  FILLER                  PIC  X.
000052     05  W-CSO-SUMM              PIC  X.
000053     05  FILLER                  PIC  X.
000054     05  W-REASONS-REQUIRED      PIC  X.
000055     05  FILLER                  PIC  X(29).
      *<<((file: ELCZREC))
000300
000301 01  MISC.
000302     12  WS-RESPONSE             PIC S9(8)   COMP.
000303         88  RESP-NORMAL                  VALUE +00.
000304         88  RESP-NOTFND                  VALUE +13.
000305         88  RESP-DUPREC                  VALUE +14.
000306         88  RESP-DUPKEY                  VALUE +15.
000307         88  RESP-NOTOPEN                 VALUE +19.
000308         88  RESP-ENDFILE                 VALUE +20.
000309
000310     12  WS-EDIT-A-DATE.
000311         16  WS-EDIT-A-MM                 PIC X(2).
000312         16  FILLER                       PIC X(1).
000313         16  WS-EDIT-A-DD                 PIC X(2).
000314         16  FILLER                       PIC X(1).
000315         16  WS-EDIT-A-CCYY.
000316             20  WS-EDIT-A-CC             PIC X(2).
000317             20  WS-EDIT-A-YY             PIC X(2).
000318
000319
000320 01  srch-commarea.
000321*                                copy ELCADLTRSPI.
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
000322 01  filler                      pic x(500).
000323
000324
000325 01  WS-SAVE-CARRIER-INFO.
000326     05  WS-PREV-CARRIER         PIC X      VALUE SPACES.
000327     05  WS-CARR-NAME            PIC X(30)  VALUE SPACES.
000328     05  WS-CARR-ICO             PIC X(30)  VALUE SPACES.
000329     05  WS-CARR-ADDR1           PIC X(30)  VALUE SPACES.
000330     05  WS-CARR-ADDR2           PIC X(30)  VALUE SPACES.
000331     05  WS-CARR-CITYST          PIC X(30)  VALUE SPACES.
000332     05  WS-CARR-ZIP             PIC X(9)   VALUE SPACES.
000333     05  WS-CARR-PHONE           PIC X(11)  VALUE SPACES.
000334
000335 01  WS-SAVE-STATE-INFO.
000336     05  WS-PREV-STATE           PIC XX     VALUE SPACES.
000337     05  WS-STATE-NAME           PIC X(25)  VALUE SPACES.
000338
000339 01  WS-SAVE-LFBEN-INFO.
000340     05  WS-PREV-LFBEN           PIC XX     VALUE SPACES.
000341     05  WS-LFBEN-ALPH           PIC XXX    VALUE SPACES.
000342     05  WS-LFBEN-DESC           PIC X(10)  VALUE SPACES.
000343
000344 01  WS-SAVE-AHBEN-INFO.
000345     05  WS-PREV-AHBEN           PIC XX     VALUE SPACES.
000346     05  WS-AHBEN-DESC           PIC X(10)  VALUE SPACES.
000347     05  WS-AHBEN-RET-ELIM       PIC X      VALUE SPACES.
000348     05  WS-AHBEN-DAYS           PIC XX     VALUE SPACES.
000349
000350 01  WS-ERPNDB-KEY.
000351     05  WS-ERPNDB-COMPANY-CD    PIC X.
000352     05  WS-ERPNDB-BATCH-NO      PIC X(6).
000353     05  WS-ERPNDB-BATCH-SEQ     PIC S9(4) COMP  VALUE +0.
000354     05  WS-ERPNDB-CHG-SEQ       PIC S9(4) COMP  VALUE +0.
000355
000356
000357 01  WS-ELENCC-KEY.
000358     05  WS-ELENCC-COMPANY-CD    PIC X.
000359     05  WS-ELENCC-REC-TYPE      PIC X.
000360     05  WS-ELENCC-ENC-CODE      PIC X(5).
000361     05  F                       PIC X(9).
000362
000363 01  WS-ELCERT-KEY.
000364     05  WS-ELCERT-COMPANY-CD    PIC X.
000365     05  WS-ELCERT-CARRIER       PIC X.
000366     05  WS-ELCERT-GROUP         PIC X(6).
000367     05  WS-ELCERT-STATE         PIC XX.
000368     05  WS-ELCERT-ACCOUNT       PIC X(10).
000369     05  WS-ELCERT-EFF-DT        PIC XX.
000370     05  WS-ELCERT-CERT-NO       PIC X(11).
000371
000372 01  WS-ELCRTT-KEY.
000373     05  WS-ELCRTT-PRIMARY       PIC X(33).
000374     05  WS-ELCRTT-REC-TYPE      PIC X(1).
000375
000376 01  WS-ERENDT-KEY.
000377     05  WS-ERENDT-COMPANY-CD    PIC X.
000378     05  WS-ERENDT-CARRIER       PIC X.
000379     05  WS-ERENDT-GROUP         PIC X(6).
000380     05  WS-ERENDT-STATE         PIC XX.
000381     05  WS-ERENDT-ACCOUNT       PIC X(10).
000382     05  WS-ERENDT-EFF-DT        PIC XX.
000383     05  WS-ERENDT-CERT-NO       PIC X(11).
000384     05  WS-ERENDT-REC-TYPE      PIC X.
000385     05  WS-ERENDT-SEQ-NO        PIC 9(4) BINARY.
000386
000387 01  WS-ERENDT-KEY-BY-ARCH.
000388     05  WS-ERENDT-COMPANY-CD-A1 PIC X.
000389     05  WS-ERENDT-ARCHIVE       PIC 9(8) BINARY.
000390
000391 01  WS-ERCOMP-KEY.
000392     05  WS-ERCOMP-COMPANY-CD    PIC X.
000393     05  WS-ERCOMP-CARRIER       PIC X.
000394     05  WS-ERCOMP-GROUP         PIC X(6).
000395     05  WS-ERCOMP-RESP-NO       PIC X(10).
000396     05  WS-ERCOMP-ACCOUNT       PIC X(10).
000397     05  WS-ERCOMP-REC-TYPE      PIC X.
000398
000399 01  WS-ERMAIL-KEY.
000400     05  WS-ERMAIL-COMPANY-CD    PIC X.
000401     05  WS-ERMAIL-CARRIER       PIC X.
000402     05  WS-ERMAIL-GROUP         PIC X(6).
000403     05  WS-ERMAIL-STATE         PIC XX.
000404     05  WS-ERMAIL-ACCOUNT       PIC X(10).
000405     05  WS-ERMAIL-EFF-DT        PIC XX.
000406     05  WS-ERMAIL-CERT-NO       PIC X(11).
000407
000408 01  WS-ERACCT-KEY.
000409     05  WS-ERACCT-COMPANY-CD    PIC X.
000410     05  WS-ERACCT-CARRIER       PIC X.
000411     05  WS-ERACCT-GROUP         PIC X(6).
000412     05  WS-ERACCT-STATE         PIC XX.
000413     05  WS-ERACCT-ACCOUNT       PIC X(10).
000414     05  WS-ERACCT-EXP-DT        PIC XX.
000415     05  FILLER                  PIC XXXX.
000416 01  WS-ELLETR-KEY.
000417     05  WS-ELLETR-COMPANY-CD    PIC X.
000418     05  WS-ELLETR-LETTER-ID     PIC X(12).
000419     05  WS-ELLETR-SEQ-NO        PIC S9(4) COMP VALUE +0.
000420 01  WS-ELCNTL-KEY.
000421     05  WS-ELCNTL-COMPANY-ID    PIC XXX.
000422     05  WS-ELCNTL-REC-TYPE      PIC X.
000423     05  WS-ELCNTL-GENL.
000424         10  WS-ELCNTL-STATE     PIC XX  VALUE SPACES.
000425         10  WS-ELCNTL-BEN-CD.
000426             15  F               PIC X.
000427             15  WS-ELCNTL-CARR  PIC X.
000428     05  WS-ELCNTL-SEQ-NO        PIC S9(4) COMP.
000429
000430*                                COPY ELCDATE.
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
000431
000432*                                COPY NSCASEXTR.
      *>>((file: NSCASEXTR))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            NSCASEXTR.                          *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = NAPERSOFT ADMIN SERVICES EXTRACT FILE     *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 4500 RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = NSASEXTR                       RKP=0,LEN=07   *
000013*                                                                *
000014*   LOG = NO                                                     *
000015*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000016******************************************************************
000017*-----------------------------------------------------------------
000018*                   C H A N G E   L O G
000019*
000020* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000021*-----------------------------------------------------------------
000022*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000023* EFFECTIVE    NUMBER
000024*-----------------------------------------------------------------
000025* 072211    2011022800001  PEMA  NEW FILE
000026*-----------------------------------------------------------------
000027 01 NSAS-EXTRACT-RECORD.
000028    05  NSAS-CONTROL-PRIMARY.
000029        10  NSAS-COMPANY-CD      PIC X.
000030        10  NSAS-ARCHIVE-NO      PIC 9(8) BINARY.
000031        10  NSAS-SEQ-NO          PIC 9(4) BINARY.
000032    05  NSAS-LETTER-VARIABLES    PIC X(4350).
000033    05  FILLER                   PIC X(143).
      *<<((file: NSCASEXTR))
000433*                                COPY ERCPNDB.
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
000434*                                COPY ERCENDT.
      *>>((file: ERCENDT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCENDT.                            *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = ENDORSEMENT FILE                          *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 579  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ERENDT                         RKP=02,LEN=36  *
000013*       ALTERNATE PATH1 = ERENDT2 (BY ARCH NO)    RKP=38,LEN=05  *
000014*                                                                *
000015*   LOG = NO                                                     *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017******************************************************************
000018*-----------------------------------------------------------------
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 052307    2006052600001  AJRA  ADDED FLAG FOR CANCELS ON CERTS
000027*                                WITH OPEN CLAIMS
000028* 072312    2011022800001  AJRA  ADDED BATCH NUMBER
000029* 110612    2012101700002  AJRA  ADD NEW FIELDS
000030* 121812  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
000031* 010616  CR2015082000001  PEMA  ADD ENDORSEMENT CHECK PROCESSING
000032*-----------------------------------------------------------------
000033
000034 01  ENDORSEMENT-RECORD.
000035     12  EN-RECORD-ID                PIC XX.
000036         88  VALID-EN-ID                VALUE 'EN'.
000037
000038     12  EN-CONTROL-PRIMARY.
000039         16  EN-COMPANY-CD           PIC X.
000040         16  EN-CARRIER              PIC X.
000041         16  EN-GROUPING             PIC X(6).
000042         16  EN-STATE                PIC XX.
000043         16  EN-ACCOUNT              PIC X(10).
000044         16  EN-CERT-EFF-DT          PIC XX.
000045         16  EN-CERT-NO.
000046             20  EN-CERT-PRIME       PIC X(10).
000047             20  EN-CERT-SFX         PIC X.
000048         16  EN-REC-TYPE             PIC X.
000049             88  EN-ISSUE               VALUE 'I'.
000050             88  EN-CANCELLATION        VALUE 'C'.
000051         16  EN-SEQ-NO               PIC 9(04) BINARY.
000052
000053     12  EN-CONTROL-BY-ARCH-NO.
000054         16  EN-COMPANY-CD-A1              PIC X.
000055         16  EN-ARCHIVE-NO                 PIC 9(8) BINARY.
000056
000057     12  EN-LAST-MAINT-DT            PIC XX.
000058     12  EN-LAST-MAINT-BY            PIC X(4).
000059     12  EN-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
000060
000061     12  EN-ENDORSEMENT-RECORD       PIC X(329).
000062
000063     12  EN-ISSUE-RECORD REDEFINES EN-ENDORSEMENT-RECORD.
000064         16  EN-INS-ORIG-LAST-NAME   PIC X(15).
000065         16  EN-INS-ORIG-FIRST-NAME  PIC X(10).
000066         16  EN-INS-ORIG-MIDDLE-INIT PIC X.
000067         16  EN-INS-ORIG-AGE         PIC S999     COMP-3.
000068         16  EN-JNT-ORIG-LAST-NAME   PIC X(15).
000069         16  EN-JNT-ORIG-FIRST-NAME  PIC X(10).
000070         16  EN-JNT-ORIG-MIDDLE-INIT PIC X.
000071         16  EN-JNT-ORIG-AGE         PIC S999     COMP-3.
000072         16  EN-LF-ORIG-BENCD        PIC XX.
000073         16  EN-LF-ORIG-TERM         PIC S999      COMP-3.
000074         16  EN-LF-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
000075         16  EN-LF-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
000076         16  EN-LF-ORIG-ALT-BEN-AMT  PIC S9(9)V99  COMP-3.
000077         16  EN-LF-ORIG-ALT-PRM-AMT  PIC S9(7)V99  COMP-3.
000078         16  EN-LF-ORIG-EXP-DT       PIC XX.
000079*        16  EN-LF-ORIG-COV-TYPE     PIC X(10).
000080         16  EN-ORIG-CRED-BENE       PIC X(25).
000081         16  EN-LF-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
000082         16  FILLER                  PIC X.
000083         16  EN-AH-ORIG-BENCD        PIC XX.
000084         16  EN-AH-ORIG-TERM         PIC S999      COMP-3.
000085         16  EN-AH-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
000086         16  EN-AH-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
000087         16  EN-AH-ORIG-EXP-DT       PIC XX.
000088*        16  EN-AH-ORIG-COV-TYPE     PIC X(10).
000089*        16  EN-AH-ORIG-WAIT-PER     PIC 99.
000090         16  EN-AH-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
000091         16  F                       PIC X(09).
000092         16  EN-AH-ORIG-CP           PIC 99.
000093
000094         16  EN-INS-NEW-LAST-NAME    PIC X(15).
000095         16  EN-INS-NEW-FIRST-NAME   PIC X(10).
000096         16  EN-INS-NEW-MIDDLE-INIT  PIC X.
000097         16  EN-INS-NEW-AGE          PIC S999     COMP-3.
000098         16  EN-JNT-NEW-LAST-NAME    PIC X(15).
000099         16  EN-JNT-NEW-FIRST-NAME   PIC X(10).
000100         16  EN-JNT-NEW-MIDDLE-INIT  PIC X.
000101         16  EN-JNT-NEW-AGE          PIC S999     COMP-3.
000102         16  EN-LF-NEW-BENCD         PIC XX.
000103         16  EN-LF-NEW-TERM          PIC S999      COMP-3.
000104         16  EN-LF-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
000105         16  EN-LF-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
000106         16  EN-LF-NEW-ALT-BEN-AMT   PIC S9(9)V99  COMP-3.
000107         16  EN-LF-NEW-ALT-PRM-AMT   PIC S9(7)V99  COMP-3.
000108         16  EN-LF-NEW-EXP-DT        PIC XX.
000109         16  EN-NEW-CRED-BENE        PIC X(25).
000110         16  EN-LF-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
000111         16  FILLER                  PIC X.
000112         16  EN-AH-NEW-BENCD         PIC XX.
000113         16  EN-AH-NEW-TERM          PIC S999      COMP-3.
000114         16  EN-AH-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
000115         16  EN-AH-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
000116         16  EN-AH-NEW-EXP-DT        PIC XX.
000117*        16  EN-AH-NEW-COV-TYPE      PIC X(10).
000118*        16  EN-AH-NEW-WAIT-PER      PIC 99.
000119*        16  F                       PIC X(12).
000120         16  EN-AH-NEW-CP            PIC 99.
000121         16  EN-SIG-SW               PIC X.
000122         16  EN-AH-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
000123         16  EN-INS-ORIG-AGE-DEF-FLAG PIC X.
000124         16  EN-JNT-ORIG-AGE-DEF-FLAG PIC X.
000125         16  EN-INS-NEW-AGE-DEF-FLAG PIC X.
000126         16  EN-JNT-NEW-AGE-DEF-FLAG PIC X.
000127         16  FILLER                  PIC X(33).
000128     12  EN-CANCEL-RECORD REDEFINES EN-ENDORSEMENT-RECORD.
000129         16  EN-LF-ORIG-REF-DT       PIC XX.
000130         16  EN-LF-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
000131         16  EN-AH-ORIG-REF-DT       PIC XX.
000132         16  EN-AH-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
000133         16  EN-LF-NEW-REF-DT        PIC XX.
000134         16  EN-LF-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
000135         16  EN-AH-NEW-REF-DT        PIC XX.
000136         16  EN-AH-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
000137         16  EN-FLAG-CERT            PIC X.
000138         16  EN-INS-LAST-NAME        PIC X(15).
000139         16  EN-INS-FIRST-NAME       PIC X(10).
000140         16  EN-INS-MIDDLE-INIT      PIC X.
000141         16  EN-LF-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
000142         16  EN-AH-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
000143         16  EN-LF-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
000144         16  EN-AH-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
000145         16  FILLER                  PIC X(262).
000146
000147     12  EN-MONEY-SW             PIC X.
000148     12  EN-HEALTH-APP           PIC X.
000149     12  EN-VOUCHER-SW           PIC X.
000150     12  EN-PAYEE                PIC X(14).
000151     12  EN-INPUT-DT             PIC XX.
000152     12  EN-PROCESS-DT           PIC XX.
000153     12  EN-LF-COMMISSION        PIC SV9(5)    COMP-3.
000154     12  EN-AH-COMMISSION        PIC SV9(5)    COMP-3.
000155
000156     12  EN-REASON-CODES.
000157         16  F OCCURS 12.
000158             20  EN-REASON-CODE  PIC X(4).
000159     12  EN-TEMPLATE-USED        PIC X(8).
000160     12  EN-DOCU-TYPE            PIC X.
000161         88  EN-VERI-DOCU          VALUE 'V'.
000162         88  EN-GCE-DOCU           VALUE 'G'.
000163         88  EN-CANC-DOCU          VALUE 'C'.
000164     12  EN-COMMENTS1            PIC X(13).
000165     12  EN-COMMENTS2            PIC X(70).
000166     12  EN-COMM-CHGBK           PIC X.
000167         88  EN-DO-NOT-CHG-ACCT    VALUE 'N'.
000168         88  EN-CHG-ACCT           VALUE 'Y'.
000169     12  EN-CSO-PORTION          PIC S9(5)V99  COMP-3.
000170     12  EN-ACCT-PORTION         PIC S9(5)V99  COMP-3.
000171     12  EN-BATCH-NUMBER         PIC X(6).
000172     12  EN-ACCT-SUMM            PIC X.
000173     12  EN-CSO-SUMM             PIC X.
000174     12  en-check-type           pic x.
000175     12  FILLER                  PIC X(12).
000176
000177******************************************************************
000178
      *<<((file: ERCENDT))
000435*                                COPY ERCMAIL.
      *>>((file: ERCMAIL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCMAIL                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 374   RECFORM = FIX                            *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
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
000027* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000028* 111108                   PEMA  ADD CRED BENE ADDR2
000029******************************************************************
000030
000031 01  MAILING-DATA.
000032     12  MA-RECORD-ID                      PIC XX.
000033         88  VALID-MA-ID                       VALUE 'MA'.
000034
000035     12  MA-CONTROL-PRIMARY.
000036         16  MA-COMPANY-CD                 PIC X.
000037         16  MA-CARRIER                    PIC X.
000038         16  MA-GROUPING.
000039             20  MA-GROUPING-PREFIX        PIC XXX.
000040             20  MA-GROUPING-PRIME         PIC XXX.
000041         16  MA-STATE                      PIC XX.
000042         16  MA-ACCOUNT.
000043             20  MA-ACCOUNT-PREFIX         PIC X(4).
000044             20  MA-ACCOUNT-PRIME          PIC X(6).
000045         16  MA-CERT-EFF-DT                PIC XX.
000046         16  MA-CERT-NO.
000047             20  MA-CERT-PRIME             PIC X(10).
000048             20  MA-CERT-SFX               PIC X.
000049
000050     12  FILLER                            PIC XX.
000051
000052     12  MA-ACCESS-CONTROL.
000053         16  MA-SOURCE-SYSTEM              PIC XX.
000054             88  MA-FROM-CREDIT                VALUE 'CR'.
000055             88  MA-FROM-VSI                   VALUE 'VS'.
000056             88  MA-FROM-WARRANTY              VALUE 'WA'.
000057             88  MA-FROM-OTHER                 VALUE 'OT'.
000058         16  MA-RECORD-ADD-DT              PIC XX.
000059         16  MA-RECORD-ADDED-BY            PIC XXXX.
000060         16  MA-LAST-MAINT-DT              PIC XX.
000061         16  MA-LAST-MAINT-BY              PIC XXXX.
000062         16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
000063
000064     12  MA-PROFILE-INFO.
000065         16  MA-QUALIFY-CODE-1             PIC XX.
000066         16  MA-QUALIFY-CODE-2             PIC XX.
000067         16  MA-QUALIFY-CODE-3             PIC XX.
000068         16  MA-QUALIFY-CODE-4             PIC XX.
000069         16  MA-QUALIFY-CODE-5             PIC XX.
000070
000071         16  MA-INSURED-LAST-NAME          PIC X(15).
000072         16  MA-INSURED-FIRST-NAME         PIC X(10).
000073         16  MA-INSURED-MIDDLE-INIT        PIC X.
000074         16  MA-INSURED-ISSUE-AGE          PIC 99.
000075         16  MA-INSURED-BIRTH-DT           PIC XX.
000076         16  MA-INSURED-SEX                PIC X.
000077             88  MA-SEX-MALE                   VALUE 'M'.
000078             88  MA-SEX-FEMALE                 VALUE 'F'.
000079         16  MA-INSURED-SOC-SEC-NO         PIC X(11).
000080
000081         16  MA-ADDRESS-CORRECTED          PIC X.
000082         16  MA-JOINT-BIRTH-DT             PIC XX.
000083*        16  FILLER                        PIC X(12).
000084
000085         16  MA-ADDRESS-LINE-1             PIC X(30).
000086         16  MA-ADDRESS-LINE-2             PIC X(30).
000087         16  MA-CITY-STATE.
000088             20  MA-CITY                   PIC X(28).
000089             20  MA-ADDR-STATE             PIC XX.
000090         16  MA-ZIP.
000091             20  MA-ZIP-CODE.
000092                 24  MA-ZIP-CODE-1ST       PIC X(1).
000093                     88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000094                 24  FILLER                PIC X(4).
000095             20  MA-ZIP-PLUS4              PIC X(4).
000096         16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
000097             20  MA-CAN-POSTAL-CODE-1      PIC X(3).
000098             20  MA-CAN-POSTAL-CODE-2      PIC X(3).
000099             20  FILLER                    PIC X(3).
000100
000101         16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
000102
000103         16  FILLER                        PIC XXX.
000104*        16  FILLER                        PIC X(10).
000105
000106
000107     12  MA-CRED-BENE-INFO.
000108         16  MA-CRED-BENE-NAME                 PIC X(25).
000109         16  MA-CRED-BENE-ADDR                 PIC X(30).
000110         16  MA-CRED-BENE-ADDR2                PIC X(30).
000111         16  MA-CRED-BENE-CTYST.
000112             20  MA-CRED-BENE-CITY             PIC X(28).
000113             20  MA-CRED-BENE-STATE            PIC XX.
000114         16  MA-CRED-BENE-ZIP.
000115             20  MA-CB-ZIP-CODE.
000116                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
000117                     88  MA-CB-CANADIAN-POST-CODE
000118                                           VALUE 'A' THRU 'Z'.
000119                 24  FILLER                    PIC X(4).
000120             20  MA-CB-ZIP-PLUS4               PIC X(4).
000121         16  MA-CB-CANADIAN-POSTAL-CODE
000122                            REDEFINES MA-CRED-BENE-ZIP.
000123             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
000124             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
000125             20  FILLER                        PIC X(3).
000126     12  MA-POST-CARD-MAIL-DATA.
000127         16  MA-MAIL-DATA OCCURS 7.
000128             20  MA-MAIL-TYPE              PIC X.
000129                 88  MA-12MO-MAILING           VALUE '1'.
000130                 88  MA-EXP-MAILING            VALUE '2'.
000131             20  MA-MAIL-STATUS            PIC X.
000132                 88  MA-MAIL-ST-MAILED         VALUE '1'.
000133                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
000134                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
000135             20  MA-MAIL-DATE              PIC XX.
000136     12  FILLER                            PIC XX.
000137     12  FILLER                            PIC XX.
000138*    12  FILLER                            PIC X(30).
000139******************************************************************
      *<<((file: ERCMAIL))
000436*                                COPY ERCCOMP.
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
000437*                                COPY ELCCERT.
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
000438*                                COPY ELCCRTO.
      *>>((file: ELCCRTO))
000001******************************************************************
000002*                                                                *
000003*                            ELCCRTO.                            *
000004*                                                                *
000005*   FILE DESCRIPTION = ORIGINAL CERTIFICATE INFORMATION          *
000006*                                                                *
000007*   FILE TYPE = VSAM,KSDS                                        *
000008*   RECORD SIZE = 524  RECFORM = FIXED                           *
000009*                                                                *
000010*   BASE CLUSTER = ELCRTO                         RKP=2,LEN=36   *
000011*                                                                *
000012*   LOG = YES                                                    *
000013*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000014******************************************************************
000015*                   C H A N G E   L O G
000016*
000017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000018*-----------------------------------------------------------------
000019*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000020* EFFECTIVE    NUMBER
000021*-----------------------------------------------------------------
000022* 061011  2011022800001    PEMA  NEW FILE TO SAVE ORIG CERT INFO
000023* 062712  2011022800001    AJRA  REDEFINE ORIG DATA
000024* 071712  CR2011022800001  AJRA  NAPERSOFT CANCELS
000025* 121812  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
000026* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
000027******************************************************************
000028
000029 01  ORIGINAL-CERTIFICATE.
000030     12  OC-RECORD-ID                      PIC XX.
000031         88  VALID-OC-ID                      VALUE 'OC'.
000032
000033     12  OC-CONTROL-PRIMARY.
000034         16  OC-COMPANY-CD                 PIC X.
000035         16  OC-CARRIER                    PIC X.
000036         16  OC-GROUPING                   PIC X(6).
000037         16  OC-STATE                      PIC XX.
000038         16  OC-ACCOUNT                    PIC X(10).
000039         16  OC-CERT-EFF-DT                PIC XX.
000040         16  OC-CERT-NO.
000041             20  OC-CERT-PRIME             PIC X(10).
000042             20  OC-CERT-SFX               PIC X.
000043         16  OC-RECORD-TYPE                PIC X.
000044         16  OC-KEY-SEQ-NO                 PIC 9(4) BINARY.
000045
000046     12  OC-LAST-MAINT-DT                  PIC XX.
000047     12  OC-LAST-MAINT-BY                  PIC X(4).
000048     12  OC-LAST-MAINT-HHMMSS              PIC S9(6)   COMP-3.
000049
000050     12  OC-ENDORSEMENT-PROCESSED-DT       PIC XX.
000051     12  FILLER                            PIC X(49).
000052
000053     12  OC-ORIG-REC.
000054         16  OC-INS-LAST-NAME              PIC X(15).
000055         16  OC-INS-FIRST-NAME             PIC X(10).
000056         16  OC-INS-MIDDLE-INIT            PIC X.
000057         16  OC-INS-AGE                    PIC S999     COMP-3.
000058         16  OC-JNT-LAST-NAME              PIC X(15).
000059         16  OC-JNT-FIRST-NAME             PIC X(10).
000060         16  OC-JNT-MIDDLE-INIT            PIC X.
000061         16  OC-JNT-AGE                    PIC S999     COMP-3.
000062         16  OC-LF-BENCD                   PIC XX.
000063         16  OC-LF-TERM                    PIC S999      COMP-3.
000064         16  OC-LF-BEN-AMT                 PIC S9(9)V99  COMP-3.
000065         16  OC-LF-PRM-AMT                 PIC S9(7)V99  COMP-3.
000066         16  OC-LF-ALT-BEN-AMT             PIC S9(9)V99  COMP-3.
000067         16  OC-LF-ALT-PRM-AMT             PIC S9(7)V99  COMP-3.
000068         16  OC-LF-EXP-DT                  PIC XX.
000069         16  OC-LF-COMM-PCT                PIC SV9(5)    COMP-3.
000070         16  OC-LF-CANCEL-DT               PIC XX.
000071         16  OC-LF-CANCEL-AMT              PIC S9(7)V99  COMP-3.
000072         16  OC-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000073         16  OC-AH-BENCD                   PIC XX.
000074         16  OC-AH-TERM                    PIC S999      COMP-3.
000075         16  OC-AH-BEN-AMT                 PIC S9(9)V99  COMP-3.
000076         16  OC-AH-PRM-AMT                 PIC S9(7)V99  COMP-3.
000077         16  OC-AH-EXP-DT                  PIC XX.
000078         16  OC-AH-COMM-PCT                PIC SV9(5)    COMP-3.
000079         16  OC-AH-CP                      PIC 99.
000080         16  OC-AH-CANCEL-DT               PIC XX.
000081         16  OC-AH-CANCEL-AMT              PIC S9(7)V99  COMP-3.
000082         16  OC-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000083         16  OC-CRED-BENE-NAME             PIC X(25).
000084         16  OC-1ST-PMT-DT                 PIC XX.
000085         16  OC-INS-AGE-DEFAULT-FLAG       PIC X.
000086         16  OC-JNT-AGE-DEFAULT-FLAG       PIC X.
000087         16  OC-ISSUE-TRAN-IND             PIC X.
000088         16  OC-CANCEL-TRAN-IND            PIC X.
000089         16  FILLER                        PIC X(211).
000090
000091     12  FILLER                            PIC X(50).
000092
000093******************************************************************
      *<<((file: ELCCRTO))
000439*                                COPY ERCACCT.
      *>>((file: ERCACCT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCACCT                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.031                          *
000007*                                                                *
000008*   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
000009*                                                                *
000010*   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
000011*   VSAM ACCOUNT MASTER FILES.                                   *
000012*                                                                *
000013*   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
000014*                                                                *
000015*   FILE TYPE = VSAM,KSDS                                        *
000016*   RECORD SIZE = 2000  RECFORM = FIX                            *
000017*                                                                *
000018*   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
000019*       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
000020*                                                                *
000021*   LOG = NO                                                     *
000022*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000023*                                                                *
000024*                                                                *
000025******************************************************************
000026*                   C H A N G E   L O G
000027*
000028* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000029*-----------------------------------------------------------------
000030*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000031* EFFECTIVE    NUMBER
000032*-----------------------------------------------------------------
000033* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
000034* 092705    2005050300006  PEMA  ADD SPP LEASES
000035* 022808    2007083100002  PEMA  ADD FREEZE STATUS
000036* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000037* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
000038* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
000039* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
000040* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
000041******************************************************************
000042
000043 01  ACCOUNT-MASTER.
000044     12  AM-RECORD-ID                      PIC XX.
000045         88  VALID-AM-ID                      VALUE 'AM'.
000046
000047     12  AM-CONTROL-PRIMARY.
000048         16  AM-COMPANY-CD                 PIC X.
000049         16  AM-MSTR-CNTRL.
000050             20  AM-CONTROL-A.
000051                 24  AM-CARRIER            PIC X.
000052                 24  AM-GROUPING.
000053                     28 AM-GROUPING-PREFIX PIC XXX.
000054                     28 AM-GROUPING-PRIME  PIC XXX.
000055                 24  AM-STATE              PIC XX.
000056                 24  AM-ACCOUNT.
000057                     28  AM-ACCOUNT-PREFIX PIC X(4).
000058                     28  AM-ACCOUNT-PRIME  PIC X(6).
000059             20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
000060                                           PIC X(19).
000061             20  AM-CNTRL-B.
000062                 24  AM-EXPIRATION-DT      PIC XX.
000063                 24  FILLER                PIC X(4).
000064             20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
000065                 24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
000066
000067     12  AM-CONTROL-BY-VAR-GRP.
000068         16  AM-COMPANY-CD-A1              PIC X.
000069         16  AM-VG-CARRIER                 PIC X.
000070         16  AM-VG-GROUPING                PIC X(6).
000071         16  AM-VG-STATE                   PIC XX.
000072         16  AM-VG-ACCOUNT                 PIC X(10).
000073         16  AM-VG-DATE.
000074             20  AM-VG-EXPIRATION-DT       PIC XX.
000075             20  FILLER                    PIC X(4).
000076         16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
000077                                           PIC 9(11)      COMP-3.
000078     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
000079         16  FILLER                        PIC X(10).
000080         16  AM-VG-KEY3.
000081             20  AM-VG3-ACCOUNT            PIC X(10).
000082             20  AM-VG3-EXP-DT             PIC XX.
000083         16  FILLER                        PIC X(4).
000084     12  AM-MAINT-INFORMATION.
000085         16  AM-LAST-MAINT-DT              PIC XX.
000086         16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000087         16  AM-LAST-MAINT-USER            PIC X(4).
000088         16  FILLER                        PIC XX.
000089
000090     12  AM-EFFECTIVE-DT                   PIC XX.
000091     12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
000092
000093     12  AM-PREV-DATES  COMP-3.
000094         16  AM-PREV-EXP-DT                PIC 9(11).
000095         16  AM-PREV-EFF-DT                PIC 9(11).
000096
000097     12  AM-REPORT-CODE-1                  PIC X(10).
000098     12  AM-REPORT-CODE-2                  PIC X(10).
000099
000100     12  AM-CITY-CODE                      PIC X(4).
000101     12  AM-COUNTY-PARISH                  PIC X(6).
000102
000103     12  AM-NAME                           PIC X(30).
000104     12  AM-PERSON                         PIC X(30).
000105     12  AM-ADDRS                          PIC X(30).
000106     12  AM-CITY.
000107         16  AM-ADDR-CITY                  PIC X(28).
000108         16  AM-ADDR-STATE                 PIC XX.
000109     12  AM-ZIP.
000110         16  AM-ZIP-PRIME.
000111             20  AM-ZIP-PRI-1ST            PIC X.
000112                 88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
000113             20  FILLER                    PIC X(4).
000114         16  AM-ZIP-PLUS4                  PIC X(4).
000115     12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
000116         16  AM-CAN-POSTAL-1               PIC XXX.
000117         16  AM-CAN-POSTAL-2               PIC XXX.
000118         16  FILLER                        PIC XXX.
000119     12  AM-TEL-NO.
000120         16  AM-AREA-CODE                  PIC 999.
000121         16  AM-TEL-PRE                    PIC 999.
000122         16  AM-TEL-NBR                    PIC 9(4).
000123     12  AM-TEL-LOC                        PIC X.
000124         88  AM-TEL-AT-HOME                   VALUE 'H'.
000125         88  AM-TEL-AT-BUSINESS               VALUE 'B'.
000126
000127     12  AM-COMM-STRUCTURE.
000128         16  AM-DEFN-1.
000129             20  AM-AGT-COMMS       OCCURS 10 TIMES.
000130                 24  AM-AGT.
000131                     28  AM-AGT-PREFIX     PIC X(4).
000132                     28  AM-AGT-PRIME      PIC X(6).
000133                 24  AM-COM-TYP            PIC X.
000134                 24  AM-L-COM              PIC SV9(5)     COMP-3.
000135                 24  AM-J-COM              PIC SV9(5)     COMP-3.
000136                 24  AM-A-COM              PIC SV9(5)     COMP-3.
000137                 24  AM-RECALC-LV-INDIC    PIC X.
000138                 24  AM-RETRO-LV-INDIC     PIC X.
000139                 24  AM-GL-CODES           PIC X.
000140                 24  AM-COMM-CHARGEBACK    PIC 9(02).
000141                 24  FILLER                PIC X(01).
000142         16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
000143             20  AM-COM-TBLS        OCCURS 10 TIMES.
000144                 24  FILLER                PIC X(11).
000145                 24  AM-L-COMA             PIC XXX.
000146                 24  AM-J-COMA             PIC XXX.
000147                 24  AM-A-COMA             PIC XXX.
000148                 24  FILLER                PIC X(6).
000149
000150     12  AM-COMM-CHANGE-STATUS             PIC X.
000151         88  AM-COMMISSIONS-CHANGED           VALUE '*'.
000152
000153     12  AM-CSR-CODE                       PIC X(4).
000154
000155     12  AM-BILLING-STATUS                 PIC X.
000156         88  AM-ACCOUNT-BILLED                VALUE 'B'.
000157         88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
000158     12  AM-AUTO-REFUND-SW                 PIC X.
000159         88  AUTO-REFUNDS-USED                VALUE 'Y'.
000160         88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
000161     12  AM-GPCD                           PIC 99.
000162     12  AM-IG                             PIC X.
000163         88  AM-HAS-INDIVIDUAL                VALUE '1'.
000164         88  AM-HAS-GROUP                     VALUE '2'.
000165     12  AM-STATUS                         PIC X.
000166         88  AM-ACCOUNT-ACTIVE                VALUE '0'.
000167         88  AM-ACCOUNT-INACTIVE              VALUE '1'.
000168         88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
000169         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
000170         88  AM-ACCOUNT-FROZEN                VALUE '4'.
000171         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
000172         88  AM-ACCOUNT-DROPPED               VALUE '6'.
000173         88  AM-ACCOUNT-LAPSED                VALUE '7'.
000174         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
000175         88  AM-ACCOUNT-PENDING               VALUE '9'.
000176     12  AM-REMIT-TO                       PIC 99.
000177     12  AM-ID-NO                          PIC X(11).
000178
000179     12  AM-CAL-TABLE                      PIC XX.
000180     12  AM-LF-DEVIATION                   PIC XXX.
000181     12  AM-AH-DEVIATION                   PIC XXX.
000182     12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
000183     12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
000184     12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
000185     12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
000186     12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
000187     12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
000188
000189     12  AM-USER-FIELDS.
000190         16  AM-FLD-1                      PIC XX.
000191         16  AM-FLD-2                      PIC XX.
000192         16  AM-FLD-3                      PIC XX.
000193         16  AM-FLD-4                      PIC XX.
000194         16  AM-FLD-5                      PIC XX.
000195
000196     12  AM-1ST-PROD-DATE.
000197         16  AM-1ST-PROD-YR                PIC XX.
000198         16  AM-1ST-PROD-MO                PIC XX.
000199         16  AM-1ST-PROD-DA                PIC XX.
000200     12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
000201     12  AM-CERTS-PURGED-DATE.
000202         16  AM-PUR-YR                     PIC XX.
000203         16  AM-PUR-MO                     PIC XX.
000204         16  AM-PUR-DA                     PIC XX.
000205     12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
000206     12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
000207     12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
000208     12  AM-INACTIVE-DATE.
000209         16  AM-INA-MO                     PIC 99.
000210         16  AM-INA-DA                     PIC 99.
000211         16  AM-INA-YR                     PIC 99.
000212     12  AM-AR-HI-CERT-DATE                PIC XX.
000213
000214     12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
000215     12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
000216
000217     12  AM-OB-PAYMENT-MODE                PIC X.
000218         88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
000219         88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
000220         88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
000221         88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
000222
000223     12  AM-AH-ONLY-INDICATOR              PIC X.
000224         88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
000225         88  AM-NO-AH-ONLY                    VALUE 'N'.
000226
000227     12  AM-EDIT-LOAN-OFC                  PIC X(01).
000228
000229     12  AM-OVER-SHORT.
000230         16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
000231         16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
000232
000233     12  AM-DCC-PRODUCT-CODE               PIC XXX.
000234     12  AM-DCC-CLP-STATE                  PIC XX.
000235
000236     12  AM-RECALC-COMM                    PIC X.
000237     12  AM-RECALC-REIN                    PIC X.
000238
000239     12  AM-REI-TABLE                      PIC XXX.
000240     12  AM-REI-ET-LF                      PIC X.
000241     12  AM-REI-ET-AH                      PIC X.
000242     12  AM-REI-PE-LF                      PIC X.
000243     12  AM-REI-PE-AH                      PIC X.
000244     12  AM-REI-PRT-ST                     PIC X.
000245     12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
000246     12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
000247     12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
000248     12  AM-REI-GROUP-A                    PIC X(6).
000249     12  AM-REI-MORT                       PIC X(4).
000250     12  AM-REI-PRT-OW                     PIC X.
000251     12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
000252     12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
000253     12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
000254     12  AM-REI-GROUP-B                    PIC X(6).
000255
000256     12  AM-TRUST-TYPE                     PIC X(2).
000257
000258     12  AM-EMPLOYER-STMT-USED             PIC X.
000259     12  AM-GROUPED-CHECKS-Y-N             PIC X.
000260
000261     12  AM-STD-AH-TYPE                    PIC XX.
000262     12  AM-EARN-METHODS.
000263         16  AM-EARN-METHOD-R              PIC X.
000264             88 AM-REF-RL-R78                 VALUE 'R'.
000265             88 AM-REF-RL-PR                  VALUE 'P'.
000266             88 AM-REF-RL-MEAN                VALUE 'M'.
000267             88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
000268         16  AM-EARN-METHOD-L              PIC X.
000269             88 AM-REF-LL-R78                 VALUE 'R'.
000270             88 AM-REF-LL-PR                  VALUE 'P'.
000271             88 AM-REF-LL-MEAN                VALUE 'M'.
000272             88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
000273         16  AM-EARN-METHOD-A              PIC X.
000274             88 AM-REF-AH-R78                 VALUE 'R'.
000275             88 AM-REF-AH-PR                  VALUE 'P'.
000276             88 AM-REF-AH-MEAN                VALUE 'M'.
000277             88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
000278             88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
000279             88 AM-REF-AH-NET                 VALUE 'N'.
000280
000281     12  AM-TOL-PREM                       PIC S999V99    COMP-3.
000282     12  AM-TOL-REF                        PIC S999V99    COMP-3.
000283     12  AM-TOL-CLM                        PIC S999V99    COMP-3.
000284
000285     12  AM-RET-Y-N                        PIC X.
000286     12  AM-RET-P-E                        PIC X.
000287     12  AM-LF-RET                         PIC S9V9999    COMP-3.
000288     12  AM-AH-RET                         PIC S9V9999    COMP-3.
000289     12  AM-RET-GRP                        PIC X(6).
000290     12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
000291         16  AM-POOL-PRIME                 PIC XXX.
000292         16  AM-POOL-SUB                   PIC XXX.
000293     12  AM-RETRO-EARNINGS.
000294         16  AM-RET-EARN-R                 PIC X.
000295         16  AM-RET-EARN-L                 PIC X.
000296         16  AM-RET-EARN-A                 PIC X.
000297     12  AM-RET-ST-TAX-USE                 PIC X.
000298         88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
000299         88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
000300     12  AM-RETRO-BEG-EARNINGS.
000301         16  AM-RET-BEG-EARN-R             PIC X.
000302         16  AM-RET-BEG-EARN-L             PIC X.
000303         16  AM-RET-BEG-EARN-A             PIC X.
000304     12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
000305     12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
000306
000307     12  AM-USER-SELECT-OPTIONS.
000308         16  AM-USER-SELECT-1              PIC X(10).
000309         16  AM-USER-SELECT-2              PIC X(10).
000310         16  AM-USER-SELECT-3              PIC X(10).
000311         16  AM-USER-SELECT-4              PIC X(10).
000312         16  AM-USER-SELECT-5              PIC X(10).
000313
000314     12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
000315
000316     12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
000317
000318     12  AM-RPT045A-SWITCH                 PIC X.
000319         88  RPT045A-OFF                   VALUE 'N'.
000320
000321     12  AM-INSURANCE-LIMITS.
000322         16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
000323         16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
000324
000325     12  AM-PROFILE-CHANGE-SWITCH          PIC X.
000326         88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
000327
000328     12  AM-DISMBR-COVERAGE-SW             PIC X.
000329         88  AM-DISMBR-COVERAGE               VALUE 'Y'.
000330         88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
000331
000332     12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
000333
000334     12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
000335     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
000336     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
000337     12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
000338     12  AM-DCC-UEF-STATE                  PIC XX.
000339     12  FILLER                            PIC XXX.
000340     12  AM-REPORT-CODE-3                  PIC X(10).
000341*    12  FILLER                            PIC X(22).
000342
000343     12  AM-RESERVE-DATE.
000344         16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
000345         16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
000346         16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
000347
000348     12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
000349     12  AM-NOTIFICATION-TYPES.
000350         16  AM-NOTIF-OF-LETTERS           PIC X.
000351         16  AM-NOTIF-OF-PAYMENTS          PIC X.
000352         16  AM-NOTIF-OF-REPORTS           PIC X.
000353         16  AM-NOTIF-OF-STATUS            PIC X.
000354
000355     12  AM-BENEFIT-TABLE-USAGE            PIC X.
000356         88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
000357         88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
000358         88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
000359         88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
000360
000361     12  AM-BENEFIT-CONTROLS.
000362         16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
000363             20  AM-BENEFIT-CODE           PIC XX.
000364             20  AM-BENEFIT-TYPE           PIC X.
000365             20  AM-BENEFIT-REVISION       PIC XXX.
000366             20  AM-BENEFIT-REM-TERM       PIC X.
000367             20  AM-BENEFIT-RETRO-Y-N      PIC X.
000368             20  FILLER                    PIC XX.
000369         16  FILLER                        PIC X(80).
000370
000371     12  AM-TRANSFER-DATA.
000372         16  AM-TRANSFERRED-FROM.
000373             20  AM-TRNFROM-CARRIER        PIC X.
000374             20  AM-TRNFROM-GROUPING.
000375                 24  AM-TRNFROM-GRP-PREFIX PIC XXX.
000376                 24  AM-TRNFROM-GRP-PRIME  PIC XXX.
000377             20  AM-TRNFROM-STATE          PIC XX.
000378             20  AM-TRNFROM-ACCOUNT.
000379                 24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
000380                 24  AM-TRNFROM-ACCT-PRIME PIC X(6).
000381             20  AM-TRNFROM-DTE            PIC XX.
000382         16  AM-TRANSFERRED-TO.
000383             20  AM-TRNTO-CARRIER          PIC X.
000384             20  AM-TRNTO-GROUPING.
000385                 24  AM-TRNTO-GRP-PREFIX   PIC XXX.
000386                 24  AM-TRNTO-GRP-PRIME    PIC XXX.
000387             20  AM-TRNTO-STATE            PIC XX.
000388             20  AM-TRNTO-ACCOUNT.
000389                 24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
000390                 24  AM-TRNTO-ACCT-PRIME   PIC X(6).
000391             20  AM-TRNTO-DTE              PIC XX.
000392         16  FILLER                        PIC X(10).
000393
000394     12  AM-SAVED-REMIT-TO                 PIC 99.
000395
000396     12  AM-COMM-STRUCTURE-SAVED.
000397         16  AM-DEFN-1-SAVED.
000398             20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
000399                 24  AM-AGT-SV             PIC X(10).
000400                 24  AM-COM-TYP-SV         PIC X.
000401                 24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
000402                 24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
000403                 24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
000404                 24  AM-RECALC-LV-INDIC-SV PIC X.
000405                 24  FILLER                PIC X.
000406                 24  AM-GL-CODES-SV        PIC X.
000407                 24  AM-COM-CHARGEBACK-SV  PIC 99.
000408                 24  FILLER                PIC X.
000409         16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
000410             20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
000411                 24  FILLER                PIC X(11).
000412                 24  AM-L-COMA-SV          PIC XXX.
000413                 24  AM-J-COMA-SV          PIC XXX.
000414                 24  AM-A-COMA-SV          PIC XXX.
000415                 24  FILLER                PIC X(6).
000416
000417     12  AM-FLC-NET-PREMIUM-ALLOWANCE.
000418         16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
000419            20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
000420            20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
000421            20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
000422
000423     12  AM-ORIG-DEALER-NO                 PIC X(10).
000424     12  FILLER                            PIC X(120).
000425
000426     12  AM-ACCOUNT-EXECUTIVE-DATA.
000427         16  AM-CONTROL-NAME               PIC X(30).
000428         16  AM-EXECUTIVE-ONE.
000429             20  AM-EXEC1-NAME             PIC X(15).
000430             20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
000431                                                          COMP-3.
000432             20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
000433                                                          COMP-3.
000434         16  AM-EXECUTIVE-TWO.
000435             20  AM-EXEC2-NAME             PIC X(15).
000436             20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
000437                                                          COMP-3.
000438             20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
000439                                                          COMP-3.
000440
000441     12  AM-RETRO-ADDITIONAL-DATA.
000442         16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
000443         16  AM-RETRO-PREM-P-E             PIC X.
000444         16  AM-RETRO-CLMS-P-I             PIC X.
000445         16  AM-RETRO-RET-BRACKET-LF.
000446             20  AM-RETRO-RET-METHOD-LF    PIC X.
000447                 88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
000448                 88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
000449             20  AM-RETRO-RET-BASIS-LF     PIC X.
000450                 88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
000451                 88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
000452             20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
000453                 24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
000454                 24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
000455         16  AM-RETRO-RET-BRACKET-AH.
000456             20  AM-RETRO-RET-METHOD-AH    PIC X.
000457                 88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
000458                 88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
000459                 88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
000460             20  AM-RETRO-RET-BASIS-AH     PIC X.
000461                 88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
000462                 88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
000463             20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
000464                 24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
000465                 24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
000466
000467     12  AM-COMMENTS.
000468         16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
000469
000470     12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
000471         16  AM-FLI-RETRO-SHARE-CODE       PIC X.
000472         16  AM-FLI-BILLING-CODE           PIC X.
000473         16  AM-FLI-ALT-STATE-CODE         PIC XX.
000474         16  AM-FLI-UNITED-IDENT           PIC X.
000475         16  AM-FLI-INTEREST-LOST-DATA.
000476             20  AM-FLI-BANK-NO            PIC X(5).
000477             20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
000478             20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
000479             20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
000480         16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
000481             20  AM-FLI-AGT                PIC X(9).
000482             20  AM-FLI-AGT-COMM-ACC       PIC X.
000483             20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
000484         16  FILLER                        PIC X(102).
000485
000486     12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
000487         16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
000488             20  AM-BENEFIT-DMD-CODE         PIC XX.
000489             20  AM-BENEFIT-DMD-TYPE         PIC X.
000490             20  AM-BENEFIT-DMD-REVISION     PIC XXX.
000491             20  AM-BENEFIT-DMD-REM-TERM     PIC X.
000492             20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
000493         16  FILLER                          PIC X(10).
000494******************************************************************
      *<<((file: ERCACCT))
000440*                                COPY ELCTEXT.
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
000441*                                COPY ELCCNTL.
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
000442*                                COPY NSCASVARS.
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
000443*                                COPY ERCARCH.
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
000444*                                COPY ELCCRTT.
      *>>((file: ELCCRTT))
000001******************************************************************
000002*                                                                *
000003*                            ELCCRTT.                            *
000004*                                                                *
000005*   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
000006*                                                                *
000007*   FILE TYPE = VSAM,KSDS                                        *
000008*   RECORD SIZE = 552  RECFORM = FIXED                           *
000009*                                                                *
000010*   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
000011*                                                                *
000012*   LOG = YES                                                    *
000013*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000014******************************************************************
000015*                   C H A N G E   L O G
000016*
000017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000018*-----------------------------------------------------------------
000019*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000020* EFFECTIVE    NUMBER
000021*-----------------------------------------------------------------
000022* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
000023* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
000024* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
000025* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
000026* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
000027* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000028* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000029* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
000030* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
000031* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
000032* 091318  CR2018073000001  PEMA  ADD Refund methods
000033* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000034******************************************************************
000035
000036 01  CERTIFICATE-TRAILERS.
000037     12  CS-RECORD-ID                      PIC XX.
000038         88  VALID-CS-ID                      VALUE 'CS'.
000039
000040     12  CS-CONTROL-PRIMARY.
000041         16  CS-COMPANY-CD                 PIC X.
000042         16  CS-CARRIER                    PIC X.
000043         16  CS-GROUPING                   PIC X(6).
000044         16  CS-STATE                      PIC XX.
000045         16  CS-ACCOUNT                    PIC X(10).
000046         16  CS-CERT-EFF-DT                PIC XX.
000047         16  CS-CERT-NO.
000048             20  CS-CERT-PRIME             PIC X(10).
000049             20  CS-CERT-SFX               PIC X.
000050         16  CS-TRAILER-TYPE               PIC X.
000051             88  COMM-TRLR           VALUE 'A'.
000052             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
000053             88  CERT-DATA-TRLR      VALUE 'C'.
000054
000055     12  CS-DATA-AREA                      PIC X(516).
000056
000057     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
000058         16  CS-BANK-COMMISSION-AREA.
000059             20  CS-BANK-COMMS       OCCURS 10.
000060                 24  CS-AGT                PIC X(10).
000061                 24  CS-COM-TYP            PIC X.
000062                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
000063                 24  CS-RECALC-LV-INDIC    PIC X.
000064                 24  FILLER                PIC X(10).
000065
000066         16  FILLER                        PIC X(256).
000067
000068     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
000069****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
000070
000071         16  CS-MB-CLAIM-DATA OCCURS 24.
000072             20  CS-CLAIM-NO               PIC X(7).
000073             20  CS-CLAIM-TYPE             PIC X.
000074                 88  CS-AH-CLM               VALUE 'A'.
000075                 88  CS-IU-CLM               VALUE 'I'.
000076                 88  CS-GP-CLM               VALUE 'G'.
000077                 88  CS-LF-CLM               VALUE 'L'.
000078                 88  CS-PR-CLM               VALUE 'P'.
000079                 88  CS-FL-CLM               VALUE 'F'.
000080                 88  CS-OT-CLM               VALUE 'O'.
000081             20  CS-INSURED-TYPE           PIC X.
000082                 88  CS-PRIM-INSURED          VALUE 'P'.
000083                 88  CS-CO-BORROWER           VALUE 'C'.
000084             20  CS-BENEFIT-PERIOD         PIC 99.
000085             20  CS-DAYS-PAID              PIC S9(5) COMP-3.
000086             20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
000087             20  CS-REMAINING-BENS         PIC S999 COMP-3.
000088         16  FILLER                        PIC X(12).
000089
000090     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
000091         16  CS-VIN-NUMBER                 PIC X(17).
000092         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
000093         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
000094         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
000095         16  cs-agent-name.
000096             20  cs-agent-fname            pic x(20).
000097             20  cs-agent-mi               pic x.
000098             20  cs-agent-lname            pic x(25).
000099         16  cs-license-no                 pic x(15).
000100         16  cs-npn-number                 pic x(10).
000101         16  cs-agent-edit-status          pic x.
000102             88  cs-ae-refer-to-manager      value 'M'.
000103             88  cs-ae-cover-sheet           value 'C'.
000104             88  cs-ae-sig-form              value 'S'.
000105             88  cs-ae-verified              value 'V'.
000106             88  cs-unidentified-signature   value 'U'.
000107             88  cs-cert-returned            value 'R'.
000108             88  cs-accept-no-commission     value 'N'.
000109         16  cs-year                       pic 9999.
000110         16  cs-make                       pic x(20).
000111         16  cs-model                      pic x(20).
000112         16  cs-future                     pic x(20).
000113         16  cs-vehicle-odometer           pic s9(7) comp-3.
000114         16  cs-claim-verification-status  pic x.
000115             88  cs-clm-ver-eligible         value 'A'.
000116             88  cs-clm-ver-partial-elig     value 'B'.
000117             88  cs-clm-ver-not-eligible     value 'C'.
000118             88  cs-clm-ver-not-elig-opn-clm value 'D'.
000119             88  cs-clm-ver-not-part-elig-rw value 'E'.
000120             88  cs-clm-ver-ND-CERT          value 'F'.
000121             88  cs-clm-ver-spec-other       value 'G'.
000122             88  cs-clam-ver-pratial-corrected
000123                                             value 'H'.
000124             88  cs-clm-ver-no-matches       value 'I'.
000125             88  cs-clm-ver-not-elig-corrected
000126                                             value 'J'.
000127             88  cs-clm-ver-needs-review     value 'R'.
000128             88  cs-clm-ver-sent-to-claims   value 'W'.
000129         16  CS-LF-REFUND-METHOD           PIC X.
000130         16  CS-AH-REFUND-METHOD           PIC X.
000131         16  FILLER                        PIC X(353). *> was 420
000132*        16  FILLER                        PIC X(496).
      *<<((file: ELCCRTT))
000445
000446 01  FILLER                      PIC X(500)  VALUE SPACES.
000447
000448*                                COPY ELCENCC.
      *>>((file: ELCENCC))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCENCC                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   CLAIM SYSTEM ENCLOSURE CODE TABLE                            *
000008*                                                                *
000009*   THIS COPYBOOK IS USED FOR THE ONLINE PROCESS OF CREATING     *
000010*   A NAPERSOFT DOCUMENT                                         *
000011*                                                                *
000012*   FILE DESCRIPTION = ENCLOSURE CODE TABLE                      *
000013*                                                                *
000014*   FILE TYPE = VSAM,KSDS                                        *
000015*   RECORD SIZE = 400   RECFORM = FIX                            *
000016*                                                                *
000017*   BASE CLUSTER NAME = ELENCC                    RKP=2,LEN=16   *
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
000031* 082010    2008100900001  PEMA  NEW COPYBOOK/FILE
000032* 061217    2017060900001  TANA  INCREASE ATTACHMENTS FIELD SIZE
000033******************************************************************
000034
000035 01  ENCLOSURE-CODES.
000036     12  NC-RECORD-ID                      PIC XX.
000037         88  VALID-NC-ID                      VALUE 'NC'.
000038
000039     12  NC-CONTROL-PRIMARY.
000040         16  NC-COMPANY-CD                 PIC X.
000041         16  NC-REC-TYPE                   PIC X.
000042             88  NC-CLAIMS                   VALUE '1'.
000043             88  NC-ADMIN                    VALUE '2'.
000044         16  NC-ENC-CODE                   PIC X(5).
000045         16  FILLER                        PIC X(09).
000046
000047     12  NC-MAINT-INFORMATION.
000048         16  NC-LAST-MAINT-DT              PIC XX.
000049         16  NC-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000050         16  NC-LAST-MAINT-USER            PIC X(4).
000051         16  FILLER                        PIC XX.
000052
000053     12  NC-OUTPUT-STACK                   PIC XXX.
000054     12  NC-ENCLOSURE-LINE                 PIC X(100).
000055     12  NC-ATTACHMENTS                    PIC X(255).
000056     12  NC-FUTURE                         PIC X(12).
000057******************************************************************
      *<<((file: ELCENCC))
000449
000450 01  connect-string              pic x(60) value spaces.
000451 01  sqlconnect-parms.
000452     05  p-sql-server            PIC X(30).
000453     05  p-sql-database          PIC X(30).
000454     05  p-connect-return-code   pic s9(5) comp-5.
000455     05  p-sql-return-message    pic x(256).
000456
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
000458
000459 01 dfhcommarea                  PIC X(6500).
000460
000461 01  var  pic x(30).
000462
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'NSRASBL' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000463 VCOBOL-DUMMY-PROCEDURE.
000464
000465     display ' entering pgm nsrasbl '
000466     display ' commarea ' dfhcommarea
000467     MOVE EIBDATE                TO DC-JULIAN-YYDDD
000468     MOVE '5'                    TO DC-OPTION-CODE
000469     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
000470     MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
000471     MOVE DC-GREG-DATE-A-EDIT    TO WS-SAVE-EDIT-A-DATE
000472     MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE
000473     MOVE DC-DAY-OF-WEEK         TO SAVE-CYCLE-DAY-OF-WEEK
000474     move dfhcommarea            to srch-commarea
000475
000476*****************************************************
000477* The full       key has been established and passed
000478* to this program via NSRASLTR
000479*****************************************************
000480
000481     PERFORM 1115-MATCH-COMPANY  THRU 1115-EXIT
000482
000483*    display ' input data ' bl-carrier ' ' bl-group ' '
000484*       bl-account ' ' bl-eff-dt ' ' bl-cert-no
000485
000486     MOVE BL-LETTER-ID           TO OUT-LETTER
000487     MOVE BL-PROC-ID             TO OUT-PROC-ID
000488     MOVE BL-CARRIER             TO OUT-CARRIER
000489     MOVE BL-GROUP               TO OUT-GROUPING
000490     MOVE BL-ACCOUNT             TO OUT-ACCOUNT
000491     MOVE BL-PRINT-NOW-SW        TO OUT-PRINT-NOW
000492     MOVE BL-CERT-FORM-ID        TO OUT-CERTIFICATE-ID
000493     MOVE BL-SOURCE-SCREEN       TO OUT-SCREEN-ID
000494
000495     STRING DC-EDITA-CCYY DC-EDITA-MONTH
000496        DC-EDITA-DAY  DELIMITED BY SIZE
000497        INTO OUT-CYCLE-DT
000498     END-STRING
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
000510        into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000511              WS-KIX-SYS
000512        end-unstring
000513     end-if
000514
000515     set P to address of KIXHOST
000516     CALL "getenv" using by value P returning var-ptr
000517     if var-ptr = null then
000518        display ' kixhost not set '
000519     else
000520        set address of var to var-ptr
000521        move 0 to env-var-len
000522        inspect var tallying env-var-len
000523          for characters before X'00'
000524        MOVE var(1:env-var-len)  to ws-kixhost
000525        DISPLAY ' WS KIX HOST ' WS-KIXHOST
000526     end-if
000527
000528     MOVE SPACES TO SVR
000529
000530**** This routine will connect to the Logic Database on SQL Server
000531**** and call a stored procedure to determine the next business da
000532
000533     PERFORM 4100-CONNECT-TO-DB  THRU 4100-EXIT
000534     IF SQLCODE = 0
000535        PERFORM 4200-GET-NEXT-BUS-DT  THRU 4200-EXIT
000536        IF SQLCODE = 0
000537            MOVE WS-NEXT-BUS-DT TO SAVE-NEXT-BUS-DT-EDIT-A
000538                                   WS-EDIT-A-DATE
000539            MOVE WS-EDIT-A-YY   TO DC-YMD-YEAR
000540            MOVE WS-EDIT-A-MM   TO DC-YMD-MONTH
000541            MOVE WS-EDIT-A-DD   TO DC-YMD-DAY
000542            MOVE '3'            TO DC-OPTION-CODE
000543            PERFORM 9700-DATE-LINK THRU 9700-EXIT
000544            IF NO-CONVERSION-ERROR
000545               MOVE DC-BIN-DATE-1 TO SAVE-BIN-NEXT-BUS-DT
000546            ELSE
000547               MOVE SAVE-BIN-DATE TO DC-BIN-DATE-1
000548               MOVE '6'         TO DC-OPTION-CODE
000549               MOVE ZEROS       TO DC-ELAPSED-MONTHS
000550               IF SAVE-CYCLE-DAY-OF-WEEK = 6
000551                  MOVE 3        TO DC-ELAPSED-DAYS
000552               ELSE
000553                 IF SAVE-CYCLE-DAY-OF-WEEK = 7
000554                    MOVE 2     TO DC-ELAPSED-DAYS
000555                 ELSE
000556                    MOVE 1     TO DC-ELAPSED-DAYS
000557                 END-IF
000558               END-IF
000559               PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000560               IF NO-CONVERSION-ERROR
000561                   MOVE DC-BIN-DATE-2 TO SAVE-BIN-NEXT-BUS-DT
000562                   MOVE DC-GREG-DATE-A-EDIT TO
000563                                SAVE-NEXT-BUS-DT-EDIT-A
000564               ELSE
000565                   MOVE SAVE-BIN-DATE TO SAVE-BIN-NEXT-BUS-DT
000566                   MOVE WS-SAVE-EDIT-A-DATE TO
000567                                SAVE-NEXT-BUS-DT-EDIT-A
000568               END-IF
000569            END-IF
000570        ELSE
000571            MOVE SAVE-BIN-DATE TO DC-BIN-DATE-1
000572            MOVE '6'         TO DC-OPTION-CODE
000573            MOVE ZEROS       TO DC-ELAPSED-MONTHS
000574            IF SAVE-CYCLE-DAY-OF-WEEK = 6
000575               MOVE 3        TO DC-ELAPSED-DAYS
000576            ELSE
000577              IF SAVE-CYCLE-DAY-OF-WEEK = 7
000578                 MOVE 2     TO DC-ELAPSED-DAYS
000579              ELSE
000580                 MOVE 1     TO DC-ELAPSED-DAYS
000581              END-IF
000582            END-IF
000583            PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000584            IF NO-CONVERSION-ERROR
000585                MOVE DC-BIN-DATE-2 TO SAVE-BIN-NEXT-BUS-DT
000586                MOVE DC-GREG-DATE-A-EDIT TO
000587                                SAVE-NEXT-BUS-DT-EDIT-A
000588            ELSE
000589                MOVE SAVE-BIN-DATE TO SAVE-BIN-NEXT-BUS-DT
000590                MOVE WS-SAVE-EDIT-A-DATE TO
000591                                SAVE-NEXT-BUS-DT-EDIT-A
000592            END-IF
000593        END-IF
000594     ELSE
000595        MOVE SAVE-BIN-DATE TO DC-BIN-DATE-1
000596        MOVE '6'         TO DC-OPTION-CODE
000597        MOVE ZEROS       TO DC-ELAPSED-MONTHS
000598        IF SAVE-CYCLE-DAY-OF-WEEK = 6
000599           MOVE 3        TO DC-ELAPSED-DAYS
000600        ELSE
000601          IF SAVE-CYCLE-DAY-OF-WEEK = 7
000602             MOVE 2     TO DC-ELAPSED-DAYS
000603          ELSE
000604             MOVE 1     TO DC-ELAPSED-DAYS
000605          END-IF
000606        END-IF
000607        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000608        IF NO-CONVERSION-ERROR
000609            MOVE DC-BIN-DATE-2 TO SAVE-BIN-NEXT-BUS-DT
000610            MOVE DC-GREG-DATE-A-EDIT TO
000611                                SAVE-NEXT-BUS-DT-EDIT-A
000612        ELSE
000613            MOVE SAVE-BIN-DATE TO SAVE-BIN-NEXT-BUS-DT
000614            MOVE WS-SAVE-EDIT-A-DATE TO
000615                                SAVE-NEXT-BUS-DT-EDIT-A
000616        END-IF
000617     END-IF
000618     PERFORM 4300-DISCONNECT THRU 4300-EXIT
000619
000620     MOVE SAVE-NEXT-BUS-DT-EDIT-A  TO WS-EDIT-A-DATE
000621     STRING WS-EDIT-A-CCYY '-' WS-EDIT-A-MM '-'
000622           WS-EDIT-A-DD ' 00:00:00.0' DELIMITED BY SIZE
000623              INTO OUT-NEXT-BUS-DT
000624     END-STRING
000625
000626     IF BL-EFF-DT = '99/99/99' OR '99/99/9999'
000627        MOVE HIGH-VALUES         TO WS-PASSED-DT
000628     ELSE
000629        IF BL-EFF-DT = SPACES
000630           display ' bad eff data ' bl-eff-dt ' ' bl-cert-no
000631        ELSE
000632           IF BL-EFF-DT (9:2) = SPACES
000633              STRING BL-EFF-DT (7:2) BL-EFF-DT (1:2)
000634                     BL-EFF-DT (4:2)
000635                DELIMITED BY SIZE INTO DC-GREG-DATE-1-YMD-R
000636              END-STRING
000637           ELSE
000638              STRING BL-EFF-DT (9:2) BL-EFF-DT (1:2)
000639                     BL-EFF-DT (4:2)
000640                DELIMITED BY SIZE INTO DC-GREG-DATE-1-YMD-R
000641              END-STRING
000642           END-IF
000643           MOVE '3'              TO DC-OPTION-CODE
000644           PERFORM 9700-DATE-LINK THRU 9700-EXIT
000645           IF NO-CONVERSION-ERROR
000646              MOVE DC-BIN-DATE-1 TO WS-PASSED-DT
000647           ELSE
000648              MOVE 'BAD EFF DATE CONVERT ' TO BL-MESSAGE
000649              SET BL-FAIL TO TRUE
000650              GO TO 0000-RETURN
000651           END-IF
000652        END-IF
000653     END-IF
000654
000655     perform 0800-init-out-data  thru 0800-exit
000656
000657     SET BL-FAIL TO TRUE
000658******************************************************************
000659* DATA SOURCE MEANINGS  BL-DATA-SRCE
000660*    1) FROM ACCT MAINT
000661*    2) FROM CERT UPDATE
000662*    3) FROM COMP MAINT
000663*    4) FROM REVIEW AND CORRECTIONS
000664******************************************************************
000665
000666     EVALUATE BL-DATA-SRCE
000667        WHEN '1'
000668           PERFORM 0100-DATA-SRCE-1 THRU 0100-EXIT
000669        WHEN '2'
000670           PERFORM 0200-DATA-SRCE-2 THRU 0200-EXIT
000671        WHEN '3'
000672           PERFORM 0300-DATA-SRCE-3 THRU 0300-EXIT
000673        WHEN '4'
000674           PERFORM 0400-DATA-SRCE-4 THRU 0400-EXIT
000675        WHEN OTHER
000676           MOVE ' BAD DATA SOURCE CODE ' TO BL-MESSAGE
000677     END-EVALUATE
000678
000679     IF BL-OK
000680        display ' bl write erarch ' bl-write-erarch
000681        PERFORM 1250-GET-ELLETR  THRU 1250-EXIT
000682        PERFORM 1700-GET-ELENCC  THRU 1700-EXIT
000683        IF BL-ARCHIVE-NO = ZEROS
000684           PERFORM 1500-GET-ARCH-NO THRU 1500-EXIT
000685           MOVE WS-ARCHIVE-NO    TO BL-ARCHIVE-NO
000686           display ' arch zeros, new arch ' bl-archive-no
000687        END-IF
000688        MOVE BL-ARCHIVE-NO       TO OUT-ARCH-NO
000689        PERFORM 0500-SCRUB-DATA  THRU 0500-EXIT
000690        display ' bl write erarch ' bl-write-erarch
000691        IF BL-WRITE-ERARCH = 'T' OR 'B'
000692           PERFORM 0700-WRITE-EXTR THRU 0700-EXIT
000693           IF BL-FAIL
000694              GO TO 0000-RETURN
000695           END-IF
000696        END-IF
000697        MOVE W-LETTER-TO-ACCT    TO BL-LETTER-TO-ACCT
000698        MOVE W-LETTER-TO-BENE    TO BL-LETTER-TO-BENE
000699     END-IF
000700
000701     .
000702 0000-RETURN.
000703
000704     move srch-commarea          to dfhcommarea
000705
000706     
      * exec cics
000707*       return
000708*    end-exec
      *    MOVE '.(                    ''   #00006065' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036303635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000709
000710     .
000711 0100-DATA-SRCE-1.
000712
000713     PERFORM 1120-MATCH-CARRIER  THRU 1120-EXIT
000714     PERFORM 1125-MATCH-STATE    THRU 1125-EXIT
000715     MOVE ' '                    TO WS-ERACCT-SW
000716     MOVE LOW-VALUES             TO WS-ERACCT-KEY
000717     MOVE WS-COMPANY-CD          TO WS-ERACCT-COMPANY-CD
000718     MOVE BL-CARRIER             TO WS-ERACCT-CARRIER
000719     MOVE BL-GROUP               TO WS-ERACCT-GROUP
000720     MOVE BL-STATE               TO WS-ERACCT-STATE
000721     MOVE BL-ACCOUNT             TO WS-ERACCT-ACCOUNT
000722     MOVE WS-PASSED-DT           TO WS-ERACCT-EXP-DT
000723                                    WS-CERT-EFF-DT
000724     PERFORM 1200-GET-ERACCT     THRU 1200-EXIT
000725     IF ACCT-FOUND
000726        MOVE AM-CSR-CODE         TO WS-CSR-ID
000727        PERFORM 1025-GET-CSR-INFO
000728                                 THRU 1025-EXIT
000729        PERFORM 1300-DETERMINE-COMM
000730                                 THRU 1300-EXIT
000731        SET BL-OK TO TRUE
000732     ELSE
000733        MOVE ' ACCOUNT MASTER NOT FOUND ' TO BL-MESSAGE
000734        SET BL-FAIL TO TRUE
000735     END-IF
000736
000737     .
000738 0100-EXIT.
000739     EXIT.
000740
000741 0200-DATA-SRCE-2.
000742
000743     display ' made it to data srce 2 '
000744     PERFORM 1120-MATCH-CARRIER  THRU 1120-EXIT
000745     PERFORM 1125-MATCH-STATE    THRU 1125-EXIT
000746     MOVE ' '                    TO WS-ELCERT-SW
000747     MOVE WS-COMPANY-CD          TO WS-ELCERT-COMPANY-CD
000748     MOVE BL-CARRIER             TO WS-ELCERT-CARRIER
000749     MOVE BL-GROUP               TO WS-ELCERT-GROUP
000750     MOVE BL-STATE               TO WS-ELCERT-STATE
000751     MOVE BL-ACCOUNT             TO WS-ELCERT-ACCOUNT
000752     MOVE WS-PASSED-DT           TO WS-ELCERT-EFF-DT
000753                                    WS-CERT-EFF-DT
000754     MOVE BL-CERT-NO             TO WS-ELCERT-CERT-NO
000755
000756     MOVE ' '                 TO WS-ERACCT-SW
000757     MOVE LOW-VALUES          TO WS-ERACCT-KEY
000758     MOVE WS-ELCERT-KEY (1:22) TO WS-ERACCT-KEY (1:22)
000759     PERFORM 1200-GET-ERACCT  THRU 1200-EXIT
000760     IF ACCT-FOUND
000761        MOVE AM-CSR-CODE      TO WS-CSR-ID
000762        PERFORM 1025-GET-CSR-INFO THRU 1025-EXIT
000763     END-IF
000764
000765     PERFORM 1100-GET-ELCERT     THRU 1100-EXIT
000766
000767     IF CERT-FOUND
000768        PERFORM 1130-MATCH-BENCDS THRU 1130-EXIT
000769        MOVE WS-LF-ABBRV         TO OUT-LF-ABBRV
000770        MOVE WS-LF-DESC          TO OUT-LF-DESC
000771        IF WS-LF-EARN = 'B'
000772           MOVE 'B'              TO OUT-BALLOON-IND
000773        END-IF
000774        MOVE WS-WAIT-PER         TO OUT-WAIT-PER
000775        MOVE WS-RET-ELIM         TO OUT-RET-ELIM
000776        MOVE WS-AH-DESC          TO OUT-AH-DESC
000777        MOVE WS-BEN-DAYS         TO OUT-BEN-DAYS
000778
000779        IF BL-ENDT-ARCH-NO > ZERO
000780           MOVE WS-COMPANY-CD      TO WS-ERENDT-COMPANY-CD-A1
000781           MOVE BL-ENDT-ARCH-NO    TO WS-ERENDT-ARCHIVE
000782           PERFORM 0600-GET-ERENDT THRU 0600-EXIT
000783        ELSE
000784           IF BL-ARCHIVE-NO > ZERO
000785              MOVE WS-COMPANY-CD      TO WS-ERENDT-COMPANY-CD-A1
000786              MOVE BL-ARCHIVE-NO      TO WS-ERENDT-ARCHIVE
000787              PERFORM 0600-GET-ERENDT THRU 0600-EXIT
000788           END-IF
000789        END-IF
000790
000791        PERFORM 4500-CHECK-BALLOON-STATE THRU 4500-EXIT
000792*072312 IF (CM-STATE = 'KY' OR 'MD' OR 'WI')
000793        IF WS-LOOKUP-VALUE > SPACES
000794          AND (OUT-BALLOON-IND = 'B')
000795           MOVE 'B'              TO OUT-FORM (3:1)
000796        END-IF
000797        SET BL-OK TO TRUE
000798     ELSE
000799        display ' cert not found ' ws-elcert-cert-no
000800        MOVE ' CERTIFICATE NOT FOUND ' TO BL-MESSAGE
000801        SET BL-FAIL TO TRUE
000802     END-IF
000803
000804     .
000805 0200-EXIT.
000806     EXIT.
000807
000808 0300-DATA-SRCE-3.
000809
000810     PERFORM 1120-MATCH-CARRIER  THRU 1120-EXIT
000811
000812     MOVE WS-COMPANY-CD          TO WS-ERCOMP-COMPANY-CD
000813     MOVE BL-CARRIER             TO WS-ERCOMP-CARRIER
000814     MOVE BL-GROUP               TO WS-ERCOMP-GROUP
000815     MOVE BL-RESP-NO             TO WS-ERCOMP-RESP-NO
000816     MOVE BL-ACCOUNT             TO WS-ERCOMP-ACCOUNT
000817     IF WS-ERCOMP-ACCOUNT = SPACES
000818        MOVE LOW-VALUES          TO WS-ERCOMP-ACCOUNT
000819        MOVE 'G'                 TO WS-ERCOMP-REC-TYPE
000820     ELSE
000821        MOVE 'A'                 TO WS-ERCOMP-REC-TYPE
000822     END-IF
000823
000824     PERFORM 1225-GET-ERCOMP     THRU 1225-EXIT
000825     IF RESP-NORMAL
000826        MOVE CO-CSR-CODE         TO WS-CSR-ID
000827        PERFORM 1025-GET-CSR-INFO
000828                                 THRU 1025-EXIT
000829        SET BL-OK TO TRUE
000830     ELSE
000831        MOVE ' COMPENSATION MASTER NOT FOUND ' TO BL-MESSAGE
000832        SET BL-FAIL TO TRUE
000833     END-IF
000834
000835     .
000836 0300-EXIT.
000837     EXIT.
000838
000839 0400-DATA-SRCE-4.
000840
000841     MOVE WS-COMPANY-CD          TO WS-ERPNDB-COMPANY-CD
000842     MOVE BL-BATCH-NO            TO WS-ERPNDB-BATCH-NO
000843     MOVE BL-BATCH-SEQ           TO WS-ERPNDB-BATCH-SEQ
000844     MOVE +0                     TO WS-ERPNDB-CHG-SEQ
000845     MOVE ' '                    TO WS-ERPNDB-SW WS-ERACCT-SW
000846                                    WS-ELCERT-SW WS-ERENDT-SW
000847
000848     
      * EXEC CICS READ
000849*         DATASET    ('ERPNDB')
000850*         INTO       (PENDING-BUSINESS)
000851*         RIDFLD     (WS-ERPNDB-KEY)
000852*         RESP       (WS-RESPONSE)
000853*    END-EXEC
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
           MOVE 'ERPNDB' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00006207' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303036323037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 WS-ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000854
000855
000856     IF RESP-NORMAL
000857        IF PB-RECORD-TYPE = '1' OR '2'
000858           MOVE PB-CARRIER       TO BL-CARRIER
000859           MOVE PB-GROUPING      TO BL-GROUP
000860           MOVE PB-STATE         TO BL-STATE
000861           MOVE PB-ACCOUNT       TO BL-ACCOUNT
000862           MOVE PB-CERT-NO       TO BL-CERT-NO
000863           MOVE PB-CSR-ID        TO WS-CSR-ID
000864           IF PB-ISSUE
000865              SET PROCESS-ISSUE TO TRUE
000866              MOVE PB-I-LF-BENEFIT-CD TO WS-LF-BENCD
000867              MOVE PB-I-AH-BENEFIT-CD TO WS-AH-BENCD
000868              MOVE PB-I-LF-PREM-CALC TO OUT-LF-CALC-PRM
000869              MOVE PB-I-AH-PREM-CALC TO OUT-AH-CALC-PRM
000870           ELSE
000871              SET PROCESS-CANCEL TO TRUE
000872              MOVE PB-CI-LF-BENEFIT-CD TO WS-LF-BENCD
000873              MOVE PB-CI-AH-BENEFIT-CD TO WS-AH-BENCD
000874              MOVE PB-C-LF-REF-CALC TO OUT-LF-CALC-REF
000875              MOVE PB-C-AH-REF-CALC TO OUT-AH-CALC-REF
000876           END-IF
000877           PERFORM 1120-MATCH-CARRIER
000878                                 THRU 1120-EXIT
000879           PERFORM 1125-MATCH-STATE
000880                                 THRU 1125-EXIT
000881           PERFORM 1130-MATCH-BENCDS
000882                                 THRU 1130-EXIT
000883           MOVE WS-LF-ABBRV      TO OUT-LF-ABBRV
000884           MOVE WS-LF-DESC       TO OUT-LF-DESC
000885                                    OUT-ORIG-LF-DESC
000886           IF WS-LF-EARN = 'B'
000887              MOVE 'B'           TO OUT-BALLOON-IND
000888           END-IF
000889           MOVE WS-WAIT-PER      TO OUT-WAIT-PER
000890                                    OUT-ORIG-WAIT-PER
000891           MOVE WS-RET-ELIM      TO OUT-RET-ELIM
000892                                    OUT-ORIG-RET-ELIM
000893           MOVE WS-AH-DESC       TO OUT-AH-DESC
000894                                    OUT-ORIG-AH-DESC
000895           MOVE WS-BEN-DAYS      TO OUT-BEN-DAYS
000896                                    OUT-ORIG-BEN-DAYS
000897           PERFORM 1025-GET-CSR-INFO
000898                                 THRU 1025-EXIT
000899
000900           MOVE LOW-VALUES       TO WS-ERACCT-KEY
000901           MOVE PB-CONTROL-BY-ACCOUNT (1:22)
000902                                 TO WS-ERACCT-KEY (1:22)
000903           MOVE PB-CERT-EFF-DT   TO WS-CERT-EFF-DT
000904           PERFORM 1200-GET-ERACCT THRU 1200-EXIT
000905           IF ACCT-FOUND
000906              MOVE AM-CSR-CODE      TO WS-CSR-ID
000907              PERFORM 1025-GET-CSR-INFO
000908                                    THRU 1025-EXIT
000909              PERFORM 1300-DETERMINE-COMM
000910                                    THRU 1300-EXIT
000911           END-IF
000912
000913           MOVE PB-CONTROL-BY-ACCOUNT (1:33)
000914                                 TO WS-ELCERT-KEY
000915           PERFORM 1100-GET-ELCERT
000916                                 THRU 1100-EXIT
000917           IF (BL-DATA-SRCE = '4')
000918             AND (PROCESS-CANCEL)
000919              MOVE PB-C-CANCEL-REASON  TO OUT-CANCEL-REASON
000920              DISPLAY ' CANCEL-REASON ' OUT-CANCEL-REASON
000921              IF PB-C-REFUND-CREATED
000922                 PERFORM 2000-GET-CANCEL-REFUND THRU 2000-EXIT
000923              END-IF
000924           END-IF
000925
000926
000927           IF BL-ENDT-ARCH-NO > ZERO
000928              MOVE PB-COMPANY-CD   TO WS-ERENDT-COMPANY-CD-A1
000929              MOVE BL-ENDT-ARCH-NO TO WS-ERENDT-ARCHIVE
000930              PERFORM 0600-GET-ERENDT THRU 0600-EXIT
000931           ELSE
000932              IF BL-ARCHIVE-NO > ZERO
000933                 MOVE PB-COMPANY-CD TO WS-ERENDT-COMPANY-CD-A1
000934                 MOVE BL-ARCHIVE-NO TO WS-ERENDT-ARCHIVE
000935                 PERFORM 0600-GET-ERENDT THRU 0600-EXIT
000936              END-IF
000937           END-IF
000938
000939           SET BL-OK TO TRUE
000940           PERFORM 4500-CHECK-BALLOON-STATE THRU 4500-EXIT
000941*072312     IF (CM-STATE = 'KY' OR 'MD' OR 'WI')
000942           IF WS-LOOKUP-VALUE > SPACES
000943             AND (OUT-BALLOON-IND = 'B')
000944              MOVE 'B'           TO OUT-FORM (3:1)
000945           END-IF
000946        END-IF
000947     END-IF
000948
000949     .
000950 0400-EXIT.
000951     EXIT.
000952
000953 0500-SCRUB-DATA.
000954
000955*   THE DECODER DOES NOT LIKE & AND I TRIED TO USE
000956*   A DIFFERENT DELIMITER IN PROGRAM NSRASLTR BUT
000957*   COULD NOT GET IT TO WORK SO I AM CONVERTING IT
000958*   TO A HEX 26 TO GET BY THE DECODER
000959
000960     MOVE FUNCTION LENGTH(OUT-ACCT-NAME)
000961                                 TO M1
000962     MOVE OUT-ACCT-NAME          TO WS-WORK-FIELD
000963     PERFORM VARYING A1 FROM +1 BY +1 UNTIL
000964        (A1 > M1)
000965        OR (WS-WORK-FIELD (A1:1) = '*')
000966        OR (WS-WORK-FIELD (A1:4) = ' DBA' OR ' AKA')
000967        OR (WS-WORK-FIELD (A1:4) = '(DBA' OR '(AKA')
000968     END-PERFORM
000969     IF A1 > M1
000970        CONTINUE
000971     ELSE
000972        MOVE WS-WORK-FIELD (1:A1 - 1)
000973                                 TO OUT-ACCT-NAME
000974     END-IF
000975
000976     MOVE FUNCTION LENGTH(OUT-BENE-NAME)
000977                                 TO M1
000978     MOVE OUT-BENE-NAME          TO WS-WORK-FIELD
000979     PERFORM VARYING A1 FROM +1 BY +1 UNTIL
000980        (A1 > M1)
000981        OR (WS-WORK-FIELD (A1:1) = '*')
000982        OR (WS-WORK-FIELD (A1:4) = ' DBA' OR ' AKA')
000983        OR (WS-WORK-FIELD (A1:4) = '(DBA' OR '(AKA')
000984     END-PERFORM
000985     IF A1 > M1
000986        CONTINUE
000987     ELSE
000988        MOVE WS-WORK-FIELD (1:A1 - 1)
000989                                 TO OUT-BENE-NAME
000990     END-IF
000991
000992     IF OUT-CERT-NO = LOW-VALUES OR SPACES
000993         MOVE '0000000000 '  TO OUT-CERT-NO
000994     END-IF
000995
000996     MOVE +1 TO M1
000997     MOVE SPACES TO BL-RECORD-PASSED-DATA
000998     MOVE FUNCTION LENGTH(NAPER-OUTPUT-DATA)
000999                                 TO NS-LEN
001000     display ' naper output data length ' ns-len
001001     PERFORM VARYING A1 FROM +1 BY +1 UNTIL
001002        (NAPER-OUTPUT-DATA (A1:11) = 'EndOfString')
001003        OR (A1 > NS-LEN)
001004        IF NAPER-OUTPUT-DATA (A1:3) = SPACES
001005           CONTINUE
001006        ELSE
001007           IF NAPER-OUTPUT-DATA (A1:1) = '~'
001008              MOVE '&'           TO BL-RECORD-PASSED-DATA (M1:1)
001009              MOVE ' '           TO NAPER-OUTPUT-DATA (A1:1)
001010              ADD +1 TO M1
001011           ELSE
001012              IF NAPER-OUTPUT-DATA (A1:1) = '&'
001013                 MOVE '%26'      TO BL-RECORD-PASSED-DATA (M1:3)
001014                 ADD +3 TO M1
001015              ELSE
001016                 MOVE NAPER-OUTPUT-DATA (A1:1)
001017                                 TO BL-RECORD-PASSED-DATA (M1:1)
001018                 ADD +1 TO M1
001019              END-IF
001020           END-IF
001021        END-IF
001022     END-PERFORM
001023     .
001024 0500-EXIT.
001025     EXIT.
001026
001027 0600-GET-ERENDT.
001028
001029     
      * EXEC CICS READ
001030*         DATASET    ('ERENDT2')
001031*         INTO       (ENDORSEMENT-RECORD)
001032*         RIDFLD     (WS-ERENDT-KEY-BY-ARCH)
001033*         RESP       (WS-RESPONSE)
001034*    END-EXEC
           MOVE LENGTH OF
            ENDORSEMENT-RECORD
             TO DFHEIV11
           MOVE 'ERENDT2' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00006388' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303036333838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ENDORSEMENT-RECORD, 
                 DFHEIV11, 
                 WS-ERENDT-KEY-BY-ARCH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001035
001036     DISPLAY ' JUST READ ENDT ' WS-RESPONSE ' '
001037         WS-ERENDT-ARCHIVE
001038     IF (RESP-NORMAL)
001039        and (en-company-cd  = cm-company-cd)
001040        and (en-carrier     = cm-carrier)
001041        and (en-state       = cm-state)
001042        and (en-account     = cm-account)
001043        and (en-cert-eff-dt = cm-cert-eff-dt)
001044        and (en-cert-no     = cm-cert-no)
001045        SET ENDT-FOUND TO TRUE
001046        PERFORM 0620-BUILD-ENDT  THRU 0620-EXIT
001047        PERFORM 0650-PROCESS-NO-Change
001048                                 THRU 0650-EXIT
001049     END-IF
001050
001051     .
001052 0600-EXIT.
001053     EXIT.
001054
001055 0620-BUILD-ENDT.
001056
001057     MOVE SPACES                 TO OUT-ORIG-LF-CAN-DT
001058                                    OUT-NEW-LF-CAN-DT
001059                                    OUT-ORIG-AH-CAN-DT
001060                                    OUT-NEW-AH-CAN-DT
001061                                    OUT-ORIG-CAN-DT
001062                                    OUT-NEW-CAN-DT
001063
001064     IF EN-REC-TYPE = 'C'
001065        GO TO 0620-PROCESS-CANCEL
001066     END-IF
001067
001068     MOVE EN-LF-ORIG-BENCD       TO WS-LF-BENCD
001069     MOVE EN-AH-ORIG-BENCD       TO WS-AH-BENCD
001070     PERFORM 1130-MATCH-BENCDS   THRU 1130-EXIT
001071     MOVE WS-LF-DESC             TO OUT-ORIG-LF-DESC
001072     IF WS-LF-EARN = 'B'
001073        MOVE 'B'                 TO OUT-BALLOON-IND
001074     END-IF
001075     MOVE WS-AH-DESC             TO OUT-ORIG-AH-DESC
001076     MOVE WS-RET-ELIM            TO OUT-ORIG-RET-ELIM
001077     MOVE WS-BEN-DAYS            TO OUT-ORIG-BEN-DAYS
001078     MOVE WS-WAIT-PER            TO OUT-ORIG-WAIT-PER
001079     MOVE EN-INS-ORIG-LAST-NAME    TO OUT-ORIG-ILNAME
001080     MOVE EN-INS-ORIG-FIRST-NAME   TO OUT-ORIG-IFNAME
001081     MOVE EN-INS-ORIG-MIDDLE-INIT  TO OUT-ORIG-MINIT
001082     IF EN-INS-ORIG-AGE-DEF-FLAG = 'Y'
001083         MOVE ZERO                 TO OUT-ORIG-IAGE
001084     ELSE
001085         MOVE EN-INS-ORIG-AGE      TO OUT-ORIG-IAGE
001086     END-IF
001087     MOVE EN-JNT-ORIG-LAST-NAME    TO OUT-ORIG-JLNAME
001088     MOVE EN-JNT-ORIG-FIRST-NAME   TO OUT-ORIG-JFNAME
001089     MOVE EN-JNT-ORIG-MIDDLE-INIT  TO OUT-ORIG-JMINIT
001090     IF EN-JNT-ORIG-AGE-DEF-FLAG = 'Y'
001091         MOVE ZERO                 TO OUT-ORIG-JAGE
001092     ELSE
001093         MOVE EN-JNT-ORIG-AGE      TO OUT-ORIG-JAGE
001094     END-IF
001095
001096     MOVE EN-LF-ORIG-BENCD         TO OUT-ORIG-LF-BENCD
001097     MOVE EN-LF-ORIG-TERM          TO OUT-ORIG-LF-TERM
001098     MOVE EN-LF-ORIG-BEN-AMT       TO OUT-ORIG-LF-BEN
001099     MOVE EN-LF-ORIG-PRM-AMT       TO OUT-ORIG-LF-PRM
001100     MOVE EN-LF-ORIG-ALT-BEN-AMT   TO OUT-ORIG-LF-ALT-BEN
001101     MOVE EN-LF-ORIG-ALT-PRM-AMT   TO OUT-ORIG-LF-ALT-PRM
001102     COMPUTE WS-WRK-AMOUNT = EN-LF-ORIG-PRM-AMT +
001103                             EN-LF-ORIG-ALT-PRM-AMT
001104     MOVE WS-WRK-AMOUNT          TO OUT-ORIG-LF-CMB-PREM
001105     IF EN-LF-ORIG-BENCD EQUAL '00' OR SPACES
001106         MOVE SPACES             TO OUT-ORIG-LF-BEN-A
001107                                    OUT-ORIG-LF-PRM-A
001108                                    OUT-ORIG-LF-ALT-BEN-A
001109                                    OUT-ORIG-LF-ALT-PRM-A
001110                                    OUT-ORIG-COVERAGE-IND
001111     ELSE
001112         MOVE 'LIFE'             TO OUT-ORIG-COVERAGE-IND
001113         IF EN-LF-ORIG-PRM-AMT = .01
001114             MOVE SPACES         TO OUT-ORIG-LF-PRM-A
001115         END-IF
001116         IF EN-LF-ORIG-BEN-AMT = 999999999.99
001117             MOVE SPACES         TO OUT-ORIG-LF-BEN-A
001118         END-IF
001119         IF EN-LF-ORIG-ALT-PRM-AMT = .01
001120             MOVE SPACES         TO OUT-ORIG-LF-ALT-PRM-A
001121         END-IF
001122         IF EN-LF-ORIG-ALT-BEN-AMT = 999999999.99
001123             MOVE SPACES         TO OUT-ORIG-LF-ALT-BEN-A
001124         END-IF
001125     END-IF
001126
001127     MOVE EN-LF-ORIG-EXP-DT      TO DC-BIN-DATE-1
001128     MOVE ' '                    TO DC-OPTION-CODE
001129     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
001130     IF NO-CONVERSION-ERROR
001131        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
001132           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
001133              INTO OUT-ORIG-LF-EXP-DT
001134        END-STRING
001135     END-IF
001136
001137     MOVE EN-AH-ORIG-BENCD       TO OUT-ORIG-AH-BENCD
001138     MOVE EN-AH-ORIG-TERM        TO OUT-ORIG-AH-TERM
001139     MOVE EN-AH-ORIG-BEN-AMT     TO OUT-ORIG-AH-BEN
001140     MOVE EN-AH-ORIG-PRM-AMT     TO OUT-ORIG-AH-PRM
001141     IF EN-AH-ORIG-BENCD EQUAL '00' OR SPACES
001142         MOVE SPACES             TO OUT-ORIG-AH-BEN-A
001143                                    OUT-ORIG-AH-PRM-A
001144     ELSE
001145        IF OUT-ORIG-COVERAGE-IND = 'LIFE'
001146            MOVE 'BOTH'          TO OUT-ORIG-COVERAGE-IND
001147        ELSE
001148            MOVE 'AH  '          TO OUT-ORIG-COVERAGE-IND
001149        END-IF
001150        IF EN-AH-ORIG-PRM-AMT = .01
001151            MOVE SPACES          TO OUT-ORIG-AH-PRM-A
001152        END-IF
001153        IF EN-AH-ORIG-BEN-AMT = 9999999.99
001154            MOVE SPACES          TO OUT-ORIG-AH-BEN-A
001155        END-IF
001156     END-IF
001157
001158     COMPUTE WS-WORK-PREM =
001159        EN-LF-ORIG-PRM-AMT + EN-LF-ORIG-ALT-PRM-AMT
001160           + EN-AH-ORIG-PRM-AMT
001161     MOVE WS-WORK-PREM           TO OUT-ORIG-TOT-PREM
001162
001163     MOVE EN-AH-ORIG-EXP-DT      TO DC-BIN-DATE-1
001164     MOVE ' '                    TO DC-OPTION-CODE
001165     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
001166     IF NO-CONVERSION-ERROR
001167        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
001168           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
001169              INTO OUT-ORIG-AH-EXP-DT
001170        END-STRING
001171     END-IF
001172     MOVE EN-AH-ORIG-CP          TO OUT-ORIG-CRIT-PER
001173                                    OUT-ORIG-MAX-PMTS
001174     IF EN-AH-ORIG-BENCD EQUAL '00' OR SPACES
001175         MOVE SPACES             TO OUT-ORIG-MAX-PMTS-A
001176     END-IF
001177
001178     MOVE EN-INS-NEW-LAST-NAME    TO OUT-NEW-ILNAME
001179     MOVE EN-INS-NEW-FIRST-NAME   TO OUT-NEW-IFNAME
001180     MOVE EN-INS-NEW-MIDDLE-INIT  TO OUT-NEW-IMINIT
001181     IF EN-INS-NEW-AGE-DEF-FLAG = 'Y'
001182         MOVE ZERO                TO OUT-NEW-IAGE
001183     ELSE
001184         MOVE EN-INS-NEW-AGE      TO OUT-NEW-IAGE
001185     END-IF
001186     MOVE EN-JNT-NEW-LAST-NAME    TO OUT-NEW-JLNAME
001187     MOVE EN-JNT-NEW-FIRST-NAME   TO OUT-NEW-JFNAME
001188     MOVE EN-JNT-NEW-MIDDLE-INIT  TO OUT-NEW-JMINIT
001189     IF EN-JNT-NEW-AGE-DEF-FLAG = 'Y'
001190         MOVE ZERO                TO OUT-NEW-JAGE
001191     ELSE
001192         MOVE EN-JNT-NEW-AGE      TO OUT-NEW-JAGE
001193     END-IF
001194
001195     MOVE EN-LF-NEW-BENCD        TO WS-LF-BENCD
001196     MOVE EN-AH-NEW-BENCD        TO WS-AH-BENCD
001197     PERFORM 1130-MATCH-BENCDS   THRU 1130-EXIT
001198     MOVE WS-LF-DESC             TO OUT-NEW-LF-DESC
001199     IF WS-LF-EARN = 'B'
001200        MOVE 'B'                 TO OUT-BALLOON-IND
001201     END-IF
001202     MOVE WS-AH-DESC             TO OUT-NEW-AH-DESC
001203     MOVE WS-RET-ELIM            TO OUT-NEW-RET-ELIM
001204     MOVE WS-BEN-DAYS            TO OUT-NEW-BEN-DAYS
001205     MOVE WS-WAIT-PER            TO OUT-NEW-WAIT-PER
001206     MOVE EN-LF-NEW-BENCD         TO OUT-NEW-LF-BENCD
001207     MOVE EN-LF-NEW-TERM          TO OUT-NEW-LF-TERM
001208     MOVE EN-LF-NEW-BEN-AMT       TO OUT-NEW-LF-BEN
001209     MOVE EN-LF-NEW-PRM-AMT       TO OUT-NEW-LF-PRM
001210     MOVE EN-LF-NEW-ALT-BEN-AMT   TO OUT-NEW-LF-ALT-BEN
001211     MOVE EN-LF-NEW-ALT-PRM-AMT   TO OUT-NEW-LF-ALT-PRM
001212     COMPUTE WS-WRK-AMOUNT = EN-LF-NEW-PRM-AMT +
001213                             EN-LF-NEW-ALT-PRM-AMT
001214     MOVE WS-WRK-AMOUNT           TO OUT-NEW-LF-CMB-PREM
001215     IF EN-LF-NEW-BENCD EQUAL '00' OR SPACES
001216         MOVE SPACES             TO OUT-NEW-LF-BEN-A
001217                                    OUT-NEW-LF-PRM-A
001218                                    OUT-NEW-LF-ALT-BEN-A
001219                                    OUT-NEW-LF-ALT-PRM-A
001220                                    OUT-NEW-LF-CMB-PREM-A
001221                                    OUT-NEW-COVERAGE-IND
001222     ELSE
001223         MOVE 'LIFE'             TO OUT-NEW-COVERAGE-IND
001224     END-IF
001225    MOVE EN-LF-NEW-EXP-DT        TO DC-BIN-DATE-1
001226     MOVE ' '                     TO DC-OPTION-CODE
001227     PERFORM 9700-DATE-LINK       THRU 9700-EXIT
001228     IF NO-CONVERSION-ERROR
001229        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
001230           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
001231              INTO OUT-NEW-LF-EXP-DT
001232        END-STRING
001233     END-IF
001234
001235     MOVE EN-AH-NEW-BENCD         TO OUT-NEW-AH-BENCD
001236     MOVE EN-AH-NEW-TERM          TO OUT-NEW-AH-TERM
001237     MOVE EN-AH-NEW-BEN-AMT       TO OUT-NEW-AH-BEN
001238     MOVE EN-AH-NEW-PRM-AMT       TO OUT-NEW-AH-PRM
001239
001240     COMPUTE WS-WORK-PREM =
001241        EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT
001242           + EN-AH-NEW-PRM-AMT
001243     MOVE WS-WORK-PREM           TO OUT-NEW-TOT-PREM
001244
001245     IF EN-AH-NEW-BENCD EQUAL '00' OR SPACES
001246         MOVE SPACES             TO OUT-NEW-AH-BEN-A
001247                                    OUT-NEW-AH-PRM-A
001248     ELSE
001249        IF OUT-NEW-COVERAGE-IND = 'LIFE'
001250            MOVE 'BOTH'          TO OUT-NEW-COVERAGE-IND
001251        ELSE
001252            MOVE 'AH  '          TO OUT-NEW-COVERAGE-IND
001253        END-IF
001254     END-IF
001255     MOVE EN-AH-NEW-EXP-DT        TO DC-BIN-DATE-1
001256     MOVE ' '                     TO DC-OPTION-CODE
001257     PERFORM 9700-DATE-LINK       THRU 9700-EXIT
001258     IF NO-CONVERSION-ERROR
001259        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
001260           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
001261              INTO OUT-NEW-AH-EXP-DT
001262        END-STRING
001263     END-IF
001264     MOVE EN-AH-NEW-CP            TO OUT-NEW-CRIT-PER
001265                                     OUT-NEW-MAX-PMTS
001266     IF EN-AH-NEW-BENCD EQUAL '00' OR SPACES
001267         MOVE SPACES             TO OUT-NEW-MAX-PMTS-A
001268     END-IF
001269
001270     IF EN-LF-ORIG-EXP-DT > EN-AH-ORIG-EXP-DT
001271        MOVE OUT-ORIG-LF-EXP-DT  TO OUT-ORIG-SCHED-EXP-DT
001272     ELSE
001273        MOVE OUT-ORIG-AH-EXP-DT  TO OUT-ORIG-SCHED-EXP-DT
001274     END-IF
001275
001276     IF EN-LF-NEW-EXP-DT > EN-AH-NEW-EXP-DT
001277        MOVE OUT-NEW-LF-EXP-DT   TO OUT-NEW-SCHED-EXP-DT
001278     ELSE
001279        MOVE OUT-NEW-AH-EXP-DT   TO OUT-NEW-SCHED-EXP-DT
001280     END-IF
001281
001282     IF EN-LF-ORIG-TERM > EN-AH-ORIG-TERM
001283        MOVE EN-LF-ORIG-TERM     TO OUT-ORIG-SCHED-TERM
001284     ELSE
001285        MOVE EN-AH-ORIG-TERM     TO OUT-ORIG-SCHED-TERM
001286     END-IF
001287
001288     IF EN-LF-NEW-TERM > EN-AH-NEW-TERM
001289        MOVE EN-LF-NEW-TERM      TO OUT-NEW-SCHED-TERM
001290     ELSE
001291        MOVE EN-AH-NEW-TERM      TO OUT-NEW-SCHED-TERM
001292     END-IF
001293     COMPUTE WS-WORK-PREM =
001294        (EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT +
001295        EN-AH-NEW-PRM-AMT) -
001296        (EN-LF-ORIG-PRM-AMT + EN-LF-ORIG-ALT-PRM-AMT +
001297        EN-AH-ORIG-PRM-AMT)
001298     MOVE WS-WORK-PREM           TO OUT-TOT-PRM-CHG
001299
001300     IF EN-CSO-PORTION NOT NUMERIC
001301        MOVE ZEROS               TO EN-CSO-PORTION
001302     END-IF
001303     IF EN-ACCT-PORTION NOT NUMERIC
001304        MOVE ZERO                TO EN-ACCT-PORTION
001305     END-IF
001306*    MOVE EN-CSO-PORTION         TO OUT-CSO-PORTION
001307*    MOVE EN-ACCT-PORTION        TO OUT-ACCT-PORTION
001308     IF WS-WORK-PREM > +0
001309        MOVE 'CSO'               TO OUT-PAYEE
001310     ELSE
001311      IF WS-WORK-PREM = +0
001312         MOVE 'ZERO'             TO OUT-PAYEE
001313      ELSE
001314         MOVE 'ACCT'             TO OUT-PAYEE
001315      END-IF
001316     END-IF
001317
001318     COMPUTE WS-ACCT-ORIG-LF-PORT =
001319        EN-LF-ORIG-PRM-AMT * EN-LF-ORIG-COMM-PCT
001320     MOVE WS-ACCT-ORIG-LF-PORT TO OUT-ACCT-ORIG-LF-PORT
001321     COMPUTE WS-ACCT-ORIG-ALT-LF-PORT =
001322        EN-LF-ORIG-ALT-PRM-AMT * EN-LF-ORIG-COMM-PCT
001323     MOVE WS-ACCT-ORIG-ALT-LF-PORT TO OUT-ACCT-ORIG-ALT-LF-PORT
001324     COMPUTE WS-ACCT-ORIG-CMB-LF-PORT =
001325        (EN-LF-ORIG-PRM-AMT + EN-LF-ORIG-ALT-PRM-AMT)
001326        * EN-LF-ORIG-COMM-PCT
001327     MOVE WS-ACCT-ORIG-CMB-LF-PORT TO OUT-ACCT-ORIG-CMB-LF-PORT
001328     COMPUTE WS-ACCT-ORIG-AH-PORT =
001329        EN-AH-ORIG-PRM-AMT * EN-AH-ORIG-COMM-PCT
001330     MOVE WS-ACCT-ORIG-AH-PORT TO OUT-ACCT-ORIG-AH-PORT
001331     COMPUTE WS-ACCT-ORIG-PORTION =
001332        WS-ACCT-ORIG-CMB-LF-PORT + WS-ACCT-ORIG-AH-PORT
001333     MOVE WS-ACCT-ORIG-PORTION TO OUT-ACCT-ORIG-PORTION
001334
001335     COMPUTE WS-CSO-ORIG-LF-PORT =
001336       EN-LF-ORIG-PRM-AMT - WS-ACCT-ORIG-LF-PORT
001337     MOVE WS-CSO-ORIG-LF-PORT TO OUT-CSO-ORIG-LF-PORT
001338     COMPUTE WS-CSO-ORIG-ALT-LF-PORT =
001339       EN-LF-ORIG-ALT-PRM-AMT - WS-ACCT-ORIG-ALT-LF-PORT
001340     MOVE WS-CSO-ORIG-ALT-LF-PORT TO OUT-CSO-ORIG-ALT-LF-PORT
001341     COMPUTE WS-CSO-ORIG-CMB-LF-PORT =
001342       (EN-LF-ORIG-PRM-AMT + EN-LF-ORIG-ALT-PRM-AMT) -
001343        WS-ACCT-ORIG-CMB-LF-PORT
001344     MOVE WS-CSO-ORIG-CMB-LF-PORT TO OUT-CSO-ORIG-CMB-LF-PORT
001345     COMPUTE WS-CSO-ORIG-AH-PORT =
001346        EN-AH-ORIG-PRM-AMT - WS-ACCT-ORIG-AH-PORT
001347     MOVE WS-CSO-ORIG-AH-PORT TO OUT-CSO-ORIG-AH-PORT
001348     COMPUTE WS-CSO-ORIG-PORTION =
001349        WS-CSO-ORIG-CMB-LF-PORT + WS-CSO-ORIG-AH-PORT
001350     MOVE WS-CSO-ORIG-PORTION TO OUT-CSO-ORIG-PORTION
001351
001352     COMPUTE WS-ACCT-NEW-LF-PORT =
001353        EN-LF-NEW-PRM-AMT * EN-LF-NEW-COMM-PCT
001354     MOVE WS-ACCT-NEW-LF-PORT TO OUT-ACCT-NEW-LF-PORT
001355     COMPUTE WS-ACCT-NEW-ALT-LF-PORT =
001356        EN-LF-NEW-ALT-PRM-AMT * EN-LF-NEW-COMM-PCT
001357     MOVE WS-ACCT-NEW-ALT-LF-PORT TO OUT-ACCT-NEW-ALT-LF-PORT
001358     COMPUTE WS-ACCT-NEW-CMB-LF-PORT =
001359        (EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT)
001360        * EN-LF-NEW-COMM-PCT
001361     MOVE WS-ACCT-NEW-CMB-LF-PORT TO OUT-ACCT-NEW-CMB-LF-PORT
001362     COMPUTE WS-ACCT-NEW-AH-PORT =
001363        EN-AH-NEW-PRM-AMT * EN-AH-NEW-COMM-PCT
001364     MOVE WS-ACCT-NEW-AH-PORT TO OUT-ACCT-NEW-AH-PORT
001365     COMPUTE WS-ACCT-NEW-PORTION =
001366        WS-ACCT-NEW-CMB-LF-PORT + WS-ACCT-NEW-AH-PORT
001367     MOVE WS-ACCT-NEW-PORTION TO OUT-ACCT-NEW-PORTION
001368
001369     COMPUTE WS-CSO-NEW-LF-PORT =
001370       EN-LF-NEW-PRM-AMT - WS-ACCT-NEW-LF-PORT
001371     MOVE WS-CSO-NEW-LF-PORT TO OUT-CSO-NEW-LF-PORT
001372     COMPUTE WS-CSO-NEW-ALT-LF-PORT =
001373       EN-LF-NEW-ALT-PRM-AMT - WS-ACCT-NEW-ALT-LF-PORT
001374     MOVE WS-CSO-NEW-ALT-LF-PORT TO OUT-CSO-NEW-ALT-LF-PORT
001375     COMPUTE WS-CSO-NEW-CMB-LF-PORT =
001376       (EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT) -
001377        WS-ACCT-NEW-CMB-LF-PORT
001378     MOVE WS-CSO-NEW-CMB-LF-PORT TO OUT-CSO-NEW-CMB-LF-PORT
001379     COMPUTE WS-CSO-NEW-AH-PORT =
001380        EN-AH-NEW-PRM-AMT - WS-ACCT-NEW-AH-PORT
001381     MOVE WS-CSO-NEW-AH-PORT TO OUT-CSO-NEW-AH-PORT
001382     COMPUTE WS-CSO-NEW-PORTION =
001383        WS-CSO-NEW-CMB-LF-PORT + WS-CSO-NEW-AH-PORT
001384     MOVE WS-CSO-NEW-PORTION TO OUT-CSO-NEW-PORTION
001385
001386     COMPUTE WS-ACCT-LF-PORT-CHG =
001387        WS-ACCT-NEW-LF-PORT - WS-ACCT-ORIG-LF-PORT
001388     MOVE WS-ACCT-LF-PORT-CHG TO OUT-ACCT-LF-PORT-CHG
001389     COMPUTE WS-ACCT-ALT-LF-PORT-CHG =
001390        WS-ACCT-NEW-ALT-LF-PORT - WS-ACCT-ORIG-ALT-LF-PORT
001391     MOVE WS-ACCT-ALT-LF-PORT-CHG TO OUT-ACCT-ALT-LF-PORT-CHG
001392     COMPUTE WS-ACCT-CMB-LF-PORT-CHG =
001393        WS-ACCT-NEW-CMB-LF-PORT - WS-ACCT-ORIG-CMB-LF-PORT
001394     MOVE WS-ACCT-CMB-LF-PORT-CHG TO OUT-ACCT-CMB-LF-PORT-CHG
001395     COMPUTE WS-ACCT-AH-PORT-CHG =
001396        WS-ACCT-NEW-AH-PORT - WS-ACCT-ORIG-AH-PORT
001397     MOVE WS-ACCT-AH-PORT-CHG TO OUT-ACCT-AH-PORT-CHG
001398     COMPUTE WS-ACCT-PORTION-CHG =
001399        WS-ACCT-NEW-PORTION - WS-ACCT-ORIG-PORTION
001400     MOVE WS-ACCT-PORTION-CHG TO OUT-ACCT-PORTION-CHG
001401
001402     COMPUTE WS-CSO-LF-PORT-CHG =
001403        WS-CSO-NEW-LF-PORT - WS-CSO-ORIG-LF-PORT
001404     MOVE WS-CSO-LF-PORT-CHG TO OUT-CSO-LF-PORT-CHG
001405     COMPUTE WS-CSO-ALT-LF-PORT-CHG =
001406        WS-CSO-NEW-ALT-LF-PORT - WS-CSO-ORIG-ALT-LF-PORT
001407     MOVE WS-CSO-ALT-LF-PORT-CHG TO OUT-CSO-ALT-LF-PORT-CHG
001408     COMPUTE WS-CSO-CMB-LF-PORT-CHG =
001409        WS-CSO-NEW-CMB-LF-PORT - WS-CSO-ORIG-CMB-LF-PORT
001410     MOVE WS-CSO-CMB-LF-PORT-CHG TO OUT-CSO-CMB-LF-PORT-CHG
001411     COMPUTE WS-CSO-AH-PORT-CHG =
001412        WS-CSO-NEW-AH-PORT - WS-CSO-ORIG-AH-PORT
001413     MOVE WS-CSO-AH-PORT-CHG TO OUT-CSO-AH-PORT-CHG
001414     COMPUTE WS-CSO-PORTION-CHG =
001415        WS-CSO-NEW-PORTION - WS-CSO-ORIG-PORTION
001416     MOVE WS-CSO-PORTION-CHG TO OUT-CSO-PORTION-CHG
001417
001418     COMPUTE WS-WORK-PREM =
001419         EN-LF-NEW-PRM-AMT - EN-LF-ORIG-PRM-AMT
001420     MOVE WS-WORK-PREM           TO OUT-LF-PREM-CHG
001421     COMPUTE WS-WORK-PREM =
001422         EN-LF-NEW-ALT-PRM-AMT - EN-LF-ORIG-ALT-PRM-AMT
001423     MOVE WS-WORK-PREM           TO OUT-LF-ALT-PREM-CHG
001424     COMPUTE WS-WORK-PREM =
001425        (EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT) -
001426        (EN-LF-ORIG-PRM-AMT + EN-LF-ORIG-ALT-PRM-AMT)
001427     MOVE WS-WORK-PREM           TO OUT-LF-CMB-PREM-CHG
001428     COMPUTE WS-WORK-PREM =
001429        (EN-AH-NEW-PRM-AMT - EN-AH-ORIG-PRM-AMT)
001430     MOVE WS-WORK-PREM           TO OUT-AH-PREM-CHG
001431
001432
001433
001434     move en-sig-sw              to out-sig-sw
001435     MOVE EN-HEALTH-APP          TO OUT-HEALTH-APP
001436     MOVE EN-COMM-CHGBK          TO OUT-CHGBACK
001437
001438     GO TO 0620-CONTINUE
001439
001440     .
001441 0620-PROCESS-CANCEL.
001442
001443     IF EN-LF-ORIG-REF-DT NOT = LOW-VALUES AND SPACES
001444        MOVE EN-LF-ORIG-REF-DT   TO DC-BIN-DATE-1
001445        MOVE ' '                 TO DC-OPTION-CODE
001446        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
001447        IF NO-CONVERSION-ERROR
001448           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
001449              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
001450                 INTO OUT-ORIG-LF-CAN-DT
001451           END-STRING
001452        END-IF
001453        MOVE OUT-ORIG-LF-CAN-DT  TO OUT-ORIG-CAN-DT
001454     END-IF
001455
001456     IF EN-LF-NEW-REF-DT NOT = LOW-VALUES AND SPACES
001457        MOVE EN-LF-NEW-REF-DT    TO DC-BIN-DATE-1
001458        MOVE ' '                 TO DC-OPTION-CODE
001459        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
001460        IF NO-CONVERSION-ERROR
001461           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
001462              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
001463                 INTO OUT-NEW-LF-CAN-DT
001464           END-STRING
001465        END-IF
001466        MOVE OUT-NEW-LF-CAN-DT   TO OUT-NEW-CAN-DT
001467     END-IF
001468
001469     IF EN-AH-ORIG-REF-DT NOT = LOW-VALUES AND SPACES
001470        MOVE EN-AH-ORIG-REF-DT      TO DC-BIN-DATE-1
001471        MOVE ' '                    TO DC-OPTION-CODE
001472        PERFORM 9700-DATE-LINK      THRU 9700-EXIT
001473        IF NO-CONVERSION-ERROR
001474           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
001475              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
001476                 INTO OUT-ORIG-AH-CAN-DT
001477           END-STRING
001478        END-IF
001479        IF EN-AH-ORIG-REF-DT > EN-LF-ORIG-REF-DT
001480           MOVE OUT-ORIG-AH-CAN-DT  TO OUT-ORIG-CAN-DT
001481        END-IF
001482     END-IF
001483
001484     IF EN-AH-NEW-REF-DT NOT = LOW-VALUES AND SPACES
001485        MOVE EN-AH-NEW-REF-DT       TO DC-BIN-DATE-1
001486        MOVE ' '                    TO DC-OPTION-CODE
001487        PERFORM 9700-DATE-LINK      THRU 9700-EXIT
001488        IF NO-CONVERSION-ERROR
001489           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
001490              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
001491                 INTO OUT-NEW-AH-CAN-DT
001492           END-STRING
001493        END-IF
001494        IF EN-AH-NEW-REF-DT > EN-LF-NEW-REF-DT
001495           MOVE OUT-NEW-AH-CAN-DT  TO OUT-NEW-CAN-DT
001496        END-IF
001497     END-IF
001498
001499     MOVE EN-LF-ORIG-REF-AMT     TO OUT-ORIG-LF-REF
001500     MOVE EN-AH-ORIG-REF-AMT     TO OUT-ORIG-AH-REF
001501     MOVE EN-LF-NEW-REF-AMT      TO OUT-NEW-LF-REF
001502     MOVE EN-AH-NEW-REF-AMT      TO OUT-NEW-AH-REF
001503     COMPUTE WS-WORK-PREM =
001504        EN-LF-ORIG-REF-AMT + EN-AH-ORIG-REF-AMT
001505     MOVE WS-WORK-PREM           TO OUT-ORIG-TOT-REF
001506     COMPUTE WS-WORK-PREM =
001507        EN-LF-NEW-REF-AMT + EN-AH-NEW-REF-AMT
001508     MOVE WS-WORK-PREM           TO OUT-NEW-TOT-REF
001509
001510     COMPUTE WS-WORK-PREM =
001511        (EN-LF-NEW-REF-AMT + EN-AH-NEW-REF-AMT) -
001512        (EN-LF-ORIG-REF-AMT + EN-AH-ORIG-REF-AMT)
001513     MOVE WS-WORK-PREM           TO OUT-TOT-REF-CHG
001514
001515     COMPUTE WS-WORK-PREM =
001516        (EN-LF-NEW-REF-AMT - EN-LF-ORIG-REF-AMT)
001517     MOVE WS-WORK-PREM           TO OUT-LF-REF-CHG
001518     COMPUTE WS-WORK-PREM =
001519        (EN-AH-NEW-REF-AMT - EN-AH-ORIG-REF-AMT)
001520     MOVE WS-WORK-PREM           TO OUT-AH-REF-CHG
001521
001522*     MOVE EN-CSO-PORTION         TO OUT-CSO-PORTION
001523*     MOVE EN-ACCT-PORTION        TO OUT-ACCT-PORTION
001524     MOVE EN-COMM-CHGBK          TO OUT-CHGBACK
001525
001526     IF EN-COMM-CHGBK = 'N'
001527         MOVE ZERO              TO OUT-CSO-ORIG-LF-PORT
001528                                   OUT-CSO-ORIG-ALT-LF-PORT
001529         MOVE EN-LF-ORIG-REF-AMT TO OUT-CSO-ORIG-CMB-LF-PORT
001530         MOVE EN-AH-ORIG-REF-AMT TO OUT-CSO-ORIG-AH-PORT
001531         COMPUTE WS-CSO-ORIG-PORTION =
001532             EN-LF-ORIG-REF-AMT + EN-AH-ORIG-REF-AMT
001533         MOVE WS-CSO-ORIG-PORTION TO OUT-CSO-ORIG-PORTION
001534         MOVE ZERO              TO OUT-ACCT-ORIG-LF-PORT
001535                                   OUT-ACCT-ORIG-ALT-LF-PORT
001536                                   OUT-ACCT-ORIG-CMB-LF-PORT
001537                                   OUT-ACCT-ORIG-AH-PORT
001538                                   OUT-ACCT-ORIG-PORTION
001539
001540         MOVE ZERO              TO OUT-CSO-NEW-LF-PORT
001541                                   OUT-CSO-NEW-ALT-LF-PORT
001542         MOVE EN-LF-NEW-REF-AMT TO OUT-CSO-NEW-CMB-LF-PORT
001543         MOVE EN-AH-NEW-REF-AMT TO OUT-CSO-NEW-AH-PORT
001544         COMPUTE WS-CSO-NEW-PORTION =
001545             EN-LF-NEW-REF-AMT + EN-AH-NEW-REF-AMT
001546         MOVE WS-CSO-NEW-PORTION TO OUT-CSO-NEW-PORTION
001547         MOVE ZERO              TO OUT-ACCT-NEW-LF-PORT
001548                                   OUT-ACCT-NEW-ALT-LF-PORT
001549                                   OUT-ACCT-NEW-CMB-LF-PORT
001550                                   OUT-ACCT-NEW-AH-PORT
001551                                   OUT-ACCT-NEW-PORTION
001552
001553         MOVE ZERO              TO OUT-CSO-LF-PORT-CHG
001554                                   OUT-CSO-ALT-LF-PORT-CHG
001555         COMPUTE WS-CSO-CMB-LF-PORT-CHG =
001556           EN-LF-NEW-REF-AMT - EN-LF-ORIG-REF-AMT
001557         MOVE WS-CSO-CMB-LF-PORT-CHG TO OUT-CSO-CMB-LF-PORT-CHG
001558         COMPUTE WS-CSO-AH-PORT-CHG =
001559           EN-AH-NEW-REF-AMT - EN-AH-ORIG-REF-AMT
001560         MOVE WS-CSO-AH-PORT-CHG TO OUT-CSO-AH-PORT-CHG
001561         COMPUTE WS-CSO-PORTION-CHG =
001562           (EN-LF-NEW-REF-AMT + EN-AH-NEW-REF-AMT) -
001563           (EN-LF-ORIG-REF-AMT + EN-AH-ORIG-REF-AMT)
001564         MOVE WS-CSO-PORTION-CHG TO OUT-CSO-PORTION-CHG
001565         MOVE ZERO              TO OUT-ACCT-LF-PORT-CHG
001566                                   OUT-ACCT-ALT-LF-PORT-CHG
001567                                   OUT-ACCT-CMB-LF-PORT-CHG
001568                                   OUT-ACCT-AH-PORT-CHG
001569                                   OUT-ACCT-PORTION-CHG
001570     ELSE
001571         MOVE ZERO              TO OUT-ACCT-ORIG-LF-PORT
001572                                   OUT-ACCT-ORIG-ALT-LF-PORT
001573         COMPUTE WS-ACCT-ORIG-CMB-LF-PORT =
001574           (EN-LF-ORIG-REF-AMT * EN-LF-ORIG-REF-COMM-PCT)
001575         MOVE WS-ACCT-ORIG-CMB-LF-PORT TO
001576                                  OUT-ACCT-ORIG-CMB-LF-PORT
001577         COMPUTE WS-ACCT-ORIG-AH-PORT =
001578           (EN-AH-ORIG-REF-AMT * EN-AH-ORIG-REF-COMM-PCT)
001579         MOVE WS-ACCT-ORIG-AH-PORT TO OUT-ACCT-ORIG-AH-PORT
001580         COMPUTE WS-ACCT-ORIG-PORTION =
001581            WS-ACCT-ORIG-CMB-LF-PORT + WS-ACCT-ORIG-AH-PORT
001582         MOVE WS-ACCT-ORIG-PORTION TO OUT-ACCT-ORIG-PORTION
001583
001584         MOVE ZERO              TO OUT-CSO-ORIG-LF-PORT
001585                                   OUT-CSO-ORIG-ALT-LF-PORT
001586         COMPUTE WS-CSO-ORIG-CMB-LF-PORT =
001587           (EN-LF-ORIG-REF-AMT - WS-ACCT-ORIG-CMB-LF-PORT)
001588         MOVE WS-CSO-ORIG-CMB-LF-PORT TO OUT-CSO-ORIG-CMB-LF-PORT
001589         COMPUTE WS-CSO-ORIG-AH-PORT =
001590           (EN-AH-ORIG-REF-AMT - WS-ACCT-ORIG-AH-PORT)
001591         MOVE WS-CSO-AH-PORTION  TO OUT-CSO-ORIG-AH-PORT
001592         COMPUTE WS-CSO-ORIG-PORTION =
001593            WS-CSO-ORIG-CMB-LF-PORT + WS-CSO-ORIG-AH-PORT
001594         MOVE WS-CSO-ORIG-PORTION TO OUT-CSO-ORIG-PORTION
001595
001596         MOVE ZERO              TO OUT-ACCT-NEW-LF-PORT
001597                                   OUT-ACCT-NEW-ALT-LF-PORT
001598         COMPUTE WS-ACCT-NEW-CMB-LF-PORT =
001599           (EN-LF-NEW-REF-AMT * EN-LF-NEW-REF-COMM-PCT)
001600         MOVE WS-ACCT-NEW-CMB-LF-PORT TO OUT-ACCT-NEW-CMB-LF-PORT
001601         COMPUTE WS-ACCT-NEW-AH-PORT =
001602           (EN-AH-NEW-REF-AMT * EN-AH-NEW-REF-COMM-PCT)
001603         MOVE WS-ACCT-NEW-AH-PORT TO OUT-ACCT-NEW-AH-PORT
001604         COMPUTE WS-ACCT-NEW-PORTION =
001605            WS-ACCT-NEW-CMB-LF-PORT + WS-ACCT-NEW-AH-PORT
001606         MOVE WS-ACCT-NEW-PORTION TO OUT-ACCT-NEW-PORTION
001607
001608         MOVE ZERO              TO OUT-CSO-NEW-LF-PORT
001609                                   OUT-CSO-NEW-ALT-LF-PORT
001610         COMPUTE WS-CSO-NEW-CMB-LF-PORT =
001611           (EN-LF-NEW-REF-AMT - WS-ACCT-NEW-CMB-LF-PORT)
001612         MOVE WS-CSO-NEW-CMB-LF-PORT TO OUT-CSO-NEW-CMB-LF-PORT
001613         COMPUTE WS-CSO-NEW-AH-PORT =
001614           (EN-AH-NEW-REF-AMT - WS-ACCT-NEW-AH-PORT)
001615         MOVE WS-CSO-NEW-AH-PORT TO OUT-CSO-NEW-AH-PORT
001616         COMPUTE WS-CSO-NEW-PORTION =
001617            WS-CSO-NEW-CMB-LF-PORT + WS-CSO-NEW-AH-PORT
001618         MOVE WS-CSO-NEW-PORTION TO OUT-CSO-NEW-PORTION
001619
001620         MOVE ZERO               TO OUT-ACCT-LF-PORT-CHG
001621                                    OUT-ACCT-ALT-LF-PORT-CHG
001622         COMPUTE WS-ACCT-CMB-LF-PORT-CHG =
001623            WS-ACCT-NEW-CMB-LF-PORT - WS-ACCT-ORIG-CMB-LF-PORT
001624         MOVE WS-ACCT-CMB-LF-PORT-CHG TO OUT-ACCT-CMB-LF-PORT-CHG
001625         COMPUTE WS-ACCT-AH-PORT-CHG =
001626            WS-ACCT-NEW-AH-PORT - WS-ACCT-ORIG-AH-PORT
001627         MOVE WS-ACCT-AH-PORT-CHG TO OUT-ACCT-AH-PORT-CHG
001628         COMPUTE WS-ACCT-PORTION-CHG =
001629            WS-ACCT-NEW-PORTION - WS-ACCT-ORIG-PORTION
001630         MOVE WS-ACCT-PORTION-CHG TO OUT-ACCT-PORTION-CHG
001631
001632         MOVE ZERO               TO OUT-CSO-LF-PORT-CHG
001633                                    OUT-CSO-ALT-LF-PORT-CHG
001634         COMPUTE WS-CSO-CMB-LF-PORT-CHG =
001635            WS-CSO-NEW-CMB-LF-PORT - WS-CSO-ORIG-CMB-LF-PORT
001636         MOVE WS-CSO-CMB-LF-PORT-CHG TO OUT-CSO-CMB-LF-PORT-CHG
001637         COMPUTE WS-CSO-AH-PORT-CHG =
001638            WS-CSO-NEW-AH-PORT - WS-CSO-ORIG-AH-PORT
001639         MOVE WS-CSO-AH-PORT-CHG TO OUT-CSO-AH-PORT-CHG
001640         COMPUTE WS-CSO-PORTION-CHG =
001641            WS-CSO-NEW-PORTION - WS-CSO-ORIG-PORTION
001642         MOVE WS-CSO-PORTION-CHG TO OUT-CSO-PORTION-CHG
001643
001644     END-IF
001645
001646     .
001647 0620-CONTINUE.
001648
001649     move en-reason-code (1)     to out-rea-cd1
001650     move en-reason-code (2)     to out-rea-cd2
001651     move en-reason-code (3)     to out-rea-cd3
001652     move en-reason-code (4)     to out-rea-cd4
001653     move en-reason-code (5)     to out-rea-cd5
001654     move en-reason-code (6)     to out-rea-cd6
001655     move en-reason-code (7)     to out-rea-cd7
001656     move en-reason-code (8)     to out-rea-cd8
001657     move en-reason-code (9)     to out-rea-cd9
001658     move en-reason-code (10)    to out-rea-cd10
001659     move en-reason-code (11)    to out-rea-cd11
001660     move en-reason-code (12)    to out-rea-cd12
001661
001662*    PERFORM 1300-DETERMINE-COMM
001663*                                THRU 1300-EXIT
001664
001665     .
001666 0620-EXIT.
001667     EXIT.
001668
001669 0650-PROCESS-NO-Change.
001670
001671     IF EN-REC-TYPE = 'C'
001672        GO TO 0650-PROCESS-CANCEL
001673     END-IF
001674
001675
001676     if ((OUT-NEW-ILNAME = OUT-ORIG-ILNAME)
001677        AND (OUT-NEW-ILNAME NOT = SPACES))
001678                      AND
001679        ((OUT-NEW-IFNAME = OUT-ORIG-IFNAME)
001680        AND (OUT-NEW-IFNAME NOT = SPACES))
001681                      AND
001682        ((OUT-NEW-IMINIT = OUT-ORIG-MINIT)
001683        AND (OUT-NEW-IFNAME NOT = SPACES))
001684        move 'No Change'         to out-new-Ifname
001685        MOVE SPACES              TO OUT-NEW-IMINIT
001686        move spaces              to out-new-Ilname
001687     ELSE
001688         IF OUT-NEW-ILNAME = SPACES
001689           AND OUT-NEW-IFNAME = SPACES
001690           AND OUT-NEW-IMINIT = SPACES
001691           AND OUT-ORIG-ILNAME NOT = SPACES
001692            MOVE 'None'          TO OUT-NEW-IFNAME
001693            MOVE SPACES          TO OUT-NEW-IMINIT
001694            MOVE SPACES          TO OUT-NEW-ILNAME
001695         END-IF
001696     END-IF
001697
001698     IF ((OUT-NEW-JLNAME = OUT-ORIG-JLNAME)
001699        AND (OUT-NEW-JLNAME NOT = SPACES))
001700                    AND
001701        ((OUT-NEW-JFNAME = OUT-ORIG-JFNAME)
001702        AND (OUT-NEW-JFNAME NOT = SPACES))
001703                    AND
001704        ((OUT-NEW-JMINIT = OUT-ORIG-JMINIT)
001705        AND (OUT-NEW-JFNAME NOT = SPACES))
001706        MOVE 'No Change'         TO OUT-NEW-JFNAME
001707        MOVE SPACES              TO OUT-NEW-JMINIT
001708        MOVE SPACES              TO OUT-NEW-JLNAME
001709     ELSE
001710         IF OUT-NEW-JLNAME = SPACES
001711           AND OUT-NEW-JFNAME = SPACES
001712           AND OUT-NEW-JMINIT = SPACES
001713           AND OUT-ORIG-JLNAME NOT = SPACES
001714            MOVE 'None'          TO OUT-NEW-JFNAME
001715            MOVE SPACES          TO OUT-NEW-JMINIT
001716            MOVE SPACES          TO OUT-NEW-JLNAME
001717         END-IF
001718     END-IF
001719
001720     IF (EN-LF-NEW-BEN-AMT = EN-LF-ORIG-BEN-AMT)
001721        AND (EN-LF-NEW-BENCD NOT EQUAL '00' AND SPACES)
001722        MOVE 'No Change'         TO OUT-NEW-LF-BEN-A
001723     ELSE
001724         IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
001725           AND (EN-LF-NEW-BENCD = '00' OR SPACES)
001726             MOVE ZEROS          TO OUT-NEW-LF-BEN
001727         END-IF
001728     END-IF
001729
001730     IF (EN-LF-NEW-PRM-AMT = EN-LF-ORIG-PRM-AMT)
001731        AND (EN-LF-NEW-BENCD NOT EQUAL '00' AND SPACES)
001732        MOVE 'No Change'         TO OUT-NEW-LF-PRM-A
001733     ELSE
001734         IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
001735           AND (EN-LF-NEW-BENCD = '00' OR SPACES)
001736             MOVE ZEROS          TO OUT-NEW-LF-PRM
001737         END-IF
001738     END-IF
001739
001740     IF (EN-LF-NEW-ALT-BEN-AMT = EN-LF-ORIG-ALT-BEN-AMT)
001741        AND (EN-LF-NEW-BENCD NOT EQUAL '00' AND SPACES)
001742        MOVE 'No Change'         TO OUT-NEW-LF-ALT-BEN-A
001743     ELSE
001744         IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
001745           AND (EN-LF-NEW-BENCD = '00' OR SPACES)
001746             MOVE ZEROS          TO OUT-NEW-LF-ALT-BEN
001747         END-IF
001748     END-IF
001749
001750     IF (EN-LF-NEW-ALT-PRM-AMT = EN-LF-ORIG-ALT-PRM-AMT)
001751        AND (EN-LF-NEW-BENCD NOT EQUAL '00' AND SPACES)
001752        MOVE 'No Change'         TO OUT-NEW-LF-ALT-PRM-A
001753     ELSE
001754         IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
001755           AND (EN-LF-NEW-BENCD = '00' OR SPACES)
001756             MOVE ZEROS          TO OUT-NEW-LF-ALT-PRM
001757         END-IF
001758     END-IF
001759
001760     COMPUTE WS-WRK-AMOUNT = EN-LF-ORIG-PRM-AMT +
001761                              EN-LF-ORIG-ALT-PRM-AMT
001762     COMPUTE WS-WRK-AMOUNT2 = EN-LF-NEW-PRM-AMT +
001763                               EN-LF-NEW-ALT-PRM-AMT
001764     IF (WS-WRK-AMOUNT2 = WS-WRK-AMOUNT)
001765        AND (EN-LF-NEW-BENCD NOT EQUAL '00' AND SPACES)
001766        MOVE 'No Change'         TO OUT-NEW-LF-CMB-PREM-A
001767     ELSE
001768         IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
001769           AND (EN-LF-NEW-BENCD = '00' OR SPACES)
001770            MOVE ZEROS          TO OUT-NEW-LF-CMB-PREM
001771         END-IF
001772     END-IF
001773
001774     IF OUT-NEW-SCHED-EXP-DT = OUT-ORIG-SCHED-EXP-DT
001775        MOVE 'No Change'         TO OUT-NEW-SCHED-EXP-DT
001776     END-IF
001777
001778     IF (OUT-NEW-LF-EXP-DT = OUT-ORIG-LF-EXP-DT)
001779        AND (OUT-NEW-LF-EXP-DT NOT = SPACES)
001780        MOVE 'No Change'         TO OUT-NEW-LF-EXP-DT
001781     END-IF
001782
001783     IF (OUT-NEW-LF-DESC = OUT-ORIG-LF-DESC)
001784        AND (OUT-NEW-LF-DESC NOT = SPACES)
001785        MOVE 'No Change'         to OUT-NEW-LF-DESC
001786     ELSE
001787        IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
001788          AND (EN-LF-NEW-BENCD = '00' OR SPACES)
001789            MOVE 'None'          TO OUT-NEW-LF-DESC
001790        END-IF
001791     end-if
001792
001793     IF (OUT-NEW-AH-EXP-DT = OUT-ORIG-AH-EXP-DT)
001794       AND (OUT-NEW-AH-EXP-DT NOT = SPACES)
001795        MOVE 'No Change'          TO OUT-NEW-AH-EXP-DT
001796     END-IF
001797
001798     IF (EN-AH-NEW-BEN-AMT = EN-AH-ORIG-BEN-AMT)
001799        AND (EN-AH-ORIG-BEN-AMT NOT = ZEROS)
001800        MOVE 'No Change'         TO OUT-NEW-AH-BEN-A
001801     ELSE
001802         IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
001803           AND (EN-AH-NEW-BENCD = '00' OR SPACES)
001804             MOVE ZEROS          TO OUT-NEW-AH-BEN
001805         END-IF
001806     END-IF
001807
001808     IF (EN-AH-NEW-PRM-AMT = EN-AH-ORIG-PRM-AMT)
001809        AND (EN-AH-ORIG-PRM-AMT NOT = ZEROS)
001810        MOVE 'No Change'         TO OUT-NEW-AH-PRM-A
001811     ELSE
001812         IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
001813           AND (EN-AH-NEW-BENCD = '00' OR SPACES)
001814             MOVE ZEROS          TO OUT-NEW-AH-PRM
001815         END-IF
001816     END-IF
001817
001818     IF (OUT-NEW-AH-DESC = OUT-ORIG-AH-DESC)
001819        AND (OUT-ORIG-AH-DESC NOT = SPACES)
001820        MOVE 'No Change'         TO OUT-NEW-AH-DESC
001821     ELSE
001822         IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
001823           AND (EN-AH-NEW-BENCD = '00' OR SPACES)
001824             MOVE 'None'         TO OUT-NEW-AH-DESC
001825         END-IF
001826     END-IF
001827
001828     IF EN-INS-ORIG-AGE-DEF-FLAG <> 'Y'
001829         MOVE 'N' TO EN-INS-ORIG-AGE-DEF-FLAG
001830     END-IF
001831     IF EN-INS-NEW-AGE-DEF-FLAG <> 'Y'
001832         MOVE 'N' TO EN-INS-NEW-AGE-DEF-FLAG
001833     END-IF
001834     IF EN-JNT-ORIG-AGE-DEF-FLAG <> 'Y'
001835         MOVE 'N' TO EN-JNT-ORIG-AGE-DEF-FLAG
001836     END-IF
001837     IF EN-JNT-NEW-AGE-DEF-FLAG <> 'Y'
001838         MOVE 'N' TO EN-JNT-NEW-AGE-DEF-FLAG
001839     END-IF
001840
001841     IF EN-INS-NEW-AGE = EN-INS-ORIG-AGE
001842       AND EN-INS-NEW-AGE-DEF-FLAG = EN-INS-ORIG-AGE-DEF-FLAG
001843       AND EN-INS-NEW-AGE-DEF-FLAG <> 'Y'
001844       AND EN-INS-NEW-LAST-NAME = EN-INS-ORIG-LAST-NAME
001845       AND EN-INS-NEW-FIRST-NAME = EN-INS-ORIG-FIRST-NAME
001846       AND EN-INS-NEW-MIDDLE-INIT = EN-INS-ORIG-MIDDLE-INIT
001847        MOVE 'No Change'         TO OUT-NEW-IAGE-A
001848     END-IF
001849
001850     IF (EN-JNT-NEW-AGE = EN-JNT-ORIG-AGE)
001851        AND (EN-JNT-ORIG-AGE NOT = ZEROS)
001852        AND EN-JNT-NEW-AGE-DEF-FLAG = EN-JNT-ORIG-AGE-DEF-FLAG
001853        AND EN-JNT-NEW-AGE-DEF-FLAG <> 'Y'
001854        AND EN-JNT-NEW-LAST-NAME = EN-JNT-ORIG-LAST-NAME
001855        AND EN-JNT-NEW-FIRST-NAME = EN-JNT-ORIG-FIRST-NAME
001856        AND EN-JNT-NEW-MIDDLE-INIT = EN-JNT-ORIG-MIDDLE-INIT
001857        MOVE 'No Change'         TO OUT-NEW-JAGE-A
001858     ELSE
001859         IF (EN-JNT-ORIG-AGE > ZEROS)
001860           AND (EN-JNT-ORIG-AGE-DEF-FLAG <> 'Y')
001861           AND (EN-JNT-NEW-AGE = ZEROS OR SPACES)
001862             MOVE 'None'         TO OUT-NEW-JAGE-A
001863         END-IF
001864     END-IF
001865
001866     IF (EN-LF-NEW-TERM = EN-LF-ORIG-TERM)
001867        AND (EN-LF-ORIG-TERM NOT = ZEROS)
001868        MOVE 'No Change'         TO OUT-NEW-LF-TERM-A
001869     ELSE
001870         IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
001871           AND (EN-LF-NEW-BENCD = '00' OR SPACES)
001872             MOVE ZEROS          TO OUT-NEW-LF-TERM
001873         END-IF
001874     END-IF
001875
001876     IF (EN-AH-NEW-TERM = EN-AH-ORIG-TERM)
001877        AND (EN-AH-ORIG-TERM NOT = ZEROS)
001878        MOVE 'No Change'         TO OUT-NEW-AH-TERM-A
001879     ELSE
001880         IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
001881           AND (EN-AH-NEW-BENCD = '00' OR SPACES)
001882             MOVE ZEROS          TO OUT-NEW-AH-TERM
001883         END-IF
001884     END-IF
001885
001886     IF (EN-AH-NEW-CP = EN-AH-ORIG-CP)
001887        AND (EN-AH-NEW-BENCD NOT EQUAL '00' AND SPACES)
001888        MOVE 'No Change'         TO OUT-NEW-CRIT-PER-A
001889                                    OUT-NEW-MAX-PMTS-A
001890     ELSE
001891         IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
001892           AND (EN-AH-NEW-BENCD = '00' OR SPACES)
001893             MOVE 'None'         TO OUT-NEW-CRIT-PER-A
001894                                    OUT-NEW-MAX-PMTS-A
001895         END-IF
001896     END-IF
001897
001898     IF OUT-NEW-SCHED-TERM = OUT-ORIG-SCHED-TERM
001899        MOVE 'No Change'         TO OUT-NEW-SCHED-TERM-A
001900     END-IF
001901
001902     IF (OUT-NEW-WAIT-PER = OUT-ORIG-WAIT-PER)
001903       AND (OUT-ORIG-WAIT-PER NOT = SPACES)
001904         MOVE 'No Change'        TO OUT-NEW-WAIT-PER
001905     ELSE
001906         IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
001907           AND (EN-AH-NEW-BENCD = '00' OR SPACES)
001908             MOVE 'None'         TO OUT-NEW-WAIT-PER
001909         END-IF
001910     END-IF
001911
001912     .
001913 0650-PROCESS-CANCEL.
001914
001915     .
001916 0650-EXIT.
001917     EXIT.
001918
001919 0700-WRITE-EXTR.
001920
001921     PERFORM 1000-BUILD-ERARCH   THRU 1000-EXIT
001922     IF BL-FAIL
001923        MOVE ' COULDNT BUILD ERARCH ' TO BL-MESSAGE
001924        GO TO 0700-EXIT
001925     END-IF
001926     MOVE WS-COMPANY-CD          TO NSAS-COMPANY-CD
001927     MOVE BL-ARCHIVE-NO          TO NSAS-ARCHIVE-NO
001928     MOVE +0                     TO NSAS-SEQ-NO
001929     PERFORM 0710-BUILD-NSASEXTR THRU 0710-EXIT
001930     PERFORM 0720-WRITE-NSASEXTR THRU 0720-EXIT
001931     IF BL-FAIL
001932        MOVE ' COULDNT BUILD STD NSASEXTR ' TO BL-MESSAGE
001933        GO TO 0700-EXIT
001934     END-IF
001935     IF W-LETTER-TO-ACCT NOT = SPACES
001936        MOVE W-LETTER-TO-ACCT   TO NSAS-LETTER-VARIABLES (5:1)
001937        MOVE WS-COMPANY-CD       TO NSAS-COMPANY-CD
001938        MOVE BL-ARCHIVE-NO       TO NSAS-ARCHIVE-NO
001939        MOVE +1                  TO NSAS-SEQ-NO
001940        PERFORM 0720-WRITE-NSASEXTR
001941                                 THRU 0720-EXIT
001942        IF BL-FAIL
001943           MOVE ' COULDNT BUILD ACCT NSASEXTR ' TO BL-MESSAGE
001944           GO TO 0700-EXIT
001945        END-IF
001946     END-IF
001947
001948     IF W-LETTER-TO-BENE NOT = SPACES
001949        MOVE W-LETTER-TO-BENE   TO NSAS-LETTER-VARIABLES (5:1)
001950        MOVE WS-COMPANY-CD       TO NSAS-COMPANY-CD
001951        MOVE BL-ARCHIVE-NO       TO NSAS-ARCHIVE-NO
001952        MOVE +2                  TO NSAS-SEQ-NO
001953        PERFORM 0720-WRITE-NSASEXTR
001954                                 THRU 0720-EXIT
001955        IF BL-FAIL
001956           MOVE ' COULDNT BUILD BENE NSASEXTR ' TO BL-MESSAGE
001957        END-IF
001958     END-IF
001959
001960     IF W-PRINT-CERTIFICATE = 'Y' AND
001961        BL-STATE = 'VA'
001962           MOVE OUT-ACCT-NAME       TO VD-ACCOUNT-NAME
001963           MOVE WS-COMPANY-CD       TO VD-COMPANY-CD
001964           MOVE BL-CARRIER          TO VD-CARRIER
001965           MOVE BL-GROUP            TO VD-GROUPING
001966           MOVE BL-STATE            TO VD-STATE
001967           MOVE BL-ACCOUNT          TO VD-ACCOUNT
001968           MOVE WS-ELCERT-EFF-DT    TO VD-CERT-EFF-DT
001969           MOVE BL-CERT-NO          TO VD-CERT-NO
001970           MOVE OUT-ILNAME          TO VD-INSURED-LAST-NAME
001971           MOVE OUT-IFNAME          TO VD-INSURED-FIRST-NAME
001972           MOVE OUT-IMINIT          TO VD-INSURED-MIDDLE-INIT
001973           MOVE OUT-JLNAME          TO VD-JOINT-LAST-NAME
001974           MOVE OUT-JFNAME          TO VD-JOINT-FIRST-NAME
001975           MOVE OUT-JMINIT          TO VD-JOINT-MIDDLE-INIT
001976           MOVE OUT-BENE-NAME       TO VD-BENEFICIARY-NAME
001977           MOVE OUT-ENTRY-BATCH     TO VD-ENTRY-BATCH
001978           MOVE OUT-CSR-CODE        TO VD-CSR-ID
001979           MOVE CM-LF-BENEFIT-CD    TO VD-LF-BENEFIT-CD
001980           MOVE CM-LF-PREMIUM-AMT   TO VD-LF-PREMIUM-AMT
001981           MOVE CM-LF-BENEFIT-AMT   TO VD-LF-BENEFIT-AMT
001982           MOVE CM-LF-ORIG-TERM     TO VD-LF-TERM
001983           MOVE CM-LF-PREMIUM-RATE  TO VD-LF-RATE
001984           MOVE CM-AH-BENEFIT-CD    TO VD-AH-BENEFIT-CD
001985           MOVE CM-AH-PREMIUM-AMT   TO VD-AH-PREMIUM-AMT
001986           MOVE CM-AH-BENEFIT-AMT   TO VD-AH-BENEFIT-AMT
001987           MOVE CM-AH-ORIG-TERM     TO VD-AH-TERM
001988           MOVE CM-AH-PREMIUM-RATE  TO VD-AH-RATE
001989           MOVE CM-LOAN-TERM        TO VD-LOAN-TERM
001990           MOVE CM-LOAN-APR         TO VD-LOAN-APR
001991           MOVE CM-LOAN-1ST-PMT-DT  TO VD-1ST-PMT-DT
001992           MOVE OUT-INS-ADDR1       TO VD-INSURED-ADDRESS-1
001993           MOVE OUT-INS-ADDR2       TO VD-INSURED-ADDRESS-2
001994           MOVE OUT-INS-CITY        TO VD-INSURED-CITY
001995           MOVE OUT-INS-STATE       TO VD-INSURED-STATE
001996           MOVE OUT-INS-ZIP         TO VD-INSURED-ZIP-CODE
001997           MOVE 'VADS'              TO VD-LETTER-ID
001998           MOVE OUT-PROC-ID         TO VD-PROC-ID
001999           MOVE BL-COMP-ID          TO VD-COMP-ID
002000           MOVE EIBTIME             TO VD-CURRENT-TIME
002001           MOVE SAVE-BIN-DATE       TO VD-CURRENT-DATE
002002           MOVE BL-ARCHIVE-NO       TO VD-ARCHIVE-NO
002003           MOVE VIRGINIA-DISCLOSURE TO WS-PASS-VADS-REC
002004           
      * EXEC CICS LINK
002005*              PROGRAM    (LINK-ELVADS)
002006*              COMMAREA   (WS-PASS-AREA)
002007*              LENGTH     (WS-PASS-AREA-LENGTH)
002008*          END-EXEC
      *    MOVE '."C                   (   #00007363' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037333633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELVADS, 
                 WS-PASS-AREA, 
                 WS-PASS-AREA-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002009     END-IF
002010
002011     .
002012 0700-EXIT.
002013     EXIT.
002014
002015  0710-BUILD-NSASEXTR.
002016
002017***  i added the extra space in front of the 1st ~
002018***  only because there may be a letter to acct or bene
002019
002020     STRING
002021         OUT-LETTER             ' ~'
002022         OUT-PROC-ID            '~'
002023         OUT-PROC-NAME          '~'
002024         OUT-PROC-TITLE         '~'
002025         OUT-CSR-NAME           '~'
002026         OUT-CSR-TITLE          '~'
002027         OUT-CARRIER            '~'
002028         OUT-GROUPING           '~'
002029         OUT-STATE              '~'
002030         OUT-ACCOUNT            '~'
002031         OUT-CERT-EFF-DT        '~'
002032         OUT-CERT-NO            '~'
002033         OUT-CERT-SFX           '~'
002034         OUT-ILNAME             '~'
002035         OUT-IFNAME             '~'
002036         OUT-IFINIT             '~'
002037         OUT-IMINIT             '~'
002038         OUT-IAGE               '~'
002039         OUT-ISEX               '~'
002040         OUT-INS-ADDR1          '~'
002041         OUT-INS-ADDR2          '~'
002042         OUT-INS-CITY           '~'
002043         OUT-INS-STATE          '~'
002044         OUT-INS-ZIP            '~'
002045         OUT-SOC-SEC-NO         '~'
002046         OUT-MEMBER-NO          '~'
002047         OUT-JLNAME             '~'
002048         OUT-JFNAME             '~'
002049         OUT-JMINIT             '~'
002050         OUT-JAGE               '~'
002051         OUT-ACCT-NAME          '~'
002052         OUT-ACCT-ADDR1         '~'
002053         OUT-ACCT-ADDR2         '~'
002054         OUT-ACCT-CITY          '~'
002055         OUT-ACCT-STATE         '~'
002056         OUT-ACCT-ZIP           '~'
002057         OUT-ACCT-PHONE         '~'
002058         OUT-ACCT-CNTRL-NAME    '~'
002059         OUT-ACCT-BUS-TYPE      '~'
002060         OUT-BENE-NAME          '~'
002061         OUT-BENE-ADDR1         '~'
002062         OUT-BENE-ADDR2         '~'
002063         OUT-BENE-CITY          '~'
002064         OUT-BENE-STATE         '~'
002065         OUT-BENE-ZIP           '~'
002066         OUT-CARR-NAME          '~'
002067         OUT-RESP-NO            '~'
002068         OUT-COMP-NAME          '~'
002069         OUT-COMP-MAIL-TO       '~'
002070         OUT-COMP-ADDR1         '~'
002071         OUT-COMP-ADDR2         '~'
002072         OUT-COMP-CITY          '~'
002073         OUT-COMP-STATE         '~'
002074         OUT-COMP-ZIP           '~'
002075         OUT-COMP-PHONE         '~'
002076         OUT-COMP-FAX           '~'
002077         OUT-COMP-STATUS        '~'
002078         OUT-BILL-SW            '~'
002079         OUT-RPT-CD1            '~'
002080         OUT-RPT-CD2            '~'
002081         OUT-ENTRY-DT           '~'
002082         OUT-ENTRY-BATCH        '~'
002083         OUT-ENTRY-STATUS       '~'
002084         OUT-1ST-PMT-DT         '~'
002085         OUT-LOAN-APR           '~'
002086         OUT-LOAN-TERM          '~'
002087         OUT-RATE-CLASS         '~'
002088         OUT-EXT-DAYS           '~'
002089         OUT-CSR-CODE           '~'
002090         OUT-UCODE              '~'
002091         OUT-PREM-TYPE          '~'
002092         OUT-IND-GRP            '~'
002093         OUT-SKIP-CD            '~'
002094         OUT-PMT-MODE           '~'
002095         OUT-LOAN-OFF           '~'
002096         OUT-REIN-TABLE         '~'
002097         OUT-SPEC-REIN          '~'
002098         OUT-LF-BENCD           '~'
002099         OUT-LF-TERM            '~'
002100         OUT-LF-DEV-CD          '~'
002101         OUT-LF-DEV-PCT         '~'
002102         OUT-LF-BEN             '~'
002103         OUT-LF-PRM             '~'
002104         OUT-LF-ALT-BEN         '~'
002105         OUT-LF-ALT-PRM         '~'
002106         OUT-LF-NSP             '~'
002107         OUT-LF-REM-BEN         '~'
002108         OUT-LF-REF             '~'
002109         OUT-LF-DTH             '~'
002110         OUT-LF-RATE            '~'
002111         OUT-LF-ALT-RATE        '~'
002112         OUT-LF-EXP-DT          '~'
002113         OUT-LF-CUR-STATUS      '~'
002114         OUT-LF-CAN-DT          '~'
002115         OUT-LF-CAN-EXIT-DT     '~'
002116         OUT-LF-DTH-DT          '~'
002117         OUT-LF-DTH-EXIT-DT     '~'
002118         OUT-LF-EXIT-BATCH      '~'
002119         OUT-LF-COMM-PCT        '~'
002120         OUT-LF-DESC            '~'
002121         OUT-LF-ABBRV           '~'
002122         OUT-AH-BENCD           '~'
002123         OUT-AH-TERM            '~'
002124         OUT-CRIT-PER           '~'
002125         OUT-AH-DEV-CD          '~'
002126         OUT-AH-DEV-PCT         '~'
002127         OUT-AH-BEN             '~'
002128         OUT-AH-PRM             '~'
002129         OUT-AH-NSP             '~'
002130         OUT-AH-REF             '~'
002131         OUT-AH-CLM             '~'
002132         OUT-AH-TOT-BEN         '~'
002133         OUT-AH-PDTHRU-DT       '~'
002134         OUT-AH-RATE            '~'
002135         OUT-AH-EXP-DT          '~'
002136         OUT-AH-CUR-STATUS      '~'
002137         OUT-AH-CAN-DT          '~'
002138         OUT-AH-CAN-EXIT-DT     '~'
002139         OUT-AH-EXIT-BATCH      '~'
002140         OUT-AH-COMM-PCT        '~'
002141         OUT-AH-DESC            '~'
002142         OUT-RET-ELIM           '~'
002143         OUT-BEN-DAYS           '~'
002144         OUT-WAIT-PER           '~'
002145         OUT-MAX-PMTS           '~'
002146         OUT-TOT-PRM            '~'
002147         OUT-TOT-REF            '~'
002148         OUT-SCHED-EXP-DT       '~'
002149         OUT-SCHED-TERM         '~'
002150         OUT-ORIG-ILNAME        '~'
002151         OUT-ORIG-IFNAME        '~'
002152         OUT-ORIG-MINIT         '~'
002153         OUT-ORIG-IAGE          '~'
002154         OUT-ORIG-JLNAME        '~'
002155         OUT-ORIG-JFNAME        '~'
002156         OUT-ORIG-JMINIT        '~'
002157         OUT-ORIG-JAGE          '~'
002158         OUT-ORIG-LF-BENCD      '~'
002159         OUT-ORIG-LF-TERM       '~'
002160         OUT-ORIG-LF-BEN        '~'
002161         OUT-ORIG-LF-PRM        '~'
002162         OUT-LF-CALC-PRM        '~'
002163         OUT-ORIG-LF-REF        '~'
002164         OUT-LF-CALC-REF        '~'
002165         OUT-ORIG-LF-ALT-BEN    '~'
002166         OUT-ORIG-LF-ALT-PRM    '~'
002167         OUT-ORIG-LF-EXP-DT     '~'
002168         OUT-ORIG-LF-DESC       '~'
002169         OUT-ORIG-AH-BENCD      '~'
002170         OUT-ORIG-AH-TERM       '~'
002171         OUT-ORIG-CRIT-PER      '~'
002172         OUT-ORIG-AH-BEN        '~'
002173         OUT-ORIG-AH-PRM        '~'
002174         OUT-AH-CALC-PRM        '~'
002175         OUT-ORIG-AH-REF        '~'
002176         OUT-AH-CALC-REF        '~'
002177         OUT-ORIG-AH-EXP-DT     '~'
002178         OUT-ORIG-AH-DESC       '~'
002179         OUT-ORIG-RET-ELIM      '~'
002180         OUT-ORIG-BEN-DAYS      '~'
002181         OUT-ORIG-WAIT-PER      '~'
002182         OUT-ORIG-MAX-PMTS      '~'
002183         OUT-ORIG-SCHED-EXP-DT  '~'
002184         OUT-ORIG-SCHED-TERM    '~'
002185         OUT-NEW-ILNAME         '~'
002186         OUT-NEW-IFNAME         '~'
002187         OUT-NEW-IMINIT         '~'
002188         OUT-NEW-IAGE           '~'
002189         OUT-NEW-JLNAME         '~'
002190         OUT-NEW-JFNAME         '~'
002191         OUT-NEW-JMINIT         '~'
002192         OUT-NEW-JAGE           '~'
002193         OUT-NEW-LF-BENCD       '~'
002194         OUT-NEW-LF-TERM        '~'
002195         OUT-NEW-LF-BEN         '~'
002196         OUT-NEW-LF-PRM         '~'
002197         OUT-NEW-LF-REF         '~'
002198         OUT-NEW-LF-ALT-BEN     '~'
002199         OUT-NEW-LF-ALT-PRM     '~'
002200         OUT-NEW-LF-EXP-DT      '~'
002201         OUT-NEW-LF-DESC        '~'
002202         OUT-NEW-AH-BENCD       '~'
002203         OUT-NEW-AH-TERM        '~'
002204         OUT-NEW-CRIT-PER       '~'
002205         OUT-NEW-AH-BEN         '~'
002206         OUT-NEW-AH-PRM         '~'
002207         OUT-NEW-AH-REF         '~'
002208         OUT-NEW-AH-EXP-DT      '~'
002209         OUT-NEW-AH-DESC        '~'
002210         OUT-NEW-RET-ELIM       '~'
002211         OUT-NEW-BEN-DAYS       '~'
002212         OUT-NEW-WAIT-PER       '~'
002213         OUT-NEW-MAX-PMTS       '~'
002214         OUT-NEW-SCHED-EXP-DT   '~'
002215         OUT-NEW-SCHED-TERM     '~'
002216         OUT-TOT-PRM-CHG        '~'
002217         OUT-TOT-REF-CHG        '~'
002218         OUT-PAYEE              '~'
002219         OUT-SIG-SW             '~'
002220         OUT-BALLOON-IND        '~'
002221         OUT-LEASE-IND          '~'
002222         OUT-RESCIND            '~'
002223         OUT-REA-CD1            '~'
002224         OUT-REA-CD2            '~'
002225         OUT-REA-CD3            '~'
002226         OUT-REA-CD4            '~'
002227         OUT-REA-CD5            '~'
002228         OUT-REA-CD6            '~'
002229         OUT-REA-CD7            '~'
002230         OUT-REA-CD8            '~'
002231         OUT-REA-CD9            '~'
002232         OUT-REA-CD10           '~'
002233         OUT-REA-CD11           '~'
002234         OUT-REA-CD12           '~'
002235         OUT-CYCLE-DT           '~'
002236         OUT-ARCH-NO            '~'
002237         OUT-FORM               '~'
002238         OUT-ENC-LINE           '~'
002239         OUT-ATTACH             '~'
002240         OUT-STACK              '~'
002241         OUT-STATE-NAME         '~'
002242         OUT-PRINT-NOW          '~'
002243         OUT-CHGBACK            '~'
002244         OUT-CSO-PORTION        '~'
002245         OUT-ACCT-PORTION       '~'
002246         OUT-LETTER-TYPE        '~'
002247         OUT-PRINT-CERTIFICATE  '~'
002248         OUT-INS-BIRTHDATE      '~'
002249         OUT-JNT-BIRTHDATE      '~'
002250         OUT-TOT-INTEREST       '~'
002251         OUT-ORIG-TOT-PREM      '~'
002252         OUT-ORIG-TOT-REF       '~'
002253         OUT-CANCEL-DT          '~'
002254         OUT-HEALTH-APP         '~'
002255         OUT-CERTIFICATE-ID     '~'
002256         OUT-UNDW-ID            '~'
002257         OUT-NEW-TOT-PREM       '~'
002258         OUT-NEW-TOT-REF        '~'
002259         OUT-COVERAGE-IND       '~'
002260         OUT-ORIG-COVERAGE-IND  '~'
002261         OUT-NEW-COVERAGE-IND   '~'
002262         OUT-LF-CMB-PREM        '~'
002263         OUT-ORIG-LF-CMB-PREM   '~'
002264         OUT-NEW-LF-CMB-PREM    '~'
002265         OUT-LF-PREM-CHG        '~'
002266         OUT-LF-ALT-PREM-CHG    '~'
002267         OUT-LF-CMB-PREM-CHG    '~'
002268         OUT-AH-PREM-CHG        '~'
002269         OUT-LF-REF-CHG         '~'
002270         OUT-AH-REF-CHG         '~'
002271         OUT-ACCT-LF-PORTION    '~'
002272         OUT-ACCT-ALT-LF-PORT   '~'
002273         OUT-ACCT-CMB-LF-PORT   '~'
002274         OUT-ACCT-AH-PORTION    '~'
002275         OUT-CSO-LF-PORTION     '~'
002276         OUT-CSO-ALT-LF-PORT    '~'
002277         OUT-CSO-CMB-LF-PORT    '~'
002278         OUT-CSO-AH-PORTION     '~'
002279         OUT-ACCT-ORIG-LF-PORT  '~'
002280         OUT-ACCT-ORIG-ALT-LF-PORT '~'
002281         OUT-ACCT-ORIG-CMB-LF-PORT '~'
002282         OUT-ACCT-ORIG-AH-PORT  '~'
002283         OUT-ACCT-ORIG-PORTION  '~'
002284         OUT-CSO-ORIG-LF-PORT   '~'
002285         OUT-CSO-ORIG-ALT-LF-PORT '~'
002286         OUT-CSO-ORIG-CMB-LF-PORT '~'
002287         OUT-CSO-ORIG-AH-PORT   '~'
002288         OUT-CSO-ORIG-PORTION   '~'
002289         OUT-ACCT-NEW-LF-PORT   '~'
002290         OUT-ACCT-NEW-ALT-LF-PORT '~'
002291         OUT-ACCT-NEW-CMB-LF-PORT '~'
002292         OUT-ACCT-NEW-AH-PORT   '~'
002293         OUT-ACCT-NEW-PORTION   '~'
002294         OUT-CSO-NEW-LF-PORT    '~'
002295         OUT-CSO-NEW-ALT-LF-PORT '~'
002296         OUT-CSO-NEW-CMB-LF-PORT '~'
002297         OUT-CSO-NEW-AH-PORT    '~'
002298         OUT-CSO-NEW-PORTION    '~'
002299         OUT-ACCT-LF-PORT-CHG   '~'
002300         OUT-ACCT-ALT-LF-PORT-CHG '~'
002301         OUT-ACCT-CMB-LF-PORT-CHG '~'
002302         OUT-ACCT-AH-PORT-CHG   '~'
002303         OUT-ACCT-PORTION-CHG   '~'
002304         OUT-CSO-LF-PORT-CHG    '~'
002305         OUT-CSO-ALT-LF-PORT-CHG '~'
002306         OUT-CSO-CMB-LF-PORT-CHG '~'
002307         OUT-CSO-AH-PORT-CHG    '~'
002308         OUT-CSO-PORTION-CHG    '~'
002309         OUT-ORIG-LF-CAN-DT     '~'
002310         OUT-NEW-LF-CAN-DT      '~'
002311         OUT-ORIG-AH-CAN-DT     '~'
002312         OUT-NEW-AH-CAN-DT      '~'
002313         OUT-ORIG-CAN-DT        '~'
002314         OUT-NEW-CAN-DT         '~'
002315         OUT-SCREEN-ID          '~'
002316         OUT-NEXT-BUS-DT        '~'
002317         OUT-VIN-NUMBER         '~'
002318            DELIMITED BY '   ' INTO NSAS-LETTER-VARIABLES
002319     END-STRING
002320
002321     .
002322 0710-exit.
002323     EXIT.
002324
002325 0720-WRITE-NSASEXTR.
002326
002327     
      * EXEC CICS WRITE
002328*       DATASET    ('NSASEXTR')
002329*       FROM       (NSAS-EXTRACT-RECORD)
002330*       RIDFLD     (NSAS-CONTROL-PRIMARY)
002331*       RESP       (WS-RESPONSE)
002332*    END-EXEC
           MOVE LENGTH OF
            NSAS-EXTRACT-RECORD
             TO DFHEIV11
           MOVE 'NSASEXTR' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00007686' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303037363836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NSAS-EXTRACT-RECORD, 
                 DFHEIV11, 
                 NSAS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002333
002334     IF NOT RESP-NORMAL
002335        display ' bad write nsasextr ' ws-response
002336        SET BL-FAIL TO TRUE
002337     END-IF
002338
002339     .
002340 0720-EXIT.
002341     EXIT.
002342
002343 0800-init-out-data.
002344
002345     MOVE ZEROS                  TO OUT-IAGE
002346                                    OUT-JAGE
002347                                    OUT-LOAN-APR
002348                                    OUT-LOAN-TERM
002349                                    OUT-EXT-DAYS
002350                                    OUT-LF-TERM
002351                                    OUT-AH-TERM
002352                                    OUT-LF-DEV-PCT
002353                                    OUT-AH-DEV-PCT
002354                                    OUT-LF-BEN
002355                                    OUT-AH-BEN
002356                                    OUT-CRIT-PER
002357                                    OUT-MAX-PMTS
002358                                    OUT-SCHED-TERM
002359                                    OUT-ORIG-IAGE
002360                                    OUT-ORIG-JAGE
002361                                    OUT-NEW-IAGE
002362                                    OUT-NEW-JAGE
002363                                    OUT-ORIG-LF-TERM
002364                                    OUT-ORIG-AH-TERM
002365                                    OUT-NEW-LF-TERM
002366                                    OUT-NEW-AH-TERM
002367                                    OUT-ORIG-CRIT-PER
002368                                    OUT-ORIG-MAX-PMTS
002369                                    OUT-ORIG-SCHED-TERM
002370                                    OUT-NEW-SCHED-TERM
002371                                    OUT-NEW-CRIT-PER
002372                                    OUT-NEW-MAX-PMTS
002373                                    OUT-CSO-PORTION
002374                                    OUT-CSO-LF-PORTION
002375                                    OUT-CSO-ALT-LF-PORT
002376                                    OUT-CSO-CMB-LF-PORT
002377                                    OUT-CSO-AH-PORTION
002378                                    OUT-ACCT-PORTION
002379                                    OUT-ACCT-LF-PORTION
002380                                    OUT-ACCT-ALT-LF-PORT
002381                                    OUT-ACCT-CMB-LF-PORT
002382                                    OUT-ACCT-AH-PORTION
002383                                    OUT-LF-PREM-CHG
002384                                    OUT-LF-ALT-PREM-CHG
002385                                    OUT-LF-CMB-PREM-CHG
002386                                    OUT-AH-PREM-CHG
002387                                    OUT-LF-REF-CHG
002388                                    OUT-AH-REF-CHG
002389                                    out-acct-orig-portion
002390                                    out-cso-orig-portion
002391
002392
002393     .
002394 0800-exit.
002395     exit.
002396
002397 1000-BUILD-ERARCH.
002398
002399* DATA SOURCE MEANINGS  BL-DATA-SRCE
002400*    1) FROM ACCT MAINT
002401*    2) FROM CERT UPDATE
002402*    3) FROM COMP MAINT
002403*    4) FROM REVIEW AND CORRECTIONS
002404******************************************************************
002405
002406
002407     MOVE 'LA'                   TO LETTER-ARCHIVE
002408
002409     MOVE BL-ARCHIVE-NO          TO LA-ARCHIVE-NO
002410                                    LA-ARCHIVE-NO-A2
002411                                    LA-ARCHIVE-NO-A3
002412                                    LA-ARCHIVE-NO-A4
002413                                    LA-ARCHIVE-NO-A5
002414                                    LA-ARCHIVE-NO-A6
002415
002416     MOVE WS-COMPANY-CD          TO LA-COMPANY-CD
002417                                    LA-COMPANY-CD-A2
002418                                    LA-COMPANY-CD-A3
002419                                    LA-COMPANY-CD-A4
002420                                    LA-COMPANY-CD-A5
002421                                    LA-COMPANY-CD-A6
002422     MOVE BL-CARRIER             TO LA-CARRIER-A2
002423                                    LA-CARRIER-A3
002424                                    LA-CARRIER-A4
002425                                    LA-CARRIER-A5
002426     MOVE BL-GROUP               TO LA-GROUPING-A2
002427                                    LA-GROUPING-A3
002428                                    LA-GROUPING-A4
002429                                    LA-GROUPING-A5
002430     MOVE BL-ACCOUNT             TO LA-ACCOUNT-A2
002431                                    LA-ACCOUNT-A3
002432                                    LA-ACCOUNT-A4
002433                                    LA-ACCOUNT-A5
002434     MOVE BL-STATE               TO LA-STATE-A2
002435                                    LA-STATE-A3
002436                                    LA-STATE-A4
002437                                    LA-STATE-A5
002438     IF BL-DATA-SRCE = '2' OR '4'
002439        MOVE WS-ELCERT-EFF-DT    TO LA-EFFECT-DATE-A2
002440        MOVE BL-CERT-NO          TO LA-CERT-NO-A2
002441     ELSE
002442        MOVE LOW-VALUES          TO LA-EFFECT-DATE-A2
002443     END-IF
002444     IF BL-DATA-SRCE = '4'
002445        MOVE BL-BATCH-NO         TO LA-ENTRY-A6
002446     END-IF
002447
002448     MOVE BL-PROC-ID             TO LA-PROCESSOR-CD
002449                                    LA-LAST-UPDATED-BY
002450
002451     MOVE LOW-VALUES             TO LA-LAST-RESENT-PRINT-DATE
002452                                    LA-INITIAL-PRINT-DATE
002453                                    LA-SENT-DATE
002454                                    LA-REPLY-DATE
002455                                    LA-RESEND-DATE
002456                                    LA-FOLLOW-UP-DATE
002457
002458     MOVE 'A'                    TO LA-STATUS
002459     MOVE W-NUMBER-OF-COPIES     TO LA-NO-OF-COPIES
002460     MOVE W-FORM-TO-RESEND       TO LA-RESEND-LETR
002461     IF W-DAYS-TO-RESEND NOT NUMERIC
002462        MOVE ZEROS               TO W-DAYS-TO-RESEND
002463     END-IF
002464     IF W-DAYS-TO-RESEND > ZEROS
002465        MOVE SAVE-BIN-NEXT-BUS-DT TO DC-BIN-DATE-1
002466        MOVE W-DAYS-TO-RESEND  TO DC-ELAPSED-DAYS
002467        MOVE ZEROS               TO DC-ELAPSED-MONTHS
002468        MOVE '6'                 TO DC-OPTION-CODE
002469        PERFORM 9700-DATE-LINK THRU 9700-EXIT
002470        IF NO-CONVERSION-ERROR
002471           MOVE DC-BIN-DATE-2    TO LA-RESEND-DATE
002472        END-IF
002473     END-IF
002474     IF W-DAYS-TO-FOLLOW-UP >= ZEROS
002475        MOVE SAVE-BIN-DATE       TO DC-BIN-DATE-1
002476        MOVE W-DAYS-TO-FOLLOW-UP TO DC-ELAPSED-DAYS
002477        MOVE ZEROS               TO DC-ELAPSED-MONTHS
002478        MOVE '6'                 TO DC-OPTION-CODE
002479        PERFORM 9700-DATE-LINK THRU 9700-EXIT
002480        IF NO-CONVERSION-ERROR
002481           MOVE DC-BIN-DATE-2    TO LA-FOLLOW-UP-DATE
002482        END-IF
002483     END-IF
002484
002485     MOVE W-AUTO-CLOSE-IND       TO LA-FINAL-ACT-IND
002486     MOVE BL-LETTER-ID           TO LA-FORM-A3
002487     MOVE BL-DATA-SRCE           TO LA-DATA-SOURCE
002488     MOVE BL-WRITE-ERARCH        TO LA-ARCHIVE-STATUS
002489     IF BL-ENDT-ARCH-NO > ZEROS
002490        MOVE BL-ENDT-ARCH-NO     TO LA-ENDT-ARCH-NO
002491     ELSE
002492        MOVE BL-ARCHIVE-NO       TO LA-ENDT-ARCH-NO
002493     END-IF
002494     MOVE SAVE-BIN-DATE          TO LA-CREATION-DATE
002495                                    LA-LAST-MAINT-DATE
002496
002497     MOVE ZEROS                  TO LA-NUMBER-LABEL-LINES
002498                                    LA-NO-OF-TEXT-RECORDS
002499     MOVE EIBTIME                TO LA-LAST-MAINT-TIME
002500
002501     
      * EXEC CICS WRITE
002502*         DATASET   ('ERARCH')
002503*         FROM      (LETTER-ARCHIVE)
002504*         RIDFLD    (LA-CONTROL-PRIMARY)
002505*         RESP      (WS-RESPONSE)
002506*    END-EXEC
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
           MOVE 'ERARCH' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00007860' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303037383630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002507
002508     IF NOT RESP-NORMAL
002509        DISPLAY ' BAD WRITE ON ERARCH ' WS-RESPONSE ' '
002510        la-archive-no
002511        SET BL-FAIL TO TRUE
002512     END-IF
002513
002514     .
002515 1000-EXIT.
002516     EXIT.
002517
002518 1025-GET-CSR-INFO.
002519
002520     MOVE WS-CSR-ID              TO OUT-CSR-CODE
002521                                    OUT-CSR-TITLE
002522                                    OUT-PROC-TITLE
002523                                    OUT-UNDW-ID
002524     MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
002525     MOVE '2'                    TO WS-ELCNTL-REC-TYPE
002526     MOVE WS-CSR-ID              TO WS-ELCNTL-GENL
002527     MOVE +0                     TO WS-ELCNTL-SEQ-NO
002528     
      * EXEC CICS READ
002529*       INTO    (CONTROL-FILE)
002530*       DATASET ('ELCNTL')
002531*       RIDFLD  (WS-ELCNTL-KEY)
002532*       RESP    (WS-RESPONSE)
002533*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00007887' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037383837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002534     IF RESP-NORMAL
002535        MOVE CF-PROCESSOR-NAME   TO OUT-CSR-NAME
002536                                    OUT-PROC-NAME
002537*       MOVE CF-PROCESSOR-TITLE  TO OUT-CSR-TITLE
002538*                                   OUT-PROC-TITLE
002539     END-IF
002540
002541     IF BL-PROC-ID NOT = PB-CSR-ID
002542        MOVE BL-COMP-ID          TO WS-ELCNTL-KEY
002543        MOVE '2'                 TO WS-ELCNTL-REC-TYPE
002544        MOVE BL-PROC-ID          TO WS-ELCNTL-GENL
002545                                    OUT-PROC-TITLE
002546        MOVE +0                  TO WS-ELCNTL-SEQ-NO
002547        
      * EXEC CICS READ
002548*          INTO    (CONTROL-FILE)
002549*          DATASET ('ELCNTL')
002550*          RIDFLD  (WS-ELCNTL-KEY)
002551*          RESP    (WS-RESPONSE)
002552*       END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00007906' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037393036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002553        IF RESP-NORMAL
002554           MOVE CF-PROCESSOR-NAME   TO OUT-PROC-NAME
002555*          MOVE CF-PROCESSOR-TITLE  TO OUT-PROC-TITLE
002556        END-IF
002557     END-IF
002558
002559     .
002560 1025-EXIT.
002561     EXIT.
002562
002563 1050-GET-ERMAIL.
002564
002565     MOVE CM-CONTROL-PRIMARY     TO WS-ERMAIL-KEY
002566
002567     
      * EXEC CICS READ
002568*         DATASET    ('ERMAIL')
002569*         INTO       (MAILING-DATA)
002570*         RIDFLD     (WS-ERMAIL-KEY)
002571*         RESP       (WS-RESPONSE)
002572*    END-EXEC
           MOVE LENGTH OF
            MAILING-DATA
             TO DFHEIV11
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00007926' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037393236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 MAILING-DATA, 
                 DFHEIV11, 
                 WS-ERMAIL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002573
002574     IF RESP-NORMAL
002575        MOVE MA-ADDRESS-LINE-1   TO OUT-INS-ADDR1
002576        MOVE MA-ADDRESS-LINE-2   TO OUT-INS-ADDR2
002577        MOVE MA-CITY             TO OUT-INS-CITY
002578        MOVE MA-ADDR-STATE       TO OUT-INS-STATE
002579        MOVE MA-ZIP              TO OUT-INS-ZIP
002580        MOVE MA-CRED-BENE-NAME   TO OUT-BENE-NAME
002581        MOVE MA-CRED-BENE-ADDR   TO OUT-BENE-ADDR1
002582        MOVE MA-CRED-BENE-ADDR2  TO OUT-BENE-ADDR2
002583        MOVE MA-CRED-BENE-CITY   TO OUT-BENE-CITY
002584        MOVE MA-CRED-BENE-STATE  TO OUT-BENE-STATE
002585        MOVE MA-CRED-BENE-ZIP    TO OUT-BENE-ZIP
002586        IF MA-INSURED-BIRTH-DT NOT = LOW-VALUES AND SPACES
002587           MOVE MA-INSURED-BIRTH-DT
002588                                 TO DC-BIN-DATE-1
002589           MOVE ' '              TO DC-OPTION-CODE
002590           PERFORM 9700-DATE-LINK THRU 9700-EXIT
002591           IF NO-CONVERSION-ERROR
002592              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
002593                DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
002594                INTO OUT-INS-BIRTHDATE
002595              END-STRING
002596           END-IF
002597        END-IF
002598        IF MA-JOINT-BIRTH-DT NOT = LOW-VALUES AND SPACES
002599           MOVE MA-JOINT-BIRTH-DT
002600                                 TO DC-BIN-DATE-1
002601           MOVE ' '              TO DC-OPTION-CODE
002602           PERFORM 9700-DATE-LINK THRU 9700-EXIT
002603           IF NO-CONVERSION-ERROR
002604              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
002605                DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
002606                INTO OUT-JNT-BIRTHDATE
002607              END-STRING
002608           END-IF
002609        END-IF
002610     END-IF
002611
002612     .
002613 1050-EXIT.
002614     EXIT.
002615
002616 1100-GET-ELCERT.
002617
002618     
      * EXEC CICS READ
002619*         DATASET    ('ELCERT')
002620*         INTO       (CERTIFICATE-MASTER)
002621*         RIDFLD     (WS-ELCERT-KEY)
002622*         RESP       (WS-RESPONSE)
002623*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00007977' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037393737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 WS-ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002624
002625     IF RESP-NORMAL
002626        SET CERT-FOUND TO TRUE
002627        PERFORM 1110-BUILD-CERT  THRU 1110-EXIT
002628        PERFORM 1050-GET-ERMAIL  THRU 1050-EXIT
002629     END-IF
002630
002631     .
002632 1100-EXIT.
002633     EXIT.
002634
002635 1110-BUILD-CERT.
002636
002637     PERFORM 1300-DETERMINE-COMM THRU 1300-EXIT
002638
002639     MOVE SPACES                 TO OUT-COVERAGE-IND
002640     MOVE SPACES                 TO OUT-ORIG-COVERAGE-IND
002641     MOVE SPACES                 TO OUT-NEW-COVERAGE-IND
002642     MOVE CM-CARRIER             TO OUT-CARRIER
002643     MOVE CM-GROUPING            TO OUT-GROUPING
002644     MOVE CM-STATE               TO OUT-STATE
002645     MOVE CM-ACCOUNT             TO OUT-ACCOUNT
002646     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
002647     MOVE ' '                    TO DC-OPTION-CODE
002648     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
002649     IF NO-CONVERSION-ERROR
002650        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
002651           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
002652              INTO OUT-CERT-EFF-DT
002653        END-STRING
002654     END-IF
002655
002656     MOVE CM-CERT-NO             TO OUT-CERT-NO
002657     MOVE CM-INSURED-LAST-NAME   TO OUT-ILNAME
002658                                    OUT-ORIG-ILNAME
002659     MOVE CM-INSURED-INITIAL1    TO OUT-IFINIT
002660     MOVE CM-INSURED-INITIAL2    TO OUT-IMINIT
002661                                    OUT-ORIG-MINIT
002662     IF (CM-SSN-STATE = CM-STATE)
002663        AND (CM-SSN-ACCOUNT = CM-ACCOUNT (5:6))
002664        MOVE SPACES              TO OUT-SOC-SEC-NO
002665     ELSE
002666        MOVE CM-SOC-SEC-NO       TO OUT-SOC-SEC-NO
002667     END-IF
002668
002669     IF (CM-MEMB-STATE = CM-STATE)
002670        AND (CM-MEMB-ACCOUNT = CM-ACCOUNT (5:6))
002671        MOVE SPACES              TO OUT-MEMBER-NO
002672     ELSE
002673        MOVE CM-MEMBER-NO        TO OUT-MEMBER-NO
002674     END-IF
002675
002676     MOVE CM-INSURED-FIRST-NAME  TO OUT-IFNAME
002677                                    OUT-ORIG-IFNAME
002678     MOVE CM-INSURED-ISSUE-AGE   TO OUT-IAGE
002679                                    OUT-ORIG-IAGE
002680     MOVE CM-INSURED-SEX         TO OUT-ISEX
002681     MOVE CM-INSURED-JOINT-AGE   TO OUT-JAGE
002682                                    OUT-ORIG-JAGE
002683     MOVE CM-JT-LAST-NAME        TO OUT-JLNAME
002684                                    OUT-ORIG-JLNAME
002685     MOVE CM-JT-FIRST-NAME       TO OUT-JFNAME
002686                                    OUT-ORIG-JFNAME
002687     MOVE CM-JT-INITIAL          TO OUT-JMINIT
002688                                    OUT-ORIG-JMINIT
002689     MOVE CM-LF-BENEFIT-CD       TO OUT-LF-BENCD
002690                                    OUT-ORIG-LF-BENCD
002691     MOVE CM-LF-ORIG-TERM        TO OUT-LF-TERM
002692                                    OUT-ORIG-LF-TERM
002693     MOVE CM-LF-DEV-CODE         TO OUT-LF-DEV-CD
002694     IF CM-LF-DEV-PCT NOT NUMERIC
002695        MOVE ZEROS               TO CM-LF-DEV-PCT
002696     END-IF
002697     MOVE CM-LF-DEV-PCT          TO OUT-LF-DEV-PCT
002698
002699     PERFORM 1160-READ-CERT-TRAILER THRU 1160-EXIT
002700     IF NOT CERT-TRL-REC-NOT-FOUND
002701         IF CS-INS-AGE-DEFAULT-FLAG = 'Y'
002702             MOVE ZERO           TO OUT-IAGE
002703                                    OUT-ORIG-IAGE
002704         END-IF
002705         IF CS-JNT-AGE-DEFAULT-FLAG = 'Y'
002706             MOVE ZERO           TO OUT-JAGE
002707                                    OUT-ORIG-JAGE
002708         END-IF
002709         MOVE CS-VIN-NUMBER      TO OUT-VIN-NUMBER
002710     END-IF
002711
002712     IF BL-DATA-SRCE = '4' AND PROCESS-CANCEL
002713        IF (PB-C-LF-CANCEL-DT = LOW-VALUES OR SPACES) AND
002714          ((CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES) OR
002715           (CM-LF-DEATH-DT NOT = LOW-VALUES AND SPACES))
002716             MOVE SPACES          TO OUT-LF-BENCD
002717                                     WS-LF-BENCD
002718                                     OUT-LF-CAN-DT
002719             MOVE ZEROS           TO OUT-LF-TERM
002720                                     OUT-LF-BEN
002721                                     OUT-LF-PRM
002722                                     OUT-LF-ALT-BEN
002723                                     OUT-LF-ALT-PRM
002724                                     OUT-LF-COMM-PCT
002725                                     OUT-LF-NSP
002726                                     OUT-LF-REM-BEN
002727                                     WS-WORK-LF-PREM
002728                                     WS-WORK-ALT-LF-PREM
002729                                     WS-WORK-CMB-LF-PREM
002730                                     OUT-LF-REF
002731                                     OUT-ORIG-LF-REF
002732                                     WS-WORK-LF-REF
002733             MOVE LOW-VALUES      TO OUT-LF-EXP-DT
002734             GO TO 1100-LF-CONT
002735        END-IF
002736     END-IF
002737
002738
002739     IF BL-DATA-SRCE = '2' AND BL-FUNC = 'CrtVerif'
002740        IF (CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES)
002741             MOVE SPACES          TO OUT-LF-BENCD
002742                                     WS-LF-BENCD
002743                                     OUT-LF-CAN-DT
002744             MOVE ZEROS           TO OUT-LF-TERM
002745                                     OUT-LF-BEN
002746                                     OUT-LF-PRM
002747                                     OUT-LF-ALT-BEN
002748                                     OUT-LF-ALT-PRM
002749                                     OUT-LF-COMM-PCT
002750                                     OUT-LF-NSP
002751                                     OUT-LF-REM-BEN
002752                                     WS-WORK-LF-PREM
002753                                     WS-WORK-ALT-LF-PREM
002754                                     WS-WORK-CMB-LF-PREM
002755                                     OUT-LF-REF
002756                                     OUT-ORIG-LF-REF
002757                                     WS-WORK-LF-REF
002758             MOVE LOW-VALUES      TO OUT-LF-EXP-DT
002759             GO TO 1100-LF-CONT
002760        END-IF
002761     END-IF
002762
002763     IF CM-LF-BENEFIT-CD NOT = '  ' AND '00'
002764         MOVE 'LIFE'             TO OUT-COVERAGE-IND
002765                                    OUT-ORIG-COVERAGE-IND
002766     END-IF
002767
002768     MOVE CM-LF-BENEFIT-CD       TO WS-LF-BENCD
002769     MOVE CM-LF-BENEFIT-AMT      TO OUT-LF-BEN
002770                                    OUT-ORIG-LF-BEN
002771     IF CM-LF-BENEFIT-AMT = 999999999.99
002772        MOVE SPACES              TO OUT-ORIG-LF-BEN-A
002773     END-IF
002774     MOVE CM-LF-PREMIUM-AMT      TO OUT-LF-PRM
002775                                    OUT-ORIG-LF-PRM
002776                                    WS-WORK-LF-PREM
002777     IF CM-LF-PREMIUM-AMT = .01
002778         MOVE SPACES             TO OUT-ORIG-LF-PRM-A
002779     END-IF
002780     IF CM-LF-ALT-BENEFIT-AMT NOT NUMERIC
002781        MOVE ZEROS               TO CM-LF-ALT-BENEFIT-AMT
002782     END-IF
002783     MOVE CM-LF-ALT-BENEFIT-AMT  TO OUT-LF-ALT-BEN
002784                                    OUT-ORIG-LF-ALT-BEN
002785     IF CM-LF-ALT-BENEFIT-AMT = 999999999.99
002786        MOVE SPACES              TO OUT-ORIG-LF-ALT-BEN-A
002787     END-IF
002788     IF CM-LF-ALT-PREMIUM-AMT NOT NUMERIC
002789        MOVE ZEROS               TO CM-LF-ALT-PREMIUM-AMT
002790     END-IF
002791     MOVE CM-LF-ALT-PREMIUM-AMT  TO OUT-LF-ALT-PRM
002792                                    OUT-ORIG-LF-ALT-PRM
002793                                    WS-WORK-ALT-LF-PREM
002794     IF CM-LF-ALT-PREMIUM-AMT = .01
002795         MOVE SPACES             TO OUT-ORIG-LF-ALT-PRM-A
002796     END-IF
002797     COMPUTE WS-WRK-AMOUNT = CM-LF-PREMIUM-AMT +
002798                             CM-LF-ALT-PREMIUM-AMT
002799     MOVE WS-WRK-AMOUNT          TO OUT-LF-CMB-PREM
002800                                 OUT-ORIG-LF-CMB-PREM
002801                                 WS-WORK-CMB-LF-PREM
002802     IF CM-LF-NSP-PREMIUM-AMT NOT NUMERIC
002803        MOVE ZEROS               TO CM-LF-NSP-PREMIUM-AMT
002804     END-IF
002805     MOVE CM-LF-NSP-PREMIUM-AMT  TO OUT-LF-NSP
002806     MOVE CM-LF-REMAINING-AMT    TO OUT-LF-REM-BEN
002807     MOVE CM-LF-ITD-CANCEL-AMT   TO OUT-LF-REF
002808                                    OUT-ORIG-LF-REF
002809                                    WS-WORK-LF-REF
002810     if (ws-work-lf-ref = zeros)
002811        and (process-cancel)
002812        move pb-ci-lf-cancel-amt to ws-work-lf-ref
002813        if ws-work-lf-ref = zeros
002814           move pb-c-lf-cancel-amt to ws-work-lf-ref
002815        end-if
002816     end-if
002817
002818     IF CM-LF-DEATH-CLAIM-APPLIED
002819        AND (PB-C-LF-CANCEL-DT NOT = LOW-VALUES OR SPACES)
002820            MOVE PB-C-LF-CANCEL-AMT TO OUT-LF-REF
002821                                    OUT-ORIG-LF-REF
002822                                    WS-WORK-LF-REF
002823     END-IF
002824     MOVE CM-LF-CANCEL-DT        TO DC-BIN-DATE-1
002825     MOVE ' '                    TO DC-OPTION-CODE
002826     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
002827     IF NO-CONVERSION-ERROR
002828        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
002829           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
002830              INTO OUT-LF-CAN-DT
002831        END-STRING
002832     END-IF
002833
002834     IF CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
002835         MOVE ZEROS          TO WS-WORK-LF-PREM
002836                                WS-WORK-ALT-LF-PREM
002837                                WS-WORK-CMB-LF-PREM
002838     END-IF
002839
002840      .
002841 1100-LF-CONT.
002842
002843     MOVE CM-LF-ITD-DEATH-AMT    TO OUT-LF-DTH
002844     IF CM-LF-PREMIUM-RATE NOT NUMERIC
002845        MOVE ZEROS               TO CM-LF-PREMIUM-RATE
002846     END-IF
002847     MOVE CM-LF-PREMIUM-RATE     TO OUT-LF-RATE
002848     IF CM-LF-ALT-PREMIUM-RATE NOT NUMERIC
002849        MOVE ZEROS               TO CM-LF-ALT-PREMIUM-RATE
002850     END-IF
002851     MOVE CM-LF-ALT-PREMIUM-RATE TO OUT-LF-ALT-RATE
002852
002853     IF BL-DATA-SRCE = '4' AND PROCESS-CANCEL
002854        IF (PB-C-AH-CANCEL-DT = LOW-VALUES OR SPACES) AND
002855          (CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES)
002856              MOVE SPACES              TO OUT-AH-BENCD
002857                                          WS-AH-BENCD
002858                                          OUT-AH-CAN-DT
002859              MOVE ZEROS               TO OUT-AH-TERM
002860                                          OUT-AH-BEN
002861                                          OUT-AH-PRM
002862                                          OUT-AH-NSP
002863                                          OUT-AH-COMM-PCT
002864                                          OUT-CRIT-PER
002865                                          WS-WORK-AH-PREM
002866                                          OUT-AH-REF
002867                                          OUT-ORIG-AH-REF
002868                                          WS-WORK-AH-REF
002869              MOVE SPACES              TO OUT-MAX-PMTS-A
002870                                          OUT-WAIT-PER
002871                                          OUT-RET-ELIM
002872                                          OUT-AH-DESC
002873                                          OUT-BEN-DAYS
002874              MOVE LOW-VALUES          TO OUT-AH-EXP-DT
002875             GO TO 1100-AH-CONT
002876        END-IF
002877     END-IF
002878
002879     IF BL-DATA-SRCE = '2' AND BL-FUNC = 'CrtVerif'
002880        IF (CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES)
002881              MOVE SPACES              TO OUT-AH-BENCD
002882                                          WS-AH-BENCD
002883                                          OUT-AH-CAN-DT
002884              MOVE ZEROS               TO OUT-AH-TERM
002885                                          OUT-AH-BEN
002886                                          OUT-AH-PRM
002887                                          OUT-AH-NSP
002888                                          OUT-AH-COMM-PCT
002889                                          OUT-CRIT-PER
002890                                          WS-WORK-AH-PREM
002891                                          OUT-AH-REF
002892                                          OUT-ORIG-AH-REF
002893                                          WS-WORK-AH-REF
002894              MOVE SPACES              TO OUT-MAX-PMTS-A
002895                                          OUT-WAIT-PER
002896                                          OUT-RET-ELIM
002897                                          OUT-AH-DESC
002898                                          OUT-BEN-DAYS
002899              MOVE LOW-VALUES          TO OUT-AH-EXP-DT
002900             GO TO 1100-AH-CONT
002901        END-IF
002902     END-IF
002903
002904     MOVE CM-AH-BENEFIT-CD       TO WS-AH-BENCD
002905     MOVE CM-AH-BENEFIT-CD       TO OUT-AH-BENCD
002906                                    OUT-ORIG-AH-BENCD
002907
002908     IF CM-AH-BENEFIT-CD NOT = '  ' AND '00'
002909        COMPUTE OUT-AH-TOT-BEN = CM-AH-ORIG-TERM *
002910           CM-AH-BENEFIT-AMT
002911        IF OUT-COVERAGE-IND = 'LIFE'
002912            MOVE 'BOTH'          TO OUT-COVERAGE-IND
002913                                    OUT-ORIG-COVERAGE-IND
002914        ELSE
002915            MOVE 'AH  '          TO OUT-COVERAGE-IND
002916                                    OUT-ORIG-COVERAGE-IND
002917        END-IF
002918     ELSE
002919        MOVE ZEROS               TO OUT-AH-TOT-BEN
002920     END-IF
002921
002922     MOVE CM-AH-ORIG-TERM        TO OUT-AH-TERM
002923                                    OUT-ORIG-AH-TERM
002924     MOVE CM-AH-CRITICAL-PERIOD  TO OUT-CRIT-PER
002925                                    OUT-ORIG-CRIT-PER
002926                                    OUT-MAX-PMTS
002927                                    OUT-ORIG-MAX-PMTS
002928     IF CM-AH-BENEFIT-CD EQUAL '00' OR SPACES
002929         MOVE SPACES             TO OUT-MAX-PMTS-A
002930     END-IF
002931     MOVE CM-AH-DEV-CODE         TO OUT-AH-DEV-CD
002932     IF CM-AH-DEV-PCT NOT NUMERIC
002933        MOVE ZEROS               TO CM-AH-DEV-PCT
002934     END-IF
002935     MOVE CM-AH-DEV-PCT          TO OUT-AH-DEV-PCT
002936     MOVE CM-AH-BENEFIT-AMT      TO OUT-AH-BEN
002937                                    OUT-ORIG-AH-BEN
002938     IF CM-AH-BENEFIT-AMT = 9999999.99
002939        MOVE SPACES              TO OUT-ORIG-AH-BEN-A
002940     END-IF
002941     MOVE CM-AH-PREMIUM-AMT      TO OUT-AH-PRM
002942                                    OUT-ORIG-AH-PRM
002943                                    WS-WORK-AH-PREM
002944     IF CM-AH-PREMIUM-AMT = .01
002945         MOVE SPACES             TO OUT-ORIG-AH-PRM-A
002946     END-IF
002947     MOVE CM-AH-NSP-PREMIUM-AMT  TO OUT-AH-NSP
002948     MOVE CM-AH-ITD-CANCEL-AMT   TO OUT-AH-REF
002949                                    OUT-ORIG-AH-REF
002950                                    WS-WORK-AH-REF
002951
002952     if (ws-work-ah-ref = zeros)
002953        and (process-cancel)
002954        move pb-ci-ah-cancel-amt to ws-work-ah-ref
002955        if ws-work-ah-ref = zeros
002956           move pb-c-ah-cancel-amt to ws-work-ah-ref
002957        end-if
002958     end-if
002959
002960     IF CM-AH-DEATH-CLAIM-APPLIED
002961        AND (PB-C-AH-CANCEL-DT NOT = LOW-VALUES OR SPACES)
002962            MOVE PB-C-AH-CANCEL-AMT TO OUT-AH-REF
002963                                    OUT-ORIG-AH-REF
002964                                    WS-WORK-AH-REF
002965     END-IF
002966     MOVE CM-AH-CANCEL-DT        TO DC-BIN-DATE-1
002967     MOVE ' '                    TO DC-OPTION-CODE
002968     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
002969     IF NO-CONVERSION-ERROR
002970        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
002971           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
002972              INTO OUT-AH-CAN-DT
002973        END-STRING
002974     END-IF
002975
002976     IF CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
002977         MOVE ZEROS          TO WS-WORK-AH-PREM
002978     END-IF
002979
002980      .
002981 1100-AH-CONT.
002982
002983     MOVE CM-AH-ITD-AH-PMT       TO OUT-AH-CLM
002984
002985     MOVE CM-AH-PAID-THRU-DT     TO DC-BIN-DATE-1
002986     MOVE ' '                    TO DC-OPTION-CODE
002987     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
002988     IF NO-CONVERSION-ERROR
002989        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
002990           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
002991              INTO OUT-AH-PDTHRU-DT
002992        END-STRING
002993     END-IF
002994
002995     IF CM-AH-PREMIUM-RATE NOT NUMERIC
002996        MOVE ZEROS               TO CM-AH-PREMIUM-RATE
002997     END-IF
002998     MOVE CM-AH-PREMIUM-RATE     TO OUT-AH-RATE
002999     IF CM-LOAN-APR NOT NUMERIC
003000        MOVE ZEROS               TO CM-LOAN-APR
003001     END-IF
003002     MOVE CM-LOAN-APR            TO OUT-LOAN-APR
003003     MOVE CM-LOAN-TERM           TO OUT-LOAN-TERM
003004     MOVE CM-RATE-CLASS          TO OUT-RATE-CLASS
003005     MOVE CM-PMT-EXTENSION-DAYS  TO OUT-EXT-DAYS
003006     MOVE CM-UNDERWRITING-CODE   TO OUT-UCODE
003007     MOVE CM-PREMIUM-TYPE        TO OUT-PREM-TYPE
003008     MOVE CM-IND-GRP-TYPE        TO OUT-IND-GRP
003009     MOVE CM-LOAN-OFFICER        TO OUT-LOAN-OFF
003010     MOVE CM-REIN-TABLE          TO OUT-REIN-TABLE
003011     MOVE CM-SPECIAL-REIN-CODE   TO OUT-SPEC-REIN
003012
003013     MOVE CM-LF-LOAN-EXPIRE-DT   TO DC-BIN-DATE-1
003014     MOVE ' '                    TO DC-OPTION-CODE
003015     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
003016     IF NO-CONVERSION-ERROR
003017        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
003018           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
003019              INTO OUT-LF-EXP-DT
003020        END-STRING
003021     END-IF
003022     MOVE OUT-LF-EXP-DT          TO OUT-ORIG-LF-EXP-DT
003023
003024     MOVE CM-AH-LOAN-EXPIRE-DT   TO DC-BIN-DATE-1
003025     MOVE ' '                    TO DC-OPTION-CODE
003026     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
003027     IF NO-CONVERSION-ERROR
003028        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
003029           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
003030              INTO OUT-AH-EXP-DT
003031        END-STRING
003032     END-IF
003033     MOVE OUT-AH-EXP-DT          TO OUT-ORIG-AH-EXP-DT
003034
003035     MOVE CM-LOAN-1ST-PMT-DT     TO DC-BIN-DATE-1
003036     MOVE ' '                    TO DC-OPTION-CODE
003037     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
003038     IF NO-CONVERSION-ERROR
003039        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
003040           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
003041              INTO OUT-1ST-PMT-DT
003042        END-STRING
003043     END-IF
003044
003045     MOVE CM-ENTRY-STATUS        TO OUT-ENTRY-STATUS
003046
003047     MOVE CM-ENTRY-DT            TO DC-BIN-DATE-1
003048     MOVE ' '                    TO DC-OPTION-CODE
003049     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
003050     IF NO-CONVERSION-ERROR
003051        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
003052           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
003053              INTO OUT-ENTRY-DT
003054        END-STRING
003055     END-IF
003056
003057
003058     MOVE CM-LF-CANCEL-EXIT-DT   TO DC-BIN-DATE-1
003059     MOVE ' '                    TO DC-OPTION-CODE
003060     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
003061     IF NO-CONVERSION-ERROR
003062        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
003063           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
003064              INTO OUT-LF-CAN-EXIT-DT
003065        END-STRING
003066     END-IF
003067
003068     MOVE CM-LF-DEATH-DT         TO DC-BIN-DATE-1
003069     MOVE ' '                    TO DC-OPTION-CODE
003070     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
003071     IF NO-CONVERSION-ERROR
003072        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
003073           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
003074              INTO OUT-LF-DTH-DT
003075        END-STRING
003076     END-IF
003077
003078     MOVE CM-LF-DEATH-EXIT-DT    TO DC-BIN-DATE-1
003079     MOVE ' '                    TO DC-OPTION-CODE
003080     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
003081     IF NO-CONVERSION-ERROR
003082        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
003083           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
003084              INTO OUT-LF-DTH-EXIT-DT
003085        END-STRING
003086     END-IF
003087
003088     MOVE CM-LF-CURRENT-STATUS   TO OUT-LF-CUR-STATUS
003089
003090     IF OUT-AH-CAN-DT NOT = SPACES
003091        MOVE OUT-AH-CAN-DT       TO OUT-CANCEL-DT
003092     ELSE
003093        MOVE OUT-LF-CAN-DT       TO OUT-CANCEL-DT
003094     END-IF
003095
003096     MOVE CM-AH-CANCEL-EXIT-DT   TO DC-BIN-DATE-1
003097     MOVE ' '                    TO DC-OPTION-CODE
003098     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
003099     IF NO-CONVERSION-ERROR
003100        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
003101           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
003102              INTO OUT-AH-CAN-EXIT-DT
003103        END-STRING
003104     END-IF
003105
003106     MOVE CM-AH-CURRENT-STATUS   TO OUT-AH-CUR-STATUS
003107     IF CM-ENTRY-BATCH = LOW-VALUES
003108        MOVE SPACES              TO CM-ENTRY-BATCH
003109     END-IF
003110     MOVE CM-ENTRY-BATCH         TO OUT-ENTRY-BATCH
003111     IF CM-LF-EXIT-BATCH = LOW-VALUES
003112        MOVE SPACES              TO CM-LF-EXIT-BATCH
003113     END-IF
003114     MOVE CM-LF-EXIT-BATCH       TO OUT-LF-EXIT-BATCH
003115     IF CM-AH-EXIT-BATCH = LOW-VALUES
003116        MOVE SPACES              TO CM-AH-EXIT-BATCH
003117     END-IF
003118     MOVE CM-AH-EXIT-BATCH       TO OUT-AH-EXIT-BATCH
003119     MOVE WS-CHGBK-LIFE-PCT      TO OUT-LF-COMM-PCT
003120     MOVE WS-CHGBK-AH-PCT        TO OUT-AH-COMM-PCT
003121     MOVE CM-CERT-SFX            TO OUT-CERT-SFX
003122     MOVE CM-CONTROL-PRIMARY     TO MA-CONTROL-PRIMARY
003123
003124     COMPUTE WS-WORK-PREM =
003125        (CM-LF-PREMIUM-AMT + CM-LF-ALT-PREMIUM-AMT
003126           + CM-AH-PREMIUM-AMT)
003127     MOVE WS-WORK-PREM           TO OUT-TOT-PRM
003128                                    OUT-ORIG-TOT-PREM
003129
003130     COMPUTE WS-WORK-REF =
003131        (WS-WORK-LF-REF + WS-WORK-AH-REF)
003132     MOVE WS-WORK-REF            TO OUT-TOT-REF
003133                                    OUT-ORIG-TOT-REF
003134
003135     IF CM-LF-LOAN-EXPIRE-DT > CM-AH-LOAN-EXPIRE-DT
003136        MOVE OUT-LF-EXP-DT       TO OUT-SCHED-EXP-DT
003137     ELSE
003138        MOVE OUT-AH-EXP-DT       TO OUT-SCHED-EXP-DT
003139     END-IF
003140     MOVE OUT-SCHED-EXP-DT       TO OUT-ORIG-SCHED-EXP-DT
003141
003142     IF CM-LF-ORIG-TERM > CM-AH-ORIG-TERM
003143        MOVE CM-LF-ORIG-TERM     TO OUT-SCHED-TERM
003144     ELSE
003145        MOVE CM-AH-ORIG-TERM     TO OUT-SCHED-TERM
003146     END-IF
003147     MOVE OUT-SCHED-TERM         TO OUT-ORIG-SCHED-TERM
003148
003149     IF CM-INT-ON-REFS NUMERIC
003150        IF CM-INT-ON-REFS NOT = ZEROS
003151           MOVE CM-INT-ON-REFS   TO OUT-TOT-INTEREST
003152        END-IF
003153     END-IF
003154
003155     IF (BL-DATA-SRCE = '4')
003156        AND (PROCESS-CANCEL)
003157        IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
003158            MOVE WS-WORK-LF-REF TO WS-WORK-CMB-LF-PREM
003159        END-IF
003160        IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
003161            MOVE WS-WORK-AH-REF TO WS-WORK-AH-PREM
003162        END-IF
003163     END-IF
003164*
003165*     IF BL-DATA-SRCE = '2'
003166*        IF CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
003167*            MOVE WS-WORK-LF-REF TO WS-WORK-CMB-LF-PREM
003168*        END-IF
003169*        IF CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
003170*            MOVE WS-WORK-AH-REF TO WS-WORK-AH-PREM
003171*        END-IF
003172*     END-IF
003173
003174*   display ' build cert ' bl-cert-no
003175*   display ' ws work lf ref ' ws-work-lf-ref
003176*   display ' ws-chgbk-life-pct ' ws-chgbk-life-pct
003177*   display ' ws-work-lf-prem ' ws-work-lf-prem
003178*   display ' ws-work-alt-lf-prem ' ws-work-alt-lf-prem
003179*   display ' ws-work-cmb-lf-prem ' ws-work-cmb-lf-prem
003180
003181*   display ' ws-chgbk-ah-pct ' ws-chgbk-ah-pct
003182*   display ' ws-work-ah-prem ' ws-work-ah-prem
003183
003184
003185     COMPUTE WS-ACCT-LF-PORTION =
003186        WS-WORK-LF-PREM * WS-CHGBK-LIFE-PCT
003187     MOVE WS-ACCT-LF-PORTION TO OUT-ACCT-LF-PORTION
003188                                OUT-ACCT-ORIG-LF-PORT
003189     COMPUTE WS-ACCT-ALT-LF-PORTION =
003190        WS-WORK-ALT-LF-PREM * WS-CHGBK-LIFE-PCT
003191     MOVE WS-ACCT-ALT-LF-PORTION TO OUT-ACCT-ALT-LF-PORT
003192                                OUT-ACCT-ORIG-ALT-LF-PORT
003193     COMPUTE WS-ACCT-CMB-LF-PORTION =
003194        WS-WORK-CMB-LF-PREM * WS-CHGBK-LIFE-PCT
003195     MOVE WS-ACCT-CMB-LF-PORTION TO OUT-ACCT-CMB-LF-PORT
003196                                OUT-ACCT-ORIG-CMB-LF-PORT
003197     COMPUTE WS-ACCT-AH-PORTION =
003198        WS-WORK-AH-PREM * WS-CHGBK-AH-PCT
003199     MOVE WS-ACCT-AH-PORTION TO OUT-ACCT-AH-PORTION
003200                                OUT-ACCT-ORIG-AH-PORT
003201     COMPUTE WS-ACCT-PORTION =
003202        WS-ACCT-CMB-LF-PORTION + WS-ACCT-AH-PORTION
003203     MOVE WS-ACCT-PORTION TO OUT-ACCT-PORTION
003204                                OUT-ACCT-ORIG-PORTION
003205
003206     COMPUTE WS-CSO-LF-PORTION =
003207       WS-WORK-LF-PREM - WS-ACCT-LF-PORTION
003208     MOVE WS-CSO-LF-PORTION TO OUT-CSO-LF-PORTION
003209                               OUT-CSO-ORIG-LF-PORT
003210     COMPUTE WS-CSO-ALT-LF-PORTION =
003211       WS-WORK-ALT-LF-PREM - WS-ACCT-ALT-LF-PORTION
003212     MOVE WS-CSO-ALT-LF-PORTION TO OUT-CSO-ALT-LF-PORT
003213                                   OUT-CSO-ORIG-ALT-LF-PORT
003214     COMPUTE WS-CSO-CMB-LF-PORTION =
003215       WS-WORK-CMB-LF-PREM - WS-ACCT-CMB-LF-PORTION
003216     MOVE WS-CSO-CMB-LF-PORTION TO OUT-CSO-CMB-LF-PORT
003217                                   OUT-CSO-ORIG-CMB-LF-PORT
003218     COMPUTE WS-CSO-AH-PORTION =
003219        WS-WORK-AH-PREM - WS-ACCT-AH-PORTION
003220     MOVE WS-CSO-AH-PORTION TO OUT-CSO-AH-PORTION
003221                               OUT-CSO-ORIG-AH-PORT
003222     COMPUTE WS-CSO-PORTION =
003223        WS-CSO-CMB-LF-PORTION + WS-CSO-AH-PORTION
003224     MOVE WS-CSO-PORTION TO OUT-CSO-PORTION
003225                            OUT-CSO-ORIG-PORTION
003226
003227     .
003228 1110-EXIT.
003229     EXIT.
003230
003231 1115-MATCH-COMPANY.
003232
003233     MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
003234     MOVE '1'                    TO WS-ELCNTL-REC-TYPE
003235     MOVE +0                     TO WS-ELCNTL-SEQ-NO
003236     
      * EXEC CICS READ
003237*       INTO    (CONTROL-FILE)
003238*       DATASET ('ELCNTL')
003239*       RIDFLD  (WS-ELCNTL-KEY)
003240*       RESP    (WS-RESPONSE)
003241*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00008595' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038353935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003242     IF RESP-NORMAL
003243        MOVE CF-COMPANY-CD       TO WS-COMPANY-CD
003244     END-IF
003245
003246     .
003247 1115-EXIT.
003248     EXIT.
003249
003250 1120-MATCH-CARRIER.
003251
003252     MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
003253     MOVE '6'                    TO WS-ELCNTL-REC-TYPE
003254     MOVE BL-CARRIER             TO WS-ELCNTL-CARR
003255     MOVE +0                     TO WS-ELCNTL-SEQ-NO
003256     
      * EXEC CICS READ
003257*       INTO    (CONTROL-FILE)
003258*       DATASET ('ELCNTL')
003259*       RIDFLD  (WS-ELCNTL-KEY)
003260*       RESP    (WS-RESPONSE)
003261*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00008615' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038363135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003262     IF RESP-NORMAL
003263        MOVE CF-MAIL-TO-NAME     TO OUT-CARR-NAME
003264     ELSE
003265        MOVE 'UNKNOWN'           TO OUT-CARR-NAME
003266     END-IF
003267
003268     .
003269 1120-EXIT.
003270     EXIT.
003271
003272 1125-MATCH-STATE.
003273
003274     MOVE BL-STATE               TO OUT-STATE
003275     MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
003276     MOVE '3'                    TO WS-ELCNTL-REC-TYPE
003277     MOVE BL-STATE               TO WS-ELCNTL-STATE
003278     MOVE +0                     TO WS-ELCNTL-SEQ-NO
003279     
      * EXEC CICS READ
003280*       INTO    (CONTROL-FILE)
003281*       DATASET ('ELCNTL')
003282*       RIDFLD  (WS-ELCNTL-KEY)
003283*       RESP    (WS-RESPONSE)
003284*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00008638' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038363338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003285     IF RESP-NORMAL
003286        MOVE CF-STATE-NAME       TO OUT-STATE-NAME
003287     ELSE
003288        MOVE 'UNKNOWN'           TO OUT-STATE-NAME
003289     END-IF
003290
003291     .
003292 1125-EXIT.
003293     EXIT.
003294
003295 1130-MATCH-BENCDS.
003296
003297     MOVE SPACES                 TO WS-LF-ABBRV
003298                                    WS-LF-DESC
003299                                    WS-LF-EARN
003300                                    WS-WAIT-PER
003301                                    WS-RET-ELIM
003302                                    WS-AH-DESC
003303                                    WS-BEN-DAYS
003304     IF WS-LF-BENCD NOT = ZEROS AND SPACES
003305        PERFORM 1140-MATCH-LFBEN THRU 1140-EXIT
003306     END-IF
003307
003308     IF WS-AH-BENCD NOT = ZEROS AND SPACES
003309        PERFORM 1150-MATCH-AHBEN THRU 1150-EXIT
003310     END-IF
003311
003312     .
003313 1130-EXIT.
003314     EXIT.
003315
003316 1140-MATCH-LFBEN.
003317
003318     MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
003319     MOVE '4'                    TO WS-ELCNTL-REC-TYPE
003320     MOVE WS-LF-BENCD            TO WS-ELCNTL-BEN-CD
003321     MOVE +0                     TO WS-ELCNTL-SEQ-NO
003322     
      * EXEC CICS READ
003323*       INTO    (CONTROL-FILE)
003324*       DATASET ('ELCNTL')
003325*       RIDFLD  (WS-ELCNTL-KEY)
003326*       GTEQ
003327*       RESP    (WS-RESPONSE)
003328*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00008681' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303038363831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003329     IF RESP-NORMAL
003330        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
003331           (WS-LF-BENCD = CF-BENEFIT-CODE (S1))
003332           OR (S1 > +8)
003333        END-PERFORM
003334        IF S1 < + 9
003335           MOVE CF-BENEFIT-ALPHA (S1)
003336                                 TO WS-LF-ABBRV
003337           MOVE WS-LF-BENCD      TO WS-LF-DESC
003338           MOVE CF-CO-EARNINGS-CALC (S1)
003339                                 TO WS-LF-EARN
003340        ELSE
003341           MOVE 'XX'             TO WS-LF-DESC
003342           MOVE 'XXX'            TO WS-LF-ABBRV
003343        END-IF
003344     ELSE
003345        DISPLAY ' NO LF BEN CD ' WS-LF-BENCD
003346     END-IF
003347
003348     .
003349 1140-EXIT.
003350     EXIT.
003351
003352 1150-MATCH-AHBEN.
003353
003354     MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
003355     MOVE '5'                    TO WS-ELCNTL-REC-TYPE
003356     MOVE WS-AH-BENCD            TO WS-ELCNTL-BEN-CD
003357     MOVE +0                     TO WS-ELCNTL-SEQ-NO
003358     
      * EXEC CICS READ
003359*       INTO    (CONTROL-FILE)
003360*       DATASET ('ELCNTL')
003361*       RIDFLD  (WS-ELCNTL-KEY)
003362*       GTEQ
003363*       RESP    (WS-RESPONSE)
003364*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00008717' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303038373137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003365     IF RESP-NORMAL
003366        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
003367           (WS-AH-BENCD = CF-BENEFIT-CODE (S1))
003368           OR (S1 > +8)
003369        END-PERFORM
003370        IF S1 < + 9
003371           MOVE CF-BENEFIT-ALPHA (S1)
003372                                 TO WS-WAIT-PER
003373           MOVE CF-BENEFIT-ALPHA (S1) (3:1)
003374                                 TO WS-RET-ELIM
003375           MOVE CF-BENEFIT-ALPHA (S1) (1:2)
003376                                 TO WS-BEN-DAYS
003377           IF CF-SPECIAL-CALC-CD (S1) = 'C'
003378              IF CF-JOINT-INDICATOR (S1) = 'J'
003379                 MOVE 'Joint Critical Period Disability'
003380                                 TO WS-AH-DESC
003381              ELSE
003382                 MOVE 'Single Critical Period-Primary Only'
003383                                 TO WS-AH-DESC
003384              END-IF
003385           ELSE
003386              IF CF-JOINT-INDICATOR (S1) = 'J'
003387                 MOVE 'Joint Disability'
003388                                 TO WS-AH-DESC
003389              ELSE
003390                 MOVE 'Single Disability (Primary Only)'
003391                                 TO WS-AH-DESC
003392              END-IF
003393           END-IF
003394        ELSE
003395           MOVE 'NOT FOUND'      TO WS-AH-DESC
003396        END-IF
003397     ELSE
003398        DISPLAY ' NO AH BEN CD ' WS-AH-BENCD
003399     END-IF
003400
003401     .
003402 1150-EXIT.
003403     EXIT.
003404
003405 1160-READ-CERT-TRAILER.
003406
003407     MOVE +0                     TO  WS-CERT-TRL-REC-NOT-FOUND.
003408     MOVE WS-ELCERT-KEY          TO  WS-ELCRTT-PRIMARY.
003409     MOVE 'C'                    TO  WS-ELCRTT-REC-TYPE.
003410
003411     
      * EXEC CICS HANDLE CONDITION
003412*        NOTFND (1160-CERT-TRL-REC-NOTFND)
003413*    END-EXEC.
      *    MOVE '"$I                   ! " #00008770' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303038373730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003414
003415     
      * EXEC CICS READ
003416*        DATASET  ('ELCRTT')
003417*        RIDFLD   (WS-ELCRTT-KEY)
003418*        INTO     (CERTIFICATE-TRAILERS)
003419*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       E          (   #00008774' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038373734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 WS-ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003420
003421     GO TO 1160-EXIT.
003422
003423 1160-CERT-TRL-REC-NOTFND.
003424     MOVE +1                     TO WS-CERT-TRL-REC-NOT-FOUND.
003425
003426 1160-EXIT.
003427     EXIT.
003428
003429 1200-GET-ERACCT.
003430
003431     MOVE WS-ERACCT-KEY          TO WS-SAVE-ERACCT-KEY
003432     MOVE ' UNKNOWN '            TO OUT-ACCT-NAME
003433                                    OUT-ACCT-ADDR1
003434                                    OUT-ACCT-ADDR2
003435                                    OUT-ACCT-CITY
003436     MOVE ZEROS                  TO WS-CHGBACK
003437
003438     
      * EXEC CICS STARTBR
003439*        DATASET   ('ERACCT')
003440*        RIDFLD    (WS-ERACCT-KEY)
003441*        GTEQ
003442*        RESP      (WS-RESPONSE)
003443*    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00008797' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303038373937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003444
003445     IF RESP-NORMAL
003446        
      * EXEC CICS READNEXT
003447*          INTO    (ACCOUNT-MASTER)
003448*          DATASET ('ERACCT')
003449*          RIDFLD  (WS-ERACCT-KEY)
003450*          RESP    (WS-RESPONSE)
003451*       END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00008805' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303038383035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 WS-ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003452     END-IF
003453
003454     IF RESP-NORMAL
003455        PERFORM UNTIL
003456           NOT (RESP-NORMAL)
003457           OR (WS-SAVE-ERACCT-KEY NOT =
003458                        AM-CONTROL-PRIMARY (1:20))
003459           IF BL-DATA-SRCE = '4' OR '2'
003460              IF (WS-CERT-EFF-DT < AM-EXPIRATION-DT
003461                 AND >= AM-EFFECTIVE-DT)
003462                 PERFORM VARYING S1 FROM +1 BY +1 UNTIL
003463                    (AM-COM-TYP (S1) = 'C' OR 'D')
003464                    OR (S1 > +10)
003465                 END-PERFORM
003466                 IF S1 < +11
003467                    MOVE AM-COMM-CHARGEBACK (S1)
003468                                 TO WS-CHGBACK
003469                 END-IF
003470              END-IF
003471           END-IF
003472           MOVE ACCOUNT-MASTER   TO WS-SAVE-ERACCT-REC
003473           
      * EXEC CICS READNEXT
003474*             INTO    (ACCOUNT-MASTER)
003475*             DATASET ('ERACCT')
003476*             RIDFLD  (WS-ERACCT-KEY)
003477*             RESP    (WS-RESPONSE)
003478*          END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00008832' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303038383332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 WS-ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003479        END-PERFORM
003480        MOVE WS-SAVE-ERACCT-REC  TO ACCOUNT-MASTER
003481        IF WS-SAVE-ERACCT-KEY = AM-CONTROL-PRIMARY (1:20)
003482           SET ACCT-FOUND TO TRUE
003483           MOVE AM-NAME          TO OUT-ACCT-NAME
003484           MOVE AM-ADDRS         TO OUT-ACCT-ADDR1
003485           MOVE SPACES           TO OUT-ACCT-ADDR2
003486           MOVE AM-ADDR-CITY     TO OUT-ACCT-CITY
003487           MOVE AM-ADDR-STATE    TO OUT-ACCT-STATE
003488           MOVE AM-ZIP-PRIME     TO OUT-ACCT-ZIP
003489           MOVE AM-CONTROL-NAME  TO OUT-ACCT-CNTRL-NAME
003490           MOVE AM-REPORT-CODE-1 TO OUT-RPT-CD1
003491           MOVE AM-REPORT-CODE-2 TO OUT-RPT-CD2
003492           IF AM-ZIP-PLUS4 NOT = SPACES AND ZEROS
003493              STRING '-' AM-ZIP-PLUS4 DELIMITED BY SIZE
003494                 INTO OUT-ACCT-ZIP (6:5)
003495              END-STRING
003496           END-IF
003497           MOVE AM-TEL-NO        TO OUT-ACCT-PHONE
003498           MOVE AM-GPCD          TO OUT-ACCT-BUS-TYPE
003499           MOVE AM-STATE         TO OUT-FORM
003500           IF AM-ACCOUNT (10:1) = 'L'
003501              MOVE 'L'           TO OUT-FORM (3:1)
003502                                    OUT-LEASE-IND
003503           END-IF
003504           MOVE AM-CONTROL-PRIMARY (1:8)
003505                                 TO WS-ERCOMP-KEY
003506           IF AM-REMIT-TO NOT NUMERIC
003507              MOVE 01            TO AM-REMIT-TO
003508           END-IF
003509           MOVE AM-AGT (AM-REMIT-TO) TO WS-ERCOMP-RESP-NO
003510           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
003511              (AM-COM-TYP (S1) = 'C' OR 'D')
003512              OR (S1 > +10)
003513           END-PERFORM
003514           IF S1 < +11
003515              MOVE AM-AGT (S1)   TO WS-ERCOMP-ACCOUNT
003516           END-IF
003517           MOVE 'A'              TO WS-ERCOMP-REC-TYPE
003518           PERFORM 1225-GET-ERCOMP THRU 1225-EXIT
003519        END-IF
003520     END-IF
003521
003522     
      * EXEC CICS ENDBR
003523*        DATASET   ('ERACCT')
003524*    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008881' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038383831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003525
003526    .
003527 1200-EXIT.
003528     EXIT.
003529
003530 1225-GET-ERCOMP.
003531
003532     
      * EXEC CICS READ
003533*       INTO    (COMPENSATION-MASTER)
003534*       DATASET ('ERCOMP')
003535*       RIDFLD  (WS-ERCOMP-KEY)
003536*       RESP    (WS-RESPONSE)
003537*    END-EXEC
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00008891' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038383931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 WS-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003538
003539     IF RESP-NORMAL
003540        MOVE CO-RESP-NO          TO OUT-RESP-NO
003541        MOVE CO-ACCT-NAME        TO OUT-COMP-NAME
003542        MOVE CO-MAIL-NAME        TO OUT-COMP-MAIL-TO
003543        MOVE CO-ADDR-1           TO OUT-COMP-ADDR1
003544        MOVE CO-ADDR-2           TO OUT-COMP-ADDR2
003545        MOVE CO-ADDR-CITY        TO OUT-COMP-CITY
003546        MOVE CO-ADDR-STATE       TO OUT-COMP-STATE
003547        MOVE CO-ZIP              TO OUT-COMP-ZIP
003548        MOVE CO-TELEPHONE        TO OUT-COMP-PHONE
003549        MOVE CO-FAXNO            TO OUT-COMP-FAX
003550        MOVE CO-GA-STATUS-CODE   TO OUT-COMP-STATUS
003551        MOVE CO-BILL-SW          TO OUT-BILL-SW
003552     END-IF
003553
003554     .
003555 1225-EXIT.
003556     EXIT.
003557
003558 1250-GET-ELLETR.
003559
003560     move spaces                 to W-Z-CONTROL-DATA
003561     MOVE WS-COMPANY-CD          TO WS-ELLETR-COMPANY-CD
003562     MOVE BL-LETTER-ID           TO WS-ELLETR-LETTER-ID
003563     MOVE +0                     TO WS-ELLETR-SEQ-NO
003564
003565     
      * EXEC CICS READ
003566*         DATASET    ('ELLETR')
003567*         INTO       (TEXT-FILES)
003568*         RIDFLD     (WS-ELLETR-KEY)
003569*         RESP       (WS-RESPONSE)
003570*         GTEQ
003571*    END-EXEC
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV11
           MOVE 'ELLETR' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00008924' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303038393234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 TEXT-FILES, 
                 DFHEIV11, 
                 WS-ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003572
003573     IF RESP-NORMAL
003574        IF (LETTER-FILE-TEXT)
003575           AND (BL-LETTER-ID = TX-LETTER-NO)
003576           AND (TX-LINE-SQUEEZE-CONTROL = 'Z')
003577           display ' found good z record '
003578           PERFORM 1280-PROCESS-Z-CONTROLS
003579                                 THRU 1280-EXIT
003580        END-IF
003581     END-IF
003582
003583     .
003584 1250-EXIT.
003585     EXIT.
003586
003587
003588 1280-PROCESS-Z-CONTROLS.
003589
003590     MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA
003591
003592*    IF W-FORM-TO-RESEND > SPACES
003593*       MOVE W-FORM-TO-RESEND    TO OUT-FORM
003594*    ELSE
003595*       MOVE SPACES              TO OUT-FORM
003596*    END-IF
003597
003598     IF BL-ENC-CD = SPACES OR LOW-VALUES
003599        MOVE W-ENCLOSURE-CD         TO BL-ENC-CD
003600     END-IF
003601     MOVE W-LETTER-TYPE          TO OUT-LETTER-TYPE
003602     MOVE W-PRINT-CERTIFICATE    TO OUT-PRINT-CERTIFICATE
003603     .
003604 1280-EXIT.
003605     EXIT.
003606
003607 1300-DETERMINE-COMM.
003608
003609******************************************************************
003610* DATA SOURCE MEANINGS  BL-DATA-SRCE
003611*    1) FROM ACCT MAINT
003612*    2) FROM CERT UPDATE
003613*    3) FROM COMP MAINT
003614*    4) FROM REVIEW AND CORRECTIONS
003615******************************************************************
003616
003617     MOVE 'Y'                    TO OUT-CHGBACK
003618     MOVE ZEROS                  TO WS-DIFF
003619                                    WS-CHGBK-LIFE-PCT
003620                                    WS-CHGBK-AH-PCT
003621
003622     IF BL-DATA-SRCE = '4'
003623        IF PB-CANCELLATION
003624           MOVE PB-CI-LIFE-COMMISSION TO WS-CHGBK-LIFE-PCT
003625           MOVE PB-CI-AH-COMMISSION   TO WS-CHGBK-AH-PCT
003626           MOVE PB-CERT-EFF-DT   TO DC-BIN-DATE-1
003627           MOVE PB-C-AH-CANCEL-DT TO DC-BIN-DATE-2
003628           IF PB-C-LF-CANCEL-DT > DC-BIN-DATE-2
003629              MOVE PB-C-LF-CANCEL-DT
003630                                 TO DC-BIN-DATE-2
003631           END-IF
003632           MOVE '1'              TO DC-OPTION-CODE
003633           PERFORM 9700-DATE-LINK THRU 9700-EXIT
003634           IF NO-CONVERSION-ERROR
003635              MOVE DC-ELAPSED-MONTHS TO WS-DIFF
003636              IF DC-ODD-DAYS-OVER > +1
003637                 ADD +1          TO WS-DIFF
003638              END-IF
003639              IF (WS-CHGBACK = 99)
003640                 OR (WS-DIFF > WS-CHGBACK AND WS-CHGBACK > 0)
003641                   MOVE +0   TO WS-CHGBK-LIFE-PCT
003642                                WS-CHGBK-AH-PCT
003643                   MOVE 'N'  TO OUT-CHGBACK
003644              END-IF
003645           END-IF
003646        ELSE
003647           MOVE PB-I-LIFE-COMMISSION TO WS-CHGBK-LIFE-PCT
003648           MOVE PB-I-AH-COMMISSION   TO WS-CHGBK-AH-PCT
003649        END-IF
003650     ELSE
003651        MOVE CM-LIFE-COMM-PCT TO WS-CHGBK-LIFE-PCT
003652        MOVE CM-AH-COMM-PCT   TO WS-CHGBK-AH-PCT
003653        IF (CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES)
003654           OR (CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES)
003655           MOVE CM-CERT-EFF-DT   TO DC-BIN-DATE-1
003656           MOVE CM-AH-CANCEL-DT  TO DC-BIN-DATE-2
003657           IF CM-LF-CANCEL-DT > DC-BIN-DATE-2
003658              MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-2
003659           END-IF
003660           MOVE '1'              TO DC-OPTION-CODE
003661           PERFORM 9700-DATE-LINK THRU 9700-EXIT
003662           IF NO-CONVERSION-ERROR
003663              MOVE DC-ELAPSED-MONTHS TO WS-DIFF
003664              IF DC-ODD-DAYS-OVER > +1
003665                 ADD +1          TO WS-DIFF
003666              END-IF
003667              IF (WS-CHGBACK = 99)
003668                 OR (WS-DIFF > WS-CHGBACK AND WS-CHGBACK > 0)
003669                   MOVE +0   TO WS-CHGBK-LIFE-PCT
003670                                WS-CHGBK-AH-PCT
003671                   MOVE 'N'  TO OUT-CHGBACK
003672              END-IF
003673           END-IF
003674        END-IF
003675     END-IF
003676
003677     .
003678 1300-EXIT.
003679     EXIT.
003680
003681 1500-GET-ARCH-NO.
003682
003683     MOVE BL-COMP-ID             TO WS-ELCNTL-COMPANY-ID
003684     MOVE '1'                    TO WS-ELCNTL-REC-TYPE
003685     MOVE SPACES                 TO WS-ELCNTL-GENL
003686     MOVE +0                     TO WS-ELCNTL-SEQ-NO
003687
003688     
      * EXEC CICS READ
003689*       INTO    (CONTROL-FILE)
003690*       DATASET ('ELCNTL')
003691*       RIDFLD  (WS-ELCNTL-KEY)
003692*       UPDATE
003693*       RESP    (WS-RESPONSE)
003694*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00009047' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303039303437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003695
003696     IF RESP-NORMAL
003697        AND (CF-COMPANY-ID  = BL-COMP-ID)
003698        AND (CF-RECORD-TYPE = '1')
003699        ADD +1                   TO CF-CREDIT-LAST-ARCH-NUM
003700        MOVE CF-CREDIT-LAST-ARCH-NUM
003701                                 TO WS-ARCHIVE-NO
003702        
      * EXEC CICS REWRITE
003703*          FROM    (CONTROL-FILE)
003704*          DATASET ('ELCNTL')
003705*       END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&& L                  %   #00009061' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039303631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003706     ELSE
003707        MOVE +0                  TO WS-ARCHIVE-NO
003708     END-IF
003709
003710    .
003711 1500-EXIT.
003712     EXIT.
003713
003714 1700-GET-ELENCC.
003715     MOVE WS-COMPANY-CD          TO WS-ELENCC-KEY
003716     MOVE '2'                    TO WS-ELENCC-REC-TYPE
003717     MOVE BL-ENC-CD              TO WS-ELENCC-ENC-CODE
003718     PERFORM VARYING E1 FROM +1 BY +1 UNTIL
003719        (WS-ELENCC-ENC-CODE (E1:1) = ' ')
003720        OR (E1 > +5)
003721     END-PERFORM
003722
003723     IF E1 < +5
003724        MOVE BL-STATE            TO WS-ELENCC-ENC-CODE (E1:2)
003725     END-IF
003726     
      * EXEC CICS READ
003727*        DATASET   ('ELENCC')
003728*        INTO      (ENCLOSURE-CODES)
003729*        RIDFLD    (WS-ELENCC-KEY)
003730*        RESP      (WS-RESPONSE)
003731*    END-EXEC
           MOVE LENGTH OF
            ENCLOSURE-CODES
             TO DFHEIV11
           MOVE 'ELENCC' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00009085' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039303835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ENCLOSURE-CODES, 
                 DFHEIV11, 
                 WS-ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003732     IF NOT RESP-NORMAL
003733        display ' elencc resp not normal ' ws-response
003734        MOVE BL-ENC-CD           TO WS-ELENCC-ENC-CODE
003735        
      * EXEC CICS READ
003736*           DATASET   ('ELENCC')
003737*           INTO      (ENCLOSURE-CODES)
003738*           RIDFLD    (WS-ELENCC-KEY)
003739*           RESP      (WS-RESPONSE)
003740*       END-EXEC
           MOVE LENGTH OF
            ENCLOSURE-CODES
             TO DFHEIV11
           MOVE 'ELENCC' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00009094' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039303934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ENCLOSURE-CODES, 
                 DFHEIV11, 
                 WS-ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003741     END-IF
003742     IF RESP-NORMAL
003743        MOVE NC-OUTPUT-STACK     TO OUT-STACK
003744        MOVE NC-ENCLOSURE-LINE   TO OUT-ENC-LINE
003745        MOVE NC-ATTACHMENTS      TO OUT-ATTACH
003746     END-IF
003747
003748     .
003749 1700-EXIT.
003750     EXIT.
003751 2000-GET-CANCEL-REFUND.
003752     IF NOT CONNECTED-TO-DB
003753        PERFORM 4150-CONNECT-TO-DB-CHECK THRU 4150-EXIT
003754     END-IF
003755
003756     MOVE WS-ELCERT-CARRIER           TO WS-CARRIER
003757     MOVE WS-ELCERT-GROUP             TO WS-GROUP
003758     MOVE WS-ELCERT-STATE             TO WS-STATE
003759     MOVE WS-ELCERT-ACCOUNT           TO WS-ACCOUNT
003760     MOVE OUT-CERT-EFF-DT             TO WS-EFF-DT
003761     MOVE WS-ELCERT-CERT-NO           TO WS-CERT-NO
003762     MOVE WS-ELCERT-CERT-NO(11:1)     TO WS-CERT-NO-SUF
003763
003765     EXEC SQL
              SELECT TOP 1
003766           Company,
003767           CertCarrier,
003768           CertGroup,
003769           CertState,
003770           CertAccount,
003771           CertEffDate,
003772           CertNumber,
003773           CertNumberSuf,
003774           CheckSeqNbr,
003775           CheckType,
003776           CheckAmount,
003777           PayeeName1,
003778           PayeeAddress1,
003779           PayeeCity,
003780           PayeeState,
003781           PayeeZIP,
003782           Preparer,
003783           CheckSubType,
003784           PayeeCode
003785        INTO
003786           :db-compid,
003787           :db-carrier,
003788           :db-grouping,
003789           :db-state,
003790           :db-account,
003791           :db-effdate,
003792           :db-certificate,
003793           :db-cert-sfx,
003794           :db-seq-no,
003795           :db-type,
003796           :db-amount-n,
003797           :db-payeename1,
003798           :db-payeeaddr1,
003799           :db-payeecity,
003800           :db-payeest,
003801           :db-payeezip,
003802           :db-preparer,
003803           :db-check-sub-type,
003804           :db-payeecode
003805        FROM
003806           ChkApp_Check
003807        WHERE
003808                  CertCarrier = :WS-CARRIER
003809             and  CertGroup   = :WS-GROUP
003810             and  CertState   = :WS-STATE
003811             and  CertAccount = :WS-ACCOUNT
003812*            and  CertEffDate = :WS-EFF-DT
003813             and  CertNumber  = :WS-CERT-NO
003814             AND  CertNumberSuf = :WS-CERT-NO-SUF
003815             AND CheckSubType = 1
003816        ORDER BY RELEASEDATE DESC
003817     end-exec
003818
003819     IF SQLCODE = 0
003820       OR SQLCODE = 1
003821        MOVE db-payeename1  TO OUT-REF-PAYEE
003822        MOVE db-payeeaddr1  TO OUT-REF-ADDRESS
003823        MOVE db-payeecity   TO OUT-REF-CITY
003824        MOVE db-payeest     TO OUT-REF-STATE
003825        MOVE db-payeezip    TO OUT-REF-ZIP
003826        MOVE db-amount-n    TO OUT-REF-CHK-AMOUNT
003827        MOVE db-payeecode   TO OUT-REF-PAYEE-CODE
003828     ELSE
003829        DISPLAY ' DIS SQL CODE ' SQLCODE
003830        DISPLAY "ERROR: CANNOT READ ROW ChkApp_Check "
003831        DISPLAY ' SQL RETURN CODE ' SQLCODE
003832        DISPLAY ' SQL ERR MESS    ' SQLERRMC
003833     END-IF
003834
003835     PERFORM 4300-DISCONNECT THRU 4300-EXIT
003836     MOVE SPACES TO WS-CONNECT-SW
003837
003838     .
003839
003840 2000-EXIT.
003841     EXIT.
003842
003843
003844 4100-CONNECT-TO-DB.
003845
003846
003847****  The below code is for when the db has been
003848****  converted to sql server 2016
003849     evaluate ws-kix-myenv
003850       when 'cid1p'
003851           move '//sdv-db01.cso.local:1433;'
003852                                 to p-sql-server
003853        when 'mdoff'
003854           move '//hov-tstdb01.cso.local:55330;'
003855                                 to p-sql-server
003856        when other
003857           move '//hov-tstdb01.cso.local:1433;'
003858                                 to p-sql-server
003859     end-evaluate
003860
003861
003862     move 'Logic'                to p-sql-database
003863
003864     CALL 'SQLCONNECT' USING sqlconnect-parms
003865     display ' ret code ' p-connect-return-code
003866     move p-connect-return-code  to sqlcode
003867     move p-sql-return-message   to sqlerrmc
003868
003869     IF SQLCODE NOT = 0
003870        DISPLAY "ERROR: CANNOT CONNECT "
003871        DISPLAY SQLCODE
003872        DISPLAY SQLERRMC
003873        GO TO 4100-EXIT
003874     END-IF
003875
003876     .
003877 4100-EXIT.
003878     EXIT.
003879
003880 4200-GET-NEXT-BUS-DT.
003881
003882     MOVE WS-SAVE-EDIT-A-DATE    TO WS-CYCLE-DATE
003883     MOVE SPACES                 TO WS-NEXT-BUS-DT
003884
003885     IF WS-KIXHOST = 'logictest'
003887        EXEC SQL
                CALL NaperTestCalcNextBusDt
003888            (
003889              @cycledate = :WS-CYCLE-DATE,
003890              @nextbusdate = :WS-NEXT-BUS-DT
003891            )
003892        END-EXEC
003893     ELSE
003895        EXEC SQL
                CALL NaperProdCalcNextBusDt
003896            (
003897              @cycledate = :WS-CYCLE-DATE,
003898              @nextbusdate = :WS-NEXT-BUS-DT
003899            )
003900        END-EXEC
003901     END-IF
003902
003903     IF SQLCODE NOT = 0
003904        DISPLAY "ERROR: DID NOT RETURN NEXT BUS DT "
003905        DISPLAY ' SQL RETURN CODE ' SQLCODE
003906        DISPLAY ' SQL ERR MESS    ' SQLERRMC
003907        GO TO 4200-EXIT
003908     END-IF
003909
003910     .
003911 4200-EXIT.
003912     EXIT.
003913
003914
003915
003916 4300-DISCONNECT.
003917
003919     EXEC SQL
              DISCONNECT
003920     END-EXEC
003921     .
003922 4300-EXIT.
003923     EXIT.
003924
003925
003926 4500-CHECK-BALLOON-STATE.
003927
003928     move spaces                 to connect-string
003929
003930     IF WS-KIXHOST = 'logictest'
003931        MOVE '1020'              TO WS-LOOKUPID
003932        move '//hov-tstdb01.cso.local:1433;'
003933                                 to p-sql-server
003934     else
003935        MOVE '1020'              TO WS-LOOKUPID
003936        move '//sdv-db01.cso.local:1433;'
003937                                 to p-sql-server
003938     end-if
003939
003940     string
003941        'jdbc:sqlserver:' delimited size
003942        p-sql-server delimited space
003943        'databaseName=' delimited size
003944        into connect-string
003945     end-string
003946     display ' connection string = **' connect-string '**'
003947
003948     SET ENVIRONMENT "jdbc.url" to connect-string
003949
003950     move 'NapersoftRepository'  to p-sql-database
003951
003952     move p-sql-database         to database
003953     MOVE 'naperadmin'           TO USR
003954     MOVE 'cCm8naper'            TO PASS
003955
003956     MOVE SPACES                 TO WS-LOOKUP-VALUE
003957
003958*    PERFORM 4100-CONNECT-TO-DB  THRU 4100-EXIT
003960      exec sql
              connect to :database
003961         user      :usr
003962         using     :pass
003963      end-exec
003964
003965     MOVE CM-STATE TO WS-LOOKUPNAME
003966     MOVE 'B'      TO WS-LOOKUPNAME (3:1)
003967
003969     EXEC SQL
                 SELECT LOOKUPVALUE
003970             INTO :WS-LOOKUP-VALUE
003971             FROM LOOKUPVALUES
003972               WHERE LOOKUPID = :WS-LOOKUPID
003973                 AND LOOKUPNAME = :WS-LOOKUPNAME
003974     END-EXEC
003975
003976     IF SQLCODE = 0
003977       OR SQLCODE = 100
003978        CONTINUE
003979     ELSE
003980        DISPLAY "ERROR: INVALID BALLOON STATE SELECT "
003981        DISPLAY ' SQL RETURN CODE ' SQLCODE
003982        DISPLAY ' SQL ERR MESS    ' SQLERRMC
003983     END-IF
003984
003985     PERFORM 4300-DISCONNECT THRU 4300-EXIT.
003986
003987 4500-EXIT.
003988     EXIT.
003989 9700-DATE-LINK.
003990
003991     
      * EXEC CICS LINK
003992*        PROGRAM   ('ELDATCV')
003993*        COMMAREA  (DATE-CONVERSION-DATA)
003994*        LENGTH    (DC-COMM-LENGTH)
003995*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00009350' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039333530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003996
003997
003998 9700-EXIT.
003999      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSRASBL' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 1160-CERT-TRL-REC-NOTFND
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSRASBL' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
