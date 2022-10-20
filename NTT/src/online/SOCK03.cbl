      *((program: SOCK03.cl2))
000001*****************************************************************
000002*                                                               *
000003* Copyright (c) 2001 by Sun Microsystems, Inc.                  *
000004* All rights reserved.                                          *
000005*                                                               *
000006*****************************************************************
000007 identification division.
000008 program-id. SOCK03.
000009*         This program is the first invoked by the SOC3
000010*         transaction. This transaction must be submited
000011*         by a message transmited over a stream socket.
000012*         The message format is:
000013
000014*         xxxx,12345678901234567890123456789012345
000015
000016*         where "xxxx" is the tran id SOCK (1 to 4 characters)
000017*         and "123456.........5" is up to 35 characters of
000018*         optional text.
000019
000020*         Information on the originating message and
000021*         remote socket is provided in the DFHCOMMAREA.
000022
000023*         As its first action this program must send a
000024*         message to the requesting socket. This lets the
000025*         remote program know its request has been
000026*         accepted and the transaction started.
000027
000028*         The program will make four transmissions to
000029*         the initating program in response to received
000030*         messages and then close the socket.
000031
000032*         This program is provided to show method and
000033*         has minimal checking and error messages.
000034*         The Cobol compiler, unless instructed otherwise,
000035*         will hold its working storage integer data items in
000036*         machine idependent form. This matches the network
000037*         ordering of information. Care should be exercised
000038*         to ensure that this data ordering is correct when
000039*         calling system library functions or passing data
000040*         structure containing such data to functions.
000041
000042******************************************************************
000043*                   C H A N G E   L O G
000044*
000045* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000046*-----------------------------------------------------------------
000047*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000048* EFFECTIVE    NUMBER
000049*-----------------------------------------------------------------
000050* 071112  IR2012042700001  PEMA  AHL CHANGES
000051* 081612  CR2011062300001  PEMA  REMOVE EXT DAY CHG FOR REFUNDS
000052* 010213  CR2011083000005  PEMA  ADD SPECIAL DCC SPP DDF REFUND
000053* 101813  IR2013101700003  PEMA  FIX MN Net Balloon calc.
000054* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000055* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
000056* 072415  IR2015071500003  PEMA  REMOVE CLOSE STATEMENT
000057* 010816  IR2015092900001  PEMA  USE CLP STATE WHERE NEEDED
000058*071117   IR2017061900003  PEMA  ALLOW 0% APR ON REFUNDS
000059* 100417  CR2017051000002  PEMA  Add fields to interface
000060* 100418  CR2018073000001  PEMA  REWORD REFUND METHODS
000061* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000062* 052319  IR2019052300001  PEMA  VIN&BENE not being passed at time
000063* 062119  CR2019050800001  PEMA  Add option to search by VIN
000064* 080519  IR2019080200001  PEMA  Allow multiple quotes by VIN
000065* 080919  IR2019080900001  PEMA  Shorten allowed Linkage Section
000066* 041720  IR2020041700001  PEMA  Change to not drop cert suffix.
000067* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000068* 080322  CR2021100800003  TANA  Add B and H claim types
000069******************************************************************
000070
000071
000072 environment division.
000073 data division.
000074 working-storage section.
       01  DFH-START PIC X(04).
000075*
000076* program buffers
000077*
000078 77  WS-BUFF-SENT-SW             PIC X  VALUE SPACES.
000079     88  ALREADY-SENT              VALUE 'Y'.
000080 77  WS-LENGTH                   PIC S9(4) COMP VALUE +0.
000081 77  WS-ELCERT-SW                PIC X  VALUE SPACES.
000082     88  ELCERT-FINISHED            VALUE 'Y'.
000083 77  WS-BIN-EFF-DT               PIC XX VALUE LOW-VALUES.
000084 77  WS-ERACCT-SW                PIC X VALUE SPACES.
000085     88  ERACCT-FOUND               VALUE 'Y'.
000086 77  WS-FREE-LOOK                PIC S999 COMP-3 VALUE +0.
000087 77 ws-send-msg-size           pic s9(8) comp value 4096.
000088 77 ws-recv-msg-size           pic s9(8) comp value 4096.
000089 77 ws-recv-buf                pic x(4096).
000090 77 ws-send-buf                pic x(4096) VALUE SPACES.
000091 77 ws-recv-total              pic s9(8) comp value 0.
000092 77 ws-recv-left               pic s9(8) comp value 0.
000093 77 ws-seq-num                 pic s9(8) comp value 0.
000094 77 ws-flags                   pic s9(8) comp value 0.
000095 77 WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
000096 77  WS-COMP-ID                  PIC XXX  VALUE SPACES.
000097 77 WS-SAVE-ACCOUNT              PIC X(10)  VALUE SPACES.
000098 77 WS-BIN-ORIG-EFF-DT           PIC XX  VALUE LOW-VALUES.
000099 77 WS-ORIG-EFF-DT               PIC X(10)  VALUE SPACES.
000100 77 WS-EFF-DATE                  PIC X(10)  VALUE SPACES.
000101 77 WS-EXP-DATE                  PIC X(10)  VALUE SPACES.
000102 77  ws-cancel-reason            pic x   value spaces.
000103 77  P1                          PIC S999 COMP-3 VALUE +0.
000104 77  P2                          PIC S999 COMP-3 VALUE +0.
000105 77  C0                          PIC S999 COMP-3 VALUE +0.
000106 77  C1                          PIC S999 COMP-3 VALUE +0.
000107 77  C2                          PIC S999 COMP-3 VALUE +0.
000108 77  C3                          PIC S999 COMP-3 VALUE +0.
000109 77 S1                           PIC S999 COMP-3 VALUE +0.
000110 77 S2                           PIC S999 COMP-3 VALUE +0.
000111 77 L1                           PIC S999 COMP-3 VALUE +0.
000112 77 L2                           PIC S999 COMP-3 VALUE +0.
000113 77 F1                           PIC S999 COMP-3 VALUE +0.
000114 77 WS-BUILD-SW                  PIC X.
000115    88  TIME-TO-BUILD               VALUE 'Y'.
000116 77 WS-SAVE-ERACCT               PIC X(2000).
000117 77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
000118 77 WS-STOP-SW                   PIC X  VALUE ' '.
000119    88  TOLD-TO-STOP               VALUE 'Y'.
000120 77 WS-PERFORM-SW                PIC X VALUE SPACES.
000121    88  GET-RATES                    VALUE 'R'.
000122    88  GET-ACT-ACCTS                VALUE 'A'.
000123 77 ws-bin-1st-pay-dt            pic xx  value low-values.
000124 77 WS-DISP-AMT                  PIC Z,ZZZ,Z99.99.
000125 77 ws-disp-rate                 pic z9.99999.
000126 77  WS-ERCTBL-SW                PIC X VALUE ' '.
000127     88  END-OF-ERCTBL                 VALUE 'Y'.
000128 77  WS-STATUS                   PIC X.
000129
000130 77  WS-CF-DEFAULT-APR           PIC S9(03)V9(04) COMP-3.
000131 77  WS-CF-CR-R78-METHOD         PIC X          VALUE SPACE.
000132 77  WS-CF-CR-REM-TERM-CALC      PIC X          VALUE SPACE.
000133 77  WS-BIN-VAL-DT               PIC XX         VALUE LOW-VALUES.
000134 77  WS-TOT-LF-RFND              PIC S9(7)V99  COMP-3 VALUE +0.
000135 77  WS-TOT-AH-RFND              PIC S9(7)V99  COMP-3 VALUE +0.
000136 77  WS-TOT-LF-PREM              PIC S9(7)V99  COMP-3 VALUE +0.
000137 77  WS-TOT-AH-PREM              PIC S9(7)V99  COMP-3 VALUE +0.
000138 77  WS-TOT-LF-COMM              PIC S9(7)V99  COMP-3 VALUE +0.
000139 77  WS-TOT-AH-COMM              PIC S9(7)V99  COMP-3 VALUE +0.
000140 77  WS-WORK-FACTOR              PIC S9V9(7)   COMP-3 VALUE +0.
000141 77  WS-ELCERT-KEY-SW            PIC X  VALUE SPACES.
000142     88  WS-ELCERT-FULL             VALUE '1'.
000143     88  WS-ELCERT-NAME             VALUE '2'.
000144     88  WS-ELCERT-CERT-NO          VALUE '3'.
000145     88  WS-ELCERT-ACT-NAME         VALUE '4'.
000146     88  WS-ELCERT-VIN              value '5'.
000147 77  ELMSTR-LENGTH               PIC S9(4) COMP VALUE +12.
000148 77  WS-CLM-STOP-SW              PIC X  VALUE ' '.
000149     88  I-SAY-TO-STOP      VALUE 'Y'.
000150 77  WS-HEX-0A                   PIC X.
000151 77  WS-CALL-TYPE                PIC X(6).
000152 77  WS-NCB-DIFF-MONTHS          PIC 999   VALUE ZEROS.
000153 77  WS-NCB-DIFF-ODD-DAYS        PIC 999   VALUE ZEROS.
000154 77  WS-STATE-EXT-DAYS-CHG       PIC X  VALUE ' '.
000155 77  DD-IU-SW                    PIC X   VALUE ' '.
000156     88  DD-IU-PRESENT                 VALUE 'Y'.
000157 77  WS-DDF-COMM-AND-MFEE        PIC S9(5)V99 VALUE +0 COMP-3.
000158 77  WS-DDF-ADMIN-FEES           PIC S9(5)V99 VALUE +0 COMP-3.
000159 77  WS-DDF-CSO-ADMIN-FEE        PIC S9(5)V99 VALUE +0 COMP-3.
000160 77  WS-DDF-1ST-YR-TOT-EXP       PIC S9(5)V99 VALUE +0 COMP-3.
000161 77  WS-COMM-PCT                 PIC S9(5)V9(5)  COMP-3 VALUE +0.
000162 77  WS-TERM                     PIC S999 COMP-3 VALUE +0.
000163 77  ws-ah-rfnd-clp              pic s9(7)v99 comp-3 value +0.
000164 77  TEX-FACT-8                  PIC S9V9(6)     COMP-3.
000165 77  WS-PDEF-RECORD-SW           PIC X           VALUE ' '.
000166     88  PDEF-FOUND                              VALUE 'Y'.
000167 77  ws-epiq-sw                  pic x  value ' '.
000168     88  ws-epiq-request           value 'Y'.
000169 77  ws-epiq-max                 pic s999 value +0 comp-3.
000170 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
000171
000172 01  WS-STRING-DATA.
000173     05  WS-COMMENT              PIC X(30)    VALUE SPACES.
000174     05  WS-RESP                 PIC 99999    VALUE ZEROS.
000175     05  WS-CARRIER              PIC X        VALUE SPACES.
000176     05  WS-GROUPING             PIC X(6)     VALUE SPACES.
000177     05  WS-STATE                PIC XX       VALUE SPACES.
000178     05  WS-ACCOUNT              PIC X(10)    VALUE SPACES.
000179     05  WS-CERT-NO              PIC X(11)    VALUE SPACES.
000180     05  WS-CERT-EFF-DATE        PIC 9(8)     VALUE ZEROS.
000181     05  WS-LAST-NAME            PIC X(15)    VALUE SPACES.
000182     05  WS-FIRST-NAME           PIC X(15)    VALUE SPACES.
000183     05  WS-MID-INIT             PIC X        VALUE SPACES.
000184     05  WS-POST-CARD-SW         PIC X        VALUE SPACES.
000185     05  WS-LF-COVERAGE-TYPE     PIC X        VALUE SPACES.
000186     05  WS-LF-STATUS            PIC X        VALUE SPACES.
000187     05  WS-LF-BEN-CODE          PIC XX       VALUE SPACES.
000188     05  WS-LF-BEN-CODE-DESC     PIC X(10)    VALUE SPACES.
000189     05  WS-LF-TERM              PIC 999      VALUE ZEROS.
000190     05  WS-LF-REM-TERM          PIC 999      VALUE ZEROS.
000191     05  WS-LF-PREM              PIC 9(7).99  VALUE ZEROS.
000192     05  WS-LF-REFUND            PIC 9(7).99  VALUE ZEROS.
000193     05  WS-LF-METHOD            PIC X(15)    VALUE SPACES.
000194     05  WS-LF-ORIG-BENEFIT      PIC 9(9).99  VALUE ZEROS.
000195     05  WS-LF-REM-BEN           PIC 9(9).99  VALUE ZEROS.
000196     05  WS-AH-COVERAGE-TYPE     PIC X        VALUE SPACES.
000197     05  WS-AH-STATUS            PIC X        VALUE SPACES.
000198     05  WS-AH-BEN-CODE          PIC XX       VALUE SPACES.
000199     05  WS-AH-BEN-CODE-DESC     PIC X(10)    VALUE SPACES.
000200     05  WS-AH-TERM              PIC 999      VALUE ZEROS.
000201     05  WS-AH-REM-TERM          PIC 999      VALUE ZEROS.
000202     05  WS-AH-PREM              PIC 9(7).99  VALUE ZEROS.
000203     05  WS-AH-REFUND            PIC 9(7).99  VALUE ZEROS.
000204     05  WS-AH-METHOD            PIC X(15)    VALUE SPACES.
000205     05  WS-AH-ORIG-BENEFIT      PIC 9(7).99  VALUE ZEROS.
000206     05  WS-AH-REM-BEN           PIC 9(7).99  VALUE ZEROS.
000207     05  WS-ACCOUNT-NAME         PIC X(30)    VALUE SPACES.
000208     05  WS-REPORT-CODE-1        PIC X(10)    VALUE SPACES.
000209     05  WS-REPORT-CODE-2        PIC X(10)    VALUE SPACES.
000210     05  WS-REPORT-CODE-3        PIC X(10)    VALUE SPACES.
000211     05  WS-LF-EXPIRE-DATE       PIC 9(8)     VALUE ZEROS.
000212     05  WS-AH-EXPIRE-DATE       PIC 9(8)     VALUE ZEROS.
000213     05  WS-LF-COMM              PIC 9(7).99  VALUE ZEROS.
000214     05  WS-AH-COMM              PIC 9(7).99  VALUE ZEROS.
000215     05  WS-LF-UEC               PIC 9(7).99  VALUE ZEROS.
000216     05  WS-AH-UEC               PIC 9(7).99  VALUE ZEROS.
000217     05  WS-LAST-JNAME           PIC X(15)    VALUE SPACES.
000218     05  WS-FIRST-JNAME          PIC X(15)    VALUE SPACES.
000219     05  WS-MID-JINIT            PIC X        VALUE SPACES.
000220     05  WS-CLM-PAID-THRU-DT     PIC 9(8)   VALUE ZEROS.
000221     05  WS-LF-BIN-PAID-THRU-DT  PIC XX     VALUE LOW-VALUES.
000222     05  WS-AH-BIN-PAID-THRU-DT  PIC XX     VALUE LOW-VALUES.
000223     05  WS-LF-CANCEL-DATE       PIC 9(8)   VALUE ZEROS.
000224     05  WS-AH-CANCEL-DATE       PIC 9(8)   VALUE ZEROS.
000225     05  WS-VIN                  PIC X(17)  VALUE SPACES.
000226     05  WS-CRED-BENE-NAME       PIC X(30)  VALUE SPACES.
000227
000228 01  ws-work-date.
000229     05  ws-work-ccyy            pic x(4).
000230     05  ws-work-mm              pic xx.
000231     05  ws-work-dd              pic xx.
000232 01  ws-work-date-num redefines ws-work-date
000233                                 pic 9(8).
000234
000235 01  FILLER.
000236     12  WS-CF-REM-TRM-CALC-OPTION PIC X  VALUE SPACE.
000237     12  WS-BALLOON-RTRM         PIC 999  VALUE ZEROS.
000238     12  WS-STATE-ABBREVIATION   PIC XX.
000239     12  WS-ACCT-USER-FLD-5      PIC  X(02).
000240     12  WS-AH-SPECIAL-CALC-CD   PIC  X(01)      VALUE SPACE.
000241     12  WS-AH-BEN-CATEGORY      PIC  X          VALUE SPACE.
000242     12  WS-LF-SPECIAL-CALC-CD   PIC  X(01)      VALUE SPACE.
000243     12  WS-AH-CO-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000244     12  WS-LF-CO-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000245     12  WS-AH-CO-EARNINGS-CALC  PIC  X(01)      VALUE SPACE.
000246     12  WS-LF-CO-EARNINGS-CALC  PIC  X(01)      VALUE SPACE.
000247     12  WS-AH-CO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000248     12  WS-LF-CO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000249     12  WS-AH-ST-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000250     12  WS-LF-ST-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000251     12  WS-AH-FO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000252     12  WS-LF-FO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000253     12  WS-AH-ST-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000254     12  WS-LF-ST-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000255     12  WS-AH-AM-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000256     12  WS-LF-AM-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000257     12  WS-AM-EARN-METHOD-A     PIC  X(01)      VALUE SPACE.
000258     12  WS-AM-EARN-METHOD-L     PIC  X(01)      VALUE SPACE.
000259     12  WS-AM-EARN-METHOD-R     PIC  X(01)      VALUE SPACE.
000260     12  WS-LF-OVERRIDE-L1       PIC  X(01).
000261     12  WS-LF-OVERRIDE-L2       PIC  X(02).
000262     12  WS-AH-OVERRIDE-L1       PIC  X(01).
000263     12  WS-AH-OVERRIDE-L2       PIC  X(02).
000264     12  WS-CF-LF-COVERAGE-TYPE  PIC  X(01)      VALUE SPACE.
000265         88  WS-REDUCING                         VALUE 'R'.
000266         88  WS-LEVEL                            VALUE 'L'  'P'.
000267
000268 01  ELMSTR-KEY.
000269     12  ELMSTR-COMP-CD          PIC X.
000270     12  ELMSTR-CERT-NO          PIC X(11).
000271
000272 01  CTBL-KEY-SAVE               PIC X(5).
000273 01  CTBL-KEY.
000274     05  CTBL-COMPANY-CD         PIC X.
000275     05  CTBL-TABLE              PIC XXX.
000276     05  CTBL-BEN-TYPE           PIC X.
000277     05  CTBL-BEN-CODE           PIC XX.
000278
000279 01  ERPDEF-KEY-SAVE             PIC X(18).
000280 01  ERPDEF-KEY.
000281     12  ERPDEF-COMPANY-CD       PIC X.
000282     12  ERPDEF-STATE            PIC XX.
000283     12  ERPDEF-PROD-CD          PIC XXX.
000284     12  F                       PIC X(7).
000285     12  ERPDEF-BEN-TYPE         PIC X.
000286     12  ERPDEF-BEN-CODE         PIC XX.
000287     12  ERPDEF-EXP-DT           PIC XX.
000288
000289 01  WS-AM3-KEY.
000290     12  WS-AM3-ACCOUNT          PIC X(10).
000291     12  WS-AM3-EXP-DT           PIC XX.
000292 01  WS-AM-KEY.
000293     12  WS-AM-COMPANY-CD        PIC X.
000294     12  WS-AM-CARRIER           PIC X.
000295     12  WS-AM-GROUPING          PIC X(6).
000296     12  WS-AM-STATE             PIC XX.
000297     12  WS-AM-ACCOUNT           PIC X(10).
000298     12  WS-AM-EXPIRATION-DT     PIC XX.
000299     12  WS-AM-FILLER            PIC XXXX.
000300 01  WS-CO-DATA.
000301     05  WS-CO-NUM               PIC X(10).
000302     05  WS-CO-PRIMARY-CONTACT   PIC X(30).
000303     05  WS-CO-NAME              PIC X(30).
000304     05  WS-CO-MAIL-NAME         PIC X(30).
000305     05  WS-CO-ADDR1             PIC X(30).
000306     05  WS-CO-ADDR2             PIC X(30).
000307     05  WS-CO-ADDR3             PIC X(30).
000308     05  WS-CO-ZIP               PIC X(9).
000309     05  WS-CO-PHONE             PIC X(10).
000310
000311 01  WS-CF-KEY.
000312     12  WS-CF-COMPANY-ID        PIC  X(03)      VALUE SPACES.
000313     12  WS-CF-RECORD-TYPE       PIC  X(01)      VALUE ZERO.
000314*        88  COMPANY-MASTER                      VALUE '1'.
000315*        88  STATE-MASTER                        VALUE '3'.
000316*        88  LF-BENEFIT-MASTER                   VALUE '4'.
000317*        88  AH-BENEFIT-MASTER                   VALUE '5'.
000318     12  WS-CF-ACCESS.
000319         16  WS-CF-STATE         PIC  X(02)      VALUE SPACES.
000320         16  WS-CF-BENEFIT-NO                    VALUE SPACES.
000321             20  FILLER          PIC  X(01).
000322             20  WS-CF-CARRIER   PIC  X(01).
000323     12  WS-CF-SEQUENCE-NO       PIC S9(04) COMP VALUE ZERO.
000324
000325 01  WS-CS-KEY.
000326     05  WS-CS-COMPANY-CD        PIC X.
000327     05  WS-CS-CARRIER           PIC X.
000328     05  WS-CS-GROUP             PIC X(6).
000329     05  WS-CS-STATE             PIC XX.
000330     05  WS-CS-ACCOUNT           PIC X(10).
000331     05  WS-CS-EFF-DT            PIC XX.
000332     05  WS-CS-CERT-NO           PIC X(11).
000333     05  WS-CS-TRLR-TYPE         PIC X.
000334
000335 01  WS-MA-KEY.
000336     05  WS-MA-COMPANY-CD        PIC X.
000337     05  WS-MA-CARRIER           PIC X.
000338     05  WS-MA-GROUP             PIC X(6).
000339     05  WS-MA-STATE             PIC XX.
000340     05  WS-MA-ACCOUNT           PIC X(10).
000341     05  WS-MA-EFF-DT            PIC XX.
000342     05  WS-MA-CERT-NO           PIC X(11).
000343
000344 01  WS-CM-KEY.
000345     05  WS-CM-COMPANY-CD        PIC X.
000346     05  WS-CM-CARRIER           PIC X.
000347     05  WS-CM-GROUP             PIC X(6).
000348     05  WS-CM-STATE             PIC XX.
000349     05  WS-CM-ACCOUNT           PIC X(10).
000350     05  WS-CM-EFF-DT            PIC XX.
000351     05  WS-CM-CERT-NO           PIC X(11).
000352
000353 01  WS-CM-KEY-A1.
000354     05  WS-CM-COMPANY-CD-A1     PIC X.
000355     05  WS-CM-LAST-NAME         PIC X(15).
000356     05  WS-CM-INITIALS          PIC XX.
000357
000358 01  WS-CM-KEY-A4.
000359     05  WS-CM-COMPANY-CD-A4     PIC X.
000360     05  WS-CM-CERT-NO-A4        PIC X(11).
000361
000362 01  WS-CID-NO                   PIC X(8).
000363
000364 01  soc-client-in-data.
000365     05  CLIENT-CAR              PIC X.
000366     05  CLIENT-GRP              PIC X(6).
000367     05  CLIENT-STATE            PIC XX.
000368     05  CLIENT-ACCOUNT          PIC X(10).
000369     05  CLIENT-EFF-DT           PIC X(8). *> ccyymmdd
000370     05  CLIENT-CERT-NO          PIC X(11).
000371     05  CLIENT-LAST-NAME        PIC X(15).
000372     05  CLIENT-FIRST-NAME       PIC X(10).
000373     05  CLIENT-VAL-DT           PIC X(8). *> ccyymmdd
000374     05  CLIENT-CAN-REASON       PIC X.
000375     05  client-vin              pic x(17).
000376
000377 01  WS-DISP-RESP                PIC 9(5).
000378 01  WS-RESPONSE                 PIC S9(8)   COMP.
000379     88  RESP-NORMAL                  VALUE +00.
000380     88  RESP-NOTFND                  VALUE +13.
000381     88  RESP-DUPKEY                  VALUE +15.
000382     88  RESP-NOTOPEN                 VALUE +19.
000383     88  RESP-ENDFILE                 VALUE +20.
000384
000385*                                 COPY ELCCERT.
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
000386*                                 COPY ELCMSTR.
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
000387*                                 COPY ERCPDEF.
      *>>((file: ERCPDEF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPDEF.                            *
000005*                                                                *
000006*    FILE DESCRIPTION = PRODUCT DEFINITION MASTER                *
000007*                                                                *
000008*    FILE TYPE = VSAM,KSDS                                       *
000009*    RECORD SIZE = 1319 RECFORM = FIXED                          *
000010*                                                                *
000011*    BASE CLUSTER = ERPDEF                      RKP=02,LEN=18    *
000012*                                                                *
000013*    LOG = YES                                                   *
000014*    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000015******************************************************************
000016*                   C H A N G E   L O G
000017*
000018* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000019*-----------------------------------------------------------------
000020*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000021* EFFECTIVE    NUMBER
000022*-----------------------------------------------------------------
000023* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000024* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000025* 100314  CR2014061900001  PEMA  ADD PCT OF BENEFIT
000026* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000027******************************************************************
000028 01  PRODUCT-MASTER.
000029    12  PD-RECORD-ID                 PIC X(02).
000030        88  VALID-PD-ID                  VALUE 'PD'.
000031
000032    12  PD-CONTROL-PRIMARY.
000033        16  PD-COMPANY-CD            PIC X.
000034        16  PD-STATE                 PIC XX.
000035        16  PD-PRODUCT-CD            PIC XXX.
000036        16  PD-FILLER                PIC X(7).
000037        16  PD-BEN-TYPE              PIC X.
000038        16  PD-BEN-CODE              PIC XX.
000039        16  PD-PROD-EXP-DT           PIC XX.
000040
000041    12  FILLER                       PIC X(50).
000042
000043    12  PD-PRODUCT-DATA OCCURS 8.
000044        16  PD-PROD-CODE             PIC X.
000045            88  PD-PROD-LIFE           VALUE 'L'.
000046            88  PD-PROD-PROP           VALUE 'P'.
000047            88  PD-PROD-AH             VALUE 'A'.
000048            88  PD-PROD-IU             VALUE 'I'.
000049            88  PD-PROD-GAP            VALUE 'G'.
000050            88  PD-PROD-FAML           VALUE 'F'.
000051            88  PD-PROD-OTH            VALUE 'O'.
000052        16  PD-MAX-ATT-AGE           PIC S999        COMP-3.
000053        16  PD-MIN-ISSUE-AGE         PIC S999        COMP-3.
000054        16  PD-MAX-ISSUE-AGE         PIC S999        COMP-3.
000055        16  PD-MAX-TERM              PIC S999        COMP-3.
000056        16  PD-MAX-AMT               PIC S9(07)      COMP-3.
000057        16  FILLER                   PIC X.
000058        16  PD-PRE-EXIST-EXCL-TYPE   PIC 99.
000059        16  PD-EXCLUSION-PERIOD-DAYS PIC S999        COMP-3.
000060        16  PD-COVERAGE-ENDS-MOS     PIC S999        COMP-3.
000061        16  PD-ACCIDENT-ONLY-MOS     PIC S999        COMP-3.
000062        16  PD-CRIT-PERIOD           PIC S999        COMP-3.
000063        16  PD-REC-CRIT-PERIOD       PIC 99.
000064        16  PD-REC-CP-ALPHA  REDEFINES PD-REC-CRIT-PERIOD.
000065            20  PD-RECURRING-YN      PIC X.
000066            20  FILLER               PIC X.
000067        16  PD-RTW-MOS               PIC 99.
000068        16  PD-MAX-EXTENSION         PIC 99.
000069        16  pd-ben-pct               pic sv999 comp-3.
000070*       16  FILLER                   PIC XX.
000071
000072    12  PD-1ST-YR-ADMIN-ALLOW        PIC S9(3)V99    COMP-3.
000073
000074    12  PD-TERM-LIMITS OCCURS 15.
000075        16  PD-LOW-TERM              PIC S999        COMP-3.
000076        16  PD-HI-TERM               PIC S999        COMP-3.
000077
000078*  THE LOAN AMT LIMITS CORRESPOND TO THE TERM LIMITS ABOVE
000079    12  PD-LOAN-AMT-LIMITS OCCURS 15.
000080        16  PD-LOW-AMT               PIC S9(5)       COMP-3.
000081        16  PD-HI-AMT                PIC S9(7)       COMP-3.
000082
000083    12  PD-EARN-FACTORS.
000084        16  FILLER OCCURS 15.
000085            20  FILLER OCCURS 15.
000086                24  PD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
000087
000088    12  PD-PRODUCT-DESC              PIC X(80).
000089    12  PD-TRUNCATED                 PIC X.
000090    12  FILLER                       PIC X(59).
000091
000092    12  PD-MAINT-INFORMATION.
000093        16  PD-LAST-MAINT-DT         PIC X(02).
000094        16  PD-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
000095        16  PD-LAST-MAINT-BY         PIC X(04).
      *<<((file: ERCPDEF))
000388*                                 COPY ERCCTBL.
      *>>((file: ERCCTBL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCCTBL                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003
000007*                                                                *
000008*   ONLINE CREDIT SYSTEM                                         *
000009*                                                                *
000010*   FILE DESCRIPTION = COMPENSATION TABLE                        *
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 200   RECFORM = FIXED                          *
000014*                                                                *
000015*   BASE CLUSTER NAME = ERCTBL                   RKP=2,LEN=7     *
000016*       ALTERNATE PATH = NONE                                    *
000017*                                                                *
000018*   LOG = NO                                                     *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020*                                                                *
000021*                                                                *
000022******************************************************************
000023
000024 01  COMM-TABLE-RECORD.
000025     12  CT-RECORD-ID                      PIC XX.
000026         88  VALID-CT-ID                      VALUE 'CT'.
000027
000028     12  CT-CONTROL-PRIMARY.
000029         16  CT-COMPANY-CD                 PIC X.
000030         16  CT-TABLE                      PIC XXX.
000031         16  CT-CNTRL-2.
000032             20  CT-BEN-TYPE               PIC X.
000033             20  CT-BEN-CODE               PIC XX.
000034
000035     12  CT-MAINT-INFORMATION.
000036         16  CT-LAST-MAINT-DT              PIC XX.
000037         16  CT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000038         16  CT-LAST-MAINT-USER            PIC X(4).
000039         16  FILLER                        PIC X(31).
000040
000041     12  CT-LIMITS.
000042         16  CT-TBF OCCURS 3 TIMES         PIC S9(7)V99   COMP-3.
000043
000044         16  CT-AGE OCCURS 3 TIMES         PIC S99        COMP-3.
000045
000046         16  CT-TRM OCCURS 3 TIMES         PIC S999       COMP-3.
000047
000048     12  CT-RATES.
000049         16  CT-RTX          OCCURS 27 TIMES.
000050             20  CT-RT                     PIC SV9(5)     COMP-3.
000051             20  CT-RT-R   REDEFINES
000052                 CT-RT                     PIC XXX.
000053
000054     12  FILLER                            PIC  X(42).
000055
000056******************************************************************
      *<<((file: ERCCTBL))
000389*                                 COPY ELCCNTL.
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
000390*                                 COPY ERCACCT.
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
000391*                                 COPY ERCMAIL.
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
000392*                                 COPY ELCCRTT.
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
000393*                                 COPY ELCDATE.
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
000394*                                 COPY ELCCALC.
      *>>((file: ELCCALC))
000001******************************************************************
000002*                                                                *
000003*                           ELCCALC.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.025                          *
000006*                                                                *
000007*   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
000008*                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
000009*                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
000010*                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
000011*                                                                *
000012*  PASSED TO ELRTRM                                              *
000013*  -----------------                                             *
000014*  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
000015*  ORIGINAL TERM                                                 *
000016*  BEGINNING DATE                                                *
000017*  ENDING DATE                                                   *
000018*  COMPANY I.D.                                                  *
000019*  ACCOUNT MASTER USER FIELD                                     *
000020*  PROCESS SWITCH (CANCEL, CLAIM)                                *
000021*  FREE LOOK DAYS                                                *
000022*                                                                *
000023*  RETURNED FROM ELRTRM                                          *
000024*  ---------------------                                         *
000025*  REMAINING TERM 1 - USED FOR EARNINGS                          *
000026*  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
000027*  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
000028*  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
000029*----------------------------------------------------------------*
000030*  PASSED TO ELRAMT                                              *
000031*  ----------------                                              *
000032*  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
000033*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000034*  ORIGINAL AMOUNT                                               *
000035*  ALTERNATE BENEFIT (BALLON)                                    *
000036*  A.P.R. - NET PAY ONLY                                         *
000037*  METHOD
000038*  PAYMENT FREQUENCY - FOR FARM PLAN                             *
000039*  COMPANY I.D.                                                  *
000040*  BENEFIT TYPE                                                  *
000041*                                                                *
000042*  RETURNED FROM ELRAMT                                          *
000043*  --------------------                                          *
000044*  REMAINING AMOUNT 1 - CURRENT                                  *
000045*  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
000046*  REMAINING AMOUNT FACTOR
000047*----------------------------------------------------------------*
000048*  PASSED TO ELRESV                                              *
000049*  -----------------                                             *
000050*  CERTIFICATE EFFECTIVE DATE                                    *
000051*  VALUATION DATE                                                *
000052*  PAID THRU DATE                                                *
000053*  BENEFIT                                                       *
000054*  INCURRED DATE                                                 *
000055*  REPORTED DATE                                                 *
000056*  ISSUE AGE                                                     *
000057*  TERM                                                          *
000058*  CDT PERCENT                                                   *
000059*  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
000060* *CLAIM TYPE (LIFE, A/H)                                        *
000061* *REMAINING BENEFIT (FROM ELRAMT)                               *
000062* *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
000063*                                                                *
000064*  RETURNED FROM ELRESV                                          *
000065*  --------------------                                          *
000066*  CDT TABLE USED                                                *
000067*  CDT FACTOR USED                                               *
000068*  PAY TO CURRENT RESERVE                                        *
000069*  I.B.N.R. - A/H ONLY                                           *
000070*  FUTURE (ACCRUED) AH ONLY                                      *
000071*----------------------------------------------------------------*
000072*  PASSED TO ELRATE                                              *
000073*  ----------------                                              *
000074*  CERT ISSUE DATE                                               *
000075*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000076*  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
000077*  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
000078*  STATE CODE (CLIENT DEFINED)                                   *
000079*  STATE CODE (STANDARD P.O. ABBRV)                              *
000080*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000081*  DEVIATION CODE                                                *
000082*  ISSUE AGE                                                     *
000083*  ORIGINAL BENEFIT AMOUNT                                       *
000084*  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
000085*  PROCESS TYPE (ISSUE OR CANCEL)                                *
000086*  BENEFIT KIND (LIFE OR A/H)                                    *
000087*  A.P.R.                                                        *
000088*  METHOD
000089*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000090*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000091*  COMPANY I.D. (3 CHARACTER)                                    *
000092*  BENEFIT CODE                                                  *
000093*  BENEFIT OVERRIDE CODE                                         *
000094*  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
000095*  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
000096*  JOINT INDICATOR (CSL ONLY)                                    *
000097*  FIRST PAYMENT DATE (CSL ONLY)                                 *
000098*  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
000099*                                                                *
000100*  RETURNED FROM ELRATE                                          *
000101*  --------------------                                          *
000102*  CALCULATED PREMIUM                                            *
000103*  PREMIUM RATE                                                  *
000104*  MORTALITY CODE                                                *
000105*  MAX ATTAINED AGE                                              *
000106*  MAX AGE                                                       *
000107*  MAX TERM                                                      *
000108*  MAX MONTHLY BENEFIT                                           *
000109*  MAX TOTAL BENIFIT                                             *
000110*  COMPOSITE RATE (OPEN-END ONLY)                                *
000111*----------------------------------------------------------------*
000112*  PASSED TO ELRFND                                              *
000113*  ----------------                                              *
000114*  CERT ISSUE DATE                                               *
000115*  REFUND DATE                                                   *
000116*  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
000117*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000118*  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
000119*  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
000120*  STATE CODE (CLIENT DEFINED)                                   *
000121*  STATE CODE (STANDARD P.O. ABBRV)                              *
000122*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000123*  DEVIATION CODE                                                *
000124*  ISSUE AGE                                                     *
000125*  ORIGINAL BENEFIT AMOUNT                                       *
000126*  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
000127*  PROCESS TYPE (CANCEL)                                         *
000128*  BENEFIT KIND (LIFE OR A/H)                                    *
000129*  A.P.R.                                                        *
000130*  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
000131*  RATING METHOD -  (CODE FROM BENEFIT)                          *
000132*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000133*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000134*  COMPANY I.D. (3 CHARACTER)                                    *
000135*  BENEFIT CODE                                                  *
000136*  BENEFIT OVERRIDE CODE                                         *
000137*                                                                *
000138*  RETURNED FROM ELRFND                                          *
000139*  --------------------                                          *
000140*  CALCULATED REFUND                                             *
000141*----------------------------------------------------------------*
000142*  PASSED TO ELEARN                                              *
000143*  ----------------                                              *
000144*  CERT ISSUE DATE                                               *
000145*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000146*  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
000147*  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
000148*  STATE CODE (CLIENT DEFINED)                                   *
000149*  STATE CODE (STANDARD P.O. ABBRV)                              *
000150*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000151*  DEVIATION CODE                                                *
000152*  ISSUE AGE                                                     *
000153*  ORIGINAL BENEFIT AMOUNT                                       *
000154*  BENEFIT KIND (LIFE OR A/H)                                    *
000155*  A.P.R.                                                        *
000156*  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
000157*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000158*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000159*  COMPANY I.D. (3 CHARACTER)                                    *
000160*  BENEFIT CODE                                                  *
000161*  BENEFIT OVERRIDE CODE                                         *
000162*                                                                *
000163*  RETURNED FROM ELEARN                                          *
000164*  --------------------                                          *
000165*  INDICATED  EARNINGS                                           *
000166*----------------------------------------------------------------*
000167*                 LENGTH = 450                                   *
000168*                                                                *
000169******************************************************************
000170******************************************************************
000171*                   C H A N G E   L O G
000172*
000173* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000174*-----------------------------------------------------------------
000175*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000176* EFFECTIVE    NUMBER
000177*-----------------------------------------------------------------
000178* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
000179* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
000180* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
000181* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
000182* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
000183* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
000184* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
000185* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
000186* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000187* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
000188* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000189* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
000190******************************************************************
000191
000192 01  CALCULATION-PASS-AREA.
000193     12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
000194                                     COMP.
000195
000196     12  CP-RETURN-CODE            PIC X             VALUE ZERO.
000197       88  NO-CP-ERROR                             VALUE ZERO.
000198       88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
000199                                  '9' 'A' 'B' 'C' 'D' 'E' 'H' 'I'.
000200       88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
000201       88  CP-ERROR-IN-DATES                       VALUE '2'.
000202       88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
000203       88  CP-ERROR-IN-TERMS                       VALUE '4'.
000204       88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
000205       88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
000206       88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
000207       88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
000208       88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
000209       88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
000210       88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
000211       88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
000212       88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
000213       88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
000214       88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
000215       88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
000216       88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
000217       88  CP-ERROR-TERM-BELOW-MINIMUM             VALUE 'I'.
000218
000219     12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
000220       88  NO-CP-ERROR-2                           VALUE ZERO.
000221***********************  INPUT AREAS ****************************
000222
000223     12  CP-CALCULATION-AREA.
000224         16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
000225         16  CP-CERT-EFF-DT        PIC XX.
000226         16  CP-VALUATION-DT       PIC XX.
000227         16  CP-PAID-THRU-DT       PIC XX.
000228         16  CP-BENEFIT-TYPE       PIC X.
000229           88  CP-AH                               VALUE 'A' 'D'
000230                                                   'I' 'U'.
000231           88  CP-REDUCING-LIFE                    VALUE 'R'.
000232           88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
000233         16  CP-INCURRED-DT        PIC XX.
000234         16  CP-REPORTED-DT        PIC XX.
000235         16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
000236         16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
000237         16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
000238                                     COMP-3.
000239         16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
000240                                     COMP-3.
000241         16  CP-CDT-METHOD         PIC X.
000242           88  CP-CDT-ROUND-NEAR                   VALUE '1'.
000243           88  CP-CDT-ROUND-HIGH                   VALUE '2'.
000244           88  CP-CDT-INTERPOLATED                 VALUE '3'.
000245         16  CP-CLAIM-TYPE         PIC X.
000246           88  CP-AH-CLAIM                         VALUE 'A'.
000247           88  CP-LIFE-CLAIM                       VALUE 'L'.
000248         16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
000249                                     COMP-3.
000250         16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
000251                                     COMP-3.
000252         16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
000253                                     COMP-3.
000254         16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
000255                                     COMP-3.
000256         16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
000257                                     COMP-3.
000258         16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
000259                                     COMP-3.
000260         16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
000261                                     COMP-3.
000262         16  CP-REM-TERM-METHOD    PIC X.
000263           88  CP-EARN-AFTER-15TH                  VALUE '1'.
000264           88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
000265           88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
000266           88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
000267           88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
000268           88  CP-EARN-AFTER-14TH                  VALUE '6'.
000269           88  CP-EARN-AFTER-16TH                  VALUE '7'.
000270         16  CP-EARNING-METHOD     PIC X.
000271           88  CP-EARN-BY-R78                      VALUE '1' 'R'.
000272           88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
000273           88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
000274           88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
000275           88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
000276           88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
000277           88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
000278           88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
000279           88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
000280           88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
000281           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
000282           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
000283           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
000284           88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
000285         16  CP-PROCESS-TYPE       PIC X.
000286           88  CP-CLAIM                            VALUE '1'.
000287           88  CP-CANCEL                           VALUE '2'.
000288           88  CP-ISSUE                            VALUE '3'.
000289         16  CP-SPECIAL-CALC-CD    PIC X.
000290           88  CP-OUTSTANDING-BAL              VALUE 'O'.
000291           88  CP-1-MTH-INTEREST               VALUE ' '.
000292           88  CP-0-MTH-INTEREST               VALUE 'A'.
000293           88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
000294           88  CP-CRITICAL-PERIOD              VALUE 'C'.
000295           88  CP-TERM-IS-DAYS                 VALUE 'D'.
000296           88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
000297           88  CP-FARM-PLAN                    VALUE 'F'.
000298           88  CP-RATE-AS-STANDARD             VALUE 'G'.
000299           88  CP-2-MTH-INTEREST               VALUE 'I'.
000300           88  CP-3-MTH-INTEREST               VALUE 'J'.
000301           88  CP-4-MTH-INTEREST               VALUE 'K'.
000302           88  CP-BALLOON-LAST-PMT             VALUE 'L'.
000303           88  CP-MORTGAGE-REC                 VALUE 'M'.
000304           88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
000305           88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
000306           88  CP-NET-PAY-SIMPLE               VALUE 'S'.
000307           88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
000308                                                     'W' 'X'.
000309           88  CP-TRUNCATE-0-MTH               VALUE 'T'.
000310           88  CP-TRUNCATE-1-MTH               VALUE 'U'.
000311           88  CP-TRUNCATE-2-MTH               VALUE 'V'.
000312           88  CP-TRUNCATE-3-MTH               VALUE 'W'.
000313           88  CP-TRUNCATE-4-MTH               VALUE 'X'.
000314           88  CP-SUMMARY-REC                  VALUE 'Z'.
000315           88  CP-PROPERTY-BENEFIT             VALUE '2'.
000316           88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
000317           88  CP-AD-D-BENEFIT                 VALUE '4'.
000318           88  CP-CSL-METH-1                   VALUE '5'.
000319           88  CP-CSL-METH-2                   VALUE '6'.
000320           88  CP-CSL-METH-3                   VALUE '7'.
000321           88  CP-CSL-METH-4                   VALUE '8'.
000322
000323         16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
000324                                     COMP-3.
000325         16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
000326         16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
000327         16  CP-STATE              PIC XX          VALUE SPACE.
000328         16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
000329         16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
000330           88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
000331               '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
000332         16  CP-R78-OPTION         PIC X.
000333           88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
000334           88  CP-TERM-TIMES-TERM                  VALUE '1'.
000335
000336         16  CP-COMPANY-CD         PIC X             VALUE SPACE.
000337         16  CP-IBNR-RESERVE-SW    PIC X.
000338         16  CP-CLAIM-STATUS       PIC X.
000339         16  CP-RATE-FILE          PIC X.
000340         16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
000341                                     COMP-3.
000342
000343         16  CP-LIFE-OVERRIDE-CODE PIC X.
000344         16  CP-AH-OVERRIDE-CODE   PIC X.
000345
000346         16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
000347                                     COMP-3.
000348         16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
000349                                   PIC S9(5)V99 COMP-3.
000350         16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
000351                                     COMP-3.
000352         16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
000353                                     COMP-3.
000354         16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
000355                                     COMP-3.
000356         16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
000357                                  PIC S9(7)V99 COMP-3.
000358
000359         16  CP-PAID-FROM-DATE     PIC X(02).
000360         16  CP-CLAIM-CALC-METHOD  PIC X(01).
000361         16  CP-EXT-DAYS-CALC      PIC X.
000362           88  CP-EXT-NO-CHG                   VALUE ' '.
000363           88  CP-EXT-CHG-LF                   VALUE '1'.
000364           88  CP-EXT-CHG-AH                   VALUE '2'.
000365           88  CP-EXT-CHG-LF-AH                VALUE '3'.
000366         16  CP-DOMICILE-STATE     PIC XX.
000367         16  CP-CARRIER            PIC X.
000368         16  CP-REIN-FLAG          PIC X.
000369         16  CP-REM-TRM-CALC-OPTION PIC X.
000370           88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
000371                      '2' '3' '4' '5'.
000372           88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
000373           88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
000374           88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
000375           88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
000376           88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
000377           88  CP-EXT-30-DAY-MONTH          VALUE '3'.
000378           88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
000379           88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
000380         16  CP-SIG-SWITCH         PIC X.
000381         16  CP-RATING-METHOD      PIC X.
000382           88  CP-RATE-AS-R78                      VALUE '1' 'R'.
000383           88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
000384           88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
000385           88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
000386           88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
000387           88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
000388           88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
000389           88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
000390           88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
000391         16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
000392                                     COMP-3.
000393         16  CP-BEN-CATEGORY       PIC X.
000394         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
000395         16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
000396                                   PIC S99V9(5) COMP-3.
000397         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
000398         16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
000399                                   PIC S99V9(5) COMP-3.
000400         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
000401         16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
000402         16  CP-EXPIRE-DT          PIC XX.
000403         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
000404         16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
000405         16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
000406         16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
000407         16  CP-DDF-SPEC-CALC      PIC X.
000408             88  CP-CALC-GROSS-FEE        VALUE 'G'.
000409             88  CP-CALC-CLP              VALUE 'C'.
000410         16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
000411         16  CP-CANCEL-REASON      PIC X.
000412         16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
000413         16  CP-PMT-MODE           PIC X.
000414         16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
000415         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
000416         16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
000417         16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
000418         16  FILLER                PIC X.
000419
000420***************    OUTPUT FROM ELRESV   ************************
000421
000422         16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
000423
000424         16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
000425                                     COMP-3.
000426         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
000427                                     COMP-3.
000428         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
000429                                     COMP-3.
000430         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
000431                                     COMP-3.
000432         16  FILLER                PIC X(09).
000433***************    OUTPUT FROM ELRTRM   *************************
000434
000435         16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
000436                                     COMP-3.
000437         16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
000438                                     COMP-3.
000439         16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
000440                                     COMP-3.
000441         16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
000442                                     COMP-3.
000443         16  FILLER                PIC X(12).
000444
000445***************    OUTPUT FROM ELRAMT   *************************
000446
000447         16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
000448                                     COMP-3.
000449         16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
000450                                     COMP-3.
000451         16  FILLER                PIC X(12).
000452
000453***************    OUTPUT FROM ELRATE   *************************
000454
000455         16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
000456                                     COMP-3.
000457         16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
000458                                     COMP-3.
000459         16  CP-MORTALITY-CODE     PIC X(4).
000460         16  CP-RATE-EDIT-FLAG     PIC X.
000461             88  CP-RATES-NEED-APR                  VALUE '1'.
000462         16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
000463                                     COMP-3.
000464         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
000465         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
000466         16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
000467                                   PIC S9(7)V99 COMP-3.
000468         16  FILLER                PIC X(07).
000469
000470***************    OUTPUT FROM ELRFND   *************************
000471
000472         16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
000473                                     COMP-3.
000474         16  CP-REFUND-TYPE-USED   PIC X.
000475           88  CP-R-AS-R78                         VALUE '1'.
000476           88  CP-R-AS-PRORATA                     VALUE '2'.
000477           88  CP-R-AS-CALIF                       VALUE '3'.
000478           88  CP-R-AS-TEXAS                       VALUE '4'.
000479           88  CP-R-AS-FARM-PLAN                   VALUE '4'.
000480           88  CP-R-AS-NET-PAY                     VALUE '5'.
000481           88  CP-R-AS-ANTICIPATION                VALUE '6'.
000482           88  CP-R-AS-MEAN                        VALUE '8'.
000483           88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
000484           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
000485           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
000486           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
000487           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
000488           88  CP-R-AS-REPOSSESSION                VALUE 'R'.
000489         16  FILLER                PIC X(12).
000490
000491***************    OUTPUT FROM ELEARN   *************************
000492
000493         16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
000494                                     COMP-3.
000495         16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
000496                                     COMP-3.
000497         16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
000498                                     COMP-3.
000499         16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
000500                                     COMP-3.
000501         16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
000502                                     COMP-3.
000503         16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
000504                                     COMP-3.
000505         16  CP-EARNING-TYPE-USED  PIC X.
000506           88  CP-E-AS-SPECIAL                     VALUE 'S'.
000507           88  CP-E-AS-R78                         VALUE '1'.
000508           88  CP-E-AS-PRORATA                     VALUE '2'.
000509           88  CP-E-AS-TEXAS                       VALUE '4'.
000510           88  CP-E-AS-FARM-PLAN                   VALUE '4'.
000511           88  CP-E-AS-NET-PAY                     VALUE '5'.
000512           88  CP-E-AS-ANTICIPATION                VALUE '6'.
000513           88  CP-E-AS-MEAN                        VALUE '8'.
000514           88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
000515         16  FILLER                PIC X(12).
000516
000517***************    OUTPUT FROM ELPMNT   *************************
000518
000519         16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
000520                                     COMP-3.
000521         16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
000522                                     COMP-3.
000523         16  FILLER                PIC X(12).
000524
000525***************   MISC WORK AREAS    *****************************
000526         16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
000527                                     COMP-3.
000528         16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
000529                                     COMP-3.
000530         16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
000531                                     COMP-3.
000532         16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
000533                                     COMP-3.
000534         16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
000535                                     COMP-3.
000536         16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
000537                                     COMP-3.
000538         16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
000539             88  OPEN-RATE-FILE                   VALUE 'O'.
000540             88  CLOSE-RATE-FILE                  VALUE 'C'.
000541             88  IO-ERROR                         VALUE 'E'.
000542
000543         16  CP-FIRST-PAY-DATE     PIC XX.
000544
000545         16  CP-JOINT-INDICATOR    PIC X.
000546
000547         16  CP-RESERVE-REMAINING-TERM
000548                                   PIC S9(4)V9    VALUE ZERO
000549                                     COMP-3.
000550
000551         16  CP-INSURED-BIRTH-DT   PIC XX.
000552
000553         16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
000554                                     COMP-3.
000555
000556         16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
000557                                     COMP-3.
000558
000559         16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
000560                                     COMP-3.
000561
000562         16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
000563                                     COMP-3.
000564
000565         16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
000566                                     COMP-3.
000567
000568         16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
000569                                     COMP-3.
000570
000571         16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
000572                                     COMP-3.
000573
000574         16  CP-ROA-REFUND         PIC X          VALUE 'N'.
000575             88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
000576
000577         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
000578                                     COMP-3.
000579         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
000580                                     COMP-3.
000581         16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
000582         16  cp-extra-periods      pic 9 value zeros.
000583         16  cp-net-only-state     pic x value spaces.
000584         16  FILLER                PIC X(13).
000585******************************************************************
      *<<((file: ELCCALC))
000395
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
000397 01  DFHCOMMAREA.
000398   05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
000399   05 LSTN-NAME                PIC X(8).
000400   05 LSTN-SUBNAME             PIC X(8).
000401
000402**** client-in-data cannot be more than 36 characters ***
000403   05 CLIENT-IN-DATA           pic x(36).
000404
000405   05 SOCKADDR-IN-PARM.
000406     15 SIN-FAMILY             PIC 9(4) COMP.
000407     15 SIN-PORT               PIC 9(4) COMP.
000408     15 SIN-ADDRESS            PIC 9(8) COMP.
000409     15 SIN-ZERO               PIC X(8).
000410
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'SOCK03' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000411 VCOBOL-DUMMY-PROCEDURE.
000412
000413     display '  BEGIN SOCK03 '
000414     display 'SOCK03:transaction data =', CLIENT-IN-DATA (1:10)
000415     display 'SOCK03:socket number    =', GIVE-TAKE-SOCKET.
000416     IF CLIENT-IN-DATA (1:6) = 'REFUND' OR 'NEWBUS'
000417        MOVE CLIENT-IN-DATA (1:6) TO WS-CALL-TYPE
000418        evaluate true
000419           when client-in-data (8:3) = 'AHL'
000420              MOVE X'06'         TO WS-COMP-CD
000421              MOVE 'AHL'         TO WS-SEND-BUF (5:3)
000422                                    WS-COMP-ID
000423           when CLIENT-IN-DATA (8:3) = 'DCC'
000424              MOVE X'05'         TO WS-COMP-CD
000425              MOVE 'DCC'         TO WS-SEND-BUF (5:3)
000426                                    WS-COMP-ID
000427           when CLIENT-IN-DATA (8:3) = 'VPP'
000428              MOVE X'07'         TO WS-COMP-CD
000429              MOVE 'VPP'         TO WS-SEND-BUF (5:3)
000430                                    WS-COMP-ID
000431           when CLIENT-IN-DATA (8:3) = 'FNL'
000432              MOVE X'08'         TO WS-COMP-CD
000433              MOVE 'FNL'         TO WS-SEND-BUF (5:3)
000434                                    WS-COMP-ID
000435           when other
000436              MOVE X'04'         TO WS-COMP-CD
000437              MOVE 'CID'         TO WS-SEND-BUF (5:3)
000438                                    WS-COMP-ID
000439        end-evaluate
000440        move 'CSO'               to ws-send-buf (1:3)
000441        move +25                 to ws-send-msg-size
000442     END-IF
000443
000444     display 'SOCK03:sequence number  =', ws-seq-num.
000445     display 'SOCK03:send buffer      =', ws-send-buf(1:25).
000446
000447     call "send" using by value GIVE-TAKE-SOCKET,
000448         by reference ws-send-buf,
000449         by value ws-send-msg-size,
000450         by value ws-flags.
000451
000452     if return-code <= zero
000453         display 'SOCK03:send error ',
000454         go to socket-error.
000455
000456     move low-values to ws-recv-buf.
000457     set ws-recv-total to zero.
000458     compute ws-recv-left = ws-recv-msg-size.
000459
000460     display 'SOCK03:About to recv '
000461
000462     call "recv" using by value GIVE-TAKE-SOCKET,
000463         by reference ws-recv-buf
000464         by value ws-recv-msg-size,
000465         by value ws-flags.
000466
000467     if return-code < zero
000468        display 'SOCK03:recv error ',
000469        go to socket-error.
000470
000471     if return-code = zero
000472        display 'SOCK03:client disconnected',
000473        go to socket-error.
000474*
000475     display 'SOCK03:Good recv  '
000476     display 'SOCK03:return code      = ', return-code
000477     display 'SOCK03:receive buffer   = ', ws-recv-buf(1:89)
000478
000479     move +4096                  to ws-send-msg-size
000480*    move +750                   to ws-send-msg-size
000481
000482     move ws-recv-buf (1:89)     to soc-client-in-data
000483
000484     MOVE SPACES                 TO WS-ELCERT-SW
000485     MOVE X'0A'                  TO WS-HEX-0A
000486
000487     if client-account = 'EPIQ000001'
000488        set ws-epiq-request to true
000489        move spaces              to client-account
000490     end-if
000491     move +0 to ws-epiq-max
000492
000493     if client-car = spaces
000494        if client-state = 'KY'
000495           move '8'              to client-car
000496        else
000497           move '9'              to client-car
000498        end-if
000499     end-if
000500     if client-grp = spaces
000501        move '000000'            to client-grp
000502     end-if
000503
000504     EVALUATE TRUE
000505        WHEN (CLIENT-CAR NOT = SPACES)
000506           AND (CLIENT-STATE NOT = SPACES)
000507           AND (CLIENT-ACCOUNT NOT = SPACES)
000508           AND (CLIENT-EFF-DT NOT = SPACES)
000509           AND (CLIENT-CERT-NO NOT = SPACES)
000510           SET WS-ELCERT-FULL    TO TRUE
000511*          DISPLAY ' SETTING FULL '
000512        WHEN (CLIENT-CERT-NO NOT = SPACES)
000513           SET WS-ELCERT-CERT-NO TO TRUE
000514*          DISPLAY ' SETTING CERT NO '
000515*       WHEN (CLIENT-EFF-DT NOT = SPACES)
000516*          AND (CLIENT-LAST-NAME NOT = SPACES)
000517        WHEN (CLIENT-ACCOUNT NOT = SPACES)
000518           AND (CLIENT-LAST-NAME NOT = SPACES)
000519           SET WS-ELCERT-ACT-NAME TO TRUE
000520        WHEN CLIENT-LAST-NAME NOT = SPACES
000521           SET WS-ELCERT-NAME    TO TRUE
000522*          DISPLAY ' SETTING NAME '
000523        when (client-vin not = spaces)
000524           and (client-state not = spaces)
000525           and (client-account not = spaces)
000526           set ws-elcert-vin to true
000527*          display ' SETTING VIN '
000528        WHEN OTHER
000529           MOVE ' BAD SELECTION OPTION ' TO WS-COMMENT
000530           MOVE 'X'              TO WS-LF-STATUS WS-AH-STATUS
000531           MOVE ZEROS            TO WS-RESP
000532           PERFORM 0030-BUILD-BUFFER
000533                                 THRU 0030-EXIT
000534           PERFORM 0020-SEND-BUFFER
000535                                 THRU 0020-EXIT
000536           PERFORM 0025-CLOSE-SOCKET
000537                                 THRU 0025-EXIT
000538           GO TO 0010-RETURN
000539     END-EVALUATE
000540
000541     IF CLIENT-CAN-REASON = 'R'
000542        MOVE 'R'                 TO WS-CANCEL-REASON
000543     ELSE
000544        MOVE ' '                 TO WS-CANCEL-REASON
000545     END-IF
000546     IF WS-ELCERT-KEY-SW NOT = SPACES
000547        PERFORM 0050-BEGIN-REFUND-PROCESS
000548                                 THRU 0050-EXIT
000549     END-IF
000550
000551     PERFORM 0025-CLOSE-SOCKET   THRU 0025-EXIT
000552
000553     .
000554 0010-RETURN.
000555     
      * exec cics return end-exec.
      *    MOVE '.(                    ''   #00004500' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034353030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000556     
      * goback.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK03' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback.
000557
000558*    PERFORM 0030-BUILD-BUFFER   THRU 0030-EXIT
000559
000560     .
000561 0020-SEND-BUFFER.
000562
000563*    display 'SOCK03:About to send      '
000564*    display 'SOCK03:sequence number  =', ws-seq-num.
000565     display 'SOCK03:send buffer      =', ws-send-buf(1:500).
000566
000567     call "send" using by value GIVE-TAKE-SOCKET,
000568         by reference ws-send-buf,
000569         by value ws-send-msg-size,
000570         by value ws-flags.
000571
000572     if return-code <= zero
000573         display 'SOCK03:send error ' return-code,
000574         go to socket-error.
000575 0020-EXIT.
000576     EXIT.
000577
000578 0025-CLOSE-SOCKET.
000579
000580*    display 'SOCK03:closing socket'.
000581*    call "close" using by value GIVE-TAKE-SOCKET .
000582*    display 'SOCK03:done'.
000583
000584     .
000585 0025-EXIT.
000586     EXIT.
000587
000588*
000589* set up the receive buffer
000590*
000591     move low-values to ws-recv-buf.
000592     set ws-recv-total to zero.
000593     compute ws-recv-left = ws-recv-msg-size.
000594*
000595* receive data
000596*
000597 recv-1.
000598     call "recv" using by value GIVE-TAKE-SOCKET,
000599         by reference ws-recv-buf(1+ws-recv-total:ws-recv-left),
000600         by value ws-recv-left,
000601         by value ws-flags.
000602*
000603* test what was received and decide what we should do
000604*
000605     if return-code < zero
000606        display 'SOCK03:recv error ',
000607        go to socket-error.
000608
000609     if return-code = zero
000610        display 'SOCK03:client disconnected',
000611        go to socket-error.
000612*
000613* have we received all the data yet?
000614*
000615     compute ws-recv-total = ws-recv-total + return-code.
000616     compute ws-recv-left = ws-recv-msg-size - ws-recv-total.
000617*
000618* not yet
000619*
000620     if ws-recv-left > 0 go to recv-1.
000621*
000622* received all the data
000623*
000624*    display 'SOCK03:receive buffer   =', ws-recv-buf(1:50).
000625*
000626* make sure what we received was what we sent
000627*
000628*    if ws-recv-buf <> ws-send-buf
000629*        display "SOCK03:data doesn't match",
000630*        go to socket-error.
000631*
000632* end of pass
000633*
000634*loop-end.
000635*    if ws-seq-num = 5 go to socket-error.
000636*    go to loop-1.
000637*
000638* program end
000639*
000640 socket-error.
000641     if ws-seq-num <> 0
000642         display "SOCK03:did not complete".
000643*
000644* flush the send buffer and deallocate
000645*
000646*    display 'SOCK03:closing socket'.
000647*    call "close" using by value GIVE-TAKE-SOCKET .
000648*
000649* finised return to cics
000650*
000651 socket-fin.
000652*    display 'SOCK03:closing socket'.
000653*    call "close" using by value GIVE-TAKE-SOCKET .
000654*    display 'SOCK03:done'.
000655     
      * exec cics return end-exec.
      *    MOVE '.(                    ''   #00004600' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034363030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000656     
      * goback.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK03' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback.
000657
000658 0030-BUILD-BUFFER.
000659
000660     MOVE SPACES                 TO WS-SEND-BUF
000661     IF WS-CALL-TYPE = 'NEWBUS'
000662        STRING
000663           WS-COMMENT            ';'
000664           WS-RESP               ';'
000665           WS-CARRIER            ';'
000666           WS-GROUPING           ';'
000667           WS-STATE              ';'
000668           WS-ACCOUNT            ';'
000669           WS-CERT-EFF-DATE      ';'
000670           WS-CERT-NO            ';'
000671           WS-LAST-NAME          ';'
000672           WS-FIRST-NAME         ';'
000673           WS-MID-INIT           ';'
000674           WS-LAST-JNAME         ';'
000675           WS-FIRST-JNAME        ';'
000676           WS-MID-JINIT          ';'
000677           WS-POST-CARD-SW       ';'
000678           ws-lf-coverage-type   ';'
000679           WS-LF-STATUS          ';'
000680           WS-LF-BEN-CODE        ';'
000681           WS-LF-BEN-CODE-DESC   ';'
000682           WS-LF-TERM            ';'
000683           WS-LF-REM-TERM        ';'
000684           WS-LF-PREM            ';'
000685           WS-LF-REFUND          ';'
000686           WS-LF-METHOD          ';'
000687           WS-LF-ORIG-BENEFIT    ';'
000688           WS-LF-REM-BEN         ';'
000689           WS-LF-EXPIRE-DATE     ';'
000690           WS-LF-COMM            ';'
000691           WS-LF-UEC             ';'
000692           WS-AH-COVERAGE-TYPE   ';'
000693           WS-AH-STATUS          ';'
000694           WS-AH-BEN-CODE        ';'
000695           WS-AH-BEN-CODE-DESC   ';'
000696           WS-AH-TERM            ';'
000697           WS-AH-REM-TERM        ';'
000698           WS-AH-PREM            ';'
000699           WS-AH-REFUND          ';'
000700           WS-AH-METHOD          ';'
000701           WS-AH-ORIG-BENEFIT    ';'
000702           WS-AH-REM-BEN         ';'
000703           WS-AH-EXPIRE-DATE     ';'
000704           WS-AH-COMM            ';'
000705           WS-AH-UEC             ';'
000706           WS-ACCOUNT-NAME       ';'
000707           WS-REPORT-CODE-1      ';'
000708           WS-REPORT-CODE-2      ';'
000709           WS-REPORT-CODE-3      ';'
000710           WS-CLM-PAID-THRU-DT   ';'
000711           WS-LF-CANCEL-DATE     ';'
000712           WS-AH-CANCEL-DATE     ';'
000713           WS-VIN                ';'
000714           WS-CRED-BENE-NAME     ';'
000715           WS-HEX-0A
000716            DELIMITED BY '  ' INTO WS-SEND-BUF
000717        END-STRING
000718     ELSE
000719        STRING
000720           WS-COMMENT            ';'  *> 0
000721           WS-RESP               ';'
000722           WS-CARRIER            ';'
000723           WS-GROUPING           ';'
000724           WS-STATE              ';'
000725           WS-ACCOUNT            ';'
000726           WS-CERT-EFF-DATE      ';'
000727           WS-CERT-NO            ';'
000728           WS-LAST-NAME          ';'
000729           WS-FIRST-NAME         ';'
000730           WS-MID-INIT           ';'
000731           WS-LAST-JNAME         ';'
000732           WS-FIRST-JNAME        ';'
000733           WS-MID-JINIT          ';'
000734           WS-POST-CARD-SW       ';'
000735           WS-LF-STATUS          ';'
000736           WS-LF-BEN-CODE        ';'  *> 16
000737           WS-LF-BEN-CODE-DESC   ';'
000738           WS-LF-TERM            ';'
000739           WS-LF-REM-TERM        ';'
000740           WS-LF-PREM            ';'
000741           WS-LF-REFUND          ';'
000742           WS-LF-METHOD          ';'
000743           WS-LF-ORIG-BENEFIT    ';'
000744           WS-LF-REM-BEN         ';'
000745           WS-LF-EXPIRE-DATE     ';'
000746           WS-LF-COMM            ';'
000747           WS-LF-UEC             ';'
000748           WS-AH-STATUS          ';'
000749           WS-AH-BEN-CODE        ';'   *> 29
000750           WS-AH-BEN-CODE-DESC   ';'
000751           WS-AH-TERM            ';'
000752           WS-AH-REM-TERM        ';'
000753           WS-AH-PREM            ';'   *> 32
000754           WS-AH-REFUND          ';'   *> 33
000755           WS-AH-METHOD          ';'
000756           WS-AH-ORIG-BENEFIT    ';'   *> 36
000757           WS-AH-REM-BEN         ';'
000758           WS-AH-EXPIRE-DATE     ';'
000759           WS-AH-COMM            ';'
000760           WS-AH-UEC             ';'
000761           WS-ACCOUNT-NAME       ';'
000762           WS-REPORT-CODE-1      ';'
000763           WS-REPORT-CODE-2      ';'
000764           WS-REPORT-CODE-3      ';'
000765           WS-CLM-PAID-THRU-DT   ';'
000766           WS-LF-CANCEL-DATE     ';'
000767           WS-AH-CANCEL-DATE     ';'
000768           WS-VIN                ';'
000769           WS-CRED-BENE-NAME     ';'
000770           'E'                   ';'
000771            DELIMITED BY '  ' INTO WS-SEND-BUF
000772        END-STRING
000773     END-IF
000774
000775     .
000776 0030-EXIT.
000777     EXIT.
000778
000779 0050-BEGIN-REFUND-PROCESS.
000780
000781     IF WS-ELCERT-ACT-NAME
000782        PERFORM 0060-FIND-FULL-KEY
000783                                 THRU 0060-EXIT
000784        MOVE AM-CARRIER          TO CLIENT-CAR
000785        MOVE AM-GROUPING         TO CLIENT-GRP
000786        MOVE AM-STATE            TO CLIENT-STATE
000787     END-IF
000788
000789     PERFORM 0100-START-ELCERT   THRU 0100-EXIT
000790
000791     IF NOT RESP-NORMAL
000792        MOVE ' CERT NOT FOUND - START ' TO WS-COMMENT
000793        MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
000794        MOVE WS-RESPONSE         TO WS-RESP
000795        PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
000796        PERFORM 0020-SEND-BUFFER
000797                                 THRU 0020-EXIT
000798        PERFORM 0025-CLOSE-SOCKET
000799                                 THRU 0025-EXIT
000800        GO TO 0010-RETURN
000801     END-IF
000802
000803     PERFORM 0150-READ-NEXT-ELCERT
000804                                 THRU 0150-EXIT
000805
000806*    display ' came back from 0150 ' ws-response
000807     IF (NOT RESP-NORMAL AND NOT RESP-DUPKEY)
000808        DISPLAY ' CERT NOT FOUND '
000809        MOVE ' CERT NOT FOUND - READ ' TO WS-COMMENT
000810        MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
000811        MOVE WS-RESPONSE         TO WS-RESP
000812        PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
000813        PERFORM 0020-SEND-BUFFER
000814                                 THRU 0020-EXIT
000815        PERFORM 0025-CLOSE-SOCKET
000816                                 THRU 0025-EXIT
000817        GO TO 0010-RETURN
000818     END-IF
000819
000820     PERFORM 0200-BEGIN-REFUND-PROCESS
000821                                 THRU 0200-EXIT UNTIL
000822        ELCERT-FINISHED
000823
000824     .
000825 0050-EXIT.
000826     EXIT.
000827
000828 0060-FIND-FULL-KEY.
000829
000830     MOVE ' ' TO WS-STOP-SW
000831     MOVE CLIENT-ACCOUNT         TO WS-AM3-ACCOUNT
000832     MOVE LOW-VALUES             TO WS-AM3-EXP-DT
000833     
      * EXEC CICS STARTBR
000834*       DATASET  ('ERACCT3')
000835*       RIDFLD   (WS-AM3-KEY)
000836*       GTEQ
000837*       RESP (WS-RESPONSE)
000838*    END-EXEC
           MOVE 'ERACCT3' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00004778' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303034373738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-AM3-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000839
000840     IF RESP-NORMAL
000841        PERFORM UNTIL TOLD-TO-STOP
000842           
      * EXEC CICS READNEXT
000843*             DATASET  ('ERACCT3')
000844*             INTO     (ACCOUNT-MASTER)
000845*             RIDFLD   (WS-AM3-KEY)
000846*          END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 'ERACCT3' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00004787' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303034373837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 WS-AM3-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000847           EVALUATE TRUE
000848              WHEN (RESP-NORMAL)
000849                 AND (AM-ACCOUNT = CLIENT-ACCOUNT)
000850                 AND (AM-COMPANY-CD = WS-COMP-CD)
000851                 SET TOLD-TO-STOP TO TRUE
000852              WHEN (AM-ACCOUNT = CLIENT-ACCOUNT)
000853                 AND (AM-COMPANY-CD NOT = WS-COMP-CD)
000854                 CONTINUE
000855              WHEN AM-ACCOUNT NOT = CLIENT-ACCOUNT
000856                 MOVE 'INVALID ACCOUNT NO ' TO WS-COMMENT
000857                 MOVE 'X'        TO WS-LF-STATUS WS-AH-STATUS
000858                 MOVE ZEROS      TO WS-RESP
000859                 PERFORM 0030-BUILD-BUFFER
000860                                 THRU 0030-EXIT
000861                 PERFORM 0020-SEND-BUFFER
000862                                 THRU 0020-EXIT
000863                 PERFORM 0025-CLOSE-SOCKET
000864                                 THRU 0025-EXIT
000865                 GO TO 0010-RETURN
000866           END-EVALUATE
000867        END-PERFORM
000868        
      * EXEC CICS ENDBR
000869*          DATASET   ('ERACCT3')
000870*       END-EXEC
           MOVE 'ERACCT3' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004813' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303034383133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000871     ELSE
000872        MOVE 'INVALID ACCOUNT NO ' TO WS-COMMENT
000873        MOVE 'X'        TO WS-LF-STATUS WS-AH-STATUS
000874        MOVE ZEROS      TO WS-RESP
000875        PERFORM 0030-BUILD-BUFFER
000876                                 THRU 0030-EXIT
000877        PERFORM 0020-SEND-BUFFER
000878                                 THRU 0020-EXIT
000879        PERFORM 0025-CLOSE-SOCKET
000880                                 THRU 0025-EXIT
000881        GO TO 0010-RETURN
000882     END-IF
000883
000884     .
000885 0060-EXIT.
000886     EXIT.
000887
000888 0100-START-ELCERT.
000889
000890     MOVE SPACES                 TO WS-SEND-BUF
000891     INITIALIZE WS-STRING-DATA
000892     MOVE LOW-VALUES             TO WS-CM-KEY
000893                                    WS-CM-KEY-A1
000894                                    WS-CM-KEY-A4
000895     MOVE WS-COMP-CD             TO WS-CM-COMPANY-CD
000896                                    WS-CM-COMPANY-CD-A1
000897                                    WS-CM-COMPANY-CD-A4
000898
000899     MOVE CLIENT-VAL-DT          TO DC-GREG-DATE-CYMD
000900     MOVE 'L'                    TO DC-OPTION-CODE
000901     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
000902     IF NO-CONVERSION-ERROR
000903        MOVE DC-BIN-DATE-1       TO WS-BIN-VAL-DT
000904     ELSE
000905        MOVE 'BAD VAL DT CONVERT ' TO WS-COMMENT
000906        MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
000907        PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
000908        PERFORM 0020-SEND-BUFFER
000909                                 THRU 0020-EXIT
000910        PERFORM 0025-CLOSE-SOCKET
000911                                 THRU 0025-EXIT
000912        GO TO 0010-RETURN
000913     END-IF
000914
000915     IF CLIENT-EFF-DT NOT = SPACES AND ZEROS
000916        MOVE CLIENT-EFF-DT       TO DC-GREG-DATE-CYMD
000917        MOVE 'L'                 TO DC-OPTION-CODE
000918        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000919        IF NO-CONVERSION-ERROR
000920           MOVE DC-BIN-DATE-1    TO WS-BIN-EFF-DT
000921        ELSE
000922           MOVE 'BAD EFF DT CONVERT '
000923                                 TO WS-COMMENT
000924           MOVE 'X'              TO WS-LF-STATUS WS-AH-STATUS
000925           PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
000926           PERFORM 0020-SEND-BUFFER
000927                                 THRU 0020-EXIT
000928           PERFORM 0025-CLOSE-SOCKET
000929                                 THRU 0025-EXIT
000930           GO TO 0010-RETURN
000931        END-IF
000932     END-IF
000933
000934     PERFORM VARYING F1 FROM +1 BY +1 UNTIL
000935        (F1 > +10)
000936        OR (CLIENT-FIRST-NAME (F1:1) = SPACES OR LOW-VALUES)
000937     END-PERFORM
000938     IF F1 > +1
000939        SUBTRACT +1 FROM F1
000940     END-IF
000941
000942     PERFORM VARYING L1 FROM +1 BY +1 UNTIL
000943        (L1 > +15)
000944        OR (CLIENT-LAST-NAME (L1:1) = SPACES OR LOW-VALUES)
000945     END-PERFORM
000946     IF L1 > +1
000947        SUBTRACT +1 FROM L1
000948     END-IF
000949
000950     EVALUATE TRUE
000951        WHEN WS-ELCERT-FULL
000952           MOVE CLIENT-CAR          TO WS-CM-CARRIER
000953           MOVE CLIENT-GRP          TO WS-CM-GROUP
000954           MOVE CLIENT-STATE        TO WS-CM-STATE
000955           MOVE CLIENT-ACCOUNT      TO WS-CM-ACCOUNT
000956           MOVE WS-BIN-EFF-DT       TO WS-CM-EFF-DT
000957           MOVE CLIENT-CERT-NO      TO WS-CM-CERT-NO
000958           
      * EXEC CICS READ
000959*             INTO    (CERTIFICATE-MASTER)
000960*             DATASET ('ELCERT')
000961*             RIDFLD  (WS-CM-KEY)
000962*             RESP    (WS-RESPONSE)
000963*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00004903' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303034393033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 WS-CM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000964        WHEN WS-ELCERT-ACT-NAME
000965           MOVE CLIENT-CAR          TO WS-CM-CARRIER
000966           MOVE CLIENT-GRP          TO WS-CM-GROUP
000967           MOVE CLIENT-STATE        TO WS-CM-STATE
000968           MOVE CLIENT-ACCOUNT      TO WS-CM-ACCOUNT
000969           MOVE LOW-VALUES          TO WS-CM-EFF-DT
000970           MOVE ZEROS               TO WS-CM-CERT-NO
000971           
      * EXEC CICS STARTBR
000972*             DATASET ('ELCERT')
000973*             RIDFLD  (WS-CM-KEY)
000974*             RESP    (WS-RESPONSE)
000975*          END-EXEC
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00004916' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303034393136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-CM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000976        WHEN WS-ELCERT-NAME
000977           MOVE CLIENT-LAST-NAME TO WS-CM-LAST-NAME
000978           MOVE SPACES           TO WS-CM-INITIALS
000979           COMPUTE WS-LENGTH = L1 + +1
000980           
      * EXEC CICS STARTBR
000981*             DATASET   ('ELCERT2')
000982*             RIDFLD    (WS-CM-KEY-A1)
000983*             GENERIC
000984*             KEYLENGTH (WS-LENGTH)
000985*             GTEQ
000986*             RESP      (WS-RESPONSE)
000987*          END-EXEC
           MOVE 'ELCERT2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    G          &  N#00004925' TO DFHEIV0
           MOVE X'262C2020204B472020202047' &
                X'202020202020202020202620' &
                X'204E233030303034393235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-CM-KEY-A1, 
                 WS-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000988*          DISPLAY ' STARTBR RESP ' WS-RESPONSE
000989*          DISPLAY ' KEY ' WS-CM-KEY-A1
000990        WHEN WS-ELCERT-CERT-NO
000991* DO NOT INCLUDE THE SUFFIX AS PART OF THE KEY
000992           MOVE +11 TO WS-LENGTH
000993           MOVE CLIENT-CERT-NO    TO WS-CM-CERT-NO-A4
000994           
      * EXEC CICS STARTBR
000995*             DATASET   ('ELCERT5')
000996*             RIDFLD    (WS-CM-KEY-A4)
000997*             GENERIC
000998*             KEYLENGTH (WS-LENGTH)
000999*             GTEQ
001000*             RESP      (WS-RESPONSE)
001001*          END-EXEC
           MOVE 'ELCERT5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    G          &  N#00004939' TO DFHEIV0
           MOVE X'262C2020204B472020202047' &
                X'202020202020202020202620' &
                X'204E233030303034393339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-CM-KEY-A4, 
                 WS-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001002        when ws-elcert-vin
001003           move low-values       to ws-cs-key
001004           move ws-comp-cd       to ws-cs-company-cd
001005           move client-car       to ws-cs-carrier
001006           move client-grp       to ws-cs-group
001007           move client-state     to ws-cs-state
001008           move client-account   to ws-cs-account
001009           
      * exec cics startbr
001010*             dataset   ('ELCRTT')
001011*             RIDFLD    (WS-CS-KEY)
001012*             GTEQ
001013*             RESP      (WS-RESPONSE)
001014*          END-EXEC
           MOVE 'ELCRTT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00004954' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303034393534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-CS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001015     END-EVALUATE
001016
001017     .
001018 0100-EXIT.
001019     EXIT.
001020
001021 0150-READ-NEXT-ELCERT.
001022
001023     EVALUATE TRUE
001024        WHEN WS-ELCERT-ACT-NAME
001025*          DISPLAY ' ABOUT TO READ NEXT - ACT NAME'
001026           
      * EXEC CICS READNEXT
001027*             DATASET   ('ELCERT')
001028*             RIDFLD    (WS-CM-KEY)
001029*             INTO      (CERTIFICATE-MASTER)
001030*             RESP      (WS-RESPONSE)
001031*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV12
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00004971' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303034393731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV12, 
                 WS-CM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001032        WHEN WS-ELCERT-NAME
001033*          DISPLAY ' ABOUT TO READ NEXT - NAME'
001034           
      * EXEC CICS READNEXT
001035*             DATASET   ('ELCERT2')
001036*             RIDFLD    (WS-CM-KEY-A1)
001037*             INTO      (CERTIFICATE-MASTER)
001038*             RESP      (WS-RESPONSE)
001039*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV12
           MOVE 'ELCERT2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00004979' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303034393739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV12, 
                 WS-CM-KEY-A1, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001040        WHEN WS-ELCERT-CERT-NO
001041           
      * EXEC CICS READNEXT
001042*             DATASET   ('ELCERT5')
001043*             RIDFLD    (WS-CM-KEY-A4)
001044*             INTO      (CERTIFICATE-MASTER)
001045*             RESP      (WS-RESPONSE)
001046*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV12
           MOVE 'ELCERT5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00004986' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303034393836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV12, 
                 WS-CM-KEY-A4, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001047        WHEN WS-ELCERT-VIN
001048           
      * EXEC CICS READNEXT
001049*             DATASET   ('ELCRTT')
001050*             RIDFLD    (WS-CS-KEY)
001051*             INTO      (CERTIFICATE-TRAILERS)
001052*             RESP      (WS-RESPONSE)
001053*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV12
           MOVE 'ELCRTT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00004993' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303034393933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV12, 
                 WS-CS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001054     END-EVALUATE
001055
001056     .
001057 0150-EXIT.
001058     EXIT.
001059
001060 0200-BEGIN-REFUND-PROCESS.
001061
001062     MOVE SPACES                 TO WS-BUFF-SENT-SW
001063                                    WS-ERACCT-SW
001064     MOVE ZEROS                  TO WS-NCB-DIFF-MONTHS
001065                                    WS-NCB-DIFF-ODD-DAYS
001066
001067     INITIALIZE WS-STRING-DATA
001068
001069     IF RESP-NORMAL OR RESP-DUPKEY
001070        EVALUATE TRUE
001071           WHEN WS-ELCERT-ACT-NAME
001072              EVALUATE TRUE
001073                 WHEN (CM-COMPANY-CD NOT = WS-COMP-CD)
001074                    OR (CM-CARRIER NOT = CLIENT-CAR)
001075                    OR (CM-GROUPING NOT = CLIENT-GRP)
001076                    OR (CM-STATE NOT = CLIENT-STATE)
001077                    OR (CM-ACCOUNT NOT = CLIENT-ACCOUNT)
001078                    SET ELCERT-FINISHED TO TRUE
001079                 WHEN (CM-INSURED-LAST-NAME (1:L1) NOT =
001080                    CLIENT-LAST-NAME (1:L1))
001081                    GO TO 0200-CONTINUE
001082                 WHEN ((CLIENT-EFF-DT NOT = SPACES)
001083                    AND (CM-CERT-EFF-DT NOT = WS-BIN-EFF-DT))
001084                    GO TO 0200-CONTINUE
001085                 WHEN ((CLIENT-FIRST-NAME NOT = SPACES)
001086                    AND (CM-INSURED-FIRST-NAME (1:F1) NOT =
001087                     CLIENT-FIRST-NAME (1:F1)))
001088                    GO TO 0200-CONTINUE
001089                 WHEN ((CLIENT-ACCOUNT NOT = SPACES)
001090                    AND (CM-ACCOUNT NOT = CLIENT-ACCOUNT))
001091                    GO TO 0200-CONTINUE
001092              END-EVALUATE
001093
001094           WHEN WS-ELCERT-CERT-NO
001095              IF (CM-COMPANY-CD NOT = WS-COMP-CD)
001096                 OR (CM-CERT-PRIME NOT = CLIENT-CERT-NO (1:10))
001097                 SET ELCERT-FINISHED TO TRUE
001098              ELSE
001099                 IF ((CLIENT-EFF-DT NOT = SPACES)
001100                     AND (CM-CERT-EFF-DT NOT = WS-BIN-EFF-DT))
001101                              OR
001102                    ((CLIENT-LAST-NAME NOT = SPACES)
001103                     AND (CM-INSURED-LAST-NAME (1:L1) NOT =
001104                        CLIENT-LAST-NAME (1:L1)))
001105                              OR
001106                    ((CLIENT-FIRST-NAME NOT = SPACES)
001107                     AND (CM-INSURED-FIRST-NAME (1:F1) NOT =
001108                        CLIENT-FIRST-NAME (1:F1)))
001109                              OR
001110                    ((CLIENT-ACCOUNT NOT = SPACES)
001111                     AND (CM-ACCOUNT NOT = CLIENT-ACCOUNT))
001112                     GO TO 0200-CONTINUE
001113                 END-IF
001114              END-IF
001115           WHEN WS-ELCERT-NAME
001116              IF (CM-COMPANY-CD NOT = WS-COMP-CD)
001117                 OR (CM-INSURED-LAST-NAME (1:L1) NOT =
001118                    CLIENT-LAST-NAME (1:L1))
001119                  SET ELCERT-FINISHED TO TRUE
001120              ELSE
001121                 IF ((CLIENT-EFF-DT NOT = SPACES)
001122                    AND (CM-CERT-EFF-DT NOT = WS-BIN-EFF-DT))
001123                           OR
001124                 ((CLIENT-FIRST-NAME NOT = SPACES)
001125                  AND (CM-INSURED-FIRST-NAME (1:F1) NOT =
001126                     CLIENT-FIRST-NAME (1:F1)))
001127                           OR
001128                 ((CLIENT-ACCOUNT NOT = SPACES)
001129                  AND (CM-ACCOUNT NOT = CLIENT-ACCOUNT))
001130                  GO TO 0200-CONTINUE
001131                 END-IF
001132              END-IF
001133           WHEN WS-ELCERT-VIN
001134              evaluate true
001135                 when (CS-COMPANY-CD NOT = WS-COMP-CD)
001136                    OR (CS-STATE NOT = CLIENT-STATE)
001137                    OR (CS-ACCOUNT NOT = CLIENT-ACCOUNT)
001138                    SET ELCERT-FINISHED TO TRUE
001139                 when (cs-trailer-type  = 'C')
001140                    and (cs-vin-number = client-vin)
001141                    continue
001142                 when (cs-trailer-type  = 'C')
001143                    and (cs-vin-number(10:8) = client-vin(1:8))
001144                    continue
001145                 when other
001146                    go to 0200-continue
001147              end-evaluate
001148           WHEN OTHER
001149              SET ELCERT-FINISHED   TO TRUE
001150        END-EVALUATE
001151     ELSE
001152        SET ELCERT-FINISHED TO TRUE
001153     END-IF
001154
001155     if ws-elcert-vin
001156        move cs-control-primary(1:33)
001157                                 to ws-cm-key
001158        
      * EXEC CICS READ
001159*          INTO    (CERTIFICATE-MASTER)
001160*          DATASET ('ELCERT')
001161*          RIDFLD  (WS-CM-KEY)
001162*          RESP    (WS-RESPONSE)
001163*       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005103' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035313033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 WS-CM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001164        if not resp-normal
001165           set elcert-finished to true
001166        end-if
001167     end-if
001168
001169*       IF WS-ELCERT-CERT-NO
001170*          IF (CM-COMPANY-CD NOT = WS-COMP-CD)
001171*             OR (CM-CERT-NO NOT = CLIENT-CERT-NO)
001172*                          OR
001173*                ((CLIENT-EFF-DT NOT = SPACES)
001174*                 AND (CM-CERT-EFF-DT NOT = WS-BIN-EFF-DT))
001175*                          OR
001176*                ((CLIENT-LAST-NAME NOT = SPACES)
001177*                 AND (CM-INSURED-LAST-NAME NOT =
001178*                    CLIENT-LAST-NAME))
001179*                 SET ELCERT-FINISHED TO TRUE
001180*          END-IF
001181
001182
001183
001184
001185
001186
001187*          IF WS-ELCERT-NAME
001188*             IF (CM-COMPANY-CD NOT = WS-COMP-CD)
001189*                OR (CM-INSURED-LAST-NAME NOT =
001190*                   CLIENT-LAST-NAME)
001191*                            OR
001192*                ((CLIENT-EFF-DT NOT = SPACES)
001193*                 AND (CM-CERT-EFF-DT NOT = WS-BIN-EFF-DT))
001194*                 SET ELCERT-FINISHED TO TRUE
001195*             END-IF
001196*          ELSE
001197*             SET ELCERT-FINISHED   TO TRUE
001198*          END-IF
001199
001200     IF NOT WS-ELCERT-FULL
001201        IF ELCERT-FINISHED
001202           GO TO 0200-EXIT
001203        END-IF
001204     END-IF
001205
001206     IF CM-ENTRY-STATUS = 'M' OR '9' OR 'D' OR 'V' OR 'U'
001207        MOVE 'CERT NOT ACTIVE '  TO WS-COMMENT
001208        MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
001209        PERFORM 0030-BUILD-BUFFER
001210                                 THRU 0030-EXIT
001211        PERFORM 0020-SEND-BUFFER
001212                                 THRU 0020-EXIT
001213        GO TO 0200-CONTINUE
001214     END-IF
001215
001216     if (ws-epiq-request)
001217        and (not EPIQ-CLASS)
001218        go to 0200-continue
001219     end-if
001220
001221     if ws-epiq-request
001222        add +1 to ws-epiq-max
001223        if ws-epiq-max > +100
001224           go to 0200-continue
001225        end-if
001226     end-if
001227
001228     PERFORM 0300-GET-ELCNTL-RECORDS
001229                                 THRU 0300-EXIT
001230
001231     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
001232     MOVE WS-BIN-VAL-DT          TO DC-BIN-DATE-2
001233     MOVE +0                     TO DC-ELAPSED-MONTHS
001234                                    DC-ELAPSED-DAYS
001235     MOVE '1'                    TO DC-OPTION-CODE
001236     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
001237     IF NO-CONVERSION-ERROR
001238        MOVE DC-ELAPSED-MONTHS   TO WS-NCB-DIFF-MONTHS
001239        MOVE DC-ODD-DAYS-OVER    TO WS-NCB-DIFF-ODD-DAYS
001240     END-IF
001241
001242     PERFORM 0400-PROCESS-LIFE   THRU 0400-EXIT
001243     PERFORM 0500-PROCESS-AH     THRU 0500-EXIT
001244
001245     perform 0610-get-ermail     thru 0610-exit
001246     perform 0620-get-elcrtt     thru 0620-exit
001247
001248     PERFORM 0030-BUILD-BUFFER   THRU 0030-EXIT
001249     IF NOT ALREADY-SENT
001250        PERFORM 0020-SEND-BUFFER THRU 0020-EXIT
001251     END-IF
001252
001253     .
001254 0200-CONTINUE.
001255
001256     PERFORM 0150-READ-NEXT-ELCERT
001257                                 THRU 0150-EXIT
001258
001259     .
001260 0200-EXIT.
001261     EXIT.
001262
001263 0300-GET-ELCNTL-RECORDS.
001264
001265     PERFORM 0310-GET-COMPANY    THRU 0310-EXIT
001266     IF NOT RESP-NORMAL
001267        MOVE ' COMPANY RECORD NOT FOUND '
001268                                 TO WS-COMMENT
001269        MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
001270        PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
001271        PERFORM 0020-SEND-BUFFER
001272                                 THRU 0020-EXIT
001273        PERFORM 0025-CLOSE-SOCKET
001274                                 THRU 0025-EXIT
001275        GO TO 0010-RETURN
001276     END-IF
001277
001278     IF CM-LF-BENEFIT-CD NOT = '  ' AND '00'
001279        PERFORM 0320-GET-LIFE-RECORD
001280                                 THRU 0320-EXIT
001281        IF NOT RESP-NORMAL
001282           MOVE ' LIFE BENEFIT RECORD NOT FOUND '
001283                                 TO WS-COMMENT
001284           MOVE 'X'              TO WS-LF-STATUS WS-AH-STATUS
001285           PERFORM 0030-BUILD-BUFFER
001286                                 THRU 0030-EXIT
001287           PERFORM 0020-SEND-BUFFER
001288                                 THRU 0020-EXIT
001289           PERFORM 0025-CLOSE-SOCKET
001290                                 THRU 0025-EXIT
001291           GO TO 0010-RETURN
001292        END-IF
001293     END-IF
001294
001295     IF CM-AH-BENEFIT-CD NOT = '  ' AND '00'
001296        PERFORM 0330-GET-AH-RECORD
001297                                 THRU 0330-EXIT
001298        IF NOT RESP-NORMAL
001299           MOVE ' AH BENEFIT RECORD NOT FOUND '
001300                                 TO WS-COMMENT
001301           MOVE 'X'              TO WS-LF-STATUS WS-AH-STATUS
001302           PERFORM 0030-BUILD-BUFFER
001303                                 THRU 0030-EXIT
001304           PERFORM 0020-SEND-BUFFER
001305                                 THRU 0020-EXIT
001306           PERFORM 0025-CLOSE-SOCKET
001307                                 THRU 0025-EXIT
001308           GO TO 0010-RETURN
001309        END-IF
001310     END-IF
001311
001312     PERFORM 0340-GET-STATE-RECORD
001313                                 THRU 0340-EXIT
001314     IF NOT RESP-NORMAL
001315        MOVE ' STATE RECORD NOT FOUND '
001316                                 TO WS-COMMENT
001317        MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
001318        PERFORM 0030-BUILD-BUFFER
001319                                 THRU 0030-EXIT
001320        PERFORM 0020-SEND-BUFFER
001321                                 THRU 0020-EXIT
001322        PERFORM 0025-CLOSE-SOCKET
001323                                 THRU 0025-EXIT
001324        GO TO 0010-RETURN
001325     END-IF
001326
001327     PERFORM 0350-GET-ERACCT     THRU 0350-EXIT
001328     IF NOT ERACCT-FOUND
001329        MOVE ' ACCOUNT NOT FOUND '
001330                                 TO WS-COMMENT
001331        MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
001332        PERFORM 0030-BUILD-BUFFER
001333                                 THRU 0030-EXIT
001334        PERFORM 0020-SEND-BUFFER
001335                                 THRU 0020-EXIT
001336        PERFORM 0025-CLOSE-SOCKET
001337                                 THRU 0025-EXIT
001338        GO TO 0010-RETURN
001339     ELSE
001340        MOVE AM-NAME             TO WS-ACCOUNT-NAME
001341        MOVE AM-REPORT-CODE-1    TO WS-REPORT-CODE-1
001342        MOVE AM-REPORT-CODE-2    TO WS-REPORT-CODE-2
001343        MOVE AM-REPORT-CODE-3    TO WS-REPORT-CODE-3
001344     END-IF
001345
001346     .
001347 0300-EXIT.
001348     EXIT.
001349
001350 0310-GET-COMPANY.
001351
001352     MOVE WS-COMP-ID             TO  WS-CF-COMPANY-ID
001353     MOVE '1'                    TO  WS-CF-RECORD-TYPE
001354     MOVE SPACES                 TO  WS-CF-ACCESS
001355     MOVE +0                     TO  WS-CF-SEQUENCE-NO
001356
001357     
      * EXEC CICS READ
001358*       INTO    (CONTROL-FILE)
001359*       DATASET ('ELCNTL')
001360*       RIDFLD  (WS-CF-KEY)
001361*       RESP    (WS-RESPONSE)
001362*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005302' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035333032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-CF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001363
001364     IF (RESP-NORMAL)
001365        AND (CF-COMPANY-ID = WS-COMP-ID)
001366        AND (CF-RECORD-TYPE = '1')
001367        MOVE CF-CR-REM-TERM-CALC TO WS-CF-CR-REM-TERM-CALC
001368        MOVE CF-CR-R78-METHOD    TO WS-CF-CR-R78-METHOD
001369        MOVE CF-LIFE-OVERRIDE-L1 TO WS-LF-OVERRIDE-L1
001370        MOVE CF-AH-OVERRIDE-L1   TO WS-AH-OVERRIDE-L1
001371        IF CF-DEFAULT-APR NUMERIC
001372           MOVE CF-DEFAULT-APR   TO WS-CF-DEFAULT-APR
001373        ELSE
001374           MOVE ZEROS            TO WS-CF-DEFAULT-APR
001375        END-IF
001376        IF CF-VALID-REM-TRM-OPTION
001377           MOVE CF-REM-TRM-CALC-OPTION
001378                                 TO WS-CF-REM-TRM-CALC-OPTION
001379        ELSE
001380           MOVE SPACES           TO WS-CF-REM-TRM-CALC-OPTION
001381        END-IF
001382     END-IF
001383
001384     .
001385 0310-EXIT.
001386     EXIT.
001387
001388 0320-GET-LIFE-RECORD.
001389
001390     MOVE '4'                    TO WS-CF-RECORD-TYPE
001391     MOVE CM-LF-BENEFIT-CD       TO WS-CF-BENEFIT-NO
001392                                    WS-LF-BEN-CODE
001393     MOVE +0                     TO WS-CF-SEQUENCE-NO
001394     
      * EXEC CICS READ
001395*       INTO    (CONTROL-FILE)
001396*       DATASET ('ELCNTL')
001397*       RIDFLD  (WS-CF-KEY)
001398*       GTEQ
001399*       RESP    (WS-RESPONSE)
001400*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00005339' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303035333339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-CF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001401
001402     IF RESP-NORMAL
001403        AND (CF-COMPANY-ID  = WS-COMP-ID)
001404        AND (CF-RECORD-TYPE = '4')
001405        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
001406           (S1 > +8)
001407           OR (CM-LF-BENEFIT-CD = CF-BENEFIT-CODE (S1))
001408        END-PERFORM
001409        IF S1 > +8
001410           CONTINUE
001411        ELSE
001412           MOVE CF-BENEFIT-DESCRIP (S1)
001413                                 TO WS-LF-BEN-CODE-DESC
001414           MOVE CF-LF-COVERAGE-TYPE (S1)
001415                                 TO WS-CF-LF-COVERAGE-TYPE
001416           IF CF-JOINT-INDICATOR (S1) NOT = 'J'
001417              MOVE 'S'           TO WS-LF-COVERAGE-TYPE
001418           ELSE
001419              MOVE 'J'           TO WS-LF-COVERAGE-TYPE
001420           END-IF
001421           IF CF-CO-REM-TERM-CALC (S1) > '0'
001422               MOVE CF-CO-REM-TERM-CALC (S1)
001423                                 TO WS-LF-CO-REM-TERM-CALC
001424           END-IF
001425           IF CF-CO-EARNINGS-CALC (S1)  > ' '
001426               MOVE CF-CO-EARNINGS-CALC (S1)
001427                                 TO WS-LF-CO-EARNINGS-CALC
001428                                    WS-LF-CO-REFUND-CALC
001429           END-IF
001430           IF CF-SPECIAL-CALC-CD (S1) > ' '
001431               MOVE CF-SPECIAL-CALC-CD (S1)
001432                                 TO WS-LF-SPECIAL-CALC-CD
001433           END-IF
001434           IF CF-CO-REFUND-CALC (S1) > '0'
001435               MOVE CF-CO-REFUND-CALC (S1)
001436                                 TO WS-LF-CO-REFUND-CALC
001437        END-IF
001438     END-IF
001439
001440     .
001441 0320-EXIT.
001442     EXIT.
001443
001444 0330-GET-AH-RECORD.
001445
001446     MOVE '5'                    TO WS-CF-RECORD-TYPE
001447     MOVE CM-AH-BENEFIT-CD       TO WS-CF-BENEFIT-NO
001448     MOVE +0                     TO WS-CF-SEQUENCE-NO
001449     
      * EXEC CICS READ
001450*       INTO    (CONTROL-FILE)
001451*       DATASET ('ELCNTL')
001452*       RIDFLD  (WS-CF-KEY)
001453*       GTEQ
001454*       RESP    (WS-RESPONSE)
001455*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00005394' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303035333934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-CF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001456
001457     IF RESP-NORMAL
001458        AND (CF-COMPANY-ID  = WS-COMP-ID)
001459        AND (CF-RECORD-TYPE = '5')
001460        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
001461           (S1 > +8)
001462           OR (CM-AH-BENEFIT-CD = CF-BENEFIT-CODE (S1))
001463        END-PERFORM
001464        IF S1 > +8
001465           CONTINUE
001466        ELSE
001467           MOVE CF-BENEFIT-DESCRIP (S1)
001468                                 TO WS-AH-BEN-CODE-DESC
001469           IF CF-JOINT-INDICATOR (S1) NOT = 'J'
001470              MOVE 'S'           TO WS-AH-COVERAGE-TYPE
001471           ELSE
001472              MOVE 'J'           TO WS-AH-COVERAGE-TYPE
001473           END-IF
001474           IF CF-CO-REM-TERM-CALC (S1) > '0'
001475              MOVE CF-CO-REM-TERM-CALC (S1)
001476                                 TO WS-AH-CO-REM-TERM-CALC
001477           END-IF
001478           IF CF-CO-EARNINGS-CALC (S1) > ' '
001479              MOVE CF-CO-EARNINGS-CALC (S1)
001480                                 TO WS-AH-CO-EARNINGS-CALC
001481                                    WS-AH-CO-REFUND-CALC
001482           END-IF
001483           IF CF-SPECIAL-CALC-CD (S1) >  ' '
001484              MOVE CF-SPECIAL-CALC-CD (S1)
001485                                 TO WS-AH-SPECIAL-CALC-CD
001486           END-IF
001487           IF CF-CO-REFUND-CALC (S1) > '0'
001488              MOVE CF-CO-REFUND-CALC (S1)
001489                                 TO WS-AH-CO-REFUND-CALC
001490           END-IF
001491           IF CF-BENEFIT-CATEGORY (S1) > ' '
001492              MOVE CF-BENEFIT-CATEGORY (S1)
001493                                 TO WS-AH-BEN-CATEGORY
001494           END-IF
001495        END-IF
001496     END-IF
001497
001498     .
001499 0330-EXIT.
001500     EXIT.
001501
001502 0340-GET-STATE-RECORD.
001503
001504     MOVE WS-COMP-ID             TO WS-CF-COMPANY-ID
001505     MOVE '3'                    TO WS-CF-RECORD-TYPE
001506     MOVE CM-STATE               TO WS-CF-ACCESS
001507     MOVE +0                     TO WS-CF-SEQUENCE-NO
001508
001509     
      * EXEC CICS READ
001510*       INTO    (CONTROL-FILE)
001511*       DATASET ('ELCNTL')
001512*       RIDFLD  (WS-CF-KEY)
001513*       RESP    (WS-RESPONSE)
001514*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005454' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035343534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-CF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001515
001516     IF (RESP-NORMAL)
001517        AND (CF-COMPANY-ID = WS-COMP-ID)
001518        AND (CF-RECORD-TYPE = '3')
001519        AND (CF-STATE-CODE = CM-STATE)
001520        MOVE CF-ST-FREE-LOOK-PERIOD
001521                                 TO WS-FREE-LOOK
001522        MOVE CF-STATE-ABBREVIATION
001523                                 TO WS-STATE-ABBREVIATION
001524        MOVE CF-ST-FST-PMT-DAYS-CHG
001525                                 TO  WS-STATE-EXT-DAYS-CHG
001526        IF CM-LF-BENEFIT-CD NOT = '00' AND '  '
001527           MOVE '0'              TO WS-LF-ST-REM-TERM-CALC
001528           IF CF-ST-RT-CALC NOT = SPACES
001529              MOVE CF-ST-RT-CALC TO WS-LF-ST-REM-TERM-CALC
001530           END-IF
001531           IF WS-CF-LF-COVERAGE-TYPE = 'R'
001532              IF CF-ST-RF-LR-CALC > '0'
001533                 MOVE CF-ST-RF-LR-CALC
001534                                 TO WS-LF-ST-REFUND-CALC
001535              END-IF
001536              IF WS-LF-CO-EARNINGS-CALC = 'N' OR '5'
001537                 IF CF-ST-RF-LN-CALC > '0'
001538                    MOVE CF-ST-RF-LN-CALC
001539                                 TO WS-LF-ST-REFUND-CALC
001540                 END-IF
001541              END-IF
001542           ELSE
001543              IF CF-ST-RF-LL-CALC > '0'
001544                 MOVE CF-ST-RF-LL-CALC
001545                                 TO WS-LF-ST-REFUND-CALC
001546              END-IF
001547           END-IF
001548           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
001549              (S1 > 50)
001550              OR ((WS-LF-OVERRIDE-L1 = CF-ST-BENEFIT-KIND (S1))
001551                 AND (CF-ST-BENEFIT-CD (S1) = CM-LF-BENEFIT-CD))
001552           END-PERFORM
001553           IF S1 < +51
001554              IF CF-ST-REM-TERM-CALC (S1) > '0'
001555                 MOVE CF-ST-REM-TERM-CALC (S1)
001556                                 TO WS-LF-ST-REM-TERM-CALC
001557              END-IF
001558              IF CF-ST-REFUND-CALC (S1) > '0'
001559                 MOVE CF-ST-REFUND-CALC (S1)
001560                                 TO WS-LF-ST-REFUND-CALC
001561              END-IF
001562           END-IF
001563        END-IF
001564        IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
001565           MOVE '0'              TO WS-AH-ST-REM-TERM-CALC
001566           IF CF-ST-RT-CALC NOT = SPACES
001567              MOVE CF-ST-RT-CALC TO WS-AH-ST-REM-TERM-CALC
001568           END-IF
001569           IF CF-ST-RF-AH-CALC > '0'
001570              MOVE CF-ST-RF-AH-CALC
001571                                 TO WS-AH-ST-REFUND-CALC
001572           END-IF
001573           IF WS-AH-SPECIAL-CALC-CD = 'C'
001574              IF CF-ST-RF-CP-CALC > '0'
001575                 MOVE CF-ST-RF-CP-CALC
001576                                 TO WS-AH-ST-REFUND-CALC
001577              END-IF
001578           END-IF
001579
001580           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
001581              (S1 > 50)
001582              OR ((WS-AH-OVERRIDE-L1 = CF-ST-BENEFIT-KIND (S1))
001583                 AND (CF-ST-BENEFIT-CD (S1) = CM-AH-BENEFIT-CD))
001584           END-PERFORM
001585           IF S1 < +51
001586              IF CF-ST-REM-TERM-CALC (S1) > '0'
001587                 MOVE CF-ST-REM-TERM-CALC (S1)
001588                                 TO WS-AH-ST-REM-TERM-CALC
001589              END-IF
001590              IF CF-ST-REFUND-CALC (S1) > '0'
001591                 MOVE CF-ST-REFUND-CALC (S1)
001592                                 TO WS-AH-ST-REFUND-CALC
001593              END-IF
001594           END-IF
001595        END-IF
001596     END-IF
001597
001598     .
001599 0340-EXIT.
001600     EXIT.
001601
001602 0350-GET-ERACCT.
001603
001604     MOVE '0'                    TO WS-AM-EARN-METHOD-L
001605                                    WS-AM-EARN-METHOD-R
001606                                    WS-AM-EARN-METHOD-A
001607                                    WS-LF-AM-REM-TERM-CALC
001608                                    WS-AH-AM-REM-TERM-CALC
001609
001610     MOVE CM-COMPANY-CD          TO WS-AM-COMPANY-CD
001611     MOVE CM-CARRIER             TO WS-AM-CARRIER
001612     MOVE CM-GROUPING            TO WS-AM-GROUPING
001613     MOVE CM-STATE               TO WS-AM-STATE
001614     MOVE CM-ACCOUNT             TO WS-AM-ACCOUNT
001615     MOVE CM-CERT-EFF-DT         TO WS-AM-EXPIRATION-DT
001616     MOVE LOW-VALUES             TO WS-AM-FILLER
001617     MOVE ' '                    TO WS-STOP-SW
001618
001619     
      * EXEC CICS STARTBR
001620*       DATASET  ('ERACCT')
001621*       RIDFLD   (WS-AM-KEY)
001622*       GTEQ
001623*       RESP (WS-RESPONSE)
001624*    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005564' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303035353634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-AM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001625
001626     IF RESP-NORMAL
001627        PERFORM UNTIL TOLD-TO-STOP
001628           
      * EXEC CICS READNEXT
001629*             DATASET  ('ERACCT')
001630*             INTO     (ACCOUNT-MASTER)
001631*             RIDFLD   (WS-AM-KEY)
001632*          END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00005573' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303035353733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 WS-AM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001633           IF RESP-NORMAL
001634              EVALUATE TRUE
001635                 WHEN AM-CONTROL-PRIMARY (1:20) NOT =
001636                    CM-CONTROL-PRIMARY (1:20)
001637                    SET TOLD-TO-STOP TO TRUE
001638                 WHEN (AM-CONTROL-PRIMARY (1:20) =
001639                    CM-CONTROL-PRIMARY (1:20))
001640                    AND (CM-CERT-EFF-DT >= AM-EFFECTIVE-DT)
001641                    AND (CM-CERT-EFF-DT < AM-EXPIRATION-DT)
001642                    SET ERACCT-FOUND      TO TRUE
001643                    SET TOLD-TO-STOP TO TRUE
001644              END-EVALUATE
001645           ELSE
001646              SET TOLD-TO-STOP TO TRUE
001647           END-IF
001648        END-PERFORM
001649        
      * EXEC CICS ENDBR
001650*          DATASET   ('ERACCT')
001651*       END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005594' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035353934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001652     END-IF
001653
001654     .
001655 0350-EXIT.
001656     EXIT.
001657
001658 0400-PROCESS-LIFE.
001659
001660     MOVE SPACES                 TO WS-BUFF-SENT-SW
001661     MOVE CM-INSURED-LAST-NAME   TO WS-LAST-NAME
001662     MOVE CM-INSURED-FIRST-NAME  TO WS-FIRST-NAME
001663     MOVE CM-INSURED-INITIAL2    TO WS-MID-INIT
001664     MOVE CM-JT-LAST-NAME        TO WS-LAST-JNAME
001665     MOVE CM-JT-FIRST-NAME       TO WS-FIRST-JNAME
001666     MOVE CM-JT-INITIAL          TO WS-MID-JINIT
001667     MOVE CM-POST-CARD-IND       TO WS-POST-CARD-SW
001668     MOVE CM-CARRIER             TO WS-CARRIER
001669     MOVE CM-GROUPING            TO WS-GROUPING
001670     MOVE CM-STATE               TO WS-STATE
001671     MOVE CM-ACCOUNT             TO WS-ACCOUNT
001672     MOVE CM-CERT-NO             TO WS-CERT-NO
001673
001674     MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
001675     MOVE ' '                    TO  DC-OPTION-CODE.
001676
001677     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
001678     IF NO-CONVERSION-ERROR
001679        MOVE DC-GREG-DATE-CYMD   TO WS-CERT-EFF-DATE
001680     ELSE
001681        MOVE 99999999            TO WS-CERT-EFF-DATE
001682     END-IF
001683
001684     IF CM-LF-BENEFIT-CD = '  ' OR '00'
001685        GO TO 0400-EXIT
001686     END-IF
001687
001688     MOVE ZEROS                  TO WS-TOT-LF-RFND
001689                                    WS-TOT-LF-PREM
001690                                    WS-TOT-LF-COMM
001691                                    WS-LF-CANCEL-DATE
001692
001693     MOVE CM-LF-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
001694     MOVE ' '                    TO  DC-OPTION-CODE
001695
001696     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
001697     IF NO-CONVERSION-ERROR
001698        MOVE DC-GREG-DATE-CYMD   TO WS-LF-EXPIRE-DATE
001699     ELSE
001700        MOVE ZEROS               TO WS-LF-EXPIRE-DATE
001701     END-IF
001702
001703     IF CM-LF-DEATH-DT NOT = LOW-VALUES
001704        MOVE CM-LF-DEATH-DT      TO DC-BIN-DATE-1
001705        MOVE ' '                 TO DC-OPTION-CODE
001706
001707        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
001708        IF NO-CONVERSION-ERROR
001709           MOVE DC-GREG-DATE-CYMD
001710                                 TO WS-LF-CANCEL-DATE
001711        ELSE
001712           MOVE ZEROS            TO WS-LF-CANCEL-DATE
001713        END-IF
001714     END-IF
001715
001716     IF CM-LF-CANCEL-DT NOT = LOW-VALUES
001717        MOVE CM-LF-CANCEL-DT     TO DC-BIN-DATE-1
001718        MOVE ' '                 TO DC-OPTION-CODE
001719
001720        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
001721        IF NO-CONVERSION-ERROR
001722           MOVE DC-GREG-DATE-CYMD
001723                                 TO WS-LF-CANCEL-DATE
001724        ELSE
001725           MOVE ZEROS            TO WS-LF-CANCEL-DATE
001726        END-IF
001727     END-IF
001728
001729     MOVE WS-LF-OVERRIDE-L1      TO CP-LIFE-OVERRIDE-CODE
001730     MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT
001731     MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE
001732     MOVE WS-BIN-VAL-DT          TO CP-VALUATION-DT
001733     MOVE CM-STATE               TO CP-STATE
001734     MOVE WS-STATE-ABBREVIATION  TO CP-STATE-STD-ABBRV
001735     MOVE WS-CF-LF-COVERAGE-TYPE TO CP-BENEFIT-TYPE
001736     MOVE WS-LF-SPECIAL-CALC-CD  TO CP-SPECIAL-CALC-CD
001737     MOVE CM-PAY-FREQUENCY       TO CP-PAY-FREQUENCY
001738     MOVE CM-LOAN-APR            TO CP-LOAN-APR
001739     MOVE '2'                    TO CP-PROCESS-TYPE
001740     MOVE WS-COMP-ID             TO CP-COMPANY-ID
001741     MOVE CM-COMPANY-CD          TO CP-COMPANY-CD
001742     MOVE WS-ACCT-USER-FLD-5     TO CP-ACCT-FLD-5
001743     MOVE WS-CF-REM-TRM-CALC-OPTION
001744                                 TO CP-REM-TRM-CALC-OPTION
001745     MOVE WS-CF-CR-REM-TERM-CALC TO CP-REM-TERM-METHOD
001746
001747     IF WS-LF-CO-REM-TERM-CALC > '0'
001748        MOVE WS-LF-CO-REM-TERM-CALC
001749                                 TO CP-REM-TERM-METHOD
001750     END-IF
001751
001752     IF WS-LF-ST-REM-TERM-CALC > '0'
001753         MOVE WS-LF-ST-REM-TERM-CALC
001754                                 TO  CP-REM-TERM-METHOD.
001755
001756     IF WS-LF-AM-REM-TERM-CALC > '0'
001757         MOVE WS-LF-AM-REM-TERM-CALC
001758                                 TO  CP-REM-TERM-METHOD.
001759
001760     IF (WS-COMP-ID = 'CID')
001761        AND (CP-STATE-STD-ABBRV = 'WI')
001762        MOVE '7'                 TO CP-REM-TERM-METHOD
001763        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
001764     END-IF
001765
001766     IF (WS-COMP-ID = 'CID')
001767        AND (CP-STATE-STD-ABBRV = 'MO')
001768        AND (CM-CERT-EFF-DT >= X'9B41')
001769        AND (CM-CERT-EFF-DT <= X'A2FB')
001770        MOVE '7'                 TO CP-REM-TERM-METHOD
001771        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
001772     END-IF
001773
001774     MOVE CM-LF-ORIG-TERM        TO CP-ORIGINAL-TERM
001775                                    CP-LOAN-TERM
001776                                    WS-LF-TERM
001777
001778     IF CP-TRUNCATED-LIFE
001779         MOVE CM-LOAN-TERM       TO  CP-LOAN-TERM.
001780
001781     IF CP-TERM-IS-DAYS
001782        IF CM-LF-TERM-IN-DAYS NUMERIC
001783           MOVE CM-LF-TERM-IN-DAYS
001784                                 TO CP-TERM-OR-EXT-DAYS
001785        ELSE
001786           MOVE ZEROS            TO CP-TERM-OR-EXT-DAYS
001787        END-IF
001788     ELSE
001789        IF CM-PMT-EXTENSION-DAYS NUMERIC
001790           MOVE CM-PMT-EXTENSION-DAYS
001791                                 TO CP-TERM-OR-EXT-DAYS
001792        ELSE
001793           MOVE ZEROS            TO CP-TERM-OR-EXT-DAYS
001794        END-IF
001795     END-IF
001796
001797     MOVE WS-FREE-LOOK           TO CP-FREE-LOOK
001798
001799     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
001800        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
001801        ADD +1   TO CP-ORIGINAL-TERM CP-LOAN-TERM
001802     END-IF
001803
001804     DISPLAY ' STARTING OPEN LIFE CLAIM TEST '
001805
001806     MOVE LOW-VALUES             TO WS-LF-BIN-PAID-THRU-DT
001807     MOVE ' '                    TO WS-CLM-STOP-SW
001808     MOVE CM-COMPANY-CD          TO ELMSTR-COMP-CD
001809     MOVE CM-CERT-NO             TO ELMSTR-CERT-NO
001810     
      * EXEC CICS STARTBR
001811*       DATASET     ('ELMSTR5')
001812*       RIDFLD      (ELMSTR-KEY)
001813*       RESP        (WS-RESPONSE)
001814*    END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005755' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303035373535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001815
001816     DISPLAY ' AFTER START ' WS-RESPONSE
001817
001818     IF RESP-NORMAL
001819        PERFORM WITH TEST AFTER UNTIL I-SAY-TO-STOP
001820
001821           
      * EXEC CICS READNEXT
001822*             DATASET   ('ELMSTR5')
001823*             RIDFLD    (ELMSTR-KEY)
001824*             INTO      (CLAIM-MASTER)
001825*             RESP      (WS-RESPONSE)
001826*          END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005766' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303035373636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV12, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001827
001828        IF (RESP-NORMAL OR RESP-DUPKEY)
001829           AND (CM-COMPANY-CD = CL-COMPANY-CD)
001830           AND (CM-CERT-NO = CL-CERT-NO)
001831           IF (CM-ACCOUNT = CL-CERT-ACCOUNT)
001832              AND (CM-CERT-EFF-DT = CL-CERT-EFF-DT)
001833              IF CL-CLAIM-TYPE = 'L' OR 'O'
001834                 IF CM-AH-BENEFIT-CD NOT = '  ' AND '00'
001835                    MOVE 'D'     TO WS-AH-STATUS
001836                 END-IF
001837                 IF CLAIM-IS-OPEN
001838                    MOVE ' OPEN LIFE CLAIM '
001839                                 TO WS-COMMENT
001840                    MOVE 'L'     TO WS-LF-STATUS
001841                    GO TO 0400-EXIT
001842                 ELSE
001843                    MOVE ' DEATH CLAIM APPLIED '
001844                                 TO WS-COMMENT
001845                    MOVE 'D'     TO WS-LF-STATUS
001846*                   IF CL-PAID-THRU-DT > WS-LF-BIN-PAID-THRU-DT
001847*                      MOVE CL-PAID-THRU-DT
001848*                                TO WS-LF-BIN-PAID-THRU-DT
001849*                   END-IF
001850                 END-IF
001851              END-IF
001852           END-IF
001853        ELSE
001854           SET I-SAY-TO-STOP TO TRUE
001855        END-IF
001856
001857        END-PERFORM
001858        
      * EXEC CICS ENDBR
001859*          DATASET   ('ELMSTR5')
001860*       END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005803' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035383033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001861     END-IF
001862
001863     IF WS-LF-BIN-PAID-THRU-DT NOT = LOW-VALUES
001864*       THIS WILL BE TRUE IF WE FOUND A CLOSED LIFE CLAIM
001865        MOVE WS-LF-BIN-PAID-THRU-DT
001866                                 TO DC-BIN-DATE-1
001867        MOVE ' '                 TO DC-OPTION-CODE
001868        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
001869        IF NO-CONVERSION-ERROR
001870           MOVE DC-GREG-DATE-CYMD
001871                                 TO WS-CLM-PAID-THRU-DT
001872        ELSE
001873           MOVE 99999999         TO WS-CLM-PAID-THRU-DT
001874        END-IF
001875*       IF WS-BIN-VAL-DT <= WS-LF-BIN-PAID-THRU-DT
001876*          MOVE 'CAN DT =< CLM PD THRU DT'
001877*                                TO WS-COMMENT
001878*          MOVE 'P'              TO WS-LF-STATUS
001879*          GO TO 0400-EXIT
001880*       END-IF
001881        GO TO 0400-EXIT
001882     END-IF
001883
001884     PERFORM 0700-LINK-REM-TERM  THRU  0700-EXIT
001885
001886     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
001887        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
001888        MOVE CP-REMAINING-TERM-1   TO WS-BALLOON-RTRM
001889        COMPUTE CP-REMAINING-TERM-1 =
001890                CP-REMAINING-TERM-1 - +1
001891        COMPUTE CP-REMAINING-TERM-2 =
001892                CP-REMAINING-TERM-2 - +1
001893     END-IF
001894
001895     IF CP-REMAINING-TERM-1 NEGATIVE
001896        MOVE ZEROS               TO CP-REMAINING-TERM-1
001897     END-IF
001898     IF CP-REMAINING-TERM-2 NEGATIVE
001899        MOVE ZEROS               TO CP-REMAINING-TERM-2
001900     END-IF
001901
001902     IF CM-LIFE-COMM-PCT NOT NUMERIC
001903        MOVE ZERO                TO CM-LIFE-COMM-PCT
001904     END-IF
001905
001906     IF CM-LIFE-COMM-PCT > ZEROS
001907        COMPUTE WS-TOT-LF-COMM = CM-LIFE-COMM-PCT *
001908           (CM-LF-PREMIUM-AMT + CM-LF-ALT-PREMIUM-AMT)
001909     END-IF
001910     MOVE WS-TOT-LF-COMM         TO WS-LF-COMM
001911
001912     MOVE CP-REMAINING-TERM-1    TO WS-LF-REM-TERM
001913     IF CM-LF-CURRENT-STATUS = '8'
001914        IF CM-LF-CANCEL-DT NOT = LOW-VALUES
001915           MOVE ' LIFE COVERAGE PREVIOUSLY CANCELLED '
001916                                 TO WS-COMMENT
001917           MOVE 'C'              TO WS-LF-STATUS
001918           move cm-lf-itd-cancel-amt
001919                                 to ws-lf-refund
001920*          PERFORM 0030-BUILD-BUFFER
001921*                                THRU 0030-EXIT
001922*          PERFORM 0020-SEND-BUFFER
001923*                                THRU 0020-EXIT
001924*          SET ALREADY-SENT      TO TRUE
001925*          MOVE SPACES           TO WS-COMMENT
001926           GO TO 0400-EXIT
001927*          PERFORM 0025-CLOSE-SOCKET
001928*                                THRU 0025-EXIT
001929*          GO TO 0010-RETURN
001930        END-IF
001931     END-IF
001932
001933     IF CM-LF-CURRENT-STATUS = '7'
001934        IF CM-LF-DEATH-DT NOT = LOW-VALUES
001935           MOVE ' DEATH CLAIM APPLIED '
001936                                 TO WS-COMMENT
001937           MOVE 'D'              TO WS-LF-STATUS
001938*          PERFORM 0030-BUILD-BUFFER
001939*                                THRU 0030-EXIT
001940*          IF NOT ALREADY-SENT
001941*             PERFORM 0020-SEND-BUFFER
001942*                                THRU 0020-EXIT
001943*             SET ALREADY-SENT   TO TRUE
001944*          END-IF
001945           GO TO 0400-EXIT
001946*          PERFORM 0025-CLOSE-SOCKET
001947*                                THRU 0025-EXIT
001948*          GO TO 0010-RETURN
001949        END-IF
001950     END-IF
001951
001952     IF ((CM-LF-LOAN-EXPIRE-DT < WS-BIN-VAL-DT)
001953        AND (CM-LF-LOAN-EXPIRE-DT > LOW-VALUES))
001954        MOVE 'LF COVERAGE EXPIRED '
001955                                 TO WS-COMMENT
001956        MOVE 'E'                 TO WS-LF-STATUS
001957        GO TO 0400-EXIT
001958     END-IF
001959
001960     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
001961        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
001962        MOVE WS-BALLOON-RTRM     TO CP-REMAINING-TERM
001963     ELSE
001964        MOVE CP-REMAINING-TERM-2 TO CP-REMAINING-TERM
001965     END-IF
001966
001967     MOVE WS-LF-CO-EARNINGS-CALC TO  CP-EARNING-METHOD.
001968     MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT.
001969*    MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE
001970
001971     if ws-comp-id = 'AHL'
001972        move cm-lf-class-cd      to cp-class-code
001973        if cp-class-code = spaces
001974           move zeros            to cp-class-code
001975        end-if
001976     else
001977        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
001978     end-if
001979
001980
001981     IF (ws-comp-id = 'DCC' or 'VPP')
001982        AND (AM-DCC-PRODUCT-CODE = 'DDF')
001983        MOVE ' '                 TO WS-PDEF-RECORD-SW
001984        PERFORM 0730-GET-DDF-FACTORS
001985                                 THRU 0730-EXIT
001986        IF PDEF-FOUND
001987           MOVE CL-INSURED-BIRTH-DT
001988                                  TO DC-BIN-DATE-1
001989           MOVE CL-INCURRED-DT    TO DC-BIN-DATE-2
001990           MOVE '1'               TO DC-OPTION-CODE
001991           PERFORM 9700-DATE-LINK   THRU 9700-EXIT
001992           COMPUTE WS-ATT-AGE =
001993               DC-ELAPSED-MONTHS / 12
001994           MOVE ZEROS TO DC-ELAPSED-MONTHS DC-ELAPSED-DAYS
001995
001996           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
001997              (P1 > +11)
001998              OR (PD-PROD-CODE (P1) = 'L' or 'O'
001999               AND PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
002000           END-PERFORM
002001           IF P1 < +12
002002              MOVE PD-MAX-AMT (P1) TO CP-R-MAX-TOT-BEN
002003           END-IF
002004        END-IF
002005     END-IF
002006
002007
002008
002009     PERFORM 0710-LINK-REM-AMOUNT THRU 0710-EXIT
002010
002011*    MOVE CM-LF-BENEFIT-AMT      TO WS-LF-ORIG-BENEFIT
002012     COMPUTE WS-LF-ORIG-BENEFIT = CM-LF-BENEFIT-AMT +
002013        CM-LF-ALT-BENEFIT-AMT
002014*    ADD CM-LF-ALT-BENEFIT-AMT   TO WS-LF-ORIG-BENEFIT
002015     MOVE CP-REMAINING-AMT       TO CP-REMAINING-BENEFIT
002016                                    WS-LF-REM-BEN
002017
002018     move cm-lf-premium-amt      to ws-tot-lf-prem
002019     if ws-lf-co-earnings-calc = 'B'
002020        COMPUTE WS-TOT-LF-PREM = ws-tot-lf-prem
002021           + CM-LF-ALT-PREMIUM-AMT
002022     end-if
002023
002024     MOVE WS-TOT-LF-PREM         TO WS-LF-PREM
002025
002026     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
002027        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
002028        MOVE WS-BALLOON-RTRM     TO CP-REMAINING-TERM
002029     ELSE
002030        MOVE CP-REMAINING-TERM-1 TO  CP-REMAINING-TERM
002031     END-IF
002032
002033     MOVE WS-LF-CO-REFUND-CALC   TO  CP-EARNING-METHOD.
002034     MOVE WS-LF-CO-EARNINGS-CALC TO  CP-RATING-METHOD.
002035
002036     IF WS-LF-ST-REFUND-CALC > ZERO
002037         MOVE WS-LF-ST-REFUND-CALC
002038                                 TO  CP-EARNING-METHOD.
002039
002040     IF WS-LF-FO-REFUND-CALC > ZERO
002041         MOVE WS-LF-FO-REFUND-CALC
002042                                 TO  CP-EARNING-METHOD.
002043
002044     IF CP-RATING-METHOD = '4'
002045        MOVE '4'                 TO CP-EARNING-METHOD
002046     END-IF
002047
002048*    MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE.
002049
002050     if ws-comp-id = 'AHL'
002051        move cm-lf-class-cd      to cp-class-code
002052        if cp-class-code = spaces
002053           move zeros            to cp-class-code
002054        end-if
002055     else
002056        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
002057     end-if
002058
002059     MOVE CM-LF-BENEFIT-CD       TO  CP-BENEFIT-CD.
002060     MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
002061                                     CP-RATING-BENEFIT-AMT.
002062     IF CP-STATE-STD-ABBRV = 'OR'
002063         COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
002064                                         CM-LF-ALT-BENEFIT-AMT.
002065****   N O T E   ****
002066*      CID DOES NOT WANT THE REFUND METHOD TO OVERRIDE
002067*      THE OH HARD CODING IN ELCRFNDP
002068
002069     IF WS-COMP-ID = 'CID'
002070        IF CP-STATE-STD-ABBRV = 'OH'
002071           MOVE WS-LF-CO-EARNINGS-CALC
002072                                 TO CP-EARNING-METHOD
002073        END-IF
002074     END-IF
002075
002076****   N O T E   ****
002077
002078     MOVE CM-LF-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
002079     MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
002080     MOVE CM-LF-DEV-CODE         TO  CP-DEVIATION-CODE.
002081     MOVE CM-LF-DEV-PCT          TO  CP-RATE-DEV-PCT.
002082     MOVE WS-CF-CR-R78-METHOD    TO  CP-R78-OPTION.
002083
002084*    IF CP-EARN-AS-NET-PAY
002085*       IF CP-LOAN-APR <= ZEROS
002086*          IF WS-CF-DEFAULT-APR > ZEROS
002087*             MOVE WS-CF-DEFAULT-APR
002088*                                TO  CP-LOAN-APR
002089*          END-IF
002090*       END-IF
002091*    END-IF
002092
002093
002094*    DISPLAY ' SOCK REM AMT      ' CP-REMAINING-BENEFIT
002095*    DISPLAY ' SOCK REM TRM      ' CP-REMAINING-TERM
002096*    DISPLAY ' SOCK EARN METH    ' CP-EARNING-METHOD
002097*    DISPLAY ' SOCK RATE METH    ' CP-RATING-METHOD
002098*    DISPLAY ' SOCK CLASS        ' CP-CLASS-CODE
002099*    DISPLAY ' SOCK BENE CODE    ' CP-BENEFIT-CD
002100*    DISPLAY ' SOCK ORIG BENE    ' CP-ORIGINAL-BENEFIT
002101*    DISPLAY ' SOCK RATE BENE    ' CP-RATING-BENEFIT-AMT
002102*    DISPLAY ' SOCK ORIG PREM    ' CP-ORIGINAL-PREMIUM
002103*    DISPLAY ' SOCK ISS AGE      ' CP-ISSUE-AGE
002104*    DISPLAY ' SOCK DEV CODE     ' CP-DEVIATION-CODE
002105*    DISPLAY ' SOCK DEV PCT      ' CP-RATE-DEV-PCT
002106*    DISPLAY ' SOCK APR          ' CP-LOAN-APR
002107*    DISPLAY ' SOCK EXT DAYS     ' CP-TERM-OR-EXT-DAYS
002108*    DISPLAY ' SOCK LOAN TERM    ' CP-LOAN-TERM
002109*    DISPLAY ' SOCK SPEC CALC    ' CP-SPECIAL-CALC-CD
002110*    DISPLAY ' SOCK ST STD ABB   ' CP-STATE-STD-ABBRV
002111
002112
002113*    MOVE WS-STATE-EXT-DAYS-CHG  TO CP-EXT-DAYS-CALC
002114
002115
002116     PERFORM 0720-LINK-REFUND  THRU  0720-EXIT
002117
002118     DISPLAY ' SOCK REF          ' CP-CALC-REFUND
002119
002120     DISPLAY ' MADE IT BACK FROM ELRFND '
002121     IF (CP-ERROR-RATE-IS-ZERO)
002122        OR (CP-ERROR-RATE-NOT-FOUND)
002123        OR (CP-ERROR-RATE-FILE-NOTOPEN)
002124        STRING ' ERROR WITH LIFE REFUND ' CP-RETURN-CODE
002125          DELIMITED BY SIZE INTO WS-COMMENT
002126        END-STRING
002127        MOVE 'X'                 TO WS-LF-STATUS
002128        GO TO 0400-EXIT
002129*       PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
002130*       IF NOT ALREADY-SENT
002131*          PERFORM 0020-SEND-BUFFER
002132*                                THRU 0020-EXIT
002133*          SET ALREADY-SENT      TO TRUE
002134*       END-IF
002135*       PERFORM 0025-CLOSE-SOCKET
002136*                                THRU 0025-EXIT
002137*       GO TO 0010-RETURN
002138     END-IF
002139
002140     MOVE CP-CALC-REFUND         TO WS-LF-REFUND
002141                                    WS-TOT-LF-RFND
002142
002143     IF (WS-LF-STATUS = SPACES)
002144        OR (CP-CALC-REFUND > ZEROS)
002145        MOVE 'A'                 TO WS-LF-STATUS
002146     END-IF
002147
002148     EVALUATE TRUE
002149        WHEN CP-R-AS-R78
002150           MOVE 'RULE 78'          TO  WS-LF-METHOD
002151        WHEN  CP-R-AS-PRORATA
002152           MOVE 'PRORATA'          TO  WS-LF-METHOD
002153        WHEN CP-R-AS-CALIF
002154           MOVE 'CALIF'            TO  WS-LF-METHOD
002155        WHEN CP-R-AS-TEXAS
002156           MOVE 'IRREG/FARM PLAN'  TO  WS-LF-METHOD
002157        WHEN CP-REFUND-TYPE-USED IS EQUAL TO 'S'
002158           MOVE 'UTAH'             TO  WS-LF-METHOD
002159        WHEN CP-R-AS-FARM-PLAN
002160           MOVE 'IRREG/FARM PLAN'  TO  WS-LF-METHOD
002161        WHEN CP-R-AS-NET-PAY
002162           MOVE 'NET PAY'          TO  WS-LF-METHOD
002163        WHEN CP-R-AS-ANTICIPATION
002164           MOVE 'ANTICIPATION'     TO  WS-LF-METHOD
002165        WHEN CP-R-AS-MEAN
002166           MOVE 'MEAN'             TO  WS-LF-METHOD
002167        WHEN CP-R-AS-SUM-OF-DIGITS
002168           MOVE 'SUM OF DIGITS'    TO  WS-LF-METHOD
002169        WHEN CP-R-AS-REPOSSESSION
002170           MOVE 'REPOSSESSION'     TO  WS-LF-METHOD
002171     END-EVALUATE
002172
002173     IF WS-TOT-LF-RFND > ZEROS
002174        COMPUTE WS-WORK-FACTOR ROUNDED =
002175           WS-TOT-LF-RFND / WS-TOT-LF-PREM
002176        COMPUTE WS-LF-UEC ROUNDED = WS-WORK-FACTOR *
002177           WS-TOT-LF-COMM
002178     END-IF
002179
002180     IF ERACCT-FOUND
002181        IF AM-COMM-CHARGEBACK (1) NOT NUMERIC
002182           MOVE ZEROS            TO AM-COMM-CHARGEBACK (1)
002183        END-IF
002184        IF AM-COMM-CHARGEBACK (1) NOT = ZEROS
002185           IF (AM-COMM-CHARGEBACK (1) = 99)
002186                     OR
002187              (WS-NCB-DIFF-MONTHS > AM-COMM-CHARGEBACK (1))
002188                     OR
002189              ((WS-NCB-DIFF-MONTHS = AM-COMM-CHARGEBACK (1))
002190              AND (WS-NCB-DIFF-ODD-DAYS > ZEROS))
002191              MOVE ZEROS         TO WS-LF-UEC
002192           END-IF
002193        END-IF
002194     END-IF
002195
002196     IF WS-LF-CO-EARNINGS-CALC NOT = 'B'
002197        GO TO 0400-EXIT
002198     END-IF
002199
002200     MOVE 'L'                    TO  CP-BENEFIT-TYPE.
002201     MOVE '2'                    TO  CP-EARNING-METHOD
002202                                     CP-RATING-METHOD.
002203     MOVE CM-LF-ALT-BENEFIT-AMT  TO  CP-ORIGINAL-BENEFIT
002204                                     CP-REMAINING-BENEFIT
002205                                     CP-RATING-BENEFIT-AMT.
002206     IF CP-STATE-STD-ABBRV = 'OR'
002207         COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
002208                                         CM-LF-ALT-BENEFIT-AMT.
002209     MOVE CM-LF-ALT-PREMIUM-AMT  TO  CP-ORIGINAL-PREMIUM.
002210
002211     IF ws-comp-id = 'CID'
002212        IF (CP-STATE-STD-ABBRV = 'MN')
002213           AND (CM-CERT-EFF-DT > X'A4FF')
002214           AND (WS-LF-CO-EARNINGS-CALC = 'B')
002215           MOVE '5'              TO CP-EARNING-METHOD
002216           MOVE WS-LF-CO-EARNINGS-CALC TO CP-RATING-METHOD
002217        END-IF
002218     END-IF
002219
002220     MOVE 'LEV'                  TO  CP-DEVIATION-CODE.
002221
002222     PERFORM 0720-LINK-REFUND  THRU  0720-EXIT.
002223
002224     IF (CP-ERROR-RATE-IS-ZERO)
002225        OR (CP-ERROR-RATE-NOT-FOUND)
002226        OR (CP-ERROR-RATE-FILE-NOTOPEN)
002227        STRING ' ERROR WITH BALLOON REFUND ' CP-RETURN-CODE
002228          DELIMITED BY SIZE INTO WS-COMMENT
002229        END-STRING
002230        MOVE 'X'              TO WS-LF-STATUS
002231        GO TO 0400-EXIT
002232*       PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
002233*       IF NOT ALREADY-SENT
002234*          PERFORM 0020-SEND-BUFFER
002235*                                THRU 0020-EXIT
002236*          SET ALREADY-SENT      TO TRUE
002237*       END-IF
002238*       PERFORM 0025-CLOSE-SOCKET
002239*                                THRU 0025-EXIT
002240*       GO TO 0010-RETURN
002241     END-IF
002242
002243     ADD CP-CALC-REFUND          TO WS-TOT-LF-RFND
002244     MOVE WS-TOT-LF-RFND         TO WS-LF-REFUND
002245
002246     IF WS-TOT-LF-RFND > ZEROS
002247        COMPUTE WS-LF-UEC = (WS-TOT-LF-RFND / WS-TOT-LF-PREM) *
002248           WS-TOT-LF-COMM
002249     END-IF
002250
002251     IF ERACCT-FOUND
002252        IF AM-COMM-CHARGEBACK (1) NOT = ZEROS
002253           IF (AM-COMM-CHARGEBACK (1) = 99)
002254                     OR
002255              (WS-NCB-DIFF-MONTHS > AM-COMM-CHARGEBACK (1))
002256                     OR
002257              ((WS-NCB-DIFF-MONTHS = AM-COMM-CHARGEBACK (1))
002258              AND (WS-NCB-DIFF-ODD-DAYS > ZEROS))
002259              MOVE ZEROS         TO WS-LF-UEC
002260           END-IF
002261        END-IF
002262     END-IF
002263
002264     .
002265 0400-EXIT.
002266     EXIT.
002267
002268 0500-PROCESS-AH.
002269
002270     IF CM-AH-BENEFIT-CD = '  ' OR '00'
002271        GO TO 0500-EXIT
002272     END-IF
002273
002274     MOVE ZEROS                  TO WS-TOT-AH-RFND
002275                                    WS-TOT-AH-PREM
002276                                    WS-TOT-AH-COMM
002277                                    WS-AH-CANCEL-DATE
002278
002279     MOVE CM-AH-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
002280     MOVE ' '                    TO  DC-OPTION-CODE
002281
002282     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
002283     IF NO-CONVERSION-ERROR
002284        MOVE DC-GREG-DATE-CYMD   TO WS-AH-EXPIRE-DATE
002285     ELSE
002286        MOVE ZEROS               TO WS-AH-EXPIRE-DATE
002287     END-IF
002288
002289     IF CM-AH-CANCEL-DT NOT = LOW-VALUES
002290        MOVE CM-AH-CANCEL-DT     TO DC-BIN-DATE-1
002291        MOVE ' '                 TO DC-OPTION-CODE
002292
002293        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
002294        IF NO-CONVERSION-ERROR
002295           MOVE DC-GREG-DATE-CYMD TO WS-AH-CANCEL-DATE
002296        ELSE
002297           MOVE ZEROS            TO WS-AH-CANCEL-DATE
002298        END-IF
002299     END-IF
002300
002301     MOVE CM-AH-BENEFIT-CD       TO WS-AH-BEN-CODE
002302     MOVE CM-AH-ORIG-TERM        TO WS-AH-TERM
002303
002304     MOVE WS-AH-OVERRIDE-L1      TO CP-AH-OVERRIDE-CODE
002305     MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
002306     MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
002307     MOVE WS-BIN-VAL-DT          TO  CP-VALUATION-DT.
002308     MOVE CM-STATE               TO  CP-STATE.
002309     MOVE WS-STATE-ABBREVIATION  TO  CP-STATE-STD-ABBRV.
002310     MOVE 'A'                    TO  CP-BENEFIT-TYPE.
002311     MOVE WS-AH-SPECIAL-CALC-CD  TO  CP-SPECIAL-CALC-CD.
002312     MOVE '2'                    TO  CP-PROCESS-TYPE.
002313     MOVE WS-COMP-ID             TO  CP-COMPANY-ID.
002314     MOVE CM-COMPANY-CD          TO  CP-COMPANY-CD.
002315     MOVE WS-ACCT-USER-FLD-5     TO  CP-ACCT-FLD-5.
002316     MOVE WS-CF-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION
002317     MOVE WS-CF-CR-REM-TERM-CALC
002318                                 TO  CP-REM-TERM-METHOD.
002319
002320     IF WS-AH-CO-REM-TERM-CALC > '0'
002321         MOVE WS-AH-CO-REM-TERM-CALC
002322                                 TO  CP-REM-TERM-METHOD.
002323
002324     IF WS-AH-ST-REM-TERM-CALC > '0'
002325         MOVE WS-AH-ST-REM-TERM-CALC
002326                                 TO  CP-REM-TERM-METHOD.
002327
002328     IF WS-AH-AM-REM-TERM-CALC > '0'
002329         MOVE WS-AH-AM-REM-TERM-CALC
002330                                 TO  CP-REM-TERM-METHOD.
002331     IF (WS-COMP-ID = 'CID')
002332        AND (CP-STATE-STD-ABBRV = 'WI')
002333        MOVE '7'                 TO CP-REM-TERM-METHOD
002334        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
002335     END-IF
002336
002337     IF (WS-COMP-ID = 'CID')
002338        AND (CP-STATE-STD-ABBRV = 'MO')
002339        AND (CM-CERT-EFF-DT >= X'9B41')
002340        AND (CM-CERT-EFF-DT <= X'A2FB')
002341        MOVE '7'                 TO CP-REM-TERM-METHOD
002342        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
002343     END-IF
002344
002345     MOVE CM-AH-ORIG-TERM        TO  CP-ORIGINAL-TERM
002346                                     CP-LOAN-TERM.
002347
002348     IF  NOT  CP-TERM-IS-DAYS
002349         IF CM-PMT-EXTENSION-DAYS  IS NUMERIC
002350             MOVE CM-PMT-EXTENSION-DAYS
002351                                 TO  CP-TERM-OR-EXT-DAYS
002352         ELSE
002353             MOVE ZEROS          TO  CP-TERM-OR-EXT-DAYS.
002354
002355     MOVE WS-FREE-LOOK           TO CP-FREE-LOOK
002356
002357     IF CM-AH-COMM-PCT > ZEROS
002358        COMPUTE WS-TOT-AH-COMM = CM-AH-COMM-PCT *
002359           CM-AH-PREMIUM-AMT
002360     END-IF
002361
002362     IF (ws-comp-id = 'DCC' or 'VPP')
002363        AND (AM-DCC-PRODUCT-CODE = 'DDF')
002364        compute ws-tot-ah-comm = cm-ah-premium-amt -
002365           (cm-ah-clp + cm-addl-clp)
002366     end-if
002367
002368     MOVE WS-TOT-AH-COMM         TO WS-AH-COMM
002369
002370     IF WS-AH-STATUS = 'D'
002371        GO TO 0500-EXIT
002372     END-IF
002373
002374     DISPLAY ' BEGIN OPEN AH CLAIM TEST '
002375     MOVE LOW-VALUES             TO WS-AH-BIN-PAID-THRU-DT
002376     MOVE ' '                    TO WS-CLM-STOP-SW
002377     MOVE CM-COMPANY-CD          TO ELMSTR-COMP-CD
002378     MOVE CM-CERT-NO             TO ELMSTR-CERT-NO
002379     
      * EXEC CICS STARTBR
002380*       DATASET     ('ELMSTR5')
002381*       RIDFLD      (ELMSTR-KEY)
002382*       RESP        (WS-RESPONSE)
002383*    END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006324' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303036333234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002384
002385     DISPLAY ' AFTER START ' WS-RESPONSE
002386
002387     IF RESP-NORMAL
002388        PERFORM WITH TEST AFTER UNTIL I-SAY-TO-STOP
002389
002390           DISPLAY ' AH READNEXT '
002391           
      * EXEC CICS READNEXT
002392*             DATASET   ('ELMSTR5')
002393*             RIDFLD    (ELMSTR-KEY)
002394*             INTO      (CLAIM-MASTER)
002395*             RESP      (WS-RESPONSE)
002396*          END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00006336' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303036333336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV12, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002397
002398        IF (RESP-NORMAL OR RESP-DUPKEY)
002399           AND (CM-COMPANY-CD = CL-COMPANY-CD)
002400           AND (CM-CERT-NO = CL-CERT-NO)
002401           IF (CM-ACCOUNT = CL-CERT-ACCOUNT)
002402              AND (CM-CERT-EFF-DT = CL-CERT-EFF-DT)
002403              IF (CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
002404                                      OR 'B' OR 'H')
002405                 IF CLAIM-IS-OPEN
002406                    DISPLAY ' FOUND OPEN AH CLAIM '
002407                    MOVE ' OPEN HEALTH CLAIM '
002408                                 TO WS-COMMENT
002409                    MOVE 'H'     TO WS-AH-STATUS
002410                 ELSE
002411                    DISPLAY ' FOUND CLOSED AH CLAIM '
002412                    MOVE ' CLOSED HEALTH CLAIM '
002413                                 TO WS-COMMENT
002414                    IF CL-PAID-THRU-DT > WS-AH-BIN-PAID-THRU-DT
002415                       MOVE CL-PAID-THRU-DT
002416                                 TO WS-AH-BIN-PAID-THRU-DT
002417                    END-IF
002418                 END-IF
002419              END-IF
002420           END-IF
002421        ELSE
002422           SET I-SAY-TO-STOP TO TRUE
002423        END-IF
002424
002425        END-PERFORM
002426        
      * EXEC CICS ENDBR
002427*          DATASET   ('ELMSTR5')
002428*       END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006371' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036333731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002429     END-IF
002430
002431     IF WS-AH-BIN-PAID-THRU-DT NOT = LOW-VALUES
002432*       THIS WILL BE TRUE IF WE FOUND A CLOSED AH CLAIM
002433        MOVE WS-AH-BIN-PAID-THRU-DT
002434                                 TO DC-BIN-DATE-1
002435        MOVE ' '                 TO DC-OPTION-CODE
002436        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
002437        IF NO-CONVERSION-ERROR
002438           MOVE DC-GREG-DATE-CYMD
002439                                 TO WS-CLM-PAID-THRU-DT
002440        ELSE
002441           MOVE 99999999         TO WS-CLM-PAID-THRU-DT
002442        END-IF
002443        IF WS-BIN-VAL-DT <= WS-AH-BIN-PAID-THRU-DT
002444           MOVE 'CAN DT NOT > CLM CLOSE DT '
002445                                 TO WS-COMMENT
002446           MOVE 'P'              TO WS-AH-STATUS
002447           GO TO 0500-EXIT
002448        END-IF
002449     END-IF
002450
002451     PERFORM 0700-LINK-REM-TERM  THRU  0700-EXIT
002452
002453     IF CM-AH-CURRENT-STATUS = '8'
002454        IF CM-AH-CANCEL-DT NOT = LOW-VALUES
002455           MOVE ' AH COVERAGE PREVIOUSLY CANCELLED '
002456                                 TO WS-COMMENT
002457           MOVE 'C'              TO WS-AH-STATUS
002458           move cm-ah-itd-cancel-amt
002459                                 to ws-ah-refund
002460           GO TO 0500-EXIT
002461        END-IF
002462     END-IF
002463
002464     IF CM-AH-CURRENT-STATUS = '6'  OR  '7'
002465        IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
002466           MOVE ' SETTLEMENT CLAIM APPLIED '
002467                                 TO WS-COMMENT
002468           MOVE 'S'              TO WS-AH-STATUS
002469*          PERFORM 0030-BUILD-BUFFER
002470*                                THRU 0030-EXIT
002471*          IF NOT ALREADY-SENT
002472*             PERFORM 0020-SEND-BUFFER
002473*                                THRU 0020-EXIT
002474*             SET ALREADY-SENT   TO TRUE
002475*          END-IF
002476           GO TO 0500-EXIT
002477*          PERFORM 0025-CLOSE-SOCKET
002478*                                THRU 0025-EXIT
002479*          GO TO 0010-RETURN
002480        END-IF
002481     END-IF
002482
002483     IF ((CM-AH-LOAN-EXPIRE-DT < WS-BIN-VAL-DT)
002484        AND (CM-AH-LOAN-EXPIRE-DT > LOW-VALUES))
002485        MOVE 'AH COVERAGE EXPIRED '
002486                                 TO WS-COMMENT
002487        MOVE 'E'                 TO WS-AH-STATUS
002488        GO TO 0500-EXIT
002489     END-IF
002490
002491     MOVE CM-AH-PREMIUM-AMT      TO WS-TOT-AH-PREM
002492                                    WS-AH-PREM
002493
002494     MOVE CM-AH-BENEFIT-AMT      TO WS-AH-ORIG-BENEFIT
002495     COMPUTE WS-AH-REM-BEN = CM-AH-BENEFIT-AMT
002496        * CP-REMAINING-TERM-1
002497     MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM
002498                                     WS-AH-REM-TERM
002499     MOVE WS-AH-CO-REFUND-CALC   TO  CP-EARNING-METHOD.
002500     MOVE WS-AH-CO-EARNINGS-CALC TO  CP-RATING-METHOD.
002501
002502     IF WS-AH-ST-REFUND-CALC > ZERO
002503         MOVE WS-AH-ST-REFUND-CALC
002504                                 TO  CP-EARNING-METHOD.
002505
002506     IF WS-AH-FO-REFUND-CALC > ZERO
002507         MOVE WS-AH-FO-REFUND-CALC
002508                                 TO  CP-EARNING-METHOD.
002509
002510*    MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE.
002511
002512     if ws-comp-id = 'AHL'
002513        move cm-ah-class-cd      to cp-class-code
002514        if cp-class-code = spaces
002515           move zeros            to cp-class-code
002516        end-if
002517     else
002518        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
002519     end-if
002520
002521     MOVE CM-AH-BENEFIT-CD       TO  CP-BENEFIT-CD.
002522     MOVE CM-AH-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
002523                                     CP-RATING-BENEFIT-AMT.
002524     IF CP-STATE-STD-ABBRV = 'OR'
002525         COMPUTE CP-RATING-BENEFIT-AMT = CM-AH-BENEFIT-AMT *
002526                                         CM-AH-ORIG-TERM.
002527     MOVE CM-AH-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
002528     MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY.
002529     MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
002530     MOVE CM-AH-DEV-CODE         TO  CP-DEVIATION-CODE.
002531     MOVE CM-AH-DEV-PCT          TO  CP-RATE-DEV-PCT.
002532     MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
002533
002534     MOVE WS-CF-CR-R78-METHOD    TO  CP-R78-OPTION.
002535     MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
002536     MOVE ' '                    TO  DC-OPTION-CODE.
002537
002538     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
002539
002540     IF CP-STATE-STD-ABBRV = 'OH'
002541        IF WS-COMP-ID NOT = 'NCL' AND 'CID'
002542         IF (CP-ORIGINAL-TERM > 60)
002543           AND (DC-GREG-DATE-1-YMD > '831101')
002544           AND (CM-LF-BENEFIT-CD  IS NOT EQUAL TO  ZERO)
002545             MOVE '6'            TO  CP-EARNING-METHOD
002546         END-IF
002547        END-IF
002548        IF WS-COMP-ID = 'CID'
002549           IF CM-LF-BENEFIT-CD = (SPACES OR ZEROS OR
002550                           LOW-VALUES)
002551              IF CP-CRITICAL-PERIOD
002552                 MOVE '2'       TO CP-EARNING-METHOD
002553              ELSE
002554                 MOVE '6'       TO CP-EARNING-METHOD
002555              END-IF
002556           ELSE
002557              IF WS-CF-LF-COVERAGE-TYPE = 'L'
002558                 MOVE '2'       TO CP-EARNING-METHOD
002559              ELSE
002560                 IF ((CM-LF-ORIG-TERM > 60) AND
002561                    (CM-RATE-CLASS NOT = 'L '))
002562                               OR
002563                    (WS-LF-CO-EARNINGS-CALC = '5')
002564                    IF CP-CRITICAL-PERIOD
002565                       MOVE '2'  TO CP-EARNING-METHOD
002566                    ELSE
002567                       MOVE '6'  TO CP-EARNING-METHOD
002568                    END-IF
002569                 ELSE
002570                    MOVE '1'     TO CP-EARNING-METHOD
002571                 END-IF
002572              END-IF
002573           END-IF
002574        END-IF
002575     END-IF
002576
002577     IF CP-STATE-STD-ABBRV = 'VA'
002578       IF WS-COMP-ID NOT = 'NCL' AND 'CID'
002579         IF DC-GREG-DATE-1-YMD > '921231'
002580            IF CP-ORIGINAL-TERM > 61
002581                MOVE '6'            TO  CP-EARNING-METHOD
002582            ELSE
002583                MOVE '1'            TO  CP-EARNING-METHOD.
002584
002585*    IF CP-EARN-AS-NET-PAY
002586*       IF CP-LOAN-APR <= ZEROS
002587*          IF WS-CF-DEFAULT-APR > ZEROS
002588*             MOVE WS-CF-DEFAULT-APR
002589*                                TO  CP-LOAN-APR
002590*          END-IF
002591*       END-IF
002592*    END-IF
002593
002594     IF WS-COMP-ID = 'DCC' or 'VPP'
002595        IF (WS-AH-BEN-CATEGORY = 'G' OR 'L')
002596           AND (CP-EARNING-METHOD NOT = 'G' AND 'D')
002597           MOVE 'S'              TO CP-EARNING-METHOD
002598        END-IF
002599     END-IF
002600
002601     MOVE WS-CANCEL-REASON       TO CP-CANCEL-REASON
002602
002603     IF (WS-COMP-ID = 'DCC' or 'VPP')
002604        AND (CP-EARNING-METHOD = 'D')
002605        MOVE +0                  TO WS-DDF-ADMIN-FEES
002606                                    WS-DDF-CSO-ADMIN-FEE
002607                                    WS-DDF-1ST-YR-TOT-EXP
002608                                    WS-DDF-COMM-AND-MFEE
002609        PERFORM VARYING S1 FROM +2 BY +1 UNTIL
002610           S1 > +10
002611           IF AM-COM-TYP (S1) = 'L' OR 'N' OR 'J' OR 'I'
002612              IF (AM-A-COM (S1) NUMERIC)
002613                 AND (AM-A-COMA (S1) (3:1) NOT = 'L' AND 'M')
002614                 COMPUTE WS-COMM-PCT = (AM-A-COM (S1) * +1000)
002615              ELSE
002616                 MOVE +0         TO WS-COMM-PCT C0
002617                 PERFORM 0740-GET-ERCTBL THRU 0740-EXIT
002618                 COMPUTE WS-COMM-PCT = WS-COMM-PCT * +1000
002619              END-IF
002620              IF AM-COM-TYP (S1) = 'L' OR 'N'
002621                 COMPUTE WS-DDF-ADMIN-FEES = WS-DDF-ADMIN-FEES
002622                    + WS-COMM-PCT
002623              END-IF
002624              IF AM-COM-TYP (S1) = 'N'
002625                 COMPUTE WS-DDF-CSO-ADMIN-FEE =
002626                    WS-DDF-CSO-ADMIN-FEE + WS-COMM-PCT
002627              END-IF
002628              IF AM-COM-TYP (S1) = 'J' OR 'L'
002629                 COMPUTE WS-DDF-1ST-YR-TOT-EXP
002630                    = WS-DDF-1ST-YR-TOT-EXP + WS-COMM-PCT
002631              END-IF
002632              IF AM-COM-TYP (S1) = 'I'
002633                 COMPUTE WS-DDF-COMM-AND-MFEE
002634                    = WS-DDF-COMM-AND-MFEE + WS-COMM-PCT
002635              END-IF
002636           END-IF
002637        END-PERFORM
002638        MOVE WS-DDF-CSO-ADMIN-FEE TO CP-DDF-CSO-ADMIN-FEE
002639        MOVE WS-DDF-ADMIN-FEES   TO CP-DDF-ADMIN-FEES
002640        COMPUTE WS-DDF-COMM-AND-MFEE = WS-DDF-COMM-AND-MFEE +
002641           (CM-AH-PREMIUM-AMT - CM-AH-CLP - CM-ADDL-CLP)
002642     END-IF
002643
002644     IF (WS-COMP-ID = 'DCC' or 'VPP')
002645        AND (CP-EARNING-METHOD = 'D')
002646        AND (ws-CANCEL-REASON NOT = 'R')
002647        PERFORM 0730-GET-DDF-FACTORS
002648                                 THRU 0730-EXIT
002649
002650        IF NOT PDEF-FOUND
002651
002652           MOVE ' DDF UE FACTORS  NOT FOUND '
002653                                 TO WS-COMMENT
002654           MOVE 'X'              TO WS-LF-STATUS WS-AH-STATUS
002655           PERFORM 0030-BUILD-BUFFER
002656                                 THRU 0030-EXIT
002657           PERFORM 0020-SEND-BUFFER
002658                                 THRU 0020-EXIT
002659           PERFORM 0025-CLOSE-SOCKET
002660                                 THRU 0025-EXIT
002661           GO TO 0010-RETURN
002662        END-IF
002663
002664*       IF NOT PDEF-FOUND
002665*          MOVE ER-9999          TO EMI-ERROR
002666*          PERFORM 9700-ERROR-FORMAT
002667*                                THRU 9799-EXIT
002668*          GO TO 8200-SEND-DATAONLY
002669*       END-IF
002670
002671        MOVE PD-UEP-FACTOR (P1 P2 + 1)
002672                                 TO CP-DDF-LO-FACT
002673        MOVE PD-UEP-FACTOR (P1 P2)
002674                                 TO CP-DDF-HI-FACT
002675        MOVE WS-DDF-COMM-AND-MFEE TO CP-DDF-COMM-AND-MFEE
002676        MOVE CM-AH-CLP           TO CP-DDF-CLP
002677        MOVE PD-1ST-YR-ADMIN-ALLOW TO CP-DDF-YR1AF
002678        COMPUTE CP-1ST-YR-ALLOW = WS-DDF-1ST-YR-TOT-EXP
002679           + PD-1ST-YR-ADMIN-ALLOW
002680
002681        MOVE 'G'                 TO CP-DDF-SPEC-CALC
002682*       IF PI-CLP-YN = 'Y'
002683*          MOVE 'C'              TO CP-DDF-SPEC-CALC
002684*          MOVE CM-AH-CLP        TO CP-ORIGINAL-PREMIUM
002685*                                   CP-DDF-CLP
002686*          MOVE ZEROS            TO CP-1ST-YR-ALLOW
002687*       END-IF
002688
002689        IF DD-IU-PRESENT
002690           MOVE 'I'              TO CP-EARNING-METHOD
002691        END-IF
002692
002693        MOVE CM-DDF-IU-RATE-UP   TO CP-IU-RATE-UP
002694                                    CP-CLP-RATE-UP
002695
002696        IF (CP-CALC-GROSS-FEE)
002697           AND (CP-IU-RATE-UP NOT = ZEROS)
002698           COMPUTE TEX-FACT-8 = 1 - ((CM-ADDL-CLP + CM-AH-CLP)
002699              / CP-ORIGINAL-PREMIUM)
002700           COMPUTE CP-IU-RATE-UP ROUNDED = CP-IU-RATE-UP
002701              / (1 - TEX-FACT-8)
002702        END-IF
002703
002704     END-IF
002705
002706     PERFORM 0720-LINK-REFUND  THRU  0720-EXIT
002707
002708     IF (CP-ERROR-RATE-IS-ZERO)
002709        OR (CP-ERROR-RATE-NOT-FOUND)
002710        OR (CP-ERROR-RATE-FILE-NOTOPEN)
002711        STRING ' ERROR WITH AH REFUND ' CP-RETURN-CODE
002712          DELIMITED BY SIZE INTO WS-COMMENT
002713        END-STRING
002714        IF WS-AH-STATUS NOT = 'H'
002715           MOVE 'X'                 TO WS-AH-STATUS
002716        END-IF
002717        GO TO 0500-EXIT
002718*       PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
002719*       IF NOT ALREADY-SENT
002720*          PERFORM 0020-SEND-BUFFER
002721*                                THRU 0020-EXIT
002722*          SET ALREADY-SENT      TO TRUE
002723*       END-IF
002724*       PERFORM 0025-CLOSE-SOCKET
002725*                                THRU 0025-EXIT
002726*       GO TO 0010-RETURN
002727     END-IF
002728
002729     MOVE CP-CALC-REFUND         TO WS-AH-REFUND
002730                                    WS-TOT-AH-RFND
002731     MOVE CP-REFUND-TYPE-USED    TO WS-AH-METHOD
002732
002733     IF ((WS-AH-STATUS = SPACES)
002734        OR (CP-CALC-REFUND > ZEROS))
002735                  AND
002736          (WS-AH-STATUS NOT = 'H')
002737        MOVE 'A'                 TO WS-AH-STATUS
002738     END-IF
002739
002740     IF WS-COMP-ID = 'DCC' or 'VPP'
002741        AND (WS-AH-BEN-CATEGORY = 'G' OR 'L')
002742        AND (CP-EARNING-METHOD = 'D' OR 'I')
002743        MOVE CM-DDF-IU-RATE-UP   TO CP-IU-RATE-UP
002744        MOVE 'C'                 TO CP-DDF-SPEC-CALC
002745        MOVE CM-AH-CLP           TO CP-ORIGINAL-PREMIUM
002746        MOVE ZEROS               TO CP-1ST-YR-ALLOW
002747        PERFORM 0720-LINK-REFUND THRU  0720-EXIT
002748        MOVE CP-CALC-REFUND      TO ws-ah-rfnd-clp
002749        DISPLAY ' CALC CLP   ' CP-CALC-REFUND
002750     END-IF
002751
002752     EVALUATE TRUE
002753        WHEN CP-R-AS-R78
002754           MOVE 'RULE 78'          TO  WS-AH-METHOD
002755        WHEN CP-R-AS-PRORATA
002756           MOVE 'PRORATA'          TO  WS-AH-METHOD
002757        WHEN CP-REFUND-TYPE-USED = '3'
002758           MOVE 'CALIF'            TO  WS-AH-METHOD
002759        WHEN CP-R-AS-TEXAS
002760           MOVE 'IRREG/FARM PLAN'  TO  WS-AH-METHOD
002761        WHEN CP-R-AS-FARM-PLAN
002762           MOVE 'IRREG/FARM PLAN'  TO  WS-AH-METHOD
002763        WHEN CP-R-AS-NET-PAY
002764           MOVE 'NET PAY'          TO  WS-AH-METHOD
002765        WHEN CP-R-AS-ANTICIPATION
002766           MOVE 'ANTICIPATION'     TO  WS-AH-METHOD
002767        WHEN CP-R-AS-MEAN
002768           MOVE 'MEAN'             TO  WS-AH-METHOD
002769        WHEN CP-R-AS-SUM-OF-DIGITS
002770           MOVE 'SUM OF DIGITS'    TO  WS-AH-METHOD
002771        WHEN CP-GAP-ACTUARIAL
002772           MOVE 'SP ACTUARIAL'     TO WS-AH-METHOD
002773        WHEN CP-R-AS-REPOSSESSION
002774           MOVE 'REPOSSESSION'     TO WS-AH-METHOD
002775     END-EVALUATE
002776
002777     IF WS-TOT-AH-RFND > ZEROS
002778        IF (ws-comp-id = 'DCC' or 'VPP')
002779           AND (AM-DCC-PRODUCT-CODE = 'DDF')
002780           compute ws-work-factor rounded =
002781              ws-ah-rfnd-clp / cm-ah-clp
002782        else
002783           COMPUTE WS-WORK-FACTOR ROUNDED =
002784              WS-TOT-AH-RFND / WS-TOT-AH-PREM
002785        end-if
002786        COMPUTE WS-AH-UEC ROUNDED = WS-WORK-FACTOR *
002787           WS-TOT-AH-COMM
002788     END-IF
002789
002790     IF ERACCT-FOUND
002791        IF AM-COMM-CHARGEBACK (1) NOT NUMERIC
002792           MOVE ZEROS            TO AM-COMM-CHARGEBACK (1)
002793        END-IF
002794        IF AM-COMM-CHARGEBACK (1) NOT = ZEROS
002795           IF (AM-COMM-CHARGEBACK (1) = 99)
002796                     OR
002797              (WS-NCB-DIFF-MONTHS > AM-COMM-CHARGEBACK (1))
002798                     OR
002799              ((WS-NCB-DIFF-MONTHS = AM-COMM-CHARGEBACK (1))
002800              AND (WS-NCB-DIFF-ODD-DAYS > ZEROS))
002801              MOVE ZEROS         TO WS-AH-UEC
002802           END-IF
002803        END-IF
002804     END-IF
002805
002806     .
002807 0500-EXIT.
002808     EXIT.
002809
002810 0610-GET-ERMAIL.
002811
002812     move cm-control-primary     to ws-ma-key
002813*    MOVE WS-CM-KEY              TO WS-MA-KEY
002814     
      * EXEC CICS READ
002815*       INTO    (MAILING-DATA)
002816*       DATASET ('ERMAIL')
002817*       RIDFLD  (WS-MA-KEY)
002818*       RESP    (WS-RESPONSE)
002819*    END-EXEC
           MOVE LENGTH OF
            MAILING-DATA
             TO DFHEIV11
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00006759' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303036373539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 MAILING-DATA, 
                 DFHEIV11, 
                 WS-MA-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002820
002821     IF RESP-NORMAL
002822        MOVE MA-CRED-BENE-NAME   to ws-cred-bene-name
002823     else
002824        move spaces              to ws-cred-bene-name
002825     end-if
002826
002827     .
002828 0610-EXIT.
002829     EXIT.
002830
002831 0620-GET-ELCRTT.
002832
002833     move cm-control-primary     to ws-cs-key
002834*    MOVE WS-CM-KEY              TO WS-CS-KEY
002835     move 'C'                    to WS-CS-TRLR-TYPE
002836
002837     
      * EXEC CICS READ
002838*       INTO    (CERTIFICATE-TRAILERS)
002839*       DATASET ('ELCRTT')
002840*       RIDFLD  (WS-CS-KEY)
002841*       RESP    (WS-RESPONSE)
002842*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00006782' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303036373832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 WS-CS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002843
002844     IF RESP-NORMAL
002845        MOVE cs-vin-number       to ws-vin
002846     else
002847        move spaces              to ws-vin
002848     end-if
002849
002850     .
002851 0620-EXIT.
002852     EXIT.
002853
002854 0700-LINK-REM-TERM.
002855     
      * EXEC CICS LINK
002856*        PROGRAM   ('ELRTRM')
002857*        COMMAREA  (CALCULATION-PASS-AREA)
002858*        LENGTH    (CP-COMM-LENGTH)
002859*    END-EXEC.
           MOVE 'ELRTRM' TO DFHEIV1
      *    MOVE '."C                   (   #00006800' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036383030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002860
002861 0700-EXIT.
002862     EXIT.
002863
002864 0710-LINK-REM-AMOUNT.
002865
002866     
      * EXEC CICS LINK
002867*        PROGRAM   ('ELRAMT')
002868*        COMMAREA  (CALCULATION-PASS-AREA)
002869*        LENGTH    (CP-COMM-LENGTH)
002870*    END-EXEC
           MOVE 'ELRAMT' TO DFHEIV1
      *    MOVE '."C                   (   #00006811' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036383131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002871
002872     .
002873 0710-EXIT.
002874     EXIT.
002875
002876 0720-LINK-REFUND.
002877
002878     
      * EXEC CICS LINK
002879*        PROGRAM   ('ELRFND')
002880*        COMMAREA  (CALCULATION-PASS-AREA)
002881*        LENGTH    (CP-COMM-LENGTH)
002882*    END-EXEC
           MOVE 'ELRFND' TO DFHEIV1
      *    MOVE '."C                   (   #00006823' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036383233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002883
002884     .
002885 0720-EXIT.
002886     EXIT.
002887
002888 0730-GET-DDF-FACTORS.
002889
002890     MOVE ' '                    TO WS-PDEF-RECORD-SW
002891
002892     MOVE WS-COMP-CD             TO ERPDEF-KEY
002893     MOVE CM-STATE               TO ERPDEF-STATE
002894     if cm-clp-state not = cm-state and spaces and zeros
002895        move cm-clp-state        to erpdef-state
002896     end-if
002897     MOVE AM-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
002898     MOVE 'A'                    TO ERPDEF-BEN-TYPE
002899     MOVE CM-AH-BENEFIT-CD       TO ERPDEF-BEN-CODE
002900     MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
002901     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
002902
002903     
      * EXEC CICS STARTBR
002904*        DATASET  ('ERPDEF')
002905*        RIDFLD   (ERPDEF-KEY)
002906*        GTEQ
002907*        RESP     (WS-RESPONSE)
002908*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006848' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303036383438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002909
002910     IF NOT RESP-NORMAL
002911        GO TO 0730-EXIT
002912     END-IF
002913
002914     .
002915 0730-READNEXT.
002916
002917     
      * EXEC CICS READNEXT
002918*       DATASET  ('ERPDEF')
002919*       INTO     (PRODUCT-MASTER)
002920*       RIDFLD   (ERPDEF-KEY)
002921*       RESP     (WS-RESPONSE)
002922*    END-EXEC
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV12
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00006862' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303036383632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PRODUCT-MASTER, 
                 DFHEIV12, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002923
002924     IF NOT RESP-NORMAL
002925        GO TO 0730-ENDBR
002926     END-IF
002927
002928     IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
002929        IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
002930           MOVE 'Y'              TO WS-PDEF-RECORD-SW
002931        ELSE
002932           GO TO 0730-READNEXT
002933        END-IF
002934     ELSE
002935        GO TO 0730-ENDBR
002936     END-IF
002937
002938     PERFORM VARYING P1 FROM +1 BY +1 UNTIL
002939        (P1 > +11)
002940        OR (PD-PROD-CODE (P1) = 'I')
002941     END-PERFORM
002942     IF P1 < +12
002943        SET DD-IU-PRESENT        TO TRUE
002944     END-IF
002945
002946     IF CM-LOAN-TERM = ZEROS
002947        MOVE CP-ORIGINAL-TERM    TO CP-LOAN-TERM
002948     END-IF
002949
002950     IF PD-TRUNCATED = 'Y'
002951        MOVE CM-LOAN-TERM        TO WS-TERM
002952     ELSE
002953        MOVE CP-ORIGINAL-TERM    TO WS-TERM
002954     END-IF
002955
002956     EVALUATE TRUE
002957        WHEN WS-TERM > +168
002958           MOVE 15               TO P1
002959        WHEN WS-TERM > +156
002960           MOVE 14               TO P1
002961        WHEN WS-TERM > +144
002962           MOVE 13               TO P1
002963        WHEN WS-TERM > +132
002964           MOVE 12               TO P1
002965        WHEN WS-TERM > +120
002966           MOVE 11               TO P1
002967        WHEN WS-TERM > +108
002968           MOVE 10               TO P1
002969        WHEN WS-TERM > +96
002970           MOVE 9                TO P1
002971        WHEN WS-TERM > +84
002972           MOVE 8                TO P1
002973        WHEN WS-TERM > +72
002974           MOVE 7                TO P1
002975        WHEN WS-TERM > +60
002976           MOVE 6                TO P1
002977        WHEN WS-TERM > +48
002978           MOVE 5                TO P1
002979        WHEN WS-TERM > +36
002980           MOVE 4                TO P1
002981        WHEN WS-TERM > +24
002982           MOVE 3                TO P1
002983        WHEN WS-TERM > +12
002984           MOVE 2                TO P1
002985        WHEN OTHER
002986           MOVE 1                TO P1
002987     END-EVALUATE
002988
002989     EVALUATE TRUE
002990*       WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +13)
002991*          AND (DD-IU-PRESENT)
002992*          MOVE 2                TO P2
002993*       WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +13
002994*          MOVE 1                TO P2
002995        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +25
002996           MOVE 2                TO P2
002997        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +37
002998           MOVE 3                TO P2
002999        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +49
003000           MOVE 4                TO P2
003001        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +61
003002           MOVE 5                TO P2
003003        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +73
003004           MOVE 6                TO P2
003005        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +85
003006           MOVE 7                TO P2
003007        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +97
003008           MOVE 8                TO P2
003009        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +109
003010           MOVE 9                TO P2
003011        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +121
003012           MOVE 10               TO P2
003013        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +133
003014           MOVE 11               TO P2
003015        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +145
003016           MOVE 12               TO P2
003017        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +157
003018           MOVE 13               TO P2
003019        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +169
003020           MOVE 14               TO P2
003021        WHEN OTHER
003022           MOVE 15               TO P2
003023     END-EVALUATE
003024
003025     .
003026 0730-ENDBR.
003027
003028     
      * EXEC CICS ENDBR
003029*       DATASET  ('ERPDEF')
003030*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006973' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036393733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003031
003032     .
003033 0730-EXIT.
003034     EXIT.
003035
003036 0740-GET-ERCTBL.
003037
003038     MOVE AM-COMPANY-CD          TO CTBL-COMPANY-CD
003039     MOVE AM-A-COMA (S1)         TO CTBL-TABLE
003040     MOVE 'A'                    TO CTBL-BEN-TYPE
003041     MOVE CM-AH-BENEFIT-CD       TO CTBL-BEN-CODE
003042     MOVE CTBL-KEY               TO CTBL-KEY-SAVE
003043
003044     PERFORM 0750-READ-ERCTBL    THRU 0750-EXIT
003045     IF RESP-NORMAL
003046        PERFORM 0760-FIND-COMM   THRU 0760-EXIT
003047     ELSE
003048        MOVE AM-COMPANY-CD          TO CTBL-COMPANY-CD
003049        MOVE AM-A-COMA (S1)         TO CTBL-TABLE
003050        MOVE 'A'                    TO CTBL-BEN-TYPE
003051        MOVE 'AA'                   TO CTBL-BEN-CODE
003052        MOVE CTBL-KEY               TO CTBL-KEY-SAVE
003053
003054        PERFORM 0750-READ-ERCTBL    THRU 0750-EXIT
003055        IF RESP-NORMAL
003056           PERFORM 0760-FIND-COMM   THRU 0760-EXIT
003057        END-IF
003058     END-IF
003059
003060     .
003061 0740-EXIT.
003062     EXIT.
003063
003064 0750-READ-ERCTBL.
003065
003066     
      * EXEC CICS READ
003067*         INTO    (COMM-TABLE-RECORD)
003068*         DATASET ('ERCTBL')
003069*         RIDFLD  (CTBL-KEY)
003070*         RESP    (WS-RESPONSE)
003071*    END-EXEC
           MOVE LENGTH OF
            COMM-TABLE-RECORD
             TO DFHEIV11
           MOVE 'ERCTBL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00007011' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037303131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 COMM-TABLE-RECORD, 
                 DFHEIV11, 
                 CTBL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003072
003073     .
003074 0750-EXIT.
003075     EXIT.
003076
003077 0760-FIND-COMM.
003078
003079     PERFORM VARYING C1 FROM +1 BY +1 UNTIL
003080        ((CM-AH-BENEFIT-AMT * CM-AH-ORIG-TERM) <= CT-TBF (C1))
003081        OR (C1 > +3)
003082     END-PERFORM
003083
003084     PERFORM VARYING C2 FROM +1 BY +1 UNTIL
003085        (CM-INSURED-ISSUE-AGE <= CT-AGE (C2))
003086        OR (C2 > +3)
003087     END-PERFORM
003088
003089     PERFORM VARYING C3 FROM +1 BY +1 UNTIL
003090        (CM-AH-ORIG-TERM <= CT-TRM (C3))
003091        OR (C3 > +3)
003092     END-PERFORM
003093
003094     IF C1 > +3
003095        MOVE +1                  TO C1
003096     END-IF
003097     IF C2 > +3
003098        MOVE +1                  TO C2
003099     END-IF
003100     IF C3 > +3
003101        MOVE +1                  TO C3
003102     END-IF
003103
003104     IF C1 = +3
003105        MOVE +18                 TO C0
003106     ELSE
003107        IF C1 = +2
003108           MOVE +9               TO C0
003109        END-IF
003110     END-IF
003111
003112     IF C2 = +3
003113        ADD +6                   TO C0
003114     ELSE
003115        IF C2 = +2
003116           ADD +3                TO C0
003117        END-IF
003118     END-IF
003119
003120     ADD C3                      TO C0
003121
003122     MOVE CT-RT (C0)             TO WS-COMM-PCT
003123
003124     .
003125 0760-EXIT.
003126     EXIT.
003127
003128 9700-DATE-LINK.
003129
003130     
      * EXEC CICS LINK
003131*         PROGRAM  ('ELDATCV')
003132*         COMMAREA (DATE-CONVERSION-DATA)
003133*         LENGTH   (DC-COMM-LENGTH)
003134*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00007075' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037303735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003135
003136 9700-EXIT.
003137      EXIT.
003138

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK03' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK03' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
