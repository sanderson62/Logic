      *****************************************************************
      *                                                               *
      * Copyright (c) 2001 by Sun Microsystems, Inc.                  *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. SOCK03.
      *         This program is the first invoked by the SOC3
      *         transaction. This transaction must be submited
      *         by a message transmited over a stream socket.
      *         The message format is:
      *         xxxx,12345678901234567890123456789012345
      *         where "xxxx" is the tran id SOCK (1 to 4 characters)
      *         and "123456.........5" is up to 35 characters of
      *         optional text.
      *         Information on the originating message and
      *         remote socket is provided in the DFHCOMMAREA.
      *         As its first action this program must send a
      *         message to the requesting socket. This lets the
      *         remote program know its request has been
      *         accepted and the transaction started.
      *         The program will make four transmissions to
      *         the initating program in response to received
      *         messages and then close the socket.
      *         This program is provided to show method and
      *         has minimal checking and error messages.
      *         The Cobol compiler, unless instructed otherwise,
      *         will hold its working storage integer data items in
      *         machine idependent form. This matches the network
      *         ordering of information. Care should be exercised
      *         to ensure that this data ordering is correct when
      *         calling system library functions or passing data
      *         structure containing such data to functions.
071112******************************************************************
071112*                   C H A N G E   L O G
071112*
071112* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
071112*-----------------------------------------------------------------
071112*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
071112* EFFECTIVE    NUMBER
071112*-----------------------------------------------------------------
071112* 071112  IR2012042700001  PEMA  AHL CHANGES
081612* 081612  CR2011062300001  PEMA  REMOVE EXT DAY CHG FOR REFUNDS
010213* 010213  CR2011083000005  PEMA  ADD SPECIAL DCC SPP DDF REFUND
101813* 101813  IR2013101700003  PEMA  FIX MN Net Balloon calc.
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
072415* 072415  IR2015071500003  PEMA  REMOVE CLOSE STATEMENT
010816* 010816  IR2015092900001  PEMA  USE CLP STATE WHERE NEEDED
071117*071117   IR2017061900003  PEMA  ALLOW 0% APR ON REFUNDS
100417* 100417  CR2017051000002  PEMA  Add fields to interface
100418* 100418  CR2018073000001  PEMA  REWORD REFUND METHODS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
052319* 052319  IR2019052300001  PEMA  VIN&BENE not being passed at time
062119* 062119  CR2019050800001  PEMA  Add option to search by VIN
080519* 080519  IR2019080200001  PEMA  Allow multiple quotes by VIN
080919* 080919  IR2019080900001  PEMA  Shorten allowed Linkage Section
041720* 041720  IR2020041700001  PEMA  Change to not drop cert suffix.
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
080322* 080322  CR2021100800003  TANA  Add B and H claim types
071112******************************************************************
       environment division.
       data division.
       working-storage section.
       01  DFH-START PIC X(04).
      *
      * program buffers
      *
       77  WS-BUFF-SENT-SW             PIC X  VALUE SPACES.
           88  ALREADY-SENT              VALUE 'Y'.
       77  WS-LENGTH                   PIC S9(4) COMP VALUE +0.
       77  WS-ELCERT-SW                PIC X  VALUE SPACES.
           88  ELCERT-FINISHED            VALUE 'Y'.
       77  WS-BIN-EFF-DT               PIC XX VALUE LOW-VALUES.
       77  WS-ERACCT-SW                PIC X VALUE SPACES.
           88  ERACCT-FOUND               VALUE 'Y'.
       77  WS-FREE-LOOK                PIC S999 COMP-3 VALUE +0.
       77 ws-send-msg-size           pic s9(8) comp value 4096.
       77 ws-recv-msg-size           pic s9(8) comp value 4096.
       77 ws-recv-buf                pic x(4096).
       77 ws-send-buf                pic x(4096) VALUE SPACES.
       77 ws-recv-total              pic s9(8) comp value 0.
       77 ws-recv-left               pic s9(8) comp value 0.
       77 ws-seq-num                 pic s9(8) comp value 0.
       77 ws-flags                   pic s9(8) comp value 0.
       77 WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
       77  WS-COMP-ID                  PIC XXX  VALUE SPACES.
       77 WS-SAVE-ACCOUNT              PIC X(10)  VALUE SPACES.
       77 WS-BIN-ORIG-EFF-DT           PIC XX  VALUE LOW-VALUES.
       77 WS-ORIG-EFF-DT               PIC X(10)  VALUE SPACES.
       77 WS-EFF-DATE                  PIC X(10)  VALUE SPACES.
       77 WS-EXP-DATE                  PIC X(10)  VALUE SPACES.
       77  ws-cancel-reason            pic x   value spaces.
       77  P1                          PIC S999 COMP-3 VALUE +0.
       77  P2                          PIC S999 COMP-3 VALUE +0.
       77  C0                          PIC S999 COMP-3 VALUE +0.
       77  C1                          PIC S999 COMP-3 VALUE +0.
       77  C2                          PIC S999 COMP-3 VALUE +0.
       77  C3                          PIC S999 COMP-3 VALUE +0.
       77 S1                           PIC S999 COMP-3 VALUE +0.
       77 S2                           PIC S999 COMP-3 VALUE +0.
       77 L1                           PIC S999 COMP-3 VALUE +0.
       77 L2                           PIC S999 COMP-3 VALUE +0.
       77 F1                           PIC S999 COMP-3 VALUE +0.
       77 WS-BUILD-SW                  PIC X.
          88  TIME-TO-BUILD               VALUE 'Y'.
       77 WS-SAVE-ERACCT               PIC X(2000).
       77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
       77 WS-STOP-SW                   PIC X  VALUE ' '.
          88  TOLD-TO-STOP               VALUE 'Y'.
       77 WS-PERFORM-SW                PIC X VALUE SPACES.
          88  GET-RATES                    VALUE 'R'.
          88  GET-ACT-ACCTS                VALUE 'A'.
       77 ws-bin-1st-pay-dt            pic xx  value low-values.
       77 WS-DISP-AMT                  PIC Z,ZZZ,Z99.99.
       77 ws-disp-rate                 pic z9.99999.
       77  WS-ERCTBL-SW                PIC X VALUE ' '.
           88  END-OF-ERCTBL                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       77  WS-CF-DEFAULT-APR           PIC S9(03)V9(04) COMP-3.
       77  WS-CF-CR-R78-METHOD         PIC X          VALUE SPACE.
       77  WS-CF-CR-REM-TERM-CALC      PIC X          VALUE SPACE.
       77  WS-BIN-VAL-DT               PIC XX         VALUE LOW-VALUES.
       77  WS-TOT-LF-RFND              PIC S9(7)V99  COMP-3 VALUE +0.
       77  WS-TOT-AH-RFND              PIC S9(7)V99  COMP-3 VALUE +0.
       77  WS-TOT-LF-PREM              PIC S9(7)V99  COMP-3 VALUE +0.
       77  WS-TOT-AH-PREM              PIC S9(7)V99  COMP-3 VALUE +0.
       77  WS-TOT-LF-COMM              PIC S9(7)V99  COMP-3 VALUE +0.
       77  WS-TOT-AH-COMM              PIC S9(7)V99  COMP-3 VALUE +0.
       77  WS-WORK-FACTOR              PIC S9V9(7)   COMP-3 VALUE +0.
       77  WS-ELCERT-KEY-SW            PIC X  VALUE SPACES.
           88  WS-ELCERT-FULL             VALUE '1'.
           88  WS-ELCERT-NAME             VALUE '2'.
           88  WS-ELCERT-CERT-NO          VALUE '3'.
           88  WS-ELCERT-ACT-NAME         VALUE '4'.
062119     88  WS-ELCERT-VIN              value '5'.
       77  ELMSTR-LENGTH               PIC S9(4) COMP VALUE +12.
       77  WS-CLM-STOP-SW              PIC X  VALUE ' '.
           88  I-SAY-TO-STOP      VALUE 'Y'.
       77  WS-HEX-0A                   PIC X.
       77  WS-CALL-TYPE                PIC X(6).
       77  WS-NCB-DIFF-MONTHS          PIC 999   VALUE ZEROS.
       77  WS-NCB-DIFF-ODD-DAYS        PIC 999   VALUE ZEROS.
       77  WS-STATE-EXT-DAYS-CHG       PIC X  VALUE ' '.
       77  DD-IU-SW                    PIC X   VALUE ' '.
           88  DD-IU-PRESENT                 VALUE 'Y'.
       77  WS-DDF-COMM-AND-MFEE        PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-ADMIN-FEES           PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-CSO-ADMIN-FEE        PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-1ST-YR-TOT-EXP       PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-COMM-PCT                 PIC S9(5)V9(5)  COMP-3 VALUE +0.
       77  WS-TERM                     PIC S999 COMP-3 VALUE +0.
       77  ws-ah-rfnd-clp              pic s9(7)v99 comp-3 value +0.
       77  TEX-FACT-8                  PIC S9V9(6)     COMP-3.
       77  WS-PDEF-RECORD-SW           PIC X           VALUE ' '.
           88  PDEF-FOUND                              VALUE 'Y'.
090314 77  ws-epiq-sw                  pic x  value ' '.
090314     88  ws-epiq-request           value 'Y'.
090314 77  ws-epiq-max                 pic s999 value +0 comp-3.
080322 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
       01  WS-STRING-DATA.
           05  WS-COMMENT              PIC X(30)    VALUE SPACES.
           05  WS-RESP                 PIC 99999    VALUE ZEROS.
           05  WS-CARRIER              PIC X        VALUE SPACES.
           05  WS-GROUPING             PIC X(6)     VALUE SPACES.
           05  WS-STATE                PIC XX       VALUE SPACES.
           05  WS-ACCOUNT              PIC X(10)    VALUE SPACES.
           05  WS-CERT-NO              PIC X(11)    VALUE SPACES.
           05  WS-CERT-EFF-DATE        PIC 9(8)     VALUE ZEROS.
           05  WS-LAST-NAME            PIC X(15)    VALUE SPACES.
           05  WS-FIRST-NAME           PIC X(15)    VALUE SPACES.
           05  WS-MID-INIT             PIC X        VALUE SPACES.
           05  WS-POST-CARD-SW         PIC X        VALUE SPACES.
           05  WS-LF-COVERAGE-TYPE     PIC X        VALUE SPACES.
           05  WS-LF-STATUS            PIC X        VALUE SPACES.
           05  WS-LF-BEN-CODE          PIC XX       VALUE SPACES.
           05  WS-LF-BEN-CODE-DESC     PIC X(10)    VALUE SPACES.
           05  WS-LF-TERM              PIC 999      VALUE ZEROS.
           05  WS-LF-REM-TERM          PIC 999      VALUE ZEROS.
           05  WS-LF-PREM              PIC 9(7).99  VALUE ZEROS.
           05  WS-LF-REFUND            PIC 9(7).99  VALUE ZEROS.
           05  WS-LF-METHOD            PIC X(15)    VALUE SPACES.
           05  WS-LF-ORIG-BENEFIT      PIC 9(9).99  VALUE ZEROS.
           05  WS-LF-REM-BEN           PIC 9(9).99  VALUE ZEROS.
           05  WS-AH-COVERAGE-TYPE     PIC X        VALUE SPACES.
           05  WS-AH-STATUS            PIC X        VALUE SPACES.
           05  WS-AH-BEN-CODE          PIC XX       VALUE SPACES.
           05  WS-AH-BEN-CODE-DESC     PIC X(10)    VALUE SPACES.
           05  WS-AH-TERM              PIC 999      VALUE ZEROS.
           05  WS-AH-REM-TERM          PIC 999      VALUE ZEROS.
           05  WS-AH-PREM              PIC 9(7).99  VALUE ZEROS.
           05  WS-AH-REFUND            PIC 9(7).99  VALUE ZEROS.
           05  WS-AH-METHOD            PIC X(15)    VALUE SPACES.
           05  WS-AH-ORIG-BENEFIT      PIC 9(7).99  VALUE ZEROS.
           05  WS-AH-REM-BEN           PIC 9(7).99  VALUE ZEROS.
           05  WS-ACCOUNT-NAME         PIC X(30)    VALUE SPACES.
           05  WS-REPORT-CODE-1        PIC X(10)    VALUE SPACES.
           05  WS-REPORT-CODE-2        PIC X(10)    VALUE SPACES.
           05  WS-REPORT-CODE-3        PIC X(10)    VALUE SPACES.
           05  WS-LF-EXPIRE-DATE       PIC 9(8)     VALUE ZEROS.
           05  WS-AH-EXPIRE-DATE       PIC 9(8)     VALUE ZEROS.
           05  WS-LF-COMM              PIC 9(7).99  VALUE ZEROS.
           05  WS-AH-COMM              PIC 9(7).99  VALUE ZEROS.
           05  WS-LF-UEC               PIC 9(7).99  VALUE ZEROS.
           05  WS-AH-UEC               PIC 9(7).99  VALUE ZEROS.
           05  WS-LAST-JNAME           PIC X(15)    VALUE SPACES.
           05  WS-FIRST-JNAME          PIC X(15)    VALUE SPACES.
           05  WS-MID-JINIT            PIC X        VALUE SPACES.
           05  WS-CLM-PAID-THRU-DT     PIC 9(8)   VALUE ZEROS.
           05  WS-LF-BIN-PAID-THRU-DT  PIC XX     VALUE LOW-VALUES.
           05  WS-AH-BIN-PAID-THRU-DT  PIC XX     VALUE LOW-VALUES.
100417     05  WS-LF-CANCEL-DATE       PIC 9(8)   VALUE ZEROS.
100417     05  WS-AH-CANCEL-DATE       PIC 9(8)   VALUE ZEROS.
100417     05  WS-VIN                  PIC X(17)  VALUE SPACES.
100417     05  WS-CRED-BENE-NAME       PIC X(30)  VALUE SPACES.
       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).
       01  FILLER.
           12  WS-CF-REM-TRM-CALC-OPTION PIC X  VALUE SPACE.
           12  WS-BALLOON-RTRM         PIC 999  VALUE ZEROS.
           12  WS-STATE-ABBREVIATION   PIC XX.
           12  WS-ACCT-USER-FLD-5      PIC  X(02).
           12  WS-AH-SPECIAL-CALC-CD   PIC  X(01)      VALUE SPACE.
           12  WS-AH-BEN-CATEGORY      PIC  X          VALUE SPACE.
           12  WS-LF-SPECIAL-CALC-CD   PIC  X(01)      VALUE SPACE.
           12  WS-AH-CO-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
           12  WS-LF-CO-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
           12  WS-AH-CO-EARNINGS-CALC  PIC  X(01)      VALUE SPACE.
           12  WS-LF-CO-EARNINGS-CALC  PIC  X(01)      VALUE SPACE.
           12  WS-AH-CO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
           12  WS-LF-CO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
           12  WS-AH-ST-REFUND-CALC    PIC  X(01)      VALUE SPACE.
           12  WS-LF-ST-REFUND-CALC    PIC  X(01)      VALUE SPACE.
           12  WS-AH-FO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
           12  WS-LF-FO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
           12  WS-AH-ST-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
           12  WS-LF-ST-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
           12  WS-AH-AM-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
           12  WS-LF-AM-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
           12  WS-AM-EARN-METHOD-A     PIC  X(01)      VALUE SPACE.
           12  WS-AM-EARN-METHOD-L     PIC  X(01)      VALUE SPACE.
           12  WS-AM-EARN-METHOD-R     PIC  X(01)      VALUE SPACE.
           12  WS-LF-OVERRIDE-L1       PIC  X(01).
           12  WS-LF-OVERRIDE-L2       PIC  X(02).
           12  WS-AH-OVERRIDE-L1       PIC  X(01).
           12  WS-AH-OVERRIDE-L2       PIC  X(02).
           12  WS-CF-LF-COVERAGE-TYPE  PIC  X(01)      VALUE SPACE.
               88  WS-REDUCING                         VALUE 'R'.
               88  WS-LEVEL                            VALUE 'L'  'P'.
       01  ELMSTR-KEY.
           12  ELMSTR-COMP-CD          PIC X.
           12  ELMSTR-CERT-NO          PIC X(11).
       01  CTBL-KEY-SAVE               PIC X(5).
       01  CTBL-KEY.
           05  CTBL-COMPANY-CD         PIC X.
           05  CTBL-TABLE              PIC XXX.
           05  CTBL-BEN-TYPE           PIC X.
           05  CTBL-BEN-CODE           PIC XX.
       01  ERPDEF-KEY-SAVE             PIC X(18).
       01  ERPDEF-KEY.
           12  ERPDEF-COMPANY-CD       PIC X.
           12  ERPDEF-STATE            PIC XX.
           12  ERPDEF-PROD-CD          PIC XXX.
           12  F                       PIC X(7).
           12  ERPDEF-BEN-TYPE         PIC X.
           12  ERPDEF-BEN-CODE         PIC XX.
           12  ERPDEF-EXP-DT           PIC XX.
       01  WS-AM3-KEY.
           12  WS-AM3-ACCOUNT          PIC X(10).
           12  WS-AM3-EXP-DT           PIC XX.
       01  WS-AM-KEY.
           12  WS-AM-COMPANY-CD        PIC X.
           12  WS-AM-CARRIER           PIC X.
           12  WS-AM-GROUPING          PIC X(6).
           12  WS-AM-STATE             PIC XX.
           12  WS-AM-ACCOUNT           PIC X(10).
           12  WS-AM-EXPIRATION-DT     PIC XX.
           12  WS-AM-FILLER            PIC XXXX.
       01  WS-CO-DATA.
           05  WS-CO-NUM               PIC X(10).
           05  WS-CO-PRIMARY-CONTACT   PIC X(30).
           05  WS-CO-NAME              PIC X(30).
           05  WS-CO-MAIL-NAME         PIC X(30).
           05  WS-CO-ADDR1             PIC X(30).
           05  WS-CO-ADDR2             PIC X(30).
           05  WS-CO-ADDR3             PIC X(30).
           05  WS-CO-ZIP               PIC X(9).
           05  WS-CO-PHONE             PIC X(10).
       01  WS-CF-KEY.
           12  WS-CF-COMPANY-ID        PIC  X(03)      VALUE SPACES.
           12  WS-CF-RECORD-TYPE       PIC  X(01)      VALUE ZERO.
      *        88  COMPANY-MASTER                      VALUE '1'.
      *        88  STATE-MASTER                        VALUE '3'.
      *        88  LF-BENEFIT-MASTER                   VALUE '4'.
      *        88  AH-BENEFIT-MASTER                   VALUE '5'.
           12  WS-CF-ACCESS.
               16  WS-CF-STATE         PIC  X(02)      VALUE SPACES.
               16  WS-CF-BENEFIT-NO                    VALUE SPACES.
                   20  FILLER          PIC  X(01).
                   20  WS-CF-CARRIER   PIC  X(01).
           12  WS-CF-SEQUENCE-NO       PIC S9(04) COMP VALUE ZERO.
100417 01  WS-CS-KEY.
100417     05  WS-CS-COMPANY-CD        PIC X.
100417     05  WS-CS-CARRIER           PIC X.
100417     05  WS-CS-GROUP             PIC X(6).
100417     05  WS-CS-STATE             PIC XX.
100417     05  WS-CS-ACCOUNT           PIC X(10).
100417     05  WS-CS-EFF-DT            PIC XX.
100417     05  WS-CS-CERT-NO           PIC X(11).
100417     05  WS-CS-TRLR-TYPE         PIC X.
100417 01  WS-MA-KEY.
100417     05  WS-MA-COMPANY-CD        PIC X.
100417     05  WS-MA-CARRIER           PIC X.
100417     05  WS-MA-GROUP             PIC X(6).
100417     05  WS-MA-STATE             PIC XX.
100417     05  WS-MA-ACCOUNT           PIC X(10).
100417     05  WS-MA-EFF-DT            PIC XX.
100417     05  WS-MA-CERT-NO           PIC X(11).
       01  WS-CM-KEY.
           05  WS-CM-COMPANY-CD        PIC X.
           05  WS-CM-CARRIER           PIC X.
           05  WS-CM-GROUP             PIC X(6).
           05  WS-CM-STATE             PIC XX.
           05  WS-CM-ACCOUNT           PIC X(10).
           05  WS-CM-EFF-DT            PIC XX.
           05  WS-CM-CERT-NO           PIC X(11).
       01  WS-CM-KEY-A1.
           05  WS-CM-COMPANY-CD-A1     PIC X.
           05  WS-CM-LAST-NAME         PIC X(15).
           05  WS-CM-INITIALS          PIC XX.
       01  WS-CM-KEY-A4.
           05  WS-CM-COMPANY-CD-A4     PIC X.
           05  WS-CM-CERT-NO-A4        PIC X(11).
       01  WS-CID-NO                   PIC X(8).
       01  soc-client-in-data.
           05  CLIENT-CAR              PIC X.
           05  CLIENT-GRP              PIC X(6).
           05  CLIENT-STATE            PIC XX.
           05  CLIENT-ACCOUNT          PIC X(10).
062119     05  CLIENT-EFF-DT           PIC X(8).  *> ccyymmdd
           05  CLIENT-CERT-NO          PIC X(11).
           05  CLIENT-LAST-NAME        PIC X(15).
           05  CLIENT-FIRST-NAME       PIC X(10).
062119     05  CLIENT-VAL-DT           PIC X(8).  *> ccyymmdd
           05  CLIENT-CAN-REASON       PIC X.
062119     05  client-vin              pic x(17).
       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-DUPKEY                  VALUE +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.
      *                                 COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
062017         16  CM-REF-INTERFACE-SW           PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
      *                                 COPY ELCMSTR.
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
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
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
022122*            88  hospital-claim         value 'H'.
022122*            88  bereavement-claim      value 'B'.
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
      *                                 COPY ERCPDEF.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCPDEF.                            *
      *                                                                *
      *    FILE DESCRIPTION = PRODUCT DEFINITION MASTER                *
      *                                                                *
      *    FILE TYPE = VSAM,KSDS                                       *
      *    RECORD SIZE = 1319 RECFORM = FIXED                          *
      *                                                                *
      *    BASE CLUSTER = ERPDEF                      RKP=02,LEN=18    *
      *                                                                *
      *    LOG = YES                                                   *
      *    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
051414*                   C H A N G E   L O G
051414*
051414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
051414*-----------------------------------------------------------------
051414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
051414* EFFECTIVE    NUMBER
051414*-----------------------------------------------------------------
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100314* 100314  CR2014061900001  PEMA  ADD PCT OF BENEFIT
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  TANA  Add B and H claim types
      ******************************************************************
       01  PRODUCT-MASTER.
          12  PD-RECORD-ID                 PIC X(02).
              88  VALID-PD-ID                  VALUE 'PD'.
          12  PD-CONTROL-PRIMARY.
              16  PD-COMPANY-CD            PIC X.
              16  PD-STATE                 PIC XX.
              16  PD-PRODUCT-CD            PIC XXX.
              16  PD-FILLER                PIC X(7).
              16  PD-BEN-TYPE              PIC X.
              16  PD-BEN-CODE              PIC XX.
              16  PD-PROD-EXP-DT           PIC XX.
          12  FILLER                       PIC X(6).
          12  PD-PRODUCT-DATA OCCURS 11.
              16  PD-PROD-CODE             PIC X.
                  88  PD-PROD-LIFE           VALUE 'L'.
                  88  PD-PROD-PROP           VALUE 'P'.
                  88  PD-PROD-AH             VALUE 'A'.
                  88  PD-PROD-IU             VALUE 'I'.
                  88  PD-PROD-GAP            VALUE 'G'.
052614            88  PD-PROD-FAML           VALUE 'F'.
100518            88  PD-PROD-OTH            VALUE 'O'.
022122            88  PD-PROD-BRV            VALUE 'B'.
022122            88  PD-PROD-HOSP           VALUE 'H'.
              16  PD-MAX-ATT-AGE           PIC S999        COMP-3.
022122        16  PD-WAIT-PERIOD.
                  20  pd-wait-days         pic 99.
022122            20  PD-RET-ELIM          PIC X.
022122        16  FILLER                   PIC X.
021222*       16  PD-MIN-ISSUE-AGE         PIC S999        COMP-3.
021222*       16  PD-MAX-ISSUE-AGE         PIC S999        COMP-3.
              16  PD-MAX-TERM              PIC S999        COMP-3.
              16  PD-MAX-AMT               PIC S9(07)      COMP-3.
              16  FILLER                   PIC X.
              16  PD-PRE-EXIST-EXCL-TYPE   PIC 99.
              16  PD-EXCLUSION-PERIOD-DAYS PIC S999        COMP-3.
              16  PD-COVERAGE-ENDS-MOS     PIC S999        COMP-3.
              16  PD-ACCIDENT-ONLY-MOS     PIC S999        COMP-3.
              16  PD-CRIT-PERIOD           PIC S999        COMP-3.
              16  PD-REC-CRIT-PERIOD       PIC 99.
              16  PD-REC-CP-ALPHA  REDEFINES PD-REC-CRIT-PERIOD.
                  20  PD-RECURRING-YN      PIC X.
                  20  FILLER               PIC X.
              16  PD-RTW-MOS               PIC 99.
051414        16  PD-MAX-EXTENSION         PIC 99.
100314        16  pd-ben-pct               pic sv999 comp-3.
100314*       16  FILLER                   PIC XX.
          12  PD-1ST-YR-ADMIN-ALLOW        PIC S9(3)V99    COMP-3.
          12  PD-TERM-LIMITS OCCURS 15.
              16  PD-LOW-TERM              PIC S999        COMP-3.
              16  PD-HI-TERM               PIC S999        COMP-3.
      *  THE LOAN AMT LIMITS CORRESPOND TO THE TERM LIMITS ABOVE
          12  PD-LOAN-AMT-LIMITS OCCURS 15.
              16  PD-LOW-AMT               PIC S9(5)       COMP-3.
              16  PD-HI-AMT                PIC S9(7)       COMP-3.
          12  PD-EARN-FACTORS.
              16  FILLER OCCURS 15.
                  20  FILLER OCCURS 15.
                      24  PD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
          12  PD-PRODUCT-DESC              PIC X(80).
          12  PD-TRUNCATED                 PIC X.
          12  FILLER                       PIC X(7).
          12  PD-MAINT-INFORMATION.
              16  PD-LAST-MAINT-DT         PIC X(02).
              16  PD-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
              16  PD-LAST-MAINT-BY         PIC X(04).
      *                                 COPY ERCCTBL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCTBL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION TABLE                        *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 200   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCTBL                   RKP=2,LEN=7     *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 *                                                                *
00021 ******************************************************************
00022
00023  01  COMM-TABLE-RECORD.
00024      12  CT-RECORD-ID                      PIC XX.
00025          88  VALID-CT-ID                      VALUE 'CT'.
00026
00027      12  CT-CONTROL-PRIMARY.
00028          16  CT-COMPANY-CD                 PIC X.
00029          16  CT-TABLE                      PIC XXX.
00030          16  CT-CNTRL-2.
00031              20  CT-BEN-TYPE               PIC X.
00032              20  CT-BEN-CODE               PIC XX.
00033
00034      12  CT-MAINT-INFORMATION.
00035          16  CT-LAST-MAINT-DT              PIC XX.
00036          16  CT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00037          16  CT-LAST-MAINT-USER            PIC X(4).
00038          16  FILLER                        PIC X(31).
00039
00040      12  CT-LIMITS.
00041          16  CT-TBF OCCURS 3 TIMES         PIC S9(7)V99   COMP-3.
00042
00043          16  CT-AGE OCCURS 3 TIMES         PIC S99        COMP-3.
00044
00045          16  CT-TRM OCCURS 3 TIMES         PIC S999       COMP-3.
00046
00047      12  CT-RATES.
00048          16  CT-RTX          OCCURS 27 TIMES.
00049              20  CT-RT                     PIC SV9(5)     COMP-3.
00050              20  CT-RT-R   REDEFINES
00051                  CT-RT                     PIC XXX.
00052
00053      12  FILLER                            PIC  X(42).
00054
00055 ******************************************************************
      *                                 COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
      *                                 COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030211* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
101711* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
021916* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
030211     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
030211         16  FILLER                        PIC X(10).
030211         16  AM-VG-KEY3.
030211             20  AM-VG3-ACCOUNT            PIC X(10).
030211             20  AM-VG3-EXP-DT             PIC XX.
030211         16  FILLER                        PIC X(4).
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
021916         88  AM-ACCOUNT-DROPPED               VALUE '6'.
021916         88  AM-ACCOUNT-LAPSED                VALUE '7'.
021916         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
021916         88  AM-ACCOUNT-PENDING               VALUE '9'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
           12  AM-DCC-UEF-STATE                  PIC XX.
           12  FILLER                            PIC XXX.
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
100417*                                 COPY ERCMAIL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCMAIL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
00013 *   ALTERNATE PATH    = NOT USED                                 *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
080406******************************************************************
080406*                   C H A N G E   L O G
080406*
080406* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080406*-----------------------------------------------------------------
080406*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080406* EFFECTIVE    NUMBER
080406*-----------------------------------------------------------------
080406* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
111108* 111108                   PEMA  ADD CRED BENE ADDR2
00017 ******************************************************************
00018
00019  01  MAILING-DATA.
00020      12  MA-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'MA'.
00022
00023      12  MA-CONTROL-PRIMARY.
00024          16  MA-COMPANY-CD                 PIC X.
00025          16  MA-CARRIER                    PIC X.
00026          16  MA-GROUPING.
00027              20  MA-GROUPING-PREFIX        PIC XXX.
00028              20  MA-GROUPING-PRIME         PIC XXX.
00029          16  MA-STATE                      PIC XX.
00030          16  MA-ACCOUNT.
00031              20  MA-ACCOUNT-PREFIX         PIC X(4).
00032              20  MA-ACCOUNT-PRIME          PIC X(6).
00033          16  MA-CERT-EFF-DT                PIC XX.
00034          16  MA-CERT-NO.
00035              20  MA-CERT-PRIME             PIC X(10).
00036              20  MA-CERT-SFX               PIC X.
00037
00038      12  FILLER                            PIC XX.
00039
00040      12  MA-ACCESS-CONTROL.
00041          16  MA-SOURCE-SYSTEM              PIC XX.
00042              88  MA-FROM-CREDIT                VALUE 'CR'.
00043              88  MA-FROM-VSI                   VALUE 'VS'.
00044              88  MA-FROM-WARRANTY              VALUE 'WA'.
00045              88  MA-FROM-OTHER                 VALUE 'OT'.
00046          16  MA-RECORD-ADD-DT              PIC XX.
00047          16  MA-RECORD-ADDED-BY            PIC XXXX.
00048          16  MA-LAST-MAINT-DT              PIC XX.
00049          16  MA-LAST-MAINT-BY              PIC XXXX.
00050          16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00051
00052      12  MA-PROFILE-INFO.
00053          16  MA-QUALIFY-CODE-1             PIC XX.
00054          16  MA-QUALIFY-CODE-2             PIC XX.
00055          16  MA-QUALIFY-CODE-3             PIC XX.
00056          16  MA-QUALIFY-CODE-4             PIC XX.
00057          16  MA-QUALIFY-CODE-5             PIC XX.
00058
00059          16  MA-INSURED-LAST-NAME          PIC X(15).
00060          16  MA-INSURED-FIRST-NAME         PIC X(10).
00061          16  MA-INSURED-MIDDLE-INIT        PIC X.
00062          16  MA-INSURED-ISSUE-AGE          PIC 99.
00063          16  MA-INSURED-BIRTH-DT           PIC XX.
00064          16  MA-INSURED-SEX                PIC X.
00065              88  MA-SEX-MALE                   VALUE 'M'.
00066              88  MA-SEX-FEMALE                 VALUE 'F'.
00067          16  MA-INSURED-SOC-SEC-NO         PIC X(11).
00068
080406         16  MA-ADDRESS-CORRECTED          PIC X.
081108         16  MA-JOINT-BIRTH-DT             PIC XX.
00069 *        16  FILLER                        PIC X(12).
00070
00071          16  MA-ADDRESS-LINE-1             PIC X(30).
00072          16  MA-ADDRESS-LINE-2             PIC X(30).
00073          16  MA-CITY-STATE.
                   20  MA-CITY                   PIC X(28).
                   20  MA-ADDR-STATE             PIC XX.
00074          16  MA-ZIP.
00075              20  MA-ZIP-CODE.
00076                  24  MA-ZIP-CODE-1ST       PIC X(1).
00077                      88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00078                  24  FILLER                PIC X(4).
00079              20  MA-ZIP-PLUS4              PIC X(4).
00080          16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
00081              20  MA-CAN-POSTAL-CODE-1      PIC X(3).
00082              20  MA-CAN-POSTAL-CODE-2      PIC X(3).
00083              20  FILLER                    PIC X(3).
00084
00085          16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
00086
               16  FILLER                        PIC XXX.
00087 *        16  FILLER                        PIC X(10).
00088
           12  MA-CRED-BENE-INFO.
CIDMOD         16  MA-CRED-BENE-NAME                 PIC X(25).
CIDMOD         16  MA-CRED-BENE-ADDR                 PIC X(30).
               16  MA-CRED-BENE-ADDR2                PIC X(30).
CIDMOD         16  MA-CRED-BENE-CTYST.
                   20  MA-CRED-BENE-CITY             PIC X(28).
                   20  MA-CRED-BENE-STATE            PIC XX.
CIDMOD         16  MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-ZIP-CODE.
CIDMOD                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
CIDMOD                     88  MA-CB-CANADIAN-POST-CODE
                                                 VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                    PIC X(4).
CIDMOD             20  MA-CB-ZIP-PLUS4               PIC X(4).
CIDMOD         16  MA-CB-CANADIAN-POSTAL-CODE
                                  REDEFINES MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
CIDMOD             20  FILLER                        PIC X(3).
080406     12  MA-POST-CARD-MAIL-DATA.
080406         16  MA-MAIL-DATA OCCURS 7.
080406             20  MA-MAIL-TYPE              PIC X.
080406                 88  MA-12MO-MAILING           VALUE '1'.
080406                 88  MA-EXP-MAILING            VALUE '2'.
080406             20  MA-MAIL-STATUS            PIC X.
080406                 88  MA-MAIL-ST-MAILED         VALUE '1'.
080406                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  MA-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC XX.
080406*    12  FILLER                            PIC X(30).
00090 ******************************************************************
100417*                                 COPY ELCCRTT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCRTT.                            *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 552  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
111204******************************************************************
111204*                   C H A N G E   L O G
111204*
111204* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111204*-----------------------------------------------------------------
111204*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111204* EFFECTIVE    NUMBER
111204*-----------------------------------------------------------------
111204* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
040109* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
012010* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
022715* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
012918* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
091318* 091318  CR2018073000001  PEMA  ADD Refund methods
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
111204******************************************************************
00021
00022  01  CERTIFICATE-TRAILERS.
00023      12  CS-RECORD-ID                      PIC XX.
00024          88  VALID-CS-ID                      VALUE 'CS'.
00025
00026      12  CS-CONTROL-PRIMARY.
00027          16  CS-COMPANY-CD                 PIC X.
00028          16  CS-CARRIER                    PIC X.
00029          16  CS-GROUPING                   PIC X(6).
00032          16  CS-STATE                      PIC XX.
00033          16  CS-ACCOUNT                    PIC X(10).
00036          16  CS-CERT-EFF-DT                PIC XX.
00037          16  CS-CERT-NO.
00038              20  CS-CERT-PRIME             PIC X(10).
00039              20  CS-CERT-SFX               PIC X.
               16  CS-TRAILER-TYPE               PIC X.
                   88  COMM-TRLR           VALUE 'A'.
061013             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
040109             88  CERT-DATA-TRLR      VALUE 'C'.
00040
040109     12  CS-DATA-AREA                      PIC X(516).
040109
040109     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
040109         16  CS-BANK-COMMISSION-AREA.
040109             20  CS-BANK-COMMS       OCCURS 10.
040109                 24  CS-AGT                PIC X(10).
040109                 24  CS-COM-TYP            PIC X.
040109                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
040109                 24  CS-RECALC-LV-INDIC    PIC X.
040109                 24  FILLER                PIC X(10).
040109         16  FILLER                        PIC X(256).
040109
061013     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
061013****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
               16  CS-MB-CLAIM-DATA OCCURS 24.
                   20  CS-CLAIM-NO               PIC X(7).
                   20  CS-CLAIM-TYPE             PIC X.
                       88  CS-AH-CLM               VALUE 'A'.
                       88  CS-IU-CLM               VALUE 'I'.
                       88  CS-GP-CLM               VALUE 'G'.
                       88  CS-LF-CLM               VALUE 'L'.
                       88  CS-PR-CLM               VALUE 'P'.
052614                 88  CS-FL-CLM               VALUE 'F'.
100518                 88  CS-OT-CLM               VALUE 'O'.
080322                 88  CS-BR-CLM               VALUE 'B'.
080322                 88  CS-HS-CLM               VALUE 'H'.
                   20  CS-INSURED-TYPE           PIC X.
                       88  CS-PRIM-INSURED          VALUE 'P'.
                       88  CS-CO-BORROWER           VALUE 'C'.
                   20  CS-BENEFIT-PERIOD         PIC 99.
                   20  CS-DAYS-PAID              PIC S9(5) COMP-3.
                   20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
                   20  CS-REMAINING-BENS         PIC S999 COMP-3.
               16  FILLER                        PIC X(12).
040109     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
040109         16  CS-VIN-NUMBER                 PIC X(17).
012010         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
121712         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
121712         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
022715         16  cs-agent-name.
022715             20  cs-agent-fname            pic x(20).
022715             20  cs-agent-mi               pic x.
022715             20  cs-agent-lname            pic x(25).
022715         16  cs-license-no                 pic x(15).
022715         16  cs-npn-number                 pic x(10).
022715         16  cs-agent-edit-status          pic x.
022715             88  cs-ae-refer-to-manager      value 'M'.
022715             88  cs-ae-cover-sheet           value 'C'.
022715             88  cs-ae-sig-form              value 'S'.
022715             88  cs-ae-verified              value 'V'.
022715             88  cs-unidentified-signature   value 'U'.
022715             88  cs-cert-returned            value 'R'.
022715             88  cs-accept-no-commission     value 'N'.
020816         16  cs-year                       pic 9999.
020816         16  cs-make                       pic x(20).
020816         16  cs-model                      pic x(20).
020816         16  cs-future                     pic x(20).
020816         16  cs-vehicle-odometer           pic s9(7) comp-3.
012918         16  cs-claim-verification-status  pic x.
012918             88  cs-clm-ver-eligible         value 'A'.
012918             88  cs-clm-ver-partial-elig     value 'B'.
012918             88  cs-clm-ver-not-eligible     value 'C'.
012918             88  cs-clm-ver-not-elig-opn-clm value 'D'.
012918             88  cs-clm-ver-not-part-elig-rw value 'E'.
012918             88  cs-clm-ver-ND-CERT          value 'F'.
012918             88  cs-clm-ver-spec-other       value 'G'.
012918             88  cs-clam-ver-pratial-corrected
012918                                             value 'H'.
012918             88  cs-clm-ver-no-matches       value 'I'.
012918             88  cs-clm-ver-not-elig-corrected
012918                                             value 'J'.
012918             88  cs-clm-ver-needs-review     value 'R'.
012918             88  cs-clm-ver-sent-to-claims   value 'W'.
091318         16  CS-LF-REFUND-METHOD           PIC X.
091318         16  CS-AH-REFUND-METHOD           PIC X.
020816         16  FILLER                        PIC X(353). *> was 420
121712*        16  FILLER                        PIC X(496).
      *                                 COPY ELCDATE.
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
      *                                 COPY ELCCALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCALC.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
00008 *                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
00009 *                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
00010 *                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
00011 *                                                                *
00012 *  PASSED TO ELRTRM                                              *
00013 *  -----------------                                             *
00014 *  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
00015 *  ORIGINAL TERM                                                 *
00016 *  BEGINNING DATE                                                *
00017 *  ENDING DATE                                                   *
00018 *  COMPANY I.D.                                                  *
00019 *  ACCOUNT MASTER USER FIELD                                     *
00020 *  PROCESS SWITCH (CANCEL, CLAIM)                                *
00021 *  FREE LOOK DAYS                                                *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  REMAINING TERM 1 - USED FOR EARNINGS                          *
00026 *  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
00027 *  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
00028 *  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
00029 *----------------------------------------------------------------*
00030 *  PASSED TO ELRAMT                                              *
00031 *  ----------------                                              *
00032 *  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
00033 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00034 *  ORIGINAL AMOUNT                                               *
00035 *  ALTERNATE BENEFIT (BALLON)                                    *
00036 *  A.P.R. - NET PAY ONLY                                         *
00037 *  METHOD
00038 *  PAYMENT FREQUENCY - FOR FARM PLAN                             *
00039 *  COMPANY I.D.                                                  *
00040 *  BENEFIT TYPE                                                  *
00041 *                                                                *
00042 *  RETURNED FROM ELRAMT                                          *
00043 *  --------------------                                          *
00044 *  REMAINING AMOUNT 1 - CURRENT                                  *
00045 *  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
00046 *  REMAINING AMOUNT FACTOR
00047 *----------------------------------------------------------------*
00048 *  PASSED TO ELRESV                                              *
00049 *  -----------------                                             *
00050 *  CERTIFICATE EFFECTIVE DATE                                    *
00051 *  VALUATION DATE                                                *
00052 *  PAID THRU DATE                                                *
00053 *  BENEFIT                                                       *
00054 *  INCURRED DATE                                                 *
00055 *  REPORTED DATE                                                 *
00056 *  ISSUE AGE                                                     *
00057 *  TERM                                                          *
00058 *  CDT PERCENT                                                   *
00059 *  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
00060 * *CLAIM TYPE (LIFE, A/H)                                        *
00061 * *REMAINING BENEFIT (FROM ELRAMT)                               *
00062 * *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
00063 *                                                                *
00064 *  RETURNED FROM ELRESV                                          *
00065 *  --------------------                                          *
00066 *  CDT TABLE USED                                                *
00067 *  CDT FACTOR USED                                               *
00068 *  PAY TO CURRENT RESERVE                                        *
00069 *  I.B.N.R. - A/H ONLY                                           *
00070 *  FUTURE (ACCRUED) AH ONLY                                      *
00071 *----------------------------------------------------------------*
00072 *  PASSED TO ELRATE                                              *
00073 *  ----------------                                              *
00074 *  CERT ISSUE DATE                                               *
00075 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00076 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00077 *  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
00078 *  STATE CODE (CLIENT DEFINED)                                   *
00079 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00080 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00081 *  DEVIATION CODE                                                *
00082 *  ISSUE AGE                                                     *
00083 *  ORIGINAL BENEFIT AMOUNT                                       *
00084 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00085 *  PROCESS TYPE (ISSUE OR CANCEL)                                *
00086 *  BENEFIT KIND (LIFE OR A/H)                                    *
00087 *  A.P.R.                                                        *
00088 *  METHOD
00089 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00090 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00091 *  COMPANY I.D. (3 CHARACTER)                                    *
00092 *  BENEFIT CODE                                                  *
00093 *  BENEFIT OVERRIDE CODE                                         *
00094 *  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
00095 *  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
00096 *  JOINT INDICATOR (CSL ONLY)                                    *
00097 *  FIRST PAYMENT DATE (CSL ONLY)                                 *
00098 *  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
00099 *                                                                *
00100 *  RETURNED FROM ELRATE                                          *
00101 *  --------------------                                          *
00102 *  CALCULATED PREMIUM                                            *
00103 *  PREMIUM RATE                                                  *
00104 *  MORTALITY CODE                                                *
00105 *  MAX ATTAINED AGE                                              *
00106 *  MAX AGE                                                       *
00107 *  MAX TERM                                                      *
00108 *  MAX MONTHLY BENEFIT                                           *
00109 *  MAX TOTAL BENIFIT                                             *
00110 *  COMPOSITE RATE (OPEN-END ONLY)                                *
00111 *----------------------------------------------------------------*
00112 *  PASSED TO ELRFND                                              *
00113 *  ----------------                                              *
00114 *  CERT ISSUE DATE                                               *
00115 *  REFUND DATE                                                   *
00116 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00117 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00118 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00119 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00120 *  STATE CODE (CLIENT DEFINED)                                   *
00121 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00122 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00123 *  DEVIATION CODE                                                *
00124 *  ISSUE AGE                                                     *
00125 *  ORIGINAL BENEFIT AMOUNT                                       *
00126 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00127 *  PROCESS TYPE (CANCEL)                                         *
00128 *  BENEFIT KIND (LIFE OR A/H)                                    *
00129 *  A.P.R.                                                        *
00130 *  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
00131 *  RATING METHOD -  (CODE FROM BENEFIT)                          *
00132 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00133 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00134 *  COMPANY I.D. (3 CHARACTER)                                    *
00135 *  BENEFIT CODE                                                  *
00136 *  BENEFIT OVERRIDE CODE                                         *
00137 *                                                                *
00138 *  RETURNED FROM ELRFND                                          *
00139 *  --------------------                                          *
00140 *  CALCULATED REFUND                                             *
00141 *----------------------------------------------------------------*
00142 *  PASSED TO ELEARN                                              *
00143 *  ----------------                                              *
00144 *  CERT ISSUE DATE                                               *
00145 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00146 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00147 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00148 *  STATE CODE (CLIENT DEFINED)                                   *
00149 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00150 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00151 *  DEVIATION CODE                                                *
00152 *  ISSUE AGE                                                     *
00153 *  ORIGINAL BENEFIT AMOUNT                                       *
00154 *  BENEFIT KIND (LIFE OR A/H)                                    *
00155 *  A.P.R.                                                        *
00156 *  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
00157 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00158 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00159 *  COMPANY I.D. (3 CHARACTER)                                    *
00160 *  BENEFIT CODE                                                  *
00161 *  BENEFIT OVERRIDE CODE                                         *
00162 *                                                                *
00163 *  RETURNED FROM ELEARN                                          *
00164 *  --------------------                                          *
00165 *  INDICATED  EARNINGS                                           *
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 450                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
010303* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
101807* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
010410* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
010410* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
041710* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
101110* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
071211* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
040615* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
012820* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
010303******************************************************************
00170
00171  01  CALCULATION-PASS-AREA.
00172      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
00173                                      COMP.
00174
00175      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CP-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
012820                                  '9' 'A' 'B' 'C' 'D' 'E' 'H' 'I'.
00179        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00180        88  CP-ERROR-IN-DATES                       VALUE '2'.
00181        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00182        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00183        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00184        88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
00185        88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
00186        88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
00187        88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
00188        88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
00189        88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
00190        88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
00191        88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
00192        88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
00193        88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
00194        88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
00195        88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
012820       88  CP-ERROR-TERM-BELOW-MINIMUM             VALUE 'I'.
00196
00197      12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
00198        88  NO-CP-ERROR-2                           VALUE ZERO.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
00202          16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
00203          16  CP-CERT-EFF-DT        PIC XX.
00204          16  CP-VALUATION-DT       PIC XX.
00205          16  CP-PAID-THRU-DT       PIC XX.
00206          16  CP-BENEFIT-TYPE       PIC X.
00207            88  CP-AH                               VALUE 'A' 'D'
00208                                                    'I' 'U'.
00209            88  CP-REDUCING-LIFE                    VALUE 'R'.
00210            88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
00211          16  CP-INCURRED-DT        PIC XX.
00212          16  CP-REPORTED-DT        PIC XX.
00213          16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
00214          16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
00215          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00216                                      COMP-3.
00217          16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
00218                                      COMP-3.
00219          16  CP-CDT-METHOD         PIC X.
00220            88  CP-CDT-ROUND-NEAR                   VALUE '1'.
00221            88  CP-CDT-ROUND-HIGH                   VALUE '2'.
00222            88  CP-CDT-INTERPOLATED                 VALUE '3'.
00223          16  CP-CLAIM-TYPE         PIC X.
00224            88  CP-AH-CLAIM                         VALUE 'A'.
00225            88  CP-LIFE-CLAIM                       VALUE 'L'.
00226          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00227                                      COMP-3.
00228          16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
00229                                      COMP-3.
00230          16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
00231                                      COMP-3.
00232          16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
00233                                      COMP-3.
00234          16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
00235                                      COMP-3.
00236          16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
00237                                      COMP-3.
00238          16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
00239                                      COMP-3.
00240          16  CP-REM-TERM-METHOD    PIC X.
00241            88  CP-EARN-AFTER-15TH                  VALUE '1'.
00242            88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
00243            88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
00244            88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
00245            88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
00246            88  CP-EARN-AFTER-14TH                  VALUE '6'.
00247            88  CP-EARN-AFTER-16TH                  VALUE '7'.
00248          16  CP-EARNING-METHOD     PIC X.
00249            88  CP-EARN-BY-R78                      VALUE '1' 'R'.
00250            88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
00251            88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
00252            88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
00253            88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
00254            88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
00255            88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
00256            88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
00257            88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
00258            88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
033104           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
033104           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
092310           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
                 88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
00259          16  CP-PROCESS-TYPE       PIC X.
00260            88  CP-CLAIM                            VALUE '1'.
00261            88  CP-CANCEL                           VALUE '2'.
00262            88  CP-ISSUE                            VALUE '3'.
00263          16  CP-SPECIAL-CALC-CD    PIC X.
00264            88  CP-OUTSTANDING-BAL              VALUE 'O'.
00265            88  CP-1-MTH-INTEREST               VALUE ' '.
00266            88  CP-0-MTH-INTEREST               VALUE 'A'.
00267            88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
00268            88  CP-CRITICAL-PERIOD              VALUE 'C'.
00269            88  CP-TERM-IS-DAYS                 VALUE 'D'.
00270            88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
00271            88  CP-FARM-PLAN                    VALUE 'F'.
00272            88  CP-RATE-AS-STANDARD             VALUE 'G'.
00273            88  CP-2-MTH-INTEREST               VALUE 'I'.
00274            88  CP-3-MTH-INTEREST               VALUE 'J'.
00275            88  CP-4-MTH-INTEREST               VALUE 'K'.
00276            88  CP-BALLOON-LAST-PMT             VALUE 'L'.
00277            88  CP-MORTGAGE-REC                 VALUE 'M'.
00278            88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
00279            88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
00280            88  CP-NET-PAY-SIMPLE               VALUE 'S'.
00281            88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
00282                                                      'W' 'X'.
00283            88  CP-TRUNCATE-0-MTH               VALUE 'T'.
00284            88  CP-TRUNCATE-1-MTH               VALUE 'U'.
00285            88  CP-TRUNCATE-2-MTH               VALUE 'V'.
00286            88  CP-TRUNCATE-3-MTH               VALUE 'W'.
00287            88  CP-TRUNCATE-4-MTH               VALUE 'X'.
00288            88  CP-SUMMARY-REC                  VALUE 'Z'.
00289            88  CP-PROPERTY-BENEFIT             VALUE '2'.
00290            88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
00291            88  CP-AD-D-BENEFIT                 VALUE '4'.
00292            88  CP-CSL-METH-1                   VALUE '5'.
00293            88  CP-CSL-METH-2                   VALUE '6'.
00294            88  CP-CSL-METH-3                   VALUE '7'.
00295            88  CP-CSL-METH-4                   VALUE '8'.
00296
00297          16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
00298                                      COMP-3.
00299          16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
00300          16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
00301          16  CP-STATE              PIC XX          VALUE SPACE.
00302          16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
00303          16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
00304            88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
00305                '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
00306          16  CP-R78-OPTION         PIC X.
00307            88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
00308            88  CP-TERM-TIMES-TERM                  VALUE '1'.
00309
00310          16  CP-COMPANY-CD         PIC X             VALUE SPACE.
00311          16  CP-IBNR-RESERVE-SW    PIC X.
00312          16  CP-CLAIM-STATUS       PIC X.
00313          16  CP-RATE-FILE          PIC X.
00314          16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
00315                                      COMP-3.
00316
00317          16  CP-LIFE-OVERRIDE-CODE PIC X.
00318          16  CP-AH-OVERRIDE-CODE   PIC X.
00319
00320          16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
00321                                      COMP-3.
               16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
                                         PIC S9(5)V99 COMP-3.
00322          16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
00323                                      COMP-3.
00324          16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
00325                                      COMP-3.
00326          16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
00327                                      COMP-3.
               16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
                                        PIC S9(7)V99 COMP-3.
00328
00329          16  CP-PAID-FROM-DATE     PIC X(02).
00330          16  CP-CLAIM-CALC-METHOD  PIC X(01).
00331          16  CP-EXT-DAYS-CALC      PIC X.
00332            88  CP-EXT-NO-CHG                   VALUE ' '.
00333            88  CP-EXT-CHG-LF                   VALUE '1'.
00334            88  CP-EXT-CHG-AH                   VALUE '2'.
00335            88  CP-EXT-CHG-LF-AH                VALUE '3'.
00336          16  CP-DOMICILE-STATE     PIC XX.
00337          16  CP-CARRIER            PIC X.
00338          16  CP-REIN-FLAG          PIC X.
00339          16  CP-REM-TRM-CALC-OPTION PIC X.
00340            88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
00341                       '2' '3' '4' '5'.
00342            88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
00343            88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
00344            88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
00345            88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
00346            88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
00347            88  CP-EXT-30-DAY-MONTH          VALUE '3'.
00348            88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
                 88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
00349          16  CP-SIG-SWITCH         PIC X.
00350          16  CP-RATING-METHOD      PIC X.
00351            88  CP-RATE-AS-R78                      VALUE '1' 'R'.
00352            88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
00353            88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
00354            88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
00355            88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
00356            88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
00357            88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
00358            88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
00359            88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
00360          16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
00361                                      COMP-3.
090803         16  CP-BEN-CATEGORY       PIC X.
011904         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
                                         PIC S99V9(5) COMP-3.
011904         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
                                         PIC S99V9(5) COMP-3.
080305         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
               16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
041310         16  CP-EXPIRE-DT          PIC XX.
041710         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
               16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-SPEC-CALC      PIC X.
                   88  CP-CALC-GROSS-FEE        VALUE 'G'.
                   88  CP-CALC-CLP              VALUE 'C'.
               16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
               16  CP-CANCEL-REASON      PIC X.
               16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-PMT-MODE           PIC X.
               16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
071211         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
               16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
071211         16  FILLER                PIC X.
00363
00364 ***************    OUTPUT FROM ELRESV   ************************
00365
00366          16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
00367
00368          16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
00369                                      COMP-3.
101807         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  FILLER                PIC X(09).
00377 ***************    OUTPUT FROM ELRTRM   *************************
00378
00379          16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
00380                                      COMP-3.
00381          16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
00382                                      COMP-3.
00383          16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
00384                                      COMP-3.
00385          16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
00386                                      COMP-3.
00387          16  FILLER                PIC X(12).
00388
00389 ***************    OUTPUT FROM ELRAMT   *************************
00390
00391          16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
00392                                      COMP-3.
00393          16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
00394                                      COMP-3.
00395          16  FILLER                PIC X(12).
00396
00397 ***************    OUTPUT FROM ELRATE   *************************
00398
00399          16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
00400                                      COMP-3.
00401          16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
00402                                      COMP-3.
00403          16  CP-MORTALITY-CODE     PIC X(4).
00404          16  CP-RATE-EDIT-FLAG     PIC X.
00405              88  CP-RATES-NEED-APR                  VALUE '1'.
00406          16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
00407                                      COMP-3.
010716         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
032905         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
               16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
                                         PIC S9(7)V99 COMP-3.
00409          16  FILLER                PIC X(07).
00410
00411 ***************    OUTPUT FROM ELRFND   *************************
00412
00413          16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
00414                                      COMP-3.
00415          16  CP-REFUND-TYPE-USED   PIC X.
00416            88  CP-R-AS-R78                         VALUE '1'.
00417            88  CP-R-AS-PRORATA                     VALUE '2'.
00418            88  CP-R-AS-CALIF                       VALUE '3'.
00419            88  CP-R-AS-TEXAS                       VALUE '4'.
00420            88  CP-R-AS-FARM-PLAN                   VALUE '4'.
00421            88  CP-R-AS-NET-PAY                     VALUE '5'.
00422            88  CP-R-AS-ANTICIPATION                VALUE '6'.
00423            88  CP-R-AS-MEAN                        VALUE '8'.
00424            88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
033104           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
033104           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
092310           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
092310           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
                 88  CP-R-AS-REPOSSESSION                VALUE 'R'.
00425          16  FILLER                PIC X(12).
00426
00427 ***************    OUTPUT FROM ELEARN   *************************
00428
00429          16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
00430                                      COMP-3.
00431          16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
00432                                      COMP-3.
00433          16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
00434                                      COMP-3.
00435          16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
00436                                      COMP-3.
00437          16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
00438                                      COMP-3.
00439          16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
00440                                      COMP-3.
00441          16  CP-EARNING-TYPE-USED  PIC X.
00442            88  CP-E-AS-SPECIAL                     VALUE 'S'.
00443            88  CP-E-AS-R78                         VALUE '1'.
00444            88  CP-E-AS-PRORATA                     VALUE '2'.
00445            88  CP-E-AS-TEXAS                       VALUE '4'.
00446            88  CP-E-AS-FARM-PLAN                   VALUE '4'.
00447            88  CP-E-AS-NET-PAY                     VALUE '5'.
00448            88  CP-E-AS-ANTICIPATION                VALUE '6'.
00449            88  CP-E-AS-MEAN                        VALUE '8'.
00450            88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
00451          16  FILLER                PIC X(12).
00452
00453 ***************    OUTPUT FROM ELPMNT   *************************
00454
00455          16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
00456                                      COMP-3.
00457          16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
00458                                      COMP-3.
00459          16  FILLER                PIC X(12).
00460
00461 ***************   MISC WORK AREAS    *****************************
00462          16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
00463                                      COMP-3.
00464          16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
00465                                      COMP-3.
00466          16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
00467                                      COMP-3.
00468          16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
00469                                      COMP-3.
00470          16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
00471                                      COMP-3.
00472          16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
00473                                      COMP-3.
00474          16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
00475              88  OPEN-RATE-FILE                   VALUE 'O'.
00476              88  CLOSE-RATE-FILE                  VALUE 'C'.
00477              88  IO-ERROR                         VALUE 'E'.
00478
00479          16  CP-FIRST-PAY-DATE     PIC XX.
00480
00481          16  CP-JOINT-INDICATOR    PIC X.
00482
00483          16  CP-RESERVE-REMAINING-TERM
00484                                    PIC S9(4)V9    VALUE ZERO
00485                                      COMP-3.
00486
00487          16  CP-INSURED-BIRTH-DT   PIC XX.
00488
00489          16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
00490                                      COMP-3.
00491
00492          16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
00493                                      COMP-3.
00494
00495          16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
00496                                      COMP-3.
00497
00498          16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
00499                                      COMP-3.
00500
00501          16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
00502                                      COMP-3.
00503
00504          16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
00505                                      COMP-3.
00506
00507          16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
00508                                      COMP-3.
00509
00510          16  CP-ROA-REFUND         PIC X          VALUE 'N'.
00511              88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
00512
010303         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
010303                                     COMP-3.
041710         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
041710                                     COMP-3.
               16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
040615         16  cp-extra-periods      pic 9 value zeros.
070115         16  cp-net-only-state     pic x value spaces.
041710         16  FILLER                PIC X(13).
00514 ******************************************************************
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
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).
081919**** client-in-data cannot be more than 36 characters ***
081919   05 CLIENT-IN-DATA           pic x(36).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'SOCK03' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           display '  BEGIN SOCK03 '
           display 'SOCK03:transaction data =', CLIENT-IN-DATA (1:10)
           display 'SOCK03:socket number    =', GIVE-TAKE-SOCKET.
           IF CLIENT-IN-DATA (1:6) = 'REFUND' OR 'NEWBUS'
              MOVE CLIENT-IN-DATA (1:6) TO WS-CALL-TYPE
010816        evaluate true
010816           when client-in-data (8:3) = 'AHL'
010816              MOVE X'06'         TO WS-COMP-CD
010816              MOVE 'AHL'         TO WS-SEND-BUF (5:3)
010816                                    WS-COMP-ID
010816           when CLIENT-IN-DATA (8:3) = 'DCC'
010816              MOVE X'05'         TO WS-COMP-CD
010816              MOVE 'DCC'         TO WS-SEND-BUF (5:3)
010816                                    WS-COMP-ID
100417           when CLIENT-IN-DATA (8:3) = 'VPP'
010816              MOVE X'07'         TO WS-COMP-CD
100417              MOVE 'VPP'         TO WS-SEND-BUF (5:3)
010816                                    WS-COMP-ID
062121           when CLIENT-IN-DATA (8:3) = 'FNL'
062121              MOVE X'08'         TO WS-COMP-CD
062121              MOVE 'FNL'         TO WS-SEND-BUF (5:3)
062121                                    WS-COMP-ID
010816           when other
010816              MOVE X'04'         TO WS-COMP-CD
010816              MOVE 'CID'         TO WS-SEND-BUF (5:3)
010816                                    WS-COMP-ID
010816        end-evaluate
              move 'CSO'               to ws-send-buf (1:3)
              move +25                 to ws-send-msg-size
           END-IF
           display 'SOCK03:sequence number  =', ws-seq-num.
           display 'SOCK03:send buffer      =', ws-send-buf(1:25).
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.
           if return-code <= zero
               display 'SOCK03:send error ',
               go to socket-error.
           move low-values to ws-recv-buf.
           set ws-recv-total to zero.
           compute ws-recv-left = ws-recv-msg-size.
           display 'SOCK03:About to recv '
           call "recv" using by value GIVE-TAKE-SOCKET,
               by reference ws-recv-buf
               by value ws-recv-msg-size,
               by value ws-flags.
           if return-code < zero
              display 'SOCK03:recv error ',
              go to socket-error.
           if return-code = zero
              display 'SOCK03:client disconnected',
              go to socket-error.
      *
           display 'SOCK03:Good recv  '
           display 'SOCK03:return code      = ', return-code
062119     display 'SOCK03:receive buffer   = ', ws-recv-buf(1:89)
           move +4096                  to ws-send-msg-size
      *    move +750                   to ws-send-msg-size
081919     move ws-recv-buf (1:89)     to soc-client-in-data
           MOVE SPACES                 TO WS-ELCERT-SW
           MOVE X'0A'                  TO WS-HEX-0A
090314     if client-account = 'EPIQ000001'
090314        set ws-epiq-request to true
090314        move spaces              to client-account
090314     end-if
090314     move +0 to ws-epiq-max
080519     if client-car = spaces
080519        if client-state = 'KY'
080519           move '8'              to client-car
080519        else
080519           move '9'              to client-car
080519        end-if
080519     end-if
080519     if client-grp = spaces
080519        move '000000'            to client-grp
080519     end-if
           EVALUATE TRUE
              WHEN (CLIENT-CAR NOT = SPACES)
                 AND (CLIENT-STATE NOT = SPACES)
                 AND (CLIENT-ACCOUNT NOT = SPACES)
                 AND (CLIENT-EFF-DT NOT = SPACES)
                 AND (CLIENT-CERT-NO NOT = SPACES)
                 SET WS-ELCERT-FULL    TO TRUE
              WHEN (CLIENT-CERT-NO NOT = SPACES)
                 SET WS-ELCERT-CERT-NO TO TRUE
      *       WHEN (CLIENT-EFF-DT NOT = SPACES)
      *          AND (CLIENT-LAST-NAME NOT = SPACES)
              WHEN (CLIENT-ACCOUNT NOT = SPACES)
                 AND (CLIENT-LAST-NAME NOT = SPACES)
                 SET WS-ELCERT-ACT-NAME TO TRUE
              WHEN CLIENT-LAST-NAME NOT = SPACES
                 SET WS-ELCERT-NAME    TO TRUE
062119        when (client-vin not = spaces)
062119           and (client-state not = spaces)
062119           and (client-account not = spaces)
062119           set ws-elcert-vin to true
062119*          display ' SETTING VIN '
              WHEN OTHER
                 MOVE ' BAD SELECTION OPTION ' TO WS-COMMENT
                 MOVE 'X'              TO WS-LF-STATUS WS-AH-STATUS
                 MOVE ZEROS            TO WS-RESP
                 PERFORM 0030-BUILD-BUFFER
                                       THRU 0030-EXIT
                 PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
                 PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
                 GO TO 0010-RETURN
           END-EVALUATE
           IF CLIENT-CAN-REASON = 'R'
              MOVE 'R'                 TO WS-CANCEL-REASON
           ELSE
              MOVE ' '                 TO WS-CANCEL-REASON
           END-IF
           IF WS-ELCERT-KEY-SW NOT = SPACES
              PERFORM 0050-BEGIN-REFUND-PROCESS
                                       THRU 0050-EXIT
           END-IF
           PERFORM 0025-CLOSE-SOCKET   THRU 0025-EXIT
           .
       0010-RETURN.
           
      * exec cics return end-exec.
      *    MOVE '.(                    ''   #00004428' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * goback.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK03' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback.
      *    PERFORM 0030-BUILD-BUFFER   THRU 0030-EXIT
           .
       0020-SEND-BUFFER.
      *    display 'SOCK03:sequence number  =', ws-seq-num.
052319     display 'SOCK03:send buffer      =', ws-send-buf(1:500).
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.
           if return-code <= zero
               display 'SOCK03:send error ' return-code,
               go to socket-error.
       0020-EXIT.
           EXIT.
       0025-CLOSE-SOCKET.
072415*    display 'SOCK03:closing socket'.
072415*    call "close" using by value GIVE-TAKE-SOCKET .
      *    display 'SOCK03:done'.
           .
       0025-EXIT.
           EXIT.
      *
      * set up the receive buffer
      *
           move low-values to ws-recv-buf.
           set ws-recv-total to zero.
           compute ws-recv-left = ws-recv-msg-size.
      *
      * receive data
      *
       recv-1.
           call "recv" using by value GIVE-TAKE-SOCKET,
               by reference ws-recv-buf(1+ws-recv-total:ws-recv-left),
               by value ws-recv-left,
               by value ws-flags.
      *
      * test what was received and decide what we should do
      *
           if return-code < zero
              display 'SOCK03:recv error ',
              go to socket-error.
           if return-code = zero
              display 'SOCK03:client disconnected',
              go to socket-error.
      *
      * have we received all the data yet?
      *
           compute ws-recv-total = ws-recv-total + return-code.
           compute ws-recv-left = ws-recv-msg-size - ws-recv-total.
      *
      * not yet
      *
           if ws-recv-left > 0 go to recv-1.
      *
      * received all the data
      *
      *    display 'SOCK03:receive buffer   =', ws-recv-buf(1:50).
      *
      * make sure what we received was what we sent
      *
      *    if ws-recv-buf <> ws-send-buf
      *        display "SOCK03:data doesn't match",
      *        go to socket-error.
      *
      * end of pass
      *
      *loop-end.
      *    if ws-seq-num = 5 go to socket-error.
      *    go to loop-1.
      *
      * program end
      *
       socket-error.
           if ws-seq-num <> 0
               display "SOCK03:did not complete".
      *
      * flush the send buffer and deallocate
      *
      *    display 'SOCK03:closing socket'.
      *    call "close" using by value GIVE-TAKE-SOCKET .
      *
      * finised return to cics
      *
       socket-fin.
072415*    display 'SOCK03:closing socket'.
072415*    call "close" using by value GIVE-TAKE-SOCKET .
      *    display 'SOCK03:done'.
           
      * exec cics return end-exec.
      *    MOVE '.(                    ''   #00004518' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * goback.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK03' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback.
       0030-BUILD-BUFFER.
           MOVE SPACES                 TO WS-SEND-BUF
           IF WS-CALL-TYPE = 'NEWBUS'
              STRING
                 WS-COMMENT            ';'
                 WS-RESP               ';'
                 WS-CARRIER            ';'
                 WS-GROUPING           ';'
                 WS-STATE              ';'
                 WS-ACCOUNT            ';'
                 WS-CERT-EFF-DATE      ';'
                 WS-CERT-NO            ';'
                 WS-LAST-NAME          ';'
                 WS-FIRST-NAME         ';'
                 WS-MID-INIT           ';'
                 WS-LAST-JNAME         ';'
                 WS-FIRST-JNAME        ';'
                 WS-MID-JINIT          ';'
                 WS-POST-CARD-SW       ';'
                 ws-lf-coverage-type   ';'
                 WS-LF-STATUS          ';'
                 WS-LF-BEN-CODE        ';'
                 WS-LF-BEN-CODE-DESC   ';'
                 WS-LF-TERM            ';'
                 WS-LF-REM-TERM        ';'
                 WS-LF-PREM            ';'
                 WS-LF-REFUND          ';'
                 WS-LF-METHOD          ';'
                 WS-LF-ORIG-BENEFIT    ';'
                 WS-LF-REM-BEN         ';'
                 WS-LF-EXPIRE-DATE     ';'
                 WS-LF-COMM            ';'
                 WS-LF-UEC             ';'
                 WS-AH-COVERAGE-TYPE   ';'
                 WS-AH-STATUS          ';'
                 WS-AH-BEN-CODE        ';'
                 WS-AH-BEN-CODE-DESC   ';'
                 WS-AH-TERM            ';'
                 WS-AH-REM-TERM        ';'
                 WS-AH-PREM            ';'
                 WS-AH-REFUND          ';'
                 WS-AH-METHOD          ';'
                 WS-AH-ORIG-BENEFIT    ';'
                 WS-AH-REM-BEN         ';'
                 WS-AH-EXPIRE-DATE     ';'
                 WS-AH-COMM            ';'
                 WS-AH-UEC             ';'
                 WS-ACCOUNT-NAME       ';'
                 WS-REPORT-CODE-1      ';'
                 WS-REPORT-CODE-2      ';'
                 WS-REPORT-CODE-3      ';'
                 WS-CLM-PAID-THRU-DT   ';'
100417           WS-LF-CANCEL-DATE     ';'
100417           WS-AH-CANCEL-DATE     ';'
100417           WS-VIN                ';'
100417           WS-CRED-BENE-NAME     ';'
                 WS-HEX-0A
                  DELIMITED BY '  ' INTO WS-SEND-BUF
              END-STRING
           ELSE
              STRING
                 WS-COMMENT            ';'  *> 0
                 WS-RESP               ';'
                 WS-CARRIER            ';'
                 WS-GROUPING           ';'
                 WS-STATE              ';'
                 WS-ACCOUNT            ';'
                 WS-CERT-EFF-DATE      ';'
                 WS-CERT-NO            ';'
                 WS-LAST-NAME          ';'
                 WS-FIRST-NAME         ';'
                 WS-MID-INIT           ';'
                 WS-LAST-JNAME         ';'
                 WS-FIRST-JNAME        ';'
                 WS-MID-JINIT          ';'
                 WS-POST-CARD-SW       ';'
                 WS-LF-STATUS          ';'
                 WS-LF-BEN-CODE        ';'  *> 16
                 WS-LF-BEN-CODE-DESC   ';'
                 WS-LF-TERM            ';'
                 WS-LF-REM-TERM        ';'
                 WS-LF-PREM            ';'
                 WS-LF-REFUND          ';'
                 WS-LF-METHOD          ';'
                 WS-LF-ORIG-BENEFIT    ';'
                 WS-LF-REM-BEN         ';'
                 WS-LF-EXPIRE-DATE     ';'
                 WS-LF-COMM            ';'
                 WS-LF-UEC             ';'
                 WS-AH-STATUS          ';'
                 WS-AH-BEN-CODE        ';'   *> 29
                 WS-AH-BEN-CODE-DESC   ';'
                 WS-AH-TERM            ';'
                 WS-AH-REM-TERM        ';'
                 WS-AH-PREM            ';'   *> 32
                 WS-AH-REFUND          ';'   *> 33
                 WS-AH-METHOD          ';'
                 WS-AH-ORIG-BENEFIT    ';'   *> 36
                 WS-AH-REM-BEN         ';'
                 WS-AH-EXPIRE-DATE     ';'
                 WS-AH-COMM            ';'
                 WS-AH-UEC             ';'
                 WS-ACCOUNT-NAME       ';'
                 WS-REPORT-CODE-1      ';'
                 WS-REPORT-CODE-2      ';'
                 WS-REPORT-CODE-3      ';'
                 WS-CLM-PAID-THRU-DT   ';'
100417           WS-LF-CANCEL-DATE     ';'
100417           WS-AH-CANCEL-DATE     ';'
100417           WS-VIN                ';'
100417           WS-CRED-BENE-NAME     ';'
                 'E'                   ';'
                  DELIMITED BY '  ' INTO WS-SEND-BUF
              END-STRING
           END-IF
           .
       0030-EXIT.
           EXIT.
       0050-BEGIN-REFUND-PROCESS.
           IF WS-ELCERT-ACT-NAME
              PERFORM 0060-FIND-FULL-KEY
                                       THRU 0060-EXIT
              MOVE AM-CARRIER          TO CLIENT-CAR
              MOVE AM-GROUPING         TO CLIENT-GRP
              MOVE AM-STATE            TO CLIENT-STATE
           END-IF
           PERFORM 0100-START-ELCERT   THRU 0100-EXIT
           IF NOT RESP-NORMAL
              MOVE ' CERT NOT FOUND - START ' TO WS-COMMENT
              MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
              MOVE WS-RESPONSE         TO WS-RESP
              PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
              PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
              PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
              GO TO 0010-RETURN
           END-IF
           PERFORM 0150-READ-NEXT-ELCERT
                                       THRU 0150-EXIT
      *    display ' came back from 0150 ' ws-response
           IF (NOT RESP-NORMAL AND NOT RESP-DUPKEY)
              DISPLAY ' CERT NOT FOUND '
              MOVE ' CERT NOT FOUND - READ ' TO WS-COMMENT
              MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
              MOVE WS-RESPONSE         TO WS-RESP
              PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
              PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
              PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
              GO TO 0010-RETURN
           END-IF
           PERFORM 0200-BEGIN-REFUND-PROCESS
                                       THRU 0200-EXIT UNTIL
              ELCERT-FINISHED
           .
       0050-EXIT.
           EXIT.
       0060-FIND-FULL-KEY.
           MOVE ' ' TO WS-STOP-SW
           MOVE CLIENT-ACCOUNT         TO WS-AM3-ACCOUNT
           MOVE LOW-VALUES             TO WS-AM3-EXP-DT
           
      * EXEC CICS STARTBR
      *       DATASET  ('ERACCT3')
      *       RIDFLD   (WS-AM3-KEY)
      *       GTEQ
      *       RESP (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERACCT3' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00004683' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303034363833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-AM3-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              PERFORM UNTIL TOLD-TO-STOP
                 
      * EXEC CICS READNEXT
      *             DATASET  ('ERACCT3')
      *             INTO     (ACCOUNT-MASTER)
      *             RIDFLD   (WS-AM3-KEY)
      *          END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 'ERACCT3' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00004691' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363931' TO DFHEIV0(25:11)
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
                 EVALUATE TRUE
                    WHEN (RESP-NORMAL)
                       AND (AM-ACCOUNT = CLIENT-ACCOUNT)
                       AND (AM-COMPANY-CD = WS-COMP-CD)
                       SET TOLD-TO-STOP TO TRUE
                    WHEN (AM-ACCOUNT = CLIENT-ACCOUNT)
                       AND (AM-COMPANY-CD NOT = WS-COMP-CD)
                       CONTINUE
                    WHEN AM-ACCOUNT NOT = CLIENT-ACCOUNT
                       MOVE 'INVALID ACCOUNT NO ' TO WS-COMMENT
                       MOVE 'X'        TO WS-LF-STATUS WS-AH-STATUS
                       MOVE ZEROS      TO WS-RESP
                       PERFORM 0030-BUILD-BUFFER
                                       THRU 0030-EXIT
                       PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
                       PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
                       GO TO 0010-RETURN
                 END-EVALUATE
              END-PERFORM
              
      * EXEC CICS ENDBR
      *          DATASET   ('ERACCT3')
      *       END-EXEC
           MOVE 'ERACCT3' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004717' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           ELSE
              MOVE 'INVALID ACCOUNT NO ' TO WS-COMMENT
              MOVE 'X'        TO WS-LF-STATUS WS-AH-STATUS
              MOVE ZEROS      TO WS-RESP
              PERFORM 0030-BUILD-BUFFER
                                       THRU 0030-EXIT
              PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
              PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
              GO TO 0010-RETURN
           END-IF
           .
       0060-EXIT.
           EXIT.
       0100-START-ELCERT.
           MOVE SPACES                 TO WS-SEND-BUF
           INITIALIZE WS-STRING-DATA
           MOVE LOW-VALUES             TO WS-CM-KEY
                                          WS-CM-KEY-A1
                                          WS-CM-KEY-A4
           MOVE WS-COMP-CD             TO WS-CM-COMPANY-CD
                                          WS-CM-COMPANY-CD-A1
                                          WS-CM-COMPANY-CD-A4
           MOVE CLIENT-VAL-DT          TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-VAL-DT
           ELSE
              MOVE 'BAD VAL DT CONVERT ' TO WS-COMMENT
              MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
              PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
              PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
              PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
              GO TO 0010-RETURN
           END-IF
           IF CLIENT-EFF-DT NOT = SPACES AND ZEROS
              MOVE CLIENT-EFF-DT       TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO WS-BIN-EFF-DT
              ELSE
                 MOVE 'BAD EFF DT CONVERT '
                                       TO WS-COMMENT
                 MOVE 'X'              TO WS-LF-STATUS WS-AH-STATUS
                 PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
                 PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
                 PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
                 GO TO 0010-RETURN
              END-IF
           END-IF
           PERFORM VARYING F1 FROM +1 BY +1 UNTIL
              (F1 > +10)
              OR (CLIENT-FIRST-NAME (F1:1) = SPACES OR LOW-VALUES)
           END-PERFORM
           IF F1 > +1
              SUBTRACT +1 FROM F1
           END-IF
           PERFORM VARYING L1 FROM +1 BY +1 UNTIL
              (L1 > +15)
              OR (CLIENT-LAST-NAME (L1:1) = SPACES OR LOW-VALUES)
           END-PERFORM
           IF L1 > +1
              SUBTRACT +1 FROM L1
           END-IF
           EVALUATE TRUE
              WHEN WS-ELCERT-FULL
                 MOVE CLIENT-CAR          TO WS-CM-CARRIER
                 MOVE CLIENT-GRP          TO WS-CM-GROUP
                 MOVE CLIENT-STATE        TO WS-CM-STATE
                 MOVE CLIENT-ACCOUNT      TO WS-CM-ACCOUNT
                 MOVE WS-BIN-EFF-DT       TO WS-CM-EFF-DT
041720           MOVE CLIENT-CERT-NO      TO WS-CM-CERT-NO
                 
      * EXEC CICS READ
      *             INTO    (CERTIFICATE-MASTER)
      *             DATASET ('ELCERT')
      *             RIDFLD  (WS-CM-KEY)
      *             RESP    (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00004799' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034373939' TO DFHEIV0(25:11)
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
              WHEN WS-ELCERT-ACT-NAME
                 MOVE CLIENT-CAR          TO WS-CM-CARRIER
                 MOVE CLIENT-GRP          TO WS-CM-GROUP
                 MOVE CLIENT-STATE        TO WS-CM-STATE
                 MOVE CLIENT-ACCOUNT      TO WS-CM-ACCOUNT
                 MOVE LOW-VALUES          TO WS-CM-EFF-DT
                 MOVE ZEROS               TO WS-CM-CERT-NO
                 
      * EXEC CICS STARTBR
      *             DATASET ('ELCERT')
      *             RIDFLD  (WS-CM-KEY)
      *             RESP    (WS-RESPONSE)
      *          END-EXEC
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00004812' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-CM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              WHEN WS-ELCERT-NAME
                 MOVE CLIENT-LAST-NAME TO WS-CM-LAST-NAME
                 MOVE SPACES           TO WS-CM-INITIALS
                 COMPUTE WS-LENGTH = L1 + +1
                 
      * EXEC CICS STARTBR
      *             DATASET   ('ELCERT2')
      *             RIDFLD    (WS-CM-KEY-A1)
      *             GENERIC
      *             KEYLENGTH (WS-LENGTH)
      *             GTEQ
      *             RESP      (WS-RESPONSE)
      *          END-EXEC
           MOVE 'ELCERT2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    G          &  N#00004821' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-CM-KEY-A1, 
                 WS-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
      *          DISPLAY ' STARTBR RESP ' WS-RESPONSE
      *          DISPLAY ' KEY ' WS-CM-KEY-A1
              WHEN WS-ELCERT-CERT-NO
      * DO NOT INCLUDE THE SUFFIX AS PART OF THE KEY
                 MOVE +11 TO WS-LENGTH
                 MOVE CLIENT-CERT-NO    TO WS-CM-CERT-NO-A4
                 
      * EXEC CICS STARTBR
      *             DATASET   ('ELCERT5')
      *             RIDFLD    (WS-CM-KEY-A4)
      *             GENERIC
      *             KEYLENGTH (WS-LENGTH)
      *             GTEQ
      *             RESP      (WS-RESPONSE)
      *          END-EXEC
           MOVE 'ELCERT5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    G          &  N#00004835' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-CM-KEY-A4, 
                 WS-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
080519        when ws-elcert-vin
080519           move low-values       to ws-cs-key
080519           move ws-comp-cd       to ws-cs-company-cd
080519           move client-car       to ws-cs-carrier
080519           move client-grp       to ws-cs-group
080519           move client-state     to ws-cs-state
080519           move client-account   to ws-cs-account
080519           
      * exec cics startbr
080519*             dataset   ('ELCRTT')
080519*             RIDFLD    (WS-CS-KEY)
080519*             GTEQ
080519*             RESP      (WS-RESPONSE)
080519*          END-EXEC
           MOVE 'ELCRTT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00004850' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-CS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-EVALUATE
           .
       0100-EXIT.
           EXIT.
       0150-READ-NEXT-ELCERT.
           EVALUATE TRUE
              WHEN WS-ELCERT-ACT-NAME
      *          DISPLAY ' ABOUT TO READ NEXT - ACT NAME'
                 
      * EXEC CICS READNEXT
      *             DATASET   ('ELCERT')
      *             RIDFLD    (WS-CM-KEY)
      *             INTO      (CERTIFICATE-MASTER)
      *             RESP      (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV12
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00004864' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383634' TO DFHEIV0(25:11)
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
              WHEN WS-ELCERT-NAME
      *          DISPLAY ' ABOUT TO READ NEXT - NAME'
                 
      * EXEC CICS READNEXT
      *             DATASET   ('ELCERT2')
      *             RIDFLD    (WS-CM-KEY-A1)
      *             INTO      (CERTIFICATE-MASTER)
      *             RESP      (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV12
           MOVE 'ELCERT2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00004872' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383732' TO DFHEIV0(25:11)
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
              WHEN WS-ELCERT-CERT-NO
                 
      * EXEC CICS READNEXT
      *             DATASET   ('ELCERT5')
      *             RIDFLD    (WS-CM-KEY-A4)
      *             INTO      (CERTIFICATE-MASTER)
      *             RESP      (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV12
           MOVE 'ELCERT5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00004879' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383739' TO DFHEIV0(25:11)
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
080519        WHEN WS-ELCERT-VIN
080519           
      * EXEC CICS READNEXT
080519*             DATASET   ('ELCRTT')
080519*             RIDFLD    (WS-CS-KEY)
080519*             INTO      (CERTIFICATE-TRAILERS)
080519*             RESP      (WS-RESPONSE)
080519*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV12
           MOVE 'ELCRTT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00004886' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383836' TO DFHEIV0(25:11)
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
           END-EVALUATE
           .
       0150-EXIT.
           EXIT.
       0200-BEGIN-REFUND-PROCESS.
           MOVE SPACES                 TO WS-BUFF-SENT-SW
                                          WS-ERACCT-SW
           MOVE ZEROS                  TO WS-NCB-DIFF-MONTHS
                                          WS-NCB-DIFF-ODD-DAYS
           INITIALIZE WS-STRING-DATA
           IF RESP-NORMAL OR RESP-DUPKEY
              EVALUATE TRUE
                 WHEN WS-ELCERT-ACT-NAME
                    EVALUATE TRUE
                       WHEN (CM-COMPANY-CD NOT = WS-COMP-CD)
                          OR (CM-CARRIER NOT = CLIENT-CAR)
                          OR (CM-GROUPING NOT = CLIENT-GRP)
                          OR (CM-STATE NOT = CLIENT-STATE)
                          OR (CM-ACCOUNT NOT = CLIENT-ACCOUNT)
                          SET ELCERT-FINISHED TO TRUE
                       WHEN (CM-INSURED-LAST-NAME (1:L1) NOT =
                          CLIENT-LAST-NAME (1:L1))
                          GO TO 0200-CONTINUE
                       WHEN ((CLIENT-EFF-DT NOT = SPACES)
                          AND (CM-CERT-EFF-DT NOT = WS-BIN-EFF-DT))
                          GO TO 0200-CONTINUE
                       WHEN ((CLIENT-FIRST-NAME NOT = SPACES)
                          AND (CM-INSURED-FIRST-NAME (1:F1) NOT =
                           CLIENT-FIRST-NAME (1:F1)))
                          GO TO 0200-CONTINUE
                       WHEN ((CLIENT-ACCOUNT NOT = SPACES)
                          AND (CM-ACCOUNT NOT = CLIENT-ACCOUNT))
                          GO TO 0200-CONTINUE
                    END-EVALUATE
                 WHEN WS-ELCERT-CERT-NO
                    IF (CM-COMPANY-CD NOT = WS-COMP-CD)
                       OR (CM-CERT-PRIME NOT = CLIENT-CERT-NO (1:10))
                       SET ELCERT-FINISHED TO TRUE
                    ELSE
                       IF ((CLIENT-EFF-DT NOT = SPACES)
                           AND (CM-CERT-EFF-DT NOT = WS-BIN-EFF-DT))
                                    OR
                          ((CLIENT-LAST-NAME NOT = SPACES)
                           AND (CM-INSURED-LAST-NAME (1:L1) NOT =
                              CLIENT-LAST-NAME (1:L1)))
                                    OR
                          ((CLIENT-FIRST-NAME NOT = SPACES)
                           AND (CM-INSURED-FIRST-NAME (1:F1) NOT =
                              CLIENT-FIRST-NAME (1:F1)))
                                    OR
                          ((CLIENT-ACCOUNT NOT = SPACES)
                           AND (CM-ACCOUNT NOT = CLIENT-ACCOUNT))
                           GO TO 0200-CONTINUE
                       END-IF
                    END-IF
                 WHEN WS-ELCERT-NAME
                    IF (CM-COMPANY-CD NOT = WS-COMP-CD)
                       OR (CM-INSURED-LAST-NAME (1:L1) NOT =
                          CLIENT-LAST-NAME (1:L1))
                        SET ELCERT-FINISHED TO TRUE
                    ELSE
                       IF ((CLIENT-EFF-DT NOT = SPACES)
                          AND (CM-CERT-EFF-DT NOT = WS-BIN-EFF-DT))
                                 OR
                       ((CLIENT-FIRST-NAME NOT = SPACES)
                        AND (CM-INSURED-FIRST-NAME (1:F1) NOT =
                           CLIENT-FIRST-NAME (1:F1)))
                                 OR
                       ((CLIENT-ACCOUNT NOT = SPACES)
                        AND (CM-ACCOUNT NOT = CLIENT-ACCOUNT))
                        GO TO 0200-CONTINUE
                       END-IF
                    END-IF
080519           WHEN WS-ELCERT-VIN
080519              evaluate true
080519                 when (CS-COMPANY-CD NOT = WS-COMP-CD)
080519                    OR (CS-STATE NOT = CLIENT-STATE)
080519                    OR (CS-ACCOUNT NOT = CLIENT-ACCOUNT)
080519                    SET ELCERT-FINISHED TO TRUE
080519                 when (cs-trailer-type  = 'C')
080519                    and (cs-vin-number = client-vin)
080519                    continue
080519                 when (cs-trailer-type  = 'C')
080519                    and (cs-vin-number(10:8) = client-vin(1:8))
080519                    continue
080519                 when other
080519                    go to 0200-continue
080519              end-evaluate
                 WHEN OTHER
                    SET ELCERT-FINISHED   TO TRUE
              END-EVALUATE
           ELSE
              SET ELCERT-FINISHED TO TRUE
           END-IF
080519     if ws-elcert-vin
080519        move cs-control-primary(1:33)
080519                                 to ws-cm-key
080519        
      * EXEC CICS READ
080519*          INTO    (CERTIFICATE-MASTER)
080519*          DATASET ('ELCERT')
080519*          RIDFLD  (WS-CM-KEY)
080519*          RESP    (WS-RESPONSE)
080519*       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00004989' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034393839' TO DFHEIV0(25:11)
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
080519        if not resp-normal
080519           set elcert-finished to true
080519        end-if
080519     end-if
      *       IF WS-ELCERT-CERT-NO
      *          IF (CM-COMPANY-CD NOT = WS-COMP-CD)
      *             OR (CM-CERT-NO NOT = CLIENT-CERT-NO)
      *                          OR
      *                ((CLIENT-EFF-DT NOT = SPACES)
      *                 AND (CM-CERT-EFF-DT NOT = WS-BIN-EFF-DT))
      *                          OR
      *                ((CLIENT-LAST-NAME NOT = SPACES)
      *                 AND (CM-INSURED-LAST-NAME NOT =
      *                    CLIENT-LAST-NAME))
      *                 SET ELCERT-FINISHED TO TRUE
      *          END-IF
      *          IF WS-ELCERT-NAME
      *             IF (CM-COMPANY-CD NOT = WS-COMP-CD)
      *                OR (CM-INSURED-LAST-NAME NOT =
      *                   CLIENT-LAST-NAME)
      *                            OR
      *                ((CLIENT-EFF-DT NOT = SPACES)
      *                 AND (CM-CERT-EFF-DT NOT = WS-BIN-EFF-DT))
      *                 SET ELCERT-FINISHED TO TRUE
      *             END-IF
      *          ELSE
      *             SET ELCERT-FINISHED   TO TRUE
      *          END-IF
           IF NOT WS-ELCERT-FULL
              IF ELCERT-FINISHED
                 GO TO 0200-EXIT
              END-IF
           END-IF
           IF CM-ENTRY-STATUS = 'M' OR '9' OR 'D' OR 'V' OR 'U'
              MOVE 'CERT NOT ACTIVE '  TO WS-COMMENT
              MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
              PERFORM 0030-BUILD-BUFFER
                                       THRU 0030-EXIT
              PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
              GO TO 0200-CONTINUE
           END-IF
090314     if (ws-epiq-request)
090314        and (not EPIQ-CLASS)
090314        go to 0200-continue
090314     end-if
090314
090314     if ws-epiq-request
090314        add +1 to ws-epiq-max
090314        if ws-epiq-max > +100
090314           go to 0200-continue
090314        end-if
090314     end-if
           PERFORM 0300-GET-ELCNTL-RECORDS
                                       THRU 0300-EXIT
           MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE WS-BIN-VAL-DT          TO DC-BIN-DATE-2
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-ELAPSED-MONTHS   TO WS-NCB-DIFF-MONTHS
              MOVE DC-ODD-DAYS-OVER    TO WS-NCB-DIFF-ODD-DAYS
           END-IF
           PERFORM 0400-PROCESS-LIFE   THRU 0400-EXIT
           PERFORM 0500-PROCESS-AH     THRU 0500-EXIT
100417     perform 0610-get-ermail     thru 0610-exit
100417     perform 0620-get-elcrtt     thru 0620-exit
           PERFORM 0030-BUILD-BUFFER   THRU 0030-EXIT
           IF NOT ALREADY-SENT
              PERFORM 0020-SEND-BUFFER THRU 0020-EXIT
           END-IF
           .
       0200-CONTINUE.
           PERFORM 0150-READ-NEXT-ELCERT
                                       THRU 0150-EXIT
           .
       0200-EXIT.
           EXIT.
       0300-GET-ELCNTL-RECORDS.
           PERFORM 0310-GET-COMPANY    THRU 0310-EXIT
           IF NOT RESP-NORMAL
              MOVE ' COMPANY RECORD NOT FOUND '
                                       TO WS-COMMENT
              MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
              PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
              PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
              PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
              GO TO 0010-RETURN
           END-IF
           IF CM-LF-BENEFIT-CD NOT = '  ' AND '00'
              PERFORM 0320-GET-LIFE-RECORD
                                       THRU 0320-EXIT
              IF NOT RESP-NORMAL
                 MOVE ' LIFE BENEFIT RECORD NOT FOUND '
                                       TO WS-COMMENT
                 MOVE 'X'              TO WS-LF-STATUS WS-AH-STATUS
                 PERFORM 0030-BUILD-BUFFER
                                       THRU 0030-EXIT
                 PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
                 PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
                 GO TO 0010-RETURN
              END-IF
           END-IF
           IF CM-AH-BENEFIT-CD NOT = '  ' AND '00'
              PERFORM 0330-GET-AH-RECORD
                                       THRU 0330-EXIT
              IF NOT RESP-NORMAL
                 MOVE ' AH BENEFIT RECORD NOT FOUND '
                                       TO WS-COMMENT
                 MOVE 'X'              TO WS-LF-STATUS WS-AH-STATUS
                 PERFORM 0030-BUILD-BUFFER
                                       THRU 0030-EXIT
                 PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
                 PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
                 GO TO 0010-RETURN
              END-IF
           END-IF
           PERFORM 0340-GET-STATE-RECORD
                                       THRU 0340-EXIT
           IF NOT RESP-NORMAL
              MOVE ' STATE RECORD NOT FOUND '
                                       TO WS-COMMENT
              MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
              PERFORM 0030-BUILD-BUFFER
                                       THRU 0030-EXIT
              PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
              PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
              GO TO 0010-RETURN
           END-IF
           PERFORM 0350-GET-ERACCT     THRU 0350-EXIT
           IF NOT ERACCT-FOUND
              MOVE ' ACCOUNT NOT FOUND '
                                       TO WS-COMMENT
              MOVE 'X'                 TO WS-LF-STATUS WS-AH-STATUS
              PERFORM 0030-BUILD-BUFFER
                                       THRU 0030-EXIT
              PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
              PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
              GO TO 0010-RETURN
           ELSE
              MOVE AM-NAME             TO WS-ACCOUNT-NAME
              MOVE AM-REPORT-CODE-1    TO WS-REPORT-CODE-1
              MOVE AM-REPORT-CODE-2    TO WS-REPORT-CODE-2
              MOVE AM-REPORT-CODE-3    TO WS-REPORT-CODE-3
           END-IF
           .
       0300-EXIT.
           EXIT.
       0310-GET-COMPANY.
           MOVE WS-COMP-ID             TO  WS-CF-COMPANY-ID
           MOVE '1'                    TO  WS-CF-RECORD-TYPE
           MOVE SPACES                 TO  WS-CF-ACCESS
           MOVE +0                     TO  WS-CF-SEQUENCE-NO
           
      * EXEC CICS READ
      *       INTO    (CONTROL-FILE)
      *       DATASET ('ELCNTL')
      *       RIDFLD  (WS-CF-KEY)
      *       RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005160' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303035313630' TO DFHEIV0(25:11)
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
           IF (RESP-NORMAL)
              AND (CF-COMPANY-ID = WS-COMP-ID)
              AND (CF-RECORD-TYPE = '1')
              MOVE CF-CR-REM-TERM-CALC TO WS-CF-CR-REM-TERM-CALC
              MOVE CF-CR-R78-METHOD    TO WS-CF-CR-R78-METHOD
              MOVE CF-LIFE-OVERRIDE-L1 TO WS-LF-OVERRIDE-L1
              MOVE CF-AH-OVERRIDE-L1   TO WS-AH-OVERRIDE-L1
              IF CF-DEFAULT-APR NUMERIC
                 MOVE CF-DEFAULT-APR   TO WS-CF-DEFAULT-APR
              ELSE
                 MOVE ZEROS            TO WS-CF-DEFAULT-APR
              END-IF
              IF CF-VALID-REM-TRM-OPTION
                 MOVE CF-REM-TRM-CALC-OPTION
                                       TO WS-CF-REM-TRM-CALC-OPTION
              ELSE
                 MOVE SPACES           TO WS-CF-REM-TRM-CALC-OPTION
              END-IF
           END-IF
           .
       0310-EXIT.
           EXIT.
       0320-GET-LIFE-RECORD.
           MOVE '4'                    TO WS-CF-RECORD-TYPE
           MOVE CM-LF-BENEFIT-CD       TO WS-CF-BENEFIT-NO
                                          WS-LF-BEN-CODE
           MOVE +0                     TO WS-CF-SEQUENCE-NO
           
      * EXEC CICS READ
      *       INTO    (CONTROL-FILE)
      *       DATASET ('ELCNTL')
      *       RIDFLD  (WS-CF-KEY)
      *       GTEQ
      *       RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00005193' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303035313933' TO DFHEIV0(25:11)
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
           IF RESP-NORMAL
              AND (CF-COMPANY-ID  = WS-COMP-ID)
              AND (CF-RECORD-TYPE = '4')
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (S1 > +8)
                 OR (CM-LF-BENEFIT-CD = CF-BENEFIT-CODE (S1))
              END-PERFORM
              IF S1 > +8
                 CONTINUE
              ELSE
                 MOVE CF-BENEFIT-DESCRIP (S1)
                                       TO WS-LF-BEN-CODE-DESC
                 MOVE CF-LF-COVERAGE-TYPE (S1)
                                       TO WS-CF-LF-COVERAGE-TYPE
                 IF CF-JOINT-INDICATOR (S1) NOT = 'J'
                    MOVE 'S'           TO WS-LF-COVERAGE-TYPE
                 ELSE
                    MOVE 'J'           TO WS-LF-COVERAGE-TYPE
                 END-IF
                 IF CF-CO-REM-TERM-CALC (S1) > '0'
                     MOVE CF-CO-REM-TERM-CALC (S1)
                                       TO WS-LF-CO-REM-TERM-CALC
                 END-IF
                 IF CF-CO-EARNINGS-CALC (S1)  > ' '
                     MOVE CF-CO-EARNINGS-CALC (S1)
                                       TO WS-LF-CO-EARNINGS-CALC
                                          WS-LF-CO-REFUND-CALC
                 END-IF
                 IF CF-SPECIAL-CALC-CD (S1) > ' '
                     MOVE CF-SPECIAL-CALC-CD (S1)
                                       TO WS-LF-SPECIAL-CALC-CD
                 END-IF
                 IF CF-CO-REFUND-CALC (S1) > '0'
                     MOVE CF-CO-REFUND-CALC (S1)
                                       TO WS-LF-CO-REFUND-CALC
              END-IF
           END-IF
           .
       0320-EXIT.
           EXIT.
       0330-GET-AH-RECORD.
           MOVE '5'                    TO WS-CF-RECORD-TYPE
           MOVE CM-AH-BENEFIT-CD       TO WS-CF-BENEFIT-NO
           MOVE +0                     TO WS-CF-SEQUENCE-NO
           
      * EXEC CICS READ
      *       INTO    (CONTROL-FILE)
      *       DATASET ('ELCNTL')
      *       RIDFLD  (WS-CF-KEY)
      *       GTEQ
      *       RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00005244' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303035323434' TO DFHEIV0(25:11)
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
           IF RESP-NORMAL
              AND (CF-COMPANY-ID  = WS-COMP-ID)
              AND (CF-RECORD-TYPE = '5')
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (S1 > +8)
                 OR (CM-AH-BENEFIT-CD = CF-BENEFIT-CODE (S1))
              END-PERFORM
              IF S1 > +8
                 CONTINUE
              ELSE
                 MOVE CF-BENEFIT-DESCRIP (S1)
                                       TO WS-AH-BEN-CODE-DESC
                 IF CF-JOINT-INDICATOR (S1) NOT = 'J'
                    MOVE 'S'           TO WS-AH-COVERAGE-TYPE
                 ELSE
                    MOVE 'J'           TO WS-AH-COVERAGE-TYPE
                 END-IF
                 IF CF-CO-REM-TERM-CALC (S1) > '0'
                    MOVE CF-CO-REM-TERM-CALC (S1)
                                       TO WS-AH-CO-REM-TERM-CALC
                 END-IF
                 IF CF-CO-EARNINGS-CALC (S1) > ' '
                    MOVE CF-CO-EARNINGS-CALC (S1)
                                       TO WS-AH-CO-EARNINGS-CALC
                                          WS-AH-CO-REFUND-CALC
                 END-IF
                 IF CF-SPECIAL-CALC-CD (S1) >  ' '
                    MOVE CF-SPECIAL-CALC-CD (S1)
                                       TO WS-AH-SPECIAL-CALC-CD
                 END-IF
                 IF CF-CO-REFUND-CALC (S1) > '0'
                    MOVE CF-CO-REFUND-CALC (S1)
                                       TO WS-AH-CO-REFUND-CALC
                 END-IF
                 IF CF-BENEFIT-CATEGORY (S1) > ' '
                    MOVE CF-BENEFIT-CATEGORY (S1)
                                       TO WS-AH-BEN-CATEGORY
                 END-IF
              END-IF
           END-IF
           .
       0330-EXIT.
           EXIT.
       0340-GET-STATE-RECORD.
           MOVE WS-COMP-ID             TO WS-CF-COMPANY-ID
           MOVE '3'                    TO WS-CF-RECORD-TYPE
           MOVE CM-STATE               TO WS-CF-ACCESS
           MOVE +0                     TO WS-CF-SEQUENCE-NO
           
      * EXEC CICS READ
      *       INTO    (CONTROL-FILE)
      *       DATASET ('ELCNTL')
      *       RIDFLD  (WS-CF-KEY)
      *       RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005299' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303035323939' TO DFHEIV0(25:11)
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
           IF (RESP-NORMAL)
              AND (CF-COMPANY-ID = WS-COMP-ID)
              AND (CF-RECORD-TYPE = '3')
              AND (CF-STATE-CODE = CM-STATE)
              MOVE CF-ST-FREE-LOOK-PERIOD
                                       TO WS-FREE-LOOK
              MOVE CF-STATE-ABBREVIATION
                                       TO WS-STATE-ABBREVIATION
              MOVE CF-ST-FST-PMT-DAYS-CHG
                                       TO  WS-STATE-EXT-DAYS-CHG
              IF CM-LF-BENEFIT-CD NOT = '00' AND '  '
                 MOVE '0'              TO WS-LF-ST-REM-TERM-CALC
                 IF CF-ST-RT-CALC NOT = SPACES
                    MOVE CF-ST-RT-CALC TO WS-LF-ST-REM-TERM-CALC
                 END-IF
                 IF WS-CF-LF-COVERAGE-TYPE = 'R'
                    IF CF-ST-RF-LR-CALC > '0'
                       MOVE CF-ST-RF-LR-CALC
                                       TO WS-LF-ST-REFUND-CALC
                    END-IF
                    IF WS-LF-CO-EARNINGS-CALC = 'N' OR '5'
                       IF CF-ST-RF-LN-CALC > '0'
                          MOVE CF-ST-RF-LN-CALC
                                       TO WS-LF-ST-REFUND-CALC
                       END-IF
                    END-IF
                 ELSE
                    IF CF-ST-RF-LL-CALC > '0'
                       MOVE CF-ST-RF-LL-CALC
                                       TO WS-LF-ST-REFUND-CALC
                    END-IF
                 END-IF
                 PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                    (S1 > 50)
                    OR ((WS-LF-OVERRIDE-L1 = CF-ST-BENEFIT-KIND (S1))
                       AND (CF-ST-BENEFIT-CD (S1) = CM-LF-BENEFIT-CD))
                 END-PERFORM
                 IF S1 < +51
                    IF CF-ST-REM-TERM-CALC (S1) > '0'
                       MOVE CF-ST-REM-TERM-CALC (S1)
                                       TO WS-LF-ST-REM-TERM-CALC
                    END-IF
                    IF CF-ST-REFUND-CALC (S1) > '0'
                       MOVE CF-ST-REFUND-CALC (S1)
                                       TO WS-LF-ST-REFUND-CALC
                    END-IF
                 END-IF
              END-IF
              IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
                 MOVE '0'              TO WS-AH-ST-REM-TERM-CALC
                 IF CF-ST-RT-CALC NOT = SPACES
                    MOVE CF-ST-RT-CALC TO WS-AH-ST-REM-TERM-CALC
                 END-IF
                 IF CF-ST-RF-AH-CALC > '0'
                    MOVE CF-ST-RF-AH-CALC
                                       TO WS-AH-ST-REFUND-CALC
                 END-IF
                 IF WS-AH-SPECIAL-CALC-CD = 'C'
                    IF CF-ST-RF-CP-CALC > '0'
                       MOVE CF-ST-RF-CP-CALC
                                       TO WS-AH-ST-REFUND-CALC
                    END-IF
                 END-IF
                 PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                    (S1 > 50)
                    OR ((WS-AH-OVERRIDE-L1 = CF-ST-BENEFIT-KIND (S1))
                       AND (CF-ST-BENEFIT-CD (S1) = CM-AH-BENEFIT-CD))
                 END-PERFORM
                 IF S1 < +51
                    IF CF-ST-REM-TERM-CALC (S1) > '0'
                       MOVE CF-ST-REM-TERM-CALC (S1)
                                       TO WS-AH-ST-REM-TERM-CALC
                    END-IF
                    IF CF-ST-REFUND-CALC (S1) > '0'
                       MOVE CF-ST-REFUND-CALC (S1)
                                       TO WS-AH-ST-REFUND-CALC
                    END-IF
                 END-IF
              END-IF
           END-IF
           .
       0340-EXIT.
           EXIT.
       0350-GET-ERACCT.
           MOVE '0'                    TO WS-AM-EARN-METHOD-L
                                          WS-AM-EARN-METHOD-R
                                          WS-AM-EARN-METHOD-A
                                          WS-LF-AM-REM-TERM-CALC
                                          WS-AH-AM-REM-TERM-CALC
           MOVE CM-COMPANY-CD          TO WS-AM-COMPANY-CD
           MOVE CM-CARRIER             TO WS-AM-CARRIER
           MOVE CM-GROUPING            TO WS-AM-GROUPING
           MOVE CM-STATE               TO WS-AM-STATE
           MOVE CM-ACCOUNT             TO WS-AM-ACCOUNT
           MOVE CM-CERT-EFF-DT         TO WS-AM-EXPIRATION-DT
           MOVE LOW-VALUES             TO WS-AM-FILLER
           MOVE ' '                    TO WS-STOP-SW
           
      * EXEC CICS STARTBR
      *       DATASET  ('ERACCT')
      *       RIDFLD   (WS-AM-KEY)
      *       GTEQ
      *       RESP (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005402' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303035343032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-AM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              PERFORM UNTIL TOLD-TO-STOP
                 
      * EXEC CICS READNEXT
      *             DATASET  ('ERACCT')
      *             INTO     (ACCOUNT-MASTER)
      *             RIDFLD   (WS-AM-KEY)
      *          END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00005410' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343130' TO DFHEIV0(25:11)
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
                 IF RESP-NORMAL
                    EVALUATE TRUE
                       WHEN AM-CONTROL-PRIMARY (1:20) NOT =
                          CM-CONTROL-PRIMARY (1:20)
                          SET TOLD-TO-STOP TO TRUE
                       WHEN (AM-CONTROL-PRIMARY (1:20) =
                          CM-CONTROL-PRIMARY (1:20))
                          AND (CM-CERT-EFF-DT >= AM-EFFECTIVE-DT)
                          AND (CM-CERT-EFF-DT < AM-EXPIRATION-DT)
                          SET ERACCT-FOUND      TO TRUE
                          SET TOLD-TO-STOP TO TRUE
                    END-EVALUATE
                 ELSE
                    SET TOLD-TO-STOP TO TRUE
                 END-IF
              END-PERFORM
              
      * EXEC CICS ENDBR
      *          DATASET   ('ERACCT')
      *       END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005431' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           .
       0350-EXIT.
           EXIT.
       0400-PROCESS-LIFE.
           MOVE SPACES                 TO WS-BUFF-SENT-SW
           MOVE CM-INSURED-LAST-NAME   TO WS-LAST-NAME
           MOVE CM-INSURED-FIRST-NAME  TO WS-FIRST-NAME
           MOVE CM-INSURED-INITIAL2    TO WS-MID-INIT
           MOVE CM-JT-LAST-NAME        TO WS-LAST-JNAME
           MOVE CM-JT-FIRST-NAME       TO WS-FIRST-JNAME
           MOVE CM-JT-INITIAL          TO WS-MID-JINIT
           MOVE CM-POST-CARD-IND       TO WS-POST-CARD-SW
           MOVE CM-CARRIER             TO WS-CARRIER
           MOVE CM-GROUPING            TO WS-GROUPING
           MOVE CM-STATE               TO WS-STATE
           MOVE CM-ACCOUNT             TO WS-ACCOUNT
           MOVE CM-CERT-NO             TO WS-CERT-NO
           MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
           MOVE ' '                    TO  DC-OPTION-CODE.
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO WS-CERT-EFF-DATE
           ELSE
              MOVE 99999999            TO WS-CERT-EFF-DATE
           END-IF
           IF CM-LF-BENEFIT-CD = '  ' OR '00'
              GO TO 0400-EXIT
           END-IF
           MOVE ZEROS                  TO WS-TOT-LF-RFND
                                          WS-TOT-LF-PREM
                                          WS-TOT-LF-COMM
100417                                    WS-LF-CANCEL-DATE
           MOVE CM-LF-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
           MOVE ' '                    TO  DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO WS-LF-EXPIRE-DATE
           ELSE
              MOVE ZEROS               TO WS-LF-EXPIRE-DATE
           END-IF
100417     IF CM-LF-DEATH-DT NOT = LOW-VALUES
100417        MOVE CM-LF-DEATH-DT      TO DC-BIN-DATE-1
100417        MOVE ' '                 TO DC-OPTION-CODE
100417
100417        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
100417        IF NO-CONVERSION-ERROR
100417           MOVE DC-GREG-DATE-CYMD
100417                                 TO WS-LF-CANCEL-DATE
100417        ELSE
100417           MOVE ZEROS            TO WS-LF-CANCEL-DATE
100417        END-IF
100417     END-IF
100417     IF CM-LF-CANCEL-DT NOT = LOW-VALUES
100417        MOVE CM-LF-CANCEL-DT     TO DC-BIN-DATE-1
100417        MOVE ' '                 TO DC-OPTION-CODE
100417
100417        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
100417        IF NO-CONVERSION-ERROR
100417           MOVE DC-GREG-DATE-CYMD
100417                                 TO WS-LF-CANCEL-DATE
100417        ELSE
100417           MOVE ZEROS            TO WS-LF-CANCEL-DATE
100417        END-IF
100417     END-IF
           MOVE WS-LF-OVERRIDE-L1      TO CP-LIFE-OVERRIDE-CODE
           MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT
           MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE
           MOVE WS-BIN-VAL-DT          TO CP-VALUATION-DT
           MOVE CM-STATE               TO CP-STATE
           MOVE WS-STATE-ABBREVIATION  TO CP-STATE-STD-ABBRV
           MOVE WS-CF-LF-COVERAGE-TYPE TO CP-BENEFIT-TYPE
           MOVE WS-LF-SPECIAL-CALC-CD  TO CP-SPECIAL-CALC-CD
           MOVE CM-PAY-FREQUENCY       TO CP-PAY-FREQUENCY
           MOVE CM-LOAN-APR            TO CP-LOAN-APR
           MOVE '2'                    TO CP-PROCESS-TYPE
           MOVE WS-COMP-ID             TO CP-COMPANY-ID
           MOVE CM-COMPANY-CD          TO CP-COMPANY-CD
           MOVE WS-ACCT-USER-FLD-5     TO CP-ACCT-FLD-5
           MOVE WS-CF-REM-TRM-CALC-OPTION
                                       TO CP-REM-TRM-CALC-OPTION
           MOVE WS-CF-CR-REM-TERM-CALC TO CP-REM-TERM-METHOD
           IF WS-LF-CO-REM-TERM-CALC > '0'
              MOVE WS-LF-CO-REM-TERM-CALC
                                       TO CP-REM-TERM-METHOD
           END-IF
           IF WS-LF-ST-REM-TERM-CALC > '0'
               MOVE WS-LF-ST-REM-TERM-CALC
                                       TO  CP-REM-TERM-METHOD.
           IF WS-LF-AM-REM-TERM-CALC > '0'
               MOVE WS-LF-AM-REM-TERM-CALC
                                       TO  CP-REM-TERM-METHOD.
           IF (WS-COMP-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'WI')
              MOVE '7'                 TO CP-REM-TERM-METHOD
              MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
           END-IF
           IF (WS-COMP-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'MO')
              AND (CM-CERT-EFF-DT >= X'9B41')
              AND (CM-CERT-EFF-DT <= X'A2FB')
              MOVE '7'                 TO CP-REM-TERM-METHOD
              MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
           END-IF
           MOVE CM-LF-ORIG-TERM        TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM
                                          WS-LF-TERM
           IF CP-TRUNCATED-LIFE
               MOVE CM-LOAN-TERM       TO  CP-LOAN-TERM.
           IF CP-TERM-IS-DAYS
              IF CM-LF-TERM-IN-DAYS NUMERIC
                 MOVE CM-LF-TERM-IN-DAYS
                                       TO CP-TERM-OR-EXT-DAYS
              ELSE
                 MOVE ZEROS            TO CP-TERM-OR-EXT-DAYS
              END-IF
           ELSE
              IF CM-PMT-EXTENSION-DAYS NUMERIC
                 MOVE CM-PMT-EXTENSION-DAYS
                                       TO CP-TERM-OR-EXT-DAYS
              ELSE
                 MOVE ZEROS            TO CP-TERM-OR-EXT-DAYS
              END-IF
           END-IF
           MOVE WS-FREE-LOOK           TO CP-FREE-LOOK
           IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
              (WS-LF-SPECIAL-CALC-CD NOT = 'L')
              ADD +1   TO CP-ORIGINAL-TERM CP-LOAN-TERM
           END-IF
           DISPLAY ' STARTING OPEN LIFE CLAIM TEST '
           MOVE LOW-VALUES             TO WS-LF-BIN-PAID-THRU-DT
           MOVE ' '                    TO WS-CLM-STOP-SW
           MOVE CM-COMPANY-CD          TO ELMSTR-COMP-CD
           MOVE CM-CERT-NO             TO ELMSTR-CERT-NO
           
      * EXEC CICS STARTBR
      *       DATASET     ('ELMSTR5')
      *       RIDFLD      (ELMSTR-KEY)
      *       RESP        (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005568' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303035353638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           DISPLAY ' AFTER START ' WS-RESPONSE
           IF RESP-NORMAL
              PERFORM WITH TEST AFTER UNTIL I-SAY-TO-STOP
                 
      * EXEC CICS READNEXT
      *             DATASET   ('ELMSTR5')
      *             RIDFLD    (ELMSTR-KEY)
      *             INTO      (CLAIM-MASTER)
      *             RESP      (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005576' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303035353736' TO DFHEIV0(25:11)
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
              IF (RESP-NORMAL OR RESP-DUPKEY)
                 AND (CM-COMPANY-CD = CL-COMPANY-CD)
                 AND (CM-CERT-NO = CL-CERT-NO)
                 IF (CM-ACCOUNT = CL-CERT-ACCOUNT)
                    AND (CM-CERT-EFF-DT = CL-CERT-EFF-DT)
100518              IF CL-CLAIM-TYPE = 'L' OR 'O'
                       IF CM-AH-BENEFIT-CD NOT = '  ' AND '00'
                          MOVE 'D'     TO WS-AH-STATUS
                       END-IF
                       IF CLAIM-IS-OPEN
                          MOVE ' OPEN LIFE CLAIM '
                                       TO WS-COMMENT
                          MOVE 'L'     TO WS-LF-STATUS
                          GO TO 0400-EXIT
                       ELSE
                          MOVE ' DEATH CLAIM APPLIED '
                                       TO WS-COMMENT
                          MOVE 'D'     TO WS-LF-STATUS
      *                   IF CL-PAID-THRU-DT > WS-LF-BIN-PAID-THRU-DT
      *                      MOVE CL-PAID-THRU-DT
      *                                TO WS-LF-BIN-PAID-THRU-DT
      *                   END-IF
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 SET I-SAY-TO-STOP TO TRUE
              END-IF
              END-PERFORM
              
      * EXEC CICS ENDBR
      *          DATASET   ('ELMSTR5')
      *       END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005611' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           IF WS-LF-BIN-PAID-THRU-DT NOT = LOW-VALUES
      *       THIS WILL BE TRUE IF WE FOUND A CLOSED LIFE CLAIM
              MOVE WS-LF-BIN-PAID-THRU-DT
                                       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD
                                       TO WS-CLM-PAID-THRU-DT
              ELSE
                 MOVE 99999999         TO WS-CLM-PAID-THRU-DT
              END-IF
      *       IF WS-BIN-VAL-DT <= WS-LF-BIN-PAID-THRU-DT
      *          MOVE 'CAN DT =< CLM PD THRU DT'
      *                                TO WS-COMMENT
      *          MOVE 'P'              TO WS-LF-STATUS
      *          GO TO 0400-EXIT
      *       END-IF
              GO TO 0400-EXIT
           END-IF
           PERFORM 0700-LINK-REM-TERM  THRU  0700-EXIT
           IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
              (WS-LF-SPECIAL-CALC-CD NOT = 'L')
              MOVE CP-REMAINING-TERM-1   TO WS-BALLOON-RTRM
              COMPUTE CP-REMAINING-TERM-1 =
                      CP-REMAINING-TERM-1 - +1
              COMPUTE CP-REMAINING-TERM-2 =
                      CP-REMAINING-TERM-2 - +1
           END-IF
           IF CP-REMAINING-TERM-1 NEGATIVE
              MOVE ZEROS               TO CP-REMAINING-TERM-1
           END-IF
           IF CP-REMAINING-TERM-2 NEGATIVE
              MOVE ZEROS               TO CP-REMAINING-TERM-2
           END-IF
           IF CM-LIFE-COMM-PCT NOT NUMERIC
              MOVE ZERO                TO CM-LIFE-COMM-PCT
           END-IF
           IF CM-LIFE-COMM-PCT > ZEROS
              COMPUTE WS-TOT-LF-COMM = CM-LIFE-COMM-PCT *
                 (CM-LF-PREMIUM-AMT + CM-LF-ALT-PREMIUM-AMT)
           END-IF
           MOVE WS-TOT-LF-COMM         TO WS-LF-COMM
           MOVE CP-REMAINING-TERM-1    TO WS-LF-REM-TERM
           IF CM-LF-CURRENT-STATUS = '8'
              IF CM-LF-CANCEL-DT NOT = LOW-VALUES
                 MOVE ' LIFE COVERAGE PREVIOUSLY CANCELLED '
                                       TO WS-COMMENT
                 MOVE 'C'              TO WS-LF-STATUS
100417           move cm-lf-itd-cancel-amt
100417                                 to ws-lf-refund
pemtst*          PERFORM 0030-BUILD-BUFFER
pemtst*                                THRU 0030-EXIT
pemtst*          PERFORM 0020-SEND-BUFFER
pemtst*                                THRU 0020-EXIT
pemtst*          SET ALREADY-SENT      TO TRUE
pemtst*          MOVE SPACES           TO WS-COMMENT
                 GO TO 0400-EXIT
      *          PERFORM 0025-CLOSE-SOCKET
      *                                THRU 0025-EXIT
      *          GO TO 0010-RETURN
              END-IF
           END-IF
           IF CM-LF-CURRENT-STATUS = '7'
              IF CM-LF-DEATH-DT NOT = LOW-VALUES
                 MOVE ' DEATH CLAIM APPLIED '
                                       TO WS-COMMENT
                 MOVE 'D'              TO WS-LF-STATUS
pemtst*          PERFORM 0030-BUILD-BUFFER
pemtst*                                THRU 0030-EXIT
pemtst*          IF NOT ALREADY-SENT
pemtst*             PERFORM 0020-SEND-BUFFER
pemtst*                                THRU 0020-EXIT
pemtst*             SET ALREADY-SENT   TO TRUE
pemtst*          END-IF
                 GO TO 0400-EXIT
      *          PERFORM 0025-CLOSE-SOCKET
      *                                THRU 0025-EXIT
      *          GO TO 0010-RETURN
              END-IF
           END-IF
           IF ((CM-LF-LOAN-EXPIRE-DT < WS-BIN-VAL-DT)
              AND (CM-LF-LOAN-EXPIRE-DT > LOW-VALUES))
              MOVE 'LF COVERAGE EXPIRED '
                                       TO WS-COMMENT
              MOVE 'E'                 TO WS-LF-STATUS
              GO TO 0400-EXIT
           END-IF
           IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
              (WS-LF-SPECIAL-CALC-CD NOT = 'L')
              MOVE WS-BALLOON-RTRM     TO CP-REMAINING-TERM
           ELSE
              MOVE CP-REMAINING-TERM-2 TO CP-REMAINING-TERM
           END-IF
           MOVE WS-LF-CO-EARNINGS-CALC TO  CP-EARNING-METHOD.
           MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT.
071112*    MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE
071112     if ws-comp-id = 'AHL'
071112        move cm-lf-class-cd      to cp-class-code
071112        if cp-class-code = spaces
071112           move zeros            to cp-class-code
071112        end-if
071112     else
071112        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
071112     end-if
100417     IF (ws-comp-id = 'DCC' or 'VPP')
              AND (AM-DCC-PRODUCT-CODE = 'DDF')
              MOVE ' '                 TO WS-PDEF-RECORD-SW
              PERFORM 0730-GET-DDF-FACTORS
                                       THRU 0730-EXIT
              IF PDEF-FOUND
080322           MOVE CL-INSURED-BIRTH-DT
080322                                  TO DC-BIN-DATE-1
080322           MOVE CL-INCURRED-DT    TO DC-BIN-DATE-2
080322           MOVE '1'               TO DC-OPTION-CODE
080322           PERFORM 9700-DATE-LINK   THRU 9700-EXIT
080322           COMPUTE WS-ATT-AGE =
080322               DC-ELAPSED-MONTHS / 12
080322           MOVE ZEROS TO DC-ELAPSED-MONTHS DC-ELAPSED-DAYS
080322
                 PERFORM VARYING P1 FROM +1 BY +1 UNTIL
080322              (P1 > +11)
080322              OR (PD-PROD-CODE (P1) = 'L' or 'O'
080322               AND PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
                 END-PERFORM
080322           IF P1 < +12
                    MOVE PD-MAX-AMT (P1) TO CP-R-MAX-TOT-BEN
                 END-IF
              END-IF
           END-IF
           PERFORM 0710-LINK-REM-AMOUNT THRU 0710-EXIT
      *    MOVE CM-LF-BENEFIT-AMT      TO WS-LF-ORIG-BENEFIT
           COMPUTE WS-LF-ORIG-BENEFIT = CM-LF-BENEFIT-AMT +
              CM-LF-ALT-BENEFIT-AMT
      *    ADD CM-LF-ALT-BENEFIT-AMT   TO WS-LF-ORIG-BENEFIT
           MOVE CP-REMAINING-AMT       TO CP-REMAINING-BENEFIT
                                          WS-LF-REM-BEN
           move cm-lf-premium-amt      to ws-tot-lf-prem
           if ws-lf-co-earnings-calc = 'B'
              COMPUTE WS-TOT-LF-PREM = ws-tot-lf-prem
                 + CM-LF-ALT-PREMIUM-AMT
           end-if
           MOVE WS-TOT-LF-PREM         TO WS-LF-PREM
           IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
              (WS-LF-SPECIAL-CALC-CD NOT = 'L')
              MOVE WS-BALLOON-RTRM     TO CP-REMAINING-TERM
           ELSE
              MOVE CP-REMAINING-TERM-1 TO  CP-REMAINING-TERM
           END-IF
           MOVE WS-LF-CO-REFUND-CALC   TO  CP-EARNING-METHOD.
           MOVE WS-LF-CO-EARNINGS-CALC TO  CP-RATING-METHOD.
           IF WS-LF-ST-REFUND-CALC > ZERO
               MOVE WS-LF-ST-REFUND-CALC
                                       TO  CP-EARNING-METHOD.
           IF WS-LF-FO-REFUND-CALC > ZERO
               MOVE WS-LF-FO-REFUND-CALC
                                       TO  CP-EARNING-METHOD.
           IF CP-RATING-METHOD = '4'
              MOVE '4'                 TO CP-EARNING-METHOD
           END-IF
071112*    MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE.
071112     if ws-comp-id = 'AHL'
071112        move cm-lf-class-cd      to cp-class-code
071112        if cp-class-code = spaces
071112           move zeros            to cp-class-code
071112        end-if
071112     else
071112        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
071112     end-if
           MOVE CM-LF-BENEFIT-CD       TO  CP-BENEFIT-CD.
           MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
                                           CP-RATING-BENEFIT-AMT.
           IF CP-STATE-STD-ABBRV = 'OR'
               COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
                                               CM-LF-ALT-BENEFIT-AMT.
      ****   N O T E   ****
      *      CID DOES NOT WANT THE REFUND METHOD TO OVERRIDE
      *      THE OH HARD CODING IN ELCRFNDP
           IF WS-COMP-ID = 'CID'
              IF CP-STATE-STD-ABBRV = 'OH'
                 MOVE WS-LF-CO-EARNINGS-CALC
                                       TO CP-EARNING-METHOD
              END-IF
           END-IF
      ****   N O T E   ****
           MOVE CM-LF-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
           MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
           MOVE CM-LF-DEV-CODE         TO  CP-DEVIATION-CODE.
           MOVE CM-LF-DEV-PCT          TO  CP-RATE-DEV-PCT.
           MOVE WS-CF-CR-R78-METHOD    TO  CP-R78-OPTION.
071117*    IF CP-EARN-AS-NET-PAY
071117*       IF CP-LOAN-APR <= ZEROS
071117*          IF WS-CF-DEFAULT-APR > ZEROS
071117*             MOVE WS-CF-DEFAULT-APR
071117*                                TO  CP-LOAN-APR
071117*          END-IF
071117*       END-IF
071117*    END-IF
      *    DISPLAY ' SOCK REM AMT      ' CP-REMAINING-BENEFIT
      *    DISPLAY ' SOCK REM TRM      ' CP-REMAINING-TERM
      *    DISPLAY ' SOCK EARN METH    ' CP-EARNING-METHOD
      *    DISPLAY ' SOCK RATE METH    ' CP-RATING-METHOD
      *    DISPLAY ' SOCK CLASS        ' CP-CLASS-CODE
      *    DISPLAY ' SOCK BENE CODE    ' CP-BENEFIT-CD
      *    DISPLAY ' SOCK ORIG BENE    ' CP-ORIGINAL-BENEFIT
      *    DISPLAY ' SOCK RATE BENE    ' CP-RATING-BENEFIT-AMT
      *    DISPLAY ' SOCK ORIG PREM    ' CP-ORIGINAL-PREMIUM
      *    DISPLAY ' SOCK ISS AGE      ' CP-ISSUE-AGE
      *    DISPLAY ' SOCK DEV CODE     ' CP-DEVIATION-CODE
      *    DISPLAY ' SOCK DEV PCT      ' CP-RATE-DEV-PCT
      *    DISPLAY ' SOCK APR          ' CP-LOAN-APR
      *    DISPLAY ' SOCK EXT DAYS     ' CP-TERM-OR-EXT-DAYS
      *    DISPLAY ' SOCK LOAN TERM    ' CP-LOAN-TERM
      *    DISPLAY ' SOCK SPEC CALC    ' CP-SPECIAL-CALC-CD
      *    DISPLAY ' SOCK ST STD ABB   ' CP-STATE-STD-ABBRV
081612*    MOVE WS-STATE-EXT-DAYS-CHG  TO CP-EXT-DAYS-CALC
           PERFORM 0720-LINK-REFUND  THRU  0720-EXIT
           DISPLAY ' SOCK REF          ' CP-CALC-REFUND
           DISPLAY ' MADE IT BACK FROM ELRFND '
           IF (CP-ERROR-RATE-IS-ZERO)
              OR (CP-ERROR-RATE-NOT-FOUND)
              OR (CP-ERROR-RATE-FILE-NOTOPEN)
              STRING ' ERROR WITH LIFE REFUND ' CP-RETURN-CODE
                DELIMITED BY SIZE INTO WS-COMMENT
              END-STRING
              MOVE 'X'                 TO WS-LF-STATUS
              GO TO 0400-EXIT
pemtst*       PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
pemtst*       IF NOT ALREADY-SENT
pemtst*          PERFORM 0020-SEND-BUFFER
pemtst*                                THRU 0020-EXIT
pemtst*          SET ALREADY-SENT      TO TRUE
pemtst*       END-IF
      *       PERFORM 0025-CLOSE-SOCKET
      *                                THRU 0025-EXIT
      *       GO TO 0010-RETURN
           END-IF
           MOVE CP-CALC-REFUND         TO WS-LF-REFUND
                                          WS-TOT-LF-RFND
           IF (WS-LF-STATUS = SPACES)
              OR (CP-CALC-REFUND > ZEROS)
              MOVE 'A'                 TO WS-LF-STATUS
           END-IF
           EVALUATE TRUE
              WHEN CP-R-AS-R78
100418           MOVE 'RULE 78'          TO  WS-LF-METHOD
              WHEN  CP-R-AS-PRORATA
100418           MOVE 'PRORATA'          TO  WS-LF-METHOD
              WHEN CP-R-AS-CALIF
                 MOVE 'CALIF'            TO  WS-LF-METHOD
              WHEN CP-R-AS-TEXAS
100418           MOVE 'IRREG/FARM PLAN'  TO  WS-LF-METHOD
              WHEN CP-REFUND-TYPE-USED IS EQUAL TO 'S'
                 MOVE 'UTAH'             TO  WS-LF-METHOD
              WHEN CP-R-AS-FARM-PLAN
100418           MOVE 'IRREG/FARM PLAN'  TO  WS-LF-METHOD
              WHEN CP-R-AS-NET-PAY
                 MOVE 'NET PAY'          TO  WS-LF-METHOD
              WHEN CP-R-AS-ANTICIPATION
                 MOVE 'ANTICIPATION'     TO  WS-LF-METHOD
              WHEN CP-R-AS-MEAN
                 MOVE 'MEAN'             TO  WS-LF-METHOD
              WHEN CP-R-AS-SUM-OF-DIGITS
100418           MOVE 'SUM OF DIGITS'    TO  WS-LF-METHOD
100418        WHEN CP-R-AS-REPOSSESSION
100418           MOVE 'REPOSSESSION'     TO  WS-LF-METHOD
           END-EVALUATE
           IF WS-TOT-LF-RFND > ZEROS
              COMPUTE WS-WORK-FACTOR ROUNDED =
                 WS-TOT-LF-RFND / WS-TOT-LF-PREM
              COMPUTE WS-LF-UEC ROUNDED = WS-WORK-FACTOR *
                 WS-TOT-LF-COMM
           END-IF
           IF ERACCT-FOUND
              IF AM-COMM-CHARGEBACK (1) NOT NUMERIC
                 MOVE ZEROS            TO AM-COMM-CHARGEBACK (1)
              END-IF
              IF AM-COMM-CHARGEBACK (1) NOT = ZEROS
                 IF (AM-COMM-CHARGEBACK (1) = 99)
                           OR
                    (WS-NCB-DIFF-MONTHS > AM-COMM-CHARGEBACK (1))
                           OR
                    ((WS-NCB-DIFF-MONTHS = AM-COMM-CHARGEBACK (1))
                    AND (WS-NCB-DIFF-ODD-DAYS > ZEROS))
                    MOVE ZEROS         TO WS-LF-UEC
                 END-IF
              END-IF
           END-IF
           IF WS-LF-CO-EARNINGS-CALC NOT = 'B'
              GO TO 0400-EXIT
           END-IF
           MOVE 'L'                    TO  CP-BENEFIT-TYPE.
           MOVE '2'                    TO  CP-EARNING-METHOD
                                           CP-RATING-METHOD.
           MOVE CM-LF-ALT-BENEFIT-AMT  TO  CP-ORIGINAL-BENEFIT
                                           CP-REMAINING-BENEFIT
                                           CP-RATING-BENEFIT-AMT.
           IF CP-STATE-STD-ABBRV = 'OR'
               COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
                                               CM-LF-ALT-BENEFIT-AMT.
           MOVE CM-LF-ALT-PREMIUM-AMT  TO  CP-ORIGINAL-PREMIUM.
101813     IF ws-comp-id = 'CID'
101813        IF (CP-STATE-STD-ABBRV = 'MN')
101813           AND (CM-CERT-EFF-DT > X'A4FF')
101813           AND (WS-LF-CO-EARNINGS-CALC = 'B')
101813           MOVE '5'              TO CP-EARNING-METHOD
101813           MOVE WS-LF-CO-EARNINGS-CALC TO CP-RATING-METHOD
101813        END-IF
101813     END-IF
           MOVE 'LEV'                  TO  CP-DEVIATION-CODE.
           PERFORM 0720-LINK-REFUND  THRU  0720-EXIT.
           IF (CP-ERROR-RATE-IS-ZERO)
              OR (CP-ERROR-RATE-NOT-FOUND)
              OR (CP-ERROR-RATE-FILE-NOTOPEN)
              STRING ' ERROR WITH BALLOON REFUND ' CP-RETURN-CODE
                DELIMITED BY SIZE INTO WS-COMMENT
              END-STRING
              MOVE 'X'              TO WS-LF-STATUS
              GO TO 0400-EXIT
pemtst*       PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
pemtst*       IF NOT ALREADY-SENT
pemtst*          PERFORM 0020-SEND-BUFFER
pemtst*                                THRU 0020-EXIT
pemtst*          SET ALREADY-SENT      TO TRUE
pemtst*       END-IF
      *       PERFORM 0025-CLOSE-SOCKET
      *                                THRU 0025-EXIT
      *       GO TO 0010-RETURN
           END-IF
           ADD CP-CALC-REFUND          TO WS-TOT-LF-RFND
           MOVE WS-TOT-LF-RFND         TO WS-LF-REFUND
           IF WS-TOT-LF-RFND > ZEROS
              COMPUTE WS-LF-UEC = (WS-TOT-LF-RFND / WS-TOT-LF-PREM) *
                 WS-TOT-LF-COMM
           END-IF
           IF ERACCT-FOUND
              IF AM-COMM-CHARGEBACK (1) NOT = ZEROS
                 IF (AM-COMM-CHARGEBACK (1) = 99)
                           OR
                    (WS-NCB-DIFF-MONTHS > AM-COMM-CHARGEBACK (1))
                           OR
                    ((WS-NCB-DIFF-MONTHS = AM-COMM-CHARGEBACK (1))
                    AND (WS-NCB-DIFF-ODD-DAYS > ZEROS))
                    MOVE ZEROS         TO WS-LF-UEC
                 END-IF
              END-IF
           END-IF
           .
       0400-EXIT.
           EXIT.
       0500-PROCESS-AH.
           IF CM-AH-BENEFIT-CD = '  ' OR '00'
              GO TO 0500-EXIT
           END-IF
           MOVE ZEROS                  TO WS-TOT-AH-RFND
                                          WS-TOT-AH-PREM
                                          WS-TOT-AH-COMM
100417                                    WS-AH-CANCEL-DATE
           MOVE CM-AH-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
           MOVE ' '                    TO  DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO WS-AH-EXPIRE-DATE
           ELSE
              MOVE ZEROS               TO WS-AH-EXPIRE-DATE
           END-IF
100417     IF CM-AH-CANCEL-DT NOT = LOW-VALUES
100417        MOVE CM-AH-CANCEL-DT     TO DC-BIN-DATE-1
100417        MOVE ' '                 TO DC-OPTION-CODE
100417
100417        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
100417        IF NO-CONVERSION-ERROR
100417           MOVE DC-GREG-DATE-CYMD TO WS-AH-CANCEL-DATE
100417        ELSE
100417           MOVE ZEROS            TO WS-AH-CANCEL-DATE
100417        END-IF
100417     END-IF
           MOVE CM-AH-BENEFIT-CD       TO WS-AH-BEN-CODE
           MOVE CM-AH-ORIG-TERM        TO WS-AH-TERM
           MOVE WS-AH-OVERRIDE-L1      TO CP-AH-OVERRIDE-CODE
           MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
           MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
           MOVE WS-BIN-VAL-DT          TO  CP-VALUATION-DT.
           MOVE CM-STATE               TO  CP-STATE.
           MOVE WS-STATE-ABBREVIATION  TO  CP-STATE-STD-ABBRV.
           MOVE 'A'                    TO  CP-BENEFIT-TYPE.
           MOVE WS-AH-SPECIAL-CALC-CD  TO  CP-SPECIAL-CALC-CD.
           MOVE '2'                    TO  CP-PROCESS-TYPE.
           MOVE WS-COMP-ID             TO  CP-COMPANY-ID.
           MOVE CM-COMPANY-CD          TO  CP-COMPANY-CD.
           MOVE WS-ACCT-USER-FLD-5     TO  CP-ACCT-FLD-5.
           MOVE WS-CF-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION
           MOVE WS-CF-CR-REM-TERM-CALC
                                       TO  CP-REM-TERM-METHOD.
           IF WS-AH-CO-REM-TERM-CALC > '0'
               MOVE WS-AH-CO-REM-TERM-CALC
                                       TO  CP-REM-TERM-METHOD.
           IF WS-AH-ST-REM-TERM-CALC > '0'
               MOVE WS-AH-ST-REM-TERM-CALC
                                       TO  CP-REM-TERM-METHOD.
           IF WS-AH-AM-REM-TERM-CALC > '0'
               MOVE WS-AH-AM-REM-TERM-CALC
                                       TO  CP-REM-TERM-METHOD.
           IF (WS-COMP-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'WI')
              MOVE '7'                 TO CP-REM-TERM-METHOD
              MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
           END-IF
           IF (WS-COMP-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'MO')
              AND (CM-CERT-EFF-DT >= X'9B41')
              AND (CM-CERT-EFF-DT <= X'A2FB')
              MOVE '7'                 TO CP-REM-TERM-METHOD
              MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
           END-IF
           MOVE CM-AH-ORIG-TERM        TO  CP-ORIGINAL-TERM
                                           CP-LOAN-TERM.
           IF  NOT  CP-TERM-IS-DAYS
               IF CM-PMT-EXTENSION-DAYS  IS NUMERIC
                   MOVE CM-PMT-EXTENSION-DAYS
                                       TO  CP-TERM-OR-EXT-DAYS
               ELSE
                   MOVE ZEROS          TO  CP-TERM-OR-EXT-DAYS.
           MOVE WS-FREE-LOOK           TO CP-FREE-LOOK
           IF CM-AH-COMM-PCT > ZEROS
              COMPUTE WS-TOT-AH-COMM = CM-AH-COMM-PCT *
                 CM-AH-PREMIUM-AMT
           END-IF
100417     IF (ws-comp-id = 'DCC' or 'VPP')
              AND (AM-DCC-PRODUCT-CODE = 'DDF')
              compute ws-tot-ah-comm = cm-ah-premium-amt -
                 (cm-ah-clp + cm-addl-clp)
           end-if
           MOVE WS-TOT-AH-COMM         TO WS-AH-COMM
           IF WS-AH-STATUS = 'D'
              GO TO 0500-EXIT
           END-IF
           DISPLAY ' BEGIN OPEN AH CLAIM TEST '
           MOVE LOW-VALUES             TO WS-AH-BIN-PAID-THRU-DT
           MOVE ' '                    TO WS-CLM-STOP-SW
           MOVE CM-COMPANY-CD          TO ELMSTR-COMP-CD
           MOVE CM-CERT-NO             TO ELMSTR-CERT-NO
           
      * EXEC CICS STARTBR
      *       DATASET     ('ELMSTR5')
      *       RIDFLD      (ELMSTR-KEY)
      *       RESP        (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006057' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303036303537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           DISPLAY ' AFTER START ' WS-RESPONSE
           IF RESP-NORMAL
              PERFORM WITH TEST AFTER UNTIL I-SAY-TO-STOP
                 DISPLAY ' AH READNEXT '
                 
      * EXEC CICS READNEXT
      *             DATASET   ('ELMSTR5')
      *             RIDFLD    (ELMSTR-KEY)
      *             INTO      (CLAIM-MASTER)
      *             RESP      (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00006066' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303036303636' TO DFHEIV0(25:11)
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
              IF (RESP-NORMAL OR RESP-DUPKEY)
                 AND (CM-COMPANY-CD = CL-COMPANY-CD)
                 AND (CM-CERT-NO = CL-CERT-NO)
                 IF (CM-ACCOUNT = CL-CERT-ACCOUNT)
                    AND (CM-CERT-EFF-DT = CL-CERT-EFF-DT)
052614              IF (CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                                      OR 'B' OR 'H')
                       IF CLAIM-IS-OPEN
                          DISPLAY ' FOUND OPEN AH CLAIM '
                          MOVE ' OPEN HEALTH CLAIM '
                                       TO WS-COMMENT
                          MOVE 'H'     TO WS-AH-STATUS
                       ELSE
                          DISPLAY ' FOUND CLOSED AH CLAIM '
                          MOVE ' CLOSED HEALTH CLAIM '
                                       TO WS-COMMENT
                          IF CL-PAID-THRU-DT > WS-AH-BIN-PAID-THRU-DT
                             MOVE CL-PAID-THRU-DT
                                       TO WS-AH-BIN-PAID-THRU-DT
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 SET I-SAY-TO-STOP TO TRUE
              END-IF
              END-PERFORM
              
      * EXEC CICS ENDBR
      *          DATASET   ('ELMSTR5')
      *       END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006099' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           IF WS-AH-BIN-PAID-THRU-DT NOT = LOW-VALUES
      *       THIS WILL BE TRUE IF WE FOUND A CLOSED AH CLAIM
              MOVE WS-AH-BIN-PAID-THRU-DT
                                       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD
                                       TO WS-CLM-PAID-THRU-DT
              ELSE
                 MOVE 99999999         TO WS-CLM-PAID-THRU-DT
              END-IF
              IF WS-BIN-VAL-DT <= WS-AH-BIN-PAID-THRU-DT
                 MOVE 'CAN DT NOT > CLM CLOSE DT '
                                       TO WS-COMMENT
                 MOVE 'P'              TO WS-AH-STATUS
                 GO TO 0500-EXIT
              END-IF
           END-IF
           PERFORM 0700-LINK-REM-TERM  THRU  0700-EXIT
           IF CM-AH-CURRENT-STATUS = '8'
              IF CM-AH-CANCEL-DT NOT = LOW-VALUES
                 MOVE ' AH COVERAGE PREVIOUSLY CANCELLED '
                                       TO WS-COMMENT
                 MOVE 'C'              TO WS-AH-STATUS
100417           move cm-ah-itd-cancel-amt
100417                                 to ws-ah-refund
                 GO TO 0500-EXIT
              END-IF
           END-IF
           IF CM-AH-CURRENT-STATUS = '6'  OR  '7'
              IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
                 MOVE ' SETTLEMENT CLAIM APPLIED '
                                       TO WS-COMMENT
                 MOVE 'S'              TO WS-AH-STATUS
pemtst*          PERFORM 0030-BUILD-BUFFER
pemtst*                                THRU 0030-EXIT
pemtst*          IF NOT ALREADY-SENT
pemtst*             PERFORM 0020-SEND-BUFFER
pemtst*                                THRU 0020-EXIT
pemtst*             SET ALREADY-SENT   TO TRUE
pemtst*          END-IF
                 GO TO 0500-EXIT
      *          PERFORM 0025-CLOSE-SOCKET
      *                                THRU 0025-EXIT
      *          GO TO 0010-RETURN
              END-IF
           END-IF
           IF ((CM-AH-LOAN-EXPIRE-DT < WS-BIN-VAL-DT)
              AND (CM-AH-LOAN-EXPIRE-DT > LOW-VALUES))
              MOVE 'AH COVERAGE EXPIRED '
                                       TO WS-COMMENT
              MOVE 'E'                 TO WS-AH-STATUS
              GO TO 0500-EXIT
           END-IF
           MOVE CM-AH-PREMIUM-AMT      TO WS-TOT-AH-PREM
                                          WS-AH-PREM
           MOVE CM-AH-BENEFIT-AMT      TO WS-AH-ORIG-BENEFIT
           COMPUTE WS-AH-REM-BEN = CM-AH-BENEFIT-AMT
              * CP-REMAINING-TERM-1
           MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM
                                           WS-AH-REM-TERM
           MOVE WS-AH-CO-REFUND-CALC   TO  CP-EARNING-METHOD.
           MOVE WS-AH-CO-EARNINGS-CALC TO  CP-RATING-METHOD.
           IF WS-AH-ST-REFUND-CALC > ZERO
               MOVE WS-AH-ST-REFUND-CALC
                                       TO  CP-EARNING-METHOD.
           IF WS-AH-FO-REFUND-CALC > ZERO
               MOVE WS-AH-FO-REFUND-CALC
                                       TO  CP-EARNING-METHOD.
071112*    MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE.
071112     if ws-comp-id = 'AHL'
071112        move cm-ah-class-cd      to cp-class-code
071112        if cp-class-code = spaces
071112           move zeros            to cp-class-code
071112        end-if
071112     else
071112        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
071112     end-if
           MOVE CM-AH-BENEFIT-CD       TO  CP-BENEFIT-CD.
           MOVE CM-AH-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
                                           CP-RATING-BENEFIT-AMT.
           IF CP-STATE-STD-ABBRV = 'OR'
               COMPUTE CP-RATING-BENEFIT-AMT = CM-AH-BENEFIT-AMT *
                                               CM-AH-ORIG-TERM.
           MOVE CM-AH-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
           MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY.
           MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
           MOVE CM-AH-DEV-CODE         TO  CP-DEVIATION-CODE.
           MOVE CM-AH-DEV-PCT          TO  CP-RATE-DEV-PCT.
           MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
           MOVE WS-CF-CR-R78-METHOD    TO  CP-R78-OPTION.
           MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
           MOVE ' '                    TO  DC-OPTION-CODE.
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF CP-STATE-STD-ABBRV = 'OH'
              IF WS-COMP-ID NOT = 'NCL' AND 'CID'
               IF (CP-ORIGINAL-TERM > 60)
                 AND (DC-GREG-DATE-1-YMD > '831101')
                 AND (CM-LF-BENEFIT-CD  IS NOT EQUAL TO  ZERO)
                   MOVE '6'            TO  CP-EARNING-METHOD
               END-IF
              END-IF
              IF WS-COMP-ID = 'CID'
                 IF CM-LF-BENEFIT-CD = (SPACES OR ZEROS OR
                                 LOW-VALUES)
                    IF CP-CRITICAL-PERIOD
                       MOVE '2'       TO CP-EARNING-METHOD
                    ELSE
                       MOVE '6'       TO CP-EARNING-METHOD
                    END-IF
                 ELSE
                    IF WS-CF-LF-COVERAGE-TYPE = 'L'
                       MOVE '2'       TO CP-EARNING-METHOD
                    ELSE
                       IF ((CM-LF-ORIG-TERM > 60) AND
                          (CM-RATE-CLASS NOT = 'L '))
                                     OR
                          (WS-LF-CO-EARNINGS-CALC = '5')
                          IF CP-CRITICAL-PERIOD
                             MOVE '2'  TO CP-EARNING-METHOD
                          ELSE
                             MOVE '6'  TO CP-EARNING-METHOD
                          END-IF
                       ELSE
                          MOVE '1'     TO CP-EARNING-METHOD
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
           IF CP-STATE-STD-ABBRV = 'VA'
             IF WS-COMP-ID NOT = 'NCL' AND 'CID'
               IF DC-GREG-DATE-1-YMD > '921231'
                  IF CP-ORIGINAL-TERM > 61
                      MOVE '6'            TO  CP-EARNING-METHOD
                  ELSE
                      MOVE '1'            TO  CP-EARNING-METHOD.
071117*    IF CP-EARN-AS-NET-PAY
071117*       IF CP-LOAN-APR <= ZEROS
071117*          IF WS-CF-DEFAULT-APR > ZEROS
071117*             MOVE WS-CF-DEFAULT-APR
071117*                                TO  CP-LOAN-APR
071117*          END-IF
071117*       END-IF
071117*    END-IF
100417     IF WS-COMP-ID = 'DCC' or 'VPP'
              IF (WS-AH-BEN-CATEGORY = 'G' OR 'L')
                 AND (CP-EARNING-METHOD NOT = 'G' AND 'D')
                 MOVE 'S'              TO CP-EARNING-METHOD
              END-IF
           END-IF
           MOVE WS-CANCEL-REASON       TO CP-CANCEL-REASON
100417     IF (WS-COMP-ID = 'DCC' or 'VPP')
              AND (CP-EARNING-METHOD = 'D')
              MOVE +0                  TO WS-DDF-ADMIN-FEES
                                          WS-DDF-CSO-ADMIN-FEE
                                          WS-DDF-1ST-YR-TOT-EXP
                                          WS-DDF-COMM-AND-MFEE
              PERFORM VARYING S1 FROM +2 BY +1 UNTIL
                 S1 > +10
                 IF AM-COM-TYP (S1) = 'L' OR 'N' OR 'J' OR 'I'
                    IF (AM-A-COM (S1) NUMERIC)
                       AND (AM-A-COMA (S1) (3:1) NOT = 'L' AND 'M')
                       COMPUTE WS-COMM-PCT = (AM-A-COM (S1) * +1000)
                    ELSE
                       MOVE +0         TO WS-COMM-PCT C0
                       PERFORM 0740-GET-ERCTBL THRU 0740-EXIT
                       COMPUTE WS-COMM-PCT = WS-COMM-PCT * +1000
                    END-IF
                    IF AM-COM-TYP (S1) = 'L' OR 'N'
                       COMPUTE WS-DDF-ADMIN-FEES = WS-DDF-ADMIN-FEES
                          + WS-COMM-PCT
                    END-IF
                    IF AM-COM-TYP (S1) = 'N'
                       COMPUTE WS-DDF-CSO-ADMIN-FEE =
                          WS-DDF-CSO-ADMIN-FEE + WS-COMM-PCT
                    END-IF
                    IF AM-COM-TYP (S1) = 'J' OR 'L'
                       COMPUTE WS-DDF-1ST-YR-TOT-EXP
                          = WS-DDF-1ST-YR-TOT-EXP + WS-COMM-PCT
                    END-IF
                    IF AM-COM-TYP (S1) = 'I'
                       COMPUTE WS-DDF-COMM-AND-MFEE
                          = WS-DDF-COMM-AND-MFEE + WS-COMM-PCT
                    END-IF
                 END-IF
              END-PERFORM
              MOVE WS-DDF-CSO-ADMIN-FEE TO CP-DDF-CSO-ADMIN-FEE
              MOVE WS-DDF-ADMIN-FEES   TO CP-DDF-ADMIN-FEES
              COMPUTE WS-DDF-COMM-AND-MFEE = WS-DDF-COMM-AND-MFEE +
                 (CM-AH-PREMIUM-AMT - CM-AH-CLP - CM-ADDL-CLP)
           END-IF
100417     IF (WS-COMP-ID = 'DCC' or 'VPP')
              AND (CP-EARNING-METHOD = 'D')
              AND (ws-CANCEL-REASON NOT = 'R')
              PERFORM 0730-GET-DDF-FACTORS
                                       THRU 0730-EXIT
              IF NOT PDEF-FOUND
                 MOVE ' DDF UE FACTORS  NOT FOUND '
                                       TO WS-COMMENT
                 MOVE 'X'              TO WS-LF-STATUS WS-AH-STATUS
                 PERFORM 0030-BUILD-BUFFER
                                       THRU 0030-EXIT
                 PERFORM 0020-SEND-BUFFER
                                       THRU 0020-EXIT
                 PERFORM 0025-CLOSE-SOCKET
                                       THRU 0025-EXIT
                 GO TO 0010-RETURN
              END-IF
      *       IF NOT PDEF-FOUND
      *          MOVE ER-9999          TO EMI-ERROR
      *          PERFORM 9700-ERROR-FORMAT
      *                                THRU 9799-EXIT
      *          GO TO 8200-SEND-DATAONLY
      *       END-IF
              MOVE PD-UEP-FACTOR (P1 P2 + 1)
                                       TO CP-DDF-LO-FACT
              MOVE PD-UEP-FACTOR (P1 P2)
                                       TO CP-DDF-HI-FACT
              MOVE WS-DDF-COMM-AND-MFEE TO CP-DDF-COMM-AND-MFEE
              MOVE CM-AH-CLP           TO CP-DDF-CLP
              MOVE PD-1ST-YR-ADMIN-ALLOW TO CP-DDF-YR1AF
              COMPUTE CP-1ST-YR-ALLOW = WS-DDF-1ST-YR-TOT-EXP
                 + PD-1ST-YR-ADMIN-ALLOW
              MOVE 'G'                 TO CP-DDF-SPEC-CALC
      *       IF PI-CLP-YN = 'Y'
      *          MOVE 'C'              TO CP-DDF-SPEC-CALC
      *          MOVE CM-AH-CLP        TO CP-ORIGINAL-PREMIUM
      *                                   CP-DDF-CLP
      *          MOVE ZEROS            TO CP-1ST-YR-ALLOW
      *       END-IF
              IF DD-IU-PRESENT
                 MOVE 'I'              TO CP-EARNING-METHOD
              END-IF
              MOVE CM-DDF-IU-RATE-UP   TO CP-IU-RATE-UP
                                          CP-CLP-RATE-UP
              IF (CP-CALC-GROSS-FEE)
                 AND (CP-IU-RATE-UP NOT = ZEROS)
                 COMPUTE TEX-FACT-8 = 1 - ((CM-ADDL-CLP + CM-AH-CLP)
                    / CP-ORIGINAL-PREMIUM)
                 COMPUTE CP-IU-RATE-UP ROUNDED = CP-IU-RATE-UP
                    / (1 - TEX-FACT-8)
              END-IF
           END-IF
           PERFORM 0720-LINK-REFUND  THRU  0720-EXIT
           IF (CP-ERROR-RATE-IS-ZERO)
              OR (CP-ERROR-RATE-NOT-FOUND)
              OR (CP-ERROR-RATE-FILE-NOTOPEN)
              STRING ' ERROR WITH AH REFUND ' CP-RETURN-CODE
                DELIMITED BY SIZE INTO WS-COMMENT
              END-STRING
              IF WS-AH-STATUS NOT = 'H'
                 MOVE 'X'                 TO WS-AH-STATUS
              END-IF
              GO TO 0500-EXIT
pemtst*       PERFORM 0030-BUILD-BUFFER THRU 0030-EXIT
pemtst*       IF NOT ALREADY-SENT
pemtst*          PERFORM 0020-SEND-BUFFER
pemtst*                                THRU 0020-EXIT
pemtst*          SET ALREADY-SENT      TO TRUE
pemtst*       END-IF
      *       PERFORM 0025-CLOSE-SOCKET
      *                                THRU 0025-EXIT
      *       GO TO 0010-RETURN
           END-IF
           MOVE CP-CALC-REFUND         TO WS-AH-REFUND
                                          WS-TOT-AH-RFND
           MOVE CP-REFUND-TYPE-USED    TO WS-AH-METHOD
           IF ((WS-AH-STATUS = SPACES)
              OR (CP-CALC-REFUND > ZEROS))
                        AND
                (WS-AH-STATUS NOT = 'H')
              MOVE 'A'                 TO WS-AH-STATUS
           END-IF
100417     IF WS-COMP-ID = 'DCC' or 'VPP'
071211        AND (WS-AH-BEN-CATEGORY = 'G' OR 'L')
071211        AND (CP-EARNING-METHOD = 'D' OR 'I')
071211        MOVE CM-DDF-IU-RATE-UP   TO CP-IU-RATE-UP
071211        MOVE 'C'                 TO CP-DDF-SPEC-CALC
071211        MOVE CM-AH-CLP           TO CP-ORIGINAL-PREMIUM
071211        MOVE ZEROS               TO CP-1ST-YR-ALLOW
071211        PERFORM 0720-LINK-REFUND THRU  0720-EXIT
071211        MOVE CP-CALC-REFUND      TO ws-ah-rfnd-clp
071211        DISPLAY ' CALC CLP   ' CP-CALC-REFUND
071211     END-IF
           EVALUATE TRUE
              WHEN CP-R-AS-R78
                 MOVE 'RULE 78'          TO  WS-AH-METHOD
              WHEN CP-R-AS-PRORATA
100418           MOVE 'PRORATA'          TO  WS-AH-METHOD
              WHEN CP-REFUND-TYPE-USED = '3'
                 MOVE 'CALIF'            TO  WS-AH-METHOD
              WHEN CP-R-AS-TEXAS
100418           MOVE 'IRREG/FARM PLAN'  TO  WS-AH-METHOD
              WHEN CP-R-AS-FARM-PLAN
100418           MOVE 'IRREG/FARM PLAN'  TO  WS-AH-METHOD
              WHEN CP-R-AS-NET-PAY
                 MOVE 'NET PAY'          TO  WS-AH-METHOD
              WHEN CP-R-AS-ANTICIPATION
                 MOVE 'ANTICIPATION'     TO  WS-AH-METHOD
              WHEN CP-R-AS-MEAN
                 MOVE 'MEAN'             TO  WS-AH-METHOD
              WHEN CP-R-AS-SUM-OF-DIGITS
100418           MOVE 'SUM OF DIGITS'    TO  WS-AH-METHOD
              WHEN CP-GAP-ACTUARIAL
                 MOVE 'SP ACTUARIAL'     TO WS-AH-METHOD
100418        WHEN CP-R-AS-REPOSSESSION
100418           MOVE 'REPOSSESSION'     TO WS-AH-METHOD
           END-EVALUATE
           IF WS-TOT-AH-RFND > ZEROS
100417        IF (ws-comp-id = 'DCC' or 'VPP')
                 AND (AM-DCC-PRODUCT-CODE = 'DDF')
                 compute ws-work-factor rounded =
                    ws-ah-rfnd-clp / cm-ah-clp
              else
                 COMPUTE WS-WORK-FACTOR ROUNDED =
                    WS-TOT-AH-RFND / WS-TOT-AH-PREM
              end-if
              COMPUTE WS-AH-UEC ROUNDED = WS-WORK-FACTOR *
                 WS-TOT-AH-COMM
           END-IF
           IF ERACCT-FOUND
              IF AM-COMM-CHARGEBACK (1) NOT NUMERIC
                 MOVE ZEROS            TO AM-COMM-CHARGEBACK (1)
              END-IF
              IF AM-COMM-CHARGEBACK (1) NOT = ZEROS
                 IF (AM-COMM-CHARGEBACK (1) = 99)
                           OR
                    (WS-NCB-DIFF-MONTHS > AM-COMM-CHARGEBACK (1))
                           OR
                    ((WS-NCB-DIFF-MONTHS = AM-COMM-CHARGEBACK (1))
                    AND (WS-NCB-DIFF-ODD-DAYS > ZEROS))
                    MOVE ZEROS         TO WS-AH-UEC
                 END-IF
              END-IF
           END-IF
           .
       0500-EXIT.
           EXIT.
100417 0610-GET-ERMAIL.
100417
052319     move cm-control-primary     to ws-ma-key
052319*    MOVE WS-CM-KEY              TO WS-MA-KEY
100417     
      * EXEC CICS READ
100417*       INTO    (MAILING-DATA)
100417*       DATASET ('ERMAIL')
100417*       RIDFLD  (WS-MA-KEY)
100417*       RESP    (WS-RESPONSE)
100417*    END-EXEC
           MOVE LENGTH OF
            MAILING-DATA
             TO DFHEIV11
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00006447' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036343437' TO DFHEIV0(25:11)
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
100417
100417     IF RESP-NORMAL
100417        MOVE MA-CRED-BENE-NAME   to ws-cred-bene-name
100417     else
100417        move spaces              to ws-cred-bene-name
100417     end-if
100417
100417     .
100417 0610-EXIT.
100417     EXIT.
100417 0620-GET-ELCRTT.
100417
052319     move cm-control-primary     to ws-cs-key
052319*    MOVE WS-CM-KEY              TO WS-CS-KEY
100417     move 'C'                    to WS-CS-TRLR-TYPE
100417
100417     
      * EXEC CICS READ
100417*       INTO    (CERTIFICATE-TRAILERS)
100417*       DATASET ('ELCRTT')
100417*       RIDFLD  (WS-CS-KEY)
100417*       RESP    (WS-RESPONSE)
100417*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00006469' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036343639' TO DFHEIV0(25:11)
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
100417
100417     IF RESP-NORMAL
100417        MOVE cs-vin-number       to ws-vin
100417     else
100417        move spaces              to ws-vin
100417     end-if
100417
100417     .
100417 0620-EXIT.
100417     EXIT.
       0700-LINK-REM-TERM.
           
      * EXEC CICS LINK
      *        PROGRAM   ('ELRTRM')
      *        COMMAREA  (CALCULATION-PASS-AREA)
      *        LENGTH    (CP-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELRTRM' TO DFHEIV1
      *    MOVE '."C                   (   #00006486' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       0700-EXIT.
           EXIT.
       0710-LINK-REM-AMOUNT.
           
      * EXEC CICS LINK
      *        PROGRAM   ('ELRAMT')
      *        COMMAREA  (CALCULATION-PASS-AREA)
      *        LENGTH    (CP-COMM-LENGTH)
      *    END-EXEC
           MOVE 'ELRAMT' TO DFHEIV1
      *    MOVE '."C                   (   #00006494' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0710-EXIT.
           EXIT.
       0720-LINK-REFUND.
           
      * EXEC CICS LINK
      *        PROGRAM   ('ELRFND')
      *        COMMAREA  (CALCULATION-PASS-AREA)
      *        LENGTH    (CP-COMM-LENGTH)
      *    END-EXEC
           MOVE 'ELRFND' TO DFHEIV1
      *    MOVE '."C                   (   #00006503' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0720-EXIT.
           EXIT.
       0730-GET-DDF-FACTORS.
           MOVE ' '                    TO WS-PDEF-RECORD-SW
           MOVE WS-COMP-CD             TO ERPDEF-KEY
           MOVE CM-STATE               TO ERPDEF-STATE
010816     if cm-clp-state not = cm-state and spaces and zeros
010816        move cm-clp-state        to erpdef-state
010816     end-if
           MOVE AM-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
           MOVE 'A'                    TO ERPDEF-BEN-TYPE
           MOVE CM-AH-BENEFIT-CD       TO ERPDEF-BEN-CODE
           MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
           MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
           
      * EXEC CICS STARTBR
      *        DATASET  ('ERPDEF')
      *        RIDFLD   (ERPDEF-KEY)
      *        GTEQ
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006523' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303036353233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              GO TO 0730-EXIT
           END-IF
           .
       0730-READNEXT.
           
      * EXEC CICS READNEXT
      *       DATASET  ('ERPDEF')
      *       INTO     (PRODUCT-MASTER)
      *       RIDFLD   (ERPDEF-KEY)
      *       RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV12
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00006534' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303036353334' TO DFHEIV0(25:11)
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
           IF NOT RESP-NORMAL
              GO TO 0730-ENDBR
           END-IF
           IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
              IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
                 MOVE 'Y'              TO WS-PDEF-RECORD-SW
              ELSE
                 GO TO 0730-READNEXT
              END-IF
           ELSE
              GO TO 0730-ENDBR
           END-IF
           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
080322        (P1 > +11)
              OR (PD-PROD-CODE (P1) = 'I')
           END-PERFORM
080322     IF P1 < +12
              SET DD-IU-PRESENT        TO TRUE
           END-IF
           IF CM-LOAN-TERM = ZEROS
              MOVE CP-ORIGINAL-TERM    TO CP-LOAN-TERM
           END-IF
           IF PD-TRUNCATED = 'Y'
              MOVE CM-LOAN-TERM        TO WS-TERM
           ELSE
              MOVE CP-ORIGINAL-TERM    TO WS-TERM
           END-IF
           EVALUATE TRUE
              WHEN WS-TERM > +168
                 MOVE 15               TO P1
              WHEN WS-TERM > +156
                 MOVE 14               TO P1
              WHEN WS-TERM > +144
                 MOVE 13               TO P1
              WHEN WS-TERM > +132
                 MOVE 12               TO P1
              WHEN WS-TERM > +120
                 MOVE 11               TO P1
              WHEN WS-TERM > +108
                 MOVE 10               TO P1
              WHEN WS-TERM > +96
                 MOVE 9                TO P1
              WHEN WS-TERM > +84
                 MOVE 8                TO P1
              WHEN WS-TERM > +72
                 MOVE 7                TO P1
              WHEN WS-TERM > +60
                 MOVE 6                TO P1
              WHEN WS-TERM > +48
                 MOVE 5                TO P1
              WHEN WS-TERM > +36
                 MOVE 4                TO P1
              WHEN WS-TERM > +24
                 MOVE 3                TO P1
              WHEN WS-TERM > +12
                 MOVE 2                TO P1
              WHEN OTHER
                 MOVE 1                TO P1
           END-EVALUATE
           EVALUATE TRUE
      *       WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +13)
      *          AND (DD-IU-PRESENT)
      *          MOVE 2                TO P2
      *       WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +13
      *          MOVE 1                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +25
                 MOVE 2                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +37
                 MOVE 3                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +49
                 MOVE 4                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +61
                 MOVE 5                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +73
                 MOVE 6                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +85
                 MOVE 7                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +97
                 MOVE 8                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +109
                 MOVE 9                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +121
                 MOVE 10               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +133
                 MOVE 11               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +145
                 MOVE 12               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +157
                 MOVE 13               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +169
                 MOVE 14               TO P2
              WHEN OTHER
                 MOVE 15               TO P2
           END-EVALUATE
           .
       0730-ENDBR.
           
      * EXEC CICS ENDBR
      *       DATASET  ('ERPDEF')
      *    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006636' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0730-EXIT.
           EXIT.
       0740-GET-ERCTBL.
           MOVE AM-COMPANY-CD          TO CTBL-COMPANY-CD
           MOVE AM-A-COMA (S1)         TO CTBL-TABLE
           MOVE 'A'                    TO CTBL-BEN-TYPE
           MOVE CM-AH-BENEFIT-CD       TO CTBL-BEN-CODE
           MOVE CTBL-KEY               TO CTBL-KEY-SAVE
           PERFORM 0750-READ-ERCTBL    THRU 0750-EXIT
           IF RESP-NORMAL
              PERFORM 0760-FIND-COMM   THRU 0760-EXIT
           ELSE
              MOVE AM-COMPANY-CD          TO CTBL-COMPANY-CD
              MOVE AM-A-COMA (S1)         TO CTBL-TABLE
              MOVE 'A'                    TO CTBL-BEN-TYPE
              MOVE 'AA'                   TO CTBL-BEN-CODE
              MOVE CTBL-KEY               TO CTBL-KEY-SAVE
              PERFORM 0750-READ-ERCTBL    THRU 0750-EXIT
              IF RESP-NORMAL
                 PERFORM 0760-FIND-COMM   THRU 0760-EXIT
              END-IF
           END-IF
           .
       0740-EXIT.
           EXIT.
       0750-READ-ERCTBL.
           
      * EXEC CICS READ
      *         INTO    (COMM-TABLE-RECORD)
      *         DATASET ('ERCTBL')
      *         RIDFLD  (CTBL-KEY)
      *         RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            COMM-TABLE-RECORD
             TO DFHEIV11
           MOVE 'ERCTBL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00006666' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036363636' TO DFHEIV0(25:11)
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
           .
       0750-EXIT.
           EXIT.
       0760-FIND-COMM.
           PERFORM VARYING C1 FROM +1 BY +1 UNTIL
              ((CM-AH-BENEFIT-AMT * CM-AH-ORIG-TERM) <= CT-TBF (C1))
              OR (C1 > +3)
           END-PERFORM
           PERFORM VARYING C2 FROM +1 BY +1 UNTIL
              (CM-INSURED-ISSUE-AGE <= CT-AGE (C2))
              OR (C2 > +3)
           END-PERFORM
           PERFORM VARYING C3 FROM +1 BY +1 UNTIL
              (CM-AH-ORIG-TERM <= CT-TRM (C3))
              OR (C3 > +3)
           END-PERFORM
           IF C1 > +3
              MOVE +1                  TO C1
           END-IF
           IF C2 > +3
              MOVE +1                  TO C2
           END-IF
           IF C3 > +3
              MOVE +1                  TO C3
           END-IF
           IF C1 = +3
              MOVE +18                 TO C0
           ELSE
              IF C1 = +2
                 MOVE +9               TO C0
              END-IF
           END-IF
           IF C2 = +3
              ADD +6                   TO C0
           ELSE
              IF C2 = +2
                 ADD +3                TO C0
              END-IF
           END-IF
           ADD C3                      TO C0
           MOVE CT-RT (C0)             TO WS-COMM-PCT
           .
       0760-EXIT.
           EXIT.
       9700-DATE-LINK.
           
      * EXEC CICS LINK
      *         PROGRAM  ('ELDATCV')
      *         COMMAREA (DATE-CONVERSION-DATA)
      *         LENGTH   (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00006717' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373137' TO DFHEIV0(25:11)
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
