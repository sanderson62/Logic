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
052319* 052319  IR2019052300001  PEMA  VIN&BENE not being passed at times
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

                                        COPY ELCCERT.
                                        COPY ELCMSTR.
                                        COPY ERCPDEF.
                                        COPY ERCCTBL.
                                        COPY ELCCNTL.
                                        COPY ERCACCT.
100417                                  COPY ERCMAIL.
100417                                  COPY ELCCRTT.
                                        COPY ELCDATE.
                                        COPY ELCCALC.
      
       linkage section.
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

       procedure division.

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
      *          DISPLAY ' SETTING FULL '
              WHEN (CLIENT-CERT-NO NOT = SPACES)
                 SET WS-ELCERT-CERT-NO TO TRUE
      *          DISPLAY ' SETTING CERT NO '
      *       WHEN (CLIENT-EFF-DT NOT = SPACES)
      *          AND (CLIENT-LAST-NAME NOT = SPACES)
              WHEN (CLIENT-ACCOUNT NOT = SPACES)
                 AND (CLIENT-LAST-NAME NOT = SPACES)
                 SET WS-ELCERT-ACT-NAME TO TRUE
              WHEN CLIENT-LAST-NAME NOT = SPACES
                 SET WS-ELCERT-NAME    TO TRUE
      *          DISPLAY ' SETTING NAME '
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
           exec cics return end-exec.
           goback.

      *    PERFORM 0030-BUILD-BUFFER   THRU 0030-EXIT

           .
       0020-SEND-BUFFER.
       
      *    display 'SOCK03:About to send      '
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
           exec cics return end-exec.
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
           EXEC CICS STARTBR
              DATASET  ('ERACCT3')
              RIDFLD   (WS-AM3-KEY)
              GTEQ
              RESP (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              PERFORM UNTIL TOLD-TO-STOP
                 EXEC CICS READNEXT
                    DATASET  ('ERACCT3')
                    INTO     (ACCOUNT-MASTER)
                    RIDFLD   (WS-AM3-KEY)
                 END-EXEC
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
              EXEC CICS ENDBR
                 DATASET   ('ERACCT3')
              END-EXEC
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
                 EXEC CICS READ
                    INTO    (CERTIFICATE-MASTER)
                    DATASET ('ELCERT')
                    RIDFLD  (WS-CM-KEY)
                    RESP    (WS-RESPONSE)
                 END-EXEC
              WHEN WS-ELCERT-ACT-NAME
                 MOVE CLIENT-CAR          TO WS-CM-CARRIER
                 MOVE CLIENT-GRP          TO WS-CM-GROUP
                 MOVE CLIENT-STATE        TO WS-CM-STATE
                 MOVE CLIENT-ACCOUNT      TO WS-CM-ACCOUNT
                 MOVE LOW-VALUES          TO WS-CM-EFF-DT
                 MOVE ZEROS               TO WS-CM-CERT-NO
                 EXEC CICS STARTBR
                    DATASET ('ELCERT')
                    RIDFLD  (WS-CM-KEY)
                    RESP    (WS-RESPONSE)
                 END-EXEC
              WHEN WS-ELCERT-NAME
                 MOVE CLIENT-LAST-NAME TO WS-CM-LAST-NAME
                 MOVE SPACES           TO WS-CM-INITIALS
                 COMPUTE WS-LENGTH = L1 + +1
                 EXEC CICS STARTBR
                    DATASET   ('ELCERT2')
                    RIDFLD    (WS-CM-KEY-A1)
                    GENERIC
                    KEYLENGTH (WS-LENGTH)
                    GTEQ
                    RESP      (WS-RESPONSE)
                 END-EXEC
      *          DISPLAY ' STARTBR RESP ' WS-RESPONSE
      *          DISPLAY ' KEY ' WS-CM-KEY-A1
              WHEN WS-ELCERT-CERT-NO
      * DO NOT INCLUDE THE SUFFIX AS PART OF THE KEY
                 MOVE +11 TO WS-LENGTH
                 MOVE CLIENT-CERT-NO    TO WS-CM-CERT-NO-A4
                 EXEC CICS STARTBR
                    DATASET   ('ELCERT5')
                    RIDFLD    (WS-CM-KEY-A4)
                    GENERIC
                    KEYLENGTH (WS-LENGTH)
                    GTEQ
                    RESP      (WS-RESPONSE)
                 END-EXEC
080519        when ws-elcert-vin
080519           move low-values       to ws-cs-key
080519           move ws-comp-cd       to ws-cs-company-cd
080519           move client-car       to ws-cs-carrier
080519           move client-grp       to ws-cs-group
080519           move client-state     to ws-cs-state
080519           move client-account   to ws-cs-account
080519           exec cics startbr
080519              dataset   ('ELCRTT')
080519              RIDFLD    (WS-CS-KEY)
080519              GTEQ
080519              RESP      (WS-RESPONSE)
080519           END-EXEC
           END-EVALUATE

           .
       0100-EXIT.
           EXIT.

       0150-READ-NEXT-ELCERT.

           EVALUATE TRUE
              WHEN WS-ELCERT-ACT-NAME
      *          DISPLAY ' ABOUT TO READ NEXT - ACT NAME'
                 EXEC CICS READNEXT
                    DATASET   ('ELCERT')
                    RIDFLD    (WS-CM-KEY)
                    INTO      (CERTIFICATE-MASTER)
                    RESP      (WS-RESPONSE)
                 END-EXEC
              WHEN WS-ELCERT-NAME
      *          DISPLAY ' ABOUT TO READ NEXT - NAME'
                 EXEC CICS READNEXT
                    DATASET   ('ELCERT2')
                    RIDFLD    (WS-CM-KEY-A1)
                    INTO      (CERTIFICATE-MASTER)
                    RESP      (WS-RESPONSE)
                 END-EXEC
              WHEN WS-ELCERT-CERT-NO
                 EXEC CICS READNEXT
                    DATASET   ('ELCERT5')
                    RIDFLD    (WS-CM-KEY-A4)
                    INTO      (CERTIFICATE-MASTER)
                    RESP      (WS-RESPONSE)
                 END-EXEC
080519        WHEN WS-ELCERT-VIN
080519           EXEC CICS READNEXT
080519              DATASET   ('ELCRTT')
080519              RIDFLD    (WS-CS-KEY)
080519              INTO      (CERTIFICATE-TRAILERS)
080519              RESP      (WS-RESPONSE)
080519           END-EXEC
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
080519        EXEC CICS READ
080519           INTO    (CERTIFICATE-MASTER)
080519           DATASET ('ELCERT')
080519           RIDFLD  (WS-CM-KEY)
080519           RESP    (WS-RESPONSE)
080519        END-EXEC
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
     
           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-CF-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC

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
           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-CF-KEY)
              GTEQ
              RESP    (WS-RESPONSE)
           END-EXEC

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
           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-CF-KEY)
              GTEQ
              RESP    (WS-RESPONSE)
           END-EXEC

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

           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-CF-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC

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
     
           EXEC CICS STARTBR
              DATASET  ('ERACCT')
              RIDFLD   (WS-AM-KEY)
              GTEQ
              RESP (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              PERFORM UNTIL TOLD-TO-STOP
                 EXEC CICS READNEXT
                    DATASET  ('ERACCT')
                    INTO     (ACCOUNT-MASTER)
                    RIDFLD   (WS-AM-KEY)
                 END-EXEC
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
              EXEC CICS ENDBR
                 DATASET   ('ERACCT')
              END-EXEC
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
           EXEC CICS STARTBR
              DATASET     ('ELMSTR5')
              RIDFLD      (ELMSTR-KEY)
              RESP        (WS-RESPONSE)
           END-EXEC

           DISPLAY ' AFTER START ' WS-RESPONSE

           IF RESP-NORMAL
              PERFORM WITH TEST AFTER UNTIL I-SAY-TO-STOP

                 EXEC CICS READNEXT
                    DATASET   ('ELMSTR5')
                    RIDFLD    (ELMSTR-KEY)
                    INTO      (CLAIM-MASTER)
                    RESP      (WS-RESPONSE)
                 END-EXEC

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
              EXEC CICS ENDBR
                 DATASET   ('ELMSTR5')
              END-EXEC
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
           EXEC CICS STARTBR
              DATASET     ('ELMSTR5')
              RIDFLD      (ELMSTR-KEY)
              RESP        (WS-RESPONSE)
           END-EXEC

           DISPLAY ' AFTER START ' WS-RESPONSE

           IF RESP-NORMAL
              PERFORM WITH TEST AFTER UNTIL I-SAY-TO-STOP

                 DISPLAY ' AH READNEXT '
                 EXEC CICS READNEXT
                    DATASET   ('ELMSTR5')
                    RIDFLD    (ELMSTR-KEY)
                    INTO      (CLAIM-MASTER)
                    RESP      (WS-RESPONSE)
                 END-EXEC

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
              EXEC CICS ENDBR
                 DATASET   ('ELMSTR5')
              END-EXEC
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
100417     EXEC CICS READ
100417        INTO    (MAILING-DATA)
100417        DATASET ('ERMAIL')
100417        RIDFLD  (WS-MA-KEY)
100417        RESP    (WS-RESPONSE)
100417     END-EXEC
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
100417     EXEC CICS READ
100417        INTO    (CERTIFICATE-TRAILERS)
100417        DATASET ('ELCRTT')
100417        RIDFLD  (WS-CS-KEY)
100417        RESP    (WS-RESPONSE)
100417     END-EXEC
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
           EXEC CICS LINK
               PROGRAM   ('ELRTRM')
               COMMAREA  (CALCULATION-PASS-AREA)
               LENGTH    (CP-COMM-LENGTH)
           END-EXEC.
     
       0700-EXIT.
           EXIT.
           
       0710-LINK-REM-AMOUNT.

           EXEC CICS LINK
               PROGRAM   ('ELRAMT')
               COMMAREA  (CALCULATION-PASS-AREA)
               LENGTH    (CP-COMM-LENGTH)
           END-EXEC

           .
       0710-EXIT.
           EXIT.

       0720-LINK-REFUND.

           EXEC CICS LINK
               PROGRAM   ('ELRFND')
               COMMAREA  (CALCULATION-PASS-AREA)
               LENGTH    (CP-COMM-LENGTH)
           END-EXEC

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

           EXEC CICS STARTBR
               DATASET  ('ERPDEF')
               RIDFLD   (ERPDEF-KEY)
               GTEQ
               RESP     (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 0730-EXIT
           END-IF

           .
       0730-READNEXT.

           EXEC CICS READNEXT
              DATASET  ('ERPDEF')
              INTO     (PRODUCT-MASTER)
              RIDFLD   (ERPDEF-KEY)
              RESP     (WS-RESPONSE)
           END-EXEC

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

           EXEC CICS ENDBR
              DATASET  ('ERPDEF')
           END-EXEC

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

           EXEC CICS READ
                INTO    (COMM-TABLE-RECORD)
                DATASET ('ERCTBL')
                RIDFLD  (CTBL-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

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

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.

