      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EL570.
      *AUTHOR.     PABLO.
      *            COLLEYVILLE, TX.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF    CSO      IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************

      *REMARKS.
      *        THIS PROGRAM CREATES A FREEDOM INTERFACE FILE OF
      *    ALL REFUNDS MADE IN NH THAT IS CONSIDERED PROCESSABLE
      *    THEN FLAGS THE ERPNDB RECORD. Added TN to the mix.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 052008    2007110500003  PEMA  NEW PROGRAM
081308* 081308    2008080500001  PEMA  ADD MULTI "D" RECORDS
082608* 082608    2008080500001  PEMA  USE NAME FROM ERMAIL FILE
      *                           AND CHANGE INVOICE NO TO MATCH TAX ID
111109* 111109    2008100900003  AJRA  CHANGE CERT NOTE TO NEW FILE      
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
081710* 081710   2010010400003   AJRA  FIX VENDOR REC CITY STATE ZIP
040411* 040411                   PEMA  CHANGE GL NUMBER
103111* 103111   2011022800001   AJRA  NAPERSOFT FOR ACCT SERV
032212* 032212    2011110200001  PEMA  AHL CHANGES
041712* 041712  IR2012041600002  AJRA  USE AHL IN T REC WHEN AHL
071212* 071212   2011022800001   AJRA  ADD UNDW ID
072312* 072312   2011022800001   AJRA	 ADD NEW FIELDS
090612* 090612   2012090600001   AJRA  CLEAR OUT OLD DATA
111912* 111912  IR2012111900001  AJRA  FIX VENDOR REC ADDRESS 
112612* 112612   2012101700002   AJRA  ADD NEW FIELDS
022414* 022414 IR2014022400001   PEMA  BYPASS FUTURE CANCEL DATES
033114* 033114   2012121800001   AJRA  ADD COV STATUS TO NAPERSOFT EXT
062017* 062017 CR2015091000001   PEMA  ADD TN INTEREST PROCESSING
041718* 041718 IR2018041700001   PEMA  Add access to KIXSYS env variable
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT ERACCT          ASSIGN TO ERACCT
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS AM-CONTROL-PRIMARY
                                  FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT ERARCH          ASSIGN TO ERARCH
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS LA-CONTROL-PRIMARY
                                  FILE STATUS IS ERARCH-FILE-STATUS.

062017     SELECT ERPNDB          ASSIGN TO ERPNDB2
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
062017                            RECORD KEY IS PB-CONTROL-by-account
                                  FILE STATUS IS ERPNDB-FILE-STATUS.

           SELECT ELCERT          ASSIGN TO ELCERT
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS CM-CONTROL-PRIMARY
                                  FILE STATUS IS ELCERT-FILE-STATUS.

111109     SELECT ERCNOT          ASSIGN TO ERCNOT
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
111109                            RECORD KEY IS CZ-CONTROL-PRIMARY
111109                            FILE STATUS IS ERCNOT-FILE-STATUS.

           SELECT ERMAIL          ASSIGN TO ERMAIL
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS MA-CONTROL-PRIMARY
                                  FILE STATUS IS ERMAIL-FILE-STATUS.

           SELECT ELCNTL          ASSIGN TO ELCNTL
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS CF-CONTROL-PRIMARY
                                  FILE STATUS IS ELCNTL-FILE-STATUS.

072312     SELECT ELLETR           ASSIGN TO ELLETR
072312                             ACCESS IS DYNAMIC
072312                             ORGANIZATION IS INDEXED
072312                             FILE STATUS IS ELLETR-FILE-STATUS
072312                             RECORD KEY IS TX-CONTROL-PRIMARY.
072312
072312     SELECT ELENCC           ASSIGN TO ELENCC
072312                             ACCESS IS DYNAMIC
072312                             ORGANIZATION IS INDEXED
072312                             FILE STATUS IS ELENCC-FILE-STATUS
072312                             RECORD KEY IS NC-CONTROL-PRIMARY.

           SELECT DISK-DATE       ASSIGN TO SYS019.
           SELECT PRNTR           ASSIGN TO SYS008.
           SELECT FICH            ASSIGN TO SYS020.
           SELECT FREEDOM-INT-FILE
                                  ASSIGN TO SYS021
              ORGANIZATION IS LINE SEQUENTIAL.
103111              
103111     SELECT NSASEXTR        ASSIGN TO NSASEXTR
103111                            ORGANIZATION IS INDEXED
103111                            ACCESS IS DYNAMIC
103111                            RECORD KEY IS NSAS-CONTROL-PRIMARY
103111                            FILE STATUS IS NSAS-FILE-STATUS.
103111        

062017     SELECT ERENDT           ASSIGN TO ERENDT
062017                             ACCESS IS DYNAMIC
062017                             ORGANIZATION IS INDEXED
062017                             FILE STATUS IS ERENDT-FILE-STATUS
062017                             RECORD KEY IS EN-CONTROL-PRIMARY.

       DATA DIVISION.

       FILE SECTION.

       FD  ERACCT.
                                       COPY ERCACCT.

       FD  ERARCH.
                                       COPY ERCARCH.

       FD  ERPNDB.
                                       COPY ERCPNDB.

       FD  ELCERT.
                                       COPY ELCCERT.

111109 FD  ERCNOT.
111109                                 COPY ERCCNOT.

       FD  ELCNTL.
                                       COPY ELCCNTL.
072312
072312 FD  ELLETR.
072312                                 COPY ELCTEXT.
072312
072312 FD  ELENCC.
072312                                 COPY ELCENCC.

       FD  ERMAIL.
                                       COPY ERCMAIL.

       FD  FREEDOM-INT-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  FREEDOM-INT-RECORD          PIC X(400).

       FD  DISK-DATE                   COPY ELCDTEFD.

       FD  PRNTR                       COPY ELCPRTFD.

       FD  FICH                        COPY ELCFCHFD.
103111
103111 FD  NSASEXTR.
103111                                 COPY NSCASEXTR.                         

062017 FD  ERENDT.
062017                                 COPY ERCENDT.

       WORKING-STORAGE SECTION.
       01  LCP-ABND-CODE               PIC S999 COMP VALUE +519.
       77  LCP-ONCTR-01                PIC S9(8) COMP-3 VALUE ZERO.
       77  LCP-ASA                     PIC X.

       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*     EL570  WORKING STORAGE   *'.
       77  FILLER  PIC X(32)   VALUE '********* VMOD=2.001 ***********'.
       77  ERPNDB-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERACCT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
062017 77  ERENDT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERARCH-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELCERT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
062017 77  ELCERT-FIND-SW              PIC X   VALUE LOW-VALUES.
062017     88  ELCERT-FOUND               VALUE 'Y'.
111109 77  ERCNOT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELCNTL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERMAIL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
103111 77  NSAS-FILE-STATUS            PIC XX  VALUE LOW-VALUES.
072312 77  ELLETR-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
072312 77  ELENCC-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-CURRENT-BIN-DT           PIC XX  VALUE LOW-VALUES.
       77  WS-BIN-DT-PLUS-15           PIC XX  VALUE LOW-VALUES.
       77  WS-DT-PLUS-15               PIC X(8)  VALUE SPACES.
       77  WS-EOF-SW                   PIC X   VALUE SPACES.
           88  END-OF-INPUT                    VALUE 'Y'.

       77  WS-RECS-IN                  PIC 9(7) VALUE ZEROS.
062017 77  i1                          pic s999 comp-3 value +0.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  N1                          PIC S999 COMP-3 VALUE +0.
       77  WS-REF-AMT                  PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-INT-AMT                  PIC S9(7)V99 COMP-3 VALUE +0.
062017 77  ws-int-rate                 pic s99v9    comp-3 value +0.
       77  WS-LF-INT-AMT               PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-AH-INT-AMT               PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-LF-RATIO                 PIC S9V9(6) COMP-3 VALUE +0.
       77  WS-SKIP-CNT                 PIC 9(5) VALUE ZEROS.
       77  WS-PROCESS-PMT-SW           PIC X.
           88  PROCESS-PMT                VALUE 'Y'.
       77  WS-PROCESS-RERUN-SW         PIC X.
           88  PROCESS-RERUN              VALUE 'Y'.
       77  WS-DAYS                     PIC S9(5)    COMP-3 VALUE +0.
       77  WS-BIN-CANC-DT              PIC XX  VALUE LOW-VALUES.
       77  WS-ARCH-NUM                 PIC S9(9)  COMP-3 VALUE +0.
       77  WS-CERT-NOTE-LINE           PIC X(77).
       77  WS-ERNOTE-AMT               PIC $,$$$.99 VALUE ZEROS.
062017 77  ws-connect-sw               pic x  value spaces.
062017     88  connected-to-db            value 'Y'.
062017 77  ws-sql-code                 pic s9(7) value zeros.
062017 77  ws-dis-sql-code             pic -9999999 value zeros.

062017 EXEC SQL
062017    INCLUDE SQLDA
062017 END-EXEC
062017
062017 EXEC SQL
062017    INCLUDE SQLCA
062017 END-EXEC

062017 01  P pointer.
062017 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
062017 01  var-ptr pointer.
062017 01  env-var-len                 pic 9(4)  binary.
062017 01  rc                          pic 9(9)  binary.
062017
062017 01  WS-KIXSYS.
062017     05  WS-KIX-FIL1             PIC X(10).
062017     05  WS-KIX-APPS             PIC X(10).
062017     05  WS-KIX-ENV              PIC X(10).
062017     05  WS-KIX-MYENV            PIC X(10).
062017     05  WS-KIX-SYS              PIC X(10).
062017
062017 EXEC SQL
062017    BEGIN DECLARE SECTION
062017 END-EXEC
062017
062017 01  sqlcmd                      pic x(1024).
062017 01  WS-MOE-DATE                 pic x(10).
062017 01  svr                         pic x(32).
062017 01  usr                         pic x(32).
062017 01  pass                        pic x(32).
062017 01  usr-pass                    pic x(64).
062017 01  ws-disp-code                pic s9(11).

062017 01  NS-EL513-TABLE-RECORD.
062017     05  TB-MOE-DATE             PIC X(10).
062017     05  TB-CARRIER              PIC X.               
062017     05  TB-GROUPING             PIC X(6).            
062017     05  TB-STATE                PIC XX.              
062017     05  TB-ACCOUNT              PIC X(10).           
062017     05  TB-CERT-EFF-DT          PIC X(10).
062017     05  TB-CERT-NO              PIC X(11).
062017     05  TB-BATCH-NO             PIC X(6).
062017     05  tb-insured-name         pic x(15).
062017     05  tb-lf-cancel-date       pic x(10).
062017     05  tb-lf-cancel-amt        pic 9(7).99.
062017     05  tb-lf-cancel-amt-a redefines
062017         tb-lf-cancel-amt        pic x(10).
062017     05  tb-ah-cancel-date       pic x(10).
062017     05  tb-ah-cancel-amt        pic 9(7).99.
062017     05  tb-ah-cancel-amt-a redefines
062017         tb-ah-cancel-amt        pic x(10).
062017
062017 EXEC SQL
062017    END DECLARE SECTION
062017 END-EXEC

       01  WS-TRANSACTION-RECORD       PIC X(400).
       01  WS-CHECK-DES-RECORD         PIC X(400).
       01  WS-DISTRIBUTION-RECORD      PIC X(400).
       01  WS-PAYEE-ADDRESS-RECORD     PIC X(400).
       01  WS-ALPHA-1099-RECORD        PIC X(400).
       01  WS-VOUCH-ADDR-1099-RECORD   PIC X(400).

       01  WS-WORK-SSN.
           05  FILLER                  PIC 999  VALUE ZEROS.
           05  WS-SSN-YEAR             PIC 99.
           05  WS-SSN-SEQ-NO           PIC 9999.
       01  WS-INVOICE-NO.
           05  WS-INVOICE-MM           PIC 99.
           05  WS-INVOICE-DD           PIC 99.
           05  WS-INVOICE-CLM-NO       PIC X(7).
           05  FILLER                  PIC XX    VALUE SPACES.
       01  WS-REC-GRP-CD.
           05  WS-REC-GRP-DA           PIC X.
           05  WS-REC-GRP-SEQ-NO       PIC 999    VALUE 000.
       01  WS-USER-DEFINED.
           05  WS-USER-CARRIER         PIC X.
           05  WS-USER-ACCT-NO         PIC X(10).
           05  WS-USER-CERT-NO         PIC X(11).
           05  WS-USER-TRLR-SEQ-NO     PIC XX.
       01  FILLER                          COMP-3.                      
           05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   
           05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +60.   
           05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  
           05  WS-REPORT-SW                PIC S9          VALUE ZERO.  
           05  WS-HEADING-SW               PIC S9          VALUE ZERO.  
           05  WS-PRINT-SW                 PIC S9          VALUE ZERO.  
           05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  
           05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  
           05  WS-ZERO                     PIC S9          VALUE ZERO.  
                                                                        
           05  WS-INCURRED-AGE             PIC S9(3)       VALUE ZERO.  
           05  WS-YEAR REDEFINES                                        
               WS-INCURRED-AGE             PIC S9(3).                   
                                                                        
           05  WS-AMOUNT                   PIC S9(7)V99 VALUE ZERO.     

103111 01  WS-SV-DATA.
103111     05  WS-SV-INS-LNAME             PIC X(15).
103111     05  WS-SV-INS-FNAME             PIC X(10).
103111     05  WS-SV-INS-MIDINIT           PIC X(01).
103111     05  WS-SV-INS-ADDR1             PIC X(30).
103111     05  WS-SV-INS-ADDR2             PIC X(30).
103111     05  WS-SV-INS-CITY              PIC X(30).
103111     05  WS-SV-INS-STATE             PIC X(02).
103111     05  WS-SV-INS-ZIP               PIC X(10).
103111     05  WS-SV-ACCT-NAME             PIC X(30).
103111     05  WS-SV-BENE-NAME             PIC X(30).
062017     05  ws-sv-bene-addr1            pic x(30).
062017     05  ws-sv-bene-addr2            pic x(30).
062017     05  ws-sv-bene-city             pic x(30).
062017     05  ws-sv-bene-state            pic xx.
062017     05  ws-sv-bene-zip              pic x(10).

072312 01  WS-ENCLOSURE_DATA.
072312     05  WS-NC-OUT-STACK             PIC X(3).
072312     05  WS-NC-OUT-ENC-LINE          PIC X(50).
072312     05  WS-NC-OUT-ATTACH            PIC X(50).

       01  FILLER                          COMP SYNC.                   
           05  PGM-SUB                     PIC S9(4)       VALUE +317.  
           05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  
                                                                        
062017 01  filler.
062017     05  ws-lf-orig-prm-amt      pic s9(7)v99 comp-3 value +0.
062017     05  ws-lf-orig-alt-prm-amt  pic s9(7)v99 comp-3 value +0.
062017     05  ws-ah-orig-prm-amt      pic s9(7)v99 comp-3 value +0.
062017     05  ws-done-sw-endt         pic x value ' '.
062017         88  i-am-done             value 'Y'.
062017     05  ws-prev-state-key.
062017         10  ws-psk-carr         pic x.
062017         10  ws-psk-group        pic x(6).
062017         10  ws-psk-state        pic xx.
062017     05  ws-curr-state-key.
062017         10  ws-csk-carr         pic x.
062017         10  ws-csk-group        pic x(6).
062017         10  ws-csk-state        pic xx.
062017     05  ws-state-int-table occurs 70.
062017         10  ws-state-abbr           pic xx.
062017         10  ws-state-ref-int-rate   pic s9v9(4) comp-3 value +0.
062017
062017 01  FILLER.
062017     05  ws-temp-note                pic x(90)     value spaces.
062017     05  ws-display-ref-amt          pic $$$$$.99  value zeros.
062017     05  ws-display-int-amt          pic $$$$.99   value zeros.
062017     05  ws-display-int-rate         pic zz.9      value zeros.
           05  ABEND-CODE                  PIC X(4).                    
           05  ABEND-OPTION                PIC X.                       
           05  OLC-REPORT-NAME             PIC X(5)      VALUE 'EL570'. 
           05  X                           PIC X           VALUE SPACE. 
                                                                        
           05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.
                                                                        
           05  WS-LAST-CARRIER             PIC X           VALUE SPACES.
                                                                        
           05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.
                                                                        
           05  WS-LAST-MONTH               PIC 99          VALUE ZERO.  
           05  WS-LAST-MONTH-X REDEFINES                                
               WS-LAST-MONTH               PIC XX.                      
                                                                        
           05  WS-MONTH                    PIC XX          VALUE ZERO.  
                                                                        
           05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  
           05  WS-FIRST-TIME-SWITCH        PIC X           VALUE 'Y'.   
               88  WS-FIRST-TIME                           VALUE 'Y'.   

           05  WS-DATE-WORK.                                            
               10  WS-DW-MONTH             PIC 99.                      
               10  FILLER                  PIC X.                       
               10  WS-DW-DAY               PIC 99.                      
               10  FILLER                  PIC X.                       
               10  WS-DW-YEAR              PIC 99.                      
072312
072312 01  WS-ELENCC-KEY.
072312     05  WS-ELENCC-COMPANY-CD    PIC X.
072312     05  WS-ELENCC-REC-TYPE      PIC X.
072312     05  WS-ELENCC-ENC-CODE      PIC X(5).
072312     05  F                       PIC X(9).
072312
072312 01  WS-ELLETR-KEY.
072312     05  WS-ELLETR-COMPANY-CD    PIC X.
072312     05  WS-ELLETR-LETTER-ID     PIC X(12).
072312     05  WS-ELLETR-SEQ-NO        PIC S9(4) COMP VALUE +0.
072312
072312****  Z RECORD LAYOUT MOVED TO COPYBOOK ELCZREC
072312                                 COPY ELCZREC.                                                                        
           EJECT                                                        
                                       COPY ELCVOCH.
       01  FILLER.                                                      
           05  WS-TA-LIFE-OVERIDE-L6       PIC X(6)        VALUE SPACE. 
           05  FILLER                      PIC X(24)       VALUE        
               ' PAYMENTS'.                                             
           05  FILLER                      COMP-3.                      
               10  CT-CURR-LF-PMTS-AMT           PIC S9(9)V99  VALUE +0.
               10  CT-CURR-LF-PMTS-CNT           PIC S9(7)     VALUE +0.
                                                                        
               10  CT-CURR-AH-PMTS-AMT           PIC S9(9)V99  VALUE +0.
               10  CT-CURR-AH-PMTS-CNT           PIC S9(7)     VALUE +0.

               10  CT-CURR-TOT-PMTS-AMT          PIC S9(9)V99  VALUE +0.
               10  CT-CURR-TOT-PMTS-CNT          PIC S9(7)     VALUE +0.

               10  CT-CURR-LF-PMTS-AMT-BM        PIC S9(9)V99  VALUE +0.
               10  CT-CURR-LF-PMTS-CNT-BM        PIC S9(7)     VALUE +0.

               10  CT-CURR-LF-PMTS-AMT-DR        PIC S9(9)V99  VALUE +0.
               10  CT-CURR-LF-PMTS-CNT-DR        PIC S9(7)     VALUE +0.

062017     05  FILLER                      COMP-3.                      
062017         10  ST-CURR-LF-PMTS-AMT           PIC S9(9)V99  VALUE +0.
062017         10  ST-CURR-LF-PMTS-CNT           PIC S9(7)     VALUE +0.
062017                                                                  
062017         10  ST-CURR-AH-PMTS-AMT           PIC S9(9)V99  VALUE +0.
062017         10  ST-CURR-AH-PMTS-CNT           PIC S9(7)     VALUE +0.
062017
062017         10  ST-CURR-TOT-PMTS-AMT          PIC S9(9)V99  VALUE +0.
062017         10  ST-CURR-TOT-PMTS-CNT          PIC S9(7)     VALUE +0.
062017
062017         10  ST-CURR-LF-PMTS-AMT-BM        PIC S9(9)V99  VALUE +0.
062017         10  ST-CURR-LF-PMTS-CNT-BM        PIC S9(7)     VALUE +0.
062017
062017         10  ST-CURR-LF-PMTS-AMT-DR        PIC S9(9)V99  VALUE +0.
062017         10  ST-CURR-LF-PMTS-CNT-DR        PIC S9(7)     VALUE +0.

       01  WS-HEADING1.                                                 
           05  FILLER                      PIC X(44)       VALUE '1'.   
           05  WS-H1-TITLE                 PIC X(80)       VALUE
               'DAILY REFUND INTEREST PAYMENTS'.
           05  WS-H1-REPORT-NUMBER         PIC X(05)  VALUE 'EL570'.
                                                                        
       01  WS-HEADING2.                                                 
           05  FILLER                      PIC X(12)       VALUE SPACES.
           05  FILLER                      PIC X           VALUE SPACES.
           05  FILLER                      PIC X(32)       VALUE SPACES.
           05  WS-H2-CLIENT-NAME           PIC X(75)       VALUE SPACES.
           05  WS-H2-DATE                  PIC X(8).                    
                                                                        
       01  WS-HEADING3.                                                 
           05  FILLER                      PIC XX          VALUE SPACES.
           05  FILLER                      PIC X(30)       VALUE SPACES.
           05  FILLER                      PIC X(21)       VALUE SPACES.
           05  WS-H3-DATE                  PIC X(67)       VALUE SPACES.
           05  FILLER                      PIC X(5)        VALUE 'PAGE'.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  
           05  FILLER                      PIC X(02)       VALUE SPACES.
                                                                        
       01  WS-HEADING4.
           05  FILLER                      PIC X(133)     VALUE
               '0 CARR GROUP   ST  ACCOUNT     EFF DATE   CERT NO      I
      -'NT AMOUNT    SOC SEC NO   CANCEL DT  ENTRY DT    CSR    REF AMOU
      -'NT     DAYS'.

       01  WS-HEADING5.
           05  FILLER                      PIC X(26)      VALUE
               '   /PAYEE NAME AND ADDRESS'.

       01  WS-HEADING6.
062017     05  FILLER                      PIC X(60)      VALUE SPACES.

       01  WS-HEADING8.
           05  FILLER                      PIC X(26)      VALUE SPACES.

       01  WS-HEADING9.                                                 
           05  FILLER                      PIC X(43)    VALUE SPACES.   
           05  FILLER                      PIC X(5)     VALUE 'COUNT'.  
           05  FILLER                      PIC X(10)    VALUE SPACES.   
           05  FILLER                      PIC X(06)    VALUE 'AMOUNT'. 
           05  FILLER                      PIC X(69)    VALUE SPACES.   

       01  WS-DETAIL1.                                                  
           05  FILLER                      PIC X(4)  VALUE SPACES.
           05  WS-D1-CARRIER               PIC X.
           05  FILLER                      PIC XX    VALUE SPACES.
           05  WS-D1-GROUP                 PIC X(6).
           05  FILLER                      PIC XX    VALUE SPACES.
           05  WS-D1-STATE                 PIC XX.
           05  FILLER                      PIC X     VALUE SPACES.
           05  WS-D1-ACCOUNT               PIC X(10) VALUE SPACES.
           05  FILLER                      PIC XX.
           05  WS-D1-EFFECTIVE-DATE        PIC X(10).
           05  FILLER                      PIC X      VALUE SPACES.
           05  WS-D1-CERT-NO               PIC X(11).
           05  FILLER                      PIC XX     VALUE SPACES.
           05  WS-D1-AMOUNT                PIC Z,ZZZ,ZZ9.99-.
           05  FILLER                      PIC XX.
           05  WS-D1-SOC-SEC-NO            PIC X(11).
           05  FILLER                      PIC XX     VALUE SPACES.
           05  WS-D1-CANCEL-DT             PIC X(10).
           05  FILLER                      PIC X      VALUE SPACES.
           05  WS-D1-ENTRY-DT              PIC X(10).
           05  FILLER                      PIC XX     VALUE SPACES.
           05  WS-D1-CSR-CODE              PIC XXXX.
           05  FILLER                      PIC XX     VALUE SPACES.
           05  WS-D1-CANC-AMOUNT           PIC Z,ZZZ,ZZ9.99-.
           05  FILLER                      PIC XXX    VALUE SPACES.
           05  WS-D1-DAYS                  PIC ZZ99.
                                                                        
       01  WS-DETAIL2.
           05  FILLER                      PIC XXX.
           05  WS-D2-PAYEE-NAME            PIC X(30).
           05  FILLER                      PIC XX.
           05  WS-D2-PAYEE-ADDR1           PIC X(30).
           05  FILLER                      PIC XX.
           05  WS-D2-PAYEE-ADDR2           PIC X(30).
           05  FILLER                      PIC XX.
           05  WS-D2-PAYEE-ADDR3           PIC X(30).

033114 01  WS-DEATH-STATUS.
033114     05  FILLER                      PIC X VALUE SPACES.
033114     05  FILLER                      PIC X(23)
033114         VALUE '* DEATH CLAIM APPLIED *'.
033114     05  FILLER                      PIC X(109) VALUE SPACES.                                                                        
                                                                        
       01  WS-TOTAL-LINE1.
           05  FILLER                      PIC XX.                      
           05  WS-T1-DESCRIPTION.                                       
               10  WS-T1-BENEFIT-CODE      PIC XX.                      
               10  FILLER                  PIC X(4).                    
               10  WS-T1-BENEFIT-DESC      PIC X(10).                   
               10  FILLER                  PIC X(16).                   
                                                                        
           05  FILLER REDEFINES                                         
               WS-T1-DESCRIPTION.                                       
               10  FILLER                  PIC X(6).                    
               10  WS-T1-TOTAL-DESCRIPTION PIC X(26).                   
                                                                        
           05  FILLER REDEFINES                                         
               WS-T1-DESCRIPTION.                                       
               10  FILLER                  PIC X(8).                    
               10  WS-T1-VOIDED-DESCRIPTION PIC X(24).                  
                                                                        
           05  FILLER REDEFINES                                         
               WS-T1-DESCRIPTION.                                       
               10  FILLER                  PIC X(4).                    
               10  WS-T1-NET-DESCRIPTION PIC X(28).                     
                                                                        
           05  FILLER                      PIC X.                       
           05  WS-T1-DASH                  PIC X.                       
           05  FILLER                      PIC X.                       
           05  WS-T1-CURR-COUNT            PIC ZZZ,ZZZ,ZZ9-.            
           05  FILLER                      PIC X.                       
           05  WS-T1-CURR-AMOUNT           PIC ZZZ,ZZZ,ZZ9.99-.         
           05  FILLER                      PIC X.                       
                                                                        
       01  WS-TOTAL-LINE2.
           05  FILLER                      PIC XX.                      
           05  WS-T2-DESCRIPTION.                                       
               10  FILLER                  PIC X(6).                    
               10  WS-T2-PAYEE-DESC        PIC X(11).                   
               10  FILLER                  PIC X(15).                   
                                                                        
           05  FILLER REDEFINES                                         
               WS-T2-DESCRIPTION.                                       
               10  FILLER                  PIC X(6).                    
               10  WS-T2-TOTAL-DESCRIPTION PIC X(26).                   
                                                                        
           05  FILLER REDEFINES                                         
               WS-T2-DESCRIPTION.                                       
               10  FILLER                  PIC X(8).                    
               10  WS-T2-VOIDED-DESCRIPTION PIC X(24).                  
                                                                        
           05  FILLER REDEFINES                                         
               WS-T2-DESCRIPTION.                                       
               10  FILLER                  PIC X(4).                    
               10  WS-T2-NET-DESCRIPTION PIC X(28).                     
                                                                        
           05  FILLER                      PIC X.                       
           05  WS-T2-DASH                  PIC X.                       
           05  FILLER                      PIC X.                       
           05  WS-T2-CURR-COUNT            PIC ZZZ,ZZZ,ZZ9-.            
           05  FILLER                      PIC X.                       
           05  WS-T2-CURR-AMOUNT           PIC ZZZ,ZZZ,ZZ9.99-.         
           05  FILLER                      PIC X.                       
                                                                        
       01  WS-NO-ACTIVITY.                                              
           05  FILLER                      PIC X(30).                   
           05  FILLER                      PIC X(30) VALUE              
                 'NO ACTIVITY WAS FOUND FOR THIS'.                      
           05  FILLER                      PIC X(30) VALUE              
                 ' DATE RANGE                   '.                      

103111                                 COPY NSCASOUTB.
                                       COPY ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

041718 LINKAGE SECTION.
041718
041718 01  PARM.
041718     05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
041718     05  PARM-PREV-CYCLE-DT          PIC X(08)     VALUE SPACES.
041718     05  PARM-CYCLE-DT               PIC X(08)     VALUE SPACES.
041718
041718 01  var                         pic x(30).

       PROCEDURE DIVISION.

       0000-DATE-CARD-READ SECTION.

                                       COPY ELCDTERX.

041718     set P to address of KIXSYS
041718     CALL "getenv" using by value P returning var-ptr
041718     if var-ptr = null then
041718        display ' kixsys not set '
041718     else
041718        set address of var to var-ptr
041718        move 0 to env-var-len
041718        inspect var tallying env-var-len
041718          for characters before X'00' 
041718        unstring var (1:env-var-len) delimited by '/'
041718           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
041718              WS-KIX-SYS
041718        end-unstring
041718     end-if
041718
041718     display ' KIXSYS  ' ws-kix-myenv

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              END-OF-INPUT

062017     EXEC SQL
062017        DISCONNECT
062017     END-EXEC
062017
062017     if sqlcode not = 0
062017        display "Error: disconnect "
062017        display ' sql return code ' sqlcode
062017        display ' sql err mess    ' sqlerrmc
062017     end-if
062017
062017     perform 0500-state-totals   thru 0500-exit
062017
062017     move +1                     to WS-REPORT-SW
           PERFORM 4000-PRINT-TOTALS   THRU 4900-EXIT

           PERFORM CLOSE-FILES         THRU CFS-EXIT.

           GOBACK.

       0010-OPEN-FILES.

062017     OPEN INPUT ELCERT ERMAIL ERACCT ERENDT
072312             ELLETR ELENCC
      *            ERPNDB ELCNTL
111109     OPEN I-O   ERPNDB ELCNTL ERARCH ERCNOT
103111                NSASEXTR
           OPEN OUTPUT FREEDOM-INT-FILE PRNTR

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERARCH-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERARCH - OPEN ' ERARCH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPNDB-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERPNDB - OPEN ' ERPNDB-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCERT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ELCERT - OPEN ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

111109     IF ERCNOT-FILE-STATUS NOT = '00' AND '97'
111109        DISPLAY ' ERROR - ERCNOT - OPEN ' ERCNOT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCNTL-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ELCNTL - OPEN ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERMAIL - OPEN ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
103111
103111     IF NSAS-FILE-STATUS NOT = '00' AND '97'
103111        DISPLAY ' ERROR - NSASEXTR - OPEN ' NSAS-FILE-STATUS
103111        PERFORM ABEND-PGM
103111     END-IF
072312
072312     IF ELLETR-FILE-STATUS NOT = '00' AND '97'
072312        MOVE ' ELLETR OPEN ERROR ' TO WS-ABEND-MESSAGE
072312        MOVE ELLETR-FILE-STATUS    TO WS-ABEND-FILE-STATUS
072312        PERFORM ABEND-PGM
072312     END-IF.
072312
072312     IF ELENCC-FILE-STATUS NOT = '00' AND '97'
072312        MOVE ' ELENCC OPEN ERROR ' TO WS-ABEND-MESSAGE
072312        MOVE ELENCC-FILE-STATUS    TO WS-ABEND-FILE-STATUS
072312        PERFORM ABEND-PGM
072312     END-IF.

062017     IF ERENDT-FILE-STATUS NOT = '00' AND '97'
062017        DISPLAY ' ERROR - ERENDT - OPEN ' ERENDT-FILE-STATUS
062017        PERFORM ABEND-PGM
062017     END-IF

           MOVE RUN-MO                 TO DC-MDY-MONTH
           MOVE RUN-DA                 TO DC-MDY-DAY
           MOVE RUN-YR                 TO DC-MDY-YEAR
                                          WS-SSN-YEAR
           MOVE '4'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           DISPLAY ' WS-FN-DATE ' WS-FN-DATE
           MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DT
           ELSE
              DISPLAY 'BAD CURRENT DATE ' WS-FN-DATE
              PERFORM ABEND-PGM
           END-IF

           MOVE WS-CURRENT-BIN-DT      TO DC-BIN-DATE-1
           MOVE +15                    TO DC-ELAPSED-DAYS
           MOVE +0                     TO DC-ELAPSED-MONTHS
           MOVE '6'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO WS-BIN-DT-PLUS-15
           ELSE
              DISPLAY 'BAD CURRENT DATE PLUS 15' WS-FN-DATE
              PERFORM ABEND-PGM
           END-IF

           MOVE WS-BIN-DT-PLUS-15      TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO WS-DT-PLUS-15
           ELSE
              DISPLAY 'BAD BIN DATE +15 ' WS-FN-DATE
              PERFORM ABEND-PGM
           END-IF

062017     perform varying i1 from +1 by +1 until i1 > +70
062017        move spaces              to ws-state-abbr (i1)
062017        move zeros               to ws-state-ref-int-rate (i1)
062017     end-perform

           MOVE LIFE-OVERRIDE-L6       TO WS-TA-LIFE-OVERIDE-L6.

           MOVE SPACES                 TO TRANSACTION-RECORD
                                          CHECK-DES-RECORD
                                          DISTRIBUTION-RECORD
                                          PAYEE-ADDRESS-RECORD
                                          ALPHA-1099-RECORD
                                          VOUCH-ADDR-1099-RECORD
                                          
           MOVE 'T'                    TO TR-RECORD-ID
           MOVE 'F'                    TO CD-RECORD-ID
           MOVE 'D'                    TO DR-RECORD-ID
           MOVE 'A'                    TO PR-RECORD-ID
           MOVE '1'                    TO AR-RECORD-ID
           MOVE '2'                    TO VR-RECORD-ID
           MOVE WS-FN-DA (2:1)         TO WS-REC-GRP-DA
           MOVE 'PSEUDO-INT'           TO TR-VENDOR-ID
                                          CD-VENDOR-ID
                                          DR-VENDOR-ID
                                          PR-VENDOR-ID
                                          AR-VENDOR-ID
                                          VR-VENDOR-ID
           MOVE 'P'                    TO TR-CHECK-TYPE
           MOVE 'R'                    TO TR-TRAN-TYPE
041712     IF DTE-CLIENT = 'AHL'
041712         MOVE 'AHL'              TO TR-CSO
041712     ELSE
041712         MOVE 'CSO'              TO TR-CSO
041712     END-IF

           STRING WS-FN-MO WS-FN-DA WS-FN-CCYR DELIMITED BY SIZE
              INTO TR-INVOICE-DATE
           END-STRING
              
           MOVE 'IMM'                  TO TR-TERMS-CODE
           MOVE 001                    TO CD-SEQ-NO
                                          DR-SEQ-NO
           MOVE 'E'                    TO DR-DIST-TYPE (1)

      *    MOVE '6146000100026620CRLIFS'


032212     if dte-client = 'AHL'
032212        MOVE '6146000150A10A10CRLIFS'
032212                                 TO DR-ACCT-NO (1)
032212     else
032212        MOVE '6146000150026620CRLIFS'
032212                                 TO DR-ACCT-NO (1)
032212     end-if

           MOVE 'AP'                   TO DR-SOURCE (1)
           MOVE 'S'                    TO DR-SUSPENSE (1) (1:1)
           MOVE 'INT'                  TO AR-CATEGORY
                                          VR-CATEGORY
           MOVE 'S'                    TO AR-TAX-ID-TYPE
           MOVE 'Y'                    TO AR-ACCUM-BY-ACCT
           MOVE 'U'                    TO VR-ADDRESS-TYPE
           MOVE ZEROS                  TO TR-NON-DIS-AMT
                                          TR-DIS-PCT
                                          TR-DIS-AMT
                                          TR-TAX-PCT
                                          TR-TAX-ADJ
                                          TR-TAX-AMT
                                          DR-USE-TAX-AMT (1)
           
           MOVE '+'                    TO TR-NON-DIS-AMT-SIGN
                                          TR-DIS-AMT-SIGN
                                          TR-TAX-ADJ-SIGN
                                          TR-TAX-AMT-SIGN
                                          DR-USE-TAX-AMT-SIGN (1)

           MOVE TRANSACTION-RECORD     TO WS-TRANSACTION-RECORD
           MOVE CHECK-DES-RECORD       TO WS-CHECK-DES-RECORD
           MOVE DISTRIBUTION-RECORD    TO WS-DISTRIBUTION-RECORD
           MOVE PAYEE-ADDRESS-RECORD   TO WS-PAYEE-ADDRESS-RECORD
           MOVE ALPHA-1099-RECORD      TO WS-ALPHA-1099-RECORD
           MOVE VOUCH-ADDR-1099-RECORD TO WS-VOUCH-ADDR-1099-RECORD

      *    MOVE ZEROS                  TO WS-LINE-COUNT
      *    MOVE '1<HTML>'              TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' <HEAD>'              TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' <PRE>'               TO PRT
      *    PERFORM WRITE-A-LINE
           MOVE +99                    TO WS-LINE-COUNT

072312     PERFORM 0250-GET-ENCLOSURE  THRU 0250-EXIT
072312            
           PERFORM 0210-GET-ARCHNO     THRU 0210-EXIT
062017     perform 0230-build-ref-int-table
062017                                 thru 0230-exit

           PERFORM 0070-START-ERPNDB   THRU 0070-EXIT
           PERFORM 0060-READ-ERPNDB    THRU 0060-EXIT

           .
       0020-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

062017     IF (PB-STATE = 'NH' or 'TN')
              AND (PB-CANCELLATION)
              AND (PB-C-REF-INTERFACE-SW NOT = 'Y')
              AND (NOT PB-FATAL-ERRORS)
              AND (NOT PB-UNFORCED-ERRORS)
              PERFORM 0100-CHECK-REF   THRU 0100-EXIT
           END-IF

      *    IF PB-CANCELLATION
      *       AND (PB-C-REF-INTERFACE-SW NOT = 'Y')
      *       AND (NOT PB-FATAL-ERRORS)
      *       AND (NOT PB-UNFORCED-ERRORS)
      *       PERFORM 0100-CHECK-REF   THRU 0100-EXIT
      *    END-IF

           PERFORM 0060-READ-ERPNDB    THRU 0060-EXIT

           .
       0050-EXIT.
           EXIT.
           
       0060-READ-ERPNDB.

           READ ERPNDB NEXT RECORD
           
           IF (ERPNDB-FILE-STATUS = '10' OR '23')
062017        OR (PB-COMPANY-CD-a1 NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERPNDB-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERPNDB - READ ' ERPNDB-FILE-STATUS
              ELSE
                 ADD 1                 TO WS-RECS-IN
              END-IF
           END-IF

           .
       0060-EXIT.
           EXIT.
           
       0070-START-ERPNDB.

062017     MOVE DTE-CLASIC-COMPANY-CD  TO PB-CONTROL-by-account

062017     START ERPNDB KEY >= PB-CONTROL-by-account
           
           IF (ERPNDB-FILE-STATUS = '10' OR '23')
062017        OR (PB-COMPANY-CD-a1 NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERPNDB-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERPNDB - START ' ERPNDB-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0070-EXIT.
           EXIT.

       0100-CHECK-REF.

062017     move pb-carrier             to ws-csk-carr
062017     move pb-grouping            to ws-csk-group
062017     move pb-state               to ws-csk-state
062017
062017     if ws-prev-state-key <> ws-curr-state-key
062017        perform 0500-state-totals
062017                                 thru 0500-exit
062017        move ws-curr-state-key   to ws-prev-state-key
062017     end-if
062017
062017*    display ' made 0100 check ref ' pb-state ' ' pb-account
062017*      ' ' pb-cert-no ' ' pb-entry-batch

           MOVE +0                     TO WS-INT-AMT WS-REF-AMT
                                          WS-LF-INT-AMT WS-AH-INT-AMT
062017                                    WS-LF-RATIO ws-int-rate
103111
103111     MOVE SPACES                 TO WS-SV-DATA

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE PB-CONTROL-BY-ACCOUNT (1:22)
                                       TO AM-CONTROL-PRIMARY (1:22)

           START ERACCT KEY > AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS = '10' OR '23'
              DISPLAY ' NO ERACCT FOUND '
              PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
              GO TO 0100-EXIT
           END-IF

           READ ERACCT NEXT RECORD 
           IF ERACCT-FILE-STATUS = '00'
              IF AM-GPCD NOT = 02 AND 03 AND 04 AND 05
                 CONTINUE
              ELSE
062017           if am-state = 'NH'
062017              DISPLAY ' REFUND BYPASSED, BUS TYPE '
062017                PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO ' '
062017                  AM-GPCD
062017              GO TO 0100-EXIT
062017           end-if
              END-IF
           ELSE
              DISPLAY ' BAD READ ON ERACCT '
                 PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
           END-IF

103111     MOVE AM-NAME               TO WS-SV-ACCT-NAME
103111
062017     if pb-state = 'NH'
062017        IF PB-CERT-EFF-DT = PB-C-LF-CANCEL-DT OR PB-C-AH-CANCEL-DT
062017           display ' NH FLAT CANCEL, BYPASS ' pb-state ' '
062017              pb-account ' ' pb-cert-no ' ' pb-entry-batch
062017           GO TO 0100-ERPNDB-REWRITE
062017        END-IF
062017     END-IF

           IF (PB-C-LF-CANCEL-AMT + PB-C-AH-CANCEL-AMT) = ZEROS
              DISPLAY ' REF BYPASSED, ZERO REFUND '
                 PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
              GO TO 0100-ERPNDB-REWRITE
           END-IF

           MOVE PB-C-LF-CANCEL-DT      TO DC-BIN-DATE-1
                                          WS-BIN-CANC-DT
           IF PB-C-AH-CANCEL-DT > DC-BIN-DATE-1
              MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1
                                          WS-BIN-CANC-DT
           END-IF

022414     if dc-bin-date-1 > bin-run-date
022414        display ' Refund bypassed, cancel date in the future '
022414           PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
022414        GO TO 0100-ERPNDB-REWRITE
022414     END-IF

062017     if pb-state <> 'NH'
062017        go to 0100-calc-tn-duration
062017     end-if

           MOVE PB-INPUT-DT            TO DC-BIN-DATE-2
           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              IF DC-ELAPSED-DAYS > +30
                 COMPUTE WS-DAYS = (DC-ELAPSED-DAYS - +30) + +15
              ELSE
                 GO TO 0100-ERPNDB-REWRITE
              END-IF
           ELSE
              DISPLAY ' CANCEL AND ENTRY DATE CONVERSION ERROR '
                 PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
              PERFORM ABEND-PGM
           END-IF

           MOVE DC-BIN-DATE-1          TO DC-BIN-DATE-2
           MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1

           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              IF DC-ELAPSED-DAYS < +16
                 DISPLAY ' REFUND WITHIN 15 DAYS OF EFFECT DATE '
                    PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
                 GO TO 0100-ERPNDB-REWRITE
              END-IF
           ELSE
              DISPLAY ' CANCEL AND EFFECT DATE CONVERSION ERROR '
                 PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
              PERFORM ABEND-PGM
           END-IF
062017     go to 0100-interest-calculation
062017
062017     .
062017 0100-calc-tn-duration.
062017
062017     MOVE pb-cert-eff-dt         TO DC-BIN-DATE-1
062017     MOVE SPACES                 TO DC-OPTION-CODE
062017     PERFORM 8500-DATE-CONVERSION
062017                                 THRU 8590-EXIT
062017     IF NO-CONVERSION-ERROR
062017        MOVE DC-GREG-DATE-A-EDIT TO tb-cert-eff-dt
062017     ELSE
062017        display ' error converting eff dt ' pb-cert-no
062017        go to 0100-erpndb-rewrite
062017     END-IF
062017
062017     move pb-state               to tb-state
062017     move pb-account             to tb-account
062017     move pb-cert-no             to tb-cert-no
062017     move pb-entry-batch         to tb-batch-no
062017     move pb-c-lf-cancel-amt     to tb-lf-cancel-amt
062017     move pb-c-ah-cancel-amt     to tb-ah-cancel-amt
062017     if not connected-to-db
062017        perform 0400-connect     thru 0400-exit
062017     end-if
062017
062017*    display ' tb  state      ' tb-state
062017*    display ' tb account     ' tb-account
062017*    display ' tb cert no     ' tb-cert-no
062017*    display ' tb cert eff dt ' tb-cert-eff-dt
062017*    display ' tb batch no    ' tb-batch-no
062017*    display ' tb lf cancel   ' tb-lf-cancel-amt
062017*    display ' tb ah cancel   ' tb-ah-cancel-amt
062017
062017     exec sql
062017        select
062017           Insured_Name
062017        into
062017           :tb-insured-name
062017        from
062017           NAPERSOFT_EL513A
062017        where
062017                State         = :tb-state
062017            and Account       = :tb-account
062017            and Cert_No       = :tb-cert-no
062017            and Cert_Eff_Dt   = :tb-cert-eff-dt
062017            and Batch_No      = :tb-batch-no
062017            and Lf_Cancel_Amt = :tb-lf-cancel-amt-a
062017            and Ah_Cancel_Amt = :tb-ah-cancel-amt-a
062017     end-exec
062017
062017     if sqlcode not = 0 and 1
062017        display "Error: Did not find auto cnc rec "
062017        move sqlcode             to ws-sql-code
062017        move ws-sql-code         to ws-dis-sql-code
062017        display ' sqlcode ' ws-dis-sql-code
062017        display ' sql err mess    ' sqlerrmc
062017        go to 0100-erpndb-rewrite
062017     end-if
062017
062017*    display ' found one ' pb-state ' ' pb-cert-no ' '
062017*       tb-insured-name
062017     MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1
062017*    MOVE WS-CURRENT-BIN-DT      TO DC-BIN-DATE-2
           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-2
062017
062017     MOVE '1'                    TO DC-OPTION-CODE
062017     PERFORM 8500-DATE-CONVERSION
062017                                 THRU 8590-EXIT
062017     IF NO-CONVERSION-ERROR
062017        move dc-elapsed-days     to ws-days
062017     ELSE
062017        DISPLAY ' CURRENT AND EFFECT DATE CONVERSION ERROR '
062017           PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
062017        PERFORM ABEND-PGM
062017     END-IF
062017
062017     .
062017 0100-interest-calculation.

062017     MOVE ' '                    TO ELCERT-FIND-SW
062017     MOVE PB-CONTROL-BY-ACCOUNT (1:33)
062017                                 TO CM-CONTROL-PRIMARY
062017
062017     READ ELCERT
062017     IF ELCERT-FILE-STATUS = '10' OR '23'
062017        DISPLAY ' ELCERT  RECORD NOT FOUND ' PB-STATE ' '
062017           PB-ACCOUNT ' ' PB-CERT-NO
062017     ELSE
062017        IF ELCERT-FILE-STATUS NOT = '00'
062017           DISPLAY ' ERROR - ELCERT - READ ' ELCERT-FILE-STATUS
062017           ' ' PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
062017        else
062017           set elcert-found to true
062017        END-IF
062017     END-IF
062017
062017     if pb-state = ws-state-abbr (i1)
062017        continue
062017     else
062017        perform varying i1 from +1 by +1 until
062017           (i1 > +70)
062017           or (ws-state-abbr (i1) = pb-state)
062017           or (ws-state-abbr (i1) = spaces)
062017        end-perform
062017        if (i1 > +70)
062017           or (ws-state-ref-int-rate (i1) = zeros)
062017           or (ws-state-abbr (i1) = spaces)
062017           DISPLAY ' REF BYPASSED, NO INTEREST '
062017              PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
062017           GO TO 0100-ERPNDB-REWRITE
062017        end-if
062017     end-if
062017
062017     compute ws-int-rate =
062017        ws-state-ref-int-rate (i1) * +100
062017
062017     move pb-c-lf-cancel-amt     to ws-lf-orig-prm-amt
062017     move +0                     to ws-lf-orig-alt-prm-amt
062017     move pb-c-ah-cancel-amt     to ws-ah-orig-prm-amt
062017
062017     if pb-state not = 'TN'
062017        go to 0100-continue
062017     end-if
062017
062017     move pb-ci-lf-premium-amt   to ws-lf-orig-prm-amt
062017     move pb-ci-lf-alt-premium-amt
062017                                 to ws-lf-orig-alt-prm-amt
062017     move pb-ci-ah-premium-amt   to ws-ah-orig-prm-amt
062017
062017     move pb-control-by-account  to en-control-primary
062017     move 'I'                    to en-rec-type
062017     move +0                     to en-seq-no
062017     start ERENDT key >= en-control-primary
062017     if erendt-file-status <> '00'
062017        go to 0100-continue
062017     end-if
062017     move ' '                    to ws-done-sw-endt
062017     perform until i-am-done
062017        read ERENDT next record
062017        if (erendt-file-status = '00')
062017           and (pb-control-by-account(1:33) =
062017              en-control-primary(1:33))
062017           and (en-rec-type = 'I')
062017           move en-lf-orig-prm-amt  to ws-lf-orig-prm-amt
062017           move en-lf-orig-alt-prm-amt
062017                                    to ws-lf-orig-alt-prm-amt
062017           move en-ah-orig-prm-amt  to ws-ah-orig-prm-amt
062017        else
062017           set i-am-done to true
062017        end-if
062017     end-perform
062017
062017     .
062017 0100-continue.
062017
062017     compute ws-ref-amt =
062017        ws-lf-orig-prm-amt + ws-lf-orig-alt-prm-amt +
062017        ws-ah-orig-prm-amt

062017*    COMPUTE WS-REF-AMT =
062017*       PB-C-LF-CANCEL-AMT + PB-C-AH-CANCEL-AMT
062017
062017     COMPUTE WS-INT-AMT ROUNDED = (WS-REF-AMT * 
062017           ((1 + ws-state-ref-int-rate (i1)) **
062017              (WS-DAYS / 365) - 1))

           IF WS-INT-AMT = ZEROS
              DISPLAY ' REF BYPASSED, ZERO INTEREST '
                 PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
              GO TO 0100-ERPNDB-REWRITE
           END-IF

           IF WS-REF-AMT NOT = ZEROS
              COMPUTE WS-LF-RATIO = PB-C-LF-CANCEL-AMT / WS-REF-AMT
              COMPUTE WS-LF-INT-AMT = WS-INT-AMT * WS-LF-RATIO
              COMPUTE WS-AH-INT-AMT = WS-INT-AMT - WS-LF-INT-AMT
           END-IF

           PERFORM 0120-GL-PROCESSING  THRU 0120-EXIT

           PERFORM 0150-PROCESS-ERMAIL THRU 0150-EXIT

           IF (AR-TAX-ID-NO = ZEROS)
              AND (WS-INT-AMT < 600.01)
              AND (WS-INT-AMT > .99)
              ADD 1                    TO WS-SSN-SEQ-NO
              MOVE WS-WORK-SSN         TO AR-TAX-ID-NO
                                          DR-SUSPENSE (1) (2:9)
           END-IF

           MOVE AR-TAX-ID-NO           TO TR-INVOICE-NO
                                          CD-INVOICE-NO
                                          DR-INVOICE-NO
                                          PR-INVOICE-NO
                                          AR-INVOICE-NO
                                          VR-INVOICE-NO

111109     PERFORM 0160-PROCESS-ERCNOT THRU 0160-EXIT

           PERFORM 0130-PRINT-REPORT   THRU 0130-EXIT

           IF WS-INT-AMT < 1.00
              CONTINUE
           ELSE
              PERFORM 0140-CREATE-FREEDOM
                                       THRU 0140-EXIT
           END-IF

           .
       0100-ERPNDB-REWRITE.

           MOVE 'Y'                    TO PB-C-REF-INTERFACE-SW

           IF WS-INT-AMT < 1.00
              CONTINUE
           ELSE
062017        MOVE WS-INT-AMT          TO PB-C-INT-ON-REFS
           END-IF

           REWRITE PENDING-BUSINESS
           IF ERPNDB-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERPNDB - REWRITE ' ERPNDB-FILE-STATUS
                 ' ' PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
           END-IF

           .
       0100-EXIT.
           EXIT.

       0120-GL-PROCESSING.

           MOVE SPACES                 TO WS-USER-DEFINED
           MOVE PB-CARRIER             TO WS-USER-CARRIER
           MOVE PB-ACCOUNT             TO WS-USER-ACCT-NO
           MOVE PB-CERT-NO             TO WS-USER-CERT-NO
           MOVE WS-USER-DEFINED        TO TR-USER-DEFINED

           MOVE WS-FN-MO               TO WS-INVOICE-MM
           MOVE WS-FN-DA               TO WS-INVOICE-DD
           MOVE PB-ACCOUNT             TO WS-INVOICE-CLM-NO
           MOVE PB-STATE               TO DR-ACCT-STATE (1)
           MOVE WS-INVOICE-NO          TO TR-INVOICE-NO
                                          CD-INVOICE-NO
                                          DR-INVOICE-NO
                                          PR-INVOICE-NO
                                          AR-INVOICE-NO
                                          VR-INVOICE-NO
           
      *    COMPUTE WS-REF-AMT = PB-C-LF-CANCEL-AMT + PB-C-AH-CANCEL-AMT

      *    COMPUTE WS-INT-AMT ROUNDED = (WS-REF-AMT * 
      *          (1.10 ** (WS-DAYS / 365) - 1))


           MOVE WS-INT-AMT             TO TR-INVOICE-AMT
                                          CD-INVOICE-AMT (1)
                                          DR-INVOICE-AMT (1)
                                          AR-INVOICE-AMT (1)
                                          
           IF WS-INT-AMT < ZEROS
              MOVE '-'                 TO TR-INVOICE-AMT-SIGN
                                          CD-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (2)
           ELSE
              MOVE '+'                 TO TR-INVOICE-AMT-SIGN
                                          CD-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (1)
                                          AR-INVOICE-AMT-SIGN (1)
           END-IF

           STRING 'CK ' 'CC ' PB-LAST-MAINT-BY ' INT ON REF'
              DELIMITED BY SIZE INTO TR-VOUCHER-REF
           END-STRING

           STRING ' INSURED : ' PB-C-LAST-NAME
              DELIMITED BY SIZE INTO CD-DESC (1)
           END-STRING

062017     move pb-c-last-name         to vr-vendor-name (1)
062017     if pb-state = 'NH'
062017        MOVE PB-C-LAST-NAME      TO PR-PAYEE-NAME
062017     else
062017        move pb-ci-beneficiary-name
062017                                 to pr-payee-name
062017     end-if

           .
       0120-EXIT.
           EXIT.

       0130-PRINT-REPORT.

           MOVE '0'                    TO WS-DETAIL1
      *    MOVE SPACES                 TO WS-D1-CHECK-NUMBER

           MOVE WS-INT-AMT             TO WS-D1-AMOUNT
           MOVE WS-REF-AMT             TO WS-D1-CANC-AMOUNT
           MOVE WS-DAYS                TO WS-D1-DAYS

      *    MOVE 'INTEREST'             TO WS-D1-PAY-TYPE

      *    MOVE PB-C-LAST-NAME         TO WS-D1-CLAIMANT-NAME

      *    MOVE PB-ACCOUNT         TO  WS-D1-CLAIM-NO

           MOVE WS-BIN-CANC-DT         TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-D1-CANCEL-DT
           ELSE
              MOVE 'ERROR'             TO WS-D1-CANCEL-DT
           END-IF

           MOVE PB-INPUT-DT            TO DC-BIN-DATE-1
           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-D1-ENTRY-DT
           ELSE
              MOVE 'ERROR'             TO WS-D1-ENTRY-DT
           END-IF

           MOVE PB-CSR-ID              TO WS-D1-CSR-CODE
           MOVE PB-CARRIER             TO WS-D1-CARRIER
           MOVE PB-CERT-NO             TO WS-D1-CERT-NO
           MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           MOVE DC-GREG-DATE-A-EDIT    TO WS-D1-EFFECTIVE-DATE
           MOVE PB-STATE               TO WS-D1-STATE
           MOVE PB-ACCOUNT             TO WS-D1-ACCOUNT
           MOVE PB-GROUPING            TO WS-D1-GROUP

           MOVE SPACES                 TO WS-DETAIL2

           MOVE PR-PAYEE-NAME          TO WS-D2-PAYEE-NAME
           MOVE PR-ADDRESS (1)         TO WS-D2-PAYEE-ADDR1
           MOVE PR-ADDRESS (2)         TO WS-D2-PAYEE-ADDR2
           MOVE PR-ADDRESS (3)         TO WS-D2-PAYEE-ADDR3

           IF ERMAIL-FILE-STATUS NOT = '00'
              MOVE ' **** MISSING **** '
                                       TO WS-D2-PAYEE-NAME
                                          WS-D2-PAYEE-ADDR1
                                          WS-D2-PAYEE-ADDR2
                                          WS-D2-PAYEE-ADDR3
           END-IF

      *    IF AR-TAX-ID-NO = ZEROS OR SPACES
      *       MOVE '*MISSING*'         TO AR-TAX-ID-NO
      *                                   DR-SUSPENSE (1) (2:9)
      *       MOVE '123456789'         TO AR-TAX-ID-NO
      *                                   DR-SUSPENSE (1) (2:9)
      *    END-IF

           MOVE AR-TAX-ID-NO           TO WS-D1-SOC-SEC-NO

           IF WS-INT-AMT < ZEROS
              ADD +1                   TO CT-CURR-LF-PMTS-CNT-DR
062017                                    ST-CURR-LF-PMTS-CNT-DR
              ADD WS-INT-AMT           TO CT-CURR-LF-PMTS-AMT-DR
062017                                    ST-CURR-LF-PMTS-AMT-DR
              MOVE ' **** DROPPED **** '
                                       TO WS-D2-PAYEE-NAME
                                          WS-D2-PAYEE-ADDR1
              MOVE ' **** NEG AMT **** '
                                       TO WS-D2-PAYEE-ADDR2
                                          WS-D2-PAYEE-ADDR3
           END-IF

           IF WS-D2-PAYEE-NAME = ' **** MISSING **** '
              ADD +1                   TO CT-CURR-LF-PMTS-CNT-DR
062017                                    ST-CURR-LF-PMTS-CNT-DR
              ADD WS-INT-AMT           TO CT-CURR-LF-PMTS-AMT-DR
062017                                    ST-CURR-LF-PMTS-AMT-DR
              MOVE ' **** DROPPED **** '
                                       TO WS-D2-PAYEE-NAME
                                          WS-D2-PAYEE-ADDR1
           END-IF

           IF WS-INT-AMT < 1.00
              ADD +1                   TO CT-CURR-LF-PMTS-CNT-BM
062017                                    ST-CURR-LF-PMTS-CNT-BM
              ADD WS-INT-AMT           TO CT-CURR-LF-PMTS-AMT-BM
062017                                    ST-CURR-LF-PMTS-AMT-BM
              MOVE ' **** DROPPED **** '
                                       TO WS-D2-PAYEE-NAME
                                          WS-D2-PAYEE-ADDR1
              MOVE ' **BELOW MINIMUM** '
                                       TO WS-D2-PAYEE-ADDR2
                                          WS-D2-PAYEE-ADDR3
           END-IF

           IF WS-LINE-COUNT > (WS-LINE-COUNT-MAX - 2)
              MOVE 61                  TO WS-LINE-COUNT
           END-IF

           MOVE WS-DETAIL1             TO PRT
           PERFORM WRITE-A-LINE

           MOVE WS-DETAIL2             TO PRT
           PERFORM WRITE-A-LINE

           .
       0130-EXIT.
           EXIT.
           
       0140-CREATE-FREEDOM.

           IF WS-D2-PAYEE-NAME = ' **** DROPPED **** '
              GO TO 0140-EXIT
           END-IF

           ADD 1                       TO WS-REC-GRP-SEQ-NO
           MOVE WS-REC-GRP-CD          TO TR-REC-GRP-CODE
                                          CD-REC-GRP-CODE
                                          DR-REC-GRP-CODE
                                          PR-REC-GRP-CODE
                                          AR-REC-GRP-CODE
                                          VR-REC-GRP-CODE

           WRITE FREEDOM-INT-RECORD    FROM TRANSACTION-RECORD
      *    WRITE FREEDOM-INT-RECORD    FROM CHECK-DES-RECORD

           IF WS-LF-INT-AMT NOT = ZEROS
032212        if dte-client = 'AHL'
032212           MOVE '6146000150A10A10CRLIFS'
032212                                 TO DR-ACCT-NO (1)
032212        else
032212           MOVE '6146000150026620CRLIFS'
032212                                 TO DR-ACCT-NO (1)
032212        end-if
              MOVE WS-LF-INT-AMT       TO DR-INVOICE-AMT (1)
              WRITE FREEDOM-INT-RECORD FROM DISTRIBUTION-RECORD
           END-IF

           IF WS-AH-INT-AMT NOT = ZEROS
032212        if dte-client = 'AHL'
032212           MOVE '6140000150A10A10CRDISS'
032212                                 TO DR-ACCT-NO (1)
032212        else
032212           MOVE '6140000150026620CRDISS'
032212                                 TO DR-ACCT-NO (1)
032212        end-if
              MOVE WS-AH-INT-AMT       TO DR-INVOICE-AMT (1)
              WRITE FREEDOM-INT-RECORD FROM DISTRIBUTION-RECORD
           END-IF

           MOVE 'P'                    TO DR-DIST-TYPE (1)
032212     if dte-client = 'AHL'
032212        MOVE '1108121010000000000000'
032212                                 TO DR-ACCT-NO (1)
032212     else
032212        MOVE '1108124700000000000000'
032212                                 TO DR-ACCT-NO (1)
032212     end-if
           MOVE WS-INT-AMT             TO DR-INVOICE-AMT (1)
           MOVE '00'                   TO DR-ACCT-STATE (1)
           MOVE SPACES                 TO DR-SUSPENSE (1)
           WRITE FREEDOM-INT-RECORD    FROM DISTRIBUTION-RECORD
           WRITE FREEDOM-INT-RECORD    FROM PAYEE-ADDRESS-RECORD
           WRITE FREEDOM-INT-RECORD    FROM ALPHA-1099-RECORD
           WRITE FREEDOM-INT-RECORD    FROM VOUCH-ADDR-1099-RECORD

           MOVE WS-TRANSACTION-RECORD     TO TRANSACTION-RECORD
           MOVE WS-CHECK-DES-RECORD       TO CHECK-DES-RECORD
           MOVE WS-DISTRIBUTION-RECORD    TO DISTRIBUTION-RECORD
           MOVE WS-PAYEE-ADDRESS-RECORD   TO PAYEE-ADDRESS-RECORD
           MOVE WS-ALPHA-1099-RECORD      TO ALPHA-1099-RECORD
           MOVE WS-VOUCH-ADDR-1099-RECORD TO VOUCH-ADDR-1099-RECORD

           ADD +1                      TO CT-CURR-TOT-PMTS-CNT
062017                                    ST-CURR-TOT-PMTS-CNT
           ADD WS-INT-AMT              TO CT-CURR-TOT-PMTS-AMT
062017                                    ST-CURR-TOT-PMTS-AMT

           IF WS-LF-INT-AMT NOT = ZEROS
              ADD +1                   TO CT-CURR-LF-PMTS-CNT
062017                                    ST-CURR-LF-PMTS-CNT
              ADD WS-LF-INT-AMT        TO CT-CURR-LF-PMTS-AMT
062017                                    ST-CURR-LF-PMTS-AMT
           END-IF

           IF WS-AH-INT-AMT NOT = ZEROS
              ADD +1                   TO CT-CURR-AH-PMTS-CNT
062017                                    ST-CURR-AH-PMTS-CNT
              ADD WS-AH-INT-AMT        TO CT-CURR-AH-PMTS-AMT
062017                                    ST-CURR-AH-PMTS-AMT
           END-IF

           PERFORM 0200-CREATE-CORRESPONDENCE
                                       THRU 0200-EXIT

      *    IF (WS-PMT-AMT < +1.00)
      *       AND (WS-PMT-AMT NOT = ZEROS)
      *       MOVE ' ** BELOW MINIMUM ** '
      *                                TO WS-D2-PAYEE-ADDR2
      *       MOVE ' PMT VOIDED  '     TO WS-D2-SSN-COMMENT
      *       ADD +1                   TO CT-CURR-LF-PMTS-CNT-BM
      *                                   ST-CURR-LF-PMTS-CNT-BM
      *       ADD WS-PMT-AMT           TO CT-CURR-LF-PMTS-AMT-BM
      *                                   ST-CURR-LF-PMTS-AMT-BM
      *       PERFORM 3610-VOID-BELOW-MINIMUM
      *                                THRU 3610-EXIT
      *       PERFORM 3620-BUILD-NOTE-TRLR
      *                                THRU 3620-EXIT
      *    END-IF

      *    MOVE +0                     TO WS-INT-AMT WS-REF-AMT

           .
       0140-EXIT.
           EXIT.

       0150-PROCESS-ERMAIL.

           MOVE PB-CONTROL-BY-ACCOUNT (1:33)
                                       TO MA-CONTROL-PRIMARY

           READ ERMAIL
           IF ERMAIL-FILE-STATUS = '10' OR '23'
              DISPLAY ' ERMAIL  RECORD NOT FOUND ' PB-STATE ' '
                 PB-ACCOUNT ' ' PB-CERT-NO
              GO TO 0150-EXIT
           ELSE
              IF ERMAIL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERMAIL - READ ' ERMAIL-FILE-STATUS
                 ' ' PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
                 PERFORM ABEND-PGM
              END-IF
           END-IF

103111     MOVE MA-INSURED-LAST-NAME   TO WS-SV-INS-LNAME
103111     MOVE MA-INSURED-FIRST-NAME  TO WS-SV-INS-FNAME
103111     MOVE MA-INSURED-MIDDLE-INIT TO WS-SV-INS-MIDINIT
103111     MOVE MA-ADDRESS-LINE-1      TO WS-SV-INS-ADDR1
103111     MOVE MA-ADDRESS-LINE-2      TO WS-SV-INS-ADDR2
103111     MOVE MA-CITY                TO WS-SV-INS-CITY
103111     MOVE MA-ADDR-STATE          TO WS-SV-INS-STATE
103111     MOVE MA-ZIP                 TO WS-SV-INS-ZIP


103111     MOVE MA-CRED-BENE-NAME      TO WS-SV-BENE-NAME
062017     move ma-cred-bene-addr      to ws-sv-bene-addr1
062017     move ma-cred-bene-addr2     to ws-sv-bene-addr2
062017     move ma-cred-bene-city      to ws-sv-bene-city
062017     move ma-cred-bene-state     to ws-sv-bene-state
062017     move ma-cred-bene-zip       to ws-sv-bene-zip

062017     if pb-state <> 'NH'
062017        go to 0150-payee-is-cred-bene
062017     end-if

           MOVE MA-ADDRESS-LINE-1      TO PR-ADDRESS (1)
                                          VR-VENDOR-ADDRESS (1)
           MOVE MA-ADDRESS-LINE-2      TO PR-ADDRESS (2)
051810     MOVE SPACES                 TO PR-ADDRESS (3)
051810     STRING MA-CITY ' ' MA-ADDR-STATE ' ' MA-ZIP
051810        DELIMITED BY '  ' INTO PR-ADDRESS (3)
051810     END-STRING

           MOVE SPACES                 TO PR-ADDRESS (4)
051810                                    VR-VENDOR-ADDRESS (2)
111912     MOVE MA-CITY                TO VR-VENDOR-ADDRESS (2) (1:29)
111912     MOVE MA-ADDR-STATE          TO VR-VENDOR-ADDRESS (2) (30:2)
111912     MOVE MA-ZIP                 TO VR-VENDOR-ADDRESS (2) (32:9)

           STRING
              MA-INSURED-FIRST-NAME ' '
              MA-INSURED-LAST-NAME DELIMITED BY '  '
              INTO PR-PAYEE-NAME
           END-STRING
           MOVE PR-PAYEE-NAME          TO VR-VENDOR-NAME (1)

           IF PR-ADDRESS (2) = SPACES
              MOVE PR-ADDRESS (3)      TO PR-ADDRESS (2)
              MOVE SPACES              TO PR-ADDRESS (3)
           END-IF

062017     IF ELCERT-FOUND
              display ' ssn ' cm-cert-no ' ' cm-soc-sec-no
062017        IF (CM-SOC-SEC-NO (4:1) = '-')
062017           AND (CM-SOC-SEC-NO (7:1) = '-')
062017           DISPLAY ' SOC SEC FIX BEFORE ' CM-SOC-SEC-NO
062017           MOVE CM-SOC-SEC-NO (5:2)
062017                                 TO CM-SOC-SEC-NO (4:2)
062017           MOVE CM-SOC-SEC-NO (8:4)
062017                                 TO CM-SOC-SEC-NO (6:4)
062017           MOVE ZEROS            TO CM-SOC-SEC-NO (10:2)
062017           DISPLAY ' SOC SEC FIX AFTER ' CM-SOC-SEC-NO
062017        END-IF
062017        IF CM-SOC-SEC-NO (1:9) NOT NUMERIC
062017           MOVE ZEROS            TO CM-SOC-SEC-NO
062017        END-IF
062017           
062017        MOVE CM-SOC-SEC-NO (1:9) TO AR-TAX-ID-NO
062017                                    DR-SUSPENSE (1) (2:9)
      *       display ' tax id ' cm-cert-no ' ' ar-tax-id-no
062017     END-IF

      *    IF (MA-INSURED-SOC-SEC-NO (4:1) = '-')
      *       AND (MA-INSURED-SOC-SEC-NO (7:1) = '-')
      *       DISPLAY ' SOC SEC FIX BEFORE ' MA-INSURED-SOC-SEC-NO
      *       MOVE MA-INSURED-SOC-SEC-NO (5:2)
      *                                TO MA-INSURED-SOC-SEC-NO (4:2)
      *       MOVE MA-INSURED-SOC-SEC-NO (8:4)
      *                                TO MA-INSURED-SOC-SEC-NO (6:4)
      *       MOVE ZEROS               TO MA-INSURED-SOC-SEC-NO (10:2)
      *       DISPLAY ' SOC SEC FIX AFTER ' MA-INSURED-SOC-SEC-NO
      *    END-IF
      *    IF MA-INSURED-SOC-SEC-NO (1:9) NOT NUMERIC
      *       MOVE ZEROS               TO MA-INSURED-SOC-SEC-NO
      *    END-IF
      *       
      *    MOVE MA-INSURED-SOC-SEC-NO (1:9)
      *                                TO AR-TAX-ID-NO
      *                                   DR-SUSPENSE (1) (2:9)
062017     go to 0150-exit

           .
062017 0150-payee-is-cred-bene.
062017
062017     move ma-cred-bene-name      to pr-payee-name
062017     MOVE MA-cred-bene-addr      TO PR-ADDRESS (1)
062017     MOVE MA-cred-bene-addr2     TO PR-ADDRESS (2)
062017     MOVE SPACES                 TO PR-ADDRESS (3)
062017     STRING
062017        MA-cred-bene-CITY ' '
062017        MA-cred-bene-STATE ' '
062017        MA-cred-bene-zip
062017        DELIMITED BY '  ' INTO PR-ADDRESS (3)
062017     END-STRING
062017
062017     MOVE SPACES                 TO PR-ADDRESS (4)
062017
062017     IF PR-ADDRESS (2) = SPACES
062017        MOVE PR-ADDRESS (3)      TO PR-ADDRESS (2)
062017        MOVE SPACES              TO PR-ADDRESS (3)
062017     END-IF
062017
062017     move zeros                  to ar-tax-id-no
062017                                    dr-suspense (1) (2:9)
062017
062017****   Build 1099 name and address   ****
062017
062017     STRING
062017        MA-INSURED-FIRST-NAME ' '
062017        MA-INSURED-LAST-NAME DELIMITED BY '  '
062017        INTO VR-VENDOR-NAME (1)
062017     END-STRING
062017
062017     MOVE MA-ADDRESS-LINE-1      TO VR-VENDOR-ADDRESS (1)
062017
062017     MOVE SPACES                 TO VR-VENDOR-ADDRESS (2)
062017     MOVE MA-CITY                TO VR-VENDOR-ADDRESS (2) (1:29)
062017     MOVE MA-ADDR-STATE          TO VR-VENDOR-ADDRESS (2) (30:2)
062017     MOVE MA-ZIP                 TO VR-VENDOR-ADDRESS (2) (32:9)

           .
       0150-EXIT.
           EXIT.

111109 0160-PROCESS-ERCNOT.

           MOVE WS-INT-AMT             TO WS-ERNOTE-AMT
           MOVE SPACES                 TO WS-CERT-NOTE-LINE
                                          ws-temp-note

062017     move ws-ref-amt             to ws-display-ref-amt
062017     move ws-int-amt             to ws-display-int-amt
062017     move ws-int-rate            to ws-display-int-rate
062017
062017     evaluate true
062017        when ws-int-amt < 1.00
062017           STRING
062017              pb-state ' '
062017              'INTEREST ON CANCELLATION LESS THAN MINIMUM '
062017              WS-CURRENT-DATE ' '
062017              WS-ERNOTE-AMT DELIMITED BY '     '
062017              INTO WS-CERT-NOTE-LINE
062017           END-STRING
062017        when pb-state = 'NH'
062017          STRING
062017           WS-CURRENT-DATE ' '
062017           pb-state        ' '
062017           'INT ON RFD, THRU ' WS-DT-PLUS-15 ' AMT '
062017           WS-ERNOTE-AMT ' TO ' PR-PAYEE-NAME DELIMITED BY '     '
062017           INTO WS-CERT-NOTE-LINE
062017          END-STRING
062017        when other
062017           string
062017              'INT OF '
062017              ws-display-int-amt
062017*             ' PD TO CRED BENE CALC ON REFD OF '
062017              ' PD TO CR BEN CALC ON ORIG PREM OF'
062017              ws-display-ref-amt
062017              ' @ '
062017              ws-display-int-rate
062017              '%' delimited by size into ws-temp-note
062017           end-string
062017           move +1               to s1
062017           perform varying n1 from +1 by +1 until
062017              n1 > +90
062017              if ws-temp-note (n1:2) = '  '
062017                 continue
062017              else
062017                 move ws-temp-note (n1:1)
062017                                 to ws-cert-note-line (s1:1)
062017                 add +1 to s1
062017              end-if
062017           end-perform
062017     end-evaluate

111109     MOVE SPACES                 TO CERT-NOTE-FILE
111109     MOVE 'CZ'                   TO CZ-RECORD-ID
111109     MOVE PB-CONTROL-BY-ACCOUNT (1:33)
111109                                 TO CZ-CONTROL-PRIMARY
111109     MOVE '1'                    TO CZ-RECORD-TYPE
111109     MOVE +1                     TO CZ-NOTE-SEQUENCE
111109     MOVE WS-CURRENT-BIN-DT      TO CZ-LAST-MAINT-DT
111109     MOVE +190000                TO CZ-LAST-MAINT-HHMMSS
111109     MOVE 'E570'                 TO CZ-LAST-MAINT-USER
111109     MOVE WS-CERT-NOTE-LINE      TO CZ-NOTE
111109     MOVE '22'                   TO ERCNOT-FILE-STATUS
111109     PERFORM 0170-WRITE-CERT-NOTE THRU 0170-EXIT
111109            UNTIL ERCNOT-FILE-STATUS NOT EQUAL '22'
111109
111109     IF ERCNOT-FILE-STATUS = '00'
              CONTINUE
           ELSE
111109        DISPLAY ' ERROR - ERCNOT - WRITE ' ERCNOT-FILE-STATUS
              ' ' PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
           END-IF

           .
       0160-EXIT.
           EXIT.

111109 0170-WRITE-CERT-NOTE.
111109 
111109      WRITE CERT-NOTE-FILE
111109      
111109      IF ERCNOT-FILE-STATUS = '22'
111109          ADD +1             TO CZ-NOTE-SEQUENCE
111109      END-IF
111109
111109      .
111109 0170-EXIT.
111109     EXIT.

       0200-CREATE-CORRESPONDENCE.

062017     if pb-state <> 'NH'
062017        go to 0200-exit
062017     end-if

           MOVE 'LA'                   TO LETTER-ARCHIVE

           MOVE DTE-CLASIC-COMPANY-CD  TO LA-COMPANY-CD
                                          LA-COMPANY-CD-A2
                                          LA-COMPANY-CD-A3
                                          LA-COMPANY-CD-A4
                                          LA-COMPANY-CD-A5
                                          LA-COMPANY-CD-A6

           MOVE PB-CARRIER             TO LA-CARRIER-A2
                                          LA-CARRIER-A3
                                          LA-CARRIER-A4
                                          LA-CARRIER-A5

           MOVE PB-GROUPING            TO LA-GROUPING-A2
                                          LA-GROUPING-A3
                                          LA-GROUPING-A4
                                          LA-GROUPING-A5

           MOVE PB-STATE               TO LA-STATE-A2
                                          LA-STATE-A3
                                          LA-STATE-A4
                                          LA-STATE-A5

           MOVE PB-ACCOUNT             TO LA-ACCOUNT-A2
                                          LA-ACCOUNT-A3
                                          LA-ACCOUNT-A4
                                          LA-ACCOUNT-A5

           MOVE PB-CERT-NO             TO LA-CERT-NO-A2
           MOVE PB-CERT-EFF-DT         TO LA-EFFECT-DATE-A2
           MOVE PB-LAST-MAINT-BY       TO LA-PROCESSOR-CD
           MOVE 'NH01'                 TO LA-FORM-A3
           MOVE PB-ENTRY-BATCH         TO LA-ENTRY-A6

           ADD +1                      TO WS-ARCH-NUM

           MOVE WS-ARCH-NUM            TO LA-ARCHIVE-NO
                                          LA-ARCHIVE-NO-A2
                                          LA-ARCHIVE-NO-A3
                                          LA-ARCHIVE-NO-A4
                                          LA-ARCHIVE-NO-A5
                                          LA-ARCHIVE-NO-A6

           MOVE WS-CURRENT-BIN-DT      TO LA-CREATION-DATE

           MOVE ZEROS                  TO LA-NUMBER-LABEL-LINES
                                          LA-NO-OF-COPIES
                                          LA-NO-OF-TEXT-RECORDS

           MOVE LOW-VALUES             TO LA-FOLLOW-UP-DATE
                                          LA-INITIAL-PRINT-DATE
                                          LA-REPLY-DATE
                                          LA-RESEND-DATES
                                          LA-LAST-RESENT-PRINT-DATE
                                          LA-PURGED-DATE
                                          LA-VOIDED-DATE
           MOVE '2'                    TO LA-DATA-SOURCE
103111     MOVE 'A'                    TO LA-STATUS

           WRITE LETTER-ARCHIVE

103111     PERFORM 0300-CREATE-NAPER-EXT THRU 0300-EXIT

           .
       0200-EXIT.
           EXIT.

       0210-GET-ARCHNO.

           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '1'                    TO CF-RECORD-TYPE
           MOVE SPACES                 TO CF-ACCESS-CD-GENL
           MOVE ZEROS                  TO CF-SEQUENCE-NO
           
           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '00'
              MOVE CF-CREDIT-LAST-ARCH-NUM
                                       TO WS-ARCH-NUM
              IF CF-CREDIT-REF-SSN-CNT NOT NUMERIC
                 MOVE +0               TO CF-CREDIT-REF-SSN-CNT
              END-IF
              MOVE CF-CREDIT-REF-SSN-CNT
                                       TO WS-SSN-SEQ-NO
           ELSE
              DISPLAY ' ERROR - ELCNTL - READ ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0210-EXIT.
           EXIT.

       0220-UPDATE-ARCHNO.

           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '1'                    TO CF-RECORD-TYPE
           MOVE SPACES                 TO CF-ACCESS-CD-GENL
           MOVE ZEROS                  TO CF-SEQUENCE-NO

           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '00'
              MOVE WS-ARCH-NUM         TO CF-CREDIT-LAST-ARCH-NUM
              MOVE WS-SSN-SEQ-NO       TO CF-CREDIT-REF-SSN-CNT
              REWRITE CONTROL-FILE
           ELSE
              DISPLAY ' ERROR - ELCNTL - READU ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0220-EXIT.
           EXIT.
072312
062017 0230-build-ref-int-table.
062017
062017     MOVE DTE-CLIENT             TO CF-COMPANY-ID
062017     MOVE '3'                    TO CF-RECORD-TYPE
062017     MOVE low-values             TO CF-ACCESS-of-state
062017     MOVE +0                     TO CF-SEQUENCE-NO
062017
062017     start ELCNTL key >= cf-control-primary
062017     if elcntl-file-status not = '00'
062017        display 'ERROR-ELCNTL-START ' ELCNTL-FILE-STATUS
062017        PERFORM ABEND-PGM
062017     END-IF
062017
062017     read elcntl next record
062017     move +1                     to i1
062017
062017     perform until
062017        (elcntl-file-status <> '00')
062017        or (cf-record-type <> '3')
062017        if cf-st-calc-interest not numeric
062017           move +0               to cf-st-calc-interest
062017        end-if
062017*       if cf-st-calc-interest = zeros
062017*          move 0.0800           to cf-st-calc-interest
062017*       end-if
062017        if cf-st-calc-interest <> zeros
062017           move cf-state-abbreviation
062017                                 to ws-state-abbr (i1)
062017           move cf-st-calc-interest
062017                                 to ws-state-ref-int-rate (i1)
062017           display ' found state with int ' cf-state-code ' '
062017              cf-st-calc-interest
062017           add +1 to i1
062017        end-if
062017        read elcntl next record
062017     end-perform
062017
062017     display ' load of ref int table complete ' i1
062017
062017     .
062017 0230-EXIT.
062017     EXIT.

072312 0250-GET-ENCLOSURE.
072312
072312     MOVE LOW-VALUES             TO TX-CONTROL-PRIMARY.
072312     MOVE DTE-CLASIC-COMPANY-CD  TO TX-COMPANY-CD.
072312     MOVE 'NH01'                 TO TX-LETTER-NO.
072312     
072312     START ELLETR KEY IS NOT LESS THAN TX-CONTROL-PRIMARY.
072312
072312     IF ELLETR-FILE-STATUS NOT = '00'
072312        DISPLAY ' ELLETR START ERROR ' 
072312        GO TO 0250-EXIT
072312     END-IF.
072312     
072312 0250-READ-NEXT.
072312 
072312     READ ELLETR NEXT RECORD.
072312
072312     IF ELLETR-FILE-STATUS NOT = '00'
072312        DISPLAY ' ELLETR READ ERROR ' 
072312        GO TO 0250-EXIT
072312     END-IF.
072312
072312     IF (TX-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD) OR
072312        (TX-LETTER-NO NOT = 'NH01')
072312           DISPLAY 'LETTER Z RECORD NOT FOUND  - NH01' 
072312           GO TO 0250-EXIT
072312     END-IF.
072312
072312     IF TX-LINE-SQUEEZE-CONTROL NOT EQUAL 'Z'
072312         GO TO 0250-READ-NEXT
072312     END-IF.
072312     
072312     INITIALIZE W-Z-CONTROL-DATA
072312
072312     MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA
072312
072312     MOVE LOW-VALUES             TO NC-CONTROL-PRIMARY
072312     MOVE DTE-CLASIC-COMPANY-CD  TO NC-COMPANY-CD
072312     MOVE '2'                    TO NC-REC-TYPE
072312     MOVE W-ENCLOSURE-CD         TO NC-ENC-CODE
072312
072312     START ELENCC KEY IS NOT LESS THAN NC-CONTROL-PRIMARY.
072312
072312     IF ELENCC-FILE-STATUS NOT = '00'
072312        DISPLAY ' ELENCC START ERROR ' 
072312        GO TO 0250-EXIT
072312     END-IF.
072312            
072312     READ ELENCC NEXT RECORD.
072312
072312     IF ELENCC-FILE-STATUS NOT = '00'
072312        DISPLAY ' ELENCC READ ERROR ' 
072312        GO TO 0250-EXIT
072312     END-IF.
072312     
072312     IF (NC-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD) OR
072312        (NC-ENC-CODE NOT = W-ENCLOSURE-CD)
072312           DISPLAY 'ENCLOSURE NOT FOUND - ' W-ENCLOSURE-CD 
072312           GO TO 0250-EXIT
072312     END-IF.
072312     
072312     MOVE NC-OUTPUT-STACK     TO WS-NC-OUT-STACK.
072312     MOVE NC-ENCLOSURE-LINE   TO WS-NC-OUT-ENC-LINE.
072312     MOVE NC-ATTACHMENTS      TO WS-NC-OUT-ATTACH.
072312
072312 0250-EXIT.
072312     EXIT.
103111
103111 0300-CREATE-NAPER-EXT.
103111
103111     MOVE PB-CSR-ID              TO OUT-CSR-CODE
103111                                    OUT-CSR-TITLE
071212                                    OUT-UNDW-ID
103111     MOVE PB-LAST-MAINT-BY       TO OUT-PROC-ID
103111                                    OUT-PROC-TITLE
103111
103111     MOVE DTE-CLIENT             TO CF-COMPANY-ID
103111     MOVE '2'                    TO CF-RECORD-TYPE
103111     MOVE PB-CSR-ID              TO CF-ACCESS-CD-GENL
103111     MOVE ZEROS                  TO CF-SEQUENCE-NO
103111
103111     READ ELCNTL
103111     IF ELCNTL-FILE-STATUS = '00'
103111        MOVE CF-PROCESSOR-NAME   TO OUT-CSR-NAME
103111                                    OUT-PROC-NAME
103111     END-IF
103111
103111     IF PB-LAST-MAINT-BY NOT = PB-CSR-ID
103111        MOVE DTE-CLIENT          TO CF-COMPANY-ID
103111        MOVE '2'                 TO CF-RECORD-TYPE
103111        MOVE PB-LAST-MAINT-BY    TO CF-ACCESS-CD-GENL
103111        MOVE ZEROS               TO CF-SEQUENCE-NO
103111
103111        READ ELCNTL
103111        IF ELCNTL-FILE-STATUS = '00'
103111           MOVE CF-PROCESSOR-NAME   TO OUT-PROC-NAME
103111        END-IF
103111     END-IF
103111
103111     
103111     MOVE 'NH01'                 TO OUT-LETTER
103111     MOVE PB-CARRIER             TO OUT-CARRIER
103111     MOVE PB-GROUPING            TO OUT-GROUPING
103111     MOVE PB-STATE               TO OUT-STATE
103111     MOVE PB-ACCOUNT             TO OUT-ACCOUNT
103111     MOVE PB-CERT-NO             TO OUT-CERT-NO
103111     MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1
103111     MOVE SPACES                 TO DC-OPTION-CODE
103111     PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT
103111     IF NO-CONVERSION-ERROR
103111        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
103111           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
103111              INTO OUT-CERT-EFF-DT
103111        END-STRING
103111     ELSE
103111        MOVE SPACES              TO OUT-CERT-EFF-DT
103111     END-IF
103111              
103111     MOVE PB-C-LF-CANCEL-DT      TO DC-BIN-DATE-1
103111     IF PB-C-AH-CANCEL-DT > DC-BIN-DATE-1
103111        MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1
103111     END-IF
103111     PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT
103111     IF NO-CONVERSION-ERROR
103111        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
103111           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
103111              INTO OUT-CANCEL-DATE
103111        END-STRING
103111     ELSE
103111        MOVE SPACES              TO OUT-CANCEL-DATE
103111     END-IF
103111
103111     MOVE WS-SV-INS-LNAME        TO OUT-ILNAME
103111     MOVE WS-SV-INS-FNAME        TO OUT-IFNAME
103111     MOVE WS-SV-INS-MIDINIT      TO OUT-IMINIT
103111     MOVE WS-SV-INS-ADDR1        TO OUT-INS-ADDR1
103111     MOVE WS-SV-INS-ADDR2        TO OUT-INS-ADDR2
103111     MOVE WS-SV-INS-CITY         TO OUT-INS-CITY
103111     MOVE WS-SV-INS-STATE        TO OUT-INS-STATE
103111     MOVE WS-SV-INS-ZIP          TO OUT-INS-ZIP
103111     MOVE WS-SV-ACCT-NAME        TO OUT-ACCT-NAME
103111     MOVE WS-SV-BENE-NAME        TO OUT-BENE-NAME
062017     move ws-sv-bene-addr1       to out-bene-addr1
062017     move ws-sv-bene-addr2       to out-bene-addr2
062017     move ws-sv-bene-city        to out-bene-city
062017     move ws-sv-bene-state       to out-bene-state
062017     move ws-sv-bene-zip         to out-bene-zip

072312     MOVE WS-NC-OUT-STACK        TO OUT-STACK
072312     MOVE WS-NC-OUT-ENC-LINE     TO OUT-ENC-LINE
072312     MOVE WS-NC-OUT-ATTACH       TO OUT-ATTACH
103111     MOVE WS-INT-AMT             TO OUT-TOT-INTEREST
103111
033114     MOVE PB-CONTROL-BY-ACCOUNT (1:33)
033114                                 TO CM-CONTROL-PRIMARY
033114
062017     if not elcert-found
062017        go to 0300-cont
062017     end-if

033114     MOVE CM-LF-CURRENT-STATUS   TO OUT-LF-CUR-STATUS.
033114     MOVE CM-AH-CURRENT-STATUS   TO OUT-AH-CUR-STATUS.
033114
033114     IF CM-LF-CURRENT-STATUS = '7'
033114        MOVE WS-DEATH-STATUS     TO PRT
033114        PERFORM WRITE-A-LINE
033114     END-IF.
033114
033114 0300-CONT.
090612     MOVE LOW-VALUES             TO NSAS-LETTER-VARIABLES
103111     MOVE DTE-CLASIC-COMPANY-CD  TO NSAS-COMPANY-CD
103111     MOVE WS-ARCH-NUM            TO NSAS-ARCHIVE-NO
072312                                    OUT-ARCH-NO
103111     MOVE +0                     TO NSAS-SEQ-NO
103111 
103111     STRING
103111         OUT-LETTER             ' ~'
103111         OUT-PROC-ID            '~'
103111         OUT-PROC-NAME          '~'
103111         OUT-PROC-TITLE         '~' 
103111         OUT-CSR-NAME           '~' 
103111         OUT-CSR-TITLE          '~' 
103111         OUT-CARRIER            '~' 
103111         OUT-GROUPING           '~' 
103111         OUT-STATE              '~' 
103111         OUT-ACCOUNT            '~' 
103111         OUT-CERT-EFF-DT        '~' 
103111         OUT-CERT-NO            '~' 
103111         OUT-CERT-SFX           '~' 
103111         OUT-ILNAME             '~' 
103111         OUT-IFNAME             '~' 
103111         OUT-IFINIT             '~' 
103111         OUT-IMINIT             '~' 
103111         OUT-IAGE               '~' 
103111         OUT-ISEX               '~' 
103111         OUT-INS-ADDR1          '~' 
103111         OUT-INS-ADDR2          '~' 
103111         OUT-INS-CITY           '~' 
103111         OUT-INS-STATE          '~' 
103111         OUT-INS-ZIP            '~' 
103111         OUT-SOC-SEC-NO         '~' 
103111         OUT-MEMBER-NO          '~' 
103111         OUT-JLNAME             '~' 
103111         OUT-JFNAME             '~' 
103111         OUT-JMINIT             '~' 
103111         OUT-JAGE               '~' 
103111         OUT-ACCT-NAME          '~' 
103111         OUT-ACCT-ADDR1         '~' 
103111         OUT-ACCT-ADDR2         '~' 
103111         OUT-ACCT-CITY          '~' 
103111         OUT-ACCT-STATE         '~' 
103111         OUT-ACCT-ZIP           '~' 
103111         OUT-ACCT-PHONE         '~' 
103111         OUT-ACCT-CNTRL-NAME    '~' 
103111         OUT-ACCT-BUS-TYPE      '~' 
103111         OUT-BENE-NAME          '~' 
103111         OUT-BENE-ADDR1         '~' 
103111         OUT-BENE-ADDR2         '~' 
103111         OUT-BENE-CITY          '~' 
103111         OUT-BENE-STATE         '~' 
103111         OUT-BENE-ZIP           '~' 
103111         OUT-CARR-NAME          '~' 
103111         OUT-RESP-NO            '~' 
103111         OUT-COMP-NAME          '~' 
103111         OUT-COMP-MAIL-TO       '~' 
103111         OUT-COMP-ADDR1         '~' 
103111         OUT-COMP-ADDR2         '~' 
103111         OUT-COMP-CITY          '~' 
103111         OUT-COMP-STATE         '~' 
103111         OUT-COMP-ZIP           '~' 
103111         OUT-COMP-PHONE         '~' 
103111         OUT-COMP-FAX           '~' 
103111         OUT-COMP-STATUS        '~' 
103111         OUT-BILL-SW            '~' 
103111         OUT-RPT-CD1            '~' 
103111         OUT-RPT-CD2            '~' 
103111         OUT-ENTRY-DT           '~' 
103111         OUT-ENTRY-BATCH        '~' 
103111         OUT-ENTRY-STATUS       '~' 
103111         OUT-1ST-PMT-DT         '~' 
103111         OUT-LOAN-APR           '~' 
103111         OUT-LOAN-TERM          '~' 
103111         OUT-RATE-CLASS         '~' 
103111         OUT-EXT-DAYS           '~' 
103111         OUT-CSR-CODE           '~' 
103111         OUT-UCODE              '~' 
103111         OUT-PREM-TYPE          '~' 
103111         OUT-IND-GRP            '~' 
103111         OUT-SKIP-CD            '~' 
103111         OUT-PMT-MODE           '~' 
103111         OUT-LOAN-OFF           '~' 
103111         OUT-REIN-TABLE         '~' 
103111         OUT-SPEC-REIN          '~' 
103111         OUT-LF-BENCD           '~' 
103111         OUT-LF-TERM            '~' 
103111         OUT-LF-DEV-CD          '~' 
103111         OUT-LF-DEV-PCT         '~' 
103111         OUT-LF-BEN             '~' 
103111         OUT-LF-PRM             '~' 
103111         OUT-LF-ALT-BEN         '~' 
103111         OUT-LF-ALT-PRM         '~' 
103111         OUT-LF-NSP             '~' 
103111         OUT-LF-REM-BEN         '~' 
103111         OUT-LF-REF             '~' 
103111         OUT-LF-DTH             '~' 
103111         OUT-LF-RATE            '~' 
103111         OUT-LF-ALT-RATE        '~' 
103111         OUT-LF-EXP-DT          '~' 
103111         OUT-LF-CUR-STATUS      '~' 
103111         OUT-LF-CAN-DT          '~' 
103111         OUT-LF-CAN-EXIT-DT     '~' 
103111         OUT-LF-DTH-DT          '~' 
103111         OUT-LF-DTH-EXIT-DT     '~' 
103111         OUT-LF-EXIT-BATCH      '~' 
103111         OUT-LF-COMM-PCT        '~' 
103111         OUT-LF-DESC            '~' 
103111         OUT-LF-ABBRV           '~' 
103111         OUT-AH-BENCD           '~' 
103111         OUT-AH-TERM            '~' 
103111         OUT-CRIT-PER           '~' 
103111         OUT-AH-DEV-CD          '~' 
103111         OUT-AH-DEV-PCT         '~' 
103111         OUT-AH-BEN             '~' 
103111         OUT-AH-PRM             '~' 
103111         OUT-AH-NSP             '~' 
103111         OUT-AH-REF             '~' 
103111         OUT-AH-CLM             '~' 
103111         OUT-AH-TOT-BEN         '~' 
103111         OUT-AH-PDTHRU-DT       '~' 
103111         OUT-AH-RATE            '~' 
103111         OUT-AH-EXP-DT          '~' 
103111         OUT-AH-CUR-STATUS      '~' 
103111         OUT-AH-CAN-DT          '~' 
103111         OUT-AH-CAN-EXIT-DT     '~' 
103111         OUT-AH-EXIT-BATCH      '~' 
103111         OUT-AH-COMM-PCT        '~' 
103111         OUT-AH-DESC            '~' 
103111         OUT-RET-ELIM           '~' 
103111         OUT-BEN-DAYS           '~' 
103111         OUT-WAIT-PER           '~' 
103111         OUT-MAX-PMTS           '~' 
103111         OUT-TOT-PRM            '~' 
103111         OUT-TOT-REF            '~' 
103111         OUT-SCHED-EXP-DT       '~' 
103111         OUT-SCHED-TERM         '~' 
103111         OUT-ORIG-ILNAME        '~' 
103111         OUT-ORIG-IFNAME        '~' 
103111         OUT-ORIG-MINIT         '~' 
103111         OUT-ORIG-IAGE          '~' 
103111         OUT-ORIG-JLNAME        '~' 
103111         OUT-ORIG-JFNAME        '~' 
103111         OUT-ORIG-JMINIT        '~' 
103111         OUT-ORIG-JAGE          '~' 
103111         OUT-ORIG-LF-BENCD      '~' 
103111         OUT-ORIG-LF-TERM       '~' 
103111         OUT-ORIG-LF-BEN        '~' 
103111         OUT-ORIG-LF-PRM        '~' 
103111         OUT-LF-CALC-PRM        '~' 
103111         OUT-ORIG-LF-REF        '~' 
103111         OUT-LF-CALC-REF        '~' 
103111         OUT-ORIG-LF-ALT-BEN    '~' 
103111         OUT-ORIG-LF-ALT-PRM    '~' 
103111         OUT-ORIG-LF-EXP-DT     '~' 
103111         OUT-ORIG-LF-DESC       '~' 
103111         OUT-ORIG-AH-BENCD      '~' 
103111         OUT-ORIG-AH-TERM       '~' 
103111         OUT-ORIG-CRIT-PER      '~' 
103111         OUT-ORIG-AH-BEN        '~' 
103111         OUT-ORIG-AH-PRM        '~' 
103111         OUT-AH-CALC-PRM        '~' 
103111         OUT-ORIG-AH-REF        '~' 
103111         OUT-AH-CALC-REF        '~' 
103111         OUT-ORIG-AH-EXP-DT     '~' 
103111         OUT-ORIG-AH-DESC       '~' 
103111         OUT-ORIG-RET-ELIM      '~' 
103111         OUT-ORIG-BEN-DAYS      '~' 
103111         OUT-ORIG-WAIT-PER      '~' 
103111         OUT-ORIG-MAX-PMTS      '~' 
103111         OUT-ORIG-SCHED-EXP-DT  '~' 
103111         OUT-ORIG-SCHED-TERM    '~' 
103111         OUT-NEW-ILNAME         '~' 
103111         OUT-NEW-IFNAME         '~' 
103111         OUT-NEW-IMINIT         '~' 
103111         OUT-NEW-IAGE           '~' 
103111         OUT-NEW-JLNAME         '~' 
103111         OUT-NEW-JFNAME         '~' 
103111         OUT-NEW-JMINIT         '~' 
103111         OUT-NEW-JAGE           '~' 
103111         OUT-NEW-LF-BENCD       '~' 
103111         OUT-NEW-LF-TERM        '~' 
103111         OUT-NEW-LF-BEN         '~' 
103111         OUT-NEW-LF-PRM         '~' 
103111         OUT-NEW-LF-REF         '~' 
103111         OUT-NEW-LF-ALT-BEN     '~' 
103111         OUT-NEW-LF-ALT-PRM     '~' 
103111         OUT-NEW-LF-EXP-DT      '~' 
103111         OUT-NEW-LF-DESC        '~' 
103111         OUT-NEW-AH-BENCD       '~' 
103111         OUT-NEW-AH-TERM        '~' 
103111         OUT-NEW-CRIT-PER       '~' 
103111         OUT-NEW-AH-BEN         '~' 
103111         OUT-NEW-AH-PRM         '~' 
103111         OUT-NEW-AH-REF         '~' 
103111         OUT-NEW-AH-EXP-DT      '~' 
103111         OUT-NEW-AH-DESC        '~' 
103111         OUT-NEW-RET-ELIM       '~' 
103111         OUT-NEW-BEN-DAYS       '~' 
103111         OUT-NEW-WAIT-PER       '~'
103111         OUT-NEW-MAX-PMTS       '~'
103111         OUT-NEW-SCHED-EXP-DT   '~'
103111         OUT-NEW-SCHED-TERM     '~'
103111         OUT-TOT-PRM-CHG        '~'
103111         OUT-TOT-REF-CHG        '~'
103111         OUT-PAYEE              '~'
103111         OUT-SIG-SW             '~'
103111         OUT-BALLOON-IND        '~'
103111         OUT-LEASE-IND          '~'
103111         OUT-RESCIND            '~'
103111         OUT-REA-CD1            '~'
103111         OUT-REA-CD2            '~'
103111         OUT-REA-CD3            '~'
103111         OUT-REA-CD4            '~'
103111         OUT-REA-CD5            '~'
103111         OUT-REA-CD6            '~'
103111         OUT-REA-CD7            '~'
103111         OUT-REA-CD8            '~'
103111         OUT-REA-CD9            '~'
103111         OUT-REA-CD10           '~'
103111         OUT-REA-CD11           '~'
103111         OUT-REA-CD12           '~'
103111         OUT-CYCLE-DT           '~'
103111         OUT-ARCH-NO            '~'
103111         OUT-FORM               '~'
103111         OUT-ENC-LINE           '~'
103111         OUT-ATTACH             '~'
103111         OUT-STACK              '~'
103111         OUT-STATE-NAME         '~'
103111         OUT-PRINT-NOW          '~'
072312         OUT-CHGBACK            '~'
103111         OUT-CSO-PORTION        '~'
112612         OUT-ACCT-PORTION       '~'
103111         OUT-LETTER-TYPE        '~'
103111         OUT-PRINT-CERTIFICATE  '~'   
103111         OUT-INS-BIRTHDATE      '~'
103111         OUT-JNT-BIRTHDATE      '~'      
103111         OUT-TOT-INTEREST       '~'  
103111         OUT-ORIG-TOT-PREM      '~'
103111         OUT-ORIG-TOT-REF       '~' 
103111         OUT-CANCEL-DATE        '~'
103111         OUT-HEALTH-APP         '~'
103111         OUT-CERTIFICATE-ID     '~'  
071212         OUT-UNDW-ID            '~'
072312         OUT-NEW-TOT-PREM       '~'
072312         OUT-NEW-TOT-REF        '~' 
112612         OUT-COVERAGE-IND       '~'
112612         OUT-ORIG-COVERAGE-IND  '~'
112612         OUT-NEW-COVERAGE-IND   '~'
112612         OUT-LF-CMB-PREM        '~'
112612         OUT-ORIG-LF-CMB-PREM   '~'
112612         OUT-NEW-LF-CMB-PREM    '~'
112612         OUT-LF-PREM-CHG        '~'
112612         OUT-LF-ALT-PREM-CHG    '~'
112612         OUT-LF-CMB-PREM-CHG    '~'
112612         OUT-AH-PREM-CHG        '~'
112612         OUT-LF-REF-CHG         '~'
112612         OUT-AH-REF-CHG         '~'
112612         OUT-ACCT-LF-PORTION    '~'
112612         OUT-ACCT-ALT-LF-PORT   '~'
112612         OUT-ACCT-CMB-LF-PORT   '~'
112612         OUT-ACCT-AH-PORTION    '~'
112612         OUT-CSO-LF-PORTION     '~'
112612         OUT-CSO-ALT-LF-PORT    '~'
112612         OUT-CSO-CMB-LF-PORT    '~'
112612         OUT-CSO-AH-PORTION     '~'
112612         OUT-ACCT-ORIG-LF-PORT  '~'
112612         OUT-ACCT-ORIG-ALT-LF-PORT '~'
112612         OUT-ACCT-ORIG-CMB-LF-PORT '~'
112612         OUT-ACCT-ORIG-AH-PORT  '~'
112612         OUT-ACCT-ORIG-PORTION  '~'
112612         OUT-CSO-ORIG-LF-PORT   '~'
112612         OUT-CSO-ORIG-ALT-LF-PORT '~'
112612         OUT-CSO-ORIG-CMB-LF-PORT '~'
112612         OUT-CSO-ORIG-AH-PORT   '~'
112612         OUT-CSO-ORIG-PORTION   '~'
112612         OUT-ACCT-NEW-LF-PORT   '~'
112612         OUT-ACCT-NEW-ALT-LF-PORT '~'
112612         OUT-ACCT-NEW-CMB-LF-PORT '~'
112612         OUT-ACCT-NEW-AH-PORT   '~'
112612         OUT-ACCT-NEW-PORTION   '~'
112612         OUT-CSO-NEW-LF-PORT    '~'
112612         OUT-CSO-NEW-ALT-LF-PORT '~'
112612         OUT-CSO-NEW-CMB-LF-PORT '~'
112612         OUT-CSO-NEW-AH-PORT    '~'
112612         OUT-CSO-NEW-PORTION    '~'
112612         OUT-ACCT-LF-PORT-CHG   '~'
112612         OUT-ACCT-ALT-LF-PORT-CHG '~'
112612         OUT-ACCT-CMB-LF-PORT-CHG '~'
112612         OUT-ACCT-AH-PORT-CHG   '~'
112612         OUT-ACCT-PORTION-CHG   '~'
112612         OUT-CSO-LF-PORT-CHG    '~'
112612         OUT-CSO-ALT-LF-PORT-CHG '~'
112612         OUT-CSO-CMB-LF-PORT-CHG '~'
112612         OUT-CSO-AH-PORT-CHG    '~'
112612         OUT-CSO-PORTION-CHG    '~'
112612         OUT-ORIG-LF-CAN-DT     '~'
112612         OUT-NEW-LF-CAN-DT      '~'
112612         OUT-ORIG-AH-CAN-DT     '~'
112612         OUT-NEW-AH-CAN-DT      '~'
112612         OUT-ORIG-CAN-DT        '~'
112612         OUT-NEW-CAN-DT         '~'
112612         OUT-SCREEN-ID          '~'
103111            DELIMITED BY '  ' INTO NSAS-LETTER-VARIABLES
103111     END-STRING
103111
103111     WRITE NSAS-EXTRACT-RECORD
103111
103111     .
103111 0300-EXIT.
103111     EXIT.
103111

062017 0400-connect.
062017
062017***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
062017***                                                            ***
062017***  If this program is run in any other region than cid1p     ***
062017***  then it will populate the tabl on NTSQLTST2, otherwise    ***
062017***  it will populate the table on ntcso2.                     ***
062017***                                                            ***
062017***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
062017
062017     if dte-client not = 'CID'
062017        go to 0400-exit
062017     end-if
062017
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022     move 'TEST_Logic'           to svr
063022
063022     if ws-kix-myenv = 'cid1p'
063022        move 'PROD_Logic'        to svr
063022     end-if
062017
062017     display 'Begin connect to DB '
062017     string
062017         usr delimited space
062017         "." delimited size
062017         pass delimited space into usr-pass
062017     end-string
062017
062017     display ' usr pass ' usr-pass
062017
062017     EXEC SQL
062017        CONNECT TO :svr USER :usr-pass
062017     END-EXEC
062017
062017     if sqlcode not = 0
062017        display "Error: cannot connect "
062017        display sqlcode
062017        display sqlerrmc
062017        goback
062017     end-if
062017
062017     set connected-to-db to true
062017     display " connect to DB successful "
062017
062017     .
062017 0400-exit.
062017     exit.

       0500-state-totals.

           if zeros =                     ST-CURR-LF-PMTS-AMT   
              and                         ST-CURR-LF-PMTS-CNT   
              and                         ST-CURR-AH-PMTS-AMT   
              and                         ST-CURR-AH-PMTS-CNT   
              and                         ST-CURR-TOT-PMTS-AMT  
              and                         ST-CURR-TOT-PMTS-CNT  
              and                         ST-CURR-LF-PMTS-AMT-BM
              and                         ST-CURR-LF-PMTS-CNT-BM
              and                         ST-CURR-LF-PMTS-AMT-DR
              and                         ST-CURR-LF-PMTS-CNT-DR
              go to 0500-exit
           end-if

           move spaces                 to ws-total-line1
           string
              '0 Totals for State - '
              ws-psk-state
                delimited by size into ws-total-line1
           end-string
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           MOVE WS-HEADING9            TO PRT                              
           PERFORM WRITE-PRINTER                                    

           MOVE '0 STATE PAYMENTS INTERFACED '
                                       TO WS-TOTAL-LINE1
      *    MOVE SPACES                 TO WS-T1-TOTAL-DESCRIPTION
           MOVE ST-CURR-TOT-PMTS-AMT   TO WS-T1-CURR-AMOUNT
           MOVE ST-CURR-TOT-PMTS-CNT   TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           MOVE '  STATE PAYMENTS BELOW MINIMUM '
                                       TO WS-TOTAL-LINE1
           MOVE ST-CURR-LF-PMTS-AMT-BM TO WS-T1-CURR-AMOUNT
           MOVE ST-CURR-LF-PMTS-CNT-BM TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           MOVE '  STATE PAYMENTS DROPPED       '
                                       TO WS-TOTAL-LINE1
           MOVE ST-CURR-LF-PMTS-AMT-DR TO WS-T1-CURR-AMOUNT
           MOVE ST-CURR-LF-PMTS-CNT-DR TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           MOVE '0 STATE LIFE INTEREST AMT   '
                                       TO WS-TOTAL-LINE1
           MOVE ST-CURR-LF-PMTS-AMT    TO WS-T1-CURR-AMOUNT
           MOVE ST-CURR-LF-PMTS-CNT    TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           MOVE '  STATE DISAB INTEREST AMT  '
                                       TO WS-TOTAL-LINE1
           MOVE ST-CURR-AH-PMTS-AMT    TO WS-T1-CURR-AMOUNT
           MOVE ST-CURR-AH-PMTS-CNT    TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           MOVE ZEROS                  TO ST-CURR-LF-PMTS-AMT   
                                          ST-CURR-LF-PMTS-CNT   
                                          ST-CURR-AH-PMTS-AMT   
                                          ST-CURR-AH-PMTS-CNT   
                                          ST-CURR-TOT-PMTS-AMT  
                                          ST-CURR-TOT-PMTS-CNT  
                                          ST-CURR-LF-PMTS-AMT-BM
                                          ST-CURR-LF-PMTS-CNT-BM
                                          ST-CURR-LF-PMTS-AMT-DR
                                          ST-CURR-LF-PMTS-CNT-DR

           MOVE +99                    TO WS-LINE-COUNT

           .
       0500-exit.
           exit.

       4000-PRINT-TOTALS.

           PERFORM 0220-UPDATE-ARCHNO  THRU 0220-EXIT

           MOVE +99                    TO WS-LINE-COUNT
           MOVE +1                     TO WS-HEADING-SW

           IF WS-REPORT-SW = +1
               MOVE '0 * * * * * * * GRAND TOTALS * * * * * * *'
                                       TO  WS-HEADING6
           END-IF

           MOVE '0 TOTAL PAYMENTS INTERFACED '
                                       TO WS-TOTAL-LINE1
      *    MOVE SPACES                 TO WS-T1-TOTAL-DESCRIPTION
           MOVE CT-CURR-TOT-PMTS-AMT   TO WS-T1-CURR-AMOUNT
           MOVE CT-CURR-TOT-PMTS-CNT   TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           MOVE '  TOTAL PAYMENTS BELOW MINIMUM '
                                       TO WS-TOTAL-LINE1
           MOVE CT-CURR-LF-PMTS-AMT-BM TO WS-T1-CURR-AMOUNT
           MOVE CT-CURR-LF-PMTS-CNT-BM TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           MOVE '  TOTAL PAYMENTS DROPPED       '
                                       TO WS-TOTAL-LINE1
           MOVE CT-CURR-LF-PMTS-AMT-DR TO WS-T1-CURR-AMOUNT
           MOVE CT-CURR-LF-PMTS-CNT-DR TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           MOVE '0 TOTAL LIFE INTEREST AMT   '
                                       TO WS-TOTAL-LINE1
           MOVE CT-CURR-LF-PMTS-AMT    TO WS-T1-CURR-AMOUNT
           MOVE CT-CURR-LF-PMTS-CNT    TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           MOVE '  TOTAL DISAB INTEREST AMT  '
                                       TO WS-TOTAL-LINE1
           MOVE CT-CURR-AH-PMTS-AMT    TO WS-T1-CURR-AMOUNT
           MOVE CT-CURR-AH-PMTS-CNT    TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

      *    MOVE ' </PRE>'              TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' </HEAD>'             TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' </HTML>'             TO PRT
      *    PERFORM WRITE-A-LINE

           .
       4900-EXIT.                                                       
           EXIT.                                                        

       8500-DATE-CONVERSION. COPY ELCDCS.

       WRITE-A-LINE SECTION.         COPY ELCWAL.                       

       WRITE-HEADINGS SECTION.                                          
       WHS-010.                                                         
           IF LCP-ONCTR-01 =  0                                         
               ADD 1 TO LCP-ONCTR-01                                    
               MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   
               MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            
               MOVE ALPH-DATE          TO  WS-H3-DATE.                  
                                                                        
           ADD +1  TO  WS-PAGE.                                         
           MOVE WS-PAGE                TO  WS-H3-PAGE.                  
           MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        
           MOVE ZERO                   TO  WS-LINE-COUNT.               
                                                                        
           MOVE WS-HEADING1            TO  PRT.                         
           MOVE '1'                    TO  X.                           
           PERFORM WRITE-PRINTER.                                       
                                                                        
           MOVE WS-HEADING2            TO  PRT.                         
           MOVE ' '                    TO  X.                           
           PERFORM WRITE-PRINTER.                                       
                                                                        
           MOVE WS-HEADING3            TO  PRT.                         
           MOVE ' '                    TO  X.                           
           PERFORM WRITE-PRINTER.                                       
                                                                        
           MOVE WS-HEADING4            TO  PRT.                         
           MOVE ' '                    TO  X.                           
           PERFORM WRITE-PRINTER.                                       
                                                                        
           MOVE WS-HEADING5            TO  PRT.                         
           PERFORM WRITE-PRINTER.                                       
                                                                        
           MOVE +10                    TO  WS-LINE-COUNT.               
                                                                        
           IF WS-HEADING-SW NOT = ZERO                                  
               MOVE WS-HEADING6        TO  PRT                          
               PERFORM WRITE-PRINTER.                                   
                                                                        
           IF WS-HEADING-SW NOT = ZERO                                  
               MOVE WS-HEADING8    TO  PRT                              
               PERFORM WRITE-PRINTER                                    
               MOVE WS-HEADING9    TO  PRT                              
               PERFORM WRITE-PRINTER                                    
               MOVE +15            TO  WS-LINE-COUNT.                   
                                                                        
           IF WS-HEADING-SW = +2 OR +3                                  
               ADD +5  TO  WS-LINE-COUNT.                               
                                                                        
       WHS-020. COPY ELCWHS2.                                           
                                                                        
       WRITE-PRINTER SECTION. COPY ELCWPS.                              
                                                                        
       WPS-020. COPY ELCPRT2.
                                                                        
       CLOSE-FILES SECTION.
                                                                        
       CFS-010. COPY ELCPRTC.

062017     CLOSE ERPNDB ERMAIL ELCERT ERENDT
111109        FREEDOM-INT-FILE PRNTR ERACCT ELCNTL ERCNOT
072312        NSASEXTR ELLETR ELENCC

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACCT - CLOSE ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPNDB-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERPNDB - CLOSE ' ERPNDB-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERMAIL - CLOSE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCERT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ELCERT - CLOSE ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

111109     IF ERCNOT-FILE-STATUS NOT = '00'
111109        DISPLAY ' ERROR - ERCNOT - CLOSE ' ERCNOT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCNTL-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ELCNTL - CLOSE ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
103111
103111     IF NSAS-FILE-STATUS NOT = '00' 
103111        DISPLAY ' ERROR - NSASEXTR - CLOSE ' NSAS-FILE-STATUS
103111        PERFORM ABEND-PGM
103111     END-IF
072312
072312     IF ELLETR-FILE-STATUS NOT = '00'
072312        DISPLAY ' ERROR - ELLETR - CLOSE ' ELLETR-FILE-STATUS
072312        PERFORM ABEND-PGM
072312     END-IF
072312
072312     IF ELENCC-FILE-STATUS NOT = '00'
072312        DISPLAY ' ERROR - ELENCC - CLOSE ' ELENCC-FILE-STATUS
072312        PERFORM ABEND-PGM
072312     END-IF

062017     IF ERENDT-FILE-STATUS NOT = '00'
062017        DISPLAY ' ERROR - ERENDT - CLOSE ' ERENDT-FILE-STATUS
062017        PERFORM ABEND-PGM
062017     END-IF

          .
       CFS-EXIT.                                                        
           EXIT.                                                        
                                                                        
       ABEND-PGM SECTION. COPY ELCABEND.                                
