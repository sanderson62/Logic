      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EL596.

      *AUTHOR.     Pablo
      *            Colleyville, TX.
      *DATE-COMPILED.

      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************

      *REMARKS.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  This program reads through all the Check records on the   ***
      ***  CheckApproval DB in table ChkApp_Check on server NTCSO2   ***
      ***  with a maintained date > prev cycle date and <=           ***
      ***  current cycle date and company                            ***
      ***  = datefile company with a checktype of 1(refund  ) and    ***
      ***  approval status of 1,3 or 5(approved,denied,manual).      ***
      ***  For the 1's and 5's we update the erchek record with      ***
      ***  approval info and create interface records to feed corp   ***
      ***  finance and write erpyaj records to Logic. For the 3's we ***
      ***  just update erchek with a denied status. All status' are  ***
      ***  reported.                                                 ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
010516******************************************************************
010516*                   C H A N G E   L O G
010516*
010516* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010516*-----------------------------------------------------------------
010516*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010516* EFFECTIVE    NUMBER
010516*-----------------------------------------------------------------
010516* 010516  IR2016010500001  PEMA  FIX BEGIN AND END DATE FORMAT
091615* 091615  CR2015082000001  PEMA  ADD ENDORSEMENT CHECK PROCESSING
041217* 041217  IR2017033000001  PEMA  PASS NAME-2 TO FREEDOM INTERFACE
060717* 060717  CR2017032900002  PEMA  CHANGE TEST DB
010219* 010219  IR2019010300007  PEMA  Drop beginning date check
010320* 010320  CR2019121800003  PEMA  Make invoice# unique
030921* 030921  CR2019012500003  PEMA  Connect to sdv-db01.cso.local
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
010516******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERCHEK       ASSIGN TO ERCHEK
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERCHEK-FILE-STATUS
                               RECORD KEY IS CH-CONTROL-PRIMARY.

           SELECT ERPYAJ       ASSIGN TO ERPYAJ
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERPYAJ-FILE-STATUS
                               RECORD KEY IS PY-CONTROL-PRIMARY.

           SELECT ERPNDB       ASSIGN TO ERPNDB2
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERPNDB-FILE-STATUS
                               RECORD KEY IS PB-CONTROL-BY-ACCOUNT.

           SELECT FREE-FILE-NRM    ASSIGN TO SYS021
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FREE-FILE-MAN    ASSIGN TO SYS022
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FREE-FILE-VOID   ASSIGN TO SYS023
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRNTR           ASSIGN TO SYS008.
           SELECT FICH            ASSIGN TO SYS020.
           SELECT DISK-DATE    ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERCHEK.
                                       COPY ERCCHEK.

       FD  ERPYAJ.
                                       COPY ERCPYAJ.

       FD  ERPNDB.
                                       COPY ERCPNDB.

       FD  FREE-FILE-NRM
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.
     
       01  FREE-NRM-RECORD             PIC X(400).
     
       FD  FREE-FILE-MAN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.
     
       01  FREE-MAN-RECORD             PIC X(400).
     
       FD  FREE-FILE-VOID
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.
     
       01  FREE-VOIDS-RECORD           PIC X(32).
     
       FD  PRNTR                       COPY ELCPRTFD.
     
       FD  FICH                        COPY ELCFCHFD.

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       working-storage section.

       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
      
       77  ws-recs-out                 pic 9(7) value zeros.
       77  ws-recs-in                  pic 9(7) value zeros.
       77  eof-sw                      pic x value ' '.
           88  end-of-input               value 'Y'.
       77  erchek-file-status          pic xx value low-values.
       77  erpyaj-file-status          pic xx  value low-values.
       77  erpndb-file-status          pic xx  value low-values.
010320 77  ck                          pic s999 comp-3 value +0.
       77  s1                          pic s999 comp-3 value +0.
       77  s2                          pic s999 comp-3 value +0.
       77  ws-bin-start-dt             pic xx  value low-values.
       77  ws-bin-end-dt               pic xx  value low-values.
       77  ws-current-bin-dt           pic xx  value low-values.
       77  WS-PMT-AMT                  PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-VOID-SW                  PIC X  VALUE SPACES.
           88  PREV-VOID                      VALUE 'Y'.
       77  LCP-ONCTR-01                PIC S9(8) COMP-3 VALUE ZERO.
       77  ws-total-paid               pic s9(9)v99 comp-3 value +0.
       77  ws-total-paid-nrm           pic s9(9)v99 comp-3 value +0.
       77  ws-total-cnt-nrm            pic 9(5)  value zeros.
       77  ws-total-paid-man           pic s9(9)v99 comp-3 value +0.
       77  ws-total-cnt-man            pic 9(5)  value zeros.
       77  ws-total-voids              pic s9(9)v99 comp-3 value +0.
       77  ws-total-cnt-voids          pic 9(5)  value zeros.
       77  ws-erchek-sw                pic x  value spaces.
           88  erchek-found              value 'Y'.
       77  WS-NEXT-CYCLE-BIN-DATE      pic xx value low-values.
       77  ws-good-checks              pic s999 comp-3 value +0.
       77  ws-check-pndb-file          pic x  value ' '.
           88  check-pndb-file            value 'Y'.
       77  ws-erchek-file-sw           pic x value ' '.
           88  end-of-erchek              value 'Y'. 
       01  WS-TRANSACTION-RECORD       PIC X(400).
       01  WS-CHECK-DES-RECORD         PIC X(400).
       01  WS-DISTRIBUTION-RECORD      PIC X(400).
       01  WS-PAYEE-ADDRESS-RECORD     PIC X(400).
       01  ws-work-amt-alpha           pic x(10).
       01  ws-work-amt-num redefines ws-work-amt-alpha
                                       pic 9(8)v99.
       01  ws-seq-alpha                pic x(7).
       01  ws-seq-no redefines ws-seq-alpha
                                       pic 9(7).
       01  PGM-SUB                     PIC S999 COMP  VALUE +317.
       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.
       01  f.
           05  ws-current-key          pic x(33)  value low-values.
           05  ws-check-key            pic x(9).
           05  ws-check-key-num redefines ws-check-key
                                       pic 9(9).
           05  FILLER                      COMP-3.                      
               10  CT-CURR-LF-PMTS-AMT     PIC S9(9)V99  VALUE +0.
               10  CT-CURR-LF-PMTS-CNT     PIC S9(7)     VALUE +0.
     
               10  CT-CURR-LF-PMTS-AMT-BM        PIC S9(9)V99  VALUE +0.
               10  CT-CURR-LF-PMTS-CNT-BM        PIC S9(7)     VALUE +0.
           05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.
           05  X                           PIC X           VALUE SPACE.
           05  WS-HEADING-SW               PIC S9          VALUE ZERO.
           05  WS-PRINT-SW                 PIC S9          VALUE ZERO.
           05  WS-LINE-COUNT           PIC S9(3)       VALUE +99. 
           05  WS-LINE-COUNT-MAX       PIC S9(3)       VALUE +60. 
           05  WS-PAGE                 PIC S9(5)       VALUE ZERO.
           05  ws-comp-cd.
               10  filler              pic x  value low-values.
               10  ws-company-cd       pic x.
           05  ws-company-cd-num redefines ws-comp-cd
                                       pic s9(4) comp.

       01  filler.
           05  ws-vouch-cert-no        pic x(11) value spaces.
           05  ws-vouch-name           pic x(28) value spaces.
           05  ws-work-voucher.
               10  ws-vouch-reason     pic x(4).
               10  ws-vouch-cert-and-name
                                       pic x(26).

       01  WS-PREV-STATE               PIC XX     VALUE SPACES.
       01  WS-PREV-KEY.
           05  WS-PREV-CARRIER         PIC X      VALUE LOW-VALUES.
           05  WS-PREV-CLAIM-NO        PIC X(7)   VALUE LOW-VALUES.
           05  WS-PREV-CERT-NO         PIC X(11)  VALUE LOW-VALUES.
       01  WS-COMP-KEY.
           05  WS-COMP-CARRIER         PIC X.
           05  WS-COMP-CLAIM-NO        PIC X(7).
           05  WS-COMP-CERT-NO         PIC X(11).
       01  WS-INVOICE-NO.
           05  WS-INVOICE-MM           PIC 99.
           05  WS-INVOICE-DD           PIC 99.
           05  WS-INVOICE-CRT-NO       PIC X(8).
       01  WS-REC-GRP-CD.
           05  WS-REC-GRP-DA           PIC X.
           05  WS-REC-GRP-SEQ-NO       PIC 999    VALUE 000.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
      
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

       01  RUNTYP                      pic x(7)  value Z"RUNTYP".
       01  WS-RUNTYP.
           05  FILLER                  PIC X(10).

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
       01  ws-begin-dt                 pic x(10).
       01  ws-end-dt                   pic x(10).
       01  ws-compid                   pic xxx.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is passed nulls from sql. The indicator will be -1        ***
      ***  if the value on sql is nulls and +0 if the value is       ***
      ***  something other than nulls. Here is an example on how     ***
      ***  to use the indicator variables.                           ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        fetch checkapp into                                 ***
      ***           :db-app-status :nu-app-status,                   ***
      ***           :db-app-by     :nu-app-by,                       ***
      ***           :db-app-date   :nu-app-date,                     ***
      ***           :db-app-batch  :nu-app-batch                     ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-checkno              pic s9(4) comp value +0.
           05  nu-checkdate            pic s9(4) comp value +0.
           05  nu-checkstatus          pic s9(4) comp value +0.
           05  nu-fincar               pic s9(4) comp value +0.
           05  nu-fingrp               pic s9(4) comp value +0.
           05  nu-finresp              pic s9(4) comp value +0.
           05  nu-finacct              pic s9(4) comp value +0.
           05  nu-app-status           pic s9(4) comp value +0.
           05  nu-app-by               pic s9(4) comp value +0.
           05  nu-app-date             pic s9(4) comp value +0.
           05  nu-app-batch            pic s9(4) comp value +0.

       01  daily-check-request-rec.
           05  db-checkkey             pic x(9). 
           05  db-compid               pic xxx.  
           05  db-carrier              pic x.    
           05  db-grouping             pic x(6). 
           05  db-state                pic xx.   
           05  db-account              pic x(10).
           05  db-effdate              pic x(25).
           05  db-certificate          pic x(10).
           05  db-cert-sfx             pic x.    
           05  db-seq-no               pic x(7).
           05  db-type                 pic x(7).
PEMMOD*    05  db-amount               pic x(10).
PEMMOD     05  db-amount               pic 9(8)v99.
           05  db-checkno              pic x(15).
           05  db-checkdate            pic x(25).
           05  db-checkstatus          pic 9(5).
           05  db-releasebatch         pic x(5). 
           05  db-releasedt            pic x(25).
           05  db-releaseby            pic x(4). 
           05  db-payeename1           pic x(30).
           05  db-payeename2           pic x(30).
           05  db-payeeaddr1           pic x(30).
           05  db-payeeaddr2           pic x(30).
           05  db-payeecity            pic x(30).
           05  db-payeest              pic xx.   
           05  db-payeezip             pic x(11).
           05  db-fincar               pic x.    
           05  db-fingrp               pic x(6). 
           05  db-finresp              pic x(10).
           05  db-finacct              pic x(10).
           05  db-preparer             pic x(4). 
           05  db-app-status           pic x(10).
           05  db-app-by               pic x(20).
           05  db-app-date             pic x(25).
           05  db-app-batch            pic x(10).
           05  db-return-to            pic x(30).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  ws-user-seq-no             pic 9(7) value zeros.
       01  ws-user-defined.
           05  ws-user-reason         pic xxx.
           05  ws-user-cert-no        pic x(11).
           05  ws-user-name           pic x(28).

       01  WS-INT-USER-DEFINED        PIC X(24).

       01  ws-void-record-out.
           05  vr-bank-code            pic xxx.
           05  vr-check-no             pic x(10).
           05  vr-check-dt             pic x(8).
           05  vr-void-option          pic x.
           05  vr-void-reason          pic x(10).

                                       COPY ELCVOCH.

                                       COPY ELCFUNDT.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
                                       copy ELCDATE.

       01  ws-heading1.
           05  filler                  pic x(127)  value '1             
      -     '                                   DAILY REFUND CHECK REQUE
      -     'ST                                            EL596  '.

       01  ws-heading2.
           05  filler                  pic x(45) value spaces.
           05  WS-H2-CLIENT-NAME       PIC X(75) VALUE SPACES.
           05  WS-H2-DATE              PIC X(8).                    

       01  ws-heading3.
           05  filler                  pic x(53) value spaces.
           05  WS-H3-DATE              PIC X(67) VALUE SPACES.
           05  FILLER                  PIC X(5)  VALUE 'PAGE '.
           05  WS-H3-PAGE              PIC ZZ,ZZ9.                  

       01  ws-heading4.
           05  filler                  pic x(106)   value '-CAR  GROUP  
      -        'ST   ACCOUNT    EFF DTE       CERT NO    TYPE   REL DATE
      -        '  REL BY  APPRV DT  APPRV BY   AMOUNT'.

       01  ws-detail1.
           05  filler                  pic xx value '0 '.
           05  ws-d1-carr              pic x(4).
           05  ws-d1-grp               pic x(7).
           05  ws-d1-state             pic x(4).
           05  ws-d1-account           pic x(12).
           05  ws-d1-eff-dt            pic x(12).
           05  ws-d1-cert-no           pic x(14).
           05  ws-d1-chk-type          pic x(5).
           05  ws-d1-rel-dt            pic x(12).
           05  ws-d1-rel-by            pic x(6).
           05  ws-d1-apprv-dt          pic x(12).
           05  ws-d1-apprv-by          pic x(8).
      *    05  ws-d1-chk-amt           pic x(10).
           05  ws-d1-chk-amt           pic $$$,$$9.99.
           05  filler                  pic xx.
           05  ws-d1-message           pic x(20).

       01  ws-detail2.
           05  filler                  pic x(12) value '    PAYEE - '.
           05  ws-d2-addr-line         pic x(120) value spaces.

       01  ws-detail3.
           05  filler                  pic x(21) value
              '    VOID INFO - DATE '.
           05  ws-d3-void-dt           pic x(10) value spaces.
           05  filler                  pic x(4)  value ' BY '.
           05  ws-d3-void-by           pic x(4).
           05  filler                  pic x(9)  value '  CHK NO '.
           05  ws-d3-chk-no            pic x(7)  value spaces.
           05  filler                  pic x(11) value '  CHK DATE '.
           05  ws-d3-chk-dt            pic x(10) value spaces.
           05  filler                  pic x(8)  value ' REASON '.
           05  ws-d3-void-reason       pic x(30) value spaces.

       01  ws-total1.
           05  filler                  pic x(5) value '-    '.
           05  ws-t1-cnt               pic zzzz9  value zeros.
           05  ws-t1-chks              pic x(24)  value spaces.
           05  ws-t1-amount            pic $$,$$$,$$9.99 value zeros.

       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       LINKAGE SECTION.                                                 
                                                                        
       01  PARM.
           05  PARM-LENGTH      PIC S9(4)   COMP.
           05  START-DATE       PIC X(8).
           05  END-DATE         PIC X(8).

       01  var                         pic x(30).
       PROCEDURE DIVISION USING PARM.
                                       COPY ELCDTERX.

       0000-begin.

           display ' Begin Program EL596 '

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if
           display ' KIXSYS = ' ws-kix-myenv

           display ' ws-kix-myenv ' ws-kix-myenv

           perform 0010-init           thru 0010-exit
           perform 0020-connect        thru 0020-exit

      *    EXEC SQL
      *       SET AUTOCOMMIT OFF
      *    END-EXEC

           perform 0025-open-files     thru 0025-exit
           perform 0030-open-cursor    thru 0030-exit
           perform 0040-process-input  thru 0040-exit until
              end-of-input
      *      or ws-recs-in > 10000

           perform 0080-finish-up      thru 0080-exit
           perform 0100-process-voids  thru 0100-exit
           perform 0120-print-totals   thru 0120-exit
           perform 0090-close-files    thru 0090-exit

           display ' End Program '
           display ' records inserted    ' ws-recs-out
           display ' table records read  ' ws-recs-in
           goback

           .
       0010-init.

           move dte-client             to ws-compid
           MOVE start-date             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 0800-date-convert   thru 0800-exit
           if no-conversion-error
010516*       move dc-greg-date-a-edit to ws-begin-dt
010516        string
010516           dc-edita-ccyy    '.'
010516           dc-edita-month   '.'
010516           dc-edita-day
010516           delimited by size into ws-begin-dt
010516        end-string
              move dc-bin-date-1       to ws-bin-start-dt
           else
              display ' Begin date invalid ' start-date
                 ' ' dc-error-code
              perform abend-pgm
           end-if

           MOVE end-date               TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 0800-date-convert   thru 0800-exit
           if no-conversion-error
010516*       move dc-greg-date-a-edit to ws-end-dt
010516        string
010516           dc-edita-ccyy    '.'
010516           dc-edita-month   '.'
010516           dc-edita-day
010516           delimited by size into ws-end-dt
010516        end-string
              move dc-bin-date-1       to ws-bin-end-dt
           else
              display ' End date invalid ' end-date
                 ' ' dc-error-code
              perform abend-pgm
           end-if

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           DISPLAY ' WS-FN-DATE ' WS-FN-DATE
           MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 0800-DATE-CONVERT   THRU 0800-EXIT
           IF NO-CONVERSION-ERROR
              move dc-greg-date-1-alpha to ws-h3-date
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DT
              DISPLAY ' DAY OF WEEK = ' DC-DAY-OF-WEEK
              MOVE +1                  TO DC-ELAPSED-DAYS
              IF DC-DAY-OF-WEEK = 6
                 ADD +2                TO DC-ELAPSED-DAYS
              ELSE
                 IF DC-DAY-OF-WEEK = 7
                    ADD +1             TO DC-ELAPSED-DAYS
                 END-IF
              END-IF
              DISPLAY ' ELAPSED DAYS ' DC-ELAPSED-DAYS
              MOVE WS-CURRENT-BIN-DT   TO DC-BIN-DATE-1
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 0800-DATE-CONVERT THRU 0800-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-2    TO WS-NEXT-CYCLE-BIN-DATE
              ELSE
                 DISPLAY ' PROBLEMS CONVERTING NEXT DATE '
                 PERFORM ABEND-PGM
              END-IF
           ELSE
              DISPLAY 'BAD CURRENT DATE ' WS-FN-DATE
              PERFORM ABEND-PGM
           END-IF

           MOVE SPACES                 TO TRANSACTION-RECORD
                                          CHECK-DES-RECORD
                                          DISTRIBUTION-RECORD
                                          PAYEE-ADDRESS-RECORD
                                          
           MOVE 'T'                    TO TR-RECORD-ID
           MOVE 'F'                    TO CD-RECORD-ID
           MOVE 'D'                    TO DR-RECORD-ID
           MOVE 'A'                    TO PR-RECORD-ID
           MOVE WS-FN-DA (2:1)         TO WS-REC-GRP-DA
           if dte-client = 'CID'
              MOVE 'CIDMISCVEN'           TO TR-VENDOR-ID
                                          CD-VENDOR-ID
                                          DR-VENDOR-ID
                                          PR-VENDOR-ID
           else
              MOVE 'MISVEN'               TO TR-VENDOR-ID
                                          CD-VENDOR-ID
                                          DR-VENDOR-ID
                                          PR-VENDOR-ID
           end-if

           MOVE 'P'                    TO TR-CHECK-TYPE
           MOVE 'R'                    TO TR-TRAN-TYPE
062121     evaluate dte-client
062121        when 'AHL'
062121           move 'AHL'            to tr-cso
062121        when 'FNL'
062121           move 'FNL'            to tr-cso
062121        when other
062121           move 'CSO'            to tr-cso
062121     end-evaluate

           STRING WS-FN-MO WS-FN-DA WS-FN-CCYR DELIMITED BY SIZE
              INTO TR-INVOICE-DATE
           END-STRING
              
           MOVE 'IMM'                  TO TR-TERMS-CODE
           MOVE 001                    TO CD-SEQ-NO
                                          DR-SEQ-NO
           MOVE 'E'                    TO DR-DIST-TYPE (1)
           MOVE 'AP'                   TO DR-SOURCE (1)
           MOVE 'S'                    TO DR-SUSPENSE (1) (1:1)
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

           MOVE +99                    TO WS-LINE-COUNT

           MOVE WS-CURRENT-DATE        TO WS-H2-DATE
           MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME

           .
       0010-exit.
           exit.

       0020-connect.

           display 'Begin connect to DB '

      ****  The below code is for when the db has been
      ****  converted to sql server 2016
           evaluate ws-kix-myenv
              when 'cid1p'
                 move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
      *       when 'mdoff'
      *          move '//hov-tstdb01.cso.local:55330;'
      *                                to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate

           move 'CheckApproval'        to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc

           if sqlcode not = 0
              display "Error: cannot connect "
              display sqlcode
              display sqlerrmc
              perform abend-pgm
           end-if

           display " connect to DB successful "

           .
       0020-exit.
           exit.

       0025-open-files.

pemtst     open i-o erchek
pemtst*    open input erchek
           if erchek-file-status not = '00'
              display ' error-open-erchek ' erchek-file-status
              perform abend-pgm
           end-if

pemtst     open i-o erpyaj
pemtst*    open input erpyaj
           if erpyaj-file-status not = '00'
              display ' error-open-erpyaj ' erpyaj-file-status
              perform abend-pgm
           end-if

pemtst     open i-o erpndb
pemtst*    open input erpndb
           if erpndb-file-status not = '00'
              display ' error-open-erpndb ' erpndb-file-status
              perform abend-pgm
           end-if

           open output prntr
                       free-file-NRM FREE-FILE-MAN

           .
       0025-exit.
           exit.

       0030-open-cursor.

pemtst     display ' declare cursor ' ws-begin-dt ' ' ws-end-dt

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  The dates on the sql table have values in the time        ***
      ***  so I convert it to a string and just use mm/dd/yyyy       ***
      ***  to perform the comparison.                                ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           EXEC SQL
              DECLARE
                 checkapp cursor for
              SELECT
                 CheckKey,                          
                 Company,
                 CertCarrier,
                 CertGroup,
                 CertState,
                 CertAccount,
                 CertEffDate,
                 CertNumber,
                 CertNumberSuf,
                 CheckSeqNbr,
                 CheckType,
      *          convert(decimal(9,2),CheckAmount),
                 CheckAmount,
                 CheckNumber,
                 CheckDate,
                 CheckStatus,
                 ReleaseBatchNbr,
                 ReleaseDate,
                 ReleasedBy,
                 PayeeName1,
                 PayeeName2,
                 PayeeAddress1,
                 PayeeAddress2,
                 PayeeCity,
                 PayeeState,
                 PayeeZIP,
                 CompCarrier,
                 CompGroup,
                 CompFinResp,
                 CompAccount,
                 Preparer,
                 ApprovalStatus,
                 MaintainedBy,
                 MaintainedDate,
                 ApprovalBatch,
                 ReturnTo
              FROM
                 ChkApp_Check
              WHERE
                 Company = :ws-compid
                            and
                 CheckType = '1'
                            and
010219           CheckStatus is null
010219                      and
010516           convert(varchar(10), MaintainedDate, 102)
                                     <= :ws-end-dt
                            and
                 (ApprovalStatus = '1'
                  or ApprovalStatus = '3'
                  or ApprovalStatus = '5')
           END-EXEC
           display ' After Declare ' sqlcode ' '
            sqlerrmc

           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform 0085-disconnect  thru 0085-exit
              perform abend-pgm
           end-if

           EXEC SQL
              open checkapp
           END-EXEC
           display ' after open cursor ' sqlcode ' '
             sqlerrmc 

           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform 0085-disconnect  thru 0085-exit
              perform abend-pgm
           end-if

           .
       0030-exit.
           exit.

       0040-process-input.
           display 'made it to 0040- '

           perform until sqlcode not = 0
              EXEC SQL
                 fetch checkapp into
                    :db-checkkey,
                    :db-compid,      
                    :db-carrier,     
                    :db-grouping,    
                    :db-state,       
                    :db-account,     
                    :db-effdate,     
                    :db-certificate, 
                    :db-cert-sfx,    
                    :db-seq-no,      
                    :db-type,        
                    :db-amount,      
                    :db-checkno :nu-checkno,
                    :db-checkdate :nu-checkdate,
                    :db-checkstatus :nu-checkstatus,
                    :db-releasebatch,
                    :db-releasedt,   
                    :db-releaseby,   
                    :db-payeename1,  
                    :db-payeename2,  
                    :db-payeeaddr1,  
                    :db-payeeaddr2,  
                    :db-payeecity,   
                    :db-payeest,     
                    :db-payeezip,    
                    :db-fincar :nu-fincar,
                    :db-fingrp :nu-fingrp,
                    :db-finresp :nu-finresp,
                    :db-finacct :nu-finacct,
                    :db-preparer,
                    :db-app-status :nu-app-status,
                    :db-app-by :nu-app-by,
                    :db-app-date :nu-app-date,
                    :db-app-batch :nu-app-batch,
                    :db-return-to
              END-EXEC
              display ' after fetch ' sqlcode

              if sqlcode not = 0 and 100
                 display "Error: cannot read row " ws-recs-in
                 display ' sql return code ' sqlcode
                 display ' sql err mess    ' sqlerrmc
                 perform 0085-disconnect  thru 0085-exit
                 perform abend-pgm
              end-if

              if (sqlcode = 0)
pemtst           and (nu-checkstatus = -1)
                 add 1                 to ws-recs-in
                 perform 0045-process-record
                                       thru 0045-exit
              end-if
           end-perform

           if sqlcode = 100
              display ' Normal end of record set '
              display ' number of records        ' ws-recs-in
           end-if

           set end-of-input            to true

           EXEC SQL
               close checkapp
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform 0085-disconnect  thru 0085-exit
           end-if

           .
       0040-exit.
           exit.

       0045-process-record.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  First, read erchek.                                       ***
      ***   If not found - report and go to next record.             ***
      ***   If found - create g/l records, update sql table with     ***
      ***     check status, update erchek rec with approval data,    ***
      ***     write print line and create erpyaj record.             ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           move dte-clasic-company-cd  to ch-company-cd
           move db-carrier             to ch-carrier
           move db-grouping            to ch-grouping
           move db-state               to ch-state
           move db-account             to ch-account
           move db-certificate         to ch-cert-prime
           move db-cert-sfx            to ch-cert-sfx

           string db-effdate (1:4)
                  db-effdate (6:2)
                  db-effdate (9:2)
              delimited by size into dc-greg-date-cymd-r
           end-string
           move 'L'                    to dc-option-code
           perform 0800-date-convert   thru 0800-exit
           if no-conversion-error
              move dc-bin-date-1       to ch-cert-eff-dt
           else
              display ' error cvtdte eff dt ' db-effdate ' '
                 dc-error-code
           end-if
           move zeros                  to ws-seq-alpha
           move +7                     to s2
           perform varying s1 from +7 by -1 until s1 < +1
              if db-seq-no (s1:1) numeric
                 move db-seq-no (s1:1) to ws-seq-alpha (s2:1)
                 subtract +1 from s2
              end-if
           end-perform

           move ws-seq-no              to ch-sequence-no
           move ' '                    to ws-erchek-sw

pemtst*    display ' dte   **' db-effdate '**'
pemtst*    display ' carrier ' ch-carrier
pemtst*    display ' group   ' ch-grouping
pemtst*    display ' state   ' ch-state
pemtst*    display ' acct    ' ch-account
pemtst*    display ' cert  **' ch-cert-no '**'
pemtst*    display ' seq     ' ch-sequence-no
           read erchek

           if erchek-file-status = '00'
              move spaces              to ws-d1-message
              set erchek-found to true
              if db-app-status = '1' or '5'
                 perform 0300-g-l-processing
                                       thru 0300-exit
              end-if
              if db-app-status = '1' or '3' or '5'
010219           perform 0050-update-table
010219                                 thru 0050-exit
                 perform 0055-update-erchek
                                       thru 0055-exit
                 move '**  D E N I E D   **'
                                       to ws-d1-message
              end-if
              if db-app-status = '1' or '5'
                 perform 0500-BUILD-ERPYAJ-RECORD
                                       thru 0500-exit
                 move 'I N T E R F A C E D '
                                       to ws-d1-message
              end-if
           else
              move '**ERROR** NOT FOUND '
                                       to ws-d1-message
           end-if
             
           perform 0200-create-rpt     thru 0200-exit

           .
       0045-EXIT.
           EXIT.

       0050-update-table.
           display ' made it to 0050- '

pemtst*    go to 0050-exit

           move 001                    to db-checkstatus

010219     if db-app-status = '3'
010219        move 003                 to db-checkstatus
010219     end-if

           EXEC SQL
              UPDATE
                 ChkApp_Check
              SET
                 CheckStatus = :db-checkstatus
              WHERE
                 CheckKey = :db-checkkey
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot update table   "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform 0085-disconnect  thru 0085-exit
              perform abend-pgm
           end-if

           .
       0050-exit.
           exit.

       0055-update-erchek.
           display ' made it to 0055- '

           move ' '                    to ws-check-pndb-file
           move +0                     to ws-good-checks
           if db-app-status = '3'
              move 'D'                 to ch-approval-status
              set check-pndb-file      to true
              move ch-control-primary  to ws-current-key
           else
              move 'A'                 to ch-approval-status
           end-if

           move function UPPER-CASE(db-app-by)
                                       to ch-approved-by

           string db-app-date (1:4)
                  db-app-date (6:2)
                  db-app-date (9:2)
              delimited by size into dc-greg-date-cymd-r
           end-string
           move 'L'                    to dc-option-code
           PERFORM 0800-date-convert   thru 0800-exit
           if no-conversion-error
              move dc-bin-date-1       to ch-approval-dt
           else
              move WS-CURRENT-BIN-DT   to ch-approval-dt
              display ' apprv date invalid ' db-app-date
                 ' ' dc-error-code
           end-if
pemtst     move '00'                   to erchek-file-status
pemtst     rewrite check-records
           if erchek-file-status not = '00'
              display ' error-erchek-rewrite ' erchek-file-status ' '
                 ch-cert-no ' ' db-checkkey
              perform 0085-disconnect  thru 0085-exit
              perform abend-pgm
           end-if

           if check-pndb-file
              perform 0060-browse-erchek
                                       thru 0060-exit
              if ws-good-checks = zeros
                 perform 0070-update-erpndb
                                       thru 0070-exit
              end-if
           end-if
              
           .
       0055-exit.
           exit.

       0060-browse-erchek.

           move ws-current-key         to ch-control-primary
           move +0                     to ch-sequence-no
           start erchek key >= ch-control-primary
           if erchek-file-status not = '00'
              display ' error-erchek-start ' erchek-file-status
                 ' ' ws-current-key (2:19) ws-current-key (23:11)
              perform 0085-disconnect  thru 0085-exit
              go to abend-pgm
           end-if           

           .
       0060-read-next.

           read erchek next record
           if (erchek-file-status = '10' or '23')
              or (ch-company-cd not = dte-clasic-company-cd)
              go to 0060-exit
           else
              if erchek-file-status not = '00'
                 display ' error-erchek-readnext ' erchek-file-status
                    ' ' ws-current-key (2:19) ws-current-key (23:11)
                 perform 0085-disconnect  thru 0085-exit
                 go to abend-pgm
              end-if
           end-if

           if ch-control-primary (1:33) not = ws-current-key
              go to 0060-exit
           end-if

           if (ch-amount-paid > zeros)
              and (ch-void-dt not = low-values)
              and (ch-approval-status not = 'D')
              add +1 to ws-good-checks
           end-if

           go to 0060-read-next

           .
       0060-exit.
           exit.

       0070-update-erpndb.

           move ws-current-key         to pb-control-by-account
           move +0                     to pb-alt-chg-seq-no
           move '2'                    to pb-record-type
           read erpndb
           if erpndb-file-status = '00'
              move ' '                 to pb-c-refund-sw
pemtst        move '00'                to erpndb-file-status
pemtst        rewrite pending-business
              if erpndb-file-status not = '00'
                 display ' error-erpndb-rewrite ' erpndb-file-status
                    ' ' ws-current-key (2:19) ws-current-key (23:11)
                 perform 0085-disconnect  thru 0085-exit
                 go to abend-pgm
              end-if
           end-if

           .
       0070-exit.
           exit.

       0080-finish-up.

           display ' Begin Finish up '
           EXEC SQL
NTTDel*        commit work release
NTTIns         commit work 
           END-EXEC
           if sqlcode not = 0
              display "Error: commit release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if
NTTIns    perform 0085-disconnect  thru 0085-exit

           .
       0080-exit.
           exit.

       0085-disconnect.

           display ' Begin Disconnect '
           EXEC SQL
               DISCONNECT ALL
           END-EXEC

           if sqlcode not = 0
              display "Error: on disconnect  "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0085-exit.
           exit.

       0090-close-files.

           close erpyaj
                 erpndb
                 erchek
                 prntr
                 free-file-nrm
                 free-file-man
                 FREE-FILE-VOID

           .
       0090-exit.
           exit.

       0100-process-voids.
           display ' made it to 0100-process '

           open output FREE-FILE-VOID.
           display ' after open output '
           move dte-clasic-company-cd  to ch-control-primary
           start erchek key >= ch-control-primary
           if (erchek-file-status = '10' or '23')
              or (ch-company-cd not = dte-clasic-company-cd)
              set end-of-erchek to true
              go to 0100-exit
           else
              if erchek-file-status not = '00'
                 display ' error erchek start ' erchek-file-status
                 go to abend-pgm
              end-if
           end-if

               .
       0100-find-voids.

           display 'made it to 0100-find-'
           read erchek next record
           if (erchek-file-status = '10' or '23')
              or (ch-company-cd not = dte-clasic-company-cd)
              set end-of-erchek to true
           else
              if erchek-file-status not = '00'
                 display ' error erchek readnext ' erchek-file-status
                 go to abend-pgm
              end-if
           end-if

           if end-of-erchek
              go to 0100-exit
           end-if

pemtst*    if ch-void-dt not = low-values
pemtst     if (ch-void-dt > ws-bin-start-dt)
              and (ch-void-dt <= ws-bin-end-dt)
              perform 0110-create-void-int
                                       thru 0110-exit
           end-if

           go to 0100-find-voids

           .
       0100-exit.
           exit.

       0110-create-void-int.
           display ' made it to 0110- '

           move spaces                 to ws-void-record-out
           if dte-client = 'CID'
              move 'SNB'               to vr-bank-code
           else
              move 'FNB'               to vr-bank-code
           end-if
           move '000'                  to vr-check-no (1:3)
           move ch-check-no            to vr-check-no (4:7)
           move 'X'                    to vr-void-option
      *    move ch-void-reason         to vr-void-reason

           move ch-check-written-dt    to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 0800-DATE-CONVERT   THRU 0800-EXIT
           IF NO-CONVERSION-ERROR
              string dc-edita-month
                     dc-edita-day
                     dc-edita-ccyy
                 delimited by size into vr-check-dt
              end-string
           end-if

           write FREE-VOIDS-RECORD from ws-void-record-out

           perform 0115-void-report    thru 0115-exit

           .
       0110-exit.
           exit.

       0115-void-report.
           display ' made it to 0115- '

           move ch-carrier             to ws-d1-carr
           move ch-grouping            to ws-d1-grp
           move ch-state               to ws-d1-state
           move ch-account             to ws-d1-account

           move ch-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 0800-DATE-CONVERT   THRU 0800-EXIT
           IF NO-CONVERSION-ERROR
              move dc-greg-date-a-edit to ws-d1-eff-dt
           end-if

           move ch-cert-no             to ws-d1-cert-no

091615     if ch-check-origin-sw = 'C'
091615        move 'COR'               to ws-d1-chk-type
091615     else
091615        move 'REF'               to ws-d1-chk-type
091615     end-if

           move ch-approval-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 0800-DATE-CONVERT   THRU 0800-EXIT
           IF NO-CONVERSION-ERROR
              move dc-greg-date-a-edit to ws-d1-apprv-dt
           end-if
           move ch-approved-by         to ws-d1-apprv-by
           move ch-amount-paid         to ws-d1-chk-amt

           move '**    V O I D     **'
                                       to ws-d1-message

           MOVE WS-DETAIL1             TO PRT
           perform write-a-line
           move spaces                 to ws-d2-addr-line
           string ch-payee-name-1  '  ' ch-payee-name-2   '  '
               ch-payee-address-1  '  ' ch-payee-address-2   '  '
               ch-payee-city       '  ' ch-payee-state      '  '
               ch-payee-zip
              delimited by '   ' into ws-d2-addr-line
           end-string
           MOVE WS-DETAIL2             TO PRT
           perform write-a-line


           move ch-void-dt             to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 0800-DATE-CONVERT   THRU 0800-EXIT
           IF NO-CONVERSION-ERROR
              move dc-greg-date-a-edit to ws-d3-void-dt
           end-if

           move ch-void-by             to ws-d3-void-by
           move ch-check-no            to ws-d3-chk-no

           move ch-check-written-dt    to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 0800-DATE-CONVERT   THRU 0800-EXIT
           IF NO-CONVERSION-ERROR
              move dc-greg-date-a-edit to ws-d3-chk-dt
           end-if
           move ch-void-reason         to ws-d3-void-reason

           MOVE WS-DETAIL3             TO PRT
           perform write-a-line

           compute ws-total-cnt-voids =
              ws-total-cnt-voids + 1

           compute ws-total-voids =
              ws-total-voids + ch-amount-paid

           .
       0115-exit.
           exit.

       0120-print-totals.

           display ' made it to 0120- '
           move ws-total-cnt-nrm       to ws-t1-cnt
           move ' Standard Checks, Total '
                                       to ws-t1-chks
           move ws-total-paid-nrm      to ws-t1-amount
           MOVE WS-total1              TO PRT
           perform write-a-line

           move spaces                 to ws-total1
           move ws-total-cnt-man       to ws-t1-cnt
           move ' Manual   Checks, Total '
                                       to ws-t1-chks
           move ws-total-paid-man      to ws-t1-amount
           MOVE WS-total1              TO PRT
           perform write-a-line

           move spaces                 to ws-total1
           compute ws-t1-cnt = ws-total-cnt-nrm + ws-total-cnt-man
           move ' Total    Checks, Total '
                                       to ws-t1-chks
           compute ws-t1-amount =
              ws-total-paid-nrm + ws-total-paid-man
           MOVE WS-total1              TO PRT
           perform write-a-line

           move spaces                 to ws-total1
           move ws-total-cnt-voids     to ws-t1-cnt
           move ' Total    Voids,  Total '
                                       to ws-t1-chks
           move ws-total-voids         to ws-t1-amount
           MOVE WS-total1              TO PRT
           perform write-a-line

           .
       0120-exit.
           exit.

       0200-create-rpt.
           display ' made it to 0200- '
           move db-carrier             to ws-d1-carr
           move db-grouping            to ws-d1-grp
           move db-state               to ws-d1-state
           move db-account             to ws-d1-account

           string db-effdate (6:2)     '/'
                  db-effdate (9:2)     '/'
                  db-effdate (1:4)     '  '
              delimited by size into ws-d1-eff-dt
           end-string

           string db-certificate db-cert-sfx
              delimited by size into ws-d1-cert-no
           end-string

091615     if erchek-file-status = '00'
091615        if ch-check-origin-sw = 'C'
091615           move 'COR'            to ws-d1-chk-type
091615        else
091615           move 'REF'            to ws-d1-chk-type
091615        end-if
091615     else
091615        move '***'               to ws-d1-chk-type
091615     end-if
091615
091615*    move 'REF'                  to ws-d1-chk-type

           string db-releasedt (6:2)     '/'
                  db-releasedt (9:2)     '/'
                  db-releasedt (1:4)     '  '
              delimited by size into ws-d1-rel-dt
           end-string

           move db-releaseby           to ws-d1-rel-by

           string db-app-date (6:2)     '/'
                  db-app-date (9:2)     '/'
                  db-app-date (1:4)     '  '
              delimited by size into ws-d1-apprv-dt
           end-string
           move db-app-by              to ws-d1-apprv-by
PEMMOD*    move +10                    to s2
PEMMOD*    move zeros                  to ws-work-amt-alpha
PEMMOD*    perform varying s1 from +10 by -1 until s1 < +1
PEMMOD*       if db-amount (s1:1) numeric
PEMMOD*          move db-amount (s1:1) to ws-work-amt-alpha (s2:1)
PEMMOD*          subtract +1 from s2
PEMMOD*       end-if
PEMMOD*    end-perform

           if erchek-found
              if db-app-status = '1'
                 compute ws-total-cnt-nrm =
                    ws-total-cnt-nrm + 1
                 compute ws-total-paid-nrm =
                    ws-total-paid-nrm + db-amount
PEMMOD*             ws-total-paid-nrm + ws-work-amt-num
              else
                 if db-app-status = '5'
                    compute ws-total-cnt-man =
                       ws-total-cnt-man + 1
                    compute ws-total-paid-man =
PEMMOD*                ws-total-paid-man + ws-work-amt-num
                       ws-total-paid-man + db-amount
                 end-if
              end-if
           end-if
PEMMOD*    move ws-work-amt-num        to ws-d1-chk-amt
PEMMOD     move db-amount              to ws-d1-chk-amt
           MOVE WS-DETAIL1             TO PRT
           perform write-a-line
           move spaces                 to ws-d2-addr-line
           string db-payeename1   '  ' db-payeename2   '  '
               db-payeeaddr1      '  ' db-payeeaddr2   '  '
               db-payeecity       '  ' db-payeest      '  '
               db-payeezip
              delimited by '   ' into ws-d2-addr-line
           end-string
           MOVE WS-DETAIL2             TO PRT
           perform write-a-line

           .
       0200-exit.
           exit.

       0300-G-L-PROCESSING.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  Keep in mind we may have to combine records if there      ***
      ***  are 2+ checks going to the same person for the same       ***
      ***  certificate number.                                       ***
      ***                                                            ***
      ***  if there is a break in the key                            ***
      ***     perform 0400-create-freedom thru 0400-exit             ***
      ***     move new key to prev key                               ***
      ***  end-if                                                    ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

010320*    MOVE WS-FN-MO               TO WS-INVOICE-MM
010320*    MOVE WS-FN-DA               TO WS-INVOICE-DD
010320*    MOVE ch-cert-no (4:8)       TO WS-INVOICE-CRT-NO
010320
010320     move spaces                 to ws-invoice-no           
010320
010320     perform varying ck from +1 by +1 until
010320        db-checkkey(ck:1) = ' '
010320     end-perform
010320
010320     compute ck = ck - 1       *> sets the length of key

010320     move db-checkkey(1:ck)      to ws-invoice-no
010320     move ch-cert-no (ck:12 - ck)
010320                                 to ws-invoice-no(ck + 1:12 - ck)

           MOVE WS-INVOICE-NO          TO TR-INVOICE-NO
                                          CD-INVOICE-NO
                                          DR-INVOICE-NO
                                          PR-INVOICE-NO

           move '00'                   to dr-acct-state (1)
           COMPUTE WS-PMT-AMT = WS-PMT-AMT + ch-amount-paid
           MOVE WS-PMT-AMT             TO TR-INVOICE-AMT
                                          CD-INVOICE-AMT (1)
                                          DR-INVOICE-AMT (1)

                                          
           IF WS-PMT-AMT < ZEROS
              MOVE '-'                 TO TR-INVOICE-AMT-SIGN
                                          CD-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (2)
           ELSE
              MOVE '+'                 TO TR-INVOICE-AMT-SIGN
                                          CD-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (1)
           END-IF

           move ch-stub-line-1         to tr-sundry

091615     if ch-check-origin-sw = 'C'
091615        move 'COR'               to ws-work-voucher
091615     else
091615        move 'REF'               to ws-work-voucher
091615     end-if
091615*    move 'REF '                 to ws-work-voucher
           move ch-cert-no             to ws-vouch-cert-no
           inspect ws-vouch-cert-no replacing leading zeros
              by spaces

           perform varying s1 from +1 by +1 until
              (s1 > +11)
              or (ws-vouch-cert-no (s1:1)) <> ' '
           end-perform

           inspect ch-insured-name
              replacing all low-values by spaces
           move ch-insured-name        to ws-vouch-name

           string ws-vouch-cert-no (s1:11 - s1 + 1) ' '
                  ws-vouch-name
              delimited by size into ws-vouch-cert-and-name
           end-string

           move ws-work-voucher        to tr-voucher-ref

           move zeros                  to ws-check-key
           move +9                     to s2
       
           perform varying s1 from +9 by -1 until s1 < +1
              if db-checkkey (s1:1) numeric
                 move db-checkkey (s1:1) to ws-check-key (s2:1)
                 subtract +1 from s2
              end-if
           end-perform
       
           move ws-check-key-num       to ws-user-seq-no
       
           if ch-return-to (1:4) = spaces
              move 'ENC '              to ch-return-to
           end-if

           string ch-comp-fin-resp    ' '
                  ch-return-to (1:4)  ' '
                  ws-user-seq-no
              delimited by size into tr-user-defined
           end-string

           STRING ' INSURED : ' ch-insured-name
              DELIMITED BY SIZE INTO CD-DESC (1)
           END-STRING

           MOVE ch-payee-name-1        TO PR-PAYEE-NAME
041217     if ch-payee-name-2 not = spaces
041217        move ch-payee-name-2     to pr-address (1)
041217     end-if

041217     MOVE ch-payee-address-1     TO PR-ADDRESS (2)
041217     MOVE ch-payee-address-2     TO PR-ADDRESS (3)
041217     MOVE SPACES                 TO PR-ADDRESS (4)

091615     string
091615        ch-payee-city delimited by '  '
091615        ' '
091615        ch-payee-state delimited by size
091615        ' '
091615        ch-payee-zip-code delimited by '  '
041217         INTO PR-ADDRESS (4)
091615     END-STRING

041217     perform 3 times
041217        if pr-address (1) = spaces
041217           move pr-address (2)   to pr-address (1)
041217           move pr-address (3)   to pr-address (2)
041217           move pr-address (4)   to pr-address (3)
041217           move spaces           to pr-address (4)
041217        end-if
041217        if pr-address (2) = spaces
041217           move pr-address (3)   to pr-address (2)
041217           move pr-address (4)   to pr-address (3)
041217           move spaces           to pr-address (4)
041217        end-if
041217        if pr-address (3) = spaces
041217           move pr-address (4)   to pr-address (3)
041217           move spaces           to pr-address (4)
041217        end-if
041217     end-perform

           MOVE '777777777'            TO DR-SUSPENSE (1) (2:9)

           perform 0400-create-freedom thru 0400-exit

           .
       0300-EXIT.
           EXIT.
       0400-CREATE-FREEDOM.

           display ' made it to 0400 '
           ADD 1                       TO WS-REC-GRP-SEQ-NO
           MOVE WS-REC-GRP-CD          TO TR-REC-GRP-CODE
                                          CD-REC-GRP-CODE
                                          DR-REC-GRP-CODE
                                          PR-REC-GRP-CODE
              
           if db-app-status = '5'
              WRITE FREE-MAN-RECORD FROM TRANSACTION-RECORD
           else
              WRITE FREE-NRM-RECORD FROM TRANSACTION-RECORD
           end-if

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  Dist type of E is for Expense                             ***
      ***  Dist type of P is for Payables and the acct # is checking ***
      ***  account number.                                           ***
      ***                                                            ***
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***


           MOVE 'E'                    TO DR-DIST-TYPE (1)
           evaluate dte-client
              when 'AHL'
                 MOVE '1825011300A10000000000'
                                       TO DR-ACCT-NO (1)
062121        when 'FNL'
062121           MOVE '1825011100F10000000000'
062121                                 TO DR-ACCT-NO (1)
              when other
                 MOVE '1825011300020000000000'
                                       TO DR-ACCT-NO (1)
           end-evaluate
           move ch-comp-fin-resp       to dr-suspense (1)              
           if db-app-status = '5'
              WRITE FREE-MAN-RECORD FROM DISTRIBUTION-RECORD
           else
              WRITE FREE-NRM-RECORD FROM DISTRIBUTION-RECORD
           end-if

           MOVE 'P'                    TO DR-DIST-TYPE (1)
062121     evaluate dte-client
062121        when 'AHL'
062121           MOVE '1108121010000000000000'
062121                                 TO DR-ACCT-NO (1)
062121        when 'FNL'
062121           MOVE '1108121010000000000000'
062121                                 TO DR-ACCT-NO (1)
062121        when other
062121           MOVE '1108124700000000000000'
062121                                 TO DR-ACCT-NO (1)
062121     end-evaluate

           MOVE '00'                   TO DR-ACCT-STATE (1)
           MOVE SPACES                 TO DR-SUSPENSE (1)
           if db-app-status = '5'
              WRITE FREE-MAN-RECORD FROM DISTRIBUTION-RECORD
           else
              WRITE FREE-NRM-RECORD FROM DISTRIBUTION-RECORD
           end-if

           if db-app-status = '5'
              WRITE FREE-MAN-RECORD FROM PAYEE-ADDRESS-RECORD
           else
              WRITE FREE-NRM-RECORD FROM PAYEE-ADDRESS-RECORD
           end-if
           
           MOVE WS-TRANSACTION-RECORD  TO TRANSACTION-RECORD
           MOVE WS-CHECK-DES-RECORD    TO CHECK-DES-RECORD
           MOVE WS-DISTRIBUTION-RECORD TO DISTRIBUTION-RECORD
           MOVE WS-PAYEE-ADDRESS-RECORD
                                       TO PAYEE-ADDRESS-RECORD
           
           ADD +1                      TO CT-CURR-LF-PMTS-CNT
           ADD WS-PMT-AMT              TO CT-CURR-LF-PMTS-AMT

           display ' about to write a line '

           MOVE +0                     TO WS-PMT-AMT
           MOVE ' '                    TO WS-VOID-SW

           .
       0400-EXIT.
           EXIT.

       0500-BUILD-ERPYAJ-RECORD.
           display ' made it to 0500-build '

           MOVE SPACES                 TO PENDING-PAY-ADJ
           MOVE 'PY'                   TO PY-RECORD-ID
           MOVE ch-company-cd          TO PY-COMPANY-CD
           move ch-comp-carrier        to py-carrier
           move ch-comp-grouping       to py-grouping
           move ch-comp-fin-resp       to py-fin-resp
           move ch-comp-account        to py-account
           move +0                     to py-file-seq-no

           move 'C'                    to py-record-type
           MOVE CH-AMOUNT-PAID         TO PY-ENTRY-AMT
           move '1825011300'           to py-gl-account
091615     if ch-check-origin-sw = 'C'
091615        move 'CORRECTCHK'        to py-gl-comment
091615     else
091615        move 'REFUND CHK'        to py-gl-comment
091615     end-if
091615*    move 'REFUND CHK'           TO py-gl-comment
           MOVE ch-released-by         TO PY-LAST-MAINT-BY
           MOVE 181000                 TO PY-LAST-MAINT-HHMMSS
      *    MOVE WS-NEXT-CYCLE-BIN-DATE TO PY-LAST-MAINT-DT
      *                                   PY-INPUT-DT
           MOVE WS-CURRENT-BIN-DT      TO PY-LAST-MAINT-DT
                                          PY-INPUT-DT
           MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT
                                          PY-REPORTED-DT
                                          PY-CHECK-WRITTEN-DT
                                          PY-BILLED-DATE
                                          PY-AR-DATE
                                          PY-GL-DATE
           MOVE clasic-credit-eom-dt   TO PY-CREDIT-SELECT-DT
           MOVE 'R'                    TO PY-CHECK-ORIGIN-SW

           .
       0500-write-rec.
           display ' made it to 0500-write '

           add +1 to py-file-seq-no
pemtst*    move '00'                   to erpyaj-file-status
pemtst     write pending-pay-adj
           if erpyaj-file-status = '22'
              go to 0500-write-rec
           else
              if erpyaj-file-status not = '00'
                 display ' error-erpyaj-write ' erpyaj-file-status
                 perform abend-pgm
              end-if
           end-if

           .
       0500-EXIT.
           EXIT.

       0800-DATE-CONVERT.
                                                                        
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
                                                                        
       0800-exit.
           EXIT.                                                        

       WRITE-A-LINE SECTION.         COPY ELCWAL.                    
                                                                     
       WRITE-HEADINGS SECTION.                                       
       WHS-010.                                                      
           display ' made it to whs 010 '
                                                                     
           ADD +1  TO  WS-PAGE.                                      
           MOVE WS-PAGE                TO WS-H3-PAGE
           MOVE PRT                    TO WS-SAVE-PRINT-RECORD
           MOVE ZERO                   TO WS-LINE-COUNT

           MOVE WS-HEADING1            TO PRT
           MOVE '1'                    TO X
           PERFORM WRITE-PRINTER

           MOVE WS-HEADING2            TO PRT
           MOVE ' '                    TO X
           PERFORM WRITE-PRINTER

           MOVE WS-HEADING3            TO PRT
           MOVE ' '                    TO X
           PERFORM WRITE-PRINTER

           MOVE WS-HEADING4            TO PRT
           MOVE ' '                    TO X
           PERFORM WRITE-PRINTER

           MOVE +6                     TO WS-LINE-COUNT

           .
       WHS-020. COPY ELCWHS2.                                        
                                                                     
       WRITE-PRINTER SECTION. COPY ELCWPS.                           
                                                                     
       WPS-020. COPY ELCPRT2.
                                                                     
       abend-pgm section.

           display ' forcing abend '
           call 'ABORTME'
           goback.
