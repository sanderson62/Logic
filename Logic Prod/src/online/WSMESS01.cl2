      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WSMESS01.
       AUTHOR.     Cowtown.
       DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************

020617******************************************************************
020617*REMARKS.                                                        *
020617*  Returns premium calc based on info passed to me.              *
020617******************************************************************
020617*                   C H A N G E   L O G
020617*
020617* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020617*-----------------------------------------------------------------
020617*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020617* EFFECTIVE    NUMBER
020617*-----------------------------------------------------------------
020617* 020617   2017020300002   PEMA  New Program
080117* 080117   2017020300002   PEMA  Tol for tot pmts, use totpmts GP
080817* 080817   2017020300002   PEMA  Limit and ben code assign changes.
040622* 040622 CR2019012500003   PEMA  Migrate to SQLSERVER 2016
061515******************************************************************
       ENVIRONMENT DIVISION.
       data division.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   WSMESS01 WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
      *
      * program buffers
      *
       77  ws-seq-num                  pic s9(8) comp value 0.
       77  ws-flags                    pic s9(8) comp value 0.
       77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
       77  ws-comp-id                  pic xxx value spaces.
       77  WS-SAVE-ACCOUNT             PIC X(10)  VALUE SPACES.
       77  WS-BIN-ORIG-EFF-DT          PIC XX  VALUE LOW-VALUES.
       77  WS-ORIG-EFF-DT              PIC X(10)  VALUE SPACES.
       77  WS-EFF-DATE                 PIC X(10)  VALUE SPACES.
       77  WS-EXP-DATE                 PIC X(10)  VALUE SPACES.
       77  X1                          PIC S999 COMP-3 VALUE +0.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  S2                          PIC S999 COMP-3 VALUE +0.
       77  S3                          PIC S999 COMP-3 VALUE +0.
       77  b1                          pic s999 comp-3 value +0.
       77  WS-BUILD-SW                 PIC X.
           88  TIME-TO-BUILD              VALUE 'Y'.
       77  WS-SAVE-ERACCT              PIC X(2000).
       77  WS-DIS-RESP                 PIC 9(05) VALUE ZEROS.
       77  WS-PERFORM-SW               PIC X VALUE SPACES.
           88  GET-RATES                   VALUE 'R'.
           88  GET-ACT-ACCTS               VALUE 'A'.
       77  ws-bin-current-dt           pic xx  value low-values.
       77  ws-bin-eff-dt               pic xx  value low-values.
       77  ws-bin-exp-dt               pic xx  value low-values.
       77  ws-bin-1st-pmt-dt           pic xx  value low-values.
       77  ws-bin-pri-birth-dt         pic xx  value low-values.
       77  ws-bin-cob-birth-dt         pic xx  value low-values.
       77  WS-DISP-AMT                 PIC Z,ZZZ,Z99.99.
       77  ws-disp-rate                pic z9.99999.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  WS-ERCTBL-SW                PIC X VALUE ' '.
           88  END-OF-ERCTBL                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       77  ws-socket-sw                pic x value ' '.
           88  end-of-socket              value 'Y'.
       77  rec-cnt                     pic 9(5) value zeros.
       77  ws-stop-sw                  pic x value ' '.
           88  i-say-stop                 value 'Y'.
       77  ws-browse-sw                pic x value ' '.
           88  browse-started            value 'Y'.
       77  ws-contract-sw              pic x  value ' '.
           88  contract-no-assigned      value 'Y'.
       77  ws-error-sw                 pic x value ' '.
           88  error-in-one-coverage     value 'Y'.
       77  ws-connect-sw               pic x value ' '.
           88  connected-to-db           value 'Y'.
       77  client-id                   pic xxx.
080817 77  ws-error-sub                pic 999  value zeros.
080817 77  ws-error-sup                pic x(25) value spaces.
080817 77  ws-form-limit-name          pic x(50) value spaces.
080817 77  l1                          pic s999 comp-3 value +0.
080817 77  ws-extension-days           pic 999 value zeros.
       
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

       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  ws-dealer-state             pic xx.
       01  ws-dealer-id                pic x(10).
       01  ws-contract-eff-dt          pic x(10).
       01  ws-contract-no              pic x(10) value spaces.
       01  ws-contract-suffix          pic x     value spaces.


       01  ws-key-stuff.
           05  ws-ks-contract-no       pic x(10) value spaces.
           05  ws-batch-no             pic x(6) value zeros.
           05  ws-batch-no-n redefines
               ws-batch-no             pic 9(6).
           05  ws-check-key            pic 9(7).
           05  ws-check-no             pic x(7).
           05  ws-compid               pic xxx.
           05  ws-carrier              pic x.
           05  ws-grouping             pic x(6).
           05  ws-state                pic xx.
           05  ws-account              pic x(10).
           05  ws-eff-date             pic x(10).
           05  ws-certificate          pic x(10).
           05  ws-cert-sfx             pic x.
           05  ws-seq-no               pic 999.
           05  ws-type                 pic 999.

       01  ws-status-code-a            pic x(7) value zeros.
       01  ws-status-code redefines
           ws-status-code-a            pic 9(7).
       01  ws-error-message            pic x(50) value spaces.
       01  ws-status-date              pic x(10).
       01  sqlcmd                      pic x(1024).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is        null.           The indicator will be -1        ***
      ***  if the value        is null  and +0 if the value is       ***
      ***  something other than null.  Here is an example on how     ***
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
      ***           OR This way on an update                         ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        UPDATE                                              ***
      ***           CUC_Logic_Remittance                             ***
      ***        SET                                                 ***
      ***           LogicStatus     = :ws-status-code,               ***
      ***           LogicStatusDate = :ws-status-date,               ***
      ***           BatchNumber     = :ws-batch-no :nu-batchno       ***
      ***        WHERE                                               ***
      ***           RemitId = :ws-remit-id                           ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***    Also, when the table has a column with a data type of   ***
      ***  "BIT" and used as true/false move the 1 byte receiving    ***
      ***  field to ws-bit and check out ws-bit-comp. if = zeros,    ***
      ***  then its false. I think true would be 256.                ***
      ***                                                            ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  ws-bit                      pic x.
       01  ws-bit-comp redefines ws-bit pic s9(4) comp.
       01  indicator-vaiables-for-nulls.
           05  nu-app-status           pic s9(4) comp value +0.
           05  nu-app-by               pic s9(4) comp value +0.
           05  nu-app-date             pic s9(4) comp value +0.
           05  nu-app-batch            pic s9(4) comp value +0.
           05  nu-fincar               pic s9(4) comp value +0.
           05  nu-batchno              pic s9(4) comp value +0.
           05  nu-error-message        pic s9(4) comp value +0.

       01  sql-cert-records.
           05  sql-dlr-state           pic xx.
           05  sql-dlr-id              pic x(10).
           05  sql-eff-dt              pic x(10).
           05  sql-contr-no            pic x(10).
           05  sql-contr-suffix        pic x.

080817 01  ws-limit-issue-age          pic 999.
080817 01  ws-att-age                  pic 999.
080817 01  form-limit-table.
080817     05  ws-limit-name           pic x(50).
080817     05  ws-limit-cov-type       pic xx.
080817     05  ws-limit-lo-age         pic 999.
080817     05  ws-limit-hi-age         pic 999.
080817     05  ws-limit-att-age        pic 999.
080817     05  ws-limit-max-term       pic 999.
080817     05  ws-limit-max-jnt-term   pic 999.
080817     05  ws-limit-max-mo-ben     pic 9(5).
080817     05  ws-limit-max-tot-ben    pic 9(7).
080817
080817 01  state-benefit-code-table.
080817     05  ws-sbc-state            pic xx.
080817     05  ws-sbc-cov-type         pic xx.
080817     05  ws-sbc-sin-jnt          pic x.
080817     05  ws-sbc-dismember        pic x.
080817     05  ws-sbc-retroelim        pic x.
080817     05  ws-sbc-wait-days        pic 999.
080817     05  ws-sbc-max-bens         pic 999.
080817     05  ws-sbc-logic-ben-code   pic xx.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  filler.
           05  ws-work-in              pic x(10).
           05  ws-work-out             pic x(10).
           05  ws-work-out-v2 redefines
               ws-work-out             pic 9(8)v99.
           05  ws-work-out-v0 redefines
               ws-work-out             pic 9(10).
           05  ws-work-out-v5 redefines
               ws-work-out             pic 9(5)v9(5).
       01  filler.
           05  ws-last-suffix          pic x value low-values.
           05  ws-tbl-last-suffix      pic x value low-values.
           05  filler.  *> Use X1 for this table.
               10  ws-codes            pic x(26) value
               ' ABCDEFGHIJKLMNOPQRSTUVWXY'.
               10  ws-suffix-value redefines ws-codes
                 occurs 26             pic x.

       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).

       01  raw-message001-area.
           05  raw-message-num         pic x(10).
           05  raw-state               pic xx.
           05  raw-acct-no             pic x(10).
           05  raw-vin                 pic x(17).
           05  raw-lf-ah               pic x.
           05  raw-ben-code            pic xx.
           05  raw-earn-meth           pic x.
           05  raw-pri-birth-date      pic x(10).
           05  raw-cob-birth-date      pic x(10).
           05  raw-benefit             pic x(9).
           05  raw-eff-date            pic x(10).
           05  raw-1st-pmt-dt          pic x(10).
           05  raw-premium             pic x(8).
           05  raw-loan-term           pic xxx.
           05  raw-ins-term            pic XXX.
           05  raw-apr                 pic x(8).
           05  raw-sin-jnt-ind         pic x.
           05  raw-dismemberment       pic x.
           05  raw-retro-elim          pic x.
           05  raw-waiting-days        pic xx.
           05  raw-crit-per            pic xx.
           05  raw-total-payments      pic x(9).
           05  raw-period-payment      pic x(8).

       01  ws-rate-work-area.
           05  ws-rate-state           pic xx.
           05  ws-rate-acct-no         pic x(10).
           05  ws-rate-vin             pic x(17).
           05  ws-rate-lf-ah           pic x.
           05  ws-rate-ben-code        pic xx.
           05  ws-rate-earn-meth       pic x.
           05  ws-rate-pri-birth-date  pic x(10).
           05  ws-rate-cob-birth-date  pic x(10).
           05  ws-rate-eff-date        pic x(10).
           05  ws-rate-1st-pmt-dt      pic x(10).
           05  ws-rate-benefit-type    pic x.
           05  ws-rate-benefit-cd      pic xx.
           05  ws-rate-benefit         pic 9(6)v99.
071017     05  ws-rate-tot-pmts        pic 9(6)v99.
           05  ws-rate-premium         pic 9(5)v99.
           05  ws-rate-apr             pic 99v9(5).
           05  ws-rate-loan-term       pic 999  value zeros.
           05  ws-rate-ins-term        pic 999  value zeros.
           05  ws-issue-age            pic 999  value zeros.
           05  ws-cob-age              pic 999  value zeros.
           05  ws-rate-age             pic 999  value zeros.
           05  ws-max-benefit          pic 9(7)v99 value zeros.
           05  ws-rate-crit-per        pic 99.
           05  ws-rate-sin-jnt-ind     pic x.
           05  ws-rate-dismemberment   pic x.
           05  ws-rate-retro-elim      pic x.
           05  ws-rate-waiting-days    pic 99.
071017     05  ws-rate-payment         pic 9(5)v99.
071017     05  ws-calc-tot-pmts        pic 9(7)v99 value zeros.

       01  WS-AM-KEY.
           05  WS-AM-COMPANY-CD        PIC X.                                       
           05  WS-AM-CARRIER           PIC X.                                       
           05  WS-AM-GROUP             PIC X(6).                                    
           05  WS-AM-STATE             PIC XX.   
           05  WS-AM-ACCOUNT           PIC X(10).
           05  WS-AM-EXP-DT            PIC XX.
           05  FILLER                  PIC X(4).

       01  ws-cm5-compare-key          pic x(12).
       01  WS-CM5-KEY.
           05  WS-CM5-COMPANY-CD       PIC X.                                       
           05  WS-CM5-CERT-NO          PIC X(11).

       01  ws-cm-compare-key           pic x(33).
       01  WS-CM-KEY.
           05  WS-CM-COMPANY-CD        PIC X.                                       
           05  WS-CM-CARRIER           PIC X.                                       
           05  WS-CM-GROUP             PIC X(6).                                    
           05  WS-CM-STATE             PIC XX.   
           05  WS-CM-ACCOUNT           PIC X(10).
           05  WS-CM-EFF-DT            PIC XX.
           05  WS-CM-CERT-NO.
               10  ws-cm-cert-ten      pic x(10).
               10  ws-cm-cert-suffix   pic x.

       01  WS-CS-KEY.
           05  WS-CS-COMPANY-CD        PIC X.                                       
           05  WS-CS-CARRIER           PIC X.                                       
           05  WS-CS-GROUP             PIC X(6).                                    
           05  WS-CS-STATE             PIC XX.   
           05  WS-CS-ACCOUNT           PIC X(10).
           05  WS-CS-EFF-DT            PIC XX.
           05  WS-CS-CERT-NO           PIC X(11).
           05  WS-CS-TRLR-TYPE         PIC X.

       01  WS-AM-ALT-KEY.
           05  WS-AM-ALT-ACCOUNT       PIC X(10).
           05  WS-AM-ALT-EXP-DT        PIC XX.

       01  WS-CF-KEY-SAVE              PIC X(10).
       01  WS-CF-KEY.
           05  WS-CF-COMPANY-ID        PIC XXX.
           05  WS-CF-RECORD-TYPE       PIC X.
           05  WS-CF-ACCESS            PIC X(4).
           05  WS-CF-SEQ-NO            PIC S9(4) COMP.
           
       01  filler.
           05  ws-errors-table.                                           
               10  filler              pic x(50) value                    
               '0000Transaction successfully completed'.                  
               10  filler              pic x(50) value                    
               '0101Problem with amounts'.                                
               10  filler              pic x(50) value                    
               '0102Problem with dates'.                                  
               10  filler              pic x(50) value                    
               '0103Issue age outside limits'.                            
               10  filler              pic x(50) value                    
               '0104Attained age outside limits'.                         
               10  filler              pic x(50) value                    
               '0105Term is not valid must be gt 0 and lt 361'.           
               10  filler              pic x(50) value                    
               '0106Term is outside limits'.                              
               10  filler              pic x(50) value                    
               '0107Amount is outside limits'.                            
               10  filler              pic x(50) value                    
               '0108Loan term must be ge insurance term'.                 
               10  filler              pic x(50) value                    
               '0109Total benefit is outside limits'.                     
               10  filler              pic x(50) value                    
               '0110Rates found were zero'.                               
               10  filler              pic x(50) value                    
               '0111Rate table was not found'.                            
               10  filler              pic x(50) value                    
               '0112Rate file not open'.                                  
               10  filler              pic x(50) value                    
               '0113Premium is out of tolerence'.                         
               10  filler              pic x(50) value                    
               '0114Duplicate contract and VIN on Logic'.                 
               10  filler              pic x(50) value                    
               '0115Problem with loan or insurance term'.                 
071017         10  filler              pic x(50) value                    
071017         '0116Total of payments incorrect '.                        
080817         10  filler              pic x(50) value                    
080817         '0117No Limits or age outside limits'.                     
080817         10  filler              pic x(50) value                    
080817         '0118Life benefit code not found '.                        
080817         10  filler              pic x(50) value                    
080817         '0119Disab benefit code not found '.                       
071017     05  filler redefines ws-errors-table occurs 20.
               10  ws-table-error-no   pic x(4).
               10  ws-table-error-mess pic x(46).

       01  ws-return-string.
           05  ws-return-error-no      pic x(4).
           05  ws-sc1                  pic x.
           05  ws-return-error-mess    pic x(46).
           05  ws-sc2                  pic x.
           05  ws-return-contract-no   pic x(11).
           05  ws-sc3                  pic x.
           05  ws-return-max-ben       pic z,zzz,z99.99.
           05  ws-sc4                  pic x.
           05  ws-return-prem          pic z,zzz,z99.99.
           05  ws-sc5                  pic x.
           05  ws-return-rate          pic z9.99999.
           05  ws-sc6                  pic x.
           05  ws-return-exp-dt        pic x(10).
           05  ws-sc7                  pic x.
           05  ws-return-benefit-cd    pic xx.

       01  ws-mess001-length           pic s9(4) comp value +1024.
       01  WS-MESS001-PASS-AREA        PIC X(1024).
       01  WS-CID-NO                   PIC X(8).

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  resp-duprec                  value +14.
           88  resp-dupkey                  value +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

                                        COPY ERCACCT.
                                        COPY ELCCERT.
                                        COPY ELCCRTT.
                                        COPY ELCCNTL.
                                        COPY ELCFUNDT.
                                        COPY ELCDATE.
                                        COPY ELCCALC.
      
       linkage section.
       01  DFHCOMMAREA                 pic x(1024).

       01  var  pic x(30).

       procedure division.

           display ' Entering program WSMESS01 '
           move dfhcommarea            to ws-mess001-pass-area

           perform 0000-init           thru 0000-exit

           display ' mess001 area ' ws-mess001-pass-area
           perform 0010-process-message
                                       thru 0010-exit
           display ' raw after unstring ' raw-message001-area
      *    move ' returning string goes here ' to ws-return-string

           go to 0300-RETURN-CICS

           .
       0000-init.

           move 'CID'                  to client-id

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           move ws-fn-cymd             to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-current-dt
           else
              display ' error current dt invalid ' dc-error-code
           end-if

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

           evaluate client-id
              when 'DCC'
                 MOVE X'05'            TO WS-COMP-CD
                 MOVE 'DCC'            TO WS-COMP-ID
              when 'CID'
                 MOVE X'04'            TO WS-COMP-CD
                 MOVE 'CID'            TO WS-COMP-ID
              when 'AHL'
                 MOVE X'06'            TO WS-COMP-CD
                 MOVE 'AHL'            TO WS-COMP-ID
              when other
                 display ' Invalid company id ' client-id
                 move 18               to ws-error-sub
                 move client-id        to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
           END-evaluate
080817     move spaces                 to ws-return-string
080817     move ';'                    to ws-sc1
080817                                    ws-sc2
080817                                    ws-sc3
080817                                    ws-sc4
080817                                    ws-sc5
080817                                    ws-sc6
080817                                    ws-sc7

           .
       0000-exit.
           exit.

       0010-process-message.

           perform 0110-unstring       thru 0110-exit
           perform 0120-format-message thru 0120-exit

           perform 0020-edit-received  thru 0020-exit
           perform 0025-assign-ben-cd  thru 0025-exit

           perform 0050-get-account    thru 0050-exit
           if ws-rate-lf-ah = 'L'
              perform 0090-check-qc-lf-limits
                                       thru 0090-exit
           else
              perform 0095-check-qc-ah-limits
                                       thru 0095-exit
           end-if                                       

           perform 0030-get-rate       thru 0030-exit
           if (contract-no-assigned)
                  or
              (error-in-one-coverage)
              continue
           else
              perform 0060-check-for-dup
                                       thru 0060-exit
              perform 0070-open-cursor thru 0070-exit
              perform 0080-process-input
                                       thru 0080-exit
           end-if
           perform 0040-format-buffer  thru 0040-exit
           set contract-no-assigned to true

           .
       0010-exit.
           exit.

       0020-edit-received.

           if ws-rate-sin-jnt-ind = 'S' OR 'J'
              continue
           else
              move 'S'                 to ws-rate-sin-jnt-ind
           end-if
           if ws-rate-dismemberment = 'Y'
              continue
           else
              move 'N'                 to ws-rate-dismemberment
           end-if
           if ws-rate-lf-ah = 'A'
              if ws-rate-retro-elim = 'R' or 'E'
                 continue
              else
                 move 'R'              to ws-rate-retro-elim
              end-if
           else
              move spaces              to ws-rate-retro-elim
           end-if

           if ws-rate-ins-term = zeros
              move '0115;Problem with insurance term '
                                       to ws-return-string
              go to 0300-RETURN-CICS
           end-if

           if ws-rate-lf-ah = 'L'
              move 'L'                 to ws-rate-benefit-type
           else
              move 'A'                 to ws-rate-benefit-type
           end-if

      **==============================================================**
      **                                                              **
      **  Validate passed effective date. Probably should add edits   **
      **                                                              **
      **==============================================================**

           move ws-rate-eff-date (7:4) to ws-work-date (1:4)
           move ws-rate-eff-date (1:2) to ws-work-date (5:2)
           move ws-rate-eff-date (4:2) to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-eff-dt
           else
              move 3                   to ws-error-sub
              move '- Eff Date'        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

           if ws-bin-eff-dt > ws-bin-current-dt
              move 3                   to ws-error-sub
              move '- Future Eff'      to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Validate passed 1st pmt date.   Probably should add edits   **
      **                                                              **
      **==============================================================**

           move ws-rate-1st-pmt-dt (7:4)
                                       to ws-work-date (1:4)
           move ws-rate-1st-pmt-dt (1:2)
                                       to ws-work-date (5:2)
           move ws-rate-1st-pmt-dt (4:2)
                                       to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-1st-pmt-dt
           else
              move low-values          to ws-bin-1st-pmt-dt
              move 3                   to ws-error-sub
              move '- 1st Pmt Dt'      to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

           if ws-bin-1st-pmt-dt <= ws-bin-eff-dt
              move low-values          to ws-bin-1st-pmt-dt
              move 3                   to ws-error-sub
              move '- 1st Pmt !> Eff dt' to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Only use the code below if there is a time they don't       **
      **  send the 1st pmt date.                                      **
      **                                                              **
      **==============================================================**

           if ws-bin-1st-pmt-dt = low-values
              move +1                  to dc-elapsed-months
              move +0                  to dc-elapsed-days
              move '6'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-bin-date-2    to ws-bin-1st-pmt-dt
              else
                 move 3                to ws-error-sub
                 move '- 1st Pmt Dt'   to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate the expiration date to be passed back.            **
      **                                                              **
      **==============================================================**

           move ws-bin-1st-pmt-dt      to dc-bin-date-1
           compute dc-elapsed-months = ws-rate-ins-term - 1
           move +0                     to dc-elapsed-days
           move '6'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-2       to ws-bin-exp-dt
              move dc-greg-date-a-edit to ws-exp-date
           else
              move 3                   to ws-error-sub
              move '- Expire Dt'       to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Validate primary borrower birth date.                       **
      **                                                              **
      **==============================================================**

           move ws-rate-pri-birth-date (7:4)
                                       to ws-work-date (1:4)
           move ws-rate-pri-birth-date (1:2)
                                       to ws-work-date (5:2)
           move ws-rate-pri-birth-date (4:2)
                                       to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-pri-birth-dt
           else
              move 3                   to ws-error-sub
              move '- Pri DOB '        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Validate co borrowers birth date.                           **
      **                                                              **
      **==============================================================**

           if ws-rate-cob-birth-date (7:4) = spaces
              move zeros               to ws-rate-cob-birth-date
           end-if
           move ws-rate-cob-birth-date (7:4)
                                       to ws-work-date (1:4)
           move ws-rate-cob-birth-date (1:2)
                                       to ws-work-date (5:2)
           move ws-rate-cob-birth-date (4:2)
                                       to ws-work-date (7:2)
           move low-values             to ws-bin-cob-birth-dt
           if ws-work-date-num not = zeros
              move ws-work-date-num    to dc-greg-date-cymd
              move 'L'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-bin-date-1    to ws-bin-cob-birth-dt
              else
                 move 3                to ws-error-sub
                 move '- Cob DOB '     to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate primary borrowers age.                            **
      **                                                              **
      **==============================================================**

           move ws-bin-pri-birth-dt    to dc-bin-date-1
           move ws-bin-eff-dt          to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              compute ws-issue-age = dc-elapsed-months / +12
           else
              move 3                   to ws-error-sub
              move '- Pri DOB '        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate co borrowers age.                                 **
      **                                                              **
      **==============================================================**

           move zeros                  to ws-cob-age
           if ws-bin-cob-birth-dt not = low-values
              move ws-bin-cob-birth-dt to dc-bin-date-1
              move ws-bin-eff-dt       to dc-bin-date-2
              move '1'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 compute ws-cob-age = dc-elapsed-months / +12
              else
                 move 3                to ws-error-sub
                 move '- Cob DOB '     to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if

           move ws-issue-age           to ws-rate-age
           if ws-cob-age > ws-rate-age
              move ws-cob-age          to ws-rate-age
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate extension days.                                   **
      **                                                              **
      **==============================================================**

           move zeros                  to ws-extension-days
           move ws-bin-eff-dt          to dc-bin-date-1
           move ws-bin-1st-pmt-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              if dc-elapsed-days > 31
                 compute ws-extension-days =
                    dc-elapsed-days - 30
              end-if
           end-if

      **==============================================================**
      **                                                              **
      **  Validate passed total of payments. loan term * payment      **
      **                                                              **
      **==============================================================**

           compute ws-calc-tot-pmts =
              ws-rate-loan-term * ws-rate-payment

           if ((ws-calc-tot-pmts - ws-rate-tot-pmts) > 1.00)
                           or
              ((ws-rate-tot-pmts - ws-calc-tot-pmts) > 1.00)
      *    if ws-calc-tot-pmts <> ws-rate-tot-pmts
              move 17                  to ws-error-sub
              move spaces              to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

           .
       0020-exit.
           exit.

       0025-assign-ben-cd.

080817     if not connected-to-db
080817        perform 6000-connect-to-db thru 6000-exit
080817     end-if
080817
080817     if ws-rate-benefit-type = 'A'
080817        go to 0025-get-ah-ben
080817     end-if
080817
080817     move ws-rate-state          to ws-sbc-state
080817     evaluate true
080817        when raw-ben-code (1:1) = 'N'
080817           move 'NP'             to ws-sbc-cov-type
080817        when raw-ben-code (1:1) = 'T'
080817           move 'NT'             to ws-sbc-cov-type
080817        when raw-ben-code (1:1) = 'L'
080817           move 'LL'             to ws-sbc-cov-type
080817        when other
080817           move 'GP'             to ws-sbc-cov-type
080817     end-evaluate
080817
080817     move ws-rate-sin-jnt-ind    to ws-sbc-sin-jnt
080817     move ws-rate-dismemberment  to ws-sbc-dismember
080817
080817     EXEC SQL
080817        SELECT
080817           LogicBenCode
080817        INTO
080817           :ws-sbc-logic-ben-code
080817        FROM
080817           BenefitCodeMapping
080817        WHERE
080817           State           = :ws-sbc-state
080817           and CovType     = :ws-sbc-cov-type
080817           and SinJnt      = :ws-sbc-sin-jnt
080817           and Dismem      = :ws-sbc-dismember
080817     end-exec
080817
080817     if sqlcode not = 0
080817        display "Error: cannot find lf benefit code "
080817           ws-sbc-state ' ' ws-sbc-cov-type ' ' ws-sbc-sin-jnt
080817           ' ' ws-sbc-dismember
080817        display ' sql return code ' sqlcode
080817        display ' sql err mess    ' sqlerrmc
080817        move 19                  to ws-error-sub
080817        move spaces              to ws-error-sup
080817        string
080817           ws-sbc-state ' '
080817           ws-sbc-cov-type ' '
080817           ws-sbc-sin-jnt ' '
080817           ws-sbc-dismember
080817           delimited by size into ws-error-sup
080817        perform 0180-error-handle
080817                                 thru 0180-exit
080817        go to 0300-return-cics
080817     end-if
080817
080817     move ws-sbc-logic-ben-code  to ws-rate-benefit-cd
080817     go to 0025-exit
080817
080817     .
080817 0025-get-ah-ben.
080817
080817     move ws-rate-state          to ws-sbc-state
080817     move 'DI'                   to ws-sbc-cov-type
080817     move ws-rate-sin-jnt-ind    to ws-sbc-sin-jnt
080817     move ws-rate-retro-elim     to ws-sbc-retroelim
080817     move ws-rate-waiting-days   to ws-sbc-wait-days
080817     move ws-rate-crit-per       to ws-sbc-max-bens
080817
080817     EXEC SQL
080817        SELECT
080817           LogicBenCode
080817        INTO
080817           :ws-sbc-logic-ben-code
080817        FROM
080817           BenefitCodeMapping
080817        WHERE
080817           State           = :ws-sbc-state
080817           and CovType     = :ws-sbc-cov-type
080817           and SinJnt      = :ws-sbc-sin-jnt
080817           and RetroElim   = :ws-sbc-retroelim
080817           and WaitDays    = :ws-sbc-wait-days
080817           and MaxBens     = :ws-sbc-max-bens
080817     end-exec
080817
080817     if sqlcode not = 0
080817        display "Error: cannot find ah benefit code "
080817           ws-sbc-state ' ' ws-sbc-cov-type ' ' ws-sbc-sin-jnt
080817           ' ' ws-sbc-retroelim ' ' ws-sbc-wait-days ' '
080817           ws-sbc-max-bens
080817
080817        display ' sql return code ' sqlcode
080817        display ' sql err mess    ' sqlerrmc
080817        move 20                  to ws-error-sub
080817        move spaces              to ws-error-sup
080817        string
080817           ws-sbc-state ' '
080817           ws-sbc-cov-type ' '
080817           ws-sbc-sin-jnt ' '
080817           ws-sbc-retroelim ' '
080817           ws-sbc-wait-days ' '
080817           ws-sbc-max-bens
080817           delimited by size into ws-error-sup
080817        perform 0180-error-handle
080817                                 thru 0180-exit
080817        go to 0300-return-cics
080817     end-if
080817
080817     move ws-sbc-logic-ben-code  to ws-rate-benefit-cd

           .
       0025-exit.
           exit.

       0030-get-rate.

      *    move spaces                 to calculation-pass-area
           move zeros                  to cp-r-max-mon-ben
                                          cp-r-max-tot-ben
                                          cp-rate-dev-pct
                                          cp-original-premium
                                          cp-critical-months

           move ws-extension-days      to cp-term-or-ext-days
           move am-cal-table           to cp-class-CODE

           if ws-rate-benefit-type = 'L'
              move am-lf-deviation     to cp-deviation-code
              if raw-ben-code (1:1) = 'N' or 'T'
                 move ws-rate-benefit  to cp-original-benefit
                                          cp-rating-benefit-amt
              else
                 move ws-rate-tot-pmts to cp-original-benefit
                                          cp-rating-benefit-amt
              end-if
           else
              move am-ah-deviation     to cp-deviation-code
              move ws-rate-benefit     to cp-original-benefit
                                          cp-rating-benefit-amt
           end-if

           move ws-rate-state          to cp-state
                                          cp-state-std-abbrv
           move ws-rate-benefit-type   to cp-benefit-type
           move ws-rate-benefit-cd     to cp-benefit-cd
           move ws-rate-age            to cp-issue-age
           move ws-comp-id             to cp-company-id
           move ws-comp-cd             to cp-company-cd

           move ws-rate-apr            to cp-loan-apr

           if ws-rate-loan-term = zeros
              move ws-rate-ins-term    to ws-rate-loan-term
           end-if
           move ws-rate-ins-term       to cp-original-term
           move ws-rate-loan-term      to cp-loan-term

           move 'L'                    to cp-life-override-code
           move 'A'                    to cp-ah-override-code
           move '3'                    to cp-process-type
           move ws-rate-earn-meth      to cp-earning-method
                                          cp-rating-method
           move 'R'                    to cp-rate-file
           move ws-bin-eff-dt          to cp-cert-eff-dt
           move ws-bin-1st-pmt-dt      to cp-first-pay-date
           
           PERFORM 7000-GET-RATE       THRU 7000-EXIT

           if (no-cp-error)
              or (cp-return-code = '8' or '9' or 'A' or 'B' or 'C')
              move '0'                 to cp-return-code
              if ((cp-calc-premium - ws-rate-premium) > 9.99)
                              or
                 ((ws-rate-premium - cp-calc-premium) > 9.99)
                 move 'Z'              to cp-return-code
              end-if
           end-if

      **==============================================================**
      **                                                              **
      ** The below code will change as we add more states and forms   **
      ** to this process.                                             **
      **                                                              **
      **==============================================================**
           move cp-original-benefit    to ws-max-benefit

           .
       0030-exit.
           exit.

       0040-format-buffer.

           move spaces                 to ws-return-string

           perform 0045-format-error   thru 0045-exit
           move ';'                    to ws-sc1
                                          ws-sc2
                                          ws-sc3
                                          ws-sc4
                                          ws-sc5
                                          ws-sc6
                                          ws-sc7
           if no-cp-error
              if ws-contract-suffix = spaces or low-values
                 move ws-contract-no   to ws-return-contract-no (2:10)
              else
                 move ws-contract-no   to ws-return-contract-no (1:10)
                 move ws-contract-suffix
                                       to ws-return-contract-no (11:1)
              end-if
              move cp-calc-premium     to ws-return-prem
              move cp-premium-rate     to ws-return-rate
              move ws-max-benefit      to ws-return-max-ben
              move ws-exp-date         to ws-return-exp-dt
              move ws-rate-benefit-cd  to ws-return-benefit-cd
           else
              set error-in-one-coverage to true
              move spaces              to ws-contract-no
                                          ws-contract-suffix
           end-if

           .
       0040-exit.
           exit.

       0045-format-error.

           evaluate true
              when cp-return-code = '0'
                 move +1               to s1
              when cp-return-code = '1'
                 move +2               to s1
              when cp-return-code = '2'
                 move +3               to s1
              when cp-return-code = 'A'
                 move +4               to s1
              when cp-return-code = 'B'
                 move +5               to s1
              when cp-return-code = '4'
                 move +6               to s1
              when cp-return-code = '9'
                 move +7               to s1
              when cp-return-code = '8'
                 move +8               to s1
              when cp-return-code = 'H'
                 move +9               to s1
              when cp-return-code = 'C'
                 move +10              to s1
              when cp-return-code = '7'
                 move +11              to s1
              when cp-return-code = '6'
                 move +12              to s1
              when cp-return-code = 'D'
                 move +13              to s1
              when cp-return-code = 'Z'
                 move +14              to s1
                 move cp-calc-premium  to ws-return-prem
                 move cp-premium-rate  to ws-return-rate
                 move ws-max-benefit   to ws-return-max-ben
                 move ws-exp-date      to ws-return-exp-dt
                 move ws-rate-benefit-cd to ws-return-benefit-cd
              when cp-return-code = 'Y'
                 move +15              to s1
                 move cp-calc-premium  to ws-return-prem
                 move cp-premium-rate  to ws-return-rate
                 move ws-max-benefit   to ws-return-max-ben
                 move ws-exp-date      to ws-return-exp-dt
                 move ws-rate-benefit-cd to ws-return-benefit-cd
           end-evaluate

           move ws-table-error-no (s1) to ws-return-error-no
           move ws-table-error-mess (s1)
                                       to ws-return-error-mess

           .
       0045-exit.
           exit.
           
       0050-get-account.

           move ws-comp-cd             to ws-am-company-cd
           move '9'                    to ws-am-carrier
           move '000000'               to ws-am-group
           move ws-rate-state          to ws-am-state
           move ws-rate-acct-no        to ws-am-account
           move ws-bin-eff-dt          to ws-am-exp-dt

           exec cics read
              dataset      ('ERACCT')
              ridfld       (ws-am-key)
              into         (account-master)
              GTEQ
              resp         (ws-response)
           end-exec

           if resp-normal
              and (ws-comp-cd = am-company-cd)
              and (ws-rate-state = am-state)
              and (ws-rate-acct-no = am-account)
              and (ws-bin-eff-dt >= am-effective-dt)
              and (ws-bin-eff-dt < am-expiration-dt)
              continue
           else
              move '0114;No account mstr found ' to ws-return-string
              go to 0300-RETURN-CICS
           end-if

080817     move am-comment-line (1)    to ws-form-limit-name
080817     move spaces                 to ws-limit-name
080817     perform varying l1 from +50 by -1 until
080817        (l1 < +1)
080817        or (ws-form-limit-name (l1:1) <> ' ')
080817     end-perform
080817     if l1 > +3
080817        perform varying l1 from l1 by -1 until
080817           (l1 < +1)
080817           or (ws-form-limit-name (l1:1) = ' ')
080817        end-perform
080817        if l1 > +3
080817           subtract +1 from l1
080817           move ws-form-limit-name (1:l1)
080817                                 to ws-limit-name
080817        end-if
080817     end-if

           .
       0050-exit.
           exit.

       0060-check-for-dup.

      **==============================================================**
      **                                                              **
      **    All i'm going to do here is check for a dup using         **
      **  the state, account, eff dt and last six of vin and a space  **
      **  in the cert suffix. If I do find one then I will have to    **
      **  find the last suffix and use the next available one in the  **
      **  suffix table.                                               **
      **                                                              **
      **==============================================================**

           move +1                     to x1
           move ' '                    to ws-browse-sw

           move ws-comp-cd             to ws-cm-key
           move '9'                    to ws-cm-carrier
           move '000000'               to ws-cm-group
           move ws-rate-state          to ws-cm-state
           move ws-rate-acct-no        to ws-cm-account
           move ws-bin-eff-dt          to ws-cm-eff-dt
           string
              '0000'
              ws-rate-vin (12:6)
              ' ' delimited by size into ws-cm-cert-no
           end-string
           move ws-cm-key              to ws-cm-compare-key
           move ws-cm-cert-ten         to ws-contract-no
           move low-values             to ws-last-suffix

           exec cics startbr
              dataset      ('ELCERT')
              ridfld       (ws-cm-key)
              gteq 
              resp         (ws-response)
           end-exec

           evaluate true
              when resp-normal
                 set browse-started to true
              when resp-notfnd or resp-endfile
                 set i-say-stop to true
              when other
                 move ws-response to ws-disp-resp
                 string
                    '9999' ';'
                    'Bad elcert startbr ' ';'
                    ws-disp-resp delimited by size
                                       into ws-return-string
                 end-string
                 display ' something went wrong with start br '
                    ws-response
                 go to 0300-RETURN-CICS
           end-evaluate

           perform until i-say-stop
              exec cics readnext
                 dataset      ('ELCERT')
                 ridfld       (ws-cm-key)
                 into         (certificate-master)
                 resp         (ws-response)
              end-exec
              if resp-normal
                 if ws-cm-key (1:32) = ws-cm-compare-key (1:32)
                    move ws-cm-cert-suffix
                                       to ws-last-suffix
                 else
                    set i-say-stop to true
                 end-if
              else
                 set i-say-stop to true
              end-if
           end-perform

           if browse-started
              exec cics endbr
                 dataset   ('ELCERT')
              END-EXEC
           end-if

           if ws-last-suffix not = low-values
              perform varying x1 from +1 by +1 until
                 (x1 > +26)
                 or (ws-last-suffix = ws-suffix-value (x1))
              end-perform
              if x1 < +27
                 move ws-suffix-value (x1 + 1)
                                       to ws-contract-suffix
              else
                 display ' more than 26 suffix codes ' ws-last-suffix
                 move 15               to ws-error-sub
                 move spaces           to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if

           .
       0060-exit.
           exit.

       0070-open-cursor.

pemtst*    display ' declare cursor ' ws-begin-dt ' ' ws-end-dt

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  The dates on the sql table have values in the time        ***
      ***  so I convert it to a string and just use mm/dd/yyyy       ***
      ***  to perform the comparison.                                ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if

           move ws-rate-state          to ws-dealer-state
           move ws-rate-acct-no        to ws-dealer-id
           move ws-rate-eff-date       to ws-contract-eff-dt
           move zeros                  to ws-ks-contract-no
           move ws-rate-vin (12:6)     to ws-ks-contract-no (5:6)
           
           EXEC SQL
              DECLARE
                 contracts cursor for
              SELECT
                 DlrState,
                 DlrId,
                 EffDt,
                 ContractNo,
                 ContractSuffix
              FROM
                 PendingContracts
              WHERE
                 DlrState     = :ws-dealer-state
                 and DlrId    = :ws-dealer-id
                 and EffDt    = :ws-contract-eff-dt
                 and ContractNo  = :ws-ks-contract-no
              ORDER BY
                 dlrstate,
                 dlrid,
                 effdt,
                 contractno,
                 contractsuffix
           end-exec

           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 0070-exit
           end-if

           EXEC SQL
              open contracts
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 0070-exit
           end-if

           .
       0070-exit.
           exit.
       0080-process-input.

           perform until sqlcode not = 0
              EXEC SQL
                 fetch contracts into
                    :sql-dlr-state,
                    :sql-dlr-id,
                    :sql-eff-dt,
                    :sql-contr-no,
                    :sql-contr-suffix
              END-EXEC

              if sqlcode = 0
                 move sql-contr-suffix to ws-tbl-last-suffix
              else
                 if sqlcode not = 0 and 100
                    display "Error: cannot fetch row " 
                    display ' sql return code ' sqlcode
                    display ' sql err mess    ' sqlerrmc
                 end-if
              end-if
           end-perform

           if ws-tbl-last-suffix not = low-values
              and (ws-tbl-last-suffix > ws-last-suffix)
              perform varying x1 from +1 by +1 until
                 (x1 > +26)
                 or (ws-tbl-last-suffix = ws-suffix-value (x1))
              end-perform
              if x1 < +27
                 move ws-suffix-value (x1 + 1)
                                       to ws-contract-suffix
              else
                 display ' more than 26 suffix codes ' ws-last-suffix
                 move 15               to ws-error-sub
                 move spaces           to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if

           EXEC SQL
               close contracts
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0080-exit.
           exit.

080817 0090-check-qc-lf-limits.
080817
080817     if not connected-to-db
080817        perform 6000-connect-to-db thru 6000-exit
080817     end-if
080817
080817     if ws-rate-ben-code (1:1) = 'L'
080817        move 'LL'                to ws-limit-cov-type
080817     else
080817        move 'RL'                to ws-limit-cov-type
080817     end-if
080817
080817     move ws-rate-age            to ws-limit-issue-age
080817
080817     EXEC SQL
080817        SELECT distinct top 1
080817           AttainedAge,
080817           MaxTerm,
080817           MaxTotBen
080817        INTO
080817           :ws-limit-att-age,
080817           :ws-limit-max-term,
080817           :ws-limit-max-tot-ben
080817        FROM
080817           FormLimits
080817        WHERE
080817           Limit           = :ws-limit-name
080817           and CovType     = :ws-limit-cov-type
080817           and LoIssueAge <= :ws-limit-issue-age
080817           and HiIssueAge >= :ws-limit-issue-age
080817     end-exec
080817
080817     if sqlcode not = 0
080817        display "Error: cannot find lf limits " ws-limit-name ' '
080817        ws-limit-cov-type ' ' ws-limit-issue-age
080817        display ' sql retrun code ' sqlcode
080817        display ' sql err mess    ' sqlerrmc
080817        move 18                  to ws-error-sub
080817        move ws-limit-name       to ws-error-sup
080817        perform 0180-error-handle
080817                                 thru 0180-exit
080817        go to 0300-return-cics
080817     end-if
080817
080817     if ws-rate-benefit <= ws-limit-max-tot-ben
080817        continue
080817     else
080817        move 8                   to ws-error-sub
080817        move '- Lf Benefit '     to ws-error-sup
080817        perform 0180-error-handle
080817                                 thru 0180-exit
080817        go to 0300-return-cics
080817     end-if
080817
080817     if ws-rate-ins-term <= ws-limit-max-term
080817        continue
080817     else
080817        move 7                   to ws-error-sub
080817        move '- Lf Term '        to ws-error-sup
080817        perform 0180-error-handle
080817                                 thru 0180-exit
080817        go to 0300-return-cics
080817     end-if
080817     if ws-att-age <= ws-limit-att-age
080817        continue
080817     else
080817        move 5                   to ws-error-sub
080817        move ws-att-age          to ws-error-sup
080817        perform 0180-error-handle
080817                                 thru 0180-exit
080817        go to 0300-return-cics
080817     end-if
080817
080817     .
080817 0090-exit.
080817     exit.
080817
080817 0095-check-qc-ah-limits.
080817
080817     if not connected-to-db
080817        perform 6000-connect-to-db thru 6000-exit
080817     end-if
080817
080817     move 'DI'                   to ws-limit-cov-type
080817     move ws-rate-age            to ws-limit-issue-age
080817
080817     EXEC SQL
080817        SELECT
080817           AttainedAge,
080817           MaxTerm,
080817           MaxMoBen,
080817           MaxTotBen
080817        INTO
080817           :ws-limit-att-age,
080817           :ws-limit-max-term,
080817           :ws-limit-max-mo-ben,
080817           :ws-limit-max-tot-ben
080817        FROM
080817           FormLimits
080817        WHERE
080817           Limit           = :ws-limit-name
080817           and CovType     = :ws-limit-cov-type
080817           and LoIssueAge <= :ws-limit-issue-age
080817           and HiIssueAge >= :ws-limit-issue-age
080817     end-exec
080817
080817     if sqlcode not = 0
080817        display "Error: cannot find ah limits " ws-limit-name
080817        display ' sql retrun code ' sqlcode
080817        display ' sql err mess    ' sqlerrmc
080817        move 18                  to ws-error-sub
080817        move ws-limit-name       to ws-error-sup
080817        perform 0180-error-handle
080817                                 thru 0180-exit
080817        go to 0300-return-cics
080817     end-if
080817
080817     if ws-rate-benefit <= ws-limit-max-mo-ben
080817        continue
080817     else
080817        move 8                   to ws-error-sub
080817        move '- Ah Benefit '     to ws-error-sup
080817        perform 0180-error-handle
080817                                 thru 0180-exit
080817        go to 0300-return-cics
080817     end-if
080817
080817     if (ws-rate-benefit * ws-rate-ins-term)
080817           <= ws-limit-max-tot-ben
080817        continue
080817     else
080817        move 10                  to ws-error-sub
080817        move '- Ah Tot Benefit ' to ws-error-sup
080817        perform 0180-error-handle
080817                                 thru 0180-exit
080817        go to 0300-return-cics
080817     end-if
080817
080817     if ws-rate-ins-term <= ws-limit-max-term
080817        continue
080817     else
080817        move 7                   to ws-error-sub
080817        move '- Ah Term '        to ws-error-sup
080817        perform 0180-error-handle
080817                                 thru 0180-exit
080817        go to 0300-return-cics
080817     end-if
080817     if ws-att-age <= ws-limit-att-age
080817        continue
080817     else
080817        move 5                   to ws-error-sub
080817        move ws-att-age          to ws-error-sup
080817        perform 0180-error-handle
080817                                 thru 0180-exit
080817        go to 0300-return-cics
080817     end-if
080817
080817     .
080817 0095-exit.
           exit.

       0110-unstring.

      ***____________________________________________________________***
      **|                                                            |**
      **|    Unstring the raw data into data elements                |**
      **|                                                            |**
      ***____________________________________________________________***

              unstring dfhcommarea
                 delimited by '|' into
                    raw-message-num
                    raw-state
                    raw-acct-no
                    raw-vin
                    raw-lf-ah
                    raw-ben-code
                    raw-earn-meth
                    raw-pri-birth-date
                    raw-cob-birth-date
                    raw-benefit
                    raw-eff-date
                    raw-1st-pmt-dt
                    raw-premium
                    raw-loan-term
                    raw-ins-term
                    raw-apr
                    raw-sin-jnt-ind
                    raw-dismemberment
                    raw-retro-elim
                    raw-waiting-days
                    raw-crit-per
                    raw-total-payments
                    raw-period-payment
              end-unstring

           .
       0110-exit.
           exit.

       0120-format-message.

      ***____________________________________________________________***
      **|                                                            |**
      **|    Format each data element to be consistant with          |**
      **|  COBOL rate copybook model                                 |**
      **|                                                            |**
      ***____________________________________________________________***

           inspect
              raw-acct-no replacing leading spaces by zeros
           inspect
              raw-loan-term replacing leading spaces by zeros
           inspect
              raw-ins-term replacing leading spaces by zeros
           inspect
              raw-waiting-days replacing leading spaces by zeros
           inspect
              raw-crit-per replacing leading spaces by zeros

           inspect
              raw-benefit replacing all spaces by zeros
           inspect
              raw-premium replacing all spaces by zeros
           inspect
              raw-apr replacing all spaces by zeros
           inspect
              raw-total-payments replacing all spaces by zeros
           inspect
              raw-period-payment replacing all spaces by zeros

           move raw-benefit            to ws-work-in
           perform 0130-format-amt     thru 0130-exit
           move ws-work-out-v2         to ws-rate-benefit
      *    display ' ben  in *' ws-work-in '*'
      *    display ' ben  out *' ws-work-out '*'

           move raw-total-payments     to ws-work-in
           perform 0130-format-amt     thru 0130-exit
           move ws-work-out-v2         to ws-rate-tot-pmts
      *    display ' tot pmts  in *' ws-work-in '*'
      *    display ' tot pmts  out *' ws-work-out '*'

           move raw-premium            to ws-work-in
           perform 0130-format-amt     thru 0130-exit
           move ws-work-out-v2         to ws-rate-premium
      *    display ' prem in *' ws-work-in '*'
      *    display ' prem out *' ws-work-out '*'

           move raw-period-payment     to ws-work-in
           perform 0130-format-amt     thru 0130-exit
           move ws-work-out-v2         to ws-rate-payment
      *    display ' pmt  in *' ws-work-in '*'
      *    display ' pmt  out *' ws-work-out '*'

           move raw-loan-term          to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-loan-term
      *    display ' rate loan term *' ws-rate-loan-term '*'
      *    display ' ln term in *' ws-work-in '*'
      *    display ' ln term out *' ws-work-out '*'

           move raw-ins-term           to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-ins-term
      *    display ' rate ins  term *' ws-rate-ins-term '*'
      *    display ' ins term in *' ws-work-in '*'
      *    display ' ins term out *' ws-work-out '*'

           move raw-waiting-days       to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-waiting-days
      *    display ' wait day in *' ws-work-in '*'
      *    display ' wait day out *' ws-work-out '*'

           move raw-crit-per           to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-crit-per
      *    display ' crit per in *' ws-work-in '*'
      *    display ' crit per out *' ws-work-out '*'

           move raw-apr                to ws-work-in
           perform 0150-format-apr     thru 0150-exit
           move ws-work-out-v5         to ws-rate-apr
      *    display ' ins apr  in *' ws-work-in '*'
      *    display ' ins apr  out *' ws-work-out '*'

           move raw-state              to ws-rate-state
           move raw-acct-no            to ws-rate-acct-no
           move raw-vin                to ws-rate-vin
           move raw-lf-ah              to ws-rate-lf-ah
           move raw-ben-code           to ws-rate-ben-code
           move raw-earn-meth          to ws-rate-earn-meth
           move raw-pri-birth-date     to ws-rate-pri-birth-date
           move raw-cob-birth-date     to ws-rate-cob-birth-date
           move raw-eff-date           to ws-rate-eff-date
           move raw-1st-pmt-dt         to ws-rate-1st-pmt-dt
           move raw-sin-jnt-ind        to ws-rate-sin-jnt-ind
           move raw-dismemberment      to ws-rate-dismemberment
           move raw-retro-elim         to ws-rate-retro-elim

           .
       0120-exit.
           exit.

       0130-format-amt.

           move zeros                  to ws-work-out
           perform varying s2 from +10 by -1 until
              (s2 < +1)
              or (ws-work-in (s2:1) = '.')
           end-perform
           if s2 < +9
              move ws-work-in (s2 + 1:2)
                                    to ws-work-out (9:2)
              move +8               to s3
              subtract +1 from s2
              perform varying s2 from s2 by -1 until
                 (s2 < +1)
                 or (s3 = 0)
                 move ws-work-in (s2:1) to ws-work-out (s3:1)
                 subtract +1 from s3
              end-perform
           end-if

           .
       0130-exit.
           exit.

       0140-format-term.

           move zeros                  to ws-work-out
           perform varying s2 from +10 by -1 until
              (s2 < +1)
              or (ws-work-in (s2:1) numeric)
           end-perform
           if s2 > +0
              move ws-work-in (s2:1)
                                    to ws-work-out (10:1)
              move +9               to s3
              subtract +1 from s2
              perform varying s2 from s2 by -1 until
                 (s2 < +1)
                 or (s3 = 7)
                 move ws-work-in (s2:1) to ws-work-out (s3:1)
                 subtract +1 from s3
              end-perform
           end-if

           .
       0140-exit.
           exit.

       0150-format-apr.

           move zeros                  to ws-work-out
           perform varying s2 from +10 by -1 until
              (s2 < +1)
              or (ws-work-in (s2:1) = '.')
           end-perform
           if s2 < +5
              move ws-work-in (s2 + 1:5)
                                    to ws-work-out (6:5)
              move +5               to s3
              subtract +1 from s2
              perform varying s2 from s2 by -1 until
                 (s2 < +1)
                 or (s3 = 0)
                 move ws-work-in (s2:1) to ws-work-out (s3:1)
                 subtract +1 from s3
              end-perform
           end-if

           .
       0150-exit.
           exit.

080817 0180-error-handle.

080817     move ws-table-error-no (ws-error-sub)
080817                                 to ws-return-error-no
080817     move spaces                 to ws-return-error-mess
080817     string
080817        ws-table-error-mess (ws-error-sub)
080817        ws-error-sup
080817        delimited by '  ' into ws-return-error-mess
080817     end-string
080817
080817     .
080817 0180-exit.
080817     exit.

       0300-RETURN-CICS.

080817     perform 0400-disconnect     thru 0400-exit
           move ws-return-string       to dfhcommarea
           exec cics return end-exec
           goback

           .
       0300-exit.
           exit.

080817 0400-disconnect.
080817
080817     if connected-to-db
080817        EXEC SQL
080817            disconnect all
080817        END-EXEC
080817        move ' ' to ws-connect-sw
080817     end-if
080817
080817     .
080817 0400-exit.
080817     exit.

       6000-CONNECT-TO-DB.
      
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

040622     move 'appuser'              to usr
040622     move 'appuser@cso'          to pass
040622     evaluate true
040622        when ws-kix-myenv = 'cid1p'
040622           MOVE 'SDVDB01_CrtManage'
040622                                 TO SVR
040622        when ws-kix-myenv = 'mdoff'
040622           MOVE 'HOVTSTDB01UAT_CrtManage'
040622                                 TO SVR
040622        when other
040622           MOVE 'HOVTSTDB01_CrtManage'
040622                                 TO SVR
040622     end-evaluate

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
       
           if sqlcode not = 0
              display "Error: cannot connect to " svr
              display sqlcode
              display sqlerrmc
           else
              display ' Successful Connect ' sqlcode
              set connected-to-db to true
           end-if

           .
       6000-EXIT.
           EXIT.

       7000-GET-RATE.
       
           EXEC CICS LINK
              PROGRAM ('ELRATE')
              COMMAREA (CALCULATION-PASS-AREA)
              LENGTH   (CP-COMM-LENGTH)
           END-EXEC
           
           .
       7000-EXIT.
           EXIT.

       9700-DATE-LINK.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.

