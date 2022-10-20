      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      *****************************************************************
      *                                                               *
      * Copyright (c) 2014 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. EL202.
      *
      *AUTHOR.    Cowtown.
      *           Colleyville, TEXAS.

      *REMARKS.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  This program is waken up by a webservice request/http     ***
      ***  request and is passed a remit id. The program will next   ***
      ***  read tables CUC_Logic_Remittance and CUC_Logic_Remittance_***
      ***  cert using the remit id passed and will in turn generate  ***
      ***  a ERPYAJ record, ERPNDB and ERPNDM records.               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

091114******************************************************************
091114*                   C H A N G E   L O G
091114*
091114* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091114*-----------------------------------------------------------------
091114*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091114* EFFECTIVE    NUMBER
091114*-----------------------------------------------------------------
091114* 091114  CR2013121800001  PEMA  NEW PROGRAM
111715* 111715  IR2015111600001  PEMA  CHG G/L ACCTNO ON CID ACH
040516* 040516  IR2016040100001  PEMA  CNVT ALL TEXT TO UPPER CASE
080116* 080116  IR2016090100005  PEMA  Expand numeric fields
120921* 120921  IR2021120900003  PEMA  Move ezLink_Prod to sdv-db01
091114*-----------------------------------------------------------------

       environment division.

       data division.

       working-storage section.

       77  s1                          pic s999 comp-3 value +0.
       77  s2                          pic s999 comp-3 value +0.
082014 77  i1                          pic s999 comp-3 value +0.
082014 77  o1                          pic s999 comp-3 value +0.
       77  ws-bin-current-dt           pic xx value low-values.
       77  ws-bin-eom-dt               pic xx value low-values.
       77  ws-table-sw                 pic x    value spaces.
           88  table-found               value 'Y'.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  ws-remit-sw                 pic x value ' '.
           88  ws-remit-not-found        value 'N'.
       77  work-seq-no                 pic s9(8) comp-3 value +0.
       77  ws-rollback-sw              pic x  value spaces.
           88  rollback-needed            value 'Y'.
       77  ws-commit-sw                pic x  value spaces.
           88  commit-needed              value 'Y'.
       77  ROLL-BACK                   PIC X(08) VALUE 'ROLLBACK'.

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

       01  ws-remit-id-a               pic x(10).
       01  ws-remit-id redefines
           ws-remit-id-a               pic 9(10).

       01  ws-key-stuff.
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

       01  sql-remit-record.
           05  sql-py-RemitId          pic 9(9).
           05  sql-py-Carrier          pic x.
           05  sql-py-Group            pic x(6).
           05  sql-py-State            pic xx.
           05  sql-py-AccountNumber    pic x(10).
           05  sql-py-Name             pic x(50).
           05  sql-py-Addr1            pic x(50).
           05  sql-py-Addr2            pic x(50).
           05  sql-py-AddrCity         pic x(50).
           05  sql-py-AddrState        pic xx.
           05  sql-py-AddrZIP          pic x(9).
           05  sql-py-ResponsibleCarrier
                                       pic x.
           05  sql-py-ResponsibleNumber
                                       pic x(10).
           05  sql-py-LogicStatus      pic 9(7).
           05  sql-py-LogicStatusDate  pic x(30).
           05  sql-py-company          pic xxx.
           05  sql-py-tot-prem         pic x(12).
           05  sql-py-tot-prem-n redefines
               sql-py-tot-prem         pic 9(9).99.
           05  sql-is-ach              pic x.

       01  sql-cert-records.
           05  sql-pb-CertId           pic 9(9).
           05  sql-pb-RemitId          pic 9(9).
           05  sql-pb-CertNumber       pic x(11).
           05  sql-pb-EffectiveDate    pic x(30).
           05  sql-pb-FirstPaymentDate pic x(30).
           05  sql-pb-FirstName        pic x(30).
           05  sql-pb-LastName         pic x(30).
           05  sql-pb-CashIndicator    pic x.
           05  sql-pb-LivesCovered     pic 9(7).
           05  sql-pb-LfBenCode        pic xx.
           05  sql-pb-LfTerm           pic 999.
           05  sql-pb-LfBenefit        pic x(12).
           05  sql-pb-lfbenefit-n redefines
               sql-pb-lfbenefit        pic 9(9).99.
           05  sql-pb-LfPremium        pic x(10).
           05  sql-pb-lfpremium-n redefines
               sql-pb-lfpremium        pic 9(7).99.
           05  sql-pb-AhBenCode        pic xx.
           05  sql-pb-AhTerm           pic 999.
           05  sql-pb-AhBenefit        pic x(12).
           05  sql-pb-ahbenefit-n redefines
               sql-pb-ahbenefit        pic 9(9).99.
           05  sql-pb-AhPremium        pic x(10).
           05  sql-pb-ahpremium-n redefines
               sql-pb-ahpremium        pic 9(7).99.
           05  sql-pb-ErrorMessage     pic x(50).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  ws-roll-back-keys.
           05  ws-rb-erpyaj-key        pic x(33) value spaces.
       01  ws-batch-header-work-area.
           05  ws-lf-iss-premium          pic s9(9)v99 comp-3 value +0.
           05  ws-ah-iss-premium          pic s9(9)v99 comp-3 value +0.
           05  ws-iss-cnt                 pic s9(3) comp-3 value +0.
           05  ws-cnc-cnt                 pic s9(3) comp-3 value +0.
           05  ws-highest-seq-no          pic s9(4) comp value +0.

       01  FILE-KEYS.
           12  ELCNTL-KEY.                                              
               16 ELCNTL-COMP-ID           PIC XXX     VALUE SPACES.    
               16 ELCNTL-REC-TYPE          PIC X       VALUE SPACES.    
               16 ELCNTL-ACCESS.                                        
                   20 ELCNTL-STATE         PIC XX      VALUE SPACES.    
                   20  FILLER              PIC X       VALUE SPACES.    
                   20 ELCNTL-CARRIER       PIC X       VALUE SPACES.    
               16 ELCNTL-SEQ               PIC S9(4)   VALUE +0    COMP.

       01  ws-init-erpndb-rec          pic x(585) value spaces.
       01  ws-init-erpndm              pic x(374) value spaces.
       01  f.
           05  ws-batch-seq-no         pic s9(4) comp value +0.
           05  ws-connect-sw               pic x  value ' '.
               88  connected-to-db             value 'Y'.
           05  ws-comp-id              pic xxx.
           05  ws-comp-cd              pic x.

       01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
           88  RESP-NORMAL                    VALUE +0.
           88  resp-file-notfnd               value +12.
           88  RESP-NOTFND                    VALUE +13.
           88  resp-duprec                    value +14.
           88  resp-dupkey                    value +15.
           88  resp-invreq                    value +16.
           88  RESP-NOTOPEN                   VALUE +19.
           88  RESP-ENDFILE                   VALUE +20.
           88  resp-lengtherr                 value +22.

       01  ws-display-response         pic s9(5) value zeros.
       01  ws-work-time-a              pic x(6).
       01  ws-work-time redefines ws-work-time-a
                                       pic 9(6).
       01  ws-work-amt-alpha           pic x(11).
       01  ws-work-amt-num redefines ws-work-amt-alpha
                                       pic 9(9)v99.
       01  ws-qry-string               pic x(80) value spaces.
       01  ws-qrystr-len               pic s9(8) comp value +60.
       01  ws-var-alpha                pic x(10).
       01  ws-seq-alpha                pic x(5).
       01  ws-seq-no-num redefines ws-seq-alpha
                                       pic 9(5).

                                       copy ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCCNTL.
                                       copy ERCPYAJ.
                                       copy ERCPNDB.
                                       copy ERCPNDM.

       01  w-doctoken                  pic x(16).

       01  BATCH-TO-PROCESS.
           05  EDIT-COMPANY-CD         PIC X       VALUE LOW-VALUES.
           05  EDIT-BATCH              PIC X(6)    VALUE SPACES.
           05  EDIT-COMPANY-ID         PIC XXX     VALUE SPACES.
           05  EDIT-RESTART-BATCH      PIC X(6)    VALUE SPACES.

       01 output-data.
          05  filler                   pic x(6) value "TITLE=".
          05  output-title             pic x(26) value 'Check Posting'.
          05  filler                   pic x(8) value "&COMPID=".
          05  out-comp-id              pic xxx.
          05  filler                   pic x(5) value "&MSG=".
          05  output-msg               pic x(50).

       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA                     PIC X(1024).                 
      
       01  var  pic x(30).

       procedure division.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***                                                We don't    ***
      ***  have any form fields to browse through, we are getting    ***
      ***  the information we need through the variables that are    ***
      ***  included in the URL. These variables are retrieved by     ***
      ***  the           exec cics web extract                       ***
      ***                   querystring  (data-value)                ***
      ***                   querystrlen  (data-value)                ***
      ***                end-exec                                    ***
      ***  BTW, you must provide the length, it isn't passed to you. ***
      ***  Allow for extra on the length or you will get a length    ***
      ***  error instead of just truncation.                         ***
      ***                                                            ***
      ***  This program is expecting                                 ***
pemtst*** http://logictest:5002/cics/cwba/EL202?comp=DCC&remit=00001 ***
      ***http://slunikix:7001/cics/cwba/EL202?comp=DCC&remit=0000000001*
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           string ws-fn-hours ws-fn-minutes ws-fn-seconds
              delimited by size into ws-work-time-a
           end-string
           string ws-fn-mo     '/'
                  ws-fn-da     '/'
                  ws-fn-ccyr
              delimited by size into ws-status-date
           end-string
           move ws-fn-cymd             to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
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

           exec cics web extract
              querystring  (ws-qry-string)
              querystrlen  (ws-qrystr-len)
           end-exec

           display ' qry string ' ws-qry-string
           display ' gry len    ' ws-qrystr-len

           move ws-qry-string (6:3)    to ws-comp-id
           move ws-qry-string (16:10)  to ws-var-alpha
           move zeros                  to ws-remit-id-a
           move +10                    to o1
           perform varying i1 from +10 by -1 until i1 < +1
              if ws-var-alpha (i1:1) numeric
                 move ws-var-alpha (i1:1) to ws-remit-id-a (o1:1)
                 subtract +1 from o1
              end-if
           end-perform

           move 'Successful Run   '    to output-msg

           move ws-comp-id             to elcntl-key
           move '1'                    to elcntl-rec-type
           move +0                     to elcntl-seq
           exec cics read
              dataset  ('ELCNTL')
              into     (control-file)
              ridfld   (elcntl-key)
              resp     (ws-response)
           end-exec

           if resp-normal
              display ' good cntl read '
              move cf-cr-month-end-dt  to ws-bin-eom-dt
              move cf-company-cd       to ws-comp-cd
           else
              move 'Company Rec not found ' to output-msg
              display ' error elcntl read ' ws-response ' '
                 elcntl-key (1:8)
              go to 0000-return
           end-if

           perform 0010-connect-db     thru 0010-exit

           perform 0020-process-remit  thru 0020-exit

pemtst     perform 0050-open-cursor    thru 0050-exit
pemtst     perform 0060-process-input  thru 0060-exit

pemtst     perform 0130-start-edit     thru 0130-exit


pemtst     perform 0226-update-remit-table
pemtst                                 thru 0226-exit
pemtst     perform 0225-commit-work    thru 0225-exit

           .
       0000-return.

           if connected-to-db
              perform 0230-disconnect  thru 0230-exit
           end-if

           perform 0220-send-form      thru 0220-exit

           if rollback-needed
              display ' executing roll back '
              perform 0005-rollback    thru 0005-exit
      *       CALL 'kixvsam' USING ROLL-BACK
      *       exec cics syncpoint
      *          rollback
      *          resp (ws-response)
      *       end-exec
      *       display ' rollback resp ' ws-response
           end-if

           exec cics
              return
           end-exec
           goback

           .
       0005-rollback.

           if ws-rb-erpyaj-key not = spaces
              move ws-rb-erpyaj-key    to py-control-primary
              EXEC CICS DELETE
                 DATASET  ('ERPYAJ')                           
                 RIDFLD   (PY-CONTROL-PRIMARY)
                 resp     (ws-response)
              END-EXEC
              if resp-normal
                 display ' good delete on erpyaj '
              else
                 display ' bad delete on erpyaj ' ws-response
              end-if
           end-if

           if ws-batch-no not = zeros
              move ws-comp-cd          to pb-company-cd
              move ws-batch-no         to pb-entry-batch
              move +0                  to pb-batch-seq-no
                                          pb-batch-chg-seq-no
              exec cics delete
                 dataset   ('ERPNDB')
                 ridfld    (pb-control-primary)
                 keylength (7)
                 generic
                 resp     (ws-response)
              end-exec
              if resp-normal
                 display ' good delete on erpndb '
              else
                 display ' bad delete on erpndp ' ws-response
              end-if
           end-if

           if ws-batch-no not = zeros
              move ws-comp-cd          to pm-company-cd
              move ws-batch-no         to pm-entry-batch
              move +0                  to pm-batch-seq-no
                                          pm-batch-chg-seq-no
              exec cics delete
                 dataset   ('ERPNDM')
                 ridfld    (pm-control-primary)
                 keylength (7)
                 generic
                 resp     (ws-response)
              end-exec
              if resp-normal
                 display ' good delete on erpndm '
              else
                 display ' bad delete on erpndm ' ws-response
              end-if
           end-if

           .
       0005-exit.
           exit.

       0010-CONNECT-DB.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  Even though it's on ntcso2, CUConnect is the test DB      ***
      ***  The production DB is ezLink_Prod.  The table names are    ***
      ***  the same.                                                 ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

120921     move 'HOVTSTDB01_ezLink_Prod'
120921                                 to svr
120921     move 'appuser'              to usr
120921     move 'appuser@cso'          to pass

           if ws-kix-myenv = 'cid1p'
120921        move 'SDVDB01_ezLink_Prod'
120921                                 to svr
120921        move 'appuser'           to usr
120921        move 'appuser@cso'       to pass
           end-if

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           display ' About to connect to ' svr ' ' usr-pass

           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
       
           if sqlcode not = 0
              display "Error: cannot connect to " svr
              display sqlcode
              display sqlerrmc
              move 'Failed to connect to DB'
                                       to output-msg
              go to 0000-return
           else
              set connected-to-db to true
              display ' Successful Connect ' sqlcode
           end-if

           .
       0010-EXIT.
           EXIT.

       0020-process-remit.

           perform 0030-get-tbl-row    thru 0030-exit
           perform 0040-build-erpyaj   thru 0040-exit

           .
       0020-exit.
           exit.

       0030-get-tbl-row.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  I'm only expecting one row so no cursor is declared       ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           display ' about to get table row ' ws-remit-id
           move ' '                    to ws-remit-sw

      *    exec sql
      *       SELECT
      *          RemitId,
      *          Carrier,
      *          Group,
      *          State,
      *          AccountNumber,
      *          Name,
      *          Addr1,
      *          Addr2,
      *          AddrCity,
      *          AddrState,
      *          AddrZIP,
      *          ResponsibleCarrier,
      *          ResponsibleNumber,
      *          LogicStatus,
      *          LogicStatusDate,
      *          Company,
      *          TotalPremium
      *       INTO
      *          :sql-py-RemitId,
      *          :sql-py-Carrier,
      *          :sql-py-Group,
      *          :sql-py-State,
      *          :sql-py-AccountNumber,
      *          :sql-py-Name,
      *          :sql-py-Addr1,
      *          :sql-py-Addr2,
      *          :sql-py-AddrCity,
      *          :sql-py-AddrState,
      *          :sql-py-AddrZIP,
      *          :sql-py-ResponsibleCarrier,
      *          :sql-py-ResponsibleNumber,
      *          :sql-py-LogicStatus,
      *          :sql-py-LogicStatusDate,
      *          :sql-py-company,
      *          :sql-py-tot-prem
      *       FROM
      *          CUC_Logic_Remittance
      *       WHERE
      *          RemitId = :ws-remit-id
      *    end-exec

           exec sql
              SELECT
                 CUC_Logic_Remittance.RemitId,
                 Carrier,
                 [Group],
                 [State],
                 AccountNumber,
                 Name,
                 Addr1,
                 Addr2,
                 AddrCity,
                 AddrState,
                 AddrZIP,
                 ResponsibleCarrier,
                 ResponsibleNumber,
                 LogicStatus,
                 Company,
                 TotalPremium,
                 CUC_Remittance.IsACH
              INTO
                 :sql-py-RemitId,
                 :sql-py-Carrier,
                 :sql-py-Group,
                 :sql-py-State,
                 :sql-py-AccountNumber,
                 :sql-py-Name,
                 :sql-py-Addr1,
                 :sql-py-Addr2,
                 :sql-py-AddrCity,
                 :sql-py-AddrState,
                 :sql-py-AddrZIP,
                 :sql-py-ResponsibleCarrier,
                 :sql-py-ResponsibleNumber,
                 :sql-py-LogicStatus,
                 :sql-py-company,
                 :sql-py-tot-prem,
                 :sql-is-ach
              FROM
                 CUC_Logic_Remittance inner join CUC_Remittance on
                    CUC_Logic_Remittance.RemitId =
                       CUC_Remittance.RemitId
              WHERE
                 CUC_Logic_Remittance.RemitId = :ws-remit-id
           end-exec

           if sqlcode not = 0
              set ws-remit-not-found   to true
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display "Error: cannot read row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move ' Remittance Not Found '
                                       to output-msg
              go to 0000-return
           end-if

           display ' good get on table ' sqlcode

           if sql-py-LogicStatus = 2
              move 'Remit previously processed '
                                       to output-msg
pemtst        go to 0000-return
           end-if

           .
       0030-exit.
           exit.

       0040-build-erpyaj.

           compute work-seq-no = ws-work-time * 3
           MOVE 'PY'                   TO pending-pay-adj
           move zeros                  to py-file-seq-no
                                          py-last-maint-hhmmss
                                          py-entry-amt
                                          py-check-que-control
                                          py-check-que-sequence
           move low-values             to py-last-maint-dt
                                          py-credit-select-dt
                                          py-credit-accept-dt
                                          py-billed-date
                                          py-reported-dt
                                          py-input-dt
                                          py-check-written-dt
                                          py-ar-date
                                          py-gl-date
                                          
           MOVE cf-COMPANY-CD          TO PY-COMPANY-CD
040516     MOVE function upper-case(sql-py-responsiblecarrier)
                                       TO PY-CARRIER
040516     MOVE function upper-case(sql-py-group)
                                       TO PY-GROUPING
040516     MOVE function upper-case(sql-py-responsiblenumber)
                                       TO PY-FIN-RESP
040516     MOVE function upper-case(sql-py-accountnumber)
                                       TO PY-ACCOUNT

           ADD +1                      TO WORK-SEQ-NO.          
           MOVE WORK-SEQ-NO            TO PY-FILE-SEQ-NO.       
                                                                 
           move 'R'                    to py-record-type
           move 'E202'                 to py-last-maint-by
           move ws-bin-current-dt      to py-last-maint-dt
                                          py-input-dt
           move ws-work-time           to py-last-maint-hhmmss
      *    move +20.00                 to py-entry-amt
           move ws-bin-eom-dt          to py-credit-select-dt
           move 'A'                    to py-ercomp-type

           move sql-is-ach             to ws-bit
           display ' bit comp ' ws-bit-comp

           evaluate true
              when (ws-comp-id = 'DCC')
                 and (py-carrier = '9')
                 and (ws-bit-comp = zeros)      *> ACH FALSE
                 move '2725040310'     to py-gl-account
              when (ws-comp-id = 'DCC')
                 and (py-carrier = '7')
                 and (ws-bit-comp not = zeros)  *> ACH TRUE
                 move '1108121010'     to py-gl-account
              when (ws-comp-id = 'DCC')
                 and (py-carrier = '9')
                 and (ws-bit-comp not = zeros)  *> ACH TRUE
                 move '1108121250'     to py-gl-account
              when (ws-comp-id = 'CID')
                 and (ws-bit-comp not = zeros)  *> ACH TRUE
111715           move '1108124700'     to py-gl-account
              when (ws-comp-id = 'DCC')
                 move '1825013400'     to py-gl-account
              when other
                 move '1825099050'     to py-gl-account
           end-evaluate

           if ws-bit-comp not = zeros     *>  ACH TRUE
              move 'ACH'               to py-gl-comment
           end-if
      *    if ws-comp-id = 'CID'
      *       move '1825099050'        to py-gl-account
      *    else
      *       move '1825013400'        to py-gl-account
      *    end-if

           move +11                    to s2
           move zeros                  to ws-work-amt-alpha
           perform varying s1 from +11 by -1 until s1 < +1
              if sql-py-tot-prem (s1:1) numeric
                 move sql-py-tot-prem (s1:1)
                                       to ws-work-amt-alpha (s2:1)
                 subtract +1 from s2
              end-if
           end-perform
           display ' tot premium ' ws-work-amt-num
           move ws-work-amt-num        to py-entry-amt

           .
       0040-write.

           display ' about to write pyaj '
           EXEC CICS WRITE                                       
               DATASET  ('ERPYAJ')                           
               FROM     (PENDING-PAY-ADJ)                        
               RIDFLD   (PY-CONTROL-PRIMARY)
               resp     (ws-response)
           END-EXEC

           if resp-normal
              move py-control-primary  to ws-rb-erpyaj-key
              move 2                   to ws-status-code
           else
              if resp-duprec
                 add +1                to py-file-seq-no
                 go to 0040-write
              else
                 move 3                to ws-status-code
                 move ws-response      to ws-display-response
                 move spaces           to ws-error-message
                 string ' Bad write on ERPYAJ FILE ' ws-display-response
                    delimited by size into ws-error-message
                 end-string
pemtst           perform 0226-update-remit-table
pemtst                                 thru 0226-exit
pemtst           perform 0225-commit-work
pemtst                                 thru 0225-exit
                 go to 0000-return
              end-if
           end-if

           .
       0040-exit.
           exit.

       0050-open-cursor.

pemtst*    display ' declare cursor ' ws-begin-dt ' ' ws-end-dt

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  The dates on the sql table have values in the time        ***
      ***  so I convert it to a string and just use mm/dd/yyyy       ***
      ***  to perform the comparison.                                ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           EXEC SQL
              DECLARE
                 remitcerts cursor for
              SELECT
                 CertId,
                 RemitId,
                 CertNumber,
                 EffectiveDate,
                 FirstPaymentDate,
                 FirstName,
                 LastName,
                 CashIndicator,
                 LivesCovered,
                 LfBenCode,
                 LfTerm,
                 LfBenefit,
                 LfPremium,
                 AhBenCode,
                 AhTerm,
                 AhBenefit,
                 AhPremium,
                 ErrorMessage
              FROM
                 CUC_Logic_Remittance_Cert
              WHERE
                 RemitId = :ws-remit-id
           end-exec

           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move ' Error declaring cursor '
                                       to output-msg
              move 3                   to ws-status-code
              move sqlcode             to ws-display-response
              move spaces              to ws-error-message
              string ' Error declaring cursor   ' ws-display-response
                 delimited by size into ws-error-message
              end-string
              perform 0226-update-remit-table
                                       thru 0226-exit
              perform 0225-commit-work thru 0225-exit
              go to 0000-return
           end-if

           display ' good declare cursor ' ws-remit-id

           EXEC SQL
              open remitcerts
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move ' Error opening cursor '
                                       to output-msg
              move 3                   to ws-status-code
              move sqlcode             to ws-display-response
              move spaces              to ws-error-message
              string ' Error openning cursor    ' ws-display-response
                 delimited by size into ws-error-message
              end-string
              perform 0226-update-remit-table
                                       thru 0226-exit
              perform 0225-commit-work thru 0225-exit
              go to 0000-return
           end-if

           display ' good open cursor ' ws-remit-id

           .
       0050-exit.
           exit.

       0060-process-input.

           perform until sqlcode not = 0
              EXEC SQL
                 fetch remitcerts into
                    :sql-pb-CertId,
                    :sql-pb-RemitId,
                    :sql-pb-CertNumber,
                    :sql-pb-EffectiveDate,
                    :sql-pb-FirstPaymentDate,
                    :sql-pb-FirstName,
                    :sql-pb-LastName,
                    :sql-pb-CashIndicator,
                    :sql-pb-LivesCovered,
                    :sql-pb-LfBenCode,
                    :sql-pb-LfTerm,
                    :sql-pb-LfBenefit,
                    :sql-pb-LfPremium,
                    :sql-pb-AhBenCode,
                    :sql-pb-AhTerm,
                    :sql-pb-AhBenefit,
                    :sql-pb-AhPremium,
                    :sql-pb-ErrorMessage
              END-EXEC

              if sqlcode = 0
                 perform 0070-bld-erpndb 
                                       thru 0070-exit
                 perform 0110-write-erpndb
                                       thru 0110-exit
                 perform 0120-write-erpndm
                                       thru 0120-exit
              else
                 if sqlcode not = 0 and 100
                    display "Error: cannot fetch row " 
                    display ' sql return code ' sqlcode
                    display ' sql err mess    ' sqlerrmc
                    move ' Error fetching cursor '
                                       to output-msg
                    set rollback-needed to true
                    move 3             to ws-status-code
                    move sqlcode       to ws-display-response
                    move spaces        to ws-error-message
                    string ' Error during fetch cursor '
                      ws-display-response
                       delimited by size into ws-error-message
                    end-string
                    perform 0226-update-remit-table
                                       thru 0226-exit
                    perform 0225-commit-work
                                       thru 0225-exit
                    go to 0000-return
                 end-if
              end-if
           end-perform

           if sqlcode = 100
              display ' Normal end of record set '
              perform 0100-build-batch-hdr
                                       thru 0100-exit
              perform 0110-write-erpndb
                                       thru 0110-exit
           end-if

           EXEC SQL
               close remitcerts
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0060-exit.
           exit.

       0070-bld-erpndb.

           if ws-batch-no-n = zeros
              perform 0080-get-batch-no thru 0080-exit
           end-if
           move ws-init-erpndb-rec     to pending-business
           move ws-batch-no            to pb-entry-batch
                                          pb-original-entry-batch
                                          pb-csr-entry-batch
           add +1 to ws-batch-seq-no
           move ws-batch-seq-no        to pb-batch-seq-no
                                          pb-original-seq-no
                                          pb-csr-batch-seq-no

040516     move function upper-case(sql-pb-certnumber)
                                       to pb-cert-no

           string sql-pb-effectivedate (1:4)
                  sql-pb-effectivedate (6:2)
                  sql-pb-effectivedate (9:2)
              delimited by size into dc-greg-date-cymd-r
           end-string
           move 'L'                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to pb-cert-eff-dt
           else
              display ' error cvtdte eff dt ' sql-pb-effectivedate
                 ' ' dc-error-code
           end-if
           move function upper-case(sql-pb-lastname)
                                       to pb-i-insured-last-name
           move function upper-case(sql-pb-firstname)
                                       to pb-i-insured-first-name
           if sql-pb-lfbencode = zeros
              move spaces              to sql-pb-lfbencode
           end-if
040516     move function upper-case(sql-pb-lfbencode)
                                       to pb-i-life-benefit-cd
                                          pb-i-lf-input-cd
           if pb-i-life-benefit-cd = spaces
              move zeros               to pb-i-life-benefit-cd
           end-if
           move sql-pb-lfterm          to pb-i-lf-term

           move 'L'                    to pb-ah-override-l1
           move +11                    to s2
           move zeros                  to ws-work-amt-alpha
           perform varying s1 from +12 by -1 until s1 < +1
              if sql-pb-lfbenefit (s1:1) numeric
                 move sql-pb-lfbenefit (s1:1)
                                       to ws-work-amt-alpha (s2:1)
                 subtract +1 from s2
              end-if
           end-perform
           display ' lf benefit  ' ws-work-amt-num
           move ws-work-amt-num        to pb-i-lf-benefit-amt

           move +11                    to s2
           move zeros                  to ws-work-amt-alpha
           perform varying s1 from +10 by -1 until s1 < +1
              if sql-pb-lfpremium (s1:1) numeric
                 move sql-pb-lfpremium (s1:1)
                                       to ws-work-amt-alpha (s2:1)
                 subtract +1 from s2
              end-if
           end-perform
           display ' lf premium  ' ws-work-amt-num
           move ws-work-amt-num        to pb-i-lf-premium-amt

           if sql-pb-ahbencode = zeros
              move spaces              to sql-pb-ahbencode
           end-if
040516     move function upper-case(sql-pb-ahbencode)
                                       to pb-i-ah-benefit-cd
                                          pb-i-ah-input-cd
           if pb-i-ah-benefit-cd = spaces
              move zeros               to pb-i-ah-benefit-cd
           end-if
           move 'A'                    to pb-ah-override-l1
           move sql-pb-ahterm          to pb-i-ah-term

           move +11                    to s2
           move zeros                  to ws-work-amt-alpha
           perform varying s1 from +12 by -1 until s1 < +1
              if sql-pb-ahbenefit (s1:1) numeric
                 move sql-pb-ahbenefit (s1:1)
                                       to ws-work-amt-alpha (s2:1)
                 subtract +1 from s2
              end-if
           end-perform
           display ' ah benefit  ' ws-work-amt-num
           move ws-work-amt-num        to pb-i-ah-benefit-amt

           move +11                    to s2
           move zeros                  to ws-work-amt-alpha
           perform varying s1 from +10 by -1 until s1 < +1
              if sql-pb-ahpremium (s1:1) numeric
                 move sql-pb-ahpremium (s1:1)
                                       to ws-work-amt-alpha (s2:1)
                 subtract +1 from s2
              end-if
           end-perform
           display ' ah premium  ' ws-work-amt-num
           move ws-bin-eom-dt          to pb-credit-select-dt

           move ws-work-amt-num        to pb-i-ah-premium-amt

           move sql-pb-LivesCovered    to pb-i-lives
040516     move function upper-case(sql-pb-CashIndicator)
                                       to pb-i-entry-status
                                          pb-batch-entry

           compute ws-lf-iss-premium =
              ws-lf-iss-premium + pb-i-lf-premium-amt

           compute ws-ah-iss-premium =
              ws-ah-iss-premium + pb-i-ah-premium-amt

           move 'A'                    to pb-force-code

           compute ws-iss-cnt = ws-iss-cnt + +1
           move pb-batch-seq-no           to ws-highest-seq-no

      *    display ' sql-pb-CertId           ' sql-pb-CertId          
      *    display ' sql-pb-RemitId          ' sql-pb-RemitId         
      *    display ' sql-pb-CertNumber       ' sql-pb-CertNumber      
      *    display ' sql-pb-EffectiveDate    ' sql-pb-EffectiveDate   
      *    display ' sql-pb-FirstPaymentDate ' sql-pb-FirstPaymentDate
      *    display ' sql-pb-FirstName        ' sql-pb-FirstName       
      *    display ' sql-pb-LastName         ' sql-pb-LastName        
      *    display ' sql-pb-CashIndicator    ' sql-pb-CashIndicator   
      *    display ' sql-pb-LivesCovered     ' sql-pb-LivesCovered    
      *    display ' sql-pb-LfBenCode        ' sql-pb-LfBenCode       
      *    display ' sql-pb-LfTerm           ' sql-pb-LfTerm          
      *    display ' sql-pb-LfBenefit        ' sql-pb-LfBenefit       
      *    display ' sql-pb-LfPremium        ' sql-pb-LfPremium       
      *    display ' sql-pb-AhBenCode        ' sql-pb-AhBenCode       
      *    display ' sql-pb-AhTerm           ' sql-pb-AhTerm          
      *    display ' sql-pb-AhBenefit        ' sql-pb-AhBenefit       
      *    display ' sql-pb-AhPremium        ' sql-pb-AhPremium       
      *    display ' sql-pb-ErrorMessage     ' sql-pb-ErrorMessage    

           .
       0070-exit.
           exit.

       0080-get-batch-no.

           perform 0090-init-erpndb    thru 0090-exit
           perform 0240-init-erpndm    thru 0240-exit
           move ws-comp-id             to elcntl-key
           move '1'                    to elcntl-rec-type
           move +0                     to elcntl-seq
           exec cics read update
              dataset  ('ELCNTL')
              into     (control-file)
              ridfld   (elcntl-key)
              resp     (ws-response)
           end-exec

           if resp-normal
              add +1                   to cf-last-batch-no
              move cf-last-batch-no    to ws-batch-no-n
              move +0                  to ws-batch-seq-no
              exec cics rewrite
                 dataset  ('ELCNTL')
                 from     (control-file)
                 resp     (ws-response)
              end-exec
              if not resp-normal
                 display ' bad rewrite on cntl - batch no '
                    ws-batch-no-n
                 move ' Error rewrite ELCNTL '
                                       to output-msg
                 set rollback-needed   to true
                 move ws-response         to ws-display-response
                 move spaces              to ws-error-message
                 string ' Bad rewrite  ELCNTL FILE '
                    ws-display-response
                    delimited by size into ws-error-message
                 end-string
                 move 3                to ws-status-code
                 perform 0226-update-remit-table
                                       thru 0226-exit
                 perform 0225-commit-work
                                       thru 0225-exit
                 go to 0000-return
              end-if
           else
              display ' bad read upde on cntl - batch no '
                    elcntl-key (1:4)
           end-if

           .
       0080-exit.
           exit.

       0090-init-erpndb.

           move 'PB'                   to pending-business
           INITIALIZE PB-issue-RECORD
           initialize pb-record-status
           
           move low-values             to pb-i-lf-expire-dt
                                          pb-i-ah-expire-dt
                                          pb-i-1st-pmt-dt
                                          pb-i-last-add-on-dt
                                          pb-i-birthday
                                          pb-i-joint-birthday
                                          pb-credit-select-dt
                                          pb-credit-accept-dt
                                          pb-billed-dt
                                          pb-input-dt
                                          pb-acct-eff-dt
                                          pb-acct-exp-dt
                                          pb-confirmation-rept-dt
                                          pb-ga-bill-dt (1)
                                          pb-ga-bill-dt (2)
                                          pb-ga-bill-dt (3)
                                          pb-ga-bill-dt (4)
                                          pb-ga-bill-dt (5)
           move ws-comp-id             to pb-company-id
           move ws-comp-cd             to pb-company-cd
                                          pb-company-cd-a1
                                          pb-original-company-cd
                                          pb-csr-company-cd
           move zeros                  to pb-chg-count
                                          pb-batch-chg-seq-no
                                          pb-alt-chg-seq-no
                                          pb-original-chg-seq-no
                                          pb-csr-batch-chg-seq-no
                                          pb-i-rate-deviation-lf
                                          pb-i-rate-deviation-ah
                                          pb-no-of-errors
                                          pb-lf-billed-amts
                                          pb-ah-billed-amts
040516     move function upper-case(sql-py-carrier)
                                       to pb-carrier
                                          pb-sv-carrier
040516     move function upper-case(sql-py-group)
                                       to pb-grouping
                                          pb-sv-grouping
040516     move function upper-case(sql-py-state)
                                       to pb-state
                                          pb-sv-state
040516     move function upper-case(sql-py-accountnumber)
                                       to pb-account
           move '1'                    to pb-record-type
           move 'E202'                 to pb-last-maint-by
                                          pb-input-by
           move ws-bin-current-dt      to pb-last-maint-dt
                                          pb-input-dt
           move ws-work-time           to pb-last-maint-hhmmss
           perform varying s1 from +1 by +1 until s1 > +10
              move +0                  to pb-common-error (s1)
           end-perform
           move pending-business       to ws-init-erpndb-rec

           .
       0090-exit.
           exit.

       0100-BUILD-BATCH-HDR.

           display ' about to build batch hdr '
           move ws-init-erpndb-rec     to pending-business
           move ws-batch-no            to pb-entry-batch
                                          pb-original-entry-batch
                                          pb-csr-entry-batch
                                          pb-cert-no
           add +1 to ws-batch-seq-no
           move 9999                   to pb-batch-seq-no
                                          pb-original-seq-no
                                          pb-csr-batch-seq-no
           move high-values            to pb-cert-eff-dt


           move zeros                  to PB-B-LF-ISS-PRM-REMITTED  
                                          PB-B-LF-ISS-PRM-ENTERED   
                                          PB-B-LF-ISS-PRM-COMPUTED  
                                          PB-B-LF-CAN-PRM-REMITTED  
                                          PB-B-LF-CAN-PRM-ENTERED   
                                          PB-B-LF-CAN-PRM-COMPUTED  
                                          PB-B-AH-ISS-PRM-REMITTED  
                                          PB-B-AH-ISS-PRM-ENTERED   
                                          PB-B-AH-ISS-PRM-COMPUTED  
                                          PB-B-AH-CAN-PRM-REMITTED  
                                          PB-B-AH-CAN-PRM-ENTERED   
                                          PB-B-AH-CAN-PRM-COMPUTED  
                                          PB-B-ISSUE-CNT-REMITTED   
                                          PB-B-ISSUE-CNT-ENTERED    
                                          PB-B-CANCEL-CNT-REMITTED  
                                          PB-B-CANCEL-CNT-ENTERED   
                                          PB-B-HIGHEST-SEQ-NO       
                                          PB-LF-BILLED-AMTS             
                                          PB-AH-BILLED-AMTS             
                                          PB-CHG-COUNT                  
                                          PB-CALC-TOLERANCE
           move spaces                 to pb-account-name
                                          pb-prem-ref-rpt-flag
                                          pb-reference
                                          
           MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT           
                                          PB-BILLED-DT                  
                                          PB-ACCT-EFF-DT                
                                          PB-ACCT-EXP-DT

           MOVE '9'                    TO PB-RECORD-TYPE
           move ws-iss-cnt             to pb-b-issue-cnt-remitted
           move ws-lf-iss-premium      to pb-b-lf-iss-prm-remitted
           move ws-ah-iss-premium      to pb-b-ah-iss-prm-remitted
           move ws-highest-seq-no      to pb-b-highest-seq-no
           move ws-bin-current-dt      to pb-b-received-dt
           move ws-bin-eom-dt          to pb-credit-select-dt
           move function upper-case(sql-py-Name)
                                       to PB-ACCOUNT-NAME

           .
       0100-EXIT.
           EXIT.

       0110-write-erpndb.

           display ' about to write pend bus ' pb-record-type

           EXEC CICS WRITE
               DATASET  ('ERPNDB')
               FROM     (PENDING-BUSINESS)
               RIDFLD   (PB-CONTROL-PRIMARY)
               RESP     (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              DISPLAY ' ERPNDB BTCH WRITE ERROR ' WS-RESPONSE
              move ' Error writing erpndb  '
                                 to output-msg
              set rollback-needed to true
              move ws-response         to ws-display-response
              move spaces              to ws-error-message
              string ' Bad write on ERPNDB FILE ' ws-display-response
                 delimited by size into ws-error-message
              end-string
              move 3             to ws-status-code
              perform 0226-update-remit-table
                                 thru 0226-exit
              perform 0225-commit-work
                                 thru 0225-exit
              go to 0000-return
           END-IF

           .
       0110-exit.
           exit.

       0120-write-erpndm.

           display ' about to write pend mail '

           move ws-init-erpndm         to pending-mailing-data

           move pb-control-primary     to pm-control-primary
           move function upper-case(sql-py-Name)
                                       to PM-CRED-BENE-NAME  
           move function upper-case(sql-py-Addr1)
                                       to PM-CRED-BENE-ADDR  
           move function upper-case(sql-py-Addr2)
                                       to PM-CRED-BENE-ADDR2 
           move function upper-case(sql-py-AddrCity)
                                       to PM-CRED-BENE-CITY
           move function upper-case(sql-py-AddrState)
                                       to pm-cred-bene-state
           move sql-py-AddrZIP         to pm-cred-bene-zip

           EXEC CICS WRITE
               DATASET  ('ERPNDM')
               FROM     (PENDING-MAILING-DATA)
               RIDFLD   (PM-CONTROL-PRIMARY)
               RESP     (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              move ' Error writing erpndm  '
                                       to output-msg
              set rollback-needed to true
              move ws-response         to ws-display-response
              move spaces              to ws-error-message
              string ' Bad write on ERPNDM FILE ' ws-display-response
                 delimited by size into ws-error-message
              end-string
              move 3                   to ws-status-code
              perform 0226-update-remit-table
                                       thru 0226-exit
              perform 0225-commit-work
                                       thru 0225-exit
              go to 0000-return
              DISPLAY ' ERPNDM WRITE ERROR ' WS-RESPONSE
           END-IF

           .
       0120-exit.
           exit.

       0130-start-edit.

           display ' about to start edit '

           MOVE ws-comp-cd             TO EDIT-COMPANY-CD
           MOVE WS-BATCH-NO            TO EDIT-BATCH
           MOVE ws-comp-id             TO EDIT-COMPANY-ID
           MOVE SPACES                 TO EDIT-RESTART-BATCH

           EXEC CICS START
                TRANSID       ('EXEB')
                FROM          (BATCH-TO-PROCESS)
           END-EXEC

           EXEC CICS DELAY
              INTERVAL    (02)
           END-EXEC

           .
       0130-exit.
           exit.

       0220-send-form.

           move ws-comp-id             to out-comp-id
           move 'CU CONNECT '          to output-title
           display ' about to send form '

           exec cics document create
              doctoken   (w-doctoken)
              template   ('WCHECKS')
              symbollist (output-data)
              listlength (length of output-data)
           end-exec

           exec cics web send
              doctoken(w-doctoken)
           end-exec

           .
       0220-exit.
           exit.

       0225-commit-work.

           display ' about to commit work   '

           if connected-to-db
              EXEC SQL
                  commit work
              END-EXEC
           end-if

           .
       0225-exit.
           exit.

       0226-update-remit-table.

           if ws-batch-no = zeros
              move -1                  to nu-batchno
           else
              move +0                  to nu-batchno
           end-if

           if ws-error-message = spaces
              move -1                  to nu-error-message
           else
              move +0                  to nu-error-message
           end-if

           EXEC SQL
              UPDATE
                 CUC_Logic_Remittance
              SET
                 LogicStatus = :ws-status-code,
                 LogicStatusDate = :ws-status-date,
                 BatchNumber = :ws-batch-no :nu-batchno,
                 ErrorMessage = :ws-error-message :nu-error-message
              WHERE
                 RemitId = :ws-remit-id
           END-EXEC

           .
       0226-exit.
           exit.

       0230-disconnect.

           display ' about to disconnect DB '

           if connected-to-db
              EXEC SQL
                  disconnect all
              END-EXEC
              move ' ' to ws-connect-sw
           end-if

           .
       0230-exit.
           exit.

       0240-init-erpndm.

           display ' made it to init pndm '

           move spaces                 to pending-mailing-data
           move 'PM'                   to pm-record-id
           move zeros                  to pm-batch-seq-no
                                          pm-batch-chg-seq-no
                                          pm-last-maint-hhmmss
                                          pm-insured-issue-age
                                          pm-phone-no
           move low-values             to pm-record-add-dt
                                          pm-last-maint-dt
                                          pm-insured-birth-dt
                                          pm-joint-birth-dt

           perform varying s1 from +1 by +1 until s1 > +7
              move low-values          to pm-mail-date (s1)
           end-perform

           move 'CR'                   to pm-source-system
           move ws-bin-current-dt      to pm-record-add-dt
                                          pm-last-maint-dt
           move 'E202'                 to pm-last-maint-by
                                          pm-record-added-by
           move ws-work-time           to pm-last-maint-hhmmss
           move pending-mailing-data   to ws-init-erpndm

           .
       0240-exit.
           exit.

       9700-DATE-CONVERT.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.
