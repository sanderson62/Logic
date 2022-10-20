      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       ID DIVISION.                                                     
                                                                        
       PROGRAM-ID.    WF001.
      *                                                                 
      *AUTHOR.     CSO
      *            OMAHA, NEBRASKA.                                     
                                                                        
      *DATE-COMPILED.                                                   
                                                                        
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *                                                                *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO             *
      *            *                                                   *
      *            *****************************************************
                                                                        
      *REMARKS.   This program is called by on-line programs that add
      *  a new claim or a new pending issue record. It then adds the
      *  appropriate entry to table Logic_OB_Workflow.  Okay, if coming
      * from el051 it only makes sense to connect one time because you
      * could have hundres of issues and you wouldn't want to connect
      * and disconnect that many time. That's why this program connects
      * only when being called by EL130.cl2 because we are only dealing
      * with one record.  Make sense? 
      
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
110921* 110921  CR2021051200001  PEMA  New program
110921* 110921  CR2021051200001  PEMA  Onbase Workflow project
      ******************************************************************
      
       ENVIRONMENT DIVISION.                                            
                                                                        
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
      
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*    WF001 WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'. 
      
       77  s1                          pic s999 comp-3 value +0.
       77  ws-pyaj-browse-sw           pic x value spaces.
           88  pyaj-browse-started        value 'Y'.
       77  ws-browse-sw                pic x value spaces.
           88  i-say-when                 value 'Y'.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  ws-bin-current-dt           pic xx value low-values.
       77  ws-comp-cd                  pic x.
      
       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
      
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
      
       01  ws-work-flow-area.
           05  wf-company-id           pic xxx.
           05  wf-carrier              pic x.
           05  wf-grouping             pic x(6).
           05  wf-state                pic xx.
           05  wf-account              pic x(10).
           05  wf-eff-date             pic x(10).
           05  wf-cert-no              pic x(11).
           05  wf-claim-no             pic x(07).
           05  wf-exp-date             pic x(10).
           05  wf-last-name            pic x(30).
           05  wf-first-name           pic x(30).
           05  wf-mid-init             pic x.
           05  wf-status               pic x.
      
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
      
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
           05  nu-claim-no             pic s9(4) comp value +0.
           05  nu-app-by               pic s9(4) comp value +0.
           05  nu-exp-date             pic s9(4) comp value +0.
           05  nu-app-batch            pic s9(4) comp value +0.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  WS-MISC-AREA.
           05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    
           05  SAVE-CURRENT-DATE-MDY       PIC X(6)    VALUE SPACES.    
           05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.    
                                                                        
       01  STANDARD-AREAS.                                              
           12  ELCERT-KEY.
               16  CERT-COMP-CD    PIC X.
               16  CERT-CARRIER    PIC X.
               16  CERT-GROUPING   PIC X(6).
               16  CERT-STATE      PIC X(2).
               16  CERT-ACCOUNT    pic x(10).
               16  CERT-EFF-DT     PIC XX.
               16  CERT-CERT-NO    pic x(11).

           12  ws-connect-sw               pic x  value ' '.
               88  connected-to-db             value 'Y'.
           12  THIS-PGM                    PIC X(8)    VALUE 'WF001'.
           12  PGM-NAME                    PIC X(8)   VALUE SPACES.     
                                                                        
       01  ws-pass-area.
           05  pa-rec-type             pic x(4).
           05  pa-company-id           pic xxx.
           05  pa-rest-of-record       pic x(600).

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

                                       COPY ELCFUNDT.
                                       COPY ERCPNDB.
                                       COPY ELCCERT.
                                       COPY ELCMSTR.
                                       COPY ELCDATE.                

       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA.
           05  filler                  PIC X(1024).
      
       01  var  pic x(30).

       PROCEDURE DIVISION.                                              
                                                                        
           MOVE DFHCOMMAREA            to ws-pass-area.

      *    display ' Entering WF001 '
      
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

           display ' ENVIRONMENT ' ws-kix-myenv

           evaluate pa-company-id
              when 'FNL'
                 move X'08'            to ws-comp-cd
              when 'VPP'
                 move X'07'            to ws-comp-cd
              when 'DCC'
                 move X'05'            to ws-comp-cd
              when 'CID'
                 move X'04'            to ws-comp-cd
              when other
                 go to 0000-return
           end-evaluate

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           move ws-fn-cymd             to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-current-dt
           else
              display ' error current dt invalid ' dc-error-code
           end-if

           evaluate pa-rec-type
              when 'CRTS'  *> Multiple certificates
                 perform 0100-certs    thru 0100-exit
              when 'CERT'  *> Single certificate
                 perform 0100-certs    thru 0100-exit
              when 'CLMS'  *> Single claim
                 perform 0200-claim    thru 0200-exit
              when other
                 display ' DO WHAT?'
           end-evaluate

           .
       0000-return.

           goback

           .
       0100-certs.

           if pa-rec-type = 'CRTS'
              exec sql
                 set connection 'WFCERT'
              end-exec
           else
              perform 0400-connect-db  thru 0400-exit
           end-if

           move pa-rest-of-record      to pending-business
           move pa-company-id          to wf-company-id
           move pb-carrier             to wf-carrier
           move pb-grouping            to wf-grouping
           move pb-state               to wf-state
           move pb-account             to wf-account
           move pb-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit
                                       to wf-eff-date
           else
              move spaces              to wf-eff-date
           end-if
           move pb-cert-no             to wf-cert-no
           move spaces                 to wf-claim-no
           move -1                     to nu-claim-no
           move 'A'                    to wf-status
           move pb-i-lf-expire-dt      to dc-bin-date-1
           if pb-i-ah-expire-dt > dc-bin-date-1
              move pb-i-ah-expire-dt   to dc-bin-date-1
           end-if
           if ws-bin-current-dt > dc-bin-date-1
              move 'E'                 to wf-status
           end-if
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to wf-exp-date
              move +0                  to nu-exp-date
           else
              display 'bad exp dt ' dc-error-code
              move spaces              to wf-exp-date
              move -1                  to nu-exp-date
           end-if
           move pb-i-insured-last-name to wf-last-name
           move pb-i-insured-first-name
                                       to wf-first-name
           move pb-i-insured-middle-init
                                       to wf-mid-init
           perform 0300-insert         thru 0300-exit
           if pa-rec-type = 'CERT'
              perform 0500-disconnect-db
                                       thru 0500-exit
           end-if

           .
       0100-exit.
           exit.

       0200-claim.

           perform 0400-connect-db     thru 0400-exit
           move pa-rest-of-record      to claim-master
           move pa-company-id          to wf-company-id
           move ws-comp-cd             to cert-comp-cd
           move cl-carrier             to wf-carrier
                                          cert-carrier
           move cl-cert-grouping       to wf-grouping
                                          cert-grouping
           move cl-cert-state          to wf-state
                                          cert-state
           move cl-cert-account        to wf-account
                                          cert-account
           move cl-cert-eff-dt         to dc-bin-date-1
                                          cert-eff-dt
           move ' '                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to wf-eff-date
           else
              move spaces              to wf-eff-date
           end-if
           move cl-cert-no             to wf-cert-no
                                          cert-cert-no
           move cl-claim-no            to wf-claim-no
           move +0                     to nu-claim-no
           move spaces                 to wf-exp-date
           move -1                     to nu-exp-date
           move cl-insured-last-name   to wf-last-name
           move cl-insured-1st-name    to wf-first-name
           move cl-insured-mid-init    to wf-mid-init
           move 'O'                    to wf-status

           EXEC CICS READ
               DATASET  ('ELCERT')
               into     (CERTIFICATE-MASTER)
               RIDFLD   (ELCERT-KEY)
               resp     (ws-response)
           END-EXEC
           if resp-normal
              if cl-claim-type = 'L' OR 'O'
                 move cm-lf-loan-expire-dt
                                       to dc-bin-date-1
              else
                 move cm-ah-loan-expire-dt
                                       to dc-bin-date-1
              end-if
              move ' '                 to dc-option-code
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to wf-exp-date
                 move +0               to nu-exp-date
              end-if
           else
              display ' bad read elcert ' ws-response
           end-if

           perform 0300-insert         thru 0300-exit
           perform 0500-disconnect-db  thru 0500-exit

           .
       0200-exit.
           exit.

       0300-INSERT.

           EXEC SQL
              INSERT into Logic_OB_Workflow (
                 Company_Id,
                 Carrier   ,
                 Grouping  ,
                 State     ,
                 Account   ,
                 Crt_Eff_Dt,
                 Cert_No   ,
                 Claim_no  ,
                 Crt_Exp_Dt,
                 Last_Name ,
                 First_Name,
                 Mid_Init  ,
                 Status)
               VALUES (
                  :wf-company-id ,
                  :wf-carrier    ,  
                  :wf-grouping   ,  
                  :wf-state      ,  
                  :wf-account    ,  
                  :wf-eff-date   ,  
                  :wf-cert-no    ,  
                  :wf-claim-no :nu-claim-no,  
                  :wf-exp-date :nu-exp-date,
                  :wf-last-name  ,  
                  :wf-first-name ,  
                  :wf-mid-init   ,
                  :wf-status)
            END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row "
              display ' sql return code ' sqlcode
              move sqlcode to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              move sqlcode to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              goback
           end-if

           .
       0300-EXIT.
           EXIT.

       0400-connect-db.

           move 'HOV-TSTDB01_Workflow' to svr
           move 'appuser'              to usr
           move 'appuser@cso'          to pass
       
           if ws-kix-myenv = 'cid1p'
              move 'SDVDB01_Workflow'  to svr
              move 'appuser'           to usr
              move 'appuser@cso'       to pass
           end-if
       
           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string
       
           EXEC SQL
              CONNECT
                 TO :svr
                 USER :usr-pass
           END-EXEC
       
           if sqlcode not = 0
              display "Error: cannot connect "
              display sqlcode
              display sqlerrmc
              goback
           end-if
           set connected-to-db to true

           .
       0400-exit.
           exit.

       0500-disconnect-db.

           EXEC SQL
               commit work release
           END-EXEC
           if sqlcode not = 0
              display "Error: commit release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if
       
           EXEC SQL
              DISCONNECT
           END-EXEC
       
           if sqlcode not = 0
              display "Error: cannot disconnect Workflow "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0500-exit.
           exit.

       8500-DATE-CONVERT.

           MOVE 'ELDATCV'              TO  PGM-NAME

           EXEC CICS LINK
               PROGRAM    (PGM-NAME)
               COMMAREA   (DATE-CONVERSION-DATA)
               LENGTH     (DC-COMM-LENGTH)
           END-EXEC.

       8500-EXIT.
            EXIT.
