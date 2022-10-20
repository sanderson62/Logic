      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      *****************************************************************
      *                                                               *
      * Copyright (c) 2017 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLBELBENE.
       AUTHOR.   Cowtown.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      
           SELECT ELBENE           ASSIGN TO ELBENE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS BE-CONTROL-primary
                                   FILE STATUS IS ELBENE-FILE-STATUS.
      
           SELECT DISK-DATE        ASSIGN TO SYS019.
      
       DATA DIVISION.
       FILE SECTION.
      
       FD  ELBENE.
                                       copy ElCBENE.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       working-storage section.
      
       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
      
       77  s1                          pic s999 comp-3 value +0.
       77  rec-cnt                     pic 9(9) value zeros.
       77  ws-recs-in                  pic 9(7) value zeros.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  ELBENE-file-status          pic xx value low-values.
       77  eof-sw                      pic x value ' '.
           88  end-of-input               value 'Y'.
      
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
      
       01  filler.
           05  ws-work-date.
               10  ws-work-ccyy        pic 9999.
               10  ws-work-mm          pic 99.
               10  ws-work-dd          pic 99.
           05  ws-work-date-moyr       pic 9999  comp value zeros.
      
       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC
      
       01  sqlcmd                      pic x(5120).
       01  WS-MOE-DATE                 pic x(10).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
      
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to tell sql server that i am    ***
      ***  passing it a null value.  The indicator will be -1        ***
      ***  if the value is nulls and +0 if the value is other than   ***
      ***  nulls.  Here is a sample on how to use it.                ***
      ***                                                            ***
      ***      if db-date1 = spaces move -1 to nu-date1 end-if       *** 
      ***     EXEC SQL                                               ***
      ***        insert into ELBENE (                                ***
      ***           date1,                                           ***
      ***           date2)                                           ***
      ***        values (                                            ***
      ***           :db-date1      :nu-date1,                        ***
      ***           :db-date2      :nu-date2)                        ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
      
       01  indicator-vaiables-for-nulls.
           05  NU-VEHICLE-ID-NO        PIC s9(4) comp value +0.
           05  NU-AGENT-NAME           PIC s9(4) comp value +0.
           05  NU-LICENSE-NO           pic s9(4) comp value +0.
           05  NU-NATIONAL-PRODUCER-NO PIC s9(4) comp value +0.
           05  NU-AGT-SIG-VER-STATUS   PIC s9(4) comp value +0.

       01  ws-table-name               PIC X(10) VALUE SPACES.

       01  ELBENE-TABLE-RECORD.                                   
           05  tb-rec-type             pic x.                     
           05  TB-BENE-CD              PIC X(10).                 
           05  TB-BENE-NAME            PIC X(30).                 
           05  TB-BENE-ADDR1           PIC X(30).                 
           05  TB-BENE-ADDR2           PIC X(30).                 
           05  TB-BENE-ADDR3           PIC X(30).                 
           05  TB-BENE-CITY            PIC X(28).                 
           05  TB-BENE-STATE           PIC XX.                    
           05  TB-BENE-ZIP             PIC X(9).                  
           05  TB-BENE-PHONE           PIC X(11).                 
           05  tb-group-checks         pic x.                     
           05  TB-CORR-NAME            PIC X(30).                 
           05  TB-CORR-ADDR1           PIC X(30).                 
           05  TB-CORR-ADDR2           PIC X(30).                 
           05  TB-CORR-ADDR3           PIC X(30).                 
           05  TB-CORR-CITY            PIC X(28).                 
           05  TB-CORR-STATE           PIC XX.                    
           05  TB-CORR-ZIP             PIC X(9).                  
           05  TB-CORR-PHONE           PIC X(11).                 
           05  tb-fax-no               pic x(11).                 
           05  TB-ach-ind              PIC X.                     
           05  TB-ach-routing          PIC X(30).                 
           05  TB-ach-acct-no          PIC X(30).                 
           05  TB-ach-sub-type         PIC XX.                    
           05  TB-ach-email-ind        PIC X.                     
           05  tb-ach-email-addr       pic x(35).                 
                                                                  
       EXEC SQL                                                   
          END DECLARE SECTION                                     
       END-EXEC
      
       01  FILLER.
           05  ABEND-CODE              PIC X(4)  VALUE SPACES.
           05  ABEND-OPTION            PIC X     VALUE 'Y'.
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-ABEND-FILE-STATUS    PIC XX    VALUE ZERO.
           05  WS-RETURN-CODE          PIC S9(3) VALUE ZERO COMP-3.
           05  PGM-SUB                 PIC S999  VALUE +344 COMP-3.
      
                                       COPY ELCDTECX.
                                       copy ELCDTEVR.
                                       COPY ELCDATE.
      
       LINKAGE SECTION.                                                 

       01  var  pic x(30).
      
       procedure division.
                                       COPY ELCDTERX.
       0000-begin.

           display ' Begin Program SQLBELBENE '

           perform 0010-init           thru 0010-exit
           perform 2000-connect-to-logic
                                       thru 2000-exit

      *    perform 1010-drop-table     thru 1010-exit
           perform 1020-truncate-table thru 1020-exit
      
      *    perform 1000-create-table   thru 1000-exit
      
           perform 0020-open-files     thru 0020-exit
      
           EXEC SQL
              SET AUTOCOMMIT OFF
           END-EXEC
      
           perform 0030-start-input    thru 0030-exit
           perform 0046-read-input     thru 0046-exit

           perform 0045-process-input  thru 0045-exit until
              end-of-input
      *      or ws-recs-in > 10000
      
           perform 0060-finish-up      thru 0060-exit
           close ELBENE
           display ' End Program '
           display ' rows inserted ' rec-cnt
           display ' records read  ' ws-recs-in
           goback
      
           .
       0010-init.

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
      
           display ' KIXSYS  ' ws-kix-myenv

           evaluate dte-client
              when 'DCC'
                 move 'DCC_ELBENE'     to ws-table-name
              when 'CID'
                 move 'ELBENE'     to ws-table-name
              when 'VPP'
                 move 'VPP_ELBENE'     to ws-table-name
           end-evaluate

           .
       0010-exit.
           exit.
      
       0020-open-files.
      
           open input ELBENE
           if ELBENE-file-status not = '00'
              display ' error-ELBENE-open ' ELBENE-file-status
              perform abend-pgm
           end-if
      
           .
       0020-exit.
           exit.
      
       0030-start-input.

           move low-values             to be-control-primary
           move dte-clasic-company-cd  to be-company-cd
           start ELBENE key >= be-control-primary
           if ELbene-file-status = '10' or '23'
              set end-of-input to true
           else
              if ELbene-file-status not = '00'
                 display ' error-ELbene-start ' ELbene-file-status
                 perform abend-pgm
              end-if
           end-if
      
           .
       0030-exit.
           exit.
      
       0045-process-input.
      
           if be-record-type = 'B'
              perform 0050-insert-row  thru 0050-exit
           end-if

           perform 0046-read-input     thru 0046-exit
      
           .
       0045-exit.
           exit.
      
       0046-read-input.
      
           read ELBENE next record
           if (elbene-file-status = '10' or '23')
              or (dte-clasic-company-cd not = be-company-cd)
              set end-of-input to true
           else
              if elbene-file-status not = '00'
                 display ' error-ELBENE-read ' ELBENE-file-status
                 perform abend-pgm
              end-if
           end-if
      
           .
       0046-exit.
           exit.
      
       0050-insert-row.
      
           perform 0052-build-values   thru 0052-exit
           perform 0057-insert-row     thru 0057-exit
      
           .
       0050-exit.
           exit.
      
       0052-build-values.

           move spaces                 to elbene-table-record

           move be-record-type         to tb-rec-type
           move be-beneficiary         to TB-BENE-CD
           inspect be-mail-to-name
              converting '''' to spaces
           move be-mail-to-name        to TB-BENE-NAME
           inspect be-address-line-1
              converting '''' to spaces
           move be-address-line-1      to TB-BENE-ADDR1       
           inspect be-address-line-2
              converting '''' to spaces
           move be-address-line-2      to TB-BENE-ADDR2       
           move be-address-line-3      to TB-BENE-ADDR3       
           inspect be-city
              converting '''' to spaces
           move be-city                to TB-BENE-CITY        
           move be-state               to TB-BENE-STATE       
           move be-zip-code            to TB-BENE-ZIP         
           move be-phone-no            to TB-BENE-PHONE
           move be-group-checks-y-n    to tb-group-checks
           inspect be-mail-to-name2
              converting '''' to spaces
           move be-mail-to-name2       to TB-CORR-NAME        
           inspect be-address-line-12
              converting '''' to spaces
           move be-address-line-12     to TB-CORR-ADDR1       
           inspect be-address-line-22
              converting '''' to spaces
           move be-address-line-22     to TB-CORR-ADDR2       
           inspect be-address-line-32
              converting '''' to spaces
           move be-address-line-32     to TB-CORR-ADDR3       
           inspect be-city2
              converting '''' to spaces
           move be-city2               to TB-CORR-CITY        
           move be-state2              to TB-CORR-STATE       
           move be-zip-code2           to TB-CORR-ZIP         
           move be-phone-no2           to TB-CORR-PHONE
           move be-bsr-fax-num         to tb-fax-no
           inspect be-ach-yes-or-no
              converting low-values to spaces
           move be-ach-yes-or-no       to TB-ach-ind
           inspect be-ach-aba-routing-number
              converting low-values to spaces
           move be-ach-aba-routing-number
                                       to TB-ach-routing
           inspect be-ach-bank-account-number
              converting low-values to spaces
           move be-ach-bank-account-number
                                       to TB-ach-acct-no
           inspect be-ach-sub-type
              converting low-values to spaces
           move be-ach-sub-type        to TB-ach-sub-type
           inspect be-ach-email-yn
              converting low-values to spaces
           move be-ach-email-yn        to tb-ach-email-ind
           inspect be-ach-email-addr
              converting low-values to spaces
           move be-ach-email-addr      to tb-ach-email-addr

           .
       0052-exit.
           exit.
      
       0057-insert-row.

           move spaces                 to sqlcmd

           string
              'INSERT INTO '
               ws-table-name
               ' (BENE_REC_TYPE, BENE_CD, BENE_NAME, BENE_ADDR1,'
               ' BENE_ADDR2,'
               ' BENE_ADDR3, BENE_CITY, BENE_STATE, BENE_ZIP,'
               ' BENE_PHONE, BENE_GRP_CHECKS, CORR_NAME, CORR_ADDR1,'
               ' CORR_ADDR2,'
               ' CORR_ADDR3, CORR_CITY, CORR_STATE, CORR_ZIP,'
               ' CORR_PHONE, FAX_NO, ACH_INDICATOR, ACH_ROUTING,'
               ' ACH_ACCTNO,'
               ' ACH_SUB_TYPE, ACH_EMAIL_IND, ACH_EMAIL_ADDR)'
              ' VALUES ('
                "'" tb-rec-type "', '"
                    tb-bene-cd "', '"
                    tb-bene-name "', '"
                    tb-bene-addr1 "', '"
                    TB-BENE-ADDR2   "', '"   
                    TB-BENE-ADDR3   "', '"   
                    TB-BENE-CITY    "', '"   
                    TB-BENE-STATE   "', '"   
                    TB-BENE-ZIP     "', '"   
                    TB-BENE-PHONE   "', '"   
                    TB-group-checks "', '"   
                    TB-CORR-NAME    "', '"   
                    TB-CORR-ADDR1   "', '"   
                    TB-CORR-ADDR2   "', '"   
                    TB-CORR-ADDR3   "', '"   
                    TB-CORR-CITY    "', '"   
                    TB-CORR-STATE   "', '"   
                    TB-CORR-ZIP     "', '"   
                    TB-CORR-PHONE   "', '"   
                    TB-fax-no       "', '"   
                    TB-ACH-IND      "', '"   
                    TB-ACH-ROUTING  "', '"   
                    TB-ACH-ACCT-NO  "', '"   
                    TB-ACH-SUB-TYPE "', '"
                    TB-ach-email-ind "', '"   
                    TB-ach-email-addr "')"
              delimited by size into sqlcmd
           end-string

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row " sqlcmd
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' in out cnt ' ws-recs-in ' ' rec-cnt
              display ' offending rec ' ELBENE-table-record
           else
              add 1 to rec-cnt
           end-if
      
           .
       0057-exit.
           exit.
      
       0060-finish-up.
      
           EXEC SQL
               commit work release
           END-EXEC
      
           if sqlcode not = 0
              display "Error: commit work release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if
      
           EXEC SQL
              DISCONNECT
           END-EXEC
      
           if sqlcode not = 0
              display "Error: disconnect  "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if
      
           .
       0060-exit.
           exit.
      
       1000-create-table.

           move spaces                 to sqlcmd

           string
              'create table '
              ws-table-name       ' ('
                 'BENE_REC_TYPE    char(1)     not null,   '    
                 'BENE_CD          char(10)    not null,   '
                 'BENE_NAME        varchar(30) null,       '
                 'BENE_ADDR1       varchar(30) null,       '
                 'BENE_ADDR2       varchar(30) null,       '
                 'BENE_ADDR3       varchar(30) null,       '
                 'BENE_CITY        varchar(28) null,       '
                 'BENE_STATE       char(2)     null,       '
                 'BENE_ZIP         varchar(9)  null,       '
                 'BENE_PHONE       varchar(11) null,       '
                 'BENE_GRP_CHECKS  char(1)     null,       '
                 'CORR_NAME        varchar(30) null,       '
                 'CORR_ADDR1       varchar(30) null,       '
                 'CORR_ADDR2       varchar(30) null,       '
                 'CORR_ADDR3       varchar(30) null,       '
                 'CORR_CITY        varchar(28) null,       '
                 'CORR_STATE       char(2)     null,       '
                 'CORR_ZIP         varchar(9)  null,       '
                 'CORR_PHONE       varchar(11) null,       '
                 'FAX_NO           varchar(11) null,       '
                 'ACH_INDICATOR    char(1)     null,       '
                 'ACH_ROUTING      varchar(30) null,       '
                 'ACH_ACCTNO       varchar(30) null,       '
                 'ACH_SUB_TYPE     char(2)     null,       '
                 'ACH_EMAIL_IND    char(1)     null,       '
                 'ACH_EMAIL_ADDR   varchar(35) null,       '
                 'constraint PK_' ws-table-name
                 ' PRIMARY KEY CLUSTERED'
                 '(BENE_REC_TYPE, BENE_CD)'
                 ')'
                delimited by size into sqlcmd
           end-string

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC
      
           if sqlcode not = 0
              display "Error : cannot Create table - " ws-table-name
              move sqlcode             to ws-disp-code
              display ' sql return code ' ws-disp-code
              display ' sql err mess    ' sqlerrmc
              goback
           end-if
      
           .
       1000-exit.
           exit.
      
       1010-drop-table.
      
           move spaces                 to sqlcmd
           string
              'drop table '
              ws-table-name
              delimited by size into sqlcmd
           end-string

           EXEC SQL
              EXECUTE IMMEDIATE :sqlcmd
           END-EXEC

           if sqlcode not = 0
              display "Error(anticipated) : cannot drop table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if
      
           .
       1010-exit.
           exit.
      
       1020-truncate-table.
      
           move spaces to sqlcmd
           string
              ' truncate table '
              ws-table-name
              delimited by size into sqlcmd
           end-string

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC
      
           if sqlcode not = 0
              display "Error : cannot truncate table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if
      
           .
       1020-exit.
           exit.
      
       2000-connect-to-logic.
      
           move 'NTSQLTST2_Logic'      to svr
           move 'sa'                   to usr
           move 'sql2008r2'            to pass
      
           if ws-kix-myenv = 'cid1p'
              move 'NTCSO2_Logic'      to svr
              move 'sa'                to usr
              move 'ntcso2'            to pass
           end-if
      
           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string
      
           EXEC SQL
              SET OPTION logintime 5
           END-EXEC
      
           EXEC SQL
              CONNECT TO :svr
                    USER :usr-pass
           END-EXEC
      
           if sqlcode not = 0
              display "Error: cannot connect to Logic"
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              perform abend-pgm
           end-if
      
           .
       2000-exit.
           exit.
      
       8500-DATE-CONVERT.              
                                       COPY ELCDCS.
      
       abend-pgm.
      
            call 'ABORTME'.
            
            goback.
