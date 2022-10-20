      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. sqlroll24b.
       AUTHOR.   Pablo.

063022******************************************************************
063022*                   C H A N G E   L O G
063022*
063022* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
063022*-----------------------------------------------------------------
063022*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
063022* EFFECTIVE    NUMBER
063022*-----------------------------------------------------------------
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
063022******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILE-IN          ASSIGN TO SYS010
                                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  FILE-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  FILE-IN-REC                PIC X(249).

       working-storage section.

       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
      
       77  rec-cnt                     pic 9(7) value zeros.
       77  ws-recs-in                  pic 9(7) value zeros.
       77  eof-sw                      pic x value ' '.
           88  end-of-input               value 'Y'.

063022 01  P pointer.
063022 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
063022 01  var-ptr pointer.
063022 01  env-var-len                 pic 9(4)  binary.
063022 01  rc                          pic 9(9)  binary.
063022
063022 01  WS-KIXSYS.
063022     05  WS-KIX-FIL1             PIC X(10).
063022     05  WS-KIX-APPS             PIC X(10).
063022     05  WS-KIX-ENV              PIC X(10).
063022     05  WS-KIX-MYENV            PIC X(10).
063022     05  WS-KIX-SYS              PIC X(10).
      
       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

       01  EXTRACT-RECORD.
           05  EXT-CARRIER             PIC X.
           05  F                       PIC X.
           05  EXT-GROUP               PIC X(6).
           05  F                       PIC X.
           05  EXT-STATE               PIC XX.
           05  F                       PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  F                       PIC X.
           05  ext-exp-dt              pic x(10).
           05  f                       pic x.
           05  ext-eff-dt              pic x(10).
           05  F                       PIC X.
           05  EXT-YEAR                PIC 9(4).
           05  f                       pic x.
           05  EXT-MONTH               PIC 99.
           05  F                       PIC X.
           05  EXT-REPORT-CDE1         PIC X(10).
           05  F                       PIC X.
           05  EXT-REPORT-CDE2         PIC X(10).
           05  F                       PIC X.
           05  EXT-REPORT-CDE3         PIC X(10).
           05  F                       PIC X.
           05  EXT-LF-BEN              PIC X(14).
           05  F                       PIC X.
           05  EXT-LF-PREM             PIC X(12).
           05  F                       PIC X.
           05  EXT-LF-CLMS             PIC X(12).
           05  F                       PIC X.
           05  EXT-AH-BEN              PIC X(14).
           05  F                       PIC X.
           05  EXT-AH-PREM             PIC X(12).
           05  F                       PIC X.
           05  EXT-AH-CLMS             PIC X(12).
           05  F                       PIC X.
           05  EXT-TOT-PREM            PIC X(14).
           05  F                       PIC X.
           05  EXT-TOT-COMM            PIC X(12).
           05  F                       PIC X.
           05  EXT-NET-COUNT           PIC X(9).
           05  F                       PIC X.
           05  ext-account-name        pic x(30).
           05  F                       PIC X.
           05  EXT-HI-CERT-DT          PIC X(10).
           05  F                       PIC X.
           05  EXT-EOR                 PIC X.
       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       LINKAGE SECTION.
       01  var  pic x(30).

       procedure division.
       0000-begin.

           display ' Begin Program '

063022     set P to address of KIXSYS
063022     CALL "getenv" using by value P returning var-ptr
063022     if var-ptr = null then
063022        display ' kixsys not set '
063022     else
063022        set address of var to var-ptr
063022        move 0 to env-var-len
063022        inspect var tallying env-var-len
063022          for characters before X'00' 
063022        unstring var (1:env-var-len) delimited by '/'
063022           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
063022              WS-KIX-SYS
063022        end-unstring
063022     end-if
063022
063022     display ' KIXSYS  ' ws-kix-myenv

           perform 0010-init           thru 0010-exit
           perform 0020-connect        thru 0020-exit
      *    perform 0030-drop-table     thru 0030-exit
      *    perform 0040-create-table   thru 0040-exit
           perform 0035-truncate-table thru 0035-exit

           open input file-in

           EXEC SQL
              SET AUTOCOMMIT OFF
           END-EXEC

           perform 0046-read-input     thru 0046-exit
           perform 0045-process-input  thru 0045-exit until
              end-of-input
      *      or ws-recs-in > 10000

           perform 0060-finish-up      thru 0060-exit
           close file-in
           display ' End Program '
           display ' rows inserted ' rec-cnt
           display ' records read  ' ws-recs-in
           goback

           .
       0010-init.

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
              when 'mdoff'
                 move '//hov-tstdb01.cso.local:55330;'
                                       to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate

           move 'Logic'                to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc


           if sqlcode not = 0
              display "Error: cannot connect "
              display sqlcode
              display sqlerrmc
              goback
           end-if

           display " connect to DB successful "

           .
       0020-exit.
           exit.

       0030-drop-table.

           display 'Begin Drop table'
           EXEC SQL
               drop table ROLL24_CPS
           END-EXEC
           if sqlcode not = 0
              display "Error(anticipated) : cannot drop table "
              display sqlcode
              display sqlerrmc
      *       goback
           end-if

           display ' sql return code ' sqlcode
           display ' sql err mess    ' sqlerrmc

           .
       0030-exit.
           exit.

       0035-truncate-table.

           display '*** Begin Truncate table ***'
           EXEC SQL
NTTDel*        truncate table ROLL24_CPS
NTTIns         EXECUTE IMMEDIATE 'truncate table ROLL24_CPS'
           END-EXEC
           if sqlcode not = 0
              display "Error : cannot truncate table "
              display sqlcode
              display sqlerrmc
              goback
           end-if

           display ' sql return code ' sqlcode
           display ' sql err mess    ' sqlerrmc
           display '***  End  Truncate table ***'

           .
       0035-exit.
           exit.

       0040-create-table.

           display ' Begin Create table'
           EXEC SQL
              create table ROLL24_CPS (
	              carrier char(1),
	              grouping char(6),
	              state char(2),
	              account char(10),
	              exp_date datetime,
	              eff_date datetime,
	              year int,
	              month int,
	              report_cd1 char(10),
	              report_cd2 char(10),
	              report_cd3 char(10),
	              lf_net_benefits decimal(13,2),
	              lf_net_premium decimal(11,2),
	              lf_claims decimal(11,2),
	              ah_net_benefits decimal(13,2),
	              ah_net_premium decimal(11,2),
	              ah_claims decimal(11,2),
	              tot_net_premium decimal(13,2),
	              tot_net_acct_commission decimal(11,2),
	              net_count decimal(9,0),
	              account_name varchar(30),
	              hi_cert_date datetime)
           END-EXEC
           if sqlcode not = 0
              display "Error: cannot create table "
              move sqlcode             to ws-disp-code
              display ' sql return code ' ws-disp-code
              display ' sql err mess    ' sqlerrmc
              goback
           end-if

           display " Create table successful "

           .
       0040-exit.
           exit.

       0045-process-input.

           perform 0050-insert-row     thru 0050-exit
           perform 0046-read-input     thru 0046-exit

           .
       0045-exit.
           exit.

       0046-read-input.

           read file-in at end
              set end-of-input         to true
           end-read
           
           if not end-of-input
              move file-in-rec         to extract-record
              add 1                    to ws-recs-in
           end-if


           .
       0046-exit.
           exit.


       0050-insert-row.

      *    display ' Begin Insert row '

           EXEC SQL
              insert into ROLL24_CPS (
	              carrier,
	              grouping,
	              state,
	              account,
	              exp_date,
	              eff_date,
	              year,
	              month,
	              report_cd1,
	              report_cd2,
	              report_cd3,
	              lf_net_benefits,
	              lf_net_premium,
	              lf_claims,
	              ah_net_benefits,
	              ah_net_premium,
	              ah_claims,
	              tot_net_premium,
	              tot_net_acct_commission,
	              net_count,
	              account_name,
	              hi_cert_date)
	              values (
                  :EXT-CARRIER,
                  :EXT-GROUP,
                  :EXT-STATE,
                  :EXT-ACCOUNT,
                  :ext-exp-dt,
                  :ext-eff-dt,
                  :EXT-YEAR,
                  :EXT-MONTH,
                  :EXT-REPORT-CDE1,
                  :EXT-REPORT-CDE2,
                  :EXT-REPORT-CDE3,
                  :EXT-LF-BEN,
                  :EXT-LF-PREM,
                  :EXT-LF-CLMS,
                  :EXT-AH-BEN,
                  :EXT-AH-PREM,
                  :EXT-AH-CLMS,
                  :EXT-TOT-PREM,
                  :EXT-TOT-COMM,
                  :EXT-NET-COUNT,
                  :ext-account-name,
                  :EXT-HI-CERT-DT)
           END-EXEC
           if sqlcode not = 0
              display "Error: cannot insert row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' recs so far ' rec-cnt
              display ' offending rec ' extract-record
              goback
           end-if

           add 1 to rec-cnt

           .
       0050-exit.
           exit.

       0060-finish-up.

           display ' Begin Commit '
           EXEC SQL
NTTDel*        commit transaction
NTTIns         commit
           END-EXEC
           if sqlcode not = 0
              display "Error: commit "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              goback
           end-if
      
           display " Commit trans successful "

           display ' Begin Disconnect '
           EXEC SQL
NTTDel*        commit work release
NTTIns         DISCONNECT ALL
           END-EXEC
           if sqlcode not = 0
              display "Error: commit release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0060-exit.
           exit.

