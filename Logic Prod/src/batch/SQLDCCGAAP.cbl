      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. sqldccgaap.
       AUTHOR.   Pablo.

      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************

070115******************************************************************
070115*                   C H A N G E   L O G
070115*
070115* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070115*-----------------------------------------------------------------
070115*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070115* EFFECTIVE    NUMBER
070115*-----------------------------------------------------------------
070115* 070115  IR2015070100001  PEMA  ADD NULL IND TO INSERT STMT
111317* 111317  IR2017111300001  PEMA  Add code to check for re-run situation
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
070115******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILE-IN          ASSIGN TO SYS010
                                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  FILE-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  FILE-IN-REC                PIC X(410).

       FD  DISK-DATE
                                    COPY ELCDTEFD.

       working-storage section.

       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
      
       77  rec-cnt                     pic 9(7) value zeros.
       77  ws-recs-in                  pic 9(7) value zeros.
       77  eof-sw                      pic x value ' '.
           88  end-of-input               value 'Y'.
111317 77  ws-sql-code                 pic s9(7) value zeros.
111317 77  ws-dis-sql-code             pic -9999999 value zeros.

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  sqlcmd                      pic x(1024).
       01  WS-MOE-DATE                 pic x(10).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

111317 01  EXT-MOE-DATE                PIC X(10).
111317 01  ws-rec-cntr                 pic s9(4) comp value +0.
111317 01  ws-test-date                pic x(10) value spaces.

070115***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
070115***                                                            ***
070115***  These indicators are used to tell sql server that i am    ***
070115***  passing it a null value.  The indicator will be -1        ***
070115***  if the value is nulls and +0 if the value is other than   ***
070115***  nulls.  Here is a sample on how to use it.                ***
070115***                                                            ***
070115***      if db-date1 = spaces move -1 to nu-date1 end-if       *** 
070115***     EXEC SQL                                               ***
070115***        insert into TABLE1 (                                ***
070115***           date1,                                           ***
070115***           date2)                                           ***
070115***        values (                                            ***
070115***           :db-date1      :nu-date1,                        ***
070115***           :db-date2      :nu-date2)                        ***
070115***     END-EXEC                                               ***
070115***                                                            ***
070115***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
070115
070115 01  indicator-vaiables-for-nulls.
070115     05  NU-LF-EXP-DT            PIC s9(4) comp value +0.
070115     05  NU-AH-EXP-DT            PIC s9(4) comp value +0.

       01  GAAP-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-EFF                  PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB6                 PIC X.
           12  EX-LFTYP                PIC XX.
           12  EX-TAB7                 PIC X.
           12  EX-LF-TERM              PIC 999.
           12  EX-TAB8                 PIC X.
           12  EX-LF-REMTERM           PIC 999.
           12  EX-TAB9                 PIC X.
           12  EX-LFBEN                PIC x(13).
           12  EX-TAB10                PIC X.
           12  EX-LFPRM                PIC x(11).
           12  EX-TAB11                PIC X.
           12  EX-AHTYP                PIC XX.
           12  EX-TAB12                PIC X.
           12  EX-AH-TERM              PIC 999.
           12  EX-TAB13                PIC X.
           12  EX-AH-REMTERM           PIC 999.
           12  EX-TAB14                PIC X.
           12  EX-AHBEN                PIC x(11).
           12  EX-TAB15                PIC X.
           12  EX-AHPRM                PIC x(11).
           12  EX-TAB16                PIC X.
           12  EX-REM-AMT              PIC x(12).
           12  EX-TAB17                PIC X.
           12  EX-PLFPRM               PIC x(11).
           12  EX-TAB18                PIC X.
           12  EX-RLFPRM               PIC x(11).
           12  EX-TAB19                PIC X.
           12  EX-SLFPRM               PIC x(11).
           12  EX-TAB20                PIC X.
           12  EX-PAHPRM               PIC x(11).
           12  EX-TAB21                PIC X.
           12  EX-RAHPRM               PIC x(11).
           12  EX-TAB22                PIC X.
           12  EX-SAHPRM               PIC x(11).
           12  EX-TAB23                PIC X.
           12  EX-REIN                 PIC X.
           12  EX-TAB24                PIC X.
           12  EX-REIN-CO              PIC X(03).
           12  EX-TAB25                PIC X.
           12  EX-AH-REM-BEN           PIC x(13).
011604     12  EX-TAB26                PIC X.
011604     12  EX-MORT-RESV            PIC x(11).
031704     12  EX-TAB27                PIC X.
031704     12  EX-REIN-SUB             PIC X(03).
           12  EX-TAB28                PIC X.
           12  EX-SEX-CODE             PIC X.
           12  EX-TAB29                PIC X.
           12  EX-LOAN-TERM            PIC 999.
           12  EX-TAB30                PIC X.
           12  EX-LF-EXP-DT            PIC X(10).
           12  EX-TAB31                PIC X.
           12  EX-AH-EXP-DT            PIC X(10).
           12  EX-TAB32                PIC X.
           12  EX-LF-UP-REMTERM        PIC 999.
           12  EX-TAB33                PIC X.
           12  EX-AH-UP-REMTERM        PIC 999.
           12  EX-TAB34                PIC X.
           12  EX-UNLD-STAT-RESV       PIC x(11).
           12  EX-TAB35                PIC X.
           12  EX-LFCOM                PIC x(09).
           12  EX-TAB36                PIC X.
           12  EX-PLFCOM               PIC x(9).
           12  EX-TAB37                PIC X.
           12  EX-RLFCOM               PIC x(9).
           12  EX-TAB38                PIC X.
           12  EX-SLFCOM               PIC x(9).
           12  EX-TAB39                PIC X.
           12  EX-AHCOM                PIC x(9).
           12  EX-TAB40                PIC X.
           12  EX-PAHCOM               PIC x(9).
           12  EX-TAB41                PIC X.
           12  EX-RAHCOM               PIC x(9).
           12  EX-TAB42                PIC X.
           12  EX-SAHCOM               PIC x(9).
           12  EX-TAB43                PIC X.
           12  EX-MO-DEC               PIC x(11).
           12  EX-TAB44                PIC X.
           12  EX-MORT-FACT            PIC x(11).
           12  EX-TAB45                PIC X.
           12  EX-MORT-AGE             PIC 999.
           12  EX-TAB46                PIC X.
           12  EX-MORT-TABLE           PIC XXXX.
           12  EX-TAB47                PIC X.
           12  EX-ALT-MORT-TABLE       PIC XXXX.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  FILLER.
           05  ABEND-CODE                  PIC X(4) VALUE SPACES.
           05  ABEND-OPTION                PIC X    VALUE 'Y'.
           05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.
           05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.
           05  WS-RETURN-CODE       COMP-3 PIC S9(3)   VALUE ZERO.
           05  PGM-SUB                     PIC S999 COMP-3 VALUE +344. 

                                       COPY ELCDTECX.
                                       copy ELCDTEVR.
                                       COPY ELCDATE.

       procedure division.
                                       COPY ELCDTERX.
       0000-begin.

           display ' Begin Program '
           perform 0010-init           thru 0010-exit
           perform 0020-connect        thru 0020-exit

111317     perform 0025-check-for-rerun thru 0025-exit
111317     if ws-rec-cntr > 0
111317        display ' This must be a re-run, about to delete '
111317        perform 1045-delete-rows thru 1045-exit
111317     end-if

      *    perform 0030-drop-table     thru 0030-exit
      *    perform 0040-create-table   thru 0040-exit
           open input file-in

           EXEC SQL
              SET AUTOCOMMIT OFF
           END-EXEC

           perform 0046-read-input     thru 0046-exit
           perform 0045-process-input  thru 0045-exit until
              end-of-input
      *      or ws-recs-in > 10000

070115*    perform 0055-update-table   thru 0055-exit
           perform 0060-finish-up      thru 0060-exit
           close file-in
           display ' End Program '
           display ' rows inserted ' rec-cnt
           display ' records read  ' ws-recs-in
           goback

           .
       0010-init.

111317     MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
111317     MOVE +0                     TO DC-ELAPSED-DAYS
111317     MOVE ' '                    TO DC-OPTION-CODE
111317     PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
111317     IF NO-CONVERSION-ERROR
111317        string
111317           dc-greg-date-a-edit (7:4) '.'
111317           dc-greg-date-a-edit (1:2) '.'
111317           dc-greg-date-a-edit (4:2)
111317              delimited by size into ws-test-date
111317        end-string
111317        display ' test date ' ws-test-date
111317     ELSE
111317        DISPLAY ' Current date  ERROR '
111317        PERFORM ABEND-PGM
111317     END-IF

063022     move 'PROD_Logic'           to svr
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022*    move 'TEST_Logic'           to svr

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE    ' '                 TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           move dc-greg-date-a-edit    to ws-moe-date

           .
       0010-exit.
           exit.

       0020-connect.

           display 'Begin connect to DB '
           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           display ' usr pass ' usr-pass

           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC

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

111317 0025-check-for-rerun.
111317
111317     exec sql
111317        SELECT
111317           MOE_DATE,
111317           Count(*)
111317        INTO
111317           :EXT-MOE-DATE,
111317           :WS-REC-CNTR
111317        FROM
111317           DCC_GAAP_DW
111317        GROUP BY MOE_DATE
111317        HAVING convert(varchar(10),MOE_DATE,102)
111317              = :ws-test-date
111317     end-exec
111317
111317     if sqlcode not = 0 and 1
111317        display "Error : check for rerun  "
111317        move sqlcode             to ws-sql-code
111317        move ws-sql-code         to ws-dis-sql-code
111317        display ' sqlcode ' ws-dis-sql-code
111317        display ' message ' sqlerrmc
111317     end-if
111317
111317     display ' counter ' ws-rec-cntr
111317
111317     .
111317 0025-exit.
111317     exit.

       0030-drop-table.

           display 'Begin Drop table'
           EXEC SQL
               drop table DCC_GAAP_DW
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

       0040-create-table.

           display ' Begin Create table'

           EXEC SQL
              create table DCC_GAAP_DW (
             	   moe_date datetime not null,
             	   rein_co char(3),
             	   rein_sub char(3),
             	   carrier char(1) not null,
             	   grouping char(6) not null,
             	   state char(2) not null,
             	   account char(10) not null,
             	   eff datetime not null,
             	   cert_no char(11) not null,
             	   lftyp char(2),
             	   lf_term int,
             	   lf_remterm int ,
             	   lfben decimal(11,2),
             	   lfprm decimal(9,2),
             	   ahtyp char(2),
             	   ah_term int,
             	   ah_remterm int,
             	   ahben decimal(9,2),
             	   ahprm decimal(9,2),
             	   rem_amt decimal(10,2),
             	   plfprm decimal(9,2),
             	   rlfprm decimal(9,2),
             	   slfprm decimal(9,2),
             	   pahprm decimal(9,2),
             	   rahprm decimal(9,2),
             	   sahprm decimal(9,2),
             	   rein char(1),
             	   ah_rem_ben decimal(11,2),
             	   mort_resv decimal(9,2),
             	   sex_code char(1),
             	   loan_term int,
             	   lf_exp_dt datetime,
             	   ah_exp_dt datetime,
             	   lf_up_remterm int,
             	   ah_up_remterm int,
             	   unld_stat_resv decimal(9,2),
             	   lfcom decimal(9,2),
             	   plfcom decimal(9,2),
             	   rlfcom decimal(9,2),
             	   slfcom decimal(9,2),
             	   ahcom decimal(9,2),
             	   pahcom decimal(9,2),
             	   rahcom decimal(9,2),
             	   sahcom decimal(9,2),
             	   mo_dec decimal(9,2),
             	   mort_fact decimal(9,4),
             	   mort_age int,
             	   mort_table char(4),
             	   alt_mort_table char(4)
                 CONSTRAINT PK_DCC_GAAP_DW PRIMARY KEY CLUSTERED
                   (moe_date, rein_co, rein_sub, carrier, grouping,
                   state, account, eff, cert_no)
             	   )
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
              move file-in-rec         to gaap-detail-record
              add 1                    to ws-recs-in
           end-if


           .
       0046-exit.
           exit.

       0050-insert-row.

070115     IF EX-LF-EXP-DT = LOW-VALUES OR SPACES
070115        MOVE -1                  TO NU-LF-EXP-DT
070115     ELSE
070115        MOVE +0                  TO NU-LF-EXP-DT
070115     END-IF
070115
070115     IF EX-AH-EXP-DT = LOW-VALUES OR SPACES
070115        MOVE -1                  TO NU-AH-EXP-DT
070115     ELSE
070115        MOVE +0                  TO NU-AH-EXP-DT
070115     END-IF

      *    display ' Begin Insert row '

           EXEC SQL
              insert into DCC_GAAP_DW (
             	   MOE_DATE,
             	   REIN_CO,
             	   REIN_SUB,
             	   CARRIER,
             	   GROUPING,
             	   STATE,
             	   ACCOUNT,
             	   EFF,
             	   CERT_NO,
             	   LFTYP,
             	   LF_TERM,
             	   LF_REMTERM,
             	   LFBEN,
             	   LFPRM,
             	   AHTYP,
             	   AH_TERM,
             	   AH_REMTERM,
             	   AHBEN,
             	   AHPRM,
             	   REM_AMT,
             	   PLFPRM,
             	   RLFPRM,
             	   SLFPRM,
             	   PAHPRM,
             	   RAHPRM,
             	   SAHPRM,
             	   REIN,
             	   AH_REM_BEN,
             	   MORT_RESV,
             	   SEX_CODE,
             	   LOAN_TERM,
             	   LF_EXP_DT,
             	   AH_EXP_DT,
             	   LF_UP_REMTERM,
             	   AH_UP_REMTERM,
             	   UNLD_STAT_RESV,
             	   LFCOM,
             	   PLFCOM,
             	   RLFCOM,
             	   SLFCOM,
             	   AHCOM,
             	   PAHCOM,
             	   RAHCOM,
             	   SAHCOM,
             	   MO_DEC,
             	   MORT_FACT,
             	   MORT_AGE,
             	   MORT_TABLE,
             	   ALT_MORT_TABLE)
	              values (
                   :WS-MOE-DATE,
                   :EX-REIN-CO,
                   :EX-REIN-SUB,
                   :EX-CARRIER,
                   :EX-GROUPING,
                   :EX-STATE,
                   :EX-ACCOUNT,
                   :EX-EFF,
                   :EX-CERT-NO,
                   :EX-LFTYP,
                   :EX-LF-TERM,
                   :EX-LF-REMTERM,
                   :EX-LFBEN,
                   :EX-LFPRM,
                   :EX-AHTYP,
                   :EX-AH-TERM,
                   :EX-AH-REMTERM,
                   :EX-AHBEN,
                   :EX-AHPRM,
                   :EX-REM-AMT,
                   :EX-PLFPRM,
                   :EX-RLFPRM,
                   :EX-SLFPRM,
                   :EX-PAHPRM,
                   :EX-RAHPRM,
                   :EX-SAHPRM,
                   :EX-REIN,
                   :EX-AH-REM-BEN,
                   :EX-MORT-RESV,
                   :EX-SEX-CODE,
                   :EX-LOAN-TERM,
070115             :EX-LF-EXP-DT         :NU-LF-EXP-DT,
070115             :EX-AH-EXP-DT         :NU-AH-EXP-DT,
                   :EX-LF-UP-REMTERM,
                   :EX-AH-UP-REMTERM,
                   :EX-UNLD-STAT-RESV,
                   :EX-LFCOM,
                   :EX-PLFCOM,
                   :EX-RLFCOM,
                   :EX-SLFCOM,
                   :EX-AHCOM,
                   :EX-PAHCOM,
                   :EX-RAHCOM,
                   :EX-SAHCOM,
                   :EX-MO-DEC,
                   :EX-MORT-FACT,
                   :EX-MORT-AGE,
                   :EX-MORT-TABLE,
                   :EX-ALT-MORT-TABLE)
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' recs so far ' rec-cnt
              display ' offending rec ' gaap-detail-record
              goback
           end-if

           add 1 to rec-cnt

           .
       0050-exit.
           exit.

       0055-update-table.

           move spaces                 to sqlcmd
           string 'update DCC_GAAP_DW '
                  'set lf_exp_dt = NULL '
                  'where (lf_exp_dt = '
                  'CONVERT(DATETIME, ''1900-01-01 00:00:00'', 102))'
              delimited by size into sqlcmd
           end-string
           display ' cmd **' sqlcmd '**'

           display ' Begin Exp Dt Updates '
           exec sql
              EXECUTE IMMEDIATE :sqlcmd
           end-exec

      *    exec sql
      *       UPDATE DCC_GAAP_DW
      *       SET lf_exp_dt = NULL
      *       WHERE (lf_exp_dt =
      *          CONVERT(DATETIME, '1900-01-01 00:00:00', 102))
      *    end-exec

           if sqlcode not = 0
              display "Error: update lf exp dt  "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              goback
           end-if

           display ' Lf Exp Dt Update success '

           move spaces                 to sqlcmd
           string 'update DCC_GAAP_DW '
                  'set ah_exp_dt = NULL '
                  'where (ah_exp_dt = '
                  'CONVERT(DATETIME, ''1900-01-01 00:00:00'', 102))'
              delimited by size into sqlcmd
           end-string
           display ' cmd **' sqlcmd '**'

           exec sql
              EXECUTE IMMEDIATE :sqlcmd
           end-exec

      *    exec sql
      *       UPDATE DCC_GAAP_DW
      *       SET ah_exp_dt = NULL
      *       WHERE (ah_exp_dt =
      *          CONVERT(DATETIME, '1900-01-01 00:00:00', 102))
      *    end-exec

           if sqlcode not = 0
              display "Error: update ah exp dt  "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              goback
           end-if

           display ' Ah Exp Dt Update success '

           .
       0055-exit.
           exit.

111317 1045-delete-rows.
111317
111317     exec sql delete
111317        FROM
111317           DCC_GAAP_DW
111317        where convert(varchar(10),MOE_DATE,102)
111317              = :ws-test-date
111317     end-exec
111317
111317     if sqlcode not = 0 and 1
111317        display "Error : deleting rows  "
111317        move sqlcode             to ws-sql-code
111317        move ws-sql-code         to ws-dis-sql-code
111317        display ' sqlcode ' ws-dis-sql-code
111317        display ' message ' sqlerrmc
111317        go to 1045-exit
111317     end-if
111317
111317     EXEC SQL
111317         commit
111317     END-EXEC
111317
111317     if sqlcode not = 0 and 1
111317        display "Error : commit of delete  "
111317        move sqlcode             to ws-sql-code
111317        move ws-sql-code         to ws-dis-sql-code
111317        display ' sqlcode ' ws-dis-sql-code
111317        display ' message ' sqlerrmc
111317     end-if
111317
111317     .
111317 1045-exit.
111317     exit.

       0060-finish-up.

           display ' Begin Commit '
           EXEC SQL
               commit transaction
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
               commit work release
           END-EXEC
           if sqlcode not = 0
              display "Error: commit release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0060-exit.
           exit.

       8500-DATE-CONVERT.              
                                       COPY ELCDCS.

       abend-pgm.

            call 'ABORTME'.
            
            goback.
