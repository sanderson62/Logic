      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. sqlbgaapd.
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
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  ws-records-inserted         pic 9(9) value zeros.
       77  ws-sql-date-time            pic x(24) value spaces.

021714 01  P pointer.
021714 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
021714 01  var-ptr pointer.
021714 01  env-var-len                 pic 9(4)  binary.
021714 01  rc                          pic 9(9)  binary.
021714
021714 01  WS-KIXSYS.
021714     05  WS-KIX-FIL1             PIC X(10).
021714     05  WS-KIX-APPS             PIC X(10).
021714     05  WS-KIX-ENV              PIC X(10).
021714     05  WS-KIX-MYENV            PIC X(10).
021714     05  WS-KIX-SYS              PIC X(10).

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  sqlcmd                      pic x(2048).
       01  WS-MOE-DATE                 pic x(10).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

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
           05  nu-lf-exp-dt-a redefines
               nu-lf-exp-dt            pic xx.
070115     05  NU-AH-EXP-DT            PIC s9(4) comp value +0.
           05  nu-ah-exp-dt-a redefines
               nu-ah-exp-dt            pic xx.

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

       01  ws-stuff-for-files-upload-data.
           05  ws-up-table-name        pic x(15).
           05  ws-up-file-name         pic x(15).
           05  ws-up-create-date       pic x(25).
           05  ws-up-modify-date       pic x(25).
           05  ws-up-full-path         pic x(25).
           05  ws-up-rec-count         pic 9(9).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  FILLER.
           05  ws-table-name.
               10  ws-table-comp-id    pic xxx  value spaces.
               10  filler              pic x(5) value '_GAAP'.
           05  ABEND-CODE                  PIC X(4) VALUE SPACES.
           05  ABEND-OPTION                PIC X    VALUE 'Y'.
           05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.
           05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.
           05  WS-RETURN-CODE       COMP-3 PIC S9(3)   VALUE ZERO.
           05  PGM-SUB                     PIC S999 COMP-3 VALUE +344. 

                                       COPY ELCDTECX.
                                       copy ELCDTEVR.
                                       copy ELCFUNDT.
                                       COPY ELCDATE.

       LINKAGE SECTION.                                                 

       01  var  pic x(30).

       procedure division.
                                       COPY ELCDTERX.
       0000-begin.

           display ' Begin Program SQLBBENCDES '

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0                   to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if

           display ' KIXSYS  ' ws-kix-myenv

           move FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           display ' function date = ' function-date

           string
              ws-fn-ccyr '-'
              ws-fn-mo   '-'
              ws-fn-da   '  '
              ws-fn-hours ':'
              ws-fn-minutes ':'
              ws-fn-seconds '.000'
                 delimited by size into ws-sql-date-time
           end-string

           display ' sql date time ' ws-sql-date-time

           perform 0010-init           thru 0010-exit
           perform 0020-connect        thru 0020-exit
      *    perform 0030-drop-table     thru 0030-exit
           perform 0035-truncate-table thru 0035-exit
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

           move dte-client             to ws-table-comp-id
           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE    ' '                 TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           move dc-greg-date-a-edit    to ws-moe-date

           .
       0010-exit.
           exit.

       0020-connect.

           display ' about to connect to Logic '

           if dte-client = 'AHL'
              move 'NTSQLTST2_Logic_TPA'
                                       to svr
              move 'sa'                to usr
              move 'sql2008r2'         to pass
           else
              move 'NTSQLTST2_Logic'   to svr
              move 'sa'                to usr
              move 'sql2008r2'         to pass
           end-if

           if ws-kix-myenv = 'cid1p'
              if dte-client = 'AHL'
                 move 'NTCSO2_Logic_TPA'
                                       to svr
                 move 'sa'             to usr
                 move 'ntcso2'         to pass
              else
                 move 'NTCSO2_Logic'   to svr
                 move 'sa'             to usr
                 move 'ntcso2'         to pass
              end-if
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
              display "Error: cannot connect to " svr
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              perform abend-pgm
           end-if

           display " connect to DB successful "

           .
       0020-exit.
           exit.

       0030-drop-table.

           move spaces                 to sqlcmd
           string
              'drop table '
              ws-table-name
              delimited by size into sqlcmd
           end-string

           display 'Begin Drop table' sqlcmd

            EXEC SQL
               EXECUTE IMMEDIATE :sqlcmd
            END-EXEC.

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

           move spaces                 to sqlcmd
           string
              'truncate table '
              ws-table-name
              delimited by size into sqlcmd
           end-string

           display 'Begin Truncate table' sqlcmd

            EXEC SQL
               EXECUTE IMMEDIATE :sqlcmd
            END-EXEC.

           if sqlcode not = 0
              display "Error : cannot truncate table "
              display sqlcode
              display sqlerrmc
              perform abend-pgm
           end-if

           display ' sql return code ' sqlcode
           display ' sql err mess    ' sqlerrmc

           .
       0035-exit.
           exit.

       0040-create-table.

           display ' Begin Create table'

           move spaces                 to sqlcmd

           string
              'create table '
              ws-table-name       ' ('
             	   'rein_co char(3),             '
             	   'rein_sub char(3),            '
             	   'carrier char(1) not null,    '
             	   'grouping char(6) not null,   '
             	   'state char(2) not null,      '
             	   'account char(10) not null,   '
             	   'eff datetime not null,       '
             	   'cert_no char(11) not null,   '
             	   'lftyp char(2),               '
             	   'lf_term int,                 '
             	   'lf_remterm int ,             '
             	   'lfben decimal(11,2),         '
             	   'lfprm decimal(9,2),          '
             	   'ahtyp char(2),               '
             	   'ah_term int,                 '
             	   'ah_remterm int,              '
             	   'ahben decimal(9,2),          '
             	   'ahprm decimal(9,2),          '
             	   'rem_amt decimal(10,2),       '
             	   'plfprm decimal(9,2),         '
             	   'rlfprm decimal(9,2),         '
             	   'slfprm decimal(9,2),         '
             	   'pahprm decimal(9,2),         '
             	   'rahprm decimal(9,2),         '
             	   'sahprm decimal(9,2),         '
             	   'rein char(1),                '
             	   'ah_rem_ben decimal(11,2),    '
             	   'mort_resv decimal(9,2),      '
             	   'sex_code char(1),            '
             	   'loan_term int,               '
             	   'lf_exp_dt datetime,          '
             	   'ah_exp_dt datetime,          '
             	   'lf_up_remterm int,           '
             	   'ah_up_remterm int,           '
             	   'unld_stat_resv decimal(9,2), '
             	   'lfcom decimal(9,2),          '
             	   'plfcom decimal(9,2),         '
             	   'rlfcom decimal(9,2),         '
             	   'slfcom decimal(9,2),         '
             	   'ahcom decimal(9,2),          '
             	   'pahcom decimal(9,2),         '
             	   'rahcom decimal(9,2),         '
             	   'sahcom decimal(9,2),         '
             	   'mo_dec decimal(9,2),         '
             	   'mort_fact decimal(9,4),      '
             	   'mort_age int,                '
             	   'mort_table char(4),          '
             	   'alt_mort_table char(4)       '
                 'CONSTRAINT PK_' ws-table-name
                 ' PRIMARY KEY CLUSTERED'
                 '(rein_co, rein_sub, carrier, grouping, '
                 'state, account, eff, cert_no)'
             	   ')'
                delimited by size into sqlcmd
           end-string

           display ' create sql ' sqlcmd

           EXEC SQL
               execute immediate :sqlcmd
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

           move spaces                 to sqlcmd
           string
              'INSERT INTO '
               ws-table-name
               ' ('
             	 'REIN_CO,        '
             	 'REIN_SUB,       '
             	 'CARRIER,        '
             	 'GROUPING,       '
             	 'STATE,          '
             	 'ACCOUNT,        '
             	 'EFF,            '
             	 'CERT_NO,        '
             	 'LFTYP,          '
             	 'LF_TERM,        '
             	 'LF_REMTERM,     '
             	 'LFBEN,          '
             	 'LFPRM,          '
             	 'AHTYP,          '
             	 'AH_TERM,        '
             	 'AH_REMTERM,     '
             	 'AHBEN,          '
             	 'AHPRM,          '
             	 'REM_AMT,        '
             	 'PLFPRM,         '
             	 'RLFPRM,         '
             	 'SLFPRM,         '
             	 'PAHPRM,         '
             	 'RAHPRM,         '
             	 'SAHPRM,         '
             	 'REIN,           '
             	 'AH_REM_BEN,     '
             	 'MORT_RESV,      '
             	 'SEX_CODE,       '
             	 'LOAN_TERM,      '
             	 'LF_EXP_DT,      '
             	 'AH_EXP_DT,      '
             	 'LF_UP_REMTERM,  '
             	 'AH_UP_REMTERM,  '
             	 'UNLD_STAT_RESV, '
             	 'LFCOM,          '
             	 'PLFCOM,         '
             	 'RLFCOM,         '
             	 'SLFCOM,         '
             	 'AHCOM,          '
             	 'PAHCOM,         '
             	 'RAHCOM,         '
             	 'SAHCOM,         '
             	 'MO_DEC,         '
             	 'MORT_FACT,      '
             	 'MORT_AGE,       '
             	 'MORT_TABLE,     '
             	 'ALT_MORT_TABLE'
             	 ')'
	             ' values ('
                "'" EX-REIN-CO       "', '"
                   EX-REIN-SUB       "', '"
                   EX-CARRIER        "', '"
                   EX-GROUPING       "', '"
                   EX-STATE          "', '"
                   EX-ACCOUNT        "', '"
                   EX-EFF            "', '"
                   EX-CERT-NO        "', '"
                   EX-LFTYP          "', '"
                   EX-LF-TERM        "', '"
                   EX-LF-REMTERM     "', '"
                   EX-LFBEN          "', '"
                   EX-LFPRM          "', '"
                   EX-AHTYP          "', '"
                   EX-AH-TERM        "', '"
                   EX-AH-REMTERM     "', '"
                   EX-AHBEN          "', '"
                   EX-AHPRM          "', '"
                   EX-REM-AMT        "', '"
                   EX-PLFPRM         "', '"
                   EX-RLFPRM         "', '"
                   EX-SLFPRM         "', '"
                   EX-PAHPRM         "', '"
                   EX-RAHPRM         "', '"
                   EX-SAHPRM         "', '"
                   EX-REIN           "', '"
                   EX-AH-REM-BEN     "', '"
                   EX-MORT-RESV      "', '"
                   EX-SEX-CODE       "', '"
                   EX-LOAN-TERM      "', '"
070115             EX-LF-EXP-DT      "', '"
      *            nu-lf-exp-dt-a    "', '"
070115             EX-AH-EXP-DT      "', '"
      *            NU-AH-EXP-DT-a    "', '"
                   EX-LF-UP-REMTERM  "', '"
                   EX-AH-UP-REMTERM  "', '"
                   EX-UNLD-STAT-RESV "', '"
                   EX-LFCOM          "', '"
                   EX-PLFCOM         "', '"
                   EX-RLFCOM         "', '"
                   EX-SLFCOM         "', '"
                   EX-AHCOM          "', '"
                   EX-PAHCOM         "', '"
                   EX-RAHCOM         "', '"
                   EX-SAHCOM         "', '"
                   EX-MO-DEC         "', '"
                   EX-MORT-FACT      "', '"
                   EX-MORT-AGE       "', '"
                   EX-MORT-TABLE     "', '"
                   EX-ALT-MORT-TABLE "')"
              delimited by size into sqlcmd
           end-string

          display ' insert sql command = ' sqlcmd

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' recs so far ' rec-cnt
              display ' offending rec ' gaap-detail-record
              goback
           end-if

           add 1 to ws-records-inserted
           add 1 to rec-cnt

           .
       0050-exit.
           exit.

       0060-finish-up.

          move ws-table-name           to ws-up-table-name
          string
             dte-client (1:2) '.XX.GAAP_00'
             delimited by size into ws-up-file-name
          end-string
          move '/data/seqfiles'        to ws-up-full-path
          move ws-records-inserted     to ws-up-rec-count
          move ws-sql-date-time        to ws-up-create-date
                                          ws-up-modify-date

          display ' ws table name ' ws-up-table-name
          display ' file name     ' ws-up-file-name
          display ' create date   ' ws-up-create-date
          display ' modify date   ' ws-up-modify-date
          display ' full path     ' ws-up-full-path
          display ' record count  ' ws-up-rec-count

          EXEC SQL
             CALL logic_Insert_FilesUploadedData
                      @table_name          = :ws-up-table-name,
                      @file_name           = :ws-up-file-name,
                      @file_created_date   = :ws-up-create-date,
                      @file_modified_date  = :ws-up-modify-date,
                      @file_full_path      = :ws-up-full-path,
                      @loaded_record_count = :ws-up-rec-count
          END-EXEC

           IF SQLCODE NOT = 0
              DISPLAY "ERROR: DID NOT update upload info "
              DISPLAY ' SQL RETURN CODE ' SQLCODE
              DISPLAY ' SQL ERR MESS    ' SQLERRMC
           END-IF

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
