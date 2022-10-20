      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SQLBPREMCOM.
       AUTHOR.        Cowtown.
       DATE-COMPILED.

      *REMARKS. This program builds the PREM_COMM
      * every month.

072018******************************************************************
072018*                   C H A N G E   L O G
072018*
072018* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
072018*-----------------------------------------------------------------
072018*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
072018* EFFECTIVE    NUMBER
072018*-----------------------------------------------------------------
072018* 072018  CR2018070500003  PEMA  New program
072018******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT COMM-TRAN-IN ASSIGN TO SYS010.

           select eracct           ASSIGN TO ERACCTT
              organization is indexed
              access is dynamic
              record key is am-control-primary
              file status is eracct-file-status.
           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  COMM-TRAN-IN 
                                       COPY ECSCOMFD.
       01  cp-record-in                pic x(270).

       FD  ERACCT.
                                       COPY ERCACCT.
       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.

       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  INPUT-CNT                   PIC 9(11)  VALUE ZEROS.
       77  OUTPUT-CNT                  PIC 9(11)  VALUE ZEROS.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  ws-records-inserted         pic 9(9) value zeros.
       77  ws-sql-date-time            pic x(24) value spaces.
       77  eracct-file-status          pic xx value low-values.

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
          BEGIN DECLARE SECTION
       END-EXEC

       01  sqlcmd                      pic x(1024).
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
      ***        insert into TABLE_NAME (                            ***
      ***           date1,                                           ***
      ***           date2)                                           ***
      ***        values (                                            ***
      ***           :db-date1      :nu-date1,                        ***
      ***           :db-date2      :nu-date2)                        ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-LF-EXP-DT            PIC s9(4) comp value +0.
           05  nu-LF-CAN-DT            PIC s9(4) comp value +0.
           05  nu-LF-CAN-EXT-DT        PIC s9(4) comp value +0.
           05  nu-LF-DTH-DT            PIC s9(4) comp value +0.
           05  nu-LF-DTH-EXT-DT        PIC s9(4) comp value +0.
           05  nu-AH-EXP-DT            PIC s9(4) comp value +0.
           05  nu-AH-CAN-DT            PIC s9(4) comp value +0.
           05  nu-AH-CAN-EXT-DT        PIC s9(4) comp value +0.
           05  nu-AH-DIS-DT            PIC s9(4) comp value +0.

       01  EXTRACT-RECORD.
           05  EXT-MOE-DATE            PIC X(10).
           05  EXT-CARRIER             PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-ACCOUNT             PIC X(10).
           05  ext-remit               pic x(10).
           05  EXT-STATE               PIC XX.
           05  EXT-EFF-DT              PIC X(10).
           05  EXT-CERT-NO             PIC X(11).
           05  ext-trans               pic x.
           05  ext-bus-type            pic xx.
           05  ext-comm-type           pic x.
           05  ext-last-name           pic x(15).
           05  ext-first-name          pic x(15).
           05  ext-init                pic x.
           05  ext-lf-type             pic xx.
           05  ext-lf-term             pic 999.
           05  ext-lf-amt-n            pic -9(9).99.
           05  ext-lf-amt redefines
               ext-lf-amt-n            pic x(13).
           05  ext-lf-prm-n            pic -9(7).99.
           05  ext-lf-prm redefines
               ext-lf-prm-n            pic x(11).
           05  ext-lf-com-n            pic -9(7).99.
           05  ext-lf-com redefines
               ext-lf-com-n            pic x(11).
           05  ext-lf-amt-alt-n        pic -9(9).99.
           05  ext-lf-amt-alt redefines
               ext-lf-amt-alt-n        pic x(13).
           05  ext-lf-prm-alt-n        pic -9(7).99.
           05  ext-lf-prm-alt redefines
               ext-lf-prm-alt-n        pic x(11).
           05  ext-lf-com-alt-n        pic -9(7).99.
           05  ext-lf-com-alt redefines
               ext-lf-com-alt-n        pic x(11).
           05  ext-ah-type             pic xx.
           05  ext-ah-term             pic 999.
           05  ext-ah-amt-n            pic -9(9).99.
           05  ext-ah-amt redefines
               ext-ah-amt-n            pic x(13).
           05  ext-ah-prm-n            pic -9(7).99.
           05  ext-ah-prm redefines
               ext-ah-prm-n            pic x(11).
           05  ext-ah-com-n            pic -9(7).99.
           05  ext-ah-com redefines
               ext-ah-com-n            pic x(11).

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

                                       copy ECSCOM01.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.

       01  DATE-AREAS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC X(4).
               10  WS-WORK-MM          PIC XX.
               10  WS-WORK-DD          PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       copy ELCFUNDT.
                                       COPY ELCDTECX.
                                       copy ELCDTEVR.
                                       COPY ELCDATE.
       LINKAGE SECTION.                                                 
       01  var                         pic x(30).

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

           display ' Begin Program SQLBPREMCOM '

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

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT
           PERFORM 0080-PROCESS-CERT   THRU 0080-EXIT UNTIL
                 (END-OF-INPUT)

           PERFORM 1050-FINISH-UP      THRU 1050-EXIT
           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           GOBACK
           .
       0002-EXIT.
           EXIT.

       0010-INITIALIZE.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 1000-CONNECT        THRU 1000-EXIT
      *    PERFORM 1010-drop-table     THRU 1010-EXIT
           perform 1020-truncate-table thru 1020-exit
      *    PERFORM 1030-create-table   THRU 1030-EXIT
           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT comm-tran-in eracct
           if eracct-file-status <> '00'
              display 'error-eracct-open ' eracct-file-status
              perform abend-pgm
           end-if

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' INPUT RECORDS  ' INPUT-CNT
           DISPLAY ' OUTPUT RECORDS ' OUTPUT-CNT
           CLOSE COMM-TRAN-IN eracct
           if eracct-file-status <> '00'
              display 'error-eracct-open ' eracct-file-status
              perform abend-pgm
           end-if

           .
       0030-EXIT.
           EXIT.

       0060-READ-INPUT.

           READ COMM-TRAN-IN into cp-record AT END
               SET END-OF-INPUT TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1 TO INPUT-CNT
           END-IF

           .
       0060-EXIT.
           EXIT.

       0080-PROCESS-CERT.

           PERFORM 0090-BUILD-EXTRACT  THRU 0090-EXIT
           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.

           if cp-trans = 'I' OR 'C' OR '8' OR '7'
              continue
           else
              go to 0090-exit
           end-if

           move dte-clasic-company-cd  to am-control-primary
           move cp-carrier             to am-carrier
           move cp-grouping            to am-grouping
           move cp-state               to am-state
           move cp-account             to am-account
           move cp-eff                 to am-expire-dt
           start eracct key > am-control-primary
           if eracct-file-status <> '00'
              display ' error-eracct-start ' eracct-file-status ' '
                 cp-state ' ' cp-account ' ' cp-eff
                 perform abend-pgm
              go to 0090-exit
           end-if
           read eracct next record
           if eracct-file-status <> '00' and '10' and '23'
              display ' error-eracct-read  ' eracct-file-status ' '
                 cp-state ' ' cp-account ' ' cp-eff
                 perform abend-pgm
              go to 0090-exit
           end-if
           if cp-carrier = am-carrier
              and cp-state = am-state
              and cp-account = am-account
              and cp-eff < am-expire-dt
              and cp-eff >= am-effect-dt
              continue
           else
              display ' error-eracct-notfnd  ' eracct-file-status ' '
                 cp-state ' ' cp-account ' ' cp-eff
              go to 0090-exit
           end-if

           if am-report-code-3 = 'LDS'
              continue
           else
              go to 0090-exit
           end-if
              
           MOVE SPACES                 TO EXTRACT-RECORD
           MOVE run-date               TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-moe-date
           END-STRING
           MOVE CP-CARRIER             TO EXT-CARRIER
           MOVE CP-GROUPING            TO EXT-GROUP
           MOVE CP-ACCOUNT             TO EXT-ACCOUNT
           MOVE CP-REMIT               TO EXT-REMIT
           MOVE CP-STATE               TO EXT-STATE
           MOVE CP-CERT                TO EXT-CERT-NO
           move cp-trans               to ext-trans

           MOVE CP-EFF                 TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING
              WS-MM '/'
              WS-DD '/'
              WS-CCYY DELIMITED BY SIZE
                                       INTO EXT-EFF-DT
           END-STRING

           move cp-gpcd                to ext-bus-type
           move cp-com-type            to ext-comm-type
           move cp-lname               to ext-last-name
           move cp-fname               to ext-first-name
           move cp-initial             to ext-init
           move cp-lf-type             to ext-lf-type
           move cp-lf-term             to ext-lf-term
           move cp-lf-amt              to ext-lf-amt-n
           move cp-lf-prm              to ext-lf-prm-n
           move cp-lf-com              to ext-lf-com-n
           move cp-lf-amt-alt          to ext-lf-amt-alt-n
           move cp-lf-prm-alt          to ext-lf-prm-alt-n
           move cp-lf-com-alt          to ext-lf-com-alt-n
           move cp-ah-type             to ext-ah-type
           move cp-ah-term             to ext-ah-term
           move cp-ah-amt              to ext-ah-amt-n
           move cp-ah-prm              to ext-ah-prm-n
           move cp-ah-com              to ext-ah-com-n
           
           PERFORM 0100-WRITE-EXTRACT  THRU 0100-EXIT

           .
       0090-EXIT.
           EXIT.

       0100-WRITE-EXTRACT.

           PERFORM 1040-INSERT-ROW     THRU 1040-EXIT

           .
       0100-EXIT.
           EXIT.

       1000-connect.

           display ' about to connect to Logic '

           move 'NTSQLTST2_PEMA'       to svr
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
              display "Error: cannot connect to PEMA DB"
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              perform abend-pgm
           end-if

           .
       1000-exit.
           exit.

       1010-drop-table.

           display 'Begin Drop table'
           EXEC SQL
              drop table LDS_BILL_TRANS
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

           display 'Begin Truncate table'
           EXEC SQL
               truncate table LDS_BILL_TRANS
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

       1030-create-table.

           display ' Begin Create table '

           EXEC SQL
              CREATE TABLE LDS_BILL_TRANS(
                 MOE_DATE           datetime NOT NULL,
                 CARRIER            char(1) NOT NULL,
                 GROUPING           char(6) NOT NULL,
                 ACCOUNT            char(10) NOT NULL,
                 REMIT              char(10) not null,
                 STATE              char(2) NOT NULL,
                 EFF_DATE           datetime NOT NULL,
                 CERT_NO            char(11) NOT NULL,
                 TRANS              char(1)  not null,
                 BUS_TYPE           char(2) NULL,
                 COMM_TYPE          char(1) null,
                 LAST_NAME          varchar(25) null,
                 FIRST_NAME         varchar(25) null,
                 MID_INIT           char(1) null,
                 LF_TYPE            char(2) null,
                 LF_TERM            int null,
                 LF_AMT             decimal(11,2) null,
                 LF_PREM            decimal(9,2) null,
                 LF_COMM            decimal(9,2) null,
                 LF_AMT_ALT         decimal(11,2) null,
                 LF_PREM_ALT        decimal(9,2) null,
                 LF_COMM_ALT        decimal(9,2) null,
                 AH_TYPE            char(2) null,
                 AH_TERM            int null,
                 AH_AMT             decimal(11,2) null,
                 AH_PREM            decimal(9,2) null,
                 AH_COMM            decimal(9,2) null 
                   )                                             
           END-EXEC                                              

           if sqlcode not = 0
              display "Error: cannot create table "
              move sqlcode             to ws-disp-code
              display ' sql return code ' ws-disp-code
              display ' sql err mess    ' sqlerrmc
              PERFORM 1050-FINISH-UP   THRU 1050-EXIT
              PERFORM ABEND-PGM
           end-if

           .
       1030-exit.
           exit.

       1040-INSERT-ROW.

           EXEC SQL
              insert into LDS_BILL_TRANS (
                 MOE_DATE      ,
                 CARRIER       ,
                 GROUPING      ,
                 ACCOUNT       ,
                 REMIT         ,
                 STATE         ,
                 EFF_DATE      ,
                 CERT_NO       ,
                 TRANS         ,
                 BUS_TYPE      ,
                 COMM_TYPE     ,
                 LAST_NAME     ,
                 FIRST_NAME    ,
                 MID_INIT      ,
                 LF_TYPE       ,
                 LF_TERM       ,
                 LF_AMT        ,
                 LF_PREM       ,
                 LF_COMM       ,
                 LF_AMT_ALT    ,
                 LF_PREM_ALT   ,
                 LF_COMM_ALT   ,
                 AH_TYPE       ,
                 AH_TERM       ,
                 AH_AMT        ,
                 AH_PREM       ,
                 AH_COMM)
	            values (
                 :EXT-MOE-DATE     ,
                 :EXT-CARRIER      ,
                 :EXT-GROUP        ,
                 :EXT-ACCOUNT      ,
                 :ext-remit        ,
                 :EXT-STATE        ,
                 :EXT-EFF-DT       ,
                 :EXT-CERT-NO      ,
                 :ext-trans        ,
                 :ext-bus-type     ,
                 :ext-comm-type    ,
                 :ext-last-name    ,
                 :ext-first-name   ,
                 :ext-init         ,
                 :ext-lf-type      ,
                 :ext-lf-term      ,
                 :ext-lf-amt       ,
                 :ext-lf-prm        ,
                 :ext-lf-com        ,
                 :ext-lf-amt-alt    ,
                 :ext-lf-prm-alt    ,
                 :ext-lf-com-alt    ,
                 :ext-ah-type       ,
                 :ext-ah-term       ,
                 :ext-ah-amt        ,
                 :ext-ah-prm        ,
                 :ext-ah-com)
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' in out cnt ' input-cnt ' ' output-cnt
              display ' offending rec ' extract-record
              PERFORM 1050-FINISH-UP   THRU 1050-EXIT
              PERFORM ABEND-PGM
           end-if

           add 1 to ws-records-inserted

           .
       1040-EXIT.
           EXIT.

       1050-finish-up.

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
       1050-exit.
           exit.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
