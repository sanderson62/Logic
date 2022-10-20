      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      *****************************************************************
      *                                                               *
      * Copyright (c) 2016 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLBBENCDS.
       AUTHOR.   Cowtown.

       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELCNTL           ASSIGN TO ELCNTL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CF-CONTROL-PRIMARY
                                   FILE STATUS IS ELCNTL-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ELCNTL.

                                       COPY ELCCNTL.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '  SQLBBENCDS WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ELCNTL                 VALUE 'Y'.
       77  ELCNTL-RECS-IN              PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S999  VALUE +0 COMP-3.
       77  S2                          PIC S999  VALUE +0 COMP-3.
       77  C1                          PIC S999  VALUE +0 COMP-3.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  PGM-SUB                     PIC S9(5) COMP-3 VALUE +515.


       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
      
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

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

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
      ***        insert into ERMEBL (                                ***
      ***           date1,                                           ***
      ***           date2)                                           ***
      ***        values (                                            ***
      ***           :db-date1      :nu-date1,                        ***
      ***           :db-date2      :nu-date2)                        ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  NU-spec-calc            PIC s9(4) comp value +0.
           05  NU-jnt-ind              PIC s9(4) comp value +0.

       01  ben-code-table.
           05  bc-lf-ah                pic x.
           05  bc-ben-code             pic xx.
           05  bc-alpha                pic xxx.
           05  bc-desc                 pic x(10).
           05  bc-comment              pic x(10).
           05  bc-cov-type             pic x.
           05  bc-spec-calc            pic x.
           05  bc-jnt-ind              pic x.
           05  bc-max-bens             pic 999.
           05  bc-category             pic x.
           05  bc-loan-type            pic x(10).
           05  bc-rem-term             pic x.
           05  bc-earn-calc            pic x.
           05  bc-ref-method           pic x.
           05  bc-ovrd-earn            pic x.
           05  bc-ind-grp              pic x.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

      ******************************************************************
       01  WS-MISC.
           05  ws-table-name.
               10  ws-table-comp-id    pic xxx  value spaces.
               10  filler              pic x(8) value '_BENCDES'.
           05  WS-CNTR                 PIC 9(4) VALUE ZEROS.
           05  ELCNTL-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       LINKAGE SECTION.                                                 

       01  var  pic x(30).

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

       0000-here-we-go.

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

           perform 0100-open-files     thru 0100-exit

           PERFORM 0110-INITIALIZE     THRU 0110-EXIT

           PERFORM 0500-PROCESS-ELCNTL THRU 0500-EXIT UNTIL
              (END-OF-ELCNTL)
PEMTST*       OR (CLM-RECS-IN > 1000)

           perform 0600-finish-up thru 0600-exit
           PERFORM 5000-CLOSE-FILES    THRU 5000-EXIT

           GOBACK

           .
       0100-OPEN-FILES.

           OPEN INPUT ELCNTL

           if elcntl-file-status not = '00' and '97'
              display ' Error-ELCNTL-Open ' elcntl-file-status
              perform abend-pgm
           end-if

           .
       0100-EXIT.
           EXIT.

       0110-INITIALIZE.

           move dte-client             to ws-table-comp-id

           perform 0120-connect-to-db  thru 0120-exit
           perform 0130-truncate-table thru 0130-exit
      *    perform 0132-drop-table     thru 0132-exit
      *    perform 0135-create-table   thru 0135-exit

           PERFORM 0140-START-ELCNTL   THRU 0140-EXIT
           PERFORM 0150-READ-ELCNTL    THRU 0150-EXIT

           .
       0110-EXIT.
           EXIT.

       0120-connect-to-db.

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

           .
       0120-exit.
           exit.

       0130-truncate-table.

           display ' Begin Truncate ' ws-table-name ' table '
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
              display "Error : cannot truncate table - " ws-table-name
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       0130-exit.
           exit.


       0132-drop-table.

           move spaces                 to sqlcmd
           string
              'drop table '
              ws-table-name
              delimited by size into sqlcmd
           end-string

           display 'Begin Drop table' sqlcmd
      *    EXEC SQL
      *       drop table VPP_EPEC_DW
      *    END-EXEC

            EXEC SQL
               EXECUTE IMMEDIATE :sqlcmd
            END-EXEC.

           if sqlcode not = 0
              display "Error(anticipated) : cannot drop table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       0132-exit.
           exit.

       0135-create-table.

           move spaces                 to sqlcmd

           string
              'create table '
              ws-table-name       ' ('
                 'LF_AH_TYP       char(1)  NOT NULL,     '
                 'BEN_CODE        char(2)  not null,     '
                 'ALPHA           varchar(3)   null,     '
                 'DESCRIPTION     varchar(10)  null,     '
                 'COMMENT         varchar(10)  null,     '
                 'COV_TYPE        char(1)      null,     '
                 'SPEC_CALC_CD    char(1)      null,     '
                 'IND_JNT         char(1)      null,     '
                 'MAX_BENS        int          null,     '
                 'CATEGORY        char(1)      null,     '
                 'LOAN_TYPE       varchar(10)  null,     '
                 'REM_TERM        char(1)      null,     '
                 'EARN_CALC       char(1)      null,     '
                 'REF_METHOD      char(1)      null,     '
                 'OVRD_EARN       char(1)      null,     '
                 'IND_GRP         char(1)      null,     '
                 'constraint PK_' ws-table-name
                 ' PRIMARY KEY CLUSTERED'
                 '(LF_AH_TYP, BEN_CODE)'
                 ')'
                delimited by size into sqlcmd
           end-string

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC

           if sqlcode not = 0
              display "Error : cannot Create table - " ws-table-name
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       0135-exit.
           exit.

       0140-START-ELCNTL.

           MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '4'                    TO CF-RECORD-TYPE  *> lf bencds
           START ELCNTL KEY >= CF-CONTROL-PRIMARY

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCNTL        TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL START     ' ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              END-IF
           END-IF

           .
       0140-EXIT.
           EXIT.

       0150-READ-ELCNTL.

           READ ELCNTL NEXT RECORD

           IF (ELCNTL-FILE-STATUS = '10' OR '23')
              or (cf-company-id <> dte-client)
              or (cf-record-type <> '4' and '5')
              SET END-OF-ELCNTL        TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL READ NEXT ' ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELCNTL
              ADD 1 TO ELCNTL-RECS-IN
           END-IF

           .
       0150-EXIT.
           EXIT.


       0500-PROCESS-ELCNTL.

           if cf-record-type = '4'
              perform varying s1 from +1 by +1 until s1 > +8
                 perform 0510-lf-ben   thru 0510-exit
              end-perform
           else
              perform varying s1 from +1 by +1 until s1 > +8
                 perform 0520-ah-ben   thru 0520-exit
              end-perform
           end-if

           PERFORM 0150-READ-ELCNTL    THRU 0150-EXIT

           .
       0500-EXIT.
           EXIT.

       0510-lf-ben.

           if cf-benefit-code (s1) = '00' or '  '
              go to 0510-exit
           end-if

           move 'L'                    to bc-lf-ah
           move cf-benefit-code (s1)   to bc-ben-code
           move cf-benefit-alpha (s1)  to bc-alpha
           move cf-benefit-descrip (s1)
                                       to bc-desc
           move cf-benefit-comment (s1)
                                       to bc-comment
           move cf-lf-coverage-type (s1)
                                       to bc-cov-type
           move cf-special-calc-cd (s1)
                                       to bc-spec-calc
           move cf-joint-indicator (s1)
                                       to bc-jnt-ind
           if cf-maximum-benefits (s1) not numeric
              move zeros               to cf-maximum-benefits (s1)
           end-if
           move cf-maximum-benefits (s1)
                                       to bc-max-bens
           move cf-benefit-category (s1)
                                       to bc-category
           move cf-loan-type (s1)      to bc-loan-type
           move cf-co-rem-term-calc (s1)
                                       to bc-rem-term
           move cf-co-earnings-calc (s1)
                                       to bc-earn-calc
           move cf-co-refund-calc (s1) to bc-ref-method
           move cf-co-ovrd-earnings-calc (s1)
                                       to bc-ovrd-earn
           move cf-co-ben-i-g-cd (s1)  to bc-ind-grp
           
           perform 0530-insert-row     thru 0530-exit

           .
       0510-exit.
           exit.

       0520-ah-ben.

           if cf-benefit-code (s1) = '00' or '  '
              go to 0520-exit
           end-if

           move 'A'                    to bc-lf-ah
           move cf-benefit-code (s1)   to bc-ben-code
           move cf-benefit-alpha (s1)  to bc-alpha
           move cf-benefit-descrip (s1)
                                       to bc-desc
           move cf-benefit-comment (s1)
                                       to bc-comment
           move spaces                 to bc-cov-type
           move cf-special-calc-cd (s1)
                                       to bc-spec-calc
           move cf-joint-indicator (s1)
                                       to bc-jnt-ind
           if cf-maximum-benefits (s1) not numeric
              move zeros               to cf-maximum-benefits (s1)
           end-if
           move cf-maximum-benefits (s1)
                                       to bc-max-bens
           move cf-benefit-category (s1)
                                       to bc-category
           move cf-loan-type (s1)      to bc-loan-type
           move cf-co-rem-term-calc (s1)
                                       to bc-rem-term
           move cf-co-earnings-calc (s1)
                                       to bc-earn-calc
           move cf-co-refund-calc (s1) to bc-ref-method
           move cf-co-ovrd-earnings-calc (s1)
                                       to bc-ovrd-earn
           move cf-co-ben-i-g-cd (s1)  to bc-ind-grp
           
           perform 0530-insert-row     thru 0530-exit

           .
       0520-exit.
           exit.

       0530-insert-row.

           move spaces                 to sqlcmd
           string
              'INSERT INTO '
               ws-table-name
               ' (LF_AH_TYP, BEN_CODE, ALPHA, DESCRIPTION, COMMENT,'
               ' COV_TYPE, SPEC_CALC_CD, IND_JNT, MAX_BENS, CATEGORY,'
               ' LOAN_TYPE, REM_TERM, EARN_CALC, REF_METHOD,'
               ' OVRD_EARN, IND_GRP)'
              ' VALUES ('
                "'" bc-lf-ah "', '"
                    bc-ben-code "', '"
                    bc-alpha     "', '"
                    bc-desc      "', '"
                    bc-comment   "', '"
                    bc-cov-type  "', '"
                    bc-spec-calc "', '"
                    bc-jnt-ind   "', '"
                    bc-max-bens  "', '"
                    bc-category  "', '"
                    bc-loan-type "', '"
                    bc-rem-term  "', '"
                    bc-earn-calc "', '"
                    bc-ref-method"', '"
                    bc-ovrd-earn "', '"
                    bc-ind-grp   "')"
              delimited by size into sqlcmd
           end-string

      *    display ' sql command = ' sqlcmd

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row " ws-table-name
                 ' ' bc-lf-ah
              display ' sql command = ' sqlcmd
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' offending rec ' ben-code-table
           end-if

           .
       0530-exit.
           exit.


       0600-finish-up.

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
       0600-exit.
           exit.

       5000-CLOSE-FILES.

           CLOSE ELCNTL

           .
       5000-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
