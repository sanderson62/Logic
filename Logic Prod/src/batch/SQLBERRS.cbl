      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SQLBERRS.
       AUTHOR.        Cowtown.
       DATE-COMPILED.

      *REMARKS.

030117******************************************************************
030117*                   C H A N G E   L O G
030117*
030117* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030117*-----------------------------------------------------------------
030117*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030117* EFFECTIVE    NUMBER
030117*-----------------------------------------------------------------
030117* 030117 CR2017022800002   PEMA  New Program
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
030117******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERRS                 ASSIGN TO SYS010.

           SELECT DISK-DATE            ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERRS.
                                       COPY ELCERRS.

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
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  WS-RECS-IN                  PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.
       77  WS-CANC-DATE                PIC 9(8)  VALUE ZEROS.
       77  S1                          PIC S999  VALUE +0 COMP-3.
       77  S2                          PIC S999  VALUE +0 COMP-3.
       77  rec-cnt                     pic 9(9) value zeros.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  eracctt-file-status         PIC XX    VALUE LOW-VALUES.
       77  ws-tally-cntr               pic s999  comp-3 value +0.
       77  ws-error-num                pic 9(4) value zeros.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
       01  ws-work-text                pic x(65) value spaces.
      
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  sqlcmd                      pic x(3500).
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
           05  nu-rein-comp            pic s9(4) comp value +0.
           05  nu-rein-sub             pic s9(4) comp value +0.
           05  nu-hi-cov-dt            pic s9(4) comp value +0.
           05  nu-hi-cov-dt-a redefines
               nu-hi-cov-dt            pic xx.
           05  nu-hi-cert-dt           pic s9(4) comp value +0.
           05  nu-hi-cert-dt-a redefines
               nu-hi-cert-dt           pic xx.
           05  nu-lo-cert-dt           pic s9(4) comp value +0.
           05  nu-lo-cert-dt-a redefines
               nu-lo-cert-dt           pic xx.

       01  EXTRACT-RECORD.
           05  ext-error-no            pic 9(4).
           05  EXT-error-text          PIC X(90).
           05  EXT-error-severity      PIC X.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

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

       01  filler.
           05  ws-work-phone-no        pic 9(11).
           05  filler redefines ws-work-phone-no.
               10  f                   pic x.
               10  ws-work-npa         pic xxx.
               10  ws-work-co          pic xxx.
               10  ws-work-num         pic xxxx.


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

                                       COPY ELCDTECX.
                                       copy ELCDTEVR.
                                       COPY ELCDATE.
       LINKAGE SECTION.                                                 
       01  var                         pic x(30).

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

           display ' Begin Program SQLBERRS '

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

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT
           PERFORM 0080-PROCESS-errs   THRU 0080-EXIT UNTIL
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
           perform 1020-truncate-table thru 1020-exit
      *    PERFORM 1010-drop-table     THRU 1010-EXIT
      *    PERFORM 1030-create-table   THRU 1030-EXIT
           PERFORM 0060-READ-errs      THRU 0060-EXIT

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN
              INPUT
                 ERRS

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' errs IN RECORDS  ' WS-RECS-IN
           CLOSE
              errs

           .
       0030-EXIT.
           EXIT.

       0060-READ-errs.

           READ errs AT END
               SET END-OF-INPUT TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1 TO WS-RECS-IN
           END-IF

           .
       0060-EXIT.
           EXIT.

       0080-PROCESS-ERRS.

           PERFORM 0090-BUILD-EXTRACT  THRU 0090-EXIT
           add 1 to ws-error-num

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.

           MOVE SPACES                 TO EXTRACT-RECORD

           if em-message-number = ws-error-num
              move em-message-number   TO EXT-error-no
              move em-error-severity   to ext-error-severity
              
              move em-error-text       to ws-work-text
              move zeros               to ws-tally-cntr
              INSPECT ws-work-text TALLYING WS-tally-cntr
                 FOR ALL "'"
              IF WS-tally-CNTR > +0
                 display ' found bad text ' em-error-text
                 move +1               to s2
                 perform varying s1 from +1 by +1 until s1 > +65
                    if ws-work-text (s1:1) = "'"
                       move ws-work-text (s1:1)
                                       to ext-error-text (s2:1)
                       add +1 to s2
                       move ws-work-text (s1:1)
                                       to ext-error-text (s2:1)
                       add +1 to s2
                    else
                       move ws-work-text (s1:1)
                                       to ext-error-text (s2:1)
                       add +1 to s2
                    end-if
                 end-perform
                 display ' Text after ' ext-error-text
              else
                 move em-error-text    to ext-error-text
              END-IF
              PERFORM 1040-INSERT-ROW  THRU 1040-EXIT
           else
              move ws-error-num        to ext-error-no
              move spaces              to ext-error-text
                                          ext-error-severity
              PERFORM 1040-INSERT-ROW  THRU 1040-EXIT
              go to 0090-exit
           end-if

           PERFORM 0060-READ-errs      THRU 0060-EXIT

           .
       0090-EXIT.
           EXIT.

       1000-connect.

           display ' about to connect to Logic '

063022     move 'TEST_Logic'           to svr
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022     if ws-kix-myenv = 'cid1p'
063022        move 'PROD_Logic'        to svr
063022     end-if

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
       1000-exit.
           exit.

       1010-drop-table.

           move spaces                 to sqlcmd
           string
              'drop table '
              'ELERRS'
              delimited by size into sqlcmd
           end-string

           display 'Begin Drop table' sqlcmd
      *    EXEC SQL
      *       drop table DCC_ELERRS
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
       1010-exit.
           exit.

       1020-truncate-table.

           display 'Begin Truncate table'
           EXEC SQL
               truncate table ELERRS
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
           move spaces                 to sqlcmd

           string
              'create table '
              'ELERRS ('
                 'ERROR_NO         int not null, '        
                 'ERROR_TEXT       varchar(90) not null, '        
                 'ERROR_SEVERITY   char(1) '        
                 'constraint PK_ELERRS '            
                 'PRIMARY KEY CLUSTERED'                           
                 '(ERROR_NO)'
                 ')'                                               
                delimited by size into sqlcmd                      
           end-string                                              
                                                                   
           display ' my sql stmt ' sqlcmd                          

           EXEC SQL
              EXECUTE IMMEDIATE :sqlcmd
           END-EXEC.

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

           move spaces                 to sqlcmd

           string
              'insert into '
              'ELERRS ('
              'ERROR_NO, '
              'ERROR_TEXT, '
              'ERROR_SEVERITY) '
	            'values ('
              "'"  EXT-error-no "',"
              "'"  EXT-error-text "',"
              "'"  EXT-error-severity"')"
                delimited by size into sqlcmd
           end-string

      *    display ' my insert sql cmd ' sqlcmd

            EXEC SQL
               EXECUTE IMMEDIATE :sqlcmd
            END-EXEC.

           if sqlcode not = 0
              display "Error: cannot insert row "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' in out cnt ' ws-recs-in ' ' rec-cnt
              display ' offending rec ' extract-record
              PERFORM 1050-FINISH-UP   THRU 1050-EXIT
              PERFORM ABEND-PGM
           end-if

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
