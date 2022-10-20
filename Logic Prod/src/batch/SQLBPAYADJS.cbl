      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SQLBPAYADJS.
       AUTHOR.        Cowtown.
       DATE-COMPILED.

      *REMARKS. This program appends to PAYADJS_DW
      * every month.

090618******************************************************************
090618*                   C H A N G E   L O G
090618*
090618* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
090618*-----------------------------------------------------------------
090618*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
090618* EFFECTIVE    NUMBER
090618*-----------------------------------------------------------------
090618* 072018  CR2018070500003  PEMA  New program
090618******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PAYADJS-IN           ASSIGN TO SYS010.

           SELECT DISK-DATE            ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  PAYADJS-IN.

       01  payadjs-rec-in              pic x(80).

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
           05  nu-account              PIC s9(4) comp value +0.

       01  EXTRACT-RECORD.
           05  EXT-MOE-DATE            PIC X(10).
           05  EXT-CARRIER             PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-RESP-NO             pic x(10).
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-REC-TYPE            PIC X.
           05  EXT-DESC                PIC X(50).
           05  EXT-PMT-TYPE            PIC X.
           05  EXT-AMOUNT-N            pic -9(7).99.
           05  EXT-AMOUNT redefines
               EXT-AMOUNT-N            pic x(11).
           05  EXT-charge-N            pic -9(7).99.
           05  EXT-charge redefines
               EXT-charge-N            pic x(11).
           05  EXT-BILL-FLAG           pic x.

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

       01  PAYADJS-RECORD.
           12  A-ID                PIC  XXX.
           12  A-SEQ.
               16  A-CARR-GROUP.
                   20  A-CARRIER   PIC  X.
                   20  A-GROUPING  PIC  X(6).
               16  A-REMIT         PIC  X(10).
               16  A-ACCOUNT       PIC  X(10).
           12  A-CO-TYPE           PIC  X.
           12  A-MAINT-DATE        PIC 9(6).
           12  FILLER              PIC  X.
           12  A-DESC              PIC  X(30).
           12  A-TYPE              PIC  X.
               88  A-VALID-TYPE       VALUE 'R' 'D' 'C' 'S' 'T'
                                            'U' 'X' 'Y' 'Z'
                                            'F'.
               88  A-VALID-DMD-TYPE   VALUE 'R' 'D' 'C' 'S' 'T'
                                            'U' 'X' 'Y' 'Z'
                                            'F' 'A' 'B'.
           12  A-AMT               PIC S9(7)V99.
           12  BW-AMT  REDEFINES
               A-AMT               PIC  X(9).
           12  FILLER              PIC  X.
           12  A-BILL-FLAG         PIC  X.
               88  A-BILLED                        VALUE 'B'.

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
      *          OR (CERT-IN-CNT > 100000)

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

           OPEN INPUT PAYADJS-IN

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' INPUT RECORDS  ' INPUT-CNT
           DISPLAY ' OUTPUT RECORDS ' OUTPUT-CNT
           CLOSE PAYADJS-IN

           .
       0030-EXIT.
           EXIT.

       0060-READ-INPUT.

           READ PAYADJS-IN into PAYADJS-RECORD AT END
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

           if a-id = 'CLA'
              continue
           else
              go to 0090-exit
           end-if

           MOVE SPACES                 TO extract-record
           move zeros                  to ext-amount-n
                                          ext-charge-n
           MOVE run-date               TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING
              WS-MM '/'
              WS-DD '/'
              WS-CCYY DELIMITED BY SIZE
              INTO EXT-moe-date
           END-STRING
           MOVE a-CARRIER              TO EXT-CARRIER
           MOVE a-GROUPING             TO EXT-GROUP
           if a-account = low-values
              move spaces              to a-account
           end-if

           MOVE a-ACCOUNT              TO EXT-ACCOUNT
           MOVE a-REMIT                TO EXT-RESP-NO
           if a-co-type = ' ' *> must be a check
              if a-account = spaces or low-values *> must be G
                 move 'G'              to a-co-type
              else
                 move 'A'              to a-co-type
              end-if
           end-if
           move a-co-type              to ext-rec-type *> A OR G
           move A-DESC                 to ext-desc
           move a-type                 to ext-pmt-type
           if a-type = 'R'
              move a-amt               to ext-amount-n
           else
              move a-amt               to ext-charge-n
           end-if
      *    move a-amt                  to ext-amount-n
           move a-bill-flag            to ext-bill-flag

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
              drop table PAYADJS_DW
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
               truncate table PAYADJS_DW
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

       1040-INSERT-ROW.

           perform 1045-check-dates    thru 1045-exit

           EXEC SQL
              insert into PAYADJS_DW (
                 MOE_DATE      ,
                 Carrier       ,
                 Grouping      ,
                 FinResp       ,
                 Account       ,
                 RecType       ,
                 Description   ,
                 PmtType       ,
                 PmtAmt        ,
                 ChgAmt        ,
                 BillFlag)
	            values (
                 :EXT-MOE-DATE     ,
                 :EXT-CARRIER      ,
                 :EXT-GROUP        ,
                 :EXT-RESP-NO      ,
                 :ext-ACCOUNT      ,
                 :EXT-REC-TYPE     ,
                 :EXT-DESC         ,
                 :EXT-PMT-TYPE     ,
                 :ext-AMOUNT       ,
                 :ext-charge       ,
                 :ext-BILL-FLAG)
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

       1045-check-dates.

           IF EXT-ACCOUNT = LOW-VALUES OR SPACES
              MOVE -1 TO NU-ACCOUNT
           ELSE
              MOVE +0 TO NU-ACCOUNT
           END-IF

           .
       1045-EXIT.
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
