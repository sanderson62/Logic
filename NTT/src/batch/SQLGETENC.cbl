      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLGETENC.

      *AUTHOR.     Pablo
      *            Colleyville, TX.
      *DATE-COMPILED.

      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************

      *REMARKS. This program populates the Logic ELENCC file.
060117******************************************************************
060117*                   C H A N G E   L O G
060117*
060117* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
060117*-----------------------------------------------------------------
060117*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
060117* EFFECTIVE    NUMBER
060117*-----------------------------------------------------------------
060117* 060117    2017060100002  TANA  Increase size of attachments
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
060117******************************************************************


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELENCC       ASSIGN TO ELENCC
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELENCC-FILE-STATUS
                               RECORD KEY IS NC-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ELENCC.
                                       COPY ELCENCC.
       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       working-storage section.

       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
      
       77  ws-recs-out                 pic 9(7) value zeros.
       77  ws-recs-in                  pic 9(7) value zeros.
       77  eof-sw                      pic x value ' '.
           88  end-of-input               value 'Y'.
       77  ws-del-elencc               pic 9(5) value zeros.

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

       01  elencc-file-status          pic xx  value low-values.
       01  PGM-SUB                     PIC S999 COMP  VALUE +158.
       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.
       01 f.
          05  ws-comp-cd.
              10  filler                   pic x  value low-values.
              10  ws-company-cd            pic x.
          05  ws-company-cd-num redefines ws-comp-cd
                                       pic s9(4) comp.

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

       01  table-data.
           12  tb-flag                 PIC X.
           12  tb-group                PIC X.
           12  tb-ins-code             PIC X(10).
           12  tb-stack                PIC XXX.
           12  tb-encloser             PIC X(100).
           12  tb-attachments          PIC X(255).
           12  tb-id                   PIC XXX.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       LINKAGE SECTION.
       01  var  pic x(30).

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

       0000-begin.

      *    display ' Begin Program '
           perform 0010-init           thru 0010-exit
           perform 0020-connect        thru 0020-exit

      *    EXEC SQL
      *       SET AUTOCOMMIT OFF
      *    END-EXEC

           perform 0025-open-files     thru 0025-exit
           perform 0026-zap-elencc     thru 0026-exit
           perform 0030-open-cursor    thru 0030-exit
           perform 0040-process-input  thru 0040-exit until
              end-of-input
      *      or ws-recs-in > 10000

           perform 0060-finish-up      thru 0060-exit
           display ' End Program '
           display ' elencc recs deleted ' ws-del-elencc
           display ' records inserted    ' ws-recs-out
           display ' table records read  ' ws-recs-in
           goback

           .
       0010-init.

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


           .
       0010-exit.
           exit.

       0020-connect.

      *    display 'Begin connect to DB '

      ****  The below code is for when the db has been
      ****  converted to sql server 2016
           evaluate ws-kix-myenv
              when 'cid1p'
                 move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
      *       when 'mdoff'
      *          move '//hov-tstdb01.cso.local:55330;'
      *                                to p-sql-server
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
              perform abend-pgm
           end-if

           display " connect to DB successful "

           .
       0020-exit.
           exit.

       0025-open-files.

           open i-o ELENCC
           if elencc-file-status not = '00'
              display ' error-elencc-open ' elencc-file-status
              perform abend-pgm
           end-if

           .
       0025-exit.
           exit.

       0026-zap-elencc.

           move dte-clasic-company-cd  to ws-company-cd
      *    display ' processing company ' DTE-CLIENT ' '
      *       ws-company-cd-num
           move low-values             TO nc-CONTROL-PRIMARY
           move dte-clasic-company-cd  to nc-company-cd
           START ELencc KEY >= nc-CONTROL-PRIMARY
           IF ELencc-FILE-STATUS = '00'
              PERFORM UNTIL ELencc-FILE-STATUS NOT = '00'
                 READ ELencc NEXT RECORD
                 IF Elencc-FILE-STATUS = '00'
                    if nc-company-cd = dte-clasic-company-cd
      *                display ' about to delete '
      *                   nc-control-primary (2:6)
                       delete elencc
                       if elencc-file-status = '00'
                          add 1 to ws-del-elencc
                       else
                          display ' error-elencc-delete '
                          elencc-file-status
                          ' ' nc-control-primary (2:15)
                       end-if
                    end-if
                 end-if
              end-perform
           end-if

           .
       0026-exit.
           exit.

       0030-open-cursor.

      *    display ' declare cursor '

           evaluate true
              when dte-client = 'CID'
                 EXEC SQL
                    declare cidcodes cursor for
                       select * from NaperProdInsertionCodes_CPS
                 END-EXEC
              when dte-client = 'DCC'
                 EXEC SQL
                    declare dcccodes cursor for
                       select * from NaperProdInsertionCodes_DCC
                 END-EXEC
              when dte-client = 'AHL'
                 EXEC SQL
                    declare ahlcodes cursor for
                       select * from NaperProdInsertionCodes_AHL
                 END-EXEC
062121        when dte-client = 'FNL'
062121           EXEC SQL
062121              declare fnlcodes cursor for
062121                 select * from NaperProdInsertionCodes_FNL
062121           END-EXEC
           end-evaluate

           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform abend-pgm
           end-if

      *    display ' open cursor '

           evaluate true
              when dte-client = 'CID'
                 EXEC SQL
                     open cidcodes
                 END-EXEC
              when dte-client = 'DCC'
                 EXEC SQL
                     open dcccodes
                 END-EXEC
              when dte-client = 'AHL'
                 EXEC SQL
                     open ahlcodes
                 END-EXEC
062121        when dte-client = 'FNL'
062121           EXEC SQL
062121               open fnlcodes
062121           END-EXEC
           end-evaluate

           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform abend-pgm
           end-if

           .
       0030-exit.
           exit.

       0040-process-input.

           perform until sqlcode not = 0
            evaluate true
              when dte-client = 'CID'
               EXEC SQL
                 fetch cidcodes into :tb-flag,
                                     :tb-group,
                                     :tb-ins-code,
                                     :tb-stack,
                                     :tb-encloser,
                                     :tb-attachments,
                                     :tb-id
               END-EXEC
              when dte-client = 'DCC'
               EXEC SQL
                 fetch dcccodes into :tb-flag,
                                     :tb-group,
                                     :tb-ins-code,
                                     :tb-stack,
                                     :tb-encloser,
                                     :tb-attachments,
                                     :tb-id
               END-EXEC
              when dte-client = 'AHL'
               EXEC SQL
                 fetch ahlcodes into :tb-flag,
                                     :tb-group,
                                     :tb-ins-code,
                                     :tb-stack,
                                     :tb-encloser,
                                     :tb-attachments,
                                     :tb-id
               END-EXEC
062121        when dte-client = 'FNL'
062121         EXEC SQL
062121           fetch fnlcodes into :tb-flag,
062121                               :tb-group,
062121                               :tb-ins-code,
062121                               :tb-stack,
062121                               :tb-encloser,
062121                               :tb-attachments,
062121                               :tb-id
062121         END-EXEC
            end-evaluate
            if sqlcode not = 0 and 100
                 display "Error: cannot read row "
                 display ' sql retrun code ' sqlcode
                 display ' sql err mess    ' sqlerrmc
                 goback
            end-if
            if sqlcode = 0
               add 1                 to ws-recs-in
               perform 0045-create-rec
                                       thru 0045-exit
            end-if
           end-perform

           if sqlcode = 100
              display ' Normal end of record set '
              display ' number of records        ' ws-recs-in
           end-if

           set end-of-input            to true

           evaluate true
              when dte-client = 'CID'
                 EXEC SQL
                     close cidcodes
                 END-EXEC
              when dte-client = 'DCC'
                 EXEC SQL
                     close dcccodes
                 END-EXEC
              when dte-client = 'AHL'
                 EXEC SQL
                     close ahlcodes
                 END-EXEC
062121        when dte-client = 'FNL'
062121           EXEC SQL
062121               close fnlcodes
062121           END-EXEC
           end-evaluate

           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              goback
           end-if

           .
       0040-exit.
           exit.

       0045-create-rec.

      *    display tb-group ' ' tb-ins-code ' ' tb-stack
      *    go to 0045-exit
           MOVE 'NC'                   TO ENCLOSURE-CODES
           MOVE DTE-CLASIC-COMPANY-CD  TO NC-COMPANY-CD
           MOVE tb-group               TO NC-REC-TYPE
           MOVE FUNCTION UPPER-CASE(tb-ins-code)
                                       TO NC-ENC-CODE
           MOVE tb-stack               TO NC-OUTPUT-STACK
           MOVE tb-encloser            TO NC-ENCLOSURE-LINE
           MOVE tb-attachments         TO NC-ATTACHMENTS
                                          
           MOVE 'AUTO'                 TO NC-LAST-MAINT-USER
           MOVE +180000                TO NC-LAST-MAINT-HHMMSS
           MOVE bin-run-date           TO NC-LAST-MAINT-DT
          
           PERFORM 0050-WRITE-ELENCC   THRU 0050-EXIT

           .
       0045-EXIT.
           EXIT.

       0050-WRITE-ELENCC.

           WRITE ENCLOSURE-CODES

           IF ELENCC-FILE-STATUS  = '00'
              ADD 1                    TO WS-recs-OUT
           ELSE
              DISPLAY ' BAD WRITE FOR ELENCC ' ELENCC-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0050-EXIT.
           EXIT.

       0060-finish-up.

      *    display ' Begin Disconnect '
           EXEC SQL
NTTDel*        commit work release
NTTIns         commit work
           END-EXEC
           if sqlcode not = 0
              display "Error: commit release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

NTTIns     EXEC SQL
NTTIns        DISCONNECT ALL
NTTIns     END-EXEC

           close elencc
           if elencc-file-status not = '00'
              display ' error-elencc-close ' elencc-file-status
           end-if

           .
       0060-exit.
           exit.

       abend-pgm.

           call 'ABORTME'
           goback.
