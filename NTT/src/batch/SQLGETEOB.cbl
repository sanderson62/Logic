      *$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLGETEOB.
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

      *REMARKS. This program populates the Logic ELEOBC file.
062121******************************************************************
062121*                   C H A N G E   L O G
062121*
062121* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062121*-----------------------------------------------------------------
062121*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062121* EFFECTIVE    NUMBER
062121*-----------------------------------------------------------------
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
062121******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELEOBC       ASSIGN TO ELEOBC
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELEOBC-FILE-STATUS
                               RECORD KEY IS EO-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ELEOBC.
                                       COPY ELCEOBC.
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
       77  ws-del-eleobc               pic 9(5) value zeros.

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

       01  ws-pass                     pic x  value spaces.
       01  eleobc-file-status          pic xx  value low-values.
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

       01  ws-rec-type                 pic x  value spaces.
       01  ws-eob-code                 pic x(4) value spaces.
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

       01  ws-lookupid-1               pic x(4).
       01  ws-lookupid-2               pic x(4).
       01  ws-lookupid-3               pic x(4).
       01  table-data.
           12  tb-lookupid             PIC 9(18).
           12  filler redefines tb-lookupid.
               16  f                   pic x(14).
               16  tb-lookupid-r       pic x(4).
           12  tb-lookupkey            PIC X(200).
           12  tb-lookupvalue          PIC X(275).

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
           perform 0026-zap-eleobc     thru 0026-exit
           perform 0030-begin-cursors  thru 0030-exit
      *    perform 0040-process-input  thru 0040-exit until
      *       end-of-input
      *      or ws-recs-in > 10000

           perform 0060-finish-up      thru 0060-exit
           display ' End Program '
           display ' eleobc recs deleted ' ws-del-eleobc
           display ' records inserted    ' ws-recs-out
           display ' table recs read     ' ws-recs-in
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

           evaluate dte-client
              when 'CID'
                 move '1005'           to ws-lookupid-1
                 move '1015'           to ws-lookupid-2
                 move '1017'           to ws-lookupid-3
              when 'DCC'
                 move '1007'           to ws-lookupid-1
                 move '1015'           to ws-lookupid-2
                 move '1018'           to ws-lookupid-3
              when 'AHL'
                 move '1009'           to ws-lookupid-1
                 move '1015'           to ws-lookupid-2
                 move '1012'           to ws-lookupid-3
062121        when 'FNL'
062121           move '1027'           to ws-lookupid-1 *>eob
062121           move '1015'           to ws-lookupid-2 *>Bill notes
062121           move '1028'           to ws-lookupid-3 *>Rea codes
           end-evaluate

062121*    move 'sdvdb01_NaperRepo'    to svr
062121*    move 'appuser'              to usr
062121*    move 'appuser@cso'          to pass

      *    move 'NTSQLTST2'            to svr
      *    move '  '                   to usr
      *    move '         '            to pass

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
      *       when 'mdoff'   *> UAT
      *          move '//hov-tstdb01.cso.local:55330;'
      *                                to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate
           move 'NapersoftRepository'  to p-sql-database

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

           open i-o ELEOBC
           if eleobc-file-status not = '00'
              display ' error-eleobc-open ' eleobc-file-status
              perform abend-pgm
           end-if

           .
       0025-exit.
           exit.

       0026-zap-eleobc.

           move dte-clasic-company-cd  to ws-company-cd
      *    display ' processing company ' dte-client ' '
      *       ws-company-cd-num
           move low-values             TO eo-CONTROL-PRIMARY
           move dte-clasic-company-cd  to eo-company-cd
           START ELeobc KEY >= eo-CONTROL-PRIMARY
           IF ELeobc-FILE-STATUS = '00'
              PERFORM UNTIL ELeobc-FILE-STATUS NOT = '00'
                 READ ELeobc NEXT RECORD
                 IF Eleobc-FILE-STATUS = '00'
                    if eo-company-cd = dte-clasic-company-cd
      *                display ' about to delete '
      *                   eo-control-primary (2:5)
                       delete eleobc
                       if eleobc-file-status = '00'
                          add 1 to ws-del-eleobc
                       else
                          display ' error-eleobc-delete '
                          eleobc-file-status
                          ' ' eo-control-primary (2:5)
                       end-if
                    end-if
                 end-if
              end-perform
           end-if

           .
       0026-exit.
           exit.

       0030-begin-cursors.

      *    display ' begin declare cursors '

           perform 0032-cursor-1       thru 0032-exit
      *    perform 0033-cursor-2       thru 0033-exit
      *    perform 0034-cursor-3       thru 0034-exit

           .
       0030-exit.
           exit.

       0032-cursor-1.

           EXEC SQL
               declare eobcodes cursor for
                   select * from LOOKUPVALUES where
                      lookupid = :ws-lookupid-1
                      or lookupid = :ws-lookupid-2
                      or lookupid = :ws-lookupid-3
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform abend-pgm
           end-if

           display ' open cursor '

           EXEC SQL
               open eobcodes
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform abend-pgm
           end-if

           move '1'                    to ws-pass
           perform 0040-process-input  thru 0040-exit

           EXEC SQL
               close eobcodes
           END-EXEC
           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform abend-pgm
           end-if

           .
       0032-exit.
           exit.

       0040-process-input.

           perform until sqlcode not = 0
              EXEC SQL
                 fetch eobcodes into :tb-lookupid,
                                     :tb-lookupkey,
                                     :tb-lookupvalue
              END-EXEC
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

           .
       0040-exit.
           exit.

       0045-create-rec.

           evaluate true
062121        when tb-lookupid-r = '1005' or '1007' or '1009' or '1027'
                 move '1'              to ws-rec-type
                 move tb-lookupkey     to ws-eob-code
              when (tb-lookupid-r = '1017' or '1018' or '1012' or
062121           '1028')
                 and (tb-lookupkey (1:1) = 'V')
                 move '2'              to ws-rec-type
                 move tb-lookupkey     to ws-eob-code
              when (tb-lookupid-r = '1017' or '1018' or '1012' or
062121           '1028')
                 and (tb-lookupkey (1:1) = 'P' OR 'C')
                 MOVE '4'              to ws-rec-type
                 move tb-lookupkey     to ws-eob-code
062121        when tb-lookupid-r = '1017' or '1018' or '1012' or '1028'
                 move '3'              to ws-rec-type
                 move tb-lookupkey (1:3) to ws-eob-code (2:3)
                 move tb-lookupkey (4:1) to ws-eob-code (1:1)
              when tb-lookupid-r = '1015'
                 move '5'              to ws-rec-type
                 move tb-lookupkey     to ws-eob-code
              when other
                 display ' found invalid option in 0045 '
                 perform abend-pgm
           end-evaluate

      *    display ' ***' tb-lookupid-r '*' ws-rec-type '*' ws-eob-code
      *       '*' tb-lookupvalue '*** '
      *    go to 0045-exit

           MOVE 'EO'                   TO EOB-CODES
           MOVE DTE-CLASIC-COMPANY-CD  TO EO-COMPANY-CD
           MOVE ws-rec-type            TO EO-RECORD-TYPE
           MOVE ws-eob-code            TO EO-EOB-CODE
           MOVE tb-lookupvalue         TO EO-DESCRIPTION
                                          
           MOVE 'AUTO'                 TO EO-LAST-MAINT-USER
           MOVE +180000                TO EO-LAST-MAINT-HHMMSS
           MOVE bin-run-date           TO EO-LAST-MAINT-DT
          
           PERFORM 0050-WRITE-ELEOBC   THRU 0050-EXIT

           .
       0045-EXIT.
           EXIT.

       0050-WRITE-ELEOBC.

           WRITE EOB-CODES

           IF ELEobC-FILE-STATUS  = '00'
              ADD 1                    TO WS-recs-OUT
           ELSE
              DISPLAY ' BAD WRITE FOR ELEOBC ' ELEOBC-FILE-STATUS
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

           close eleobc
           if eleobc-file-status not = '00'
              display ' error-eleobc-close ' eleobc-file-status
           end-if

           .
       0060-exit.
           exit.

       abend-pgm.

           call 'ABORTME'
           goback.
