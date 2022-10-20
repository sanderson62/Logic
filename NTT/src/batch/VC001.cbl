      *$SET SQL(dbman=JDBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       identification division.
       program-id. VC001.
       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       data division.
       FILE SECTION.

       working-storage section.
       77  s1 pic s999 comp-3 value +0.
       77  BYTE-OFFSET PIC S9(8) COMP VALUE +0.
       77  ws-eof-sw                   pic x  value spaces.
           88  end-of-input                  value 'Y'.
       77  ws-error-sw                 pic x  value spaces.
           88  error-found               value 'Y'.
       77  ws-string-len               pic s999 comp-3 value zeros.
      
       01  P pointer.
       01  KIXSYS.
           05  FILLER PIC X(6) VALUE 'KIXSYS'.
           05  FILLER PIC X  VALUE X'00'.
      *01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
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
          INCLUDE SQLCA
       END-EXEC

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  sqlcmd                      pic x(1024).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is passed nulls from sql. The indicator will be -1        ***
      ***  if the value on sql is nulls and +0 if the value is       ***
      ***  something other than nulls. Here is an example on how     ***
      ***  to use the indicator variables.                           ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        fetch checkapp into                                 ***
      ***           :db-app-status :nu-app-status,                   ***
      ***           :db-app-by     :nu-app-by,                       ***
      ***           :db-app-date   :nu-app-date,                     ***
      ***           :db-app-batch  :nu-app-batch                     ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-state                pic s9(4) comp value +0.
           05  nu-city                 pic s9(4) comp value +0.
           05  nu-county               pic s9(4) comp value +0.

       01  host-variables.
           05  hv-UniqueId             pic 999.
           05  hv-fname                pic x(30).
           05  hv-lname                pic x(30).
           05  hv-addr1                pic x(30).
           05  hv-addr2                pic x(30).
           05  hv-addr3                pic x(30).
           05  hv-city                 pic x(30).
           05  hv-state                pic xx.
           05  hv-zip                  pic x(10).

       01  name-addr-record.
           05  vc-UniqueId             pic 999.
           05  vc-fname                pic x(15).
           05  vc-lname                pic x(30).
           05  vc-addr1                pic x(30).
           05  vc-addr2                pic x(30).
           05  vc-addr3                pic x(30).
           05  vc-city                 pic x(30).
           05  vc-state                pic xx.
           05  vc-zip                  pic x(10).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  ws-misc.
           12  ws-file-in              pic x(26) value spaces.
           12  ws-connect-sw               pic x  value ' '.
               88  connected-to-db             value 'Y'.
           12  ws-file-in-status       pic xx  value spaces.

       LINKAGE SECTION.
       
       01  DFHCOMMAREA                 PIC X(587).
      
       01  var  pic x(30).
      
       procedure division.
      
           display ' entering program VC001'

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

           display ' environment is ' ws-kix-MYENV

           perform 0010-init           thru 0010-exit
           perform 0020-connect        thru 0020-exit
           perform 0030-exec-select    thru 0030-exit
      *    perform 0040-exec-insert    thru 0040-exit
      *    perform 0050-exec-delete    thru 0050-exit
      *    perform 0055-exec-update    thru 0055-exit
      *    perform 0060-exec-query-sp  thru 0060-exit
      *    perform 0070-exec-insert-sp thru 0070-exit
      *    perform 0080-exec-declare   thru 0080-exit
      *    perform 0090-exec-fetch     thru 0090-exit
           perform 1000-disconnect     thru 1000-exit

           .
       0000-return.


           GOBACK

           .
       0010-init.

           move 'Kathi'                to hv-fname

           .
       0010-exit.
           exit.

       0020-connect.

      *     SET ENVIRONMENT "jdbc.driver" TO
      *        "net.sourceforge.jtds.jdbc.Driver"
      *     SET ENVIRONMENT "jdbc.url" TO
      *        "jdbc:jtds:sqlserver://hov-tstdb01.cso.local/VCobolTest;us
      *-"er=appuser;password=appuser@cso".

           if hv-fname = 'paul'
              SET ENVIRONMENT "jdbc.url" to
            "jdbc:sqlserver://ntsqltst2.cso.local:1433;databaseName="
           else
              SET ENVIRONMENT "jdbc.url" to
            "jdbc:sqlserver://hov-tstdb01.cso.local:1433;databaseName="
           end-if

      *     SET ENVIRONMENT "jdbc.url" TO
      *        "jdbc:sqlserver://ntsqltst2.cso.local:1433;databaseName=V
      *"CobolTest;user=appuser;password=appuser@cso".

      *     exec sql
      *       connect
      *     end-exec

            move 'VCobolTest' to svr
            move 'appuser'     to usr
            move 'appuser@cso' to pass
            exec sql
              connect to :svr
               user      :usr
               using     :pass
            end-exec

      *     EXEC SQL
      *        CONNECT TO 'VCobolTest'
      *         USER 'appuser'
      *         USING 'appuser@cso'
      *     END-EXEC
 
           if sqlcode not = 0
              display "Error: cannot connect "
              display sqlcode
              display sqlerrmc
              perform abend-pgm
           else
              display ' Good connect '
           end-if

           set connected-to-db to true

           .
       0020-exit.
           exit.
      
       0030-exec-select.

           display ' Values of Host-Variables '
           display ' fname   ' hv-fname

           EXEC SQL
              SELECT
                 Addr1, Addr2
              INTO 
                 :vc-addr1, :vc-addr2
              FROM
                 NameAddr
              WHERE
                 FName = :hv-fname
           END-EXEC

           display ' Address ' vc-addr1 ' ' vc-addr2

           if sqlcode not = 0
              display "Error: cannot read row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' first name      ' hv-fname
              go to 0030-exit
           end-if

           .
       0030-exit.
           exit.

       0040-exec-insert.

           move 'Jane' to vc-fname
           move 'Johnson'  to vc-lname
           move '432 Easy St' to vc-addr1
           move 'Omaha' to vc-city
           move 'NE' to vc-state
           move '68111' to vc-zip

           display ' Values of Host-Variables '
           display ' addr1   ' vc-addr1

           EXEC SQL
              INSERT INTO NameAddr
                 (FName,
                  LName,
                  Addr1,
                  Addr2,
                  Addr3,
                  City,
                  State,
                  Zip) VALUES
                  (:vc-fname, 
                   :vc-lname, 
                   :vc-addr1, 
                   :vc-addr2, 
                   :vc-addr3, 
                   :vc-city, 
                   :vc-state, 
                   :vc-zip)
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' first name      ' vc-fname
           else
              display ' Good Insert '
           end-if

           EXEC SQL commit work END-EXEC

           if sqlcode not = 0
              display ' Error: commit '
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' first name      ' vc-fname
           else
              display ' Good Commit '
           end-if

           .
       0040-exit.
           exit.

       0050-exec-delete.

           move 6 to hv-uniqueid

           display ' Values of Host-Variables '
           display ' unique id   ' hv-uniqueid

           EXEC SQL
              delete NameAddr
              where Unique_Id = :hv-uniqueid
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot delete row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' UniqueId        ' hv-uniqueid
           else
              display ' Good Delete '
           end-if

           .
       0050-exit.
           exit.

       0055-exec-update.

           move 'McGrath'  to hv-lname

           move 'STE 100' to vc-addr2
           move '5th Floor' to vc-addr3

           display ' Values of Host-Variables '
           display ' addr2   ' vc-addr2
           display ' addr3   ' vc-addr3

           EXEC SQL
              UPDATE NameAddr
                set Addr2 = :vc-addr2,
                 Addr3 = :vc-addr3
              where
                LName = :hv-lname
           end-exec

           if sqlcode not = 0
              display "Error: cannot update row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' last name       ' hv-lname
           else
              display ' Good Update '
           end-if

           EXEC SQL commit work END-EXEC

           if sqlcode not = 0
              display ' Error: commit '
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' first name      ' vc-fname
           else
              display ' Good Commit '
           end-if


           .
       0055-exit.
           exit.

       0060-exec-query-sp.

           move 'Paul' to hv-fname

           display ' Values of Host-Variables '
           display ' Fname   ' hv-fname

           EXEC SQL
              CALL uspi_NameAddr_Get
                (@fname = :hv-fname,
                @city = :vc-city)
           END-EXEC

           display ' returned city ' vc-city

           if sqlcode not = 0
              display "Error: cannot retrieve row SP "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' first name      ' vc-fname
           else
              display ' good SP Query Call '
           end-if

           .
       0060-exit.
           exit.

       0070-exec-insert-sp.

           move 'John' to vc-fname
           move 'Johnson'  to vc-lname
           move '123 JJ St' to vc-addr1
           move 'Omaha' to vc-city
           move 'NE' to vc-state
           move '68987' to vc-zip

           display ' Values of Host-Variables '
           display ' addr1   ' vc-addr1

           EXEC SQL
              CALL uspi_NameAddr_Add
                  (:vc-fname, 
                   :vc-lname, 
                   :vc-addr1, 
                   :vc-addr2, 
                   :vc-addr3, 
                   :vc-city, 
                   :vc-state, 
                   :vc-zip)
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row SP "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' first name      ' vc-fname
              go to 0070-exit
           else
              display ' Good SP Insert '
           end-if

           EXEC SQL commit work END-EXEC

           if sqlcode not = 0
              display "Error: cannot commit "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' first name      ' vc-fname
           else
              display ' Good Commit '
           end-if

           .
       0070-exit.
           exit.

       0080-exec-declare.

           move 'John' to hv-FName
           display ' declare cursor ' hv-fname

           EXEC SQL
              DECLARE
                 curs1 cursor for
              SELECT
                 FName,
                 LName,
                 Addr1,
                 Addr2,
                 Addr3,
                 City,
                 State,
                 Zip
              FROM
                 NameAddr
              WHERE
                 FName = :hv-FName
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 0080-exit
           else
              display ' Good Declare Cursor '
           end-if

           EXEC SQL
              open curs1
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           else
              display ' Good Open Cursor '
           end-if

           .
       0080-exit.
           exit.

       0090-exec-fetch.

           display ' about to fetch '
           perform until sqlcode not = 0
              exec sql
                 fetch curs1 into
                   :vc-fname,
                   :vc-lname,
                   :vc-addr1,
                   :vc-addr2,
                   :vc-addr3,
                   :vc-city,
                   :vc-state,
                   :vc-zip
              end-exec
              if sqlcode not = 0 and 100
                 display "Error: cannot fetch row "
                 display ' sql retrun code ' sqlcode
                 display ' sql err mess    ' sqlerrmc
                 go to 0090-exit
              else
                 display ' Good fetch '
              end-if
              display ' name ' vc-fname ' ' vc-lname
              display ' addr ' vc-addr1 ' ' vc-addr2 ' ' vc-addr3
              display ' csz  ' vc-city ' ' vc-state ' ' vc-zip
           end-perform

           if sqlcode = 100
              display ' Normal end of record set '
           end-if

           exec sql close curs1 end-exec

           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           else
              display ' Good close cursor '
           end-if

           .
       0090-exit.
           exit.

       1000-disconnect.

           EXEC SQL
              DISCONNECT ALL
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot disconnect zipcodes "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           else
              display ' Good Disconnect '
           end-if

           .
       1000-exit.
           exit.
       abend-pgm.

           CALL 'ABORTME'
           goback
           .
       abend-exit.
            exit.


      
