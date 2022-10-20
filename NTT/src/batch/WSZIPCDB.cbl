      *$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       identification division.
       program-id. WSZIPCDB.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 101017  CR2016091600001  PEMA  New program to verify zipcode.
021521* 021521  CR2020121600001  PEMA  Switch to different table
010722* 010722 CR2019012500003   PEMA  Convert to SQLSERVER 2016
      ******************************************************************
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
      
       01  ws-xml-stuff.
           05  ws-fld-1                pic x(20) value spaces.
           05  ws-fld-2                pic x(20) value spaces.
           05  ws-fld-3                pic x(50) value spaces.
           05  ws-error-cd redefines
               ws-fld-3                pic 9.
           05  ws-len-of-5 redefines
               ws-fld-3                pic 9(5).
           05  ws-model-year redefines
               ws-fld-3                pic 9999.
           05  ws-base-price redefines
               ws-fld-3                pic 9(11).
           05  ws-fld-4                pic x(20) value spaces.
           05  ws-fld-5                pic x(20) value spaces.
      
NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
      
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

       01  zip-codes.
           05  zc-zipcode              pic x(5).
           05  zc-state                pic xx.
           05  zc-city                 pic x(30).
021521     05  zc-county               pic x(40).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  ws-misc.
           12  ws-file-in              pic x(26) value spaces.
           12  ws-connect-sw               pic x  value ' '.
               88  connected-to-db             value 'Y'.
           12  ws-file-in-status       pic xx  value spaces.
           12  ws-curl-return-cd       pic s9(8) comp-5 value +0.
           12  ws-curl-string.
               16  f                   pic x(16) value
                'curl -o /tmp/zip'.
               16  filename-zip        pic x(5)  value spaces.
               16  f                   pic xxxx value '.txt'.
               16  f                   pic x(15) value
                ' --data "USZip='.
               16  curl-zip            pic x(5) value zeros.
               16  f                   pic x(48) value
                '" http://webservicex.net/uszip.asmx/GetInfoByZIP'.
               16  f                   pic x value low-values.

       01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
           88  RESP-NORMAL                    VALUE +0.
           88  resp-file-notfnd               value +12.
           88  RESP-NOTFND                    VALUE +13.
           88  resp-duprec                    value +14.
           88  resp-dupkey                    value +15.
           88  resp-invreq                    value +16.
           88  RESP-NOTOPEN                   VALUE +19.
           88  RESP-ENDFILE                   VALUE +20.
           88  resp-lengtherr                 value +22.
      
       01  f.
           05  ws-outputzip            pic x(5).
           05  ws-city                 pic x(50).
           05  ws-state                pic xx.

       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       LINKAGE SECTION.
       
       01  var                         pic x(30).
       01  ws-pass-area.
           03  PA-ZIP                  PIC X(5).
           03  PA-ErrorCode            PIC X(10).
           03  PA-city                 PIC x(50).
           03  PA-state                PIC XX.
      
       procedure division using ws-pass-area.
      
           display ' entering program WSZIPCDB'
      
           if ws-pass-area (1:7) = 'CONNECT'
              perform 0020-connect     thru 0020-exit
              go to 0000-return
           end-if

           if ws-pass-area (1:10) = 'DISCONNECT'
              perform 0060-disconnect  thru 0060-exit
              go to 0000-return
           end-if

           perform 0010-init           thru 0010-exit
           perform 0030-get-zipcd-data thru 0030-exit
           perform 0050-bld-pass-area  thru 0050-exit

           .
       0000-return.

           GOBACK

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

           move pa-zip                 to zc-zipcode
           move pa-city                to zc-city
           move pa-state               to zc-state

           .
       0010-exit.
           exit.

       0020-connect.

           display ' about to connect '

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


           move 'repository'           to p-sql-database

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

           set connected-to-db to true

           .
       0020-exit.
           exit.
      
       0030-get-zipcd-data.

           EXEC SQL
              SELECT
021521           County
              INTO 
021521           :zc-county
              FROM
021521           ZIPCodes
              WHERE
                 ZipCode = :zc-zipcode
                 and State = :zc-state
                 and City = :zc-city
           END-EXEC

           if sqlcode not = 0
              move 'NOTFOUND'          to pa-errorcode
              display "Error: cannot read row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' zip code        ' zc-zipcode
              display ' city            ' zc-city
              display ' state           ' zc-state
           end-if

           .
       0030-exit.
           exit.

       0050-bld-pass-area.

           move function upper-case(zc-city)
                                       to pa-city
           move zc-state               to pa-state

           .           
       0050-exit.
           exit.

       0060-disconnect.

           display ' about to disconnect '

           EXEC SQL
              DISCONNECT ALL
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot disconnect zipcodes "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0060-exit.
           exit.
