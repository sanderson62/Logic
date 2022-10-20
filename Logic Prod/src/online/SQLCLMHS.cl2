      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       identification division.
       program-id. SQLCLMHS.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 020218  CR2017062000002  PEMA  New program to verify CLM HIST.
031221* 031221  CR2019012500003  PEMA  Change connection to sdv-db01
110921* 110921  CR2021051200001  PEMA  Onbase Workflow project
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
      
      
       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
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

       01  clm-hist-stuff.
           05  ch-state                pic xx.
           05  ch-account              pic x(10).
           05  ch-eff-dt               pic x(10).
           05  ch-cert-no              pic x(11).
           05  ch-clm-count            pic 9(5).

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

       01  WS-PASS-AREa.
           03  pa-state                pic xx.
           03  pa-account              pic x(10).
           03  pa-eff-dt               pic x(10).
           03  pa-cert-no              pic x(11).
           03  pa-clm-count            pic 9(5).
      
       LINKAGE SECTION.
       
       01  DFHCOMMAREA                 PIC X(587).
      
       01  var  pic x(30).
      
       procedure division.
      
           display ' entering program SQLCLMHS'
      
           move dfhcommarea            to ws-pass-AREA
      
           display ' pa state        ' pa-state
           display ' pa acct         ' pa-account
           display ' pa eff dt       ' pa-eff-dt
           display ' pa cert no      ' pa-cert-no
           display ' pa-clm-count    ' pa-clm-count

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
      
           perform 0010-init           thru 0010-exit
           perform 0020-connect        thru 0020-exit
           perform 0030-get-clmhs-data thru 0030-exit
           perform 0050-bld-pass-area  thru 0050-exit
           perform 0060-disconnect     thru 0060-exit

           .
       0000-return.

           move ws-pass-area           to dfhcommarea

           exec cics return
           end-exec

           GOBACK

           .
       0010-init.

           move pa-state               to ch-state
           move pa-account             to ch-account
           move pa-eff-dt              to ch-eff-dt
           move pa-cert-no             to ch-cert-no

           .
       0010-exit.
           exit.

       0020-connect.

           move 'SDVDB01_ClmVer'       to svr
           move 'appuser'              to usr
           move 'appuser@cso'          to pass
      
           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
       
           if sqlcode not = 0
              display "Error: cannot connect "
              display sqlcode
              display sqlerrmc
           end-if

           set connected-to-db to true

           .
       0020-exit.
           exit.
      
       0030-get-clmhs-data.

           EXEC SQL
              CALL spch_CntFindClaims_online
                 @PendState       = :ch-state,
                 @PendAcct        = :ch-account,
                 @PendEffDt       = :ch-eff-dt,
                 @PendCertNo      = :ch-cert-no,
                 @NumClms         = :ch-clm-count out
           END-EXEC

           move sqlcode                to ws-disp-code
           display ' sql ret code ' ws-disp-code ' ' ch-clm-count

           if sqlcode not = 0 and 1 and 100
      *       move 'NOTFOUND'          to pa-errorcode
              display "Error: cannot read row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              display ' cert no         ' ch-cert-no
              go to 0030-exit
           end-if

           .
       0030-exit.
           exit.

       0050-bld-pass-area.

           move ch-clm-count           to pa-clm-count

           .           
       0050-exit.
           exit.

       0060-disconnect.

           EXEC SQL
110921        DISCONNECT
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot disconnect zipcodes "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0060-exit.
           exit.
