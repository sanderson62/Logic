      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       identification division.
       program-id. WSZIPCD.
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
110921* 110921  CR2021051200001  PEMA  Onbase Workflow project
010722* 010722 CR2019012500003   PEMA  Convert to SQLSERVER 2016
      ******************************************************************
       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       data division.
       FILE SECTION.
       working-storage section.
       01  DFH-START PIC X(04).
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
       01  WS-PASS-AREa.
           03  PA-ZIP                  PIC X(5).
           03  PA-ErrorCode            PIC X(10).
           03  PA-city                 PIC x(50).
           03  PA-state                PIC XX.
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
       01  DFHCOMMAREA                 PIC X(587).
       01  var  pic x(30).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'WSZIPCD' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           display ' entering program WSZIPCD'
           move dfhcommarea            to ws-pass-AREA
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
           perform 0030-get-zipcd-data thru 0030-exit
           perform 0050-bld-pass-area  thru 0050-exit
           perform 0060-disconnect     thru 0060-exit
           .
       0000-return.
           move ws-pass-area           to dfhcommarea
           
      * exec cics return
      *    end-exec
      *    MOVE '.(                    ''   #00000261' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * GOBACK

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'WSZIPCD' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK
           .
       0010-init.
           move pa-zip                 to zc-zipcode
           move pa-city                to zc-city
           move pa-state               to zc-state
           .
       0010-exit.
           exit.
       0020-connect.
010722     move 'SDVDB01_Repos'        to svr
021521*    move 'HOVTSTDB01_Repos'     to svr
021521     move 'appuser'              to usr
021521     move 'appuser@cso'          to pass
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
       0030-get-zipcd-data.
           EXEC SQL
              SELECT
021521           County
              INTO
021521           :zc-County
              FROM
021521           ZipCodes
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
              go to 0030-exit
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
       0125-calc-field-len.
           move 0                      to ws-string-len
           inspect ws-fld-3 tallying ws-string-len for all ' '
      *    display ' string len b4 ' ws-string-len
           compute ws-string-len = 50 - ws-string-len
      *    display ' string len af ' ws-string-len
           .
       0125-exit.
           exit.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'WSZIPCD' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'WSZIPCD' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
