      *((program: WSZIPCD.cl2))
000001*$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
000002 identification division.
000003 program-id. WSZIPCD.
000004******************************************************************
000005*                   C H A N G E   L O G
000006*
000007* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000008*-----------------------------------------------------------------
000009*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000010* EFFECTIVE    NUMBER
000011*-----------------------------------------------------------------
000012* 101017  CR2016091600001  PEMA  New program to verify zipcode.
000013* 021521  CR2020121600001  PEMA  Switch to different table
000014* 110921  CR2021051200001  PEMA  Onbase Workflow project
000015* 010722 CR2019012500003   PEMA  Convert to SQLSERVER 2016
000016******************************************************************
000017 environment division.
000018 INPUT-OUTPUT SECTION.
000019 FILE-CONTROL.
000020
000021 data division.
000022 FILE SECTION.
000023
000024 working-storage section.
       01  DFH-START PIC X(04).
000025 77  s1 pic s999 comp-3 value +0.
000026 77  BYTE-OFFSET PIC S9(8) COMP VALUE +0.
000027 77  ws-eof-sw                   pic x  value spaces.
000028     88  end-of-input                  value 'Y'.
000029 77  ws-error-sw                 pic x  value spaces.
000030     88  error-found               value 'Y'.
000031 77  ws-string-len               pic s999 comp-3 value zeros.
000032
000033 01  P pointer.
000034 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000035 01  var-ptr pointer.
000036 01  env-var-len                 pic 9(4)  binary.
000037 01  rc                          pic 9(9)  binary.
000038
000039 01  WS-KIXSYS.
000040     05  WS-KIX-FIL1             PIC X(10).
000041     05  WS-KIX-APPS             PIC X(10).
000042     05  WS-KIX-ENV              PIC X(10).
000043     05  WS-KIX-MYENV            PIC X(10).
000044     05  WS-KIX-SYS              PIC X(10).
000045
000046 01  ws-xml-stuff.
000047     05  ws-fld-1                pic x(20) value spaces.
000048     05  ws-fld-2                pic x(20) value spaces.
000049     05  ws-fld-3                pic x(50) value spaces.
000050     05  ws-error-cd redefines
000051         ws-fld-3                pic 9.
000052     05  ws-len-of-5 redefines
000053         ws-fld-3                pic 9(5).
000054     05  ws-model-year redefines
000055         ws-fld-3                pic 9999.
000056     05  ws-base-price redefines
000057         ws-fld-3                pic 9(11).
000058     05  ws-fld-4                pic x(20) value spaces.
000059     05  ws-fld-5                pic x(20) value spaces.
000060
000061*EXEC SQL
000062*   INCLUDE SQLDA
000063*END-EXEC
000064
000067*EXEC SQL
      *   INCLUDE SQLCA
      *END-EXEC
      *>>((file: SQLCA))
000001****************************************************************<*
000002* Copyright (c) 2016-2021 NTT DATA, Inc. All rights reserved.  *<*
000003* Users of NTT DATA Enterprise COBOL may freely                *<*
000004* redistribute this copybook.                                  *<*
000005****************************************************************<*
000006
000007 01  SQLCA GLOBAL.
000008     05  SQLCAID                PIC X(8).
000009     05  SQLCABC                PIC S9(9) COMP-5.
000010     05  SQLCODE                PIC S9(9) COMP-5.
000011     05  SQLERRM.
000012         49  SQLERRML           PIC S9(4) COMP-5.
000013         49  SQLERRMC           PIC X(254).
000014     05  SQLERRP                PIC X(8).
000015     05  SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5.
000016     05  SQLWARN.
000017         10 SQLWARN0            PIC X(1).
000018         10 SQLWARN1            PIC X(1).
000019         10 SQLWARN2            PIC X(1).
000020         10 SQLWARN3            PIC X(1).
000021         10 SQLWARN4            PIC X(1).
000022         10 SQLWARN5            PIC X(1).
000023         10 SQLWARN6            PIC X(1).
000024         10 SQLWARN7            PIC X(1).
000025     05  SQLSTATE               PIC X(5).
000026     05  SQLEXT                 PIC S9(5) COMP-3 VALUE 1.
      *<<((file: SQLCA))
000068
000070 EXEC SQL
          BEGIN DECLARE SECTION
000071 END-EXEC
000072
000073 01  sqlcmd                      pic x(1024).
000074 01  svr                         pic x(32).
000075 01  usr                         pic x(32).
000076 01  pass                        pic x(32).
000077 01  usr-pass                    pic x(64).
000078 01  ws-disp-code                pic s9(11).
000079
000080***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000081***                                                            ***
000082***  These indicators are used to determine if a variable      ***
000083***  is passed nulls from sql. The indicator will be -1        ***
000084***  if the value on sql is nulls and +0 if the value is       ***
000085***  something other than nulls. Here is an example on how     ***
000086***  to use the indicator variables.                           ***
000087***                                                            ***
000088***     EXEC SQL                                               ***
000089***        fetch checkapp into                                 ***
000090***           :db-app-status :nu-app-status,                   ***
000091***           :db-app-by     :nu-app-by,                       ***
000092***           :db-app-date   :nu-app-date,                     ***
000093***           :db-app-batch  :nu-app-batch                     ***
000094***     END-EXEC                                               ***
000095***                                                            ***
000096***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000097
000098 01  indicator-vaiables-for-nulls.
000099     05  nu-state                pic s9(4) comp value +0.
000100     05  nu-city                 pic s9(4) comp value +0.
000101     05  nu-county               pic s9(4) comp value +0.
000102
000103 01  zip-codes.
000104     05  zc-zipcode              pic x(5).
000105     05  zc-state                pic xx.
000106     05  zc-city                 pic x(30).
000107     05  zc-county               pic x(40).
000108
000110 EXEC SQL
          END DECLARE SECTION
000111 END-EXEC
000112
000113 01  ws-misc.
000114     12  ws-file-in              pic x(26) value spaces.
000115     12  ws-connect-sw               pic x  value ' '.
000116         88  connected-to-db             value 'Y'.
000117     12  ws-file-in-status       pic xx  value spaces.
000118     12  ws-curl-return-cd       pic s9(8) comp-5 value +0.
000119     12  ws-curl-string.
000120         16  f                   pic x(16) value
000121          'curl -o /tmp/zip'.
000122         16  filename-zip        pic x(5)  value spaces.
000123         16  f                   pic xxxx value '.txt'.
000124         16  f                   pic x(15) value
000125          ' --data "USZip='.
000126         16  curl-zip            pic x(5) value zeros.
000127         16  f                   pic x(48) value
000128          '" http://webservicex.net/uszip.asmx/GetInfoByZIP'.
000129         16  f                   pic x value low-values.
000130
000131 01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
000132     88  RESP-NORMAL                    VALUE +0.
000133     88  resp-file-notfnd               value +12.
000134     88  RESP-NOTFND                    VALUE +13.
000135     88  resp-duprec                    value +14.
000136     88  resp-dupkey                    value +15.
000137     88  resp-invreq                    value +16.
000138     88  RESP-NOTOPEN                   VALUE +19.
000139     88  RESP-ENDFILE                   VALUE +20.
000140     88  resp-lengtherr                 value +22.
000141
000142 01  f.
000143     05  ws-outputzip            pic x(5).
000144     05  ws-city                 pic x(50).
000145     05  ws-state                pic xx.
000146
000147 01  WS-PASS-AREa.
000148     03  PA-ZIP                  PIC X(5).
000149     03  PA-ErrorCode            PIC X(10).
000150     03  PA-city                 PIC x(50).
000151     03  PA-state                PIC XX.
000152
000153 01  sqlconnect-parms.
000154     05  p-sql-server            PIC X(30).
000155     05  p-sql-database          PIC X(30).
000156     05  p-connect-return-code   pic s9(5) comp-5.
000157     05  p-sql-return-message    pic x(256).
000158
      ****************************************************************
      *                                                               
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        
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
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        *
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
000160
000161 01  DFHCOMMAREA                 PIC X(587).
000162
000163 01  var  pic x(30).
000164
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'WSZIPCD' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000165 VCOBOL-DUMMY-PROCEDURE.
000166
000167     display ' entering program WSZIPCD'
000168
000169     move dfhcommarea            to ws-pass-AREA
000170
000171     set P to address of KIXSYS
000172     CALL "getenv" using by value P returning var-ptr
000173     if var-ptr = null then
000174        display ' kixsys not set '
000175     else
000176        set address of var to var-ptr
000177        move 0 to env-var-len
000178        inspect var tallying env-var-len
000179          for characters before X'00'
000180        unstring var (1:env-var-len) delimited by '/'
000181           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000182              WS-KIX-SYS
000183        end-unstring
000184     end-if
000185
000186     perform 0010-init           thru 0010-exit
000187     perform 0020-connect        thru 0020-exit
000188     perform 0030-get-zipcd-data thru 0030-exit
000189     perform 0050-bld-pass-area  thru 0050-exit
000190     perform 0060-disconnect     thru 0060-exit
000191
000192     .
000193 0000-return.
000194
000195     move ws-pass-area           to dfhcommarea
000196
000197     
      * exec cics return
000198*    end-exec
      *    MOVE '.(                    ''   #00000325' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303030333235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000199
000200     
      * GOBACK

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'WSZIPCD' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK
000201
000202     .
000203 0010-init.
000204
000205     move pa-zip                 to zc-zipcode
000206     move pa-city                to zc-city
000207     move pa-state               to zc-state
000208
000209     .
000210 0010-exit.
000211     exit.
000212
000213 0020-connect.
000214
000215****  The below code is for when the db has been
000216****  converted to sql server 2016
000217     evaluate ws-kix-myenv
000218        when 'cid1p'
000219           move '//sdv-db01.cso.local:1433;'
000220                                 to p-sql-server
000221        when 'mdoff'
000222           move '//hov-tstdb01.cso.local:55330;'
000223                                 to p-sql-server
000224        when other
000225           move '//hov-tstdb01.cso.local:1433;'
000226                                 to p-sql-server
000227     end-evaluate
000228
000229
000230     move 'repository'           to p-sql-database
000231
000232     CALL 'SQLCONNECT' USING sqlconnect-parms
000233     display ' ret code ' p-connect-return-code
000234     move p-connect-return-code  to sqlcode
000235     move p-sql-return-message   to sqlerrmc
000236
000237
000238*
000239*     EXEC SQL
000240**       CONNECT TO :svr USER :usr-pass
000241*        CONNECT TO :svr
000242*          USER     :usr
000243*          USING    :pass
000244*     END-EXEC
000245
000246     if sqlcode not = 0
000247        display "Error: cannot connect "
000248        display sqlcode
000249        display sqlerrmc
000250     end-if
000251
000252     set connected-to-db to true
000253
000254     .
000255 0020-exit.
000256     exit.
000257
000258 0030-get-zipcd-data.
000259
000261     EXEC SQL
              SELECT
000262           County
000263        INTO
000264           :zc-County
000265        FROM
000266           ZipCodes
000267        WHERE
000268           ZipCode = :zc-zipcode
000269           and State = :zc-state
000270           and City = :zc-city
000271     END-EXEC
000272
000273     if sqlcode not = 0
000274        move 'NOTFOUND'          to pa-errorcode
000275        display "Error: cannot read row "
000276        display ' sql return code ' sqlcode
000277        display ' sql err mess    ' sqlerrmc
000278        display ' zip code        ' zc-zipcode
000279        go to 0030-exit
000280     end-if
000281
000282     .
000283 0030-exit.
000284     exit.
000285
000286 0050-bld-pass-area.
000287
000288     move function upper-case(zc-city)
000289                                 to pa-city
000290     move zc-state               to pa-state
000291
000292     .
000293 0050-exit.
000294     exit.
000295
000296 0060-disconnect.
000297
000299     EXEC SQL
              DISCONNECT
000300     END-EXEC
000301
000302     if sqlcode not = 0
000303        display "Error: cannot disconnect zipcodes "
000304        display ' sql return code ' sqlcode
000305        display ' sql err mess    ' sqlerrmc
000306     end-if
000307
000308     .
000309 0060-exit.
000310     exit.
000311
000312 0125-calc-field-len.
000313
000314     move 0                      to ws-string-len
000315     inspect ws-fld-3 tallying ws-string-len for all ' '
000316*    display ' string len b4 ' ws-string-len
000317     compute ws-string-len = 50 - ws-string-len
000318*    display ' string len af ' ws-string-len
000319
000320     .
000321 0125-exit.
000322     exit.

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
