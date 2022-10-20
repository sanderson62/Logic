      *((program: SQLCLMHS.cl2))
000001*$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
000002 identification division.
000003 program-id. SQLCLMHS.
000004******************************************************************
000005*                   C H A N G E   L O G
000006*
000007* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000008*-----------------------------------------------------------------
000009*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000010* EFFECTIVE    NUMBER
000011*-----------------------------------------------------------------
000012* 020218  CR2017062000002  PEMA  New program to verify CLM HIST.
000013* 031221  CR2019012500003  PEMA  Change connection to sdv-db01
000014* 110921  CR2021051200001  PEMA  Onbase Workflow project
000015******************************************************************
000016 environment division.
000017 INPUT-OUTPUT SECTION.
000018 FILE-CONTROL.
000019
000020 data division.
000021 FILE SECTION.
000022
000023 working-storage section.
       01  DFH-START PIC X(04).
000024 77  s1 pic s999 comp-3 value +0.
000025 77  BYTE-OFFSET PIC S9(8) COMP VALUE +0.
000026 77  ws-eof-sw                   pic x  value spaces.
000027     88  end-of-input                  value 'Y'.
000028 77  ws-error-sw                 pic x  value spaces.
000029     88  error-found               value 'Y'.
000030 77  ws-string-len               pic s999 comp-3 value zeros.
000031
000032 01  P pointer.
000033 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000034 01  var-ptr pointer.
000035 01  env-var-len                 pic 9(4)  binary.
000036 01  rc                          pic 9(9)  binary.
000037
000038 01  WS-KIXSYS.
000039     05  WS-KIX-FIL1             PIC X(10).
000040     05  WS-KIX-APPS             PIC X(10).
000041     05  WS-KIX-ENV              PIC X(10).
000042     05  WS-KIX-MYENV            PIC X(10).
000043     05  WS-KIX-SYS              PIC X(10).
000044
000045
000046*EXEC SQL
000047*   INCLUDE SQLDA
000048*END-EXEC
000049
000052*EXEC SQL
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
000053
000055 EXEC SQL
          BEGIN DECLARE SECTION
000056 END-EXEC
000057
000058 01  sqlcmd                      pic x(1024).
000059 01  svr                         pic x(32).
000060 01  usr                         pic x(32).
000061 01  pass                        pic x(32).
000062 01  usr-pass                    pic x(64).
000063 01  ws-disp-code                pic s9(11).
000064
000065***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000066***                                                            ***
000067***  These indicators are used to determine if a variable      ***
000068***  is passed nulls from sql. The indicator will be -1        ***
000069***  if the value on sql is nulls and +0 if the value is       ***
000070***  something other than nulls. Here is an example on how     ***
000071***  to use the indicator variables.                           ***
000072***                                                            ***
000073***     EXEC SQL                                               ***
000074***        fetch checkapp into                                 ***
000075***           :db-app-status :nu-app-status,                   ***
000076***           :db-app-by     :nu-app-by,                       ***
000077***           :db-app-date   :nu-app-date,                     ***
000078***           :db-app-batch  :nu-app-batch                     ***
000079***     END-EXEC                                               ***
000080***                                                            ***
000081***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000082
000083 01  indicator-vaiables-for-nulls.
000084     05  nu-state                pic s9(4) comp value +0.
000085     05  nu-city                 pic s9(4) comp value +0.
000086     05  nu-county               pic s9(4) comp value +0.
000087
000088 01  clm-hist-stuff.
000089     05  ch-state                pic xx.
000090     05  ch-account              pic x(10).
000091     05  ch-eff-dt               pic x(10).
000092     05  ch-cert-no              pic x(11).
000093     05  ch-clm-count            pic 9(5).
000094
000096 EXEC SQL
          END DECLARE SECTION
000097 END-EXEC
000098
000099 01  ws-misc.
000100     12  ws-file-in              pic x(26) value spaces.
000101     12  ws-connect-sw               pic x  value ' '.
000102         88  connected-to-db             value 'Y'.
000103     12  ws-file-in-status       pic xx  value spaces.
000104     12  ws-curl-return-cd       pic s9(8) comp-5 value +0.
000105     12  ws-curl-string.
000106         16  f                   pic x(16) value
000107          'curl -o /tmp/zip'.
000108         16  filename-zip        pic x(5)  value spaces.
000109         16  f                   pic xxxx value '.txt'.
000110         16  f                   pic x(15) value
000111          ' --data "USZip='.
000112         16  curl-zip            pic x(5) value zeros.
000113         16  f                   pic x(48) value
000114          '" http://webservicex.net/uszip.asmx/GetInfoByZIP'.
000115         16  f                   pic x value low-values.
000116
000117 01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
000118     88  RESP-NORMAL                    VALUE +0.
000119     88  resp-file-notfnd               value +12.
000120     88  RESP-NOTFND                    VALUE +13.
000121     88  resp-duprec                    value +14.
000122     88  resp-dupkey                    value +15.
000123     88  resp-invreq                    value +16.
000124     88  RESP-NOTOPEN                   VALUE +19.
000125     88  RESP-ENDFILE                   VALUE +20.
000126     88  resp-lengtherr                 value +22.
000127
000128 01  f.
000129     05  ws-outputzip            pic x(5).
000130     05  ws-city                 pic x(50).
000131     05  ws-state                pic xx.
000132
000133 01  WS-PASS-AREa.
000134     03  pa-state                pic xx.
000135     03  pa-account              pic x(10).
000136     03  pa-eff-dt               pic x(10).
000137     03  pa-cert-no              pic x(11).
000138     03  pa-clm-count            pic 9(5).
000139
000140 01  sqlconnect-parms.
000141     05  p-sql-server            PIC X(30).
000142     05  p-sql-database          PIC X(30).
000143     05  p-connect-return-code   pic s9(5) comp-5.
000144     05  p-sql-return-message    pic x(256).
000145
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
000147
000148 01  DFHCOMMAREA                 PIC X(587).
000149
000150 01  var  pic x(30).
000151
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'SQLCLMHS' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000152 VCOBOL-DUMMY-PROCEDURE.
000153
000154     display ' entering program SQLCLMHS'
000155
000156     move dfhcommarea            to ws-pass-AREA
000157
000158     display ' pa state        ' pa-state
000159     display ' pa acct         ' pa-account
000160     display ' pa eff dt       ' pa-eff-dt
000161     display ' pa cert no      ' pa-cert-no
000162     display ' pa-clm-count    ' pa-clm-count
000163
000164     set P to address of KIXSYS
000165     CALL "getenv" using by value P returning var-ptr
000166     if var-ptr = null then
000167        display ' kixsys not set '
000168     else
000169        set address of var to var-ptr
000170        move 0 to env-var-len
000171        inspect var tallying env-var-len
000172          for characters before X'00'
000173        unstring var (1:env-var-len) delimited by '/'
000174           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000175              WS-KIX-SYS
000176        end-unstring
000177     end-if
000178
000179     perform 0010-init           thru 0010-exit
000180     perform 0020-connect        thru 0020-exit
000181     perform 0030-get-clmhs-data thru 0030-exit
000182     perform 0050-bld-pass-area  thru 0050-exit
000183     perform 0060-disconnect     thru 0060-exit
000184
000185     .
000186 0000-return.
000187
000188     move ws-pass-area           to dfhcommarea
000189
000190     
      * exec cics return
000191*    end-exec
      *    MOVE '.(                    ''   #00000318' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303030333138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000192
000193     
      * GOBACK

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SQLCLMHS' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK
000194
000195     .
000196 0010-init.
000197
000198     move pa-state               to ch-state
000199     move pa-account             to ch-account
000200     move pa-eff-dt              to ch-eff-dt
000201     move pa-cert-no             to ch-cert-no
000202
000203     .
000204 0010-exit.
000205     exit.
000206
000207 0020-connect.
000208
000209****  The below code is for when the db has been
000210****  converted to sql server 2016
000211     evaluate ws-kix-myenv
000212        when 'cid1p'
000213           move '//sdv-db01.cso.local:1433;'
000214                                 to p-sql-server
000215        when 'mdoff'
000216           move '//hov-tstdb01.cso.local:55330;'
000217                                 to p-sql-server
000218        when other
000219           move '//hov-tstdb01.cso.local:1433;'
000220                                 to p-sql-server
000221     end-evaluate
000222
000223     move 'CSO_ClaimVerification' to p-sql-database
000224
000225     CALL 'SQLCONNECT' USING sqlconnect-parms
000226     display ' ret code ' p-connect-return-code
000227     move p-connect-return-code  to sqlcode
000228     move p-sql-return-message   to sqlerrmc
000229
000230
000231     if sqlcode not = 0
000232        display "Error: cannot connect "
000233        display sqlcode
000234        display sqlerrmc
000235     end-if
000236
000237     set connected-to-db to true
000238
000239     .
000240 0020-exit.
000241     exit.
000242
000243 0030-get-clmhs-data.
000244
000246     EXEC SQL
              CALL spch_CntFindClaims_online
000247          (
000248           @PendState       = :ch-state,
000249           @PendAcct        = :ch-account,
000250           @PendEffDt       = :ch-eff-dt,
000251           @PendCertNo      = :ch-cert-no,
000252           @NumClms         = :ch-clm-count
000253          )
000254     END-EXEC
000255
000256     move sqlcode                to ws-disp-code
000257     display ' sql ret code ' ws-disp-code ' ' ch-clm-count
000258
000259     if sqlcode not = 0 and 1 and 100
000260*       move 'NOTFOUND'          to pa-errorcode
000261        display "Error: cannot read row "
000262        display ' sql return code ' sqlcode
000263        display ' sql err mess    ' sqlerrmc
000264        display ' cert no         ' ch-cert-no
000265        go to 0030-exit
000266     end-if
000267
000268     .
000269 0030-exit.
000270     exit.
000271
000272 0050-bld-pass-area.
000273
000274     move ch-clm-count           to pa-clm-count
000275
000276     .
000277 0050-exit.
000278     exit.
000279
000280 0060-disconnect.
000281
000283     EXEC SQL
              DISCONNECT
000284     END-EXEC
000285
000286     if sqlcode not = 0
000287        display "Error: cannot disconnect zipcodes "
000288        display ' sql return code ' sqlcode
000289        display ' sql err mess    ' sqlerrmc
000290     end-if
000291
000292     .
000293 0060-exit.
000294     exit.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SQLCLMHS' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SQLCLMHS' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
