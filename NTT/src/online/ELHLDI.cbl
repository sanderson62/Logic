      *((program: ELHLDI.cl2))
000001*$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
000002 identification division.
000003 program-id. ELHLDI.
000004******************************************************************
000005*                   C H A N G E   L O G
000006*
000007* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000008*-----------------------------------------------------------------
000009*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000010* EFFECTIVE    NUMBER
000011*-----------------------------------------------------------------
000012* 061517  CR               PEMA  ADD CHECK FOR CID1P VS. TEST
000013* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
000014******************************************************************
000015 environment division.
000016 INPUT-OUTPUT SECTION.
000017 FILE-CONTROL.
000018
000019 SELECT file-in ASSIGN TO dynamic ws-file-in
000020    FILE STATUS IS ws-file-in-status
000021                            ORGANIZATION IS LINE SEQUENTIAL.
000022 data division.
000023 FILE SECTION.
000024
000025 FD  file-in
000026     BLOCK CONTAINS 0
000027     RECORDING MODE F.
000028 01  file-in-rec                 pic x(200).
000029
000030 working-storage section.
       01  DFH-START PIC X(04).
000031 77  s1 pic s999 comp-3 value +0.
000032 77  BYTE-OFFSET PIC S9(8) COMP VALUE +0.
000033 77  ws-eof-sw                   pic x  value spaces.
000034     88  end-of-input                  value 'Y'.
000035 77  ws-error-sw                 pic x  value spaces.
000036     88  error-found               value 'Y'.
000037 77  ws-sql-code                 pic s9(7) value zeros.
000038 77  ws-dis-sql-code             pic -9999999 value zeros.
000039 77  ws-string-len               pic s999 comp-3 value zeros.
000040 77  ws-vin-exists-sw            pic x value ' '.
000041     88  vin-exists                value 'Y'.
000042
000043 01  P pointer.
000044 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000045 01  var-ptr pointer.
000046 01  env-var-len                 pic 9(4)  binary.
000047 01  rc                          pic 9(9)  binary.
000048
000049 01  WS-KIXSYS.
000050     05  WS-KIX-FIL1             PIC X(10).
000051     05  WS-KIX-APPS             PIC X(10).
000052     05  WS-KIX-ENV              PIC X(10).
000053     05  WS-KIX-MYENV            PIC X(10).
000054     05  WS-KIX-SYS              PIC X(10).
000055
000056 01  ws-xml-stuff.
000057     05  ws-fld-1                pic x(20) value spaces.
000058     05  ws-fld-2                pic x(20) value spaces.
000059     05  ws-fld-3                pic x(50) value spaces.
000060     05  ws-error-cd redefines
000061         ws-fld-3                pic 9.
000062     05  ws-len-of-5 redefines
000063         ws-fld-3                pic 9(5).
000064     05  ws-model-year redefines
000065         ws-fld-3                pic 9999.
000066     05  ws-base-price redefines
000067         ws-fld-3                pic 9(11).
000068     05  ws-fld-4                pic x(20) value spaces.
000069     05  ws-fld-5                pic x(20) value spaces.
000070
000071 01  ws-misc.
000072     12  ws-file-in              pic x(26) value spaces.
000073     12  ws-connect-sw               pic x  value ' '.
000074         88  connected-to-db             value 'Y'.
000075     12  ws-file-in-status       pic xx  value spaces.
000076     12  ws-curl-return-cd       pic s9(8) comp-5 value +0.
000077     12  ws-curl-string.
000078         16  f                   pic x(13) value
000079          'curl -o /tmp/'.
000080         16  filename-vin        pic x(17) value spaces.
000081         16  f                   pic xxxx value '.txt'.
000082         16  f                   pic x(63) value
000083          ' "http://www.iihs-hldi.org/vinxml/Vindicate.asmx/Vindic
000084-         'ate?Vin='.
000085         16  curl-vin            pic x(17).
000086         16  f                   pic x(18) value
000087          '&CCr=VKD0V7P7B7T1"'.
000088         16  f                   pic x value low-values.
000089
000090*EXEC SQL
000091*   INCLUDE SQLDA
000092*END-EXEC
000093
000096*EXEC SQL
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
000097
000099 EXEC SQL
          BEGIN DECLARE SECTION
000100 END-EXEC
000101
000102 01  svr                         pic x(32).
000103 01  usr                         pic x(32).
000104 01  pass                        pic x(32).
000105 01  usr-pass                    pic x(64).
000106 01  ws-disp-code                pic s9(11).
000107
000108 01  ws-sql-record.
000109     05  ws-VIN                   pic x(17).
000110     05  ws-OutputVin             pic x(17).
000111     05  ws-ErrorCode             pic 9.
000112     05  ws-ErrorDesc             pic x(50).
000113     05  ws-CharSub               pic 9(5).
000114     05  ws-ModelYear             pic 9999.
000115     05  ws-MakeNumber            pic 9(5).
000116     05  ws-MakeName              pic x(50).
000117     05  ws-SeriesNumber          pic 9(5).
000118     05  ws-SeriesName            pic x(50).
000119     05  ws-ModelNumber           pic 9(5).
000120     05  ws-ModelName             pic x(50).
000121     05  ws-BodyStyleNumber       pic 9(5).
000122     05  ws-BodyStyleName         pic x(50).
000123     05  ws-CurbWeight            pic 9(5).
000124     05  ws-WheelBase             pic x(50).
000125     05  ws-Length                pic x(50).
000126     05  ws-Width                 pic x(50).
000127     05  ws-Height                pic x(50).
000128     05  ws-RestraintCode         pic 9(5).
000129     05  ws-RestraintDesc         pic x(50).
000130     05  ws-EngineNumber          pic 9(5).
000131     05  ws-EngineText            pic x(50).
000132     05  ws-HorsePowerMin         pic 9(5).
000133     05  ws-HorsePowerMax         pic 9(5).
000134     05  ws-ABSCode               pic 9(5).
000135     05  ws-VehicleType           pic 9(5).
000136     05  ws-TransNumber           pic 9(5).
000137     05  ws-TransDesc             pic x(50).
000138     05  ws-VehicleSizeID         pic 9(5).
000139     05  ws-VehicleSizeDesc       pic x(50).
000140     05  ws-VehicleClassID        pic 9(5).
000141     05  ws-VehicleClassDesc      pic x(50).
000142     05  ws-ATDDRLText            pic x(50).
000143     05  ws-ABSDescription        pic x(50).
000144     05  ws-BasePrice             pic 9(11).
000145     05  ws-InputVin              pic x(17).
000146
000148 EXEC SQL
          END DECLARE SECTION
000149 END-EXEC
000150
000151 01  WS-RESPONSE2                PIC S9(8) COMP VALUE +0.
000152 01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
000153     88  RESP-NORMAL                    VALUE +0.
000154     88  resp-file-notfnd               value +12.
000155     88  RESP-NOTFND                    VALUE +13.
000156     88  resp-duprec                    value +14.
000157     88  resp-dupkey                    value +15.
000158     88  resp-invreq                    value +16.
000159     88  RESP-NOTOPEN                   VALUE +19.
000160     88  RESP-ENDFILE                   VALUE +20.
000161     88  resp-lengtherr                 value +22.
000162
000163 01  msg-error.
000164     05  filler                  pic x(08) value ' error- '.
000165     05  me-response             pic 9(5)  value zeros.
000166     05  filler                  pic xx value spaces.
000167     05  me-response2            pic 9(5)  value zeros.
000168     05  filler                  pic x(5)  value spaces.
000169     05  me-command              pic x(20) value spaces.
000170
000171 01 MSG.
000172    03 FILLER                    PIC X(11) VALUE "Vindicator ".
000173    03 msg-vindicator            PIC x(1800)  value spaces.
000174
000175 01  WS-PASS-AREa.
000176     03  PA-VIN                  PIC X(17).
000177     03  PA-ErrorCode            PIC X(10).
000178     03  PA-ErrorDesc            PIC X(30).
000179     03  PA-ModelYear            PIC 9(7).
000180     03  PA-MakeName             PIC X(50).
000181     03  PA-ModelName            PIC X(50).
000182     03  PA-SeriesName           PIC X(50).
000183
000184 01  sqlconnect-parms.
000185     05  p-sql-server            PIC X(30).
000186     05  p-sql-database          PIC X(30).
000187     05  p-connect-return-code   pic s9(5) comp-5.
000188     05  p-sql-return-message    pic x(256).
000189
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
000191
000192 01  DFHCOMMAREA                 PIC X(587).
000193
000194 01  var  pic x(30).
000195
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'ELHLDI' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000196 VCOBOL-DUMMY-PROCEDURE.
000197
000198     display ' entering program ELHLDIT'
000199
000200     move dfhcommarea            to ws-pass-AREA
000201
000202     set P to address of KIXSYS
000203     CALL "getenv" using by value P returning var-ptr
000204     if var-ptr = null then
000205        display ' kixsys not set '
000206     else
000207        set address of var to var-ptr
000208        move 0 to env-var-len
000209        inspect var tallying env-var-len
000210          for characters before X'00'
000211        unstring var (1:env-var-len) delimited by '/'
000212           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000213              WS-KIX-SYS
000214        end-unstring
000215     end-if
000216
000217     perform 0010-init           thru 0010-exit
000218     perform 0020-exec-curl      thru 0020-exit
000219     perform 0030-get-hldi-data  thru 0030-exit
000220     perform 0050-bld-pass-area  thru 0050-exit
000221
000222     .
000223 0000-return.
000224
000225     move ws-pass-area           to dfhcommarea
000226
000227     
      * exec cics return
000228*    end-exec
      *    MOVE '.(                    ''   #00000355' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303030333535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000229
000230     
      * GOBACK

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELHLDI' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK
000231
000232     .
000233 0010-init.
000234
000235     move pa-vin                 to filename-vin
000236                                    curl-vin
000237
000238     .
000239 0010-exit.
000240     exit.
000241
000242 0020-exec-curl.
000243
000244     display ' curl string **' ws-curl-string '**'
000245
000246     call "SYSTEM" using ws-curl-string
000247        returning ws-curl-return-cd
000248
000249     display ' curl  return code ' ws-curl-return-cd
000250
000251     if ws-curl-return-cd not = zeros
000252        display ' error curl ' ws-curl-return-cd
000253        display ' Vin =      ' ws-vin
000254        move '8'                 to pa-errorcode
000255        go to 0000-return
000256     end-if
000257
000258     .
000259 0020-exit.
000260     exit.
000261
000262 0030-get-hldi-data.
000263
000264     string
000265        '/tmp/'
000266        filename-vin
000267        '.txt' delimited by size into ws-file-in
000268     end-string
000269
000270     open input file-in
000271
000272     if ws-file-in-status not = '00'
000273        display 'error open filein  ' ws-file-in-status
000274        move '8'                 to pa-errorcode
000275        go to 0000-return
000276     end-if
000277
000278     perform 0105-read-input     thru 0105-exit
000279
000280     If ws-file-in-status not = '00'
000281        Display 'Read failed, file status: '  ws-file-in-status
000282        move '8'                 to pa-errorcode
000283        go to 0000-return
000284     End-if
000285
000286     move pa-vin                 to ws-vin
000287     perform 0120-process-xml    thru 0120-exit until
000288        end-of-input or error-found
000289
000290     close file-in
000291
000292*    if not connected-to-db
000293*       perform 1000-connect-db  thru 1000-exit
000294*    end-if
000295
000296*    EXEC SQL
000297*       DELETE FROM HLDI
000298*       WHERE VIN = :WS-VIN
000299*    END-EXEC
000300
000301*    if sqlcode not = 0
000302*       display "Error: delete  " ws-vin
000303*       display ' sql return code ' sqlcode
000304*       display ' sql err mess    ' sqlerrmc
000305*    end-if
000306
000307     perform 0140-check-if-exist thru 0140-exit
000308     if vin-exists
000309        perform 0145-update-row  thru 0145-exit
000310     else
000311        perform 0130-insert-row  thru 0130-exit
000312     end-if
000313
000314     if connected-to-db
000317        EXEC SQL
      *           commit work release
                  commit work
000318        END-EXEC
000319        if sqlcode not = 0
000320           display "Error: commit release "
000321           display ' sql return code ' sqlcode
000322           display ' sql err mess    ' sqlerrmc
000323        end-if
000324     end-if
000325
000326     if connected-to-db
000328        EXEC SQL
                  disconnect all
000329        END-EXEC
000330        move ' ' to ws-connect-sw
000331     end-if
000332
000333     .
000334 0030-exit.
000335     exit.
000336
000337 0105-read-input.
000338
000339      Read file-in end
000340         set end-of-input to true
000341      end-read
000342
000343     .
000344 0105-exit.
000345     exit.
000346
000347 0120-process-xml.
000348
000349     display ' processing record ' file-in-rec
000350     unstring file-in-rec
000351        delimited by '<' or '>'
000352        into ws-fld-1
000353           ws-fld-2
000354           ws-fld-3
000355           ws-fld-4
000356           ws-fld-5
000357     end-unstring
000358
000359*    display ' fld 1 ' ws-fld-1
000360*    display ' fld 2 ' ws-fld-2
000361*    display ' fld 3 ' ws-fld-3
000362*    display ' fld 4 ' ws-fld-4
000363*    display ' fld 5 ' ws-fld-5
000364
000365     perform 0125-calc-field-len thru 0125-exit
000366
000367     evaluate ws-fld-2
000368        when 'OutputVIN'
000369           move ws-fld-3         to ws-outputvin
000370        when 'ErrorCode'
000371           display ' found error code ' ws-fld-3
000372           inspect ws-fld-3 (1:1) replacing all spaces by zeros
000373           move ws-error-cd      to ws-errorcode
000374           if ws-error-cd <> '0'
000375              set error-found to true
000376           end-if
000377        when 'ErrorDesc'
000378           move ws-fld-3         to ws-errordesc
000379        when 'CharSub'
000380           move ws-fld-3 (1:ws-string-len)
000381                                 to ws-charsub
000382        when 'ModelYear'
000383           move ws-fld-3 (1:ws-string-len)
000384                                 to ws-modelyear
000385        when 'MakeNumber'
000386           move ws-fld-3 (1:ws-string-len)
000387                                 to ws-makenumber
000388        when 'MakeName'
000389           move ws-fld-3         to ws-makename
000390        when 'SeriesNumber'
000391           move ws-fld-3 (1:ws-string-len)
000392                                 to ws-seriesnumber
000393        when 'SeriesName'
000394           move ws-fld-3         to ws-seriesname
000395        when 'ModelNumber'
000396           move ws-fld-3 (1:ws-string-len)
000397                                 to ws-modelnumber
000398        when 'ModelName'
000399           move ws-fld-3         to ws-modelname
000400        when 'BodyStyleNumber'
000401           move ws-fld-3 (1:ws-string-len)
000402                                 to ws-bodystylenumber
000403        when 'BodyStyleName'
000404           move ws-fld-3         to ws-bodystylename
000405        when 'CurbWeight'
000406           move ws-fld-3 (1:ws-string-len)
000407                                 to ws-curbweight
000408        when 'Wheelbase'
000409           move ws-fld-3 (1:ws-string-len)
000410                                 to ws-wheelbase
000411        when 'Length'
000412           move ws-fld-3 (1:ws-string-len)
000413                                 to ws-length
000414        when 'Width'
000415           move ws-fld-3 (1:ws-string-len)
000416                                 to ws-width
000417        when 'Height'
000418           move ws-fld-3 (1:ws-string-len)
000419                                 to ws-height
000420        when 'RestraintCode'
000421           move ws-fld-3 (1:ws-string-len)
000422                                 to ws-restraintcode
000423        when 'RestraintDesc'
000424           move ws-fld-3         to ws-restraintdesc
000425        when 'EngineNumber'
000426           move ws-fld-3 (1:ws-string-len)
000427                                 to ws-enginenumber
000428        when 'EngineText'
000429           move ws-fld-3         to ws-enginetext
000430        when 'HorsepowerMin'
000431           move ws-fld-3 (1:ws-string-len)
000432                                 to ws-horsepowermin
000433        when 'HorsepowerMax'
000434           move ws-fld-3 (1:ws-string-len)
000435                                 to ws-horsepowermax
000436        when 'ABSCode'
000437           move ws-fld-3 (1:ws-string-len)
000438                                 to ws-abscode
000439        when 'VehicleType'
000440           move ws-fld-3 (1:ws-string-len)
000441                                 to ws-vehicletype
000442        when 'TransNumber'
000443           move ws-fld-3 (1:ws-string-len)
000444                                 to ws-transnumber
000445        when 'TransDesc'
000446           move ws-fld-3         to ws-transdesc
000447        when 'VehicleSizeID'
000448           move ws-fld-3 (1:ws-string-len)
000449                                 to ws-vehiclesizeid
000450        when 'VehicleSizeDesc'
000451           move ws-fld-3         to ws-vehiclesizedesc
000452        when 'VehicleClassID'
000453           move ws-fld-3 (1:ws-string-len)
000454                                 to ws-vehicleclassid
000455        when 'VehicleClassDesc'
000456           move ws-fld-3         to ws-vehicleclassdesc
000457        when 'ATD_DRLText'
000458           move ws-fld-3         to ws-atddrltext
000459        when 'ABSDescription'
000460           move ws-fld-3         to ws-absdescription
000461        when 'BasePrice'
000462           move ws-fld-3 (1:ws-string-len)
000463                                 to ws-baseprice
000464        when 'InputVIN'
000465           move ws-fld-3         to ws-inputvin
000466        when other
000467           display ' no worries ' ws-fld-2 ' ' ws-fld-3
000468     end-evaluate
000469
000470     perform 0105-read-input     thru 0105-exit
000471
000472     .
000473 0120-exit.
000474     exit.
000475
000476 0125-calc-field-len.
000477
000478     move 0                      to ws-string-len
000479     inspect ws-fld-3 tallying ws-string-len for all ' '
000480*    display ' string len b4 ' ws-string-len
000481     compute ws-string-len = 50 - ws-string-len
000482*    display ' string len af ' ws-string-len
000483
000484     .
000485 0125-exit.
000486     exit.
000487
000488 0130-insert-row.
000489
000490     if not connected-to-db
000491        perform 1000-connect-db  thru 1000-exit
000492     end-if
000493
000495     EXEC SQL
              INSERT into HLDI (
000496           VIN
000497          ,OutputVin
000498          ,ErrorCode
000499          ,ErrorDesc
000500          ,CharSub
000501          ,ModelYear
000502          ,MakeNumber
000503          ,MakeName
000504          ,SeriesNumber
000505          ,SeriesName
000506          ,ModelNumber
000507          ,ModelName
000508          ,BodyStyleNumber
000509          ,BodyStyleName
000510          ,CurbWeight
000511          ,WheelBase
000512          ,Length
000513          ,Width
000514          ,Height
000515          ,RestraintCode
000516          ,RestraintDesc
000517          ,EngineNumber
000518          ,EngineText
000519          ,HorsePowerMin
000520          ,HorsePowerMax
000521          ,ABSCode
000522          ,VehicleType
000523          ,TransNumber
000524          ,TransDesc
000525          ,VehicleSizeID
000526          ,VehicleSizeDesc
000527          ,VehicleClassID
000528          ,VehicleClassDesc
000529          ,ATDDRLText
000530          ,ABSDescription
000531          ,BasePrice
000532          ,InputVin)
000533         VALUES (
000534          :ws-VIN
000535          ,:ws-OutputVin
000536          ,:ws-ErrorCode
000537          ,:ws-ErrorDesc
000538          ,:ws-CharSub
000539          ,:ws-ModelYear
000540          ,:ws-MakeNumber
000541          ,:ws-MakeName
000542          ,:ws-SeriesNumber
000543          ,:ws-SeriesName
000544          ,:ws-ModelNumber
000545          ,:ws-ModelName
000546          ,:ws-BodyStyleNumber
000547          ,:ws-BodyStyleName
000548          ,:ws-CurbWeight
000549          ,:ws-WheelBase
000550          ,:ws-Length
000551          ,:ws-Width
000552          ,:ws-Height
000553          ,:ws-RestraintCode
000554          ,:ws-RestraintDesc
000555          ,:ws-EngineNumber
000556          ,:ws-EngineText
000557          ,:ws-HorsePowerMin
000558          ,:ws-HorsePowerMax
000559          ,:ws-ABSCode
000560          ,:ws-VehicleType
000561          ,:ws-TransNumber
000562          ,:ws-TransDesc
000563          ,:ws-VehicleSizeID
000564          ,:ws-VehicleSizeDesc
000565          ,:ws-VehicleClassID
000566          ,:ws-VehicleClassDesc
000567          ,:ws-ATDDRLText
000568          ,:ws-ABSDescription
000569          ,:ws-BasePrice
000570          ,:ws-InputVin)
000571     end-exec
000572
000573     if sqlcode not = 0
000574        display "Error: cannot insert row "
000575        display ' sql return code ' sqlcode
000576        move sqlcode to ws-sql-code
000577        move ws-sql-code to ws-dis-sql-code
000578        display ' dis sql code ' ws-dis-sql-code
000579        display ' sql err mess    ' sqlerrmc
000580     end-if
000581
000582     .
000583 0130-exit.
000584     exit.
000585
000586 0140-check-if-exist.
000587
000588     if not connected-to-db
000589        perform 1000-connect-db  thru 1000-exit
000590     end-if
000591
000593     EXEC SQL
              SELECT OutputVin
000594        INTO   :ws-outputvin
000595        FROM   HLDI
000596        WHERE  VIN = :WS-VIN
000597     END-EXEC
000598
000599     move sqlcode to ws-dis-sql-code
000600     display ' code after select ' ws-dis-sql-code
000601
000602     if sqlcode = 0
000603        display ' found vin ' ws-vin
000604        set vin-exists to true
000605     else
000606        display "Error: cannot select row "
000607        display ' sql return code ' sqlcode
000608        move sqlcode to ws-sql-code
000609        move ws-sql-code to ws-dis-sql-code
000610        display ' dis sql code ' ws-dis-sql-code
000611        display ' sql err mess    ' sqlerrmc
000612     end-if
000613
000614     .
000615 0140-exit.
000616     exit.
000617
000618 0145-update-row.
000619
000620     display ' About to Update row ' ws-vin
000621
000623     EXEC SQL
              UPDATE HLDI SET
000624           OutputVin          = :ws-OutputVin
000625          ,ErrorCode          = :ws-ErrorCode
000626          ,ErrorDesc          = :ws-ErrorDesc
000627          ,CharSub            = :ws-CharSub
000628          ,ModelYear          = :ws-ModelYear
000629          ,MakeNumber         = :ws-MakeNumber
000630          ,MakeName           = :ws-MakeName
000631          ,SeriesNumber       = :ws-SeriesNumber
000632          ,SeriesName         = :ws-SeriesName
000633          ,ModelNumber        = :ws-ModelNumber
000634          ,ModelName          = :ws-ModelName
000635          ,BodyStyleNumber    = :ws-BodyStyleNumber
000636          ,BodyStyleName      = :ws-BodyStyleName
000637          ,CurbWeight         = :ws-CurbWeight
000638          ,WheelBase          = :ws-WheelBase
000639          ,Length             = :ws-Length
000640          ,Width              = :ws-Width
000641          ,Height             = :ws-Height
000642          ,RestraintCode      = :ws-RestraintCode
000643          ,RestraintDesc      = :ws-RestraintDesc
000644          ,EngineNumber       = :ws-EngineNumber
000645          ,EngineText         = :ws-EngineText
000646          ,HorsePowerMin      = :ws-HorsePowerMin
000647          ,HorsePowerMax      = :ws-HorsePowerMax
000648          ,ABSCode            = :ws-ABSCode
000649          ,VehicleType        = :ws-VehicleType
000650          ,TransNumber        = :ws-TransNumber
000651          ,TransDesc          = :ws-TransDesc
000652          ,VehicleSizeID      = :ws-VehicleSizeID
000653          ,VehicleSizeDesc    = :ws-VehicleSizeDesc
000654          ,VehicleClassID     = :ws-VehicleClassID
000655          ,VehicleClassDesc   = :ws-VehicleClassDesc
000656          ,ATDDRLText         = :ws-ATDDRLText
000657          ,ABSDescription     = :ws-ABSDescription
000658          ,BasePrice          = :ws-BasePrice
000659          ,InputVin           = :ws-InputVin
000660         WHERE VIN = :WS-VIN
000661     end-exec
000662
000663     if sqlcode not = 0
000664        display "Error: cannot update row "
000665        display ' sql return code ' sqlcode
000666        move sqlcode to ws-sql-code
000667        move ws-sql-code to ws-dis-sql-code
000668        display ' dis sql code ' ws-dis-sql-code
000669        display ' sql err mess    ' sqlerrmc
000670     end-if
000671
000672     .
000673 0145-exit.
000674     exit.
000675
000676 0050-bld-pass-area.
000677
000678     move ws-errorcode           to pa-errorcode
000679     move ws-errordesc           to pa-errordesc
000680     move ws-modelyear           to pa-modelyear
000681     move ws-makename            to pa-makename
000682     move ws-modelname           to pa-modelname
000683     move ws-seriesname          to pa-seriesname
000684
000685     .
000686 0050-exit.
000687     exit.
000688
000689 1000-CONNECT-DB.
000690
000691****  The below code is for when the db has been
000692****  converted to sql server 2016
000693     evaluate ws-kix-myenv
000694        when 'cid1p'
000695           move '//sdv-db01.cso.local:1433;'
000696                                 to p-sql-server
000697        when 'mdoff'
000698           move '//hov-tstdb01.cso.local:55330;'
000699                                 to p-sql-server
000700        when other
000701           move '//hov-tstdb01.cso.local:1433;'
000702                                 to p-sql-server
000703     end-evaluate
000704
000705     move 'Logic'                to p-sql-database
000706
000707     CALL 'SQLCONNECT' USING sqlconnect-parms
000708     display ' ret code ' p-connect-return-code
000709     move p-connect-return-code  to sqlcode
000710     move p-sql-return-message   to sqlerrmc
000711
000712*     EXEC SQL
000713**       CONNECT TO :svr USER :usr-pass
000714*        CONNECT TO :svr
000715*          USER     :usr
000716*          USING    :pass
000717*     END-EXEC
000718
000719     if sqlcode not = 0
000720        move '8'                 to pa-errorcode
000721        display "Error: cannot connect "
000722        display sqlcode
000723        display sqlerrmc
000724        go to 0000-return
000725     else
000726        set connected-to-db to true
000727     end-if
000728
000729     .
000730 1000-EXIT.
000731     EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELHLDI' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELHLDI' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
