      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       identification division.
       program-id. ELHLDI.
061517******************************************************************
061517*                   C H A N G E   L O G
061517*
061517* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
061517*-----------------------------------------------------------------
061517*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
061517* EFFECTIVE    NUMBER
061517*-----------------------------------------------------------------
061517* 061517  CR               PEMA  ADD CHECK FOR CID1P VS. TEST
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
061517******************************************************************
       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT file-in ASSIGN TO dynamic ws-file-in
          FILE STATUS IS ws-file-in-status
                                  ORGANIZATION IS LINE SEQUENTIAL.
       data division.
       FILE SECTION.

       FD  file-in
           BLOCK CONTAINS 0
           RECORDING MODE F.
       01  file-in-rec                 pic x(200).

       working-storage section.
       77  s1 pic s999 comp-3 value +0.
       77  BYTE-OFFSET PIC S9(8) COMP VALUE +0.
       77  ws-eof-sw                   pic x  value spaces.
           88  end-of-input                  value 'Y'.
       77  ws-error-sw                 pic x  value spaces.
           88  error-found               value 'Y'.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  ws-string-len               pic s999 comp-3 value zeros.
       77  ws-vin-exists-sw            pic x value ' '.
           88  vin-exists                value 'Y'.

061517 01  P pointer.
061517 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
061517 01  var-ptr pointer.
061517 01  env-var-len                 pic 9(4)  binary.
061517 01  rc                          pic 9(9)  binary.
061517
061517 01  WS-KIXSYS.
061517     05  WS-KIX-FIL1             PIC X(10).
061517     05  WS-KIX-APPS             PIC X(10).
061517     05  WS-KIX-ENV              PIC X(10).
061517     05  WS-KIX-MYENV            PIC X(10).
061517     05  WS-KIX-SYS              PIC X(10).

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

       01  ws-misc.
           12  ws-file-in              pic x(26) value spaces.
           12  ws-connect-sw               pic x  value ' '.
               88  connected-to-db             value 'Y'.
           12  ws-file-in-status       pic xx  value spaces.
           12  ws-curl-return-cd       pic s9(8) comp-5 value +0.
           12  ws-curl-string.
               16  f                   pic x(13) value
                'curl -o /tmp/'.
               16  filename-vin        pic x(17) value spaces.
               16  f                   pic xxxx value '.txt'.
               16  f                   pic x(63) value
                ' "http://www.iihs-hldi.org/vinxml/Vindicate.asmx/Vindic
      -         'ate?Vin='.
               16  curl-vin            pic x(17).
               16  f                   pic x(18) value
                '&CCr=VKD0V7P7B7T1"'.
               16  f                   pic x value low-values.

       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

       01  ws-sql-record.
           05  ws-VIN                   pic x(17).
           05  ws-OutputVin             pic x(17).
           05  ws-ErrorCode             pic 9.
           05  ws-ErrorDesc             pic x(50).
           05  ws-CharSub               pic 9(5).
           05  ws-ModelYear             pic 9999.
           05  ws-MakeNumber            pic 9(5).
           05  ws-MakeName              pic x(50).
           05  ws-SeriesNumber          pic 9(5).
           05  ws-SeriesName            pic x(50).
           05  ws-ModelNumber           pic 9(5).
           05  ws-ModelName             pic x(50).
           05  ws-BodyStyleNumber       pic 9(5).
           05  ws-BodyStyleName         pic x(50).
           05  ws-CurbWeight            pic 9(5).
           05  ws-WheelBase             pic x(50).
           05  ws-Length                pic x(50).
           05  ws-Width                 pic x(50).
           05  ws-Height                pic x(50).
           05  ws-RestraintCode         pic 9(5).
           05  ws-RestraintDesc         pic x(50).
           05  ws-EngineNumber          pic 9(5).
           05  ws-EngineText            pic x(50).
           05  ws-HorsePowerMin         pic 9(5).
           05  ws-HorsePowerMax         pic 9(5).
           05  ws-ABSCode               pic 9(5).
           05  ws-VehicleType           pic 9(5).
           05  ws-TransNumber           pic 9(5).
           05  ws-TransDesc             pic x(50).
           05  ws-VehicleSizeID         pic 9(5).
           05  ws-VehicleSizeDesc       pic x(50).
           05  ws-VehicleClassID        pic 9(5).
           05  ws-VehicleClassDesc      pic x(50).
           05  ws-ATDDRLText            pic x(50).
           05  ws-ABSDescription        pic x(50).
           05  ws-BasePrice             pic 9(11).
           05  ws-InputVin              pic x(17).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  WS-RESPONSE2                PIC S9(8) COMP VALUE +0.
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

       01  msg-error.
           05  filler                  pic x(08) value ' error- '.
           05  me-response             pic 9(5)  value zeros.
           05  filler                  pic xx value spaces.
           05  me-response2            pic 9(5)  value zeros.
           05  filler                  pic x(5)  value spaces.
           05  me-command              pic x(20) value spaces.

       01 MSG.
          03 FILLER                    PIC X(11) VALUE "Vindicator ".
          03 msg-vindicator            PIC x(1800)  value spaces.

       01  WS-PASS-AREa.
           03  PA-VIN                  PIC X(17).
           03  PA-ErrorCode            PIC X(10).
           03  PA-ErrorDesc            PIC X(30).
           03  PA-ModelYear            PIC 9(7).
           03  PA-MakeName             PIC X(50).
           03  PA-ModelName            PIC X(50).
           03  PA-SeriesName           PIC X(50).

       LINKAGE SECTION.
       
       01  DFHCOMMAREA                 PIC X(587).

061517 01  var  pic x(30).

       procedure division.

           display ' entering program ELHLDIT'

           move dfhcommarea            to ws-pass-AREA

061517     set P to address of KIXSYS
061517     CALL "getenv" using by value P returning var-ptr
061517     if var-ptr = null then
061517        display ' kixsys not set '
061517     else
061517        set address of var to var-ptr
061517        move 0 to env-var-len
061517        inspect var tallying env-var-len
061517          for characters before X'00' 
061517        unstring var (1:env-var-len) delimited by '/'
061517           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
061517              WS-KIX-SYS
061517        end-unstring
061517     end-if

           perform 0010-init           thru 0010-exit
           perform 0020-exec-curl      thru 0020-exit
           perform 0030-get-hldi-data  thru 0030-exit
           perform 0050-bld-pass-area  thru 0050-exit

           .
       0000-return.

           move ws-pass-area           to dfhcommarea

           exec cics return
           end-exec

           GOBACK

           .
       0010-init.

           move pa-vin                 to filename-vin
                                          curl-vin

           .
       0010-exit.
           exit.

       0020-exec-curl.

           display ' curl string **' ws-curl-string '**'

           call "SYSTEM" using ws-curl-string
              returning ws-curl-return-cd

           display ' curl  return code ' ws-curl-return-cd

           if ws-curl-return-cd not = zeros
              display ' error curl ' ws-curl-return-cd
              display ' Vin =      ' ws-vin
              move '8'                 to pa-errorcode
              go to 0000-return
           end-if

           .
       0020-exit.
           exit.

       0030-get-hldi-data.

           string
              '/tmp/'
              filename-vin
              '.txt' delimited by size into ws-file-in
           end-string

           open input file-in

           if ws-file-in-status not = '00'
              display 'error open filein  ' ws-file-in-status
              move '8'                 to pa-errorcode
              go to 0000-return
           end-if

           perform 0105-read-input     thru 0105-exit

           If ws-file-in-status not = '00'
              Display 'Read failed, file status: '  ws-file-in-status
              move '8'                 to pa-errorcode
              go to 0000-return
           End-if

           move pa-vin                 to ws-vin
           perform 0120-process-xml    thru 0120-exit until
              end-of-input or error-found

           close file-in

      *    if not connected-to-db
      *       perform 1000-connect-db  thru 1000-exit
      *    end-if

      *    EXEC SQL
      *       DELETE FROM HLDI
      *       WHERE VIN = :WS-VIN
      *    END-EXEC

      *    if sqlcode not = 0
      *       display "Error: delete  " ws-vin
      *       display ' sql return code ' sqlcode
      *       display ' sql err mess    ' sqlerrmc
      *    end-if

           perform 0140-check-if-exist thru 0140-exit
           if vin-exists
              perform 0145-update-row  thru 0145-exit
           else
              perform 0130-insert-row  thru 0130-exit
           end-if

           if connected-to-db
              EXEC SQL
                  commit work release
              END-EXEC
              if sqlcode not = 0
                 display "Error: commit release "
                 display ' sql return code ' sqlcode
                 display ' sql err mess    ' sqlerrmc
              end-if
           end-if

           if connected-to-db
              EXEC SQL
                  disconnect all
              END-EXEC
              move ' ' to ws-connect-sw
           end-if

           .
       0030-exit.
           exit.

       0105-read-input.

            Read file-in end
               set end-of-input to true
            end-read

           .
       0105-exit.
           exit.

       0120-process-xml.

           display ' processing record ' file-in-rec
           unstring file-in-rec
              delimited by '<' or '>'
              into ws-fld-1
                 ws-fld-2
                 ws-fld-3
                 ws-fld-4
                 ws-fld-5
           end-unstring

      *    display ' fld 1 ' ws-fld-1
      *    display ' fld 2 ' ws-fld-2
      *    display ' fld 3 ' ws-fld-3
      *    display ' fld 4 ' ws-fld-4
      *    display ' fld 5 ' ws-fld-5

           perform 0125-calc-field-len thru 0125-exit

           evaluate ws-fld-2
              when 'OutputVIN'
                 move ws-fld-3         to ws-outputvin
              when 'ErrorCode'
                 display ' found error code ' ws-fld-3
                 inspect ws-fld-3 (1:1) replacing all spaces by zeros
                 move ws-error-cd      to ws-errorcode
                 if ws-error-cd <> '0'
                    set error-found to true
                 end-if
              when 'ErrorDesc'
                 move ws-fld-3         to ws-errordesc
              when 'CharSub'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-charsub
              when 'ModelYear'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-modelyear
              when 'MakeNumber'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-makenumber
              when 'MakeName'
                 move ws-fld-3         to ws-makename
              when 'SeriesNumber'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-seriesnumber
              when 'SeriesName'
                 move ws-fld-3         to ws-seriesname
              when 'ModelNumber'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-modelnumber
              when 'ModelName'
                 move ws-fld-3         to ws-modelname
              when 'BodyStyleNumber'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-bodystylenumber
              when 'BodyStyleName'
                 move ws-fld-3         to ws-bodystylename
              when 'CurbWeight'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-curbweight
              when 'Wheelbase'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-wheelbase
              when 'Length'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-length
              when 'Width'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-width
              when 'Height'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-height
              when 'RestraintCode'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-restraintcode
              when 'RestraintDesc'
                 move ws-fld-3         to ws-restraintdesc
              when 'EngineNumber'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-enginenumber
              when 'EngineText'
                 move ws-fld-3         to ws-enginetext
              when 'HorsepowerMin'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-horsepowermin
              when 'HorsepowerMax'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-horsepowermax
              when 'ABSCode'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-abscode
              when 'VehicleType'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-vehicletype
              when 'TransNumber'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-transnumber
              when 'TransDesc'
                 move ws-fld-3         to ws-transdesc
              when 'VehicleSizeID'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-vehiclesizeid
              when 'VehicleSizeDesc'
                 move ws-fld-3         to ws-vehiclesizedesc
              when 'VehicleClassID'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-vehicleclassid
              when 'VehicleClassDesc'
                 move ws-fld-3         to ws-vehicleclassdesc
              when 'ATD_DRLText'
                 move ws-fld-3         to ws-atddrltext
              when 'ABSDescription'
                 move ws-fld-3         to ws-absdescription
              when 'BasePrice'
                 move ws-fld-3 (1:ws-string-len)
                                       to ws-baseprice
              when 'InputVIN'
                 move ws-fld-3         to ws-inputvin
              when other
                 display ' no worries ' ws-fld-2 ' ' ws-fld-3
           end-evaluate

           perform 0105-read-input     thru 0105-exit

           .
       0120-exit.
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

       0130-insert-row.

           if not connected-to-db
              perform 1000-connect-db  thru 1000-exit
           end-if

           EXEC SQL
              INSERT into HLDI (
                 VIN         
                ,OutputVin   
                ,ErrorCode   
                ,ErrorDesc   
                ,CharSub     
                ,ModelYear   
                ,MakeNumber  
                ,MakeName    
                ,SeriesNumber
                ,SeriesName 
                ,ModelNumber 
                ,ModelName
                ,BodyStyleNumber 
                ,BodyStyleName   
                ,CurbWeight      
                ,WheelBase       
                ,Length          
                ,Width           
                ,Height          
                ,RestraintCode   
                ,RestraintDesc   
                ,EngineNumber    
                ,EngineText      
                ,HorsePowerMin   
                ,HorsePowerMax   
                ,ABSCode         
                ,VehicleType     
                ,TransNumber     
                ,TransDesc       
                ,VehicleSizeID   
                ,VehicleSizeDesc 
                ,VehicleClassID  
                ,VehicleClassDesc
                ,ATDDRLText      
                ,ABSDescription  
                ,BasePrice       
                ,InputVin)
               VALUES (
                :ws-VIN         
                ,:ws-OutputVin   
                ,:ws-ErrorCode   
                ,:ws-ErrorDesc   
                ,:ws-CharSub     
                ,:ws-ModelYear   
                ,:ws-MakeNumber  
                ,:ws-MakeName    
                ,:ws-SeriesNumber
                ,:ws-SeriesName 
                ,:ws-ModelNumber 
                ,:ws-ModelName
                ,:ws-BodyStyleNumber 
                ,:ws-BodyStyleName   
                ,:ws-CurbWeight      
                ,:ws-WheelBase       
                ,:ws-Length          
                ,:ws-Width           
                ,:ws-Height          
                ,:ws-RestraintCode   
                ,:ws-RestraintDesc   
                ,:ws-EngineNumber    
                ,:ws-EngineText      
                ,:ws-HorsePowerMin   
                ,:ws-HorsePowerMax   
                ,:ws-ABSCode         
                ,:ws-VehicleType     
                ,:ws-TransNumber     
                ,:ws-TransDesc       
                ,:ws-VehicleSizeID   
                ,:ws-VehicleSizeDesc 
                ,:ws-VehicleClassID  
                ,:ws-VehicleClassDesc
                ,:ws-ATDDRLText      
                ,:ws-ABSDescription  
                ,:ws-BasePrice       
                ,:ws-InputVin)
           end-exec

           if sqlcode not = 0
              display "Error: cannot insert row "
              display ' sql return code ' sqlcode
              move sqlcode to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0130-exit.
           exit.

       0140-check-if-exist.

           if not connected-to-db
              perform 1000-connect-db  thru 1000-exit
           end-if

           EXEC SQL
              SELECT OutputVin
              INTO   :ws-outputvin
              FROM   HLDI
              WHERE  VIN = :WS-VIN
           END-EXEC

           move sqlcode to ws-dis-sql-code
           display ' code after select ' ws-dis-sql-code

           if sqlcode = 0
              display ' found vin ' ws-vin
              set vin-exists to true
           else
              display "Error: cannot select row "
              display ' sql return code ' sqlcode
              move sqlcode to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0140-exit.
           exit.

       0145-update-row.

           display ' About to Update row ' ws-vin

           EXEC SQL
              UPDATE HLDI SET
                 OutputVin          = :ws-OutputVin                
                ,ErrorCode          = :ws-ErrorCode                
                ,ErrorDesc          = :ws-ErrorDesc                
                ,CharSub            = :ws-CharSub                  
                ,ModelYear          = :ws-ModelYear                
                ,MakeNumber         = :ws-MakeNumber               
                ,MakeName           = :ws-MakeName                 
                ,SeriesNumber       = :ws-SeriesNumber             
                ,SeriesName         = :ws-SeriesName               
                ,ModelNumber        = :ws-ModelNumber              
                ,ModelName          = :ws-ModelName                
                ,BodyStyleNumber    = :ws-BodyStyleNumber          
                ,BodyStyleName      = :ws-BodyStyleName            
                ,CurbWeight         = :ws-CurbWeight               
                ,WheelBase          = :ws-WheelBase                
                ,Length             = :ws-Length                   
                ,Width              = :ws-Width                    
                ,Height             = :ws-Height                   
                ,RestraintCode      = :ws-RestraintCode            
                ,RestraintDesc      = :ws-RestraintDesc            
                ,EngineNumber       = :ws-EngineNumber             
                ,EngineText         = :ws-EngineText               
                ,HorsePowerMin      = :ws-HorsePowerMin            
                ,HorsePowerMax      = :ws-HorsePowerMax            
                ,ABSCode            = :ws-ABSCode                  
                ,VehicleType        = :ws-VehicleType              
                ,TransNumber        = :ws-TransNumber              
                ,TransDesc          = :ws-TransDesc                
                ,VehicleSizeID      = :ws-VehicleSizeID            
                ,VehicleSizeDesc    = :ws-VehicleSizeDesc          
                ,VehicleClassID     = :ws-VehicleClassID           
                ,VehicleClassDesc   = :ws-VehicleClassDesc         
                ,ATDDRLText         = :ws-ATDDRLText               
                ,ABSDescription     = :ws-ABSDescription           
                ,BasePrice          = :ws-BasePrice                
                ,InputVin           = :ws-InputVin
               WHERE VIN = :WS-VIN
           end-exec

           if sqlcode not = 0
              display "Error: cannot update row "
              display ' sql return code ' sqlcode
              move sqlcode to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0145-exit.
           exit.

       0050-bld-pass-area.

           move ws-errorcode           to pa-errorcode
           move ws-errordesc           to pa-errordesc
           move ws-modelyear           to pa-modelyear
           move ws-makename            to pa-makename
           move ws-modelname           to pa-modelname
           move ws-seriesname          to pa-seriesname

           .           
       0050-exit.
           exit.

       1000-CONNECT-DB.

063022     move 'TEST_Logic'           to svr
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022     if ws-kix-myenv = 'cid1p'
063022        move 'PROD_Logic'        to svr
063022     end-if

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
       
           if sqlcode not = 0
              move '8'                 to pa-errorcode
              display "Error: cannot connect "
              display sqlcode
              display sqlerrmc
              go to 0000-return
           else
              set connected-to-db to true
           end-if

           .
       1000-EXIT.
           EXIT.
