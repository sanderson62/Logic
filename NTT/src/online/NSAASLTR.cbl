      *((program: NSAASLTR.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. NSAASLTR.
000004
000005*AUTHOR.     PABLO
000006*            COLLEYVILLE, TEXAS.
000007
000008*REMARKS.    EXECUTED FROM addasarch.html
000009
000010******************************************************************
000011*                   C H A N G E   L O G
000012*
000013* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000014*-----------------------------------------------------------------
000015*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000016* EFFECTIVE    NUMBER
000017*-----------------------------------------------------------------
000018* 071111    2011022800001  PEMA  NEW PROGRAM
000019* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
000020* 061421  CR2017031500001  PEMA  Update to CCM8
000021
000022******************************************************************
000023 ENVIRONMENT DIVISION.
000024
000025 DATA DIVISION.
000026 working-storage section.
       01  DFH-START PIC X(04).
000027
000028************************************************
000029* commarea passed to the business logic
000030************************************************
000031 77  bl-input-length             pic 9(04) BINARY.
000032
000033 01 srch-commarea.
000034*                                copy ELCADLTRSPI.
      *>>((file: ELCADLTRSPI))
000001******************************************************************
000002*                   C H A N G E   L O G
000003*
000004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000005*-----------------------------------------------------------------
000006*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000007* EFFECTIVE    NUMBER
000008*-----------------------------------------------------------------
000009* 060611    2011022800001  PEMA  NEW COPYBOOK
000010* 101812    2012101700002  AJRA  ADD ENDT ARCHIVE NO, SCREENID
000011* 110612    2012101700002  AJRA  EXPAND PASSED DATA
000012******************************************************************
000013****************************************
000014*  commarea for NaperSoft On Demand Admin services letters
000015*  (business logic input & output)
000016****************************************
000017
000018     03  BL-INPUT.
000019         05  BL-DATA-SRCE        PIC X.
000020         05  BL-LETTER-ID        PIC XXXX.
000021         05  BL-CARRIER          PIC X.
000022         05  BL-GROUP            PIC X(6).
000023         05  BL-STATE            PIC XX.
000024         05  BL-ACCOUNT          PIC X(10).
000025         05  BL-EFF-DT           PIC X(10).
000026         05  BL-CERT-NO          PIC X(11).
000027         05  BL-BATCH-NO         PIC X(6).
000028         05  BL-BATCH-SEQ        PIC 9(8).
000029         05  BL-RESP-NO          PIC X(10).
000030         05  BL-NO-OF-COPIES     PIC 99.
000031         05  BL-PROC-ID          PIC XXXX.
000032         05  BL-COMP-ID          PIC XXX.
000033         05  BL-PRINT-NOW-SW     PIC X.
000034         05  BL-ENC-CD           PIC XXX.
000035         05  BL-RESEND-DT        PIC X(10).
000036         05  BL-FOLLOW-UP-DT     PIC X(10).
000037         05  BL-ARCHIVE-NO       PIC 9(8).
000038         05  BL-FUNC             PIC X(8).
000039         05  BL-COMMENTS         PIC X(100).
000040         05  FILLER REDEFINES BL-COMMENTS.
000041             10  BL-REASON-CODE OCCURS 12 PIC X(4).
000042             10  BL-LETTER-TO-ACCT PIC X.
000043             10  BL-LETTER-TO-BENE PIC X.
000044             10  BL-WRITE-ERARCH   PIC X.
000045                 88  ERARCH-QWS      VALUE 'Q'.
000046                 88  ERARCH-BATCH    VALUE 'B'.
000047                 88  ERARCH-TEMP     VALUE 'T'.
000048             10  BL-PROCESS-TYPE PIC X(07).
000049             10  BL-CERT-FORM-ID PIC X(05).
000050             10  BL-ENDT-ARCH-NO PIC 9(08) BINARY.
000051             10  BL-SOURCE-SCREEN PIC X(8).
000052             10  FILLER          PIC X(25).
000053
000054     03  BL-OUTPUT.
000055         05  BL-STATUS                   PIC X.
000056             88  BL-OK                      VALUE "P".
000057             88  BL-FAIL                  VALUE "F".
000058         05  BL-MESSAGE          PIC X(50).
000059     03  BL-RECORD-PASSED-DATA   PIC X(6200).
000060     03  FILLER                  PIC X(31).
      *<<((file: ELCADLTRSPI))
000035
000036 01  INPUT-FROM-FORM.
000037     05  IFF-COMP-ID           PIC XXX.
000038     05  IFF-PRINT-NOW-SW      PIC X.
000039     05  IFF-ARCHIVE-NO        PIC 9(08).
000040     05  IFF-FUNC              PIC X(06).
000041     05  iff-batch-no          pic x(06).
000042     05  iff-batch-seq-no      pic 9(08).
000043     05  IFF-PROCESS-TYPE      PIC X(07).
000044
000045
000046************************************
000047* fields used to read web data
000048************************************
000049
000050 01  w-form-name       pic x(80).
000051 01  w-form-value      pic x(160).
000052 01  w-form-name-len   pic s9(8) comp.
000053 01  w-form-value-len  pic s9(8) comp.
000054 01  w-resp            pic s9(8) comp.
000055 01  w-doctoken        pic x(16).
000056
000057 01 output-msg.
000058    05 filler              pic x(4) value "MSG=".
000059    05 out-msg-text        pic x(50).
000060
000061 01  MISC.
000062     12  WS-RESPONSE             PIC S9(8)   COMP.
000063         88  RESP-NORMAL                  VALUE +00.
000064         88  RESP-NOTFND                  VALUE +13.
000065         88  RESP-DUPREC                  VALUE +14.
000066         88  RESP-DUPKEY                  VALUE +15.
000067         88  RESP-NOTOPEN                 VALUE +19.
000068         88  RESP-ENDFILE                 VALUE +20.
000069
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
       01  DFHCOMMAREA       PIC X(01).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'NSAASLTR' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000070 VCOBOL-DUMMY-PROCEDURE.
000071
000072*********************
000073* Receive web input
000074*********************
000075
000076     
      * exec cics web
000077*       startbr formfield resp(w-resp)
000078*     end-exec.
      *    MOVE 'X(f                   &  N#00000240' TO DFHEIV0
           MOVE X'582866202020202020202020' &
                X'202020202020202020202620' &
                X'204E233030303030323430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000079
000080      perform read-form thru read-form-exit
000081         until w-resp not = 0 .
      *   dfhresp(normal)
000082
000083      
      * exec cics web
000084*       endbr formfield
000085*     end-exec.
      *    MOVE 'X,f                   #   #00000247' TO DFHEIV0
           MOVE X'582C66202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303030323437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000086
000087     move spaces                 to bl-input
000088     MOVE IFF-COMP-ID            TO BL-COMP-ID
000089     MOVE IFF-PRINT-NOW-SW       TO BL-PRINT-NOW-SW
000090     MOVE IFF-ARCHIVE-NO         TO BL-ARCHIVE-NO
000091     MOVE IFF-FUNC               TO BL-FUNC
000092     move iff-batch-no           to bl-batch-no
000093     move iff-batch-seq-no       to bl-batch-seq
000094     MOVE IFF-PROCESS-TYPE       TO BL-PROCESS-TYPE
000095
000096*    DISPLAY ' I F F ' INPUT-FROM-FORM
000097*****************************************
000098* Invoke the SEARCH business logic
000099*****************************************
000100
000101*    DISPLAY ' BL INPUT        ' BL-INPUT
000102
000103     display ' about to link to nsaasbl '
000104     move function length(bl-input) to bl-input-length
000105     display ' bl input length ' bl-input-length
000106     
      * exec cics link
000107*       program  ('NSAASBL')
000108*       commarea (bl-input)
000109*       length   (bl-input-length)
000110*    end-exec.
           MOVE 'NSAASBL' TO DFHEIV1
      *    MOVE '."C                   (   #00000270' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303030323730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 bl-input, 
                 bl-input-length, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000111
000112     display ' returning from nsaasbl and about to cics return '
000113
000114     
      * exec cics
000115*       return
000116*    end-exec.
      *    MOVE '.(                    ''   #00000278' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303030323738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000117
000118******************************************************
000119* Read all fields of the incoming form, moving
000120* each to the corresponding field of the commarea
000121* (business logic input fields).  For a search,
000122* both form fields, last_name and first_initial,
000123* may be null.  In that case, set the business
000124* logic input fields to spaces.
000125******************************************************
000126
000127 read-form.
000128     move spaces to w-form-name.
000129     move length of w-form-name to w-form-name-len.
000130           move spaces to w-form-value.
000131     move length of w-form-value to w-form-value-len.
000132     
      * exec cics web readnext
000133*                  formfield(w-form-name)
000134*                  namelength(w-form-name-len)
000135*                  value(w-form-value)
000136*                  valuelength(w-form-value-len)
000137*                  resp(w-resp)
000138*    end-exec.
      *    MOVE 'X*FLVL                &  N#00000296' TO DFHEIV0
           MOVE X'582A464C564C202020202020' &
                X'202020202020202020202620' &
                X'204E233030303030323936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-form-name, 
                 w-form-name-len, 
                 w-form-value, 
                 w-form-value-len, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000139
000140     evaluate w-resp
000141        when 0 
      *   dfhresp(normal)
000142           evaluate w-form-name(1:w-form-name-len)
000143              when 'archkey'
000144                 if w-form-value-len not = 0
000145                    move w-form-value(1:w-form-value-len)
000146                           to INPUT-FROM-FORM
000147                 else
000148                    move spaces to INPUT-FROM-FORM
000149                 end-if
000150           end-evaluate
000151        when other
000152           continue
000153     end-evaluate.
000154
000155 read-form-exit.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSAASLTR' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSAASLTR' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
