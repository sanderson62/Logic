       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   NSRASLTR.

      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.                                       

      *REMARKS.    EXECUTED FROM INDEX.HTML

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 060611    2011022800001  PEMA  NEW PROGRAM
080612* 080612    2011022800001  AJRA  ADD AHL
101812* 101812  CR2012101700002  AJRA  ADD ENDARCH AND SCREENID
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc notes.
061421* 061421  CR2017031500001  PEMA  Update to CCM8
      ******************************************************************
       ENVIRONMENT DIVISION.                                            

       DATA DIVISION.                                                   
       working-storage section.

       77  ws-comm-length      pic 9(4) BINARY.

       01  P pointer.
       01  KIXHOST             pic x(9) value Z"HOSTNAME".

       01  WS-KIXHOST                  PIC X(10).
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
       01  bl-length                   pic s9(5) value +0 comp-3.
       01  sl-length                   pic 9(8) binary.

       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

      ************************************************
      * commarea passed to the business logic
      ************************************************
      	  
       01 srch-commarea.
                                       copy ELCADLTRSPI.

       01  INPUT-FROM-FORM.
           05  IFF-DATA-SRCE           PIC X.
           05  IFF-LETTER-ID           PIC XXXX.
           05  IFF-CARRIER             PIC X.
           05  IFF-GROUP               PIC X(6).
           05  IFF-STATE               PIC XX.
           05  IFF-ACCOUNT             PIC X(10).
           05  IFF-EFF-DT              PIC X(10).
           05  IFF-CERT-NO             PIC X(11).
           05  IFF-BATCH-NO            PIC X(6).
           05  IFF-BATCH-SEQ           PIC 9(8).
           05  IFF-RESP-NO             PIC X(10).
           05  IFF-NO-OF-COPIES        PIC 99.
           05  IFF-PROC-ID             PIC XXXX.
           05  IFF-COMP-ID             PIC XXX.
           05  IFF-PRINT-NOW-SW        PIC X.
           05  IFF-ENC-CD              PIC XXX.
           05  IFF-RESEND-DT           PIC X(10).
           05  IFF-FOLLOW-UP-DT        PIC X(10).
           05  IFF-ARCHIVE-NO          PIC 9(08).
           05  IFF-FUNC                PIC X(08).
101812*    05  IFF-COMMENTS            PIC X(70).
101812     05  IFF-ENDT-ARCH-NO        PIC 9(08).

      ************************************
      * fields used to read web data
      ************************************
      
       01  w-form-name       pic x(80).
       01  w-form-value      pic x(80).
       01  w-form-name-len   pic s9(8) comp.
       01  w-form-value-len  pic s9(8) comp.
       01  w-resp            pic s9(8) comp.
       01  w-doctoken        pic x(16).


      * COMP ID TPE REGION   GROUP NAME       HOST          HTTP PORT
      *
061421*  CID      CID1P      AcctServCID  sdv-nsft01       7001
061421*  CID      MDOFF      AcctServCID  hov-nsft01       7003
061421*  CID      CID1T      AcctServCID  hov-nsft01       6002
061421*  CID      PAUL       AcctServCID  hov-nsft01       5002
061421*  CID      TONY       AcctServCID  hov-nsft01       6003
061421*  DCC      CID1P      AcctServDCC  sdv-nsft01       7001
061421*  DCC      MDOFF      AcctServDCC  hov-nsft01       7003
061421*  DCC      CID1T      AcctServDCC  hov-nsft01       6002
061421*  DCC      PAUL       AcctServDCC  hov-nsft01       5002
061421*  DCC      TONY       AcctServDCC  hov-nsft01       6003
061421*  AHL      CID1P      AcctServAHL  sdv-nsft01       7001
061421*  AHL      MDOFF      AcctServAHL  hov-nsft01       7003
061421*  AHL      CID1T      AcctServAHL  hov-nsft01       6002
061421*  AHL      PAUL       AcctServAHL  hov-nsft01       5002
061421*  AHL      TONY       AcctServAHL  hov-nsft01       6003
061421*  AHL      AHLTST     AcctServAHL  hov-nsft01       6007
061421*  FNL      CID1P      AcctServFNL  sdv-nsft01       7001
061421*  FNL      MDOFF      AcctServFNL  hov-nsft01       7003
061421*  FNL      CID1T      AcctServFNL  hov-nsft01       6002
061421*  FNL      PAUL       AcctServFNL  hov-nsft01       5002
061421*  FNL      TONY       AcctServFNL  hov-nsft01       6003
061421*  FNL      AHLTST     AcctServFNL  hov-nsft01       6007


      ******************************************
      * symbol list text for ASERR template
      ******************************************

       01  WS-ASERR.
           05  F                       PIC X(6)  VALUE "PGMDT=".
           05  ASERR-DT                PIC X(12) VALUE ' '.
           05  F                       PIC X(5)  VALUE '&MSG='.
           05  ASERR-MESS              PIC X(50) VALUE ' '.

      ******************************************
      * symbol list text for ASHDR template
      ******************************************

       01  WS-PROD-CID-ASHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
           05  F                       PIC X(11) VALUE '&GROUPNAME='.
           05  F                       PIC X(11) VALUE 'AcctServCID'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.

       01  WS-PROD-DCC-ASHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
           05  F                       PIC X(11) VALUE "&GROUPNAME=".
           05  F                       PIC X(11) VALUE 'AcctServDCC'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.

020816 01  WS-PROD-VPP-ASHDR.
020816     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
020816     05  F                       PIC X(11) VALUE "&GROUPNAME=".
020816     05  F                       PIC X(11) VALUE 'AcctServVPP'.
020816     05  F                       PIC X(7)  VALUE '&UNAME='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
020816     05  F                       PIC X(11) VALUE '&UPASSWORD='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.

080612 01  WS-PROD-AHL-ASHDR.
080612     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
080612     05  F                       PIC X(11) VALUE "&GROUPNAME=".
080612     05  F                       PIC X(11) VALUE 'AcctServAHL'.
080612     05  F                       PIC X(7)  VALUE '&UNAME='.
080612     05  F                       PIC X(8)  VALUE 'batchjob'.
080612     05  F                       PIC X(11) VALUE '&UPASSWORD='.
080612     05  F                       PIC X(8)  VALUE 'batchjob'.

061421 01  WS-PROD-FNL-ASHDR.
061421     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
061421     05  F                       PIC X(11) VALUE '&GROUPNAME='.
061421     05  F                       PIC X(11) VALUE 'AcctServFNL'.
061421     05  F                       PIC X(7)  VALUE '&UNAME='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
061421     05  F                       PIC X(11) VALUE '&UPASSWORD='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.

       01  WS-TEST-CID-ASHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
           05  F                       PIC X(11) VALUE '&GROUPNAME='.
           05  F                       PIC X(11) VALUE 'AcctServCID'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.

       01  WS-TEST-DCC-ASHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
           05  F                       PIC X(11) VALUE "&GROUPNAME=".
           05  F                       PIC X(11) VALUE 'AcctServDCC'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
080612
020816 01  WS-TEST-VPP-ASHDR.
020816     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
020816     05  F                       PIC X(11) VALUE "&GROUPNAME=".
020816     05  F                       PIC X(11) VALUE 'AcctServVPP'.
020816     05  F                       PIC X(7)  VALUE '&UNAME='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
020816     05  F                       PIC X(11) VALUE '&UPASSWORD='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
080612
080612 01  WS-TEST-AHL-ASHDR.
080612     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
080612     05  F                       PIC X(11) VALUE "&GROUPNAME=".
080612     05  F                       PIC X(11) VALUE 'AcctServAHL'.
080612     05  F                       PIC X(7)  VALUE '&UNAME='.
080612     05  F                       PIC X(8)  VALUE 'batchjob'.
080612     05  F                       PIC X(11) VALUE '&UPASSWORD='.
080612     05  F                       PIC X(8)  VALUE 'batchjob'.

061421 01  WS-TEST-FNL-ASHDR.
061421     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
061421     05  F                       PIC X(11) VALUE '&GROUPNAME='.
061421     05  F                       PIC X(11) VALUE 'AcctServFNL'.
061421     05  F                       PIC X(7)  VALUE '&UNAME='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
061421     05  F                       PIC X(11) VALUE '&UPASSWORD='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.

      ******************************************
      * symbol list text for ASFTR template
      ******************************************

       01  WS-VAR-SLUNIKIX.
           05  FILLER                  PIC X(18) VALUE
                                       "HOSTINFO=slunikix:".
           05  WS-SL-PORT              PIC XXXX  VALUE '7001'.
           05  FILLER                  PIC X(11)  VALUE "&URLVARLST=".
061421     05  WS-SL-KEY               PIC X(32)  VALUE SPACES.
      *    05  FILLER                  PIC X(20) VALUE
      *                                "&HOSTINFO2=slunikix:".
      *    05  WS-SL-PORT2             PIC XXXX  VALUE '7001'.
      *    05  FILLER                  PIC X(12)  VALUE "&URLVARLST2=".
      *    05  WS-SL-KEY2              PIC X(12)  VALUE SPACES.

       01  WS-VAR-LOGICTEST.
           05  FILLER                  PIC X(19) VALUE
                                       "HOSTINFO=logictest:".
           05  WS-LT-PORT              PIC XXXX  VALUE '6002'.
           05  FILLER                  PIC X(11)  VALUE "&URLVARLST=".
061421     05  WS-LT-KEY               PIC X(32)  VALUE SPACES.
      *    05  FILLER                  PIC X(21) VALUE
      *                                "&HOSTINFO2=logictest:".
      *    05  WS-LT-PORT2             PIC XXXX  VALUE '6002'.
      *    05  FILLER                  PIC X(12)  VALUE "&URLVARLST2=".
      *    05  WS-LT-KEY2              PIC X(12)  VALUE SPACES.

       01 WS-ASFTR.
          05  OT-COMP-ID           PIC XXX.
          05  OT-PRINT-NOW-SW      PIC X.
          05  OT-ARCHIVE-NO        PIC 9(08).
061421    05  OT-FUNC              PIC X(06).
041320    05  ot-batch-no          pic x(06).
041320    05  ot-batch-seq-no      pic x(08).


       01 output-msg.
          05 filler              pic x(4) value "MSG=".
          05 out-msg-text        pic x(50).

       01  MISC.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPREC                  VALUE +14.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.

                                       COPY ELCDATE.

      *****************************************
      * symbol list for the ASBOD template
      *****************************************
      
                                       COPY NSCASVARS.

01168  LINKAGE SECTION.                                                 

       01  var  pic x(30).

       procedure division.

      *********************
      * Receive web input
      *********************
      
PEMTST*    DISPLAY ' ENTERING NSRASLTR '
      
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
PEMTST        DISPLAY '  KIXSYS = ' var (1:env-var-len)
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
PEMTST*       DISPLAY ' WS KIX SYS ' WS-KIXSYS
PEMTST*       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if

           set P to address of KIXHOST
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixhost not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              MOVE var(1:env-var-len)  to ws-kixhost
              DISPLAY ' WS KIX HOST ' WS-KIXHOST
           end-if

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-GREG-DATE-A-EDIT    TO ASERR-DT

           exec cics web 
              startbr formfield resp(w-resp) 
            end-exec.
      	         
            perform read-form thru read-form-exit
               until w-resp not = dfhresp(normal).
      	             
            exec cics web
              endbr formfield
            end-exec
		
      *    display ' form input ' INPUT-FROM-FORM
           MOVE SPACES                  TO BL-INPUT
           MOVE IFF-DATA-SRCE           TO BL-DATA-SRCE
           MOVE IFF-CARRIER             TO BL-CARRIER
           MOVE IFF-GROUP               TO BL-GROUP
           MOVE IFF-STATE               TO BL-STATE
           MOVE IFF-ACCOUNT             TO BL-ACCOUNT
           MOVE IFF-EFF-DT              TO BL-EFF-DT
           MOVE IFF-CERT-NO             TO BL-CERT-NO
           MOVE IFF-BATCH-NO            TO BL-BATCH-NO
041320                                     ot-batch-no
           MOVE IFF-BATCH-SEQ           TO BL-BATCH-SEQ
041320                                     ot-batch-seq-no
           MOVE IFF-RESP-NO             TO BL-RESP-NO
           MOVE IFF-LETTER-ID           TO BL-LETTER-ID
           MOVE IFF-NO-OF-COPIES        TO BL-NO-OF-COPIES
           MOVE IFF-PROC-ID             TO BL-PROC-ID
           MOVE IFF-COMP-ID             TO OT-COMP-ID
                                           BL-COMP-ID
           MOVE IFF-PRINT-NOW-SW        TO OT-PRINT-NOW-SW
                                           BL-PRINT-NOW-SW
           MOVE IFF-ENC-CD              TO BL-ENC-CD
           MOVE IFF-RESEND-DT           TO BL-RESEND-DT
           MOVE IFF-FOLLOW-UP-DT        TO BL-FOLLOW-UP-DT
101812*    MOVE IFF-COMMENTS            TO BL-COMMENTS
           MOVE IFF-ARCHIVE-NO          TO BL-ARCHIVE-NO
           MOVE IFF-FUNC                TO BL-FUNC
                                           OT-FUNC
101812     MOVE IFF-ENDT-ARCH-NO        TO BL-ENDT-ARCH-NO
101812
101812     EVALUATE BL-FUNC
101812         WHEN 'Letter'
101812             MOVE 'LETTER'        TO BL-SOURCE-SCREEN
101812         WHEN 'CrtVerif'
101812             MOVE 'VERIFY'        TO BL-SOURCE-SCREEN
101812         WHEN 'Endorse'
101812             MOVE 'ISS ENDT'      TO BL-SOURCE-SCREEN
101812         WHEN 'CancChg'
101812             MOVE 'CAN ENDT'      TO BL-SOURCE-SCREEN
101812         WHEN OTHER
101812             MOVE 'UNKNOWN'       TO BL-SOURCE-SCREEN
101812     END-EVALUATE
101812
      *****************************************
      * Since this program was called from the webservice
      * we really don't know if a real letter is being created
      * a "T" below will create temporary erarch and nsasextr
      * records and "X" we won't create anything
      *****************************************
           MOVE 'T'                    TO BL-WRITE-ERARCH
           IF BL-PRINT-NOW-SW = 'P'
              MOVE 'X'                 TO BL-WRITE-ERARCH
           END-IF

           MOVE 'X'                 TO BL-WRITE-ERARCH

PEMTST*    DISPLAY ' INPUT FORM ' WS-KEY
      *****************************************
      * Invoke the LETTER business logic
      *****************************************

      *    DISPLAY ' BL INPUT ' BL-INPUT
           MOVE FUNCTION LENGTH(SRCH-COMMAREA) TO WS-COMM-LENGTH

           exec cics link
              program('NSRASBL')
              commarea(srch-commarea)
              LENGTH  (WS-COMM-LENGTH)
           end-exec.

           IF BL-OK
              perform varying bl-length from +6200 by -1 until
                 (bl-record-passed-data (bl-length:1) not = ' ')
                 or (bl-length = +1)
              end-perform
PEMTST*       DISPLAY ' BL OK ' BL-ARCHIVE-NO
              move bl-length to sl-length
              move spaces to naper-output-data
              MOVE BL-RECORD-PASSED-DATA (1:bl-length)
                                       TO NAPER-OUTPUT-DATA
              MOVE BL-ARCHIVE-NO          TO OT-ARCHIVE-NO
           ELSE
              MOVE BL-MESSAGE          TO ASERR-MESS
              exec cics document create
                 doctoken   (w-doctoken)
                 template   ('ASERR')
                 symbollist (WS-ASERR)
                 resp       (WS-RESPONSE)
              end-exec
              exec cics web send
                 doctoken(w-doctoken)
		             resp   (WS-RESPONSE)
              end-exec
              exec cics
                 return
              end-exec
           END-IF

           evaluate ws-kixhost
              when 'slunikix'
                 evaluate ws-kix-myenv
                    when 'cid1p'
                       move '7001'     to ws-sl-port
      *                                   ws-sl-port2
                    when 'mdoff'
                       move '7003'     to ws-sl-port
      *                                   ws-sl-port2
                    when 'cid1t'
                       move '7002'     to ws-sl-port
      *                                   ws-sl-port2
                 end-evaluate
                 move ws-asftr         to ws-sl-key
      *                                   ws-sl-key2
              when 'logictest'
                 evaluate ws-kix-myenv
                    when 'cid1t'
                       move '6002'     to ws-lt-port
      *                                   ws-lt-port2
                    when 'tony'
                       move '6003'     to ws-lt-port
      *                                   ws-lt-port2
                    when 'paul'
                       move '5002'     to ws-lt-port
      *                                   ws-lt-port2
080612              when 'ahltst'
080612                 move '6007'     to ws-lt-port
080612*                                   ws-lt-port2
                 end-evaluate
                 move ws-asftr         to ws-lt-key
      *                                   ws-lt-key2
           end-evaluate

      ***********************************************************
      * Build output document.  There are three templates used
      * for this document.  ASHDR and ASFTR are the header
      * and footer, respectively.  ASBOD is used for the
      * actual data.  For each array entry in the business
      * logic output, set the symbol list from the array
      * entry and insert into the document using the ASBOD
      * template.
      ***********************************************************

      *    move bl-output-message to out-msg-text.

           evaluate true
              when iff-comp-id = 'DCC'
                 IF WS-KIX-MYENV = 'cid1p'
                    exec cics document create
                       doctoken   (w-doctoken)
                       template   ('ASHDR')
                       symbollist (WS-PROD-DCC-ASHDR)
                       resp       (WS-RESPONSE)
                    end-exec
                 ELSE
                    exec cics document create
                       doctoken   (w-doctoken)
                       template   ('ASHDR')
                       symbollist (WS-TEST-DCC-ASHDR)
                       resp       (WS-RESPONSE)
                    end-exec
                 END-IF
              when iff-comp-id = 'AHL'
080612           IF WS-KIX-MYENV = 'cid1p'
080612              exec cics document create
080612                 doctoken   (w-doctoken)
080612                 template   ('ASHDR')
080612                 symbollist (WS-PROD-AHL-ASHDR)
080612                 resp       (WS-RESPONSE)
080612              end-exec
080612           ELSE
080612              exec cics document create
080612                 doctoken   (w-doctoken)
080612                 template   ('ASHDR')
080612                 symbollist (WS-TEST-AHL-ASHDR)
080612                 resp       (WS-RESPONSE)
080612              end-exec
080612           END-IF
              when iff-comp-id = 'VPP'
020816           IF WS-KIX-MYENV = 'cid1p'
020816              exec cics document create
020816                 doctoken   (w-doctoken)
020816                 template   ('ASHDR')
020816                 symbollist (WS-PROD-VPP-ASHDR)
020816                 resp       (WS-RESPONSE)
020816              end-exec
020816           ELSE
020816              exec cics document create
020816                 doctoken   (w-doctoken)
020816                 template   ('ASHDR')
020816                 symbollist (WS-TEST-VPP-ASHDR)
020816                 resp       (WS-RESPONSE)
020816              end-exec
020816           END-IF
061421        when iff-comp-id = 'FNL'
061421           IF WS-KIX-MYENV = 'cid1p'
061421              exec cics document create
061421                 doctoken   (w-doctoken)
061421                 template   ('ASHDR')
061421                 symbollist (WS-PROD-FNL-ASHDR)
061421                 resp       (WS-RESPONSE)
061421              end-exec
061421           ELSE
061421              exec cics document create
061421                 doctoken   (w-doctoken)
061421                 template   ('ASHDR')
061421                 symbollist (WS-TEST-FNL-ASHDR)
061421                 resp       (WS-RESPONSE)
061421              end-exec
061421           END-IF
              when OTHER
                 IF WS-KIX-MYENV = 'cid1p'
                   exec cics document create
                      doctoken   (w-doctoken)
                      template   ('ASHDR')
                      symbollist (WS-PROD-CID-ASHDR)
                      resp       (WS-RESPONSE)
                   end-exec
                 ELSE
                   exec cics document create
                      doctoken   (w-doctoken)
                      template   ('ASHDR')
                      symbollist (WS-TEST-CID-ASHDR)
                      resp       (WS-RESPONSE)
                   end-exec
                 END-IF
           end-evaluate


      *    DISPLAY ' ASHDR DOC CREATE ' WS-RESPONSE

      *    move 1 to bl-index.
      *
      *    perform bl-output-record-count times
      *
      *        move bl-output-account-number(bl-index)
      *          to out-account-number
      *        move bl-output-last-name(bl-index)
      *          to out-last-name
      *        move bl-output-first-name(bl-index)
      *          to out-first-name
      *        move bl-output-middle-initial(bl-index)
      *          to out-middle-initial
      *

      *    MOVE 'EL01'                 TO OUT-TEMP
      *    MOVE 'ALWA'                 TO OUT-PROC-ID
      *    MOVE 'EL01'                 TO OUT-LETTER
      *    MOVE '9'                    TO OUT-CARRIER
      *    MOVE 'MCDANIEL'             TO OUT-ILNAME
      *    MOVE 'KATHI'                TO OUT-IFNAME
      *
      *    MOVE '0009235906 '          TO OUT-CERT-NO
      *    MOVE '0990000208'           TO OUT-ACCOUNT

           exec cics document set
              doctoken(w-doctoken)
              symbollist(NAPER-OUTPUT-DATA (1:bl-length))
061410*       delimiter ('?')
      *       length(length of NAPER-OUTPUT-DATA)
              length(sl-length)
		          resp   (WS-RESPONSE)
061410*     unescaped
           end-exec
      
      *    DISPLAY ' DOC SET    ' WS-RESPONSE

           exec cics document insert
              doctoken(w-doctoken)
              template('ASBOD')
		          resp   (WS-RESPONSE)
           end-exec

           IF BL-LETTER-TO-ACCT NOT = SPACES
              MOVE BL-LETTER-TO-ACCT   TO NAPER-OUTPUT-DATA (12:1)
      *       MOVE 'EL01T'             TO NAPER-OUTPUT-DATA (8:6)
              exec cics document set
                 doctoken(w-doctoken)
                 symbollist(NAPER-OUTPUT-DATA (1:bl-length))
                 length(sl-length)
                 resp   (WS-RESPONSE)
              end-exec
              
              exec cics document insert
                 doctoken(w-doctoken)
                 template('ASBOD')
                 resp   (WS-RESPONSE)
              end-exec
           END-IF

           IF BL-LETTER-TO-BENE NOT = SPACES
              MOVE BL-LETTER-TO-BENE   TO NAPER-OUTPUT-DATA (12:1)
      *       MOVE 'EL01T'             TO NAPER-OUTPUT-DATA (8:6)
              exec cics document set
                 doctoken(w-doctoken)
                 symbollist(NAPER-OUTPUT-DATA (1:bl-length))
                 length(sl-length)
                 resp   (WS-RESPONSE)
              end-exec
              
              exec cics document insert
                 doctoken(w-doctoken)
                 template('ASBOD')
                 resp   (WS-RESPONSE)
              end-exec
           END-IF

      *    DISPLAY ' ASBOD DOC INSERT ' WS-RESPONSE

      *
      *        add 1 to bl-index
      *
      *    end-perform.

      *    MOVE BL-CARRIER  TO OT-CARRIER
      *    MOVE BL-CLAIM-NO TO OT-CLMNO
      *    MOVE BL-CERT-NO  TO OT-CRTNO
      *    MOVE BL-ARCHIVE-NO TO OT-ARCHNO


           IF WS-KIXHOST = 'slunikix'
              display ' host slunikix ' ws-kixhost
              exec cics document set
                 doctoken(w-doctoken)
                 symbollist(WS-VAR-SLUNIKIX)
                 length(length of WS-VAR-SLUNIKIX)
		             resp   (WS-RESPONSE)
              end-exec
           else
              display ' host logictest ' ws-kixhost
              exec cics document set
                 doctoken(w-doctoken)
                 symbollist(WS-VAR-LOGICTEST)
                 length(length of WS-VAR-LOGICTEST)
		             resp   (WS-RESPONSE)
              end-exec
           end-if      
      *    DISPLAY ' DOC SET ' WS-RESPONSE

           exec cics document insert
              doctoken(w-doctoken)
              template('ASFTR')
		          resp   (WS-RESPONSE)
           end-exec

      *    DISPLAY ' ASFTR DOC INSERT ' WS-RESPONSE
      *    if bl-fail
      *       exec cics syncpoint rollback
      *       end-exec
      *    end-if.

      ****************************************
      * Send the document and return.
      ****************************************
      	   
           exec cics web send
              doctoken(w-doctoken)
		          resp   (WS-RESPONSE)
           end-exec.		

PEMTST*    DISPLAY ' WEB SEND  ' WS-RESPONSE

PEMTST*    DISPLAY ' ABOUT TO RETURN '
           exec cics
              return
           end-exec.
	   
	   
      ******************************************************
      * Read all fields of the incoming form, moving
      * each to the corresponding field of the commarea 
      * (business logic input fields). 
      ******************************************************
	   	
       read-form.
           move spaces to w-form-name.
           move length of w-form-name to w-form-name-len.
	         move spaces to w-form-value.
           move length of w-form-value to w-form-value-len.
           exec cics web readnext 
                         formfield(w-form-name)
                         namelength(w-form-name-len)
                         value(w-form-value)
                         valuelength(w-form-value-len)
                         resp(w-resp)
           end-exec.
           evaluate w-resp
              when dfhresp(normal)
                 evaluate w-form-name(1:w-form-name-len)
                    when 'data_src'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-DATA-SRCE
                       else
		                      move spaces to IFF-DATA-SRCE
                       end-if
                    when 'letter_id'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-LETTER-ID
                       else
		                      move spaces to IFF-LETTER-ID
                       end-if
                    when 'carrier'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                   to IFF-CARRIER
                       else
		                      move spaces to IFF-CARRIER
                       end-if
                    when 'grouping'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
			                                 to IFF-GROUP
                       else
		                      move spaces  to IFF-GROUP
                       end-if
                    when 'state_cd'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-STATE
                       else
		                      move spaces  to IFF-STATE
                       end-if
                    when 'acct_no'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-ACCOUNT
                       else
                          move spaces  to IFF-ACCOUNT
                       end-if
                    when 'eff_date'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-EFF-DT
                       else
		                      move spaces  to IFF-EFF-DT
                       end-if
                    when 'cert_no'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-CERT-NO
                       else
		                      move spaces  to IFF-CERT-NO
                       end-if
                    when 'batch_no'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-BATCH-NO
                       else
061421                  move zeros   to IFF-BATCH-NO
                       end-if
                    when 'batch_seq'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-BATCH-SEQ
                       else
		                      move zeros   to IFF-BATCH-SEQ
                       end-if
                    when 'resp_no'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-RESP-NO
                       else
		                      move spaces  to IFF-RESP-NO
                       end-if
                    when 'no_copies'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-NO-OF-COPIES
                       else
                          move zeros   to IFF-NO-OF-COPIES
                       end-if
                    when 'proc_id'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-PROC-ID
                       else
                          move 'JJVA'  to IFF-PROC-ID
                       end-if
                    when 'comp_id'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-COMP-ID
                       else
                          move 'CID'   to IFF-COMP-ID
                       end-if
                    when 'prt_now'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-PRINT-NOW-SW
                       else
                          move 'N'     to IFF-PRINT-NOW-SW
                       end-if
                    when 'enc_cd'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-ENC-CD
                       else
                          move '0'     to IFF-ENC-CD
                       end-if
                    when 'resend_dt'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-RESEND-DT
                       else
                          move SPACES  to IFF-RESEND-DT
                       end-if
                    when 'follow_dt'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-FOLLOW-UP-DT
                       else
                          move SPACES  to IFF-FOLLOW-UP-DT
                       end-if
                    when 'arch_no'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-ARCHIVE-NO
                       else
                          move ZEROS   to IFF-ARCHIVE-NO
                       end-if
                    when 'pgm_func'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-FUNC
                       else
                          move ZEROS   to IFF-FUNC
                       end-if
101812*             when 'ltr_comments'
101812*                if w-form-value-len not = 0
101812*                   move w-form-value(1:w-form-value-len)
101812*                                to IFF-COMMENTS
101812*                else
101812*                   move SPACES  to IFF-COMMENTS
101812*                end-if
101812              when 'end_arch'
101812                 if w-form-value-len not = 0
101812                    move w-form-value(1:w-form-value-len)
101812                                 to IFF-ENDT-ARCH-NO
101812                 else
101812                    move ZEROS  to IFF-ENDT-ARCH-NO
101812                 end-if
                 end-evaluate
              when other
                 continue
           end-evaluate.
       read-form-exit.		
					
       9700-DATE-LINK.                                                  

           EXEC CICS LINK                                               
               PROGRAM   ('ELDATCV')
               COMMAREA  (DATE-CONVERSION-DATA)                         
               LENGTH    (DC-COMM-LENGTH)                               
           END-EXEC.                                                    
                                                                        
                                                                        
       9700-EXIT.                                                       
            EXIT.
				        
