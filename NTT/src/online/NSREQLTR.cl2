       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   NSREQLTR.

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
      * 121802    2009122800001  PEMA  NEW PROGRAM
022212* 022212    2011120900003  AJRA  ADD AHL
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
061421* 061421  CR2017031500001  PEMA  Update to CCM8
      ******************************************************************
       ENVIRONMENT DIVISION.                                            

       DATA DIVISION.                                                   
       working-storage section.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.

061421 01  filler.
061421     05  a1                      pic s999 comp-3 value +0.
061421     05  a2                      pic s999 comp-3 value +0.
061421     05  m1                      pic s999 comp-3 value +0.
061421     05  v1                      pic s999 comp-3 value +0.
061421     05  v2                      pic s999 comp-3 value +0.
061421     05  WS-WORK-FIELD           PIC X(90)    VALUE SPACES.
           
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
                                       copy ELCLTRSPI.

       01  INPUT-FROM-FORM.
           05  IFF-CARRIER             PIC X.
           05  IFF-CLAIM-NO            PIC X(7).
           05  IFF-CERT-NO             PIC X(11).
           05  IFF-LETTER-ID           PIC XXXX.
           05  IFF-FOLLOW-UP-DT        PIC X(10).
           05  IFF-RESEND-DT           PIC X(10).
           05  IFF-NO-OF-COPIES        PIC 99.
           05  IFF-PROC-ID             PIC XXXX.
           05  IFF-COMP-ID             PIC XXX.
           05  IFF-PRINT-NOW-SW        PIC X.
           05  IFF-ENC-CD              PIC XXX.
           05  IFF-REGARDING           PIC X(70).

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
061421*  CID      CID1P      BatchClaims     sdv-nsft01       7001
061421*  CID      MDOFF      BatchClaims     hov-nsft01       7003
061421*  CID      CID1T      BatchClaims     hov-nsft01       6002
061421*  CID      PAUL       BatchClaims     hov-nsft01       5002
061421*  CID      TONY       BatchClaims     hov-nsft01       6003
061421*  DCC      CID1P      BatchDCCClaims  sdv-nsft01       7001
061421*  DCC      MDOFF      BatchDCCClaims  hov-nsft01       7003
061421*  DCC      CID1T      BatchDCCClaims  hov-nsft01       6002
061421*  DCC      PAUL       BatchDCCClaims  hov-nsft01       5002
061421*  DCC      TONY       BatchDCCClaims  hov-nsft01       6003
061421*  AHL      CID1P      BatchAHLClaims  sdv-nsft01       7001
061421*  AHL      MDOFF      BatchAHLClaims  hov-nsft01       7003
061421*  AHL      CID1T      BatchAHLClaims  hov-nsft01       6002
061421*  AHL      PAUL       BatchAHLClaims  hov-nsft01       5002
061421*  AHL      TONY       BatchAHLClaims  hov-nsft01       6003
061421*  AHL      AHLTST     BatchAHLClaims  hov-nsft01       6007
061421*  FNL      CID1P      BatchFNLClaims  sdv-nsft01       7001
061421*  FNL      MDOFF      BatchFNLClaims  hov-nsft01       7003
061421*  FNL      CID1T      BatchFNLClaims  hov-nsft01       6002
061421*  FNL      PAUL       BatchFNLClaims  hov-nsft01       5002
061421*  FNL      TONY       BatchFNLClaims  hov-nsft01       6003
061421*  FNL      AHLTST     BatchFNLClaims  hov-nsft01       6007

      ******************************************
      * symbol list text for PEMHDR template
      ******************************************

       01  WS-PROD-CID-PEMHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
           05  F                       PIC X(11) VALUE '&GROUPNAME='.
           05  F                       PIC X(11) VALUE 'BatchClaims'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.

       01  WS-PROD-DCC-PEMHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
           05  F                       PIC X(11) VALUE "&GROUPNAME=".
           05  F                       PIC X(14) VALUE 'BatchDCCClaims'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.

020816 01  WS-PROD-VPP-PEMHDR.
020816     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
020816     05  F                       PIC X(11) VALUE "&GROUPNAME=".
020816     05  F                       PIC X(14) VALUE 'BatchVPPClaims'.
020816     05  F                       PIC X(7)  VALUE '&UNAME='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
020816     05  F                       PIC X(11) VALUE '&UPASSWORD='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.

022212 01  WS-PROD-AHL-PEMHDR.
022212     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
022212     05  F                       PIC X(11) VALUE "&GROUPNAME=".
022212     05  F                       PIC X(14) VALUE 'BatchAHLClaims'.
022212     05  F                       PIC X(7)  VALUE '&UNAME='.
022212     05  F                       PIC X(8)  VALUE 'batchjob'.
022212     05  F                       PIC X(11) VALUE '&UPASSWORD='.
022212     05  F                       PIC X(8)  VALUE 'batchjob'.
022212
061421 01  WS-PROD-FNL-PEMHDR.
061421     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                                    'sdv-nsft01.cso.local'.
061421     05  F                       PIC X(11) VALUE '&GROUPNAME='.
061421     05  F                       PIC X(14) VALUE 'BatchFNLClaims'.
061421     05  F                       PIC X(7)  VALUE '&UNAME='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
061421     05  F                       PIC X(11) VALUE '&UPASSWORD='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.

       01  WS-TEST-CID-PEMHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
           05  F                       PIC X(11) VALUE '&GROUPNAME='.
           05  F                       PIC X(11) VALUE 'BatchClaims'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.

       01  WS-TEST-DCC-PEMHDR.
           05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
           05  F                       PIC X(11) VALUE "&GROUPNAME=".
           05  F                       PIC X(14) VALUE 'BatchDCCClaims'.
           05  F                       PIC X(7)  VALUE '&UNAME='.
           05  F                       PIC X(8)  VALUE 'batchjob'.
           05  F                       PIC X(11) VALUE '&UPASSWORD='.
           05  F                       PIC X(8)  VALUE 'batchjob'.

020816 01  WS-TEST-VPP-PEMHDR.
020816     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
020816     05  F                       PIC X(11) VALUE "&GROUPNAME=".
020816     05  F                       PIC X(14) VALUE 'BatchVPPClaims'.
020816     05  F                       PIC X(7)  VALUE '&UNAME='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.
020816     05  F                       PIC X(11) VALUE '&UPASSWORD='.
020816     05  F                       PIC X(8)  VALUE 'batchjob'.

022212 01  WS-TEST-AHL-PEMHDR.
022212     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
022212     05  F                       PIC X(11) VALUE "&GROUPNAME=".
022212     05  F                       PIC X(14) VALUE 'BatchAHLClaims'.
022212     05  F                       PIC X(7)  VALUE '&UNAME='.
022212     05  F                       PIC X(8)  VALUE 'batchjob'.
022212     05  F                       PIC X(11) VALUE '&UPASSWORD='.
022212     05  F                       PIC X(8)  VALUE 'batchjob'.

061421 01  WS-TEST-FNL-PEMHDR.
061421     05  F                       PIC X(7)  VALUE "SERVER=".
061421     05  F                       PIC X(20) VALUE
061421                          'hov-nsft02.cso.local'.
061421     05  F                       PIC X(11) VALUE '&GROUPNAME='.
061421     05  F                       PIC X(14) VALUE 'BatchFNLClaims'.
061421     05  F                       PIC X(7)  VALUE '&UNAME='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.
061421     05  F                       PIC X(11) VALUE '&UPASSWORD='.
061421     05  F                       PIC X(8)  VALUE 'batchjob'.

      ******************************************
      * symbol list text for PEMFTR template
      ******************************************

061421 01  ws-sl-var-length pic s9(8) comp value +33.
       01  WS-VAR-SLUNIKIX.
           05  FILLER                  PIC X(09) VALUE "HOSTINFO=".
           05  WS-SL-HOST-INFO         PIC X(09) VALUE 'slunikix:'.
           05  WS-SL-PORT              PIC XXXX  VALUE '7001'.
061421     05  WS-SL-REST              PIC X(200) VALUE SPACES.

061421 01  ws-lt-var-length pic s9(8) comp value +34.
       01  WS-VAR-LOGICTEST.
           05  FILLER                  PIC X(09) VALUE "HOSTINFO=".
           05  WS-LT-HOST-INFO         PIC X(10) VALUE 'logictest:'.
           05  WS-LT-PORT              PIC XXXX  VALUE '6002'.
061421     05  WS-LT-REST              PIC X(200) VALUE SPACES.

061421 01  finished-string.
061421     05  FILLER                   PIC X(11)  VALUE "&URLVARLST=".
061421     05  var-string               pic x(200) value spaces.

       01 WS-TEST-TESTER.
          05  FILLER                   PIC X(11)  VALUE "&URLVARLST=".
          05  WS-KEY.
              10  OT-CARRIER           PIC X.
              10  OT-CLMNO             PIC X(7).
              10  OT-CRTNO             PIC X(11).
              10  OT-LETTER-ID         PIC X(4).
              10  OT-FOLLOW-UP-DT      PIC X(10).
              10  OT-RESEND-DT         PIC X(10).
              10  OT-NO-OF-COPIES      PIC 99.
              10  OT-PROC-ID           PIC XXXX.
              10  OT-COMP-ID           PIC XXX.
              10  OT-PRINT-NOW-SW      PIC X.
              10  OT-ENC-CD            PIC XXX.
              10  OT-ARCHIVE-NO        PIC 9(08).
              10  OT-REGARDING         PIC X(70).


       01  WS-ELMSTR-KEY.
           05  WS-ELMSTR-COMPANY-CD    PIC X.
           05  WS-ELMSTR-CARRIER       PIC X.
           05  WS-ELMSTR-CLAIM-NO      PIC X(7).
           05  WS-ELMSTR-CERT-NO       PIC X(11).

                                       COPY ELCMSTR.

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

      *****************************************
      * symbol list for the PEMBOD template
      *****************************************
      
                                       COPY NSCVARS.

01168  LINKAGE SECTION.                                                 

       01  var  pic x(30).

       procedure division.

      *********************
      * Receive web input
      *********************

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
PEMTST*       DISPLAY '  KIXSYS = ' var (1:env-var-len)
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
PEMTST*       DISPLAY ' WS KIX SYS ' WS-KIXSYS
PEMTST*       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if

           exec cics web 
              startbr formfield resp(w-resp) 
            end-exec.
      	         
            perform read-form thru read-form-exit
               until w-resp not = dfhresp(normal).
      	             
            exec cics web
              endbr formfield
            end-exec.
		
           MOVE IFF-CARRIER             TO OT-CARRIER         
           MOVE IFF-CLAIM-NO            TO OT-CLMNO           
           MOVE IFF-CERT-NO             TO OT-CRTNO           
           MOVE IFF-LETTER-ID           TO OT-LETTER-ID       
           MOVE IFF-FOLLOW-UP-DT        TO OT-FOLLOW-UP-DT    
           MOVE IFF-RESEND-DT           TO OT-RESEND-DT       
           MOVE IFF-NO-OF-COPIES        TO OT-NO-OF-COPIES    
           MOVE IFF-PROC-ID             TO OT-PROC-ID         
           MOVE IFF-COMP-ID             TO OT-COMP-ID         
           MOVE IFF-PRINT-NOW-SW        TO OT-PRINT-NOW-SW    
           MOVE IFF-ENC-CD              TO OT-ENC-CD
           MOVE ZEROS                   TO OT-ARCHIVE-NO
           MOVE IFF-REGARDING           TO OT-REGARDING       

PEMTST*    DISPLAY ' INPUT FORM ' WS-KEY
      *****************************************
      * Invoke the LETTER business logic
      *****************************************

           exec cics link
              program('NSRLTRBL')
              commarea(srch-commarea)
           end-exec.

      *    DISPLAY ' MADE IT BACK FROM NSRLTRBL '
           IF BL-OK
PEMTST*       DISPLAY ' BL OK ' BL-ARCHIVE-NO
              MOVE BL-RECORD-PASSED-DATA
                                       TO NAPER-OUTPUT-DATA
              MOVE BL-ARCHIVE-NO          TO OT-ARCHIVE-NO
           END-IF

061421     move +1 to v2
061421     perform varying v1 from +1 by +1 until v1 > +1
061421        if ot-carrier (v1:1) <> spaces
061421           move ot-carrier(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +7
061421        if ot-clmno (v1:1) <> spaces
061421           move ot-clmno(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +11
061421        if ot-crtno (v1:1) <> spaces
061421           move ot-crtno(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +4
061421        if ot-letter-id (v1:1) <> spaces
061421           move ot-letter-id(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +10
061421        if ot-follow-up-dt (v1:1) <> spaces
061421           move ot-follow-up-dt(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +10
061421        if ot-resend-dt (v1:1) <> spaces
061421           move ot-resend-dt(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +2
061421        if ot-no-of-copies (v1:1) <> spaces
061421           move ot-no-of-copies(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421     
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +4
061421        if ot-proc-id (v1:1) <> spaces
061421           move ot-proc-id(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +3
061421        if ot-comp-id (v1:1) <> spaces
061421           move ot-comp-id(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +1
061421        if ot-print-now-sw (v1:1) <> spaces
061421           move ot-print-now-sw(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +3
061421        if ot-enc-cd (v1:1) <> spaces
061421           move ot-enc-cd(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +8
061421        if ot-archive-no (v1:1) <> spaces
061421           move ot-archive-no(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform
061421
061421     perform varying m1 from +70 by -1 until
061421        (m1 < +1)
061421        or (ot-regarding(m1:1) <> spaces)
061421     end-perform
061421
061421     move +1 to a2
061421     if m1 > +0
061421        MOVE SPACES              TO WS-WORK-FIELD
061421        perform varying a1 from +1 by +1 until a1 > m1
061421           if ot-regarding(a1:1) = ' '
061421              move '~'           to ws-work-field(a2:1)
061421              add +1 to a2
061421           else
061421              move ot-regarding(a1:1) to ws-work-field(a2:1)
061421              add +1 to a2
061421           end-if
061421        end-perform
061421     end-if
061421
061421     move ws-work-field to ot-regarding
061421
061421     move '~~' to var-string(v2:2)
061421     add +2 to v2
061421     perform varying v1 from +1 by +1 until v1 > +70
061421        if ot-regarding (v1:1) <> spaces and '&'
061421           move ot-regarding(v1:1) to var-string(v2:1)
061421           add +1 to v2
061421        end-if
061421     end-perform

061421     move finished-string        to ws-lt-rest
           evaluate ws-kix-myenv
              when 'cid1p'
061421           move finished-string  to ws-sl-rest
                 move '7001'           to ws-sl-port
              when 'mdoff'
061421           move finished-string  to ws-sl-rest
                 move '7003'           to ws-sl-port
              when 'paul'
                 move '5002'           to ws-lt-port
022212        when 'tony'
022212           move '6003'           to ws-lt-port
022212        when 'ahltst'
022212           move '6007'           to ws-lt-port
              when other
                 move '6002'           to ws-lt-port
           end-evaluate

      *    MOVE X'04'                  TO WS-ELMSTR-COMPANY-CD
      *    MOVE IFF-CARRIER            TO WS-ELMSTR-CARRIER
      *    MOVE IFF-CLAIM-NO           TO WS-ELMSTR-CLAIM-NO
      *    MOVE IFF-CERT-NO            TO WS-ELMSTR-CERT-NO
      *
02761 *    EXEC CICS READ                                               
02762 *         DATASET    ('ELMSTR') 
02763 *         INTO       (CLAIM-MASTER)                    
02764 *         RIDFLD     (WS-ELMSTR-KEY)                                   
      *         RESP       (WS-RESPONSE)
02766 *    END-EXEC.                                                    
      *
      *    IF RESP-NORMAL
      *       MOVE IFF-LETTER-ID       TO OUT-TEMP
      *                                   OUT-LETTER
      *       MOVE CL-CARRIER          TO OUT-CARR
      *       MOVE CL-INSURED-LAST-NAME
      *                                TO OUT-ILNAME
      *       MOVE CL-INSURED-1ST-NAME TO OUT-IFNAME
      *       MOVE CL-CLAIM-NO         TO OUT-CLMNO
      *       MOVE CL-CERT-NO          TO OUT-CRTNO
      *       MOVE CL-CERT-ACCOUNT     TO OUT-ACTNO
      *    END-IF
02767                                                                   
      *    exec cics link program('NSRLTRBL')
      *       commarea(srch-commarea)
      *    end-exec.

      ***********************************************************
      * Build output document.  There are three templates used
      * for this document.  PEMHDR and PEMFTR are the header
      * and footer, respectively.  PEMBOD is used for the
      * actual data.  For each array entry in the business
      * logic output, set the symbol list from the array
      * entry and insert into the document using the PEMBOD
      * template.
      ***********************************************************

      *    move bl-output-message to out-msg-text.
	   
           evaluate true
              when iff-comp-id = 'DCC'
                 IF WS-KIX-MYENV = 'cid1p'
                    exec cics document create
                       doctoken   (w-doctoken)
                       template   ('PEMHDR')
                       symbollist (WS-PROD-DCC-PEMHDR)
                       resp       (WS-RESPONSE)
                    end-exec
                 ELSE
                    exec cics document create
                       doctoken   (w-doctoken)
                       template   ('PEMHDR')
                       symbollist (WS-TEST-DCC-PEMHDR)
                       resp       (WS-RESPONSE)
                    end-exec
                 END-IF
022212        when IFF-COMP-ID = 'AHL'
022212           IF WS-KIX-MYENV = 'cid1p'
022212              exec cics document create
022212                 doctoken   (w-doctoken)
022212                 template   ('PEMHDR')
022212                 symbollist (WS-PROD-AHL-PEMHDR)
022212                 resp       (WS-RESPONSE)
022212              end-exec
022212           ELSE
022212              exec cics document create
022212                 doctoken   (w-doctoken)
022212                 template   ('PEMHDR')
022212                 symbollist (WS-TEST-AHL-PEMHDR)
022212                 resp       (WS-RESPONSE)
022212              end-exec
022212           END-IF
020816        when IFF-COMP-ID = 'VPP'
020816           IF WS-KIX-MYENV = 'cid1p'
020816              exec cics document create
020816                 doctoken   (w-doctoken)
020816                 template   ('PEMHDR')
020816                 symbollist (WS-PROD-VPP-PEMHDR)
020816                 resp       (WS-RESPONSE)
020816              end-exec
020816           ELSE
020816              exec cics document create
020816                 doctoken   (w-doctoken)
020816                 template   ('PEMHDR')
020816                 symbollist (WS-TEST-VPP-PEMHDR)
020816                 resp       (WS-RESPONSE)
020816              end-exec
020816           END-IF
061421        when IFF-COMP-ID = 'FNL'
061421           IF WS-KIX-MYENV = 'cid1p'
061421              exec cics document create
061421                 doctoken   (w-doctoken)
061421                 template   ('PEMHDR')
061421                 symbollist (WS-PROD-FNL-PEMHDR)
061421                 resp       (WS-RESPONSE)
061421              end-exec
061421           ELSE
061421              exec cics document create
061421                 doctoken   (w-doctoken)
061421                 template   ('PEMHDR')
061421                 symbollist (WS-TEST-FNL-PEMHDR)
061421                 resp       (WS-RESPONSE)
061421              end-exec
061421           END-IF
              when other
                 IF WS-KIX-MYENV = 'cid1p'
                    exec cics document create
                       doctoken   (w-doctoken)
                       template   ('PEMHDR')
                       symbollist (WS-PROD-CID-PEMHDR)
                       resp       (WS-RESPONSE)
                    end-exec
                 ELSE
                    exec cics document create
                       doctoken   (w-doctoken)
                       template   ('PEMHDR')
                       symbollist (WS-TEST-CID-PEMHDR)
                       resp       (WS-RESPONSE)
                    end-exec
                 END-IF
           end-evaluate

      *    DISPLAY ' PEMHDR DOC CREATE ' WS-RESPONSE

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

      *    MOVE 'CICM'                 TO OUT-TEMP
      *    MOVE 'CICM'                 TO OUT-LETTER
      *    MOVE '9'                    TO OUT-CARR
      *    MOVE 'MCDANIEL'             TO OUT-ILNAME
      *    MOVE 'KATHI'                TO OUT-IFNAME
      *    MOVE '0151216'              TO OUT-CLMNO
      *    MOVE '0009235906 '          TO OUT-CRTNO
      *    MOVE '0990000208'           TO OUT-ACTNO

           exec cics document set
              doctoken(w-doctoken)
              symbollist(NAPER-OUTPUT-DATA)
              length(length of NAPER-OUTPUT-DATA)
		          resp   (WS-RESPONSE)
           end-exec
      
           exec cics document insert
              doctoken(w-doctoken)
              template('PEMBOD')
		          resp   (WS-RESPONSE)
           end-exec

           INSPECT OT-REGARDING REPLACING ALL '&' BY ' '

           if ws-kix-myenv = 'cid1p' or 'mdoff'
061421        compute ws-sl-var-length =
061421           (ws-sl-var-length + v2) - +1
              exec cics document set
                 doctoken(w-doctoken)
                 symbollist(WS-VAR-SLUNIKIX)
061421           length(ws-sl-var-length)
		             resp   (WS-RESPONSE)
              end-exec
           else
061421        compute ws-lt-var-length =
061421           (ws-lt-var-length + v2) - +1
              exec cics document set
                 doctoken(w-doctoken)
                 symbollist(WS-VAR-LOGICTEST)
061421           length(ws-lt-var-length)
		             resp   (WS-RESPONSE)
              end-exec
           end-if      

           exec cics document insert
              doctoken(w-doctoken)
              template('PEMFTR')
		          resp   (WS-RESPONSE)
           end-exec

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
                    when 'carrier'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
			                                 to IFF-CARRIER
			                                    BL-CARRIER
                       else
		                      move spaces to IFF-CARRIER
                       end-if
                    when 'claim_no'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
			                                 to IFF-CLAIM-NO
			                                    BL-CLAIM-NO
                       else
		                      move spaces  to IFF-CLAIM-NO
                       end-if
                    when 'cert_no'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-CERT-NO
                                          BL-CERT-NO
                       else
		                      move spaces  to IFF-CERT-NO
                       end-if
                    when 'letter_id'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-LETTER-ID
                                          BL-LETTER-ID
                       else
                          move spaces  to IFF-LETTER-ID
                       end-if
                    when 'fu_date'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-FOLLOW-UP-DT
                                          BL-FOLLOW-UP-DT
                       else
		                      move spaces  to IFF-FOLLOW-UP-DT
                       end-if
                    when 'rs_date'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-RESEND-DT
                                          BL-RESEND-DT
                       else
		                      move spaces  to IFF-RESEND-DT
                       end-if
                    when 'no_copies'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-NO-OF-COPIES
                                          BL-NO-OF-COPIES
                       else
                          move zeros   to IFF-NO-OF-COPIES
                       end-if
                    when 'proc_id'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-PROC-ID
                                          BL-PROC-ID
                       else
                          move 'KMSB'  to IFF-PROC-ID
                       end-if
                    when 'comp_id'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-COMP-ID
                                          BL-COMP-ID
                       else
                          move 'CID'   to IFF-COMP-ID
                       end-if
                    when 'prt_now'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-PRINT-NOW-SW
                                          BL-PRINT-NOW-SW
                       else
                          move 'N'     to IFF-PRINT-NOW-SW
                       end-if
                    when 'enc_cd'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-ENC-CD
                                          BL-ENC-CD
                       else
                          move '0'     to IFF-ENC-CD
                       end-if
                    when 'regarding'
		                   if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to IFF-REGARDING
                                          BL-REGARDING
                       else
                          move SPACES  to IFF-REGARDING BL-REGARDING
                       end-if
                 end-evaluate
              when other
                 continue
           end-evaluate.
       read-form-exit.		
					
				        
