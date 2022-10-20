       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   PYRPT.

      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.                                       

      *REMARKS.    EXECUTED FROM /PayAdj/INDEX.HTML

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 111810                   PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.                                            

       DATA DIVISION.                                                   
       working-storage section.
       77  SAVE-DATE                   PIC X(8)    VALUE SPACES.
       77  WS-SAVE-EDIT-A-DATE         PIC X(10)   VALUE SPACES.
       77  SAVE-BIN-DATE               PIC XX      VALUE SPACES.


       01  S1                          PIC S999 VALUE +0 COMP-3.
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

      ************************************************
      * commarea passed to the business logic
      ************************************************
      	  
       01  BL-COMMAREA.
           03  BL-OUTPUT OCCURS 200.
               05  BL-CARRIER          PIC X.
               05  BL-STATE            PIC XX.
               05  BL-FIN-RESP         PIC X(10).
               05  BL-ACCOUNT          PIC X(10).
               05  BL-ACCT-NAME        PIC X(33).
               05  BL-NET-AMT          PIC 9(7)V99.
               05  BL-TYPE             PIC X.
               05  BL-GL-ACCT          PIC X(10).
               05  BL-RETURN-CODE REDEFINES BL-GL-ACCT
                                       PIC 9(10).
               05  BL-COMMENT          PIC X(13).
               05  BL-MAINT-DT         PIC X(10).
           03  BL-COUNT                PIC 999.
           03  BL-STATUS.
               88  BL-OK		          VALUE "P".
               88  BL-FAIL		        VALUE "F".
               05  BL-MESSAGE          PIC X(50).

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
      *  CID      CID1P      BatchClaims     ntcso3            7001
      *  CID      MDOFF      BatchClaims     ntnapersofttst    7003
      *  CID      CID1T      BatchClaims     ntnapersofttst    6002
      *  CID      PAUL       BatchClaims     ntnapersofttst    5002
      *  DCC      CID1P      BatchDCCClaims  ntcso3            7001
      *  DCC      MDOFF      BatchDCCClaims  ntnapersofttst    7003
      *  DCC      CID1T      BatchDCCClaims  ntnapersofttst    6002
      *  DCC      PAUL       BatchDCCClaims  ntnapersofttst    5002

      ******************************************
      * symbol list text for PYHDR template
      ******************************************

       01  WS-PROD-PYHDR. 
           05  F                       PIC X(7)  VALUE "PGMDT=".
           05  PYHDR-DT                PIC X(10) VALUE ' '.

      ******************************************
      * symbol list text for PYBOD template
      ******************************************

       01 WS-PROD-PYBOD.
          05 FILLER              PIC X(8) VALUE "CARRIER=".
          05 OUT-CARR            PIC X.
          05 FILLER              PIC X(7) VALUE "&STATE=".
          05 OUT-STATE           PIC XX.
          05 FILLER              PIC X(6) VALUE "&RESP=".
          05 OUT-RESP            PIC X(10).
          05 FILLER              PIC X(9) VALUE "&ACCOUNT=".
          05 OUT-ACCT            PIC X(10).
          05 FILLER              PIC X(8) VALUE "&ACCTNM=".
          05 OUT-ACCT-NAME       PIC X(33).
          05 FILLER              PIC X(8) VALUE "&NETAMT=".
          05 OUT-AMT             PIC 9999999.99.
          05 FILLER              PIC X(6) VALUE "&TYPE=".
          05 OUT-TYPE            PIC X.
          05 FILLER              PIC X(8) VALUE "&GLACCT=".
          05 OUT-GLACCT          PIC X(10).
          05 FILLER              PIC X(9) VALUE "&COMMENT=".
          05 OUT-COMMENT         PIC X(13).
          05 FILLER              PIC X(9) VALUE "&MAINTDT=".
          05 OUT-MAINT-DT        PIC X(10).

      ******************************************
      * symbol list text for PYFTR template
      ******************************************

       01  WS-PROD-PYFTR. 
           05  F                       PIC X(7)  VALUE "MSG=".
           05  PYFTR-MSG               PIC X(50) VALUE ' '.

       01  WS-VAR-SLUNIKIX.
           05  FILLER                  PIC X(09) VALUE "HOSTINFO=".
           05  WS-SL-HOST-INFO         PIC X(09) VALUE 'slunikix:'.
           05  WS-SL-PORT              PIC XXXX  VALUE '7001'.
           05  WS-SL-REST              PIC X(145) VALUE SPACES.

       01  WS-VAR-LOGICTEST.
           05  FILLER                  PIC X(09) VALUE "HOSTINFO=".
           05  WS-LT-HOST-INFO         PIC X(10) VALUE 'logictest:'.
           05  WS-LT-PORT              PIC XXXX  VALUE '5002'.
           05  WS-LT-REST              PIC X(145) VALUE SPACES.


       01  MISC.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPREC                  VALUE +14.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.

                                       COPY ELCDATE.


01168  LINKAGE SECTION.                                                 

       01  var  pic x(30).

       procedure division.

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


           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-GREG-DATE-A-EDIT    TO PYHDR-DT
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE

      *********************
      * Receive web input
      *********************
      * NO WEB INPUT FROM THIS PROJECT YET

      *****************************************
      * Invoke the report business logic
      *****************************************

           MOVE SPACES                 TO BL-COMMAREA

           exec cics link
              program('PYRPTBL')
              commarea(BL-COMMAREA)
           end-exec.

      ***********************************************************
      * Build output document.  There are three templates used
      * for this document.  PYHDR  and  PYFTR are the header
      * and footer, respectively.   PYBOD is used for the
      * actual data.  For each array entry in the business
      * logic output, set the symbol list from the array
      * entry and insert into the document using the  PYBOD
      * template.
      ***********************************************************

           move bl-MESSAGE        to PYFTR-MSG
	   
           exec cics document create
              doctoken   (w-doctoken)
              template   ('PYHDR')
              symbollist (WS-PROD-PYHDR)
              resp       (WS-RESPONSE)
           end-exec

           move 1 to S1

           perform bl-COUNT times
              MOVE BL-CARRIER   (S1)   TO OUT-CARR         
              MOVE BL-STATE     (S1)   TO OUT-STATE   
              MOVE BL-FIN-RESP  (S1)   TO OUT-RESP    
              MOVE BL-ACCOUNT   (S1)   TO OUT-ACCT   
              MOVE BL-ACCT-NAME (S1)   TO OUT-ACCT-NAME
              MOVE BL-NET-AMT   (S1)   TO OUT-AMT
              MOVE BL-TYPE      (S1)   TO OUT-TYPE
              MOVE BL-GL-ACCT   (S1)   TO OUT-GLACCT
              MOVE BL-COMMENT   (S1)   TO OUT-COMMENT
              MOVE BL-MAINT-DT  (S1)   TO OUT-MAINT-DT

              exec cics document set
                 doctoken   (w-doctoken)
                 symbollist (WS-PROD-PYBOD)
                 length     (length of WS-PROD-PYBOD)
		             resp       (WS-RESPONSE)
              end-exec
      
              exec cics document insert
                 doctoken(w-doctoken)
                 template('PYBOD')
		             resp   (WS-RESPONSE)
              end-exec

              add 1 to S1

           end-perform

           exec cics document set
              doctoken   (w-doctoken)
              symbollist (WS-PROD-PYFTR)
              length     (length of WS-PROD-PYFTR)
		          resp       (WS-RESPONSE)
           end-exec

           exec cics document insert
              doctoken(w-doctoken)
              template('PYFTR')
		          resp   (WS-RESPONSE)
           end-exec

      ****************************************
      * Send the document and return.
      ****************************************
      	   
           exec cics web send
              doctoken(w-doctoken)
		          resp   (WS-RESPONSE)
           end-exec.		

           exec cics
              return
           end-exec.

       9700-DATE-LINK.                                                  

           EXEC CICS LINK                                               
               PROGRAM   ('ELDATCV')
               COMMAREA  (DATE-CONVERSION-DATA)                         
               LENGTH    (DC-COMM-LENGTH)                               
           END-EXEC.                                                    

       9700-EXIT.                                                       
            EXIT.
