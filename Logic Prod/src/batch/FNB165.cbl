      *TITLE 'CID PAYMENTS AND ADJUSTMENTS INTERFACE'                   00010031
                                                                        00020030
       IDENTIFICATION DIVISION.                                         00030030
       PROGRAM-ID.    FNB165.                                           00040030
      *AUTHOR         DAN DRYDEN.                                       00050030
      *DATE-WRITTEN   MAY, 1998.                                        00060030
                                                                        00070030
      ***************************************************************** 00080030
      *                         H I S T O R Y                         * 00090030
      ***************************************************************** 00100030
      * NAME  DATE      DESCRIPTION                                   * 00110030
      * ----  --------  --------------------------------------------- * 00120030
      * DANA  01/01/99  CR#199801150013 - FREEDOM SYSTEM INSTALL      * 00130030
      * DANA  02/24/99  COMPILED FOR LOGIC Y2K COPYBOOK               * 00131038
      * DANA  09/01/99  CR#1999072000004 - NEW FORMAT FOR SYS020      * 00132039
      * DANA  02/16/01  CR#2001011600002 - REMOVE LOGIC ACCT MASTER   * 00133041
061501* DANA  06/22/01  CR#1999061000004 - REMOVE MSA CONVERSION FILE * 00134043
030702* SMVA  03/07/02  CR#2002021300008  ELIMINATE PLAN CODE TABLE
      *                                 DEPENDENCIES
103002* PEMA 10/30/2002                  ADD PROCESSING FOR DCC
010803* PEMA 01/08/2003                  SEPARATE DCC FROM CID
060503* SMVA 06/05/2003 IR 2003060500003 CHECK FOR NULLS IN GL-ACCOUNT;
060503*                                  SKEWED REC LNGTH IN INTFCE FILE
122205* 122205    2005033100001  PEMA  ADD PROCESSING FOR CSI
101708* 101708    2008050500001  AJRA  ADD PROCESSING FOR CCC
022812* 022812    2011110200001  AJRA  ADD PROCESSING FOR AHL
081414* 081414    2014012300001  PEMA  ADD PROCESSING FOR CARRIER 7 DCC
011116* 011116  CR2015082400003  PEMA  ADD CARRIER 9 PROCESSING
040616* 040616  CR2016032400001  PEMA  MORE CARRIER 7 CHANGES
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
111616* 111616  CR2016092200001  PEMA  Remove CSI processing
062321* 063021  CR2021021600001  TANA  ADD FNL COMPANY CODE
      ***************************************************************** 00150030
                                                                        00160000
       ENVIRONMENT DIVISION.                                            00170000
       INPUT-OUTPUT SECTION.                                            00180000
       FILE-CONTROL.                                                    00190000
                                                                        00200000
           SELECT CID-PAYMENTS                                          00210000
               ASSIGN TO SYS010                                         00220000
               ORGANIZATION IS INDEXED                                  00230002
               ACCESS IS SEQUENTIAL                                     00240002
               RECORD KEY IS PY-CONTROL-PRIMARY                         00250002
               FILE STATUS IS SYS010-STATUS.                            00260000
                                                                        00270000
           SELECT WORK-FILE                                             00280000
               ASSIGN TO SYS011
               organization is line sequential.

101708     SELECT CCC-WORK-FILE
101708         ASSIGN TO SYS013
101708         ORGANIZATION IS LINE SEQUENTIAL.
101708
103002     SELECT DISK-DATE        ASSIGN TO SYS019.
                                                                        00450000
       DATA DIVISION.                                                   00460000
       FILE SECTION.                                                    00470000
                                                                        00480000
       FD  CID-PAYMENTS.                                                00490017
           COPY ERCPYAJ.                                                00500010
                                                                        00510000
       FD  WORK-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  WORK-RECORD.
           COPY FNC022.

101708 FD  CCC-WORK-FILE
101708     LABEL RECORDS ARE STANDARD
101708     RECORDING MODE IS F
101708     BLOCK CONTAINS 0 RECORDS.
101708 01  CCC-WORK-RECORD             PIC X(250).
101708
103002 FD  DISK-DATE                                                    
103002     COPY ELCDTEFD.                                               
103002                                                                  
                                                                        00700000
       WORKING-STORAGE SECTION.                                         00710000
                                                                        00720000
       01  FILLER.                                                      00730000
103002     05  WS-ZERO                 PIC S9(3)  COMP-3  VALUE +0.
103002     05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.    
103002     05  PGM-SUB                 PIC S9(4) COMP VALUE +310.
103002     05  WS-RETURN-CODE          PIC S999  COMP-3 VALUE +0.
           05  DUMP                PIC X         VALUE ' '.             00740000
           05  FORCE-DUMP REDEFINES DUMP PIC S9 COMP-3.                 00750000
           05  SYS010-STATUS       PIC XX        VALUE '00'.            00780029
               88  EOF                           VALUE '10'.            00790000
           05  WS-PREV-DATE        PIC XX.                              00800010
           05  WS-CURR-DATE        PIC XX.                              00810010
           05  WS-SUSPENSE.                                             00820026
               10  WS-FIN-RESP     PIC X(7).                            00830027
               10  WS-CID-ACCT     PIC X(8).                            00840027
                                                                        00850026
       COPY ELCDATE.


030702 01  SYSTEM-DATE.
030702     05  SYS-MO         PIC 9(2).
030702     05  SYS-DA         PIC 9(2).
030702     05  SYS-CCYY       PIC 9(4).


030702* FUNCTION-DATE COPYBOOK
030702                                      COPY ELCFUNDT.

030702* STATE EDIT TABLE
030702 COPY FNC018.

103002     COPY ELCDTECX.                                               
103002     COPY ELCDTEVR.                                               

       LINKAGE SECTION.                                                 00950032
                                                                        00960032
       01  PARM.                                                        00970032
           05  PARM-LENGTH      PIC S9(4)  COMP.                        00980032
           05  PARM-PREV-DATE   PIC X(8).                               00990032
           05  FILLER           PIC X.                                  01000032
           05  PARM-CURR-DATE   PIC X(8).                               01010032
                                                                        01020032
                                                                        01030032
                                                                        01040032
      *                                                                 01070032
       PROCEDURE DIVISION USING PARM.                                   01080032
      *                                                                 01090000
103002*************************************************************     
103002                                 COPY ELCDTERX.                   
103002*************************************************************     
           PERFORM 0000-HOUSEKEEPING THRU 0000-EXIT                     01100000
                                                                        01110000
           PERFORM 1000-PROCESS      THRU 1000-EXIT
               UNTIL EOF.                                               01120000
                                                                        01130000
           CLOSE WORK-FILE CID-PAYMENTS

122205     IF DTE-CLIENT = 'DCC'
111616        CLOSE CCC-WORK-FILE
122205     END-IF

030702     GOBACK.                                                      01140000
                                                                        01150000
                                                                        01160000
       1000-PROCESS.                                                    01190000
      *                                                                 01200000
           READ CID-PAYMENTS                                            01210000
               AT END GO TO 1000-EXIT.                                  01220000
010803
010803     IF PY-COMPANY-CD < DTE-CLASIC-COMPANY-CD
010803        GO TO 1000-EXIT
010803     ELSE
010803        IF PY-COMPANY-CD > DTE-CLASIC-COMPANY-CD
010803           SET EOF               TO TRUE
010803           GO TO 1000-EXIT
010803        END-IF
010803     END-IF    

           IF (PY-RECORD-ID = 'PY')          AND                        01240010
103002*       (PY-CARRIER = '6' OR '9')      AND                        01250010
              (PY-RECORD-TYPE = 'R' OR 'C')                             01260010
                CONTINUE                                                01270010
           ELSE                                                         01280010
                GO TO 1000-EXIT.                                        01290010
                                                                        01300010
           IF (PY-INPUT-DT > WS-PREV-DATE)                              01310011
             AND (PY-INPUT-DT NOT > WS-CURR-DATE)                       01320011
               CONTINUE                                                 01330002
           ELSE                                                         01340002
               GO TO 1000-EXIT.                                         01350002
                                                                        01360002
PEMTST*    DISPLAY ' PY CARRIER = ' PY-CARRIER
PEMTST*    MOVE '3'                    TO PY-CARRIER

           MOVE SPACES      TO WORK-RECORD                              01370015
                                                                        01380011
103002     MOVE '10'                   TO FX-TRAN-TYPE
103002     MOVE '98'                   TO FX-SUB-TYPE


122205     EVALUATE TRUE
062321        WHEN DTE-CLIENT = 'FNL'
062321           MOVE 'FNL PYADJ'      TO FX-SYSTEM
062321           MOVE 'CRLOGC'         TO FX-SOURCE-CODE
062321           MOVE 'F1'             TO FX-DIVISION
022812        WHEN DTE-CLIENT = 'AHL'
022812           MOVE 'AHL PYADJ'      TO FX-SYSTEM
022812           MOVE 'CRLOGC'         TO FX-SOURCE-CODE
022812           MOVE '02'             TO FX-DIVISION
122205        WHEN DTE-CLIENT = 'CID'
122205           MOVE 'CID PYADJ'      TO FX-SYSTEM
122205           MOVE 'CRLOGC'         TO FX-SOURCE-CODE
122205           MOVE '02'             TO FX-DIVISION
111616*       WHEN (DTE-CLIENT = 'DCC')
111616*            AND (PY-CARRIER = '3' OR '4')
111616*          MOVE 'CSIDCCPYAJ'     TO FX-SYSTEM
111616*          MOVE 'LOGIC '         TO FX-SOURCE-CODE
111616*          MOVE '11'             TO FX-DIVISION
101708        WHEN (DTE-CLIENT = 'DCC') AND
111616             (PY-CARRIER = '3' OR '4' OR '5' OR '6')
101708           MOVE 'CCCDCCPYAJ'     TO FX-SYSTEM
101708           MOVE 'LOGIC '         TO FX-SOURCE-CODE
101708           MOVE '50'             TO FX-DIVISION
040616        WHEN (DTE-CLIENT = 'DCC') AND
040616             (PY-CARRIER = '7')
040616           MOVE 'CCCDCCPYAJ'     TO FX-SYSTEM
040616           MOVE 'LOGIC '         TO FX-SOURCE-CODE
040616           MOVE '5C'             TO FX-DIVISION
122205        WHEN DTE-CLIENT = 'DCC'  *> default car 1,2,9
122205           MOVE 'LPAC PYADJ'     TO FX-SYSTEM
122205           MOVE 'LOGIC '         TO FX-SOURCE-CODE
122205           MOVE '11'             TO FX-DIVISION
010716        WHEN DTE-CLIENT = 'VPP'
010716           MOVE 'VPA PYADJ'      TO FX-SYSTEM
010716           MOVE 'LOGIC '         TO FX-SOURCE-CODE
010716           MOVE '5V'             TO FX-DIVISION
122205     END-EVALUATE
        
           IF PY-RECORD-TYPE = 'C'                                      01440025
              MULTIPLY PY-ENTRY-AMT BY -1 GIVING FX-AMOUNT              01450025
           ELSE                                                         01460025
              MOVE PY-ENTRY-AMT TO FX-AMOUNT                            01470025
           END-IF                                                       01480025
                                                                        01490018
           MOVE ' '              TO FX-CLM-RES                          01500025
           MOVE ' '              TO FX-ANN-STMT                         01510025
           MOVE ' '              TO FX-PLAN-CODE                        01520025
           MOVE PARM-CURR-DATE   TO FX-POSTING-DATE                     01530032
           MOVE ' '              TO FX-POLICY-NO                        01540025
                                                                        01550029
           MOVE ' '              TO FX-STATE                            01580025
           MOVE 'Y'              TO FX-LOC-CODE                         01590030
           MOVE ' '              TO FX-ZIP-CODE                         01600025

022812     IF DTE-CLIENT = 'CID' OR 'AHL'
103002        MOVE 'S'                 TO FX-FY-REN
103002     ELSE
103002        MOVE ' '                 TO FX-FY-REN
103002     END-IF

           MOVE ' '               TO FX-AGENT-01    
           MOVE ' '               TO FX-DISTR      
                                                                        01640016
060503     IF PY-GL-ACCOUNT = LOW-VALUES
060503         MOVE SPACES        TO FX-SOURCE-ACCT
060503     ELSE
061501         MOVE PY-GL-ACCOUNT TO FX-SOURCE-ACCT
060503     END-IF
           MOVE PY-CARRIER        TO FX-DESCRIPTION(27:1)     
           MOVE PY-GROUPING(4:3)  TO FX-DESCRIPTION(28:3)    
           MOVE ' '               TO FX-REFERENCE           
           MOVE PY-FIN-RESP(4:7)  TO WS-FIN-RESP
                                                           
           MOVE PY-ACCOUNT(3:8)   TO WS-CID-ACCT          
012099     IF WS-CID-ACCT = LOW-VALUE                    
012099        MOVE SPACES         TO WS-CID-ACCT        
012099     END-IF
                                                       
           MOVE WS-SUSPENSE       TO FX-SUSPENSE      
           MOVE PY-FIN-RESP       TO FX-SUSPENSE
           IF PY-ACCOUNT = LOW-VALUES
              MOVE SPACES         TO FX-REFERENCE
           ELSE
              MOVE PY-ACCOUNT     TO FX-REFERENCE
           END-IF

030702**** THE PLAN CODE TABLE IS NO LONGER ACCESSED WITHIN THE CID
030702**** LOGIC SYSTEM; THEREFORE THE FOLLOWING CALL IS NOT REQUIRED
030702**** THIS CALL NEVER CAME BACK WITH ANYTHING - NO PLAN CODE KEY   01800026
030702*    CALL 'FNB160' USING WORK-RECORD

030702**** SEARCH PULLED FROM FNB160
030702     IF FX-STATE NOT= SPACES
030702         SEARCH ALL STATE-TABLE
030702             AT END DISPLAY 'INVALID STATE CODE: ' FX-STATE
030702             WHEN ST-STATE (ST-INDEX) = FX-STATE
030702             MOVE ST-ALT-STATE (ST-INDEX) TO FX-STATE
030702         END-SEARCH
030702     END-IF

030702     MOVE SYSTEM-DATE              TO FX-JOURNAL-DATE

030702**** THE '*' IN POSITION 250 ENSURES A 250 BYTE RECORD IS PASSED
030702**** TO FREEDOM - PREVENTS TRUNCATION OF BLANK FIELDS
030702     MOVE '*'                      TO WORK-RECORD(250:1)

101708     IF (DTE-CLIENT = 'DCC') AND
111616        (PY-CARRIER = '3' or '4' or '5' OR '6' or '7')
101708         WRITE CCC-WORK-RECORD FROM WORK-RECORD
101708     ELSE  *> default carrier 1, 2, 9
122205        WRITE WORK-RECORD
122205     END-IF


           .
       1000-EXIT.                                                       01840000
           EXIT.                                                        01850000
                                                                        01860029
                                                                        01870029
       0000-HOUSEKEEPING.                                               02260008
                                                                        02270008
           MOVE PARM-PREV-DATE(1:2) TO DC-GREG-DATE-1-MDY(1:2)          02280032
           MOVE PARM-PREV-DATE(3:2) TO DC-GREG-DATE-1-MDY(3:2)          02290032
           MOVE PARM-PREV-DATE(7:2) TO DC-GREG-DATE-1-MDY(5:2)          02300032
           MOVE '4' TO DC-OPTION-CODE                                   02310032
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA                    02320012
           IF DC-ERROR-CODE = ' '                                       02330010
               MOVE DC-BIN-DATE-1 TO WS-PREV-DATE                       02340010
           ELSE                                                         02350010
               DISPLAY 'PARAMETER PREVIOUS DATE IS INVALID: '           02360033
                        PARM-PREV-DATE                                  02370033
               ADD +1 TO FORCE-DUMP.                                    02380010
                                                                        02390010
           MOVE PARM-CURR-DATE(1:2) TO DC-GREG-DATE-1-MDY(1:2)          02400032
           MOVE PARM-CURR-DATE(3:2) TO DC-GREG-DATE-1-MDY(3:2)          02410032
           MOVE PARM-CURR-DATE(7:2) TO DC-GREG-DATE-1-MDY(5:2)          02420032
           MOVE '4' TO DC-OPTION-CODE                                   02430032
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA                    02440032
           IF DC-ERROR-CODE = ' '                                       02450032
               MOVE DC-BIN-DATE-1 TO WS-CURR-DATE                       02460032
           ELSE                                                         02470032
               DISPLAY 'PARAMETER CURRENT DATE IS INVALID: '            02480033
                        PARM-CURR-DATE                                  02490033
               ADD +1 TO FORCE-DUMP.                                    02500032
                                                                        02510032
           IF WS-PREV-DATE > WS-CURR-DATE                               02520010
               DISPLAY 'INVALID PARAMETER: PREVIOUS DATE > CURRENT DATE'02530033
               DISPLAY 'PREVIOUS DATE = ' PARM-PREV-DATE                02540033
               DISPLAY ' CURRENT DATE = ' PARM-CURR-DATE                02550033
               ADD +1 TO FORCE-DUMP.                                    02560010
                                                                        02570015
030702     MOVE FUNCTION CURRENT-DATE
030702                                 TO FUNCTION-DATE
030702     MOVE WS-FN-MO               TO SYS-MO
030702     MOVE WS-FN-DA               TO SYS-DA
030702     MOVE WS-FN-CCYR             TO SYS-CCYY

           OPEN  INPUT CID-PAYMENTS                                     02720016
                OUTPUT WORK-FILE.                                       02730016
                                                                        02740010
122205     IF DTE-CLIENT = 'DCC'
111616        OPEN OUTPUT CCC-WORK-FILE
122205     END-IF

           .
       0000-EXIT.                                                       02750010
           EXIT.                                                        02760010
103002 ABEND-PGM SECTION.                                               00026280
           DISPLAY 'REACHED ABEND PGM '
103002     DIVIDE WS-ZERO BY WS-ZERO GIVING WS-ZERO.                    00026290
103002 ABEND-EXIT.                                                      00026300
                                                                        02770009
