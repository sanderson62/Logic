       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMPMC1.
      *AUTHOR.     PABLO.
      *REMARKS.
      * THIS PROGRAM READS ERPNDM BACKUPS AND CONVERS THE OLD FORMAT
      * TO THE NEW FORMAT. THE NEXT STEP SHOULD BE LOADING THE OUTPUT
      * OF THIS PROGRAM TO A VSAM FILE.       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ERPNDM-IN        ASSIGN TO SYS010.
           
           SELECT ERPNDM-OUT       ASSIGN TO SYS011.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE                   COPY ELCDTEFD.

       FD  ERPNDM-IN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.

       01  BU-PENDING-MAILING-DATA.                                        
           12  BU-RECORD-ID                      PIC XX.                
               88  VALID-PM-ID                       VALUE 'PM'.        
                                                                        
           12  BU-CONTROL-PRIMARY.                                      
               16  BU-COMPANY-CD                 PIC X.                 
               16  BU-ENTRY-BATCH                PIC X(6).              
               16  BU-BATCH-SEQ-NO               PIC S9(4)     COMP.    
               16  BU-BATCH-CHG-SEQ-NO           PIC S9(4)     COMP.    
                                                                        
           12  FILLER                            PIC X(14).             
                                                                        
           12  BU-ACCESS-CONTROL.                                       
               16  BU-SOURCE-SYSTEM              PIC XX.                
                   88  BU-FROM-CREDIT                VALUE 'CR'.        
                   88  BU-FROM-VSI                   VALUE 'VS'.        
                   88  BU-FROM-WARRANTY              VALUE 'WA'.        
                   88  BU-FROM-OTHER                 VALUE 'OT'.        
               16  BU-RECORD-ADD-DT              PIC XX.                
               16  BU-RECORD-ADDED-BY            PIC XXXX.              
               16  BU-LAST-MAINT-DT              PIC XX.                
               16  BU-LAST-MAINT-BY              PIC XXXX.              
               16  BU-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
                                                                        
           12  BU-PROFILE-INFO.                                         
               16  BU-QUALIFY-CODE-1             PIC XX.                
               16  BU-QUALIFY-CODE-2             PIC XX.                
               16  BU-QUALIFY-CODE-3             PIC XX.                
               16  BU-QUALIFY-CODE-4             PIC XX.                
               16  BU-QUALIFY-CODE-5             PIC XX.                
                                                                        
               16  BU-INSURED-LAST-NAME          PIC X(15).             
               16  BU-INSURED-FIRST-NAME         PIC X(10).             
               16  BU-INSURED-MIDDLE-INIT        PIC X.                 
               16  BU-INSURED-ISSUE-AGE          PIC 99.                
               16  BU-INSURED-BIRTH-DT           PIC XX.                
               16  BU-INSURED-SEX                PIC X.                 
                   88  BU-SEX-MALE                   VALUE 'M'.         
                   88  BU-SEX-FEMALE                 VALUE 'F'.         
               16  BU-INSURED-SOC-SEC-NO         PIC X(11).             
                                                                        
               16  BU-ADDRESS-CORRECTED          PIC X.
               16  BU-JOINT-BIRTH-DT             PIC XX.
               16  FILLER                        PIC X(12).             
                                                                        
               16  BU-ADDRESS-LINE-1             PIC X(30).             
               16  BU-ADDRESS-LINE-2             PIC X(30).             
               16  BU-CITY-STATE                 PIC X(30).             
               16  BU-ZIP.                                              
                   20  BU-ZIP-CODE.                                     
                       24  BU-ZIP-1              PIC X.                 
                           88  BU-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
                       24  FILLER                PIC X(4).              
                   20  BU-ZIP-PLUS4              PIC X(4).              
               16  BU-CANADIAN-ZIP  REDEFINES  BU-ZIP.                  
                   20  BU-CAN-POST1              PIC XXX.               
                   20  BU-CAN-POST2              PIC XXX.               
                   20  FILLER                    PIC XXX.               
                                                                        
               16  BU-PHONE-NO                   PIC 9(11)       COMP-3.
                                                                        
      *        16  FILLER                        PIC X(10).             
               16  FILLER                        PIC X(03).             
                                                                        
      *    12  FILLER                            PIC X(23).             
           12  BU-CRED-BENE-ADDR2                PIC X(30).
           12  BU-CRED-BENE-NAME                 PIC X(25).
           12  BU-CRED-BENE-ADDR                 PIC X(30).
           12  BU-CRED-BENE-CTYST                PIC X(30).
           12  BU-CRED-BENE-ZIP.                                        
               16  BU-CB-ZIP-CODE.                                      
                   20  BU-CB-ZIP-1               PIC X.                 
                       88  BU-CB-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'. 
                   20  FILLER                    PIC X(4).              
               16  BU-CB-ZIP-PLUS4               PIC X(4).              
           12  BU-CB-CANADIAN-ZIP  REDEFINES  BU-CRED-BENE-ZIP.         
               16  BU-CB-CAN-POST1               PIC XXX.               
               16  BU-CB-CAN-POST2               PIC XXX.               
               16  FILLER                        PIC XXX.               
           12  BU-POST-CARD-MAIL-DATA.
               16  BU-MAIL-DATA OCCURS 7.
                   20  BU-MAIL-TYPE              PIC X.
                       88  BU-12MO-MAILING           VALUE '1'.
                       88  BU-EXP-MAILING            VALUE '2'.
                   20  BU-MAIL-STATUS            PIC X.
                       88  BU-MAIL-ST-MAILED         VALUE '1'.
                       88  BU-MAIL-ST-RETURNED       VALUE '2'.
                       88  BU-MAIL-ST-NOT-MAILED     VALUE '3'.
                   20  BU-MAIL-DATE              PIC XX.
           12  FILLER                            PIC XX.
      *    12  FILLER                            PIC X(30).             
                                                                        
      ******************************************************************

       FD  ERPNDM-OUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.

       01  ERPNDM-OUT-RECORD             PIC X(374).


       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  PGM-SUB                     PIC S999  COMP   VALUE +511.    
       77  WS-ABEND-FILE-STATUS        PIC XX            VALUE ZERO.    
       77  WS-ABEND-MESSAGE            PIC X(80)         VALUE SPACES.  
       77  WS-ABEND-PROGRAM            PIC X(8)          VALUE SPACES.  
       77  WS-RETURN-CODE              PIC S9(4)         VALUE +0.      
       77  WS-ZERO                     PIC S9     COMP-3 VALUE +0.      
       77  IN-CNT                 PIC 9999999   VALUE ZEROS.
       77  S1                     PIC S999      VALUE +0 COMP-3.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  WS-CURRENT-BIN-DATE         PIC XX VALUE LOW-VALUES.
       77  WS-BIN-COMPARE-DT           PIC XX VALUE LOW-VALUES.
       77  WS-DIS-EFF-DT               PIC X(10) VALUE SPACES.
       01  W-MISC.
           05  WORK-DATE-X.
               10  WORK-MO-X       PIC XX.
               10  WORK-DA-X       PIC XX.
               10  WORK-YR-X       PIC XX.
           05  WORK-DATE-N    REDEFINES   WORK-DATE-X.
               10  WORK-MO-N       PIC 99.
               10  WORK-DA-N       PIC 99.
               10  WORK-YR-N       PIC 99.
           05  WS-WORK-DT             PIC 9(11).
           05  WS-WORK-DT-A REDEFINES WS-WORK-DT.
               10  FILLER             PIC XXX.
               10  V-ISS-CCYR         PIC X(4).
               10  V-ISS-MO           PIC XX.
               10  V-ISS-DA           PIC XX.
           05  WS-WORK-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER            PIC X(5).
               10  WS-WORK-DATE-YR   PIC XX.
               10  WS-WORK-DATE-MO   PIC XX.
               10  WS-WORK-DATE-DA   PIC XX.
           05  CERT-KEY.
               10  CERT-STATE         PIC XX.
               10  CERT-DATE          PIC 9(11).
               10  CERT-CERT          PIC X(11).
           05  WS-DATE-ALPH.
               10  FILLER             PIC XXX VALUE '000'.
               10  WS-WORK-CENT       PIC XX.
               10  WS-WORK-YR         PIC XX.
               10  WS-WORK-MO         PIC XX.
               10  WS-WORK-DA         PIC XX.
           05  WS-DATE-NUM REDEFINES WS-DATE-ALPH
                                      PIC 9(11).

           05  WS-BIN-EFF             PIC XX VALUE LOW-VALUES.
           05  EXTR-IN-CNT            PIC 9(9) VALUE ZEROS.
           05  EXTR-OUT-CNT           PIC 9(9) VALUE ZEROS.
           05  WS-INPUT-SW            PIC X VALUE ' '.
               88  END-OF-INPUT             VALUE 'Y'.

                                       COPY ERCPNDM.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT

           PERFORM 0050-PROCESS        THRU 0050-EXIT UNTIL
              (END-OF-INPUT)
      *       OR (EXTR-IN-CNT > 10)
           PERFORM 0060-CLOSE-FILES    THRU 0060-EXIT

           DISPLAY ' ERPNDM BACKUPS IN    ' EXTR-IN-CNT
           DISPLAY ' ERPNDM BACKUPS OUT   ' EXTR-OUT-CNT

           GOBACK

           .
       0010-OPEN-FILES.
       
           OPEN INPUT ERPNDM-IN
               OUTPUT ERPNDM-OUT

           .
       0010-EXIT.
           EXIT.

       0020-INIT.
       
           DISPLAY ' ACCEPT DATE ' WS-ACCEPT-DATE

           MOVE WS-ACCEPT-DATE         TO DC-GREG-DATE-1-YMD
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           PERFORM 0150-READ-INPUT     THRU 0150-EXIT

           .
       0020-EXIT.
           EXIT.

       0050-PROCESS.

           MOVE SPACES                 TO PENDING-MAILING-DATA
           MOVE 'PM'                   TO PM-RECORD-ID
           MOVE BU-CONTROL-PRIMARY     TO PM-CONTROL-PRIMARY
           MOVE BU-ACCESS-CONTROL      TO PM-ACCESS-CONTROL
           MOVE BU-PROFILE-INFO        TO PM-PROFILE-INFO
           MOVE BU-ADDRESS-LINE-1      TO PM-ADDRESS-LINE-1
           MOVE BU-ADDRESS-LINE-2      TO PM-ADDRESS-LINE-2
           MOVE BU-CITY-STATE          TO PM-CITY-STATE
           MOVE BU-ZIP                 TO PM-ZIP
           MOVE BU-PHONE-NO            TO PM-PHONE-NO

           MOVE BU-CRED-BENE-NAME      TO PM-CRED-BENE-NAME
           MOVE BU-CRED-BENE-ADDR      TO PM-CRED-BENE-ADDR
           MOVE BU-CRED-BENE-ADDR2     TO PM-CRED-BENE-ADDR2
           MOVE BU-CRED-BENE-CTYST     TO PM-CRED-BENE-CTYST
           MOVE BU-CRED-BENE-ZIP       TO PM-CRED-BENE-ZIP
           MOVE BU-POST-CARD-MAIL-DATA TO PM-POST-CARD-MAIL-DATA

           PERFORM 0100-WRITE-OUTPUT   THRU 0100-EXIT

           PERFORM 0150-READ-INPUT     THRU 0150-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-WRITE-OUTPUT.

           WRITE ERPNDM-OUT-RECORD     FROM PENDING-MAILING-DATA
           ADD 1                       TO EXTR-OUT-CNT

           .
       0100-EXIT.
           EXIT.

       0150-READ-INPUT.

           READ ERPNDM-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO EXTR-IN-CNT
           END-IF

           .
       0150-EXIT.
           EXIT.

       0060-CLOSE-FILES.
       
           CLOSE ERPNDM-IN ERPNDM-OUT

           .
       0060-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX'              USING DATE-CONVERSION-DATA.

       8500-EXIT.
           EXIT.

       ABEND-PGM.   COPY ELCABEND.

