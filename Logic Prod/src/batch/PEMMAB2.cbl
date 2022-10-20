       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMMAB2.
      *AUTHOR.     PABLO.
      *REMARKS.
      * THIS PROGRAM READS ERMAIL BACKUPS AND PUTS BACK THE ERMAIL
      * RECORDS THAT ARE MISSING. ALSO, IT WILL CHECK ELCERT TO 
      * INDICATE THAT AN ERMAIL RECORD EXISTS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DISK-DATE        ASSIGN TO SYS019.
           SELECT RPT-FILE         ASSIGN TO SYS008.

           SELECT ERMAIL-IN        ASSIGN TO SYS010.
           
           SELECT ELCERT           ASSIGN TO ELCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CM-CONTROL-PRIMARY
                                   FILE STATUS IS ELCERT-FILE-STATUS.

           SELECT ECCERT           ASSIGN TO ECCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CA-CONTROL-PRIMARY
                                   FILE STATUS IS ECCERT-FILE-STATUS.

           SELECT ERMAIL           ASSIGN TO ERMAIL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS MA-CONTROL-PRIMARY
                                   FILE STATUS IS ERMAIL-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE                   COPY ELCDTEFD.

       FD  RPT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RPT-REC-OUT.

       01  RPT-REC-OUT.
           05  RPT-REC                  PIC X(132).

       FD  ERMAIL-IN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.

       01  BU-MAILING-DATA.                                                
           12  BU-RECORD-ID                      PIC XX.                
               88  VALID-MA-ID                       VALUE 'MA'.        
                                                                        
           12  BU-CONTROL-PRIMARY.                                      
               16  BU-COMPANY-CD                 PIC X.                 
               16  BU-CARRIER                    PIC X.                 
               16  BU-GROUPING.                                         
                   20  BU-GROUPING-PREFIX        PIC XXX.               
                   20  BU-GROUPING-PRIME         PIC XXX.               
               16  BU-STATE                      PIC XX.                
               16  BU-ACCOUNT.                                          
                   20  BU-ACCOUNT-PREFIX         PIC X(4).              
                   20  BU-ACCOUNT-PRIME          PIC X(6).              
               16  BU-CERT-EFF-DT                PIC XX.                
               16  BU-CERT-NO.                                          
                   20  BU-CERT-PRIME             PIC X(10).             
                   20  BU-CERT-SFX               PIC X.                 
                                                                        
           12  FILLER                            PIC XX.                
                                                                        
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
               16  FILLER                        PIC X(14).             
                                                                        
               16  BU-ADDRESS-LINE-1             PIC X(30).             
               16  BU-ADDRESS-LINE-2             PIC X(30).             
               16  BU-CITY-STATE                 PIC X(30).             
               16  BU-ZIP.                                              
                   20  BU-ZIP-CODE.                                     
                       24  BU-ZIP-CODE-1ST       PIC X(1).              
                           88  BU-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
                       24  FILLER                PIC X(4).              
                   20  BU-ZIP-PLUS4              PIC X(4).              
               16  BU-CANADIAN-POSTAL-CODE REDEFINES BU-ZIP.            
                   20  BU-CAN-POSTAL-CODE-1      PIC X(3).              
                   20  BU-CAN-POSTAL-CODE-2      PIC X(3).              
                   20  FILLER                    PIC X(3).              
                                                                        
               16  BU-PHONE-NO                   PIC 9(11)       COMP-3.
                                                                        
               16  FILLER                        PIC X(10).             
                                                                        
           12  FILLER                            PIC X(13).             
           12  BU-CRED-BENE-NAME                 PIC X(25).
           12  BU-CRED-BENE-ADDR                 PIC X(30).
           12  BU-CRED-BENE-CTYST                PIC X(30).
           12  BU-CRED-BENE-ZIP.                                        
               16  BU-CB-ZIP-CODE.                                      
                   20  BU-CB-ZIP-CODE-1ST        PIC X(1).              
                       88  BU-CB-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'. 
                   20  FILLER                    PIC X(4).              
               16  BU-CB-ZIP-PLUS4               PIC X(4).              
           12  BU-CB-CANADIAN-POSTAL-CODE REDEFINES BU-CRED-BENE-ZIP.   
               16  BU-CB-CAN-POSTAL-CODE-1       PIC X(3).              
               16  BU-CB-CAN-POSTAL-CODE-2       PIC X(3).              
               16  FILLER                        PIC X(3).              
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

       FD  ELCERT.
                                       COPY ELCCERT.                         

       FD  ECCERT.
                                       COPY ECCCERT.                         

       FD  ERMAIL.
                                       COPY ERCMAIL.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  SAVE-CERT                   PIC X(11) VALUE SPACES.
       77  SAVE-EFF-DT                 PIC X(6)  VALUE SPACES.
       77  SAVE-STATE                  PIC XX    VALUE SPACES.
       77  PGM-SUB                     PIC S999  COMP   VALUE +511.    
       77  WS-ABEND-FILE-STATUS        PIC XX            VALUE ZERO.    
       77  WS-ABEND-MESSAGE            PIC X(80)         VALUE SPACES.  
       77  WS-ABEND-PROGRAM            PIC X(8)          VALUE SPACES.  
       77  WS-RETURN-CODE              PIC S9(4)         VALUE +0.      
       77  WS-ZERO                     PIC S9     COMP-3 VALUE +0.      
       77  IN-CNT                 PIC 9999999   VALUE ZEROS.
       77  CERT-CNT               PIC 9999      VALUE ZEROS.
       77  S1                     PIC S999      VALUE +0 COMP-3.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ECCERT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELCERT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
           88  ELCERT-FOUND                    VALUE '00'.
       77  ERMAIL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  AGT-SUB                     PIC S999 COMP-3 VALUE +0.
       77  WS-CURRENT-BIN-DATE         PIC XX VALUE LOW-VALUES.
       77  WS-BIN-COMPARE-DT           PIC XX VALUE LOW-VALUES.
       77  WS-COMPARE-TYPE             PIC X  VALUE SPACES.
       77  WS-COMPARE-STATUS           PIC X  VALUE SPACES.
       77  WS-DUP-SW                   PIC X  VALUE SPACES.
           88  DUPLICATE-MAILING              VALUE 'Y'.
       77  WS-CERT-CANCELLED-SW        PIC X  VALUE SPACES.
           88  CERT-ALREADY-CANCELLED         VALUE 'Y'.
       77  WS-DIS-EFF-DT               PIC X(10) VALUE SPACES.
       77  WS-ECCERT-SW                PIC X  VALUE ' '.
           88  VALID-ECCERT                 VALUE 'Y'.
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
           05  EXTR-IN-CNT            PIC 9(7) VALUE ZEROS.
           05  ELCERT-IN-CNT          PIC 9(7) VALUE ZEROS.
           05  ERMAIL-DP-CNT          PIC 9(7) VALUE ZEROS.
           05  ERMAIL-MTH-CNT         PIC 9(7) VALUE ZEROS.
           05  ERMAIL-CID-CNT         PIC 9(7) VALUE ZEROS.
           05  ERMAIL-OT-CNT          PIC 9(7) VALUE ZEROS.
           05  ERMAIL-UD-CNT          PIC 9(7) VALUE ZEROS.
           05  ELCERT-UD-CNT          PIC 9(7) VALUE ZEROS.
           05  ELCERT-AC-CNT          PIC 9(7) VALUE ZEROS.
           05  WS-INPUT-SW            PIC X VALUE ' '.
               88  END-OF-INPUT             VALUE 'Y'.
                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0040-OPEN-FILES     THRU 0040-EXIT
           PERFORM 0050-INIT           THRU 0050-EXIT

           PERFORM 0020-PROCESS        THRU 0020-EXIT UNTIL
              (END-OF-INPUT)
      *       OR (IN-CNT > 10)
           PERFORM 0060-CLOSE-FILES    THRU 0060-EXIT

           DISPLAY ' ERMAIL BACKUPS READ  ' EXTR-IN-CNT
           DISPLAY ' CID ERMAIL RECORDS   ' ERMAIL-CID-CNT
           DISPLAY ' MATCHES TO BACKUP    ' ERMAIL-MTH-CNT
           DISPLAY ' ELCERT RECS FOUND    ' ELCERT-IN-CNT
           DISPLAY ' ERMAIL RECS DUPS     ' ERMAIL-DP-CNT
           DISPLAY ' ERMAIL RECS ADDED    ' ERMAIL-OT-CNT
           DISPLAY ' ERMAIL RECS UPDATED  ' ERMAIL-UD-CNT
           DISPLAY ' ELCERT RECS UPDATED  ' ELCERT-UD-CNT
           DISPLAY ' ELCERT RECS PREV CAN ' ELCERT-AC-CNT

           GOBACK

           .
       0015-CHECK-CERT.

           MOVE BU-CONTROL-PRIMARY     TO CM-CONTROL-PRIMARY

           READ ELCERT

           IF ELCERT-FILE-STATUS = '00'
              ADD 1                    TO ELCERT-IN-CNT
              PERFORM 0030-REWRITE-ELCERT
                                       THRU 0030-EXIT
           ELSE
              IF ELCERT-FILE-STATUS = '23' OR '10'
PEMTST           DISPLAY ' NO ELCERT    ' BU-ACCOUNT '  ' BU-CERT-NO
PEMTST*          CONTINUE
              ELSE
                 DISPLAY ' ERROR - ELCERT - READ ' ELCERT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0015-EXIT.
           EXIT.

       0016-READ-ECCERT.

           MOVE ' '                    TO WS-ECCERT-SW
           MOVE BU-CONTROL-PRIMARY     TO CA-CONTROL-PRIMARY

           READ ECCERT

           IF ECCERT-FILE-STATUS = '00'
              SET VALID-ECCERT         TO TRUE
           END-IF

           .
       0016-EXIT.
           EXIT.

       0017-READ-INPUT.

           READ ERMAIL-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO EXTR-IN-CNT
           END-IF

           .
       0017-EXIT.
           EXIT.

       0020-PROCESS.

           IF BU-COMPANY-CD = DTE-CLASIC-COMPANY-CD
              PERFORM 0021-PROCESS     THRU 0021-EXIT
           END-IF

           PERFORM 0017-READ-INPUT     THRU 0017-EXIT

           .
       0020-EXIT.
           EXIT.

       0021-PROCESS.
       
           IF BU-CARRIER = '6'
              MOVE '9'                 TO BU-CARRIER
           END-IF

           ADD 1     TO ERMAIL-CID-CNT

           PERFORM 0022-READ-ERMAIL    THRU 0022-EXIT

           IF ERMAIL-FILE-STATUS = '10' OR '23'
              PERFORM 0016-READ-ECCERT THRU 0016-EXIT
              IF VALID-ECCERT
                 PERFORM 0015-CHECK-CERT
                                       THRU 0015-EXIT
                 PERFORM 0070-ADD-ERMAIL
                                       THRU 0070-EXIT
              ELSE
                 MOVE BU-CERT-EFF-DT         TO DC-BIN-DATE-1
                 MOVE ' '                    TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE DC-GREG-DATE-1-EDIT TO WS-DIS-EFF-DT
                 ELSE
                    DISPLAY ' ERROR CONVERING EFF DT '
                       BU-CERT-NO
                    PERFORM ABEND-PGM
                 END-IF
                 DISPLAY ' NO ECCERT RECORD FOUND ' BU-CARRIER
                    BU-STATE ' ' BU-ACCOUNT ' '
                    WS-DIS-EFF-DT ' ' BU-CERT-NO
              END-IF
           END-IF

           .
       0021-EXIT.
            EXIT.

       0022-READ-ERMAIL.

           MOVE BU-CONTROL-PRIMARY     TO MA-CONTROL-PRIMARY
           READ ERMAIL

           IF ERMAIL-FILE-STATUS = '00'
              ADD 1                    TO ERMAIL-MTH-CNT
           ELSE
              IF ERMAIL-FILE-STATUS NOT = '23' AND '10'
                 DISPLAY ' ERROR - ERMAIL - READ '  ERMAIL-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0022-EXIT.
           EXIT.

       0030-REWRITE-ELCERT.
       
           IF CM-INSURED-ADDRESS-SW NOT = '1'
              MOVE '1'                 TO CM-INSURED-ADDRESS-SW
PEMTST*       MOVE '00'                TO ELCERT-FILE-STATUS
PEMTST        REWRITE CERTIFICATE-MASTER
              IF ELCERT-FILE-STATUS = '00'
      *          DISPLAY ' REWRITE CERT ' EX-ACCOUNT '  ' EX-CERT
                 ADD 1                 TO ELCERT-UD-CNT
              ELSE
                 DISPLAY ' ERROR - ELCERT - REWRITE '
                    ELCERT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0030-EXIT.
            EXIT.

       0040-OPEN-FILES.
       
           OPEN INPUT ERMAIL-IN ECCERT
PEMTST*           ELCERT ERMAIL
PEMTST     OPEN I-O ELCERT

           IF ELCERT-FILE-STATUS NOT = '00'
              DISPLAY ' ELCERT OPEN ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ECCERT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ECCERT - OPEN ' ECCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

PEMTST     OPEN I-O ERMAIL

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL OPEN ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-INIT.
       
           DISPLAY ' ACCEPT DATE ' WS-ACCEPT-DATE

           MOVE WS-ACCEPT-DATE         TO DC-GREG-DATE-1-YMD
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           PERFORM 0017-READ-INPUT     THRU 0017-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-CLOSE-FILES.
       
           CLOSE ELCERT
                 ERMAIL
                 ERMAIL-IN
                 ECCERT

           IF ELCERT-FILE-STATUS NOT = '00'
              DISPLAY ' ELCERT CLOSE ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ECCERT-FILE-STATUS NOT = '00'
              DISPLAY ' ECCERT CLOSE ' ECCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL CLOSE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-ADD-ERMAIL.

           MOVE BU-MAILING-DATA        TO MAILING-DATA

           MOVE MA-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO WS-DIS-EFF-DT
           ELSE
              DISPLAY ' ERROR CONVERING EFF DT '
                 BU-CERT-NO
              PERFORM ABEND-PGM
           END-IF

           PERFORM 0074-WRITE-ERMAIL   THRU 0074-EXIT

           .
       0070-EXIT.
            EXIT.

       0074-WRITE-ERMAIL.
       
           PERFORM 0076-CHK-FOR-HEX7E  THRU 0076-EXIT

           MOVE 'MA'                   TO MA-RECORD-ID
                                          
           DISPLAY ' ABOUT TO ADD ERMAIL RECORD ' MA-CARRIER ' '
              MA-STATE ' ' MA-ACCOUNT ' ' WS-DIS-EFF-DT ' ' MA-CERT-NO
PEMTST*    MOVE '00'                   TO ERMAIL-FILE-STATUS
PEMTST     WRITE MAILING-DATA

           IF ERMAIL-FILE-STATUS = '00'
      *       DISPLAY ' WRITE ERMAIL   ' EX-ACCOUNT '  ' EX-CERT
              ADD 1                    TO ERMAIL-OT-CNT
           ELSE
              DISPLAY ' ERROR - ERMAIL - WRITE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0074-EXIT.
           EXIT.

       0076-CHK-FOR-HEX7E.

      *  I KNOW, I KNOW, UR ASKING Y I ADDED THIS.
      *  WELL, I WAS GETTING SOME ~ CHARACTERS FROM THE 
      *  EXTRACT FILE FROM SDGA, SO I ADDED THIS CODE

           INSPECT MA-ADDRESS-LINE-1
              REPLACING ALL X'7E' BY SPACES
           INSPECT MA-ADDRESS-LINE-2
              REPLACING ALL X'7E' BY SPACES
           INSPECT MA-CITY-STATE
              REPLACING ALL X'7E' BY SPACES
           INSPECT MA-ZIP
              REPLACING ALL X'7E' BY SPACES
              
           .
       0076-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX'              USING DATE-CONVERSION-DATA.

       8500-EXIT.
           EXIT.
           EJECT

       ABEND-PGM.   COPY ELCABEND.

