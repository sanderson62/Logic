       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCRFTAX.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  CERT-IN             ASSIGN TO CERTIN.
           SELECT  CERT-OUT            ASSIGN TO CERTOT.

           SELECT DISK-DATE            ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  CERT-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSCRT01.

       FD  CERT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  CERT-RECORD                 PIC X(1056).

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE ' PEMCRFTAX   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.                          
           88  END-OF-INPUT              VALUE 'Y'.
       77  CERT-RECS-IN            PIC 9(9) VALUE ZEROS.                        
       77  CERT-RECS-OUT           PIC 9(9) VALUE ZEROS.                        
       77  CERT-RECS-FIX           PIC 9(9) VALUE ZEROS.                        
       77  WS-DIS-DATE             PIC X(10)  VALUE SPACES.
       77  A1                      PIC S999 COMP-3 VALUE +0.
       77  WS-LF-ISS-TAX           PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-AH-ISS-TAX           PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-LF-REF-TAX           PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-AH-REF-TAX           PIC S9(7)V99 COMP-3 VALUE +0.
       77  PGM-SUB                 PIC S999    COMP    VALUE +050.
       77  WS-RETURN-CODE          PIC S9(4)   COMP.
       77  WS-ABEND-MESSAGE        PIC X(80).
       77  WS-ZERO                 PIC S999    COMP-3 VALUE +0.
       77  WS-ABEND-FILE-STATUS    PIC XX      VALUE '00'.
       01  WS-DISPLAY-DT           PIC 9(8) VALUE ZEROS.
       01  WS-DISPLAY-AMT          PIC Z,ZZZ,ZZ9.99 VALUE ZEROS.
       01  FILLER.
           05  WS-WORK-DATE        PIC 9(8)  VALUE ZEROS.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAIN.                                                               
                                                                                
                                       COPY ELCDTERX.

           PERFORM 0500-OPEN-FILES     THRU 0500-EXIT
                                                                                
           PERFORM 0700-INITIALIZE     THRU 0700-EXIT

           PERFORM 0050-PROCESS-CERT   THRU 0050-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0600-CLOSE-FILES    THRU 0600-EXIT

           DISPLAY ' CERT RECORDS READ    ' CERT-RECS-IN                        
           DISPLAY ' CERT RECORDS WRITTEN ' CERT-RECS-OUT                       
           DISPLAY ' CERT RECORDS FIXED   ' CERT-RECS-FIX                    
           MOVE WS-LF-ISS-TAX          TO WS-DISPLAY-AMT
           DISPLAY ' LIFE PREM TAX    ' WS-DISPLAY-AMT
           MOVE WS-AH-ISS-TAX          TO WS-DISPLAY-AMT
           DISPLAY '  AH  PREM TAX    ' WS-DISPLAY-AMT
           MOVE WS-LF-REF-TAX          TO WS-DISPLAY-AMT
           DISPLAY ' LIFE RFND TAX    ' WS-DISPLAY-AMT
           MOVE WS-AH-REF-TAX          TO WS-DISPLAY-AMT
           DISPLAY '  AH  RFND TAX    ' WS-DISPLAY-AMT

           GOBACK                                                               

           .                                                                    
       0050-PROCESS-CERT.
                                                                                
           IF DTE-CLIENT = 'CID'
              PERFORM 0100-FIX-CID-CERT
                                       THRU 0100-EXIT
           ELSE
              PERFORM 0200-FIX-DCC-CERT
                                       THRU 0200-EXIT
           END-IF

           PERFORM 0400-WRITE-CERT     THRU 0400-EXIT
           PERFORM 0300-CERT-READ      THRU 0300-EXIT

           .                                                                    
       0050-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0100-FIX-CID-CERT.

           IF (CR-STATE = 'ID')
              AND (CR-DT > 20091231)
              IF CR-LFPRM > ZEROS
                 AND CR-LF-ISS-PREM-TAX NOT = +.0150
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF ISS FROM ' CR-LF-ISS-PREM-TAX
                 ' TO +.0150 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0150           TO CR-LF-ISS-PREM-TAX
                 COMPUTE WS-LF-ISS-TAX = WS-LF-ISS-TAX +
                    (CR-LF-ISS-PREM-TAX * (CR-LFPRM + CR-LFPRM-ALT))
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF CR-AHPRM > ZEROS
                 AND CR-AH-ISS-PREM-TAX NOT = +.0150
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH ISS FROM ' CR-AH-ISS-PREM-TAX
                 ' TO +.0150 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0150           TO CR-AH-ISS-PREM-TAX
                 COMPUTE WS-AH-ISS-TAX = WS-AH-ISS-TAX +
                    (CR-AH-ISS-PREM-TAX * CR-AHPRM)
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF (CR-LFRFND > ZEROS)
                 AND (CR-LF-CNC-PREM-TAX NOT = +.0150)
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF REF FROM ' CR-LF-CNC-PREM-TAX
                    ' TO +.0150 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0150           TO CR-LF-CNC-PREM-TAX
                 COMPUTE WS-LF-REF-TAX = WS-LF-REF-TAX +
                    (CR-LF-CNC-PREM-TAX * CR-LFRFND)
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF (CR-AHRFND > ZEROS)
                 AND (CR-AH-CNC-PREM-TAX NOT = +.0150)
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH REF FROM ' CR-AH-CNC-PREM-TAX
                    ' TO +.0150 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0150           TO CR-AH-CNC-PREM-TAX
                 COMPUTE WS-AH-REF-TAX = WS-AH-REF-TAX +
                    (CR-AH-CNC-PREM-TAX * CR-AHRFND)
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
           END-IF

           IF (CR-STATE = 'NH')
              AND (CR-DT > 20091231)
              IF (CR-LFPRM > ZEROS)
                 AND (CR-LF-ISS-PREM-TAX NOT = +.0125)
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF ISS FROM ' CR-LF-ISS-PREM-TAX
                 ' TO +.0125 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0125           TO CR-LF-ISS-PREM-TAX
                 COMPUTE WS-LF-ISS-TAX = WS-LF-ISS-TAX +
                    (CR-LF-ISS-PREM-TAX * (CR-LFPRM + CR-LFPRM-ALT))
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF (CR-AHPRM > ZEROS)
                 AND (CR-AH-ISS-PREM-TAX NOT = +.0200)
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH ISS FROM ' CR-AH-ISS-PREM-TAX
                 ' TO +.0200 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0200           TO CR-AH-ISS-PREM-TAX
                 COMPUTE WS-AH-ISS-TAX = WS-AH-ISS-TAX +
                    (CR-AH-ISS-PREM-TAX * CR-AHPRM)
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF (CR-LFRFND > ZEROS)
                 AND (CR-LF-CNC-PREM-TAX NOT = +.0125)
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF REF FROM ' CR-LF-CNC-PREM-TAX
                    ' TO +.0125 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0125           TO CR-LF-CNC-PREM-TAX
                 COMPUTE WS-LF-REF-TAX = WS-LF-REF-TAX +
                    (CR-LF-CNC-PREM-TAX * CR-LFRFND)
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF (CR-AHRFND > ZEROS)
                 AND (CR-AH-CNC-PREM-TAX NOT = +.0200)
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH REF FROM ' CR-AH-CNC-PREM-TAX
                    ' TO +.0200 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0200           TO CR-AH-CNC-PREM-TAX
                 COMPUTE WS-AH-REF-TAX = WS-AH-REF-TAX +
                    (CR-AH-CNC-PREM-TAX * CR-AHRFND)
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-FIX-DCC-CERT.

           IF (CR-STATE = 'NJ')
              AND (CR-DT > 20081231)
              IF CR-LFPRM > ZEROS
                 AND CR-LF-ISS-PREM-TAX NOT = +.0500
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF ISS FROM ' CR-LF-ISS-PREM-TAX
                 ' TO +.0500 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0500           TO CR-LF-ISS-PREM-TAX
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF CR-AHPRM > ZEROS
                 AND CR-AH-ISS-PREM-TAX NOT = +.0500
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH ISS FROM ' CR-AH-ISS-PREM-TAX
                 ' TO +.0500 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0500           TO CR-AH-ISS-PREM-TAX
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF (CR-LFRFND > ZEROS)
                 AND (CR-LF-CNC-PREM-TAX NOT = +.0500)
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF REF FROM ' CR-LF-CNC-PREM-TAX
                    ' TO +.0500 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0500           TO CR-LF-CNC-PREM-TAX
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF (CR-AHRFND > ZEROS)
                 AND (CR-AH-CNC-PREM-TAX NOT = +.0500)
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH REF FROM ' CR-AH-CNC-PREM-TAX
                    ' TO +.0500 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0500           TO CR-AH-CNC-PREM-TAX
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
           END-IF

           IF (CR-STATE = 'NV')
              AND (CR-DT > 20071231)
              IF CR-LFPRM > ZEROS
                 AND CR-LF-ISS-PREM-TAX NOT = +.0200
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF ISS FROM ' CR-LF-ISS-PREM-TAX
                 ' TO +.0200 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0200           TO CR-LF-ISS-PREM-TAX
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF CR-AHPRM > ZEROS
                 AND CR-AH-ISS-PREM-TAX NOT = +.0200
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH ISS FROM ' CR-AH-ISS-PREM-TAX
                 ' TO +.0200 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0200           TO CR-AH-ISS-PREM-TAX
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF (CR-LFRFND > ZEROS)
                 AND (CR-LF-CNC-PREM-TAX NOT = +.0200)
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF REF FROM ' CR-LF-CNC-PREM-TAX
                    ' TO +.0200 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0200           TO CR-LF-CNC-PREM-TAX
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
              IF (CR-AHRFND > ZEROS)
                 AND (CR-AH-CNC-PREM-TAX NOT = +.0200)
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH REF FROM ' CR-AH-CNC-PREM-TAX
                    ' TO +.0200 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0200           TO CR-AH-CNC-PREM-TAX
                 ADD 1                 TO CERT-RECS-FIX
              END-IF
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-CERT-READ.                                                          

           READ CERT-IN AT END                                                  
              SET END-OF-INPUT         TO TRUE                           
           END-READ                                                             

           IF NOT END-OF-INPUT
              ADD 1                    TO CERT-RECS-IN
           END-IF

           .                                                                    
       0300-EXIT.                                                               
           EXIT.                                                                

       0400-WRITE-CERT.                                                         

           WRITE CERT-RECORD           FROM CERTIFICATE-RECORD
           ADD 1                       TO CERT-RECS-OUT

           .                                                                    
       0400-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0500-OPEN-FILES.                                                         
                                                                                
           OPEN INPUT CERT-IN
               OUTPUT CERT-OUT

           .                                                                    
       0500-EXIT.                                                               
           EXIT.                                                                

       0600-CLOSE-FILES.                                                        

           CLOSE CERT-IN CERT-OUT

           .                                                                    
       0600-EXIT.                                                               
           EXIT.                                                                

       0700-INITIALIZE.                                                         

           PERFORM 0300-CERT-READ      THRU 0300-EXIT

           .                                                                    
       0700-EXIT.                                                               
           EXIT.                                                                
                                                                                
       ABEND-PGM.
                                       COPY ELCABEND.
