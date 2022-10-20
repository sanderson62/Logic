       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCRF3B
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                                                                                
           SELECT  CERT-IN             ASSIGN TO CERTIN.
           SELECT  CERT-OUT            ASSIGN TO CERTOT.

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

       WORKING-STORAGE SECTION.                                                 
       77  FILLER  PIC X(32) VALUE '********************************'.          
       77  FILLER  PIC X(32) VALUE '   PEMCRF3B  WORKING-STORAGE    '.          
       77  FILLER  PIC X(32) VALUE '********************************'.          

       77  WS-EOF-SW               PIC X VALUE SPACES.                          
           88  END-OF-INPUT              VALUE 'Y'.
       77  CERT-RECS-IN            PIC 9(9) VALUE ZEROS.                        
       77  CERT-RECS-OUT           PIC 9(9) VALUE ZEROS.                        
       77  CERT-RECS-FIX           PIC 9(9) VALUE ZEROS.                        
       77  ID-ISS-RECS             PIC 9(9) VALUE ZEROS.
       77  NM-ISS-RECS             PIC 9(9) VALUE ZEROS.
       77  NH-ISS-RECS             PIC 9(9) VALUE ZEROS.
       77  ID-REF-RECS             PIC 9(9) VALUE ZEROS.
       77  NH-REF-RECS             PIC 9(9) VALUE ZEROS.
       77  NM-REF-RECS             PIC 9(9) VALUE ZEROS.
       77  WS-DIS-DATE             PIC X(10)  VALUE SPACES.
       77  A1                      PIC S999 COMP-3 VALUE +0.
       01  FILLER.
           05  WS-WORK-DATE        PIC 9(8)  VALUE ZEROS.
                                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAIN.                                                               
                                                                                
           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT
                                                                                
           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-CERT   THRU 0100-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CERT RECORDS READ    ' CERT-RECS-IN                        
           DISPLAY ' CERT RECORDS WRITTEN ' CERT-RECS-OUT                       
      *    DISPLAY ' CERT RECORDS FIXED   ' CERT-RECS-FIX                    
           DISPLAY ' ID ISS RECS          ' ID-ISS-RECS
           DISPLAY ' NM ISS RECS          ' NM-ISS-RECS
           DISPLAY ' NH ISS RECS          ' NH-ISS-RECS
           DISPLAY ' ID REF RECS          ' ID-REF-RECS
           DISPLAY ' NM REF RECS          ' NM-REF-RECS
           DISPLAY ' NH REF RECS          ' NH-REF-RECS
           GOBACK                                                               

           .                                                                    
       0100-PROCESS-CERT.                                                       
                                                                                
           IF (CR-STATE = 'IA')
              AND (CR-ACCOUNT = '05500403OE')
              PERFORM 0110-FIX-CERT    THRU 0110-EXIT
           END-IF

           PERFORM 0300-WRITE-CERT     THRU 0300-EXIT
           PERFORM 0200-CERT-READ      THRU 0200-EXIT

           .                                                                    
       0100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0110-FIX-CERT.

           IF CR-AH-DEV-PCT NOT NUMERIC
              MOVE +0                  TO CR-AH-DEV-PCT
           END-IF

           IF CR-AH-DEV-PCT = ZEROS
              MOVE +1.818218           TO CR-AH-DEV-PCT
              COMPUTE CR-AH-NSP-PRM ROUNDED = CR-AHPRM / 1.818218
              DISPLAY ' FIXING CERT ' CR-STATE ' ' CR-ACCOUNT
                 ' ' CR-CERT-NO
           END-IF

           .
       0110-EXIT.
           EXIT.

       0200-CERT-READ.                                                          

           READ CERT-IN AT END                                                  
              SET END-OF-INPUT         TO TRUE                           
           END-READ                                                             

           IF NOT END-OF-INPUT
              ADD 1                    TO CERT-RECS-IN
           END-IF

           .                                                                    
       0200-EXIT.                                                               
           EXIT.                                                                

       0300-WRITE-CERT.                                                         

           WRITE CERT-RECORD           FROM CERTIFICATE-RECORD
           ADD 1                       TO CERT-RECS-OUT

           .                                                                    
       0300-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0400-OPEN-FILES.                                                         
                                                                                
           OPEN INPUT CERT-IN
               OUTPUT CERT-OUT

           .                                                                    
       0400-EXIT.                                                               
           EXIT.                                                                

       0500-CLOSE-FILES.                                                        

           CLOSE CERT-IN CERT-OUT

           .                                                                    
       0500-EXIT.                                                               
           EXIT.                                                                

       0600-INITIALIZE.                                                         

           PERFORM 0200-CERT-READ      THRU 0200-EXIT

           .                                                                    
       0600-EXIT.                                                               
           EXIT.                                                                
                                                                                
