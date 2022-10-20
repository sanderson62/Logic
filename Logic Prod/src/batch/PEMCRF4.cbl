       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCRF4.
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
       77  FILLER  PIC X(32) VALUE '   PEMCRF4   WORKING-STORAGE    '.          
       77  FILLER  PIC X(32) VALUE '********************************'.          

       77  WS-EOF-SW               PIC X VALUE SPACES.                          
           88  END-OF-INPUT              VALUE 'Y'.
       77  CERT-RECS-IN            PIC 9(9) VALUE ZEROS.                        
       77  CERT-RECS-OUT           PIC 9(9) VALUE ZEROS.                        
       77  CERT-RECS-FIX           PIC 9(9) VALUE ZEROS.                        
       77  WS-DIS-DATE             PIC X(10)  VALUE SPACES.
       77  A1                      PIC S999 COMP-3 VALUE +0.
       01  WS-DISPLAY-DT           PIC 9(8) VALUE ZEROS.
       01  WS-DISPLAY-AMT          PIC Z,ZZZ,ZZ9.99 VALUE ZEROS.
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
           DISPLAY ' CERT RECORDS FIXED   ' CERT-RECS-FIX                    

           GOBACK                                                               

           .                                                                    
       0100-PROCESS-CERT.                                                       
                                                                                
           EVALUATE TRUE
              WHEN (CR-STATE = 'TX')
                 AND (CR-ACCOUNT = '0600001385' OR '0600001386' OR
                   '0600001387' OR '0600001465' OR '0600001728' OR
                   '0600001758' OR '0600009091' OR '0600990013')
                 AND (CR-DT > 20091231)
                 ADD 1 TO CERT-RECS-FIX
                 IF CR-LCOM-L (2) NOT = ZEROS
                    DISPLAY ' 2 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (2)
                    MOVE .04500        TO CR-LCOM-L (2)
                    DISPLAY ' 2 LIFE COMM AFTER  ' CR-LCOM-L (2)
                 END-IF
                 IF CR-LCOM-AH (2) NOT = ZEROS
                    DISPLAY ' 2  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (2)
                    MOVE .04500        TO CR-LCOM-AH (2)
                    DISPLAY ' 2  AH  COMM AFTER  ' CR-LCOM-AH (2)
                 END-IF
                 IF CR-LCOM-L (3) NOT = ZEROS
                    DISPLAY ' 3 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (3)
                    MOVE .00333        TO CR-LCOM-L (3)
                    DISPLAY ' 3 LIFE COMM AFTER  ' CR-LCOM-L (3)
                 END-IF
                 IF CR-LCOM-AH (3) NOT = ZEROS
                    DISPLAY ' 3  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (3)
                    MOVE .00333           TO CR-LCOM-AH (3)
                    DISPLAY ' 3  AH  COMM AFTER  ' CR-LCOM-AH (3)
                 END-IF
                 IF CR-LCOM-L (4) NOT = ZEROS
                    DISPLAY ' 4 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (4)
                    MOVE .00167        TO CR-LCOM-L (4)
                    DISPLAY ' 4 LIFE COMM AFTER  ' CR-LCOM-L (4)
                 END-IF
                 IF CR-LCOM-AH (4) NOT = ZEROS
                    DISPLAY ' 4  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (4)
                    MOVE .00167        TO CR-LCOM-AH (4)
                    DISPLAY ' 4  AH  COMM AFTER  ' CR-LCOM-AH (4)
                 END-IF

              WHEN (CR-STATE = 'TX')
                 AND (CR-ACCOUNT = '0600001420')
                 AND (CR-DT > 20091231)
                 ADD 1 TO CERT-RECS-FIX
                 IF CR-LCOM-L (2) NOT = ZEROS
                    DISPLAY ' 2 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (2)
                    MOVE .04500        TO CR-LCOM-L (2)
                    DISPLAY ' 2 LIFE COMM AFTER  ' CR-LCOM-L (2)
                 END-IF
                 IF CR-LCOM-AH (2) NOT = ZEROS
                    DISPLAY ' 2  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (2)
                    MOVE .04500        TO CR-LCOM-AH (2)
                    DISPLAY ' 2  AH  COMM AFTER  ' CR-LCOM-AH (2)
                 END-IF

                 MOVE '0001300107'     TO CR-COM-AGT (3)
                 IF CR-LCOM-L (3) NOT = ZEROS
                    DISPLAY ' 3 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (3)
                    MOVE .00333        TO CR-LCOM-L (3)
                    DISPLAY ' 3 LIFE COMM AFTER  ' CR-LCOM-L (3)
                 END-IF
                 IF CR-LCOM-AH (3) NOT = ZEROS
                    DISPLAY ' 3  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (3)
                    MOVE .00333           TO CR-LCOM-AH (3)
                    DISPLAY ' 3  AH  COMM AFTER  ' CR-LCOM-AH (3)
                 END-IF

                 MOVE '0001300108'     TO CR-COM-AGT (4)
                 IF CR-LFTYP NOT = '00' AND '  '
                    DISPLAY ' 4 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (4)
                    MOVE .00167        TO CR-LCOM-L (4)
                    DISPLAY ' 4 LIFE COMM AFTER  ' CR-LCOM-L (4)
                 END-IF
                 IF CR-AHTYP NOT = '00' AND '  '
                    DISPLAY ' 4  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (4)
                    MOVE .00167        TO CR-LCOM-AH (4)
                    DISPLAY ' 4  AH  COMM AFTER  ' CR-LCOM-AH (4)
                 END-IF
                 IF CR-DT > 20100131
                    MOVE '0001300119'  TO CR-COM-AGT (4)
                 END-IF

              WHEN (CR-STATE = 'TX')
                 AND (CR-ACCOUNT = '0600001440')
                 AND (CR-DT > 20091231)
                 ADD 1 TO CERT-RECS-FIX
                 IF CR-LCOM-L (2) NOT = ZEROS
                    DISPLAY ' 2 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (2)
                    MOVE .04500        TO CR-LCOM-L (2)
                    DISPLAY ' 2 LIFE COMM AFTER  ' CR-LCOM-L (2)
                 END-IF
                 IF CR-LCOM-AH (2) NOT = ZEROS
                    DISPLAY ' 2  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (2)
                    MOVE .04500        TO CR-LCOM-AH (2)
                    DISPLAY ' 2  AH  COMM AFTER  ' CR-LCOM-AH (2)
                 END-IF

                 MOVE '0001300107'     TO CR-COM-AGT (3)
                 IF CR-LCOM-L (3) NOT = ZEROS
                    DISPLAY ' 3 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (3)
                    MOVE .00333        TO CR-LCOM-L (3)
                    DISPLAY ' 3 LIFE COMM AFTER  ' CR-LCOM-L (3)
                 END-IF
                 IF CR-LCOM-AH (3) NOT = ZEROS
                    DISPLAY ' 3  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (3)
                    MOVE .00333           TO CR-LCOM-AH (3)
                    DISPLAY ' 3  AH  COMM AFTER  ' CR-LCOM-AH (3)
                 END-IF

                 MOVE '0001300108'     TO CR-COM-AGT (4)
                 IF CR-LFTYP NOT = '00' AND '  '
                    DISPLAY ' 4 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (4)
                    MOVE .00167        TO CR-LCOM-L (4)
                    DISPLAY ' 4 LIFE COMM AFTER  ' CR-LCOM-L (4)
                 END-IF
                 IF CR-AHTYP NOT = '00' AND '  '
                    DISPLAY ' 4  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (4)
                    MOVE .00167        TO CR-LCOM-AH (4)
                    DISPLAY ' 4  AH  COMM AFTER  ' CR-LCOM-AH (4)
                 END-IF


              WHEN (CR-STATE = 'TX')
                 AND (CR-ACCOUNT = '0600001579')
                 AND (CR-DT > 20091231)
                 ADD 1 TO CERT-RECS-FIX
                 IF CR-LCOM-L (2) NOT = ZEROS
                    DISPLAY ' 2 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (2)
                    MOVE .04500        TO CR-LCOM-L (2)
                    DISPLAY ' 2 LIFE COMM AFTER  ' CR-LCOM-L (2)
                 END-IF
                 IF CR-LCOM-AH (2) NOT = ZEROS
                    DISPLAY ' 2  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (2)
                    MOVE .04500        TO CR-LCOM-AH (2)
                    DISPLAY ' 2  AH  COMM AFTER  ' CR-LCOM-AH (2)
                 END-IF
                 IF CR-LCOM-L (3) NOT = ZEROS
                    DISPLAY ' 3 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (3)
                    MOVE .00500        TO CR-LCOM-L (3)
                    DISPLAY ' 3 LIFE COMM AFTER  ' CR-LCOM-L (3)
                 END-IF
                 IF CR-LCOM-AH (3) NOT = ZEROS
                    DISPLAY ' 3  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (3)
                    MOVE .00500           TO CR-LCOM-AH (3)
                    DISPLAY ' 3  AH  COMM AFTER  ' CR-LCOM-AH (3)
                 END-IF


              WHEN (CR-STATE = 'FL')
                 AND (CR-ACCOUNT = '0900733015')
                 AND (CR-DT > 20081231)
                 ADD 1 TO CERT-RECS-FIX
                 IF CR-LCOM-L (2) NOT = ZEROS
                    DISPLAY ' 2 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (2)
                    MOVE .17250        TO CR-LCOM-L (2)
                    DISPLAY ' 2 LIFE COMM AFTER  ' CR-LCOM-L (2)
                 END-IF
                 IF CR-LCOM-AH (2) NOT = ZEROS
                    DISPLAY ' 2  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (2)
                    MOVE .17250        TO CR-LCOM-AH (2)
                    DISPLAY ' 2  AH  COMM AFTER  ' CR-LCOM-AH (2)
                 END-IF
                 IF CR-LCOM-L (3) NOT = ZEROS
                    DISPLAY ' 3 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (3)
                    MOVE .05750        TO CR-LCOM-L (3)
                    DISPLAY ' 3 LIFE COMM AFTER  ' CR-LCOM-L (3)
                 END-IF
                 IF CR-LCOM-AH (3) NOT = ZEROS
                    DISPLAY ' 3  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (3)
                    MOVE .05750           TO CR-LCOM-AH (3)
                    DISPLAY ' 3  AH  COMM AFTER  ' CR-LCOM-AH (3)
                 END-IF

              WHEN (CR-STATE = 'NC')
                 AND (CR-ACCOUNT = '0900733030')
                 AND (CR-DT > 20081231)
                 ADD 1 TO CERT-RECS-FIX
                 IF CR-LCOM-L (2) NOT = ZEROS
                    DISPLAY ' 2 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (2)
                    MOVE .23000        TO CR-LCOM-L (2)
                    DISPLAY ' 2 LIFE COMM AFTER  ' CR-LCOM-L (2)
                 END-IF
                 IF CR-LCOM-AH (2) NOT = ZEROS
                    DISPLAY ' 2  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (2)
                    MOVE .23000        TO CR-LCOM-AH (2)
                    DISPLAY ' 2  AH  COMM AFTER  ' CR-LCOM-AH (2)
                 END-IF
                 DISPLAY ' 3 LIFE COMM BEFORE ' CR-STATE ' '
                    CR-ACCOUNT ' ' CR-CERT-NO ' '
                    CR-LCOM-L (3)
                 MOVE .00000        TO CR-LCOM-L (3)
                 DISPLAY ' 3 LIFE COMM AFTER  ' CR-LCOM-L (3)
                 DISPLAY ' 3  AH  COMM BEFORE ' CR-STATE ' '
                    CR-ACCOUNT ' ' CR-CERT-NO ' '
                    CR-LCOM-AH (3)
                 MOVE .00000           TO CR-LCOM-AH (3)
                 DISPLAY ' 3  AH  COMM AFTER  ' CR-LCOM-AH (3)

              WHEN (CR-STATE = 'FL')
                 AND (CR-ACCOUNT = '0900733036')
                 AND (CR-DT > 20080108)
                 ADD 1 TO CERT-RECS-FIX
                 IF CR-LCOM-L (2) NOT = ZEROS
                    DISPLAY ' 2 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (2)
                    MOVE .17250        TO CR-LCOM-L (2)
                    DISPLAY ' 2 LIFE COMM AFTER  ' CR-LCOM-L (2)
                 END-IF
                 IF CR-LCOM-AH (2) NOT = ZEROS
                    DISPLAY ' 2  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (2)
                    MOVE .17250        TO CR-LCOM-AH (2)
                    DISPLAY ' 2  AH  COMM AFTER  ' CR-LCOM-AH (2)
                 END-IF
                 IF CR-LCOM-L (3) NOT = ZEROS
                    DISPLAY ' 3 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (3)
                    MOVE .05750        TO CR-LCOM-L (3)
                    DISPLAY ' 3 LIFE COMM AFTER  ' CR-LCOM-L (3)
                 END-IF
                 IF CR-LCOM-AH (3) NOT = ZEROS
                    DISPLAY ' 3  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (3)
                    MOVE .05750           TO CR-LCOM-AH (3)
                    DISPLAY ' 3  AH  COMM AFTER  ' CR-LCOM-AH (3)
                 END-IF
                 DISPLAY ' 4 LIFE COMM BEFORE ' CR-STATE ' '
                    CR-ACCOUNT ' ' CR-CERT-NO ' '
                    CR-LCOM-L (4)
                 MOVE .00000        TO CR-LCOM-L (4)
                 DISPLAY ' 4 LIFE COMM AFTER  ' CR-LCOM-L (4)
                 DISPLAY ' 4  AH  COMM BEFORE ' CR-STATE ' '
                    CR-ACCOUNT ' ' CR-CERT-NO ' '
                    CR-LCOM-AH (4)
                 MOVE .00000           TO CR-LCOM-AH (4)
                 DISPLAY ' 4  AH  COMM AFTER  ' CR-LCOM-AH (4)

              WHEN (CR-STATE = 'GA')
                 AND (CR-ACCOUNT = '0990000190')
                 AND (CR-DT > 20091130)
                 ADD 1 TO CERT-RECS-FIX
                 IF CR-LCOM-L (2) NOT = ZEROS
                    DISPLAY ' 2 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (2)
                    MOVE .05750        TO CR-LCOM-L (2)
                    DISPLAY ' 2 LIFE COMM AFTER  ' CR-LCOM-L (2)
                 END-IF
                 IF CR-LCOM-AH (2) NOT = ZEROS
                    DISPLAY ' 2  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (2)
                    MOVE .05750        TO CR-LCOM-AH (2)
                    DISPLAY ' 2  AH  COMM AFTER  ' CR-LCOM-AH (2)
                 END-IF

                 MOVE '0001200084'     TO CR-COM-AGT (3)
                 IF CR-LCOM-L (3) NOT = ZEROS
                    DISPLAY ' 3 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (3)
                    MOVE .01150        TO CR-LCOM-L (3)
                    DISPLAY ' 3 LIFE COMM AFTER  ' CR-LCOM-L (3)
                 END-IF
                 IF CR-LCOM-AH (3) NOT = ZEROS
                    DISPLAY ' 3  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (3)
                    MOVE .01150           TO CR-LCOM-AH (3)
                    DISPLAY ' 3  AH  COMM AFTER  ' CR-LCOM-AH (3)
                 END-IF

                 MOVE '0001200010'     TO CR-COM-AGT (4)
                 IF CR-LFTYP NOT = '00' AND '  '
                    DISPLAY ' 4 LIFE COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-L (4)
                    MOVE .16100        TO CR-LCOM-L (4)
                    DISPLAY ' 4 LIFE COMM AFTER  ' CR-LCOM-L (4)
                 END-IF
                 IF CR-AHTYP NOT = '00' AND '  '
                    DISPLAY ' 4  AH  COMM BEFORE ' CR-STATE ' '
                       CR-ACCOUNT ' ' CR-CERT-NO ' '
                       CR-LCOM-AH (4)
                    MOVE .16100        TO CR-LCOM-AH (4)
                    DISPLAY ' 4  AH  COMM AFTER  ' CR-LCOM-AH (4)
                 END-IF

           END-EVALUATE

           PERFORM 0300-WRITE-CERT     THRU 0300-EXIT
           PERFORM 0200-CERT-READ      THRU 0200-EXIT

           .                                                                    
       0100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0110-FIX-CERT.


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
                                                                                
