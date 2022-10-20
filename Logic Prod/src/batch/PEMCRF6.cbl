       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCRF6.
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
       77  FILLER  PIC X(32) VALUE '   PEMCRF6   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.          

       77  WS-EOF-SW               PIC X VALUE SPACES.                          
           88  END-OF-INPUT              VALUE 'Y'.
       77  CERT-RECS-IN            PIC 9(9) VALUE ZEROS.                        
       77  CERT-RECS-OUT           PIC 9(9) VALUE ZEROS.                        
       77  CERT-RECS-FIX           PIC 9(9) VALUE ZEROS.                        
       77  CERT-BYPASS-7           PIC 9(9) VALUE ZEROS.
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

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CERT RECORDS READ    ' CERT-RECS-IN
           DISPLAY ' CERT RECORDS WRITTEN ' CERT-RECS-OUT
           DISPLAY ' CERT RECORDS FIXED   ' CERT-RECS-FIX
           DISPLAY ' CERT BYPASSED 7      ' CERT-BYPASS-7

           GOBACK

           .                                                                    
       0050-PROCESS-INPUT.

           IF CR-LFRFND = +0
              AND CR-DTHAMT > +0
              AND CR-LF-CANCEL-EXIT-DATE > 0
              AND CR-LF-CLAIM-EXIT-DATE > 0
              PERFORM 0100-FIX-CERT    THRU 0100-EXIT
           END-IF

           PERFORM 0300-WRITE-CERT     THRU 0300-EXIT
           PERFORM 0200-CERT-READ      THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-FIX-CERT.

           IF CR-LF-CURRENT-STATUS = '7'
              ADD 1                    TO CERT-BYPASS-7
      *       DISPLAY ' CURRENT STATUS 7  '
      *          CR-CARRIER ' ' CR-STATE ' ' CR-ACCOUNT ' '
      *          CR-CERT-NO ' ' CR-LF-STATUS-AT-CANCEL '     '
      *          CR-LF-STATUS-AT-DEATH
              GO TO 0100-EXIT
           END-IF

           IF CR-LF-CURRENT-STATUS = '8'
              MOVE '7'                 TO CR-LF-CURRENT-STATUS
              MOVE CR-LF-CLAIM-EXIT-DATE TO CR-LF-CANCEL-EXIT-DATE
              MOVE CR-ENTRY-STATUS     TO CR-LF-STATUS-AT-CANCEL
              MOVE '8'                 TO CR-LF-STATUS-AT-DEATH
              DISPLAY CR-CARRIER ' ' CR-STATE ' ' CR-ACCOUNT ' '
                 CR-CERT-NO ' ' CR-LF-STATUS-AT-CANCEL '     '
                 CR-LF-STATUS-AT-DEATH ' ' CR-LFRFND-CALC
              ADD 1                    TO CERT-RECS-FIX
           END-IF

           .
       0100-EXIT.
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
                                                                                
