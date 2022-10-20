       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCRF3C.
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
       77  FILLER  PIC X(32) VALUE '   PEMCRF3C  WORKING-STORAGE    '.
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
       77  WS-OLD-NAME             PIC X(15) VALUE SPACES.
       77  WS-NEW-NAME             PIC X(15) VALUE SPACES.
       77  WS-NAME-TYPE            PIC X(10) VALUE SPACES.
       77  WS-FIX-SW               PIC X  VALUE SPACES.
           88  NAME-FIXED              VALUE 'Y'.
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
                                                                                
           MOVE ' '                    TO WS-FIX-SW

           IF (CR-LNAME NOT = SPACES)
              AND (CR-LNAME (1:1) = SPACES)
      *       AND (CR-LNAME (1:1) < 'A' OR > 'Z')
              MOVE CR-LNAME            TO WS-OLD-NAME
              MOVE FUNCTION UPPER-CASE(CR-LNAME)
                                       TO CR-LNAME
              PERFORM UNTIL CR-LNAME (1:1) NOT = SPACES
      *       PERFORM UNTIL (CR-LNAME >= 'A' AND <= 'Z')
      *          OR (CR-LNAME = SPACES)
                 MOVE CR-LNAME (2:14)  TO CR-LNAME
              END-PERFORM
              MOVE CR-LNAME            TO WS-NEW-NAME
              MOVE 'PRI LAST'          TO WS-NAME-TYPE
              PERFORM 0150-DISPLAY-CRT THRU 0150-EXIT                            
           END-IF

           IF (CR-FNAME NOT = SPACES)
              AND (CR-FNAME (1:1) = SPACES)
      *       AND (CR-FNAME (1:1) < 'A' OR > 'Z')
              MOVE CR-FNAME            TO WS-OLD-NAME
              MOVE FUNCTION UPPER-CASE(CR-FNAME)
                                       TO CR-FNAME
              PERFORM UNTIL CR-FNAME (1:1) NOT = SPACES
      *       PERFORM UNTIL (CR-FNAME >= 'A' AND <= 'Z')
      *          OR (CR-FNAME = SPACES)
                 MOVE CR-FNAME (2:9)   TO CR-FNAME
              END-PERFORM
              MOVE CR-FNAME            TO WS-NEW-NAME
              MOVE 'PRI FIRST'         TO WS-NAME-TYPE
              PERFORM 0150-DISPLAY-CRT THRU 0150-EXIT                            
           END-IF

           IF (CR-JT-LNAME NOT = SPACES)
              AND (CR-JT-LNAME (1:1) = SPACES)
      *       AND (CR-JT-LNAME (1:1) < 'A' OR > 'Z')
              MOVE CR-JT-LNAME         TO WS-OLD-NAME
              MOVE FUNCTION UPPER-CASE(CR-JT-LNAME)
                                       TO CR-JT-LNAME
              PERFORM UNTIL CR-JT-LNAME (1:1) NOT = SPACES
      *       PERFORM UNTIL (CR-JT-LNAME >= 'A' AND <= 'Z')
      *          OR (CR-JT-LNAME = SPACES)
                 MOVE CR-JT-LNAME (2:14)
                                       TO CR-JT-LNAME
              END-PERFORM
              MOVE CR-JT-LNAME         TO WS-NEW-NAME
              MOVE 'JNT LAST'          TO WS-NAME-TYPE
              PERFORM 0150-DISPLAY-CRT THRU 0150-EXIT                            
           END-IF

           IF (CR-JT-FNAME NOT = SPACES)
              AND (CR-JT-FNAME (1:1) = SPACES)
      *       AND (CR-JT-FNAME (1:1) < 'A' OR > 'Z')
              MOVE CR-JT-FNAME         TO WS-OLD-NAME
              MOVE FUNCTION UPPER-CASE(CR-JT-FNAME)
                                       TO CR-JT-FNAME
              PERFORM UNTIL CR-JT-FNAME (1:1) NOT = SPACES
      *       PERFORM UNTIL (CR-JT-FNAME >= 'A' AND <= 'Z')
      *          OR (CR-JT-FNAME = SPACES)
                 MOVE CR-JT-FNAME (2:9) 
                                       TO CR-JT-FNAME
              END-PERFORM
              MOVE CR-JT-FNAME         TO WS-NEW-NAME
              MOVE 'JNT FIRST'         TO WS-NAME-TYPE
              PERFORM 0150-DISPLAY-CRT THRU 0150-EXIT                            
           END-IF

      *     IF (CR-CARRIER = '1')
      *        AND (CR-ACCOUNT = '0005500120')
      *        AND (CR-ENTRY-STATUS = 'M')
      *        AND ((CR-LF-EXPIRE-DATE > 20070731)
      *            OR (CR-AH-EXPIRE-DATE > 20070731))
      *        PERFORM 0110-FIX-CERT    THRU 0110-EXIT
      *     END-IF

           PERFORM 0300-WRITE-CERT     THRU 0300-EXIT
           PERFORM 0200-CERT-READ      THRU 0200-EXIT

           .                                                                    
       0100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0110-FIX-CERT.

           IF ((CR-LFTYP NOT = '00' AND '  ')
              AND (CR-LF-CANCEL-EXIT-DATE = ZEROS))
                             OR
              ((CR-AHTYP NOT = '00' AND '  ')
              AND (CR-AH-CANCEL-EXIT-DATE = ZEROS))
              ADD 1 TO CERT-RECS-FIX
           END-IF


           IF CR-LFTYP NOT = '00' AND '  '
              IF CR-LF-CANCEL-EXIT-DATE = ZEROS
                 MOVE 20090831         TO CR-LF-CANCEL-EXIT-DATE
                 MOVE 20070801         TO CR-LF-CANC-DT
                 MOVE CR-LF-CURRENT-STATUS
                                       TO CR-LF-STATUS-AT-CANCEL
                 MOVE '8'              TO CR-LF-CURRENT-STATUS
                 MOVE 'AUTO'           TO CR-LF-EXIT-BATCH
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CANCELING LF COVERAGE ON ' CR-ACCT-CONTROL
                 ' ' WS-DISPLAY-DT ' ' CR-CERT-NO
              END-IF
           END-IF

           IF CR-AHTYP NOT = '00' AND '  '
              IF CR-AH-CANCEL-EXIT-DATE = ZEROS
                 MOVE 20090831         TO CR-AH-CANCEL-EXIT-DATE
                 MOVE 20070801         TO CR-AH-CANC-DT
                 MOVE CR-AH-CURRENT-STATUS
                                       TO CR-AH-STATUS-AT-CANCEL
                 MOVE '8'              TO CR-AH-CURRENT-STATUS
                 MOVE 'AUTO'           TO CR-AH-EXIT-BATCH
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CANCELING AH COVERAGE ON ' CR-ACCT-CONTROL
                 ' ' WS-DISPLAY-DT ' ' CR-CERT-NO
              END-IF
           END-IF

           .
       0110-EXIT.
           EXIT.

       0150-DISPLAY-CRT.

           MOVE CR-DT                  TO WS-DISPLAY-DT

           SET NAME-FIXED              TO TRUE

           DISPLAY ' CORRECTD ' WS-NAME-TYPE ' FROM ' WS-OLD-NAME
              ' TO ' WS-NEW-NAME ' ON ' CR-CARRIER ' ' CR-STATE ' '
              CR-ACCOUNT ' ' WS-DISPLAY-DT ' ' CR-CERT-NO

           .
       0150-EXIT.
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

           IF NAME-FIXED
              ADD 1                    TO CERT-RECS-FIX
           END-IF

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
                                                                                
