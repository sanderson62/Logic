       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCRF3A.
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
       77  FILLER  PIC X(32) VALUE '   PEMCRF3A  WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  CERT-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  CERT-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  CERT-RECS-FIX           PIC 9(9) VALUE ZEROS.
       77  WS-DIS-DATE             PIC X(10)  VALUE SPACES.
       77  A1                      PIC S999 COMP-3 VALUE +0.
       77  WS-PC-SW                PIC X VALUE ' '.

       01  FILLER.
           05  WS-WORK-DATE        PIC 9(8)  VALUE ZEROS.

       01  CERT-TABLE.
        05  FIX-TABLE.
         10  FILLER PIC X(31)  VALUE 'FL0000904200200302250008494436 '.
         10  FILLER PIC X(31)  VALUE 'GU0000861300200101250007845175 '.
         10  FILLER PIC X(31)  VALUE 'IA0000556700200001190007334892 '.
         10  FILLER PIC X(31)  VALUE 'IA0000678100200107170008159305 '.
         10  FILLER PIC X(31)  VALUE 'IL0000579700200201070008285794 '.
         10  FILLER PIC X(31)  VALUE 'IL0000641000200101190007946074 '.
         10  FILLER PIC X(31)  VALUE 'IL0000725200200302130008279590 '.
         10  FILLER PIC X(31)  VALUE 'MN0000652100200002120007769093 '.
         10  FILLER PIC X(31)  VALUE 'MN0000652400200102010008025691 '.
         10  FILLER PIC X(31)  VALUE 'MO0000457900200102200008023157 '.
         10  FILLER PIC X(31)  VALUE 'MO0000652800200102160007178690 '.
         10  FILLER PIC X(31)  VALUE 'MO0000652800200102280007178733 '.
         10  FILLER PIC X(31)  VALUE 'MO0000652800200301060008462583 '.
         10  FILLER PIC X(31)  VALUE 'MO0000674800200201120008275524 '.
         10  FILLER PIC X(31)  VALUE 'NE0000019460200101220007488866 '.
         10  FILLER PIC X(31)  VALUE 'NE0000084620200002250007491316 '.
         10  FILLER PIC X(31)  VALUE 'NE0000100490200202110007947433 '.
         10  FILLER PIC X(31)  VALUE 'NE0000573900200201310007962226 '.
         10  FILLER PIC X(31)  VALUE 'NE0000797000200101060007948251 '.
         10  FILLER PIC X(31)  VALUE 'NE0000797500200308090008504884 '.
         10  FILLER PIC X(31)  VALUE 'NE0000847800200201270007955263 '.
         10  FILLER PIC X(31)  VALUE 'OH0000733300200102230007890975 '.
         10  FILLER PIC X(31)  VALUE 'OH0000791600200301270008156082 '.
         10  FILLER PIC X(31)  VALUE 'OH0000793100200101230007827683 '.
         10  FILLER PIC X(31)  VALUE 'OH0000799200200306300008514914 '.
         10  FILLER PIC X(31)  VALUE 'OK0000805500200102190007766111 '.
         10  FILLER PIC X(31)  VALUE 'OR0000069220200102170008013378 '.
         10  FILLER PIC X(31)  VALUE 'OR0000069360199808080006855968 '.
         10  FILLER PIC X(31)  VALUE 'OR0000069400200005290007880035 '.
         10  FILLER PIC X(31)  VALUE 'OR0000595200199802130006179587 '.
         10  FILLER PIC X(31)  VALUE 'OR0000616000200006260007467267 '.
         10  FILLER PIC X(31)  VALUE 'OR0000706900200103240007406943 '.
         10  FILLER PIC X(31)  VALUE 'OR0000803700200207010008300305 '.
         10  FILLER PIC X(31)  VALUE 'SD0000013630200201220008162918 '.
         10  FILLER PIC X(31)  VALUE 'VA0000902800200101080008064412 '.
         10  FILLER PIC X(31)  VALUE 'VA0000902800200102010008065652 '.
         10  FILLER PIC X(31)  VALUE 'VA0000903200200102100008064902 '.
         10  FILLER PIC X(31)  VALUE 'VA0000903400200102210008066266 '.
         10  FILLER PIC X(31)  VALUE 'VA0000905700200102200008064467 '.
         10  FILLER PIC X(31)  VALUE 'VA0000906000200201220008330972 '.
         10  FILLER PIC X(31)  VALUE 'VA0000933200200301150223027890 '.
         10  FILLER PIC X(31)  VALUE 'WI0000480200200102080007267581 '.
         10  FILLER PIC X(31)  VALUE 'WV0000719900200207230008256070 '.
         10  FILLER PIC X(31)  VALUE 'WV0000720600200202120007913837 '.
         10  FILLER PIC X(31)  VALUE 'WV0000722900200007130007909887 '.
         10  FILLER PIC X(31)  VALUE 'WV0000722900200102090007911293 '.
         10  FILLER PIC X(31)  VALUE 'WV0000722900200102170007911298 '.
         10  FILLER PIC X(31)  VALUE 'WV0000723000200102280007819329 '.
         10  FILLER PIC X(31)  VALUE 'WV0000723300200101110007911333 '.
         10  FILLER PIC X(31)  VALUE 'WV0000831600200302120008256140 '.
         10  FILLER PIC X(31)  VALUE 'WV0000832800200101050007910353 '.
        05  FIX-TABLE-R REDEFINES FIX-TABLE OCCURS 51 INDEXED BY S1
             ASCENDING KEY IS FT-KEY.
         10  FT-KEY.
             15  FT-STATE              PIC XX.
             15  FT-ACCOUNT            PIC X(10).
             15  FT-EFFDT              PIC 9(8).
             15  FT-CERT-NO            PIC X(11).

       01  WS-COMPARE-KEY.
           05  WS-STATE                PIC XX.
           05  WS-ACCOUNT              PIC X(10).
           05  WS-EFFDT                PIC 9(8).
           05  WS-CERT-NO              PIC X(11).

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
                                                                                
           PERFORM 0110-FIX-CERT       THRU 0110-EXIT

           PERFORM 0300-WRITE-CERT     THRU 0300-EXIT
           PERFORM 0200-CERT-READ      THRU 0200-EXIT

           .                                                                    
       0100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0110-FIX-CERT.

           MOVE SPACES                 TO WS-COMPARE-KEY
           MOVE CR-STATE               TO WS-STATE
           MOVE CR-ACCOUNT             TO WS-ACCOUNT
           MOVE CR-DT                  TO WS-EFFDT
           MOVE CR-CERT-NO             TO WS-CERT-NO

           MOVE SPACES                 TO WS-PC-SW

           SEARCH ALL FIX-TABLE-R AT END
              MOVE SPACES              TO WS-PC-SW
            WHEN WS-COMPARE-KEY = FT-KEY (S1)
               MOVE 'Y'                TO WS-PC-SW
           END-SEARCH

           IF WS-PC-SW = 'Y'
              ADD 1                    TO CERT-RECS-FIX
              MOVE 'Y'                 TO CR-POST-CARD-IND
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

           MOVE ZEROS                  TO CR-LF-CNC-ENT-DT
                                          CR-AH-CNC-ENT-DT
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
                                                                                
