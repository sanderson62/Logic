       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDRETRO.
       AUTHOR.        AJR.
       DATE-COMPILED.
      *REMARKS.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
081408* 081408   2008073000002   AJRA  COMBINE ECS043 EXTRACT FILES INTO
081408*                                ONE DELIMITED FILE.
020409* 020409   2008073000002   AJRA  ADD RETRP PAYMENTS
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  FILECUR-IN      ASSIGN TO SYS011-UT-2400-S-SYS011
                                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  FILEYTD-IN      ASSIGN TO SYS012-UT-2400-S-SYS012
                                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  FILEL12-IN      ASSIGN TO SYS013-UT-2400-S-SYS013
                                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  FILE-OUT        ASSIGN TO SYS020-UT-FBA1-S-SYS020
                                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  FILECUR-IN                                
           RECORDING MODE F                        
           BLOCK CONTAINS 0 RECORDS.               
                                                   
020409 01  FILECUR-IN-REC               PIC X(1413). 

       FD  FILEYTD-IN                                
           RECORDING MODE F                        
           BLOCK CONTAINS 0 RECORDS.               
                                                   
020409 01  FILEYTD-IN-REC               PIC X(1413). 

       FD  FILEL12-IN                                
           RECORDING MODE F                        
           BLOCK CONTAINS 0 RECORDS.               
                                                   
020409 01  FILEL12-IN-REC               PIC X(1413). 

       FD  FILE-OUT                                
           RECORDING MODE F                        
           BLOCK CONTAINS 0 RECORDS.               
                                                   
020409 01  FILE-OUT-REC                 PIC X(2649). 
                                   

           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     AJRCODT WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.


       01  WS-MISC.
           05  WS-EOFCUR-SW            PIC X      VALUE SPACES.
               88  END-OF-CUR                     VALUE 'Y'.
           05  WS-EOFYTD-SW            PIC X      VALUE SPACES.
               88  END-OF-YTD                     VALUE 'Y'.
           05  WS-EOFL12-SW            PIC X      VALUE SPACES.
               88  END-OF-L12                     VALUE 'Y'.
           05  WS-SUB          COMP-3  PIC S9(3)  VALUE +0.
           05  WS-CUR-IN-CNT   COMP-3  PIC S9(7)  VALUE +0.     
           05  WS-YTD-IN-CNT   COMP-3  PIC S9(7)  VALUE +0.     
           05  WS-L12-IN-CNT   COMP-3  PIC S9(7)  VALUE +0.     
           05  WS-OUT-CNT      COMP-3  PIC S9(7)  VALUE +0.     

       01  WS-CUR-IN-RECORD.
           05  WS-CUR-KEY.
               10  WS-CUR-ME-DATE       PIC X(10).
               10  WS-CUR-CARRIER       PIC X(1).
               10  WS-CUR-COMP          PIC X(6).
               10  WS-CUR-STATE         PIC X(2).
               10  WS-CUR-ACCOUNT-NUM   PIC X(10).
               10  WS-CUR-GROUP         PIC X(10).
               10  WS-CUR-EXP-DATE      PIC X(10).
           05  WS-CUR-EFF-DATE          PIC X(10).
           05  WS-CUR-ACCOUNT-NAME      PIC X(30).
           05  WS-CUR-PERIOD            PIC X(3).
           05  WS-CUR-ACCUMULATORS OCCURS 9 TIMES.
               10  WS-CUR-DESCR         PIC X(28).
               10  WS-CUR-PERLFP        PIC X(14).
               10  WS-CUR-PERLFE        PIC X(14).
               10  WS-CUR-ITDLFP        PIC X(14).
               10  WS-CUR-ITDLFE        PIC X(14).
               10  WS-CUR-PERAHP        PIC X(14).
               10  WS-CUR-PERAHE        PIC X(14).
               10  WS-CUR-ITDAHP        PIC X(14).
               10  WS-CUR-ITDAHE        PIC X(14).
           05  WS-CUR-PREM-P-E          PIC X(1).
           05  WS-CUR-CLAIM-P-I         PIC X(1).
           05  WS-CUR-RETRO-Y-N         PIC X(1).
           05  WS-CUR-RETRO-P-E         PIC X(1).
           05  WS-CUR-TAX-P-E           PIC X(1).
020409     05  WS-CUR-PERLF-RETRO-PMT   PIC X(14).
020409     05  WS-CUR-ITDLF-RETRO-PMT   PIC X(14).
020409     05  WS-CUR-PERAH-RETRO-PMT   PIC X(14).
020409     05  WS-CUR-ITDAH-RETRO-PMT   PIC X(14).


       01  WS-YTD-IN-RECORD.
           05  WS-YTD-KEY.
               10  WS-YTD-ME-DATE       PIC X(10).
               10  WS-YTD-CARRIER       PIC X(1).
               10  WS-YTD-COMP          PIC X(6).
               10  WS-YTD-STATE         PIC X(2).
               10  WS-YTD-ACCOUNT-NUM   PIC X(10).
               10  WS-YTD-GROUP         PIC X(10).
               10  WS-YTD-EXP-DATE      PIC X(10).
           05  WS-YTD-EFF-DATE          PIC X(10).
           05  WS-YTD-ACCOUNT-NAME      PIC X(30).
           05  WS-YTD-PERIOD            PIC X(3).
           05  WS-YTD-ACCUMULATORS OCCURS 9 TIMES.
               10  WS-YTD-DESCR         PIC X(28).
               10  WS-YTD-PERLFP        PIC X(14).
               10  WS-YTD-PERLFE        PIC X(14).
               10  WS-YTD-ITDLFP        PIC X(14).
               10  WS-YTD-ITDLFE        PIC X(14).
               10  WS-YTD-PERAHP        PIC X(14).
               10  WS-YTD-PERAHE        PIC X(14).
               10  WS-YTD-ITDAHP        PIC X(14).
               10  WS-YTD-ITDAHE        PIC X(14).
           05  WS-YTD-PREM-P-E          PIC X(1).
           05  WS-YTD-CLAIM-P-I         PIC X(1).
           05  WS-YTD-RETRO-Y-N         PIC X(1).
           05  WS-YTD-RETRO-P-E         PIC X(1).
           05  WS-YTD-TAX-P-E           PIC X(1).
020409     05  WS-YTD-PERLF-RETRO-PMT   PIC X(14).
020409     05  WS-YTD-ITDLF-RETRO-PMT   PIC X(14).
020409     05  WS-YTD-PERAH-RETRO-PMT   PIC X(14).
020409     05  WS-YTD-ITDAH-RETRO-PMT   PIC X(14).


       01  WS-L12-IN-RECORD.
           05  WS-L12-KEY.
               10  WS-L12-ME-DATE       PIC X(10).
               10  WS-L12-CARRIER       PIC X(1).
               10  WS-L12-COMP          PIC X(6).
               10  WS-L12-STATE         PIC X(2).
               10  WS-L12-ACCOUNT-NUM   PIC X(10).
               10  WS-L12-GROUP         PIC X(10).
               10  WS-L12-EXP-DATE      PIC X(10).
           05  WS-L12-EFF-DATE          PIC X(10).
           05  WS-L12-ACCOUNT-NAME      PIC X(30).
           05  WS-L12-PERIOD            PIC X(3).
           05  WS-L12-ACCUMULATORS OCCURS 9 TIMES.
               10  WS-L12-DESCR         PIC X(28).
               10  WS-L12-PERLFP        PIC X(14).
               10  WS-L12-PERLFE        PIC X(14).
               10  WS-L12-ITDLFP        PIC X(14).
               10  WS-L12-ITDLFE        PIC X(14).
               10  WS-L12-PERAHP        PIC X(14).
               10  WS-L12-PERAHE        PIC X(14).
               10  WS-L12-ITDAHP        PIC X(14).
               10  WS-L12-ITDAHE        PIC X(14).
           05  WS-L12-PREM-P-E          PIC X(1).
           05  WS-L12-CLAIM-P-I         PIC X(1).
           05  WS-L12-RETRO-Y-N         PIC X(1).
           05  WS-L12-RETRO-P-E         PIC X(1).
           05  WS-L12-TAX-P-E           PIC X(1).
020409     05  WS-L12-PERLF-RETRO-PMT   PIC X(14).
020409     05  WS-L12-ITDLF-RETRO-PMT   PIC X(14).
020409     05  WS-L12-PERAH-RETRO-PMT   PIC X(14).
020409     05  WS-L12-ITDAH-RETRO-PMT   PIC X(14).


       01  WS-FILE-OUT-RECORD.
           05  WS-FO-KEY.
               10  WS-FO-ME-DATE       PIC X(10).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-CARRIER       PIC X(1).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-COMP          PIC X(6).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-STATE         PIC X(2).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-ACCOUNT-NUM   PIC X(10).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-GROUP         PIC X(10).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-EXP-DATE      PIC X(10).
               10  FILLER              PIC X(1) VALUE ';'.
           05  WS-FO-EFF-DATE          PIC X(10).
           05  FILLER                  PIC X(1) VALUE ';'.
           05  WS-FO-ACCOUNT-NAME      PIC X(30).
           05  FILLER                  PIC X(1) VALUE ';'.
           05  WS-FO-ACCUMULATORS OCCURS 9 TIMES.
               10  WS-FO-DESCR         PIC X(28).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-CURLFP        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-CURLFE        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-YTDLFP        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-YTDLFE        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-L12LFP        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-L12LFE        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-ITDLFP        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-ITDLFE        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-CURAHP        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-CURAHE        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-YTDAHP        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-YTDAHE        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-L12AHP        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-L12AHE        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-ITDAHP        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
               10  WS-FO-ITDAHE        PIC X(14).
               10  FILLER              PIC X(1) VALUE ';'.
           05  WS-FO-PREM-P-E          PIC X(1).
           05  FILLER                  PIC X(1) VALUE ';'.
           05  WS-FO-CLAIM-P-I         PIC X(1).
           05  FILLER                  PIC X(1) VALUE ';'.
           05  WS-FO-RETRO-Y-N         PIC X(1).
           05  FILLER                  PIC X(1) VALUE ';'.
           05  WS-FO-RETRO-P-E         PIC X(1).
           05  FILLER                  PIC X(1) VALUE ';'.
           05  WS-FO-TAX-P-E           PIC X(1).
           05  FILLER                  PIC X(1) VALUE ';'.
020409     05  WS-FO-CURLFRETROPMT     PIC X(14).
020409     05  FILLER                  PIC X(1) VALUE ';'.
020409     05  WS-FO-YTDLFRETROPMT     PIC X(14).
020409     05  FILLER                  PIC X(1) VALUE ';'.
020409     05  WS-FO-L12LFRETROPMT     PIC X(14).
020409     05  FILLER                  PIC X(1) VALUE ';'.
020409     05  WS-FO-ITDLFRETROPMT     PIC X(14).
020409     05  FILLER                  PIC X(1) VALUE ';'.
020409     05  WS-FO-CURAHRETROPMT     PIC X(14).
020409     05  FILLER                  PIC X(1) VALUE ';'.
020409     05  WS-FO-YTDAHRETROPMT     PIC X(14).
020409     05  FILLER                  PIC X(1) VALUE ';'.
020409     05  WS-FO-L12AHRETROPMT     PIC X(14).
020409     05  FILLER                  PIC X(1) VALUE ';'.
020409     05  WS-FO-ITDAHRETROPMT     PIC X(14).
020409     05  FILLER                  PIC X(1) VALUE ';'.
           
       01  WS-DUMMY-RECORD.
           05  WS-DMY-CARRIER           PIC X(1)    VALUE SPACES.
           05  WS-DMY-COMP              PIC X(6)    VALUE SPACES.
           05  WS-DMY-STATE             PIC X(2)    VALUE SPACES.
           05  WS-DMY-ACCOUNT-NUM       PIC X(10)   VALUE SPACES.
           05  WS-DMY-GROUP             PIC X(10)   VALUE SPACES.
           05  WS-DMY-ACCOUNT-NAME      PIC X(30)   VALUE SPACES.
           05  WS-DMY-PERIOD            PIC X(3)    VALUE SPACES.
           05  WS-DMY-ACCUMULATORS OCCURS 9 TIMES.
               10  WS-DMY-DESCR         PIC X(28).
               10  WS-DMY-PERLFP        PIC X(14).
               10  WS-DMY-PERLFE        PIC X(14).
               10  WS-DMY-ITDLFP        PIC X(14).
               10  WS-DMY-ITDLFE        PIC X(14).
               10  WS-DMY-PERAHP        PIC X(14).
               10  WS-DMY-PERAHE        PIC X(14).
               10  WS-DMY-ITDAHP        PIC X(14).
               10  WS-DMY-ITDAHE        PIC X(14).
           05  WS-DMY-PREM-P-E          PIC X(1)    VALUE SPACES.
           05  WS-DMY-CLAIM-P-I         PIC X(1)    VALUE SPACES.
           05  WS-DMY-RETRO-Y-N         PIC X(1).
           05  WS-DMY-RETRO-P-E         PIC X(1).
           05  WS-DMY-TAX-P-E           PIC X(1).
020409     05  WS-DMY-PERLF-RETRO-PMT   PIC X(14).
020409     05  WS-DMY-ITDLF-RETRO-PMT   PIC X(14).
020409     05  WS-DMY-PERAH-RETRO-PMT   PIC X(14).
020409     05  WS-DMY-ITDAH-RETRO-PMT   PIC X(14).

       PROCEDURE DIVISION.

       0000-BEGIN.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-CUR AND END-OF-YTD AND END-OF-L12)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' CUR RECORDS IN ' WS-CUR-IN-CNT
           DISPLAY ' YTD RECORDS IN ' WS-YTD-IN-CNT
           DISPLAY ' L12 RECORDS IN ' WS-L12-IN-CNT
           DISPLAY ' RECORDS OUT    ' WS-OUT-CNT
           GOBACK
           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT FILECUR-IN
                      FILEYTD-IN
                      FILEL12-IN.
           OPEN OUTPUT FILE-OUT.

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE FILECUR-IN
                 FILEYTD-IN
                 FILEL12-IN.
           CLOSE FILE-OUT.

       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE +0 TO WS-SUB.
           PERFORM 9 TIMES
              ADD +1 TO WS-SUB
              MOVE SPACES TO WS-DMY-DESCR (WS-SUB)
              MOVE '           .00' TO WS-DMY-PERLFP (WS-SUB) 
              MOVE '           .00' TO WS-DMY-PERLFE (WS-SUB) 
              MOVE '           .00' TO WS-DMY-ITDLFP (WS-SUB) 
              MOVE '           .00' TO WS-DMY-ITDLFE (WS-SUB) 
              MOVE '           .00' TO WS-DMY-PERAHP (WS-SUB) 
              MOVE '           .00' TO WS-DMY-PERAHE (WS-SUB) 
              MOVE '           .00' TO WS-DMY-ITDAHP (WS-SUB) 
              MOVE '           .00' TO WS-DMY-ITDAHE (WS-SUB) 
              MOVE '           .00' TO WS-DMY-PERLF-RETRO-PMT
              MOVE '           .00' TO WS-DMY-ITDLF-RETRO-PMT
              MOVE '           .00' TO WS-DMY-PERAH-RETRO-PMT
              MOVE '           .00' TO WS-DMY-ITDAH-RETRO-PMT
           END-PERFORM.
           
           PERFORM 0100-READ-FILECUR    THRU 0100-EXIT.
           PERFORM 0110-READ-FILEYTD    THRU 0110-EXIT.
           PERFORM 0120-READ-FILEL12    THRU 0120-EXIT.

       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           IF WS-CUR-KEY EQUAL WS-YTD-KEY AND
              WS-CUR-KEY EQUAL WS-L12-KEY
                 PERFORM 2000-MOVE-AMOUNTS THRU 2000-EXIT
                 PERFORM 0100-READ-FILECUR    THRU 0100-EXIT
                 PERFORM 0110-READ-FILEYTD    THRU 0110-EXIT
                 PERFORM 0120-READ-FILEL12    THRU 0120-EXIT
                 GO TO 0050-EXIT
           END-IF.
           
           IF WS-CUR-KEY EQUAL WS-YTD-KEY
               IF WS-CUR-KEY LESS THAN WS-L12-KEY
                   PERFORM 2010-MOVE-CUR-YTD-AMTS THRU 2010-EXIT
                   PERFORM 0100-READ-FILECUR    THRU 0100-EXIT
                   PERFORM 0110-READ-FILEYTD    THRU 0110-EXIT
                   GO TO 0050-EXIT
               ELSE
                   PERFORM 2020-MOVE-L12-AMTS THRU 2020-EXIT
                   PERFORM 0120-READ-FILEL12  THRU 0120-EXIT
                   GO TO 0050-EXIT 
               END-IF
           END-IF.
           
           IF WS-CUR-KEY LESS THAN WS-YTD-KEY
               IF WS-CUR-KEY EQUAL WS-L12-KEY
                   PERFORM 2030-MOVE-CUR-L12-AMTS THRU 2030-EXIT
                   PERFORM 0100-READ-FILECUR    THRU 0100-EXIT
                   PERFORM 0120-READ-FILEL12    THRU 0120-EXIT
                   GO TO 0050-EXIT
               ELSE
                   IF WS-CUR-KEY LESS THAN WS-L12-KEY
                       PERFORM 2040-MOVE-CUR-AMTS THRU 2040-EXIT
                       PERFORM 0100-READ-FILECUR  THRU 0100-EXIT
                       GO TO 0050-EXIT
                   ELSE
                       PERFORM 2020-MOVE-L12-AMTS THRU 2020-EXIT    
                       PERFORM 0120-READ-FILEL12  THRU 0120-EXIT
                       GO TO 0050-EXIT
                   END-IF
               END-IF
           END-IF.
           
           IF WS-CUR-KEY GREATER THAN WS-YTD-KEY
               IF WS-YTD-KEY EQUAL WS-L12-KEY
                   PERFORM 2050-MOVE-YTD-L12-AMTS THRU 2050-EXIT
                   PERFORM 0110-READ-FILEYTD    THRU 0110-EXIT
                   PERFORM 0120-READ-FILEL12    THRU 0120-EXIT
                   GO TO 0050-EXIT
               ELSE
                   IF WS-YTD-KEY LESS THAN WS-L12-KEY
                       PERFORM 2060-MOVE-YTD-AMTS THRU 2060-EXIT
                       PERFORM 0110-READ-FILEYTD  THRU 0110-EXIT
                       GO TO 0050-EXIT
                   ELSE
                       PERFORM 2020-MOVE-L12-AMTS THRU 2020-EXIT    
                       PERFORM 0120-READ-FILEL12  THRU 0120-EXIT
                       GO TO 0050-EXIT
                   END-IF
               END-IF
           END-IF.
                   
                           
       0050-EXIT.          
           EXIT.           
                           
                           
       0100-READ-FILECUR.   
                           
           READ FILECUR-IN
              AT END      
              SET END-OF-CUR        TO TRUE
           END-READ.        
                           
           IF NOT END-OF-CUR
               ADD +1              TO WS-CUR-IN-CNT
               MOVE FILECUR-IN-REC TO WS-CUR-IN-RECORD
           ELSE
               MOVE WS-DUMMY-RECORD TO WS-CUR-IN-RECORD
           END-IF.          
                                  
       0100-EXIT.          
           EXIT.           
                           
                           
       0110-READ-FILEYTD.   
                           
           READ FILEYTD-IN
              AT END      
              SET END-OF-YTD        TO TRUE
           END-READ.        
                           
           IF NOT END-OF-YTD
               ADD +1              TO WS-YTD-IN-CNT
               MOVE FILEYTD-IN-REC TO WS-YTD-IN-RECORD
           ELSE
               MOVE WS-DUMMY-RECORD TO WS-YTD-IN-RECORD
           END-IF.          
                                  
       0110-EXIT.          
           EXIT.           
                           
       0120-READ-FILEL12.   
                           
           READ FILEL12-IN
              AT END      
              SET END-OF-L12        TO TRUE
           END-READ.        
                           
           IF NOT END-OF-L12
               ADD +1              TO WS-L12-IN-CNT
               MOVE FILEL12-IN-REC TO WS-L12-IN-RECORD
           ELSE
               MOVE WS-DUMMY-RECORD TO WS-L12-IN-RECORD
           END-IF.          
                                  
       0120-EXIT.          
           EXIT.           


       2000-MOVE-AMOUNTS.
       
       MOVE WS-CUR-ME-DATE      TO WS-FO-ME-DATE.
       MOVE WS-CUR-CARRIER      TO WS-FO-CARRIER.
       MOVE WS-CUR-COMP         TO WS-FO-COMP.      
       MOVE WS-CUR-STATE        TO WS-FO-STATE.
       MOVE WS-CUR-ACCOUNT-NUM  TO WS-FO-ACCOUNT-NUM.
       MOVE WS-CUR-GROUP        TO WS-FO-GROUP.
       MOVE WS-CUR-EXP-DATE     TO WS-FO-EXP-DATE.
       MOVE WS-CUR-EFF-DATE     TO WS-FO-EFF-DATE.
       MOVE WS-CUR-ACCOUNT-NAME TO WS-FO-ACCOUNT-NAME.
       MOVE WS-CUR-PREM-P-E     TO WS-FO-PREM-P-E.
       MOVE WS-CUR-CLAIM-P-I    TO WS-FO-CLAIM-P-I.
       MOVE WS-CUR-RETRO-Y-N    TO WS-FO-RETRO-Y-N.
       MOVE WS-CUR-RETRO-P-E    TO WS-FO-RETRO-P-E.
       MOVE WS-CUR-TAX-P-E      TO WS-FO-TAX-P-E.
020409 MOVE WS-CUR-PERLF-RETRO-PMT TO WS-FO-CURLFRETROPMT.
020409 MOVE WS-YTD-PERLF-RETRO-PMT TO WS-FO-YTDLFRETROPMT.
020409 MOVE WS-L12-PERLF-RETRO-PMT TO WS-FO-L12LFRETROPMT.
020409 MOVE WS-CUR-ITDLF-RETRO-PMT TO WS-FO-ITDLFRETROPMT.
020409 MOVE WS-CUR-PERAH-RETRO-PMT TO WS-FO-CURAHRETROPMT.
020409 MOVE WS-YTD-PERAH-RETRO-PMT TO WS-FO-YTDAHRETROPMT.
020409 MOVE WS-L12-PERAH-RETRO-PMT TO WS-FO-L12AHRETROPMT.
020409 MOVE WS-CUR-ITDAH-RETRO-PMT TO WS-FO-ITDAHRETROPMT.

       MOVE +0 TO WS-SUB.
       PERFORM 9 TIMES
           ADD +1 TO WS-SUB
           MOVE WS-CUR-DESCR (WS-SUB) TO WS-FO-DESCR (WS-SUB)
           MOVE WS-CUR-PERLFP (WS-SUB) TO WS-FO-CURLFP (WS-SUB)
           MOVE WS-CUR-PERLFE (WS-SUB) TO WS-FO-CURLFE (WS-SUB)
           MOVE WS-YTD-PERLFP (WS-SUB) TO WS-FO-YTDLFP (WS-SUB)
           MOVE WS-YTD-PERLFE (WS-SUB) TO WS-FO-YTDLFE (WS-SUB)
           MOVE WS-L12-PERLFP (WS-SUB) TO WS-FO-L12LFP (WS-SUB)
           MOVE WS-L12-PERLFE (WS-SUB) TO WS-FO-L12LFE (WS-SUB)
           MOVE WS-CUR-ITDLFP (WS-SUB) TO WS-FO-ITDLFP (WS-SUB)
           MOVE WS-CUR-ITDLFE (WS-SUB) TO WS-FO-ITDLFE (WS-SUB)
           MOVE WS-CUR-PERAHP (WS-SUB) TO WS-FO-CURAHP (WS-SUB)
           MOVE WS-CUR-PERAHE (WS-SUB) TO WS-FO-CURAHE (WS-SUB)
           MOVE WS-YTD-PERAHP (WS-SUB) TO WS-FO-YTDAHP (WS-SUB)
           MOVE WS-YTD-PERAHE (WS-SUB) TO WS-FO-YTDAHE (WS-SUB)
           MOVE WS-L12-PERAHP (WS-SUB) TO WS-FO-L12AHP (WS-SUB)
           MOVE WS-L12-PERAHE (WS-SUB) TO WS-FO-L12AHE (WS-SUB)
           MOVE WS-CUR-ITDAHP (WS-SUB) TO WS-FO-ITDAHP (WS-SUB)
           MOVE WS-CUR-ITDAHE (WS-SUB) TO WS-FO-ITDAHE (WS-SUB)
       END-PERFORM.
       
       WRITE FILE-OUT-REC FROM WS-FILE-OUT-RECORD.
       ADD +1 TO WS-OUT-CNT.
       
       2000-EXIT.
           EXIT.


       2010-MOVE-CUR-YTD-AMTS.
       
       MOVE WS-CUR-ME-DATE      TO WS-FO-ME-DATE.
       MOVE WS-CUR-CARRIER      TO WS-FO-CARRIER.
       MOVE WS-CUR-COMP         TO WS-FO-COMP.      
       MOVE WS-CUR-STATE        TO WS-FO-STATE.
       MOVE WS-CUR-ACCOUNT-NUM  TO WS-FO-ACCOUNT-NUM.
       MOVE WS-CUR-GROUP        TO WS-FO-GROUP.
       MOVE WS-CUR-EXP-DATE     TO WS-FO-EXP-DATE.
       MOVE WS-CUR-EFF-DATE     TO WS-FO-EFF-DATE.
       MOVE WS-CUR-ACCOUNT-NAME TO WS-FO-ACCOUNT-NAME.
       MOVE WS-CUR-PREM-P-E     TO WS-FO-PREM-P-E.
       MOVE WS-CUR-CLAIM-P-I    TO WS-FO-CLAIM-P-I.
       MOVE WS-CUR-RETRO-Y-N    TO WS-FO-RETRO-Y-N.
       MOVE WS-CUR-RETRO-P-E    TO WS-FO-RETRO-P-E.
       MOVE WS-CUR-TAX-P-E      TO WS-FO-TAX-P-E.
020409 MOVE WS-CUR-PERLF-RETRO-PMT TO WS-FO-CURLFRETROPMT.
020409 MOVE WS-YTD-PERLF-RETRO-PMT TO WS-FO-YTDLFRETROPMT.
020409 MOVE WS-DMY-PERLF-RETRO-PMT TO WS-FO-L12LFRETROPMT.
020409 MOVE WS-CUR-ITDLF-RETRO-PMT TO WS-FO-ITDLFRETROPMT.
020409 MOVE WS-CUR-PERAH-RETRO-PMT TO WS-FO-CURAHRETROPMT.
020409 MOVE WS-YTD-PERAH-RETRO-PMT TO WS-FO-YTDAHRETROPMT.
020409 MOVE WS-DMY-PERAH-RETRO-PMT TO WS-FO-L12AHRETROPMT.
020409 MOVE WS-CUR-ITDAH-RETRO-PMT TO WS-FO-ITDAHRETROPMT.

       MOVE +0 TO WS-SUB.
       PERFORM 9 TIMES
           ADD +1 TO WS-SUB
           MOVE WS-CUR-DESCR (WS-SUB) TO WS-FO-DESCR (WS-SUB)
           MOVE WS-CUR-PERLFP (WS-SUB) TO WS-FO-CURLFP (WS-SUB)
           MOVE WS-CUR-PERLFE (WS-SUB) TO WS-FO-CURLFE (WS-SUB)
           MOVE WS-YTD-PERLFP (WS-SUB) TO WS-FO-YTDLFP (WS-SUB)
           MOVE WS-YTD-PERLFE (WS-SUB) TO WS-FO-YTDLFE (WS-SUB)
           MOVE WS-DMY-PERLFP (WS-SUB) TO WS-FO-L12LFP (WS-SUB)
           MOVE WS-DMY-PERLFE (WS-SUB) TO WS-FO-L12LFE (WS-SUB)
           MOVE WS-CUR-ITDLFP (WS-SUB) TO WS-FO-ITDLFP (WS-SUB)
           MOVE WS-CUR-ITDLFE (WS-SUB) TO WS-FO-ITDLFE (WS-SUB)
           MOVE WS-CUR-PERAHP (WS-SUB) TO WS-FO-CURAHP (WS-SUB)
           MOVE WS-CUR-PERAHE (WS-SUB) TO WS-FO-CURAHE (WS-SUB)
           MOVE WS-YTD-PERAHP (WS-SUB) TO WS-FO-YTDAHP (WS-SUB)
           MOVE WS-YTD-PERAHE (WS-SUB) TO WS-FO-YTDAHE (WS-SUB)
           MOVE WS-DMY-PERAHP (WS-SUB) TO WS-FO-L12AHP (WS-SUB)
           MOVE WS-DMY-PERAHE (WS-SUB) TO WS-FO-L12AHE (WS-SUB)
           MOVE WS-CUR-ITDAHP (WS-SUB) TO WS-FO-ITDAHP (WS-SUB)
           MOVE WS-CUR-ITDAHE (WS-SUB) TO WS-FO-ITDAHE (WS-SUB)
       END-PERFORM.
       
       WRITE FILE-OUT-REC FROM WS-FILE-OUT-RECORD.
       ADD +1 TO WS-OUT-CNT.
       
       2010-EXIT.
           EXIT.


       2020-MOVE-L12-AMTS.
       
       MOVE WS-L12-ME-DATE      TO WS-FO-ME-DATE.
       MOVE WS-L12-CARRIER      TO WS-FO-CARRIER.
       MOVE WS-L12-COMP         TO WS-FO-COMP.      
       MOVE WS-L12-STATE        TO WS-FO-STATE.
       MOVE WS-L12-ACCOUNT-NUM  TO WS-FO-ACCOUNT-NUM.
       MOVE WS-L12-GROUP        TO WS-FO-GROUP.
       MOVE WS-L12-EXP-DATE     TO WS-FO-EXP-DATE.
       MOVE WS-L12-EFF-DATE     TO WS-FO-EFF-DATE.
       MOVE WS-L12-ACCOUNT-NAME TO WS-FO-ACCOUNT-NAME.
       MOVE WS-L12-PREM-P-E     TO WS-FO-PREM-P-E.
       MOVE WS-L12-CLAIM-P-I    TO WS-FO-CLAIM-P-I.
       MOVE WS-L12-RETRO-Y-N    TO WS-FO-RETRO-Y-N.
       MOVE WS-L12-RETRO-P-E    TO WS-FO-RETRO-P-E.
       MOVE WS-L12-TAX-P-E      TO WS-FO-TAX-P-E.
020409 MOVE WS-DMY-PERLF-RETRO-PMT TO WS-FO-CURLFRETROPMT.
020409 MOVE WS-DMY-PERLF-RETRO-PMT TO WS-FO-YTDLFRETROPMT.
020409 MOVE WS-L12-PERLF-RETRO-PMT TO WS-FO-L12LFRETROPMT.
020409 MOVE WS-L12-ITDLF-RETRO-PMT TO WS-FO-ITDLFRETROPMT.
020409 MOVE WS-DMY-PERAH-RETRO-PMT TO WS-FO-CURAHRETROPMT.
020409 MOVE WS-DMY-PERAH-RETRO-PMT TO WS-FO-YTDAHRETROPMT.
020409 MOVE WS-L12-PERAH-RETRO-PMT TO WS-FO-L12AHRETROPMT.
020409 MOVE WS-L12-ITDAH-RETRO-PMT TO WS-FO-ITDAHRETROPMT.

       MOVE +0 TO WS-SUB.
       PERFORM 9 TIMES
           ADD +1 TO WS-SUB
           MOVE WS-L12-DESCR (WS-SUB) TO WS-FO-DESCR (WS-SUB)
           MOVE WS-DMY-PERLFP (WS-SUB) TO WS-FO-CURLFP (WS-SUB)
           MOVE WS-DMY-PERLFE (WS-SUB) TO WS-FO-CURLFE (WS-SUB)
           MOVE WS-DMY-PERLFP (WS-SUB) TO WS-FO-YTDLFP (WS-SUB)
           MOVE WS-DMY-PERLFE (WS-SUB) TO WS-FO-YTDLFE (WS-SUB)
           MOVE WS-L12-PERLFP (WS-SUB) TO WS-FO-L12LFP (WS-SUB)
           MOVE WS-L12-PERLFE (WS-SUB) TO WS-FO-L12LFE (WS-SUB)
           MOVE WS-L12-ITDLFP (WS-SUB) TO WS-FO-ITDLFP (WS-SUB)
           MOVE WS-L12-ITDLFE (WS-SUB) TO WS-FO-ITDLFE (WS-SUB)
           MOVE WS-DMY-PERAHP (WS-SUB) TO WS-FO-CURAHP (WS-SUB)
           MOVE WS-DMY-PERAHE (WS-SUB) TO WS-FO-CURAHE (WS-SUB)
           MOVE WS-DMY-PERAHP (WS-SUB) TO WS-FO-YTDAHP (WS-SUB)
           MOVE WS-DMY-PERAHE (WS-SUB) TO WS-FO-YTDAHE (WS-SUB)
           MOVE WS-L12-PERAHP (WS-SUB) TO WS-FO-L12AHP (WS-SUB)
           MOVE WS-L12-PERAHE (WS-SUB) TO WS-FO-L12AHE (WS-SUB)
           MOVE WS-L12-ITDAHP (WS-SUB) TO WS-FO-ITDAHP (WS-SUB)
           MOVE WS-L12-ITDAHE (WS-SUB) TO WS-FO-ITDAHE (WS-SUB)
       END-PERFORM.
       
       WRITE FILE-OUT-REC FROM WS-FILE-OUT-RECORD.
       ADD +1 TO WS-OUT-CNT.
       
       2020-EXIT.
           EXIT.


       2030-MOVE-CUR-L12-AMTS.
       
       MOVE WS-CUR-ME-DATE      TO WS-FO-ME-DATE.
       MOVE WS-CUR-CARRIER      TO WS-FO-CARRIER.
       MOVE WS-CUR-COMP         TO WS-FO-COMP.      
       MOVE WS-CUR-STATE        TO WS-FO-STATE.
       MOVE WS-CUR-ACCOUNT-NUM  TO WS-FO-ACCOUNT-NUM.
       MOVE WS-CUR-GROUP        TO WS-FO-GROUP.
       MOVE WS-CUR-EXP-DATE     TO WS-FO-EXP-DATE.
       MOVE WS-CUR-EFF-DATE     TO WS-FO-EFF-DATE.
       MOVE WS-CUR-ACCOUNT-NAME TO WS-FO-ACCOUNT-NAME.
       MOVE WS-CUR-PREM-P-E     TO WS-FO-PREM-P-E.
       MOVE WS-CUR-CLAIM-P-I    TO WS-FO-CLAIM-P-I.
       MOVE WS-CUR-RETRO-Y-N    TO WS-FO-RETRO-Y-N.
       MOVE WS-CUR-RETRO-P-E    TO WS-FO-RETRO-P-E.
       MOVE WS-CUR-TAX-P-E      TO WS-FO-TAX-P-E.
020409 MOVE WS-CUR-PERLF-RETRO-PMT TO WS-FO-CURLFRETROPMT.
020409 MOVE WS-DMY-PERLF-RETRO-PMT TO WS-FO-YTDLFRETROPMT.
020409 MOVE WS-L12-PERLF-RETRO-PMT TO WS-FO-L12LFRETROPMT.
020409 MOVE WS-CUR-ITDLF-RETRO-PMT TO WS-FO-ITDLFRETROPMT.
020409 MOVE WS-CUR-PERAH-RETRO-PMT TO WS-FO-CURAHRETROPMT.
020409 MOVE WS-DMY-PERAH-RETRO-PMT TO WS-FO-YTDAHRETROPMT.
020409 MOVE WS-L12-PERAH-RETRO-PMT TO WS-FO-L12AHRETROPMT.
020409 MOVE WS-CUR-ITDAH-RETRO-PMT TO WS-FO-ITDAHRETROPMT.

       MOVE +0 TO WS-SUB.
       PERFORM 9 TIMES
           ADD +1 TO WS-SUB
           MOVE WS-CUR-DESCR (WS-SUB) TO WS-FO-DESCR (WS-SUB)
           MOVE WS-CUR-PERLFP (WS-SUB) TO WS-FO-CURLFP (WS-SUB)
           MOVE WS-CUR-PERLFE (WS-SUB) TO WS-FO-CURLFE (WS-SUB)
           MOVE WS-DMY-PERLFP (WS-SUB) TO WS-FO-YTDLFP (WS-SUB)
           MOVE WS-DMY-PERLFE (WS-SUB) TO WS-FO-YTDLFE (WS-SUB)
           MOVE WS-L12-PERLFP (WS-SUB) TO WS-FO-L12LFP (WS-SUB)
           MOVE WS-L12-PERLFE (WS-SUB) TO WS-FO-L12LFE (WS-SUB)
           MOVE WS-CUR-ITDLFP (WS-SUB) TO WS-FO-ITDLFP (WS-SUB)
           MOVE WS-CUR-ITDLFE (WS-SUB) TO WS-FO-ITDLFE (WS-SUB)
           MOVE WS-CUR-PERAHP (WS-SUB) TO WS-FO-CURAHP (WS-SUB)
           MOVE WS-CUR-PERAHE (WS-SUB) TO WS-FO-CURAHE (WS-SUB)
           MOVE WS-DMY-PERAHP (WS-SUB) TO WS-FO-YTDAHP (WS-SUB)
           MOVE WS-DMY-PERAHE (WS-SUB) TO WS-FO-YTDAHE (WS-SUB)
           MOVE WS-L12-PERAHP (WS-SUB) TO WS-FO-L12AHP (WS-SUB)
           MOVE WS-L12-PERAHE (WS-SUB) TO WS-FO-L12AHE (WS-SUB)
           MOVE WS-CUR-ITDAHP (WS-SUB) TO WS-FO-ITDAHP (WS-SUB)
           MOVE WS-CUR-ITDAHE (WS-SUB) TO WS-FO-ITDAHE (WS-SUB)
       END-PERFORM.
       
       WRITE FILE-OUT-REC FROM WS-FILE-OUT-RECORD.
       ADD +1 TO WS-OUT-CNT.
       
       2030-EXIT.
           EXIT.


       2040-MOVE-CUR-AMTS.
       
       MOVE WS-CUR-ME-DATE      TO WS-FO-ME-DATE.
       MOVE WS-CUR-CARRIER      TO WS-FO-CARRIER.
       MOVE WS-CUR-COMP         TO WS-FO-COMP.      
       MOVE WS-CUR-STATE        TO WS-FO-STATE.
       MOVE WS-CUR-ACCOUNT-NUM  TO WS-FO-ACCOUNT-NUM.
       MOVE WS-CUR-GROUP        TO WS-FO-GROUP.
       MOVE WS-CUR-EXP-DATE     TO WS-FO-EXP-DATE.
       MOVE WS-CUR-EFF-DATE     TO WS-FO-EFF-DATE.
       MOVE WS-CUR-ACCOUNT-NAME TO WS-FO-ACCOUNT-NAME.
       MOVE WS-CUR-PREM-P-E     TO WS-FO-PREM-P-E.
       MOVE WS-CUR-CLAIM-P-I    TO WS-FO-CLAIM-P-I.
       MOVE WS-CUR-RETRO-Y-N    TO WS-FO-RETRO-Y-N.
       MOVE WS-CUR-RETRO-P-E    TO WS-FO-RETRO-P-E.
       MOVE WS-CUR-TAX-P-E      TO WS-FO-TAX-P-E.
020409 MOVE WS-CUR-PERLF-RETRO-PMT TO WS-FO-CURLFRETROPMT.
020409 MOVE WS-DMY-PERLF-RETRO-PMT TO WS-FO-YTDLFRETROPMT.
020409 MOVE WS-DMY-PERLF-RETRO-PMT TO WS-FO-L12LFRETROPMT.
020409 MOVE WS-CUR-ITDLF-RETRO-PMT TO WS-FO-ITDLFRETROPMT.
020409 MOVE WS-CUR-PERAH-RETRO-PMT TO WS-FO-CURAHRETROPMT.
020409 MOVE WS-DMY-PERAH-RETRO-PMT TO WS-FO-YTDAHRETROPMT.
020409 MOVE WS-DMY-PERAH-RETRO-PMT TO WS-FO-L12AHRETROPMT.
020409 MOVE WS-CUR-ITDAH-RETRO-PMT TO WS-FO-ITDAHRETROPMT.

       MOVE +0 TO WS-SUB.
       PERFORM 9 TIMES
           ADD +1 TO WS-SUB
           MOVE WS-CUR-DESCR (WS-SUB) TO WS-FO-DESCR (WS-SUB)
           MOVE WS-CUR-PERLFP (WS-SUB) TO WS-FO-CURLFP (WS-SUB)
           MOVE WS-CUR-PERLFE (WS-SUB) TO WS-FO-CURLFE (WS-SUB)
           MOVE WS-DMY-PERLFP (WS-SUB) TO WS-FO-YTDLFP (WS-SUB)
           MOVE WS-DMY-PERLFE (WS-SUB) TO WS-FO-YTDLFE (WS-SUB)
           MOVE WS-DMY-PERLFP (WS-SUB) TO WS-FO-L12LFP (WS-SUB)
           MOVE WS-DMY-PERLFE (WS-SUB) TO WS-FO-L12LFE (WS-SUB)
           MOVE WS-CUR-ITDLFP (WS-SUB) TO WS-FO-ITDLFP (WS-SUB)
           MOVE WS-CUR-ITDLFE (WS-SUB) TO WS-FO-ITDLFE (WS-SUB)
           MOVE WS-CUR-PERAHP (WS-SUB) TO WS-FO-CURAHP (WS-SUB)
           MOVE WS-CUR-PERAHE (WS-SUB) TO WS-FO-CURAHE (WS-SUB)
           MOVE WS-DMY-PERAHP (WS-SUB) TO WS-FO-YTDAHP (WS-SUB)
           MOVE WS-DMY-PERAHE (WS-SUB) TO WS-FO-YTDAHE (WS-SUB)
           MOVE WS-DMY-PERAHP (WS-SUB) TO WS-FO-L12AHP (WS-SUB)
           MOVE WS-DMY-PERAHE (WS-SUB) TO WS-FO-L12AHE (WS-SUB)
           MOVE WS-CUR-ITDAHP (WS-SUB) TO WS-FO-ITDAHP (WS-SUB)
           MOVE WS-CUR-ITDAHE (WS-SUB) TO WS-FO-ITDAHE (WS-SUB)
       END-PERFORM.
       
       WRITE FILE-OUT-REC FROM WS-FILE-OUT-RECORD.
       ADD +1 TO WS-OUT-CNT.
       
       2040-EXIT.
           EXIT.


       2050-MOVE-YTD-L12-AMTS.
       
       MOVE WS-YTD-ME-DATE      TO WS-FO-ME-DATE.
       MOVE WS-YTD-CARRIER      TO WS-FO-CARRIER.
       MOVE WS-YTD-COMP         TO WS-FO-COMP.      
       MOVE WS-YTD-STATE        TO WS-FO-STATE.
       MOVE WS-YTD-ACCOUNT-NUM  TO WS-FO-ACCOUNT-NUM.
       MOVE WS-YTD-GROUP        TO WS-FO-GROUP.
       MOVE WS-YTD-EXP-DATE     TO WS-FO-EXP-DATE.
       MOVE WS-YTD-EFF-DATE     TO WS-FO-EFF-DATE.
       MOVE WS-YTD-ACCOUNT-NAME TO WS-FO-ACCOUNT-NAME.
       MOVE WS-YTD-PREM-P-E     TO WS-FO-PREM-P-E.
       MOVE WS-YTD-CLAIM-P-I    TO WS-FO-CLAIM-P-I.
       MOVE WS-YTD-RETRO-Y-N    TO WS-FO-RETRO-Y-N.
       MOVE WS-YTD-RETRO-P-E    TO WS-FO-RETRO-P-E.
       MOVE WS-YTD-TAX-P-E      TO WS-FO-TAX-P-E.
020409 MOVE WS-DMY-PERLF-RETRO-PMT TO WS-FO-CURLFRETROPMT.
020409 MOVE WS-YTD-PERLF-RETRO-PMT TO WS-FO-YTDLFRETROPMT.
020409 MOVE WS-L12-PERLF-RETRO-PMT TO WS-FO-L12LFRETROPMT.
020409 MOVE WS-YTD-ITDLF-RETRO-PMT TO WS-FO-ITDLFRETROPMT.
020409 MOVE WS-DMY-PERAH-RETRO-PMT TO WS-FO-CURAHRETROPMT.
020409 MOVE WS-YTD-PERAH-RETRO-PMT TO WS-FO-YTDAHRETROPMT.
020409 MOVE WS-L12-PERAH-RETRO-PMT TO WS-FO-L12AHRETROPMT.
020409 MOVE WS-YTD-ITDAH-RETRO-PMT TO WS-FO-ITDAHRETROPMT.

       MOVE +0 TO WS-SUB.
       PERFORM 9 TIMES
           ADD +1 TO WS-SUB
           MOVE WS-YTD-DESCR (WS-SUB) TO WS-FO-DESCR (WS-SUB)
           MOVE WS-DMY-PERLFP (WS-SUB) TO WS-FO-CURLFP (WS-SUB)
           MOVE WS-DMY-PERLFE (WS-SUB) TO WS-FO-CURLFE (WS-SUB)
           MOVE WS-YTD-PERLFP (WS-SUB) TO WS-FO-YTDLFP (WS-SUB)
           MOVE WS-YTD-PERLFE (WS-SUB) TO WS-FO-YTDLFE (WS-SUB)
           MOVE WS-L12-PERLFP (WS-SUB) TO WS-FO-L12LFP (WS-SUB)
           MOVE WS-L12-PERLFE (WS-SUB) TO WS-FO-L12LFE (WS-SUB)
           MOVE WS-YTD-ITDLFP (WS-SUB) TO WS-FO-ITDLFP (WS-SUB)
           MOVE WS-YTD-ITDLFE (WS-SUB) TO WS-FO-ITDLFE (WS-SUB)
           MOVE WS-DMY-PERAHP (WS-SUB) TO WS-FO-CURAHP (WS-SUB)
           MOVE WS-DMY-PERAHE (WS-SUB) TO WS-FO-CURAHE (WS-SUB)
           MOVE WS-YTD-PERAHP (WS-SUB) TO WS-FO-YTDAHP (WS-SUB)
           MOVE WS-YTD-PERAHE (WS-SUB) TO WS-FO-YTDAHE (WS-SUB)
           MOVE WS-L12-PERAHP (WS-SUB) TO WS-FO-L12AHP (WS-SUB)
           MOVE WS-L12-PERAHE (WS-SUB) TO WS-FO-L12AHE (WS-SUB)
           MOVE WS-YTD-ITDAHP (WS-SUB) TO WS-FO-ITDAHP (WS-SUB)
           MOVE WS-YTD-ITDAHE (WS-SUB) TO WS-FO-ITDAHE (WS-SUB)
       END-PERFORM.
       
       WRITE FILE-OUT-REC FROM WS-FILE-OUT-RECORD.
       ADD +1 TO WS-OUT-CNT.
      
       2050-EXIT.
           EXIT.



       2060-MOVE-YTD-AMTS.
       
       MOVE WS-YTD-ME-DATE      TO WS-FO-ME-DATE.
       MOVE WS-YTD-CARRIER      TO WS-FO-CARRIER.
       MOVE WS-YTD-COMP         TO WS-FO-COMP.      
       MOVE WS-YTD-STATE        TO WS-FO-STATE.
       MOVE WS-YTD-ACCOUNT-NUM  TO WS-FO-ACCOUNT-NUM.
       MOVE WS-YTD-GROUP        TO WS-FO-GROUP.
       MOVE WS-YTD-EXP-DATE     TO WS-FO-EXP-DATE.
       MOVE WS-YTD-EFF-DATE     TO WS-FO-EFF-DATE.
       MOVE WS-YTD-ACCOUNT-NAME TO WS-FO-ACCOUNT-NAME.
       MOVE WS-YTD-PREM-P-E     TO WS-FO-PREM-P-E.
       MOVE WS-YTD-CLAIM-P-I    TO WS-FO-CLAIM-P-I.
       MOVE WS-YTD-RETRO-Y-N    TO WS-FO-RETRO-Y-N.
       MOVE WS-YTD-RETRO-P-E    TO WS-FO-RETRO-P-E.
       MOVE WS-YTD-TAX-P-E      TO WS-FO-TAX-P-E.
020409 MOVE WS-DMY-PERLF-RETRO-PMT TO WS-FO-CURLFRETROPMT.
020409 MOVE WS-YTD-PERLF-RETRO-PMT TO WS-FO-YTDLFRETROPMT.
020409 MOVE WS-DMY-PERLF-RETRO-PMT TO WS-FO-L12LFRETROPMT.
020409 MOVE WS-YTD-ITDLF-RETRO-PMT TO WS-FO-ITDLFRETROPMT.
020409 MOVE WS-DMY-PERAH-RETRO-PMT TO WS-FO-CURAHRETROPMT.
020409 MOVE WS-YTD-PERAH-RETRO-PMT TO WS-FO-YTDAHRETROPMT.
020409 MOVE WS-DMY-PERAH-RETRO-PMT TO WS-FO-L12AHRETROPMT.
020409 MOVE WS-YTD-ITDAH-RETRO-PMT TO WS-FO-ITDAHRETROPMT.

       MOVE +0 TO WS-SUB.
       PERFORM 9 TIMES
           ADD +1 TO WS-SUB
           MOVE WS-YTD-DESCR (WS-SUB) TO WS-FO-DESCR (WS-SUB)
           MOVE WS-DMY-PERLFP (WS-SUB) TO WS-FO-CURLFP (WS-SUB)
           MOVE WS-DMY-PERLFE (WS-SUB) TO WS-FO-CURLFE (WS-SUB)
           MOVE WS-YTD-PERLFP (WS-SUB) TO WS-FO-YTDLFP (WS-SUB)
           MOVE WS-YTD-PERLFE (WS-SUB) TO WS-FO-YTDLFE (WS-SUB)
           MOVE WS-DMY-PERLFP (WS-SUB) TO WS-FO-L12LFP (WS-SUB)
           MOVE WS-DMY-PERLFE (WS-SUB) TO WS-FO-L12LFE (WS-SUB)
           MOVE WS-YTD-ITDLFP (WS-SUB) TO WS-FO-ITDLFP (WS-SUB)
           MOVE WS-YTD-ITDLFE (WS-SUB) TO WS-FO-ITDLFE (WS-SUB)
           MOVE WS-DMY-PERAHP (WS-SUB) TO WS-FO-CURAHP (WS-SUB)
           MOVE WS-DMY-PERAHE (WS-SUB) TO WS-FO-CURAHE (WS-SUB)
           MOVE WS-YTD-PERAHP (WS-SUB) TO WS-FO-YTDAHP (WS-SUB)
           MOVE WS-YTD-PERAHE (WS-SUB) TO WS-FO-YTDAHE (WS-SUB)
           MOVE WS-DMY-PERAHP (WS-SUB) TO WS-FO-L12AHP (WS-SUB)
           MOVE WS-DMY-PERAHE (WS-SUB) TO WS-FO-L12AHE (WS-SUB)
           MOVE WS-YTD-ITDAHP (WS-SUB) TO WS-FO-ITDAHP (WS-SUB)
           MOVE WS-YTD-ITDAHE (WS-SUB) TO WS-FO-ITDAHE (WS-SUB)
       END-PERFORM.
       
       WRITE FILE-OUT-REC FROM WS-FILE-OUT-RECORD.
       ADD +1 TO WS-OUT-CNT.
       
       2060-EXIT.
           EXIT.


           EJECT
