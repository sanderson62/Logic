       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCRF3.
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
       77  FILLER  PIC X(32) VALUE '   PEMCRF3   WORKING-STORAGE    '.          
       77  FILLER  PIC X(32) VALUE '********************************'.          

       77  WS-EOF-SW               PIC X VALUE SPACES.                          
           88  END-OF-INPUT              VALUE 'Y'.
       77  CERT-RECS-IN            PIC 9(9) VALUE ZEROS.                        
       77  CERT-RECS-OUT           PIC 9(9) VALUE ZEROS.                        
       77  CERT-RECS-FIX           PIC 9(9) VALUE ZEROS.                        
       77  ID-ISS-RECS             PIC 9(9) VALUE ZEROS.
       77  ID-REF-RECS             PIC 9(9) VALUE ZEROS.
       77  NM-ISS-RECS             PIC 9(9) VALUE ZEROS.
       77  NM-REF-RECS             PIC 9(9) VALUE ZEROS.
       77  NH-ISS-RECS             PIC 9(9) VALUE ZEROS.
       77  NH-REF-RECS             PIC 9(9) VALUE ZEROS.
       77  MN-ISS-RECS             PIC 9(9) VALUE ZEROS.
       77  MN-REF-RECS             PIC 9(9) VALUE ZEROS.
       77  WI-ISS-RECS             PIC 9(9) VALUE ZEROS.
       77  WI-REF-RECS             PIC 9(9) VALUE ZEROS.
       77  LA-ISS-RECS             PIC 9(9) VALUE ZEROS.
       77  LA-REF-RECS             PIC 9(9) VALUE ZEROS.
       77  WS-DIS-DATE             PIC X(10)  VALUE SPACES.
       77  A1                      PIC S999 COMP-3 VALUE +0.
       77  WS-LF-ISS-TAX           PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-AH-ISS-TAX           PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-LF-REF-TAX           PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-AH-REF-TAX           PIC S9(7)V99 COMP-3 VALUE +0.
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
           DISPLAY ' ID ISS COVERAGES     ' ID-ISS-RECS
           DISPLAY ' MN ISS COVERAGES     ' MN-ISS-RECS
           DISPLAY ' NH ISS COVERAGES     ' NH-ISS-RECS
           DISPLAY ' ID REF COVERAGES     ' ID-REF-RECS
           DISPLAY ' MN REF COVERAGES     ' MN-REF-RECS
           DISPLAY ' NH REF COVERAGES     ' NH-REF-RECS
           DISPLAY ' WI ISS COVERAGES     ' WI-ISS-RECS
           DISPLAY ' WI REF COVERAGES     ' WI-REF-RECS
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
       0100-PROCESS-CERT.                                                       
                                                                                
      *    IF (CR-CARRIER = '1')
      *       AND (CR-ACCOUNT = '0005500120')
      *       PERFORM 0110-FIX-CERT    THRU 0110-EXIT
      *    END-IF

           IF (CR-STATE = 'LA')
                   OR
              ((CR-STATE = 'WI')
              AND (CR-ACCOUNT > '0900320000')
              AND (CR-ACCOUNT < '0900320016'))
              PERFORM 0110-FIX-CERT    THRU 0110-EXIT
           END-IF

      *    IF (CR-STATE = 'WI')
      *       AND (CR-DT > 20061231)
      *       PERFORM 0110-FIX-CERT    THRU 0110-EXIT
      *    END-IF

      *    IF (CR-STATE = 'ID' OR 'MN' OR 'NH')
      *       AND (CR-DT > 20081231)
      *       PERFORM 0110-FIX-CERT    THRU 0110-EXIT
      *    END-IF


      *    IF (CR-STATE = 'NM')
      *       AND (CR-ACCOUNT = '0900866001' OR '0900866002')
      *       AND (CR-DT > 20061231)
      *       PERFORM 0110-FIX-CERT    THRU 0110-EXIT
      *    END-IF

      *    IF ((CR-STATE = 'AK' OR 'AL' OR 'AZ' OR 'AR' OR 'CO' OR 'CT'
      *               OR 'DC' OR 'FL' OR 'GA' OR 'GU' OR 'HI' OR 'ID'
      *               OR 'IA' OR 'IN' OR 'IL' OR 'KY' OR 'LA' OR 'ME'
      *               OR 'MD' OR 'MN' OR 'MO' OR 'MS' OR 'MT' OR 'NE'
      *               OR 'NH' OR 'NJ' OR 'NC' OR 'ND' OR 'NV' OR 'OH'
      *               OR 'RI' OR 'SC' OR 'SD' OR 'TN' OR 'TX' OR 'VT'
      *               OR 'WA' OR 'WI' OR 'WY')
      *       AND (CR-DT > 20051231))
      *                     OR
      *       ((CR-STATE = 'PA')
      *       AND (CR-DT > 20061231))
      *       PERFORM 0110-FIX-CERT    THRU 0110-EXIT
      *    END-IF

      *    PERFORM 0110-FIX-CERT       THRU 0110-EXIT

      *    IF (CR-DT > 19971231)
      *       AND (CR-STATE = 'AK' OR 'AL' OR 'AR' OR 'GA' OR 'HI'
      *                    OR 'NV' OR 'NC' OR 'SC' OR 'WA')
      *       PERFORM 0110-FIX-CERT    THRU 0110-EXIT
      *    END-IF

      *    IF (CR-STATE = 'WY')
      *       AND (CR-ACCOUNT = '0001146900')
      *       AND (CR-DT >= 20080526)
      *       DISPLAY ' ABOUT TO CHG REIN TBL ON ' CR-STATE
      *          ' ' CR-ACCOUNT ' ' CR-CERT-NO ' FROM ' CR-REIN-TABLE
      *          ' TO 0V9'
      *       MOVE '0V9'               TO CR-REIN-TABLE
      *    END-IF
           PERFORM 0300-WRITE-CERT     THRU 0300-EXIT
           PERFORM 0200-CERT-READ      THRU 0200-EXIT

           .                                                                    
       0100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0110-FIX-CERT.

           IF (CR-STATE = 'LA')
              AND (CR-DT > 20061231)
              IF CR-LFPRM > ZEROS
                 AND CR-LF-ISS-PREM-TAX NOT = +.0225
                 ADD 1                 TO LA-ISS-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF ISS FROM ' CR-LF-ISS-PREM-TAX
                 ' TO +.0225 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0225           TO CR-LF-ISS-PREM-TAX
              END-IF
              IF CR-AHPRM > ZEROS
                 AND CR-AH-ISS-PREM-TAX NOT = +.0225
                 ADD 1                 TO LA-ISS-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH ISS FROM ' CR-AH-ISS-PREM-TAX
                 ' TO +.0225 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0225           TO CR-AH-ISS-PREM-TAX
              END-IF
              IF (CR-LFRFND > ZEROS)
                 AND (CR-LF-CNC-PREM-TAX NOT = +.0225)
                 ADD 1                 TO LA-REF-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF REF FROM ' CR-LF-CNC-PREM-TAX
                    ' TO +.0225 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0225           TO CR-LF-CNC-PREM-TAX
              END-IF
              IF (CR-AHRFND > ZEROS)
                 AND (CR-AH-CNC-PREM-TAX NOT = +.0225)
                 ADD 1                 TO LA-REF-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH REF FROM ' CR-AH-CNC-PREM-TAX
                    ' TO +.0225 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0225           TO CR-AH-CNC-PREM-TAX
              END-IF
           END-IF
       
           IF (CR-STATE = 'WI')
              AND (CR-DT > 20051231)
              AND (CR-DT < 20070101)
              IF (CR-LFPRM > ZEROS)
                 AND (CR-LF-ISS-PREM-TAX NOT = +.0200)
                 ADD 1                 TO WI-ISS-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF ISS FROM ' CR-LF-ISS-PREM-TAX
                 ' TO +.0200 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0200           TO CR-LF-ISS-PREM-TAX
                 COMPUTE WS-LF-ISS-TAX = WS-LF-ISS-TAX +
                    (CR-LF-ISS-PREM-TAX * (CR-LFPRM + CR-LFPRM-ALT))
              END-IF
              IF (CR-AHPRM > ZEROS)
                 AND (CR-AH-ISS-PREM-TAX NOT = +.0084)
                 ADD 1                 TO WI-ISS-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH ISS FROM ' CR-AH-ISS-PREM-TAX
                 ' TO +.0084 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0084           TO CR-AH-ISS-PREM-TAX
                 COMPUTE WS-AH-ISS-TAX = WS-AH-ISS-TAX +
                    (CR-AH-ISS-PREM-TAX * CR-AHPRM)
              END-IF
              IF (CR-LFRFND > ZEROS)
                 AND (CR-LF-CNC-PREM-TAX NOT = +.0200)
                 ADD 1                 TO WI-REF-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF REF FROM ' CR-LF-CNC-PREM-TAX
                    ' TO +.0200 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0200           TO CR-LF-CNC-PREM-TAX
                 COMPUTE WS-LF-REF-TAX = WS-LF-REF-TAX +
                    (CR-LF-CNC-PREM-TAX * CR-LFRFND)
              END-IF
              IF (CR-AHRFND > ZEROS)
                 AND (CR-AH-CNC-PREM-TAX NOT = +.0084)
                 ADD 1                 TO WI-REF-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH REF FROM ' CR-AH-CNC-PREM-TAX
                    ' TO +.0084 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0084           TO CR-AH-CNC-PREM-TAX
                 COMPUTE WS-AH-REF-TAX = WS-AH-REF-TAX +
                    (CR-AH-CNC-PREM-TAX * CR-AHRFND)
              END-IF
           END-IF
       
           IF (CR-STATE = 'WI')
              AND (CR-DT > 20061231)
              AND (CR-DT < 20090101)
              IF (CR-LFPRM > ZEROS)
                 AND (CR-LF-ISS-PREM-TAX NOT = +.0200)
                 ADD 1                 TO WI-ISS-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF ISS FROM ' CR-LF-ISS-PREM-TAX
                 ' TO +.0200 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0200           TO CR-LF-ISS-PREM-TAX
                 COMPUTE WS-LF-ISS-TAX = WS-LF-ISS-TAX +
                    (CR-LF-ISS-PREM-TAX * (CR-LFPRM + CR-LFPRM-ALT))
              END-IF
              IF (CR-AHPRM > ZEROS)
                 AND (CR-AH-ISS-PREM-TAX NOT = +.0071)
                 ADD 1                 TO WI-ISS-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH ISS FROM ' CR-AH-ISS-PREM-TAX
                 ' TO +.0071 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0071           TO CR-AH-ISS-PREM-TAX
                 COMPUTE WS-AH-ISS-TAX = WS-AH-ISS-TAX +
                    (CR-AH-ISS-PREM-TAX * CR-AHPRM)
              END-IF
              IF (CR-LFRFND > ZEROS)
                 AND (CR-LF-CNC-PREM-TAX NOT = +.0200)
                 ADD 1                 TO WI-REF-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING LF REF FROM ' CR-LF-CNC-PREM-TAX
                    ' TO +.0200 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0200           TO CR-LF-CNC-PREM-TAX
                 COMPUTE WS-LF-REF-TAX = WS-LF-REF-TAX +
                    (CR-LF-CNC-PREM-TAX * CR-LFRFND)
              END-IF
              IF (CR-AHRFND > ZEROS)
                 AND (CR-AH-CNC-PREM-TAX NOT = +.0071)
                 ADD 1                 TO WI-REF-RECS
                 MOVE CR-DT            TO WS-DISPLAY-DT
                 DISPLAY ' CHANGING AH REF FROM ' CR-AH-CNC-PREM-TAX
                    ' TO +.0071 ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 ' ' WS-DISPLAY-DT
                 MOVE +.0071           TO CR-AH-CNC-PREM-TAX
                 COMPUTE WS-AH-REF-TAX = WS-AH-REF-TAX +
                    (CR-AH-CNC-PREM-TAX * CR-AHRFND)
              END-IF
           END-IF

      *    IF ((CR-LFTYP NOT = '00' AND '  ')
      *       AND (CR-LF-CANCEL-EXIT-DATE = ZEROS))
      *                      OR
      *       ((CR-AHTYP NOT = '00' AND '  ')
      *       AND (CR-AH-CANCEL-EXIT-DATE = ZEROS))
      *       ADD 1 TO CERT-RECS-FIX
      *    END-IF
      *
      *
      *    IF CR-LFTYP NOT = '00' AND '  '
      *       IF CR-LF-CANCEL-EXIT-DATE = ZEROS
      *          MOVE 20090831         TO CR-LF-CANCEL-EXIT-DATE
      *          MOVE 20070801         TO CR-LF-CANC-DT
      *          MOVE CR-LF-CURRENT-STATUS
      *                                TO CR-LF-STATUS-AT-CANCEL
      *          MOVE '8'              TO CR-LF-CURRENT-STATUS
      *          MOVE 'AUTO'           TO CR-LF-EXIT-BATCH
      *          MOVE CR-DT            TO WS-DISPLAY-DT
      *          DISPLAY ' CANCELING LF COVERAGE ON ' CR-ACCT-CONTROL
      *          ' ' WS-DISPLAY-DT ' ' CR-CERT-NO
      *       END-IF
      *    END-IF
      *
      *    IF CR-AHTYP NOT = '00' AND '  '
      *       IF CR-AH-CANCEL-EXIT-DATE = ZEROS
      *          MOVE 20090831         TO CR-AH-CANCEL-EXIT-DATE
      *          MOVE 20070801         TO CR-AH-CANC-DT
      *          MOVE CR-AH-CURRENT-STATUS
      *                                TO CR-AH-STATUS-AT-CANCEL
      *          MOVE '8'              TO CR-AH-CURRENT-STATUS
      *          MOVE 'AUTO'           TO CR-AH-EXIT-BATCH
      *          MOVE CR-DT            TO WS-DISPLAY-DT
      *          DISPLAY ' CANCELING AH COVERAGE ON ' CR-ACCT-CONTROL
      *          ' ' WS-DISPLAY-DT ' ' CR-CERT-NO
      *       END-IF
      *    END-IF

      *    IF (CR-STATE = 'ID')
      *       IF (CR-LFPRM > ZEROS)
      *          AND (CR-LF-ISS-PREM-TAX NOT = +.0170)
      *          ADD 1                 TO ID-ISS-RECS
      *          DISPLAY ' CHANGING LF ISS FROM ' CR-LF-ISS-PREM-TAX
      *             ' TO +.0170 ' CR-STATE ' ' CR-CERT-NO
      *          MOVE +.0170           TO CR-LF-ISS-PREM-TAX
      *       END-IF
      *       IF (CR-AHPRM > ZEROS)
      *          AND (CR-AH-ISS-PREM-TAX NOT = +.0170)
      *          ADD 1                 TO ID-ISS-RECS
      *          DISPLAY ' CHANGING AH ISS FROM ' CR-AH-ISS-PREM-TAX
      *             ' TO +.0170 ' CR-STATE ' ' CR-CERT-NO
      *          MOVE +.0170           TO CR-AH-ISS-PREM-TAX
      *       END-IF
      *       IF (CR-LFRFND > ZEROS)
      *          AND (CR-LF-CNC-PREM-TAX NOT = +.0170)
      *          ADD 1                 TO ID-REF-RECS
      *          DISPLAY ' CHANGING LF REF FROM ' CR-LF-CNC-PREM-TAX
      *             ' TO +.0170 ' CR-STATE ' ' CR-CERT-NO
      *          MOVE +.0170           TO CR-LF-CNC-PREM-TAX
      *       END-IF
      *       IF (CR-AHRFND > ZEROS)
      *          AND (CR-AH-CNC-PREM-TAX NOT = +.0170)
      *          ADD 1                 TO ID-REF-RECS
      *          DISPLAY ' CHANGING AH REF FROM ' CR-AH-CNC-PREM-TAX
      *             ' TO +.0170 ' CR-STATE ' ' CR-CERT-NO
      *          MOVE +.0170           TO CR-AH-CNC-PREM-TAX
      *       END-IF
      *    END-IF
      *
      *    IF (CR-STATE = 'MN')
      *       IF (CR-LFPRM > ZEROS)
      *          AND (CR-LF-ISS-PREM-TAX NOT = +.0150)
      *          ADD 1                 TO MN-ISS-RECS
      *          DISPLAY ' CHANGING LF ISS FROM ' CR-LF-ISS-PREM-TAX
      *             ' TO +.0150 ' CR-STATE ' ' CR-CERT-NO
      *          MOVE +.0150           TO CR-LF-ISS-PREM-TAX
      *       END-IF
      *       IF (CR-AHPRM > ZEROS)
      *          AND (CR-AH-ISS-PREM-TAX NOT = +.0200)
      *          DISPLAY ' FOUND MN ISS MISTAKES ' CR-AH-ISS-PREM-TAX
      *          ADD 1                 TO MN-ISS-RECS
      *          MOVE +.0200           TO CR-AH-ISS-PREM-TAX
      *       END-IF
      *       IF (CR-LFRFND > ZEROS)
      *          AND (CR-LF-CNC-PREM-TAX NOT = +.0150)
      *          ADD 1                 TO MN-REF-RECS
      *          DISPLAY ' CHANGING LF REF FROM ' CR-LF-CNC-PREM-TAX
      *             ' TO +.0150 ' CR-STATE ' ' CR-CERT-NO
      *          MOVE +.0150           TO CR-LF-CNC-PREM-TAX
      *       END-IF
      *       IF (CR-AHRFND > ZEROS)
      *          AND (CR-AH-CNC-PREM-TAX NOT = +.0200)
      *          DISPLAY ' FOUND MN CNC MISTAKES ' CR-AH-CNC-PREM-TAX
      *          ADD 1                 TO MN-REF-RECS
      *          MOVE +.0200           TO CR-AH-CNC-PREM-TAX
      *       END-IF
      *    END-IF
      *
      *    IF (CR-STATE = 'NH')
      *       IF (CR-LFPRM > ZEROS)
      *          AND (CR-LF-ISS-PREM-TAX NOT = +.0150)
      *          ADD 1                 TO NH-ISS-RECS
      *          DISPLAY ' CHANGING LF ISS FROM ' CR-LF-ISS-PREM-TAX
      *             ' TO +.0150 ' CR-STATE ' ' CR-CERT-NO
      *          MOVE +.0150           TO CR-LF-ISS-PREM-TAX
      *       END-IF
      *       IF (CR-AHPRM > ZEROS)
      *          AND (CR-AH-ISS-PREM-TAX NOT = +.0200)
      *          DISPLAY ' FOUND NH ISS MISTAKES ' CR-AH-ISS-PREM-TAX
      *          ADD 1                 TO NH-ISS-RECS
      *          MOVE +.0200           TO CR-AH-ISS-PREM-TAX
      *       END-IF
      *       IF (CR-LFRFND > ZEROS)
      *          AND (CR-LF-CNC-PREM-TAX NOT = +.0150)
      *          ADD 1                 TO NH-REF-RECS
      *          DISPLAY ' CHANGING LF REF FROM ' CR-LF-CNC-PREM-TAX
      *             ' TO +.0150 ' CR-STATE ' ' CR-CERT-NO
      *          MOVE +.0150           TO CR-LF-CNC-PREM-TAX
      *       END-IF
      *       IF (CR-AHRFND > ZEROS)
      *          AND (CR-AH-CNC-PREM-TAX NOT = +.0200)
      *          DISPLAY ' FOUND NH CNC MISTAKES ' CR-AH-CNC-PREM-TAX
      *          ADD 1                 TO NH-REF-RECS
      *          MOVE +.0200           TO CR-AH-CNC-PREM-TAX
      *       END-IF
      *    END-IF

      *    IF (CR-STATE = 'WI')
      *       DISPLAY ' FOUND CERT ' CR-ACCOUNT '  ' CR-CERT-NO
      *       IF CR-LFPRM > ZEROS
      *          ADD 1                 TO ID-ISS-RECS
      *          MOVE +.0200           TO CR-LF-ISS-PREM-TAX
      *       END-IF
      *       IF CR-AHPRM > ZEROS
      *          MOVE +.0071           TO CR-AH-ISS-PREM-TAX
      *       END-IF
      *       IF CR-LFRFND > ZEROS
      *          ADD 1                 TO ID-REF-RECS
      *          MOVE +.0200           TO CR-LF-CNC-PREM-TAX
      *       END-IF
      *       IF CR-AHRFND > ZEROS
      *          MOVE +.0071           TO CR-AH-CNC-PREM-TAX
      *       END-IF
      *    END-IF

      *    IF (CR-STATE = 'NM')
      *       DISPLAY ' FOUND CERT ' CR-ACCOUNT '  ' CR-CERT-NO
      *       IF CR-AHPRM > ZEROS
      *          ADD 1                 TO NM-ISS-RECS
      *          MOVE +.0300           TO CR-AH-ISS-PREM-TAX
      *       END-IF
      *       IF CR-AHRFND > ZEROS
      *          ADD 1                 TO NM-REF-RECS
      *          MOVE +.0300           TO CR-AH-CNC-PREM-TAX
      *       END-IF
      *    END-IF

      *    IF (CR-STATE = 'NH')
      *       DISPLAY ' FOUND CERT ' CR-ACCOUNT '  ' CR-CERT-NO
      *       IF CR-LFPRM > ZEROS
      *          ADD 1                 TO NH-ISS-RECS
      *          MOVE +.0175           TO CR-LF-ISS-PREM-TAX
      *       END-IF
      *       IF CR-LFRFND > ZEROS
      *          ADD 1                 TO NH-REF-RECS
      *          MOVE +.0175           TO CR-LF-CNC-PREM-TAX
      *       END-IF
      *    END-IF

      *    IF (CR-STATE = 'ID')
      *       DISPLAY ' FOUND CERT ' CR-ACCOUNT '  ' CR-CERT-NO
      *       IF CR-LFPRM > ZEROS
      *          ADD 1                 TO ID-ISS-RECS
      *          MOVE +.0190           TO CR-LF-ISS-PREM-TAX
      *       END-IF
      *       IF CR-AHPRM > ZEROS
      *          MOVE +.0190           TO CR-AH-ISS-PREM-TAX
      *       END-IF
      *       IF CR-LFRFND > ZEROS
      *          ADD 1                 TO ID-REF-RECS
      *          MOVE +.0190           TO CR-LF-CNC-PREM-TAX
      *       END-IF
      *       IF CR-AHRFND > ZEROS
      *          MOVE +.0190           TO CR-AH-CNC-PREM-TAX
      *       END-IF
      *    END-IF

      *    IF CR-LFPRM > ZEROS
      *       MOVE +.03                TO CR-LF-ISS-PREM-TAX
      *    END-IF
      *    IF CR-AHPRM > ZEROS
      *       MOVE +.01                TO CR-AH-ISS-PREM-TAX
      *    END-IF
      *    IF CR-LFRFND > ZEROS
      *       MOVE +.03                TO CR-LF-CNC-PREM-TAX
      *    END-IF
      *    IF CR-AHRFND > ZEROS
      *       MOVE +.01                TO CR-AH-CNC-PREM-TAX
      *    END-IF

      *    IF CR-STATE = 'KY'
      *       DISPLAY ' FOUND KY CERT ' CR-CERT-NO
      *       MOVE +0                  TO CR-LF-ISS-PREM-TAX
      *                                   CR-LF-CNC-PREM-TAX
      *                                   CR-AH-ISS-PREM-TAX
      *                                   CR-AH-CNC-PREM-TAX
      *    END-IF

      *    IF (CR-STATE = 'KS')
      *       AND (CR-DT > 20080117)
      *       AND (CR-LFTYP NOT = SPACES AND ZEROS)
      *       MOVE CR-DT               TO WS-WORK-DATE
      *       STRING WS-WORK-DATE (5:2) '-' WS-WORK-DATE (7:2) '-'
      *          WS-WORK-DATE (1:4) DELIMITED BY SIZE INTO WS-DIS-DATE
      *       END-STRING
      *       DISPLAY ' MORT  FIX ' CR-STATE ' ' CR-ACCOUNT ' '
      *          WS-DIS-DATE ' ' CR-CERT-NO ' FROM ' CR-MORT ' TO L010 '
      *       MOVE 'L010'              TO CR-MORT
      *       ADD 1                    TO CERT-RECS-FIX
      *    END-IF

      *    EVALUATE TRUE
      *       WHEN (CR-STATE = 'TN')
      *          AND (CR-ACCOUNT NOT = '0900091033' AND '0900733033'
      *              AND '0900733034' AND '0990000329' AND '0990000330'
      *              AND '0990000334' AND '0990000335' AND '0990000336'
      *              AND '0990000337' AND '0990000391' AND '0990000407'
      *              AND '0997329001' AND '0997329002' AND '0997416001'
      *              AND '0997464002')
      *          PERFORM VARYING A1 FROM +2 BY +1 UNTIL
      *             A1 > +10
      *             IF CR-COM-AGT (A1) = '0000957900'
      *                MOVE CR-DT      TO WS-WORK-DATE
      *                STRING WS-WORK-DATE (5:2) '-' WS-WORK-DATE (7:2)
      *                 '-' WS-WORK-DATE (1:4) DELIMITED BY SIZE
      *                    INTO WS-DIS-DATE
      *                END-STRING
      *                DISPLAY '  TN   FIX ' CR-STATE ' ' CR-ACCOUNT ' '
      *                   WS-DIS-DATE ' ' CR-CERT-NO 
      *                MOVE '0000957700'  TO CR-COM-AGT (A1)
      *             END-IF
      *          END-PERFORM
      *       WHEN (CR-STATE = 'NM')
      *          PERFORM VARYING A1 FROM +2 BY +1 UNTIL
      *             A1 > +10
      *             IF CR-COM-AGT (A1) = '0000810200'
      *                MOVE CR-DT      TO WS-WORK-DATE
      *                STRING WS-WORK-DATE (5:2) '-' WS-WORK-DATE (7:2)
      *                 '-' WS-WORK-DATE (1:4) DELIMITED BY SIZE
      *                    INTO WS-DIS-DATE
      *                END-STRING
      *                DISPLAY '  NM   FIX ' CR-STATE ' ' CR-ACCOUNT ' '
      *                   WS-DIS-DATE ' ' CR-CERT-NO 
      *                MOVE '0000628900'  TO CR-COM-AGT (A1)
      *             END-IF
      *          END-PERFORM
      *       WHEN (CR-STATE = 'FL')
      *          PERFORM VARYING A1 FROM +2 BY +1 UNTIL
      *             A1 > +10
      *             IF CR-COM-AGT (A1) = '0000957700'
      *                MOVE CR-DT      TO WS-WORK-DATE
      *                STRING WS-WORK-DATE (5:2) '-' WS-WORK-DATE (7:2)
      *                 '-' WS-WORK-DATE (1:4) DELIMITED BY SIZE
      *                    INTO WS-DIS-DATE
      *                END-STRING
      *                DISPLAY '  FL   FIX ' CR-STATE ' ' CR-ACCOUNT ' '
      *                   WS-DIS-DATE ' ' CR-CERT-NO 
      *                MOVE '0000907200' TO CR-COM-AGT (A1)
      *             END-IF
      *          END-PERFORM
      *       WHEN (CR-STATE = 'IN' OR 'MI')
      *          PERFORM VARYING A1 FROM +2 BY +1 UNTIL
      *             A1 > +10
      *             IF CR-COM-AGT (A1) = '0000957900'
      *                MOVE CR-DT      TO WS-WORK-DATE
      *                STRING WS-WORK-DATE (5:2) '-' WS-WORK-DATE (7:2)
      *                 '-' WS-WORK-DATE (1:4) DELIMITED BY SIZE
      *                    INTO WS-DIS-DATE
      *                END-STRING
      *                DISPLAY ' IN MI FIX ' CR-STATE ' ' CR-ACCOUNT ' '
      *                   WS-DIS-DATE ' ' CR-CERT-NO 
      *                MOVE '0000907100' TO CR-COM-AGT (A1)
      *             END-IF
      *          END-PERFORM
      *       WHEN (CR-STATE = 'KY')
      *          PERFORM VARYING A1 FROM +2 BY +1 UNTIL
      *             A1 > +10
      *             IF CR-COM-AGT (A1) = '0000957901'
      *                MOVE CR-DT      TO WS-WORK-DATE
      *                STRING WS-WORK-DATE (5:2) '-' WS-WORK-DATE (7:2)
      *                 '-' WS-WORK-DATE (1:4) DELIMITED BY SIZE
      *                    INTO WS-DIS-DATE
      *                END-STRING
      *                DISPLAY ' KY    FIX ' CR-STATE ' ' CR-ACCOUNT ' '
      *                   WS-DIS-DATE ' ' CR-CERT-NO 
      *                MOVE '0000907101' TO CR-COM-AGT (A1)
      *             END-IF
      *          END-PERFORM
      *       WHEN (CR-STATE = 'NC' OR 'SC')
      *          PERFORM VARYING A1 FROM +2 BY +1 UNTIL
      *             A1 > +10
      *             IF CR-COM-AGT (A1) = '0000957800'
      *                MOVE CR-DT      TO WS-WORK-DATE
      *                STRING WS-WORK-DATE (5:2) '-' WS-WORK-DATE (7:2)
      *                 '-' WS-WORK-DATE (1:4) DELIMITED BY SIZE
      *                    INTO WS-DIS-DATE
      *                END-STRING
      *                DISPLAY ' NC SC FIX ' CR-STATE ' ' CR-ACCOUNT ' '
      *                   WS-DIS-DATE ' ' CR-CERT-NO 
      *                MOVE '0000957700' TO CR-COM-AGT (A1)
      *             END-IF
      *          END-PERFORM
      *    END-EVALUATE

      *    EVALUATE CR-STATE
      *       WHEN 'AK'
      *          MOVE 'L100'           TO CR-MORT
      *          DISPLAY ' FIXED CERT ' CR-CERT-NO '  ' CR-MORT
      *       WHEN 'AL' 
      *          MOVE 'L150'           TO CR-MORT
      *          DISPLAY ' FIXED CERT ' CR-CERT-NO '  ' CR-MORT
      *       WHEN 'AR' 
      *          MOVE 'L170'           TO CR-MORT
      *          DISPLAY ' FIXED CERT ' CR-CERT-NO '  ' CR-MORT
      *       WHEN 'GA' 
      *          MOVE 'L140'           TO CR-MORT
      *          DISPLAY ' FIXED CERT ' CR-CERT-NO '  ' CR-MORT
      *       WHEN 'HI' 
      *          MOVE 'L150'           TO CR-MORT
      *          DISPLAY ' FIXED CERT ' CR-CERT-NO '  ' CR-MORT
      *       WHEN 'NV' 
      *          MOVE 'L140'           TO CR-MORT
      *          DISPLAY ' FIXED CERT ' CR-CERT-NO '  ' CR-MORT
      *       WHEN 'NC' 
      *          MOVE 'L150'           TO CR-MORT
      *          DISPLAY ' FIXED CERT ' CR-CERT-NO '  ' CR-MORT
      *       WHEN 'SC' 
      *          MOVE 'L140'           TO CR-MORT
      *          DISPLAY ' FIXED CERT ' CR-CERT-NO '  ' CR-MORT
      *       WHEN 'WA' 
      *          MOVE 'L140'           TO CR-MORT
      *          DISPLAY ' FIXED CERT ' CR-CERT-NO '  ' CR-MORT
      *       WHEN OTHER
      *          DISPLAY ' I SHOULDN''T BE HERE ' CR-CERT-NO
      *    END-EVALUATE

      *    IF CR-MOB-NET-TOT-FEES = 105277.71
      *       DISPLAY ' FOUND AMOUNT ' CR-MOB-NET-TOT-FEES
      *       MOVE 5277.71             TO CR-MOB-NET-TOT-FEES
      *       ADD 1                    TO CERT-RECS-FIX
      *    END-IF

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
                                                                                
