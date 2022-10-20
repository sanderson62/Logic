       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMBLC1.
      *AUTHOR.     PABLO.
      *REMARKS.
      * THIS PROGRAM READS ERCOBI BACKUPS AND CONVERTS THE OLD FORMAT
      * TO THE NEW FORMAT. THE NEXT STEP SHOULD BE LOADING THE OUTPUT
      * OF THIS PROGRAM TO A VSAM FILE.       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT FILE-IN          ASSIGN TO SYS010.
           
           SELECT FILE-OUT         ASSIGN TO SYS011.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE                   COPY ELCDTEFD.

       FD  FILE-IN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.

00022  01  FILE-IN-RECORD.
00023      12  IN-RECORD-ID                          PIC XX.
00024          88  VALID-BL-ID                          VALUE 'BL'.

00026      12  IN-CONTROL-PRIMARY.
00027          16  IN-COMPANY-CD                     PIC X.
00028          16  IN-STMT-OWNER                     PIC X(4).
00031          16  IN-REPORT-GROUP-ID                PIC X(12).

00047      12  IN-MAINT-INFORMATION.
00048          16  IN-LAST-MAINT-DT                  PIC XX.
00049          16  IN-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  IN-LAST-MAINT-USER                PIC X(4).
00051          16  FILLER                            PIC X(10).

           12  IN-CONTACT-NAME                       PIC X(35).
           12  IN-ADDR1                              PIC X(30).
           12  IN-ADDR2                              PIC X(30).
           12  IN-CITY                               PIC X(30).
           12  IN-STATE                              PIC XX.
00074      12  IN-ZIP.
00075          16  IN-ZIP-PRIME.
00076              20  IN-ZIP-PRI-1ST                PIC X.
00077                  88  IN-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  IN-ZIP-PLUS4                      PIC X(4).
00080      12  IN-CANADIAN-POSTAL-CODE  REDEFINES  IN-ZIP.
00081          16  IN-CAN-POSTAL-1                   PIC XXX.
00082          16  IN-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
           12  FILLER                                PIC X(30).
           12  IN-CHECK-HANDLING                     PIC X.
               88  IN-CHECKS-NET                VALUE '1' ' '.
               88  IN-CHECKS-SEPARATE           VALUE '2'.
           12  IN-SPECIAL-INSTRUCTIONS.
               16  IN-SI-LINE-1                      PIC X(70).
               16  IN-SI-LINE-2                      PIC X(70).
               16  IN-SI-LINE-3                      PIC X(70).
               16  IN-SI-LINE-4                      PIC X(70).
               16  IN-SI-LINE-5                      PIC X(70).
           12  FILLER                                PIC X(64).

       FD  FILE-OUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.

       01  FILE-OUT-RECORD               PIC X(620).


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

                                       COPY ERCCOBI.

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

           DISPLAY ' INPUT RECORDS    ' EXTR-IN-CNT
           DISPLAY ' OUTPUT RECORDS   ' EXTR-OUT-CNT

           GOBACK
           .
       0010-OPEN-FILES.
       
           OPEN INPUT FILE-IN
               OUTPUT FILE-OUT

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

           MOVE SPACES                 TO COMP-BILLING-INSTRUCTIONS
           MOVE 'BL'                   TO BL-RECORD-ID

           MOVE IN-CONTROL-PRIMARY     TO BL-CONTROL-PRIMARY
           MOVE IN-MAINT-INFORMATION   TO BL-MAINT-INFORMATION

           MOVE IN-CONTACT-NAME        TO BL-CONTACT-NAME 
           MOVE IN-ADDR1               TO BL-ADDR1        
           MOVE IN-ADDR2               TO BL-ADDR2        
           MOVE IN-CITY                TO BL-CITY         
           MOVE IN-STATE               TO BL-STATE        
           MOVE IN-ZIP                 TO BL-ZIP

           MOVE IN-CHECK-HANDLING      TO BL-CHECK-HANDLING
           MOVE IN-SPECIAL-INSTRUCTIONS TO BL-SPECIAL-INSTRUCTIONS

           PERFORM 0100-WRITE-OUTPUT   THRU 0100-EXIT

           PERFORM 0150-READ-INPUT     THRU 0150-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-WRITE-OUTPUT.

           WRITE FILE-OUT-RECORD       FROM COMP-BILLING-INSTRUCTIONS
           ADD 1                       TO EXTR-OUT-CNT

           .
       0100-EXIT.
           EXIT.

       0150-READ-INPUT.

           READ FILE-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO EXTR-IN-CNT
           END-IF

           .
       0150-EXIT.
           EXIT.

       0060-CLOSE-FILES.
       
           CLOSE FILE-IN FILE-OUT

           .
       0060-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX'              USING DATE-CONVERSION-DATA.

       8500-EXIT.
           EXIT.

       ABEND-PGM.   COPY ELCABEND.

