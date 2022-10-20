       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CPSAXB1.
       AUTHOR.        Cowtown.
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
050815* 050815  CR2015022600002  PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ALPHA           ASSIGN TO SYS010.
           SELECT DISK-DATE       ASSIGN TO SYS019.

           SELECT ERALPH          ASSIGN TO ERALPH
                                  ACCESS IS DYNAMIC
                                  ORGANIZATION IS INDEXED
                                  FILE STATUS IS ERALPH-FILE-STATUS
                                  RECORD KEY IS AF-CONTROL-PRIMARY.

       DATA DIVISION.
       FILE SECTION.

       FD  ALPHA
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
                                       COPY ECSAEX01.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERALPH.
                                                                        
                                       COPY ERCALPH.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CPSAXB1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  IN-CNT                      PIC 9(11)  VALUE ZEROS.
       77  OUT-CNT                     PIC 9(11)  VALUE ZEROS.
       77  SEL-IN-CNT                  PIC 9(11)  VALUE ZEROS.
       77  WS-REL-RECS                 PIC 9(11)  VALUE ZEROS.
       77  A1                          PIC S9(5)  VALUE +0 COMP-3.
       77  S1                          PIC S9(5)  VALUE +0 COMP-3.
       77  WS-S1                       PIC S999   VALUE +0 COMP-3.
       77  WS-S2                       PIC S999   VALUE +0 COMP-3.
       77  WS-S3                       PIC S999   VALUE +0 COMP-3.
       77  WS-S4                       PIC S999   VALUE +0 COMP-3.
       77  ERALPH-FILE-STATUS          PIC XX VALUE LOW-VALUES.
       77  ws-lf-inforce               pic x  value spaces.
           88  lf-inforce                  value 'Y'.
       77  ws-ah-inforce               pic x  value spaces.
           88  ah-inforce                  value 'Y'.

       01  WS-CURRENT-KEY.
           05  WS-CONTROL              PIC X(19)  VALUE LOW-VALUES.
           05  WS-RPTCD1               PIC X(10)  VALUE LOW-VALUES.
           05  WS-RPTCD2               PIC X(10)  VALUE LOW-VALUES.
           05  WS-RPTCD3               PIC X(10)  VALUE LOW-VALUES.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WS-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.

       01  DATE-AREAS.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC X(4).
               10  WS-WORK-CCYY-N REDEFINES WS-WORK-CCYY
                                       PIC 9(4).
               10  WS-WORK-MM          PIC XX.
               10  WS-WORK-DD          PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT
           PERFORM 0100-PROCESS-INPUT  THRU 0100-EXIT UNTIL
              (END-OF-INPUT)
      *       OR (IN-CNT > 5000)

           PERFORM 0040-CLOSE-FILES    THRU 0040-EXIT
           GOBACK

           .
       0010-INITIALIZE.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0030-READ-ALPHA     THRU 0030-EXIT

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ALPHA i-o ERALPH

           IF ERALPH-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR-ERALPH-OPEN ' ERALPH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-READ-ALPHA.

           READ ALPHA AT END
               SET END-OF-INPUT        TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO IN-CNT
              IF AX-ALPHA-TYPE-CODE <> 'I' AND 'J'
                 GO TO 0030-READ-ALPHA
              END-IF
              perform 0035-check-inforce
                                       thru 0035-exit
              if lf-inforce or ah-inforce
                 ADD 1                 TO SEL-IN-CNT
              else
                 go to 0030-read-alpha
           END-IF

           .
       0030-EXIT.
           EXIT.

       0035-check-inforce.

           move spaces                 to ws-lf-inforce
                                          ws-ah-inforce

           if (ax-lf-typ <> spaces and zeros)
              and (ax-lf-remterm > +0)
              and (ax-lf-remamt > +0)
              set lf-inforce to true
           end-if

           if (ax-ah-typ <> spaces and zeros)
              and (ax-ah-remterm > +0)
              and (ax-ah-remamt > +0)
              set ah-inforce to true
           end-if

           .
       0035-exit.
           exit.

       0040-CLOSE-FILES.

           DISPLAY ' ALPHA IN RECORDS    ' IN-CNT
           DISPLAY ' ALPHA SELECTED      ' SEL-IN-CNT
           DISPLAY ' ERALPH RECS WRITTEN ' OUT-CNT
           CLOSE ALPHA ERALPH

           .
       0040-EXIT.
           EXIT.

       0100-PROCESS-INPUT.

           MOVE SPACES                 TO ALPHA-FILE-REC
           MOVE 'AF'                   TO AF-RECORD-ID

           MOVE AX-COMPANY-CD          TO AF-COMPANY-CD
                                          af-company-cd-a1
           MOVE AX-CARRIER             TO AF-CARRIER    
           MOVE AX-GROUPING            TO AF-GROUPING   
           MOVE AX-STATE               TO AF-STATE      
           MOVE AX-ACCOUNT             TO AF-ACCOUNT
                                          af-account-a1

           MOVE AX-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO AF-DT
           ELSE
              DISPLAY ' ERROR-EFF DT-CONVERSION '
                 DC-ERROR-CODE ' ' AX-DT ' ' AX-CERT-NO
              PERFORM ABEND-PGM
           END-IF

           MOVE AX-CERT-NO             TO AF-CERT-NO
           move ax-alpha-type-code     to af-alpha-type-code

           MOVE AX-NAME                TO AF-NAME
           MOVE AX-AGE                 TO AF-AGE
           MOVE AX-SEX                 TO AF-SEX
           MOVE AX-LF-TYP              TO AF-LF-TYP          
           MOVE AX-LF-TERM             TO AF-LF-TERM         
           MOVE AX-LF-REMTERM          TO AF-LF-REMTERM      
           MOVE AX-LF-AMT              TO AF-LF-AMT          
           MOVE AX-LF-REMAMT           TO AF-LF-REMAMT       
           MOVE AX-LF-PRM              TO AF-LF-PRM          
           MOVE AX-LF-AMT-ALT          TO AF-LF-AMT-ALT      
           MOVE AX-LF-REMAMT-ALT       TO AF-LF-REMAMT-ALT   
           MOVE AX-LF-PRM-ALT          TO AF-LF-PRM-ALT      
           MOVE AX-AH-TYP              TO AF-AH-TYP          
           MOVE AX-AH-TERM             TO AF-AH-TERM         
           MOVE AX-AH-REMTERM          TO AF-AH-REMTERM      
           MOVE AX-AH-AMT              TO AF-AH-AMT          
           MOVE AX-AH-REMAMT           TO AF-AH-REMAMT       
           MOVE AX-AH-PRM              TO AF-AH-PRM          
           MOVE AX-APR                 TO AF-APR             
           MOVE AX-IND-GRP             TO AF-IND-GRP         
           MOVE AX-SOC-NO              TO AF-SOC-NO          
           MOVE AX-LF-STATUS           TO AF-LF-STATUS       
           MOVE AX-LF-PRE-PLST         TO AF-LF-PRE-PLST     

           if ax-lf-cncl = zeros
              move low-values          to af-lf-cncl
           else
              MOVE AX-LF-CNCL          TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO AF-LF-CNCL
              ELSE
                 DISPLAY ' ERROR-LF CNC-CONVERSION '
                    DC-ERROR-CODE ' ' AX-LF-CNCL ' ' AX-CERT-NO
                 PERFORM ABEND-PGM
              END-IF
           end-if

           if ax-death = zeros
              move low-values          to af-death
           else
              MOVE AX-DEATH            TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO AF-DEATH
              ELSE
                 DISPLAY ' ERROR-DEATH -CONVERSION '
                    DC-ERROR-CODE ' ' AX-DEATH ' ' AX-CERT-NO
                 PERFORM ABEND-PGM
              END-IF
           end-if

           if ax-lf-exit = zeros
              move low-values          to af-lf-exit
           else
              MOVE AX-LF-EXIT          TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO AF-LF-EXIT
              ELSE
                 DISPLAY ' ERROR-LFEXIT-CONVERSION '
                    DC-ERROR-CODE ' ' AX-LF-EXIT ' ' AX-CERT-NO
                 PERFORM ABEND-PGM
              END-IF
           end-if

           if ax-lf-expires = zeros
              move low-values          to af-lf-expires
           else
              MOVE AX-LF-EXPIRES       TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO AF-LF-EXPIRES
              ELSE
                 DISPLAY ' ERROR-LFEXP -CONVERSION '
                    DC-ERROR-CODE ' ' AX-LF-EXPIRES  ' ' AX-CERT-NO
                 PERFORM ABEND-PGM
              END-IF
           end-if

           MOVE AX-AH-STATUS           TO AF-AH-STATUS       
           MOVE AX-AH-PRE-PLST         TO AF-AH-PRE-PLST     
           if ax-ah-cncl = zeros
              move low-values          to af-ah-cncl
           else
              MOVE AX-AH-CNCL          TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO AF-AH-CNCL
              ELSE
                 DISPLAY ' ERROR-AH CNC-CONVERSION '
                    DC-ERROR-CODE ' ' AX-AH-CNCL ' ' AX-CERT-NO
                 PERFORM ABEND-PGM
              END-IF
           end-if

           if ax-lump-sum = zeros
              move low-values          to af-lump-sum
           else
              MOVE AX-LUMP-SUM         TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO AF-LUMP-SUM
              ELSE
                 DISPLAY ' ERROR-LUMP  -CONVERSION '
                    DC-ERROR-CODE ' ' AX-LUMP-SUM  ' ' AX-CERT-NO
                 PERFORM ABEND-PGM
              END-IF
           end-if

           if ax-ah-exit = zeros
              move low-values          to af-ah-exit
           else
              MOVE AX-AH-EXIT          TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO AF-AH-EXIT
              ELSE
                 DISPLAY ' ERROR-AHEXIT-CONVERSION '
                    DC-ERROR-CODE ' ' AX-AH-EXIT ' ' AX-CERT-NO
                 PERFORM ABEND-PGM
              END-IF
           end-if

           if ax-ah-expires = zeros
              move low-values          to af-ah-expires
           else
              MOVE AX-AH-EXPIRES       TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO AF-AH-EXPIRES
              ELSE
                 DISPLAY ' ERROR-AHEXP -CONVERSION '
                    DC-ERROR-CODE ' ' AX-AH-EXPIRES  ' ' AX-CERT-NO
                 PERFORM ABEND-PGM
              END-IF
           end-if

           MOVE AX-ENTRY               TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO AF-ENTRY
           ELSE
              DISPLAY ' ERROR-ENTRY -CONVERSION '
                 DC-ERROR-CODE ' ' AX-ENTRY ' ' AX-CERT-NO
              PERFORM ABEND-PGM
           END-IF

           MOVE AX-ENTRY-STATUS        TO AF-ENTRY-STATUS    

           PERFORM 0110-WRITE-ERALPH   THRU 0110-EXIT
           PERFORM 0030-READ-ALPHA     THRU 0030-EXIT

           .
       0100-EXIT.
           EXIT.

       0110-WRITE-ERALPH.

           WRITE ALPHA-FILE-REC
           IF ERALPH-FILE-STATUS <> '00'
              DISPLAY 'ERROR-ERALPH-WRITE ' ERALPH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
           ADD 1 TO OUT-CNT

           .
       0110-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

