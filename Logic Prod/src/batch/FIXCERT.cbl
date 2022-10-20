00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   FIXCERT
00003  PROGRAM-ID.                FIXCERT.                                  LV004
00009                                                                   FIXCERT
00010 *AUTHOR.       SUZAN VUKOV.                                       FIXCERT
00012                                                                   FIXCERT
00025 *REMARKS.                                                         FIXCERT
00026 *        FIX LOAN OFFICER CODE ON BATCH CERT FILE. BATCH CERT 
      *        FILE WILL UPDATE ONLINE FILE AT MONTHEND.
      * 
032703******************************************************************
032703*                   C H A N G E   L O G
032703*
032703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
032703*-----------------------------------------------------------------
032703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
032703* EFFECTIVE    NUMBER
032703*-----------------------------------------------------------------
032703* 032703   2001061800003   SMVA  NEW FIX PROGRAM
032703******************************************************************
00027                                                                   FIXCERT
00028  ENVIRONMENT DIVISION.                                            FIXCERT
00029  CONFIGURATION SECTION.                                           FIXCERT
00030                                                                      CL**3
00031  INPUT-OUTPUT SECTION.                                            FIXCERT
00032  FILE-CONTROL.                                                    FIXCERT
00033                                                                   FIXCERT
           SELECT OFFLN-CERT-IN    ASSIGN TO SYS010.

           SELECT OFFLN-CERT-OUT   ASSIGN TO SYS011.

00036      SELECT DISK-DATE        ASSIGN TO SYS019. 

00038  EJECT                                                            FIXCERT
00039  DATA DIVISION.                                                   FIXCERT
00040  FILE SECTION.                                                    FIXCERT
00041                                                                   FIXCERT
00045  FD  OFFLN-CERT-IN
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.
       01  OFFLN-CERT-REC-IN                PIC X(1056).
00066                                                                   FIXCERT
00045  FD  OFFLN-CERT-OUT 
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.
       01  OFFLN-CERT-REC-OUT               PIC X(1056).
00066                                                                   FIXCERT
00067  FD  DISK-DATE                                                    FIXCERT
00068                              COPY ELCDTEFD.                       FIXCERT


00073  WORKING-STORAGE SECTION.                                         FIXCERT
00074  77  FILLER  PIC  X(32) VALUE '********************************'. FIXCERT
00075  77  FILLER  PIC  X(32) VALUE '     FIXCERT WORKING STORAGE    '. FIXCERT
00076  77  FILLER  PIC  X(32) VALUE '********** VMOD=2.008 **********'.    CL**3
00077                                                                   FIXCERT
00082  01  WS-COUNTERS-AND-SUBS.
00081      05  PGM-SUB              COMP    PIC S9(03) VALUE +57.   
00081      05  WS-CORRECTION-CNT    COMP-3  PIC S9(03) VALUE +0.   

00082  01  WS-DATES.
           05  WS-CERT-EFF-DT               PIC 9(11).
           05  WS-CERT-EFF-DT-R                        REDEFINES
               WS-CERT-EFF-DT.
               10  FILLER                   PIC 9(03).
               10  WS-CERT-EFF-CYMD.
                   15  WS-CERT-EFF-CC       PIC 9(02).
                   15  WS-CERT-EFF-YY       PIC 9(02).
                   15  WS-CERT-EFF-MM       PIC 9(02).
                   15  WS-CERT-EFF-DD       PIC 9(02).

00082  01  WS-SWITCHES.
           05  WS-EOF-OFFLN-CERT            PIC X(01)  VALUE SPACE.
               88  EOF                                 VALUE '1'.

00276  01  WS-WK.                               
00277      12  WS-RETURN-CODE               PIC X(04)   VALUE SPACES. 
00278      12  WS-ABEND-MESSAGE             PIC X(80)   VALUE SPACES.
00279      12  WS-ZERO                      PIC S9(01)  VALUE ZERO. 
00280      12  WS-ABEND-FILE-STATUS         PIC X(02)   VALUE SPACES. 
00281                                                                   FIXCERT
           COPY ECSCRT01.

00282      COPY ELCDTECX.                                               FIXCERT
00283                                                                   FIXCERT
00284      COPY ELCDTEVR.                                               FIXCERT
00285                                                                   FIXCERT
00286      COPY ELCDATE.                                                   CL**4


00288  PROCEDURE DIVISION.                                              FIXCERT
00289                                                                   FIXCERT
00290  0000-MAIN. 
00291                              COPY ELCDTERX.                       FIXCERT
00292                                                                   FIXCERT
           PERFORM 8800-OPEN-FILES        THRU 8800-EXIT 
00369      PERFORM 0200-READ-OFFLN-CERT   THRU 0200-EXIT
               UNTIL EOF
           DISPLAY '# of corrections  ' WS-CORRECTION-CNT
           PERFORM 8900-CLOSE-FILES       THRU 8900-EXIT 

00829      GOBACK

           .
00300                                                                   FIXCERT
00451  0200-READ-OFFLN-CERT.

00452      READ OFFLN-CERT-IN
               INTO CERTIFICATE-RECORD  
               AT END 
                   SET EOF                TO TRUE
                   GO TO 0200-EXIT
           END-READ

           IF CR-ACCOUNT = '0000428100'
               CONTINUE
           ELSE
               WRITE OFFLN-CERT-REC-OUT   FROM CERTIFICATE-RECORD
               GO TO 0200-EXIT
           END-IF

           MOVE CR-DT                     TO WS-CERT-EFF-DT
           IF WS-CERT-EFF-CYMD >= '20020701'
               CONTINUE
           ELSE
               WRITE OFFLN-CERT-REC-OUT   FROM CERTIFICATE-RECORD
               GO TO 0200-EXIT
           END-IF

           IF CR-POLICY-IS-ACTIVE
               CONTINUE
           ELSE
               WRITE OFFLN-CERT-REC-OUT   FROM CERTIFICATE-RECORD
               GO TO 0200-EXIT
           END-IF

           EVALUATE TRUE
           WHEN CR-LOAN-OFFICER = '000'
               DISPLAY 'CHANGE ' CR-ACCOUNT ' ' CR-CERT-NO ' '
                   CR-LOAN-OFFICER 
               DISPLAY 'CERT EFF DATE ' WS-CERT-EFF-CYMD
               MOVE 'LAC'                 TO CR-LOAN-OFFICER 
               DISPLAY '   ' CR-LOAN-OFFICER  
               ADD +1                     TO WS-CORRECTION-CNT

           WHEN CR-LOAN-OFFICER = '001'
               DISPLAY 'CHANGE ' CR-ACCOUNT ' ' CR-CERT-NO ' '
                   CR-LOAN-OFFICER 
               DISPLAY 'CERT EFF DATE ' WS-CERT-EFF-CYMD
               MOVE 'JLE'                 TO CR-LOAN-OFFICER 
               DISPLAY '   ' CR-LOAN-OFFICER  
               ADD +1                     TO WS-CORRECTION-CNT

           WHEN CR-LOAN-OFFICER = '002'
               DISPLAY 'CHANGE ' CR-ACCOUNT ' ' CR-CERT-NO ' '
                   CR-LOAN-OFFICER 
               DISPLAY 'CERT EFF DATE ' WS-CERT-EFF-CYMD
               MOVE 'DMJ'                 TO CR-LOAN-OFFICER 
               DISPLAY '   ' CR-LOAN-OFFICER  
               ADD +1                     TO WS-CORRECTION-CNT

           WHEN CR-LOAN-OFFICER = '003'
               DISPLAY 'CHANGE ' CR-ACCOUNT ' ' CR-CERT-NO ' '
                   CR-LOAN-OFFICER 
               DISPLAY 'CERT EFF DATE ' WS-CERT-EFF-CYMD
               MOVE 'JAM'                 TO CR-LOAN-OFFICER 
               DISPLAY '   ' CR-LOAN-OFFICER  
               ADD +1                     TO WS-CORRECTION-CNT

           WHEN CR-LOAN-OFFICER = '004'
               DISPLAY 'CHANGE ' CR-ACCOUNT ' ' CR-CERT-NO ' '
                   CR-LOAN-OFFICER 
               DISPLAY 'CERT EFF DATE ' WS-CERT-EFF-CYMD
               MOVE 'PCR'                 TO CR-LOAN-OFFICER 
               DISPLAY '   ' CR-LOAN-OFFICER  
               ADD +1                     TO WS-CORRECTION-CNT

           WHEN CR-LOAN-OFFICER = '005'
               DISPLAY 'CHANGE ' CR-ACCOUNT ' ' CR-CERT-NO ' '
                   CR-LOAN-OFFICER 
               DISPLAY 'CERT EFF DATE ' WS-CERT-EFF-CYMD
               MOVE 'RWS'                 TO CR-LOAN-OFFICER 
               DISPLAY '   ' CR-LOAN-OFFICER  
               ADD +1                     TO WS-CORRECTION-CNT

           WHEN CR-LOAN-OFFICER = '006'
               DISPLAY 'CHANGE ' CR-ACCOUNT ' ' CR-CERT-NO ' '
                   CR-LOAN-OFFICER 
               DISPLAY 'CERT EFF DATE ' WS-CERT-EFF-CYMD
               MOVE 'LLW'                 TO CR-LOAN-OFFICER 
               DISPLAY '   ' CR-LOAN-OFFICER  
               ADD +1                     TO WS-CORRECTION-CNT

           WHEN CR-LOAN-OFFICER = '007'
               DISPLAY 'CHANGE ' CR-ACCOUNT ' ' CR-CERT-NO ' '
                   CR-LOAN-OFFICER 
               DISPLAY 'CERT EFF DATE ' WS-CERT-EFF-CYMD
               MOVE 'BDW'                 TO CR-LOAN-OFFICER 
               DISPLAY '   ' CR-LOAN-OFFICER  
               ADD +1                     TO WS-CORRECTION-CNT
           END-EVALUATE

           WRITE OFFLN-CERT-REC-OUT       FROM CERTIFICATE-RECORD

           .
       0200-EXIT.
           EXIT.

00828  8800-OPEN-FILES.
00345                                                                   FIXCERT
00346      OPEN INPUT  OFFLN-CERT-IN    
                OUTPUT OFFLN-CERT-OUT
           .
       8800-EXIT.
           EXIT.
00348                                                                   FIXCERT
00828  8900-CLOSE-FILES.
00345                                                                   FIXCERT
00346      CLOSE OFFLN-CERT-IN    
                 OFFLN-CERT-OUT

           .
       8900-EXIT.
           EXIT.
00348                                                                   FIXCERT
           
00831  ABEND-PGM SECTION.                                               FIXCERT
00832                              COPY ELCABEND.                       FIXCERT
