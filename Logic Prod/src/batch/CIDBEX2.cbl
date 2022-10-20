00003  PROGRAM-ID.     CIDBEX2.       
00010 *AUTHOR.         SUZAN DOWNING.                                       
00013 *DATE-COMPILED.                                                   

00025 *REMARKS.               
      ******************************************************************
      *  CIDBEX1 SORTS THE BENEFICIARY FILE BY LAST MAINT DATE
      *  CIDBEX2 SORTS THE BENEFICIARY FILE BY MAIL-TO NAME 
      *  THIS PROGRAM PROCESSES THE ELBENE FILE SEPARATELY FOR EACH 
      *  COMPANY DEPENDING ON THE DATECARD USED IN THE JOB MACRO
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 021104    2002123000003  SMVA  NEW PROGRAM  
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
      ******************************************************************

00033  ENVIRONMENT DIVISION.                                            
00034                                                                   
00035  INPUT-OUTPUT SECTION.                                            
00036                                                                   
00037  FILE-CONTROL.                                                    
00038                                                                   
00039      SELECT ELBENE              ASSIGN TO ELBENE                  
00040                                 ORGANIZATION IS INDEXED
00041                                 ACCESS IS DYNAMIC
00042                                 RECORD KEY IS BE-CONTROL-PRIMARY
00043                                 FILE STATUS IS BE-STATUS. 
00044                                                                   
00050                                                                   
00051      SELECT SORT-WORK           ASSIGN TO SYS001.  
00052      SELECT PRNTR               ASSIGN TO SYS008.   
00053      SELECT DISK-DATE           ASSIGN TO SYS019.   
00055  EJECT                                                            
00056  DATA DIVISION.                                                   
00057  FILE SECTION.                                                    
00058                                                                   
00059  FD  ELBENE.                                                      
00060                                                                   
00061      COPY ELCBENE.                                                
00062  EJECT                                                            
00066  SD  SORT-WORK.                                                   
00068  01  SORT-RECORD.                                                 
00069      12  FILLER                      PIC X(16).                   
00070      12  SORT-BE-MAIL-TO-NAME        PIC X(30).                   
00073      12  FILLER                      PIC X(454).                  
00074                                                                   
00075  FD  DISK-DATE                                                    
00076                              COPY ELCDTEFD.                          CL**2
00077  FD  PRNTR                                                        
00078                              COPY ELCPRTFD.                          CL**2
00081  EJECT                                                            
00082  WORKING-STORAGE SECTION.                                         
00084  77  FILLER   PIC X(32) VALUE '********************************'. 
00085  77  FILLER   PIC X(32) VALUE '*           CIDBEX2            *'. 
00086  77  FILLER   PIC X(32) VALUE '******** VMOD=2.005*************'. 
00087                                                                   
00126  01  MISC-WS.                                                     
00098      05  PGM-SUB             COMP-3   PIC S9(03)    VALUE +343.           
           05  WS-LINE-COUNT       COMP-3   PIC S9(03)    VALUE +56.
           05  WS-LINE-COUNT-MAX   COMP-3   PIC S9(03)    VALUE +55.
           05  WS-PAGE             COMP-3   PIC S9(05)    VALUE +0.
           05  WS-EOF1                      PIC X(01)     VALUE SPACE.
               88  BENE-EOF                               VALUE 'Y'.
           05  WS-EOF2                      PIC X(01)     VALUE SPACE.
               88  SORT-EOF                               VALUE 'Y'.
00106      05  WS-ABEND-MESSAGE             PIC  X(80)    VALUE SPACES.      
00107      05  WS-ABEND-FILE-STATUS         PIC  X(02)    VALUE ZERO.        
00108      05  WS-ABEND-PROGRAM             PIC  X(08)    VALUE SPACES.      
00109      05  WS-RETURN-CODE      COMP     PIC S9(04)    VALUE +0.     
00110      05  WS-ZERO             COMP-3   PIC S9(01)    VALUE +0.          
00111      05  ABEND-CODE                   PIC  X(04)    VALUE ZERO.        
00112      05  ABEND-OPTION                 PIC  X(01)    VALUE 'Y'.         
00114      05  BE-STATUS                    PIC  X(02)    VALUE ZEROS.                                                   
00302      05  WS-REFORMAT-TIME             PIC S9(06).
           05  WS-TIME-REDEF  REDEFINES WS-REFORMAT-TIME.
               10  WS-REFORMAT-TIME-HH      PIC  9(02).
               10  WS-REFORMAT-TIME-MM      PIC  9(02).
               10  WS-REFORMAT-TIME-SS      PIC  9(02).
                   
00123      COPY ELCDTECX. 
00124      COPY ELCDTEVR. 
00124      COPY ELCDATE.


00131  01  WS-HEADING1.
           05  FILLER                      PIC X(01)   VALUE '1'.
           05  FILLER                      PIC X(43)   VALUE SPACES.
           05  WS-H1-TITLE                 PIC X(42)   VALUE 
           'FULL BENEFICIARY MASTER EXTRACT'.
           05  FILLER                      PIC X(35)   VALUE SPACES.
           05  WS-H1-REPORT-ID             PIC X(07)   VALUE 'CIDBEX2'.
           05  FILLER                      PIC X(05)   VALUE SPACES.


       01  WS-HEADING2.
           05  FILLER                      PIC X(01)   VALUE ' '.
           05  FILLER                      PIC X(46)   VALUE SPACES.
           05  WS-H2-COMPANY-NAME          PIC X(30)   VALUE SPACES.
           05  FILLER                      PIC X(44)   VALUE SPACES.
           05  WS-H2-DATE                  PIC X(08)   VALUE SPACES.
           05  FILLER                      PIC X(04)   VALUE SPACES.


       01  WS-HEADING3.
           05  FILLER                      PIC X(01)    VALUE ' '.
           05  FILLER                      PIC X(125)   VALUE SPACES.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.
           05  FILLER                      PIC X(01)    VALUE SPACE.


       01  WS-HEADING4.
           05  FILLER                      PIC X(01)    VALUE '-'.
           05  FILLER                      PIC X(05)    VALUE SPACES.
           05  FILLER                      PIC X(10)    VALUE
              'MAINT DATE'.
           05  FILLER                      PIC X(03)    VALUE SPACES.
           05  FILLER                      PIC X(10)    VALUE
              'MAINT TIME'.
           05  FILLER                      PIC X(04)    VALUE SPACES.
           05  FILLER                      PIC X(10)    VALUE
              'MAINT USER'.
           05  FILLER                      PIC X(03)    VALUE SPACES.
           05  FILLER                      PIC X(14)    VALUE
               'BENEFICIARY ID'. 
           05  FILLER                      PIC X(03)    VALUE SPACES.
           05  FILLER                      PIC X(12)    VALUE
               'MAIL-TO NAME'.
           05  FILLER                      PIC X(58)    VALUE SPACES.


       01  WS-HEADING5.
           05  FILLER                      PIC X(01)    VALUE ' '.
           05  FILLER                      PIC X(15)    VALUE SPACES.
           05  FILLER                      PIC X(16)    VALUE
               'MAIL-TO ADDRESS:'.
           05  FILLER                      PIC X(101)   VALUE SPACES.


       01  WS-DETAIL1.
           05  FILLER                      PIC X(01)    VALUE ' '.
           05  FILLER                      PIC X(05)    VALUE SPACES.
00036      05  WS-D1-LAST-MAINT-DT         PIC X(10)    VALUE SPACES.
           05  FILLER                      PIC X(05)    VALUE SPACES.
00038      05  WS-D1-LAST-MAINT-HH         PIC 9(02)    VALUE ZEROS.
           05  FILLER                      PIC X(01)    VALUE ':'.
00038      05  WS-D1-LAST-MAINT-MM         PIC 9(02)    VALUE ZEROS.
           05  FILLER                      PIC X(08)    VALUE SPACES.
00037      05  WS-D1-LAST-MAINT-BY         PIC X(04)    VALUE SPACES.
           05  FILLER                      PIC X(08)    VALUE SPACES.
00029      05  WS-D1-BENEFICIARY-ID        PIC X(10)    VALUE SPACES.
           05  FILLER                      PIC X(03)    VALUE SPACES.
00041      05  WS-D1-MAIL-TO-NAME          PIC X(30)    VALUE SPACES.
           05  FILLER                      PIC X(44)    VALUE SPACES.


       01  WS-DETAIL2.
           05  FILLER                      PIC X(01)    VALUE SPACE.
           05  FILLER                      PIC X(15)    VALUE SPACES.
00042      05  WS-D2-MAIL-TO-ADDR-LN1      PIC X(30)    VALUE SPACES.
           05  FILLER                      PIC X(02)    VALUE SPACES.
00042      05  WS-D2-MAIL-TO-ADDR-LN2      PIC X(30)    VALUE SPACES.
           05  FILLER                      PIC X(02)    VALUE SPACES.
00045      05  WS-D2-MAIL-TO-CITY-STATE    PIC X(30)    VALUE SPACES.
           05  FILLER                      PIC X(02)    VALUE SPACES.
00046      05  WS-D2-MAIL-TO-ZIP           PIC X(09)    VALUE SPACES.
           05  FILLER                      PIC X(12)    VALUE SPACES.
00132                                                                   

00201  PROCEDURE DIVISION.                                              

00205  0000-MAIN.                                                 
00203      COPY ELCDTERX.     
00204                                                                   
00206      OPEN INPUT  ELBENE                                           
00207           OUTPUT PRNTR
00208                                                                   
00209      IF BE-STATUS = '00' OR '97'                                  
00210          CONTINUE                                                 
00211      ELSE                                                       
00212          MOVE BE-STATUS           TO WS-ABEND-FILE-STATUS         
00213          MOVE 'ERROR OCCURED OPEN - ELBENE'                       
00214                                   TO WS-ABEND-MESSAGE             
00215          PERFORM ABEND-PGM
           END-IF
00216                                                                   
00231      MOVE COMPANY-NAME            TO WS-H2-COMPANY-NAME
           MOVE WS-CURRENT-DATE         TO WS-H2-DATE

           PERFORM 1500-SORT-BENE       THRU 1500-EXIT

00242      CLOSE ELBENE                                                 
00243            PRNTR

00245      GOBACK

           .
       0000-EXIT.
           EXIT.
00233                                                                   

00234  1500-SORT-BENE.                                                  

00235      SORT SORT-WORK ON ASCENDING KEY SORT-BE-MAIL-TO-NAME
00239          INPUT  PROCEDURE 2000-SORT-BENE   THRU 2000-EXIT
00240          OUTPUT PROCEDURE 3000-MAKE-REPORT THRU 3000-EXIT
00241                                                                   
           .
       1500-EXIT.
           EXIT.

00247  2000-SORT-BENE.                                  

00248      MOVE LOW-VALUES              TO BE-CONTROL-PRIMARY
00249      MOVE DTE-CLASIC-COMPANY-CD   TO BE-COMPANY-CD
00250                                                                   
00251      START ELBENE KEY NOT < BE-CONTROL-PRIMARY
00252                                                                   
00253      IF BE-STATUS = '23'                                          
00254          GO TO 2000-EXIT
           END-IF
00255                                                                   
00256      IF BE-STATUS NOT = ZERO                                      
00257          MOVE BE-STATUS           TO  WS-ABEND-FILE-STATUS         
00258          MOVE 'ERROR OCCURED START - ELBENE'                      
00259                                   TO  WS-ABEND-MESSAGE             
00260          PERFORM ABEND-PGM
           END-IF

           PERFORM 2020-READ-NEXT-BENE  THRU 2020-EXIT
               UNTIL BENE-EOF
00261                                                                   
           .
       2000-EXIT.
           EXIT.
00262                                                                   
00263  2020-READ-NEXT-BENE.                                                    

00264      READ ELBENE NEXT RECORD
               AT END
                   SET BENE-EOF TO TRUE
                   GO TO 2020-EXIT
           END-READ
00265                                                                   
00266      IF BE-STATUS NOT = '00'
00270          MOVE BE-STATUS           TO  WS-ABEND-FILE-STATUS         
00271          MOVE 'ERROR OCCURED READ - ELBENE'                       
00272                                   TO  WS-ABEND-MESSAGE             
00273          PERFORM ABEND-PGM
           END-IF
00274                                                                   
00275      IF BE-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 
               SET BENE-EOF TO TRUE
00276          GO TO 2020-EXIT
           END-IF
00277                                                                   
00279      MOVE BENEFICIARY-MASTER      TO SORT-RECORD
00280      RELEASE SORT-RECORD

           .
       2020-EXIT.
           EXIT.

00288  3000-MAKE-REPORT.                                    
           
           PERFORM 3020-GET-SORT-RECORD THRU 3020-EXIT
               UNTIL SORT-EOF

           .
       3000-EXIT.
           EXIT.

00288  3020-GET-SORT-RECORD.                                 

00290      RETURN SORT-WORK                                             
00291          AT END SET SORT-EOF TO TRUE
                      GO TO 3020-EXIT
           END-RETURN
00292                                                                   
00293      MOVE SORT-RECORD             TO BENEFICIARY-MASTER

           PERFORM 3030-FORMAT-REPORT   THRU 3030-EXIT

           .
       3020-EXIT.
           EXIT.
00294                                                                   
00295  3030-FORMAT-REPORT.                                         

           MOVE BE-LAST-MAINT-DT        TO DC-BIN-DATE-1    
           MOVE SPACE                   TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO WS-D1-LAST-MAINT-DT
           ELSE
               DISPLAY 'INVALID MAINT DATE FOR BENE ' BE-BENEFICIARY
               MOVE '00/00/0000'        TO WS-D1-LAST-MAINT-DT
           END-IF
00299                                                                   
00302      MOVE BE-LAST-MAINT-HHMMSS    TO WS-REFORMAT-TIME              
           MOVE WS-REFORMAT-TIME-HH     TO WS-D1-LAST-MAINT-HH
           MOVE WS-REFORMAT-TIME-MM     TO WS-D1-LAST-MAINT-MM

00302      MOVE BE-LAST-MAINT-BY        TO WS-D1-LAST-MAINT-BY                  
00302      MOVE BE-BENEFICIARY          TO WS-D1-BENEFICIARY-ID
00308      MOVE BE-MAIL-TO-NAME         TO WS-D1-MAIL-TO-NAME                 

00309      MOVE BE-ADDRESS-LINE-1       TO WS-D2-MAIL-TO-ADDR-LN1             
00310      MOVE BE-ADDRESS-LINE-2       TO WS-D2-MAIL-TO-ADDR-LN2             
051810     MOVE SPACES                  TO WS-D2-MAIL-TO-CITY-STATE           
051810     STRING BE-CITY ' ' BE-STATE DELIMITED BY '  '
051810        INTO WS-D2-MAIL-TO-CITY-STATE
051810     END-STRING
00312      MOVE BE-ZIP-CODE             TO WS-D2-MAIL-TO-ZIP                  
00313                                                                   
           PERFORM 4000-PRINT-DETAIL    THRU 4000-EXIT

           .
       3030-EXIT.
           EXIT.


       4000-PRINT-DETAIL.

00297      IF WS-LINE-COUNT > WS-LINE-COUNT-MAX  
00298          PERFORM 4020-HEADER-RTN  THRU 4020-EXIT
           END-IF

           MOVE WS-DETAIL1               TO PRT
           PERFORM 4900-WRITE            THRU 4900-EXIT

           MOVE WS-DETAIL2               TO PRT
           PERFORM 4900-WRITE            THRU 4900-EXIT

           MOVE SPACES                   TO PRT
           PERFORM 4900-WRITE            THRU 4900-EXIT

           .
       4000-EXIT.
           EXIT.


       4020-HEADER-RTN.

           MOVE WS-HEADING1             TO PRT
           PERFORM 4900-WRITE           THRU 4900-EXIT

           MOVE WS-HEADING2             TO PRT
           PERFORM 4900-WRITE           THRU 4900-EXIT

           MOVE WS-PAGE                 TO WS-H3-PAGE
           MOVE WS-HEADING3             TO PRT
           PERFORM 4900-WRITE           THRU 4900-EXIT

           MOVE WS-HEADING4             TO PRT
           PERFORM 4900-WRITE           THRU 4900-EXIT

           MOVE WS-HEADING5             TO PRT
           PERFORM 4900-WRITE           THRU 4900-EXIT

           MOVE SPACES                  TO PRT
           MOVE '0'                     TO P-CTL
           PERFORM 4900-WRITE           THRU 4900-EXIT

           .
       4020-EXIT.
           EXIT.

       4900-WRITE.

           EVALUATE TRUE
           WHEN P-CTL = '1'
               MOVE +1                  TO WS-LINE-COUNT
               ADD +1                   TO WS-PAGE

           WHEN P-CTL = SPACE
               ADD +1                   TO WS-LINE-COUNT

           WHEN P-CTL = '0'
               ADD +2                   TO WS-LINE-COUNT

           WHEN OTHER
               ADD +3                   TO WS-LINE-COUNT
           END-EVALUATE


           WRITE PRT

           .
       4900-EXIT.
           EXIT.

 
       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.


00372  ABEND-PGM SECTION.                                               
00373                     COPY ELCABEND.                                   CL**4
