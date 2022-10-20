00003  PROGRAM-ID.     EL360.       
00010 *AUTHOR.         SUZAN DOWNING.                                       
00013 *DATE-COMPILED.                                                   

00025 *REMARKS.                                                         
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 021104    2002123000003  SMVA  NEW PROGRAM  
      * 060608    2008060500001  AJRA  FIX DEL PROCESS
020413* 020413  IR2013020100003  PEMA  ADD END OF COMP CHK FOR PREV
022213* 022213  IR2013022200001  PEMA  ADD END OF COMP CHK FOR CURR
      ******************************************************************

00033  ENVIRONMENT DIVISION.                                            
00034                                                                   
00035  INPUT-OUTPUT SECTION.                                            
00036                                                                   
00037  FILE-CONTROL.                                                    
00038                                                                   
00039      SELECT ELBENE-CURRENT      ASSIGN TO SYS021.                  
00052      SELECT ELBENE-PREVIOUS     ASSIGN TO SYS022.   
00052      SELECT AUDIT-ELBENE        ASSIGN TO SYS007
                                      ORGANIZATION IS LINE SEQUENTIAL.
00053      SELECT DISK-DATE           ASSIGN TO SYS019.   

00056  DATA DIVISION.                                                   
00057  FILE SECTION.                                                    
00058                                                                   
00059  FD  ELBENE-CURRENT.                                                      
00061      COPY ELCBENE.                                                

00059  FD  ELBENE-PREVIOUS.
       01  ELBENE-PREVIOUS-RECORD           PIC X(500).

00077  FD  AUDIT-ELBENE.
       01  AUDIT-ELBENE-RECORD.
           05  AUDIT-ELBENE-CURR-DT         PIC X(08).
           05  AUDIT-ELBENE-REC-SEQ         PIC 9(04).
           05  AUDIT-ELBENE-RPT-LINE        PIC X(133).
                                                   
00075  FD  DISK-DATE                                                    
00076                              COPY ELCDTEFD.                          CL**2
00081  EJECT                                                            
00082  WORKING-STORAGE SECTION.                                         
00084  77  FILLER   PIC X(32) VALUE '********************************'. 
00085  77  FILLER   PIC X(32) VALUE '*           EL360              *'. 
00086  77  FILLER   PIC X(32) VALUE '******** VMOD=2.005*************'. 
00087                                                                   
00126  01  MISC-WS.                                                     
00098      05  PGM-SUB               COMP-3 PIC S9(03)    VALUE +360.           
060608     05  WS-LINE-COUNT         COMP-3 PIC S9(05)    VALUE +0.
           05  WS-AUDIT-BENE-REC-CNT COMP-3 PIC S9(03)    VALUE +0.
           05  WS-EOF1                      PIC X(01)     VALUE SPACE.
               88  EOF-CURR-BENE                          VALUE 'Y'.
           05  WS-EOF2                      PIC X(01)     VALUE SPACE.
               88  EOF-PREV-BENE                          VALUE 'Y'.
00106      05  WS-ABEND-MESSAGE             PIC  X(80)    VALUE SPACES.      
00107      05  WS-ABEND-FILE-STATUS         PIC  X(02)    VALUE ZERO.        
00108      05  WS-ABEND-PROGRAM             PIC  X(08)    VALUE SPACES.      
00109      05  WS-RETURN-CODE        COMP   PIC S9(04)    VALUE +0.     
00110      05  WS-ZERO               COMP-3 PIC S9(01)    VALUE +0.          
00111      05  ABEND-CODE                   PIC  X(04)    VALUE ZERO.        
00112      05  ABEND-OPTION                 PIC  X(01)    VALUE 'Y'.         

00302      05  WS-REFORMAT-TIME             PIC S9(06).
           05  WS-TIME-REDEF  REDEFINES WS-REFORMAT-TIME.
               10  WS-REFORMAT-TIME-HH      PIC  9(02).
               10  WS-REFORMAT-TIME-MM      PIC  9(02).
               10  WS-REFORMAT-TIME-SS      PIC  9(02).
                   

00131  01  WS-ELBENE-PREVIOUS.
           05  FILLER                       PIC X(02).
           05  BE2-COMPANY-CD               PIC X(01).
           05  FILLER                       PIC X(01).
           05  BE2-BENEFICIARY              PIC X(10).
           05  FILLER                       PIC X(42).
00036      05  BE2-LAST-MAINT-DT            PIC X(02). 
00037      05  BE2-LAST-MAINT-BY            PIC X(04).       
00038      05  BE2-LAST-MAINT-HHMMSS COMP-3 PIC S9(06).     
00041      05  BE2-MAIL-TO-NAME             PIC X(30).              
00042      05  BE2-ADDRESS-LINE-1           PIC X(30).             
00043      05  BE2-ADDRESS-LINE-2           PIC X(30).            
00044      05  FILLER                       PIC X(30).           
00045      05  BE2-CITY-STATE               PIC X(30).          
00046      05  BE2-ZIP-CODE                 PIC X(09).
00044      05  FILLER                       PIC X(275).           

00123      COPY ELCDTECX. 
00124      COPY ELCDTEVR. 
00124      COPY ELCDATE.


       01  WS-DETAIL1.
           05  FILLER                       PIC X(01)    VALUE ' '.
           05  WS-D1-CHG-TYPE               PIC X(03)    VALUE SPACES.
           05  FILLER                       PIC X(02)    VALUE SPACES.
00036      05  WS-D1-LAST-MAINT-DT          PIC X(10)    VALUE SPACES.
           05  FILLER                       PIC X(05)    VALUE SPACES.
00038      05  WS-D1-LAST-MAINT-HH          PIC 9(02)    VALUE ZEROS.
           05  FILLER                       PIC X(01)    VALUE ':'.
00038      05  WS-D1-LAST-MAINT-MM          PIC 9(02)    VALUE ZEROS.
           05  FILLER                       PIC X(08)    VALUE SPACES.
00037      05  WS-D1-LAST-MAINT-BY          PIC X(04)    VALUE SPACES.
           05  FILLER                       PIC X(08)    VALUE SPACES.
00029      05  WS-D1-BENEFICIARY-ID         PIC X(10)    VALUE SPACES.
           05  FILLER                       PIC X(03)    VALUE SPACES.
00041      05  WS-D1-MAIL-TO-NAME           PIC X(30)    VALUE SPACES.
           05  FILLER                       PIC X(44)    VALUE SPACES.


       01  WS-DETAIL2.
           05  FILLER                       PIC X(01)    VALUE SPACE.
           05  FILLER                       PIC X(15)    VALUE SPACES.
00042      05  WS-D2-MAIL-TO-ADDR-LN1       PIC X(30)    VALUE SPACES.
           05  FILLER                       PIC X(02)    VALUE SPACES.
00042      05  WS-D2-MAIL-TO-ADDR-LN2       PIC X(30)    VALUE SPACES.
           05  FILLER                       PIC X(02)    VALUE SPACES.
00045      05  WS-D2-MAIL-TO-CITY-STATE     PIC X(30)    VALUE SPACES.
           05  FILLER                       PIC X(02)    VALUE SPACES.
00046      05  WS-D2-MAIL-TO-ZIP            PIC X(09)    VALUE SPACES.
           05  FILLER                       PIC X(12)    VALUE SPACES.
00132                                                                   

00201  PROCEDURE DIVISION.                                              

00205  0000-MAIN.       
                                          
00203      COPY ELCDTERX.

00206      OPEN INPUT  ELBENE-CURRENT
                       ELBENE-PREVIOUS
00207           EXTEND AUDIT-ELBENE
00208                                                                   
           MOVE WS-CURRENT-DATE         TO AUDIT-ELBENE-CURR-DT

           PERFORM 1500-READ-PREV-BENE  THRU 1500-EXIT
060608     PERFORM 1400-READ-CURR-BENE  THRU 1400-EXIT
060608     PERFORM 1000-COMPARE-RECORDS THRU 1000-EXIT
               UNTIL EOF-CURR-BENE


00242      CLOSE ELBENE-CURRENT
                 ELBENE-PREVIOUS
00243            AUDIT-ELBENE

00245      GOBACK

           .
       0000-EXIT.
           EXIT.
00233                                                                   

060608 1000-COMPARE-RECORDS.                                                  

00275      IF BE-COMPANY-CD NOT <= DTE-CLASIC-COMPANY-CD                 
               SET EOF-CURR-BENE TO TRUE
00276          GO TO 1000-EXIT
           END-IF

           IF EOF-PREV-BENE
               MOVE 'ADD'                     TO WS-D1-CHG-TYPE
               PERFORM 3000-REPORT-REC1       THRU 3000-EXIT
060608         PERFORM 1400-READ-CURR-BENE    THRU 1400-EXIT
               GO TO 1000-EXIT
           END-IF
 
           IF BENEFICIARY-MASTER = WS-ELBENE-PREVIOUS
060608         PERFORM 1400-READ-CURR-BENE    THRU 1400-EXIT
               PERFORM 1500-READ-PREV-BENE    THRU 1500-EXIT
               GO TO 1000-EXIT
           END-IF
 
           EVALUATE TRUE
           WHEN BE-BENEFICIARY = BE2-BENEFICIARY
               MOVE 'DIF'                     TO WS-D1-CHG-TYPE
               PERFORM 3000-REPORT-REC1       THRU 3000-EXIT
               MOVE '   '                     TO WS-D1-CHG-TYPE
               PERFORM 3100-REPORT-REC2       THRU 3100-EXIT
060608         PERFORM 1400-READ-CURR-BENE    THRU 1400-EXIT
               PERFORM 1500-READ-PREV-BENE    THRU 1500-EXIT

           WHEN BE-BENEFICIARY > BE2-BENEFICIARY
               MOVE 'DEL'                     TO WS-D1-CHG-TYPE
060608         PERFORM 3100-REPORT-REC2       THRU 3100-EXIT
               PERFORM 1500-READ-PREV-BENE    THRU 1500-EXIT

           WHEN BE-BENEFICIARY < BE2-BENEFICIARY
               MOVE 'ADD'                     TO WS-D1-CHG-TYPE
               PERFORM 3000-REPORT-REC1       THRU 3000-EXIT
060608         PERFORM 1400-READ-CURR-BENE    THRU 1400-EXIT
           END-EVALUATE
                   
           .
       1000-EXIT.
           EXIT.

060608 1400-READ-CURR-BENE.                                                  
060608
060608     READ ELBENE-CURRENT 
060608         AT END
060608             SET EOF-CURR-BENE TO TRUE
060608     END-READ

022213     if not eof-curr-bene
022213        if be-company-cd not = dte-clasic-company-cd
022213           go to 1400-read-curr-bene
022213        end-if
022213     end-if

060608     .
060608 1400-EXIT.
060608     EXIT.


00234  1500-READ-PREV-BENE.
                                                  
00241      READ ELBENE-PREVIOUS INTO WS-ELBENE-PREVIOUS
               AT END
                   SET EOF-PREV-BENE TO TRUE
           END-READ

020413     if not eof-prev-bene
020413        if be2-company-cd not = dte-clasic-company-cd
020413           go to 1500-read-prev-bene
020413        end-if
020413     end-if

           .
       1500-EXIT.
           EXIT.

00295  3000-REPORT-REC1.                                         

           MOVE BE-LAST-MAINT-DT        TO DC-BIN-DATE-1    
           MOVE SPACE                   TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO WS-D1-LAST-MAINT-DT
           ELSE
               DISPLAY 'rec1 INVALID MAINT DATE FOR BENE '
                  BE-BENEFICIARY
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
           ADD +1                       TO WS-LINE-COUNT
           MOVE WS-LINE-COUNT           TO AUDIT-ELBENE-REC-SEQ
           MOVE WS-DETAIL1              TO AUDIT-ELBENE-RPT-LINE
           WRITE AUDIT-ELBENE-RECORD
           ADD +1                       TO WS-LINE-COUNT
           MOVE WS-LINE-COUNT           TO AUDIT-ELBENE-REC-SEQ
           MOVE WS-DETAIL2              TO AUDIT-ELBENE-RPT-LINE
           WRITE AUDIT-ELBENE-RECORD

           .
       3000-EXIT.
           EXIT.

00295  3100-REPORT-REC2.                                         

           MOVE BE2-LAST-MAINT-DT       TO DC-BIN-DATE-1    
           MOVE SPACE                   TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO WS-D1-LAST-MAINT-DT
           ELSE
               DISPLAY 'rec2 INVALID MAINT DATE FOR BENE '
               BE2-BENEFICIARY
               MOVE '00/00/0000'        TO WS-D1-LAST-MAINT-DT
           END-IF
00299                                                                   
00302      MOVE BE2-LAST-MAINT-HHMMSS   TO WS-REFORMAT-TIME              
           MOVE WS-REFORMAT-TIME-HH     TO WS-D1-LAST-MAINT-HH
           MOVE WS-REFORMAT-TIME-MM     TO WS-D1-LAST-MAINT-MM

00302      MOVE BE2-LAST-MAINT-BY       TO WS-D1-LAST-MAINT-BY                  
00302      MOVE BE2-BENEFICIARY         TO WS-D1-BENEFICIARY-ID
00308      MOVE BE2-MAIL-TO-NAME        TO WS-D1-MAIL-TO-NAME                 

00309      MOVE BE2-ADDRESS-LINE-1      TO WS-D2-MAIL-TO-ADDR-LN1             
00310      MOVE BE2-ADDRESS-LINE-2      TO WS-D2-MAIL-TO-ADDR-LN2             
00311      MOVE BE2-CITY-STATE          TO WS-D2-MAIL-TO-CITY-STATE           
00312      MOVE BE2-ZIP-CODE            TO WS-D2-MAIL-TO-ZIP                  
00313                                                                   
           ADD +1                       TO WS-LINE-COUNT
           MOVE WS-LINE-COUNT           TO AUDIT-ELBENE-REC-SEQ
           MOVE WS-DETAIL1              TO AUDIT-ELBENE-RPT-LINE
           WRITE AUDIT-ELBENE-RECORD
           ADD +1                       TO WS-LINE-COUNT
           MOVE WS-LINE-COUNT           TO AUDIT-ELBENE-REC-SEQ
           MOVE WS-DETAIL2              TO AUDIT-ELBENE-RPT-LINE
           WRITE AUDIT-ELBENE-RECORD

           .
       3100-EXIT.
           EXIT.


       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.


00372  ABEND-PGM SECTION.                                               
00373                     COPY ELCABEND.                                   CL**4
