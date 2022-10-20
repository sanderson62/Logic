       >> IMP OPTION '-m1 -dv=0 -dci -align=8 -cax'
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDBRST32.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       INPUT-OUTPUT SECTION.
              
       FILE-CONTROL.
       
       SELECT I-SYSTSIN-FILE ASSIGN TO SYSTSIN
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS WS-SYSTSIN-STATUS.
                  
       DATA DIVISION.
       FILE SECTION.
       
       FD  I-SYSTSIN-FILE                                               
           RECORDING MODE IS F.
       01  SYSTSIN-REC                     PIC X(80).
       
      ***********************************************************
       WORKING-STORAGE SECTION.
      ***********************************************************
      
       01  WS-PGM-NAME                     PIC X(8) VALUE SPACES.
        
       01  WS-RET-CODE                     PIC S9(4)  VALUE  0 COMP.   
              
       01  WS-CDBRST32-FIELDS.  
       
           05  WS-BOXX                     PIC X(60)  VALUE  ALL '*'.   
           05  WS-SUB1                     PIC 9(8) COMP.
           05  WS-SUB2                     PIC 9(8) COMP.
           05  WS-SUB3                     PIC 9(8) COMP.
           
       01  WS-SYSTIN-FIELDS.
       
           05  WS-SYSTSIN-STATUS           PIC XX     VALUE '00'.
               88  SYSTSIN-STATUS-OK                  VALUE '00'.
               88  SYSTSIN-NOT-FOUND                  VALUE '35'.

           05  WS-SYSTSIN-EOF-FLAG         PIC X      VALUE 'N'.
               88 SYSTSIN-NOT-AT-EOF                  VALUE 'N'.
               88 SYSTSIN-EOF                         VALUE 'Y'.
               
           05  WS-CONTINUATION-FOUND-FLAG  PIC X      VALUE 'N'.
               88 CONTINUATION-FOUND                  VALUE 'Y'.
               88 CONTINUATION-NOT-FOUND              VALUE 'N'.
               
           05  WS-CONTINUATION-ACTIVE-FLAG PIC X      VALUE 'Y'.
               88 CONTINUATION-ACTIVE                 VALUE 'Y'.
               88 CONTINUATION-NOT-ACTIVE             VALUE 'N'.
               
           05  WS-SYSTSIN-STATEMENT          PIC X(432) VALUE SPACES.
           05  WS-SYSTSIN-KEY-NAME           PIC X(7)   VALUE SPACES.
           05  WS-SYSTSIN-KEY-VALUE          PIC X(100) VALUE SPACES.
           05  WS-SYSTSIN-KEY-VALUE-SIZE     PIC S9(8) COMP.
           05  WS-SYSTSIN-MAX-KEY-VALUE-SIZE PIC S9(8) COMP.
           05  WS-SYSTSIN-PROGRAM-NAME       PIC X(8).
           05  WS-SYSTSIN-PARM-STRING-SIZE   PIC S9(8) COMP.
           05  WS-SYSTSIN-PARM-STRING        PIC X(100).
           
           05  WS-RUN-KEY-PROCESSED-FLAG     PIC X    VALUE 'N'.
               88 RUN-KEY-NOT-PROCESSED               VALUE 'N'.
               88 RUN-KEY-PROCESSED                   VALUE 'Y'.
               
           05  WS-KEY-VALUE-LOCATED-FLAG     PIC X    VALUE 'N'.
               88 KEY-VALUE-NOT-LOCATED               VALUE 'N'.
               88 KEY-VALUE-LOCATED                   VALUE 'Y'.

           05  WS-KEY-VALUE-COPIED-FLAG      PIC X    VALUE 'N'.
               88 KEY-VALUE-NOT-COPIED                VALUE 'N'.
               88 KEY-VALUE-COPIED                    VALUE 'Y'.

           05  WS-KEY-VALUE-QUOTED-FLAG      PIC X    VALUE 'N'.
               88 KEY-VALUE-NOT-QUOTED                VALUE 'N'.
               88 KEY-VALUE-QUOTED                    VALUE 'Y'.

           05  WS-PROCESSING-COMMENT-FLAG    PIC X    VALUE 'N'.
               88 NOT-PROCESSING-COMMENT              VALUE 'N'.
               88 PROCESSING-COMMENT                  VALUE 'Y'.
               
           05  WS-RUN-STATEMENT-FLAG         PIC X    VALUE 'N'.
               88 RUN-STATEMENT-NOT-FOUND             VALUE 'N'.
               88 RUN-STATEMENT-FOUND                 VALUE 'Y'.
               
       01  APPL-MESSAGE-STORAGE.
           05  APPL-PARM-ARGC               PIC 9(4) COMP VALUE ZERO.
           05  APPL-PARM-ARGV.              
               10 APPL-READPARM             PIC X OCCURS 1 TO 100 
                                            DEPENDING ON APPL-PARM-ARGC.
       
       01  COBRST-RC                        PIC 9(9) COMP-5.
           88  CDBRSTR-RC-START-MODE                  VALUE 0.
           88  CDBRSTR-RC-RESTART-MODE                VALUE 4.
           
           88  BPECHKP-RC-COMMITTED                   VALUE 0.
           88  BPECHKP-RC-NO-COMMIT                   VALUE 4.

       01  COBRST-ABEND-CODE                PIC 9(9) COMP-5.
       
       01  COBRST-SAVE-AREA-NUMBER          PIC 9(9) COMP-5.
       
       01  COND-CODE-PASSED                 PIC 9(9) COMP-5.
       
       01  INSTALL-STATUS-CODE              PIC 9(4) COMP VALUE ZEROS.       

       77  SYNCPOINT                        PIC X(8) VALUE 'SYNCPT'.
       
       01  WS-KIXSYS.
           05  ENV-KIXSYS                   PIC X(6)  VALUE 'KIXSYS'.
           05  KIXSYS-PATH                  PIC X(1024) VALUE SPACES.

       LINKAGE SECTION.

      ***********************************************************
      * PARAMETERS FOR CDBRSTR                                  *
      ***********************************************************
      
       01  CDBRSTR-LUW-INDICATOR           PIC S9(4) COMP.
       01  CDBRSTR-SAVE-AREA-NUMBER        PIC 9(4)  COMP.
       01  CDBRSTR-SAVE-WS-START           PIC X(8).
       01  CDBRSTR-SAVE-WS-END             PIC X(8).
       
      ***********************************************************
      * PARAMETERS FOR CDBABEND                                 *
      ***********************************************************
       
       01  CDBABEND-ABCODE                 PIC S9(4) COMP.
       
      ***********************************************************
      * PARM VALUE FOR CDBRSTR32                                *
      ***********************************************************
      
       01  MESSAGE-STORAGE.
           05  PARM-ARGC                   PIC 9(4)  COMP.
           05  PARM-ARGV.
               10 READPARM                 PIC X 
                                           OCCURS 1 TO 100 
                                           DEPENDING ON PARM-ARGC.
                                           
       PROCEDURE DIVISION USING MESSAGE-STORAGE.
       
      ***********************************************************
       MAIN-SECTION SECTION.       
      ***********************************************************      
            
           DISPLAY ENV-KIXSYS UPON ENVIRONMENT-NAME.
           ACCEPT KIXSYS-PATH FROM ENVIRONMENT-VALUE.

           PERFORM PROCESS-PARM-ROUTINE
                 THRU PROCESS-PARM-ROUTINE-EXIT.
                 
           PERFORM PROCESS-SYSTSIN-ROUTINE 
                 THRU PROCESS-SYSTSIN-ROUTINE-EXIT.  
                 
           IF WS-PGM-NAME EQUAL SPACES
              DISPLAY 'CDBRST32: '
                      'PROGRAM NAME NOT FOUND IN PARMS OR SYSTSIN'
              SET RETURN-CODE TO 0004
              PERFORM ABEND-ROUTINE THRU ABEND-ROUTINE-EXIT
           END-IF.



      *    deinstalling the exit procedure.
           CALL "CBL_EXIT_PROC" USING 254 "CBLEXIT"
                                 RETURNING INSTALL-STATUS-CODE.
      *    deinstalling the error procedure.
           CALL "CBL_ERROR_PROC" USING 254 "CBLERROR"
                                 RETURNING INSTALL-STATUS-CODE.


           CALL 'cobrsp_cdbrst32' USING WS-PGM-NAME
                                        COBRST-RC
                                        COND-CODE-PASSED
                                        APPL-MESSAGE-STORAGE.
                                        
      *    installing the exit procedure.
           CALL "CBL_EXIT_PROC" USING 0 "CBLEXIT"
                                 RETURNING INSTALL-STATUS-CODE.
      *    installing the error procedure.
           CALL "CBL_ERROR_PROC" USING 0 "CBLERROR"
                                 RETURNING INSTALL-STATUS-CODE.
                                        
           SET RETURN-CODE TO COBRST-RC.
           
           IF COND-CODE-PASSED NOT = 0
              PERFORM ABEND-ROUTINE
           ELSE
              GOBACK
           END-IF.
           
           EXIT PROGRAM.
      
       MAIN-SECTION-EXIT.     
           GOBACK.              

      ***********************************************************       
       ENTRY 'CDBRSTR' USING CDBRSTR-LUW-INDICATOR
                             CDBRSTR-SAVE-AREA-NUMBER
                             CDBRSTR-SAVE-WS-START
                             CDBRSTR-SAVE-WS-END.           
      ***********************************************************
           
           SET COBRST-SAVE-AREA-NUMBER TO CDBRSTR-SAVE-AREA-NUMBER.  
      
           CALL 'cobrsp_cdbrstr' USING COBRST-SAVE-AREA-NUMBER
                                       CDBRSTR-SAVE-WS-START
                                       CDBRSTR-SAVE-WS-END
                                       COBRST-RC
                                       COBRST-ABEND-CODE.

           IF COBRST-ABEND-CODE NOT = 0
              SET RETURN-CODE TO COBRST-ABEND-CODE
              PERFORM ABEND-ROUTINE
           END-IF.

           SET RETURN-CODE TO COBRST-RC.
           GOBACK.
           
      ***********************************************************       
       ENTRY 'BPECHKP'.
      ***********************************************************       
         
           CALL 'cobrsp_bpechkp' USING COBRST-RC COBRST-ABEND-CODE.
           
           IF COBRST-ABEND-CODE NOT = 0
              SET RETURN-CODE TO COBRST-ABEND-CODE
              PERFORM ABEND-ROUTINE
           END-IF.

           IF COBRST-RC = 0 AND KIXSYS-PATH NOT = SPACES
              CALL 'kixvsam' USING SYNCPOINT.

           SET RETURN-CODE TO COBRST-RC.
           GOBACK.
           
      ***********************************************************       
       ENTRY 'CDBABEND' USING CDBABEND-ABCODE.
      ***********************************************************       
                      
           CALL 'cobrsp_cdbabend' USING CDBABEND-ABCODE.
           MOVE CDBABEND-ABCODE TO RETURN-CODE.
           STOP RUN.
           EXIT-PROGRAM.   

      ***********************************************************       
       ABEND-ROUTINE.                                               
      ***********************************************************    
                 
           MOVE RETURN-CODE TO WS-RET-CODE.   
           
      *    DISPLAY ' '.
      *    DISPLAY WS-BOXX.
      *    DISPLAY '**  CDBRST32: ABENDING WITH ABEND CODE - ' 
      *            WS-RET-CODE
      *            '            ***'.
      *    DISPLAY WS-BOXX.
      *    DISPLAY ' '.
           
           CALL "CCF_SET_RETCODE" USING WS-RET-CODE.
           CALL "CCF_SET_CONDCODE" USING WS-RET-CODE.
           SET RETURN-CODE TO WS-RET-CODE.
           STOP RUN.
           EXIT.
       ABEND-ROUTINE-EXIT.                                         
           EXIT.

      ***********************************************************      
      *
        PROCESS-PARM-ROUTINE.
      *
      * Under some circumstances, BPE PRS will assume that the 
      * beginning of the parameter string is addressed to him and 
      * contains a member name. The member name will be used for 
      * the allocations of the PROGRAM. BPE PRS will try to isolate
      * the first % sign in the parameter string as the end of name
      * marker. The resultant string (positions 1-up to the % sign 
      * position) must be a valid library member name (1-8 characters,
      * alphabetic, etc.). Obviously, the member name, including the
      * % sign is stripped off the string which is transferred to the
      * application program.       
      *
      ***********************************************************       
                                 
           MOVE ZERO TO WS-SUB2.
           
           PERFORM VARYING WS-SUB1 FROM 1 BY 1 
                   UNTIL WS-SUB1 > 9   OR 
                   WS-SUB1 > PARM-ARGC OR
                   WS-SUB2 NOT ZERO
                   
               IF READPARM(WS-SUB1) = '%'
                  ADD 1 TO WS-SUB2
                  SET APPL-PARM-ARGC TO PARM-ARGC
                  SUBTRACT WS-SUB1 FROM APPL-PARM-ARGC
                      
                  IF WS-SUB1 < PARM-ARGC
                     MOVE PARM-ARGV(WS-SUB1 + 1:) TO APPL-PARM-ARGV
                  END-IF
                  
                  IF WS-SUB1 NOT EQUAL 1
                     MOVE PARM-ARGV(1:WS-SUB1 - 1) TO WS-PGM-NAME
                  END-IF
                  
               END-IF
           END-PERFORM.  
           EXIT.
        PROCESS-PARM-ROUTINE-EXIT.
           EXIT.
           
      ***********************************************************       
       PROCESS-SYSTSIN-ROUTINE.
      *
      * BPE PRS will process the following control statements in the
      * SYSTSIN data set:
      *
      * DSN - The DSN control statement instructs BPE PRS to connect to
      *       a specific DB2. Its format is:
      *
      * DSN SYSTEM (system-id) RETRY (retriesthreshold)
      *
      * system-id: the DB2 subsystem identifier to use during this run. 
      *            This parameter is ignored by CDBRST32.
      *
      * retries-threshold: the maximum number of retries to perform if 
      *                    the connect to DB2 fails. This parameter is 
      *                    ignored by CDBRST32.
      *
      * RUN PROGRAM - The RUN PROGRAM statement instructs BPE PRS to
      *               run a specific application program. Its format 
      *               is:
      *
      * RUN PROGRAM(program) +
      *             PLAN(plan-name) +
      *             PARM('parm') +
      *             LIBRARY('library')
      *
      * program: application program name. If specified, this parameter 
      *          will override the program name optionally specified on
      *          the PARM for EXEC PGM=CDBRST32.
      *
      * plan name: application plan name. This parameter is ignored by
      *            CDBRST32.
      *
      * parm: optional character string which is passed the application.
      *       It can be up to 100 characters in length, and it must be 
      *       in quotes. It can contain any character, including a 
      *       quote. If specified, this parameter will override the
      *       parm value specified on the PARM for EXEC PGM=CDBRST32.
      *
      * library: MVS library from which the program will be loaded. 
      *          This parameter is optional. If the parameter is 
      *          omitted, standard MVS load procedures are followed
      *          (i.e. STEPLIB, etc.). The parameter may be abbreviated
      *          as LIB. This parameter is ignored by CDBRST32.
      *
      * END - The END statement has no parameters and is used to close
      *       the connection to DB2. This statement is ignored by 
      *       CDBRST32.
      *
      * The control statement are free format. Each statement can span
      * a maximum of 6 input records, and each record can contain data
      * anywhere between columns 1 and 72 (columns 73-80 are ignored). 
      * If the last non-blank character of a record is a plus (+) or a
      * minus (-) sign, the next record is treated as a continuation. 
      * Otherwise, the next record is treated as the beginning of a new
      * statement. When an input record has a continuation sign, the 
      * continuation sign and all blank characters that follow it are
      * ignored. In addition, all blank characters preceding the first 
      * non-blank on the following input record are also ignored.
      *
      ***********************************************************       
           
           OPEN INPUT I-SYSTSIN-FILE.
           
           IF SYSTSIN-NOT-FOUND
              GO TO PROCESS-SYSTSIN-ROUTINE-EXIT.
              
           IF NOT SYSTSIN-STATUS-OK
              DISPLAY 'CDBRST32: UNABLE TO OPEN SYSTSIN ' 
                      'STATUS IS ' WS-SYSTSIN-STATUS
              GO TO PROCESS-SYSTSIN-ROUTINE-EXIT.

           READ I-SYSTSIN-FILE
           AT END 
              SET SYSTSIN-EOF TO TRUE
           END-READ.
           
           SET CONTINUATION-ACTIVE TO TRUE.
           SET CONTINUATION-NOT-FOUND TO TRUE.
           MOVE 0 TO WS-SUB2.
           MOVE SPACES TO WS-SYSTSIN-STATEMENT.
              
           PERFORM UNTIL SYSTSIN-EOF
           
              PERFORM VARYING WS-SUB1 FROM 1 BY 1
                    UNTIL WS-SUB1 > 73 OR
                    CONTINUATION-FOUND
                    OR SYSTSIN-REC(WS-SUB1:) = SPACES
                    
                 IF CONTINUATION-ACTIVE AND
                    SYSTSIN-REC(WS-SUB1:1) NOT = SPACE
                        SET CONTINUATION-NOT-ACTIVE TO TRUE
                 END-IF
                 
                 IF CONTINUATION-NOT-ACTIVE
                    IF SYSTSIN-REC(WS-SUB1:1) = '+' OR
                       SYSTSIN-REC(WS-SUB1:1) = '-'
                       IF WS-SUB1 = 72 OR
                          SYSTSIN-REC(WS-SUB1 + 1:) EQUAL SPACES
                          SET CONTINUATION-FOUND TO TRUE
                          SET CONTINUATION-ACTIVE TO TRUE
                       END-IF
                    END-IF
                 END-IF
              
                 IF CONTINUATION-NOT-FOUND AND CONTINUATION-NOT-ACTIVE
                 
                    ADD 1 TO WS-SUB2
                    
                    IF WS-SUB2 > 432
                       CLOSE I-SYSTSIN-FILE
                       DISPLAY 'CDBRST32: SYSTSIN STATEMENT '
                               'EXCEEDS 432 CHARACTERS:'
                       DISPLAY WS-SYSTSIN-STATEMENT
                       SET RETURN-CODE TO 0020
                       PERFORM ABEND-ROUTINE THRU ABEND-ROUTINE-EXIT
                    END-IF
                    
                    MOVE SYSTSIN-REC(WS-SUB1:1) 
                         TO WS-SYSTSIN-STATEMENT(WS-SUB2:1)
                 END-IF
              
              END-PERFORM
              
              IF CONTINUATION-NOT-FOUND AND WS-SUB2 NOT ZERO
                    
                 PERFORM PROCESS-SYSTSIN-STATEMENT 
                    THRU PROCESS-SYSTSIN-STATEMENT-EXIT
                 
                 SET CONTINUATION-ACTIVE TO TRUE
                 SET CONTINUATION-NOT-FOUND TO TRUE
                 MOVE 0 TO WS-SUB2
                 MOVE SPACES TO WS-SYSTSIN-STATEMENT
                          
              END-IF
              
              IF CONTINUATION-FOUND
                 SET CONTINUATION-ACTIVE TO TRUE
                 SET CONTINUATION-NOT-FOUND TO TRUE
              END-IF
              
              READ I-SYSTSIN-FILE
              AT END
                 SET SYSTSIN-EOF TO TRUE
              END-READ
              
           END-PERFORM.
           
           CLOSE I-SYSTSIN-FILE.
           EXIT.
       PROCESS-SYSTSIN-ROUTINE-EXIT.
           EXIT.

      ***********************************************************   
       PROCESS-SYSTSIN-STATEMENT.
      ***********************************************************   
                  
           SET NOT-PROCESSING-COMMENT TO TRUE.
           SET RUN-STATEMENT-NOT-FOUND TO TRUE.
                      
           PERFORM VARYING WS-SUB1 FROM 1 BY 1
              UNTIL WS-SUB1 > WS-SUB2
              
              IF PROCESSING-COMMENT AND
                 WS-SYSTSIN-STATEMENT(WS-SUB1:2) = "*/"
                 ADD 2 TO WS-SUB1
                 SET NOT-PROCESSING-COMMENT TO TRUE
              END-IF
              
              IF WS-SYSTSIN-STATEMENT(WS-SUB1:2) = "/*"
                 SET PROCESSING-COMMENT TO TRUE
              END-IF
              
              IF NOT-PROCESSING-COMMENT AND 
                 RUN-STATEMENT-NOT-FOUND AND
                 WS-SYSTSIN-STATEMENT(WS-SUB1:1) NOT = SPACE
              
                 IF WS-SYSTSIN-STATEMENT(WS-SUB1:4) = 'DSN '
                    GO TO PROCESS-SYSTSIN-STATEMENT-EXIT
                 END-IF
                    
                 IF WS-SYSTSIN-STATEMENT(WS-SUB1:4) = 'END '
                    GO TO PROCESS-SYSTSIN-STATEMENT-EXIT
                 END-IF
                    
                 IF WS-SYSTSIN-STATEMENT(WS-SUB1:4) = 'RUN '
                    SET RUN-STATEMENT-FOUND TO TRUE
                    ADD 3 TO WS-SUB1
                 ELSE
                    DISPLAY 'CDBRST32: WARNING, UNRECOGNIZED STATEMENT '
                            'FROM SYSTSIN WILL BE IGNORED'
                    DISPLAY WS-SYSTSIN-STATEMENT(1:WS-SUB2)
                    DISPLAY ' '
                 END-IF
                    
              END-IF                

              IF NOT-PROCESSING-COMMENT AND 
                 RUN-STATEMENT-FOUND AND
                 WS-SYSTSIN-STATEMENT(WS-SUB1:1) NOT = SPACE
                 
                 SET RUN-KEY-NOT-PROCESSED TO TRUE
              
                 IF WS-SYSTSIN-STATEMENT(WS-SUB1:7) = 'PROGRAM'  
                    SET RUN-KEY-PROCESSED TO TRUE
                    ADD 7 TO WS-SUB1
                    SET WS-SYSTSIN-MAX-KEY-VALUE-SIZE TO 8
                    PERFORM PROCESS-SYSTSIN-KEY-VALUE
                       THRU PROCESS-SYSTSIN-KEY-VALUE-EXIT
                    MOVE 
                      WS-SYSTSIN-KEY-VALUE(1:WS-SYSTSIN-KEY-VALUE-SIZE)
                         TO WS-PGM-NAME
                 END-IF
                 
      * PROGRAM CAN BE ABBREVIATED AS PROG
                 
                 IF WS-SYSTSIN-STATEMENT(WS-SUB1:4) = 'PROG' AND
                    RUN-KEY-NOT-PROCESSED
                    SET RUN-KEY-PROCESSED TO TRUE
                    ADD 4 TO WS-SUB1
                    SET WS-SYSTSIN-MAX-KEY-VALUE-SIZE TO 8
                    PERFORM PROCESS-SYSTSIN-KEY-VALUE
                       THRU PROCESS-SYSTSIN-KEY-VALUE-EXIT
                    MOVE 
                      WS-SYSTSIN-KEY-VALUE(1:WS-SYSTSIN-KEY-VALUE-SIZE)
                         TO WS-PGM-NAME
                 END-IF
                       
                 IF WS-SYSTSIN-STATEMENT(WS-SUB1:4) = 'PARM' AND
                    RUN-KEY-NOT-PROCESSED
                    SET RUN-KEY-PROCESSED TO TRUE
                    ADD 4 TO WS-SUB1
                    SET WS-SYSTSIN-MAX-KEY-VALUE-SIZE TO 100
                    PERFORM PROCESS-SYSTSIN-KEY-VALUE
                       THRU PROCESS-SYSTSIN-KEY-VALUE-EXIT
                    IF WS-SYSTSIN-KEY-VALUE-SIZE NOT = 0
                       SET APPL-PARM-ARGC TO WS-SYSTSIN-KEY-VALUE-SIZE
                       MOVE WS-SYSTSIN-KEY-VALUE(1:APPL-PARM-ARGC)
                         TO APPL-PARM-ARGV
                    END-IF
                 END-IF
                 
                 IF WS-SYSTSIN-STATEMENT(WS-SUB1:7) = 'LIBRARY' AND
                    RUN-KEY-NOT-PROCESSED
                    SET RUN-KEY-PROCESSED TO TRUE
                    ADD 7 TO WS-SUB1
                    SET WS-SYSTSIN-MAX-KEY-VALUE-SIZE TO 100
                    PERFORM PROCESS-SYSTSIN-KEY-VALUE
                       THRU PROCESS-SYSTSIN-KEY-VALUE-EXIT
                 END-IF
                 
                 IF WS-SYSTSIN-STATEMENT(WS-SUB1:3) = 'LIB' AND
                    RUN-KEY-NOT-PROCESSED
                    SET RUN-KEY-PROCESSED TO TRUE
                    ADD 3 TO WS-SUB1
                    SET WS-SYSTSIN-MAX-KEY-VALUE-SIZE TO 100
                    PERFORM PROCESS-SYSTSIN-KEY-VALUE
                       THRU PROCESS-SYSTSIN-KEY-VALUE-EXIT
                 END-IF
                 
                 IF WS-SYSTSIN-STATEMENT(WS-SUB1:4) = 'PLAN' AND
                    RUN-KEY-NOT-PROCESSED
                    SET RUN-KEY-PROCESSED TO TRUE
                    ADD 4 TO WS-SUB1
                    SET WS-SYSTSIN-MAX-KEY-VALUE-SIZE TO 100
                    PERFORM PROCESS-SYSTSIN-KEY-VALUE
                       THRU PROCESS-SYSTSIN-KEY-VALUE-EXIT
                 END-IF
                 
                 IF RUN-KEY-NOT-PROCESSED 
                    DISPLAY 'CDBRST32: WARNING, UNRECOGNIZED SYSTSIN'
                            ' RUN OPTION STARTING AT CHAR POSITION ' 
                            WS-SUB1 ' WILL BE IGNORED'
                    DISPLAY WS-SYSTSIN-STATEMENT(1:WS-SUB2)
                    DISPLAY ' '
                    
                    PERFORM VARYING WS-SUB1 FROM WS-SUB1 BY 1
                       UNTIL WS-SYSTSIN-STATEMENT(WS-SUB1:1) = '(' OR
                             WS-SYSTSIN-STATEMENT(WS-SUB1:1) = ' ' OR
                             WS-SUB1 > WS-SUB2
                    END-PERFORM
                 
                    SET WS-SYSTSIN-MAX-KEY-VALUE-SIZE TO 100
                    PERFORM PROCESS-SYSTSIN-KEY-VALUE
                       THRU PROCESS-SYSTSIN-KEY-VALUE-EXIT
                 END-IF
                 
                 IF KEY-VALUE-NOT-COPIED
                    CLOSE I-SYSTSIN-FILE
                    DISPLAY 'CDBRST32: INVALID SYSTSIN RUN STATEMENT:'
                    DISPLAY WS-SYSTSIN-STATEMENT(1:WS-SUB2)
                    SET RETURN-CODE TO 0028
                    PERFORM ABEND-ROUTINE THRU ABEND-ROUTINE-EXIT
                 END-IF
                 
              END-IF
              
           END-PERFORM.
           EXIT.                     
       PROCESS-SYSTSIN-STATEMENT-EXIT.
           EXIT.
           
      ***********************************************************   
       PROCESS-SYSTSIN-KEY-VALUE.
      ***********************************************************   
                  
           SET KEY-VALUE-NOT-LOCATED TO TRUE.
           SET KEY-VALUE-NOT-QUOTED TO TRUE.
           SET KEY-VALUE-NOT-COPIED TO TRUE.
           SET WS-SUB3 TO 0.
           
           PERFORM VARYING WS-SUB1 FROM WS-SUB1 BY 1
              UNTIL WS-SUB1 > WS-SUB2 OR
                    KEY-VALUE-COPIED
                                  
              IF KEY-VALUE-NOT-LOCATED AND
                 WS-SYSTSIN-STATEMENT(WS-SUB1:1) = '('
                 SET KEY-VALUE-LOCATED TO TRUE
                 IF WS-SYSTSIN-STATEMENT(WS-SUB1 + 1:1) = "'"
                    SET KEY-VALUE-QUOTED TO TRUE
                    ADD 2 TO WS-SUB1
                 ELSE
                    ADD 1 TO WS-SUB1
                 END-IF
              END-IF
                 
              IF KEY-VALUE-NOT-LOCATED AND
                 WS-SYSTSIN-STATEMENT(WS-SUB1:1) NOT = ' '
                 CLOSE I-SYSTSIN-FILE
                 DISPLAY 'CDBRST32: SYSTSIN STATEMENT '
                         'NOT SPECIFIED CORRECTLY:'
                 DISPLAY WS-SYSTSIN-STATEMENT(1:WS-SUB2)
                 SET RETURN-CODE TO 0024
                 PERFORM ABEND-ROUTINE THRU ABEND-ROUTINE-EXIT
              END-IF
                 
              IF KEY-VALUE-LOCATED
                 IF KEY-VALUE-QUOTED AND
                    WS-SYSTSIN-STATEMENT(WS-SUB1:2) = "')"
                    ADD 1 TO WS-SUB1
                    MOVE WS-SUB3 TO WS-SYSTSIN-KEY-VALUE-SIZE
                    SET KEY-VALUE-COPIED TO TRUE
                 ELSE
                    IF KEY-VALUE-NOT-QUOTED AND
                       WS-SYSTSIN-STATEMENT(WS-SUB1:1) = ")"
                       MOVE WS-SUB3 TO WS-SYSTSIN-KEY-VALUE-SIZE
                       SET KEY-VALUE-COPIED TO TRUE
                    ELSE
                       ADD 1 TO WS-SUB3
                       IF WS-SUB3 > WS-SYSTSIN-MAX-KEY-VALUE-SIZE
                          CLOSE I-SYSTSIN-FILE
                          DISPLAY 'CDBRST32: VALUE TOO LARGE: '
                          DISPLAY WS-SYSTSIN-STATEMENT(1:WS-SUB2)
                          SET RETURN-CODE TO 0028
                          PERFORM ABEND-ROUTINE THRU ABEND-ROUTINE-EXIT
                       END-IF
                       MOVE WS-SYSTSIN-STATEMENT(WS-SUB1:1)
                            TO WS-SYSTSIN-KEY-VALUE(WS-SUB3:1)
                    END-IF
                 END-IF    
              END-IF
              
           END-PERFORM.
           EXIT.
       PROCESS-SYSTSIN-KEY-VALUE-EXIT.
           EXIT.           

