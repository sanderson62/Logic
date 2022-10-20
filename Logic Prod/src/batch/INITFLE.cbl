       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 INITFLE.


      *AUTHOR.     SUZAN VUKOV.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT INPUT-FILE         ASSIGN TO ELCNTL
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS CF-CONTROL-PRIMARY
                                     FILE STATUS IS
                                       WS-INPUT-FILE-STATUS.


       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE.
                                COPY ELCCNTL.

       WORKING-STORAGE SECTION.
       01  LCP-ABND-CODE                   PIC S999 COMP   VALUE +519.


       01  FILLER.
           05  ABEND-CODE                  PIC X(04).
           05  ABEND-OPTION                PIC X(01).

           05  WS-EOF-SW                   PIC X(01)     VALUE SPACE.
               88  END-OF-INPUT                          VALUE 'Y'.

           05  WS-INPUT-FILE-STATUS        PIC X(02)     VALUE ZERO.
 
           05  WS-RETURN-CODE              PIC S9(03)    VALUE +0.
           05  WS-ZERO                     PIC S9(01)    VALUE +0.
           05  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.

           05  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE ZERO.

      ********************************
       PROCEDURE DIVISION.

       1000-MAIN-LOGIC.

           PERFORM OPEN-FILES                 THRU OPEN-FILES-EXIT

           MOVE LOW-VALUES                    TO CF-CONTROL-PRIMARY
           START INPUT-FILE KEY NOT < CF-CONTROL-PRIMARY
           END-START
           PERFORM 2100-READ-INPUT            THRU 2100-EXIT
               UNTIL END-OF-INPUT 

           PERFORM CLOSE-FILES                THRU CLOSE-FILES-EXIT
           GOBACK.

       2100-READ-INPUT.

           READ INPUT-FILE NEXT RECORD
               AT END
                   SET END-OF-INPUT           TO TRUE
                   GO TO 2100-EXIT
           END-READ

           MOVE ZEROS                         TO CF-CARRIER-CLP-TOL-PCT
           REWRITE CONTROL-FILE       

           .
       2100-EXIT.
           EXIT.
            


       OPEN-FILES.

           OPEN I-O    INPUT-FILE

           IF WS-INPUT-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON IO FILE'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-INPUT-FILE-STATUS       TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF

           .
       OPEN-FILES-EXIT.
           EXIT.


       CLOSE-FILES.

           CLOSE INPUT-FILE

            .
       CLOSE-FILES-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
