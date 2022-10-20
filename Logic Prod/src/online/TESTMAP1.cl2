       IDENTIFICATION DIVISION.
       PROGRAM-ID.     TESTMAP.
      ***********************************************************
      * TEST MAPS
      ***********************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01    FILLER.
           05  MAPNAME.
               10 MP-CHR   OCCURS 8 TIMES PIC X.
           05  SETNAME.
               10 ST-CHR   OCCURS 8 TIMES PIC X.
           05 SCREEN-IMAGE.
              10 SC-CHR              OCCURS 80 TIMES PIC X.
           05 SCREEN-LENGTH         PIC S9(4)  COMP VALUE +80.
           05 SUB1                         PIC 999 VALUE 0.
           05 SUB2                         PIC 999 VALUE 0.

       PROCEDURE DIVISION.
       100-MAIN-CONTROL SECTION.
      ***********************************************************
      * RECEIVE SCREEN
      ***********************************************************
           EXEC CICS RECEIVE
                     INTO(SCREEN-IMAGE)
                     LENGTH(SCREEN-LENGTH)
                     END-EXEC.

      ***********************************************************
      * CHECK FOR MAPNAME MAPSET
      ***********************************************************
           MOVE SPACES TO MAPNAME SETNAME.
           MOVE 5 TO SUB1.
           MOVE 1 TO SUB2.
           PERFORM MAP-LOOP THRU MAP-EXIT.

      ***********************************************************
      * SENT MAP
      ***********************************************************
           IF SETNAME = SPACES
               EXEC CICS SEND MAP(MAPNAME)
                         FROM (SETNAME)
                         CURSOR
                         MAPONLY
                         ERASE
               END-EXEC
           ELSE
               EXEC CICS SEND MAP(MAPNAME)
                         MAPSET(SETNAME)
                         MAPONLY
                         ERASE
               END-EXEC.


           EXEC CICS RETURN END-EXEC.
           GOBACK.
       MAP-LOOP.
           ADD 1 TO SUB1.
           IF SUB1 GREATER THAN SCREEN-LENGTH
               GO TO MAP-EXIT.
           IF SUB2 GREATER THAN 8
              MOVE 1 TO SUB2
              GO TO MAP-LOOP2.
           IF SC-CHR(SUB1) = SPACES
               MOVE 1 TO SUB2
               GO TO MAP-LOOP2.
           MOVE SC-CHR(SUB1) TO MP-CHR(SUB2).
           ADD 1 TO SUB2.
           GO TO MAP-LOOP.
       MAP-LOOP2.
           ADD 1 TO SUB1.
           IF SUB1 GREATER THAN SCREEN-LENGTH
               GO TO MAP-EXIT.
           IF SUB2 GREATER THAN 8
              GO TO MAP-EXIT.
           IF SC-CHR(SUB1) = SPACES
              GO TO MAP-EXIT.
           MOVE SC-CHR(SUB1) TO ST-CHR(SUB2).
           ADD 1 TO SUB2.
           GO TO MAP-LOOP2.
       MAP-EXIT.  EXIT.
