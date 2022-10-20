       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID.  ISD053.                                             00020000
                                                                        00030000
      ******************************************************************00040000
      *                                                                *00070000
      * THIS PROGRAM REMOVES BLANK LINES FROM A SET OF ADDRESS LINES.  *00100000
      *                                                                *00140000
      ******************************************************************00160000
                                                                        00180000
       DATA DIVISION.                                                   00370000
                                                                        00620000
       WORKING-STORAGE SECTION.                                         00630000
                                                                        00640000
       01  PARM-SUB  PIC S9(4)  BINARY.                                 00691000
       01  WORK-SUB  PIC S9(4)  BINARY.                                 00691100
                                                                        00692000
       01  WORK-TABLE.                                                  00693000
           05  WORK-ADDR OCCURS 5 TIMES PIC X(50).                      00700000
                                                                        00710000
       LINKAGE SECTION.                                                 00960000
                                                                        00961000
       01  PARM-TABLE.                                                  00961100
           05  PARM-ADDR OCCURS 5 TIMES PIC X(50).                      00961200
                                                                        00961300
                                                                        00961600
                                                                        00962000
           EJECT                                                        00963000
      *                                                                 00964000
       PROCEDURE DIVISION USING PARM-TABLE.                             00965002
      *                                                                 00970000
           MOVE SPACES TO WORK-TABLE                                    01671100
           MOVE +1 TO WORK-SUB                                          01671200
           MOVE +1 TO PARM-SUB                                          01671300
                                                                        01671400
           PERFORM UNTIL PARM-SUB > 5                                   01672001
              IF PARM-ADDR(PARM-SUB) NOT = SPACE                        01672400
                 MOVE PARM-ADDR(PARM-SUB) TO WORK-ADDR(WORK-SUB)        01672600
                 ADD +1 TO WORK-SUB                                     01672700
              END-IF                                                    01672800
              ADD +1 TO PARM-SUB                                        01672900
           END-PERFORM                                                  01673000
                                                                        01673300
           PERFORM VARYING WORK-SUB FROM 1 BY 1 UNTIL WORK-SUB > 5      01673400
              MOVE WORK-SUB TO PARM-SUB                                 01673500
              MOVE WORK-ADDR(WORK-SUB) TO PARM-ADDR(PARM-SUB)           01673600
           END-PERFORM                                                  01673700
                                                                        01673800
           GOBACK.                                                      01673900
                                                                        01674000
