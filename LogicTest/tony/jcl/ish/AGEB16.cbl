       IDENTIFICATION DIVISION.
       PROGRAM-ID.  AGEB16.
       AUTHOR.       J. CONDON.

      *REMARKS.
      *    THIS SUB-PROGRAM IS CALLED BY BATCH IDEAL PROGRAMS TO SET
      *    UP PARAMETERS NEEDED BY, AND ISSUE CALL TO, THE IBM BAR CODE
      *    ROUTINE (EANSRC).  PARAMETERS PASSED TO THIS PROGRAM ARE

           EJECT
       ENVIRONMENT DIVISION.
      *
      *    This special-names section is necessary and I'd use the set
      *    statement at the top of this file also.
      *
       SPECIAL-NAMES.
           CALL-CONVENTION 0 IS callC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PARM-FLDS.
           05  PARM-BC-TYPE                PIC X(1).
      *
      *        Note that this has been increased from 28 to 50
      *
           05  PARM-BC-IN                  PIC X(50).
           05  PARM-BC-LEN                 PIC S9(8)   COMP-5.
           05  PARM-BC-OUT                 PIC X(128).
           05  PARM-BC-RC                  PIC S9(8)   COMP-5.
           05  PARM-TEST                   PIC S9(5)    COMP-5.

       LINKAGE SECTION.
       01  PARM-LIST.
          05  BC-LENGTH                    PIC 9(4).
          05  BC-CODE-IN                   PIC X(50).
          05  BC-CODE-OUT                  PIC X(128).

       PROCEDURE DIVISION USING PARM-LIST.

            MOVE 'C' TO PARM-BC-TYPE.
            MOVE BC-CODE-IN TO PARM-BC-IN.
            MOVE '0011600000000000277700001' TO PARM-BC-IN.
            MOVE BC-LENGTH TO PARM-BC-LEN.
            MOVE 27 TO PARM-BC-LEN.
            MOVE 27 to PARM-TEST.
            MOVE 'OUT' TO PARM-BC-OUT.
            DISPLAY 'Calling BarCode'.
      *      SET FUNCPTR TO ENTRY 'bcsubr'.
            CALL callC "BarCode" USING
                BY REFERENCE PARM-BC-TYPE,
                BY REFERENCE PARM-BC-IN,
               	BY REFERENCE PARM-BC-LEN,
                BY REFERENCE PARM-BC-OUT,
                RETURNING PARM-BC-RC.
            CANCEL "BarCode"

            MOVE PARM-BC-LEN TO BC-LENGTH.
            MOVE PARM-BC-OUT TO BC-CODE-OUT.
            DISPLAY 'RC was: ' PARM-BC-RC.
            DISPLAY 'Got ' PARM-BC-LEN ' bytes back'.
            DISPLAY 'Out: "' PARM-BC-OUT '"'.
            GOBACK.
