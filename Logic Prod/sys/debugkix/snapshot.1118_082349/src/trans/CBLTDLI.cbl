      *****************************************************************
      *                                                               *
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      * Version 1.0                                                   *
      *                                                               *
      *****************************************************************
      *                                                               *
      * Warning: when using Dell COBOL compiler, do not specify       *
      * options -sl1 or -sl2                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CBLTDLI.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
         01 WS-PTR-TABLE.
            05 WS-PTR  POINTER OCCURS 11.
         01 WS-COUNT   PIC S9(9) COMP-5.

       LINKAGE SECTION.
         01 PX01 PIC X(04).
         01 PX02 PIC X(08).
         01 PX03 PIC X(08).
         01 PX04 PIC X(08).
         01 PX05 PIC X(08).
         01 PX06 PIC X(08).
         01 PX07 PIC X(08).
         01 PX08 PIC X(08).
         01 PX09 PIC X(08).
         01 PX10 PIC X(08).
         01 PX11 PIC X(08).

         01 LS-FUNC             PIC X(04).

       PROCEDURE DIVISION USING PX01 PX02
                           PX03 PX04 PX05
                           PX06 PX07 PX08
                           PX09 PX10 PX11.

         MOVE LOW-VALUE TO WS-PTR-TABLE.
         IF ADDRESS OF PX01 = NULL
         THEN
           DISPLAY 'CBLTDLI[IMS TM]: ERROR NO ARGUMENT BEING PASSED'
           GOBACK
         ELSE IF ADDRESS OF PX02 = NULL
         THEN
           SET ADDRESS OF LS-FUNC TO ADDRESS OF PX01
           SET WS-COUNT = 0
         ELSE IF ADDRESS OF PX03 = NULL
         THEN
           SET ADDRESS OF LS-FUNC TO ADDRESS OF PX01
           SET WS-PTR(01) TO ADDRESS OF PX02
           SET WS-COUNT = 1
         ELSE IF ADDRESS OF PX04 = NULL
         THEN
           SET ADDRESS OF LS-FUNC TO ADDRESS OF PX01
           SET WS-PTR(01) TO ADDRESS OF PX02
           SET WS-PTR(02) TO ADDRESS OF PX03
           SET WS-COUNT = 2
         ELSE IF ADDRESS OF PX05 = NULL
         THEN
           SET ADDRESS OF LS-FUNC TO ADDRESS OF PX01
           SET WS-PTR(01) TO ADDRESS OF PX02
           SET WS-PTR(02) TO ADDRESS OF PX03
           SET WS-PTR(03) TO ADDRESS OF PX04
           SET WS-COUNT = 3
         ELSE IF ADDRESS OF PX06 = NULL
         THEN
           SET ADDRESS OF LS-FUNC TO ADDRESS OF PX01
           SET WS-PTR(01) TO ADDRESS OF PX02
           SET WS-PTR(02) TO ADDRESS OF PX03
           SET WS-PTR(03) TO ADDRESS OF PX04
           SET WS-PTR(04) TO ADDRESS OF PX05
           SET WS-COUNT = 4
         ELSE IF ADDRESS OF PX07 = NULL
         THEN
           SET ADDRESS OF LS-FUNC TO ADDRESS OF PX01
           SET WS-PTR(01) TO ADDRESS OF PX02
           SET WS-PTR(02) TO ADDRESS OF PX03
           SET WS-PTR(03) TO ADDRESS OF PX04
           SET WS-PTR(04) TO ADDRESS OF PX05
           SET WS-PTR(05) TO ADDRESS OF PX06
           SET WS-COUNT = 5
         ELSE IF ADDRESS OF PX08 = NULL
           SET ADDRESS OF LS-FUNC TO ADDRESS OF PX01
           SET WS-PTR(01) TO ADDRESS OF PX02
           SET WS-PTR(02) TO ADDRESS OF PX03
           SET WS-PTR(03) TO ADDRESS OF PX04
           SET WS-PTR(04) TO ADDRESS OF PX05
           SET WS-PTR(05) TO ADDRESS OF PX06
           SET WS-PTR(06) TO ADDRESS OF PX07
           SET WS-COUNT = 6
         ELSE IF ADDRESS OF PX09 = NULL
           SET ADDRESS OF LS-FUNC TO ADDRESS OF PX01
           SET WS-PTR(01) TO ADDRESS OF PX02
           SET WS-PTR(02) TO ADDRESS OF PX03
           SET WS-PTR(03) TO ADDRESS OF PX04
           SET WS-PTR(04) TO ADDRESS OF PX05
           SET WS-PTR(05) TO ADDRESS OF PX06
           SET WS-PTR(06) TO ADDRESS OF PX07
           SET WS-PTR(07) TO ADDRESS OF PX08
           SET WS-COUNT = 7
         ELSE IF ADDRESS OF PX10 = NULL
           SET ADDRESS OF LS-FUNC TO ADDRESS OF PX01
           SET WS-PTR(01) TO ADDRESS OF PX02
           SET WS-PTR(02) TO ADDRESS OF PX03
           SET WS-PTR(03) TO ADDRESS OF PX04
           SET WS-PTR(04) TO ADDRESS OF PX05
           SET WS-PTR(05) TO ADDRESS OF PX06
           SET WS-PTR(06) TO ADDRESS OF PX07
           SET WS-PTR(07) TO ADDRESS OF PX08
           SET WS-PTR(08) TO ADDRESS OF PX09
           SET WS-COUNT = 8
         ELSE IF ADDRESS OF PX11 = NULL
           SET ADDRESS OF LS-FUNC TO ADDRESS OF PX01
           SET WS-PTR(01) TO ADDRESS OF PX02
           SET WS-PTR(02) TO ADDRESS OF PX03
           SET WS-PTR(03) TO ADDRESS OF PX04
           SET WS-PTR(04) TO ADDRESS OF PX05
           SET WS-PTR(05) TO ADDRESS OF PX06
           SET WS-PTR(06) TO ADDRESS OF PX07
           SET WS-PTR(07) TO ADDRESS OF PX08
           SET WS-PTR(08) TO ADDRESS OF PX09
           SET WS-PTR(09) TO ADDRESS OF PX10
           SET WS-COUNT = 9
         ELSE
           SET ADDRESS OF LS-FUNC TO ADDRESS OF PX01
           SET WS-PTR(01) TO ADDRESS OF PX02
           SET WS-PTR(02) TO ADDRESS OF PX03
           SET WS-PTR(03) TO ADDRESS OF PX04
           SET WS-PTR(04) TO ADDRESS OF PX05
           SET WS-PTR(05) TO ADDRESS OF PX06
           SET WS-PTR(06) TO ADDRESS OF PX07
           SET WS-PTR(07) TO ADDRESS OF PX08
           SET WS-PTR(08) TO ADDRESS OF PX09
           SET WS-PTR(09) TO ADDRESS OF PX10
           SET WS-PTR(10) TO ADDRESS OF PX11
           SET WS-COUNT = 10
         END-IF.
Debug *  DISPLAY 'CBLTDLI[IMS TM|' LS-FUNC ']: invoking KXIMSDC with ' 
Debug *           WS-COUNT ' parameters'.
         CALL 'KXIMSDC' USING LS-FUNC WS-COUNT WS-PTR-TABLE.
Debug *  DISPLAY 'CBLTDLI[IMS TM|' LS-FUNC ']: after call to KXIMSDC'.
         GOBACK.

 
