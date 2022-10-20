       IDENTIFICATION DIVISION.
       PROGRAM-ID. EL549.
       AUTHOR.     PABLO.
       DATE-COMPILED.
030404******************************************************************
030404*                   C H A N G E   L O G
030404*
030404* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030404*-----------------------------------------------------------------
030404*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030404* EFFECTIVE    NUMBER
030404*-----------------------------------------------------------------
030404* 030404                   SMVA  ADD PROCESSING FOR DCC 
052704* 052704  IR2004052400001  SMVA  CHG DELIMITER FROM TAB TO ; 
092707* 092707  CR2007061900001  PEMA  ADD REPORT SWITCHES TO EXTRACT
042309* 042309  CR2009032400004  PEMA  ADD FIELDS TO EXTRACT
030404******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERREIN           ASSIGN TO ERREIN
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS RE-CONTROL-PRIMARY
                                   FILE STATUS IS ERREIN-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.

           SELECT REINT-OUT        ASSIGN TO REINOTT
              ORGANIZATION LINE SEQUENTIAL.
           SELECT REINC-OUT        ASSIGN TO REINOTC
              ORGANIZATION LINE SEQUENTIAL.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ERREIN.

           COPY ERCREIN.

       FD  REINT-OUT
           RECORDING MODE V
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REINT-OUT-REC                PIC X(146).
       01  REINT-HEAD-REC               PIC X(173).

       FD  REINC-OUT
           RECORDING MODE V
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

042309 01  REINC-OUT-REC                PIC X(187).
042309 01  REINC-HEAD-REC               PIC X(363).

       FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   EL549 WORKING STORAGE        '.
       77  FILLER  PIC X(32) VALUE '********************************'.

030404 01  WS-MISC.
           05  WS-EOF-SW               PIC X     VALUE SPACES.
               88  END-OF-ERREIN                 VALUE 'Y'.
               88  MORE-ERREIN                   VALUE ' '.
           05  ERREIN-RECS-IN          PIC 9(9)  VALUE ZEROS.
           05  REINT-RECS-OUT          PIC 9(9)  VALUE ZEROS.
           05  REINC-RECS-OUT          PIC 9(9)  VALUE ZEROS.
           05  SUB                     PIC S9(5) VALUE +0 COMP-3.

           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC X(4).
               10  WS-WORK-MM          PIC XX.
               10  WS-WORK-DD          PIC XX.
           05  ERREIN-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.

           05  PGM-SUB                 PIC S9(4)   VALUE +548.
030404**** PROGRAM ABEND FIELDS
030404     05  WS-RETURN-CODE          PIC S9(03) VALUE +0.
030404     05  WS-ZERO                 PIC S9(01) VALUE +0.
030404     05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
030404     05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.

042309 01  WS-SAVE-REINT               PIC X(146) VALUE LOW-VALUES.
       01  REINT-DETAIL-RECORD.
           12  REINT-TABLE             PIC XXX.
           12  REINT-TAB1              PIC X.
           12  REINT-NSP-ST-LF         PIC XX.
           12  REINT-TAB2              PIC X.
           12  REINT-NSP-ST-AH         PIC XX.
           12  REINT-TAB3              PIC X.
           12  REINT-COMP-PRIME        PIC XXX.
           12  REINT-TAB4              PIC X.
           12  REINT-COMP-SUB          PIC XXX.
           12  REINT-TAB5              PIC X.
           12  REINT-LEVEL             PIC 99.
           12  REINT-TAB6              PIC X.
           12  REINT-EFF-DATE          PIC X(10).
           12  REINT-TAB7              PIC X.
           12  REINT-EXP-DATE          PIC X(10).
           12  REINT-TAB8              PIC X.
           12  REINT-LFQC              PIC X.
           12  REINT-TAB9              PIC X.
           12  REINT-AHQC              PIC X.
           12  REINT-TAB10             PIC X.
           12  REINT-LFBC              PIC X.
           12  REINT-TAB11             PIC X.
           12  REINT-AHBC              PIC X.
           12  REINT-TAB12             PIC X.
           12  REINT-IN                PIC X.
           12  REINT-TAB13             PIC X.
           12  REINT-RM                PIC X.
           12  REINT-TAB14             PIC X.
           12  REINT-LFLO              PIC -9(9).99.
           12  REINT-TAB15             PIC X.
           12  REINT-AHLO              PIC -9(7).99.
           12  REINT-TAB16             PIC X.
           12  REINT-AHBLO             PIC -9(7).99.
           12  REINT-TAB17             PIC X.
           12  REINT-LFHI              PIC -9(9).99.
           12  REINT-TAB18             PIC X.
           12  REINT-AHHI              PIC -9(7).99.
           12  REINT-TAB19             PIC X.
           12  REINT-AHBHI             PIC -9(7).99.
           12  REINT-TAB20             PIC X.
           12  REINT-LFPCT             PIC -9.9999.
           12  REINT-TAB21             PIC X.
           12  REINT-AHPCT             PIC -9.9999.

       01  REINT-HEADER-RECORD.
           12  REINT-HD-TABLE          PIC X(5)  VALUE 'TABLE'.
           12  REINT-HD-TAB1           PIC X.
           12  REINT-HD-STATE-LF       PIC X(12) VALUE 'LF NSP STATE'.
           12  REINT-HD-TAB2           PIC X.
           12  REINT-HD-STATE-AH       PIC X(12) VALUE 'AH NSP STATE'.
           12  REINT-HD-TAB3           PIC X.
           12  REINT-HD-COMP-PRIME     PIC X(10) VALUE 'COMP PRIME'.
           12  REINT-HD-TAB4           PIC X.
           12  REINT-HD-COMP-SUB       PIC X(8)  VALUE 'COMP SUB'.
           12  REINT-HD-TAB5           PIC X.
           12  REINT-HD-LEVEL          PIC X(5)  VALUE 'LEVEL'.
           12  REINT-HD-TAB6           PIC X.
           12  REINT-HD-EFF-DATE       PIC X(6)  VALUE 'EFF DT'.
           12  REINT-HD-TAB7           PIC X.
           12  REINT-HD-EXP-DATE       PIC X(6)  VALUE 'EXP DT'.
           12  REINT-HD-TAB8           PIC X.
           12  REINT-HD-LFQC           PIC X(5)  VALUE 'LF QC'.
           12  REINT-HD-TAB9           PIC X.
           12  REINT-HD-AHQC           PIC X(5)  VALUE 'AH QC'.
           12  REINT-HD-TAB10          PIC X.
           12  REINT-HD-LFBC           PIC X(5)  VALUE 'LF BC'.
           12  REINT-HD-TAB11          PIC X.
           12  REINT-HD-AHBC           PIC X(5)  VALUE 'AH BC'.
           12  REINT-HD-TAB12          PIC X.
           12  REINT-HD-INTER          PIC X(11) VALUE 'INTERACTIVE'.
           12  REINT-HD-TAB13          PIC X.
           12  REINT-HD-REMAIN         PIC X(9)  VALUE 'REMAINING'.
           12  REINT-HD-TAB14          PIC X.
           12  REINT-HD-LFLO           PIC X(5)  VALUE 'LF LO'.
           12  REINT-HD-TAB15          PIC X.
           12  REINT-HD-AHLO           PIC X(5)  VALUE 'AH LO'.
           12  REINT-HD-TAB16          PIC X.
           12  REINT-HD-AHBLO          PIC X(8)  VALUE 'MOBEN LO'.
           12  REINT-HD-TAB17          PIC X.
           12  REINT-HD-LFHI           PIC X(5)  VALUE 'LF HI'.
           12  REINT-HD-TAB18          PIC X.
           12  REINT-HD-AHHI           PIC X(5)  VALUE 'AH HI'.
           12  REINT-HD-TAB19          PIC X.
           12  REINT-HD-AHBHI          PIC X(8)  VALUE 'MOBEN HI'.
           12  REINT-HD-TAB20          PIC X.
           12  REINT-HD-LFPCT          PIC X(6)  VALUE 'LF PCT'.
           12  REINT-HD-TAB21          PIC X.
           12  REINT-HD-AHPCT          PIC X(6)  VALUE 'AH PCT'.

042309 01  WS-SAVE-REINC               PIC X(187) VALUE LOW-VALUES.
       01  REINC-DETAIL-RECORD.
           12  REINC-COMP-PRIME        PIC XXX.
           12  REINC-TAB1              PIC X.
           12  REINC-COMP-SUB          PIC XXX.
           12  REINC-TAB2              PIC X.
           12  REINC-GROUP             PIC X(6).
           12  REINC-TAB3              PIC X.
           12  REINC-CARRIER-SEC       PIC X.
           12  REINC-TAB4              PIC X.
           12  REINC-REIN-NAME         PIC X(30).
           12  REINC-TAB5              PIC X.
           12  REINC-TYPE              PIC X.
           12  REINC-TAB6              PIC X.
           12  REINC-CEDE-NAME         PIC X(30).
           12  REINC-TAB7              PIC X.
           12  REINC-METH-LF           PIC X.
           12  REINC-TAB8              PIC X.
           12  REINC-METH-AH           PIC X.
           12  REINC-TAB9              PIC X.
           12  REINC-FEES-LF           PIC -9.9999.
           12  REINC-TAB10             PIC X.
           12  REINC-FEES-AH           PIC -9.9999.
           12  REINC-TAB11             PIC X.
           12  REINC-LF-PR-PCT         PIC -9.9999.
           12  REINC-TAB12             PIC X.
           12  REINC-LF-78-PCT         PIC -9.9999.
           12  REINC-TAB13             PIC X.
           12  REINC-AH-PR-PCT         PIC -9.9999.
           12  REINC-TAB14             PIC X.
           12  REINC-AH-78-PCT         PIC -9.9999.
           12  REINC-TAB15             PIC X.
           12  REINC-FEE-METH-LF       PIC X.
           12  REINC-TAB16             PIC X.
           12  REINC-FEE-METH-AH       PIC X.
           12  REINC-TAB17             PIC X.
           12  REINC-FEE-BAS-LF        PIC X.
           12  REINC-TAB18             PIC X.
           12  REINC-FEE-BAS-AH        PIC X.
           12  REINC-TAB19             PIC X.
092707     12  REINC-REPTA             PIC X.
           12  REINC-TAB20             PIC X.
           12  REINC-REPTB             PIC X.
           12  REINC-TAB21             PIC X.
           12  REINC-REPTC             PIC X.
           12  REINC-TAB22             PIC X.
           12  REINC-REPTD             PIC X.
           12  REINC-TAB23             PIC X.
           12  REINC-REPTE             PIC X.
           12  REINC-TAB24             PIC X.
           12  REINC-REPTF             PIC X.
042309     12  REINC-TAB25             PIC X.
042309     12  REINC-PRT-TAX           PIC X.
042309     12  REINC-TAB26             PIC X.
042309     12  REINC-PRT-OW            PIC X.
042309     12  REINC-TAB27             PIC X.
042309     12  REINC-PRT-CRSV          PIC X.
042309     12  REINC-TAB28             PIC X.
042309     12  REINC-MORT-CODE         PIC X(4).
042309     12  REINC-TAB29             PIC X.
042309     12  REINC-MORT-SW           PIC X.
042309     12  REINC-TAB30             PIC X.
042309     12  REINC-CLAIM-OPT         PIC X.
042309     12  REINC-TAB31             PIC X.
042309     12  REINC-LF-FEE-OPT        PIC X.
042309     12  REINC-TAB32             PIC X.
042309     12  REINC-AH-FEE-OPT        PIC X.
042309     12  REINC-TAB33             PIC X.
042309     12  REINC-LF-COM-OPT        PIC X.
042309     12  REINC-TAB34             PIC X.
042309     12  REINC-AH-COM-OPT        PIC X.
042309     12  REINC-TAB35             PIC X.
042309     12  REINC-LF-TAX-OPT        PIC X.
042309     12  REINC-TAB36             PIC X.
042309     12  REINC-AH-TAX-OPT        PIC X.
042309     12  REINC-TAB37             PIC X.
042309     12  REINC-EXCISE-TAX        PIC -9.9999.

       01  REINC-HEADER-RECORD.
           12  REINC-HD-COMP-PRIME     PIC X(10) VALUE 'COMP PRIME'.
           12  REINC-HD-TAB1           PIC X.
           12  REINC-HD-COMP-SUB       PIC X(8)  VALUE 'COMP SUB'.
           12  REINC-HD-TAB2           PIC X.
           12  REINC-HD-GROUP          PIC X(8)  VALUE 'GROUPING'.
           12  REINC-HD-TAB3           PIC X.
           12  REINC-HD-CARR           PIC X(13) VALUE 'CARR SECURITY'.
           12  REINC-HD-TAB4           PIC X.
           12  REINC-HD-COMPANY        PIC X(12) VALUE 'REIN COMPANY'.
           12  REINC-HD-TAB5           PIC X.
           12  REINC-HD-TYPE           PIC X(9)  VALUE 'CESS TYPE'.
           12  REINC-HD-TAB6           PIC X.
           12  REINC-HD-CEDE-COMP      PIC X(12) VALUE 'CEDE COMPANY'.
           12  REINC-HD-TAB7           PIC X.
           12  REINC-HD-METH-LF        PIC X(7)  VALUE 'METH LF'.
           12  REINC-HD-TAB8           PIC X.
           12  REINC-HD-METH-AH        PIC X(7)  VALUE 'METH AH'.
           12  REINC-HD-TAB9           PIC X.
           12  REINC-HD-FEES-LF        PIC X(7)  VALUE 'FEES LF'.
           12  REINC-HD-TAB10          PIC X.
           12  REINC-HD-FEES-AH        PIC X(7)  VALUE 'FEES AH'.
           12  REINC-HD-TAB11          PIC X.
           12  REINC-HD-PRO-LF         PIC X(6)  VALUE 'PRO LF'.
           12  REINC-HD-TAB12          PIC X.
           12  REINC-HD-R78-LF         PIC X(6)  VALUE 'R78 LF'.
           12  REINC-HD-TAB13          PIC X.
           12  REINC-HD-PRO-AH         PIC X(6)  VALUE 'PRO AH'.
           12  REINC-HD-TAB14          PIC X.
           12  REINC-HD-R78-AH         PIC X(6)  VALUE 'R78 AH'.
           12  REINC-HD-TAB15          PIC X.
           12  REINC-HD-FEE-METH-LF    PIC X(11) VALUE 'FEE METH LF'.
           12  REINC-HD-TAB16          PIC X.
           12  REINC-HD-FEE-METH-AH    PIC X(11) VALUE 'FEE METH AH'.
           12  REINC-HD-TAB17          PIC X.
           12  REINC-HD-FEE-BAS-LF     PIC X(12) VALUE 'FEE BASIS LF'.
           12  REINC-HD-TAB18          PIC X.
           12  REINC-HD-FEE-BAS-AH     PIC X(12) VALUE 'FEE BASIS AH'.
           12  REINC-HD-TAB19          PIC X.
092707     12  REINC-HD-REPTA          PIC X(5)  VALUE 'RPT A'.
           12  REINC-HD-TAB20          PIC X.
           12  REINC-HD-REPTB          PIC X(5)  VALUE 'RPT B'.
           12  REINC-HD-TAB21          PIC X.
           12  REINC-HD-REPTC          PIC X(5)  VALUE 'RPT C'.
           12  REINC-HD-TAB22          PIC X.
           12  REINC-HD-REPTD          PIC X(5)  VALUE 'RPT D'.
           12  REINC-HD-TAB23          PIC X.
           12  REINC-HD-REPTE          PIC X(5)  VALUE 'RPT E'.
           12  REINC-HD-TAB24          PIC X.
           12  REINC-HD-REPTF          PIC X(5)  VALUE 'RPT F'.
042309     12  REINC-HD-TAB25          PIC X.
042309     12  REINC-HD-PRT-TAX        PIC X(9)  VALUE 'PRINT TAX'.
042309     12  REINC-HD-TAB26          PIC X.
042309     12  REINC-HD-PRT-OW         PIC X(8)  VALUE 'PRINT OW'.
042309     12  REINC-HD-TAB27          PIC X.
042309     12  REINC-HD-PRT-CRSV       PIC X(10) VALUE 'PRINT CRSV'.
042309     12  REINC-HD-TAB28          PIC X.
042309     12  REINC-HD-MORT-CD        PIC X(9)  VALUE 'MORT CODE'.
042309     12  REINC-HD-TAB29          PIC X.
042309     12  REINC-HD-MORT-SW        PIC X(7)  VALUE 'MORT SW'.
042309     12  REINC-HD-TAB30          PIC X.
042309     12  REINC-HD-CLAIM-SW       PIC X(12) VALUE 'CLAIM OPTION'.
042309     12  REINC-HD-TAB31          PIC X.
042309     12  REINC-HD-LF-FEE-OPT     PIC X(10) VALUE 'LF FEE OPT'.
042309     12  REINC-HD-TAB32          PIC X.
042309     12  REINC-HD-AH-FEE-OPT     PIC X(10) VALUE 'AH FEE OPT'.
042309     12  REINC-HD-TAB33          PIC X.
042309     12  REINC-HD-LF-COM-OPT     PIC X(10) VALUE 'LF COM OPT'.
042309     12  REINC-HD-TAB34          PIC X.
042309     12  REINC-HD-AH-COM-OPT     PIC X(10) VALUE 'AH COM OPT'.
042309     12  REINC-HD-TAB35          PIC X.
042309     12  REINC-HD-LF-TAX-OPT     PIC X(10) VALUE 'LF TAX OPT'.
042309     12  REINC-HD-TAB36          PIC X.
042309     12  REINC-HD-AH-TAX-OPT     PIC X(10) VALUE 'AH TAX OPT'.
042309     12  REINC-HD-TAB37          PIC X.
042309     12  REINC-HD-EXCISE-TAX     PIC X(10) VALUE 'EXCISE TAX'.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

       0000-LOAD-DATE-CARD.            COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-ERREIN THRU 0100-EXIT UNTIL
                 END-OF-ERREIN

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' REIN  RECORDS READ    '  ERREIN-RECS-IN
           DISPLAY ' REINT RECORDS WRITTEN '  REINT-RECS-OUT
           DISPLAY ' REINC RECORDS WRITTEN '  REINC-RECS-OUT
           GOBACK

           .
       0100-PROCESS-ERREIN.

           IF RE-CODE = 'A'
              PERFORM 0150-PROCESS-A   THRU 0150-EXIT
                 VARYING SUB FROM +1 BY +1 UNTIL
                   (SUB > +30) OR
                   (RE-REI-COMP (SUB) = '   ' OR '000')
           ELSE
              PERFORM 0170-PROCESS-B   THRU 0170-EXIT
           END-IF

           PERFORM 0200-READ-ERREIN    THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0150-PROCESS-A.

           MOVE WS-SAVE-REINT          TO REINT-DETAIL-RECORD
           MOVE RE-TABLE               TO REINT-TABLE
           MOVE RE-NSP-ST-CD-LF        TO REINT-NSP-ST-LF
           MOVE RE-NSP-ST-CD-AH        TO REINT-NSP-ST-AH
           MOVE RE-REI-COMP (SUB)      TO REINT-COMP-PRIME
           MOVE RE-REI-COMP-SUB (SUB)  TO REINT-COMP-SUB
           MOVE SUB                    TO REINT-LEVEL
           MOVE RE-LF-QC (SUB)         TO REINT-LFQC
           MOVE RE-AH-QC (SUB)         TO REINT-AHQC
           MOVE RE-LF-PCT (SUB)        TO REINT-LFPCT
           MOVE RE-AH-PCT (SUB)        TO REINT-AHPCT
           MOVE RE-LF-LO (SUB)         TO REINT-LFLO
           MOVE RE-LF-HI (SUB)         TO REINT-LFHI
           MOVE RE-AHBEN-LO (SUB)      TO REINT-AHLO
           MOVE RE-AHBEN-HI (SUB)      TO REINT-AHHI
           MOVE RE-AHMOA-LO (SUB)      TO REINT-AHBLO
           MOVE RE-AHMOA-HI (SUB)      TO REINT-AHBHI
           MOVE RE-LF-BEN-CODE (SUB)   TO REINT-LFBC
           MOVE RE-AH-BEN-CODE (SUB)   TO REINT-AHBC
           MOVE RE-INTERACTIVE (SUB)   TO REINT-IN
           MOVE RE-REMAINING (SUB)     TO REINT-RM

           MOVE RE-LO-DATE (SUB)       TO WS-WORK-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO REINT-EFF-DATE
           END-STRING

           MOVE RE-HI-DATE (SUB)       TO WS-WORK-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO REINT-EXP-DATE
           END-STRING

           PERFORM 0300-WRITE-REINT    THRU 0300-EXIT

           .
       0150-EXIT.
           EXIT.

       0170-PROCESS-B.

           MOVE WS-SAVE-REINC          TO REINC-DETAIL-RECORD
           MOVE RE-COMP-PRIME          TO REINC-COMP-PRIME
           MOVE RE-COMP-SUB            TO REINC-COMP-SUB
           MOVE RE-REINS-GROUPING-CODE TO REINC-GROUP
           MOVE RE-COMP-CARRIER-SECURITY
                                       TO REINC-CARRIER-SEC
           MOVE RE-NAME                TO REINC-REIN-NAME
           MOVE RE-CEDING-TYPE-FLAG    TO REINC-TYPE
           MOVE RE-CEDE-NAME           TO REINC-CEDE-NAME
           MOVE RE-LF-PE               TO REINC-METH-LF
           MOVE RE-AH-PE               TO REINC-METH-AH
           MOVE RE-LF-FEE              TO REINC-FEES-LF
           MOVE RE-AH-FEE              TO REINC-FEES-AH
           MOVE RE-LF-PR-PCT           TO REINC-LF-PR-PCT
           MOVE RE-AH-PR-PCT           TO REINC-AH-PR-PCT
           MOVE RE-LF-78-PCT           TO REINC-LF-78-PCT
           MOVE RE-AH-78-PCT           TO REINC-AH-78-PCT
           MOVE RE-LF-FEE-METHOD       TO REINC-FEE-METH-LF
           MOVE RE-AH-FEE-METHOD       TO REINC-FEE-METH-AH
           MOVE RE-LF-FEE-BASIS        TO REINC-FEE-BAS-LF
           MOVE RE-AH-FEE-BASIS        TO REINC-FEE-BAS-AH
092707     MOVE RE-CEDING-STMT-OPT-A   TO REINC-REPTA
           MOVE RE-CEDING-STMT-OPT-B   TO REINC-REPTB
           MOVE RE-CEDING-STMT-OPT-C   TO REINC-REPTC
           MOVE RE-CEDING-STMT-OPT-D   TO REINC-REPTD
           MOVE RE-CEDING-STMT-OPT-E   TO REINC-REPTE
           MOVE RE-STATE-EXHIBIT-OPT-F TO REINC-REPTF

042309     MOVE RE-PRT-ST              TO  REINC-PRT-TAX   
042309     MOVE RE-PRT-OW              TO  REINC-PRT-OW    
042309     MOVE RE-PRT-CRSV            TO  REINC-PRT-CRSV  
042309     MOVE RE-MORT-CODE           TO  REINC-MORT-CODE 
042309     MOVE RE-MORT-SW             TO  REINC-MORT-SW   
042309     MOVE RE-CLAIM-CODE          TO  REINC-CLAIM-OPT 
042309     MOVE RE-ZERO-LF-FEE         TO  REINC-LF-FEE-OPT
042309     MOVE RE-ZERO-AH-FEE         TO  REINC-AH-FEE-OPT
042309     MOVE RE-LF-COMM             TO  REINC-LF-COM-OPT
042309     MOVE RE-AH-COMM             TO  REINC-AH-COM-OPT
042309     MOVE RE-LF-TAX              TO  REINC-LF-TAX-OPT
042309     MOVE RE-AH-TAX              TO  REINC-AH-TAX-OPT
           IF RE-EXCISE-TAX NOT NUMERIC
              MOVE ZEROS               TO RE-EXCISE-TAX
           END-IF
042309     MOVE RE-EXCISE-TAX          TO  REINC-EXCISE-TAX

           PERFORM 0350-WRITE-REINC    THRU 0350-EXIT

           .
       0170-EXIT.
           EXIT.

       0200-READ-ERREIN.

           READ ERREIN NEXT RECORD

           IF (ERREIN-FILE-STATUS = '10' OR '23')
              OR (RE-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERREIN    TO TRUE
           ELSE
              IF ERREIN-FILE-STATUS NOT = '00'
                 DISPLAY 'ERREIN READ NEXT ' ERREIN-FILE-STATUS
                 SET END-OF-ERREIN  TO TRUE
              END-IF
           END-IF

           IF MORE-ERREIN
              ADD 1 TO ERREIN-RECS-IN
      *       IF RE-CODE = 'B'
      *          SET END-OF-ERREIN TO TRUE
      *       END-IF
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-REINT.

           WRITE REINT-OUT-REC FROM REINT-DETAIL-RECORD
           ADD 1 TO REINT-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0350-WRITE-REINC.

           WRITE REINC-OUT-REC FROM REINC-DETAIL-RECORD
           ADD 1 TO REINC-RECS-OUT

           .
       0350-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERREIN
               OUTPUT REINT-OUT REINC-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERREIN REINT-OUT REINC-OUT

           .
       0500-EXIT.
           EXIT.

       0550-START-ERREIN.

           MOVE LOW-VALUES             TO RE-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO RE-COMPANY-CD

           START ERREIN KEY IS NOT < RE-CONTROL-PRIMARY

           IF ERREIN-FILE-STATUS = '10' OR '23'
              SET END-OF-ERREIN  TO TRUE
           ELSE
              IF ERREIN-FILE-STATUS NOT = '00'
                 DISPLAY 'ERREIN START     ' ERREIN-FILE-STATUS
                 SET END-OF-ERREIN  TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO REINT-DETAIL-RECORD
052704     MOVE X'3B'                  TO REINT-TAB1
                                          REINT-TAB2
                                          REINT-TAB3
                                          REINT-TAB4
                                          REINT-TAB5
                                          REINT-TAB6
                                          REINT-TAB7
                                          REINT-TAB8
                                          REINT-TAB9
                                          REINT-TAB10
                                          REINT-TAB11
                                          REINT-TAB12
                                          REINT-TAB13
                                          REINT-TAB14
                                          REINT-TAB15
                                          REINT-TAB16
                                          REINT-TAB17
                                          REINT-TAB18
                                          REINT-TAB19
                                          REINT-TAB20
                                          REINT-TAB21

           MOVE ZEROS                  TO REINT-LFLO
                                          REINT-AHLO
                                          REINT-AHBLO
                                          REINT-LFHI
                                          REINT-AHHI
                                          REINT-AHBHI
                                          REINT-LFPCT
                                          REINT-AHPCT

           MOVE REINT-DETAIL-RECORD    TO WS-SAVE-REINT

052704     MOVE X'3B'                  TO REINT-HD-TAB1
                                          REINT-HD-TAB2
                                          REINT-HD-TAB3
                                          REINT-HD-TAB4
                                          REINT-HD-TAB5
                                          REINT-HD-TAB6
                                          REINT-HD-TAB7
                                          REINT-HD-TAB8
                                          REINT-HD-TAB9
                                          REINT-HD-TAB10
                                          REINT-HD-TAB11
                                          REINT-HD-TAB12
                                          REINT-HD-TAB13
                                          REINT-HD-TAB14
                                          REINT-HD-TAB15
                                          REINT-HD-TAB16
                                          REINT-HD-TAB17
                                          REINT-HD-TAB18
                                          REINT-HD-TAB19
                                          REINT-HD-TAB20
                                          REINT-HD-TAB21

           WRITE REINT-HEAD-REC        FROM REINT-HEADER-RECORD

           MOVE SPACES                 TO REINC-DETAIL-RECORD
052704     MOVE X'3B'                  TO REINC-TAB1
                                          REINC-TAB2
                                          REINC-TAB3
                                          REINC-TAB4
                                          REINC-TAB5
                                          REINC-TAB6
                                          REINC-TAB7
                                          REINC-TAB8
                                          REINC-TAB9
                                          REINC-TAB10
                                          REINC-TAB11
                                          REINC-TAB12
                                          REINC-TAB13
                                          REINC-TAB14
                                          REINC-TAB15
                                          REINC-TAB16
                                          REINC-TAB17
                                          REINC-TAB18
                                          REINC-TAB19
                                          REINC-TAB20
                                          REINC-TAB21
                                          REINC-TAB22
                                          REINC-TAB23
                                          REINC-TAB24
                                          REINC-TAB25
                                          REINC-TAB26
                                          REINC-TAB27
                                          REINC-TAB28
                                          REINC-TAB29
                                          REINC-TAB30
                                          REINC-TAB31
                                          REINC-TAB32
                                          REINC-TAB33
                                          REINC-TAB34
                                          REINC-TAB35
                                          REINC-TAB36
                                          REINC-TAB37

           MOVE ZEROS                  TO REINC-FEES-LF
                                          REINC-FEES-AH
                                          REINC-LF-PR-PCT
                                          REINC-LF-78-PCT
                                          REINC-AH-PR-PCT
                                          REINC-AH-78-PCT
                                          REINC-EXCISE-TAX

           MOVE REINC-DETAIL-RECORD    TO WS-SAVE-REINC

052704     MOVE X'3B'                  TO REINC-HD-TAB1
                                          REINC-HD-TAB2
                                          REINC-HD-TAB3
                                          REINC-HD-TAB4
                                          REINC-HD-TAB5
                                          REINC-HD-TAB6
                                          REINC-HD-TAB7
                                          REINC-HD-TAB8
                                          REINC-HD-TAB9
                                          REINC-HD-TAB10
                                          REINC-HD-TAB11
                                          REINC-HD-TAB12
                                          REINC-HD-TAB13
                                          REINC-HD-TAB14
                                          REINC-HD-TAB15
                                          REINC-HD-TAB16
                                          REINC-HD-TAB17
                                          REINC-HD-TAB18
                                          REINC-HD-TAB19
                                          REINC-HD-TAB20
                                          REINC-HD-TAB21
                                          REINC-HD-TAB22
                                          REINC-HD-TAB23
                                          REINC-HD-TAB24
                                          REINC-HD-TAB25
                                          REINC-HD-TAB26
                                          REINC-HD-TAB27
                                          REINC-HD-TAB28
                                          REINC-HD-TAB29
                                          REINC-HD-TAB30
                                          REINC-HD-TAB31
                                          REINC-HD-TAB32
                                          REINC-HD-TAB33
                                          REINC-HD-TAB34
                                          REINC-HD-TAB35
                                          REINC-HD-TAB36
                                          REINC-HD-TAB37

           WRITE REINC-HEAD-REC        FROM REINC-HEADER-RECORD

           PERFORM 0550-START-ERREIN   THRU 0550-EXIT
           PERFORM 0200-READ-ERREIN    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

030404 ABEND-PGM. COPY ELCABEND.
