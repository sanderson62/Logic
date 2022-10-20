061814$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 EL582.
      *
      *            THIS IS A COPY OF ONLINE PROGRAM EL682
      *            AND RUNS AS A SCHEDULED BATCH JOB.
      *
      *
      *AUTHOR.     CENTRAL STATES
      *            ISD.

      *DATE-COMPILED.

      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************

      *REMARKS.
      *        PAYMENT AND ADJUSTMENT PRINT
      *        THIS PROGRAM PRINTS THE PAYMENT AND ADJUSTMENT
      *        SUMARY IN A BATCH ENVIRONMENT.
      *     PROGRAM OPTIONS
      *       OPTION 1 (DEFAULT)   PRINTS DAILY REPORT
      *       OPTION 2             PRINTS A MONTH TO DATE REPORT

031507******************************************************************
031507*                   C H A N G E   L O G
031507*
031507* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031507*-----------------------------------------------------------------
031507*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031507* EFFECTIVE    NUMBER
031507*-----------------------------------------------------------------
031507* 031507    2007031300002  PEMA  ADD CYCLE DATE PROCESSING
091108* 091108    2008090200005  AJRA  ADD DAILY EXTRACT FILE
111709* 111709    2009111700001  AJRA  USE PARM TO LIMIT CARRIER
061814* 061814  CR2013092700001  PEMA  UPLOAD DLY G/L INT BAL DATA
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
082514* 082514  CR2013092700001  PEMA  move to new DB.
100114* 100114  CR2013092700001  PEMA  bypass sql upd if month-end
011116* 011116  CR2015082400003  PEMA  ADD CARRIER 9 PROCESSING
112216* 112216  IR2016112200001  PEMA  Treat car 3&4 like 5&6
062618* 062618  CR2018042300001  PEMA  Remove batch 81 from interface
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
120821* 120821  CR2019012500003  PEMA  Migrate to SQLSERVER 2016
031507******************************************************************
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.

           SELECT DISK-DATE       ASSIGN TO SYS019-FBA1-S-SYS019.

           SELECT FICH            ASSIGN TO SYS020-UR-2400-S-SYS020.

           SELECT ELREPT          ASSIGN TO SYS010-FBA1-ELREPT
                                  ORGANIZATION INDEXED
                                  ACCESS       DYNAMIC
                                  RECORD KEY   RF-CONTROL-PRIMARY
                                  FILE STATUS  DTE-VSAM-FLAGS.

           SELECT ERPYAJ          ASSIGN TO SYS011-FBA1-ERPYAJ
                                  ORGANIZATION INDEXED
                                  ACCESS       DYNAMIC
                                  RECORD KEY   PY-CONTROL-PRIMARY
                                  FILE STATUS  ERPYAJ-FILE-STATUS.

           SELECT ELCNTL          ASSIGN TO SYS012-FBA1-ELCNTL
                                  ORGANIZATION INDEXED
                                  ACCESS       DYNAMIC
                                  RECORD KEY   CF-CONTROL-PRIMARY
                                  FILE STATUS  ELCNTL-FILE-STATUS.

091108     SELECT EXTRACT         ASSIGN TO SYS022
091108                            ORGANIZATION IS LINE SEQUENTIAL.

       EJECT
       DATA DIVISION.

       FILE SECTION.

       FD  PRNTR                   COPY ELCPRTFD.
       EJECT
       FD  ELREPT                  COPY ELCRPTFD.
                                   COPY ELCREPT.
       EJECT
       FD  DISK-DATE               COPY ELCDTEFD.
       EJECT
       FD  FICH                    COPY ELCFCHFD.
       EJECT
       FD  ERPYAJ.
                                   COPY ERCPYAJ.
       EJECT
       FD  ELCNTL.
                                   COPY ELCCNTL.
091108 FD EXTRACT.
091108 01 EXTRACT-RECORD                 PIC X(101).

       EJECT
       WORKING-STORAGE SECTION.

061814 EXEC SQL
061814    INCLUDE SQLCA
061814 END-EXEC

       01  LCP-TIME-OF-DAY-XX.
           05  LCP-TIME-OF-DAY-68        PIC 9(6).
           05  FILLER                    PIC 99.
       01  LCP-CICS-TIME                 PIC 9(15).
       77  FILLER PIC  X(32) VALUE '********************************'.
       77  FILLER PIC  X(32) VALUE '    ECS682  WORKING-STORAGE     '.
       77  FILLER PIC  X(32) VALUE '************ V/M 2.012 *********'.

       77  CTR                     PIC S99    COMP    VALUE +0.
       77  CLEN                    PIC S9(4)  COMP    VALUE +1024.
       77  D-AMT                   PIC S9(7)V99       COMP-3.
       77  SAV-CO-PRE              PIC  X              VALUE LOW-VALUE.

061814 01  P pointer.
061814 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
061814 01  var-ptr pointer.
061814 01  env-var-len                 pic 9(4)  binary.
061814 01  rc                          pic 9(9)  binary.
061814
061814 01  WS-KIXSYS.
061814     05  WS-KIX-FIL1             PIC X(10).
061814     05  WS-KIX-APPS             PIC X(10).
061814     05  WS-KIX-ENV              PIC X(10).
061814     05  WS-KIX-MYENV            PIC X(10).
061814     05  WS-KIX-SYS              PIC X(10).
061814
061814 EXEC SQL BEGIN DECLARE SECTION END-EXEC
061814
061814 01  svr                         pic x(32).
061814 01  usr                         pic x(32).
061814 01  pass                        pic x(32).
061814 01  usr-pass                    pic x(64).
061814 01  ws-disp-code                pic s9(11).
061814
061814 01  ws-table-values.
061814     05  tbl-comp-id             pic x(5)   value spaces.
061814     05  tbl-batch-no            pic x(50)  value spaces.
061814     05  tbl-carrier             pic x      value spaces.
061814     05  tbl-date                pic x(10)  value spaces.
061814     05  tbl-source-rpt          pic x(8)   value 'EL585'.
061814     05  tbl-remits              pic x(10)  value spaces.
061814     05  em-remits redefines tbl-remits
061814                                 pic zzzzzzz.99.
061814                                 
061814     05  tbl-charges             pic x(10)  value spaces.
061814     05  em-charges redefines tbl-charges
061814                                 pic zzzzzzz.99.
061814     05  tbl-other               pic x(10)  value spaces.
061814     05  tbl-clm-pmts            pic x(10)  value spaces.
061814     05  em-clm-pmts redefines tbl-clm-pmts
061814                                 pic zzzzzzz.99.
061814     05  tbl-clm-stop-pays       pic x(10)  value spaces.
061814     05  em-clm-stop-pays redefines tbl-clm-stop-pays
061814                                 pic zzzzzzz.99.
061814     05  tbl-clm-offline-pmts    pic x(10)  value spaces.
061814     05  em-clm-offline-pmts redefines tbl-clm-offline-pmts
061814                                 pic zzzzzzz.99.
061814
061814 EXEC SQL END DECLARE SECTION END-EXEC

       01  WORK-AREAS.
031507     12  WS-PREV-CYCLE-DT    PIC XX VALUE LOW-VALUES.
031507     12  WS-CURR-CYCLE-DT    PIC XX VALUE LOW-VALUES.
           12  PGM-NAME            PIC  X(8)      VALUE SPACES.
           12  LINK-ELDATCV        PIC  X(8)      VALUE 'ELDATCV'.
           12  EMI-LINE1           PIC  X(72).
           12  WS-NEXT-TRAN        PIC  X(4).
           12  WS-TERMINAL-ID.
               16  WS-TERM-PREFIX  PIC  XX.
               16  FILLER          PIC  XX.
           12  PRT-CNT             PIC S9(3)       VALUE +0 COMP-3.
           12  REC-CNT             PIC S9(4)       VALUE +0 COMP-3.
           12  WS-LINE-NUMBER      PIC S9(7)       VALUE +0 COMP-3.
           12  WS-PAGE             PIC S9(5)       VALUE +0 COMP-3.
           12  WS-REPORT-SW        PIC S9          VALUE +0 COMP-3.
           12  WS-PRINT-SW         PIC S9          VALUE +0 COMP-3.
           12  TOT-ENTRY-AMT       PIC S9(7)V99    VALUE ZERO.
           12  SUB-ENTRY-AMT       PIC S9(7)V99    VALUE ZERO.
           12  REPT-FILE-ID        PIC  X(8)       VALUE 'ELREPT  '.
           12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ  '.
           12  CNTL-ID             PIC  X(8)       VALUE 'ELCNTL  '.
           12  SA                  PIC S9(3)  COMP.
           12  B-REC-FLAG          PIC  X          VALUE SPACES.
           12  WS-DATE.
               16  WS-YR           PIC  XX.
               16  WS-MO           PIC  XX.
               16  WS-DA           PIC  XX.
           12  ABEND-AREA          PIC  X(72).
           12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.
           12  SV-CONTROL.
               16  SV-CARR         PIC  X          VALUE SPACES.
               16  SV-GROUP        PIC  X(6)       VALUE SPACES.
               16  SV-RESP         PIC  X(10)      VALUE SPACES.
               16  SV-ACCT         PIC  X(10)      VALUE SPACES.
           12  PY-CONTROL.
               16  PY-CARR         PIC  X.
               16  PY-GROUP        PIC  X(6).
               16  PY-RESP         PIC  X(10).
               16  PY-ACCT         PIC  X(10).
           12  X                      PIC X        VALUE SPACE.
           12  PGM-SUB                PIC S999     COMP  VALUE +582.
           12  OLC-REPORT-NAME        PIC X(6)     VALUE 'EL582'.
           12  WS-ABEND-MESSAGE       PIC X(80)    VALUE SPACES.
           12  WS-ABEND-FILE-STATUS   PIC XX       VALUE ZERO.
           12  ERPYAJ-FILE-STATUS     PIC XX       VALUE ZERO.
           12  ELCNTL-FILE-STATUS     PIC XX       VALUE ZERO.
           12  WS-RETURN-CODE         PIC S9(4)    COMP  VALUE ZERO.
           12  WS-ZERO                PIC S9       VALUE ZERO.
           12  WS-COMPANY-ID          PIC X(3)     VALUE SPACES.
           12  WS-COMPANY-CD          PIC X        VALUE SPACE.
           12  WS-CURRENT-DTE-BIN     PIC XX       VALUE SPACE.

       01  SUMMARY-TOTS.
           12  SUB-R               PIC S9(7)V99    COMP-3 VALUE +0.
           12  SUB-D               PIC S9(7)V99    COMP-3 VALUE +0.
           12  SUB-C               PIC S9(7)V99    COMP-3 VALUE +0.
           12  SUB-S               PIC S9(7)V99    COMP-3 VALUE +0.
           12  SUB-T               PIC S9(7)V99    COMP-3 VALUE +0.
           12  SUB-U               PIC S9(7)V99    COMP-3 VALUE +0.
           12  SUB-X               PIC S9(7)V99    COMP-3 VALUE +0.
           12  SUB-Y               PIC S9(7)V99    COMP-3 VALUE +0.
           12  SUB-Z               PIC S9(7)V99    COMP-3 VALUE +0.
           12  SUB-F               PIC S9(7)V99    COMP-3 VALUE +0.
           12  TOT-R               PIC S9(7)V99    COMP-3 VALUE +0.
           12  TOT-D               PIC S9(7)V99    COMP-3 VALUE +0.
           12  TOT-C               PIC S9(7)V99    COMP-3 VALUE +0.
           12  TOT-S               PIC S9(7)V99    COMP-3 VALUE +0.
           12  TOT-T               PIC S9(7)V99    COMP-3 VALUE +0.
           12  TOT-U               PIC S9(7)V99    COMP-3 VALUE +0.
           12  TOT-X               PIC S9(7)V99    COMP-3 VALUE +0.
           12  TOT-Y               PIC S9(7)V99    COMP-3 VALUE +0.
           12  TOT-Z               PIC S9(7)V99    COMP-3 VALUE +0.
           12  TOT-F               PIC S9(7)V99    COMP-3 VALUE +0.
           12  SUM-TOT             PIC S9(7)V9     COMP-3 VALUE +0.

       01  ACCESS-KEYS.
           12  ELCNTL-KEY.
               16  CNTL-COMP-ID    PIC  XXX.
               16  CNTL-REC-TYPE   PIC  X.
               16  CNTL-ACCESS     PIC  X(4).
               16  CNTL-SEQ-NO     PIC S9(4)   COMP.
           EJECT
       01  HDR-LINES.
           12  HDR-1.
               16  FILLER          PIC  X(32)      VALUE SPACES.
               16  FILLER          PIC  X(33)      VALUE
                       'PAYMENT AND ADJUSTMENT SUMMARY'.
               16  FILLER          PIC  X(24)      VALUE SPACES.
               16  FILLER          PIC  X(8)       VALUE 'EL - 682'.
           12  HDR-2.
               16  FILLER          PIC  X(33)      VALUE SPACES.
               16  H2-COMP         PIC  X(30)      VALUE
                       'LOGIC, INC.'.
               16  FILLER          PIC  X(27)      VALUE SPACES.
               16  H2-DATE         PIC  X(8).
           12  HDR-3.
               16  FILLER          PIC  X(37)      VALUE SPACES.
               16  H3-DATE.
                   20  H3-DATE1    PIC  X(8)       VALUE SPACES.
                   20  H3-THRU     PIC  X(6)       VALUE SPACES.
                   20  H3-DATE2    PIC  X(8)       VALUE SPACES.
               16  FILLER          PIC  X(31)      VALUE SPACES.
               16  FILLER          PIC  X(5)       VALUE 'PAGE'.
               16  H3-PAGE         PIC Z,ZZ9.
           12  HDR-4.
               16  FILLER          PIC  X          VALUE SPACE.
               16  FILLER          PIC  X(28)      VALUE
                       'CARR  GROUP    FIN. RESP.'.
               16  FILLER          PIC  X(33)      VALUE
                       'ACCOUNT       AMOUNT    TYPE'.
               16  FILLER          PIC  X(10)      VALUE
                       '   COMMENT'.
               16  FILLER          PIC  X(26)      VALUE SPACES.
               16  FILLER          PIC  XX         VALUE 'BY'.
               16  FILLER          PIC  X(4)       VALUE SPACES.
               16  HDR-4-DT        PIC  X(6)       VALUE 'EOM DT'.
               16  FILLER          PIC  XX         VALUE SPACES.
               16  HDR-4-INV       PIC  X(7)       VALUE 'INVOICE'.
               16  FILLER          PIC  XX         VALUE SPACES.
               16  HDR-4-REF       PIC  X(9)       VALUE 'REFERENCE'.

       01  DATA-1.
           12  FILLER              PIC  XX         VALUE SPACES.
           12  D-CARR              PIC  X.
           12  FILLER              PIC  X(4)       VALUE SPACES.
           12  D-GROUP             PIC  X(6).
           12  D-TOTAL.
               16  FILLER          PIC  X(3)       VALUE SPACES.
               16  D-RESP          PIC  X(10).
               16  FILLER          PIC  XX         VALUE SPACES.
               16  D-ACCT          PIC  X(10).
           12  FILLER              PIC  X          VALUE SPACES.
           12  D-AMOUNT            PIC ZZZZ,ZZZ.99-.
           12  DET-AMT  REDEFINES
               D-AMOUNT            PIC  Z(12).
           12  FILLER              PIC  XX         VALUE SPACES.
           12  D-TYPE              PIC  X(10).
           12  FILLER              PIC  XX         VALUE SPACES.
           12  D-COMM              PIC  X(30).
           12  FILLER              PIC  XX         VALUE SPACE.
           12  D-BY                PIC  X(4)       VALUE SPACE.
           12  FILLER              PIC  XX         VALUE SPACE.
           12  D-MEDT              PIC  X(8)       VALUE SPACE.
           12  FILLER              PIC  X          VALUE SPACE.
           12  D-INVOICE           PIC  X(6)       VALUE SPACE.
           12  FILLER              PIC  XX         VALUE SPACE.
           12  D-REFERENCE         PIC  X(12)      VALUE SPACE.

       01  DATA-2.
           12  FILLER              PIC  X          VALUE SPACES.
           12  FILLER              PIC  X(22)      VALUE
                   'REPORT NOT PRODUCED - '.
           12  FILLER              PIC  X(41)      VALUE
                   'NO RECORDS FOUND TO MATCH SELECTION DATES'.

091108 01  EXTRACT-HDR.
091108     12  FILLER              PIC  X(7)    VALUE 'CARRIER'.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  FILLER              PIC  X(5)    VALUE 'GROUP'.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  FILLER              PIC  X(8)    VALUE 'FIN_RESP'.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  FILLER              PIC  X(7)    VALUE 'ACCOUNT'.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  FILLER              PIC  X(6)    VALUE 'AMOUNT'.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  FILLER              PIC  X(4)    VALUE 'TYPE'.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  FILLER              PIC  X(9)    VALUE 'TYPE_DESC'.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  FILLER              PIC  X(7)    VALUE 'COMMENT'.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  FILLER              PIC  X(6)    VALUE 'MNT_BY'.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  FILLER              PIC  X(6)    VALUE 'MNT_DT'.
091108     12  FILLER              PIC  X(27)   VALUE SPACES.

091108 01  EXTRACT-REC.
091108     12  EXT-CARR            PIC  X.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  EXT-GROUP           PIC  X(6).
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  EXT-RESP            PIC  X(10).
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  EXT-ACCT            PIC  X(10).
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  EXT-AMOUNT          PIC  -(9).99.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  EXT-TYPE            PIC  X.
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  EXT-TYPE-DESC       PIC  X(10).
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  EXT-COMM            PIC  X(30).
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  EXT-BY              PIC  X(4).
091108     12  FILLER              PIC  X       VALUE ';'.
091108     12  EXT-MEDT            PIC  X(8).
       EJECT
           COPY ELCDATE.

           COPY ELCDTECX.

           COPY ELCDTEVR.

031507 LINKAGE SECTION.
061814 01  var  pic x(30).

031507 01  PARM.
031507     05  PARM-LEN      PIC S9(04) COMP.
031507     05  PARM-VALUE    PIC X(100).

031507 PROCEDURE DIVISION USING PARM.

       0000-LOAD-DATE-CARD.
                                      COPY ELCDTERX.

       0000-MAIN-LOGIC  SECTION.

031507     IF PARM-LEN = 0
031507        DISPLAY ' INVALID PARM '
031507        PERFORM ABEND-PGM
031507     END-IF

061814     set P to address of KIXSYS
061814     CALL "getenv" using by value P returning var-ptr
061814     if var-ptr = null then
061814        display ' kixsys not set '
061814     else
061814        set address of var to var-ptr
061814        move 0                   to env-var-len
061814        inspect var tallying env-var-len
061814          for characters before X'00' 
061814        unstring var (1:env-var-len) delimited by '/'
061814           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
061814              WS-KIX-SYS
061814        end-unstring
061814     end-if
061814
061814     display ' KIXSYS  ' ws-kix-myenv

031507     MOVE PARM-VALUE (1:6)       TO DC-GREG-DATE-1-MDY
031507     MOVE '4'                    TO DC-OPTION-CODE
031507     PERFORM 8500-DATE-CONVERT   THRU 8599-EXIT
031507     IF NO-CONVERSION-ERROR
031507        MOVE DC-BIN-DATE-1       TO WS-PREV-CYCLE-DT
031507     ELSE
031507        DISPLAY ' INVALID PREV DATE ' PARM-VALUE (1:6)
031507        PERFORM ABEND-PGM
031507     END-IF

031507     MOVE WS-PREV-CYCLE-DT       TO DC-BIN-DATE-1
031507     MOVE +0                     TO DC-ELAPSED-MONTHS
031507     MOVE +1                     TO DC-ELAPSED-DAYS
031507     MOVE '6'                    TO DC-OPTION-CODE
031507     PERFORM 8500-DATE-CONVERT   THRU 8599-EXIT
031507     IF NO-CONVERSION-ERROR
031507        STRING DC-EDIT1-MONTH  "/"
031507               DC-EDIT1-DAY    "/"
031507               DC-EDIT1-YEAR
031507           DELIMITED BY SIZE INTO H3-DATE1
031507     ELSE
031507        DISPLAY ' ERROR ADDING 1 TO PREV DATE ' PARM-VALUE (1:6)
031507        PERFORM ABEND-PGM
031507     END-IF

031507     MOVE PARM-VALUE (7:6)       TO DC-GREG-DATE-1-MDY
031507     MOVE '4'                    TO DC-OPTION-CODE
031507     PERFORM 8500-DATE-CONVERT   THRU 8599-EXIT
031507     IF NO-CONVERSION-ERROR
031507        STRING DC-GREG-DATE-1-MDY (1:2) "/"
031507               DC-GREG-DATE-1-MDY (3:2) "/"
031507               DC-GREG-DATE-1-MDY (5:2)
031507           DELIMITED BY SIZE INTO H3-DATE2
031507        MOVE DC-BIN-DATE-1       TO WS-CURR-CYCLE-DT
061814         move dc-greg-date-a-edit     to tbl-date
031507     ELSE
031507        DISPLAY ' INVALID CURR DATE ' PARM-VALUE (7:6)
031507        PERFORM ABEND-PGM
031507     END-IF

           MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT
           MOVE '2'                    TO  DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8599-EXIT
           MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DTE-BIN

           PERFORM OPEN-FILES.

           MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.
           MOVE DTE-CLIENT             TO  CF-COMPANY-ID.
           MOVE '1'                    TO  CF-RECORD-TYPE.
           MOVE SPACES                 TO  CF-ACCESS-CD-GENL.
           MOVE ZEROS                  TO  CF-SEQUENCE-NO.

           READ ELCNTL.

           IF ELCNTL-FILE-STATUS NOT = '00'
               MOVE 'ERROR OCCURED READ INITIAL - ELCNTL'
                                       TO  WS-ABEND-MESSAGE
               MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS
               GO TO ABEND-PGM.

           MOVE CF-CL-MAIL-TO-NAME     TO  H2-COMP.
           MOVE CF-COMPANY-ID          TO  WS-COMPANY-ID.
           MOVE CF-COMPANY-CD          TO  WS-COMPANY-CD.
           MOVE WS-CURRENT-DATE        TO  H2-DATE.
           MOVE ' THRU '               TO  H3-THRU.
      *    MOVE WS-CURRENT-DATE        TO  H3-DATE1.
      *    MOVE WS-CURRENT-DATE        TO  H3-DATE2.
           IF DTE-PGM-OPT = 2
              MOVE ALPH-DATE           TO  H3-DATE
           END-IF
           MOVE SPACES                 TO  HDR-4-INV
                                           HDR-4-REF.
           MOVE 'MNT DT'               TO  HDR-4-DT.

           MOVE LOW-VALUES             TO  PY-CONTROL-PRIMARY.
           MOVE WS-COMPANY-CD          TO  PY-COMPANY-CD.

           START ERPYAJ
               KEY IS GREATER THAN PY-CONTROL-PRIMARY.

pemuni     IF ERPYAJ-FILE-STATUS = '23' or '10'
               DISPLAY 'ECS682 NO RECORDS FOUND - ERPYAJ  FOR CO - '
                   WS-COMPANY-ID
pemuni         GO TO 3200-eof.

           IF ERPYAJ-FILE-STATUS NOT = ZERO
               MOVE 'ERROR OCCURED START - ERPYAJ'
                                       TO  WS-ABEND-MESSAGE
               MOVE ERPYAJ-FILE-STATUS TO  WS-ABEND-FILE-STATUS
               GO TO ABEND-PGM.

           EJECT

       3100-READNEXT.
           READ ERPYAJ NEXT RECORD.

           IF ERPYAJ-FILE-STATUS = '10'
               GO TO 3200-EOF.

           IF ERPYAJ-FILE-STATUS NOT = ZERO
               MOVE 'ERROR OCCURED READNEXT - ERPYAJ'
                                       TO  WS-ABEND-MESSAGE
               MOVE ERPYAJ-FILE-STATUS TO  WS-ABEND-FILE-STATUS
               GO TO ABEND-PGM.

           IF DTE-CLASIC-COMPANY-CD = PY-COMPANY-CD
               NEXT SENTENCE
           ELSE
               GO TO 3200-EOF.

           IF PY-CREDIT-ACCEPT-DT = LOW-VALUES
              CONTINUE
           ELSE
              GO TO 3100-READNEXT
           END-IF

           IF DTE-PGM-OPT NOT = 2
031507        IF PY-LAST-MAINT-DT > WS-PREV-CYCLE-DT
031507           AND <= WS-CURR-CYCLE-DT
031507           CONTINUE
031507        ELSE
                 GO TO 3100-READNEXT
              END-IF
           END-IF
111709
111709     IF PARM-VALUE (14:3) = 'DCC'
011116        IF PY-CARRIER = '1' OR '2' or '9'
111709          CONTINUE
111709        ELSE
111709          GO TO 3100-READNEXT
111709        END-IF
111709     END-IF
111709     IF PARM-VALUE (14:3) = 'CCC' 
052814        IF PY-CARRIER = '3' or '4' or '5' OR '6' or '7'
111709          CONTINUE
111709     ELSE
111709          GO TO 3100-READNEXT
111709        END-IF
111709     END-IF

           GO TO 5000-PRINT-IT

           .
       3200-EOF.
           IF WS-LINE-NUMBER = ZEROS
               PERFORM 6500-HDR-RTN   THRU  6599-EXIT
               MOVE DATA-2             TO  PRT
               PERFORM 7000-PRT-LINE  THRU  7099-EXIT
               GO TO 3400-SKIP.

           MOVE SPACES                 TO  DATA-1.

           ADD SUB-X  SUB-Y  SUB-Z  SUB-F  GIVING  D-AMT.

           IF D-AMT = ZERO
              MOVE '* CASH SUBTOTAL'   TO  D-TOTAL
              COMPUTE D-AMT =
                     SUB-R + SUB-D + SUB-S + SUB-T - SUB-C - SUB-U
              MOVE D-AMT               TO  D-AMOUNT
              MOVE ' '                 TO  X
              MOVE DATA-1              TO  PRT
              PERFORM 7000-PRT-LINE  THRU  7099-EXIT
              GO TO 3300-SKIP.

           MOVE '* CASH'               TO  D-TOTAL.

           COMPUTE D-AMT =
                  SUB-R + SUB-D + SUB-S + SUB-T - SUB-C - SUB-U.

           MOVE D-AMT                  TO  D-AMOUNT.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE '* NON-CASH '          TO  D-TOTAL.

           ADD SUB-X  SUB-Y  SUB-Z  SUB-F  GIVING  D-AMOUNT.

           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE '* SUB-TOTAL'          TO  D-TOTAL.
           COMPUTE SUB-ENTRY-AMT =
                   SUB-R + SUB-D + SUB-S + SUB-T - SUB-C - SUB-U +
                   SUB-X + SUB-Y + SUB-Z + SUB-F.
           MOVE SUB-ENTRY-AMT          TO  D-AMOUNT.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

       3300-SKIP.
           MOVE SPACES                 TO  DATA-1.
           MOVE 'REMITTANCES'          TO  D-TOTAL.
           COMPUTE D-AMOUNT = TOT-R + TOT-S.
           MOVE '-'                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE 'DEPOSITS  '           TO  D-TOTAL.
           COMPUTE D-AMOUNT = TOT-D + TOT-T.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE 'AGENT CHARGES'        TO  D-TOTAL.
           COMPUTE D-AMOUNT = TOT-C + TOT-U.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE 'CASH GRAND TOTAL'     TO  D-TOTAL.

           COMPUTE D-AMT = TOT-R + TOT-S + TOT-D + TOT-T - TOT-C - TOT-U

           MOVE D-AMT                  TO  D-AMOUNT.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           COMPUTE D-AMT = TOT-X + TOT-Y + TOT-Z + TOT-F.

           IF D-AMT = ZERO
               GO TO 3400-SKIP.

           MOVE 'ADD-TO-YTD'           TO  D-TOTAL.
           MOVE TOT-X                  TO  D-AMOUNT.
           MOVE '-'                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE 'SUB-FM-YTD'           TO  D-TOTAL.
           MOVE TOT-Y                  TO  D-AMOUNT.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE 'ADJ-BAL   '           TO  D-TOTAL.
           MOVE TOT-Z                  TO  D-AMOUNT.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE 'FICA-ENTRY'           TO  D-TOTAL.
           MOVE TOT-F                  TO  D-AMOUNT.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE 'OTHER GRAND TOTAL'    TO  D-TOTAL.

           COMPUTE D-AMT = TOT-X + TOT-Y + TOT-Z + TOT-F.

           MOVE D-AMT                  TO  D-AMOUNT.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           COMPUTE D-AMT = TOT-R + TOT-S + TOT-D + TOT-T - TOT-C - TOT-U
                         + TOT-X + TOT-Y + TOT-Z + TOT-F.

           MOVE '** GRAND TOTAL'       TO  D-TOTAL.
           MOVE D-AMT                  TO  D-AMOUNT.
           MOVE '0'                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

       3400-SKIP.
           MOVE SPACES                 TO  DATA-1.
           MOVE '*TOTAL RECORDS '      TO  D-TOTAL.
           MOVE REC-CNT                TO  DET-AMT.
           MOVE '0'                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE '1'                    TO  X.
           MOVE SPACES                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           PERFORM CLOSE-FILES.

011116     if (dte-client = 'DCC' or 'VPP')
061814        and (parm-value (14:3) not = 'DCC' AND 'CCC')
061814        continue
061814     else
061814        perform 8000-build-sql-table
061814                                 thru 8000-exit
061814     end-if

           GOBACK.

       5000-PRINT-IT.
           IF PRT-CNT GREATER THAN 58  OR
              PRT-CNT = ZEROS
               PERFORM 6500-HDR-RTN  THRU  6599-EXIT.

           MOVE +1                     TO  WS-LINE-NUMBER.
           MOVE PY-CARRIER             TO  PY-CARR.
           MOVE PY-GROUPING            TO  PY-GROUP.
           MOVE PY-FIN-RESP            TO  PY-RESP.
           MOVE PY-ACCOUNT             TO  PY-ACCT.

           IF PY-CONTROL = SV-CONTROL  OR
              SV-CONTROL = SPACES
               GO TO 5100-SKIP.

           MOVE SPACES                 TO  DATA-1.

           COMPUTE D-AMT =  SUB-X + SUB-Y + SUB-Z + SUB-F.

           IF D-AMT = ZERO
               MOVE '* CASH SUBTOTAL'  TO  D-TOTAL
               COMPUTE D-AMT = SUB-R + SUB-D + SUB-S + SUB-T -
                               SUB-C - SUB-U
               MOVE D-AMT              TO  D-AMOUNT
               MOVE ' '                TO  X
               MOVE DATA-1             TO  PRT
               PERFORM 7000-PRT-LINE  THRU  7099-EXIT
               MOVE ZEROS              TO  SUB-R SUB-S
                                           SUB-D SUB-T
                                           SUB-C SUB-U
               GO TO 5100-SKIP.

           MOVE '* CASH'               TO  D-TOTAL.

           COMPUTE D-AMT = SUB-R + SUB-D + SUB-S + SUB-T -
                           SUB-C - SUB-U.

           MOVE D-AMT                  TO  D-AMOUNT.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE '* NON-CASH '          TO  D-TOTAL.

           ADD SUB-X SUB-Y SUB-Z SUB-F GIVING D-AMOUNT.

           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE '* SUB-TOTAL'          TO  D-TOTAL.

           COMPUTE D-AMT =
               SUB-R + SUB-D + SUB-S + SUB-T + SUB-C + SUB-U +
               SUB-X + SUB-Y + SUB-Z + SUB-F.

           MOVE D-AMT                  TO  D-AMOUNT.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE SPACES                 TO  DATA-1.
           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.
           MOVE ZEROS                  TO  SUB-R SUB-D SUB-C
                                           SUB-S SUB-T SUB-U
                                           SUB-X SUB-Y SUB-Z
                                           SUB-F.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

       5100-SKIP.
           MOVE PY-CONTROL             TO  SV-CONTROL.
           MOVE SPACES                 TO  D-TOTAL.
           MOVE PY-CARRIER             TO  D-CARR.
           MOVE PY-GROUPING            TO  D-GROUP.
           MOVE PY-FIN-RESP            TO  D-RESP.
           MOVE PY-ACCOUNT             TO  D-ACCT.
pemuni     if py-account = spaces or low-values
pemuni        move spaces              to d-acct
pemuni     end-if
           MOVE PY-ENTRY-AMT           TO  D-AMOUNT.
           MOVE PY-ENTRY-COMMENT       TO  D-COMM.
           MOVE PY-LAST-MAINT-BY       TO  D-BY.

091108     MOVE PY-CARRIER             TO  EXT-CARR.
091108     MOVE PY-GROUPING            TO  EXT-GROUP.
091108     MOVE PY-FIN-RESP            TO  EXT-RESP.
091108     MOVE PY-ACCOUNT             TO  EXT-ACCT.
091108     IF PY-ACCOUNT = SPACES OR LOW-VALUES
091108        MOVE SPACES              TO EXT-ACCT
091108     END-IF
091108     MOVE PY-ENTRY-AMT           TO  EXT-AMOUNT.
091108     MOVE PY-ENTRY-COMMENT       TO  EXT-COMM.
091108     MOVE PY-LAST-MAINT-BY       TO  EXT-BY.
           
           
           MOVE SPACE                  TO  DC-OPTION-CODE.

           MOVE PY-LAST-MAINT-DT       TO DC-BIN-DATE-1.

           PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.

           MOVE DC-GREG-DATE-1-EDIT    TO  D-MEDT.
091108     MOVE DC-GREG-DATE-1-EDIT    TO  EXT-MEDT.

           IF PY-RECORD-TYPE = 'R'
               ADD PY-ENTRY-AMT        TO  SUB-R TOT-R
               MOVE 'REMIT-RECD'       TO  D-TYPE.

           IF PY-RECORD-TYPE = 'D'
               ADD PY-ENTRY-AMT        TO  SUB-D TOT-D
               MOVE 'DEPOSIT   '       TO  D-TYPE.

           IF PY-RECORD-TYPE = 'C'
               ADD PY-ENTRY-AMT        TO  SUB-C TOT-C
               MOVE 'CHG-TO-AGT'       TO  D-TYPE.

           IF PY-RECORD-TYPE = 'S'
               ADD PY-ENTRY-AMT        TO  SUB-S TOT-S
               MOVE 'ADJ REMIT '       TO  D-TYPE.

           IF PY-RECORD-TYPE = 'T'
               ADD PY-ENTRY-AMT        TO  SUB-T TOT-T
               MOVE 'ADJ DEP   '       TO  D-TYPE.

           IF PY-RECORD-TYPE = 'U'
               ADD PY-ENTRY-AMT        TO  SUB-U TOT-U
               MOVE 'ADJ CHARGE'       TO  D-TYPE.

           IF PY-RECORD-TYPE = 'X'
               ADD PY-ENTRY-AMT        TO  SUB-X TOT-X
               MOVE 'ADD-TO-YTD'       TO  D-TYPE.

           IF PY-RECORD-TYPE = 'Y'
               ADD PY-ENTRY-AMT        TO  SUB-Y TOT-Y
               MOVE 'SUB-FM-YTD'       TO  D-TYPE.

           IF PY-RECORD-TYPE = 'Z'
               ADD PY-ENTRY-AMT        TO  SUB-Z TOT-Z
               MOVE 'ADJ-BAL   '       TO  D-TYPE.

           IF PY-RECORD-TYPE = 'F'
               ADD PY-ENTRY-AMT        TO  SUB-F TOT-F
               MOVE 'FICA-ENTRY'       TO  D-TYPE.

091108     MOVE PY-RECORD-TYPE         TO EXT-TYPE.
091108     MOVE D-TYPE                 TO EXT-TYPE-DESC.
091108     IF DTE-PGM-OPT NOT EQUAL 2
091108         WRITE EXTRACT-RECORD FROM EXTRACT-REC
091108     END-IF.

           MOVE ' '                    TO  X.
           MOVE DATA-1                 TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           ADD 1                       TO  REC-CNT.

           GO TO 3100-READNEXT.

       6500-HDR-RTN.
           MOVE +0                     TO  PRT-CNT.

           ADD +1                      TO  WS-PAGE.

           MOVE WS-PAGE                TO  H3-PAGE.
           MOVE '1'                    TO  X.
           MOVE HDR-1                  TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE ' '                    TO  X.
           MOVE HDR-2                  TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE ' '                    TO  X.
           MOVE HDR-3                  TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

           MOVE '0'                    TO  X.
           MOVE HDR-4                  TO  PRT.

           PERFORM 7000-PRT-LINE  THRU  7099-EXIT.

       6599-EXIT.
           EXIT.

       7000-PRT-LINE.
                                       COPY ELCPRT2X.

      *****************************************************************
      *                                                               *
      * THIS SECTION CONTROLS THE PAGE HEADINGS.                      *
      *****************************************************************
       WAL-010.

           IF P-CTL = '1'
               MOVE +1  TO  PRT-CNT
             ELSE
               IF P-CTL = ' '
                   ADD +1  TO  PRT-CNT
                 ELSE
                   IF P-CTL = '0'
                       ADD +2  TO  PRT-CNT
                     ELSE
                       ADD +3  TO  PRT-CNT.

       WAL-EXIT.
           EXIT.

       7099-EXIT.
            EXIT.

061814 8000-build-sql-table.
061814
100114     if dte-pgm-opt = 2
100114        display ' Bypass SQL update - month-end '
100114        go to 8000-exit
100114     end-if
120821     move 'HOVTSTDB01_GLDailyBalancing'
120821                                 to svr
120821     move 'appuser'              to usr
120821     move 'appuser@cso'          to pass
120821
120821     if ws-kix-myenv = 'cid1p'
120821        move 'SDVDB01_GLDailyBalancing'
120821                                 to svr
120821        move 'appuser'           to usr
120821        move 'appuser@cso'       to pass
120821     end-if
06181
061814     display 'Begin connect to DB '
061814     string
061814         usr delimited space
061814         "." delimited size
061814         pass delimited space into usr-pass
061814     end-string
061814
061814     display ' usr pass ' usr-pass
061814
061814     EXEC SQL
061814        CONNECT TO :svr USER :usr-pass
061814     END-EXEC
061814
061814     if sqlcode not = 0
061814        display "Error: cannot connect "
061814        display sqlcode
061814        display sqlerrmc
061814        goback
061814     end-if
061814
061814     display " connect to DB successful "
061814
061814*    perform 8010-drop-table     thru 8010-exit
061814*    perform 8020-truncate-table thru 8020-exit
061814*    perform 8030-create-table   thru 8030-exit
061814     perform 8040-insert-row     thru 8040-exit
061814     perform 8050-finish-up      thru 8050-exit
061814
061814     .
061814 8000-exit.
061814     exit.
061814
061814 8010-drop-table.
061814
061814     display 'Begin Drop table'
061814     EXEC SQL
061814         drop table DAILYGLINT
061814     END-EXEC
061814     if sqlcode not = 0
061814        display "Error(anticipated) : cannot drop table "
061814        display sqlcode
061814        display sqlerrmc
061814*       goback
061814     end-if
061814
061814     display ' sql return code ' sqlcode
061814     display ' sql err mess    ' sqlerrmc
061814
061814     .
061814 8010-exit.
061814     exit.
061814
061814 8020-truncate-table.
061814
061814     display '*** Begin Truncate table ***'
061814     EXEC SQL
061814         truncate table DAILYGLINT
061814     END-EXEC
061814     if sqlcode not = 0
061814        display "Error : cannot truncate table "
061814        display sqlcode
061814        display sqlerrmc
061814        goback
061814     end-if
061814
061814     display ' sql return code ' sqlcode
061814     display ' sql err mess    ' sqlerrmc
061814     display '***  End  Truncate table ***'
061814
061814     .
061814 8020-exit.
061814     exit.
061814
061814 8030-create-table.
061814
061814     display ' Begin Create table'
061814     EXEC SQL
061814        create table DAILYGLINT (
061814         compid char(5),
061814         batchno char(50),
061814         carrier char(1),
061814         procdate datetime,
061814         sourcerpt char(8),
061814         dlyremits decimal(9,2),
061814         dlycharges decimal(9,2),
061814         dlyclmpmts decimal(9,2),
061814         dlystoppays decimal(9,2),
061814         dlyoffline decimal(9,2))
061814     END-EXEC
061814     if sqlcode not = 0
061814        display "Error: cannot create table "
061814        move sqlcode             to ws-disp-code
061814        display ' sql return code ' ws-disp-code
061814        display ' sql err mess    ' sqlerrmc
061814        goback
061814     end-if
061814
061814     display " Create table successful "
061814
061814     .
061814 8030-exit.
061814     exit.
061814
061814 8040-insert-row.
061814
061814     evaluate true
061814        when dte-client = 'CID'
061814           MOVE 'CSO'            to tbl-comp-id
061814           move 'Batch 95 CPS Payments & Adjustments'
061814                                 to tbl-batch-no
061814        when dte-client = 'AHL'
061814           MOVE 'AHL'            to tbl-comp-id
061814           move 'Batch 5 AHL Payments & Adjustments'
061814                                 to tbl-batch-no
062121        when dte-client = 'FNL'
062121           MOVE 'FNL'            to tbl-comp-id
062121           move 'Batch 5 FNL Payments & Adjustments'
062121                                 to tbl-batch-no
112216        when PARM-VALUE (14:3) = 'CCC'
061814           move 'CCC'            to tbl-comp-id
061814           move 'Batch 5 CCC DCC Payments & Adjustments'
061814                                 to tbl-batch-no
061814     end-evaluate
061814
061814*    move current-date           to tbl-date
061814     move 'EL682'                to tbl-source-rpt
061814     move '0000000.00'           to tbl-remits
061814                                    tbl-charges
061814                                    tbl-other
061814                                    tbl-clm-stop-pays
061814                                    tbl-clm-offline-pmts
061814                                    tbl-clm-pmts
061814
061814     compute em-remits = tot-r + tot-s
061814     compute em-charges = tot-c + tot-u
061814
061814     EXEC SQL
061814        insert into DAILYGLINT (
061814         compid,
061814         batchno,
061814         carrier,
061814         procdate,
061814         sourcerpt,
061814         dlyremits,
061814         dlycharges,
061814         dlyclmpmts,
061814         dlystoppays,
061814         dlyoffline)
061814         values (
061814            :tbl-comp-id,
061814            :tbl-batch-no,
061814            :tbl-carrier,
061814            :tbl-date,
061814            :tbl-source-rpt,
061814            :tbl-remits,
061814            :tbl-charges,
061814            :tbl-clm-pmts,
061814            :tbl-clm-stop-pays,
061814            :tbl-clm-offline-pmts)
061814     END-EXEC
061814     if sqlcode not = 0
061814        display "Error: cannot insert row "
061814        display ' sql return code ' sqlcode
061814        display ' sql err mess    ' sqlerrmc
061814        goback
061814     end-if
061814
061814     .
061814 8040-exit.
061814     exit.
061814
061814 8050-finish-up.
061814
061814     display ' Begin Commit '
061814     EXEC SQL
061814         commit transaction
061814     END-EXEC
061814     if sqlcode not = 0
061814        display "Error: commit "
061814        display ' sql return code ' sqlcode
061814        display ' sql err mess    ' sqlerrmc
061814        goback
061814     end-if
061814
061814     display " Commit trans successful "
061814
061814     display ' Begin Disconnect '
061814     EXEC SQL
061814         commit work
061814     END-EXEC
061814     if sqlcode not = 0
061814        display "Error: commit release "
061814        display ' sql return code ' sqlcode
061814        display ' sql err mess    ' sqlerrmc
061814     end-if
061814
120821     EXEC SQL
120821         disconnect
120821     END-EXEC
120821
120821     if sqlcode not = 0
120821        display "Error: disconnect "
120821        display ' sql return code ' sqlcode
120821        display ' sql err mess    ' sqlerrmc
120821     end-if

061814     .
061814 8050-exit.
061814     exit.

       8500-DATE-CONVERT.
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.

       8599-EXIT.
           EXIT.

       EJECT
       OPEN-FILES SECTION.
       OFS-010.
           OPEN INPUT ERPYAJ
                      ELCNTL
               OUTPUT PRNTR.
               
           IF ERPYAJ-FILE-STATUS  = '00' OR '97'
              NEXT SENTENCE
           ELSE
              MOVE '*** ECS682  ERPYAJ OPEN ERROR - JOB WILL ABEND'
                   TO WS-ABEND-MESSAGE
              MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS
              GO TO ABEND-PGM.

           IF ELCNTL-FILE-STATUS  = '00' OR '97'
               NEXT SENTENCE
             ELSE
               MOVE '*** ECS682  ELCNTL OPEN ERROR - JOB WILL ABEND'
                   TO WS-ABEND-MESSAGE
               MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS
               GO TO ABEND-PGM.

091108     IF DTE-PGM-OPT NOT EQUAL 2
091108         OPEN OUTPUT EXTRACT
091108         WRITE EXTRACT-RECORD FROM EXTRACT-HDR
091108     END-IF.               

       OFS-EXIT.
           EXIT.

       EJECT
       CLOSE-FILES SECTION.        COPY ELCPRTCX.

       CFS-010.
           CLOSE ERPYAJ
                 ELCNTL
                 PRNTR.

091108     IF DTE-PGM-OPT NOT EQUAL 2
091108         CLOSE EXTRACT
091108     END-IF.               

           IF ERPYAJ-FILE-STATUS NOT = ZEROS
               MOVE '*** ECS682  ERPYAJ CLOSE ERROR - JOB WILL ABEND'
                   TO WS-ABEND-MESSAGE
               MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS
               GO TO ABEND-PGM.

           IF ELCNTL-FILE-STATUS NOT = ZEROS
               MOVE '*** ECS682  ELCNTL CLOSE ERROR - JOB WILL ABEND'
                   TO WS-ABEND-MESSAGE
               MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS
               GO TO ABEND-PGM.

       CFS-EXIT.
           EXIT.

       ABEND-PGM SECTION.          COPY ELCABEND.
