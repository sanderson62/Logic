       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDNAPEX.
       AUTHOR.     PABLO.
       DATE-COMPILED.
033110******************************************************************
033110*                   C H A N G E   L O G
033110*
033110* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
033110*-----------------------------------------------------------------
033110*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
033110* EFFECTIVE    NUMBER
033110*-----------------------------------------------------------------
033110* 033110    2009122800001  AJRA  NEW PROGRAM
102810* 102810    2009122800001  AJRA  BYPASS LETTER IF STOP DATE EXISTS
102810*                                ADD PROCESSOR ID AND COMPANY ID
031912* 031912    2011120900003  AJRA  ADD AHL CLAIM NO TO EXTRACT
012313* 012313    2012110800002  AJRA  ADD ACCOUNT GPCD TO EXTRACT
041113* 041113    2013040400002  AJRA	 CREATE STOP QWS LETTER FILE
041813* 041813    2013040400004  AJRA  CREATE SPEC HANDLE QWS LETTER FILE
031116* 031116    2015110400001  TANA  ADD EL150D FIELDS
071719* 071719    2019011600010  TANA  ADD VERFICATION CODE
082322* 022122  CR2021100800003  PEMA  Increase data occurences
033110******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELNAPS           ASSIGN TO ELNAPS
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS NA-CONTROL-PRIMARY
                                   FILE STATUS IS ELNAPS-FILE-STATUS.

           SELECT ERACCT           ASSIGN TO ERACCT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT ELMSTR           ASSIGN TO ELMSTR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CL-CONTROL-PRIMARY
                                   FILE STATUS IS ELMSTR-FILE-STATUS.

           SELECT ELTRLR           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AT-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLR-FILE-STATUS.

           SELECT ELCERT           ASSIGN TO ELCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CM-CONTROL-PRIMARY
                                   FILE STATUS IS ELCERT-FILE-STATUS.

           SELECT ELBENE           ASSIGN TO ELBENE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS BE-CONTROL-PRIMARY
                                   FILE STATUS IS ELBENE-FILE-STATUS.

031116     SELECT ELCRTT           ASSIGN TO ELCRTT
031116                             ORGANIZATION IS INDEXED
031116                             ACCESS IS DYNAMIC
031116                             RECORD KEY IS CS-CONTROL-PRIMARY
031116                             FILE STATUS IS ELCRTT-FILE-STATUS.
031116
031116     SELECT ERPDEF           ASSIGN TO ERPDEF
031116                             ORGANIZATION IS INDEXED
031116                             ACCESS IS DYNAMIC
031116                             RECORD KEY IS PD-CONTROL-PRIMARY
031116                             FILE STATUS IS ERPDEF-FILE-STATUS.


           SELECT NAPER-OUT       ASSIGN TO NAPEROT
               ORGANIZATION IS LINE SEQUENTIAL.

041113     SELECT STOP-QWS        ASSIGN TO STOPQWS
041113         ORGANIZATION IS LINE SEQUENTIAL.
041113
041813     SELECT QWS-SPEC-HNDL    ASSIGN TO QWSSPEC
041813         ORGANIZATION IS LINE SEQUENTIAL.
041813
           SELECT DISK-DATE        ASSIGN TO SYS019.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ELNAPS.
       
           COPY ELCNAPS.

       FD  ERACCT.

           COPY ERCACCT.

       FD  ELMSTR.

           COPY ELCMSTR.

       FD  ELTRLR.

           COPY ELCTRLR.

       FD  ELCERT.

           COPY ELCCERT.

       FD  ELBENE.

           COPY ELCBENE.

031116 FD  ELCRTT.

031116     COPY ELCCRTT.

031116 FD  ERPDEF.

031116     COPY ERCPDEF.


       FD  NAPER-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  NAPER-OUT-REC              PIC X(1100).
041113
041113 FD  STOP-QWS
041113     RECORDING MODE F
041113     LABEL RECORDS STANDARD
041113     BLOCK CONTAINS 0 RECORDS.
041113
041113 01  STOP-QWS-RECORD            PIC X(34).
041813
041813 FD  QWS-SPEC-HNDL
041813     RECORDING MODE F
041813     LABEL RECORDS STANDARD
041813     BLOCK CONTAINS 0 RECORDS.
041813
041813 01  QWS-SPEC-HNDL-RECORD       PIC X(34).


       FD  DISK-DATE
           COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMCLX2  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ELNAPS             VALUE 'Y'.
       77  NAPS-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  CLM-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  CRT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  TRL-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  CLM-RECS-OUT            PIC 9(9) VALUE ZEROS.
041113 77  QWS-RECS-STOPPED        PIC 9(9) VALUE ZEROS.
041813 77  QWS-SPEC-RECS           PIC 9(9) VALUE ZEROS.
       77  WS-DIAGNOSIS            PIC X(60) VALUE SPACES.
       77  WS-LAST-ACT-DT          PIC XX    VALUE LOW-VALUES.
       77  WS-ACTIVITY-DT          PIC XX    VALUE LOW-VALUES.
       77  WS-AUTO-LAST-SCHED-DT   PIC XX    VALUE LOW-VALUES.
       77  WS-LAST-ACT-TYPE        PIC X(15) VALUE SPACES.
       77  WS-ACTIVITY-TYPE        PIC X(15) VALUE SPACES.
       77  WS-FORM                 PIC X(4)  VALUE SPACES.
       77  WS-LETTER-ID            PIC XXXX  VALUE SPACES.
       77  WS-GOT-IT-SW            PIC X     VALUE ' '.
           88  WE-GOT-IT-ALL                 VALUE 'Y'.
       77  WS-DONE-WITH-ACCT       PIC X     VALUE 'N'.
           88  DONE-WITH-ACCT                VALUE 'Y'.
           88  NOT-DONE-WITH-ACCT            VALUE 'N'.
031116 77  S1                          PIC S999 COMP-3 VALUE +0.
031116 77  S2                          PIC S999 COMP-3 VALUE +0.
031116 77  S3                          PIC S999 COMP-3 VALUE +0.
031116 77  M1                          PIC S999 COMP-3 VALUE +0.
031116 77  P1                          PIC S999 COMP-3 VALUE +0.
031116 77  WS-HOLD-KEY                 PIC X(20).
031116 77  WS-HOLD-S1                  PIC S999 COMP-3 VALUE +0.
031116 77  WS-CNTR                     pic s999 comp-3 value +0.
031116 77  WS-MONTHS-BETWEEN           PIC S999 COMP-3 VALUE +0.
031116 77  WS-ACCUM-DAYS               PIC S9(5) COMP-3 VALUE +0.
031116 77  WS-ACCUM-AMT                PIC S9(9)V99 COMP-3 VALUE +0.
031116 77  WS-ACCUM-PD-BENS            PIC S999 COMP-3 VALUE +0.
031116 77  WS-PREV-CLM-TYPE            PIC X   VALUE ' '.
031116 77  WS-PREV-INS-TYPE            PIC X   VALUE ' '.
031116 77  WS-PREV-BEN-PER             PIC 99 VALUE ZEROS.
031116 77  WS-PDEF-RECORD-SW           PIC X  VALUE ' '.
031116     88  PDEF-FOUND                   VALUE 'Y'.
031116 77  WK1                         PIC 999 VALUE ZEROS.
031116 77  WK2                         PIC 999 VALUE ZEROS.
031116 77  WS-WORK-BEN-PCT             PIC S9V999 COMP-3 VALUE +0.
031116 77  WS-MAX-SUB                  PIC 9(4)    VALUE ZEROS.
031116 77  WS-CLAIM-READ               PIC X VALUE SPACES.
031116     88  GOOD-CLAIM-READ            VALUE 'Y'.

031116 01  OUTPUT-SCREEN-WORK-AREA.
031116     05  OS-PREV-KEY.
031116         10  OS-PREV-CLM-TYPE        PIC X.
031116         10  OS-PREV-INS-TYPE        PIC X.
031116     05  WS-PD-BENS                  PIC 999 VALUE ZEROS.
031116     05  WS-COV-REM-BENS             PIC S999 VALUE ZEROS.

       01  WORK-AREA.
           05  WRK-END-FOUND           PIC X   VALUE 'N'.
               88 END-FOUND                    VALUE 'Y'.
       
           05  WS-INT-RATE             PIC S99V9(5)  COMP-3.

       01  WS-LOAN-NO                  PIC X(25).
       01  WS-INS-NAME-AND-ADDRESS.
           05  WS-INS-NAME             PIC X(30).
           05  WS-INS-ADDR1            PIC X(30).
           05  WS-INS-ADDR2            PIC X(30).
           05  WS-INS-CITY             PIC X(30).
           05  WS-INS-STATE            PIC X(2).
           05  WS-INS-ZIP              PIC X(9).
           05  WS-INS-PHONE            PIC X(13).
           05  WS-WORK-PHONE           PIC X(11).

       01  WS-BEN-NAME-AND-ADDRESS.
           05  WS-BEN-NAME             PIC X(30).
           05  WS-BEN-ADDR1            PIC X(30).
           05  WS-BEN-ADDR2            PIC X(30).
           05  WS-BEN-CITY             PIC X(30).
           05  WS-BEN-STATE            PIC X(2).
           05  WS-BEN-ZIP              PIC X(9).
           05  WS-BEN-PHONE            PIC X(13).

       01  WS-ACCT-NAME-AND-ADDRESS.
           05  WS-ORIG-ACCT-NAME        PIC X(30).
           05  WS-ACCT-NAME             PIC X(30).
           05  WS-ACCT-ADDR1            PIC X(30).
           05  WS-ACCT-ADDR2            PIC X(30).
           05  WS-ACCT-CITY             PIC X(30).
           05  WS-ACCT-STATE            PIC X(2).
           05  WS-ACCT-ZIP              PIC X(9).
           05  WS-ACCT-PHONE            PIC X(13).
012313     05  WS-ACCT-GPCD             PIC 99.

031116 01  ERPDEF-KEY-SAVE             PIC X(18).
031116 01  ERPDEF-KEY.
031116     12  ERPDEF-COMPANY-CD       PIC X.
031116     12  ERPDEF-STATE            PIC XX.
031116     12  ERPDEF-PROD-CD          PIC XXX.
031116     12  F                       PIC X(7).
031116     12  ERPDEF-BEN-TYPE         PIC X.
031116     12  ERPDEF-BEN-CODE         PIC XX.
031116     12  ERPDEF-EXP-DT           PIC XX.
031116 01  TEXT-WORK-AREAS.
031116     05  WS-COV-TYPE             PIC X(4) VALUE SPACES.


031116 01  WS-UNSORTED-TABLE.
031116     12  WS-UNSRTD-TABLE   OCCURS 25 TIMES.
031116         16  WS-KEY.
031116             20  WS-CLM-TYPE     PIC X.
031116             20  WS-INS-TYPE     PIC X.
031116             20  WS-BEN-PER      PIC 99.
031116             20  WS-INC-DT       PIC XX.
031116         16  WS-EXCL-PER         PIC 999.
031116         16  WS-COV-ENDS         PIC 999.
031116         16  WS-ACC-PER          PIC 999.
031116         16  WS-MAX-BENS         PIC 999.
031116         16  WS-REC-MOS          PIC 99.
031116         16  WS-MAX-EXTEN        PIC 99.
031116         16  WS-STATUS           PIC X.
031116         16  WS-PD-THRU-DT       PIC XX.
031116         16  WS-CLAIM-NO         PIC X(7).
031116         16  WS-MAX-MOBEN        PIC S9(7)V99 COMP-3.
031116         16  WS-TOTAL-PAID       PIC S9(7)V99 COMP-3.
031116         16  WS-REM-BENS         PIC 999.
031116         16  WS-SORTED-SW        PIC X.

031116 01  WS-SORTED-TABLE.
031116     12  WS-SRTD-TABLE OCCURS 25 TIMES.
031116         16  WS-SRTD-KEY.
031116             20  WS-SRTD-CLM-TYPE PIC X.
031116             20  WS-SRTD-INS-TYPE PIC X.
031116             20  WS-SRTD-BEN-PER PIC 99.
031116             20  WS-SRTD-INC-DT  PIC XX.
031116         16  WS-SRTD-EXCL-PER    PIC 999.
031116         16  WS-SRTD-COV-ENDS    PIC 999.
031116         16  WS-SRTD-ACC-PER     PIC 999.
031116         16  WS-SRTD-MAX-BENS    PIC 999.
031116         16  WS-SRTD-REC-MOS     PIC 99.
031116         16  WS-SRTD-MAX-EXTEN   PIC 99.
031116         16  WS-SRTD-STATUS      PIC X.
031116         16  WS-SRTD-PD-THRU-DT  PIC XX.
031116         16  WS-SRTD-CLAIM-NO    PIC X(7).
031116         16  WS-SRTD-MAX-MOBEN   PIC S9(7)V99 COMP-3.
031116         16  WS-SRTD-TOTAL-PAID  PIC S9(7)V99 COMP-3.
031116         16  WS-SRTD-REM-BENS    PIC 999.
031116         16  WS-SRTD-SW          PIC X.


       01  WS-SAVE-NAPER              PIC X(1100) VALUE LOW-VALUES.
012313****currently 1040 
       01  NAPER-DETAIL-RECORD.
           12  EX-CYCLE-DATE           PIC X(8).
           12  EX-TAB00                PIC X.
           12  EX-LETTER-ID            PIC X(4).
           12  EX-TAB01                PIC X.
           12  EX-CARRIER              PIC X.
           12  EX-TAB02                PIC X.
           12  EX-CLAIM-NO             PIC X(7).
           12  EX-TAB03                PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB04                PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB05                PIC X.
           12  EX-INSURED-LAST-NAME    PIC X(15).
           12  EX-TAB06                PIC X.
           12  EX-INSURED-1ST-NAME     PIC X(11).
           12  EX-TAB07                PIC X.
           12  EX-INSURED-SEX-CD       PIC X.
           12  EX-TAB08                PIC X.
           12  EX-SSN                  PIC X(11).
           12  EX-TAB09                PIC X.
           12  EX-CLAIM-STATUS         PIC X.
           12  EX-TAB10                PIC X.
           12  EX-CLAIM-TYPE           PIC X.
           12  EX-TAB11                PIC X.
           12  EX-FILE-ESTABLISH-DT    PIC X(10).
           12  EX-TAB12                PIC X.
           12  EX-INCURRED-DT          PIC X(10).
           12  EX-TAB13                PIC X.
           12  EX-REPORTED-DT          PIC X(10).
           12  EX-TAB14                PIC X.
           12  EX-PAID-THRU-DT         PIC X(10).
           12  EX-TAB15                PIC X.
           12  EX-LAST-PMT-DT          PIC X(10).
           12  EX-TAB16                PIC X.
           12  EX-NO-OF-PMTS           PIC ZZ9.
           12  EX-TAB17                PIC X.
           12  EX-TOTAL-PAID-AMT       PIC -9(7).99.
           12  EX-TAB18                PIC X.
           12  EX-LAST-PAID-AMT        PIC -9(7).99.
           12  EX-TAB19                PIC X.
           12  EX-ACCOUNT-ADDR-CNT     PIC 9.
           12  EX-TAB20                PIC X.
           12  EX-CERT-STATE           PIC XX.
           12  EX-TAB21                PIC X.
           12  EX-CERT-ACCOUNT         PIC X(10).
           12  EX-TAB22                PIC X.
           12  EX-CERT-EFF-DT          PIC X(10).
           12  EX-TAB23                PIC X.
           12  EX-LAST-MAINT-DT        PIC X(10).
           12  EX-TAB24                PIC X.
           12  EX-LAST-MAINT-USER      PIC X(4).
           12  EX-TAB25                PIC X.
           12  EX-LAST-MAINT-TYPE      PIC X.
           12  EX-TAB26                PIC X.
           12  EX-DIAG                 PIC X(60).
           12  EX-TAB27                PIC X.
           12  EX-EXP-DT               PIC X(10).
           12  EX-TAB28                PIC X.
           12  EX-LAST-CLOSE-REASON    PIC X.
           12  EX-TAB29                PIC X.
           12  EX-BIRTH-DT             PIC X(10).
           12  EX-TAB30                PIC X.
           12  EX-TERM                 PIC 999.
           12  EX-TAB31                PIC X.
           12  EX-BEN-CODE             PIC XX.
           12  EX-TAB32                PIC X.
           12  EX-BENEFIT-AMT          PIC -9(7).99.
           12  EX-TAB33                PIC X.
           12  EX-CRIT-PERIOD          PIC 99.
           12  EX-TAB34                PIC X.
           12  EX-WAIT-PERIOD          PIC XX.
           12  EX-TAB35                PIC X.
           12  EX-LF-INTEREST-RATE     PIC 99.9(5).
           12  EX-TAB36                PIC X.
           12  EX-AUTOPYDT             PIC X(10).
           12  EX-TAB37                PIC X.
           12  EX-AUTO-LAST-SCHED-DT   PIC X(10).
           12  EX-TAB38                PIC X.
           12  EX-LOAN-NO              PIC X(25).
           12  EX-TAB39                PIC X.
           12  EX-APR                  PIC Z9.9999.
           12  EX-TAB40                PIC X.
           12  EX-LAST-ACT-DT          PIC X(10).
           12  EX-TAB41                PIC X.
           12  EX-ACTIVITY-DT          PIC X(10).
           12  EX-TAB42                PIC X.
           12  EX-LAST-ACT-TYPE        PIC X(15).
           12  EX-TAB43                PIC X.
           12  EX-ACTIVITY-TYPE        PIC X(15).
           12  EX-TAB44                PIC X.
           12  EX-FORM                 PIC X(4).
           12  EX-TAB45                PIC X.
           12  EX-INS-NAME             PIC X(30).
           12  EX-TAB46                PIC X.
           12  EX-INS-ADDR1            PIC X(30).
           12  EX-TAB47                PIC X.
           12  EX-INS-ADDR2            PIC X(30).
           12  EX-TAB48                PIC X.
           12  EX-INS-CITY             PIC X(30).
           12  EX-TAB49                PIC X.
           12  EX-INS-STATE            PIC X(2).
           12  EX-TAB50                PIC X.
           12  EX-INS-ZIP              PIC X(9).
           12  EX-TAB51                PIC X.
           12  EX-INS-PHONE            PIC X(13).
           12  EX-TAB52                PIC X.
           12  EX-BEN-NAME             PIC X(30).
           12  EX-TAB53                PIC X.
           12  EX-BEN-ADDR1            PIC X(30).
           12  EX-TAB54                PIC X.
           12  EX-BEN-ADDR2            PIC X(30).
           12  EX-TAB55                PIC X.
           12  EX-BEN-CITY             PIC X(30).
           12  EX-TAB56                PIC X.
           12  EX-BEN-STATE            PIC X(2).
           12  EX-TAB57                PIC X.
           12  EX-BEN-ZIP              PIC X(9).
           12  EX-TAB58                PIC X.
           12  EX-BEN-PHONE            PIC X(13).
           12  EX-TAB59                PIC X.
           12  EX-ACCT-NAME            PIC X(30).
           12  EX-TAB60                PIC X.
           12  EX-ACCT-ADDR1           PIC X(30).
           12  EX-TAB61                PIC X.
           12  EX-ACCT-ADDR2           PIC X(30).
           12  EX-TAB62                PIC X.
           12  EX-ACCT-CITY            PIC X(30).
           12  EX-TAB63                PIC X.
           12  EX-ACCT-STATE           PIC X(2).
           12  EX-TAB64                PIC X.
           12  EX-ACCT-ZIP             PIC X(9).
           12  EX-TAB65                PIC X.
           12  EX-ACCT-PHONE           PIC X(13).
           12  EX-TAB66                PIC X.
           12  EX-ENCLOSURE-CD         PIC X(3).
           12  EX-TAB67                PIC X.
           12  EX-ENC-STATE            PIC XX.
           12  EX-TAB68                PIC X.
           12  EX-ARCHIVE-NO           PIC 9(8).
           12  EX-TAB69                PIC X.
           12  EX-1ST-LTR-PRINT-DT     PIC X(10).
           12  EX-TAB70                PIC X.
           12  EX-NEXT-DUE-DT          PIC X(10).
           12  EX-TAB71                PIC X.
           12  EX-JT-BORROWER-LAST-NAME PIC X(15).
           12  EX-TAB72                PIC X.
           12  EX-JT-BORROWER-1ST-NAME PIC X(11).
           12  EX-TAB73                PIC X.
           12  EX-ORIG-ACCT-NAME       PIC X(30).
           12  EX-TAB74                PIC X.
           12  EX-ORIG-ARCHIVE-NO      PIC 9(9).
           12  EX-TAB75                PIC X.
           12  EX-RESEND-PROMPT-IND    PIC X.
           12  EX-TAB76                PIC X.
102810     12  EX-PROCESSOR-ID         PIC X(4).
102810     12  EX-TAB77                PIC X.
102810     12  EX-COMPANY-ID           PIC X(3).
102810     12  EX-TAB78                PIC X.
031912     12  EX-AHL-CLAIM-NO         PIC X(9).
031912     12  EX-TAB79                PIC X.
012313     12  EX-ACCT-GPCD            PIC 99.
012313     12  EX-TAB80                PIC X.
031116     12  EX-MAX-BENS             PIC 9(3).
031116     12  EX-TAB81                PIC X.
031116     12  EX-PAID-BENS            PIC 9(3).
031116     12  EX-TAB82                PIC X.
031116     12  EX-REM-BENS             PIC 9(3).
031116     12  EX-TAB83                PIC X.
031116     12  EX-EXCL-PER             PIC 9(3).
031116     12  EX-TAB84                PIC X.
031116     12  EX-REC-MOS              PIC 9(2).
031116     12  EX-TAB85                PIC X.
031116     12  EX-INS-TYP              PIC X(4).
031116     12  EX-TAB86                PIC X.
031116     12  EX-BEN-PER              PIC 9(2).
031116     12  EX-TAB87                PIC X.
031116     12  EX-ACC-SW               PIC X.
031116     12  EX-TAB88                PIC X.
071719     12  EX-VER-CD               PIC X(5).
071719     12  EX-TAB89                PIC X.
           12  EX-LAST-BYTE            PIC X.
041113
041113 01  STOP-QWS-REC                PIC X(34).
041113           
041813
041813 01  QWS-SPEC-HNDL-REC           PIC X(34).
041813           
      ******************************************************************
       01  WS-MISC.
           05  ELNAPS-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ERACCT-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELMSTR-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELTRLR-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELCERT-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELBENE-FILE-STATUS      PIC XX     VALUE ZEROS.
031116     05  ELCRTT-FILE-STATUS      PIC XX     VALUE ZEROS.
031116     05  ERPDEF-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-CURRENT-BIN-DT       PIC X(02)  VALUE LOW-VALUES.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.
           05  PGM-SUB          COMP-3 PIC S9(04) VALUE +585.
           05  WS-RETURN-CODE   COMP   PIC S9(03) VALUE +0.
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
           05  WS-ZERO          COMP-3 PIC S9(01) VALUE +0.
           05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.
       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-CYCLE-DATE             PIC X(08)     VALUE SPACES.
       

       PROCEDURE DIVISION USING PARM.

            IF PARM-CYCLE-DATE = SPACES
                DISPLAY 'MISSING CYCLE DATE PARM'
                GO TO ABEND-PGM
            END-IF.

       0000-DATE-CARD-READ. COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                 (END-OF-ELNAPS)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CLAIM RECORDS READ    '  NAPS-RECS-IN
           DISPLAY ' TRLR  RECORDS READ    '  TRL-RECS-IN
           DISPLAY ' CLAIM RECORDS WRITTEN '  CLM-RECS-OUT
041113     DISPLAY ' QWS LETTERS STOPPED   '  QWS-RECS-STOPPED
041813     DISPLAY ' QWS SPEC HNDL LETTERS '  QWS-SPEC-RECS
           GOBACK

           .
       0050-PROCESS-FILE.

           IF NA-INITIAL-PRINT-DT EQUAL LOW-VALUES OR SPACES 
              PERFORM 0100-PROCESS-ELNAPS THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-ELNAPS    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-ELNAPS.

           IF NA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD
               SET END-OF-ELNAPS TO TRUE
               GO TO 0100-EXIT
           END-IF
           
           MOVE NA-COMPANY-CD      TO AT-COMPANY-CD.
           MOVE NA-CARRIER         TO AT-CARRIER.
           MOVE NA-CLAIM-NO        TO AT-CLAIM-NO.
           MOVE NA-CERT-NO         TO AT-CERT-NO.
           MOVE NA-CORR-TRLR-SEQ   TO AT-SEQUENCE-NO.

           READ ELTRLR RECORD INVALID KEY
               GO TO 0100-EXIT
           END-READ.
041113
041113     IF (AT-LETTER-ANSWERED-DT NOT EQUAL LOW-VALUES AND SPACES) 
041113       OR (AT-STOP-LETTER-DT NOT EQUAL LOW-VALUES AND SPACES)
041113          IF NA-CREATED-IN-NAPERSOFT = 'Y'
041113              MOVE SPACES TO STOP-QWS-REC
041113              IF NA-CERT-SFX > SPACES
041113                  STRING NA-CLAIM-NO ' '
041113                         NA-CERT-NO ' '
041113                         NA-LETTER-ID ' 0'
041113                         NA-ARCHIVE-NO
041113                  INTO STOP-QWS-REC
041113              ELSE
041113                  STRING NA-CLAIM-NO ' '
041113                         NA-CERT-PRIME ' '
041113                         NA-LETTER-ID ' 0'
041113                         NA-ARCHIVE-NO
041113                  INTO STOP-QWS-REC
041113              END-IF
041113              WRITE STOP-QWS-RECORD FROM STOP-QWS-REC
041113              ADD +1  TO QWS-RECS-STOPPED
041113          END-IF
041113     END-IF

           IF AT-LETTER-ANSWERED-DT IS NOT EQUAL TO LOW-VALUES
               GO TO 0100-EXIT
           END-IF.
102810
102810     IF AT-STOP-LETTER-DT NOT EQUAL LOW-VALUES AND SPACES
102810         GO TO 0100-EXIT
102810     END-IF.
           
           IF NA-LETTER-ID = SPACES
              GO TO 0100-EXIT
           END-IF.
           
           IF NA-LETTER-ID = 'HOHI' OR 'CI74'
              GO TO 0100-EXIT
           END-IF.
041813
041813     IF NA-CREATED-IN-NAPERSOFT = 'Y'  AND
041813        (NA-ENCLOSURE-CD (2:1) = 'X' OR
041813         NA-ENCLOSURE-CD (3:1) = 'X')
041813            MOVE SPACES TO QWS-SPEC-HNDL-REC
041813            IF NA-CERT-SFX > SPACES
041813                  STRING NA-CLAIM-NO ' '
041813                         NA-CERT-NO ' '
041813                         NA-LETTER-ID ' 0'
041813                         NA-ARCHIVE-NO
041813                  INTO QWS-SPEC-HNDL-REC
041813            ELSE
041813                  STRING NA-CLAIM-NO ' '
041813                         NA-CERT-PRIME ' '
041813                         NA-LETTER-ID ' 0'
041813                         NA-ARCHIVE-NO
041813                  INTO QWS-SPEC-HNDL-REC
041813            END-IF
041813            WRITE QWS-SPEC-HNDL-RECORD FROM QWS-SPEC-HNDL-REC
041813            ADD +1  TO QWS-SPEC-RECS
041813     END-IF

           MOVE WS-CURRENT-BIN-DT   TO NA-INITIAL-PRINT-DT
                                       AT-INITIAL-PRINT-DATE.
                                       
           REWRITE NAPERSOFT-FILE
                INVALID KEY
                    DISPLAY ' CIDNAPEX REWRITE ERROR - ELNAPS'
                    SET END-OF-ELNAPS TO TRUE
                    GO TO ABEND-PGM.

           REWRITE ACTIVITY-TRAILERS
                INVALID KEY
                    DISPLAY ' CIDNAPEX REWRITE ERROR - ELTRLR'
                    SET END-OF-ELNAPS TO TRUE
                    GO TO ABEND-PGM.
031116     ADD 1 TO TRL-RECS-IN

           IF NA-CREATED-IN-NAPERSOFT = 'Y'
               GO TO 0100-EXIT
           END-IF.                                       
       
           PERFORM 0205-READ-ELMSTR THRU 0205-EXIT
           PERFORM 0275-READ-ELCERT THRU 0275-EXIT
           PERFORM 0210-GET-ELTRLRS THRU 0210-EXIT

           MOVE WS-SAVE-NAPER         TO NAPER-DETAIL-RECORD

           MOVE PARM-CYCLE-DATE        TO EX-CYCLE-DATE
           MOVE NA-LETTER-ID           TO EX-LETTER-ID
           MOVE NA-CARRIER             TO EX-CARRIER
           MOVE NA-CLAIM-NO            TO EX-CLAIM-NO
071719     MOVE FUNCTION REVERSE(NA-CLAIM-NO(3:5)) TO EX-VER-CD
           MOVE NA-CERT-NO             TO EX-CERT-NO
           MOVE NA-ARCHIVE-NO          TO EX-ARCHIVE-NO
102810     MOVE NA-PROCESSOR-ID        TO EX-PROCESSOR-ID
102810     MOVE DTE-CLIENT             TO EX-COMPANY-ID
           MOVE CL-CERT-ACCOUNT        TO EX-ACCOUNT
           MOVE CL-INSURED-LAST-NAME   TO EX-INSURED-LAST-NAME
           MOVE CL-INSURED-1ST-NAME    TO EX-INSURED-1ST-NAME
           MOVE CL-CLAIM-STATUS        TO EX-CLAIM-STATUS
           MOVE CL-CLAIM-TYPE          TO EX-CLAIM-TYPE
           MOVE CL-TOTAL-PAID-AMT      TO EX-TOTAL-PAID-AMT
           MOVE CL-NO-OF-PMTS-MADE     TO EX-NO-OF-PMTS
           MOVE CL-LAST-PMT-AMT        TO EX-LAST-PAID-AMT
           MOVE CL-INSURED-SEX-CD      TO EX-INSURED-SEX-CD
           MOVE CL-ACCOUNT-ADDR-CNT    TO EX-ACCOUNT-ADDR-CNT
           MOVE CL-CERT-STATE          TO EX-CERT-STATE
                                          EX-ENC-STATE
           MOVE CL-CERT-ACCOUNT        TO EX-CERT-ACCOUNT
           MOVE CL-LAST-MAINT-USER     TO EX-LAST-MAINT-USER
           MOVE CL-LAST-MAINT-TYPE     TO EX-LAST-MAINT-TYPE
           MOVE CL-LAST-CLOSE-REASON   TO EX-LAST-CLOSE-REASON
           MOVE CL-SOC-SEC-NO          TO EX-SSN
031912     MOVE CL-CCN (1:9)           TO EX-AHL-CLAIM-NO

           MOVE CL-FILE-ESTABLISH-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-FILE-ESTABLISH-DT
           END-IF

           MOVE CL-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-CERT-EFF-DT
           END-IF

           MOVE CL-PAID-THRU-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-PAID-THRU-DT
           END-IF

           MOVE CL-LAST-PMT-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-PMT-DT
           END-IF

           MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-INCURRED-DT
           END-IF

           MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-REPORTED-DT
           END-IF

           MOVE CL-LAST-MAINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-MAINT-DT
           END-IF

           MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-BIRTH-DT
           END-IF

           MOVE NA-1ST-LTR-PRINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-1ST-LTR-PRINT-DT
           END-IF

           MOVE NA-NEXT-DUE-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-NEXT-DUE-DT
           END-IF

           MOVE NA-AUTOPYDT            TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-AUTOPYDT
           END-IF

           INSPECT WS-DIAGNOSIS
              REPLACING ALL X'00' BY SPACES

           MOVE WS-DIAGNOSIS           TO EX-DIAG

           MOVE WS-LAST-ACT-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-ACT-DT
           END-IF

           MOVE WS-ACTIVITY-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-ACTIVITY-DT
           END-IF

           MOVE WS-AUTO-LAST-SCHED-DT  TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-AUTO-LAST-SCHED-DT
           END-IF

           MOVE WS-LAST-ACT-TYPE       TO EX-LAST-ACT-TYPE
           MOVE WS-ACTIVITY-TYPE       TO EX-ACTIVITY-TYPE
           MOVE WS-FORM                TO EX-FORM

           IF WS-ACCT-NAME-AND-ADDRESS NOT = SPACES
              MOVE WS-ORIG-ACCT-NAME      TO EX-ORIG-ACCT-NAME
              MOVE WS-ACCT-NAME           TO EX-ACCT-NAME
              MOVE WS-ACCT-ADDR1          TO EX-ACCT-ADDR1
              MOVE WS-ACCT-ADDR2          TO EX-ACCT-ADDR2
              MOVE WS-ACCT-CITY           TO EX-ACCT-CITY
              MOVE WS-ACCT-STATE          TO EX-ACCT-STATE
              MOVE WS-ACCT-ZIP            TO EX-ACCT-ZIP
              MOVE WS-ACCT-PHONE          TO EX-ACCT-PHONE
012313        MOVE WS-ACCT-GPCD           TO EX-ACCT-GPCD
           END-IF
           
           IF WS-LOAN-NO NOT = SPACES
              MOVE WS-LOAN-NO      TO EX-LOAN-NO
           END-IF

           IF WS-INS-NAME-AND-ADDRESS NOT = SPACES
              MOVE WS-INS-NAME         TO EX-INS-NAME
              MOVE WS-INS-ADDR1        TO EX-INS-ADDR1
              MOVE WS-INS-ADDR2        TO EX-INS-ADDR2
              MOVE WS-INS-CITY         TO EX-INS-CITY
              MOVE WS-INS-STATE        TO EX-INS-STATE
              MOVE WS-INS-ZIP          TO EX-INS-ZIP
              MOVE WS-INS-PHONE        TO EX-INS-PHONE
           END-IF

           IF WS-BEN-NAME-AND-ADDRESS NOT = SPACES
              MOVE WS-BEN-NAME         TO EX-BEN-NAME
              MOVE WS-BEN-ADDR1        TO EX-BEN-ADDR1
              MOVE WS-BEN-ADDR2        TO EX-BEN-ADDR2
              MOVE WS-BEN-CITY         TO EX-BEN-CITY
              MOVE WS-BEN-STATE        TO EX-BEN-STATE
              MOVE WS-BEN-ZIP          TO EX-BEN-ZIP
              MOVE WS-BEN-PHONE        TO EX-BEN-PHONE
           ELSE
              IF CL-BENEFICIARY NOT = SPACES
                 MOVE CL-COMPANY-CD    TO BE-COMPANY-CD
                 MOVE 'B'              TO BE-RECORD-TYPE
                 MOVE CL-BENEFICIARY   TO BE-BENEFICIARY
                 READ ELBENE
                 IF ELBENE-FILE-STATUS = '00'
                    MOVE BE-MAIL-TO-NAME TO EX-BEN-NAME
                    MOVE BE-ADDRESS-LINE-1 TO EX-BEN-ADDR1
                    MOVE BE-ADDRESS-LINE-2 TO EX-BEN-ADDR2
                    MOVE BE-CITY       TO EX-BEN-CITY
                    MOVE BE-STATE      TO EX-BEN-STATE
                    MOVE BE-ZIP-CODE   TO EX-BEN-ZIP
                    MOVE BE-PHONE-NO   TO EX-BEN-PHONE
                 ELSE
                    DISPLAY ' ELBENE NOT FOUND ' CL-BENEFICIARY
                    ' ' ELBENE-FILE-STATUS
                 END-IF
              END-IF
           END-IF


           IF ELCERT-FILE-STATUS = '00'
              IF CL-INSURED-1ST-NAME = CM-INSURED-FIRST-NAME
                  MOVE CM-JT-FIRST-NAME TO EX-JT-BORROWER-1ST-NAME
                  MOVE CM-JT-LAST-NAME  TO EX-JT-BORROWER-LAST-NAME
              ELSE
                  MOVE CM-INSURED-FIRST-NAME TO EX-JT-BORROWER-1ST-NAME
                  MOVE CM-INSURED-LAST-NAME TO EX-JT-BORROWER-LAST-NAME
              END-IF
              IF CL-CLAIM-TYPE = 'L' OR 'P'
                 MOVE CM-LF-LOAN-EXPIRE-DT
                                       TO DC-BIN-DATE-1
                 MOVE CM-LF-ORIG-TERM  TO EX-TERM
                 MOVE CM-LF-BENEFIT-CD TO EX-BEN-CODE
                 MOVE CM-LF-BENEFIT-AMT TO EX-BENEFIT-AMT
                 MOVE CM-LF-CRITICAL-PERIOD
                                       TO EX-CRIT-PERIOD
                 MOVE CM-LOAN-APR      TO EX-APR
              ELSE
                 MOVE CM-AH-LOAN-EXPIRE-DT 
                                       TO DC-BIN-DATE-1
                 MOVE CM-AH-ORIG-TERM  TO EX-TERM
                 MOVE CM-AH-BENEFIT-CD TO EX-BEN-CODE
                 MOVE CM-AH-BENEFIT-AMT TO EX-BENEFIT-AMT
                 MOVE CM-AH-CRITICAL-PERIOD
                                       TO EX-CRIT-PERIOD
                 PERFORM VARYING CLAS-INDEXA
                    FROM CLAS-STARTA BY +1 UNTIL
                    (CLAS-I-BEN (CLAS-INDEXA) = EX-BEN-CODE)
                    OR (CLAS-INDEXA > CLAS-MAXA)
                 END-PERFORM
                 IF CLAS-INDEXA > CLAS-MAXA
                    DISPLAY ' ERROR AH BEN CODE ' EX-BEN-CODE ' '
                      CL-CLAIM-NO ' ' CL-CARRIER ' ' CL-CERT-NO
                 ELSE
                    MOVE CLAS-EXCLUSION (CLAS-INDEXA)
                                       TO EX-WAIT-PERIOD
                 END-IF
              END-IF
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-EXP-DT
              END-IF
           END-IF
           
           MOVE WS-INT-RATE            TO EX-LF-INTEREST-RATE
           IF NA-ENCLOSURE-CD EQUAL LOW-VALUES
               MOVE SPACES             TO EX-ENCLOSURE-CD
           ELSE
               MOVE NA-ENCLOSURE-CD    TO EX-ENCLOSURE-CD
           END-IF
           
           IF NA-ORIG-ARCHIVE-NO EQUAL LOW-VALUES
               MOVE ZEROS              TO EX-ORIG-ARCHIVE-NO
           ELSE
               MOVE NA-ORIG-ARCHIVE-NO TO EX-ORIG-ARCHIVE-NO
           END-IF
           
           IF NA-RESEND-PROMPT-IND EQUAL LOW-VALUES
               MOVE SPACES             TO EX-RESEND-PROMPT-IND
           ELSE
               MOVE NA-RESEND-PROMPT-IND TO EX-RESEND-PROMPT-IND
           END-IF

031116     PERFORM 1800-SET-EL150D-FIELDS THRU 1800-EXIT

           PERFORM 0300-WRITE-MSTR     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ELNAPS.

           READ ELNAPS NEXT RECORD

           IF (ELNAPS-FILE-STATUS = '10' OR '23')
              OR (NA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ELNAPS        TO TRUE
           ELSE
              IF ELNAPS-FILE-STATUS NOT = '00'
                 DISPLAY 'ELNAPS READ NEXT ' ELNAPS-FILE-STATUS
                 SET END-OF-ELNAPS     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELNAPS
              ADD 1 TO NAPS-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0205-READ-ELMSTR.

           MOVE LOW-VALUES      TO  CLAIM-MASTER
           MOVE NA-COMPANY-CD   TO  CL-COMPANY-CD
           MOVE NA-CARRIER      TO  CL-CARRIER 
           MOVE NA-CLAIM-NO     TO  CL-CLAIM-NO
           MOVE NA-CERT-NO      TO  CL-CERT-NO
           
           READ ELMSTR

           IF ELMSTR-FILE-STATUS = '10' OR '23'
              DISPLAY ' CLAIM NOT FOUND ' NA-CLAIM-NO
                 ' ' NA-CERT-NO 
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELMSTR -ERROR - READ ' ELMSTR-FILE-STATUS
              ELSE
                 ADD 1 TO CLM-RECS-IN   
              END-IF
           END-IF

           .
       0205-EXIT.
           EXIT.


       0210-GET-ELTRLRS.

           MOVE SPACES                 TO WS-DIAGNOSIS
                                          WS-LAST-ACT-TYPE
                                          WS-ACTIVITY-TYPE
                                          WS-FORM
                                          WS-LETTER-ID
                                          WS-GOT-IT-SW
                                          WS-LOAN-NO
                                          WS-LETTER-ID
           MOVE LOW-VALUES             TO WS-LAST-ACT-DT
                                          WS-ACTIVITY-DT
                                          WS-AUTO-LAST-SCHED-DT
           MOVE ZEROS                  TO WS-INT-RATE

           PERFORM 0215-GET-INS-ADDR-TRLR
                                       THRU 0215-EXIT

           PERFORM 0216-GET-BEN-ADDR-TRLR
                                       THRU 0216-EXIT

           PERFORM 0220-START-ELTRLR   THRU 0220-EXIT
           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              PERFORM 0230-READ-ELTRLR THRU 0230-EXIT UNTIL
                 WE-GOT-IT-ALL
           END-IF

           .
       0210-EXIT.
           EXIT.

       0215-GET-INS-ADDR-TRLR.

           MOVE SPACES                 TO WS-INS-NAME-AND-ADDRESS
           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +1                     TO AT-SEQUENCE-NO

           READ ELTRLR

           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              IF AT-SEQUENCE-NO = +1
                 MOVE AT-MAIL-TO-NAME  TO WS-INS-NAME
                 MOVE AT-ADDRESS-LINE-1
                                       TO WS-INS-ADDR1
                 MOVE AT-ADDRESS-LINE-2
                                       TO WS-INS-ADDR2
                 MOVE AT-CITY          TO WS-INS-CITY
                 MOVE AT-STATE         TO WS-INS-STATE
                 MOVE AT-ZIP           TO WS-INS-ZIP
                 IF AT-PHONE-NO NOT = ZEROS
                    MOVE AT-PHONE-NO   TO WS-WORK-PHONE
                    STRING '(' WS-WORK-PHONE (2:3) ')'
                       WS-WORK-PHONE (5:3) '-' WS-WORK-PHONE (8:4)
                       DELIMITED BY SIZE INTO WS-INS-PHONE
                    END-STRING
                 END-IF
              END-IF
           END-IF

           .
       0215-EXIT.
           EXIT.

       0216-GET-BEN-ADDR-TRLR.

           MOVE SPACES                 TO WS-BEN-NAME-AND-ADDRESS
           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +11                    TO AT-SEQUENCE-NO

           READ ELTRLR

           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              IF AT-SEQUENCE-NO = +11
                 MOVE AT-MAIL-TO-NAME  TO WS-BEN-NAME
                 MOVE AT-ADDRESS-LINE-1
                                       TO WS-BEN-ADDR1
                 MOVE AT-ADDRESS-LINE-2
                                       TO WS-BEN-ADDR2
                 MOVE AT-CITY          TO WS-BEN-CITY
                 MOVE AT-STATE         TO WS-BEN-STATE
                 MOVE AT-ZIP           TO WS-BEN-ZIP
                 IF AT-PHONE-NO NOT = ZEROS
                    MOVE AT-PHONE-NO   TO WS-WORK-PHONE
                    STRING '(' WS-WORK-PHONE (2:3) ')'
                       WS-WORK-PHONE (5:3) '-' WS-WORK-PHONE (8:4)
                       DELIMITED BY SIZE INTO WS-BEN-PHONE
                    END-STRING
                 END-IF
              END-IF
           END-IF

           .
       0216-EXIT.
           EXIT.

       0220-START-ELTRLR.

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +90                    TO AT-SEQUENCE-NO
           START ELTRLR KEY >= AT-CONTROL-PRIMARY
           IF ELTRLR-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ELTRLR - START ' ELTRLR-FILE-STATUS
                 ' ' CL-CONTROL-PRIMARY (2:19)
           END-IF

           .
       0220-EXIT.
           EXIT.

           
       0230-READ-ELTRLR.

           READ ELTRLR NEXT RECORD

           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              EVALUATE TRUE
                 WHEN AT-SEQUENCE-NO = +90
                    MOVE AT-INFO-LINE-1   TO WS-DIAGNOSIS
                 WHEN AT-SEQUENCE-NO = +91
                    MOVE AT-INFO-LINE-1 TO WS-LOAN-NO
                 WHEN AT-TRAILER-TYPE = '2' AND AT-PAYMENT-TYPE = 'I'
                    IF AT-INT-RATE NUMERIC AND
                       AT-INT-RATE NOT EQUAL ZEROS
                          MOVE AT-INT-RATE TO WS-INT-RATE
                    END-IF
                 WHEN AT-TRAILER-TYPE = '3'
                    IF WS-CURRENT-BIN-DT NOT LESS THAN 
                             AT-SCHEDULE-START-DT  AND
                       WS-CURRENT-BIN-DT NOT GREATER THAN
                             AT-SCHEDULE-END-DT  AND
                      (AT-TERMINATED-DT EQUAL LOW-VALUES OR SPACES)
                         MOVE AT-SCHEDULE-END-DT TO 
                                      WS-AUTO-LAST-SCHED-DT
                    END-IF
                 WHEN AT-TRAILER-TYPE = '8'
                    IF AT-RECORDED-DT > WS-LAST-ACT-DT
                       MOVE AT-RECORDED-DT
                                    TO WS-LAST-ACT-DT
                       MOVE 'DENIAL'
                                    TO WS-LAST-ACT-TYPE
                    END-IF
                 WHEN AT-TRAILER-TYPE = 'A'
                    IF AT-RECORDED-DT > WS-LAST-ACT-DT
                       MOVE AT-RECORDED-DT
                                    TO WS-LAST-ACT-DT
                       MOVE 'FORMS' TO WS-LAST-ACT-TYPE
                       IF INITIAL-FORM
                          MOVE 'INIT'
                                    TO WS-FORM
                       END-IF
                       IF PROGRESS-FORM
                          MOVE 'PROG'
                                    TO WS-FORM
                       END-IF
                    END-IF
                    IF AT-FORM-SEND-ON-DT > WS-ACTIVITY-DT
                       MOVE AT-FORM-SEND-ON-DT
                                    TO WS-ACTIVITY-DT
                    END-IF
                    IF AT-FORM-REPRINT-DT > WS-ACTIVITY-DT
                       MOVE AT-FORM-REPRINT-DT
                                    TO WS-ACTIVITY-DT
                    END-IF
                    IF AT-FORM-ANSWERED-DT > WS-ACTIVITY-DT
                       MOVE AT-FORM-ANSWERED-DT
                                    TO WS-ACTIVITY-DT
                    END-IF
              END-EVALUATE
           ELSE
              SET WE-GOT-IT-ALL        TO TRUE
           END-IF

           .
       0230-EXIT.
           EXIT.


       0275-READ-ELCERT.

           MOVE CL-CERT-KEY-DATA       TO CM-CONTROL-PRIMARY (2:21)
           MOVE DTE-CLASIC-COMPANY-CD  TO CM-COMPANY-CD
           MOVE CL-CERT-NO             TO CM-CERT-NO
           
           READ ELCERT

           IF ELCERT-FILE-STATUS = '10' OR '23'
              DISPLAY ' CERT NOT FOUND ' CL-CERT-KEY-DATA (1:19)
                 ' ' CL-CERT-NO ' ' CL-CLAIM-NO
           ELSE
              IF ELCERT-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCERT -ERROR - READ ' ELCERT-FILE-STATUS
              ELSE
                 PERFORM 0280-GET-ERACCT
                                       THRU 0280-EXIT
                 ADD 1 TO CRT-RECS-IN   
              END-IF
           END-IF

           .
       0275-EXIT.
           EXIT.

       0280-GET-ERACCT.

           MOVE SPACES                 TO WS-ACCT-NAME-AND-ADDRESS
           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE CM-CONTROL-PRIMARY (1:20)
                                       TO AM-CONTROL-PRIMARY (1:20)
           MOVE CM-CERT-EFF-DT         TO AM-EXPIRATION-DT
           SET NOT-DONE-WITH-ACCT      TO TRUE
           START ERACCT KEY >= AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS = '00'
              READ ERACCT NEXT RECORD
              IF ERACCT-FILE-STATUS = '00'
                 IF CM-CONTROL-PRIMARY (1:20) =
                    AM-CONTROL-PRIMARY (1:20)
                    MOVE AM-NAME       TO WS-ORIG-ACCT-NAME
                    PERFORM UNTIL DONE-WITH-ACCT
                        MOVE AM-NAME   TO WS-ACCT-NAME
                        MOVE AM-ADDRS  TO WS-ACCT-ADDR1
                        MOVE SPACES    TO WS-ACCT-ADDR2
                        MOVE AM-ADDR-CITY  TO WS-ACCT-CITY
                        MOVE AM-ADDR-STATE  TO WS-ACCT-STATE
                        MOVE AM-ZIP    TO WS-ACCT-ZIP
                        MOVE AM-TEL-NO TO WS-ACCT-PHONE
012313                  MOVE AM-GPCD   TO WS-ACCT-GPCD
                        READ ERACCT NEXT RECORD
                        IF ERACCT-FILE-STATUS = '00'
                           IF CM-CONTROL-PRIMARY (1:20) <>
                              AM-CONTROL-PRIMARY (1:20)
                                 SET DONE-WITH-ACCT TO TRUE
                           END-IF
                        END-IF
                    END-PERFORM
                 END-IF
              END-IF
           END-IF

           .
       0280-EXIT.
           EXIT.

       0300-WRITE-MSTR.

           INSPECT NAPER-DETAIL-RECORD REPLACING ALL ';' BY ' '
           INSPECT NAPER-DETAIL-RECORD REPLACING ALL X'A2' BY ';'
           WRITE NAPER-OUT-REC        FROM NAPER-DETAIL-RECORD
           ADD 1 TO CLM-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN I-O ELNAPS ELTRLR 
031116          INPUT ELMSTR ELCERT ERACCT ELBENE ELCRTT ERPDEF
041813          OUTPUT NAPER-OUT STOP-QWS QWS-SPEC-HNDL

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELNAPS ELMSTR NAPER-OUT ELTRLR ELCERT ERACCT ELBENE
041813           STOP-QWS QWS-SPEC-HNDL
031116           ELCRTT ERPDEF
           .
       0500-EXIT.
           EXIT.

       0550-START-ELNAPS.

           MOVE LOW-VALUES             TO NA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO NA-COMPANY-CD

           START ELNAPS KEY IS NOT < NA-CONTROL-PRIMARY

           IF ELNAPS-FILE-STATUS = '10' OR '23'
              SET END-OF-ELNAPS        TO TRUE
           ELSE
              IF ELNAPS-FILE-STATUS NOT = '00'
                 DISPLAY 'ELNAPS START     ' ELNAPS-FILE-STATUS
                 SET END-OF-ELNAPS     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           INITIALIZE NAPER-DETAIL-RECORD
           MOVE X'A2'                  TO EX-TAB00
                                          EX-TAB01
                                          EX-TAB02
                                          EX-TAB03
                                          EX-TAB04
                                          EX-TAB05
                                          EX-TAB06
                                          EX-TAB07
                                          EX-TAB08
                                          EX-TAB09
                                          EX-TAB10
                                          EX-TAB11
                                          EX-TAB12
                                          EX-TAB13
                                          EX-TAB14
                                          EX-TAB15
                                          EX-TAB16
                                          EX-TAB17
                                          EX-TAB18
                                          EX-TAB19
                                          EX-TAB20
                                          EX-TAB21
                                          EX-TAB22
                                          EX-TAB23
                                          EX-TAB24
                                          EX-TAB25
                                          EX-TAB26
                                          EX-TAB27
                                          EX-TAB28
                                          EX-TAB29
                                          EX-TAB30
                                          EX-TAB31
                                          EX-TAB32
                                          EX-TAB33
                                          EX-TAB34
                                          EX-TAB35
                                          EX-TAB36
                                          EX-TAB37
                                          EX-TAB38
                                          EX-TAB39
                                          EX-TAB40
                                          EX-TAB41
                                          EX-TAB42
                                          EX-TAB43
                                          EX-TAB44
                                          EX-TAB45
                                          EX-TAB46
                                          EX-TAB47
                                          EX-TAB48
                                          EX-TAB49
                                          EX-TAB50
                                          EX-TAB51
                                          EX-TAB52
                                          EX-TAB53
                                          EX-TAB54
                                          EX-TAB55
                                          EX-TAB56
                                          EX-TAB57
                                          EX-TAB58
                                          EX-TAB59
                                          EX-TAB60
                                          EX-TAB61
                                          EX-TAB62
                                          EX-TAB63
                                          EX-TAB64
                                          EX-TAB65
                                          EX-TAB66
                                          EX-TAB67
                                          EX-TAB68
                                          EX-TAB69
                                          EX-TAB70
                                          EX-TAB71
                                          EX-TAB72
                                          EX-TAB73
                                          EX-TAB74
                                          EX-TAB75
                                          EX-TAB76
102810                                    EX-TAB77
102810                                    EX-TAB78
031612                                    EX-TAB79
012313                                    EX-TAB80
031116                                    EX-TAB81
031116                                    EX-TAB82
031116                                    EX-TAB83
031116                                    EX-TAB84
031116                                    EX-TAB85
031116                                    EX-TAB86
031116                                    EX-TAB87
031116                                    EX-TAB88
071719                                    EX-TAB89
           MOVE 'E'                    TO EX-LAST-BYTE

           MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.
           MOVE '2'                    TO  DC-OPTION-CODE.
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.
           DISPLAY 'CURRENT DATE USED FOR RUN IS - - ' WS-CURRENT-DATE.
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.

           PERFORM 8510-DATE-CONVERSION.
           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1      TO  WS-CURRENT-BIN-DT
           END-IF

           MOVE NAPER-DETAIL-RECORD   TO WS-SAVE-NAPER
           PERFORM 0550-START-ELNAPS   THRU 0550-EXIT
           PERFORM 0200-READ-ELNAPS    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

031116 1800-SET-EL150D-FIELDS.
031116     MOVE SPACES TO WS-UNSORTED-TABLE
031116                    WS-SORTED-TABLE
031116
031116     MOVE ' '                    TO WS-PDEF-RECORD-SW
031116
031116     MOVE CM-CONTROL-PRIMARY     TO CS-CONTROL-PRIMARY
031116     MOVE 'B'                    TO CS-TRAILER-TYPE
031116
031116     READ ELCRTT
031116
031116     IF ELCRTT-FILE-STATUS = '10' OR '23'
031116        DISPLAY ' NO TRLRS ' CL-CERT-NO
031116        GO TO 1800-EXIT
031116     ELSE
031116        IF ELCRTT-FILE-STATUS NOT = '00'
031116           DISPLAY 'ELCRTT -ERROR - READ ' ELCRTT-FILE-STATUS
031116        END-IF
031116     END-IF
031116
031116     PERFORM VARYING S1 FROM +1 BY +1 UNTIL
031116       (S1 > +24)
031116       OR (CS-CLAIM-NO (S1) = SPACES)
031116        MOVE CS-CLAIM-NO (S1)    TO WS-CLAIM-NO (S1)
031116        MOVE CS-CLAIM-TYPE (S1)  TO WS-CLM-TYPE (S1)
031116        IF CS-INSURED-TYPE (S1) = 'C'
031116           MOVE '2'              TO WS-INS-TYPE (S1)
031116        ELSE
031116           MOVE '1'              TO WS-INS-TYPE (S1)
031116        END-IF
031116        MOVE CS-BENEFIT-PERIOD (S1)
031116                                 TO WS-BEN-PER (S1)
031116*       MOVE CS-DAYS-PAID (S1)   TO WS-DAYS-PAID (S1)
031116        MOVE CS-TOTAL-PAID (S1)  TO WS-TOTAL-PAID (S1)
031116        MOVE ' '                 TO WS-SORTED-SW (S1)
031116
031116        MOVE CS-COMPANY-CD          TO CL-COMPANY-CD
031116        MOVE CS-CARRIER             TO CL-CARRIER
031116        MOVE CS-CLAIM-NO (S1)       TO CL-CLAIM-NO
031116        MOVE CS-CERT-NO             TO CL-CERT-NO
031116
031116        MOVE 'N' TO WS-CLAIM-READ
031116
031116        READ ELMSTR
031116
031116        IF ELMSTR-FILE-STATUS = '10' OR '23'
031116           DISPLAY ' CLAIM NOT FOUND ' CL-CLAIM-NO
031116              ' ' CL-CERT-NO
031116        ELSE
031116           IF ELMSTR-FILE-STATUS NOT = '00'
031116              DISPLAY 'ELMSTR -ERROR - READ ' ELMSTR-FILE-STATUS
031116           ELSE
031116              SET GOOD-CLAIM-READ TO TRUE
031116           END-IF
031116        END-IF
031116
031116        IF GOOD-CLAIM-READ
031116           MOVE CL-INCURRED-DT   TO WS-INC-DT (S1)
031116           MOVE CL-PAID-THRU-DT  TO WS-PD-THRU-DT (S1)
031116           MOVE CL-CLAIM-STATUS  TO WS-STATUS (S1)
031116           IF CL-CRITICAL-PERIOD NOT NUMERIC
031116              MOVE ZEROS TO CL-CRITICAL-PERIOD
031116           END-IF
031116           MOVE CL-CRITICAL-PERIOD
031116                                 TO WS-MAX-BENS (S1)
031116           IF CL-CRITICAL-PERIOD = ZEROS
031116              IF CL-CLAIM-TYPE = 'L' OR 'P'
031116                 MOVE 01         TO WS-MAX-BENS (S1)
031116              ELSE
031116                 MOVE CM-AH-ORIG-TERM
031116                                 TO WS-MAX-BENS (S1)
031116              END-IF
031116           END-IF
031116           IF CL-DENIAL-TYPE = '1' OR '2' OR '3' OR '4'
031116              MOVE 'D'           TO WS-STATUS (S1)
031116           END-IF
031116           IF CS-CLAIM-NO (S1) = NA-CLAIM-NO
031116              MOVE CL-ACCIDENT-CLAIM-SW TO EX-ACC-SW
031116           END-IF
031116
031116           MOVE ' '                    TO WS-PDEF-RECORD-SW
031116           IF (AM-DCC-PRODUCT-CODE NOT = SPACES)
031116              PERFORM 1850-GET-DDF-LIMITS
031116                                 THRU 1850-EXIT
031116           END-IF
031116           IF PDEF-FOUND
031116              PERFORM VARYING P1 FROM +1 BY +1 UNTIL
031116                 (PD-PROD-CODE (P1) = CL-CLAIM-TYPE)
082322                 OR (P1 > +11)
031116              END-PERFORM
082322              IF P1 < +12
031116                 IF CL-CLAIM-TYPE = 'L' OR 'P'
031116                    IF (PD-MAX-AMT (P1) NOT = ZEROS)
031116                       AND (PD-MAX-AMT (P1) < CM-LF-BENEFIT-AMT)
031116                       MOVE PD-MAX-AMT (P1)
031116                                 TO WS-MAX-MOBEN (S1)
031116                    ELSE
031116                       MOVE CM-LF-BENEFIT-AMT
031116                                 TO WS-MAX-MOBEN (S1)
031116                    END-IF
031116                 ELSE
031116                    IF PD-BEN-PCT (P1) NOT NUMERIC
031116                       MOVE ZEROS   TO PD-BEN-PCT (P1)
031116                    END-IF
031116                    IF PD-BEN-PCT (P1) = ZEROS
031116                       MOVE +1      TO WS-WORK-BEN-PCT
031116                    ELSE
031116                       MOVE PD-BEN-PCT (P1)
031116                                    TO WS-WORK-BEN-PCT
031116                    END-IF
031116                    COMPUTE WS-MAX-MOBEN (S1) =
031116                       CM-AH-BENEFIT-AMT * WS-WORK-BEN-PCT
031116                    IF (PD-MAX-AMT (P1) NOT = ZEROS)
031116                       AND (PD-MAX-AMT (P1) < WS-MAX-MOBEN (S1))
031116                       MOVE PD-MAX-AMT (P1)
031116                                 TO WS-MAX-MOBEN (S1)
031116                    END-IF
031116                 END-IF
031116*                IF (PD-MAX-AMT (P1) NOT = ZEROS)
031116*                   AND (PD-MAX-AMT (P1) < CM-AH-BENEFIT-AMT)
031116*                   MOVE PD-MAX-AMT (P1) TO WS-MAX-MOBEN (S1)
031116*                ELSE
031116*                   MOVE CM-AH-BENEFIT-AMT TO WS-MAX-MOBEN (S1)
031116*                END-IF
031116                 MOVE PD-EXCLUSION-PERIOD-DAYS (P1)
031116                                 TO WS-EXCL-PER (S1)
031116                 MOVE PD-COVERAGE-ENDS-MOS (P1)
031116                                 TO WS-COV-ENDS (S1)
031116                 MOVE PD-ACCIDENT-ONLY-MOS (P1)
031116                                 TO WS-ACC-PER (S1)
031116                 MOVE PD-MAX-EXTENSION (P1)
031116                                 TO WS-MAX-EXTEN (S1)
031116                 EVALUATE TRUE
031116                    WHEN PD-RECURRING-YN (P1) = 'N'
031116                       MOVE 00   TO WS-REC-MOS (S1)
031116                    WHEN PD-RECURRING-YN (P1) = 'Y'
031116                       MOVE 99   TO WS-REC-MOS (S1)
031116                    WHEN PD-REC-CRIT-PERIOD (P1) NUMERIC
031116                       MOVE PD-REC-CRIT-PERIOD (P1)
031116                                 TO WS-REC-MOS (S1)
031116                    WHEN OTHER
031116                       MOVE ZEROS TO WS-REC-MOS (S1)
031116                 END-EVALUATE
031116                 IF WS-REC-MOS (S1) = ZEROS
031116                    MOVE 01      TO WS-REC-MOS (S1)
031116                 END-IF
031116              END-IF
031116           ELSE
031116              IF CL-CLAIM-TYPE NOT = 'L' AND 'P'
031116                 MOVE CM-AH-BENEFIT-AMT
031116                                 TO WS-MAX-MOBEN (S1)
031116              ELSE
031116                 MOVE ZEROS      TO WS-MAX-MOBEN (S1)
031116              END-IF
031116              MOVE ZEROS         TO WS-EXCL-PER (S1)
031116                                    WS-MAX-EXTEN (S1)
031116                                    WS-ACC-PER  (S1)
031116              MOVE 999           TO WS-COV-ENDS (S1)
031116              MOVE 01            TO WS-REC-MOS  (S1)
031116           END-IF
031116        END-IF
031116     END-PERFORM
031116
031116*      SORT THE TABLE
031116     MOVE HIGH-VALUES            TO WS-KEY (S1)
031116     COMPUTE WS-MAX-SUB = S1 - +1
031116
031116     MOVE WS-MAX-SUB             TO WS-CNTR
031116
031116     MOVE +1                     TO S2
031116     MOVE +0                     TO S3
031116     PERFORM UNTIL (WS-CNTR = ZERO) OR (S3 > 700)
031116        MOVE +0 TO WS-HOLD-S1
031116        MOVE HIGH-VALUES TO WS-HOLD-KEY
031116        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
031116           (S1 > WS-MAX-SUB)
031116           IF (WS-SORTED-SW (S1) NOT = 'Y')
031116              AND (WS-KEY (S1) <= WS-HOLD-KEY)
031116              MOVE WS-KEY (S1)   TO WS-HOLD-KEY
031116              MOVE S1            TO WS-HOLD-S1
031116           END-IF
031116        END-PERFORM
031116        IF WS-HOLD-S1 NOT = ZEROS
031116           MOVE WS-HOLD-S1         TO S1
031116           MOVE WS-KEY        (S1) TO WS-SRTD-KEY        (S2)
031116           MOVE WS-STATUS     (S1) TO WS-SRTD-STATUS     (S2)
031116           MOVE WS-MAX-BENS   (S1) TO WS-SRTD-MAX-BENS   (S2)
031116           MOVE WS-EXCL-PER   (S1) TO WS-SRTD-EXCL-PER   (S2)
031116           MOVE WS-COV-ENDS   (S1) TO WS-SRTD-COV-ENDS   (S2)
031116           MOVE WS-ACC-PER    (S1) TO WS-SRTD-ACC-PER    (S2)
031116           MOVE WS-REC-MOS    (S1) TO WS-SRTD-REC-MOS    (S2)
031116           MOVE WS-MAX-EXTEN  (S1) TO WS-SRTD-MAX-EXTEN  (S2)
031116           MOVE WS-PD-THRU-DT (S1) TO WS-SRTD-PD-THRU-DT (S2)
031116           MOVE WS-CLAIM-NO   (S1) TO WS-SRTD-CLAIM-NO   (S2)
031116           MOVE WS-MAX-MOBEN  (S1) TO WS-SRTD-MAX-MOBEN  (S2)
031116           MOVE WS-TOTAL-PAID (S1) TO WS-SRTD-TOTAL-PAID (S2)
031116*          MOVE WS-REM-BENS   (S1) TO WS-SRTD-REM-BENS   (S2)
031116           MOVE 'Y'                TO WS-SORTED-SW       (S1)
031116           SUBTRACT 1 FROM WS-CNTR
031116           ADD +1 TO S2
031116        END-IF
031116        ADD 1 TO S3
031116     END-PERFORM
031116
031116***  -----------------------
031116*    IF BL-COMP-ID NOT = 'DCC' AND 'VPP'
           IF NA-COMPANY-CD NOT = X'05' AND X'07'
031116        PERFORM 1900-BUILD-NON-DCC
031116        GO TO 1800-EXIT
031116     END-IF
031116
031116     MOVE SPACES                 TO WS-PREV-CLM-TYPE
031116                                    WS-PREV-INS-TYPE
031116     MOVE ZEROS                  TO WS-PREV-BEN-PER
031116                                    WS-ACCUM-DAYS
031116                                    WS-ACCUM-AMT
031116                                    WS-ACCUM-PD-BENS
031116
031116     MOVE +1                     TO M1
031116     PERFORM VARYING S1 FROM +1 BY +1 UNTIL
031116        S1 > WS-MAX-SUB
031116        IF (WS-SRTD-CLM-TYPE (S1) = WS-PREV-CLM-TYPE)
031116           AND (WS-SRTD-BEN-PER (S1) = WS-PREV-BEN-PER)
031116           AND (WS-SRTD-INS-TYPE (S1) NOT = WS-PREV-INS-TYPE)
031116           MOVE ZEROS            TO WS-ACCUM-DAYS
031116                                    WS-ACCUM-AMT
031116                                    WS-ACCUM-PD-BENS
031116        END-IF
031116        MOVE WS-SRTD-INS-TYPE (S1) TO WS-PREV-INS-TYPE
031116        IF (WS-SRTD-CLM-TYPE (S1) = WS-PREV-CLM-TYPE)
031116           AND (WS-SRTD-BEN-PER (S1) NOT = WS-PREV-BEN-PER)
031116           MOVE ZEROS            TO WS-ACCUM-DAYS
031116                                    WS-ACCUM-AMT
031116                                    WS-ACCUM-PD-BENS
031116        END-IF
031116        MOVE WS-SRTD-BEN-PER (S1) TO WS-PREV-BEN-PER
031116        IF WS-SRTD-CLM-TYPE (S1) NOT = WS-PREV-CLM-TYPE
031116           PERFORM 1810-SET-NEW-HEAD
031116                                 THRU 1810-EXIT
031116           MOVE WS-SRTD-CLM-TYPE (S1)
031116                                 TO WS-PREV-CLM-TYPE
031116           ADD +1                TO M1
031116        END-IF
031116
031116        MOVE WS-SRTD-BEN-PER (S1)  TO EX-BEN-PER
031116        IF WS-SRTD-INS-TYPE (S1) = '1'
031116           MOVE 'PRIM'             TO EX-INS-TYP
031116        ELSE
031116           MOVE 'COBO'             TO EX-INS-TYP
031116        END-IF
031116*       EVALUATE WS-SRTD-STATUS (S1)
031116*          WHEN 'C'
031116*             MOVE 'CLOSED'        TO WSM-STATUS (M1)
031116*          WHEN 'D'
031116*             MOVE 'DENIED'        TO WSM-STATUS (M1)
031116*          WHEN 'O'
031116*             MOVE 'OPEN'          TO WSM-STATUS (M1)
031116*          WHEN OTHER
031116*             MOVE 'OTHER'         TO WSM-STATUS (M1)
031116*       END-EVALUATE
031116*       MOVE WS-SRTD-CLAIM-NO (S1) TO WSM-CLAIM-NO (M1)
031116*                                     PI-WSM-CLAIM-NOS (S1)
031116*       MOVE WS-SRTD-TOTAL-PAID (S1) TO WSM-TOTAL-PAID (M1)
031116
031116        COMPUTE EX-MAX-BENS =
031116           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
031116
031116        MOVE ZEROS TO WK1 WK2
031116        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
031116           COMPUTE WS-PD-BENS ROUNDED =
031116              WS-SRTD-TOTAL-PAID (S1) / WS-SRTD-MAX-MOBEN (S1)
031116           IF WS-SRTD-CLM-TYPE (S1) NOT = 'L' AND 'P'
031116              DIVIDE WS-SRTD-TOTAL-PAID (S1) BY
031116                 WS-SRTD-MAX-MOBEN(S1) GIVING WK1
031116                 REMAINDER WK2
031116           END-IF
031116        ELSE
031116           MOVE ZEROS            TO WS-PD-BENS
031116        END-IF
031116        IF (WS-PD-BENS = ZEROS)
031116           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
031116           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
031116           MOVE 1                TO WS-PD-BENS
031116        END-IF
031116        MOVE WS-PD-BENS          TO EX-PAID-BENS
031116*       if wk2 not = zeros
031116*          move '*'              to wsm-part (m1)
031116*       end-if
031116*       compute ws-accum-days =
031116*          ws-accum-days + ws-srtd-days-paid (s1)
031116        COMPUTE WS-ACCUM-AMT =
031116           WS-ACCUM-AMT + WS-SRTD-TOTAL-PAID (S1)
031116        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
031116           COMPUTE WS-ACCUM-PD-BENS ROUNDED =
031116              WS-ACCUM-AMT / WS-SRTD-MAX-MOBEN (S1)
031116        ELSE
031116           MOVE ZEROS            TO WS-ACCUM-PD-BENS
031116        END-IF
031116        IF (WS-ACCUM-PD-BENS = ZEROS)
031116           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
031116           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
031116           MOVE 1                TO WS-ACCUM-PD-BENS
031116        END-IF
031116        COMPUTE EX-REM-BENS =
031116           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
031116        PERFORM 1820-FIND-QUALIFY THRU 1820-EXIT
031116
031116*       MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-1
031116*       MOVE ' '                    TO DC-OPTION-CODE
031116*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
031116*       IF NO-CONVERSION-ERROR
031116*          MOVE DC-GREG-DATE-A-EDIT TO wsm-inc-date (m1)
031116*       ELSE
031116*          MOVE SPACES              TO wsm-inc-date (m1)
031116*       END-IF
031116*       MOVE ws-srtd-pd-thru-dt (s1) TO DC-BIN-DATE-1
031116*       MOVE ' '                     TO DC-OPTION-CODE
031116*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
031116*       IF NO-CONVERSION-ERROR
031116*          MOVE DC-GREG-DATE-A-EDIT TO wsm-pd-thru-dt (m1)
031116*       ELSE
031116*          MOVE SPACES              TO wsm-pd-thru-dt (m1)
031116*       END-IF
031116        ADD +1                   TO M1
031116        IF WS-SRTD-CLAIM-NO (S1) = NA-CLAIM-NO
031116           MOVE WS-SRTD-EXCL-PER (S1) TO EX-EXCL-PER
031116           MOVE 26 TO S1
031116        END-IF
031116
031116     END-PERFORM
031116
031116     .
031116 1800-EXIT.
031116     EXIT.
031116
031116     EJECT
031116
031116 1810-SET-NEW-HEAD.
031116     MOVE ZEROS                  TO WS-ACCUM-DAYS
031116                                    WS-ACCUM-AMT
031116                                    WS-ACCUM-PD-BENS
031116     EVALUATE WS-SRTD-CLM-TYPE (S1)
031116        WHEN 'A'
031116           MOVE 'A&H'            TO WS-COV-TYPE
031116        WHEN 'F'
031116           MOVE 'FAM '           TO WS-COV-TYPE
031116        WHEN 'I'
031116           MOVE ' IU '           TO WS-COV-TYPE
031116        WHEN 'L'
031116           MOVE 'LIFE'           TO WS-COV-TYPE
031116        WHEN OTHER
031116           MOVE WS-SRTD-CLM-TYPE (S1)
031116                                 TO WS-COV-TYPE
031116     END-EVALUATE
031116
031116     MOVE WS-SRTD-REC-MOS (S1) TO EX-REC-MOS
031116     MOVE WS-SRTD-EXCL-PER (S1) TO EX-EXCL-PER
031116
031116*    string '  ' ws-cov-type ' ExPer ' EX-EXCL-PER
031116*       ' CovEnd ' ws-srtd-cov-ends (s1)
031116*       ' MaxBens ' ws-srtd-max-bens (s1)
031116*       ' Recurring ' EX-REC-MOS
031116*       delimited by size into ws-map-output (m1)
031116*    end-string
031116
031116     .
031116 1810-EXIT.
031116     EXIT.
031116
031116 1820-FIND-QUALIFY.
031116     IF NOT PDEF-FOUND
031116        GO TO 1820-EXIT
031116     END-IF
031116
031116     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
031116     MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-2
031116     MOVE '1'                    TO DC-OPTION-CODE
031116     MOVE +0                     TO DC-ELAPSED-MONTHS
031116                                    DC-ELAPSED-DAYS
031116     PERFORM 8510-DATE-CONVERSION
031116                                 THRU 8590-EXIT
031116
031116     IF NO-CONVERSION-ERROR
031116        MOVE DC-ELAPSED-MONTHS   TO WS-MONTHS-BETWEEN
031116        IF DC-ELAPSED-DAYS > 1
031116           ADD 1 TO WS-MONTHS-BETWEEN
031116        END-IF
031116     ELSE
031116        MOVE ZEROS               TO WS-MONTHS-BETWEEN
031116     END-IF
031116
031116     EVALUATE TRUE
031116        WHEN (EX-EXCL-PER NOT = ZEROS)
031116           AND (WS-MONTHS-BETWEEN <= EX-EXCL-PER)
031116           MOVE ZEROS TO EX-MAX-BENS
031116                         EX-REM-BENS
031116        WHEN (WS-SRTD-COV-ENDS (S1) NOT = ZEROS)
031116           AND (WS-MONTHS-BETWEEN > WS-SRTD-COV-ENDS (S1))
031116           MOVE ZEROS TO EX-MAX-BENS
031116     END-EVALUATE
031116     IF EX-REC-MOS < WS-SRTD-BEN-PER (S1)
031116        MOVE ZEROS               TO EX-REM-BENS
031116     END-IF
031116
031116     .
031116 1820-EXIT.
031116     EXIT.
031116
031116 1850-GET-DDF-LIMITS.
031116
031116     IF CM-CLP-STATE = SPACES OR LOW-VALUES OR ZEROS
031116        MOVE CM-STATE            TO CM-CLP-STATE
031116     END-IF
031116
031116     MOVE CL-COMPANY-CD          TO ERPDEF-KEY
031116     MOVE CM-CLP-STATE           TO ERPDEF-STATE
031116     MOVE AM-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
031116
031116     IF (CL-CLAIM-TYPE = 'L' OR 'P')
031116        AND (CM-LF-BENEFIT-CD NOT = '00' AND '  ' AND 'DD')
031116        MOVE 'L'                 TO ERPDEF-BEN-TYPE
031116        MOVE CM-LF-BENEFIT-CD    TO ERPDEF-BEN-CODE
031116     ELSE
031116        MOVE 'A'                 TO ERPDEF-BEN-TYPE
031116        MOVE CM-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
031116     END-IF
031116
031116     MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
031116
031116     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
031116                                    PD-CONTROL-PRIMARY
031116     START ERPDEF KEY >= PD-CONTROL-PRIMARY
031116
031116     IF NOT ERPDEF-FILE-STATUS = '00'
031116        GO TO 1850-EXIT
031116     END-IF
031116
031116     .
031116 1850-READNEXT.
031116
031116     READ ERPDEF NEXT RECORD
031116
031116     IF NOT ERPDEF-FILE-STATUS = '00'
031116        GO TO 1850-EXIT
031116     END-IF
031116
031116     IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
031116        IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
031116           MOVE 'Y'              TO WS-PDEF-RECORD-SW
031116        ELSE
031116           GO TO 1850-READNEXT
031116        END-IF
031116     END-IF
031116
031116     .
031116 1850-EXIT.
031116     EXIT.
031116
031116 1900-BUILD-NON-DCC.
031116
031116     MOVE SPACES                 TO WS-PREV-CLM-TYPE
031116                                    WS-PREV-INS-TYPE
031116     MOVE ZEROS                  TO WS-PREV-BEN-PER
031116                                    WS-ACCUM-DAYS
031116                                    WS-ACCUM-AMT
031116                                    WS-ACCUM-PD-BENS
031116
031116     MOVE +1                     TO M1
031116     PERFORM VARYING S1 FROM +1 BY +1 UNTIL
031116        S1 > WS-MAX-SUB
031116        IF WS-SRTD-CLM-TYPE (S1) NOT = WS-PREV-CLM-TYPE
031116           PERFORM 1810-SET-NEW-HEAD
031116                                 THRU 1810-EXIT
031116           MOVE WS-SRTD-CLM-TYPE (S1)
031116                                 TO WS-PREV-CLM-TYPE
031116           ADD +1                TO M1
031116        END-IF
031116        MOVE WS-SRTD-BEN-PER (S1)  TO EX-BEN-PER
031116        IF WS-SRTD-INS-TYPE (S1) = '1'
031116           MOVE 'PRIM'             TO EX-INS-TYP
031116        ELSE
031116           MOVE 'COBO'             TO EX-INS-TYP
031116        END-IF
031116*       evaluate ws-srtd-status (s1)
031116*          when 'C'
031116*             MOVE 'CLOSED'        TO WSM-STATUS (M1)
031116*          WHEN 'D'
031116*             MOVE 'DENIED'        TO WSM-STATUS (M1)
031116*          WHEN 'O'
031116*             MOVE 'OPEN'          TO WSM-STATUS (M1)
031116*          WHEN OTHER
031116*             MOVE 'OTHER'         TO WSM-STATUS (M1)
031116*       END-EVALUATE
031116*       move ws-srtd-claim-no (s1) to wsm-claim-no (m1)
031116*                                     pi-wsm-claim-nos (s1)
031116*       move ws-srtd-total-paid (s1) to wsm-total-paid (m1)
031116
031116        COMPUTE EX-MAX-BENS =
031116           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
031116
031116        MOVE ZEROS TO WK1 WK2
031116        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
031116           COMPUTE WS-PD-BENS ROUNDED =
031116              WS-SRTD-TOTAL-PAID (S1) / WS-SRTD-MAX-MOBEN (S1)
031116           IF WS-SRTD-CLM-TYPE (S1) NOT = 'L' AND 'P'
031116              DIVIDE WS-SRTD-TOTAL-PAID (S1) BY
031116                 WS-SRTD-MAX-MOBEN(S1) GIVING WK1
031116                 REMAINDER WK2
031116           END-IF
031116        ELSE
031116           MOVE ZEROS            TO WS-PD-BENS
031116        END-IF
031116        IF (WS-PD-BENS = ZEROS)
031116           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
031116           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
031116           MOVE 1                TO WS-PD-BENS
031116        END-IF
031116        MOVE WS-PD-BENS          TO EX-PAID-BENS
031116*       IF WK2 NOT = ZEROS
031116*          MOVE '*'              TO WSM-PART (M1)
031116*       END-IF
031116        COMPUTE WS-ACCUM-AMT =
031116           WS-ACCUM-AMT + WS-SRTD-TOTAL-PAID (S1)
031116        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
031116           COMPUTE WS-ACCUM-PD-BENS ROUNDED =
031116              WS-ACCUM-AMT / WS-SRTD-MAX-MOBEN (S1)
031116        ELSE
031116           MOVE ZEROS            TO WS-ACCUM-PD-BENS
031116        END-IF
031116        IF (WS-ACCUM-PD-BENS = ZEROS)
031116           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
031116           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
031116           MOVE 1                TO WS-ACCUM-PD-BENS
031116        END-IF
031116        COMPUTE EX-REM-BENS =
031116           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
031116
031116*       MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-1
031116*       MOVE ' '                    TO DC-OPTION-CODE
031116*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
031116*       IF NO-CONVERSION-ERROR
031116*          MOVE DC-GREG-DATE-A-EDIT TO wsm-inc-date (m1)
031116*       ELSE
031116*          MOVE SPACES              TO wsm-inc-date (m1)
031116*       END-IF
031116*       MOVE ws-srtd-pd-thru-dt (s1) TO DC-BIN-DATE-1
031116*       MOVE ' '                     TO DC-OPTION-CODE
031116*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
031116*       IF NO-CONVERSION-ERROR
031116*          MOVE DC-GREG-DATE-A-EDIT TO wsm-pd-thru-dt (m1)
031116*       ELSE
031116*          MOVE SPACES              TO wsm-pd-thru-dt (m1)
031116*       END-IF
031116*       move WS-MAP-OUTPUT (m1)  to replineo (m1)
031116        IF WS-SRTD-CLAIM-NO (S1) = NA-CLAIM-NO
031116           MOVE WS-SRTD-EXCL-PER (S1) TO EX-EXCL-PER
031116           MOVE 26 TO S1
031116        END-IF
031116
031116        ADD +1                   TO M1
031116     END-PERFORM
           .

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
