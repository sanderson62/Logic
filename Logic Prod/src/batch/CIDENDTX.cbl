       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDENDTX.
       AUTHOR.     AJRA
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERENDT           ASSIGN TO ERENDT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS EN-CONTROL-PRIMARY
                                   FILE STATUS IS ERENDT-FILE-STATUS.

           SELECT  ISS-EXT-OUT     ASSIGN TO ISSOUT
                                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT  CAN-EXT-OUT     ASSIGN TO CANOUT
                                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       EJECT
       DATA DIVISION.
       FILE SECTION.


       FD  ERENDT.

                                   COPY ERCENDT.

       FD  ISS-EXT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  ISS-EXT-RECORD             PIC X(800).

       FD  CAN-EXT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  CAN-EXT-RECORD             PIC X(800).

       FD  DISK-DATE
           COPY ELCDTEFD.

       EJECT

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     ENDTX WORKING-STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ERENDT             VALUE 'Y'.
       77  EXT-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  ISS-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  CAN-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-FIX           PIC 9(9) VALUE ZEROS.
       77  PGM-SUB                PIC S9(04) COMP-3 VALUE +585.
       
       01  WS-MISC.
           05  ws-work-seq             pic x.
           05  ws-num-seq redefines ws-work-seq pic 9.
           05  WS-SAVE-ERENDT          PIC X(207) VALUE LOW-VALUES.
           05  ERENDT-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  ws-cert-no.
               10  ws-cert-prime       pic x(10) value spaces.
               10  ws-cert-sfx         pic x(1)  value spaces.
           05  WS-ZERO               PIC S9(01)    VALUE +0.
           05  WS-RETURN-CODE        PIC S9(03)    VALUE +0.
           05  WS-ABEND-MESSAGE      PIC X(80)     VALUE SPACES.
           05  WS-ABEND-FILE-STATUS  PIC X(02)     VALUE ZERO.
           05  WRK-COMPANY             PIC 99 COMP.
           05  FILLER REDEFINES WRK-COMPANY   COMP.
               10  FILLER              PIC 9.
               10  WRK-COMPANY-CD      PIC 9.
               
               
       01  EXT-I-REC.
           12  EXT-I-CARRIER              PIC X.
           12  ITAB-1                     PIC X.
           12  EXT-I-GROUPING             PIC X(6).
           12  ITAB-2                     PIC X.
           12  EXT-I-STATE                PIC XX.
           12  ITAB-3                     PIC X.
           12  EXT-I-ACCOUNT              PIC X(10).
           12  ITAB-4                     PIC X.
           12  EXT-I-CERT-EFF-DT          PIC X(10).
           12  ITAB-5                     PIC X.
           12  EXT-I-CERT-NO.
               16  EXT-I-CERT-PRIME       PIC X(10).
               16  EXT-I-CERT-SFX         PIC X.
           12  ITAB-6                     PIC X.
           12  EXT-I-REC-TYPE             PIC X.
           12  ITAB-7                     PIC X.
           12  EXT-I-SEQ-NO               PIC 9(04).
           12  ITAB-8                     PIC X.
           12  EXT-I-ARCHIVE-NO           PIC 9(8).
           12  ITAB-9                     PIC X.
           12  EXT-I-LAST-MAINT-DT        PIC X(10).
           12  ITAB-10                    PIC X.
           12  EXT-I-LAST-MAINT-BY        PIC X(4).
           12  ITAB-11                    PIC X.
           12  EXT-I-LAST-MAINT-HHMMSS    PIC 9(6).
           12  ITAB-12                    PIC X.
           12  EXT-I-INS-ORIG-LAST-NAME   PIC X(15).
           12  ITAB-13                    PIC X.
           12  EXT-I-INS-ORIG-FIRST-NAME  PIC X(10).
           12  ITAB-14                    PIC X.
           12  EXT-I-INS-ORIG-MIDDLE-INIT PIC X.
           12  ITAB-15                    PIC X.
           12  EXT-I-INS-ORIG-AGE         PIC 999.
           12  ITAB-16                    PIC X.
           12  EXT-I-INS-ORIG-AGE-DEF-FLAG PIC X.
           12  ITAB-17                    PIC X.
           12  EXT-I-JNT-ORIG-LAST-NAME   PIC X(15).
           12  ITAB-18                    PIC X.
           12  EXT-I-JNT-ORIG-FIRST-NAME  PIC X(10).
           12  ITAB-19                    PIC X.
           12  EXT-I-JNT-ORIG-MIDDLE-INIT PIC X.
           12  ITAB-20                    PIC X.
           12  EXT-I-JNT-ORIG-AGE         PIC 999.
           12  ITAB-21                    PIC X.
           12  EXT-I-JNT-ORIG-AGE-DEF-FLAG PIC X.
           12  ITAB-22                    PIC X.
           12  EXT-I-LF-ORIG-BENCD        PIC XX.
           12  ITAB-23                    PIC X.
           12  EXT-I-LF-ORIG-TERM         PIC 999.
           12  ITAB-24                    PIC X.
           12  EXT-I-LF-ORIG-BEN-AMT      PIC -(9).99.
           12  ITAB-25                    PIC X.
           12  EXT-I-LF-ORIG-PRM-AMT      PIC -(7).99.
           12  ITAB-26                    PIC X.
           12  EXT-I-LF-ORIG-ALT-BEN-AMT  PIC -(9).99.
           12  ITAB-27                    PIC X.
           12  EXT-I-LF-ORIG-ALT-PRM-AMT  PIC -(7).99.
           12  ITAB-28                    PIC X.
           12  EXT-I-LF-ORIG-EXP-DT       PIC X(10).
           12  ITAB-29                    PIC X.
           12  EXT-I-LF-ORIG-COMM-PCT     PIC -.9(5).
           12  ITAB-30                    PIC X.
           12  EXT-I-ORIG-CRED-BENE       PIC X(25).
           12  ITAB-31                    PIC X.
           12  EXT-I-AH-ORIG-BENCD        PIC XX.
           12  ITAB-32                    PIC X.
           12  EXT-I-AH-ORIG-TERM         PIC 999.
           12  ITAB-33                    PIC X.
           12  EXT-I-AH-ORIG-BEN-AMT      PIC -(9).99.
           12  ITAB-34                    PIC X.
           12  EXT-I-AH-ORIG-PRM-AMT      PIC -(7).99.
           12  ITAB-35                    PIC X.
           12  EXT-I-AH-ORIG-EXP-DT       PIC X(10).
           12  ITAB-36                    PIC X.
           12  EXT-I-AH-ORIG-COMM-PCT     PIC -.9(5).
           12  ITAB-37                    PIC X.
           12  EXT-I-AH-ORIG-CP           PIC 99.
           12  ITAB-38                    PIC X.
           12  EXT-I-INS-NEW-LAST-NAME    PIC X(15).
           12  ITAB-39                    PIC X.
           12  EXT-I-INS-NEW-FIRST-NAME   PIC X(10).
           12  ITAB-40                    PIC X.
           12  EXT-I-INS-NEW-MIDDLE-INIT  PIC X.
           12  ITAB-41                    PIC X.
           12  EXT-I-INS-NEW-AGE          PIC 999.
           12  ITAB-42                    PIC X.
           12  EXT-I-INS-NEW-AGE-DEF-FLAG PIC X.
           12  ITAB-43                    PIC X.
           12  EXT-I-JNT-NEW-LAST-NAME    PIC X(15).
           12  ITAB-44                    PIC X.
           12  EXT-I-JNT-NEW-FIRST-NAME   PIC X(10).
           12  ITAB-45                    PIC X.
           12  EXT-I-JNT-NEW-MIDDLE-INIT  PIC X.
           12  ITAB-46                    PIC X.
           12  EXT-I-JNT-NEW-AGE          PIC 999.
           12  ITAB-47                    PIC X.
           12  EXT-I-JNT-NEW-AGE-DEF-FLAG PIC X.
           12  ITAB-48                    PIC X.
           12  EXT-I-LF-NEW-BENCD         PIC XX.
           12  ITAB-49                    PIC X.
           12  EXT-I-LF-NEW-TERM          PIC 999.
           12  ITAB-50                    PIC X.
           12  EXT-I-LF-NEW-BEN-AMT       PIC -(9).99.
           12  ITAB-51                    PIC X.
           12  EXT-I-LF-NEW-PRM-AMT       PIC -(7).99.
           12  ITAB-52                    PIC X.
           12  EXT-I-LF-NEW-ALT-BEN-AMT   PIC -(9).99.
           12  ITAB-53                    PIC X.
           12  EXT-I-LF-NEW-ALT-PRM-AMT   PIC -(7).99.
           12  ITAB-54                    PIC X.
           12  EXT-I-LF-NEW-EXP-DT        PIC X(10).
           12  ITAB-55                    PIC X.
           12  EXT-I-LF-NEW-COMM-PCT      PIC -.9(5).
           12  ITAB-56                    PIC X.
           12  EXT-I-NEW-CRED-BENE        PIC X(25).
           12  ITAB-57                    PIC X.
           12  EXT-I-AH-NEW-BENCD         PIC XX.
           12  ITAB-58                    PIC X.
           12  EXT-I-AH-NEW-TERM          PIC 999.
           12  ITAB-59                    PIC X.
           12  EXT-I-AH-NEW-BEN-AMT       PIC -(9).99.
           12  ITAB-60                    PIC X.
           12  EXT-I-AH-NEW-PRM-AMT       PIC -(7).99.
           12  ITAB-61                    PIC X.
           12  EXT-I-AH-NEW-EXP-DT        PIC X(10).
           12  ITAB-62                    PIC X.
           12  EXT-I-AH-NEW-COMM-PCT      PIC -.9(5).
           12  ITAB-63                    PIC X.
           12  EXT-I-AH-NEW-CP            PIC 99.
           12  ITAB-64                    PIC X.
           12  EXT-I-SIG-SW               PIC X.
           12  ITAB-65                    PIC X.
           12  EXT-I-MONEY-SW             PIC X.
           12  ITAB-66                    PIC X.
           12  EXT-I-HEALTH-APP           PIC X.
           12  ITAB-67                    PIC X.
           12  EXT-I-VOUCHER-SW           PIC X.
           12  ITAB-68                    PIC X.
           12  EXT-I-PAYEE                PIC X(14).
           12  ITAB-69                    PIC X.
           12  EXT-I-INPUT-DT             PIC X(10).
           12  ITAB-70                    PIC X.
           12  EXT-I-PROCESS-DT           PIC X(10).
           12  ITAB-71                    PIC X.
           12  EXT-I-LF-COMMISSION        PIC -.9(5).
           12  ITAB-72                    PIC X.
           12  EXT-I-AH-COMMISSION        PIC -.9(5).
           12  ITAB-73                    PIC X.
           12  EXT-I-REASON-CODE-1        PIC X(4).
           12  ITAB-74                    PIC X.
           12  EXT-I-REASON-CODE-2        PIC X(4).
           12  ITAB-75                    PIC X.
           12  EXT-I-REASON-CODE-3        PIC X(4).
           12  ITAB-76                    PIC X.
           12  EXT-I-REASON-CODE-4        PIC X(4).
           12  ITAB-77                    PIC X.
           12  EXT-I-REASON-CODE-5        PIC X(4).
           12  ITAB-78                    PIC X.
           12  EXT-I-REASON-CODE-6        PIC X(4).
           12  ITAB-79                    PIC X.
           12  EXT-I-REASON-CODE-7        PIC X(4).
           12  ITAB-80                    PIC X.
           12  EXT-I-REASON-CODE-8        PIC X(4).
           12  ITAB-81                    PIC X.
           12  EXT-I-REASON-CODE-9        PIC X(4).
           12  ITAB-82                    PIC X.
           12  EXT-I-REASON-CODE-10       PIC X(4).
           12  ITAB-83                    PIC X.
           12  EXT-I-REASON-CODE-11       PIC X(4).
           12  ITAB-84                    PIC X.
           12  EXT-I-REASON-CODE-12       PIC X(4).
           12  ITAB-85                    PIC X.
           12  EXT-I-TEMPLATE-USED        PIC X(8).
           12  ITAB-86                    PIC X.
           12  EXT-I-DOCU-TYPE            PIC X.
           12  ITAB-87                    PIC X.
           12  EXT-I-COMMENTS1            PIC X(13).
           12  ITAB-88                    PIC X.
           12  EXT-I-COMMENTS2            PIC X(70).
           12  ITAB-89                    PIC X.
           12  EXT-I-COMM-CHGBK           PIC X.
           12  ITAB-90                    PIC X.
           12  EXT-I-CSO-PORTION          PIC -(5).99.
           12  ITAB-91                    PIC X.
           12  EXT-I-ACCT-PORTION         PIC -(5).99.
           12  ITAB-92                    PIC X.
           12  EXT-I-BATCH-NUMBER         PIC X(6).
           12  ITAB-93                    PIC X.
           12  EXT-I-ACCT-SUMM            PIC X.
           12  ITAB-94                    PIC X.
           12  EXT-I-CSO-SUMM             PIC X.
           12  ITAB-95                    PIC X.
           12  IEOR                       PIC X(1).

       01  EXT-C-REC.
           12  EXT-C-CARRIER              PIC X.
           12  CTAB-1                     PIC X.
           12  EXT-C-GROUPING             PIC X(6).
           12  CTAB-2                     PIC X.
           12  EXT-C-STATE                PIC XX.
           12  CTAB-3                     PIC X.
           12  EXT-C-ACCOUNT              PIC X(10).
           12  CTAB-4                     PIC X.
           12  EXT-C-CERT-EFF-DT          PIC X(10).
           12  CTAB-5                     PIC X.
           12  EXT-C-CERT-NO.
               16  EXT-C-CERT-PRIME       PIC X(10).
               16  EXT-C-CERT-SFX         PIC X.
           12  CTAB-6                     PIC X.
           12  EXT-C-REC-TYPE             PIC X.
           12  CTAB-7                     PIC X.
           12  EXT-C-SEQ-NO               PIC 9(04).
           12  CTAB-8                     PIC X.
           12  EXT-C-ARCHIVE-NO           PIC 9(8).
           12  CTAB-9                     PIC X.
           12  EXT-C-LAST-MAINT-DT        PIC X(10).
           12  CTAB-10                    PIC X.
           12  EXT-C-LAST-MAINT-BY        PIC X(4).
           12  CTAB-11                    PIC X.
           12  EXT-C-LAST-MAINT-HHMMSS    PIC 9(6).
           12  CTAB-12                    PIC X.
           12  EXT-C-INS-LAST-NAME        PIC X(15).
           12  CTAB-13                    PIC X.
           12  EXT-C-INS-FIRST-NAME       PIC X(10).
           12  CTAB-14                    PIC X.
           12  EXT-C-INS-MIDDLE-INIT      PIC X.
           12  CTAB-15                    PIC X.
           12  EXT-C-LF-ORIG-REF-DT       PIC X(10).
           12  CTAB-16                    PIC X.
           12  EXT-C-LF-ORIG-REF-AMT      PIC -(7).99.
           12  CTAB-17                    PIC X.
           12  EXT-C-LF-ORIG-COMM-PCT     PIC -.9(5).
           12  CTAB-18                    PIC X.
           12  EXT-C-AH-ORIG-REF-DT       PIC X(10).
           12  CTAB-19                    PIC X.
           12  EXT-C-AH-ORIG-REF-AMT      PIC -(7).99.
           12  CTAB-20                    PIC X.
           12  EXT-C-AH-ORIG-COMM-PCT     PIC -.9(5).
           12  CTAB-21                    PIC X.
           12  EXT-C-LF-NEW-REF-DT        PIC X(10).
           12  CTAB-22                    PIC X.
           12  EXT-C-LF-NEW-REF-AMT       PIC -(7).99.
           12  CTAB-23                    PIC X.
           12  EXT-C-LF-NEW-COMM-PCT      PIC -.9(5).
           12  CTAB-24                    PIC X.
           12  EXT-C-AH-NEW-REF-DT        PIC X(10).
           12  CTAB-25                    PIC X.
           12  EXT-C-AH-NEW-REF-AMT       PIC -(7).99.
           12  CTAB-26                    PIC X.
           12  EXT-C-AH-NEW-COMM-PCT      PIC -.9(5).
           12  CTAB-27                    PIC X.
           12  EXT-C-FLAG-CERT            PIC X.
           12  CTAB-28                    PIC X.
           12  EXT-C-MONEY-SW             PIC X.
           12  CTAB-29                    PIC X.
           12  EXT-C-HEALTH-APP           PIC X.
           12  CTAB-30                    PIC X.
           12  EXT-C-VOUCHER-SW           PIC X.
           12  CTAB-31                    PIC X.
           12  EXT-C-PAYEE                PIC X(14).
           12  CTAB-32                    PIC X.
           12  EXT-C-INPUT-DT             PIC X(10).
           12  CTAB-33                    PIC X.
           12  EXT-C-PROCESS-DT           PIC X(10).
           12  CTAB-34                    PIC X.
           12  EXT-C-LF-COMMISSION        PIC -.9(5).
           12  CTAB-35                    PIC X.
           12  EXT-C-AH-COMMISSION        PIC -.9(5).
           12  CTAB-36                    PIC X.
           12  EXT-C-REASON-CODE-1        PIC X(4).
           12  CTAB-37                    PIC X.
           12  EXT-C-REASON-CODE-2        PIC X(4).
           12  CTAB-38                    PIC X.
           12  EXT-C-REASON-CODE-3        PIC X(4).
           12  CTAB-39                    PIC X.
           12  EXT-C-REASON-CODE-4        PIC X(4).
           12  CTAB-40                    PIC X.
           12  EXT-C-REASON-CODE-5        PIC X(4).
           12  CTAB-41                    PIC X.
           12  EXT-C-REASON-CODE-6        PIC X(4).
           12  CTAB-42                    PIC X.
           12  EXT-C-REASON-CODE-7        PIC X(4).
           12  CTAB-43                    PIC X.
           12  EXT-C-REASON-CODE-8        PIC X(4).
           12  CTAB-44                    PIC X.
           12  EXT-C-REASON-CODE-9        PIC X(4).
           12  CTAB-45                    PIC X.
           12  EXT-C-REASON-CODE-10       PIC X(4).
           12  CTAB-46                    PIC X.
           12  EXT-C-REASON-CODE-11       PIC X(4).
           12  CTAB-47                    PIC X.
           12  EXT-C-REASON-CODE-12       PIC X(4).
           12  CTAB-48                    PIC X.
           12  EXT-C-TEMPLATE-USED        PIC X(8).
           12  CTAB-49                    PIC X.
           12  EXT-C-DOCU-TYPE            PIC X.
           12  CTAB-50                    PIC X.
           12  EXT-C-COMMENTS1            PIC X(13).
           12  CTAB-51                    PIC X.
           12  EXT-C-COMMENTS2            PIC X(70).
           12  CTAB-52                    PIC X.
           12  EXT-C-COMM-CHGBK           PIC X.
           12  CTAB-53                    PIC X.
           12  EXT-C-CSO-PORTION          PIC -(5).99.
           12  CTAB-54                    PIC X.
           12  EXT-C-ACCT-PORTION         PIC -(5).99.
           12  CTAB-55                    PIC X.
           12  EXT-C-BATCH-NUMBER         PIC X(6).
           12  CTAB-56                    PIC X.
           12  EXT-C-ACCT-SUMM            PIC X.
           12  CTAB-57                    PIC X.
           12  EXT-C-CSO-SUMM             PIC X.
           12  CTAB-58                    PIC X.
           12  CEOR                       PIC X(1).


                                     COPY ELCDTECX.

                                     COPY ELCDTEVR.

                                       COPY ELCDATE.
       
       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-CERT-NUMBER            PIC X(11)     VALUE SPACES.
       

       PROCEDURE DIVISION USING PARM.

       0000-MAIN.
           DISPLAY 'PARM = ' PARM-CERT-NUMBER.

       0000-DATE-CARD-READ. COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-EXT THRU 0100-EXIT UNTIL
                 END-OF-ERENDT

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' ENDR RECORDS READ    ' EXT-RECS-IN
           DISPLAY ' ISS RECORDS WRITTEN  ' ISS-RECS-OUT
           DISPLAY ' CAN RECORDS WRITTEN  ' CAN-RECS-OUT
           DISPLAY ' ENDR RECORDS FIXED   ' EXT-RECS-FIX
           GOBACK

           .
       0100-PROCESS-EXT.

           move en-cert-prime to ws-cert-prime
           move en-cert-sfx   to ws-cert-sfx
           IF ws-cert-no = parm-cert-number OR 
              PARM-CERT-NUMBER = 'ALL'
                PERFORM 0300-WRITE-EXT THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-EXT THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-EXT.

           READ ERENDT NEXT RECORD

           IF ERENDT-FILE-STATUS = '10' OR '23'
              SET END-OF-ERENDT        TO TRUE
           ELSE
              IF ERENDT-FILE-STATUS NOT = '00'
                 DISPLAY 'ERENDT READ NEXT ' ERENDT-FILE-STATUS
                 SET END-OF-ERENDT     TO TRUE
              END-IF
           END-IF

           IF EN-COMPANY-CD NOT EQUAL DTE-CLASIC-COMPANY-CD
               SET END-OF-ERENDT  TO TRUE
           END-IF

           IF NOT END-OF-ERENDT
              ADD 1 TO EXT-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-EXT.
       
           PERFORM 0310-INIT-EXT THRU 0310-EXIT.
                          
           MOVE EN-CARRIER       TO  EXT-I-CARRIER
                                     EXT-C-CARRIER.
           MOVE EN-GROUPING      TO  EXT-I-GROUPING
                                     EXT-C-GROUPING.
           MOVE EN-STATE         TO  EXT-I-STATE
                                     EXT-C-STATE.
           MOVE EN-ACCOUNT       TO  EXT-I-ACCOUNT
                                     EXT-C-ACCOUNT.
           MOVE EN-CERT-EFF-DT   TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EXT-I-CERT-EFF-DT
                                           EXT-C-CERT-EFF-DT
           ELSE
               MOVE SPACES              TO EXT-I-CERT-EFF-DT
                                           EXT-C-CERT-EFF-DT
           END-IF.
           MOVE EN-CERT-NO       TO  EXT-I-CERT-NO
                                     EXT-C-CERT-NO.
           MOVE EN-REC-TYPE      TO  EXT-I-REC-TYPE
                                     EXT-C-REC-TYPE.
           MOVE EN-SEQ-NO        TO  EXT-I-SEQ-NO
                                     EXT-C-SEQ-NO.
           MOVE EN-ARCHIVE-NO    TO  EXT-I-ARCHIVE-NO
                                     EXT-C-ARCHIVE-NO.
           MOVE EN-LAST-MAINT-DT TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EXT-I-LAST-MAINT-DT
                                           EXT-C-LAST-MAINT-DT
           ELSE
               MOVE SPACES             TO EXT-I-LAST-MAINT-DT
                                          EXT-C-LAST-MAINT-DT
           END-IF.
           MOVE EN-LAST-MAINT-BY TO EXT-I-LAST-MAINT-BY
                                    EXT-C-LAST-MAINT-BY.
           MOVE EN-LAST-MAINT-HHMMSS TO EXT-I-LAST-MAINT-HHMMSS
                                        EXT-C-LAST-MAINT-HHMMSS.

           IF EN-REC-TYPE = 'I'
              MOVE EN-INS-ORIG-LAST-NAME TO EXT-I-INS-ORIG-LAST-NAME
              MOVE EN-INS-ORIG-FIRST-NAME TO EXT-I-INS-ORIG-FIRST-NAME
              MOVE EN-INS-ORIG-MIDDLE-INIT TO EXT-I-INS-ORIG-MIDDLE-INIT
              IF EN-INS-ORIG-AGE NUMERIC
                  MOVE EN-INS-ORIG-AGE TO EXT-I-INS-ORIG-AGE
              END-IF
              MOVE EN-JNT-ORIG-LAST-NAME TO EXT-I-JNT-ORIG-LAST-NAME
              MOVE EN-JNT-ORIG-FIRST-NAME TO EXT-I-JNT-ORIG-FIRST-NAME
              MOVE EN-JNT-ORIG-MIDDLE-INIT TO EXT-I-JNT-ORIG-MIDDLE-INIT
              IF EN-JNT-ORIG-AGE NUMERIC
                  MOVE EN-JNT-ORIG-AGE TO EXT-I-JNT-ORIG-AGE
              END-IF
              MOVE EN-LF-ORIG-BENCD TO EXT-I-LF-ORIG-BENCD
              MOVE EN-LF-ORIG-TERM TO EXT-I-LF-ORIG-TERM
              IF EN-LF-ORIG-BEN-AMT NUMERIC
                  MOVE EN-LF-ORIG-BEN-AMT TO EXT-I-LF-ORIG-BEN-AMT
              END-IF
              IF EN-LF-ORIG-PRM-AMT NUMERIC
                  MOVE EN-LF-ORIG-PRM-AMT TO EXT-I-LF-ORIG-PRM-AMT
              END-IF
              IF EN-LF-ORIG-ALT-BEN-AMT NUMERIC
                  MOVE EN-LF-ORIG-ALT-BEN-AMT TO 
                                         EXT-I-LF-ORIG-ALT-BEN-AMT
              END-IF
              IF EN-LF-ORIG-ALT-PRM-AMT NUMERIC
                  MOVE EN-LF-ORIG-ALT-PRM-AMT TO 
                                         EXT-I-LF-ORIG-ALT-PRM-AMT
              END-IF
              MOVE EN-LF-ORIG-EXP-DT TO DC-BIN-DATE-1
              MOVE ' '               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EXT-I-LF-ORIG-EXP-DT
              ELSE
                  MOVE SPACES             TO EXT-I-LF-ORIG-EXP-DT
              END-IF
              MOVE EN-ORIG-CRED-BENE TO EXT-I-ORIG-CRED-BENE
              IF EN-LF-ORIG-COMM-PCT NUMERIC
                  MOVE EN-LF-ORIG-COMM-PCT TO EXT-I-LF-ORIG-COMM-PCT
	            END-IF
              MOVE EN-AH-ORIG-BENCD TO EXT-I-AH-ORIG-BENCD
              MOVE EN-AH-ORIG-TERM TO EXT-I-AH-ORIG-TERM
              IF EN-AH-ORIG-BEN-AMT NUMERIC
                 MOVE EN-AH-ORIG-BEN-AMT TO EXT-I-AH-ORIG-BEN-AMT
              END-IF
              IF EN-AH-ORIG-PRM-AMT NUMERIC
                  MOVE EN-AH-ORIG-PRM-AMT TO EXT-I-AH-ORIG-PRM-AMT
              END-IF
              MOVE EN-AH-ORIG-EXP-DT TO DC-BIN-DATE-1
              MOVE ' '               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EXT-I-AH-ORIG-EXP-DT
              ELSE
                  MOVE SPACES             TO EXT-I-AH-ORIG-EXP-DT
              END-IF
              IF EN-AH-ORIG-COMM-PCT NUMERIC
                  MOVE EN-AH-ORIG-COMM-PCT TO EXT-I-AH-ORIG-COMM-PCT
              END-IF
              MOVE EN-AH-ORIG-CP TO EXT-I-AH-ORIG-CP
              MOVE EN-INS-NEW-LAST-NAME TO EXT-I-INS-NEW-LAST-NAME
              MOVE EN-INS-NEW-FIRST-NAME TO EXT-I-INS-NEW-FIRST-NAME
              MOVE EN-INS-NEW-MIDDLE-INIT TO EXT-I-INS-NEW-MIDDLE-INIT
              IF EN-INS-NEW-AGE NUMERIC
                  MOVE EN-INS-NEW-AGE TO EXT-I-INS-NEW-AGE
              END-IF
              MOVE EN-JNT-NEW-LAST-NAME TO EXT-I-JNT-NEW-LAST-NAME
              MOVE EN-JNT-NEW-FIRST-NAME TO EXT-I-JNT-NEW-FIRST-NAME
              MOVE EN-JNT-NEW-MIDDLE-INIT TO EXT-I-JNT-NEW-MIDDLE-INIT
              IF EN-JNT-NEW-AGE NUMERIC
                  MOVE EN-JNT-NEW-AGE TO EXT-I-JNT-NEW-AGE
              END-IF
              MOVE EN-LF-NEW-BENCD TO EXT-I-LF-NEW-BENCD
              MOVE EN-LF-NEW-TERM  TO EXT-I-LF-NEW-TERM
              IF EN-LF-NEW-BEN-AMT NUMERIC
                  MOVE EN-LF-NEW-BEN-AMT TO EXT-I-LF-NEW-BEN-AMT
              END-IF
              IF EN-LF-NEW-PRM-AMT NUMERIC
                  MOVE EN-LF-NEW-PRM-AMT TO EXT-I-LF-NEW-PRM-AMT
              END-IF
              IF EN-LF-NEW-ALT-BEN-AMT NUMERIC
                  MOVE EN-LF-NEW-ALT-BEN-AMT TO EXT-I-LF-NEW-ALT-BEN-AMT
              END-IF
              IF EN-LF-NEW-ALT-PRM-AMT NUMERIC
                  MOVE EN-LF-NEW-ALT-PRM-AMT TO EXT-I-LF-NEW-ALT-PRM-AMT
              END-IF
              MOVE EN-LF-NEW-EXP-DT TO DC-BIN-DATE-1
              MOVE ' '              TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EXT-I-LF-NEW-EXP-DT
              ELSE
                  MOVE SPACES             TO EXT-I-LF-NEW-EXP-DT
              END-IF
              MOVE EN-NEW-CRED-BENE TO EXT-I-NEW-CRED-BENE
              IF EN-LF-NEW-COMM-PCT NUMERIC
                  MOVE EN-LF-NEW-COMM-PCT TO EXT-I-LF-NEW-COMM-PCT
              END-IF
              MOVE EN-AH-NEW-BENCD TO EXT-I-AH-NEW-BENCD
              MOVE EN-AH-NEW-TERM TO EXT-I-AH-NEW-TERM
              IF EN-AH-NEW-BEN-AMT NUMERIC
                  MOVE EN-AH-NEW-BEN-AMT TO EXT-I-AH-NEW-BEN-AMT
              END-IF
              IF EN-AH-NEW-PRM-AMT NUMERIC
                  MOVE EN-AH-NEW-PRM-AMT TO EXT-I-AH-NEW-PRM-AMT
              END-IF
              MOVE EN-AH-NEW-EXP-DT TO DC-BIN-DATE-1
              MOVE ' '              TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EXT-I-AH-NEW-EXP-DT
              ELSE
                  MOVE SPACES             TO EXT-I-AH-NEW-EXP-DT
              END-IF
              MOVE EN-AH-NEW-CP TO EXT-I-AH-NEW-CP
              MOVE EN-SIG-SW TO EXT-I-SIG-SW
              MOVE EN-AH-NEW-COMM-PCT TO EXT-I-AH-NEW-COMM-PCT
121812        MOVE EN-INS-ORIG-AGE-DEF-FLAG TO 
                                   EXT-I-INS-ORIG-AGE-DEF-FLAG
121812        MOVE EN-JNT-ORIG-AGE-DEF-FLAG TO 
                                   EXT-I-JNT-ORIG-AGE-DEF-FLAG
121812        MOVE EN-INS-NEW-AGE-DEF-FLAG TO 
                                   EXT-I-INS-NEW-AGE-DEF-FLAG
121812        MOVE EN-JNT-NEW-AGE-DEF-FLAG TO 
                                   EXT-I-JNT-NEW-AGE-DEF-FLAG
           ELSE
              MOVE EN-INS-LAST-NAME  TO EXT-C-INS-LAST-NAME
              MOVE EN-INS-FIRST-NAME TO EXT-C-INS-FIRST-NAME
              MOVE EN-INS-MIDDLE-INIT TO EXT-C-INS-MIDDLE-INIT
              MOVE EN-LF-ORIG-REF-DT TO DC-BIN-DATE-1
              MOVE ' '               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EXT-C-LF-ORIG-REF-DT
              ELSE
                  MOVE SPACES             TO EXT-C-LF-ORIG-REF-DT
              END-IF
              IF EN-LF-ORIG-REF-AMT NUMERIC
                  MOVE EN-LF-ORIG-REF-AMT TO EXT-C-LF-ORIG-REF-AMT
              END-IF
              IF EN-LF-ORIG-REF-COMM-PCT NUMERIC
                  MOVE EN-LF-ORIG-REF-COMM-PCT TO 
                                          EXT-C-LF-ORIG-COMM-PCT
              END-IF
              MOVE EN-AH-ORIG-REF-DT TO DC-BIN-DATE-1
              MOVE ' '               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EXT-C-AH-ORIG-REF-DT
              ELSE
                  MOVE SPACES             TO EXT-C-AH-ORIG-REF-DT
              END-IF
              IF EN-AH-ORIG-REF-AMT NUMERIC
                  MOVE EN-AH-ORIG-REF-AMT TO EXT-C-AH-ORIG-REF-AMT
              END-IF
              IF EN-AH-ORIG-REF-COMM-PCT NUMERIC
                  MOVE EN-AH-ORIG-REF-COMM-PCT TO 
                                           EXT-C-AH-ORIG-COMM-PCT
              END-IF
              MOVE EN-LF-NEW-REF-DT TO DC-BIN-DATE-1
              MOVE ' '              TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EXT-C-LF-NEW-REF-DT
              ELSE
                  MOVE SPACES             TO EXT-C-LF-NEW-REF-DT
              END-IF
              IF EN-LF-NEW-REF-AMT NUMERIC
                  MOVE EN-LF-NEW-REF-AMT TO EXT-C-LF-NEW-REF-AMT
              END-IF
              IF EN-LF-NEW-REF-COMM-PCT NUMERIC
                  MOVE EN-LF-NEW-REF-COMM-PCT TO 
                                           EXT-C-LF-NEW-COMM-PCT
              END-IF
              MOVE EN-AH-NEW-REF-DT TO DC-BIN-DATE-1
              MOVE ' '              TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EXT-C-AH-NEW-REF-DT
              ELSE
                  MOVE SPACES             TO EXT-C-AH-NEW-REF-DT
              END-IF
              IF EN-AH-NEW-REF-AMT NUMERIC
                  MOVE EN-AH-NEW-REF-AMT TO EXT-C-AH-NEW-REF-AMT
              END-IF
              IF EN-AH-NEW-REF-COMM-PCT NUMERIC
                  MOVE EN-AH-NEW-REF-COMM-PCT TO 
                                           EXT-C-AH-NEW-COMM-PCT
              END-IF
              MOVE EN-FLAG-CERT TO EXT-C-FLAG-CERT
           END-IF.

           MOVE EN-MONEY-SW TO EXT-I-MONEY-SW
                               EXT-C-MONEY-SW.
           MOVE EN-HEALTH-APP TO EXT-I-HEALTH-APP
                                 EXT-C-HEALTH-APP.
           MOVE EN-VOUCHER-SW TO EXT-I-VOUCHER-SW
                                 EXT-C-VOUCHER-SW.
           MOVE EN-PAYEE TO EXT-I-PAYEE
                            EXT-C-PAYEE.
           MOVE EN-INPUT-DT      TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EXT-I-INPUT-DT
                                           EXT-C-INPUT-DT
           ELSE
               MOVE SPACES             TO EXT-I-INPUT-DT
                                          EXT-C-INPUT-DT
           END-IF.
           MOVE EN-PROCESS-DT    TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EXT-I-PROCESS-DT
                                           EXT-C-PROCESS-DT
           ELSE
               MOVE SPACES             TO EXT-I-PROCESS-DT
                                          EXT-C-PROCESS-DT
           END-IF.
           IF EN-LF-COMMISSION NUMERIC
               MOVE EN-LF-COMMISSION TO EXT-I-LF-COMMISSION
                                        EXT-C-LF-COMMISSION
           END-IF.
           IF EN-AH-COMMISSION NUMERIC
               MOVE EN-AH-COMMISSION TO EXT-I-AH-COMMISSION
                                        EXT-C-AH-COMMISSION
           END-IF.
           MOVE EN-REASON-CODE(1) TO EXT-I-REASON-CODE-1
                                     EXT-C-REASON-CODE-1.
           MOVE EN-REASON-CODE(2) TO EXT-I-REASON-CODE-2
                                     EXT-C-REASON-CODE-2.
           MOVE EN-REASON-CODE(3) TO EXT-I-REASON-CODE-3
                                     EXT-C-REASON-CODE-3.
           MOVE EN-REASON-CODE(4) TO EXT-I-REASON-CODE-4
                                     EXT-C-REASON-CODE-4.
           MOVE EN-REASON-CODE(5) TO EXT-I-REASON-CODE-5
                                     EXT-C-REASON-CODE-5.
           MOVE EN-REASON-CODE(6) TO EXT-I-REASON-CODE-6
                                     EXT-C-REASON-CODE-6.
           MOVE EN-REASON-CODE(7) TO EXT-I-REASON-CODE-7
                                     EXT-C-REASON-CODE-7.
           MOVE EN-REASON-CODE(8) TO EXT-I-REASON-CODE-8
                                     EXT-C-REASON-CODE-8.
           MOVE EN-REASON-CODE(9) TO EXT-I-REASON-CODE-9
                                     EXT-C-REASON-CODE-9.
           MOVE EN-REASON-CODE(10) TO EXT-I-REASON-CODE-10
                                      EXT-C-REASON-CODE-10.
           MOVE EN-REASON-CODE(11) TO EXT-I-REASON-CODE-11
                                      EXT-C-REASON-CODE-11.
           MOVE EN-REASON-CODE(12) TO EXT-I-REASON-CODE-12
                                      EXT-C-REASON-CODE-12.
           MOVE EN-TEMPLATE-USED TO EXT-I-TEMPLATE-USED
                                    EXT-C-TEMPLATE-USED.
           MOVE EN-DOCU-TYPE TO EXT-I-DOCU-TYPE
                                EXT-C-DOCU-TYPE.
           MOVE EN-COMMENTS1 TO EXT-I-COMMENTS1
                                EXT-C-COMMENTS1.
           MOVE EN-COMMENTS2 TO EXT-I-COMMENTS2
                                EXT-C-COMMENTS2.
           MOVE EN-COMM-CHGBK TO EXT-I-COMM-CHGBK
                                 EXT-C-COMM-CHGBK.
           IF EN-CSO-PORTION NUMERIC
               MOVE EN-CSO-PORTION TO EXT-I-CSO-PORTION
                                      EXT-C-CSO-PORTION
           END-IF.
           IF EN-ACCT-PORTION NUMERIC
               MOVE EN-ACCT-PORTION TO EXT-I-ACCT-PORTION
                                       EXT-C-ACCT-PORTION
           END-IF.
           MOVE EN-BATCH-NUMBER TO EXT-I-BATCH-NUMBER
                                   EXT-C-BATCH-NUMBER.
           MOVE EN-ACCT-SUMM    TO EXT-I-ACCT-SUMM
                                   EXT-C-ACCT-SUMM.
           MOVE EN-CSO-SUMM     TO EXT-I-CSO-SUMM
                                   EXT-C-CSO-SUMM.
           
           EXAMINE EXT-I-REC REPLACING ALL LOW-VALUES BY SPACES.
           EXAMINE EXT-C-REC REPLACING ALL LOW-VALUES BY SPACES.

           IF EN-REC-TYPE = 'I'
               WRITE ISS-EXT-RECORD FROM EXT-I-REC
               ADD 1 TO ISS-RECS-OUT
           ELSE
               WRITE CAN-EXT-RECORD FROM EXT-C-REC
               ADD 1 TO CAN-RECS-OUT
           END-IF.

           .

       0300-EXIT.
           EXIT.
           
           

       0310-INIT-EXT.
           MOVE SPACES TO EXT-I-REC
                          EXT-C-REC.
           MOVE +0     TO EXT-I-LF-COMMISSION
                          EXT-C-LF-COMMISSION
                          EXT-I-AH-COMMISSION 
                          EXT-C-AH-COMMISSION
                          EXT-I-CSO-PORTION
                          EXT-C-CSO-PORTION
                          EXT-I-ACCT-PORTION
                          EXT-C-ACCT-PORTION
                          EXT-I-INS-ORIG-AGE
                          EXT-I-JNT-ORIG-AGE                
                          EXT-I-LF-ORIG-TERM                
                          EXT-I-LF-ORIG-BEN-AMT             
                          EXT-I-LF-ORIG-PRM-AMT             
                          EXT-I-LF-ORIG-ALT-BEN-AMT         
                          EXT-I-LF-ORIG-ALT-PRM-AMT         
                          EXT-I-LF-ORIG-COMM-PCT
                          EXT-I-AH-ORIG-BEN-AMT
                          EXT-I-AH-ORIG-PRM-AMT      
                          EXT-I-AH-ORIG-COMM-PCT     
                          EXT-I-INS-NEW-AGE          
                          EXT-I-JNT-NEW-AGE          
                          EXT-I-LF-NEW-BEN-AMT       
                          EXT-I-LF-NEW-PRM-AMT       
                          EXT-I-LF-NEW-ALT-BEN-AMT   
                          EXT-I-LF-NEW-ALT-PRM-AMT                                
                          EXT-I-LF-NEW-COMM-PCT             
                          EXT-I-AH-NEW-BEN-AMT              
                          EXT-I-AH-NEW-PRM-AMT              
                          EXT-I-AH-NEW-COMM-PCT
                          EXT-C-LF-ORIG-REF-AMT      
                          EXT-C-LF-ORIG-COMM-PCT                                  
                          EXT-C-AH-ORIG-REF-AMT
                          EXT-C-AH-ORIG-COMM-PCT
                          EXT-C-LF-NEW-REF-AMT       
                          EXT-C-LF-NEW-COMM-PCT                                  
                          EXT-C-AH-NEW-REF-AMT
                          EXT-C-AH-NEW-COMM-PCT.
                          
            MOVE '~' TO ITAB-1 ITAB-2 ITAB-3 ITAB-4 ITAB-5 ITAB-6
                 ITAB-7  ITAB-8  ITAB-9  ITAB-10 ITAB-11 ITAB-12 
                 ITAB-13 ITAB-14 ITAB-15 ITAB-16 ITAB-17 ITAB-18
                 ITAB-19 ITAB-20 ITAB-21 ITAB-22 ITAB-23 ITAB-24
                 ITAB-25 ITAB-26 ITAB-27 ITAB-28 ITAB-29 ITAB-30
                 ITAB-31 ITAB-32 ITAB-33 ITAB-34 ITAB-35 ITAB-36
                 ITAB-37 ITAB-38 ITAB-39 ITAB-40 ITAB-41 ITAB-42 
                 ITAB-43 ITAB-44 ITAB-45 ITAB-46 ITAB-47 ITAB-48
                 ITAB-49 ITAB-50 ITAB-51 ITAB-52 ITAB-53 ITAB-54
                 ITAB-55 ITAB-56 ITAB-57 ITAB-58 ITAB-59 ITAB-60
                 ITAB-61 ITAB-62 ITAB-63 ITAB-64 ITAB-65 ITAB-66
                 ITAB-67 ITAB-68 ITAB-69 ITAB-70 ITAB-71 ITAB-72 
                 ITAB-73 ITAB-74 ITAB-75 ITAB-76 ITAB-77 ITAB-78
                 ITAB-79 ITAB-80 ITAB-81 ITAB-82 ITAB-83 ITAB-84
                 ITAB-85 ITAB-86 ITAB-87 ITAB-88 ITAB-89 ITAB-90
                 ITAB-91 ITAB-92 ITAB-93 ITAB-94 ITAB-95. 

            MOVE '~' TO CTAB-1 CTAB-2 CTAB-3 CTAB-4 CTAB-5 CTAB-6
                 CTAB-7  CTAB-8  CTAB-9  CTAB-10 CTAB-11 CTAB-12 
                 CTAB-13 CTAB-14 CTAB-15 CTAB-16 CTAB-17 CTAB-18
                 CTAB-19 CTAB-20 CTAB-21 CTAB-22 CTAB-23 CTAB-24
                 CTAB-25 CTAB-26 CTAB-27 CTAB-28 CTAB-29 CTAB-30
                 CTAB-31 CTAB-32 CTAB-33 CTAB-34 CTAB-35 CTAB-36
                 CTAB-37 CTAB-38 CTAB-39 CTAB-40 CTAB-41 CTAB-42 
                 CTAB-43 CTAB-44 CTAB-45 CTAB-46 CTAB-47 CTAB-48
                 CTAB-49 CTAB-50 CTAB-51 CTAB-52 CTAB-53 CTAB-54
                 CTAB-55 CTAB-56 CTAB-57 CTAB-58.
                 
           MOVE 'E' TO IEOR CEOR.
                 
                          
       0310-EXIT.
           EXIT.
           
           

       0400-OPEN-FILES.
           OPEN INPUT ERENDT
               OUTPUT ISS-EXT-OUT CAN-EXT-OUT

           IF ERENDT-FILE-STATUS = '00' OR '97'
              continue
           ELSE
              DISPLAY 'ERENDT open err  ' ERENDT-FILE-STATUS
           END-IF


           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERENDT ISS-EXT-OUT CAN-EXT-OUT

           .

       0500-EXIT.
           EXIT.
       0550-START-ERENDT.

           MOVE LOW-VALUES             TO EN-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO EN-COMPANY-CD

           START ERENDT KEY IS NOT < EN-CONTROL-PRIMARY

           IF ERENDT-FILE-STATUS = '10' OR '23'
              SET END-OF-ERENDT        TO TRUE
           ELSE
              IF ERENDT-FILE-STATUS NOT = '00'
                 DISPLAY 'ERENDT START     ' ERENDT-FILE-STATUS
                 SET END-OF-ERENDT     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           PERFORM 0550-START-ERENDT   THRU 0550-EXIT
           PERFORM 0200-READ-EXT THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.

       ABEND-PGM.   COPY ELCABEND.