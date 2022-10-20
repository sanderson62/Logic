00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL004.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:23:08.
00007 *                            VMOD=2.004
00008 *
00008 *
00009 *AUTHOR.           LOGIC,INC.
00010 *                  DALLAS,TEXAS.
00011
00012 *DATE-COMPILED.
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022
00023 *REMARKS.  COMMON ABEND ROUTINE.
00024 *        THIS PROGRAM IS LINK'D TO WHENEVER AN ERROR OCCURS THAT
00025 *        IS NOT CONTROLLED THROUGH A HANDLE CONDITION.
00026
00027
00028      EJECT
00029  ENVIRONMENT DIVISION.
00030
00031  DATA DIVISION.
00032
00033  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00034
00035  77  FILLER  PIC X(32)  VALUE '********************************'.
00036  77  FILLER  PIC X(32)  VALUE '*    EL004 WORKING STORAGE     *'.
00037  77  FILLER  PIC X(32)  VALUE '********* V/M 2.004 ************'.
00038
00039  77  WS-DUMP-SW    COMP-3        PIC S9              VALUE ZERO.
00040  77  WS-INDEX      COMP-3        PIC S9(3)           VALUE ZERO.
00041  77  WS-INDEX2     COMP-3        PIC S9(3)           VALUE ZERO.
00042
00043  01  TERMINAL-MESSAGE.
00044      05  TM-SBA                  PIC X.
00045      05  TM-FADDR                PIC XX.
00046      05  TM-SF                   PIC X.
00047      05  TM-ATTRB                PIC X.
00048      05  TM-MESG                 PIC X(72)   VALUE SPACES.
00049      05  FILLER REDEFINES TM-MESG.
00050          10  TMR1-A              PIC X(19).
00051          10  TMR1-B              PIC X(53).
00052      05  FILLER REDEFINES TM-MESG.
00053          10  TMR2-A              PIC X(9).
00054          10  TMR2-B              PIC X(10).
00055          10  TMR2-C              PIC X(53).
00056      05  FILLER REDEFINES TM-MESG.
00057          10  FILLER              PIC X(68).
00058          10  TMR3-A              PIC X(4).
00059
00060      05  FILLER                  PIC X               VALUE SPACES.
00061
00062      05  TM-EIBFN                PIC X(4)            VALUE SPACES.
00063      05  TM-EIBFN-CHAR           REDEFINES
00064          TM-EIBFN                PIC X           OCCURS 4 TIMES.
00065
00066      05  FILLER                  PIC X               VALUE SPACES.
00067
00068      05  TM-EIBRCODE             PIC X(12)           VALUE SPACES.
00069      05  TM-EIBRCODE-CHAR        REDEFINES
00070          TM-EIBRCODE             PIC X           OCCURS 12 TIMES.
00071
00072
00073  01  EIB-BREAK-DOWN.
00074      05  EBD-FN.
00075          10  EBD-FN1             PIC X.
00076          10  EBD-FN2             PIC X.
00077      05  EIBFN-CHAR              REDEFINES
00078          EBD-FN                  PIC X    OCCURS 2 TIMES.
00079
00080      05  EBD-RCODE.
00081          10  EBD-RC1             PIC X.
00082          10  EBD-RC2A.
00083              15  EBD-RC2         PIC X.
00084              15  EBD-RC3         PIC X.
00085              15  EBD-RC4         PIC X.
00086              15  EBD-RC5         PIC X.
00087              15  EBD-RC6         PIC X.
00088 *
00089      05  EIBRCODE-CHAR           REDEFINES
00090          EBD-RCODE               PIC X    OCCURS 6 TIMES.
00091
00092      05  WS-NUMBER                   PIC S9(4)       VALUE ZERO
00093                                      COMP.
00094
00095      05  FILLER                      REDEFINES
00096          WS-NUMBER.
00097          10  FILLER                  PIC X.
00098          10  WS-CHAR                 PIC X.
00099
00100      05  WS-REMAINDER                PIC S9(3)       VALUE ZERO
00101                                      COMP-3.
00102
00103      05  WS-HEX-VALUES               PIC X(15)       VALUE
00104          '123456789ABCDEF'.
00105
00106      05  WS-HEX-CHAR                 REDEFINES
00107          WS-HEX-VALUES               PIC X
00108          OCCURS 15 TIMES.
00109
00110      05  WS-CHARACTERS               PIC X(10)       VALUE SPACES.
00111      05  WS-CHARACTER                REDEFINES
00112          WS-CHARACTERS               PIC X
00113          OCCURS 10 TIMES.
00114
00115      05  WS-ZONE                     PIC X
00116          OCCURS 10 TIMES.
00117
00118      05  WS-DIGIT                    PIC X
00119          OCCURS 10 TIMES.
00120
00121  01  HEX-BREAK-DOWN.
00122      05  HEX-BD                  PIC 9(4)    COMP VALUE ZEROS.
00123      05  HEX-1  REDEFINES HEX-BD.
00124          10  FILLER              PIC X.
00125          10  HEX-NBR             PIC X.
00126
00127  01  HEX-NX                      PIC XX      VALUE '*0'.
00128 *    HEX-NX IS FOR LINE 24
00129
00130  01  HEX-L21                     PIC XX      VALUE 'R '.
00131 *    HEX-L21 IS FOR LINE 21
00132
00133  01  SAVE-DS                     PIC X(8).
00134
00135  01  WORK-EIB                    PIC X(80).
00136
00137  01  FILLER  REDEFINES  WORK-EIB.
00138      05  FILLER                  PIC S9(7)   COMP-3.
00139      05  FILLER                  PIC S9(7)   COMP-3.
00140      05  FILLER                  PIC X(4).
00141      05  FILLER                  PIC S9(7)   COMP-3.
00142      05  FILLER                  PIC X(4).
00143      05  FILLER                  PIC S9(4)   COMP.
00144      05  FILLER                  PIC S9(4)   COMP.
00145      05  FILLER                  PIC S9(4)   COMP.
00146      05  FILLER                  PIC XX.
00147      05  W-EIBFN                 PIC XX.
PEMUNI     05  FILLER                  PIC XX.
00148      05  W-EIBRCODE              PIC X(6).
PEMUNI     05  FILLER                  PIC XX.
00149      05  W-EIBDS                 PIC X(8).
00150      05  W-EIBREQID              PIC X(8).
00151      05  W-EIBRSRCE              PIC X(8).
00152
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00154
00155  01  DFHCOMMAREA                 PIC X(72).
00156
00157      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL004' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00159
00160  000-MAIN-LOGIC SECTION.
00161
00162  000-MLS-10-NOTES.
00163 *
00164 *    THIS SECTION CONTROLS ALL OTHER SECTIONS
00165 *
00166 *    CHECK EIBFN CODES TO PERFORM CORRECT
00167 *    OPERATION
00168 *
00169      MOVE DFHCOMMAREA            TO WORK-EIB.
00170      MOVE W-EIBFN                TO EBD-FN
00171                                  WS-CHARACTERS.
00172
00173      PERFORM 1000-CONVERT-HEX-CHARACTERS
00174          VARYING WS-INDEX FROM +1 BY +1
00175              UNTIL WS-INDEX GREATER THAN +2.
00176
00177      MOVE WS-ZONE (1)            TO  TM-EIBFN-CHAR (1).
00178      MOVE WS-DIGIT (1)           TO  TM-EIBFN-CHAR (2).
00179      MOVE WS-ZONE (2)            TO  TM-EIBFN-CHAR (3).
00180      MOVE WS-DIGIT (2)           TO  TM-EIBFN-CHAR (4).
00181
00182      MOVE W-EIBRCODE             TO EBD-RCODE
00183                                  WS-CHARACTERS.
00184
00185      PERFORM 1000-CONVERT-HEX-CHARACTERS
00186          VARYING WS-INDEX FROM +1 BY +1
00187              UNTIL WS-INDEX GREATER THAN +6.
00188
00189      MOVE WS-ZONE  (1)           TO  TM-EIBRCODE-CHAR (1).
00190      MOVE WS-DIGIT (1)           TO  TM-EIBRCODE-CHAR (2).
00191      MOVE WS-ZONE  (2)           TO  TM-EIBRCODE-CHAR (3).
00192      MOVE WS-DIGIT (2)           TO  TM-EIBRCODE-CHAR (4).
00193      MOVE WS-ZONE  (3)           TO  TM-EIBRCODE-CHAR (5).
00194      MOVE WS-DIGIT (3)           TO  TM-EIBRCODE-CHAR (6).
00195      MOVE WS-ZONE  (4)           TO  TM-EIBRCODE-CHAR (7).
00196      MOVE WS-DIGIT (4)           TO  TM-EIBRCODE-CHAR (8).
00197      MOVE WS-ZONE  (5)           TO  TM-EIBRCODE-CHAR (9).
00198      MOVE WS-DIGIT (5)           TO  TM-EIBRCODE-CHAR (10).
00199      MOVE WS-ZONE  (6)           TO  TM-EIBRCODE-CHAR (11).
00200      MOVE WS-DIGIT (6)           TO  TM-EIBRCODE-CHAR (12).
00201
00202      MOVE W-EIBDS                TO SAVE-DS.
00203      MOVE DFHEIBLK               TO WORK-EIB.
00204
00205      MOVE 02 TO HEX-BD.
00206      IF HEX-NBR = EBD-FN1
00207          PERFORM 100-CICS-ERROR
00208          GO TO 000-MLS-100.
00209
00210      MOVE 04 TO HEX-BD.
00211      IF HEX-NBR = EBD-FN1
00212          PERFORM 125-ISSUE
00213          GO TO 000-MLS-100.
00214
00215      MOVE 06 TO HEX-BD.
00216      IF HEX-NBR = EBD-FN1
00217          PERFORM 150-FILE
00218          GO TO 000-MLS-100.
00219
00220      MOVE 08 TO HEX-BD.
00221      IF HEX-NBR = EBD-FN1
00222          PERFORM 175-TD
00223          GO TO 000-MLS-100.
00224
00225      MOVE 10 TO HEX-BD.
00226      IF HEX-NBR = EBD-FN1
00227          PERFORM 200-TS
00228          GO TO 000-MLS-100.
00229
00230      MOVE 12 TO HEX-BD.
00231      IF HEX-NBR = EBD-FN1
00232          PERFORM 225-CORE
00233          GO TO 000-MLS-100.
00234
00235      MOVE 14 TO HEX-BD.
00236      IF HEX-NBR = EBD-FN1
00237          PERFORM 250-ABEND
00238          GO TO 000-MLS-100.
00239
00240      MOVE 16 TO HEX-BD.
00241      IF HEX-NBR = EBD-FN1
00242          PERFORM 275-POST
00243          GO TO 000-MLS-100.
00244
00245      MOVE 18 TO HEX-BD.
00246      IF HEX-NBR = EBD-FN1
00247          PERFORM 300-ENQ
00248          GO TO 000-MLS-100.
00249
00250      MOVE 20 TO HEX-BD.
00251      IF HEX-NBR = EBD-FN1
00252          PERFORM 325-JOURNAL
00253          GO TO 000-MLS-100.
00254
00255      MOVE 24 TO HEX-BD.
00256      IF HEX-NBR = EBD-FN1
00257          PERFORM 350-MAP
00258          GO TO 000-MLS-100.
00259
00260      MOVE 30 TO HEX-BD.
00261      IF HEX-NBR = EBD-FN1
00262          PERFORM 375-ISS-QUERY
00263          GO TO 000-MLS-100.
00264
00265      MOVE 'UNDETERMINED ERROR' TO TM-MESG.
00266
00267  000-MLS-100.
00268      MOVE 17                     TO HEX-BD.
00269      MOVE HEX-NBR                TO TM-SBA.
00270 *    MOVE HEX-NX                 TO HEX-1.
00271      MOVE HEX-L21                TO HEX-1.
00272      MOVE HEX-1                  TO TM-FADDR.
00273      MOVE 29                     TO HEX-BD.
00274      MOVE HEX-NBR                TO TM-SF.
00275      MOVE '8'                    TO TM-ATTRB.
00276
00277      MOVE EIBTRMID               TO TMR3-A.
00278
00279      
      * EXEC CICS WRITEQ TD
00280 *         QUEUE   ('CSMT')
00281 *         FROM    (TM-MESG)
00282 *         LENGTH  (90)
00283 *         END-EXEC.
           MOVE 90
             TO DFHEIV11
           MOVE 'CSMT' TO DFHEIV5
      *    MOVE '(" L                  &   #00000371' TO DFHEIV0
           MOVE X'2822204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030333731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TM-MESG, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00284
00285      
      * EXEC CICS SYNCPOINT
00286 *         ROLLBACK
00287 *    END-EXEC.
      *    MOVE '6"R                   !   #00000377' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303030333737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00288
00289      MOVE W-EIBFN                TO EBD-FN.
00290      MOVE 14                     TO HEX-BD.
00291      IF HEX-NBR = EBD-FN1
00292         PERFORM 050-CK-SEND.
00293
00294      
      * EXEC CICS SEND
00295 *        FROM   (TERMINAL-MESSAGE)
00296 *        LENGTH (77) END-EXEC.
           MOVE 77
             TO DFHEIV11
      *    MOVE '$$                L F ,   #00000386' TO DFHEIV0
           MOVE X'242420202020202020202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303030333836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TERMINAL-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00297
pemuni*    IF WS-DUMP-SW NOT = ZERO
pemuni*        EXEC CICS DUMP
pemuni*            DUMPCODE  ('LGXX') TASK END-EXEC.
00301
00302      
      * EXEC CICS RETURN END-EXEC.
      *    MOVE '.(                    &   #00000394' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00303      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL004' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00304
00305  000-MLS-900-EXIT.
00306      EXIT.
00307
00308      EJECT
00309  050-CK-SEND SECTION.
00310
00311  050-CKS-10-NOTES.
00312      MOVE 02                     TO HEX-BD.
00313
pemuni*    IF WS-DUMP-SW NOT = ZERO
pemuni*        MOVE ZERO               TO WS-DUMP-SW
pemuni*        EXEC CICS DUMP DUMPCODE('LGXX') TASK END-EXEC.
00317
00318      IF HEX-NBR = EBD-FN2
00319         MOVE TM-MESG             TO DFHCOMMAREA
00320         
      * EXEC CICS RETURN END-EXEC.
      *    MOVE '.(                    &   #00000412' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030343132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00321
00322  050-CKS-900-EXIT.
00323      EXIT.
00324
00325      EJECT
00326  100-CICS-ERROR SECTION.
00327
00328  100-CES-10-NOTES.
00329 *
00330 *    THIS SECTION HANDLES THE ADDRESS OR ASSIGN ERROR
00331 *
00332      MOVE 'INVREQ FOR ADDRESS ASSIGN OR HANDLE CONDITION'
00333          TO TM-MESG.
00334
00335  100-CES-900-EXIT.
00336      EXIT.
00337
00338      EJECT
00339  125-ISSUE SECTION.
00340
00341  125-ISS-10-NOTES.
00342
00343 *
00344 *    THIS SECTION HANDLES CLASS 04 ERRORS
00345 *
00346      MOVE 02                     TO HEX-BD.
00347      IF HEX-NBR = EBD-FN2
00348          MOVE 'RECEIVE'          TO TMR1-A.
00349
00350      MOVE 04                     TO HEX-BD.
00351      IF HEX-NBR = EBD-FN2
00352          MOVE 'SEND'             TO TMR1-A.
00353
00354      MOVE 06                     TO HEX-BD.
00355      IF HEX-NBR = EBD-FN2
00356          MOVE 'CONVERSE'         TO TMR1-A.
00357
00358      MOVE 08                     TO HEX-BD.
00359      IF HEX-NBR = EBD-FN2
00360          MOVE 'ISSUE EODS'       TO TMR1-A.
00361
00362      MOVE 10                     TO HEX-BD.
00363      IF HEX-NBR = EBD-FN2
00364          MOVE 'ISSUE COPY'       TO TMR1-A.
00365
00366      MOVE 12                     TO HEX-BD.
00367      IF HEX-NBR = EBD-FN2
00368          MOVE 'WAIT TERMINAL'    TO TMR1-A.
00369
00370      MOVE 14                     TO HEX-BD.
00371      IF HEX-NBR = EBD-FN2
00372          MOVE 'ISSUE LOAD'       TO TMR1-A.
00373
00374      MOVE 16                     TO HEX-BD.
00375      IF HEX-NBR = EBD-FN2
00376          MOVE 'WAIT SIGNAL'      TO TMR1-A.
00377
00378      MOVE 18                     TO HEX-BD.
00379      IF HEX-NBR = EBD-FN2
00380          MOVE 'ISSUE RESET'      TO TMR1-A.
00381
00382      MOVE 20                     TO HEX-BD.
00383      IF HEX-NBR = EBD-FN2
00384          MOVE 'ISSUE DISCONNECT' TO TMR1-A.
00385
00386      MOVE 22                     TO HEX-BD.
00387      IF HEX-NBR = EBD-FN2
00388          MOVE 'ISSUE ENDOUTPUT'  TO TMR1-A.
00389
00390      MOVE 24                     TO HEX-BD.
00391      IF HEX-NBR = EBD-FN2
00392          MOVE 'ISSUE ERASEAUP'   TO TMR1-A.
00393
00394      MOVE 26                     TO HEX-BD.
00395      IF HEX-NBR = EBD-FN2
00396          MOVE 'ISSUE ENDFILE'    TO TMR1-A.
00397
00398      MOVE 28                     TO HEX-BD.
00399      IF HEX-NBR = EBD-FN2
00400          MOVE 'ISSUE PRINT'      TO TMR1-A.
00401
00402      MOVE 04                     TO HEX-BD.
00403      IF HEX-NBR = EBD-RC1
00404          MOVE 'EOF ERROR'        TO TMR1-B.
00405
00406      MOVE 16                     TO HEX-BD.
00407      IF HEX-NBR = EBD-RC1
00408          MOVE 'EODS ERROR'       TO TMR1-B.
00409
00410      MOVE 193                    TO HEX-BD.
00411      IF HEX-NBR = EBD-RC1
00412          MOVE 'EOF ERROR'        TO TMR1-B.
00413
00414      MOVE 194                    TO HEX-BD.
00415      IF HEX-NBR = EBD-RC1
00416          MOVE 'ENDINPT'          TO TMR1-B.
00417
00418      MOVE 225                    TO HEX-BD.
00419      IF HEX-NBR = EBD-RC1
00420          MOVE 'LENGERR'          TO TMR1-B.
00421
00422      MOVE 227                    TO HEX-BD.
00423      IF HEX-NBR = EBD-RC1
00424          MOVE 'WRBRK ERROR'      TO TMR1-B.
00425
00426      MOVE 228                    TO HEX-BD.
00427      IF HEX-NBR = EBD-RC1
00428          MOVE 'RDATT ERROR'      TO TMR1-B.
00429
00430      MOVE 229                    TO HEX-BD.
00431      IF HEX-NBR = EBD-RC1
00432          MOVE 'SIGNAL ERROR'     TO TMR1-B.
00433
00434      MOVE 230                    TO HEX-BD.
00435      IF HEX-NBR = EBD-RC1
00436          MOVE 'TERMIDERR'        TO TMR1-B.
00437
00438      MOVE 231                    TO HEX-BD.
00439      IF HEX-NBR = EBD-RC1
00440          MOVE 'NOPASSBKRD'       TO TMR1-B.
00441
00442      MOVE 232                    TO HEX-BD.
00443      IF HEX-NBR = EBD-RC1
00444          MOVE 'NOPASSBKWR'       TO TMR1-B.
00445
00446      MOVE 32                     TO HEX-BD.
00447      IF HEX-NBR = EBD-RC1
00448          MOVE 'EOC ERROR'        TO TMR1-B.
00449
00450      MOVE 64                     TO HEX-BD.
00451      IF HEX-NBR = EBD-RC1
00452          MOVE 'INBFMH ERROR'     TO TMR1-B.
00453
00454      MOVE 246                    TO HEX-BD.
00455      IF HEX-NBR = EBD-RC1
00456          MOVE 'NOSTART ERROR'    TO TMR1-B.
00457
00458      MOVE 247                    TO HEX-BD.
00459      IF HEX-NBR = EBD-RC1
00460          MOVE 'NONVAL ERROR'     TO TMR1-B.
00461
00462  125-ISS-900-EXIT.
00463
00464      EXIT.
00465
00466      EJECT
00467  150-FILE SECTION.
00468
00469  150-FLE-10-NOTES.
00470 *
00471 *    THIS SECTION HANDLES FILE ERRORS
00472 *
00473      MOVE SAVE-DS                TO TMR2-A.
00474
00475      MOVE 02                     TO HEX-BD.
00476      IF HEX-NBR = EBD-FN2 MOVE 'READ' TO TMR2-B.
00477
00478      MOVE 04                     TO HEX-BD.
00479      IF HEX-NBR = EBD-FN2 MOVE 'WRITE' TO TMR2-B.
00480
00481      MOVE 06                     TO HEX-BD.
00482      IF HEX-NBR = EBD-FN2 MOVE 'REWRITE' TO TMR2-B.
00483
00484      MOVE 08                     TO HEX-BD.
00485      IF HEX-NBR = EBD-FN2 MOVE 'DELETE' TO TMR2-B.
00486
00487      MOVE 10                     TO HEX-BD.
00488      IF HEX-NBR = EBD-FN2 MOVE 'UNLOCK' TO TMR2-B.
00489
00490      MOVE 12                     TO HEX-BD.
00491      IF HEX-NBR = EBD-FN2 MOVE 'STARTBR' TO TMR2-B.
00492
00493      MOVE 14                     TO HEX-BD.
00494      IF HEX-NBR = EBD-FN2 MOVE 'READNEXT' TO TMR2-B.
00495
00496      MOVE 16                     TO HEX-BD.
00497      IF HEX-NBR = EBD-FN2 MOVE 'READPREV' TO TMR2-B.
00498
00499      MOVE 18                     TO HEX-BD.
00500      IF HEX-NBR = EBD-FN2 MOVE 'ENDBR' TO TMR2-B.
00501
00502      MOVE 20                     TO HEX-BD.
00503      IF HEX-NBR = EBD-FN2 MOVE 'RESETBR' TO TMR2-B.
00504
00505      MOVE 01                     TO HEX-BD.
00506      IF HEX-NBR = EBD-RC1 MOVE 'DSIDERR' TO TMR2-C.
00507
00508      MOVE 02                     TO HEX-BD.
00509      IF HEX-NBR = EBD-RC1 MOVE 'ILLOGIC' TO TMR2-C.
00510
00511      MOVE 04                     TO HEX-BD.
00512      IF HEX-NBR = EBD-RC1 MOVE 'SEGIDERR' TO TMR2-C.
00513
00514      MOVE 08                     TO HEX-BD.
00515      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ'  TO TMR2-C.
00516
00517      MOVE 12                     TO HEX-BD.
00518      IF HEX-NBR = EBD-RC1 MOVE 'NOTOPEN' TO TMR2-C
00519         GO TO 150-FLE-900-EXIT.
00520
00521      MOVE 13                     TO HEX-BD.
00522      IF HEX-NBR = EBD-RC1 MOVE 'DISABLED' TO TMR2-C
00523         GO TO 150-FLE-900-EXIT.
00524
00525      MOVE 15                     TO HEX-BD.
00526      IF HEX-NBR = EBD-RC1 MOVE 'ENDFILE' TO TMR2-C.
00527
00528      MOVE 128                    TO HEX-BD.
00529      IF HEX-NBR = EBD-RC1 MOVE 'IOERR'   TO TMR2-C.
00530
00531      MOVE 129                    TO HEX-BD.
00532      IF HEX-NBR = EBD-RC1 MOVE 'NOTFND'  TO TMR2-C.
00533
00534      MOVE 130                    TO HEX-BD.
00535      IF HEX-NBR = EBD-RC1 MOVE 'DUPREC'  TO TMR2-C.
00536
00537      MOVE 131                    TO HEX-BD.
00538      IF HEX-NBR = EBD-RC1 MOVE 'NOSPACE' TO TMR2-C.
00539
00540      MOVE 132                    TO HEX-BD.
00541      IF HEX-NBR = EBD-RC1 MOVE 'DUPREC'  TO TMR2-C.
00542
00543      MOVE 208                    TO HEX-BD.
00544      IF HEX-NBR = EBD-RC1 MOVE 'SYSIDERR' TO TMR2-C.
00545
00546      MOVE 209                    TO HEX-BD.
00547      IF HEX-NBR = EBD-RC1 MOVE 'ISCINVREQ' TO TMR2-C.
00548
00549      MOVE 225                    TO HEX-BD.
00550      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR2-C.
00551
00552      MOVE +1                     TO  WS-DUMP-SW.
00553
00554  150-FLE-900-EXIT.
00555      EXIT.
00556
00557      EJECT
00558  175-TD SECTION.
00559
00560  175-TD-10-NOTES.
00561 *
00562 *    THIS SECTION HANDLES TRANSIENT DATA ERRORS
00563 *
00564      MOVE 02                     TO HEX-BD.
00565      IF HEX-NBR = EBD-FN2 MOVE 'WRITEQ TD' TO TMR1-A.
00566      MOVE 04                     TO HEX-BD.
00567      IF HEX-NBR = EBD-FN2 MOVE 'READQ TD' TO TMR1-A.
00568
00569      MOVE 06                     TO HEX-BD.
00570      IF HEX-NBR = EBD-FN2 MOVE 'DELETEQ TD' TO TMR1-A.
00571
00572      MOVE 01                     TO HEX-BD.
00573      IF HEX-NBR = EBD-RC1 MOVE 'QZERO' TO TMR1-B.
00574
00575      MOVE 02                     TO HEX-BD.
00576      IF HEX-NBR = EBD-RC1 MOVE 'QIDERR' TO TMR1-B.
00577
00578      MOVE 04                     TO HEX-BD.
00579      IF HEX-NBR = EBD-RC1 MOVE 'IOERR' TO TMR1-B.
00580
00581      MOVE 08                     TO HEX-BD.
00582      IF HEX-NBR = EBD-RC1 MOVE 'NOTOPEN' TO TMR1-B.
00583
00584      MOVE 16                     TO HEX-BD.
00585      IF HEX-NBR = EBD-RC1 MOVE 'NOSPACE' TO TMR1-B.
00586
00587      MOVE 192                    TO HEX-BD.
00588      IF HEX-NBR = EBD-RC1 MOVE 'QBUSY' TO TMR1-B.
00589
00590      MOVE 208                    TO HEX-BD.
00591      IF HEX-NBR = EBD-RC1 MOVE 'SYSIDERR' TO TMR1-B.
00592
00593      MOVE 209                    TO HEX-BD.
00594      IF HEX-NBR = EBD-RC1 MOVE 'ISCINVREQ' TO TMR1-B.
00595
00596      MOVE 225                    TO HEX-BD.
00597      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR1-B.
00598
00599  175-TDS-900-EXIT.
00600      EXIT.
00601
00602      EJECT
00603  200-TS SECTION.
00604
00605  200-TSS-10-NOTES.
00606 *
00607 *    THIS SECTION HANDLES TEMP STORAGE
00608 *
00609      MOVE 02                     TO HEX-BD.
00610      IF HEX-NBR = EBD-FN2 MOVE 'WRITEQ TS' TO TMR1-A.
00611
00612      MOVE 04                     TO HEX-BD.
00613      IF HEX-NBR = EBD-FN2 MOVE 'READQ TS' TO TMR1-A.
00614
00615      MOVE 06                     TO HEX-BD.
00616      IF HEX-NBR = EBD-FN2 MOVE 'DELETEQ TS' TO TMR1-A.
00617
00618      MOVE 01                     TO HEX-BD.
00619      IF HEX-NBR = EBD-RC1 MOVE 'ITEMERR' TO TMR1-B.
00620
00621      MOVE 02                     TO HEX-BD.
00622      IF HEX-NBR = EBD-RC1 MOVE 'QIDERR'  TO TMR1-B.
00623
00624      MOVE 04                     TO HEX-BD.
00625      IF HEX-NBR = EBD-RC1 MOVE 'IOERR'   TO TMR1-B.
00626
00627      MOVE 08                     TO HEX-BD.
00628      IF HEX-NBR = EBD-RC1 MOVE 'NOSPACE' TO TMR1-B.
00629
00630      MOVE 32                     TO HEX-BD.
00631      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ'  TO TMR1-B.
00632
00633      MOVE 208                    TO HEX-BD.
00634      IF HEX-NBR = EBD-RC1 MOVE 'SYSIDERR' TO TMR1-B.
00635
00636      MOVE 209                    TO HEX-BD.
00637      IF HEX-NBR = EBD-RC1 MOVE 'ISCINVREQ' TO TMR1-B.
00638
00639      MOVE 225                    TO HEX-BD.
00640      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR1-B.
00641
00642  200-TSS-900-EXIT.
00643      EXIT.
00644
00645      EJECT
00646  225-CORE SECTION.
00647
00648  225-CES-10-NOTES.
00649 *
00650 *    THIS SECTION HANDLES GETMAIN AND FREEMAIN
00651 *
00652      MOVE 02                     TO HEX-BD.
00653      IF HEX-NBR = EBD-FN2 MOVE 'GETMAIN' TO TMR1-A.
00654
00655      MOVE 04                     TO HEX-BD.
00656      IF HEX-NBR = EBD-FN2 MOVE 'FREEMAIN' TO TMR1-A.
00657
00658      MOVE 226                    TO HEX-BD.
00659      IF HEX-NBR = EBD-RC1 MOVE 'NOSTG  ' TO TMR1-B.
00660
00661  225-CDS-900-EXIT.
00662      EXIT.
00663
00664      EJECT
00665  250-ABEND SECTION.
00666
00667  250-ABS-10-NOTES.
00668 *
00669 *    THIS SECTION HANDLES ABEND CODES
00670 *
00671      MOVE 02                     TO HEX-BD.
00672      IF HEX-NBR = EBD-FN2 MOVE 'LINK' TO TMR1-A
00673      MOVE W-EIBRSRCE TO TMR2-B.
00674
00675      MOVE 04                     TO HEX-BD.
00676      IF HEX-NBR = EBD-FN2 MOVE 'XCTL' TO TMR1-A.
00677
00678      MOVE 06                     TO HEX-BD.
00679      IF HEX-NBR = EBD-FN2 MOVE 'LOAD' TO TMR1-A.
00680
00681      MOVE 08                     TO HEX-BD.
00682      IF HEX-NBR = EBD-FN2 MOVE 'RETURN' TO TMR1-A.
00683
00684      MOVE 10                     TO HEX-BD.
00685      IF HEX-NBR = EBD-FN2 MOVE 'RELEASE' TO TMR1-A.
00686
00687      MOVE 12                     TO HEX-BD.
00688      IF HEX-NBR = EBD-FN2 MOVE 'ABEND' TO TMR1-A.
00689
00690      MOVE 14                     TO HEX-BD.
00691      IF HEX-NBR = EBD-FN2 MOVE 'HANDLE ABEND' TO TMR1-A.
00692
00693      MOVE 01                     TO HEX-BD.
00694      IF HEX-NBR = EBD-RC1 MOVE 'PGMIDERR' TO TMR1-B.
00695
00696      MOVE 214                    TO HEX-BD.
00697      IF HEX-NBR = EBD-RC1 MOVE 'NOTAUTH ' TO TMR1-B.
00698
00699      MOVE 224                    TO HEX-BD.
00700      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ' TO TMR1-B.
00701
00702  250-ABS-900-EXIT.
00703      EXIT.
00704
00705      EJECT
00706  275-POST SECTION.
00707
00708  275-PTS-10-NOTES.
00709 *
00710 *    THIS SECTION HANDLES TIME RELATED CONDITIONS
00711 *
00712      MOVE 02                     TO HEX-BD.
00713      IF HEX-NBR = EBD-FN2 MOVE 'ASKTIME' TO TMR1-A.
00714
00715      MOVE 04                     TO HEX-BD.
00716      IF HEX-NBR = EBD-FN2 MOVE 'DELAY' TO TMR1-A.
00717
00718      MOVE 06                     TO HEX-BD.
00719      IF HEX-NBR = EBD-FN2 MOVE 'POST' TO TMR1-A.
00720
00721      MOVE 08                     TO HEX-BD.
00722      IF HEX-NBR = EBD-FN2 MOVE 'START' TO TMR1-A.
00723
00724      MOVE 10                     TO HEX-BD.
00725      IF HEX-NBR = EBD-FN2 MOVE 'RETRIEVE' TO TMR1-A.
00726
00727      MOVE 12                     TO HEX-BD.
00728      IF HEX-NBR = EBD-FN2 MOVE 'CANCEL'  TO TMR1-A.
00729
00730      MOVE 01                     TO HEX-BD.
00731      IF HEX-NBR = EBD-RC1 MOVE 'ENDDATA' TO TMR1-B.
00732
00733      MOVE 04                     TO HEX-BD.
00734      IF HEX-NBR = EBD-RC1 MOVE 'IOERR'   TO TMR1-B.
00735
00736      MOVE 17                     TO HEX-BD.
00737      IF HEX-NBR = EBD-RC1 MOVE 'TRANSIDERR' TO TMR1-B.
00738
00739      MOVE 18                     TO HEX-BD.
00740      IF HEX-NBR = EBD-RC1 MOVE 'TERMIDERR' TO TMR1-B.
00741
00742      MOVE 20                     TO HEX-BD.
00743      IF HEX-NBR = EBD-RC1 MOVE 'INVTSREQ' TO TMR1-B.
00744
00745      MOVE 32                     TO HEX-BD.
00746      IF HEX-NBR = EBD-RC1 MOVE 'EXPIRED' TO TMR1-B.
00747
00748      MOVE 129                    TO HEX-BD.
00749      IF HEX-NBR = EBD-RC1 MOVE 'NOTFND'  TO TMR1-B.
00750
00751      MOVE 208                    TO HEX-BD.
00752      IF HEX-NBR = EBD-RC1 MOVE 'SYSIDERR' TO TMR1-B.
00753
00754      MOVE 209                    TO HEX-BD.
00755      IF HEX-NBR = EBD-RC1 MOVE 'ISCINVRER' TO TMR1-B.
00756
00757      MOVE 225                    TO HEX-BD.
00758      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR1-B.
00759
00760      MOVE 233                    TO HEX-BD.
00761      IF HEX-NBR = EBD-RC1 MOVE 'ENVDEFERR' TO TMR1-B.
00762
00763      MOVE 255                    TO HEX-BD.
00764      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ'  TO TMR1-B.
00765
00766  275-PTS-900-EXIT.
00767      EXIT.
00768
00769      EJECT
00770  300-ENQ SECTION.
00771
00772  300-ENQ-10-NOTES.
00773 *
00774 *    THIS SECTION HANDLES WAIT EVENT, ENQ, DEQ, AND SUSPEND
00775 *
00776      MOVE 02                     TO HEX-BD.
00777      IF HEX-NBR = EBD-FN2 MOVE 'WAIT EVENT' TO TMR1-A.
00778
00779      MOVE 04                     TO HEX-BD.
00780      IF HEX-NBR = EBD-FN2 MOVE 'ENQ' TO TMR1-A.
00781
00782      MOVE 06                     TO HEX-BD.
00783      IF HEX-NBR = EBD-FN2 MOVE 'DEQ' TO TMR1-A.
00784
00785      MOVE 08                     TO HEX-BD.
00786      IF HEX-NBR = EBD-FN2 MOVE 'SUSPEND' TO TMR1-A.
00787
00788      MOVE 50                     TO HEX-BD.
00789      IF HEX-NBR = EBD-RC1 MOVE 'ENQBUSY' TO TMR1-B.
00790
00791  300-ENQ-900-EXIT.
00792      EXIT.
00793
00794      EJECT
00795  325-JOURNAL SECTION.
00796
00797  325-JUS-10-NOTES.
00798 *
00799 *    THIS SECTION HANDLES ALL JOURNAL ABENDS
00800 *
00801      MOVE 02                     TO HEX-BD.
00802      IF HEX-NBR = EBD-FN2 MOVE 'JOURNAL' TO TMR1-A.
00803
00804      MOVE 04                     TO HEX-BD.
00805      IF HEX-NBR = EBD-FN2 MOVE 'WAIT JOURNAL' TO TMR1-A.
00806
00807      MOVE 01                     TO HEX-BD.
00808      IF HEX-NBR = EBD-RC1 MOVE 'JIDERR' TO TMR1-B.
00809
00810      MOVE 02                     TO HEX-BD.
00811      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ' TO TMR1-B.
00812
00813      MOVE 05                     TO HEX-BD.
00814      IF HEX-NBR = EBD-RC1 MOVE 'NOTOPEN' TO TMR1-B.
00815
00816      MOVE 06                     TO HEX-BD.
00817      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR1-B.
00818
00819      MOVE 07                     TO HEX-BD.
00820      IF HEX-NBR = EBD-RC1 MOVE 'IOERR' TO TMR1-B.
00821
00822      MOVE 09                     TO HEX-BD.
00823      IF HEX-NBR = EBD-RC1 MOVE 'NOJBUFSP' TO TMR1-B.
00824
00825  325-JUS-900-EXIT.
00826      EXIT.
00827
00828      EJECT
00829  350-MAP SECTION.
00830
00831  350-MPS-10-NOTES.
00832 *
00833 *    THIS SECTION HANDLES ALL MAP ERRORS
00834 *
00835      MOVE 02                     TO HEX-BD.
00836      IF HEX-NBR = EBD-FN2 MOVE 'RECEIVE MAP' TO TMR1-A.
00837
00838      MOVE 04                     TO HEX-BD.
00839      IF HEX-NBR = EBD-FN2 MOVE 'SEND MAP' TO TMR1-A.
00840
00841      MOVE 06                     TO HEX-BD.
00842      IF HEX-NBR = EBD-FN2 MOVE 'SEND TEXT' TO TMR1-A.
00843
00844      MOVE 08                     TO HEX-BD.
00845      IF HEX-NBR = EBD-FN2 MOVE 'SEND PAGE' TO TMR1-A.
00846
00847      MOVE 10                     TO HEX-BD.
00848      IF HEX-NBR = EBD-FN2 MOVE 'PURGE MESSAGE' TO TMR1-A.
00849
00850      MOVE 12                     TO HEX-BD.
00851      IF HEX-NBR = EBD-FN2 MOVE 'ROUTE' TO TMR1-A.
00852
00853      MOVE 01                     TO HEX-BD.
00854      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ' TO TMR1-B.
00855
00856      MOVE 02                     TO HEX-BD.
00857      IF HEX-NBR = EBD-RC1 MOVE 'RETPAGE' TO TMR1-B.
00858
00859      MOVE 04                     TO HEX-BD.
00860      IF HEX-NBR = EBD-RC1 MOVE 'MAPFAIL' TO TMR1-B.
00861
00862      MOVE 08                     TO HEX-BD.
00863      IF HEX-NBR = EBD-RC1 MOVE 'INVMPSZ' TO TMR1-B.
00864
00865      MOVE 32                     TO HEX-BD.
00866      IF HEX-NBR = EBD-RC1 MOVE 'INVERRTERM' TO TMR1-B.
00867
00868      MOVE 64                     TO HEX-BD.
00869      IF HEX-NBR = EBD-RC1 MOVE 'RTESOME' TO TMR1-B.
00870
00871      MOVE 128                    TO HEX-BD.
00872      IF HEX-NBR = EBD-RC1 MOVE 'RTEFAIL' TO TMR1-B.
00873
00874      MOVE 227                    TO HEX-BD.
00875      IF HEX-NBR = EBD-RC1 MOVE 'WRBRK'  TO TMR1-B.
00876
00877      MOVE 228                    TO HEX-BD.
00878      IF HEX-NBR = EBD-RC1 MOVE 'RDATT' TO TMR1-B.
00879
00880      MOVE 16                     TO HEX-BD.
00881      IF HEX-NBR = EBD-RC2 MOVE 'INVLDC' TO TMR1-B.
00882
00883      MOVE 128                    TO HEX-BD.
00884      IF HEX-NBR = EBD-RC2 MOVE 'TSIOERR' TO TMR1-B.
00885
00886      MOVE 01                     TO HEX-BD.
00887      IF HEX-NBR = EBD-RC3 MOVE 'OVERFLOW' TO TMR1-B.
00888
00889  350-MPS-900-EXIT.
00890      EXIT.
00891
00892      EJECT
00893  375-ISS-QUERY SECTION.
00894
00895  375-ISS-10-NOTES.
00896 *
00897 *    THIS SECTION HANDLES ALL ISSUE ERRORS
00898 *
00899      MOVE 02                     TO HEX-BD.
00900      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE ADD' TO TMR1-A.
00901
00902      MOVE 04                     TO HEX-BD.
00903      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE ERASE' TO TMR1-A.
00904
00905      MOVE 06                     TO HEX-BD.
00906      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE REPLACE' TO TMR1-A.
00907
00908      MOVE 08                     TO HEX-BD.
00909      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE ABORT' TO TMR1-A.
00910
00911      MOVE 10                     TO HEX-BD.
00912      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE QUERY' TO TMR1-A.
00913
00914      MOVE 12                     TO HEX-BD.
00915      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE END' TO TMR1-A.
00916
00917      MOVE 14                     TO HEX-BD.
00918      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE RECEIVE' TO TMR1-A.
00919
00920      MOVE 16                     TO HEX-BD.
00921      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE NOTE' TO TMR1-A.
00922
00923      MOVE 18                     TO HEX-BD.
00924      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE WAIT' TO TMR1-A.
00925
00926      MOVE 04                     TO HEX-BD.
00927      IF HEX-NBR = EBD-RC3 MOVE 'EDOS' TO TMR1-B.
00928
00929      MOVE 08                     TO HEX-BD.
00930      IF HEX-NBR = EBD-RC3 MOVE 'EOC' TO TMR1-B.
00931
00932      MOVE 16                     TO HEX-BD.
00933      IF HEX-NBR = EBD-RC3 MOVE 'IGREQID' TO TMR1-B.
00934
00935      MOVE 04                     TO HEX-BD.
00936      IF HEX-NBR = EBD-RC1 MOVE 'DSSTAT' TO TMR1-B.
00937
00938      MOVE 08                     TO HEX-BD.
00939      IF HEX-NBR = EBD-RC1 MOVE 'FUNCERR' TO TMR1-B.
00940
00941      MOVE 12                     TO HEX-BD.
00942      IF HEX-NBR = EBD-RC1 MOVE 'SELNERR' TO TMR1-B.
00943
00944      MOVE 16                     TO HEX-BD.
00945      IF HEX-NBR = EBD-RC1 MOVE 'UNEXPIN' TO TMR1-B.
00946
00947      MOVE 225                    TO HEX-BD.
00948      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR1-B.
00949
00950      MOVE 17                     TO HEX-BD.
00951      IF HEX-NBR = EBD-RC2 MOVE 'EDOS' TO TMR1-B.
00952
00953      MOVE 32                     TO HEX-BD.
00954      IF HEX-NBR = EBD-RC3 MOVE 'EOC' TO TMR1-B.
00955
00956  375-ISS-900-EXIT.
00957      EXIT.
00958
00959      EJECT
00960  1000-CONVERT-HEX-CHARACTERS SECTION.
00961      IF WS-INDEX GREATER THAN +1
00962          MULTIPLY WS-INDEX BY +2 GIVING WS-INDEX2
00963        ELSE
00964          MOVE +1                 TO  WS-INDEX2.
00965
00966      MOVE WS-CHARACTER (WS-INDEX) TO WS-CHAR
00967
00968      IF WS-NUMBER = ZERO
00969          MOVE ZERO               TO  WS-ZONE (WS-INDEX)
00970                                      WS-DIGIT (WS-INDEX)
00971          GO TO 1099-EXIT.
00972
00973      DIVIDE WS-NUMBER BY +16 GIVING WS-NUMBER
00974          REMAINDER WS-REMAINDER
00975
00976      IF WS-NUMBER NOT = ZERO
00977          MOVE WS-HEX-CHAR (WS-NUMBER) TO  WS-ZONE (WS-INDEX)
00978        ELSE
00979          MOVE ZERO               TO  WS-ZONE (WS-INDEX).
00980
00981      IF WS-REMAINDER NOT = ZERO
00982          MOVE WS-HEX-CHAR (WS-REMAINDER) TO  WS-DIGIT (WS-INDEX)
00983        ELSE
00984          MOVE ZERO               TO  WS-DIGIT (WS-INDEX).
00985
00986  1099-EXIT.
00987      EXIT.
00988

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL004' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL004' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
