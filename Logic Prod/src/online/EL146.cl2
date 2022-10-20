00001  IDENTIFICATION DIVISION.                                         12/19/95
00002                                                                   EL146
00003  PROGRAM-ID.                 EL146 .                                 LV003
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 09/27/95 14:15:18.                    CL**3
00007 *                            VMOD=2.003                              CL**3
00008 *                                                                 EL146
00008 *                                                                 EL146
00009 *AUTHOR.    LOGIC, INC.                                              CL**3
00010 *           DALLAS, TEXAS.                                           CL**3
00025 *REMARKS.                                                         EL146
00026 *        THIS PROGRAM CONTROLS THE MAINTENANCE TO THE CHECK RECON-EL146
00027 *    CILIATION RECORD.                                            EL146
00028 *                                                                 EL146
00029 *    SCREENS     - EL146S - AUTOMATIC ACTIVITY MAINTENANCE        EL146
00030 *    ENTERED BY  - EL171A - ON-LINE REPORTS MENU                  EL146
00031 *                                                                 EL146
00032 *    EXIT TO     - EL171A - RESULT OF CLEAR                       EL146
00033 *                                                                 EL146
00034 *    INPUT FILES - ELRCON - CHECK RECONCILIATION FILE             EL146
00035 *                                                                 EL146
00036 *    OUTPUT FILES - ELRCON                                        EL146
00037 *                                                                 EL146
00038 *    COMMAREA    - PASSED.                                        EL146
00039                                                                   EL146
00040      EJECT                                                        EL146
00041  ENVIRONMENT DIVISION.                                            EL146
00042                                                                   EL146
00043  DATA DIVISION.                                                   EL146
00044                                                                   EL146
00045  WORKING-STORAGE SECTION.                                         EL146
00046                                                                   EL146
00047  77  FILLER  PIC X(32)  VALUE '********************************'. EL146
00048  77  FILLER  PIC X(32)  VALUE '*   EL146  WORKING STORAGE     *'. EL146
00049  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.003 *********'.    CL**3
00050                                                                   EL146
00051                                  COPY ELCSCTM.                    EL146
00052                                                                   EL146
00053                                  COPY ELCSCRTY.                   EL146
00054                                                                   EL146
00055  01  WS-DATE-AREA.                                                EL146
00056      12  SAVE-DATE               PIC X(08)       VALUE SPACES.    EL146
00057      12  SAVE-BIN-DATE           PIC X(02)       VALUE SPACES.    EL146
00058                                                                   EL146
00059  01  FILLER                      COMP-3.                          EL146
00060                                                                   EL146
00061      12  TIME-IN                 PIC S9(7)       VALUE ZERO.      EL146
00062      12  TIME-OUT                REDEFINES                        EL146
00063          TIME-IN                 PIC S9(3)V9(4).                  EL146
00064                                                                   EL146
00065  01  FILLER.                                                      EL146
00066      12  GETMAIN-SPACE           PIC X(01)       VALUE SPACES.    EL146
00067      12  ELRCON-DSID             PIC X(08)       VALUE 'ELRCON'.  EL146
00068                                                                   EL146
00069      12  LINK-ELDATCV            PIC X(08)       VALUE 'ELDATCV'. EL146
00070      12  LINK-001                PIC X(08)       VALUE 'EL001'.   EL146
00071      12  LINK-004                PIC X(08)       VALUE 'EL004'.   EL146
00072                                                                   EL146
00073      12  XCTL-005                PIC X(08)       VALUE 'EL005'.   EL146
00074      12  XCTL-010                PIC X(08)       VALUE 'EL010'.   EL146
00075      12  XCTL-126                PIC X(08)       VALUE 'EL126'.   EL146
00076                                                                   EL146
00077      12  WS-MAPSET-NAME          PIC X(08)       VALUE 'EL146S'.  EL146
00078      12  WS-MAP-NAME             PIC X(08)       VALUE 'EL146A'.  EL146
00079      12  FILLER                  REDEFINES                        EL146
00080          WS-MAP-NAME.                                             EL146
00081          16  FILLER              PIC X(02).                       EL146
00082          16  WS-MAP-NUMBER       PIC X(04).                       EL146
00083          16  FILLER              PIC X(02).                       EL146
00084                                                                   EL146
00085      12  WS-DATE-YY              PIC 9(04).                       EL146
00086                                                                   EL146
00087      12  WS-EDIT-DATE.                                            EL146
00088          16  WS-EDIT-MM          PIC 99.                          EL146
00089          16  FILLER              PIC X(01)  VALUE '/'.            EL146
00090          16  WS-EDIT-DD          PIC 99.                          EL146
00091          16  FILLER              PIC X(01)  VALUE '/'.            EL146
00092          16  WS-EDIT-YY          PIC 99.                          EL146
00093                                                                   EL146
00094      12  THIS-PGM                PIC X(08)       VALUE 'EL146'.   EL146
00095      12  WS-TRANS-ID             PIC X(04)       VALUE 'E027'.    EL146
00096      12  SC-ITEM                 PIC S9(04)      VALUE +1 COMP.   EL146
00097      12  SUB                     PIC S9(02)      VALUE +0.        EL146
00098      12  SUB1                    PIC S9(02)      VALUE +0.        EL146
00099      12  WS-EDIT-CHECK-AMT       PIC 9(07)V99.                    EL146
00100      12  WS-RECORDS-READ-SW      PIC X(01)       VALUE 'N'.       EL146
00101                                                                   EL146
00102      12  DEEDIT-FIELD            PIC X(12).                       EL146
00103      12  DEEDIT-FIELD-CHK REDEFINES DEEDIT-FIELD  PIC 9(10)V99.   EL146
00104                                                                   EL146
00105      12  WS-WORK-DATE.                                            EL146
00106          16  WS-WORK-MM          PIC 9(02)       VALUE ZEROS.     EL146
00107          16  WS-WORK-DD          PIC 9(02)       VALUE ZEROS.     EL146
00108          16  WS-WORK-YY          PIC 9(02)       VALUE ZEROS.     EL146
00109                                                                   EL146
00110      12  WS-RCON-DATE.                                            EL146
00111          16  WS-RCON-YEAR.                                        EL146
00112              20  WS-RCON-YY-1    PIC 9(02).                       EL146
00113              20  WS-RCON-YY-2    PIC 9(02).                       EL146
00114          16  WS-RCON-MM          PIC 9(02).                       EL146
00115          16  WS-RCON-DD          PIC 9(02).                       EL146
00116      EJECT                                                        EL146
00117  01  ACCESS-KEYS.                                                 EL146
00118                                                                   EL146
00119      12  ELRCON-KEY.                                              EL146
00120          16  RECON-COMPANY-CD    PIC X(01).                       EL146
00121          16  RECON-CHECK-NO      PIC X(07).                       EL146
00122          16  RECON-ORIGIN        PIC X(01).                       EL146
00123          16  RECON-BANK-NO       PIC X(10).                       EL146
00124                                                                   EL146
00125      EJECT                                                        EL146
00126  01  ERROR-MESSAGES.                                              EL146
00127      12  ER-0000                 PIC X(04)       VALUE '0000'.    EL146
00128      12  ER-0004                 PIC X(04)       VALUE '0004'.    EL146
00129      12  ER-0008                 PIC X(04)       VALUE '0008'.    EL146
00130      12  ER-0023                 PIC X(04)       VALUE '0023'.    EL146
00131      12  ER-0029                 PIC X(04)       VALUE '0029'.    EL146
00132      12  ER-0070                 PIC X(04)       VALUE '0070'.    EL146
00133      12  ER-0130                 PIC X(04)       VALUE '0130'.    EL146
00134      12  ER-0131                 PIC X(04)       VALUE '0131'.    EL146
00135      12  ER-0772                 PIC X(04)       VALUE '0772'.    EL146
00136      12  ER-0773                 PIC X(04)       VALUE '0773'.    EL146
00137      12  ER-0774                 PIC X(04)       VALUE '0774'.    EL146
00138      12  ER-0775                 PIC X(04)       VALUE '0775'.    EL146
00139      12  ER-0776                 PIC X(04)       VALUE '0776'.    EL146
00140      12  ER-0789                 PIC X(04)       VALUE '0789'.    EL146
00141      12  ER-0795                 PIC X(04)       VALUE '0795'.    EL146
00142      12  ER-0796                 PIC X(04)       VALUE '0796'.    EL146
00143      12  ER-0827                 PIC X(04)       VALUE '0827'.    EL146
00144      12  ER-0831                 PIC X(04)       VALUE '0831'.    EL146
00145      12  ER-0850                 PIC X(04)       VALUE '0850'.       CL**3
00146      12  ER-0898                 PIC X(04)       VALUE '0898'.    EL146
00147      12  ER-9999                 PIC X(04)       VALUE '9999'.    EL146
00148                                                                   EL146
00149      EJECT                                                        EL146
00150                                  COPY ELCINTF.                    EL146
00151      12  PI-REDEF    REDEFINES PI-PROGRAM-WORK-AREA.              EL146
00152          16  PI-PREV-KEY.                                         EL146
00153              20  PI-PREV-COMPANY-CD   PIC X(01).                  EL146
00154              20  PI-PREV-CHECK-NO     PIC X(07).                  EL146
00155              20  PI-PREV-ORGIN        PIC X(01).                  EL146
00156              20  PI-PREV-BANK-NO      PIC X(10).                  EL146
00157          16  PI-SAVE-KEY.                                         EL146
00158              20  PI-SAVE-KEY-OCCURS   OCCURS  08 TIMES.           EL146
00159                  24  PI-SAVE-COMPANY-CD   PIC X(01).              EL146
00160                  24  PI-SAVE-CHECK-NO     PIC X(07).              EL146
00161                  24  PI-SAVE-ORIGIN       PIC X(01).              EL146
00162                  24  PI-SAVE-BANK-NO      PIC X(10).              EL146
00163          16  PI-FIRST-TIME-SW             PIC X(01).              EL146
00164              88  FIRST-TIME                   VALUE 'Y'.          EL146
00165          16  PI-STOP-PAY-SW               PIC X(01).              EL146
00166              88  PI-STOP-PAY                  VALUE 'Y'.          EL146
00167          16  PI-STOP-PAY-KEY.                                     EL146
00168              20  PI-SPAY-COMPANY-CD   PIC X(01).                  EL146
00169              20  PI-SPAY-CHECK-NO     PIC X(07).                  EL146
00170              20  PI-SPAY-ORGIN        PIC X(01).                  EL146
00171              20  PI-SPAY-BANK-NO      PIC X(10).                  EL146
00172          16  FILLER                   PIC X(448).                    CL**3
00173      EJECT                                                        EL146
00174                                  COPY EL146S.                     EL146
00175  01  FILLER                      REDEFINES     
00176      EL146AI.                                                     EL146
020403     12  FILLER                            PIC X(35).             EL146
00178      12  EL146A-SCREEN.                                           EL146
020403         16  EL146A-TARGET-CHK-NO-LENGTH   PIC S9(04)  COMP.
020403         16  EL146A-TARGET-CHK-NO-ATTRB    PIC X(01).
020403         16  EL146A-TARGET-CHK-NO          PIC X(07).

020403         16  EL146A-BANK-NO-LENGTH         PIC S9(04)  COMP.
020403         16  EL146A-BANK-NO-ATTRB          PIC X(01).
020403         16  EL146A-BANK-NO                PIC X(10).

020403         16  EL146A-TARGET-CSH-AMT-LENGTH  PIC S9(04)  COMP. 
020403         16  EL146A-TARGET-CSH-AMT-ATTRB   PIC X(01).
020403         16  EL146A-TARGET-CSH-AMT         PIC Z,ZZZ,ZZ9.99.

020403         16  EL146A-CHK-NOTE-LENGTH        PIC S9(04)  COMP.
020403         16  EL146A-CHK-NOTE-ATTRB         PIC X(01).
020403         16  EL146A-CHK-NOTE               PIC X(67).

00179          16  EL146A-CHECK-DEFINED-ACTIVITY OCCURS 08 TIMES.       EL146
00180              20  EL146A-CHK-NO-LENGTH      PIC S9(04)  COMP. 
00181              20  EL146A-CHK-NO-ATTRB       PIC X(01).       
00182              20  EL146A-CHK-NO             PIC X(07).      
00183                                                                   EL146
00184              20  EL146A-CHK-DATE-LENGTH    PIC S9(04)  COMP. 
00185              20  EL146A-CHK-DATE-ATTRB     PIC X(01).       
00186              20  EL146A-CHK-DATE           PIC 9(08).      
00187                                                                   EL146
00188              20  EL146A-CHK-STAT-LENGTH    PIC S9(04)  COMP. 
00189              20  EL146A-CHK-STAT-ATTRB     PIC X(01).       
00190              20  EL146A-CHK-STAT           PIC X(01).      
00191                                                                   EL146
00192              20  EL146A-CHK-STDT-LENGTH    PIC S9(04)  COMP. 
00193              20  EL146A-CHK-STDT-ATTRB     PIC X(01).       
00194              20  EL146A-CHK-STDT           PIC 9(08).      
00195                                                                   EL146
00196              20  EL146A-CHK-AMT-LENGTH     PIC S9(04)  COMP. 
00197              20  EL146A-CHK-AMT-ATTRB      PIC X(01).       
00198              20  EL146A-CHK-AMT            PIC Z,ZZZ,ZZ9.99. 
00199                                                                   EL146
020403             20  EL146A-CHK-CSH-AMT-LENGTH PIC S9(04)  COMP.
020403             20  EL146A-CHK-CSH-AMT-ATTRB  PIC X(01).      
020403             20  EL146A-CHK-CSH-AMT        PIC Z,ZZZ,ZZ9.99.  
00203                                                                   EL146
020403             20  EL146A-CHK-CLM-NO-LENGTH  PIC S9(04)  COMP.  
020403             20  EL146A-CHK-CLM-NO-ATTRB   PIC X(01).        
020403             20  EL146A-CHK-CLM-NO         PIC X(07).       
00211                                                                   EL146
00211                                                                   EL146
00212      EJECT                                                        EL146
00213                                  COPY ELCEMIB.                    EL146
00214      EJECT                                                        EL146
00215                                  COPY ELCDATE.                    EL146
00216      EJECT                                                        EL146
00217                                  COPY ELCLOGOF.                   EL146
00218      EJECT                                                        EL146
00219                                  COPY ELCATTR.                    EL146
00220      EJECT                                                        EL146
00221                                  COPY ELCAID.                     EL146
00222  01  FILLER                      REDEFINES                        EL146
00223      DFHAID.                                                      EL146
00224                                                                   EL146
00225      12  FILLER                  PIC X(08).                       EL146
00226                                                                   EL146
00227      12  PF-VALUES               PIC X(01)                        EL146
00228          OCCURS 24 TIMES.                                         EL146
00229      EJECT                                                        EL146
00230  LINKAGE SECTION.                                                 EL146
00231                                                                   EL146
00232  01  DFHCOMMAREA                 PIC X(1024).                     EL146
00233                                                                   EL146
00234      EJECT                                                        EL146
00235                                  COPY ELCRCON.                    EL146
00236      EJECT                                                        EL146
00237  PROCEDURE DIVISION.                                              EL146
00238                                                                   EL146
00239      MOVE EIBDATE                TO  DC-JULIAN-YYDDD
00240      MOVE '5'                    TO  DC-OPTION-CODE
00241      PERFORM 8500-DATE-CONVERSION.                                EL146
00242      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE
00243      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE
00244                                                                   EL146
00245      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK
00246                                                                   EL146
00247      MOVE +2                     TO  EMI-NUMBER-OF-LINES
00248      MOVE '2'                    TO  EMI-SWITCH2
00249                                                                   EL146
00250      IF EIBCALEN IS EQUAL TO 0                                    EL146
00251          GO TO 8800-UNAUTHORIZED-ACCESS
020403     END-IF                                                       EL146

00253 *    NOTE ******************************************************* EL146
00254 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL146
00255 *         *  FROM ANOTHER MODULE.                               * EL146
00256 *         *******************************************************.EL146
00257                                                                   EL146
00258      EXEC CICS HANDLE CONDITION                                   EL146
00259          ERROR    (9990-ERROR)                                    EL146
00260          NOTOPEN  (8870-NOTOPEN)                                  EL146
00261          PGMIDERR (9600-PGMIDERR)                                 EL146
00262      END-EXEC

020403     .
00263                                                                   EL146
00264      EJECT                                                        EL146
00265  0010-MAIN-LOGIC.                                                 EL146
00266                                                                   EL146
00267      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL146
00268          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM  
00269              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00270              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5   
00271              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4  
00272              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3 
00273              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00274              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1    
00275              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM 
00276              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM  
00277          ELSE                                                      EL146
00278              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM   
00279              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00280              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1 
00281              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00282              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3    
00283              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4   
00284              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5  
00285              MOVE SPACES               TO  PI-SAVED-PROGRAM-6 
020403         END-IF
00286      ELSE                                                         EL146
00287          GO TO 0020-MAIN-LOGIC
020403     END-IF

020403     .
00288                                                                   EL146
00289  0015-MAIN-LOGIC.                                                 EL146
00290 *    NOTE ******************************************************* EL146
00291 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL146
00292 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL146
00293 *         *******************************************************.EL146
00294                                                                   EL146
00295      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
00296                                                                   EL146
00297      IF EIBTRNID NOT EQUAL WS-TRANS-ID                            EL146
00298          MOVE LOW-VALUES         TO  EL146AI            
00299          MOVE LOW-VALUES         TO  PI-PREV-KEY       
00300          MOVE LOW-VALUES         TO  ELRCON-KEY       
00301                                      PI-SAVE-KEY            
020403*                                    PI-STOP-PAY-KEY       
020403*        MOVE 'N'                TO  PI-STOP-PAY-SW  
00304                                      WS-RECORDS-READ-SW         
00305          MOVE 'Y'                TO  PI-FIRST-TIME-SW          
00306          MOVE PI-COMPANY-CD      TO  PI-PREV-COMPANY-CD       
00307          GO TO 8100-SEND-INITIAL-MAP
020403     END-IF

020403     .
00308                                                                   EL146
00309      EJECT                                                        EL146
00310  0020-MAIN-LOGIC.                                                 EL146
00311 *    NOTE ******************************************************* EL146
00312 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL146
00313 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL146
00314 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL146
00315 *         *******************************************************.EL146
00316                                                                   EL146
00317      IF EIBAID EQUAL DFHCLEAR                                     EL146
00318          GO TO 9400-CLEAR
020403     END-IF
00319                                                                   EL146
00323      EXEC CICS READQ TS       
00324              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL146
00325              INTO    (SECURITY-CONTROL)                           EL146
00326              LENGTH  (SC-COMM-LENGTH)                             EL146
00327              ITEM    (SC-ITEM)                                    EL146
00328      END-EXEC     

00329      MOVE SC-CLAIMS-DISPLAY (23)  TO  PI-DISPLAY-CAP    
00330      MOVE SC-CLAIMS-UPDATE  (23)  TO  PI-MODIFY-CAP    
00331      IF NOT DISPLAY-CAP                                    
00332          MOVE 'READ'              TO  SM-READ          
00333          PERFORM 9995-SECURITY-VIOLATION               
00334          MOVE ER-0070             TO  EMI-ERROR      
00335          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    
00336          GO TO 8100-SEND-INITIAL-MAP
020403     END-IF

020403     .
00337                                                                   EL146
00338  0200-RECEIVE.                                                    EL146
00339                                                                   EL146
00340      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3                   EL146
00341          MOVE LOW-VALUES         TO  EL146AI                      EL146
00342          MOVE ER-0008            TO  EMI-ERROR                    EL146
00343          PERFORM 8200-SEND-DATAONLY
020403     END-IF
00344                                                                   EL146
00345      EXEC CICS RECEIVE                                            EL146
00346          INTO   (EL146AI)                                         EL146
00347          MAPSET (WS-MAPSET-NAME)                                  EL146
00348          MAP    (WS-MAP-NAME)                                     EL146
00349      END-EXEC
00350                                                                   EL146
00351      IF ENTERPFL IS EQUAL TO +0                                   EL146
00352          GO TO 0300-CHECK-PFKEYS
020403     END-IF
00353                                                                   EL146
00354      IF EIBAID NOT = DFHENTER                                     EL146
00355          MOVE ER-0004            TO  EMI-ERROR                    EL146
00356          GO TO 0320-INPUT-ERROR
020403     END-IF
00357                                                                   EL146
00358      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)
00359          MOVE PF-VALUES (ENTERPFI)   TO  EIBAID                   EL146
00360      ELSE                                                         EL146
00361          MOVE ER-0029                TO  EMI-ERROR                EL146
00362          GO TO 0320-INPUT-ERROR
020403     END-IF

020403     .
00363                                                                   EL146
00364  0300-CHECK-PFKEYS.                                               EL146
00365                                                                   EL146
020403*    IF (EIBAID = DFHPF1)  OR        
020403*       (EIBAID = DFHPF2)  OR       
020403*       (EIBAID = DFHPF12) OR  
020403*       (EIBAID = DFHPF23) OR     
020403*       (EIBAID = DFHPF24)   
020403*        MOVE 'N' TO PI-STOP-PAY-SW
020403*    END-IF
00372                                                                   EL146
00373      IF EIBAID EQUAL DFHPF12                                      EL146
00374          MOVE XCTL-010                TO  THIS-PGM    
00375          GO TO 9300-XCTL
020403     END-IF
00376                                                                   EL146
00377      IF EIBAID EQUAL DFHPF23                                      EL146
00378          GO TO 9000-RETURN-CICS
020403     END-IF
00379                                                                   EL146
00380      IF EIBAID EQUAL DFHPF24                                      EL146
00381          MOVE XCTL-126                TO  THIS-PGM   
00382          GO TO 9300-XCTL
020403     END-IF
00383                                                                   EL146
00384      IF EIBAID EQUAL DFHPF1                                       EL146
00385          MOVE  +0                     TO  SUB   
00386          GO TO 4000-START-BROWSE-FORWARD
020403     END-IF
00387                                                                   EL146
00388      IF EIBAID EQUAL DFHPF2                                       EL146
00389          MOVE  +9                     TO  SUB  
00390          GO TO 5000-START-BROWSE-BACKWARD
020403     END-IF
00391                                                                   EL146
020403     IF EIBAID = DFHPF3 
020403         IF CHKNOL > +0
020403             MOVE  +0                 TO  SUB  
020403             GO TO 4000-START-BROWSE-FORWARD 
020403         ELSE
020403             MOVE AL-UABON            TO  MAINTA 
020403             MOVE  -1                 TO  MAINTL    
020403             MOVE  ER-0773            TO  EMI-ERROR 
020403             PERFORM 9900-ERROR-FORMAT THRU  9900-EXIT  
00430              GO TO 8200-SEND-DATAONLY
020403         END-IF
020403     END-IF
00391                                                                   EL146
00392      IF EIBAID EQUAL DFHENTER                                     EL146
00393          IF MAINTL IS EQUAL TO +0                                 EL146
00394              MOVE +0                  TO  SUB                     EL146
00395              GO TO 4000-START-BROWSE-FORWARD    
00396          ELSE                                                     EL146
00397              GO TO 0330-EDIT-DATA
020403         END-IF
020403     END-IF  

00399      MOVE ER-0029                TO  EMI-ERROR

020403     .
00400                                                                   EL146
00401  0320-INPUT-ERROR.                                                EL146
00402                                                                   EL146
00403      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00404      MOVE AL-UNBON               TO  ENTERPFA
00405                                                                   EL146
00406      IF ENTERPFL IS EQUAL TO +0                                   EL146
00407          MOVE -1                 TO  MAINTL                       EL146
00408      ELSE                                                         EL146
00409          MOVE -1                 TO  ENTERPFL
020403     END-IF
00410                                                                   EL146
00411      GO TO 8200-SEND-DATAONLY

020403     .
00412                                                                   EL146
00413  0330-EDIT-DATA.                                                  EL146
00414                                                                   EL146
00415      IF NOT  MODIFY-CAP                                           EL146
00416          MOVE 'UPDATE'         TO  SM-READ                        EL146
00417          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL146
00418          MOVE ER-0070          TO EMI-ERROR                       EL146
00419          PERFORM  9900-ERROR-FORMAT  THRU  9900-EXIT              EL146
00420          MOVE LOW-VALUES       TO EL146AO                         EL146
00421          GO TO 8100-SEND-INITIAL-MAP
020403     END-IF
00422                                                                   EL146
020403     IF (MAINTI IS EQUAL TO  'R' OR 'O' OR 'A' OR 'M') 
00424          CONTINUE
00425      ELSE                                                         EL146
00426          MOVE AL-UABON           TO  MAINTA                       EL146
00427          MOVE  -1                TO  MAINTL                       EL146
00428          MOVE  ER-0773           TO  EMI-ERROR                    EL146
00429          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL146
00430          GO TO 8200-SEND-DATAONLY
020403     END-IF
00431                                                                   EL146
020403     IF (CHKNOL > +0)
020403        AND (BANKNOL > +0)
020403         CONTINUE       
00436      ELSE                                                         EL146
00437          MOVE ER-0774           TO  EMI-ERROR    
00438          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT         
020403         MOVE -1                TO  CHKNOL    
020403         MOVE AL-UABON          TO  CHKNOA
00441          GO TO 8200-SEND-DATAONLY
020403     END-IF
00442                                                                   EL146
020403     IF CSHAMTL > +0                                              EL146
020403         MOVE CSHAMTI               TO DEEDIT-FIELD               EL146
00445          PERFORM 8000-DEEDIT        THRU 8000-EXIT                EL146
00446          IF DEEDIT-FIELD-CHK NUMERIC                              EL146
020403             MOVE DEEDIT-FIELD-CHK  TO CSHAMTO                    EL146
00448          ELSE                                                     EL146
020403             MOVE AL-UNBON          TO CSHAMTA                    EL146
020403             MOVE -1                TO CSHAMTL                    EL146
00451              MOVE ER-0775           TO EMI-ERROR                  EL146
00452              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
020403         END-IF
020403     END-IF
00462                                                                   EL146
00463       IF NOT EMI-NO-ERRORS                                        EL146
00464           GO TO 8200-SEND-DATAONLY                                EL146
00465       ELSE                                                        EL146
00466           GO TO 6000-REWRITE-CHECK-RECON
020403      END-IF

020403      .
00467                                                                   EL146
00468  4000-START-BROWSE-FORWARD.                                       EL146
00469                                                                   EL146
00470      MOVE  PI-COMPANY-CD         TO RECON-COMPANY-CD
00471                                                                   EL146
00472      IF CHKNOL > +0 
00473          MOVE CHKNOI             TO RECON-CHECK-NO
020403******** Origin is always 'C' indicating the check is from the 
020403******** claims system
020403         MOVE 'C'                TO RECON-ORIGIN

020403         IF EIBAID = DFHPF3
020403             MOVE SPACES         TO RECON-BANK-NO
020403         ELSE
00484              IF BANKNOL > +0 
00485                  MOVE BANKNOI    TO RECON-BANK-NO                 EL146
020403             ELSE
020403                 MOVE SPACES     TO RECON-BANK-NO
020403             END-IF
020403         END-IF

020403     ELSE
00491          IF PI-FIRST-TIME-SW IS EQUAL TO 'Y'    
00492              MOVE 'N'                        TO PI-FIRST-TIME-SW   
00493          ELSE                                                 
00494              IF PI-SAVE-CHECK-NO (8) = LOW-VALUES       
00495                  MOVE -1                     TO MAINTL          
00496                  MOVE ER-0130                TO EMI-ERROR      
00497                  PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT 
00498                  GO TO 8200-SEND-DATAONLY                
00499              ELSE                                       
00500                  MOVE PI-SAVE-COMPANY-CD (8) TO RECON-COMPANY-CD  EL146
00501                  MOVE PI-SAVE-CHECK-NO (8)   TO RECON-CHECK-NO    EL146
00502                  MOVE PI-SAVE-ORIGIN (8)     TO RECON-ORIGIN      EL146
00503                  MOVE PI-SAVE-BANK-NO (8)    TO RECON-BANK-NO     EL146
00504                  MOVE LOW-VALUES             TO PI-SAVE-KEY
020403             END-IF
020403         END-IF
020403     END-IF
00474                                                                   EL146
020403     .
00505                                                                   EL146
00506  4000-START-BROWSE.                                               EL146
00507                                                                   EL146
00508      EXEC CICS HANDLE CONDITION                                   EL146
00509          ENDFILE   (8700-END-FILE)                                EL146
00510          NOTFND    (8300-NOT-FOUND)                               EL146
00511      END-EXEC

00512      EXEC CICS STARTBR                                            EL146
00513          DATASET   ('ELRCON')                                     EL146
00514          RIDFLD    (ELRCON-KEY)                                   EL146
00515      END-EXEC

020403     .
00516                                                                   EL146
00517  4000-READ-NEXT.                                                  EL146
00518                                                                   EL146
00519      EXEC CICS READNEXT                                           EL146
00520          DATASET   ('ELRCON')                                     EL146
00521          SET       (ADDRESS OF CHECK-RECONCILIATION)                 CL**3
00522          RIDFLD    (ELRCON-KEY)                                   EL146
00523      END-EXEC
00524                                                                   EL146
00525      IF PI-COMPANY-CD NOT = RC-COMPANY-CD   
00526          GO TO 8700-END-FILE
020403     END-IF
00527                                                                   EL146
020403*    IF RC-CONTROL-PRIMARY = PI-PREV-KEY  
020403*        GO TO 4000-READ-NEXT
020403*    END-IF
00530                                                                   EL146
00531      MOVE 'N'                      TO  PI-FIRST-TIME-SW
00532      MOVE 'Y'                      TO  WS-RECORDS-READ-SW
00533                                                                   EL146
00534      ADD +1                        TO  SUB
020403     IF EIBAID = DFHPF3 AND SUB = +1 
020403        AND RC-CHECK-NO = CHKNOI
020403         MOVE RC-CHECK-NO          TO  EL146A-TARGET-CHK-NO
020403         MOVE RC-GL-ACCOUNT-NO     TO  EL146A-BANK-NO
020403         MOVE RC-CASHED-AMOUNT     TO  EL146A-TARGET-CSH-AMT
020403         MOVE RC-CHECK-NOTE        TO  EL146A-CHK-NOTE
020403     END-IF

00535      IF SUB > +8 
020403        AND EIBAID NOT = DFHPF3 
00536          MOVE +0                 TO  SUB                          EL146
00537          MOVE ELRCON-KEY         TO  PI-PREV-KEY                  EL146
020403         MOVE LOW-VALUES         TO  CHKNOO                       EL146
020403         MOVE ZEROS              TO  CSHAMTO                      EL146
020403         MOVE LOW-VALUES         TO  BANKNOO                      EL146
020403         MOVE LOW-VALUES         TO  NOTEO                        EL146
020403         MOVE LOW-VALUES         TO  MAINTO                       EL146
00543          GO TO 8100-SEND-INITIAL-MAP
020403     ELSE
020403         IF SUB > +8
020403            AND EIBAID = DFHPF3
020403            GO TO 8100-SEND-INITIAL-MAP
020403         END-IF
020403     END-IF
00544                                                                   EL146
00545      MOVE ELRCON-KEY             TO  PI-PREV-KEY
00546                                                                   EL146
00547      MOVE RC-COMPANY-CD          TO  PI-SAVE-COMPANY-CD (SUB)
020403     MOVE RC-CHECK-ORIGIN        TO  PI-SAVE-ORIGIN (SUB)
00548      MOVE RC-GL-ACCOUNT-NO       TO  PI-SAVE-BANK-NO (SUB)
00550      MOVE RC-CHECK-NO            TO  EL146A-CHK-NO (SUB)          EL146
00551                                      PI-SAVE-CHECK-NO (SUB)

00553      MOVE RC-ISSUE-YYYY          TO  WS-DATE-YY
00554      MOVE WS-DATE-YY             TO  WS-EDIT-YY
00555      MOVE RC-ISSUE-MM            TO  WS-EDIT-MM
00556      MOVE RC-ISSUE-DD            TO  WS-EDIT-DD
00557      MOVE WS-EDIT-DATE           TO  EL146A-CHK-DATE (SUB)
00558                                                                   EL146
00559      MOVE RC-STATUS              TO  EL146A-CHK-STAT (SUB)
00560                                                                   EL146
00561      MOVE RC-STATUS-YYYY         TO  WS-DATE-YY
00562      MOVE WS-DATE-YY             TO  WS-EDIT-YY
00563      MOVE RC-STATUS-MM           TO  WS-EDIT-MM
00564      MOVE RC-STATUS-DD           TO  WS-EDIT-DD
00565      MOVE WS-EDIT-DATE           TO  EL146A-CHK-STDT (SUB)
00566                                                                   EL146
00567      MOVE RC-CHECK-AMOUNT        TO  EL146A-CHK-AMT (SUB)
020403     MOVE RC-CASHED-AMOUNT       TO  EL146A-CHK-CSH-AMT (SUB)
020403     MOVE RC-CLAIM-NO            TO  EL146A-CHK-CLM-NO (SUB)

00576      GO TO 4000-READ-NEXT

020403     .
00577                                                                   EL146
00578  4000-EXIT.                                                       EL146
00579      EXIT.                                                        EL146
00580      EJECT                                                        EL146
00581  5000-START-BROWSE-BACKWARD.                                      EL146
00582                                                                   EL146
00583      EXEC CICS HANDLE CONDITION                                   EL146
00584          ENDFILE   (8710-END-FILE)                                EL146
00585          NOTFND    (8300-NOT-FOUND)                               EL146
00586      END-EXEC
00587                                                                   EL146
00588      MOVE  PI-COMPANY-CD                   TO RECON-COMPANY-CD
00589                                                                   EL146
00590      IF CHKNOL > +0 
00591          MOVE CHKNOI                       TO RECON-CHECK-NO
00596          IF BANKNOL > +0  
00597              MOVE BANKNOI                  TO RECON-BANK-NO  
00598          ELSE          
00599              MOVE SPACES                   TO RECON-BANK-NO
020403         END-IF
020403     ELSE
00603          IF PI-SAVE-CHECK-NO (1) = LOW-VALUES 
00604              IF PI-PREV-KEY = LOW-VALUES    
00605                  MOVE -1                   TO MAINTL      
00606                  MOVE ER-0131              TO EMI-ERROR  
00607                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT 
00608                  GO TO 8200-SEND-DATAONLY                         EL146
00609              ELSE                                                 EL146
00610                  MOVE PI-PREV-KEY          TO ELRCON-KEY   
020403             END-IF
00611          ELSE    
00612              MOVE PI-SAVE-COMPANY-CD (1)   TO RECON-COMPANY-CD  
00613              MOVE PI-SAVE-CHECK-NO (1)     TO RECON-CHECK-NO   
00614              MOVE PI-SAVE-ORIGIN (1)       TO RECON-ORIGIN   
00615              MOVE PI-SAVE-BANK-NO (1)      TO RECON-BANK-NO    
00616              MOVE LOW-VALUES               TO PI-SAVE-KEY
020403         END-IF
020403     END-IF
00592                                                                   EL146
00618      EXEC CICS STARTBR                                            EL146
00619          DATASET   ('ELRCON')                                     EL146
00620          RIDFLD    (ELRCON-KEY)                                   EL146
00621      END-EXEC

020403     .
00622                                                                   EL146
00623  5000-READ-PREV.                                                  EL146
00624                                                                   EL146
00625      EXEC CICS READPREV                                           EL146
00626          DATASET   ('ELRCON')                                     EL146
00627          SET       (ADDRESS OF CHECK-RECONCILIATION)                 CL**3
00628          RIDFLD    (ELRCON-KEY)                                   EL146
00629      END-EXEC
00630                                                                   EL146
00631      IF PI-COMPANY-CD NOT = RC-COMPANY-CD    
00632          GO TO 8710-END-FILE
020403     END-IF
00633                                                                   EL146
020403     IF RC-CONTROL-PRIMARY  EQUAL  PI-PREV-KEY                    EL146
020403         GO TO 5000-READ-PREV
020403     END-IF
00636                                                                   EL146
00637      MOVE 'Y'                    TO  WS-RECORDS-READ-SW
00638                                                                   EL146
00639      SUBTRACT +1                 FROM  SUB
00640      IF SUB IS LESS THAN  +1                                      EL146
00641          MOVE +0                 TO  SUB                          EL146
00642          MOVE LOW-VALUES         TO  CHKNOO                       EL146
00644          MOVE LOW-VALUES         TO  BANKNOO                      EL146
020403*        MOVE ZEROS              TO  CHKAMTO                      EL146
00646          MOVE ELRCON-KEY         TO  PI-PREV-KEY                  EL146
00647          GO TO 8100-SEND-INITIAL-MAP
020403     END-IF
00648                                                                   EL146
00649      MOVE ELRCON-KEY             TO  PI-PREV-KEY                  EL146

00651      MOVE RC-COMPANY-CD          TO  PI-SAVE-COMPANY-CD (SUB)
020403     MOVE RC-CHECK-ORIGIN        TO  PI-SAVE-ORIGIN (SUB)
00652      MOVE RC-GL-ACCOUNT-NO       TO  PI-SAVE-BANK-NO (SUB)
00653                                                                   EL146
00654      MOVE RC-CHECK-NO            TO  EL146A-CHK-NO (SUB)          EL146
00655                                      PI-SAVE-CHECK-NO (SUB)
00656                                                                   EL146
00657      MOVE RC-ISSUE-YYYY          TO  WS-DATE-YY
00658      MOVE WS-DATE-YY             TO  WS-EDIT-YY
00659      MOVE RC-ISSUE-MM            TO  WS-EDIT-MM
00660      MOVE RC-ISSUE-DD            TO  WS-EDIT-DD
00661      MOVE WS-EDIT-DATE           TO  EL146A-CHK-DATE (SUB)
00662                                                                   EL146
00663      MOVE RC-STATUS              TO  EL146A-CHK-STAT (SUB)
00664                                                                   EL146
00665      MOVE RC-STATUS-YYYY         TO  WS-DATE-YY
00666      MOVE WS-DATE-YY             TO  WS-EDIT-YY
00667      MOVE RC-STATUS-MM           TO  WS-EDIT-MM
00668      MOVE RC-STATUS-DD           TO  WS-EDIT-DD
00669      MOVE WS-EDIT-DATE           TO  EL146A-CHK-STDT (SUB)
00670                                                                   EL146
00671      MOVE RC-CHECK-AMOUNT        TO  EL146A-CHK-AMT (SUB)
020403     MOVE RC-CASHED-AMOUNT       TO  EL146A-CHK-CSH-AMT (SUB)
020403     MOVE RC-CLAIM-NO            TO  EL146A-CHK-CLM-NO (SUB)

00680      GO TO 5000-READ-PREV

020403     .
00681                                                                   EL146
00682  5000-EXIT.                                                       EL146
00683      EXIT.                                                        EL146
00684      EJECT                                                        EL146
00685  6000-REWRITE-CHECK-RECON.                                        EL146
00686                                                                   EL146
00687      EXEC CICS HANDLE CONDITION                                   EL146
00688          NOTFND    (8300-NOT-FOUND)                               EL146
00689      END-EXEC
00690                                                                   EL146
00691      MOVE  LOW-VALUES           TO  ELRCON-KEY
00692                                                                   EL146
00693      MOVE  PI-COMPANY-CD        TO  RECON-COMPANY-CD
00694      MOVE  CHKNOI               TO  RECON-CHECK-NO
020403     MOVE 'C'                   TO  RECON-ORIGIN
00699      MOVE BANKNOI               TO  RECON-BANK-NO
00700                                                                   EL146
00701      EXEC CICS READ                                               EL146
00702          DATASET     ('ELRCON')                                   EL146
00703          SET         (ADDRESS OF CHECK-RECONCILIATION)               CL**3
00704          RIDFLD      (ELRCON-KEY)                                 EL146
00705          UPDATE                                                   EL146
00706      END-EXEC
00707                                                                   EL146
00708      MOVE ELRCON-KEY         TO  PI-PREV-KEY
00709                                                                   EL146
00710      MOVE  SAVE-BIN-DATE     TO  RC-LAST-MAINT-DT
00711      MOVE  EIBTIME           TO  RC-LAST-MAINT-HHMMSS
00712      MOVE  PI-PROCESSOR-ID   TO  RC-LAST-MAINT-BY
00713                                                                   EL146
020403     IF (MAINTI = 'O')     
020403        AND (RC-STATUS NOT = 'R' AND 'A')  
00726          MOVE ER-0795                    TO EMI-ERROR             EL146
00727          MOVE -1                         TO MAINTL                   CL**3
00728          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT              CL**3
00729          GO TO 8200-SEND-DATAONLY
020403     END-IF
00730                                                                      CL**3
00731      IF (MAINTI = 'R')   
020403        AND (RC-STATUS NOT = 'O' AND 'A' AND 'S') 
00733          MOVE ER-0850                    TO EMI-ERROR                CL**3
00734          MOVE -1                         TO MAINTL                EL146
00735          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT           EL146
00736          GO TO 8200-SEND-DATAONLY
020403     END-IF
00737                                                                   EL146
020403     IF (MAINTI = 'A')   
020403        AND (RC-STATUS NOT = 'O')   
020403         MOVE ER-0789                    TO EMI-ERROR                CL**3
020403         MOVE -1                         TO MAINTL                EL146
020403         PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT           EL146
020403         GO TO 8200-SEND-DATAONLY
020403     END-IF
00737                                                                   EL146
020403     IF MAINTI = 'M'   
020403         IF CSHAMTL > +0
020403            AND RC-STATUS NOT = 'R'   
020403             MOVE ER-0796                TO EMI-ERROR                CL**3
020403             MOVE -1                     TO MAINTL                EL146
020403             PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT           EL146
020403             GO TO 8200-SEND-DATAONLY
020403         END-IF
020403     END-IF
00737                                                                   EL146
020403*    IF MAINTI  IS EQUAL TO  'S'                                  EL146
020403*       IF RC-STATUS  IS EQUAL TO  'S' OR 'V'                     EL146
020403*           NEXT SENTENCE                                         EL146
020403*       ELSE                                                      EL146
020403*           MOVE ER-0796         TO  EMI-ERROR                    EL146
020403*           MOVE -1              TO  MAINTL                       EL146
020403*           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT            EL146
020403*           GO TO 8200-SEND-DATAONLY.                             EL146
00746                                                                   EL146
020403*    IF MAINTI IS EQUAL TO 'S'                                    EL146
020403*       IF PI-STOP-PAY-KEY NOT EQUAL TO  PI-PREV-KEY              EL146
020403*           MOVE 'N'          TO  PI-STOP-PAY-SW.                 EL146
00750                                                                   EL146
020403*    IF MAINTI IS EQUAL TO 'S'                                    EL146
020403*       IF PI-STOP-PAY-SW   IS EQUAL TO 'N'                       EL146
020403*           MOVE ER-0831      TO  EMI-ERROR                       EL146
020403*           MOVE -1           TO  MAINTL                          EL146
020403*           MOVE AL-UABON     TO  MAINTA                             CL**2
020403*           MOVE SPACES       TO  MAINTO                          EL146
020403*           MOVE 'Y'          TO  PI-STOP-PAY-SW                  EL146
020403*           MOVE PI-PREV-KEY  TO  PI-STOP-PAY-KEY                 EL146
020403*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL146
020403*           GO TO 8200-SEND-DATAONLY.                             EL146
00761                                                                   EL146
00762      IF MAINTI = 'R'       
00770          MOVE 'R'                        TO RC-STATUS
020403         MOVE RC-CHECK-AMOUNT            TO RC-CASHED-AMOUNT
020403     END-IF
00771                                                                   EL146
020403     IF (MAINTI = 'O')  
00773          MOVE  'O'                       TO RC-STATUS
020403         MOVE ZEROS                      TO RC-CASHED-AMOUNT
020403     END-IF

020403     IF (MAINTI = 'A')   
020403         MOVE  'A'                       TO RC-STATUS
020403     END-IF

020403     IF MAINTI = 'M'
020403        AND CSHAMTL > +0
020403         MOVE CSHAMTO                    TO RC-CASHED-AMOUNT
020403     END-IF 
00774                                                                   EL146
00775      MOVE EIBDATE                        TO DC-JULIAN-YYDDD
00776      MOVE '5'                            TO DC-OPTION-CODE
00777      PERFORM 8500-DATE-CONVERSION
00778      MOVE DC-GREG-DATE-1-MDY             TO WS-WORK-DATE
00779      IF WS-WORK-YY IS GREATER THAN 50                             EL146
00780          MOVE '19'            TO  WS-RCON-YY-1                    EL146
00781          MOVE WS-WORK-YY      TO  WS-RCON-YY-2                    EL146
00782          MOVE WS-WORK-MM      TO  WS-RCON-MM                      EL146
00783          MOVE WS-WORK-DD      TO  WS-RCON-DD                      EL146
00784      ELSE                                                         EL146
00785          MOVE '20'            TO  WS-RCON-YY-1                    EL146
00786          MOVE WS-WORK-YY      TO  WS-RCON-YY-2                    EL146
00787          MOVE WS-WORK-MM      TO  WS-RCON-MM                      EL146
00788          MOVE WS-WORK-DD      TO  WS-RCON-DD
020403     END-IF
00789                                                                   EL146
020403     IF MAINTI NOT = 'M'
00790          MOVE WS-RCON-DATE               TO RC-STATUS-DATE
020403     END-IF

020403     IF NOTEL > +0
020403         MOVE NOTEI                      TO RC-CHECK-NOTE
020403     END-IF
00791                                                                   EL146
00792      EXEC CICS REWRITE                                            EL146
00793          DATASET   (ELRCON-DSID)                                  EL146
00794          FROM      (CHECK-RECONCILIATION)                         EL146
00795      END-EXEC

020403     IF EIBAID = DFHPF3
020403         GO TO 6000-EXIT
           END-IF
00796                                                                   EL146
00693 *    MOVE PI-COMPANY-CD                  TO RECON-COMPANY-CD
00694 *    MOVE CHKNOI                         TO RECON-CHECK-NO
020403*    MOVE 'C'                            TO RECON-ORIGIN
00699 *    MOVE BANKNOI                        TO RECON-BANK-NO

020403     MOVE PI-SAVE-COMPANY-CD (1) TO  RECON-COMPANY-CD
020403     MOVE PI-SAVE-CHECK-NO (1)   TO  RECON-CHECK-NO
020403     MOVE PI-SAVE-ORIGIN (1)     TO  RECON-ORIGIN
020403     MOVE PI-SAVE-BANK-NO (1)    TO  RECON-BANK-NO
00801      MOVE LOW-VALUES             TO  PI-SAVE-KEY                  EL146
020403                                     EL146AO
020403*    MOVE 'N'                    TO  PI-STOP-PAY-SW

020403     MOVE RC-CHECK-NO            TO  EL146A-TARGET-CHK-NO
020403     MOVE RC-GL-ACCOUNT-NO       TO  EL146A-BANK-NO
020403     MOVE RC-CASHED-AMOUNT       TO  EL146A-TARGET-CSH-AMT
020403     MOVE RC-CHECK-NOTE          TO  EL146A-CHK-NOTE
00804                                                                   EL146
00805      GO TO 4000-START-BROWSE

020403     .
00806                                                                   EL146
00807  6000-EXIT.                                                       EL146
00808      EXIT.                                                        EL146
00809  8000-DEEDIT.                                                     EL146

00810      EXEC CICS BIF DEEDIT                                         EL146
00811          FIELD    (DEEDIT-FIELD)                                  EL146
00812          LENGTH   (12)                                            EL146
00813      END-EXEC

020403     .
00814                                                                   EL146
00815  8000-EXIT.                                                       EL146
00816      EXIT.                                                        EL146
00817      EJECT                                                        EL146
00818  8100-SEND-INITIAL-MAP.                                           EL146
00819                                                                   EL146
00820      MOVE -1                     TO  MAINTL
00821      MOVE EIBTIME                TO  TIME-IN
00822      MOVE SAVE-DATE              TO  DATEO
00823      MOVE TIME-OUT               TO  TIMEO
00824      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O
00825      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O
00826                                                                   EL146
00827      EXEC CICS SEND                 
00828          FROM   (EL146AI)         
00829          MAPSET (WS-MAPSET-NAME) 
00830          MAP    (WS-MAP-NAME)   
00831          CURSOR                
00832          ERASE                
00833      END-EXEC

00834      GO TO 9100-RETURN-TRAN

020403     .
00835                                                                   EL146
00836  8200-SEND-DATAONLY.                                              EL146
00837                                                                   EL146
00838      MOVE EIBTIME                TO  TIME-IN
00839      MOVE SAVE-DATE              TO  DATEO
00840      MOVE TIME-OUT               TO  TIMEO
00841      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O
00842      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O
00843                                                                   EL146
00844      EXEC CICS SEND DATAONLY                                      EL146
00845          FROM   (EL146AI)                                         EL146
00846          MAPSET (WS-MAPSET-NAME)                                  EL146
00847          MAP    (WS-MAP-NAME)                                     EL146
00848          CURSOR                                                   EL146
00849      END-EXEC
00850                                                                   EL146
00851      GO TO 9100-RETURN-TRAN

020403     .
00852                                                                   EL146
00853      EJECT                                                        EL146
00854  8300-NOT-FOUND.                                                  EL146
00855                                                                   EL146
020403*    MOVE AL-UABON                TO  CHKNOA  ORIGINA.            EL146
020403     MOVE AL-UABON                TO  CHKNOA
020403*    MOVE AL-UNBON                TO  CHKAMTA
00858      MOVE -1                      TO  ENTERPFL
00859      MOVE ER-0772                 TO  EMI-ERROR
00860      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00861      GO TO 8200-SEND-DATAONLY

020403     .
00862                                                                   EL146
00863  8300-EXIT.                                                       EL146
00864      EXIT.                                                        EL146
00865  8500-DATE-CONVERSION.                                            EL146
00866                                                                   EL146
00867      EXEC CICS LINK                                               EL146
00868          PROGRAM  ('ELDATCV')                                     EL146
00869          COMMAREA (DATE-CONVERSION-DATA)                          EL146
00870          LENGTH   (DC-COMM-LENGTH)                                EL146
00871      END-EXEC

020403     .
00872                                                                   EL146
00873  8500-EXIT.                                                       EL146
00874      EXIT.                                                        EL146
00875                                                                   EL146
00876  8700-END-FILE.                                                   EL146

00877      MOVE -1                     TO ENTERPFL
00878      MOVE ER-0130                TO EMI-ERROR
00879      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00880      IF WS-RECORDS-READ-SW IS EQUAL TO 'Y'                        EL146
00881          GO TO 8100-SEND-INITIAL-MAP                              EL146
00882      ELSE                                                         EL146
00883          GO TO 8200-SEND-DATAONLY
020403     END-IF

020403     . 
00884                                                                   EL146
00885  8700-EXIT.                                                       EL146
00886      EXIT.                                                        EL146
00887                                                                   EL146
00888  8710-END-FILE.                                                   EL146
00889                                                                   EL146
00890      MOVE LOW-VALUES             TO PI-SAVE-CHECK-NO (SUB)        EL146
00891      MOVE -1                     TO ENTERPFL
00892      MOVE ER-0131                TO EMI-ERROR
00893      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00894                                                                   EL146
00895      IF WS-RECORDS-READ-SW IS EQUAL TO 'Y'                        EL146
00896          GO TO 8100-SEND-INITIAL-MAP                              EL146
00897      ELSE                                                         EL146
00898          GO TO 8200-SEND-DATAONLY
020403     END-IF

020403     .
00899                                                                   EL146
00900  8710-EXIT.                                                       EL146
00901      EXIT.                                                        EL146
00902  8800-UNAUTHORIZED-ACCESS.                                        EL146
00903                                                                   EL146
00904      MOVE UNACCESS-MSG           TO  LOGOFF-MSG
00905      GO TO 8850-SEND-TEXT

           .
00906                                                                   EL146
00907  8850-SEND-TEXT.                                                  EL146
00908                                                                   EL146
00909      EXEC CICS SEND TEXT                                          EL146
00910          FROM   (LOGOFF-TEXT)                                     EL146
00911          LENGTH (LOGOFF-LENGTH)                                   EL146
00912          ERASE  FREEKB                                            EL146
00913      END-EXEC
00914                                                                   EL146
00915      EXEC CICS RETURN                                             EL146
00916      END-EXEC

020403     .
00917                                                                   EL146
00918  8870-NOTOPEN.                                                    EL146
00919                                                                   EL146
00920      MOVE ER-0776                TO  EMI-ERROR
00921      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00922      MOVE -1                     TO  MAINTL
00923      GO TO 8200-SEND-DATAONLY

020403     .
00924                                                                   EL146
00925      EJECT                                                        EL146
00926  9000-RETURN-CICS.                                                EL146
00927                                                                   EL146
00928      MOVE XCTL-005               TO  THIS-PGM
00929      MOVE EIBAID                 TO  PI-ENTRY-CD-1
00930      GO TO 9300-XCTL

           .
00931                                                                   EL146
00932  9100-RETURN-TRAN.                                                EL146
00933                                                                   EL146
00934      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO
00935      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO
00936                                                                   EL146
00937      EXEC CICS RETURN                                             EL146
00938          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL146
00939          LENGTH   (PI-COMM-LENGTH)                                EL146
00940          TRANSID  (WS-TRANS-ID)                                   EL146
00941      END-EXEC

020403     .
00942                                                                   EL146
00943  9100-EXIT.                                                       EL146
00944      EXIT.                                                        EL146
00945                                                                   EL146
00946  9300-XCTL.                                                       EL146
00947                                                                   EL146
00948      MOVE DFHENTER               TO  EIBAID
00949                                                                   EL146
00950      EXEC CICS XCTL                                               EL146
00951          PROGRAM  (THIS-PGM)                                      EL146
00952          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL146
00953          LENGTH   (PI-COMM-LENGTH)                                EL146
00954      END-EXEC

020403     .
00955                                                                   EL146
00956  9300-EXIT.                                                       EL146
00957      EXIT.                                                        EL146
00958                                                                   EL146
00959      EJECT                                                        EL146
00960  9400-CLEAR.                                                      EL146
00961                                                                   EL146
00962      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM
00963      GO TO 9300-XCTL

           .
00964                                                                   EL146
00965  9600-PGMIDERR.                                                   EL146
00966                                                                   EL146
00967      EXEC CICS HANDLE CONDITION                                   EL146
00968          PGMIDERR (8200-SEND-DATAONLY)                            EL146
00969      END-EXEC
00970                                                                   EL146
00971      MOVE  THIS-PGM              TO  PI-CALLING-PROGRAM           EL146
00972                                      LOGOFF-PGM
00973                                                                   EL146
00974      MOVE XCTL-005               TO  THIS-PGM
00975      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL
00976      MOVE SPACES                 TO  PI-ENTRY-CD-1
00977      GO TO 9300-XCTL

020403     .
00978                                                                   EL146
00979  9900-ERROR-FORMAT.                                               EL146
00980                                                                   EL146
00981      MOVE LINK-001               TO  THIS-PGM
00982      EXEC CICS LINK                                               EL146
00983          PROGRAM  (THIS-PGM)                                      EL146
00984          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL146
00985          LENGTH   (EMI-COMM-LENGTH)                               EL146
00986      END-EXEC

020403     .
00987                                                                   EL146
00988  9900-EXIT.                                                       EL146
00989      EXIT.                                                        EL146
00990                                                                   EL146
00991  9990-ERROR.                                                      EL146
00992                                                                   EL146
00993      MOVE DFHEIBLK               TO  EMI-LINE1
00994      MOVE LINK-004               TO  THIS-PGM
00995      EXEC CICS LINK                                               EL146
00996          PROGRAM  (THIS-PGM)                                      EL146
00997          COMMAREA (EMI-LINE1)                                     EL146
00998          LENGTH   (72)                                            EL146
00999      END-EXEC
01000                                                                   EL146
01001      GO TO 8200-SEND-DATAONLY

020403     .
01002                                                                   EL146
01003      EJECT                                                        EL146
01004  9995-SECURITY-VIOLATION.                                         EL146
01005                              COPY ELCSCTP.                        EL146
01006                                                                   EL146
01007  9995-EXIT.                                                       EL146
01008      EXIT.                                                        EL146
01009                                                                   EL146
01010  9999-LAST-PARAGRAPH SECTION.                                     EL146
01011                                                                   EL146
01012      GOBACK.                                                      EL146
