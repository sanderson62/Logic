00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL1782
00003  PROGRAM-ID.                 EL1782.
00004 *              PROGRAM CONVERTED BY                                  CL*10
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*10
00006 *              CONVERSION DATE 02/13/96 09:36:39.                    CL*10
00007 *                            VMOD=2.011                              CL*11
00008 *                                                                 EL1782
00009 *AUTHOR.           LOGIC,INC.                                        CL*10
00010 *                  DALLAS,TEXAS.                                     CL*10
00011 *################################################################# 
      *
00024 *REMARKS. TRANSACTION EX56 - LETTER PRINTER.                         CL**3
00025 *        THIS PROGRAM IS USED TO PRINT THE STORED LETTERS AND        CL**3
00026 *        LABELS  DEPENDING ON THE VALUE OF THE PI-ENTRY-CODES.       CL**3
00027 *
00028 *        PRINT INITIAL LETTERS          CODE-1 = 1    
00029 *                                       CODE-2 = 1   
00030 * 
00031 *        PRINT FOLLOW-UP LETTERS        CODE-1 = 1  
00032 *                                       CODE-2 = 2 
00033 *
110402*        RE-PRINT LETTERS               CODE-1 = ' ' 
00035 *                                       CODE-2 = 3  
00036 *
110402*        PRINT ADDRESS LABELS           CODE-1 = ' '
00038 *                                       CODE-2 = 2 
00036 *
110402*        RE-PRINT LETTERS FOR A CLAIM   CODE-1 = ' '
110402*                                       CODE-2 = 4  
      *
00011 *################################################################# 
110402*
110402*                        C H A N G E   L O G
110402*-----------------------------------------------------------------
110402*  CHANGE   CHANGE REQUEST  PGMR  DESCRIPTION OF CHANGE
110402* EFFECTIVE    NUMBER
110402*-----------------------------------------------------------------
110402* 110402    2001031200008   SMVA  ADD REPRINT OF LETTERS FOR A  
110402*                                 SPECIFIC CLAIM 
020503* 020503                    SMVA  CLAIM LETTERS WOULD NOT CONTINUE
020503*                                 PRINTING AFTER BLANK INIT PRT 
020503*                                 DETECTED ON LTR W/ ARCHIVE #
110402******************************************************************
00039      EJECT                                                        EL1782
00040  ENVIRONMENT DIVISION.                                            EL1782
00041  DATA DIVISION.                                                   EL1782
00042  WORKING-STORAGE SECTION.                                         EL1782
00043  77  THIS-PGM PIC X(6)  VALUE 'EL1782'.                              CL*11
00044  77  FILLER  PIC X(32)  VALUE '********************************'. EL1782
00045  77  FILLER  PIC X(32)  VALUE '*   EL1782 WORKING STORAGE     *'. EL1782
00046  77  FILLER  PIC X(32)  VALUE '********* V/M 2.011 ************'.    CL*11
00047                                                                      CL**8
00048  01  WS-DATA-SHIFT-AREA.                                             CL**8
00049      05  WS-DATA-FIL         PIC X(7).                               CL**8
00050      05  WS-DATA-SHIFT       PIC X(125).                             CL**8
00051                                                                   EL1782
00052  01  WS-DATE-AREA.                                                EL1782
00053      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL1782
00054      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.            EL1782
00055                                                                   EL1782
00056  01  WS-CONSTANTS.                                                EL1782
00057      12  PGM-EL1782              PIC X(8)    VALUE 'EL1782'.         CL**8
00058      12  PGM-NAME                PIC X(8).                        EL1782
00059      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.        CL**8
00060      12  ARCH-ID                 PIC X(8)    VALUE 'ELARCH'.      EL1782
00061      12  ARCH-ID2                PIC X(8)    VALUE 'ELARCH2'.     EL1782
00062      12  ACTV-ID                 PIC X(8)    VALUE 'ELTRLR'.      EL1782
00063      12  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL'.      EL1782
CIDMOD     12  PI-CHECK-PRINTER-ID     PIC X(4)    VALUE SPACES.             000
00064                                                                   EL1782
00065      12  CNTL-KEY.                                                EL1782
00066          16  CNTL-CO             PIC X(3).                        EL1782
00067          16  CNTL-RECORD-TYPE    PIC X       VALUE '1'.           EL1782
00068          16  CNTL-GENL.                                           EL1782
00069            18 CNTL-GEN1          PIC XX      VALUE SPACES.        EL1782
00070            18 CNTL-GEN2.                                          EL1782
00071              20 CNTL-GEN3         PIC X       VALUE SPACES.       EL1782
00072              20 CNTL-GEN4         PIC X       VALUE SPACES.       EL1782
00073          16  CNTL-SEQ             PIC S9(4)   VALUE +0    COMP.   EL1782
00074                                                                   EL1782
00075      12  ARCH-KEY.                                                EL1782
00076          16  ARCH-PARTIAL-KEY.                                    EL1782
00077              20  ARCH-CO          PIC X.                          EL1782
00078              20  ARCH-NUMBER      PIC S9(8)      COMP.            EL1782
00079              20  ARCH-REC-TYPE    PIC X.                          EL1782
00080          16  ARCH-SEQ             PIC S9(4)      COMP VALUE +0.   EL1782
00081                                                                   EL1782
00082      12  ARCH-KEY2.                                               EL1782
00083          16  ARCH-PARTIAL-KEY2.                                   EL1782
00084              20  ARCH-CO2         PIC X.                          EL1782
00085              20  ARCH-REC-TYPE2   PIC X.                          EL1782
00086          16  ARCH-NUMBER2         PIC S9(8)      COMP.            EL1782
00087          16  ARCH-SEQ2            PIC S9(4)      COMP VALUE +0.   EL1782
00088                                                                   EL1782
00089      12  ACTV-KEY.                                                EL1782
110402         16  ACTV-PARTIAL-KEY.
00090              20  ACTV-CO          PIC X.                          EL1782
00091              20  ACTV-CARRIER     PIC X.                          EL1782
00092              20  ACTV-CLAIM       PIC X(7).                       EL1782
00093              20  ACTV-CERT-NO     PIC X(11).                      EL1782
00094          16  ACTV-SEQ             PIC S9(4)   COMP.               EL1782
00095                                                                   EL1782
110402     12  WS-SAVE-ACTV-PARTIAL-KEY PIC X(20).

110402     12  WS-ARCHIVE-NUM-TBL.
110402         16  WS-ARCHIVE-NUM      PIC S9(08)   COMP
110402                                              OCCURS 40 TIMES.

00096      12  CURRENT-SAVE            PIC XX.                          EL1782
00097      12  ARCH-SAVE-KEY           PIC X(6).                        EL1782
00098      12  ERROR-LINE              PIC X(80).                       EL1782
00099      12  TEXT-BROWSE-STARTED     PIC X VALUE 'N'.                 EL1782
00100      12  HEADER-BROWSE-STARTED   PIC X VALUE 'N'.                 EL1782
00101      12  WS-PRINT-SW   COMP-3    PIC S9              VALUE ZERO.  EL1782
00102      12  WS-LETTER-FORM          PIC X(4)            VALUE SPACES.EL1782
110402     12  SUB                     PIC S9(04) COMP     VALUE ZEROS. 
110402     12  WS-SAVE-FINAL-SUB       PIC S9(04) COMP     VALUE ZEROS.
00104      12  WS-SKIP                 PIC 99.                          EL1782
00105      12  WS-COPIES               PIC 9.                           EL1782
00106      12  TOP-FORM-SW             PIC X VALUE SPACE.               EL1782
00107          88  TOP-FORM-SET        VALUE 'T'.                       EL1782
00108                                                                   EL1782
00109      12  HEADER-SW               PIC X VALUE SPACE.               EL1782
00110          88  HEADER-REC-FOUND    VALUE SPACE.                     EL1782
00111                                                                   EL1782
00112      12  CORRESPOND-SW           PIC X VALUE SPACE.               EL1782
00113          88  CORR-REC-FOUND      VALUE SPACE.                     EL1782
00114                                                                   EL1782
00115      12  ADDR-SW                 PIC X VALUE SPACE.               EL1782
00116          88  ADDRESS-REC-FOUND   VALUE SPACE.                     EL1782
00117                                                                      CL**3
110402     12  WS-TRLR-BROWSE-SW       PIC X VALUE ' '.                    CL**3
110402         88  TRLR-BROWSE-STARTED       VALUE 'Y'.                    CL**3
110402         88  TRLR-BROWSE-ENDED         VALUE 'N'.                    CL**3
110402                                                                     CL**3
00118      12  END-BROWSE-SW           PIC X VALUE 'N'.                    CL**3
00119          88  BROWSE-ENDED              VALUE 'Y'.                    CL**3
00120                                                                      CL**3
00121      12  WS-RECORD-COUNT         PIC S9(4)   VALUE +0.               CL**3
00122                                                                      CL**3
00123      12  WS-DELAY-INTERVAL       PIC S9(7)   VALUE +2  COMP-3.       CL**3
00124                                                                   EL1782
00125      12  ARCHIVE-SAVE            PIC S9(8)   COMP.                EL1782
00126      12  OPTION-CODES            PIC XX.                          EL1782
00127          88  PRINT-INITIAL       VALUE '11'.                      EL1782
00128          88  PRINT-FOLLOW-UP     VALUE '12'.                      EL1782
00129          88  PRINT-LABELS        VALUE ' 2'.                      EL1782
00130          88  REPRINT-LETTERS     VALUE ' 3'.                      EL1782
00130          88  REPRINT-FOR-CLAIM   VALUE ' 4'.   
00131                                                                   EL1782
00132      12  WDS-PRINT-LINE.                                          EL1782
00133          16  FILLER              PIC X(6)    VALUE 'CLAIM-'.      EL1782
00134          16  WDS-CLAIM-NO        PIC X(7).                        EL1782
00135          16  FILLER              PIC X(7)    VALUE '  CERT-'.     EL1782
00136          16  WDS-CERT-NO         PIC X(11).                       EL1782
00137                                                                   EL1782
00138      12  J-ARCH-LENGTH           PIC S9(4)   COMP VALUE +113.     EL1782
00139      12  J-ACTV-LENGTH           PIC S9(4)   COMP VALUE +189.     EL1782
00140      12  JOURNAL-LENGTH          PIC S9(4)   COMP.                EL1782
00141                                                                   EL1782
00142      12  SAVE-ARCH-NO            PIC S9(8)   COMP VALUE +0.       EL1782
00143                                                                   EL1782
00144      12  WS-LABEL-HOLD-AREA.                                      EL1782
00145          16  WS-LABEL-LINES OCCURS 6 TIMES INDEXED BY L-INDX.     EL1782
00146            18  WS-LABEL-ZIP.                                      EL1782
00147              20  WS-LABEL-1ST-ZIP    PIC X(4).                    EL1782
00148              20  WS-LABEL-2ND-ZIP    PIC X(5).                    EL1782
00149            18  FILLER                PIC X(12).                   EL1782
00150            18  WS-LAST-ZIP.                                       EL1782
00151              20  WS-LAST-1ST-ZIP     PIC X(4).                    EL1782
00152              20  WS-LAST-2ND-ZIP     PIC X(5).                    EL1782
00153                                      COPY ELCDMD34.                  CL*11
00154      EJECT                                                        EL1782
00155                                      COPY ELCINTF.                   CL**8
00156                                                                      CL*11
00157      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL1782
00158          16  PI-PRINT-DATE       PIC X(8).                        EL1782
00159          16  PI-PRINT-DATE-BIN   PIC XX.                          EL1782
00160          16  PI-PRINT-ID         PIC X(4).                        EL1782
00161          16  PI-STARTING-ARCH-NO PIC S9(8) COMP.                  EL1782
00162          16  PI-PRINT-BY-CARR    PIC X.                              CL**2
00163              88  PRINT-BY-CARR             VALUE 'Y'.                CL**2
00164          16  PI-PRINT-CARRIER    PIC X.                              CL**2
00165          16  PI-LETTER-TYPE      PIC X.                              CL**5
00166          16  FILLER              PIC X(619).                         CL*10
00167      EJECT                                                        EL1782
00168                                  COPY ELCJPFX.                       CL**8
00169                                  PIC X(520).                      EL1782
00170      EJECT                                                        EL1782
00171                                  COPY ELPRTCVD.                      CL**8
00172      EJECT                                                        EL1782
00173                                  COPY ELCDATE.                       CL**8
00174      EJECT                                                        EL1782
00175  LINKAGE SECTION.                                                 EL1782
00176                              COPY ELCARCH.                           CL**8
00177      EJECT                                                        EL1782
00178                              COPY ELCTRLR.                           CL**8
00179      EJECT                                                        EL1782
00180                              COPY ELCCNTL.                           CL**8
00181      EJECT                                                        EL1782
00182  PROCEDURE DIVISION.                                              EL1782
00183                                                                   EL1782
00184      MOVE EIBDATE               TO DC-JULIAN-YYDDD
00185      MOVE '5'                   TO DC-OPTION-CODE
00186      PERFORM 9700-DATE-LINK     THRU 9700-EXIT
00187      MOVE DC-GREG-DATE-1-EDIT   TO SAVE-DATE
00188      MOVE DC-BIN-DATE-1         TO SAVE-BIN-DATE
00189      MOVE SPACES                TO DL34-PROCESS-TYPE

110402     .
00191                                                                   EL1782
00192  0100-RETRIEVE-LOOP.                                              EL1782

00193      EXEC CICS HANDLE CONDITION                                   EL1782
00194           ENDDATA(200-END-DATA)                                   EL1782
00195           NOTFND (300-NOT-FOUND)                                  EL1782
00196      END-EXEC
00197                                                                   EL1782
00198      EXEC CICS RETRIEVE                                           EL1782
00199           INTO  (PROGRAM-INTERFACE-BLOCK)                         EL1782
00200           LENGTH(PI-COMM-LENGTH)                                  EL1782
00201      END-EXEC
00202                                                                   EL1782
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*11
00205          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL*11
00206              MOVE 'O'                TO DL34-PROCESS-TYPE            CL*11
00207              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL*11
00208              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL*11
00209              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL*11
00210              MOVE SPACES             TO DL34-PRINT-LINE              CL*11
00211              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL*11
00212              EXEC CICS LINK                                          CL*11
00213                  PROGRAM    ('DLO034')                               CL*11
00214                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL*11
00215                  LENGTH     (DLO034-REC-LENGTH)                      CL*11
00216              END-EXEC                                                CL*11
00217              IF DL34-RETURN-CODE NOT = 'OK'                          CL*11
00218                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL*11
00219                                      TO ERROR-LINE                   CL*11
00220                  PERFORM 400-SEND-TEXT                               CL*11
00221                  EXEC CICS RETURN                                    CL*11
00222                  END-EXEC
110402             END-IF
110402         END-IF
110402     END-IF
00223                                                                      CL*11
00203                                                                        000
00224      PERFORM 1000-INITIALIZE         THRU 1000-EXIT
110402     
110402     IF REPRINT-FOR-CLAIM
110402         MOVE PI-PRINT-CARRIER       TO ACTV-CARRIER    
110402         MOVE PI-CLAIM-NO            TO ACTV-CLAIM  
110402         MOVE PI-CERT-NO             TO ACTV-CERT-NO   
110402         MOVE ZEROS                  TO ACTV-SEQ  
110402         MOVE ACTV-PARTIAL-KEY       TO WS-SAVE-ACTV-PARTIAL-KEY 

110402         EXEC CICS HANDLE CONDITION  
110402              NOTOPEN (8870-ACTV-NOT-OPEN)  
110402              NOTFND (8880-ACTV-REC-NOTFND)   
110402         END-EXEC
110402
110402         EXEC CICS STARTBR 
110402              DATASET (ACTV-ID) 
110402              RIDFLD (ACTV-KEY) 
110402         END-EXEC
110402                         
110402         SET TRLR-BROWSE-STARTED     TO TRUE

110402         EXEC CICS HANDLE CONDITION  
110402              ENDFILE (1200-READ-ARCH-NOW)
110402         END-EXEC

110402         PERFORM 1100-GET-ARCH-NUMS  THRU 1100-EXIT
110402             UNTIL TRLR-BROWSE-ENDED

110402         PERFORM 1200-READ-ARCH-NOW  THRU 1200-EXIT

110402****** Processing will never return here, there is a      
110402******   go to 200-end-data in 1200-read-arch-now because of the
110402******   possibility of an endfile handle cond branching there

110402     END-IF

00225      PERFORM 6000-BROWSE-ARCHIVE-HEADERS THRU 6099-EXIT

           .
00226                                                                   EL1782
00227  200-END-DATA.                                                    EL1782
00228                                                                      CL*11
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*11
00231          MOVE 'C'                TO DL34-PROCESS-TYPE                CL*11
00232          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID                  CL*11
00233          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID            CL*11
00234          MOVE PI-PROCESSOR-ID    TO DL34-USERID                      CL*11
00235          MOVE SPACES             TO DL34-PRINT-LINE                  CL*11
00236                                     DL34-OVERRIDE-PRINTER-ID         CL*11
00237          EXEC CICS LINK                                              CL*11
00238              PROGRAM    ('DLO034')                                   CL*11
00239              COMMAREA   (DLO034-COMMUNICATION-AREA)                  CL*11
00240              LENGTH     (DLO034-REC-LENGTH)                          CL*11
00241          END-EXEC                                                    CL*11
00242          IF DL34-RETURN-CODE NOT = 'OK'                              CL*11
00243              MOVE  '**DLO034 CLOSE ERROR - ABORT**'                  CL*11
00244                                  TO ERROR-LINE                       CL*11
00245              PERFORM 400-SEND-TEXT
110402         END-IF
110402     END-IF
00246                                                                      CL*11
00247      EXEC CICS RETURN                                             EL1782
00248      END-EXEC

110402     .
00249                                                                   EL1782
00250  300-NOT-FOUND.                                                   EL1782

00251      MOVE 'NO COMMUNICATION AREA FOUND' TO ERROR-LINE
00252      PERFORM 400-SEND-TEXT
00253      GO TO 200-END-DATA

110402     .
00254                                                                   EL1782
00255  400-SEND-TEXT.                                                   EL1782

00256      EXEC CICS SEND TEXT                                          EL1782
00257          FROM  (ERROR-LINE)                                       EL1782
00258          LENGTH(70)                                               EL1782
00259      END-EXEC

110402     .

00261  1000-INITIALIZE.                                                 EL1782

00262      MOVE +80                    TO WS-LINE-LEN
00263      MOVE SAVE-BIN-DATE          TO CURRENT-SAVE
00264                                                                   EL1782
00265      MOVE PI-COMPANY-CD          TO ARCH-CO2                      EL1782
00266                                     ACTV-CO                       EL1782
00267                                     ARCH-CO
00268                                                                   EL1782
00269      MOVE PI-ENTRY-CODES         TO OPTION-CODES

110402     .
00270                                                                   EL1782
110402 1000-EXIT.
110402     EXIT.

110402 1100-GET-ARCH-NUMS.
 
110402     EXEC CICS READNEXT  
110402          DATASET (ACTV-ID)     
110402          RIDFLD (ACTV-KEY) 
110402          SET (ADDRESS OF ACTIVITY-TRAILERS)  
110402     END-EXEC

110402     IF AT-CONTROL-PRIMARY (1:20) = WS-SAVE-ACTV-PARTIAL-KEY
110402         IF (CORRESPONDENCE-TR  AND
110402             AT-LETTER-ARCHIVE-NO > ZEROS) 
110402             ADD +1                    TO SUB
110402             MOVE AT-LETTER-ARCHIVE-NO TO WS-ARCHIVE-NUM (SUB)       
110402         END-IF
110402     ELSE
110402         SET TRLR-BROWSE-ENDED         TO TRUE
110402     END-IF

110402     .
110402 1100-EXIT.
110402     EXIT.

110402 1200-READ-ARCH-NOW.

110402     EXEC CICS ENDBR       
110402          DATASET(ACTV-ID) 
110402     END-EXEC

110402     IF SUB > +0 
110402         CONTINUE
110402     ELSE
110402         MOVE 'NO ARCHIVE LETTERS TO PRINT FOR THIS CLAIM' TO        
110402                                                       ERROR-LINE        
110402         PERFORM 400-SEND-TEXT
110402         GO TO 200-END-DATA
110402     END-IF
110402        
110402     MOVE SUB                            TO WS-SAVE-FINAL-SUB 
110402     MOVE +1                             TO SUB
110402     PERFORM 6000-BROWSE-ARCHIVE-HEADERS THRU 6099-EXIT
110402         UNTIL SUB > WS-SAVE-FINAL-SUB

110402     GO TO 200-END-DATA

110402     .
110402 1200-EXIT.
110402     EXIT.


00272  6000-BROWSE-ARCHIVE-HEADERS.                                     EL1782

00273      MOVE '1'                      TO ARCH-REC-TYPE2
00274      MOVE ZEROS                    TO ARCH-NUMBER2 
00275                                       ARCH-SEQ2
00276      IF PRINT-INITIAL OR PRINT-FOLLOW-UP         
00277          MOVE PI-STARTING-ARCH-NO  TO ARCH-NUMBER2
110402     END-IF

110402     IF REPRINT-FOR-CLAIM
110402         MOVE WS-ARCHIVE-NUM (SUB) TO ARCH-NUMBER2
110402         ADD +1                    TO SUB
110402     END-IF

110402     .
00278                                                                   EL1782
00279  6005-START-BROWSE.                                               EL1782

00280      EXEC CICS HANDLE CONDITION                                   EL1782
00281           NOTFND (6099-EXIT)                                      EL1782
00282           NOTOPEN(8860-ARCH2-NOT-OPEN)                            EL1782
00283      END-EXEC
00284                                                                   EL1782
00285      EXEC CICS STARTBR                                            EL1782
00286           DATASET(ARCH-ID2)                                       EL1782
00287           RIDFLD (ARCH-KEY2)                                      EL1782
00288      END-EXEC
00289                                                                   EL1782
00290      MOVE 'Y' TO HEADER-BROWSE-STARTED

110402     .
00291                                                                   EL1782
00292  6010-READ-NEXT.                                                  EL1782

00293      EXEC CICS HANDLE CONDITION                                   EL1782
00294           NOTFND  (6050-END-BR)                                   EL1782
00295           ENDFILE (6050-END-BR)                                   EL1782
00296           NOTOPEN (8860-ARCH2-NOT-OPEN)                           EL1782
00297      END-EXEC
00298                                                                   EL1782
00299      EXEC CICS READNEXT                                           EL1782
00300           DATASET (ARCH-ID2)                                      EL1782
00301           RIDFLD (ARCH-KEY2)          
00302           SET (ADDRESS OF LETTER-ARCHIVE) 
00303      END-EXEC
00304                                                                      CL**3
00305      IF (PI-COMPANY-CD = ARCH-CO2 AND   
00306          ARCH-REC-TYPE2 = '1')
110402         CONTINUE                                                 EL1782
110402*        NEXT SENTENCE                                            EL1782
00308      ELSE
00309          GO TO 6050-END-BR
110402     END-IF
00310                                                                      CL**2
00311      ADD +1                      TO WS-RECORD-COUNT
00312      IF WS-RECORD-COUNT IS GREATER THAN +50                          CL**3
00313          MOVE +0                 TO WS-RECORD-COUNT                  CL**3
00314          EXEC CICS DELAY                                             CL**3
00315              INTERVAL  (WS-DELAY-INTERVAL)                           CL**3
00316          END-EXEC
110402     END-IF
00317                                                                      CL**3
00318      IF PRINT-BY-CARR 
00319          IF LA-CARRIER IS EQUAL  TO PI-PRINT-CARRIER  
110402             CONTINUE                                                CL**2
110402*            NEXT SENTENCE                                           CL**2
00321          ELSE                                                        CL**2
00322              GO TO 6010-READ-NEXT
110402         END-IF
110402     END-IF

110402     IF REPRINT-FOR-CLAIM
110402         IF LA-CLAIM-NO = PI-CLAIM-NO
110402             IF LA-INITIAL-PRINT-DATE NOT = LOW-VALUES
110402                 MOVE ARCH-NUMBER2             TO ARCHIVE-SAVE       
110402                 PERFORM 8200-END-BROWSE THRU 8200-EXIT       
110402                 PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT 
110402                 MOVE 'N'                      TO END-BROWSE-SW      
020503             ELSE
020503                 PERFORM 8200-END-BROWSE THRU 8200-EXIT       
110402             END-IF
110402         END-IF
110402         GO TO 6099-EXIT
110402     END-IF
00323                                                                   EL1782
00324      MOVE ARCH-NUMBER2           TO ARCHIVE-SAVE
00325                                                                   EL1782
00326      MOVE SPACES                 TO WS-LETTER-FORM
00327                                                                   EL1782
00328      IF PRINT-INITIAL                                             EL1782
00329          PERFORM 6900-INITIAL-CHECKS THRU 6999-EXIT               EL1782
00330          IF BROWSE-ENDED                                             CL**3
00331              MOVE 'N'            TO END-BROWSE-SW                    CL**3
00332              ADD 1               TO ARCH-NUMBER2                     CL**3
00333              GO TO 6005-START-BROWSE                                 CL**3
00334          ELSE                                                        CL**3
00335              GO TO 6010-READ-NEXT
110402         END-IF
110402     END-IF
00336                                                                   EL1782
00337      IF PRINT-FOLLOW-UP                                           EL1782
00338          PERFORM 6100-FOLLOW-UP-CHECKS THRU 6199-EXIT             EL1782
CIDMOD         IF BROWSE-ENDED                                             CL**3
CIDMOD             MOVE 'N'            TO END-BROWSE-SW                    CL**3
CIDMOD             ADD 1               TO ARCH-NUMBER2                     CL**3
CIDMOD             GO TO 6005-START-BROWSE                                 CL**3
CIDMOD         ELSE                                                        CL**3
00344              GO TO 6010-READ-NEXT
110402         END-IF
110402     END-IF
00345                                                                   EL1782
00346      IF REPRINT-LETTERS                                           EL1782
00347         PERFORM 6200-REPRINT-CHECKS THRU 6299-EXIT                EL1782
CIDMOD         IF BROWSE-ENDED                                             CL**3
CIDMOD             MOVE 'N'            TO END-BROWSE-SW                    CL**3
CIDMOD             ADD 1               TO ARCH-NUMBER2                     CL**3
CIDMOD             GO TO 6005-START-BROWSE                                 CL**3
CIDMOD         ELSE                                                        CL**3
00353              GO TO 6010-READ-NEXT
110402         END-IF
110402     END-IF
00354                                                                   EL1782
00355      PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT
CIDMOD     IF BROWSE-ENDED                                                 CL**3
CIDMOD         MOVE 'N'                TO END-BROWSE-SW                    CL**3
CIDMOD         ADD 1                   TO ARCH-NUMBER2                     CL**3
CIDMOD         GO TO 6005-START-BROWSE                                     CL**3
CIDMOD     ELSE                                                            CL**3
00361          GO TO 6010-READ-NEXT
110402     END-IF
00362                                                                   EL1782
110402     .

00363  6050-END-BR.                                                     EL1782

00364      IF PRINT-LABELS                                              EL1782
00365         MOVE 'X'                 TO WS-PROG-END
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
110402     END-IF
00367                                                                   EL1782
00368      IF HEADER-BROWSE-STARTED = 'Y'                               EL1782
00369         MOVE 'N'                 TO HEADER-BROWSE-STARTED         EL1782
00370         EXEC CICS ENDBR                                           EL1782
00371              DATASET(ARCH-ID2)                                    EL1782
00372         END-EXEC
110402     END-IF

110402     .
00373                                                                   EL1782
00374  6099-EXIT.                                                       EL1782
00375       EXIT.                                                       EL1782
00376                                                                   EL1782
00378  6100-FOLLOW-UP-CHECKS.                                           EL1782

CIDMOD     IF LA-INITIAL-PRINT-DATE = LOW-VALUES AND                    EL1782
CIDMOD        SAVE-ARCH-NO = ZEROS                                      EL1782
CIDMOD          MOVE ARCH-NUMBER2 TO SAVE-ARCH-NO
110402     END-IF
00382                                                                   EL1782
00383      IF LA-RESEND-DATE = LOW-VALUES                               EL1782
00384         GO TO 6199-EXIT
110402     END-IF
00385                                                                   EL1782
00386      IF LA-RESEND-DATE NOT GREATER CURRENT-SAVE AND               EL1782
00387         LA-RESEND-PRINT-DATE = LOW-VALUES                         EL1782
110402        CONTINUE                                                  EL1782
110402*       NEXT SENTENCE                                             EL1782
00389      ELSE                                                         EL1782
00390         GO TO 6199-EXIT
110402     END-IF
00391                                                                      CL**3
CIDMOD     IF PI-PRINT-DATE-BIN NOT = LOW-VALUES                           CL**5
CIDMOD         IF LA-RESEND-DATE NOT = PI-PRINT-DATE-BIN                   CL**5
CIDMOD             GO TO 6199-EXIT
110402         END-IF
110402     END-IF 
CIDMOD                                                                     CL**5
CIDMOD                                                                     CL**5
CIDMOD     PERFORM 8200-END-BROWSE THRU 8200-EXIT
CIDMOD                                                                  EL1782
CIDMOD     IF SAVE-ARCH-NO = ZEROS                                      EL1782
CIDMOD        MOVE ARCH-NUMBER2 TO SAVE-ARCH-NO
110402     END-IF
CIDMOD                                                                  EL1782
00402      MOVE SPACES                 TO CORRESPOND-SW                 EL1782
00403                                     HEADER-SW
00404                                                                   EL1782
00405      PERFORM 8100-READ-HEADER THRU 8199-EXIT
00406                                                                   EL1782
00407      MOVE LA-NO-OF-COPIES        TO WS-COPIES
00408                                                                   EL1782
00409      PERFORM 8000-READ-CORRESPOND THRU 8099-EXIT
00410                                                                   EL1782
00411      IF AT-LETTER-ANSWERED-DT = LOW-VALUES AND                    EL1782
00412         AT-AUTO-RE-SEND-DT = LA-RESEND-DATE                       EL1782
00413         PERFORM 6500-UPDATE-RESEND-PRINT   THRU 6599-EXIT         EL1782
00414         PERFORM 6800-REWRITE-HEADER        THRU 6899-EXIT         EL1782
00415         PERFORM 6600-UPDATE-CORR-TRLR      THRU 6699-EXIT         EL1782
00416         PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT         EL1782
00417                 WS-COPIES TIMES                                   EL1782
00418         PERFORM 7500-SYNCPOINT             THRU 7599-EXIT            CL**6
00419         GO TO 6199-EXIT
110402     END-IF
00420                                                                   EL1782
00421      IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES                    EL1782
00422         MOVE LOW-VALUES          TO  LA-RESEND-DATE                  CL**9
00423                                      AT-AUTO-RE-SEND-DT              CL**9
CIDMOD*       PERFORM 6550-REWRITE-TRLR-NO-UP THRU 6550-EXIT                 000
CIDMOD        PERFORM 6800-REWRITE-HEADER        THRU 6899-EXIT         EL1782
CIDMOD        PERFORM 6600-UPDATE-CORR-TRLR      THRU 6699-EXIT            CL**9
00426         GO TO 6199-EXIT
110402     END-IF
00427                                                                   EL1782
00428      MOVE AT-AUTO-RE-SEND-DT     TO LA-RESEND-DATE
00429                                                                   EL1782
00430      IF LA-RESEND-DATE NOT GREATER CURRENT-SAVE                   EL1782
110402        CONTINUE                                                  EL1782
110402*       NEXT SENTENCE                                             EL1782
00432      ELSE
00433         PERFORM 6800-REWRITE-HEADER THRU 6899-EXIT                EL1782
00434         PERFORM 7900-RELEASE-CORR   THRU 7900-EXIT                EL1782
00435         GO TO 6199-EXIT
110402     END-IF
00436                                                                   EL1782
00437      PERFORM 6500-UPDATE-RESEND-PRINT   THRU 6599-EXIT
00438      PERFORM 6800-REWRITE-HEADER        THRU 6899-EXIT
00439      PERFORM 6600-UPDATE-CORR-TRLR      THRU 6699-EXIT
00440      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT
00441      PERFORM 7500-SYNCPOINT             THRU 7599-EXIT

110402     .
00442                                                                   EL1782
00443  6199-EXIT.                                                       EL1782
00444       EXIT.                                                       EL1782

00446  6200-REPRINT-CHECKS.                                             EL1782

00447      IF PI-LETTER-TYPE = 'I'                                         CL**5
00448          IF LA-RESEND-PRINT-DATE GREATER THAN LOW-VALUES             CL**5
00449              GO TO 6299-EXIT
110402         END-IF
110402     END-IF
00450                                                                      CL**5
00451      IF PI-LETTER-TYPE = 'P'                                         CL**5
00452          IF LA-RESEND-PRINT-DATE = LOW-VALUES                        CL**5
00453              GO TO 6299-EXIT
110402         END-IF
110402     END-IF
00454                                                                      CL**5
00455      IF LA-INITIAL-PRINT-DATE   = PI-PRINT-DATE-BIN OR            EL1782
00456         LA-RESEND-PRINT-DATE    = PI-PRINT-DATE-BIN               EL1782
00457         MOVE ARCH-NUMBER2        TO ARCHIVE-SAVE                  EL1782
00458         MOVE LA-NO-OF-COPIES     TO WS-COPIES                     EL1782
CIDMOD        PERFORM 8200-END-BROWSE THRU 8200-EXIT                       CL**3
00460         PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT         EL1782
00461                 WS-COPIES TIMES                                      CL**6
00462         PERFORM 7500-SYNCPOINT             THRU 7599-EXIT
110402     END-IF

110402     .
00463                                                                   EL1782
00464  6299-EXIT.                                                       EL1782
00465       EXIT.                                                       EL1782

00467  6300-LABEL-CHECKS.                                               EL1782

00468      IF FIRST-TIME                                                EL1782
00469         PERFORM 6400-ALIGNMENT-PRINT THRU 6450-EXIT
110402     END-IF
00470                                                                   EL1782
00471      MOVE SPACES TO WS-LABEL-HOLD-AREA
00472                                                                   EL1782
00473      IF LA-INITIAL-PRINT-DATE   = PI-PRINT-DATE-BIN OR            EL1782
00474         LA-RESEND-PRINT-DATE    = PI-PRINT-DATE-BIN               EL1782
00475          MOVE ARCH-NUMBER2        TO ARCHIVE-SAVE                 EL1782
CIDMOD         MOVE LA-CLAIM-NO         TO WDS-CLAIM-NO                 EL1782
CIDMOD         MOVE LA-CERT-NO          TO WDS-CERT-NO                  EL1782
CIDMOD         PERFORM 8200-END-BROWSE THRU 8200-EXIT                      CL**6
00479          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT           CL**6
00480          PERFORM 7500-SYNCPOINT             THRU 7599-EXIT
110402     END-IF

110402     .
00481                                                                   EL1782
00482  6399-EXIT.                                                       EL1782
00483       EXIT.                                                       EL1782

00485  6400-ALIGNMENT-PRINT.                                            EL1782

00486      MOVE ALL '*'                TO WS-LABEL-HOLD-AREA
00487      MOVE SPACES                 TO WS-PASSED-CNTL-CHAR           EL1782
00488                                     WS-LABEL-LINES (6)
00489                                                                   EL1782
00490      PERFORM 6480-MOVE-TO-PRINT THRU 6499-EXIT 6 TIMES
00491                                                                   EL1782
110402     .

00492  6450-EXIT.                                                       EL1782
00493       EXIT.                                                       EL1782
00494                                                                   EL1782
00495  6480-MOVE-TO-PRINT.                                              EL1782
 
00496      MOVE SPACES                 TO  WS-PASSED-CNTL-CHAR
00497      MOVE WS-LABEL-LINES (1)     TO WS-PASSED-DATA
00498      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00499      MOVE WS-LABEL-LINES (2)     TO WS-PASSED-DATA
00500      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00501      MOVE WS-LABEL-LINES (3)     TO WS-PASSED-DATA
00502      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00503      MOVE WS-LABEL-LINES (4)     TO WS-PASSED-DATA
00504      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00505      MOVE WS-LABEL-LINES (5)     TO WS-PASSED-DATA
00506      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00507      MOVE WS-LABEL-LINES (6)     TO WS-PASSED-DATA                EL1782
00508      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT

110402     . 
00509                                                                   EL1782
00510  6499-EXIT.                                                       EL1782
00511       EXIT.                                                       EL1782

00513  6500-UPDATE-RESEND-PRINT.                                        EL1782
00514                                                                      CL**9
00515      MOVE CURRENT-SAVE           TO  LA-RESEND-PRINT-DATE            CL**9
00516                                      LA-1ST-RESEND-PRINT-DT          CL**9
00517                                      AT-RESEND-PRINT-DATE
00518                                                                      CL**9
00519      MOVE LOW-VALUES             TO  LA-RESEND-DATE                  CL**9
00520                                      AT-AUTO-RE-SEND-DT
00521                                                                      CL**9
00522      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')                   CL**9
110402         CONTINUE
110402*        NEXT SENTENCE                                               CL**9
00524      ELSE                                                            CL**9
00525          GO TO 6599-EXIT
110402     END-IF
00526                                                                      CL**9
00527      MOVE LA-INITIAL-PRINT-DATE  TO  DC-BIN-DATE-1
00528      MOVE LA-RESEND-PRINT-DATE   TO  DC-BIN-DATE-2
00529      MOVE '1'                    TO  DC-OPTION-CODE
00530      MOVE +0                     TO  DC-ELAPSED-MONTHS               CL**9
00531                                      DC-ELAPSED-DAYS
00532      PERFORM 9700-DATE-LINK   THRU    9700-EXIT
00533                                                                      CL**9
00534      IF DC-ELAPSED-DAYS IS NOT EQUAL TO +30                          CL**9
00535          GO TO 6599-EXIT
110402     END-IF
00536                                                                      CL**9
00537      MOVE CURRENT-SAVE           TO  DC-BIN-DATE-1
00538      MOVE '6'                    TO  DC-OPTION-CODE
00539      MOVE +0                     TO  DC-ELAPSED-MONTHS
00540      MOVE +30                    TO  DC-ELAPSED-DAYS
00541      PERFORM 9700-DATE-LINK   THRU    9700-EXIT
00542      IF NO-CONVERSION-ERROR                                          CL**9
00543          MOVE DC-BIN-DATE-1      TO  LA-RESEND-DATE                  CL**9
00544                                      AT-AUTO-RE-SEND-DT              CL**9
00545          MOVE LOW-VALUES         TO  LA-RESEND-PRINT-DATE            CL**9
00546                                      AT-RESEND-PRINT-DATE
110402     END-IF

110402     .
00547                                                                   EL1782
00548  6599-EXIT.                                                       EL1782
00549       EXIT.                                                       EL1782

CIDMOD 6550-REWRITE-TRLR-NO-UP.                                              000

CIDMOD     IF CORR-REC-FOUND                                                 000
CIDMOD         CONTINUE                                                      000
CIDMOD     ELSE                                                              000
CIDMOD         GO TO 6550-EXIT                                               000
CIDMOD     END-IF
CIDMOD                                                                       000
CIDMOD     EXEC CICS REWRITE                                                 000
CIDMOD          DATASET (ACTV-ID)                                            000
CIDMOD          FROM    (ACTIVITY-TRAILERS)                                  000
CIDMOD     END-EXEC

110402     .

CIDMOD 6550-EXIT.                                                            000
CIDMOD     EXIT.                                                             000
CIDMOD                                                                       000
00551  6600-UPDATE-CORR-TRLR.                                           EL1782

00552      IF CORR-REC-FOUND                                            EL1782
110402         CONTINUE                                                 EL1782
110402*        NEXT SENTENCE                                            EL1782
00554      ELSE 
00555          GO TO 6699-EXIT
110402     END-IF
00556                                                                   EL1782
00557      EXEC CICS REWRITE                                            EL1782
00558           DATASET(ACTV-ID)                                        EL1782
00559           FROM(ACTIVITY-TRAILERS)                                 EL1782
00560      END-EXEC

110402     .
00561                                                                   EL1782
00562  6699-EXIT.                                                       EL1782
00563       EXIT.                                                       EL1782

00565  6800-REWRITE-HEADER.                                             EL1782

00566      EXEC CICS HANDLE CONDITION                                   EL1782
00567          DUPKEY (6899-EXIT)                                          CL*10
00568      END-EXEC
00569                                                                   EL1782
00570      EXEC CICS REWRITE                                            EL1782
00571           DATASET(ARCH-ID)                                        EL1782
00572           FROM   (LETTER-ARCHIVE)                                 EL1782
00573      END-EXEC

110402     .
00574                                                                   EL1782
00575  6899-EXIT.                                                       EL1782
00576       EXIT.                                                       EL1782

00578  6900-INITIAL-CHECKS.                                             EL1782

CIDMOD     IF LA-RESEND-DATE NOT   = LOW-VALUES AND                     EL1782
CIDMOD        LA-RESEND-PRINT-DATE = LOW-VALUES AND                     EL1782
CIDMOD        SAVE-ARCH-NO = ZEROS                                      EL1782
CIDMOD          MOVE ARCH-NUMBER2 TO SAVE-ARCH-NO
110402     END-IF
CIDMOD                                                                     CL**5
CIDMOD     IF PI-PRINT-DATE-BIN NOT = LOW-VALUES                           CL**5
CIDMOD         IF LA-CREATION-DT  NOT =  PI-PRINT-DATE-BIN                 CL**5
CIDMOD             GO TO 6999-EXIT
110402         END-IF
110402     END-IF
CIDMOD                                                                  EL1782
00588      IF LA-INITIAL-PRINT-DATE = LOW-VALUES                        EL1782
00589         MOVE ARCH-NUMBER2       TO ARCHIVE-SAVE                   EL1782
CIDMOD        PERFORM 8200-END-BROWSE THRU 8200-EXIT                       CL**3
00591         PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT         EL1782
00592         IF HEADER-REC-FOUND                                       EL1782
00593            PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT      EL1782
00594                    WS-COPIES TIMES                                EL1782
00595            PERFORM 7400-UPDATE-CORRESPOND-TRLR THRU 7499-EXIT        CL**9
00596            PERFORM 7500-SYNCPOINT             THRU 7599-EXIT
110402        END-IF
110402     END-IF

110402     .
00597                                                                   EL1782
00598  6999-EXIT.                                                       EL1782
00599       EXIT.                                                       EL1782
00600                                                                   EL1782
00602  7200-PRINT-ARCHIVE-RECORDS.                                      EL1782

00603      MOVE PI-COMPANY-CD          TO ARCH-CO
00604      MOVE SPACES                 TO WS-PROG-END
00605      MOVE ARCHIVE-SAVE           TO ARCH-NUMBER
00606                                                                   EL1782
00607      IF PRINT-LABELS                                              EL1782
00608         MOVE '2'                 TO ARCH-REC-TYPE                 EL1782
00609         SET L-INDX               TO 1                             EL1782
00610      ELSE
00611         MOVE '3'                 TO ARCH-REC-TYPE
110402     END-IF
00612                                                                   EL1782
00613      MOVE ZEROS                  TO ARCH-SEQ
00614                                                                   EL1782
00615      EXEC CICS HANDLE CONDITION                                   EL1782
00616           NOTFND  (7250-CHECK-FIRST-SW)                           EL1782
00617           ENDFILE (7250-CHECK-FIRST-SW)                           EL1782
00618           NOTOPEN (8890-ARCH-NOT-OPEN)                            EL1782
00619      END-EXEC
00620                                                                   EL1782
00621      EXEC CICS STARTBR                                            EL1782
00622           DATASET (ARCH-ID)                                       EL1782
00623           RIDFLD  (ARCH-KEY)                                      EL1782
00624      END-EXEC
00625                                                                   EL1782
00626      MOVE ARCH-PARTIAL-KEY       TO ARCH-SAVE-KEY
00627      MOVE 'Y'                    TO TEXT-BROWSE-STARTED
00628                                                                   EL1782
00629      MOVE '1'                    TO  WS-PRINT-AREA
00630      MOVE ZERO                   TO  WS-PRINT-SW

110402     .
00631                                                                   EL1782
00632  7210-READ-NEXT.                                                  EL1782

00633      EXEC CICS READNEXT                                           EL1782
00634           DATASET(ARCH-ID)                                        EL1782
00635           RIDFLD (ARCH-KEY)                                       EL1782
00636           SET (ADDRESS OF LETTER-ARCHIVE) 
00637      END-EXEC
00638                                                                   EL1782
00639      IF ARCH-PARTIAL-KEY NOT = ARCH-SAVE-KEY                      EL1782
00640         GO TO 7250-CHECK-FIRST-SW
110402     END-IF
00641                                                                   EL1782
00649      IF PRINT-LABELS                                              EL1782
00650         MOVE LA-ADDRESS-LINE     TO WS-LABEL-LINES (L-INDX)       EL1782
00651         SET L-INDX UP BY 1                                        EL1782
00652         GO TO 7210-READ-NEXT
110402     END-IF
00653                                                                   EL1782
110402*    IF PI-COMPANY-ID NOT = 'FIA'                                 EL1782
110402*        GO TO 7220-NOT-FIA
110402*    END-IF
00656                                                                   EL1782
110402*    IF WS-PRINT-SW NOT = ZERO                                    EL1782
110402*        GO TO 7220-NOT-FIA
110402*    END-IF
00659                                                                   EL1782
110402*    IF WS-LETTER-FORM NOT = 'EMPS'                               EL1782
110402*        GO TO 7220-NOT-FIA
110402*    END-IF
00662                                                                   EL1782
110402*    IF LA-TEXT-LINE  = SPACES                                    EL1782
110402*        GO TO 7210-READ-NEXT
110402*    END-IF
00665                                                                   EL1782
110402*    MOVE +1                     TO  WS-PRINT-SW

110402*7220-NOT-FIA.                                                    EL1782
110402*    IF PI-COMPANY-ID  = 'MON'                                       CL**8
110402*        MOVE SPACES                 TO WS-DATA-SHIFT-AREA           CL**8
110402*        MOVE LA-TEXT-LINE           TO WS-DATA-SHIFT                CL**8
110402*        MOVE WS-DATA-SHIFT-AREA     TO WS-PASSED-DATA               CL**8
110402*    ELSE
00674      MOVE LA-TEXT-LINE               TO WS-PASSED-DATA
110402*    END-IF
00675                                                                   EL1782
00676      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00677                                                                   EL1782
00678      IF SKIP-TO-NEXT-PAGE                                         EL1782
00679          MOVE '1'                TO  WS-PRINT-AREA                EL1782
00680      ELSE 
00681          MOVE SPACES             TO  WS-PRINT-AREA
110402     END-IF
00682                                                                   EL1782
00683      IF LA-SKIP-CONTROL GREATER '00' AND LESS '99'                EL1782
00684         MOVE SPACES              TO WS-PRINT-AREA                 EL1782
00685         MOVE LA-SKIP-CONTROL     TO WS-SKIP                       EL1782
00686         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT   WS-SKIP TIMES
110402     END-IF
00687                                                                   EL1782
00688      GO TO 7210-READ-NEXT

110402     .
00689                                                                   EL1782
00690  7250-CHECK-FIRST-SW.                                             EL1782

00691      IF PRINT-LABELS                                              EL1782
00692         GO TO 7260-LABEL-PRINT
110402     END-IF
00693                                                                   EL1782
00694      MOVE 'X'                 TO WS-PROG-END
CIDMOD     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00696                                                                   EL1782
00697      GO TO 7290-END-BR

110402     . 
00698                                                                   EL1782
00699  7260-LABEL-PRINT.                                                EL1782

00700      IF L-INDX = 1 OR                                             EL1782
00701         WS-LABEL-HOLD-AREA = SPACES                               EL1782
00702         GO TO 7290-END-BR
110402     END-IF
00703                                                                   EL1782
00704      PERFORM 6480-MOVE-TO-PRINT THRU 6499-EXIT
00705                                                                   EL1782
00706      GO TO 7290-END-BR

110402     .
00708                                                                   EL1782
00709  7290-END-BR.                                                     EL1782

00710      IF TEXT-BROWSE-STARTED = 'Y'                                 EL1782
00711         MOVE 'N'                 TO TEXT-BROWSE-STARTED           EL1782
00712         EXEC CICS ENDBR                                           EL1782
00713              DATASET(ARCH-ID)                                     EL1782
00714         END-EXEC
110402     END-IF
00715                                                                   EL1782
110402     .

00716  7299-EXIT.                                                       EL1782
00717       EXIT.                                                       EL1782

00718  7300-UPDATE-ARCHIVE-HEADER.                                      EL1782

CIDMOD     IF SAVE-ARCH-NO = ZEROS                                      EL1782
CIDMOD        MOVE ARCH-NUMBER2 TO SAVE-ARCH-NO
110402     END-IF
CIDMOD                                                                  EL1782
00722      PERFORM 8100-READ-HEADER THRU 8199-EXIT
00723                                                                   EL1782
00724      IF HEADER-REC-FOUND                                          EL1782
110402        CONTINUE                                                  EL1782
110402*       NEXT SENTENCE                                             EL1782
00726      ELSE
00727         GO TO 7399-EXIT
110402     END-IF
00728                                                                   EL1782
00729      MOVE CURRENT-SAVE           TO LA-INITIAL-PRINT-DATE
00730      MOVE LA-NO-OF-COPIES        TO WS-COPIES
00731                                                                   EL1782
00732      EXEC CICS HANDLE CONDITION                                   EL1782
00733          DUPKEY (7399-EXIT)                                          CL**9
00734      END-EXEC
00735                                                                   EL1782
00736      EXEC CICS REWRITE                                            EL1782
00737          DATASET (ARCH-ID)                                        EL1782
00738          FROM    (LETTER-ARCHIVE)                                 EL1782
00739      END-EXEC

110402     .
00740                                                                   EL1782
00741  7399-EXIT.                                                       EL1782
00742       EXIT.                                                       EL1782

00744  7400-UPDATE-CORRESPOND-TRLR.                                     EL1782

00745      MOVE SPACE                  TO CORRESPOND-SW
00746      PERFORM 8000-READ-CORRESPOND THRU 8099-EXIT
00747                                                                   EL1782
00748      IF CORR-REC-FOUND                                            EL1782
110402        CONTINUE                                                  EL1782
110402*       NEXT SENTENCE                                             EL1782
00750      ELSE
00751         GO TO 7499-EXIT
110402     END-IF
00752                                                                   EL1782
00753      MOVE CURRENT-SAVE           TO AT-INITIAL-PRINT-DATE
00754                                                                   EL1782
00755      EXEC CICS REWRITE                                            EL1782
00756          DATASET(ACTV-ID)                                         EL1782
00757          FROM(ACTIVITY-TRAILERS)                                  EL1782
00758      END-EXEC

110402     .
00759                                                                   EL1782
00760                                                                   EL1782
00761  7499-EXIT.                                                       EL1782
00762       EXIT.                                                       EL1782

00764  7500-SYNCPOINT.                                                     CL**6
00765                                                                      CL**6
00766      EXEC CICS SYNCPOINT                                             CL**6
00767           END-EXEC

110402     .
00768                                                                      CL**6
00769  7599-EXIT.                                                          CL**6
00770       EXIT.                                                          CL**6
00771                                                                      CL**6
CIDMOD 7900-RELEASE-CORR.                                               EL1782

CIDMOD     EXEC CICS UNLOCK                                             EL1782
CIDMOD         DATASET(ACTV-ID)                                         EL1782
CIDMOD     END-EXEC

110402     .
CIDMOD                                                                  EL1782
CIDMOD 7900-EXIT.                                                       EL1782
CIDMOD      EXIT.                                                       EL1782
CIDMOD                                                                  EL1782
00780  8000-READ-CORRESPOND.                                            EL1782

00781      EXEC CICS HANDLE CONDITION                                   EL1782
00782           NOTOPEN(8870-ACTV-NOT-OPEN)                             EL1782
00783           NOTFND (8050-REC-NOT-FOUND)                             EL1782
00784      END-EXEC
00785                                                                   EL1782
00786      EXEC CICS READ                                               EL1782
00787           DATASET(ACTV-ID)                                        EL1782
00788           RIDFLD (ACTV-KEY)                                       EL1782
00789           SET (ADDRESS OF ACTIVITY-TRAILERS)    
00790           UPDATE                                                  EL1782
00791      END-EXEC
00792                                                                   EL1782
00793      MOVE AT-STD-LETTER-FORM     TO  WS-LETTER-FORM
00794                                                                   EL1782
00795      GO TO 8099-EXIT

110402     .
00796                                                                   EL1782
00797  8050-REC-NOT-FOUND.                                              EL1782

00798      MOVE '1'                    TO CORRESPOND-SW

110402     .
00799                                                                   EL1782
00800  8099-EXIT.                                                       EL1782
00801       EXIT.                                                       EL1782

00803  8100-READ-HEADER.                                                EL1782

00804      MOVE ARCHIVE-SAVE           TO ARCH-NUMBER
00805      MOVE '1'                    TO ARCH-REC-TYPE
00806      MOVE ZEROS                  TO ARCH-SEQ
00807                                                                   EL1782
00808      EXEC CICS HANDLE CONDITION                                   EL1782
00809           NOTOPEN(8890-ARCH-NOT-OPEN)                             EL1782
00810           NOTFND (8150-NOT-FOUND)                                 EL1782
00811      END-EXEC
00812                                                                   EL1782
00813      EXEC CICS READ                                               EL1782
00814          DATASET (ARCH-ID)                                        EL1782
00815          RIDFLD  (ARCH-KEY)                                       EL1782
00816          SET     (ADDRESS OF LETTER-ARCHIVE)                         CL*10
00817          UPDATE                                                   EL1782
00818      END-EXEC
00819                                                                   EL1782
00820      MOVE LA-CARRIER             TO ACTV-CARRIER
00821      MOVE LA-CLAIM-NO            TO ACTV-CLAIM
00822      MOVE LA-CERT-NO             TO ACTV-CERT-NO
00823      MOVE LA-CORR-TRLR-SEQ       TO ACTV-SEQ
00824      GO TO 8199-EXIT

110402     . 
00825                                                                   EL1782
00826  8150-NOT-FOUND.                                                  EL1782

00827      MOVE '1'                    TO HEADER-SW

110402     .
00828                                                                   EL1782
00829  8199-EXIT.                                                       EL1782
00830       EXIT.                                                       EL1782
00831                                                                      CL**3
CIDMOD 8200-END-BROWSE.                                                    CL**3
CIDMOD                                                                     CL**3
CIDMOD     EXEC CICS ENDBR                                                 CL**3
CIDMOD         DATASET (ARCH-ID2)    
CIDMOD     END-EXEC
CIDMOD                                                                     CL**3
CIDMOD     MOVE 'Y'                    TO END-BROWSE-SW

110402     .
CIDMOD                                                                     CL**3
CIDMOD 8200-EXIT.                                                          CL**3
CIDMOD     EXIT.                                                           CL**3
00842                                                                   EL1782
00843  8860-ARCH2-NOT-OPEN.                                             EL1782

00844      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ELARCH2' TO ERROR-LINE
00845      PERFORM 400-SEND-TEXT
00846      GO TO 200-END-DATA

110402     .

00848  8870-ACTV-NOT-OPEN.                                              EL1782

00849      MOVE 'ACTIVITY TRAILER FILE NOT OPEN - ELTRLR' TO ERROR-LINE
00850      PERFORM 400-SEND-TEXT
00851      GO TO 200-END-DATA

110402     .
00852                                                                   EL1782
110402 8880-ACTV-REC-NOTFND. 

110402     MOVE 'ACTIVITY TRLR RECORD NOT FOUND FOR THIS CLAIM' TO
110402                                                       ERROR-LINE        
110402     PERFORM 400-SEND-TEXT
110402     GO TO 200-END-DATA

110402     .
00852                                                                   EL1782
00853  8890-ARCH-NOT-OPEN.                                              EL1782

00854      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ELARCH' TO ERROR-LINE
00855      PERFORM 400-SEND-TEXT
00856      GO TO 200-END-DATA

110402     .
00857                                                                   EL1782
00859  9700-DATE-LINK.                                                  EL1782

00860      MOVE LINK-ELDATCV TO PGM-NAME
00861      EXEC CICS LINK                                               EL1782
00862          PROGRAM (PGM-NAME)                                       EL1782
00863          COMMAREA(DATE-CONVERSION-DATA)                           EL1782
00864          LENGTH  (DC-COMM-LENGTH)                                 EL1782
00865      END-EXEC

110402     .
00866                                                                   EL1782
00867  9700-EXIT.                                                       EL1782
00868       EXIT.                                                       EL1782
00871                                                                      CL*11
CIDMOD                                 COPY ELPRTCVP.                        000
