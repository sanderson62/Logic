00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 ELRATE.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 03/05/96 16:20:51.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.012                           
00009                                                                   
00010 *AUTHOR.     LOGIC, INC.                                          
00011 *            DALLAS, TEXAS.                                       
00012                                                                   
00013 *DATE-COMPILED.                                                   
00014                                                                   
00015 *SECURITY.   *****************************************************
00016 *            *                                                   *
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00018 *            *                                                   *
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024                                                                   
00025 ******************************************************************
00026 *  REMARKS   *                                                   *
00027 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *
00028 *            *    OPTION SPECIFIED, COMPUTE PREMIUM AMOUNTS      *
00029 *            *    AND RATES.                                     *
00030 ******************************************************************
00031                                                                   
00032                                                                   
00033 ******************************************************************
00034 *                                                                *
00035 *              INPUT FIELDS USED                                 *
00036 *                                                                *
00037 ******************************************************************
00038 *  ORIGINAL TERM    - CP-ORIGINAL-TERM    (RATING TERM)          *
00039 *  CLASS CODE       - CP-CLASS-CODE                              *
00040 *  DEVIATION CODE   - CP-DEVIATION-CODE                          *
00041 *  ORIGINAL BENEFIT - CP-ORIGINAL-BENEFIT                        *
00042 *  RATING BENEFIT   - CP-RATING-BENEFIT-AMT (TOT BEN ON BALLOON) *
00043 *  BENEFIT TYPE     - CP-BENEFIT-TYPE                            *
00044 *  STATE CODE (NUM) - CP-STATE                                   *
00045 *  STATE CODE       - CP-STATE-STD-ABBV                          *
00046 *  COMPANY I.D.     - CP-COMPANY-ID                              *
00047 *  COMPANY CD (NUM) - CP-COMPANY-CD                              *
00048 *  INSURED AGE      - CP-ISSUE-AGE                               *
00049 *  RATING METHOD    - CP-EARNING-METHOD                          *
00050 *  SPECIAL METHOD   - CP-SPECIAL-CALC-CODE                       *
00051 *  A.P.R.           - CP-LOAN-APR                                *
00052 *  PAYMENT FREQUENCY- CP-PAY-FREQUENCY                           *
00053 *  CERT ISSUE DATE  - CP-CERT-EFF-DT                             *
00054 *  TERM OR EXT DAYS - CP-TERM-OR-EXT-DAYS                        *
00055 ******************************************************************
00056  ENVIRONMENT DIVISION.                                            
00057                                                                   
00058  DATA DIVISION.                                                   
00059      EJECT                                                        
00060  WORKING-STORAGE SECTION.                                         
00061  77  FILLER   PIC X(32) VALUE '********************************'. 
00062  77  FILLER   PIC X(32) VALUE '**  ELRATE  WORKING STORAGE   **'. 
00063  77  FILLER   PIC X(32) VALUE '***********VMOD 2.012 **********'. 
00064                                                                   
00065 ***  Y2K PROJ 7744                                                
00066  01  WS-HOLD-RATE-EXP.                                            
00067      05  WS-HOLD-RATE-EXP-AL PIC X(11).                           
00068      05  WS-HOLD-RATE-EXP-DT REDEFINES                            
00069          WS-HOLD-RATE-EXP-AL PIC 9(11).                           
00070 ***  Y2K PROJ 7744                                                
00071                                                                   
00072                              COPY ELCRATWS.                       
00073                                                                   
00074      EJECT                                                        
00075                              COPY ERCNETWS.                       
00076                                                                   
00077      EJECT                                                        
00078                              COPY ELCDATE.                        
00079                                                                   
00080      EJECT                                                        
00081                              COPY ELCCALC.                        
00082                                                                   
00083      EJECT                                                        
00084  LINKAGE SECTION.                                                 
00085  01  DFHCOMMAREA             PIC X(450).                          
00086                                                                   
00087      EJECT                                                        
00088 *01 PARMLIST              COMP.                                   
00089 *    02  FILLER              PIC S9(8).                           
00090 *    02  ERRATE-POINTER      PIC S9(8).                           
00091                                                                   
00092                                  COPY ERCRATE.                    
00093      EJECT                                                        
00094  PROCEDURE DIVISION.                                              
00095                                                                   
00096      MOVE DFHCOMMAREA            TO  CALCULATION-PASS-AREA.       
00097                                                                   
00098  000-START-PREMIUM-CALC.                                          
00099      MOVE ZERO                   TO CP-RETURN-CODE                
00100                                     CP-CALC-PREMIUM               
00101                                     CP-PREMIUM-RATE               
020816                                    CP-cancel-FEE.                
00103                                                                   
00104      MOVE SPACES                 TO CP-MORTALITY-CODE.            
00105      MOVE ALL '9'                TO ERRATE-KEY.                   
00106                                                                   
00107 *    IF CP-RATE-FILE = 'O'                                        
00108 *        MOVE OERATE-FILE-ID     TO ERRATE-FILE-ID.               
00109                                                                   
00110      MOVE CP-COMPANY-CD          TO RATE-COMPANY-CD.              
00111      MOVE CP-STATE               TO RATE-ST-CODE.                 
00112      MOVE CP-CLASS-CODE          TO RATE-ST-CLASS.                
00113      MOVE CP-DEVIATION-CODE      TO RATE-ST-DEV.                  
00114      MOVE CP-ISSUE-AGE           TO RATE-HIGH-AGE.                
00115                                                                   
00116      IF CP-RATING-BENEFIT-AMT NOT NUMERIC  OR                     
00117         CP-RATING-BENEFIT-AMT = ZEROS                             
00118          MOVE CP-ORIGINAL-BENEFIT    TO RATE-HIGH-AMT             
00119      ELSE                                                         
00120          MOVE CP-RATING-BENEFIT-AMT  TO RATE-HIGH-AMT.            
00121                                                                   
00122      IF CP-AH                                                     
00123          MOVE CP-AH-OVERRIDE-CODE     TO RATE-L-AH                
00124      ELSE                                                         
00125          MOVE CP-LIFE-OVERRIDE-CODE   TO RATE-L-AH.               
00126                                                                   
00127      MOVE CP-BENEFIT-CD          TO RATE-LAH-NUM.                 
00128      MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1.                
00129      MOVE SPACE                  TO DC-OPTION-CODE.               
00130                                                                   
00131      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.                    
00132                                                                   
00133 *** Y2K, PROJ 7744                                                
00134      IF NO-CONVERSION-ERROR                                       
00135          MOVE ZEROS                 TO RATE-EXPIRY-DATE           
00136                                        WS-HOLD-RATE-EXP-DT        
00137          MOVE DC-GREG-DATE-CYMD     TO WS-HOLD-RATE-EXP-AL(4:8)   
00138          MOVE WS-HOLD-RATE-EXP-DT   TO RATE-EXPIRY-DATE           
00139          MOVE ERRATE-KEY            TO SAVE-ERRATE-KEY            
00140      ELSE                                                         
00141          MOVE '2'                   TO CP-RETURN-CODE             
00142          GO TO 9999-CALC-RATE-PREM-X                              
00143      END-IF.                                                      
00144 *** Y2K, PROJ 7744                                                
00145                                                                   
00146      EXEC CICS HANDLE CONDITION                                   
00147          NOTFND   (9999-RATE-NOTFND)                              
00148          NOTOPEN  (9999-RATE-NOTOPEN)                             
00149          ENDFILE  (9999-RATE-NOTFND)                              
00150      END-EXEC.                                                    
00151                                                                   
00152  0050-RATE-START-BROWSE.                                          
00153      EXEC CICS STARTBR                                            
00154          DATASET  (ERRATE-FILE-ID)                                
00155          RIDFLD   (ERRATE-KEY)                                    
00156          GTEQ                                                     
00157      END-EXEC.                                                    
00158                                                                   
00159      MOVE 'Y'                  TO WS-BROWSE-SW.                   
00160                                                                   
00161  0100-READ-RATE-LOOP.                                             
00162      EXEC CICS READNEXT                                           
00163          DATASET  (ERRATE-FILE-ID)                                
00164          SET      (ADDRESS OF RATE-RECORD)                        
00165          RIDFLD   (ERRATE-KEY)                                    
00166      END-EXEC.                                                    
00167                                                                   
00168  0110-CHECK-KEYS.                                                 
120202                             COPY ELCRATPD.                       
00170                                                                   
00171      MOVE CALCULATION-PASS-AREA  TO  DFHCOMMAREA.                 
00172                                                                   
00173      IF WS-BROWSE-STARTED                                         
00174         EXEC CICS ENDBR                                           
00175              DATASET  (ERRATE-FILE-ID)                            
00176         END-EXEC.                                                 
00177                                                                   
00178      EXEC CICS RETURN                                             
00179      END-EXEC.                                                    
00180                                                                   
00181  9100-CONVERT-DATE.                                               
00182      EXEC CICS LINK                                               
00183          PROGRAM  ('ELDATCV')                                     
00184          COMMAREA (DATE-CONVERSION-DATA)                          
00185          LENGTH   (DC-COMM-LENGTH)                                
00186      END-EXEC.                                                    
00187                                                                   
00188  9100-EXIT.                                                       
00189      EXIT.                                                        
00190                                                                   
00191  EJECT                                                            
00192  10000-NET-TERM SECTION.                                          
00193                           COPY ERCNETP.                           
00194  99999-DUMMY-STOP-RUN.                                            
00195      GOBACK.                                                      
00196                                                                   
