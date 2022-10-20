00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
SUNPSD PROGRAM-ID.                SUNATEX.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 12/11/95 14:39:14.                 
00007 *                           VMOD=2.009                            
00008 *                                                                 
00009 *AUTHOR.     LOGIC, INC.                                          
00010 *            DALLAS, TEXAS.                                       
00011                                                                   
00012 *DATE-COMPILED.                                                   
00013                                                                   
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS.    *****************************************************
00025 *            *    (BATCH VERSION OF ELRATE)                      *
00026 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *
00027 *            *    OPTION SPECIFIED, COMPUTE PREMIUM AMOUNTS      *
00028 *            *    AND RATES.                                     *
00029 *            *****************************************************
00030                                                                   
00031                                                                   
00032 ******************************************************************
00033 *                                                                *
00034 *              INPUT FIELDS USED                                 *
00035 *                                                                *
00036 ******************************************************************
00037 *  ORIGINAL TERM    - CP-ORIGINAL-TERM    (RATING TERM)          *
00038 *  CLASS CODE       - CP-CLASS-CODE                              *
00039 *  DEVIATION CODE   - CP-DEVIATION-CODE                          *
00040 *  ORIGINAL BENEFIT - CP-ORIGINAL-BENEFIT                        *
00041 *  RATING BENEFIT   - CP-RATING-BENEFIT-AMT (TOT BEN ON BALLOON) *
00042 *  BENEFIT TYPE     - CP-BENEFIT-TYPE                            *
00043 *  STATE CODE (NUM) - CP-STATE                                   *
00044 *  STATE CODE       - CP-STATE-STD-ABBV                          *
00045 *  COMPANY I.D.     - CP-COMPANY-ID                              *
00046 *  COMPANY CD (NUM) - CP-COMPANY-CD                              *
00047 *  INSURED AGE      - CP-ISSUE-AGE                               *
00048 *  RATING METHOD    - CP-EARNING-METHOD                          *
00049 *  SPECIAL METHOD   - CP-SPECIAL-CALC-CODE                       *
00050 *  A.P.R.           - CP-LOAN-APR                                *
00051 *  A/H RATING TERM  - CP-CAPPED-TERM  (ONLY FOR SKIP CODE A/H)   *
00052 *  PAYMENT FREQUENCY- CP-PAY-FREQUENCY                           *
00053 *  CERT ISSUE DATE  - CP-CERT-EFF-DT                             *
00054 ******************************************************************
00055  ENVIRONMENT DIVISION.                                            
00056                                                                   
00057  INPUT-OUTPUT SECTION.                                            
00058  FILE-CONTROL.                                                    
00059                                                                   
00060      SELECT ERRATEF ASSIGN TO SYS001-FBA1-ERRATE                  
00061              ORGANIZATION IS INDEXED                              
00062              ACCESS IS SEQUENTIAL                                 
00063              RECORD KEY IS ER-RATE-KEY                            
00064              FILE STATUS IS ER-STATUS.                            
00065                                                                   
00066      SELECT OERATEF ASSIGN TO SYS001-FBA1-OERATE                  
00067              ORGANIZATION IS INDEXED                              
00068              ACCESS IS SEQUENTIAL                                 
00069              RECORD KEY IS OE-RATE-KEY                            
00070              FILE STATUS IS ER-STATUS.                            
00071                                                                   
00072  DATA DIVISION.                                                   
00073  FILE SECTION.                                                    
00074  FD  ERRATEF.                                                     
00075  01  ER-RATE-REC.                                                 
00076      03  FILLER       PIC XX.                                     
00077      03  ER-RATE-KEY  PIC X(28).                                  
00078      03  FILLER       PIC X(1735).                                
00079                                                                   
00080  FD  OERATEF.                                                     
00081  01  OE-RATE-REC.                                                 
00082      03  FILLER       PIC XX.                                     
00083      03  OE-RATE-KEY  PIC X(28).                                  
00084      03  FILLER       PIC X(1735).                                
00085                                                                   
00086      EJECT                                                        
00087  WORKING-STORAGE SECTION.                                         
00088  77  FILLER   PIC X(32) VALUE '********************************'. 
00089  77  FILLER   PIC X(32) VALUE '**  ELRATEX  WORKING STORAGE  **'. 
00090  77  FILLER   PIC X(32) VALUE '***********VMOD 2.009 **********'. 
00091                                                                   
00092                              COPY ELCRATWS.                       
00093                                                                   
00094                              COPY ERCNETWS.                       
00095                                                                   
00096      EJECT                                                        
00097                              COPY ELCDATE.                        
00098                                                                   
00099                              COPY ERCRATE.                        
00100      EJECT                                                        
00101  LINKAGE SECTION.                                                 
00102                                                                   
00103                              COPY ELCCALC.                        
00104                                                                   
00105      EJECT                                                        
00106  PROCEDURE DIVISION USING CALCULATION-PASS-AREA.                  
00107                                                                   
00108  000-START-PREMIUM-CALC.                                          
00109                                                                   
00110 *    IF CP-RATE-FILE = 'O'                                        
00111 *        MOVE OERATE-FILE-ID     TO ERRATE-FILE-ID.               
00112                                                                   
00113      IF  OPEN-RATE-FILE                                           
00114          GO TO 9999-OPEN-RATE-FILE.                               
00115                                                                   
00116      IF  CLOSE-RATE-FILE                                          
00117          GO TO 9999-CLOSE-RATE-FILE.                              
00118                                                                   
00119      MOVE ZERO                   TO CP-RETURN-CODE                
00120                                     CP-CALC-PREMIUM               
00121                                     CP-PREMIUM-RATE               
00122                                     CP-POLICY-FEE.                
00123                                                                   
00124      MOVE SPACES                 TO CP-MORTALITY-CODE.            
00125      MOVE ALL '9'                TO ERRATE-KEY.                   
00126                                                                   
00127      MOVE CP-COMPANY-CD          TO RATE-COMPANY-CD.              
00128      MOVE CP-STATE               TO RATE-ST-CODE.                 
00129      MOVE CP-CLASS-CODE          TO RATE-ST-CLASS.                
00130      MOVE CP-DEVIATION-CODE      TO RATE-ST-DEV.                  
00131      MOVE CP-ISSUE-AGE           TO RATE-HIGH-AGE.                
00132                                                                   
00133      IF CP-RATING-BENEFIT-AMT NOT NUMERIC  OR                     
00134         CP-RATING-BENEFIT-AMT = ZEROS                             
00135          MOVE CP-ORIGINAL-BENEFIT    TO RATE-HIGH-AMT             
00136      ELSE                                                         
00137          MOVE CP-RATING-BENEFIT-AMT  TO RATE-HIGH-AMT.            
00138                                                                   
00139      IF CP-AH                                                     
00140          MOVE CP-AH-OVERRIDE-CODE    TO RATE-L-AH                 
00141      ELSE                                                         
00142          MOVE CP-LIFE-OVERRIDE-CODE  TO RATE-L-AH.                
00143                                                                   
00144      MOVE CP-BENEFIT-CD          TO RATE-LAH-NUM.                 
00145      MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1.                
00146      MOVE SPACE                  TO DC-OPTION-CODE.               
00147                                                                   
00148      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.                    
00149      IF NO-CONVERSION-ERROR                                       
00150          MOVE DC-GREG-DATE-CYMD  TO RATE-EXPIRY-DATE              
00151          MOVE  ERRATE-KEY        TO SAVE-ERRATE-KEY               
00152      ELSE                                                         
00153          MOVE '2'                TO CP-RETURN-CODE                
00154          GO TO 9999-CALC-RATE-PREM-X.                             
00155                                                                   
00156  0050-RATE-START-BROWSE.                                          
00157      IF ERRATE-FILE-ID = OERATE-FILE-ID                           
00158          MOVE ERRATE-KEY TO OE-RATE-KEY                           
00159          START OERATEF KEY NOT LESS THAN OE-RATE-KEY              
00160      ELSE                                                         
00161          MOVE ERRATE-KEY TO ER-RATE-KEY                           
00162          START ERRATEF KEY NOT LESS THAN ER-RATE-KEY.             
00163                                                                   
00164      IF  ER-STATUS NOT = '00'                                     
00165          GO TO 9999-RATE-NOTFND.                                  
00166                                                                   
00167  0100-READ-RATE-LOOP.                                             
00168      IF ERRATE-FILE-ID = OERATE-FILE-ID                           
00169          READ OERATEF NEXT RECORD                                 
00170      ELSE                                                         
00171          READ ERRATEF NEXT RECORD.                                
00172                                                                   
00173      IF  ER-STATUS NOT = '00'                                     
00174          GO TO 9999-RATE-NOTFND.                                  
00175                                                                   
00176      IF ERRATE-FILE-ID = OERATE-FILE-ID                           
00177          MOVE OE-RATE-REC TO RATE-RECORD                          
00178      ELSE                                                         
00179          MOVE ER-RATE-REC TO RATE-RECORD.                         
00180                                                                   
00181  0110-CHECK-KEYS.                                                 
00182                       COPY ELCRATPD.                              
00183                                                                   
00184      GOBACK.                                                      
00185                                                                   
00186  9100-CONVERT-DATE.                                               
SUNPSD     CALL 'SUNATCX' USING DATE-CONVERSION-DATA.                   
00188                                                                   
00189  9100-EXIT.                                                       
00190      EXIT.                                                        
00191                                                                   
00192  9999-OPEN-RATE-FILE.                                             
00193      MOVE SPACE TO CP-RETURN-CODE                                 
00194                    CP-IO-FUNCTION.                                
00195                                                                   
00196      IF  ERRATE-FILE-ID = OERATE-FILE-ID                          
00197          OPEN INPUT OERATEF                                       
00198         ELSE                                                      
00199          OPEN INPUT ERRATEF.                                      
00200                                                                   
00201      IF  ER-STATUS NOT = '00' AND '97'                            
00202          MOVE 'E'                 TO CP-IO-FUNCTION.              
00203                                                                   
00204      GOBACK.                                                      
00205                                                                   
00206  9999-CLOSE-RATE-FILE.                                            
00207      MOVE SPACE TO CP-RETURN-CODE                                 
00208                    CP-IO-FUNCTION.                                
00209                                                                   
00210      IF  ERRATE-FILE-ID = OERATE-FILE-ID                          
00211          CLOSE OERATEF                                            
00212         ELSE                                                      
00213          CLOSE ERRATEF.                                           
00214                                                                   
00215      IF  ER-STATUS NOT = '00'                                     
00216          MOVE 'E'                 TO CP-IO-FUNCTION.              
00217                                                                   
00218      GOBACK.                                                      
00219                                                                   
00220  EJECT                                                            
00221  10000-NET-TERM SECTION.                                          
00222                           COPY ERCNETP.                           
00223                                                                   
00224  99999-DUMMY-STOP-RUN.                                            
00225      GOBACK.                                                      
00226                                                                   
