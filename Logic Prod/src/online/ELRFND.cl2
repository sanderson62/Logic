00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 ELRFND.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 03/05/96 16:23:50.                 
00007 *                            VMOD=2.011                           
00008 *                                                                 
00009 *AUTHOR.       LOGIC, INC.                                        
00010 *              DALLAS, TEXAS.                                     
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
00025 *            *                                                   *
00026 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *
00027 *            *    OPTION SPECIFIED, COMPUTE PREMIUM REFUNDS      *
00028 *            *****************************************************
00029                                                                   
00030                                                                   
00031 ******************************************************************
00032 *                                                                *
00033 *              INPUT FIELDS USED                                 *
00034 *                                                                *
00035 ******************************************************************
00036 *  CERT ISSUE DATE  - CP-CERT-EFF-DT                             *
00037 *  REFUND DATE      - CP-VALUATION-DT                            *
00038 *  ORIGINAL TERM    - CP-ORIGINAL-TERM                           *
00039 *  LOAN TERM        - CP-LOAN-TERM                               *
00040 *  REMAINING TERM   - CP-REMAINING-TERM                          *
00041 *  TERM OR EXT DAYS - CP-TERM-OR-EXT-DAYS                        *
00042 *  STATE CODE       - CP-STATE                                   *
00043 *  STATE CODE       - CP-STATE-STD-ABBV                          *
00044 *  CLASS CODE       - CP-CLASS-CODE                              *
00045 *  DEVIATION CODE   - CP-DEVIATION-CODE                          *
00046 *  ORIGINAL BENEFIT - CP-ORIGINAL-BENEFIT                        *
00047 *  PROCESS TYPE     - CP-PROCESS-TYPE                            *
00048 *  BENEFIT KIND     - CP-BENEFIT-TYPE                            *
00049 *  A.P.R.           - CP-LOAN-APR                                *
00050 *  METHOD           - CP-EARNING-METHOD                          *
00051 *  SPECIAL METHOD   - CP-SPECIAL-CALC-CODE                       *
00052 *  PAYMENT FREQUENCY- CP-PAY-FREQUENCY                           *
00053 *  COMPANY I.D.     - CP-COMPANY-ID                              *
00054 *  BENEFIT CODE     - CP-BENEFIT-CD                              *
00055 *  INSURED AGE      - CP-ISSUE-AGE                               *
00056 ******************************************************************
00057  ENVIRONMENT DIVISION.                                            
00058                                                                   
00059  DATA DIVISION.                                                   
00060      EJECT                                                        
00061  WORKING-STORAGE SECTION.                                         
00062  77  FILLER   PIC X(32) VALUE '********************************'. 
00063  77  FILLER   PIC X(32) VALUE '**  ELRFND  WORKING STORAGE   **'. 
00064  77  FILLER   PIC X(32) VALUE '********VMOD=2.011 *************'. 
       77  WS-UE-CLP                   PIC S9(5)V99 COMP-3 VALUE +0.
       77  WS-GFR3                     PIC S9(5)V99 COMP-3 VALUE +0.
       77  WS-MONTH                    PIC S999     COMP-3 VALUE +0.
00065                                                                   
00066  01  WS-WORK-MISC.                                                
00067      12  WS-REMAINING-TERM   PIC S9(4)V9      COMP-3.             
00068      12  WS-TEMP-RESULT      PIC S9(7)V9(6)   COMP-3.             
00069      12  WS-REMAINING-AMT    PIC S9(7)V9(6)   COMP-3.             
00070      12  WS-ORIG-PREM        PIC S9(7)V9(6)   COMP-3.             
00071      12  WS-REMAIN-PREM      PIC S9(7)V9(6)   COMP-3.             
00072      12  WS-NP-REF-FACTOR    PIC S9(4)V9(11)  COMP-3.             
00073      12  WS-SAVE-TERM        PIC S9(3)        COMP-3.             
00074      12  WS-SAVE-LOAN-TERM   PIC S9(3)        COMP-3.             
00075      12  WS-SAVE-BENEFIT     PIC S9(9)V99     COMP-3.             
00076      12  WS-SAVE-EARN-METHOD PIC X.                               
050713     12  ws-save-ext-days    pic s9(5)        comp-3.
00077      12  CAL-RFND            PIC S9(5)V99     COMP-3.             
00078      12  FACTOR-1            PIC S9(4)V9(11)  COMP-3.             
00079      12  FACTOR-2            PIC S9(4)V9(11)  COMP-3.             
00080      12  FACTOR-3            PIC S9(4)V9(11)  COMP-3.             
00081      12  FACTOR-4            PIC S9(4)V9(11)  COMP-3.             
00082      12  FACTOR-5            PIC S9(7)        COMP-3.             
00083      12  FACTOR-6            PIC S9(7)        COMP-3.             
00084      12  RATE-FACTOR-1       PIC S9V99        COMP-3.             
00085      12  RATE-FACTOR-2       PIC S999         COMP-3.             
00086      12  R78-FACTOR          PIC S9(4)V9(11)  COMP-3.             
00087      12  PR-FACTOR           PIC S9(4)V9(11)  COMP-3.             
00088      12  RSUM-FACTOR         PIC S9(4)V9(11)  COMP-3.             
00089      12  RSUM-REMAINING-TERM PIC S9(3)V99     COMP-3.             
00090                                                                   
00091  01  TEXAS-REG-WORK-AREAS.                                        
00092      12  TEX-FACT-1          PIC S9(7)V9(2)  COMP-3.              
00093      12  TEX-FACT-2          PIC S9(3)       COMP-3.              
00094      12  TEX-FACT-3          PIC S9(3)       COMP-3.              
00095      12  TEX-FACT-4          PIC S9(7)       COMP-3.              
00096      12  TEX-FACT-5          PIC S9(3)       COMP-3.              
00097      12  TEX-FACT-6          PIC S9(3)       COMP-3.              
00098      12  TEX-FACT-7          PIC S9(7)       COMP-3.              
00099      12  TEX-FACT-8          PIC S9V9(6)     COMP-3.              
00100      12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.              
00101                                                                   
00102  01  NET-PAY-INTERFACE.                                           
00103      12  N-P-APR             PIC S9(3)V9(4)  COMP-3.              
00104      12  N-P-ORIG            PIC S9(3)       COMP-3.              
00105      12  N-P-REM             PIC S9(3)       COMP-3.              
00106      12  N-P-OPT             PIC X.                               
00107      12  N-P-LOAN            PIC S9(3)       COMP-3.              
00108      12  N-P-FACTOR          PIC S9(4)V9(9)  COMP-3.              
00109                                                                   
00110                          COPY ERCNETWS.                           
00111  EJECT                                                            
00112                          COPY ELCDATE.                            
00113  EJECT                                                            
00114                          COPY ELCCALC.                            
00115                                                                   
00116                                                                   
00117  LINKAGE SECTION.                                                 
00118  01  DFHCOMMAREA                PIC X(450).                       
00119                                                                   
00120      EJECT                                                        
00121  PROCEDURE DIVISION.                                              
00122      MOVE DFHCOMMAREA    TO  CALCULATION-PASS-AREA.               
00123                                                                   
00124  000-START-REFUND-CALC.                                           
00125                        COPY ELCRFNDP.                             
00126                                                                   
00127      MOVE CALCULATION-PASS-AREA TO DFHCOMMAREA.                   
00128                                                                   
00129      EXEC CICS RETURN                                             
00130      END-EXEC.                                                    
00131      GOBACK.                                                      
00132                                                                   
00133      EJECT                                                        
00134   2500-GET-RATE.                                                  
00135      EXEC CICS LINK                                               
00136          PROGRAM    ('ELRATE')                                    
00137          COMMAREA   (CALCULATION-PASS-AREA)                       
00138          LENGTH     (CP-COMM-LENGTH)                              
00139          END-EXEC.                                                
00140                                                                   
00141   2500-EXIT.                                                      
00142        EXIT.                                                      
00143                                                                   
00144   9100-CONVERT-DATE.                                              
00145      EXEC CICS LINK                                               
00146           PROGRAM   ('ELDATCV')                                   
00147           COMMAREA  (DATE-CONVERSION-DATA)                        
00148           LENGTH    (DC-COMM-LENGTH)                              
00149      END-EXEC.                                                    
00150                                                                   
00151   9100-EXIT.                                                      
00152        EXIT.                                                      
00153                                                                   
00154      EJECT                                                        
00155   10000-NET-TERM  SECTION.                                        
00156                        COPY ERCNETP.                              
