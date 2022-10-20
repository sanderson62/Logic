00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 ELRFNDX.                             
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 12/20/95 13:26:06.                 
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
00039 *  CAPPED TERM      - CP-LOAN-TERM                               *
00040 *  REMAINING TERM   - CP-REMAINING-TERM                          *
00041 *  STATE CODE       - CP-STATE                                   *
00042 *  STATE CODE       - CP-STATE-STD-ABBV                          *
00043 *  CLASS CODE       - CP-CLASS-CODE                              *
00044 *  DEVIATION CODE   - CP-DEVIATION-CODE                          *
00045 *  ORIGINAL BENEFIT - CP-ORIGINAL-BENEFIT                        *
00046 *  PROCESS TYPE     - CP-PROCESS-TYPE                            *
00047 *  BENEFIT KIND     - CP-BENEFIT-TYPE                            *
00048 *  A.P.R.           - CP-LOAN-APR                                *
00049 *  METHOD           - CP-EARNING-METHOD                          *
00050 *  SPECIAL METHOD   - CP-SPECIAL-CALC-CODE                       *
00051 *  PAYMENT FREQUENCY- CP-PAY-FREQUENCY                           *
00052 *  COMPANY I.D.     - CP-COMPANY-ID                              *
00053 *  BENEFIT CODE     - CP-BENEFIT-CD                              *
00054 *  INSURED AGE      - CP-ISSUE-AGE                               *
00055 ******************************************************************
00056  ENVIRONMENT DIVISION.                                            
00057                                                                   
00058  DATA DIVISION.                                                   
00059      EJECT                                                        
00060  WORKING-STORAGE SECTION.                                         
00061  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
00062  77  FILLER   PIC X(32) VALUE '********************************'. 
00063  77  FILLER   PIC X(32) VALUE '**  ELRFNDX WORKING STORAGE   **'. 
00064  77  FILLER   PIC X(32) VALUE '*** VMOD=2.011 *****************'. 
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
00112                                                                   
00113                          COPY ELCDATE.                            
00114  EJECT                                                            
00115  LINKAGE SECTION.                                                 
00116                          COPY ELCCALC.                            
00117                                                                   
00118      EJECT                                                        
00119  PROCEDURE DIVISION  USING CALCULATION-PASS-AREA.                 
00120                                                                   
00121  000-START-REFUND-CALC.                                           
00122                        COPY ELCRFNDPP.                             
00123      GOBACK.                                                      
00124                                                                   
00125      EJECT                                                        
00126  2500-GET-RATE.                                                   
00127      CALL 'ELRATEX' USING CALCULATION-PASS-AREA.                  
00128                                                                   
00129  2500-EXIT.                                                       
00130       EXIT.                                                       
00131      EJECT                                                        
00132                                                                   
00133  9100-CONVERT-DATE.                                               
00134      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
00135                                                                   
00136  9100-EXIT.                                                       
00137       EXIT.                                                       
00138                                                                   
00139      EJECT                                                        
00140  10000-NET-TERM SECTION.                                          
00141                              COPY ERCNETP.                        
00142                                                                   
