00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
SUNPSD PROGRAM-ID.                SUNAMTX.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 12/20/95 13:19:23.                 
00007 *                           VMOD=2.007                            
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
00025 *            *                                                   *
00026 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *
00027 *            *    OPTION SPECIFIED, COMPUTE REMAINING AMOUNTS    *
00028 *            *    FOR A CERTIFICATE.                             *
00029 *            *                                                   *
00030 *            *    ORIGINAL BENEFIT = CP-ORIGINAL-BENEFIT         *
00031 *            *    ORIGINAL TERM   = CP-ORIGINAL-TERM             *
00032 *            *    REMAINING-TERM  = CP-REMAINING-TERM            *
00033 *            *    A.P.R (FOR NET PAY) = CP-LOAN-APR              *
00034 *            *    BENEFIT TYPE = CP-BENEFIT-TYPE                 *
00035 *            *    METHOD = CP-EARNING-METHOD                     *
00036 *            *    IF COMPANY SPECIAL METHOD  - PUT COMPANY       *
00037 *            *     I.D. IN CP-COMPANY-ID.                        *
00038 *            *                                                   *
00039 *            *****************************************************
00040  ENVIRONMENT DIVISION.                                            
00041                                                                   
00042  DATA DIVISION.                                                   
00043      EJECT                                                        
00044  WORKING-STORAGE SECTION.                                         
00045  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
00046  77  FILLER   PIC X(32) VALUE '********************************'. 
00047  77  FILLER   PIC X(32) VALUE '**  ELRAMTX WORKING STORAGE   **'. 
00048  77  FILLER   PIC X(32) VALUE '******** VMOD=2.007 ************'. 
00049                                                                   
00050      COPY ELCRAMTW.                                               
00051                                                                   
00052      COPY ELCDATE.                                                
00053                                                                   
00054      EJECT                                                        
00055  LINKAGE SECTION.                                                 
00056      COPY ELCCALC.                                                
00057                                                                   
00058      EJECT                                                        
00059  PROCEDURE DIVISION USING CALCULATION-PASS-AREA.                  
00060                                                                   
00061  000-START-REM-CALC.        COPY ELCRAMTP.                        
00062                                                                   
00063      GOBACK.                                                      
00064                                                                   
00065  0030-CNVT-DT.                                                    
00066                                                                   
SUNPSD     CALL 'SUNATCX' USING DATE-CONVERSION-DATA.                   
00068                                                                   
00069  0030-CNVT-EXIT.                                                  
00070      EXIT.                                                        
00071      EJECT                                                        
00072  1000-NET-TERM SECTION.     COPY ELCRAMTN.                        
00073                                                                   
