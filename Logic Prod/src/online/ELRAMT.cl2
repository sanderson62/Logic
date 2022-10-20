00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                ELRAMT.                               
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 03/05/96 16:20:15.                 
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
00031 *            *    ORIGINAL TERM = CP-ORIGINAL-TERM               *
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
00045  77  FILLER   PIC X(32) VALUE '********************************'. 
00046  77  FILLER   PIC X(32) VALUE '**  ELRAMT  WORKING STORAGE   **'. 
00047  77  FILLER   PIC X(32) VALUE '********* VMOD 2.007 ***********'. 
00048                                                                   
00049                            COPY ELCRAMTW.                         
00050      EJECT                                                        
00051                            COPY ELCCALC.                          
00052      EJECT                                                        
00053                            COPY ELCDATE.                          
00054                                                                   
00055  LINKAGE SECTION.                                                 
00056  01  DFHCOMMAREA                 PIC X(450).                      
00057                                                                   
00058      EJECT                                                        
00059  PROCEDURE DIVISION.                                              
00060      MOVE DFHCOMMAREA            TO  CALCULATION-PASS-AREA.       
00061                                                                   
00062  000-START-REM-CALC.        COPY ELCRAMTP.                        
00063                                                                   
00064      MOVE CALCULATION-PASS-AREA  TO  DFHCOMMAREA.                 
00065                                                                   
00066      EXEC CICS RETURN                                             
00067          END-EXEC.                                                
00068                                                                   
00069       GOBACK.                                                     
00070                                                                   
00071  0030-CNVT-DT.                                                    
00072                                                                   
00073      EXEC CICS LINK                                               
00074          PROGRAM    ('ELDATCV')                                   
00075          COMMAREA   (DATE-CONVERSION-DATA)                        
00076          LENGTH     (DC-COMM-LENGTH)                              
00077      END-EXEC.                                                    
00078                                                                   
00079  0030-CNVT-EXIT.                                                  
00080      EXIT.                                                        
00081                                                                   
00082  1000-NET-TERM SECTION.          COPY ELCRAMTN.                   
00083                                                                   
