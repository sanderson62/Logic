00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 ELUPRMX.                             
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 10/17/94 15:27:20.                 
00007 *                            VMOD=2.004                           
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
00019 *                                                                *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024                                                                   
00025 *REMARKS.    *****************************************************
00026 *            *                                                   *
00027 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *
00028 *            *    OPTION SPECIFIED, COMPUTE UNEARNED PREMIUM     *
00029 *            *****************************************************
00030                                                                   
122607******************************************************************
122607*                   C H A N G E   L O G
122607*
122607* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122607*-----------------------------------------------------------------
122607*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122607* EFFECTIVE    NUMBER
122607*-----------------------------------------------------------------
122607* 122607    2007010300003  PEMA  CHANGE ALL CP TO PRORATA
122607******************************************************************
00031                                                                   
00032 ******************************************************************
00033 *                                                                *
00034 *              INPUT FIELDS USED                                 *
00035 *                                                                *
00036 ******************************************************************
00037 *  CERT ISSUE DATE  - CP-CERT-EFF-DT                             *
00038 *  REFUND DATE      - CP-VALUATION-DT                            *
00039 *  ORIGINAL TERM    - CP-ORIGINAL-TERM                           *
00040 *  CAPPED TERM      - CP-LOAN-TERM                               *
00041 *  REMAINING TERM   - CP-REMAINING-TERM                          *
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
00063  77  FILLER   PIC X(32) VALUE '**  ELUPRMX WORKING STORAGE   **'. 
00064  77  FILLER   PIC X(32) VALUE '*** VMOD=2.004 *****************'. 
00065                                                                   
00066  01  WS-WORK-MISC.                                                
021904     12  WS-CP-METHOD        PIC X.
021904     12  WS-AH-METHOD        PIC X.
00067      12  WS-REMAINING-TERM   PIC S9(4)V9      COMP-3.             
00068      12  WS-TEMP-RESULT      PIC S9(7)V9(6)   COMP-3.             
00069      12  WS-REMAINING-AMT    PIC S9(7)V9(6)   COMP-3.             
00070      12  WS-ORIG-PREM        PIC S9(7)V9(6)   COMP-3.             
00071      12  WS-REMAIN-PREM      PIC S9(7)V9(6)   COMP-3.             
00072      12  WS-NP-REF-FACTOR    PIC S9(4)V9(11)  COMP-3.             
00073      12  WS-SAVE-TERM        PIC S9(3)        COMP-3.             
00074      12  UNEARN-PRM          PIC S9(5)V99     COMP-3.             
00075      12  FACTOR-1            PIC S9(4)V9(11)  COMP-3.             
00076      12  FACTOR-2            PIC S9(4)V9(11)  COMP-3.             
00077      12  FACTOR-3            PIC S9(4)V9(11)  COMP-3.             
00078      12  FACTOR-4            PIC S9(4)V9(11)  COMP-3.             
00079      12  FACTOR-5            PIC S9(7)        COMP-3.             
00080      12  FACTOR-6            PIC S9(7)        COMP-3.             
00081      12  RATE-FACTOR-1       PIC S9V99        COMP-3.             
00082      12  RATE-FACTOR-2       PIC S999         COMP-3.             
00083      12  R78-TEMP-1          PIC S9(5)V99     COMP-3.             
00084      12  R78-TEMP-2          PIC S9(5)V99     COMP-3.             
00085      12  R78-FACTOR          PIC S9(4)V9(11)  COMP-3.             
00086      12  R78-UN-PREMIUM      PIC S9(7)V99     COMP-3.             
00087      12  R78-UN-PREM-ALT     PIC S9(7)V99     COMP-3.             
00088      12  PR-FACTOR           PIC S9(4)V9(11)  COMP-3.             
00089      12  PR-UN-PREMIUM       PIC S9(7)V99     COMP-3.             
00090      12  PR-UN-PREM-ALT      PIC S9(7)V99     COMP-3.             
00091      12  STATE-UN-PREMIUM    PIC S9(7)V99     COMP-3.             
00092      12  DOMI-UN-PREMIUM     PIC S9(7)V99     COMP-3.             
00093      12  SRL                 PIC S9(4)        COMP  VALUE +00.    
00094                                                                   


021904 01  METHOD-TABLE.                                                
021904     05  M-TABLE.                                                 
021904         10  FILLER              PIC XXXX VALUE 'AKPP'.
122607         10  FILLER              PIC XXXX VALUE 'ALPR'.
122607         10  FILLER              PIC XXXX VALUE 'ARPR'.
021904         10  FILLER              PIC XXXX VALUE 'AZPP'.
021904         10  FILLER              PIC XXXX VALUE 'CAPP'.
021904         10  FILLER              PIC XXXX VALUE 'COPM'.
021904         10  FILLER              PIC XXXX VALUE 'CTPP'.
021904         10  FILLER              PIC XXXX VALUE 'DCPR'.
122607         10  FILLER              PIC XXXX VALUE 'DEPR'.
122607         10  FILLER              PIC XXXX VALUE 'FLPR'.
122607         10  FILLER              PIC XXXX VALUE 'GAPM'.
021904         10  FILLER              PIC XXXX VALUE 'GUPR'.
021904         10  FILLER              PIC XXXX VALUE 'HIPR'.
021904         10  FILLER              PIC XXXX VALUE 'IAPR'.
021904         10  FILLER              PIC XXXX VALUE 'IDPR'.
021904         10  FILLER              PIC XXXX VALUE 'ILPR'.
021904         10  FILLER              PIC XXXX VALUE 'INPR'.
021904         10  FILLER              PIC XXXX VALUE 'KSPR'.
021904         10  FILLER              PIC XXXX VALUE 'KYPR'.
021904         10  FILLER              PIC XXXX VALUE 'LAPR'.
021904         10  FILLER              PIC XXXX VALUE 'MAPP'.
021904         10  FILLER              PIC XXXX VALUE 'MDPR'.
021904         10  FILLER              PIC XXXX VALUE 'MEPP'.
021904         10  FILLER              PIC XXXX VALUE 'MIPR'.
021904         10  FILLER              PIC XXXX VALUE 'MNPR'.
021904         10  FILLER              PIC XXXX VALUE 'MOPR'.
021904         10  FILLER              PIC XXXX VALUE 'MSPR'.
021904         10  FILLER              PIC XXXX VALUE 'MTPR'.
021904         10  FILLER              PIC XXXX VALUE 'NCPP'.
021904         10  FILLER              PIC XXXX VALUE 'NDPR'.
021904         10  FILLER              PIC XXXX VALUE 'NEPR'.
021904         10  FILLER              PIC XXXX VALUE 'NHPP'.
021904         10  FILLER              PIC XXXX VALUE 'NJPR'.
021904         10  FILLER              PIC XXXX VALUE 'NMPM'.
021904         10  FILLER              PIC XXXX VALUE 'NVPR'.
021904         10  FILLER              PIC XXXX VALUE 'NYPP'.
062806         10  FILLER              PIC XXXX VALUE 'OHPM'.
021904         10  FILLER              PIC XXXX VALUE 'OKPM'.
021904         10  FILLER              PIC XXXX VALUE 'ORPP'.
021904         10  FILLER              PIC XXXX VALUE 'PAPR'.
021904         10  FILLER              PIC XXXX VALUE 'PRPR'.
021904         10  FILLER              PIC XXXX VALUE 'RIPP'.
021904         10  FILLER              PIC XXXX VALUE 'SCPR'.
021904         10  FILLER              PIC XXXX VALUE 'SDPR'.
021904         10  FILLER              PIC XXXX VALUE 'TNPR'.
021904         10  FILLER              PIC XXXX VALUE 'TXPM'.
021904         10  FILLER              PIC XXXX VALUE 'UTPR'.
021904         10  FILLER              PIC XXXX VALUE 'VAPR'.
021904         10  FILLER              PIC XXXX VALUE 'VTPP'.
021904         10  FILLER              PIC XXXX VALUE 'WAPR'.
021904         10  FILLER              PIC XXXX VALUE 'WIPR'.
021904         10  FILLER              PIC XXXX VALUE 'WVPR'.
021904         10  FILLER              PIC XXXX VALUE 'WYPP'.
021904     05  MR-TABLE REDEFINES M-TABLE  OCCURS 53 TIMES
021904                            INDEXED BY TABLE-INDEX.
021904         10  T-STATE             PIC XX.
021904         10  CP-METHOD-CODE      PIC X.
021904         10  AH-METHOD-CODE      PIC X.
021904                                                                  



00095  01  AH-RES-STATE-TABLE.                                          
00096      12  FILLER              PIC X(16)  VALUE                     
021904            'AKALAZCANCNDORWY'.
00098      12  FILLER              PIC XX     VALUE HIGH-VALUES.        
00099                                                                   
00100  01  AH-RES-STAT-TAB  REDEFINES AH-RES-STATE-TABLE.               
021904     12  AH-RES-STATE-ENT    PIC XX     OCCURS 9 TIMES.          
00102                                                                   
00103  01  TEXAS-REG-WORK-AREAS.                                        
00104      12  TEX-FACT-1          PIC S9(7)V9(2)  COMP-3.              
00105      12  TEX-FACT-2          PIC S9(3)       COMP-3.              
00106      12  TEX-FACT-3          PIC S9(3)       COMP-3.              
00107      12  TEX-FACT-4          PIC S9(7)       COMP-3.              
00108      12  TEX-FACT-5          PIC S9(3)       COMP-3.              
00109      12  TEX-FACT-6          PIC S9(3)       COMP-3.              
00110      12  TEX-FACT-7          PIC S9(7)       COMP-3.              
00111      12  TEX-FACT-8          PIC S9V9(6)     COMP-3.              
00112      12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.              
00113                                                                   
00114  01  NET-PAY-INTERFACE.                                           
00115      12  N-P-APR             PIC S9(3)V9(4)  COMP-3.              
00116      12  N-P-ORIG            PIC S9(3)       COMP-3.              
00117      12  N-P-REM             PIC S9(3)       COMP-3.              
00118      12  N-P-OPT             PIC X.                               
00119      12  N-P-CAP             PIC S9(3)       COMP-3.              
00120      12  N-P-LOAN            PIC S9(3)       COMP-3.              
00121      12  N-P-FACTOR          PIC S9(4)V9(9)  COMP-3.              
00122                                                                   
00123 *************************COPY ERCNETWS.                           
00124  EJECT                                                            
00125                          COPY ELCDATE.                            
00126  EJECT                                                            
00127                                                                   
00128                                                                   
00129  LINKAGE SECTION.                                                 
00130                           COPY ELCCALC.                           
00131                                                                   
00132      EJECT                                                        
00133  PROCEDURE DIVISION  USING CALCULATION-PASS-AREA.                 
00134                                                                   
00135  0000-START-UNEARNED-CALC.                                        
00136                        COPY ELCUPRMPT.
00137      GOBACK.                                                      
00138                                                                   
00139      EJECT                                                        
00140  2500-GET-RATE.                                                   
00141      CALL 'ELRATEX' USING CALCULATION-PASS-AREA.                  
00142                                                                   
00143  2500-EXIT.                                                       
00144      EXIT.                                                        
00145                                                                   
00146  9100-CONVERT-DATE.                                               
00147      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
00148                                                                   
00149  9100-EXIT.                                                       
00150       EXIT.                                                       
00151                                                                   
00152      EJECT                                                        
00153 *10000-NET-TERM SECTION.                                          
00154 *                            COPY ERCNETP.                        
00155                                                                   
