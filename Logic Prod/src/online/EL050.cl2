00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL050 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 06/07/94 08:48:38.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.032.                          
00008 *                                                                 
00009                                                                   
00010 *AUTHOR.     LOGIC, INC.                                          
00011 *            DALLAS, TEXAS.                                       
00024                                                                   
00025 *REMARKS.                                                         
00026 *            THIS SUBROUTINE IS THE EDIT ROUTINE FOR THE          
00027 *            ON-LINE CREDIT SYSTEM.                               
00028 *                                                                 
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
100703*                          SMVA  ADD NEW CLM TYPE G FOR SECURE PAY
062904* 062904    2004020600012  PEMA  ON CANCELS, CHECK TO SEE IF CLAIM
062904*                                IS OPEN OR CNC DT < PD THRU
111504* 111504    2004110300005  PEMA  SPLIT SPP BANK COMMISSION
083106* 083106  CR2006063000001  PEMA  ADD LOAN OFFICER FIELD EDIT
030309* 030309  CR2009021700001  PEMA  ADD EDIT FOR BENE AND INS ADDR
042709* 042709  CR2009031600001  AJRA  ADD VIN UPDATE TO CERT TRAILER
012010* 012010  CR2009061500002  AJRA  ADD FLAG FOR REFUND WITH OPEN CLAIM
060710* 060710  CR2009061500002  AJRA  NO REFUND FLAG WHEN NO DISAB REFUND
011211* 011211  CR2010030900001  AJRA  CANCELS WITH CLAIMS PROCESS
050311* 050311  IR2011050300001  AJRA  FIX MSG FOR RESCINDED COVERAGE
042412* 042412  IR2012042300001  AJRA  FIX MSG FOR AH CLOSED CLAIM
061412* 061412  CR2011022800001  AJRA  NAPERSOFT
062712* 062712  CR2011022800001  AJRA  REDEFINE ORIG DATA
071712* 071712  CR2011022800001  AJRA  NAPERSOFT CANCELS
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
011413* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
111113* 111113  CR2013110500002  AJRA  ALLOW CANCEL WHEN CLAIM DENIED
021714* 021714  CR2013080900002  PEMA  ADD ERROR 1573 and 1574
040914* 040914  IR2014040900001  AJRA  CHECK ALL OPEN/CLOSED CLAIMS
050614* 050614  IR2014050500002  AJRA  ALLOW CANCEL OF OLD COVERAGE
051914* 051914  CR2014043000002  TANA  CHANGE EDIT FOR CANCEL DATE
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
091114* 091114  IR2014091000001  TANA  CHANGE 2890 EDIT FOR EXACT MATCH ON
091114*                                CERT #, ADD 2898 FORCIBLE ERROR
072215* 072215  IR2015031000002  PEMA  CHG EDIT FOR ERROR 2889
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
100217* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
020218* 020218  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
092618* 092618  CR2018050200001  PEMA  Add check for open clm on issue
101019* 101019  CR2019100700001  PEMA  Disallow V status with claim
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
121802******************************************************************
00029  ENVIRONMENT DIVISION.                                            
00030                                                                   
00031  DATA DIVISION.                                                   
00032      EJECT                                                        
00033  WORKING-STORAGE SECTION.                                         
00034                                                                   
00035  77  FILLER   PIC X(32) VALUE '********************************'. 
00036  77  FILLER   PIC X(32) VALUE '**  EL050   WORKING STORAGE   **'. 
00037  77  FILLER   PIC X(32) VALUE '*********** VMOD 2.032 *********'. 
00038                                                                   
00039  77  LCP-WS-ADDR-COMP              PIC S9(8) COMP VALUE +0.       
00040  77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP     
00041                                    USAGE POINTER.                 
00042  77  FILLER                        PIC S9(8)   COMP VALUE +0.     
00043  77  ERPLAN-WS-COMP                PIC S9(8)   COMP VALUE +0.     
00044  77  ERPLAN-POINTER  REDEFINES ERPLAN-WS-COMP                     
00045                                    USAGE IS POINTER.              
00046  77  ELCNTL-WS-COMP                PIC S9(8)   COMP VALUE +0.     
00047  77  ELCNTL-POINTER  REDEFINES ELCNTL-WS-COMP                     
00048                                    USAGE IS POINTER.              
00049  77  ERACCT-WS-COMP                PIC S9(8)   COMP VALUE +0.     
00050  77  ERACCT-POINTER  REDEFINES ERACCT-WS-COMP                     
00051                                    USAGE IS POINTER.              
00052                                                                   
00053  77  ERFORM-WS-COMP                PIC S9(8)   COMP VALUE +0.     
00054  77  ERFORM-POINTER  REDEFINES ERFORM-WS-COMP                     
00055                                    USAGE IS POINTER.              
00056                                                                   
00057      COPY ELC50WS.                                                
00058      EJECT                                                        
00059  01  WORKING-STORAGE-WORK-AREAS.                                  
083106     12  WS-RESPONSE             PIC S9(8)  COMP.
083106         88  WS-RESP-NORMAL           VALUE +00.
083106         88  WS-RESP-ERROR            VALUE +01.
083106         88  WS-RESP-NOTFND           VALUE +13.
083106         88  WS-RESP-DUPREC           VALUE +14.
083106         88  WS-RESP-ENDFILE          VALUE +20.
050311     12  WS-AH-CLAIM-RESCINDED        PIC X  VALUE 'N'.
00060      12  WORK-AREAS.                                              
00061          16  WK-CONV-GREG.                                        
00062              24  WCG-CENT             PIC XX.                     
00063              24  WCG-REST             PIC X(4).                   
00064          16  WORK-GREGORIAN-DATE.                                 
00065              24  WK-GREG-CENT         PIC XX.                     
00066              24  WK-GREG-DATE-YYMMDD.                             
00067                  28  WK-GREG-YYMM     PIC X(4).                   
00068                  28  WK-GREG-DD       PIC XX.                     
00069                                                                   
CIDMOD         16  WORK-SELECT-DATE.                                    
CIDMOD             24  WK-SELECT-CENT            PIC XX.                
CIDMOD             24  WK-SELECT-DATE-YYMMDD.                           
CIDMOD                 28  WK-SELECT-GREG-YYMM   PIC X(4).              
CIDMOD                 28  FILLER                PIC XX.                
00073                                                                   
00074          16  WK-FORM-NO.                                          
00075              24  WK-FORM-BANK-ID       PIC X(5).                  
00076              24  WK-FORM-UNDRWRTR      PIC X(1).                  
00077              24  WK-FORM-MASTER-POL-NO PIC X(6).                  
00078                                                                   
00079          16  WK-MEMBER-NO.                                        
00080              24  WK-MEMBER-1-6        PIC X(6).                   
00081              24  WK-MEMBER-7-12       PIC X(6).                   
00082                                                                   
00083          16  WK-FULL-CERT-NO.                                     
00084              24  WK-CERT-POS-1-2      PIC X(2).                   
00085              24  WK-CERT-POS-3        PIC X(1).                   
00086              24  WK-CERT-POS-4        PIC X(1).                   
00087              24  WK-CERT-POS-5-6      PIC X(2).                   
00088              24  WK-CERT-POS-7        PIC X(1).                   
00089              24  WK-CERT-POS-8-11     PIC X(4).                   
00090                                                                   
00091          16  WK-VALID-RATE-CODE       PIC X  VALUE ' '.           
00092              88  VALID-RATE-CODE        VALUE 'Y'.                
00093              88  FOUND-TO-BE-INVALID    VALUE 'N'.                
00094                                                                   
00095          16  DLO001-COMM-LENGTH    PIC S9(4)   COMP VALUE +8.     
00096          16  DLO002-COMM-LENGTH    PIC S9(4)   COMP VALUE +8.     
00097          16  DLO003-COMM-LENGTH    PIC S9(4)   COMP VALUE +8.     
00098          16  DLO007-COMM-LENGTH    PIC S9(4)   COMP VALUE +51.    
00099          16  DLO011-COMM-LENGTH    PIC S9(4)   COMP VALUE +20.    
00100          16  DLO017-COMM-LENGTH    PIC S9(4)   COMP VALUE +6.     
00101          16  DLO019-COMM-LENGTH    PIC S9(4)   COMP VALUE +14.    
00102          16  DLO021-COMM-LENGTH    PIC S9(4)   COMP VALUE +5.     
00103          16  DLO022-COMM-LENGTH    PIC S9(4)   COMP VALUE +4.     
00104          16  DLO023-COMM-LENGTH    PIC S9(4)   COMP VALUE +132.   
00105                                                                   
00106 * DLO001                                                          
00107  01  WS-DLO-MASTER-POLICY-FORM.                                   
00108      12  DMPF-POLICY-FORM-NUMBER   PIC X(6).                      
00109      12  DMPF-RETURN-CODE          PIC XX.                        
00110          88  DMPF-SUCCESSFUL-EDIT              VALUE 'OK'.        
00111          88  DMPF-POLICY-NOT-INPUT             VALUE '01'.        
00112          88  DMPF-POLICY-NOT-FOUND             VALUE '02'.        
00113          88  DMPF-POLICY-INACTIVE              VALUE '03'.        
00114          88  DMPF-GEN-ERR-MPN-TBL              VALUE 'E1'.        
00115          88  DMPF-MPN-TABLE-NOT-OPEN           VALUE 'N1'.        
00116                                                                   
00117 * DLO002                                                          
00118  01  WS-DLO-INTERNAL-POLICY-FORM.                                 
00119      12  DIPF-POLICY-FORM-NUMBER   PIC X(6).                      
00120      12  DIPF-RETURN-CODE          PIC XX.                        
00121          88  DIPF-SUCCESSFUL-EDIT              VALUE 'OK'.        
00122          88  DIPF-POLICY-NOT-INPUT             VALUE '01'.        
00123          88  DIPF-POLICY-NOT-FOUND             VALUE '02'.        
00124          88  DIPF-POLICY-INACTIVE              VALUE '03'.        
00125          88  DIPF-GEN-ERR-PFA-TBL              VALUE 'E1'.        
00126          88  DIPF-PFA-TABLE-NOT-OPEN           VALUE 'N1'.        
00127                                                                   
00128 * DLO003                                                          
00129  01  WS-DLO-VALID-BANK-ID.                                        
00130      12  DVBI-BANK-ID              PIC X(5).                      
00131      12  DVBI-BANK-ID-TYPE         PIC X.                         
00132          88  DVBI-BILLING-BANK                 VALUE 'B'.         
00133          88  DVBI-PLAN-BANK                    VALUE 'P'.         
00134          88  DVBI-ENROLL-BANK                  VALUE 'E'.         
00135          88  DVBI-NEGOT-BANK                   VALUE 'N'.         
00136          88  DVBI-PROCESS-CENTER               VALUE 'X'.         
00137      12  DVBI-RETURN-CODE          PIC XX.                        
00138          88  DVBI-SUCCESSFUL-EDIT              VALUE 'OK'.        
00139          88  DVBI-BANK-ID-NOT-INPUT            VALUE '01'.        
00140          88  DVBI-BANK-ID-TYPE-NOT-INPUT       VALUE '02'.        
00141          88  DVBI-BANK-ID-TYPE-INVALID         VALUE '03'.        
00142          88  DVBI-BANK-ID-NOT-FOUND            VALUE '04'.        
00143          88  DVBI-BANK-ID-INACTIVE             VALUE '05'.        
00144          88  DVBI-BANK-ID-TYPE-INCORRECT       VALUE '06'.        
00145          88  DVBI-GEN-ERR-BNK-TBL              VALUE 'E1'.        
00146          88  DVBI-BNK-TABLE-NOT-OPEN           VALUE 'N1'.        
00147                                                                   
00148 * DLO007                                                          
00149  01  WS-DLO-CERT-UNDRWRTR-ASSIGN.                                 
00150      12  DCUA-RES-STATE            PIC XX.                        
00151      12  DCUA-BEN-TYPE             PIC XX.                        
00152      12  DCUA-SYS-IDENT            PIC X.                         
00153          88  DCUA-CLAIM                        VALUE 'C'.         
00154          88  DCUA-PREMIUM                      VALUE 'P'.         
00155      12  DCUA-COVERAGE-RULE        PIC X.                         
00156      12  DCUA-CLIENT-ID            PIC X(10).                     
00157      12  DCUA-EFF-DATE             PIC X(8).                      
00158      12  DCUA-PREV-CERT-NUMBER     PIC X(11).                     
00159      12  DCUA-RETURN-CODE          PIC XX.                        
00160          88  DCUA-SUCCESSFUL-PROCESS           VALUE 'OK'.        
00161      12  DCUA-CERT-NUMBER          PIC X(11).                     
00162      12  DCUA-UNDRWRTR-CODE        PIC XX.                        
00163      12  DCUA-UNDRWRTR-GROUP-CODE  PIC X.                         
00164                                                                   
00165 * DLO011                                                          
00166  01  WS-DLO-PREM-RATES-BEN-TYPE.                                  
00167      12  DPRB-RATE-CODE            PIC X(4).                      
00168      12  DPRB-COVERAGE-CATEG       PIC X.                         
00169      12  DPRB-RETURN-CODE          PIC XX.                        
00170          88  DPRB-SUCCESSFUL-PROCESS           VALUE 'OK'.        
00171          88  DPRB-NO-RATE-CODE                 VALUE '01'.        
00172          88  DPRB-NO-COV-CATEG                 VALUE '02'.        
00173          88  DPRB-NO-TOT-RATE-CODE             VALUE '03'.        
00174          88  DPRB-NO-COV-CAT-RATE-BEN-TYP      VALUE '04'.        
00175          88  DPRB-GEN-ERR-TTR-TBL              VALUE 'E1'.        
00176          88  DPRB-GEN-ERR-RTT-TBL              VALUE 'E2'.        
00177          88  DPRB-TTR-TABLE-NOT-OPEN           VALUE 'N1'.        
00178          88  DPRB-RTT-TABLE-NOT-OPEN           VALUE 'N2'.        
00179      12  DPRB-TOT-RATE-AMT         PIC 9(3)V9(3).                 
00180      12  DPRB-COV-CAT-RATE         PIC 9(2)V9(3).                 
00181      12  DPRB-BEN-TYPE             PIC XX.                        
00182                                                                   
00183 * DLO017                                                          
00184  01  WS-DLO-VALID-RATE-CODE.                                      
00185      12  DVRC-RATE-CODE            PIC X(4).                      
00186      12  DVRC-RETURN-CODE          PIC XX.                        
00187          88  DVRC-SUCCESSFUL-EDIT              VALUE 'OK'.        
00188          88  DVRC-RATE-CODE-NOT-INPUT          VALUE '01'.        
00189          88  DVRC-RATE-CODE-NOT-FOUND          VALUE '02'.        
00190          88  DVRC-RATE-CODE-INACTIVE           VALUE '03'.        
00191          88  DVRC-GEN-ERR-TTR-TBL              VALUE 'E1'.        
00192          88  DVRC-TTR-TABLE-NOT-OPEN           VALUE 'N1'.        
00193                                                                   
00194 * DLO019                                                          
00195  01  WS-DLO-ORIG-COV-CAT.                                         
00196      12  DOCC-ORIG-BEN-TYPE        PIC XX.                        
00197      12  DOCC-RETURN-CODE          PIC XX.                        
00198      12  DOCC-ORIG-COV-CAT         PIC X.                         
00199      12  DOCC-UNDERWITER           PIC XX.                        
00200      12  DOCC-UNDW-GROUP-CODE      PIC X.                         
00201      12  DOCC-FILLER               PIC X(6).                      
00202                                                                   
00203 * DLO021                                                          
00204  01  WS-DLO-VALID-LOAN-OFFICER.                                   
00205      12  DVLO-LOAN-OFFICER.                                       
00206          16  DVLO-BILL-TYPE        PIC X.                         
00207          16  DVLO-BILL-SOURCE      PIC X.                         
00208          16  DVLO-BATCH-TYPE       PIC X.                         
00209      12  DVLO-RETURN-CODE          PIC XX.                        
00210          88  DVLO-SUCCESSFUL-EDIT              VALUE 'OK'.        
00211          88  DVLO-LOAN-OFFCR-NOT-INPUT         VALUE '01'.        
00212          88  DVLO-BILL-TYPE-INVALID            VALUE '02'.        
00213          88  DVLO-BILL-SOURCE-INVALID          VALUE '03'.        
00214          88  DVLO-BATCH-TYPE-M-INVALID         VALUE '04'.        
00215          88  DVLO-BATCH-TYPE-D-INVALID         VALUE '05'.        
00216                                                                   
00217 * DLO022                                                          
00218  01  WS-DLO-VALID-STATE-CODE.                                     
00219      12  DVSC-STATE-CODE           PIC XX.                        
00220      12  DVSC-RETURN-CODE          PIC XX.                        
00221          88  DVSC-SUCCESSFUL-EDIT              VALUE 'OK'.        
00222                                                                   
00223 * DLO023                                                          
00224  01  WS-DLO-VALID-CODE-VALUES.                                    
00225      12  DVCV-SYSTEM-ID            PIC XX.                        
00226          88  DVCV-CLAIMS                       VALUE 'CL'.        
00227          88  DVCV-CREDIT                       VALUE 'CR'.        
00228      12  DVCV-RECORD-TYPE          PIC XX.                        
00229          88  DVCV-DENIAL-CODE                  VALUE 'DN'.        
00230          88  DVCV-EOB-CODE                     VALUE 'EO'.        
00231          88  DVCV-ICD9-CODE                    VALUE 'I9'.        
00232          88  DVCV-OCCUPATION-CODE              VALUE 'OC'.        
00233          88  DVCV-PAYMENT-CODE                 VALUE 'PN'.        
00234          88  DVCV-CAUSE-CODE                   VALUE 'CS'.        
00235          88  DVCV-COV-CAT-CODE                 VALUE 'CV'.        
00236          88  DVCV-RES-STATE-CODE               VALUE 'ST'.        
00237      12  DVCV-RECORD-KEY           PIC X(6).                      
00238      12  DVCV-RETURN-CODE          PIC XX.                        
00239          88  DVCV-SUCCESSFUL-EDIT              VALUE 'OK'.        
00240          88  DVCV-NO-RECORD-TYPE-INPUT         VALUE '01'.        
00241          88  DVCV-NO-SYSTEM-ID-INPUT           VALUE '02'.        
00242          88  DVCV-NO-RECORD-KEY-INPUT          VALUE '03'.        
00243          88  DVCV-INVALID-RECORD-KEY-INPUT     VALUE '04'.        
00244          88  DVCV-INVALID-SYSTEM-ID-INPUT      VALUE '05'.        
00245          88  DVCV-INVALID-RECORD-KEY-INPUT     VALUE '06'.        
00246      12  DVCV-CODE-DESC            PIC X(60).                     
00247      12  DVCV-GEN-DESC-1           PIC X(20).                     
00248      12  DVCV-GEN-DESC-2           PIC X(20).                     
00249      12  DVCV-GEN-DESC-3           PIC X(20).                     
00250                                                                   
       01  hldi-pass-area-len          pic s9(4) comp value +214.
       01  hldi-pass-area.
           03  PA-VIN                  PIC X(17).
           03  PA-ErrorCode            PIC X(10).
           03  PA-ErrorDesc            PIC X(30).
           03  PA-ModelYear            PIC 9(7).
           03  PA-MakeName             PIC X(50).
           03  PA-ModelName            PIC X(50).
           03  PA-SeriesName           PIC X(50).

100217 01  zipcd-pass-area-len         pic s9(4) comp value +67.
100217 01  zipcd-pass-area.
100217     03  PA-zip                  PIC X(5).
100217     03  PA-ErrorCode-zip        PIC X(10).
100217     03  PA-city                 PIC x(50).
100217     03  PA-state                PIC Xx.

020218 01  clmhs-pass-area-len         pic s9(4) comp value +40.
020218 01  clmhs-pass-area.
020218     03  pa-clmhs-state          pic xx.
020218     03  pa-account              pic x(10).
020218     03  pa-eff-dt               pic x(10).
020218     03  pa-cert-no              pic x(11).
020218     03  pa-clm-count            pic 9(5).

      ****  this is for aggregate report.  
      ****  Length is erpndb + 100 of passed-key-area
       01  ws-passed-key-length        pic s9(4) comp value +587.

       01  ws-pass-to-pemaggo.
           05  ws-passed-erpndb        pic x(585).
           05  ws-exceed-limit-yn      pic x.
           05  ws-has-claim-yn         pic x.

00251      EJECT                                                        
100703     COPY ERCBXRF.
100703
100703     COPY ERCCOMP.
100703
111504     COPY ERCAGTC.
00252      COPY ERCRESS.                                                
00253      EJECT                                                        
00254      COPY ERCRESC.                                                
00255      EJECT                                                        
00256      COPY ERCPLAN.                                                
00257      EJECT                                                        
00258      COPY ELCCERT.                                                
00259      EJECT                                                        
00260      COPY ERCMAIL.                                                
00261      EJECT                                                        
00262      COPY ERCPNDM.                                                
083106                                 COPY ERCLOFC.
                                       COPY ERCPDEF.
                                       COPY ELCSTATE.
042709     EJECT                                                        
042709     COPY ELCCRTT.                                                
041412     COPY ELCCRTO.
00264                                                                   
00265      COPY ELCDATE.                                                
00266                                                                   
00267      EJECT                                                        
00268      COPY ELCCALC.                                                
00269      EJECT                                                        
00270 ******************************************************************
00271 *** THE FOLLOWING AREA IS RESERVED FOR THE INFORMATION PASSED  ***
00272 *** FROM PROGRAM TO PROGRAM DURING THE EDITING PROCESS.  IF    ***
00273 *** ANY FIELD IS MODIFIED OR THE AREA IS EXTENDED, ALL PROGRAMS***
00274 *** CALLING EL050 MUST BE MODIFIED IDENTICALLY TO REFLECT THIS ***
00275 *** MODIFICATION.                                              ***
00276 *** ONCE COMPILED, THIS AREA IS CONSIDERED TO BE A 1036 BYTE   ***
00277 *** WORK AREA WITH A LENGTH IDENTICAL TO THE DFHCOMMAREA       ***
00278 ******************************************************************
00279      COPY ERCPNDB.                                                
00280                                                                   
00281 * COPYBOOK FOR ADDITIONAL DFHCOMMAREA WK-WORK-AREA.               
00282      COPY ELC50W1.                                                
00283                                                                   
00284      COPY ELCEDITC.                                               
00285 ******************************************************************
00286                                                                   
00287      EJECT                                                        
00288      COPY ELCDIFRD.                                               
00289                                                                   
00290      EJECT                                                        
00291  LINKAGE SECTION.                                                 
00292                                                                   
00293  01  DFHCOMMAREA                   PIC X(1036).                   
00294                                                                   
00295 *01 PARMLIST .                                                    
00296 *    02  FILLER                    PIC S9(8)   COMP.              
00297 *    02  ELCNTL-POINTER            PIC S9(8)   COMP.              
00298 *    02  ERACCT-POINTER            PIC S9(8)   COMP.              
00299 *    02  ERREIN-POINTER            PIC S9(8)   COMP.              
00300 *    02  ERCTBL-POINTER            PIC S9(8)   COMP.              
00301 *    02  ERFORM-POINTER            PIC S9(8)   COMP.              
00302 *    02  ELMSTR-POINTER            PIC S9(8)   COMP.              
00303 *    02  ERPLAN-POINTER            PIC S9(8)   COMP.              
00304                                                                   
00305      EJECT                                                        
00306      COPY ELCCNTL.                                                
00307      EJECT                                                        
00308      COPY ERCACCT.                                                
00309      EJECT                                                        
00310      COPY ERCREIN.                                                
00311      EJECT                                                        
00312      COPY ERCCTBL.                                                
00313      EJECT                                                        
00314      COPY ERCFORM.                                                
00315      EJECT                                                        
00316      COPY ELCMSTR.                                                
00317                                                                   
00318  01  PLAN-RECORDS.                                                
00319      12  LINK-LIFE-PLAN-RECORD   PIC X(420).                      
00320      12  LINK-AH-PLAN-RECORD     PIC X(420).                      
00321                                                                   
00322      EJECT                                                        
00323  PROCEDURE DIVISION.                                              
00324                                                                   
00325      MOVE DFHCOMMAREA            TO  PENDING-BUSINESS.            
00326                                                                   
00327      EXEC CICS  HANDLE CONDITION                                  
00328             ERROR    (9300-ABEND)                                 
00329             NOTOPEN  (3990-FILES-NOT-OPEN)                        
00330      END-EXEC.                                                    
00331                                                                   
00332  0000-COPY-PROCEDURE-DIV.                                         
00333                            COPY ELC50PD.                          
00334      EJECT                                                        
00335                                                                   
00336 **START***********************************************************
00337 *      CUSTOM CODING FOR CLIENT "DMD"            PROJECT# 6475   *
00338 *            PARAGRAPHS  6600 THRU 6700-COMM-EXIT                *
00339 ******************************************************************
00340 *                                                                 
00341  6600-GET-STATE-MUNICIPAL-TAX.                                    
00342                                                                   
00343      IF PB-COMPANY-ID NOT = 'DMD'                                 
00344          GO TO 6640-EXIT.                                         
00345                                                                   
00346      MOVE PB-COMPANY-CD       TO WS-ERRESS-COMPANY-CD.            
00347      MOVE PB-CERT-NO (1:2)    TO WS-ERRESS-RESIDENT-STATE.        
00348      MOVE PB-CERT-NO (4:1)    TO WS-ERRESS-COVERAGE-CAT.          
00349 *CONVERT PB-CERT-EFF-DT TO YYYYMMDD FORMATT TO READ MASTER        
00350 *                                                                 
00351 ***  Y2K PROJ 7744                                                
00352      MOVE PB-CERT-EFF-DT      TO DC-BIN-DATE-1.                   
00353      SET BIN-TO-GREG          TO TRUE.                            
00354      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
00355                                                                   
00356      MOVE DC-GREG-DATE-CYMD   TO WS-CONTRACT-DATE.                
00357                                                                   
00358      MOVE WS-CONTRACT-DATE    TO WS-ERRESS-EXPIRE-DT.             
00359      MOVE WS-ERRESS-KEY       TO WS-SAVE-ERRESS-KEY.              
00360                                                                   
00361      EXEC CICS HANDLE CONDITION                                   
00362          INVREQ    (6610-START-BROWSE)                            
00363          NOTOPEN   (6630-ERRESS-PROBLEM)                          
00364          NOTFND    (6630-END-OF-FILE)                             
00365      END-EXEC.                                                    
00366                                                                   
00367  6610-START-BROWSE.                                               
00368                                                                   
00369      EXEC CICS HANDLE CONDITION                                   
00370          NOTFND    (6630-END-OF-FILE)                             
00371          DISABLED  (6630-ERRESS-PROBLEM)                          
00372          NOTOPEN   (6630-ERRESS-PROBLEM)                          
00373          ENDFILE   (6630-END-OF-FILE)                             
00374      END-EXEC.                                                    
00375                                                                   
00376      EXEC CICS STARTBR                                            
00377          DATASET (FILE-ID-ERRESS)                                 
00378          RIDFLD  (WS-ERRESS-KEY)                                  
00379          GTEQ                                                     
00380      END-EXEC.                                                    
00381                                                                   
00382  6620-READ-NEXT-ERRESS-MASTER.                                    
00383                                                                   
00384      EXEC CICS HANDLE CONDITION                                   
00385          NOTFND    (6630-END-OF-FILE)                             
00386          ENDFILE   (6630-END-OF-FILE)                             
00387          DISABLED  (6630-ERRESS-PROBLEM)                          
00388          NOTOPEN   (6630-ERRESS-PROBLEM)                          
00389      END-EXEC.                                                    
00390                                                                   
00391      EXEC CICS READNEXT                                           
00392          DATASET (FILE-ID-ERRESS)                                 
00393          INTO    (RESIDENT-STATE-TAX-MASTER)                      
00394          RIDFLD  (WS-ERRESS-KEY)                                  
00395      END-EXEC.                                                    
00396                                                                   
00397      IF WS-SAVE-ERRESS-KEY (1:4) NOT = ERRESS-PRIMARY-KEY (1:4)   
00398         EXEC CICS ENDBR                                           
00399              DATASET (FILE-ID-ERRESS)                             
00400         END-EXEC                                                  
00401         GO TO 6630-END-OF-FILE.                                   
00402                                                                   
00403      IF WS-CONTRACT-DATE < RES-EXPIRE-DATE                        
00404         NEXT SENTENCE                                             
00405       ELSE                                                        
00406         GO TO 6620-READ-NEXT-ERRESS-MASTER.                       
00407                                                                   
00408 *IS IT LIFE(1) OR IS IT P&C(2) ?????                              
00409 *WS-SUB1 SELECTS LIFE OR P&C RES-TAX-RECORD-DATA OCCURRENCE OF MAS
00410 *                                                                 
00411      MOVE +2                     TO WS-SUB1.                      
00412                                                                   
00413      IF PB-ISSUE                                                  
00414      IF PB-I-POLICY-FORM-NO (6:1) = '1'                           
00415         MOVE +1                  TO WS-SUB1.                      
00416                                                                   
00417      IF PB-CANCELLATION                                           
00418      IF PB-C-POLICY-FORM-NO (6:1) = '1'                           
00419         MOVE +1                  TO WS-SUB1.                      
00420 *                                                                 
00421 *CONVERT ANNIVERSARY  DATE OF ACCOUNT MASTER TO YYYYMMDD FORMAT.  
00422 *                                                                 
00423 ***  Y2K PROJ 7744                                                
00424      MOVE AM-ANNIVERSARY-DATE     TO WS-NEXT-CONTRACT-DATE.       
00425 ***  Y2K PROJ 7744                                                
00426                                                                   
00427 *BUMP ANNIVERSARY DATE UP BY 1 YEAR IN WS-NEXT-CONTRACT-DATE      
00428 * TEST IF ANNIVERSARY DATE HAS EXISTED MORE THAN 1 YEAR           
00429 * TEST TO DETERMINE IF STATE AND MUNICIPAL RENEWAL PERCENT TAX APP
00430                                                                   
00431      ADD 1                    TO WS-YEAR-YY.                      
00432                                                                   
00433      IF WS-CONTRACT-DATE > WS-NEXT-CONTRACT-DATE                  
00434          MOVE RES-REN-STATE-TAX (WS-SUB1)                         
00435                               TO WS-RES-STATE-TAX                 
00436          MOVE RES-REN-MUNICIPAL-TAX (WS-SUB1)                     
00437                               TO WS-RES-MUNI-TAX                  
00438      ELSE                                                         
00439          MOVE RES-1YR-STATE-TAX (WS-SUB1)                         
00440                               TO WS-RES-STATE-TAX                 
00441          MOVE RES-1YR-MUNICIPAL-TAX (WS-SUB1)                     
00442                               TO WS-RES-MUNI-TAX.                 
00443                                                                   
00444 *COMPUTE TAX DEPENDING ON NON ZERO AMOUNTS                        
00445                                                                   
00446      IF PB-CANCELLATION                                           
00447          GO TO 6640-CANCEL-TAX.                                   
00448                                                                   
00449      IF PB-I-LF-PREMIUM-AMT > ZEROS                               
00450          COMPUTE PB-I-STATE-TAX ROUNDED =                         
00451                                    PB-I-LF-PREMIUM-AMT            
00452                                  * WS-RES-STATE-TAX               
00453          COMPUTE PB-I-MUNI-TAX ROUNDED =                          
00454                                    PB-I-LF-PREMIUM-AMT            
00455                                  * WS-RES-MUNI-TAX                
00456      ELSE                                                         
00457      IF PB-I-AH-PREMIUM-AMT > ZEROS                               
00458          COMPUTE PB-I-STATE-TAX ROUNDED =                         
00459                                    PB-I-AH-PREMIUM-AMT            
00460                                  * WS-RES-STATE-TAX               
00461          COMPUTE PB-I-MUNI-TAX ROUNDED =                          
00462                                    PB-I-AH-PREMIUM-AMT            
00463                                  * WS-RES-MUNI-TAX.               
00464                                                                   
00465      EXEC CICS ENDBR                                              
00466           DATASET (FILE-ID-ERRESS)                                
00467      END-EXEC.                                                    
00468                                                                   
00469      GO TO 6640-EXIT.                                             
00470                                                                   
00471  6640-CANCEL-TAX.                                                 
00472      MOVE CM-POLICY-FORM-NO    TO PB-C-POLICY-FORM-NO.            
00473                                                                   
00474      IF PB-C-LF-CANCEL-AMT > ZEROS                                
00475          COMPUTE PB-CI-STATE-TAX ROUNDED =                        
00476                                    PB-C-LF-CANCEL-AMT             
00477                                  * WS-RES-STATE-TAX               
00478          COMPUTE PB-CI-MUNI-TAX ROUNDED =                         
00479                                    PB-C-LF-CANCEL-AMT             
00480                                 *  WS-RES-MUNI-TAX                
00481      ELSE                                                         
00482      IF PB-C-AH-CANCEL-AMT > ZEROS                                
00483          COMPUTE PB-CI-STATE-TAX ROUNDED =                        
00484                                    PB-C-AH-CANCEL-AMT             
00485                                  * WS-RES-STATE-TAX               
00486          COMPUTE PB-CI-MUNI-TAX ROUNDED =                         
00487                                    PB-C-AH-CANCEL-AMT             
00488                                 *  WS-RES-MUNI-TAX.               
00489                                                                   
00490      EXEC CICS ENDBR                                              
00491           DATASET (FILE-ID-ERRESS)                                
00492      END-EXEC.                                                    
00493                                                                   
00494      GO TO 6640-EXIT.                                             
00495                                                                   
00496  6630-END-OF-FILE.                                                
00497      MOVE ER-8002              TO WS-ERROR.                       
00498      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00499                                                                   
00500      GO TO 6640-EXIT.                                             
00501                                                                   
00502  6630-ERRESS-PROBLEM.                                             
00503      MOVE ER-8044              TO WS-ERROR.                       
00504      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00505                                                                   
00506  6640-EXIT.                                                       
00507      EXIT.                                                        
00508                                                                   
00509      EJECT                                                        
00510  6650-GET-RES-STATE-COMM.                                         
00511                                                                   
00512      IF PB-COMPANY-ID NOT = 'DMD'                                 
00513          GO TO 6700-COMM-EXIT.                                    
00514                                                                   
00515      MOVE PB-COMPANY-CD       TO WS-ERRESC-COMPANY-CD.            
00516      MOVE PB-SV-CARRIER       TO WS-ERRESC-CARRIER.               
00517      MOVE PB-SV-GROUPING      TO WS-ERRESC-GROUP.                 
00518      MOVE PB-SV-STATE         TO WS-ERRESC-STATE.                 
00519      MOVE PB-ACCOUNT          TO WS-ERRESC-ACCOUNT.               
00520      MOVE AM-AGT (SUB1)       TO WS-ERRESC-AGENT.                 
00521      MOVE PB-CERT-NO (1:2)    TO WS-ERRESC-RES-STATE.             
00522                                                                   
00523 *CONVERT PB-CERT-EFF-DT TO YYYYMMDD FORMAT TO READ MASTER         
00524 *                                                                 
00525 ***  Y2K PROJ 7744                                                
00526      MOVE PB-CERT-EFF-DT      TO DC-BIN-DATE-1.                   
00527      SET BIN-TO-GREG          TO TRUE.                            
00528      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
00529                                                                   
00530      MOVE DC-GREG-DATE-CYMD   TO WS-CONTRACT-DATE.                
00531 ***  Y2K PROJ 7744                                                
00532                                                                   
00533      MOVE WS-CONTRACT-DATE    TO WS-ERRESC-EXPIRE-DT.             
00534                                                                   
00535      EXEC CICS HANDLE CONDITION                                   
00536          INVREQ    (6660-START-BROWSE)                            
00537          DISABLED  (6690-ERRESC-PROBLEM)                          
00538          NOTOPEN   (6690-ERRESC-PROBLEM)                          
00539          NOTFND    (6690-END-OF-FILE)                             
00540      END-EXEC.                                                    
00541                                                                   
00542      EXEC CICS RESETBR                                            
00543          DATASET (FILE-ID-ERRESC)                                 
00544          RIDFLD  (WS-ERRESC-KEY)                                  
00545      END-EXEC.                                                    
00546                                                                   
00547  6660-START-BROWSE.                                               
00548                                                                   
00549      EXEC CICS HANDLE CONDITION                                   
00550          NOTFND    (6690-END-OF-FILE)                             
00551          DISABLED  (6690-ERRESC-PROBLEM)                          
00552          NOTOPEN   (6690-ERRESC-PROBLEM)                          
00553          ENDFILE   (6690-END-OF-FILE)                             
00554      END-EXEC.                                                    
00555                                                                   
00556      EXEC CICS STARTBR                                            
00557          DATASET (FILE-ID-ERRESC)                                 
00558          RIDFLD  (WS-ERRESC-KEY)                                  
00559          GTEQ                                                     
00560      END-EXEC.                                                    
00561                                                                   
00562      MOVE WS-ERRESC-KEY           TO ERRESC-RECORD-KEY.           
00563                                                                   
00564  6670-READ-NEXT-ERRESC-MASTER.                                    
00565                                                                   
00566      EXEC CICS HANDLE CONDITION                                   
00567          NOTFND    (6690-END-OF-FILE)                             
00568          DISABLED  (6690-ERRESC-PROBLEM)                          
00569          NOTOPEN   (6690-ERRESC-PROBLEM)                          
00570          ENDFILE   (6690-END-OF-FILE)                             
00571      END-EXEC.                                                    
00572                                                                   
00573      EXEC CICS READNEXT                                           
00574          DATASET (FILE-ID-ERRESC)                                 
00575          INTO    (ACCOUNT-RESIDENT-ST-COMMISSION)                 
00576          RIDFLD  (ERRESC-RECORD-KEY)                              
00577      END-EXEC.                                                    
00578                                                                   
00579      IF ERRESC-RECORD-KEY (1:32) = WS-ERRESC-KEY (1:32)           
00580         NEXT SENTENCE                                             
00581      ELSE                                                         
00582         EXEC CICS ENDBR                                           
00583              DATASET (FILE-ID-ERRESC)                             
00584         END-EXEC                                                  
00585         GO TO 6690-END-OF-FILE.                                   
00586                                                                   
00587      IF WS-CONTRACT-DATE < RESC-EFFECTIVE-DATE                    
00588         EXEC CICS ENDBR                                           
00589              DATASET (FILE-ID-ERRESC)                             
00590         END-EXEC                                                  
00591         GO TO 6690-END-OF-FILE.                                   
00592                                                                   
00593      IF WS-CONTRACT-DATE < RESC-EXPIRE-DATE                       
00594         NEXT SENTENCE                                             
00595       ELSE                                                        
00596         GO TO 6670-READ-NEXT-ERRESC-MASTER.                       
00597                                                                   
00598 * SEARCH FOR MATCHING CATEGORY CODE ENTRY                         
00599 *                                                                 
00600      MOVE ZEROS               TO WS-SUB WS-SUB1.                  
00601                                                                   
00602      PERFORM 6680-SEARCH-CATEGORY THRU 6680-EXIT.                 
00603                                                                   
00604      IF WS-SUB1 > ZERO                                            
00605          IF RESC-COMMISSION-PER (WS-SUB) IS NUMERIC               
00606              PERFORM 6680-SET-COMMISSION                          
00607          ELSE                                                     
00608              PERFORM 6680-SET-TABLE-CODE.                         
00609                                                                   
00610      EXEC CICS ENDBR                                              
00611           DATASET (FILE-ID-ERRESC)                                
00612      END-EXEC.                                                    
00613                                                                   
00614      GO TO 6700-COMM-EXIT.                                        
00615                                                                   
00616  6680-SET-TABLE-CODE.                                             
00617      IF RESC-COMMISSION-TAB (WS-SUB) EQUAL SPACES                 
00618         MOVE ZEROS             TO WS-SUB1                         
00619      ELSE                                                         
00620 * DEFAULT TO AH-COVERAGE TO READ COMMISSION TABLE(CTBL)           
00621 * TO COMPUTE AH COMMISSION.                                       
00622         MOVE WS-AH-COMP-CD                TO CTBL-BEN-CODE        
00623         MOVE PB-I-AH-TERM                 TO WS-WORK-TERM         
00624         MOVE PB-AH-OVERRIDE-L1            TO CTBL-BEN-TYPE        
00625         MOVE RESC-COMMISSION-TAB (WS-SUB) TO CTBL-TABLE           
00626         PERFORM 6680-READ-CTBL-TABLE                              
00627         IF WS-CTBL-READ-SW NOT = 'Y'                              
00628            MOVE ZEROS          TO WS-SUB1.                        
00629                                                                   
00630  6680-READ-CTBL-TABLE.                                            
00631 *                                                                 
00632 * WHEN CTBL-SW = '0540' READ COMMISSION TABLE(CTBL)               
00633 * TO COMPUTE LIFE COMMISSION                                      
00634                                                                   
00635      IF CTBL-SW = '0540'                                          
00636         MOVE WS-LF-COMP-CD       TO CTBL-BEN-CODE                 
00637         MOVE PB-I-LF-TERM        TO WS-WORK-TERM                  
00638         MOVE PB-LIFE-OVERRIDE-L1 TO CTBL-BEN-TYPE.                
00639                                                                   
00640      PERFORM 8300-READ-CTBL-TABLE THRU 8300-EXIT.                 
00641                                                                   
00642      IF CTBL-TABLE-FOUND                                          
00643         IF CTBL-SW = '0530'                                       
00644              COMPUTE WS-COMM-CK-AMT = PB-I-AH-BENEFIT-AMT         
00645                                     * PB-I-AH-TERM                
00646              PERFORM 8800-GET-COMP-RATE THRU 8800-EXIT            
00647              MOVE WS-WK-RATE TO WS-COMMISSION PB-I-AH-COMMISSION  
00648         ELSE                                                      
00649              MOVE PB-I-LF-BENEFIT-AMT TO WS-COMM-CK-AMT           
00650              PERFORM 8800-GET-COMP-RATE THRU 8800-EXIT            
00651              MOVE WS-WK-RATE TO WS-COMMISSION PB-I-LIFE-COMMISSION
00652      ELSE                                                         
00653         IF FIRST-TIME-THROUGH                                     
00654             GO TO 6680-READ-CTBL-TABLE                            
00655         ELSE                                                      
00656             MOVE ZEROS         TO WS-SUB1.                        
00657                                                                   
00658  6680-SET-COMMISSION.                                             
00659      IF RESC-COMMISSION-PER (WS-SUB) EQUAL ZEROS                  
00660            MOVE ZEROS          TO WS-SUB1                         
00661        ELSE                                                       
00662            MOVE RESC-COMMISSION-PER (WS-SUB)                      
00663                                TO WS-COMMISSION                   
00664            IF CTBL-SW = '0530'                                    
00665                MOVE WS-COMMISSION TO PB-I-AH-COMMISSION           
00666            ELSE                                                   
00667                MOVE WS-COMMISSION TO PB-I-LIFE-COMMISSION.        
00668                                                                   
00669  6680-SEARCH-CATEGORY.                                            
00670      ADD +1                   TO WS-SUB.                          
00671      IF WS-SUB > +12                                              
00672          GO TO 6680-EXIT.                                         
00673                                                                   
00674      IF PB-CERT-NO (4:1) = RESC-COVERAGE-CAT (WS-SUB)             
00675           MOVE WS-SUB         TO WS-SUB1                          
00676           GO TO 6680-EXIT.                                        
00677                                                                   
00678      GO TO 6680-SEARCH-CATEGORY.                                  
00679                                                                   
00680  6680-EXIT.                                                       
00681      EXIT.                                                        
00682                                                                   
00683  6690-END-OF-FILE.                                                
00684      GO TO 6700-COMM-EXIT.                                        
00685                                                                   
00686  6690-ERRESC-PROBLEM.                                             
00687       MOVE ER-8030              TO WS-ERROR.                      
00688       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   
00689                                                                   
00690       GO TO 6700-COMM-EXIT.                                       
00691                                                                   
00692  6700-COMM-EXIT.                                                  
00693      EXIT.                                                        
00694                                                                   
00695  7000-GET-RATE.                                                   
00696      EXEC CICS LINK                                               
00697          PROGRAM    ('ELRATE')                                    
00698          COMMAREA   (CALCULATION-PASS-AREA)                       
00699          LENGTH     (CP-COMM-LENGTH)                              
00700      END-EXEC.                                                    
00701                                                                   
00702  7000-EXIT.                                                       
00703      EXIT.                                                        
00704      EJECT                                                        
00705  7100-FIND-BENEFIT-IN-STATE.                                      
00706      MOVE 'N'                    TO BEN-SEARCH-SW.                
00707                                                                   
00708      PERFORM 7100-BENEFIT-DUMMY THRU 7100-DUMMY-EXIT              
00709          VARYING SUB3 FROM 1 BY 1 UNTIL                           
00710             ((SUB3 GREATER 50) OR                                 
00711               ((CF-ST-BENEFIT-CD (SUB3) = WS-BEN-CD)              
00712                AND (WS-LOOKUP-TYPE = CF-ST-BENEFIT-KIND (SUB3)))).
00713                                                                   
00714      IF SUB3 NOT = 51                                             
00715          MOVE 'Y'                TO BEN-SEARCH-SW.                
00716                                                                   
00717      GO TO 7199-EXIT.                                             
00718                                                                   
00719  7100-BENEFIT-DUMMY.                                              
00720                                                                   
00721  7100-DUMMY-EXIT.                                                 
00722      EXIT.                                                        
00723                                                                   
00724  7199-EXIT.                                                       
00725      EXIT.                                                        
00726      EJECT                                                        
00727  7200-FIND-BENEFIT.                                               
00728      MOVE 'N'                    TO BEN-SEARCH-SW.                
00729                                                                   
00730      PERFORM 7200-BENEFIT-DUMMY THRU 7200-DUMMY-EXIT              
00731          VARYING SUB3 FROM 1 BY 1 UNTIL                           
00732             ((SUB3 GREATER 8) OR                                  
00733             (CF-BENEFIT-NUMERIC (SUB3) = WS-BEN-CD)).             
00734                                                                   
00735      IF SUB3 NOT = 9                                              
00736          MOVE 'Y'                TO BEN-SEARCH-SW.                
00737                                                                   
00738      GO TO 7200-EXIT.                                             
00739                                                                   
00740  7200-BENEFIT-DUMMY.                                              
00741                                                                   
00742  7200-DUMMY-EXIT.                                                 
00743      EXIT.                                                        
00744                                                                   
00745  7200-EXIT.                                                       
00746      EXIT.                                                        
00747      EJECT                                                        
050311 7250-LOOK-FOR-RESCINDED-CLAIM.
050311     MOVE PB-COMPANY-CD          TO MSTR-COMP-CD
050311                                    W-CL-COMP-CD.
050311     MOVE PB-CERT-NO             TO MSTR-CERT-NO
050311                                    W-CL-CERT-NO.
050311
050311     EXEC CICS HANDLE CONDITION
050311         NOTFND  (7250-CONTINUE)
050311         ENDFILE (7250-CONTINUE)
050311     END-EXEC.
050311
050311     EXEC CICS IGNORE CONDITION
050311         DUPKEY
050311     END-EXEC.
050311
050311     EXEC CICS STARTBR
050311         DATASET   (CLMS-ID)
050311         RIDFLD    (ELMSTR-KEY)
050311         GENERIC   EQUAL
050311         KEYLENGTH (ELMSTR-LENGTH)
050311     END-EXEC.
050311
050311     MOVE 'Y'                    TO W-BROWSE-SW.
050311
050311 7250-NEXT-CLAIM.
050311
050311     EXEC CICS READNEXT
050311         DATASET   (CLMS-ID)
050311         RIDFLD    (ELMSTR-KEY)
050311         SET       (ADDRESS OF CLAIM-MASTER)
050311     END-EXEC.
050311
050311     IF MSTR-COMP-CD NOT = W-CL-COMP-CD  OR
040914        MSTR-CERT-NO(1:10) NOT = W-CL-CERT-NO(1:10)
050311         GO TO 7250-CONTINUE
050311     END-IF.
050311
050311     IF PB-SV-CARRIER  NOT = CL-CARRIER        OR
050311        PB-SV-GROUPING NOT = CL-CERT-GROUPING  OR
050311        PB-SV-STATE    NOT = CL-CERT-STATE     OR
050311        PB-ACCOUNT     NOT = CL-CERT-ACCOUNT   OR
050311        PB-CERT-EFF-DT NOT = CL-CERT-EFF-DT
050311         GO TO 7250-NEXT-CLAIM
050311     END-IF.
050311
050311     IF (CL-CLAIM-TYPE = PB-AH-OVERRIDE-L1)
050311        OR (CL-CLAIM-TYPE = 'I')
050311        OR (CL-CLAIM-TYPE = 'G')
052614        OR (CL-CLAIM-TYPE = 'F')
022122        OR (CL-CLAIM-TYPE = 'B')
022122        OR (CL-CLAIM-TYPE = 'H')
050311          IF CL-DENIAL-TYPE = '2' OR '3' OR '4'
050311              MOVE 'Y' TO WS-AH-CLAIM-RESCINDED
050311              GO TO 7250-CONTINUE
050311          END-IF
050311     END-IF.
050311
050311     GO TO 7250-NEXT-CLAIM.
050311
050311 7250-CONTINUE.
050311
050311     IF W-BROWSE-SW = 'Y'
050311         EXEC CICS ENDBR
050311             DATASET   (CLMS-ID)
050311         END-EXEC
050311         MOVE 'N' TO W-BROWSE-SW
050311     END-IF.
050311
050311 7250-EXIT.
050311     EXIT.
050311     EJECT
00748  7300-CHECK-FOR-OPEN-CLMS.                                        
00749                                                                   
062904*    MOVE PB-COMPANY-ID          TO CNTL-COMP-ID.                 
062904*    MOVE SPACES                 TO CNTL-ACCESS.                  
062904*    MOVE '1'                    TO CNTL-REC-TYPE.                
062904*    MOVE +0                     TO CNTL-SEQ-NO.                  
062904                                                                  
062904*    MOVE '3'                    TO RC-SW-1.                      
062904*    PERFORM 9000-READ-CONTROL THRU 9000-EXIT.                    
062904                                                                  
062904*    IF NOT CF-COMPANY-MASTER                                     
062904*         PERFORM 0099-COMPANY-NOT-FOUND                          
062904*         GO TO 7300-EXIT.                                        
00761                                                                   
00762      MOVE PB-COMPANY-CD          TO MSTR-COMP-CD                  
00763                                     W-CL-COMP-CD.                 
00764      MOVE PB-CERT-NO             TO MSTR-CERT-NO                  
00765                                     W-CL-CERT-NO.                 
00766                                                                   
00767      EXEC CICS HANDLE CONDITION                                   
00768          NOTFND  (7300-CONTINUE)                                  
00769          ENDFILE (7300-CONTINUE)                                  
00770      END-EXEC.                                                    
00771                                                                   
00772      EXEC CICS IGNORE CONDITION                                   
00773          DUPKEY                                                   
00774      END-EXEC.                                                    
00775                                                                   
00776      EXEC CICS STARTBR                                            
00777          DATASET   (CLMS-ID)                                      
00778          RIDFLD    (ELMSTR-KEY)                                   
00779          GENERIC   EQUAL                                          
00780          KEYLENGTH (ELMSTR-LENGTH)                                
00781      END-EXEC.                                                    
00782                                                                   
00783      MOVE 'Y'                    TO W-BROWSE-SW.                  
00784                                                                   
00785  7300-NEXT-CLAIM.                                                 
00786                                                                   
00787      EXEC CICS READNEXT                                           
00788          DATASET   (CLMS-ID)                                      
00789          RIDFLD    (ELMSTR-KEY)                                   
00790          SET       (ADDRESS OF CLAIM-MASTER)                      
00791      END-EXEC.                                                    
00792                                                                   
00793      IF MSTR-COMP-CD NOT = W-CL-COMP-CD  OR                       
040914        MSTR-CERT-NO(1:10) NOT = W-CL-CERT-NO(1:10)                           
00795          GO TO 7300-CONTINUE.                                     
00796                                                                   
00797      IF PB-SV-CARRIER  NOT = CL-CARRIER        OR                 
00798         PB-SV-GROUPING NOT = CL-CERT-GROUPING  OR                 
00799         PB-SV-STATE    NOT = CL-CERT-STATE     OR                 
00800         PB-ACCOUNT     NOT = CL-CERT-ACCOUNT   OR                 
00801         PB-CERT-EFF-DT NOT = CL-CERT-EFF-DT                       
00802          GO TO 7300-NEXT-CLAIM.                                   
00803                                                                   
121802*    IF CL-CLAIM-TYPE = CF-AH-OVERRIDE-L1 OR
062904     IF (CL-CLAIM-TYPE = PB-AH-OVERRIDE-L1)
121802        OR (CL-CLAIM-TYPE = 'I')
100703        OR (CL-CLAIM-TYPE = 'G')
052614        OR (CL-CLAIM-TYPE = 'F')
022122        OR (CL-CLAIM-TYPE = 'B')
022122        OR (CL-CLAIM-TYPE = 'H')
00805         IF CLAIM-IS-OPEN                                         
040914           IF PB-C-AH-CANCEL-DT <= CL-PAID-THRU-DT
040914             AND PB-C-AH-CANCEL-DT > SPACES
040914              MOVE ER-2896        TO WS-ERROR                      
040914              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
040914           END-IF
012010           EVALUATE TRUE
012010             WHEN WS-ST-REFUND-CLAIM-FLAG = '1'
012010                 MOVE ER-2884      TO WS-ERROR
012010             WHEN WS-ST-REFUND-CLAIM-FLAG = '2'
011211                 MOVE ER-2887      TO WS-ERROR
012010             WHEN WS-ST-REFUND-CLAIM-FLAG = '3'
012010                 MOVE ER-2886      TO WS-ERROR
011211             WHEN WS-ST-REFUND-CLAIM-FLAG = '4'
011211                 MOVE ER-2888      TO WS-ERROR
011211             WHEN WS-ST-REFUND-CLAIM-FLAG = '5'
011211                 MOVE ER-2885      TO WS-ERROR
012010             WHEN OTHER
012010                 MOVE ER-2768      TO WS-ERROR
012010           END-EVALUATE
060710           IF PB-C-AH-CANCEL-VOIDED OR 
011211              ((WS-ST-REFUND-CLAIM-FLAG = '2' OR '3' OR '4') AND 
060710                NOT AH-COVERAGE-CANCELLED)
012010               MOVE SPACES TO WS-ST-REFUND-CLAIM-FLAG
012010               MOVE ER-2768      TO WS-ERROR
012010           END-IF
00807            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
011211           IF (WS-ST-REFUND-CLAIM-FLAG = '2' OR '3' OR '4') AND 
012010              PB-FORCE-CODE NOT EQUAL '8'
012010               MOVE SPACES TO WS-ST-REFUND-CLAIM-FLAG
012010           END-IF
012010           PERFORM 8190-UPDATE-REFUND-CLAIM-FLAG THRU 8190-EXIT
040914           GO TO 7300-NEXT-CLAIM
062904        ELSE
011211           IF CL-DENIAL-TYPE = '2' OR '3' OR '4'
011211*****THIS IS A RESCISSION OR REFORMATION
051914              IF AH-COVERAGE-CANCELLED 
051914                 IF WS-ST-CAUSAL-STATE = ('A' OR 'B')
051914                    MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1                        
051914                    MOVE 1                   TO DC-ELAPSED-DAYS                      
051914                    MOVE ZERO                TO DC-ELAPSED-MONTHS                    
051914                    SET BIN-PLUS-ELAPSED     TO TRUE                                 
051914                    PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                         
051914                    IF NO-CONVERSION-ERROR                                           
051914                       IF NOT (DC-BIN-DATE-2 = PB-C-AH-CANCEL-DT 
051914                        OR PB-C-AH-CANCEL-DT = CL-CERT-EFF-DT)
051914                          MOVE ER-2897 TO WS-ERROR
051914                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
051914                          GO TO 7300-NEXT-CLAIM
051914                       END-IF
051914                    END-IF
051914                 ELSE
011211                   IF PB-C-AH-CANCEL-DT <> CL-CERT-EFF-DT
091114                      IF MSTR-CERT-NO = W-CL-CERT-NO
011211                         MOVE ER-2890 TO WS-ERROR
011211                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040914                         GO TO 7300-NEXT-CLAIM
091114                      ELSE
091114                         MOVE ER-2898 TO WS-ERROR
091114                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
091114                         GO TO 7300-NEXT-CLAIM
091114                      END-IF
051914                   END-IF
011211                 END-IF
051914              END-IF
011211           ELSE
111113              IF CL-DENIAL-TYPE = '1'  AND
111113                 CL-TOTAL-PAID-AMT = 0
111113                    GO TO 7300-NEXT-CLAIM
111113              END-IF
011211              IF PB-C-AH-CANCEL-DT <= CL-PAID-THRU-DT
042412               AND PB-C-AH-CANCEL-DT > SPACES
011211                 MOVE ER-2756        TO WS-ERROR                      
011211                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
040914                 GO TO 7300-NEXT-CLAIM
011211              END-IF
011211           END-IF
011211        END-IF
00811      ELSE                                                         
00812 *       IF CL-CLAIM-TYPE = CF-LIFE-OVERRIDE-L1                   
062904        IF CL-CLAIM-TYPE = PB-LIFE-OVERRIDE-L1                   
100518          OR (CL-CLAIM-TYPE = 'O')
00813            IF (CLAIM-IS-OPEN)
072215              and ((pb-ci-lf-cancel-amt + pb-c-lf-cancel-amt)
072215                 <> zeros
072215              or (pb-ci-ah-cancel-amt + pb-c-ah-cancel-amt)
072215                 <> zeros)
011211              MOVE ER-2889      TO WS-ERROR
011211              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
040914              GO TO 7300-NEXT-CLAIM
062904           ELSE
011211              IF CL-DENIAL-TYPE = '2' OR '3' OR '4'
072215*****THIS IS A RESCISSION OR REFORMATION or refor to rescission
011211                 IF (LF-COVERAGE-CANCELLED AND
011211                     PB-C-LF-CANCEL-DT <> CL-CERT-EFF-DT)
091114                      IF MSTR-CERT-NO = W-CL-CERT-NO
011211                         MOVE ER-2890 TO WS-ERROR
011211                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040914                         GO TO 7300-NEXT-CLAIM
091114                      ELSE
091114                         MOVE ER-2898 TO WS-ERROR
091114                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
091114                         GO TO 7300-NEXT-CLAIM
091114                      END-IF
011211                 END-IF
011211              ELSE
111113                 IF (CL-DENIAL-TYPE = '1')   *> regular denial
111113                    and (CL-TOTAL-PAID-AMT = 0)
111113                    GO TO 7300-NEXT-CLAIM
111113                 END-IF
011211                 IF (
072215                      (AH-COVERAGE-CANCELLED)
072215                      AND (PB-C-AH-CANCEL-DT <> CL-INCURRED-DT)
072215                      AND (WS-AH-CLAIM-RESCINDED <> 'Y')
072215                      and (pb-ci-ah-cancel-amt +
072215                         pb-c-ah-cancel-amt) <> zeros
072215                    )
072215                               OR
011211                    (
072215                      (LF-COVERAGE-CANCELLED)
072215                      AND (PB-C-LF-CANCEL-DT <> CL-INCURRED-DT)
072215                      AND (WS-ST-REFUND-CLAIM-FLAG = '4')
072215                      and (pb-ci-lf-cancel-amt +
072215                         pb-c-lf-cancel-amt) <> zeros
072215                    )
100518                       IF CL-CLAIM-TYPE = 'O'
100518                          MOVE ER-2998 TO WS-ERROR
100518                       ELSE
011211                          MOVE ER-2965 TO WS-ERROR
100518                       END-IF
011211                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040914                       GO TO 7300-NEXT-CLAIM
011211                 END-IF
011211                 IF LF-COVERAGE-CANCELLED
050311                  IF CL-TOTAL-PAID-AMT > 0
011211                    IF WS-ST-REFUND-CLAIM-FLAG = '4' 
011211                      IF PB-C-LF-CANCEL-DT = CL-INCURRED-DT
011211                        MOVE ER-2891 TO WS-ERROR
011211                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040914                        GO TO 7300-NEXT-CLAIM
011211                      END-IF
011211                    ELSE
050614****only show this error if cert num matches exactly
050614                      IF PB-CERT-NO = CL-CERT-NO
100518                         IF CL-CLAIM-TYPE = 'O'
100518                            MOVE ER-2895 TO WS-ERROR
100518                         ELSE
011211                            MOVE ER-2892 TO WS-ERROR
100518                         END-IF
011211                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040914                         GO TO 7300-NEXT-CLAIM
050614                      END-IF
011211                    END-IF
050311                  END-IF
011211                 END-IF
062904              END-IF
062904           END-IF
062904        END-IF
062904     END-IF
00817                                                                   
00818      GO TO 7300-NEXT-CLAIM.                                       
00819                                                                   
00820  7300-CONTINUE.                                                   
00821                                                                   
00822      IF W-BROWSE-SW = 'Y'                                         
00823          EXEC CICS ENDBR                                          
00824              DATASET   (CLMS-ID)                                  
00825          END-EXEC.                                                
00826                                                                   
00827  7300-EXIT.                                                       
00828      EXIT.                                                        
00829      EJECT                                                        

092618 7320-CHECK-FOR-OPEN-CLMS-issue.
092618
092618     move ' '                    to w-browse-sw
092618     MOVE PB-COMPANY-CD          TO MSTR-COMP-CD
092618                                    W-CL-COMP-CD
092618     MOVE PB-CERT-NO(1:10)       TO MSTR-CERT-NO
092618                                    W-CL-CERT-NO
092618
092618     EXEC CICS HANDLE CONDITION                                   
092618         NOTFND  (7320-CONTINUE)                                  
092618         ENDFILE (7320-CONTINUE)                                  
092618     END-EXEC.                                                    
092618
092618     EXEC CICS IGNORE CONDITION                                   
092618         DUPKEY                                                   
092618     END-EXEC.                                                    
092618
092618     EXEC CICS STARTBR                                            
092618         DATASET   (CLMS-ID)                                      
092618         RIDFLD    (ELMSTR-KEY)                                   
092618         GENERIC   EQUAL                                          
092618         KEYLENGTH (ELMSTR-LENGTH)                                
092618     END-EXEC.                                                    
092618
092618     MOVE 'Y'                    TO W-BROWSE-SW.                  
092618
092618 7320-NEXT-CLAIM.                                                 
092618
092618     EXEC CICS READNEXT                                           
092618         DATASET   (CLMS-ID)                                      
092618         RIDFLD    (ELMSTR-KEY)                                   
092618         SET       (ADDRESS OF CLAIM-MASTER)                      
092618     END-EXEC.                                                    
092618
092618     IF MSTR-COMP-CD NOT = W-CL-COMP-CD  OR                       
092618        MSTR-CERT-NO(1:10) NOT = W-CL-CERT-NO(1:10)                           
092618         GO TO 7320-CONTINUE.                                     
092618
092618     IF PB-SV-CARRIER  NOT = CL-CARRIER        OR                 
092618        PB-SV-GROUPING NOT = CL-CERT-GROUPING  OR                 
092618        PB-SV-STATE    NOT = CL-CERT-STATE     OR                 
092618        PB-ACCOUNT     NOT = CL-CERT-ACCOUNT   OR                 
092618        PB-CERT-EFF-DT NOT = CL-CERT-EFF-DT                       
092618         GO TO 7320-NEXT-CLAIM.                                   
092618
092618     IF CLAIM-IS-OPEN                                         
092618        MOVE ER-3273             TO WS-ERROR                      
092618        PERFORM 9900-ERROR-FORMAT
092618                                 THRU 9900-EXIT             
092618     END-IF
101019     if pb-i-entry-status = 'V'
101019        IF CLAIM-IS-OPEN
101019             OR
101019           CL-LAST-PMT-DT NOT = LOW-VALUES
101019             OR
101019           CL-LAST-PMT-AMT > 0
101019           MOVE ER-3281          TO WS-ERROR
101019           PERFORM 9900-ERROR-FORMAT
101019                                 THRU 9900-EXIT
101019        END-IF
101019     END-IF
092618
092618     GO TO 7320-NEXT-CLAIM.                                       
092618
092618 7320-CONTINUE.                                                   
092618
092618     IF W-BROWSE-SW = 'Y'                                         
092618         EXEC CICS ENDBR                                          
092618             DATASET   (CLMS-ID)                                  
092618         END-EXEC.                                                
092618
092618 7320-EXIT.                                                       
092618     EXIT.                                                        
092618     EJECT                                                        

00831  7400-GET-REMAIN-TERM.                                            
00832      EXEC CICS LINK                                               
00833          PROGRAM    ('ELRTRM')                                    
00834          COMMAREA   (CALCULATION-PASS-AREA)                       
00835          LENGTH     (CP-COMM-LENGTH)                              
00836      END-EXEC.                                                    
00837                                                                   
00838  7400-EXIT.                                                       
00839      EXIT.                                                        
00840      EJECT                                                        
00841                                                                   
00842  7500-GET-REMAIN-AMOUNT.                                          
00843      EXEC CICS LINK                                               
00844          PROGRAM    ('ELRAMT')                                    
00845          COMMAREA   (CALCULATION-PASS-AREA)                       
00846          LENGTH     (CP-COMM-LENGTH)                              
00847      END-EXEC.                                                    
00848                                                                   
00849  7500-EXIT.                                                       
00850      EXIT.                                                        
00851      EJECT                                                        
00852                                                                   
00853  7600-GET-REFUND.                                                 
00854      EXEC CICS LINK                                               
00855          PROGRAM    ('ELRFND')                                    
00856          COMMAREA   (CALCULATION-PASS-AREA)                       
00857          LENGTH     (CP-COMM-LENGTH)                              
00858      END-EXEC.                                                    
00859                                                                   
00860  7600-EXIT.                                                       
00861      EXIT.                                                        
00862      EJECT                                                        
00863  7700-CALL-DL1.                                                   
00864      EXEC CICS LINK                                               
00865          PROGRAM    ('DLO001')                                    
00866          COMMAREA   (WS-DLO-MASTER-POLICY-FORM)                   
00867          LENGTH     (DLO001-COMM-LENGTH)                          
00868      END-EXEC.                                                    
00869                                                                   
00870  7700-EXIT.                                                       
00871      EXIT.                                                        
00872                                                                   
00873  7710-CALL-DL2.                                                   
00874      EXEC CICS LINK                                               
00875          PROGRAM    ('DLO002')                                    
00876          COMMAREA   (WS-DLO-INTERNAL-POLICY-FORM)                 
00877          LENGTH     (DLO002-COMM-LENGTH)                          
00878      END-EXEC.                                                    
00879                                                                   
00880  7710-EXIT.                                                       
00881      EXIT.                                                        
00882                                                                   
00883  7720-CALL-DL3.                                                   
00884      EXEC CICS LINK                                               
00885          PROGRAM    ('DLO003')                                    
00886          COMMAREA   (WS-DLO-VALID-BANK-ID)                        
00887          LENGTH     (DLO003-COMM-LENGTH)                          
00888      END-EXEC.                                                    
00889                                                                   
00890  7720-EXIT.                                                       
00891      EXIT.                                                        
00892                                                                   
00893  7730-CALL-DL7.                                                   
00894      EXEC CICS LINK                                               
00895          PROGRAM    ('DLO007')                                    
00896          COMMAREA   (WS-DLO-CERT-UNDRWRTR-ASSIGN)                 
00897          LENGTH     (DLO007-COMM-LENGTH)                          
00898      END-EXEC.                                                    
00899                                                                   
00900  7730-EXIT.                                                       
00901      EXIT.                                                        
00902                                                                   
00903  7740-CALL-DL11.                                                  
00904      EXEC CICS LINK                                               
00905          PROGRAM    ('DLO011')                                    
00906          COMMAREA   (WS-DLO-PREM-RATES-BEN-TYPE)                  
00907          LENGTH     (DLO011-COMM-LENGTH)                          
00908      END-EXEC.                                                    
00909                                                                   
00910  7740-EXIT.                                                       
00911      EXIT.                                                        
00912                                                                   
00913  7750-CALL-DL17.                                                  
00914      EXEC CICS LINK                                               
00915          PROGRAM    ('DLO017')                                    
00916          COMMAREA   (WS-DLO-VALID-RATE-CODE)                      
00917          LENGTH     (DLO017-COMM-LENGTH)                          
00918      END-EXEC.                                                    
00919                                                                   
00920  7750-EXIT.                                                       
00921      EXIT.                                                        
00922                                                                   
00923  7755-CALL-DL19.                                                  
00924      EXEC CICS LINK                                               
00925          PROGRAM    ('DLO019')                                    
00926          COMMAREA   (WS-DLO-ORIG-COV-CAT)                         
00927          LENGTH     (DLO019-COMM-LENGTH)                          
00928      END-EXEC.                                                    
00929                                                                   
00930  7755-EXIT.                                                       
00931      EXIT.                                                        
00932                                                                   
00933  7760-CALL-DL21.                                                  
00934      EXEC CICS LINK                                               
00935          PROGRAM    ('DLO021')                                    
00936          COMMAREA   (WS-DLO-VALID-LOAN-OFFICER)                   
00937          LENGTH     (DLO021-COMM-LENGTH)                          
00938      END-EXEC.                                                    
00939                                                                   
00940  7760-EXIT.                                                       
00941      EXIT.                                                        
00942                                                                   
00943  7770-CALL-DL22.                                                  
00944      EXEC CICS LINK                                               
00945          PROGRAM    ('DLO022')                                    
00946          COMMAREA   (WS-DLO-VALID-STATE-CODE)                     
00947          LENGTH     (DLO022-COMM-LENGTH)                          
00948      END-EXEC.                                                    
00949                                                                   
00950  7770-EXIT.                                                       
00951      EXIT.                                                        
00952                                                                   
00953      EJECT                                                        
00954                                                                   
       7780-CALL-AGG.
           EXEC CICS LINK
               PROGRAM    ('ELAGGO')
               COMMAREA   (WS-PASS-to-pemaggo)
               LENGTH     (ws-passed-key-LENGTH)
           END-EXEC.

       7780-EXIT.
           EXIT.

       7785-CALL-HLDI.
           EXEC CICS LINK
               PROGRAM    ('ELHLDI')
               COMMAREA   (HLDI-PASS-AREA)
               LENGTH     (HLDI-PASS-AREA-LEN)
           END-EXEC.

       7785-EXIT.
           EXIT.

       7786-CALL-HLDIM.
           EXEC CICS LINK
               PROGRAM    ('ELHLDIM')
               COMMAREA   (HLDI-PASS-AREA)
               LENGTH     (HLDI-PASS-AREA-LEN)
           END-EXEC.

       7786-EXIT.
           EXIT.

100217 7790-CALL-ZIP-VERIFY.
100217
100217     EXEC CICS LINK
100217         PROGRAM    ('WSZIPCD')
100217         COMMAREA   (ZIPCD-PASS-AREA)
100217         LENGTH     (ZIPCD-PASS-AREA-LEN)
100217     END-EXEC.
100217
100217 7790-EXIT.
100217     EXIT.

020218 7795-call-sqlclmhs.
020218
020218     EXEC CICS LINK
020218         PROGRAM    ('SQLCLMHS')
020218         COMMAREA   (clmhs-PASS-AREA)
020218         LENGTH     (clmhs-PASS-AREA-LEN)
020218     END-EXEC.
020218
020218 7795-EXIT.
020218     EXIT.

00955  7800-REWRITE-CERT.                                               
00956 ******************************************************************
00957 *     IF THE ALTERNATE INDEXES CHANGE DELETE THE CERTIFICATE     *
00958 *        AND WRITE THE RECORD TO REBUILD THE ALTERNATE INDEXES.  *
00959 ******************************************************************
00960                                                                   
00961      IF CM-CONTROL-BY-NAME     = CS-CONTROL-BY-NAME      AND      
00962         CM-CONTROL-BY-SSN      = CS-CONTROL-BY-SSN       AND      
00963         CM-CONTROL-BY-CERT-NO  = CS-CONTROL-BY-CERT-NO   AND      
00964         CM-CONTROL-BY-MEMB     = CS-CONTROL-BY-MEMB               
00965                EXEC CICS REWRITE                                  
00966                    DATASET (CERT-ID)                              
00967                    FROM    (CERTIFICATE-MASTER)                   
00968                END-EXEC                                           
00969         GO TO 7800-EXIT.                                          
00970                                                                   
00971        PERFORM 7900-DELETE-CERT THRU 7900-EXIT.                   
00972        PERFORM 8000-WRITE-CERT  THRU 8000-EXIT.                   
00973                                                                   
00974  7800-EXIT.                                                       
00975       EXIT.                                                       
00976                                                                   
00977  7900-DELETE-CERT.                                                
00978      EXEC CICS DELETE                                             
00979          DATASET (CERT-ID)                                        
00980      END-EXEC.                                                    
00981                                                                   
00982  7900-EXIT.                                                       
00983      EXIT.                                                        
00984                                                                   
00985  8000-WRITE-CERT.                                                 
00986      EXEC CICS HANDLE CONDITION                                   
00987          DUPKEY    (8000-FREE-MAIN)                               
00988      END-EXEC.                                                    
00989                                                                   
00990      EXEC CICS WRITE                                              
00991          DATASET     (CERT-ID)                                    
00992          FROM        (CERTIFICATE-MASTER)                         
00993          RIDFLD      (ELCERT-KEY)                                 
00994      END-EXEC.                                                    
00995                                                                   
00996  8000-FREE-MAIN.                                                  
00997                                                                   
00998  8000-EXIT.                                                       
00999       EXIT.                                                       
01000       EJECT                                                       
01001                                                                   
01002  8100-READ-ERPNDM.                                                
01003      EXEC CICS HANDLE CONDITION                                   
01004           NOTFND     (8100-NOTFND)                                
01005           ENDFILE    (8100-NOTFND)                                
01006      END-EXEC.                                                    
01007                                                                   
01008      MOVE SPACES                 TO ERPNDM-FOUND.                 
01009      MOVE PB-CONTROL-PRIMARY     TO PM-CONTROL-PRIMARY.           
01010                                                                   
100217     EXEC CICS READ UPDATE
01012           DATASET   (FILE-ERPNDM)                                 
01013           RIDFLD    (PM-CONTROL-PRIMARY)                          
01014           INTO      (PENDING-MAILING-DATA)                        
01015      END-EXEC.                                                    
01016                                                                   
01017      GO TO 8100-EXIT.                                             
01018                                                                   
01019  8100-NOTFND.                                                     
01020                                                                   
01021      MOVE 'X'                    TO ERPNDM-FOUND.                 
01022      MOVE ER-2694                TO WS-ERROR.                     
01023      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE ER-2883                TO WS-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT

           .
01025  8100-EXIT.                                                       
01026      EXIT.                                                        

100217 8102-REWRITE-ERPNDM.
100217
100217     EXEC CICS REWRITE
100217          DATASET    (FILE-ERPNDM)
100217          FROM       (PENDING-MAILING-DATA)
100217     END-EXEC
100217
100217     .
100217 8102-EXIT.
100217     EXIT.

100217 8104-UNLOCK-ERPNDM.
100217
100217     EXEC CICS UNLOCK
100217          DATASET   (FILE-ERPNDM)
100217     END-EXEC
100217
100217     .
100217 8104-EXIT.                                                       
100217     EXIT.                                                        

01028  8110-READ-ERMAIL.                                                
01029      EXEC CICS HANDLE CONDITION                                   
01030           NOTFND     (8110-NOTFND)                                
01031           ENDFILE    (8110-NOTFND)                                
01032      END-EXEC.                                                    
01033                                                                   
01034      MOVE SPACES                 TO ERMAIL-FOUND.                 
01035      MOVE PB-CONTROL-BY-ACCOUNT  TO MA-CONTROL-PRIMARY.           
01036      MOVE PB-SV-CARRIER          TO MA-CARRIER.                   
01037      MOVE PB-SV-GROUPING         TO MA-GROUPING.                  
01038      MOVE PB-SV-STATE            TO MA-STATE.                     
01039                                                                   
01040      EXEC CICS READ                                               
01041           UPDATE                                                  
01042           DATASET   (FILE-ERMAIL)                                 
01043           RIDFLD    (MA-CONTROL-PRIMARY)                          
01044           INTO      (MAILING-DATA)                                
01045      END-EXEC.                                                    
01046                                                                   
01047      GO TO 8110-EXIT.                                             
01048                                                                   
01049  8110-NOTFND.                                                     
01050      MOVE 'X'                    TO ERMAIL-FOUND.                 
01051                                                                   
01052  8110-EXIT.                                                       
01053      EXIT.                                                        
01054                                                                   
01055  8120-REWRITE-ERMAIL.                                             
01056      EXEC CICS REWRITE                                            
01057           DATASET    (FILE-ERMAIL)                                
01058           FROM       (MAILING-DATA)                               
01059      END-EXEC.                                                    
01060                                                                   
01061  8120-EXIT.                                                       
01062      EXIT.                                                        
01063                                                                   
01064  8130-WRITE-ERMAIL.                                               
01065      EXEC CICS HANDLE CONDITION                                   
01066           DUPKEY   (8130-EXIT)                                    
01067      END-EXEC.                                                    
01068                                                                   
01069      EXEC CICS WRITE                                              
01070           DATASET     (FILE-ERMAIL)                               
01071           RIDFLD      (MA-CONTROL-PRIMARY)                        
01072           FROM        (MAILING-DATA)                              
01073      END-EXEC.                                                    
01074                                                                   
01075  8130-EXIT.                                                       
01076      EXIT.                                                        
01077                                                                   
01078  8140-UNLOCK-ERMAIL.                                              
01079      EXEC CICS UNLOCK                                             
01080           DATASET   (FILE-ERMAIL)                                 
01081      END-EXEC.                                                    
01082                                                                   
01083  8140-EXIT.                                                       
01084      EXIT.                                                        
01085                                                                   
020118 8145-read-elcrtt-upd.
020118
020118     move ' '                    to elcrtt-found-sw
020118                                    elcrtt-update-sw
020118     EXEC CICS HANDLE CONDITION
020118         NOTFND   (8145-exit)
020118     END-EXEC
020118
020118     MOVE CM-CONTROL-PRIMARY TO WS-ELCRTT-PRIMARY
020118     MOVE 'C'                TO WS-ELCRTT-REC-TYPE
020118
020118     EXEC CICS READ UPDATE
020118         DATASET  (CRTT-ID)
020118         RIDFLD   (WS-ELCRTT-KEY)
020118         INTO     (CERTIFICATE-TRAILERS)
020118     END-EXEC
020118
020118     set elcrtt-found to true
020118
020118     .
020118 8145-exit.
020118     exit.

020118 8150-UPDATE-ELCRTT.
020118
020118     if elcrtt-found
020118         EXEC CICS REWRITE
020118            DATASET  (CRTT-ID)
020118            FROM     (CERTIFICATE-TRAILERS)
020118         END-EXEC
020118     ELSE
020118        EXEC CICS WRITE
020118           DATASET  (CRTT-ID)
020118           RIDFLD   (WS-ELCRTT-KEY)
020118           FROM     (CERTIFICATE-TRAILERS)
020118        END-EXEC
020118     end-if
020118
020118     .
020118 8150-EXIT.                                                       
020118     EXIT.                                                        
020118                                                                  
020118 8155-unlock-elcrtt.
020118
020118     EXEC CICS UNLOCK
020118        DATASET  (CRTT-ID)
020118     END-EXEC
020118
020118     .
020118 8155-exit.
020118     exit.
042709                                                                  
012010 8190-UPDATE-REFUND-CLAIM-FLAG.
012010     EXEC CICS HANDLE CONDITION
012010         NOTFND   (8190-NOTFND)
012010     END-EXEC.
012010
012010     MOVE CM-CONTROL-PRIMARY TO WS-ELCRTT-PRIMARY
012010     MOVE 'C'                TO WS-ELCRTT-REC-TYPE
012010
012010     EXEC CICS READ
012010         UPDATE
012010         DATASET  (CRTT-ID)
012010         RIDFLD   (WS-ELCRTT-KEY)
012010         INTO     (CERTIFICATE-TRAILERS)
012010     END-EXEC.
012010
012010     MOVE WS-ST-REFUND-CLAIM-FLAG TO CS-REFUND-CLAIM-FLAG.
012010     EXEC CICS REWRITE
012010        DATASET  (CRTT-ID)
012010        FROM     (CERTIFICATE-TRAILERS)
012010     END-EXEC.
012010
012010     GO TO 8190-EXIT.
012010
012010 8190-NOTFND.
012010
012010     MOVE SPACES       TO CERTIFICATE-TRAILERS.
012010     MOVE 'CS'         TO CS-RECORD-ID.
012010     MOVE CM-CONTROL-PRIMARY TO WS-ELCRTT-PRIMARY.
012010     MOVE 'C'          TO WS-ELCRTT-REC-TYPE.
012010     MOVE WS-ELCRTT-KEY TO CS-CONTROL-PRIMARY.
012010     MOVE WS-ST-REFUND-CLAIM-FLAG TO CS-REFUND-CLAIM-FLAG.
012010     EXEC CICS WRITE
012010        DATASET  (CRTT-ID)
012010        RIDFLD   (WS-ELCRTT-KEY)
012010        FROM     (CERTIFICATE-TRAILERS)
012010     END-EXEC.
012010
012010 8190-EXIT.                                                       
012010     EXIT.                                                        
012010                                                                  
121712 8195-UPDATE-AGE-FLAGS.
121712     EXEC CICS HANDLE CONDITION
121712         NOTFND   (8195-NOTFND)
121712     END-EXEC.
121712
121712     MOVE CM-CONTROL-PRIMARY TO WS-ELCRTT-PRIMARY
121712     MOVE 'C'                TO WS-ELCRTT-REC-TYPE
121712
121712     EXEC CICS READ
121712         UPDATE
121712         DATASET  (CRTT-ID)
121712         RIDFLD   (WS-ELCRTT-KEY)
121712         INTO     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
121712
121712     IF WS-INS-AGE-SET = 'Y'
121712        MOVE WS-INS-AGE-DEFAULTED TO CS-INS-AGE-DEFAULT-FLAG
121712     END-IF
121712     IF WS-JNT-AGE-SET = 'Y'
121712        MOVE WS-JNT-AGE-DEFAULTED TO CS-JNT-AGE-DEFAULT-FLAG
121712     END-IF
121712
121712     EXEC CICS REWRITE
121712        DATASET  (CRTT-ID)
121712        FROM     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
121712
121712     GO TO 8195-EXIT.
121712
121712 8195-NOTFND.
121712
121712     MOVE SPACES       TO CERTIFICATE-TRAILERS.
121712     MOVE 'CS'         TO CS-RECORD-ID.
121712     MOVE CM-CONTROL-PRIMARY TO WS-ELCRTT-PRIMARY.
121712     MOVE 'C'          TO WS-ELCRTT-REC-TYPE.
121712     MOVE WS-ELCRTT-KEY TO CS-CONTROL-PRIMARY.
121712     MOVE WS-INS-AGE-DEFAULTED TO CS-INS-AGE-DEFAULT-FLAG.
121712     MOVE WS-JNT-AGE-DEFAULTED TO CS-JNT-AGE-DEFAULT-FLAG.
121712     EXEC CICS WRITE
121712        DATASET  (CRTT-ID)
121712        RIDFLD   (WS-ELCRTT-KEY)
121712        FROM     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
121712
121712 8195-EXIT.                                                       
121712     EXIT.                                                        
121712
01086  8200-READ-REIN-TABLE.                                            
01087      EXEC CICS HANDLE CONDITION                                   
01088           NOTFND   (8200-NOTFND)                                  
01089      END-EXEC.                                                    
01090                                                                   
01091      EXEC CICS READ                                               
01092          DATASET     (REIN-ID)                                    
01093          SET         (ADDRESS OF REINSURANCE-RECORD)              
01094          RIDFLD      (ERREIN-KEY)                                 
01095      END-EXEC.                                                    
01096                                                                   
01097      GO TO 8200-EXIT.                                             
01098                                                                   
01099  8200-NOTFND.                                                     
01100      MOVE 'N'             TO WS-REIN-READ-SW.                     
01101                                                                   
01102  8200-EXIT.                                                       
01103       EXIT.                                                       
01104       EJECT                                                       
01105                                                                   
01106  8300-READ-CTBL-TABLE.                                            
01107      EXEC CICS HANDLE CONDITION                                   
01108          NOTFND    (8300-NOTFND)                                  
01109          DISABLED  (8300-ERRESS-PROBLEM)                          
01110          NOTOPEN   (8300-ERRESS-PROBLEM)                          
01111          ENDFILE   (8300-NOTFND)                                  
01112      END-EXEC.                                                    
01113                                                                   
01114      EXEC CICS READ                                               
01115          DATASET     (CTBL-ID)                                    
01116          SET         (ADDRESS OF COMM-TABLE-RECORD)               
01117          RIDFLD      (ERCTBL-KEY)                                 
01118      END-EXEC.                                                    
01119                                                                   
01120      MOVE 'Y'                          TO WS-CTBL-READ-SW.        
01121                                                                   
01122      GO TO 8300-EXIT.                                             
01123                                                                   
01124  8300-NOTFND.                                                     
01125                                                                   
01126      IF CTBL-SW = '0530'                                          
01127          MOVE 'AA'                     TO WS-AH-COMP-CD           
01128          MOVE '0550'                   TO CTBL-SW                 
01129          MOVE '1'                      TO WS-CTBL-READ-SW         
01130          GO TO 8300-EXIT.                                         
01131                                                                   
01132      IF CTBL-SW = '0540'                                          
01133          MOVE 'AA'                     TO WS-LF-COMP-CD           
01134          MOVE '0550'                   TO CTBL-SW                 
01135          MOVE '1'                      TO WS-CTBL-READ-SW         
01136          GO TO 8300-EXIT.                                         
01137                                                                   
01138      IF CTBL-SW = '0550'                                          
01139          MOVE '2'                      TO WS-CTBL-READ-SW         
01140          GO TO 8300-EXIT.                                         
01141                                                                   
01142      GO TO 8300-EXIT.                                             
01143                                                                   
01144  8300-ERRESS-PROBLEM.                                             
01145                                                                   
01146       MOVE ER-8050              TO WS-ERROR.                      
01147       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   
01148                                                                   
01149      GO TO 8300-EXIT.                                             
01150                                                                   
01151  8300-EXIT.                                                       
01152       EXIT.                                                       
01153       EJECT                                                       
01154                                                                   
01155                                                                   
01156  8400-GETMAIN-ERFORM.                                             
01157                                                                   
01158      EXEC CICS   GETMAIN                                          
01159          LENGTH  (500)                                            
01160          SET     (ADDRESS OF FORM-MASTER)                         
01161      END-EXEC.                                                    
01162                                                                   
01163      SET ERFORM-POINTER TO ADDRESS OF FORM-MASTER.                
01164                                                                   
01165  8400-EXIT.                                                       
01166       EXIT.                                                       
01167                                                                   
01168  8450-READ-FORM-MASTER.                                           
01169                                                                   
01170      EXEC CICS  HANDLE CONDITION                                  
01171          NOTFND   (8450-FORM-NOT-FOUND)                           
01172          ENDFILE  (8450-FORM-NOT-FOUND)                           
01173      END-EXEC.                                                    
01174                                                                   
01175      EXEC CICS   READ                                             
01176          DATASET   (FORM-ID)                                      
01177          RIDFLD    (ERFORM-KEY)                                   
01178          INTO      (FORM-MASTER)                                  
01179          GTEQ                                                     
01180      END-EXEC.                                                    
01181                                                                   
01182      MOVE  'Y'           TO  FORM-MASTER-SW.                      
01183                                                                   
01184      GO TO  8450-EXIT.                                            
01185                                                                   
01186  8450-FORM-NOT-FOUND.                                             
01187                                                                   
01188      MOVE  'N'           TO  FORM-MASTER-SW.                      
01189                                                                   
01190  8450-EXIT.                                                       
01191       EXIT.                                                       
01192                                                                   
01193  8500-DATE-CONVERT.                                               
01194      EXEC CICS LINK                                               
01195          PROGRAM    ('ELDATCV')                                   
01196          COMMAREA   (DATE-CONVERSION-DATA)                        
01197          LENGTH     (DC-COMM-LENGTH)                              
01198      END-EXEC.                                                    
01199                                                                   
01200  8500-EXIT.                                                       
01201      EXIT.                                                        
01202      EJECT                                                        
01203                                                                   
01204  8555-RELOAD-CONTROL-FILE.                                        
01205      CONTINUE.                                                    
01206                                                                   
01207  8555-EXIT.                                                       
01208      EXIT.                                                        
01209                                                                   
01210  8557-GETMAIN-ERPLAN.                                             
01211                                                                   
01212      EXEC CICS GETMAIN                                            
01213          LENGTH    (840)                                          
01214          SET       (ADDRESS OF PLAN-RECORDS)                      
01215      END-EXEC.                                                    
01216                                                                   
01217      SET ERPLAN-POINTER  TO ADDRESS OF PLAN-RECORDS.              
01218                                                                   
01219  8557-EXIT.                                                       
01220      EXIT.                                                        
01221                                                                   
01222  8560-GETMAIN.                                                    
01223      EXEC CICS GETMAIN                                            
01224          LENGTH    (2000)                                         
01225          SET       (ADDRESS OF ACCOUNT-MASTER)                    
01226      END-EXEC.                                                    
01227                                                                   
01228      SET ERACCT-POINTER TO ADDRESS OF ACCOUNT-MASTER.             
01229                                                                   
01230  8560-EXIT.                                                       
01231      EXIT.                                                        
01232                                                                   
01233  8570-START-ACCOUNT-MASTER.                                       
01234      EXEC CICS HANDLE CONDITION                                   
01235          NOTFND    (0199-ACCT-NOT-FOUND)                          
01236          ENDFILE   (0199-ACCT-NOT-FOUND)                          
01237      END-EXEC.                                                    
01238                                                                   
01239      EXEC CICS STARTBR                                            
01240          DATASET    (ACCT-ID)                                     
01241          RIDFLD     (ERACCT-KEY)                                  
01242          GTEQ                                                     
01243      END-EXEC.                                                    
01244                                                                   
01245  8570-EXIT.                                                       
01246      EXIT.                                                        
01247                                                                   
01248  8580-READ-ACCOUNT-MASTER.                                        
01249      EXEC CICS HANDLE CONDITION                                   
01250          NOTFND    (0199-ACCT-NOT-FOUND)                          
01251          ENDFILE   (0199-ACCT-NOT-FOUND)                          
01252      END-EXEC.                                                    
01253                                                                   
01254      EXEC CICS READNEXT                                           
01255          DATASET     (ACCT-ID)                                    
01256          INTO        (ACCOUNT-MASTER)                             
01257          RIDFLD      (ERACCT-KEY)                                 
01258      END-EXEC.                                                    
01259                                                                   
01260  8580-EXIT.                                                       
01261      EXIT.                                                        
01262                                                                   
100703 8582-READ-COMPENSATION-MASTER.                                   
100703     EXEC CICS HANDLE CONDITION                                   
100703         NOTFND    (8582-COMP-NOT-FOUND)                          
100703         ENDFILE   (8582-COMP-NOT-FOUND)                          
100703     END-EXEC.                                                    
100703                                                                  
100703     EXEC CICS READ                                               
100703         DATASET     (COMP-ID)                                    
100703         INTO        (COMPENSATION-MASTER)                        
100703         RIDFLD      (ERCOMP-KEY)                                 
100703     END-EXEC.                                                    
100703                                                                  
100703     MOVE 'Y'                    TO COMP-MASTER-SW
100703                                                                  
100703     GO TO 8582-EXIT
100703     .                                                            
100703 8582-COMP-NOT-FOUND.                                             
100703                                                                  
100703     MOVE 'N'                    TO COMP-MASTER-SW.               
100703                                                                  
100703 8582-EXIT.                                                       
100703     EXIT.                                                        
100703                                                                  
111504 8583-READ-ERAGTC.

111504     EXEC CICS HANDLE CONDITION
111504         NOTFND    (8583-AGTC-NOT-FOUND)
111504         ENDFILE   (8583-AGTC-NOT-FOUND)
111504     END-EXEC.
111504
030905     EXEC CICS STARTBR
030905         DATASET    (AGTC-ID)
030905         RIDFLD     (ERAGTC-KEY)
030905         GTEQ
030905     END-EXEC.
030905
01248  8583-READ-ERAGTC-NEXT.

111504     EXEC CICS READNEXT
111504         DATASET     (AGTC-ID)
111504         INTO        (AGENT-COMMISSIONS)
111504         RIDFLD      (ERAGTC-KEY)
111504     END-EXEC.
111504                                                                  
111504*    MOVE 'Y'                    TO AGTC-MASTER-SW
111504                                                                  
111504     GO TO 8583-EXIT
111504     .                                                            
111504 8583-AGTC-NOT-FOUND.
111504                                                                  
111504     MOVE 'N'                    TO AGTC-MASTER-SW.
111504                                                                  
111504 8583-EXIT.
111504     EXIT.
100703                                                                  
100703 8584-READ-BANK-CROSS-REF.
100703     EXEC CICS HANDLE CONDITION                                   
100703         NOTFND    (8584-BXRF-NOT-FOUND)                          
100703         ENDFILE   (8584-BXRF-NOT-FOUND)                          
100703     END-EXEC.                                                    
100703                                                                  
100703     EXEC CICS READ                                               
100703         DATASET     (BXRF-ID)                                    
100703         INTO        (BANK-CROSS-REFERENCE)                       
100703         RIDFLD      (ERBXRF-KEY)                                 
100703         LENGTH      (BXRF-REC-LENGTH)
100703     END-EXEC.                                                    
100703                                                                  
100703     MOVE 'Y'                    TO BXRF-MASTER-SW
100703                                                                  
100703     GO TO 8584-EXIT
100703     .                                                            
100703 8584-BXRF-NOT-FOUND.                                             
100703                                                                  
100703     MOVE 'N'                    TO BXRF-MASTER-SW.               
100703                                                                  
100703 8584-EXIT.                                                       
100703     EXIT.                                                        
100703                                                                  
01263  8585-READ-PLAN-MASTER.                                           
01264                                                                   
01265      EXEC CICS HANDLE CONDITION                                   
01266          NOTFND    (8585-PLAN-NOT-FOUND)                          
01267          ENDFILE   (8585-PLAN-NOT-FOUND)                          
01268      END-EXEC.                                                    
01269                                                                   
01270      EXEC CICS READ                                               
01271          DATASET     (PLAN-ID)                                    
01272          INTO        (PLAN-MASTER)                                
01273          RIDFLD      (ERPLAN-KEY)                                 
01274      END-EXEC.                                                    
01275                                                                   
01276      MOVE 'Y'                    TO PLAN-MASTER-SW.               
01277                                                                   
01278      GO TO 8585-EXIT.                                             
01279                                                                   
01280  8585-PLAN-NOT-FOUND.                                             
01281                                                                   
01282      MOVE 'N'                    TO PLAN-MASTER-SW.               
01283                                                                   
01284  8585-EXIT.                                                       
01285      EXIT.                                                        
01286                                                                   
083106 8586-READ-LOAN-OFFICER.

083106     EXEC CICS READ                                               
083106         DATASET     (LOFC-ID)                                    
083106         INTO        (LOAN-OFFICER-MASTER)                        
083106         RIDFLD      (ERLOFC-KEY)                                 
083106         RESP        (WS-RESPONSE)
083106     END-EXEC
083106                                                                  
           IF WS-RESP-NORMAL
083106        MOVE 'Y'                 TO LOFC-MASTER-SW
           ELSE
              MOVE 'N'                 TO LOFC-MASTER-SW
           END-IF

           .
083106 8586-EXIT.                                                       
083106     EXIT.                                                        

       8587-START-ERPDEF.

           EXEC CICS STARTBR
              DATASET   (PDEF-ID)
              RIDFLD    (ERPDEF-KEY)
              GTEQ
              RESP      (WS-RESPONSE)
           END-EXEC

           IF WS-RESP-NORMAL
              MOVE 'Y'                 TO PDEF-MASTER-SW
           ELSE
              MOVE 'N'                 TO PDEF-MASTER-SW
           END-IF

           .
       8587-EXIT.
           EXIT.

       8588-READ-ERPDEF.

           EXEC CICS READNEXT
              DATASET   (PDEF-ID)
              RIDFLD    (ERPDEF-KEY)
              INTO      (PRODUCT-MASTER)
              RESP      (WS-RESPONSE)
           END-EXEC

           IF WS-RESP-NORMAL
              MOVE 'Y'                 TO PDEF-MASTER-SW
           ELSE
              MOVE 'N'                 TO PDEF-MASTER-SW
           END-IF

           .
       8588-EXIT.
           EXIT.

       8589-ENDBR-ERPDEF.

           EXEC CICS ENDBR
              DATASET   (PDEF-ID)
           END-EXEC

           .
       8589-EXIT.
           EXIT.

01287  8590-READ-CERT-MASTER.                                           
01288      IF PB-COMPANY-ID = 'UCL'                                     
01289          GO TO 8690-READ-CERT-MASTER-UCL.                         
01290                                                                   
01291      EXEC CICS HANDLE CONDITION                                   
01292           NOTFND    (0299-CERT-NOT-FOUND)                         
01293      END-EXEC.                                                    
01294                                                                   
01295      EXEC CICS READ                                               
01296          DATASET     (CERT-ID)                                    
01297          INTO        (CERTIFICATE-MASTER)                         
01298          RIDFLD      (ELCERT-KEY)                                 
01299          UPDATE                                                   
01300      END-EXEC.                                                    
01301                                                                   
01302  8590-EXIT.                                                       
01303      EXIT.                                                        
01304                                                                   
01305  8690-READ-CERT-MASTER-UCL.                                       
01306      EXEC CICS HANDLE CONDITION                                   
01307           NOTFND    (8700-READ-CERT-MASTER)                       
01308      END-EXEC.                                                    
01309                                                                   
01310  8690-READ-CERT-CONT.                                             
01311      EXEC CICS READ                                               
01312          DATASET     (CERT-ID)                                    
01313          INTO        (CERTIFICATE-MASTER)                         
01314          RIDFLD      (ELCERT-KEY)                                 
01315          UPDATE                                                   
01316      END-EXEC.                                                    
01317                                                                   
01318      GO TO 8590-EXIT.                                             
01319                                                                   
01320  8700-READ-CERT-MASTER.                                           
01321      MOVE PB-COMPANY-CD          TO CERT2-COMP-CD.                
01322      MOVE PB-CERT-NO             TO CERT2-CERT-NO.                
01323      EXEC CICS HANDLE CONDITION                                   
01324           DUPKEY    (8720-COMP-ACCOUNTS)                          
01325           ENDFILE   (8730-END-BROWSE)                             
01326           NOTFND    (8740-NOT-FOUND)                              
01327      END-EXEC.                                                    
01328                                                                   
01329      EXEC CICS STARTBR                                            
01330          DATASET     ('ELCERT5')                                  
01331          RIDFLD      (ELCERT2-KEY)                                
01332          EQUAL                                                    
01333      END-EXEC.                                                    
01334                                                                   
01335  8710-READ-NEXT.                                                  
01336      EXEC CICS READNEXT                                           
01337          DATASET     ('ELCERT5')                                  
01338          INTO        (CERTIFICATE-MASTER)                         
01339          RIDFLD      (ELCERT2-KEY)                                
01340      END-EXEC.                                                    
01341                                                                   
01342  8720-COMP-ACCOUNTS.                                              
01343      EXEC CICS ENTER                                              
01344           TRACEID (001)                                           
01345           FROM (PB-C-LAST-NAME)                                   
01346      END-EXEC.                                                    
01347                                                                   
01348      EXEC CICS ENTER                                              
01349           TRACEID (002)                                           
01350           FROM (CM-INSURED-LAST-NAME)                             
01351      END-EXEC.                                                    
01352                                                                   
01353      IF PB-CERT-NO NOT = CM-CERT-NO                               
01354          GO TO 8730-END-BROWSE.                                   
01355                                                                   
01356      IF PB-ACCOUNT = CM-ACCOUNT                                   
01357          IF PB-C-LAST-NAME = CM-INSURED-LAST-NAME                 
01358              MOVE ER-2799        TO WS-ERROR                      
01359              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
01360              MOVE CM-CERT-EFF-DT TO EC-AM-EFFECTIVE-DT            
01361              GO TO 8730-END-BROWSE.                               
01362                                                                   
01363      GO TO 8710-READ-NEXT.                                        
01364                                                                   
01365  8730-END-BROWSE.                                                 
01366      EXEC CICS ENDBR                                              
01367           DATASET    ('ELCERT5')                                  
01368      END-EXEC.                                                    
01369                                                                   
01370  8740-NOT-FOUND.                                                  
01371      GO TO 0299-CERT-NOT-FOUND.                                   
01372                                                                   
       8750-START-ELSTAT.

           EXEC CICS STARTBR
              DATASET   (STAT-ID)
              RIDFLD    (ELSTAT-KEY)
              GTEQ
              RESP      (WS-RESPONSE)
           END-EXEC

           IF WS-RESP-NORMAL
              MOVE 'Y'                 TO STAT-MASTER-SW
           ELSE
              MOVE 'N'                 TO STAT-MASTER-SW
           END-IF

           .
       8750-EXIT.
           EXIT.

       8752-READ-ELSTAT.

           EXEC CICS READNEXT
              DATASET   (STAT-ID)
              RIDFLD    (ELSTAT-KEY)
              INTO      (STATE-FILE)
              RESP      (WS-RESPONSE)
           END-EXEC

           IF WS-RESP-NORMAL
              MOVE 'Y'                 TO STAT-MASTER-SW
           ELSE
              MOVE 'N'                 TO STAT-MASTER-SW
           END-IF

           .
       8752-EXIT.
           EXIT.

01375  8800-GET-COMP-RATE.                                              
01376      MOVE +1 TO IB.                                               
01377      IF WS-COMM-CK-AMT GREATER CT-TBF (1)                         
01378          ADD +9 TO IB                                             
01379          IF WS-COMM-CK-AMT GREATER CT-TBF (2)                     
01380              ADD +9 TO IB                                         
01381              IF WS-COMM-CK-AMT GREATER CT-TBF (3)                 
01382                 MOVE ZEROS       TO WS-WK-RATE                    
01383                 GO TO 8800-EXIT.                                  
01384                                                                   
01385      IF PB-I-AGE GREATER CT-AGE (1)                               
01386          ADD +3 TO IB                                             
01387          IF PB-I-AGE GREATER CT-AGE (2)                           
01388              ADD +3 TO IB                                         
01389              IF PB-I-AGE GREATER CT-AGE (3)                       
01390                 MOVE ZEROS       TO WS-WK-RATE                    
01391                 GO TO 8800-EXIT.                                  
01392                                                                   
01393      IF WS-WORK-TERM GREATER CT-TRM (1)                           
01394          ADD +1 TO IB                                             
01395          IF WS-WORK-TERM GREATER CT-TRM (2)                       
01396              ADD +1 TO IB                                         
01397              IF WS-WORK-TERM GREATER CT-TRM (3)                   
01398                 MOVE ZEROS       TO WS-WK-RATE                    
01399                 GO TO 8800-EXIT.                                  
01400                                                                   
01401      IF CT-RT (IB) NOT NUMERIC                                    
01402          MOVE CT-RT-R (IB) TO CTBL-TABLE                          
01403          PERFORM 8300-READ-CTBL-TABLE THRU 8300-EXIT              
01404          GO TO 8800-GET-COMP-RATE.                                
01405                                                                   
01406      MOVE CT-RT (IB) TO WS-WK-RATE.                               
01407                                                                   
01408  8800-EXIT.                                                       
01409      EXIT.                                                        
01410      EJECT                                                        
01411  8900-ADDRESS-FILE.                                               
01412      IF ERACCT-FILE-ADDRESSED                                     
01413          SET ADDRESS OF ACCOUNT-MASTER TO ERACCT-POINTER          
01414          GO TO 8900-ADDRESS-EXIT.                                 
01415                                                                   
01416      IF ELCNTL-FILE-ADDRESSED                                     
01417          SET ADDRESS OF CONTROL-FILE   TO ELCNTL-POINTER          
01418          GO TO 8900-ADDRESS-EXIT.                                 
01419                                                                   
01420      IF ERPLAN-FILE-ADDRESSED                                     
01421          SET ADDRESS OF PLAN-RECORDS TO ERPLAN-POINTER            
01422          GO TO 8900-ADDRESS-EXIT.                                 
01423                                                                   
01424      IF ERFORM-FILE-ADDRESSED                                     
01425          SET ADDRESS OF FORM-MASTER TO ERFORM-POINTER             
01426          GO TO 8900-ADDRESS-EXIT.                                 
01427                                                                   
01428  8900-ADDRESS-EXIT.                                               
01429      EXIT.                                                        
01430                                                                   
01431  9000-READ-CONTROL.                                               
01432      EXEC CICS HANDLE CONDITION                                   
01433          NOTFND    (9000-NOTFND)                                  
01434      END-EXEC.                                                    
01435                                                                   
01436      EXEC CICS READ                                               
01437          DATASET     (CNTL-ID)                                    
01438          SET         (ADDRESS OF CONTROL-FILE)                    
01439          RIDFLD      (ELCNTL-KEY)                                 
01440          GTEQ                                                     
01441      END-EXEC.                                                    
01442                                                                   
01443      GO TO 9000-EXIT.                                             
01444                                                                   
01445  9000-NOTFND.                                                     
01446      IF RC-SW-1 = '1'                                             
01447         MOVE ER-2616              TO WS-ERROR                     
01448         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01449         GO TO 9000-EXIT.                                          
01450                                                                   
01451      IF RC-SW-1 = '2'                                             
01452         MOVE ER-2602              TO WS-ERROR                     
01453         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 
01454                                                                   
01455  9000-EXIT.                                                       
01456       EXIT.                                                       
01457                                                                   
01458      EJECT                                                        
01459  9100-READ-CONTROL.                                               
01460      EXEC CICS HANDLE CONDITION                                   
01461          NOTFND  (9100-NOTFND)                                    
01462          ENDFILE (9100-NOTFND)                                    
01463      END-EXEC.                                                    
01464                                                                   
01465      EXEC CICS READ                                               
01466          DATASET     (CNTL-ID)                                    
01467          INTO        (CONTROL-FILE)                               
01468          RIDFLD      (ELCNTL-KEY)                                 
01469          GTEQ                                                     
01470      END-EXEC.                                                    
01471                                                                   
01472      GO TO 9100-EXIT.                                             
01473                                                                   
01474  9100-NOTFND.                                                     

           IF RC-SW-2 = '0'
              MOVE '1'                  TO STATE-RECORD-ERROR
              GO TO 9100-EXIT
           END-IF

01475      IF RC-SW-2 = '1'                                             
01476         MOVE '1'                  TO LF-BENEFIT-ERROR             
01477         MOVE ER-2604              TO WS-ERROR                     
01478         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01479         GO TO 9100-EXIT.                                          
01480                                                                   
01481      IF RC-SW-2 = '2'                                             
01482         MOVE '1'                  TO AH-BENEFIT-ERROR             
01483         MOVE ER-2605              TO WS-ERROR                     
01484         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01485         GO TO 9100-EXIT.                                          
01486                                                                   
01487      IF RC-SW-2 = '3'                                             
01488          MOVE ER-2603           TO WS-ERROR                       
01489          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01490          GO TO 9100-EXIT.                                         
01491                                                                   
01492      IF RC-SW-2 = '4'                                             
01493          MOVE PB-I-LF-INPUT-CD  TO PB-I-LF-BENEFIT-CD             
01494          GO TO 9100-EXIT.                                         
01495                                                                   
01496      IF RC-SW-2 = '5'                                             
01497          MOVE PB-I-AH-INPUT-CD  TO WS-AH-INPUT-CD                 
01498          IF WS-AH-INPUT-CD-1 = SPACE                              
01499              MOVE WS-AH-INPUT-CD-2-3  TO PB-I-AH-BENEFIT-CD       
01500          ELSE                                                     
01501              MOVE PB-I-AH-INPUT-CD    TO PB-I-AH-BENEFIT-CD.      
01502                                                                   
01503  9100-EXIT.                                                       
01504       EXIT.                                                       
01505      EJECT                                                        
01506  9200-CONTROL-GETMAIN.                                            
01507      EXEC CICS GETMAIN                                            
01508           LENGTH   (750)                                          
01509           SET      (ADDRESS OF CONTROL-FILE)                      
01510      END-EXEC.                                                    
01511                                                                   
01512      SET ELCNTL-POINTER TO ADDRESS OF CONTROL-FILE.               
01513                                                                   
01514  9200-EXIT.                                                       
01515      EXIT.                                                        
01516                                                                   
01517  9250-UNLOCK-RETURN.                                              
01518      EXEC CICS UNLOCK                                             
01519          DATASET   (CERT-ID)                                      
01520      END-EXEC.                                                    
01521                                                                   
01522  9300-ABEND.                                                      
01523      MOVE DFHEIBLK               TO WS-ERROR-LINE.                
01524                                                                   
01525      EXEC CICS LINK                                               
01526          PROGRAM   ('EL004')                                      
01527          COMMAREA  (WS-ERROR-LINE)                                
01528          LENGTH    (72)                                           
01529      END-EXEC.                                                    
01530                                                                   
01531      MOVE WS-ERROR-LINE          TO PENDING-BUSINESS.             
01532                                                                   
01533      EXEC CICS ABEND                                              
01534          ABCODE ('EL50')                                          
01535      END-EXEC.                                                    
01536                                                                   
01537      EJECT                                                        
01538                                                                   
01539 ******************************************************************
01540 *                                                                *
01541 *            E R R O R   F O R M A T   R O U T I N E             *
01542 *                                                                *
01543 ******************************************************************
01544                                                                   
01545  9900-ERROR-FORMAT.                                               
01546                                                                   
01547      IF WS-ERROR  = PB-COMMON-ERROR (1) OR                        
01548                     PB-COMMON-ERROR (2) OR                        
01549                     PB-COMMON-ERROR (3) OR                        
01550                     PB-COMMON-ERROR (4) OR                        
01551                     PB-COMMON-ERROR (5) OR                        
01552                     PB-COMMON-ERROR (6) OR                        
01553                     PB-COMMON-ERROR (7) OR                        
01554                     PB-COMMON-ERROR (8) OR                        
01555                     PB-COMMON-ERROR (9) OR                        
01556                     PB-COMMON-ERROR (10)                          
01557         GO TO 9900-EXIT.                                          
01558                                                                   
01559      IF PB-NO-OF-ERRORS = +10                                     
01560         GO TO 9900-EXIT.                                          
01561                                                                   
01562      ADD +1  TO PB-NO-OF-ERRORS.                                  
01563      MOVE WS-ERROR               TO PB-COMMON-ERROR               
01564                                        (PB-NO-OF-ERRORS).         
01565  9900-EXIT.                                                       
01566      EXIT.                                                        
01567                                                                   
01568  9990-RETURN.                                                     
01569      IF BROWSE-STARTED                                            
01570         EXEC CICS ENDBR                                           
01571              DATASET  (ACCT-ID)                                   
01572         END-EXEC.                                                 
01573                                                                   
01574  9990-EXIT.                                                       
01575      EXIT.                                                        
01576                                                                   
061412
061412****Check for CRTO record, add one if not found.
061412 9991-CHECK-FOR-ELCRTO.
061412      EXEC CICS HANDLE CONDITION                                   
061412          NOTFND  (9991-CRTO-NOT-FOUND)                                    
061412          ENDFILE (9991-CRTO-NOT-FOUND)                                    
061412      END-EXEC.                                                    
061412     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO ELCRTO-KEY
062712     MOVE 'I'                    TO ELCRTO-RECORD-TYPE
061412     MOVE +0                     TO ELCRTO-SEQ-NO
061412
061412     EXEC CICS READ
061412        DATASET   ('ELCRTO')
061412        INTO      (ORIGINAL-CERTIFICATE)
061412        RIDFLD    (ELCRTO-KEY)
061412        GTEQ
061412        RESP      (WS-RESPONSE)
061412     END-EXEC
061412
061412     IF WS-RESP-NORMAL
061412        AND (OC-CONTROL-PRIMARY (1:33) =
061412                 PB-CONTROL-BY-ACCOUNT (1:33))
062712        AND (OC-RECORD-TYPE =  'I')
061412           IF (OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES)
011413               GO TO 9991-CHECK-INDICATORS
061412           ELSE
061412               SUBTRACT +1 FROM OC-KEY-SEQ-NO
061412               GO TO 9991-CRTO-ADD-REC
061412           END-IF
061412     END-IF.
061412     
061412 9991-CRTO-NOT-FOUND.
061412           
061412     MOVE SPACES              TO ORIGINAL-CERTIFICATE
061412     MOVE 'OC'                TO OC-RECORD-ID
061412     MOVE PB-CONTROL-BY-ACCOUNT (1:33)
061412                              TO OC-CONTROL-PRIMARY (1:33)
061412     MOVE +4096               TO OC-KEY-SEQ-NO.
061412
061412 9991-CRTO-ADD-REC.
061412
061412     MOVE 'E050'              TO OC-LAST-MAINT-BY
061412     MOVE EIBTIME             TO OC-LAST-MAINT-HHMMSS
061412     MOVE WS-CURRENT-BIN-DT   TO OC-LAST-MAINT-DT
062712     MOVE 'I'                 TO OC-RECORD-TYPE
061412     IF PB-ISSUE
062712         MOVE PB-I-INSURED-LAST-NAME TO OC-INS-LAST-NAME   
062712         MOVE PB-I-INSURED-FIRST-NAME TO OC-INS-FIRST-NAME  
062712         MOVE PB-I-INSURED-MIDDLE-INIT TO OC-INS-MIDDLE-INIT 
062712         MOVE PB-I-AGE               TO OC-INS-AGE         
062712         MOVE PB-I-JOINT-LAST-NAME   TO OC-JNT-LAST-NAME   
062712         MOVE PB-I-JOINT-FIRST-NAME  TO OC-JNT-FIRST-NAME  
062712         MOVE PB-I-JOINT-MIDDLE-INIT TO OC-JNT-MIDDLE-INIT 
062712         MOVE PB-I-JOINT-AGE         TO OC-JNT-AGE         
062712         MOVE PB-I-LF-BENEFIT-CD     TO OC-LF-BENCD        
062712         MOVE PB-I-LF-TERM           TO OC-LF-TERM         
062712         MOVE PB-I-LF-BENEFIT-AMT    TO OC-LF-BEN-AMT      
062712         MOVE PB-I-LF-PREMIUM-AMT    TO OC-LF-PRM-AMT      
062712         MOVE PB-I-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT  
062712         MOVE PB-I-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT  
062712         MOVE PB-I-LF-EXPIRE-DT      TO OC-LF-EXP-DT       
062712         IF PB-I-JOINT-COMMISSION > +0
062712             MOVE PB-I-JOINT-COMMISSION TO OC-LF-COMM-PCT
062712         ELSE
062712             MOVE PB-I-LIFE-COMMISSION  TO OC-LF-COMM-PCT
062712         END-IF
062712         MOVE LOW-VALUES             TO OC-LF-CANCEL-DT
062712         MOVE +0                     TO OC-LF-CANCEL-AMT
071712                                        OC-LF-ITD-CANCEL-AMT
062712         MOVE PB-I-AH-BENEFIT-CD     TO OC-AH-BENCD        
062712         MOVE PB-I-AH-TERM           TO OC-AH-TERM         
062712         MOVE PB-I-AH-BENEFIT-AMT    TO OC-AH-BEN-AMT      
062712         MOVE PB-I-AH-PREMIUM-AMT    TO OC-AH-PRM-AMT      
062712         MOVE PB-I-AH-EXPIRE-DT      TO OC-AH-EXP-DT       
062712         MOVE PB-I-AH-COMMISSION     TO OC-AH-COMM-PCT     
062712         MOVE PB-I-AH-CRIT-PER       TO OC-AH-CP           
062712         MOVE LOW-VALUES             TO OC-AH-CANCEL-DT
062712         MOVE +0                     TO OC-AH-CANCEL-AMT
071712                                        OC-AH-ITD-CANCEL-AMT
062712         MOVE PB-I-1ST-PMT-DT        TO OC-1ST-PMT-DT
011413         MOVE 'Y'                    TO OC-ISSUE-TRAN-IND
011413         MOVE 'N'                    TO OC-CANCEL-TRAN-IND
061412     ELSE
062712         MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME   
062712         MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME  
062712         MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT 
062712         MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE         
062712         MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME   
062712         MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME  
062712         MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT 
062712         MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE         
072312         IF ((PB-C-LF-CANCEL-DT = SPACES OR LOW-VALUES) AND
072312          (CM-LF-CANCEL-DT NOT = SPACES AND LOW-VALUES)) 
072312             MOVE SPACES             TO OC-LF-BENCD
072312             MOVE ZEROS              TO OC-LF-TERM         
072312                                        OC-LF-BEN-AMT      
072312                                        OC-LF-PRM-AMT      
072312                                        OC-LF-ALT-BEN-AMT  
072312                                        OC-LF-ALT-PRM-AMT  
072312                                        OC-LF-COMM-PCT     
072312                                        OC-LF-CANCEL-AMT
072312             MOVE LOW-VALUES         TO OC-LF-EXP-DT       
072312             MOVE CM-LF-CANCEL-DT    TO OC-LF-CANCEL-DT
072312         ELSE
062712             MOVE CM-LF-BENEFIT-CD   TO OC-LF-BENCD        
062712             MOVE CM-LF-ORIG-TERM    TO OC-LF-TERM         
062712             MOVE CM-LF-BENEFIT-AMT  TO OC-LF-BEN-AMT      
062712             MOVE CM-LF-PREMIUM-AMT  TO OC-LF-PRM-AMT      
062712             MOVE CM-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT  
062712             MOVE CM-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT  
062712             MOVE CM-LF-LOAN-EXPIRE-DT TO OC-LF-EXP-DT       
062712             MOVE CM-LIFE-COMM-PCT   TO OC-LF-COMM-PCT     
071712             MOVE PB-C-LF-CANCEL-DT  TO OC-LF-CANCEL-DT
071712             MOVE PB-C-LF-CANCEL-AMT TO OC-LF-CANCEL-AMT
072312         END-IF
071712         MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT
072312         IF ((PB-C-AH-CANCEL-DT = SPACES OR LOW-VALUES) AND
072312          (CM-AH-CANCEL-DT NOT = SPACES AND LOW-VALUES)) 
072312             MOVE SPACES             TO OC-AH-BENCD
072312             MOVE ZEROS              TO OC-AH-TERM         
072312                                        OC-AH-BEN-AMT      
072312                                        OC-AH-PRM-AMT      
072312                                        OC-AH-COMM-PCT     
072312                                        OC-AH-CANCEL-AMT
072312                                        OC-AH-CP
072312             MOVE LOW-VALUES         TO OC-AH-EXP-DT       
072312             MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT
072312         ELSE
062712             MOVE CM-AH-BENEFIT-CD   TO OC-AH-BENCD        
062712             MOVE CM-AH-ORIG-TERM    TO OC-AH-TERM         
062712             MOVE CM-AH-BENEFIT-AMT  TO OC-AH-BEN-AMT      
062712             MOVE CM-AH-PREMIUM-AMT  TO OC-AH-PRM-AMT      
062712             MOVE CM-AH-LOAN-EXPIRE-DT TO OC-AH-EXP-DT       
062712             MOVE CM-AH-COMM-PCT     TO OC-AH-COMM-PCT     
062712             MOVE CM-AH-CRITICAL-PERIOD TO OC-AH-CP
071712             MOVE PB-C-AH-CANCEL-DT  TO OC-AH-CANCEL-DT
071712             MOVE PB-C-AH-CANCEL-AMT TO OC-AH-CANCEL-AMT
072312         END-IF
071712         MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
062712         MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413         MOVE 'N'                    TO OC-ISSUE-TRAN-IND
011413         MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
061412     END-IF
061412     MOVE LOW-VALUES          TO OC-ENDORSEMENT-PROCESSED-DT
061412
091312     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO MA-CONTROL-PRIMARY
091312
091312     EXEC CICS READ
091312          DATASET   (FILE-ERMAIL)
091312          RIDFLD    (MA-CONTROL-PRIMARY)
091312          INTO      (MAILING-DATA)
091312          RESP      (WS-RESPONSE)
091312     END-EXEC
091312
091312     IF WS-RESP-NORMAL
061412         MOVE MA-CRED-BENE-NAME TO OC-CRED-BENE-NAME
061412     END-IF.
121712
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO WS-ELCRTT-PRIMARY
121712     MOVE 'C'                   TO WS-ELCRTT-REC-TYPE
121712
121712     EXEC CICS READ
121712          DATASET  (CRTT-ID)
121712          RIDFLD   (WS-ELCRTT-KEY)
121712          INTO     (CERTIFICATE-TRAILERS)
121712          RESP     (WS-RESPONSE)
121712     END-EXEC
121712
121712     IF WS-RESP-NORMAL
121712         MOVE CS-INS-AGE-DEFAULT-FLAG TO OC-INS-AGE-DEFAULT-FLAG
121712         MOVE CS-JNT-AGE-DEFAULT-FLAG TO OC-JNT-AGE-DEFAULT-FLAG
121712     END-IF.
061412
061412     EXEC CICS WRITE
061412        DATASET   ('ELCRTO')
061412        FROM      (ORIGINAL-CERTIFICATE)
061412        RIDFLD    (OC-CONTROL-PRIMARY)
061412        RESP      (WS-RESPONSE)
061412     END-EXEC.
061412
011413     GO TO 9991-EXIT.
011413
011413 9991-CHECK-INDICATORS.
011413
011413     IF PB-ISSUE AND OC-ISSUE-TRAN-IND NOT = 'N'
011413         GO TO 9991-EXIT
011413     END-IF.
011413
011413     IF PB-CANCELLATION AND OC-CANCEL-TRAN-IND NOT = 'N'
011413         GO TO 9991-EXIT
011413     END-IF.
011413
011413     EXEC CICS READ
011413        DATASET   ('ELCRTO')
011413        INTO      (ORIGINAL-CERTIFICATE)
011413        RIDFLD    (OC-CONTROL-PRIMARY)
011413        UPDATE
011413        RESP      (WS-RESPONSE)
011413     END-EXEC
011413     IF WS-RESP-NORMAL
011413        IF PB-ISSUE
011413          MOVE PB-I-INSURED-LAST-NAME TO OC-INS-LAST-NAME   
011413          MOVE PB-I-INSURED-FIRST-NAME TO OC-INS-FIRST-NAME  
011413          MOVE PB-I-INSURED-MIDDLE-INIT TO OC-INS-MIDDLE-INIT 
011413          MOVE PB-I-AGE               TO OC-INS-AGE         
011413          MOVE PB-I-JOINT-LAST-NAME   TO OC-JNT-LAST-NAME   
011413          MOVE PB-I-JOINT-FIRST-NAME  TO OC-JNT-FIRST-NAME  
011413          MOVE PB-I-JOINT-MIDDLE-INIT TO OC-JNT-MIDDLE-INIT 
011413          MOVE PB-I-JOINT-AGE         TO OC-JNT-AGE         
011413          MOVE PB-I-LF-BENEFIT-CD     TO OC-LF-BENCD
011413          MOVE PB-I-LF-TERM           TO OC-LF-TERM
011413          MOVE PB-I-LF-BENEFIT-AMT    TO OC-LF-BEN-AMT
011413          MOVE PB-I-LF-PREMIUM-AMT    TO OC-LF-PRM-AMT
011413          MOVE PB-I-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT
011413          MOVE PB-I-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT
011413          MOVE PB-I-LF-EXPIRE-DT      TO OC-LF-EXP-DT       
011413          IF PB-I-JOINT-COMMISSION > +0
011413              MOVE PB-I-JOINT-COMMISSION TO OC-LF-COMM-PCT
011413          ELSE
011413              MOVE PB-I-LIFE-COMMISSION  TO OC-LF-COMM-PCT
011413          END-IF
011413          MOVE PB-I-AH-BENEFIT-CD     TO OC-AH-BENCD        
011413          MOVE PB-I-AH-TERM           TO OC-AH-TERM         
011413          MOVE PB-I-AH-BENEFIT-AMT    TO OC-AH-BEN-AMT      
011413          MOVE PB-I-AH-PREMIUM-AMT    TO OC-AH-PRM-AMT      
011413          MOVE PB-I-AH-EXPIRE-DT      TO OC-AH-EXP-DT       
011413          MOVE PB-I-AH-COMMISSION     TO OC-AH-COMM-PCT     
011413          MOVE PB-I-AH-CRIT-PER       TO OC-AH-CP           
011413          MOVE PB-I-1ST-PMT-DT        TO OC-1ST-PMT-DT
011413          MOVE 'Y'                    TO OC-ISSUE-TRAN-IND
011413
011413          MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO MA-CONTROL-PRIMARY
011413          EXEC CICS READ
011413              DATASET   (FILE-ERMAIL)
011413              RIDFLD    (MA-CONTROL-PRIMARY)
011413              INTO      (MAILING-DATA)
011413              RESP      (WS-RESPONSE)
011413          END-EXEC
011413          IF WS-RESP-NORMAL
011413             MOVE MA-CRED-BENE-NAME TO OC-CRED-BENE-NAME
011413          END-IF
011413    
011413          MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO WS-ELCRTT-PRIMARY
011413          MOVE 'C'                   TO WS-ELCRTT-REC-TYPE
011413          EXEC CICS READ
011413              DATASET  (CRTT-ID)
011413              RIDFLD   (WS-ELCRTT-KEY)
011413              INTO     (CERTIFICATE-TRAILERS)
011413              RESP     (WS-RESPONSE)
011413          END-EXEC
011413          IF WS-RESP-NORMAL
011413             MOVE CS-INS-AGE-DEFAULT-FLAG TO 
011413                               OC-INS-AGE-DEFAULT-FLAG
011413             MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
011413                               OC-JNT-AGE-DEFAULT-FLAG
011413          END-IF
011413
011413        ELSE
011413          IF PB-C-LF-CANCEL-DT NOT = SPACES AND LOW-VALUES
011413             MOVE PB-C-LF-CANCEL-DT  TO OC-LF-CANCEL-DT
011413             MOVE PB-C-LF-CANCEL-AMT TO OC-LF-CANCEL-AMT
011413             MOVE CM-LF-ITD-CANCEL-AMT TO OC-LF-ITD-CANCEL-AMT
011413          END-IF
011413          IF PB-C-AH-CANCEL-DT NOT = SPACES AND LOW-VALUES
011413             MOVE PB-C-AH-CANCEL-DT  TO OC-AH-CANCEL-DT
011413             MOVE PB-C-AH-CANCEL-AMT TO OC-AH-CANCEL-AMT
011413             MOVE CM-AH-ITD-CANCEL-AMT TO OC-AH-ITD-CANCEL-AMT
011413          END-IF
011413          MOVE 'Y'                   TO OC-CANCEL-TRAN-IND
011413        END-IF
011413        EXEC CICS REWRITE
011413           DATASET   ('ELCRTO')
011413           FROM      (ORIGINAL-CERTIFICATE)
011413           RESP      (WS-RESPONSE)
011413        END-EXEC
011413     END-IF.
011413     
061412 9991-EXIT.
061412     EXIT.
061412
061412
01577  9999-RETURN.                                                     
01578      MOVE PENDING-BUSINESS       TO  DFHCOMMAREA.                 
01579                                                                   
01580      EXEC CICS RETURN                                             
01581      END-EXEC.                                                    
01582                                                                   
01583      GOBACK.                                                      
