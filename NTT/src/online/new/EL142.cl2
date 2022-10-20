      *$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL142 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 06/29/95 10:53:47.                 
00007 *                            VMOD=2.033                           
00008 *                                                                 
00009 *AUTHOR.    LOGIC, INC.                                           
00010 *           DALLAS, TEXAS.                                        
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
00024 *REMARKS.                                                         
00025 *        THIS PROGRAM PROVIDES THE FUNCTIONS TO BROWSE THRU AND   
00026 *    PERFORM MAINTENANCE ON ACTIVITY TRAILERS.  THE TRAILERS ARE  
00027 *    CREATED BY OTHER VARIOUS FUNCTIONS.                          
00028                                                                   
00029 *    TRANS ID = EX25                                              
00030                                                                   
00031 *    SCREENS     - EL142A - BROWSE QUALIFICATION                  
00032 *                  EL142B - PAYMENTS                              
00033 *                  EL142C - AUTOMATIC PAYMENTS                    
00034 *                  EL142D - LETTERS                               
00035 *                  EL142E - GENERAL INFORMATION                   
00036 *                  EL142F - REMINDERS                             
00037 *                  EL142G - DENIAL RECORD                         
00038 *                  EL142H - RESERVES & EXPENSES                   
00039 *                  EL142I - INCURED CHANGE INFORMATION            
00040 *                  EL142J - FORM INFORMATION                      
00041                                                                   
00042 *    ENTERED BY  - EL131  - CLAIM MAINTENANCE                     
00043 *                  EL150  - STATUS                                
00044 *                  EL1622 - CLAIM AUDIT                           
00045                                                                   
00046 *    EXIT TO     - CALLING PROGRAM                                
00047                                                                   
00048 *    INPUT FILES - ELTRLR - ACTIVITY TRAILERS                     
00049 *                  ELMSTR - CLAIM MASTER                          
00050                                                                   
00051 *    OUTPUT FILES - ELTRLR - ACTIVITY TRAILERS                    
00052 *                   ELMSTR - CLAIM MASTER                         
00053                                                                   
00054 *    COMMAREA    - PASSED.  WHEN CALLED BY EL150, A 4 BYTE        
00055 *                  CHARACTER, TRAILER SEQUENCE NUMBER, IS PASSED  
00056 *                  IN THE FIRST 4 BYTES OF THE PROGRAM-WORK-AREA  
00057 *                  OF THE INTERFACE BLOCK.                        
00058                                                                   
00059 *    NARRATIVE   - ON FIRST ENTRY, THE BROWSE QUALIFICATION SCREEN
00060 *                  IS SENT.  IF THE ENTRY CAME FROM EL150, THE    
00061 *                  PASSED SEQUENCE NUMBER IS PLACED IN THE "START-
00062 *                  ING SEQUENCE NUMBER FIELD" SO THAT THE OPERATOR
00063 *                  HAS THE OPTION OF STARTING AT THE SAME POINT AS
00064 *                  THE STATUS DISPLAY.                            
00065                                                                   
00066 *                  VIA THE QUALIFICATION SCREEN THE OPERATOR      
00067 *                  INDICATES WHAT TYPE OF TRAILERS ARE TO BE      
00068 *                  VIEWED, THE EARLIEST ACTIVITY AND A STARTING   
00069 *                  SEQUENCE NUMBER.  THE DATE IS OPTIONAL, SPACE  
00070 *                  IMPLIES ALL DATES.  IF THE SEQUENCE NUMBER IS  
00071 *                  NOT GIVEN, ZERO IS ASSUMED.  THE TRAILER FILE  
00072 *                  IS ALWAYS READ FORWARD, SO THAT ACTIVITY WILL  
00073 *                  BE SHOWN AS MOST RECENT FIRST.  THE QUALIFICA- 
00074 *                  TIONS ARE SAVED IN THE PROGRAM WORK AREA OF THE
00075 *                  INTERFACE BLOCK AND THE FIRST QUALIFYING       
00076 *                  TRAILER IS READ.                               
00077                                                                   
00078 *                  THE DISPLAY AND MAINTENANCE TYPE DEPENDS ON THE
00079 *                  TYPE OF TRAILER RECORD READ.  SCREENS EL142B   
00080 *                  THRU EL142H ARE USED DEPENDING ON THE RECORD   
00081 *                  TYPE.                                          
062602******************************************************************
062602*                   C H A N G E   L O G
062602*
062602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062602*-----------------------------------------------------------------
062602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062602* EFFECTIVE    NUMBER
062602*-----------------------------------------------------------------
062602* 062602    2002030700006  PEMA  Add note type of 'S'
062602*                                  (special review)
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST CALC
052506* 052506    2006030600001  AJRA  ADD PROOF DT TO PAYMENT & DENIAL 
082807* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
101807* 101807    2007100100007  PEMA  INCREASE CLM RESERVE FIELDS
031808* 031808    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
070909* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
033010* 033010  CR2008100900001  PEMA  ADD DENIAL CODE EDITS
042110* 042110  CR2008100900001  PEMA  ADD DELETE PROCESSING FOR MAPG
050110* 050110    2009122800001  AJRA  ADD NAPERSOFT
100610* 100610    2009122800001  AJRA  LOCK RECV DT W/ RESEND PRINTED DT
102510* 102510    2009122800001  AJRA  UPDATE ELNAPS WHEN RESEND DT CHGD
102610* 102510    2009122800001  AJRA  ADD STOP DATE TO LETTER TRAILER
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
020413* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM IND
041613* 041613    2013040400004  AJRA  ADD ENC CODE FOR SPECIAL HANDLING 
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
102413* 102413    2013100800001  AJRA  ADD SPEC RELEASE, FIX ENX ENC CODE
111113* 111113    2013110600002  AJRA  CHG LEVEL 5 RESTRICTIONS TO LEVEL 4 & 5
021114* 021114    2014012100002  AJRA  ADD CHECK CASHED DATE
081214* 081214  IR2014081100001  PEMA  CORRECT INT CHK CASHED DT PROCESS
091714* 091714  IR2014090800003  PEMA  ADD "manual" table to query
111714* 111714  CR2014073000001  PEMA  DRAFTS TO CHECKS
051215* 051215  IR2015051100002  PEMA  Correct bogus cashed date
060315* 060315  IR2015060200004  PEMA  Correct cut-off date
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
013017* 013017  CR2017022000001  PEMA  DRAFTS TO CHECKS - DCC
120718* 120718  CR2018120400002  PEMA  REMOVE USER HARDCODING
040819* 040819  IR2019030400004  TANA  PROTECT HOLD UNTIL DATE FIELD
043019* 043019  IR2019042300001  PEMA  DISALLOW PROOF DT > REC DT OR < INC DT
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
062602******************************************************************
00082
00083      EJECT                                                        
00084  ENVIRONMENT DIVISION.                                            
00085                                                                   
00086  DATA DIVISION.                                                   
00087                                                                   
00088  WORKING-STORAGE SECTION.                                         
00089  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
00090  01  LCP-CURRENT-DATE-68.                                         
00091      05  LCP-MONTH                 PIC X(2).                      
00092      05  FILLER                    PIC X VALUE '/'.               
00093      05  LCP-DAY1                  PIC X(2).                      
00094      05  FILLER                    PIC X VALUE '/'.               
00095      05  LCP-YEAR                  PIC X(2).                      
00096  01  LCP-CICS-DATE                 PIC 9(15).                     
00097                                                                   
00098  77  FILLER  PIC X(32)  VALUE '********************************'. 
00099  77  FILLER  PIC X(32)  VALUE '*   EL142  WORKING STORAGE     *'. 
00100  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.033 *********'. 
021114
NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
021114
021114 EXEC SQL
021114    INCLUDE SQLCA
021114 END-EXEC
021114
021114 EXEC SQL
021114    BEGIN DECLARE SECTION
021114 END-EXEC
021114
021114 01  SQLCMD                      PIC X(1024).
021114 01  SVR                         PIC X(32).
021114 01  USR                         PIC X(32).
021114 01  PASS                        PIC X(32).
021114 01  USR-PASS                    PIC X(64).
021114
111714 01  ws-sql-manual-data.
111714     05  ws-check-number-man     pic x(7).
111714     05  ws-check-company-man    pic xxx.
111714
021114 01  WS-SQL-DATA.
111714     05  ws-draft-or-check       pic x.
021114     05  WS-CHECK-TYPE           PIC X.
111714     05  WS-CHECK-NUMBER         PIC X(10).
111714     05  ws-claim-number         pic x(7).
021114     05  WS-CHECK-AMOUNT         PIC X(10).
111714     05  WS-CHECK-COMPANY        PIC X(5).
111714     05  WS-CHECK-CASHED-DT      PIC X(20).
021114
       01  ws-ach-data.
           05  ws-carrier              pic x.
           05  ws-state                pic xx.
           05  ws-account-no           pic x(10).
           05  ws-cert-no              pic x(11).
           05  ws-claim-no             pic x(7).
           05  ws-check-no             pic x(10).
           05  ws-cashed-date          pic x(10).

021114 EXEC SQL
021114    END DECLARE SECTION
021114 END-EXEC
021114
00101                                                                   
00102                                      COPY ELCSCTM.                
00103                                                                   
00104                                      COPY ELCSCRTY.               
00105                                                                   
00106                                      COPY ELCDCTB.                
00107                                                                   
00108                                      COPY ELCNWA.                 
00109                                                                   
020810 01  WS-ELDENY-KEY.
020810     05  ELDENY-COMPANY-CD   PIC X.
020810     05  ELDENY-DENIAL-CODE  PIC X(4).
020810     05  FILLER              PIC X(10).

020810 01  WS-RESPONSE             PIC S9(8)   COMP.
020810     88  RESP-NORMAL              VALUE +00.
020810     88  RESP-ERROR               VALUE +01.
020810     88  RESP-NOTFND              VALUE +13.
020810     88  RESP-NOTOPEN             VALUE +19.
020810     88  RESP-ENDFILE             VALUE +20.

00110  01  FILLER                          COMP-3.                      
00111      05  WS-RECORD-COUNT             PIC S9(5)       VALUE ZERO.  
00112      05  WS-RECORD-REMAINDER         PIC S9(5)       VALUE ZERO.  
00113      05  WS-RECORD-DIV               PIC S9(5)       VALUE ZERO.  
00114      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.  
00115      05  WS-NOT-FOUND                PIC S9          VALUE ZERO.  
00116      05  WS-ERROR-COUNT              PIC S9(3)       VALUE ZERO.  
00117      05  WS-UPDATE-SW                PIC S9          VALUE ZERO.  
00118      05  WS-COMPLETED-SUCCESSFUL     PIC S9          VALUE ZERO.  
00119        88  TRANSACTION-SUCCESSFUL                    VALUE +1.    
00120                                                                   
00121      05  TIME-IN                     PIC S9(7)       VALUE ZERO.  
00122      05  TIME-OUT                    REDEFINES                    
00123          TIME-IN                     PIC S9(3)V9(4).              
00124                                                                   
00125      05  WS-HPCTCDTI                 PIC S9(3)V99 VALUE ZERO.     
101807     05  WS-HMANAMTI                 PIC S9(7)V99 VALUE ZERO.     
00127      05  WS-HEXPAMTI                 PIC S9(5)V99 VALUE ZERO.     
00128                                                                   
00129  01  FILLER                          COMP  SYNC.                  
00130      05  SC-ITEM                     PIC S9(4)       VALUE +0001. 
00131      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  
00132                                                                   
00133      05  WS-ACTIVITY-TRAILERS-LENGTH PIC S9(4)       VALUE +200.  
00134      05  WS-CHECK-QUEUE-LENGTH       PIC S9(4)       VALUE +100.  
00135      05  WS-DEEDIT-LENGTH            PIC S9(4)       VALUE +8.    
00136      05  WS-HPCTCDT-LENGTH           PIC S9(4)       VALUE +7.    
101807     05  WS-HMANAMT-LENGTH           PIC S9(4)       VALUE +12.   
00138      05  WS-HEXPAMT-LENGTH           PIC S9(4)       VALUE +7.    
00139      05  WS-CHECK-QUE                PIC S9(8)       VALUE ZERO.  
00140      05  WS-CHECK-QUE-SEQ            PIC S9(4)       VALUE ZERO.  
00141      05  WS-DMO-LENGTH               PIC S9(4)       VALUE +108.  
00142      05  WS-DCT-LENGTH               PIC S9(4)       VALUE +53.   
00143                                                                   
00144  01  FILLER.                                                      
00145      05  WS-CL-CERT-KEY-DATA.                                     
00146          10  WS-CL-CERT-CARRIER  PIC X.                           
00147          10  WS-CL-CERT-GROUPING PIC X(6).                        
00148          10  WS-CL-CERT-STATE    PIC XX.                          
00149          10  WS-CL-CERT-ACCOUNT  PIC X(10).                       
00150          10  WS-CL-CERT-EFF-DT   PIC XX.                          
00151      05  WS-CL-CERT-NO           PIC X(11).                       
00152      05  WS-CL-BENEFICIARY       PIC X(10).                       
00153      05  WS-CL-CCN               PIC X(16).                       
00154      05  WS-CL-CLAIM-NO          PIC X(7).                        
00155      05  WS-CL-CLAIM-TYPE        PIC X.                           
00156      05  WS-CL-INSURED-LAST-NAME PIC X(15).                       
00157      05  WS-CL-INSURED-NAME.                                      
00158          10  WS-CL-INSURED-1ST-NAME PIC X(12).                    
00159          10  WS-CL-INSURED-MID-INIT PIC X.                        
00160      05  W-NAME-LAST             PIC  X(15).                      
00161      05  W-NAME-FIRST            PIC  X(15).                      
00162      05  W-NAME-MIDDLE.                                           
00163          10  FILLER              PIC  X.                          
00164          10  W-NAME-MIDDLE-2     PIC  X.                          
00165          10  FILLER              PIC  X(13).                      
00166      05  WS-CL-NO-OF-PMTS-MADE   PIC S9(3) COMP-3.                
00167                                                                   
00168      05  NOTE-KEY.                                                
00169          10  NOTE-COMP-CD            PIC X.                       
00170          10  NOTE-CERT-KEY.                                       
00171              15  NOTE-CARRIER        PIC X.                       
00172              15  NOTE-GROUP          PIC X(6).                    
00173              15  NOTE-STATE          PIC XX.                      
00174              15  NOTE-ACCOUNT        PIC X(10).                   
00175              15  NOTE-DATE           PIC XX.                      
00176              15  NOTE-CERT-NO        PIC X(11).                   
00177                                                                   
00178      05  WS-ACTIVITY-TRAILERS-KEY.                                
00179          10  WS-ATK-COMPANY-CODE     PIC X.                       
00180          10  WS-ATK-CARRIER          PIC X.                       
00181          10  WS-ATK-CLAIM-NO         PIC X(7).                    
00182          10  WS-ATK-CERT-NO.                                      
00183              15  WS-ATK-CERT-NO-PRIME  PIC X(10).                 
00184              15  WS-ATK-CERT-NO-SUFX   PIC X.                     
00185          10  WS-ATK-SEQUENCE-NO      PIC S9(4) COMP.              
00186                                                                   
00187      05  WS-CLAIM-KEY.                                            
00188          10  WS-CK-COMPANY-CODE     PIC X.                        
00189          10  WS-CK-CARRIER          PIC X.                        
00190          10  WS-CK-CLAIM-NO         PIC X(7).                     
00191          10  WS-CK-CERT-NO.                                       
00192              15  WS-CK-CERT-NO-PRIME   PIC X(10).                 
00193              15  WS-CK-CERT-NO-SUFX    PIC X.                     
00194                                                                   
00195      05  WS-ACCOUNT-MASTER-KEY.                                   
00196          10  WS-AM-COMPANY-CD        PIC X.                       
00197          10  WS-AM-CARRIER           PIC X.                       
00198          10  WS-AM-GROUPING          PIC X(6).                    
00199          10  WS-AM-STATE             PIC XX.                      
00200          10  WS-AM-ACCOUNT           PIC X(10).                   
00201          10  WS-AM-EXPIRATION-DT     PIC XX.                      
00202          10  FILLER                  PIC X(4).                    
00203                                                                   
00204      05  WS-PRODUCER-MASTER-KEY.                                  
00205          10  WS-PD-COMPANY-CD        PIC X.                       
00206          10  WS-PD-CARRIER           PIC X.                       
00207          10  WS-PD-GROUPING          PIC X(6).                    
00208          10  WS-PD-STATE             PIC XX.                      
00209          10  WS-PD-PRODUCER          PIC X(10).                   
00210          10  WS-PD-EXPIRATION-DT     PIC XX.                      
00211                                                                   
00212      05  WS-LETTER-ARCHIVE-KEY.                                   
00213          10  WS-LA-COMPANY-CD        PIC X.                       
00214          10  WS-LA-ARCHIVE-NO        PIC S9(8)       COMP.        
00215          10  WS-LA-RECORD-TYPE       PIC X.                       
00216 *          88  HEADER-DATA                           VALUE '1'.   
00217 *          88  ADDRESS-DATA                          VALUE '2'.   
00218 *          88  TEXT-DATA                             VALUE '3'.   
00219          10  WS-LA-LINE-SEQ-NO       PIC S9(4)       COMP.        
102510
102510     05  WS-NAPERSOFT-KEY.
102510         10  WS-NA-COMPANY-CD        PIC X.
102510         10  WS-NA-CARRIER           PIC X.
102510         10  WS-NA-CLAIM-NO          PIC X(7).
102510         10  WS-NA-CERT-NO           PIC X(11).
102510         10  WS-NA-ARCHIVE-NO        PIC 9(8).
041613
041613     05  WS-ELENCC-KEY.
041613         10  WS-ELENCC-COMPANY-CD    PIC X.
041613         10  WS-ELENCC-REC-TYPE      PIC X.
041613         10  WS-ELENCC-ENC-CODE      PIC X(5).
041613         10  F                       PIC X(09).
00220                                                                   
00221      05  WS-LETTER-ARCHIVE-ALT-KEY.                               
00222          10  WS-LA-ALT-COMPANY-CD    PIC X.                       
00223          10  WS-LA-ALT-RECORD-TYPE   PIC X.                       
00224          10  WS-LA-ALT-ARCHIVE-NO    PIC S9(8)       COMP.        
00225          10  WS-LA-ATL-LINE-SEQ-NO   PIC S9(4)       COMP.        
00226                                                                   
00227      05  WS-CHECK-QUEUE-KEY.                                      
00228          10  WS-CQ-COMPANY-CD        PIC X.                       
00229          10  WS-CQ-CONTROL-NUMBER    PIC S9(9)       COMP.        
00230          10  WS-CQ-SEQUENCE-NUMBER   PIC S9(4)       COMP.        
00231                                                                   
00232      05  WS-CONTROL-FILE-KEY.                                     
00233          16  CNTL-CO             PIC X(3).                        
00234          16  CNTL-RECORD-TYPE    PIC X       VALUE '1'.           
00235          16  CNTL-GENL           PIC X(4)    VALUE SPACES.        
00236          16  CNTL-SEQ            PIC S9(4)   VALUE +0    COMP.    
00237                                                                   
00238      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL142S'.
00239                                                                   
00240      05  EL142A                      PIC X(8) VALUE 'EL142A'.     
00241      05  EL142B                      PIC X(8) VALUE 'EL142B'.     
00242      05  EL142B2                     PIC X(8) VALUE 'EL142B2'.    
00243      05  EL142C                      PIC X(8) VALUE 'EL142C'.     
00244      05  EL142D                      PIC X(8) VALUE 'EL142D'.     
00245      05  EL142D2                     PIC X(8) VALUE 'EL142D2'.    
00246      05  EL142E                      PIC X(8) VALUE 'EL142E'.     
00247      05  EL142F                      PIC X(8) VALUE 'EL142F'.     
00248      05  EL142G                      PIC X(8) VALUE 'EL142G'.     
00249      05  EL142H                      PIC X(8) VALUE 'EL142H'.     
00250      05  EL142I                      PIC X(8) VALUE 'EL142I'.     
00251      05  EL142J                      PIC X(8) VALUE 'EL142J'.     
00252                                                                   
00253      05  THIS-PGM                    PIC X(8)      VALUE 'EL142'. 
00254      05  XCTL-PGM                    PIC X(8).                    
00255                                                                   
00256      05  PGM-EL1501                  PIC X(8)      VALUE 'EL1501'.
00257                                                                   
00258      05  EL001                       PIC X(8)      VALUE 'EL001'. 
00259      05  EL004                       PIC X(8)      VALUE 'EL004'. 
00260      05  EL005                       PIC X(8)      VALUE 'EL005'. 
00261      05  EL010                       PIC X(8)      VALUE 'EL010'. 
00262      05  EL126                       PIC X(8)      VALUE 'EL126'. 
00263      05  ELDATCV                     PIC X(8) VALUE 'ELDATCV'.    
00264                                                                   
00265      05  WS-CLAIM-MASTER-DSID        PIC X(8) VALUE 'ELMSTR'.     
00266      05  WS-ACCOUNT-MASTER-DSID      PIC X(8) VALUE 'ERACCT'.     
00267      05  WS-PRODUCER-MASTER-DSID     PIC X(8) VALUE 'MPPROD'.     
00268      05  WS-ACTIVITY-TRAILERS-DSID   PIC X(8) VALUE 'ELTRLR'.     
00269      05  WS-LETTER-ARCHIVE-DSID      PIC X(8) VALUE 'ELARCH'.     
00270      05  WS-ELARCT-FILE-ID           PIC X(8) VALUE 'ELARCT'.     
00271      05  WS-LETTER-ARCHIVE-DSID2     PIC X(8) VALUE 'ELARCH2'.    
00272      05  WS-CHECK-QUEUE-DSID         PIC X(8) VALUE 'ELCHKQ'.     
00273      05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.     
00274      05  WS-NOTE-FILE-DSID           PIC X(8) VALUE 'ERNOTE'.     
102510     05  WS-NAPERSOFT-DSID           PIC X(8) VALUE 'ELNAPS'.
041613     05  WS-ELENCC-FILE-DSID         PIC X(8) VALUE 'ELENCC'.
00275                                                                   
00276      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX25'.
00277                                                                   
00278      05  WS-CLAIM-TYPE               PIC X           VALUE SPACES.
00279                                                                   
00280      05  WS-PI-EL142-PRIORITY        PIC X           VALUE SPACES.
00281                                                                   
00282      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70    
00283                                      COMP SYNC.                   
00284                                                                   
00285      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.
00286                                                                   
00287      05  WS-DEEDIT-FIELD             PIC X(8)        VALUE ZERO.  
00288                                                                   
00289      05  WS-DEEDIT-FIELD-V0          REDEFINES                    
00290          WS-DEEDIT-FIELD             PIC S9(8).                   
00291                                                                   
00292      05  WS-RESEND-DATE              PIC XX VALUE LOW-VALUES.     
00293      05  WS-SEND-ON-DATE             PIC XX VALUE LOW-VALUES.     
00294      05  WS-FOLLOW-UP-DATE           PIC XX VALUE LOW-VALUES.     
00295      05  WS-RECEIVED-DATE            PIC XX VALUE LOW-VALUES.     
00296      05  WS-RECEIVED-PHY-DATE        PIC XX VALUE LOW-VALUES.     
00297      05  WS-RECEIVED-EMP-DATE        PIC XX VALUE LOW-VALUES.     
00298      05  WS-START-DATE               PIC XX VALUE LOW-VALUES.     
00299      05  WS-END-DATE                 PIC XX VALUE LOW-VALUES.     
00300      05  WS-CURRENT-DATE             PIC XX VALUE LOW-VALUES.     
00301      05  SAVE-DATE-CCYYMMDD.                                      
00302          10  SAVE-DATE-CC            PIC XX VALUE SPACES.         
00303          10  SAVE-DATE-YMD.                                       
00304              15  SAVE-DATE-YY        PIC XX VALUE SPACES.         
00305              15  FILLER              PIC X(4) VALUE SPACES.       
00306      05  WS-DATE-SENT                PIC XX VALUE LOW-VALUES.     
00307      05  WS-IN-PRINT-DATE            PIC XX VALUE LOW-VALUES.     
00308      05  WS-REPRINTED-DATE           PIC XX VALUE LOW-VALUES.     
00309      05  WS-ARCHIVE-NUMBER           PIC 9(8)  VALUE ZEROS.       
050110     05  WS-RESEND-FORM-NUMBER       PIC X(4) VALUE LOW-VALUES.
102610     05  WS-STOP-LETTER-DATE         PIC XX VALUE LOW-VALUES.
041613     05  WS-ENCLOSURE-CODE           PIC X(3) VALUE SPACES.
041613     05  WS-TEMP-ENCCODE             PIC X(3) VALUE SPACES.
021114     05  WS-CHECK-AMT-TMP            PIC Z(7).99.
021114     05  WS-CHECK-AMT-TMPX REDEFINES 
021114         WS-CHECK-AMT-TMP            PIC X(10).
00310                                                                   
00311                                                                   
00312      05  WS-CRSEL                    PIC XX VALUE LOW-VALUES.     
00313      05  WS-VOIDSD                   PIC XX VALUE LOW-VALUES.     
00314      05  WS-FORM-NUMBER              PIC X(4)    VALUE SPACES.    
00315      05  WS-TEMP-DT                  PIC 99B99B99.                
00316                                                                   
00317      05  SLASH                       PIC X           VALUE '/'.   
00318                                                                   
00319      05  WS-SPACES                   PIC X           VALUE SPACES.
00320      05  WS-ZIP.                                                  
00321          10  WS-ZIP-CODE         PIC X(5).                        
00322          10  WS-DASH             PIC X     VALUE '-'.             
00323          10  WS-ZIP-PLUS4        PIC X(4).                        
00324      05  WS-CANADIAN-POSTAL-CODES REDEFINES WS-ZIP.               
00325          10  WS-CAN-POSTAL-CD-1  PIC X(3).                        
00326          10  WS-DASH-CAN         PIC X.                           
00327          10  WS-CAN-POSTAL-CD-2  PIC X(3).                        
00328          10  WS-CAN-FILLER       PIC X(3).                        
00329      05  WS-WORK-PHONE           PIC X(10)  VALUE ZEROS.          
00330      05  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE                 
00331                                  PIC 9(10).                       
00332  
052506     05  WS-PRF-DT               PIC X(2)    VALUE LOW-VALUES.   
                                                                 
00333      EJECT                                                        
00334      05  ER-ZERO                 PIC 9(4)        VALUE 0000.      
00335      05  ER-0000                 PIC 9(4)        VALUE 0000.      
00336      05  ER-0004                 PIC 9(4)        VALUE 0004.      
00337      05  ER-0006                 PIC 9(4)        VALUE 0006.      
00338      05  ER-0008                 PIC 9(4)        VALUE 0008.      
00339      05  ER-0021                 PIC 9(4)        VALUE 0021.      
00340      05  ER-0023                 PIC 9(4)        VALUE 0023.      
00341      05  ER-0029                 PIC 9(4)        VALUE 0029.      
00342      05  ER-0068                 PIC 9(4)        VALUE 0068.      
00343      05  ER-0070                 PIC 9(4)        VALUE 0070.      
00344      05  ER-0105                 PIC 9(4)        VALUE 0105.      
00345      05  ER-0106                 PIC 9(4)        VALUE 0106.      
00346      05  ER-0107                 PIC 9(4)        VALUE 0107.      
00347      05  ER-0109                 PIC 9(4)        VALUE 0109.      
00348      05  ER-0111                 PIC 9(4)        VALUE 0111.      
00349      05  ER-0175                 PIC 9(4)        VALUE 0175.      
00350      05  ER-0198                 PIC 9(4)        VALUE 0198.      
00351      05  ER-0285                 PIC 9(4)        VALUE 0285.      
00352      05  ER-0286                 PIC 9(4)        VALUE 0286.      
00353      05  ER-0287                 PIC 9(4)        VALUE 0287.      
00354      05  ER-0288                 PIC 9(4)        VALUE 0288.      
00355      05  ER-0289                 PIC 9(4)        VALUE 0289.      
00356      05  ER-0290                 PIC 9(4)        VALUE 0290.      
00357      05  ER-0291                 PIC 9(4)        VALUE 0291.      
00358      05  ER-0292                 PIC 9(4)        VALUE 0292.      
00359      05  ER-0293                 PIC 9(4)        VALUE 0293.      
00360      05  ER-0294                 PIC 9(4)        VALUE 0294.      
00361      05  ER-0295                 PIC 9(4)        VALUE 0295.      
00362      05  ER-0296                 PIC 9(4)        VALUE 0296.      
00363      05  ER-0297                 PIC 9(4)        VALUE 0297.      
00364      05  ER-0298                 PIC 9(4)        VALUE 0298.      
00365      05  ER-0299                 PIC 9(4)        VALUE 0299.      
00366      05  ER-0300                 PIC 9(4)        VALUE 0300.      
00367      05  ER-0303                 PIC 9(4)        VALUE 0303.      
042110     05  ER-0310                 PIC 9(4)        VALUE 0310.
00368      05  ER-0324                 PIC 9(4)        VALUE 0324.      
00369      05  ER-0325                 PIC 9(4)        VALUE 0325.      
00370      05  ER-0327                 PIC 9(4)        VALUE 0327.      
00371      05  ER-0328                 PIC 9(4)        VALUE 0328.      
00372      05  ER-0329                 PIC 9(4)        VALUE 0329.      
00373      05  ER-0341                 PIC 9(4)        VALUE 0341.      
00374      05  ER-0342                 PIC 9(4)        VALUE 0342.      
00375      05  ER-0344                 PIC 9(4)        VALUE 0344.      
00376      05  ER-0384                 PIC 9(4)        VALUE 0384.      
00377      05  ER-0388                 PIC 9(4)        VALUE 0388.      
00378      05  ER-0532                 PIC 9(4)        VALUE 0532.      
00379      05  ER-0538                 PIC 9(4)        VALUE 0538.      
00380      05  ER-0550                 PIC 9(4)        VALUE 0550.      
00381      05  ER-0551                 PIC 9(4)        VALUE 0551.      
00382      05  ER-0564                 PIC 9(4)        VALUE 0564.      
00383      05  ER-0570                 PIC 9(4)        VALUE 0570.      
00384      05  ER-0571                 PIC 9(4)        VALUE 0571.      
00385      05  ER-0574                 PIC 9(4)        VALUE 0574.      
00386      05  ER-0575                 PIC 9(4)        VALUE 0575.      
00387      05  ER-0576                 PIC 9(4)        VALUE 0576.      
00388      05  ER-0577                 PIC 9(4)        VALUE 0577.      
00389      05  ER-0578                 PIC 9(4)        VALUE 0578.      
00390      05  ER-0579                 PIC 9(4)        VALUE 0579.      
00391      05  ER-0580                 PIC 9(4)        VALUE 0580.      
00392      05  ER-0581                 PIC 9(4)        VALUE 0581.      
00393      05  ER-0641                 PIC 9(4)        VALUE 0641.      
00394      05  ER-0642                 PIC 9(4)        VALUE 0642.      
00395      05  ER-0643                 PIC 9(4)        VALUE 0643.      
042110     05  ER-0755                 PIC 9(4)        VALUE 0755.
00396      05  ER-0830                 PIC 9(4)        VALUE 0830.      
00397      05  ER-0849                 PIC 9(4)        VALUE 0849.      
052506     05  ER-0873                 PIC 9(4)        VALUE 0873.      
020810     05  ER-0884                 PIC 9(4)        VALUE 0884.
102610     05  ER-0897                 PIC 9(4)        VALUE 0897.
00398      05  ER-0919                 PIC 9(4)        VALUE 0919.      
00399      05  ER-0921                 PIC 9(4)        VALUE 0921.      
00400      05  ER-0937                 PIC 9(4)        VALUE 0937.      
00401      05  ER-0946                 PIC 9(4)        VALUE 0946.      
00402      05  ER-0947                 PIC 9(4)        VALUE 0947.      
00403      05  ER-0948                 PIC 9(4)        VALUE 0948.      
00404      05  ER-0949                 PIC 9(4)        VALUE 0949.      
00405      05  ER-0950                 PIC 9(4)        VALUE 0950.      
00406      05  ER-0951                 PIC 9(4)        VALUE 0951.      
00407      05  ER-0954                 PIC 9(4)        VALUE 0954.      
00408      05  ER-0969                 PIC 9(4)        VALUE 0969.      
00409      05  ER-0974                 PIC 9(4)        VALUE 0974.      
00410      05  ER-0975                 PIC 9(4)        VALUE 0975.      
041613     05  ER-1560                 PIC 9(4)        VALUE 1560.
           05  ER-1561                 PIC 9(4)        VALUE 1561.
020413     05  ER-1566                 PIC 9(4)        VALUE 1566.
020413     05  ER-1567                 PIC 9(4)        VALUE 1567.
041613     05  ER-1568                 PIC 9(4)        VALUE 1568.
102413     05  ER-1569                 PIC 9(4)        VALUE 1569.
00411      05  ER-2466                 PIC 9(4)        VALUE 2466.      
00412      05  ER-8003                 PIC 9(4)        VALUE 8003.      
00413      05  ER-8004                 PIC 9(4)        VALUE 8004.      
00414      05  ER-8051                 PIC 9(4)        VALUE 8051.      
00415      05  ER-8052                 PIC 9(4)        VALUE 8052.      
00416      05  ER-8053                 PIC 9(4)        VALUE 8053.      
00417      05  ER-8054                 PIC 9(4)        VALUE 8054.      
00418      05  ER-8055                 PIC 9(4)        VALUE 8055.      
00419      05  ER-8056                 PIC 9(4)        VALUE 8056.      
00420      05  ER-8057                 PIC 9(4)        VALUE 8057.      
00421      05  ER-8058                 PIC 9(4)        VALUE 8058.      
00422      05  ER-8059                 PIC 9(4)        VALUE 8059.      
00423      05  ER-8060                 PIC 9(4)        VALUE 8060.      
00424      05  ER-8061                 PIC 9(4)        VALUE 8061.      
00425      05  ER-8062                 PIC 9(4)        VALUE 8062.      
00426      05  ER-8063                 PIC 9(4)        VALUE 8063.      
00427      05  ER-8064                 PIC 9(4)        VALUE 8064.      
00428      05  ER-8065                 PIC 9(4)        VALUE 8065.      
00429      05  ER-8066                 PIC 9(4)        VALUE 8066.      
00430      05  ER-8152                 PIC 9(4)        VALUE 8152.      
00431      05  ER-8153                 PIC 9(4)        VALUE 8153.      
00432      05  ER-8154                 PIC 9(4)        VALUE 8154.      
00433      05  ER-8155                 PIC 9(4)        VALUE 8155.      
00434      05  ER-9616                 PIC 9(4)        VALUE 9616.      
00435                                                                   
00436  01  HAN-PAYMENT-NOTE-DATA.                                       
00437      12  WS-HAN-PAYMENT-NOTE.                                     
00438          16  WS-HAN-PMT-CODE     PIC X.                           
00439          16  WS-HAN-PMT-TEXT     PIC X(59).                       
00440                                                                   
00441  01  HAN-LETTER-REASON-DATA.                                      
00442      12  WS-REASON-TEXT.                                          
00443          16  WS-RE-NDX           PIC 99.                          
00444          16  FILLER              PIC X(68).                       
00445                                                                   
00446      12  HAN-REASON-TABLE.                                        
00447          16  FILLER              PIC X(50) VALUE                  
00448            'ADDITIONAL INFO REQUESTED FROM PHYSICIAN          '.  
00449          16  FILLER              PIC X(50) VALUE                  
00450            'CHECKING PRE-EXISTING CONDITION                   '.  
00451          16  FILLER              PIC X(50) VALUE                  
00452            'ADDITIONAL INFO RECEIVED / CLAIM REOPENED         '.  
00453          16  FILLER              PIC X(50) VALUE                  
00454            'LETTER TO INSURED                                 '.  
00455          16  FILLER              PIC X(50) VALUE                  
00456            'LETTER TO CREDITOR                                '.  
00457          16  FILLER              PIC X(50) VALUE                  
00458            'LETTER TO EMPLOYER                                '.  
00459          16  FILLER              PIC X(50) VALUE                  
00460            'LETTER TO INSURED / 2ND REQUEST                   '.  
00461          16  FILLER              PIC X(50) VALUE                  
00462            'LETTER TO CREDITOR / 2ND REQUEST                  '.  
00463          16  FILLER              PIC X(50) VALUE                  
00464            'LETTER TO EMPLOYER / 2ND REQUEST                  '.  
00465          16  FILLER              PIC X(50) VALUE                  
00466            'AWAITING INITIAL CLAIM FORM                       '.  
00467          16  FILLER              PIC X(50) VALUE                  
00468            'AWAITING SUPPLEMENTAL INFORMATION                 '.  
00469          16  FILLER              PIC X(50) VALUE                  
00470            'DENIED / PRE-EXISTING CONDITION                   '.  
00471          16  FILLER              PIC X(50) VALUE                  
00472            'DENIED / WAITING PERIOD NOT MET                   '.  
00473          16  FILLER              PIC X(50) VALUE                  
00474            'DENIED / NORMAL PREGNANCY                         '.  
00475          16  FILLER              PIC X(50) VALUE                  
00476            'DENIED / ACT OF WAR                               '.  
00477          16  FILLER              PIC X(50) VALUE                  
00478            'DENIED / NOT TOTALLY DISABLED                     '.  
00479          16  FILLER              PIC X(50) VALUE                  
00480            'DENIED / NOT UNDER CARE & TREATMENT OF PHYSICIAN  '.  
00481          16  FILLER              PIC X(50) VALUE                  
00482            'DENIED / NO COVERAGE INFORCE                      '.  
00483          16  FILLER              PIC X(50) VALUE                  
00484            'DENIED / DISABLED ON DATE OF LOAN                 '.  
00485          16  FILLER              PIC X(50) VALUE                  
00486            'DENIED / OVER MAXIMUM AGE                         '.  
00487          16  FILLER              PIC X(50) VALUE                  
00488            'CLOSED / CLAIM INFO NOT PROVIDED                  '.  
00489          16  FILLER              PIC X(50) VALUE                  
00490            'PHYSICIAN INFORMATION INCOMPLETE                  '.  
00491          16  FILLER              PIC X(50) VALUE                  
00492            'ACKNOWLEDGEMENT LETTER TO INSURED                 '.  
00493                                                                   
00494      12  HAN-LETTER-REASON-TABLE  REDEFINES  HAN-REASON-TABLE.    
00495          16  HAN-TABLE-ENTRIES  OCCURS  23  TIMES.                
00496              20  HAN-REASON-TEXT PIC X(50).                       
00497                                                                   
00498      EJECT                                                        
00499                                  COPY ELCINTF.                    
00500                                                                   
00501      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   
00502          16  PI-TRAILER-NUMBER       PIC 9(4) COMP.               
00503          16  PI-EL142-PRIORITY       PIC X.                       
00504          16  FILLER                  PIC X.                       
00505                                                                   
00506          16  PI-MAP-NAME             PIC X(8).                    
00507                                                                   
00508          16  FILLER                  REDEFINES                    
00509              PI-MAP-NAME.                                         
00510              20  FILLER              PIC XX.                      
00511              20  PI-MAP-NUMBER       PIC X(6).                    
00512                                                                   
00513          16  PI-QUALIFICATION-SWITCHES    COMP-3.                 
00514              20  PI-REMINDERS-SW     PIC S9.                      
00515              20  PI-LETTERS-SW       PIC S9.                      
00516              20  PI-PAYMENTS-SW      PIC S9.                      
00517              20  PI-AUTO-PAY-SW      PIC S9.                      
00518              20  PI-NOTES-SW         PIC S9.                      
00519              20  PI-RES-EXP-SW       PIC S9.                      
00520              20  PI-DENIALS-SW       PIC S9.                      
00521              20  PI-INCURRED-DATE-SW PIC S9.                      
00522              20  PI-FORMS-SW         PIC S9.                      
00523                                                                   
00524          16  PI-AFTER-DATE           PIC XX.                      
00525          16  PI-AFTER-DATE-2         PIC XX.                      
00526          16  PI-AFTER-DATE-3         PIC XX.                      
00527          16  PI-HOLD-UNTIL-DATE      PIC XX.                      
00528                                                                   
00529          16  PI-ACTIVITY-TRAILERS-KEY.                            
00530              20  PI-ATK-COMPANY-CODE PIC X.                       
00531              20  PI-ATK-CARRIER      PIC X.                       
00532              20  PI-ATK-CLAIM-NO     PIC X(7).                    
00533              20  PI-ATK-CERT-NO.                                  
00534                  24  PI-ATK-CERT-NO-PRIME  PIC X(10).             
00535                  24  PI-ATK-CERT-NO-SUFX   PIC X.                 
00536              20  PI-ATK-SEQUENCE-NO  PIC S9(4) COMP.              
00537                                                                   
00538          16  PI-PREV-ACTIVITY-TRAILERS-KEY.                       
00539              20  PI-PREV-ATK-COMPANY-CODE PIC X.                  
00540              20  PI-PREV-ATK-CARRIER      PIC X.                  
00541              20  PI-PREV-ATK-CLAIM-NO     PIC X(7).               
00542              20  PI-PREV-ATK-CERT-NO.                             
00543                  24  PI-PREV-ATK-CERT-NO-PRIME PIC X(10).         
00544                  24  PI-PREV-ATK-CERT-NO-SUFX  PIC X.             
00545              20  PI-PREV-ATK-SEQUENCE-NO  PIC S9(4) COMP.         
00546                                                                   
00547          16  PI-SAVE-KEY.                                         
00548              20  PI-SAVE-ATK-COMPANY-CODE PIC X.                  
00549              20  PI-SAVE-ATK-CARRIER      PIC X.                  
00550              20  PI-SAVE-ATK-CLAIM-NO     PIC X(7).               
00551              20  PI-SAVE-ATK-CERT-NO.                             
00552                  24  PI-SAVE-ATK-CERT-NO-PRIME  PIC X(10).        
00553                  24  PI-SAVE-ATK-CERT-NO-PRIME  PIC X.            
00554              20  PI-SAVE-ATK-SEQUENCE-NO        PIC S9(4) COMP.   
00555                                                                   
00556          16  PI-PREV-AID             PIC X.                       
00557                                                                   
00558          16  PI-RECORD-COUNT         PIC S9  COMP-3.              
00559          16  PI-END-OF-FILE          PIC S9  COMP-3.
042110         16  PI-DENIAL-REASON-CODE   PIC X(4).
042110         16  PI-MAPG-DELETE-CNT      PIC 9.
00560          16  FILLER                  PIC X.
00561                                                                   
00562          16  PI-FIRST-TIME-SW        PIC X.                       
00563              88  FIRST-TIME                       VALUE 'Y'.      
00564                                                                   
00565          16  PI-SAVE-LAST-MAINT-DT   PIC XX.                      
00566          16  PI-SAVE-LAST-UPD-BY     PIC X(4).                    
020413         16  PI-APPROVAL-LEVEL       PIC X.
041613         16  PI-ENC-CODE             PIC X(3).
041613         16  PI-CREATED-IN-NAPERSOFT PIC X.
043019         16  pi-den-recorded-dt      pic xx.
043019         16  pi-incurred-dt          pic xx.
00567                                                                   
043019         16  FILLER                  PIC X(520).                  
00569                                                                   
00570      EJECT                                                        
00571                                      COPY EL142S.                 
00572  01  FILLER REDEFINES EL142HI.                                    
00573                                                                   
101807     05  FILLER                      PIC X(259).
00575                                                                   
00576      05  FILLER  OCCURS 5  INDEXED BY EL142H-INDEX1.              
00577          10  EL142H-MAP-LINE  OCCURS 2 INDEXED BY EL142H-INDEX2.  
00578                                                                   
00579              15  EL142H-DATE-LENGTH  PIC S9(4)  COMP.             
00580              15  EL142H-DATE-ATTRB   PIC X.                       
00581              15  EL142H-DATE         PIC X(8).                    
00582                                                                   
00583              15  EL142H-OC-LENGTH    PIC S9(4)  COMP.             
00584              15  EL142H-OC-ATTRB     PIC X.                       
00585              15  EL142H-OC           PIC X.                       
00586                                                                   
00587              15  EL142H-CAUSE-LENGTH PIC S9(4)  COMP.             
00588              15  EL142H-CAUSE-ATTRB  PIC X.                       
00589              15  EL142H-CAUSE        PIC X(5).                    
00590                                                                   
00591      EJECT                                                        
00592                                  COPY ELCEMIB.                    
00593      EJECT                                                        
00594                                  COPY ELCDATE.                    
00595      EJECT                                                        
00596                                  COPY ELCLOGOF.                   
00597      EJECT                                                        
00598                                  COPY ELCATTR.                    
00599      EJECT                                                        
00600                                  COPY ELCAID.                     
00601  01  FILLER REDEFINES DFHAID.                                     
00602      05  FILLER                      PIC X(8).                    
00603      05  PF-VALUES                   PIC X  OCCURS 24.            
00604                                                                   
00605      EJECT                                                        
00606                                                                   
00607      COPY ELCDMO.                                                 

       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

00611  LINKAGE SECTION.                                                 
00612                                                                   
00613  01  DFHCOMMAREA                     PIC X(1024).                 
00614                                                                   
00615 *01 DFHBLLDS                         COMP SYNC.                   
00616 *    05  BLLCBAR                     PIC S9(9).                   
00617 *    05  ELTRLR-POINTER              PIC S9(9).                   
00618 *    05  ELMSTR-POINTER              PIC S9(9).                   
00619 *    05  ERACCT-POINTER              PIC S9(9).                   
00620 *    05  ELARCH-POINTER              PIC S9(9).                   
00621 *    05  ELCHKQ-POINTER              PIC S9(9).                   
00622 *    05  ELCNTL-POINTER              PIC S9(9).                   
00623 *    05  EMPROD-POINTER              PIC S9(9).                   
00624                                                                   
00625      EJECT                                                        
00626                                  COPY ELCTRLR.                    
00627      EJECT                                                        
00628                                  COPY ELCMSTR.                    
00629      EJECT                                                        
00630                                  COPY ERCACCT.                    
00631      EJECT                                                        
00632                                  COPY ELCARCH.                    
00633      EJECT                                                        
00634                                  COPY ELCARCT.                    
00635      EJECT                                                        
00636                                  COPY ELCCHKQ.                    
00637      EJECT                                                        
00638                                  COPY ELCCNTL.

020810                                 COPY ELCDENY.

102510                                 COPY ELCNAPS.
102510
041613                                 COPY ELCENCC.
041613
00640                                  COPY MPCPROD.                    
00641      EJECT                                                        
00642                                  COPY ERCDMDNT.                   
00643      EJECT                                                        
00644  PROCEDURE DIVISION.                                              
00645                                                                   
00646      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     
00647                                                                   
00648 *    NOTE ******************************************************* 
00649 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * 
00650 *         *  FROM ANOTHER MODULE.                               * 
00651 *         *******************************************************.
00652                                                                   
00653      MOVE PI-EL142-PRIORITY      TO WS-PI-EL142-PRIORITY.         
00654                                                                   
00655      IF EIBCALEN NOT GREATER ZERO                                 
00656          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   
00657          GO TO 8300-SEND-TEXT.                                    
00658                                                                   
00659      EXEC CICS HANDLE CONDITION                                   
00660          PGMIDERR (9600-PGMIDERR)                                 
00661          NOTFND   (0130-MAIN-LOGIC)                               
00662          ENDFILE  (6000-END-OF-FILE)                              
00663          ERROR    (9990-ERROR)                                    
00664      END-EXEC.                                                    
00665                                                                   
00666      MOVE EIBDATE               TO  DC-JULIAN-YYDDD.              
00667      MOVE '5'                   TO  DC-OPTION-CODE.               
00668      PERFORM 8500-DATE-CONVERSION.                                
00669      MOVE DC-BIN-DATE-1         TO  WS-CURRENT-DATE.              
00670      MOVE DC-GREG-DATE-1-YMD    TO  SAVE-DATE-YMD.                
00671                                                                   
00672      IF SAVE-DATE-YY GREATER 70                                   
00673          MOVE 19                TO  SAVE-DATE-CC                  
00674        ELSE                                                       
00675          MOVE 20                TO  SAVE-DATE-CC.                 
00676                                                                   
00677      EJECT                                                        
00678  0010-MAIN-LOGIC.                                                 
00679      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
00680          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
00681              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     
00682              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     
00683              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     
00684              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     
00685              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     
00686              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     
00687              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   
00688              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     
00689            ELSE                                                   
00690              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     
00691              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   
00692              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     
00693              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     
00694              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     
00695              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     
00696              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     
00697              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     
00698        ELSE                                                       
00699          GO TO 0020-MAIN-LOGIC.                                   
00700                                                                   
00701  0015-MAIN-LOGIC.    
00702 *    NOTE ******************************************************* 
00703 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * 
00704 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * 
00705 *         *******************************************************.
00706                                                                   
00707      IF PI-RETURN-TO-PROGRAM = PGM-EL1501                         
00708          NEXT SENTENCE                                            
00709      ELSE                                                         
00710          MOVE +1                 TO  PI-REMINDERS-SW              
00711                                      PI-LETTERS-SW                
00712                                      PI-PAYMENTS-SW               
00713                                      PI-AUTO-PAY-SW               
00714                                      PI-NOTES-SW                  
00715                                      PI-RES-EXP-SW                
00716                                      PI-DENIALS-SW                
00717                                      PI-INCURRED-DATE-SW          
00718                                      PI-FORMS-SW.                 
00719                                                                   
00720      MOVE ZERO                   TO  PI-END-OF-FILE               
00721                                      PI-RECORD-COUNT              
00722                                      PI-TRAILER-NUMBER
042110                                     PI-MAPG-DELETE-CNT
00723                                                                   
00724      MOVE LOW-VALUES             TO  PI-AFTER-DATE                
00725                                      PI-AFTER-DATE-2              
00726                                      PI-AFTER-DATE-3.             
00727                                                                   
00728      MOVE DFHENTER               TO  PI-PREV-AID.                 
00729                                                                   
020413     MOVE '1'                    TO PI-APPROVAL-LEVEL
020413     MOVE PI-COMPANY-ID          TO CNTL-CO
020413     MOVE '2'                    TO CNTL-RECORD-TYPE
020413     MOVE +0                     TO CNTL-SEQ
020413     MOVE PI-PROCESSOR-ID        TO CNTL-GENL
020413
020413     EXEC CICS READ
020413         DATASET  ('ELCNTL')
020413         SET      (ADDRESS OF CONTROL-FILE)
020413         RIDFLD   (WS-CONTROL-FILE-KEY)
020413         RESP     (WS-RESPONSE)
020413     END-EXEC
020413     IF RESP-NORMAL
020413        MOVE CF-APPROVAL-LEVEL TO PI-APPROVAL-LEVEL
020413     END-IF
020413
00730      IF FIRST-TIME                                                
00731          MOVE 'N'                TO  PI-FIRST-TIME-SW             
00732          IF PI-RETURN-TO-PROGRAM = PGM-EL1501                     
00733              GO TO 4000-READ-TRAILER-FILE.                        
00734                                                                   
00735      MOVE LOW-VALUES             TO  EL142AO.                     
00736                                                                   
00737      MOVE PI-TRAILER-NUMBER      TO  ASEQO.                       
00738      MOVE AL-UNNON               TO  ASEQA.                       
00739      MOVE -1                     TO  ARECDTEL.                    
00740                                                                   
00741      MOVE EL142A                 TO  PI-MAP-NAME.                 
00742                                                                   
00743      PERFORM 8100-SEND-INITIAL-MAP.                               
00744                                                                   
00745      GO TO 9100-RETURN-TRAN.                                      
00746                                                                   
00747      EJECT                                                        
00748  0020-MAIN-LOGIC.   
00749 *    NOTE ******************************************************* 
00750 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * 
00751 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * 
00752 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * 
00753 *         *******************************************************.
00754                                                                   
00755      IF EIBAID = DFHCLEAR                                         
00756          GO TO 9400-CLEAR.                                        
00757                                                                   
00758      IF PI-PROCESSOR-ID = 'LGXX'                                  
00759          NEXT SENTENCE                                            
00760      ELSE                                                         
00761          EXEC CICS  READQ TS                                      
00762              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  
00763              INTO    (SECURITY-CONTROL)                           
00764              LENGTH  (SC-COMM-LENGTH)                             
00765              ITEM    (SC-ITEM)                                    
00766          END-EXEC                                                 
00767          MOVE SC-CLAIMS-DISPLAY (15)  TO  PI-DISPLAY-CAP          
00768          MOVE SC-CLAIMS-UPDATE  (15)  TO  PI-MODIFY-CAP           
00769          IF NOT DISPLAY-CAP                                       
00770              MOVE 'READ'          TO SM-READ                      
00771              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       
00772              MOVE ER-0070         TO  EMI-ERROR                   
00773              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00774              GO TO 8100-SEND-INITIAL-MAP.                         
00775                                                                   
00776      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
00777          MOVE ER-0008            TO  EMI-ERROR                    
00778          IF PI-MAP-NAME = EL142A                                  
00779              MOVE -1             TO  ARECDTEL                     
00780              PERFORM 8200-SEND-DATAONLY                           
00781            ELSE                                                   
00782          IF PI-MAP-NAME = EL142B                                  
00783              MOVE -1             TO  BPFKL                        
00784              PERFORM 8200-SEND-DATAONLY                           
00785            ELSE                                                   
00786          IF PI-MAP-NAME = EL142B2                                 
00787              MOVE -1             TO  KPFKL                        
00788              PERFORM 8200-SEND-DATAONLY                           
00789            ELSE                                                   
00790          IF PI-MAP-NAME = EL142C                                  
00791              MOVE -1             TO  CPFKL                        
00792              PERFORM 8200-SEND-DATAONLY                           
00793            ELSE                                                   
00794          IF PI-MAP-NAME = EL142D                                  
00795              MOVE -1             TO  DPFKL                        
00796              PERFORM 8200-SEND-DATAONLY                           
00797            ELSE                                                   
00798          IF PI-MAP-NAME = EL142D2                                 
00799              MOVE -1             TO  LPFKL                        
00800              PERFORM 8200-SEND-DATAONLY                           
00801            ELSE                                                   
00802          IF PI-MAP-NAME = EL142E                                  
00803              MOVE -1             TO  EPFKL                        
00804              PERFORM 8200-SEND-DATAONLY                           
00805            ELSE                                                   
00806          IF PI-MAP-NAME = EL142F                                  
00807              MOVE -1             TO  FPFKL                        
00808              PERFORM 8200-SEND-DATAONLY                           
00809            ELSE                                                   
00810          IF PI-MAP-NAME = EL142G                                  
00811              MOVE -1             TO  GPFKL                        
00812              PERFORM 8200-SEND-DATAONLY                           
00813            ELSE                                                   
00814          IF PI-MAP-NAME = EL142H                                  
00815              MOVE -1             TO  HPFKL                        
00816              PERFORM 8200-SEND-DATAONLY                           
00817            ELSE                                                   
00818          IF PI-MAP-NAME = EL142I                                  
00819              MOVE -1             TO  IPFKL                        
00820              PERFORM 8200-SEND-DATAONLY                           
00821            ELSE                                                   
00822          IF PI-MAP-NAME = EL142J                                  
00823              MOVE -1             TO  JPFKL                        
00824              PERFORM 8200-SEND-DATAONLY                           
00825            ELSE                                                   
00826              PERFORM 8200-SEND-DATAONLY.                          
00827                                                                   
00828      EXEC CICS RECEIVE                                            
00829          INTO   (EL142DI)                                         
00830          MAPSET (WS-MAPSET-NAME)                                  
00831          MAP    (PI-MAP-NAME)                                     
00832      END-EXEC.                                                    
00833                                                                   
00834      IF PI-MAP-NAME = EL142B                                      
00835          IF BPFKL GREATER ZERO                                    
00836              IF EIBAID NOT = DFHENTER                             
00837                  MOVE ER-0004    TO  EMI-ERROR                    
00838                  MOVE AL-UNBOF   TO  BPFKA                        
00839                  MOVE -1         TO  BPFKL                        
00840                  PERFORM 8200-SEND-DATAONLY                       
00841                ELSE                                               
00842                  IF (BPFKO NUMERIC) AND                           
00843                     (BPFKO GREATER ZERO AND LESS 25)              
00844                      MOVE PF-VALUES (BPFKI)  TO  EIBAID           
00845                    ELSE                                           
00846                      MOVE ER-0029  TO  EMI-ERROR                  
00847                      MOVE AL-UNBON TO  BPFKA                      
00848                      MOVE -1       TO  BPFKL                      
00849                      PERFORM 8200-SEND-DATAONLY                   
00850            ELSE                                                   
00851              NEXT SENTENCE                                        
00852        ELSE                                                       
00853      IF PI-MAP-NAME = EL142B2                                     
00854         IF KPFKL GREATER ZERO                                     
00855            IF EIBAID NOT = DFHENTER                               
00856               MOVE ER-0004       TO  EMI-ERROR                    
00857               MOVE AL-UNBOF      TO  KPFKA                        
00858               MOVE -1            TO  KPFKL                        
00859               PERFORM 8200-SEND-DATAONLY                          
00860            ELSE                                                   
00861               IF (KPFKO NUMERIC) AND                              
00862                  (KPFKO GREATER ZERO AND LESS 25)                 
00863                   MOVE PF-VALUES (KPFKI)  TO  EIBAID              
00864               ELSE                                                
00865                   MOVE ER-0029    TO  EMI-ERROR                   
00866                   MOVE AL-UNBOF   TO  KPFKA                       
00867                   MOVE -1         TO  KPFKL                       
00868                   PERFORM 8200-SEND-DATAONLY                      
00869         ELSE                                                      
00870            NEXT SENTENCE                                          
00871      ELSE                                                         
00872      IF PI-MAP-NAME = EL142C                                      
00873          IF CPFKL GREATER ZERO                                    
00874              IF EIBAID NOT = DFHENTER                             
00875                  MOVE ER-0004    TO  EMI-ERROR                    
00876                  MOVE AL-UNBOF   TO  CPFKA                        
00877                  MOVE -1         TO  CPFKL                        
00878                  PERFORM 8200-SEND-DATAONLY                       
00879                ELSE                                               
00880                  IF (CPFKO NUMERIC) AND                           
00881                     (CPFKO GREATER ZERO AND LESS 25)              
00882                      MOVE PF-VALUES (CPFKI)  TO  EIBAID           
00883                    ELSE                                           
00884                      MOVE ER-0029  TO  EMI-ERROR                  
00885                      MOVE AL-UNBOF TO  CPFKA                      
00886                      MOVE -1       TO  CPFKL                      
00887                      PERFORM 8200-SEND-DATAONLY                   
00888            ELSE                                                   
00889              NEXT SENTENCE                                        
00890        ELSE                                                       
00891      IF PI-MAP-NAME = EL142D                                      
00892          IF DPFKL GREATER ZERO                                    
00893              IF EIBAID NOT = DFHENTER                             
00894                  MOVE ER-0004    TO  EMI-ERROR                    
00895                  MOVE AL-UNBOF   TO  DPFKA                        
00896                  MOVE -1         TO  DPFKL                        
00897                  PERFORM 8200-SEND-DATAONLY                       
00898              ELSE                                                 
00899                  IF (DPFKO NUMERIC) AND                           
00900                     (DPFKO GREATER ZERO AND LESS 25)              
00901                      MOVE PF-VALUES (DPFKI)  TO  EIBAID           
00902                  ELSE                                             
00903                      MOVE ER-0029  TO  EMI-ERROR                  
00904                      MOVE AL-UNBOF TO  DPFKA                      
00905                      MOVE -1       TO  DPFKL                      
00906                      PERFORM 8200-SEND-DATAONLY                   
00907          ELSE                                                     
00908              NEXT SENTENCE                                        
00909      ELSE                                                         
00910      IF PI-MAP-NAME = EL142D2                                     
00911         IF LPFKL GREATER ZERO                                     
00912            IF EIBAID NOT = DFHENTER                               
00913               MOVE ER-0004       TO  EMI-ERROR                    
00914               MOVE AL-UNBOF      TO  LPFKA                        
00915               MOVE -1            TO  LPFKL                        
00916               PERFORM 8200-SEND-DATAONLY                          
00917            ELSE                                                   
00918               IF (LPFKO NUMERIC) AND                              
00919                  (LPFKO GREATER ZERO AND LESS 25)                 
00920                  MOVE PF-VALUES (LPFKI)  TO  EIBAID               
00921               ELSE                                                
00922                  MOVE ER-0029    TO  EMI-ERROR                    
00923                  MOVE AL-UNBON   TO  LPFKA                        
00924                  MOVE -1         TO  LPFKL                        
00925                  PERFORM 8200-SEND-DATAONLY                       
00926         ELSE                                                      
00927            NEXT SENTENCE                                          
00928      ELSE                                                         
00929      IF PI-MAP-NAME = EL142E                                      
00930          IF EPFKL GREATER ZERO                                    
00931              IF EIBAID NOT = DFHENTER                             
00932                  MOVE ER-0004    TO  EMI-ERROR                    
00933                  MOVE AL-UNBOF   TO  EPFKA                        
00934                  MOVE -1         TO  EPFKL                        
00935                  PERFORM 8200-SEND-DATAONLY                       
00936                ELSE                                               
00937                  IF (EPFKO NUMERIC) AND                           
00938                     (EPFKO GREATER ZERO AND LESS 25)              
00939                      MOVE PF-VALUES (EPFKI)  TO  EIBAID           
00940                    ELSE                                           
00941                      MOVE ER-0029  TO  EMI-ERROR                  
00942                      MOVE AL-UNBOF TO  EPFKA                      
00943                      MOVE -1       TO  EPFKL                      
00944                      PERFORM 8200-SEND-DATAONLY                   
00945            ELSE                                                   
00946              NEXT SENTENCE                                        
00947        ELSE                                                       
00948      IF PI-MAP-NAME = EL142F                                      
00949          IF FPFKL GREATER ZERO                                    
00950              IF EIBAID NOT = DFHENTER                             
00951                  MOVE ER-0004    TO  EMI-ERROR                    
00952                  MOVE AL-UNBOF   TO  FPFKA                        
00953                  MOVE -1         TO  FPFKL                        
00954                  PERFORM 8200-SEND-DATAONLY                       
00955                ELSE                                               
00956                  IF (FPFKO NUMERIC) AND                           
00957                     (FPFKO GREATER ZERO AND LESS 25)              
00958                      MOVE PF-VALUES (FPFKI)  TO  EIBAID           
00959                    ELSE                                           
00960                      MOVE ER-0029 TO  EMI-ERROR                   
00961                      MOVE AL-UNBOF TO  FPFKA                      
00962                      MOVE -1      TO  FPFKL                       
00963                      PERFORM 8200-SEND-DATAONLY                   
00964            ELSE                                                   
00965              NEXT SENTENCE                                        
00966        ELSE                                                       
00967      IF PI-MAP-NAME = EL142G                                      
00968          IF GPFKL GREATER ZERO                                    
00969              IF EIBAID NOT = DFHENTER                             
00970                  MOVE ER-0004    TO  EMI-ERROR                    
00971                  MOVE AL-UNBOF   TO  GPFKA                        
00972                  MOVE -1         TO  GPFKL                        
00973                  PERFORM 8200-SEND-DATAONLY                       
00974                ELSE                                               
00975                  IF (GPFKO NUMERIC) AND                           
00976                     (GPFKO GREATER ZERO AND LESS 25)              
00977                      MOVE PF-VALUES (GPFKI)  TO  EIBAID           
00978                    ELSE                                           
00979                      MOVE ER-0029  TO  EMI-ERROR                  
00980                      MOVE AL-UNBOF TO  GPFKA                      
00981                      MOVE -1       TO  GPFKL                      
00982                      PERFORM 8200-SEND-DATAONLY                   
00983            ELSE                                                   
00984              NEXT SENTENCE                                        
00985        ELSE                                                       
00986      IF PI-MAP-NAME = EL142H                                      
00987          IF HPFKL GREATER ZERO                                    
00988              IF EIBAID NOT = DFHENTER                             
00989                  MOVE ER-0004    TO  EMI-ERROR                    
00990                  MOVE AL-UNBOF   TO  HPFKA                        
00991                  MOVE -1         TO  HPFKL                        
00992                  PERFORM 8200-SEND-DATAONLY                       
00993                ELSE                                               
00994                  IF (HPFKO NUMERIC) AND                           
00995                     (HPFKO GREATER ZERO AND LESS 25)              
00996                      MOVE PF-VALUES (HPFKI)  TO  EIBAID           
00997                    ELSE                                           
00998                      MOVE ER-0029  TO  EMI-ERROR                  
00999                      MOVE AL-UNBOF TO  HPFKA                      
01000                      MOVE -1       TO  HPFKL                      
01001                      PERFORM 8200-SEND-DATAONLY                   
01002            ELSE                                                   
01003              NEXT SENTENCE                                        
01004        ELSE                                                       
01005      IF PI-MAP-NAME = EL142I                                      
01006          IF IPFKL GREATER ZERO                                    
01007              IF EIBAID NOT = DFHENTER                             
01008                  MOVE ER-0004    TO  EMI-ERROR                    
01009                  MOVE AL-UNBOF   TO  IPFKA                        
01010                  MOVE -1         TO  IPFKL                        
01011                  PERFORM 8200-SEND-DATAONLY                       
01012                ELSE                                               
01013                  IF (IPFKO NUMERIC) AND                           
01014                     (IPFKO GREATER ZERO AND LESS 25)              
01015                      MOVE PF-VALUES (IPFKI)  TO  EIBAID           
01016                    ELSE                                           
01017                      MOVE ER-0029  TO  EMI-ERROR                  
01018                      MOVE AL-UNBOF TO  IPFKA                      
01019                      MOVE -1       TO  IPFKL                      
01020                      PERFORM 8200-SEND-DATAONLY                   
01021            ELSE                                                   
01022              NEXT SENTENCE                                        
01023        ELSE                                                       
01024      IF PI-MAP-NAME = EL142J                                      
01025          IF JPFKL GREATER ZERO                                    
01026              IF EIBAID NOT = DFHENTER                             
01027                  MOVE ER-0004    TO  EMI-ERROR                    
01028                  MOVE AL-UNBOF   TO  JPFKA                        
01029                  MOVE -1         TO  JPFKL                        
01030                  PERFORM 8200-SEND-DATAONLY                       
01031                ELSE                                               
01032                  IF (JPFKO NUMERIC) AND                           
01033                     (JPFKO GREATER ZERO AND LESS 25)              
01034                      MOVE PF-VALUES (JPFKI)  TO  EIBAID           
01035                    ELSE                                           
01036                      MOVE ER-0029  TO  EMI-ERROR                  
01037                      MOVE AL-UNBOF TO  JPFKA                      
01038                      MOVE -1       TO  JPFKL                      
01039                      PERFORM 8200-SEND-DATAONLY.                  
01040                                                                   
01041      IF EIBAID = DFHPF12                                          
01042          MOVE EL010              TO  XCTL-PGM                     
01043          GO TO 9300-XCTL.                                         
01044                                                                   
01045      IF EIBAID = DFHPF23                                          
01046          GO TO 9000-RETURN-CICS.                                  
01047                                                                   
01048      IF EIBAID = DFHPF24                                          
01049          MOVE EL126              TO  XCTL-PGM                     
01050          GO TO 9300-XCTL.                                         
01051                                                                   
01052      IF EIBAID = DFHENTER OR                                      
01053        (EIBAID = DFHPF1 OR DFHPF2 OR DFHPF5) AND                  
01054        (PI-MAP-NAME NOT = EL142A AND EL142B2 AND EL142D2)         
01055                    OR                                             
01056        (EIBAID = DFHPF6 AND                                       
01057         PI-PROCESSOR-ID = 'PEMA' AND
01058         PI-MAP-NAME = EL142B OR EL142D)                           
01059            GO TO 0040-MAIN-LOGIC.                                 
01060                                                                   
01061      MOVE ER-0008                TO  EMI-ERROR.                   
01062                                                                   
01063      IF PI-MAP-NAME = EL142A                                      
01064          MOVE -1                 TO  ARECDTEL                     
01065        ELSE                                                       
01066      IF PI-MAP-NAME = EL142B                                      
01067          MOVE -1                 TO  BPFKL                        
01068        ELSE                                                       
01069      IF PI-MAP-NAME = EL142B2                                     
01070          MOVE -1                 TO  KPFKL                        
01071        ELSE                                                       
01072      IF PI-MAP-NAME = EL142C                                      
01073          MOVE -1                 TO  CPFKL                        
01074        ELSE                                                       
01075      IF PI-MAP-NAME = EL142D                                      
01076          MOVE -1                 TO  DPFKL                        
01077        ELSE                                                       
01078      IF PI-MAP-NAME = EL142D2                                     
01079          MOVE -1                 TO  LPFKL                        
01080        ELSE                                                       
01081      IF PI-MAP-NAME = EL142E                                      
01082          MOVE -1                 TO  EPFKL                        
01083        ELSE                                                       
01084      IF PI-MAP-NAME = EL142F                                      
01085          MOVE -1                 TO  FPFKL                        
01086        ELSE                                                       
01087      IF PI-MAP-NAME = EL142G                                      
01088          MOVE -1                 TO  GPFKL                        
01089        ELSE                                                       
01090      IF PI-MAP-NAME = EL142H                                      
01091          MOVE -1                 TO  HPFKL                        
01092        ELSE                                                       
01093      IF PI-MAP-NAME = EL142I                                      
01094          MOVE -1                 TO  IPFKL                        
01095        ELSE                                                       
01096      IF PI-MAP-NAME = EL142J                                      
01097          MOVE -1                 TO  JPFKL.                       
01098                                                                   
01099      PERFORM 8200-SEND-DATAONLY.                                  
01100                                                                   
01101  0040-MAIN-LOGIC.                                                 
01102      IF EIBAID = DFHPF1 OR DFHPF2                                 
01103          PERFORM 4000-READ-TRAILER-FILE.                          
01104                                                                   
01105      IF EIBAID = DFHPF5                                           
01106          GO TO 0015-MAIN-LOGIC.                                   
01107                                                                   
01108      EJECT                                                        
01109  0100-MAIN-LOGIC.                                                 
01110      IF PI-MAP-NAME NOT = EL142A                                  
01111          GO TO 0200-MAIN-LOGIC.                                   
01112                                                                   
01113      MOVE +3                     TO  EMI-NUMBER-OF-LINES.         
01114      MOVE +2                     TO  EMI-SWITCH2.                 
01115                                                                   
01116      IF ARECDTEL NOT GREATER ZERO                                 
01117          MOVE LOW-VALUES         TO  PI-AFTER-DATE                
01118      ELSE                                                         
01119          MOVE ARECDTEI           TO  WS-DEEDIT-FIELD              
01120          PERFORM 8600-DEEDIT                                      
01121          MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY           
01122          MOVE '4'                TO  DC-OPTION-CODE               
01123          PERFORM 8500-DATE-CONVERSION                             
01124          IF DC-ERROR-CODE NOT = SPACE                             
01125              MOVE ER-0285        TO  EMI-ERROR                    
01126              PERFORM 9900-ERROR-FORMAT                            
01127              MOVE -1             TO  ARECDTEL                     
01128              MOVE AL-UNBON       TO  ARECDTEA                     
01129          ELSE                                                     
01130              MOVE +1             TO  WS-UPDATE-SW                 
01131              MOVE DC-BIN-DATE-1  TO  PI-AFTER-DATE                
01132              MOVE AL-UNNON       TO  ARECDTEA                     
01133              MOVE WS-DEEDIT-FIELD-V0  TO  ARECDTEO                
01134              INSPECT ARECDTEI CONVERTING SPACES TO SLASH.         
01135                                                                   
01136      IF AREMINDL GREATER ZERO                                     
01137          IF AREMINDI = 'Y'                                        
01138              MOVE +1             TO  PI-REMINDERS-SW              
01139                                      WS-UPDATE-SW                 
01140              MOVE AL-UANON       TO  AREMINDA                     
01141          ELSE                                                     
01142              IF AREMINDI = 'N'                                    
01143                  MOVE ZERO       TO  PI-REMINDERS-SW              
01144                  MOVE AL-UANON   TO  AREMINDA                     
01145              ELSE                                                 
01146                  MOVE ER-0286    TO  EMI-ERROR                    
01147                  PERFORM 9900-ERROR-FORMAT                        
01148                  MOVE -1         TO  AREMINDL                     
01149                  MOVE AL-UABON   TO  AREMINDA                     
01150      ELSE                                                         
01151          MOVE ER-0286            TO  EMI-ERROR                    
01152          PERFORM 9900-ERROR-FORMAT                                
01153          MOVE -1                 TO  AREMINDL                     
01154          MOVE AL-UABON           TO  AREMINDA.                    
01155                                                                   
01156      IF ALETTERL GREATER ZERO                                     
01157          IF ALETTERI = 'Y'                                        
01158              MOVE +1             TO  PI-LETTERS-SW                
01159                                      WS-UPDATE-SW                 
01160              MOVE AL-UANON       TO  ALETTERA                     
01161          ELSE                                                     
01162              IF ALETTERI = 'N'                                    
01163                  MOVE ZERO       TO  PI-LETTERS-SW                
01164                  MOVE AL-UANON   TO  ALETTERA                     
01165              ELSE                                                 
01166                  MOVE ER-0287    TO  EMI-ERROR                    
01167                  PERFORM 9900-ERROR-FORMAT                        
01168                  MOVE -1         TO  ALETTERL                     
01169                  MOVE AL-UABON   TO  ALETTERA                     
01170      ELSE                                                         
01171          MOVE ER-0287            TO  EMI-ERROR                    
01172          PERFORM 9900-ERROR-FORMAT                                
01173          MOVE -1                 TO  ALETTERL                     
01174          MOVE AL-UABON           TO  ALETTERA.                    
01175                                                                   
01176      IF APAYMNTL GREATER ZERO                                     
01177          IF APAYMNTI = 'Y'                                        
01178              MOVE +1             TO  PI-PAYMENTS-SW               
01179                                      WS-UPDATE-SW                 
01180              MOVE AL-UANON       TO  APAYMNTA                     
01181          ELSE                                                     
01182              IF APAYMNTI = 'N'                                    
01183                  MOVE ZERO       TO  PI-PAYMENTS-SW               
01184                  MOVE AL-UANON   TO  APAYMNTA                     
01185              ELSE                                                 
01186                  MOVE ER-0288       TO  EMI-ERROR                 
01187                  PERFORM 9900-ERROR-FORMAT                        
01188                  MOVE -1         TO  APAYMNTL                     
01189                  MOVE AL-UABON   TO  APAYMNTA                     
01190      ELSE                                                         
01191          MOVE ER-0288            TO  EMI-ERROR                    
01192          PERFORM 9900-ERROR-FORMAT                                
01193          MOVE -1                 TO  APAYMNTL                     
01194          MOVE AL-UABON           TO  APAYMNTA.                    
01195                                                                   
01196      IF AAUTOPAL GREATER ZERO                                     
01197          IF AAUTOPAI = 'Y'                                        
01198              MOVE +1             TO  PI-AUTO-PAY-SW               
01199                                      WS-UPDATE-SW                 
01200              MOVE AL-UANON       TO  AAUTOPAA                     
01201          ELSE                                                     
01202              IF AAUTOPAI = 'N'                                    
01203                  MOVE ZERO       TO  PI-AUTO-PAY-SW               
01204                  MOVE AL-UANON   TO  AAUTOPAA                     
01205              ELSE                                                 
01206                  MOVE ER-0289       TO  EMI-ERROR                 
01207                  PERFORM 9900-ERROR-FORMAT                        
01208                  MOVE -1         TO  AAUTOPAL                     
01209                  MOVE AL-UABON   TO  AAUTOPAA                     
01210      ELSE                                                         
01211          MOVE ER-0289            TO  EMI-ERROR                    
01212          PERFORM 9900-ERROR-FORMAT                                
01213          MOVE -1                 TO  AAUTOPAL                     
01214          MOVE AL-UABON           TO  AAUTOPAA.                    
01215                                                                   
01216      IF ANOTESL GREATER ZERO                                      
01217          IF ANOTESI = 'Y'                                         
01218              MOVE +1             TO  PI-NOTES-SW                  
01219                                      WS-UPDATE-SW                 
01220              MOVE AL-UANON       TO  ANOTESA                      
01221          ELSE                                                     
01222              IF ANOTESI = 'N'                                     
01223                  MOVE ZERO       TO  PI-NOTES-SW                  
01224                  MOVE AL-UANON   TO  ANOTESA                      
01225              ELSE                                                 
01226                  MOVE ER-0290       TO  EMI-ERROR                 
01227                  PERFORM 9900-ERROR-FORMAT                        
01228                  MOVE -1         TO  ANOTESL                      
01229                  MOVE AL-UABON   TO  ANOTESA                      
01230      ELSE                                                         
01231          MOVE ER-0290            TO  EMI-ERROR                    
01232          PERFORM 9900-ERROR-FORMAT                                
01233          MOVE -1                 TO  ANOTESL                      
01234          MOVE AL-UABON           TO  ANOTESA.                     
01235                                                                   
01236      IF ARESEXPL GREATER ZERO                                     
01237          IF ARESEXPI = 'Y'                                        
01238              MOVE +1             TO  PI-RES-EXP-SW                
01239                                      WS-UPDATE-SW                 
01240              MOVE AL-UANON       TO  ARESEXPA                     
01241          ELSE                                                     
01242              IF ARESEXPI = 'N'                                    
01243                  MOVE ZERO       TO  PI-RES-EXP-SW                
01244                  MOVE AL-UANON   TO  ARESEXPA                     
01245              ELSE                                                 
01246                  MOVE ER-0291    TO  EMI-ERROR                    
01247                  PERFORM 9900-ERROR-FORMAT                        
01248                  MOVE -1         TO  ARESEXPL                     
01249                  MOVE AL-UABON   TO  ARESEXPA                     
01250      ELSE                                                         
01251          MOVE ER-0291            TO  EMI-ERROR                    
01252          PERFORM 9900-ERROR-FORMAT                                
01253          MOVE -1                 TO  ARESEXPL                     
01254          MOVE AL-UABON           TO  ARESEXPA.                    
01255                                                                   
01256      IF ADENIALL GREATER ZERO                                     
01257          IF ADENIALI = 'Y'                                        
01258              MOVE +1             TO  PI-DENIALS-SW                
01259                                      WS-UPDATE-SW                 
01260              MOVE AL-UANON       TO  ADENIALA                     
01261          ELSE                                                     
01262              IF ADENIALI = 'N'                                    
01263                  MOVE ZERO       TO  PI-DENIALS-SW                
01264              MOVE AL-UANON       TO  ADENIALA                     
01265              ELSE                                                 
01266                  MOVE ER-0292    TO  EMI-ERROR                    
01267                  PERFORM 9900-ERROR-FORMAT                        
01268                  MOVE -1         TO  ADENIALL                     
01269                  MOVE AL-UABON   TO  ADENIALA                     
01270      ELSE                                                         
01271          MOVE ER-0292            TO  EMI-ERROR                    
01272          PERFORM 9900-ERROR-FORMAT                                
01273          MOVE -1                 TO  ADENIALL                     
01274          MOVE AL-UABON           TO  ADENIALA.                    
01275                                                                   
01276      IF AIDCL GREATER ZERO                                        
01277          IF AIDCI = 'Y'                                           
01278              MOVE +1             TO  PI-INCURRED-DATE-SW          
01279                                      WS-UPDATE-SW                 
01280              MOVE AL-UANON       TO  AIDCA                        
01281          ELSE                                                     
01282              IF AIDCI = 'N'                                       
01283                  MOVE ZERO       TO  PI-INCURRED-DATE-SW          
01284                  MOVE AL-UANON   TO  AIDCA                        
01285              ELSE                                                 
01286                  MOVE ER-0341    TO  EMI-ERROR                    
01287                  PERFORM 9900-ERROR-FORMAT                        
01288                  MOVE -1         TO  AIDCL                        
01289                  MOVE AL-UABON   TO  AIDCA                        
01290      ELSE                                                         
01291          MOVE ER-0341            TO  EMI-ERROR                    
01292          PERFORM 9900-ERROR-FORMAT                                
01293          MOVE -1                 TO  AIDCL                        
01294          MOVE AL-UABON           TO  AIDCA.                       
01295                                                                   
01296      IF AFORMSL GREATER ZERO                                      
01297          IF AFORMSI = 'Y'                                         
01298              MOVE +1             TO  PI-FORMS-SW                  
01299                                      WS-UPDATE-SW                 
01300              MOVE AL-UANON       TO  AFORMSA                      
01301          ELSE                                                     
01302              IF AFORMSI = 'N'                                     
01303                  MOVE ZERO       TO  PI-FORMS-SW                  
01304                  MOVE AL-UANON   TO  AFORMSA                      
01305              ELSE                                                 
01306                  MOVE ER-0538    TO  EMI-ERROR                    
01307                  PERFORM 9900-ERROR-FORMAT                        
01308                  MOVE -1         TO  AFORMSL                      
01309                  MOVE AL-UABON   TO  AFORMSA                      
01310      ELSE                                                         
01311          MOVE ER-0538            TO  EMI-ERROR                    
01312          PERFORM 9900-ERROR-FORMAT                                
01313          MOVE -1                 TO  AFORMSL                      
01314          MOVE AL-UABON           TO  AFORMSA.                     
01315                                                                   
01316      IF ASEQL NOT GREATER ZERO                                    
01317          MOVE ZERO               TO  PI-TRAILER-NUMBER            
01318      ELSE                                                         
01319          IF ASEQI IS NUMERIC                                      
01320              MOVE ASEQI          TO  PI-TRAILER-NUMBER            
01321          ELSE                                                     
01322              MOVE ER-0293        TO  EMI-ERROR                    
01323              PERFORM 9900-ERROR-FORMAT                            
01324              MOVE -1             TO  ASEQL                        
01325              MOVE AL-UNBON       TO  ASEQA.                       
01326                                                                   
01327      IF WS-UPDATE-SW = ZERO                                       
01328          MOVE ER-0329            TO  EMI-ERROR                    
01329          MOVE -1                 TO  AREMINDL                     
01330          PERFORM 9900-ERROR-FORMAT.                               
01331                                                                   
01332      IF WS-ERROR-COUNT GREATER ZERO                               
01333          PERFORM 8200-SEND-DATAONLY.                              
01334                                                                   
01335      MOVE SPACES                 TO  PI-ACTIVITY-TRAILERS-KEY.    
01336                                                                   
01337      MOVE PI-COMPANY-CD          TO  PI-ATK-COMPANY-CODE.         
01338      MOVE PI-CARRIER             TO  PI-ATK-CARRIER.              
01339      MOVE PI-CLAIM-NO            TO  PI-ATK-CLAIM-NO.             
01340      MOVE PI-CERT-NO             TO  PI-ATK-CERT-NO.              
01341      MOVE PI-TRAILER-NUMBER      TO  PI-ATK-SEQUENCE-NO.          
01342                                                                   
01343      EXEC CICS READ                                               
01344          DATASET   (WS-ACTIVITY-TRAILERS-DSID)                    
01345          RIDFLD    (PI-ACTIVITY-TRAILERS-KEY)                     
01346          SET       (ADDRESS OF ACTIVITY-TRAILERS)                 
01347      END-EXEC                                                     
01348                                                                   
01349      PERFORM 4000-READ-TRAILER-FILE.                              
01350                                                                   
01351  0130-MAIN-LOGIC.                                                 
01352      MOVE ER-0342                TO  EMI-ERROR.                   
01353      MOVE -1                     TO  ARECDTEL.                    
01354      PERFORM 8200-SEND-DATAONLY.                                  
01355                                                                   
01356      EJECT                                                        
01357  0200-MAIN-LOGIC.                                                 
01358      IF PI-MAP-NAME NOT = EL142B                                  
01359          GO TO 0300-MAIN-LOGIC.                                   
01360                                                                   
01361      IF NOT MODIFY-CAP                                            
01362          IF BMAINTI = 'S'                                         
01363              NEXT SENTENCE                                        
01364          ELSE                                                     
01365              MOVE 'UPDATE'       TO SM-READ                       
01366              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       
01367              MOVE ER-0070        TO  EMI-ERROR                    
01368              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
01369              GO TO 8100-SEND-INITIAL-MAP.                         
01370                                                                   
01371      IF EIBAID = DFHPF6                                           
01372          PERFORM 5000-DISPLAY-CHECK-QUEUE.                        
01373                                                                   
01374      MOVE +2                     TO  EMI-NUMBER-OF-LINES          
01375                                      EMI-SWITCH2.                 
01376                                                                   
01377      IF BMAINTL NOT GREATER ZERO                                  
01378        OR (BMAINTL GREATER ZERO AND                               
01379            BMAINTI = 'S')                                         
01380              PERFORM 4000-READ-TRAILER-FILE.                      
01381                                                                   
01382      IF BMAINTI NOT = 'C'                                         
01383          MOVE ER-0023            TO  EMI-ERROR                    
01384          MOVE -1                 TO  BMAINTL                      
01385          MOVE AL-UABON           TO  BMAINTA                      
01386          PERFORM 8200-SEND-DATAONLY.                              
01387                                                                   
020816     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' OR 'VPP'
062121           or 'FNL'
062602        IF (PI-EL142-PRIORITY = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO BMAINTL
062602           MOVE AL-UABON         TO BMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
01388      MOVE AL-UANON               TO  BMAINTA.                     
01389                                                                   
01390      IF BCKNOL GREATER ZERO                                       
01391          MOVE +1                 TO  WS-UPDATE-SW.        
052506
052506     IF  BPRFDTL > 0
052506         MOVE BPRFDTI             TO WS-DEEDIT-FIELD
052506         PERFORM 8600-DEEDIT      
052506         MOVE WS-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY 
052506         MOVE '4'                 TO DC-OPTION-CODE
052506         PERFORM 8500-DATE-CONVERSION
052506         IF DC-ERROR-CODE NOT = SPACES                     
052506            MOVE ER-0021          TO EMI-ERROR        
052506            MOVE -1               TO BPRFDTL         
052506            MOVE AL-UABON         TO BPRFDTA         
052506            PERFORM 9900-ERROR-FORMAT 
052506         ELSE                                   
052506            IF DC-BIN-DATE-1 > WS-CURRENT-DATE
052506                MOVE ER-0873      TO EMI-ERROR
052506                MOVE -1           TO BPRFDTL
052506                MOVE AL-UABON     TO BPRFDTA
052506                PERFORM 9900-ERROR-FORMAT
052506            ELSE                                         
052506                MOVE AL-UANON       TO  BPRFDTA             
052506                MOVE +1             TO  WS-UPDATE-SW         
052506                MOVE DC-BIN-DATE-1    TO WS-PRF-DT     
052506                MOVE WS-DEEDIT-FIELD-V0  TO BPRFDTO         
052506                INSPECT BPRFDTI CONVERTING ' ' TO '/'.                  
052506        
01392                                                                   
01393      IF BPAYEEL GREATER ZERO                                      
01394          IF BPAYEEI = ('I' OR 'B' OR 'A' OR 'O' OR 'Q' OR 'P' OR  
01395                               'E'           OR                    
01396                               'INSURED'     OR                    
01397                               'BENEFICIARY' OR                    
01398                               'ACCOUNT'     OR                    
01399                               'OTHER 1'     OR                    
01400                               'REM BORR'    OR                    
01401                               'EMPLOYER')                         
01402              MOVE +1             TO  WS-UPDATE-SW                 
01403              MOVE AL-UANON       TO  BPAYEEA                      
01404          ELSE                                                     
01405              MOVE -1             TO  BPAYEEL                      
01406              MOVE AL-UABON       TO  BPAYEEA                      
01407              MOVE ER-0294        TO  EMI-ERROR                    
01408              PERFORM 9900-ERROR-FORMAT.                           
01409                                                                   
01410      IF BEXPTYPL GREATER ZERO                                     
01411          MOVE +1                 TO WS-UPDATE-SW                  
01412          IF BEXPTYPI GREATER 0                                    
01413            OR BEXPTYPI = SPACE                                    
01414              MOVE AL-UANON       TO BEXPTYPA                      
01415          ELSE                                                     
01416              MOVE ER-2466        TO EMI-ERROR                     
01417              MOVE -1             TO BEXPTYPL                      
01418              MOVE AL-UABON       TO BEXPTYPA                      
01419              PERFORM 9900-ERROR-FORMAT.                           
01420                                                                   
           IF BEOBYNL > +0
              MOVE +1               TO WS-UPDATE-SW
020413        IF BEOBYNI NOT = 'Y' AND 'N' AND 'S'
                 MOVE ER-1561          TO EMI-ERROR
                 MOVE AL-UABON         TO BEOBYNA
                 MOVE -1               TO BEOBYNL
                 PERFORM 9900-ERROR-FORMAT
              END-IF
           END-IF

020413     IF BCLMYNL > +0
020413        MOVE +1               TO WS-UPDATE-SW
020413        IF BCLMYNI NOT = 'Y' AND 'N'
020413           MOVE ER-1566          TO EMI-ERROR
020413           MOVE AL-UABON         TO BCLMYNA
020413           MOVE -1               TO BCLMYNL
020413           PERFORM 9900-ERROR-FORMAT
020413        END-IF
020413     END-IF
020413
020413     IF BSRVYNL > +0
020413        MOVE +1               TO WS-UPDATE-SW
020413        IF BSRVYNI NOT = 'Y' AND 'N'
020413           MOVE ER-1567          TO EMI-ERROR
020413           MOVE AL-UABON         TO BSRVYNA
020413           MOVE -1               TO BSRVYNL
020413           PERFORM 9900-ERROR-FORMAT
020413        END-IF
020413     END-IF
020413
102413     IF BSPRELL > +0
102413        MOVE +1               TO WS-UPDATE-SW
102413        IF BSPRELI NOT = 'Y' AND 'N'
102413           MOVE ER-1569          TO EMI-ERROR
102413           MOVE AL-UABON         TO BSPRELA
102413           MOVE -1               TO BSPRELL
102413           PERFORM 9900-ERROR-FORMAT
102413        END-IF
102413     END-IF
102413
01421      IF BCRSELL GREATER ZERO                                      
01422          IF BCRSELI = SPACES                                      
01423              MOVE LOW-VALUES     TO  WS-CRSEL                     
01424              MOVE AL-UNNON       TO  BCRSELA                      
01425              MOVE +1             TO  WS-UPDATE-SW                 
01426          ELSE                                                     
01427              MOVE BCRSELI        TO  WS-DEEDIT-FIELD              
01428              PERFORM 8600-DEEDIT                                  
01429              MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY       
01430              MOVE '4'            TO  DC-OPTION-CODE               
01431              PERFORM 8500-DATE-CONVERSION                         
01432              IF DC-ERROR-CODE NOT = SPACE                         
01433                  MOVE ER-0021    TO  EMI-ERROR                    
01434                  PERFORM 9900-ERROR-FORMAT                        
01435                  MOVE -1         TO  BCRSELL                      
01436                  MOVE AL-UNBON   TO  BCRSELA                      
01437              ELSE                                                 
01438                  MOVE +1                  TO  WS-UPDATE-SW        
01439                  MOVE DC-BIN-DATE-1       TO  WS-CRSEL            
01440                  MOVE AL-UNNON            TO  BCRSELA             
01441                  MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT           
01442                  INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH    
01443                  MOVE WS-TEMP-DT          TO BCRSELO.             
01444                                                                   
01445      IF BVOIDSDL GREATER ZERO                                     
01446          IF BVOIDSDI = SPACES                                     
01447              MOVE LOW-VALUES     TO  WS-VOIDSD                    
01448              MOVE AL-UNNON       TO  BVOIDSDA                     
01449              MOVE +1             TO  WS-UPDATE-SW                 
01450          ELSE                                                     
01451              MOVE BVOIDSDI       TO  WS-DEEDIT-FIELD              
01452              PERFORM 8600-DEEDIT                                  
01453              MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY       
01454              MOVE '4'            TO  DC-OPTION-CODE               
01455              PERFORM 8500-DATE-CONVERSION                         
01456              IF DC-ERROR-CODE NOT = SPACE                         
01457                  MOVE ER-0021    TO  EMI-ERROR                    
01458                  PERFORM 9900-ERROR-FORMAT                        
01459                  MOVE -1         TO  BVOIDSDL                     
01460                  MOVE AL-UNBON   TO  BVOIDSDA                     
01461              ELSE                                                 
01462                  MOVE +1                  TO  WS-UPDATE-SW        
01463                  MOVE DC-BIN-DATE-1       TO  WS-VOIDSD           
01464                  MOVE AL-UNNON            TO  BVOIDSDA            
01465                  MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT           
01466                  INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH    
01467                  MOVE WS-TEMP-DT          TO BVOIDSDO.            
01468                                                                   
01469      IF BCRACPL GREATER ZERO                                      
01470          IF BCRACPI = SPACES                                      
01471              MOVE LOW-VALUES     TO  PI-AFTER-DATE-2              
01472              MOVE AL-UNNON       TO  BCRACPA                      
01473              MOVE +1             TO  WS-UPDATE-SW                 
01474          ELSE                                                     
01475              MOVE BCRACPI        TO  WS-DEEDIT-FIELD              
01476              PERFORM 8600-DEEDIT                                  
01477              MOVE WS-DEEDIT-FIELD-V0     TO  DC-GREG-DATE-1-MDY   
01478              MOVE '4'            TO  DC-OPTION-CODE               
01479              PERFORM 8500-DATE-CONVERSION                         
01480              IF DC-ERROR-CODE NOT = SPACE                         
01481                  MOVE ER-0021    TO  EMI-ERROR                    
01482                  PERFORM 9900-ERROR-FORMAT                        
01483                  MOVE -1         TO  BCRACPL                      
01484                  MOVE AL-UNBON   TO  BCRACPA                      
01485              ELSE                                                 
01486                  MOVE +1                  TO WS-UPDATE-SW         
01487                  MOVE DC-BIN-DATE-1       TO PI-AFTER-DATE-2      
01488                  MOVE AL-UNNON            TO BCRACPA              
01489                  MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT           
01490                  INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH    
01491                  MOVE WS-TEMP-DT          TO BCRACPO.             
01492                                                                   
01493      IF BVOIDACL GREATER ZERO                                     
01494          IF BVOIDACI = SPACES                                     
01495              MOVE LOW-VALUES     TO  PI-AFTER-DATE-3              
01496              MOVE +1             TO  WS-UPDATE-SW                 
01497              MOVE AL-UNNON       TO  BVOIDACA                     
01498          ELSE                                                     
01499              MOVE BVOIDACI       TO  WS-DEEDIT-FIELD              
01500              PERFORM 8600-DEEDIT                                  
01501              MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY       
01502              MOVE '4'            TO  DC-OPTION-CODE               
01503              PERFORM 8500-DATE-CONVERSION                         
01504              IF DC-ERROR-CODE NOT = SPACE                         
01505                  MOVE ER-0021    TO  EMI-ERROR                    
01506                  PERFORM 9900-ERROR-FORMAT                        
01507                  MOVE -1         TO  BVOIDACL                     
01508                  MOVE AL-UNBON   TO  BVOIDACA                     
01509              ELSE                                                 
01510                  MOVE +1                  TO WS-UPDATE-SW         
01511                  MOVE DC-BIN-DATE-1       TO PI-AFTER-DATE-3      
01512                  MOVE AL-UNNON            TO BVOIDACA             
01513                  MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT           
01514                  INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH    
01515                  MOVE WS-TEMP-DT          TO BVOIDACO.            
01516                                                                   
01517      IF BHOLDATL GREATER ZERO                                     
01518          IF BHOLDATI = SPACES                                     
01519              MOVE LOW-VALUES     TO  PI-HOLD-UNTIL-DATE           
01520              MOVE +1             TO  WS-UPDATE-SW                 
01521              MOVE AL-UNNON       TO  BHOLDATA                     
01522          ELSE                                                     
01523              MOVE BHOLDATI       TO  WS-DEEDIT-FIELD              
01524              PERFORM 8600-DEEDIT                                  
01525              MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY       
01526              MOVE '4'            TO  DC-OPTION-CODE               
01527              PERFORM 8500-DATE-CONVERSION                         
01528              IF DC-ERROR-CODE NOT = SPACE                         
01529                  MOVE ER-0021    TO  EMI-ERROR                    
01530                  PERFORM 9900-ERROR-FORMAT                        
01531                  MOVE -1         TO  BHOLDATL                     
01532                  MOVE AL-UNBON   TO  BHOLDATA                     
01533              ELSE                                                 
01534                  MOVE +1                  TO WS-UPDATE-SW         
01535                  MOVE DC-BIN-DATE-1       TO PI-HOLD-UNTIL-DATE   
01536                  MOVE AL-UNNON            TO BHOLDATA             
01537                  MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT           
01538                  INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH    
01539                  MOVE WS-TEMP-DT          TO BHOLDATO.            
01540                                                                   
01541      IF BCKQUEL GREATER ZERO                                      
01542          EXEC CICS BIF DEEDIT                                     
01543              FIELD   (BCKQUEI)                                    
01544              LENGTH  (8)                                          
01545          END-EXEC                                                 
01546          IF BCKQUEI NOT NUMERIC                                   
01547              MOVE ER-0570        TO  EMI-ERROR                    
01548              PERFORM 9900-ERROR-FORMAT                            
01549              MOVE -1             TO  BCKQUEL                      
01550              MOVE AL-UNBON       TO  BCKQUEA                      
01551          ELSE                                                     
01552              MOVE AL-UNNON       TO  BCKQUEA                      
01553              MOVE BCKQUEI        TO  WS-CHECK-QUE  BCKQUEO        
01554              MOVE +1             TO  WS-UPDATE-SW.                
01555                                                                   
01556      IF BCKSEQL GREATER ZERO                                      
01557          EXEC CICS BIF DEEDIT                                     
01558              FIELD   (BCKSEQI)                                    
01559              LENGTH  (4)                                          
01560          END-EXEC                                                 
01561          IF BCKSEQI NOT NUMERIC                                   
01562              MOVE ER-0571        TO  EMI-ERROR                    
01563              PERFORM 9900-ERROR-FORMAT                            
01564              MOVE -1             TO  BCKSEQL                      
01565              MOVE AL-UNBON       TO  BCKSEQA                      
01566          ELSE                                                     
01567              MOVE AL-UNNON       TO  BCKSEQA                      
01568              MOVE BCKSEQI        TO  WS-CHECK-QUE-SEQ  BCKSEQO    
01569              MOVE +1             TO  WS-UPDATE-SW.                
01570                                                                   
01571      IF (PI-COMPANY-ID = 'RMC' OR 'LAP') OR                       
01572         (PI-PROCESSOR-ID = 'LGXX')                                
01573          IF BPMTORGL IS GREATER THAN +0                           
01574              IF BPMTORGI = '1' OR '2' OR '3'                      
01575                  MOVE AL-UANON   TO  BPMTORGA                     
01576                  MOVE +1         TO  WS-UPDATE-SW                 
01577              ELSE                                                 
01578                  MOVE ER-0830    TO  EMI-ERROR                    
01579                  PERFORM 9900-ERROR-FORMAT                        
01580                  MOVE AL-UANON   TO  BPMTORGA                     
01581                  MOVE -1         TO  BPMTORGL.                    
01582                                                                   
01583      IF WS-ERROR-COUNT GREATER ZERO                               
01584          PERFORM 8200-SEND-DATAONLY.                              
01585                                                                   
01586      IF WS-UPDATE-SW NOT GREATER ZERO                             
01587          IF (BNOTE1L IS GREATER THAN 0 OR                         
01588              BNOTE2L IS GREATER THAN 0)                           
01589                  GO TO 0200-UPDATE-PMNT-NOTE-TRLR                 
01590               ELSE                                                
01591                  PERFORM 4000-READ-TRAILER-FILE.                  
01592                                                                   
01593      PERFORM 3000-READ-FOR-UPDATE.                                
01594                                                                   
01595      IF BPAYEEL GREATER ZERO                                      
01596          IF BPAYEEI EQUAL 'I'                                     
01597              MOVE 'I1'           TO  AT-PAYEE-TYPE-CD             
01598          ELSE                                                     
01599          IF BPAYEEI EQUAL 'B'                                     
01600              MOVE 'B1'           TO  AT-PAYEE-TYPE-CD             
01601          ELSE                                                     
01602          IF BPAYEEI EQUAL 'A'                                     
01603              MOVE 'A1'           TO  AT-PAYEE-TYPE-CD             
01604          ELSE                                                     
01605          IF BPAYEEI EQUAL 'O'                                     
01606              MOVE 'O1'           TO  AT-PAYEE-TYPE-CD             
01607          ELSE                                                     
01608          IF BPAYEEI EQUAL 'Q' OR 'REM BORR'                       
01609              MOVE 'Q1'           TO  AT-PAYEE-TYPE-CD             
01610            ELSE                                                   
01611          IF BPAYEEI = 'P' OR 'D'                                  
01612              MOVE 'P1'           TO  AT-PAYEE-TYPE-CD             
01613            ELSE                                                   
01614          IF BPAYEEI EQUAL 'E'                                     
01615              MOVE 'E1'           TO  AT-PAYEE-TYPE-CD. 
052506
052506     IF BPRFDTL GREATER ZERO
052506         MOVE WS-PRF-DT          TO  AT-PMT-PROOF-DT.           
01616                                                                   
01617      IF BCKNOL GREATER ZERO                                       
01618          MOVE BCKNOI             TO  AT-CHECK-NO.                 
01619                                                                   
01620      IF BCRSELL GREATER ZERO                                      
01621         MOVE WS-CRSEL            TO  AT-PMT-SELECT-DT.            
01622                                                                   
01623      IF BVOIDSDL GREATER ZERO                                     
01624         MOVE WS-VOIDSD           TO  AT-VOID-SELECT-DT.           
01625                                                                   
01626      IF BCRACPL GREATER ZERO                                      
01627         MOVE PI-AFTER-DATE-2     TO  AT-PMT-ACCEPT-DT.            
01628                                                                   
           IF BEOBYNL > 0
              MOVE BEOBYNI             TO AT-PRINT-EOB-WITH-CHECK
           END-IF

020413     IF BCLMYNL > 0
020413        MOVE BCLMYNI             TO AT-PRINT-CLM-FORM
020413     END-IF
020413
020413     IF BSRVYNL > 0
020413        MOVE BSRVYNI             TO AT-PRINT-SURVEY
020413     END-IF
020413
102413     IF BSPRELL > 0
102413        MOVE BSPRELI             TO AT-SPECIAL-RELEASE
102413     END-IF
102413
01629      IF BVOIDACL GREATER ZERO                                     
01630         MOVE PI-AFTER-DATE-3     TO  AT-VOID-ACCEPT-DT.           
01631                                                                   
01632      IF BEXPTYPL GREATER ZERO                                     
01633          MOVE BEXPTYPI           TO  AT-EXPENSE-TYPE.             
01634                                                                   
01635      IF BHOLDATL GREATER ZERO                                     
01636          MOVE PI-HOLD-UNTIL-DATE TO  AT-TO-BE-WRITTEN-DT.         
01637                                                                   
01638      IF BCKQUEL GREATER ZERO                                      
01639          MOVE WS-CHECK-QUE       TO  AT-CHECK-QUE-CONTROL.        
01640                                                                   
01641      IF BCKSEQL GREATER ZERO                                      
01642          MOVE WS-CHECK-QUE-SEQ   TO  AT-CHECK-QUE-SEQUENCE.       
01643                                                                   
01644      IF BPMTORGL GREATER ZERO                                     
01645          MOVE BPMTORGI           TO  AT-PAYMENT-ORIGIN.           
01646                                                                   
01647      MOVE PI-PROCESSOR-ID        TO  AT-PAYMENT-LAST-UPDATED-BY.  
01648                                                                   
01649      MOVE WS-CURRENT-DATE        TO  AT-PAYMENT-LAST-MAINT-DT.    
01650                                                                   
01651      PERFORM 3100-REWRITE.                                        
01652      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.     
01653                                                                   
01654      MOVE 'S'                    TO  BMAINTO.                     
01655      MOVE -1                     TO  BMAINTL.                     
01656      MOVE AL-UANOF               TO  BMAINTA.                     
01657                                                                   
01658      IF BNOTE1L IS GREATER THAN 0 OR                              
01659         BNOTE2L IS GREATER THAN 0                                 
01660              GO TO 0200-UPDATE-PMNT-NOTE-TRLR                     
01661           ELSE                                                    
01662              PERFORM 8200-SEND-DATAONLY.                          
01663                                                                   
01664  0200-UPDATE-PMNT-NOTE-TRLR.                                      
01665                                                                   
01666      EXEC CICS READ                                               
01667          DATASET   (WS-ACTIVITY-TRAILERS-DSID)                    
01668          RIDFLD    (PI-SAVE-KEY)                                  
01669          SET       (ADDRESS OF ACTIVITY-TRAILERS)                 
01670      END-EXEC.                                                    
01671                                                                   
01672      IF AT-PAYMENT-NOTE-SEQ-NO = 0                                
01673          GO TO 0200-ADD-PMNT-NOTE-TRLR.                           
01674                                                                   
01675      MOVE PI-SAVE-KEY            TO  WS-ACTIVITY-TRAILERS-KEY.    
01676      MOVE AT-PAYMENT-NOTE-SEQ-NO TO  WS-ATK-SEQUENCE-NO.          
01677                                                                   
01678      EXEC CICS HANDLE CONDITION                                   
01679          NOTFND   (0200-ADD-PMNT-NOTE-TRLR)                       
01680      END-EXEC.                                                    
01681                                                                   
01682      EXEC CICS READ                                               
01683          DATASET   (WS-ACTIVITY-TRAILERS-DSID)                    
01684          RIDFLD    (WS-ACTIVITY-TRAILERS-KEY)                     
01685          SET       (ADDRESS OF ACTIVITY-TRAILERS)                 
01686          UPDATE                                                   
01687      END-EXEC.                                                    
01688                                                                   
01689      IF BNOTE1L IS GREATER THAN 0                                 
01690          MOVE BNOTE1I            TO  AT-INFO-LINE-1.              
01691                                                                   
01692      IF PI-COMPANY-ID = 'HAN'  OR  'JHL'                          
01693        IF BNOTE1L IS GREATER THAN 0                               
01694          MOVE BNOTE1I            TO  WS-HAN-PAYMENT-NOTE          
01695          IF WS-HAN-PMT-CODE = SPACE                               
01696              MOVE SPACES         TO  WS-HAN-PMT-TEXT              
01697          ELSE                                                     
01698          IF WS-HAN-PMT-CODE = 'A'                                 
01699              MOVE ':FINAL PAYMENT / MAXIMUM BENEFIT-PAID'         
01700                                  TO  WS-HAN-PMT-TEXT              
01701          ELSE                                                     
01702          IF WS-HAN-PMT-CODE = 'B'                                 
01703              MOVE ':FINAL PAYMENT / DECEASED'                     
01704                                  TO  WS-HAN-PMT-TEXT              
01705          ELSE                                                     
01706          IF WS-HAN-PMT-CODE = 'C'                                 
01707              MOVE ':FINAL PAYMENT / NO LONGER DISABLED'           
01708                                  TO  WS-HAN-PMT-TEXT              
01709          ELSE                                                     
01710          IF WS-HAN-PMT-CODE = 'D'                                 
01711              MOVE ':FINAL PAYMENT / ADDL INFO NOT PROVIDED'       
01712                                  TO  WS-HAN-PMT-TEXT              
01713          ELSE                                                     
01714          IF WS-HAN-PMT-CODE = 'E'                                 
01715              MOVE ':FINAL PAYMENT / RETURNED TO WORK'             
01716                                  TO  WS-HAN-PMT-TEXT              
01717          ELSE                                                     
01718          IF WS-HAN-PMT-CODE = 'F'                                 
01719              MOVE ':FINAL PAYMENT / PAID TO MATURITY DATE'        
01720                                  TO  WS-HAN-PMT-TEXT              
01721          ELSE                                                     
01722          IF WS-HAN-PMT-CODE = 'P'                                 
01723              MOVE ':PARTIAL PAYMENT' TO  WS-HAN-PMT-TEXT.         
01724                                                                   
01725      IF PI-COMPANY-ID = 'HAN'  OR  'JHL'                          
01726        IF BNOTE1L IS GREATER THAN 0                               
01727          MOVE WS-HAN-PAYMENT-NOTE  TO  BNOTE1I                    
01728                                        AT-INFO-LINE-1.            
01729                                                                   
01730      IF BNOTE2L IS GREATER THAN 0                                 
01731          MOVE BNOTE2I            TO  AT-INFO-LINE-2.              
01732                                                                   
01733      MOVE PI-PROCESSOR-ID        TO  AT-PAYMENT-LAST-UPDATED-BY.  
01734                                                                   
01735      MOVE WS-CURRENT-DATE        TO  AT-PAYMENT-LAST-MAINT-DT.    
01736                                                                   
01737      PERFORM 3100-REWRITE.                                        
01738                                                                   
01739      MOVE 'S'                    TO  BMAINTO.                     
01740      MOVE -1                     TO  BMAINTL.                     
01741      MOVE AL-UANOF               TO  BMAINTA.                     
01742                                                                   
01743      PERFORM 8200-SEND-DATAONLY.                                  
01744                                                                   
01745  0200-ADD-PMNT-NOTE-TRLR.                                         
01746                                                                   
01747      PERFORM 3500-READ-ELMSTR-FOR-UPDATE.                         
01748                                                                   
01749      PERFORM 3000-READ-FOR-UPDATE.                                
01750                                                                   
01751      MOVE CL-TRAILER-SEQ-CNT     TO  AT-PAYMENT-NOTE-SEQ-NO.      
01752      SUBTRACT +1 FROM AT-PAYMENT-NOTE-SEQ-NO.                     
01753                                                                   
01754      EXEC CICS REWRITE                                            
01755          DATASET   (WS-ACTIVITY-TRAILERS-DSID)                    
01756          FROM      (ACTIVITY-TRAILERS)                            
01757      END-EXEC.                                                    
01758                                                                   
01759      EXEC CICS GETMAIN                                            
01760          LENGTH  (WS-ACTIVITY-TRAILERS-LENGTH)                    
01761          INITIMG (WS-SPACES)                                      
01762          SET     (ADDRESS OF ACTIVITY-TRAILERS)                   
01763      END-EXEC.                                                    
01764                                                                   
01765      MOVE 'AT'                   TO  AT-RECORD-ID.                
01766      MOVE PI-SAVE-KEY            TO  AT-CONTROL-PRIMARY.          
01767      MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.              
01768      SUBTRACT +1 FROM AT-SEQUENCE-NO.                             
01769                                                                   
01770      MOVE '6'                    TO  AT-TRAILER-TYPE.             
01771                                                                   
01772      MOVE WS-CURRENT-DATE        TO  AT-RECORDED-DT               
01773                                      AT-PAYMENT-LAST-MAINT-DT.    
01774      MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY               
01775                                      AT-PAYMENT-LAST-UPDATED-BY.  
01776      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.        
01777                                                                   
01778      MOVE SPACES                 TO  WS-HAN-PAYMENT-NOTE.         
01779                                                                   
01780      IF BNOTE1L IS GREATER THAN 0                                 
01781          MOVE BNOTE1I            TO  AT-INFO-LINE-1               
01782                                      WS-HAN-PAYMENT-NOTE.         
01783                                                                   
01784      IF PI-COMPANY-ID = 'HAN'  OR  'JHL'                          
01785          IF WS-HAN-PMT-CODE = 'A'                                 
01786              MOVE ':FINAL PAYMENT / MAXIMUM BENEFIT-PAID'         
01787                                  TO  WS-HAN-PMT-TEXT              
01788          ELSE                                                     
01789          IF WS-HAN-PMT-CODE = 'B'                                 
01790              MOVE ':FINAL PAYMENT / DECEASED'                     
01791                                  TO  WS-HAN-PMT-TEXT              
01792          ELSE                                                     
01793          IF WS-HAN-PMT-CODE = 'C'                                 
01794              MOVE ':FINAL PAYMENT / NO LONGER DISABLED'           
01795                                  TO  WS-HAN-PMT-TEXT              
01796          ELSE                                                     
01797          IF WS-HAN-PMT-CODE = 'D'                                 
01798              MOVE ':FINAL PAYMENT / ADDL INFO NOT PROVIDED'       
01799                                  TO  WS-HAN-PMT-TEXT              
01800          ELSE                                                     
01801          IF WS-HAN-PMT-CODE = 'E'                                 
01802              MOVE ':FINAL PAYMENT / RETURNED TO WORK'             
01803                                  TO  WS-HAN-PMT-TEXT              
01804          ELSE                                                     
01805          IF WS-HAN-PMT-CODE = 'F'                                 
01806              MOVE ':FINAL PAYMENT / PAID TO MATURITY DATE'        
01807                                  TO  WS-HAN-PMT-TEXT              
01808          ELSE                                                     
01809          IF WS-HAN-PMT-CODE = 'P'                                 
01810              MOVE ':PARTIAL PAYMENT' TO  WS-HAN-PMT-TEXT.         
01811                                                                   
01812      IF PI-COMPANY-ID = 'HAN'  OR  'JHL'                          
01813          MOVE WS-HAN-PAYMENT-NOTE  TO  BNOTE1I                    
01814                                        AT-INFO-LINE-1.            
01815                                                                   
01816      IF BNOTE2L IS GREATER THAN 0                                 
01817          MOVE BNOTE2I            TO  AT-INFO-LINE-2.              
01818                                                                   
01819      MOVE 'P'                    TO  AT-INFO-TRAILER-TYPE.        
01820                                                                   
01821      EXEC CICS WRITE                                              
01822          DATASET   (WS-ACTIVITY-TRAILERS-DSID)                    
01823          FROM      (ACTIVITY-TRAILERS)                            
01824          RIDFLD    (AT-CONTROL-PRIMARY)                           
01825      END-EXEC.                                                    
01826                                                                   
01827      SUBTRACT +1 FROM  CL-TRAILER-SEQ-CNT.                        
01828                                                                   
01829      PERFORM 3600-REWRITE-ELMSTR.                                 
01830                                                                   
01831      MOVE 'S'                    TO  BMAINTO.                     
01832      MOVE -1                     TO  BMAINTL.                     
01833      MOVE AL-UANOF               TO  BMAINTA.                     
01834                                                                   
01835      PERFORM 8200-SEND-DATAONLY.                                  
01836                                                                   
01837      EJECT                                                        
01838  0300-MAIN-LOGIC.                                                 
01839      IF PI-MAP-NAME NOT = EL142C                                  
01840          GO TO 0400-MAIN-LOGIC.                                   
01841                                                                   
01842      PERFORM 4000-READ-TRAILER-FILE.                              
01843                                                                   
01844      EJECT                                                        
01845  0400-MAIN-LOGIC.                                                 
01846      IF PI-MAP-NAME NOT = EL142D                                  
01847          GO TO 0500-MAIN-LOGIC.                                   
01848                                                                   
01849      IF NOT MODIFY-CAP                                            
01850          IF DMAINTI = 'S'                                         
01851              NEXT SENTENCE                                        
01852          ELSE                                                     
01853              MOVE 'UPDATE'       TO SM-READ                       
01854              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       
01855              MOVE ER-0070        TO  EMI-ERROR                    
01856              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
01857              GO TO 8100-SEND-INITIAL-MAP.                         
01858                                                                   
01859      IF EIBAID = DFHPF6                                           
102510         PERFORM 6000-DISPLAY-ELNAPS.                             
01861                                                                   
01862      MOVE PI-COMPANY-ID          TO CNTL-CO.                      
01863      MOVE '1'                    TO CNTL-RECORD-TYPE.             
01864      MOVE SPACES                 TO CNTL-GENL.                    
01865      MOVE ZEROS                  TO CNTL-SEQ.                     
01866                                                                   
01867      EXEC CICS READ                                               
01868          DATASET(WS-CONTROL-FILE-DSID)                            
01869          SET    (ADDRESS OF CONTROL-FILE)                         
01870          RIDFLD (WS-CONTROL-FILE-KEY)                             
01871      END-EXEC.                                                    
01872                                                                   
01873      MOVE +2                     TO  EMI-NUMBER-OF-LINES          
01874                                      EMI-SWITCH2.                 
01875                                                                   
01876      IF DMAINTL NOT GREATER ZERO                                  
01877        OR (DMAINTL GREATER ZERO AND                               
01878            DMAINTI = 'S')                                         
01879              PERFORM 4000-READ-TRAILER-FILE.                      
01880                                                                   
01881      IF DMAINTI NOT = 'C'                                         
01882          MOVE ER-0023            TO  EMI-ERROR                    
01883          MOVE -1                 TO  DMAINTL                      
01884          MOVE AL-UABON           TO  DMAINTA                      
01885          PERFORM 8200-SEND-DATAONLY.                              
01886                                                                   
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO dMAINTL
062602           MOVE AL-UABON         TO dMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
01887      MOVE AL-UANON               TO  DMAINTA                      
01888                                                                   
01889 *    NOTE ******************************************************* 
01890 *         *                                                     * 
01891 *         *      THE FOLLOWING CHECK ALLOWS THE LGXX SIGNON     * 
01892 *         *  TO UPDATE THE ARCHIVE NUMBER, THE DATE SENT FIELD, * 
01893 *         *  THE INITIAL PRINT DATE FIELD, AND THE RESEND PRINT * 
01894 *         *  DATE FIELD.  IF NOT LGXX SIGNON, THE FIELDS ARE    * 
01895 *         *  SET TO ASKIP.                                      * 
01896 *         *                                                     * 
01897 *         *******************************************************.
01898                                                                   
01899      IF PI-PROCESSOR-ID = 'LGXX'                                  
01900          PERFORM 0450-MAIN-LOGIC                                  
01901      ELSE                                                         
01902          MOVE AL-SANOF           TO  DARCHNOA                     
050110                                     DRESFRMA
050110                                     DAUTOCLA
01903                                      DDTSENTA                     
01904                                      DINPRNTA                     
01905                                      DREPRNTA.  
01906                                                                   
01907      IF DFORMNOL GREATER ZERO                                     
01908         MOVE DFORMNOI            TO WS-FORM-NUMBER                
01909         MOVE +1                  TO WS-UPDATE-SW.                 
01910                                                                   
01911      IF DRESENDL GREATER ZERO                                     
01912          IF DRESENDI = SPACES                                     
01913              MOVE AL-UANON       TO  DRESENDA                     
01914              MOVE +1             TO  WS-UPDATE-SW                 
01915              MOVE LOW-VALUES     TO  WS-RESEND-DATE               
01916          ELSE                                                     
01917              MOVE DRESENDI       TO  WS-DEEDIT-FIELD              
01918              PERFORM 8600-DEEDIT                                  
01919              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
01920                  MOVE WS-DEEDIT-FIELD-V0  TO  DRESENDO            
01921                  INSPECT DRESENDI CONVERTING SPACES TO SLASH      
01922                  MOVE '4'        TO  DC-OPTION-CODE               
01923                  MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY  
01924                  PERFORM 8500-DATE-CONVERSION                     
01925                  IF DC-ERROR-CODE NOT = SPACES                    
01926                      MOVE ER-0295         TO  EMI-ERROR           
01927                      MOVE -1              TO  DRESENDL            
01928                      MOVE AL-UABON        TO  DRESENDA            
01929                      PERFORM 9900-ERROR-FORMAT                    
01930                  ELSE                                             
01931                      MOVE AL-UANON       TO  DRESENDA             
01932                      MOVE +1             TO  WS-UPDATE-SW         
01933                      MOVE DC-BIN-DATE-1  TO  WS-RESEND-DATE       
01934              ELSE                                                 
01935                  MOVE ER-0295    TO  EMI-ERROR                    
01936                  MOVE -1         TO  DRESENDL                     
01937                  MOVE AL-UABON   TO  DRESENDA                     
01938                  PERFORM 9900-ERROR-FORMAT.                       
01939                                                                   
01940      IF DREPLYL GREATER ZERO                                      
01941          IF DREPLYI = SPACES                                      
01942              MOVE AL-UANON       TO  DREPLYA                      
01943              MOVE +1             TO  WS-UPDATE-SW                 
01944              MOVE LOW-VALUES     TO  WS-FOLLOW-UP-DATE            
01945          ELSE                                                     
01946              MOVE DREPLYI        TO  WS-DEEDIT-FIELD              
01947              PERFORM 8600-DEEDIT                                  
01948              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
01949                  MOVE WS-DEEDIT-FIELD-V0  TO  DREPLYO             
01950                  INSPECT DREPLYI CONVERTING SPACES TO SLASH       
01951                  MOVE '4'                 TO  DC-OPTION-CODE      
01952                  MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY  
01953                  PERFORM 8500-DATE-CONVERSION                     
01954                  IF DC-ERROR-CODE NOT = SPACES                    
01955                      MOVE ER-0296           TO  EMI-ERROR         
01956                      MOVE -1             TO  DREPLYL              
01957                      MOVE AL-UABON       TO  DREPLYA              
01958                      PERFORM 9900-ERROR-FORMAT                    
01959                  ELSE                                             
01960                      MOVE AL-UANON       TO  DREPLYA              
01961                      MOVE +1             TO  WS-UPDATE-SW         
01962                      MOVE DC-BIN-DATE-1  TO  WS-FOLLOW-UP-DATE    
01963              ELSE                                                 
01964                  MOVE ER-0296    TO  EMI-ERROR                    
01965                  MOVE -1         TO  DREPLYL                      
01966                  MOVE AL-UABON   TO  DREPLYA                      
01967                  PERFORM 9900-ERROR-FORMAT.                       
01968                                                                   
01969      IF DRECEVEL GREATER ZERO                                     
01970          IF DRECEVEI = SPACES                                     
01971              MOVE AL-UANON       TO  DRECEVEA                     
01972              MOVE +1             TO  WS-UPDATE-SW                 
01973              MOVE LOW-VALUES     TO  WS-RECEIVED-DATE             
01974          ELSE                                                     
01975              MOVE DRECEVEI       TO  WS-DEEDIT-FIELD              
01976              PERFORM 8600-DEEDIT                                  
01977              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
01978                  MOVE WS-DEEDIT-FIELD-V0  TO  DRECEVEO            
01979                  INSPECT DRECEVEI CONVERTING SPACES TO SLASH      
01980                  MOVE '4'        TO  DC-OPTION-CODE               
01981                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY   
01982                  PERFORM 8500-DATE-CONVERSION                     
01983                  IF DC-ERROR-CODE NOT = SPACES                    
01984                      MOVE ER-0297        TO  EMI-ERROR            
01985                      MOVE -1             TO  DRECEVEL             
01986                      MOVE AL-UABON       TO  DRECEVEA             
01987                      PERFORM 9900-ERROR-FORMAT                    
01988                  ELSE                                             
01989                      MOVE AL-UANON       TO  DRECEVEA             
01990                      MOVE +1             TO  WS-UPDATE-SW         
01991                      MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-DATE     
01992              ELSE                                                 
01993                  MOVE ER-0297    TO  EMI-ERROR                    
01994                  MOVE -1         TO  DRECEVEL                     
01995                  MOVE AL-UABON   TO  DRECEVEA                     
01996                  PERFORM 9900-ERROR-FORMAT.                       
102610                                                                  
102610     IF DSTOPLTL GREATER ZERO                                     
102610         IF DSTOPLTI = SPACES                                     
102610             MOVE AL-UANON       TO  DSTOPLTA                     
102610             MOVE +1             TO  WS-UPDATE-SW                 
102610             MOVE LOW-VALUES     TO  WS-STOP-LETTER-DATE               
102610         ELSE                                                     
102610             MOVE DSTOPLTI       TO  WS-DEEDIT-FIELD              
102610             PERFORM 8600-DEEDIT                                  
102610             IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
102610                 MOVE WS-DEEDIT-FIELD-V0  TO  DSTOPLTO            
102610                 INSPECT DSTOPLTI CONVERTING SPACES TO SLASH      
102610                 MOVE '4'        TO  DC-OPTION-CODE               
102610                 MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY  
102610                 PERFORM 8500-DATE-CONVERSION                     
102610                 IF DC-ERROR-CODE NOT = SPACES                    
102610                     MOVE ER-0897         TO  EMI-ERROR           
102610                     MOVE -1              TO  DSTOPLTL            
102610                     MOVE AL-UABON        TO  DSTOPLTA            
102610                     PERFORM 9900-ERROR-FORMAT                    
102610                 ELSE                                             
102610                     MOVE AL-UANON       TO  DSTOPLTA             
102610                     MOVE +1             TO  WS-UPDATE-SW         
102610                     MOVE DC-BIN-DATE-1  TO  WS-STOP-LETTER-DATE       
102610             ELSE                                                 
102610                 MOVE ER-0897    TO  EMI-ERROR                    
102610                 MOVE -1         TO  DSTOPLTL                     
102610                 MOVE AL-UABON   TO  DSTOPLTA                     
102610                 PERFORM 9900-ERROR-FORMAT.                       
01997                                                                   
01998      IF DREASONL GREATER ZERO                                     
01999          MOVE +1                 TO  WS-UPDATE-SW.                
041613
041613     IF DENCCODL GREATER THAN ZERO
041613        IF PI-CREATED-IN-NAPERSOFT = 'Y'
041613            MOVE DENCCODI TO WS-TEMP-ENCCODE
041613            IF WS-TEMP-ENCCODE (3:1) = 'X'
102413                IF WS-TEMP-ENCCODE (1:2) = 'EN'
102413                   MOVE 'V' TO WS-TEMP-ENCCODE (3:1)
102413                ELSE
102413                   MOVE SPACES TO WS-TEMP-ENCCODE (3:1)
102413                END-IF
041613            END-IF
041613            IF WS-TEMP-ENCCODE (2:1) = 'X'
041613                MOVE SPACES TO WS-TEMP-ENCCODE (2:1)
041613            END-IF
041613            IF WS-TEMP-ENCCODE <> PI-ENC-CODE
041613               MOVE ER-1568          TO EMI-ERROR
041613               MOVE -1               TO DENCCODL
041613               MOVE AL-UABON         TO DENCCODA
041613               PERFORM 9900-ERROR-FORMAT
041613            END-IF
041613        END-IF
041613        MOVE SPACES             TO WS-ELENCC-KEY
041613        MOVE PI-COMPANY-CD      TO WS-ELENCC-COMPANY-CD
041613        MOVE '1'                TO WS-ELENCC-REC-TYPE
041613        MOVE DENCCODI           TO WS-ELENCC-ENC-CODE
041613
041613        EXEC CICS READ
041613            DATASET    (WS-ELENCC-FILE-DSID)
041613            SET        (ADDRESS OF ENCLOSURE-CODES)
041613            RIDFLD     (WS-ELENCC-KEY)
041613            RESP       (WS-RESPONSE)
041613        END-EXEC
041613
041613        IF RESP-NORMAL
041613           MOVE DENCCODI         TO WS-ENCLOSURE-CODE
041613           MOVE AL-UANON         TO DENCCODA
041613           MOVE +1               TO WS-UPDATE-SW
041613        ELSE
041613           MOVE ER-1560          TO EMI-ERROR
041613           MOVE -1               TO DENCCODL
041613           MOVE AL-UABON         TO DENCCODA
041613           PERFORM 9900-ERROR-FORMAT
041613        END-IF
041613     END-IF
041613
02000                                                                   
02001      IF WS-ERROR-COUNT GREATER ZERO                               
02002          PERFORM 8200-SEND-DATAONLY.                              
02003                                                                   
02004      IF WS-UPDATE-SW NOT GREATER ZERO                             
02005          PERFORM 4000-READ-TRAILER-FILE.                          
02006                                                                   
02007      IF DREPLYL GREATER ZERO                                      
02008          PERFORM 3300-UPDATE-CLAIM-MASTER.                        
02009                                                                   
02010      PERFORM 3000-READ-FOR-UPDATE.                                
02011                                                                   
02012      IF DRESENDL GREATER ZERO AND                                 
02013         AT-LETTER-ARCHIVE-NO LESS THAN CF-STARTING-ARCH-NO        
02014           EXEC CICS READ                                          
02015               UPDATE                                              
02016               DATASET (WS-CONTROL-FILE-DSID)                      
02017               SET     (ADDRESS OF CONTROL-FILE)                   
02018               RIDFLD  (WS-CONTROL-FILE-KEY)                       
02019           END-EXEC                                                
02020           MOVE AT-LETTER-ARCHIVE-NO TO CF-STARTING-ARCH-NO        
02021           EXEC CICS REWRITE                                       
02022               DATASET (WS-CONTROL-FILE-DSID)                      
02023               FROM    (CONTROL-FILE)                              
02024           END-EXEC.                                               
02025                                                                   
02026      IF DRESENDL GREATER ZERO                                   
02027          MOVE WS-RESEND-DATE     TO  AT-AUTO-RE-SEND-DT           
102510     END-IF.
102510*02028          MOVE LOW-VALUES         TO  AT-RESEND-PRINT-DATE         
102510*02029          MOVE PI-COMPANY-CD      TO  WS-LA-COMPANY-CD             
102510*02030          MOVE AT-LETTER-ARCHIVE-NO  TO  WS-LA-ARCHIVE-NO          
102510*02031          MOVE '1'                TO  WS-LA-RECORD-TYPE            
102510*02032          MOVE ZERO               TO  WS-LA-LINE-SEQ-NO            
102510*02033          EXEC CICS READ UPDATE                                    
102510*02034              DATASET (WS-LETTER-ARCHIVE-DSID)                     
102510*02035              RIDFLD  (WS-LETTER-ARCHIVE-KEY)                      
102510*02036              SET     (ADDRESS OF LETTER-ARCHIVE)                  
102510*02037          END-EXEC                                                 
102510*02038          MOVE WS-RESEND-DATE     TO  LA-RESEND-DATE               
102510*02039          MOVE LOW-VALUES         TO  LA-RESEND-PRINT-DATE         
102510*02040          EXEC CICS REWRITE                                        
102510*02041              DATASET (WS-LETTER-ARCHIVE-DSID)                     
102510*02042              FROM    (LETTER-ARCHIVE)                             
102510*02043          END-EXEC                                                 
102510*02044          IF PI-COMPANY-ID = 'DMD'                                 
102510*02045              EXEC CICS READ UPDATE                                
102510*02046                  DATASET (WS-ELARCT-FILE-ID)                      
102510*02047                  RIDFLD  (WS-LETTER-ARCHIVE-KEY)                  
102510*02048                  SET     (ADDRESS OF LETTER-ARCHIVE-TEMP)         
102510*02049              END-EXEC                                             
102510*02050              MOVE WS-RESEND-DATE     TO  LT-RESEND-DATE           
102510*02051              MOVE LOW-VALUES         TO  LT-RESEND-PRINT-DATE     
102510*02052              EXEC CICS REWRITE                                    
102510*02053                  DATASET (WS-ELARCT-FILE-ID)                      
102510*02054                  FROM    (LETTER-ARCHIVE-TEMP)                    
102510*02055              END-EXEC.                                            
02056                                                                   
02057      IF DREPLYL GREATER ZERO                                      
02058          MOVE WS-FOLLOW-UP-DATE  TO  AT-RECEIPT-FOLLOW-UP.        
102510
102510     IF DRESENDL GREATER ZERO OR DREPLYL GREATER ZERO
041613        OR DENCCODL GREATER ZERO
102510         MOVE PI-COMPANY-CD      TO  WS-NA-COMPANY-CD
102510         MOVE PI-CARRIER         TO  WS-NA-CARRIER
102510         MOVE PI-CLAIM-NO        TO  WS-NA-CLAIM-NO
102510         MOVE PI-CERT-NO         TO  WS-NA-CERT-NO
102510         MOVE AT-LETTER-ARCHIVE-NO  TO  WS-NA-ARCHIVE-NO
102510         EXEC CICS READ UPDATE
102510             DATASET (WS-NAPERSOFT-DSID)
102510             RIDFLD  (WS-NAPERSOFT-KEY)
102510             SET     (ADDRESS OF NAPERSOFT-FILE)
102510         END-EXEC
102510         IF DRESENDL GREATER ZERO
102510             MOVE WS-RESEND-DATE    TO  NA-RESEND-DT
102510         END-IF
102510         IF DREPLYL GREATER ZERO
102510             MOVE WS-FOLLOW-UP-DATE TO  NA-FOLLOW-UP-DT
102510         END-IF
041613         IF DENCCODL GREATER ZERO
041613             MOVE WS-ENCLOSURE-CODE TO  NA-ENCLOSURE-CD
041613         END-IF
102510         EXEC CICS REWRITE
102510             DATASET (WS-NAPERSOFT-DSID)
102510             FROM    (NAPERSOFT-FILE)
102510         END-EXEC
102510     END-IF.
02059                                                                   
02060      IF DRECEVEL GREATER ZERO                                     
02061          MOVE WS-RECEIVED-DATE   TO  AT-LETTER-ANSWERED-DT.       
02062                                                                   
02063      IF DFORMNOL GREATER ZEROS                                    
02064         MOVE WS-FORM-NUMBER      TO AT-STD-LETTER-FORM.           
02065                                                                   
02066      IF DARCHNOL GREATER ZERO                                     
02067         MOVE DARCHNOI            TO  AT-LETTER-ARCHIVE-NO.        
02068                                                                   
02069      IF DDTSENTL GREATER ZERO                                     
02070         MOVE WS-DATE-SENT        TO  AT-LETTER-SENT-DT.           
02071                                                                   
02072      IF DINPRNTL GREATER ZERO                                     
02073         MOVE WS-IN-PRINT-DATE    TO  AT-INITIAL-PRINT-DATE.       
02074                                                                   
02075      IF DREPRNTL GREATER ZERO                                     
02076         MOVE WS-REPRINTED-DATE   TO  AT-RESEND-PRINT-DATE.        
050110
050110     IF DRESFRML GREATER ZERO
050110        MOVE DRESFRMI            TO  AT-RESEND-LETTER-FORM.
050110
050110     IF DAUTOCLL GREATER ZERO
050110        MOVE DAUTOCLI            TO  AT-AUTO-CLOSE-IND.
102610
102610     IF DSTOPLTL GREATER ZERO
102610        MOVE WS-STOP-LETTER-DATE TO  AT-STOP-LETTER-DT
102610     END-IF.
02077                                                                   
02078      IF DREASONL GREATER ZERO                                     
02079         MOVE DREASONI            TO  AT-REASON-TEXT.              
02080                                                                   
02081      IF PI-COMPANY-ID = 'HAN'  OR  'JHL'                          
02082          MOVE AT-REASON-TEXT     TO  WS-REASON-TEXT               
02083          IF WS-RE-NDX NUMERIC  AND                                
02084             WS-RE-NDX GREATER THAN ZERO  AND                      
02085             WS-RE-NDX LESS THAN 24                                
02086              MOVE HAN-REASON-TEXT (WS-RE-NDX)                     
02087                                  TO  AT-REASON-TEXT               
02088                                      DREASONO.                    
02089                                                                   
02090      MOVE PI-PROCESSOR-ID        TO  AT-CORR-LAST-UPDATED-BY.     
02091                                                                   
02092      MOVE WS-CURRENT-DATE        TO  AT-CORR-LAST-MAINT-DT.       
02093                                                                   
02094      PERFORM 3100-REWRITE.                                        
02095      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.     
02096                                                                   
02097      MOVE 'S'                    TO  DMAINTO.                     
02098      MOVE -1                     TO  DMAINTL.                     
02099      MOVE AL-UANOF               TO  DMAINTA.                     
02100      PERFORM 8200-SEND-DATAONLY.                                  
02101                                                                   
02102      EJECT                                                        
02103                                                                   
02104  0450-MAIN-LOGIC.                                                 
02105      IF DARCHNOL GREATER ZERO                                     
02106          MOVE DARCHNOI           TO  WS-DEEDIT-FIELD              
02107          PERFORM 8600-DEEDIT                                      
02108          IF WS-DEEDIT-FIELD-V0 IS NUMERIC                         
02109              MOVE WS-DEEDIT-FIELD-V0     TO  DARCHNOO             
02110              MOVE AL-UNNON       TO  DARCHNOA                     
02111              MOVE +1             TO  WS-UPDATE-SW                 
02112          ELSE                                                     
02113              MOVE -1             TO  DARCHNOL                     
02114              MOVE AL-UNBON       TO  DARCHNOA                     
02115              MOVE ER-0175        TO  EMI-ERROR                    
02116              PERFORM 9900-ERROR-FORMAT.                           
02117                                                                   
02118      IF DDTSENTL GREATER ZERO                                     
02119          IF DDTSENTI = SPACES                                     
02120              MOVE AL-UANON       TO  DDTSENTA                     
02121              MOVE +1             TO  WS-UPDATE-SW                 
02122              MOVE LOW-VALUES     TO  WS-DATE-SENT                 
02123          ELSE                                                     
02124              MOVE DDTSENTI       TO  WS-DEEDIT-FIELD              
02125              PERFORM 8600-DEEDIT                                  
02126              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
02127                  MOVE WS-DEEDIT-FIELD-V0    TO  DDTSENTO          
02128                  INSPECT DDTSENTI CONVERTING SPACES TO SLASH      
02129                  MOVE '4'                   TO  DC-OPTION-CODE    
02130                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY   
02131                  PERFORM 8500-DATE-CONVERSION                     
02132                  IF DC-ERROR-CODE NOT = SPACES                    
02133                      MOVE ER-0641           TO  EMI-ERROR         
02134                      MOVE -1                TO  DDTSENTL          
02135                      MOVE AL-UABON          TO  DDTSENTA          
02136                      PERFORM 9900-ERROR-FORMAT                    
02137                  ELSE                                             
02138                      MOVE AL-UANON       TO  DDTSENTA             
02139                      MOVE +1             TO  WS-UPDATE-SW         
02140                      MOVE DC-BIN-DATE-1  TO  WS-DATE-SENT         
02141              ELSE                                                 
02142                  MOVE ER-0641        TO  EMI-ERROR                
02143                  MOVE -1             TO  DDTSENTL                 
02144                  MOVE AL-UABON       TO  DDTSENTA                 
02145                  PERFORM 9900-ERROR-FORMAT.                       
02146                                                                   
02147      IF DINPRNTL GREATER ZERO                                     
02148          IF DINPRNTI = SPACES                                     
02149              MOVE AL-UANON       TO  DINPRNTA                     
02150              MOVE +1             TO  WS-UPDATE-SW                 
02151              MOVE LOW-VALUES     TO  WS-IN-PRINT-DATE             
02152          ELSE                                                     
02153              MOVE DINPRNTI       TO  WS-DEEDIT-FIELD              
02154              PERFORM 8600-DEEDIT                                  
02155              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
02156                  MOVE WS-DEEDIT-FIELD-V0  TO  DINPRNTO            
02157                  INSPECT DINPRNTI CONVERTING SPACES TO SLASH      
02158                  MOVE '4'                 TO  DC-OPTION-CODE      
02159                  MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY  
02160                  PERFORM 8500-DATE-CONVERSION                     
02161                  IF DC-ERROR-CODE NOT = SPACES                    
02162                      MOVE ER-0642           TO  EMI-ERROR         
02163                      MOVE -1                TO  DINPRNTL          
02164                      MOVE AL-UABON          TO  DINPRNTA          
02165                      PERFORM 9900-ERROR-FORMAT                    
02166                  ELSE                                             
02167                      MOVE AL-UANON       TO  DINPRNTA             
02168                      MOVE +1             TO  WS-UPDATE-SW         
02169                      MOVE DC-BIN-DATE-1  TO  WS-IN-PRINT-DATE     
02170              ELSE                                                 
02171                  MOVE ER-0642    TO  EMI-ERROR                    
02172                  MOVE -1         TO  DINPRNTL                     
02173                  MOVE AL-UABON   TO  DINPRNTA                     
02174                  PERFORM 9900-ERROR-FORMAT.                       
02175                                                                   
02176      IF DREPRNTL GREATER ZERO                                     
02177          IF DREPRNTI = SPACES                                     
02178              MOVE AL-UANON       TO  DREPRNTA                     
02179              MOVE +1             TO  WS-UPDATE-SW                 
02180              MOVE LOW-VALUES     TO  WS-REPRINTED-DATE            
02181          ELSE                                                     
02182              MOVE DREPRNTI       TO  WS-DEEDIT-FIELD              
02183              PERFORM 8600-DEEDIT                                  
02184              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
02185                  MOVE WS-DEEDIT-FIELD-V0  TO  DREPRNTO            
02186                  INSPECT DREPRNTI CONVERTING SPACES TO SLASH      
02187                  MOVE '4'                 TO  DC-OPTION-CODE      
02188                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY   
02189                  PERFORM 8500-DATE-CONVERSION                     
02190                  IF DC-ERROR-CODE NOT = SPACES                    
02191                      MOVE ER-0643        TO  EMI-ERROR            
02192                      MOVE -1             TO  DREPRNTL             
02193                      MOVE AL-UABON       TO  DREPRNTA             
02194                      PERFORM 9900-ERROR-FORMAT                    
02195                  ELSE                                             
02196                      MOVE AL-UANON       TO  DREPRNTA             
02197                      MOVE +1             TO  WS-UPDATE-SW         
02198                      MOVE DC-BIN-DATE-1  TO  WS-REPRINTED-DATE    
02199              ELSE                                                 
02200                  MOVE ER-0643       TO  EMI-ERROR                 
02201                  MOVE -1             TO  DREPRNTL                 
02202                  MOVE AL-UABON       TO  DREPRNTA                 
02203                  PERFORM 9900-ERROR-FORMAT.                       
02204                                                                   
02205      EJECT                                                        
02206  0500-MAIN-LOGIC.                                                 
02207      IF PI-MAP-NAME NOT = EL142E                                  
02208          GO TO 0600-MAIN-LOGIC.                                   
02209                                                                   
02210      IF NOT MODIFY-CAP                                            
02211          IF EMAINTI = 'S'                                         
02212              NEXT SENTENCE                                        
02213          ELSE                                                     
02214              MOVE 'UPDATE'       TO SM-READ                       
02215              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       
02216              MOVE ER-0070        TO  EMI-ERROR                    
02217              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
02218              GO TO 8100-SEND-INITIAL-MAP.                         
02219                                                                   
02220      MOVE +2                     TO  EMI-NUMBER-OF-LINES          
02221                                      EMI-SWITCH2                  
02222                                                                   
02223      IF EMAINTL NOT GREATER ZERO                                  
02224        OR (EMAINTL GREATER ZERO AND                               
02225            EMAINTI = 'S')                                         
02226              PERFORM 4000-READ-TRAILER-FILE.                      
02227                                                                   
02228      IF EMAINTI = 'C' OR 'D'                                      
02229          NEXT SENTENCE                                            
02230      ELSE                                                         
02231          MOVE ER-0023            TO  EMI-ERROR                    
02232          MOVE -1                 TO  EMAINTL                      
02233          MOVE AL-UABON           TO  EMAINTA                      
02234          PERFORM 8200-SEND-DATAONLY.                              
02235                                                                   
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO EMAINTL
062602           MOVE AL-UABON         TO EMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
02236      IF PI-COMPANY-ID EQUAL 'DMD'
02237       IF SYSTEM-MODIFY-CAP OR                                     
02238         PI-PROCESSOR-ID = 'LGXX'                                  
02239          NEXT SENTENCE                                            
02240        ELSE                                                       
02241          IF (PI-PROCESSOR-ID NOT = PI-SAVE-LAST-UPD-BY)           
02242                                   OR                              
02243             (WS-CURRENT-DATE NOT = PI-SAVE-LAST-MAINT-DT)         
02244               MOVE ER-8003        TO EMI-ERROR                    
02245               MOVE -1             TO EMAINTL                      
02246               MOVE AL-UABON       TO EMAINTA                      
02247               GO TO 8200-SEND-DATAONLY.                           
02248                                                                   
02249      IF EMAINTI = 'D'                                             
02250          PERFORM 3000-READ-FOR-UPDATE                             
02251          PERFORM 3200-DELETE                                      
02252          MOVE +1                 TO  WS-COMPLETED-SUCCESSFUL      
02253          MOVE -1                 TO  EPFKL                        
02254          MOVE 'S'                TO  EMAINTO                      
02255          MOVE AL-SANOF           TO  EMAINTA  ELINE1A  ELINE2A    
02256          PERFORM 8200-SEND-DATAONLY.                              
02257                                                                   
02258      MOVE AL-UANON               TO  EMAINTA.                     
02259                                                                   
02260      IF ELINE1L GREATER ZERO OR                                   
02261         ELINE2L GREATER ZERO                                      
02262           NEXT SENTENCE                                           
02263      ELSE                                                         
02264           PERFORM 4000-READ-TRAILER-FILE.                         
02265                                                                   
02266      PERFORM 3000-READ-FOR-UPDATE.                                
02267                                                                   
02268      IF ELINE1L GREATER ZERO                                      
02269          MOVE ELINE1I            TO  AT-INFO-LINE-1.              
02270                                                                   
02271      IF ELINE2L GREATER ZERO                                      
02272          MOVE ELINE2I            TO  AT-INFO-LINE-2.              
02273                                                                   
02274      MOVE PI-PROCESSOR-ID        TO  AT-GEN-INFO-LAST-UPDATED-BY. 
02275                                                                   
02276      MOVE WS-CURRENT-DATE        TO  AT-GEN-INFO-LAST-MAINT-DT.   
02277                                                                   
02278      PERFORM 3100-REWRITE.                                        
02279      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.     
02280                                                                   
02281      MOVE 'S'                    TO  EMAINTO.                     
02282      MOVE -1                     TO  EMAINTL.                     
02283      MOVE AL-UANOF               TO  EMAINTA.                     
02284      PERFORM 8200-SEND-DATAONLY.                                  
02285                                                                   
02286      EJECT                                                        
02287  0600-MAIN-LOGIC.                                                 
02288      IF PI-MAP-NAME NOT = EL142F                                  
02289          GO TO 0700-MAIN-LOGIC.                                   
02290                                                                   
02291      IF NOT MODIFY-CAP                                            
02292          IF FMAINTI = 'S'                                         
02293              NEXT SENTENCE                                        
02294          ELSE                                                     
02295              MOVE 'UPDATE'       TO SM-READ                       
02296              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       
02297              MOVE ER-0070        TO  EMI-ERROR                    
02298              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
02299              GO TO 8100-SEND-INITIAL-MAP.                         
02300                                                                   
02301      MOVE +2                     TO  EMI-NUMBER-OF-LINES          
02302                                      EMI-SWITCH2.                 
02303                                                                   
02304      IF FMAINTL NOT GREATER ZERO                                  
02305        OR (FMAINTL GREATER ZERO AND                               
02306            FMAINTI = 'S')                                         
02307              PERFORM 4000-READ-TRAILER-FILE.                      
02308                                                                   
02309      IF FMAINTI = 'C' OR 'D'                                      
02310          NEXT SENTENCE                                            
02311      ELSE                                                         
02312          MOVE ER-0023            TO  EMI-ERROR                    
02313          MOVE -1                 TO  FMAINTL                      
02314          MOVE AL-UABON           TO  FMAINTA                      
02315          PERFORM 8200-SEND-DATAONLY.                              
02316                                                                   
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO fMAINTL
062602           MOVE AL-UABON         TO fMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
02317      IF FMAINTI = 'D'                                             
02318          PERFORM 3000-READ-FOR-UPDATE                             
02319          PERFORM 3200-DELETE                                      
02320          MOVE +1                 TO  WS-COMPLETED-SUCCESSFUL      
02321          MOVE -1                 TO  FPFKL                        
02322          MOVE 'S'                TO  FMAINTO                      
02323          MOVE AL-SANOF           TO  FMAINTA  FSNOTIFA  FENOTIFA  
02324                                      FLINE1A  FLINE2A             
02325          PERFORM 8200-SEND-DATAONLY.                              
02326                                                                   
02327      MOVE AL-UANON               TO  FMAINTA.                     
02328                                                                   
02329      IF FSNOTIFL GREATER ZERO                                     
02330          MOVE FSNOTIFI           TO  WS-DEEDIT-FIELD              
02331          PERFORM 8600-DEEDIT                                      
02332          IF WS-DEEDIT-FIELD-V0 IS NUMERIC                         
02333              MOVE WS-DEEDIT-FIELD-V0  TO  FSNOTIFO                
02334              INSPECT FSNOTIFI CONVERTING SPACES TO SLASH          
02335              MOVE '4'                 TO  DC-OPTION-CODE          
02336              MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY      
02337              PERFORM 8500-DATE-CONVERSION                         
02338              IF DC-ERROR-CODE NOT = SPACES                        
02339                  MOVE ER-0298        TO  EMI-ERROR                
02340                  MOVE -1             TO  FSNOTIFL                 
02341                  MOVE AL-UABON       TO  FSNOTIFA                 
02342                  PERFORM 9900-ERROR-FORMAT                        
02343              ELSE                                                 
02344                  MOVE AL-UANON       TO  FSNOTIFA                 
02345                  MOVE +1             TO  WS-UPDATE-SW             
02346                  MOVE DC-BIN-DATE-1  TO  WS-START-DATE            
02347          ELSE                                                     
02348              MOVE ER-0298        TO  EMI-ERROR                    
02349              MOVE -1             TO  FSNOTIFL                     
02350              MOVE AL-UABON       TO  FSNOTIFA                     
02351              PERFORM 9900-ERROR-FORMAT.                           
02352                                                                   
02353      IF FENOTIFL GREATER ZERO                                     
02354          MOVE FENOTIFI           TO  WS-DEEDIT-FIELD              
02355          PERFORM 8600-DEEDIT                                      
02356          IF WS-DEEDIT-FIELD-V0 IS NUMERIC                         
02357              MOVE WS-DEEDIT-FIELD-V0  TO  FENOTIFO                
02358              INSPECT FENOTIFI CONVERTING SPACES TO SLASH          
02359              MOVE '4'                 TO  DC-OPTION-CODE          
02360              MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY      
02361              PERFORM 8500-DATE-CONVERSION                         
02362              IF DC-ERROR-CODE NOT = SPACES                        
02363                  MOVE ER-0299           TO  EMI-ERROR             
02364                  MOVE -1                TO  FENOTIFL              
02365                  MOVE AL-UABON          TO  FENOTIFA              
02366                  PERFORM 9900-ERROR-FORMAT                        
02367              ELSE                                                 
02368                  MOVE AL-UANON       TO  FENOTIFA                 
02369                  MOVE +1             TO  WS-UPDATE-SW             
02370                  MOVE DC-BIN-DATE-1  TO  WS-END-DATE              
02371          ELSE                                                     
02372              MOVE ER-0299        TO  EMI-ERROR                    
02373              MOVE -1             TO  FENOTIFL                     
02374              MOVE AL-UABON       TO  FENOTIFA                     
02375              PERFORM 9900-ERROR-FORMAT.                           
02376                                                                   
02377      IF FLINE1L GREATER ZERO                                      
02378        OR FLINE2L GREATER ZERO                                    
02379          MOVE +1                 TO  WS-UPDATE-SW.                
02380                                                                   
02381      IF WS-ERROR-COUNT GREATER ZERO                               
02382          PERFORM 8200-SEND-DATAONLY.                              
02383                                                                   
02384      IF WS-UPDATE-SW NOT GREATER ZERO                             
02385          PERFORM 4000-READ-TRAILER-FILE.                          
02386                                                                   
02387      IF FSNOTIFL GREATER ZERO                                     
02388          PERFORM 3300-UPDATE-CLAIM-MASTER.                        
02389                                                                   
02390      PERFORM 3000-READ-FOR-UPDATE.                                
02391                                                                   
02392      IF FLINE1L GREATER ZERO                                      
02393          MOVE FLINE1I            TO  AT-PROMPT-LINE-1.            
02394                                                                   
02395      IF FLINE2L GREATER ZERO                                      
02396          MOVE FLINE2I            TO  AT-PROMPT-LINE-2.            
02397                                                                   
02398      IF FSNOTIFL GREATER ZERO                                     
02399          MOVE WS-START-DATE      TO  AT-PROMPT-START-DT.          
02400                                                                   
02401      IF FENOTIFL GREATER ZERO                                     
02402          MOVE WS-END-DATE        TO  AT-PROMPT-END-DT.            
02403                                                                   
02404      MOVE PI-PROCESSOR-ID        TO  AT-PROMPT-LAST-UPDATED-BY.   
02405                                                                   
02406      MOVE WS-CURRENT-DATE        TO  AT-PROMPT-LAST-MAINT-DT.     
02407                                                                   
02408      PERFORM 3100-REWRITE.                                        
02409      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.     
02410                                                                   
02411      MOVE 'S'                    TO  FMAINTO.                     
02412      MOVE -1                     TO  FMAINTL.                     
02413      MOVE AL-UANOF               TO  FMAINTA.                     
02414      PERFORM 8200-SEND-DATAONLY.                                  
02415                                                                   
02416      EJECT                                                        
02417  0700-MAIN-LOGIC.                                                 
02418      IF PI-MAP-NAME NOT = EL142G                                  
02419          GO TO 0800-MAIN-LOGIC.                                   
02420                                                                   
02421      IF NOT MODIFY-CAP                                            
02422          IF GMAINTI = 'S'                                         
02423              NEXT SENTENCE                                        
02424          ELSE                                                     
02425              MOVE 'UPDATE'       TO SM-READ                       
02426              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       
02427              MOVE ER-0070        TO  EMI-ERROR                    
02428              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
02429              GO TO 8100-SEND-INITIAL-MAP.                         
02430                                                                   
02431      MOVE +2                     TO  EMI-NUMBER-OF-LINES          
02432                                      EMI-SWITCH2.                 
02433                                                                   
02434      IF GMAINTL NOT GREATER ZERO                                  
02435        OR (GMAINTL GREATER ZERO AND                               
02436            GMAINTI = 'S')                                         
02437              PERFORM 4000-READ-TRAILER-FILE.                      
02438                                                                   
042110     IF GMAINTI NOT = 'C' AND 'D'
02440          MOVE ER-0023            TO  EMI-ERROR                    
02441          MOVE -1                 TO  GMAINTL                      
02442          MOVE AL-UABON           TO  GMAINTA                      
02443          PERFORM 8200-SEND-DATAONLY.                              
02444                                                                   
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO gMAINTL
062602           MOVE AL-UABON         TO gMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
042110     IF GMAINTI = 'D'
120718        if pi-approval-level <> '5'
042110           MOVE ER-0310            TO  EMI-ERROR                    
042110           MOVE -1                 TO  GMAINTL                      
042110           MOVE AL-UABON           TO  GMAINTA                      
042110           PERFORM 8200-SEND-DATAONLY
042110        ELSE
042110           ADD 1 TO PI-MAPG-DELETE-CNT
042110           IF PI-MAPG-DELETE-CNT > 1
042110              PERFORM 3000-READ-FOR-UPDATE
042110              PERFORM 3200-DELETE
042110              PERFORM 3500-READ-ELMSTR-FOR-UPDATE
042110                                 THRU 3599-EXIT
042110              MOVE SPACES        TO CL-DENIAL-TYPE
042110              MOVE 'O'           TO CL-CLAIM-STATUS
042110              EXEC CICS REWRITE
042110                 DATASET (WS-CLAIM-MASTER-DSID)
042110                 FROM    (CLAIM-MASTER)
042110              END-EXEC
042110              MOVE 'O'              TO WS-CLAIM-TYPE
042110              PERFORM 0710-UPDATE-ZERO-TRLR THRU 0730-MAIN-LOGIC
042110              MOVE +1               TO WS-COMPLETED-SUCCESSFUL
042110              MOVE -1               TO GPFKL
042110              MOVE 'S'              TO GMAINTO
042110              MOVE AL-SANOF         TO GMAINTA  GLINE1A  GLINE2A
042110              PERFORM 8200-SEND-DATAONLY
042110           ELSE
042110              MOVE ER-0755            TO  EMI-ERROR                    
042110              MOVE -1                 TO  GMAINTL                      
042110              MOVE AL-UABOF           TO  GMAINTA                      
042110              PERFORM 8200-SEND-DATAONLY
042110           END-IF
042110        END-IF
042110     END-IF

02445      MOVE AL-UANON               TO  GMAINTA.                     
02446                                                                   
02447      IF GRECONSL GREATER ZERO                                     
02448          IF GRECONSI = SPACES                                     
02449              MOVE AL-UANON       TO  GRECONSA                     
02450              MOVE +1             TO  WS-UPDATE-SW                 
02451              MOVE LOW-VALUES     TO  WS-END-DATE                  
042110             MOVE +4             TO  GRSNCDL
02452          ELSE                                                     
042110             MOVE +0             TO  GRSNCDL
02453              MOVE GRECONSI           TO  WS-DEEDIT-FIELD          
02454              PERFORM 8600-DEEDIT                                  
02455              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
02456                  MOVE WS-DEEDIT-FIELD-V0  TO  GRECONSO            
02457                  INSPECT GRECONSI CONVERTING SPACES TO SLASH      
02458                  MOVE '4'                 TO  DC-OPTION-CODE      
02459                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY   
02460                  PERFORM 8500-DATE-CONVERSION                     
02461                  IF DC-ERROR-CODE NOT = SPACES                    
02462                      MOVE ER-0300        TO  EMI-ERROR            
02463                      MOVE -1             TO  GRECONSL             
02464                      MOVE AL-UABON       TO  GRECONSA             
02465                      PERFORM 9900-ERROR-FORMAT                    
02466                  ELSE                                             
02467                      MOVE AL-UANON       TO  GRECONSA             
02468                      MOVE +1             TO  WS-UPDATE-SW         
02469                      MOVE DC-BIN-DATE-1  TO  WS-END-DATE          
02470              ELSE                                                 
02471                  MOVE ER-0300        TO  EMI-ERROR                
02472                  MOVE -1             TO  GRECONSL                 
02473                  MOVE AL-UNBON       TO  GRECONSA                 
02474                  PERFORM 9900-ERROR-FORMAT.                       
02475                                                                   
02476      IF WS-END-DATE GREATER THAN WS-CURRENT-DATE                  
02477         MOVE ER-0300             TO  EMI-ERROR                    
02478         MOVE -1                  TO  GRECONSL                     
02479         MOVE AL-UABON            TO  GRECONSA                     
02480         PERFORM 9900-ERROR-FORMAT.                                
02481                                                                   
052506     IF  GPRFDTL > 0
052506         MOVE GPRFDTI             TO WS-DEEDIT-FIELD
052506         PERFORM 8600-DEEDIT      
052506         MOVE WS-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY 
052506         MOVE '4'                 TO DC-OPTION-CODE
052506         PERFORM 8500-DATE-CONVERSION
052506         IF DC-ERROR-CODE NOT = SPACES                     
052506            MOVE ER-0021          TO EMI-ERROR        
052506            MOVE -1               TO GPRFDTL         
052506            MOVE AL-UABON         TO GPRFDTA         
052506            PERFORM 9900-ERROR-FORMAT 
052506         ELSE                                   
043019            IF (DC-BIN-DATE-1 > WS-CURRENT-DATE)
043019               or (dc-bin-date-1 > pi-den-recorded-dt)
043019               or (dc-bin-date-1 < pi-incurred-dt)
052506                MOVE ER-0873      TO EMI-ERROR
052506                MOVE -1           TO GPRFDTL
052506                MOVE AL-UABON     TO GPRFDTA
052506                PERFORM 9900-ERROR-FORMAT
052506            ELSE     
052506                MOVE AL-UANON       TO  GPRFDTA             
052506                MOVE +1             TO  WS-UPDATE-SW         
052506                MOVE DC-BIN-DATE-1    TO WS-PRF-DT     
052506                MOVE WS-DEEDIT-FIELD-V0  TO GPRFDTO         
052506                INSPECT GPRFDTI CONVERTING ' ' TO '/'.                  
052506         
02482      IF GLINE1L GREATER ZERO OR                                   
02483         GLINE2L GREATER ZERO                                      
02484            MOVE +1               TO  WS-UPDATE-SW.                
02485                                                                   
02486      IF WS-ERROR-COUNT GREATER ZERO                               
02487          PERFORM 8200-SEND-DATAONLY.                              
02488                                                                   
02489      IF GRSNCDL GREATER ZERO                                      
02490          MOVE +1 TO WS-UPDATE-SW.                                 
02491                                                                   
02492      IF WS-UPDATE-SW NOT GREATER ZERO                             
02493          PERFORM 4000-READ-TRAILER-FILE.                          
02494                                                                   
02495      IF GRECONSL NOT GREATER ZERO                                 
02496          GO TO 0740-MAIN-LOGIC.                                   
02497                                                                   
02498      MOVE PI-SAVE-KEY            TO PI-ACTIVITY-TRAILERS-KEY.     
02499                                                                   
02500      PERFORM 3500-READ-ELMSTR-FOR-UPDATE.                         
02501                                                                   
02502      IF PI-COMPANY-ID = 'DMD'                                     
02503          MOVE CL-CERT-KEY-DATA     TO WS-CL-CERT-KEY-DATA         
02504          MOVE CL-CERT-NO           TO WS-CL-CERT-NO               
02505          MOVE CL-BENEFICIARY       TO WS-CL-BENEFICIARY           
02506          MOVE CL-CCN               TO WS-CL-CCN                   
02507          MOVE CL-CLAIM-NO          TO WS-CL-CLAIM-NO              
02508          MOVE CL-CLAIM-TYPE        TO WS-CL-CLAIM-TYPE            
02509          MOVE CL-INSURED-LAST-NAME TO WS-CL-INSURED-LAST-NAME     
02510          MOVE CL-INSURED-1ST-NAME  TO WS-CL-INSURED-1ST-NAME      
02511          MOVE CL-INSURED-MID-INIT  TO WS-CL-INSURED-MID-INIT      
02512          MOVE CL-NO-OF-PMTS-MADE   TO WS-CL-NO-OF-PMTS-MADE.      
02513                                                                   
02514      IF WS-END-DATE = LOW-VALUES                                  
02515         IF CLAIM-IS-CLOSED                                        
02516             EXEC CICS UNLOCK                                      
02517                  DATASET   (WS-CLAIM-MASTER-DSID)                 
02518             END-EXEC                                              
02519             GO TO 0740-MAIN-LOGIC                                 
02520         ELSE                                                      
02521            MOVE 'C'              TO CL-CLAIM-STATUS               
02522            MOVE WS-CURRENT-DATE  TO CL-LAST-CLOSE-DT              
02523      ELSE                                                         
02524         IF CLAIM-IS-OPEN                                          
02525             EXEC CICS UNLOCK                                      
02526                  DATASET   (WS-CLAIM-MASTER-DSID)                 
02527             END-EXEC                                              
02528             GO TO 0740-MAIN-LOGIC                                 
02529         ELSE                                                      
02530            MOVE 'O'              TO CL-CLAIM-STATUS
042110           MOVE '5'              TO CL-DENIAL-TYPE
02531            MOVE WS-CURRENT-DATE  TO CL-LAST-REOPEN-DT.            
02532                                                                   
02533      MOVE CL-CLAIM-STATUS        TO  WS-CLAIM-TYPE.               
02534                                                                   
02535      PERFORM 3600-REWRITE-ELMSTR.                                 
02536                                                                   
042110 0710-UPDATE-ZERO-TRLR.

042110     MOVE PI-SAVE-KEY              TO  WS-ACTIVITY-TRAILERS-KEY.  
02538      MOVE ZERO                     TO  WS-ATK-SEQUENCE-NO.        

02539                                                                   
02540      EXEC CICS READ UPDATE                                        
02541          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      
02542          RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                       
02543          SET     (ADDRESS OF ACTIVITY-TRAILERS)                   
02544      END-EXEC.                                                    
02545                                                                   
02546      MOVE +1                     TO  WS-INDEX.                    
02547                                                                   
02548  0720-MAIN-LOGIC.                                                 

02549      IF AT-OPEN-CLOSE-TYPE (WS-INDEX) = SPACES                    
02550         IF WS-INDEX GREATER +1                                    
02551            SUBTRACT +1 FROM WS-INDEX                              
02552            IF AT-OPEN-CLOSE-TYPE (WS-INDEX) = WS-CLAIM-TYPE       
02553               MOVE WS-CURRENT-DATE TO                             
02554                    AT-OPEN-CLOSE-DATE (WS-INDEX)                  
02555               GO TO 0730-MAIN-LOGIC                               
02556            ELSE                                                   
02557               ADD +1 TO WS-INDEX                                  
02558               MOVE WS-CURRENT-DATE  TO                            
02559                      AT-OPEN-CLOSE-DATE (WS-INDEX)                
02560               MOVE WS-CLAIM-TYPE    TO                            
02561                      AT-OPEN-CLOSE-TYPE (WS-INDEX)                
02562               MOVE 'ALTER'          TO                            
02563                      AT-OPEN-CLOSE-REASON (WS-INDEX)              
02564               GO TO 0730-MAIN-LOGIC.                              
02565                                                                   
02566      IF WS-INDEX LESS THAN +6                                     
02567          ADD +1                  TO  WS-INDEX                     
02568          GO TO 0720-MAIN-LOGIC.                                   
02569                                                                   
02570      IF AT-OPEN-CLOSE-TYPE (6) = WS-CLAIM-TYPE                    
02571         MOVE WS-CURRENT-DATE     TO AT-OPEN-CLOSE-DATE (6)        
02572         GO TO 0730-MAIN-LOGIC.                                    
02573                                                                   
02574      MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1). 
02575      MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2). 
02576      MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3). 
02577      MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4). 
02578      MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5). 
02579                                                                   
02580      MOVE WS-CURRENT-DATE        TO  AT-OPEN-CLOSE-DATE (6).      
02581      MOVE WS-CLAIM-TYPE          TO  AT-OPEN-CLOSE-TYPE (6).      
02582      MOVE 'ALTER'                TO  AT-OPEN-CLOSE-REASON (6).    
02583                                                                   
02584  0730-MAIN-LOGIC.                                                 
02585                                                                   
02586      MOVE PI-PROCESSOR-ID        TO  AT-RESERVES-LAST-UPDATED-BY. 
02587      MOVE WS-CURRENT-DATE        TO  AT-RESERVES-LAST-MAINT-DT.   
02588                                                                   
02589      EXEC CICS REWRITE                                            
02590          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      
02591          FROM    (ACTIVITY-TRAILERS)                              
02592      END-EXEC.                                                    
02593                                                                   
02594  0740-MAIN-LOGIC.                                                 
02595                                                                   
02596      PERFORM 3000-READ-FOR-UPDATE.                                
02597                                                                   
02598      IF GLINE1L GREATER ZERO                                      
02599          MOVE GLINE1I            TO  AT-DENIAL-INFO-1.            
02600                                                                   
02601      IF GLINE2L GREATER ZERO                                      
02602          MOVE GLINE2I            TO  AT-DENIAL-INFO-2.            

033010     IF GRSNCDL > +0
033010        MOVE LOW-VALUES          TO WS-ELDENY-KEY
033010        MOVE PI-COMPANY-CD       TO ELDENY-COMPANY-CD
033010        MOVE GRSNCDI             TO ELDENY-DENIAL-CODE
033010        EXEC CICS READ
033010           DATASET('ELDENY')
033010           SET    (ADDRESS OF DENIAL-CODES)
033010           RIDFLD (WS-ELDENY-KEY)
033010           RESP   (WS-RESPONSE)
033010        END-EXEC
033010        IF RESP-NORMAL
033010           MOVE GRSNCDI          TO AT-DENIAL-REASON-CODE
042110           IF AT-DENIAL-REASON-CODE NOT = PI-DENIAL-REASON-CODE
033010              IF GLINE1L > +0
033010                 STRING DN-DESCRIPTION ' ' GLINE1I
033010                    DELIMITED BY '  ' INTO AT-DENIAL-INFO-1
033010                 END-STRING
033010              ELSE
033010                 STRING AT-DENIAL-INFO-1 ' ' DN-DESCRIPTION
033010                    DELIMITED BY '  ' INTO AT-DENIAL-INFO-1
033010                 END-STRING
033010              END-IF
                 END-IF
033010           PERFORM 3500-READ-ELMSTR-FOR-UPDATE
033010                                 THRU 3599-EXIT
033010           MOVE DN-RECORD-TYPE   TO CL-DENIAL-TYPE
033010           EXEC CICS REWRITE
033010              DATASET (WS-CLAIM-MASTER-DSID)
033010              FROM    (CLAIM-MASTER)
033010           END-EXEC
033010           MOVE AT-DENIAL-INFO-1 TO GLINE1O
033010           MOVE +1               TO GLINE1L
033010           MOVE AL-UANON         TO GLINE1A
033010        ELSE
033010           MOVE AL-UABON         TO GRSNCDA
033010           MOVE -1               TO GRSNCDL
033010           MOVE ER-0884          TO EMI-ERROR
033010           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT       
033010           EXEC CICS UNLOCK
033010              DATASET (WS-ACTIVITY-TRAILERS-DSID)
033010           END-EXEC
033010           GO TO 8200-SEND-DATAONLY
033010        END-IF
033010     END-IF
02608                                                                   
02609      IF GRECONSL GREATER ZERO                                     
02610          MOVE WS-END-DATE        TO  AT-RETRACTION-DT             
02611          IF PI-COMPANY-ID = 'DMD'                                 
02612              PERFORM 8000-CREATE-DMO-REC THRU 8000-EXIT.   
052506
052506     IF GPRFDTL GREATER ZERO
052506         MOVE WS-PRF-DT          TO  AT-DENIAL-PROOF-DT.       
02613                                                                   
02614      MOVE PI-PROCESSOR-ID        TO  AT-DENIAL-LAST-UPDATED-BY.   
02615                                                                   
02616      MOVE WS-CURRENT-DATE        TO  AT-DENIAL-LAST-MAINT-DT.     
02617                                                                   
02618      PERFORM 3100-REWRITE.                                        
02619      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.     
02620                                                                   
02621      MOVE 'S'                    TO  GMAINTO.                     
02622      MOVE -1                     TO  GMAINTL.                     
02623      MOVE AL-UANOF               TO  GMAINTA.                     
02624      PERFORM 8200-SEND-DATAONLY.                                  
02625                                                                   
02626      EJECT                                                        
02627  0800-MAIN-LOGIC.                                                 
02628      IF PI-MAP-NAME NOT = EL142H                                  
02629          GO TO 0900-MAIN-LOGIC.                                   
02630                                                                   
02631      IF NOT MODIFY-CAP                                            
02632          IF HMAINTI = 'S'                                         
02633              NEXT SENTENCE                                        
02634          ELSE                                                     
02635              MOVE 'UPDATE'       TO SM-READ                       
02636              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       
02637              MOVE ER-0070        TO  EMI-ERROR                    
02638              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
02639              GO TO 8100-SEND-INITIAL-MAP.                         
02640                                                                   
02641      MOVE +2                     TO  EMI-NUMBER-OF-LINES          
02642                                      EMI-SWITCH2.                 
02643                                                                   
02644      IF HMAINTL NOT GREATER ZERO                                  
02645        OR (HMAINTL GREATER ZERO AND                               
02646            HMAINTI = 'S')                                         
02647              PERFORM 4000-READ-TRAILER-FILE.                      
02648                                                                   
02649      IF HMAINTI NOT = 'C'                                         
02650          MOVE ER-0023            TO  EMI-ERROR                    
02651          MOVE -1                 TO  HMAINTL                      
02652          MOVE AL-UABON           TO  HMAINTA                      
02653          PERFORM 8200-SEND-DATAONLY.                              
02654                                                                   
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO hMAINTL
062602           MOVE AL-UABON         TO hMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
02655      MOVE AL-UANON               TO  HMAINTA.                     
02656                                                                   
02657      IF HRESMANL GREATER ZERO                                     
02658          IF HRESMANI = 'Y' OR 'N'                                 
02659              MOVE AL-UANON       TO  HRESMANA                     
02660          ELSE                                                     
02661              MOVE -1             TO  HRESMANL                     
02662              MOVE AL-UABON       TO  HRESMANA                     
02663              MOVE ER-0107        TO  EMI-ERROR                    
02664              PERFORM 9900-ERROR-FORMAT.                           
02665                                                                   
02666      IF HRESFUTL GREATER ZERO                                     
02667          IF HRESFUTI = 'Y' OR 'N'                                 
02668              MOVE AL-UANON       TO  HRESFUTA                     
02669          ELSE                                                     
02670              MOVE -1             TO  HRESFUTL                     
02671              MOVE AL-UABON       TO  HRESFUTA                     
02672              MOVE ER-0109        TO  EMI-ERROR                    
02673              PERFORM 9900-ERROR-FORMAT.                           
02674                                                                   
02675      IF HRESIBNL GREATER ZERO                                     
02676          IF HRESIBNI = 'Y' OR 'N'                                 
02677              MOVE AL-UANON       TO  HRESIBNA                     
02678          ELSE                                                     
02679              MOVE -1             TO  HRESIBNL                     
02680              MOVE AL-UABON       TO  HRESIBNA                     
02681              MOVE ER-0111        TO  EMI-ERROR                    
02682              PERFORM 9900-ERROR-FORMAT.                           
02683                                                                   
02684      IF HRESLFPL GREATER ZERO                                     
02685          IF HRESLFPI = 'Y' OR 'N'                                 
02686              MOVE AL-UANON       TO  HRESLFPA                     
02687          ELSE                                                     
02688              MOVE -1             TO  HRESLFPL                     
02689              MOVE AL-UABON       TO  HRESLFPA                     
02690              MOVE ER-0324        TO  EMI-ERROR                    
02691              PERFORM 9900-ERROR-FORMAT.                           
02692                                                                   
02693      IF HRESAHPL GREATER ZERO                                     
02694          IF HRESAHPI = 'Y' OR 'N'                                 
02695              MOVE AL-UANON       TO  HRESAHPA                     
02696          ELSE                                                     
02697              MOVE -1             TO  HRESAHPL                     
02698              MOVE AL-UABON       TO  HRESAHPA                     
02699              MOVE ER-0325        TO  EMI-ERROR                    
02700              PERFORM 9900-ERROR-FORMAT.                           
02701                                                                   
02702      IF HCDTAML GREATER ZERO                                      
02703          IF HCDTAMI = '1' OR '2' OR '3'                           
02704              MOVE AL-UNNON       TO  HCDTAMA                      
02705          ELSE                                                     
02706              MOVE AL-UNBON       TO  HCDTAMA                      
02707              MOVE -1             TO  HCDTAML                      
02708              MOVE ER-0105        TO  EMI-ERROR                    
02709              PERFORM 9900-ERROR-FORMAT.                           
02710                                                                   
02711      IF HMANAMTL GREATER ZERO                                     
02712          EXEC CICS BIF DEEDIT                                     
02713              FIELD  (HMANAMTI)                                    
02714              LENGTH (WS-HMANAMT-LENGTH)                           
02715          END-EXEC                                                 
02716          IF HMANAMTI IS NUMERIC                                   
02717              MOVE HMANAMTI       TO  WS-HMANAMTI                  
02718                                      HMANAMTO                     
02719              MOVE AL-UNNON       TO  HMANAMTA                     
02720            ELSE                                                   
02721              MOVE -1             TO  HMANAMTL                     
02722              MOVE AL-UNBON       TO  HMANAMTA                     
02723              MOVE ER-0107        TO  EMI-ERROR                    
02724              PERFORM 9900-ERROR-FORMAT.                           
02725                                                                   
02726      IF HPCTCDTL GREATER ZERO                                     
02727          EXEC CICS BIF DEEDIT                                     
02728              FIELD  (HPCTCDTI)                                    
02729              LENGTH (WS-HPCTCDT-LENGTH)                           
02730          END-EXEC                                                 
02731          IF HPCTCDTI IS NUMERIC                                   
02732              MOVE HPCTCDTI       TO  WS-HPCTCDTI                  
02733                                      HPCTCDTO                     
02734              MOVE AL-UNNON       TO  HPCTCDTA                     
02735          ELSE                                                     
02736              MOVE -1             TO  HPCTCDTL                     
02737              MOVE AL-UNBON       TO  HPCTCDTA                     
02738              MOVE ER-0106        TO  EMI-ERROR                    
02739              PERFORM 9900-ERROR-FORMAT.                           
02740                                                                   
02741      IF HEXPL GREATER ZERO                                        
02742          IF HEXPI = '1' OR '2' OR '3'                             
02743              MOVE AL-UNNON       TO  HEXPA                        
02744          ELSE                                                     
02745              MOVE -1             TO  HEXPL                        
02746              MOVE AL-UNBON       TO  HEXPA                        
02747              MOVE ER-0327        TO  EMI-ERROR                    
02748              PERFORM 9900-ERROR-FORMAT.                           
02749                                                                   
02750      IF HEXPAMTL GREATER ZERO                                     
02751          EXEC CICS BIF DEEDIT                                     
02752              FIELD  (HEXPAMTI)                                    
02753              LENGTH (WS-HEXPAMT-LENGTH)                           
02754          END-EXEC                                                 
02755          IF HEXPAMTI IS NUMERIC                                   
02756              MOVE HEXPAMTI       TO  WS-HEXPAMTI                  
02757                                      HEXPAMTO                     
02758              MOVE AL-UNNON       TO  HEXPAMTA                     
02759          ELSE                                                     
02760              MOVE -1             TO  HEXPAMTL                     
02761              MOVE AL-UNBON       TO  HEXPAMTA                     
02762              MOVE ER-0328        TO  EMI-ERROR                    
02763              PERFORM 9900-ERROR-FORMAT.                           
02764                                                                   
02765      IF WS-ERROR-COUNT GREATER ZERO                               
02766          PERFORM 8200-SEND-DATAONLY.                              
02767                                                                   
02768      PERFORM 3000-READ-FOR-UPDATE.                                
02769                                                                   
02770      IF HRESMANL GREATER ZERO                                     
02771          MOVE HRESMANI           TO  AT-MANUAL-SW                 
02772          INSPECT AT-MANUAL-SW CONVERTING 'NY' TO ' 1'.            
02773                                                                   
02774      IF HMANAMTL GREATER ZERO                                     
02775          MOVE WS-HMANAMTI        TO  AT-CURRENT-MANUAL-RESERVE.   
02776                                                                   
02777      IF HRESFUTL GREATER ZERO                                     
02778          MOVE HRESFUTI           TO  AT-FUTURE-SW                 
02779          INSPECT AT-FUTURE-SW CONVERTING 'NY' TO ' 1'.            
02780                                                                   
02781      IF HRESIBNL GREATER ZERO                                     
02782          MOVE HRESIBNI           TO  AT-IBNR-SW                   
02783          INSPECT AT-IBNR-SW CONVERTING 'NY' TO ' 1'.              
02784                                                                   
02785      IF HRESLFPL GREATER ZERO                                     
02786          MOVE HRESLFPI           TO  AT-PTC-LF-SW                 
02787          INSPECT AT-PTC-LF-SW CONVERTING 'NY' TO ' 1'.            
02788                                                                   
02789      IF HRESAHPL GREATER ZERO                                     
02790          MOVE HRESAHPI           TO  AT-PTC-SW                    
02791          INSPECT AT-PTC-SW CONVERTING 'NY' TO ' 1'.               
02792                                                                   
02793      IF HCDTAML GREATER ZERO                                      
02794          MOVE HCDTAMI            TO  AT-CDT-ACCESS-METHOD.        
02795                                                                   
02796      IF HMANAMTL GREATER ZERO                                     
02797          MOVE WS-HMANAMTI        TO  AT-CURRENT-MANUAL-RESERVE.   
02798                                                                   
02799      IF HPCTCDTL GREATER ZERO                                     
02800          MOVE WS-HPCTCDTI        TO  AT-PERCENT-OF-CDT.           
02801                                                                   
02802      IF HEXPL GREATER ZERO                                        
02803          MOVE HEXPI              TO  AT-EXPENSE-METHOD.           
02804                                                                   
02805      IF HEXPAMTL GREATER ZERO                                     
02806          IF AT-EXPENSE-METHOD = '2' OR '4'                        
02807              MOVE WS-HEXPAMTI    TO  AT-EXPENSE-DOLLAR            
02808            ELSE                                                   
02809              IF AT-EXPENSE-METHOD = '3'                           
02810                  MOVE WS-HEXPAMTI TO AT-EXPENSE-PERCENT           
02811                ELSE                                               
02812                  MOVE ZERO       TO  AT-EXPENSE-DOLLAR            
02813                                      AT-EXPENSE-PERCENT.          
02814                                                                   
02815      MOVE PI-PROCESSOR-ID        TO  AT-RESERVES-LAST-UPDATED-BY. 
02816                                                                   
02817      MOVE WS-CURRENT-DATE        TO  AT-RESERVES-LAST-MAINT-DT.   
02818                                                                   
02819      PERFORM 3100-REWRITE.                                        
02820      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.     
02821                                                                   
02822      MOVE 'S'                    TO  HMAINTO.                     
02823      MOVE -1                     TO  HMAINTL.                     
02824      MOVE AL-UANOF               TO  HMAINTA.                     
02825      PERFORM 8200-SEND-DATAONLY.                                  
02826                                                                   
02827      EJECT                                                        
02828  0900-MAIN-LOGIC.                                                 
02829      IF PI-MAP-NAME NOT = EL142I                                  
02830          GO TO 1000-MAIN-LOGIC.                                   
02831                                                                   
02832      PERFORM 4000-READ-TRAILER-FILE.                              
02833                                                                   
02834      EJECT                                                        
02835  1000-MAIN-LOGIC.                                                 
02836      IF PI-MAP-NAME NOT = EL142J                                  
02837          GO TO 1100-MAIN-LOGIC.                                   
02838                                                                   
02839      IF NOT MODIFY-CAP                                            
02840          IF JMAINTI = 'S'                                         
02841              NEXT SENTENCE                                        
02842          ELSE                                                     
02843              MOVE 'UPDATE'       TO SM-READ                       
02844              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       
02845              MOVE ER-0070        TO  EMI-ERROR                    
02846              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
02847              GO TO 8100-SEND-INITIAL-MAP.                         
02848                                                                   
02849      MOVE +3                     TO  EMI-NUMBER-OF-LINES.         
02850      MOVE +2                     TO  EMI-SWITCH2.                 
02851                                                                   
02852      IF JMAINTL NOT GREATER ZERO                                  
02853        OR (JMAINTL GREATER ZERO AND                               
02854            JMAINTI = 'S')                                         
02855              PERFORM 4000-READ-TRAILER-FILE.                      
02856                                                                   
02857      IF JMAINTI = 'C' OR 'D'                                      
02858          NEXT SENTENCE                                            
02859      ELSE                                                         
02860          MOVE ER-0023            TO  EMI-ERROR                    
02861          MOVE -1                 TO  JMAINTL                      
02862          MOVE AL-UABON           TO  JMAINTA                      
02863          PERFORM 8200-SEND-DATAONLY.                              
02864                                                                   
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO jMAINTL
062602           MOVE AL-UABON         TO jMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
02865      IF FMAINTI NOT = 'D'                                         
02866          GO TO 1050-MAIN-LOGIC.                                   
02867                                                                   
02868      EXEC CICS READ                                               
02869          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      
02870          RIDFLD  (PI-SAVE-KEY)                                    
02871          SET     (ADDRESS OF ACTIVITY-TRAILERS)                   
02872      END-EXEC.                                                    
02873                                                                   
02874      IF AT-FORM-PRINTED-DT NOT = LOW-VALUES                       
02875          MOVE ER-0564            TO  EMI-ERROR                    
02876          MOVE -1                 TO  JMAINTL                      
02877          MOVE AL-UABON           TO  JMAINTA                      
02878          PERFORM 8200-SEND-DATAONLY.                              
02879                                                                   
02880      PERFORM 3000-READ-FOR-UPDATE.                                
02881                                                                   
02882      PERFORM 3400-DELETE-FORM-ARCHIVE.                            
02883                                                                   
02884      PERFORM 3200-DELETE.                                         
02885                                                                   
02886      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.     
02887      MOVE -1                     TO  JPFKL.                       
02888      MOVE 'S'                    TO  JMAINTO.                     
02889                                                                   
02890      MOVE AL-SANOF               TO  JMAINTA  JDTSENTA  JRESENDA  
02891                                      JSI1A    JREPLYA   JSI2A     
02892                                      JRECEVEA JSI3A     JFORMA    
02893                                      JCARR1A  JCLAIM1A  JCERT1A   
02894                                      JCARR2A  JCLAIM2A  JCERT2A   
02895                                      JPHYRECA JEMPRECA  JREMDTA.  
02896                                                                   
02897      PERFORM 8200-SEND-DATAONLY.                                  
02898                                                                   
02899  1050-MAIN-LOGIC.                                                 
02900      MOVE AL-UANON               TO  JMAINTA.                     
02901                                                                   
02902      IF JDTSENTL GREATER ZERO                                     
02903          MOVE JDTSENTI           TO  WS-DEEDIT-FIELD              
02904          PERFORM 8600-DEEDIT                                      
02905          IF WS-DEEDIT-FIELD-V0 IS NUMERIC                         
02906              MOVE WS-DEEDIT-FIELD-V0  TO  JDTSENTO                
02907              INSPECT JDTSENTI CONVERTING SPACES TO SLASH          
02908              MOVE '4'                 TO  DC-OPTION-CODE          
02909              MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY       
02910              PERFORM 8500-DATE-CONVERSION                         
02911              IF DC-ERROR-CODE NOT = SPACES                        
02912                  MOVE ER-0550    TO  EMI-ERROR                    
02913                  MOVE -1         TO  JDTSENTL                     
02914                  MOVE AL-UABON   TO  JDTSENTA                     
02915                  PERFORM 9900-ERROR-FORMAT                        
02916              ELSE                                                 
02917                  MOVE AL-UANON   TO  JDTSENTA                     
02918                  MOVE +1         TO  WS-UPDATE-SW                 
02919                  MOVE DC-BIN-DATE-1  TO  WS-SEND-ON-DATE          
02920                  IF WS-SEND-ON-DATE LESS THAN WS-CURRENT-DATE     
02921                      MOVE ER-0551 TO  EMI-ERROR                   
02922                      MOVE -1      TO  JDTSENTL                    
02923                      MOVE AL-UABON TO  JDTSENTA                   
02924                      PERFORM 9900-ERROR-FORMAT                    
02925                  ELSE                                             
02926                      NEXT SENTENCE                                
02927          ELSE                                                     
02928              MOVE ER-0550        TO  EMI-ERROR                    
02929              MOVE -1             TO  JDTSENTL                     
02930              MOVE AL-UABON       TO  JDTSENTA                     
02931              PERFORM 9900-ERROR-FORMAT.                           
02932                                                                   
02933      IF JRESENDL GREATER ZERO                                     
02934          IF JRESENDI = SPACES                                     
02935              MOVE AL-UANON       TO  JRESENDA                     
02936              MOVE +1             TO  WS-UPDATE-SW                 
02937              MOVE LOW-VALUES     TO  WS-RESEND-DATE               
02938          ELSE                                                     
02939              MOVE JRESENDI       TO  WS-DEEDIT-FIELD              
02940              PERFORM 8600-DEEDIT                                  
02941              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
02942                  MOVE WS-DEEDIT-FIELD-V0  TO  JRESENDO            
02943                  INSPECT JRESENDI CONVERTING SPACES TO SLASH      
02944                  MOVE '4'                 TO  DC-OPTION-CODE      
02945                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY   
02946                  PERFORM 8500-DATE-CONVERSION                     
02947                  IF DC-ERROR-CODE NOT = SPACES                    
02948                      MOVE ER-0295           TO  EMI-ERROR         
02949                      MOVE -1             TO  JRESENDL             
02950                      MOVE AL-UABON       TO  JRESENDA             
02951                      PERFORM 9900-ERROR-FORMAT                    
02952                  ELSE                                             
02953                      MOVE AL-UANON       TO  JRESENDA             
02954                      MOVE +1             TO  WS-UPDATE-SW         
02955                      MOVE DC-BIN-DATE-1  TO  WS-RESEND-DATE       
02956              ELSE                                                 
02957                  MOVE ER-0295        TO  EMI-ERROR                
02958                  MOVE -1             TO  JRESENDL                 
02959                  MOVE AL-UABON       TO  JRESENDA                 
02960                  PERFORM 9900-ERROR-FORMAT.                       
02961                                                                   
02962      IF JREPLYL GREATER ZERO                                      
02963          IF JREPLYI = SPACES                                      
02964              MOVE AL-UANON       TO  JREPLYA                      
02965              MOVE +1             TO  WS-UPDATE-SW                 
02966              MOVE LOW-VALUES     TO  WS-FOLLOW-UP-DATE            
02967          ELSE                                                     
02968              MOVE JREPLYI        TO  WS-DEEDIT-FIELD              
02969              PERFORM 8600-DEEDIT                                  
02970              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
02971                  MOVE WS-DEEDIT-FIELD-V0  TO  JREPLYO             
02972                  INSPECT JREPLYI CONVERTING SPACES TO SLASH       
02973                  MOVE '4'                 TO  DC-OPTION-CODE      
02974                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY   
02975                  PERFORM 8500-DATE-CONVERSION                     
02976                  IF DC-ERROR-CODE NOT = SPACES                    
02977                      MOVE ER-0296           TO  EMI-ERROR         
02978                      MOVE -1             TO  JREPLYL              
02979                      MOVE AL-UABON       TO  JREPLYA              
02980                      PERFORM 9900-ERROR-FORMAT                    
02981                  ELSE                                             
02982                      MOVE AL-UANON       TO  JREPLYA              
02983                      MOVE +1             TO  WS-UPDATE-SW         
02984                      MOVE DC-BIN-DATE-1  TO  WS-FOLLOW-UP-DATE    
02985              ELSE                                                 
02986                  MOVE ER-0296        TO  EMI-ERROR                
02987                  MOVE -1             TO  JREPLYL                  
02988                  MOVE AL-UABON       TO  JREPLYA                  
02989                  PERFORM 9900-ERROR-FORMAT.                       
02990                                                                   
02991      IF JRECEVEL GREATER ZERO                                     
02992          IF JRECEVEI = SPACES                                     
02993              MOVE AL-UANON       TO  JRECEVEA                     
02994              MOVE +1             TO  WS-UPDATE-SW                 
02995              MOVE LOW-VALUES     TO  WS-RECEIVED-DATE             
02996          ELSE                                                     
02997              MOVE JRECEVEI       TO  WS-DEEDIT-FIELD              
02998              PERFORM 8600-DEEDIT                                  
02999              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
03000                  MOVE WS-DEEDIT-FIELD-V0  TO  JRECEVEO            
03001                  INSPECT JRECEVEI CONVERTING SPACES TO SLASH      
03002                  MOVE '4'                TO  DC-OPTION-CODE       
03003                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY   
03004                  PERFORM 8500-DATE-CONVERSION                     
03005                  IF DC-ERROR-CODE NOT = SPACES                    
03006                      MOVE ER-0297        TO  EMI-ERROR            
03007                      MOVE -1             TO  JRECEVEL             
03008                      MOVE AL-UABON       TO  JRECEVEA             
03009                      PERFORM 9900-ERROR-FORMAT                    
03010                  ELSE                                             
03011                      MOVE AL-UANON       TO  JRECEVEA             
03012                      MOVE +1             TO  WS-UPDATE-SW         
03013                      MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-DATE     
03014              ELSE                                                 
03015                  MOVE ER-0297        TO  EMI-ERROR                
03016                  MOVE -1             TO  JRECEVEL                 
03017                  MOVE AL-UABON       TO  JRECEVEA                 
03018                  PERFORM 9900-ERROR-FORMAT.                       
03019                                                                   
03020      IF JPHYRECL GREATER ZERO                                     
03021          IF JPHYRECI = SPACES                                     
03022              MOVE AL-UANON       TO  JPHYRECA                     
03023              MOVE +1             TO  WS-UPDATE-SW                 
03024              MOVE LOW-VALUES     TO  WS-RECEIVED-PHY-DATE         
03025          ELSE                                                     
03026              MOVE JPHYRECI       TO  WS-DEEDIT-FIELD              
03027              PERFORM 8600-DEEDIT                                  
03028              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
03029                  MOVE WS-DEEDIT-FIELD-V0  TO  JPHYRECO            
03030                  INSPECT JPHYRECI CONVERTING SPACES TO SLASH      
03031                  MOVE '4'                TO  DC-OPTION-CODE       
03032                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY   
03033                  PERFORM 8500-DATE-CONVERSION                     
03034                  IF DC-ERROR-CODE NOT = SPACES                    
03035                      MOVE ER-0297        TO  EMI-ERROR            
03036                      MOVE -1             TO  JPHYRECL             
03037                      MOVE AL-UABON       TO  JPHYRECA             
03038                      PERFORM 9900-ERROR-FORMAT                    
03039                  ELSE                                             
03040                      MOVE AL-UANON       TO  JPHYRECA             
03041                      MOVE +1             TO  WS-UPDATE-SW         
03042                      MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-PHY-DATE 
03043              ELSE                                                 
03044                  MOVE ER-0297        TO  EMI-ERROR                
03045                  MOVE -1             TO  JPHYRECL                 
03046                  MOVE AL-UABON       TO  JPHYRECA                 
03047                  PERFORM 9900-ERROR-FORMAT.                       
03048                                                                   
03049      IF JEMPRECL GREATER ZERO                                     
03050          IF JEMPRECI = SPACES                                     
03051              MOVE AL-UANON       TO  JEMPRECA                     
03052              MOVE +1             TO  WS-UPDATE-SW                 
03053              MOVE LOW-VALUES     TO  WS-RECEIVED-EMP-DATE         
03054          ELSE                                                     
03055              MOVE JEMPRECI       TO  WS-DEEDIT-FIELD              
03056              PERFORM 8600-DEEDIT                                  
03057              IF WS-DEEDIT-FIELD-V0 IS NUMERIC                     
03058                  MOVE WS-DEEDIT-FIELD-V0  TO  JEMPRECO            
03059                  INSPECT JEMPRECI CONVERTING SPACES TO SLASH      
03060                  MOVE '4'                TO  DC-OPTION-CODE       
03061                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY   
03062                  PERFORM 8500-DATE-CONVERSION                     
03063                  IF DC-ERROR-CODE NOT = SPACES                    
03064                      MOVE ER-0297        TO  EMI-ERROR            
03065                      MOVE -1             TO  JEMPRECL             
03066                      MOVE AL-UABON       TO  JEMPRECA             
03067                      PERFORM 9900-ERROR-FORMAT                    
03068                  ELSE                                             
03069                      MOVE AL-UANON       TO  JEMPRECA             
03070                      MOVE +1             TO  WS-UPDATE-SW         
03071                      MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-EMP-DATE 
03072              ELSE                                                 
03073                  MOVE ER-0297        TO  EMI-ERROR                
03074                  MOVE -1             TO  JEMPRECL                 
03075                  MOVE AL-UABON       TO  JEMPRECA                 
03076                  PERFORM 9900-ERROR-FORMAT.                       
03077                                                                   
03078      IF JFORML GREATER ZERO                                       
03079          IF JFORMI = 'INITIAL'  OR 'I' OR                         
03080                      'PROGRESS' OR 'P'                            
03081              MOVE AL-UANON       TO  JFORMA                       
03082          ELSE                                                     
03083              MOVE ER-0532        TO  EMI-ERROR                    
03084              MOVE AL-UABON       TO  JFORMA                       
03085              MOVE -1             TO  JFORML                       
03086              PERFORM 9900-ERROR-FORMAT.                           
03087                                                                   
03088      IF JSI1L GREATER ZERO         OR                             
03089         JSI2L GREATER ZERO         OR                             
03090         JSI3L GREATER ZERO         OR                             
03091         JCARR1L GREATER ZERO       OR                             
03092         JCLAIM1L GREATER ZERO      OR                             
03093         JCERT1L GREATER ZERO       OR                             
03094         JCARR2L GREATER ZERO       OR                             
03095         JCLAIM2L GREATER ZERO      OR                             
03096         JCERT2L GREATER ZERO                                      
03097          MOVE +1                 TO  WS-UPDATE-SW.                
03098                                                                   
03099      IF WS-ERROR-COUNT GREATER ZERO                               
03100          PERFORM 8200-SEND-DATAONLY.                              
03101                                                                   
03102      IF WS-UPDATE-SW NOT GREATER ZERO                             
03103          PERFORM 4000-READ-TRAILER-FILE.                          
03104                                                                   
03105      IF JREPLYL GREATER ZERO                                      
03106          PERFORM 3300-UPDATE-CLAIM-MASTER.                        
03107                                                                   
03108      PERFORM 3000-READ-FOR-UPDATE.                                
03109                                                                   
03110      IF JDTSENTL GREATER ZERO                                     
03111          MOVE WS-SEND-ON-DATE    TO  AT-FORM-SEND-ON-DT           
03112          MOVE LOW-VALUES         TO  AT-FORM-PRINTED-DT.          
03113                                                                   
03114      IF JRESENDL GREATER ZERO                                     
03115          MOVE WS-RESEND-DATE     TO  AT-FORM-RE-SEND-DT.          
03116                                                                   
03117      IF JREPLYL GREATER ZERO                                      
03118          MOVE WS-FOLLOW-UP-DATE  TO  AT-FORM-FOLLOW-UP-DT.        
03119                                                                   
03120      IF JRECEVEL GREATER ZERO                                     
03121          MOVE WS-RECEIVED-DATE   TO  AT-FORM-ANSWERED-DT.         
03122                                                                   
03123      IF JPHYRECL GREATER ZERO                                     
03124          MOVE WS-RECEIVED-PHY-DATE   TO  AT-PHY-FORM-ANSWERED-DT. 
03125                                                                   
03126      IF JEMPRECL GREATER ZERO                                     
03127          MOVE WS-RECEIVED-EMP-DATE   TO  AT-EMP-FORM-ANSWERED-DT. 
03128                                                                   
03129      IF JFORML GREATER ZERO                                       
03130          MOVE JFORMI             TO  AT-FORM-TYPE                 
03131          INSPECT AT-FORM-TYPE CONVERTING 'IP' TO '12'.            
03132                                                                   
03133      IF JSI1L GREATER ZERO                                        
03134          MOVE JSI1I              TO  AT-INSTRUCT-LN-1.            
03135                                                                   
03136      IF JSI2L GREATER ZERO                                        
03137          MOVE JSI2I              TO  AT-INSTRUCT-LN-2.            
03138                                                                   
03139      IF JSI3L GREATER ZERO                                        
03140          MOVE JSI3I              TO  AT-INSTRUCT-LN-3.            
03141                                                                   
03142      IF AT-INSTRUCT-LN-2 = SPACES                                 
03143          MOVE AT-INSTRUCT-LN-3   TO  AT-INSTRUCT-LN-2             
03144          MOVE SPACES             TO  AT-INSTRUCT-LN-3.            
03145                                                                   
03146      IF AT-INSTRUCT-LN-1 = SPACES                                 
03147          MOVE AT-INSTRUCT-LN-2   TO  AT-INSTRUCT-LN-1             
03148          MOVE AT-INSTRUCT-LN-3   TO  AT-INSTRUCT-LN-2             
03149          MOVE SPACES             TO  AT-INSTRUCT-LN-3.            
03150                                                                   
03151      IF JCARR1L GREATER ZERO                                      
03152          MOVE JCARR1I            TO  AT-REL-CARR-1.               
03153                                                                   
03154      IF JCLAIM1L GREATER ZERO                                     
03155          MOVE JCLAIM1I           TO  AT-REL-CLAIM-1.              
03156                                                                   
03157      IF JCERT1L GREATER ZERO                                      
03158          MOVE JCERT1I            TO  AT-REL-CERT-1.               
03159                                                                   
03160      IF JCARR2L GREATER ZERO                                      
03161          MOVE JCARR2I            TO  AT-REL-CARR-2.               
03162                                                                   
03163      IF JCLAIM2L GREATER ZERO                                     
03164          MOVE JCLAIM2I           TO  AT-REL-CLAIM-2.              
03165                                                                   
03166      IF JCERT2L GREATER ZERO                                      
03167          MOVE JCERT2I            TO  AT-REL-CERT-2.               
03168                                                                   
03169      MOVE PI-PROCESSOR-ID        TO  AT-FORM-LAST-UPDATED-BY.     
03170                                                                   
03171      MOVE WS-CURRENT-DATE        TO  AT-FORM-LAST-MAINT-DT.       
03172                                                                   
03173      PERFORM 3100-REWRITE.                                        
03174      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.     
03175                                                                   
03176      MOVE 'S'                    TO  JMAINTO.                     
03177      MOVE -1                     TO  JMAINTL.                     
03178      MOVE AL-UANOF               TO  JMAINTA.                     
03179      PERFORM 8200-SEND-DATAONLY.                                  
03180                                                                   
03181      EJECT                                                        
03182  1100-MAIN-LOGIC.                                                 
03183      IF PI-MAP-NAME NOT = EL142B2                                 
03184          GO TO 1200-MAIN-LOGIC.                                   
03185                                                                   
03186      IF NOT MODIFY-CAP                                            
03187          IF KMAINTI = 'S'                                         
03188              NEXT SENTENCE                                        
03189          ELSE                                                     
03190              MOVE 'UPDATE'       TO SM-READ                       
03191              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       
03192              MOVE ER-0070        TO  EMI-ERROR                    
03193              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
03194              GO TO 8100-SEND-INITIAL-MAP.                         
03195                                                                   
03196      MOVE +3                     TO  EMI-NUMBER-OF-LINES.         
03197      MOVE +2                     TO  EMI-SWITCH2.                 
03198                                                                   
03199      IF KMAINTL NOT GREATER ZERO                                  
03200        OR (KMAINTL GREATER ZERO AND                               
03201            KMAINTI = 'S')                                         
03202              MOVE PI-SAVE-KEY    TO  PI-ACTIVITY-TRAILERS-KEY     
03203              PERFORM 4000-READ-TRAILER-FILE.                      
03204                                                                   

062602     if kmainti = 'C' or 'D' or 'A'
020816        if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602           if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602              MOVE ER-8003       TO EMI-ERROR
062602              MOVE -1            TO kMAINTL
062602              MOVE AL-UABON      TO kMAINTA
062602              GO TO 8200-SEND-DATAONLY
062602           end-if
062602        end-if
062602     end-if
062602
03205      IF KMAINTI NOT = 'D'                                         
03206          GO TO 1120-MAIN-LOGIC.                                   
03207                                                                   
03208      PERFORM 3000-READ-FOR-UPDATE.                                
03209                                                                   
03210      MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.            
03211      MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER.        
03212      MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER.       
03213                                                                   
03214      EXEC CICS READ UPDATE                                        
03215          DATASET (WS-CHECK-QUEUE-DSID)                            
03216          RIDFLD  (WS-CHECK-QUEUE-KEY)                             
03217          SET     (ADDRESS OF CHECK-QUE)                           
03218      END-EXEC.                                                    
03219                                                                   
03220      EXEC CICS DELETE                                             
03221          DATASET (WS-CHECK-QUEUE-DSID)                            
03222      END-EXEC.                                                    
03223                                                                   
03224      MOVE +99999999              TO  AT-CHECK-QUE-CONTROL.        
03225      MOVE ZERO                   TO  AT-CHECK-QUE-SEQUENCE.       
03226                                                                   
03227      PERFORM 3100-REWRITE.                                        
03228                                                                   
03229      MOVE ER-ZERO                TO  EMI-ERROR.                   
03230      PERFORM 9900-ERROR-FORMAT.                                   
03231                                                                   
03232      MOVE PI-SAVE-KEY  TO  PI-ACTIVITY-TRAILERS-KEY.              
03233      PERFORM 4000-READ-TRAILER-FILE.                              
03234                                                                   
03235  1120-MAIN-LOGIC.                                                 
03236      IF KMAINTI = 'A' OR 'C'                                      
03237          NEXT SENTENCE                                            
03238      ELSE                                                         
03239          MOVE ER-0023            TO  EMI-ERROR                    
03240          MOVE -1                 TO  KMAINTL                      
03241          MOVE AL-UABON           TO  KMAINTA                      
03242          PERFORM 8200-SEND-DATAONLY.                              
03243                                                                   
03244      IF KMAINTI NOT = 'C'                                         
03245          GO TO 1150-MAIN-LOGIC.                                   
03246                                                                   
03247      IF KTIMPRTL GREATER ZERO                                     
03248          EXEC CICS BIF DEEDIT                                     
03249              FIELD  (KTIMPRTI)                                    
03250              LENGTH (4)                                           
03251          END-EXEC                                                 
03252          IF KTIMPRTI NOT NUMERIC                                  
03253              MOVE -1             TO  KTIMPRTL                     
03254              MOVE AL-UNBON       TO  KTIMPRTA                     
03255              MOVE ER-0579        TO  EMI-ERROR                    
03256              PERFORM 9900-ERROR-FORMAT                            
03257          ELSE                                                     
03258              MOVE KTIMPRTI       TO  KTIMPRTO                     
03259              MOVE +1             TO  WS-UPDATE-SW.                
03260                                                                   
03261      IF WS-ERROR-COUNT GREATER ZERO                               
03262          PERFORM 8200-SEND-DATAONLY.                              
03263                                                                   
03264      EXEC CICS READ                                               
03265          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      
03266          RIDFLD  (PI-SAVE-KEY)                                    
03267          SET     (ADDRESS OF ACTIVITY-TRAILERS)                   
03268      END-EXEC.                                                    
03269                                                                   
03270      MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.            
03271      MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER.        
03272      MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER.       
03273                                                                   
03274      EXEC CICS READ UPDATE                                        
03275          DATASET (WS-CHECK-QUEUE-DSID)                            
03276          RIDFLD  (WS-CHECK-QUEUE-KEY)                             
03277          SET     (ADDRESS OF CHECK-QUE)                           
03278      END-EXEC.                                                    
03279                                                                   
03280      IF KTIMPRTL GREATER ZERO                                     
03281          EXEC CICS BIF DEEDIT                                     
03282              FIELD  (KTIMPRTI)                                    
03283              LENGTH (4)                                           
03284          END-EXEC                                                 
03285          MOVE KTIMPRTI           TO  CQ-TIMES-PRINTED             
03286                                      KTIMPRTO.                    
03287                                                                   
03288      EXEC CICS REWRITE                                            
03289          DATASET (WS-CHECK-QUEUE-DSID)                            
03290          FROM    (CHECK-QUE)                                      
03291      END-EXEC.                                                    
03292                                                                   
03293      MOVE ER-ZERO                TO  EMI-ERROR.                   
03294      PERFORM 9900-ERROR-FORMAT.                                   
03295                                                                   
03296      MOVE -1                     TO  KMAINTL.                     
03297                                                                   
03298      PERFORM 8200-SEND-DATAONLY.                                  
03299                                                                   
03300  1150-MAIN-LOGIC.                                                 
03301      IF KCONTRLL GREATER ZERO                                     
03302          EXEC CICS BIF DEEDIT                                     
03303              FIELD  (KCONTRLI)                                    
03304              LENGTH (8)                                           
03305          END-EXEC                                                 
03306          IF KCONTRLI NOT NUMERIC                                  
03307              MOVE -1             TO  KCONTRLL                     
03308              MOVE AL-UNBON       TO  KCONTRLA                     
03309              MOVE ER-0580        TO  EMI-ERROR                    
03310              PERFORM 9900-ERROR-FORMAT                            
03311          ELSE                                                     
03312              MOVE KCONTRLI       TO  KCONTRLO                     
03313              MOVE +1             TO  WS-UPDATE-SW.                
03314                                                                   
03315      IF KSEQL GREATER ZERO                                        
03316          EXEC CICS BIF DEEDIT                                     
03317              FIELD  (KSEQI)                                       
03318              LENGTH (4)                                           
03319          END-EXEC                                                 
03320          IF KSEQI NOT NUMERIC                                     
03321              MOVE -1             TO  KSEQL                        
03322              MOVE AL-UNBON       TO  KSEQA                        
03323              MOVE ER-0581        TO  EMI-ERROR                    
03324              PERFORM 9900-ERROR-FORMAT                            
03325          ELSE                                                     
03326              MOVE KSEQI          TO  KSEQO                        
03327              MOVE +1             TO  WS-UPDATE-SW.                
03328                                                                   
03329      IF KTIMPRTL GREATER ZERO                                     
03330          EXEC CICS BIF DEEDIT                                     
03331              FIELD  (KTIMPRTI)                                    
03332              LENGTH (4)                                           
03333          END-EXEC                                                 
03334          IF KTIMPRTI NOT NUMERIC                                  
03335              MOVE -1             TO  KTIMPRTL                     
03336              MOVE AL-UNBON       TO  KTIMPRTA                     
03337              MOVE ER-0579           TO  EMI-ERROR                 
03338              PERFORM 9900-ERROR-FORMAT                            
03339          ELSE                                                     
03340              MOVE KTIMPRTI       TO  KTIMPRTO                     
03341              MOVE +1             TO  WS-UPDATE-SW.                
03342                                                                   
03343      IF WS-ERROR-COUNT GREATER ZERO                               
03344          PERFORM 8200-SEND-DATAONLY.                              
03345                                                                   
03346      IF KCONTRLL GREATER ZERO                                     
03347        OR KSEQL GREATER ZERO                                      
03348          PERFORM 3000-READ-FOR-UPDATE                             
03349      ELSE                                                         
03350          EXEC CICS READ                                           
03351              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
03352              RIDFLD  (PI-SAVE-KEY)                                
03353              SET     (ADDRESS OF ACTIVITY-TRAILERS)               
03354          END-EXEC.                                                
03355                                                                   
03356      MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.            
03357      MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER.        
03358      MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER.       
03359                                                                   
03360      EXEC CICS GETMAIN                                            
03361          LENGTH  (WS-CHECK-QUEUE-LENGTH)                          
03362          INITIMG (WS-SPACES)                                      
03363          SET     (ADDRESS OF CHECK-QUE)                           
03364      END-EXEC.                                                    
03365                                                                   
03366      MOVE 'CQ'                   TO  CQ-RECORD-ID.                
03367      MOVE PI-COMPANY-CD          TO  CQ-COMPANY-CD.               
03368      MOVE AT-CHECK-QUE-CONTROL   TO  CQ-CONTROL-NUMBER.           
03369      MOVE AT-CHECK-QUE-SEQUENCE  TO  CQ-SEQUENCE-NUMBER.          
03370                                                                   
03371      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.               
03372      MOVE AT-CARRIER             TO  CQ-CARRIER.                  
03373      MOVE AT-CLAIM-NO            TO  CQ-CLAIM-NO.                 
03374      MOVE AT-CERT-NO             TO  CQ-CERT-NO.                  
03375      MOVE AT-CLAIM-TYPE          TO  CQ-CLAIM-TYPE.               
03376      MOVE AT-CLAIM-PREM-TYPE     TO  CQ-CLAIM-SUB-TYPE.           
03377                                                                   
03378      MOVE AT-SEQUENCE-NO         TO  CQ-PMT-TRLR-SEQUENCE.        
03379      MOVE AT-CHECK-NO            TO  CQ-CHECK-NUMBER.             
03380      MOVE AT-AMOUNT-PAID         TO  CQ-CHECK-AMOUNT.             
03381      MOVE AT-PAYMENT-TYPE        TO  CQ-PAYMENT-TYPE.             
03382                                                                   
03383      IF AT-VOID-DT NOT = LOW-VALUES                               
03384          MOVE 'V'                TO  CQ-VOID-INDICATOR.           
03385                                                                   
03386      MOVE ZERO                   TO  CQ-TIMES-PRINTED             
03387                                      CQ-PRINT-AT-HHMM.            
03388      MOVE AT-RECORDED-BY         TO  CQ-CHECK-BY-USER.            
03389      MOVE AT-CHECK-WRITTEN-DT    TO  CQ-CHECK-WRITTEN-DT.         
03390      MOVE +1420                  TO  CQ-LAST-UPDATED-BY.          
03391                                                                   
03392      IF KCONTRLL GREATER ZERO                                     
03393          EXEC CICS BIF DEEDIT                                     
03394              FIELD  (KCONTRLI)                                    
03395              LENGTH (8)                                           
03396          END-EXEC                                                 
03397          MOVE KCONTRLI           TO  CQ-CONTROL-NUMBER            
03398                                      AT-CHECK-QUE-CONTROL         
03399                                      KCONTRLO.                    
03400      IF KSEQL GREATER ZERO                                        
03401          EXEC CICS BIF DEEDIT                                     
03402              FIELD  (KSEQI)                                       
03403              LENGTH (4)                                           
03404          END-EXEC                                                 
03405          MOVE KSEQI              TO  CQ-SEQUENCE-NUMBER           
03406                                      AT-CHECK-QUE-SEQUENCE        
03407                                      KSEQO.                       
03408      IF KTIMPRTL GREATER ZERO                                     
03409          EXEC CICS BIF DEEDIT                                     
03410              FIELD  (KTIMPRTI)                                    
03411              LENGTH (4)                                           
03412          END-EXEC                                                 
03413          MOVE KTIMPRTI           TO  CQ-TIMES-PRINTED             
03414                                      KTIMPRTO.                    
03415                                                                   
03416      EXEC CICS WRITE                                              
03417          DATASET (WS-CHECK-QUEUE-DSID)                            
03418          FROM    (CHECK-QUE)                                      
03419          RIDFLD  (CQ-CONTROL-PRIMARY)                             
03420      END-EXEC.                                                    
03421                                                                   
03422      IF KCONTRLL GREATER ZERO OR                                  
03423         KSEQL    GREATER ZERO                                     
03424          PERFORM 3100-REWRITE.                                    
03425                                                                   
03426      MOVE ER-ZERO                TO  EMI-ERROR.                   
03427      PERFORM 9900-ERROR-FORMAT.                                   
03428                                                                   
03429      MOVE SPACES                 TO  KMAINTO.                     
03430      MOVE AL-UANOF               TO  KMAINTA.                     
03431      MOVE -1                     TO  KMAINTL.                     
03432                                                                   
03433      PERFORM 8200-SEND-DATAONLY.                                  
03434                                                                   
03435      EJECT                                                        
03436  1200-MAIN-LOGIC.                                                 
03437      IF PI-MAP-NAME = EL142D2    
102510*03438          GO TO 0015-MAIN-LOGIC.
102510         MOVE EL142D TO PI-MAP-NAME
102510         IF PI-END-OF-FILE = ZERO
102510             SUBTRACT +1 FROM PI-ATK-SEQUENCE-NO
102510             ADD +1 TO PI-PREV-ATK-SEQUENCE-NO
102510         END-IF
102510         MOVE ZERO TO PI-END-OF-FILE
102510         PERFORM 4000-READ-TRAILER-FILE
102510     END-IF.
03439                                                                   
03440      MOVE '1200-MAIN-LOGIC REACHED'  TO  LOGOFF-MSG.              
03441      PERFORM 8300-SEND-TEXT.                                      
03442                                                                   
03443      EJECT                                                        
03444  3000-READ-FOR-UPDATE SECTION.                                    
03445                                                                   
03446      EXEC CICS READ UPDATE                                        
03447          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      
03448          RIDFLD  (PI-SAVE-KEY)                                    
03449          SET     (ADDRESS OF ACTIVITY-TRAILERS)                   
03450      END-EXEC.                                                    
03451                                                                   
03452      IF AT-INFO-TRAILER-TYPE = 'M'                                
03453          MOVE ER-0969        TO EMI-ERROR                         
03454          MOVE -1             TO EMAINTL                           
03455          MOVE AL-UABON       TO EMAINTA                           
03456          EXEC CICS UNLOCK                                         
03457              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
03458          END-EXEC                                                 
03459          GO TO 8200-SEND-DATAONLY.                                
03460                                                                   
03461      IF AT-RECORDED-BY NOT = PI-UPDATE-BY                         
03462        OR AT-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS             
03463          MOVE AL-UABON           TO  HMAINTA                      
03464          MOVE -1                 TO  HMAINTL                      
03465          MOVE ER-0068            TO  EMI-ERROR                    
03466          EXEC CICS UNLOCK                                         
03467              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
03468          END-EXEC                                                 
03469          PERFORM 8200-SEND-DATAONLY.                              
03470                                                                   
03471  3000-EXIT.                                                       
03472      EXIT.                                                        
03473                                                                   
03474      EJECT                                                        
03475  3100-REWRITE SECTION.                                            
03476                                                                   
03477      MOVE PI-PROCESSOR-ID        TO  PI-UPDATE-BY                 
03478                                      BMANTBYO.                    
03479                                                                   
03480      MOVE AT-RECORDED-DT             TO  DC-BIN-DATE-1.           
03481      MOVE ' '                        TO  DC-OPTION-CODE.          
03482      PERFORM 8500-DATE-CONVERSION.                                
03483      IF NO-CONVERSION-ERROR                                       
03484          MOVE DC-GREG-DATE-1-EDIT    TO  BRECDTEO                 
03485      ELSE                                                         
03486          MOVE SPACES                 TO  BRECDTEO.                
03487                                                                   
03488      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS         
03489                                      PI-UPDATE-HHMMSS.            
03490                                                                   
03491      EXEC CICS REWRITE                                            
03492          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      
03493          FROM    (ACTIVITY-TRAILERS)                              
03494      END-EXEC.                                                    
03495                                                                   
03496      MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.                
03497                                                                   
03498      PERFORM 3500-READ-ELMSTR-FOR-UPDATE.                         
03499                                                                   
03500      PERFORM 3600-REWRITE-ELMSTR.                                 
03501                                                                   
03502  3100-EXIT.                                                       
03503      EXIT.                                                        
03504                                                                   
03505      EJECT                                                        
03506  3200-DELETE SECTION.                                             
03507                                                                   
03508      EXEC CICS DELETE                                             
03509          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      
03510      END-EXEC.                                                    
03511                                                                   
03512      MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.                
03513                                                                   
03514      EXEC CICS READ                                               
03515          DATASET (WS-CLAIM-MASTER-DSID)                           
03516          RIDFLD  (WS-CLAIM-KEY)                                   
03517          SET     (ADDRESS OF CLAIM-MASTER)                        
03518      END-EXEC.                                                    
03519                                                                   
03520      IF CL-TRAILER-SEQ-CNT NOT = PI-SAVE-ATK-SEQUENCE-NO          
03521          GO TO 3200-EXIT.                                         
03522                                                                   
03523      PERFORM 3500-READ-ELMSTR-FOR-UPDATE.                         
03524                                                                   
03525      EXEC CICS HANDLE CONDITION                                   
03526          NOTFND (3200-NOT-FOUND)                                  
03527      END-EXEC.                                                    
03528                                                                   
03529      EXEC CICS READ                                               
03530          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      
03531          RIDFLD  (PI-SAVE-KEY)                                    
03532          SET     (ADDRESS OF ACTIVITY-TRAILERS)                   
03533          GTEQ                                                     
03534      END-EXEC.                                                    
03535                                                                   
03536      IF PI-SAVE-ATK-COMPANY-CODE = AT-COMPANY-CD    AND           
03537         PI-SAVE-ATK-CARRIER      = AT-CARRIER       AND           
03538         PI-SAVE-ATK-CLAIM-NO     = AT-CLAIM-NO      AND           
03539         PI-SAVE-ATK-CERT-NO      = AT-CERT-NO                     
03540          MOVE AT-SEQUENCE-NO           TO  CL-TRAILER-SEQ-CNT     
03541          GO TO 3200-REWRITE-CLAIM-MASTER.                         
03542                                                                   
03543  3200-NOT-FOUND.                                                  
03544      MOVE +4095                  TO  CL-TRAILER-SEQ-CNT.          
03545                                                                   
03546  3200-REWRITE-CLAIM-MASTER.                                       
03547      PERFORM 3600-REWRITE-ELMSTR.                                 
03548                                                                   
03549  3200-EXIT.                                                       
03550      EXIT.                                                        
03551                                                                   
03552      EJECT                                                        
03553  3300-UPDATE-CLAIM-MASTER SECTION.                                
03554      MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.                
03555                                                                   
03556      EXEC CICS READ                                               
03557          DATASET (WS-CLAIM-MASTER-DSID)                           
03558          RIDFLD  (WS-CLAIM-KEY)                                   
03559          SET     (ADDRESS OF CLAIM-MASTER)                        
03560      END-EXEC.                                                    
03561                                                                   
03562      IF WS-FOLLOW-UP-DATE NOT LESS THAN CL-NEXT-FOLLOWUP-DT       
03563          GO TO 3300-EXIT.                                         
03564                                                                   
03565      PERFORM 3500-READ-ELMSTR-FOR-UPDATE.                         
03566                                                                   
03567      MOVE WS-FOLLOW-UP-DATE      TO  CL-NEXT-FOLLOWUP-DT.         
03568                                                                   
03569      PERFORM 3600-REWRITE-ELMSTR.                                 
03570                                                                   
03571  3300-EXIT.                                                       
03572      EXIT.                                                        
03573                                                                   
03574      EJECT                                                        
03575  3400-DELETE-FORM-ARCHIVE SECTION.                                
03576      EXEC CICS HANDLE CONDITION                                   
03577          ENDFILE (3490-DELETE-FORM-ARCHIVE)                       
03578          NOTFND  (3499-EXIT)                                      
03579      END-EXEC                                                     
03580                                                                   
03581      MOVE LOW-VALUES             TO  WS-LETTER-ARCHIVE-ALT-KEY.   
03582                                                                   
03583      MOVE PI-COMPANY-CD          TO  WS-LA-ALT-COMPANY-CD.        
03584      MOVE '4'                    TO  WS-LA-ALT-RECORD-TYPE.       
03585                                                                   
03586      EXEC CICS STARTBR                                            
03587          DATASET (WS-LETTER-ARCHIVE-DSID2)                        
03588          RIDFLD  (WS-LETTER-ARCHIVE-ALT-KEY)                      
03589          GTEQ                                                     
03590      END-EXEC.                                                    
03591                                                                   
03592  3410-DELETE-FORM-ARCHIVE.                                        
03593      EXEC CICS READNEXT                                           
03594          DATASET (WS-LETTER-ARCHIVE-DSID2)                        
03595          RIDFLD  (WS-LETTER-ARCHIVE-ALT-KEY)                      
03596          SET     (ADDRESS OF LETTER-ARCHIVE)                      
03597      END-EXEC.                                                    
03598                                                                   
03599      IF LA-COMPANY-CD NOT = PI-COMPANY-CD                         
03600          GO TO 3490-DELETE-FORM-ARCHIVE.                          
03601                                                                   
03602      IF LA-RECORD-TYPE NOT = '4'                                  
03603          GO TO 3490-DELETE-FORM-ARCHIVE.                          
03604                                                                   
03605      IF LA-CARRIER  NOT = PI-SAVE-ATK-CARRIER   OR                
03606         LA-CLAIM-NO NOT = PI-SAVE-ATK-CLAIM-NO  OR                
03607         LA-CERT-NO  NOT = PI-SAVE-ATK-CERT-NO                     
03608          GO TO 3410-DELETE-FORM-ARCHIVE.                          
03609                                                                   
03610      MOVE LA-COMPANY-CD          TO WS-LA-COMPANY-CD.             
03611      MOVE LA-ARCHIVE-NO          TO WS-LA-ARCHIVE-NO.             
03612      MOVE LA-RECORD-TYPE         TO WS-LA-RECORD-TYPE.            
03613      MOVE LA-LINE-SEQ-NO         TO WS-LA-LINE-SEQ-NO.            
03614                                                                   
03615      EXEC CICS ENDBR                                              
03616          DATASET (WS-LETTER-ARCHIVE-DSID2)                        
03617      END-EXEC.                                                    
03618                                                                   
03619      EXEC CICS READ UPDATE                                        
03620          DATASET (WS-LETTER-ARCHIVE-DSID)                         
03621          RIDFLD  (WS-LETTER-ARCHIVE-KEY)                          
03622          SET     (ADDRESS OF LETTER-ARCHIVE)                      
03623      END-EXEC.                                                    
03624                                                                   
03625      EXEC CICS DELETE                                             
03626          DATASET (WS-LETTER-ARCHIVE-DSID)                         
03627      END-EXEC.                                                    
03628                                                                   
03629      GO TO 3499-EXIT.                                             
03630                                                                   
03631  3490-DELETE-FORM-ARCHIVE.                                        
03632      EXEC CICS ENDBR                                              
03633          DATASET (WS-LETTER-ARCHIVE-DSID2)                        
03634      END-EXEC.                                                    
03635                                                                   
03636  3499-EXIT.                                                       
03637      EXIT.                                                        
03638                                                                   
03639      EJECT                                                        
03640  3500-READ-ELMSTR-FOR-UPDATE SECTION.                             
03641      MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.                
03642                                                                   
03643      EXEC CICS READ UPDATE                                        
03644          DATASET (WS-CLAIM-MASTER-DSID)                           
03645          RIDFLD  (WS-CLAIM-KEY)                                   
03646          SET     (ADDRESS OF CLAIM-MASTER)                        
03647      END-EXEC.                                                    
03648                                                                   
03649  3599-EXIT.                                                       
03650      EXIT.                                                        
03651                                                                   
03652  3600-REWRITE-ELMSTR SECTION.                                     
03653      EXEC CICS HANDLE CONDITION                                   
03654          DUPKEY (3699-EXIT)                                       
03655      END-EXEC.                                                    
03656                                                                   
03657      MOVE WS-CURRENT-DATE        TO  CL-LAST-MAINT-DT.            
03658      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.        
03659      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.          
03660      MOVE '3'                    TO  CL-LAST-MAINT-TYPE.          
03661                                                                   
03662      EXEC CICS REWRITE                                            
03663          DATASET (WS-CLAIM-MASTER-DSID)                           
03664          FROM    (CLAIM-MASTER)                                   
03665      END-EXEC.                                                    
03666                                                                   
03667  3699-EXIT.                                                       
03668      EXIT.                                                        
03669                                                                   
03670      EJECT                                                        
03671  4000-READ-TRAILER-FILE SECTION.                                  
03672      IF ((EIBAID = (DFHENTER OR DFHPF1) AND                       
03673          PI-PREV-AID = (DFHENTER OR DFHPF1))                      
03674        OR                                                         
03675         (EIBAID = DFHPF2 AND                                      
03676          PI-PREV-AID = DFHPF2)                                    
03677        OR                                                         
03678          PI-RECORD-COUNT = +1)                                    
03679        AND                                                        
03680          PI-END-OF-FILE NOT = ZERO                                
03681              GO TO 0015-MAIN-LOGIC.                               
03682                                                                   
03683      IF PI-END-OF-FILE NOT = ZERO                                 
03684          IF EIBAID = DFHPF2                                       
03685              MOVE PI-SAVE-KEY    TO  PI-PREV-ACTIVITY-TRAILERS-KEY
03686          ELSE                                                     
03687              MOVE PI-SAVE-KEY    TO  PI-ACTIVITY-TRAILERS-KEY.    
03688                                                                   
03689      MOVE LOW-VALUES             TO  EL142DO.                     
03690                                                                   
03691      IF EIBAID = DFHPF2                                           
03692          EXEC CICS STARTBR                                        
03693              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
03694              RIDFLD  (PI-PREV-ACTIVITY-TRAILERS-KEY)              
03695              EQUAL                                                
03696          END-EXEC                                                 
03697        ELSE                                                       
03698          EXEC CICS STARTBR                                        
03699              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
03700              RIDFLD  (PI-ACTIVITY-TRAILERS-KEY)                   
03701              GTEQ                                                 
03702          END-EXEC.                                                
03703                                                                   
03704      EJECT                                                        
03705  4100-READNEXT.                                                   
03706      IF EIBAID = DFHPF2                                           
03707          MOVE PI-PREV-ACTIVITY-TRAILERS-KEY                       
03708                                  TO  PI-ACTIVITY-TRAILERS-KEY     
03709          EXEC CICS READPREV                                       
03710              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
03711              RIDFLD  (PI-PREV-ACTIVITY-TRAILERS-KEY)              
03712              SET     (ADDRESS OF ACTIVITY-TRAILERS)               
03713          END-EXEC                                                 
03714      ELSE                                                         
03715          MOVE PI-ACTIVITY-TRAILERS-KEY                            
03716                                  TO  PI-PREV-ACTIVITY-TRAILERS-KEY
03717          EXEC CICS READNEXT                                       
03718              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
03719              RIDFLD  (PI-ACTIVITY-TRAILERS-KEY)                   
03720              SET     (ADDRESS OF ACTIVITY-TRAILERS)               
03721          END-EXEC.                                                
03722                                                                   
CIDMOD     IF AT-SEQUENCE-NO = +90 OR +91                               
03724          GO TO 4100-READNEXT.                                     
03725                                                                   
03726      IF AT-TRAILER-TYPE = '6'                                     
03727          IF AT-PAYMENT-NOTE                                       
03728              GO TO 4100-READNEXT.                                 
03729                                                                   
03730      ADD +1  TO  WS-RECORD-COUNT.                                 
03731      IF WS-RECORD-COUNT GREATER +0                                
03732         DIVIDE WS-RECORD-COUNT BY +10 GIVING WS-RECORD-DIV        
101807               REMAINDER WS-RECORD-REMAINDER
101807     END-IF
101807*       IF WS-RECORD-REMAINDER = +0                               
101807*          EXEC CICS DELAY                                        
101807*               INTERVAL(00001)
101807*          END-EXEC.                                              
03738      EJECT                                                        
03739      IF AT-COMPANY-CD NOT = PI-COMPANY-CD OR                      
03740         AT-CARRIER    NOT = PI-CARRIER    OR                      
03741         AT-CLAIM-NO   NOT = PI-CLAIM-NO   OR                      
03742         AT-CERT-NO    NOT = PI-CERT-NO                            
03743          GO TO 6000-END-OF-FILE.                                  
03744                                                                   
03745      IF AT-RECORDED-DT LESS THAN PI-AFTER-DATE                    
03746         GO TO 4100-READNEXT.                                      
03747                                                                   
03748      IF ((EIBAID NOT = DFHPF2 AND                                 
03749           PI-PREV-AID = DFHPF2)                                   
03750         OR                                                        
03751          (EIBAID = DFHPF2 AND                                     
03752           PI-PREV-AID NOT = DFHPF2)                               
03753         OR                                                        
03754           PI-END-OF-FILE = +1)                                    
03755        AND                                                        
03756          WS-RECORD-COUNT NOT GREATER +1                           
03757              GO TO 4100-READNEXT.                                 
03758                                                                   
03759      MOVE ZERO                   TO  PI-END-OF-FILE.              
03760                                                                   
03761      IF (AT-TRAILER-TYPE = '1' AND                                
03762          PI-RES-EXP-SW = +1)                                      
03763        OR                                                         
03764         (AT-TRAILER-TYPE = '2' AND                                
03765          PI-PAYMENTS-SW = +1)                                     
03766        OR                                                         
03767         (AT-TRAILER-TYPE = '3' AND                                
03768          PI-AUTO-PAY-SW = +1)                                     
03769        OR                                                         
03770         (AT-TRAILER-TYPE = '4' AND                                
03771          PI-LETTERS-SW = +1)                                      
03772        OR                                                         
03773         (AT-TRAILER-TYPE = '6' AND                                
03774          PI-NOTES-SW = +1)                                        
03775        OR                                                         
03776         (AT-TRAILER-TYPE = '7' AND                                
03777          PI-REMINDERS-SW = +1)                                    
03778        OR                                                         
03779         (AT-TRAILER-TYPE = '8' AND                                
03780          PI-DENIALS-SW = +1)                                      
03781        OR                                                         
03782         (AT-TRAILER-TYPE = '9' AND                                
03783          PI-INCURRED-DATE-SW = +1)                                
03784        OR                                                         
03785         (AT-TRAILER-TYPE = 'A' AND                                
03786          PI-FORMS-SW = +1)                                        
03787              NEXT SENTENCE                                        
03788            ELSE                                                   
03789              GO TO 4100-READNEXT.                                 
03790                                                                   
03791      ADD 1 TO LCP-ONCTR-01                                        
03792                                                                   
03793      IF LCP-ONCTR-01 = 2                                          
03794          GO TO 6000-ENDBROWSE.                                    
03795                                                                   
03796      MOVE AT-RECORDED-BY         TO  PI-UPDATE-BY.                
03797      MOVE AT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            
03798      MOVE AT-CONTROL-PRIMARY     TO  PI-SAVE-KEY.                 
03799                                                                   
03800      ADD +1                      TO  PI-RECORD-COUNT.             
03801                                                                   
03802      EJECT                                                        
03803      IF AT-TRAILER-TYPE NOT = '1'                                 
03804          GO TO 4200-PAYMENT-TRAILER.                              
03805                                                                   
03806      MOVE AT-CONTROL-PRIMARY     TO WS-CLAIM-KEY.                 
03807                                                                   
03808      EXEC CICS READ                                               
03809           DATASET (WS-CLAIM-MASTER-DSID)                          
03810           RIDFLD  (WS-CLAIM-KEY)                                  
03811           SET     (ADDRESS OF CLAIM-MASTER)                       
03812      END-EXEC.                                                    
03813                                                                   
03814      IF CL-LAST-CLOSE-DT = LOW-VALUES                             
03815         MOVE SPACES              TO HLSTCLOO                      
03816      ELSE                                                         
03817         MOVE CL-LAST-CLOSE-DT    TO DC-BIN-DATE-1                 
03818         MOVE SPACES              TO  DC-OPTION-CODE               
03819         PERFORM 8500-DATE-CONVERSION                              
03820         MOVE DC-GREG-DATE-1-EDIT TO HLSTCLOO.                     
03821                                                                   
03822      IF CL-LAST-REOPEN-DT = LOW-VALUES                            
03823         MOVE SPACES TO HLSTOPEO                                   
03824      ELSE                                                         
03825         MOVE CL-LAST-REOPEN-DT   TO DC-BIN-DATE-1                 
03826         MOVE SPACES              TO  DC-OPTION-CODE               
03827         PERFORM 8500-DATE-CONVERSION                              
03828         MOVE DC-GREG-DATE-1-EDIT TO HLSTOPEO.                     
03829                                                                   
03830      MOVE EL142H                 TO  PI-MAP-NAME.                 
03831                                                                   
03832      MOVE AT-TRAILER-TYPE        TO  HTLRTYPO.                    
03833      MOVE AT-SEQUENCE-NO         TO  HSEQO.                       
03834                                                                   
03835      MOVE SPACES                 TO  DC-OPTION-CODE.              
03836      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               
03837      PERFORM 8500-DATE-CONVERSION.                                
03838      MOVE DC-GREG-DATE-1-EDIT    TO  HRECDTEO.                    
03839                                                                   
03840      MOVE AT-RECORDED-BY         TO  HBYO.                        
03841                                                                   
03842      MOVE AT-RESERVES-LAST-MAINT-DT   TO  DC-BIN-DATE-1.          
03843      MOVE ' '                         TO  DC-OPTION-CODE.         
03844      PERFORM 8500-DATE-CONVERSION.                                
03845      IF NO-CONVERSION-ERROR                                       
03846          MOVE DC-GREG-DATE-1-EDIT     TO  HMANTONO                
03847      ELSE                                                         
03848          MOVE SPACES                  TO  HMANTONO.               
03849                                                                   
03850      MOVE AT-RESERVES-LAST-UPDATED-BY TO  HMANTBYO.               
03851                                                                   
03852      MOVE AT-LAST-MAINT-HHMMSS        TO  TIME-IN.                
03853      MOVE TIME-OUT                    TO  HMANTATO.               
03854                                                                   
03855      MOVE AT-MANUAL-SW           TO  HRESMANO.                    
03856      INSPECT HRESMANO CONVERTING ' 1' TO 'NY'.                    
03857                                                                   
03858      MOVE AT-FUTURE-SW           TO  HRESFUTO.                    
03859      INSPECT HRESFUTO CONVERTING ' 1' TO 'NY'.                    
03860                                                                   
03861      MOVE AT-PTC-SW              TO  HRESAHPO.                    
03862      INSPECT HRESAHPO CONVERTING ' 1' TO 'NY'.                    
03863                                                                   
03864      MOVE AT-IBNR-SW             TO  HRESIBNO.                    
03865      INSPECT HRESIBNO CONVERTING ' 1' TO 'NY'.                    
03866                                                                   
03867      MOVE AT-PTC-LF-SW           TO  HRESLFPO.                    
03868      INSPECT HRESLFPO CONVERTING ' 1' TO 'NY'.                    
03869                                                                   
03870      MOVE AT-CURRENT-MANUAL-RESERVE  TO  HMANAMTO.                
03871      MOVE AT-FUTURE-RESERVE      TO  HFUTAMTO.                    
03872      MOVE AT-IBNR-RESERVE        TO  HIBNAMTO.                    
03873      MOVE AT-PAY-CURRENT-RESERVE TO  HPTCAMTO.                    
03874                                                                   
03875      MOVE AT-ITD-CHARGEABLE-EXPENSE TO HITDCO.                    
03876      MOVE AT-ITD-PAID-EXPENSES   TO  HITDNCO.                     
03877                                                                   
03878      MOVE AT-CDT-ACCESS-METHOD   TO  HCDTAMO.                     
03879      MOVE AT-PERCENT-OF-CDT      TO  HPCTCDTO.                    
03880                                                                   
03881      MOVE AT-EXPENSE-METHOD      TO  HEXPO.                       
03882                                                                   
03883      IF AT-EXPENSE-METHOD = '2' OR '4'                            
03884          MOVE AT-EXPENSE-DOLLAR  TO  HEXPAMTO                     
03885      ELSE                                                         
03886          IF AT-EXPENSE-METHOD = '3'                               
03887              MOVE AT-EXPENSE-PERCENT  TO  HEXPAMTO                
03888          ELSE                                                     
03889              MOVE ZERO           TO  HEXPAMTO.                    
03890                                                                   
03891      MOVE -1                     TO  HMAINTL.                     
03892                                                                   
03893      MOVE +1                     TO  WS-INDEX.                    
03894      SET EL142H-INDEX1                                            
03895          EL142H-INDEX2  TO  +1.                                   
03896                                                                   
03897  4110-MOVE-CAUSE.                                                 
03898      MOVE AT-OPEN-CLOSE-DATE (WS-INDEX)  TO  DC-BIN-DATE-1.       
03899                                                                   
03900      IF DC-BIN-DATE-1 = LOW-VALUES OR SPACES                      
03901          GO TO 4120-BUMP-INDEX.                                   
03902                                                                   
03903      MOVE SPACES                 TO  DC-OPTION-CODE.              
03904      PERFORM 8500-DATE-CONVERSION.                                
03905      MOVE DC-GREG-DATE-1-EDIT    TO EL142H-DATE                   
03906                                     (EL142H-INDEX1  EL142H-INDEX2)
03907                                                                   
03908      MOVE AT-OPEN-CLOSE-TYPE (WS-INDEX) TO EL142H-OC              
03909                                     (EL142H-INDEX1  EL142H-INDEX2)
03910                                                                   
03911      MOVE AT-OPEN-CLOSE-REASON (WS-INDEX) TO EL142H-CAUSE         
03912                                    (EL142H-INDEX1  EL142H-INDEX2).
03913                                                                   
03914  4120-BUMP-INDEX.                                                 
03915      IF WS-INDEX LESS +6                                          
03916          ADD +1                  TO  WS-INDEX                     
03917      ELSE                                                         
03918          GO TO 4100-READNEXT.                                     
03919                                                                   
03920      IF EL142H-INDEX1 LESS +5                                     
03921          SET EL142H-INDEX1 UP BY +1                               
03922          GO TO 4110-MOVE-CAUSE.                                   
03923                                                                   
03924      IF EL142H-INDEX2 LESS +2                                     
03925          SET EL142H-INDEX1 TO +1                                  
03926          SET EL142H-INDEX2 UP BY +1                               
03927          GO TO 4110-MOVE-CAUSE.                                   
03928                                                                   
03929      GO TO 4100-READNEXT.                                         
03930                                                                   
03931      EJECT                                                        
03932  4200-PAYMENT-TRAILER.                                            
03933      IF AT-TRAILER-TYPE NOT = '2'                                 
03934          GO TO 4300-AUTO-PAY-TRAILER.                             
03935                                                                   
03936      MOVE EL142B                 TO  PI-MAP-NAME.                 
03937                                                                   
03938      MOVE AT-TRAILER-TYPE        TO  BTLRTYPO.                    
03939      MOVE AT-SEQUENCE-NO         TO  BSEQO.                       
03940                                                                   
03941      MOVE SPACES                 TO  DC-OPTION-CODE.              
03942      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               
03943      PERFORM 8500-DATE-CONVERSION.                                
03944      MOVE DC-GREG-DATE-1-EDIT    TO  BRECDTEO.                    
03945                                                                   
03946      MOVE AT-RECORDED-BY         TO  BBYO.                        
03947                                                                   
03948      MOVE AT-PAYMENT-LAST-UPDATED-BY TO  BMANTBYO.                
03949      MOVE AT-PAYMENT-LAST-MAINT-DT   TO  DC-BIN-DATE-1.           
03950      MOVE ' '                        TO  DC-OPTION-CODE.          
03951      PERFORM 8500-DATE-CONVERSION.                                
03952      IF NO-CONVERSION-ERROR                                       
03953          MOVE DC-GREG-DATE-1-EDIT    TO  BMANTONO                 
03954      ELSE                                                         
03955          MOVE SPACES                 TO  BMANTONO.                
03956                                                                   
03957      MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.                 
03958      MOVE TIME-OUT                   TO  BMANTATO.                
03959                                                                   
03960      MOVE AT-CHECK-NO            TO  BCKNOO.
013017     if at-ach-payment = 'Y'
013017        move 'YES'               to bachpmto
013017        go to 4200-ach-cashed-dt
013017     else
013017        move 'NO '               to bachpmto
013017     end-if
021114
021114**** This routine will connect to the Logic Database on SQL Server
021114**** and call a stored procedure to determine the check cashed date
021114
021114     PERFORM 7000-CONNECT-TO-DB  THRU 7000-EXIT
021114     IF SQLCODE = 0
021114        PERFORM 7100-GET-CHK-CASHED-DT  THRU 7100-EXIT
111714        if sqlcode = zeros
111714           move ws-check-cashed-dt (3:2)
111714                                 to dc-ymd-year
111714           move ws-check-cashed-dt (6:2)
111714                                 to dc-ymd-month
111714           move ws-check-cashed-dt (9:2)
111714                                 to dc-ymd-day
091714        else
091714           perform 7110-check-manual thru 7110-exit
021114           IF SQLCODE = 0
081214              move ws-check-cashed-dt (7:2)
081214                                 to dc-ymd-year
081214              move ws-check-cashed-dt (1:2)
081214                                 to dc-ymd-month
081214              move ws-check-cashed-dt (4:2)
081214                                 to dc-ymd-day
091714           end-if
111714        end-if
111714        if sqlcode = zeros
081214           move '3' to dc-option-code
081214           perform 8500-date-conversion
081214           if (no-conversion-error)
081214              and (dc-bin-date-1 > at-check-written-dt)
                    move dc-greg-date-1-edit to bcashedo
021114*             MOVE WS-CHECK-CASHED-DT TO BCASHEDO
081214           else
081214              move spaces        to bcashedo
081214           end-if
021114        END-IF
021114     END-IF
021114     PERFORM 7200-DISCONNECT THRU 7200-EXIT
           go to 4200-carry-on-my-waward-son

013017     .
       4200-ach-cashed-dt.

           MOVE AT-CONTROL-PRIMARY     TO WS-CLAIM-KEY

           EXEC CICS READ
                DATASET (WS-CLAIM-MASTER-DSID)
                RIDFLD  (WS-CLAIM-KEY)
                SET     (ADDRESS OF CLAIM-MASTER)
           END-EXEC

           move spaces                 to ws-check-cashed-dt
           move zeros                  to sqlcode

           PERFORM 7000-CONNECT-TO-DB  THRU 7000-EXIT
           IF SQLCODE <> 0
              display ' bad connect ' sqlcode
              go to 4200-carry-on-my-waward-son
           end-if

           move at-carrier             to ws-carrier
           move cl-cert-state          to ws-state
           move cl-cert-account        to ws-account-no
           move at-cert-no             to ws-cert-no
           move at-claim-no            to ws-claim-no
           move zeros                  to ws-check-no
           move at-check-no            to ws-check-no (4:7)

           if pi-company-id = 'CID'
              exec sql
                 SELECT
                    CASHED_DATE
                 INTO
                    :ws-check-cashed-dt
                 FROM
                    CLM_PMTS_ACH
                 WHERE
                        CARRIER   = :ws-carrier
                    and STATE     = :ws-state
                    and ACCOUNT   = :ws-account-no
                    and CERT_NO   = :ws-cert-no
                    and CLAIM_NO  = :ws-claim-no
                    and CHECK_NO  = :ws-check-no
              end-exec
           else
              exec sql
                 SELECT
                    CASHED_DATE
                 INTO
                    :ws-check-cashed-dt
                 FROM
                    DCC_CLM_PMTS_ACH
                 WHERE
                        CARRIER   = :ws-carrier
                    and STATE     = :ws-state
                    and ACCOUNT   = :ws-account-no
                    and CERT_NO   = :ws-cert-no
                    and CLAIM_NO  = :ws-claim-no
                    and CHECK_NO  = :ws-check-no
              end-exec
           end-if

           if sqlcode not = 0 and 1
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display "Error: cannot read row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           else
              move ws-check-cashed-dt (3:2)
                                       to dc-ymd-year
              move ws-check-cashed-dt (6:2)
                                       to dc-ymd-month
              move ws-check-cashed-dt (9:2)
                                       to dc-ymd-day
              move '3'                 to dc-option-code
              perform 8500-date-conversion
              if (no-conversion-error)
                 and (dc-bin-date-1 > at-check-written-dt)
                 move dc-greg-date-1-edit
                                       to bcashedo
              else
                 move spaces           to bcashedo
              end-if
           end-if

           PERFORM 7200-DISCONNECT THRU 7200-EXIT

           .
013017 4200-carry-on-my-waward-son.

03962      IF OFFLINE-PMT                                               
03963         MOVE AL-UANOF            TO BCKNOA                        
03964        ELSE                                                       
03965         MOVE AL-SANOF            TO BCKNOA.                       
03966                                                                   
03967      IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES                      
03968          MOVE SPACES             TO  DC-OPTION-CODE               
03969          MOVE AT-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1               
03970          PERFORM 8500-DATE-CONVERSION                             
03971          MOVE DC-GREG-DATE-1-EDIT TO  BDTWRITO.                   
03972                                                                   
03973      MOVE AT-RECORDED-BY         TO  BWRITBYO.                    
031808     MOVE AT-PMT-APPROVED-BY     TO  BAPPVBYO.
03974      MOVE AT-AMOUNT-PAID         TO  BAMTO.                       
03975                                                                   
03976      IF AT-PAID-THRU-DT NOT = LOW-VALUES                          
03977         MOVE SPACES             TO  DC-OPTION-CODE                
03978         MOVE AT-PAID-THRU-DT    TO  DC-BIN-DATE-1                 
03979         PERFORM 8500-DATE-CONVERSION                              
03980         MOVE DC-GREG-DATE-1-EDIT TO  BPDTHRUO                     
03981         IF PI-USES-PAID-TO                                        
03982            MOVE '6'                TO  DC-OPTION-CODE             
03983            MOVE AT-PAID-THRU-DT    TO  DC-BIN-DATE-1              
03984            MOVE +1                 TO  DC-ELAPSED-DAYS            
03985            MOVE +0                 TO  DC-ELAPSED-MONTHS          
03986            PERFORM 8500-DATE-CONVERSION                           
03987            MOVE DC-GREG-DATE-1-EDIT TO  BPDTHRUO.                 
03988                                                                   
03989      IF AT-TO-BE-WRITTEN-DT NOT = LOW-VALUES AND SPACES           
03990          MOVE SPACES             TO  DC-OPTION-CODE               
03991          MOVE AT-TO-BE-WRITTEN-DT TO  DC-BIN-DATE-1               
03992          PERFORM 8500-DATE-CONVERSION                             
03993          MOVE DC-GREG-DATE-1-EDIT TO  BHOLDATO.                   
03994                                                                   
03995      MOVE AT-DAYS-IN-PERIOD      TO  BDAYSPDO
020413     IF AT-PRINT-EOB-WITH-CHECK = 'Y' OR 'S'
020413        MOVE AT-PRINT-EOB-WITH-CHECK TO BEOBYNO
           ELSE
              MOVE 'N'                 TO BEOBYNO
           END-IF
020413     IF AT-PRINT-CLM-FORM = 'N'
020413         MOVE 'N'                TO BCLMYNO
020413     ELSE
020413         MOVE 'Y'                TO BCLMYNO
020413     END-IF           
020413     IF AT-PRINT-SURVEY = 'N'
020413         MOVE 'N'                TO BSRVYNO
020413     ELSE
020413         MOVE 'Y'                TO BSRVYNO
020413     END-IF           
102413     IF AT-SPECIAL-RELEASE = 'Y'
102413         MOVE 'Y'                TO BSPRELO
102413     ELSE
102413         MOVE 'N'                TO BSPRELO
102413     END-IF           
           IF AT-CHECK-WRITTEN-DT = LOW-VALUES
020413      AND AT-VOID-DT = LOW-VALUES
              MOVE AL-UANON            TO BEOBYNA
020413        MOVE AL-UANON            TO BCLMYNA
102413        MOVE AL-UANON            TO BSPRELA
111113        IF PI-APPROVAL-LEVEL = '4' OR '5'
020413           MOVE AL-UANON         TO BSRVYNA
020413        END-IF
           END-IF
082807     IF AT-PAYMENT-TYPE = 'I'
082807        IF AT-INT-RATE NOT NUMERIC
082807           MOVE ZEROS            TO AT-INT-RATE
082807        END-IF
082807        MOVE AT-INT-RATE         TO  BDAYRATO
082807        MOVE 'INTEREST RT  -'    TO  BRATHDO
082807     ELSE
03996         MOVE AT-DAILY-RATE       TO  BDAYRATO
082807     END-IF
03997                                                                   
03998      IF AT-CV-PMT-CODE = ' '                                      
03999          GO TO 4200-DISPLAY-PMT-DESC.                             
04000                                                                   
04001      IF AT-CV-PMT-CODE = '1'                                      
04002          MOVE 'FULL DEATH'               TO  BPAYTYPO.            
04003                                                                   
04004      IF AT-CV-PMT-CODE = '2'                                      
04005          MOVE 'HALF DEATH'               TO  BPAYTYPO.            
04006                                                                   
04007      IF AT-CV-PMT-CODE = '3'                                      
04008          MOVE 'FULL AD&D'                TO  BPAYTYPO.            
04009                                                                   
04010      IF AT-CV-PMT-CODE = '4'                                      
04011          MOVE 'HALF AD&D'                TO  BPAYTYPO.            
04012                                                                   
04013      IF AT-CV-PMT-CODE = '5'                                      
04014          MOVE 'FULL RIDER'               TO  BPAYTYPO.            
04015                                                                   
04016      IF AT-CV-PMT-CODE = '6'                                      
04017          MOVE 'HALF RIDER'               TO  BPAYTYPO.            
04018                                                                   
04019      IF AT-CV-PMT-CODE = '7'                                      
04020          MOVE 'NON-CHG EXP'              TO  BPAYTYPO.            
04021                                                                   
04022      IF AT-CV-PMT-CODE = '8'                                      
04023          MOVE 'ADDITIONAL'               TO  BPAYTYPO.            
04024                                                                   
04025      GO TO 4200-PAYMENT-TRAILER-CONT.                             
04026                                                                   
04027  4200-DISPLAY-PMT-DESC.                                           
04028                                                                   
022106     EVALUATE AT-PAYMENT-TYPE
022106        WHEN '1'
022106           MOVE 'PARTIAL PAYMENT'        TO BPAYTYPO
022106        WHEN '2'
022106           MOVE 'FINAL PAYMENT'          TO BPAYTYPO
022106        WHEN '3'
022106           MOVE 'LUMP SUM PAYMENT'       TO BPAYTYPO
022106        WHEN '4'
022106           MOVE 'ADDITIONAL PAYMENT'     TO BPAYTYPO
022106        WHEN '5'
022106           MOVE 'CHARGEABLE PAYMENT'     TO BPAYTYPO
022106        WHEN '6'
022106           MOVE 'NON-CHARGEABLE PAYMENT' TO BPAYTYPO
022106        WHEN '7'
022106           MOVE 'LIFE PREMIUM REFUND'    TO BPAYTYPO
022106        WHEN '8'
022106           MOVE 'A & H PREMIUM REFUND'   TO BPAYTYPO
022106        WHEN 'I'
022106           MOVE 'INTEREST PAYMENT   '    TO BPAYTYPO
022106        WHEN OTHER
022106           MOVE 'ENTRY CORRECTION'       TO BPAYTYPO
022106     END-EVALUATE

           .
04055  4200-PAYMENT-TRAILER-CONT.                                       
04056                                                                   
04057      MOVE AT-FORCE-CONTROL       TO  BFORCEDO.                    
04058      INSPECT BFORCEDO CONVERTING ' 1' TO 'NY'.                    
04059                                                                   
04060      IF AT-VOID-DT NOT = LOW-VALUES                               
04061          MOVE SPACES              TO  DC-OPTION-CODE              
04062          MOVE AT-VOID-DT          TO  DC-BIN-DATE-1               
04063          PERFORM 8500-DATE-CONVERSION                             
04064          MOVE DC-GREG-DATE-1-EDIT TO  BVOIDDTO.                   
04065                                                                           
052506     IF AT-PMT-PROOF-DT NOT = LOW-VALUES                               
052506         MOVE SPACES              TO  DC-OPTION-CODE  
052506         MOVE AT-PMT-PROOF-DT     TO  WS-PRF-DT            
052506         MOVE AT-PMT-PROOF-DT     TO  DC-BIN-DATE-1               
052506         PERFORM 8500-DATE-CONVERSION                             
052506         MOVE DC-GREG-DATE-1-EDIT TO  BPRFDTI.                   
052506         
04066      IF AT-PAYEE-TYPE EQUAL 'I'                                   
04067          MOVE 'INSURED'          TO  BPAYEEO                      
04068        ELSE                                                       
04069      IF AT-PAYEE-TYPE EQUAL 'B'                                   
04070          MOVE 'BENEFICIARY'      TO  BPAYEEO                      
04071        ELSE                                                       
04072      IF AT-PAYEE-TYPE EQUAL 'A'                                   
04073          MOVE 'ACCOUNT'          TO  BPAYEEO                      
04074        ELSE                                                       
04075      IF AT-PAYEE-TYPE EQUAL 'O'                                   
04076          MOVE 'OTHER 1'          TO  BPAYEEO                      
04077        ELSE                                                       
04078      IF AT-PAYEE-TYPE EQUAL 'Q'                                   
04079          MOVE 'REM BORR   '      TO  BPAYEEO                      
04080        ELSE                                                       
04081      IF AT-PAYEE-TYPE EQUAL 'P'                                   
04082          MOVE 'DOCTOR'           TO  BPAYEEO                      
04083        ELSE                                                       
04084      IF AT-PAYEE-TYPE EQUAL 'E'                                   
04085          MOVE 'EMPLOYER'         TO  BPAYEEO.                     
04086                                                                   
           
04087      MOVE AT-PAYEES-NAME         TO  BPNAMEO.                     
04088                                                                   
04089      MOVE AT-ADDL-RESERVE        TO  BRESERVO.                    
082807     IF AT-PAYMENT-TYPE NOT = 'I'
082807        MOVE AT-EXPENSE-PER-PMT  TO  BEXPO
082807     END-IF
04091                                                                   
04092      IF AT-PMT-SELECT-DT NOT = LOW-VALUES                         
04093          MOVE SPACES             TO  DC-OPTION-CODE               
04094          MOVE AT-PMT-SELECT-DT   TO  DC-BIN-DATE-1                
04095          PERFORM 8500-DATE-CONVERSION                             
04096          MOVE DC-GREG-DATE-1-EDIT TO  BCRSELO.                    
04097                                                                   
04098      IF AT-PMT-ACCEPT-DT NOT = LOW-VALUES                         
04099          MOVE SPACES             TO  DC-OPTION-CODE               
04100          MOVE AT-PMT-ACCEPT-DT   TO  DC-BIN-DATE-1                
04101          PERFORM 8500-DATE-CONVERSION                             
04102          MOVE DC-GREG-DATE-1-EDIT TO  BCRACPO.                    
04103                                                                   
04104      IF AT-VOID-SELECT-DT NOT = LOW-VALUES                        
04105          MOVE SPACES             TO  DC-OPTION-CODE               
04106          MOVE AT-VOID-SELECT-DT  TO  DC-BIN-DATE-1                
04107          PERFORM 8500-DATE-CONVERSION                             
04108          MOVE DC-GREG-DATE-1-EDIT TO  BVOIDSDO.                   
04109                                                                   
04110      IF AT-VOID-ACCEPT-DT NOT = LOW-VALUES                        
04111          MOVE SPACES             TO  DC-OPTION-CODE               
04112          MOVE AT-VOID-ACCEPT-DT  TO  DC-BIN-DATE-1                
04113          PERFORM 8500-DATE-CONVERSION                             
04114          MOVE DC-GREG-DATE-1-EDIT TO  BVOIDACO.                   
04115                                                                   
04116      IF AT-PAYMENT-ORIGIN = '1'                                   
04117          MOVE 'ON-LINE'          TO  BORIGINO                     
04118        ELSE                                                       
04119      IF AT-PAYMENT-ORIGIN = '2'                                   
04120          MOVE 'AUTO PAY'         TO  BORIGINO                     
04121        ELSE                                                       
04122      IF AT-PAYMENT-ORIGIN = '3'                                   
04123          MOVE 'MANUAL'           TO  BORIGINO                     
04124        ELSE                                                       
04125          MOVE AT-PAYMENT-ORIGIN  TO  BORIGINO.                    
04126                                                                   
04127      IF AT-EXPENSE-TYPE = SPACE                                   
04128          MOVE AL-SADOF           TO BEXPHDGA                      
04129          MOVE AL-SANOF           TO BEXPTYPA                      
04130      ELSE                                                         
04131          MOVE AT-EXPENSE-TYPE    TO BEXPTYPO                      
04132          MOVE AL-SANOF           TO BEXPHDGA                      
04133          MOVE AL-UANOF           TO BEXPTYPA.                     
04134                                                                   
04135      MOVE AT-CHECK-QUE-CONTROL   TO  BCKQUEO.                     
04136      MOVE AT-CHECK-QUE-SEQUENCE  TO  BCKSEQO.                     
04137                                                                   
04138      IF PI-PROCESSOR-ID = 'LGXX'                                  
04139          MOVE AL-UNNOF           TO  BCKQUEA  BCKSEQA             
04140                                      BCRSELA  BVOIDSDA            
04141          MOVE AL-UANOF           TO  BPAYEEA  BEXPTYPA            
04142                                      BCRACPA  BHOLDATA            
04143                                      BVOIDACA                     
04144      ELSE                                                         
040819         MOVE AL-SANOF           TO  BHOLDATA
04145          IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES 
020413          OR AT-VOID-DT NOT = LOW-VALUES                 
040819             MOVE AL-SANOF       TO  BPAYEEA
04147          ELSE                                                     
040819             MOVE AL-UANOF       TO  BPAYEEA.
04149                                                                   
04150      IF (PI-COMPANY-ID = 'LAP' OR 'RMC') OR                       
04151         (PI-PROCESSOR-ID = 'LGXX')                                
04152          MOVE AL-UANOF           TO  BPMTORGA.                    
04153                                                                   
04154      MOVE -1                     TO  BMAINTL.                     
04155                                                                   
04156      IF AT-PAYMENT-NOTE-SEQ-NO = 0                                
04157          GO TO 4100-READNEXT.                                     
04158                                                                   
04159  4200-READ-PAYMENT-NOTE-TRLR.                                     
04160                                                                   
04161      EXEC CICS HANDLE CONDITION                                   
04162          NOTFND   (4200-NOTE-TRLR-NOTFND)                         
04163      END-EXEC.                                                    
04164                                                                   
04165      MOVE PI-ACTIVITY-TRAILERS-KEY   TO                           
04166                                 PI-PREV-ACTIVITY-TRAILERS-KEY.    
04167                                                                   
04168      MOVE AT-PAYMENT-NOTE-SEQ-NO TO  PI-ATK-SEQUENCE-NO.          
04169                                                                   
04170      EXEC CICS READ                                               
04171          DATASET   (WS-ACTIVITY-TRAILERS-DSID)                    
04172          RIDFLD    (PI-ACTIVITY-TRAILERS-KEY)                     
04173          SET       (ADDRESS OF ACTIVITY-TRAILERS)                 
04174      END-EXEC.                                                    
04175                                                                   
04176      IF AT-PAYMENT-NOTE                                           
04177          MOVE AT-INFO-LINE-1     TO  BNOTE1O                      
04178          MOVE AT-INFO-LINE-2     TO  BNOTE2O
               IF AT-EOB-CODES-PRESENT
                  MOVE AL-SANOF        TO  BNOTE2A
               END-IF
04179      ELSE                                                         
04180          MOVE SPACES             TO  BNOTE1O                      
04181                                      BNOTE2O.                     
04182                                                                   
04183      MOVE PI-PREV-ACTIVITY-TRAILERS-KEY  TO                       
04184                                      PI-ACTIVITY-TRAILERS-KEY.    
04185                                                                   
04186      GO TO 4100-READNEXT.                                         
04187                                                                   
04188  4200-NOTE-TRLR-NOTFND.                                           
04189                                                                   
04190      MOVE PI-PREV-ACTIVITY-TRAILERS-KEY  TO                       
04191                                      PI-ACTIVITY-TRAILERS-KEY.    
04192                                                                   
04193      GO TO 4100-READNEXT.                                         
04194                                                                   
04195      EJECT                                                        
04196  4300-AUTO-PAY-TRAILER.                                           
04197      IF AT-TRAILER-TYPE NOT = '3'                                 
04198          GO TO 4400-CORRESPONDENCE-TRAILER.                       
04199                                                                   
04200      MOVE EL142C                 TO  PI-MAP-NAME.                 
04201                                                                   
04202      MOVE AT-TRAILER-TYPE        TO  CTLRTYPO.                    
04203      MOVE AT-SEQUENCE-NO         TO  CSEQO.                       
04204                                                                   
04205      MOVE SPACES                 TO  DC-OPTION-CODE.              
04206      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               
04207      PERFORM 8500-DATE-CONVERSION.                                
04208      MOVE DC-GREG-DATE-1-EDIT    TO  CRECDTEO.                    
04209                                                                   
04210      MOVE AT-RECORDED-BY         TO  CBYO.                        
04211                                                                   
04212      MOVE AT-AUTO-PAY-LAST-MAINT-DT  TO  DC-BIN-DATE-1.           
04213      MOVE ' '                        TO  DC-OPTION-CODE.          
04214      PERFORM 8500-DATE-CONVERSION.                                
04215      IF NO-CONVERSION-ERROR                                       
04216          MOVE DC-GREG-DATE-1-EDIT    TO  CMANTONO                 
04217      ELSE                                                         
04218          MOVE SPACES                 TO  CMANTONO.                
04219                                                                   
04220      MOVE AT-AUTO-PAY-LAST-UPDATED-BY    TO  CMANTBYO.            
04221                                                                   
04222      MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.                 
04223      MOVE TIME-OUT                   TO  CMANTATO.                
04224                                                                   
04225      IF AT-SCHEDULE-START-DT NOT = LOW-VALUES                     
04226          MOVE SPACES             TO  DC-OPTION-CODE               
04227          MOVE AT-SCHEDULE-START-DT   TO  DC-BIN-DATE-1            
04228          PERFORM 8500-DATE-CONVERSION                             
04229          MOVE DC-GREG-DATE-1-EDIT    TO  CEFFDTEO.                
04230                                                                   
04231      IF AT-TERMINATED-DT NOT = LOW-VALUES                         
04232          MOVE SPACES             TO  DC-OPTION-CODE               
04233          MOVE AT-TERMINATED-DT   TO  DC-BIN-DATE-1                
04234          PERFORM 8500-DATE-CONVERSION                             
04235          MOVE DC-GREG-DATE-1-EDIT TO  CREPDTEO.                   
04236                                                                   
04237      MOVE AT-FIRST-PMT-AMT       TO  C1STPAO.                     
04238      MOVE AT-REGULAR-PMT-AMT     TO  CREGPAO.                     
04239                                                                   
04240      IF AT-1ST-PAY-THRU-DT NOT = LOW-VALUES                       
04241          IF PI-USES-PAID-TO                                       
04242              MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1        
04243              MOVE '6'                    TO  DC-OPTION-CODE       
04244              MOVE +1                     TO  DC-ELAPSED-DAYS      
04245              MOVE +0                     TO  DC-ELAPSED-MONTHS    
04246              PERFORM 8500-DATE-CONVERSION                         
04247              IF NO-CONVERSION-ERROR                               
04248                  MOVE DC-GREG-DATE-1-EDIT TO C1STPSO              
04249              ELSE                                                 
04250                  MOVE LOW-VALUES         TO  C1STPSO              
04251          ELSE                                                     
04252              MOVE SPACES                 TO  DC-OPTION-CODE       
04253              MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1        
04254              MOVE +0                     TO  DC-ELAPSED-DAYS      
04255                                              DC-ELAPSED-MONTHS    
04256              PERFORM 8500-DATE-CONVERSION                         
04257              IF NO-CONVERSION-ERROR                               
04258                  MOVE DC-GREG-DATE-1-EDIT TO C1STPSO              
04259              ELSE                                                 
04260                  MOVE SPACES             TO  C1STPSO.             
04261                                                                   
04262      IF AT-SCHEDULE-END-DT NOT = LOW-VALUES                       
04263          IF PI-USES-PAID-TO                                       
04264              MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1        
04265              MOVE '6'                    TO  DC-OPTION-CODE       
04266              MOVE +1                     TO  DC-ELAPSED-DAYS      
04267              MOVE +0                     TO  DC-ELAPSED-MONTHS    
04268              PERFORM 8500-DATE-CONVERSION                         
04269              IF NO-CONVERSION-ERROR                               
04270                  MOVE DC-GREG-DATE-1-EDIT TO CLSTPSO              
04271              ELSE                                                 
04272                  MOVE LOW-VALUES         TO  CLSTPSO              
04273          ELSE                                                     
04274              MOVE SPACES                 TO  DC-OPTION-CODE       
04275              MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1        
04276              MOVE +0                     TO  DC-ELAPSED-DAYS      
04277                                              DC-ELAPSED-MONTHS    
04278              PERFORM 8500-DATE-CONVERSION                         
04279              IF NO-CONVERSION-ERROR                               
04280                  MOVE DC-GREG-DATE-1-EDIT TO CLSTPSO              
04281              ELSE                                                 
04282                  MOVE LOW-VALUES         TO  CLSTPSO.             
04283                                                                   
04284      MOVE AT-DAYS-IN-1ST-PMT     TO  CDIFPO.                      
04285      MOVE AT-INTERVAL-MONTHS     TO  CMBPAYO.                     
04286                                                                   
04287      IF AT-LAST-PMT-TYPE = 'P'                                    
04288          MOVE 'PARTIAL'          TO  CLSTPATO                     
04289      ELSE                                                         
04290          IF AT-LAST-PMT-TYPE = 'F'                                
04291              MOVE 'FINAL  '      TO  CLSTPATO.                    
04292                                                                   
04293      IF AT-AUTO-PAYEE-TYPE EQUAL 'I'                              
04294          MOVE 'INSURED'          TO  CPAYEEO                      
04295        ELSE                                                       
04296      IF AT-AUTO-PAYEE-TYPE EQUAL 'B'                              
04297          MOVE 'BENEFICIARY'      TO  CPAYEEO                      
04298        ELSE                                                       
04299      IF AT-AUTO-PAYEE-TYPE EQUAL 'A'                              
04300          MOVE 'ACCOUNT'          TO  CPAYEEO                      
04301        ELSE                                                       
04302      IF AT-AUTO-PAYEE-TYPE EQUAL 'O'                              
04303          MOVE 'OTHER 1'          TO  CPAYEEO                      
04304        ELSE                                                       
04305          MOVE 'REM BORR'         TO  CPAYEEO.   
070909
070909     MOVE AT-AUTO-END-LETTER     TO  CENDLETO.                  
04306                                                                   
04307      MOVE -1                     TO  CPFKL.                       
04308                                                                   
04309      GO TO 4100-READNEXT.                                         
04310                                                                   
04311      EJECT                                                        
04312  4400-CORRESPONDENCE-TRAILER.                                     
04313      IF AT-TRAILER-TYPE NOT = '4'                                 
04314          GO TO 4500-GENERAL-INFO-TRAILER.                         
041613
041613     MOVE PI-COMPANY-CD          TO  WS-NA-COMPANY-CD.
041613     MOVE PI-SAVE-ATK-CARRIER    TO  WS-NA-CARRIER.
041613     MOVE PI-SAVE-ATK-CLAIM-NO   TO  WS-NA-CLAIM-NO.
041613     MOVE PI-SAVE-ATK-CERT-NO    TO  WS-NA-CERT-NO.
041613     MOVE AT-LETTER-ARCHIVE-NO   TO  WS-NA-ARCHIVE-NO.
041613
041613     EXEC CICS READ
041613          DATASET (WS-NAPERSOFT-DSID)
041613          RIDFLD  (WS-NAPERSOFT-KEY)
041613          SET     (ADDRESS OF NAPERSOFT-FILE)
041613          RESP    (WS-RESPONSE)
041613     END-EXEC.
041613
041613     IF RESP-NORMAL
041613          MOVE NA-ENCLOSURE-CD TO DENCCODO
041613                                  PI-ENC-CODE
041613          MOVE NA-CREATED-IN-NAPERSOFT TO 
041613                                  PI-CREATED-IN-NAPERSOFT
041613     ELSE
041613          MOVE SPACES TO DENCCODO
041613                         PI-ENC-CODE
041613                         PI-CREATED-IN-NAPERSOFT
041613     END-IF.
04315                                                                   
04316      MOVE AT-CONTROL-PRIMARY     TO  WS-CLAIM-KEY.                
04317                                                                   
04318      EXEC CICS READ                                               
04319          DATASET   (WS-CLAIM-MASTER-DSID)                         
04320          RIDFLD    (WS-CLAIM-KEY)                                 
04321          SET       (ADDRESS OF CLAIM-MASTER)                      
04322      END-EXEC.                                                    
04323                                                                   
04324      EXEC CICS HANDLE CONDITION                                   
04325          NOTFND (4410-ADDRESS-NOT-FOUND)                          
04326      END-EXEC.                                                    
04327                                                                   
04328      MOVE EL142D                 TO  PI-MAP-NAME.                 
04329                                                                   
04330      MOVE +2                     TO  EMI-NUMBER-OF-LINES          
04331                                      EMI-SWITCH2.                 
04332                                                                   
04333      MOVE AT-TRAILER-TYPE        TO  DTLRTYPO.                    
04334      MOVE AT-SEQUENCE-NO         TO  DSEQO.                       
04335                                                                   
04336      MOVE SPACES                 TO  DC-OPTION-CODE.              
04337      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               
04338      PERFORM 8500-DATE-CONVERSION.                                
04339      MOVE DC-GREG-DATE-1-EDIT    TO  DRECDTEO.                    
04340                                                                   
04341      MOVE AT-RECORDED-BY         TO  DBYO.                        
04342                                                                   
04343      MOVE AT-CORR-LAST-MAINT-DT      TO  DC-BIN-DATE-1.           
04344      MOVE ' '                        TO  DC-OPTION-CODE.          
04345      PERFORM 8500-DATE-CONVERSION.                                
04346      IF NO-CONVERSION-ERROR                                       
04347          MOVE DC-GREG-DATE-1-EDIT    TO  DMANTONO                 
04348      ELSE                                                         
04349          MOVE SPACES                 TO  DMANTONO.                
04350                                                                   
04351      MOVE AT-CORR-LAST-UPDATED-BY    TO  DMANTBYO.                
04352                                                                   
04353      MOVE AT-LAST-MAINT-HHMMSS   TO  TIME-IN.                     
04354      MOVE TIME-OUT               TO  DMANTATO.                    
04355                                                                   
04356      MOVE AT-STD-LETTER-FORM     TO  DFORMNOO.                    
04357      MOVE AT-LETTER-ARCHIVE-NO   TO  DARCHNOO.                    
050110     MOVE AT-RESEND-LETTER-FORM  TO  DRESFRMO.
050110     MOVE AT-AUTO-CLOSE-IND      TO  DAUTOCLO.
050110     IF AT-LETTER-TO-BENE EQUAL 'Y'
050110         MOVE 'LETTER SENT TO BENEFICIARY' TO DBENLETO
050110     ELSE
050110         MOVE LOW-VALUES TO DBENLETO
050110     END-IF.
04358                                                                   
04359      IF AT-LETTER-ARCHIVE-NO = ZEROS                              
04360         MOVE AL-UANON TO DFORMNOA.                                
04361                                                                   
04362      IF AT-LETTER-SENT-DT NOT = LOW-VALUES                        
04363          MOVE SPACES             TO  DC-OPTION-CODE               
04364          MOVE AT-LETTER-SENT-DT  TO  DC-BIN-DATE-1                
04365          PERFORM 8500-DATE-CONVERSION                             
04366          MOVE DC-GREG-DATE-1-EDIT TO  DDTSENTI.                   
04367                                                                   
04368      IF AT-AUTO-RE-SEND-DT NOT = LOW-VALUES                       
04369          MOVE SPACES             TO  DC-OPTION-CODE               
04370          MOVE AT-AUTO-RE-SEND-DT TO  DC-BIN-DATE-1                
04371          PERFORM 8500-DATE-CONVERSION                             
04372          MOVE DC-GREG-DATE-1-EDIT TO  DRESENDI.                   
04373                                                                   
04374      IF AT-RECEIPT-FOLLOW-UP NOT = LOW-VALUES                     
04375          MOVE SPACES             TO  DC-OPTION-CODE               
04376          MOVE AT-RECEIPT-FOLLOW-UP TO  DC-BIN-DATE-1              
04377          PERFORM 8500-DATE-CONVERSION                             
04378          MOVE DC-GREG-DATE-1-EDIT TO  DREPLYI.                    
04379                                                                   
04380      IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES                    
04381          MOVE SPACES             TO  DC-OPTION-CODE               
04382          MOVE AT-LETTER-ANSWERED-DT TO  DC-BIN-DATE-1             
04383          PERFORM 8500-DATE-CONVERSION                             
04384          MOVE DC-GREG-DATE-1-EDIT TO  DRECEVEI.                   
04385                                                                   
04386      IF AT-INITIAL-PRINT-DATE NOT = LOW-VALUES                    
04387          MOVE SPACES             TO  DC-OPTION-CODE               
04388          MOVE AT-INITIAL-PRINT-DATE  TO  DC-BIN-DATE-1            
04389          PERFORM 8500-DATE-CONVERSION                             
04390          MOVE DC-GREG-DATE-1-EDIT TO  DINPRNTI.                   
04391                                                                   
04392      IF AT-RESEND-PRINT-DATE  NOT = LOW-VALUES                    
04393          MOVE SPACES             TO  DC-OPTION-CODE               
04394          MOVE AT-RESEND-PRINT-DATE   TO  DC-BIN-DATE-1            
04395          PERFORM 8500-DATE-CONVERSION                             
04396          MOVE DC-GREG-DATE-1-EDIT TO  DREPRNTI.                   
041613
041613     IF AT-INITIAL-PRINT-DATE NOT = LOW-VALUES AND SPACES
041613         MOVE AL-SANOF               TO DENCCODA
041613     END-IF.
100610
100610     IF AT-RESEND-PRINT-DATE NOT = LOW-VALUES
100610         MOVE AL-SANOF               TO DRECEVEA
102610                                        DRESENDA
102610                                        DREPLYA
102610                                        DSTOPLTA
100610     END-IF.
102610
102610     IF AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES
102610         MOVE SPACES           TO DC-OPTION-CODE
102610         MOVE AT-STOP-LETTER-DT TO DC-BIN-DATE-1
102610         PERFORM 8500-DATE-CONVERSION
102610         MOVE DC-GREG-DATE-1-EDIT TO DSTOPLTI
102610         MOVE AL-SANOF            TO DRECEVEA
102610                                     DRESENDA
102610                                     DREPLYA
041613                                     DENCCODA
102610     END-IF.
04397                                                                   
04398      IF PI-COMPANY-ID NOT = 'DMD'                                 
04399          GO TO 4400-CONTINUE.                                     
04400                                                                   
04401 ** DMD CODE START *****************                               
04402 *    MOVE AT-DMD-LETTER-STATUS       TO DMDLETSI.                 
04403 *                                                                 
04404 *    IF AT-DMD-LETTER-ONLINE                                      
04405 *        MOVE 'ONLINE'               TO DMDLETDO                  
04406 *      ELSE                                                       
04407 *    IF AT-DMD-LETTER-PURGED                                      
04408 *        MOVE 'PURGED'               TO DMDLETDO                  
04409 *      ELSE                                                       
04410 *    IF AT-DMD-LETTER-RELOADED                                    
04411 *        MOVE 'RELOADED'             TO DMDLETDO.                 
04412 *                                                                 
04413 *    IF AT-DMD-LETTER-PURGE-DT NOT = LOW-VALUES AND SPACES        
04414 *        MOVE SPACES                 TO DC-OPTION-CODE            
04415 *        MOVE AT-DMD-LETTER-PURGE-DT TO DC-BIN-DATE-1             
04416 *        PERFORM 8500-DATE-CONVERSION                             
04417 *        MOVE DC-GREG-DATE-1-EDIT    TO DMDPURDI.                 
04418 *                                                                 
04419 *    IF AT-DMD-LETTER-RELOAD-DT NOT = LOW-VALUES AND SPACES       
04420 *        MOVE SPACES                  TO DC-OPTION-CODE           
04421 *        MOVE AT-DMD-LETTER-RELOAD-DT TO DC-BIN-DATE-1            
04422 *        PERFORM 8500-DATE-CONVERSION                             
04423 *        MOVE DC-GREG-DATE-1-EDIT     TO DMDRELDI.                
04424 ** DMD CODE END *******************                               
04425                                                                   
04426  4400-CONTINUE.                                                   
04427      IF AT-LETTER-PURGED-DT NOT = LOW-VALUES AND SPACES           
04428          MOVE ' '                    TO  DC-OPTION-CODE           
04429          MOVE AT-LETTER-PURGED-DT    TO  DC-BIN-DATE-1            
04430          PERFORM 8500-DATE-CONVERSION                             
04431          MOVE DC-GREG-DATE-1-EDIT    TO  DPURGDTI                 
04432          MOVE AL-SABON               TO  DPURGHDA                 
04433      ELSE                                                         
04434          MOVE AL-SANON               TO  DPURGHDA.                
04435                                                                   
04436      MOVE AT-LETTER-ORIGIN       TO  DWRITENO.                    
04437      INSPECT DWRITENO CONVERTING '1234' TO 'NYNY'.                    
04438                                                                   
04439      MOVE AT-REASON-TEXT         TO  DREASONO.                    
04440                                                                   
04441      MOVE -1                     TO  DMAINTL.                     
04442                                                                   
04443      MOVE AT-ADDRESSEE-NAME      TO  DMAILTOO.                    
04444                                                                   
04445      IF AT-ADDRESEE-TYPE = SPACE                                  
04446          NEXT SENTENCE                                            
04447       ELSE                                                        
04448      IF AT-ADDRESEE-TYPE      = '3'   AND                         
04449         AT-ADDRESS-REC-SEQ-NO = ZERO  AND                         
04450         CL-SYSTEM-IDENTIFIER  = 'CR'                              
04451          MOVE SPACES             TO  WS-ACCOUNT-MASTER-KEY        
04452          MOVE PI-COMPANY-CD      TO  WS-AM-COMPANY-CD             
04453          MOVE PI-CARRIER         TO  WS-AM-CARRIER                
04454          MOVE PI-GROUPING        TO  WS-AM-GROUPING               
04455          MOVE PI-STATE           TO  WS-AM-STATE                  
04456          MOVE PI-ACCOUNT         TO  WS-AM-ACCOUNT                
04457          MOVE PI-CERT-EFF-DT     TO  WS-AM-EXPIRATION-DT          
04458          PERFORM 4420-FIND-ACCOUNT-MASTER THRU 4449-EXIT          
04459       ELSE                                                        
04460      IF AT-ADDRESEE-TYPE      = '3'   AND                         
04461         AT-ADDRESS-REC-SEQ-NO = ZERO  AND                         
04462         CL-SYSTEM-IDENTIFIER  = 'CV'                              
04463          MOVE SPACES             TO  WS-PRODUCER-MASTER-KEY       
04464          MOVE PI-COMPANY-CD      TO  WS-PD-COMPANY-CD             
04465          MOVE PI-CARRIER         TO  WS-PD-CARRIER                
04466          MOVE PI-GROUPING        TO  WS-PD-GROUPING               
04467          MOVE PI-STATE           TO  WS-PD-STATE                  
04468          MOVE PI-PRODUCER        TO  WS-PD-PRODUCER               
04469          MOVE PI-CERT-EFF-DT     TO  WS-PD-EXPIRATION-DT          
04470          PERFORM 4450-FIND-PRODUCER-MASTER THRU 4499-EXIT         
04471       ELSE                                                        
04472      IF AT-ADDRESS-REC-SEQ-NO NOT = ZERO                          
04473          MOVE PI-ACTIVITY-TRAILERS-KEY TO WS-ACTIVITY-TRAILERS-KEY
04474          MOVE AT-ADDRESS-REC-SEQ-NO    TO  WS-ATK-SEQUENCE-NO     
04475          EXEC CICS READ                                           
04476              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
04477              RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                   
04478              SET     (ADDRESS OF ACTIVITY-TRAILERS)               
04479          END-EXEC                                                 
04480          IF AT-MAIL-TO-NAME NOT = DMAILTOO                        
04481              MOVE ER-0384           TO  EMI-ERROR                 
04482              PERFORM 9900-ERROR-FORMAT                            
04483          ELSE                                                     
04484              MOVE AT-MAIL-TO-NAME        TO  DMAILTOO             
04485              MOVE AT-ADDRESS-LINE-1      TO  DADDR1O              
04486              MOVE AT-ADDRESS-LINE-2      TO  DADDR2O              
04487 *            MOVE AT-CITY-STATE          TO  DCITYSTO
                   STRING AT-CITY ' ' AT-STATE
                      DELIMITED BY '  ' INTO DCITYSTO
                   END-STRING
04488              MOVE AT-PHONE-NO            TO  DPHONEO              
04489              INSPECT DPHONEI CONVERTING SPACES TO '-'             
04490              IF AT-CANADIAN-POST-CODE                             
04491                  MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1    
04492                  MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2    
04493                  MOVE SPACES             TO WS-DASH-CAN           
04494                                             WS-CAN-FILLER         
04495                  MOVE WS-CANADIAN-POSTAL-CODES                    
04496                                          TO DZIPO                 
04497              ELSE                                                 
04498                  MOVE AT-ZIP-CODE        TO WS-ZIP-CODE           
04499                  IF AT-ZIP-PLUS4 = SPACES OR ZEROS                
04500                      MOVE SPACES         TO WS-ZIP-PLUS4          
04501                                             WS-DASH               
04502                      MOVE WS-ZIP         TO DZIPO                 
04503                  ELSE                                             
04504                      MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4          
04505                      MOVE '-'            TO WS-DASH               
04506                      MOVE WS-ZIP         TO DZIPO.                
04507                                                                   
04508      GO TO 4100-READNEXT.                                         
04509                                                                   
04510  4410-ADDRESS-NOT-FOUND.                                          
04511      MOVE ER-0388                TO  EMI-ERROR.                   
04512      PERFORM 9900-ERROR-FORMAT.                                   
04513                                                                   
04514      GO TO 4100-READNEXT.                                         
04515                                                                   
04516      EJECT                                                        
04517  4420-FIND-ACCOUNT-MASTER.                                        
04518      MOVE ZERO                   TO  WS-NOT-FOUND.                
04519                                                                   
04520      EXEC CICS HANDLE CONDITION                                   
04521          NOTFND (4440-NOT-FOUND)                                  
04522      END-EXEC.                                                    
04523                                                                   
04524      EXEC CICS STARTBR                                            
04525          DATASET   (WS-ACCOUNT-MASTER-DSID)                       
04526          RIDFLD    (WS-ACCOUNT-MASTER-KEY)                        
04527          GENERIC   EQUAL                                          
04528          KEYLENGTH (13)                                           
04529      END-EXEC.                                                    
04530                                                                   
04531      MOVE +1                     TO  WS-READNEXT-SW.              
04532                                                                   
04533  4425-READNEXT.                                                   
04534      EXEC CICS READNEXT                                           
04535          DATASET   (WS-ACCOUNT-MASTER-DSID)                       
04536          RIDFLD    (WS-ACCOUNT-MASTER-KEY)                        
04537          SET       (ADDRESS OF ACCOUNT-MASTER)                    
04538      END-EXEC.                                                    
04539                                                                   
04540      IF PI-COMPANY-CD = WS-AM-COMPANY-CD AND                      
04541         PI-CARRIER    = WS-AM-CARRIER    AND                      
04542         PI-GROUPING   = WS-AM-GROUPING   AND                      
04543         PI-STATE      = WS-AM-STATE      AND                      
04544         PI-ACCOUNT    = WS-AM-ACCOUNT                             
04545          NEXT SENTENCE                                            
04546        ELSE                                                       
04547          GO TO 4440-NOT-FOUND.                                    
04548                                                                   
04549      IF (PI-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT             
04550        AND PI-CERT-EFF-DT LESS THAN AM-EXPIRATION-DT)             
04551          IF AT-TRAILER-TYPE = '4'                                 
04552              MOVE AM-NAME        TO  DMAILTOO                     
04553              MOVE AM-PERSON      TO  DADDR1O                      
04554              MOVE AM-ADDRS       TO  DADDR2O                      
04555 *            MOVE AM-CITY        TO  DCITYSTO                     
                   STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
                      DELIMITED BY '  ' INTO DCITYSTO
                   END-STRING
04556              MOVE AM-TEL-NO      TO WS-WORK-PHONE                 
04557              INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'       
04558              MOVE WS-NUMERIC-PHONE TO DPHONEO                     
04559              INSPECT DPHONEI CONVERTING SPACES TO '-'             
04560              IF  AM-CANADIAN-POST-CODE                            
04561                  MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1    
04562                  MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2    
04563                  MOVE SPACES             TO WS-DASH-CAN           
04564                                             WS-CAN-FILLER         
04565                  MOVE WS-CANADIAN-POSTAL-CODES                    
04566                                          TO DZIPO                 
04567                  GO TO 4445-END-OF-BROWSE                         
04568              ELSE                                                 
04569                  MOVE AM-ZIP-PRIME       TO WS-ZIP-CODE           
04570                  IF AM-ZIP-PLUS4 = SPACES OR ZEROS                
04571                      MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH  
04572                      MOVE WS-ZIP         TO DZIPO                 
04573                      GO TO 4445-END-OF-BROWSE                     
04574                  ELSE                                             
04575                      MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4          
04576                      MOVE '-'            TO WS-DASH               
04577                      MOVE WS-ZIP         TO DZIPO                 
04578                      GO TO 4445-END-OF-BROWSE                     
04579          ELSE                                                     
04580              MOVE AM-NAME        TO  JMAILTOO                     
04581              MOVE AM-PERSON      TO  JADDR1O                      
04582              MOVE AM-ADDRS       TO  JADDR2O                      
04583 *            MOVE AM-CITY        TO  JCITYSTO                     
                   STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
                      DELIMITED BY '  ' INTO JCITYSTO
                   END-STRING
04584              MOVE AM-TEL-NO      TO WS-WORK-PHONE                 
04585              INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'       
04586              MOVE WS-NUMERIC-PHONE TO JPHONEO                     
04587              INSPECT JPHONEI CONVERTING SPACES TO '-'             
04588              IF  AM-CANADIAN-POST-CODE                            
04589                  MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1    
04590                  MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2    
04591                  MOVE SPACES             TO WS-DASH-CAN           
04592                                             WS-CAN-FILLER         
04593                  MOVE WS-CANADIAN-POSTAL-CODES                    
04594                                          TO JZIPO                 
04595                  GO TO 4445-END-OF-BROWSE                         
04596              ELSE                                                 
04597                  MOVE AM-ZIP-PRIME       TO WS-ZIP-CODE           
04598                  IF AM-ZIP-PLUS4 = SPACES OR ZEROS                
04599                      MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH  
04600                      MOVE WS-ZIP         TO JZIPO                 
04601                      GO TO 4445-END-OF-BROWSE                     
04602                  ELSE                                             
04603                      MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4          
04604                      MOVE '-'            TO WS-DASH               
04605                      MOVE WS-ZIP         TO JZIPO                 
04606                      GO TO 4445-END-OF-BROWSE.                    
04607                                                                   
04608      GO TO 4425-READNEXT.                                         
04609                                                                   
04610  4440-NOT-FOUND.                                                  
04611      MOVE ER-0198                TO  EMI-ERROR.                   
04612      PERFORM 9900-ERROR-FORMAT.                                   
04613                                                                   
04614  4445-END-OF-BROWSE.                                              
04615      IF WS-READNEXT-SW = +1                                       
04616          EXEC CICS ENDBR                                          
04617              DATASET (WS-ACCOUNT-MASTER-DSID)                     
04618          END-EXEC.                                                
04619                                                                   
04620  4449-EXIT.                                                       
04621      EXIT.                                                        
04622                                                                   
04623      EJECT                                                        
04624  4450-FIND-PRODUCER-MASTER.                                       
04625      MOVE ZERO                   TO  WS-NOT-FOUND.                
04626                                                                   
04627      EXEC CICS HANDLE CONDITION                                   
04628          NOTFND (4490-NOT-FOUND)                                  
04629      END-EXEC.                                                    
04630                                                                   
04631      EXEC CICS STARTBR                                            
04632          DATASET   (WS-PRODUCER-MASTER-DSID)                      
04633          RIDFLD    (WS-PRODUCER-MASTER-KEY)                       
04634          GENERIC   EQUAL                                          
04635          KEYLENGTH (13)                                           
04636      END-EXEC.                                                    
04637                                                                   
04638      MOVE +1                     TO  WS-READNEXT-SW.              
04639                                                                   
04640  4455-READNEXT.                                                   
04641      EXEC CICS READNEXT                                           
04642          DATASET   (WS-PRODUCER-MASTER-DSID)                      
04643          RIDFLD    (WS-PRODUCER-MASTER-KEY)                       
04644          SET       (ADDRESS OF PRODUCER-MASTER)                   
04645      END-EXEC.                                                    
04646                                                                   
04647      IF PI-COMPANY-CD = WS-PD-COMPANY-CD AND                      
04648         PI-CARRIER    = WS-PD-CARRIER    AND                      
04649         PI-GROUPING   = WS-PD-GROUPING   AND                      
04650         PI-STATE      = WS-PD-STATE      AND                      
04651         PI-PRODUCER   = WS-PD-PRODUCER                            
04652          NEXT SENTENCE                                            
04653        ELSE                                                       
04654          GO TO 4490-NOT-FOUND.                                    
04655                                                                   
04656      IF (PI-CERT-EFF-DT NOT LESS THAN PD-EFFECT-DT                
04657        AND PI-CERT-EFF-DT LESS THAN PD-EXPIRE-DATE)               
04658          IF AT-TRAILER-TYPE = '4'                                 
04659              MOVE PD-NAME        TO  DMAILTOO                     
04660              MOVE PD-PERSON      TO  DADDR1O                      
04661              MOVE PD-ADDRS       TO  DADDR2O                      
04662              MOVE PD-CITY        TO  DCITYSTO                     
04663              MOVE PD-TEL-NO      TO WS-WORK-PHONE                 
04664              INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'       
04665              MOVE WS-NUMERIC-PHONE TO DPHONEO                     
04666              INSPECT DPHONEI CONVERTING SPACES TO '-'             
04667              MOVE PD-ZIP-PRIME       TO WS-ZIP-CODE               
04668              IF PD-ZIP-PLUS4 = SPACES OR ZEROS                    
04669                  MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH      
04670                  MOVE WS-ZIP         TO DZIPO                     
04671                  GO TO 4495-END-OF-BROWSE                         
04672              ELSE                                                 
04673                  MOVE PD-ZIP-PLUS4   TO WS-ZIP-PLUS4              
04674                  MOVE '-'            TO WS-DASH                   
04675                  MOVE WS-ZIP         TO DZIPO                     
04676                  GO TO 4495-END-OF-BROWSE                         
04677          ELSE                                                     
04678              MOVE PD-NAME        TO  JMAILTOO                     
04679              MOVE PD-PERSON      TO  JADDR1O                      
04680              MOVE PD-ADDRS       TO  JADDR2O                      
04681              MOVE PD-CITY        TO  JCITYSTO                     
04682              MOVE PD-TEL-NO      TO WS-WORK-PHONE                 
04683              INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'       
04684              MOVE WS-NUMERIC-PHONE TO JPHONEO                     
04685              INSPECT JPHONEI CONVERTING SPACES TO '-'             
04686              IF PD-ZIP-PLUS4 = SPACES OR ZEROS                    
04687                  MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH      
04688                  MOVE WS-ZIP         TO JZIPO                     
04689                  GO TO 4495-END-OF-BROWSE                         
04690              ELSE                                                 
04691                  MOVE PD-ZIP-PLUS4   TO WS-ZIP-PLUS4              
04692                  MOVE '-'            TO WS-DASH                   
04693                  MOVE WS-ZIP         TO JZIPO                     
04694                  GO TO 4495-END-OF-BROWSE.                        
04695                                                                   
04696      GO TO 4455-READNEXT.                                         
04697                                                                   
04698  4490-NOT-FOUND.                                                  
04699      MOVE ER-9616                TO  EMI-ERROR.                   
04700      PERFORM 9900-ERROR-FORMAT.                                   
04701                                                                   
04702  4495-END-OF-BROWSE.                                              
04703      IF WS-READNEXT-SW = +1                                       
04704          EXEC CICS ENDBR                                          
04705              DATASET (WS-PRODUCER-MASTER-DSID)                    
04706          END-EXEC.                                                
04707                                                                   
04708  4499-EXIT.                                                       
04709      EXIT.                                                        
04710                                                                   
04711      EJECT                                                        
04712  4500-GENERAL-INFO-TRAILER.                                       
04713      IF AT-TRAILER-TYPE NOT = '6'                                 
04714          GO TO 4600-AUTO-PROMPT-TRAILER.                          
04715                                                                   
04716      MOVE EL142E                 TO  PI-MAP-NAME.                 
04717                                                                   
04718      MOVE AT-TRAILER-TYPE        TO  ETLRTYPO.                    
04719      MOVE AT-SEQUENCE-NO         TO  ESEQO.                       
04720                                                                   
04721      MOVE SPACES                 TO  DC-OPTION-CODE.              
04722      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               
04723      PERFORM 8500-DATE-CONVERSION.                                
04724      MOVE DC-GREG-DATE-1-EDIT    TO  ERECDTEO.                    
04725                                                                   
04726      MOVE AT-RECORDED-BY         TO  EBYO.                        
04727                                                                   
04728      MOVE AT-GEN-INFO-LAST-MAINT-DT    TO  DC-BIN-DATE-1          
04729                                            PI-SAVE-LAST-MAINT-DT. 
04730      MOVE ' '                          TO  DC-OPTION-CODE.        
04731      PERFORM 8500-DATE-CONVERSION.                                
04732      IF NO-CONVERSION-ERROR                                       
04733          MOVE DC-GREG-DATE-1-EDIT      TO  EMANTONO               
04734      ELSE                                                         
04735          MOVE SPACES                   TO  EMANTONO.              
04736                                                                   
04737      MOVE AT-GEN-INFO-LAST-UPDATED-BY  TO  EMANTBYO               
04738                                            PI-SAVE-LAST-UPD-BY.   
04739                                                                   
04740      MOVE AT-LAST-MAINT-HHMMSS   TO  TIME-IN.                     
04741      MOVE TIME-OUT               TO  EMANTATO.                    
04742                                                                   
04743      MOVE AT-INFO-TRAILER-TYPE   TO  ETYPENO.                     
04744                                                                   
04745      IF AT-INFO-TRAILER-TYPE = 'C'                                
04746          MOVE AT-CALL-TYPE       TO  ECALLO                       
04747        ELSE                                                       
04748          MOVE SPACES             TO  ECALLO                       
04749                                      ECALLTO.                     
04750                                                                   
04751      MOVE AT-INFO-LINE-1         TO  ELINE1O.                     
04752      MOVE AT-INFO-LINE-2         TO  ELINE2O.                     
04753                                                                   
04754      MOVE -1                     TO  EMAINTL.                     
04755                                                                   
04756      GO TO 4100-READNEXT.                                         
04757                                                                   
04758      EJECT                                                        
04759  4600-AUTO-PROMPT-TRAILER.                                        
04760      IF AT-TRAILER-TYPE NOT = '7'                                 
04761          GO TO 4700-DENIAL-TRAILER.                               
04762                                                                   
04763      MOVE EL142F                 TO  PI-MAP-NAME.                 
04764                                                                   
04765      MOVE AT-TRAILER-TYPE        TO  FTLRTYPO.                    
04766      MOVE AT-SEQUENCE-NO         TO  FSEQO.                       
04767                                                                   
04768      MOVE SPACES                 TO  DC-OPTION-CODE.              
04769      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               
04770      PERFORM 8500-DATE-CONVERSION.                                
04771      MOVE DC-GREG-DATE-1-EDIT    TO  FRECDTEO.                    
04772                                                                   
04773      MOVE AT-RECORDED-BY         TO  FBYO.                        
04774                                                                   
04775      MOVE AT-PROMPT-LAST-MAINT-DT    TO  DC-BIN-DATE-1.           
04776      MOVE ' '                        TO  DC-OPTION-CODE.          
04777      PERFORM 8500-DATE-CONVERSION.                                
04778      IF NO-CONVERSION-ERROR                                       
04779          MOVE DC-GREG-DATE-1-EDIT    TO  FMANTONO                 
04780      ELSE                                                         
04781          MOVE SPACES                 TO  FMANTONO.                
04782                                                                   
04783      MOVE AT-PROMPT-LAST-UPDATED-BY  TO  FMANTBYO.                
04784                                                                   
04785      MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.                 
04786      MOVE TIME-OUT                   TO  FMANTATO.                
04787                                                                   
04788      IF AT-PROMPT-START-DT NOT = LOW-VALUES                       
04789          MOVE SPACES                 TO  DC-OPTION-CODE           
04790          MOVE AT-PROMPT-START-DT     TO  DC-BIN-DATE-1            
04791          PERFORM 8500-DATE-CONVERSION                             
04792          MOVE DC-GREG-DATE-1-EDIT    TO  FSNOTIFI.                
04793                                                                   
04794      IF AT-PROMPT-END-DT NOT = LOW-VALUES                         
04795          MOVE SPACES                 TO  DC-OPTION-CODE           
04796          MOVE AT-PROMPT-END-DT       TO  DC-BIN-DATE-1            
04797          PERFORM 8500-DATE-CONVERSION                             
04798          MOVE DC-GREG-DATE-1-EDIT    TO  FENOTIFI.                
04799                                                                   
04800      MOVE AT-PROMPT-LINE-1       TO  FLINE1O.                     
04801      MOVE AT-PROMPT-LINE-2       TO  FLINE2O.                     
04802                                                                   
04803      MOVE -1                     TO  FMAINTL.                     
04804                                                                   
04805      GO TO 4100-READNEXT.                                         
04806                                                                   
04807      EJECT                                                        
04808  4700-DENIAL-TRAILER.                                             
04809      IF AT-TRAILER-TYPE NOT = '8'                                 
04810          GO TO 4800-INCURRED-CHANGE-TRAILER.                      

043019     MOVE AT-CONTROL-PRIMARY     TO WS-CLAIM-KEY
043019                                                                  
043019     EXEC CICS READ
043019        DATASET   (WS-CLAIM-MASTER-DSID)
043019        RIDFLD    (WS-CLAIM-KEY)
043019        SET       (ADDRESS OF CLAIM-MASTER)
043019        resp      (ws-response)
043019     END-EXEC

043019     if resp-normal
043019        move cl-incurred-dt      to pi-incurred-dt
043019     else
043019        move low-values          to pi-incurred-dt
043019     end-if

04812      MOVE EL142G                 TO  PI-MAP-NAME.                 
04813                                                                   
04814      MOVE AT-TRAILER-TYPE        TO  GTLRTYPO.                    
04815      MOVE AT-SEQUENCE-NO         TO  GSEQO.                       
04816                                                                   
04817      MOVE SPACES                 TO  DC-OPTION-CODE.              
04818      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1
043019                                     pi-den-recorded-dt
04819      PERFORM 8500-DATE-CONVERSION.                                
04820      MOVE DC-GREG-DATE-1-EDIT    TO  GRECDTEO.                    
04821                                                                   
04822      MOVE AT-RECORDED-BY         TO  GBYO.                        
04823                                                                   
04824      MOVE AT-DENIAL-LAST-MAINT-DT    TO  DC-BIN-DATE-1.           
04825      MOVE ' '                        TO  DC-OPTION-CODE.          
04826      PERFORM 8500-DATE-CONVERSION.                                
04827      IF NO-CONVERSION-ERROR                                       
04828          MOVE DC-GREG-DATE-1-EDIT    TO  GMANTONO                 
04829      ELSE                                                         
04830          MOVE SPACES                 TO  GMANTONO.                
04831                                                                   
04832      MOVE AT-DENIAL-LAST-UPDATED-BY  TO  GMANTBYO.                
04833                                                                   
04834      MOVE AT-LAST-MAINT-HHMMSS   TO  TIME-IN.                     
04835      MOVE TIME-OUT               TO  GMANTATO.                    
04836                                                                   
04837      MOVE AT-DENIAL-INFO-1       TO  GLINE1O.                     
04838      MOVE AT-DENIAL-INFO-2       TO  GLINE2O.                     
04839                                                                   
04840      IF AT-RETRACTION-DT NOT = LOW-VALUES                         
04841          MOVE SPACES             TO  DC-OPTION-CODE               
04842          MOVE AT-RETRACTION-DT   TO  DC-BIN-DATE-1                
04843          PERFORM 8500-DATE-CONVERSION                             
04844          MOVE DC-GREG-DATE-1-EDIT TO  GRECONSI.                   
04845                                           
052506     IF AT-DENIAL-PROOF-DT NOT = LOW-VALUES
052506         MOVE SPACES             TO DC-OPTION-CODE
052506         MOVE AT-DENIAL-PROOF-DT TO DC-BIN-DATE-1
052506         PERFORM 8500-DATE-CONVERSION 
052506         MOVE DC-GREG-DATE-1-EDIT TO GPRFDTI.
052506
04846      MOVE AT-DENIAL-REASON-CODE  TO  GRSNCDI
042110                                     PI-DENIAL-REASON-CODE
04847                                                                   
04848      MOVE -1                     TO  BMAINTL.                     
04849                                                                   
04850      GO TO 4100-READNEXT.                                         
04851                                                                   
04852      EJECT                                                        
04853  4800-INCURRED-CHANGE-TRAILER.                                    
04854      IF AT-TRAILER-TYPE NOT = '9'                                 
04855          GO TO 4900-FORMS-TRAILER.                                
04856                                                                   
04857      MOVE EL142I                 TO  PI-MAP-NAME.                 
04858                                                                   
04859      MOVE AT-TRAILER-TYPE        TO  ITLRTYPO.                    
04860      MOVE AT-SEQUENCE-NO         TO  ISEQO.                       
04861                                                                   
04862      MOVE SPACES                 TO  DC-OPTION-CODE.              
04863      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               
04864      PERFORM 8500-DATE-CONVERSION.                                
04865      MOVE DC-GREG-DATE-1-EDIT    TO  IRECDTEO.                    
04866                                                                   
04867      MOVE AT-RECORDED-BY         TO  IBYO.                        
04868                                                                   
04869      IF AT-OLD-INCURRED-DT NOT = LOW-VALUES                       
04870          MOVE SPACES                 TO  DC-OPTION-CODE           
04871          MOVE AT-OLD-INCURRED-DT     TO  DC-BIN-DATE-1            
04872          PERFORM 8500-DATE-CONVERSION                             
04873          MOVE DC-GREG-DATE-1-EDIT    TO  IINCDTO.                 
04874                                                                   
04875      IF AT-OLD-REPORTED-DT NOT = LOW-VALUES                       
04876          MOVE SPACES                 TO  DC-OPTION-CODE           
04877          MOVE AT-OLD-REPORTED-DT     TO  DC-BIN-DATE-1            
04878          PERFORM 8500-DATE-CONVERSION                             
04879          MOVE DC-GREG-DATE-1-EDIT    TO  IREPDTO.                 
04880                                                                   
04881      IF AT-OLD-ESTABLISHED-DT NOT = LOW-VALUES                    
04882          MOVE SPACES                 TO  DC-OPTION-CODE           
04883          MOVE AT-OLD-ESTABLISHED-DT  TO  DC-BIN-DATE-1            
04884          PERFORM 8500-DATE-CONVERSION                             
04885          MOVE DC-GREG-DATE-1-EDIT    TO  IESTDTO.                 
04886                                                                   
04887      MOVE AT-TRAILER-CNT-AT-CHG  TO  ITLRCNTO.                    
04888      MOVE AT-OLD-TOTAL-PAID      TO  ITAPDO.                      
04889      MOVE AT-OLD-DAYS-PAID       TO  ITDPDO.                      
04890      MOVE AT-OLD-NO-OF-PMTS      TO  INOPMTO.                     
04891                                                                   
04892      IF AT-OLD-PAID-THRU-DT NOT = LOW-VALUES                      
04893         MOVE SPACES                 TO  DC-OPTION-CODE            
04894         MOVE AT-OLD-PAID-THRU-DT    TO  DC-BIN-DATE-1             
04895         PERFORM 8500-DATE-CONVERSION                              
04896         MOVE DC-GREG-DATE-1-EDIT    TO  IPDTHRUO                  
04897         IF PI-USES-PAID-TO                                        
04898            MOVE '6'                TO  DC-OPTION-CODE             
04899            MOVE AT-OLD-PAID-THRU-DT TO  DC-BIN-DATE-1             
04900            MOVE +1                 TO  DC-ELAPSED-DAYS            
04901            MOVE +0                 TO  DC-ELAPSED-MONTHS          
04902            PERFORM 8500-DATE-CONVERSION                           
04903            MOVE DC-GREG-DATE-1-EDIT TO  IPDTHRUO.                 
04904                                                                   
04905      IF AT-LAST-PMT-MADE-DT NOT = LOW-VALUES                      
04906          MOVE SPACES                 TO  DC-OPTION-CODE           
04907          MOVE AT-LAST-PMT-MADE-DT    TO  DC-BIN-DATE-1            
04908          PERFORM 8500-DATE-CONVERSION                             
04909          MOVE DC-GREG-DATE-1-EDIT    TO  ILSTPDTO.                
04910                                                                   
04911      MOVE AT-OLD-INIT-MAN-RESV     TO  IMANRESO.                  
04912      MOVE AT-OLD-CURRENT-MAN-RESV  TO  ICURRESO.                  
04913      MOVE AT-OLD-ADDL-MAN-RESV     TO  IADDRESO.                  
04914                                                                   
04915      MOVE AT-OLD-ITD-PAID-EXPENSE  TO  ITEXPDO.                   
04916      MOVE AT-OLD-CHARGABLE-EXPENSE TO  ICHGEXPO.                  
04917                                                                   
04918      MOVE AT-OLD-DIAG-CODE         TO  ICAUSCDO.                  
04919      MOVE AT-OLD-DIAG-DESCRIP      TO  IDIAGO.                    
04920                                                                   
04921      MOVE -1                     TO  IPFKL.                       
04922                                                                   
04923      GO TO 4100-READNEXT.                                         
04924                                                                   
04925      EJECT                                                        
04926  4900-FORMS-TRAILER.                                              
04927      IF AT-TRAILER-TYPE NOT = 'A'                                 
04928          GO TO 6000-BAD-RECORD-TYPE.                              
04929                                                                   
04930      MOVE AT-CONTROL-PRIMARY     TO  WS-CLAIM-KEY.                
04931                                                                   
04932      EXEC CICS READ                                               
04933          DATASET   (WS-CLAIM-MASTER-DSID)                         
04934          RIDFLD    (WS-CLAIM-KEY)                                 
04935          SET        (ADDRESS OF CLAIM-MASTER)                     
04936      END-EXEC.                                                    
04937                                                                   
04938      EXEC CICS HANDLE CONDITION                                   
04939          NOTFND (4910-ADDRESS-NOT-FOUND)                          
04940      END-EXEC.                                                    
04941                                                                   
04942      MOVE EL142J                 TO  PI-MAP-NAME.                 
04943                                                                   
04944      MOVE +1                     TO  EMI-NUMBER-OF-LINES.         
04945      MOVE +2                     TO  EMI-SWITCH2.                 
04946                                                                   
04947      MOVE AT-TRAILER-TYPE        TO  JTLRTYPO.                    
04948      MOVE AT-SEQUENCE-NO         TO  JSEQO.                       
04949                                                                   
04950      MOVE SPACES                 TO  DC-OPTION-CODE.              
04951      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               
04952      PERFORM 8500-DATE-CONVERSION.                                
04953      MOVE DC-GREG-DATE-1-EDIT    TO  JRECDTEO.                    
04954                                                                   
04955      MOVE AT-RECORDED-BY         TO  JBYO.                        
04956                                                                   
04957      MOVE AT-FORM-LAST-MAINT-DT      TO  DC-BIN-DATE-1.           
04958      MOVE ' '                        TO  DC-OPTION-CODE.          
04959      PERFORM 8500-DATE-CONVERSION.                                
04960      IF NO-CONVERSION-ERROR                                       
04961          MOVE DC-GREG-DATE-1-EDIT    TO  JMANTONO                 
04962      ELSE                                                         
04963          MOVE SPACES                 TO  JMANTONO.                
04964                                                                   
04965      MOVE AT-FORM-LAST-UPDATED-BY    TO  JMANTBYO.                
04966                                                                   
04967      MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.                 
04968      MOVE TIME-OUT                   TO  JMANTATO.                
04969                                                                   
04970      IF AT-FORM-SEND-ON-DT NOT = LOW-VALUES                       
04971          MOVE SPACES             TO  DC-OPTION-CODE               
04972          MOVE AT-FORM-SEND-ON-DT  TO  DC-BIN-DATE-1               
04973          PERFORM 8500-DATE-CONVERSION                             
04974          MOVE DC-GREG-DATE-1-EDIT TO  JDTSENTI.                   
04975                                                                   
04976      IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES                       
04977          MOVE SPACES              TO  DC-OPTION-CODE              
04978          MOVE AT-FORM-RE-SEND-DT  TO  DC-BIN-DATE-1               
04979          PERFORM 8500-DATE-CONVERSION                             
04980          MOVE DC-GREG-DATE-1-EDIT    TO  JRESENDI                 
04981          IF AT-FORM-REPRINT-DT NOT = LOW-VALUES                   
04982              MOVE AL-SANOF       TO  JRESENDA                     
04983            ELSE                                                   
04984              NEXT SENTENCE                                        
04985      ELSE                                                         
04986          MOVE AL-SANOF           TO  JRESENDA.                    
04987                                                                   
04988      IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES                     
04989          MOVE SPACES             TO  DC-OPTION-CODE               
04990          MOVE AT-FORM-FOLLOW-UP-DT TO DC-BIN-DATE-1               
04991          PERFORM 8500-DATE-CONVERSION                             
04992          MOVE DC-GREG-DATE-1-EDIT TO  JREPLYI.                    
04993                                                                   
04994      IF AT-FORM-ANSWERED-DT NOT = LOW-VALUES                      
04995          MOVE SPACES             TO  DC-OPTION-CODE               
04996          MOVE AT-FORM-ANSWERED-DT TO DC-BIN-DATE-1                
04997          PERFORM 8500-DATE-CONVERSION                             
04998          MOVE DC-GREG-DATE-1-EDIT TO JRECEVEI.                    
04999                                                                   
05000      IF AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES AND SPACES       
05001          MOVE SPACES                  TO  DC-OPTION-CODE          
05002          MOVE AT-PHY-FORM-ANSWERED-DT TO  DC-BIN-DATE-1           
05003          PERFORM 8500-DATE-CONVERSION                             
05004          MOVE DC-GREG-DATE-1-EDIT     TO JPHYRECI.                
05005                                                                   
05006      IF AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES AND SPACES       
05007          MOVE SPACES                  TO  DC-OPTION-CODE          
05008          MOVE AT-EMP-FORM-ANSWERED-DT TO  DC-BIN-DATE-1           
05009          PERFORM 8500-DATE-CONVERSION                             
05010          MOVE DC-GREG-DATE-1-EDIT     TO JEMPRECI.                
05011                                                                   
05012      IF AT-FORM-REM-PRINT-DT NOT = LOW-VALUES AND SPACES          
05013          MOVE SPACES                  TO  DC-OPTION-CODE          
05014          MOVE AT-FORM-REM-PRINT-DT    TO  DC-BIN-DATE-1           
05015          PERFORM 8500-DATE-CONVERSION                             
05016          MOVE DC-GREG-DATE-1-EDIT     TO JREMDTI.                 
05017                                                                   
05018      IF AT-FORM-TYPE = '1'                                        
05019          MOVE 'INITIAL'          TO  JFORMO                       
05020      ELSE                                                         
05021          IF AT-FORM-TYPE = '2'                                    
05022              MOVE 'PROGRESS'     TO  JFORMO                       
05023          ELSE                                                     
05024              MOVE AT-FORM-TYPE   TO  JFORMO.                      
05025                                                                   
05026      MOVE AT-INSTRUCT-LN-1       TO  JSI1O.                       
05027      MOVE AT-INSTRUCT-LN-2       TO  JSI2O.                       
05028      MOVE AT-INSTRUCT-LN-3       TO  JSI3O.                       
05029                                                                   
05030      MOVE AT-REL-CARR-1          TO  JCARR1O.                     
05031      MOVE AT-REL-CLAIM-1         TO  JCLAIM1O.                    
05032      MOVE AT-REL-CERT-1          TO  JCERT1O.                     
05033                                                                   
05034      MOVE AT-REL-CARR-2          TO  JCARR2O.                     
05035      MOVE AT-REL-CLAIM-2         TO  JCLAIM2O.                    
05036      MOVE AT-REL-CERT-2          TO  JCERT2O.                     
05037                                                                   
05038      MOVE -1                     TO  JMAINTL.                     
05039                                                                   
05040      IF AT-FORM-ADDRESS = SPACE                                   
05041          NEXT SENTENCE                                            
05042        ELSE                                                       
05043      IF AT-FORM-ADDRESS       = '3'    AND                        
05044         AT-FORM-ADDR-SEQ-NO   = ZERO   AND                        
05045         CL-SYSTEM-IDENTIFIER  = 'CR'                              
05046          MOVE SPACES             TO  WS-ACCOUNT-MASTER-KEY        
05047          MOVE PI-COMPANY-CD      TO  WS-AM-COMPANY-CD             
05048          MOVE PI-CARRIER         TO  WS-AM-CARRIER                
05049          MOVE PI-GROUPING        TO  WS-AM-GROUPING               
05050          MOVE PI-STATE           TO  WS-AM-STATE                  
05051          MOVE PI-ACCOUNT         TO  WS-AM-ACCOUNT                
05052          MOVE PI-CERT-EFF-DT     TO  WS-AM-EXPIRATION-DT          
05053          PERFORM 4420-FIND-ACCOUNT-MASTER THRU 4449-EXIT          
05054        ELSE                                                       
05055      IF AT-FORM-ADDRESS       = '3'    AND                        
05056         AT-FORM-ADDR-SEQ-NO   = ZERO   AND                        
05057         CL-SYSTEM-IDENTIFIER  = 'CV'                              
05058          MOVE SPACES             TO  WS-PRODUCER-MASTER-KEY       
05059          MOVE PI-COMPANY-CD      TO  WS-PD-COMPANY-CD             
05060          MOVE PI-CARRIER         TO  WS-PD-CARRIER                
05061          MOVE PI-GROUPING        TO  WS-PD-GROUPING               
05062          MOVE PI-STATE           TO  WS-PD-STATE                  
05063          MOVE PI-PRODUCER        TO  WS-PD-PRODUCER               
05064          MOVE PI-CERT-EFF-DT     TO  WS-PD-EXPIRATION-DT          
05065          PERFORM 4450-FIND-PRODUCER-MASTER THRU 4499-EXIT         
05066        ELSE                                                       
05067      IF AT-FORM-ADDR-SEQ-NO NOT = ZERO                            
05068          MOVE PI-ACTIVITY-TRAILERS-KEY TO WS-ACTIVITY-TRAILERS-KEY
05069          MOVE AT-FORM-ADDR-SEQ-NO    TO  WS-ATK-SEQUENCE-NO       
05070          EXEC CICS READ                                           
05071              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
05072              RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                   
05073              SET     (ADDRESS OF ACTIVITY-TRAILERS)               
05074          END-EXEC                                                 
05075          MOVE AT-MAIL-TO-NAME    TO  JMAILTOO                     
05076          MOVE AT-ADDRESS-LINE-1  TO  JADDR1O                      
05077          MOVE AT-ADDRESS-LINE-2  TO  JADDR2O                      
05078          MOVE AT-CITY-STATE      TO  JCITYSTO                     
               STRING AT-CITY ' ' AT-STATE
                  DELIMITED BY '  ' INTO JCITYSTO
               END-STRING
05079          MOVE AT-PHONE-NO        TO  JPHONEO                      
05080          INSPECT JPHONEI CONVERTING SPACES TO '-'                 
05081          IF  AT-CANADIAN-POST-CODE                                
05082              MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1        
05083              MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2        
05084              MOVE SPACES             TO WS-DASH-CAN               
05085                                         WS-CAN-FILLER             
05086              MOVE WS-CANADIAN-POSTAL-CODES                        
05087                                      TO JZIPO                     
05088          ELSE                                                     
05089              MOVE AT-ZIP-CODE        TO WS-ZIP-CODE               
05090              IF  AT-ZIP-PLUS4 = SPACES OR ZEROS                   
05091                  MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH      
05092                  MOVE WS-ZIP         TO JZIPO                     
05093              ELSE                                                 
05094                  MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4              
05095                  MOVE '-'            TO WS-DASH                   
05096                  MOVE WS-ZIP         TO JZIPO.                    
05097                                                                   
05098      GO TO 4100-READNEXT.                                         
05099                                                                   
05100  4910-ADDRESS-NOT-FOUND.                                          
05101      MOVE ER-0388                TO  EMI-ERROR.                   
05102      PERFORM 9900-ERROR-FORMAT.                                   
05103                                                                   
05104      GO TO 4100-READNEXT.                                         
05105                                                                   
05106      EJECT                                                        
05107  6000-BAD-RECORD-TYPE.                                            
05108      MOVE 'BAD TRAILER RECORD TYPE - PROBABLE LOGIC ERROR'        
05109                                  TO  LOGOFF-MSG.                  
05110                                                                   
05111      PERFORM 8300-SEND-TEXT.                                      
05112                                                                   
05113  6000-END-OF-FILE.                                                
05114      IF PI-MAP-NAME = EL142A                                      
05115          MOVE ER-0344            TO  EMI-ERROR                    
05116          MOVE -1                 TO  ARECDTEL                     
05117          EXEC CICS ENDBR                                          
05118              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
05119          END-EXEC                                                 
05120          PERFORM 8200-SEND-DATAONLY.                              
05121                                                                   
05122      MOVE ER-0303                TO  EMI-ERROR.                   
05123      MOVE +1                     TO  PI-END-OF-FILE.              
05124                                                                   
05125  6000-ENDBROWSE.                                                  
05126      EXEC CICS ENDBR                                              
05127          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      
05128      END-EXEC.                                                    
05129                                                                   
05130      MOVE EIBAID                 TO  PI-PREV-AID.                 
05131                                                                   
05132      PERFORM 8100-SEND-INITIAL-MAP.                               
05133      PERFORM 9100-RETURN-TRAN.                                    
05134                                                                   
05135  6990-EXIT.                                                       
05136      EXIT.                                                        
05137                                                                   
05138      EJECT                                                        
05139  5000-DISPLAY-CHECK-QUEUE SECTION.                                
05140      MOVE +3                     TO  EMI-NUMBER-OF-LINES.         
05141      MOVE +2                     TO  EMI-SWITCH2.                 
05142                                                                   
05143      MOVE EL142B2                TO  PI-MAP-NAME.                 
05144                                                                   
05145      MOVE LOW-VALUES             TO  EL142B2O.                    
05146                                                                   
05147      MOVE -1                     TO  KMAINTL.                     
05148                                                                   
05149      EXEC CICS READ                                               
05150          DATASET  (WS-ACTIVITY-TRAILERS-DSID)                     
05151          RIDFLD   (PI-SAVE-KEY)                                   
05152          SET      (ADDRESS OF ACTIVITY-TRAILERS)                  
05153      END-EXEC.                                                    
05154                                                                   
05155      MOVE AT-CLAIM-TYPE          TO  KCTYPEO.                     
05156                                                                   
05157      MOVE AT-CARRIER             TO  KCARRO.                      
05158      MOVE AT-CLAIM-NO            TO  KCLMNOO.                     
05159      MOVE AT-CERT-NO             TO  KCERTNOO.                    
05160                                                                   
05161      MOVE AT-SEQUENCE-NO         TO  KTSEQO.                      
05162                                                                   
05163      IF AT-CLAIM-PREM-TYPE = '1'                                  
05164          MOVE 'SP'               TO  KCOVERO                      
05165        ELSE                                                       
05166      IF AT-CLAIM-PREM-TYPE = '2'                                  
05167          MOVE 'OB'               TO  KCOVERO                      
05168        ELSE                                                       
05169      IF AT-CLAIM-PREM-TYPE = '3'                                  
05170          MOVE 'OE'               TO  KCOVERO                      
05171        ELSE                                                       
05172          MOVE AT-CLAIM-PREM-TYPE TO  KCOVERO.                     
05173                                                                   
05174      MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.            
05175      MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER         
05176                                      KCONTRLO.                    
05177      MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER        
05178                                      KSEQO.                       
05179                                                                   
05180      MOVE AT-CHECK-NO            TO  KCKNOO.                      
05181      MOVE AT-AMOUNT-PAID         TO  KCKAMTO.                     
05182                                                                   
05183      IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES                      
05184          MOVE AT-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1               
05185          MOVE SPACES              TO  DC-OPTION-CODE              
05186          PERFORM 8500-DATE-CONVERSION                             
05187          MOVE DC-GREG-DATE-1-EDIT TO  KCKDATEO.                   
05188                                                                   
05189      MOVE AT-RECORDED-BY         TO  KBYO.                        
05190                                                                   
05191      IF AT-CV-PMT-CODE = ' '                                      
05192          GO TO 5000-DISPLAY-CHECK-DESC.                           
05193                                                                   
05194      IF AT-CV-PMT-CODE = '1'                                      
05195          MOVE 'FULL DEATH     '         TO  KPAYTYPO.             
05196                                                                   
05197      IF AT-CV-PMT-CODE = '2'                                      
05198          MOVE 'HALF DEATH     '         TO  KPAYTYPO.             
05199                                                                   
05200      IF AT-CV-PMT-CODE = '3'                                      
05201          MOVE 'FULL AD&D      '         TO  KPAYTYPO.             
05202                                                                   
05203      IF AT-CV-PMT-CODE = '4'                                      
05204          MOVE 'HALF AD&D      '         TO  KPAYTYPO.             
05205                                                                   
05206      IF AT-CV-PMT-CODE = '5'                                      
05207          MOVE 'FULL RIDER     '         TO  KPAYTYPO.             
05208                                                                   
05209      IF AT-CV-PMT-CODE = '6'                                      
05210          MOVE 'HALF RIDER     '         TO  KPAYTYPO.             
05211                                                                   
05212      IF AT-CV-PMT-CODE = '7'                                      
05213          MOVE 'NON-CHG EXP    '         TO  KPAYTYPO.             
05214                                                                   
05215      IF AT-CV-PMT-CODE = '8'                                      
05216          MOVE 'ADDITIONAL     '         TO  KPAYTYPO.             
05217                                                                   
05218      GO TO 5000-DISPLAY-CHECK-QUE-CONT.                           
05219                                                                   
05220  5000-DISPLAY-CHECK-DESC.                                         
05221                                                                   
05222      IF AT-PAYMENT-TYPE = '1'                                     
05223          MOVE 'PARTIAL PAYMENT'         TO  KPAYTYPO              
05224        ELSE                                                       
05225      IF AT-PAYMENT-TYPE = '2'                                     
05226          MOVE 'FINAL PAYMENT'           TO  KPAYTYPO              
05227        ELSE                                                       
05228      IF AT-PAYMENT-TYPE = '3'                                     
05229          MOVE 'LUMP SUM PAYMENT'        TO  KPAYTYPO              
05230        ELSE                                                       
05231      IF AT-PAYMENT-TYPE = '4'                                     
05232          MOVE 'ADDITIONAL PAYMENT'      TO  KPAYTYPO              
05233        ELSE                                                       
05234      IF AT-PAYMENT-TYPE = '5'                                     
05235          MOVE 'CHARGEABLE PAYMENT'      TO  KPAYTYPO              
05236        ELSE                                                       
05237      IF AT-PAYMENT-TYPE = '6'                                     
05238          MOVE 'NON-CHARGEABLE PAYMENT'  TO  KPAYTYPO              
05239        ELSE                                                       
05240      IF AT-PAYMENT-TYPE = '7'                                     
05241          MOVE 'LIFE PREMIUM REFUND'     TO  KPAYTYPO              
05242        ELSE                                                       
05243      IF AT-PAYMENT-TYPE = '8'                                     
05244          MOVE 'A & H PREMIUM REFUND'    TO  KPAYTYPO              
05245        ELSE                                                       
05246          MOVE 'ENTRY CORRECTION'        TO  KPAYTYPO.             
05247                                                                   
05248  5000-DISPLAY-CHECK-QUE-CONT.                                     
05249                                                                   
05250      IF AT-VOID-DT NOT = LOW-VALUES                               
05251          MOVE 'YES'              TO  KVOIDO                       
05252        ELSE                                                       
05253          MOVE 'NO'               TO  KVOIDO.                      
05254                                                                   
05255      EXEC CICS HANDLE CONDITION                                   
05256          NOTFND (5020-DISPLAY-CHECK-QUEUE)                        
05257      END-EXEC.                                                    
05258                                                                   
05259      EXEC CICS READ                                               
05260          DATASET (WS-CHECK-QUEUE-DSID)                            
05261          RIDFLD  (WS-CHECK-QUEUE-KEY)                             
05262          SET     (ADDRESS OF CHECK-QUE)                           
05263      END-EXEC.                                                    
05264                                                                   
05265      MOVE CQ-ENTRY-TYPE          TO  KTYPEO.                      
05266                                                                   
05267      IF CQ-CHECK-NUMBER NOT = AT-CHECK-NO                         
05268          MOVE CQ-CHECK-NUMBER    TO  KCKNO2O                      
05269          MOVE ER-0574            TO  EMI-ERROR                    
05270          PERFORM 9900-ERROR-FORMAT.                               
05271                                                                   
05272      IF CQ-PMT-TRLR-SEQUENCE NOT = AT-SEQUENCE-NO                 
05273          MOVE ER-0575            TO  EMI-ERROR                    
05274          PERFORM 9900-ERROR-FORMAT.                               
05275                                                                   
05276      IF CQ-CHECK-AMOUNT NOT = AT-AMOUNT-PAID                      
05277          MOVE ER-0576            TO  EMI-ERROR                    
05278          PERFORM 9900-ERROR-FORMAT.                               
05279                                                                   
05280      IF CQ-CHECK-WRITTEN-DT NOT = AT-CHECK-WRITTEN-DT             
05281          MOVE ER-0577            TO  EMI-ERROR                    
05282          PERFORM 9900-ERROR-FORMAT.                               
05283                                                                   
05284      MOVE CQ-TIMES-PRINTED       TO  KTIMPRTO.                    
05285                                                                   
05286      IF CHECKS-WERE-PRE-NUMBERED                                  
05287          MOVE 'YES'              TO  KPRENOO                      
05288      ELSE                                                         
05289          MOVE 'NO '              TO  KPRENOO.                     
05290                                                                   
05291      GO TO 5080-DISPLAY-CHECK-QUEUE.                              
05292                                                                   
05293  5020-DISPLAY-CHECK-QUEUE.                                        
05294      MOVE 'A'                    TO  KMAINTO.                     
05295      MOVE AL-SANON               TO  KMAINTA.                     
05296                                                                   
05297      MOVE -1                     TO  KCONTRLO.                    
05298                                                                   
05299      MOVE ER-0578                TO  EMI-ERROR.                   
05300      PERFORM 9900-ERROR-FORMAT.                                   
05301                                                                   
05302  5080-DISPLAY-CHECK-QUEUE.                                        
05303      PERFORM 8100-SEND-INITIAL-MAP.                               
05304                                                                   
05305  5090-EXIT.                                                       
05306      EXIT.                                                        
05307                                                                   
05308      EJECT                                                        
05309                                                                   
102510 6000-DISPLAY-ELNAPS SECTION.                                     
05311      MOVE +3                     TO  EMI-NUMBER-OF-LINES.         
05312      MOVE +2                     TO  EMI-SWITCH2.                 
05313                                                                   
05314      MOVE EL142D2                TO  PI-MAP-NAME.                 
05315                                                                   
05316      MOVE LOW-VALUES             TO  EL142D2O.                    
05317                                                                   
05318      MOVE -1                     TO LPFKL.                        
05319                                                                   
05320      EXEC CICS READ                                               
05321          DATASET  (WS-ACTIVITY-TRAILERS-DSID)                     
05322          RIDFLD   (PI-SAVE-KEY)                                   
05323          SET      (ADDRESS OF ACTIVITY-TRAILERS)                  
05324      END-EXEC.                                                    
05325                                                                   
102510     MOVE PI-COMPANY-CD          TO  WS-NA-COMPANY-CD.            
102510     MOVE PI-SAVE-ATK-CARRIER    TO  WS-NA-CARRIER.
102510     MOVE PI-SAVE-ATK-CLAIM-NO   TO  WS-NA-CLAIM-NO.
102510     MOVE PI-SAVE-ATK-CERT-NO    TO  WS-NA-CERT-NO.
102510     MOVE AT-LETTER-ARCHIVE-NO   TO  WS-NA-ARCHIVE-NO.            
05330                                                                   
05331      EXEC CICS HANDLE CONDITION                                   
102510         ENDFILE (6020-ELNAPS-NOTFND)                             
102510         NOTFND  (6020-ELNAPS-NOTFND)                             
05334      END-EXEC.                                                    
05335                                                                   
05336      EXEC CICS READ                                               
102510          DATASET (WS-NAPERSOFT-DSID)                        
102510          RIDFLD  (WS-NAPERSOFT-KEY)                         
102510          SET     (ADDRESS OF NAPERSOFT-FILE)                     
05340      END-EXEC.                                                    
05341                                                                   
102510     MOVE NA-CARRIER             TO LCARRO.
102510     MOVE NA-CLAIM-NO            TO LCLMNOO.
102510     MOVE NA-CERT-NO             TO LCRTNOO.
102510     MOVE NA-ARCHIVE-NO          TO LARCHNOO.
102510     MOVE NA-LETTER-ID           TO LLETRIDO.
102510     MOVE NA-PROCESSOR-ID        TO LPROCO.
102510     MOVE NA-RESEND-LETTER-ID    TO LRSLTIDO.
102510     MOVE NA-NO-OF-COPIES        TO LNOCPYSO.
102510     MOVE NA-ADDRESS-TYPE        TO LADDTYPO.
102510     MOVE NA-CORR-TRLR-SEQ       TO LCORSEQO.
102510     MOVE NA-ENCLOSURE-CD        TO LENCCODO.
102510     MOVE NA-CREATED-IN-NAPERSOFT TO LCREATNO.
102510     MOVE NA-ORIG-ARCHIVE-NO     TO LORIGARO.
102510     MOVE NA-RESEND-PROMPT-IND   TO LPROMPTO.
102510     MOVE NA-ORIG-ENCLOSURE-CD   TO LORIGENO.
05347                                                                   
102510     IF NA-CREATION-DT NOT = LOW-VALUES                           
102510         MOVE NA-CREATION-DT     TO  DC-BIN-DATE-1                
05358          MOVE SPACES             TO  DC-OPTION-CODE               
05359          PERFORM 8500-DATE-CONVERSION                             
05360          MOVE DC-GREG-DATE-1-EDIT TO LCREDTEO.                    
05361                                                                   
102510     IF NA-INITIAL-PRINT-DT NOT = LOW-VALUES                    
102510         MOVE NA-INITIAL-PRINT-DT TO  DC-BIN-DATE-1            
05364          MOVE SPACES             TO  DC-OPTION-CODE               
05365          PERFORM 8500-DATE-CONVERSION                             
05366          MOVE DC-GREG-DATE-1-EDIT TO LINPRNTO.                    
05367                                                                   
102510     IF NA-FOLLOW-UP-DT NOT = LOW-VALUES                           
102510         MOVE NA-FOLLOW-UP-DT    TO  DC-BIN-DATE-1                
102510         MOVE SPACES             TO  DC-OPTION-CODE               
102510         PERFORM 8500-DATE-CONVERSION                             
102510         MOVE DC-GREG-DATE-1-EDIT TO LFUPDTEO.                    
102510                                                                  
102510     IF NA-RESEND-DT NOT = LOW-VALUES                           
102510         MOVE NA-RESEND-DT       TO  DC-BIN-DATE-1                
102510         MOVE SPACES             TO  DC-OPTION-CODE               
102510         PERFORM 8500-DATE-CONVERSION                             
102510         MOVE DC-GREG-DATE-1-EDIT TO LRESDTEO.                    
102510                                                                  
102510     IF NA-RESEND-PRINT-DT NOT = LOW-VALUES                     
102510         MOVE NA-RESEND-PRINT-DT TO  DC-BIN-DATE-1            
05370          MOVE SPACES             TO  DC-OPTION-CODE               
05371          PERFORM 8500-DATE-CONVERSION                             
05372          MOVE DC-GREG-DATE-1-EDIT  TO LREPRNTO.                   
05373                                                                   
102510     IF NA-1ST-LTR-PRINT-DT NOT = LOW-VALUES                           
102510         MOVE NA-1ST-LTR-PRINT-DT TO  DC-BIN-DATE-1                
102510         MOVE SPACES             TO  DC-OPTION-CODE               
102510         PERFORM 8500-DATE-CONVERSION                             
102510         MOVE DC-GREG-DATE-1-EDIT TO L1STPRTO.
102510
102510     IF NA-NEXT-DUE-DT NOT = LOW-VALUES
102510         MOVE NA-NEXT-DUE-DT     TO  DC-BIN-DATE-1
102510         MOVE SPACES             TO  DC-OPTION-CODE
102510         PERFORM 8500-DATE-CONVERSION
102510         MOVE DC-GREG-DATE-1-EDIT TO LNXTDUEO.
102510
102510     IF NA-AUTOPYDT NOT = LOW-VALUES
102510         MOVE NA-AUTOPYDT        TO  DC-BIN-DATE-1
102510         MOVE SPACES             TO  DC-OPTION-CODE
102510         PERFORM 8500-DATE-CONVERSION
102510         MOVE DC-GREG-DATE-1-EDIT TO LAUTOPYO.
05375                                                                   
05376      GO TO 8100-SEND-INITIAL-MAP.                                 
05377                                                                   
102510 6020-ELNAPS-NOTFND.                                              
05379      MOVE ER-0006                TO  EMI-ERROR.                   
05380      PERFORM 9900-ERROR-FORMAT.                                   
05381                                                                   
05382      GO TO 8100-SEND-INITIAL-MAP.                                 
05383      EJECT                                                        
021114           
021114           
021114 7000-CONNECT-TO-DB.

      ****  The below code is for when the db has been
      ****  converted to sql server 2016
      *    move '//sdv-db01.cso.local:1433;'
      *                                to p-sql-server

      ****  The below code is for when the db is still
      ****  on sql server 2008 R2
           move '//ntcso2.cso.local:1433;'
                                       to p-sql-server

           move 'Logic'                to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc


021114     MOVE 'NTCSO2_LOGIC'         TO SVR
021114     MOVE 'sa'                   TO USR
021114     MOVE 'ntcso2'               TO PASS
111714*    MOVE 'NTSQLTST2_LOGIC'      TO SVR
111714*    MOVE 'sa'                   TO USR
111714*    MOVE 'sql2008r2'            TO PASS
021114
021114*     STRING
021114*         USR DELIMITED SPACE
021114*         "." DELIMITED SIZE
021114*         PASS DELIMITED SPACE INTO USR-PASS
021114*     END-STRING
021114* 
021114*     EXEC SQL
021114*        CONNECT TO :SVR USER :USR-PASS
021114*     END-EXEC
021114 
021114     IF SQLCODE NOT = 0
021114        DISPLAY "ERROR: CANNOT CONNECT "
021114        DISPLAY SQLCODE
021114        DISPLAY SQLERRMC
021114        GO TO 7000-EXIT
021114     END-IF
021114
021114     .
021114 7000-EXIT.
021114     EXIT.
021114
021114
021114
021114 7100-GET-CHK-CASHED-DT.
021114
051215     move spaces                 to ws-check-cashed-dt
051215     move zeros                  to sqlcode
111714     move '2'                    to ws-draft-or-check  *>  draft
111714     if at-check-written-dt not = low-values and spaces
111714        evaluate true
060315           when (at-check-written-dt > X'ACFE')    *> 04/30/2015
090415              and (pi-company-id not = 'DCC')
111714*          when at-check-written-dt >= X'AC93'    *> 01/19/2015
111714              move '1'           to ws-draft-or-check  *>  check
111714              move '000'         to ws-check-number (1:3)
111714              move at-check-no   to ws-check-number (4:7)
013017           when (at-check-written-dt > X'B016')    *> 05/22/2017
013017              and (pi-company-id = 'DCC')
013017              move '1'           to ws-draft-or-check  *>  check
013017              move '000'         to ws-check-number (1:3)
013017              move at-check-no   to ws-check-number (4:7)
111714           when at-payment-type = 'I'
111714              move '000'         to ws-check-number (1:3)
111714              move at-check-no   to ws-check-number (4:7)
111714           when other
111714              move '0'           to ws-check-number (1:1)
111714              move at-check-no (1:1)
111714                                 to ws-check-number (2:1)
111714              move '00'          to ws-check-number (3:2)
111714              move at-check-no (2:6)
111714                                 to ws-check-number (5:6)
111714        end-evaluate
051215     else   *>  no check written date yet
051215        go to 7100-exit
111714     end-if

111714     evaluate true
111714        when pi-company-id = 'AHL'
111714           move '%AHL%'          to ws-check-company
111714        when pi-company-id = 'DCC'
111714           move '%DCC%'          to ws-check-company
020816        when pi-company-id = 'VPP'
020816           move '%VPP%'          to ws-check-company
062121        when pi-company-id = 'FNL'
062121           move '%FNL%'          to ws-check-company
111714        when other   *>   CID s/b   CSO
111714           move '%CSO%'          to ws-check-company
111714     end-evaluate

021114     MOVE AT-PAYMENT-TYPE        TO WS-CHECK-TYPE
111714     move at-claim-no            to ws-claim-number
021114     MOVE AT-AMOUNT-PAID         TO WS-CHECK-AMT-TMP
021114     MOVE WS-CHECK-AMT-TMPX      TO WS-CHECK-AMOUNT
021114     MOVE SPACES                 TO WS-CHECK-CASHED-DT
021114
021114     EXEC SQL
021114        CALL LogicPaidBankChkCashedDt
NTTIns         (
111714           @draftorcheck    = :WS-DRAFT-OR-CHECK,
021114           @checktype       = :WS-CHECK-TYPE,
111714           @claimnumber     = :WS-CLAIM-NUMBER,
021114           @checknumber     = :WS-CHECK-NUMBER,
021114           @checkamount     = :WS-CHECK-AMOUNT,
021114           @checkcompany    = :WS-CHECK-COMPANY,
021114           @checkcasheddate = :WS-CHECK-CASHED-DT OUT
NTTIns         )
021114     END-EXEC
021114
021114     IF SQLCODE NOT = 0
021114        MOVE SPACES TO WS-CHECK-CASHED-DT
021114*        DISPLAY "ERROR: DID NOT RETURN CHK CASHED DT "
021114*        DISPLAY ' SQL RETURN CODE ' SQLCODE
021114*        DISPLAY ' SQL ERR MESS    ' SQLERRMC
021114        GO TO 7100-EXIT
021114     END-IF
021114
021114     .
021114 7100-EXIT.
021114     EXIT.
021114
091714 7110-CHECK-MANUAL.
091714
091714     MOVE AT-PAYMENT-TYPE        TO WS-CHECK-TYPE
111714     MOVE AT-CHECK-NO            TO WS-CHECK-NUMBER-man
091714     MOVE AT-AMOUNT-PAID         TO WS-CHECK-AMT-TMP
091714     MOVE WS-CHECK-AMT-TMPX      TO WS-CHECK-AMOUNT
111714     MOVE PI-COMPANY-ID          TO WS-CHECK-COMPANY-man
091714     MOVE SPACES                 TO WS-CHECK-CASHED-DT
091714
091714     EXEC SQL
091714       CALL LogicPaidBankChkCashedDtManual
NTTIns               (
091714                 @checktype = :WS-CHECK-TYPE,
111714                 @checknumber = :WS-CHECK-NUMBER-man,
091714                 @checkamount = :WS-CHECK-AMOUNT,
111714                 @checkcompany = :WS-CHECK-COMPANY-man,
091714                 @checkcasheddate = :WS-CHECK-CASHED-DT OUT
NTTIns               )
091714     END-EXEC
091714
091714     IF SQLCODE NOT = 0
091714        MOVE SPACES TO WS-CHECK-CASHED-DT
091714*        DISPLAY "ERROR: DID NOT RETURN CHK CASHED DT "
091714*        DISPLAY ' SQL RETURN CODE ' SQLCODE
091714*        DISPLAY ' SQL ERR MESS    ' SQLERRMC
091714*       GO TO 7100-EXIT
091714     END-IF
091714
091714     .
091714 7110-EXIT.
091714     EXIT.
021114
021114 7200-DISCONNECT.
021114
021114     EXEC SQL
021114        DISCONNECT
021114     END-EXEC
021114     .
021114 7200-EXIT.
021114     EXIT.
021114
05384                                                                   
05385  8000-CREATE-DMO-REC.                                             
05386      MOVE PI-COMPANY-CD          TO NOTE-COMP-CD.                 
05387      MOVE WS-CL-CERT-KEY-DATA    TO NOTE-CERT-KEY.                
05388      MOVE WS-CL-CERT-NO          TO NOTE-CERT-NO.                 
05389                                                                   
05390      EXEC CICS HANDLE CONDITION                                   
05391           NOTFND   (8000-NOTE-NOT-FOUND)                          
05392      END-EXEC.                                                    
05393                                                                   
05394      EXEC CICS READ                                               
05395           DATASET(WS-NOTE-FILE-DSID)                              
05396           SET    (ADDRESS OF CERTIFICATE-NOTE)                    
05397           RIDFLD (NOTE-KEY)                                       
05398      END-EXEC.                                                    
05399                                                                   
05400      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.       
05401      MOVE WS-CL-BENEFICIARY      TO DCT-LOGIC-BENEFICIARY-ID.     
05402      MOVE WS-CL-CCN              TO DCT-CREDIT-CARD-NUMBER.       
05403                                                                   
05404      IF PI-GROUPING (5:2) = ZEROS OR SPACES                       
05405          MOVE 'CC'               TO DCT-PRODUCT-CODE              
05406      ELSE                                                         
05407          MOVE PI-GROUPING (5:2)  TO DCT-PRODUCT-CODE.             
05408                                                                   
05409      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.          
05410      MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.      
05411                                                                   
05412      EXEC CICS LINK                                               
05413          PROGRAM    ('DLO006')                                    
05414          COMMAREA   (DCT-COMMUNICATION-AREA)                      
05415          LENGTH     (WS-DCT-LENGTH)                               
05416      END-EXEC.                                                    
05417                                                                   
05418      IF DCT-RETURN-CODE = 'OK'                                    
05419          GO TO 8000-CONT.                                         
05420                                                                   
05421      IF DCT-RETURN-CODE = '01' OR '02'                            
05422          GO TO 8000-EXIT.                                         
05423                                                                   
05424      IF DCT-RETURN-CODE = '03'                                    
05425          MOVE ER-0951            TO EMI-ERROR                     
05426          MOVE -1                 TO GMAINTL                       
05427          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05428          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05429          GO TO 8200-SEND-DATAONLY.                                
05430                                                                   
05431      IF DCT-RETURN-CODE = '06'                                    
05432          MOVE ER-0921            TO EMI-ERROR                     
05433          MOVE -1                 TO GMAINTL                       
05434          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05435          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05436          GO TO 8200-SEND-DATAONLY.                                
05437                                                                   
05438      IF DCT-RETURN-CODE = '07'                                    
05439          MOVE ER-0919            TO EMI-ERROR                     
05440          MOVE -1                 TO GMAINTL                       
05441          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05442          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05443          GO TO 8200-SEND-DATAONLY.                                
05444                                                                   
05445      IF DCT-RETURN-CODE = '04'                                    
05446          MOVE ER-0946            TO EMI-ERROR                     
05447          MOVE -1                 TO GMAINTL                       
05448          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05449          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05450          GO TO 8200-SEND-DATAONLY.                                
05451                                                                   
05452      IF DCT-RETURN-CODE = '05'                                    
05453          MOVE ER-0947            TO EMI-ERROR                     
05454          MOVE -1                 TO GMAINTL                       
05455          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05456          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05457          GO TO 8200-SEND-DATAONLY.                                
05458                                                                   
05459      IF DCT-RETURN-CODE = '08'                                    
05460          MOVE ER-0948            TO EMI-ERROR                     
05461          MOVE -1                 TO GMAINTL                       
05462          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05463          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05464          GO TO 8200-SEND-DATAONLY.                                
05465                                                                   
05466      IF DCT-RETURN-CODE = 'N1'                                    
05467          MOVE ER-0950            TO EMI-ERROR                     
05468          MOVE -1                 TO GMAINTL                       
05469          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05470          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05471          GO TO 8200-SEND-DATAONLY.                                
05472                                                                   
05473      IF DCT-RETURN-CODE = 'E1'                                    
05474          MOVE ER-0974            TO EMI-ERROR                     
05475          MOVE -1                 TO GMAINTL                       
05476          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05477          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05478          GO TO 8200-SEND-DATAONLY.                                
05479                                                                   
05480      IF DCT-RETURN-CODE = 'E2'                                    
05481          MOVE ER-0975            TO EMI-ERROR                     
05482          MOVE -1                 TO GMAINTL                       
05483          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05484          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05485          GO TO 8200-SEND-DATAONLY.                                
05486                                                                   
05487      IF DCT-RETURN-CODE NOT = 'OK'                                
05488           MOVE ER-0949            TO EMI-ERROR                    
05489           MOVE -1                 TO GMAINTL                      
05490           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                
05491           PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                 
05492           GO TO 8200-SEND-DATAONLY.                               
05493                                                                   
05494  8000-CONT.                                                       
05495                                                                   
05496      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.       
05497      MOVE 'CS'                   TO DM-RECORD-TYPE.               
05498      MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.                 
05499      MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.                 
05500      MOVE WS-CL-CLAIM-NO         TO DM-CLAIM-NO.                  
05501      MOVE WS-CL-CERT-NO (4:1)    TO DM-CLAIM-TYPE.                
05502      MOVE WS-CL-CCN              TO DM-CREDIT-CARD-NUMBER.        
05503      MOVE SAVE-DATE-CCYYMMDD     TO DM-STATUS-DATE.               
05504                                                                   
05505      MOVE WS-CL-INSURED-LAST-NAME TO W-NAME-LAST.                 
05506      MOVE WS-CL-INSURED-1ST-NAME  TO W-NAME-FIRST.                
05507      MOVE WS-CL-INSURED-MID-INIT  TO W-NAME-MIDDLE.               
05508      PERFORM 8050-FORMAT-LAST-NAME-1ST THRU 8050-EXIT.            
05509      MOVE WS-NAME-WORK           TO DM-INSURED-NAME.              
05510                                                                   
05511      MOVE 'R'                    TO DM-STAT-CHANGE-TYPE.          
05512                                                                   
05513      IF WS-CL-NO-OF-PMTS-MADE = 0                                 
05514          MOVE '1'                TO DM-CLAIM-STATUS               
05515       ELSE                                                        
05516          MOVE '2'                TO DM-CLAIM-STATUS.              
05517                                                                   
05518      MOVE WS-CL-CERT-CARRIER     TO DM-STAT-CARRIER.              
05519                                                                   
05520      EXEC CICS LINK                                               
05521          PROGRAM    ('DLO025')                                    
05522          COMMAREA   (DMO-COMMUNICATION-AREA)                      
05523          LENGTH     (WS-DMO-LENGTH)                               
05524      END-EXEC.                                                    
05525                                                                   
05526      IF DM-RETURN-CODE = 'OK'                                     
05527          GO TO 8000-EXIT.                                         
05528                                                                   
05529      IF DM-RETURN-CODE = '01'                                     
05530          MOVE ER-8051            TO EMI-ERROR                     
05531          MOVE -1                 TO GMAINTL                       
05532          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05533          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05534          GO TO 8200-SEND-DATAONLY.                                
05535                                                                   
05536      IF DM-RETURN-CODE = '02'                                     
05537          MOVE ER-8052            TO EMI-ERROR                     
05538          MOVE -1                 TO GMAINTL                       
05539          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05540          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05541          GO TO 8200-SEND-DATAONLY.                                
05542                                                                   
05543      IF DM-RETURN-CODE = '03'                                     
05544          MOVE ER-8053            TO EMI-ERROR                     
05545          MOVE -1                 TO GMAINTL                       
05546          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05547          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05548          GO TO 8200-SEND-DATAONLY.                                
05549                                                                   
05550      IF DM-RETURN-CODE = '04'                                     
05551          MOVE ER-8054            TO EMI-ERROR                     
05552          MOVE -1                 TO GMAINTL                       
05553          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05554          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05555          GO TO 8200-SEND-DATAONLY.                                
05556                                                                   
05557      IF DM-RETURN-CODE = '05'                                     
05558          MOVE ER-8055            TO EMI-ERROR                     
05559          MOVE -1                 TO GMAINTL                       
05560          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05561          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05562          GO TO 8200-SEND-DATAONLY.                                
05563                                                                   
05564      IF DM-RETURN-CODE = '06'                                     
05565          MOVE ER-8056            TO EMI-ERROR                     
05566          MOVE -1                 TO GMAINTL                       
05567          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05568          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05569          GO TO 8200-SEND-DATAONLY.                                
05570                                                                   
05571      IF DM-RETURN-CODE = '07'                                     
05572          MOVE ER-8057            TO EMI-ERROR                     
05573          MOVE -1                 TO GMAINTL                       
05574          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05575          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05576          GO TO 8200-SEND-DATAONLY.                                
05577                                                                   
05578      IF DM-RETURN-CODE = '08'                                     
05579          MOVE ER-8058            TO EMI-ERROR                     
05580          MOVE -1                 TO GMAINTL                       
05581          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05582          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05583          GO TO 8200-SEND-DATAONLY.                                
05584                                                                   
05585      IF DM-RETURN-CODE = '09'                                     
05586          MOVE ER-8059            TO EMI-ERROR                     
05587          MOVE -1                 TO GMAINTL                       
05588          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05589          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05590          GO TO 8200-SEND-DATAONLY.                                
05591                                                                   
05592      IF DM-RETURN-CODE = '10'                                     
05593          MOVE ER-8060            TO EMI-ERROR                     
05594          MOVE -1                 TO GMAINTL                       
05595          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05596          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05597          GO TO 8200-SEND-DATAONLY.                                
05598                                                                   
05599      IF DM-RETURN-CODE = '11'                                     
05600          MOVE ER-8061            TO EMI-ERROR                     
05601          MOVE -1                 TO GMAINTL                       
05602          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05603          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05604          GO TO 8200-SEND-DATAONLY.                                
05605                                                                   
05606      IF DM-RETURN-CODE = '12'                                     
05607          MOVE ER-8062            TO EMI-ERROR                     
05608          MOVE -1                 TO GMAINTL                       
05609          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05610          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05611          GO TO 8200-SEND-DATAONLY.                                
05612                                                                   
05613      IF DM-RETURN-CODE = '13'                                     
05614          MOVE ER-8063            TO EMI-ERROR                     
05615          MOVE -1                 TO GMAINTL                       
05616          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05617          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05618          GO TO 8200-SEND-DATAONLY.                                
05619                                                                   
05620      IF DM-RETURN-CODE = '14'                                     
05621          MOVE ER-8064            TO EMI-ERROR                     
05622          MOVE -1                 TO GMAINTL                       
05623          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05624          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05625          GO TO 8200-SEND-DATAONLY.                                
05626                                                                   
05627      IF DM-RETURN-CODE = '15'                                     
05628          MOVE ER-8065            TO EMI-ERROR                     
05629          MOVE -1                 TO GMAINTL                       
05630          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05631          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05632          GO TO 8200-SEND-DATAONLY.                                
05633                                                                   
05634      IF DM-RETURN-CODE = '16'                                     
05635          MOVE ER-8154            TO EMI-ERROR                     
05636          MOVE -1                 TO GMAINTL                       
05637          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05638          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05639          GO TO 8200-SEND-DATAONLY.                                
05640                                                                   
05641      IF DM-RETURN-CODE = '17'                                     
05642          MOVE ER-8155            TO EMI-ERROR                     
05643          MOVE -1                 TO GMAINTL                       
05644          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05645          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05646          GO TO 8200-SEND-DATAONLY.                                
05647                                                                   
05648      IF DM-RETURN-CODE = 'N1'                                     
05649          MOVE ER-8152            TO EMI-ERROR                     
05650          MOVE -1                 TO GMAINTL                       
05651          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05652          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05653          GO TO 8200-SEND-DATAONLY.                                
05654                                                                   
05655      IF DM-RETURN-CODE = 'E1'                                     
05656          MOVE ER-8153            TO EMI-ERROR                     
05657          MOVE -1                 TO GMAINTL                       
05658          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
05659          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT                  
05660          GO TO 8200-SEND-DATAONLY.                                
05661                                                                   
05662      MOVE ER-8066                TO EMI-ERROR.                    
05663      MOVE -1                     TO GMAINTL.                      
05664      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05665      PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT.                     
05666      GO TO 8200-SEND-DATAONLY.                                    
05667                                                                   
05668  8000-NOTE-NOT-FOUND.                                             
05669      MOVE ER-0954                TO EMI-ERROR.                    
05670      MOVE -1                     TO GMAINTL.                      
05671      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05672      PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT.                     
05673      GO TO 8200-SEND-DATAONLY.                                    
05674                                                                   
05675  8000-EXIT.                                                       
05676      EXIT.                                                        
05677  EJECT                                                            
05678  8050-FORMAT-LAST-NAME-1ST.                                       
05679 ***************************************************************** 
05680 *             M O V E   N A M E   R O U T I N E                 * 
05681 *                                                               * 
05682 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          * 
05683 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            * 
05684 *     FIELDS IN THE FOLLOWING WORKING-STORAGE FIELDS.           * 
05685 *                                                               * 
05686 *                  FIELD               VALUE                    * 
05687 *                  -----               -----                    * 
05688 *           W-NAME-LAST    (CL15)      SMITH                    * 
05689 *           W-NAME-FIRST   (CL15)      JOHN                     * 
05690 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  * 
05691 *                                                               * 
05692 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK WILL CONTAIN       * 
05693 *                SMITH, JOHN ALLEN                              * 
05694 *                     OR                                        * 
05695 *                SMITH, JOHN A.                                 * 
05696 *                                                               * 
05697 *     TO USE THIS ROUTINE YOU NEED THE WORKING-STORAGE          * 
05698 *     COPYBOOK, ELCNWA.                                         * 
05699 *****************************************************************.
05700                                                                   
05701      MOVE SPACES                 TO WS-NAME-WORK-AREA.            
05702      MOVE ZERO                   TO WS-NAME-SW.                   
05703      SET NWA-INDEX               TO +1.                           
05704                                                                   
05705      IF W-NAME-LAST   = SPACES  AND                               
05706         W-NAME-MIDDLE = SPACES                                    
05707           MOVE +1                TO WS-NAME-SW.                   
05708                                                                   
05709      MOVE W-NAME-LAST            TO WS-NAME-WORK2.                
05710      PERFORM 8060-MOVE-NAME THRU 8060-EXIT.                       
05711                                                                   
05712      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.                
05713      PERFORM 8060-MOVE-NAME THRU 8060-EXIT.                       
05714                                                                   
05715      SET NWA-INDEX UP BY +1.                                      
05716                                                                   
05717      IF W-NAME-MIDDLE NOT = SPACES                                
05718          IF W-NAME-MIDDLE-2 = SPACES                              
05719              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)             
05720              SET NWA-INDEX UP BY +1                               
05721              MOVE '.'            TO WS-NW (NWA-INDEX)             
05722              SET NWA-INDEX UP BY +2                               
05723          ELSE                                                     
05724              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2                 
05725              PERFORM 8060-MOVE-NAME THRU 8060-EXIT.               
05726                                                                   
05727  8050-EXIT.                                                       
05728      EXIT.                                                        
05729                                                                   
05730  EJECT                                                            
05731  8060-MOVE-NAME.                                                  
05732      IF WS-NAME-SW GREATER THAN +1                                
05733          GO TO 8060-EXIT.                                         
05734                                                                   
05735      IF WS-NAME-WORK2 = SPACES                                    
05736          GO TO 8060-EXIT.                                         
05737                                                                   
05738      SET NWA-INDEX2            TO +1.                             
05739      SET NWA-INDEX3            TO +2.                             
05740                                                                   
05741  8060-MOVE-NAME-CYCLE.                                            
05742      MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).              
05743                                                                   
05744      IF NWA-INDEX LESS THAN +30                                   
05745          SET NWA-INDEX UP BY +1                                   
05746      ELSE                                                         
05747          ADD +2 TO  WS-NAME-SW                                    
05748          GO TO 8060-EXIT.                                         
05749                                                                   
05750      IF NWA-INDEX2 LESS THAN +20                                  
05751          SET NWA-INDEX3 UP BY +1                                  
05752          SET NWA-INDEX2 UP BY +1.                                 
05753                                                                   
05754      IF WS-NW2 (NWA-INDEX2) = SPACES  AND                         
05755         WS-NW2 (NWA-INDEX3) = SPACES                              
05756          IF WS-NAME-SW = ZERO                                     
05757              MOVE ','            TO WS-NW (NWA-INDEX)             
05758              SET NWA-INDEX UP BY +2                               
05759              MOVE +1             TO WS-NAME-SW                    
05760              GO TO 8060-EXIT                                      
05761          ELSE                                                     
05762              GO TO 8060-EXIT.                                     
05763                                                                   
05764      GO TO 8060-MOVE-NAME-CYCLE.                                  
05765                                                                   
05766  8060-EXIT.                                                       
05767      EXIT.                                                        
05768                                                                   
05769      EJECT                                                        
05770                                                                   
05771  8070-UNLOCK-TRLR.                                                
05772      EXEC CICS UNLOCK                                             
05773          DATASET   (WS-ACTIVITY-TRAILERS-DSID)                    
05774      END-EXEC.                                                    
05775                                                                   
05776  8070-EXIT.                                                       
05777       EXIT.                                                       
05778                                                                   
05779      EJECT                                                        
05780  8100-SEND-INITIAL-MAP SECTION.                                   
05781      IF EMI-ERROR NOT = ZERO                                      
05782          PERFORM 9900-ERROR-FORMAT                                
05783        ELSE                                                       
05784          IF TRANSACTION-SUCCESSFUL                                
05785              PERFORM 9900-ERROR-FORMAT.                           
05786                                                                   
05787      MOVE EIBTIME                TO  TIME-IN.                     
05788                                                                   
05789      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             
05790      MOVE '5'                    TO  DC-OPTION-CODE.              
05791      PERFORM 8500-DATE-CONVERSION.                                
05792                                                                   
05793      IF PI-MAP-NAME = EL142A                                      
05794          MOVE DC-GREG-DATE-1-EDIT  TO ADATEO                      
05795          MOVE TIME-OUT             TO ATIMEO                      
05796          MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O                     
05797          MOVE EMI-MESSAGE-AREA (2) TO AEMSG2O                     
05798          MOVE EMI-MESSAGE-AREA (3) TO AEMSG3O                     
05799          GO TO 8110-SEND-MAP.                                     
05800                                                                   
05801      IF PI-MAP-NAME = EL142B                                      
05802          MOVE DC-GREG-DATE-1-EDIT  TO BDATEO                      
05803          MOVE TIME-OUT             TO BTIMEO                      
05804          MOVE EMI-MESSAGE-AREA (1) TO BEMSG1O                     
05805          GO TO 8110-SEND-MAP.                                     
05806                                                                   
05807      IF PI-MAP-NAME = EL142B2                                     
05808          MOVE DC-GREG-DATE-1-EDIT  TO KDATEO                      
05809          MOVE TIME-OUT             TO KTIMEO                      
05810          MOVE EMI-MESSAGE-AREA (1) TO KEMSG1O                     
05811          MOVE EMI-MESSAGE-AREA (2) TO KEMSG2O                     
05812          MOVE EMI-MESSAGE-AREA (3) TO KEMSG3O                     
05813          GO TO 8110-SEND-MAP.                                     
05814                                                                   
05815      IF PI-MAP-NAME = EL142C                                      
05816          MOVE DC-GREG-DATE-1-EDIT  TO CDATEO                      
05817          MOVE TIME-OUT             TO CTIMEO                      
05818          MOVE EMI-MESSAGE-AREA (1) TO CEMSG1O                     
05819          MOVE EMI-MESSAGE-AREA (2) TO CEMSG2O                     
05820          GO TO 8110-SEND-MAP.                                     
05821                                                                   
05822      IF PI-MAP-NAME = EL142D                                      
05823          IF PI-PROCESSOR-ID = 'LGXX'                              
05824              MOVE AL-UNNON       TO  DARCHNOA                     
050110                                     DRESFRMA
050110                                     DAUTOCLA
05825              MOVE AL-UANON       TO  DDTSENTA                     
05826                                      DINPRNTA                     
05827                                      DREPRNTA                     
05828      ELSE                                                         
05829          MOVE AL-SANOF           TO  DARCHNOA                     
050110                                     DRESFRMA
050110                                     DAUTOCLA
05830                                      DDTSENTA                     
05831                                      DINPRNTA                     
05832                                      DREPRNTA.                    
05833                                                                   
050110*05834      IF PI-MAP-NAME = EL142D                                      
050110*05835          IF PI-COMPANY-ID = 'DMD'                                 
050110*05836              MOVE AL-SANON       TO  DMDLETPA                     
050110*05837                                      DMDPURPA                     
050110*05838                                      DMDRELPA.                    
05839                                                          
05840      IF PI-MAP-NAME = EL142D                                      
05841          MOVE DC-GREG-DATE-1-EDIT  TO DDATEO                      
05842          MOVE TIME-OUT             TO DTIMEO                      
05843          MOVE EMI-MESSAGE-AREA (1) TO DEMSG1O                     
05844          GO TO 8110-SEND-MAP.                                     
05845                                                                   
05846      IF PI-MAP-NAME = EL142D2                                     
05847          MOVE DC-GREG-DATE-1-EDIT  TO LDATEO                      
05848          MOVE TIME-OUT             TO LTIMEO                      
05849          MOVE EMI-MESSAGE-AREA (1) TO LEMSG1O                     
05850          MOVE EMI-MESSAGE-AREA (2) TO LEMSG2O                     
05851          MOVE EMI-MESSAGE-AREA (3) TO LEMSG3O                     
05852          GO TO 8110-SEND-MAP.                                     
05853                                                                   
05854      IF PI-MAP-NAME = EL142E                                      
05855          MOVE DC-GREG-DATE-1-EDIT  TO EDATEO                      
05856          MOVE TIME-OUT             TO ETIMEO                      
05857          MOVE EMI-MESSAGE-AREA (1) TO EEMSG1O                     
05858          MOVE EMI-MESSAGE-AREA (2) TO EEMSG2O                     
05859          GO TO 8110-SEND-MAP.                                     
05860                                                                   
05861      IF PI-MAP-NAME = EL142F                                      
05862          MOVE DC-GREG-DATE-1-EDIT  TO FDATEO                      
05863          MOVE TIME-OUT             TO FTIMEO                      
05864          MOVE EMI-MESSAGE-AREA (1) TO FEMSG1O                     
05865          MOVE EMI-MESSAGE-AREA (2) TO FEMSG2O                     
05866          GO TO 8110-SEND-MAP.                                     
05867                                                                   
05868      IF PI-MAP-NAME = EL142G                                      
05869          MOVE DC-GREG-DATE-1-EDIT  TO GDATEO                      
05870          MOVE TIME-OUT             TO GTIMEO                      
05871          MOVE EMI-MESSAGE-AREA (1) TO GEMSG1O                     
05872          MOVE EMI-MESSAGE-AREA (2) TO GEMSG2O
042110         MOVE AL-UANON             TO GRSNCDA
05873          GO TO 8110-SEND-MAP.                                     
05874                                                                   
05875      IF PI-MAP-NAME = EL142H                                      
05876          MOVE DC-GREG-DATE-1-EDIT  TO HDATEO                      
05877          MOVE TIME-OUT             TO HTIMEO                      
05878          MOVE EMI-MESSAGE-AREA (1) TO HEMSG1O                     
05879          GO TO 8110-SEND-MAP.                                     
05880                                                                   
05881      IF PI-MAP-NAME = EL142I                                      
05882          MOVE DC-GREG-DATE-1-EDIT  TO IDATEO                      
05883          MOVE TIME-OUT             TO ITIMEO                      
05884          MOVE EMI-MESSAGE-AREA (1) TO IEMSG1O                     
05885          GO TO 8110-SEND-MAP.                                     
05886                                                                   
05887      IF PI-MAP-NAME = EL142J                                      
05888          MOVE DC-GREG-DATE-1-EDIT  TO JDATEO                      
05889          MOVE TIME-OUT             TO JTIMEO                      
05890          MOVE EMI-MESSAGE-AREA (1) TO JEMSG1O                     
05891          GO TO 8110-SEND-MAP.                                     
05892                                                                   
05893  8110-SEND-MAP.                                                   
05894      IF PI-USES-PAID-TO                                           
05895         IF PI-MAP-NAME = EL142B                                   
05896            MOVE 'PAID  TO     -'    TO BTHRUHDO                   
05897            ELSE                                                   
05898            IF PI-MAP-NAME = EL142I                                
05899               MOVE 'PAID  TO  DATE' TO ITHRUHDO.                  
05900                                                                   
05901      IF PI-COMPANY-ID = 'DMD'                                     
05902          PERFORM  8400-DMD-NOTES-ONLY.                            
05903                                                                   
05904      EXEC CICS SEND                                               
05905          FROM   (EL142DI)                                         
05906          MAPSET (WS-MAPSET-NAME)                                  
05907          MAP    (PI-MAP-NAME)                                     
05908          CURSOR                                                   
05909          ERASE                                                    
05910      END-EXEC.                                                    
05911                                                                   
05912      PERFORM 9100-RETURN-TRAN.                                    
05913                                                                   
05914  8100-EXIT.                                                       
05915      EXIT.                                                        
05916                                                                   
05917      EJECT                                                        
05918  8200-SEND-DATAONLY SECTION.                                      
05919      IF EMI-ERROR NOT = ZERO                                      
05920          PERFORM 9900-ERROR-FORMAT                                
05921        ELSE                                                       
05922          IF TRANSACTION-SUCCESSFUL                                
05923              PERFORM 9900-ERROR-FORMAT.                           
05924                                                                   
05925      MOVE EIBTIME                TO  TIME-IN.                     
05926                                                                   
05927      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             
05928      MOVE '5'                    TO  DC-OPTION-CODE.              
05929      PERFORM 8500-DATE-CONVERSION.                                
05930                                                                   
05931      IF PI-MAP-NAME = EL142A                                      
05932          MOVE DC-GREG-DATE-1-EDIT  TO ADATEO                      
05933          MOVE TIME-OUT             TO ATIMEO                      
05934          MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O                     
05935          MOVE EMI-MESSAGE-AREA (2) TO AEMSG2O                     
05936          MOVE EMI-MESSAGE-AREA (3) TO AEMSG3O                     
05937          GO TO 8210-SEND-MAP.                                     
05938                                                                   
05939      IF PI-MAP-NAME = EL142B                                      
05940          MOVE DC-GREG-DATE-1-EDIT  TO BDATEO                      
05941          MOVE TIME-OUT             TO BTIMEO                      
05942          MOVE EMI-MESSAGE-AREA (1) TO BEMSG1O                     
05943          GO TO 8210-SEND-MAP.                                     
05944                                                                   
05945      IF PI-MAP-NAME = EL142B2                                     
05946          MOVE DC-GREG-DATE-1-EDIT  TO KDATEO                      
05947          MOVE TIME-OUT             TO KTIMEO                      
05948          MOVE EMI-MESSAGE-AREA (1) TO KEMSG1O                     
05949          MOVE EMI-MESSAGE-AREA (2) TO KEMSG2O                     
05950          MOVE EMI-MESSAGE-AREA (3) TO KEMSG3O                     
05951          GO TO 8210-SEND-MAP.                                     
05952                                                                   
05953      IF PI-MAP-NAME = EL142C                                      
05954          MOVE DC-GREG-DATE-1-EDIT  TO CDATEO                      
05955          MOVE TIME-OUT             TO CTIMEO                      
05956          MOVE EMI-MESSAGE-AREA (1) TO CEMSG1O                     
05957          MOVE EMI-MESSAGE-AREA (2) TO CEMSG2O                     
05958          GO TO 8210-SEND-MAP.                                     
05959                                                                   
05960      IF PI-MAP-NAME = EL142D                                      
05961          MOVE DC-GREG-DATE-1-EDIT  TO DDATEO                      
05962          MOVE TIME-OUT             TO DTIMEO                      
05963          MOVE EMI-MESSAGE-AREA (1) TO DEMSG1O                     
05964          GO TO 8210-SEND-MAP.                                     
05965                                                                   
05966      IF PI-MAP-NAME = EL142D2                                     
05967          MOVE DC-GREG-DATE-1-EDIT  TO LDATEO                      
05968          MOVE TIME-OUT             TO LTIMEO                      
05969          MOVE EMI-MESSAGE-AREA (1) TO LEMSG1O                     
05970          MOVE EMI-MESSAGE-AREA (2) TO LEMSG2O                     
05971          MOVE EMI-MESSAGE-AREA (3) TO LEMSG3O                     
05972          GO TO 8210-SEND-MAP.                                     
05973                                                                   
05974      IF PI-MAP-NAME = EL142E                                      
05975          MOVE DC-GREG-DATE-1-EDIT  TO EDATEO                      
05976          MOVE TIME-OUT             TO ETIMEO                      
05977          MOVE EMI-MESSAGE-AREA (1) TO EEMSG1O                     
05978          MOVE EMI-MESSAGE-AREA (2) TO EEMSG2O                     
05979          GO TO 8210-SEND-MAP.                                     
05980                                                                   
05981      IF PI-MAP-NAME = EL142F                                      
05982          MOVE DC-GREG-DATE-1-EDIT  TO FDATEO                      
05983          MOVE TIME-OUT             TO FTIMEO                      
05984          MOVE EMI-MESSAGE-AREA (1) TO FEMSG1O                     
05985          MOVE EMI-MESSAGE-AREA (2) TO FEMSG2O                     
05986          GO TO 8210-SEND-MAP.                                     
05987                                                                   
05988      IF PI-MAP-NAME = EL142G                                      
05989          MOVE DC-GREG-DATE-1-EDIT  TO GDATEO                      
05990          MOVE TIME-OUT             TO GTIMEO                      
05991          MOVE EMI-MESSAGE-AREA (1) TO GEMSG1O                     
05992          MOVE EMI-MESSAGE-AREA (2) TO GEMSG2O                     
05993          GO TO 8210-SEND-MAP.                                     
05994                                                                   
05995      IF PI-MAP-NAME = EL142H                                      
05996          MOVE DC-GREG-DATE-1-EDIT  TO HDATEO                      
05997          MOVE TIME-OUT             TO HTIMEO                      
05998          MOVE EMI-MESSAGE-AREA (1) TO HEMSG1O                     
05999          GO TO 8210-SEND-MAP.                                     
06000                                                                   
06001      IF PI-MAP-NAME = EL142I                                      
06002          MOVE DC-GREG-DATE-1-EDIT  TO IDATEO                      
06003          MOVE TIME-OUT             TO ITIMEO                      
06004          MOVE EMI-MESSAGE-AREA (1) TO IEMSG1O                     
06005          GO TO 8210-SEND-MAP.                                     
06006                                                                   
06007      IF PI-MAP-NAME = EL142J                                      
06008          EXEC CICS ASKTIME ABSTIME(LCP-CICS-DATE)                 
06009          END-EXEC                                                 
06010          EXEC CICS FORMATTIME                                     
06011                    ABSTIME(LCP-CICS-DATE)                         
06012                    YYMMDD(LCP-CURRENT-DATE-68)                    
06013                    DATESEP('/')                                   
06014          END-EXEC                                                 
06015          MOVE LCP-CURRENT-DATE-68  TO JDATEO                      
06016          MOVE TIME-OUT             TO JTIMEO                      
06017          MOVE EMI-MESSAGE-AREA (1) TO JEMSG1O                     
06018          GO TO 8210-SEND-MAP.                                     
06019                                                                   
06020  8210-SEND-MAP.                                                   
06021      IF PI-USES-PAID-TO                                           
06022         IF PI-MAP-NAME = EL142B                                   
06023            MOVE 'PAID  TO     -'    TO BTHRUHDO                   
06024           ELSE                                                    
06025            IF PI-MAP-NAME = EL142I                                
06026               MOVE 'PAID  TO  DATE' TO ITHRUHDO.                  
06027                                                                   
06028      IF PI-COMPANY-ID = 'DMD'                                     
06029          PERFORM  8400-DMD-NOTES-ONLY.                            
06030                                                                   
06031      EXEC CICS SEND DATAONLY                                      
06032          FROM   (EL142DI)                                         
06033          MAPSET (WS-MAPSET-NAME)                                  
06034          MAP    (PI-MAP-NAME)                                     
06035          CURSOR                                                   
06036      END-EXEC.                                                    
06037                                                                   
06038      PERFORM 9100-RETURN-TRAN.                                    
06039                                                                   
06040  8290-EXIT.                                                       
06041      EXIT.                                                        
06042                                                                   
06043      EJECT                                                        
06044  8300-SEND-TEXT SECTION.                                          
06045                                                                   
06046      EXEC CICS SEND TEXT                                          
06047          FROM   (LOGOFF-TEXT)                                     
06048          LENGTH (LOGOFF-LENGTH)                                   
06049          ERASE  FREEKB                                            
06050      END-EXEC.                                                    
06051                                                                   
06052      EXEC CICS RETURN                                             
06053      END-EXEC.                                                    
06054                                                                   
06055  8300-EXIT.                                                       
06056      EXIT.                                                        
06057                                                                   
06058  8400-DMD-NOTES-ONLY SECTION.                                     
06059                                                                   
06060      IF NOT SYSTEM-MODIFY-CAP                                     
06061         IF PI-MAP-NAME = EL142A                                   
06062            IF WS-PI-EL142-PRIORITY = '9'                          
06063              MOVE AL-SANON   TO AREMINDA  ALETTERA  APAYMNTA      
06064                                 AAUTOPAA  ARESEXPA  ADENIALA      
06065                                 AIDCA     AFORMSA                 
06066              MOVE -1         TO ANOTESL.                          
06067                                                                   
06068  8400-EXIT.                                                       
06069      EXIT.                                                        
06070                                                                   
06071      EJECT                                                        
06072  8500-DATE-CONVERSION SECTION.                                    
06073      EXEC CICS LINK                                               
06074          PROGRAM  (ELDATCV)                                       
06075          COMMAREA (DATE-CONVERSION-DATA)                          
06076          LENGTH   (DC-COMM-LENGTH)                                
06077      END-EXEC.                                                    
06078                                                                   
06079                                                                   
06080  8500-EXIT.                                                       
06081      EXIT.                                                        
06082                                                                   
06083  8600-DEEDIT SECTION.                                             
06084      EXEC CICS BIF DEEDIT                                         
06085          FIELD  (WS-DEEDIT-FIELD)                                 
06086          LENGTH (WS-DEEDIT-LENGTH)                                
06087      END-EXEC.                                                    
06088                                                                   
06089  8600-EXIT.                                                       
06090      EXIT.                                                        
06091                                                                   
06092      EJECT                                                        
06093  9000-RETURN-CICS SECTION.                                        
06094      MOVE EL005                  TO  XCTL-PGM.                    
06095      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               
06096      PERFORM 9300-XCTL.                                           
06097                                                                   
06098  9000-EXIT.                                                       
06099      EXIT.                                                        
06100                                                                   
06101  9100-RETURN-TRAN SECTION.                                        
06102      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            
06103      MOVE PI-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        
06104                                                                   
06105      EXEC CICS RETURN                                             
06106          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       
06107          LENGTH   (PI-COMM-LENGTH)                                
06108          TRANSID  (WS-TRANS-ID)                                   
06109      END-EXEC.                                                    
06110                                                                   
06111  9100-EXIT.                                                       
06112      EXIT.                                                        
06113                                                                   
06114  9300-XCTL SECTION.                                               
06115      MOVE DFHENTER               TO  EIBAID.                      
06116                                                                   
06117      EXEC CICS XCTL                                               
06118          PROGRAM  (XCTL-PGM)                                      
06119          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       
06120          LENGTH   (PI-COMM-LENGTH)                                
06121      END-EXEC.                                                    
06122                                                                   
06123  9300-EXIT.                                                       
06124      EXIT.                                                        
06125                                                                   
06126      EJECT                                                        
06127  9400-CLEAR SECTION.                                              
041613     IF PI-MAP-NAME = EL142D2
041613         GO TO 1200-MAIN-LOGIC
041613     END-IF
041613
06128      MOVE PI-RETURN-TO-PROGRAM  TO  XCTL-PGM.                     
06129      PERFORM 9300-XCTL.                                           
06130                                                                   
06131  9400-EXIT.                                                       
06132      EXIT.                                                        
06133                                                                   
06134  9600-PGMIDERR SECTION.                                           
06135      EXEC CICS HANDLE CONDITION                                   
06136          PGMIDERR (8300-SEND-TEXT)                                
06137      END-EXEC.                                                    
06138                                                                   
06139      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.          
06140                                                                   
06141      MOVE EL005                  TO  XCTL-PGM                     
06142                                      LOGOFF-PGM.                  
06143      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 
06144      MOVE SPACES                 TO  PI-ENTRY-CD-1.               
06145      PERFORM 9300-XCTL.                                           
06146                                                                   
06147  9600-EXIT.                                                       
06148      EXIT.                                                        
06149                                                                   
06150      EJECT                                                        
06151  9900-ERROR-FORMAT SECTION.                                       
06152      ADD +1  TO  WS-ERROR-COUNT.                                  
06153                                                                   
06154      IF EMI-ERRORS-COMPLETE                                       
06155          MOVE ER-ZERO            TO  EMI-ERROR                    
06156          GO TO 9900-EXIT.                                         
06157                                                                   
06158      EXEC CICS LINK                                               
06159          PROGRAM  (EL001)                                         
06160          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 
06161          LENGTH   (EMI-COMM-LENGTH)                               
06162      END-EXEC.                                                    
06163                                                                   
06164      MOVE ER-ZERO                TO  EMI-ERROR.                   
06165                                                                   
06166  9900-EXIT.                                                       
06167      EXIT.                                                        
06168                                                                   
06169      EJECT                                                        
06170  9990-ERROR SECTION.                                              
06171      MOVE DFHEIBLK               TO EMI-LINE1.                    
06172      EXEC CICS LINK                                               
06173          PROGRAM  (EL004)                                         
06174          COMMAREA (EMI-LINE1)                                     
06175          LENGTH   (72)                                            
06176      END-EXEC.                                                    
06177                                                                   
06178      PERFORM 8200-SEND-DATAONLY.                                  
06179      GO TO 9100-RETURN-TRAN.                                      
06180                                                                   
06181  9990-EXIT.                                                       
06182      EXIT.                                                        
06183                                                                   
06184  9995-SECURITY-VIOLATION.                                         
06185                              COPY ELCSCTP.                        
06186                                                                   
06187  9995-EXIT.                                                       
06188       EXIT.                                                       
06189                                                                   
06190  9999-LAST-PARAGRAPH SECTION.                                     
06191                                                                   
06192      GOBACK.                                                      
