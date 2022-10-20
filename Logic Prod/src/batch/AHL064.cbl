00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                AHL064.                               
00008 *                           VMOD=2.005.                           
00009                                                                   
00010 *AUTHOR.        LOGIC, INC.                                       
00011 *               DALLAS, TEXAS.                                    
00012                                                                   
00013 *DATE-COMPILED.                                                   
00014                                                                   
00015 *SECURITY.   *****************************************************
00016 *            *                                                   *
00017 *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
00018 *            *                                                   *
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00020 *            *   OF    CSO      IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF CSO             *
00022 *            *                                                   *
00023 *            *****************************************************
00024                                                                   
00025 *REMARKS.                                                         
00026 *        THIS PROGRAM PRINTS THE AGED ACCOUNTS RECEIVABLE.        
062104******************************************************************
062104*                   C H A N G E   L O G
062104*
062104* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062104*-----------------------------------------------------------------
062104*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062104* EFFECTIVE    NUMBER
062104*-----------------------------------------------------------------
082012* 082012  CR2012042700005  PEMA  ADD OVER 120 DAYS TO AGEING
062104******************************************************************
00027                                                                   
00028  ENVIRONMENT DIVISION.                                            
00029  INPUT-OUTPUT SECTION.                                            
00030  FILE-CONTROL.                                                    
00031                                                                   
00032      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   
00033      SELECT COMM-MSTR-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.   

062104     SELECT ME50-ECS064-BALANCE
062104                             ASSIGN TO SYS012
062104                             ORGANIZATION IS LINE SEQUENTIAL.

00034      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   
00035      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   
00036  EJECT                                                            
00037  DATA DIVISION.                                                   
00038  FILE SECTION.                                                    
00039                                                                   
00040  FD  PRNTR                                                        
00007      RECORDING MODE F                                             
00008      LABEL RECORDS OMITTED                                        
00009      BLOCK CONTAINS 0 RECORDS
00010      RECORD CONTAINS 150 CHARACTERS.                              
00011  01  PRT.                                                         
00012      12  P-CTL               PIC  X.                              
00013      12  P-DATA              PIC  X(149).                         
00043  FD  COMM-MSTR-IN                                                 
00044                              COPY ECSCOIFD.                       
00045  EJECT                                                            

062104 FD  ME50-ECS064-BALANCE
062104     RECORDING MODE IS F
062104     BLOCK CONTAINS 0 RECORDS.
062104 01  ME50-ECS064-BALANCE-REC    PIC X(95).

00046  FD  DISK-DATE                                                    
00047                              COPY ELCDTEFD.                       
00048  EJECT                                                            
00049  FD  FICH                                                         
00050                              COPY ELCFCHFD.                       
00051  EJECT                                                            
00052  WORKING-STORAGE SECTION.                                         
00053  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
00054  77  FILLER PIC X(32) VALUE '********************************'.   
00055  77  FILLER PIC X(32) VALUE '*            AHL064            *'.   
00056  77  FILLER PIC X(32) VALUE '*********** VMOD=2.001 *********'.   
00057                                                                   
00058  77  PGM-SUB                 PIC S9(03)      VALUE +64  COMP-3.   
00059  77  SPACE-NP                PIC  X(01)      VALUE '1'.           
00060  77  SPACE-1                 PIC  X(01)      VALUE ' '.           
00061  77  SPACE-2                 PIC  X(01)      VALUE '0'.           
00062  77  SPACE-3                 PIC  X(01)      VALUE '-'.           
00063  77  X                       PIC  X(01)      VALUE '1'.           
00064                                                                   
00065  01  REQUIRED-STORAGE.                                            
00066      12  WS-RETURN-CODE          PIC S9(04)             COMP.     
00067      12  WS-ABEND-MESSAGE        PIC  X(80).                      
00068      12  WS-ABEND-FILE-STATUS    PIC  X(02)  VALUE ZEROS.         
00069      12  WS-ZERO                 PIC S9(01)  VALUE ZERO COMP-3.   
00070  EJECT                                                            
00071                              COPY ERCCOMP.                        


062104 01  WS-BAL50-DESCRIPTION          PIC X(50)  VALUE
062104     'ECS064 End Bal should match EL509 Current Bal     '.

062104 01  WS-ME50-BALANCE-REC.
062104     12  WS-ME50-BAL-JOB           PIC X(11)  VALUE SPACES.
062104     12  WS-ME50-BAL-DELIM1        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-STEP          PIC X(08)  VALUE 'ECS064  '.
062104     12  WS-ME50-BAL-DELIM2        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-LOW       PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM3        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-HIGH      PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM4        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-DESCRIP       PIC X(50)  VALUE SPACES.

00073  01  COMP-WORKING-AMOUNTS.                                        
00074      12  COMP-3-AREA     COMP-3.                                  
00075          16  COMP-AMT-BEG    PIC S9(09)V9(02).                    
00076          16  COMP-AMT-COM    PIC S9(09)V9(02).                    
00077          16  COMP-AMT-CHG    PIC S9(09)V9(02).                    
00078          16  COMP-AMT-PMT    PIC S9(09)V9(02).                    
00079          16  COMP-AMT-END    PIC S9(09)V9(02).                    
00080          16  COMP-AMT-CUR    PIC S9(09)V9(02).                    
00081          16  COMP-AMT-OV30   PIC S9(09)V9(02).                    
00082          16  COMP-AMT-OV60   PIC S9(09)V9(02).                    
00083          16  COMP-AMT-OV90   PIC S9(09)V9(02).                    
00083          16  COMP-AMT-OV120  PIC S9(09)V9(02).                    
00084  01  MISC-WORKING-STORAGE.                                        
00085      12  COMP-3-AREA     COMP-3.                                  
00086          16  PGCTR           PIC S9(05)      VALUE +0.            
00087          16  LNCTR           PIC S9(03)      VALUE +66.           
00088      12  GA-TOTALS       COMP-3.                                  
00089          16  G-CTR           PIC S9(07).                          
00090          16  G-BEG           PIC S9(09)V9(02).                    
00091          16  G-CHG           PIC S9(09)V9(02).                    
00092          16  G-PMT           PIC S9(09)V9(02).                    
00093          16  G-END           PIC S9(09)V9(02).                    
00094          16  G-OV30          PIC S9(09)V9(02).                    
00095          16  G-OV60          PIC S9(09)V9(02).                    
00096          16  G-OV90          PIC S9(09)V9(02).                    
00096          16  G-OV120         PIC S9(09)V9(02).                    
00097          16  G-DBT           PIC S9(09)V9(02).                    
00098          16  G-CRD           PIC S9(09)V9(02).                    
00099      12  SUB-TOTALS      COMP-3.                                  
00100          16  S-BEG           PIC S9(09)V9(02).                    
00101          16  S-CHG           PIC S9(09)V9(02).                    
00102          16  S-PMT           PIC S9(09)V9(02).                    
00103          16  S-END           PIC S9(09)V9(02).                    
00104          16  S-OV30          PIC S9(09)V9(02).                    
00105          16  S-OV60          PIC S9(09)V9(02).                    
00106          16  S-OV90          PIC S9(09)V9(02).                    
00106          16  S-OV120         PIC S9(09)V9(02).                    
00107          16  S-DBT           PIC S9(09)V9(02).                    
00108          16  S-CRD           PIC S9(09)V9(02).                    
00109      12  CARRIER-TOTALS  COMP-3.                                  
00110          16  C-BEG           PIC S9(09)V9(02).                    
00111          16  C-CHG           PIC S9(09)V9(02).                    
00112          16  C-PMT           PIC S9(09)V9(02).                    
00113          16  C-END           PIC S9(09)V9(02).                    
00114          16  C-OV30          PIC S9(09)V9(02).                    
00115          16  C-OV60          PIC S9(09)V9(02).                    
00116          16  C-OV90          PIC S9(09)V9(02).                    
00116          16  C-OV120         PIC S9(09)V9(02).                    
00117          16  C-DBT           PIC S9(09)V9(02).                    
00118          16  C-CRD           PIC S9(09)V9(02).                    
00119      12  FINAL-TOTALS    COMP-3.                                  
00120          16  F-BEG           PIC S9(09)V9(02).                    
00121          16  F-CHG           PIC S9(09)V9(02).                    
00122          16  F-PMT           PIC S9(09)V9(02).                    
00123          16  F-END           PIC S9(09)V9(02).                    
00124          16  F-OV30          PIC S9(09)V9(02).                    
00125          16  F-OV60          PIC S9(09)V9(02).                    
00126          16  F-OV90          PIC S9(09)V9(02).                    
00126          16  F-OV120         PIC S9(09)V9(02).                    
00127          16  F-DBT           PIC S9(09)V9(02).                    
00128          16  F-CRD           PIC S9(09)V9(02).                    
00129      12  CUR-CTL-1.                                               
00130          16  CUR-CARR-GROUP.                                      
00131              20  CUR-CARR    PIC  X(01).                          
00132              20  CUR-GROUP   PIC  X(06).                          
00133          16  CUR-RESP        PIC  X(10).                          
00134      12  PRE-CONTROL.                                             
00135          16  PRE-CTL-1.                                           
00136              20  PRE-CARR-GROUP.                                  
00137                  24  PRE-CARR    PIC  X(01).                      
00138                  24  PRE-GROUP   PIC  X(06).                      
00139              20  PRE-RESP        PIC  X(10).                      
00140          16  PRE-CTL-2.                                           
00141              20  PRE-ACCT        PIC  X(10).                      
00142      12  BALANCE-SW          PIC  X(01).                          
00143          88  ACCOUNT-CARRIES-BALANCE         VALUE 'A'.           
00144          88  AGENT-CARRIES-BALANCE           VALUE 'G'.           
00145      12  SAVE-COMPANY-NAME   PIC  X(30).                          
00146  EJECT                                                            
00147  01  HD1.                                                         
00148      12  FILLER              PIC  X(62)      VALUE SPACES.        
00149      12  FILLER              PIC  X(24)      VALUE                
00150              'AGED ACCOUNTS RECEIVABLE'.      
00151      12  FILLER              PIC  X(49)      VALUE SPACES.        
00152      12  FILLER              PIC  X(08)      VALUE 'ECS064'.      
00153                                                                   
00154  01  HD2.                                                         
00155      12  FILLER              PIC  X(60)      VALUE SPACES.        
00156      12  HD-CO               PIC  X(30).                          
00157      12  FILLER              PIC  X(45)      VALUE SPACES.        
00158      12  HD-RUN-DT           PIC  X(08)      VALUE SPACES.        
00159                                                                   
00160  01  HD3.                                                         
00161      12  FILLER              PIC  X(63)      VALUE SPACES.        
00162      12  HD-DT               PIC  X(18).                          
00163      12  FILLER              PIC  X(54)      VALUE SPACES.        
00164      12  FILLER              PIC  X(05)      VALUE 'PAGE '.       
00165      12  HD-PG               PIC ZZ,ZZ9.                          
00166                                                                   
00167  01  HD4.                                                         
00168      12  FILLER              PIC  X(44)      VALUE                
00169              '-- ACCOUNT NUMBERS --                BEGINNI'.      
00170      12  FILLER              PIC  X(44)      VALUE                
00171              'NG      CURRENT      PAYMENTS       ENDING  '.      
00172      12  FILLER              PIC  X(56)      VALUE                
00173              '  ----------------------- A G I N G ----------------
      -            '----'.

00175  01  HD5.                                                         
00176      12  FILLER              PIC  X(44)      VALUE                
00177              '-------- ACCOUNT NAME --------        BALANC'.      
00178      12  FILLER              PIC  X(44)      VALUE                
00179              'E       CHARGES     ADJUSTMENTS     BALANCE '.      
00180      12  FILLER              PIC  X(55)      VALUE                
00181              '      OVER 30       OVER 60       OVER 90       OVER
      -            '120'.

00182  EJECT                                                            
00183  01  P-REC.                                                       
00184      12  P-CCSW              PIC  X(01).                          
00185      12  P-LINE.                                                  
00186          16 FILLER           PIC  X(149).                         
00187      12  P-LINE-1 REDEFINES P-LINE.                               
00188          16  P-MSG           PIC  X(07).                          
00189          16  FILLER          PIC  X(01).                          
00190          16  P-DASH          PIC  X(01).                          
00191          16  FILLER          PIC  X(01).                          
00192          16  P-CARR          PIC  X(01).                          
00193          16  FILLER          PIC  X(01).                          
00194          16  P-GROUP         PIC  X(06).                          
00195          16  FILLER          PIC  X(04).                          
00196          16  P-TOT-TITLE     PIC  X(12).                          
00197          16  FILLER          PIC  X(98).                          
00198      12  P-LINE-2 REDEFINES P-LINE.                               
00199          16  P-RESP          PIC  X(10).                          
00200          16  FILLER          PIC  X(01).                          
00201          16  P-ACCT          PIC  X(10).                          
00202          16  FILLER          PIC  X(111).                         
00203      12  P-LINE-3 REDEFINES P-LINE.                               
00204          16  P-NAME          PIC  X(30).                          
00205          16  FILLER          PIC  X(04).                          
00206          16  P-BEG           PIC ZZ,ZZZ,ZZZ.ZZ-.                  
00207          16  P-CHG           PIC ZZ,ZZZ,ZZZ.ZZ-.                  
00208          16  P-PMT           PIC ZZ,ZZZ,ZZZ.ZZ-.                  
00209          16  P-END           PIC ZZ,ZZZ,ZZZ.ZZ-.                  
00210          16  P-OV30          PIC ZZ,ZZZ,ZZZ.ZZ-.                  
00211          16  P-OV60          PIC ZZ,ZZZ,ZZZ.ZZ-.                  
00212          16  P-OV90          PIC ZZ,ZZZ,ZZZ.ZZ-.                  
00212          16  P-OV120         PIC ZZ,ZZZ,ZZZ.ZZ-.                  
00213      12  T-LINE-1 REDEFINES P-LINE.                               
00214          16  T-NAME          PIC  X(30).                          
00215          16  FILLER          PIC  X(03).                          
00216          16  T-BEG           PIC ZZZ,ZZZ,ZZZ.ZZ-.                 
00217          16  FILLER          PIC  X(13).                          
00218          16  T-PMT           PIC ZZZ,ZZZ,ZZZ.ZZ-.                 
00219          16  FILLER          PIC  X(13).                          
00220          16  T-OV30          PIC ZZZ,ZZZ,ZZZ.ZZ-.                 
00221          16  FILLER          PIC  X(13).                          
00222          16  T-OV90          PIC ZZZ,ZZZ,ZZZ.ZZ-.                 
00223      12  T-LINE-2 REDEFINES P-LINE.                               
00224          16  FILLER          PIC  X(47).                          
00225          16  T-CHG           PIC ZZZ,ZZZ,ZZZ.ZZ-.                 
00226          16  FILLER          PIC  X(13).                          
00227          16  T-END           PIC ZZZ,ZZZ,ZZZ.ZZ-.                 
00228          16  FILLER          PIC  X(13).                          
00229          16  T-OV60          PIC ZZZ,ZZZ,ZZZ.ZZ-.                 
00230          16  FILLER          PIC  X(14).                          
00222          16  T-OV120         PIC ZZZ,ZZZ,ZZZ.ZZ-.                 
00231  EJECT                                                            
00232                              COPY ELCDTECX.                       
00233  EJECT                                                            
00234                              COPY ELCDTEVR.                       
00235  EJECT                                                            
00236  PROCEDURE DIVISION.                                              
00237                                                                   
00238  0000-STANDARD-RTN.                                               
00239                              COPY ELCDTERX.                       
00240  EJECT                                                            
00241  1000-INITIALIZE-OUTPUT.                                          
00242      OPEN INPUT  COMM-MSTR-IN                                     
00243           OUTPUT PRNTR
062104                 ME50-ECS064-BALANCE.
00244                                                                   
00245      MOVE LOW-VALUE              TO  COMPENSATION-MASTER          
00246                                      COMP-IN-RECORD               
00247                                      CUR-CTL-1                    
00248                                      PRE-CONTROL.                 
00249      MOVE COMPANY-NAME           TO  HD-CO.                       
00250      MOVE SPACES                 TO  SAVE-COMPANY-NAME.           
00251      MOVE ALPH-DATE              TO  HD-DT.                       
00252      MOVE WS-CURRENT-DATE        TO  HD-RUN-DT.                   
00253      MOVE SPACE-NP               TO  P-REC.                       

062104     IF DTE-CLIENT = 'CID'
062104         MOVE 'CILGM35'          TO  WS-ME50-BAL-JOB
062104     ELSE
030612       IF DTE-CLIENT = 'AHL'
030612         MOVE 'AHLGM35'          TO  WS-ME50-BAL-JOB
030612       ELSE
062104         MOVE 'CIDCLGM35'        TO  WS-ME50-BAL-JOB
030612       END-IF
062104     END-IF.
00254                                                                   
00255      GO TO 2000-PROCESS-RTN.                                      
00256  EJECT                                                            
00257  2000-PROCESS-RTN.                                                
00258      PERFORM 8000-MSTR-CONTROL-RTN  THRU  8099-EXIT.              
00259                                                                   
00260      IF CO-CTL-1  IS NOT EQUAL TO  CUR-CTL-1                      
00261          PERFORM 6000-BREAK-RTN  THRU  6999-EXIT.                 
00262                                                                   
00263      IF CO-CTL-1  IS EQUAL TO  HIGH-VALUE                         
00264          GO TO 9990-E-O-J.                                        
00265                                                                   
00266      IF CO-COMPANY-TYPE                                           
00267          GO TO 2000-PROCESS-RTN.                                  
00268                                                                   
00269 ******************************************************************
00270 *    APPLIES TO A/R USERS.                                        
00271 *    DTE TOTAL OPTIONS IS TO ALLOW PROGRAM TO USE AMOUNTS THAT    
00272 *    REPRESENT EITHER THE LAST MONTH END RUN OR THE LAST A/R CYCLE
00273 *    THAT WAS RUN (OPTION 2).                                     
00274 ******************************************************************
00275                                                                   
00276      IF DTE-SYS-G-AR-USED NOT = '1' OR                            
00277         DTE-TOT-OPT = '1'                                         
00278          MOVE CO-BAL-FWD         TO  COMP-AMT-BEG                 
00279          MOVE CO-CUR-COM         TO  COMP-AMT-COM                 
00280          MOVE CO-CUR-CHG         TO  COMP-AMT-CHG                 
00281          MOVE CO-CUR-PMT         TO  COMP-AMT-PMT                 
00282          MOVE CO-END-BAL         TO  COMP-AMT-END                 
00283          MOVE CO-CUR             TO  COMP-AMT-CUR                 
00284          MOVE CO-OV30            TO  COMP-AMT-OV30                
00285          MOVE CO-OV60            TO  COMP-AMT-OV60                
00286          MOVE CO-OV90            TO  COMP-AMT-OV90                
00286          MOVE CO-OV120           TO  COMP-AMT-OV120
00287       ELSE                                                        
00288          MOVE CO-CURRENT-BAL-FWD TO  COMP-AMT-BEG                 
00289          MOVE CO-CURRENT-CUR-COM TO  COMP-AMT-COM                 
00290          MOVE CO-CURRENT-CUR-CHG TO  COMP-AMT-CHG                 
00291          MOVE CO-CURRENT-CUR-PMT TO  COMP-AMT-PMT                 
00292          MOVE CO-CURRENT-END-BAL TO  COMP-AMT-END                 
00293          MOVE CO-CURRENT-CUR     TO  COMP-AMT-CUR                 
00294          MOVE CO-CURRENT-OV30    TO  COMP-AMT-OV30                
00295          MOVE CO-CURRENT-OV60    TO  COMP-AMT-OV60                
00296          MOVE CO-CURRENT-OV90    TO  COMP-AMT-OV90
00296          MOVE CO-CURRENT-OV120   TO  COMP-AMT-OV120.
00297                                                                   
00298      IF COMP-AMT-BEG      IS EQUAL TO +0                          
00299        AND  COMP-AMT-COM  IS EQUAL TO +0                          
00300        AND  COMP-AMT-CHG  IS EQUAL TO +0                          
00301        AND  COMP-AMT-PMT  IS EQUAL TO +0                          
00302        AND  COMP-AMT-END  IS EQUAL TO +0                          
00303        AND  COMP-AMT-CUR  IS EQUAL TO +0                          
00304        AND  COMP-AMT-OV30 IS EQUAL TO +0                          
00305        AND  COMP-AMT-OV60 IS EQUAL TO +0                          
00306        AND  COMP-AMT-OV90 IS EQUAL TO +0                          
00306        AND  COMP-AMT-OV120 IS EQUAL TO +0                          
00307          GO TO 2000-PROCESS-RTN.                                  
00308                                                                   
00309      SUBTRACT COMP-AMT-COM       FROM  COMP-AMT-CHG.              
00310                                                                   
00311      IF LNCTR  IS GREATER THAN  +056                              
00312          PERFORM 8600-HD-RTN  THRU  8699-EXIT.                    
00313                                                                   
00314      MOVE CO-RESP-NO             TO  P-RESP.                      
00315      MOVE SPACE-2                TO  P-CCSW.                      
00316                                                                   
00317      IF CO-ACCOUNT  IS NOT EQUAL TO  LOW-VALUE                    
00318          MOVE CO-ACCOUNT         TO  P-ACCT.                      
00319                                                                   
00320      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00321                                                                   
00322      MOVE CO-ACCT-NAME           TO  P-NAME.                      
00323      MOVE COMP-AMT-BEG           TO  P-BEG.                       
00324      MOVE COMP-AMT-CHG           TO  P-CHG.                       
00325      MOVE COMP-AMT-PMT           TO  P-PMT.                       
00326      MOVE COMP-AMT-END           TO  P-END.                       
00327      MOVE COMP-AMT-OV30          TO  P-OV30.                      
00328      MOVE COMP-AMT-OV60          TO  P-OV60.                      
00329      MOVE COMP-AMT-OV90          TO  P-OV90.                      
00329      MOVE COMP-AMT-OV120         TO  P-OV120.
00330                                                                   
00331      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00332                                                                   
00333      ADD +1                      TO  G-CTR.                       
00334      ADD COMP-AMT-BEG            TO  G-BEG.                       
00335      ADD COMP-AMT-CHG            TO  G-CHG.                       
00336      ADD COMP-AMT-PMT            TO  G-PMT.                       
00337      ADD COMP-AMT-END            TO  G-END.                       
00338      ADD COMP-AMT-OV30           TO  G-OV30.                      
00339      ADD COMP-AMT-OV60           TO  G-OV60.                      
00340      ADD COMP-AMT-OV90           TO  G-OV90.                      
00340      ADD COMP-AMT-OV120          TO  G-OV120.
00341                                                                   
00342      IF COMP-AMT-END IS GREATER THAN ZERO                         
00343          ADD COMP-AMT-END        TO  G-DBT                        
00344      ELSE                                                         
00345          ADD COMP-AMT-END        TO  G-CRD.                       
00346                                                                   
00347      GO TO 2000-PROCESS-RTN.                                      
00348  EJECT                                                            
00349  6000-BREAK-RTN.                                                  
00350      IF CUR-CTL-1  IS EQUAL TO  LOW-VALUE                         
00351          GO TO 6500-INITIALIZE-ALL.                               
00352                                                                   
00353  6100-AGENT-BREAK.                                                
00354      ADD G-BEG                   TO  S-BEG.                       
00355      ADD G-CHG                   TO  S-CHG.                       
00356      ADD G-PMT                   TO  S-PMT.                       
00357      ADD G-END                   TO  S-END.                       
00358      ADD G-OV30                  TO  S-OV30.                      
00359      ADD G-OV60                  TO  S-OV60.                      
00360      ADD G-OV90                  TO  S-OV90.                      
00360      ADD G-OV120                 TO  S-OV120.
00361      ADD G-DBT                   TO  S-DBT.                       
00362      ADD G-CRD                   TO  S-CRD.                       
00363                                                                   
00364      IF G-CTR  IS LESS THAN  +2                                   
00365          GO TO 6200-COMPANY-BREAK.                                
00366                                                                   
00367      MOVE 'GENERAL AGENT TOTALS'  TO  P-NAME.                     
00368                                                                   
00369      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00370                                                                   
00371      MOVE G-BEG                   TO  T-BEG.                      
00372      MOVE G-PMT                   TO  T-PMT.                      
00373      MOVE G-OV30                  TO  T-OV30.                     
00374      MOVE G-OV90                  TO  T-OV90.                     
00375                                                                   
00376      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00377                                                                   
00378      MOVE G-CHG                   TO  T-CHG.                      
00379      MOVE G-END                   TO  T-END.                      
00380      MOVE G-OV60                  TO  T-OV60.                     
00374      MOVE G-OV120                 TO  T-OV120.
00381                                                                   
00382      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00383                                                                   
00384      MOVE SPACE-2 TO P-CCSW.                                      
00385                                                                   
00386  6200-COMPANY-BREAK.                                              
00387      IF CUR-CARR-GROUP  IS EQUAL TO  CO-CARR-GROUP                
00388          GO TO 6800-INITIALIZE-GA.                                
00389                                                                   
00390      MOVE 'COMPANY'              TO  P-MSG.                       
00391      MOVE CUR-CARR               TO  P-CARR.                      
00392      MOVE CUR-GROUP              TO  P-GROUP.                     
00393      MOVE ' TOTALS'              TO  P-TOT-TITLE.                 
00394                                                                   
00395      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00396                                                                   
00397      MOVE S-BEG                  TO  T-BEG.                       
00398      MOVE S-PMT                  TO  T-PMT.                       
00399      MOVE S-OV30                 TO  T-OV30.                      
00400      MOVE S-OV90                 TO  T-OV90.                      
00401      MOVE SPACE-2                TO  P-CCSW.                      
00402                                                                   
00403      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00404                                                                   
00405      MOVE S-CHG                  TO  T-CHG.                       
00406      MOVE S-END                  TO  T-END.                       
00407      MOVE S-OV60                 TO  T-OV60.                      
00407      MOVE S-OV120                TO  T-OV120.
00408                                                                   
00409      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00410                                                                   
00411      MOVE 'TOTAL DEBIT BALANCE'  TO  P-NAME.                      
00412      MOVE S-DBT                  TO  P-END.                       
00413      MOVE SPACE-2                TO  P-CCSW.                      
00414                                                                   
00415      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00416                                                                   
00417      MOVE 'TOTAL CREDIT BALANCE'  TO  P-NAME.                     
00418      MOVE S-CRD                   TO  P-END.                      
00419                                                                   
00420      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00421                                                                   
00422      ADD S-BEG                   TO  C-BEG.                       
00423      ADD S-CHG                   TO  C-CHG.                       
00424      ADD S-PMT                   TO  C-PMT.                       
00425      ADD S-END                   TO  C-END.                       
00426      ADD S-OV30                  TO  C-OV30.                      
00427      ADD S-OV60                  TO  C-OV60.                      
00428      ADD S-OV90                  TO  C-OV90.                      
00428      ADD S-OV120                 TO  C-OV120.
00429      ADD S-DBT                   TO  C-DBT.                       
00430      ADD S-CRD                   TO  C-CRD.                       
00431                                                                   
00432  6300-CARRIER-BREAK.                                              
00433      IF CO-CARRIER  IS EQUAL  TO CUR-CARR                         
00434          GO TO 6700-INITIALIZE-COMPANY.                           
00435                                                                   
00436      MOVE SPACES                 TO  CUR-GROUP.                   
00437                                                                   
00438      PERFORM 8600-HD-RTN  THRU  8699-EXIT.                        
00439                                                                   
00440      MOVE 'CARRIER'              TO  P-MSG.                       
00441      MOVE CUR-CARR               TO  P-CARR.                      
00442      MOVE ' TOTALS'              TO  P-TOT-TITLE.                 
00443                                                                   
00444      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00445                                                                   
00446      MOVE C-BEG                  TO  T-BEG.                       
00447      MOVE C-PMT                  TO  T-PMT.                       
00448      MOVE C-OV30                 TO  T-OV30.                      
00449      MOVE C-OV90                 TO  T-OV90.                      
00450      MOVE SPACE-2                TO  P-CCSW.                      
00451                                                                   
00452      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00453                                                                   
00454      MOVE C-CHG                  TO  T-CHG.                       
00455      MOVE C-END                  TO  T-END.                       
00456      MOVE C-OV60                 TO  T-OV60.                      
00456      MOVE C-OV120                TO  T-OV120.
00457                                                                   
00458      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00459                                                                   
00460      MOVE 'TOTAL DEBIT BALANCE'  TO  P-NAME.                      
00461      MOVE C-DBT                  TO  P-END.                       
00462      MOVE SPACE-2                TO  P-CCSW.                      
00463                                                                   
00464      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00465                                                                   
00466      MOVE 'TOTAL CREDIT BALANCE'  TO  P-NAME.                     
00467      MOVE C-CRD                   TO  P-END.                      
00468                                                                   
00469      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00470                                                                   
00471      ADD C-BEG                   TO  F-BEG.                       
00472      ADD C-CHG                   TO  F-CHG.                       
00473      ADD C-PMT                   TO  F-PMT.                       
00474      ADD C-END                   TO  F-END.                       
00475      ADD C-OV30                  TO  F-OV30.                      
00476      ADD C-OV60                  TO  F-OV60.                      
00477      ADD C-OV90                  TO  F-OV90.                      
00477      ADD C-OV120                 TO  F-OV120                      
00478      ADD C-DBT                   TO  F-DBT.                       
00479      ADD C-CRD                   TO  F-CRD.                       
00480                                                                   
00481  6030-FINAL-BREAK.                                                
00482      IF CO-CTL-1  IS NOT EQUAL TO  HIGH-VALUE                     
00483          GO TO 6600-INITIALIZE-CARRIER.                           
00484                                                                   
00485      MOVE SPACES                 TO  CUR-CARR.                    
00486                                                                   
00487      PERFORM 8600-HD-RTN  THRU  8699-EXIT.                        
00488                                                                   
00489      MOVE 'FINAL TOTALS'         TO  P-TOT-TITLE.                 
00490                                                                   
00491      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00492                                                                   
00493      MOVE F-BEG                  TO  T-BEG.                       
00494      MOVE F-PMT                  TO  T-PMT.                       
00495      MOVE F-OV30                 TO  T-OV30.                      
00496      MOVE F-OV90                 TO  T-OV90.                      
00497      MOVE SPACE-2                TO  P-CCSW.                      
00498                                                                   
00499      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00500                                                                   
00501      MOVE F-CHG                  TO  T-CHG.                       
00502      MOVE F-END                  TO  T-END.                       
00503      MOVE F-OV60                 TO  T-OV60.                      
00503      MOVE F-OV120                TO  T-OV120.
00504                                                                   
00505      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       

062104     MOVE F-END                  TO WS-ME50-BAL-AMT-LOW.
062104     MOVE F-END                  TO WS-ME50-BAL-AMT-HIGH.
062104     MOVE WS-BAL50-DESCRIPTION   TO WS-ME50-BAL-DESCRIP.
062104     WRITE ME50-ECS064-BALANCE-REC FROM WS-ME50-BALANCE-REC.
00506                                                                   
00507      MOVE 'TOTAL DEBIT BALANCE'  TO  P-NAME.                      
00508      MOVE F-DBT                  TO  P-END.                       
00509      MOVE SPACE-2                TO  P-CCSW.                      
00510                                                                   
00511      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00512                                                                   
00513      MOVE 'TOTAL CREDIT BALANCE'  TO  P-NAME.                     
00514      MOVE F-CRD                   TO  P-END.                      
00515                                                                   
00516      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00517                                                                   
00518      GO TO 6999-EXIT.                                             
00519                                                                   
00520  6500-INITIALIZE-ALL.                                             
00521      MOVE +0                     TO  F-BEG   F-CHG                
00522                                      F-PMT   F-END                
00523                                      F-OV30  F-OV60               
00524                                      F-OV90  F-DBT                
00525                                      F-CRD   F-OV120.
00526                                                                   
00527  6600-INITIALIZE-CARRIER.                                         
00528      MOVE CO-CARRIER             TO  CUR-CARR.                    
00529      MOVE +0                     TO  C-BEG   C-CHG                
00530                                      C-PMT   C-END                
00531                                      C-OV30  C-OV60               
00532                                      C-OV90  C-DBT                
00533                                      C-CRD   C-OV120.
00534                                                                   
00535      IF SAVE-COMPANY-NAME  IS EQUAL TO  SPACES                    
00536          MOVE COMPANY-NAME       TO  HD-CO                        
00537      ELSE                                                         
00538          MOVE SAVE-COMPANY-NAME  TO  HD-CO.                       
00539                                                                   
00540  6700-INITIALIZE-COMPANY.                                         
00541      MOVE CO-GROUPING            TO  CUR-GROUP.                   
00542      MOVE +0                     TO  S-BEG   S-CHG                
00543                                      S-PMT   S-END                
00544                                      S-OV30  S-OV60               
00545                                      S-OV90  S-DBT                
00546                                      S-CRD   S-OV120.
00547                                                                   
00548      IF SAVE-COMPANY-NAME  IS EQUAL TO  SPACES                    
00549          MOVE COMPANY-NAME       TO  HD-CO                        
00550      ELSE                                                         
00551          MOVE SAVE-COMPANY-NAME  TO  HD-CO.                       
00552                                                                   
00553      MOVE +066                   TO  LNCTR.                       
00554                                                                   
00555  6800-INITIALIZE-GA.                                              
00556      MOVE CO-RESP-NO             TO  CUR-RESP.                    
00557      MOVE +0                     TO  G-CTR.                       
00558      MOVE +0                     TO  G-BEG   G-CHG                
00559                                      G-PMT   G-END                
00560                                      G-OV30  G-OV60               
00561                                      G-OV90  G-DBT                
00562                                      G-CRD   G-OV120.
00563                                                                   
00564  6999-EXIT.                                                       
00565      EXIT.                                                        
00566  EJECT                                                            
00567  8000-MSTR-CONTROL-RTN.                                           
00568      READ COMM-MSTR-IN                                            
00569          AT END                                                   
00570              MOVE HIGH-VALUE     TO  COMP-IN-RECORD               
00571                                      COMPENSATION-MASTER          
00572              GO TO 8099-EXIT.                                     
00573                                                                   
00574      MOVE COMP-IN-RECORD         TO  COMPENSATION-MASTER.         
00575                                                                   
00576      IF CO-CTL-1  IS EQUAL TO  PRE-CTL-1                          
00577          GO TO 8050-SET-NEW.                                      
00578                                                                   
00579  8010-RESET-CARRIER.                                              
00580      IF PRE-CARR  IS EQUAL TO  CO-CARRIER                         
00581          GO TO 8020-RESET-COMPANY.                                
00582                                                                   
00583      MOVE CO-CARRIER             TO  PRE-CARR.                    
00584      MOVE SPACES                 TO  SAVE-COMPANY-NAME.           
00585                                                                   
00586  8020-RESET-COMPANY.                                              
00587      IF PRE-GROUP  IS EQUAL TO  CO-GROUPING                       
00588          GO TO 8030-RESET-AGENT.                                  
00589                                                                   
00590      MOVE CO-GROUPING            TO  PRE-GROUP.                   
00591                                                                   
00592  8030-RESET-AGENT.                                                
00593      MOVE CO-RESP-NO             TO  PRE-RESP.                    
00594      MOVE 'A'                    TO  BALANCE-SW.                  
00595                                                                   
00596  8050-SET-NEW.                                                    
00597      IF CO-COMPANY-TYPE                                           
00598          GO TO 8080-SET-COMPANY.                                  
00599                                                                   
00600      IF CO-GEN-AGENT-TYPE                                         
00601          GO TO 8070-SET-AGENT.                                    
00602                                                                   
00603      IF NOT CO-ACCOUNT-TYPE                                       
00604          MOVE '0302'              TO  WS-RETURN-CODE              
00605          MOVE 'FATAL FILE ERROR'  TO  WS-ABEND-MESSAGE            
00606          GO TO ABEND-PGM.                                         
00607                                                                   
00608  8060-SET-ACCOUNT.                                                
00609      IF CO-NO-BALANCE                                             
00610          GO TO 8000-MSTR-CONTROL-RTN.                             
00611                                                                   
00612      GO TO 8099-EXIT.                                             
00613                                                                   
00614  8070-SET-AGENT.                                                  
00615      MOVE 'G'                    TO  BALANCE-SW.                  
00616                                                                   
00617      GO TO 8099-EXIT.                                             
00618                                                                   
00619  8080-SET-COMPANY.                                                
00620      MOVE CO-ACCT-NAME           TO  SAVE-COMPANY-NAME.           
00621                                                                   
00622      GO TO 8099-EXIT.                                             
00623                                                                   
00624  8099-EXIT.                                                       
00625      EXIT.                                                        
00626  EJECT                                                            
00627  8600-HD-RTN.                                                     
00628      MOVE HD1                    TO  P-LINE.                      
00629      MOVE SPACE-NP               TO  P-CCSW.                      
00630                                                                   
00631      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00632                                                                   
00633      ADD +1                      TO  PGCTR.                       
00634                                                                   
00635      MOVE PGCTR                  TO  HD-PG.                       
00636      MOVE HD2                    TO  P-LINE.                      
00637      MOVE SPACE-1                TO  P-CCSW.                      
00638                                                                   
00639      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00640                                                                   
00641      MOVE HD3                    TO  P-LINE.                      
00642      MOVE SPACE-1                TO  P-CCSW.                      
00643                                                                   
00644      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00645                                                                   
00646      MOVE SPACE-2                TO  P-CCSW.                      
00647      MOVE 'COMPANY'              TO  P-MSG.                       
00648      MOVE CUR-CARR               TO  P-CARR.                      
00649      MOVE CUR-GROUP              TO  P-GROUP.                     
00650      MOVE '-'                    TO  P-DASH.                      
00651                                                                   
00652      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00653                                                                   
00654      MOVE SPACE-2                TO  P-CCSW.                      
00655      MOVE HD4                    TO  P-LINE.                      
00656                                                                   
00657      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00658                                                                   
00659      MOVE HD5                    TO  P-LINE.                      
00660                                                                   
00661      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       
00662                                                                   
00663      MOVE +10                    TO  LNCTR.                       
00664      MOVE SPACE-2                TO  P-CCSW.                      
00665                                                                   
00666  8699-EXIT.                                                       
00667      EXIT.                                                        
00668                                                                   
00669  8800-PRT-RTN.                                                    
00670      MOVE P-REC                  TO  PRT.                         
00671      MOVE P-CCSW                 TO  X.                           
00672                                                                   
00673      IF P-CCSW  IS EQUAL TO  SPACE-1                              
00674          ADD +1                  TO  LNCTR                        
00675      ELSE                                                         
00676          IF P-CCSW  IS EQUAL TO  SPACE-2                          
00677              ADD +2              TO  LNCTR                        
00678          ELSE                                                     
00679              IF P-CCSW  IS EQUAL TO  SPACE-3                      
00680                  ADD +3          TO  LNCTR.                       
00681                                                                   
00682      MOVE SPACES                 TO  P-REC.                       
00683      MOVE SPACE-1                TO  P-CCSW.                      
00684                                                                   
00685  8850-COPY-PRT-RTN.                                               
00686                              COPY ELCPRT2.                        
00687                                                                   
00688  8800-EXIT.                                                       
00689      EXIT.                                                        
00690  EJECT                                                            
00691  9990-E-O-J.                                                      
00692                              COPY ELCPRTC.                        
00693                                                                   
00694  9995-CLOSE.                                                      
00695      CLOSE COMM-MSTR-IN                                           
00696            PRNTR
062104           ME50-ECS064-BALANCE.
00697                                                                   
00698      GOBACK.                                                      
00699                                                                   
00700  ABEND-PGM SECTION.                                               
00701                              COPY ELCABEND.                       
