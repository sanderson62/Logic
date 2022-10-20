       IDENTIFICATION DIVISION.                                         
                                                                        
       PROGRAM-ID.                 EL349 .                              
      *              PROGRAM CONVERTED BY                               
      *              COBOL CONVERSION AID PO 5785-ABJ                   
      *              CONVERSION DATE 06/20/95 12:50:51.                 
      *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
      *                            VMOD=2.022                           
                                                                        
                                                                        
      *AUTHOR.     LOGIC INC.                                           
      *            DALLAS, TEXAS.                                       
                                                                        
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
      *            *                                                   *
      *            *****************************************************
                                                                        
      *REMARKS.                                                         
      **************** THIS PROGRAM RUNS FOR A CLIENT -              
      **                              (DATE-CARD DRIVEN)            
      **                                                                
      *         THIS PROGRAM IS USED TO DELETE A COMPANIES DATA FROM
      *       A PARTICULAR FILE SUPPLIED BY SYSIN.
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT CARD-FILE     ASSIGN TO SYS006-UR-3505-S-SYS006.      

           SELECT DISK-DATE     ASSIGN TO SYS019.
                                                                        
           SELECT ELACTQ        ASSIGN TO SYS010-FBA1-ELACTQ            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      AQ-CONTROL-PRIMARY      
                                                IN ELACTQ               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELALPH        ASSIGN TO SYS010-FBA1-ELALPH            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      AI-CONTROL-PRIMARY      
                                                IN ELALPH               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELARCH        ASSIGN TO SYS010-FBA1-ELARCH            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      LA-CONTROL-PRIMARY      
                                                IN ELARCH               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELARCT        ASSIGN TO SYS010-FBA1-ELARCT            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      LT-CONTROL-PRIMARY      
                                                IN ELARCT               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELBENE        ASSIGN TO ELBENE            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      BE-CONTROL-PRIMARY      
                                                IN ELBENE               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELCERT        ASSIGN TO SYS010-FBA1-ELCERT            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CM-CONTROL-PRIMARY      
                                                IN ELCERT               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELPURG        ASSIGN TO SYS010-FBA1-ELPURG            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      PG-CONTROL-PRIMARY      
                                                IN ELPURG               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELCHKQ        ASSIGN TO SYS010-FBA1-ELCHKQ            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CQ-CONTROL-PRIMARY      
                                                IN ELCHKQ               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELCNTL        ASSIGN TO SYS010-FBA1-ELCNTL            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CF-CONTROL-PRIMARY      
                                                IN ELCNTL               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELRCON        ASSIGN TO SYS010-FBA1-ELRCON            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      RC-CONTROL-PRIMARY      
                                                IN ELRCON               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELERRS        ASSIGN TO SYS010-FBA1-ELERRS            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      EM-CONTROL-PRIMARY      
                                                IN ELERRS               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELFORM        ASSIGN TO SYS010-FBA1-ELFORM            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      TX-CONTROL-PRIMARY      
                                                IN ELFORM               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELHELP        ASSIGN TO SYS010-FBA1-ELHELP            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      TX-CONTROL-PRIMARY      
                                                IN ELHELP               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELLETR        ASSIGN TO SYS010-FBA1-ELLETR            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      TX-CONTROL-PRIMARY      
                                                IN ELLETR               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELMSTR        ASSIGN TO SYS010-FBA1-ELMSTR            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CL-CONTROL-PRIMARY      
                                                IN ELMSTR               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELRETR        ASSIGN TO SYS010-FBA1-ELRETR            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      RL-CONTROL-PRIMARY      
                                                IN ELRETR               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELPEND        ASSIGN TO SYS010-FBA1-ELPEND            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      SU-CONTROL-PRIMARY      
                                                IN ELPEND               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELPOLF        ASSIGN TO SYS010-FBA1-ELPOLF            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      PF-CONTROL-PRIMARY      
                                                IN ELPOLF               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELTRLR        ASSIGN TO SYS010-FBA1-ELTRLR            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      AT-CONTROL-PRIMARY      
                                                IN ELTRLR               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELREPT        ASSIGN TO SYS010-FBA1-ELREPT            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      RF-CONTROL-PRIMARY      
                                                IN ELREPT               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERACCT        ASSIGN TO SYS010-FBA1-ERACCT            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      AM-CONTROL-PRIMARY      
                                                IN ERACCT               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERACNT        ASSIGN TO SYS010-FBA1-ERACNT            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      NT-CONTROL-PRIMARY      
                                                IN ERACNT               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERARCH        ASSIGN TO SYS010-FBA1-ERARCH            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      LA-CONTROL-PRIMARY      
                                                IN ERARCH               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERARCT        ASSIGN TO SYS010-FBA1-ERARCT            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      LT-CONTROL-PRIMARY      
                                                IN ERARCT               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERBILL        ASSIGN TO SYS010-FBA1-ERBILL            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      BI-CONTROL-PRIMARY      
                                                IN ERBILL               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERCHEK        ASSIGN TO SYS010-FBA1-ERCHEK            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CH-CONTROL-PRIMARY      
                                                IN ERCHEK               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERCHKQ        ASSIGN TO SYS010-FBA1-ERCHKQ            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CQ-CONTROL-PRIMARY      
                                                IN ERCHKQ               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERCOMP        ASSIGN TO SYS010-FBA1-ERCOMP            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CO-CONTROL-PRIMARY      
                                                IN ERCOMP               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERCRTC        ASSIGN TO SYS010-FBA1-ERCRTC            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CC-CONTROL-PRIMARY      
                                                IN ERCRTC               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERCTBL        ASSIGN TO SYS010-FBA1-ERCTBL            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CT-CONTROL-PRIMARY      
                                                IN ERCTBL               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERNAME        ASSIGN TO SYS010-FBA1-ERNAME            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      NL-RECORD-KEY           
                                                IN ERNAME               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERPLAN        ASSIGN TO SYS010-FBA1-ERPLAN            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      PL-CONTROL-PRIMARY      
                                                IN ERPLAN               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERPNDB        ASSIGN TO SYS010-FBA1-ERPNDB            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      PB-CONTROL-PRIMARY      
                                                IN ERPNDB               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERPNDE        ASSIGN TO SYS010-FBA1-ERPNDE            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      PB-CONTROL-PRIMARY      
                                                IN ERPNDE               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERPNDC        ASSIGN TO SYS010-FBA1-ERPNDC            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      PC-CONTROL-PRIMARY      
                                                IN ERPNDC               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERPYAJ        ASSIGN TO SYS010-FBA1-ERPYAJ            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      PY-CONTROL-PRIMARY      
                                                IN ERPYAJ               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERRATE        ASSIGN TO SYS010-FBA1-ERRATE            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      RT-CONTROL-PRIMARY      
                                                IN ERRATE               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERREIN        ASSIGN TO SYS010-FBA1-ERREIN            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      RE-CONTROL-PRIMARY      
                                                IN ERREIN               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERREPY        ASSIGN TO SYS010-FBA1-ERREPY            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      RP-CONTROL-PRIMARY      
                                                IN ERREPY               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERNOTE        ASSIGN TO SYS010-FBA1-ERNOTE            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CN-CONTROL-PRIMARY      
                                                IN ERNOTE               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERGXRF        ASSIGN TO SYS010-FBA1-ERGXRF            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      GX-CONTROL-PRIMARY      
                                                IN ERGXRF               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERCOMM        ASSIGN TO SYS010-FBA1-ERCOMM            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CE-CONTROL-PRIMARY      
                                                IN ERCOMM               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERLOFC        ASSIGN TO SYS010-FBA1-ERLOFC            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      LO-CONTROL-PRIMARY      
                                                IN ERLOFC               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERMEBL        ASSIGN TO SYS010-FBA1-ERMEBL            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      ME-CONTROL-PRIMARY      
                                                IN ERMEBL               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERMAIL        ASSIGN TO SYS010-FBA1-ERMAIL            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      MA-CONTROL-PRIMARY      
                                                IN ERMAIL               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERPNDM        ASSIGN TO SYS010-FBA1-ERPNDM            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      PM-CONTROL-PRIMARY      
                                                IN ERPNDM               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERRQST        ASSIGN TO SYS010-FBA1-ERRQST            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      RQ-CONTROL-PRIMARY      
                                                IN ERRQST               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERSUMM        ASSIGN TO SYS010-FBA1-ERSUMM            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      SX-CONTROL-PRIMARY      
                                                IN ERSUMM               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERRECV        ASSIGN TO SYS010-FBA1-ERRECV            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      AR-CONTROL-PRIMARY      
                                                IN ERRECV               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERCKWK        ASSIGN TO SYS010-FBA1-ERCKWK            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CW-CONTROL-PRIMARY      
                                                IN ERCKWK               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERCMCK        ASSIGN TO SYS010-FBA1-ERCMCK            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      CK-CONTROL-PRIMARY      
                                                IN ERCMCK               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERCMKQ        ASSIGN TO SYS010-FBA1-ERCMKQ            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      MQ-CONTROL-PRIMARY      
                                                IN ERCMKQ               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERFORM        ASSIGN TO SYS010-FBA1-ERFORM            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      FO-CONTROL-PRIMARY      
                                                IN ERFORM               
                                FILE STATUS     CHK.                    
           SELECT ERRTRO        ASSIGN TO SYS010-FBA1-ERRTRO            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      RM-CONTROL-PRIMARY      
                                                IN ERRTRO               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERINMS        ASSIGN TO SYS010-FBA1-ERINMS            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      IM-CONTROL-PRIMARY      
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERARBR        ASSIGN TO SYS010-FBA1-ERARBR            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      AB-CONTROL-PRIMARY      
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERCCAP        ASSIGN TO SYS010-FBA1-ERCCAP            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      ERCCAP-PRIMARY-KEY      
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELNOTE        ASSIGN TO SYS010-FBA1-ELNOTE            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      EN-CONTROL-PRIMARY      
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERRESS        ASSIGN TO SYS010-FBA1-ERRESS            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      ERRESS-PRIMARY-KEY      
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ERRESC        ASSIGN TO SYS010-FBA1-ERRESC            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      ERRESC-RECORD-KEY       
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELACHP        ASSIGN TO SYS010-FBA1-ELACHP            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      AP-CONTROL-PRIMARY      
                                                IN ELACHP               
                                FILE STATUS     CHK.                    
                                                                        
           SELECT ELBANK        ASSIGN TO SYS010-FBA1-ELBANK            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      BM-CONTROL-PRIMARY      
                                                IN ELBANK               
                                FILE STATUS     CHK.                    
           SELECT ERLOSS        ASSIGN TO SYS010-FBA1-ERLOSS            
                                ORGANIZATION    INDEXED                 
                                ACCESS          DYNAMIC                 
                                RECORD KEY      LR-CONTROL              
                                                IN ERLOSS               
                                FILE STATUS     CHK.                    
           EJECT                                                        
                                                                        
       DATA DIVISION.                                                   
                                                                        
       FILE SECTION.                                                    
                                                                        
       FD  CARD-FILE                                                    
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.                                            
                                                                        
       01  CARD-RECORD.                                                 
           05  ER-FILE-NAME                PIC X(8).                    
           05  ER-ACTION                   PIC X(6).                    
           05  ER-COMPANY                  PIC 99.                      
           05  FILLER                      PIC X(64).                   
                                                                        
       EJECT                                                            

       FD  DISK-DATE
           COPY ELCDTEFD.

       FD  ELACTQ.                                                      
                                                                        
           COPY ELCACTQ.                                                
                                                                        
       EJECT                                                            
       FD  ELALPH.                                                      
                                                                        
           COPY ELCALPH.                                                
                                                                        
       EJECT                                                            
       FD  ELARCH.                                                      
                                                                        
           COPY ELCARCH.                                                
                                                                        
       EJECT                                                            
       FD  ELARCT.                                                      
                                                                        
           COPY ELCARCT.                                                
                                                                        
       EJECT                                                            
       FD  ELBENE.                                                      
                                                                        
           COPY ELCBENE.                                                
                                                                        
       EJECT                                                            
       FD  ELCERT.                                                      
                                                                        
           COPY ELCCERT.                                                
                                                                        
       EJECT                                                            
       FD  ELPURG.                                                      
                                                                        
           COPY ELCPURG.                                                
                                                                        
       EJECT                                                            
       FD  ELCHKQ.                                                      
                                                                        
           COPY ELCCHKQ.                                                
                                                                        
       EJECT                                                            
       FD  ELCNTL.                                                      
                                                                        
           COPY ELCCNTL.                                                
                                                                        
       EJECT                                                            
       FD  ELRCON.                                                      
                                                                        
           COPY ELCRCON.                                                
                                                                        
       EJECT                                                            
       FD  ELERRS.                                                      
                                                                        
           COPY ELCERRS.                                                
                                                                        
       EJECT                                                            
       FD  ELFORM.                                                      
                                                                        
           COPY ELCTEXT.                                                
                                                                        
       EJECT                                                            
       FD  ELHELP.                                                      
                                                                        
           COPY ELCTEXT.                                                
                                                                        
       EJECT                                                            
       FD  ELLETR.                                                      
                                                                        
           COPY ELCTEXT.                                                
                                                                        
       EJECT                                                            
       FD  ELMSTR.                                                      
                                                                        
           COPY ELCMSTR.                                                
                                                                        
       EJECT                                                            
       FD  ELRETR.                                                      
                                                                        
           COPY ELCRETR.                                                
                                                                        
       EJECT                                                            
       FD  ELTRLR.                                                      
                                                                        
           COPY ELCTRLR.                                                
                                                                        
       EJECT                                                            
       FD  ELREPT.                                                      
                                                                        
           COPY ELCREPT.                                                
                                                                        
       EJECT                                                            
       FD  ERACCT.                                                      
                                                                        
           COPY ERCACCT.                                                
                                                                        
       FD  ERACNT.                                                      
                                                                        
           COPY ERCACNT.                                                
                                                                        
       EJECT                                                            
       FD  ERARCH.                                                      
                                                                        
           COPY ERCARCH.                                                
                                                                        
       EJECT                                                            
       FD  ERARCT.                                                      
                                                                        
           COPY ERCARCT.                                                
                                                                        
       EJECT                                                            
       FD  ERBILL.                                                      
                                                                        
           COPY ERCBILL.                                                
                                                                        
       EJECT                                                            
       FD  ERCHKQ.                                                      
                                                                        
           COPY ERCCHKQ.                                                
                                                                        
       EJECT                                                            
       FD  ERCHEK.                                                      
                                                                        
           COPY ERCCHEK.                                                
                                                                        
       EJECT                                                            
       FD  ERCOMP.                                                      
                                                                        
           COPY ERCCOMP.                                                
                                                                        
       EJECT                                                            
       FD  ERCRTC.                                                      
                                                                        
           COPY ERCCRTC.                                                
                                                                        
       EJECT                                                            
       FD  ERCTBL.                                                      
                                                                        
           COPY ERCCTBL.                                                
                                                                        
       EJECT                                                            
       FD  ERNAME.                                                      
                                                                        
           COPY ERCNAME.                                                
                                                                        
       EJECT                                                            
       FD  ERPLAN.                                                      
                                                                        
           COPY ERCPLAN.                                                
                                                                        
       EJECT                                                            
       FD  ERPNDB.                                                      
                                                                        
           COPY ERCPNDB.                                                
                                                                        
       EJECT                                                            
       FD  ERPNDE.                                                      
                                                                        
           COPY ERCPNDB.                                                
                                                                        
       EJECT                                                            
       FD  ERPNDC.                                                      
                                                                        
           COPY ERCPNDC.                                                
                                                                        
       EJECT                                                            
       FD  ERRATE.                                                      
                                                                        
           COPY ERCRATE.                                                
                                                                        
       EJECT                                                            
       FD  ERREIN.                                                      
                                                                        
           COPY ERCREIN.                                                
                                                                        
       EJECT                                                            
       FD  ERREPY.                                                      
                                                                        
           COPY ERCREPY.                                                
                                                                        
       EJECT                                                            
       FD  ERPYAJ.                                                      
                                                                        
           COPY ERCPYAJ.                                                
                                                                        
       EJECT                                                            
       FD  ERNOTE.                                                      
                                                                        
           COPY ERCNOTE.                                                
                                                                        
       EJECT                                                            
       FD  ERGXRF.                                                      
                                                                        
           COPY ERCGXRF.                                                
                                                                        
       EJECT                                                            
       FD  ERCOMM.                                                 
                                                                        
           COPY ERCCOMM.                                                
                                                                        
       EJECT                                                            
       FD  ERLOFC.                                                      
                                                                        
           COPY ERCLOFC.                                                
                                                                        
       EJECT                                                            
       FD  ERMEBL.                                                      
                                                                        
           COPY ERCMEBL.                                                
                                                                        
       EJECT                                                            
       FD  ERMAIL.                                                      
                                                                        
           COPY ERCMAIL.                                                
                                                                        
       EJECT                                                            
       FD  ERPNDM.                                                      
                                                                        
           COPY ERCPNDM.                                                
                                                                        
       EJECT                                                            
       FD  ERRQST.                                                      
                                                                        
           COPY ERCRQST.                                                
                                                                        
       EJECT                                                            
       FD  ERSUMM.                                                      
                                                                        
           COPY ERCSUMM.                                                
                                                                        
       EJECT                                                            
       FD  ERRECV.                                                      
                                                                        
           COPY ERCRECV.                                                
                                                                        
       EJECT                                                            
       FD  ERCKWK.                                                      
                                                                        
           COPY ERCCKWK.                                                
                                                                        
       EJECT                                                            
       FD  ERCMCK.                                                      
                                                                        
           COPY ERCCMCK.                                                
                                                                        
       EJECT                                                            
       FD  ERCMKQ.                                                      
                                                                        
           COPY ERCCMKQ.                                                
                                                                        
       EJECT                                                            
       FD  ERFORM.                                                      
                                                                        
           COPY ERCFORM.                                                
                                                                        
       FD  ERRTRO.                                                      
                                                                        
           COPY ERCRTRO.                                                
                                                                        
           EJECT                                                        
       FD  ELPEND.                                                      
                                                                        
           COPY ELCPEND.                                                
           EJECT                                                        
       FD  ELPOLF.                                                      
                                                                        
           COPY ELCPOLF.                                                
                                                                        
           EJECT                                                        
       FD  ERINMS.                                                      
                                                                        
           COPY INVMSTR.                                                
                                                                        
           EJECT                                                        
       FD  ERARBR.                                                      
                                                                        
           COPY ERCARBR.                                                
                                                                        
           EJECT                                                        
       FD  ERCCAP.                                                      
                                                                        
           COPY ERCCAPS.                                                
                                                                        
           EJECT                                                        
       FD  ELNOTE.                                                      
                                                                        
           COPY ELCNOTE.                                                
                                                                        
           EJECT                                                        
       FD  ERRESS.                                                      
                                                                        
           COPY ERCRESS.                                                
                                                                        
           EJECT                                                        
       FD  ERRESC.                                                      
                                                                        
           COPY ERCRESC.                                                
           EJECT                                                        
       FD  ELACHP.                                                      
                                                                        
           COPY ELCACHP.                                                
           EJECT                                                        
       FD  ELBANK.                                                      
                                                                        
           COPY ELCBANK.                                                
           EJECT                                                        
       FD  ERLOSS.                                                      
                                                                        
           COPY ERCLOSS.                                                
           EJECT                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*   EL349  WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '********* VMOD=2.022 ***********'. 
                                                                        
       01  FILLER.                                                      
           05  WS-EOF-SW               PIC X       VALUE ' '.
               88  END-OF-CARD                     VALUE 'Y'.
           05  WS-FILE-ID              PIC X(8)    VALUE SPACES.
           05  WS-ACTION               PIC X(8)    VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(4)  COMP  VALUE ZEROS.    
           05  WS-ZERO                 PIC S9     VALUE +0  COMP-3.
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         
           05  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.          
           05  PGM-SUB             PIC S999   COMP-3  VALUE +061.   
           05  CHK.                                                     
               10  CHK1               PIC X.                            
               10  CHK2               PIC X.                            
                                                                        
           EJECT                                                        
                                       COPY ELCDTECX.

                                       COPY ELCDTEVR. 

       PROCEDURE DIVISION.                                              
                                       COPY ELCDTERX.
                                                                        
           OPEN INPUT CARD-FILE

           PERFORM 0100-READ           THRU 0100-EXIT
           
           PERFORM 0200-PROCESS        THRU 0200-EXIT UNTIL
              END-OF-CARD

           CLOSE CARD-FILE

           GOBACK

           .                                                             
       0100-READ.                                                       

           READ CARD-FILE AT END
              SET END-OF-CARD          TO TRUE
           END-READ

           IF NOT END-OF-CARD
              DISPLAY ' '
              DISPLAY CARD-RECORD
           END-IF
           
           .
       0100-EXIT.
           EXIT.

       0200-PROCESS.
       
           MOVE ER-FILE-NAME           TO WS-FILE-ID

           IF (ER-FILE-NAME = 'ELACTQ')
              AND (ER-ACTION = 'DELETE')
              MOVE 'OPEN'              TO WS-ACTION
              OPEN I-O ELACTQ
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO AQ-CONTROL-PRIMARY IN ELACTQ
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO AQ-COMPANY-CD IN ELACTQ
              START ELACTQ
                        KEY IS NOT < AQ-CONTROL-PRIMARY IN ELACTQ
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELACTQ NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (AQ-COMPANY-CD IN ELACTQ NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELACTQ
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELACTQ NEXT RECORD
              END-PERFORM
              CLOSE ELACTQ
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELACTQ IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF
                                                                        
           IF (ER-FILE-NAME = 'ELALPH')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELALPH
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO AI-CONTROL-PRIMARY IN ELALPH
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO AI-COMPANY-CD IN ELALPH
              START ELALPH
                        KEY IS NOT < AI-CONTROL-PRIMARY IN ELALPH
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELALPH NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (AI-COMPANY-CD IN ELALPH NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELALPH
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELALPH NEXT RECORD
              END-PERFORM
              CLOSE ELALPH
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELALPH IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELARCH')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELARCH
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO LA-CONTROL-PRIMARY IN ELARCH
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO LA-COMPANY-CD IN ELARCH
              START ELARCH
                        KEY IS NOT < LA-CONTROL-PRIMARY IN ELARCH
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELARCH NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (LA-COMPANY-CD IN ELARCH NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELARCH
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELARCH NEXT RECORD
              END-PERFORM
              CLOSE ELARCH
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELARCH IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELARCT')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELARCT
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO LT-CONTROL-PRIMARY IN ELARCT
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO LT-COMPANY-CD IN ELARCT
              START ELARCT
                        KEY IS NOT < LT-CONTROL-PRIMARY IN ELARCT
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELARCT NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (LT-COMPANY-CD IN ELARCT NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELARCT
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELARCT NEXT RECORD
              END-PERFORM
              CLOSE ELARCT
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELARCT IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELBENE')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELBENE
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO BE-CONTROL-PRIMARY IN ELBENE
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO BE-COMPANY-CD IN ELBENE
              DISPLAY ' BENE KEY ' BE-CONTROL-PRIMARY IN ELBENE
              START ELBENE
                        KEY IS NOT < BE-CONTROL-PRIMARY IN ELBENE
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELBENE NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (BE-COMPANY-CD IN ELBENE NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELBENE
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELBENE NEXT RECORD
              END-PERFORM
              CLOSE ELBENE
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELBENE IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELCERT')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELCERT
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CM-CONTROL-PRIMARY IN ELCERT
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CM-COMPANY-CD IN ELCERT
              START ELCERT
                        KEY IS NOT < CM-CONTROL-PRIMARY IN ELCERT
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELCERT NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CM-COMPANY-CD IN ELCERT NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELCERT
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELCERT NEXT RECORD
              END-PERFORM
              CLOSE ELCERT
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELCERT IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELPURG')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELPURG
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO PG-CONTROL-PRIMARY IN ELPURG
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO PG-COMPANY-CD IN ELPURG
              START ELPURG
                        KEY IS NOT < PG-CONTROL-PRIMARY IN ELPURG
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELPURG NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (PG-COMPANY-CD IN ELPURG NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELPURG
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELPURG NEXT RECORD
              END-PERFORM
              CLOSE ELPURG
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELPURG IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELCHKQ')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELCHKQ
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CQ-CONTROL-PRIMARY IN ELCHKQ
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CQ-COMPANY-CD IN ELCHKQ
              START ELCHKQ
                        KEY IS NOT < CQ-CONTROL-PRIMARY IN ELCHKQ
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELCHKQ NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CQ-COMPANY-CD IN ELCHKQ NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELCHKQ
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELCHKQ NEXT RECORD
              END-PERFORM
              CLOSE ELCHKQ
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELCHKQ IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELCNTL')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELCNTL
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CF-CONTROL-PRIMARY IN ELCNTL
              MOVE DTE-CLIENT     TO CF-COMPANY-ID IN ELCNTL
              MOVE '3'            TO CF-RECORD-TYPE
              START ELCNTL
                        KEY IS NOT < CF-CONTROL-PRIMARY IN ELCNTL
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELCNTL NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CF-COMPANY-ID IN ELCNTL NOT =
                           DTE-CLIENT)
                IF CF-RECORD-TYPE NOT = '1' AND '2' AND 'N' AND 'R'
                 DELETE ELCNTL
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                END-IF
                 READ ELCNTL NEXT RECORD
              END-PERFORM
              CLOSE ELCNTL
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELCNTL IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELRCON')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELRCON
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO RC-CONTROL-PRIMARY IN ELRCON
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO RC-COMPANY-CD IN ELRCON
              START ELRCON
                        KEY IS NOT < RC-CONTROL-PRIMARY IN ELRCON
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELRCON NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (RC-COMPANY-CD IN ELRCON NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELRCON
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELRCON NEXT RECORD
              END-PERFORM
              CLOSE ELRCON
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELRCON IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELERRS')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELERRS
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO EM-CONTROL-PRIMARY IN ELERRS
              START ELERRS
                        KEY IS NOT < EM-CONTROL-PRIMARY IN ELERRS
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELERRS NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 DELETE ELERRS
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELERRS NEXT RECORD
              END-PERFORM
              CLOSE ELERRS
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELERRS IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELFORM')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELFORM
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO TX-CONTROL-PRIMARY IN ELFORM
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO TX-COMPANY-CD IN ELFORM
              START ELFORM
                        KEY IS NOT < TX-CONTROL-PRIMARY IN ELFORM
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELFORM NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (TX-COMPANY-CD IN ELFORM NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELFORM
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELFORM NEXT RECORD
              END-PERFORM
              CLOSE ELFORM
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELFORM IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELHELP')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELHELP
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO TX-CONTROL-PRIMARY IN ELHELP
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO TX-COMPANY-CD IN ELHELP
              START ELHELP
                        KEY IS NOT < TX-CONTROL-PRIMARY IN ELHELP
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELHELP NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (TX-COMPANY-CD IN ELHELP NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELHELP
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELHELP NEXT RECORD
              END-PERFORM
              CLOSE ELHELP
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELHELP IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELLETR')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELLETR
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO TX-CONTROL-PRIMARY IN ELLETR
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO TX-COMPANY-CD IN ELLETR
              START ELLETR
                        KEY IS NOT < TX-CONTROL-PRIMARY IN ELLETR
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELLETR NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (TX-COMPANY-CD IN ELLETR NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELLETR
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELLETR NEXT RECORD
              END-PERFORM
              CLOSE ELLETR
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELLETR IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELMSTR')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELMSTR
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CL-CONTROL-PRIMARY IN ELMSTR
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CL-COMPANY-CD IN ELMSTR
              START ELMSTR
                        KEY IS NOT < CL-CONTROL-PRIMARY IN ELMSTR
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELMSTR NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CL-COMPANY-CD IN ELMSTR NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELMSTR
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELMSTR NEXT RECORD
              END-PERFORM
              CLOSE ELMSTR
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELMSTR IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELRETR')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELRETR
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO RL-CONTROL-PRIMARY IN ELRETR
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO RL-COMPANY-CD IN ELRETR
              START ELRETR
                        KEY IS NOT < RL-CONTROL-PRIMARY IN ELRETR
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELRETR NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (RL-COMPANY-CD IN ELRETR NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELRETR
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELRETR NEXT RECORD
              END-PERFORM
              CLOSE ELRETR
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELRETR IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELTRLR')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELTRLR
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO AT-CONTROL-PRIMARY IN ELTRLR
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO AT-COMPANY-CD IN ELTRLR
              START ELTRLR
                        KEY IS NOT < AT-CONTROL-PRIMARY IN ELTRLR
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELTRLR NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (AT-COMPANY-CD IN ELTRLR NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELTRLR
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELTRLR NEXT RECORD
              END-PERFORM
              CLOSE ELTRLR
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELTRLR IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELREPT')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELREPT
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO RF-CONTROL-PRIMARY IN ELREPT
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO RF-COMPANY-CD IN ELREPT
              START ELREPT
                        KEY IS NOT < RF-CONTROL-PRIMARY IN ELREPT
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELREPT NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (RF-COMPANY-CD IN ELREPT NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELREPT
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELREPT NEXT RECORD
              END-PERFORM
              CLOSE ELREPT
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELREPT IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERACCT')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERACCT
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO AM-CONTROL-PRIMARY IN ERACCT
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO AM-COMPANY-CD IN ERACCT
              START ERACCT
                        KEY IS NOT < AM-CONTROL-PRIMARY IN ERACCT
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERACCT NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (AM-COMPANY-CD IN ERACCT NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERACCT
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERACCT NEXT RECORD
              END-PERFORM
              CLOSE ERACCT
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERACCT IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERACNT')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERACNT
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO NT-CONTROL-PRIMARY IN ERACNT
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO NT-COMPANY-CD IN ERACNT
              START ERACNT
                        KEY IS NOT < NT-CONTROL-PRIMARY IN ERACNT
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERACNT NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (NT-COMPANY-CD IN ERACNT NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERACNT
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERACNT NEXT RECORD
              END-PERFORM
              CLOSE ERACNT
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERACNT IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERARCH')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERARCH
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO LA-CONTROL-PRIMARY IN ERARCH
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO LA-COMPANY-CD IN ERARCH
              START ERARCH
                        KEY IS NOT < LA-CONTROL-PRIMARY IN ERARCH
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERARCH NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (LA-COMPANY-CD IN ERARCH NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERARCH
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERARCH NEXT RECORD
              END-PERFORM
              CLOSE ERARCH
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERARCH IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERARCT')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERARCT
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO LT-CONTROL-PRIMARY IN ERARCT
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO LT-COMPANY-CD IN ERARCT
              START ERARCT
                        KEY IS NOT < LT-CONTROL-PRIMARY IN ERARCT
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERARCT NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (LT-COMPANY-CD IN ERARCT NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERARCT
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERARCT NEXT RECORD
              END-PERFORM
              CLOSE ERARCT
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERARCT IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERBILL')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERBILL
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO BI-CONTROL-PRIMARY IN ERBILL
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO BI-COMPANY-CD IN ERBILL
              START ERBILL
                        KEY IS NOT < BI-CONTROL-PRIMARY IN ERBILL
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERBILL NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (BI-COMPANY-CD IN ERBILL NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERBILL
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERBILL NEXT RECORD
              END-PERFORM
              CLOSE ERBILL
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERBILL IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERCHEK')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERCHEK
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CH-CONTROL-PRIMARY IN ERCHEK
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CH-COMPANY-CD IN ERCHEK
              START ERCHEK
                        KEY IS NOT < CH-CONTROL-PRIMARY IN ERCHEK
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERCHEK NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CH-COMPANY-CD IN ERCHEK NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERCHEK
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERCHEK NEXT RECORD
              END-PERFORM
              CLOSE ERCHEK
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERCHEK IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERCHKQ')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERCHKQ
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CQ-CONTROL-PRIMARY IN ERCHKQ
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CQ-COMPANY-CD IN ERCHKQ
              START ERCHKQ
                        KEY IS NOT < CQ-CONTROL-PRIMARY IN ERCHKQ
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERCHKQ NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CQ-COMPANY-CD IN ERCHKQ NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERCHKQ
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERCHKQ NEXT RECORD
              END-PERFORM
              CLOSE ERCHKQ
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERCHKQ IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERCOMP')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERCOMP
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CO-CONTROL-PRIMARY IN ERCOMP
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CO-COMPANY-CD IN ERCOMP
              START ERCOMP
                        KEY IS NOT < CO-CONTROL-PRIMARY IN ERCOMP
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERCOMP NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CO-COMPANY-CD IN ERCOMP NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERCOMP
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERCOMP NEXT RECORD
              END-PERFORM
              CLOSE ERCOMP
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERCOMP IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF
                                                                        
           IF (ER-FILE-NAME = 'ERCRTC')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERCRTC
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CC-CONTROL-PRIMARY IN ERCRTC
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CC-COMPANY-CD IN ERCRTC
              START ERCRTC
                        KEY IS NOT < CC-CONTROL-PRIMARY IN ERCRTC
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERCRTC NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CC-COMPANY-CD IN ERCRTC NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERCRTC
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERCRTC NEXT RECORD
              END-PERFORM
              CLOSE ERCRTC
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERCRTC IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERCTBL')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERCTBL
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CT-CONTROL-PRIMARY IN ERCTBL
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CT-COMPANY-CD IN ERCTBL
              START ERCTBL
                        KEY IS NOT < CT-CONTROL-PRIMARY IN ERCTBL
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERCTBL NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CT-COMPANY-CD IN ERCTBL NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERCTBL
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERCTBL NEXT RECORD
              END-PERFORM
              CLOSE ERCTBL
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERCTBL IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERNAME')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERNAME
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO NL-CONTROL-PRIMARY IN ERNAME
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO NL-COMPANY-CD IN ERNAME
              START ERNAME
                        KEY IS NOT < NL-CONTROL-PRIMARY IN ERNAME
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERNAME NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (NL-COMPANY-CD IN ERNAME NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERNAME
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERNAME NEXT RECORD
              END-PERFORM
              CLOSE ERNAME
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERNAME IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERPLAN')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERPLAN
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO PL-CONTROL-PRIMARY IN ERPLAN
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO PL-COMPANY-CD IN ERPLAN
              START ERPLAN
                        KEY IS NOT < PL-CONTROL-PRIMARY IN ERPLAN
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERPLAN NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (PL-COMPANY-CD IN ERPLAN NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERPLAN
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERPLAN NEXT RECORD
              END-PERFORM
              CLOSE ERPLAN
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERPLAN IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERPNDB')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERPNDB
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO PB-CONTROL-PRIMARY IN ERPNDB
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO PB-COMPANY-CD IN ERPNDB
              START ERPNDB
                        KEY IS NOT < PB-CONTROL-PRIMARY IN ERPNDB
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERPNDB NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (PB-COMPANY-CD IN ERPNDB NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERPNDB
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERPNDB NEXT RECORD
              END-PERFORM
              CLOSE ERPNDB
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERPNDB IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERPNDE')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERPNDE
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              IF CHK = '00'
                MOVE LOW-VALUES   TO PB-CONTROL-PRIMARY IN ERPNDE
                MOVE DTE-CLASIC-COMPANY-CD
                                  TO PB-COMPANY-CD IN ERPNDE
                START ERPNDE
                        KEY IS NOT < PB-CONTROL-PRIMARY IN ERPNDE
                MOVE 'START'             TO WS-ACTION
                PERFORM 1000-ERROR-CHECK
                READ ERPNDE NEXT RECORD
                PERFORM UNTIL
                   (CHK1 NOT = '0')
                   OR (PB-COMPANY-CD IN ERPNDE NOT =
                           DTE-CLASIC-COMPANY-CD)
                   DELETE ERPNDE
                   MOVE 'DELETE'         TO WS-ACTION
                   PERFORM 1000-ERROR-CHECK
                   READ ERPNDE NEXT RECORD
                END-PERFORM
                CLOSE ERPNDE
                MOVE 'CLOSE'             TO WS-ACTION
                PERFORM 1000-ERROR-CHECK
                DISPLAY 'ERPNDE IS WIPPED OUT (UNLESS NOTED ABOVE)'
              END-IF
           END-IF

           IF (ER-FILE-NAME = 'ERPNDC')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERPNDC
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO PC-CONTROL-PRIMARY IN ERPNDC
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO PC-COMPANY-CD IN ERPNDC
              START ERPNDC
                        KEY IS NOT < PC-CONTROL-PRIMARY IN ERPNDC
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERPNDC NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (PC-COMPANY-CD IN ERPNDC NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERPNDC
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERPNDC NEXT RECORD
              END-PERFORM
              CLOSE ERPNDC
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERPNDC IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERRATE')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERRATE
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO RT-CONTROL-PRIMARY IN ERRATE
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO RT-COMPANY-CD IN ERRATE
              START ERRATE
                        KEY IS NOT < RT-CONTROL-PRIMARY IN ERRATE
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERRATE NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (RT-COMPANY-CD IN ERRATE NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERRATE
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERRATE NEXT RECORD
              END-PERFORM
              CLOSE ERRATE
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERRATE IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERREIN')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERREIN
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO RE-CONTROL-PRIMARY IN ERREIN
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO RE-COMPANY-CD IN ERREIN
              START ERREIN
                        KEY IS NOT < RE-CONTROL-PRIMARY IN ERREIN
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERREIN NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (RE-COMPANY-CD IN ERREIN NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERREIN
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERREIN NEXT RECORD
              END-PERFORM
              CLOSE ERREIN
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERREIN IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERREPY')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERREPY
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO RP-CONTROL-PRIMARY IN ERREPY
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO RP-COMPANY-CD IN ERREPY
              START ERREPY
                        KEY IS NOT < RP-CONTROL-PRIMARY IN ERREPY
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERREPY NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (RP-COMPANY-CD IN ERREPY NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERREPY
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERREPY NEXT RECORD
              END-PERFORM
              CLOSE ERREPY
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERREPY IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERPYAJ')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERPYAJ
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO PY-CONTROL-PRIMARY IN ERPYAJ
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO PY-COMPANY-CD IN ERPYAJ
              START ERPYAJ
                        KEY IS NOT < PY-CONTROL-PRIMARY IN ERPYAJ
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERPYAJ NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (PY-COMPANY-CD IN ERPYAJ NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERPYAJ
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERPYAJ NEXT RECORD
              END-PERFORM
              CLOSE ERPYAJ
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERPYAJ IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERNOTE')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERNOTE
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CN-CONTROL-PRIMARY IN ERNOTE
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CN-COMPANY-CD IN ERNOTE
              START ERNOTE
                        KEY IS NOT < CN-CONTROL-PRIMARY IN ERNOTE
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERNOTE NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CN-COMPANY-CD IN ERNOTE NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERNOTE
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERNOTE NEXT RECORD
              END-PERFORM
              CLOSE ERNOTE
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERNOTE IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERGXRF')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERGXRF
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO GX-CONTROL-PRIMARY IN ERGXRF
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO GX-COMPANY-CD IN ERGXRF
              START ERGXRF
                        KEY IS NOT < GX-CONTROL-PRIMARY IN ERGXRF
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERGXRF NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (GX-COMPANY-CD IN ERGXRF NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERGXRF
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERGXRF NEXT RECORD
              END-PERFORM
              CLOSE ERGXRF
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERGXRF IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERCOMM')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERCOMM
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CE-CONTROL-PRIMARY IN ERCOMM
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CE-COMPANY-CD IN ERCOMM
              START ERCOMM
                        KEY IS NOT < CE-CONTROL-PRIMARY IN ERCOMM
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERCOMM NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CE-COMPANY-CD IN ERCOMM NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERCOMM
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERCOMM NEXT RECORD
              END-PERFORM
              CLOSE ERCOMM
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERCOMM IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERLOFC')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERLOFC
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO LO-CONTROL-PRIMARY IN ERLOFC
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO LO-COMPANY-CD IN ERLOFC
              START ERLOFC
                        KEY IS NOT < LO-CONTROL-PRIMARY IN ERLOFC
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERLOFC NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (LO-COMPANY-CD IN ERLOFC NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERLOFC
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERLOFC NEXT RECORD
              END-PERFORM
              CLOSE ERLOFC
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERLOFC IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERMEBL')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERMEBL
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO ME-CONTROL-PRIMARY IN ERMEBL
              MOVE DTE-CLIENT     TO ME-COMPANY IN ERMEBL
              START ERMEBL
                        KEY IS NOT < ME-CONTROL-PRIMARY IN ERMEBL
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERMEBL NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (ME-COMPANY IN ERMEBL NOT =
                           DTE-CLIENT)
                 DELETE ERMEBL
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERMEBL NEXT RECORD
              END-PERFORM
              CLOSE ERMEBL
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERMEBL IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERMAIL')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERMAIL
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO MA-CONTROL-PRIMARY IN ERMAIL
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO MA-COMPANY-CD IN ERMAIL
              START ERMAIL
                        KEY IS NOT < MA-CONTROL-PRIMARY IN ERMAIL
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERMAIL NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (MA-COMPANY-CD IN ERMAIL NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERMAIL
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERMAIL NEXT RECORD
              END-PERFORM
              CLOSE ERMAIL
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERMAIL IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERPNDM')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERPNDM
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO PM-CONTROL-PRIMARY IN ERPNDM
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO PM-COMPANY-CD IN ERPNDM
              START ERPNDM
                        KEY IS NOT < PM-CONTROL-PRIMARY IN ERPNDM
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERPNDM NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (PM-COMPANY-CD IN ERPNDM NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERPNDM
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERPNDM NEXT RECORD
              END-PERFORM
              CLOSE ERPNDM
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERPNDM IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERRQST')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERRQST
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO RQ-CONTROL-PRIMARY IN ERRQST
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO RQ-COMPANY-CD IN ERRQST
              START ERRQST
                        KEY IS NOT < RQ-CONTROL-PRIMARY IN ERRQST
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERRQST NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (RQ-COMPANY-CD IN ERRQST NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERRQST
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERRQST NEXT RECORD
              END-PERFORM
              CLOSE ERRQST
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERRQST IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERSUMM')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERSUMM
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO SX-CONTROL-PRIMARY IN ERSUMM
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO SX-COMPANY-CD IN ERSUMM
              START ERSUMM
                        KEY IS NOT < SX-CONTROL-PRIMARY IN ERSUMM
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERSUMM NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (SX-COMPANY-CD IN ERSUMM NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERSUMM
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERSUMM NEXT RECORD
              END-PERFORM
              CLOSE ERSUMM
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERSUMM IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERRECV')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERRECV
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO AR-CONTROL-PRIMARY IN ERRECV
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO AR-COMPANY-CD IN ERRECV
              START ERRECV
                        KEY IS NOT < AR-CONTROL-PRIMARY IN ERRECV
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERRECV NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (AR-COMPANY-CD IN ERRECV NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERRECV
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERRECV NEXT RECORD
              END-PERFORM
              CLOSE ERRECV
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERRECV IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERCKWK')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERCKWK
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CW-CONTROL-PRIMARY IN ERCKWK
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CW-COMPANY-CD IN ERCKWK
              START ERCKWK
                        KEY IS NOT < CW-CONTROL-PRIMARY IN ERCKWK
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERCKWK NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CW-COMPANY-CD IN ERCKWK NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERCKWK
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERCKWK NEXT RECORD
              END-PERFORM
              CLOSE ERCKWK
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERCKWK IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERCMCK')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERCMCK
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO CK-CONTROL-PRIMARY IN ERCMCK
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CK-COMPANY-CD IN ERCMCK
              START ERCMCK
                        KEY IS NOT < CK-CONTROL-PRIMARY IN ERCMCK
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERCMCK NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CK-COMPANY-CD IN ERCMCK NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERCMCK
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERCMCK NEXT RECORD
              END-PERFORM
              CLOSE ERCMCK
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERCMCK IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERCMKQ')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERCMKQ
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO MQ-CONTROL-PRIMARY IN ERCMKQ
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO MQ-COMPANY-CD IN ERCMKQ
              START ERCMKQ
                        KEY IS NOT < MQ-CONTROL-PRIMARY IN ERCMKQ
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERCMKQ NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (MQ-COMPANY-CD IN ERCMKQ NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERCMKQ
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERCMKQ NEXT RECORD
              END-PERFORM
              CLOSE ERCMKQ
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERCMKQ IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERFORM')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERFORM
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO FO-CONTROL-PRIMARY IN ERFORM
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO FO-COMPANY-CD IN ERFORM
              START ERFORM
                        KEY IS NOT < FO-CONTROL-PRIMARY IN ERFORM
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERFORM NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (FO-COMPANY-CD IN ERFORM NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERFORM
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERFORM NEXT RECORD
              END-PERFORM
              CLOSE ERFORM
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERFORM IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERRTRO')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERRTRO
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO RM-CONTROL-PRIMARY IN ERRTRO
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO RM-COMPANY-CD IN ERRTRO
              START ERRTRO
                        KEY IS NOT < RM-CONTROL-PRIMARY IN ERRTRO
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERRTRO NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (RM-COMPANY-CD IN ERRTRO NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERRTRO
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERRTRO NEXT RECORD
              END-PERFORM
              CLOSE ERRTRO
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERRTRO IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELPEND')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELPEND
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO SU-CONTROL-PRIMARY IN ELPEND
              START ELPEND
                        KEY IS NOT < SU-CONTROL-PRIMARY IN ELPEND
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELPEND NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 DELETE ELPEND
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELPEND NEXT RECORD
              END-PERFORM
              CLOSE ELPEND
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELPEND IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELPOLF')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELPOLF
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO PF-CONTROL-PRIMARY IN ELPOLF
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO PF-COMPANY-CD IN ELPOLF
              START ELPOLF
                        KEY IS NOT < PF-CONTROL-PRIMARY IN ELPOLF
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELPOLF NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (PF-COMPANY-CD IN ELPOLF NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELPOLF
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELPOLF NEXT RECORD
              END-PERFORM
              CLOSE ELPOLF
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELPOLF IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERINMS')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERINMS
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO IM-CONTROL-PRIMARY IN ERINMS
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO IM-COMPANY-CD IN ERINMS
              START ERINMS
                        KEY IS NOT < IM-CONTROL-PRIMARY IN ERINMS
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERINMS NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (IM-COMPANY-CD IN ERINMS NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERINMS
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERINMS NEXT RECORD
              END-PERFORM
              CLOSE ERINMS
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERINMS IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERARBR')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERARBR
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO AB-CONTROL-PRIMARY IN ERARBR
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO AB-COMPANY-CD IN ERARBR
              START ERARBR
                        KEY IS NOT < AB-CONTROL-PRIMARY IN ERARBR
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERARBR NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (AB-COMPANY-CD IN ERARBR NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERARBR
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERARBR NEXT RECORD
              END-PERFORM
              CLOSE ERARBR
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERARBR IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERCCAP')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERCCAP
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO ERCCAP-PRIMARY-KEY IN ERCCAP
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO CAPS-COMPANY-CD IN ERCCAP
              START ERCCAP
                        KEY IS NOT < ERCCAP-PRIMARY-KEY IN ERCCAP
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERCCAP NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (CAPS-COMPANY-CD IN ERCCAP NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERCCAP
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERCCAP NEXT RECORD
              END-PERFORM
              CLOSE ERCCAP
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERCCAP IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELNOTE')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELNOTE
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO EN-CONTROL-PRIMARY IN ELNOTE
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO EN-COMPANY-CD IN ELNOTE
              START ELNOTE
                        KEY IS NOT < EN-CONTROL-PRIMARY IN ELNOTE
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELNOTE NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (EN-COMPANY-CD IN ELNOTE NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELNOTE
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELNOTE NEXT RECORD
              END-PERFORM
              CLOSE ELNOTE
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELNOTE IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERRESS')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERRESS
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO ERRESS-PRIMARY-KEY IN ERRESS
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO RES-COMPANY-CD IN ERRESS
              START ERRESS
                        KEY IS NOT < ERRESS-PRIMARY-KEY IN ERRESS
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERRESS NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (RES-COMPANY-CD IN ERRESS NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERRESS
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERRESS NEXT RECORD
              END-PERFORM
              CLOSE ERRESS
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERRESS IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERRESC')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERRESC
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO ERRESC-RECORD-KEY IN ERRESC
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO RESC-COMPANY-CD IN ERRESC
              START ERRESC
                        KEY IS NOT < ERRESC-RECORD-KEY IN ERRESC
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERRESC NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (RESC-COMPANY-CD IN ERRESC NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERRESC
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERRESC NEXT RECORD
              END-PERFORM
              CLOSE ERRESC
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERRESC IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELACHP')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELACHP
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO AP-CONTROL-PRIMARY IN ELACHP
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO AP-CONTROL-CD IN ELACHP
              START ELACHP
                        KEY IS NOT < AP-CONTROL-PRIMARY IN ELACHP
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELACHP NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (AP-CONTROL-CD IN ELACHP NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELACHP
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELACHP NEXT RECORD
              END-PERFORM
              CLOSE ELACHP
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELACHP IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ELBANK')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ELBANK
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO BM-CONTROL-PRIMARY IN ELBANK
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO BM-COMPANY-CD IN ELBANK
              START ELBANK
                        KEY IS NOT < BM-CONTROL-PRIMARY IN ELBANK
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ELBANK NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (BM-COMPANY-CD IN ELBANK NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ELBANK
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ELBANK NEXT RECORD
              END-PERFORM
              CLOSE ELBANK
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ELBANK IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           IF (ER-FILE-NAME = 'ERLOSS')
              AND (ER-ACTION = 'DELETE')
              OPEN I-O ERLOSS
              MOVE 'OPEN'              TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              MOVE LOW-VALUES     TO LR-CONTROL IN ERLOSS
              MOVE DTE-CLASIC-COMPANY-CD
                                  TO LR-COMPANY-CD IN ERLOSS
              START ERLOSS
                        KEY IS NOT < LR-CONTROL IN ERLOSS
              MOVE 'START'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              READ ERLOSS NEXT RECORD
              PERFORM UNTIL
                 (CHK1 NOT = '0')
                 OR (LR-COMPANY-CD IN ERLOSS NOT =
                           DTE-CLASIC-COMPANY-CD)
                 DELETE ERLOSS
                 MOVE 'DELETE'         TO WS-ACTION
                 PERFORM 1000-ERROR-CHECK
                 READ ERLOSS NEXT RECORD
              END-PERFORM
              CLOSE ERLOSS
              MOVE 'CLOSE'             TO WS-ACTION
              PERFORM 1000-ERROR-CHECK
              DISPLAY 'ERLOSS IS WIPPED OUT (UNLESS NOTED ABOVE)'
           END-IF

           PERFORM 0100-READ           THRU 0100-EXIT

           .
       0200-EXIT.
           EXIT.
                                                                        
       1000-ERROR-CHECK.                                                

           IF CHK1 NOT = ZEROS                                           
              DISPLAY ' FILE = ' WS-FILE-ID ' ACTION = ' WS-ACTION
              DISPLAY 'CHECK FIELDS = ', CHK                           
              DISPLAY ' I/O ERROR  FILE DELETE SKIPPED'
           END-IF

           IF (CHK NOT = '00' AND '10' AND '23' AND '05')
              AND (WS-ACTION NOT = 'OPEN')
              PERFORM ABEND-PGM
           END-IF

           .                                                                        
       1000-EXIT.
           EXIT.

       ABEND-PGM.
                           COPY ELCABEND.
                                                                        
                                                                        
