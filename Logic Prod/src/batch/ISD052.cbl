       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    ISD052.                                           
       AUTHOR.        DAN DRYDEN.                                       
       DATE-WRITTEN.  JANUARY, 1999.                                    
                                                                        
      ***************************************************************** 
      * THIS PROGRAM CALLS IEBGENER (COBGENER) FOR AS MANY TIMES      * 
      * AS THERE ARE CHARACTERS IN THE PARAMETER.   FOR EXAMPLE:      * 
      *                                                               * 
      * //ISD052  EXEC PGM=ISD052,PARM='ATH'                          * 
      * //SYSUT1  DD   DSN=...............                            * 
      * //SYSUTA  DD   SYSOUT=A                                       * 
      * //SYSUTT  DD   SYSOUT=T                                       * 
      * //SYSUTH  DD   SYSOUT=H                                       * 
      *                                                               * 
      * COPIES THE DATASET ON SYSUT1 TO OUTPUT CLASSES A H AND T      * 
      * THIS WILL WORK FOR ANY LRECL AND ANY BLKSIZE                  * 
      * IT WILL ALSO WORK FOR COPYING DATASET TO DATASET              * 
      ***************************************************************** 
                                                                        
       DATA DIVISION.                                                   
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER.                                                      
           05  SUB         PIC S9(4)  BINARY.                           
           05  SYSUT1      PIC X(8)   VALUE 'SYSUT1  '.                 
           05  SYSUT2      PIC X(8)   VALUE 'SYSUT   '.                 
                                                                        
                                                                        
                                                                        
       LINKAGE SECTION.                                                 
                                                                        
       01  PARM.                                                        
           05  PARM-LENGTH     PIC S9(4)  COMP.                         
           05  PARM-CLASS OCCURS 100 TIMES PIC X.                       
                                                                        
                                                                        
        EJECT                                                           
      *                                                                 
       PROCEDURE DIVISION USING PARM.                                   
      *                                                                 
           IF PARM-LENGTH = +0                                          
              MOVE 'A' TO SYSUT2(6:1)                                   
              CALL 'COBGENER' USING SYSUT1, SYSUT2                      
           ELSE                                                         
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > PARM-LENGTH   
                 MOVE PARM-CLASS(SUB) TO SYSUT2(6:1)                    
                 CALL 'COBGENER' USING SYSUT1, SYSUT2                   
              END-PERFORM                                               
           END-IF                                                       
                                                                        
           STOP RUN.                                                    
                                                                        
