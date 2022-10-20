/*--------------------------------------------------------------*/              
/*             BAR CODE PRINT PROGRAM - HEADER FILE             */              
/*--------------------------------------------------------------*/              
/*                                                              */              
/* TRANSLATED FROM PL/I TO C                                    */              
/*                                                              */              
/*--------------------------------------------------------------*/              
                                                                                
#ifndef _EANSRP_H                                                               
#define _EANSRP_H                                                               
                                                                                
#define MAX_IN          40          /* maximum size of input string       */    
#define MAX_OUT         128         /* maximum size of output string     */     
                                                                                
#define BLANK_CHAR      '\x9D'      /* blank character for bar code font   */   
                                                                                
/* Definitions for all types */                                                 
#define NARROW          0                                                       
#define WIDE            1                                                       
                                                                                
#define DEFAULTCODE     0                                                       
#define FCODE39         1                                                       
#define FCODE128        2                                                       
                                                                                
/* Definitions for UPC and EAN */                                               
#define UENARR          1                                                       
#define UPCA            1                                                       
#define UPCE            0                                                       
#define NUM_SYS_0       0                   /* @001 */                          
#define NUM_SYS_1       1                   /* @001 */                          
                                                                                
/* Definitionss for Code 3 of 9 */                                              
#define C39NN           1                                                       
#define C39NW           3                                                       
#define C39WN           2                                                       
#define C39WW           5                                                       
                                                                                
/* Definitions for Interleaved 2 of 5 */                                        
#define ITFNN           1                                                       
#define ITFNW           3                                                       
#define ITFWN           2                                                       
#define ITFWW           5                                                       
                                                                                
/* Definitions for Industrial 2 of 5 */                                         
#define INDUST          1                                                       
#define INDN            1                                                       
#define INDW            3                                                       
                                                                                
/* Definitions for Matrix 2 of 5 */                                             
#define MATRIX          0                                                       
#define MATN            1                                                       
#define MATW            3                                                       
                                                                                
/* Definitions for MSI */                                                       
#define MSIN            1                                                       
#define MSIW            3                                                       
                                                                                
/* Definitions for Code 128 */                                                  
#define C128NARR        1                                                       
#define C128MAXL        128                                                     
#define C128MOD         103                                                     
#define ASTATE          1                                                       
#define BSTATE          2                                                       
#define CSTATE          3                                                       
#define KNEITHER        0                                                       
#define KCONTROL        1                                                       
#define KLOWER          2                                                       
#define ZSHIFT          99                                                      
                                                                                
#define ZCODEA          101                                                     
#define ZCODEB          100                                                     
#define ZCODEC           99                                                     
                                                                                
#define ZSTARTA         103                                                     
#define ZSTARTB         104                                                     
#define ZSTARTC         105 

/*  CSO needs a stop byte */
#define ZSTOP			106                                                   
                                                                                
/* Return code for having too large of an exit string */                        
#define ERRCODE         128                                                     
                                                                                
/* Prototype for bcsubr function */                                             
int bcsubr( char _barcode, const char *_input, int _size, char *_output);       
                                                                                
#endif  /* _EANSRP_H */                                                         
